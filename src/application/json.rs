use std::{
    collections::HashMap,
    ffi::OsString,
    fs,
    path::{Path, PathBuf},
};

use anyhow::Error;
use serde::Serialize;
use serde_json::{json, ser::PrettyFormatter, Map, Serializer, Value};
use uuid::Uuid;

use crate::{
    application::base::{self, Application, Target},
    config::Config,
    data::{
        self, Description, Entry, EntryId, Identity, Metadata, Plaintext, SchemaVersion, Timestamp,
    },
    gpg,
};

pub struct JsonApplication {
    config: Config,
    entries: Vec<Entry>,
}

impl JsonApplication {
    const ENV: [(OsString, OsString); 0] = [];
    const MSG_NO_ENTRIES: &'static str = "no entries match this target";
    const MSG_MULTIPLE_ENTRIES: &'static str = "multiple entries match this target";

    pub fn new(config: Config) -> Result<JsonApplication, Error> {
        let schema_version = data::schema_version(config.schema_file())?;
        if schema_version != SchemaVersion::CURRENT {
            migrate(&config, schema_version)?;
            fs::write(config.schema_file(), SchemaVersion::CURRENT.to_string())?;
        }
        let entries =
            if config.data_file().exists() { base::read(config.data_file())? } else { Vec::new() };
        Ok(JsonApplication { config, entries })
    }

    fn write(&self, path: PathBuf) -> Result<(), Error> {
        let entries: &[Entry] = self.entries.as_slice();
        base::write(path, entries)
    }

    fn env() -> impl Iterator<Item = (OsString, OsString)> {
        Self::ENV.into_iter()
    }
}

impl Application for JsonApplication {
    type Error = Error;

    fn add(
        &mut self,
        description: Description,
        plaintext: Plaintext,
        maybe_identity: Option<Identity>,
        maybe_metadata: Option<Metadata>,
    ) -> Result<(), Self::Error> {
        let timestamp = Timestamp::now();
        let key_id = self.config.key_id();
        let entry_id = EntryId::new();
        let ciphertext = gpg::encrypt(key_id, &plaintext, Self::env)?;
        let identity = maybe_identity;
        let metadata = maybe_metadata;
        let entry = {
            let key_id = key_id.clone();
            let description = description.clone();
            Entry { timestamp, entry_id, key_id, description, identity, ciphertext, metadata }
        };
        self.entries.push(entry);
        self.write(self.config.data_file())?;
        Ok(())
    }

    fn lookup(
        &self,
        description: Description,
        maybe_identity: Option<Identity>,
    ) -> Result<Vec<(Entry, Plaintext)>, Self::Error> {
        let mut ret = Vec::new();
        for entry in self.entries.iter().filter(|e| e.description.contains(description.as_str())) {
            match (maybe_identity.as_ref(), entry.identity.as_ref()) {
                (Some(identity), Some(entry_identity)) if identity == entry_identity => {
                    let plaintext = gpg::decrypt(&entry.ciphertext, Self::env)?;
                    ret.push((entry.clone(), plaintext))
                }
                (None, _) => {
                    let plaintext = gpg::decrypt(&entry.ciphertext, Self::env)?;
                    ret.push((entry.clone(), plaintext))
                }
                (_, _) => (),
            }
        }
        Ok(ret)
    }

    fn modify(
        &mut self,
        target: Target,
        maybe_description: Option<Description>,
        maybe_plaintext: Option<Plaintext>,
        maybe_identity: Option<Identity>,
        maybe_metadata: Option<Metadata>,
    ) -> Result<(), Self::Error> {
        let is: Vec<usize> = self
            .entries
            .iter()
            .enumerate()
            .filter(|(_i, entry)| target.matches(entry))
            .map(|(i, _entry)| i)
            .collect();

        if is.is_empty() {
            return Err(Error::msg(Self::MSG_NO_ENTRIES));
        }

        if is.len() > 1 {
            return Err(Error::msg(Self::MSG_MULTIPLE_ENTRIES));
        }

        let mut entry = self.entries.remove(is[0]);
        if let Some(description) = maybe_description {
            entry.description = description
        }
        if let Some(plaintext) = maybe_plaintext {
            entry.ciphertext = gpg::encrypt(&entry.key_id, &plaintext, Self::env)?
        }
        if maybe_identity.is_some() {
            entry.identity = maybe_identity
        }
        if maybe_metadata.is_some() {
            entry.metadata = maybe_metadata
        }
        entry.update();
        self.entries.push(entry);

        self.write(self.config.data_file())?;
        Ok(())
    }

    fn remove(&mut self, target: Target) -> Result<(), Self::Error> {
        let is: Vec<usize> = self
            .entries
            .iter()
            .enumerate()
            .filter(|(_i, entry)| target.matches(entry))
            .map(|(i, _entry)| i)
            .collect();

        if is.is_empty() {
            return Err(Error::msg(Self::MSG_NO_ENTRIES));
        }

        if is.len() > 1 {
            return Err(Error::msg(Self::MSG_MULTIPLE_ENTRIES));
        }

        self.entries.remove(is[0]);

        self.write(self.config.data_file())?;
        Ok(())
    }

    fn import(&mut self, path: PathBuf) -> Result<(), Self::Error> {
        let entries: Vec<Entry> = base::read(path)?;
        self.entries.extend(entries);
        self.write(self.config.data_file())?;
        Ok(())
    }

    fn export(&self, path: PathBuf) -> Result<(), Self::Error> {
        self.write(path)?;
        Ok(())
    }
}

fn write_value(path: impl AsRef<Path>, value: Value) -> Result<(), anyhow::Error> {
    let mut buf = Vec::new();
    let formatter = PrettyFormatter::with_indent(b"    ");
    let mut ser = Serializer::with_formatter(&mut buf, formatter);
    value.serialize(&mut ser)?;
    let mut ret = String::from_utf8(buf)?;
    ret.push('\n');
    if let Some(parent) = path.as_ref().parent() {
        if !parent.exists() {
            fs::create_dir_all(parent)?;
        }
    }
    fs::write(path, ret).map_err(Into::into)
}

fn migrate(config: &Config, schema_version: SchemaVersion) -> Result<(), Error> {
    if schema_version == SchemaVersion::new(3) {
        let json = fs::read_to_string(config.data_file())?;
        let mut value: Value = serde_json::from_str(&json)?;
        let arr = value.as_array_mut().ok_or_else(|| Error::msg("value is not an array"))?;
        for value in arr {
            let obj = value.as_object_mut().ok_or_else(|| Error::msg("value is not an object"))?;
            obj.insert(String::from("id"), json!(Uuid::new_v4().to_string()));
        }
        write_value(config.data_file(), value).map_err(Into::into)
    } else if schema_version == SchemaVersion::new(2) {
        let mappings: HashMap<String, String> = HashMap::from_iter(
            [
                ("Timestamp", "timestamp"),
                ("Id", "id"),
                ("KeyId", "keyId"),
                ("Description", "description"),
                ("Identity", "identity"),
                ("Ciphertext", "ciphertext"),
                ("Meta", "meta"),
            ]
            .into_iter()
            .map(|(k, v)| (String::from(k), String::from(v))),
        );
        let json = fs::read_to_string(config.data_file())?;
        let mut value: Value = serde_json::from_str(&json)?;
        let arr = value.as_array_mut().ok_or_else(|| Error::msg("value is not an array"))?;
        for value in arr {
            let obj = value.as_object_mut().ok_or_else(|| Error::msg("value is not an object"))?;
            let mut target = Map::new();
            for (k, v) in obj.into_iter() {
                if let Some(mapped) = mappings.get(k) {
                    target.insert(mapped.to_owned(), v.to_owned());
                } else {
                    target.insert(k.to_owned(), v.to_owned());
                }
            }
            *value = target.into();
        }
        write_value(config.data_file(), value)?;
        migrate(config, SchemaVersion::new(3))
    } else if schema_version == SchemaVersion::new(1) {
        Err(Error::msg("schema version 1 not supported by JSON backend"))
    } else {
        Err(Error::msg(format!(
            "no supported migration path for schema version {}",
            schema_version
        )))
    }
}

#[cfg(test)]
mod tests {
    use std::{env::VarError, fs, path::PathBuf};

    use anyhow::Error;
    use tempfile::TempDir;

    use crate::{
        config::ConfigBuilder,
        data::{KeyId, SchemaVersion},
    };

    #[test]
    fn migrate_v3_v4() -> Result<(), Error> {
        let tmp_dir = TempDir::with_prefix("ananke-")?;

        {
            let source = {
                const JSON_PATH: [&str; 3] = ["example", "db", "data.json"];
                JSON_PATH.into_iter().collect::<PathBuf>()
            };

            let dest_dir = {
                let mut tmp = tmp_dir.path().to_owned();
                tmp.push("db");
                tmp
            };

            fs::create_dir_all(&dest_dir)?;

            let dest = {
                let mut tmp = dest_dir;
                tmp.push("data.json");
                tmp
            };

            fs::copy(source, dest)?;
        }

        let getenv = {
            let data_dir = tmp_dir.path();
            let key_id = KeyId::from("371C136C");
            move |s| match s {
                "ANANKE_DATA_DIR" => Ok(data_dir.to_string_lossy().to_string()),
                "ANANKE_KEY_ID" => Ok(key_id.to_string()),
                _ => Err(VarError::NotPresent),
            }
        };

        let config = ConfigBuilder::new(getenv).with_defaults()?.with_env()?.build()?;

        super::migrate(&config, SchemaVersion::new(3))
    }
}
