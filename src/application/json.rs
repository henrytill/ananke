use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

use anyhow::Error;
use serde::Serialize;
use serde_json::{Map, Serializer, Value, json, ser::PrettyFormatter};
use uuid::Uuid;

use crate::{
    application::{
        base::{Application, Matcher, Target},
        text,
    },
    cipher::{
        base::Cipher,
        gpg::{Binary, Text},
    },
    config::Config,
    data::{
        self, Description, Entry, EntryId, Identity, Metadata, Plaintext, SchemaVersion,
        SecureEntry, Timestamp,
    },
};

pub struct JsonApplication {
    config: Config,
    cipher: Binary,
    entries: Vec<Entry>,
}

impl JsonApplication {
    const MSG_NO_ENTRIES: &'static str = "no entries match this target";
    const MSG_MULTIPLE_ENTRIES: &'static str = "multiple entries match this target";

    pub fn new(config: Config) -> Result<JsonApplication, Error> {
        let schema_version = data::schema_version(config.schema_file())?;
        if schema_version != SchemaVersion::CURRENT {
            migrate(&config, schema_version)?;
            fs::write(config.schema_file(), SchemaVersion::CURRENT.to_string())?;
        }
        let cipher = Binary::default();
        let entries = if config.db().exists() {
            read(config.db())?
        } else {
            Vec::new()
        };
        Ok(JsonApplication {
            config,
            cipher,
            entries,
        })
    }

    fn write(&self, path: PathBuf) -> Result<(), Error> {
        let entries: &[Entry] = self.entries.as_slice();
        write(path, entries)
    }
}

impl Application for JsonApplication {
    type Error = Error;

    type Record = (Entry, Plaintext);

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
        let ciphertext = self.cipher.encrypt(key_id, &plaintext)?;
        let identity = maybe_identity;
        let metadata = maybe_metadata;
        let entry = {
            let key_id = key_id.clone();
            let description = description.clone();
            Entry {
                timestamp,
                entry_id,
                key_id,
                description,
                identity,
                ciphertext,
                metadata,
            }
        };
        self.entries.push(entry);
        self.write(self.config.db())?;
        Ok(())
    }

    fn lookup(
        &self,
        description: Description,
        maybe_identity: Option<Identity>,
    ) -> Result<Vec<Self::Record>, Self::Error> {
        let mut ret = Vec::new();
        for entry in self.entries.iter() {
            if !entry.description.contains(description.as_str()) {
                continue;
            }
            match (maybe_identity.as_ref(), entry.identity.as_ref()) {
                (Some(identity), Some(entry_identity)) if entry_identity.contains(identity) => {
                    let plaintext = self.cipher.decrypt(&entry.ciphertext)?;
                    ret.push((entry.clone(), plaintext))
                }
                (None, _) => {
                    let plaintext = self.cipher.decrypt(&entry.ciphertext)?;
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
            .filter(|(_i, entry)| target.matches(*entry))
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
            entry.ciphertext = self.cipher.encrypt(&entry.key_id, &plaintext)?
        }
        if maybe_identity.is_some() {
            entry.identity = maybe_identity
        }
        if maybe_metadata.is_some() {
            entry.metadata = maybe_metadata
        }
        entry.update();
        self.entries.push(entry);

        self.write(self.config.db())?;
        Ok(())
    }

    fn remove(&mut self, target: Target) -> Result<(), Self::Error> {
        let is: Vec<usize> = self
            .entries
            .iter()
            .enumerate()
            .filter(|(_i, entry)| target.matches(*entry))
            .map(|(i, _entry)| i)
            .collect();

        if is.is_empty() {
            return Err(Error::msg(Self::MSG_NO_ENTRIES));
        }

        if is.len() > 1 {
            return Err(Error::msg(Self::MSG_MULTIPLE_ENTRIES));
        }

        self.entries.remove(is[0]);

        self.write(self.config.db())?;
        Ok(())
    }

    fn import(&mut self, path: PathBuf) -> Result<(), Self::Error> {
        let entries: Vec<SecureEntry> = {
            let cipher = Text::default();
            text::read(&cipher, path)?
        };
        for entry in entries {
            let timestamp = entry.timestamp;
            let entry_id = entry.entry_id;
            let key_id = entry.key_id.clone();
            let description = entry.description.clone();
            let identity = entry.identity.clone();
            let ciphertext = self.cipher.encrypt(&key_id, &entry.plaintext)?;
            let metadata = entry.metadata.clone();
            self.entries.push(Entry {
                timestamp,
                entry_id,
                key_id,
                description,
                identity,
                ciphertext,
                metadata,
            })
        }
        self.write(self.config.db())?;
        Ok(())
    }

    fn export(&self, path: PathBuf) -> Result<(), Self::Error> {
        let mut out = Vec::new();
        for entry in self.entries.iter() {
            let timestamp = entry.timestamp;
            let entry_id = entry.entry_id;
            let key_id = entry.key_id.clone();
            let description = entry.description.clone();
            let identity = entry.identity.clone();
            let plaintext = self.cipher.decrypt(&entry.ciphertext)?;
            let metadata = entry.metadata.clone();
            out.push(SecureEntry {
                timestamp,
                entry_id,
                key_id,
                description,
                identity,
                plaintext,
                metadata,
            })
        }
        let plaintext = {
            let json = serde_json::to_string_pretty(&out)?;
            Plaintext::from(json)
        };
        let cipher = Text::default();
        text::write(&cipher, path, plaintext, self.config.key_id())?;
        Ok(())
    }
}

pub fn read(path: impl AsRef<Path>) -> Result<Vec<Entry>, anyhow::Error> {
    let json = fs::read_to_string(path)?;
    serde_json::from_str(&json).map_err(Into::into)
}

pub fn write(path: impl AsRef<Path>, data: impl Serialize) -> Result<(), anyhow::Error> {
    let mut buf = Vec::new();
    const SPACES_PER_INDENT: usize = 4;
    const INDENT: [u8; SPACES_PER_INDENT] = [b' '; SPACES_PER_INDENT];
    let formatter = PrettyFormatter::with_indent(&INDENT);
    let mut ser = Serializer::with_formatter(&mut buf, formatter);
    data.serialize(&mut ser)?;
    let mut ret = String::from_utf8(buf)?;
    ret.push('\n');
    if let Some(parent) = path.as_ref().parent()
        && !parent.exists()
    {
        fs::create_dir_all(parent)?;
    }
    fs::write(path, ret).map_err(Into::into)
}

fn migrate(config: &Config, schema_version: SchemaVersion) -> Result<(), Error> {
    if schema_version == SchemaVersion::new(3) {
        let json = fs::read_to_string(config.db())?;
        let mut value: Value = serde_json::from_str(&json)?;
        let arr = value
            .as_array_mut()
            .ok_or_else(|| Error::msg("value is not an array"))?;
        for value in arr {
            let obj = value
                .as_object_mut()
                .ok_or_else(|| Error::msg("value is not an object"))?;
            obj.insert(String::from("id"), json!(Uuid::new_v4()));
        }
        write(config.db(), value)
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
        let json = fs::read_to_string(config.db())?;
        let mut value: Value = serde_json::from_str(&json)?;
        let arr = value
            .as_array_mut()
            .ok_or_else(|| Error::msg("value is not an array"))?;
        for value in arr {
            let obj = value
                .as_object()
                .ok_or_else(|| Error::msg("value is not an object"))?;
            let mut target = Map::new();
            for (k, v) in obj.into_iter() {
                if let Some(mapped) = mappings.get(k) {
                    target.insert(mapped.clone(), v.clone());
                } else {
                    target.insert(k.clone(), v.clone());
                }
            }
            *value = target.into();
        }
        write(config.db(), value)?;
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
