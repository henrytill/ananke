use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::Error;
use serde::Deserialize;

use crate::{
    application::base::{Application, Matcher, Target},
    cipher::{base::Cipher, gpg::Text},
    config::{Backend, Config},
    data::{
        self, ArmoredCiphertext, Description, EntryId, Identity, KeyId, Metadata, Plaintext,
        SchemaVersion, SecureEntry, SecureIndexElement, Timestamp,
    },
};

pub struct TextApplication {
    config: Config,
    cipher: Text,
    elems: Vec<SecureIndexElement>,
}

impl TextApplication {
    const MSG_NO_ENTRIES: &'static str = "no entries match this target";
    const MSG_MULTIPLE_ENTRIES: &'static str = "multiple entries match this target";
    const OBJECTS_DIR: &'static str = "objects";
    const MINIMUM_SUPPORTED_SCHEMA_VERSION: SchemaVersion = SchemaVersion::new(4);

    pub fn new(config: Config) -> Result<TextApplication, Error> {
        assert_eq!(config.backend(), Backend::Text);
        let schema_version = data::schema_version(config.schema_file())?;
        if schema_version != SchemaVersion::CURRENT {
            migrate(&config, schema_version)?;
            fs::write(config.schema_file(), SchemaVersion::CURRENT.to_string())?;
        }
        let cipher = Text::default();
        let elems = if config.db().exists() { read(&cipher, config.db())? } else { Vec::new() };
        let ret = TextApplication { config, cipher, elems };
        Ok(ret)
    }

    fn write_index(&self) -> Result<(), Error> {
        let elems: &[SecureIndexElement] = self.elems.as_slice();
        let json = serde_json::to_string_pretty(elems)?;
        let plaintext = Plaintext::from(json);
        write(&self.cipher, self.config.db(), plaintext, self.config.key_id())
    }

    fn entry_path(&self, entry_id: EntryId) -> PathBuf {
        let mut ret = self.objects_dir();
        ret.push(format!("{}.asc", entry_id));
        ret
    }

    fn entry(&self, entry_id: EntryId) -> Result<SecureEntry, Error> {
        let path = self.entry_path(entry_id);
        read(&self.cipher, path)
    }

    fn write_entry(&self, entry: SecureEntry) -> Result<(), Error> {
        let path = self.entry_path(entry.entry_id);
        let plaintext = {
            let json = serde_json::to_string_pretty(&entry)?;
            Plaintext::from(json)
        };
        write(&self.cipher, path, plaintext, self.config.key_id())
    }

    fn delete_entry(&self, entry_id: EntryId) -> Result<(), Error> {
        let path = self.entry_path(entry_id);
        fs::remove_file(path).map_err(Into::into)
    }

    fn objects_dir(&self) -> PathBuf {
        let mut ret = self.config.db_dir();
        ret.push(Self::OBJECTS_DIR);
        ret
    }
}

impl Application for TextApplication {
    type Error = Error;

    type Record = SecureEntry;

    fn add(
        &mut self,
        description: Description,
        plaintext: Plaintext,
        maybe_identity: Option<Identity>,
        maybe_metadata: Option<Metadata>,
    ) -> Result<(), Self::Error> {
        let key_id = self.config.key_id().clone();
        let entry_id = EntryId::new();
        let entry = {
            let timestamp = Timestamp::now();
            let key_id = key_id.clone();
            let description = description.clone();
            let identity = maybe_identity;
            let metadata = maybe_metadata;
            SecureEntry { timestamp, entry_id, key_id, description, identity, plaintext, metadata }
        };
        let elem = SecureIndexElement { description, key_id, entry_id };
        self.write_entry(entry)?;
        self.elems.push(elem);
        self.write_index()
    }

    fn lookup(
        &self,
        description: Description,
        maybe_identity: Option<Identity>,
    ) -> Result<Vec<Self::Record>, Self::Error> {
        let mut ret = Vec::new();
        for elem in self.elems.iter().filter(|e| e.description.contains(description.as_str())) {
            let entry: SecureEntry = self.entry(elem.entry_id)?;
            match (maybe_identity.as_ref(), entry.identity.as_ref()) {
                (Some(identity), Some(entry_identity)) if entry_identity.contains(identity) => {
                    ret.push(entry)
                }
                (None, _) => ret.push(entry),
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
            .elems
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

        let i = is[0];
        let mut elem = self.elems.remove(i);
        let mut entry: SecureEntry = self.entry(elem.entry_id)?;
        if let Some(description) = maybe_description {
            entry.description = description.clone();
            elem.description = description;
        }
        if let Some(plaintext) = maybe_plaintext {
            entry.plaintext = plaintext
        }
        if maybe_identity.is_some() {
            entry.identity = maybe_identity
        }
        if maybe_metadata.is_some() {
            entry.metadata = maybe_metadata
        }
        entry.update();
        self.write_entry(entry)?;
        self.elems.push(elem);
        self.write_index()
    }

    fn remove(&mut self, target: Target) -> Result<(), Self::Error> {
        let is: Vec<usize> = self
            .elems
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

        let i = is[0];
        let elem = self.elems.remove(i);
        self.delete_entry(elem.entry_id)?;
        self.write_index()
    }

    fn import(&mut self, path: PathBuf) -> Result<(), Self::Error> {
        let entries: Vec<SecureEntry> = read(&self.cipher, path)?;
        for entry in entries {
            let key_id = self.config.key_id().clone();
            let entry_id = entry.entry_id;
            let elem = {
                let description = entry.description.clone();
                SecureIndexElement { description, key_id, entry_id }
            };
            self.write_entry(entry)?;
            self.elems.push(elem);
        }
        self.write_index()?;
        Ok(())
    }

    fn export(&self, path: PathBuf) -> Result<(), Self::Error> {
        let mut entries = Vec::new();
        for elem in &self.elems {
            let secure_entry = self.entry(elem.entry_id)?;
            entries.push(secure_entry);
        }
        let plaintext = {
            let json = serde_json::to_string_pretty(&entries)?;
            Plaintext::from(json)
        };
        write(&self.cipher, path, plaintext, self.config.key_id())?;
        Ok(())
    }
}

fn migrate(_config: &Config, schema_version: SchemaVersion) -> Result<(), Error> {
    if schema_version < TextApplication::MINIMUM_SUPPORTED_SCHEMA_VERSION {
        return Err(Error::msg(format!(
            "schema version < {} not supported by text backend",
            TextApplication::MINIMUM_SUPPORTED_SCHEMA_VERSION
        )));
    }
    Ok(())
}

pub fn read<T>(cipher: &Text, path: impl AsRef<Path>) -> Result<T, anyhow::Error>
where
    T: for<'a> Deserialize<'a>,
{
    let blob = fs::read_to_string(path)?;
    let armored = ArmoredCiphertext::from(blob);
    let json: Plaintext = cipher.decrypt(&armored)?;
    serde_json::from_str(json.as_str()).map_err(Into::into)
}

pub fn write(
    cipher: &Text,
    path: impl AsRef<Path>,
    plaintext: Plaintext,
    key_id: &KeyId,
) -> Result<(), anyhow::Error> {
    let armored = cipher.encrypt(key_id, &plaintext)?;
    if let Some(parent) = path.as_ref().parent() {
        if !parent.exists() {
            fs::create_dir_all(parent)?;
        }
    }
    fs::write(path, armored.as_str()).map_err(Into::into)
}
