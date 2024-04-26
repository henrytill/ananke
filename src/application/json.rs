use std::{ffi::OsString, path::PathBuf};

use anyhow::Error;

use super::common::{self, Application, Target};
use crate::{
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
        if schema_version > SchemaVersion::CURRENT {
            migrate(&config, schema_version)?;
        }
        let entries = if config.data_file().exists() {
            common::read(config.data_file())?
        } else {
            Vec::new()
        };
        Ok(JsonApplication { config, entries })
    }

    fn write(&self, path: PathBuf) -> Result<(), Error> {
        let entries: &[Entry] = self.entries.as_slice();
        common::write(path, entries)
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
        let entry_id = EntryId::make(key_id, &timestamp, &description, maybe_identity.as_ref())?;
        let ciphertext = gpg::encrypt(key_id, &plaintext, Self::ENV)?;
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
                    let plaintext = gpg::decrypt(&entry.ciphertext, Self::ENV)?;
                    ret.push((entry.clone(), plaintext))
                }
                (None, _) => {
                    let plaintext = gpg::decrypt(&entry.ciphertext, Self::ENV)?;
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
            entry.ciphertext = gpg::encrypt(&entry.key_id, &plaintext, Self::ENV)?
        }
        if maybe_identity.is_some() {
            entry.identity = maybe_identity
        }
        if maybe_metadata.is_some() {
            entry.metadata = maybe_metadata
        }
        entry.update()?;
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
        let entries: Vec<Entry> = common::read(path)?;
        self.entries.extend(entries);
        self.write(self.config.data_file())?;
        Ok(())
    }

    fn export(&self, path: PathBuf) -> Result<(), Self::Error> {
        self.write(path)?;
        Ok(())
    }
}

fn migrate(_config: &Config, _schema_version: SchemaVersion) -> Result<(), Error> {
    unimplemented!()
}
