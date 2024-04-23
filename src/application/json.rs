use std::{ffi::OsString, fs, path::PathBuf};

use anyhow::Error;
use serde::Serialize;
use serde_json::ser::{PrettyFormatter, Serializer};

use super::common::{Application, Target};
use crate::{
    config::Config,
    data::{Description, Entry, EntryId, Identity, Metadata, Plaintext, Timestamp},
    gpg,
};

pub struct JsonApplication {
    config: Config,
    entries: Vec<Entry>,
}

impl JsonApplication {
    const ENV: [(OsString, OsString); 0] = [];
    const NO_ENTRIES_MSG: &'static str = "no entries match this target";
    const MULTIPLE_ENTRIES_MSG: &'static str = "multiple entries match this target";

    pub fn new(config: Config) -> Result<JsonApplication, Error> {
        let entries = if config.data_file().exists() {
            let json = fs::read_to_string(config.data_file())?;
            serde_json::from_str(&json)?
        } else {
            Vec::new()
        };
        Ok(JsonApplication { config, entries })
    }

    fn write(&self, path: PathBuf) -> Result<(), Error> {
        let entries: &[Entry] = self.entries.as_slice();
        let mut buf = Vec::new();
        let formatter = PrettyFormatter::with_indent(b"    ");
        let mut ser = Serializer::with_formatter(&mut buf, formatter);
        entries.serialize(&mut ser)?;
        let mut ret = String::from_utf8(buf)?;
        ret.push('\n');
        if let Some(parent) = path.parent() {
            if !parent.exists() {
                fs::create_dir_all(parent)?;
            }
        }
        fs::write(path, ret).map_err(Into::into)
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
            return Err(Error::msg(Self::NO_ENTRIES_MSG));
        }

        if is.len() > 1 {
            return Err(Error::msg(Self::MULTIPLE_ENTRIES_MSG));
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
            return Err(Error::msg(Self::NO_ENTRIES_MSG));
        }

        if is.len() > 1 {
            return Err(Error::msg(Self::MULTIPLE_ENTRIES_MSG));
        }

        self.entries.remove(is[0]);

        self.write(self.config.data_file())?;
        Ok(())
    }

    fn import(&mut self, path: PathBuf) -> Result<(), Self::Error> {
        let json = fs::read_to_string(path)?;
        let entries: Vec<Entry> = serde_json::from_str(&json)?;
        self.entries.extend(entries);
        self.write(self.config.data_file())?;
        Ok(())
    }

    fn export(&self, path: PathBuf) -> Result<(), Self::Error> {
        self.write(path)?;
        Ok(())
    }
}
