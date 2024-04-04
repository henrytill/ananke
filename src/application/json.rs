use std::{ffi::OsString, fmt, fs, io, path::PathBuf, string};

use serde::Serialize;
use serde_json::ser::{PrettyFormatter, Serializer};

use super::common::Target;
use crate::{
    config::Config,
    data::{Description, Entry, Id, Identity, Metadata, Plaintext, Timestamp},
    gpg,
};

#[derive(Debug)]
pub enum ErrorImpl {
    Io(io::Error),
    Json(serde_json::Error),
    FromUtf8(string::FromUtf8Error),
    Time(time::error::Format),
    MissingStdin,
    MissingStdout,
    Join,
    MultipleEntries,
}

#[derive(Debug)]
pub struct Error {
    inner: Box<ErrorImpl>,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner.as_ref() {
            ErrorImpl::Io(e) => write!(f, "Io error: {}", e),
            ErrorImpl::Json(e) => write!(f, "Json error: {}", e),
            ErrorImpl::FromUtf8(e) => write!(f, "UTF8 conversion error: {}", e),
            ErrorImpl::Time(e) => write!(f, "Time error: {}", e),
            ErrorImpl::MissingStdin => write!(f, "Error: missing stdin"),
            ErrorImpl::MissingStdout => write!(f, "Error: missing stdout"),
            ErrorImpl::Join => write!(f, "Error: join"),
            ErrorImpl::MultipleEntries => write!(f, "Multiple entries match this target"),
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        let inner = Box::new(ErrorImpl::Io(err));
        Self { inner }
    }
}

impl From<serde_json::Error> for Error {
    fn from(err: serde_json::Error) -> Self {
        let inner = Box::new(ErrorImpl::Json(err));
        Self { inner }
    }
}

impl From<string::FromUtf8Error> for Error {
    fn from(err: string::FromUtf8Error) -> Self {
        let inner = Box::new(ErrorImpl::FromUtf8(err));
        Self { inner }
    }
}

impl From<time::error::Format> for Error {
    fn from(err: time::error::Format) -> Self {
        let inner = Box::new(ErrorImpl::Time(err));
        Self { inner }
    }
}

impl From<gpg::Error> for Error {
    fn from(err: gpg::Error) -> Self {
        let inner = match err {
            gpg::Error::Io(err) => ErrorImpl::Io(err),
            gpg::Error::FromUtf8(err) => ErrorImpl::FromUtf8(err),
            gpg::Error::MissingStdin => ErrorImpl::MissingStdin,
            gpg::Error::MissingStdout => ErrorImpl::MissingStdout,
            gpg::Error::Join => ErrorImpl::Join,
        };
        let inner = Box::new(inner);
        Self { inner }
    }
}

impl Error {
    fn multiple_entries() -> Self {
        let inner = Box::new(ErrorImpl::MultipleEntries);
        Self { inner }
    }
}

impl std::error::Error for Error {}

pub struct JsonApplication {
    config: Config,
    entries: Vec<Entry>,
    dirty: bool,
}

impl JsonApplication {
    const ENV: [(OsString, OsString); 0] = [];

    pub fn new(config: Config) -> Result<Self, Error> {
        let json = fs::read_to_string(config.data_file())?;
        let entries: Vec<Entry> = serde_json::from_str(&json)?;
        let dirty = false;
        Ok(Self { config, entries, dirty })
    }

    fn write(&self, path: PathBuf) -> Result<(), Error> {
        if !self.dirty {
            return Ok(());
        }
        let entries: &[Entry] = self.entries.as_slice();
        let mut buf = Vec::new();
        let formatter = PrettyFormatter::with_indent(b"    ");
        let mut ser = Serializer::with_formatter(&mut buf, formatter);
        entries.serialize(&mut ser)?;
        let mut ret = String::from_utf8(buf)?;
        ret.push('\n');
        fs::write(path, ret).map_err(Into::into)
    }

    pub fn add(
        &mut self,
        description: Description,
        plaintext: Plaintext,
        maybe_identity: Option<Identity>,
        maybe_metadata: Option<Metadata>,
    ) -> Result<(), Error> {
        let timestamp = Timestamp::now();
        let key_id = self.config.key_id().clone();
        let id = Id::generate(&key_id, &timestamp, &description, maybe_identity.as_ref())?;
        let ciphertext = gpg::encrypt(&key_id, &plaintext, Self::ENV)?;
        let identity = maybe_identity;
        let metadata = maybe_metadata;
        let entry = {
            let description = description.clone();
            Entry { timestamp, id, key_id, description, identity, ciphertext, metadata }
        };
        self.entries.push(entry);
        self.write(self.config.data_file())?;
        Ok(())
    }

    pub fn lookup(
        &self,
        description: Description,
        maybe_identity: Option<Identity>,
    ) -> Result<Vec<(Entry, Plaintext)>, Error> {
        let mut ret = Vec::new();
        for entry in self.entries.iter() {
            if entry.description.contains(description.as_str()) {
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
        }
        Ok(ret)
    }

    pub fn modify(
        &mut self,
        target: Target,
        maybe_description: Option<Description>,
        maybe_plaintext: Option<Plaintext>,
        maybe_identity: Option<Identity>,
        maybe_metadata: Option<Metadata>,
    ) -> Result<(), Error> {
        let is: Vec<usize> = self
            .entries
            .iter()
            .enumerate()
            .filter(|(_i, entry)| target.matches(entry))
            .map(|(i, _entry)| i)
            .collect();

        if is.is_empty() {
            return Ok(());
        }

        if is.len() > 1 {
            return Err(Error::multiple_entries());
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

    pub fn remove(&mut self, target: Target) -> Result<(), Error> {
        let is: Vec<usize> = self
            .entries
            .iter()
            .enumerate()
            .filter(|(_i, entry)| target.matches(entry))
            .map(|(i, _entry)| i)
            .collect();

        if is.is_empty() {
            return Ok(());
        }

        if is.len() > 1 {
            return Err(Error::multiple_entries());
        }

        self.entries.remove(is[0]);

        self.write(self.config.data_file())?;
        Ok(())
    }

    pub fn import(&mut self, path: PathBuf) -> Result<(), Error> {
        let json = fs::read_to_string(path)?;
        let entries: Vec<Entry> = serde_json::from_str(&json)?;
        self.entries.extend(entries);
        self.write(self.config.data_file())?;
        Ok(())
    }

    pub fn export(&self, path: PathBuf) -> Result<(), Error> {
        self.write(path)?;
        Ok(())
    }
}
