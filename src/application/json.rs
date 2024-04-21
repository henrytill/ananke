use std::{backtrace::Backtrace, ffi::OsString, fmt, fs, io, path::PathBuf, string};

use serde::Serialize;
use serde_json::ser::{PrettyFormatter, Serializer};

use super::common::{Application, Target};
use crate::{
    config::Config,
    data::{Description, Entry, EntryId, Identity, Metadata, Plaintext, Timestamp},
    gpg,
};

#[derive(Debug)]
pub enum ErrorInner {
    Gpg(gpg::Error),
    Io(io::Error),
    Json(serde_json::Error),
    FromUtf8(string::FromUtf8Error),
    Time(time::error::Format),
    MultipleEntries,
    NoEntries,
}

impl fmt::Display for ErrorInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorInner::Gpg(err) => err.fmt(f),
            ErrorInner::Io(err) => err.fmt(f),
            ErrorInner::Json(err) => err.fmt(f),
            ErrorInner::FromUtf8(err) => err.fmt(f),
            ErrorInner::Time(err) => err.fmt(f),
            ErrorInner::MultipleEntries => write!(f, "multiple entries match this target"),
            ErrorInner::NoEntries => write!(f, "no entries match this target"),
        }
    }
}

#[derive(Debug)]
pub struct ErrorImpl {
    inner: ErrorInner,
    backtrace: Option<Backtrace>,
}

#[derive(Debug)]
pub struct Error {
    inner: Box<ErrorImpl>,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.inner.fmt(f)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self.inner.inner {
            ErrorInner::Gpg(ref err) => Some(err),
            ErrorInner::Io(ref err) => Some(err),
            ErrorInner::Json(ref err) => Some(err),
            ErrorInner::FromUtf8(ref err) => Some(err),
            ErrorInner::Time(ref err) => Some(err),
            ErrorInner::MultipleEntries => None,
            ErrorInner::NoEntries => None,
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::capture(ErrorInner::Io(err))
    }
}

impl From<serde_json::Error> for Error {
    fn from(err: serde_json::Error) -> Error {
        Error::capture(ErrorInner::Json(err))
    }
}

impl From<string::FromUtf8Error> for Error {
    fn from(err: string::FromUtf8Error) -> Error {
        Error::capture(ErrorInner::FromUtf8(err))
    }
}

impl From<time::error::Format> for Error {
    fn from(err: time::error::Format) -> Error {
        Error::capture(ErrorInner::Time(err))
    }
}

impl From<gpg::Error> for Error {
    fn from(mut err: gpg::Error) -> Error {
        let backtrace = err.backtrace();
        let inner = ErrorInner::Gpg(err);
        let inner = Box::new(ErrorImpl { inner, backtrace });
        Error { inner }
    }
}

impl Error {
    fn capture(inner: ErrorInner) -> Error {
        let backtrace = Some(Backtrace::capture());
        let inner = Box::new(ErrorImpl { inner, backtrace });
        Error { inner }
    }

    pub fn backtrace(&mut self) -> Option<Backtrace> {
        self.inner.backtrace.take()
    }

    fn multiple_entries() -> Error {
        Error::capture(ErrorInner::MultipleEntries)
    }

    fn no_entries() -> Error {
        Error::capture(ErrorInner::NoEntries)
    }
}

pub struct JsonApplication {
    config: Config,
    entries: Vec<Entry>,
}

impl JsonApplication {
    const ENV: [(OsString, OsString); 0] = [];

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
            return Err(Error::no_entries());
        }

        if is.len() > 1 {
            return Err(Self::Error::multiple_entries());
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
            return Err(Error::no_entries());
        }

        if is.len() > 1 {
            return Err(Self::Error::multiple_entries());
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
