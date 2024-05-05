use std::{
    fs,
    path::{Path, PathBuf},
};

use serde::Serialize;
use serde_json::ser::{PrettyFormatter, Serializer};

use crate::data::{Description, Entry, EntryId, Identity, Metadata, Plaintext};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Target {
    EntryId(EntryId),
    Description(Description),
}

impl Target {
    pub fn matches(&self, entry: &Entry) -> bool {
        match self {
            Target::EntryId(entry_id) => *entry_id == entry.entry_id,
            Target::Description(description) => *description == entry.description,
        }
    }
}

/// This trait provides a definition of the interface that the various
/// application types must implement.  Its role is more to ensure
/// conformity among implementations and less for generic programming.
pub trait Application {
    type Error;

    fn add(
        &mut self,
        description: Description,
        plaintext: Plaintext,
        maybe_identity: Option<Identity>,
        maybe_metadata: Option<Metadata>,
    ) -> Result<(), Self::Error>;

    fn lookup(
        &self,
        description: Description,
        maybe_identity: Option<Identity>,
    ) -> Result<Vec<(Entry, Plaintext)>, Self::Error>;

    fn modify(
        &mut self,
        target: Target,
        maybe_description: Option<Description>,
        maybe_plaintext: Option<Plaintext>,
        maybe_identity: Option<Identity>,
        maybe_metadata: Option<Metadata>,
    ) -> Result<(), Self::Error>;

    fn remove(&mut self, target: Target) -> Result<(), Self::Error>;

    fn import(&mut self, path: PathBuf) -> Result<(), Self::Error>;

    fn export(&self, path: PathBuf) -> Result<(), Self::Error>;
}

pub fn read(path: impl AsRef<Path>) -> Result<Vec<Entry>, anyhow::Error> {
    let json = fs::read_to_string(path)?;
    serde_json::from_str(&json).map_err(Into::into)
}

pub fn write(path: impl AsRef<Path>, entries: &[Entry]) -> Result<(), anyhow::Error> {
    let mut buf = Vec::new();
    let formatter = PrettyFormatter::with_indent(b"    ");
    let mut ser = Serializer::with_formatter(&mut buf, formatter);
    entries.serialize(&mut ser)?;
    let mut ret = String::from_utf8(buf)?;
    ret.push('\n');
    if let Some(parent) = path.as_ref().parent() {
        if !parent.exists() {
            fs::create_dir_all(parent)?;
        }
    }
    fs::write(path, ret).map_err(Into::into)
}
