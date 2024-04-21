use std::path::PathBuf;

use crate::data::{Description, Entry, EntryId, Identity, Metadata, Plaintext};

#[derive(Debug, PartialEq, Eq)]
pub enum Target {
    EntryId(EntryId),
    Description(Description),
}

impl Target {
    pub fn matches(&self, entry: &Entry) -> bool {
        match self {
            Target::EntryId(entry_id) if *entry_id == entry.entry_id => true,
            Target::Description(d) if *d == entry.description => true,
            _ => false,
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
