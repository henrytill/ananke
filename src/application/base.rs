use std::path::PathBuf;

use crate::data::{Description, Entry, EntryId, Identity, Metadata, Plaintext, SecureIndexElement};

pub trait Matcher<A> {
    fn matches(&self, candidate: A) -> bool;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Target {
    EntryId(EntryId),
    Description(Description),
}

impl Matcher<&Entry> for Target {
    fn matches(&self, candidate: &Entry) -> bool {
        match self {
            Target::EntryId(entry_id) => *entry_id == candidate.entry_id,
            Target::Description(description) => *description == candidate.description,
        }
    }
}

impl Matcher<&SecureIndexElement> for Target {
    fn matches(&self, candidate: &SecureIndexElement) -> bool {
        match self {
            Target::EntryId(entry_id) => *entry_id == candidate.entry_id,
            Target::Description(description) => *description == candidate.description,
        }
    }
}

/// This trait provides a definition of the interface that the various
/// application types must implement.  Its role is more to ensure
/// conformity among implementations and less for generic programming.
pub trait Application {
    type Error;

    type Record;

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
    ) -> Result<Vec<Self::Record>, Self::Error>;

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
