use std::path::Path;

use crate::data::{Description, Entry, Id, Identity, Metadata, Plaintext};

#[derive(Debug, PartialEq, Eq)]
pub enum Target {
    Id(Id),
    Description(Description),
}

impl Target {
    pub fn matches(&self, entry: &Entry) -> bool {
        match self {
            Target::Id(id) if *id == entry.id => true,
            Target::Description(d) if *d == entry.description => true,
            _ => false,
        }
    }
}

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

    fn import<P: AsRef<Path>>(&mut self, path: P) -> Result<(), Self::Error>;

    fn export<P: AsRef<Path>>(&self, path: P) -> Result<(), Self::Error>;
}
