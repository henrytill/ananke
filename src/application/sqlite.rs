use std::{fmt, path::Path};

use super::common::Target;
use crate::{
    config::Config,
    data::{Description, Entry, Identity, Metadata, Plaintext},
};

#[derive(Debug)]
pub struct Error {}

impl fmt::Display for Error {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl std::error::Error for Error {}

pub struct SqliteApplication {}

impl SqliteApplication {
    pub fn new(_config: Config) -> Result<Self, Error> {
        let ret = Self {};
        Ok(ret)
    }

    pub fn add(
        &mut self,
        description: Description,
        plaintext: Plaintext,
        maybe_identity: Option<Identity>,
        maybe_metadata: Option<Metadata>,
    ) -> Result<(), Error> {
        println!("description: {:?}", description);
        println!("plaintext: {:?}", plaintext);
        println!("maybe_identity: {:?}", maybe_identity);
        println!("maybe_metadata: {:?}", maybe_metadata);
        Ok(())
    }

    pub fn lookup(
        &self,
        description: Description,
        maybe_identity: Option<Identity>,
    ) -> Result<Vec<(Entry, Plaintext)>, Error> {
        println!("description: {:?}", description);
        println!("maybe_identity: {:?}", maybe_identity);
        Ok(Vec::new())
    }

    pub fn modify(
        &mut self,
        target: Target,
        maybe_description: Option<Description>,
        maybe_plaintext: Option<Plaintext>,
        maybe_identity: Option<Identity>,
        maybe_metadata: Option<Metadata>,
    ) -> Result<(), Error> {
        println!("target: {:?}", target);
        println!("maybe_description: {:?}", maybe_description);
        println!("maybe_plaintext: {:?}", maybe_plaintext);
        println!("maybe_identity: {:?}", maybe_identity);
        println!("maybe_metadata: {:?}", maybe_metadata);
        Ok(())
    }

    pub fn remove(&mut self, target: Target) -> Result<(), Error> {
        println!("target: {:?}", target);
        Ok(())
    }

    pub fn import(&mut self, path: impl AsRef<Path>) -> Result<(), Error> {
        println!("path: {}", path.as_ref().display());
        Ok(())
    }

    pub fn export(&self, path: impl AsRef<Path>) -> Result<(), Error> {
        println!("path: {}", path.as_ref().display());
        Ok(())
    }
}
