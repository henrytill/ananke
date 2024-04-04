use std::{fmt, path::PathBuf};

use super::common::{Application, Target};
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
}

impl Application for SqliteApplication {
    type Error = Error;

    fn add(
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

    fn lookup(
        &self,
        description: Description,
        maybe_identity: Option<Identity>,
    ) -> Result<Vec<(Entry, Plaintext)>, Error> {
        println!("description: {:?}", description);
        println!("maybe_identity: {:?}", maybe_identity);
        Ok(Vec::new())
    }

    fn modify(
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

    fn remove(&mut self, target: Target) -> Result<(), Error> {
        println!("target: {:?}", target);
        Ok(())
    }

    fn import(&mut self, path: PathBuf) -> Result<(), Error> {
        println!("path: {}", path.display());
        Ok(())
    }

    fn export(&self, path: PathBuf) -> Result<(), Error> {
        println!("path: {}", path.display());
        Ok(())
    }
}
