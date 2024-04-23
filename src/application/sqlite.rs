use std::path::PathBuf;

use anyhow::Error;

use super::common::{Application, Target};
use crate::{
    config::Config,
    data::{Description, Entry, Identity, Metadata, Plaintext},
};

pub struct SqliteApplication {}

impl SqliteApplication {
    pub fn new(_config: Config) -> Result<SqliteApplication, Error> {
        let ret = SqliteApplication {};
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
