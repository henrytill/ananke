use std::{convert::TryFrom, ffi::OsString, fs, path::PathBuf};

use anyhow::Error;
use rusqlite::{named_params, params_from_iter, Connection, ToSql};

use super::common::{self, Application, Target};
use crate::{
    config::{Backend, Config},
    data::{
        self, Description, Entry, EntryId, Identity, Metadata, Plaintext, SchemaVersion, Timestamp,
    },
    gpg,
};

const CREATE_TABLE: &str = "\
CREATE TABLE IF NOT EXISTS entries (
    id TEXT UNIQUE NOT NULL,
    keyid TEXT NOT NULL,
    timestamp TEXT NOT NULL,
    description TEXT NOT NULL,
    identity TEXT,
    ciphertext TEXT NOT NULL,
    meta TEXT
)
";

const INSERT: &str = "\
INSERT OR REPLACE INTO
entries(id, keyid, timestamp, description, identity, ciphertext, meta)
VALUES(:id, :keyid, :timestamp, :description, :identity, :ciphertext, :meta)
";

pub struct SqliteApplication {
    config: Config,
    connection: Connection,
}

impl SqliteApplication {
    const ENV: [(OsString, OsString); 0] = [];
    const MSG_NO_ENTRIES: &'static str = "no entries match this target";
    const MSG_MULTIPLE_ENTRIES: &'static str = "multiple entries match this target";

    pub fn new(config: Config) -> Result<SqliteApplication, Error> {
        assert_eq!(config.backend(), Backend::Sqlite);
        if let Some(parent) = config.data_file().parent() {
            if !parent.exists() {
                fs::create_dir_all(parent)?;
            }
        }
        let mut connection = rusqlite::Connection::open(config.data_file())?;
        let schema_version = data::schema_version(config.schema_file())?;
        if schema_version > SchemaVersion::CURRENT {
            migrate(&mut connection, &config, schema_version)?;
        }
        connection.execute_batch(CREATE_TABLE)?;
        let ret = SqliteApplication { config, connection };
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
        let timestamp = Timestamp::now();
        let key_id = self.config.key_id();
        let entry_id = EntryId::make(key_id, &timestamp, &description, maybe_identity.as_ref())?;
        let ciphertext = gpg::encrypt(key_id, &plaintext, Self::ENV)?;
        let identity = maybe_identity;
        let metadata = maybe_metadata;

        let mut stmt = self.connection.prepare(INSERT)?;
        stmt.execute(named_params! {
            ":id": entry_id,
            ":keyid": key_id,
            ":timestamp": timestamp,
            ":description": description,
            ":identity": identity,
            ":ciphertext": ciphertext,
            ":meta": metadata
        })?;

        Ok(())
    }

    fn lookup(
        &self,
        description: Description,
        maybe_identity: Option<Identity>,
    ) -> Result<Vec<(Entry, Plaintext)>, Error> {
        let (stmt, params) = make_query(Target::Description(description), maybe_identity);
        let mut stmt = self.connection.prepare(&stmt)?;
        let mut rows = stmt.query(params_from_iter(params))?;
        let mut results = Vec::new();
        while let Some(row) = rows.next()? {
            let (entry_id, key_id, timestamp, description, identity, ciphertext, metadata) =
                TryFrom::try_from(row)?;
            let plaintext = gpg::decrypt(&ciphertext, Self::ENV)?;
            results.push((
                Entry { timestamp, entry_id, key_id, description, identity, ciphertext, metadata },
                plaintext,
            ))
        }
        Ok(results)
    }

    fn modify(
        &mut self,
        target: Target,
        maybe_description: Option<Description>,
        maybe_plaintext: Option<Plaintext>,
        maybe_identity: Option<Identity>,
        maybe_metadata: Option<Metadata>,
    ) -> Result<(), Error> {
        let tx = self.connection.transaction()?;

        let mut entries = {
            let (stmt, params) = make_query(target.clone(), maybe_identity.clone());
            let mut stmt = tx.prepare(&stmt)?;
            let mut rows = stmt.query(params_from_iter(params))?;
            let mut entries = Vec::new();
            while let Some(row) = rows.next()? {
                let (entry_id, key_id, timestamp, description, identity, ciphertext, metadata) =
                    TryFrom::try_from(row)?;
                entries.push(Entry {
                    timestamp,
                    entry_id,
                    key_id,
                    description,
                    identity,
                    ciphertext,
                    metadata,
                })
            }
            entries
        };

        let entries_len = entries.len();

        if entries_len == 0 {
            return Err(Error::msg(Self::MSG_NO_ENTRIES));
        }

        if entries_len > 1 {
            return Err(Error::msg(Self::MSG_MULTIPLE_ENTRIES));
        }

        let entry = {
            let mut entry = entries.remove(0);
            if let Some(description) = maybe_description {
                entry.description = description
            }
            if let Some(plaintext) = maybe_plaintext {
                let ciphertext = gpg::encrypt(self.config.key_id(), &plaintext, Self::ENV)?;
                entry.ciphertext = ciphertext
            }
            if maybe_identity.is_some() {
                entry.identity = maybe_identity
            }
            if maybe_metadata.is_some() {
                entry.metadata = maybe_metadata
            }
            entry.update()?;
            entry
        };

        {
            let (stmt, params) = make_update(&target, &entry)?;
            let mut stmt = tx.prepare(&stmt)?;
            stmt.execute(params.as_slice())?;
        }

        tx.commit()?;
        Ok(())
    }

    fn remove(&mut self, target: Target) -> Result<(), Error> {
        match target {
            Target::EntryId(entry_id) => {
                let sql = "DELETE FROM entries WHERE id = :id";
                let mut stmt = self.connection.prepare(sql)?;
                stmt.execute(named_params! { ":id": entry_id, })?;
            }
            Target::Description(description) => {
                let sql = "DELETE FROM entries WHERE description = :description";
                let mut stmt = self.connection.prepare(sql)?;
                stmt.execute(named_params! { ":description": description, })?;
            }
        }
        Ok(())
    }

    fn import(&mut self, path: PathBuf) -> Result<(), Error> {
        let entries: Vec<Entry> = common::read(path)?;
        let tx = self.connection.transaction()?;
        {
            let mut stmt = tx.prepare(INSERT)?;
            for entry in entries {
                stmt.execute(named_params! {
                    ":id": entry.entry_id,
                    ":keyid": entry.key_id,
                    ":timestamp": entry.timestamp,
                    ":description": entry.description,
                    ":identity": entry.identity,
                    ":ciphertext": entry.ciphertext,
                    ":meta": entry.metadata
                })?;
            }
        }
        tx.commit()?;
        Ok(())
    }

    fn export(&self, path: PathBuf) -> Result<(), Error> {
        let stmt =
            "SELECT id, keyid, timestamp, description, identity, ciphertext, meta FROM entries";
        let mut stmt = self.connection.prepare(stmt)?;
        let mut rows = stmt.query([])?;
        let mut entries = Vec::new();
        while let Some(row) = rows.next()? {
            let (entry_id, key_id, timestamp, description, identity, ciphertext, metadata) =
                TryFrom::try_from(row)?;
            entries.push(Entry {
                timestamp,
                entry_id,
                key_id,
                description,
                identity,
                ciphertext,
                metadata,
            })
        }
        common::write(path, &entries)?;
        Ok(())
    }
}

fn make_query(target: Target, maybe_identity: Option<Identity>) -> (String, Vec<String>) {
    let mut sql = String::from(
        "SELECT id, keyid, timestamp, description, identity, ciphertext, meta FROM entries WHERE ",
    );
    let mut wheres = Vec::new();
    let mut params = Vec::new();
    match target {
        Target::EntryId(entry_id) => {
            wheres.push(String::from("id = ?1"));
            params.push(entry_id.into_inner());
        }
        Target::Description(description) => {
            wheres.push(String::from("description LIKE ?1"));
            params.push(format!("%{}%", description.into_inner()));
        }
    }
    if let Some(identity) = maybe_identity {
        wheres.push(String::from("identity = ?2"));
        params.push(identity.into_inner());
    }
    sql.push_str(&wheres.join(" AND "));
    (sql, params)
}

type NamedParams<'a> = Vec<(&'a str, &'a dyn ToSql)>;

fn make_update<'a>(
    target: &'a Target,
    entry: &'a Entry,
) -> Result<(String, NamedParams<'a>), Error> {
    let mut wheres: Vec<&str> = vec![];
    let mut params: Vec<(&'a str, &'a dyn ToSql)> = vec![];

    match target {
        Target::EntryId(ref entry_id) => {
            wheres.push("entries.id = :target");
            params.push((":target", entry_id));
        }
        Target::Description(ref description) => {
            wheres.push("entries.description = :target");
            params.push((":target", description));
        }
    }

    let mut sets: Vec<&str> = vec![];
    sets.push("id = :id");
    params.push((":id", &entry.entry_id));

    sets.push("timestamp = :timestamp");
    params.push((":timestamp", &entry.timestamp));

    sets.push("keyid = :keyid");
    params.push((":keyid", &entry.key_id));

    sets.push("description = :description");
    params.push((":description", &entry.description));

    sets.push("ciphertext = :ciphertext");
    params.push((":ciphertext", &entry.ciphertext));

    sets.push("identity = :identity");
    params.push((":identity", &entry.identity));

    sets.push("meta = :meta");
    params.push((":meta", &entry.metadata));

    let sql = format!("UPDATE entries SET {} WHERE {}", sets.join(", "), wheres.join(" AND "));
    Ok((sql, params))
}

fn migrate(
    _connection: &mut Connection,
    _config: &Config,
    _schema_version: SchemaVersion,
) -> Result<(), Error> {
    unimplemented!()
}
