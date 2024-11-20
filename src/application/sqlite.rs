use std::{convert::TryFrom, ffi::OsString, fs, path::PathBuf};

use anyhow::Error;
use rusqlite::{named_params, params_from_iter, Connection, ToSql};
use uuid::Uuid;

use crate::{
    application::{
        base::{Application, Target},
        json,
    },
    config::{Backend, Config},
    data::{
        self, Description, Entry, EntryId, Identity, Metadata, Plaintext, SchemaVersion, Timestamp,
    },
    gpg,
};

const CREATE_TABLE: &str = "\
CREATE TABLE IF NOT EXISTS entries (
    id TEXT PRIMARY KEY NOT NULL,
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
        if let Some(parent) = config.db().parent() {
            if !parent.exists() {
                fs::create_dir_all(parent)?;
            }
        }
        let mut connection = rusqlite::Connection::open(config.db())?;
        let schema_version = data::schema_version(config.schema_file())?;
        if schema_version != SchemaVersion::CURRENT {
            migrate(&mut connection, &config, schema_version)?;
            fs::write(config.schema_file(), SchemaVersion::CURRENT.to_string())?;
        }
        connection.execute_batch(CREATE_TABLE)?;
        let ret = SqliteApplication { config, connection };
        Ok(ret)
    }

    fn env() -> impl Iterator<Item = (OsString, OsString)> {
        Self::ENV.into_iter()
    }
}

impl Application for SqliteApplication {
    type Error = Error;

    type Record = (Entry, Plaintext);

    fn add(
        &mut self,
        description: Description,
        plaintext: Plaintext,
        maybe_identity: Option<Identity>,
        maybe_metadata: Option<Metadata>,
    ) -> Result<(), Error> {
        let timestamp = Timestamp::now();
        let key_id = self.config.key_id();
        let entry_id = EntryId::new();
        let ciphertext = gpg::binary::encrypt(key_id, &plaintext, Self::env)?;
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
    ) -> Result<Vec<Self::Record>, Error> {
        let (stmt, params) = make_query(Target::Description(description), maybe_identity);
        let mut stmt = self.connection.prepare(&stmt)?;
        let mut rows = stmt.query(params_from_iter(params))?;
        let mut results = Vec::new();
        while let Some(row) = rows.next()? {
            let (entry_id, key_id, timestamp, description, identity, ciphertext, metadata) =
                TryFrom::try_from(row)?;
            let plaintext = gpg::binary::decrypt(&ciphertext, Self::env)?;
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
                let ciphertext = gpg::binary::encrypt(self.config.key_id(), &plaintext, Self::env)?;
                entry.ciphertext = ciphertext
            }
            if maybe_identity.is_some() {
                entry.identity = maybe_identity
            }
            if maybe_metadata.is_some() {
                entry.metadata = maybe_metadata
            }
            entry.update();
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
        let tx = self.connection.transaction()?;

        let count: u64 = match target {
            Target::EntryId(ref entry_id) => {
                let sql = "SELECT count(*) FROM entries WHERE id = :id";
                let mut stmt = tx.prepare(sql)?;
                let params = named_params! { ":id": entry_id, };
                stmt.query_row(params, |row| row.get(0))?
            }
            Target::Description(ref description) => {
                let sql = "SELECT count(*) FROM entries WHERE description = :description";
                let mut stmt = tx.prepare(sql)?;
                let params = named_params! { ":description": description, };
                stmt.query_row(params, |row| row.get(0))?
            }
        };

        if count == 0 {
            return Err(Error::msg(Self::MSG_NO_ENTRIES));
        }

        if count > 1 {
            return Err(Error::msg(Self::MSG_MULTIPLE_ENTRIES));
        }

        match target {
            Target::EntryId(ref entry_id) => {
                let sql = "DELETE FROM entries WHERE id = :id";
                let mut stmt = tx.prepare(sql)?;
                stmt.execute(named_params! { ":id": entry_id, })?;
            }
            Target::Description(ref description) => {
                let sql = "DELETE FROM entries WHERE description = :description";
                let mut stmt = tx.prepare(sql)?;
                stmt.execute(named_params! { ":description": description, })?;
            }
        }

        tx.commit()?;
        Ok(())
    }

    fn import(&mut self, path: PathBuf) -> Result<(), Error> {
        let entries: Vec<Entry> = json::read(path)?;
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
        let sql =
            "SELECT id, keyid, timestamp, description, identity, ciphertext, meta FROM entries";
        let mut stmt = self.connection.prepare(sql)?;
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
        json::write(path, &entries)?;
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
            params.push(entry_id.to_string());
        }
        Target::Description(description) => {
            wheres.push(String::from("description LIKE ?1"));
            params.push(format!("%{}%", description.as_str()));
        }
    }
    if let Some(identity) = maybe_identity {
        wheres.push(String::from("identity LIKE ?2"));
        params.push(format!("%{}%", identity.as_str()));
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
    let mut params: NamedParams<'a> = vec![];

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
    connection: &mut Connection,
    config: &Config,
    schema_version: SchemaVersion,
) -> Result<(), Error> {
    if schema_version == SchemaVersion::new(3) {
        let tx = connection.transaction()?;
        {
            tx.execute_batch("ALTER TABLE entries RENAME TO entries_v3")?;
            tx.execute_batch(CREATE_TABLE)?;
            let sql = "\
INSERT INTO entries
(id, keyid, timestamp, description, identity, ciphertext, meta)
SELECT id, keyid, timestamp, description, identity, ciphertext, meta
FROM entries_v3
";
            let mut stmt = tx.prepare(sql)?;
            stmt.execute([])?;
        }
        {
            let hashes: Vec<String> = {
                let sql = "SELECT id FROM entries";
                let mut stmt = tx.prepare(sql)?;
                let mut rows = stmt.query([])?;
                let mut tmp = Vec::new();
                while let Some(row) = rows.next()? {
                    let (entry_id,) = TryFrom::try_from(row)?;
                    tmp.push(entry_id);
                }
                tmp
            };
            for hash in hashes {
                let sql = "UPDATE entries SET id = :uuid WHERE id = :hash";
                let mut stmt = tx.prepare(sql)?;
                let uuid = Uuid::new_v4().to_string();
                stmt.execute(named_params! { ":uuid": uuid, ":hash": hash })?;
            }
            tx.execute_batch("DROP TABLE entries_v3")?;
        }
        tx.commit()?;
        Ok(())
    } else if schema_version == SchemaVersion::new(2) {
        migrate(connection, config, SchemaVersion::new(3))
    } else if schema_version == SchemaVersion::new(1) {
        let tx = connection.transaction()?;
        {
            tx.execute_batch("ALTER TABLE entries RENAME TO entries_v1")?;
            tx.execute_batch(CREATE_TABLE)?;
            let sql = "\
INSERT INTO entries
(id, keyid, timestamp, description, identity, ciphertext, meta)
SELECT id, ?1, timestamp, description, identity, ciphertext, meta
FROM entries_v1
";
            let mut stmt = tx.prepare(sql)?;
            stmt.execute([config.key_id()])?;
            tx.execute_batch("DROP TABLE entries_v1")?;
        }
        tx.commit()?;
        migrate(connection, config, SchemaVersion::new(2))
    } else {
        Err(Error::msg(format!(
            "no supported migration path for schema version {}",
            schema_version
        )))
    }
}
