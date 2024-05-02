mod schema;

use std::{
    hash::{Hash, Hasher},
    str::FromStr,
};

use data_encoding::{DecodeError, BASE64, HEXLOWER};
use rusqlite::types::{FromSql, FromSqlError, FromSqlResult, ToSql, ToSqlOutput, ValueRef};
use serde::{Deserialize, Serialize};
use sha1::{Digest, Sha1};
use time::{
    format_description::well_known::{
        iso8601::{Config, EncodedConfig},
        Iso8601,
    },
    OffsetDateTime,
};
use zeroize::{Zeroize, ZeroizeOnDrop};

pub use schema::{schema_version, SchemaVersion};

/// Wraps a [`String`] in a newtype
macro_rules! wrap_string {
    ($name:ident) => {
        /// A newtype that wraps a [`String`].
        #[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
        pub struct $name(String);

        #[allow(dead_code)]
        impl $name {
            pub const fn new(value: String) -> Self {
                Self(value)
            }

            pub fn as_str(&self) -> &str {
                self.0.as_str()
            }

            pub fn into_inner(self) -> String {
                self.0
            }
        }

        impl From<String> for $name {
            fn from(value: String) -> Self {
                Self(value)
            }
        }

        impl ToString for $name {
            fn to_string(&self) -> String {
                self.0.to_string()
            }
        }

        #[cfg(test)]
        impl From<&str> for $name {
            fn from(name: &str) -> Self {
                Self(name.to_string())
            }
        }

        impl ToSql for $name {
            fn to_sql(&self) -> rusqlite::Result<ToSqlOutput<'_>> {
                self.0.to_sql()
            }
        }

        impl FromSql for $name {
            fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
                String::column_result(value).map(Self)
            }
        }
    };
}

wrap_string!(EntryId);
wrap_string!(KeyId);
wrap_string!(Description);
wrap_string!(Identity);
wrap_string!(Metadata);

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Zeroize, ZeroizeOnDrop)]
pub struct Plaintext(String);

impl Plaintext {
    pub const fn new(value: String) -> Plaintext {
        Plaintext(value)
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }
}

impl From<String> for Plaintext {
    fn from(value: String) -> Plaintext {
        Plaintext(value)
    }
}

impl ToString for Plaintext {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

#[cfg(test)]
impl From<&str> for Plaintext {
    fn from(name: &str) -> Plaintext {
        Plaintext(name.to_string())
    }
}

impl EntryId {
    pub fn make(
        key_id: &KeyId,
        timestamp: &Timestamp,
        description: &Description,
        maybe_identity: Option<&Identity>,
    ) -> Result<EntryId, time::error::Format> {
        let mut input =
            format!("{}{}{}", key_id.to_string(), timestamp.isoformat()?, description.to_string());
        if let Some(identity) = maybe_identity {
            input.push_str(identity.as_str())
        }
        let digest = {
            let mut hasher = Sha1::new();
            hasher.update(input.as_bytes());
            let bytes = hasher.finalize();
            HEXLOWER.encode(&bytes)
        };
        Ok(EntryId(digest))
    }
}

impl Description {
    pub fn contains(&self, pat: &str) -> bool {
        self.0.contains(pat)
    }
}

impl Hash for Description {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Timestamp(#[serde(with = "iso8601")] OffsetDateTime);

impl Timestamp {
    pub fn now() -> Timestamp {
        Timestamp(OffsetDateTime::now_utc())
    }

    pub fn isoformat(&self) -> Result<String, time::error::Format> {
        self.0.format(&Iso8601::DEFAULT)
    }
}

impl FromSql for Timestamp {
    fn column_result(value: ValueRef<'_>) -> FromSqlResult<Timestamp> {
        let as_string = String::column_result(value)?;
        let parsed = OffsetDateTime::parse(&as_string, &FORMAT)
            .map_err(|err| FromSqlError::Other(Box::new(err)))?;
        Ok(Timestamp(parsed))
    }
}

impl ToSql for Timestamp {
    fn to_sql(&self) -> rusqlite::Result<ToSqlOutput<'_>> {
        let encoded = self
            .isoformat()
            .map_err(|err| rusqlite::Error::ToSqlConversionFailure(Box::new(err)))?;
        encoded.leak().to_sql()
    }
}

const FORMAT_CONFIG: EncodedConfig = Config::DEFAULT.set_year_is_six_digits(false).encode();
const FORMAT: Iso8601<FORMAT_CONFIG> = Iso8601::<FORMAT_CONFIG>;
time::serde::format_description!(iso8601, OffsetDateTime, FORMAT);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Ciphertext(#[serde(with = "base64")] Vec<u8>);

impl Ciphertext {
    pub const fn new(value: Vec<u8>) -> Ciphertext {
        Ciphertext(value)
    }
}

impl AsRef<[u8]> for Ciphertext {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

impl FromStr for Ciphertext {
    type Err = DecodeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let decoded = BASE64.decode(s.as_bytes())?;
        Ok(Ciphertext(decoded))
    }
}

impl ToString for Ciphertext {
    fn to_string(&self) -> String {
        BASE64.encode(&self.0)
    }
}

impl FromSql for Ciphertext {
    fn column_result(value: ValueRef<'_>) -> FromSqlResult<Ciphertext> {
        let bytes = value.as_bytes()?;
        let decoded = BASE64.decode(bytes).map_err(|err| FromSqlError::Other(Box::new(err)))?;
        Ok(Ciphertext(decoded))
    }
}

impl ToSql for Ciphertext {
    fn to_sql(&self) -> rusqlite::Result<ToSqlOutput<'_>> {
        let encoded = BASE64.encode(&self.0);
        encoded.leak().to_sql()
    }
}

mod base64 {
    use data_encoding::BASE64;
    use serde::{de::Error, Deserializer, Serializer};

    struct Vis;
    impl serde::de::Visitor<'_> for Vis {
        type Value = Vec<u8>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("a base64 string")
        }

        fn visit_str<E: Error>(self, v: &str) -> Result<Self::Value, E> {
            BASE64.decode(v.as_bytes()).map_err(Error::custom)
        }
    }

    pub fn serialize<S: Serializer>(v: &[u8], s: S) -> Result<S::Ok, S::Error> {
        let base64 = BASE64.encode(v);
        s.serialize_str(&base64)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Vec<u8>, D::Error> {
        d.deserialize_str(Vis)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Entry {
    pub timestamp: Timestamp,
    #[serde(rename = "id")]
    pub entry_id: EntryId,
    pub key_id: KeyId,
    pub description: Description,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub identity: Option<Identity>,
    pub ciphertext: Ciphertext,
    #[serde(skip_serializing_if = "Option::is_none", rename = "meta")]
    pub metadata: Option<Metadata>,
}

impl PartialEq for Entry {
    fn eq(&self, other: &Entry) -> bool {
        self.entry_id == other.entry_id
    }
}

impl Eq for Entry {}

impl Hash for Entry {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.entry_id.0.hash(state)
    }
}

impl Entry {
    pub fn update(&mut self) -> Result<(), time::error::Format> {
        self.timestamp = Timestamp::now();
        self.entry_id = self.fresh_entry_id()?;
        Ok(())
    }

    fn fresh_entry_id(&self) -> Result<EntryId, time::error::Format> {
        EntryId::make(&self.key_id, &self.timestamp, &self.description, self.identity.as_ref())
    }
}

#[cfg(test)]
mod tests {
    use std::{fs, path::PathBuf};

    use serde::Serialize;
    use serde_json::{ser::PrettyFormatter, Serializer};

    use super::Entry;

    const EXAMPLE_PATH: [&str; 3] = [r"example", "db", "data.json"];

    #[test]
    fn roundtrip_example() {
        let path: PathBuf = EXAMPLE_PATH.iter().collect();
        let expected = fs::read_to_string(path).unwrap();
        let entries: Vec<Entry> = serde_json::from_str(&expected).unwrap();
        let actual = {
            let mut buf = Vec::new();
            let formatter = PrettyFormatter::with_indent(b"    ");
            let mut ser = Serializer::with_formatter(&mut buf, formatter);
            entries.serialize(&mut ser).unwrap();
            let mut ret = String::from_utf8(buf).unwrap();
            ret.push('\n');
            ret
        };
        #[cfg(target_os = "windows")]
        let actual = actual.replace("\n", "\r\n");
        assert_eq!(expected, actual);
    }

    #[test]
    fn check_ids() {
        let path: PathBuf = EXAMPLE_PATH.iter().collect();
        let expected = fs::read_to_string(path).unwrap();
        let entries: Vec<Entry> = serde_json::from_str(&expected).unwrap();
        for entry in entries {
            let fresh_id = entry.fresh_entry_id().unwrap();
            assert_eq!(entry.entry_id, fresh_id);
        }
    }
}
