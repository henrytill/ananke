mod schema;

use std::{
    hash::{Hash, Hasher},
    str::FromStr,
};

use data_encoding::{BASE64, DecodeError};
use rusqlite::types::{FromSql, FromSqlError, FromSqlResult, ToSql, ToSqlOutput, ValueRef};
use serde::{Deserialize, Serialize};
use time::{
    OffsetDateTime,
    format_description::well_known::{
        Iso8601,
        iso8601::{Config, EncodedConfig},
    },
};
use uuid::Uuid;
use zeroize::{DefaultIsZeroes, Zeroize, ZeroizeOnDrop};

pub use schema::{SchemaVersion, schema_version};

/// Wraps a [`String`] in a newtype
macro_rules! wrap_string {
    ($name:ident) => {
        /// A newtype that wraps a [`String`].
        #[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Zeroize, ZeroizeOnDrop)]
        pub struct $name(String);

        impl $name {
            pub const fn new(value: String) -> Self {
                Self(value)
            }

            pub fn as_str(&self) -> &str {
                self.0.as_str()
            }

            pub fn as_bytes(&self) -> &[u8] {
                self.0.as_bytes()
            }
        }

        impl From<String> for $name {
            fn from(value: String) -> Self {
                Self(value)
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.0.fmt(f)
            }
        }

        impl AsRef<str> for $name {
            #[inline]
            fn as_ref(&self) -> &str {
                self.0.as_ref()
            }
        }

        impl From<&str> for $name {
            fn from(s: &str) -> Self {
                Self(s.to_string())
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

wrap_string!(KeyId);
wrap_string!(Description);
wrap_string!(Identity);
wrap_string!(Metadata);
wrap_string!(ArmoredCiphertext);

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

#[cfg(test)]
impl From<&str> for Plaintext {
    fn from(name: &str) -> Plaintext {
        Plaintext(name.to_string())
    }
}

impl std::fmt::Display for Plaintext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct EntryId(Uuid);

impl Default for EntryId {
    fn default() -> EntryId {
        EntryId(Uuid::nil())
    }
}

impl DefaultIsZeroes for EntryId {}

impl EntryId {
    pub fn new() -> EntryId {
        EntryId(Uuid::new_v4())
    }
}

impl ToSql for EntryId {
    fn to_sql(&self) -> rusqlite::Result<ToSqlOutput<'_>> {
        self.0.to_string().leak().to_sql()
    }
}

impl FromSql for EntryId {
    fn column_result(value: ValueRef<'_>) -> FromSqlResult<EntryId> {
        let uuid_str = String::column_result(value)?;
        EntryId::try_from(uuid_str).map_err(|e| FromSqlError::Other(Box::new(e)))
    }
}

impl std::fmt::Display for EntryId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl TryFrom<String> for EntryId {
    type Error = uuid::Error;

    fn try_from(value: String) -> Result<EntryId, Self::Error> {
        let inner = Uuid::parse_str(value.as_str())?;
        Ok(EntryId(inner))
    }
}

impl FromStr for EntryId {
    type Err = uuid::Error;

    fn from_str(s: &str) -> Result<EntryId, Self::Err> {
        let inner = Uuid::parse_str(s)?;
        Ok(EntryId(inner))
    }
}

impl Description {
    pub fn contains(&self, pat: impl AsRef<str>) -> bool {
        self.0.contains(pat.as_ref())
    }
}

impl Hash for Description {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl Identity {
    pub fn contains(&self, pat: impl AsRef<str>) -> bool {
        self.0.contains(pat.as_ref())
    }
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub struct Timestamp(#[serde(with = "iso8601")] OffsetDateTime);

impl Timestamp {
    pub fn now() -> Timestamp {
        Timestamp(OffsetDateTime::now_utc())
    }

    pub fn isoformat(&self) -> Result<String, time::error::Format> {
        self.0.format(&Iso8601::DEFAULT)
    }
}

impl Default for Timestamp {
    fn default() -> Self {
        Timestamp(OffsetDateTime::UNIX_EPOCH)
    }
}

impl DefaultIsZeroes for Timestamp {}

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

#[derive(Debug, Clone, Serialize, Deserialize, Zeroize, ZeroizeOnDrop)]
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

    fn from_str(s: &str) -> Result<Ciphertext, Self::Err> {
        let decoded = BASE64.decode(s.as_bytes())?;
        Ok(Ciphertext(decoded))
    }
}

impl std::fmt::Display for Ciphertext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", BASE64.encode(&self.0))
    }
}

impl FromSql for Ciphertext {
    fn column_result(value: ValueRef<'_>) -> FromSqlResult<Ciphertext> {
        let bytes = value.as_bytes()?;
        let decoded = BASE64
            .decode(bytes)
            .map_err(|err| FromSqlError::Other(Box::new(err)))?;
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
    use serde::{
        Deserializer, Serializer,
        de::{Error, Visitor},
    };

    struct Vis;

    impl Visitor<'_> for Vis {
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

#[derive(Debug, Clone, Serialize, Deserialize, Zeroize, ZeroizeOnDrop)]
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
    pub fn update(&mut self) {
        self.timestamp = Timestamp::now();
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Zeroize, ZeroizeOnDrop)]
#[serde(rename_all = "camelCase")]
pub struct SecureIndexElement {
    pub entry_id: EntryId,
    pub key_id: KeyId,
    pub description: Description,
}

#[derive(Debug, Clone, Serialize, Deserialize, Zeroize, ZeroizeOnDrop)]
#[serde(rename_all = "camelCase")]
pub struct SecureEntry {
    pub timestamp: Timestamp,
    #[serde(rename = "id")]
    pub entry_id: EntryId,
    pub key_id: KeyId,
    pub description: Description,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub identity: Option<Identity>,
    pub plaintext: Plaintext,
    #[serde(skip_serializing_if = "Option::is_none", rename = "meta")]
    pub metadata: Option<Metadata>,
}

impl PartialEq for SecureEntry {
    fn eq(&self, other: &Self) -> bool {
        self.description == other.description
    }
}

impl Eq for SecureEntry {}

impl Hash for SecureEntry {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.description.0.hash(state)
    }
}

impl SecureEntry {
    pub fn update(&mut self) {
        self.timestamp = Timestamp::now();
    }
}
