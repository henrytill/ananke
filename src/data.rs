use std::hash::{Hash, Hasher};

use data_encoding::BASE64;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use time::{
    format_description::well_known::{
        iso8601::{Config, EncodedConfig},
        Iso8601,
    },
    OffsetDateTime,
};

/// Wraps a [`String`] in a newtype
macro_rules! wrap_string {
    ($name:ident) => {
        /// A newtype that wraps a [`String`].
        #[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
        pub struct $name(String);

        impl $name {
            pub const fn new(value: String) -> Self {
                Self(value)
            }

            pub fn into_inner(self) -> String {
                self.0
            }

            pub fn as_str(&self) -> &str {
                self.0.as_str()
            }
        }

        impl From<String> for $name {
            fn from(value: String) -> Self {
                Self(value)
            }
        }

        #[cfg(test)]
        impl From<&str> for $name {
            fn from(name: &str) -> Self {
                Self(name.to_string())
            }
        }

        impl ToString for $name {
            fn to_string(&self) -> String {
                self.0.to_string()
            }
        }
    };
}

wrap_string!(Id);
wrap_string!(KeyId);
wrap_string!(Description);
wrap_string!(Identity);
wrap_string!(Metadata);
wrap_string!(Plaintext);

impl Id {
    pub fn generate(
        key_id: &KeyId,
        timestamp: &Timestamp,
        description: &Description,
        maybe_identity: Option<&Identity>,
    ) -> Result<Id, time::error::Format> {
        let mut input =
            format!("{}{}{}", key_id.to_string(), timestamp.isoformat()?, description.to_string());
        if let Some(identity) = maybe_identity {
            input.push_str(identity.as_str())
        }
        let digest = {
            let mut hasher = Sha256::new();
            hasher.update(input.as_bytes());
            let bytes = hasher.finalize();
            BASE64.encode(&bytes)
        };
        Ok(Id(digest))
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

    fn isoformat(&self) -> Result<String, time::error::Format> {
        self.0.format(&Iso8601::DEFAULT)
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
    pub id: Id,
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
        self.id == other.id
    }
}

impl Eq for Entry {}

impl Hash for Entry {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.0.hash(state)
    }
}

impl Entry {
    pub fn update(&mut self) -> Result<(), time::error::Format> {
        let timestamp = Timestamp::now();
        let id = Id::generate(&self.key_id, &timestamp, &self.description, self.identity.as_ref())?;
        self.id = id;
        Ok(())
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
        let expected = fs::read_to_string(path).expect("should read file");
        let entries: Vec<Entry> = serde_json::from_str(&expected).expect("should deserialize");
        let actual = {
            let mut buf = Vec::new();
            let formatter = PrettyFormatter::with_indent(b"    ");
            let mut ser = Serializer::with_formatter(&mut buf, formatter);
            entries.serialize(&mut ser).expect("should serialize");
            let mut ret = String::from_utf8(buf).expect("should convert to string");
            ret.push('\n');
            ret
        };
        #[cfg(target_os = "windows")]
        let actual = actual.replace("\n", "\r\n");
        assert_eq!(expected, actual);
    }
}
