use serde::{Deserialize, Serialize};
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

        #[allow(dead_code)]
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

        impl From<&str> for $name {
            fn from(name: &str) -> Self {
                Self(name.to_string())
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

#[derive(Serialize, Deserialize, Debug)]
pub struct Timestamp(#[serde(with = "iso8601")] OffsetDateTime);

const FORMAT_CONFIG: EncodedConfig = Config::DEFAULT.set_year_is_six_digits(false).encode();
const FORMAT: Iso8601<FORMAT_CONFIG> = Iso8601::<FORMAT_CONFIG>;
time::serde::format_description!(iso8601, OffsetDateTime, FORMAT);

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Ciphertext(#[serde(with = "base64")] Vec<u8>);

impl Ciphertext {
    pub const fn new(value: Vec<u8>) -> Self {
        Self(value)
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

#[derive(Serialize, Deserialize, Debug)]
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
