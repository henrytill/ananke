use std::{fs, num::ParseIntError, path::Path, str::FromStr};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SchemaVersion(u64);

impl SchemaVersion {
    pub const CURRENT: SchemaVersion = SchemaVersion::new(3);

    pub const fn new(value: u64) -> SchemaVersion {
        SchemaVersion(value)
    }
}

impl FromStr for SchemaVersion {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        u64::from_str(s).map(SchemaVersion::new)
    }
}

impl ToString for SchemaVersion {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

impl From<u64> for SchemaVersion {
    fn from(value: u64) -> Self {
        SchemaVersion::new(value)
    }
}

pub fn schema_version(path: impl AsRef<Path>) -> Result<SchemaVersion, anyhow::Error> {
    let schema_version = if path.as_ref().exists() {
        let contents = fs::read_to_string(path)?;
        SchemaVersion::from_str(contents.as_str())?
    } else {
        if let Some(parent) = path.as_ref().parent() {
            if !parent.exists() {
                fs::create_dir_all(parent)?;
            }
        }
        let schema_version = SchemaVersion::CURRENT;
        fs::write(path, schema_version.to_string())?;
        schema_version
    };
    Ok(schema_version)
}
