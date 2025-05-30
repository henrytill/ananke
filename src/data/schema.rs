use std::{fs, num::ParseIntError, path::Path, str::FromStr};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SchemaVersion(u64);

impl SchemaVersion {
    pub const CURRENT: SchemaVersion = SchemaVersion::new(4);

    pub const fn new(value: u64) -> SchemaVersion {
        SchemaVersion(value)
    }
}

impl FromStr for SchemaVersion {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<SchemaVersion, Self::Err> {
        u64::from_str(s).map(SchemaVersion::new)
    }
}

impl std::fmt::Display for SchemaVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl From<u64> for SchemaVersion {
    fn from(value: u64) -> SchemaVersion {
        SchemaVersion(value)
    }
}

pub fn schema_version(path: impl AsRef<Path>) -> Result<SchemaVersion, anyhow::Error> {
    if path.as_ref().exists() {
        let contents = fs::read_to_string(path)?;
        let schema_version = SchemaVersion::from_str(contents.as_str())?;
        return Ok(schema_version);
    }

    let schema_version = SchemaVersion::CURRENT;
    if let Some(parent) = path.as_ref().parent() {
        if !parent.exists() {
            fs::create_dir_all(parent)?;
        }
    }
    fs::write(path, schema_version.to_string())?;
    Ok(schema_version)
}
