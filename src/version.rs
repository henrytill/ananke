pub struct CommitInfo {
    pub short_commit_hash: String,
    pub commit_hash: String,
    pub commit_date: String,
}

pub struct VersionInfo {
    pub version: String,
    pub commit_info: Option<CommitInfo>,
}

impl std::fmt::Display for VersionInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.version)?;
        if let Some(ref ci) = self.commit_info {
            write!(f, " ({} {})", ci.short_commit_hash, ci.commit_date)?;
        };
        Ok(())
    }
}

macro_rules! option_env_str {
    ($name:expr) => {
        option_env!($name).map(ToString::to_string)
    };
}

pub fn version_info() -> VersionInfo {
    let version = env!("CARGO_PKG_VERSION").to_string();
    let commit_info = option_env_str!("ANANKE_COMMIT_HASH").map(|commit_hash| {
        let short_commit_hash = option_env_str!("ANANKE_COMMIT_SHORT_HASH").unwrap();
        let commit_date = option_env_str!("ANANKE_COMMIT_DATE").unwrap();
        CommitInfo { short_commit_hash, commit_hash, commit_date }
    });
    VersionInfo { version, commit_info }
}
