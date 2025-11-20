pub struct CommitInfo {
    pub short_commit_hash: String,
    pub commit_hash: String,
    pub maybe_commit_date: Option<String>,
}

pub struct VersionInfo {
    pub version: String,
    pub maybe_commit_info: Option<CommitInfo>,
}

impl std::fmt::Display for VersionInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.version)?;
        if let Some(ref ci) = self.maybe_commit_info {
            if let Some(ref commit_date) = ci.maybe_commit_date {
                write!(f, " ({} {})", ci.short_commit_hash, commit_date)?;
            } else {
                write!(f, " ({})", ci.short_commit_hash)?;
            }
        }
        Ok(())
    }
}

macro_rules! option_env_str {
    ($name:expr) => {
        option_env!($name).map(ToString::to_string)
    };
}

fn commit_info() -> Option<CommitInfo> {
    let short_commit_hash = option_env_str!("ANANKE_COMMIT_SHORT_HASH")?;
    let commit_hash = option_env_str!("ANANKE_COMMIT_HASH")?;
    let maybe_commit_date = option_env_str!("ANANKE_COMMIT_DATE");
    Some(CommitInfo {
        short_commit_hash,
        commit_hash,
        maybe_commit_date,
    })
}

#[must_use]
pub fn version_info() -> VersionInfo {
    let version = env!("CARGO_PKG_VERSION").to_string();
    let maybe_commit_info = commit_info();
    VersionInfo {
        version,
        maybe_commit_info,
    }
}
