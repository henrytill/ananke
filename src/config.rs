use std::{env, fmt, path::PathBuf};

use crate::data::KeyId;

#[derive(Debug)]
pub enum Error {
    Var(env::VarError, String),
    MissingConfigDir,
    MissingDataDir,
    MissingKeyId,
}

impl From<(env::VarError, String)> for Error {
    fn from((err, msg): (env::VarError, String)) -> Self {
        Error::Var(err, msg)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Var(err, string) => write!(f, "{}: {}", err, string),
            Error::MissingConfigDir => write!(f, "missing config_dir"),
            Error::MissingDataDir => write!(f, "missing data_dir"),
            Error::MissingKeyId => write!(f, "missing key_id"),
        }
    }
}

impl std::error::Error for Error {}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Backend {
    Sqlite,
    Json,
}

#[derive(Debug)]
pub struct Config {
    config_dir: PathBuf,
    data_dir: PathBuf,
    backend: Backend,
    key_id: KeyId,
    mult_keys: bool,
}

impl Config {
    pub fn config_dir(&self) -> &PathBuf {
        &self.config_dir
    }

    pub fn data_dir(&self) -> &PathBuf {
        &self.data_dir
    }

    pub fn backend(&self) -> Backend {
        self.backend
    }

    pub fn key_id(&self) -> &KeyId {
        &self.key_id
    }

    pub fn mult_keys(&self) -> bool {
        self.mult_keys
    }
}

#[derive(Debug)]
pub struct ConfigBuilder {
    name: String,
    maybe_config_dir: Option<PathBuf>,
    maybe_data_dir: Option<PathBuf>,
    backend: Backend,
    maybe_key_id: Option<KeyId>,
    mult_keys: bool,
}

impl ConfigBuilder {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            maybe_config_dir: None,
            maybe_data_dir: None,
            backend: Backend::Json,
            mult_keys: false,
            maybe_key_id: None,
        }
    }

    pub fn with_defaults(mut self) -> Result<ConfigBuilder, Error> {
        let config_dir = internal::config_dir(&self.name)?;
        let data_dir = internal::data_dir(&self.name)?;
        self.maybe_config_dir = Some(config_dir);
        self.maybe_data_dir = Some(data_dir);
        Ok(self)
    }

    pub fn build(mut self) -> Result<Config, Error> {
        let config_dir = self
            .maybe_config_dir
            .take()
            .ok_or(Error::MissingConfigDir)?;
        let data_dir = self.maybe_data_dir.take().ok_or(Error::MissingDataDir)?;
        let backend = self.backend;
        let key_id = self.maybe_key_id.take().ok_or(Error::MissingKeyId)?;
        let mult_keys = self.mult_keys;
        Ok(Config {
            config_dir,
            data_dir,
            backend,
            key_id,
            mult_keys,
        })
    }
}

mod internal {
    use std::{
        env,
        path::{Path, PathBuf},
    };

    use cfg_if::cfg_if;

    #[inline]
    pub fn config_dir(name: impl AsRef<Path>) -> Result<PathBuf, (env::VarError, String)> {
        cfg_if! {
            if #[cfg(target_os = "windows")] {
                windows::local_app_data(name)
            } else if #[cfg(target_os = "macos")] {
                macos::support_dir(name)
            } else {
                xdg::config_dir(name)
            }
        }
    }

    #[inline]
    pub fn data_dir(name: impl AsRef<Path>) -> Result<PathBuf, (env::VarError, String)> {
        cfg_if! {
            if #[cfg(target_os = "windows")] {
                windows::app_data(name)
            } else if #[cfg(target_os = "macos")] {
                macos::support_dir(name)
            } else {
                xdg::data_dir(name)
            }
        }
    }

    #[cfg(target_os = "windows")]
    mod windows {
        use std::{
            env,
            path::{Path, PathBuf},
        };

        #[inline]
        pub fn local_app_data(name: impl AsRef<Path>) -> Result<PathBuf, (env::VarError, String)> {
            let var = "LOCALAPPDATA";
            match env::var(var) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(name);
                    Ok(ret)
                }
                Err(err) => Err((err, String::from(var))),
            }
        }

        #[inline]
        pub fn app_data(name: impl AsRef<Path>) -> Result<PathBuf, (env::VarError, String)> {
            let var = "APPDATA";
            match env::var(var) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(name);
                    Ok(ret)
                }
                Err(err) => Err((err, String::from(var))),
            }
        }
    }

    #[cfg(target_os = "macos")]
    mod macos {
        use std::{
            env,
            path::{Path, PathBuf},
        };

        #[inline]
        pub fn support_dir(name: impl AsRef<Path>) -> Result<PathBuf, (env::VarError, String)> {
            let var = "HOME";
            match env::var(var) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push("Library");
                    ret.push("Application Support");
                    ret.push(name);
                    Ok(ret)
                }
                Err(err) => Err((err, String::from(var))),
            }
        }
    }

    #[cfg(not(any(target_os = "windows", target_os = "macos")))]
    mod xdg {
        use std::{
            env,
            env::VarError,
            path::{Path, PathBuf},
        };

        #[inline]
        pub fn config_dir(name: impl AsRef<Path>) -> Result<PathBuf, (env::VarError, String)> {
            let var = "XDG_CONFIG_DIR";
            match env::var(var) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(name);
                    return Ok(ret);
                }
                Err(err @ VarError::NotUnicode(_)) => return Err((err, String::from(var))),
                Err(VarError::NotPresent) => (),
            }
            let var = "HOME";
            match env::var(var) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(".config");
                    ret.push(name);
                    Ok(ret)
                }
                Err(err) => Err((err, String::from(var))),
            }
        }

        #[inline]
        pub fn data_dir(name: impl AsRef<Path>) -> Result<PathBuf, (env::VarError, String)> {
            let var = "XDG_DATA_DIR";
            match env::var(var) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(name);
                    return Ok(ret);
                }
                Err(err @ VarError::NotUnicode(_)) => return Err((err, String::from(var))),
                Err(VarError::NotPresent) => (),
            }
            let var = "HOME";
            match env::var(var) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(".local");
                    ret.push("share");
                    ret.push(name);
                    Ok(ret)
                }
                Err(err) => Err((err, String::from(var))),
            }
        }
    }

    #[cfg(test)]
    mod tests {}
}
