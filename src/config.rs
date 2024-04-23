use std::{
    backtrace::Backtrace, convert::Infallible, env, fmt, fs, io, path::PathBuf, str::FromStr,
};

use configparser::ini::Ini;
use directories::ProjectDirs;

use crate::data::KeyId;

#[derive(Debug)]
pub enum ErrorInner {
    Var(env::VarError, &'static str),
    Io(io::Error),
    Ini(String),
    ProjectDirs,
    MissingConfigDir,
    MissingDataDir,
    MissingKeyId,
}

impl fmt::Display for ErrorInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorInner::Var(err, var) => write!(f, "{}: {}", err, var),
            ErrorInner::Io(err) => err.fmt(f),
            ErrorInner::Ini(err) => err.fmt(f),
            ErrorInner::ProjectDirs => write!(f, "could not create ProjectDirs"),
            ErrorInner::MissingConfigDir => write!(f, "missing config_dir"),
            ErrorInner::MissingDataDir => write!(f, "missing data_dir"),
            ErrorInner::MissingKeyId => write!(f, "missing key_id"),
        }
    }
}

#[derive(Debug)]
pub struct ErrorImpl {
    inner: ErrorInner,
    backtrace: Option<Backtrace>,
}

#[derive(Debug)]
pub struct Error {
    inner: Box<ErrorImpl>,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.inner.fmt(f)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self.inner.inner {
            ErrorInner::Var(ref err, _) => Some(err),
            ErrorInner::Io(ref err) => Some(err),
            ErrorInner::Ini(_) => None,
            ErrorInner::ProjectDirs => None,
            ErrorInner::MissingConfigDir => None,
            ErrorInner::MissingDataDir => None,
            ErrorInner::MissingKeyId => None,
        }
    }
}

impl From<(env::VarError, &'static str)> for Error {
    fn from((err, var): (env::VarError, &'static str)) -> Error {
        Error::capture(ErrorInner::Var(err, var))
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::capture(ErrorInner::Io(err))
    }
}

impl From<Infallible> for Error {
    fn from(_: Infallible) -> Error {
        unreachable!()
    }
}

impl Error {
    fn capture(inner: ErrorInner) -> Error {
        let backtrace = Some(Backtrace::capture());
        let inner = Box::new(ErrorImpl { inner, backtrace });
        Error { inner }
    }

    pub fn backtrace(&mut self) -> Option<Backtrace> {
        self.inner.backtrace.take()
    }

    fn ini(value: String) -> Error {
        Error::capture(ErrorInner::Ini(value))
    }

    fn project_dirs() -> Error {
        Error::capture(ErrorInner::ProjectDirs)
    }

    fn missing_config_dir() -> Error {
        Error::capture(ErrorInner::MissingConfigDir)
    }

    fn missing_data_dir() -> Error {
        Error::capture(ErrorInner::MissingDataDir)
    }

    fn missing_key_id() -> Error {
        Error::capture(ErrorInner::MissingKeyId)
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub enum Backend {
    #[default]
    Json,
    Sqlite,
}

impl FromStr for Backend {
    type Err = ();

    fn from_str(s: &str) -> Result<Backend, Self::Err> {
        match s {
            "sqlite" | "SQLITE" | "Sqlite" | "SQLite" => Ok(Backend::Sqlite),
            "json" | "JSON" | "Json" => Ok(Backend::Json),
            _ => Err(()),
        }
    }
}

impl ToString for Backend {
    fn to_string(&self) -> String {
        let str = match self {
            Backend::Json => "json",
            Backend::Sqlite => "sqlite",
        };
        String::from(str)
    }
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

    fn db_dir(&self) -> PathBuf {
        let mut ret = self.data_dir.to_owned();
        ret.push("db");
        ret
    }

    pub fn data_file(&self) -> PathBuf {
        let mut ret = self.db_dir().to_owned();
        match self.backend {
            Backend::Json => ret.push("data.json"),
            Backend::Sqlite => ret.push("db.sqlite"),
        }
        ret
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
struct Flag(bool);

impl From<Flag> for bool {
    fn from(value: Flag) -> bool {
        value.0
    }
}

impl From<bool> for Flag {
    fn from(value: bool) -> Flag {
        Flag(value)
    }
}

impl FromStr for Flag {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "true" | "t" | "yes" | "y" | "1" => Ok(Flag(true)),
            _ => Ok(Flag(false)),
        }
    }
}

impl ToString for Flag {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ConfigBuilder {
    maybe_config_dir: Option<PathBuf>,
    maybe_data_dir: Option<PathBuf>,
    backend: Backend,
    maybe_key_id: Option<KeyId>,
    mult_keys: Flag,
}

impl Default for ConfigBuilder {
    fn default() -> ConfigBuilder {
        ConfigBuilder::new()
    }
}

struct IniSelector {
    section: &'static str,
    key: &'static str,
}

impl IniSelector {
    const fn new(section: &'static str, key: &'static str) -> IniSelector {
        IniSelector { section, key }
    }
}

impl ConfigBuilder {
    const QUALIFIER: &'static str = "com.github";
    const ORGANIZATION: &'static str = "henrytill";
    const NAME: &'static str = "ananke";
    const CONFIG_FILE: &'static str = "ananke.ini";
    const ENV_CONFIG_DIR: &'static str = "ANANKE_CONFIG_DIR";
    const ENV_DATA_DIR: &'static str = "ANANKE_DATA_DIR";
    const ENV_BACKEND: &'static str = "ANANKE_BACKEND";
    const ENV_KEY_ID: &'static str = "ANANKE_KEY_ID";
    const ENV_MULT_KEYS: &'static str = "ANANKE_ALLOW_MULTIPLE_KEYS";
    const INI_DATA_DIR: IniSelector = IniSelector::new("data", "dir");
    const INI_BACKEND: IniSelector = IniSelector::new("data", "backend");
    const INI_KEY_ID: IniSelector = IniSelector::new("gpg", "key_id");
    const INI_MULT_KEYS: IniSelector = IniSelector::new("gpg", "allow_multiple_keys");

    pub fn new() -> ConfigBuilder {
        ConfigBuilder {
            maybe_config_dir: None,
            maybe_data_dir: None,
            backend: Backend::default(),
            mult_keys: Flag::default(),
            maybe_key_id: None,
        }
    }

    pub fn with_dirs(
        mut self,
        getenv: &impl Fn(&'static str) -> Result<String, env::VarError>,
    ) -> Result<ConfigBuilder, Error> {
        let project_dirs = ProjectDirs::from(Self::QUALIFIER, Self::ORGANIZATION, Self::NAME)
            .ok_or_else(Error::project_dirs)?;

        if let Ok(config_dir) = getenv(Self::ENV_CONFIG_DIR) {
            self.maybe_config_dir = Some(PathBuf::from(config_dir))
        } else {
            let config_dir = project_dirs.config_dir().into();
            self.maybe_config_dir = Some(config_dir);
        }

        let data_dir = project_dirs.data_dir().into();
        self.maybe_data_dir = Some(data_dir);

        Ok(self)
    }

    pub fn with_config(mut self, maybe_input: Option<String>) -> Result<ConfigBuilder, Error> {
        let input = if let Some(input) = maybe_input {
            input
        } else if let Some(mut path) = self.maybe_config_dir.to_owned() {
            path.push(Self::CONFIG_FILE);
            fs::read_to_string(path)?
        } else {
            return Err(Error::missing_config_dir());
        };

        let mut config = Ini::new();
        config.read(input).map_err(Error::ini)?;

        if let Some(data_dir) = config.get(Self::INI_DATA_DIR.section, Self::INI_DATA_DIR.key) {
            self.maybe_data_dir = Some(PathBuf::from(data_dir))
        }

        if let Some(backend) = config.get(Self::INI_BACKEND.section, Self::INI_BACKEND.key) {
            if let Ok(backend) = backend.parse::<Backend>() {
                self.backend = backend
            }
        }

        if let Some(key_id) = config.get(Self::INI_KEY_ID.section, Self::INI_KEY_ID.key) {
            self.maybe_key_id = Some(KeyId::from(key_id))
        }

        if let Some(mult_keys) = config.get(Self::INI_MULT_KEYS.section, Self::INI_MULT_KEYS.key) {
            self.mult_keys = mult_keys.parse::<Flag>()?
        }

        Ok(self)
    }

    pub fn with_env(
        mut self,
        getenv: &impl Fn(&'static str) -> Result<String, env::VarError>,
    ) -> Result<ConfigBuilder, Error> {
        if let Ok(data_dir) = getenv(Self::ENV_DATA_DIR) {
            self.maybe_data_dir = Some(PathBuf::from(data_dir))
        }

        if let Ok(backend) = getenv(Self::ENV_BACKEND) {
            if let Ok(backend) = backend.parse::<Backend>() {
                self.backend = backend
            }
        }

        if let Ok(key_id) = getenv(Self::ENV_KEY_ID) {
            self.maybe_key_id = Some(KeyId::from(key_id))
        }

        if let Ok(mult_keys) = getenv(Self::ENV_MULT_KEYS) {
            self.mult_keys = mult_keys.parse::<Flag>()?
        }

        Ok(self)
    }

    pub fn build(mut self) -> Result<Config, Error> {
        let config_dir = self.maybe_config_dir.take().ok_or_else(Error::missing_config_dir)?;
        let data_dir = self.maybe_data_dir.take().ok_or_else(Error::missing_data_dir)?;
        let backend = self.backend;
        let key_id = self.maybe_key_id.take().ok_or_else(Error::missing_key_id)?;
        let mult_keys = self.mult_keys.into();
        Ok(Config { config_dir, data_dir, backend, key_id, mult_keys })
    }
}

#[cfg(test)]
mod tests {
    use std::{env::VarError, path::PathBuf};

    use super::{Backend, ConfigBuilder, ErrorInner, Flag};
    use crate::data::KeyId;

    #[test]
    fn with_config_parses_ini() {
        let data_dir = "/tmp/data";
        let backend = Backend::Sqlite;
        let key_id = KeyId::from("371C136C");
        let mult_keys = Flag::from(true);
        let expected = ConfigBuilder {
            maybe_config_dir: None,
            maybe_data_dir: Some(PathBuf::from(data_dir)),
            backend,
            maybe_key_id: Some(key_id.clone()),
            mult_keys: true.into(),
        };
        let input = format!(
            "\
[data]
dir={}
backend={}

[gpg]
key_id={}
allow_multiple_keys={}
",
            data_dir,
            backend.to_string(),
            key_id.to_string(),
            mult_keys.to_string()
        );
        let actual = ConfigBuilder::new().with_config(Some(input)).expect("should parse");
        assert_eq!(expected, actual);
    }

    #[test]
    fn with_config_parses_empty_ini() {
        let expected = ConfigBuilder::new();
        let input = String::new();
        let actual = ConfigBuilder::new().with_config(Some(input)).expect("should parse");
        assert_eq!(expected, actual);
    }

    #[test]
    fn with_config_returns_missing_config_dir() {
        let result = ConfigBuilder::new().with_config(None);
        if let Err(err) = result {
            if let ErrorInner::MissingConfigDir = err.inner.inner {
                return;
            }
            panic!()
        }
        panic!()
    }

    #[test]
    fn with_env_parses_env() {
        let data_dir = "/tmp/data";
        let backend = Backend::Sqlite;
        let key_id = KeyId::from("371C136C");
        let mult_keys = Flag::from(true);
        let getenv = {
            let key_id = key_id.clone();
            move |s| match s {
                ConfigBuilder::ENV_DATA_DIR => Ok(String::from(data_dir)),
                ConfigBuilder::ENV_BACKEND => Ok(backend.clone().to_string()),
                ConfigBuilder::ENV_KEY_ID => Ok(key_id.to_string()),
                ConfigBuilder::ENV_MULT_KEYS => Ok(mult_keys.to_string()),
                _ => Err(VarError::NotPresent),
            }
        };
        let expected = ConfigBuilder {
            maybe_config_dir: None,
            maybe_data_dir: Some(PathBuf::from(data_dir)),
            backend,
            maybe_key_id: Some(key_id),
            mult_keys,
        };
        let actual = ConfigBuilder::new().with_env(&getenv).expect("should parse");
        assert_eq!(expected, actual);
    }
}
