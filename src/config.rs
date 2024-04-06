use std::{
    backtrace::Backtrace, convert::Infallible, env, fmt, fs, io, path::PathBuf, str::FromStr,
};

use configparser::ini::Ini;

use crate::data::KeyId;

#[derive(Debug)]
pub enum ErrorKind {
    Var(env::VarError, &'static str),
    Io(io::Error),
    Ini(String),
    MissingConfigDir,
    MissingDataDir,
    MissingKeyId,
}

#[derive(Debug)]
pub struct ErrorImpl {
    kind: ErrorKind,
    backtrace: Option<Backtrace>,
}

#[derive(Debug)]
pub struct Error {
    inner: Box<ErrorImpl>,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind() {
            ErrorKind::Var(err, var) => write!(f, "{}: {}", err, var),
            ErrorKind::Io(err) => err.fmt(f),
            ErrorKind::Ini(err) => err.fmt(f),
            ErrorKind::MissingConfigDir => write!(f, "missing config_dir"),
            ErrorKind::MissingDataDir => write!(f, "missing data_dir"),
            ErrorKind::MissingKeyId => write!(f, "missing key_id"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self.kind() {
            ErrorKind::Var(err, _) => Some(err),
            ErrorKind::Io(err) => Some(err),
            ErrorKind::Ini(_) => None,
            ErrorKind::MissingConfigDir => None,
            ErrorKind::MissingDataDir => None,
            ErrorKind::MissingKeyId => None,
        }
    }
}

impl From<(env::VarError, &'static str)> for Error {
    fn from((err, var): (env::VarError, &'static str)) -> Error {
        let kind = ErrorKind::Var(err, var);
        Error::new(kind)
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        let kind = ErrorKind::Io(err);
        Error::new(kind)
    }
}

impl From<Infallible> for Error {
    fn from(_: Infallible) -> Error {
        unreachable!()
    }
}

impl Error {
    fn new(kind: ErrorKind) -> Error {
        let backtrace = Some(Backtrace::capture());
        let inner = Box::new(ErrorImpl { kind, backtrace });
        Error { inner }
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.inner.kind
    }

    pub fn backtrace(&mut self) -> Option<Backtrace> {
        self.inner.backtrace.take()
    }

    fn ini(value: String) -> Error {
        let kind = ErrorKind::Ini(value);
        Error::new(kind)
    }

    fn missing_config_dir() -> Error {
        let kind = ErrorKind::MissingConfigDir;
        Error::new(kind)
    }

    fn missing_data_dir() -> Error {
        let kind = ErrorKind::MissingDataDir;
        Error::new(kind)
    }

    fn missing_key_id() -> Error {
        let kind = ErrorKind::MissingKeyId;
        Error::new(kind)
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
        if let Ok(config_dir) = getenv(Self::ENV_CONFIG_DIR) {
            self.maybe_config_dir = Some(PathBuf::from(config_dir))
        } else {
            let config_dir = internal::config_dir(getenv, Self::NAME)?;
            self.maybe_config_dir = Some(config_dir)
        }

        let data_dir = internal::data_dir(getenv, Self::NAME)?;
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
        let config_dir = self.maybe_config_dir.take().ok_or(Error::missing_config_dir())?;
        let data_dir = self.maybe_data_dir.take().ok_or(Error::missing_data_dir())?;
        let backend = self.backend;
        let key_id = self.maybe_key_id.take().ok_or(Error::missing_key_id())?;
        let mult_keys = self.mult_keys.into();
        Ok(Config { config_dir, data_dir, backend, key_id, mult_keys })
    }
}

mod internal {
    use std::{
        env::VarError,
        path::{Path, PathBuf},
    };

    use cfg_if::cfg_if;

    #[inline]
    pub fn config_dir(
        getenv: &impl Fn(&'static str) -> Result<String, VarError>,
        name: impl AsRef<Path>,
    ) -> Result<PathBuf, (VarError, &'static str)> {
        cfg_if! {
            if #[cfg(target_os = "windows")] {
                windows::local_app_data(getenv, name)
            } else if #[cfg(target_os = "macos")] {
                macos::support_dir(getenv, name)
            } else {
                xdg::config_dir(getenv, name)
            }
        }
    }

    #[inline]
    pub fn data_dir(
        getenv: &impl Fn(&'static str) -> Result<String, VarError>,
        name: impl AsRef<Path>,
    ) -> Result<PathBuf, (VarError, &'static str)> {
        cfg_if! {
            if #[cfg(target_os = "windows")] {
                windows::app_data(getenv, name)
            } else if #[cfg(target_os = "macos")] {
                macos::support_dir(getenv, name)
            } else {
                xdg::data_dir(getenv, name)
            }
        }
    }

    #[cfg(target_os = "windows")]
    mod windows {
        use std::{
            env::VarError,
            path::{Path, PathBuf},
        };

        #[inline]
        pub fn local_app_data(
            getenv: &impl Fn(&'static str) -> Result<String, VarError>,
            name: impl AsRef<Path>,
        ) -> Result<PathBuf, (VarError, &'static str)> {
            let var = "LOCALAPPDATA";
            match getenv(var) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(name);
                    Ok(ret)
                }
                Err(err) => Err((err, var)),
            }
        }

        #[inline]
        pub fn app_data(
            getenv: &impl Fn(&'static str) -> Result<String, VarError>,
            name: impl AsRef<Path>,
        ) -> Result<PathBuf, (VarError, &'static str)> {
            let var = "APPDATA";
            match getenv(var) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(name);
                    Ok(ret)
                }
                Err(err) => Err((err, var)),
            }
        }

        #[cfg(test)]
        mod tests {
            use std::{env::VarError, path::PathBuf};

            #[test]
            fn local_app_data_returns_ok() {
                let local_app_data = "C:\\Users\\test\\AppData\\Local";
                let getenv = |s| {
                    if s == "LOCALAPPDATA" {
                        Ok(local_app_data.into())
                    } else {
                        Err(VarError::NotPresent)
                    }
                };
                let expected: PathBuf = [local_app_data, "test"].into_iter().collect();
                let actual = super::local_app_data(&getenv, "test").expect("should return path");
                assert_eq!(expected, actual)
            }

            #[test]
            fn local_app_data_returns_err() {
                let getenv = |_| Err(VarError::NotPresent);
                let expected = Err((VarError::NotPresent, "LOCALAPPDATA"));
                let actual = super::local_app_data(&getenv, "test");
                assert_eq!(expected, actual);
            }

            #[test]
            fn app_data_returns_ok() {
                let app_data = "C:\\Users\\test\\AppData\\Roaming";
                let getenv = |s| {
                    if s == "APPDATA" {
                        Ok(app_data.into())
                    } else {
                        Err(VarError::NotPresent)
                    }
                };
                let expected: PathBuf = [app_data, "test"].into_iter().collect();
                let actual = super::app_data(&getenv, "test").expect("should return path");
                assert_eq!(expected, actual)
            }

            #[test]
            fn app_data_returns_err() {
                let getenv = |_| Err(VarError::NotPresent);
                let expected = Err((VarError::NotPresent, "APPDATA"));
                let actual = super::app_data(&getenv, "test");
                assert_eq!(expected, actual);
            }
        }
    }

    #[cfg(target_os = "macos")]
    mod macos {
        use std::{
            env::VarError,
            path::{Path, PathBuf},
        };

        #[inline]
        pub fn support_dir(
            getenv: &impl Fn(&'static str) -> Result<String, VarError>,
            name: impl AsRef<Path>,
        ) -> Result<PathBuf, (VarError, &'static str)> {
            let var = "HOME";
            match getenv(var) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push("Library");
                    ret.push("Application Support");
                    ret.push(name);
                    Ok(ret)
                }
                Err(err) => Err((err, var)),
            }
        }

        #[cfg(test)]
        mod tests {
            use std::{env::VarError, path::PathBuf};

            #[test]
            fn support_dir_returns_ok() {
                let home = "/Users/test";
                let getenv = |s| {
                    if s == "HOME" {
                        Ok(home.into())
                    } else {
                        Err(VarError::NotPresent)
                    }
                };
                let expected: PathBuf =
                    [home, "Library", "Application Support", "test"].into_iter().collect();
                let actual = super::support_dir(&getenv, "test").expect("should return path");
                assert_eq!(expected, actual)
            }

            #[test]
            fn support_dir_returns_err() {
                let getenv = |_| Err(VarError::NotPresent);
                let expected = Err((VarError::NotPresent, "HOME"));
                let actual = super::support_dir(&getenv, "test");
                assert_eq!(expected, actual);
            }
        }
    }

    #[cfg(not(any(target_os = "windows", target_os = "macos")))]
    mod xdg {
        use std::{
            env::VarError,
            path::{Path, PathBuf},
        };

        #[inline]
        pub fn config_dir(
            getenv: &impl Fn(&'static str) -> Result<String, VarError>,
            name: impl AsRef<Path>,
        ) -> Result<PathBuf, (VarError, &'static str)> {
            let var = "XDG_CONFIG_DIR";
            match getenv(var) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(name);
                    return Ok(ret);
                }
                Err(err @ VarError::NotUnicode(_)) => return Err((err, var)),
                Err(VarError::NotPresent) => (),
            }
            let var = "HOME";
            match getenv(var) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(".config");
                    ret.push(name);
                    Ok(ret)
                }
                Err(err) => Err((err, var)),
            }
        }

        #[inline]
        pub fn data_dir(
            getenv: &impl Fn(&'static str) -> Result<String, VarError>,
            name: impl AsRef<Path>,
        ) -> Result<PathBuf, (VarError, &'static str)> {
            let var = "XDG_DATA_DIR";
            match getenv(var) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(name);
                    return Ok(ret);
                }
                Err(err @ VarError::NotUnicode(_)) => return Err((err, var)),
                Err(VarError::NotPresent) => (),
            }
            let var = "HOME";
            match getenv(var) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(".local");
                    ret.push("share");
                    ret.push(name);
                    Ok(ret)
                }
                Err(err) => Err((err, var)),
            }
        }

        #[cfg(test)]
        mod tests {
            use std::{env::VarError, path::PathBuf};

            #[test]
            fn config_dir_returns_ok() {
                let home = "/home/test";
                let getenv = |s| {
                    if s == "HOME" {
                        Ok(home.into())
                    } else {
                        Err(VarError::NotPresent)
                    }
                };
                let expected: PathBuf = [home, ".config", "test"].into_iter().collect();
                let actual = super::config_dir(&getenv, "test").expect("should return path");
                assert_eq!(expected, actual)
            }

            #[test]
            fn config_dir_returns_ok_custom_xdg() {
                let xdg_config_dir = "/tmp/config";
                let getenv = |s| {
                    if s == "XDG_CONFIG_DIR" {
                        Ok(xdg_config_dir.into())
                    } else {
                        Err(VarError::NotPresent)
                    }
                };
                let expected: PathBuf = [xdg_config_dir, "test"].into_iter().collect();
                let actual = super::config_dir(&getenv, "test").expect("should return path");
                assert_eq!(expected, actual);
            }

            #[test]
            fn config_dir_returns_err() {
                let getenv = |_| Err(VarError::NotPresent);
                let expected = Err((VarError::NotPresent, "HOME"));
                let actual = super::config_dir(&getenv, "test");
                assert_eq!(expected, actual);
            }

            #[test]
            fn data_dir_returns_ok() {
                let home = "/home/test";
                let getenv = |s| {
                    if s == "HOME" {
                        Ok(home.into())
                    } else {
                        Err(VarError::NotPresent)
                    }
                };
                let expected: PathBuf = [home, ".local", "share", "test"].into_iter().collect();
                let actual = super::data_dir(&getenv, "test").expect("should return path");
                assert_eq!(expected, actual)
            }

            #[test]
            fn data_dir_returns_ok_custom_xdg() {
                let xdg_data_dir = "/tmp/data";
                let getenv = |s| {
                    if s == "XDG_DATA_DIR" {
                        Ok(xdg_data_dir.into())
                    } else {
                        Err(VarError::NotPresent)
                    }
                };
                let expected: PathBuf = [xdg_data_dir, "test"].into_iter().collect();
                let actual = super::data_dir(&getenv, "test").expect("should return path");
                assert_eq!(expected, actual);
            }

            #[test]
            fn data_dir_returns_err() {
                let getenv = |_| Err(VarError::NotPresent);
                let expected = Err((VarError::NotPresent, "HOME"));
                let actual = super::data_dir(&getenv, "test");
                assert_eq!(expected, actual);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{env::VarError, path::PathBuf};

    use super::{Backend, ConfigBuilder, ErrorKind, Flag};
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
            if let ErrorKind::MissingConfigDir = err.kind() {
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
