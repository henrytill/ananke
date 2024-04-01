use std::{
    env,
    ffi::OsString,
    fmt, fs, io,
    path::PathBuf,
    str::{FromStr, ParseBoolError},
};

use configparser::ini::Ini;

use crate::data::KeyId;

#[derive(Debug)]
pub enum Error {
    Var(env::VarError, String),
    Io(io::Error),
    Ini(String),
    MissingConfigDir,
    MissingDataDir,
    MissingKeyId,
}

impl From<(env::VarError, String)> for Error {
    fn from((err, var): (env::VarError, String)) -> Self {
        Error::Var(err, var)
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Io(err)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Var(err, var) => write!(f, "{}: {}", err, var),
            Error::Io(err) => write!(f, "{}", err),
            Error::Ini(why) => write!(f, "ini: {}", why),
            Error::MissingConfigDir => write!(f, "missing config_dir"),
            Error::MissingDataDir => write!(f, "missing data_dir"),
            Error::MissingKeyId => write!(f, "missing key_id"),
        }
    }
}

impl std::error::Error for Error {}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub enum Backend {
    #[default]
    Json,
    Sqlite,
}

impl FromStr for Backend {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "sqlite" | "SQLITE" | "Sqlite" | "SQLite" => Ok(Backend::Sqlite),
            "json" | "JSON" | "Json" => Ok(Backend::Json),
            _ => Err(()),
        }
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
}

#[derive(Debug, Default, PartialEq, Eq)]
struct Bool(bool);

impl From<Bool> for bool {
    fn from(value: Bool) -> Self {
        value.0
    }
}

impl From<bool> for Bool {
    fn from(value: bool) -> Self {
        Bool(value)
    }
}

impl FromStr for Bool {
    type Err = ParseBoolError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "true" | "t" | "yes" | "y" | "1" => Ok(Bool(true)),
            _ => Ok(Bool(false)),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ConfigBuilder {
    name: String,
    maybe_config_dir: Option<PathBuf>,
    maybe_data_dir: Option<PathBuf>,
    backend: Backend,
    maybe_key_id: Option<KeyId>,
    mult_keys: Bool,
}

impl ConfigBuilder {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            maybe_config_dir: None,
            maybe_data_dir: None,
            backend: Backend::default(),
            mult_keys: Bool::default(),
            maybe_key_id: None,
        }
    }

    pub fn with_defaults(
        mut self,
        getenv: &impl Fn(OsString) -> Result<String, env::VarError>,
    ) -> Result<ConfigBuilder, Error> {
        let config_dir = internal::config_dir(getenv, self.name.as_str())?;
        let data_dir = internal::data_dir(getenv, self.name.as_str())?;
        self.maybe_config_dir = Some(config_dir);
        self.maybe_data_dir = Some(data_dir);
        Ok(self)
    }

    pub fn with_config(
        mut self,
        maybe_input: Option<impl Into<String>>,
    ) -> Result<ConfigBuilder, Error> {
        let input = if let Some(input) = maybe_input {
            input.into()
        } else if let Some(mut path) = self.maybe_config_dir.to_owned() {
            path.push(format!("{}.ini", self.name));
            fs::read_to_string(path)?
        } else {
            return Err(Error::MissingConfigDir);
        };
        let mut config = Ini::new();
        config.read(input).map_err(Error::Ini)?;
        if let maybe_data_dir @ Some(_) = config
            .get("data", "dir")
            .and_then(|data_dir| data_dir.parse::<PathBuf>().ok())
        {
            self.maybe_data_dir = maybe_data_dir;
        }
        if let Some(backend) = config
            .get("data", "backend")
            .and_then(|backend| backend.parse::<Backend>().ok())
        {
            self.backend = backend;
        }
        if let Some(mult_keys) = config
            .get("data", "allow_multiple_keys")
            .and_then(|mult_keys| mult_keys.parse::<Bool>().ok())
        {
            self.mult_keys = mult_keys
        }
        if let maybe_key_id @ Some(_) = config.get("gpg", "key_id").map(KeyId::from) {
            self.maybe_key_id = maybe_key_id;
        }
        Ok(self)
    }

    pub fn with_env(
        mut self,
        getenv: &impl Fn(OsString) -> Result<String, env::VarError>,
    ) -> Result<ConfigBuilder, Error> {
        let name = self.name.to_uppercase();
        if let maybe_data_dir @ Some(_) = getenv(format!("{}_DATA_DIR", name).into())
            .ok()
            .and_then(|data_dir| data_dir.parse::<PathBuf>().ok())
        {
            self.maybe_data_dir = maybe_data_dir
        }
        if let Some(backend) = getenv(format!("{}_BACKEND", name).into())
            .ok()
            .and_then(|backend| backend.parse::<Backend>().ok())
        {
            self.backend = backend
        }
        if let Some(mult_keys) = getenv(format!("{}_ALLOW_MULTIPLE_KEYS", name).into())
            .ok()
            .and_then(|mult_keys| mult_keys.parse::<Bool>().ok())
        {
            self.mult_keys = mult_keys
        }
        if let maybe_key_id @ Some(_) = getenv(format!("{}_KEY_ID", name).into())
            .ok()
            .map(KeyId::from)
        {
            self.maybe_key_id = maybe_key_id
        }
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
        let mult_keys = self.mult_keys.into();
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
        env::VarError,
        ffi::OsString,
        path::{Path, PathBuf},
    };

    use cfg_if::cfg_if;

    #[inline]
    pub fn config_dir(
        getenv: &impl Fn(OsString) -> Result<String, VarError>,
        name: impl AsRef<Path>,
    ) -> Result<PathBuf, (VarError, String)> {
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
        getenv: &impl Fn(OsString) -> Result<String, VarError>,
        name: impl AsRef<Path>,
    ) -> Result<PathBuf, (VarError, String)> {
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
            ffi::OsString,
            path::{Path, PathBuf},
        };

        #[inline]
        pub fn local_app_data(
            getenv: &impl Fn(OsString) -> Result<String, VarError>,
            name: impl AsRef<Path>,
        ) -> Result<PathBuf, (VarError, String)> {
            let var = "LOCALAPPDATA";
            match getenv(var.into()) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(name);
                    Ok(ret)
                }
                Err(err) => Err((err, var.into())),
            }
        }

        #[inline]
        pub fn app_data(
            getenv: &impl Fn(OsString) -> Result<String, VarError>,
            name: impl AsRef<Path>,
        ) -> Result<PathBuf, (VarError, String)> {
            let var = "APPDATA";
            match getenv(var.into()) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(name);
                    Ok(ret)
                }
                Err(err) => Err((err, var.into())),
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
                let expected = Err((VarError::NotPresent, String::from("LOCALAPPDATA")));
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
                let expected = Err((VarError::NotPresent, String::from("APPDATA")));
                let actual = super::app_data(&getenv, "test");
                assert_eq!(expected, actual);
            }
        }
    }

    #[cfg(target_os = "macos")]
    mod macos {
        use std::{
            env::VarError,
            ffi::OsString,
            path::{Path, PathBuf},
        };

        #[inline]
        pub fn support_dir(
            getenv: &impl Fn(OsString) -> Result<String, VarError>,
            name: impl AsRef<Path>,
        ) -> Result<PathBuf, (VarError, String)> {
            let var = "HOME";
            match getenv(var.into()) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push("Library");
                    ret.push("Application Support");
                    ret.push(name);
                    Ok(ret)
                }
                Err(err) => Err((err, var.into())),
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
                let expected: PathBuf = [home, "Library", "Application Support", "test"]
                    .into_iter()
                    .collect();
                let actual = super::support_dir(&getenv, "test").expect("should return path");
                assert_eq!(expected, actual)
            }

            #[test]
            fn support_dir_returns_err() {
                let getenv = |_| Err(VarError::NotPresent);
                let expected = Err((VarError::NotPresent, String::from("HOME")));
                let actual = super::support_dir(&getenv, "test");
                assert_eq!(expected, actual);
            }
        }
    }

    #[cfg(not(any(target_os = "windows", target_os = "macos")))]
    mod xdg {
        use std::{
            env::VarError,
            ffi::OsString,
            path::{Path, PathBuf},
        };

        #[inline]
        pub fn config_dir(
            getenv: &impl Fn(OsString) -> Result<String, VarError>,
            name: impl AsRef<Path>,
        ) -> Result<PathBuf, (VarError, String)> {
            let var = "XDG_CONFIG_DIR";
            match getenv(var.into()) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(name);
                    return Ok(ret);
                }
                Err(err @ VarError::NotUnicode(_)) => return Err((err, var.into())),
                Err(VarError::NotPresent) => (),
            }
            let var = "HOME";
            match getenv(var.into()) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(".config");
                    ret.push(name);
                    Ok(ret)
                }
                Err(err) => Err((err, var.into())),
            }
        }

        #[inline]
        pub fn data_dir(
            getenv: &impl Fn(OsString) -> Result<String, VarError>,
            name: impl AsRef<Path>,
        ) -> Result<PathBuf, (VarError, String)> {
            let var = "XDG_DATA_DIR";
            match getenv(var.into()) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(name);
                    return Ok(ret);
                }
                Err(err @ VarError::NotUnicode(_)) => return Err((err, var.into())),
                Err(VarError::NotPresent) => (),
            }
            let var = "HOME";
            match getenv(var.into()) {
                Ok(dir) => {
                    let mut ret = PathBuf::from(dir);
                    ret.push(".local");
                    ret.push("share");
                    ret.push(name);
                    Ok(ret)
                }
                Err(err) => Err((err, var.into())),
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
                let expected = Err((VarError::NotPresent, String::from("HOME")));
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
                let expected = Err((VarError::NotPresent, String::from("HOME")));
                let actual = super::data_dir(&getenv, "test");
                assert_eq!(expected, actual);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{env::VarError, ffi::OsString, path::PathBuf};

    use super::{Backend, ConfigBuilder, Error};
    use crate::data::KeyId;

    #[test]
    fn with_config_parses_ini() {
        let expected = ConfigBuilder {
            name: String::from("test"),
            maybe_config_dir: None,
            maybe_data_dir: Some(PathBuf::from("/tmp/ananke")),
            backend: Backend::Sqlite,
            maybe_key_id: Some(KeyId::from("371C136C")),
            mult_keys: true.into(),
        };
        let input = "\
[data]
backend=sqlite
dir=/tmp/ananke
allow_multiple_keys=true

[gpg]
key_id=371C136C
";
        let actual = ConfigBuilder::new("test")
            .with_config(Some(input))
            .expect("should parse");
        assert_eq!(expected, actual);
    }

    #[test]
    fn with_config_parses_empty_ini() {
        let expected = ConfigBuilder::new("test");
        let input = String::new();
        let actual = ConfigBuilder::new("test")
            .with_config(Some(input))
            .expect("should parse");
        assert_eq!(expected, actual);
    }

    #[test]
    fn with_config_returns_missing_config_dir() {
        if let Err(Error::MissingConfigDir) =
            ConfigBuilder::new("test").with_config(Option::<String>::None)
        {
            return;
        }
        panic!("Expected: {:?}", Error::MissingConfigDir)
    }

    #[test]
    fn with_env_parses_env() {
        let data_dir = "/tmp/data";
        let key_id = "371C136C";
        let getenv = |s: OsString| match s.to_str() {
            Some("TEST_DATA_DIR") => Ok(String::from(data_dir)),
            Some("TEST_BACKEND") => Ok(String::from("sqlite")),
            Some("TEST_ALLOW_MULTIPLE_KEYS") => Ok(String::from("true")),
            Some("TEST_KEY_ID") => Ok(String::from(key_id)),
            _ => Err(VarError::NotPresent),
        };
        let expected = ConfigBuilder {
            name: String::from("test"),
            maybe_config_dir: None,
            maybe_data_dir: Some(PathBuf::from(data_dir)),
            backend: Backend::Sqlite,
            maybe_key_id: Some(KeyId::from(key_id)),
            mult_keys: true.into(),
        };
        let actual = ConfigBuilder::new("test")
            .with_env(&getenv)
            .expect("should parse");
        assert_eq!(expected, actual);
    }
}
