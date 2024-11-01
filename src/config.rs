use std::{convert::Infallible, env, fs, path::PathBuf, str::FromStr};

use anyhow::Error;
use configparser::ini::Ini;
use directories::ProjectDirs;

use crate::data::KeyId;

const MSG_PROJECT_DIRS: &str = "could not create ProjectDirs";
const MSG_MISSING_CONFIG_DIR: &str = "missing config_dir";
const MSG_MISSING_DATA_DIR: &str = "missing data_dir";
const MSG_MISSING_KEY_ID: &str = "missing key_id";

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub enum Backend {
    #[default]
    Json = 0,
    Sqlite = 1,
    Text = 2,
}

impl TryFrom<u8> for Backend {
    type Error = ();

    fn try_from(value: u8) -> Result<Backend, Self::Error> {
        match value {
            0 => Ok(Backend::Json),
            1 => Ok(Backend::Sqlite),
            2 => Ok(Backend::Text),
            _ => Err(()),
        }
    }
}

impl FromStr for Backend {
    type Err = ();

    fn from_str(s: &str) -> Result<Backend, Self::Err> {
        match s.to_lowercase().as_str() {
            "sqlite" => Ok(Backend::Sqlite),
            "json" => Ok(Backend::Json),
            "text" => Ok(Backend::Text),
            _ => Err(()),
        }
    }
}

impl std::fmt::Display for Backend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Backend::Json => "json",
            Backend::Sqlite => "sqlite",
            Backend::Text => "text",
        };
        write!(f, "{}", str)
    }
}

#[derive(Debug)]
pub struct Config {
    #[allow(dead_code)]
    config_dir: PathBuf,
    data_dir: PathBuf,
    backend: Backend,
    key_id: KeyId,
    #[allow(dead_code)]
    mult_keys: bool,
}

impl Config {
    pub fn backend(&self) -> Backend {
        self.backend
    }

    pub fn key_id(&self) -> &KeyId {
        &self.key_id
    }

    pub fn db_dir(&self) -> PathBuf {
        let mut ret = self.data_dir.clone();
        ret.push("db");
        ret
    }

    pub fn schema_file(&self) -> PathBuf {
        let mut ret = self.db_dir().clone();
        ret.push("schema");
        ret
    }

    pub fn db(&self) -> PathBuf {
        let mut ret = self.db_dir().clone();
        match self.backend {
            Backend::Json => ret.push("data.json"),
            Backend::Sqlite => ret.push("db.sqlite"),
            Backend::Text => ret.push("index.asc"),
        }
        ret
    }

    pub fn pretty_print(&self) -> String {
        format!(
            "\
config_dir = {}
data_dir = {}
backend = {}
key_id = {}
allow_multiple_keys = {}",
            self.config_dir.display(),
            self.data_dir.display(),
            self.backend,
            self.key_id,
            self.mult_keys
        )
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

    fn from_str(s: &str) -> Result<Flag, Self::Err> {
        match s {
            "true" | "t" | "yes" | "y" | "1" => Ok(Flag(true)),
            _ => Ok(Flag(false)),
        }
    }
}

impl std::fmt::Display for Flag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub struct ConfigBuilder<'a> {
    getenv: Box<dyn Fn(&'a str) -> Result<String, env::VarError> + 'a>,
    maybe_config_dir: Option<PathBuf>,
    maybe_data_dir: Option<PathBuf>,
    maybe_backend: Option<Backend>,
    maybe_key_id: Option<KeyId>,
    mult_keys: Flag,
}

impl<'a> std::fmt::Debug for ConfigBuilder<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ConfigBuilder")
            .field("maybe_config_dir", &self.maybe_config_dir)
            .field("maybe_data_dir", &self.maybe_data_dir)
            .field("backend", &self.maybe_backend)
            .field("maybe_key_id", &self.maybe_key_id)
            .field("mult_keys", &self.mult_keys)
            .finish()
    }
}

impl<'a> PartialEq for ConfigBuilder<'a> {
    fn eq(&self, other: &ConfigBuilder) -> bool {
        self.maybe_config_dir == other.maybe_config_dir
            && self.maybe_data_dir == other.maybe_data_dir
            && self.maybe_backend == other.maybe_backend
            && self.maybe_key_id == other.maybe_key_id
            && self.mult_keys == other.mult_keys
    }
}

impl<'a> Eq for ConfigBuilder<'a> {}

struct IniSelector {
    section: &'static str,
    key: &'static str,
}

impl IniSelector {
    const fn new(section: &'static str, key: &'static str) -> IniSelector {
        IniSelector { section, key }
    }
}

impl<'a> ConfigBuilder<'a> {
    const PROJECT_QUALIFIER: &'static str = "com.github";
    const PROJECT_ORGANIZATION: &'static str = "henrytill";
    const PROJECT_NAME: &'static str = "ananke";
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

    pub fn new(
        getenv: impl Fn(&'a str) -> Result<String, std::env::VarError> + 'a,
    ) -> ConfigBuilder<'a> {
        ConfigBuilder {
            getenv: Box::new(getenv),
            maybe_config_dir: None,
            maybe_data_dir: None,
            maybe_backend: None,
            mult_keys: Flag::default(),
            maybe_key_id: None,
        }
    }

    pub fn with_defaults(mut self) -> Result<ConfigBuilder<'a>, Error> {
        let project_dirs = ProjectDirs::from(
            Self::PROJECT_QUALIFIER,
            Self::PROJECT_ORGANIZATION,
            Self::PROJECT_NAME,
        )
        .ok_or_else(|| Error::msg(MSG_PROJECT_DIRS))?;

        let config_dir: PathBuf = if let Ok(config_dir) = (self.getenv)(Self::ENV_CONFIG_DIR) {
            config_dir.into()
        } else {
            project_dirs.config_dir().into()
        };
        self.maybe_config_dir = Some(config_dir);

        let data_dir: PathBuf = project_dirs.data_dir().into();
        self.maybe_data_dir = Some(data_dir);

        Ok(self)
    }

    pub fn with_ini(mut self, maybe_input: Option<String>) -> Result<ConfigBuilder<'a>, Error> {
        let input = if let Some(input) = maybe_input {
            input
        } else if let Some(mut path) = self.maybe_config_dir.clone() {
            path.push(Self::CONFIG_FILE);
            if path.exists() {
                fs::read_to_string(path)?
            } else {
                String::new()
            }
        } else {
            return Err(Error::msg(MSG_MISSING_CONFIG_DIR));
        };

        let mut config = Ini::new();
        config.read(input).map_err(Error::msg)?;

        if let Some(data_dir) = config.get(Self::INI_DATA_DIR.section, Self::INI_DATA_DIR.key) {
            self.maybe_data_dir = Some(PathBuf::from(data_dir))
        }

        if let Some(backend) = config.get(Self::INI_BACKEND.section, Self::INI_BACKEND.key) {
            self.maybe_backend = backend.parse::<Backend>().ok()
        }

        if let Some(key_id) = config.get(Self::INI_KEY_ID.section, Self::INI_KEY_ID.key) {
            self.maybe_key_id = Some(KeyId::from(key_id))
        }

        if let Some(mult_keys) = config.get(Self::INI_MULT_KEYS.section, Self::INI_MULT_KEYS.key) {
            self.mult_keys = mult_keys.parse::<Flag>()?
        }

        Ok(self)
    }

    pub fn with_env(mut self) -> Result<ConfigBuilder<'a>, Error> {
        if let Ok(data_dir) = (self.getenv)(Self::ENV_DATA_DIR) {
            self.maybe_data_dir = Some(PathBuf::from(data_dir))
        }

        if let Ok(backend) = (self.getenv)(Self::ENV_BACKEND) {
            self.maybe_backend = backend.parse::<Backend>().ok()
        }

        if let Ok(key_id) = (self.getenv)(Self::ENV_KEY_ID) {
            self.maybe_key_id = Some(KeyId::from(key_id))
        }

        if let Ok(mult_keys) = (self.getenv)(Self::ENV_MULT_KEYS) {
            self.mult_keys = mult_keys.parse::<Flag>()?
        }

        Ok(self)
    }

    pub fn build(mut self) -> Result<Config, Error> {
        let config_dir =
            self.maybe_config_dir.take().ok_or_else(|| Error::msg(MSG_MISSING_CONFIG_DIR))?;
        let data_dir =
            self.maybe_data_dir.take().ok_or_else(|| Error::msg(MSG_MISSING_DATA_DIR))?;
        let backend = self.maybe_backend.unwrap_or(Backend::Json);
        let key_id = self.maybe_key_id.take().ok_or_else(|| Error::msg(MSG_MISSING_KEY_ID))?;
        let mult_keys = self.mult_keys.into();
        Ok(Config { config_dir, data_dir, backend, key_id, mult_keys })
    }

    pub fn maybe_config_dir(&self) -> Option<&PathBuf> {
        self.maybe_config_dir.as_ref()
    }

    pub fn maybe_data_dir(&self) -> Option<&PathBuf> {
        self.maybe_data_dir.as_ref()
    }

    pub fn maybe_key_id(&self) -> Option<&KeyId> {
        self.maybe_key_id.as_ref()
    }

    pub fn maybe_key_id_mut(&mut self) -> &mut Option<KeyId> {
        &mut self.maybe_key_id
    }

    pub fn maybe_backend(&self) -> Option<&Backend> {
        self.maybe_backend.as_ref()
    }

    pub fn maybe_backend_mut(&mut self) -> &mut Option<Backend> {
        &mut self.maybe_backend
    }

    pub fn maybe_config_file(&self) -> Option<PathBuf> {
        self.maybe_config_dir.as_ref().map(|config_dir| {
            let mut tmp = config_dir.clone();
            tmp.push("ananke.ini");
            tmp
        })
    }

    pub fn ini(&self) -> Option<String> {
        match (self.maybe_backend, self.maybe_data_dir.as_ref(), self.maybe_key_id.as_ref()) {
            (Some(backend), Some(data_dir), Some(key_id)) => Some(format!(
                "\
[data]
backend={}
dir={}
[gpg]
key_id={}
",
                backend,
                data_dir.display(),
                key_id,
            )),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{env::VarError, path::PathBuf};

    use super::{Backend, ConfigBuilder, Flag, MSG_MISSING_CONFIG_DIR};
    use crate::data::KeyId;

    fn empty_getenv(_var: &str) -> Result<String, VarError> {
        Err(VarError::NotPresent)
    }

    #[test]
    fn with_ini_parses_ini() {
        let data_dir = "/tmp/data";
        let backend = Backend::Sqlite;
        let key_id = KeyId::from("371C136C");
        let mult_keys = Flag::from(true);
        let expected = ConfigBuilder {
            getenv: Box::new(empty_getenv),
            maybe_config_dir: None,
            maybe_data_dir: Some(PathBuf::from(data_dir)),
            maybe_backend: Some(backend),
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
            data_dir, backend, key_id, mult_keys
        );
        let actual = ConfigBuilder::new(empty_getenv).with_ini(Some(input)).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn with_ini_parses_empty_ini() {
        let expected = ConfigBuilder::new(empty_getenv);
        let input = String::new();
        let actual = ConfigBuilder::new(empty_getenv).with_ini(Some(input)).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn with_ini_returns_missing_config_dir() {
        let result = ConfigBuilder::new(empty_getenv).with_ini(None);
        if let Err(err) = result {
            if MSG_MISSING_CONFIG_DIR == err.to_string() {
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
            getenv: Box::new(&getenv),
            maybe_config_dir: None,
            maybe_data_dir: Some(PathBuf::from(data_dir)),
            maybe_backend: Some(backend),
            maybe_key_id: Some(key_id),
            mult_keys,
        };
        let actual = ConfigBuilder::new(&getenv).with_env().unwrap();
        assert_eq!(expected, actual);
    }
}
