use anyhow::Result;
use clap::{Arg, Command};

fn command() -> Command {
    let version = version::version_info().to_string();

    let lookup = {
        let arg_description =
            Arg::new("DESCRIPTION").value_name("DESCRIPTION").help("Description or URL");
        Command::new("lookup")
            .about("Lookup an entry")
            .arg(arg_description)
            .arg_required_else_help(true)
    };

    Command::new("ananke")
        .about("A password manager")
        .version(version)
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(lookup)
}

fn main() -> Result<()> {
    let matches = command().get_matches();
    match matches.subcommand() {
        Some(("lookup", sub_matches)) => {
            let description = sub_matches.get_one::<String>("DESCRIPTION").expect("required");
            command::lookup(description, None)
        }
        Some((&_, _)) => panic!(),
        None => panic!(),
    }
}

mod command {
    use anyhow::Result;

    use ananke::{
        application::{json::JsonApplication, sqlite::SqliteApplication},
        config::{Backend, Config, ConfigBuilder},
        data::{Description, Entry, Identity, Plaintext},
    };

    fn configure() -> Result<Config> {
        let mut config_builder = ConfigBuilder::new();
        config_builder = config_builder
            .with_dirs(&std::env::var)?
            .with_config(Option::<String>::None)?
            .with_env(&std::env::var)?;
        let config = config_builder.build()?;
        Ok(config)
    }

    fn print_result(result: &[(Entry, Plaintext)], _verbose: bool) {
        if result.len() == 1 {
            let (_, plaintext) = &result[0];
            println!("{}", plaintext.to_string());
            return;
        }
        for (entry, plaintext) in result.iter() {
            println!(
                "{} {} {}",
                entry.description.as_str(),
                entry.identity.as_ref().map_or("<none>", Identity::as_str),
                plaintext.as_str()
            )
        }
    }

    pub fn lookup(description: &str, maybe_identity: Option<&str>) -> Result<()> {
        let description = Description::from(description);
        let maybe_identity = maybe_identity.map(Identity::from);
        let config = configure()?;
        match config.backend() {
            Backend::Json => {
                let app = JsonApplication::new(config)?;
                let result = app.lookup(description, maybe_identity)?;
                print_result(&result, false)
            }
            Backend::Sqlite => {
                let app = SqliteApplication::new(config)?;
                let result = app.lookup(description, maybe_identity)?;
                print_result(&result, false)
            }
        }
        Ok(())
    }
}

mod version {
    use std::fmt;

    pub struct CommitInfo {
        pub short_commit_hash: String,
        pub commit_hash: String,
        pub commit_date: String,
    }

    pub struct VersionInfo {
        pub version: String,
        pub commit_info: Option<CommitInfo>,
    }

    impl fmt::Display for VersionInfo {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
}

#[cfg(test)]
mod tests {
    #[test]
    fn main_test() {
        assert!(true)
    }
}
