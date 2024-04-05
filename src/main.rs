use std::{
    io::{self, BufRead, Write},
    process::ExitCode,
};

use clap::{Arg, ArgGroup, Command};

fn command() -> Command {
    let version = version::version_info().to_string();

    let add = {
        let arg_description =
            Arg::new("description").value_name("DESCRIPTION").help("URL or description");
        let arg_identity = Arg::new("identity")
            .short('i')
            .long("identity")
            .value_name("IDENTITY")
            .help("username or email address");
        let arg_meta = Arg::new("metadata")
            .short('m')
            .long("meta")
            .value_name("METADATA")
            .help("additional metadata");
        Command::new("add")
            .about("add an entry")
            .arg(arg_description)
            .arg(arg_identity)
            .arg(arg_meta)
            .arg_required_else_help(true)
    };

    let lookup = {
        let arg_description =
            Arg::new("description").value_name("DESCRIPTION").help("URL or description");
        let arg_identity = Arg::new("identity")
            .short('i')
            .long("identity")
            .value_name("IDENTITY")
            .help("username or email address");
        let arg_verbose = Arg::new("verbose")
            .short('v')
            .long("verbose")
            .num_args(0)
            .help("enable verbose output");
        Command::new("lookup")
            .about("lookup an entry")
            .arg(arg_description)
            .arg(arg_identity)
            .arg(arg_verbose)
            .arg_required_else_help(true)
    };

    let modify = {
        let arg_description = Arg::new("description")
            .short('d')
            .long("description")
            .value_name("DESCRIPTION")
            .help("URL or description");
        let arg_entry_id = Arg::new("entry-id")
            .short('e')
            .long("entry-id")
            .value_name("ENTRY_ID")
            .help("entry_id");
        let arg_plaintext = Arg::new("plaintext")
            .short('p')
            .long("plaintext")
            .num_args(0)
            .help("prompt for plaintext");
        let arg_identity = Arg::new("identity")
            .short('i')
            .long("identity")
            .value_name("IDENTITY")
            .help("username or email address");
        let arg_meta = Arg::new("metadata")
            .short('m')
            .long("meta")
            .value_name("METADATA")
            .help("additional metadata");
        Command::new("modify")
            .about("modify an entry")
            .arg(arg_description)
            .arg(arg_entry_id)
            .arg(arg_plaintext)
            .arg(arg_identity)
            .arg(arg_meta)
            .group(ArgGroup::new("modify").args(["description", "entry-id"]))
            .arg_required_else_help(true)
    };

    let remove = {
        let arg_description = Arg::new("description")
            .short('d')
            .long("description")
            .value_name("DESCRIPTION")
            .help("URL or description");
        let arg_entry_id = Arg::new("entry-id")
            .short('e')
            .long("entry-id")
            .value_name("ENTRY_ID")
            .help("entry_id");
        Command::new("remove")
            .about("remove an entry")
            .arg(arg_description)
            .arg(arg_entry_id)
            .group(ArgGroup::new("remove").args(["description", "entry-id"]))
            .arg_required_else_help(true)
    };

    let import = {
        let arg_file = Arg::new("file").value_name("FILE").help("file to import from");
        Command::new("import")
            .about("import entries from JSON file")
            .arg(arg_file)
            .arg_required_else_help(true)
    };

    let export = {
        let arg_file = Arg::new("file").value_name("FILE").help("file to export to");
        Command::new("export")
            .about("export entries to JSON file")
            .arg(arg_file)
            .arg_required_else_help(true)
    };

    Command::new("ananke")
        .about("A password manager")
        .version(version)
        .subcommand_required(true)
        .subcommand(add)
        .subcommand(lookup)
        .subcommand(modify)
        .subcommand(remove)
        .subcommand(import)
        .subcommand(export)
        .arg_required_else_help(true)
}

fn prompt(display: &str) -> Result<String, io::Error> {
    print!("{}", display);
    io::stdout().flush()?;
    let mut ret = String::new();
    let stdin = io::stdin();
    stdin.lock().read_line(&mut ret)?;
    ret.truncate(ret.len() - 1);
    Ok(ret)
}

fn run() -> Result<ExitCode, Box<dyn std::error::Error>> {
    let matches = command().get_matches();
    match matches.subcommand() {
        Some(("add", sub_matches)) => {
            let description = sub_matches.get_one::<String>("description").cloned().unwrap();
            let identity = sub_matches.get_one::<String>("identity").cloned();
            let metadata = sub_matches.get_one::<String>("metadata").cloned();
            let plaintext = prompt("Enter plaintext: ")?;
            command::add(description, plaintext, identity, metadata)
        }
        Some(("lookup", sub_matches)) => {
            let description = sub_matches.get_one::<String>("description").cloned().unwrap();
            let identity = sub_matches.get_one::<String>("identity").cloned();
            let verbose = sub_matches.get_one::<bool>("verbose").copied().unwrap_or_default();
            command::lookup(description, identity, verbose)
        }
        Some(("modify", sub_matches)) => {
            let description = sub_matches.get_one::<String>("description").cloned();
            let entry_id = sub_matches.get_one::<String>("entry-id").cloned();
            let plaintext = sub_matches.get_one::<bool>("plaintext").copied().unwrap_or_default();
            let identity = sub_matches.get_one::<String>("identity").cloned();
            let metadata = sub_matches.get_one::<String>("metadata").cloned();
            let maybe_plaintext = if plaintext {
                let plaintext = prompt("Enter plaintext: ")?;
                Some(plaintext)
            } else {
                None
            };
            command::modify(description, entry_id, None, maybe_plaintext, identity, metadata)
        }
        Some(("remove", sub_matches)) => {
            let description = sub_matches.get_one::<String>("description").cloned();
            let entry_id = sub_matches.get_one::<String>("entry-id").cloned();
            command::remove(description, entry_id)
        }
        Some(("import", sub_matches)) => {
            let file = sub_matches.get_one::<String>("file").cloned().unwrap();
            command::import(file)
        }
        Some(("export", sub_matches)) => {
            let file = sub_matches.get_one::<String>("file").cloned().unwrap();
            command::export(file)
        }
        Some((&_, _)) => panic!(),
        None => panic!(),
    }
}

fn main() -> ExitCode {
    match run() {
        Ok(code) => code,
        Err(err) => {
            eprintln!("Error: {}", err);
            ExitCode::FAILURE
        }
    }
}

mod command {
    use std::{path::PathBuf, process::ExitCode};

    use ananke::{
        application::{
            common::{Application, Target},
            json::JsonApplication,
            sqlite::SqliteApplication,
        },
        config::{Backend, Config, ConfigBuilder},
        data::{Description, Entry, Id, Identity, Metadata, Plaintext},
    };

    type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

    fn configure() -> Result<Config> {
        let mut config_builder = ConfigBuilder::new();
        config_builder = config_builder
            .with_dirs(&std::env::var)?
            .with_config(None)?
            .with_env(&std::env::var)?;
        let config = config_builder.build()?;
        Ok(config)
    }

    fn format_brief(entry: &Entry, plaintext: &Plaintext) -> String {
        let description = entry.description.to_string();
        let identity = entry
            .identity
            .as_ref()
            .map(ToString::to_string)
            .unwrap_or_else(|| String::from("<none>"));
        let plaintext = plaintext.to_string();
        format!("{} {} {}", description, identity, plaintext)
    }

    fn format_verbose(entry: &Entry, plaintext: &Plaintext) -> Result<String> {
        let mut elements = vec![
            entry.timestamp.isoformat()?,
            entry.id.to_string(),
            entry.key_id.to_string(),
            entry.description.to_string(),
        ];

        if let Some(ref identity) = entry.identity {
            elements.push(identity.to_string())
        }

        elements.push(plaintext.to_string());

        if let Some(ref metadata) = entry.metadata {
            elements.push(format!("\"{}\"", metadata.to_string()))
        }

        Ok(elements.join(" "))
    }

    fn format_results(results: &[(Entry, Plaintext)], verbose: bool) -> Result<String> {
        if results.len() == 1 {
            let (entry, plaintext) = &results[0];
            if verbose {
                return format_verbose(entry, plaintext);
            } else {
                return Ok(plaintext.to_string());
            }
        }

        let mut formatted_results = Vec::new();

        for (entry, plaintext) in results {
            let formatted = if verbose {
                format_verbose(entry, plaintext)?
            } else {
                format_brief(entry, plaintext)
            };
            formatted_results.push(formatted);
        }

        Ok(formatted_results.join("\n"))
    }

    pub fn add(
        description: String,
        plaintext: String,
        maybe_identity: Option<String>,
        maybe_metadata: Option<String>,
    ) -> Result<ExitCode> {
        let description = Description::from(description);
        let plaintext = Plaintext::from(plaintext);
        let maybe_identity = maybe_identity.map(Identity::from);
        let maybe_metadata = maybe_metadata.map(Metadata::from);
        let config = configure()?;
        match config.backend() {
            Backend::Json => {
                let mut app = JsonApplication::new(config)?;
                app.add(description, plaintext, maybe_identity, maybe_metadata)?;
            }
            Backend::Sqlite => {
                let mut app = SqliteApplication::new(config)?;
                app.add(description, plaintext, maybe_identity, maybe_metadata)?;
            }
        }
        Ok(ExitCode::SUCCESS)
    }

    pub fn lookup(
        description: String,
        maybe_identity: Option<String>,
        verbose: bool,
    ) -> Result<ExitCode> {
        let description = Description::from(description);
        let maybe_identity = maybe_identity.map(Identity::from);
        let config = configure()?;
        let results = match config.backend() {
            Backend::Json => {
                let app = JsonApplication::new(config)?;
                app.lookup(description, maybe_identity)?
            }
            Backend::Sqlite => {
                let app = SqliteApplication::new(config)?;
                app.lookup(description, maybe_identity)?
            }
        };
        if results.is_empty() {
            return Ok(ExitCode::FAILURE);
        }
        let output = format_results(&results, verbose)?;
        println!("{}", output);
        Ok(ExitCode::SUCCESS)
    }

    pub fn modify(
        target_description: Option<String>,
        target_id: Option<String>,
        maybe_description: Option<String>,
        maybe_plaintext: Option<String>,
        maybe_identity: Option<String>,
        maybe_metadata: Option<String>,
    ) -> Result<ExitCode> {
        let target = match (target_description, target_id) {
            (Some(d), None) => Target::Description(Description::from(d)),
            (None, Some(i)) => Target::Id(Id::from(i)),
            (Some(_), Some(_)) => panic!(),
            (None, None) => panic!(),
        };
        let maybe_description = maybe_description.map(Description::from);
        let maybe_plaintext = maybe_plaintext.map(Plaintext::from);
        let maybe_identity = maybe_identity.map(Identity::from);
        let maybe_metadata = maybe_metadata.map(Metadata::from);
        let config = configure()?;
        match config.backend() {
            Backend::Json => {
                let mut app = JsonApplication::new(config)?;
                app.modify(
                    target,
                    maybe_description,
                    maybe_plaintext,
                    maybe_identity,
                    maybe_metadata,
                )?;
            }
            Backend::Sqlite => {
                let mut app = SqliteApplication::new(config)?;
                app.modify(
                    target,
                    maybe_description,
                    maybe_plaintext,
                    maybe_identity,
                    maybe_metadata,
                )?;
            }
        }
        Ok(ExitCode::SUCCESS)
    }

    pub fn remove(
        target_description: Option<String>,
        target_id: Option<String>,
    ) -> Result<ExitCode> {
        let target = match (target_description, target_id) {
            (Some(d), None) => Target::Description(Description::from(d)),
            (None, Some(i)) => Target::Id(Id::from(i)),
            (Some(_), Some(_)) => panic!(),
            (None, None) => panic!(),
        };
        let config = configure()?;
        match config.backend() {
            Backend::Json => {
                let mut app = JsonApplication::new(config)?;
                app.remove(target)?;
            }
            Backend::Sqlite => {
                let mut app = SqliteApplication::new(config)?;
                app.remove(target)?;
            }
        }
        Ok(ExitCode::SUCCESS)
    }

    pub fn import(path: String) -> Result<ExitCode> {
        let path = PathBuf::from(path);
        let config = configure()?;
        match config.backend() {
            Backend::Json => {
                let mut app = JsonApplication::new(config)?;
                app.import(path)?;
            }
            Backend::Sqlite => {
                let mut app = SqliteApplication::new(config)?;
                app.import(path)?;
            }
        }
        Ok(ExitCode::SUCCESS)
    }

    pub fn export(path: String) -> Result<ExitCode> {
        let path = PathBuf::from(path);
        let config = configure()?;
        match config.backend() {
            Backend::Json => {
                let app = JsonApplication::new(config)?;
                app.export(path)?;
            }
            Backend::Sqlite => {
                let app = SqliteApplication::new(config)?;
                app.export(path)?;
            }
        }
        Ok(ExitCode::SUCCESS)
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
