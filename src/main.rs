use std::{
    io::{self, BufRead, Write},
    process::ExitCode,
};

use anyhow::Result;
use clap::{Arg, ArgGroup, Command};

use ananke::command;

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
            .about("Add an entry")
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
            .about("Lookup an entry")
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
            .about("Modify an entry")
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
            .about("Remove an entry")
            .arg(arg_description)
            .arg(arg_entry_id)
            .group(ArgGroup::new("remove").args(["description", "entry-id"]))
            .arg_required_else_help(true)
    };

    let import = {
        let arg_file = Arg::new("file").value_name("FILE").help("file to import from");
        Command::new("import")
            .about("Import entries from JSON file")
            .arg(arg_file)
            .arg_required_else_help(true)
    };

    let export = {
        let arg_file = Arg::new("file").value_name("FILE").help("file to export to");
        Command::new("export")
            .about("Export entries to JSON file")
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

fn trim_newline(s: &mut String) {
    if s.ends_with('\n') {
        s.pop();
        if s.ends_with('\r') {
            s.pop();
        }
    }
}

fn prompt(display: &str) -> Result<String, io::Error> {
    print!("{}", display);
    io::stdout().flush()?;
    let mut ret = String::new();
    let stdin = io::stdin();
    stdin.lock().read_line(&mut ret)?;
    trim_newline(&mut ret);
    Ok(ret)
}

fn main() -> Result<ExitCode> {
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
