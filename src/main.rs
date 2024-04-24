use std::process::ExitCode;

use anyhow::Result;
use clap::{Arg, ArgGroup, Command};

use ananke::{cli, version};

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

fn main() -> Result<ExitCode> {
    let matches = command().get_matches();
    match matches.subcommand() {
        Some(("add", sub_matches)) => {
            let description = sub_matches.get_one::<String>("description").cloned().unwrap();
            let identity = sub_matches.get_one::<String>("identity").cloned();
            let metadata = sub_matches.get_one::<String>("metadata").cloned();
            cli::add(description, identity, metadata)
        }
        Some(("lookup", sub_matches)) => {
            let description = sub_matches.get_one::<String>("description").cloned().unwrap();
            let identity = sub_matches.get_one::<String>("identity").cloned();
            let verbose = sub_matches.get_one::<bool>("verbose").copied().unwrap_or_default();
            cli::lookup(description, identity, verbose)
        }
        Some(("modify", sub_matches)) => {
            let description = sub_matches.get_one::<String>("description").cloned();
            let entry_id = sub_matches.get_one::<String>("entry-id").cloned();
            let plaintext = sub_matches.get_one::<bool>("plaintext").copied().unwrap_or_default();
            let identity = sub_matches.get_one::<String>("identity").cloned();
            let metadata = sub_matches.get_one::<String>("metadata").cloned();
            cli::modify(description, entry_id, plaintext, None, identity, metadata)
        }
        Some(("remove", sub_matches)) => {
            let description = sub_matches.get_one::<String>("description").cloned();
            let entry_id = sub_matches.get_one::<String>("entry-id").cloned();
            cli::remove(description, entry_id)
        }
        Some(("import", sub_matches)) => {
            let file = sub_matches.get_one::<String>("file").cloned().unwrap();
            cli::import(file)
        }
        Some(("export", sub_matches)) => {
            let file = sub_matches.get_one::<String>("file").cloned().unwrap();
            cli::export(file)
        }
        Some((&_, _)) => panic!(),
        None => panic!(),
    }
}
