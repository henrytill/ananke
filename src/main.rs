use std::process::ExitCode;

use anyhow::Result;
use clap::{Arg, ArgGroup, Command};

use ananke::{cli, version};

const ARG_DESCRIPTION: &str = "description";
const ARG_IDENTITY: &str = "identity";
const ARG_ENTRY_ID: &str = "entry-id";
const ARG_PLAINTEXT: &str = "plaintext";
const ARG_METADATA: &str = "metadata";
const ARG_VERBOSE: &str = "verbose";
const ARG_FILE: &str = "file";
const ARG_LIST: &str = "list";

const CMD_ADD: &str = "add";
const CMD_LOOKUP: &str = "lookup";
const CMD_MODIFY: &str = "modify";
const CMD_REMOVE: &str = "remove";
const CMD_IMPORT: &str = "import";
const CMD_EXPORT: &str = "export";
const CMD_CONFIGURE: &str = "configure";

fn command() -> Command {
    let version = version::version_info().to_string();

    let add = {
        let arg_description =
            Arg::new(ARG_DESCRIPTION).value_name("DESCRIPTION").help("URL or description");
        let arg_identity = Arg::new(ARG_IDENTITY)
            .short('i')
            .long("identity")
            .value_name("IDENTITY")
            .help("username or email address");
        let arg_meta = Arg::new(ARG_METADATA)
            .short('m')
            .long("meta")
            .value_name("METADATA")
            .help("additional metadata");
        Command::new(CMD_ADD)
            .about("Add an entry")
            .arg(arg_description)
            .arg(arg_identity)
            .arg(arg_meta)
            .arg_required_else_help(true)
    };

    let lookup = {
        let arg_description =
            Arg::new(ARG_DESCRIPTION).value_name("DESCRIPTION").help("URL or description");
        let arg_identity = Arg::new(ARG_IDENTITY)
            .short('i')
            .long("identity")
            .value_name("IDENTITY")
            .help("username or email address");
        let arg_verbose = Arg::new(ARG_VERBOSE)
            .short('v')
            .long("verbose")
            .num_args(0)
            .help("enable verbose output");
        Command::new(CMD_LOOKUP)
            .about("Lookup an entry")
            .arg(arg_description)
            .arg(arg_identity)
            .arg(arg_verbose)
            .arg_required_else_help(true)
    };

    let modify = {
        let arg_description = Arg::new(ARG_DESCRIPTION)
            .short('d')
            .long("description")
            .value_name("DESCRIPTION")
            .help("URL or description");
        let arg_entry_id = Arg::new(ARG_ENTRY_ID)
            .short('e')
            .long("entry-id")
            .value_name("ENTRY_ID")
            .help("entry_id");
        let arg_plaintext = Arg::new(ARG_PLAINTEXT)
            .short('p')
            .long("plaintext")
            .num_args(0)
            .help("prompt for plaintext");
        let arg_identity = Arg::new(ARG_IDENTITY)
            .short('i')
            .long("identity")
            .value_name("IDENTITY")
            .help("username or email address");
        let arg_meta = Arg::new(ARG_METADATA)
            .short('m')
            .long("meta")
            .value_name("METADATA")
            .help("additional metadata");
        Command::new(CMD_MODIFY)
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
        let arg_description = Arg::new(ARG_DESCRIPTION)
            .short('d')
            .long("description")
            .value_name("DESCRIPTION")
            .help("URL or description");
        let arg_entry_id = Arg::new(ARG_ENTRY_ID)
            .short('e')
            .long("entry-id")
            .value_name("ENTRY_ID")
            .help("entry_id");
        Command::new(CMD_REMOVE)
            .about("Remove an entry")
            .arg(arg_description)
            .arg(arg_entry_id)
            .group(ArgGroup::new("remove").args(["description", "entry-id"]))
            .arg_required_else_help(true)
    };

    let import = {
        let arg_file = Arg::new(ARG_FILE).value_name("FILE").help("file to import from");
        Command::new(CMD_IMPORT)
            .about("Import entries from JSON file")
            .arg(arg_file)
            .arg_required_else_help(true)
    };

    let export = {
        let arg_file = Arg::new(ARG_FILE).value_name("FILE").help("file to export to");
        Command::new(CMD_EXPORT)
            .about("Export entries to JSON file")
            .arg(arg_file)
            .arg_required_else_help(true)
    };

    let configure = {
        let arg_list =
            Arg::new(ARG_LIST).short('l').long("list").num_args(0).help("List the configuration");
        Command::new(CMD_CONFIGURE).about("Create, modify, and list configuration").arg(arg_list)
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
        .subcommand(configure)
        .arg_required_else_help(true)
}

fn main() -> Result<ExitCode> {
    let matches = command().get_matches();
    match matches.subcommand() {
        Some((CMD_ADD, sub_matches)) => {
            let description = sub_matches.get_one::<String>(ARG_DESCRIPTION).cloned().unwrap();
            let identity = sub_matches.get_one::<String>(ARG_IDENTITY).cloned();
            let metadata = sub_matches.get_one::<String>(ARG_METADATA).cloned();
            cli::add(description, identity, metadata)
        }
        Some((CMD_LOOKUP, sub_matches)) => {
            let description = sub_matches.get_one::<String>(ARG_DESCRIPTION).cloned().unwrap();
            let identity = sub_matches.get_one::<String>(ARG_IDENTITY).cloned();
            let verbose = sub_matches.get_one::<bool>(ARG_VERBOSE).copied().unwrap_or_default();
            cli::lookup(description, identity, verbose)
        }
        Some((CMD_MODIFY, sub_matches)) => {
            let description = sub_matches.get_one::<String>(ARG_DESCRIPTION).cloned();
            let entry_id = sub_matches.get_one::<String>(ARG_ENTRY_ID).cloned();
            let plaintext = sub_matches.get_one::<bool>(ARG_PLAINTEXT).copied().unwrap_or_default();
            let identity = sub_matches.get_one::<String>(ARG_IDENTITY).cloned();
            let metadata = sub_matches.get_one::<String>(ARG_METADATA).cloned();
            cli::modify(description, entry_id, plaintext, None, identity, metadata)
        }
        Some((CMD_REMOVE, sub_matches)) => {
            let description = sub_matches.get_one::<String>(ARG_DESCRIPTION).cloned();
            let entry_id = sub_matches.get_one::<String>(ARG_ENTRY_ID).cloned();
            cli::remove(description, entry_id)
        }
        Some((CMD_IMPORT, sub_matches)) => {
            let file = sub_matches.get_one::<String>(ARG_FILE).cloned().unwrap();
            cli::import(file)
        }
        Some((CMD_EXPORT, sub_matches)) => {
            let file = sub_matches.get_one::<String>(ARG_FILE).cloned().unwrap();
            cli::export(file)
        }
        Some((CMD_CONFIGURE, sub_matches)) => {
            let list = sub_matches.get_one::<bool>(ARG_LIST).copied().unwrap_or_default();
            cli::configure(list)
        }
        Some((&_, _)) => panic!(),
        None => panic!(),
    }
}
