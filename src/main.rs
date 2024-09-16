use std::process::ExitCode;

use anyhow::Result;
use clap::{Args, Parser, Subcommand};

use ananke::{cli, version};

/// A password manager
#[derive(Parser)]
#[command(name = "ananke", version = version::version_info().to_string())]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Add an entry
    #[command(arg_required_else_help = true)]
    Add {
        /// URL or description
        #[arg(required = true)]
        description: String,
        /// username or email address
        #[arg(short, long)]
        identity: Option<String>,
        /// additional metadata
        #[arg(short, long, value_name = "METADATA")]
        meta: Option<String>,
    },
    /// Lookup an entry
    #[command(arg_required_else_help = true)]
    Lookup {
        /// URL or description
        #[arg(required = true)]
        description: String,
        /// username or email address
        #[arg(short, long)]
        identity: Option<String>,
        /// enable verbose output
        #[arg(short, long)]
        verbose: bool,
    },
    /// Modify an entry
    #[command(arg_required_else_help = true)]
    Modify {
        #[command(flatten)]
        target: ModifyTarget,
        /// prompt for plaintext
        #[arg(short, long)]
        plaintext: bool,
        /// username or email address
        #[arg(short, long)]
        identity: Option<String>,
        /// additional metadata
        #[arg(short, long, value_name = "METADATA")]
        meta: Option<String>,
    },
    /// Remove an entry
    #[command(arg_required_else_help = true)]
    Remove {
        #[command(flatten)]
        target: RemoveTarget,
    },
    /// Import entries from JSON file
    #[command(arg_required_else_help = true)]
    Import {
        /// file to import from
        #[arg(required = true)]
        file: String,
    },
    /// Export entries to JSON file
    #[command(arg_required_else_help = true)]
    Export {
        /// file to export to
        #[arg(required = true)]
        file: String,
    },
    /// Create, modify, and list configuration
    Configure {
        /// List the configuration
        #[arg(short, long)]
        list: bool,
    },
}

#[derive(Args)]
#[group(required = true, multiple = false)]
struct ModifyTarget {
    /// URL or description
    #[arg(short, long)]
    description: Option<String>,
    /// Entry ID
    #[arg(short, long, value_name = "ENTRYID")]
    entry_id: Option<String>,
}

#[derive(Args)]
#[group(required = true, multiple = false)]
struct RemoveTarget {
    /// URL or description
    #[arg(short, long)]
    description: Option<String>,
    /// Entry ID
    #[arg(short, long, value_name = "ENTRYID")]
    entry_id: Option<String>,
}

fn main() -> Result<ExitCode> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Add { description, identity, meta } => cli::add(description, identity, meta),
        Commands::Lookup { description, identity, verbose } => {
            cli::lookup(description, identity, verbose)
        }
        Commands::Modify {
            target: ModifyTarget { description, entry_id },
            plaintext,
            identity,
            meta,
        } => cli::modify(description, entry_id, plaintext, None, identity, meta),
        Commands::Remove { target: RemoveTarget { description, entry_id } } => {
            cli::remove(description, entry_id)
        }
        Commands::Import { file } => cli::import(file),
        Commands::Export { file } => cli::export(file),
        Commands::Configure { list } => cli::configure(list),
    }
}
