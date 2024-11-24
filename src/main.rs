use std::{path::PathBuf, process::ExitCode};

use clap::{Args, Parser, Subcommand};

use ananke::{
    cli::{self, Target},
    data::{Description, EntryId, Identity, Metadata},
    version,
};

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
        description: Description,
        /// username or email address
        #[arg(short, long)]
        identity: Option<Identity>,
        /// additional metadata
        #[arg(short, long, value_name = "METADATA")]
        meta: Option<Metadata>,
    },
    /// Lookup an entry
    #[command(arg_required_else_help = true)]
    Lookup {
        /// URL or description
        #[arg(required = true)]
        description: Description,
        /// username or email address
        #[arg(short, long)]
        identity: Option<Identity>,
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
        identity: Option<Identity>,
        /// additional metadata
        #[arg(short, long, value_name = "METADATA")]
        meta: Option<Metadata>,
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
        file: PathBuf,
    },
    /// Export entries to JSON file
    #[command(arg_required_else_help = true)]
    Export {
        /// file to export to
        #[arg(required = true)]
        file: PathBuf,
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
    description: Option<Description>,
    /// Entry ID
    #[arg(short, long, value_name = "ENTRYID")]
    entry_id: Option<EntryId>,
}

impl TryFrom<ModifyTarget> for Target {
    type Error = anyhow::Error;

    fn try_from(value: ModifyTarget) -> Result<Self, Self::Error> {
        match (value.description, value.entry_id) {
            (Some(d), None) => Ok(Target::Description(d)),
            (None, Some(i)) => Ok(Target::EntryId(i)),
            _ => Err(anyhow::Error::msg("ill-formed ModifyTarget value")),
        }
    }
}

#[derive(Args)]
#[group(required = true, multiple = false)]
struct RemoveTarget {
    /// URL or description
    #[arg(short, long)]
    description: Option<Description>,
    /// Entry ID
    #[arg(short, long, value_name = "ENTRYID")]
    entry_id: Option<EntryId>,
}

impl TryFrom<RemoveTarget> for Target {
    type Error = anyhow::Error;

    fn try_from(value: RemoveTarget) -> Result<Self, Self::Error> {
        match (value.description, value.entry_id) {
            (Some(d), None) => Ok(Target::Description(d)),
            (None, Some(i)) => Ok(Target::EntryId(i)),
            _ => Err(anyhow::Error::msg("ill-formed RemoveTarget value")),
        }
    }
}

fn main() -> anyhow::Result<ExitCode> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Add { description, identity, meta } => cli::add(description, identity, meta),
        Commands::Lookup { description, identity, verbose } => {
            cli::lookup(description, identity, verbose)
        }
        Commands::Modify { target, plaintext, identity, meta } => {
            cli::modify(target.try_into()?, plaintext, None, identity, meta)
        }
        Commands::Remove { target } => cli::remove(target.try_into()?),
        Commands::Import { file } => cli::import(file),
        Commands::Export { file } => cli::export(file),
        Commands::Configure { list } => cli::configure(list),
    }
}
