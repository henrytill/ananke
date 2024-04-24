use std::{
    io::{self, BufRead, Write},
    path::PathBuf,
    process::ExitCode,
};

use anyhow::Result;
use zeroize::Zeroize;

use crate::{
    application::{
        common::{Application, Target},
        json::JsonApplication,
        sqlite::SqliteApplication,
    },
    config::{Backend, Config, ConfigBuilder},
    data::{Description, Entry, EntryId, Identity, Metadata, Plaintext},
};

const PROMPT_PLAINTEXT: &str = "Enter plaintext: ";

fn configure() -> Result<Config> {
    let mut config_builder = ConfigBuilder::new();
    config_builder =
        config_builder.with_dirs(&std::env::var)?.with_config(None)?.with_env(&std::env::var)?;
    let config = config_builder.build()?;
    Ok(config)
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

fn enter_plaintext() -> Result<Plaintext, io::Error> {
    let plaintext = prompt(PROMPT_PLAINTEXT)?;
    Ok(Plaintext::from(plaintext))
}

fn format_brief(entry: &Entry, plaintext: &Plaintext) -> String {
    let description = entry.description.as_str();
    let identity = entry.identity.as_ref().map(Identity::as_str).unwrap_or_else(|| "<none>");
    let plaintext = plaintext.as_str();
    format!("{} {} {}", description, identity, plaintext)
}

fn format_verbose(entry: &Entry, plaintext: &Plaintext) -> Result<String> {
    let mut elements = vec![
        entry.timestamp.isoformat()?,
        entry.entry_id.to_string(),
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

    let ret = elements.join(" ");
    elements.zeroize();
    Ok(ret)
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

    let ret = formatted_results.join("\n");
    formatted_results.zeroize();
    Ok(ret)
}

pub fn add(
    description: String,
    maybe_identity: Option<String>,
    maybe_metadata: Option<String>,
) -> Result<ExitCode> {
    let plaintext = enter_plaintext()?;

    let description = Description::from(description);
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
    let mut output = format_results(&results, verbose)?;
    println!("{}", output);
    output.zeroize();
    Ok(ExitCode::SUCCESS)
}

pub fn modify(
    target_description: Option<String>,
    target_id: Option<String>,
    ask_plaintext: bool,
    maybe_description: Option<String>,
    maybe_identity: Option<String>,
    maybe_metadata: Option<String>,
) -> Result<ExitCode> {
    let maybe_plaintext = if ask_plaintext {
        let plaintext = enter_plaintext()?;
        Some(plaintext)
    } else {
        None
    };

    let target = match (target_description, target_id) {
        (Some(d), None) => Target::Description(Description::from(d)),
        (None, Some(i)) => Target::EntryId(EntryId::from(i)),
        (Some(_), Some(_)) => panic!(),
        (None, None) => panic!(),
    };

    let maybe_description = maybe_description.map(Description::from);
    let maybe_identity = maybe_identity.map(Identity::from);
    let maybe_metadata = maybe_metadata.map(Metadata::from);

    let config = configure()?;
    match config.backend() {
        Backend::Json => {
            let mut app = JsonApplication::new(config)?;
            app.modify(target, maybe_description, maybe_plaintext, maybe_identity, maybe_metadata)?;
        }
        Backend::Sqlite => {
            let mut app = SqliteApplication::new(config)?;
            app.modify(target, maybe_description, maybe_plaintext, maybe_identity, maybe_metadata)?;
        }
    }
    Ok(ExitCode::SUCCESS)
}

pub fn remove(target_description: Option<String>, target_id: Option<String>) -> Result<ExitCode> {
    let target = match (target_description, target_id) {
        (Some(d), None) => Target::Description(Description::from(d)),
        (None, Some(i)) => Target::EntryId(EntryId::from(i)),
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
