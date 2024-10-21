use std::{
    io::{self, BufRead, Write},
    path::PathBuf,
    process::ExitCode,
    str::FromStr,
};

use anyhow::Error;
use zeroize::Zeroize;

use crate::{
    application::{
        base::{Application, Target},
        json::JsonApplication,
        sqlite::SqliteApplication,
    },
    config::{Backend, Config, ConfigBuilder},
    data::{Description, Entry, EntryId, Identity, KeyId, Metadata, Plaintext},
    gpg,
};

const PROMPT_PLAINTEXT: &str = "Enter plaintext: ";

fn config() -> Result<Config, Error> {
    ConfigBuilder::new(std::env::var).with_defaults()?.with_ini(None)?.with_env()?.build()
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

fn format_verbose(entry: &Entry, plaintext: &Plaintext) -> Result<String, Error> {
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
        elements.push(format!("\"{}\"", metadata))
    }

    let ret = elements.join(" ");
    elements.zeroize();
    Ok(ret)
}

fn format_results(results: &[(Entry, Plaintext)], verbose: bool) -> Result<String, Error> {
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
) -> Result<ExitCode, Error> {
    let plaintext = enter_plaintext()?;

    let description = Description::from(description);
    let maybe_identity = maybe_identity.map(Identity::from);
    let maybe_metadata = maybe_metadata.map(Metadata::from);

    let config = config()?;
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
) -> Result<ExitCode, Error> {
    let description = Description::from(description);
    let maybe_identity = maybe_identity.map(Identity::from);

    let config = config()?;
    match config.backend() {
        Backend::Json => {
            let app = JsonApplication::new(config)?;
            let results = app.lookup(description, maybe_identity)?;
            if results.is_empty() {
                return Ok(ExitCode::FAILURE);
            }
            let mut output = format_results(&results, verbose)?;
            println!("{}", output);
            output.zeroize();
        }
        Backend::Sqlite => {
            let app = SqliteApplication::new(config)?;
            let results = app.lookup(description, maybe_identity)?;
            if results.is_empty() {
                return Ok(ExitCode::FAILURE);
            }
            let mut output = format_results(&results, verbose)?;
            println!("{}", output);
            output.zeroize();
        }
    };
    Ok(ExitCode::SUCCESS)
}

pub fn modify(
    target_description: Option<String>,
    target_entry_id: Option<String>,
    ask_plaintext: bool,
    maybe_description: Option<String>,
    maybe_identity: Option<String>,
    maybe_metadata: Option<String>,
) -> Result<ExitCode, Error> {
    let maybe_plaintext = if ask_plaintext {
        let plaintext = enter_plaintext()?;
        Some(plaintext)
    } else {
        None
    };

    let target = match (target_description, target_entry_id) {
        (Some(d), None) => Target::Description(Description::from(d)),
        (None, Some(i)) => Target::EntryId(EntryId::try_from(i)?),
        (Some(_), Some(_)) => panic!(),
        (None, None) => panic!(),
    };

    let maybe_description = maybe_description.map(Description::from);
    let maybe_identity = maybe_identity.map(Identity::from);
    let maybe_metadata = maybe_metadata.map(Metadata::from);

    let config = config()?;
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

pub fn remove(
    target_description: Option<String>,
    target_entry_id: Option<String>,
) -> Result<ExitCode, Error> {
    let target = match (target_description, target_entry_id) {
        (Some(d), None) => Target::Description(Description::from(d)),
        (None, Some(i)) => Target::EntryId(EntryId::try_from(i)?),
        (Some(_), Some(_)) => panic!(),
        (None, None) => panic!(),
    };

    let config = config()?;
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

pub fn import(path: String) -> Result<ExitCode, Error> {
    let path = PathBuf::from(path);
    let config = config()?;
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

pub fn export(path: String) -> Result<ExitCode, Error> {
    let path = PathBuf::from(path);
    let config = config()?;
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

pub fn configure(list: bool) -> Result<ExitCode, Error> {
    if list {
        let config = config()?;
        println!("{}", config.pretty_print());
        return Ok(ExitCode::SUCCESS);
    }

    let mut builder = ConfigBuilder::new(std::env::var).with_defaults()?.with_env()?;

    let maybe_config_file = builder.maybe_config_file();
    match maybe_config_file {
        Some(config_file) if config_file.exists() => {
            println!("Configuration file exists at: {}", config_file.display());
            println!("To view settings, run:\n  ananke configure --list");
            return Ok(ExitCode::SUCCESS);
        }
        _ => {}
    }

    if builder.maybe_key_id().is_none() {
        // Prompt for key id
        let mut key_candidate = None;
        while key_candidate.is_none() {
            key_candidate = gpg::suggest_key(std::env::vars)?;
            let key_candidate_str = if let Some(ref key_id) = key_candidate {
                format!("[{}] ", key_id)
            } else {
                String::new()
            };
            let prompt_str = format!("Enter GPG key id: {}", key_candidate_str);
            let key_input = prompt(prompt_str.as_str())?;
            if !key_input.is_empty() {
                key_candidate = Some(KeyId::from(key_input))
            }
        }
        *builder.maybe_key_id_mut() = key_candidate
    }

    if builder.maybe_backend().is_none() {
        // Prompt for backend
        let mut backend_candidate: Option<Backend> = None;
        let default_backend = Backend::default();
        while backend_candidate.is_none() {
            println!("Available backends:");
            for backend in [Backend::Json, Backend::Sqlite] {
                let backend_value = backend as u8;
                if backend == default_backend {
                    println!("  {}: {} (default)", backend_value, backend)
                } else {
                    println!("  {}: {}", backend_value, backend)
                }
            }
            let prompt_str = format!("Enter choice: [{}] ", default_backend as u8);
            let backend_input = prompt(prompt_str.as_str())?;
            backend_candidate = if backend_input.is_empty() {
                Some(default_backend)
            } else {
                Backend::from_str(backend_input.as_str()).ok()
            }
        }
        *builder.maybe_backend_mut() = backend_candidate
    }

    for maybe_dir in [builder.maybe_config_dir(), builder.maybe_data_dir()] {
        let dir = maybe_dir.unwrap();
        if !dir.exists() {
            std::fs::create_dir(dir)?
        }
    }

    let config_file = builder.maybe_config_file().unwrap();
    let ini = builder.ini().unwrap();
    std::fs::write(config_file.as_path(), ini)?;
    println!("Configuration file written to: {}", config_file.display());
    Ok(ExitCode::SUCCESS)
}
