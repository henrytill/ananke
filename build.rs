use std::path::Path;
use std::process::Command;

fn commit_info_git() {
    let output = match Command::new("git")
        .arg("log")
        .arg("-1")
        .arg("--date=short")
        .arg("--format=%H %h %cd")
        .arg("--abbrev=7")
        .output()
    {
        Ok(output) if output.status.success() => output,
        _ => return,
    };
    let stdout = String::from_utf8(output.stdout).unwrap();
    let mut parts = stdout.split_whitespace();
    let mut next = || parts.next().unwrap();
    println!("cargo:rustc-env=ANANKE_COMMIT_HASH={}", next());
    println!("cargo:rustc-env=ANANKE_COMMIT_SHORT_HASH={}", next());
    println!("cargo:rustc-env=ANANKE_COMMIT_DATE={}", next())
}

fn commit_info_env() {
    for var in ["ANANKE_COMMIT_HASH", "ANANKE_COMMIT_SHORT_HASH", "ANANKE_COMMIT_DATE"] {
        if let Ok(value) = std::env::var(var) {
            println!("cargo:rustc-env={}={}", var, value);
        }
    }
}

fn main() {
    if Path::new(".git").exists() {
        commit_info_git();
    } else {
        commit_info_env();
    }
}
