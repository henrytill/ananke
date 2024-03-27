use std::path::Path;
use std::process::Command;

fn commit_info() {
    if !Path::new(".git").exists() {
        return;
    }
    let output = match Command::new("git")
        .arg("log")
        .arg("-1")
        .arg("--date=short")
        .arg("--format=%H %h %cd")
        .arg("--abbrev=9")
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

fn main() {
    commit_info();
}
