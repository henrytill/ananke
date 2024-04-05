use std::ffi::OsString;
use std::path::PathBuf;

use snapbox::cmd::cargo_bin;
use snapbox::cmd::Command;
use snapbox::file;

const BIN: &'static str = env!("CARGO_PKG_NAME");

const EXAMPLE_DIR: &'static str = r"example";

const GNUPGHOME: [&'static str; 2] = [EXAMPLE_DIR, "gnupg"];

fn vars() -> impl IntoIterator<Item = (OsString, OsString)> + Clone {
    [
        (OsString::from("GNUPGHOME"), GNUPGHOME.iter().collect::<PathBuf>().into_os_string()),
        (OsString::from("ANANKE_CONFIG_DIR"), OsString::from(EXAMPLE_DIR)),
        (OsString::from("ANANKE_DATA_DIR"), OsString::from(EXAMPLE_DIR)),
    ]
}

#[test]
fn usage() {
    Command::new(cargo_bin(BIN))
        .envs(vars())
        .assert()
        .stderr_matches(file!("cli_tests/usage.stderr"))
        .failure();
}

#[test]
fn lookup_foo() {
    Command::new(cargo_bin(BIN))
        .args(["lookup", "foo"])
        .envs(vars())
        .assert()
        .stdout_eq(file!("cli_tests/lookup_foo.stdout"))
        .success();
}

#[test]
fn lookup_www() {
    Command::new(cargo_bin(BIN))
        .args(["lookup", "www"])
        .envs(vars())
        .assert()
        .stdout_eq(file!("cli_tests/lookup_www.stdout"))
        .success();
}

#[test]
fn lookup_www_verbose() {
    Command::new(cargo_bin(BIN))
        .args(["lookup", "www", "-v"])
        .envs(vars())
        .assert()
        .stdout_eq(file!("cli_tests/lookup_www_verbose.stdout"))
        .success();
}
