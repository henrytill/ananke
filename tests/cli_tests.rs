use std::ffi::OsString;
use std::io;
use std::path::{Path, PathBuf};

use snapbox::cmd::cargo_bin;
use snapbox::cmd::Command;
use snapbox::file;
use snapbox::path::PathFixture;

const BIN: &'static str = env!("CARGO_PKG_NAME");

const EXAMPLE_DIR: &'static str = r"example";

const GNUPGHOME: [&'static str; 2] = [EXAMPLE_DIR, "gnupg"];

const INI_PATH: [&'static str; 2] = [EXAMPLE_DIR, "ananke.ini"];

const JSON_PATH: [&'static str; 3] = [EXAMPLE_DIR, "db", "data.json"];

fn vars_from(path: impl AsRef<Path>) -> impl IntoIterator<Item = (OsString, OsString)> + Clone {
    let val: OsString = path.as_ref().into();
    [
        (OsString::from("GNUPGHOME"), GNUPGHOME.iter().collect::<PathBuf>().into_os_string()),
        (OsString::from("ANANKE_CONFIG_DIR"), val.clone()),
        (OsString::from("ANANKE_DATA_DIR"), val),
    ]
}

fn vars() -> impl IntoIterator<Item = (OsString, OsString)> + Clone {
    vars_from(PathBuf::from(EXAMPLE_DIR))
}

fn copy_config(path: impl AsRef<Path>) -> Result<(), io::Error> {
    let source: PathBuf = INI_PATH.into_iter().collect::<PathBuf>();
    let dest: PathBuf = {
        let mut ret: PathBuf = path.as_ref().into();
        ret.push("ananke.ini");
        ret
    };
    std::fs::copy(source, dest)?;
    Ok(())
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

#[test]
fn lookup_non_existent() {
    Command::new(cargo_bin(BIN))
        .args(["lookup", "paul"])
        .envs(vars())
        .assert()
        .stderr_eq(String::new())
        .failure()
        .code(1);
}

#[test]
fn modify_non_existent() {
    Command::new(cargo_bin(BIN))
        .args(["modify", "-d", "paul"])
        .envs(vars())
        .assert()
        .stderr_eq(file!("cli_tests/modify_non_existent.stderr"))
        .failure()
        .code(1);
}

#[test]
fn remove_non_existent() {
    Command::new(cargo_bin(BIN))
        .args(["remove", "-d", "paul"])
        .envs(vars())
        .assert()
        .stderr_eq(file!("cli_tests/remove_non_existent.stderr"))
        .failure()
        .code(1);
}

#[test]
fn import() {
    let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
    let path = path_fixture.path().expect("should get path");
    copy_config(path).expect("should copy");
    let data_file: OsString = JSON_PATH.into_iter().collect::<PathBuf>().into_os_string();
    let data_file_str: &str = data_file.to_str().expect("should have path");
    Command::new(cargo_bin(BIN))
        .args(["import", data_file_str])
        .envs(vars_from(path))
        .assert()
        .success();
}

#[test]
fn add() {
    let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
    let path = path_fixture.path().expect("should get path");
    copy_config(path).expect("should copy");
    let data_file: OsString = JSON_PATH.into_iter().collect::<PathBuf>().into_os_string();
    let data_file_str: &str = data_file.to_str().expect("should have path");
    Command::new(cargo_bin(BIN))
        .args(["import", data_file_str])
        .envs(vars_from(path))
        .assert()
        .success();
    Command::new(cargo_bin(BIN))
        .args(["add", "-i", "quux", "https://www.quuxlib.com/"])
        .envs(vars_from(path))
        .stdin("pass137pass")
        .assert()
        .success();
    Command::new(cargo_bin(BIN))
        .args(["lookup", "https://www.quuxlib.com/"])
        .envs(vars_from(path))
        .assert()
        .stdout_eq(file!("cli_tests/add.stdout"))
        .success();
}
