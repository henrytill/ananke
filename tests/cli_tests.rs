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

fn vars(
    config_dir: impl Into<OsString>,
    data_dir: impl Into<OsString>,
) -> impl IntoIterator<Item = (OsString, OsString)> + Clone {
    let gnupg_home = {
        let mut ret = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        ret.push(GNUPGHOME.iter().collect::<PathBuf>());
        ret
    };
    [
        (OsString::from("GNUPGHOME"), gnupg_home.into_os_string()),
        (OsString::from("ANANKE_CONFIG_DIR"), config_dir.into()),
        (OsString::from("ANANKE_DATA_DIR"), data_dir.into()),
    ]
}

fn example_vars() -> impl IntoIterator<Item = (OsString, OsString)> + Clone {
    let mut dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dir.push(EXAMPLE_DIR);
    vars(dir.clone(), dir)
}

fn copy_config(path: impl AsRef<Path>) -> Result<(), io::Error> {
    let source = {
        let mut ret = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        ret.push(INI_PATH.into_iter().collect::<PathBuf>());
        ret
    };
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
        .envs(example_vars())
        .assert()
        .stderr_matches(file!("cli_tests/usage.stderr"))
        .failure();
}

#[test]
fn lookup_foo() {
    Command::new(cargo_bin(BIN))
        .args(["lookup", "foo"])
        .envs(example_vars())
        .assert()
        .stdout_eq(file!("cli_tests/lookup_foo.stdout"))
        .success();
}

#[test]
fn lookup_www() {
    Command::new(cargo_bin(BIN))
        .args(["lookup", "www"])
        .envs(example_vars())
        .assert()
        .stdout_eq(file!("cli_tests/lookup_www.stdout"))
        .success();
}

#[test]
fn lookup_www_verbose() {
    Command::new(cargo_bin(BIN))
        .args(["lookup", "www", "-v"])
        .envs(example_vars())
        .assert()
        .stdout_eq(file!("cli_tests/lookup_www_verbose.stdout"))
        .success();
}

#[test]
fn lookup_non_existent() {
    Command::new(cargo_bin(BIN))
        .args(["lookup", "paul"])
        .envs(example_vars())
        .assert()
        .stderr_eq(String::new())
        .failure()
        .code(1);
}

#[test]
fn modify_non_existent() {
    Command::new(cargo_bin(BIN))
        .args(["modify", "-d", "paul"])
        .envs(example_vars())
        .assert()
        .stderr_eq(file!("cli_tests/modify_non_existent.stderr"))
        .failure()
        .code(1);
}

#[test]
fn remove_non_existent() {
    Command::new(cargo_bin(BIN))
        .args(["remove", "-d", "paul"])
        .envs(example_vars())
        .assert()
        .stderr_eq(file!("cli_tests/remove_non_existent.stderr"))
        .failure()
        .code(1);
}

#[test]
fn import() {
    let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
    let dir = path_fixture.path().expect("should get path");
    copy_config(dir).expect("should copy");
    let data_file: OsString = JSON_PATH.into_iter().collect::<PathBuf>().into_os_string();
    let data_file_str: &str = data_file.to_str().expect("should have path");
    Command::new(cargo_bin(BIN))
        .args(["import", data_file_str])
        .envs(vars(dir, dir))
        .assert()
        .success();
}

#[test]
fn import_lookup() {
    let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
    let dir = path_fixture.path().expect("should get path");
    copy_config(dir).expect("should copy");
    let data_file: OsString = JSON_PATH.into_iter().collect::<PathBuf>().into_os_string();
    let data_file_str: &str = data_file.to_str().expect("should have path");
    Command::new(cargo_bin(BIN))
        .args(["import", data_file_str])
        .envs(vars(dir, dir))
        .assert()
        .success();
    Command::new(cargo_bin(BIN))
        .args(["lookup", "foomail"])
        .envs(vars(dir, dir))
        .assert()
        .stdout_eq(file!("cli_tests/import_lookup.stdout"))
        .success();
}

#[test]
fn import_lookup_many() {
    let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
    let dir = path_fixture.path().expect("should get path");
    copy_config(dir).expect("should copy");
    let data_file: OsString = JSON_PATH.into_iter().collect::<PathBuf>().into_os_string();
    let data_file_str: &str = data_file.to_str().expect("should have path");
    Command::new(cargo_bin(BIN))
        .args(["import", data_file_str])
        .envs(vars(dir, dir))
        .assert()
        .success();
    Command::new(cargo_bin(BIN))
        .args(["lookup", "www"])
        .envs(vars(dir, dir))
        .assert()
        .stdout_eq(file!("cli_tests/import_lookup_many.stdout"))
        .success();
}

#[test]
fn import_add() {
    let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
    let dir = path_fixture.path().expect("should get path");
    copy_config(dir).expect("should copy");
    let data_file: OsString = JSON_PATH.into_iter().collect::<PathBuf>().into_os_string();
    let data_file_str: &str = data_file.to_str().expect("should have path");
    Command::new(cargo_bin(BIN))
        .args(["import", data_file_str])
        .envs(vars(dir, dir))
        .assert()
        .success();
    Command::new(cargo_bin(BIN))
        .args(["add", "-i", "quux", "https://www.quuxlib.com/"])
        .envs(vars(dir, dir))
        .stdin("pass137pass")
        .assert()
        .success();
    Command::new(cargo_bin(BIN))
        .args(["lookup", "https://www.quuxlib.com/"])
        .envs(vars(dir, dir))
        .assert()
        .stdout_eq(file!("cli_tests/import_add.stdout"))
        .success();
}

#[test]
fn import_modify() {
    let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
    let dir = path_fixture.path().expect("should get path");
    copy_config(dir).expect("should copy");
    let data_file: OsString = JSON_PATH.into_iter().collect::<PathBuf>().into_os_string();
    let data_file_str: &str = data_file.to_str().expect("should have path");
    Command::new(cargo_bin(BIN))
        .args(["import", data_file_str])
        .envs(vars(dir, dir))
        .assert()
        .success();
    Command::new(cargo_bin(BIN))
        .args(["modify", "-p", "-d", "https://www.barphone.com"])
        .envs(vars(dir, dir))
        .stdin("MyNewPassword")
        .assert()
        .success();
    Command::new(cargo_bin(BIN))
        .args(["lookup", "barphone"])
        .envs(vars(dir, dir))
        .assert()
        .stdout_eq(file!("cli_tests/import_modify.stdout"))
        .success();
}
