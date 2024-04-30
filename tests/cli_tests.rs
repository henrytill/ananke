use std::ffi::OsString;
use std::io;
use std::path::{Path, PathBuf};

use snapbox::cmd::cargo_bin;
use snapbox::cmd::Command;
use snapbox::file;

const BIN: &'static str = env!("CARGO_PKG_NAME");

const EXAMPLE_DIR: &'static str = r"example";

const GNUPGHOME: [&'static str; 2] = [EXAMPLE_DIR, "gnupg"];

const INI_PATH: [&'static str; 2] = [EXAMPLE_DIR, "ananke.ini"];

const JSON_PATH: [&'static str; 3] = [EXAMPLE_DIR, "db", "data.json"];

const SCHEMA_PATH: [&'static str; 2] = ["db", "schema"];

const CURRENT_SCHEMA_VERSION: u64 = 3;

macro_rules! data_file {
    () => {
        JSON_PATH.into_iter().collect::<PathBuf>().into_os_string()
    };
}

fn schema_path(path: impl AsRef<Path>) -> PathBuf {
    let mut path = PathBuf::from(path.as_ref());
    path.push(SCHEMA_PATH.into_iter().collect::<PathBuf>());
    path
}

fn json_vars(
    config_dir: impl Into<OsString>,
    data_dir: impl Into<OsString>,
) -> impl IntoIterator<Item = (OsString, OsString)> + Clone {
    let gnupg_home = GNUPGHOME.iter().collect::<PathBuf>();
    [
        (OsString::from("GNUPGHOME"), gnupg_home.into_os_string()),
        (OsString::from("ANANKE_BACKEND"), OsString::from("json")),
        (OsString::from("ANANKE_CONFIG_DIR"), config_dir.into()),
        (OsString::from("ANANKE_DATA_DIR"), data_dir.into()),
    ]
}

fn sqlite_vars(
    config_dir: impl Into<OsString>,
    data_dir: impl Into<OsString>,
) -> impl IntoIterator<Item = (OsString, OsString)> + Clone {
    let gnupg_home = GNUPGHOME.iter().collect::<PathBuf>();
    [
        (OsString::from("GNUPGHOME"), gnupg_home.into_os_string()),
        (OsString::from("ANANKE_BACKEND"), OsString::from("sqlite")),
        (OsString::from("ANANKE_CONFIG_DIR"), config_dir.into()),
        (OsString::from("ANANKE_DATA_DIR"), data_dir.into()),
    ]
}

fn copy_config(path: impl AsRef<Path>) -> Result<(), io::Error> {
    let source = INI_PATH.into_iter().collect::<PathBuf>();
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
    let vars: [(OsString, OsString); 0] = [];
    Command::new(cargo_bin(BIN))
        .envs(vars)
        .assert()
        .stderr_matches(file!("cli_tests/usage.stderr"))
        .failure();
}

macro_rules! make_tests {
    ($name:ident, $vars:ident) => {
        mod $name {
            use std::{ffi::OsString, fs, path::PathBuf};

            use snapbox::{
                cmd::{cargo_bin, Command},
                file,
                path::PathFixture,
            };

            use super::{copy_config, schema_path, $vars, BIN, CURRENT_SCHEMA_VERSION, JSON_PATH};

            #[test]
            fn import() {
                let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
                let dir = path_fixture.path().expect("should get path");
                copy_config(dir).expect("should copy");
                let data_file: OsString = data_file!();
                let data_file_str: &str = data_file.to_str().expect("should have path");
                Command::new(cargo_bin(BIN))
                    .args(["import", data_file_str])
                    .envs($vars(dir, dir))
                    .assert()
                    .success();
                {
                    let schema_path = schema_path(dir);
                    assert!(schema_path.exists());
                    let schema_version =
                        fs::read_to_string(schema_path).expect("should get schema version");
                    assert_eq!(schema_version, CURRENT_SCHEMA_VERSION.to_string());
                }
            }

            #[test]
            fn lookup() {
                let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
                let dir = path_fixture.path().expect("should get path");
                copy_config(dir).expect("should copy");
                let data_file: OsString = data_file!();
                let data_file_str: &str = data_file.to_str().expect("should have path");
                let schema_path = schema_path(dir);
                Command::new(cargo_bin(BIN))
                    .args(["import", data_file_str])
                    .envs($vars(dir, dir))
                    .assert()
                    .success();
                {
                    let schema_path = schema_path.clone();
                    assert!(schema_path.exists());
                    let schema_version =
                        fs::read_to_string(schema_path).expect("should get schema version");
                    assert_eq!(schema_version, CURRENT_SCHEMA_VERSION.to_string());
                }
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "foomail"])
                    .envs($vars(dir, dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/lookup.stdout"))
                    .success();
                {
                    let schema_path = schema_path.clone();
                    assert!(schema_path.exists());
                    let schema_version =
                        fs::read_to_string(schema_path).expect("should get schema version");
                    assert_eq!(schema_version, CURRENT_SCHEMA_VERSION.to_string());
                }
            }

            #[test]
            fn lookup_single() {
                let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
                let dir = path_fixture.path().expect("should get path");
                copy_config(dir).expect("should copy");
                let data_file: OsString = data_file!();
                let data_file_str: &str = data_file.to_str().expect("should have path");
                Command::new(cargo_bin(BIN))
                    .args(["import", data_file_str])
                    .envs($vars(dir, dir))
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "foomail", "-i", "quux"])
                    .envs($vars(dir, dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/lookup_single.stdout"))
                    .success();
            }

            #[test]
            fn lookup_many() {
                let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
                let dir = path_fixture.path().expect("should get path");
                copy_config(dir).expect("should copy");
                let data_file: OsString = data_file!();
                let data_file_str: &str = data_file.to_str().expect("should have path");
                Command::new(cargo_bin(BIN))
                    .args(["import", data_file_str])
                    .envs($vars(dir, dir))
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "www"])
                    .envs($vars(dir, dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/lookup_many.stdout"))
                    .success();
            }

            #[test]
            fn lookup_many_verbose() {
                let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
                let dir = path_fixture.path().expect("should get path");
                copy_config(dir).expect("should copy");
                let data_file: OsString = data_file!();
                let data_file_str: &str = data_file.to_str().expect("should have path");
                Command::new(cargo_bin(BIN))
                    .args(["import", data_file_str])
                    .envs($vars(dir, dir))
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "www", "-v"])
                    .envs($vars(dir, dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/lookup_many_verbose.stdout"))
                    .success();
            }

            #[test]
            fn lookup_non_existent() {
                let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
                let dir = path_fixture.path().expect("should get path");
                copy_config(dir).expect("should copy");
                let data_file: OsString = data_file!();
                let data_file_str: &str = data_file.to_str().expect("should have path");
                Command::new(cargo_bin(BIN))
                    .args(["import", data_file_str])
                    .envs($vars(dir, dir))
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "paul"])
                    .envs($vars(dir, dir))
                    .assert()
                    .stderr_eq(String::new())
                    .failure()
                    .code(1);
            }

            #[test]
            fn modify() {
                let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
                let dir = path_fixture.path().expect("should get path");
                copy_config(dir).expect("should copy");
                let data_file: OsString = data_file!();
                let data_file_str: &str = data_file.to_str().expect("should have path");
                Command::new(cargo_bin(BIN))
                    .args(["import", data_file_str])
                    .envs($vars(dir, dir))
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["modify", "-p", "-d", "https://www.barphone.com"])
                    .envs($vars(dir, dir))
                    .stdin("MyNewPassword")
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "https://www.barphone.com"])
                    .envs($vars(dir, dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/modify.stdout"))
                    .success();
            }

            #[test]
            fn modify_from_entry_id() {
                let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
                let dir = path_fixture.path().expect("should get path");
                copy_config(dir).expect("should copy");
                let data_file: OsString = data_file!();
                let data_file_str: &str = data_file.to_str().expect("should have path");
                Command::new(cargo_bin(BIN))
                    .args(["import", data_file_str])
                    .envs($vars(dir, dir))
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["modify", "-p", "-e", "39d8363eda9253a779c7719997b1a2656af19af7"])
                    .envs($vars(dir, dir))
                    .stdin("MyNewPassword")
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "https://www.barphone.com"])
                    .envs($vars(dir, dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/modify.stdout"))
                    .success();
            }

            #[test]
            fn modify_non_existent() {
                let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
                let dir = path_fixture.path().expect("should get path");
                copy_config(dir).expect("should copy");
                let data_file: OsString = data_file!();
                let data_file_str: &str = data_file.to_str().expect("should have path");
                Command::new(cargo_bin(BIN))
                    .args(["import", data_file_str])
                    .envs($vars(dir, dir))
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["modify", "-d", "paul"])
                    .envs($vars(dir, dir))
                    .assert()
                    .stderr_eq(file!("cli_tests/modify_non_existent.stderr"))
                    .failure()
                    .code(1);
            }

            #[test]
            fn modify_multiple() {
                let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
                let dir = path_fixture.path().expect("should get path");
                copy_config(dir).expect("should copy");
                let data_file: OsString = data_file!();
                let data_file_str: &str = data_file.to_str().expect("should have path");
                Command::new(cargo_bin(BIN))
                    .args(["import", data_file_str])
                    .envs($vars(dir, dir))
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["remove", "-d", "https://www.foomail.com"])
                    .envs($vars(dir, dir))
                    .assert()
                    .stderr_eq(file!("cli_tests/modify_multiple.stderr"))
                    .failure()
                    .code(1);
            }

            #[test]
            fn remove() {
                let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
                let dir = path_fixture.path().expect("should get path");
                copy_config(dir).expect("should copy");
                let data_file: OsString = data_file!();
                let data_file_str: &str = data_file.to_str().expect("should have path");
                Command::new(cargo_bin(BIN))
                    .args(["import", data_file_str])
                    .envs($vars(dir, dir))
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["remove", "-d", "https://www.barphone.com"])
                    .envs($vars(dir, dir))
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "https://www.barphone.com"])
                    .envs($vars(dir, dir))
                    .assert()
                    .stderr_eq(String::new())
                    .failure()
                    .code(1);
            }

            #[test]
            fn remove_from_entry_id() {
                let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
                let dir = path_fixture.path().expect("should get path");
                copy_config(dir).expect("should copy");
                let data_file: OsString = data_file!();
                let data_file_str: &str = data_file.to_str().expect("should have path");
                Command::new(cargo_bin(BIN))
                    .args(["import", data_file_str])
                    .envs($vars(dir, dir))
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["remove", "-e", "39d8363eda9253a779c7719997b1a2656af19af7"])
                    .envs($vars(dir, dir))
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "https://www.barphone.com"])
                    .envs($vars(dir, dir))
                    .assert()
                    .stderr_eq(String::new())
                    .failure()
                    .code(1);
            }

            #[test]
            fn remove_non_existent() {
                let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
                let dir = path_fixture.path().expect("should get path");
                copy_config(dir).expect("should copy");
                let data_file: OsString = data_file!();
                let data_file_str: &str = data_file.to_str().expect("should have path");
                Command::new(cargo_bin(BIN))
                    .args(["import", data_file_str])
                    .envs($vars(dir, dir))
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["remove", "-d", "paul"])
                    .envs($vars(dir, dir))
                    .assert()
                    .stderr_eq(file!("cli_tests/remove_non_existent.stderr"))
                    .failure()
                    .code(1);
            }

            #[test]
            fn remove_multiple() {
                let path_fixture = PathFixture::mutable_temp().expect("should get path fixture");
                let dir = path_fixture.path().expect("should get path");
                copy_config(dir).expect("should copy");
                let data_file: OsString = data_file!();
                let data_file_str: &str = data_file.to_str().expect("should have path");
                Command::new(cargo_bin(BIN))
                    .args(["import", data_file_str])
                    .envs($vars(dir, dir))
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["remove", "-d", "https://www.foomail.com"])
                    .envs($vars(dir, dir))
                    .assert()
                    .stderr_eq(file!("cli_tests/remove_multiple.stderr"))
                    .failure()
                    .code(1);
            }
        }
    };
}

make_tests!(json, json_vars);
make_tests!(sqlite, sqlite_vars);
