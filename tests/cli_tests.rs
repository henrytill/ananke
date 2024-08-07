mod base;

use std::ffi::{OsStr, OsString};

use snapbox::{
    cmd::{cargo_bin, Command},
    file,
};

use base::{BIN, EXAMPLE_DIR};

fn import(vars: impl IntoIterator<Item = (impl AsRef<OsStr>, impl AsRef<OsStr>)>) {
    const JSON_PATH: [&str; 3] = [EXAMPLE_DIR, "db", "data.json"];
    let data_file = JSON_PATH.into_iter().collect::<std::path::PathBuf>().into_os_string();
    let data_file_str = data_file.to_str().unwrap();
    Command::new(cargo_bin(BIN)).args(["import", data_file_str]).envs(vars).assert().success();
}

#[test]
fn usage() {
    let vars: [(OsString, OsString); 0] = [];
    Command::new(cargo_bin(BIN))
        .envs(vars)
        .assert()
        .stderr_eq(file!("cli_tests/usage.stderr"))
        .failure();
}

macro_rules! make_tests {
    ($name:ident, $vars:expr) => {
        mod $name {
            use snapbox::{
                cmd::{cargo_bin, Command},
                dir::DirRoot,
                file,
            };

            use crate::base::{self, BIN};

            #[test]
            fn import() {
                let path_fixture = DirRoot::mutable_temp().unwrap();
                let dir = path_fixture.path().unwrap();
                super::import($vars(dir));
                base::check_schema(dir, 4);
            }

            #[test]
            fn lookup() {
                let path_fixture = DirRoot::mutable_temp().unwrap();
                let dir = path_fixture.path().unwrap();
                super::import($vars(dir));
                base::check_schema(dir, 4);
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "foomail"])
                    .envs($vars(dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/lookup.stdout"))
                    .success();
                base::check_schema(dir, 4);
            }

            #[test]
            fn lookup_identity() {
                let path_fixture = DirRoot::mutable_temp().unwrap();
                let dir = path_fixture.path().unwrap();
                super::import($vars(dir));
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "foomail", "-i", "quux"])
                    .envs($vars(dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/lookup_identity.stdout"))
                    .success();
            }

            #[test]
            fn lookup_many() {
                let path_fixture = DirRoot::mutable_temp().unwrap();
                let dir = path_fixture.path().unwrap();
                super::import($vars(dir));
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "www"])
                    .envs($vars(dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/lookup_many.stdout"))
                    .success();
            }

            #[test]
            fn lookup_many_verbose() {
                let path_fixture = DirRoot::mutable_temp().unwrap();
                let dir = path_fixture.path().unwrap();
                super::import($vars(dir));
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "www", "-v"])
                    .envs($vars(dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/lookup_many_verbose.stdout"))
                    .success();
            }

            #[test]
            fn lookup_non_existent() {
                let path_fixture = DirRoot::mutable_temp().unwrap();
                let dir = path_fixture.path().unwrap();
                super::import($vars(dir));
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "paul"])
                    .envs($vars(dir))
                    .assert()
                    .stderr_eq(String::new())
                    .failure()
                    .code(1);
            }

            #[test]
            fn modify() {
                let path_fixture = DirRoot::mutable_temp().unwrap();
                let dir = path_fixture.path().unwrap();
                super::import($vars(dir));
                Command::new(cargo_bin(BIN))
                    .args(["modify", "-p", "-d", "https://www.barphone.com"])
                    .envs($vars(dir))
                    .stdin("MyNewPassword")
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "https://www.barphone.com"])
                    .envs($vars(dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/modify.stdout"))
                    .success();
            }

            #[test]
            fn modify_retains_id() {
                let path_fixture = DirRoot::mutable_temp().unwrap();
                let dir = path_fixture.path().unwrap();
                super::import($vars(dir));
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "-v", "https://www.barphone.com"])
                    .envs($vars(dir))
                    .assert()
                    .stdout_eq("[..] ba9d7666-f201-4d78-ae30-300ff236de7f 371C136C https://www.barphone.com quux YetAnotherSecretPassword\n")
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["modify", "-p", "-d", "https://www.barphone.com"])
                    .envs($vars(dir))
                    .stdin("MyNewPassword")
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "-v", "https://www.barphone.com"])
                    .envs($vars(dir))
                    .assert()
                    .stdout_eq("[..] ba9d7666-f201-4d78-ae30-300ff236de7f 371C136C https://www.barphone.com quux MyNewPassword\n")
                    .success();
            }

            #[test]
            fn modify_from_entry_id() {
                let path_fixture = DirRoot::mutable_temp().unwrap();
                let dir = path_fixture.path().unwrap();
                super::import($vars(dir));
                Command::new(cargo_bin(BIN))
                    .args(["modify", "-p", "-e", "ba9d7666-f201-4d78-ae30-300ff236de7f"])
                    .envs($vars(dir))
                    .stdin("MyNewPassword")
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "https://www.barphone.com"])
                    .envs($vars(dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/modify.stdout"))
                    .success();
            }

            #[test]
            fn modify_non_existent() {
                let path_fixture = DirRoot::mutable_temp().unwrap();
                let dir = path_fixture.path().unwrap();
                super::import($vars(dir));
                Command::new(cargo_bin(BIN))
                    .args(["modify", "-d", "paul"])
                    .envs($vars(dir))
                    .assert()
                    .stderr_eq(file!("cli_tests/modify_non_existent.stderr"))
                    .failure()
                    .code(1);
            }

            #[test]
            fn modify_multiple() {
                let path_fixture = DirRoot::mutable_temp().unwrap();
                let dir = path_fixture.path().unwrap();
                super::import($vars(dir));
                Command::new(cargo_bin(BIN))
                    .args(["remove", "-d", "https://www.foomail.com"])
                    .envs($vars(dir))
                    .assert()
                    .stderr_eq(file!("cli_tests/modify_multiple.stderr"))
                    .failure()
                    .code(1);
            }

            #[test]
            fn remove() {
                let path_fixture = DirRoot::mutable_temp().unwrap();
                let dir = path_fixture.path().unwrap();
                super::import($vars(dir));
                Command::new(cargo_bin(BIN))
                    .args(["remove", "-d", "https://www.barphone.com"])
                    .envs($vars(dir))
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "https://www.barphone.com"])
                    .envs($vars(dir))
                    .assert()
                    .stderr_eq(String::new())
                    .failure()
                    .code(1);
            }

            #[test]
            fn remove_from_entry_id() {
                let path_fixture = DirRoot::mutable_temp().unwrap();
                let dir = path_fixture.path().unwrap();
                super::import($vars(dir));
                Command::new(cargo_bin(BIN))
                    .args(["remove", "-e", "ba9d7666-f201-4d78-ae30-300ff236de7f"])
                    .envs($vars(dir))
                    .assert()
                    .success();
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "https://www.barphone.com"])
                    .envs($vars(dir))
                    .assert()
                    .stderr_eq(String::new())
                    .failure()
                    .code(1);
            }

            #[test]
            fn remove_non_existent() {
                let path_fixture = DirRoot::mutable_temp().unwrap();
                let dir = path_fixture.path().unwrap();
                super::import($vars(dir));
                Command::new(cargo_bin(BIN))
                    .args(["remove", "-d", "paul"])
                    .envs($vars(dir))
                    .assert()
                    .stderr_eq(file!("cli_tests/remove_non_existent.stderr"))
                    .failure()
                    .code(1);
            }

            #[test]
            fn remove_multiple() {
                let path_fixture = DirRoot::mutable_temp().unwrap();
                let dir = path_fixture.path().unwrap();
                super::import($vars(dir));
                Command::new(cargo_bin(BIN))
                    .args(["remove", "-d", "https://www.foomail.com"])
                    .envs($vars(dir))
                    .assert()
                    .stderr_eq(file!("cli_tests/remove_multiple.stderr"))
                    .failure()
                    .code(1);
            }
        }
    };
}

make_tests!(json, base::json_vars);
make_tests!(sqlite, base::sqlite_vars);
