mod base;

use std::ffi::OsStr;

use snapbox::cmd::{cargo_bin, Command};

use base::{BIN, EXAMPLE_DIR};

fn import(vars: impl IntoIterator<Item = (impl AsRef<OsStr>, impl AsRef<OsStr>)>) {
    const JSON_PATH: [&str; 3] = [EXAMPLE_DIR, "db", "data.json"];
    let data_file = JSON_PATH.into_iter().collect::<std::path::PathBuf>().into_os_string();
    let data_file_str = data_file.to_str().unwrap();
    Command::new(cargo_bin(BIN)).args(["import", data_file_str]).envs(vars).assert().success();
}

mod usage {
    use std::ffi::OsString;

    use snapbox::{
        cmd::{cargo_bin, Command},
        file,
    };

    use crate::base::BIN;

    #[test]
    fn usage() {
        let vars: [(OsString, OsString); 0] = [];
        Command::new(cargo_bin(BIN))
            .envs(vars)
            .assert()
            .stderr_eq(file!("cli_tests/usage.stderr"))
            .failure();
    }

    #[test]
    fn add() {
        let vars: [(OsString, OsString); 0] = [];
        Command::new(cargo_bin(BIN))
            .args(["add"])
            .envs(vars)
            .assert()
            .stderr_eq(file!("cli_tests/usage_add.stderr"))
            .failure();
    }

    #[test]
    fn lookup() {
        let vars: [(OsString, OsString); 0] = [];
        Command::new(cargo_bin(BIN))
            .args(["lookup"])
            .envs(vars)
            .assert()
            .stderr_eq(file!("cli_tests/usage_lookup.stderr"))
            .failure();
    }

    #[test]
    fn modify() {
        let vars: [(OsString, OsString); 0] = [];
        Command::new(cargo_bin(BIN))
            .args(["modify"])
            .envs(vars)
            .assert()
            .stderr_eq(file!("cli_tests/usage_modify.stderr"))
            .failure();
    }

    #[test]
    fn remove() {
        let vars: [(OsString, OsString); 0] = [];
        Command::new(cargo_bin(BIN))
            .args(["remove"])
            .envs(vars)
            .assert()
            .stderr_eq(file!("cli_tests/usage_remove.stderr"))
            .failure();
    }

    #[test]
    fn import() {
        let vars: [(OsString, OsString); 0] = [];
        Command::new(cargo_bin(BIN))
            .args(["import"])
            .envs(vars)
            .assert()
            .stderr_eq(file!("cli_tests/usage_import.stderr"))
            .failure();
    }

    #[test]
    fn export() {
        let vars: [(OsString, OsString); 0] = [];
        Command::new(cargo_bin(BIN))
            .args(["export"])
            .envs(vars)
            .assert()
            .stderr_eq(file!("cli_tests/usage_export.stderr"))
            .failure();
    }
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
