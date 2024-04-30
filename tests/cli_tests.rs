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
    let data_file_str = data_file.to_str().expect("should convert to str");
    Command::new(cargo_bin(BIN)).args(["import", data_file_str]).envs(vars).assert().success();
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
    ($name:ident, $vars:expr) => {
        mod $name {
            use snapbox::{
                cmd::{cargo_bin, Command},
                file,
                path::PathFixture,
            };

            use crate::base::{self, BIN};

            const MSG_SHOULD_GET_PATH_FIXTURE: &'static str = "should get path fixture";
            const MSG_SHOULD_GET_PATH: &'static str = "should get path";

            #[test]
            fn import() {
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::import($vars(dir));
                base::check_schema(dir, 3);
            }

            #[test]
            fn lookup() {
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::import($vars(dir));
                base::check_schema(dir, 3);
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "foomail"])
                    .envs($vars(dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/lookup.stdout"))
                    .success();
                base::check_schema(dir, 3);
            }

            #[test]
            fn lookup_single() {
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::import($vars(dir));
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "foomail", "-i", "quux"])
                    .envs($vars(dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/lookup_single.stdout"))
                    .success();
            }

            #[test]
            fn lookup_many() {
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
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
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
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
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
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
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
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
            fn modify_from_entry_id() {
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::import($vars(dir));
                Command::new(cargo_bin(BIN))
                    .args(["modify", "-p", "-e", "39d8363eda9253a779c7719997b1a2656af19af7"])
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
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
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
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
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
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
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
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::import($vars(dir));
                Command::new(cargo_bin(BIN))
                    .args(["remove", "-e", "39d8363eda9253a779c7719997b1a2656af19af7"])
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
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
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
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
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
