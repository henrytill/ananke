use std::{
    ffi::{OsStr, OsString},
    fs,
    path::{Path, PathBuf},
};

use snapbox::{
    cmd::{cargo_bin, Command},
    file,
};

const BIN: &str = env!("CARGO_PKG_NAME");

const EXAMPLE_DIR: &str = r"example";

const GNUPGHOME: [&str; 2] = [EXAMPLE_DIR, "gnupg"];

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

fn copy_config(path: impl AsRef<Path>) {
    const INI_PATH: [&str; 2] = [EXAMPLE_DIR, "ananke.ini"];
    let source = INI_PATH.into_iter().collect::<PathBuf>();
    let dest: PathBuf = {
        let mut ret: PathBuf = path.as_ref().into();
        ret.push("ananke.ini");
        ret
    };
    fs::copy(source, dest).expect("should copy");
}

fn import(vars: impl IntoIterator<Item = (impl AsRef<OsStr>, impl AsRef<OsStr>)>) {
    const JSON_PATH: [&str; 3] = [EXAMPLE_DIR, "db", "data.json"];
    let data_file = JSON_PATH.into_iter().collect::<std::path::PathBuf>().into_os_string();
    let data_file_str = data_file.to_str().expect("should convert to str");
    Command::new(cargo_bin(BIN)).args(["import", data_file_str]).envs(vars).assert().success();
}

fn check_schema(dir: impl AsRef<Path>) {
    const SCHEMA_PATH: [&str; 2] = ["db", "schema"];
    const CURRENT_SCHEMA_VERSION: u64 = 3;
    let schema_path = {
        let mut path = PathBuf::from(dir.as_ref());
        path.push(SCHEMA_PATH.into_iter().collect::<PathBuf>());
        path
    };
    assert!(schema_path.exists());
    let schema_version = fs::read_to_string(schema_path).expect("should get schema version");
    assert_eq!(schema_version, CURRENT_SCHEMA_VERSION.to_string());
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
            use snapbox::{
                cmd::{cargo_bin, Command},
                file,
                path::PathFixture,
            };

            use super::{$vars, BIN};

            const MSG_SHOULD_GET_PATH_FIXTURE: &'static str = "should get path fixture";
            const MSG_SHOULD_GET_PATH: &'static str = "should get path";

            #[test]
            fn import() {
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::copy_config(dir);
                super::import($vars(dir, dir));
                super::check_schema(dir);
            }

            #[test]
            fn lookup() {
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::copy_config(dir);
                super::import($vars(dir, dir));
                super::check_schema(dir);
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "foomail"])
                    .envs($vars(dir, dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/lookup.stdout"))
                    .success();
                super::check_schema(dir);
            }

            #[test]
            fn lookup_single() {
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::copy_config(dir);
                super::import($vars(dir, dir));
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "foomail", "-i", "quux"])
                    .envs($vars(dir, dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/lookup_single.stdout"))
                    .success();
            }

            #[test]
            fn lookup_many() {
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::copy_config(dir);
                super::import($vars(dir, dir));
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "www"])
                    .envs($vars(dir, dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/lookup_many.stdout"))
                    .success();
            }

            #[test]
            fn lookup_many_verbose() {
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::copy_config(dir);
                super::import($vars(dir, dir));
                Command::new(cargo_bin(BIN))
                    .args(["lookup", "www", "-v"])
                    .envs($vars(dir, dir))
                    .assert()
                    .stdout_eq(file!("cli_tests/lookup_many_verbose.stdout"))
                    .success();
            }

            #[test]
            fn lookup_non_existent() {
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::copy_config(dir);
                super::import($vars(dir, dir));
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
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::copy_config(dir);
                super::import($vars(dir, dir));
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
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::copy_config(dir);
                super::import($vars(dir, dir));
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
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::copy_config(dir);
                super::import($vars(dir, dir));
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
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::copy_config(dir);
                super::import($vars(dir, dir));
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
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::copy_config(dir);
                super::import($vars(dir, dir));
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
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::copy_config(dir);
                super::import($vars(dir, dir));
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
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::copy_config(dir);
                super::import($vars(dir, dir));
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
                let path_fixture = PathFixture::mutable_temp().expect(MSG_SHOULD_GET_PATH_FIXTURE);
                let dir = path_fixture.path().expect(MSG_SHOULD_GET_PATH);
                super::copy_config(dir);
                super::import($vars(dir, dir));
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
