mod base;

use std::{fs, path::Path};

const UNKNOWN_SCHEMA_VERSION: u64 = u64::MAX;

fn create_schema_file(dir: impl AsRef<Path>, version: u64) {
    const TARGET: [&str; 2] = ["db", "schema"];
    let target_file = {
        let mut tmp = dir.as_ref().to_path_buf();
        tmp.push(TARGET.into_iter().collect::<std::path::PathBuf>());
        tmp
    };
    if let Some(parent) = target_file.parent() {
        if !parent.exists() {
            fs::create_dir_all(parent).unwrap();
        }
    }
    fs::write(target_file, version.to_string()).unwrap();
}

mod json {
    use std::{fs, path::Path};

    use snapbox::{
        self,
        cmd::{cargo_bin, Command},
        dir::DirRoot,
        file,
    };

    use crate::{
        base::{self, BIN},
        UNKNOWN_SCHEMA_VERSION,
    };

    fn copy_data(dir: impl AsRef<Path>) {
        const SOURCE: [&str; 3] = ["tests", "migration_tests", "data-schema-v2.json"];
        const TARGET: [&str; 2] = ["db", "data.json"];
        let data_file = SOURCE.into_iter().collect::<std::path::PathBuf>();
        let target_file = {
            let mut tmp = dir.as_ref().to_path_buf();
            tmp.push(TARGET.into_iter().collect::<std::path::PathBuf>());
            tmp
        };
        if let Some(parent) = target_file.parent() {
            if !parent.exists() {
                fs::create_dir_all(parent).unwrap();
            }
        }
        fs::copy(data_file, target_file).unwrap();
    }

    #[test]
    fn migrate_v2_v3() {
        let path_fixture = DirRoot::mutable_temp().unwrap();
        let dir = path_fixture.path().unwrap();
        copy_data(dir);
        super::create_schema_file(dir, 2);
        let vars = base::json_vars(dir);
        Command::new(cargo_bin(BIN))
            .args(["lookup", "foomail"])
            .envs(vars)
            .assert()
            .stdout_eq_(file!("cli_tests/lookup.stdout"))
            .success();
        base::check_schema(dir, 3);
        let data_file = {
            let mut tmp = dir.to_path_buf();
            tmp.push("db");
            tmp.push("data.json");
            tmp
        };
        let actual = fs::read(data_file).unwrap();
        snapbox::assert_data_eq!(actual, file!("migration_tests/data-schema-v3.json"));
    }

    #[test]
    fn migrate_to_unknown_schema_version() {
        let path_fixture = DirRoot::mutable_temp().unwrap();
        let dir = path_fixture.path().unwrap();
        copy_data(dir);
        super::create_schema_file(dir, UNKNOWN_SCHEMA_VERSION);
        let vars = base::json_vars(dir);
        Command::new(cargo_bin(BIN))
            .args(["lookup", "foomail"])
            .envs(vars)
            .assert()
            .stderr_eq_(file!("migration_tests/migrate_to_unknown_schema_version.stderr"))
            .failure();
    }
}

mod sqlite {
    use std::{fs, path::Path};

    use rusqlite::Connection;
    use snapbox::{
        cmd::{cargo_bin, Command},
        dir::DirRoot,
        file,
    };

    use crate::{
        base::{self, BIN},
        UNKNOWN_SCHEMA_VERSION,
    };

    fn copy_data(dir: impl AsRef<Path>, source: impl AsRef<Path>) {
        const TARGET: [&str; 2] = ["db", "db.sqlite"];
        let target_file = {
            let mut tmp = dir.as_ref().to_path_buf();
            tmp.push(TARGET.into_iter().collect::<std::path::PathBuf>());
            tmp
        };
        if let Some(parent) = target_file.parent() {
            if !parent.exists() {
                fs::create_dir_all(parent).unwrap();
            }
        };
        let connection = Connection::open(target_file).unwrap();
        let batch = fs::read_to_string(source.as_ref()).unwrap();
        connection.execute_batch(batch.as_str()).unwrap();
        connection.close().unwrap();
    }

    #[test]
    fn migrate_v1_v3() {
        let data_file = {
            const SOURCE: [&str; 3] = ["tests", "migration_tests", "data-schema-v1.sql"];
            SOURCE.into_iter().collect::<std::path::PathBuf>()
        };
        let path_fixture = DirRoot::mutable_temp().unwrap();
        let dir = path_fixture.path().unwrap();
        copy_data(dir, data_file);
        super::create_schema_file(dir, 1);
        let vars = base::sqlite_vars(dir);
        Command::new(cargo_bin(BIN))
            .args(["lookup", "foomail"])
            .envs(vars)
            .assert()
            .stdout_eq_(file!("cli_tests/lookup.stdout"))
            .success();
        base::check_schema(dir, 3);
    }

    #[test]
    fn migrate_v2_v3() {
        let data_file = {
            const SOURCE: [&str; 3] = ["tests", "migration_tests", "data-schema-v2.sql"];
            SOURCE.into_iter().collect::<std::path::PathBuf>()
        };
        let path_fixture = DirRoot::mutable_temp().unwrap();
        let dir = path_fixture.path().unwrap();
        copy_data(dir, data_file);
        super::create_schema_file(dir, 2);
        let vars = base::sqlite_vars(dir);
        Command::new(cargo_bin(BIN))
            .args(["lookup", "foomail"])
            .envs(vars)
            .assert()
            .stdout_eq_(file!("cli_tests/lookup.stdout"))
            .success();
        base::check_schema(dir, 3);
    }

    #[test]
    fn migrate_to_unknown_schema_version() {
        let data_file = {
            const SOURCE: [&str; 3] = ["tests", "migration_tests", "data-schema-v2.sql"];
            SOURCE.into_iter().collect::<std::path::PathBuf>()
        };
        let path_fixture = DirRoot::mutable_temp().unwrap();
        let dir = path_fixture.path().unwrap();
        copy_data(dir, data_file);
        super::create_schema_file(dir, UNKNOWN_SCHEMA_VERSION);
        let vars = base::sqlite_vars(dir);
        Command::new(cargo_bin(BIN))
            .args(["lookup", "foomail"])
            .envs(vars)
            .assert()
            .stderr_eq_(file!("migration_tests/migrate_to_unknown_schema_version.stderr"))
            .failure();
    }
}
