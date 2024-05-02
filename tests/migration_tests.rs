#[allow(dead_code)]
mod base;

use std::{fs, path::Path};

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
        file,
        path::PathFixture,
    };

    use crate::base::{self, BIN};

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
        let path_fixture = PathFixture::mutable_temp().unwrap();
        let dir = path_fixture.path().unwrap();
        copy_data(dir);
        super::create_schema_file(dir, 2);
        let vars = base::json_vars(dir);
        Command::new(cargo_bin(BIN))
            .args(["lookup", "foomail"])
            .envs(vars)
            .assert()
            .stdout_eq(file!("cli_tests/lookup.stdout"))
            .success();
        base::check_schema(dir, 3);
        let data_file = {
            let mut tmp = dir.to_path_buf();
            tmp.push("db");
            tmp.push("data.json");
            tmp
        };
        let actual = fs::read(data_file).unwrap();
        snapbox::assert_eq(file!("migration_tests/data-schema-v3.json"), actual);
    }
}

mod sqlite {
    use std::{fs, path::Path};

    use rusqlite::Connection;
    use snapbox::{
        cmd::{cargo_bin, Command},
        file,
        path::PathFixture,
    };

    use crate::base::{self, BIN};

    fn copy_data(dir: impl AsRef<Path>) {
        const SOURCE: [&str; 3] = ["tests", "migration_tests", "data-schema-v1.sql"];
        const TARGET: [&str; 2] = ["db", "db.sqlite"];
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
        };
        let connection = Connection::open(target_file).unwrap();
        let batch = fs::read_to_string(data_file).unwrap();
        connection.execute_batch(batch.as_str()).unwrap();
        connection.close().unwrap();
    }

    #[test]
    fn migrate_v1_v3() {
        let path_fixture = PathFixture::mutable_temp().unwrap();
        let dir = path_fixture.path().unwrap();
        copy_data(dir);
        super::create_schema_file(dir, 1);
        let vars = base::sqlite_vars(dir);
        Command::new(cargo_bin(BIN))
            .args(["lookup", "foomail"])
            .envs(vars)
            .assert()
            .stdout_eq(file!("cli_tests/lookup.stdout"))
            .success();
        base::check_schema(dir, 3);
    }
}
