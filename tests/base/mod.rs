use std::{
    ffi::OsString,
    fs,
    path::{Path, PathBuf},
};

pub const BIN: &str = env!("CARGO_PKG_NAME");

pub const EXAMPLE_DIR: &str = r"example";

const GNUPGHOME: [&str; 2] = [EXAMPLE_DIR, "gnupg"];

pub fn json_vars(
    data_dir: impl Into<OsString>,
) -> impl IntoIterator<Item = (OsString, OsString)> + Clone {
    let gnupg_home = GNUPGHOME.iter().collect::<PathBuf>();
    [
        (OsString::from("GNUPGHOME"), gnupg_home.into_os_string()),
        (OsString::from("ANANKE_BACKEND"), OsString::from("json")),
        (OsString::from("ANANKE_KEY_ID"), OsString::from("371C136C")),
        (OsString::from("ANANKE_DATA_DIR"), data_dir.into()),
    ]
}

pub fn sqlite_vars(
    data_dir: impl Into<OsString>,
) -> impl IntoIterator<Item = (OsString, OsString)> + Clone {
    let gnupg_home = GNUPGHOME.iter().collect::<PathBuf>();
    [
        (OsString::from("GNUPGHOME"), gnupg_home.into_os_string()),
        (OsString::from("ANANKE_BACKEND"), OsString::from("sqlite")),
        (OsString::from("ANANKE_KEY_ID"), OsString::from("371C136C")),
        (OsString::from("ANANKE_DATA_DIR"), data_dir.into()),
    ]
}

pub fn check_schema(dir: impl AsRef<Path>, version: u64) {
    const SCHEMA_PATH: [&str; 2] = ["db", "schema"];
    let schema_path = {
        let mut path = PathBuf::from(dir.as_ref());
        path.push(SCHEMA_PATH.into_iter().collect::<PathBuf>());
        path
    };
    assert!(schema_path.exists());
    let schema_version = fs::read_to_string(schema_path).unwrap();
    assert_eq!(schema_version, version.to_string());
}
