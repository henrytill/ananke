use std::{
    ffi::OsStr,
    io::{self, Read, Write},
    process::{Command, Stdio},
    string,
};

use crate::data::{Ciphertext, KeyId, Plaintext};

#[allow(dead_code)]
#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    FromUtf8(string::FromUtf8Error),
    MissingStdin,
    MissingStdout,
    Join,
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Io(err)
    }
}

impl From<string::FromUtf8Error> for Error {
    fn from(err: string::FromUtf8Error) -> Self {
        Error::FromUtf8(err)
    }
}

#[allow(dead_code)]
pub fn encrypt<I, K, V>(key_id: KeyId, plaintext: &Plaintext, vars: I) -> Result<Ciphertext, Error>
where
    I: IntoIterator<Item = (K, V)>,
    K: AsRef<OsStr>,
    V: AsRef<OsStr>,
{
    let mut child = Command::new("gpg")
        .args(["--batch", "-q", "-e", "-r", key_id.as_str()])
        .envs(vars)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;

    let join_handle = {
        let plaintext = plaintext.clone();
        let mut stdin = child.stdin.take().ok_or(Error::MissingStdin)?;
        std::thread::spawn(move || stdin.write_all(plaintext.as_str().as_bytes()))
    };

    let mut buf = Vec::new();
    let mut stdout = child.stdout.take().ok_or(Error::MissingStdout)?;
    let _len = stdout.read_to_end(&mut buf)?;

    let status = child.wait()?;
    let thread_result = join_handle.join().map_err(|_| Error::Join)?;
    thread_result?;

    if !status.success() {
        return Err(Error::from(io::Error::other(format!(
            "gpg exited with status {}",
            status
        ))));
    }

    Ok(Ciphertext::new(buf))
}

#[allow(dead_code)]
pub fn decrypt<I, K, V>(ciphertext: &Ciphertext, vars: I) -> Result<Plaintext, Error>
where
    I: IntoIterator<Item = (K, V)>,
    K: AsRef<OsStr>,
    V: AsRef<OsStr>,
{
    let mut child = Command::new("gpg")
        .args(["--batch", "-q", "-d"])
        .envs(vars)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;

    let join_handle = {
        let ciphertext = ciphertext.clone();
        let mut stdin = child.stdin.take().ok_or(Error::MissingStdin)?;
        std::thread::spawn(move || stdin.write_all(ciphertext.as_ref()))
    };

    let mut buf = Vec::new();
    let mut stdout = child.stdout.take().ok_or(Error::MissingStdout)?;
    let _len = stdout.read_to_end(&mut buf)?;

    let status = child.wait()?;
    let thread_result = join_handle.join().map_err(|_| Error::Join)?;
    thread_result?;

    if !status.success() {
        return Err(Error::from(io::Error::other(format!(
            "gpg exited with status {}",
            status
        ))));
    }

    let txt = String::from_utf8(buf)?;
    Ok(Plaintext::new(txt))
}

#[cfg(test)]
mod tests {
    use std::{ffi::OsString, path::PathBuf};

    use rand::Rng;

    use crate::data::{KeyId, Plaintext};

    const GNUPGHOME: [&str; 2] = [r"example", "gnupg"];

    const RANDOM_LEN: usize = 100 * 1024 * 1024;

    #[test]
    fn roundtrip() {
        let key_id = KeyId::from("371C136C");
        let plaintext = Plaintext::from("Hello, world!");
        let vars = {
            let key = OsString::from("GNUPGHOME");
            let val = GNUPGHOME.iter().collect::<PathBuf>().into_os_string();
            [(key, val)].into_iter()
        };
        let encrypted = super::encrypt(key_id, &plaintext, vars.clone()).expect("should encrypt");
        let decrypted = super::decrypt(&encrypted, vars).expect("should decrypt");
        assert_eq!(plaintext, decrypted);
    }

    #[test]
    fn roundtrip_large() {
        let random = {
            let mut rng = rand::thread_rng();
            let mut data = Vec::with_capacity(RANDOM_LEN);
            for _ in 0..(RANDOM_LEN / 8) {
                let random_bytes: [u8; 8] = rng.gen();
                data.extend_from_slice(&random_bytes);
            }
            String::from_utf8_lossy(&data).to_string()
        };

        let key_id = KeyId::from("371C136C");
        let plaintext = Plaintext::from(random);
        let vars = {
            let key = OsString::from("GNUPGHOME");
            let val = GNUPGHOME.iter().collect::<PathBuf>().into_os_string();
            [(key, val)].into_iter()
        };
        let encrypted = super::encrypt(key_id, &plaintext, vars.clone()).expect("should encrypt");
        let decrypted = super::decrypt(&encrypted, vars).expect("should decrypt");
        assert_eq!(plaintext, decrypted);
    }
}
