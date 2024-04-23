use std::{
    ffi::OsStr,
    io::{self, Read, Write},
    process::{Command, Stdio},
};

use anyhow::Error;

use crate::data::{Ciphertext, KeyId, Plaintext};

const TAKE_STDOUT_MSG: &str = "missing stdout";
const TAKE_STDIN_MSG: &str = "missing stdin";
const JOIN_MSG: &str = "join thread failed";

pub fn encrypt<I, K, V>(key_id: &KeyId, plaintext: &Plaintext, vars: I) -> Result<Ciphertext, Error>
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
        .stderr(Stdio::null())
        .spawn()?;

    let stdout_handle = {
        let mut stdout = child.stdout.take().ok_or_else(|| Error::msg(TAKE_STDOUT_MSG))?;
        std::thread::spawn(move || {
            let mut buf = Vec::new();
            let _len = stdout.read_to_end(&mut buf)?;
            Ok(buf)
        })
    };

    {
        let mut stdin = child.stdin.take().ok_or_else(|| Error::msg(TAKE_STDIN_MSG))?;
        stdin.write_all(plaintext.as_bytes())?;
    }

    let status = child.wait()?;
    if !status.success() {
        return Err(Error::from(io::Error::other(format!("gpg exited with status {}", status))));
    }

    let stdout: Result<Vec<u8>, io::Error> =
        stdout_handle.join().map_err(|_| Error::msg(JOIN_MSG))?;
    let buf: Vec<u8> = stdout?;
    Ok(Ciphertext::new(buf))
}

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
        .stderr(Stdio::null())
        .spawn()?;

    let stdout_handle = {
        let mut stdout = child.stdout.take().ok_or_else(|| Error::msg(TAKE_STDOUT_MSG))?;
        std::thread::spawn(move || {
            let mut buf = Vec::new();
            let _len = stdout.read_to_end(&mut buf)?;
            Ok(buf)
        })
    };

    {
        let mut stdin = child.stdin.take().ok_or_else(|| Error::msg(TAKE_STDIN_MSG))?;
        stdin.write_all(ciphertext.as_ref())?;
    }

    let status = child.wait()?;
    if !status.success() {
        return Err(Error::from(io::Error::other(format!("gpg exited with status {}", status))));
    }

    let stdout: Result<Vec<u8>, io::Error> =
        stdout_handle.join().map_err(|_| Error::msg(JOIN_MSG))?;
    let buf: Vec<u8> = stdout?;
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

    fn vars() -> impl IntoIterator<Item = (OsString, OsString)> + Clone {
        [(OsString::from("GNUPGHOME"), GNUPGHOME.iter().collect::<PathBuf>().into_os_string())]
    }

    #[test]
    fn roundtrip() {
        let key_id = KeyId::from("371C136C");
        let plaintext = Plaintext::from("Hello, world!");
        let vars = vars();
        let encrypted = super::encrypt(&key_id, &plaintext, vars.clone()).expect("should encrypt");
        let decrypted = super::decrypt(&encrypted, vars).expect("should decrypt");
        assert_eq!(plaintext, decrypted);
    }

    #[test]
    #[ignore]
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
        let vars = vars();
        let encrypted = super::encrypt(&key_id, &plaintext, vars.clone()).expect("should encrypt");
        let decrypted = super::decrypt(&encrypted, vars).expect("should decrypt");
        assert_eq!(plaintext, decrypted);
    }
}
