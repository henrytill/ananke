use std::{
    ffi::OsStr,
    fmt,
    io::{self, Read, Write},
    process::{Command, Stdio},
    string,
};

use crate::data::{Ciphertext, KeyId, Plaintext};

#[derive(Debug)]
pub enum ErrorImpl {
    Io(io::Error),
    FromUtf8(string::FromUtf8Error),
    MissingStdin,
    MissingStdout,
    Join,
}

#[derive(Debug)]
pub struct Error {
    inner: Box<ErrorImpl>,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner.as_ref() {
            ErrorImpl::Io(err) => fmt::Display::fmt(err, f),
            ErrorImpl::FromUtf8(err) => fmt::Display::fmt(err, f),
            ErrorImpl::MissingStdin => write!(f, "missing stdin"),
            ErrorImpl::MissingStdout => write!(f, "missing stdout"),
            ErrorImpl::Join => write!(f, "join thread failed"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self.inner.as_ref() {
            ErrorImpl::Io(err) => Some(err),
            ErrorImpl::FromUtf8(err) => Some(err),
            ErrorImpl::MissingStdin => None,
            ErrorImpl::MissingStdout => None,
            ErrorImpl::Join => None,
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        let inner = Box::new(ErrorImpl::Io(err));
        Error { inner }
    }
}

impl From<string::FromUtf8Error> for Error {
    fn from(err: string::FromUtf8Error) -> Error {
        let inner = Box::new(ErrorImpl::FromUtf8(err));
        Error { inner }
    }
}

impl Error {
    pub fn into_inner(self) -> ErrorImpl {
        *self.inner
    }

    fn missing_stdin() -> Error {
        let inner = Box::new(ErrorImpl::MissingStdin);
        Error { inner }
    }

    fn missing_stdout() -> Error {
        let inner = Box::new(ErrorImpl::MissingStdout);
        Error { inner }
    }

    fn join() -> Error {
        let inner = Box::new(ErrorImpl::Join);
        Error { inner }
    }
}

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
        .spawn()?;

    let join_handle = {
        let plaintext = plaintext.clone();
        let mut stdin = child.stdin.take().ok_or(Error::missing_stdin())?;
        std::thread::spawn(move || stdin.write_all(plaintext.as_str().as_bytes()))
    };

    let mut buf = Vec::new();
    let mut stdout = child.stdout.take().ok_or(Error::missing_stdout())?;
    let _len = stdout.read_to_end(&mut buf)?;

    let status = child.wait()?;
    let thread_result = join_handle.join().map_err(|_| Error::join())?;
    thread_result?;

    if !status.success() {
        return Err(Error::from(io::Error::other(format!("gpg exited with status {}", status))));
    }

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
        .spawn()?;

    let join_handle = {
        let ciphertext = ciphertext.clone();
        let mut stdin = child.stdin.take().ok_or(Error::missing_stdin())?;
        std::thread::spawn(move || stdin.write_all(ciphertext.as_ref()))
    };

    let mut buf = Vec::new();
    let mut stdout = child.stdout.take().ok_or(Error::missing_stdout())?;
    let _len = stdout.read_to_end(&mut buf)?;

    let status = child.wait()?;
    let thread_result = join_handle.join().map_err(|_| Error::join())?;
    thread_result?;

    if !status.success() {
        return Err(Error::from(io::Error::other(format!("gpg exited with status {}", status))));
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
