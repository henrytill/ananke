use std::{
    backtrace::Backtrace,
    ffi::OsStr,
    fmt,
    io::{self, Read, Write},
    process::{Command, Stdio},
    string,
};

use crate::data::{Ciphertext, KeyId, Plaintext};

#[derive(Debug)]
pub enum ErrorInner {
    Io(io::Error),
    FromUtf8(string::FromUtf8Error),
    MissingStdin,
    MissingStdout,
    Join,
}

impl fmt::Display for ErrorInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorInner::Io(err) => err.fmt(f),
            ErrorInner::FromUtf8(err) => err.fmt(f),
            ErrorInner::MissingStdin => write!(f, "missing stdin"),
            ErrorInner::MissingStdout => write!(f, "missing stdout"),
            ErrorInner::Join => write!(f, "join thread failed"),
        }
    }
}

#[derive(Debug)]
pub struct ErrorImpl {
    inner: ErrorInner,
    backtrace: Option<Backtrace>,
}

#[derive(Debug)]
pub struct Error {
    inner: Box<ErrorImpl>,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.inner.fmt(f)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self.inner.inner {
            ErrorInner::Io(ref err) => Some(err),
            ErrorInner::FromUtf8(ref err) => Some(err),
            ErrorInner::MissingStdin => None,
            ErrorInner::MissingStdout => None,
            ErrorInner::Join => None,
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::capture(ErrorInner::Io(err))
    }
}

impl From<string::FromUtf8Error> for Error {
    fn from(err: string::FromUtf8Error) -> Error {
        Error::capture(ErrorInner::FromUtf8(err))
    }
}

impl Error {
    fn capture(inner: ErrorInner) -> Error {
        let backtrace = Some(Backtrace::capture());
        let inner = Box::new(ErrorImpl { inner, backtrace });
        Error { inner }
    }

    pub fn backtrace(&mut self) -> Option<Backtrace> {
        self.inner.backtrace.take()
    }

    fn missing_stdin() -> Error {
        Error::capture(ErrorInner::MissingStdin)
    }

    fn missing_stdout() -> Error {
        Error::capture(ErrorInner::MissingStdout)
    }

    fn join() -> Error {
        Error::capture(ErrorInner::Join)
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
        .stderr(Stdio::null())
        .spawn()?;

    let stdout_handle = {
        let mut stdout = child.stdout.take().ok_or_else(Error::missing_stdout)?;
        std::thread::spawn(move || {
            let mut buf = Vec::new();
            let _len = stdout.read_to_end(&mut buf)?;
            Ok(buf)
        })
    };

    {
        let mut stdin = child.stdin.take().ok_or_else(Error::missing_stdin)?;
        stdin.write_all(plaintext.as_bytes())?;
    }

    let status = child.wait()?;
    if !status.success() {
        return Err(Error::from(io::Error::other(format!("gpg exited with status {}", status))));
    }

    let stdout: Result<Vec<u8>, io::Error> = stdout_handle.join().map_err(|_| Error::join())?;
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
        let mut stdout = child.stdout.take().ok_or_else(Error::missing_stdout)?;
        std::thread::spawn(move || {
            let mut buf = Vec::new();
            let _len = stdout.read_to_end(&mut buf)?;
            Ok(buf)
        })
    };

    {
        let mut stdin = child.stdin.take().ok_or_else(Error::missing_stdin)?;
        stdin.write_all(ciphertext.as_ref())?;
    }

    let status = child.wait()?;
    if !status.success() {
        return Err(Error::from(io::Error::other(format!("gpg exited with status {}", status))));
    }

    let stdout: Result<Vec<u8>, io::Error> = stdout_handle.join().map_err(|_| Error::join())?;
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
