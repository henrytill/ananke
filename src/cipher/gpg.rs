use std::{
    ffi::{OsStr, OsString},
    io::{self, BufRead, BufReader, Read, Write},
    process::{Command, Stdio},
    thread::JoinHandle,
};

use anyhow::Error;

use crate::data::{ArmoredCiphertext, Ciphertext, KeyId, Plaintext};

use super::base::Cipher;

const MSG_TAKE_STDOUT: &str = "missing stdout";
const MSG_TAKE_STDIN: &str = "missing stdin";
const MSG_JOIN: &str = "join thread failed";

#[derive(Default)]
pub struct Binary {
    env: Vec<(OsString, OsString)>,
}

#[cfg(test)]
impl Binary {
    pub const fn new(env: Vec<(OsString, OsString)>) -> Binary {
        Binary { env }
    }
}

impl Cipher for Binary {
    type Error = anyhow::Error;

    type Out = Ciphertext;

    fn encrypt(&self, key_id: &KeyId, plaintext: &Plaintext) -> Result<Self::Out, Self::Error> {
        let cmd = {
            let mut tmp = Command::new("gpg");
            let vars = self.env.iter().cloned();
            tmp.args(["--batch", "-q", "-e", "-r", key_id.as_str()]).envs(vars);
            tmp
        };
        let buf = run(cmd, plaintext.as_bytes())?;
        Ok(Ciphertext::new(buf))
    }

    fn decrypt(&self, ciphertext: &Self::Out) -> Result<Plaintext, Self::Error> {
        let cmd = {
            let mut tmp = Command::new("gpg");
            let vars = self.env.iter().cloned();
            tmp.args(["--batch", "-q", "-d"]).envs(vars);
            tmp
        };
        let buf = run(cmd, ciphertext.as_ref())?;
        let txt = String::from_utf8(buf)?;
        Ok(Plaintext::new(txt))
    }
}

#[derive(Default)]
pub struct Text {
    env: Vec<(OsString, OsString)>,
}

#[cfg(test)]
impl Text {
    pub const fn new(env: Vec<(OsString, OsString)>) -> Text {
        Text { env }
    }
}

impl Cipher for Text {
    type Error = anyhow::Error;

    type Out = ArmoredCiphertext;

    fn encrypt(&self, key_id: &KeyId, plaintext: &Plaintext) -> Result<Self::Out, Self::Error> {
        let cmd = {
            let mut tmp = Command::new("gpg");
            let vars = self.env.iter().cloned();
            tmp.args(["--batch", "--armor", "-q", "-e", "-r", key_id.as_str()]).envs(vars);
            tmp
        };
        let buf = run(cmd, plaintext.as_bytes())?;
        let txt = String::from_utf8(buf)?;
        Ok(ArmoredCiphertext::new(txt))
    }

    fn decrypt(&self, ciphertext: &Self::Out) -> Result<Plaintext, Self::Error> {
        let cmd = {
            let mut tmp = Command::new("gpg");
            let vars = self.env.iter().cloned();
            tmp.args(["--batch", "-q", "-d"]).envs(vars);
            tmp
        };
        let buf = run(cmd, ciphertext.as_bytes())?;
        let txt = String::from_utf8(buf)?;
        Ok(Plaintext::new(txt))
    }
}

fn run(mut cmd: Command, buf: &[u8]) -> Result<Vec<u8>, Error> {
    let program = cmd.get_program().to_os_string(); // for error messages

    let mut child =
        cmd.stdin(Stdio::piped()).stdout(Stdio::piped()).stderr(Stdio::null()).spawn()?;

    let stdout_handle: JoinHandle<Result<Vec<u8>, io::Error>> = {
        let mut stdout = child.stdout.take().ok_or_else(|| Error::msg(MSG_TAKE_STDOUT))?;
        std::thread::spawn(move || {
            let mut buf = Vec::new();
            let _len = stdout.read_to_end(&mut buf)?;
            Ok(buf)
        })
    };

    {
        let mut stdin = child.stdin.take().ok_or_else(|| Error::msg(MSG_TAKE_STDIN))?;
        stdin.write_all(buf)?;
    }

    let status = child.wait()?;
    if !status.success() {
        let msg = format!("{} exited with status {}", program.to_string_lossy(), status);
        return Err(Error::msg(msg));
    }

    let buf_or_error = stdout_handle.join().map_err(|_| Error::msg(MSG_JOIN))?;
    let buf = buf_or_error?;
    Ok(buf)
}

pub fn suggest_key<F, I, K, V>(f: F) -> Result<Option<KeyId>, Error>
where
    F: Fn() -> I,
    I: IntoIterator<Item = (K, V)>,
    K: AsRef<OsStr>,
    V: AsRef<OsStr>,
{
    // Try getting default public key
    // https://lists.gnupg.org/pipermail/gnupg-devel/2011-November/026308.html
    let mut child = Command::new("gpgconf")
        .args(["--list-options", "gpg"])
        .envs(f())
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()?;

    let maybe_key_id: Option<KeyId> = {
        let mut tmp = None;
        let stdout = child.stdout.take().ok_or_else(|| Error::msg(MSG_TAKE_STDOUT))?;
        for line in BufReader::new(stdout).lines() {
            let line = line?;
            let mut fields = line.split(':');
            match fields.next() {
                Some("default-key") => {}
                _ => continue,
            }
            match fields.nth(8) {
                Some(key) if !key.is_empty() => {
                    let key = &key[1..];
                    tmp = Some(KeyId::from(key));
                }
                _ => {}
            }
            break;
        }
        tmp
    };

    let status = child.wait()?;
    if !status.success() {
        return Err(Error::from(io::Error::other(format!("gpg exited with status {}", status))));
    }

    if let result @ Some(_) = maybe_key_id {
        return Ok(result);
    }

    // Fall back to getting first public key listed
    let mut child = Command::new("gpg")
        .args(["-k", "--with-colons"])
        .envs(f())
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()?;

    let maybe_key_id: Option<KeyId> = {
        let mut tmp = None;
        let stdout = child.stdout.take().ok_or_else(|| Error::msg(MSG_TAKE_STDOUT))?;
        for line in BufReader::new(stdout).lines() {
            let line = line?;
            let mut fields = line.split(':');
            match fields.next() {
                Some("pub") => {}
                _ => continue,
            }
            match fields.nth(3) {
                Some(key) if !key.is_empty() => {
                    let start_pos = key.char_indices().nth_back(7).map(|x| x.0).unwrap_or(0);
                    let key = &key[start_pos..];
                    tmp = Some(KeyId::from(key));
                }
                _ => {}
            }
            break;
        }
        tmp
    };

    let status = child.wait()?;
    if !status.success() {
        return Err(Error::from(io::Error::other(format!("gpg exited with status {}", status))));
    }

    if let result @ Some(_) = maybe_key_id {
        return Ok(result);
    }

    Ok(None)
}

#[cfg(test)]
mod tests {
    use std::{ffi::OsString, path::PathBuf};

    use rand::Rng;

    use crate::{
        cipher::{
            base::Cipher,
            gpg::{Binary, Text},
        },
        data::{KeyId, Plaintext},
    };

    const RANDOM_LEN: usize = 100 * 1024 * 1024;

    fn vars() -> impl IntoIterator<Item = (OsString, OsString)> {
        const GNUPGHOME: [&str; 2] = [r"example", "gnupg"];
        [(OsString::from("GNUPGHOME"), GNUPGHOME.iter().collect::<PathBuf>().into_os_string())]
    }

    #[test]
    fn roundtrip_binary() {
        let cipher = Binary::new(vars().into_iter().collect());
        let key_id = KeyId::from("371C136C");
        let plaintext = Plaintext::from("Hello, world!");
        let encrypted = cipher.encrypt(&key_id, &plaintext).unwrap();
        let decrypted = cipher.decrypt(&encrypted).unwrap();
        assert_eq!(plaintext, decrypted);
    }

    #[test]
    fn roundtrip_text() {
        let cipher = Text::new(vars().into_iter().collect());
        let key_id = KeyId::from("371C136C");
        let plaintext = Plaintext::from("Hello, world!");
        let encrypted = cipher.encrypt(&key_id, &plaintext).unwrap();
        let decrypted = cipher.decrypt(&encrypted).unwrap();
        assert_eq!(plaintext, decrypted);
    }

    #[test]
    #[ignore]
    fn roundtrip_binary_large() {
        let cipher = Binary::new(vars().into_iter().collect());
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
        let encrypted = cipher.encrypt(&key_id, &plaintext).unwrap();
        let decrypted = cipher.decrypt(&encrypted).unwrap();
        assert_eq!(plaintext, decrypted);
    }

    #[test]
    #[ignore]
    fn roundtrip_text_large() {
        let cipher = Text::new(vars().into_iter().collect());
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
        let encrypted = cipher.encrypt(&key_id, &plaintext).unwrap();
        let decrypted = cipher.decrypt(&encrypted).unwrap();
        assert_eq!(plaintext, decrypted);
    }

    #[test]
    fn suggest_key() {
        let expected = Some(KeyId::from("371C136C"));
        let actual = super::suggest_key(vars).unwrap();
        assert_eq!(expected, actual);
    }
}
