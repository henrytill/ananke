use std::ffi::{OsStr, OsString};

use anyhow::Error;
use gpgme::{Context, Protocol};

use crate::data::{ArmoredCiphertext, Ciphertext, KeyId, Plaintext};

use super::base::Cipher;

#[derive(Default)]
pub struct Binary {
    env: Vec<(OsString, OsString)>,
}

impl Binary {
    #[cfg(test)]
    pub const fn new(env: Vec<(OsString, OsString)>) -> Binary {
        Binary { env }
    }

    fn context(&self) -> Result<Context, Error> {
        let mut ctx = Context::from_protocol(Protocol::OpenPgp)?;
        for (key, value) in &self.env {
            if key.as_os_str() == "GNUPGHOME" {
                ctx.set_engine_home_dir(value.as_encoded_bytes())?;
                break;
            }
        }
        Ok(ctx)
    }
}

impl Cipher for Binary {
    type Error = anyhow::Error;

    type Out = Ciphertext;

    fn encrypt(&self, key_id: &KeyId, plaintext: &Plaintext) -> Result<Self::Out, Self::Error> {
        let mut ctx = self.context()?;
        ctx.set_armor(false);

        let key = ctx.get_key(key_id.as_str())?;

        let mut ciphertext_buf = Vec::new();
        ctx.encrypt(Some(&key), plaintext.as_bytes(), &mut ciphertext_buf)?;

        Ok(Ciphertext::new(ciphertext_buf))
    }

    fn decrypt(&self, ciphertext: &Self::Out) -> Result<Plaintext, Self::Error> {
        let mut ctx = self.context()?;

        let mut plaintext_buf = Vec::new();
        ctx.decrypt(ciphertext.as_ref(), &mut plaintext_buf)?;

        let txt = String::from_utf8(plaintext_buf)?;
        Ok(Plaintext::new(txt))
    }
}

#[derive(Default)]
pub struct Text {
    env: Vec<(OsString, OsString)>,
}

impl Text {
    #[cfg(test)]
    pub const fn new(env: Vec<(OsString, OsString)>) -> Text {
        Text { env }
    }

    fn context(&self) -> Result<Context, Error> {
        let mut ctx = Context::from_protocol(Protocol::OpenPgp)?;
        for (key, value) in &self.env {
            if key.as_os_str() == "GNUPGHOME" {
                ctx.set_engine_home_dir(value.as_encoded_bytes())?;
                break;
            }
        }
        Ok(ctx)
    }
}

impl Cipher for Text {
    type Error = anyhow::Error;

    type Out = ArmoredCiphertext;

    fn encrypt(&self, key_id: &KeyId, plaintext: &Plaintext) -> Result<Self::Out, Self::Error> {
        let mut ctx = self.context()?;
        ctx.set_armor(true);

        let key = ctx.get_key(key_id.as_str())?;

        let mut ciphertext_buf = Vec::new();
        ctx.encrypt(Some(&key), plaintext.as_bytes(), &mut ciphertext_buf)?;

        let txt = String::from_utf8(ciphertext_buf)?;
        Ok(ArmoredCiphertext::new(txt))
    }

    fn decrypt(&self, ciphertext: &Self::Out) -> Result<Plaintext, Self::Error> {
        let mut ctx = self.context()?;

        let mut plaintext_buf = Vec::new();
        ctx.decrypt(ciphertext.as_bytes(), &mut plaintext_buf)?;

        let txt = String::from_utf8(plaintext_buf)?;
        Ok(Plaintext::new(txt))
    }
}

pub fn suggest_key<F, I, K, V>(f: F) -> Result<Option<KeyId>, Error>
where
    F: Fn() -> I,
    I: IntoIterator<Item = (K, V)>,
    K: AsRef<OsStr>,
    V: AsRef<OsStr>,
{
    let env: Vec<(OsString, OsString)> = f()
        .into_iter()
        .map(|(k, v)| (k.as_ref().to_os_string(), v.as_ref().to_os_string()))
        .collect();

    let mut ctx = Context::from_protocol(Protocol::OpenPgp)?;
    for (key, value) in &env {
        if key.as_os_str() == "GNUPGHOME" {
            ctx.set_engine_home_dir(value.as_encoded_bytes())?;
            break;
        }
    }

    // Try to find a secret key that can be used for signing/encryption
    let mut keys = ctx.secret_keys()?;

    // Get the first valid key
    if let Some(key) = keys.next() {
        let key = key?;
        if let Some(subkey) = key.subkeys().next() {
            if let Ok(fpr) = subkey.fingerprint() {
                // Get last 8 characters (short key ID)
                let key_id = if fpr.len() >= 8 {
                    &fpr[fpr.len() - 8..]
                } else {
                    fpr
                };
                return Ok(Some(KeyId::from(key_id)));
            }
        }
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
            gpgme::{Binary, Text},
        },
        data::{KeyId, Plaintext},
    };

    const RANDOM_LEN: usize = 100 * 1024 * 1024;

    fn vars() -> impl IntoIterator<Item = (OsString, OsString)> {
        const GNUPGHOME: [&str; 2] = [r"example", "gnupg"];
        [(
            OsString::from("GNUPGHOME"),
            GNUPGHOME.iter().collect::<PathBuf>().into_os_string(),
        )]
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
            let mut rng = rand::rng();
            let mut data = Vec::with_capacity(RANDOM_LEN);
            for _ in 0..(RANDOM_LEN / 8) {
                let random_bytes: [u8; 8] = rng.random();
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
            let mut rng = rand::rng();
            let mut data = Vec::with_capacity(RANDOM_LEN);
            for _ in 0..(RANDOM_LEN / 8) {
                let random_bytes: [u8; 8] = rng.random();
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
