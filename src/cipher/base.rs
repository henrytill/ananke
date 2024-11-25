use crate::data::{KeyId, Plaintext};

pub trait Cipher {
    type Error;

    type Out;

    fn encrypt(&self, key_id: &KeyId, plaintext: &Plaintext) -> Result<Self::Out, Self::Error>;

    fn decrypt(&self, ciphertext: &Self::Out) -> Result<Plaintext, Self::Error>;
}
