pub mod base;
#[cfg(not(feature = "gpgme"))]
pub mod gpg;
#[cfg(feature = "gpgme")]
pub mod gpgme;
