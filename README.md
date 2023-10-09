# ananke

[![Haskell CI](https://github.com/henrytill/ananke/actions/workflows/haskell.yml/badge.svg)](https://github.com/henrytill/ananke/actions/workflows/haskell.yml)

`ananke` is a minimal password manager with a command-line interface.  Like [`pass`](https://www.passwordstore.org/), it uses [GnuPG](https://gnupg.org/) for asymmetric encryption of passwords.  Unlike `pass`, it stores all information in an [SQLite](https://sqlite.org) database.

## Status

I created `ananke` primarily for personal use, but recognize that it may be useful to others.  At this moment in time, it should be considered alpha-quality, experimental software.  Prospective users should keep a backup of their passwords elsewhere.

It is worth noting that `ananke` stores information in a manner that allows manual access using `sqlite`, `base64`, and `gpg` command line tools, in the event that a user does not have access to a working `ananke` executable.

## Requirements

To build and install `ananke`, you will need either:

* [cabal](https://www.haskell.org/cabal/)

  or

* [Nix](http://nixos.org/nix/)

To use `ananke`, you will need:

* [GnuPG](https://gnupg.org/)

You should have a basic understanding of how to use GnuPG and have a keypair to use for encryption and decryption of passwords.  If not, you should familiarize yourself with GnuPG by reading the GnuPG MiniHOWTO available [here](https://gnupg.org/documentation/howtos.html).

## Installation

Currently, the recommended way of getting `ananke` is to build and install it from the [latest release](https://github.com/henrytill/ananke/releases/latest).

### with `cabal`

After downloading and decompressing the [latest release](https://github.com/henrytill/ananke/releases/latest),

```sh
$ cd ananke-<version>
$ cabal v2-install exe:ananke
```

This will install the `ananke` executable in `$HOME/.cabal/bin`.

### with `nix-env`

```sh
$ nix-env -f https://github.com/henrytill/ananke/archive/<version>.tar.gz -i ananke
```

## Setup

Before using `ananke`, **you must create a configuration file** at `$HOME/.ananke/ananke.conf`.

Your configuration file should look like the example given [here](example/ananke.conf).

`keyid` is used to specify the public key of the GnuPG keypair with which you will encrypt and decrypt passwords.

You can find the keyid of your public key with the following command:

```sh
$ gpg --keyid-format LONG -k <user>
```

where `<user>` is usually the email address associated with the keypair.

## Usage

```sh
# Add an entry to the database, consisting of a description of the entry, a piece of text to encrypt,
# an optional accompanying identity, and an optional piece of metadata
$ ananke add http://notarealwebsite.com -i alice@notarealserver.com -m "This is some metadata"
Enter text to encrypt: notarealpassword

# Retrieve an entry from the database and display its ciphertext as plaintext (see NOTE below)
# In most cases, this command will cause pinentry to appear.
$ ananke lookup http://notarealwebsite.com
notarealpassword

# Modify the ciphertext of a given entry
$ ananke modify -d http://notarealwebsite.com -c
Enter text to encrypt: anotherfakepassword

# Modify the identity of a given entry
$ ananke modify -d http://notarealwebsite.com -i alice_alt@notarealserver.com

# Modify the metadata of a given entry
$ ananke modify -d http://notarealwebsite.com -m "My alternate account"

# Retrieve the modified entry
$ ananke lookup http://notarealwebsite.com
anotherfakepassword

# Change the description of an entry
$ ananke redescribe -d http://notarealwebsite.com http://notarealwebsite.net

# Check if the old entry still exists
$ ananke lookup http://notarealwebsite.com

# Retrieve the newly-redescribed entry
$ ananke lookup http://notarealwebsite.net
anotherfakepassword

# Retrieve the newly-redescribed entry using fuzzy input
$ ananke lookup notarealwebsite
anotherfakepassword

# Remove an entry from the database
$ ananke remove -d http://notarealwebsite.net
```

### Bash completion

The `ananke` executable can produce a script suitable for use with Bash completion.

To test this feature directly, you can use the following command:

```sh
$ source <(ananke --bash-completion-script `which ananke`)
```

### More info

```sh
$ ananke --help
```
