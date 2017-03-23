# hecate

[![Build Status](https://travis-ci.org/henrytill/hecate.svg?branch=master)](https://travis-ci.org/henrytill/hecate)

`hecate` is a minimal password manager with a command-line interface.  Like [`pass`](https://www.passwordstore.org/) and its derivatives, it uses [GnuPG](https://gnupg.org/) for asymmetric encryption of passwords.  Unlike `pass`, it stores all information in an [SQLite](https://sqlite.org) database.  It is also written in Haskell, rather than Bash.

## Status

I created `hecate` primarily for personal use, but recognize that it may be useful to others.  At this moment in time, it should be considered alpha-quality, experimental software.  Prospective users should keep a backup of their passwords elsewhere.  

It is worth nothing that `hecate` stores information in a manner that allows manual access using `sqlite`, `base64`, and `gpg` command line tools, in the event that a user does not have access to a working `hecate` executable.

## Requirements

To build and install `hecate`, you will need either:

* [Nix](http://nixos.org/nix/) (**recommended**)

  or

* [Cabal](https://www.haskell.org/cabal/)

To use `hecate`, you will need:

* [GnuPG](https://gnupg.org/)
* [SQLite](https://sqlite.org/)

**NOTE:** SQLite is pre-installed on macOS and some Linux distributions.

You should have a basic understanding of how to use GnuPG and have a keypair to use for encryption and decryption of passwords.  If not, you should familiarize yourself with GnuPG by reading the GnuPG MiniHOWTO available [here](https://gnupg.org/documentation/howtos.html).

## Installation

### with Nix

```
$ git clone https://github.com/henrytill/hecate.git
  ...
$ cd hecate
$ nix-env -f default.nix -i hecate
```

### with Cabal

```
$ git clone https://github.com/henrytill/hecate.git
  ...
$ cd hecate
$ cabal sandbox init
  ...
$ cabal install --enable-relocatable
```

You can then move the resultant `hecate` executable to a desired location.  For example:
```
$ mv <path to hecate clone>/.cabal-sandbox/bin/hecate /usr/local/bin
```

## Setup

Before using `hecate`, **you must create a configuration file** at `$HOME/.hecate/hecate.toml`.

Your configuration file should look like the example given [here](example/hecate.toml).

`keyid` is used to specify the public key of the GnuPG keypair with which you will encrypt and decrypt passwords.

You can find the keyid of your public key with the following command:

```
$ gpg --keyid-format LONG -k <user>
```

where `<user>` is usually the email address associated with the keypair.

## Usage

```
$ hecate --help
```
