#!/usr/bin/env sh

export ANANKE_CONFIG_DIR="$(pwd)/example"
export ANANKE_DATA_DIR="$(pwd)/example"
export GNUPGHOME="$(pwd)/example/gnupg"

exec cabal v2-run -v0 exe:ananke -- $@
