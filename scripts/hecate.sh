#!/usr/bin/env sh

export HECATE_DATA_DIR="$(pwd)/example"
export GNUPGHOME="$(pwd)/example/gnupg"

exec cabal v2-run -v0 exe:hecate -- $@
