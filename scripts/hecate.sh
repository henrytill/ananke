#!/bin/sh

CMD="cabal v2-run -v0 exe:hecate"

export HECATE_DATA_DIR="$(pwd)/example"
export GNUPGHOME="$(pwd)/example/gnupg"

exec $CMD -- $@
