#! /usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

cd "$(dirname $0)"

example_dir="../example"
tmp_dir="$(mktemp -d --tmpdir hecate-migration-XXXXXXXX)"
v3_commit="f891df2f8ddeab9a43e8f44a02e93ae2b591822e"
hecate_v3="./dist-newstyle/build/x86_64-linux/ghc-8.0.2/hecate-0.3.0.0/c/hecate/build/hecate/hecate"
hecate_v6="./dist-newstyle/build/x86_64-linux/ghc-8.0.2/hecate-0.6.0.0/c/hecate/build/hecate/hecate"

export HECATE_DATA_DIR="$tmp_dir/data"
export GNUPGHOME="$(realpath $example_dir)/gnupg"

mkdir -p "$HECATE_DATA_DIR"

cp "$example_dir/hecate.toml" "$HECATE_DATA_DIR/hecate.toml"

echo HECATE_DATA_DIR: $HECATE_DATA_DIR
echo GNUPGHOME: $GNUPGHOME
echo

pushd $tmp_dir

git clone https://github.com/henrytill/hecate.git
cd hecate
git checkout -q v0.3.0.0
cabal new-configure
cabal new-build

echo 'bazzer' | $hecate_v3 add quux

$hecate_v3 lookup -v quux

git checkout -q develop
cabal new-configure
cabal new-build

echo
echo === TESTING MIGRATION ===
echo

$hecate_v6 lookup -v quux
echo

echo 'foozer' | $hecate_v6 add quuxer
echo

$hecate_v6 lookup -v %
