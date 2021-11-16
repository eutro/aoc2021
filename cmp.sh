#!/usr/bin/env sh

DIR="$(readlink -f $(dirname "$0"))"
DAYP="$(printf "%02d" "$1")"
shift

mkdir -p "$DIR/out"
cd "$DIR/src"
ghc "$@"-o "$DIR/out/Day$DAYP" "$DIR/src/Day$DAYP.hs"
