#!/usr/bin/env sh

DIR="$(readlink -f $(dirname "$0"))"
if [ -z "$1" ]
then DAY="$(date +%d)"
else
    DAY="$1"
    shift
fi
DAYP="$(printf "%02d" "$DAY")"

mkdir -p "$DIR/out"
cd "$DIR/src"
ghc "$@"-o "$DIR/out/Day$DAYP" "$DIR/src/Day$DAYP.hs"
exit $?
