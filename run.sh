#!/usr/bin/env sh

DIR="$(readlink -f $(dirname "$0"))"
DAYP="$(printf "%02d" "$1")"

"$DIR/cmp.sh" "$1"

if [ "$2" = "--" ]
then "$DIR/out/Day$DAYP" < /dev/stdin
else
    if [ ! -f "$DIR/input/Day$DAYP.txt" ]
       then "$DIR/fetch.sh" "$1" || exit 1
    fi
    "$DIR/out/Day$DAYP" < "$DIR/input/Day$DAYP.txt"
fi
