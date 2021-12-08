#!/usr/bin/env sh

DIR="$(readlink -f $(dirname "$0"))"
if [ -z "$1" ]
then DAYN="$(date +%d)"
else DAYN="$1"
fi
DAY="$(echo "$DAYN" | sed 's/^0*//')"
DAYP="$(printf "%02d" "$DAY")"

"$DIR/cmp.sh" "$DAY" || exit 1

if [ "$2" = "--" ]
then "$DIR/out/Day$DAYP" < /dev/stdin
else
    if [ ! -f "$DIR/input/Day$DAYP.txt" ]
       then "$DIR/fetch.sh" "$DAY" || exit 1
    fi
    "$DIR/out/Day$DAYP" < "$DIR/input/Day$DAYP.txt"
fi
