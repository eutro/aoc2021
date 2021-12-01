#!/usr/bin/env sh

DIR="$(readlink -f $(dirname "$0"))"
cd "$DIR" || exit 1

KEY="$(cat session.key)"

if [ -z "$1" ]
then DAYX="$(date +%d)"
else DAYX="$1"
fi
DAY="$(printf "%d" "$DAYX")"
DAYP="$(printf "%02d" "$DAYX")"

mkdir -p input
echo "Fetching day $DAY..."
curl -fs -H"Cookie: session=$KEY" \
     "https://adventofcode.com/2021/day/$DAY/input" \
     -o "input/Day$DAYP.txt" &&
    echo "Success" && exit 0 || echo "Errored" && exit 1
