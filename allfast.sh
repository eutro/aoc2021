#!/usr/bin/env sh

DIR="$(readlink -f $(dirname "$0"))"

cd "$DIR/src/"
DAYS_DONE="$(find -name "Day*.hs" | sort | tail -1 | sed -E 's/[^0-9]//g')"

EXIT_CODE=0
for i in $(seq "$DAYS_DONE")
do echo "Day $i"
   DAYP="$(printf "%02d" "$i")"
   if ! time "$DIR/out/Day$DAYP" < "$DIR/input/Day$DAYP.txt"
   then EXIT_CODE=1
   fi
   echo
done

exit "$EXIT_CODE"
