#!/usr/bin/env sh

DIR="$(readlink -f $(dirname "$0"))"
RUN="$DIR/run.sh"

cd "$DIR/src/"
DAYS_DONE="$(find -name "Day*.hs" | sort | tail -1 | sed -E 's/[^0-9]//g')"

EXIT_CODE=0
for i in $(seq "$DAYS_DONE")
do echo "Day $i"
   if ! "$RUN" --time $i -O2
   then EXIT_CODE=1
   fi
   echo
done

exit "$EXIT_CODE"
