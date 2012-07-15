#!/bin/bash

VALIDATOR=$1
GREEN="\033[32m"
RED="\033[31m"
PLAIN="\033[0m"

if [ -z "$VALIDATOR" ]; then
  echo "Argument (validator path) missing."
  exit 1
fi

for d in `dirname "$0"`/tests/*; do
  MAP=$d/map
  for f in $d/paths/*; do
    ROUTE=`basename $f`
    TMP=`tempfile`
    echo "$ROUTE" | $VALIDATOR -vv $MAP > $TMP
    if diff -B $f $TMP > /dev/null; then
      echo -e "$GREEN$f ok $PLAIN"; rm $TMP
    else
      echo -e "$RED$f failed!!!$PLAIN"
    fi
  done
done
