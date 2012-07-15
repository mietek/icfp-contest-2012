#!/bin/sh

VALIDATOR=${1-./lambdamine}

for d in `dirname "$0"`/tests/*; do
  MAP=$d/map
  for f in $d/paths/*; do
    ROUTE=`basename $f`
    TMP=/tmp/$ROUTE
    echo "$ROUTE" | $VALIDATOR -vv $MAP > $TMP
    diff -B $f $TMP && (echo $f ok; rm $TMP) || echo "$f failed; output in $TMP" 
  done
done
