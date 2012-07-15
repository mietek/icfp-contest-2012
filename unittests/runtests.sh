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
  MAPNAME=`basename $d`
  
  MAP=$d/map
  for INFILE in $d/*.in; do
    BASENAME=`basename $INFILE .in`
    OUTFILE=$d/$BASENAME.out
    TMP=`mktemp -t icfp2012`
    cat "$INFILE" | $VALIDATOR -vv $MAP > $TMP
    if diff -B $OUTFILE $TMP > /dev/null; then
      echo -e "$GREEN$MAPNAME/$BASENAME ok $PLAIN"
    else
      echo -e "$RED$MAPNAME/$BASENAME failed!!!$PLAIN"
      echo "Input:"
      cat $INFILE
      echo "Expected output:"
      cat $OUTFILE
      echo "Got:"
      cat $TMP
    fi
    rm $TMP
  done
done
