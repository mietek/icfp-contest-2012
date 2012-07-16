#!/bin/bash

VALIDATOR=$1
GREEN="\033[32m"
RED="\033[31m"
PLAIN="\033[0m"

if [ -z "$VALIDATOR" ]; then
  echo "Argument (validator path) missing."
  exit 1
fi

BASEDIR="`dirname "$0"`"

. $BASEDIR/test.lib

for d in $BASEDIR/tests/*; do
  MAPNAME=`basename $d`
  
  MAP=$d/map
  for INFILE in $d/*.in; do
     run_test $INFILE
  done
done
