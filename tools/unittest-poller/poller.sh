#!/bin/bash

BASEDIR=`dirname "$0"`

random_map() {
  MAP=`sort -R $BASEDIR/maps | head -n1`
  echo $MAP
}

MINUTES=11

while true; do
  MAP=$(random_map)
  SOLN=`./answer.py`
  echo "Making test for $MAP $SOLN..."
  $BASEDIR/../../unittests/maketest.sh $MAP $SOLN
  echo "Running tests..."
  make -C "$BASEDIR/../.." test
  echo Sleeping for $MINUTES minutes...
  sleep $(($MINUTES*60))
done
