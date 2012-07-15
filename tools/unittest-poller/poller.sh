#!/bin/bash

BASEDIR=`dirname "$0"`

random_map() {
  cat $BASEDIR/maps | perl -ne 'BEGIN{$/=undef}@m=split;print $m[rand($#m)]'
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
