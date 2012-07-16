#!/bin/bash

BASEDIR=`dirname "$0"`

random_map() {
  cat $BASEDIR/maps | perl -ne 'BEGIN{$/=undef}@m=split;print $m[rand($#m)]'
}

THROTTLE=${THROTTLE-660}

while true; do
  MAP=$(random_map)
  SOLN=`./answer.py`
  echo "Making test for $MAP $SOLN..."
  $BASEDIR/../../unittests/maketest.sh $MAP $SOLN
  echo "Running tests..."
  make -C "$BASEDIR/../.." test
  echo Sleeping for $THROTTLE seconds...
  sleep $THROTTLE
done
