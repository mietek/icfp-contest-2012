#!/bin/bash

BASEDIR=`dirname "$0"`
TOPDIR=$BASEDIR/../..

VALIDATOR=$TOPDIR/bin/validator
. $TOPDIR/unittests/test.lib

random_map() {
  cat $BASEDIR/maps | perl -ne 'BEGIN{$/=undef}@m=split;print $m[rand($#m)]'
}

THROTTLE=${THROTTLE-660}

while true; do
  MAP=$(random_map)
  SOLN=`$BASEDIR/answer.py`
  echo "Making test for $MAP $SOLN..."
  NEWIN=`$TOPDIR/unittests/maketest.sh $MAP $SOLN`
  # NEWIN=$TOPDIR/unittests/tests/beard3/case3.in
  if [ $? -eq 0 ]; then # success
    make -C $TOPDIR bin/validator
    MAPNAME=$MAP
    MAP=$TOPDIR/tests/$MAPNAME.map
    if run_test $NEWIN; then
      echo "New test succeeds."
    else
      echo "New test fails."
    fi
    echo Sleeping for $THROTTLE seconds...
    sleep $THROTTLE
  else
    echo "Fetch failed, sleeping 5 minutes..."
    sleep $((60*5))
  fi
done
