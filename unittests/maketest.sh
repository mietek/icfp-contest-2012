#!/bin/bash
# Fetch a test case from the web validator.

usage() {
  cat << EOF
Usage:
  maketest.sh <map> <solution> [<casename>]
EOF
  exit 1
}

MAP=$1
SOLUTION=$2
CASENAME=$3

if [ -z "$1" ] || [ -z "$2" ]; then
  usage
fi

BASEDIR=`dirname "$0"`/tests/$MAP
if ! [ -d "$BASEDIR" ]; then
  mkdir $BASEDIR
  cp tests/$MAP.map $BASEDIR/map || exit 2
fi

if [ -z "$CASENAME" ]; then
  i=0
  while [ -f "$BASEDIR/case$i.in" ]; do i=$((i+1)); done
  CASENAME="case$i"
fi

INFILE=$BASEDIR/$CASENAME.in
OUTFILE=$BASEDIR/$CASENAME.out

if [ -f "$OUTFILE" ]; then
  echo "$OUTFILE already exists."
  exit 3
fi

echo $SOLUTION > $INFILE

echo "Fetching validator output..."
curl 'http://www.undecidable.org.uk/~edwin/cgi-bin/weblifter.cgi' -d "mapfile=$MAP&route=$SOLUTION" 2>/dev/null | 
  perl -ne 'BEGIN{$/=undef;} m@<pre>(.*)</pre>.*Score: (.*?)<br>@s; print "$2\n$1"' |
  tee $OUTFILE
