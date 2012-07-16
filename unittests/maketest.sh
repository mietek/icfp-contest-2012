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

CURL=${CURL-curl}

if ! command -v $CURL >/dev/null; then
  echo "Curl command $CURL bad"
  exit 3
fi

if [ -z "$1" ] || [ -z "$2" ]; then
  usage
fi

. `dirname "$0"`/test.lib

BASEDIR=`dirname "$0"`/tests/$MAP
if ! [ -d "$BASEDIR" ]; then
  mkdir $BASEDIR
  cp `dirname "$0"`/../tests/$MAP.map $BASEDIR/map || exit 2
fi

if [ -z "$CASENAME" ]; then
  while true; do
    MAXID=1000
    RND=`perl -e "printf '%03d', int(rand($MAXID))"`
    CASENAME="case$RND"
    [ -f "$BASEDIR/$CASENAME.out" ] || break
  done
fi

INFILE=$BASEDIR/$CASENAME.in
OUTFILE=$BASEDIR/$CASENAME.out

if [ -f "$OUTFILE" ]; then
  echo "$OUTFILE already exists."
  exit 3
fi

TMP=`$MKTEMP`

if ! $CURL 'http://www.undecidable.org.uk/~edwin/cgi-bin/weblifter.cgi' -d "mapfile=$MAP&route=$SOLUTION" 2>/dev/null > $TMP; then
  echo "Curl error"
  exit 5
fi

echo $SOLUTION > $INFILE

cat $TMP | 
  perl -ne 'BEGIN{$/=undef;} m@<pre>(.*)</pre>.*Score: (.*?)<br>@s; print "$2\n$1"' > $OUTFILE

rm $TMP

echo $INFILE
