#!/bin/sh

VALIDATORS="validator lambdamine validate.py"

for v in ${VALIDATORS}
do
	VALIDATOR="./${v}"
	for t in `ls tests`
	do
		TESTFILE="tests/${t}"
		m=`echo ${t} | cut -d'-' -f 1`
		MAP="../../tests/${m}.map"
		ANSWER=`echo ${t} | cut -d'-' -f 2`
		TMP=`mktemp test.${v}.${m}.${ANSWER}.XXXXX.fail`
		echo ${ANSWER} | timeout 1 ${VALIDATOR} -vv ${MAP} | head -n1 > $TMP
		echo
		diff -B $TESTFILE $TMP && (echo $TESTFILE ok; echo; rm $TMP) || (echo "$TESTFILE failed; output in $TMP"; echo)
	done
done


#for d in "`dirname "$0"`/tests/*"; do
#  MAP=$d/map
#  for f in paths/*; do
#    ROUTE=`basename $f`
#    TMP=/tmp/$ROUTE
#    echo "$ROUTE" | $VALIDATOR -vv $MAP > $TMP
#  done
#done
