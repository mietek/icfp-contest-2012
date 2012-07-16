#!/bin/sh

if [ "$1" ]
then
	VALIDATORS="$1"
else
	VALIDATORS="validator lambdamine validate.py"
fi

for v in ${VALIDATORS}
do
	VALIDATOR="./${v}"
	for t in `ls tests`
	do
		echo -e "\033[30;1m---------------------------------------------------------------------------------\033[0m"
		TESTFILE="tests/${t}"
		m=`echo ${t} | cut -d'-' -f 1`
		MAP="../../tests/${m}.map"
		ANSWER=`echo ${t} | cut -d'-' -f 2`
		TMP=`mktemp /tmp/test.${m}.${ANSWER}.XXXXX`
		echo ${ANSWER} | timeout 1 ${VALIDATOR} -vv ${MAP} | head -n1 > $TMP
		echo ${m}
		echo ${ANSWER}
		diff -B $TESTFILE $TMP > /dev/null && echo -e "\033[32;1mok\033[0m" ||
			(echo -e "\033[31;1mfail\033[0m"; echo -e "\033[30;1mexpected:\033[0m"; cat $TESTFILE; echo -e "\033[30;1mgot:\033[0m"; cat $TMP)
		rm $TMP
		echo
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
