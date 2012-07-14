while true
do
	ANSWER=`./answer.py`
	MAP=`./map.py`
	echo ${ANSWER} | ./validate.sh ${MAP} > ${MAP}-${ANSWER}

	sleep $((12*60))
done
