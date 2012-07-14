#!/bin/bash
### usage: ./validate.sh nr ans
###        echo ans |./validate.sh nr

map=$1
if [ $# -eq 1 ]; then read sol ; else sol=$2 ; fi

curl 'http://www.undecidable.org.uk/~edwin/cgi-bin/weblifter.cgi' -d "mapfile=$map&route=$sol" 2>/dev/null | grep Score|awk -F '<br>' '{print $2,$3}'|cut -d' ' -f 2
