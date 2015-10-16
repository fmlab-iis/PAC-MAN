#!/bin/bash
BASE=CPAchecker
BIN=${BASE}/scripts/cpa.sh
CONF=${BASE}/config/generateWitness.properties
SPEC=${BASE}/config/specification/sv-comp-reachability.spc
date1=$(date +"%s")
${BIN} -heap 10000M -config ${CONF} -spec ${SPEC} -noout $*
date2=$(date +"%s")
diff=$(($date2-$date1))
echo -e "\n*** Witness Generation: $(($diff / 60)) minutes and $(($diff % 60)) seconds elapsed."
