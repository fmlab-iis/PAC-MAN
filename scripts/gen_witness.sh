#!/bin/bash
BASE=CPAchecker-1.4-svn
BIN=${BASE}/scripts/cpa.sh
CONF=${BASE}/config/generateWitness.properties
SPEC=${BASE}/config/specification/sv-comp-reachability.spc
date1=$(date +"%s")
#${BIN} -heap 10000M -config ${CONF} -spec ${SPEC} -noout $*
if [ -d output ]; then
  rm -rf output
fi
mkdir output
#cp template.graphml output/witness.graphml
genWitness/genWitness $1 > witness.graphml
sed -i 's/_call[0-9]//g' witness.graphml
mv witness.graphml output
date2=$(date +"%s")
diff=$(($date2-$date1))
echo -e "\n*** Witness Generation: $(($diff / 60)) minutes and $(($diff % 60)) seconds elapsed."
