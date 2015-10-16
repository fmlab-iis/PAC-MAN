#!/bin/bash
crest_path=crest-0.1.2
bs=$1
binary=$2

if [[ "${bs}" == "" || "${binary}" == "" ]]; then
  echo "Usage: ${0} BATCH_SIZE BINARY"
  exit
fi

if [[ ! -x "${binary}" ]]; then
  echo "The binary file ${binary} is not executable."
  exit
fi

#echo "=========================================================================" | tee -a $binary.txt
#echo "   $binary" | tee -a $binary.txt
echo "$binary" > $binary.txt 2>&1
for i in `seq 1 1 999999`
do
    if grep 'Reached __VERIFIER_error' $binary.txt > /dev/null; then 
        #echo -e "\n============ Reached error at iteration $((i-1))" | tee -a $binary.txt
        echo -e "\n\nReached error at iteration $((i-1))\n" | tee -a $binary.txt
        exit 0
    else
#echo -e "\n============ Run No. $i ==================================================" | tee -a $binary.txt
        echo -e -n "." | tee -a $binary.txt
        echo -e "\n\nRun No. $i\n" >> $binary.txt 2>&1
        if [ $i -ne 1 ]; then
          rm szd_execution
          rm decisions
        fi
        #$crest_path/bin/run_crest ./$binary $bs -cfg |& tee -a $binary.txt
        $crest_path/bin/run_crest ./$binary $bs -cfg >> $binary.txt 2>&1 #|& tee -a 
    fi
done
