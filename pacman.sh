#!/bin/bash
if [ $# -ne 1 ]; then
  echo
  echo "Invalid file. "
  echo "Usage: ./pacman.sh [Filename]"
  echo
  exit 1
fi

in_file=$1
raw_file="${in_file##*/}"
echo -e "\n=================================================================================\n"
echo "VERIFICATION BEGIN: $raw_file" | tee -a results
#echo "VERIFICATION BEGIN: $raw_file"
echo -e "\n---------------------------------------------------------------------------------\n"
if [ ! -f $raw_file ]; then
  cp $in_file .
fi
temp=${raw_file%.*}
mod_file="$temp"_modded.c
binary=${mod_file%.*}
cp $raw_file $mod_file

# Modify the file for CREST

sed -i -e '1i #include <crest.h>\n' $mod_file 
if ! grep -q "stdio.h" $mod_file ; then
    if ! grep -q "_IO_flockfile" $mod_file ; then
        sed -i -e '1i #include <stdio.h>\n' $mod_file
    fi
fi
if ! grep -q "stdlib.h" $mod_file ; then
    if ! grep -q "void \*malloc(int size)" $mod_file; then
        sed -i -e '1i #include <stdlib.h>\n' $mod_file
    fi
fi

if grep -q "extern  __attribute__((__nothrow__)) void \*malloc(size_t __size )  __attribute__((__malloc__))" $mod_file; then
    sed -i 's/extern  __attribute__((__nothrow__)) void \*malloc(size_t __size )  __attribute__((__malloc__))//g' $mod_file
fi
if grep -q "extern  __attribute__((__nothrow__)) void \*malloc(size_t __size ) __attribute__((__malloc__))" $mod_file; then
    sed -i 's/extern  __attribute__((__nothrow__)) void \*malloc(size_t __size ) __attribute__((__malloc__))//g' $mod_file
fi
if grep -q "void \*malloc(size_t size);" $mod_file; then
    sed -i 's/void \*malloc(size_t size);//g' $mod_file
fi
if grep -q "int snprintf(char \* buf, size_t size, const char \* fmt, ...);" $mod_file; then
    sed -i 's/int snprintf(char \* buf, size_t size, const char \* fmt, ...);//g' $mod_file
fi
if grep -q "int __VERIFIER_nondet_int(void) { int val; return val; }" $mod_file; then
    sed -i 's/int __VERIFIER_nondet_int(void) { int val; return val; }//g' $mod_file
fi

echo 'int __VERIFIER_nondet_int() { int ret; CREST_int(ret); return ret; }' >> $mod_file
echo 'unsigned int __VERIFIER_nondet_uint() { unsigned int ret; CREST_unsigned_int(ret); return ret; }' >> $mod_file
echo '_Bool __VERIFIER_nondet__Bool() { int ret; CREST_int(ret); return (ret >= 0); }' >> $mod_file
if grep -q "enum __bool __VERIFIER_nondet_bool();" $mod_file; then 
    sed -i 's/enum __bool __VERIFIER_nondet_bool();//g' $mod_file
fi
echo '_Bool __VERIFIER_nondet_bool() { int ret; CREST_int(ret); return (ret >= 0); }' >> $mod_file
echo 'char __VERIFIER_nondet_char() { char ret; CREST_char(ret); return ret; }' >> $mod_file
echo 'long __VERIFIER_nondet_long() { long ret; CREST_long(ret); return ret; }' >> $mod_file
echo 'unsigned short __VERIFIER_nondet_ushort() { unsigned short ret; CREST_unsigned_short(ret); return ret; }' >> $mod_file
echo 'unsigned char __VERIFIER_nondet_uchar() { unsigned char ret; CREST_unsigned_char(ret); return ret; }' >> $mod_file
echo 'unsigned long __VERIFIER_nondet_ulong() { unsigned long ret; CREST_unsigned_long(ret); return ret; }' >> $mod_file
echo 'void *__VERIFIER_nondet_pointer() { }' >> $mod_file
if grep -q "extern void __VERIFIER_assume" $mod_file; then
  echo 'void __VERIFIER_assume(int cond) { if (!(cond)) exit(0); }' >> $mod_file
fi
echo 'void __VERIFIER_error(){ fprintf(stdout, "Reached __VERIFIER_error\n"); exit(1); }' >> $mod_file

# Run crestc

rc_1=$(date +"%s")
timeout 10 ./crest-0.1.2/bin/crestc $mod_file > /dev/null 2>&1

# Run run_crest

timeout 900 ./scripts/run_crest.sh 15 $binary

rc_2=$(date +"%s")
rc_d=$(($rc_2-$rc_1))
#echo -e "\n*** Executing CREST: $(($rc_d / 60)) minutes and $(($rc_d % 60)) seconds elapsed."
#echo -e "\n---------------------------------------------------------------------------------\n"
tr1=$((900-rc_d))

# If error was reached, generate witness

crest_log=$binary.txt
if grep "Reached __VERIFIER_error" $crest_log > /dev/null; then
  ft=${raw_file##*.}
  if [ "$ft" != "c" ]; then
    mv $raw_file $temp.c
    raw_file="$temp.c"
  fi
  gp_1=$(date +"%s")
  timeout $tr1 ./ocaml/pac.native -c 2 -t decisions -f main $raw_file > path.c
  gp_2=$(date +"%s")
  gp_d=$(($gp_2-$gp_1))
  #echo "*** Error Path Generation: $(($gp_d / 60)) minutes and $(($gp_d % 60)) seconds elapsed."
  #echo -e "\n---------------------------------------------------------------------------------\n"
  tr2=$(($tr1-$gp_d))
  #timeout ${tr2} ./scripts/gen_witness.sh path.c
  timeout ${tr2} ./genWitness/genWitness path.c | tee witness.graphml
  echo -e "\n================================================================================="
  echo -e "\n\nVerification Result: FALSE\n\n\n"| tee -a results
  #echo -e "\n\nVerification Result: FALSE\n\n\n"
else
  echo -e "\n================================================================================="
  #threshold is 1036
  threshold=1036
  if grep "Run No. $threshold" $crest_log > /dev/null; then 
    echo -e "\n\nVerification Result: TRUE\n\n\n"| tee -a results
    #echo -e "\n\nVerification Result: TRUE\n\n\n"
  else
    echo -e "\n\nVerification Result: UNKNOWN\n\n\n"| tee -a results
    #echo -e "\n\nVerification Result: UNKNOWN\n\n\n"
  fi
fi
garbage=`find . -maxdepth 1 -not -name "*.sh" -not -name "*.md" -not -name "results" -not -name "*.graphml" -type f`
if [ ! -d "sideProducts_$temp" ]; then
  mkdir sideProducts_$temp
fi
for file in $garbage
do
  mv ${file:2} sideProducts_$temp
done
