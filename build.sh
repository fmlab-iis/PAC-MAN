#!/bin/bash
# Install CPAChecker
cd CPAchecker-1.4-svn; ant; cd ..; 
# Install Yices
sudo cp yices-1.0.40/lib/* /usr/lib; 
sudo cp yices-1.0.40/include/* /usr/include; 
sudo cp yices-1.0.40/bin/* /usr/bin; 
sudo chmod 777 /usr/bin/yices;
# Install Crest
cd crest-0.1.2/cil; ./configure; make; 
cd ../src; make; cd ../; ./set_decisions_bound 15 ; cd ../;
# Install pac.native
cd ocaml; make; cd ..;
# Install homemade witness generator
cd genWitness; make; cd ..;
