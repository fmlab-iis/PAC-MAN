#!/bin/bash
read -p "Press [Enter] key to install CPAchecker..."
cd CPAchecker-1.4-svn; ant; cd ..; 
read -p "Press [Enter] key to install Yices..."
sudo cp yices-1.0.40/lib/* /usr/lib; 
sudo cp yices-1.0.40/include/* /usr/include; 
sudo cp yices-1.0.40/bin/* /usr/bin; 
sudo chmod 777 /usr/bin/yices;
read -p "Press [Enter] key to install Crest..."
cd crest-0.1.2/cil; ./configure; make; 
cd ../src; make; cd ../..;
cd ocaml; make; cd ..; 
cd genWitness; make; cd ..;
