#!/bin/bash
cd CPAchecker-1.4-svn; ant; cd ..; cd crest-0.1.2/cil; ./configure; make; cd ../src; make; cd ../..; cd ocaml; make; cd ..; cd genWitness; make; cd ..;
