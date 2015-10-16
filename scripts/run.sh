#!/bin/bash

for file in benchmarks/recursive-simple/*.c; 
do 
  ./pacman.sh $file 
done
for file in benchmarks/recursive/*.c; 
do 
  ./pacman.sh $file 
done
