# PACMAN

Setup: 
./build.sh

Usage: 
./pacman.sh [File]

Note: 
Currently the batch size is 15 and the threshold for UNKNOWN is 1036.
Garbage produced from PACMAN and CREST will be stashed in the sideProducts diresctory.
If an error was reached, the witness will be named witness.graphml in ./output.
To clean everything, use scripts/clear.sh to remove directories output and sideProducts.
