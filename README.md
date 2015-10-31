# PACMAN
PAC-MAN: Probably Approximately Correct Model ANalyzer
## Setup (only for Linux)
1. Before installing PAC-MAN, please make sure the following packages are installed (with apt-get install):    
	`ant`  
	`m4`  
	`autoconf`   
	`g++`  

2. Setup OCaml 4.01.0:  
    Note: This version of OCaml cannot be replaced with the newer versions. CREST, which is an important component of PAC-MAN, needs this specific version of OCaml.  
	`wget http://caml.inria.fr/pub/distrib/ocaml-4.01/ocaml-4.01.0.tar.gz;`  
	`tar xzf ocaml-4.01.0.tar.gz;`  
	`cd ocaml-4.01.0; ./configure; make world.opt; sudo make install; cd ..;`  

3. Setup FindLib 1.5.6: (if FindLib is installed, please skip this part)  
	`wget http://download.camlcity.org/download/findlib-1.5.6.tar.gz;`  
  `tar xzf findlib-1.5.6.tar.gz`  
	`cd findlib-1.5.6; ./configure; make all; make opt; sudo make install; cd ..;`  

4. Install OPAM and dependency packages: (if OPAM is installed, please skip the first two commands)  
	`sudo apt-get install opam;`  
	`opam init https://opam.ocaml.org/1.1;`  
	`opam install cil; `  
	`opam install xstr;`  

5. Build PAC-MAN:  
	`./build.sh`

## Usage 
./pacman.sh [File]

## Note:  
Error rate = 0.004
Confidence level = 0.98427195992
