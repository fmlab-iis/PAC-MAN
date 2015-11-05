OCAMLMAKEFILE = OCamlMakefile
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLFIND = ocamlfind
OCAMLLIBPATH =
NCSUFFIX = .native
BCSUFFIX = .byte
PP=camlp4o pa_macro.cmo
export PP
OCAML_VERSION = $(shell ocamlc -version)
OCAML_MAJOR = $(shell ocamlc -version | cut -c1)
PPFLAGS =
ifeq ($(OCAML_MAJOR), 4)
	PPFLAGS = -DOCAML4
endif

SOURCES = ia.ml
CILDIR = 
INCDIRS = 
LIBDIRS = 
MODULES = 
LIBS =
PACKS = cil str
OCAMLFLAGS = -g -annot
LDFLAGS = -fexceptions
#CLIBS = stdc++ 
RESULT = ia

all: nc

-include $(OCAMLMAKEFILE)
