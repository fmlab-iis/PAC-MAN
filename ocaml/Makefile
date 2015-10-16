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

BASE_SOURCES = \
	hash.ml inliner.ml badT.ml badUtil.ml badNFA.ml badPDA.ml bad.ml maybe.ml pac.ml
SOURCES = $(BASE_SOURCES) main.ml
LIBPAC_SOURCES = $(BASE_SOURCES) pac_stubs.ml
CILDIR = 
INCDIRS = 
LIBDIRS = 
MODULES = 
LIBS =
PACKS = cil str
OCAMLFLAGS = -g -annot
LDFLAGS = -fexceptions
#CLIBS = stdc++ 
RESULT = pac
COMMA = ,
LIBPAC = libpac.o

all: nc $(LIBPAC)

$(LIBPAC): $(LIBPAC_SOURCES)
	$(OCAMLFIND) $(OCAMLOPT) -package $(subst $(eval) $(eval),$(COMMA),$(PACKS)) \
	-linkpkg -output-obj $^ -o $@

clean::
	rm -f $(LIBPAC) pac_stubs.o

-include $(OCAMLMAKEFILE)
