VPATH = src:examples

## bytecode compilation
#CMO=cmo
#CMA=cma
#OCAMLC=ocamlc

# native code compilation
CMO=cmx
CMA=cmxa
OCAMLC=ocamlopt

OCAMLLEX=ocamllex
CAMLLIB=$(shell ocamlc -where)
INCLUDES= -I src                 # all relevant -I options here
OCAMLFLAGS= -pp "camlp4o pa_op.cmo" $(INCLUDES)    # add other options for ocamlc here
OCAMLDOCFLAGS= $(OCAMLFLAGS) -d ocamldoc



# The list of object files
COMMON=common.$(CMO) token.$(CMO) src/lexer.$(CMO) syntax.$(CMO) lSet.$(CMO) cis.$(CMO) term.$(CMO) logic.$(CMO)
OLD = pair.$(CMO) opt.$(CMO) conj.$(CMO) interv.$(CMO)
ATOM = unit.$(CMO) card.$(CMO) int.$(CMO) intpow.$(CMO) float.$(CMO) floatpow.$(CMO) time.$(CMO) date.$(CMO) charclass.$(CMO) nt.$(CMO) aa.$(CMO) abcn.$(CMO) vorto.$(CMO) substring.$(CMO) sentence.$(CMO) atom.$(CMO) attr.$(CMO) taxo.$(CMO)
ALGEBRA = top.$(CMO) prod.$(CMO) sum.$(CMO)
STRUCTURE = interval.$(CMO) multiset.$(CMO)
LOGIC = iset.$(CMO) bottom.$(CMO) single.$(CMO) id.$(CMO) aik.$(CMO) prop.$(CMO) prop2.$(CMO) enum.$(CMO)
SYNTAX = prefix.$(CMO) suffix.$(CMO) bracket.$(CMO)
GEN2 = option.$(CMO) vector.$(CMO)  openinterval.$(CMO)
GEN3 = prelist.$(CMO) mltype.$(CMO) javatype.$(CMO) date_pattern.$(CMO)
GEN4 = segment.$(CMO) motif.$(CMO)
ALL = $(COMMON) $(OLD) $(ATOM) $(ALGEBRA) $(STRUCTURE) $(LOGIC) $(SYNTAX) $(GEN2) $(GEN3) $(GEN4)


all: lib/logfun.$(CMA)
	@echo

lib/logfun.$(CMA): $(ALL)
	@echo "Compiling the Logfun library 'logfun.$(CMA)' ..."
	$(OCAMLC) -a $(OCAMLFLAGS) -o $@ $^

#src/make_sum: make_sum.$(CMO)
#	$(OCAMLC) -I src -o $@ $^

examples: bibtex.exe bioinfo.exe ocamltypes.exe
	@echo "Compiling small examples for verification ..."

html:
	@echo "Generating documentation with ocamldoc in subdirectory 'ocamldoc' ..."
	ocamldoc $(OCAMLDOCFLAGS) -html src/*.ml

install: lib/logfun.$(CMA)
	@echo "Installing Logfun in the Caml library directory $(CAMLLIB) ..."
	-mkdir $(CAMLLIB)/logfun
	cp -f src/*.cmi $(CAMLLIB)/logfun
	cp -f lib/logfun.$(CMA) $(CAMLLIB)/logfun
	-cp -f lib/logfun.a $(CAMLLIB)/logfun

uninstall:
	@echo "Uninstalling Logfun from the Caml library directory $(CAMLLIB) ..."
	-rm -f $(CAMLLIB)/logfun/*
	-rmdir $(CAMLLIB)/logfun

clean:
	@echo "Cleaning ..."
	-rm -f src/lexer.ml
	-rm -f src/make_sum
	-rm -f src/sum?*.ml
	-rm -f */*.cm[ioax]
	-rm -f */*.cmxa
	-rm -f */*.[oa]
	-rm -f */*~
	-rm -f *~

cleanall: clean
	@echo "Cleaning all..."
	-rm -f ocamldoc/*
	$(MAKE) -C examples clean

# Common rules
.SUFFIXES: .ml .$(CMO)

%.ml: %.mll
	$(OCAMLLEX) $^

%.$(CMO): %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $^

#src/sum%.ml: src/make_sum
#	./src/make_sum $*
#	mv sum$*.ml src/

%.exe:
	$(MAKE) -C examples $@

#pad:
depend::
all.opt::