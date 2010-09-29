##############################################################################
# Variables
##############################################################################
PLUGINSML= int_logic string_logic date_logic    \
           prop_logic  type_logic    \
	   size_logic duree_logic

FULLPLUGIN=$(PLUGINSML) date_logic_via_logfun

INCLUDES= -I ../commons -I ../commons/ocamlextra
LIBS=../commons/commons.cma 

SYSLIBS=str unix bigarray nums

##############################################################################
# Generic variables
##############################################################################
OCAMLCFLAGS=-g -dtypes # -w A

# no threads!!,  ocaml threads sux when have pipe, strange behaviour
OCAMLC=ocamlc $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT=ocamlopt $(INCLUDES)
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLDEP=ocamldep $(INCLUDES)
OCAMLMKTOP=ocamlmktop -custom $(INCLUDES)

#-ccopt -static
STATIC= 

##############################################################################
# Top rules
##############################################################################
all: $(FULLPLUGIN) 

%: $(LIBS:.cma=.cmxa) common_logic.cmx %.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:=.cmxa) $+

clean::
	rm -f $(FULLPLUGIN)

top: p_logic.top
p_logic.top: ../commons/commons.cma common_logic.cmo prop_logic.cmo
	$(OCAMLMKTOP) -custom $(SYSLIBS:=.cma) -o $@ $^

clean::
	rm -f p_logic.top

#.SUFFIXES: .byte 
#%.byte: $(LIBS:.cma) common_logic.cma %.cma
#	$(OCAMLC) -o $@ str.cma unix.cma $+



#old: when not using parser combinators 
#prop_logic: $(LIBS:.cma=.cmxa) common_logic.cmx proptype.cmx propparser.cmx proplexer.cmx prop_logic.cmx 
#	$(OCAMLOPT) $(STATIC) -o $@ str.cmxa unix.cmxa $+
#
#proplexer.ml: proplexer.mll
#	$(OCAMLLEX) proplexer.mll
#
#propparser.ml propparser.mli: propparser.mly
#	$(OCAMLYACC) propparser.mly
#
#beforedepend:: proplexer.ml propparser.ml propparser.mli
clean::
	rm -f proplexer.ml 
	rm -f propparser.ml propparser.mli 


type_logic: $(LIBS:.cma=.cmxa) common_logic.cmx typetype.cmx typeparser.cmx typelexer.cmx type_logic.cmx 
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:=.cmxa) $+

typelexer.ml: typelexer.mll
	$(OCAMLLEX) typelexer.mll

typeparser.ml typeparser.mli: typeparser.mly
	$(OCAMLYACC) typeparser.mly

beforedepend:: typelexer.ml typeparser.ml typeparser.mli
clean::
	rm -f typelexer.ml 
	rm -f typeparser.ml typeparser.mli 




prop_logic2: prop_logic2.pm
	pmc prop_logic2.pm -o prop_logic2
clean:: 
	rm -f  *.i *.tmp *.c


date_logic_via_logfun: date_logic_via_logfun.ml
	$(OCAMLOPT) $(STATIC) -I ../logfun/src $(SYSLIBS:=.cmxa) ../logfun/lib/logfun.cmxa  \
	  date_logic_via_logfun.ml -o date_logic_via_logfun



static:
	rm -f $(FULLPLUGIN)
	$(MAKE) STATIC="-ccopt -static" all


##############################################################################
# Generic rules
##############################################################################
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmo:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<

clean::
	rm -f *.cm[iox] *.o *.annot

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.ml *.mli > .depend


-include .depend


