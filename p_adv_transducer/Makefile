##############################################################################
# Variables
##############################################################################
PLUGINSML= bibtex_adv_transducer naivec_adv_transducer naiveml_adv_transducer
FULLPLUGIN= $(PLUGINSML) 

LIBS=../commons/commons.cma
SYSLIBS=str.cma unix.cma bigarray.cma nums.cma
INCLUDES = -I ../commons

##############################################################################
# Generic variables
##############################################################################
OCAMLC=ocamlc -g $(INCLUDES)
OCAMLOPT=ocamlopt $(INCLUDES)
OCAMLDEP=ocamldep $(INCLUDES)

STATIC=
##############################################################################
# Top rules
##############################################################################
all: $(PLUGINSML)

#note: it does not work if dont have a .depend, cos it even erase the intermediare cmx file
# cos the dependeny is between plugin and plugin.cmx, 
%: $(LIBS:.cma=.cmxa) common_adv_transducer.cmx %.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $+

clean::
	rm -f $(PLUGINSML)

static: 
	rm -f $(PLUGINSML)
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
	rm -f *.cm[iox] *.o

beforedepend::

depend: beforedepend
	$(OCAMLDEP) *.ml *.mli > .depend

-include .depend
