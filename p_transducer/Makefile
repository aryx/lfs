##############################################################################
# Variables
##############################################################################
PLUGINSML = fakemp3_transducer #c_transducer4

LIBS=../commons/commons.cma
SYSLIBS=str.cma unix.cma bigarray.cma nums.cma
INCLUDES = -I ../commons

##############################################################################
# Generic variables
##############################################################################
OCAMLC=ocamlc -g $(INCLUDES)
OCAMLOPT=ocamlopt $(INCLUDES)
OCAMLDEP=ocamldep $(INCLUDES)

#-ccopt -static
STATIC=
##############################################################################
# Top rules
##############################################################################
all: $(PLUGINSML) index_LFS_with_glimpse

%: $(LIBS:.cma=.cmxa) common_transducer.cmx %.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $+

clean::
	rm -f $(PLUGINSML) index_LFS_with_glimpse

static:
	rm -f $(PLUGINSML) index_LFS_with_glimpse
	$(MAKE) STATIC="-ccopt -static" all


##############################################################################
# Generic rules
##############################################################################
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<
.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<


clean::
	rm -f *.cm[iox] *.o


beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml > .depend

-include .depend