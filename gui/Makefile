##############################################################################
# Variables
##############################################################################
INCLUDES= -I ../commons
LIBS=../commons/commons.cma

SYSLIBS=unix.cma str.cma

##############################################################################
# Generic variables
##############################################################################
#-w s
OCAMLC=ocamlc -g -w s  $(INCLUDES)
OCAMLOPT=ocamlopt  $(INCLUDES)
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLDEP=ocamldep $(INCLUDES)
OCAMLMKTOP=ocamlmktop -custom $(INCLUDES)


##############################################################################
# Top rules
##############################################################################
PROGS=lfs-tunes #lfs-photos 

all: $(PROGS)

lfs-tunes: $(LIBS) lfs-tunes.ml
	$(OCAMLC) -I +lablgtk2 $(SYSLIBS)  lablgtk.cma gtkInit.cmo $^ -o $@

lfs-photos: $(LIBS) lfs-photos.ml
	$(OCAMLC) -I +lablgtk $(SYSLIBS) lablgtk.cma gtkInit.cmo $^ -o $@

#the native version dont work well, pb when launch other process, 
#pb with signals, ...
#seems that threaded version dont work well too
#lfs-tunes: $(LIBS:.cma=.cmxa) lfs-tunes.ml
#	$(OCAMLOPT) -I +lablgtk $(SYSLIBS:.cma=.cmxa) lablgtk.cmxa gtkInit.cmx $^ -o $@

clean::
	rm -f $(PROGS)

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

depend:: beforedepend
	$(OCAMLDEP) *.ml *.mli > .depend

-include .depend
