##############################################################################
# Variables
##############################################################################
TARGET=lfs_path

SRC=pathparser.ml pathlexer.ml path.ml

LIBS=../commons/commons.cma ../lfs_core/lfs_core.cma
INCLUDES=-I ../commons -I ../lfs_core

SYSLIBS=bigarray

##############################################################################
# Generic variables
##############################################################################
OCAMLC=ocamlc$(OPTBIN) -g -dtypes   $(INCLUDES)
OCAMLOPT=ocamlopt$(OPTBIN)   $(INCLUDES) $(OPTFLAGS)
#OCAMLLEX=ocamllex$(OPTBIN) -ml
OCAMLLEX=ocamllex$(OPTBIN)
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep$(OPTBIN)  $(INCLUDES)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES)

##############################################################################
# Top rules
##############################################################################
OBJS= $(SRC:.ml=.cmo)
OPTOBJS= $(SRC:.ml=.cmx)


all: $(TARGET).cma
all.opt: $(TARGET).cmxa
opt: all.opt


$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)


pathlexer.ml: pathlexer.mll
	$(OCAMLLEX) pathlexer.mll
clean::
	rm -f pathlexer.ml
beforedepend:: pathlexer.ml


pathparser.ml pathparser.mli: pathparser.mly
	$(OCAMLYACC) pathparser.mly
clean::
	rm -f pathparser.ml pathparser.mli pathparser.output
beforedepend:: pathparser.ml pathparser.mli



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

.ml.mldepend:
	$(OCAMLC) -i $<

clean::
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot

clean::
	rm -f *~ .*~ gmon.out #*#

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml    > .depend

-include .depend
