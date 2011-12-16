##############################################################################
# Variables
##############################################################################
TARGET=lfs_real

SRC= ioplugins.ml \
     lfs_real.ml lfs_persistent.ml lfs_fuse.ml \
     lfs_extensions.ml \
     lfs_bdb.ml lfs_gdbm.ml \
     lfs_semireal.ml

#Todo ? normally require dbm only for semi_real mode

LIBS=../commons/commons.cma ../globals/globals.cma \
     ../ocamlbdb/bdb.cma ../ocamlfuse/Fuse.cma ../ocamlagrep/agrep.cma \
     ../lfs_core/lfs_core.cma ../lfs_path/lfs_path.cma
INCLUDEDIRS=../commons ../commons/ocamlextra ../commons/ocamlextra ../commons/ocollection \
         ../globals \
	 ../ocamlbdb ../ocamlfuse ../ocamlagrep \
	 ../lfs_core ../lfs_path

INCLUDES=$(INCLUDEDIRS:%=-I %)

##############################################################################
# Generic variables
##############################################################################

OCAMLC=ocamlc$(OPTBIN) -g -dtypes   $(INCLUDES)  -thread
OCAMLOPT=ocamlopt$(OPTBIN)   $(INCLUDES) $(OPTFLAGS) -thread
#OCAMLLEX=ocamllex$(OPTBIN) -ml
OCAMLLEX=ocamllex$(OPTBIN)
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep$(OPTBIN)  $(INCLUDES)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES)

OBJS= $(SRC:.ml=.cmo)
OPTOBJS= $(SRC:.ml=.cmx)


##############################################################################
# Top rules
##############################################################################
all: $(TARGET).cma lfs_real1.cma

all.opt: $(TARGET).cmxa lfs_real1.cmxa


$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)


##############################################################################
# Other targets
##############################################################################

MINISRC=ioplugins.ml lfs_real.ml lfs_persistent.ml lfs_bdb.ml lfs_semireal.ml
# less dependencies, for a "core++ LFS"
lfs_real1.cma: $(MINISRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

lfs_real1.cmxa: $(MINISRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^

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
	rm -f *~ .*~ gmon.out #*#

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml    > .depend

-include .depend
