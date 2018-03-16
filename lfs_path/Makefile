##############################################################################
# Variables
##############################################################################
TARGET=lfs_path
LIBNAME=lfs_path

SRC=pathparser.ml pathlexer.ml path.ml 

DEP_LIBS=commons commons_ocollection commons_core

INCLUDES=${DEP_LIBS:%=-I ../external/%} -I ../lfs_core

EXPORTSRC=path.mli

include ../Makefile.common

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
