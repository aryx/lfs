OCAMLMAKEFILE=OCamlMakefile

SOURCES=Fuse_bindings.idl Fuse_util.c Fuse_lib.ml Fuse.ml
RESULT=Fuse
CLIBS=fuse
OCAMLLDFLAGS=-linkall
THREADS=threads
TRASH=*~ \\\#*
CFLAGS=-D_FILE_OFFSET_BITS=64 -I. -pthread

LIBINSTALL_FILES=Fuse.a Fuse.cma Fuse.cmi Fuse.cmxa Fuse_bindings.cmi Fuse_lib.cmi dllFuse_stubs.so libFuse_stubs.a
OCAML_LIB_INSTALL=$(OCAMLLIBPATH)/site-packages/ocamlfuse

all: native-code-library byte-code-library

install: rawinstall
uninstall: rawuninstall

#pad:
depend::
all.opt:: native-code-library

-include $(OCAMLMAKEFILE)
