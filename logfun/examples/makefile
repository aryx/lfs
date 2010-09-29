## bytecode compilation
#CMO=cmo
#CMA=cma
#OCAMLC=ocamlc

# native code compilation
CMO=cmx
CMA=cmxa
OCAMLC=ocamlopt

INCLUDES= -I +logfun   # all relevant -I options here
OCAMLFLAGS= -pp "camlp4o pa_op.cmo" $(INCLUDES)  # add other options for ocamlc here
# for Windows, add the option: -dllpath "C:\Program Files\Objective Caml\lib"

# The list of library and object files
OBJS = unix.$(CMA) nums.$(CMA) str.$(CMA) logfun.$(CMA)

all: bibtex.exe bioinfo.exe
	@echo

# Common rules
.SUFFIXES: .ml .$(CMO) .exe

%.exe: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(OBJS) $<

# Clean up
clean:
	-rm -f *.cm[iox]
	-rm -f *.o
	-rm -f *.exe
	-rm -f *~
