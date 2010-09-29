SRCSPEC= lfs.ml plugins.ml demo.ml
SRCCORE= lfs.ml spec.ml 
SRCREALMEM=lfsmem_fuse.ml

#spec:
#	perl -pi -e 's/.*##\s*IMPL//g'    LfsOcaml.mlx
#	perl -pi -e 's/##(.*)##SPEC/$$1/g' LfsOcaml.mlx


coredemo: $(SRCCOREDEMO:.ml=.cmx)
	$(OCAMLOPT) -o $@ $(SYSLIBSCORE:=.cmxa) $(SRCCOREDEMO:.ml=.cmx)
clean:: 
	rm -f coredemo


coredemo.top: $(SRCCOREDEMO:.ml=.cmo)
	$(OCAMLMKTOP) -o $@ $(LIBSCORE:=.cma) $(SRCCOREDEMO:.ml=.cmo) 
clean:: 
	rm -f coredemo.top


lfsmem: $(SRCREALMEM:.ml=.cmx)
	$(OCAMLOPT) -o $@ $(LIBSCORE:=.cmxa) \
	   -ccopt -Locamlfuse Fuse.cmxa -cclib -lfuse -cclib -lcamlidl \
	   $(SRCREALMEM:.ml=.cmx) 
clean:: 
	rm -f lfsmem


specdemo: $(SRCSPEC:.ml=.cmx)
	$(OCAMLOPT) -o $@ $(LIBSSPEC:=.cmxa) $(SRCSPEC:.ml=.cmx)
clean:: 
	rm -f specdemo
