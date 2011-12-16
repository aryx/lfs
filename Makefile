#############################################################################
# Configuration section
#############################################################################

-include Makefile.config

VERSION=$(shell  cat globals/config_lfs.ml |grep version | perl -p -e 's/.*"(.*)".*/$$1/;')

##############################################################################
# Variables
##############################################################################

BATTERY_INCLUDED=ocamlagrep ocamlfuse ocamlbdb  logfun
PLUGINSSUBDIRS=p_logic p_transducer p_adv_transducer
EXTRADIRS=gui

MAKESUBDIRS=$(BATTERY_INCLUDED) commons globals  lfs_core lfs_path lfs_real
INCLUDEDIRS=$(BATTERY_INCLUDED) commons globals  lfs_core lfs_path lfs_real
ALLSUBDIRS=$(MAKESUBDIRS) $(PLUGINSSUBDIRS) $(EXTRADIRS)

INCLUDES=$(INCLUDEDIRS:%=-I %) -I commons/ocamlextra



# Basic libs.
SYSLIBS1=str unix bigarray nums   threads
# Normally require dbm only for semi_real mode, so could remove this dependency
SYSLIBS2=agrep bdb dbm
# With fuse
SYSLIBS3=Fuse

SYSLIBS=$(SYSLIBS1) $(SYSLIBS2) $(SYSLIBS3)

ifeq ($(FEATURE_BACKTRACE), 1)
BTCMD= $(MAKE) backtrace -C commons
BTCMDOPT= $(MAKE) backtrace.opt -C commons
BTCMA=commons/commons_backtrace.cma
else
endif


LIBS1=commons/commons.cma globals/globals.cma \
      commons/commons_bdb.cma commons/commons_gdbm.cma \
      commons/commons_features.cma \
      lfs_core/lfs_core.cma lfs_path/lfs_path.cma
LIBS2=lfs_real/lfs_real.cma
# no fuse dependency
LIBS3=lfs_real/lfs_real1.cma

LIBS=$(LIBS1) $(LIBS2)



SRC= mount.ml


PLUGINSML= p_logic/int_logic.ml p_logic/string_logic.ml p_logic/date_logic.ml \
	   p_logic/prop_logic.ml \
           p_transducer/fakemp3_transducer.ml \
	   p_adv_transducer/naivec_adv_transducer.ml \
	   p_adv_transducer/bibtex_adv_transducer.ml \
	   p_adv_transducer/naiveml_adv_transducer.ml
#could put type_logic if use parser combinator instead of yacc.

##############################################################################
# Generic variables
##############################################################################
OCAMLCFLAGS=-g -dtypes # -w A

# for profiling add  -p -inline 0
OPTFLAGS=

OCAMLC=ocamlc $(OCAMLCFLAGS)  $(INCLUDES) -thread
OCAMLOPT=ocamlopt $(OPTFLAGS) $(INCLUDES) -thread
OCAMLLEX=ocamllex #-ml # -ml for debugging lexer, but slightly slower
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep $(INCLUDES)
OCAMLMKTOP=ocamlmktop -g -custom -thread $(INCLUDES)

# can also be set via 'make static'
STATIC= #-ccopt -static

##############################################################################
# Top rules
##############################################################################
PROGS=mount.lfs mount-byte.lfs plugins coredemo build_db build_db.opt

all: rec rec.opt $(PROGS)
all.opt: rec.opt $(PROGS)
opt: all.opt
top: mount.top

rec:
	$(MAKE) -C commons all
	$(MAKE) -C ocamlbdb
	$(MAKE) -C commons bdb
	$(MAKE) -C commons gdbm
	$(MAKE) features -C commons
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all; done
rec.opt:
	$(MAKE) -C commons all.opt
	$(MAKE) -C ocamlbdb opt
	$(MAKE) -C commons bdb.opt
	$(MAKE) -C commons gdbm.opt
	$(MAKE) features.opt -C commons
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt; done

clean::
	set -e; for i in $(ALLSUBDIRS); do $(MAKE) -C $$i clean; done
depend::
	set -e; for i in $(ALLSUBDIRS); do $(MAKE) -C $$i depend; done




mount.lfs: $(LIBS:.cma=.cmxa) $(SRC:.ml=.cmx)
	$(OCAMLOPT) $(STATIC) -o $@  $(SYSLIBS:=.cmxa) \
	   -ccopt -Locamlfuse -cclib -lfuse -cclib -lcamlidl \
	   $^
mount-byte.lfs: $(LIBS) $(SRC:.ml=.cmo)
	$(OCAMLC) -o $@ $(SYSLIBS:=.cma) \
	   -ccopt -Locamlfuse -cclib -lfuse -cclib -lcamlidl \
	   $^

mount.top: $(LIBS) $(SRC:.ml=.cmo)
	$(OCAMLMKTOP) -o $@ $(SYSLIBS:=.cma) \
	   -ccopt -Locamlfuse -cclib -lfuse -cclib -lcamlidl \
	   $^

clean::
	rm -f mount.lfs mount-byte.lfs mount.top




.PHONY: plugins
plugins:
	cd p_logic; $(MAKE)
	cd p_transducer; $(MAKE)
	cd p_adv_transducer; $(MAKE)


plugins.ml: $(PLUGINSML)
	echo "(* automatically generated from plugins source *)" > plugins.ml
	echo "(* useful only to construct core LFS *)" >> plugins.ml
	echo "(* useful only for test/devel purpose *)" >> plugins.ml
	echo "open Common" >> plugins.ml
	echo "open Lfs" >> plugins.ml
	echo "open Parser_combinators.Infix" >> plugins.ml
	cat $(PLUGINSML) | grep -v "open" | grep -v "interact_" >> plugins.ml
beforedepend:: plugins.ml
clean::
	rm -f plugins.ml




# note that this does not need lfs_real.cma
LIBSCORE=commons/commons.cma globals/globals.cma lfs_core/lfs_core.cma lfs_path/lfs_path.cma
coredemo: plugins.ml demo.ml
	$(MAKE) -C commons
	$(MAKE) -C globals
	$(MAKE) -C lfs_core
	$(MAKE) -C lfs_path
	$(OCAMLC) -custom $(SYSLIBS1:=.cma) -o $@ $(LIBSCORE)  $^

coredemo.top: commons/commons.cma globals/globals.cma lfs_core/lfs_core.cma lfs_path/lfs_path.cma plugins.ml demo.ml
	$(OCAMLMKTOP) -custom str.cma unix.cma -o $@ $^


clean::
	rm -f coredemo coredemo.top





SRCBUILDDB=build_db.ml

build_db: $(LIBS1) $(LIBS3) commons/commons_gdbm.cma $(SRCBUILDDB:.ml=.cmo)
	$(MAKE) -C commons gdbm
	$(MAKE) nofuse
	$(OCAMLC) -o $@ $(SYSLIBS1:=.cma) $(SYSLIBS2:=.cma) $^

build_db.opt: $(LIBS1:.cma=.cmxa) $(LIBS3:.cma=.cmxa) commons/commons_gdbm.cmxa $(SRCBUILDDB:.ml=.cmx)
	$(MAKE) -C commons gdbm.opt
	$(MAKE) nofuse.opt
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS1:=.cmxa) $(SYSLIBS2:=.cmxa) \
	   $^

clean::
	rm -f build_db build_db.opt




# google says that need -lrt if get an error with "clock_gettime undefined"
static:
	rm -f mount.lfs build_db.opt
	make -C p_logic static
	make -C p_transducer static
	make -C p_adv_transducer static
	$(MAKE) STATIC="-ccopt -static" build_db.opt
	$(MAKE) STATIC="-ccopt -static -cclib -lrt" mount.lfs

##############################################################################
# Other cut down LFS targets
##############################################################################

# Here is a cut down version of LFS, to be used by other programs
# - no ocamlfuse, no ocamlagrep, no logfun
# - a limited lfs_real
nofuse:
	$(MAKE) -C commons
	$(MAKE) -C globals
	$(MAKE) -C lfs_core
	$(MAKE) -C lfs_path
	$(MAKE) -C ocamlbdb
	$(MAKE) bdb -C commons
	$(MAKE) lfs_real1.cma -C lfs_real

nofuse.opt:
	$(MAKE) opt -C commons
	$(MAKE) opt -C globals
	$(MAKE) opt -C lfs_core
	$(MAKE) opt -C lfs_path
	$(MAKE) opt -C ocamlbdb
	$(MAKE) bdb.opt -C commons
	$(MAKE) lfs_real1.cmxa -C lfs_real

##############################################################################
# Install
##############################################################################

# DESTDIR can be set by package build system like ebuild
install:: all all.opt
	mkdir -p $(DESTDIR)$(BINDIR)
	mkdir -p $(DESTDIR)$(SHAREDIR)
	cp mount.lfs $(DESTDIR)$(BINDIR)
	cp build_db.opt $(DESTDIR)$(BINDIR)
	rm -rf $(DESTDIR)$(SHAREDIR)/p_logic $(DESTDIR)$(SHAREDIR)/p_transducer $(DESTDIR)$(SHAREDIR)/p_adv_transducer
	cp -a p_logic p_transducer p_adv_transducer  $(DESTDIR)$(SHAREDIR)
	rm -rf $(DESTDIR)$(SHAREDIR)/data
	cp -a data $(DESTDIR)$(SHAREDIR)
	@echo "you can also install LFS by copying the program 'mount.lfs'"
	@echo "(available in this directory) anywhere you want"

#	cp scripts/*.lfs $(BINDIR)
#	cp p_transducer/index_LFS_with_glimpse $(BINDIR)

#uninstall:
#	rm -f $(BINDIR)/*.lfs


version:
	@echo $(VERSION)


##############################################################################
# Package rules, pad's specific
##############################################################################

PACKAGE=lfs-$(VERSION)

BINSRC=env.sh env.csh *.txt mount.lfs build_db.opt data/*.sh scripts/*.lfs   p_*
BINSRC2=$(BINSRC:%=$(PACKAGE)/%)

TXT=$(wildcard *.txt)

TOPLFS=/home/pad/mobile/project-lfs
WEBSITE=/home/pad/mobile/homepage/software/project-lfs



package: srctar

srctar:
	make clean
	rm -f $(TOPLFS)/$(PACKAGE)
	ln -s $(TOPLFS)/code $(TOPLFS)/$(PACKAGE)
	cd $(TOPLFS); tar cvfz $(PACKAGE).tgz $(PACKAGE)/*

bintar: all all.opt
	rm -f $(TOPLFS)/$(PACKAGE)
	ln -s $(TOPLFS)/code $(TOPLFS)/$(PACKAGE)
	cd $(TOPLFS); tar cvfz $(PACKAGE)-bin.tgz $(BINSRC2)
	make static
	cd $(TOPLFS); tar cvfz $(PACKAGE)-bin-static.tgz $(BINSRC2)

clean::
	rm -f $(PACKAGE) $(PACKAGE)-bin.tgz $(PACKAGE)-bin-static.tgz





website:
	cp $(TOPLFS)/$(PACKAGE).tgz  $(WEBSITE)
	cp $(TOPLFS)/$(PACKAGE)-bin.tgz        $(WEBSITE)
	cp $(TOPLFS)/$(PACKAGE)-bin-static.tgz $(WEBSITE)

syncwiki:
	unison ~/public_html/wiki/wiki-LFS/data/pages/ docs/wiki/
#	set -e; for i in $(TXT); do unison $$i docs/wiki/$$i; done

hgweb:
	@echo pull from ~/public_html/hg/c-lfs and c-commons




package2:
	cd "original/.ca/!todo/!RCS/!data/!thesis/!env/!cited/!history/!old/!backup/!figs/!html/!me/!paperasse/!perlfs/!publications/!website/!name:history/"; \
	tar cvfz ~/$(PACKAGE).tgz code/

packagesmall:
	cd "original/.ca/((!docs)|name:user-manual)/!doc/!todo/!obsolete/!RCS/!ext:ps/!name:man/!data/!thesis/!env/!cited/!history/!old/!backup/!figs/!html/!me/!paperasse/!perlfs/!publications/!website/!name:history"; \
	tar cvfz ~/LFS-nodocs-$(VERSION).tgz code/



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
	rm -f *.cm[iox] *.o *.annot

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml > .depend

-include .depend

##############################################################################
# Developer rules
##############################################################################

META=/home/pad/meta-lfs
CURRENT=home
DEST=/home/pad/lfs

MOUNTOPTIONS=-semi_real_mode -nofsck -nostat -verbose_level 3 -profile
#MOUNTOPTIONS=-timeout 1200 -nostat -nofsck -command -notransact
MOUNTPROG=./mount.lfs
#MOUNTPROG=./mount-byte.lfs

.PHONY: mount umount mkfs

mount:
	rm -f /tmp/debugml500:*
	rm -f /tmp/xdebugml500:*
	rm -f /tmp/debugml1000:*
	rm -f /tmp/xdebugml1000:*
	$(MOUNTPROG) $(MOUNTOPTIONS) $(META)/$(CURRENT) $(DEST)

umount:
	fusermount -u $(DEST)

mkfs:
	./scripts/mkfs.lfs $(META)/$(CURRENT)


test:
	./mount.lfs -nofsck -verbose_level 2 -command_lfs $(META)



#todo: define the following targets
#  check, bench, package that suppr debugging stuff automatically ?
#  fsck, stat,

#for debug:  make debug (debug realfs) or make command,   can also
# put a -inline 0 with OCAMLC when want good backtrace of a coredump, ...
# for the moment the make command is done via modifying common_fuse.ml
# at a certain place, C-r command from eof or simply by execution
# mount.lfs -command (where command content is specified in lfsbdb_fuse

#for profiling=  make prof (prof corefs)

#------------------------------------------------------------------------------
# boostrapping LFS
#------------------------------------------------------------------------------

#if ininitialise a new context from a code.tgz
# (to make code.tgz just make clean; tar cvfz /tmp/code.tgz code/ from
#  lfs-src dir :)  )
# detar code.tgz
# cd code
# make
# edit Makefile, put CURRENT=lfs-src
# make mkfs
# make mount
# cd $mount   sh -x basicenv.sh
# mkdir forpof
# cd .compat/.strict/original
# tar xvfz code.tgz  (the same)
# cd /
# make umount

#you can now erase this code directory
# bootlfs.lfs

#------------------------------------------------------------------------------
# old
#------------------------------------------------------------------------------

#BINSRC2=$(BINSRC:%=$(PACKAGE)/%)
#bintar: all all.opt
#	rm -f $(PACKAGE)
#	ln -s 'lfs_view.query/!.svn/!.hg/' $(PACKAGE)
#	tar cvfz $(PACKAGE)-bin.tgz $(BINSRC2)
#	make static
#	tar cvfz $(PACKAGE)-bin-static.tgz $(BINSRC2)
#	rm -f $(PACKAGE)

##############################################################################
# Pad rules
##############################################################################

# need -fuse_allow_other because I want to access it from my website and so
# apache must be able to access my fuse fs.
home: all.opt
	fusermount -u ~/lfs || echo was not mounted
	./build_db.opt -build_db_pad_home ~/meta-lfs/home
	./mount.lfs -nofsck -semi_real_mode -fuse_allow_other ~/meta-lfs/home ~/lfs

home2:
	./mount.lfs -nofsck -semi_real_mode -fuse_allow_other ~/meta-lfs/home ~/lfs

LINUXSRC=~/linux/
linux: all.opt
	fusermount -u ~/lfs3 || echo was not mounted
	./build_db.opt -use_c_transducer -lfs_metapath ~/meta-lfs/linux $(LINUXSRC)
	./mount.lfs -nofsck -semi_real_mode -fuse_allow_other ~/meta-lfs/linux ~/lfs3


wc:
	wc -l *.ml* commons/*.ml* globals/*.ml* gui/*.ml* lfs_core/*.ml* lfs_path/*.ml* lfs_real/*.ml* p_adv_transducer/* p_logic/* p_transducer/* scripts/*