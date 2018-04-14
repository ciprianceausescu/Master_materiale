#
# Copyright (C) 1988-1998 Swedish Institute of Computer Science.
#
#
# NOTE: This Makefile requires GNU make.
#
# NOTE2: If you get this make file as part of a binary installation
# then it may serve as an example but it is unlikely to work "as is". 
#

# [PM] 3.9 Experiment with making more dependencies explicit to
# decrease the number of "make[4]: `foo' is up to date"
NEW_MAKE_RULES:=yes

SRCDIR=.

.PHONY: default
default: all

# # [PM] 3.11.2 WinCE a test
# SP_SYSTEM_PO := sp_system.po

# [PM] 3.10.2 new target
.PHONY: library_examples
library_examples: jasper_examples prologbeans_examples

# Include common Makefile parts.
include $(SRCDIR)/../Common

# [PM] WinCE compensate for invalid clock on WinCE device.
ifeq ($(WINCE),yes)

# [PM] 3.11.2 sometimes on WinCE the file does not get created (or
# gets created empty?) but there is no error code.
#  -s FILE     FILE exists and has a size greater than zero
TOUCH_SP_TARGET = { [ -s '$@' ] && touch '$@'; } || { echo '$@ was not created' 1>&2; exit 1; }
endif				# WINCE

VERIFY_TARGET_CREATED = @ [ -f $@ ] || { echo "ERROR: Target $@ was not created" 1>&2; exit 1; }

.SUFFIXES:
.SUFFIXES: .ql .pl .po .c .$(OBJEXT) $(EXEEXT) .$(FLI_SHSFX) .$(STSFX)


# [PM] stuff that is not used from prolog but is still part of
## library, e.g., vbsp.dll (but vbsp.dll is windows specific so added below)
OTHER=

SOCKETS_MODULE = sockets.po

# [MC] removed math.po
MODULES1= \
	aggregate.po \
	arrays3.po \
	assoc.po \
	assoc3.po \
	atts.po \
	avl.po \
	bags.po \
	between.po \
	codesio.po \
	detcheck.po \
	determinacy.po \
	det.po \
	fastrw.po \
	file_systems.po \
	heaps.po \
	lists.po \
	lists3.po \
	logarr.po \
	mkindex.po \
	nondetdecl.po \
	obj_decl.po \
	objects.po \
	ordsets.po \
	process.po \
	queues.po \
	queues3.po \
	random.po \
	random3.po \
	rem.po \
	resgen.po \
	samsort.po \
	sets.po \
	str_decl.po \
	structs.po \
	SU_messages.po \
	system.po \
	system3.po \
        $(SP_SYSTEM_PO) \
	terms.po \
	trees.po \
	types.po \
	ugraphs.po \
	varnumbers.po \
	wgraphs.po \
	xml.po \
	xref.po \
	$(SOCKETS_MODULE) \
	$(BDB_MODULE) \
	$(TIMEOUT_MODULE) \
        $(COMCLIENT_MODULE) \
        $(TCLTK_MODULE)

ifneq ($(SOCKETS_MODULE),)
MODULES1 += \
	linda/client.po \
	linda/server.po \
	pillow.po \
	prologbeans.po \
	prologbeansserver.po \

endif				# SOCKETS_MODULE



# Modules only supported under Win32
ifeq ($(ENABLE_VBSP),yes)

# [PM] 3.10.0b2 SPRM 4227
NO_VBSP_PL=yes

ifneq ($(NO_VBSP_PL),yes)
MODULES1 += vbsp/vbsp.po
endif				# !NO_VBSP_PL

OTHER +=vbsp
endif				# ENABLE_VBSP

# [PD] 3.11.3
ifeq ($(ENABLE_DOTNET),yes)
OTHER +=prologbeansdotnet
endif

ifeq ($(WIN32),yes)
COMCLIENT_MODULE = comclient.po
endif				# WIN32

# [PM] 3.9 it is now possible to turn off clpfd
ifeq ($(ENABLE_CLPFD),yes)
	CLPFD_MODULE=clpfd.po
ifneq ($(SOCKETS_MODULE),)
	MODULES1 += fdbg.po
endif				# SOCKETS_MODULE
endif


ifeq ($(TCLTK_MODULE),)
MODULES2=
else
MODULES2= gauge.po
endif

MODULES3= \
	clpq.po \
	clpr.po \
	$(CLPFD_MODULE) \

MODULES4= \
	chr.po \

# Modules requiring -m (i.e, those that load Java/jasper)
JAVA_MODULES= \
	$(JASPER_MODULE)

# Dependencies between .po files
aggregate.po:		types.po
arrays3.po:		logarr.po
assoc.po:		
assoc3.po:		avl.po
atts.po:		types.po
avl.po:			
bags.po:		
bdb.po:			types.po fastrw.po
between.po:		types.po
chr.po:			types.po lists.po atts.po avl.po ordsets.po terms.po 
clpfd.po:		types.po lists.po atts.po avl.po ordsets.po ugraphs.po between.po
clpq.po clpr.po: 	types.po ordsets.po atts.po terms.po assoc3.po ugraphs.po
codesio.po:		types.po
comclient.po:		lists.po
detcheck.po:		determinacy.po
determinacy.po:		ordsets.po
det.po:			determinacy.po
fastrw.po:		types.po
fdbg.po: 		lists.po sockets.po codesio.po avl.po atts.po clpfd.po
file_systems.po:	types.po system.po
gauge.po: 		lists.po tcltk.po context.pl 
heaps.po:		
jasper.po:		types.po lists.po 
jserver.po:		types.po sockets.po codesio.po system.po avl.po
lists.po:		types.po
lists3.po:		lists.po
linda/client.po:	fastrw.po sockets.po types.po
linda/server.po:	fastrw.po sockets.po lists.po
logarr.po:		types.po
math.po:		types.po
mkindex.po:		lists.po file_systems.po system.po
nondetdecl.po:		
obj_decl.po:		lists.po $(PLATFORM)/alignments.pl
objects.po:		types.po structs.po
ordsets.po:		
pillow.po:		lists.po sockets.po
prologbeans.po:		lists.po terms.po fastrw.po codesio.po system.po prologbeansserver.po
prologbeansserver.po:	sockets.po
queues.po:		
queues3.po:		queues.po
random.po:		types.po
random3.po:		random.po assoc3.po
rem.po:			between.po
resgen.po:		types.po lists.po file_systems.po
samsort.po:		
sets.po:		lists.po
sockets.po:		types.po
str_decl.po:		$(PLATFORM)/alignments.pl
structs.po:		types.po $(PLATFORM)/alignments.pl
SU_messages.po:		
system.po:		types.po
system3.po:		system.po file_systems.po sockets.po lists.po
terms.po:		types.po avl.po
timeout.po:		types.po
trees.po:		
types.po:		
ugraphs.po:		types.po lists.po ordsets.po avl.po random.po
varnumbers.po:		types.po
wgraphs.po:		types.po lists.po ordsets.po avl.po heaps.po random.po
xml.po:			types.po lists.po
xref.po:		avl.po

# [PM] 4.0 library(sockets) has no foreign resource with SPIO
SOCKETS_C_SOURCE = 


$(PLATFORM)/alignments.pl:	$(PLATFORM)/alignments$(EXEEXT)
	$< > $@

$(PLATFORM)/alignments$(EXEEXT):	structs/str_align.c
	$(MACOSX_MALLOC_SCRIBBLE_KLUDGE) $(CC) $(CFLAGS) $(OUTPUT_OPT)$@ $<
	rm -f $(<F).$(OBJEXT)

MODULES=$(MODULES1) $(JAVA_MODULES) $(MODULES2) $(MODULES3) $(MODULES4)

# [PM] 4.0 order only prerequisite telling to make SU_messages.po before everything else
$(filter-out SU_messages.po, $(MODULES)) : | SU_messages.po

# .po files are not really portable, so make sure that they are
# rebuilt when changing platform.
$(MODULES): $(SRCDIR)/../$(PCACHE)

$(SRCDIR)/../$(PCACHE):
	touch $@

# [PM]
.PHONY: other
other: $(OTHER)

.PHONY: test_modules
test_modules:
	set -e; for module in $(MODULES:.po=); do \
	  echo ;\
	  echo "*****************************************" ;\
	  echo "*** Attemting to load module $$module ***" ;\
	  echo "*****************************************" ;\
	  echo "use_module(library('$$module')),halt." | $(SICSTUS) -f ;\
	done


# ------------------------------------------------------------------------------
# Stuff to do before building...
# ------------------------------------------------------------------------------

SUBDIR_MODULES= $(CLPFD_MODULE) $(JASPER_MODULE) $(TCLTK_MODULE) $(BDB_MODULE) $(COMCLIENT_MODULE) chr.po

# [MC] 4.0beta6 additional subdirs that should be installed
EXTRA_ORIG_FILES_DIRS = chr/orig_files chr/orig_files/examples chr/orig_files/errors chr/orig_files/warnings clpqr/examples clpqr/examples/monash


DIRS=$(patsubst %.po,$(PLATFORM)/%,$(SUBDIR_MODULES))

$(DIRS):
	$(MKDIR) $@

.PHONY: dirs
dirs: $(DIRS)

# ------------------------------------------------------------------------------
# Some library modules want soft-links here and there...
# ------------------------------------------------------------------------------
CLPQR_COMMONS=compenv.pl arith.pl bb.pl bv.pl dump.pl fourmotz.pl \
	   ineq.pl itf3.pl nf.pl ordering.pl project.pl redund.pl store.pl
CLPQ_LINKS= $(patsubst %.pl,clpq/%.pl,$(CLPQR_COMMONS))
CLPR_LINKS= $(patsubst %.pl,clpr/%.pl,$(CLPQR_COMMONS))

SOFTLINKS=$(CLPQ_LINKS) $(CLPR_LINKS)

ifeq ($(WIN32),yes)
$(CLPQ_LINKS): clpq/%.pl: clpqr/%.pl
$(CLPR_LINKS): clpr/%.pl: clpqr/%.pl
$(CLPR_LINKS) $(CLPQ_LINKS):
	($(RM) -f $@; $(CP) -f $< $@)
else
$(CLPQ_LINKS): clpq/%.pl: clpqr/%.pl
	($(RM) -f $@; $(LN) ../$< $@)
$(CLPR_LINKS): clpr/%.pl: clpqr/%.pl
	($(RM) -f $@; $(LN) ../$< $@)
endif

clpq.po: $(CLPQ_LINKS)
clpr.po: $(CLPR_LINKS)

# ------------------------------------------------------------------------------
# Main rule. Build invididual modules by:
# % make all MODULES='tcltk.po system.po'".
# ------------------------------------------------------------------------------

ifeq ($(NEW_MAKE_RULES),yes)

.PHONY: all
all: dirs
	$(MAKE) all_the_rest

.PHONY: all_the_rest
all_the_rest: $(MODULES) other INDEX.pl


# Duh! Making a real target depend on a phony target will never
# consider the real target up-to-date
# all: $(MODULES) other INDEX.pl
#
# # This is to force dirs before anything else. I did not find a working
# # way to express the dependency on the $(PLATFORM) directory directly.
# # Note that you really do not want to depend on the modification date
# # of a directory, only its existence. One (failed) idea was to use a
# # time-stamp file in each directory.
# $(MODULES): dirs
# other: dirs
# INDEX.pl: dirs


else				# 3.8/early 3.9
.PHONY: all
all:
	$(MAKE) dirs jni_headers other
	$(MAKE) $(MODULES)
	$(MAKE) INDEX.pl
endif

# [PM] 3.12.2 more silent by default
SICSTUS_SILENT_FLAGS := --nologo --noinfo
SICSTUS_FLAGS = -f $(SICSTUS_SILENT_FLAGS)

# [PM] 3.11.2 make it possible to override for cross compiling
LIBRARY_SICSTUS_FLAGS ?= $(SICSTUS_FLAGS)
MKLIBS_MODULES1_SICSTUS ?= $(SICSTUS)
MKLIBS_MODULES1_SICSTUS_FLAGS ?= $(LIBRARY_SICSTUS_FLAGS)
MKLIBS_MODULES2_SICSTUS ?= $(SICSTUS)
MKLIBS_MODULES2_SICSTUS_FLAGS ?= $(LIBRARY_SICSTUS_FLAGS)
MKLIBS_MODULES3_SICSTUS ?= $(SICSTUS)
MKLIBS_MODULES3_SICSTUS_FLAGS ?= $(LIBRARY_SICSTUS_FLAGS)
MKLIBS_MODULES4_SICSTUS ?= $(SICSTUS)
MKLIBS_MODULES4_SICSTUS_FLAGS ?= $(LIBRARY_SICSTUS_FLAGS)
MKLIBS_JAVA_MODULES_SICSTUS ?= $(SICSTUS)
MKLIBS_JAVA_MODULES_SICSTUS_FLAGS ?= $(LIBRARY_SICSTUS_FLAGS)

# [PM] 4.0 When we go final we should not allow any known broken library modules
ifneq ($(SICSTUS_BETA_VERSION), 0)
ALLOW_KNOWN_BAD_LIBRARIES := yes
endif

ifeq ($(ALLOW_KNOWN_BAD_LIBRARIES), yes)
QA_BARF = :
else
QA_BARF = printf "Module \"%s\" was disapproved%s for QA reasons, cannot continue (see disapproved.txt)\n" "$(@F)" "$${d_by:+ by $${d_by}}" 1>&2; exit 1
endif

define BUILD_LEGEND
	@printf "\nBuilding %s " "$@"; \
	 if [ -f approved.txt ]; then \
	   a_by=`sed -n -e 's/#.*^//' -e 's/^$(basename $(@F))[ \t][ \t]*\([a-zA-Z0-9][a-zA-Z0-9]*\).*$$/\1/p' < approved.txt`; \
	   d_by=`sed -n -e 's/#.*^//' -e 's/^$(basename $(@F))[ \t][ \t]*\([a-zA-Z0-9][a-zA-Z0-9]*\).*$$/\1/p' < disapproved.txt`; \
	   d_cmnt=`sed -n -e 's/^$(basename $(@F))[ \t][ \t]*\([a-zA-Z0-9][a-zA-Z0-9]*\)[ \t]*#[ \t]*\(..*\)$$/\2/p' < disapproved.txt`; \
	 else \
	   a_by=default; \
	   d_by=''; \
	   d_cmnt=''; \
	 fi; \
	 if [ -n "$${d_by}" ]; then \
	   if [ -n "$${a_by}" ]; then \
	     printf "!!! QA REJECTED by %s but approved by %s, fight it out will you!\n" "$${d_by}" "$${a_by}"; \
	   else \
	     printf "!!! QA REJECTED BY %s%s !!\n" "$${d_by}" "$${d_cmnt:+ (}$${d_cmnt}$${d_cmnt:+)}"; \
	   fi; \
	   $(QA_BARF) ; \
	 elif [ -n "$${a_by}" ]; then \
	   if [ "default" != "$${a_by}"  ]; then printf "(approved by %s) ...\n" "$${a_by}"; else printf "...\n"; fi \
	 else \
	   printf " Not QA-approved! (see approved.txt)\n"; \
	 fi; \
	 printf "=========================================\n\n"
endef

$(MODULES1): %.po: %.pl
	$(BUILD_LEGEND)
	echo "make('$*',[]),halt." | $(MKLIBS_MODULES1_SICSTUS) $(MKLIBS_MODULES1_SICSTUS_FLAGS) -l mklibs $(SICSTUS_MKLIBS_FLAGS)
	$(VERIFY_TARGET_CREATED)
	$(TOUCH_SP_TARGET)

$(MODULES2): %.po: %.pl context.pl
	$(BUILD_LEGEND)
	echo "make('$*',[context]),halt." | $(MKLIBS_MODULES2_SICSTUS) $(MKLIBS_MODULES2_SICSTUS_FLAGS) -l mklibs $(SICSTUS_MKLIBS_FLAGS)
	$(VERIFY_TARGET_CREATED)
	$(TOUCH_SP_TARGET)

$(MODULES3): %.po: %.pl
	$(BUILD_LEGEND)
	echo "make_$*,halt." | $(MKLIBS_MODULES3_SICSTUS) $(MKLIBS_MODULES3_SICSTUS_FLAGS) -l mklibs $(SICSTUS_MKLIBS_FLAGS)
	$(VERIFY_TARGET_CREATED)
	$(TOUCH_SP_TARGET)

$(MODULES4): %.po: 
	$(BUILD_LEGEND)
	$(MAKE) -C chr all SICSTUS=$(SICSTUS)
	echo "make_$*,halt." | $(MKLIBS_MODULES4_SICSTUS) $(MKLIBS_MODULES4_SICSTUS_FLAGS) -l mklibs $(SICSTUS_MKLIBS_FLAGS)
	$(VERIFY_TARGET_CREATED)
	$(TOUCH_SP_TARGET)

$(JAVA_MODULES): %.po: %.pl
	$(BUILD_LEGEND)
	echo "make('$*',[]),halt." | $(JAVA_PRELOAD) $(MKLIBS_JAVA_MODULES_SICSTUS) $(MKLIBS_JAVA_MODULES_SICSTUS_FLAGS) -l mklibs $(SICSTUS_MKLIBS_FLAGS)
	$(VERIFY_TARGET_CREATED)
	$(TOUCH_SP_TARGET)

## [PM] Build vbsp dll. Note that this is not a foreign resource. It
## is only loaded from VB.
##

# C flags for foreign resources. Make sure the latest sicstus.h is
# found first by placing prefix/include first.
CFLAGS:= -I'$(call NATIVEPATH, $(prefix)/include)' $(CFLAGS)

.PHONY: vbsp
vbsp: $(PLATFORM)/vbsp.$(SHSFX)

## We would like to use SPLD but there is currently (3.9 beta/3.8.6)
## no way to avoid linking in charmain.obj etc.
## Want someting like $(SPLD) --main=main /* userwritten main etc */ --dll ...
#$(PLATFORM)/vbsp.$(SHSFX): vbsp/vbsp.c
#	: # do we need to create an import library?
#	$(SPLD) -main=user --cflag=-LD $(SPLDFLAGS) $^ -o $@
#

$(PLATFORM)/vbsp.$(SHSFX): %.$(SHSFX): %.$(OBJEXT)
	$(SHLD) $(SHLDFLAGS) $^ ../Emulator/$(PLATFORM)/spaux.$(OBJEXT) '$(call NATIVEPATH, $(SP_BINDIR)/$(IMPLIB))' $(SHLD_OUTPUT_OPT)$@ -IMPLIB:$*.lib
	$(RM) -f $*.exp $(RMDUMMY)
	: # Do we really need an import library?
	$(RM) -f $*.lib $(RMDUMMY)

$(PLATFORM)/vbsp.$(OBJEXT): vbsp/vbsp.c vbsp/sp_fpwrap.h
	$(CC) $(CFLAGS) $(INCR_CFLAGS) $(NOLINK_OPT) $< $(NOLINK_OUTPUT_OPT)$@

# [PD] 3.11.3
.PHONY: prologbeansdotnet
prologbeansdotnet:
	$(MAKE) -C prologbeans.NET

# ------------------------------------------------------------------------------
# Definition of foreign resources.
# ------------------------------------------------------------------------------

# TODO: fix dependencies from C source to header files.

# These definitions are used in the makefile fragments generated below by
# genmakefile.pl. Their name is very significant: <module>_src etc. is used
# to construct dependencies for <module>.<shsfx>.
# The .c suffix should not be present
math_src=math
system_src=system
# ifeq ($(WIN32),yes)
# system_src += systw32
# endif
sp_system_src=$(addprefix sp_, $(system_src))



ifneq ($(strip $(SOCKETS_C_SOURCE)),)
sockets_src=sockets
endif				# SOCKETS_C_SOURCE

codesio_src=codesio
timeout_src=timeout
random_src=random
fastrw_src=fastrw
structs_src=structs
$(patsubst %,$(PLATFORM)/%_d.$(OBJEXT),$(fastrw_src)) : fastrw.h
$(patsubst %,$(PLATFORM)/%_s.$(OBJEXT),$(fastrw_src)) : fastrw.h

tcltk_src=tcltk/tcl tcltk/tk tcltk/tkappini tcltk/tkterm tcltk/util
comclient_src=comclient/comclient

clpfd_src= \
clpfd/ac_arith \
clpfd/alldifferent \
clpfd/alldistinct \
clpfd/arith \
clpfd/bc_alldiff \
clpfd/bool \
clpfd/case \
clpfd/disjoint2 \
clpfd/discrete \
clpfd/dvars \
clpfd/fdsets \
clpfd/gcc \
clpfd/indexical \
clpfd/lcc \
clpfd/lex \
clpfd/main \
clpfd/nvalue \
clpfd/profile \
clpfd/reified \
clpfd/relation \
clpfd/setsingleton \
clpfd/sorting \
clpfd/statistics \
clpfd/support \
clpfd/symmcum \


# [PM] 3.9b4 (almost) all CLPFD files depend on fd.h, dvars.h, qsort.ic
$(patsubst %,$(PLATFORM)/%_d.$(OBJEXT),$(clpfd_src)) : clpfd/fd.h clpfd/dvars.h clpfd/qsort.ic
$(patsubst %,$(PLATFORM)/%_s.$(OBJEXT),$(clpfd_src)) : clpfd/fd.h clpfd/dvars.h clpfd/qsort.ic

# [PM] 4.0 everything in tcltk depends on tcl.h
$(patsubst %,$(PLATFORM)/%_d.$(OBJEXT),$(tcltk_src)) : tcltk/tcl.h
$(patsubst %,$(PLATFORM)/%_s.$(OBJEXT),$(tcltk_src)) : tcltk/tcl.h

jasper_src=jasper/jasper
bdb_src=bdb/dbaux bdb/hash bdb/bdb bdb/ixkeys bdb/bdb_sync
ifneq ($(strip $(SOCKETS_C_SOURCE)),)
sockets_lib=$(SOCKETLIB)
endif				# SOCKETS_C_SOURCE
system_lib=$(SYSTEMLIB) 
tcltk_lib=$(TCLLIB)
bdb_lib=$(BDBLIB)
jasper_lib=$(JAVALIB)
comclient_lib=$(COMCLIENTLIB)

#################################################################################
######## Tcl/Tk

## [PM] 3.9 Added -I. to CFLAGS for all resources that reside in
##          subdirs in order to find <RESOURCE>_glue.h
tcltk_copt=$(TCLINC) -I.

## [PM] 3.9 On scheutz (sparc-solaris-5.7) The X11 headers contains a
## lot of function declarations without return type.
ifeq "$(strip $(notdir $(CC)))" "gcc"
   tcltk_copt += -Wno-implicit-int
endif

# [PM] 3.9b4 Tcl/Tk headers are broken. they should say that their API uses __cdecl
ifeq ($(WIN32),yes)
   tcltk_copt += -Gd
   tcltk_d_splfr_copt = $(patsubst %,--cflag=%$(space),$(filter-out -Gd, $(tcltk_copt)))
   tcltk_s_splfr_copt = $(patsubst %,--cflag=%$(space),$(filter-out -Gd, $(tcltk_copt)))
endif				# WIN32


ifeq ("$(strip $(notdir $(CC)))","gcc") # gcc

        # [PM] 3.9b4 Older X11 headers are not fully prototyped, even
        # if __STDC__ In particular this happens on scheutz (SunOS
        # 5.7, X11 headers from 1991) We could check the headers, X11
        # on Linux does the right thing.

	tcltk_copt += -Wno-strict-prototypes

endif				# gcc
# [PM] 3.9b5 ignore #pragma ident in Solaris Xlib.h
tcltk_copt += $(NO_UNKNOWN_PRAGMAS)

#################################################################################
######## Berkeley DB


ifeq ($(WIN32),yes)

   # VS 8 uses 64bit time_t by default but the BDB installer is built
   # with VS 7.1 which used 32bit time_t. Try to work around it by
   # forcing VS 8 to go back the the old behavior
   bdb_defines += -D_USE_32BIT_TIME_T
endif

bdb_copt=-DSICSTUS $(bdb_defines) $(BDBINC) -I.

# [PM] 3.9b4 bdb headers are broken. they should say that their API uses__cdecl
ifeq ($(WIN32),yes)
   bdb_copt += -Gd

   bdb_d_splfr_copt = $(patsubst %,--cflag=%$(space),$(filter-out -Gd, $(bdb_copt)))
   bdb_s_splfr_copt = $(patsubst %,--cflag=%$(space),$(filter-out -Gd, $(bdb_copt)))

endif				# WIN32

#################################################################################
######## Sockets
ifneq ($(strip $(SOCKETS_C_SOURCE)),)
sockets_copt=
ifeq ($(ENABLE_MULTITHREADED_LIBS),yes)
sockets_d_copt = $(sockets_copt)
sockets_d_copt += -DMULTI_SP_AWARE
sockets_d_splfrflags = $(sockets_splfrflags)
sockets_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTITHREADED_LIBS
endif				# SOCKETS_C_SOURCE
#################################################################################
######## Jasper

# [PM] 3.9 Q: If PCC_STRUCT_RETURN is needed then it should perhaps
# make it default for all foreign resources (I guess it used to work
# because foreign resources never return structures to SICStus).

jasper_copt = 
# [PM] 4.0 Do not pass -fpcc-struct-return.
#
#  GCC docs implies that GCC does the right thing if
#  -fpcc-struct-return is NOT specified:
#    If you specify neither `-fpcc-struct-return' nor
#    `-freg-struct-return', GCC defaults to whichever convention is
#    standard for the target.
#
# jasper_copt += $(PCC_STRUCT_RETURN)

jasper_copt += $(NO_UNKNOWN_PRAGMAS) $(JAVAINC) -I.

# NOTE: (lib)jasper_splfrflags should match what is used by
#        InstallSICStus.in ([PM] 3.8.6)



jasper_splfrflags=$(JASPER_SPLFR_FLAGS)

# NOTE: (lib)jasper_splfrflags should match what is used by
#        InstallSICStus.in ([PM] 3.8.6)

ifeq ($(ENABLE_MULTI_SP),yes)
jasper_d_copt = $(jasper_copt)
jasper_d_copt += -DMULTI_SP_AWARE
jasper_d_splfrflags = $(jasper_splfrflags)
jasper_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTI_SP

#################################################################################
######## codesio
ifeq ($(ENABLE_MULTITHREADED_LIBS),yes)
codesio_d_copt = $(codesio_copt)
codesio_d_copt += -DMULTI_SP_AWARE
codesio_d_splfrflags = $(codesio_splfrflags)
codesio_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTITHREADED_LIBS


#################################################################################
######## CLPFD

SP_INSIDER_COPTS = -DSP_INSIDER -DLOCAL_INCLUDES -I$(SRCDIR)/../Emulator -I$(SRCDIR)/../Emulator/$(PLATFORM) -I.

clpfd_copt = $(SP_INSIDER_COPTS)

ifeq ($(ENABLE_CLPFD_MULTI_SP),yes)
ifeq ($(ENABLE_MULTI_SP),yes)
clpfd_d_copt = $(clpfd_copt)
clpfd_d_copt += -DMULTI_SP_AWARE
clpfd_d_splfrflags = $(clpfd_splfrflags)
clpfd_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTI_SP
endif				# ENABLE_CLPFD_MULTI_SP


#################################################################################
######## fastrw
fastrw_copt = -DSICSTUS
ifeq ($(ENABLE_MULTITHREADED_LIBS),yes)
fastrw_d_copt = $(fastrw_copt)
fastrw_d_copt += -DMULTI_SP_AWARE
fastrw_d_splfrflags = $(fastrw_splfrflags)
fastrw_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTITHREADED_LIBS

#################################################################################
######## math

# Let the warning happen so we can fix it for 4.0.0 release
# ifeq ($(WIN32),yes)
# $(warning \#error FIXME: REMOVE WIN32 BUILD HACK)
# math_copt += /wd4996
# endif				# WIN32

ifeq ($(ENABLE_MULTITHREADED_LIBS),yes)
math_d_copt = $(math_copt)
math_d_copt += -DMULTI_SP_AWARE
math_d_splfrflags = $(math_splfrflags)
math_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTITHREADED_LIBS

#################################################################################
######## random
ifeq ($(ENABLE_MULTITHREADED_LIBS),yes)
random_d_copt = $(random_copt)
random_d_copt += -DMULTI_SP_AWARE
random_d_splfrflags = $(random_splfrflags)
random_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTITHREADED_LIBS

#################################################################################
######## system

## [PM] 4.0 hopefully no WinCE specifics needed anymore
## # [PM] WinCE need Emulator/wince.h, Emulator/win32getimes.ic
## system_copt=-I$(SRCDIR)/../Emulator

# [PM] 4.0 The little that remains of system is now multithreaded
ifeq ($(ENABLE_MULTITHREADED_LIBS),yes)
system_d_copt = $(system_copt)
system_d_copt += -DMULTI_SP_AWARE
system_d_splfrflags = $(system_splfrflags)
system_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTITHREADED_LIBS


## ifeq ($(ENABLE_SYSTEM_SINGLE_THREADED),yes)
## system_d_copt = $(system_copt)
## # system_d_copt += -DSP_SINGLE_THREADED
## system_d_splfrflags = $(system_splfrflags)
## # system_d_splfrflags += --exclusive-access
## endif

## sp_system_copt=-I$(SRCDIR)/../Emulator -DSP_SYSTEM
## ifeq ($(ENABLE_SYSTEM_SINGLE_THREADED),yes)
## sp_system_d_copt = $(sp_system_copt)
## # sp_system_d_copt += -DSP_SINGLE_THREADED
## sp_system_d_splfrflags = $(sp_system_splfrflags)
## # sp_system_d_splfrflags += --exclusive-access
## endif

#################################################################################
######## timeout
ifeq ($(ENABLE_PTHREAD_TIMEOUT),yes)
# [PM] 3.9b4 the new pthread based timeout. Very experimental
timeout_copt += -DLEGACY_TIMOUT=0
endif
# [PM] 3.11.2 needs Emulator/win32getimes.ic
timeout_copt += -I$(SRCDIR)/../Emulator

#################################################################################
######## structs


############################################################################################

# ------------------------------------------------------------------------------
# Build rules for foreign resources
# ------------------------------------------------------------------------------

# FOREIGN_MODULES define which modules get a shared/static resource
# built with them. 
FOREIGN_MODULES= \
	math.po random.po system.po $(SP_SYSTEM_PO) timeout.po codesio.po \
	$(if $(strip $(SOCKETS_C_SOURCE)), sockets.po ) fastrw.po structs.po \
	$(CLPFD_MODULE) $(TCLTK_MODULE) \
	$(BDB_MODULE) $(COMCLIENT_MODULE)

# [PM] 3.9 jasper is just another foreign module
FOREIGN_MODULES += $(JASPER_MODULE)

# This rule says: every foreign module has an associated shared resource
# and an optional static resource.
$(FOREIGN_MODULES): %.po: $(PLATFORM)/%.$(FLI_SHSFX)
ifeq ($(ENABLE_STATLIB),yes)
$(FOREIGN_MODULES): %.po: $(PLATFORM)/%.$(STSFX)
endif

SHARED_RESOURCES=$(patsubst %.po,$(PLATFORM)/%.$(FLI_SHSFX),$(FOREIGN_MODULES)) 
STATIC_RESOURCES=$(patsubst %.po,$(PLATFORM)/%.$(STSFX),$(FOREIGN_MODULES))

# We generate makefile fragments which describe foreign resource
# dependencies.
# [PM] 3.9 generate in $(PLATFORM)
FR_MAKEFILES=$(patsubst %.po,$(PLATFORM)/Makefile_%,$(FOREIGN_MODULES))

# Generate makefile fragments for foreign resource dependencies.
# [PM] 3.9 Create $PLATFORM if not already there. (the - is to ignore
# errors in case mkdir complains if the directory is there (which in
# turn will only be a problem with paralell make which we do not use
# (yet), oh well).
$(FR_MAKEFILES): $(PLATFORM)/Makefile_%: genmakefile.pl
	-test -d $(dir $@) || mkdir $(dir $@)
	$(PERL) genmakefile.pl $* $*.pl $(OBJEXT) $($(*)_src) > $@

# [PM] 4.0 Ensure generated makefiles are re-built if this makefile changes
$(FR_MAKEFILES): Makefile

# [PM] 3.9 Build extern declarations for foreign/2,3 facts
$(PLATFORM)/%_glue.h: %.pl
	$(SPLFR) $(SPLFRFLAGS) --header='$@' --nocompile $^
	$(TOUCH_SP_TARGET)

# [PM] 3.9 Do not include (in particular do not generate) Makefile_* when cleaning
ifeq ($(findstring clean,$(MAKECMDGOALS)),) # clean not found in MAKECMDGOALS
include $(FR_MAKEFILES)
endif


ifneq ($(JASPER_MODULE),)

# ------------------------------------------------------------------------------
# Build JNI header files
# ------------------------------------------------------------------------------

#[PD] 3.9 added Jasper (*** FIX? Since no native methods are left in SPTerm,
#                       SPPredicate, SPQuery, SPException and SPCanonicalAtom
#                       (they have been moved to SICStus), should we remove all
#                       but Jasper and SICStus?)
JNI_CLASSES= Jasper SICStus SPTerm SPPredicate SPQuery SPException SPCanonicalAtom
JNI_HDR= $(patsubst %,jasper/se_sics_jasper_%.h,$(JNI_CLASSES))

.PHONY: jni_headers
jni_headers: $(JNI_HDR)

# [PM] 3.10.2 ../ is ugly
$(JNI_HDR): jasper/se_sics_jasper_%.h: $(SRCDIR)/../se/sics/jasper/%.class
	(cd jasper ; $(JAVAH) -verbose -classpath $(SRCDIR)/../.. -jni se.sics.jasper.$* )
	touch $@

# this is cheating, it is really compiling spnative.c that needs JNI_HDR 
jasper.po: $(JNI_HDR)

# ------------------------------------------------------------------------------
# Build examples in library('jasper/examples')
# ------------------------------------------------------------------------------

JASPER_XMPLDIR=$(SRCDIR)/jasper/examples

#[PD] 3.8.7 no foreign resources in example
#JASPER_EXAMPLES=$(patsubst %, $(JASPER_XMPLDIR)/%, simple.$(SHSFX) Queens.class Simple.class simple.po jqueens.po)

JASPER_EXAMPLE_FILES = Simple.class simple.po

ifeq ($(ENABLE_CLPFD),yes)	# [PM] 3.10.1 conditionalize on clpfd
JASPER_EXAMPLE_FILES += Queens.class jqueens.po
endif				# ENABLE_CLPFD

JASPER_EXAMPLES := $(addprefix $(JASPER_XMPLDIR)/, $(JASPER_EXAMPLE_FILES))

.PHONY: jasper_examples
jasper_examples: $(JASPER_EXAMPLES)

# [PM] 3.10.2 ensure errors aborts compilation
$(JASPER_XMPLDIR)/simple.po $(JASPER_XMPLDIR)/jqueens.po: %.po: %.pl $(SRCDIR)/../$(PCACHE)
	echo "$(BARF_ON_ERROR) ensure_loaded('$<'),save_files('$<','$@'),halt." | $(JAVA_PRELOAD) $(SICSTUS) $(LIBRARY_SICSTUS_FLAGS)
	$(VERIFY_TARGET_CREATED)
	$(TOUCH_SP_TARGET)

# We don't use jasper.jar here in case it hasn't been built. ([PM] 3.10.2 FIXME: it should have been built!)
JASPER_XMPL_CLASSPATH='$(SRCDIR)$(PATHSEP)..$(PATHSEP)$(call NATIVEPATH, $(JASPER_XMPLDIR))'

$(JASPER_XMPLDIR)/Queens.class $(JASPER_XMPLDIR)/Simple.class: %.class: %.java
	$(JAVAC) $(JAVACFLAGS) $(JAVACDBGFLAGS) -verbose -deprecation -classpath $(JASPER_XMPL_CLASSPATH) $^

# [PM] 3.10.2 why was this here?
# JAVA_EXAMPLE_FLAGS=$(JAVA_EXECOPTS) \
# 	-Djava.library.path='$(call NATIVEPATH, $(SP_ROOTLIBDIR))' \
# 	-Dsicstus.path='$(call NATIVEPATH, $(SP_LIBDIR))' \
# 	-classpath $(JASPER_XMPL_CLASSPATH)
#
# #[PD] 3.8.7 no foreign resources in example
# #run_simple: $(JASPER_XMPLDIR)/Simple.class $(JASPER_XMPLDIR)/simple.po $(JASPER_XMPLDIR)/simple.$(SHSFX)
# run_simple: $(JASPER_XMPLDIR)/Simple.class $(JASPER_XMPLDIR)/simple.po
# 	$(JAVAEXEC) $(JAVA_EXAMPLE_FLAGS) Simple

else				# !JASPER_MODULE
.PHONY: jasper_examples jni_headers
jasper_examples jni_headers:
	@echo "Jasper is not supported on this platform. Target \"$@\" will not be built."
endif				# !JASPER_MODULE

# [PM] 3.10.2 PrologBeans examples

# [PM] 3.11.2 Assume ENABLE_JAVA=yes implies PROLOGBEANS_MODULE non-empty
ifeq ($(ENABLE_JAVA),yes)

# [PM] 3.10.2 ../ is ugly
PROLOGBEANS_JAR := $(SRCDIR)/../se/sics/prologbeans/prologbeans.jar

PROLOGBEANS_EXAMPLES_DIR := $(SRCDIR)/prologbeans/examples

# [PM] 3.10.2 there is not much point in compiling the example prolog files.
PROLOGBEANS_EXAMPLE_FILES := $(filter-out %.po, \
	pbtest/PBTest.class pbtest/pbtest.po \
        evaluate/EvaluateGUI.class evaluate/evaluate.po \
	sessionsum/sessionsum.po )

PROLOGBEANS_EXAMPLES := $(addprefix $(PROLOGBEANS_EXAMPLES_DIR)/, $(PROLOGBEANS_EXAMPLE_FILES))


PROLOGBEANS_EXAMPLE_CLASSPATH := '$(call NATIVEPATH, $(PROLOGBEANS_JAR))$(PATHSEP)$(call NATIVEPATH, $(PROLOGBEANS_EXAMPLES_DIR))'

$(filter %.po, $(PROLOGBEANS_EXAMPLES)): %.po: %.pl $(SRCDIR)/../$(PCACHE)
	echo "$(BARF_ON_ERROR) ensure_loaded('$<'),save_files('$<','$@'),halt." | $(SICSTUS) $(LIBRARY_SICSTUS_FLAGS)
	$(VERIFY_TARGET_CREATED)
	$(TOUCH_SP_TARGET)

$(filter %.class, $(PROLOGBEANS_EXAMPLES)): %.class: %.java
	$(JAVAC) $(JAVACFLAGS) $(JAVACDBGFLAGS) -verbose -deprecation -classpath $(PROLOGBEANS_EXAMPLE_CLASSPATH) $^


.PHONY: prologbeans_examples
prologbeans_examples: $(PROLOGBEANS_EXAMPLES)

else				# !ENABLE_JAVA
.PHONY: prologbeans_examples
prologbeans_examples:
	@echo "PrologBeans is not supported on this platform. Target \"$@\" will not be built."
endif				# !ENABLE_JAVA
# ------------------------------------------------------------------------------
# Support for require/1
# ------------------------------------------------------------------------------

INDEX.pl: mkindex.po $(MODULES:.po=.pl) 
	@echo
	@echo "Building $@..."
	@echo =========================================
	@echo 
	echo "$(BARF_ON_ERROR) ensure_loaded(mkindex),make_index:make_library_index('.'); halt(2)." | $(SICSTUS) $(LIBRARY_SICSTUS_FLAGS)
	$(VERIFY_TARGET_CREATED)
	$(TOUCH_SP_TARGET)

# ------------------------------------------------------------------------------
# Installation
# ------------------------------------------------------------------------------

INSTALLFILES = Makefile README genmakefile.pl hidemod.pl mklibs.pl

ifeq ($(WIN32),no)
# [PM] 3.9b5 license.pl is only used on non-Windows.
INSTALLFILES += license.pl
endif				# !WIN32

# [PM] 4.0 *~ is (emacs) backup file, #*# is (emacs) autosave file, .#* is CVS "previous version"-file
# Had trouble quoting # so it would not be treated as a comment in either make or shell. Therefore use define instead of ordinary SKIPTEST = ...
# "$${file##*/}" is a (faster?) way of doing $$(basename "$$file")
define SKIPTEST
case "$${file##*/}" in (*~|\#*\#|.\#*) echo "skipping $$file"; continue;; esac;
endef
# [PM] 4.0 This was used before but did the wrong thing for non-basename $file
# SKIPTEST = if expr "$$file" : '.*~.*' > /dev/null || expr "$$file" : '\.#.*' > /dev/null; then echo "Skipping $$file..." ; continue; fi;

# [PM] 4.0 the original install target does not install everything it should so we might as well trust the new copy-everything approach
INSTALL_WITH_TARCOPY=yes

ifeq ($(WIN32),yes)
# [PM] 4.0 copying with tar instead of (INSTALL) is a gazillion times faster, at least on cygwin
INSTALL_WITH_TARCOPY=yes
endif

# xref doc/Makefile keep in sync
ifeq ($(INSTALL_WITH_TARCOPY), yes)

#          We would use pax -r -w but there is no pax for cygwin

# We copy everything not explicitly removed. We may need to do some manual deletes or add a --exclude-from=FILE 
# This is fast but also easier to maintain. The original install target turned out to be quit badly out of date.

TARCOPY_EXCLUDES = --exclude CVS      # CVS meta info directory
TARCOPY_EXCLUDES += --exclude "\#*\#" # emacs autosave file
TARCOPY_EXCLUDES += --exclude='.\#*'  # CVS previous version
TARCOPY_EXCLUDES += --exclude '*~'    # (emacs) backup files
TARCOPY_EXCLUDES += --exclude '*_glue.*' # splfr generated glue
TARCOPY_EXCLUDES += --exclude '*.stackdump' # cygwin core dumps
# not on Win32: TARCOPY_EXCLUDES += --exclude 'core*' # core dumps

TARCOPY_EXCLUDES += --exclude '*_d.$(OBJEXT)' # dynamic foreign resource objects
TARCOPY_EXCLUDES += --exclude '*_s.$(OBJEXT)' # static foreign resource objects

TARCOPY_EXCLUDES += --exclude license.pl

TARCOPY_EXCLUDES += --exclude approved.txt --exclude disapproved.txt

TARCOPY_EXCLUDES += --exclude 'bdb/iqsoft' # internal documentation etc
TARCOPY_EXCLUDES += --exclude 'Makefile_*' # generated makefiles
TARCOPY_EXCLUDES += --exclude 'alignments' # str_align.pl generated files

TARCOPY_EXCLUDES += --exclude '*/prologbeans.NET/*.cs' # The C# code is not longer used

TARCOPY_EXCLUDES += --exclude='chr/examples*'

# not (currently) distributed libraries
TARCOPY_EXCLUDES += --exclude vbsp
TARCOPY_EXCLUDES += --exclude jasper


# $(warning $$(TARCOPY_EXCLUDES) == $(TARCOPY_EXCLUDES))

.PHONY: install
install:
	@echo "Creating directory $(SP_LIBDIR)/library..."
	$(MKDIR) "$(SP_LIBDIR)/library"
	$(INSTALL_DATA) $(SRCDIR)/../Common $(SP_LIBDIR)
	$(GNUTAR) --create --mode=a+rwX $(TARCOPY_EXCLUDES) . | $(GNUTAR) --file=- --extract -C "$(SP_LIBDIR)/library" --verbose
else

$(error [PM] 4.0 The original install target has not managed to keep itself updated.)

.PHONY: install
install:
	@echo "Creating directory $(SP_LIBDIR)/library..."
	$(MKDIR) "$(SP_LIBDIR)/library"
	$(INSTALL_DATA) $(SRCDIR)/../Common $(SP_LIBDIR)
	: # Install makefile fragments for foreign resources
	: # and some auxilliary Prolog code
	set -e; for file in $(INSTALLFILES); do \
	    $(SKIPTEST) \
	    echo "Installing $$file..." ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/`dirname "$$file"`" ;\
	    $(INSTALL_DATA) "$$file" "$(SP_LIBDIR)/library/`dirname "$$file"`" ;\
	done
# [PM] 4.0 these are gone, for now
#	: # Install additional Tcl/Tk stuff
#	set -e; if [ -n "$(TCLTK_MODULE)" ]; then \
#	    for file in tkconsole.pl tkconsole.tcl tkterm.tcl; do \
#	        if expr $$file : '.*~.*' > /dev/null || expr $$file : '\.#.*' > /dev/null; then echo "Skipping $$file..." ; continue; fi; \
#	    	echo "Installing $$file..." ;\
#	    	$(INSTALL_DATA) $$file "$(SP_LIBDIR)/library" ;\
#	    done ;\
#	fi
	: # Install Prolog source code for all the modules
	set -e; for file in $(MODULES:.po=.pl); do \
	    $(SKIPTEST) \
	    echo "Installing $$file..." ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/`dirname "$$file"`" ;\
	    $(INSTALL_DATA) $$file "$(SP_LIBDIR)/library/`dirname "$$file"`" ;\
	done
	: # Install auxilliary Prolog source code in module subdirectories
	set -e; for subdir in $(SUBDIR_MODULES:.po=) $(EXTRA_ORIG_FILES_DIRS); do \
	    echo "Creating directory $(SP_LIBDIR)/library/$$subdir" ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/$$subdir" ;\
	    for file in $$subdir/*; do \
	        $(SKIPTEST) \
		if [ -f "$$file" ]; then \
		    echo "Installing $$file..." ;\
		    $(INSTALL_DATA) "$$file" "$(SP_LIBDIR)/library/$$subdir" ;\
		fi ;\
	    done ;\
	done
	: # Install example directories
	: # [PM] 3.8.4 Ensure wcx_example is installed
	set -e; for file in `find */examples wcx_example \( -name CVS -prune \) -o \( -type f -a -print \)`; do \
	    $(SKIPTEST) \
	    echo "Installing $$file..." ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/`dirname "$$file"`" ;\
	    $(INSTALL_DATA) $$file "$(SP_LIBDIR)/library/`dirname "$$file"`" ;\
	done
	: # Misc stuff which does not fit in the other categories
	: # categories.
	set -e; for file in INDEX.pl codesio.c context.pl fastrw.* gauge.txt random.c $(SOCKETS_C_SOURCE) system.c timeout.c debugger_examples.pl; do \
	    $(SKIPTEST) \
	    echo "Installing $$file..." ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/`dirname "$$file"`" ;\
	    $(INSTALL_DATA) "$$file" "$(SP_LIBDIR)/library/$$file" ;\
	done
	: # Install object code in $(SP_LIBDIR)/$(PLATFORM)
	@echo "Creating directory $(SP_LIBDIR)/library/$(PLATFORM)..."
	$(MKDIR) "$(SP_LIBDIR)/library/$(PLATFORM)"
	: # [PM] 3.10.2 SPRM 5224 An unmatched *.pdb will not become empty! use make wildcard function instead
	: # [PM] 3.9.1 add any .pdb files (really only for Win32 but will not hurt other platforms)
	set -e; for file in $(PLATFORM)/*.$(FLI_SHSFX) $(PLATFORM)/*.$(STSFX) $(PLATFORM)/*.$(OBJEXT) $(wildcard $(PLATFORM)/*.pdb); do \
	    $(SKIPTEST) \
	    echo "Installing `basename $$file`..." ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/`dirname "$$file"`" ;\
	    $(INSTALL) "$$file" "$(SP_LIBDIR)/library/`dirname "$$file"`" ;\
	done
	: # Install auxilliary object code in $(SP_LIBDIR)/$(PLATFORM)/<subdir>
	set -e; for dir in $(DIRS) clpq clpr clpqr linda; do \
	    echo "Creating directory $$dir..." ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/$$dir" ;\
	    for file in $$dir/* ; do \
	        $(SKIPTEST) \
		if [ -f "$$file" ]; then \
	            if [ -x "$$file" ]; then \
	    	        echo "Installing `basename $$file` (utility)..." ;\
	    	        $(INSTALL) "$$file" "$(SP_LIBDIR)/library/$$dir" ;\
		    else \
	    	        echo "Installing `basename "$$file"`..." ;\
	    	        $(INSTALL_DATA) "$$file" "$(SP_LIBDIR)/library/$$dir" ;\
		    fi \
		fi \
	    done \
	done
	: # Install module .po files.
	set -e; for file in $(MODULES); do \
	    $(SKIPTEST) \
	    echo "Installing $$file..." ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/`dirname "$$file"`" ;\
	    $(INSTALL_DATA) "$$file" "$(SP_LIBDIR)/library/`dirname "$$file"`" ;\
	done
ifeq ($(ENABLE_VBSP),yes)
	: # Install Visual Basic support
	set -e; for file in vbsp/vbsp.c vbsp/vbsp.bas vbsp/sp_fpwrap.h; do \
	    $(SKIPTEST) \
	    echo "Installing $$file..." ;\
	    $(MKDIR) "$(SP_LIBDIR)/library/`dirname "$$file"`" ;\
	    $(INSTALL_DATA) "$$file" "$(SP_LIBDIR)/library/$$file" ;\
	done
endif
endif				# !INSTALL_WITH_TARCOPY

# ------------------------------------------------------------------------------
# Administration
# ------------------------------------------------------------------------------
.PHONY: clean distclean
# [PM] WinCE FIXME: check if the *.o and *.obj should be here, was not in 3.11.1
clean distclean:
	$(RM) -rf $(patsubst %,'%',$(wildcard $(PLATFORM) */$(PLATFORM) *.pdb *.lib *.exp *.mapfile *.o *.obj *.sbr)) $(RMDUMMY)
	$(RM) -f $(patsubst %,'%',$(wildcard $(SOFTLINKS))) $(RMDUMMY)
	$(MAKE) -C chr $@
	$(MAKE) -C jasper/examples $@
	(cd jasper; $(RM) -f se_sics_jasper*.h $(RMDUMMY))
	$(RM) -f $(MODULES) index.pl so_locations $(RMDUMMY)
	$(RM) -f $(patsubst %,'%',$(wildcard *_glue.h *_glue_*.c *_glue*.c)) $(RMDUMMY)
	$(RM) -f $(patsubst %,'%', $(wildcard vbsp/examples/*/vbsp.bas)) $(RMDUMMY)
	$(RM) -f $(patsubst %,'%', $(wildcard prologbeans/examples/*/*.class)) $(RMDUMMY)
	$(MAKE) -C prologbeans.NET $@

.PHONY: maintainerclean mostlyclean
maintainerclean mostlyclean: distclean
	$(MAKE) -C chr $@
	$(RM) -f $(FR_MAKEFILES) $(RMDUMMY)
