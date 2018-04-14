#
# Copyright (C) 1988-1998 Swedish Institute of Computer Science.
#
# CVS: $Id: Makefile,v 1.2 2002/03/05 15:54:12 matsc Exp $
#
# NOTE: This Makefile requires GNU make.
#

SRCDIR=.

# Include common Makefile parts.
include $(SRCDIR)/../../Common

.SUFFIXES:

ENV= server.ql client.ql

$(ENV): %.ql: %.pl hidemod.pl
	-echo "prolog_flag(compiling,_,$(PLFLAGS)), \
	       ['$(filter %hidemod.pl,$^)'], \
	       fcompile('$<'), halt."  | $(SICSTUS) -f
	mv $(patsubst %.pl,%.ql,$<) .	


all-post-recursive: $(ENV)

install:
	$(MKDIR) $(LIBDIR)/linda
	-@CP@ $(ENV) $(LIBDIR)/linda

clean:
	rm -f $(PLATFORM) $(ENV)
