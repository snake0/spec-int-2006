# Generated automatically from Makefile.in by configure.
#
# Makefile for pm_stub performance monitor module for specinvoke
# Copyright (C) 2003 Standard Performance Evaluation Corporation
#
# $Id: Makefile.in 2348 2005-02-04 03:00:06Z cloyce $

# Generated automatically from Makefile.in by configure.


prefix      = /var/services/homes/spparmar/spec2006/tools/output
exec_prefix = ${prefix}
bindir      = ${exec_prefix}/bin

CC=gcc
OPTIMIZE=
DEFS=-DHAVE_CONFIG_H -DPERFMON
CPPFLAGS=-I/var/services/homes/spparmar/spec2006/tools/output/include   -I.. -I.
CFLAGS=  $(CPPFLAGS) $(DEFS) $(OPTIMIZE) $(EXTRA_CFLAGS)
LIBS=  $(EXTRA_LIBS)
LDFLAGS=-L/var/services/homes/spparmar/spec2006/tools/output/lib   $(EXTRA_LDFLAGS)

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}

PERL=@PERL@

all: pmfuncs.o

pmfuncs.o: pmfuncs.c specinvoke_pm.h

clean:
	rm -f core *.o

distclean: clean
	rm -rf Makefile
