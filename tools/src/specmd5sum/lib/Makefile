# Generated automatically from Makefile.in by configure.
#
# Makefile for SPEC's hacked-up md5sum
# Copyright (C) 2003 Standard Performance Evaluation Corporation
#
# $Id: Makefile.in 2348 2005-02-04 03:00:06Z cloyce $

# Generated automatically from Makefile.in by configure.


prefix      = /var/services/homes/spparmar/spec2006/tools/output
exec_prefix = ${prefix}
bindir      = ${exec_prefix}/bin

CC=gcc
AR=/usr/bin/ar
OPTIMIZE=
DEFS=-DHAVE_CONFIG_H
CPPFLAGS=-I/var/services/homes/spparmar/spec2006/tools/output/include   -I. -I..
CFLAGS=  $(DEFS) $(OPTIMIZE) $(EXTRA_CFLAGS) $(SPECMD5_CFLAGS)
LIBS=  $(EXTRA_LIBS)
LDFLAGS=-Llib -L/var/services/homes/spparmar/spec2006/tools/output/lib   $(EXTRA_LDFLAGS)

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
RANLIB = ranlib

# For multi-architecture support
srcdir = .

OBJS = error.o getopt.o getopt1.o long-options.o md5.o strtol.o \
       strtoul.o strtoull.o xmalloc.o 

all: libtu.a

libtu.a: $(OBJS)
	$(AR) r libtu.a $(OBJS)
	$(RANLIB) libtu.a

clean:
	rm -f libtu.a *.o

distclean: clean
	rm -rf Makefile

# Deps
error.o: error.c error.h ../config.h
getline.o: getline.c getline.h ../config.h
getopt.o: getopt.c getopt.h ../config.h
getopt1.o: getopt1.c getopt.h ../config.h
long-options.o: long-options.c long-options.h ../config.h
md5.o: md5.c md5.h ../config.h
strtol.o: strtol.c ../config.h
strtoul.o: strtol.c strtoul.c ../config.h
strtoull.o: strtol.c strtoull.c ../config.h
xmalloc.o: xmalloc.c ../config.h xmalloc.h
