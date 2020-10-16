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
OPTIMIZE=
DEFS=-DHAVE_CONFIG_H
CPPFLAGS=-I/var/services/homes/spparmar/spec2006/tools/output/include   -I. -Ilib
CFLAGS=  $(DEFS) $(OPTIMIZE) $(EXTRA_CFLAGS) $(SPECMD5_CFLAGS)
LIBS=  $(EXTRA_LIBS)
LDFLAGS=-Llib -L/var/services/homes/spparmar/spec2006/tools/output/lib   $(EXTRA_LDFLAGS)
STRIP=/usr/bin/strip

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}

LIBSRC=lib/error.c lib/getline.c lib/getopt.c lib/getopt1.c \
       lib/long-options.c lib/md5.c lib/strtol.c lib/strtoul.c \
       lib/strtoull.c lib/xmalloc.c

# For multi-architecture support
srcdir = .

all: specmd5sum

specmd5sum: md5sum.o $(LIBSRC) lib/xmalloc.h
	(cd lib; $(MAKE) libtu.a)
	$(CC) $(LDFLAGS) -o $@ md5sum.o -ltu $(LIBS)
	$(STRIP) $@

install: specmd5sum
	$(INSTALL_PROGRAM) specmd5sum $(bindir)

clean:
	(cd lib && $(MAKE) clean)
	rm -f specmd5sum core *.o

distclean: clean
	(cd lib && $(MAKE) distclean)
	rm -rf Makefile config.h config.log config.cache config.status stamp-h
