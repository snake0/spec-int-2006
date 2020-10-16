# Generated automatically from Makefile.in by configure.
#
# Makefile for SPEC
# Copyright (C) 1996 Standard Performance Evaluation Corporation
#
# $Id: Makefile.in 4288 2006-05-25 10:42:04Z cloyce $

# Generated automatically from Makefile.in by configure.


prefix      = /var/services/homes/spparmar/spec2006/tools/output
exec_prefix = ${prefix}
bindir      = ${exec_prefix}/bin

CC=gcc
OPTIMIZE=
DEFS=-DHAVE_CONFIG_H
CPPFLAGS=-I/var/services/homes/spparmar/spec2006/tools/output/include  
CFLAGS=  $(CPPFLAGS) $(DEFS) $(OPTIMIZE) $(EXTRA_CFLAGS) $(SPECINVOKE_CFLAGS)
LIBS=  $(EXTRA_LIBS)
LDFLAGS=-L/var/services/homes/spparmar/spec2006/tools/output/lib   $(EXTRA_LDFLAGS)

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}

# For multi-architecture support
srcdir = .

# For the performance monitor hooks
ifeq ($(PM_DIR),)
	PM_DIR=pm_stub
	include $(PM_DIR)/Makefile.settings
endif

all: specinvoke specinvoke_pm

specinvoke: specinvoke.o unix.o getopt.o
	$(CC) $(LDFLAGS) -o $@ specinvoke.o unix.o getopt.o

specinvoke_pm: specinvoke.pm.o unix.pm.o getopt.o pmfuncs.o
	$(CC) $(LDFLAGS) -o $@ specinvoke.pm.o unix.pm.o getopt.o pmfuncs.o

install: specinvoke specinvoke_pm
	$(INSTALL_PROGRAM) specinvoke $(bindir)
	$(INSTALL_PROGRAM) specinvoke_pm $(bindir)

clean:
	(cd $(PM_DIR) && $(MAKE) clean)
	rm -f specinvoke specinvoke_pm core *.o

distclean: clean
	(cd $(PM_DIR) && $(MAKE) distclean)
	rm -rf Makefile config.h config.log config.cache config.status

%.pm.o: %.c specinvoke.h $(PM_DIR)/specinvoke_pm.h
	$(CC) $(CFLAGS) -DPERFMON -I$(PM_DIR) -c -o $@ $<

%.o: %.c specinvoke.h
	$(CC) $(CFLAGS) -c -o $@ $<

pmfuncs.o: $(PM_DEPS)
	(cd $(PM_DIR) && $(MAKE) $@ && cp $@ ..)



