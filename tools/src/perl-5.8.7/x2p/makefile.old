# $RCSfile: Makefile.SH,v 868Revision: 4.1 868Date: 92/08/07 18:29:07 $
#
# $Log:	Makefile.SH,v $

CC = cc
BYACC = byacc
LDFLAGS =  -L/var/services/homes/spparmar/spec2006/tools/output/lib -L/usr/local/lib -L/usr/lib/x86_64-linux-gnu
# XXX Perl malloc temporarily unusable (declaration collisions with stdlib.h)
#mallocsrc = 
#mallocobj = 
shellflags = 

libs = -lnsl -ldl -lm -lcrypt -lutil -lc

#
# grrr
SHELL = /bin/sh

# These variables may need to be manually set for non-Unix systems.
AR = ar
EXE_EXT = 
LIB_EXT = .a
OBJ_EXT = .o
PATH_SEP = :

FIRSTMAKEFILE = makefile

# how to tr(anslate) newlines

TRNL = '\n'

OPTIMIZE = -O2

.SUFFIXES: .c $(OBJ_EXT)


REALPERL = ../perl
CCCMD = `sh $(shellflags) cflags "optimize='$(OPTIMIZE)'" $@`

public = a2p$(EXE_EXT) s2p find2perl

private = 

manpages = a2p.man s2p.man

util =

sh = Makefile.SH cflags.SH
shextract = Makefile cflags

pl = find2perl.PL s2p.PL
plextract = find2perl s2p
plexe = find2perl.exe s2p.exe
plc   = find2perl.c s2p.c
plm   = a2p.loadmap

addedbyconf = $(shextract) $(plextract)

h = EXTERN.h INTERN.h ../config.h ../handy.h hash.h a2p.h str.h util.h

c = hash.c $(mallocsrc) str.c util.c walk.c

obj = hash$(OBJ_EXT) $(mallocobj) str$(OBJ_EXT) util$(OBJ_EXT) walk$(OBJ_EXT)

lintflags = -phbvxac


.c$(OBJ_EXT):
	$(CCCMD) -DPERL_FOR_X2P $*.c

all: $(public) $(private) $(util)
	@echo " "

compile: all
	$(REALPERL) -I../lib ../utils/perlcc -I .. -L .. $(plextract) -v -log ../compilelog;  

a2p$(EXE_EXT): $(obj) a2p$(OBJ_EXT)
	$(CC) -o a2p $(LDFLAGS) $(obj) a2p$(OBJ_EXT) $(libs)

# I now supply a2p.c with the kits, so the following section is
# used only if you force byacc to run by saying
# make  run_byacc

check_byacc:
	@$(BYACC) -V 2>&1 | grep 'version 1\.8\.2'

run_byacc:	FORCE check_byacc
	@ echo Expect many shift/reduce and reduce/reduce conflicts
	$(BYACC) a2p.y
	rm -f a2p.c
	sed -e 's/(yyn = yydefred\[yystate\])/((yyn = yydefred[yystate]))/' \
	    -e 's/(yys = getenv("YYDEBUG"))/((yys = getenv("YYDEBUG")))/' \
	    -e 's/^yyerrlab://' \
	    -e 's/^    goto yyerrlab;//' \
	    -e 's/^yynewerror://' \
	    -e 's/^    goto yynewerror;//' \
	    -e 's|^static char yysccsid\(.*\)|/* static char yysccsid\1 */|' \
	    < y.tab.c > a2p.c

FORCE:

# We don't want to regenerate a2p.c, but it might appear out-of-date
# after a patch is applied or a new distribution is made.
a2p.c: a2p.y
	-@sh -c true

a2p$(OBJ_EXT): a2p.c a2py.c a2p.h EXTERN.h util.h INTERN.h \
		../handy.h ../config.h str.h hash.h
	$(CCCMD) a2p.c

clean:
	rm -f a2p$(EXE_EXT) psed *$(OBJ_EXT) $(plexe) $(plc) $(plm)

realclean: clean
	-rmdir .depending
	rm -f core $(addedbyconf) all malloc.c
	rm -f $(FIRSTMAKEFILE) $(FIRSTMAKEFILE).old makefile.old

veryclean: realclean
	rm -f *~ *.orig

# The following lint has practically everything turned on.  Unfortunately,
# you have to wade through a lot of mumbo jumbo that can't be suppressed.
# If the source file has a /*NOSTRICT*/ somewhere, ignore the lint message
# for that spot.

lint:
	lint $(lintflags) $(defs) $(c) > a2p.fuzz

depend: $(mallocsrc) ../makedepend
	sh ../makedepend MAKE=$(MAKE)

clist:
	echo $(c) | tr ' ' $(TRNL) >.clist

hlist:
	echo $(h) | tr ' ' $(TRNL) >.hlist

shlist:
	echo $(sh) | tr ' ' $(TRNL) >.shlist

# These should be automatically generated

$(plextract):
	../miniperl -I../lib $@.PL

find2perl: find2perl.PL

s2p: s2p.PL

malloc.c: ../malloc.c
	rm -f malloc.c
	sed <../malloc.c >malloc.c \
	    -e 's/"EXTERN.h"/"..\/EXTERN.h"/' \
	    -e 's/"perl.h"/"..\/perl.h"/' \
	    -e 's/my_exit/exit/' \
	    -e 's/MUTEX_[A-Z_]*(&PL_malloc_mutex);//'

# AUTOMATICALLY GENERATED MAKE DEPENDENCIES--PUT NOTHING BELOW THIS LINE
$(obj):
	@ echo "You haven't done a "'"make depend" yet!'; exit 1
makedepend: depend
