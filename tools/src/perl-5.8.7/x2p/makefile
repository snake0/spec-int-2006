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
# If this runs make out of memory, delete /usr/include lines.
hash$(OBJ_EXT): ../config.h
hash$(OBJ_EXT): ../handy.h
hash$(OBJ_EXT): /usr/include/_G_config.h
hash$(OBJ_EXT): /usr/include/alloca.h
hash$(OBJ_EXT): /usr/include/ctype.h
hash$(OBJ_EXT): /usr/include/endian.h
hash$(OBJ_EXT): /usr/include/features.h
hash$(OBJ_EXT): /usr/include/inttypes.h
hash$(OBJ_EXT): /usr/include/libio.h
hash$(OBJ_EXT): /usr/include/math.h
hash$(OBJ_EXT): /usr/include/stdint.h
hash$(OBJ_EXT): /usr/include/stdio.h
hash$(OBJ_EXT): /usr/include/stdlib.h
hash$(OBJ_EXT): /usr/include/string.h
hash$(OBJ_EXT): /usr/include/time.h
hash$(OBJ_EXT): /usr/include/wchar.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
hash$(OBJ_EXT): /usr/include/xlocale.h
hash$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
hash$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
hash$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
hash$(OBJ_EXT): EXTERN.h
hash$(OBJ_EXT): a2p.h
hash$(OBJ_EXT): hash.c
hash$(OBJ_EXT): hash.h
hash$(OBJ_EXT): str.h
hash$(OBJ_EXT): util.h
str$(OBJ_EXT): ../config.h
str$(OBJ_EXT): ../handy.h
str$(OBJ_EXT): /usr/include/_G_config.h
str$(OBJ_EXT): /usr/include/alloca.h
str$(OBJ_EXT): /usr/include/ctype.h
str$(OBJ_EXT): /usr/include/endian.h
str$(OBJ_EXT): /usr/include/features.h
str$(OBJ_EXT): /usr/include/inttypes.h
str$(OBJ_EXT): /usr/include/libio.h
str$(OBJ_EXT): /usr/include/math.h
str$(OBJ_EXT): /usr/include/stdint.h
str$(OBJ_EXT): /usr/include/stdio.h
str$(OBJ_EXT): /usr/include/stdlib.h
str$(OBJ_EXT): /usr/include/string.h
str$(OBJ_EXT): /usr/include/time.h
str$(OBJ_EXT): /usr/include/wchar.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
str$(OBJ_EXT): /usr/include/xlocale.h
str$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
str$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
str$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
str$(OBJ_EXT): EXTERN.h
str$(OBJ_EXT): a2p.h
str$(OBJ_EXT): hash.h
str$(OBJ_EXT): str.c
str$(OBJ_EXT): str.h
str$(OBJ_EXT): util.h
util$(OBJ_EXT): ../config.h
util$(OBJ_EXT): ../handy.h
util$(OBJ_EXT): /usr/include/_G_config.h
util$(OBJ_EXT): /usr/include/alloca.h
util$(OBJ_EXT): /usr/include/ctype.h
util$(OBJ_EXT): /usr/include/endian.h
util$(OBJ_EXT): /usr/include/features.h
util$(OBJ_EXT): /usr/include/inttypes.h
util$(OBJ_EXT): /usr/include/libio.h
util$(OBJ_EXT): /usr/include/math.h
util$(OBJ_EXT): /usr/include/stdint.h
util$(OBJ_EXT): /usr/include/stdio.h
util$(OBJ_EXT): /usr/include/stdlib.h
util$(OBJ_EXT): /usr/include/string.h
util$(OBJ_EXT): /usr/include/time.h
util$(OBJ_EXT): /usr/include/wchar.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
util$(OBJ_EXT): /usr/include/xlocale.h
util$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
util$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
util$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
util$(OBJ_EXT): EXTERN.h
util$(OBJ_EXT): INTERN.h
util$(OBJ_EXT): a2p.h
util$(OBJ_EXT): hash.h
util$(OBJ_EXT): str.h
util$(OBJ_EXT): util.c
util$(OBJ_EXT): util.h
walk$(OBJ_EXT): ../config.h
walk$(OBJ_EXT): ../handy.h
walk$(OBJ_EXT): /usr/include/_G_config.h
walk$(OBJ_EXT): /usr/include/alloca.h
walk$(OBJ_EXT): /usr/include/ctype.h
walk$(OBJ_EXT): /usr/include/endian.h
walk$(OBJ_EXT): /usr/include/features.h
walk$(OBJ_EXT): /usr/include/inttypes.h
walk$(OBJ_EXT): /usr/include/libio.h
walk$(OBJ_EXT): /usr/include/math.h
walk$(OBJ_EXT): /usr/include/stdint.h
walk$(OBJ_EXT): /usr/include/stdio.h
walk$(OBJ_EXT): /usr/include/stdlib.h
walk$(OBJ_EXT): /usr/include/string.h
walk$(OBJ_EXT): /usr/include/time.h
walk$(OBJ_EXT): /usr/include/wchar.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
walk$(OBJ_EXT): /usr/include/xlocale.h
walk$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
walk$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
walk$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
walk$(OBJ_EXT): EXTERN.h
walk$(OBJ_EXT): a2p.h
walk$(OBJ_EXT): hash.h
walk$(OBJ_EXT): str.h
walk$(OBJ_EXT): util.h
walk$(OBJ_EXT): walk.c
cflags: cflags.SH ../config.sh ; /bin/sh cflags.SH
# WARNING: Put nothing here or make depend will gobble it up!
