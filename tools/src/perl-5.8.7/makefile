# Makefile.SH
# This file is derived from Makefile.SH.  Any changes made here will
# be lost the next time you run Configure.
#  Makefile is used to generate makefile.  The only difference
#  is that makefile has the dependencies filled in at the end.
#
#
# I now supply perly.c with the kits, so don't remake perly.c without byacc
BYACC = byacc
CC = cc
LD = cc

LDFLAGS =  -L/var/services/homes/spparmar/spec2006/tools/output/lib -L/usr/local/lib -L/usr/lib/x86_64-linux-gnu
CLDFLAGS =  -L/var/services/homes/spparmar/spec2006/tools/output/lib -L/usr/local/lib -L/usr/lib/x86_64-linux-gnu

mallocsrc = 
mallocobj = 
LNS = /bin/ln -s
# NOTE: some systems don't grok "cp -f". XXX Configure test needed?
CPS = cp
RMS = rm -f
ranlib = :

# The following are mentioned only to make metaconfig include the
# appropriate questions in Configure.  If you want to change these,
# edit config.sh instead, or specify --man1dir=/wherever on
# installman commandline.
bin = /var/services/homes/spparmar/spec2006/tools/output/bin
scriptdir = /var/services/homes/spparmar/spec2006/tools/output/bin
shrpdir = /var/services/homes/spparmar/spec2006/tools/output/lib/perl5/5.8.7/x86_64-linux/CORE
privlib = /var/services/homes/spparmar/spec2006/tools/output/lib/perl5/5.8.7
man1dir = /var/services/homes/spparmar/spec2006/tools/output/share/man/man1
man1ext = 1
man3dir = /var/services/homes/spparmar/spec2006/tools/output/share/man/man3
man3ext = 3

# The following are used to build and install shared libraries for
# dynamic loading.
LDDLFLAGS = -shared -fPIC -L/var/services/homes/spparmar/spec2006/tools/output/lib -L/usr/local/lib -L/usr/lib/x86_64-linux-gnu
SHRPLDFLAGS = $(LDDLFLAGS)
CCDLFLAGS = -Wl,-E -Wl,-rpath,/var/services/homes/spparmar/spec2006/tools/output/lib/perl5/5.8.7/x86_64-linux/CORE
DLSUFFIX = .so
PLDLFLAGS = -fPIC -shared
LIBPERL = libperl.so
LLIBPERL= $(LIBPERL)
SHRPENV = 

# Static targets are ordinarily built without CCCDLFLAGS.  However,
# if building a shared libperl.so that might later be linked into
# another application, then it might be appropriate to also build static
# extensions (usually just DynaLoader) with relocatable code (e.g. -fPIC
# for GNU cc).  This is handled by ext/util/make_ext.
STATIC = static_pic

# The following is used to include the current directory in
# the dynamic loader path you are building a shared libperl.
LDLIBPTH = LD_LIBRARY_PATH=/var/services/homes/spparmar/spec2006/tools/src/perl-5.8.7:/var/services/homes/spparmar/spec2006/tools/src/perl-5.8.7

dynamic_ext =   lib/auto/B/B.so lib/auto/ByteLoader/ByteLoader.so lib/auto/Cwd/Cwd.so lib/auto/Data/Dumper/Dumper.so lib/auto/Devel/DProf/DProf.so lib/auto/Devel/PPPort/PPPort.so lib/auto/Devel/Peek/Peek.so lib/auto/Digest/MD5/MD5.so lib/auto/Encode/Encode.so lib/auto/Fcntl/Fcntl.so lib/auto/File/Glob/Glob.so lib/auto/Filter/Util/Call/Call.so lib/auto/I18N/Langinfo/Langinfo.so lib/auto/IO/IO.so lib/auto/IPC/SysV/SysV.so lib/auto/List/Util/Util.so lib/auto/MIME/Base64/Base64.so lib/auto/Opcode/Opcode.so lib/auto/POSIX/POSIX.so lib/auto/PerlIO/encoding/encoding.so lib/auto/PerlIO/scalar/scalar.so lib/auto/PerlIO/via/via.so lib/auto/SDBM_File/SDBM_File.so lib/auto/Socket/Socket.so lib/auto/Storable/Storable.so lib/auto/Sys/Hostname/Hostname.so lib/auto/Sys/Syslog/Syslog.so lib/auto/Time/HiRes/HiRes.so lib/auto/Unicode/Normalize/Normalize.so lib/auto/XS/APItest/APItest.so lib/auto/XS/Typemap/Typemap.so lib/auto/attrs/attrs.so lib/auto/re/re.so lib/auto/threads/threads.so lib/auto/threads/shared/shared.so
static_ext =  
nonxs_ext =   ext/Errno/pm_to_blib
ext = $(dynamic_ext) $(static_ext) $(nonxs_ext)
DYNALOADER = lib/auto/DynaLoader/DynaLoader$(LIB_EXT)

libs = -lnsl -ldl -lm -lcrypt -lutil -lc 

public = perl$(EXE_EXT)  utilities translators

shellflags = 

# This is set to  MAKE=make if your make command doesn't
# do it for you.
#

# Mention gmake here so it gets probed for by Configure.

# These variables may need to be manually set for non-Unix systems.
AR = /usr/bin/ar
EXE_EXT = 
LIB_EXT = .a
OBJ_EXT = .o
PATH_SEP = :

FIRSTMAKEFILE = makefile

# Any special object files needed by this architecture, e.g. os2/os2.obj
ARCHOBJS = 

.SUFFIXES: .c $(OBJ_EXT) .i .s

# grrr
SHELL = /bin/sh

# how to tr(anslate) newlines
TRNL = '\n'

OPTIMIZE = -O2

EXTRAS = 

INSTALLPREFIXEXP = /var/services/homes/spparmar/spec2006/tools/output


CCCMD    = `sh $(shellflags) cflags "optimize='$(OPTIMIZE)'" $@`

CCCMDSRC = `sh $(shellflags) cflags "optimize='$(OPTIMIZE)'" $<`

CONFIGPM = lib/Config.pm lib/Config_heavy.pl
private = preplibrary $(CONFIGPM) lib/ExtUtils/Miniperl.pm

# Files to be built with variable substitution before miniperl
# is available.
sh = Makefile.SH cflags.SH config_h.SH makeaperl.SH makedepend.SH \
	makedir.SH myconfig.SH writemain.SH pod/Makefile.SH

shextract = Makefile cflags config.h makeaperl makedepend \
	makedir myconfig writemain pod/Makefile

# Files to be built with variable substitution after miniperl is
# available.  Dependencies handled manually below (for now).

pl = pod/pod2html.PL pod/pod2latex.PL pod/pod2man.PL pod/pod2text.PL \
	pod/pod2usage.PL pod/podchecker.PL pod/podselect.PL

# lib/lib.pm is not listed here because it has a rule of its own.
plextract = pod/pod2html pod/pod2latex pod/pod2man pod/pod2text \
	pod/pod2usage pod/podchecker pod/podselect

addedbyconf = UU $(shextract) $(plextract) lib/lib.pm pstruct

# Unicode data files generated by mktables
unidatafiles = lib/unicore/Canonical.pl lib/unicore/Exact.pl \
	lib/unicore/Properties lib/unicore/Decomposition.pl \
	lib/unicore/CombiningClass.pl lib/unicore/Name.pl lib/unicore/PVA.pl

# Directories of Unicode data files generated by mktables
unidatadirs = lib/unicore/To lib/unicore/lib

h1 = EXTERN.h INTERN.h XSUB.h av.h config.h cop.h cv.h dosish.h
h2 = embed.h form.h gv.h handy.h hv.h keywords.h mg.h op.h opcode.h
h3 = pad.h patchlevel.h perl.h perlapi.h perly.h pp.h proto.h regcomp.h
h4 = regexp.h scope.h sv.h unixish.h util.h iperlsys.h thread.h
h5 = utf8.h warnings.h
h = $(h1) $(h2) $(h3) $(h4) $(h5)

c1 = $(mallocsrc) av.c scope.c op.c doop.c doio.c dump.c hv.c mg.c reentr.c
c2 = perl.c perly.c pp.c pp_hot.c pp_ctl.c pp_sys.c regcomp.c regexec.c utf8.c
c3 = gv.c sv.c taint.c toke.c util.c deb.c run.c universal.c xsutils.c pad.c
c4 = globals.c perlio.c perlapi.c numeric.c locale.c pp_pack.c pp_sort.c

c = $(c1) $(c2) $(c3) $(c4) miniperlmain.c perlmain.c opmini.c

obj1 = $(mallocobj) gv$(OBJ_EXT) toke$(OBJ_EXT) perly$(OBJ_EXT) op$(OBJ_EXT) pad$(OBJ_EXT) regcomp$(OBJ_EXT) dump$(OBJ_EXT) util$(OBJ_EXT) mg$(OBJ_EXT) reentr$(OBJ_EXT)
obj2 = hv$(OBJ_EXT) av$(OBJ_EXT) run$(OBJ_EXT) pp_hot$(OBJ_EXT) sv$(OBJ_EXT) pp$(OBJ_EXT) scope$(OBJ_EXT) pp_ctl$(OBJ_EXT) pp_sys$(OBJ_EXT)
obj3 = doop$(OBJ_EXT) doio$(OBJ_EXT) regexec$(OBJ_EXT) utf8$(OBJ_EXT) taint$(OBJ_EXT) deb$(OBJ_EXT) universal$(OBJ_EXT) xsutils$(OBJ_EXT) globals$(OBJ_EXT) perlio$(OBJ_EXT) perlapi$(OBJ_EXT) numeric$(OBJ_EXT) locale$(OBJ_EXT) pp_pack$(OBJ_EXT) pp_sort$(OBJ_EXT)

obj = $(obj1) $(obj2) $(obj3) $(ARCHOBJS)

lintflags = -hbvxac

.c$(OBJ_EXT):
	$(CCCMD) $(PLDLFLAGS) $*.c

.c.i:
	$(CCCMDSRC) -E $*.c > $*.i

.c.s:
	$(CCCMDSRC) -S $*.c

all: $(FIRSTMAKEFILE) miniperl$(EXE_EXT) extra.pods $(private) $(unidatafiles) $(public) $(dynamic_ext) $(nonxs_ext) extras.make
	@echo " ";
	@echo "	Everything is up to date. Type '$(MAKE) test' to run test suite."

.PHONY: all compile translators utilities

compile: all
	echo "testing compilation" > testcompile;
	cd utils;  $(MAKE) compile;
	cd x2p; $(MAKE) compile;
	cd pod; $(MAKE) compile;

translators:	miniperl$(EXE_EXT) $(CONFIGPM) FORCE
	@echo " "; echo "	Making x2p stuff"; cd x2p; $(LDLIBPTH) $(MAKE) all

utilities:	miniperl$(EXE_EXT) $(CONFIGPM) $(plextract) lib/lib.pm FORCE
	@echo " "; echo "	Making utilities"; cd utils; $(LDLIBPTH) $(MAKE) all


# This is now done by installman only if you actually want the man pages.
#	@echo " "; echo "	Making docs"; cd pod; $(MAKE) all;

# Phony target to force checking subdirectories.
# Apparently some makes require an action for the FORCE target.
.PHONY: FORCE
FORCE:
	@sh -c true

# We do a copy of the op.c instead of a symlink because gcc gets huffy
# if we have a symlink forest to another disk (it complains about too many
# levels of symbolic links, even if we have only two)

opmini.c: op.c
	$(CPS) op.c opmini.tmp
	sh mv-if-diff opmini.tmp opmini.c

opmini$(OBJ_EXT): opmini.c
	$(CCCMD) $(PLDLFLAGS) -DPERL_EXTERNAL_GLOB opmini.c

miniperlmain$(OBJ_EXT): miniperlmain.c patchlevel.h
	$(CCCMD) $(PLDLFLAGS) $*.c

perlmain.c: miniperlmain.c config.sh $(FIRSTMAKEFILE)
	sh writemain $(DYNALOADER) $(static_ext) > writemain.tmp
	sh mv-if-diff writemain.tmp perlmain.c

perlmain$(OBJ_EXT): perlmain.c
	$(CCCMD) $(PLDLFLAGS) $*.c

# The file ext.libs is a list of libraries that must be linked in
# for static extensions, e.g. -lm -lgdbm, etc.  The individual
# static extension Makefile's add to it.
ext.libs: $(static_ext)
	-@test -f ext.libs || touch ext.libs

$(LIBPERL): $& perl$(OBJ_EXT) $(obj) $(LIBPERLEXPORT)
	$(LD) -o $@ $(SHRPLDFLAGS) perl$(OBJ_EXT) $(obj) $(libs)

# How to build executables.

# The $& notation tells Sequent machines that it can do a parallel make,
# and is harmless otherwise.
# The miniperl -w -MExporter line is a basic cheap test to catch errors
# before make goes on to run preplibrary and then MakeMaker on extensions.
# This is very handy because later errors are often caused by miniperl
# build problems but that's not obvious to the novice.
# The Module used here must not depend on Config or any extensions.

miniperl$(EXE_EXT): $& miniperlmain$(OBJ_EXT) $(LIBPERL) opmini$(OBJ_EXT)
	-@rm -f miniperl.xok
	$(LDLIBPTH) $(CC) $(CLDFLAGS) -o miniperl \
	    miniperlmain$(OBJ_EXT) opmini$(OBJ_EXT) $(LLIBPERL) $(libs)
	$(LDLIBPTH) ./miniperl -w -Ilib -MExporter -e '<?>' || $(MAKE) minitest

perl$(EXE_EXT): $& perlmain$(OBJ_EXT) $(LIBPERL) $(DYNALOADER) $(static_ext) ext.libs $(PERLEXPORT)
	-@rm -f miniperl.xok
	$(SHRPENV) $(LDLIBPTH) $(CC) -o perl$(PERL_SUFFIX) $(PERL_PROFILE_LDFLAGS) $(CLDFLAGS) $(CCDLFLAGS) perlmain$(OBJ_EXT) $(DYNALOADER) $(static_ext) $(LLIBPERL) `cat ext.libs` $(libs)

# Purify/Quantify Perls.

pureperl$(EXE_EXT): $& perlmain$(OBJ_EXT) $(LIBPERL) $(DYNALOADER) $(static_ext) ext.libs $(PERLEXPORT)
	$(SHRPENV) $(LDLIBPTH) purify $(CC) -o pureperl $(CLDFLAGS) $(CCDLFLAGS) perlmain$(OBJ_EXT) $(DYNALOADER) $(static_ext) $(LLIBPERL) `cat ext.libs` $(libs)

purecovperl$(EXE_EXT): $& perlmain$(OBJ_EXT) $(LIBPERL) $(DYNALOADER) $(static_ext) ext.libs $(PERLEXPORT)
	$(SHRPENV) $(LDLIBPTH) purecov $(CC) -o purecovperl $(CLDFLAGS) $(CCDLFLAGS) perlmain$(OBJ_EXT) $(DYNALOADER) $(static_ext) $(LLIBPERL) `cat ext.libs` $(libs)

quantperl$(EXE_EXT): $& perlmain$(OBJ_EXT) $(LIBPERL) $(DYNALOADER) $(static_ext) ext.libs $(PERLEXPORT)
	$(SHRPENV) $(LDLIBPTH) quantify $(CC) -o quantperl $(CLDFLAGS) $(CCDLFLAGS) perlmain$(OBJ_EXT) $(DYNALOADER) $(static_ext) $(LLIBPERL) `cat ext.libs` $(libs)

# Valgrind perl (currently Linux only)

perl.valgrind.config: config.sh
	@echo "To build perl.valgrind you must Configure -Doptimize=-g -Uusemymalloc, checking..."
	@$(MAKE) perl.config.dashg
	@echo "Checking usemymalloc='n' in config.sh..."
	@grep "^usemymalloc="    config.sh
	@grep "^usemymalloc='n'" config.sh >/dev/null || exit 1
	@echo "And of course you have to have valgrind..."
	valgrind ./perl -e 1 2>/dev/null || exit 1

# Third Degree Perl (Tru64 only)

perl.config.dashg:
	@echo "Checking optimize='-g' in config.sh..."
	@grep "^optimize="     config.sh
	@grep "^optimize='-g'" config.sh >/dev/null || exit 1

perl.third.config: config.sh
	@echo "To build perl.third you must Configure -Doptimize=-g -Uusemymalloc, checking..."
	@$(MAKE) perl.config.dashg
	@echo "Checking usemymalloc='n' in config.sh..."
	@grep "^usemymalloc="    config.sh
	@grep "^usemymalloc='n'" config.sh >/dev/null || exit 1

perl.third: /usr/bin/atom perl.third.config perl
	atom -tool third -L. -all -gp -toolargs="-invalid -uninit heap+stack+copy -min 0" perl
	@echo "Now you may run perl.third and then study perl.3log."

# Pixie Perls (Tru64 and IRIX only)

perl.pixie.config: config.sh
	@echo "To build perl.pixie you must Configure -Doptimize=-g, checking..."
	@$(MAKE) perl.config.dashg

perl.pixie.atom: /usr/bin/atom perl
	atom -tool pixie -L. -all -toolargs="-quiet" perl

perl.pixie.irix: perl
	pixie perl

perl.pixie: /usr/bin/pixie perl.pixie.config perl
	if test -x /usr/bin/atom; then \
	  $(MAKE) perl.pixie.atom; \
	else \
	  $(MAKE) perl.pixie.irix; \
	fi
	@echo "Now you may run perl.pixie and then run pixie."

# Gprof Perl

perl.config.dashpg:
	@echo "Checking optimize='-pg' in config.sh..."
	@grep "^optimize="      config.sh
	@grep "^optimize='.*-pg.*'" config.sh >/dev/null || exit 1

perl.gprof.config: config.sh
	@echo "To build perl.gprof you must Configure -Doptimize=-pg, checking..."
	@$(MAKE) perl.config.dashpg

perl.gprof: /usr/bin/gprof perl.gprof.config
	@-rm -f perl
	$(MAKE) PERL_SUFFIX=.gprof PERL_PROFILE_LDFLAGS=-pg perl
	@echo "Now you may run perl.gprof and then run gprof perl.gprof."

# Gcov Perl

perl.config.gcov:
	@echo "To build perl.gcov you must use gcc 3.0 or newer, checking..."
	@echo "Checking gccversion in config.sh..."
	@grep "^gccversion="      config.sh
	@grep "^gccversion='[3-9]\." config.sh >/dev/null || exit 1
	@echo "To build perl.gcov you must Configure -Dccflags=-fprofile-arcs -ftest-coverage, checking..."
	@echo "Checking ccflags='-fprofile-arcs -ftest-coverage' in config.sh..."
	@grep "^ccflags="      config.sh
	@grep "^ccflags='.*-fprofile-arcs -ftest-coverage.*'" config.sh >/dev/null || exit 1

perl.gcov: perl.config.gcov
	@-rm -f perl
	$(MAKE) PERL_SUFFIX=.gcov PERL_PROFILE_LDFLAGS='' perl
	@echo "Now you may run perl.gcov and then run gcov some.c."

# Microperl.  This is just a convenience thing if one happens to
# build also the full Perl and therefore the real big Makefile:
# usually one should manually explicitly issue the below command.

.PHONY: microperl
microperl:
	$(MAKE) -f Makefile.micro

# This version, if specified in Configure, does ONLY those scripts which need
# set-id emulation.  Suidperl must be setuid root.  It contains the "taint"
# checks as well as the special code to validate that the script in question
# has been invoked correctly.

suidperl$(EXE_EXT): $& sperl$(OBJ_EXT) perlmain$(OBJ_EXT) $(LIBPERL) $(DYNALOADER) $(static_ext) ext.libs $(PERLEXPORT)
	$(SHRPENV) $(LDLIBPTH) $(CC) -o suidperl $(CLDFLAGS) $(CCDLFLAGS) perlmain$(OBJ_EXT) sperl$(OBJ_EXT) $(DYNALOADER) $(static_ext) $(LLIBPERL) `cat ext.libs` $(libs)


sperl$(OBJ_EXT): perl.c $(h)
	$(RMS) sperl.c
	$(LNS) perl.c sperl.c
	$(CCCMD) -DIAMSUID sperl.c
	$(RMS) sperl.c

# We have to call our ./makedir because Ultrix 4.3 make can't handle the line
#	test -d lib/auto || mkdir lib/auto
# We need to autosplit in two steps because VOS can't handle so many args
#
.PHONY: preplibrary
preplibrary: miniperl$(EXE_EXT) $(CONFIGPM) lib/lib.pm $(PREPLIBRARY_LIBPERL)
	@sh ./makedir lib/auto
	@echo "	AutoSplitting perl library"
	$(LDLIBPTH) ./miniperl -Ilib -e 'use AutoSplit; \
		autosplit_lib_modules(@ARGV)' lib/*.pm
	$(LDLIBPTH) ./miniperl -Ilib -e 'use AutoSplit; \
		autosplit_lib_modules(@ARGV)' lib/*/*.pm
	$(MAKE) lib/re.pm

# Take care to avoid modifying lib/Config.pm without reason
# (If trying to create a new port and having problems with the configpm script,
# try 'make minitest' and/or commenting out the tests at the end of configpm.)
$(CONFIGPM): config.sh miniperl$(EXE_EXT) configpm Porting/Glossary
	$(LDLIBPTH) ./miniperl -Ilib configpm --heavy=lib/Config_heavy.pl configpm.tmp
	sh mv-if-diff configpm.tmp lib/Config.pm

lib/ExtUtils/Miniperl.pm: miniperlmain.c miniperl$(EXE_EXT) minimod.pl $(CONFIGPM)
	$(LDLIBPTH) ./miniperl minimod.pl > minimod.tmp
	sh mv-if-diff minimod.tmp $@
	-touch lib/ExtUtils/Miniperl.pm

lib/re.pm: ext/re/re.pm
	cp ext/re/re.pm ext/re/re.tmp && sh mv-if-diff ext/re/re.tmp lib/re.pm

$(plextract):	miniperl$(EXE_EXT) $(CONFIGPM) x2p/s2p
	@-rm -f $@
	$(LDLIBPTH) ./miniperl -I`pwd`/lib $@.PL

x2p/s2p: miniperl$(EXE_EXT) $(CONFIGPM) x2p/s2p.PL
	cd x2p; $(LDLIBPTH) $(MAKE) s2p

lib/lib.pm:	miniperl$(EXE_EXT) $(CONFIGPM)
	@-rm -f $@
	$(LDLIBPTH) ./miniperl -Ilib lib/lib_pm.PL

$(unidatafiles): miniperl$(EXE_EXT) $(CONFIGPM) lib/unicore/mktables
	cd lib/unicore && $(LDLIBPTH) ../../miniperl -I../../lib mktables -w

extra.pods: miniperl$(EXE_EXT)
	-@test -f extra.pods && rm -f `cat extra.pods`
	-@rm -f extra.pods
	-@for x in `grep -l '^=[a-z]' README.* | grep -v README.vms` ; do \
	    nx=`echo $$x | sed -e "s/README\.//"`; \
	    cd pod ; $(LNS) ../$$x "perl"$$nx".pod" ; cd .. ; \
	    echo "pod/perl"$$nx".pod" >> extra.pods ; \
	done
	-@rm -f pod/perlvms.pod
	-@test -f vms/perlvms.pod && cd pod && $(LNS) ../vms/perlvms.pod perlvms.pod && cd .. && echo "pod/perlvms.pod" >> extra.pods
	-@rm -f pod/perldelta.pod
	-@test -f pod/perl587delta.pod && cd pod && $(LNS) perl587delta.pod perldelta.pod && cd .. && echo "pod/perldelta.pod" >> extra.pods # See buildtoc

extras.make: perl$(EXE_EXT)
	-@test -s extras.lst && PATH="`pwd`:${PATH}" PERL5LIB="`pwd`/lib" $(LDLIBPTH) ./perl -Ilib -MCPAN -e '@ARGV&&make(@ARGV)' `cat extras.lst`

extras.test: perl$(EXE_EXT)
	-@test -s extras.lst && PATH="`pwd`:${PATH}" PERL5LIB="`pwd`/lib" $(LDLIBPTH) ./perl -Ilib -MCPAN -e '@ARGV&&test(@ARGV)' `cat extras.lst`

extras.install: perl$(EXE_EXT)
	-@test -s extras.lst && PATH="`pwd`:${PATH}" PERL5LIB="`pwd`/lib" $(LDLIBPTH) ./perl -Ilib -MCPAN -e '@ARGV&&install(@ARGV)' `cat extras.lst`

.PHONY: install install-strip install-all install-verbose install-silent \
	no-install install.perl install.man install.html

META.yml:	Porting/makemeta Porting/Maintainers.pl Porting/Maintainers.pm
	$(LDLIBPTH) ./miniperl -Ilib Porting/makemeta

install-strip:
	$(MAKE) STRIPFLAGS=-s install DESTDIR="$(DESTDIR)"

install install-all:
	$(MAKE) install.perl install.man STRIPFLAGS=$(STRIPFLAGS) DESTDIR="$(DESTDIR)"

install-verbose:
	$(MAKE) install.perl install.man STRIPFLAGS=$(STRIPFLAGS) INSTALLFLAGS=-V DESTDIR="$(DESTDIR)"

install-silent:
	$(MAKE) install.perl install.man STRIPFLAGS=$(STRIPFLAGS) INSTALLFLAGS=-S DESTDIR="$(DESTDIR)"

no-install:
	$(MAKE) install.perl install.man STRIPFLAGS=$(STRIPFLAGS) INSTALLFLAGS=-n DESTDIR="$(DESTDIR)"

# Set this to an empty string to avoid an attempt of rebuild before install
INSTALL_DEPENDENCE = all

install.perl:	$(INSTALL_DEPENDENCE) installperl
	if [ -n "$(COMPILE)" ]; \
	then \
		cd utils; $(MAKE) compile; \
		cd ../x2p; $(MAKE) compile; \
		cd ../pod; $(MAKE) compile; \
	else :; \
	fi
	$(LDLIBPTH) ./perl installperl --destdir=$(DESTDIR) $(INSTALLFLAGS) $(STRIPFLAGS)
	$(MAKE) extras.install

install.man:	all installman
	$(LDLIBPTH) ./perl installman --destdir=$(DESTDIR) $(INSTALLFLAGS)

# XXX Experimental. Hardwired values, but useful for testing.
# Eventually Configure could ask for some of these values.
install.html: all installhtml
	-@test -f README.vms && cd vms && $(LNS) ../README.vms README_vms.pod && cd ..
	$(LDLIBPTH) ./perl installhtml                   \
      --podroot=. --podpath=. --recurse  \
      --htmldir=$(privlib)/html     \
      --htmlroot=$(privlib)/html    \
      --splithead=pod/perlipc     \
      --splititem=pod/perlfunc    \
      --libpods=perlfunc:perlguts:perlvar:perlrun:perlop \
      --verbose


# I now supply perly.c with the kits, so the following section is
# used only if you force byacc to run by saying
# 	make run_byacc
# Since we patch up the byacc output, the perly.fixer script needs
# to run with precisely the same version of byacc as I use.  You
# normally shouldn't remake perly.[ch].

.PHONY: check_byacc run_byacc

check_byacc:
	@$(BYACC) -V 2>&1 | grep 'version 1\.8\.2'

run_byacc:	FORCE check_byacc
	$(BYACC) -d perly.y
	-chmod 664 perly.c perly.h
	sh $(shellflags) ./perly.fixer y.tab.c perly.c
	sed -e 's/fprintf *( *stderr *,/PerlIO_printf(Perl_debug_log,/g' \
	    -e 's/y\.tab/perly/g' perly.c >perly.tmp && mv perly.tmp perly.c
	sed -e '/^extern YYSTYPE yy/D' y.tab.h >yh.tmp && mv yh.tmp y.tab.h
	cmp -s y.tab.h perly.h && rm -f y.tab.h || mv y.tab.h perly.h
	perl -i.old perlyline.pl perly.c
	chmod 664 vms/perly_c.vms vms/perly_h.vms
	perl vms/vms_yfix.pl perly.c perly.h vms/perly_c.vms vms/perly_h.vms

# We don't want to regenerate perly.c and perly.h, but they might
# appear out-of-date after a patch is applied or a new distribution is
# made.
perly.c: perly.y
	-@sh -c true

perly.h: perly.y
	-@sh -c true

PERLYVMS = vms/perly_c.vms vms/perly_h.vms

$(PERLYVMS):	perly.c perly.h vms/vms_yfix.pl
	perl vms/vms_yfix.pl perly.c perly.h vms/perly_c.vms vms/perly_h.vms

# No compat3.sym here since and including the 5.004_50.
# No interp.sym since 5.005_03.
SYM  = global.sym globvar.sym perlio.sym pp.sym

SYMH = perlvars.h intrpvar.h thrdvar.h

CHMOD_W = chmod +w

# The following files are generated automatically
#	autodoc.pl:	pod/perlapi.pod pod/perlintern.pod
#	bytecode.pl:	ext/ByteLoader/byterun.h ext/ByteLoader/byterun.c
#			ext/B/B/Asmdata.pm
#	embed.pl:	proto.h embed.h embedvar.h global.sym
#			perlapi.h perlapi.c 
# [* embed.pl needs pp.sym generated by opcode.pl! *]
#	keywords.pl:	keywords.h
#	opcode.pl:	opcode.h opnames.h pp_proto.h pp.sym
#	regcomp.pl:	regnodes.h
#	warnings.pl:	warnings.h lib/warnings.pm
# The correct versions should be already supplied with the perl kit,
# in case you don't have perl available.
# To force them to be regenerated, run
#       perl regen.pl
# with your existing copy of perl
# (make regen_headers is kept for backwards compatibility)

AUTOGEN_FILES = keywords.h opcode.h opnames.h pp_proto.h pp.sym proto.h \
		embed.h embedvar.h global.sym \
		pod/perlintern.pod pod/perlapi.pod \
		perlapi.h perlapi.c ext/ByteLoader/byterun.h \
		ext/ByteLoader/byterun.c ext/B/B/Asmdata.pm regnodes.h \
		warnings.h lib/warnings.pm

.PHONY: regen_headers regen_pods regen_all

regen regen_headers:	FORCE
	-perl regen.pl

regen_pods:	FORCE
	-cd pod; $(LDLIBPTH) $(MAKE) regen_pods

regen_all: $(PERLYVMS) regen regen_pods

.PHONY:	manisort manicheck

manisort:	FORCE
	LC_ALL=C sort -fdc MANIFEST || (echo "WARNING: re-sorting MANIFEST"; \
		LC_ALL=C sort -fdo MANIFEST MANIFEST)

manicheck:	FORCE
	perl Porting/manicheck

# Extensions:
# Names added to $(dynamic_ext) or $(static_ext) or $(nonxs_ext) will
# automatically get built.  There should ordinarily be no need to change
# any of this part of makefile.
#
# The dummy dependency is a place holder in case $(dynamic_ext) or
# $(static_ext) is empty.
#
# DynaLoader may be needed for extensions that use Makefile.PL.

$(DYNALOADER):	miniperl$(EXE_EXT) preplibrary FORCE
	@$(LDLIBPTH) sh ext/util/make_ext $(STATIC) $@ MAKE=$(MAKE) LIBPERL_A=$(LIBPERL)

d_dummy $(dynamic_ext):	miniperl$(EXE_EXT) preplibrary $(DYNALOADER) FORCE
	@$(LDLIBPTH) sh ext/util/make_ext dynamic $@ MAKE=$(MAKE) LIBPERL_A=$(LIBPERL)

s_dummy $(static_ext):	miniperl$(EXE_EXT) preplibrary $(DYNALOADER) FORCE
	@$(LDLIBPTH) sh ext/util/make_ext $(STATIC) $@ MAKE=$(MAKE) LIBPERL_A=$(LIBPERL)

n_dummy $(nonxs_ext):	miniperl$(EXE_EXT) preplibrary $(DYNALOADER) FORCE
	@$(LDLIBPTH) sh ext/util/make_ext nonxs $@ MAKE=$(MAKE) LIBPERL_A=$(LIBPERL)

.PHONY: clean _tidy _mopup _cleaner1 _cleaner2 \
	realclean _realcleaner clobber _clobber \
	distclean veryclean _verycleaner

clean:		_tidy _mopup

realclean:	_realcleaner _mopup
	@echo "Note that '$(MAKE) realclean' does not delete config.sh or Policy.sh"

_clobber:
	-@rm -f Cross/run-* Cross/to-* Cross/from-*
	rm -f config.sh cppstdin Policy.sh extras.lst

clobber:	_realcleaner _mopup _clobber

distclean:	clobber

# Like distclean but also removes emacs backups and *.orig.
veryclean:	_verycleaner _mopup _clobber
	-@rm -f Obsolete Wanted

# Do not 'make _mopup' directly.
_mopup:
	rm -f *$(OBJ_EXT) *$(LIB_EXT) all perlmain.c opmini.c
	-rmdir .depending
	-@test -f extra.pods && rm -f `cat extra.pods`
	-@test -f vms/README_vms.pod && rm -f vms/README_vms.pod
	-rm -f perl.exp ext.libs extra.pods opmini.o
	-rm -f perl.export perl.dll perl.libexp perl.map perl.def
	-rm -f perl.loadmap miniperl.loadmap perl.prelmap miniperl.prelmap
	-rm -f perl.third lib*.so.perl.third perl.3log t/perl.third t/perl.3log
	-rm -f perl.pixie lib*.so.perl.pixie lib*.so.Addrs
	-rm -f perl.Addrs perl.Counts t/perl.Addrs t/perl.Counts *perl.xok
	-rm -f perlld cygwin.c ld2 libperl*.def libperl*.dll cygperl*.dll *.exe.stackdump
	-rm -f perl$(EXE_EXT) suidperl$(EXE_EXT) miniperl$(EXE_EXT) $(LIBPERL) libperl.* microperl
	-rm -f opcode.h-old opnames.h-old pp.sym-old pp_proto.h-old

# Do not 'make _tidy' directly.
_tidy:
	-cd pod; $(LDLIBPTH) $(MAKE) clean
	-cd utils; $(LDLIBPTH) $(MAKE) clean
	-cd x2p; $(LDLIBPTH) $(MAKE) clean
	-@for x in $(DYNALOADER) $(dynamic_ext) $(static_ext) $(nonxs_ext) ; do \
	$(LDLIBPTH) sh ext/util/make_ext clean $$x MAKE=$(MAKE) ; \
	done
	rm -f testcompile compilelog

_cleaner1:
	-cd os2; rm -f Makefile
	-cd pod; $(LDLIBPTH) $(MAKE) $(CLEAN)
	-cd utils; $(LDLIBPTH) $(MAKE) $(CLEAN)
	-cd x2p; $(LDLIBPTH) $(MAKE) $(CLEAN)
	-@for x in $(DYNALOADER) $(dynamic_ext) $(static_ext) $(nonxs_ext) ; do \
	$(LDLIBPTH) sh ext/util/make_ext $(CLEAN) $$x MAKE=$(MAKE) ; \
	done

# Some systems do not support "?", so keep these files separate.
_cleaner2:
	-rm -f core.*perl.*.? t/core.perl.*.? .?*.c
	rm -f core *perl.core t/core t/*perl.core
	rm -f t/misctmp* t/forktmp* t/tmp* t/c t/perl$(EXE_EXT) t/rantests
	rm -f so_locations $(LIBPERL_NONSHR) $(MINIPERL_NONSHR)
	rm -rf $(addedbyconf)
	rm -f $(FIRSTMAKEFILE) $(FIRSTMAKEFILE).old makefile.old
	rm -f $(private) lib/Config.pod
	rm -rf $(unidatafiles) $(unidatadirs)
	rm -rf lib/auto
	rm -f lib/.exists lib/*/.exists lib/*/*/.exists
	rm -f h2ph.man pstruct
	rm -rf .config
	rm -f preload
	rm -f testcompile compilelog
	rm -rf lib/Encode
	-rmdir lib/B lib/Data
	-rmdir lib/Filter/Util lib/IO/Socket lib/IO
	-rmdir lib/List lib/MIME lib/Scalar lib/Sys 
	-rmdir lib/threads lib/XS

_realcleaner:
	@$(LDLIBPTH) $(MAKE) _cleaner1 CLEAN=realclean
	@$(LDLIBPTH) $(MAKE) _cleaner2

_verycleaner:
	@$(LDLIBPTH) $(MAKE) _cleaner1 CLEAN=veryclean
	@$(LDLIBPTH) $(MAKE) _cleaner2
	-rm -f *~ *.orig */*~ */*.orig */*/*~ */*/*.orig

# The following lint has practically everything turned on.  Unfortunately,
# you have to wade through a lot of mumbo jumbo that can't be suppressed.
# If the source file has a /*NOSTRICT*/ somewhere, ignore the lint message
# for that spot.

.PHONY: lint
lint: $(c)
	lint $(lintflags) $(defs) perly.c $(c) > perl.fuzz

# Need to unset during recursion to go out of loop.
# The README below ensures that the dependency list is never empty and
# that when MAKEDEPEND is empty $(FIRSTMAKEFILE) doesn't need rebuilding.

MAKEDEPEND = Makefile makedepend

$(FIRSTMAKEFILE):	README $(MAKEDEPEND)
	$(MAKE) depend MAKEDEPEND=

config.h: config_h.SH config.sh
	$(SHELL) config_h.SH

# When done, touch perlmain.c so that it doesn't get remade each time.
.PHONY: depend
depend: makedepend
	sh ./makedepend MAKE=$(MAKE)
	- test -s perlmain.c && touch perlmain.c
	cd x2p; $(MAKE) depend

# Cannot postpone this until $firstmakefile is ready ;-)
makedepend: makedepend.SH config.sh
	sh ./makedepend.SH

.PHONY: test check test_prep test_prep_nodll test_prep_pre _test_prep \
	test_tty test-tty _test_tty test_notty test-notty _test_notty \
	utest ucheck test.utf8 check.utf8 test.torture torturetest \
	test.utf16 check.utf16 utest.utf16 ucheck.utf16 \
	test.third check.third utest.third ucheck.third test_notty.third \
	test.deparse test_notty.deparse test_harness test_harness_notty \
	test.bytecompile minitest coretest test.taintwarn

# Cannot delegate rebuilding of t/perl to make
# to allow interlaced test and minitest

TESTFILE=TEST

_test_prep:
	cd t && (rm -f $(PERL)$(EXE_EXT); $(LNS) ../$(PERL)$(EXE_EXT) $(PERL)$(EXE_EXT))

# Architecture-neutral stuff:

test_prep_pre: preplibrary utilities $(nonxs_ext)

test_prep: test_prep_pre miniperl$(EXE_EXT) $(unidatafiles) perl$(EXE_EXT) $(dynamic_ext) $(TEST_PERL_DLL)
	PERL=./perl $(MAKE) _test_prep

_test_tty:
	cd t && $(PERL_DEBUG) $(LDLIBPTH) $(PERL) $(TESTFILE) $(TEST_ARGS) </dev/tty

_test_notty:
	cd t && $(PERL_DEBUG) PERL_SKIP_TTY_TEST=1 $(LDLIBPTH) $(PERL) $(TESTFILE) $(TEST_ARGS)

# The second branch is for testing without a tty or controlling terminal,
# see t/op/stat.t
_test:
	if (true </dev/tty) >/dev/null 2>&1; then \
	  $(MAKE) TEST_ARGS=$(TEST_ARGS) TESTFILE=$(TESTFILE) _test_tty   ; \
	else \
	  $(MAKE) TEST_ARGS=$(TEST_ARGS) TESTFILE=$(TESTFILE) _test_notty ; \
	fi
	@echo "Ran tests" > t/rantests

test check: test_prep
	PERL=./perl $(MAKE) _test

test_tty: test_prep
	PERL=./perl $(MAKE) _test_tty

test_notty: test_prep
	PERL=./perl $(MAKE) _test_notty

utest ucheck test.utf8 check.utf8: test_prep
	PERL=./perl TEST_ARGS=-utf8 $(MAKE) _test

coretest: test_prep
	PERL=./perl TEST_ARGS=-core $(MAKE) _test

test-prep:	test_prep

test-tty:	test_tty

test-notty:	test_notty

# Torture testing

test.torture torturetest:	test_prep
	PERL=./perl TEST_ARGS=-torture $(MAKE) _test

# Targets for UTF16 testing:

minitest.utf16: minitest.prep
	- cd t && (rm -f perl$(EXE_EXT); $(LNS) ../miniperl$(EXE_EXT) perl$(EXE_EXT)) \
		&& $(LDLIBPTH) ./perl TEST -minitest -utf16 base/*.t comp/*.t cmd/*.t run/*.t io/*.t op/*.t uni/*.t </dev/tty

test.utf16 check.utf16: test_prep
	PERL=./perl $(MAKE) TEST_ARGS=-utf16 _test

utest.utf16 ucheck.utf16: test_prep
	PERL=./perl $(MAKE) TEST_ARGS="-utf8 -utf16" _test

# Targets for valgrind testing:

test_prep.valgrind: test_prep perl.valgrind
	PERL=./perl $(MAKE) _test_prep

test.valgrind check.valgrind:	test_prep perl.valgrind.config
	PERL=./perl PERL_DEBUG=PERL_VALGRIND=1 $(MAKE) _test

utest.valgrind ucheck.valgrind: test_prep.valgrind perl.valgrind
	PERL=./perl PERL_DEBUG=PERL_VALGRIND=1 TEST_ARGS=-utf8 $(MAKE) _test

test_notty.valgrind: test_prep.valgrind perl.valgrind
	PERL=./perl $(MAKE) PERL_DEBUG=PERL_VALGRIND=1 _test_notty

# Targets for Third Degree testing.

test_prep.third: test_prep perl.third
	PERL=./perl.third $(MAKE) _test_prep

test.third check.third:	test_prep.third perl.third
	PERL=./perl.third PERL_DEBUG=PERL_3LOG=1 $(MAKE) _test

utest.third ucheck.third: test_prep.third perl.third
	PERL=./perl.third PERL_DEBUG=PERL_3LOG=1 TEST_ARGS=-utf8 $(MAKE) _test

test_notty.third: test_prep.third perl.third
	PERL=./perl.third $(MAKE) PERL_DEBUG=PERL_3LOG=1 _test_notty

# Targets for Bytecode/ByteLoader testing.

test.bytecompile:	test_prep
	PERL=./perl TEST_ARGS=-bytecompile $(MAKE) _test

# Targets for Deparse testing.

test.deparse:	test_prep
	PERL=./perl TEST_ARGS=-deparse $(MAKE) _test

test_notty.deparse:	test_prep
	PERL=./perl TEST_ARGS=-deparse $(MAKE) _test_notty

# Targets to run the test suite with -t

test.taintwarn:	test_prep
	PERL=./perl TEST_ARGS=-taintwarn $(MAKE) _test

minitest.prep:
	-@test -f lib/lib.pm && test -f lib/Config.pm || \
	  $(MAKE) lib/Config.pm lib/lib.pm $(unidatafiles)
	@echo " "
	@echo "You may see some irrelevant test failures if you have been unable"
	@echo "to build lib/Config.pm, lib/lib.pm or the Unicode data files."
	@echo " "

# Can't depend on lib/Config.pm because that might be where miniperl
# is crashing.
minitest: miniperl$(EXE_EXT) lib/re.pm minitest.prep
	- cd t && (rm -f perl$(EXE_EXT); $(LNS) ../miniperl$(EXE_EXT) perl$(EXE_EXT)) \
		&& $(LDLIBPTH) ./perl TEST -minitest base/*.t comp/*.t cmd/*.t run/*.t io/*.t op/*.t uni/*.t </dev/tty

# Test via harness

test_harness: test_prep
	PERL=./perl $(MAKE) TESTFILE=harness _test

test_harness_notty: test_prep
	PERL=./perl HARNESS_NOTTY=1 $(MAKE) TESTFILE=harness _test

# Handy way to run perlbug -ok without having to install and run the
# installed perlbug. We don't re-run the tests here - we trust the user.
# Please *don't* use this unless all tests pass.
# If you want to report test failures, use "make nok" instead.

.PHONY: ok okfile oknack okfilenack nok nokfile noknack nokfilenack

ok:	utilities
	$(LDLIBPTH) ./perl -Ilib utils/perlbug -ok -s '(UNINSTALLED)'

okfile:	utilities
	$(LDLIBPTH) ./perl -Ilib utils/perlbug -ok -s '(UNINSTALLED)' -F perl.ok

oknack:	utilities
	$(LDLIBPTH) ./perl -Ilib utils/perlbug -ok -s '(UNINSTALLED)' -A

okfilenack:	utilities
	$(LDLIBPTH) ./perl -Ilib utils/perlbug -ok -s '(UNINSTALLED)' -F perl.ok -A

nok:	utilities
	$(LDLIBPTH) ./perl -Ilib utils/perlbug -nok -s '(UNINSTALLED)'

nokfile:	utilities
	$(LDLIBPTH) ./perl -Ilib utils/perlbug -nok -s '(UNINSTALLED)' -F perl.nok

noknack:	utilities
	$(LDLIBPTH) ./perl -Ilib utils/perlbug -nok -s '(UNINSTALLED)' -A

nokfilenack:	utilities
	$(LDLIBPTH) ./perl -Ilib utils/perlbug -nok -s '(UNINSTALLED)' -F perl.nok -A

.PHONY: clist hlist shlist pllist

clist:	$(c)
	echo $(c) | tr ' ' $(TRNL) >.clist

hlist:  $(h)
	echo $(h) | tr ' ' $(TRNL) >.hlist

shlist: $(sh)
	echo $(sh) | tr ' ' $(TRNL) >.shlist

pllist: $(pl)
	echo $(pl) | tr ' ' $(TRNL) >.pllist

Makefile: Makefile.SH ./config.sh
	$(SHELL) Makefile.SH

.PHONY: distcheck
distcheck: FORCE
	perl '-MExtUtils::Manifest=&fullcheck' -e 'fullcheck()'

.PHONY: elc
elc:	emacs/cperl-mode.elc

emacs/cperl-mode.elc: emacs/cperl-mode.el
	-cd emacs; emacs -batch -q -no-site-file -f batch-byte-compile cperl-mode.el

.PHONY: etags ctags tags

etags:	TAGS

TAGS: emacs/cperl-mode.elc
	sh emacs/ptags

ctags:	tags

# Let's hope make will not go into an infinite loop on case-unsensitive systems
# This may also fail if . is in the head of the path, since perl will
# require -Ilib
tags:	TAGS
	perl emacs/e2ctags.pl TAGS > tags

# AUTOMATICALLY GENERATED MAKE DEPENDENCIES--PUT NOTHING BELOW THIS LINE
# If this runs make out of memory, delete /usr/include lines.
av$(OBJ_EXT): /usr/include/_G_config.h
av$(OBJ_EXT): /usr/include/alloca.h
av$(OBJ_EXT): /usr/include/arpa/inet.h
av$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
av$(OBJ_EXT): /usr/include/asm-generic/errno.h
av$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
av$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
av$(OBJ_EXT): /usr/include/asm-generic/param.h
av$(OBJ_EXT): /usr/include/asm-generic/socket.h
av$(OBJ_EXT): /usr/include/asm-generic/sockios.h
av$(OBJ_EXT): /usr/include/ctype.h
av$(OBJ_EXT): /usr/include/dirent.h
av$(OBJ_EXT): /usr/include/endian.h
av$(OBJ_EXT): /usr/include/errno.h
av$(OBJ_EXT): /usr/include/fcntl.h
av$(OBJ_EXT): /usr/include/features.h
av$(OBJ_EXT): /usr/include/getopt.h
av$(OBJ_EXT): /usr/include/inttypes.h
av$(OBJ_EXT): /usr/include/libio.h
av$(OBJ_EXT): /usr/include/limits.h
av$(OBJ_EXT): /usr/include/linux/errno.h
av$(OBJ_EXT): /usr/include/linux/ioctl.h
av$(OBJ_EXT): /usr/include/linux/limits.h
av$(OBJ_EXT): /usr/include/linux/param.h
av$(OBJ_EXT): /usr/include/locale.h
av$(OBJ_EXT): /usr/include/math.h
av$(OBJ_EXT): /usr/include/netdb.h
av$(OBJ_EXT): /usr/include/netinet/in.h
av$(OBJ_EXT): /usr/include/rpc/netdb.h
av$(OBJ_EXT): /usr/include/setjmp.h
av$(OBJ_EXT): /usr/include/signal.h
av$(OBJ_EXT): /usr/include/stdint.h
av$(OBJ_EXT): /usr/include/stdio.h
av$(OBJ_EXT): /usr/include/stdlib.h
av$(OBJ_EXT): /usr/include/string.h
av$(OBJ_EXT): /usr/include/time.h
av$(OBJ_EXT): /usr/include/unistd.h
av$(OBJ_EXT): /usr/include/wchar.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
av$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
av$(OBJ_EXT): /usr/include/xlocale.h
av$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
av$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
av$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
av$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
av$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
av$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
av$(OBJ_EXT): EXTERN.h
av$(OBJ_EXT): av.c
av$(OBJ_EXT): av.h
av$(OBJ_EXT): config.h
av$(OBJ_EXT): cop.h
av$(OBJ_EXT): cv.h
av$(OBJ_EXT): embed.h
av$(OBJ_EXT): embedvar.h
av$(OBJ_EXT): form.h
av$(OBJ_EXT): gv.h
av$(OBJ_EXT): handy.h
av$(OBJ_EXT): hv.h
av$(OBJ_EXT): intrpvar.h
av$(OBJ_EXT): iperlsys.h
av$(OBJ_EXT): mg.h
av$(OBJ_EXT): op.h
av$(OBJ_EXT): opcode.h
av$(OBJ_EXT): opnames.h
av$(OBJ_EXT): pad.h
av$(OBJ_EXT): patchlevel.h
av$(OBJ_EXT): perl.h
av$(OBJ_EXT): perlio.h
av$(OBJ_EXT): perlvars.h
av$(OBJ_EXT): perly.h
av$(OBJ_EXT): pp.h
av$(OBJ_EXT): pp_proto.h
av$(OBJ_EXT): proto.h
av$(OBJ_EXT): reentr.inc
av$(OBJ_EXT): regexp.h
av$(OBJ_EXT): scope.h
av$(OBJ_EXT): sv.h
av$(OBJ_EXT): thrdvar.h
av$(OBJ_EXT): thread.h
av$(OBJ_EXT): unixish.h
av$(OBJ_EXT): utf8.h
av$(OBJ_EXT): util.h
av$(OBJ_EXT): warnings.h
scope$(OBJ_EXT): /usr/include/_G_config.h
scope$(OBJ_EXT): /usr/include/alloca.h
scope$(OBJ_EXT): /usr/include/arpa/inet.h
scope$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
scope$(OBJ_EXT): /usr/include/asm-generic/errno.h
scope$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
scope$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
scope$(OBJ_EXT): /usr/include/asm-generic/param.h
scope$(OBJ_EXT): /usr/include/asm-generic/socket.h
scope$(OBJ_EXT): /usr/include/asm-generic/sockios.h
scope$(OBJ_EXT): /usr/include/ctype.h
scope$(OBJ_EXT): /usr/include/dirent.h
scope$(OBJ_EXT): /usr/include/endian.h
scope$(OBJ_EXT): /usr/include/errno.h
scope$(OBJ_EXT): /usr/include/fcntl.h
scope$(OBJ_EXT): /usr/include/features.h
scope$(OBJ_EXT): /usr/include/getopt.h
scope$(OBJ_EXT): /usr/include/inttypes.h
scope$(OBJ_EXT): /usr/include/libio.h
scope$(OBJ_EXT): /usr/include/limits.h
scope$(OBJ_EXT): /usr/include/linux/errno.h
scope$(OBJ_EXT): /usr/include/linux/ioctl.h
scope$(OBJ_EXT): /usr/include/linux/limits.h
scope$(OBJ_EXT): /usr/include/linux/param.h
scope$(OBJ_EXT): /usr/include/locale.h
scope$(OBJ_EXT): /usr/include/math.h
scope$(OBJ_EXT): /usr/include/netdb.h
scope$(OBJ_EXT): /usr/include/netinet/in.h
scope$(OBJ_EXT): /usr/include/rpc/netdb.h
scope$(OBJ_EXT): /usr/include/setjmp.h
scope$(OBJ_EXT): /usr/include/signal.h
scope$(OBJ_EXT): /usr/include/stdint.h
scope$(OBJ_EXT): /usr/include/stdio.h
scope$(OBJ_EXT): /usr/include/stdlib.h
scope$(OBJ_EXT): /usr/include/string.h
scope$(OBJ_EXT): /usr/include/time.h
scope$(OBJ_EXT): /usr/include/unistd.h
scope$(OBJ_EXT): /usr/include/wchar.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
scope$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
scope$(OBJ_EXT): /usr/include/xlocale.h
scope$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
scope$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
scope$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
scope$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
scope$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
scope$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
scope$(OBJ_EXT): EXTERN.h
scope$(OBJ_EXT): av.h
scope$(OBJ_EXT): config.h
scope$(OBJ_EXT): cop.h
scope$(OBJ_EXT): cv.h
scope$(OBJ_EXT): embed.h
scope$(OBJ_EXT): embedvar.h
scope$(OBJ_EXT): form.h
scope$(OBJ_EXT): gv.h
scope$(OBJ_EXT): handy.h
scope$(OBJ_EXT): hv.h
scope$(OBJ_EXT): intrpvar.h
scope$(OBJ_EXT): iperlsys.h
scope$(OBJ_EXT): mg.h
scope$(OBJ_EXT): op.h
scope$(OBJ_EXT): opcode.h
scope$(OBJ_EXT): opnames.h
scope$(OBJ_EXT): pad.h
scope$(OBJ_EXT): patchlevel.h
scope$(OBJ_EXT): perl.h
scope$(OBJ_EXT): perlio.h
scope$(OBJ_EXT): perlvars.h
scope$(OBJ_EXT): perly.h
scope$(OBJ_EXT): pp.h
scope$(OBJ_EXT): pp_proto.h
scope$(OBJ_EXT): proto.h
scope$(OBJ_EXT): reentr.inc
scope$(OBJ_EXT): regexp.h
scope$(OBJ_EXT): scope.c
scope$(OBJ_EXT): scope.h
scope$(OBJ_EXT): sv.h
scope$(OBJ_EXT): thrdvar.h
scope$(OBJ_EXT): thread.h
scope$(OBJ_EXT): unixish.h
scope$(OBJ_EXT): utf8.h
scope$(OBJ_EXT): util.h
scope$(OBJ_EXT): warnings.h
op$(OBJ_EXT): /usr/include/_G_config.h
op$(OBJ_EXT): /usr/include/alloca.h
op$(OBJ_EXT): /usr/include/arpa/inet.h
op$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
op$(OBJ_EXT): /usr/include/asm-generic/errno.h
op$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
op$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
op$(OBJ_EXT): /usr/include/asm-generic/param.h
op$(OBJ_EXT): /usr/include/asm-generic/socket.h
op$(OBJ_EXT): /usr/include/asm-generic/sockios.h
op$(OBJ_EXT): /usr/include/ctype.h
op$(OBJ_EXT): /usr/include/dirent.h
op$(OBJ_EXT): /usr/include/endian.h
op$(OBJ_EXT): /usr/include/errno.h
op$(OBJ_EXT): /usr/include/fcntl.h
op$(OBJ_EXT): /usr/include/features.h
op$(OBJ_EXT): /usr/include/getopt.h
op$(OBJ_EXT): /usr/include/inttypes.h
op$(OBJ_EXT): /usr/include/libio.h
op$(OBJ_EXT): /usr/include/limits.h
op$(OBJ_EXT): /usr/include/linux/errno.h
op$(OBJ_EXT): /usr/include/linux/ioctl.h
op$(OBJ_EXT): /usr/include/linux/limits.h
op$(OBJ_EXT): /usr/include/linux/param.h
op$(OBJ_EXT): /usr/include/locale.h
op$(OBJ_EXT): /usr/include/math.h
op$(OBJ_EXT): /usr/include/netdb.h
op$(OBJ_EXT): /usr/include/netinet/in.h
op$(OBJ_EXT): /usr/include/rpc/netdb.h
op$(OBJ_EXT): /usr/include/setjmp.h
op$(OBJ_EXT): /usr/include/signal.h
op$(OBJ_EXT): /usr/include/stdint.h
op$(OBJ_EXT): /usr/include/stdio.h
op$(OBJ_EXT): /usr/include/stdlib.h
op$(OBJ_EXT): /usr/include/string.h
op$(OBJ_EXT): /usr/include/time.h
op$(OBJ_EXT): /usr/include/unistd.h
op$(OBJ_EXT): /usr/include/wchar.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
op$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
op$(OBJ_EXT): /usr/include/xlocale.h
op$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
op$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
op$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
op$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
op$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
op$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
op$(OBJ_EXT): EXTERN.h
op$(OBJ_EXT): XSUB.h
op$(OBJ_EXT): av.h
op$(OBJ_EXT): config.h
op$(OBJ_EXT): cop.h
op$(OBJ_EXT): cv.h
op$(OBJ_EXT): embed.h
op$(OBJ_EXT): embedvar.h
op$(OBJ_EXT): form.h
op$(OBJ_EXT): gv.h
op$(OBJ_EXT): handy.h
op$(OBJ_EXT): hv.h
op$(OBJ_EXT): intrpvar.h
op$(OBJ_EXT): iperlsys.h
op$(OBJ_EXT): keywords.h
op$(OBJ_EXT): mg.h
op$(OBJ_EXT): op.c
op$(OBJ_EXT): op.h
op$(OBJ_EXT): opcode.h
op$(OBJ_EXT): opnames.h
op$(OBJ_EXT): pad.h
op$(OBJ_EXT): patchlevel.h
op$(OBJ_EXT): perl.h
op$(OBJ_EXT): perlapi.h
op$(OBJ_EXT): perlio.h
op$(OBJ_EXT): perlvars.h
op$(OBJ_EXT): perly.h
op$(OBJ_EXT): pp.h
op$(OBJ_EXT): pp_proto.h
op$(OBJ_EXT): proto.h
op$(OBJ_EXT): reentr.inc
op$(OBJ_EXT): regexp.h
op$(OBJ_EXT): scope.h
op$(OBJ_EXT): sv.h
op$(OBJ_EXT): thrdvar.h
op$(OBJ_EXT): thread.h
op$(OBJ_EXT): unixish.h
op$(OBJ_EXT): utf8.h
op$(OBJ_EXT): util.h
op$(OBJ_EXT): warnings.h
doop$(OBJ_EXT): /usr/include/_G_config.h
doop$(OBJ_EXT): /usr/include/alloca.h
doop$(OBJ_EXT): /usr/include/arpa/inet.h
doop$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
doop$(OBJ_EXT): /usr/include/asm-generic/errno.h
doop$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
doop$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
doop$(OBJ_EXT): /usr/include/asm-generic/param.h
doop$(OBJ_EXT): /usr/include/asm-generic/socket.h
doop$(OBJ_EXT): /usr/include/asm-generic/sockios.h
doop$(OBJ_EXT): /usr/include/ctype.h
doop$(OBJ_EXT): /usr/include/dirent.h
doop$(OBJ_EXT): /usr/include/endian.h
doop$(OBJ_EXT): /usr/include/errno.h
doop$(OBJ_EXT): /usr/include/fcntl.h
doop$(OBJ_EXT): /usr/include/features.h
doop$(OBJ_EXT): /usr/include/getopt.h
doop$(OBJ_EXT): /usr/include/inttypes.h
doop$(OBJ_EXT): /usr/include/libio.h
doop$(OBJ_EXT): /usr/include/limits.h
doop$(OBJ_EXT): /usr/include/linux/errno.h
doop$(OBJ_EXT): /usr/include/linux/ioctl.h
doop$(OBJ_EXT): /usr/include/linux/limits.h
doop$(OBJ_EXT): /usr/include/linux/param.h
doop$(OBJ_EXT): /usr/include/locale.h
doop$(OBJ_EXT): /usr/include/math.h
doop$(OBJ_EXT): /usr/include/netdb.h
doop$(OBJ_EXT): /usr/include/netinet/in.h
doop$(OBJ_EXT): /usr/include/rpc/netdb.h
doop$(OBJ_EXT): /usr/include/setjmp.h
doop$(OBJ_EXT): /usr/include/signal.h
doop$(OBJ_EXT): /usr/include/stdint.h
doop$(OBJ_EXT): /usr/include/stdio.h
doop$(OBJ_EXT): /usr/include/stdlib.h
doop$(OBJ_EXT): /usr/include/string.h
doop$(OBJ_EXT): /usr/include/time.h
doop$(OBJ_EXT): /usr/include/unistd.h
doop$(OBJ_EXT): /usr/include/wchar.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
doop$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
doop$(OBJ_EXT): /usr/include/xlocale.h
doop$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
doop$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
doop$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
doop$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
doop$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
doop$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
doop$(OBJ_EXT): EXTERN.h
doop$(OBJ_EXT): av.h
doop$(OBJ_EXT): config.h
doop$(OBJ_EXT): cop.h
doop$(OBJ_EXT): cv.h
doop$(OBJ_EXT): doop.c
doop$(OBJ_EXT): embed.h
doop$(OBJ_EXT): embedvar.h
doop$(OBJ_EXT): form.h
doop$(OBJ_EXT): gv.h
doop$(OBJ_EXT): handy.h
doop$(OBJ_EXT): hv.h
doop$(OBJ_EXT): intrpvar.h
doop$(OBJ_EXT): iperlsys.h
doop$(OBJ_EXT): mg.h
doop$(OBJ_EXT): op.h
doop$(OBJ_EXT): opcode.h
doop$(OBJ_EXT): opnames.h
doop$(OBJ_EXT): pad.h
doop$(OBJ_EXT): patchlevel.h
doop$(OBJ_EXT): perl.h
doop$(OBJ_EXT): perlio.h
doop$(OBJ_EXT): perlvars.h
doop$(OBJ_EXT): perly.h
doop$(OBJ_EXT): pp.h
doop$(OBJ_EXT): pp_proto.h
doop$(OBJ_EXT): proto.h
doop$(OBJ_EXT): reentr.inc
doop$(OBJ_EXT): regexp.h
doop$(OBJ_EXT): scope.h
doop$(OBJ_EXT): sv.h
doop$(OBJ_EXT): thrdvar.h
doop$(OBJ_EXT): thread.h
doop$(OBJ_EXT): unixish.h
doop$(OBJ_EXT): utf8.h
doop$(OBJ_EXT): util.h
doop$(OBJ_EXT): warnings.h
doio$(OBJ_EXT): /usr/include/_G_config.h
doio$(OBJ_EXT): /usr/include/alloca.h
doio$(OBJ_EXT): /usr/include/arpa/inet.h
doio$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
doio$(OBJ_EXT): /usr/include/asm-generic/errno.h
doio$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
doio$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
doio$(OBJ_EXT): /usr/include/asm-generic/param.h
doio$(OBJ_EXT): /usr/include/asm-generic/socket.h
doio$(OBJ_EXT): /usr/include/asm-generic/sockios.h
doio$(OBJ_EXT): /usr/include/ctype.h
doio$(OBJ_EXT): /usr/include/dirent.h
doio$(OBJ_EXT): /usr/include/endian.h
doio$(OBJ_EXT): /usr/include/errno.h
doio$(OBJ_EXT): /usr/include/fcntl.h
doio$(OBJ_EXT): /usr/include/features.h
doio$(OBJ_EXT): /usr/include/getopt.h
doio$(OBJ_EXT): /usr/include/inttypes.h
doio$(OBJ_EXT): /usr/include/libio.h
doio$(OBJ_EXT): /usr/include/limits.h
doio$(OBJ_EXT): /usr/include/linux/errno.h
doio$(OBJ_EXT): /usr/include/linux/ioctl.h
doio$(OBJ_EXT): /usr/include/linux/limits.h
doio$(OBJ_EXT): /usr/include/linux/param.h
doio$(OBJ_EXT): /usr/include/locale.h
doio$(OBJ_EXT): /usr/include/math.h
doio$(OBJ_EXT): /usr/include/netdb.h
doio$(OBJ_EXT): /usr/include/netinet/in.h
doio$(OBJ_EXT): /usr/include/rpc/netdb.h
doio$(OBJ_EXT): /usr/include/setjmp.h
doio$(OBJ_EXT): /usr/include/signal.h
doio$(OBJ_EXT): /usr/include/stdint.h
doio$(OBJ_EXT): /usr/include/stdio.h
doio$(OBJ_EXT): /usr/include/stdlib.h
doio$(OBJ_EXT): /usr/include/string.h
doio$(OBJ_EXT): /usr/include/time.h
doio$(OBJ_EXT): /usr/include/unistd.h
doio$(OBJ_EXT): /usr/include/utime.h
doio$(OBJ_EXT): /usr/include/wchar.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/msq.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/shm.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/msg.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/shm.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
doio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
doio$(OBJ_EXT): /usr/include/xlocale.h
doio$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
doio$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
doio$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
doio$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
doio$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
doio$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
doio$(OBJ_EXT): EXTERN.h
doio$(OBJ_EXT): av.h
doio$(OBJ_EXT): config.h
doio$(OBJ_EXT): cop.h
doio$(OBJ_EXT): cv.h
doio$(OBJ_EXT): doio.c
doio$(OBJ_EXT): embed.h
doio$(OBJ_EXT): embedvar.h
doio$(OBJ_EXT): form.h
doio$(OBJ_EXT): gv.h
doio$(OBJ_EXT): handy.h
doio$(OBJ_EXT): hv.h
doio$(OBJ_EXT): intrpvar.h
doio$(OBJ_EXT): iperlsys.h
doio$(OBJ_EXT): mg.h
doio$(OBJ_EXT): op.h
doio$(OBJ_EXT): opcode.h
doio$(OBJ_EXT): opnames.h
doio$(OBJ_EXT): pad.h
doio$(OBJ_EXT): patchlevel.h
doio$(OBJ_EXT): perl.h
doio$(OBJ_EXT): perlio.h
doio$(OBJ_EXT): perlvars.h
doio$(OBJ_EXT): perly.h
doio$(OBJ_EXT): pp.h
doio$(OBJ_EXT): pp_proto.h
doio$(OBJ_EXT): proto.h
doio$(OBJ_EXT): reentr.inc
doio$(OBJ_EXT): regexp.h
doio$(OBJ_EXT): scope.h
doio$(OBJ_EXT): sv.h
doio$(OBJ_EXT): thrdvar.h
doio$(OBJ_EXT): thread.h
doio$(OBJ_EXT): unixish.h
doio$(OBJ_EXT): utf8.h
doio$(OBJ_EXT): util.h
doio$(OBJ_EXT): warnings.h
dump$(OBJ_EXT): /usr/include/_G_config.h
dump$(OBJ_EXT): /usr/include/alloca.h
dump$(OBJ_EXT): /usr/include/arpa/inet.h
dump$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
dump$(OBJ_EXT): /usr/include/asm-generic/errno.h
dump$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
dump$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
dump$(OBJ_EXT): /usr/include/asm-generic/param.h
dump$(OBJ_EXT): /usr/include/asm-generic/socket.h
dump$(OBJ_EXT): /usr/include/asm-generic/sockios.h
dump$(OBJ_EXT): /usr/include/ctype.h
dump$(OBJ_EXT): /usr/include/dirent.h
dump$(OBJ_EXT): /usr/include/endian.h
dump$(OBJ_EXT): /usr/include/errno.h
dump$(OBJ_EXT): /usr/include/fcntl.h
dump$(OBJ_EXT): /usr/include/features.h
dump$(OBJ_EXT): /usr/include/getopt.h
dump$(OBJ_EXT): /usr/include/inttypes.h
dump$(OBJ_EXT): /usr/include/libio.h
dump$(OBJ_EXT): /usr/include/limits.h
dump$(OBJ_EXT): /usr/include/linux/errno.h
dump$(OBJ_EXT): /usr/include/linux/ioctl.h
dump$(OBJ_EXT): /usr/include/linux/limits.h
dump$(OBJ_EXT): /usr/include/linux/param.h
dump$(OBJ_EXT): /usr/include/locale.h
dump$(OBJ_EXT): /usr/include/math.h
dump$(OBJ_EXT): /usr/include/netdb.h
dump$(OBJ_EXT): /usr/include/netinet/in.h
dump$(OBJ_EXT): /usr/include/rpc/netdb.h
dump$(OBJ_EXT): /usr/include/setjmp.h
dump$(OBJ_EXT): /usr/include/signal.h
dump$(OBJ_EXT): /usr/include/stdint.h
dump$(OBJ_EXT): /usr/include/stdio.h
dump$(OBJ_EXT): /usr/include/stdlib.h
dump$(OBJ_EXT): /usr/include/string.h
dump$(OBJ_EXT): /usr/include/time.h
dump$(OBJ_EXT): /usr/include/unistd.h
dump$(OBJ_EXT): /usr/include/wchar.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
dump$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
dump$(OBJ_EXT): /usr/include/xlocale.h
dump$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
dump$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
dump$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
dump$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
dump$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
dump$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
dump$(OBJ_EXT): EXTERN.h
dump$(OBJ_EXT): av.h
dump$(OBJ_EXT): config.h
dump$(OBJ_EXT): cop.h
dump$(OBJ_EXT): cv.h
dump$(OBJ_EXT): dump.c
dump$(OBJ_EXT): embed.h
dump$(OBJ_EXT): embedvar.h
dump$(OBJ_EXT): form.h
dump$(OBJ_EXT): gv.h
dump$(OBJ_EXT): handy.h
dump$(OBJ_EXT): hv.h
dump$(OBJ_EXT): intrpvar.h
dump$(OBJ_EXT): iperlsys.h
dump$(OBJ_EXT): mg.h
dump$(OBJ_EXT): op.h
dump$(OBJ_EXT): opcode.h
dump$(OBJ_EXT): opnames.h
dump$(OBJ_EXT): pad.h
dump$(OBJ_EXT): patchlevel.h
dump$(OBJ_EXT): perl.h
dump$(OBJ_EXT): perlio.h
dump$(OBJ_EXT): perlvars.h
dump$(OBJ_EXT): perly.h
dump$(OBJ_EXT): pp.h
dump$(OBJ_EXT): pp_proto.h
dump$(OBJ_EXT): proto.h
dump$(OBJ_EXT): reentr.inc
dump$(OBJ_EXT): regcomp.h
dump$(OBJ_EXT): regexp.h
dump$(OBJ_EXT): regnodes.h
dump$(OBJ_EXT): scope.h
dump$(OBJ_EXT): sv.h
dump$(OBJ_EXT): thrdvar.h
dump$(OBJ_EXT): thread.h
dump$(OBJ_EXT): unixish.h
dump$(OBJ_EXT): utf8.h
dump$(OBJ_EXT): util.h
dump$(OBJ_EXT): warnings.h
hv$(OBJ_EXT): /usr/include/_G_config.h
hv$(OBJ_EXT): /usr/include/alloca.h
hv$(OBJ_EXT): /usr/include/arpa/inet.h
hv$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
hv$(OBJ_EXT): /usr/include/asm-generic/errno.h
hv$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
hv$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
hv$(OBJ_EXT): /usr/include/asm-generic/param.h
hv$(OBJ_EXT): /usr/include/asm-generic/socket.h
hv$(OBJ_EXT): /usr/include/asm-generic/sockios.h
hv$(OBJ_EXT): /usr/include/ctype.h
hv$(OBJ_EXT): /usr/include/dirent.h
hv$(OBJ_EXT): /usr/include/endian.h
hv$(OBJ_EXT): /usr/include/errno.h
hv$(OBJ_EXT): /usr/include/fcntl.h
hv$(OBJ_EXT): /usr/include/features.h
hv$(OBJ_EXT): /usr/include/getopt.h
hv$(OBJ_EXT): /usr/include/inttypes.h
hv$(OBJ_EXT): /usr/include/libio.h
hv$(OBJ_EXT): /usr/include/limits.h
hv$(OBJ_EXT): /usr/include/linux/errno.h
hv$(OBJ_EXT): /usr/include/linux/ioctl.h
hv$(OBJ_EXT): /usr/include/linux/limits.h
hv$(OBJ_EXT): /usr/include/linux/param.h
hv$(OBJ_EXT): /usr/include/locale.h
hv$(OBJ_EXT): /usr/include/math.h
hv$(OBJ_EXT): /usr/include/netdb.h
hv$(OBJ_EXT): /usr/include/netinet/in.h
hv$(OBJ_EXT): /usr/include/rpc/netdb.h
hv$(OBJ_EXT): /usr/include/setjmp.h
hv$(OBJ_EXT): /usr/include/signal.h
hv$(OBJ_EXT): /usr/include/stdint.h
hv$(OBJ_EXT): /usr/include/stdio.h
hv$(OBJ_EXT): /usr/include/stdlib.h
hv$(OBJ_EXT): /usr/include/string.h
hv$(OBJ_EXT): /usr/include/time.h
hv$(OBJ_EXT): /usr/include/unistd.h
hv$(OBJ_EXT): /usr/include/wchar.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
hv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
hv$(OBJ_EXT): /usr/include/xlocale.h
hv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
hv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
hv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
hv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
hv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
hv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
hv$(OBJ_EXT): EXTERN.h
hv$(OBJ_EXT): av.h
hv$(OBJ_EXT): config.h
hv$(OBJ_EXT): cop.h
hv$(OBJ_EXT): cv.h
hv$(OBJ_EXT): embed.h
hv$(OBJ_EXT): embedvar.h
hv$(OBJ_EXT): form.h
hv$(OBJ_EXT): gv.h
hv$(OBJ_EXT): handy.h
hv$(OBJ_EXT): hv.c
hv$(OBJ_EXT): hv.h
hv$(OBJ_EXT): intrpvar.h
hv$(OBJ_EXT): iperlsys.h
hv$(OBJ_EXT): mg.h
hv$(OBJ_EXT): op.h
hv$(OBJ_EXT): opcode.h
hv$(OBJ_EXT): opnames.h
hv$(OBJ_EXT): pad.h
hv$(OBJ_EXT): patchlevel.h
hv$(OBJ_EXT): perl.h
hv$(OBJ_EXT): perlio.h
hv$(OBJ_EXT): perlvars.h
hv$(OBJ_EXT): perly.h
hv$(OBJ_EXT): pp.h
hv$(OBJ_EXT): pp_proto.h
hv$(OBJ_EXT): proto.h
hv$(OBJ_EXT): reentr.inc
hv$(OBJ_EXT): regexp.h
hv$(OBJ_EXT): scope.h
hv$(OBJ_EXT): sv.h
hv$(OBJ_EXT): thrdvar.h
hv$(OBJ_EXT): thread.h
hv$(OBJ_EXT): unixish.h
hv$(OBJ_EXT): utf8.h
hv$(OBJ_EXT): util.h
hv$(OBJ_EXT): warnings.h
mg$(OBJ_EXT): /usr/include/_G_config.h
mg$(OBJ_EXT): /usr/include/alloca.h
mg$(OBJ_EXT): /usr/include/arpa/inet.h
mg$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
mg$(OBJ_EXT): /usr/include/asm-generic/errno.h
mg$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
mg$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
mg$(OBJ_EXT): /usr/include/asm-generic/param.h
mg$(OBJ_EXT): /usr/include/asm-generic/socket.h
mg$(OBJ_EXT): /usr/include/asm-generic/sockios.h
mg$(OBJ_EXT): /usr/include/ctype.h
mg$(OBJ_EXT): /usr/include/dirent.h
mg$(OBJ_EXT): /usr/include/endian.h
mg$(OBJ_EXT): /usr/include/errno.h
mg$(OBJ_EXT): /usr/include/fcntl.h
mg$(OBJ_EXT): /usr/include/features.h
mg$(OBJ_EXT): /usr/include/getopt.h
mg$(OBJ_EXT): /usr/include/grp.h
mg$(OBJ_EXT): /usr/include/inttypes.h
mg$(OBJ_EXT): /usr/include/libio.h
mg$(OBJ_EXT): /usr/include/limits.h
mg$(OBJ_EXT): /usr/include/linux/errno.h
mg$(OBJ_EXT): /usr/include/linux/ioctl.h
mg$(OBJ_EXT): /usr/include/linux/limits.h
mg$(OBJ_EXT): /usr/include/linux/param.h
mg$(OBJ_EXT): /usr/include/locale.h
mg$(OBJ_EXT): /usr/include/math.h
mg$(OBJ_EXT): /usr/include/netdb.h
mg$(OBJ_EXT): /usr/include/netinet/in.h
mg$(OBJ_EXT): /usr/include/rpc/netdb.h
mg$(OBJ_EXT): /usr/include/setjmp.h
mg$(OBJ_EXT): /usr/include/signal.h
mg$(OBJ_EXT): /usr/include/stdint.h
mg$(OBJ_EXT): /usr/include/stdio.h
mg$(OBJ_EXT): /usr/include/stdlib.h
mg$(OBJ_EXT): /usr/include/string.h
mg$(OBJ_EXT): /usr/include/time.h
mg$(OBJ_EXT): /usr/include/unistd.h
mg$(OBJ_EXT): /usr/include/wchar.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
mg$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
mg$(OBJ_EXT): /usr/include/xlocale.h
mg$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
mg$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
mg$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
mg$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
mg$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
mg$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
mg$(OBJ_EXT): EXTERN.h
mg$(OBJ_EXT): av.h
mg$(OBJ_EXT): config.h
mg$(OBJ_EXT): cop.h
mg$(OBJ_EXT): cv.h
mg$(OBJ_EXT): embed.h
mg$(OBJ_EXT): embedvar.h
mg$(OBJ_EXT): form.h
mg$(OBJ_EXT): gv.h
mg$(OBJ_EXT): handy.h
mg$(OBJ_EXT): hv.h
mg$(OBJ_EXT): intrpvar.h
mg$(OBJ_EXT): iperlsys.h
mg$(OBJ_EXT): mg.c
mg$(OBJ_EXT): mg.h
mg$(OBJ_EXT): op.h
mg$(OBJ_EXT): opcode.h
mg$(OBJ_EXT): opnames.h
mg$(OBJ_EXT): pad.h
mg$(OBJ_EXT): patchlevel.h
mg$(OBJ_EXT): perl.h
mg$(OBJ_EXT): perlio.h
mg$(OBJ_EXT): perlvars.h
mg$(OBJ_EXT): perly.h
mg$(OBJ_EXT): pp.h
mg$(OBJ_EXT): pp_proto.h
mg$(OBJ_EXT): proto.h
mg$(OBJ_EXT): reentr.inc
mg$(OBJ_EXT): regexp.h
mg$(OBJ_EXT): scope.h
mg$(OBJ_EXT): sv.h
mg$(OBJ_EXT): thrdvar.h
mg$(OBJ_EXT): thread.h
mg$(OBJ_EXT): unixish.h
mg$(OBJ_EXT): utf8.h
mg$(OBJ_EXT): util.h
mg$(OBJ_EXT): warnings.h
reentr$(OBJ_EXT): /usr/include/_G_config.h
reentr$(OBJ_EXT): /usr/include/alloca.h
reentr$(OBJ_EXT): /usr/include/arpa/inet.h
reentr$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
reentr$(OBJ_EXT): /usr/include/asm-generic/errno.h
reentr$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
reentr$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
reentr$(OBJ_EXT): /usr/include/asm-generic/param.h
reentr$(OBJ_EXT): /usr/include/asm-generic/socket.h
reentr$(OBJ_EXT): /usr/include/asm-generic/sockios.h
reentr$(OBJ_EXT): /usr/include/ctype.h
reentr$(OBJ_EXT): /usr/include/dirent.h
reentr$(OBJ_EXT): /usr/include/endian.h
reentr$(OBJ_EXT): /usr/include/errno.h
reentr$(OBJ_EXT): /usr/include/fcntl.h
reentr$(OBJ_EXT): /usr/include/features.h
reentr$(OBJ_EXT): /usr/include/getopt.h
reentr$(OBJ_EXT): /usr/include/inttypes.h
reentr$(OBJ_EXT): /usr/include/libio.h
reentr$(OBJ_EXT): /usr/include/limits.h
reentr$(OBJ_EXT): /usr/include/linux/errno.h
reentr$(OBJ_EXT): /usr/include/linux/ioctl.h
reentr$(OBJ_EXT): /usr/include/linux/limits.h
reentr$(OBJ_EXT): /usr/include/linux/param.h
reentr$(OBJ_EXT): /usr/include/locale.h
reentr$(OBJ_EXT): /usr/include/math.h
reentr$(OBJ_EXT): /usr/include/netdb.h
reentr$(OBJ_EXT): /usr/include/netinet/in.h
reentr$(OBJ_EXT): /usr/include/rpc/netdb.h
reentr$(OBJ_EXT): /usr/include/setjmp.h
reentr$(OBJ_EXT): /usr/include/signal.h
reentr$(OBJ_EXT): /usr/include/stdint.h
reentr$(OBJ_EXT): /usr/include/stdio.h
reentr$(OBJ_EXT): /usr/include/stdlib.h
reentr$(OBJ_EXT): /usr/include/string.h
reentr$(OBJ_EXT): /usr/include/time.h
reentr$(OBJ_EXT): /usr/include/unistd.h
reentr$(OBJ_EXT): /usr/include/wchar.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
reentr$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
reentr$(OBJ_EXT): /usr/include/xlocale.h
reentr$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
reentr$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
reentr$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
reentr$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
reentr$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
reentr$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
reentr$(OBJ_EXT): EXTERN.h
reentr$(OBJ_EXT): av.h
reentr$(OBJ_EXT): config.h
reentr$(OBJ_EXT): cop.h
reentr$(OBJ_EXT): cv.h
reentr$(OBJ_EXT): embed.h
reentr$(OBJ_EXT): embedvar.h
reentr$(OBJ_EXT): form.h
reentr$(OBJ_EXT): gv.h
reentr$(OBJ_EXT): handy.h
reentr$(OBJ_EXT): hv.h
reentr$(OBJ_EXT): intrpvar.h
reentr$(OBJ_EXT): iperlsys.h
reentr$(OBJ_EXT): mg.h
reentr$(OBJ_EXT): op.h
reentr$(OBJ_EXT): opcode.h
reentr$(OBJ_EXT): opnames.h
reentr$(OBJ_EXT): pad.h
reentr$(OBJ_EXT): patchlevel.h
reentr$(OBJ_EXT): perl.h
reentr$(OBJ_EXT): perlio.h
reentr$(OBJ_EXT): perlvars.h
reentr$(OBJ_EXT): perly.h
reentr$(OBJ_EXT): pp.h
reentr$(OBJ_EXT): pp_proto.h
reentr$(OBJ_EXT): proto.h
reentr$(OBJ_EXT): reentr.c
reentr$(OBJ_EXT): reentr.h
reentr$(OBJ_EXT): reentr.inc
reentr$(OBJ_EXT): regexp.h
reentr$(OBJ_EXT): scope.h
reentr$(OBJ_EXT): sv.h
reentr$(OBJ_EXT): thrdvar.h
reentr$(OBJ_EXT): thread.h
reentr$(OBJ_EXT): unixish.h
reentr$(OBJ_EXT): utf8.h
reentr$(OBJ_EXT): util.h
reentr$(OBJ_EXT): warnings.h
perl$(OBJ_EXT): /usr/include/_G_config.h
perl$(OBJ_EXT): /usr/include/alloca.h
perl$(OBJ_EXT): /usr/include/arpa/inet.h
perl$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
perl$(OBJ_EXT): /usr/include/asm-generic/errno.h
perl$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
perl$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
perl$(OBJ_EXT): /usr/include/asm-generic/param.h
perl$(OBJ_EXT): /usr/include/asm-generic/socket.h
perl$(OBJ_EXT): /usr/include/asm-generic/sockios.h
perl$(OBJ_EXT): /usr/include/ctype.h
perl$(OBJ_EXT): /usr/include/dirent.h
perl$(OBJ_EXT): /usr/include/endian.h
perl$(OBJ_EXT): /usr/include/errno.h
perl$(OBJ_EXT): /usr/include/fcntl.h
perl$(OBJ_EXT): /usr/include/features.h
perl$(OBJ_EXT): /usr/include/getopt.h
perl$(OBJ_EXT): /usr/include/inttypes.h
perl$(OBJ_EXT): /usr/include/libio.h
perl$(OBJ_EXT): /usr/include/limits.h
perl$(OBJ_EXT): /usr/include/linux/errno.h
perl$(OBJ_EXT): /usr/include/linux/ioctl.h
perl$(OBJ_EXT): /usr/include/linux/limits.h
perl$(OBJ_EXT): /usr/include/linux/param.h
perl$(OBJ_EXT): /usr/include/locale.h
perl$(OBJ_EXT): /usr/include/math.h
perl$(OBJ_EXT): /usr/include/netdb.h
perl$(OBJ_EXT): /usr/include/netinet/in.h
perl$(OBJ_EXT): /usr/include/rpc/netdb.h
perl$(OBJ_EXT): /usr/include/setjmp.h
perl$(OBJ_EXT): /usr/include/signal.h
perl$(OBJ_EXT): /usr/include/stdint.h
perl$(OBJ_EXT): /usr/include/stdio.h
perl$(OBJ_EXT): /usr/include/stdlib.h
perl$(OBJ_EXT): /usr/include/string.h
perl$(OBJ_EXT): /usr/include/time.h
perl$(OBJ_EXT): /usr/include/unistd.h
perl$(OBJ_EXT): /usr/include/wchar.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
perl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
perl$(OBJ_EXT): /usr/include/xlocale.h
perl$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
perl$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
perl$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
perl$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
perl$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
perl$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
perl$(OBJ_EXT): EXTERN.h
perl$(OBJ_EXT): av.h
perl$(OBJ_EXT): config.h
perl$(OBJ_EXT): cop.h
perl$(OBJ_EXT): cv.h
perl$(OBJ_EXT): embed.h
perl$(OBJ_EXT): embedvar.h
perl$(OBJ_EXT): form.h
perl$(OBJ_EXT): gv.h
perl$(OBJ_EXT): handy.h
perl$(OBJ_EXT): hv.h
perl$(OBJ_EXT): intrpvar.h
perl$(OBJ_EXT): iperlsys.h
perl$(OBJ_EXT): mg.h
perl$(OBJ_EXT): op.h
perl$(OBJ_EXT): opcode.h
perl$(OBJ_EXT): opnames.h
perl$(OBJ_EXT): pad.h
perl$(OBJ_EXT): patchlevel.h
perl$(OBJ_EXT): perl.c
perl$(OBJ_EXT): perl.h
perl$(OBJ_EXT): perlio.h
perl$(OBJ_EXT): perlvars.h
perl$(OBJ_EXT): perly.h
perl$(OBJ_EXT): pp.h
perl$(OBJ_EXT): pp_proto.h
perl$(OBJ_EXT): proto.h
perl$(OBJ_EXT): reentr.inc
perl$(OBJ_EXT): regexp.h
perl$(OBJ_EXT): scope.h
perl$(OBJ_EXT): sv.h
perl$(OBJ_EXT): thrdvar.h
perl$(OBJ_EXT): thread.h
perl$(OBJ_EXT): unixish.h
perl$(OBJ_EXT): utf8.h
perl$(OBJ_EXT): util.h
perl$(OBJ_EXT): warnings.h
perly$(OBJ_EXT): /usr/include/_G_config.h
perly$(OBJ_EXT): /usr/include/alloca.h
perly$(OBJ_EXT): /usr/include/arpa/inet.h
perly$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
perly$(OBJ_EXT): /usr/include/asm-generic/errno.h
perly$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
perly$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
perly$(OBJ_EXT): /usr/include/asm-generic/param.h
perly$(OBJ_EXT): /usr/include/asm-generic/socket.h
perly$(OBJ_EXT): /usr/include/asm-generic/sockios.h
perly$(OBJ_EXT): /usr/include/ctype.h
perly$(OBJ_EXT): /usr/include/dirent.h
perly$(OBJ_EXT): /usr/include/endian.h
perly$(OBJ_EXT): /usr/include/errno.h
perly$(OBJ_EXT): /usr/include/fcntl.h
perly$(OBJ_EXT): /usr/include/features.h
perly$(OBJ_EXT): /usr/include/getopt.h
perly$(OBJ_EXT): /usr/include/inttypes.h
perly$(OBJ_EXT): /usr/include/libio.h
perly$(OBJ_EXT): /usr/include/limits.h
perly$(OBJ_EXT): /usr/include/linux/errno.h
perly$(OBJ_EXT): /usr/include/linux/ioctl.h
perly$(OBJ_EXT): /usr/include/linux/limits.h
perly$(OBJ_EXT): /usr/include/linux/param.h
perly$(OBJ_EXT): /usr/include/locale.h
perly$(OBJ_EXT): /usr/include/math.h
perly$(OBJ_EXT): /usr/include/netdb.h
perly$(OBJ_EXT): /usr/include/netinet/in.h
perly$(OBJ_EXT): /usr/include/rpc/netdb.h
perly$(OBJ_EXT): /usr/include/setjmp.h
perly$(OBJ_EXT): /usr/include/signal.h
perly$(OBJ_EXT): /usr/include/stdint.h
perly$(OBJ_EXT): /usr/include/stdio.h
perly$(OBJ_EXT): /usr/include/stdlib.h
perly$(OBJ_EXT): /usr/include/string.h
perly$(OBJ_EXT): /usr/include/time.h
perly$(OBJ_EXT): /usr/include/unistd.h
perly$(OBJ_EXT): /usr/include/wchar.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
perly$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
perly$(OBJ_EXT): /usr/include/xlocale.h
perly$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
perly$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
perly$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
perly$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
perly$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
perly$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
perly$(OBJ_EXT): EXTERN.h
perly$(OBJ_EXT): av.h
perly$(OBJ_EXT): config.h
perly$(OBJ_EXT): cop.h
perly$(OBJ_EXT): cv.h
perly$(OBJ_EXT): embed.h
perly$(OBJ_EXT): embedvar.h
perly$(OBJ_EXT): form.h
perly$(OBJ_EXT): gv.h
perly$(OBJ_EXT): handy.h
perly$(OBJ_EXT): hv.h
perly$(OBJ_EXT): intrpvar.h
perly$(OBJ_EXT): iperlsys.h
perly$(OBJ_EXT): mg.h
perly$(OBJ_EXT): op.h
perly$(OBJ_EXT): opcode.h
perly$(OBJ_EXT): opnames.h
perly$(OBJ_EXT): pad.h
perly$(OBJ_EXT): patchlevel.h
perly$(OBJ_EXT): perl.h
perly$(OBJ_EXT): perlio.h
perly$(OBJ_EXT): perlvars.h
perly$(OBJ_EXT): perly.c
perly$(OBJ_EXT): perly.h
perly$(OBJ_EXT): pp.h
perly$(OBJ_EXT): pp_proto.h
perly$(OBJ_EXT): proto.h
perly$(OBJ_EXT): reentr.inc
perly$(OBJ_EXT): regexp.h
perly$(OBJ_EXT): scope.h
perly$(OBJ_EXT): sv.h
perly$(OBJ_EXT): thrdvar.h
perly$(OBJ_EXT): thread.h
perly$(OBJ_EXT): unixish.h
perly$(OBJ_EXT): utf8.h
perly$(OBJ_EXT): util.h
perly$(OBJ_EXT): warnings.h
pp$(OBJ_EXT): /usr/include/_G_config.h
pp$(OBJ_EXT): /usr/include/alloca.h
pp$(OBJ_EXT): /usr/include/arpa/inet.h
pp$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
pp$(OBJ_EXT): /usr/include/asm-generic/errno.h
pp$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
pp$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
pp$(OBJ_EXT): /usr/include/asm-generic/param.h
pp$(OBJ_EXT): /usr/include/asm-generic/socket.h
pp$(OBJ_EXT): /usr/include/asm-generic/sockios.h
pp$(OBJ_EXT): /usr/include/ctype.h
pp$(OBJ_EXT): /usr/include/dirent.h
pp$(OBJ_EXT): /usr/include/endian.h
pp$(OBJ_EXT): /usr/include/errno.h
pp$(OBJ_EXT): /usr/include/fcntl.h
pp$(OBJ_EXT): /usr/include/features.h
pp$(OBJ_EXT): /usr/include/getopt.h
pp$(OBJ_EXT): /usr/include/inttypes.h
pp$(OBJ_EXT): /usr/include/libio.h
pp$(OBJ_EXT): /usr/include/limits.h
pp$(OBJ_EXT): /usr/include/linux/errno.h
pp$(OBJ_EXT): /usr/include/linux/ioctl.h
pp$(OBJ_EXT): /usr/include/linux/limits.h
pp$(OBJ_EXT): /usr/include/linux/param.h
pp$(OBJ_EXT): /usr/include/locale.h
pp$(OBJ_EXT): /usr/include/math.h
pp$(OBJ_EXT): /usr/include/netdb.h
pp$(OBJ_EXT): /usr/include/netinet/in.h
pp$(OBJ_EXT): /usr/include/rpc/netdb.h
pp$(OBJ_EXT): /usr/include/setjmp.h
pp$(OBJ_EXT): /usr/include/signal.h
pp$(OBJ_EXT): /usr/include/stdint.h
pp$(OBJ_EXT): /usr/include/stdio.h
pp$(OBJ_EXT): /usr/include/stdlib.h
pp$(OBJ_EXT): /usr/include/string.h
pp$(OBJ_EXT): /usr/include/time.h
pp$(OBJ_EXT): /usr/include/unistd.h
pp$(OBJ_EXT): /usr/include/wchar.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
pp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
pp$(OBJ_EXT): /usr/include/xlocale.h
pp$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
pp$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
pp$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
pp$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
pp$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
pp$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
pp$(OBJ_EXT): EXTERN.h
pp$(OBJ_EXT): av.h
pp$(OBJ_EXT): config.h
pp$(OBJ_EXT): cop.h
pp$(OBJ_EXT): cv.h
pp$(OBJ_EXT): embed.h
pp$(OBJ_EXT): embedvar.h
pp$(OBJ_EXT): form.h
pp$(OBJ_EXT): gv.h
pp$(OBJ_EXT): handy.h
pp$(OBJ_EXT): hv.h
pp$(OBJ_EXT): intrpvar.h
pp$(OBJ_EXT): iperlsys.h
pp$(OBJ_EXT): keywords.h
pp$(OBJ_EXT): mg.h
pp$(OBJ_EXT): op.h
pp$(OBJ_EXT): opcode.h
pp$(OBJ_EXT): opnames.h
pp$(OBJ_EXT): pad.h
pp$(OBJ_EXT): patchlevel.h
pp$(OBJ_EXT): perl.h
pp$(OBJ_EXT): perlio.h
pp$(OBJ_EXT): perlvars.h
pp$(OBJ_EXT): perly.h
pp$(OBJ_EXT): pp.c
pp$(OBJ_EXT): pp.h
pp$(OBJ_EXT): pp_proto.h
pp$(OBJ_EXT): proto.h
pp$(OBJ_EXT): reentr.h
pp$(OBJ_EXT): reentr.inc
pp$(OBJ_EXT): regexp.h
pp$(OBJ_EXT): scope.h
pp$(OBJ_EXT): sv.h
pp$(OBJ_EXT): thrdvar.h
pp$(OBJ_EXT): thread.h
pp$(OBJ_EXT): unixish.h
pp$(OBJ_EXT): utf8.h
pp$(OBJ_EXT): util.h
pp$(OBJ_EXT): warnings.h
pp_hot$(OBJ_EXT): /usr/include/_G_config.h
pp_hot$(OBJ_EXT): /usr/include/alloca.h
pp_hot$(OBJ_EXT): /usr/include/arpa/inet.h
pp_hot$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
pp_hot$(OBJ_EXT): /usr/include/asm-generic/errno.h
pp_hot$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
pp_hot$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
pp_hot$(OBJ_EXT): /usr/include/asm-generic/param.h
pp_hot$(OBJ_EXT): /usr/include/asm-generic/socket.h
pp_hot$(OBJ_EXT): /usr/include/asm-generic/sockios.h
pp_hot$(OBJ_EXT): /usr/include/ctype.h
pp_hot$(OBJ_EXT): /usr/include/dirent.h
pp_hot$(OBJ_EXT): /usr/include/endian.h
pp_hot$(OBJ_EXT): /usr/include/errno.h
pp_hot$(OBJ_EXT): /usr/include/fcntl.h
pp_hot$(OBJ_EXT): /usr/include/features.h
pp_hot$(OBJ_EXT): /usr/include/getopt.h
pp_hot$(OBJ_EXT): /usr/include/inttypes.h
pp_hot$(OBJ_EXT): /usr/include/libio.h
pp_hot$(OBJ_EXT): /usr/include/limits.h
pp_hot$(OBJ_EXT): /usr/include/linux/errno.h
pp_hot$(OBJ_EXT): /usr/include/linux/ioctl.h
pp_hot$(OBJ_EXT): /usr/include/linux/limits.h
pp_hot$(OBJ_EXT): /usr/include/linux/param.h
pp_hot$(OBJ_EXT): /usr/include/locale.h
pp_hot$(OBJ_EXT): /usr/include/math.h
pp_hot$(OBJ_EXT): /usr/include/netdb.h
pp_hot$(OBJ_EXT): /usr/include/netinet/in.h
pp_hot$(OBJ_EXT): /usr/include/rpc/netdb.h
pp_hot$(OBJ_EXT): /usr/include/setjmp.h
pp_hot$(OBJ_EXT): /usr/include/signal.h
pp_hot$(OBJ_EXT): /usr/include/stdint.h
pp_hot$(OBJ_EXT): /usr/include/stdio.h
pp_hot$(OBJ_EXT): /usr/include/stdlib.h
pp_hot$(OBJ_EXT): /usr/include/string.h
pp_hot$(OBJ_EXT): /usr/include/time.h
pp_hot$(OBJ_EXT): /usr/include/unistd.h
pp_hot$(OBJ_EXT): /usr/include/wchar.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
pp_hot$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
pp_hot$(OBJ_EXT): /usr/include/xlocale.h
pp_hot$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
pp_hot$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
pp_hot$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
pp_hot$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
pp_hot$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
pp_hot$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
pp_hot$(OBJ_EXT): EXTERN.h
pp_hot$(OBJ_EXT): av.h
pp_hot$(OBJ_EXT): config.h
pp_hot$(OBJ_EXT): cop.h
pp_hot$(OBJ_EXT): cv.h
pp_hot$(OBJ_EXT): embed.h
pp_hot$(OBJ_EXT): embedvar.h
pp_hot$(OBJ_EXT): form.h
pp_hot$(OBJ_EXT): gv.h
pp_hot$(OBJ_EXT): handy.h
pp_hot$(OBJ_EXT): hv.h
pp_hot$(OBJ_EXT): intrpvar.h
pp_hot$(OBJ_EXT): iperlsys.h
pp_hot$(OBJ_EXT): mg.h
pp_hot$(OBJ_EXT): op.h
pp_hot$(OBJ_EXT): opcode.h
pp_hot$(OBJ_EXT): opnames.h
pp_hot$(OBJ_EXT): pad.h
pp_hot$(OBJ_EXT): patchlevel.h
pp_hot$(OBJ_EXT): perl.h
pp_hot$(OBJ_EXT): perlio.h
pp_hot$(OBJ_EXT): perlvars.h
pp_hot$(OBJ_EXT): perly.h
pp_hot$(OBJ_EXT): pp.h
pp_hot$(OBJ_EXT): pp_hot.c
pp_hot$(OBJ_EXT): pp_proto.h
pp_hot$(OBJ_EXT): proto.h
pp_hot$(OBJ_EXT): reentr.inc
pp_hot$(OBJ_EXT): regexp.h
pp_hot$(OBJ_EXT): scope.h
pp_hot$(OBJ_EXT): sv.h
pp_hot$(OBJ_EXT): thrdvar.h
pp_hot$(OBJ_EXT): thread.h
pp_hot$(OBJ_EXT): unixish.h
pp_hot$(OBJ_EXT): utf8.h
pp_hot$(OBJ_EXT): util.h
pp_hot$(OBJ_EXT): warnings.h
pp_ctl$(OBJ_EXT): /usr/include/_G_config.h
pp_ctl$(OBJ_EXT): /usr/include/alloca.h
pp_ctl$(OBJ_EXT): /usr/include/arpa/inet.h
pp_ctl$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
pp_ctl$(OBJ_EXT): /usr/include/asm-generic/errno.h
pp_ctl$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
pp_ctl$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
pp_ctl$(OBJ_EXT): /usr/include/asm-generic/param.h
pp_ctl$(OBJ_EXT): /usr/include/asm-generic/socket.h
pp_ctl$(OBJ_EXT): /usr/include/asm-generic/sockios.h
pp_ctl$(OBJ_EXT): /usr/include/ctype.h
pp_ctl$(OBJ_EXT): /usr/include/dirent.h
pp_ctl$(OBJ_EXT): /usr/include/endian.h
pp_ctl$(OBJ_EXT): /usr/include/errno.h
pp_ctl$(OBJ_EXT): /usr/include/fcntl.h
pp_ctl$(OBJ_EXT): /usr/include/features.h
pp_ctl$(OBJ_EXT): /usr/include/getopt.h
pp_ctl$(OBJ_EXT): /usr/include/inttypes.h
pp_ctl$(OBJ_EXT): /usr/include/libio.h
pp_ctl$(OBJ_EXT): /usr/include/limits.h
pp_ctl$(OBJ_EXT): /usr/include/linux/errno.h
pp_ctl$(OBJ_EXT): /usr/include/linux/ioctl.h
pp_ctl$(OBJ_EXT): /usr/include/linux/limits.h
pp_ctl$(OBJ_EXT): /usr/include/linux/param.h
pp_ctl$(OBJ_EXT): /usr/include/locale.h
pp_ctl$(OBJ_EXT): /usr/include/math.h
pp_ctl$(OBJ_EXT): /usr/include/netdb.h
pp_ctl$(OBJ_EXT): /usr/include/netinet/in.h
pp_ctl$(OBJ_EXT): /usr/include/rpc/netdb.h
pp_ctl$(OBJ_EXT): /usr/include/setjmp.h
pp_ctl$(OBJ_EXT): /usr/include/signal.h
pp_ctl$(OBJ_EXT): /usr/include/stdint.h
pp_ctl$(OBJ_EXT): /usr/include/stdio.h
pp_ctl$(OBJ_EXT): /usr/include/stdlib.h
pp_ctl$(OBJ_EXT): /usr/include/string.h
pp_ctl$(OBJ_EXT): /usr/include/time.h
pp_ctl$(OBJ_EXT): /usr/include/unistd.h
pp_ctl$(OBJ_EXT): /usr/include/wchar.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
pp_ctl$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
pp_ctl$(OBJ_EXT): /usr/include/xlocale.h
pp_ctl$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
pp_ctl$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
pp_ctl$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
pp_ctl$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
pp_ctl$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
pp_ctl$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
pp_ctl$(OBJ_EXT): EXTERN.h
pp_ctl$(OBJ_EXT): av.h
pp_ctl$(OBJ_EXT): config.h
pp_ctl$(OBJ_EXT): cop.h
pp_ctl$(OBJ_EXT): cv.h
pp_ctl$(OBJ_EXT): embed.h
pp_ctl$(OBJ_EXT): embedvar.h
pp_ctl$(OBJ_EXT): form.h
pp_ctl$(OBJ_EXT): gv.h
pp_ctl$(OBJ_EXT): handy.h
pp_ctl$(OBJ_EXT): hv.h
pp_ctl$(OBJ_EXT): intrpvar.h
pp_ctl$(OBJ_EXT): iperlsys.h
pp_ctl$(OBJ_EXT): mg.h
pp_ctl$(OBJ_EXT): op.h
pp_ctl$(OBJ_EXT): opcode.h
pp_ctl$(OBJ_EXT): opnames.h
pp_ctl$(OBJ_EXT): pad.h
pp_ctl$(OBJ_EXT): patchlevel.h
pp_ctl$(OBJ_EXT): perl.h
pp_ctl$(OBJ_EXT): perlio.h
pp_ctl$(OBJ_EXT): perlvars.h
pp_ctl$(OBJ_EXT): perly.h
pp_ctl$(OBJ_EXT): pp.h
pp_ctl$(OBJ_EXT): pp_ctl.c
pp_ctl$(OBJ_EXT): pp_proto.h
pp_ctl$(OBJ_EXT): proto.h
pp_ctl$(OBJ_EXT): reentr.inc
pp_ctl$(OBJ_EXT): regexp.h
pp_ctl$(OBJ_EXT): scope.h
pp_ctl$(OBJ_EXT): sv.h
pp_ctl$(OBJ_EXT): thrdvar.h
pp_ctl$(OBJ_EXT): thread.h
pp_ctl$(OBJ_EXT): unixish.h
pp_ctl$(OBJ_EXT): utf8.h
pp_ctl$(OBJ_EXT): util.h
pp_ctl$(OBJ_EXT): warnings.h
pp_sys$(OBJ_EXT): /usr/include/_G_config.h
pp_sys$(OBJ_EXT): /usr/include/alloca.h
pp_sys$(OBJ_EXT): /usr/include/arpa/inet.h
pp_sys$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
pp_sys$(OBJ_EXT): /usr/include/asm-generic/errno.h
pp_sys$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
pp_sys$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
pp_sys$(OBJ_EXT): /usr/include/asm-generic/param.h
pp_sys$(OBJ_EXT): /usr/include/asm-generic/socket.h
pp_sys$(OBJ_EXT): /usr/include/asm-generic/sockios.h
pp_sys$(OBJ_EXT): /usr/include/ctype.h
pp_sys$(OBJ_EXT): /usr/include/dirent.h
pp_sys$(OBJ_EXT): /usr/include/endian.h
pp_sys$(OBJ_EXT): /usr/include/errno.h
pp_sys$(OBJ_EXT): /usr/include/fcntl.h
pp_sys$(OBJ_EXT): /usr/include/features.h
pp_sys$(OBJ_EXT): /usr/include/getopt.h
pp_sys$(OBJ_EXT): /usr/include/grp.h
pp_sys$(OBJ_EXT): /usr/include/inttypes.h
pp_sys$(OBJ_EXT): /usr/include/libio.h
pp_sys$(OBJ_EXT): /usr/include/limits.h
pp_sys$(OBJ_EXT): /usr/include/linux/errno.h
pp_sys$(OBJ_EXT): /usr/include/linux/ioctl.h
pp_sys$(OBJ_EXT): /usr/include/linux/limits.h
pp_sys$(OBJ_EXT): /usr/include/linux/param.h
pp_sys$(OBJ_EXT): /usr/include/locale.h
pp_sys$(OBJ_EXT): /usr/include/math.h
pp_sys$(OBJ_EXT): /usr/include/netdb.h
pp_sys$(OBJ_EXT): /usr/include/netinet/in.h
pp_sys$(OBJ_EXT): /usr/include/paths.h
pp_sys$(OBJ_EXT): /usr/include/pwd.h
pp_sys$(OBJ_EXT): /usr/include/rpc/netdb.h
pp_sys$(OBJ_EXT): /usr/include/setjmp.h
pp_sys$(OBJ_EXT): /usr/include/shadow.h
pp_sys$(OBJ_EXT): /usr/include/signal.h
pp_sys$(OBJ_EXT): /usr/include/stdint.h
pp_sys$(OBJ_EXT): /usr/include/stdio.h
pp_sys$(OBJ_EXT): /usr/include/stdlib.h
pp_sys$(OBJ_EXT): /usr/include/string.h
pp_sys$(OBJ_EXT): /usr/include/time.h
pp_sys$(OBJ_EXT): /usr/include/unistd.h
pp_sys$(OBJ_EXT): /usr/include/utime.h
pp_sys$(OBJ_EXT): /usr/include/wchar.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/resource.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/resource.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
pp_sys$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/wait.h
pp_sys$(OBJ_EXT): /usr/include/xlocale.h
pp_sys$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
pp_sys$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
pp_sys$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
pp_sys$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
pp_sys$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
pp_sys$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
pp_sys$(OBJ_EXT): EXTERN.h
pp_sys$(OBJ_EXT): av.h
pp_sys$(OBJ_EXT): config.h
pp_sys$(OBJ_EXT): cop.h
pp_sys$(OBJ_EXT): cv.h
pp_sys$(OBJ_EXT): embed.h
pp_sys$(OBJ_EXT): embedvar.h
pp_sys$(OBJ_EXT): form.h
pp_sys$(OBJ_EXT): gv.h
pp_sys$(OBJ_EXT): handy.h
pp_sys$(OBJ_EXT): hv.h
pp_sys$(OBJ_EXT): intrpvar.h
pp_sys$(OBJ_EXT): iperlsys.h
pp_sys$(OBJ_EXT): mg.h
pp_sys$(OBJ_EXT): op.h
pp_sys$(OBJ_EXT): opcode.h
pp_sys$(OBJ_EXT): opnames.h
pp_sys$(OBJ_EXT): pad.h
pp_sys$(OBJ_EXT): patchlevel.h
pp_sys$(OBJ_EXT): perl.h
pp_sys$(OBJ_EXT): perlio.h
pp_sys$(OBJ_EXT): perlvars.h
pp_sys$(OBJ_EXT): perly.h
pp_sys$(OBJ_EXT): pp.h
pp_sys$(OBJ_EXT): pp_proto.h
pp_sys$(OBJ_EXT): pp_sys.c
pp_sys$(OBJ_EXT): proto.h
pp_sys$(OBJ_EXT): reentr.h
pp_sys$(OBJ_EXT): reentr.inc
pp_sys$(OBJ_EXT): regexp.h
pp_sys$(OBJ_EXT): scope.h
pp_sys$(OBJ_EXT): sv.h
pp_sys$(OBJ_EXT): thrdvar.h
pp_sys$(OBJ_EXT): thread.h
pp_sys$(OBJ_EXT): unixish.h
pp_sys$(OBJ_EXT): utf8.h
pp_sys$(OBJ_EXT): util.h
pp_sys$(OBJ_EXT): warnings.h
regcomp$(OBJ_EXT): /usr/include/_G_config.h
regcomp$(OBJ_EXT): /usr/include/alloca.h
regcomp$(OBJ_EXT): /usr/include/arpa/inet.h
regcomp$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
regcomp$(OBJ_EXT): /usr/include/asm-generic/errno.h
regcomp$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
regcomp$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
regcomp$(OBJ_EXT): /usr/include/asm-generic/param.h
regcomp$(OBJ_EXT): /usr/include/asm-generic/socket.h
regcomp$(OBJ_EXT): /usr/include/asm-generic/sockios.h
regcomp$(OBJ_EXT): /usr/include/ctype.h
regcomp$(OBJ_EXT): /usr/include/dirent.h
regcomp$(OBJ_EXT): /usr/include/endian.h
regcomp$(OBJ_EXT): /usr/include/errno.h
regcomp$(OBJ_EXT): /usr/include/fcntl.h
regcomp$(OBJ_EXT): /usr/include/features.h
regcomp$(OBJ_EXT): /usr/include/getopt.h
regcomp$(OBJ_EXT): /usr/include/inttypes.h
regcomp$(OBJ_EXT): /usr/include/libio.h
regcomp$(OBJ_EXT): /usr/include/limits.h
regcomp$(OBJ_EXT): /usr/include/linux/errno.h
regcomp$(OBJ_EXT): /usr/include/linux/ioctl.h
regcomp$(OBJ_EXT): /usr/include/linux/limits.h
regcomp$(OBJ_EXT): /usr/include/linux/param.h
regcomp$(OBJ_EXT): /usr/include/locale.h
regcomp$(OBJ_EXT): /usr/include/math.h
regcomp$(OBJ_EXT): /usr/include/netdb.h
regcomp$(OBJ_EXT): /usr/include/netinet/in.h
regcomp$(OBJ_EXT): /usr/include/rpc/netdb.h
regcomp$(OBJ_EXT): /usr/include/setjmp.h
regcomp$(OBJ_EXT): /usr/include/signal.h
regcomp$(OBJ_EXT): /usr/include/stdint.h
regcomp$(OBJ_EXT): /usr/include/stdio.h
regcomp$(OBJ_EXT): /usr/include/stdlib.h
regcomp$(OBJ_EXT): /usr/include/string.h
regcomp$(OBJ_EXT): /usr/include/time.h
regcomp$(OBJ_EXT): /usr/include/unistd.h
regcomp$(OBJ_EXT): /usr/include/wchar.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
regcomp$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
regcomp$(OBJ_EXT): /usr/include/xlocale.h
regcomp$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
regcomp$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
regcomp$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
regcomp$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
regcomp$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
regcomp$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
regcomp$(OBJ_EXT): EXTERN.h
regcomp$(OBJ_EXT): INTERN.h
regcomp$(OBJ_EXT): av.h
regcomp$(OBJ_EXT): config.h
regcomp$(OBJ_EXT): cop.h
regcomp$(OBJ_EXT): cv.h
regcomp$(OBJ_EXT): embed.h
regcomp$(OBJ_EXT): embedvar.h
regcomp$(OBJ_EXT): form.h
regcomp$(OBJ_EXT): gv.h
regcomp$(OBJ_EXT): handy.h
regcomp$(OBJ_EXT): hv.h
regcomp$(OBJ_EXT): intrpvar.h
regcomp$(OBJ_EXT): iperlsys.h
regcomp$(OBJ_EXT): mg.h
regcomp$(OBJ_EXT): op.h
regcomp$(OBJ_EXT): opcode.h
regcomp$(OBJ_EXT): opnames.h
regcomp$(OBJ_EXT): pad.h
regcomp$(OBJ_EXT): patchlevel.h
regcomp$(OBJ_EXT): perl.h
regcomp$(OBJ_EXT): perlio.h
regcomp$(OBJ_EXT): perlvars.h
regcomp$(OBJ_EXT): perly.h
regcomp$(OBJ_EXT): pp.h
regcomp$(OBJ_EXT): pp_proto.h
regcomp$(OBJ_EXT): proto.h
regcomp$(OBJ_EXT): reentr.inc
regcomp$(OBJ_EXT): regcomp.c
regcomp$(OBJ_EXT): regcomp.h
regcomp$(OBJ_EXT): regexp.h
regcomp$(OBJ_EXT): regnodes.h
regcomp$(OBJ_EXT): scope.h
regcomp$(OBJ_EXT): sv.h
regcomp$(OBJ_EXT): thrdvar.h
regcomp$(OBJ_EXT): thread.h
regcomp$(OBJ_EXT): unixish.h
regcomp$(OBJ_EXT): utf8.h
regcomp$(OBJ_EXT): util.h
regcomp$(OBJ_EXT): warnings.h
regexec$(OBJ_EXT): /usr/include/_G_config.h
regexec$(OBJ_EXT): /usr/include/alloca.h
regexec$(OBJ_EXT): /usr/include/arpa/inet.h
regexec$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
regexec$(OBJ_EXT): /usr/include/asm-generic/errno.h
regexec$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
regexec$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
regexec$(OBJ_EXT): /usr/include/asm-generic/param.h
regexec$(OBJ_EXT): /usr/include/asm-generic/socket.h
regexec$(OBJ_EXT): /usr/include/asm-generic/sockios.h
regexec$(OBJ_EXT): /usr/include/ctype.h
regexec$(OBJ_EXT): /usr/include/dirent.h
regexec$(OBJ_EXT): /usr/include/endian.h
regexec$(OBJ_EXT): /usr/include/errno.h
regexec$(OBJ_EXT): /usr/include/fcntl.h
regexec$(OBJ_EXT): /usr/include/features.h
regexec$(OBJ_EXT): /usr/include/getopt.h
regexec$(OBJ_EXT): /usr/include/inttypes.h
regexec$(OBJ_EXT): /usr/include/libio.h
regexec$(OBJ_EXT): /usr/include/limits.h
regexec$(OBJ_EXT): /usr/include/linux/errno.h
regexec$(OBJ_EXT): /usr/include/linux/ioctl.h
regexec$(OBJ_EXT): /usr/include/linux/limits.h
regexec$(OBJ_EXT): /usr/include/linux/param.h
regexec$(OBJ_EXT): /usr/include/locale.h
regexec$(OBJ_EXT): /usr/include/math.h
regexec$(OBJ_EXT): /usr/include/netdb.h
regexec$(OBJ_EXT): /usr/include/netinet/in.h
regexec$(OBJ_EXT): /usr/include/rpc/netdb.h
regexec$(OBJ_EXT): /usr/include/setjmp.h
regexec$(OBJ_EXT): /usr/include/signal.h
regexec$(OBJ_EXT): /usr/include/stdint.h
regexec$(OBJ_EXT): /usr/include/stdio.h
regexec$(OBJ_EXT): /usr/include/stdlib.h
regexec$(OBJ_EXT): /usr/include/string.h
regexec$(OBJ_EXT): /usr/include/time.h
regexec$(OBJ_EXT): /usr/include/unistd.h
regexec$(OBJ_EXT): /usr/include/wchar.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
regexec$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
regexec$(OBJ_EXT): /usr/include/xlocale.h
regexec$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
regexec$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
regexec$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
regexec$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
regexec$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
regexec$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
regexec$(OBJ_EXT): EXTERN.h
regexec$(OBJ_EXT): av.h
regexec$(OBJ_EXT): config.h
regexec$(OBJ_EXT): cop.h
regexec$(OBJ_EXT): cv.h
regexec$(OBJ_EXT): embed.h
regexec$(OBJ_EXT): embedvar.h
regexec$(OBJ_EXT): form.h
regexec$(OBJ_EXT): gv.h
regexec$(OBJ_EXT): handy.h
regexec$(OBJ_EXT): hv.h
regexec$(OBJ_EXT): intrpvar.h
regexec$(OBJ_EXT): iperlsys.h
regexec$(OBJ_EXT): mg.h
regexec$(OBJ_EXT): op.h
regexec$(OBJ_EXT): opcode.h
regexec$(OBJ_EXT): opnames.h
regexec$(OBJ_EXT): pad.h
regexec$(OBJ_EXT): patchlevel.h
regexec$(OBJ_EXT): perl.h
regexec$(OBJ_EXT): perlio.h
regexec$(OBJ_EXT): perlvars.h
regexec$(OBJ_EXT): perly.h
regexec$(OBJ_EXT): pp.h
regexec$(OBJ_EXT): pp_proto.h
regexec$(OBJ_EXT): proto.h
regexec$(OBJ_EXT): reentr.inc
regexec$(OBJ_EXT): regcomp.h
regexec$(OBJ_EXT): regexec.c
regexec$(OBJ_EXT): regexp.h
regexec$(OBJ_EXT): regnodes.h
regexec$(OBJ_EXT): scope.h
regexec$(OBJ_EXT): sv.h
regexec$(OBJ_EXT): thrdvar.h
regexec$(OBJ_EXT): thread.h
regexec$(OBJ_EXT): unixish.h
regexec$(OBJ_EXT): utf8.h
regexec$(OBJ_EXT): util.h
regexec$(OBJ_EXT): warnings.h
utf8$(OBJ_EXT): /usr/include/_G_config.h
utf8$(OBJ_EXT): /usr/include/alloca.h
utf8$(OBJ_EXT): /usr/include/arpa/inet.h
utf8$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
utf8$(OBJ_EXT): /usr/include/asm-generic/errno.h
utf8$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
utf8$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
utf8$(OBJ_EXT): /usr/include/asm-generic/param.h
utf8$(OBJ_EXT): /usr/include/asm-generic/socket.h
utf8$(OBJ_EXT): /usr/include/asm-generic/sockios.h
utf8$(OBJ_EXT): /usr/include/ctype.h
utf8$(OBJ_EXT): /usr/include/dirent.h
utf8$(OBJ_EXT): /usr/include/endian.h
utf8$(OBJ_EXT): /usr/include/errno.h
utf8$(OBJ_EXT): /usr/include/fcntl.h
utf8$(OBJ_EXT): /usr/include/features.h
utf8$(OBJ_EXT): /usr/include/getopt.h
utf8$(OBJ_EXT): /usr/include/inttypes.h
utf8$(OBJ_EXT): /usr/include/libio.h
utf8$(OBJ_EXT): /usr/include/limits.h
utf8$(OBJ_EXT): /usr/include/linux/errno.h
utf8$(OBJ_EXT): /usr/include/linux/ioctl.h
utf8$(OBJ_EXT): /usr/include/linux/limits.h
utf8$(OBJ_EXT): /usr/include/linux/param.h
utf8$(OBJ_EXT): /usr/include/locale.h
utf8$(OBJ_EXT): /usr/include/math.h
utf8$(OBJ_EXT): /usr/include/netdb.h
utf8$(OBJ_EXT): /usr/include/netinet/in.h
utf8$(OBJ_EXT): /usr/include/rpc/netdb.h
utf8$(OBJ_EXT): /usr/include/setjmp.h
utf8$(OBJ_EXT): /usr/include/signal.h
utf8$(OBJ_EXT): /usr/include/stdint.h
utf8$(OBJ_EXT): /usr/include/stdio.h
utf8$(OBJ_EXT): /usr/include/stdlib.h
utf8$(OBJ_EXT): /usr/include/string.h
utf8$(OBJ_EXT): /usr/include/time.h
utf8$(OBJ_EXT): /usr/include/unistd.h
utf8$(OBJ_EXT): /usr/include/wchar.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
utf8$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
utf8$(OBJ_EXT): /usr/include/xlocale.h
utf8$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
utf8$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
utf8$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
utf8$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
utf8$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
utf8$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
utf8$(OBJ_EXT): EXTERN.h
utf8$(OBJ_EXT): av.h
utf8$(OBJ_EXT): config.h
utf8$(OBJ_EXT): cop.h
utf8$(OBJ_EXT): cv.h
utf8$(OBJ_EXT): embed.h
utf8$(OBJ_EXT): embedvar.h
utf8$(OBJ_EXT): form.h
utf8$(OBJ_EXT): gv.h
utf8$(OBJ_EXT): handy.h
utf8$(OBJ_EXT): hv.h
utf8$(OBJ_EXT): intrpvar.h
utf8$(OBJ_EXT): iperlsys.h
utf8$(OBJ_EXT): mg.h
utf8$(OBJ_EXT): op.h
utf8$(OBJ_EXT): opcode.h
utf8$(OBJ_EXT): opnames.h
utf8$(OBJ_EXT): pad.h
utf8$(OBJ_EXT): patchlevel.h
utf8$(OBJ_EXT): perl.h
utf8$(OBJ_EXT): perlio.h
utf8$(OBJ_EXT): perlvars.h
utf8$(OBJ_EXT): perly.h
utf8$(OBJ_EXT): pp.h
utf8$(OBJ_EXT): pp_proto.h
utf8$(OBJ_EXT): proto.h
utf8$(OBJ_EXT): reentr.inc
utf8$(OBJ_EXT): regexp.h
utf8$(OBJ_EXT): scope.h
utf8$(OBJ_EXT): sv.h
utf8$(OBJ_EXT): thrdvar.h
utf8$(OBJ_EXT): thread.h
utf8$(OBJ_EXT): unixish.h
utf8$(OBJ_EXT): utf8.c
utf8$(OBJ_EXT): utf8.h
utf8$(OBJ_EXT): util.h
utf8$(OBJ_EXT): warnings.h
gv$(OBJ_EXT): /usr/include/_G_config.h
gv$(OBJ_EXT): /usr/include/alloca.h
gv$(OBJ_EXT): /usr/include/arpa/inet.h
gv$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
gv$(OBJ_EXT): /usr/include/asm-generic/errno.h
gv$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
gv$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
gv$(OBJ_EXT): /usr/include/asm-generic/param.h
gv$(OBJ_EXT): /usr/include/asm-generic/socket.h
gv$(OBJ_EXT): /usr/include/asm-generic/sockios.h
gv$(OBJ_EXT): /usr/include/ctype.h
gv$(OBJ_EXT): /usr/include/dirent.h
gv$(OBJ_EXT): /usr/include/endian.h
gv$(OBJ_EXT): /usr/include/errno.h
gv$(OBJ_EXT): /usr/include/fcntl.h
gv$(OBJ_EXT): /usr/include/features.h
gv$(OBJ_EXT): /usr/include/getopt.h
gv$(OBJ_EXT): /usr/include/inttypes.h
gv$(OBJ_EXT): /usr/include/libio.h
gv$(OBJ_EXT): /usr/include/limits.h
gv$(OBJ_EXT): /usr/include/linux/errno.h
gv$(OBJ_EXT): /usr/include/linux/ioctl.h
gv$(OBJ_EXT): /usr/include/linux/limits.h
gv$(OBJ_EXT): /usr/include/linux/param.h
gv$(OBJ_EXT): /usr/include/locale.h
gv$(OBJ_EXT): /usr/include/math.h
gv$(OBJ_EXT): /usr/include/netdb.h
gv$(OBJ_EXT): /usr/include/netinet/in.h
gv$(OBJ_EXT): /usr/include/rpc/netdb.h
gv$(OBJ_EXT): /usr/include/setjmp.h
gv$(OBJ_EXT): /usr/include/signal.h
gv$(OBJ_EXT): /usr/include/stdint.h
gv$(OBJ_EXT): /usr/include/stdio.h
gv$(OBJ_EXT): /usr/include/stdlib.h
gv$(OBJ_EXT): /usr/include/string.h
gv$(OBJ_EXT): /usr/include/time.h
gv$(OBJ_EXT): /usr/include/unistd.h
gv$(OBJ_EXT): /usr/include/wchar.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
gv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
gv$(OBJ_EXT): /usr/include/xlocale.h
gv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
gv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
gv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
gv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
gv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
gv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
gv$(OBJ_EXT): EXTERN.h
gv$(OBJ_EXT): av.h
gv$(OBJ_EXT): config.h
gv$(OBJ_EXT): cop.h
gv$(OBJ_EXT): cv.h
gv$(OBJ_EXT): embed.h
gv$(OBJ_EXT): embedvar.h
gv$(OBJ_EXT): form.h
gv$(OBJ_EXT): gv.c
gv$(OBJ_EXT): gv.h
gv$(OBJ_EXT): handy.h
gv$(OBJ_EXT): hv.h
gv$(OBJ_EXT): intrpvar.h
gv$(OBJ_EXT): iperlsys.h
gv$(OBJ_EXT): mg.h
gv$(OBJ_EXT): op.h
gv$(OBJ_EXT): opcode.h
gv$(OBJ_EXT): opnames.h
gv$(OBJ_EXT): pad.h
gv$(OBJ_EXT): patchlevel.h
gv$(OBJ_EXT): perl.h
gv$(OBJ_EXT): perlio.h
gv$(OBJ_EXT): perlvars.h
gv$(OBJ_EXT): perly.h
gv$(OBJ_EXT): pp.h
gv$(OBJ_EXT): pp_proto.h
gv$(OBJ_EXT): proto.h
gv$(OBJ_EXT): reentr.inc
gv$(OBJ_EXT): regexp.h
gv$(OBJ_EXT): scope.h
gv$(OBJ_EXT): sv.h
gv$(OBJ_EXT): thrdvar.h
gv$(OBJ_EXT): thread.h
gv$(OBJ_EXT): unixish.h
gv$(OBJ_EXT): utf8.h
gv$(OBJ_EXT): util.h
gv$(OBJ_EXT): warnings.h
sv$(OBJ_EXT): /usr/include/_G_config.h
sv$(OBJ_EXT): /usr/include/alloca.h
sv$(OBJ_EXT): /usr/include/arpa/inet.h
sv$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
sv$(OBJ_EXT): /usr/include/asm-generic/errno.h
sv$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
sv$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
sv$(OBJ_EXT): /usr/include/asm-generic/param.h
sv$(OBJ_EXT): /usr/include/asm-generic/socket.h
sv$(OBJ_EXT): /usr/include/asm-generic/sockios.h
sv$(OBJ_EXT): /usr/include/ctype.h
sv$(OBJ_EXT): /usr/include/dirent.h
sv$(OBJ_EXT): /usr/include/endian.h
sv$(OBJ_EXT): /usr/include/errno.h
sv$(OBJ_EXT): /usr/include/fcntl.h
sv$(OBJ_EXT): /usr/include/features.h
sv$(OBJ_EXT): /usr/include/getopt.h
sv$(OBJ_EXT): /usr/include/inttypes.h
sv$(OBJ_EXT): /usr/include/libio.h
sv$(OBJ_EXT): /usr/include/limits.h
sv$(OBJ_EXT): /usr/include/linux/errno.h
sv$(OBJ_EXT): /usr/include/linux/ioctl.h
sv$(OBJ_EXT): /usr/include/linux/limits.h
sv$(OBJ_EXT): /usr/include/linux/param.h
sv$(OBJ_EXT): /usr/include/locale.h
sv$(OBJ_EXT): /usr/include/math.h
sv$(OBJ_EXT): /usr/include/netdb.h
sv$(OBJ_EXT): /usr/include/netinet/in.h
sv$(OBJ_EXT): /usr/include/rpc/netdb.h
sv$(OBJ_EXT): /usr/include/setjmp.h
sv$(OBJ_EXT): /usr/include/signal.h
sv$(OBJ_EXT): /usr/include/stdint.h
sv$(OBJ_EXT): /usr/include/stdio.h
sv$(OBJ_EXT): /usr/include/stdlib.h
sv$(OBJ_EXT): /usr/include/string.h
sv$(OBJ_EXT): /usr/include/time.h
sv$(OBJ_EXT): /usr/include/unistd.h
sv$(OBJ_EXT): /usr/include/wchar.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
sv$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
sv$(OBJ_EXT): /usr/include/xlocale.h
sv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
sv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
sv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
sv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
sv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
sv$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
sv$(OBJ_EXT): EXTERN.h
sv$(OBJ_EXT): av.h
sv$(OBJ_EXT): config.h
sv$(OBJ_EXT): cop.h
sv$(OBJ_EXT): cv.h
sv$(OBJ_EXT): embed.h
sv$(OBJ_EXT): embedvar.h
sv$(OBJ_EXT): form.h
sv$(OBJ_EXT): gv.h
sv$(OBJ_EXT): handy.h
sv$(OBJ_EXT): hv.h
sv$(OBJ_EXT): intrpvar.h
sv$(OBJ_EXT): iperlsys.h
sv$(OBJ_EXT): mg.h
sv$(OBJ_EXT): op.h
sv$(OBJ_EXT): opcode.h
sv$(OBJ_EXT): opnames.h
sv$(OBJ_EXT): pad.h
sv$(OBJ_EXT): patchlevel.h
sv$(OBJ_EXT): perl.h
sv$(OBJ_EXT): perlio.h
sv$(OBJ_EXT): perlvars.h
sv$(OBJ_EXT): perly.h
sv$(OBJ_EXT): pp.h
sv$(OBJ_EXT): pp_proto.h
sv$(OBJ_EXT): proto.h
sv$(OBJ_EXT): reentr.inc
sv$(OBJ_EXT): regcomp.h
sv$(OBJ_EXT): regexp.h
sv$(OBJ_EXT): regnodes.h
sv$(OBJ_EXT): scope.h
sv$(OBJ_EXT): sv.c
sv$(OBJ_EXT): sv.h
sv$(OBJ_EXT): thrdvar.h
sv$(OBJ_EXT): thread.h
sv$(OBJ_EXT): unixish.h
sv$(OBJ_EXT): utf8.h
sv$(OBJ_EXT): util.h
sv$(OBJ_EXT): warnings.h
taint$(OBJ_EXT): /usr/include/_G_config.h
taint$(OBJ_EXT): /usr/include/alloca.h
taint$(OBJ_EXT): /usr/include/arpa/inet.h
taint$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
taint$(OBJ_EXT): /usr/include/asm-generic/errno.h
taint$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
taint$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
taint$(OBJ_EXT): /usr/include/asm-generic/param.h
taint$(OBJ_EXT): /usr/include/asm-generic/socket.h
taint$(OBJ_EXT): /usr/include/asm-generic/sockios.h
taint$(OBJ_EXT): /usr/include/ctype.h
taint$(OBJ_EXT): /usr/include/dirent.h
taint$(OBJ_EXT): /usr/include/endian.h
taint$(OBJ_EXT): /usr/include/errno.h
taint$(OBJ_EXT): /usr/include/fcntl.h
taint$(OBJ_EXT): /usr/include/features.h
taint$(OBJ_EXT): /usr/include/getopt.h
taint$(OBJ_EXT): /usr/include/inttypes.h
taint$(OBJ_EXT): /usr/include/libio.h
taint$(OBJ_EXT): /usr/include/limits.h
taint$(OBJ_EXT): /usr/include/linux/errno.h
taint$(OBJ_EXT): /usr/include/linux/ioctl.h
taint$(OBJ_EXT): /usr/include/linux/limits.h
taint$(OBJ_EXT): /usr/include/linux/param.h
taint$(OBJ_EXT): /usr/include/locale.h
taint$(OBJ_EXT): /usr/include/math.h
taint$(OBJ_EXT): /usr/include/netdb.h
taint$(OBJ_EXT): /usr/include/netinet/in.h
taint$(OBJ_EXT): /usr/include/rpc/netdb.h
taint$(OBJ_EXT): /usr/include/setjmp.h
taint$(OBJ_EXT): /usr/include/signal.h
taint$(OBJ_EXT): /usr/include/stdint.h
taint$(OBJ_EXT): /usr/include/stdio.h
taint$(OBJ_EXT): /usr/include/stdlib.h
taint$(OBJ_EXT): /usr/include/string.h
taint$(OBJ_EXT): /usr/include/time.h
taint$(OBJ_EXT): /usr/include/unistd.h
taint$(OBJ_EXT): /usr/include/wchar.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
taint$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
taint$(OBJ_EXT): /usr/include/xlocale.h
taint$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
taint$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
taint$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
taint$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
taint$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
taint$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
taint$(OBJ_EXT): EXTERN.h
taint$(OBJ_EXT): av.h
taint$(OBJ_EXT): config.h
taint$(OBJ_EXT): cop.h
taint$(OBJ_EXT): cv.h
taint$(OBJ_EXT): embed.h
taint$(OBJ_EXT): embedvar.h
taint$(OBJ_EXT): form.h
taint$(OBJ_EXT): gv.h
taint$(OBJ_EXT): handy.h
taint$(OBJ_EXT): hv.h
taint$(OBJ_EXT): intrpvar.h
taint$(OBJ_EXT): iperlsys.h
taint$(OBJ_EXT): mg.h
taint$(OBJ_EXT): op.h
taint$(OBJ_EXT): opcode.h
taint$(OBJ_EXT): opnames.h
taint$(OBJ_EXT): pad.h
taint$(OBJ_EXT): patchlevel.h
taint$(OBJ_EXT): perl.h
taint$(OBJ_EXT): perlio.h
taint$(OBJ_EXT): perlvars.h
taint$(OBJ_EXT): perly.h
taint$(OBJ_EXT): pp.h
taint$(OBJ_EXT): pp_proto.h
taint$(OBJ_EXT): proto.h
taint$(OBJ_EXT): reentr.inc
taint$(OBJ_EXT): regexp.h
taint$(OBJ_EXT): scope.h
taint$(OBJ_EXT): sv.h
taint$(OBJ_EXT): taint.c
taint$(OBJ_EXT): thrdvar.h
taint$(OBJ_EXT): thread.h
taint$(OBJ_EXT): unixish.h
taint$(OBJ_EXT): utf8.h
taint$(OBJ_EXT): util.h
taint$(OBJ_EXT): warnings.h
toke$(OBJ_EXT): /usr/include/_G_config.h
toke$(OBJ_EXT): /usr/include/alloca.h
toke$(OBJ_EXT): /usr/include/arpa/inet.h
toke$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
toke$(OBJ_EXT): /usr/include/asm-generic/errno.h
toke$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
toke$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
toke$(OBJ_EXT): /usr/include/asm-generic/param.h
toke$(OBJ_EXT): /usr/include/asm-generic/socket.h
toke$(OBJ_EXT): /usr/include/asm-generic/sockios.h
toke$(OBJ_EXT): /usr/include/ctype.h
toke$(OBJ_EXT): /usr/include/dirent.h
toke$(OBJ_EXT): /usr/include/endian.h
toke$(OBJ_EXT): /usr/include/errno.h
toke$(OBJ_EXT): /usr/include/fcntl.h
toke$(OBJ_EXT): /usr/include/features.h
toke$(OBJ_EXT): /usr/include/getopt.h
toke$(OBJ_EXT): /usr/include/inttypes.h
toke$(OBJ_EXT): /usr/include/libio.h
toke$(OBJ_EXT): /usr/include/limits.h
toke$(OBJ_EXT): /usr/include/linux/errno.h
toke$(OBJ_EXT): /usr/include/linux/ioctl.h
toke$(OBJ_EXT): /usr/include/linux/limits.h
toke$(OBJ_EXT): /usr/include/linux/param.h
toke$(OBJ_EXT): /usr/include/locale.h
toke$(OBJ_EXT): /usr/include/math.h
toke$(OBJ_EXT): /usr/include/netdb.h
toke$(OBJ_EXT): /usr/include/netinet/in.h
toke$(OBJ_EXT): /usr/include/rpc/netdb.h
toke$(OBJ_EXT): /usr/include/setjmp.h
toke$(OBJ_EXT): /usr/include/signal.h
toke$(OBJ_EXT): /usr/include/stdint.h
toke$(OBJ_EXT): /usr/include/stdio.h
toke$(OBJ_EXT): /usr/include/stdlib.h
toke$(OBJ_EXT): /usr/include/string.h
toke$(OBJ_EXT): /usr/include/time.h
toke$(OBJ_EXT): /usr/include/unistd.h
toke$(OBJ_EXT): /usr/include/wchar.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
toke$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
toke$(OBJ_EXT): /usr/include/xlocale.h
toke$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
toke$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
toke$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
toke$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
toke$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
toke$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
toke$(OBJ_EXT): EXTERN.h
toke$(OBJ_EXT): av.h
toke$(OBJ_EXT): config.h
toke$(OBJ_EXT): cop.h
toke$(OBJ_EXT): cv.h
toke$(OBJ_EXT): embed.h
toke$(OBJ_EXT): embedvar.h
toke$(OBJ_EXT): form.h
toke$(OBJ_EXT): gv.h
toke$(OBJ_EXT): handy.h
toke$(OBJ_EXT): hv.h
toke$(OBJ_EXT): intrpvar.h
toke$(OBJ_EXT): iperlsys.h
toke$(OBJ_EXT): keywords.h
toke$(OBJ_EXT): mg.h
toke$(OBJ_EXT): op.h
toke$(OBJ_EXT): opcode.h
toke$(OBJ_EXT): opnames.h
toke$(OBJ_EXT): pad.h
toke$(OBJ_EXT): patchlevel.h
toke$(OBJ_EXT): perl.h
toke$(OBJ_EXT): perlio.h
toke$(OBJ_EXT): perlvars.h
toke$(OBJ_EXT): perly.h
toke$(OBJ_EXT): pp.h
toke$(OBJ_EXT): pp_proto.h
toke$(OBJ_EXT): proto.h
toke$(OBJ_EXT): reentr.inc
toke$(OBJ_EXT): regexp.h
toke$(OBJ_EXT): scope.h
toke$(OBJ_EXT): sv.h
toke$(OBJ_EXT): thrdvar.h
toke$(OBJ_EXT): thread.h
toke$(OBJ_EXT): toke.c
toke$(OBJ_EXT): unixish.h
toke$(OBJ_EXT): utf8.h
toke$(OBJ_EXT): util.h
toke$(OBJ_EXT): warnings.h
util$(OBJ_EXT): /usr/include/_G_config.h
util$(OBJ_EXT): /usr/include/alloca.h
util$(OBJ_EXT): /usr/include/arpa/inet.h
util$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
util$(OBJ_EXT): /usr/include/asm-generic/errno.h
util$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
util$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
util$(OBJ_EXT): /usr/include/asm-generic/param.h
util$(OBJ_EXT): /usr/include/asm-generic/socket.h
util$(OBJ_EXT): /usr/include/asm-generic/sockios.h
util$(OBJ_EXT): /usr/include/ctype.h
util$(OBJ_EXT): /usr/include/dirent.h
util$(OBJ_EXT): /usr/include/endian.h
util$(OBJ_EXT): /usr/include/errno.h
util$(OBJ_EXT): /usr/include/fcntl.h
util$(OBJ_EXT): /usr/include/features.h
util$(OBJ_EXT): /usr/include/getopt.h
util$(OBJ_EXT): /usr/include/inttypes.h
util$(OBJ_EXT): /usr/include/libio.h
util$(OBJ_EXT): /usr/include/limits.h
util$(OBJ_EXT): /usr/include/linux/errno.h
util$(OBJ_EXT): /usr/include/linux/ioctl.h
util$(OBJ_EXT): /usr/include/linux/limits.h
util$(OBJ_EXT): /usr/include/linux/param.h
util$(OBJ_EXT): /usr/include/locale.h
util$(OBJ_EXT): /usr/include/math.h
util$(OBJ_EXT): /usr/include/netdb.h
util$(OBJ_EXT): /usr/include/netinet/in.h
util$(OBJ_EXT): /usr/include/rpc/netdb.h
util$(OBJ_EXT): /usr/include/setjmp.h
util$(OBJ_EXT): /usr/include/signal.h
util$(OBJ_EXT): /usr/include/stdint.h
util$(OBJ_EXT): /usr/include/stdio.h
util$(OBJ_EXT): /usr/include/stdlib.h
util$(OBJ_EXT): /usr/include/string.h
util$(OBJ_EXT): /usr/include/time.h
util$(OBJ_EXT): /usr/include/unistd.h
util$(OBJ_EXT): /usr/include/wchar.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/resource.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/resource.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/wait.h
util$(OBJ_EXT): /usr/include/xlocale.h
util$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
util$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
util$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
util$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
util$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
util$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
util$(OBJ_EXT): EXTERN.h
util$(OBJ_EXT): av.h
util$(OBJ_EXT): config.h
util$(OBJ_EXT): cop.h
util$(OBJ_EXT): cv.h
util$(OBJ_EXT): embed.h
util$(OBJ_EXT): embedvar.h
util$(OBJ_EXT): form.h
util$(OBJ_EXT): gv.h
util$(OBJ_EXT): handy.h
util$(OBJ_EXT): hv.h
util$(OBJ_EXT): intrpvar.h
util$(OBJ_EXT): iperlsys.h
util$(OBJ_EXT): mg.h
util$(OBJ_EXT): op.h
util$(OBJ_EXT): opcode.h
util$(OBJ_EXT): opnames.h
util$(OBJ_EXT): pad.h
util$(OBJ_EXT): patchlevel.h
util$(OBJ_EXT): perl.h
util$(OBJ_EXT): perlio.h
util$(OBJ_EXT): perlvars.h
util$(OBJ_EXT): perly.h
util$(OBJ_EXT): pp.h
util$(OBJ_EXT): pp_proto.h
util$(OBJ_EXT): proto.h
util$(OBJ_EXT): reentr.inc
util$(OBJ_EXT): regexp.h
util$(OBJ_EXT): scope.h
util$(OBJ_EXT): sv.h
util$(OBJ_EXT): thrdvar.h
util$(OBJ_EXT): thread.h
util$(OBJ_EXT): unixish.h
util$(OBJ_EXT): utf8.h
util$(OBJ_EXT): util.c
util$(OBJ_EXT): util.h
util$(OBJ_EXT): warnings.h
deb$(OBJ_EXT): /usr/include/_G_config.h
deb$(OBJ_EXT): /usr/include/alloca.h
deb$(OBJ_EXT): /usr/include/arpa/inet.h
deb$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
deb$(OBJ_EXT): /usr/include/asm-generic/errno.h
deb$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
deb$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
deb$(OBJ_EXT): /usr/include/asm-generic/param.h
deb$(OBJ_EXT): /usr/include/asm-generic/socket.h
deb$(OBJ_EXT): /usr/include/asm-generic/sockios.h
deb$(OBJ_EXT): /usr/include/ctype.h
deb$(OBJ_EXT): /usr/include/dirent.h
deb$(OBJ_EXT): /usr/include/endian.h
deb$(OBJ_EXT): /usr/include/errno.h
deb$(OBJ_EXT): /usr/include/fcntl.h
deb$(OBJ_EXT): /usr/include/features.h
deb$(OBJ_EXT): /usr/include/getopt.h
deb$(OBJ_EXT): /usr/include/inttypes.h
deb$(OBJ_EXT): /usr/include/libio.h
deb$(OBJ_EXT): /usr/include/limits.h
deb$(OBJ_EXT): /usr/include/linux/errno.h
deb$(OBJ_EXT): /usr/include/linux/ioctl.h
deb$(OBJ_EXT): /usr/include/linux/limits.h
deb$(OBJ_EXT): /usr/include/linux/param.h
deb$(OBJ_EXT): /usr/include/locale.h
deb$(OBJ_EXT): /usr/include/math.h
deb$(OBJ_EXT): /usr/include/netdb.h
deb$(OBJ_EXT): /usr/include/netinet/in.h
deb$(OBJ_EXT): /usr/include/rpc/netdb.h
deb$(OBJ_EXT): /usr/include/setjmp.h
deb$(OBJ_EXT): /usr/include/signal.h
deb$(OBJ_EXT): /usr/include/stdint.h
deb$(OBJ_EXT): /usr/include/stdio.h
deb$(OBJ_EXT): /usr/include/stdlib.h
deb$(OBJ_EXT): /usr/include/string.h
deb$(OBJ_EXT): /usr/include/time.h
deb$(OBJ_EXT): /usr/include/unistd.h
deb$(OBJ_EXT): /usr/include/wchar.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
deb$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
deb$(OBJ_EXT): /usr/include/xlocale.h
deb$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
deb$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
deb$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
deb$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
deb$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
deb$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
deb$(OBJ_EXT): EXTERN.h
deb$(OBJ_EXT): av.h
deb$(OBJ_EXT): config.h
deb$(OBJ_EXT): cop.h
deb$(OBJ_EXT): cv.h
deb$(OBJ_EXT): deb.c
deb$(OBJ_EXT): embed.h
deb$(OBJ_EXT): embedvar.h
deb$(OBJ_EXT): form.h
deb$(OBJ_EXT): gv.h
deb$(OBJ_EXT): handy.h
deb$(OBJ_EXT): hv.h
deb$(OBJ_EXT): intrpvar.h
deb$(OBJ_EXT): iperlsys.h
deb$(OBJ_EXT): mg.h
deb$(OBJ_EXT): op.h
deb$(OBJ_EXT): opcode.h
deb$(OBJ_EXT): opnames.h
deb$(OBJ_EXT): pad.h
deb$(OBJ_EXT): patchlevel.h
deb$(OBJ_EXT): perl.h
deb$(OBJ_EXT): perlio.h
deb$(OBJ_EXT): perlvars.h
deb$(OBJ_EXT): perly.h
deb$(OBJ_EXT): pp.h
deb$(OBJ_EXT): pp_proto.h
deb$(OBJ_EXT): proto.h
deb$(OBJ_EXT): reentr.inc
deb$(OBJ_EXT): regexp.h
deb$(OBJ_EXT): scope.h
deb$(OBJ_EXT): sv.h
deb$(OBJ_EXT): thrdvar.h
deb$(OBJ_EXT): thread.h
deb$(OBJ_EXT): unixish.h
deb$(OBJ_EXT): utf8.h
deb$(OBJ_EXT): util.h
deb$(OBJ_EXT): warnings.h
run$(OBJ_EXT): /usr/include/_G_config.h
run$(OBJ_EXT): /usr/include/alloca.h
run$(OBJ_EXT): /usr/include/arpa/inet.h
run$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
run$(OBJ_EXT): /usr/include/asm-generic/errno.h
run$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
run$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
run$(OBJ_EXT): /usr/include/asm-generic/param.h
run$(OBJ_EXT): /usr/include/asm-generic/socket.h
run$(OBJ_EXT): /usr/include/asm-generic/sockios.h
run$(OBJ_EXT): /usr/include/ctype.h
run$(OBJ_EXT): /usr/include/dirent.h
run$(OBJ_EXT): /usr/include/endian.h
run$(OBJ_EXT): /usr/include/errno.h
run$(OBJ_EXT): /usr/include/fcntl.h
run$(OBJ_EXT): /usr/include/features.h
run$(OBJ_EXT): /usr/include/getopt.h
run$(OBJ_EXT): /usr/include/inttypes.h
run$(OBJ_EXT): /usr/include/libio.h
run$(OBJ_EXT): /usr/include/limits.h
run$(OBJ_EXT): /usr/include/linux/errno.h
run$(OBJ_EXT): /usr/include/linux/ioctl.h
run$(OBJ_EXT): /usr/include/linux/limits.h
run$(OBJ_EXT): /usr/include/linux/param.h
run$(OBJ_EXT): /usr/include/locale.h
run$(OBJ_EXT): /usr/include/math.h
run$(OBJ_EXT): /usr/include/netdb.h
run$(OBJ_EXT): /usr/include/netinet/in.h
run$(OBJ_EXT): /usr/include/rpc/netdb.h
run$(OBJ_EXT): /usr/include/setjmp.h
run$(OBJ_EXT): /usr/include/signal.h
run$(OBJ_EXT): /usr/include/stdint.h
run$(OBJ_EXT): /usr/include/stdio.h
run$(OBJ_EXT): /usr/include/stdlib.h
run$(OBJ_EXT): /usr/include/string.h
run$(OBJ_EXT): /usr/include/time.h
run$(OBJ_EXT): /usr/include/unistd.h
run$(OBJ_EXT): /usr/include/wchar.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
run$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
run$(OBJ_EXT): /usr/include/xlocale.h
run$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
run$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
run$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
run$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
run$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
run$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
run$(OBJ_EXT): EXTERN.h
run$(OBJ_EXT): av.h
run$(OBJ_EXT): config.h
run$(OBJ_EXT): cop.h
run$(OBJ_EXT): cv.h
run$(OBJ_EXT): embed.h
run$(OBJ_EXT): embedvar.h
run$(OBJ_EXT): form.h
run$(OBJ_EXT): gv.h
run$(OBJ_EXT): handy.h
run$(OBJ_EXT): hv.h
run$(OBJ_EXT): intrpvar.h
run$(OBJ_EXT): iperlsys.h
run$(OBJ_EXT): mg.h
run$(OBJ_EXT): op.h
run$(OBJ_EXT): opcode.h
run$(OBJ_EXT): opnames.h
run$(OBJ_EXT): pad.h
run$(OBJ_EXT): patchlevel.h
run$(OBJ_EXT): perl.h
run$(OBJ_EXT): perlio.h
run$(OBJ_EXT): perlvars.h
run$(OBJ_EXT): perly.h
run$(OBJ_EXT): pp.h
run$(OBJ_EXT): pp_proto.h
run$(OBJ_EXT): proto.h
run$(OBJ_EXT): reentr.inc
run$(OBJ_EXT): regexp.h
run$(OBJ_EXT): run.c
run$(OBJ_EXT): scope.h
run$(OBJ_EXT): sv.h
run$(OBJ_EXT): thrdvar.h
run$(OBJ_EXT): thread.h
run$(OBJ_EXT): unixish.h
run$(OBJ_EXT): utf8.h
run$(OBJ_EXT): util.h
run$(OBJ_EXT): warnings.h
universal$(OBJ_EXT): /usr/include/_G_config.h
universal$(OBJ_EXT): /usr/include/alloca.h
universal$(OBJ_EXT): /usr/include/arpa/inet.h
universal$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
universal$(OBJ_EXT): /usr/include/asm-generic/errno.h
universal$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
universal$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
universal$(OBJ_EXT): /usr/include/asm-generic/param.h
universal$(OBJ_EXT): /usr/include/asm-generic/socket.h
universal$(OBJ_EXT): /usr/include/asm-generic/sockios.h
universal$(OBJ_EXT): /usr/include/ctype.h
universal$(OBJ_EXT): /usr/include/dirent.h
universal$(OBJ_EXT): /usr/include/endian.h
universal$(OBJ_EXT): /usr/include/errno.h
universal$(OBJ_EXT): /usr/include/fcntl.h
universal$(OBJ_EXT): /usr/include/features.h
universal$(OBJ_EXT): /usr/include/getopt.h
universal$(OBJ_EXT): /usr/include/inttypes.h
universal$(OBJ_EXT): /usr/include/libio.h
universal$(OBJ_EXT): /usr/include/limits.h
universal$(OBJ_EXT): /usr/include/linux/errno.h
universal$(OBJ_EXT): /usr/include/linux/ioctl.h
universal$(OBJ_EXT): /usr/include/linux/limits.h
universal$(OBJ_EXT): /usr/include/linux/param.h
universal$(OBJ_EXT): /usr/include/locale.h
universal$(OBJ_EXT): /usr/include/math.h
universal$(OBJ_EXT): /usr/include/netdb.h
universal$(OBJ_EXT): /usr/include/netinet/in.h
universal$(OBJ_EXT): /usr/include/rpc/netdb.h
universal$(OBJ_EXT): /usr/include/setjmp.h
universal$(OBJ_EXT): /usr/include/signal.h
universal$(OBJ_EXT): /usr/include/stdint.h
universal$(OBJ_EXT): /usr/include/stdio.h
universal$(OBJ_EXT): /usr/include/stdlib.h
universal$(OBJ_EXT): /usr/include/string.h
universal$(OBJ_EXT): /usr/include/time.h
universal$(OBJ_EXT): /usr/include/unistd.h
universal$(OBJ_EXT): /usr/include/wchar.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
universal$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
universal$(OBJ_EXT): /usr/include/xlocale.h
universal$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
universal$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
universal$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
universal$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
universal$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
universal$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
universal$(OBJ_EXT): EXTERN.h
universal$(OBJ_EXT): XSUB.h
universal$(OBJ_EXT): av.h
universal$(OBJ_EXT): config.h
universal$(OBJ_EXT): cop.h
universal$(OBJ_EXT): cv.h
universal$(OBJ_EXT): embed.h
universal$(OBJ_EXT): embedvar.h
universal$(OBJ_EXT): form.h
universal$(OBJ_EXT): gv.h
universal$(OBJ_EXT): handy.h
universal$(OBJ_EXT): hv.h
universal$(OBJ_EXT): intrpvar.h
universal$(OBJ_EXT): iperlsys.h
universal$(OBJ_EXT): mg.h
universal$(OBJ_EXT): op.h
universal$(OBJ_EXT): opcode.h
universal$(OBJ_EXT): opnames.h
universal$(OBJ_EXT): pad.h
universal$(OBJ_EXT): patchlevel.h
universal$(OBJ_EXT): perl.h
universal$(OBJ_EXT): perlapi.h
universal$(OBJ_EXT): perlio.h
universal$(OBJ_EXT): perliol.h
universal$(OBJ_EXT): perlvars.h
universal$(OBJ_EXT): perly.h
universal$(OBJ_EXT): pp.h
universal$(OBJ_EXT): pp_proto.h
universal$(OBJ_EXT): proto.h
universal$(OBJ_EXT): reentr.inc
universal$(OBJ_EXT): regexp.h
universal$(OBJ_EXT): scope.h
universal$(OBJ_EXT): sv.h
universal$(OBJ_EXT): thrdvar.h
universal$(OBJ_EXT): thread.h
universal$(OBJ_EXT): universal.c
universal$(OBJ_EXT): unixish.h
universal$(OBJ_EXT): utf8.h
universal$(OBJ_EXT): util.h
universal$(OBJ_EXT): warnings.h
xsutils$(OBJ_EXT): /usr/include/_G_config.h
xsutils$(OBJ_EXT): /usr/include/alloca.h
xsutils$(OBJ_EXT): /usr/include/arpa/inet.h
xsutils$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
xsutils$(OBJ_EXT): /usr/include/asm-generic/errno.h
xsutils$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
xsutils$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
xsutils$(OBJ_EXT): /usr/include/asm-generic/param.h
xsutils$(OBJ_EXT): /usr/include/asm-generic/socket.h
xsutils$(OBJ_EXT): /usr/include/asm-generic/sockios.h
xsutils$(OBJ_EXT): /usr/include/ctype.h
xsutils$(OBJ_EXT): /usr/include/dirent.h
xsutils$(OBJ_EXT): /usr/include/endian.h
xsutils$(OBJ_EXT): /usr/include/errno.h
xsutils$(OBJ_EXT): /usr/include/fcntl.h
xsutils$(OBJ_EXT): /usr/include/features.h
xsutils$(OBJ_EXT): /usr/include/getopt.h
xsutils$(OBJ_EXT): /usr/include/inttypes.h
xsutils$(OBJ_EXT): /usr/include/libio.h
xsutils$(OBJ_EXT): /usr/include/limits.h
xsutils$(OBJ_EXT): /usr/include/linux/errno.h
xsutils$(OBJ_EXT): /usr/include/linux/ioctl.h
xsutils$(OBJ_EXT): /usr/include/linux/limits.h
xsutils$(OBJ_EXT): /usr/include/linux/param.h
xsutils$(OBJ_EXT): /usr/include/locale.h
xsutils$(OBJ_EXT): /usr/include/math.h
xsutils$(OBJ_EXT): /usr/include/netdb.h
xsutils$(OBJ_EXT): /usr/include/netinet/in.h
xsutils$(OBJ_EXT): /usr/include/rpc/netdb.h
xsutils$(OBJ_EXT): /usr/include/setjmp.h
xsutils$(OBJ_EXT): /usr/include/signal.h
xsutils$(OBJ_EXT): /usr/include/stdint.h
xsutils$(OBJ_EXT): /usr/include/stdio.h
xsutils$(OBJ_EXT): /usr/include/stdlib.h
xsutils$(OBJ_EXT): /usr/include/string.h
xsutils$(OBJ_EXT): /usr/include/time.h
xsutils$(OBJ_EXT): /usr/include/unistd.h
xsutils$(OBJ_EXT): /usr/include/wchar.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
xsutils$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
xsutils$(OBJ_EXT): /usr/include/xlocale.h
xsutils$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
xsutils$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
xsutils$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
xsutils$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
xsutils$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
xsutils$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
xsutils$(OBJ_EXT): EXTERN.h
xsutils$(OBJ_EXT): XSUB.h
xsutils$(OBJ_EXT): av.h
xsutils$(OBJ_EXT): config.h
xsutils$(OBJ_EXT): cop.h
xsutils$(OBJ_EXT): cv.h
xsutils$(OBJ_EXT): embed.h
xsutils$(OBJ_EXT): embedvar.h
xsutils$(OBJ_EXT): form.h
xsutils$(OBJ_EXT): gv.h
xsutils$(OBJ_EXT): handy.h
xsutils$(OBJ_EXT): hv.h
xsutils$(OBJ_EXT): intrpvar.h
xsutils$(OBJ_EXT): iperlsys.h
xsutils$(OBJ_EXT): mg.h
xsutils$(OBJ_EXT): op.h
xsutils$(OBJ_EXT): opcode.h
xsutils$(OBJ_EXT): opnames.h
xsutils$(OBJ_EXT): pad.h
xsutils$(OBJ_EXT): patchlevel.h
xsutils$(OBJ_EXT): perl.h
xsutils$(OBJ_EXT): perlapi.h
xsutils$(OBJ_EXT): perlio.h
xsutils$(OBJ_EXT): perlvars.h
xsutils$(OBJ_EXT): perly.h
xsutils$(OBJ_EXT): pp.h
xsutils$(OBJ_EXT): pp_proto.h
xsutils$(OBJ_EXT): proto.h
xsutils$(OBJ_EXT): reentr.inc
xsutils$(OBJ_EXT): regexp.h
xsutils$(OBJ_EXT): scope.h
xsutils$(OBJ_EXT): sv.h
xsutils$(OBJ_EXT): thrdvar.h
xsutils$(OBJ_EXT): thread.h
xsutils$(OBJ_EXT): unixish.h
xsutils$(OBJ_EXT): utf8.h
xsutils$(OBJ_EXT): util.h
xsutils$(OBJ_EXT): warnings.h
xsutils$(OBJ_EXT): xsutils.c
pad$(OBJ_EXT): /usr/include/_G_config.h
pad$(OBJ_EXT): /usr/include/alloca.h
pad$(OBJ_EXT): /usr/include/arpa/inet.h
pad$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
pad$(OBJ_EXT): /usr/include/asm-generic/errno.h
pad$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
pad$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
pad$(OBJ_EXT): /usr/include/asm-generic/param.h
pad$(OBJ_EXT): /usr/include/asm-generic/socket.h
pad$(OBJ_EXT): /usr/include/asm-generic/sockios.h
pad$(OBJ_EXT): /usr/include/ctype.h
pad$(OBJ_EXT): /usr/include/dirent.h
pad$(OBJ_EXT): /usr/include/endian.h
pad$(OBJ_EXT): /usr/include/errno.h
pad$(OBJ_EXT): /usr/include/fcntl.h
pad$(OBJ_EXT): /usr/include/features.h
pad$(OBJ_EXT): /usr/include/getopt.h
pad$(OBJ_EXT): /usr/include/inttypes.h
pad$(OBJ_EXT): /usr/include/libio.h
pad$(OBJ_EXT): /usr/include/limits.h
pad$(OBJ_EXT): /usr/include/linux/errno.h
pad$(OBJ_EXT): /usr/include/linux/ioctl.h
pad$(OBJ_EXT): /usr/include/linux/limits.h
pad$(OBJ_EXT): /usr/include/linux/param.h
pad$(OBJ_EXT): /usr/include/locale.h
pad$(OBJ_EXT): /usr/include/math.h
pad$(OBJ_EXT): /usr/include/netdb.h
pad$(OBJ_EXT): /usr/include/netinet/in.h
pad$(OBJ_EXT): /usr/include/rpc/netdb.h
pad$(OBJ_EXT): /usr/include/setjmp.h
pad$(OBJ_EXT): /usr/include/signal.h
pad$(OBJ_EXT): /usr/include/stdint.h
pad$(OBJ_EXT): /usr/include/stdio.h
pad$(OBJ_EXT): /usr/include/stdlib.h
pad$(OBJ_EXT): /usr/include/string.h
pad$(OBJ_EXT): /usr/include/time.h
pad$(OBJ_EXT): /usr/include/unistd.h
pad$(OBJ_EXT): /usr/include/wchar.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
pad$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
pad$(OBJ_EXT): /usr/include/xlocale.h
pad$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
pad$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
pad$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
pad$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
pad$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
pad$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
pad$(OBJ_EXT): EXTERN.h
pad$(OBJ_EXT): av.h
pad$(OBJ_EXT): config.h
pad$(OBJ_EXT): cop.h
pad$(OBJ_EXT): cv.h
pad$(OBJ_EXT): embed.h
pad$(OBJ_EXT): embedvar.h
pad$(OBJ_EXT): form.h
pad$(OBJ_EXT): gv.h
pad$(OBJ_EXT): handy.h
pad$(OBJ_EXT): hv.h
pad$(OBJ_EXT): intrpvar.h
pad$(OBJ_EXT): iperlsys.h
pad$(OBJ_EXT): mg.h
pad$(OBJ_EXT): op.h
pad$(OBJ_EXT): opcode.h
pad$(OBJ_EXT): opnames.h
pad$(OBJ_EXT): pad.c
pad$(OBJ_EXT): pad.h
pad$(OBJ_EXT): patchlevel.h
pad$(OBJ_EXT): perl.h
pad$(OBJ_EXT): perlio.h
pad$(OBJ_EXT): perlvars.h
pad$(OBJ_EXT): perly.h
pad$(OBJ_EXT): pp.h
pad$(OBJ_EXT): pp_proto.h
pad$(OBJ_EXT): proto.h
pad$(OBJ_EXT): reentr.inc
pad$(OBJ_EXT): regexp.h
pad$(OBJ_EXT): scope.h
pad$(OBJ_EXT): sv.h
pad$(OBJ_EXT): thrdvar.h
pad$(OBJ_EXT): thread.h
pad$(OBJ_EXT): unixish.h
pad$(OBJ_EXT): utf8.h
pad$(OBJ_EXT): util.h
pad$(OBJ_EXT): warnings.h
globals$(OBJ_EXT): /usr/include/_G_config.h
globals$(OBJ_EXT): /usr/include/alloca.h
globals$(OBJ_EXT): /usr/include/arpa/inet.h
globals$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
globals$(OBJ_EXT): /usr/include/asm-generic/errno.h
globals$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
globals$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
globals$(OBJ_EXT): /usr/include/asm-generic/param.h
globals$(OBJ_EXT): /usr/include/asm-generic/socket.h
globals$(OBJ_EXT): /usr/include/asm-generic/sockios.h
globals$(OBJ_EXT): /usr/include/ctype.h
globals$(OBJ_EXT): /usr/include/dirent.h
globals$(OBJ_EXT): /usr/include/endian.h
globals$(OBJ_EXT): /usr/include/errno.h
globals$(OBJ_EXT): /usr/include/fcntl.h
globals$(OBJ_EXT): /usr/include/features.h
globals$(OBJ_EXT): /usr/include/getopt.h
globals$(OBJ_EXT): /usr/include/inttypes.h
globals$(OBJ_EXT): /usr/include/libio.h
globals$(OBJ_EXT): /usr/include/limits.h
globals$(OBJ_EXT): /usr/include/linux/errno.h
globals$(OBJ_EXT): /usr/include/linux/ioctl.h
globals$(OBJ_EXT): /usr/include/linux/limits.h
globals$(OBJ_EXT): /usr/include/linux/param.h
globals$(OBJ_EXT): /usr/include/locale.h
globals$(OBJ_EXT): /usr/include/math.h
globals$(OBJ_EXT): /usr/include/netdb.h
globals$(OBJ_EXT): /usr/include/netinet/in.h
globals$(OBJ_EXT): /usr/include/rpc/netdb.h
globals$(OBJ_EXT): /usr/include/setjmp.h
globals$(OBJ_EXT): /usr/include/signal.h
globals$(OBJ_EXT): /usr/include/stdint.h
globals$(OBJ_EXT): /usr/include/stdio.h
globals$(OBJ_EXT): /usr/include/stdlib.h
globals$(OBJ_EXT): /usr/include/string.h
globals$(OBJ_EXT): /usr/include/time.h
globals$(OBJ_EXT): /usr/include/unistd.h
globals$(OBJ_EXT): /usr/include/wchar.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
globals$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
globals$(OBJ_EXT): /usr/include/xlocale.h
globals$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
globals$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
globals$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
globals$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
globals$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
globals$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
globals$(OBJ_EXT): INTERN.h
globals$(OBJ_EXT): av.h
globals$(OBJ_EXT): config.h
globals$(OBJ_EXT): cop.h
globals$(OBJ_EXT): cv.h
globals$(OBJ_EXT): embed.h
globals$(OBJ_EXT): embedvar.h
globals$(OBJ_EXT): form.h
globals$(OBJ_EXT): globals.c
globals$(OBJ_EXT): gv.h
globals$(OBJ_EXT): handy.h
globals$(OBJ_EXT): hv.h
globals$(OBJ_EXT): intrpvar.h
globals$(OBJ_EXT): iperlsys.h
globals$(OBJ_EXT): mg.h
globals$(OBJ_EXT): op.h
globals$(OBJ_EXT): opcode.h
globals$(OBJ_EXT): opnames.h
globals$(OBJ_EXT): pad.h
globals$(OBJ_EXT): patchlevel.h
globals$(OBJ_EXT): perl.h
globals$(OBJ_EXT): perlapi.h
globals$(OBJ_EXT): perlio.h
globals$(OBJ_EXT): perlvars.h
globals$(OBJ_EXT): perly.h
globals$(OBJ_EXT): pp.h
globals$(OBJ_EXT): pp_proto.h
globals$(OBJ_EXT): proto.h
globals$(OBJ_EXT): reentr.inc
globals$(OBJ_EXT): regexp.h
globals$(OBJ_EXT): scope.h
globals$(OBJ_EXT): sv.h
globals$(OBJ_EXT): thrdvar.h
globals$(OBJ_EXT): thread.h
globals$(OBJ_EXT): unixish.h
globals$(OBJ_EXT): utf8.h
globals$(OBJ_EXT): util.h
globals$(OBJ_EXT): warnings.h
perlio$(OBJ_EXT): /usr/include/_G_config.h
perlio$(OBJ_EXT): /usr/include/alloca.h
perlio$(OBJ_EXT): /usr/include/arpa/inet.h
perlio$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
perlio$(OBJ_EXT): /usr/include/asm-generic/errno.h
perlio$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
perlio$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
perlio$(OBJ_EXT): /usr/include/asm-generic/param.h
perlio$(OBJ_EXT): /usr/include/asm-generic/socket.h
perlio$(OBJ_EXT): /usr/include/asm-generic/sockios.h
perlio$(OBJ_EXT): /usr/include/ctype.h
perlio$(OBJ_EXT): /usr/include/dirent.h
perlio$(OBJ_EXT): /usr/include/endian.h
perlio$(OBJ_EXT): /usr/include/errno.h
perlio$(OBJ_EXT): /usr/include/fcntl.h
perlio$(OBJ_EXT): /usr/include/features.h
perlio$(OBJ_EXT): /usr/include/getopt.h
perlio$(OBJ_EXT): /usr/include/inttypes.h
perlio$(OBJ_EXT): /usr/include/libio.h
perlio$(OBJ_EXT): /usr/include/limits.h
perlio$(OBJ_EXT): /usr/include/linux/errno.h
perlio$(OBJ_EXT): /usr/include/linux/ioctl.h
perlio$(OBJ_EXT): /usr/include/linux/limits.h
perlio$(OBJ_EXT): /usr/include/linux/param.h
perlio$(OBJ_EXT): /usr/include/locale.h
perlio$(OBJ_EXT): /usr/include/math.h
perlio$(OBJ_EXT): /usr/include/netdb.h
perlio$(OBJ_EXT): /usr/include/netinet/in.h
perlio$(OBJ_EXT): /usr/include/rpc/netdb.h
perlio$(OBJ_EXT): /usr/include/setjmp.h
perlio$(OBJ_EXT): /usr/include/signal.h
perlio$(OBJ_EXT): /usr/include/stdint.h
perlio$(OBJ_EXT): /usr/include/stdio.h
perlio$(OBJ_EXT): /usr/include/stdlib.h
perlio$(OBJ_EXT): /usr/include/string.h
perlio$(OBJ_EXT): /usr/include/time.h
perlio$(OBJ_EXT): /usr/include/unistd.h
perlio$(OBJ_EXT): /usr/include/wchar.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mman.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/mman.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
perlio$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
perlio$(OBJ_EXT): /usr/include/xlocale.h
perlio$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
perlio$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
perlio$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
perlio$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
perlio$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
perlio$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
perlio$(OBJ_EXT): EXTERN.h
perlio$(OBJ_EXT): XSUB.h
perlio$(OBJ_EXT): av.h
perlio$(OBJ_EXT): config.h
perlio$(OBJ_EXT): cop.h
perlio$(OBJ_EXT): cv.h
perlio$(OBJ_EXT): embed.h
perlio$(OBJ_EXT): embedvar.h
perlio$(OBJ_EXT): form.h
perlio$(OBJ_EXT): gv.h
perlio$(OBJ_EXT): handy.h
perlio$(OBJ_EXT): hv.h
perlio$(OBJ_EXT): intrpvar.h
perlio$(OBJ_EXT): iperlsys.h
perlio$(OBJ_EXT): mg.h
perlio$(OBJ_EXT): op.h
perlio$(OBJ_EXT): opcode.h
perlio$(OBJ_EXT): opnames.h
perlio$(OBJ_EXT): pad.h
perlio$(OBJ_EXT): patchlevel.h
perlio$(OBJ_EXT): perl.h
perlio$(OBJ_EXT): perlapi.h
perlio$(OBJ_EXT): perlio.c
perlio$(OBJ_EXT): perlio.h
perlio$(OBJ_EXT): perliol.h
perlio$(OBJ_EXT): perlvars.h
perlio$(OBJ_EXT): perly.h
perlio$(OBJ_EXT): pp.h
perlio$(OBJ_EXT): pp_proto.h
perlio$(OBJ_EXT): proto.h
perlio$(OBJ_EXT): reentr.inc
perlio$(OBJ_EXT): regexp.h
perlio$(OBJ_EXT): scope.h
perlio$(OBJ_EXT): sv.h
perlio$(OBJ_EXT): thrdvar.h
perlio$(OBJ_EXT): thread.h
perlio$(OBJ_EXT): unixish.h
perlio$(OBJ_EXT): utf8.h
perlio$(OBJ_EXT): util.h
perlio$(OBJ_EXT): warnings.h
perlapi$(OBJ_EXT): /usr/include/_G_config.h
perlapi$(OBJ_EXT): /usr/include/alloca.h
perlapi$(OBJ_EXT): /usr/include/arpa/inet.h
perlapi$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
perlapi$(OBJ_EXT): /usr/include/asm-generic/errno.h
perlapi$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
perlapi$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
perlapi$(OBJ_EXT): /usr/include/asm-generic/param.h
perlapi$(OBJ_EXT): /usr/include/asm-generic/socket.h
perlapi$(OBJ_EXT): /usr/include/asm-generic/sockios.h
perlapi$(OBJ_EXT): /usr/include/ctype.h
perlapi$(OBJ_EXT): /usr/include/dirent.h
perlapi$(OBJ_EXT): /usr/include/endian.h
perlapi$(OBJ_EXT): /usr/include/errno.h
perlapi$(OBJ_EXT): /usr/include/fcntl.h
perlapi$(OBJ_EXT): /usr/include/features.h
perlapi$(OBJ_EXT): /usr/include/getopt.h
perlapi$(OBJ_EXT): /usr/include/inttypes.h
perlapi$(OBJ_EXT): /usr/include/libio.h
perlapi$(OBJ_EXT): /usr/include/limits.h
perlapi$(OBJ_EXT): /usr/include/linux/errno.h
perlapi$(OBJ_EXT): /usr/include/linux/ioctl.h
perlapi$(OBJ_EXT): /usr/include/linux/limits.h
perlapi$(OBJ_EXT): /usr/include/linux/param.h
perlapi$(OBJ_EXT): /usr/include/locale.h
perlapi$(OBJ_EXT): /usr/include/math.h
perlapi$(OBJ_EXT): /usr/include/netdb.h
perlapi$(OBJ_EXT): /usr/include/netinet/in.h
perlapi$(OBJ_EXT): /usr/include/rpc/netdb.h
perlapi$(OBJ_EXT): /usr/include/setjmp.h
perlapi$(OBJ_EXT): /usr/include/signal.h
perlapi$(OBJ_EXT): /usr/include/stdint.h
perlapi$(OBJ_EXT): /usr/include/stdio.h
perlapi$(OBJ_EXT): /usr/include/stdlib.h
perlapi$(OBJ_EXT): /usr/include/string.h
perlapi$(OBJ_EXT): /usr/include/time.h
perlapi$(OBJ_EXT): /usr/include/unistd.h
perlapi$(OBJ_EXT): /usr/include/wchar.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
perlapi$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
perlapi$(OBJ_EXT): /usr/include/xlocale.h
perlapi$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
perlapi$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
perlapi$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
perlapi$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
perlapi$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
perlapi$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
perlapi$(OBJ_EXT): EXTERN.h
perlapi$(OBJ_EXT): av.h
perlapi$(OBJ_EXT): config.h
perlapi$(OBJ_EXT): cop.h
perlapi$(OBJ_EXT): cv.h
perlapi$(OBJ_EXT): embed.h
perlapi$(OBJ_EXT): embedvar.h
perlapi$(OBJ_EXT): form.h
perlapi$(OBJ_EXT): gv.h
perlapi$(OBJ_EXT): handy.h
perlapi$(OBJ_EXT): hv.h
perlapi$(OBJ_EXT): intrpvar.h
perlapi$(OBJ_EXT): iperlsys.h
perlapi$(OBJ_EXT): mg.h
perlapi$(OBJ_EXT): op.h
perlapi$(OBJ_EXT): opcode.h
perlapi$(OBJ_EXT): opnames.h
perlapi$(OBJ_EXT): pad.h
perlapi$(OBJ_EXT): patchlevel.h
perlapi$(OBJ_EXT): perl.h
perlapi$(OBJ_EXT): perlapi.c
perlapi$(OBJ_EXT): perlapi.h
perlapi$(OBJ_EXT): perlio.h
perlapi$(OBJ_EXT): perlvars.h
perlapi$(OBJ_EXT): perly.h
perlapi$(OBJ_EXT): pp.h
perlapi$(OBJ_EXT): pp_proto.h
perlapi$(OBJ_EXT): proto.h
perlapi$(OBJ_EXT): reentr.inc
perlapi$(OBJ_EXT): regexp.h
perlapi$(OBJ_EXT): scope.h
perlapi$(OBJ_EXT): sv.h
perlapi$(OBJ_EXT): thrdvar.h
perlapi$(OBJ_EXT): thread.h
perlapi$(OBJ_EXT): unixish.h
perlapi$(OBJ_EXT): utf8.h
perlapi$(OBJ_EXT): util.h
perlapi$(OBJ_EXT): warnings.h
numeric$(OBJ_EXT): /usr/include/_G_config.h
numeric$(OBJ_EXT): /usr/include/alloca.h
numeric$(OBJ_EXT): /usr/include/arpa/inet.h
numeric$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
numeric$(OBJ_EXT): /usr/include/asm-generic/errno.h
numeric$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
numeric$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
numeric$(OBJ_EXT): /usr/include/asm-generic/param.h
numeric$(OBJ_EXT): /usr/include/asm-generic/socket.h
numeric$(OBJ_EXT): /usr/include/asm-generic/sockios.h
numeric$(OBJ_EXT): /usr/include/ctype.h
numeric$(OBJ_EXT): /usr/include/dirent.h
numeric$(OBJ_EXT): /usr/include/endian.h
numeric$(OBJ_EXT): /usr/include/errno.h
numeric$(OBJ_EXT): /usr/include/fcntl.h
numeric$(OBJ_EXT): /usr/include/features.h
numeric$(OBJ_EXT): /usr/include/getopt.h
numeric$(OBJ_EXT): /usr/include/inttypes.h
numeric$(OBJ_EXT): /usr/include/libio.h
numeric$(OBJ_EXT): /usr/include/limits.h
numeric$(OBJ_EXT): /usr/include/linux/errno.h
numeric$(OBJ_EXT): /usr/include/linux/ioctl.h
numeric$(OBJ_EXT): /usr/include/linux/limits.h
numeric$(OBJ_EXT): /usr/include/linux/param.h
numeric$(OBJ_EXT): /usr/include/locale.h
numeric$(OBJ_EXT): /usr/include/math.h
numeric$(OBJ_EXT): /usr/include/netdb.h
numeric$(OBJ_EXT): /usr/include/netinet/in.h
numeric$(OBJ_EXT): /usr/include/rpc/netdb.h
numeric$(OBJ_EXT): /usr/include/setjmp.h
numeric$(OBJ_EXT): /usr/include/signal.h
numeric$(OBJ_EXT): /usr/include/stdint.h
numeric$(OBJ_EXT): /usr/include/stdio.h
numeric$(OBJ_EXT): /usr/include/stdlib.h
numeric$(OBJ_EXT): /usr/include/string.h
numeric$(OBJ_EXT): /usr/include/time.h
numeric$(OBJ_EXT): /usr/include/unistd.h
numeric$(OBJ_EXT): /usr/include/wchar.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
numeric$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
numeric$(OBJ_EXT): /usr/include/xlocale.h
numeric$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
numeric$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
numeric$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
numeric$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
numeric$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
numeric$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
numeric$(OBJ_EXT): EXTERN.h
numeric$(OBJ_EXT): av.h
numeric$(OBJ_EXT): config.h
numeric$(OBJ_EXT): cop.h
numeric$(OBJ_EXT): cv.h
numeric$(OBJ_EXT): embed.h
numeric$(OBJ_EXT): embedvar.h
numeric$(OBJ_EXT): form.h
numeric$(OBJ_EXT): gv.h
numeric$(OBJ_EXT): handy.h
numeric$(OBJ_EXT): hv.h
numeric$(OBJ_EXT): intrpvar.h
numeric$(OBJ_EXT): iperlsys.h
numeric$(OBJ_EXT): mg.h
numeric$(OBJ_EXT): numeric.c
numeric$(OBJ_EXT): op.h
numeric$(OBJ_EXT): opcode.h
numeric$(OBJ_EXT): opnames.h
numeric$(OBJ_EXT): pad.h
numeric$(OBJ_EXT): patchlevel.h
numeric$(OBJ_EXT): perl.h
numeric$(OBJ_EXT): perlio.h
numeric$(OBJ_EXT): perlvars.h
numeric$(OBJ_EXT): perly.h
numeric$(OBJ_EXT): pp.h
numeric$(OBJ_EXT): pp_proto.h
numeric$(OBJ_EXT): proto.h
numeric$(OBJ_EXT): reentr.inc
numeric$(OBJ_EXT): regexp.h
numeric$(OBJ_EXT): scope.h
numeric$(OBJ_EXT): sv.h
numeric$(OBJ_EXT): thrdvar.h
numeric$(OBJ_EXT): thread.h
numeric$(OBJ_EXT): unixish.h
numeric$(OBJ_EXT): utf8.h
numeric$(OBJ_EXT): util.h
numeric$(OBJ_EXT): warnings.h
locale$(OBJ_EXT): /usr/include/_G_config.h
locale$(OBJ_EXT): /usr/include/alloca.h
locale$(OBJ_EXT): /usr/include/arpa/inet.h
locale$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
locale$(OBJ_EXT): /usr/include/asm-generic/errno.h
locale$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
locale$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
locale$(OBJ_EXT): /usr/include/asm-generic/param.h
locale$(OBJ_EXT): /usr/include/asm-generic/socket.h
locale$(OBJ_EXT): /usr/include/asm-generic/sockios.h
locale$(OBJ_EXT): /usr/include/ctype.h
locale$(OBJ_EXT): /usr/include/dirent.h
locale$(OBJ_EXT): /usr/include/endian.h
locale$(OBJ_EXT): /usr/include/errno.h
locale$(OBJ_EXT): /usr/include/fcntl.h
locale$(OBJ_EXT): /usr/include/features.h
locale$(OBJ_EXT): /usr/include/getopt.h
locale$(OBJ_EXT): /usr/include/inttypes.h
locale$(OBJ_EXT): /usr/include/langinfo.h
locale$(OBJ_EXT): /usr/include/libio.h
locale$(OBJ_EXT): /usr/include/limits.h
locale$(OBJ_EXT): /usr/include/linux/errno.h
locale$(OBJ_EXT): /usr/include/linux/ioctl.h
locale$(OBJ_EXT): /usr/include/linux/limits.h
locale$(OBJ_EXT): /usr/include/linux/param.h
locale$(OBJ_EXT): /usr/include/locale.h
locale$(OBJ_EXT): /usr/include/math.h
locale$(OBJ_EXT): /usr/include/netdb.h
locale$(OBJ_EXT): /usr/include/netinet/in.h
locale$(OBJ_EXT): /usr/include/nl_types.h
locale$(OBJ_EXT): /usr/include/rpc/netdb.h
locale$(OBJ_EXT): /usr/include/setjmp.h
locale$(OBJ_EXT): /usr/include/signal.h
locale$(OBJ_EXT): /usr/include/stdint.h
locale$(OBJ_EXT): /usr/include/stdio.h
locale$(OBJ_EXT): /usr/include/stdlib.h
locale$(OBJ_EXT): /usr/include/string.h
locale$(OBJ_EXT): /usr/include/time.h
locale$(OBJ_EXT): /usr/include/unistd.h
locale$(OBJ_EXT): /usr/include/wchar.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
locale$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
locale$(OBJ_EXT): /usr/include/xlocale.h
locale$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
locale$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
locale$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
locale$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
locale$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
locale$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
locale$(OBJ_EXT): EXTERN.h
locale$(OBJ_EXT): av.h
locale$(OBJ_EXT): config.h
locale$(OBJ_EXT): cop.h
locale$(OBJ_EXT): cv.h
locale$(OBJ_EXT): embed.h
locale$(OBJ_EXT): embedvar.h
locale$(OBJ_EXT): form.h
locale$(OBJ_EXT): gv.h
locale$(OBJ_EXT): handy.h
locale$(OBJ_EXT): hv.h
locale$(OBJ_EXT): intrpvar.h
locale$(OBJ_EXT): iperlsys.h
locale$(OBJ_EXT): locale.c
locale$(OBJ_EXT): mg.h
locale$(OBJ_EXT): op.h
locale$(OBJ_EXT): opcode.h
locale$(OBJ_EXT): opnames.h
locale$(OBJ_EXT): pad.h
locale$(OBJ_EXT): patchlevel.h
locale$(OBJ_EXT): perl.h
locale$(OBJ_EXT): perlio.h
locale$(OBJ_EXT): perlvars.h
locale$(OBJ_EXT): perly.h
locale$(OBJ_EXT): pp.h
locale$(OBJ_EXT): pp_proto.h
locale$(OBJ_EXT): proto.h
locale$(OBJ_EXT): reentr.h
locale$(OBJ_EXT): reentr.inc
locale$(OBJ_EXT): regexp.h
locale$(OBJ_EXT): scope.h
locale$(OBJ_EXT): sv.h
locale$(OBJ_EXT): thrdvar.h
locale$(OBJ_EXT): thread.h
locale$(OBJ_EXT): unixish.h
locale$(OBJ_EXT): utf8.h
locale$(OBJ_EXT): util.h
locale$(OBJ_EXT): warnings.h
pp_pack$(OBJ_EXT): /usr/include/_G_config.h
pp_pack$(OBJ_EXT): /usr/include/alloca.h
pp_pack$(OBJ_EXT): /usr/include/arpa/inet.h
pp_pack$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
pp_pack$(OBJ_EXT): /usr/include/asm-generic/errno.h
pp_pack$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
pp_pack$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
pp_pack$(OBJ_EXT): /usr/include/asm-generic/param.h
pp_pack$(OBJ_EXT): /usr/include/asm-generic/socket.h
pp_pack$(OBJ_EXT): /usr/include/asm-generic/sockios.h
pp_pack$(OBJ_EXT): /usr/include/ctype.h
pp_pack$(OBJ_EXT): /usr/include/dirent.h
pp_pack$(OBJ_EXT): /usr/include/endian.h
pp_pack$(OBJ_EXT): /usr/include/errno.h
pp_pack$(OBJ_EXT): /usr/include/fcntl.h
pp_pack$(OBJ_EXT): /usr/include/features.h
pp_pack$(OBJ_EXT): /usr/include/getopt.h
pp_pack$(OBJ_EXT): /usr/include/inttypes.h
pp_pack$(OBJ_EXT): /usr/include/libio.h
pp_pack$(OBJ_EXT): /usr/include/limits.h
pp_pack$(OBJ_EXT): /usr/include/linux/errno.h
pp_pack$(OBJ_EXT): /usr/include/linux/ioctl.h
pp_pack$(OBJ_EXT): /usr/include/linux/limits.h
pp_pack$(OBJ_EXT): /usr/include/linux/param.h
pp_pack$(OBJ_EXT): /usr/include/locale.h
pp_pack$(OBJ_EXT): /usr/include/math.h
pp_pack$(OBJ_EXT): /usr/include/netdb.h
pp_pack$(OBJ_EXT): /usr/include/netinet/in.h
pp_pack$(OBJ_EXT): /usr/include/rpc/netdb.h
pp_pack$(OBJ_EXT): /usr/include/setjmp.h
pp_pack$(OBJ_EXT): /usr/include/signal.h
pp_pack$(OBJ_EXT): /usr/include/stdint.h
pp_pack$(OBJ_EXT): /usr/include/stdio.h
pp_pack$(OBJ_EXT): /usr/include/stdlib.h
pp_pack$(OBJ_EXT): /usr/include/string.h
pp_pack$(OBJ_EXT): /usr/include/time.h
pp_pack$(OBJ_EXT): /usr/include/unistd.h
pp_pack$(OBJ_EXT): /usr/include/wchar.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
pp_pack$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
pp_pack$(OBJ_EXT): /usr/include/xlocale.h
pp_pack$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
pp_pack$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
pp_pack$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
pp_pack$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
pp_pack$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
pp_pack$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
pp_pack$(OBJ_EXT): EXTERN.h
pp_pack$(OBJ_EXT): av.h
pp_pack$(OBJ_EXT): config.h
pp_pack$(OBJ_EXT): cop.h
pp_pack$(OBJ_EXT): cv.h
pp_pack$(OBJ_EXT): embed.h
pp_pack$(OBJ_EXT): embedvar.h
pp_pack$(OBJ_EXT): form.h
pp_pack$(OBJ_EXT): gv.h
pp_pack$(OBJ_EXT): handy.h
pp_pack$(OBJ_EXT): hv.h
pp_pack$(OBJ_EXT): intrpvar.h
pp_pack$(OBJ_EXT): iperlsys.h
pp_pack$(OBJ_EXT): mg.h
pp_pack$(OBJ_EXT): op.h
pp_pack$(OBJ_EXT): opcode.h
pp_pack$(OBJ_EXT): opnames.h
pp_pack$(OBJ_EXT): pad.h
pp_pack$(OBJ_EXT): patchlevel.h
pp_pack$(OBJ_EXT): perl.h
pp_pack$(OBJ_EXT): perlio.h
pp_pack$(OBJ_EXT): perlvars.h
pp_pack$(OBJ_EXT): perly.h
pp_pack$(OBJ_EXT): pp.h
pp_pack$(OBJ_EXT): pp_pack.c
pp_pack$(OBJ_EXT): pp_proto.h
pp_pack$(OBJ_EXT): proto.h
pp_pack$(OBJ_EXT): reentr.inc
pp_pack$(OBJ_EXT): regexp.h
pp_pack$(OBJ_EXT): scope.h
pp_pack$(OBJ_EXT): sv.h
pp_pack$(OBJ_EXT): thrdvar.h
pp_pack$(OBJ_EXT): thread.h
pp_pack$(OBJ_EXT): unixish.h
pp_pack$(OBJ_EXT): utf8.h
pp_pack$(OBJ_EXT): util.h
pp_pack$(OBJ_EXT): warnings.h
pp_sort$(OBJ_EXT): /usr/include/_G_config.h
pp_sort$(OBJ_EXT): /usr/include/alloca.h
pp_sort$(OBJ_EXT): /usr/include/arpa/inet.h
pp_sort$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
pp_sort$(OBJ_EXT): /usr/include/asm-generic/errno.h
pp_sort$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
pp_sort$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
pp_sort$(OBJ_EXT): /usr/include/asm-generic/param.h
pp_sort$(OBJ_EXT): /usr/include/asm-generic/socket.h
pp_sort$(OBJ_EXT): /usr/include/asm-generic/sockios.h
pp_sort$(OBJ_EXT): /usr/include/ctype.h
pp_sort$(OBJ_EXT): /usr/include/dirent.h
pp_sort$(OBJ_EXT): /usr/include/endian.h
pp_sort$(OBJ_EXT): /usr/include/errno.h
pp_sort$(OBJ_EXT): /usr/include/fcntl.h
pp_sort$(OBJ_EXT): /usr/include/features.h
pp_sort$(OBJ_EXT): /usr/include/getopt.h
pp_sort$(OBJ_EXT): /usr/include/inttypes.h
pp_sort$(OBJ_EXT): /usr/include/libio.h
pp_sort$(OBJ_EXT): /usr/include/limits.h
pp_sort$(OBJ_EXT): /usr/include/linux/errno.h
pp_sort$(OBJ_EXT): /usr/include/linux/ioctl.h
pp_sort$(OBJ_EXT): /usr/include/linux/limits.h
pp_sort$(OBJ_EXT): /usr/include/linux/param.h
pp_sort$(OBJ_EXT): /usr/include/locale.h
pp_sort$(OBJ_EXT): /usr/include/math.h
pp_sort$(OBJ_EXT): /usr/include/netdb.h
pp_sort$(OBJ_EXT): /usr/include/netinet/in.h
pp_sort$(OBJ_EXT): /usr/include/rpc/netdb.h
pp_sort$(OBJ_EXT): /usr/include/setjmp.h
pp_sort$(OBJ_EXT): /usr/include/signal.h
pp_sort$(OBJ_EXT): /usr/include/stdint.h
pp_sort$(OBJ_EXT): /usr/include/stdio.h
pp_sort$(OBJ_EXT): /usr/include/stdlib.h
pp_sort$(OBJ_EXT): /usr/include/string.h
pp_sort$(OBJ_EXT): /usr/include/time.h
pp_sort$(OBJ_EXT): /usr/include/unistd.h
pp_sort$(OBJ_EXT): /usr/include/wchar.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
pp_sort$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
pp_sort$(OBJ_EXT): /usr/include/xlocale.h
pp_sort$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
pp_sort$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
pp_sort$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
pp_sort$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
pp_sort$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
pp_sort$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
pp_sort$(OBJ_EXT): EXTERN.h
pp_sort$(OBJ_EXT): av.h
pp_sort$(OBJ_EXT): config.h
pp_sort$(OBJ_EXT): cop.h
pp_sort$(OBJ_EXT): cv.h
pp_sort$(OBJ_EXT): embed.h
pp_sort$(OBJ_EXT): embedvar.h
pp_sort$(OBJ_EXT): form.h
pp_sort$(OBJ_EXT): gv.h
pp_sort$(OBJ_EXT): handy.h
pp_sort$(OBJ_EXT): hv.h
pp_sort$(OBJ_EXT): intrpvar.h
pp_sort$(OBJ_EXT): iperlsys.h
pp_sort$(OBJ_EXT): mg.h
pp_sort$(OBJ_EXT): op.h
pp_sort$(OBJ_EXT): opcode.h
pp_sort$(OBJ_EXT): opnames.h
pp_sort$(OBJ_EXT): pad.h
pp_sort$(OBJ_EXT): patchlevel.h
pp_sort$(OBJ_EXT): perl.h
pp_sort$(OBJ_EXT): perlio.h
pp_sort$(OBJ_EXT): perlvars.h
pp_sort$(OBJ_EXT): perly.h
pp_sort$(OBJ_EXT): pp.h
pp_sort$(OBJ_EXT): pp_proto.h
pp_sort$(OBJ_EXT): pp_sort.c
pp_sort$(OBJ_EXT): proto.h
pp_sort$(OBJ_EXT): reentr.inc
pp_sort$(OBJ_EXT): regexp.h
pp_sort$(OBJ_EXT): scope.h
pp_sort$(OBJ_EXT): sv.h
pp_sort$(OBJ_EXT): thrdvar.h
pp_sort$(OBJ_EXT): thread.h
pp_sort$(OBJ_EXT): unixish.h
pp_sort$(OBJ_EXT): utf8.h
pp_sort$(OBJ_EXT): util.h
pp_sort$(OBJ_EXT): warnings.h
miniperlmain$(OBJ_EXT): /usr/include/_G_config.h
miniperlmain$(OBJ_EXT): /usr/include/alloca.h
miniperlmain$(OBJ_EXT): /usr/include/arpa/inet.h
miniperlmain$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
miniperlmain$(OBJ_EXT): /usr/include/asm-generic/errno.h
miniperlmain$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
miniperlmain$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
miniperlmain$(OBJ_EXT): /usr/include/asm-generic/param.h
miniperlmain$(OBJ_EXT): /usr/include/asm-generic/socket.h
miniperlmain$(OBJ_EXT): /usr/include/asm-generic/sockios.h
miniperlmain$(OBJ_EXT): /usr/include/ctype.h
miniperlmain$(OBJ_EXT): /usr/include/dirent.h
miniperlmain$(OBJ_EXT): /usr/include/endian.h
miniperlmain$(OBJ_EXT): /usr/include/errno.h
miniperlmain$(OBJ_EXT): /usr/include/fcntl.h
miniperlmain$(OBJ_EXT): /usr/include/features.h
miniperlmain$(OBJ_EXT): /usr/include/getopt.h
miniperlmain$(OBJ_EXT): /usr/include/inttypes.h
miniperlmain$(OBJ_EXT): /usr/include/libio.h
miniperlmain$(OBJ_EXT): /usr/include/limits.h
miniperlmain$(OBJ_EXT): /usr/include/linux/errno.h
miniperlmain$(OBJ_EXT): /usr/include/linux/ioctl.h
miniperlmain$(OBJ_EXT): /usr/include/linux/limits.h
miniperlmain$(OBJ_EXT): /usr/include/linux/param.h
miniperlmain$(OBJ_EXT): /usr/include/locale.h
miniperlmain$(OBJ_EXT): /usr/include/math.h
miniperlmain$(OBJ_EXT): /usr/include/netdb.h
miniperlmain$(OBJ_EXT): /usr/include/netinet/in.h
miniperlmain$(OBJ_EXT): /usr/include/rpc/netdb.h
miniperlmain$(OBJ_EXT): /usr/include/setjmp.h
miniperlmain$(OBJ_EXT): /usr/include/signal.h
miniperlmain$(OBJ_EXT): /usr/include/stdint.h
miniperlmain$(OBJ_EXT): /usr/include/stdio.h
miniperlmain$(OBJ_EXT): /usr/include/stdlib.h
miniperlmain$(OBJ_EXT): /usr/include/string.h
miniperlmain$(OBJ_EXT): /usr/include/time.h
miniperlmain$(OBJ_EXT): /usr/include/unistd.h
miniperlmain$(OBJ_EXT): /usr/include/wchar.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
miniperlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
miniperlmain$(OBJ_EXT): /usr/include/xlocale.h
miniperlmain$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
miniperlmain$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
miniperlmain$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
miniperlmain$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
miniperlmain$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
miniperlmain$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
miniperlmain$(OBJ_EXT): EXTERN.h
miniperlmain$(OBJ_EXT): av.h
miniperlmain$(OBJ_EXT): config.h
miniperlmain$(OBJ_EXT): cop.h
miniperlmain$(OBJ_EXT): cv.h
miniperlmain$(OBJ_EXT): embed.h
miniperlmain$(OBJ_EXT): embedvar.h
miniperlmain$(OBJ_EXT): form.h
miniperlmain$(OBJ_EXT): gv.h
miniperlmain$(OBJ_EXT): handy.h
miniperlmain$(OBJ_EXT): hv.h
miniperlmain$(OBJ_EXT): intrpvar.h
miniperlmain$(OBJ_EXT): iperlsys.h
miniperlmain$(OBJ_EXT): mg.h
miniperlmain$(OBJ_EXT): miniperlmain.c
miniperlmain$(OBJ_EXT): op.h
miniperlmain$(OBJ_EXT): opcode.h
miniperlmain$(OBJ_EXT): opnames.h
miniperlmain$(OBJ_EXT): pad.h
miniperlmain$(OBJ_EXT): patchlevel.h
miniperlmain$(OBJ_EXT): perl.h
miniperlmain$(OBJ_EXT): perlio.h
miniperlmain$(OBJ_EXT): perlvars.h
miniperlmain$(OBJ_EXT): perly.h
miniperlmain$(OBJ_EXT): pp.h
miniperlmain$(OBJ_EXT): pp_proto.h
miniperlmain$(OBJ_EXT): proto.h
miniperlmain$(OBJ_EXT): reentr.inc
miniperlmain$(OBJ_EXT): regexp.h
miniperlmain$(OBJ_EXT): scope.h
miniperlmain$(OBJ_EXT): sv.h
miniperlmain$(OBJ_EXT): thrdvar.h
miniperlmain$(OBJ_EXT): thread.h
miniperlmain$(OBJ_EXT): unixish.h
miniperlmain$(OBJ_EXT): utf8.h
miniperlmain$(OBJ_EXT): util.h
miniperlmain$(OBJ_EXT): warnings.h
perlmain$(OBJ_EXT): /usr/include/_G_config.h
perlmain$(OBJ_EXT): /usr/include/alloca.h
perlmain$(OBJ_EXT): /usr/include/arpa/inet.h
perlmain$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
perlmain$(OBJ_EXT): /usr/include/asm-generic/errno.h
perlmain$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
perlmain$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
perlmain$(OBJ_EXT): /usr/include/asm-generic/param.h
perlmain$(OBJ_EXT): /usr/include/asm-generic/socket.h
perlmain$(OBJ_EXT): /usr/include/asm-generic/sockios.h
perlmain$(OBJ_EXT): /usr/include/ctype.h
perlmain$(OBJ_EXT): /usr/include/dirent.h
perlmain$(OBJ_EXT): /usr/include/endian.h
perlmain$(OBJ_EXT): /usr/include/errno.h
perlmain$(OBJ_EXT): /usr/include/fcntl.h
perlmain$(OBJ_EXT): /usr/include/features.h
perlmain$(OBJ_EXT): /usr/include/getopt.h
perlmain$(OBJ_EXT): /usr/include/inttypes.h
perlmain$(OBJ_EXT): /usr/include/libio.h
perlmain$(OBJ_EXT): /usr/include/limits.h
perlmain$(OBJ_EXT): /usr/include/linux/errno.h
perlmain$(OBJ_EXT): /usr/include/linux/ioctl.h
perlmain$(OBJ_EXT): /usr/include/linux/limits.h
perlmain$(OBJ_EXT): /usr/include/linux/param.h
perlmain$(OBJ_EXT): /usr/include/locale.h
perlmain$(OBJ_EXT): /usr/include/math.h
perlmain$(OBJ_EXT): /usr/include/netdb.h
perlmain$(OBJ_EXT): /usr/include/netinet/in.h
perlmain$(OBJ_EXT): /usr/include/rpc/netdb.h
perlmain$(OBJ_EXT): /usr/include/setjmp.h
perlmain$(OBJ_EXT): /usr/include/signal.h
perlmain$(OBJ_EXT): /usr/include/stdint.h
perlmain$(OBJ_EXT): /usr/include/stdio.h
perlmain$(OBJ_EXT): /usr/include/stdlib.h
perlmain$(OBJ_EXT): /usr/include/string.h
perlmain$(OBJ_EXT): /usr/include/time.h
perlmain$(OBJ_EXT): /usr/include/unistd.h
perlmain$(OBJ_EXT): /usr/include/wchar.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
perlmain$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
perlmain$(OBJ_EXT): /usr/include/xlocale.h
perlmain$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
perlmain$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
perlmain$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
perlmain$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
perlmain$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
perlmain$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
perlmain$(OBJ_EXT): EXTERN.h
perlmain$(OBJ_EXT): av.h
perlmain$(OBJ_EXT): config.h
perlmain$(OBJ_EXT): cop.h
perlmain$(OBJ_EXT): cv.h
perlmain$(OBJ_EXT): embed.h
perlmain$(OBJ_EXT): embedvar.h
perlmain$(OBJ_EXT): form.h
perlmain$(OBJ_EXT): gv.h
perlmain$(OBJ_EXT): handy.h
perlmain$(OBJ_EXT): hv.h
perlmain$(OBJ_EXT): intrpvar.h
perlmain$(OBJ_EXT): iperlsys.h
perlmain$(OBJ_EXT): mg.h
perlmain$(OBJ_EXT): op.h
perlmain$(OBJ_EXT): opcode.h
perlmain$(OBJ_EXT): opnames.h
perlmain$(OBJ_EXT): pad.h
perlmain$(OBJ_EXT): patchlevel.h
perlmain$(OBJ_EXT): perl.h
perlmain$(OBJ_EXT): perlio.h
perlmain$(OBJ_EXT): perlmain.c
perlmain$(OBJ_EXT): perlvars.h
perlmain$(OBJ_EXT): perly.h
perlmain$(OBJ_EXT): pp.h
perlmain$(OBJ_EXT): pp_proto.h
perlmain$(OBJ_EXT): proto.h
perlmain$(OBJ_EXT): reentr.inc
perlmain$(OBJ_EXT): regexp.h
perlmain$(OBJ_EXT): scope.h
perlmain$(OBJ_EXT): sv.h
perlmain$(OBJ_EXT): thrdvar.h
perlmain$(OBJ_EXT): thread.h
perlmain$(OBJ_EXT): unixish.h
perlmain$(OBJ_EXT): utf8.h
perlmain$(OBJ_EXT): util.h
perlmain$(OBJ_EXT): warnings.h
opmini$(OBJ_EXT): /usr/include/_G_config.h
opmini$(OBJ_EXT): /usr/include/alloca.h
opmini$(OBJ_EXT): /usr/include/arpa/inet.h
opmini$(OBJ_EXT): /usr/include/asm-generic/errno-base.h
opmini$(OBJ_EXT): /usr/include/asm-generic/errno.h
opmini$(OBJ_EXT): /usr/include/asm-generic/ioctl.h
opmini$(OBJ_EXT): /usr/include/asm-generic/ioctls.h
opmini$(OBJ_EXT): /usr/include/asm-generic/param.h
opmini$(OBJ_EXT): /usr/include/asm-generic/socket.h
opmini$(OBJ_EXT): /usr/include/asm-generic/sockios.h
opmini$(OBJ_EXT): /usr/include/ctype.h
opmini$(OBJ_EXT): /usr/include/dirent.h
opmini$(OBJ_EXT): /usr/include/endian.h
opmini$(OBJ_EXT): /usr/include/errno.h
opmini$(OBJ_EXT): /usr/include/fcntl.h
opmini$(OBJ_EXT): /usr/include/features.h
opmini$(OBJ_EXT): /usr/include/getopt.h
opmini$(OBJ_EXT): /usr/include/inttypes.h
opmini$(OBJ_EXT): /usr/include/libio.h
opmini$(OBJ_EXT): /usr/include/limits.h
opmini$(OBJ_EXT): /usr/include/linux/errno.h
opmini$(OBJ_EXT): /usr/include/linux/ioctl.h
opmini$(OBJ_EXT): /usr/include/linux/limits.h
opmini$(OBJ_EXT): /usr/include/linux/param.h
opmini$(OBJ_EXT): /usr/include/locale.h
opmini$(OBJ_EXT): /usr/include/math.h
opmini$(OBJ_EXT): /usr/include/netdb.h
opmini$(OBJ_EXT): /usr/include/netinet/in.h
opmini$(OBJ_EXT): /usr/include/rpc/netdb.h
opmini$(OBJ_EXT): /usr/include/setjmp.h
opmini$(OBJ_EXT): /usr/include/signal.h
opmini$(OBJ_EXT): /usr/include/stdint.h
opmini$(OBJ_EXT): /usr/include/stdio.h
opmini$(OBJ_EXT): /usr/include/stdlib.h
opmini$(OBJ_EXT): /usr/include/string.h
opmini$(OBJ_EXT): /usr/include/time.h
opmini$(OBJ_EXT): /usr/include/unistd.h
opmini$(OBJ_EXT): /usr/include/wchar.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/errno.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctl.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/ioctls.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/param.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/socket.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/asm/sockios.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/confname.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/dirent.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/environments.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/errno.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fcntl.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_val.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_valf.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/huge_vall.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/in.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/inf.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctl-types.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ioctls.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipc.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/ipctypes.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/local_lim.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/locale.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathdef.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/nan.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/netdb.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix1_lim.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix2_lim.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/posix_opt.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/predefs.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sem.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/setjmp.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigaction.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigcontext.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/siginfo.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/signum.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigset.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigstack.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sigthread.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sockaddr.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/socket.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stat.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uio.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/xopen_lim.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/file.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ioctl.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ipc.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/param.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sem.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/socket.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/stat.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ttydefaults.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/ucontext.h
opmini$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/uio.h
opmini$(OBJ_EXT): /usr/include/xlocale.h
opmini$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h
opmini$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h
opmini$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h
opmini$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h
opmini$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h
opmini$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h
opmini$(OBJ_EXT): EXTERN.h
opmini$(OBJ_EXT): XSUB.h
opmini$(OBJ_EXT): av.h
opmini$(OBJ_EXT): config.h
opmini$(OBJ_EXT): cop.h
opmini$(OBJ_EXT): cv.h
opmini$(OBJ_EXT): embed.h
opmini$(OBJ_EXT): embedvar.h
opmini$(OBJ_EXT): form.h
opmini$(OBJ_EXT): gv.h
opmini$(OBJ_EXT): handy.h
opmini$(OBJ_EXT): hv.h
opmini$(OBJ_EXT): intrpvar.h
opmini$(OBJ_EXT): iperlsys.h
opmini$(OBJ_EXT): keywords.h
opmini$(OBJ_EXT): mg.h
opmini$(OBJ_EXT): op.h
opmini$(OBJ_EXT): opcode.h
opmini$(OBJ_EXT): opmini.c
opmini$(OBJ_EXT): opnames.h
opmini$(OBJ_EXT): pad.h
opmini$(OBJ_EXT): patchlevel.h
opmini$(OBJ_EXT): perl.h
opmini$(OBJ_EXT): perlapi.h
opmini$(OBJ_EXT): perlio.h
opmini$(OBJ_EXT): perlvars.h
opmini$(OBJ_EXT): perly.h
opmini$(OBJ_EXT): pp.h
opmini$(OBJ_EXT): pp_proto.h
opmini$(OBJ_EXT): proto.h
opmini$(OBJ_EXT): reentr.inc
opmini$(OBJ_EXT): regexp.h
opmini$(OBJ_EXT): scope.h
opmini$(OBJ_EXT): sv.h
opmini$(OBJ_EXT): thrdvar.h
opmini$(OBJ_EXT): thread.h
opmini$(OBJ_EXT): unixish.h
opmini$(OBJ_EXT): utf8.h
opmini$(OBJ_EXT): util.h
opmini$(OBJ_EXT): warnings.h
cflags: cflags.SH ./config.sh ; /bin/sh cflags.SH
makeaperl: makeaperl.SH ./config.sh ; /bin/sh makeaperl.SH
makedir: makedir.SH ./config.sh ; /bin/sh makedir.SH
myconfig: myconfig.SH ./config.sh ; /bin/sh myconfig.SH
writemain: writemain.SH ./config.sh ; /bin/sh writemain.SH
pod/Makefile: pod/Makefile.SH ./config.sh ; /bin/sh pod/Makefile.SH
# WARNING: Put nothing here or make depend will gobble it up!
