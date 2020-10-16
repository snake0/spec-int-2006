@echo off
rem This version of buildtools.bat last updated by Cloyce
rem $Id: buildtools.bat 3499 2005-12-18 03:36:56Z cloyce $

rem Insist on command extensions.  The "cd" will clear any
rem lingering error status ("color" does not!) and the color
rem command is only recognized if we have extensions enabled.
cd >nul:
color
if errorlevel 1 goto nocmdext
goto efcmdext
:nocmdext
echo.
echo Sorry, we cannot build the tools unless you have command
echo extensions enabled.  Please see "cmd /?" and "if /?".
echo.
goto end
:efcmdext

echo Tools build started on
hostname
echo at
date/t

rem if the SPEC variable is set, and it points to something that looks
rem reasonable, then use that, otherwise fall through
if "%SPEC%."=="." goto SPEC_env_not_defined
if not defined SPEC goto SPEC_env_not_defined
if exist %SPEC%\bin\runspec goto SPEC_env_defined
:SPEC_env_not_defined

rem go ahead and fetch the path, thanks to JH.  
rem Is there any easier way to get this info?
if exist %temp%\set_spec_loc.bat del /F /Q %temp%\set_spec_loc.bat
echo >%temp%\set_spec_loc.bat set SPEC=^^
cd >>%temp%\set_spec_loc.bat
call %temp%\set_spec_loc.bat
del /F /Q %temp%\set_spec_loc.bat
set SPEC=%SPEC%\..\..

:SPEC_env_defined
if defined SPEC goto gotspec
rem It should not be possible to get to this code...
echo You have not defined the SPEC environment variable.
echo Strictly speaking, it is not necessary to set this by hand before
echo attempting to build the tools.  However, it may lead to more sanity.
echo We recommend setting this by hand to the path of the top-level CPU2006
echo tree.  For example, if your CPU2006 distribution is in C:\special_k\cpu2006
echo you would execute
echo   set SPEC=c:\special_k\cpu2006
echo Hit enter to proceed without setting it, or CTRL-C to exit now.
pause
:gotspec

echo.
set perlver=5.8.7
set perlroot=c:\specperl
set specperl=%perlroot%\%perlver%
echo ============================================================
echo buildtools.bat
echo.
echo This procedure will attempt to build the SPEC CPU2006 tools
echo for Windows.  Notably, it will attempt to build Perl into the
echo directory %perlroot%.  If that is not satisfactory (for
echo example, because you already have something you like better
echo in %perlroot%), press Control-C **now**, and make a backup
echo copy of %perlroot%.
echo ============================================================
echo.
pause

if defined CPU goto efcpudef
rem When there's a non-X86 version of Windows, uncomment the section below
rem echo Sorry, we cannot build the tools unless the variable CPU is 
rem echo defined.  Please set it, typically to one of these values:
rem echo .
rem echo      set CPU=i386
rem echo or
rem echo      set CPU=ALPHA
rem echo .
rem echo and try again.
rem goto end
set CPU=i386
:efcpudef

if "%cpu%."=="i386." path=%specperl%\bin\MSWin32-x86;%path%
if "%cpu%."=="ia32." path=%specperl%\bin\MSWin32-x86;%path%
if "%cpu%."=="ia64." path=%specperl%\bin\MSWin32-x86;%path%
path=%perlroot%\bin;%specperl%\bin\MSWin32-%cpu%;%path%

rem Clear out the Perl library path hints
set SPECPERLLIB=
set PERLLIB=

rem Turn on the parts of the build that user has requested.
rem To just do the final copy to destinations:
rem      set INSTALLONLY=1
rem
rem To turn on just step "x":
rem      set SKIPALL=1
rem      set DOx=1
rem
rem WARNING once you start setting any of these, they tend to be sticky
rem unless you exit the command interpreter.  So the following will NOT
rem do what you expect:
rem      set SKIPALL=1
rem      set DOMAKE=1
rem      buildtools       <- builds only make
rem      set INSTALLONLY=1
rem      buildtools       <- unexpectedly does nothing
rem
rem The above fails to do what you expect because SKIPCOPY is still set 
rem as a result of the first buildtools.  But the following will work
rem just fine:
rem      cmd/q
rem      set SKIPALL=1
rem      set DOMAKE=1
rem      buildtools       <- builds only make
rem      exit
rem      cmd/q
rem      set INSTALLONLY=1
rem      buildtools       <- just does the final copy
rem
rem The Unix version of this script does something like this before 
rem each build step:
rem      if defined DOx or (not defined SKIPcategory and not defined SKIPx)

rem down into SKIPxxx now.  (Thus the above WARNING.)

if not defined INSTALLONLY goto efinonly
  set SKIPCLEAN=1
  set SKIPNONPERL=1
  set SKIPPERL=1
  set SKIPPERL2=1
:efinonly
if not defined SKIPALL goto efskipall
  set SKIPTOOLSRM=1
  set SKIPCLEAN=1
  set SKIPNONPERL=1
  set SKIPPERL=1
  set SKIPPERL2=1
  set SKIPCOPY=1
:efskipall

rem Being able to debug is more important that prettiness
rem echo on
rem ...but only sometimes (the informationals get harder to read)
@echo off

if defined DOTOOLSRM goto toolsrm
if defined SKIPTOOLSRM goto notoolsinst
:toolsrm
if exist %SPEC%\bin\specperl.exe goto toolsinst
if exist %SPEC%\SUMS.tools goto toolsinst
if exist %SPEC%\bin\lib goto toolsinst
goto notoolsinst
:toolsinst
echo Removing previous tools installation...
rem The one-line equivalent under Unix turns into this hack to write a batch
rem file and then call it.  At least we can build the batch file using Perl...
if not exist %SPEC%\bin\specperl.exe goto noperl
if not exist %temp%\toolsdel.bat goto no_del_toolsdel
del /F /Q %temp%\toolsdel.bat
:no_del_toolsdel
%SPEC%\bin\specperl -ne "@f=split; next unless m#bin/#; $_=$f[3]; s#^#$ENV{SPEC}/#; s#\\#/#g; if (-f $_) { unlink $_; } elsif (-d $_) { s#/#\\#g; print """rmdir /Q /S $_\n"""; }" %SPEC%\SUMS.tools > %temp%\toolsdel.bat
call %temp%\toolsdel.bat
del /F /Q %temp%\toolsdel.bat
rem Now fall through in case some things were missed by toolsdel.bat
rem goto tools_rm_done
:noperl
rem Okay, _some_ of the tools files are present, but evidently specperl is
rem missing.  So make a best effort (it'll probably be good enough) and go
rem on.
rmdir /Q /S %SPEC%\bin\lib
del   /Q /F %SPEC%\bin\*.exe
del   /Q /F %SPEC%\bin\*.dll
del   /Q /F %SPEC%\bin\MANIFEST.pl
del   /Q /F %SPEC%\SUMS.tools
del   /Q /F %SPEC%\packagename
:tools_rm_done
echo Finished removing old tools install
:notoolsinst

if defined DOCLEAN goto clean
if defined SKIPCLEAN goto skipclean
:clean
echo "================================================================"
echo "=== Removing remnants of previous builds                     ==="
echo "================================================================"
echo "=== This will cause a lot of errors, don't worry about them! ==="
echo "================================================================"
rem SPECPERLLIB needs to be set because all of the Perl module makefiles
rem use Perl versions of Unix commands to do cleaning.  Sheesh.
set SPECPERLLIB=%perlroot%;%perlroot%\lib;%perlroot%\site\lib;%perlroot%\lib\site
FOR /D %%D in (*) DO CALL :clean_tree %%D
CALL :do_clean_tree expat-*\lib Makefile.nt
set SPECPERLLIB=
rem Some of this stuff will only be around if the tree was used for a
rem non-Windows build
del /S /Q /F config.cache Makefile.old autom4te.cache
cd Compress-Zlib*
del /S /Q /F *.bak
del    /Q /F test.gz
cd ..
cd perl-*
CALL win32\distclean
cd win32
nmake distclean
cd ..
rmdir /S /Q win32\html
rmdir /S /Q win32\ext\Win32\blib
rmdir /S /Q ext\Encode\blib
rmdir /S /Q ext\Unicode\Normalize\blib
del /S /Q /F t\runltmp*
del /S /Q /F t\Recurs
del /S /Q /F t\err
del /S /Q /F t\swtest.pm
del /S /Q /F .config
del    /Q /F config.sh
del    /Q /F Policy.sh
rmdir  /Q /S UU
cd ..
FOR /D %%D in (IO-string* MIME-tools*) DO (
cd %%D
rmdir /S /Q testout
cd ..
)
cd XML-SAX*
rmdir /S /Q t\lib
cd ..

cd libwww-perl*
del /Q /F t\CAN_TALK_TO_OURSELF
cd ..

cd bzip2*
del /Q /F bz2.lib
cd ..

cd expat*
del /S /Q /F tests\*.o*
cd ..

cd make-*
rmdir /S /Q tests\work
cd ..

cd tar*
cd rmt
rmdir /S /Q .deps
del /F /Q rmt
del /F /Q Makefile
cd ..\..

del /S /Q /F .gdb*
rmdir /S /Q %perlroot%
rem Remove the libs and include files in the output directory
rmdir /S /Q ..\output

rem Finally, anything else that might've been missed by poorly-written Makefiles
del /S /F /Q *.pdb
del /S /F /Q *.exe
del /S /F /Q *.ilk
del /S /F /Q *.obj
rem Can't do this because the filesystem isn't case-sensitive...
rem del /S /F /Q *.lib
del /S /F /Q *.idb
if defined CLEANONLY goto end
:skipclean

mkdir ..\output
mkdir ..\output\lib
mkdir ..\output\include

if defined DOMAKE goto make
if defined SKIPNONPERL goto skipmake
if defined SKIPMAKE goto skipmake
:make
echo ============================================================
echo Building make
echo ============================================================
cd make*
nmake -nologo /f NMakefile clean
nmake -nologo /f NMakefile
if exist WinRel\make.exe goto makemakeok
echo make seems to have not been built.  Please examine the output to
echo this point and fix the problem.
goto end
:makemakeok
dir WinRel\make.exe WinDebug\make.exe
if "%PAUSE%"=="yes" pause
cd ..
:skipmake

if defined DOBZIP2 goto bzip2
if defined SKIPNONPERL goto skipbzip2
if defined SKIPBZIP2 goto skipbzip2
:bzip2
echo ============================================================
echo Building bzip2
echo ============================================================
cd bzip2*
nmake -nologo /f makefile.msc clean
nmake -nologo /f makefile.msc
copy libbz2.lib bz2.lib
if exist bzip2.exe goto bzip2makeok
echo bzip2 seems to have not been built.  Please examine the output to
echo this point and fix the problem.
goto end
:bzip2makeok
dir bzip2.exe bz2.lib
if "%PAUSE%"=="yes" pause
cd ..
:skipbzip2

if defined DOTAR goto tar
if defined SKIPNONPERL goto skiptar
if defined SKIPTAR goto skiptar
:tar
echo ============================================================
echo Building TAR
echo ============================================================
cd nttar*
nmake -nologo /f Makefile.nt clean
nmake -nologo /f Makefile.nt
if exist tar.exe goto tarmakeok
echo tar seems to have not been built.  Please examine the output to
echo this point and fix the problem.
goto end
:tarmakeok
dir tar.exe
if "%PAUSE%"=="yes" pause
cd ..
:skiptar

if defined DOMD5 goto md5
if defined SKIPNONPERL goto skipmd5
if defined SKIPMD5 goto skipmd5
:md5
echo ============================================================
echo Building md5sum
echo ============================================================
cd specmd5sum*
nmake -nologo /f Makefile.nt clean
nmake -nologo /f Makefile.nt
if exist specmd5sum.exe goto md5summakeok
echo specmd5sum seems to have not been built.  Please examine the output to
echo this point and fix the problem.
goto end
:md5summakeok
dir specmd5sum.exe
if "%PAUSE%"=="yes" pause
cd ..
:skipmd5

if defined DOSPECINVOKE goto specinvoke
if defined SKIPNONPERL goto skipinvok
if defined SKIPSPECINVOKE goto skipinvok
:specinovke
echo ============================================================
echo Building specinvoke
echo ============================================================
cd specinvoke*
nmake -nologo /f Makefile.nt clean
nmake -nologo /f Makefile.nt
if exist specinvoke.exe goto invokemakeok
echo specinvoke seems to have not been built.  Please examine the output to
echo this point and fix the problem.
goto end
:invokemakeok
dir specinvoke.exe
dir specinvoke_pm.exe
if "%PAUSE%"=="yes" pause
cd ..
:skipinvok

if defined DOEXPAT goto expat
if defined SKIPPERL goto skipexpat
if defined SKIPEXPAT goto skipexpat
:expat
echo ============================================================
echo Building EXPAT
echo ============================================================
cd expat*\lib
nmake -nologo /f Makefile.nt clean
nmake -nologo /f Makefile.nt
if exist libexpatMT.lib goto expatmakeok
echo expat seems to have not been built.  Please examine the output to
echo this point and fix the problem.
goto end
:expatmakeok
copy libexpatMT.lib ..\..\..\output\lib
copy expat*.h ..\..\..\output\include
cd ..\..
dir ..\output\lib\libexpatMT.lib
if "%PAUSE%"=="yes" pause
:skipexpat

set SPECPERLLIB=C:\specperl;C:\specperl\lib;C:\specperl\site\lib;C:\specperl\lib\site;..\lib

if defined DOPERL goto perl
if defined SKIPPERL goto skipperl
:perl
echo ============================================================
echo Building PERL
echo ============================================================
cd perl*\win32
nmake -nologo clean
nmake -nologo install
if exist C:\specperl\bin\perl.exe goto perlmakeok
echo Perl seems to have not been built.  Please examine the output to
echo this point and fix the problem.
goto end
:perlmakeok
if "%PAUSE%"=="yes" pause
cd ..\..
:skipperl

echo ============================================================
echo About to build Perl modules.
echo This would be a good time to cut-n-paste the build log up to
echo this point.
echo ============================================================
pause

if defined DOPERL2 goto stringy
if defined SKIPPERL2 goto skipstring
:stringy
echo ============================================================
echo Building PERL module (IO-Stringy)
echo ============================================================
cd IO-stringy*
nmake -nologo clean
perl Makefile.PL
nmake -nologo UNINST=1 install
if "%PAUSE%"=="yes" pause
cd ..
:skipstring

if defined DOPERL2 goto gd
if defined SKIPPERL2 goto skipgd
:gd
echo ============================================================
echo Building PERL module (GD)
echo ============================================================
cd GD-*
nmake -nologo clean
perl Makefile.PL
nmake -nologo UNINST=1 install
if "%PAUSE%"=="yes" pause
cd ..
:skipgd

if defined DOPERL2 goto compzlib
if defined SKIPPERL2 goto skipcomp
:compzlib
echo ============================================================
echo Building PERL module (Compress::Zlib)
echo ============================================================
cd Compress-Zlib-*
nmake -nologo clean
perl Makefile.PL
nmake -nologo UNINST=1 install
if "%PAUSE%"=="yes" pause
cd ..
:skipcomp

if defined DOPERL2 goto compbzip
if defined SKIPPERL2 goto skipcomp2
:compbzip
echo ============================================================
echo Building PERL module (Compress::Bzip2)
echo ============================================================
cd Compress-Bzip2-*
set BUILD_BZLIB=1
nmake -nologo clean
perl Makefile.PL
nmake -nologo UNINST=1 install
if "%PAUSE%"=="yes" pause
cd ..
:skipcomp2

if defined DOPERL2 goto mailtools
if defined SKIPPERL2 goto skipmail
:mailtools
echo ============================================================
echo Building PERL module (MailTools)
echo ============================================================
cd MailTools-*
nmake -nologo clean
perl Makefile.PL
nmake -nologo UNINST=1 install
if "%PAUSE%"=="yes" pause
cd ..
:skipmail

if defined DOPERL2 goto mimetools
if defined SKIPPERL2 goto skipmimet
:mimetools
echo ============================================================
echo Building PERL module (MIME-tools)
echo ============================================================
cd MIME-tools-*
nmake -nologo clean
perl Makefile.PL
nmake -nologo UNINST=1 install
if "%PAUSE%"=="yes" pause
cd ..
:skipmimet

if defined DOPERL2 goto pdf2
if defined SKIPPERL2 goto skippdf
:pdf2
echo ============================================================
echo Building PERL module (PDF-API2)
echo ============================================================
cd PDF-API2-*
nmake -nologo clean
perl Makefile.PL
nmake -nologo UNINST=1 install
if "%PAUSE%"=="yes" pause
cd ..
:skippdf

if defined DOPERL2 goto uri
if defined SKIPPERL2 goto skipuri
:uri
echo ============================================================
echo Building PERL module (URI)
echo ============================================================
cd URI-*
nmake -nologo clean
perl Makefile.PL
nmake -nologo UNINST=1 install
if "%PAUSE%"=="yes" pause
cd ..
:skipuri

if defined SKIPCSV goto csv
if defined SKIPPERL2 goto skipcsv
:csv
echo ============================================================
echo Building PERL module (Text-CSV_XS)
echo ============================================================
cd Text-CSV_XS*
nmake -nologo clean
perl Makefile.PL
nmake -nologo UNINST=1 install
if "%PAUSE%"=="yes" pause
cd ..
:skipcsv

if defined DOPERL2 goto htmltags
if defined SKIPPERL2 goto skiphtmlts
:htmltags
echo ============================================================
echo Building PERL module (HTML-Tagset)
echo ============================================================
cd HTML-Tagset-*
nmake -nologo clean
perl Makefile.PL
nmake -nologo UNINST=1 install
if "%PAUSE%"=="yes" pause
cd ..
:skiphtmlts

if defined DOPERL2 goto htmlparse
if defined SKIPPERL2 goto skiphtmlp
:htmlparse
echo ============================================================
echo Building PERL module (HTML-Parser)
echo ============================================================
cd HTML-Parser-*
nmake -nologo clean
perl Makefile.PL
nmake -nologo UNINST=1 install
if "%PAUSE%"=="yes" pause
cd ..
:skiphtmlp

if defined DOPERL2 goto lwp
if defined SKIPPERL2 goto skiplwp
:lwp
echo ============================================================
echo Building PERL module (LWP)
echo ============================================================
cd libwww-perl-*
nmake -nologo clean
perl Makefile.PL -n
nmake -nologo UNINST=1 install
if "%PAUSE%"=="yes" pause
cd ..
:skiplwp

if defined DOPERL2 goto afm
if defined SKIPPERL2 goto skipafm
:afm
echo ============================================================
echo Building PERL module (Font-AFM)
echo ============================================================
cd Font-AFM-*
nmake -nologo clean
perl Makefile.PL
nmake -nologo UNINST=1 install
if "%PAUSE%"=="yes" pause
cd ..
:skipafm

if defined DOPERL2 goto algdiff
if defined SKIPPERL2 goto skipad
:algdiff
echo ============================================================
echo Building PERL module (Algorithm-Diff)
echo ============================================================
cd Algorithm-Diff-*
nmake -nologo clean
perl Makefile.PL
nmake -nologo UNINST=1 install
if "%PAUSE%"=="yes" pause
cd ..
:skipad

if defined DOPERL2 goto xmlns
if defined SKIPPERL2 goto skipxmlnss
:xmlns
echo ============================================================
echo Building PERL module (XML-NamespaceSupport)
echo ============================================================
cd XML-NamespaceSupport-*
nmake -nologo clean
perl Makefile.PL
nmake -nologo UNINST=1 install
if "%PAUSE%"=="yes" pause
cd ..
:skipxmlnss

if defined DOPERL2 goto xmlsax
if defined SKIPPERL2 goto skipxmlsax
:xmlsax
echo ============================================================
echo Building PERL module (XML-SAX)
echo ============================================================
cd XML-SAX-0*
nmake -nologo clean
perl Makefile.PL
nmake -nologo UNINST=1 install
if "%PAUSE%"=="yes" pause
cd ..
:skipxmlsax

if defined DOPERL2 goto xmlsaxxs
if defined SKIPPERL2 goto skipxmlsaxxs
:xmlsaxxs
echo ============================================================
echo Building PERL module (XML-SAX-ExpatXS)
echo ============================================================
cd XML-SAX-ExpatXS-*
nmake -nologo clean
perl Makefile.PL DEFINE=-DXML_STATIC INC=-I..\..\output\include
nmake -nologo UNINST=1 install
if "%PAUSE%"=="yes" pause
cd ..
:skipxmlsaxxs

:start

if defined DOCOPY goto docopy
if defined SKIPCOPY goto skipcopy
:docopy
echo ============================================================
echo Copying tools to specroot\bin
echo Please watch this section carefully, as errors here indicate
echo something that needs to be reviewed further back.
echo ============================================================
pause

if defined SPEC goto :sanitycd
goto setbin
:sanitycd
cd %SPEC%\tools\src

:setbin
set specbin=%SPEC%\bin
mkdir %specbin%
mkdir %specbin%\lib

cd make-*
echo Copying WinRel\make.exe to %specbin%\specmake.exe
copy WinRel\make.exe %specbin%\specmake.exe
dir %specbin%\specmake.exe
cd ..

cd nttar*
echo Copying tar.exe to %specbin%\spectar.exe
copy tar.exe %specbin%\spectar.exe
dir %specbin%\spectar.exe
cd ..

cd specinvoke*
echo Copying specinvoke.exe to %specbin%\specinvoke.exe
copy specinvoke.exe %specbin%\specinvoke.exe
echo Copying specinvoke_pm.exe to %specbin%\specinvoke_pm.exe
copy specinvoke_pm.exe %specbin%\specinvoke_pm.exe
dir %specbin%\specinvoke.exe
dir %specbin%\specinvoke_pm.exe
cd ..

cd specmd5sum*
echo Copying specmd5sum.exe to %specbin%\specmd5sum.exe
copy specmd5sum.exe %specbin%\specmd5sum.exe
dir %specbin%\specmd5sum.exe
cd ..

cd bzip2*
echo Copying bzip2.exe to %specbin%\specbzip2.exe
copy bzip2.exe %specbin%\specbzip2.exe
dir %specbin%\specbzip2.exe
cd ..

echo Copying perl.exe and perl*.dll
if exist %perlroot%\bin\perl.exe goto cpregperl
if "%CPU%"=="i386" goto cpperlx86
if "%CPU%"=="ia32" goto cpperlx86
if "%CPU%"=="ia64" goto cpperlx86
copy %specperl%\bin\MSWin32-%cpu%\perl.exe %specbin%\specperl.exe
copy %specperl%\bin\MSWin32-%cpu%\perl*.dll %specbin%
goto efcpperl
:cpperlx86
copy %specperl%\bin\MSWin32-x86\perl.exe %specbin%\specperl.exe
copy %specperl%\bin\MSWin32-x86\perl*.dll %specbin%
goto efcpperl
:cpregperl
copy %perlroot%\bin\perl.exe %specbin%\specperl.exe
copy %perlroot%\bin\perl*.dll %specbin%
goto efcpperl

:efcpperl
dir %specbin%\specperl.exe %specbin%\perl*.dll

pause

echo Copying perl libraries

CALL :tar_copy %specperl%\lib %specbin%\lib
CALL :tar_copy %perlroot%\lib %specbin%\lib
CALL :tar_copy %perlroot%\site\%perlver%\lib %specbin%\lib
CALL :tar_copy %perlroot%\site\lib %specbin%\lib
if "%PAUSE%"=="yes" pause

if exist %specbin%\lib\XML\SAX\ParserDetails.ini goto xmlinipresent
echo Making XML parser config file
echo "# Nothing here" > %specbin%\lib\XML\SAX\ParserDetails.ini
if "%PAUSE%"=="yes" pause
dir %specbin%\lib\XML\SAX\*.ini
:xmlinipresent

echo Build complete and copied

:skipcopy
goto end

:tar_copy
rem Copy a directory via intermediate tar file.  Ignores revision control dirs
if not exist %1 goto end
if not exist %temp%\foo.tar goto notemprm
del /Q /F %temp%\foo.tar
:notemprm
cd %1
%specbin%\spectar -cf %temp%\foo.tar --exclude .svn --exclude CVS .
if not exist %2 mkdir %2
cd %2
%specbin%\spectar -xvf %temp%\foo.tar
del /Q /F %temp%\foo.tar
cd %specbin%\..\tools\src
goto end

:clean_tree
rem Attempt to make clean, realclean, etc in the tree specified in %1
if not exist %1\win32\makefile goto not_perl
rem This must be perl; it's special, and will be done later.
goto end

:not_perl
FOR %%I in (NMakefile Makefile.nt makefile.msc Makefile) DO CALL :do_clean_tree %1 %%I
cd %SPEC%\tools\src
goto end

:do_clean_tree
rem echo on
rem This is what actually runs nmake
cd %1
if not exist %2 goto nothing_to_clean
nmake -nologo /k /f %2 realclean
nmake -nologo /k /f %2 distclean
nmake -nologo /k /f %2 clean
:nothing_to_clean
cd %SPEC%\tools\src
goto end

:end
