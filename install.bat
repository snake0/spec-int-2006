@echo off
if not "%INSTALL_DEBUG%."=="." @echo on
rem
rem
rem  install.bat - installs SPEC CPU2006 or just tool binaries
rem  Copyright (c) 1999-2006 Standard Performance Evaluation Corporation
rem
rem  Authors:  Bill Carr (CPU95 version)
rem            John Henning
rem            Cloyce D. Spradling
rem            Rahul Rahatekar
rem            Diego Esteves
rem  $Id: install.bat 4645 2006-07-21 20:08:17Z cloyce $
rem

set SUITE=cpu2006

rem Currently the only platform that runs Windows is x86.  Currently the
rem x86-64 systems also run the x86 tools.  So just make sure CPU is set
rem right:
set CPU=i386
if "%CPU%."=="." (
rem echo The instance of Windows that you're running doesn't seem to set the
rem echo CPU environment variable.  Please set it appropriately by hand.  For
rem echo valid values, look in the \tools\bin directory on the distribution media.
rem echo Toolsets for Windows have names like 'windows-i386'.
rem echo For example, if you have an x86- or x64-based machine, you would
rem echo .
rem echo   set CPU=i386
rem echo .
rem echo Whereas if you have an Alpha NT system, you would
rem echo .
rem echo   set CPU=alpha
rem echo .
rem echo If there is no toolset for your processor, you will need to build the
rem echo tools yourself.  In that case, set CPU to a reasonable value.
rem echo .
rem echo After setting the CPU environment variable, please re-run install.bat.
rem goto bad_end
)

rem Insist on command extensions.  The "cd" will clear any
rem lingering error status ("color" does not!) and the color
rem command is only recognized if we have extensions enabled.
cd >nul:
color
if errorlevel 1 (
    echo .
    echo Sorry, we cannot build the tools unless you have command
    echo extensions enabled.  Please see "cmd /?" and "if /?".
    echo .
    goto end_it_all
)
:efcmdext

if "%temp%."=="." (
    echo This install process will write several small temporary files.  Currently,
    echo the TEMP environment variable is not set.  Please set it to the full
    echo pathname of a directory where it's okay to write those files.  After
    echo that variable is set, please re-run install.bat.
    goto bad_end
)

rem This is a guess, but it should be a good one
set SPEC_VALID_ARCH=windows-%cpu%

rem Find top of SPEC heirarchy

if "%SPEC%."=="." (
    echo The environment variable SPEC should point to the source of the
    echo SPEC distribution as an absolute path.  I will now try to set
    echo the variable for you...
    echo.
    echo SPEC is set to %CD%
    echo If this is NOT what you want, press control-C 
    pause

    set SPEC=%CD%
)

echo Installing from "%SPEC%"
if not exist "%SPEC%\install.bat" (
    echo It appears that the environment variable SPEC - defined as %SPEC% -
    echo is incorrect.  Please check the value and try again.
    goto bad_end
)

set SPECNEWDEV=%1
if not "%SPECNEWDEV%." == "." (
    echo %SPECNEWDEV%>%temp%\specdev.txt
    findstr -r "^[a-zA-Z]:$" %temp%\specdev.txt >nul 2>&1
    if errorlevel 1 (
	echo First parameter must be in the form "C:"
	goto bad_end
    )
rem     The specperl test a few lines down will fail if the
rem     current directory is read-only.  So if the user has specified
rem     a valid destination, let's go there!
    %SPECNEWDEV%
)

set SPECNEWPATH=%2
if not "%SPECNEWPATH%." == "." (
    echo "%SPECNEWPATH%">%temp%\specdev.txt
    findstr -r "^.\\\\" %temp%\specdev.txt >nul 2>&1
    if errorlevel 1 (
	echo Second parameter must be a rooted directory e.g. \%SUITE%
	goto bad_end
    )
)
set SPECNEW=%1%2

rem Check to see if we are writable
if not "%SPECNEW%." == "." goto specnew_set
set SPECNEW=%SPEC%
:specnew_set
if not exist "%SPECNEW%" (
    mkdir "%SPECNEW%"
    if errorlevel 1 (
        echo There was an error creating the %SPECNEW% directory.
        goto bad_end
    )
)
set SPECNEWTMPFILE=%SPECNEW%\__SPEC__.TMP
echo hello >"%SPECNEWTMPFILE%" 2>&1
dir "%SPECNEWTMPFILE%" >nul 2>&1
if errorlevel 1 (
    echo You seem to be installing from a CD or DVD.  Please re-run
    echo this command with the first parameter being the destination
    echo drive letter and the second parameter being the path of the 
    echo directory into which you wish to install the benchmark suite
    echo For example:   install c: \%SUITE%
    goto bad_end
)
del "%SPECNEWTMPFILE%"

if exist %SPEC%\bin\specperl.exe goto toolsinst
if exist %SPEC%\SUMS.tools goto toolsinst
if exist %SPEC%\bin\lib goto toolsinst
goto notoolsinst
:toolsinst
echo Removing previous tools installation...
rem The one-line equivalent under Unix turns into this hack to write a batch
rem file and then call it.  At least we can build the batch file using Perl...
if exist %SPEC%\bin\specperl.exe (
    if exist %temp%\toolsdel.bat del /F /Q %temp%\toolsdel.bat
    %SPEC%\bin\specperl -ne "@f=split; next unless m#bin/#; $_=$f[3]; s#^#$ENV{SPEC}/#; s#\\#/#g; if (-f $_) { unlink $_; } elsif (-d $_) { s#/#\\#g; print """rmdir /Q /S $_\n"""; }" %SPEC%\SUMS.tools > %temp%\toolsdel.bat
    call %temp%\toolsdel.bat
    del /F /Q %temp%\toolsdel.bat
    rem Now fall through in case some things were missed by toolsdel.bat
)
rem Now make a non-Perl-based best effort to remove things.
rmdir /Q /S %SPEC%\bin\lib
del   /Q /F %SPEC%\bin\*.exe
del   /Q /F %SPEC%\bin\*.dll
del   /Q /F %SPEC%\bin\MANIFEST.pl
del   /Q /F %SPEC%\SUMS.tools
del   /Q /F %SPEC%\packagename
echo Finished removing old tools install
:notoolsinst

set SPECARCH=none

set TAR_VERBOSE=
set EXCLUDE_OPTS=
set EXCLUDE_PAT=
if "%VERBOSE%."=="." goto be_quiet
set TAR_VERBOSE=-v
:be_quiet

if exist "%SPEC%\tools\bin\windows-%cpu%\specbzip2.exe" (
    set toolsbindir="%SPEC%\tools\bin\windows-%cpu%"
    set SPEC_VALID_ARCH=windows-%cpu%
    set INSTALL_DONT_COPY_TOOLS=TRUE
    set EXCLUDE_OPTS=%EXCLUDE_OPTS% --exclude=tools/*
    set EXCLUDE_PAT=%EXCLUDE_PAT% tools/

    set SPECARCH=%SPEC_VALID_ARCH%
)

if not "%INSTALL_DONT_COPY_BENCHSPEC%." == "." (
    set EXCLUDE_OPTS=%EXCLUDE_OPTS% --exclude=benchspec/*
    set EXCLUDE_PAT=%EXCLUDE_PAT% benchspec/
)

if "%SPECNEW%."=="%SPEC%." (
    set EXCLUDE_OPTS=%EXCLUDE_OPTS% --exclude=bin/* --exclude=config/* --exclude=Docs/* --exclude=Docs.txt/* --exclude=result/*
    rem bin/ is not excluded from checking because those files are not
    rem touched by the install process.
    set EXCLUDE_PAT=%EXCLUDE_PAT% config/ Docs/ Docs.txt/ result/
)

rem Since there are two possible sources for benchmark stuff (the big unified
rem tarball, or the individual tarballs), here's the selection algorithm:
rem - If SPEC_USE_UNIFIED is set to a nonempty value, try the big tarball
rem - If original.src\release_control exists try to unpack the individual
rem     benchmark tarballs
rem - Otherwise, try the big tarball
rem If there's no original.src directory, or if the install is running from
rem a Subversion working tree copy, lack of the big tarball will not cause
rem an abort; the tools will happily proceed to MD5 checking and tools
rem unpacking.  Otherwise, an error will be printed and the installation
rem aborted.

cd /d %SPECNEW%
if not "%SPECARCH%."=="none." (
  if "%SPEC_USE_UNIFIED%." NEQ "." goto unified
  if not exist "%SPEC%\original.src\release_control" goto unified

:perbenchmark
  CALL :install_time_warning
  for /F "usebackq tokens=1,3*" %%i in ("%SPEC%\original.src\release_control") do CALL :unpack_benchmark "%SPEC%\original.src\benchball\%%i" %%j "%%k"
goto check

:unified
  rem If there's no original.src directory, it's not a CD/DVD, and so just
  rem skip to the check and the tools copy.
  if not exist "%SPEC%\original.src" goto check
  rem If there's an original.src directory and also a .svn directory, then
  rem it's a working tree copy.  Skip to the check and the tools copy.
  if exist "%SPEC%\.svn" goto check
  if not exist "%SPEC%\original.src\%SUITE%.tar.bz2" (
      echo .
      echo Can not find %SPEC%\original.src\%SUITE%.tar.bz2
      echo .
      echo The compressed suite image for %SUITE% is not present.  Aborting installation.
      goto bad_end
  )
  CALL :install_time_warning
  CALL :unpack_benchmark "%SPEC%\original.src\%SUITE%.tar.bz2" "~1200" "%SUITE% benchmark and data files"

:check
    echo %SPECARCH% > bin\packagename
    if "%SPECNOCHECK%." == "." (
        echo.
        echo Checking the integrity of your source tree...
        echo.
        echo. Depending on the amount of memory in your system, and the speed of your
        echo. destination disk, this may take more than 10 minutes.
        echo. Please be patient.
        echo.
        type %SPEC%\MANIFEST > MANIFEST.tmp
        CALL :cull_manifest MANIFEST.tmp original.src/
        rem Also remove things from the mnaifest that have been
        rem excluded from the tar
        for %%I in (%EXCLUDE_PAT%) DO CALL :cull_manifest MANIFEST.tmp %%I
        %toolsbindir%\specmd5sum -e -c MANIFEST.tmp > manifest.check.out
        if errorlevel 1 (
            findstr /V /R /C:": OK$" manifest.check.out
            del /F /Q MANIFEST.tmp >nul 2>&1
            echo Package integrity check failed!
            goto bad_end
        )
    )
    del /F /Q MANIFEST.tmp >nul 2>&1
    del manifest.check.out
    echo Unpacking tools binaries
    %toolsbindir%\specbzip2 -dc %toolsbindir%\%SUITE%tools-%SPEC_VALID_ARCH%.tar.bz2 2>NUL: | %toolsbindir%\spectar %TAR_VERBOSE% -xf - 
    
    echo Setting SPEC environment variable to %SPECNEW%
    set SPEC=%SPECNEW%
rem There's no Windows relocate, or it'd be run here
    if "%SPECNOCHECK%." == "." (
        echo Checking the integrity of your binary tools...
        %toolsbindir%\specmd5sum -e -c SUMS.tools > toolcheck.out
        if errorlevel 1 (
            findstr /V /R /C:": OK$" toolcheck.out
            echo Binary tools integrity check failed!
            del /F /Q toolcheck.out >nul 2>&1
            goto bad_end
        )
    )
    goto end_build
)

rem So we don't have a pre-built executable. 
if not "%INSTALL_DEBUG%."=="." @echo on
rem Re-home ourselves
echo Setting SPEC environment variable to %SPECNEW%
set SPEC=%SPECNEW%
%SPECNEWDEV%
cd "%SPEC%"

rem Ask the question about compiling here so the person can go away
rem (hopefully) and let the thing install on it's own.
if "%SPECARCH%."=="none." (
    echo We do not appear to have vendor supplied binaries for your
    echo architecture.  You will have to compile specmake and specperl
    echo by yourself.  Please read \Docs\tools_build.txt and
    echo \tools\src\buildtools.bat.
    echo --------------------------------------------------------------
    echo If you wish I can attempt to build the tools myself.
    echo I'm not very intelligent so if anything goes wrong I'm just going
    echo to stop.
    echo If you do not hit CTRL-C *NOW*, I will attempt to execute the build.
    pause
)

%SPECNEWDEV%
rem Let buildtools worry about whether or not their build environment
rem works.
if not exist %SPEC%\tools\bin (
    echo Creating directory %SPEC%\tools\bin
    mkdir "%SPEC%\tools\bin"
)
if not exist %SPEC%\tools\bin\%SPEC_VALID_ARCH% (
    echo Creating directory %SPEC%\tools\bin\%SPEC_VALID_ARCH%
    mkdir "%SPEC%\tools\bin\%SPEC_VALID_ARCH%"
)

echo Running %SPECNEW%\tools\src\buildtools.bat
cd "%SPECNEW%\tools\src"
buildtools.bat

:end_build
del /F /Q toolcheck.out

echo %PATH%>%temp%\specpath.txt
findstr -l "%SPECNEW%\bin" %temp%\specpath.txt >nul 2>&1
if not errorlevel 1 goto path_ok
    set PATH="%SPECNEW%\bin";%PATH%
:path_ok
del %temp%\specpath.txt

:done
cd "%SPEC%"

rem Check for WinZip munging
if exist %SPEC%\bin\test\WinZip.guard (
  %toolsbindir%\specmd5sum -e %SPEC%\bin\test\WinZip.guard > %temp%\winzip.test
  findstr /C:"1401a09c7fed3b499c79d987f1bf11e7" %temp%\winzip.test >nul 2>&1
  if not errorlevel 1 (
    del %temp%\winzip.test
    echo.
    echo.
    echo. It looks like WinZip has helpfully performed CR/LF conversion on files
    echo. it's extracted from the tarball.  Unfortunately, this has corrupted
    echo. most of the files in the distribution.
    echo. Please DISABLE the "Automatic CR/LF conversion for TAR files" in the
    echo. WinZip preferences before unpacking the distribution tarball, or
    echo. preferably, use specbzip2 and spectar to unpack the distribution.
    echo. You can find them in
    echo.   %toolsbindir%
    echo.
    echo.
    goto bad_end
  )
)

goto end_it_all

:cull_manifest
rem This is how to get multiple commands into a FOR loop.  Geez.
rem The name of the file to cull is in %1, and the strings that should
rem be removed are in %2.
findstr /V /C:" %2" %1 > cpu2006.cull.filetemp
del /F /Q %1
rename cpu2006.cull.filetemp %1
goto actual_end

:unpack_benchmark
rem This unpacks the file in %1
echo Unpacking %~3 (%~2 MB)
%toolsbindir%\specbzip2 -dc "%1" 2>NUL: | %toolsbindir%\spectar %EXCLUDE_OPTS% %TAR_VERBOSE% -xf -
goto actual_end

:install_time_warning
echo.
echo. Depending on the speed of the drive holding your installation media
echo. and the speed of your destination disk, this may take more than 5 minutes.
echo. Please be patient.
echo.
goto actual_end

:bad_end
set SPECARCH=
set SPECNEW=
set SPECNEWDEV=
set SPECNEWPATH=
set SPECNEWTMPFILE=
set SPEC_MAKE_LEVEL=
set SPEC_PERL_LEVEL=
set SPEC_VALID_ARCH=
echo Installation NOT completed!
goto actual_end

:end_it_all
set SPECARCH=
set SPECNEW=
set SPECNEWDEV=
set SPECNEWPATH=
set SPECNEWTMPFILE=
set SPEC_MAKE_LEVEL=
set SPEC_PERL_LEVEL=
set SPEC_VALID_ARCH=
echo Installation completed!
goto actual_end

:actual_end
