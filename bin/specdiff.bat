@rem = '--*-Perl-*--';
@rem = '
@echo off
rem
rem specdiff.bat
rem
rem Copyright (C) 1999-2006 Standard Performance Evaluation Corporation
rem  All Rights Reserved
rem
rem $Id: specdiff.bat 4175 2006-04-26 16:51:31Z cloyce $
rem
rem Diego-hacks added 4/26/2006

rem Find top of SPEC heirarchy

if "%SPEC%."=="." (

    echo The environment variable SPEC should point to the source of the
    echo SPEC distribution as an absolute path.  I will now try to set
    echo the variable for you...

    echo.
    echo SPEC will be set to set to %CD%
    echo If this is NOT what you want, press control-C
    pause

    set SPEC=%CD%
)

if not exist "%SPEC%\bin\specdiff" (
   echo It appears that the environment variable SPEC - defined as %SPEC% -
   echo is incorrect.  Please run shrc.bat and try again.
   goto done
)

rem Add SPEC bin to PATH variable
echo %PATH%>%temp%\specpath.txt

findstr -l "%SPEC%\bin" %temp%\specpath.txt >nul 2>&1
if errorlevel 1 set PATH=%SPEC%\bin;%PATH%
del %temp%\specpath.txt

specperl "%SPEC%\bin\specdiff" %*

:done
