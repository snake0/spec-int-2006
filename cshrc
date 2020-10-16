#!/bin/csh 
 
#
#  cshrc - sets up the environment to run SPEC CPU2006
#  Copyright (C) 2004-2006 Standard Performance Evaluation Corporation
#
#  Authors:  Bob Larson
#            Cloyce D. Spradling
#
# $Id: cshrc 4375 2006-06-07 22:59:48Z cloyce $
#

# Enable this for _some_ debug output
set debug = 0

# Uncomment this for full trace output
#set verbose
 
if ( $?SPEC_LOCALE_OK == 0 ) then
    if ( $debug ) echo Setting locale
    if ( $?LC_ALL == 0 || $?LC_LANG == 0 ) then
        # This is just to make sure that the variables are defined *sigh*
        setenv LC_ALL ""
        setenv LC_LANG ""
    endif
    if ( "$LC_ALL" == "" || "$LC_LANG" == "" ) then
        setenv  LANGVAR C
        setenv  LC_ALL C
        setenv  LC_LANG C
    endif
endif

if ! { dirname . >&! /dev/null } then
    echo "Your OS must have a dirname program!  Please check your path\!"
    exit
endif

# Attempt to find the top of the SPEC tree
if ( $debug ) echo Looking for correct setting for \$SPEC

# Ignore pre-set SPEC
setenv SPEC ""
set TEMPSPEC=`pwd`
while ( ! -f "$TEMPSPEC/bin/runspec" && "$TEMPSPEC" != "/" && \
        "$TEMPSPEC" != "." && "$TEMPSPEC" != "" )
    set TEMPSPEC=`dirname "$TEMPSPEC"`
end
if ( -f "$TEMPSPEC/bin/runspec" ) then
    setenv SPEC $TEMPSPEC
endif

if ( "$SPEC" == "" ) then
  echo "Can't find the top of your SPEC tree\!  Please change to your SPEC"
  echo "directory and source $0 again."
  exit
endif

# In order to discourage folks from sourcing the cshrc on the CD (and
# thereby getting the wrong values for $SPEC and friends), make sure that
# the config directory is writable.

if ( ! -x "$SPEC/bin/specperl" && ! -x "$SPEC/bin/specperl.exe" ) then
    echo ""
    echo "WARNING: this benchmark tree has not yet been installed.  Please"
    echo "         run install.sh before continuing."
    echo ""
endif

if ! { touch "$SPEC/config/shrc$$writetest" >&! /dev/null } then
  echo "You are not allowed to write into the current directory."
  echo "That may be because you are attempting to source the cshrc on your distribution"
  echo "media."
  echo "It may also be that a different user installed the benchmark tree."
  echo "Please correct the problem and try again."
  unsetenv SPEC
  exit
else
  # Everything is okay!
  \rm -f "$SPEC/config/shrc$$writetest" >&! /dev/null
endif
if ( $debug ) echo \$SPEC set successfully to $SPEC

if ( $path[1] != "$SPEC/bin" ) then
    if ( $debug ) echo Added $SPEC/bin to PATH
    setenv PATH $SPEC/bin:$PATH
endif
 
setenv SPECPERLLIB ""
if ( $debug ) echo About to set \$SPECPERLLIB
foreach i ( "$SPEC"/bin "$SPEC"/bin/lib `find "$SPEC/bin/lib" -type d -name 5\*` "$SPEC"/bin/lib/site_perl )
    if ( $debug ) echo SPECPERLLIB candidate is \"$i\"
    if ( -d $i ) then
        if ( "$SPECPERLLIB" == "" ) then
            setenv SPECPERLLIB $i
        else
            setenv SPECPERLLIB "$SPECPERLLIB"':'"$i"
        endif
    endif
end

if ( $debug ) echo About to set library paths
foreach j ( `find "$SPEC/bin" -name libperl.dylib\* -o -name libperl.so\*` )
    if ( $debug ) echo \*LD_LIBRARY_PATH candidate is \"$j\"
    switch( $j )
        case "*dylib*":
            if ( $?DYLD_LIBRARY_PATH == 0 ) then
                setenv  DYLD_LIBRARY_PATH `dirname $j`
            else
                setenv  DYLD_LIBRARY_PATH "$DYLD_LIBRARY_PATH"':'`dirname $j`
            endif
        breaksw
        default:
            if ( $?LD_LIBRARY_PATH == 0 ) then
                setenv  LD_LIBRARY_PATH `dirname $j`
            else
                setenv  LD_LIBRARY_PATH "$LD_LIBRARY_PATH"':'`dirname $j`
            endif
        breaksw
    endsw
end

if ( $debug ) echo Setting up go and ogo aliases
alias go "\rm -f $SPEC/.gogo >&! /dev/null; csh -f $SPEC/bin/scripts.misc/do_go.csh go \!*; if ( -f $SPEC/.gogo ) source $SPEC/.gogo; \rm -f $SPEC/.gogo >&! /dev/null"
alias ogo "\rm -f $SPEC/.gogo >&! /dev/null; csh -f $SPEC/bin/scripts.misc/do_go.csh ogo \!*; if ( -f $SPEC/.gogo ) source $SPEC/.gogo; \rm -f $SPEC/.gogo >&! /dev/null"

