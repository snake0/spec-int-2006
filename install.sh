#!/bin/sh

#
#  install.sh - installs the full benchmark tree or just the tools binaries
#  Copyright (c) 1998-2005 by Standard Performance Evaluation Corporation
#
#  Authors:  Christopher Chan-Nui, Cloyce D. Spradling
#

SUITE=cpu2006
UCSUITE=CPU2006
CURDIR=`pwd`

is_spec_dir() {
    [ -x "$1/bin/runspec\;1"  ] || 
    [ -x "$1/bin/runspec.\;1" ] || 
    [ -x "$1/bin/runspec."    ] || 
    [ -x "$1/bin/runspec"     ] || 
    [ -d "$1/tools/bin"       ] 
}

uninstall_previous() {
    # UNinstall previously installed tools.  This might not get everything,
    # but it'll get everything that might not be overwritten by the next
    # installation.
    if [ -f "$1/SUMS.tools" ]; then
        (cd "$1"; cat SUMS.tools | grep bin/ | awk '{print $4}' | xargs rm -f)
        # At this point bin/lib should be empty, but just to be sure...
        rm -rf "$1/bin/lib"
        rm -f "$1/bin/packagename"
        rm -f "$1/SUMS.tools"
    fi
}

check_manifest() {
    # Given a manifest file, a top dir, and an exclude pattern, verify the
    # contents of the files.
    manifest="$1"
    topdir="$2"
    exclude_pat="$3"
    if [ ! -f $manifest ]; then
        errors="${errors}$manifest does not exist!
"
    else
        echo
	echo "Checking the integrity of your source tree..."
	echo
        cat "$manifest" > "$topdir/MANIFEST.tmp.$$"

        # Filter out errors due to newly-rebuilt tools packages
        if [ -d "$topdir/tools/bin" -a ! -d "$topdir/tools/output" ]; then
          for i in `(cd "$topdir"; find tools/bin -name unbundled -print)`; do
            grep -v `dirname "$i"` "$topdir/MANIFEST.tmp.$$" > "$topdir/manifest.tmp.$$.1"
            cat "$topdir/manifest.tmp.$$.1" > "$topdir/MANIFEST.tmp.$$"
            rm -f "$topdir/manifest.tmp.$$.1"
          done
        else
          # This can happen when installing from DVD
          grep -v ' tools/bin/' "$topdir/MANIFEST.tmp.$$" > "$topdir/manifest.tmp.$$.1"
          cat "$topdir/manifest.tmp.$$.1" > "$topdir/MANIFEST.tmp.$$"
          rm -f "$topdir/manifest.tmp.$$.1"
        fi

        # Filter out names that were not unpacked
	for i in original.src/ $exclude_pat; do
          cat "$topdir/MANIFEST.tmp.$$" | grep -v " $i" > "$topdir/manifest.tmp.$$.1"
          cat "$topdir/manifest.tmp.$$.1" > "$topdir/MANIFEST.tmp.$$"
          rm -f "$topdir/manifest.tmp.$$.1"
        done

	# Don't do the grep -v ': OK' here because grep will exit successfully
	cat "$topdir/MANIFEST.tmp.$$" | grep -v '/ORIG$' | (cd "$topdir"; $MD5PROG -e -c -) > "$topdir/manifest.check.$$"
	rm -f "$topdir/MANIFEST.tmp.$$"
        cat "$topdir/manifest.check.$$" | grep -v ': OK$' >> "$topdir/manifest.errors.$$"
        echo
	if grep ':' "$topdir/manifest.errors.$$"; then
	    if [ -d "$topdir/tools/output" ]; then
	      # The tools have just been built, so some failures are expected
	      echo "Package integrity check failed.  Some failures are to be expected after"
	      echo "building the tools, as the build process modifies some files in the"
	      echo "distribution."
	    else
	      # If we set errors, the install will stop before installing binaries
	      errors="${errors}Package integrity check failed.
"
	    fi
	else
	    echo "Checksums are all okay."
	fi
	rm -f "$topdir/manifest.check.$$" "$topdir/manifest.errors.$$"
    fi
}

clear=`tput clear`
echo "${clear}SPEC $UCSUITE Installation"
echo

# We want everything to be world readable
umask 022

# Set the locale, if it isn't already set and the user hasn't forbidden it
if [ -z "$SPEC_INSTALL_LOCALE_OK" ]; then
    if [ -z "$LC_ALL" -o -z "$LC_LANG" ]; then
        LC_ALL=C
        LC_LANG=C
        export LC_ALL LC_LANG
    fi
fi

# Set some flags for later
if [ ! -z "$VERBOSE" ]; then
    VERBOSE=`echo $VERBOSE | tr -c 0-9`
fi
if [ "0$VERBOSE" -gt 0 ]; then
    SPEC_INSTALL_VERBOSE=-v
else
    SPEC_INSTALL_VERBOSE=
fi

# Find top of SPEC heirarchy
if [ -n "$SPEC" ] ; then
    if is_spec_dir "$SPEC"; then
	if [ "$SPEC" != "`pwd`" ]; then
	    echo "The SPEC environment variable is already set to \"$SPEC\","
            echo "which does not match the current directory (\"`pwd`\").  If you continue with the"
            echo "installation, the tools will be installed in \"$SPEC\".  Is this the desired behavior?"
	    echo "Please enter 'yes' or 'no'."
	    read ans
	    ans=`echo $ans | tr YESNO yesno`
	    if [ "$ans" = 1 -o "$ans" = 'y' -o "$ans" = 'yes' ]; then
		true
	    else
                echo
                echo "Okay, ignoring your environment setting of \$SPEC.  Please pay special"
                echo "attention to the next non-blank line.  If it does not contain the location"
                echo "where you would like to install, terminate the install.sh process and"
                echo "try again."
                echo
		SPEC=
	    fi
	fi
    else
	SPEC=
    fi
fi
if [ -z "$SPEC" ]; then
    SPEC=`pwd`
    while [ -n "$SPEC" ]; do
	if is_spec_dir "$SPEC"; then
	    break;
	fi
        if [ "$SPEC" != "." ]; then
          # At least some vendors' /bin/sh doesn't like this substitution
          #SPEC=${SPEC%/*}
          # Everyone should still have sed
          SPEC=`echo $SPEC | sed -e 's/\/[^\/]*$//'`
        else
          SPEC=
        fi
    done
fi
if [ -z "$SPEC" ]; then
    SPEC=`dirname "$0"`
    while [ -n "$SPEC" ]; do
	if is_spec_dir "$SPEC"; then
	    break;
	fi
        if [ "$SPEC" != "." ]; then
          # At least some vendors' /bin/sh doesn't like this substitution
          #SPEC=${SPEC%/*}
          # Everyone should still have sed
          SPEC=`echo $SPEC | sed -e 's/\/[^\/]*$//'`
        else
          SPEC=
        fi
    done
fi
if [ -z "$SPEC" ]; then
    echo
    echo "Can't find the top of your $UCSUITE tree!  Please change to the benchmark"
    echo "directory and run this program ($0) again!"
    exit 1
fi
echo "Top of the $UCSUITE tree is '$SPEC'"

if cd "$SPEC" ; then
    true
else
    echo
    echo "Huh?  Can't cd into $UCSUITE directory '$SPEC'"
    exit 1
fi

# Find out where to install the executables
arch=
if [ $# -gt 0 ]; then
    arch=$1; shift
fi
if [ $# -gt 0 ]; then
    SPECTARGET=$1; shift
    mkdir -p $SPECTARGET 2> /dev/null || mkdir $SPECTARGET 2> /dev/null
    if [ ! -d "$SPECTARGET" ]; then
	SPECTARGET=
    fi
fi

if [ -z "$SPECTARGET" ]; then
    if touch a 2> /dev/null; then 
	rm a
	SPECTARGET=$SPEC
    fi
fi

while [ -z "$SPECTARGET" ]; do
    echo "Enter the directory you wish to install to (e.g. /usr/$SUITE)"
    read SPECTARGET
    if [ "x$SPECTARGET" = "x." ]; then
      SPECTARGET=$CURDIR
      echo Attempting to install to $SPECTARGET
    fi
    mkdir -p $SPECTARGET 2> /dev/null || mkdir $SPECTARGET 2> /dev/null
    if [ ! -d "$SPECTARGET" ]; then
	SPECTARGET=
    fi
done

valid_archs=
archcount=0
if [ "$arch" != 'none' ]; then
    mach=`uname -m 2>/dev/null | sed 's/\./\\./g'`
    os=`uname -s 2>/dev/null | sed 's/\./\\./g'`
    rev=`uname -r 2>/dev/null | sed 's/\./\\./g'`
    proc=`uname -p 2>/dev/null | sed 's/\./\\./g'`
    for tmparch in `ls "$SPEC/tools/bin" | sed 's/\/$//'`; do
	if [ -n "$mach" -a -n "$os" -a -f "$SPEC/tools/bin/$tmparch/excludearch" ] && 
             grep -i "$mach" "$SPEC/tools/bin/$tmparch/excludearch" >/dev/null 2>&1 ||
             grep -i "$os" "$SPEC/tools/bin/$tmparch/excludearch" >/dev/null 2>&1; then
            # The machine type or OS name was listed in excludearch;
            # don't try running binaries on this system
            true
        elif [ -n "$rev" -a -f "$SPEC/tools/bin/$tmparch/excluderev" ] && 
               grep -i "^$rev\$" "$SPEC/tools/bin/$tmparch/excluderev" >/dev/null 2>&1; then 
            # The OS revision was listed in excluderev;
            # don't try running binaries on this system
            true
        elif [ -n "$proc" -a -f "$SPEC/tools/bin/$tmparch/excludeproc" ] && 
               grep -i "^$proc\$" "$SPEC/tools/bin/$tmparch/excludeproc" >/dev/null 2>&1; then 
            # The processor architecture was listed in excludeproc;
            # don't try running binaries on this system
            true
        elif [ -n "$SPEC_DONT_TELL" ] &&
              echo $SPEC_DONT_TELL | grep -i "^$tmparch\$" >/dev/null 2>&1; then
            # The user wishes to not see this toolset.
            true
        elif [ -x "$SPEC/tools/bin/$tmparch/specbzip2" ] &&
             "$SPEC/tools/bin/$tmparch/specbzip2" -h > /dev/null 2>&1 &&
             [ -x "$SPEC/tools/bin/$tmparch/spectar" ] &&
             "$SPEC/tools/bin/$tmparch/spectar" --help > /dev/null 2>&1 &&
             [ -x "$SPEC/tools/bin/$tmparch/specmd5sum" ] &&
             "$SPEC/tools/bin/$tmparch/specmd5sum" --help > /dev/null 2>&1; then
            if [ -n "$valid_archs" ]; then
              valid_archs="$tmparch $valid_archs"
            else
              valid_archs="$tmparch"
            fi
            archcount=`expr $archcount + 1`
        fi
    done
fi

if [ -n "$valid_archs" ]; then
    error=
    if [ -n "$arch" ]; then
        if "$SPEC/tools/bin/$arch/specbzip2" -h > /dev/null 2>&1 &&
            "$SPEC/tools/bin/$arch/spectar" --help > /dev/null 2>&1 &&
            "$SPEC/tools/bin/$arch/specmd5sum" --help > /dev/null 2>&1; then
	    true
	elif [ -d "$SPEC/tools/bin/$arch" ]; then
	    error="'${arch}' does not appear to be executable on this machine.";
	    arch=
	else
	    error="Tools for '${arch}' do not exist.";
	    arch=
	fi
    fi
    while [ -z "$arch" ]; do
        if [ $archcount -gt 1 ]; then
          echo
          echo "These appear to be valid toolsets:"
          echo ""
          for i in $valid_archs; do
            descfile="$SPEC/tools/bin/$i/description"
            if [ -f "$descfile" ]; then
              printf "%-29s %s\n" $i "`head -1 "$descfile"`"
              if [ `cat "$descfile" | wc -l` -gt 1 ]; then
                tail +2 "$descfile"
              fi
            else
              echo $i
            fi
            echo ""
          done
          echo "${error}"
          echo "Enter the architecture you are using:"
          read arch
        else
          echo
          echo "There appears to be only one valid toolset:"
          echo ""
          descfile="$SPEC/tools/bin/$valid_archs/description"
          if [ -f "$descfile" ]; then
            printf "%-29s %s\n" $valid_archs "`head -1 "$descfile"`"
            if [ `cat "$descfile" | wc -l` -gt 1 ]; then
              tail +2 "$descfile"
            fi
          else
            echo $valid_archs
          fi
          echo ""
          echo "${error}"
          if [ -z "$SPEC_DONT_ASK" ]; then
            echo "Use this? (y/n)"
            read arch
          else
            echo "Using this toolset..."
          fi
        fi
        if [ "$arch" = "none" ]; then arch=; break; fi
        if [ $archcount -eq 1 -a \( "x$arch" = "xy" -o "x$arch" = "xY" -o -n "$SPEC_DONT_ASK" \) ]; then
          # Strip the whitespace
          arch=`echo $valid_archs | sed 's/ //g'`
        elif [ "x$arch" = "xn" -o "x$arch" = "xN" ]; then
          arch=
          break
        fi
        if [ -x "$SPEC/tools/bin/$arch/specbzip2" ] &&
           "$SPEC/tools/bin/$arch/specbzip2" -h > /dev/null 2>&1 &&
           [ -x "$SPEC/tools/bin/$arch/spectar" ] &&
           "$SPEC/tools/bin/$arch/spectar" --help > /dev/null 2>&1 &&
           [ -x "$SPEC/tools/bin/$arch/specmd5sum" ] &&
           "$SPEC/tools/bin/$arch/specmd5sum" --help > /dev/null 2>&1; then
            true
        else
            error="'${arch}' does not appear to be executable on this machine.";
            arch=
        fi
    done
else
    arch=$valid_archs
fi

# Set up the excludes for the installation.
EXCLUDE=
EXCLUDE_PAT=config/

echo ""
if [ -z "$arch" ]; then
    echo "${clear}We do not appear to have vendor supplied binaries for your"
    echo "architecture.  You will have to compile the tool binaries"
    echo "by yourself.  Please read the file"
    echo ""
    echo "    $SPECTARGET/Docs/tools_build"
    echo ""
    echo "and then try the following."
    echo
    echo "If you wish I can try to perform these steps for you.  However"
    echo "I'm not very intelligent so if anything goes wrong I'm just going"
    echo "to stop.  Want me to try?  (yes/no)"
    read ans
    dobuild=
    if [ "$ans" = "yes" ] || [ "$ans" = "YES" ] || \
	[ "$ans" = "y" ] || [ $ans = "Y" ] || [ $ans = "Yes" ]
    then
	dobuild=1
    else
	echo ""
	echo "There are no working toolsets, and you have elected not to have"
	echo "me try to build them for you.  Exiting."
	echo ""
	exit 1
    fi
elif [ -z "$SPEC_INSTALL_TOOLS" ]; then
    EXCLUDE="$EXCLUDE --exclude=tools/*"
    EXCLUDE_PAT="$EXCLUDE_PAT tools/"
fi

UNCOMPRESS="bzip2 -d"
TAR=tar
if [ -n "$arch" ]; then
    UNCOMPRESS="\"$SPEC/tools/bin/$arch/specbzip2\" -d"
    TAR="\"$SPEC/tools/bin/$arch/spectar\" $EXCLUDE"
fi

# Install things here
# Here's a part where we try to unpack a tarball that lives in original.src
# on the installation media.  If it's not found, it's not necessarily fatal;
# this situation could arise when running install.sh to install tools
# binaries in a benchmark tree that has already been unpacked.
errors=
if [ "$SPEC" != "$SPECTARGET" -o ! -d "$SPEC/benchspec" -o ! -d "$SPEC/bin" ]; then
    if [ -n "$SPEC_USE_UNIFIED" -o ! -f "$SPEC/original.src/release_control" ]; then
      if [ -z "$SOC" ]; then
          for i in "$SPEC/original.src/${SUITE}.tar.bz2" "$SPEC/original.src/${SUITE}.tar.bz2\;1"; do
              if [ -f $i ]; then
                  SOC=$i
                  break
              fi
          done
      fi

      if [ -z "$SOC" ]; then
          errors="Can't find $SPEC/original.src/${SUITE}.tar.bz2 file"
      elif [ -z "$errors" ]; then
          echo "Unpacking $UCSUITE archive..."
          if eval $UNCOMPRESS < "$SOC" | \
                (cd "$SPECTARGET" ; eval $TAR $SPEC_INSTALL_VERBOSE -xf -) ; then
              true
              echo "Done unpacking $UCSUITE archive."
          else
              errors="Error extracting the $UCSUITE archive."
          fi
      fi
    else
      # Unpack the individual tarballs according to the release_control file
      cat "$SPEC/original.src/release_control" | \
      while read file csize usize desc; do
        if [ -f "$SPEC/original.src/benchball/$file" ]; then
          if [ -z "$dobuild" ] && echo $file | grep tools-src >/dev/null; then
            # Skip unpacking tools sources for normal installs
            true
          else
            echo "Unpacking $desc ($usize MB)"
            if eval $UNCOMPRESS < "$SPEC/original.src/benchball/$file" | \
                  (cd "$SPECTARGET" ; eval $TAR $SPEC_INSTALL_VERBOSE -xf -) ; then
                true
            else
                errors="${errors}Error unpacking the $file archive!
  "
            fi
          fi
        else
          errors="${errors}Benchmark archive $file could not be found!
"
        fi
      done
    fi

    if [ -n "$errors" ]; then
        echo $errors
        echo
        echo "Installation aborted; removing files written."
        rm -rf "$SPECTARGET"
        exit 1
    fi
fi

MD5PROG=
if [ ! -z "$arch" -a -x "$SPEC/tools/bin/$arch/specmd5sum" ]; then
    MD5PROG="$SPEC/tools/bin/$arch/specmd5sum"
elif [ ! -z "$arch" -a -x tools/bin/$arch/specmd5sum ]; then
    MD5PROG=tools/bin/$arch/specmd5sum
elif [ -x bin/specmd5sum ]; then
    MD5PROG=bin/specmd5sum
fi
if [ -z "$MD5PROG" -a -z "$dobuild" ]; then
    echo "No specmd5sum found.  Tools are out of date or incomplete."
    echo "Skipping file integrity checks."
    errors="Binary tools are incomplete."
elif [ ! -z "$dobuild" ]; then
    echo "No usable specmd5sum exists.  Skipping file integrity checks."
elif [ -z "$SPEC_INSTALL_NOCHECK" ]; then
    if $MD5PROG -e >/dev/null 2>&1 </dev/null; then
        check_manifest "$SPEC/MANIFEST" "$SPECTARGET" "$EXCLUDE_PAT"
    else
	echo "Your specmd5sum program is not up-to-date."
	echo "Skipping file integrity checks."
	errors="${errors}Binary tools are out of date or incomplete.
"
    fi
fi

if [ -n "$errors" ]; then
    echo $errors
    echo
    echo "Installation aborted."
    uninstall_previous "$SPECTARGET"
    exit 1
fi

if [ -z "$arch" ]; then
    if [ -n "$dobuild" ]; then
        echo "Attempting to build the tools for $UCSUITE..."
	cd "$SPECTARGET/tools/src"
	if "./buildtools"; then
	    true
        else
            echo
	    echo "Whoops! I had trouble building your tools.  Please consult"
	    echo "$SPECTARGET/Docs/tools-build.html"
	    exit 1;
	fi
        cd "$SPECTARGET"
	. ./shrc
	if "bin/packagetools" `bin/specperl -MConfig -e 'print $Config{"archname"};'`; then
	    true
        else
            echo
	    echo "Whoops! I had trouble packaging your tools.  Please consult"
	    echo "$SPECTARGET/Docs/tools-build.html"
	    exit 1;
	fi
        echo "Tools build successful."
        MD5PROG=bin/specmd5sum
        check_manifest "$SPECTARGET/MANIFEST" "$SPECTARGET" "$EXCLUDE_PAT"
    else
	echo "Ok... good luck!"
        exit 1
    fi
else
    # Install binaries here
    rm -rf bin/lib

    if [ -f "$SPECTARGET/SUMS.tools" ]; then
        echo "Removing previous tools installation"
        uninstall_previous "$SPECTARGET"
    fi

    echo "Unpacking binary tools for $arch..."
    # Check for the one you really want _last_
    TOOLS=
    for testsuite in cpu2006 ${SUITE}; do
      if [ -f "$SPEC/tools/bin/$arch/${testsuite}tools-$arch.tar.bz2" ]; then
        TOOLS="$SPEC/tools/bin/$arch/${testsuite}tools-$arch.tar.bz2"
        UNCOMPRESS="\"$SPEC/tools/bin/$arch/specbzip2\" -dc"
      fi
    done
    if [ -z "$TOOLS" ]; then
      # This should never happen
      echo
      echo "Huh?  There's no binary tools tarball?!"
      exit 1;
    fi

    if eval $UNCOMPRESS < `ls "$TOOLS"` | \
	    (cd "$SPECTARGET"; eval $TAR $SPEC_INSTALL_VERBOSE -xf -); then
        if [ -z "$SPEC_INSTALL_NOCHECK" ]; then
          echo "Checking the integrity of your binary tools..."
          echo
          # Don't do the grep -v ': OK' here because grep will exit successfully
          (cd "$SPECTARGET"; cat SUMS.tools | grep -v '/ORIG$' | $MD5PROG -e -c - > manifest.check.$$)
          if [ $? != 0 ]; then
             cat "$SPECTARGET/manifest.check.$$" | grep -v ': OK$'
             echo
             errors="${errors}Binary tools integrity check failed.
"
          else
             echo "Checksums are all okay."
          fi
          rm -f "$SPECTARGET/manifest.check.$$"
        fi
    else
	errors="${errors}Error extracting the '$arch' binaries tar file.
"
    fi
    echo $arch > "$SPECTARGET/bin/packagename"
fi

if [ "x$errors" != "x" ]; then
    # Uninstall the tools that have just been unpacked (if possible)
    uninstall_previous "$SPECTARGET"
else
    cd "$SPECTARGET"

    # shrc will add LD_LIBRARY_PATH if it is needed
    . ./shrc

    if bin/relocate ; then
        true
    else
        errors="${errors}Error re-homing the benchmark tools.
"
    fi
fi

if [ "x$errors" = "x" ]; then
    echo "Everything looks okay.  cd to $SPECTARGET,"
    echo "source the shrc file and have at it!"
else
    echo $errors
    echo
    echo "Installation aborted."
    uninstall_previous "$SPECTARGET"
    exit 1
fi

exit 0
