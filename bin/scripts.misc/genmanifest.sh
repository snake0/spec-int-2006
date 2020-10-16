#!/bin/sh
#
# genmanifest.sh - generate the top-level MANIFEST file
# No support is provided for this script.
#
# Copyright (C) 1999-2006 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: genmanifest.sh 4762 2006-08-03 20:52:44Z cloyce $

if [ -z "$SPEC" ]; then
    echo "SPEC variable is not set!";
    exit 1;
fi

# Tweak this; it's a regexp that recognizes the name of the suite tarball
# that lives in original.src
suitetarballre='/(mpi200|cpu200).*\.(t[ab]z|tar.[gb]z2?)(\.md5)?$'

# Don't tweak this
novc='( ( -name CVS -o -name .svn ) -prune ) -o'

cd $SPEC
rm -f MANIFEST SUMS.data

if [ -f tools/tools_src.tar.bz2 ]; then
  exclude_toolsrc="grep -v ^tools/src/"
else
  exclude_toolsrc=cat
fi

echo Generating MD5 sums for compressed data files
compressedre=''
for i in `find . $novc \( -type f -name '*.bz2' -print \) | grep /data/ | sed 's/^\.\///' | sort`; do
    # If this is a full working tree, there should already be an uncompressed
    # copy in the directory.  But do the decompression, because it's the file
    # that's shipping that's important...
    fname=`dirname $i`/`basename $i .bz2`
    compressedre="$compressedre|$fname"
    bzip2 -dc $i | specmd5sum --binary -e | sed "s|-|$fname|" >> SUMS.data
done
if [ "x$compressedre" = "x" ]; then
  compressedre=cat
else
  compressedre="egrep -v ^($compressedre)\$"
fi

echo Generating MD5 sums for distribution files
find . $novc \( -type f ! -name MANIFEST ! -name MANIFEST.tmp -print \) | \
  sed 's#^\./##' | \
  egrep -v '(\.ignoreme|\.cvsignore|\.DS_Store)' | \
  egrep -v $suitetarballre | \
  $compressedre | \
  egrep -v '^shrc\.bat$' | \
  egrep -v '^bin/specperldoc' | \
  egrep -v '^benchspec/.*/(exe|run)/' | \
  egrep -v '^result' | \
  egrep -v '^tools/src/buildtools.log/.*buildlog.txt$' | \
  $exclude_toolsrc | \
  sort | \
  xargs specmd5sum --binary -e >> MANIFEST
