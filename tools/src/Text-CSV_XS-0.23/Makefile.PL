# -*- perl -*-
require 5.004;
use strict;

require ExtUtils::MakeMaker;

my($def) = '';
if ($ENV{'USER'} eq 'joe'  &&  $ENV{'HOSTNAME'} eq 'laptop.ispsoft.de') {
    $def .= ' -Wall -Wno-unused';
}

eval { require Text::CSV_XS };
if (!$@  &&  $Text::CSV_XS::VERSION < 0.15) {
    print <<'MSG';

You seem to have installed a previous version of the Text::CSV_XS module.
Note that you might encounter problems in existing applications (not the
DBD::CSV driver) due to incompatible changes in the print and getline
methods. See "perldoc CSV_XS.pm" for details.

MSG
    sleep 5;
}

ExtUtils::MakeMaker::WriteMakefile(
    'NAME'	   => 'Text::CSV_XS',
    'VERSION_FROM' => 'CSV_XS.pm', # finds $VERSION
    'dist'         => { 'SUFFIX'       => ".gz",
                         'DIST_DEFAULT' => 'all tardist',
                         'COMPRESS'     => "gzip -9f" },
    'DEFINE'       => $def
);
