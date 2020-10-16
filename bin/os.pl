#
# os.pm
#
# Copyright (C) 1999-2006 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: os.pl 4160 2006-04-22 22:21:59Z cloyce $

use strict;

my $version = '$LastChangedRevision: 4160 $ '; # Make emacs happier
$version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'os.pl'} = $version;

# Set up a process group if we can
sub setProcGrp {
    my ($config) = @_;
    eval "setpgrp;";
    if ($@) {
# This isn't much use if we never define handle_sigint...
#	$SIG{'INT'} = 'handle_sigint' 
    } else {
	$config->{'setpgrp_enabled'} = 1;
    }
}

sub initialize_os {
    my ($config) = @_;
    setProcGrp($config) if istrue($config->setprocgroup);
}

1;
