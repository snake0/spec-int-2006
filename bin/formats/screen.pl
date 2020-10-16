#
#  screen.pl - produces ASCII table on stdout
#  Copyright (C) 2004-2006 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Authors:  Cloyce D. Spradling
#
# $Id: screen.pl 3626 2006-01-17 14:16:50Z cloyce $

use strict;

use vars qw($name $extension $synonyms);

$name      = 'Screen';
$extension = undef;
$synonyms  = { map { lc($_) => 1 } ($name, qw(scr display disp terminal term)) };

$Spec::Format::screen::non_default = 1;       # You must ask for it by name
my $screen_version = '$LastChangedRevision: 3626 $ '; # Make emacs happier
$screen_version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'screen.pl'} = $screen_version;

sub format {
    my($me, $r, $fn) = @_;

    my @nc = @{$r->{'nc'}};
    my $invalid = ($r->{'invalid'} ||
		   ((ref($r->{'errors'}) eq 'ARRAY') && @{$r->{'errors'}}));

    print "\n\n".join("\n", Spec::Format::asc::screen_format($me, $r, $fn, 0, $invalid, \@nc))."\n\n";

    return ([], []);
}
