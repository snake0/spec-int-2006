#
#  cfgfile.pl - produces config files
#  Copyright (C) 1999-2006 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Author: Cloyce D. Spradling
#
# $Id: cfgfile.pl 4238 2006-05-19 22:12:09Z cloyce $

use IO::File;
use File::Basename;
use strict;

use vars qw($name $extension $synonyms);

$name      = 'config';
$extension = 'cfg';
$synonyms  = { map { lc($_) => 1 } ($name, $extension, qw(conf conffile configfile cfgfile)) };

$Spec::Format::cfgfile::non_default = 1; # You must ask for it by name
$Spec::Format::cfgfile::part_of_all = 1; # You must ask for it by name
my $version = '$LastChangedRevision: 4238 $ '; # Make emacs happier
$version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'cfgfile.pl'} = $version;

sub format {
    my($me, $r, $fn) = @_;
    my $enconfig = '';
    my @output = ();
    my $written = [];

    # Assemble the rawtxtconfig lines
    if (exists($r->{'rawconfig'}) && # it should
        ref($r->{'rawconfig'}) eq 'ARRAY') { # it should
      $enconfig = join('', @{$r->{'rawconfig'}});
    } else {
      return (undef, []);
    }
    my (undef, $decodedconfig, $txtconfig) = ::decode_decompress($enconfig);

    # Decide which of the three possibilities to use.
    # Basically, choose in descending order of preference:
    # txtconfig (decoded, decompressed)
    # decodedconfig (just decoded)
    # enconfig (the original input)
    my $compconfig = defined($txtconfig) ? $txtconfig : defined($decodedconfig) ? $decodedconfig : $enconfig;
    push @output, split(/(?:\r\n|\n)/, $compconfig, -1);

    # The first line of the stored config file should be a comment labelling
    # the invocation command line, so check for that:
    if ($output[0] !~ /^\# Invocation/) {
        ::Log(0, "ERROR: Contents of rawconfig array are not a configuration file!\n");
        return(undef, []);
    }

    foreach my $line (@output) {
	$line =~ tr/\015\012//d; # More reliable than the double chomp
    }

    # Dump the original config, too (if necessary)
    if (exists($r->{'origconfig'})) {
	my @orig = split(/(?:\r\n|\n)/, ::decode_decompress(join("\n", @{$r->{'origconfig'}})), -1);
	foreach my $line (@orig) {
	    $line =~ tr/\015\012//d; # More reliable than the double chomp
	}
	my ($barename, $outputpath) = fileparse($fn, ".$extension");
	$outputpath = '' if ($outputpath =~ /^\.[\/\\]$/o ||
			     (defined($::website_formatter) &&
			      $::website_formatter));
	my $outfn = "${outputpath}${barename}.orig.$extension";
	my $ofh = new IO::File '>'.$outfn;
	if (defined($ofh)) {
	    $ofh->print(join("\n", @orig)."\n");
	    $ofh->close();
	    push @{$written}, $outfn;
	} else {
	    ::Log(0, "ERROR: Could not open orig config for writing: $!\n");
	}
    }
	
    return (\@output, $written);
}

1;
