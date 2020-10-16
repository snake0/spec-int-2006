#
#  subcheck.pl - raw file submission checker
#  Copyright (C) 2005-2006 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Authors:  Cloyce D. Spradling
#
# $Id: subcheck.pl 4631 2006-07-20 03:45:36Z cloyce $

use IO::File;
use strict;

use vars qw($name $extension $synonyms $prefix %syntax $first $last);
do "formats/${main::lcsuite}.syntax" || die "\n  Error reading ${main::lcsuite}.syntax file:\n    $@\n";

$name      = 'Submission Check';
$extension = undef;
$synonyms  = { map { lc($_) => 1 } ($name, qw(check chk subcheck subtest test)) };

$Spec::Format::subcheck::non_default = 1;       # You must ask for it by name
my $subcheck_version = '$LastChangedRevision: 4631 $ '; # Make emacs happier
$subcheck_version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$Spec::Format::subcheck::non_default = 1;  # You must _sometimes_ ask for it
$::tools_versions{'subcheck.pl'} = $subcheck_version;

sub format {
    my($me, $r, $fn, $written) = @_;
    my $rawfile = $fn.$Spec::Format::raw::extension;
    my @errors = ();
    my @data = ();
    my @written = ::isa($written, 'ARRAY') ? @{$written} : ( $written );

    # Look for the raw file that's been generated
    if (!-f $rawfile || !grep { /$rawfile$/ } @written) {
      # Okay... there might not be a raw file if this is a fake report.
      # Those _should_ be checked, so work on the in-memory copy if it's
      # available.  (But do warn about it -- no raw file could mean out of
      # disk space.)
      if (!exists $r->{'compraw'}) {
	::Log(0, "FAILED (no raw file!)\n");
	return ([], []);
      } else {
        my $rawdata = ::decode_decompress($r->{'compraw'});
        if (!defined($rawdata)) {
          ::Log(0, "FAILED (no raw file!)\n");
          return ([], []);
        }
        @data = split(/\n/, $rawdata);
      }
    } else {
      my $ifh = new IO::File $rawfile;
      if (!defined($ifh)) {
          ::Log(0, "FAILED (could not read raw file)\n");
          return ([], []);
      }
      @data = <$ifh>;
    }
    
    # Read in the values from all the lines so that they can be referred to
    # later.
    my %values = ();
    foreach my $line (@data) {
        next unless $line =~ /^$prefix(\S+): (.*)/;
        my ($key, $val) = ($1, $2);
        $key =~ s/0*([1-9]\d*)$/$1 + 0/ego;     # Collapse indices
        $values{$key} = $val;
    }

    # This is ripped nearly directly from submit_functions.pl
    foreach my $key (sort keys %syntax) {
	my @lines = grep(/^$prefix$key[\s=]/, @data);

        my ( $re, $explanation, $one_match_sufficient, $complain_once, $logic, $default );
        if (::isa($syntax{$key}, 'ARRAY')) {
          ($re, $explanation, $one_match_sufficient, $complain_once, $logic) = @{$syntax{$key}};
          $explanation = [ $explanation ] unless ::isa($explanation, 'ARRAY');
          $default = 0;
        } else {
          $re = qr/$syntax{$key}/;
          $explanation = [ 'Invalid syntax: $line',
                           '\tUnable to match regular expression: $re'
                         ];
          $one_match_sufficient = 0;
          $complain_once = 0;
          $logic = undef;
          $default = 1;
        }

	if (@lines+0 <= 0) {

            # No lines, so no matches are possible.
            # Strip the colon from the key and try to make it otherwise
            # presentable:
            $key =~ s/://;
            my $k = $key;
            $k =~ s/\\.*//;
            my $line = '';
            my $value = '';
            my @tmpprobs = ( "- The \"$k\" field (any field matching \"$key\") is missing." );
            if ( !$default ) {
                push @tmpprobs, map { eval "return \"$_\"" } @{$explanation};
            }
            push @tmpprobs, '';
            push @errors, @tmpprobs;

	} else {

            my $matched = 0;
            my $complained = 0;
            foreach my $line (@lines) {
                $line =~ tr/\012\015//d;  # Pesky line endings
                my ($k, @slirp) = ($line =~ /^${prefix}(${key})\s*[\s=](.*)/i);
                $k =~ s/:$//;
                my $value = pop @slirp;
                if ( $value !~ /$re/ &&
                    ( !$matched || ( $matched && !$one_match_sufficient ) ) &&
                    ( !$complained || ( $complained && !$complain_once ) ) ) {
                    my @tmpprobs = map { eval "return \"$_\"" } @{$explanation};
                    if ( !$default && $tmpprobs[0] !~ /^-/ ) {
                        unshift @tmpprobs, "- The \"$k\" field is invalid.";
                    }
                    push @tmpprobs, '';
                    $complained++ if @tmpprobs;
                    push @errors, @tmpprobs;
                } else {
                    $matched = 1;
                }
                # Let the custom code (if any) have a crack at it too.
                if (ref($logic) eq 'CODE') {
                    my @tmpprobs = &{$logic}($line, $k, $value, $re, $matched, $one_match_sufficient, $complained, $complain_once, \%values);
                    $complained++ if @tmpprobs;
                    push @errors, @tmpprobs;
                }
            }

        }
    }

    if (@errors) {
	::Log(0, "FAILED.  Found the following errors:\n");
	::Log(0, "                 ".join("\n                 ", @errors)."\n\n");
    } else {
	::Log(0, "PASSED syntax check\n");
    }
    return([],[]);

}
