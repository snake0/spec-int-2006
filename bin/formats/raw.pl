#
#  raw.pl - produces RAW output
#  Copyright (C) 1995-2006 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Authors:  Christopher Chan-Nui
#            Cloyce D. Spradling
#
# $Id: raw.pl 4481 2006-06-27 21:56:47Z cloyce $

use strict;
use IO::File;
use UNIVERSAL qw(isa);

use vars qw($name $extension $synonyms $prefix);

$name      = 'raw';
$extension = 'rsf';
$synonyms  = { map { lc($_) => 1 } ($name, $extension) };
$prefix = "spec.$::lcsuite";

my $version = '$LastChangedRevision: 4481 $ '; # Make emacs happier
$version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'raw.pl'} = $version;
$Spec::Format::raw::part_of_all = 1;    # Part of '-o all' (just to be clear)

sub format {
    my($me, $r, $fn, $noresults) = @_;
    my @output;
    my $written = [];
    my %dumped = ( 'benchmarklist' => 1, 'datestr' => 1,
		   'rawfile' => 1, 'compraw' => 1, 'rawhash' => 1,
		   'flagsinfo' => 1, 'output' => 1, 'orig_raw_config' => 1,
                   'do_dump' => 1, 'graph_min' => 1, 'graph_max' => 1,
                   'graph_auto' => 1, 'forbiddenused' => 1, 'unknownused' => 1,
                   'allow_extension_override' => 1, 'output_root' => 1,
                   'expid' => 1, 'review' => 1, 'topdir' => 1, 'flags' => 1,
                 );

    my @keys = grep { !/^(refs|benchmarks)$/ } $r->list_keys;

    # Generated fields should not be dumped, or people will think that they
    # can be edited in the raw file.
    foreach my $genfield (keys %::generated_fields) {
        $dumped{$genfield} = 1;
    }

    # Dump configuration info
    for my $key (sort main::bytrailingnum @keys) {
	next if exists $dumped{$key};
	next unless ($key =~ /$::info_re/);
        next if $key =~ /^notes/;       # Notes are special now.
	my $val = $r->accessor($key);
	if (::isa($val, 'ARRAY')) {
	    for (my $i = 0; $i < @$val; $i++) {
                next unless defined($val->[$i]);        # Handle holes
		push(@output, 
		    sprintf "%s.%s%03d: %s", $prefix, $key, $i, $val->[$i]);
	    }
	} elsif (ref $val eq '') {
	    push (@output, sprintf "%s.%s: %s", $prefix, $key, $val);
	}
	$dumped{$key} = 1;
    }

    # Dump the notes.  We do this separately so that the numbers can be spaced
    # It should always be an array, but just in case it isn't, give it the
    # full treatment.
    # 'nowarn' because there might not be any notes
    foreach my $section (map { $_->[0] } @::notes_info) {
        next unless ref($r->{$section}) eq 'HASH';
        foreach my $key (sort keys %{$r->{$section}}) {
            my $val = $r->{$section}->{$key};
            next unless ref($val) eq 'ARRAY';
	    for (my $i = 0; $i < @$val; $i++) {
		my ($tag, $text) = @{$val->[$i]};
		push @output, "$prefix.$tag: $text";
	    }
        }
        $dumped{$section} = 1;
    }

    # Print a little delimiter
    push @output, '# =============== do not edit below this point ===================';

    # From here on in, we make an MD5 hash of all the lines
    my $ctx = new Digest::MD5;
    my @md5list = (); # Here's where we store the info to be hashed

    # Make sure that the suite versions exist and are integrated properly
    if (!grep { /^suitever$/o } @keys) {
	$r->{'suitever'} = $::suite_version;
	push @keys, 'suitever';
    }
    if (!grep { /^runspecver$/o } @keys) {
	$r->{'runspecver'} = $::version;
	push @keys, 'runspecver';
    }
    if (!grep { /^toolset$/o } @keys) {
	$r->{'toolset'} = $::toolset_name;
	push @keys, 'toolset';
    }
    if (!grep { /^invalid$/o } @keys) {
	# Make sure that invalid results have an invalid tag:
	if (!exists($r->{'invalid'})) {
	    if (exists($r->{'errors'})) {
		if ((ref($r->{'errors'}) eq 'ARRAY') &&
		    (@{$r->{'errors'}}+0) > 0) {
		    $r->{'invalid'} = 1;
		    $r->{'valid'} = 'X';
		}
	    } else {
		$r->{'invalid'} = 0;
	    }
	} else {
	    $r->{'invalid'} = 0 if (!defined($r->{'invalid'}));
	}
	push @keys, 'invalid';
    }

    # Put all the cfidx items together.  In most cases it will make them take
    # less space.
    my @cfidx = ();
    for my $key (sort ::bytag grep { /^cfidx_/ } @keys) {
	my $index = $r->accessor($key);
	$key =~ s/^cfidx_//;
	push @cfidx, "$key:$index";
	$dumped{$key}++;
    }
    my ($raw, $comp, $enc) = ::compress_encode(join(',', @cfidx));
    my @complines = split(/\n/, (defined($enc) && $enc ne '') ? $enc : $raw);
    $r->{'cfidx'} = [ @complines ];
    push @keys, 'cfidx';

    if (!exists($r->{'toolvers'})) {
	# Mash up the various file versions
	($raw, $comp, $enc) = ::compress_encode(join(',', map { "$_:$::tools_versions{$_}" } sort keys %::tools_versions));
	@complines = split(/\n/, (defined($enc) && $enc ne '') ? $enc : $raw);
	$r->{'toolvers'} = [ @complines ];
	push @keys, 'toolvers';
    }

    # Some things simply _must_ be dumped
    foreach my $important_thing (@{$r->{'do_dump'}}) {
        delete $dumped{$important_thing};
    }

    # Dump non-configuration, non-result info (leftovers)
    for my $key (sort @keys) {
	next if exists $dumped{$key};
	my $val = $r->accessor($key);
	if (ref $val eq 'ARRAY') {
	    for (my $i = 0; $i < @$val; $i++) {
		push(@md5list, 
		    sprintf "%s.%s%03d: %s", $prefix, $key, $i, $val->[$i]);
	    }
	} elsif (ref $val eq '') {
	    push (@md5list, sprintf "%s.%s: %s", $prefix, $key, $val);
	}
	$dumped{$key} = 1;
    }

    # Make sure there are stored compile options...
    if (!exists($r->{'compile_options'})) {
	Log(0, "\nERROR: No saved compile options in the result object!\n");
	main::do_exit(1);
    }

    # Dump result info
    my $means = (!exists($dumped{'basemean'}) || $r->accessor('basemean')) &&
                (!exists($dumped{'peakmean'}) || $r->accessor('peakmean'));
    if (defined($noresults) && $noresults && $means == 0) {
      Log(110, "Notice: Skipping results output; noresults is '$noresults'; means is '$means'\n");
    }
    for my $bench (sort keys %{$r->{'results'}}) {
	my $benchname = $bench;
	$benchname =~ y/\./_/;
	for my $tune (keys %{$r->{'results'}{$bench}}) {
	    next unless ::isa($r->{'results'}{$bench}{$tune}{'data'}, 'ARRAY');
            if (!defined($noresults) || !$noresults || $means) {
		my @tmp = sort { $a->{'iteration'} <=> $b->{'iteration'} } @{$r->{'results'}{$bench}{$tune}{'data'}};
		for (my $i = 0; $i < @tmp; $i++) {
		    for my $key (sort keys %{$tmp[$i]}) {
			next if $key eq 'ref' || $key eq 'refs' || $key eq 'tune';
			if (ref $tmp[$i]->{$key} eq 'ARRAY') {
			    my @tmp2 = @{$tmp[$i]->{$key}};
			    for (my $j = 0; $j < @tmp2; $j++) {
				push (@md5list, 
				    sprintf "%s.results.%s.%s.%03d.%s%03d: %s", 
					    $prefix, $benchname, $tune, $i, $key, 
					    $j, $tmp[$i]->{$key}->[$j]);
			    }
			} else {
			    push (@md5list, 
				sprintf "%s.results.%s.%s.%03d.%s: %s", 
					$prefix, $benchname, $tune, $i, $key, 
					$tmp[$i]->{$key});
			}
		    }
		}
            }

	    # Dump the compile options so that flag reports can be generated
	    # with just the raw file.
	    if (exists($r->{'compile_options'}->{$bench}) &&
		exists($r->{'compile_options'}->{$bench}->{$tune})) {
		my $opts = $r->{'compile_options'}->{$bench}->{$tune};
		if ($opts =~ /:/o) {
		    # It's probably not compressed or anything...
		    $opts = ::compress_encode($opts);
		}
		my @optlist = split(/\n+/, $opts);
		for (my $i = 0; $i < @optlist; $i++) {
		    push (@md5list, 
			sprintf "%s.compopts%03d.%s.%s: %s", 
				$prefix, $i, $benchname, $tune,
				$optlist[$i]);
		}
	    }
	}
    }

    foreach my $line (@md5list) {
	$line =~ tr/\015\012//d; # More reliable than the double chomp
    }
    $ctx->add(@md5list);
    push @output, "$prefix.rawhash: ".$ctx->hexdigest();
    push @output, @md5list;
    foreach my $line (@output) {
	$line =~ tr/\015\012//d; # More reliable than the double chomp
    }

    # Make sure we save a copy of the rawfile in the result object:
    if (!exists $r->{'compraw'}) {
        my (undef, $comp, $enc) = ::compress_encode(join("\n", @output)."\n");
	if (defined($comp)) {
	    $r->{'rawfile'} = undef;
	    $r->{'compraw'} = $enc;
	} else {
	    $r->{'rawfile'} = $enc;
	    $r->{'compraw'} = undef;
	}
    }

    # If forbidden or unknown flags were used, add a note to errors and set
    # the invalid flag.  This isn't done before, because we don't want to
    # permanently mark them as invalid (for that reason only) in the raw
    # file.
    if ($r->{'forbiddenused'}) {
      $r->{'invalid'} = 1;
      push @{$r->{'errors'}}, 'Forbidden flags were used!';
    }
    if ($r->{'unknownused'}) {
      $r->{'invalid'} = 1;
      if (!exists $r->{'errors'} || !::isa($r->{'errors'}, 'ARRAY') ||
          @{$r->{'errors'}} == 0) {
        push @{$r->{'errors'}}, 'Your run was marked invalid because it has one or more flags in the',
                                '"unknown" category. You might be able to resolve this problem without',
                                're-running your test; see',
                                "     http://www.spec.org/auto/${main::lcsuite}/Docs/runspec.html#flagsurl",
                                'for more information.';
      } else {
        # There are other errors, so do not hold out the false hope of
        # correction without a re-run.
        push @{$r->{'errors'}}, 'Unknown flags were used! See',
                                "     http://www.spec.org/auto/${main::lcsuite}/Docs/runspec.html#flagsurl",
                                'for information about how to get rid of this error.';
      }
    }

    if (exists($r->{'flags'}) && $r->{'flags'} ne '' &&
        $r->{'flags'} =~ /<flagsdescription>/) {
      # Save off a copy of the flags file source
      my $flag_file = $fn;
      $flag_file =~ s/\.${extension}$/.xml/;
      my $ofh = new IO::File '>'.$flag_file;
      if (defined($ofh)) {
        $ofh->print($r->{'flags'});  
        $ofh->close();
        push @{$written}, $flag_file;
      } else {
        ::Log(0, "Couldn't open $flag_file for writing: $!\n");
      }
    }

    return (\@output, $written);
}

sub Log { main::Log(@_) }

1;
