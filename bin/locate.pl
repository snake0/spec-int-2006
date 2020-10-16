#
# locate.pm
#
# Copyright (C) 1999-2006 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: locate.pl 4453 2006-06-26 21:08:21Z cloyce $

use strict;
use IO::Dir;
use Time::HiRes qw(time);

require 'flagutils.pl';

my $version = '$LastChangedRevision: 4453 $ '; # Make emacs happier
$version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'locate.pl'} = $version;

# This is the list of benchsets and benchmarks that will be used for the
# standalone result formatter at SPEC.  This should be updated whenever the
# *bset files change, and right before release.
my %bmarklist;
if ($::suite eq 'CPU2006') {
    %bmarklist = ('int' => { 'name' => 'int',
                             'topdir' => 'CPU2006',
                             'units' => 'SPECint',
                             'metric' => 'CINT2006',
                             'benchmarklist' => [
                                 [ '400.perlbench',	'C'   ],
                                 [ '401.bzip2', 	'C'   ],
                                 [ '403.gcc',   	'C'   ],
                                 [ '429.mcf',   	'C'   ],
                                 [ '445.gobmk', 	'C'   ],
                                 [ '456.hmmer', 	'C'   ],
                                 [ '458.sjeng', 	'C'   ],
                                 [ '462.libquantum',	'C'   ],
                                 [ '464.h264ref',	'C'   ],
                                 [ '471.omnetpp',	'CXX' ],
                                 [ '473.astar', 	'CXX' ],
                                 [ '483.xalancbmk',	'CXX' ],
                                 [ '999.specrand',	'C'   ],
                             ],
                             'no_output' => { '999.specrand' => 1 },
			       },
		     'fp' => { 'name' => 'fp',
                               'topdir' => 'CPU2006',
			       'units' => 'SPECfp',
			       'metric' => 'CFP2006',
			       'benchmarklist' => [
                                   [ '410.bwaves',	'F77'   ],
                                   [ '416.gamess',	'F77'   ],
                                   [ '433.milc',	'C'     ],
                                   [ '434.zeusmp',	'F77'   ],
                                   [ '435.gromacs',	'F77,C' ],
                                   [ '436.cactusADM',	'F,C'   ],
                                   [ '437.leslie3d',	'F'     ],
                                   [ '444.namd',	'CXX'   ],
                                   [ '447.dealII',	'CXX'   ],
                                   [ '450.soplex',	'CXX'   ],
                                   [ '453.povray',	'CXX'   ],
                                   [ '454.calculix',	'F,C'   ],
                                   [ '459.GemsFDTD',	'F'     ],
                                   [ '465.tonto',	'F'     ],
                                   [ '470.lbm', 	'C'     ],
                                   [ '481.wrf', 	'F,C'   ],
                                   [ '482.sphinx3',	'C'     ],
                                   [ '998.specrand',	'C'     ],
			       ],
                               'no_output' => { '998.specrand' => 1 },
			      }
		     );
} else {
    %bmarklist = (${main::lcsuite} => { 'name' => ${main::lcsuite},
                                       'topdir' => ${main::suite},
				       'units' => 'SPECfoo_mark',
				       'metric' => ${main::suite},
				       'benchmarklist' => [qw()] }
		 );
}

sub locate_benchmarks {
    my ($config) = @_;

    $config->{'formats'} = {} if !exists $config->{'formats'};

    my $benchdir = jp($config->top, $config->benchdir);
    my %seen;
    if ($::rawformat) {
	# We're just formatting, so don't bother to look for the
	# benchmark directories; use the hardcoded list instead
	foreach my $bset (keys %bmarklist) {
	    my $setclass = "Spec::Benchset::${bset}";
	    eval "package $setclass; \@${setclass}::ISA = qw(Spec::Benchset);";
	    my $setobj = $setclass->new($config);
	    for my $var (keys %{$bmarklist{$bset}}) {
		next if ($var eq 'benchmarklist');
		$setobj->{$var} = $bmarklist{$bset}->{$var};
	    }
	    $config->{'benchsets'}->{$setobj->name} = $setobj;
	    foreach my $bmarkref (@{$bmarklist{$bset}->{'benchmarklist'}}) {
                my ($bmark, $benchlang);
                if (isa($bmarkref, 'ARRAY')) {
                  ($bmark, $benchlang) = @{$bmarkref};
                } else {
                  $bmark = $bmarkref;
                  $benchlang = '?';
                }
		$bmark =~ /(\d+).(.*)/o;
		my ($num, $name) = ($1, $2);
		my $bmarkclass = "Spec::Benchmark::${name}";
		eval "package $bmarkclass; \@${bmarkclass}::ISA = qw(Spec::Benchmark); \$${bmarkclass}::benchname = \'$name\'; \$${bmarkclass}::benchnum = $num; \$${bmarkclass}::benchlang = \'$benchlang\'; \@${bmarkclass}::base_exe = (\'foo\');";
                if ($@) {
                    Log(0, "\nError building benchmark class for '$bmark': $@\n");
                    next;
                }
		my $bmobj = $bmarkclass->new("$num.$name", $config, $num, $name);
		push @{$setobj->{'benchmarklist'}}, $bmark;
		$config->{'benchmarks'}->{$bmark} = $bmobj;
                my $flags_file;
                if (defined($::website_formatter) && $::website_formatter) {
                  $flags_file = jp($::flag_base, $bmark.'.flags.xml');
                } else {
                  $flags_file = jp($benchdir, $setobj->{'topdir'}, $bmark, 'Spec', 'flags.xml');
                }
                if (-e $flags_file) {
                    my $objectstarttime = Time::HiRes::time;
                    (undef, $config->{'flaginfo'}->{$bmark}) =
                        main::get_flags_file($flags_file, $bmark);
                    Log(90, sprintf("    Reading + parsing $flags_file took %8.9fs\n", Time::HiRes::time - $objectstarttime));
                }
                if (!-e $flags_file ||
                    !defined($config->{'flaginfo'}->{$bmark})) {
                    Log(0, "\nERROR: The flags file for $bmark ($flags_file) could not be parsed.\n");
                    do_exit(1);
                }
	    }
	}
    } else {
	my $dh = new IO::Dir $benchdir;
	if (!defined $dh) {
	    Log(0, "\nCan't open benchmark directory '$benchdir': $!\n");
	    do_exit(1);
	}
	while (defined(my $suitename = $dh->read)) { 
	    next if $suitename =~ /^\./;
	    Log(90, "Reading suite directory for '$suitename', '$benchdir'\n");
	    my $suitestarttime = Time::HiRes::time;
	    my $dir     = jp($benchdir, $suitename);
	    my $basedir = $suitename;
	    next if !-d $dir;
	    my $dh2 = new IO::Dir $dir;
	    while (defined(my $bmname = $dh2->read)) { 
	        next if $bmname =~ /^\./;
	        Log(90, "  Reading benchmark directory for '$dir', '$benchdir', '$bmname'\n");
		my $bmstarttime = Time::HiRes::time;
		my $topdir = jp ($dir, $bmname);
		if ($bmname =~ m/^(\d{3})\.(\S+)$/) {
		    my ($num, $name) = ($1, $2);
		    if ($seen{$name} && $num < 990) {
                        # 990 and up are for "system" benchmarks like specrand
                        # which must be run but aren't reported.
			Log(0, "\n'$name' appeared as a benchmark name more than once, ignoring\n");
			next;
		    }
		    my $specdir = jp($topdir, 'Spec');
		    my $pm = jp($specdir, 'object.pm');
                    my $flags_file = jp($specdir, 'flags.xml');
		    if ($name =~ m/\./) {
			Log(0, "\nBenchmark name '$name' may not contain a '.'; ignoring\n");
		    } elsif (-d $specdir && -r $pm && -r $flags_file) {
		        my $objectstarttime = Time::HiRes::time;
			eval "package Spec::Benchmark::${name}; \@Spec::Benchmark::${name}::ISA = qw(Spec::Benchmark); require '$pm';";
			Log(90, sprintf("    Evaluated $pm in %8.9fs\n", Time::HiRes::time - $objectstarttime));
			if ($@) {
			    Log(0, "\nError requiring '$pm': $@\n");
			    next;
			}
			$objectstarttime = Time::HiRes::time;
			my $class="Spec::Benchmark::${name}";
			if (!$class->can('new')) {
			    Log(0, "\nNo 'new' for class '$class' in '$pm'\n");
			    next;
			}
			my $obj = $class->new($topdir, $config, $num, $name);
			if (!defined($obj) || !ref($obj)) {
			    Log(0, "\nError initializing '$pm'\n");
			    next;
			}
			Log(90, sprintf("    Instantiated $class in %8.9fs\n", Time::HiRes::time - $objectstarttime));
			$objectstarttime = Time::HiRes::time;
			locate_srcalts($obj);
			Log(90, sprintf("    Finding src.alts took %8.9fs\n", Time::HiRes::time - $objectstarttime));
			$seen{$name}++;
			$config->{'benchmarks'}{$bmname} = $obj;
			if (-e $flags_file) {
			    $objectstarttime = Time::HiRes::time;
			    (undef, $config->{'flaginfo'}->{$bmname}) =
				main::get_flags_file($flags_file, $bmname);
			    Log(90, sprintf("    Reading + parsing $flags_file took %8.9fs\n", Time::HiRes::time - $objectstarttime));
                            if (!defined($config->{'flaginfo'}->{$bmname})) {
                                Log(0, "\nERROR: The flags file for $bmname ($flags_file) could not be parsed.\n");
                                do_exit(1);
                            }
			}
			Log(90, sprintf("  Setting up $name took %8.9fs\n\n", Time::HiRes::time - $bmstarttime));
		    }
		} elsif ($bmname =~ /^([^\/\\:;]+)\.bset$/o) {
		    my $name = $1;
		    eval "package Spec::Benchset::${name}; \@Spec::Benchset::${name}::ISA = qw(Spec::Benchset); require '$topdir';";
		    if ($@) {
			Log(0, "\nError requiring benchset file '$topdir': $@\n");
			next;
		    }
		    my $class="Spec::Benchset::${name}";
		    if (!$class->can('new')) {
			Log(0, "\nNo 'new' for class '$class' in '$topdir'\n");
			next;
		    }
		    my $obj = $class->new($config);
		    if (!defined($obj) || !ref($obj)) {
			Log(0, "\nError initializing '$topdir'\n");
			next;
		    }
		    $config->{'benchsets'}{$obj->name} = $obj;
		} elsif ($suitename =~ m/^(\d{3})\.(\S+).tar/i) {
                  next;
                }
	    }
	    Log(90, sprintf("Setting up suite took %8.9fs\n", Time::HiRes::time - $suitestarttime));
	}
    }
    my $error = 0;
    for my $set (keys %{$config->{'benchsets'}}) {
	my $obj = $config->{'benchsets'}{$set};
	my $ref = {};
	$config->{'benchsets'}{$set}{'benchmarks'} = $ref;
	my @benchmarks = @{$obj->{'benchmarklist'}};
	for my $bench (@benchmarks) {
	    if (!exists $config->{'benchmarks'}{$bench}) {
		Log(0, "\nBenchmark Set '$set' calls for non-existant benchmark '$bench'\n");
		$obj->{'valid'} = 'X';
                $error++;
	    } else {
	        $ref->{$bench} = $config->{'benchmarks'}{$bench};
            }
	}
    }
    ::do_exit(1) if $error;
    $config->{'setobjs'} = [ map {$config->{'benchsets'}{$_}} keys %{$config->{'benchsets'}} ];
}

sub locate_formats {
    my ($config, $quiet) = @_;

    my @formats = list_files(jp($config->top, 'bin', 'formats'));
    @formats = grep (m|\/[^.][^/\\]*\.p[lm]$|o, @formats);

    my $logged = 0;
    for my $pm (@formats) { ## for each format .pl file
	my ($name) = $pm =~ m|([^/]+)\.p[lm]$|o;
        my $ok = 0;
        eval "\$ok = ::${name}_ok();";
	$ok = 1 if ($@ && $@ =~ /undefined subroutine/i);
        if (($@ && $@ !~ /undefined subroutine/i) || !$ok) {
            Log(2, ', ') if $logged && !$quiet;
            Log(2, "$name (DISABLED)") unless $quiet;
            Log(8, " [$@]");
            $logged++;
            next;
        }
	eval "package Spec::Format::${name}; \@Spec::Format::${name}::ISA = qw(Spec::Format); require '$pm';";
	if ($@) {
	    Log(102, "\nError requiring $pm for $name format: $@\nContinuing with output formats...");
            $logged = 0;
	    next;
	}

	my $class= "Spec::Format::${name}";

	if (!$class->can('new')) {
	    Log(12, "\nNo 'new' function for class '$class' in '$pm'\n");
	    next;
	}

	## run the creation method for the object, i.e., "new"
	my $obj = $class->new;
	if (!defined($obj) || !ref($obj)) {
	    Log(12, "\nError initializing format object\n");
	    next;
	}
	$config->{'formats'}{$name} = $obj;
        Log(2, ', ') if $logged && !$quiet;
	Log(2, $obj->{'name'}) unless $quiet;
        $logged++;
    }
    Log(2, "\n") unless $quiet;
}

sub locate_srcalts {
    my ($bmobj) = @_;

    my $srcaltdir = jp($bmobj->{'path'}, 'src', 'src.alt');
    my $dh = new IO::Dir $srcaltdir;
    return unless defined($dh);
    while (defined(my $dir = $dh->read)) { 
	#print  "Reading '$dir', '$srcaltdir'\n";
	next if $dir eq '.' || $dir eq '..';
        next if $dir =~ /^\./o;         # src.alt names may not begin with '.'
	my $path     = jp($srcaltdir, $dir);
	my $basedir = $dir;
	next if ! -d $path;
	my $pm = jp($path, 'srcalt.pm');
	if (! -r $pm) {
	    Log(0, "\nERROR: src.alt '$dir' for $bmobj->{'num'}.$bmobj->{'name'} contains no control file! Skipping...\n");
	    next;
	}
	eval "package Spec::Benchmark::srcalt::${dir}; require '$pm';";
	if ($@) {
	    Log(0, "\nERROR in src.alt control file '$pm': $@\n");
	} else {
	    my $infoptr;
	    {
		no strict 'refs';
		$infoptr = ${"Spec::Benchmark::srcalt::${dir}::info"};
	    }
            foreach my $member (qw(name forbench usewith filesums diffsums diffs)) {
		if (!exists $infoptr->{$member}) {
		    Log(0, "\nERROR: src.alt '$dir' for $bmobj->{'num'}.$bmobj->{'name'} has an incomplete control file; ignoring\n");
		    next;
		}
	    }
            if (ref($infoptr->{'filesums'}) ne 'HASH') {
		Log(0, "\nERROR: src.alt '$dir' for $bmobj->{'num'}.$bmobj->{'name'} has corrupt file sums; ignoring\n");
		next;
	    }
	    if ($infoptr->{'forbench'} ne $bmobj->{'num'}.'.'.$bmobj->{'name'}) {
		Log(0, "\nERROR: src.alt '$dir' for $infoptr->{'forbench'} is in $bmobj->{'num'}.$bmobj->{'name'}\'s directory; ignoring\n");
		next;
	    }
            my ($min, $max);
	    if (ref($infoptr->{'usewith'}) eq 'ARRAY') {
	        # It's a range
	        ($min, $max) = @{$infoptr->{'usewith'}};
	    } else {
	        ($min, $max) = ($infoptr->{'usewith'}, $infoptr->{'usewith'});
	    }
	    if ($::suite_version >= $min && $::suite_version <= $max) {
		$infoptr->{'path'} = $path;
		# Fix up the paths in the filesums struct
		foreach my $filepath (keys %{$infoptr->{'filesums'}}) {
		    next unless ($filepath =~ /^benchspec/);
		    $infoptr->{'filesums'}->{jp($ENV{'SPEC'}, $filepath)} =
			$infoptr->{'filesums'}->{$filepath};
		    # Neaten up
		    delete $infoptr->{'filesums'}->{$filepath};
		}
	        $bmobj->{'srcalts'}->{$infoptr->{'name'}} = $infoptr;
		Log(10, "\nFound src.alt '$infoptr->{'name'}' for $bmobj->{'num'}.$bmobj->{'name'}\n");
	    } else {
		Log(0, "\nERROR: src.alt '$infoptr->{'name'}' for $bmobj->{'num'}.$bmobj->{'name'} is not useable with this version of ${main::suite}!\n");
	    }
        }
    }
}

1;
