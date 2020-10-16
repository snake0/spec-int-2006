#
# parse.pl
#
# Copyright (C) 1999-2006 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: parse.pl 4678 2006-07-26 12:39:35Z cloyce $

use strict;
use UNIVERSAL qw( isa );

use Getopt::Long;
use Text::ParseWords;
require 'flagutils.pl';
require 'util.pl';

my $version = '$LastChangedRevision: 4678 $ '; # Make emacs happier
$version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'parse.pl'} = $version;

sub parse_commandline {
    my ($config, $cl_opts) = @_;
    my @macros = ();
    my @undefkeys = ();
    my $bp_bench = undef;

    if (exists $ENV{'SPEC_RUNSPEC'}) {
	unshift @ARGV, shellwords($ENV{'SPEC_RUNSPEC'});
    }

    # Here's a hack:
    # This is called each time before rawformatting a result.
    # So save @ARGV before we munge it, and reuse it if appropriate
    if (exists $ENV{'SPEC_SAVED_ARGC_DONT_EVER_SET_THIS'}) {
	# Restore @ARGV
	::Log(88, "Restoring \@ARGV: before = ".join('', @ARGV)."\n");
	@ARGV = split(/\|\|/, $ENV{'SPEC_SAVED_ARGC_DONT_EVER_SET_THIS'});
	::Log(88, "Restoring \@ARGV: after = ",join('', @ARGV),"\n");
    } else {
	# Save @ARGV
	::Log(88, "Saving \@ARGV: before = ",join('', @ARGV),"\n");
	$ENV{'SPEC_SAVED_ARGC_DONT_EVER_SET_THIS'} = join('||', @ARGV);
	::Log(88, "Saving \@ARGV: after = ",join('', @ARGV),"\n");
    }

    Getopt::Long::config("no_ignore_case");
    my @actions = ();
    my $rc = GetOptions ($cl_opts, qw(
			config|c=s
			make_no_clobber|make-no-clobber|M
			ext|extension|e=s
			help|h|?
			mach|machine|m=s
			iterations|n=s
			output_format|output-format|o=s
			speed
			rate|hrate|homogenousrate|rateclassic|r:i
			shrate|staggeredhomogenousrate:i
                        stagger=i
			size|input|i=s
			tune|tuning|T=s
			clcopies|copies|C=s
			max_active_compares|max-active-compares|maxcompares=i
			username|U=s
			rebuild|D
			nobuild|N
			deletework|d
			unbuffer|f
			ignore_errors|ignoreerror|ignore-errors|I
			verbose|debug|v=i
			version|V
			setprocgroup!
			reportable!
			strict!
			s
                        loose|l
                        fake|dryrun|dry-run|dry_run!
                        table!
			rawformat|R
                        nc=s
                        test
                        comment=s
                        configpp
			delay=i
			feedback!
			flagsurl|flags|F=s
                        reportonly|fakereport
                        update-flags|update_flags|flagupdate|flagsupdate|newflags|getflags
                        notes_wrap_columns|notes-wrap|notes-wrap-columns|noteswrap=i
                        info_wrap_columns|info-wrap|info-wrap-columns|infowrap=i
                        graph_min|graph-min|graphmin=f
                        graph_max|graph-max|graphmax=f
                        graph_auto|graph-auto!
                        http_proxy|http-proxy=s
                        http_timeout|http-timeout=i
                        review!
                        mockup|fakereportable
                        check_version|check-version
			),
			 'define|S=s' => \@macros,
			 'undef=s' => \@undefkeys,
			 'action|a=s' => \@actions,
			 'basepeak:s@' => sub { if (!defined($_[1]) || $_[1] eq '') { $bp_bench = 'full'; } else { push @$bp_bench, $_[1]; } },
			 );

    # Just in case there has been an oversight and a nonvolatile option has
    # been inserted into the list of command line options, throw it out and
    # issue a warning.
    foreach my $nvconfig (keys %$main::nonvolatile_config) {
	if (exists($cl_opts->{$nvconfig})) {
	    delete $cl_opts->{$nvconfig};
	    Log(0, "$nvconfig is immutable.  Do not attempt to set it via the command line.\n");
	}
    }

    # Check settings for ext and mach
    foreach my $what (qw(ext mach)) {
      next unless exists($cl_opts->{$what});
      if ($cl_opts->{$what} !~ /^[A-Za-z0-9_., -]+$/) {
        Log(100, "ERROR: Illegal characters in '$what'; please use only alphanumerics,\n");
        Log(100, "       underscores (_), hyphens (-), and periods (.).\n");
        do_exit(1);
      }
    }

    # Expand the list of benchmarks for basepeak
    if (ref($bp_bench) eq 'ARRAY') {
	$bp_bench = [ split(/[,:]+/, join(',', @$bp_bench)) ];
	# Now throw back everything that is an existing filename
	$config->{'bp_bench'} = [ ];
	foreach my $bench (@$bp_bench) {
	    if (-e $bench) {
		# It's a file, so put it back in @ARGV to be processed
		unshift @ARGV, $bench;
	    } else {
		push @{$config->{'bp_bench'}}, $bench;
	    }
	}
	$config->{'bp_bench'} = 'full' unless @{$config->{'bp_bench'}};
    } elsif (defined($bp_bench) && $bp_bench eq 'full') {
	$config->{'bp_bench'} = 'full';
    }

    # Immediately set verbose if asked; there's a lot of stuff that happens
    # between now and finalize_config
    $config->{'verbose'} = $cl_opts->{'verbose'} if exists($cl_opts->{'verbose'});

    # Fix up macros for use when parsing the config file
    foreach my $macro (@macros) {
        if ($macro =~ /^([^=]+)=(.*)$/o) {
            # It's a name/value pair
            $cl_opts->{'pp_macros'}->{$1} = $2;
        } else {
            # It's just a name; define it to 1
            $cl_opts->{'pp_macros'}->{$macro} = 1;
        }
    }
    foreach my $key (@undefkeys) {
	delete $cl_opts->{'pp_macros'}->{$key} if exists $cl_opts->{'pp_macros'}->{$key};
    }

    # --configpp is just a handy synonym for '--action=configpp', so act
    # accordingly
    if (exists $cl_opts->{'configpp'}) {
	push @actions, 'configpp';
	delete $cl_opts->{'configpp'};
    }

    # --fakereportable and --mockup just mean '--fakereport --reportable',
    # so fix things up.
    if (exists $cl_opts->{'mockup'}) {
       $cl_opts->{'reportonly'} = 1;
       $cl_opts->{'reportable'} = 1;
       delete $cl_opts->{'fakereportable'};
    }

    # --reportonly and --fakereport are just handy synonyms for
    # '--action=report', so act accordingly
    if (exists $cl_opts->{'reportonly'}) {
	@actions = ('report');
	delete $cl_opts->{'reportonly'};
    }

    # Check the action(s) specified.  This will help catch typos like
    # '-a <cfgfile>'.  In any case, only the last one specified gets stuffed
    # into the action slot.
    foreach my $action (map { lc($_) } @actions) {
	if (! grep { $action eq $_ } @{$::nonvolatile_config->{'valid_actions'}}) {
	    die "\n\"$action\" is not a valid action!\n\n";
	}
    }
    my $action = pop @actions;
    $cl_opts->{'action'} = $action if (defined($action) && $action);

    # 'loose', 'strict', and 'reportable' are all really ways of talking
    # about 'reportable', so munge things up properly
    # We don't do a whole lot of checking here because those would only
    # cover strange cases where the user says '--strict --noreportable'
    # or things like that, and our job isn't to help people be not stupid
    if (exists $cl_opts->{'loose'}) {
	$cl_opts->{'reportable'} = 1 - $cl_opts->{'loose'};
	delete $cl_opts->{'loose'};
    }
    for my $strictopt (qw(strict s)) {
	if (exists $cl_opts->{$strictopt}) {
	    $cl_opts->{'reportable'} = $cl_opts->{$strictopt};
	    delete $cl_opts->{$strictopt};
	}
    }

    # Run the Perl test suite, if asked
    if (exists $cl_opts->{'test'} && $cl_opts->{'test'}) {
      print "Running the Perl test suite...\n";
      chdir main::jp($ENV{'SPEC'}, 'bin');
      exec "./specperl", main::jp('test', 'TEST');
    }

    # Try to be reasonably helpful
    if (exists($cl_opts->{'nc'})) {
	if (!defined($::website_formatter) || $::website_formatter == 0) {
	    die "The --nc flag is not for you.\nStopped";
	} elsif (! -e $cl_opts->{'nc'}) {
	    die "Please specify an existing file for the NC text!\nStopped";
	} else {
	    $cl_opts->{'ncfname'} = $cl_opts->{'nc'};
	    open(TMP, "<$cl_opts->{nc}") || die "Can't open $cl_opts->{nc} for reading: $!\nStopped";
	    my $eol = $/;
	    undef $/;
	    $cl_opts->{'nc'} = [ split(/[\r\n]+/, <TMP>) ];
	    $/ = $eol;
	}
    }

    # Take care of the flags file
    if (   exists($cl_opts->{'flagsurl'})
        && $cl_opts->{'flagsurl'} ne ''
        && $cl_opts->{'flagsurl'} ne 'noflags') {
	$cl_opts->{'flagsurl'} =~ s|\\|/|g; # Change \ to / for Windows users
	if ($cl_opts->{'flagsurl'} !~ m|^[^:]+://|) {
	    if (! -e $cl_opts->{'flagsurl'}) {
		Log(0, 'ERROR: Specified flags file ('.$cl_opts->{'flagsurl'}.") could not be found.\n");
		Log(0, "       To get a flags report, you must re-format this result with a valid\n");
		Log(0, "       flags file.\n");
		delete $cl_opts->{'flagsurl'};
	    }
	} elsif ($cl_opts->{'flagsurl'} !~ /^(http|ftp|file):/) {
	    die "Unsupported flags file URL scheme; please use file:, http:, or ftp:.\nStopped";
	}
    }

    # Staggered homogenous rate => regular rate
    # Since this can now specify the number of copies as well, treat it
    # carefully...
    if (exists($cl_opts->{'shrate'})) {
        $cl_opts->{'rate'} = 1;
        if (($cl_opts->{'shrate'} >= 400 && $cl_opts->{'shrate'} < 500) ||
            $cl_opts->{'shrate'} == 998 || $cl_opts->{'shrate'} == 999) {
            Log(0, "\nWARNING: You have specified a number of copies that looks like a benchmark\n");
            Log(0, "         selection.  If this is really the correct number of copies to run,\n");
            Log(0, "         specify it using the '--copies' command line flag.\n\n");
            unshift @ARGV, $cl_opts->{'shrate'};
        } elsif ($cl_opts->{'clcopies'} eq '') {
            $cl_opts->{'clcopies'} = $cl_opts->{'shrate'};
        }
        $cl_opts->{'shrate'} = 1;
    } elsif (exists($cl_opts->{'rate'})) {
        if (($cl_opts->{'rate'} >= 400 && $cl_opts->{'rate'} < 500) ||
            $cl_opts->{'rate'} == 998 || $cl_opts->{'rate'} == 999) {
            Log(0, "\nWARNING: You have specified a number of copies that looks like a benchmark\n");
            Log(0, "         selection.  If this is really the correct number of copies to run,\n");
            Log(0, "         specify it using the '--copies' command line flag.\n\n");
            unshift @ARGV, $cl_opts->{'rate'};
        } elsif ($cl_opts->{'clcopies'} eq '') {
            $cl_opts->{'clcopies'} = $cl_opts->{'rate'};
        }
	$cl_opts->{'rate'} = 1;
    }

    if (exists($cl_opts->{'rawformat'}) && exists($cl_opts->{'speed'}) &&
        exists($cl_opts->{'rate'})) {
        die "Can't format _both_ speed and 1-copy rate results; please pick one.\n";
    }

    # Format for review?
    $config->{'review'} = $cl_opts->{'review'} if exists($cl_opts->{'review'});

    # Set the username _now_ so that immediate substitution in the config file
    # will get the "right" value.
    $config->{'username'} = $cl_opts->{'username'} if exists($cl_opts->{'username'});

    # Unset the http proxy?
    if ($cl_opts->{'http_proxy'} eq 'none') {
      delete $ENV{'http_proxy'};
      $cl_opts->{'http_proxy'} = '';
    }

    if (!$rc || istrue($cl_opts->{'help'})) {
	print "For help, type \"runspec --help\"\n";
        #usage();
	exit (!$rc ? 1 : 0);
    }
    return 1;
}

sub validate_options {
    my ($config) = @_;

    if (istrue($config->unbuffer)) {
	$|=1;
    }
    if ($config->ext eq '') {
	Log(0, "Please specify an extension!  (-e or ext= in config file)\n");
	do_exit(1);
    }

    if (!istrue($config->rawformat)) {
	# Check the types for numerics
	my ($rc, $badval);
	if (istrue($config->rate)) {
	    ($rc, $badval) = check_numbers($config->copylist);
	    if (!$rc) {
		Log(100, "'$badval' is not a valid value for copies!\n");
		do_exit(1);
	    }
	}
	($rc, $badval) = check_numbers($config->iterlist);
	if (!$rc) {
	    Log(100, "'$badval' is not a valid value for iterations!\n");
	    do_exit(1);
	}
	($rc, $badval) = check_numbers($config->max_active_compares);
	if (!$rc) {
	    Log(100, "'$badval' is not a valid value for max_active_compares!\n");
	    do_exit(1);
	}
	($rc, $badval) = check_numbers($config->verbose);
	if (!$rc) {
	    Log(100, "'$badval' is not a valid value for verbose!\n");
	    do_exit(1);
	}
    }

    if (istrue($config->fake) && !istrue($config->teeout)) {
      $config->{'teeout'} = 1;
    }
}

sub check_numbers {
    my ($aref) = @_;
    my ($rc, $badval);

    if (ref($aref) eq 'ARRAY') {
	foreach my $thing (@{$aref}) {
	    $rc = check_number($thing);
	    return ($rc, $thing) unless $rc;
	}
    } elsif (ref($aref) eq '') {
	$rc = check_number($aref);
	return ($rc, $aref) unless $rc;
    } else {
	Log(100, "Can't check object of type ".ref($aref)."!\n");
	return (1, $aref);
    }
    return(1, undef);
}

sub check_number {
    my ($thing) = @_;

    return scalar($thing =~ /^\d+$/o);
}
	
sub resolve_choices {
    my ($config, $cl_opts) = @_;
    my $default_formats = [ grep { eval "die if \$Spec::Format::${_}::non_default"; !$@ } keys %{$config->formats} ];
    my $all_formats = [ grep { eval "die unless \$Spec::Format::${_}::part_of_all"; !$@ } keys %{$config->formats} ];

    # This is a list of all the possible names for all of the output formats
    my @synonyms = map { keys %{$config->formats->{$_}->{'synonyms'}} } keys %{$config->formats};

    # If the invocation is for a run, "raw" is mandatory
    my $user_choices = 'raw,' unless istrue($config->{'rawformat'});

    # Now tack on what the user actually asked for
    $user_choices .= $config->output_format;

    # If 'all' is explicitly requested, change the list of default formats
    if ($user_choices =~ /all/i) {
      $default_formats = $all_formats;
    }

    # If 'default' is requested, that's like 'all', but without the change
    # of list.
    $user_choices =~ s/default/all/i;

    # Put the selected list of formats into a hash to weed out duplicates
    my %formatlist = map { ::get_format($config->formats, $_)->{'name'} => 1 }
                           choose_strings('Output',
                                      $user_choices,
                                      $default_formats,
                                      [ @synonyms ]);

    # Add HTML flag report if PDF or HTML (or other linkable output format) is
    # requested.
    if (exists $formatlist{'HTML'} || exists $formatlist{'PDF'}) {
	$formatlist{::get_format($config->formats, 'flags')->{'name'}}++;
    }

    # Make the final list of outputs to be generated
    $config->{'formatlist'} = [ sort ::byformat map { ::get_format($config->formats, $_)  } keys %formatlist ];

    if (istrue($config->{'rawformat'})) {
	# Just make the list of files to format.
	$config->{'runlist'} = [ @ARGV ];
    } else {
	my $action = $config->action;
	my $tmp = choose_string($action, @{$config->valid_actions});
	if (!defined $tmp) {
	    if (istrue($cl_opts->{'check_version'})) {
              return 0;
            } else {
              Log(0, "I don't know what type of action '$action' is!\n");
              do_exit(1);
            }
	}
        # Fix up the small number of synonyms that we have
        if (lc($action) eq 'onlyrun') {
          $action = 'only_run';
        } elsif (lc($action) eq 'run') {
          $action = 'validate';
        } elsif (lc($action) eq 'runsetup') {
          $action = 'setup';
        }
	$config->action($action);

	$config->{'tunelist'} = [ choose_strings('Tune', $config->{'tune'}, 
						 [ @{$config->valid_tunes} ], []) ];

        # Take care of size 'all':
        if ($::lcsuite eq 'cpu2006') {
          $config->{'size'} =~ s/\ball\b/test,train,ref/;
        } elsif ($::lcsuite eq 'mpi2006') {
          $config->{'size'} =~ s/\ball\b/medium/;
        } else {
          # It doesn't mean anything special... what did you expect?
        }

	$config->{'iterlist'} = [ split(/,+|\s+/, $config->{'iterations'}) ];
	$config->{'extlist'}  = [ split(/,+|\s+/, $config->{'ext'}) ];
	$config->{'machlist'} = [ split(/,+|\s+/, $config->{'mach'}) ];
	$config->{'copylist'} = [ 1 ];  # Placeholder

        # Validate the sizes
	$config->{'sizelist'} = [ ];
        foreach my $maybe_size (split(/,+|\s+/, $config->{'size'})) {
          if ($maybe_size =~ /^(?:all|test|train|ref)$/i) {
            push @{$config->{'sizelist'}}, $maybe_size;
          } else {
            Log(0, "Notice: '$maybe_size' is not a valid workload size.  It will be added to the list\n");
            Log(0, "         of benchmarks to run.\n");
            push @ARGV, $maybe_size;
          }
        }
        if (@{$config->{'sizelist'}} == 0) {
          Log(0, "\nNo workload size specified!\n");
          do_exit(1);
        }

        my $copies = $config->{'copies'};
	if (exists($config->{'clcopies'}) &&
            $config->{'clcopies'} ne '' && $config->{'clcopies'} != 0) {
	    $copies = $config->{'clcopies'};
	} else {
            delete $config->{'clcopies'};
        }
	$config->{'copylist'} = [ split(/,+|\s+/, $copies) ];

# Make sure there is at least one entry in each of the categories
	for (qw(extlist machlist)) {
	    $config->{$_} = [ 'default' ] if (ref($config->{$_}) ne 'ARRAY' || @{$config->{$_}} == 0);
	}
	if (ref($config->{'tunelist'}) ne 'ARRAY' ||
            @{$config->{'tunelist'}} == 0) {
            if (istrue($cl_opts->{'check_version'})) {
              return 0;
            } else {
              Log(0, "A valid tuning level must be selected using -T or --tune on the command line\n");
              Log(0, "or by setting 'tunelist' in the configuration file.\n");
              do_exit(1);
            }
	}

	my @candidates = (@ARGV);
	if (@candidates+0 < 1 && exists $config->{'runlist'}) {
	    push @candidates, split(/(?:\s+|\s*,\s*)/, $config->{'runlist'});
	}
	my @benchmarks = resolve_user_selection($config, \@candidates);

	if (!@benchmarks) {
            if (istrue($cl_opts->{'check_version'})) {
              return 0;
            } else {
              Log(0, "\nNo benchmarks specified!\n");
              do_exit(1);
            }
	}
	$config->{'runlist'} = [ @benchmarks ];
    }
    return 1;
}

sub resolve_user_selection {	## figure out which benchmarks are to be run
                                ## and return them as a list.
    my ($config, $selection) = @_;       ## typically, what is on the command line
    my @benchmarks = ();
    my %benchmark_seen;
    my $sel;
    my $set;

    @$selection = ::expand_all(@$selection);

    # Find out what benchmarks the user wants to run, knock out duplicates
    my $error = 0;
    for my $sel (@$selection) {
	my @temp = ();
	my $not = 0;
	if ($sel =~ m/^\^+(.*)/) { ## if the argument begins with a ^
	    $sel = $1;            ## it implies a NOT of the selection
	    if ($config->action =~ /(?:validate|report)/ &&
                istrue($config->reportable)) {
		Log(0, "Benchmark exclusion is not allowed for a reportable run; ignoring\n");
	    } else {
		$not = 1;
	    }
	}

	# look for the selection in the list of benchmark sets
	if (exists $config->{'benchsets'}{$sel}) {
	    my $ref = $config->{'benchsets'}{$sel}{'benchmarks'};
	    push (@temp, map { $ref->{$_} } keys %$ref);
	} else {
	    if ($config->action =~ /(?:validate|report)/ &&
                istrue($config->reportable)) { 
		Log(0, "Individual benchmark selection is not allowed for a reportable run;\n   ignoring '$sel'\n");
		next;
	    } else {
		my $temp = find_benchmark($config, $sel);
		if (!defined $temp) {
		    Log(0, "Can't find benchmark '$sel'\n");
		} else {
		    @temp = ($temp);
		}
	    }
	} 

	if (!@temp) {
	    Log(0, "Can't parse '$sel' into a benchmark\n");
	    next;
	}

        ## process the temporary list of benchmarks
	for my $bench (sort { $a->benchmark cmp $b->benchmark } @temp) {
	    my $name = $bench->benchmark;
	    if ($not) { ## delete this benchmark from the list
		## don't bother removing it if we haven't added it yet
		next if !$benchmark_seen{$name};
		Log(4, "  '$name' removed\n");
		$benchmark_seen{$name} = 0;
		## remove (filter) it from the benchmarks list
		@benchmarks = grep($name ne $_->benchmark, @benchmarks);
	    } else { ## add it to the benchmark list
		next if $benchmark_seen{$name}; ## skip if we have seen it
		push (@benchmarks, $bench); ## add it to the final list
		$benchmark_seen{$name} = 1; ## flag that we have seen this
		Log(24, "  '$name' added\n");
	    }
	}
    }
    return @benchmarks; ## return the list
}

sub find_benchmark {
    my ($config, $name) = @_;
    my @objs = map { $config->benchmarks->{$_} } keys %{$config->benchmarks};
    if ($name =~ /^(\d+)$/ || $name =~ /^(\d{1,3})\./) {
        my $benchnum = $1;
	for (@objs) {
	    return $_ if ($_->num == $benchnum);
	}
    } else {
	my $match;
	for (@objs) {
	    return $_ if ($_->name eq $name || $_->benchmark eq $name);

	    if ($_->name =~ m/^$name/) {
		return undef if defined $match;
		$match = $_;
	    }
	}

	return $match if defined $match;
    }
    return undef;
}

sub parse_raw {
    my ($fh, $config) = @_;
    my $prefix = $Spec::Format::raw::prefix;
    my $bsname = 'Who knows?';
    my $ignoreflags = 0;
    my $compopts = {};
    my %seennote = ();

    # Suck in the raw file
    my @raw = <$fh>;
    foreach my $ln (@raw) {
	if ($ln =~ /$prefix\.name:\s+(\S+)/o) { $bsname = $1; last; }
    };
    if ($bsname eq 'Who knows?') {
        Log(0, "   Not a valid ${main::suite} raw result file\n\n");
        return undef;
    }

    my $r = $config->{'benchsets'}->{$bsname};
    # Weed out benchmarks that should not be output
    foreach my $bm (sort keys %{$r->{'benchmarks'}}) {
      delete $r->{'benchmarks'}->{$bm} if exists ($config->{'benchsets'}->{$bsname}->{'no_output'}->{$bm});
    }
    my $rawfile = '';
    foreach (@raw) {
	$rawfile .= $_;
	tr/\012\015//d;		# Hooray for Microsoft!
    }
    (undef, $r->{'rawfile'}, $r->{'compraw'}) = ::compress_encode($rawfile);
    if (defined($r->{'compraw'})) {
	delete $r->{'rawfile'};
    } else {
	delete $r->{'compraw'};
    }

    # If flagsurl is set, use it; reformatting with a different flags file
    # will probably be a common thing.
    my $flagsurl = $config->accessor_nowarn('flagsurl');
    if (defined($flagsurl) && $flagsurl ne '') {
      $r->{'flagsurl'} = $flagsurl;
      $ignoreflags++;
      if ($flagsurl eq 'noflags') {
        Log(2, "Ignoring existing stored flags file.\n");
        $r->{'flaginfo'}->{'user'} = {};
      } else {
        Log(2, "Retrieving flags file ($flagsurl)...\n");
        ($r->{'flags'}, $r->{'flaginfo'}->{'user'}) =
            main::get_flags_file($flagsurl, 'user', 0,
                                 $config->http_timeout,
                                 $config->http_proxy);
        if (!defined($r->{'flaginfo'}->{'user'})) {
            Log(0, "ERROR: No usable flag description found.\n");
            do_exit(1);
        }
        my $compflags = ::compress_encode($r->{'flags'});
        $compflags = '*'.main::encode_base64($r->{'flags'}) if !defined($compflags);
        $r->{'rawflags'} = [ split(/\n/, $compflags) ];
      }
    }

    my ($tag, $value) = ('', undef);
    my (%seen) = (()); # clear, at least
    my $md5record = 0;
    my $ctx = new Digest::MD5;

    # Read in a RAW file from $fh and reconstruct a result object from it.
    while(defined($_ = shift(@raw))) {
	next if /^\s*$/o;
	next if /^\#/o;		# Allow comments in the raw file
	($tag, $value) = /^$prefix\.([^:]+):[ 	](\s*.*)/o;

	# This handles the submission info that the email handler tacks on
	if (defined($::website_formatter) && $::website_formatter &&
	    defined($::format_for_publication) &&
	    /^-SUBMIT-$/o) {
	    # Handle the submission information specially
	    my $submittedby_line = shift(@raw);
	    my $submitdate_line = shift(@raw);
	    if (!$::format_for_publication) {
		# These aren't appended to the notes on the public side
                # The tools will want to helpfully renumber these, so they're
                # given a (hopefully) unique tag and indices.
		push @{$r->{'notes'}->{''}}, (
                    [ 'notes_zzzzsubmitinfo_86', '' ],
                    [ 'notes_zzzzsubmitinfo_87', $submittedby_line ],
                    [ 'notes_zzzzsubmitinfo_88', $submitdate_line ],
                    [ 'notes_zzzzsubmitinfo_89', shift(@raw) ]
                    );
	    }
	    $submittedby_line =~ s/^[^:]+:\s+//;
	    $submitdate_line =~ s/^[^:]+:\s+//;
	    $r->{'Submitted_by'} = $submittedby_line;
	    $r->{'Submit_date'} = $submitdate_line;
	    next;
	}
	next unless (defined($tag) && defined($value) && ($tag ne ''));

	# Check for/add to the MD5 hash
	if ($md5record) {
	    tr/\015\012//d;
	    $ctx->add($_);
	}
	$md5record = 1	if ($tag eq 'rawhash');
	# The What-Goes-Where Heuristic: if $tag has no periods, it is an
	# 'info' thing.  If it does, it is a 'result' thing.
	if ($tag !~ /\./o) { # 'info' thing
            next if ($ignoreflags && $tag =~ /^(?:raw|comp)?flags(?:url|[0-9:])/o);
	    # Don't allow things that go below the fence to be set above it
	    next if (!$md5record && ($tag =~ /^(?:cfidx)/o));
	    if ($tag =~ /^(.*?)(\d+)$/o) {
		# an ARRAY 'info' thing
		my ($what, $which) = ($1, $2);
                next if ($ignoreflags && ($what =~ /^(?:raw|comp)?flags(?:url)?$/o));
                if ($what =~ /^notes/) {
                    # Now that there are so many different kinds of notes
                    # to choose from, things are more complicated.
                    my $found = 0;
                    foreach my $notere (@::notes_regexps) {
                        if ($tag =~ /$notere/) {
                            my ($section, $key, $num) = ($1, $2, $3);
                            $found = 1;
                            $r->{$section} = {} unless (exists($r->{$section})
                                            && ref($r->{$section}) eq 'HASH');
                            $r->{$section}->{$key} = [] unless (exists($r->{$section}->{$key})
                                            && ref($r->{$section}->{$key}) eq 'ARRAY');
                            if (defined($r->{$section}->{$key}->[$num]) &&
                                $r->{$section}->{$key}->[$num] ne '') {
                                Log(0, "WARNING: notes line $tag is duplicated!\n".
                                    "     Original: [".$r->{$section}->{$key}->[$num]->[1]."]\n".
                                    "  Replacement: [$value]\n".
                                    "The original value will be used.\n");
                                next;
                            } else {
                                $r->{$section}->{$key}->[$num] = [ $tag, $value ];
                                $seennote{$tag} = $r->{$section}->{$key}->[$num];
                            }
                            last;
                        }
                    }
                    next if $found;

                    # If $tag didn't match _any_ of the notes regexps, it
                    # must be some other informational array that starts with
                    # 'notes'.  (What could that be?)  Anyway, just let it
                    # fall through.
		}
                $r->{$what} = [] unless (exists ($r->{$what}) &&
                                         ref($r->{$what}) eq 'ARRAY');
                $r->{$what}->[$which] = $value;
	    } else {
		$r->{$tag} = $value;
	    }
	} else {
	    # Don't allow things that go below the fence to be set above it
	    next if !$md5record;
	    # a 'result' or 'options' thing
	    my @comps = split(/\./, $tag);
            # The first element tells what it is...
            my $what = (($#comps != 2) && ($#comps != 4)) ? '!' : shift(@comps);
            if ($what eq 'results')  {
                my ($bench, $tune, $idx, $key) = @comps;
                if (exists($r->{'iterations'}) && ($idx+1 > $r->{'iterations'})) {
                    $r->{'iterations'} = $idx+1;
                }
                $bench =~ y/_/./;
                if (!exists $seen{"$bench|$tune|$idx"}) {
                    # Make it.
                    $r->{'results'}->{$bench}->{$tune}->{'data'}->[$idx] =
                        new Spec::Config();
                    my $tmpref = $r->{'results'}->{$bench}->{$tune}->{'data'}->[$idx];
                    # Set up 'refs', so references to members will work
                    $tmpref->{'refs'}  = [ $tmpref, $r, $config ];
                    # Set up 'errors', so it's there even if there aren't any
                    $tmpref->{'errors'} = [];
                    # Say what tuning was used
                    $tmpref->{'tune'} = $tune;
		    # Keep track of which one it was
		    $tmpref->{'iteration'} = $idx + 0;
                    # Remember that it was made
                    $seen{"$bench|$tune|$idx"} = 1;
                }
                my $rref = $r->{'results'}->{$bench}->{$tune}->{'data'}->[$idx];
                if ($key eq 'reference') {
                    if (exists $r->{'reference'}->{$bench}) {
                        if ($r->{'reference'}->{$bench} != $value) {
                            Log(0, "Whoa!  Reference time for iteration $idx of $bench ($value) doesn't match the\npreviously recorded reference time of ".$r->{'reference'}->{$bench}."\n");
                        }
                    } else {
                        $r->{'reference'}->{$bench} = $value;
                    }
                }
                if ($key =~ /^(.*?)(\d+)$/o) {
                    # It's an ARRAY 'result' thing
                    $rref->{$1}->[$2] = $value;
                } else {
                    # It's a SCALAR 'result' thing
                    $rref->{$key} = $value;
                }
            } elsif ($what =~ /^compopts(\d+)$/o) {
                my $idx = $1;
                my ($bench, $tune) = @comps;
                $bench =~ y/_/./;
                $compopts->{$bench}->{$tune}->[$idx] = $value;
            } else {
                Log(0, "Hey!  Wierd line in raw file:\n$_\n");
                next; # return undef; ?
            }
	}
    }

    # Let's make sure some things are set properly
    if (!exists($r->{'invalid'})) {
	if (exists($r->{'errors'})) {
	    if ((ref($r->{'errors'}) eq 'ARRAY') &&
		(@{$r->{'errors'}}+0) > 0) {
		$r->{'invalid'} = 1;
            }
        }
        $r->{'invalid'} = 0 unless $r->{'invalid'};
    } else {
	$r->{'invalid'} = 0 if (!defined($r->{'invalid'}));
    }
    # 'table' is a special case of a variable that the
    # user should be able to override at rawformat time
    if (exists $config->{'table'}) {
	$config->{'table'} = istrue($config->{'table'});
	$r->{'table'} = $config->{'table'} if ($config->{'table'} != istrue($r->{'table'}));
    }

    # Check to see if the MD5s match
    if (exists($r->{'rawhash'})) {
	my $genmd5 = $ctx->hexdigest();
	if ($r->{'rawhash'} !~ /$genmd5/i) {
	    Log(0, "\nError: corrupt result file; unable to format.\n");
            Log(0, "(This problem may occur if you edited your raw file using an editor that\n");
            Log(0, "changed the non-user-editable portions of the file.  If you have a backup of\n");
            Log(0, "the original, please restore it and try a different editor.)\n");
	    return undef;
	}
    } else {
      Log(0, "\nERROR: corrupt result file; unable to format.\n");
      Log(0, "(The file to be formatted contains no checksum.)\n");
      return undef;
    }

    if (exists($r->{'suitever'})) {
	if (($r->{'suitever'} ne 'unknown') &&
	    ($r->{'suitever'} < 1.00)) {
	    Log(0, "The version of the ${main::suite} suite used to produce this result is older than\n  the latest production version.\n");
	} elsif ($r->{'suitever'} > 10) {
	    Log(0, "The version of the ${main::suite} suite used to produce this result is NOT a release\nversion.\n");
        }
    } else {
	Log(0, "The version of the ${main::suite} suite used to produce this result is unknown.  It\n  might not be a release version!\n");
        $r->{'suitever'} = 'unknown';
    }
    if (exists($r->{'runspecver'})) {
	if (($r->{'runspecver'} ne 'unknown') &&
	    ($r->{'runspecver'} < 1.00)) {
	    Log(0, "The version of the ${main::suite} tools used to produce this result is older than\n  the latest production version.\n");
	}
    } else {
	Log(0, "The version of the ${main::suite} tools used to produce this result is unknown.  It\n  might not be a release version!\n");
        $r->{'runspecver'} = 'unknown';
    }

    if (istrue($config->rate) || istrue($config->speed)) {
        # We are being asked to format a speed result as a 1-way rate result
        # or vice-versa
    
        my $multiplier = undef;
        my $doing = undef;
        if (istrue($config->rate) && !istrue($r->rate)) {
            $r->{'rate'} = 1;
            $multiplier = $::rate_multiplier;
            $doing = 'rate';
        } elsif (istrue($config->speed) && istrue($r->rate)) {
            $r->{'rate'} = 0;
            if ($r->{'base_copies'} != 1) {
                Log(0, "\nERROR: Can't format multi-copy rate run as speed!\n");
                return undef;
            }
            $multiplier = $::speed_multiplier;
            $doing = 'speed';
        }

        if (defined($multiplier)) {
            my $peakseen = 0;
            foreach my $key (keys %seen) {
                my ($bench, $tune, $idx) = split(/\|/, $key);
                $peakseen = 1 if ($tune =~ /peak/io);
                my $rref = $r->{'results'}->{$bench}->{$tune}->{'data'}->[$idx];
                if ($rref->size eq 'ref') {
                    my $reference = $rref->reference;
                    my $reported = $rref->reported_time;
                    $rref->{'ratio'} = ($reported)?$reference / $reported:0;
                    $rref->{'ratio'} *= $multiplier;
                } else {
                    $rref->{'reference'} = '--';
                    $rref->{'ratio'} = '--';
                }
            }
            if ($doing eq 'rate') {
                $r->{'basemean'} = $r->calc_mean_rate('base');
                $r->{'peakmean'} = $r->calc_mean_rate('peak') if $peakseen;
            } elsif ($doing eq 'speed') {
                $r->{'basemean'} = $r->calc_mean_speed('base');
                $r->{'peakmean'} = $r->calc_mean_speed('peak') if $peakseen;
            }
        }
    }

    # If a flags file was NOT specified on the command line, go ahead and
    # parse the flags in the raw file.
    if (!$ignoreflags) {
        my $rawflags = (ref($r->{'rawflags'}) eq 'ARRAY') ? join("\n", @{$r->{'rawflags'}}) : $r->{'rawflags'};
	my $parsed = 0;
	if ($rawflags ne '' && $r->{'flagsurl'} ne 'noflags') {
	  my (undef, $decode, $decomp) = decode_decompress($rawflags);
	  $r->{'flags'} = (defined($decomp)) ? $decomp : $decode;
	  if ($r->{'flags'} =~ /<flagsdescription>/) {
            $r->{'flaginfo'}->{'user'} = main::parse_flags( $r->{'flags'}, undef, 'user', undef, 'forbid' => [ 'mandatory' ] );
            if (!defined($r->{'flaginfo'}->{'user'})) {
                Log(0, "ERROR: No usable flag description found.\n");
                do_exit(1);
            }
	    $parsed = 1;
	  }
	}
	if ($parsed == 0) {
          if (exists($r->{'flagsurl'}) && $r->{'flagsurl'} ne '') {
            if ($r->{'flagsurl'} ne 'noflags') {
              Log(2, "Retrieving flags file ($r->{'flagsurl'})...\n");
              ($r->{'flags'}, $r->{'flaginfo'}->{'user'}) =
                  main::get_flags_file($r->{'flagsurl'}, 'user');
              if (!defined($r->{'flaginfo'}->{'user'})) {
                  Log(0, "ERROR: No usable flag description found.\n");
                  do_exit(1);
              } else {
                my $compflags = ::compress_encode($r->{'flags'});
                $compflags = '*'.main::encode_base64($r->{'flags'}) if !defined($compflags);
                $r->{'rawflags'} = [ split(/\n/, $compflags) ];
              }
            } else {
              delete $r->{'rawflags'};
            }
          } else {
            Log(0, "\nWARNING: Neither stored flags information nor flags file URL was found.\n");
            # But be sure to put an object in anyway...
            $r->{'flaginfo'}->{'user'} = {};
          }
        }
    }
    # Make sure the result gets copies of the other flaginfos, too:
    foreach my $flaggroup (keys %{$config->{'flaginfo'}}) {
        next if $flaggroup eq 'user';
        $r->{'flaginfo'}->{$flaggroup} = $config->{'flaginfo'}->{$flaggroup};
    }

    # At SPEC, store the flags files in a common place.  If there's already
    # a copy there, use it!  If not, make one and be sure to format it so
    # that it can be linked.
    if (defined($::website_formatter) && $::website_formatter) {
      if (defined($r->{'flags'}) && $r->{'flags'} ne '') {
        $r->{'flagsurl'} = ::store_flags_file($r->{'flags'}, $r->{'flaginfo'}->{'user'});
      } else {
        $r->{'flagsurl'} = '';
      }
    } else {
      delete $r->{'flags'};
    }

    # Put the compile options back into the result object
    foreach my $bench (keys %{$compopts}) {
        next unless ref($compopts->{$bench}) eq 'HASH';
        foreach my $tune (keys %{$compopts->{$bench}}) {
            next unless ref($compopts->{$bench}->{$tune}) eq 'ARRAY';
            my ($opts, $decode, $decomp) =
               decode_decompress(join("\n", @{$compopts->{$bench}->{$tune}}));
            $r->{'compile_options'}->{$bench}->{$tune} = defined($decomp) ? $decomp : defined($decode) ? $decode : $opts;
        }
    }

    # Fix up the raw file indices
    if (exists($r->{'cfidx'})) {
	my $cfidx = ::decode_decompress(join('', @{$r->{'cfidx'}}));
	my ($item,$line);
	foreach my $idx (split(/,/, $cfidx)) {
	    ($item, $line) = ($idx =~ /^([^:]+):(\d+)$/);
	    $r->{'cfidx_'.$item} = $line;
	}
    }
    delete $r->{'cfidx'};

    # Compress the notes -- they will probably have gaps in them
    foreach my $section (grep { /^notes/ } keys %{$r}) {
        next unless ref($r->{$section}) eq 'HASH';
        foreach my $key (keys %{$r->{$section}}) {
            next unless ref($r->{$section}->{$key}) eq 'ARRAY';
            ::squeeze_undef($r->{$section}->{$key});
        }
    }

    if ($config->info_wrap_columns) {   # Most likely!
        # Split long lines for informational fields.
        # These changes will be propagated back into the config file.
        foreach my $item (sort ::bytag grep { /^(?:hw_|sw_)/ } keys %{$r}) {
            my $value = $r->{$item};
            # The default (50 characters) seems like a reasonable limit for
            # things that will only get half the width of a page.
            if (ref($value) eq 'ARRAY') {
                # It's already multiple lines!
                for(my $i = 0; $i < @{$value}; $i++) {
                    next if length($value->[$i]) <= $config->info_wrap_columns;
                    Log(0, "\nNOTICE: $item$i is longer than ".$config->info_wrap_columns." characters and will be split\n");
                    my @newlines = ::wrap_lines([$value->[$i]], $config->info_wrap_columns);
                    if (@newlines > 1) {
                        # A wrap happened; splice it back in to the array
                        splice @{$value}, $i, 1, @newlines;
                    }
                }
            } else {
                next if length($value) <= $config->info_wrap_columns;
                Log(0, "\nNOTICE: $item is longer than ".$config->info_wrap_columns." characters and will be split\n");
                my @newlines = ::wrap_lines([$value], $config->info_wrap_columns);
                if (@newlines > 1) {
                    # A wrap happened; the value is now an array
                    $r->{$item} = [ @newlines ];
                }
            }
        }
    }

    # Assemble the hw_ncpu field from the various components
    $r->{'hw_ncpu'} = ::assemble_cpu_description($r);

    # Now (whee!) fix up the stored config file, if any of the lines don't
    # match any more.
    my @idxitems = reverse sort ::bytag grep { s/^cfidx_// } keys %{$r};
    $r->{'txtconfig'} = [ split(/\n/, ::decode_decompress(join("\n", @{$r->{'rawconfig'}}))) ];
    delete $r->{'rawconfig'};
    if (exists($r->{'origconfig'})) {
	$r->{'orig_raw_config'} = [ split(/\n/, ::decode_decompress(join("\n", @{$r->{'origconfig'}}))) ];
        $r->{'orig_raw_good'} = 1;
    } else {
        # Save off a copy of the orig config (if one isn't already done).
        # Later on we'll delete it if orig_raw_good isn't set.  This is done
        # because now it sometimes gets modified before update_stored_config
        # sees it.
        @{$r->{'orig_raw_config'}} = @{$r->{'txtconfig'}};
    }

    # First go through and fix up any lines that still exist.  Also notice
    # which are gone.
    my %missing = ();
    foreach my $idxitem (@idxitems) {
        next unless exists($r->{'cfidx_'.$idxitem});  # Might have been deleted
	my ($now, $is_missing) = (undef, 0);
	my $old = $idxitem;
	my $line = $r->{'cfidx_'.$idxitem};
	my $was = $r->{'txtconfig'}->[$line];
	if ($idxitem =~ /^(.*?)(\d+)$/) {
	    # An array thing.
	    my ($item, $idx) = ($1, $2);
	    if (!exists($seennote{$idxitem}) && !exists($r->{$item})) {
                # It's gone in the raw file.  Mark it for deletion.
                $is_missing = 1;
	    } elsif ($idxitem !~ /^notes/ &&
                     !::isa($r->{$item}, 'ARRAY') &&
                     $idx != 0) {
                if (exists($r->{'cfidx_'.$item})) {
                    # A bug in pre-relase tools could cause duplicate indices
                    # to be inserted for single-element items with indices > 0
                    # in the config file.  Rewriting will fail because the
                    # tags are similar, so just suppress the original.
                    delete $r->{'cfidx_'.$item};
                    next;
                }
		# It was a single thing in the config file, but has become a
                # multi thing in the raw file.  Mark it for deletion.
		$is_missing = 1;
	    } else {
		if ($item =~ /^notes/) {
		    # Notes are special because their indices do not (except
		    # for notes000) correspond to their array positions.
		    # Also, notes are arrays internally.
		    $now = $seennote{$idxitem}->[1];
		} else {
		    if (ref($r->{$item}) ne 'ARRAY') {
			$idxitem = $item;
			$now = $r->{$item};
		    } else {
			$now = $r->{$item}->[$idx];
		    }
		}
	    }
	} else {
	    # An easy, normal single thing
	    if (!exists($r->{$idxitem})) {
		# Item no longer present or has become multi-valued; mark its
		# line for deletion.
		$is_missing = 1;
                if ($idxitem eq 'tools_added_default_section' &&
                   $r->{'txtconfig'}->[$r->{'cfidx_'.$idxitem}] =~ /^default[=:]/) {
                  # Oh, it's not missing after all...
                  $is_missing = 0;
                }
	    } else {
		$now = $r->{$idxitem};
		if (ref($r->{$idxitem}) eq 'ARRAY') {
		    $now = $r->{$idxitem}->[0];
		    $idxitem .= '000';
		}
	    }
	}
	if ($is_missing) {
	    # Put it on the dead list
	    $missing{$idxitem} = 1;
	} else {
	    # Re-escape any bare # in $now; these are unescaped in the raw
	    # file but _must_ be escaped in the config file.
	    $now =~ s/\#/\\\#/g;

            # Get the position of the current line (if it exists); it'll
            # be used in a few different places below.
            my $pos = exists($r->{'cfidx_'.$idxitem}) ? $r->{'cfidx_'.$idxitem} : undef;

            # Strip any block quote markers, as they may cause the match
            # to falsely fail.  This will also save the marker, because
            # if the value really _has_ changed we'll have to strip everything
            # to the marker from the config file.  This is really only
            # a concern for single-value block quotes, as the notes splitter
            # will take care of the others.
            my $blockquote = undef;
            if ($was =~ s/^(\s*\Q${idxitem}\E\s*=\s*)<<(\S+)\s*$/$1/) {
                $blockquote = $2;
                # What's more, the value that the tools will think should be
                # on that line is actually on the _next_ line.  So fake it
                # up here... if it's been changed, it'll all get munged anyway.
                if ($r->{'txtconfig'}->[$pos+1] !~ /^$blockquote\s*$/) {
                    # If the next line is the end marker, then the note is
                    # empty; otherwise it's what's here.
                    $was .= $r->{'txtconfig'}->[$pos+1];
                }
            }
	    # See if the old and the new values match.  The config file
	    # line has the variable _and_ the value, so take that into
	    # account.
	    if ($idxitem ne 'tools_added_default_section' &&
                $was !~ /^\s*\Q${idxitem}\E\s*=\s*\Q${now}\E$/) {
		# It changed, so update it
                if (defined($blockquote) && $blockquote ne '' &&
                    defined($pos) && $pos ne '') {
                    # We know that the line that started the block will
                    # be changed or deleted, so go ahead and erase up to
                    # and including the end marker.
                    my $lines = 1;
                    while($r->{'txtconfig'}->[$pos+$lines] !~ /^$blockquote\s*$/) {
                        $lines++;
                    }
                    splice @{$r->{'txtconfig'}}, $pos+1, $lines;
                    ::shift_indices($r, $pos+1, -$lines);
                }
                my $curr_action;
                if (defined($now) && $now ne '') {
                    ::Log(15, "Stored config: Updating \"$idxitem\"".($old ne $idxitem ? " (formerly \"$old\")" : '')."\n");
                    $curr_action = 'Update';
                } else {
                    ::Log(15, "Stored config: Deleting \"$idxitem\"\n");
                    $curr_action = 'Deletion';
                }
		if (!::update_stored_config($r, $old, $idxitem, $now, 1)) {
		    ::Log(0, "ERROR: Edit of stored config file failed; $curr_action of $idxitem failed.\n");
		} elsif ($was =~ /\\\s*$/) {
                    # The line we just fixed up was part of a continuation,
                    # so it's important to remove the rest of them.
                    $pos = $r->{'cfidx_'.$idxitem};  # Might be different now
                    if (defined($pos) && $pos ne '') {  # Just in case
                        my $lines = 1;
                        while($r->{'txtconfig'}->[$pos + $lines] =~ /\\\s*$/) {
                            $lines++;
                        }
                        # It is intentional that this removes one more line
                        # than ends with a backslash.
                        splice @{$r->{'txtconfig'}}, $pos+1, $lines;
                        ::shift_indices($r, $pos+1, -$lines);
                    }
                }
	    }
	}
    }

    # Process additions of non-notes items
    foreach my $info_item (grep { /$::info_re/ } sort ::bytag keys %{$r}) {
        next if $info_item =~ /^notes/;         # Notes are special now.
        next if exists($::generated_fields{$info_item});  # Skip generated ones
	if (ref($r->{$info_item}) eq 'ARRAY') {
	    # Process each item separately
	    for (my $i = 0; $i < @{$r->{$info_item}}; $i++) {
		my $tag = sprintf '%s%03d', $info_item, $i;
		my $neighbor = undef;
		my $text = $r->{$info_item}->[$i];
                next unless defined($text) && $text ne '';

		if (!exists($r->{'cfidx_'.$tag})) {
                    $neighbor = get_neighbor_idx($r, $r->{$info_item}, $i, $info_item);

		    if (defined($neighbor) && $neighbor ne '') {
			$tag = "$tag:$neighbor";
			::Log(15, "Stored config: Adding new \"$tag\" \@ $neighbor\n");
		    } else {
			::Log(15, "Stored config: Adding new \"$tag\" \@ EOF\n");
		    }
		    if (!::update_stored_config($r, undef, $tag, $text, 1)) {
			::Log(0, "ERROR: Addition of '$tag' to stored config file failed.\n");
		    }
		}
	    }
	} else {
	    # It's just a single thing.
	    if ($r->{$info_item} ne '--' && $r->{$info_item} ne '' &&
		!exists($r->{'cfidx_'.$info_item})) {
		my $pos = find_singleton_pos($r, $info_item);
		my $tag = $info_item;
		::Log(15, "Stored config: Adding new \"$tag\" \@ ".(defined($pos) ? $pos : 'EOF')."\n");
		$tag .= ":$pos" if defined($pos);
		if (!::update_stored_config($r, undef, $tag, $r->{$info_item}, 1)) {
		    ::Log(0, "ERROR: Addition of '$info_item' to stored config file failed.\n");
		}
	    }
	}
    }

    # Go through all notes, wrapping (if necessary), doing additions, and
    # renumbering.
    foreach my $section (grep { /^notes/ } keys %{$r}) {
        next unless ref($r->{$section}) eq 'HASH';
        foreach my $key (keys %{$r->{$section}}) {
            my $notesref = $r->{$section}->{$key};
            next unless ref($notesref) eq 'ARRAY';
	    for (my $i = 0; $i < @{$notesref}; $i++) {
		my $neighbor = undef;
                my $tag = $notesref->[$i]->[0];
                my $text = $notesref->[$i]->[1];

		if (!exists($r->{'cfidx_'.$tag})) {
                    my $neighbor = get_neighbor_idx($r, $notesref, $i, undef, 0);
		    if (defined($neighbor) && $neighbor ne '') {
			$tag = "$tag:$neighbor";
			::Log(15, "Stored config: Adding new \"$tag\" \@ $neighbor\n");
		    } else {
			::Log(15, "Stored config: Adding new \"$tag\" \@ EOF\n");
		    }
		    if (!::update_stored_config($r, undef, $tag, $text, 1)) {
			::Log(0, "ERROR: Addition of '$tag' to stored config file failed.\n");
		    }
		}
	    }

            # So that it's possible to fix up the stored config file and the
            # associated indices, wrap the notes lines one by one
            if ($config->notes_wrap_columns > 0) {
                for(my $i = 0; $i < @{$notesref}; $i++) {
                    my ($tag, $note) = @{$notesref->[$i]};
                    if (length($note) >= $config->notes_wrap_columns) {
                        my @newlines = main::wrap_lines( [ $note ],
                                                         $config->notes_wrap_columns,
                                                         $config->notes_wrap_indent);
                        if (@newlines > 1) {
                            # A line was wrapped
                            $seennote{$tag} = $notesref->[$i] = [ $tag, shift(@newlines) ];
                            splice @{$notesref}, $i + 1, 0, map { [ undef, $_ ] } @newlines;
                        }
                    }
                }
            }
        }
    }

    # Process deletions
    foreach my $idxitem (sort ::bytag keys %missing) {
	::Log(15, "Stored config: Deleting \"$idxitem\"\n");
	if (!::update_stored_config($r, $idxitem)) {
	    ::Log(0, "ERROR: Edit of stored config file failed; Could not delete line for $idxitem\n");
	}
    }

    # Actually do the renumbering now.  It's important to process deletions
    # first to handle the case where an existing notes line wants to be
    # renumbered to the name of a line that's been deleted in the raw file
    ::renumber_notes($r, 2, 0);

    # Do basepeak (if asked nicely)
    if (exists($config->{'bp_bench'}) &&
	($r->{'basepeak'} == 0 || $r->{'basepeak'} == 2)) {
	my @benchmarks = (ref($config->{'bp_bench'}) eq 'ARRAY') ? ::expand_all(@{$config->bp_bench}) : ();
	my @bp_message = (
			  '# The following setting was inserted automatically as a result of',
			  '# post-run basepeak application.',
			  'basepeak = 1'
			  );
	# Since this really affects the score, only output a raw file
	my $rawhandle = ::get_format($config->formats, 'raw');
	$config->{'formatlist'} = [ $rawhandle ];
	# ...and make sure that it has a unique name (though this _is_ a bad
	# way to go about it...
	$rawhandle->{'extension'} = 'basepeak.'.$rawhandle->{'extension'} unless $rawhandle->{'extension'} =~ /basepeak/;

	if (!@benchmarks && $config->{'bp_bench'} eq 'full') {
	    # --basepeak was specified, but without arguments
	    $r->{'basepeak'} = 1;
	    if (!grep { /^peak$/ } @{$r->{'tunelist'}}) {
		push @{$r->{'tunelist'}}, 'peak';
	    }
	    ::basepeak_munge($r);
	    # Total basepeak results have no peak flags
	    foreach my $bench (@benchmarks) {
		delete $r->{'compile_options'}->{$bench}->{'peak'};
	    }
	    # Fix up the config file; add bp_message on the first line
	    # following comments
	    my $i = 0;
	    for($i = 0; $i < $#{$r->{'txtconfig'}}; $i++) {
		last unless $r->{'txtconfig'}->[$i] =~ /^\#/;
	    }
	    splice @{$r->{'txtconfig'}}, $i, 0, ('', @bp_message);
	    # Now fix up all the indices that we've shifted
	    ::shift_indices($r, $i, @bp_message+1);
	} else {
	    if (grep { /\|peak\|/ } keys %seen) {
		for(my $i = 0; $i < @benchmarks; $i++) {
		    if (exists($config->{'benchsets'}->{$benchmarks[$i]})) {
			splice @benchmarks, $i, 1, sort keys %{$config->{'benchsets'}->{$benchmarks[$i]}->{'benchmarks'}};
			redo;
		    }
		    my $temp = find_benchmark($config, $benchmarks[$i]);
		    if (!defined($temp)) {
			Log(0, "Could not parse '$benchmarks[$i]' into a benchmark!\n");
			splice @benchmarks, $i, 1;
			redo if $i < @benchmarks;
		    } else {
			splice @benchmarks, $i, 1, $temp->benchmark;
		    }
		}
		# Per-benchmark basepeak results have copies of base flags
		# Also, put in a config section for them.
		my $insert_point = @{$r->{'txtconfig'}}+0;
		if (exists($r->{'cfidx_tools_added_default_section'})) {
		    $insert_point = $r->{'cfidx_tools_added_default_section'};
		}
		foreach my $bench (@benchmarks) {
		    $r->{'compile_options'}->{$bench}->{'peak'} = $r->{'compile_options'}->{$bench}->{'base'};
		    splice @{$r->{'txtconfig'}}, $insert_point, 0, ('', $bench, @bp_message);
		    # Now fix up all the indices that we've shifted
		    ::shift_indices($r, $insert_point, @bp_message+2);
		}
		$r->{'basepeak'} = 2;
		::basepeak_munge($r, 1, @benchmarks);
	    } else {
		Log(0, "ERROR: Cannot apply per-benchmark basepeak to base-only run!\n");
		return undef;
	    }
	}
	if ($r->{'rate'}) {
	    $r->{'basemean'} = $r->calc_mean_rate('base');
	    $r->{'peakmean'} = $r->calc_mean_rate('peak');
	} else {
	    $r->{'basemean'} = $r->calc_mean_speed('base');
	    $r->{'peakmean'} = $r->calc_mean_speed('peak');
	}
    }

    # Remove the mailto addresses from the stored configs
    if (defined($::website_formatter) && $::website_formatter) {
      my $re = '(?:mailto';
      foreach my $strip_item (qw(http_proxy ftp_proxy)) {
        $re .= "|$strip_item";
      }
      $re = qr/^(\s*$re)\s*=).*/;
      foreach my $cfg qw(txtconfig orig_raw_config) {
        next unless (exists($r->{$cfg}) && isa($r->{$cfg}, 'ARRAY'));
        $r->{$cfg} = [ map { s/$re/$1/; $_ } @{$r->{$cfg}} ];
      }
    }

    # Finally, restore the rawconfig
    $r->{'rawconfig'} = [ split(/\n/, ::compress_encode(join("\n", @{$r->{'txtconfig'}}))) ];
    delete $r->{'txtconfig'};
    if ($r->{'orig_raw_good'}) {
	$r->{'origconfig'} = [ split(/\n/, ::compress_encode(join("\n", @{$r->{'orig_raw_config'}}))) ];
    }
    delete $r->{'orig_raw_good'};

    # If different graph settings have been specified, copy them in
    foreach my $what (qw(graph_min graph_max graph_auto)) {
      if (defined($config->accessor_nowarn($what))) {
        $r->{$what} = $config->accessor_nowarn($what);
      }
    }

    return $r;
}

sub get_neighbor_idx {
    my ($r, $itemarray, $idx, $tag, $tagidx) = @_;
    my $neighbortag;
    my $neighbor = 0;

    # Figure out which neighbor should provide position info
    # for us.
    # If there's a previous thing, put it right after.
    # If there's a next thing, put it right before.
    # Otherwise just put it at the end.

    if ($idx > 0 && defined($itemarray->[$idx - 1])) {
        $neighbor = -1;
    } elsif ($idx < @{$itemarray} - 1 &&
             defined($itemarray->[$idx + 1])) {
        $neighbor = 1;
    }
    if (!defined($tagidx)) {
        # Make up the tag
        $neighbortag = sprintf 'cfidx_%s%03d', $tag, $idx + $neighbor;
    } else {
        # Get the tag from item
        return undef unless ref($itemarray->[$idx + $neighbor]) eq 'ARRAY';
        $neighbortag = 'cfidx_'.$itemarray->[$idx + $neighbor]->[$tagidx];
    }
    $neighbor = exists($r->{$neighbortag}) ? $r->{$neighbortag} - $neighbor : undef;

    return $neighbor;
}

sub find_singleton_pos {
    # Find a neighbor for $tag.  Since it's not part of an array, we can't
    # find the previous or next item of the same thing.  So...
    my ($r, $tag) = @_;
    my ($i, $p, $n) = 0;
    for ($i = 0; $i < @::field_order && $::field_order[$i] ne $tag; $i++) {
    }
    # Now $i points to the position within @::field_order
    for (my $diff = 1; $diff < @::field_order / 2; $diff++) {
	# Go for the next line after the previous field first
	if ($i - $diff >= 0) {
	    if (exists($r->{'cfidx_'.$::field_order[$i - $diff]})) {
		return $r->{'cfidx_'.$::field_order[$i - $diff]} + 1;
	    }
	}
	# If that's no good, try for the line before the next field
	if ($i + $diff <= $#::field_order) {
	    if (exists($r->{'cfidx_'.$::field_order[$i + $diff]})) {
		return $r->{'cfidx_'.$::field_order[$i + $diff]} - 1;
	    }
	}
    }

    return undef;
}

1;
