#
# config.pl
#
# Copyright (C) 1999-2006 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: config.pl 4666 2006-07-25 04:53:58Z cloyce $
#
package Spec::Config;

use strict;
use IO::File;
use Safe;
use ConfigDumper;
use File::Basename;
use UNIVERSAL qw(isa);
use vars qw(%refmapping);
%refmapping = ();

my $version = '$LastChangedRevision: 4666 $ '; # Make emacs happier
$version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'config.pl'} = $version;

# List of variables and settings that are only effective when set in the
# header section.
%::header_only = (
                  'action'                   => 1,
                  'allow_extension_override' => 1,
                  'backup_config'            => 1,
                  'bind'                     => 1,
                  'check_md5'                => 1,
                  'check_version'            => 1,
                  'command_add_redirect'     => 1,
                  'expand_notes'             => 1,
                  'expid'                    => 1,
                  'ext'                      => 1,
                  'flagsurl'                 => 1,
                  'http_proxy'               => 1,
                  'http_timeout'             => 1,
                  'ignore_errors'            => 1,
                  'ignore_sigint'            => 1,
                  'info_wrap_columns'        => 1,
                  'iterations'               => 1, # Not _supposed_ to be true,
                                                   # but yields surprises when
                                                   # used in named sections.
                  'line_width'               => 1,
                  'locking'                  => 1,
                  'log_line_width'           => 1,
                  'mach'                     => 1,
                  'mail_reports'             => 1,
                  'mailcompress'             => 1,
                  'mailmethod'               => 1,
                  'mailport'                 => 1,
                  'mailserver'               => 1,
                  'mailto'                   => 1,
                  'mail_reports'             => 1,
                  'mean_anyway'              => 1,
                  'minimize_builddirs'       => 1,
                  'minimize_rundirs'         => 1,
                  'nobuild'                  => 1,
                  'notes_wrap_columns'       => 1,
                  'notes_wrap_indent'        => 1,
                  'output_format'            => 1,
                  'output_root'              => 1,
                  'rate'                     => 1,
                  'rebuild'                  => 1,
                  'reportable'               => 1,
                  'runlist'                  => 1,
                  'section_specifier_fatal'  => 1,
                  'sendmail'                 => 1,
                  'setprocgroup'             => 1,
                  'size'                     => 1,
                  'sysinfo_program'          => 1,
                  'strict_rundir_verify'     => 1, # Not strictly true, but
                                                   # necessary for --reportable
                                                   # to be able to enforce it.
                  'table'                    => 1,
                  'teeout'                   => 1,
                  'tune'                     => 1,
                  'unbuffer'                 => 1,
                  'use_version_url'          => 1,
                  'verbose'                  => 1,
                 );

# Create a new object
sub new {
    my ($class) = @_;
    my $me = bless {}, $class;

    # These are used for accessor lookups.  Initialize to be the base level
    $me->{'refs'} = [ $me ];

    return $me;
}

# Dump the config object, resolving refs into human-readable (sort of) form:
sub dumpconf {
    my ($me, $name) = @_;

    my $dumper = new ConfigDumper([$me], ['*'.$name]);
    $dumper->Indent(1);
    $dumper->Translate([qw(config refs)]);
    $dumper->Refhash(\%refmapping);
    $dumper->Sortkeys(1);
    print $dumper->Dump;
}

# Load a file and merge it with the current data
sub merge {
    my ($me, $filename, $comment, $pp_macros, %opts) = @_;
    my @prev = isa($opts{'prev'}, 'ARRAY') ? @{$opts{'prev'}} : ();
    my ($include, $name, $value, @vals);
    my @reflist = isa($opts{'reflist'}, 'ARRAY') ? @{$opts{'reflist'}} : ($me);
    my @curr_sections = isa($opts{'curr_sections'}, 'ARRAY') ? @{$opts{'curr_sections'}} : ('header');
    my $continued = 0;
    my $appended = 0;
    my ($blockquote, $first_blockquote) = (undef, undef);
    my $eol_carp_done = 0;
    my @lastrefs = ();
    my %seen_extensions = isa($opts{'seen_extensions'}, 'HASH') ? %{$opts{'seen_extensions'}} : ( 'default' => 1, 'none' => 1 );
    my $included = (@prev+0 > 0);
    my $tmppath = ($filename =~ m#^/#) ? $filename : jp($me->top, $me->configdir, $filename);
    my $sysinfo_run = 0;	# Have we run the system information program?
    my @pp_state = (1);		# 1 => include lines, 0 => don't
    my @pp_good = ();		# Keep track of which nesting levels have true ifs
    $pp_macros = {} unless ref($pp_macros) eq 'HASH';
    $pp_macros->{'runspec'} = $me->runspec;
    my @bmlist = ('default', keys %{$me->{'benchsets'}},
		  map { $_->benchmark } values %{$me->{'benchmarks'}});
    my @tunelist = ('default', @{$me->{'valid_tunes'}});
    my $trimmedcomment = '';
    my $md5mode = 0;
    my (%sections, %variables);
    my %fixup_needed = isa($opts{'fixup_needed'}, 'HASH') ? %{$opts{'fixup_needed'}} : ();

    if (-f $tmppath) {
        # The name specified exists!  Yay!
        $filename = $tmppath;
    } else {
      # First try failed; does "$filename".cfg exist?
      if (-f $tmppath.'.cfg') {
        # Yes; use it
        $filename .= ".cfg";
        $filename = jp($me->top, $me->configdir, $filename) unless $filename =~ m#^/#;
      } else {
        if ($opts{'missing_ok'}) {
          # Don't complain about it.
          return 1;
        } else {
          # No.  Complain about it.
          if ($filename =~ m#^/#) {
              Log(100, "Neither config file '$filename' nor '${filename}.cfg' exist!\n");
          } else {
              Log(100, "Neither config file '$filename' nor '${filename}.cfg' exist in ".jp($me->top, $me->configdir)."!\n");
          }
          return 0;
        }
      }
    }

    # See if this is one of the default configs.
    if (exists $::file_md5{$filename} ||
    	($^O =~ /MSWin32/ && grep { /$filename/i } keys %::file_md5)) {
      my $warning = <<"EOW";

=============================================================================
Warning:  You appear to be using one of the config files that is supplied
with the SPEC $::suite distribution.  This can be a fine way to get started.

Each config file was developed for a specific combination of compiler / OS /
hardware.  If your platform uses different versions of the software or
hardware listed, or operates in a different mode (e.g. 32- vs. 64-bit mode),
there is the possibility that this configuration file may not work as-is. If
problems arise please see the technical support file at

  http://www.spec.org/$::lcsuite/Docs/techsupport.html

A more recent config file for your platform may be among result submissions at

  http://www.spec.org/$::lcsuite/ 

Generally, issues with compilation should be directed to the compiler vendor.
You can find hints about how to debug problems by looking at the section on
"Troubleshooting" in
  http://www.spec.org/$::lcsuite/Docs/config.html

This warning will go away if you rename your config file to something other
than one of the names of the presupplied config files.

==================== The run will continue in 30 seconds ====================
EOW
      Log(0, $warning);
      sleep 30;
    }

    # Break include loops
    if ($included &&
	grep { $_ eq $filename } @prev) {
	Log(100, "ERROR: include loop detected.  Here is the include chain:\n    ".
	    join("\n    ", ($filename, @prev))."\nIgnoring topmost entry.\n");
	return 1;
    }

    $me->{'configpath'} = $filename unless $included;
    my $fh = new IO::File;
    if (!$fh->open($filename)) {
	Log(0, "ERROR: Can't read config file '$filename': $!\n");
	return 0;
    }

    Log(0, "Reading ".($included ? 'included ' : '')."config file '$filename'\n");

    if (!$included) {
	$me->{'oldmd5'} =  '';
        my @stuff = (
          "# Invocation command line:",
          "# $0 ".join(' ', @{$me->{'orig_argv'}}),
          '# output_root was not used for this run',
          "############################################################################");
	$me->{'rawtxtconfigall'} = [ @stuff ];
	$me->{'rawtxtconfig'} = [ @stuff ];
	$me->{'pptxtconfig'} = [ @stuff ];
    }
    if ($comment ne '') {
	my @comments = map { "# $_" } split(/(?:\r\n|\n)/, $comment);
        push @comments, '############################################################################';
        foreach my $list (qw(pptxtconfig rawtxtconfig rawtxtconfigall)) {
            push @{$me->{$list}}, @comments;
        }
    }

    my ($cfline, @cflines) = (<$fh>);
    my $cflinenum = 1;
    while (defined($cfline)) {
        $cfline =~ tr/\012\015//d;
	last if ($cfline =~ m/^__END__$/o);
	if ($cfline =~ m/^__MD5__$/o) {
	    if ($included) {
		last; # Ignore MD5 sections in included files
	    }
	    if (!$sysinfo_run) {
		unshift @cflines, "\n", $me->get_sysinfo(), $cfline;
	        $sysinfo_run = 1;
	    } else {
		$md5mode = 1;
	    }
	    $cfline = shift(@cflines);
            $cflinenum++;
	    next;
	}

        # Idiot guard
        if (!defined($blockquote) && !$continued && !$appended &&
            !$eol_carp_done && $cfline =~ m#^\s*([-/])#) {
          my $eol_carp = <<"EOCarp";

   =====================================================================
   Notice: Found a line at line number $cflinenum that starts with "$1"
            in "$filename":

$cfline

   This is not normally useful syntax in a SPEC $::suite config file.
   Have you perhaps passed a config file through a tool that
   thought it would be "helpful" to introduce an end-of-line
   character in the middle of a line that has a series of
   options?  (Some primitive email clients have been known to
   destroy technical content by doing so.)

   Only the first instance will receive this warning.  Lines
   like these will probably end up being ignored.

   Continuing in 10 seconds...
   =====================================================================

EOCarp
          ::Log(0, $eol_carp);
          $eol_carp_done = 1;
          sleep 10;
        }

	if ($md5mode) {
	    $me->{'oldmd5'} .= $cfline;
	} else {
	    # Save off this line but not protected comments that begin with '#>'
	    push @{$me->{'rawtxtconfigall'}}, $cfline;
	    if ($cfline =~ m/^\s*include:\s*\S+\s*$/i) {
		push @{$me->{'rawtxtconfig'}}, "#$cfline";
	    } elsif ($cfline !~ m/^\s*\#>/) {
		push @{$me->{'rawtxtconfig'}}, $cfline;
	    }
	}

        # Trim comments
	$trimmedcomment = '';
	# Bare # => comment; \# => #
	$trimmedcomment = $1 if ($cfline =~ s/(\s*(?<!\\)\#.*)//o);
        $cfline =~ s/\\\#/\#/og; # Unescape the escaped #

	# Do the preprocessor stuff
	if ($cfline =~ m/^%/o) {
	    if ($md5mode) {
		# No PP stuff in MD5 section
		$cfline = shift(@cflines);
                $cflinenum++;
		next;
	    }
	    if ($cfline =~ m/^%\s*(warning|error|define|undef|ifdef|ifndef|else|elif|if|endif)\s*(.*?)\s*$/o) {
		my ($what, $rest) = (lc($1), $2);

		# Handle conditionals first.  Since we must pay attention for
		# %endif to end an exclusionary block, we also must pay
		# attention to %if and %ifdef so that one of _their_ %endifs
		# doesn't improperly end an enclosing block.
		if ($what eq 'endif') {
		    shift @pp_state;
		    if (@pp_state+0 <= 0) {
			Log(100, "ERROR: Unmatched %endif at $filename line $cflinenum\n");
			do_exit(1);
		    }

		} elsif ($what eq 'ifdef' || $what eq 'ifndef') {
		    # Make it possible to do this the easy way
                    if ($rest !~ /^%/) {   # ...and catch the common mistake
                        Log(100, "WARNING: Syntax error in %$what conditional; attempting to fix...\n");
			$rest = '%'.'{'.$rest.'}';
		    }
		    $rest = pp_macro_expand($rest, $pp_macros, 0, 0, '', 0);
		    $rest = 'defined(undef)' if $rest eq '';
		    $rest = '!'.$rest if $what eq 'ifndef';
		    unshift @pp_state, $pp_state[0] ? eval_pp_conditional($rest, $pp_macros, $filename, $cflinenum) : 0;
		    $pp_good[$#pp_state] = $pp_state[0];

		} elsif ($what eq 'else') {
		    shift @pp_state;
		    unshift @pp_state, $pp_good[$#pp_state + 1] == 0 ? 1 : 0;
		    # Set this so that if someone puts _more_ else or elif
		    # blocks after that they don't get executed.
		    $pp_good[$#pp_state] = 1;
		    

		} elsif ($what eq 'elif') {
		    # Only evaluate the %elif if no other blocks have been true
		    shift @pp_state;
		    unshift @pp_state, $pp_good[$#pp_state + 1] == 0 ? eval_pp_conditional($rest, $pp_macros, $filename, $cflinenum) : 0;
		    $pp_good[$#pp_state] = $pp_state[0] if (!$pp_good[$#pp_state]);

		} elsif ($what eq 'if') {
		    unshift @pp_state, $pp_state[0] ? eval_pp_conditional($rest, $pp_macros, $filename, $cflinenum) : 0;
		    $pp_good[$#pp_state] = $pp_state[0];
		}

		# If we're currently excluding lines, don't go any further in
		# order to avoid processing any directives that shouldn't be
		# seen.
		if (!$pp_state[0]) {
		    $cfline = shift(@cflines);
		    $cflinenum++;
		    next;
		}

		if ($what eq 'define') {
		    my ($symbol, $value) = $rest =~ m/(\S+)\s*(.*)/;
                    $value = 1 if (!defined($value) || ($value eq ''));
                    if ($symbol =~ m/^%{(.*)}$/) {
                        $symbol = $1;
                        Log(100, "Notice: Macro names should not be enclosed in '%{}' for definition.\n");
                        Log(100, "        Attempting to do the right thing...\n");
                    }

		    if (exists ($pp_macros->{$symbol})) {
			Log(100, "WARNING: Redefinition of preprocessor macro '$symbol' at $filename line $cflinenum\n");
		    }
		    $pp_macros->{$symbol} = $value;

		} elsif ($what eq 'undef') {
		    if (exists ($pp_macros->{$rest})) {
			delete $pp_macros->{$rest};
		    } else {
			Log(100, "ERROR: Can't undefine nonexistant symbol '$rest' at $filename line $cflinenum\n");
		    }
		} elsif ($what eq 'warning') {
		    Log(100, 'WARNING: '.pp_macro_expand($rest, $pp_macros, 0, 0, $filename, $cflinenum, 1)."\n");
		} elsif ($what eq 'error') {
		    Log(100, 'ERROR: '.pp_macro_expand($rest, $pp_macros, 0, 0, $filename, $cflinenum, 1)."\n");
		    do_exit(1);
		}
		# We've handled the preprocessor directive; start over.
		$cfline = shift(@cflines);
                $cflinenum++;
		next;
	    } else {
		Log(100, "ERROR: Unknown preprocessor directive: $_\n");
		do_exit(1);
            }
	}

	# Here's the main place where we skip stuff if we're in an
	# exclusionary block.
	if (!$pp_state[0] && !$md5mode) {
	    $cfline = shift(@cflines);
            $cflinenum++;
	    next;
	}

	# Expand any macros that may be present
	if (!$md5mode) {
	    $cfline = pp_macro_expand($cfline, $pp_macros, 0, 0, $filename, $cflinenum);
	    my $tmpstr = $cfline;
	    my $cnt = $tmpstr =~ tr/\015\012//d;
	    $tmpstr .= $trimmedcomment;
	    if ($cfline =~ m/^\s*include:\s*\S+\s*$/i) {
		$tmpstr = "#$tmpstr";
	    }
            push @{$me->{'pptxtconfig'}}, $tmpstr;
            push @{$me->{'pptxtconfig'}}, '' if $cnt;
	}

	# Handle <<EOT type blocks in config file
	if (defined $blockquote) {
	    if ($cfline eq $blockquote) {
		undef $blockquote;
	    } elsif ($first_blockquote) {
		map { $$_ .= $cfline } @lastrefs;
		$first_blockquote=0;
	    } else {
		map { $$_ .= "\n$cfline" } @lastrefs;
	    }

	# Handle continued lines with '\' at end of line
	} elsif ($continued) {
	    $continued = 0;
	    $appended = 1 if ($cfline =~ s/\\\\$//);
	    $continued = 1 if ($cfline =~ s/\\$//);
	    map { $$_ .= "\n$cfline" } @lastrefs;

        # Handle appended lines with '\\' at end of line
	} elsif ($appended) {
            $cfline =~ s/^\s+//;
	    $appended = 0;
	    $appended = 1 if ($cfline =~ s/\\\\$//);
	    $continued = 1 if ($cfline =~ s/\\$//);
	    map { $$_ .= " $cfline" } @lastrefs;
            # Fix up the lines that were just present; doing this here
            # avoids lots of nastiness in the raw file parser.
            foreach my $list (qw(pptxtconfig rawtxtconfig rawtxtconfigall)) {
                pop @{$me->{$list}};
                $me->{$list}->[$#{$me->{$list}}] .= " $cfline";
            }

        # Include a file
	} elsif (($include) = $cfline =~ m/^\s*include:\s*(\S+)\s*$/i) {
            if ($include =~ m/\$[\[\{]([^\}]+)[\}\]](\S*)\s*$/) {
		my ($tmpname, $rest) = ($1, $2);
                if ($tmpname =~ s/^ENV_//) {
                    $include = $ENV{$tmpname}.$rest;
                } else {
                    $include = $me->{$tmpname}.$rest;
                }
	    }
            @{$opts{'prev'}} = ($filename, @prev);
            @{$opts{'reflist'}} = @reflist;
            @{$opts{'curr_sections'}} = @curr_sections;
            %{$opts{'seen_extensions'}} = %seen_extensions;
            %{$opts{'sections'}} = %sections;
            %{$opts{'fixup_needed'}} = %fixup_needed;
	    if (! $me->merge($include, " ----- Begin inclusion of '$include'",
			     $pp_macros, %opts)) {
		Log(100, "Can't include file '$include'\n");
		do_exit(1);
	    }
            @reflist = @{$opts{'reflist'}};
            @curr_sections = @{$opts{'curr_sections'}};
            %seen_extensions = %{$opts{'seen_extensions'}};
            %sections = %{$opts{'sections'}};
            %fixup_needed = %{$opts{'fixup_needed'}};

	# Check to see if the line is in the form of x=x=x=x: or some subset.
	# if so, then point the working reference pointer at that data.
	} elsif ((@vals) = $cfline =~
	    m/^([^\#=\s]+)(?:=([^=\s]+))?(?:=([^=\s]+))?(?:=([^=\s]+))?:\s*$/o) {
	    for (my $i = 0; $i < 4; $i++) {
		if (!defined($vals[$i]) || $vals[$i] eq '') {
		    $vals[$i] = [ 'default' ];
		    $sections{'default'}++;
		    next;
		}
		if ($vals[$i] =~ /:/o) {
		    Log(100, "':' is not allowed in a section name:\n '$vals[$i]' in '$cfline'\n");
		    $vals[$i] =~ s/://go;
		} elsif ($vals[$i] =~ /,/o) {
		    # Handle multiple values, and make sure they're unique
		    $vals[$i] = [ uniq(map { (defined($_) && ($_ ne '')) ? $_ : 'default' } split(/,/, $vals[$i])) ];
		} else {
		    $vals[$i] = [ $vals[$i] ];
		}
	    }
	    my ($bench, $tune, $ext, $mach) = @vals;
	    @reflist = ();
	    @curr_sections = ();
	    foreach my $tb (@$bench) {
		$sections{$tb}++;
		if (!grep { $tb eq $_ } @bmlist) {
                    next if $md5mode;
		    Log(100, "ERROR: Unknown benchmark \'$tb\' specified in $filename, line $cflinenum");
		    if (istrue($me->{'section_specifier_fatal'}) &&
                        !istrue($me->{'ignore_errors'})) {
			Log(100, "\n");
			do_exit(1);
		    } else {
			Log(100, "; ignoring\n");
			next;
		    }
		}
		foreach my $tt (@$tune) {
		    $sections{$tt}++;
		    if (!grep { $tt eq $_ } @tunelist) {
                        next if $md5mode;
			Log(100, "ERROR: Unknown tuning level \'$tt\' specified in $filename, line $cflinenum");
			if (istrue($me->{'section_specifier_fatal'})) {
			    Log(100, "\n");
			    do_exit(1);
			} else {
			    Log(100, "; ignoring\n");
			    next;
			}
		    }
		    foreach my $te (@$ext) {
                        $seen_extensions{$te}++;
			$sections{$te}++;
			foreach my $tm (@$mach) {
			    $sections{$tm}++;
			    $me->ref_tree(basename(__FILE__).':'.__LINE__,
					  [$tb],[$tt],[$te],[$tm]);
			    add_refs($me, $tb, $tt, $te, $tm);
			    push @reflist, $me->{$tb}{$tt}{$te}{$tm};
			    push @curr_sections, "${tb}=${tt}=${te}=${tm}";

			}
		    }
		}
	    }

        # Named pairs to current working reference
	} elsif (($name, $value) = $cfline =~ m/^\s*([A-Za-z0-9_]+)\s*=(.*)/) {

	    $variables{$name}++;

	    # Check for and remove nonvolatile config options if necessary
	    # Just in case there has been an oversight and a nonvolatile
	    # option has been inserted into the list of command line options,
	    # throw it out and issue a warning.
	    if (exists($::nonvolatile_config->{$name})) {
		Log(100, "$name is immutable.  Do not attempt to set it via config file.\n");
		$cfline = shift(@cflines);
                $cflinenum++;
		next;
	    }

            # Check for and remove options that must be set in the header
            # section.
            my $nonumname = $name;
            $nonumname =~ s/\d+$//;
            if ($reflist[0] != $me &&
                (exists($::header_only{$name}) ||
                 exists($::header_only{$nonumname}))) {
		Log(100, "\nWARNING: \"$name\" may be set only in the header section of the config file.\n");
                if (@curr_sections > 1) {
                    Log(100, "  Current sections are ".join("\n                       ", @curr_sections)."\n");
                } else {
                    Log(100, "  Current section is $curr_sections[0]\n");
                }
                Log(100, "Ignoring the setting on line $cflinenum of $filename.\n\n");
                sleep 3;
		$cfline = shift(@cflines);
                $cflinenum++;
		next;
            }

	    # Everything except notes should have leading whitespace removed
	    $value =~ s/^\s*//o unless ($name =~ /^notes/io);

	    # I've promised that ext, mach, iterations, and bind can be
            # explicitly multi-valued.  But (except for iterations and bind),
            # they can't be set within a benchmark section, and iterations
            # can't be multi-valued in that context.
	    if ($name =~ /^(ext|mach|iterations|output_root|expid)$/) {
		my $what = lc($1);
                if ($what =~ /^(ext|mach)$/ && $value !~ /^[A-Za-z0-9_., -]+$/) {
                    Log(100, "ERROR: Illegal characters in '$what'; please use only alphanumerics,\n");
                    Log(100, "       underscores (_), hyphens (-), and periods (.).\n");
                    ::do_exit(1);
                }
		if ($reflist[0] != $me) {
		    if ($what =~ /^(?:ext|mach|output_root|expid)$/) {
			Log(100, "ERROR: $what may only appear before the first section marker!\n");
                        ::do_exit(1);
		    } elsif ($what eq 'iterations' && $value =~ /,/) {
			Log(100, "Notice: per-benchmark iterations cannot be multi-valued! Ignoring setting.\n");
			$cfline = shift(@cflines);
		        $cflinenum++;
			next;
		    }
		}
	    }

	    # If it is the special case %undef% then remove the specified keys
	    # from the working reference and replace them with the special
            # %undef% tag to block down-tree references
	    if ($value =~ m/^\s*%undef%\s*$/io) {
		foreach my $ref (@reflist) {
		    for my $key (find_keys($ref, $name)) {
			if (exists($ref->{$key})) {
                          delete $ref->{$key};
                          $ref->{$key} = '%undef%';
                        }
		    }
		    $ref->{$name} = '%undef%';
		}
	    } elsif (!$md5mode &&
		     ($name eq 'optmd5' || $name eq 'exemd5' ||
		      $name eq 'compile_options' || $name eq 'baggage' ||
		      $name eq 'raw_compile_options')) {
		$cfline = shift(@cflines);
		$cflinenum++;
		next;
	    } elsif ($name eq 'rawtxtconfig' || $name eq 'rawtxtconfigall' ||
		     $name eq 'pptxtconfig' || $name =~ /^cfidx_/) {
		$cfline = shift(@cflines);
		$cflinenum++;
		next; # We don't allow people to overwrite the stored configs

	    } elsif ($name eq 'inherit_from') {
		# inherit_from is special because it's allowed to be
		# implicitly multi-valued.
		my @reftrees = ();
                # Strip whitespace from $value.  Section name components can't
                # have any, so it doesn't make sense for there to be any in
                # here, either.
                $value =~ s/\s+//g;
		foreach my $ancestor (split(/,+/, $value)) {
		    push @reftrees, $me->ref_tree('', map { [ (defined($_) && $_ ne '') ? $_ : 'default' ] } (split(/:/, $ancestor))[0..3]);
		}
		foreach my $ref (@reflist) {
		    if (exists($ref->{'inherit_from'}) &&
			ref($ref->{'inherit_from'}) eq 'ARRAY') {
			push @{$ref->{'inherit_from'}}, @reftrees;
		    } else {
			$ref->{'inherit_from'} = [ @reftrees ];
		    }
		}

	    } elsif ($name =~ /^hw_ncpu$/) {
		# hw_ncpu is special because it's likely to appear in
                # (old) config files, but we construct it from other
                # things, so it should be ignored.
                ::Log(0, "\nWARNING: hw_ncpu setting on line $cflinenum of\n");
                ::Log(0, "         $filename is ignored.\n");
                ::Log(0, "         Please set values individually using hw_nchips, hw_ncores,\n");
                ::Log(0, "         hw_ncoresperchip, and hw_nthreadspercore.\n\n");

	    } else {

                # In order to make things VASTLY simpler in Makefile.defaults,
                # fix up ONESTEP here.  The intent is to turn No, False, 0,
                # etc. into an empty value.
                if ($name =~ /ONESTEP$/) {
                    $value = istrue($value) ? 1 : '';
                }

		if ($value=~ s/^\s*<<\s*(\S+)\s*$//) {
		    $blockquote = $1;
		    $first_blockquote = 1;
		    $value = '';
		} elsif ($value=~ s/\\\\\s*$//) {
		    $appended  = 1;
                    # Trim this from the saved line as well.
                    foreach my $list (qw(pptxtconfig rawtxtconfig rawtxtconfigall)) {
                        $me->{$list}->[$#{$me->{$list}}] =~ s/\\\\\s*$//;
                    }
		} elsif ($value=~ s/\\\s*$//) {
		    $continued  = 1;
		}

                if ($name eq 'ext') {
                  # If it's ext, mark it down as 'seen'
                  $seen_extensions{$value}++;
                }

		@lastrefs = ();
		foreach my $ref (@reflist) {
                    
                    if ($name =~ /^(bind|srcalt)(\d*)$/) {
                        my ($base, $idx) = ($1, $2);
                        # Keep track of which refs need fixups, and
                        # also which line numbers the settings came
                        # from.  This is to allow "overwrite" warnings
                        # if necessary.
                        $name = " $base" if ($idx eq '');
                        $fixup_needed{$base}->{$ref}->{'ref'} = $ref;
                        push @{$fixup_needed{$base}->{$ref}->{'pairs'}}, [ $name, $filename, $cflinenum ];
                    }

                    $ref->{'copylist'} = [ split(/,+|\s+/, $value) ] if ($name eq 'copies');
                    if ($name eq 'submit') {
                      # This'll get fixed up later
                      $name = 'submit_default';
                    }
                    $ref->{$name} = $value;
                    if ($name =~ /$::info_re/) {
                        if (exists($ref->{'cfidx_'.$name})) {
                            ::Log(0, "WARNING: Duplicate setting for \"$name\" in file $filename, line $cflinenum\n");
                        }
                        $ref->{'cfidx_'.$name} = $#{$me->{'rawtxtconfig'}};
                    }
		    push @lastrefs, \$ref->{$name};
		}
	    }
	} elsif (!$appended && !$continued && !defined($blockquote) &&
                 $cfline !~ /^\s*$/) {
            ::Log(0, "Notice: Ignored non-comment line in file $filename:\n  line $cflinenum: \"$cfline\"\n");
        }
	$cfline = shift(@cflines);
	$cflinenum++;
	if (!$included && !defined($cfline) && !$sysinfo_run) {
	    # We're to the end of the file... is there a sysinfo program?
	    push @cflines, $me->get_sysinfo();
	    $cfline = "\n";
	    $sysinfo_run = 1;
	}
    }

    if ($included) {
	my $eos = "# ---- End inclusion of '$filename'";
	push @{$me->{'rawtxtconfigall'}}, $eos;
	push @{$me->{'rawtxtconfig'}}, $eos;
	push @{$me->{'pptxtconfig'}}, $eos;
    } else {
        # Fix up items that become lists.  This includes srcalt and bind.
        # The idea is to walk through the elements that WOULD contribute to
        # the single value.  In these cases, they can be set all at once (as in
        # "bind = 1 2 4 foo"), or one by one (as in "bind0 = 1; bind1 = 2").
        # The objective is to end up with one element per ref, which will be
        # an array, and with all of the "contributing" items and their cfidx
        # deleted.
        if (exists($fixup_needed{'srcalt'})) {
            # srcalt is special because it's allowed to be
            # implicitly or explicitly multi-valued.
            foreach my $section (keys %{$fixup_needed{'srcalt'}}) {
                # Go through each key that was added and apply it to the
                # final value.
                my $ref = $fixup_needed{'srcalt'}->{$section}->{'ref'};
                my $final = undef;
                $final = $ref->{'srcalt'} if isa($ref->{'srcalt'}, 'ARRAY');
                my %kill_keys = ();
                foreach my $pair (@{$fixup_needed{'srcalt'}->{$section}->{'pairs'}}) {
                    my ($name, $file, $line) = @{$pair};
                    my $value = $ref->{$name};
                    $kill_keys{$name} = 1;

                    my @srcalts = grep { defined && !/%undef%/i } split(/[\s,]+/, $value);
                    undef $final if ($value =~ /%undef%/i);
                    if (!defined($final) || !isa($final, 'ARRAY')) {
                        $final = [];
                    }
                    my %seensrcalts = map { $_ => 1 } @{$final};
                    foreach my $srcalt (@srcalts) {
                        next if $seensrcalts{$srcalt}++;
                        push @{$final}, $srcalt;
                    }
                }
                foreach my $key (keys %kill_keys) {
                    delete $ref->{$key};
                    delete $ref->{'cfidx_'.$key} if exists($ref->{'cfidx_'.$key});
                }
                $ref->{'srcalt'} = [ @{$final} ];   # Get a new ref
            }
        }

        # See the comments before the 'srcalt' block above to find out what's
        # going on here.
        if (exists($fixup_needed{'bind'})) {
            # bind is special because it's allowed to be
            # implicitly or explicitly multi-valued.
            my %change_count;
            foreach my $section (keys %{$fixup_needed{'bind'}}) {
                # Go through each key that was added and apply it to the
                # final value.
                my $ref = $fixup_needed{'bind'}->{$section}->{'ref'};
                my $final = undef;
                $final = $ref->{'bind'} if isa($ref->{'bind'}, 'ARRAY');
                my %kill_keys = ();
                foreach my $pair (@{$fixup_needed{'bind'}->{$section}->{'pairs'}}) {
                    my ($name, $file, $line) = @{$pair};
                    my $value = $ref->{$name};
                    $kill_keys{$name} = 1;
                    my $idx = undef;
                    if ($name =~ /^bind(\d+)/) {
                        $name = 'bind';
                        $idx = $1;
                    }
                    $change_count{$section}++;

                    if (!defined($idx) || $idx eq '') {
                        # Implicitly multi-valued
                        my @values = split(/[\n ,]+/, $value);
                        ::Log(0, "Notice: 'bind' setting from $file line $line will override previously set value\n") if ($change_count{$section} > 1);
                        $final = [ @values ];
                    } else {
                        $idx = $idx + 0;	# Make it a proper number
                        $final->[$idx] = $value;
                    }
                }
                foreach my $key (keys %kill_keys) {
                    delete $ref->{$key};
                    delete $ref->{'cfidx_'.$key} if exists($ref->{'cfidx_'.$key});
                }
                $ref->{'bind'} = [ @{$final} ];   # Get a new ref
            }
        }

	# Make sure all of the benchmark sections have 'refs'
	foreach my $tb (@bmlist) {
	    next unless (exists($me->{$tb}) && ref($me->{$tb}) eq 'HASH');
	    my $ref = $me->{$tb};
	    foreach my $tt (sort keys %{$ref}) {
		next unless (exists($ref->{$tt}) && ref($ref->{$tt}) eq 'HASH');
		$ref = $ref->{$tt};
		foreach my $te (sort keys %{$ref}) {
		    next unless (exists($ref->{$te}) && ref($ref->{$te}) eq 'HASH');
		    $ref = $ref->{$te};
		    foreach my $tm (sort keys %{$ref}) {
			next unless (exists($ref->{$tm}) && ref($ref->{$tm}) eq 'HASH');
			add_refs($me, $tb, $tt, $te, $tm);
		    }
		}
	    }
	}
#	$me->dumpconf('initial_config'); exit;
	$me->expand_vars();
#	$me->dumpconf('expanded_config'); exit;
    }

    # Check for conflicts between variable names (including nonvolatile
    # and default config) and section names.  First, the stuff we know about:
    my $conflicts = '';
    foreach my $section (sort keys %sections) {
	if (exists($::nonvolatile_config->{$section})) {
	    $conflicts .= " Section name '$section' (".pluralize($sections{$section}, 'occurence').")\n  conflicts with the name of a non-volatile variable.\n";
	} elsif (exists($::default_config->{$section})) {
	    $conflicts .= " Section name '$section' (".pluralize($sections{$section}, 'occurence').")\n  conflicts with the name of a default variable.\n";
	} elsif (exists($variables{$section})) {
	    $conflicts .= " Variable name '$section' (".pluralize($variables{$section}, 'occurence').")\n  conflicts with section name (".pluralize($sections{$section}, 'occurence').")\n";
	}
    }
    if ($conflicts ne '') {
	Log(100, "ERROR:  Variable/section name conflicts detected:\n\n");
	Log(100, $conflicts);
	do_exit(1);
    }

    if ($included) {
      # Put the opts back
      @{$opts{'reflist'}} = @reflist;
      @{$opts{'curr_sections'}} = @curr_sections;
      %{$opts{'seen_extensions'}} = %seen_extensions;
      %{$opts{'sections'}} = %sections;
      %{$opts{'fixup_needed'}} = %fixup_needed;
    } else {
        # Make the stored config texts back into strings
        foreach my $list (qw(pptxtconfig rawtxtconfig rawtxtconfigall)) {
            my $tmp = join("\n", @{$me->{$list}})."\n";
            $me->{$list} = $tmp;
        }

        # Store away the list of seen extentions
        $me->{'seen_extensions'} = \%seen_extensions;
    }

    $fh->close();
    1;
}

sub add_refs {
    my ($ref, $tb, $tt, $te, $tm) = @_;

    my @sets = $ref->benchmark_in_sets($tb);
    my $tmpref = $ref->{$tb}{$tt}{$te}{$tm};
    if (exists($tmpref->{'refs'})) {
#	print "add_refs: refs for $tb:$tt:$te:$tm already exist:\n";
#	dumpconf($tmpref);
	return;
    }

    # Set up refs so that variable interpolation can be
    # performed.  This value will be blown away after
    # variable expansion so that there's no danger of
    # using a local copy of refs to get back to the
    # data in global_config.
    $tmpref->{'ref_added'} = basename(__FILE__).':'.__LINE__;
    $tmpref->{'refs'} = [ $tmpref,
			  reverse ($ref,
				   $ref->ref_tree(basename(__FILE__).':'.__LINE__,
						  ['default', @sets, $tb],
						  ['default', $tt],
						  ['default', $te],
						  ['default', $tm])) ];
}

sub expand_vars {
    # This handles the first pass of runspec variable substitution (aka
    # "square bracket" substitution).  The work is done in config_var_expand().
    my ($me) = @_;

    return if ref($me) !~ /^(?:HASH|Spec::Config)$/;

    # At this point, all of the sections' refs fields are filled in... do
    # the promised expansion
    foreach my $member (sort keys %{$me}) {
	# There are some settings which should not have variable expansion
	# applied
	next if (exists($::nonvolatile_config->{$member}) ||
		 $member =~ /(?:txtconfig$\|^orig_|^ref|^bench(?:mark|set)s)/o);
	if (ref($me->{$member}) ne '') {
	    if (ref($me->{$member}) eq 'HASH') {
		expand_vars($me->{$member});
		delete ($me->{$member}->{'refs'});
	    }
	} else {
	    $me->{$member} = config_var_expand($me->{$member}, $me, 1);
	}
    }
}

sub eval_pp_conditional {
    my ($text, $macros, $file, $linenum) = @_;

    # Resolve the values of all the macros in the line.
    $text = pp_macro_expand($text, $macros, 0, 0, $file, $linenum);
    # In a construction like
    #    %if defined(%{foo})
    # where 'foo' is undefined, the resulting expression is
    #    defined()
    # which Perl very much does not like.  So fix it up, like this:
    $text =~ s/defined\(\)/defined(undef)/g;

    if ($text eq '') {
	# It could happen
	return 0;
    } else {
	my $compartment = new Safe;
	$compartment->permit_only(qw(:base_core :base_math));
	# What we're really after here are the Perl math and boolean operations
	# There shouldn't be anything like a variable in there at this point,
	# so (among other things) disallow variable references.
	$compartment->deny(qw(aelem aelemfast aslice av2arylen rv2av
			      helem hslice each values keys exists delete rv2hv
			      sassign aassign preinc i_preinc predec i_predec
			      postinc i_postinc postdec i_postdec trans splice push
			      pop shift unshift andassign orassign warn die anoncode
			      prototype setstate entersub leavesub leavesublv rv2cv
			      method method_named));
	$compartment->permit(qw(concat padany));
	# Keep the args safe
	$_ = undef;
	@_ = ();
	%_ = ();
	my $rc = $compartment->reval("return $text");
	if ($@) {
	    Log(100, "WARNING: Evaluation of expression from $file line $linenum failed.\nThe failed expression was\n  $text\n");
	    if ($@ =~ /trapped by operation mask (.*)/) {
		Log(100, "An illegal operation ($1) was attempted.\n");
		do_exit(1);
	    } else {
		Log(100, "The error message from the parser was: $@\n");
		do_exit(1);
	    }
	}
	# Try to figure out if $rc is true or false.
	if (!defined($rc) || ($rc eq '') || ($rc =~ /^[-+]?0+(?:\.0+)?$/)) {
	    return 0;
	} else {
	    return 1;
	}
    }
}

sub macro_defined {
    my ($macros, $what) = @_;

    return istrue(exists($macros->{$what}));
}

sub pp_macro_expand {
    my ($line, $macros, $warn, $quoting, $file, $linenum, $noreplace) = @_;

    do {
    } while
	$line =~ s/(?<!\\)%\{([^%\{\}]+)\}/
                   my $symbol = $1;
		   if (exists($macros->{$symbol})) {
		       # Well, then... try to figure out if it's a number or what
                       if ($macros->{$symbol}+0 != 0 &&
                           $macros->{$symbol} =~ m{^[-+]?(?:\d+|\d+\.\d*|\d*\.\d+|\d+[eEgG][-+]?\d+)$}) {
			   $macros->{$symbol}+0;
		       } elsif ($macros->{$symbol} =~ m{^[-+]?0+(?:\.0+)?$}) {
			   # It's a zero!
			   0;
		       } else {
			   # It's not a number...
			   if ($quoting) {
			       "'".$macros->{$symbol}."'";
			   } else {
			       $macros->{$symbol};
			   }
		       }
		   } else {
		       Log(100, "ERROR: Undefined preprocessor macro '$symbol' referenced at $file line $linenum\n") if $warn;
                       if ($noreplace) {
                          $1;
                       } else {
                          # Return an empty string; this should cause most
                          # expressions to break in such a way that the user
                          # will be alerted of their error. :)
                          '';
                       }
		   }
                            /egs;
    return $line;
}

sub config_var_expand {
    my ($line, $ref, $warn) = @_;

#    print "config_var_expand([$line], $ref, $warn)\n";
    return $line if ($line eq '' || ref($ref) !~ /^(?:HASH|Spec::Config)/);

    do {
#	print "line: {$line}\n";
    } while
	$line =~ s/(?<!\\)\$\[([^\$\[\]]+)\]/
                   my $var = $1;
#    print "    var: $var\n";
                   my $val = Spec::Config::accessor_backend($ref, 0, $var);
#    print "    val: $val\n";
                   if (defined($val)) {
		       # Well, then... try to figure out if it's a number or what
		       if ($val+0 != 0) {
			   # It's a number...numify it (well, this probably doesn't
			   # matter too much, but if it needs to be a string, it'll
			   # be re-stringified later)
			   $val+0;
		       } elsif ($val =~ m{^[-+]?0+(?:\.0+)?$}) {
			   # It's a zero!
			   0;
		       } else {
			   # It's not a number...
			   $val;
		       }
		   } else {
		       Log(100, "ERROR: Undefined config variable '$var' referenced during variable expansion\n") if $warn;
		       # Return an empty string; this should cause most expressions to
		       # break in such a way that the user will be alerted of their
		       # error. :)
		       '';
		   }
                            /egs;
#    print "return: {$line}\n";
    return $line;
}

sub get_sysinfo {
    my ($me) = @_;
    my @cflines = ();
    my @infolines = ();

    if (exists($me->{'sysinfo_program'}) &&
        $me->{'sysinfo_program'} ne '') {
        Log(0, "Running \"".$me->{'sysinfo_program'}."\" to gather system information.\n");
        if (open SYSINFO, '-|', split(/\s+/, $me->{'sysinfo_program'})) {
          @infolines = <SYSINFO>;
          Log(130, "Read ".(@infolines+0)." total lines from the sysinfo program.\n");
          # Only allow notes-type lines and comments
          @infolines = grep { /$::info_re/ || /^\s*\#/ } @infolines;
          Log(130, "Read ".(@infolines+0)." usable lines from the sysinfo program.\n");
        } else {
          Log(0, "\nERROR: Could not run sysinfo_program \"".$me->{'sysinfo_program'}."\".\n       The error returned was \"$!\"\n\n");
        }
        if (@infolines) {
	    push @cflines, "# The following settings were obtained by running ".$me->{'sysinfo_program'}."\n";
	    push @cflines, "default=default=default=default:\n";
	    push @cflines, @infolines;
        } else {
	    Log(130, "Read _NO_ lines from the sysinfo program.  Perhaps there was a problem?\n");
        }
    }
    return @cflines;
}

sub copies {
    my $me = shift;
    return 1 if !istrue($me->rate);
    my @check = qw(clcopies);
    push @check, 'copies' unless $me->tune eq 'base';
    foreach my $check (@check) {
        my $tmp = $me->accessor_nowarn($check);
        next unless defined($tmp) && $tmp ne '';
        return main::expand_ranges(split(/,+|\s+/, $tmp));
    }
    return main::expand_ranges(@{$me->copylist});
}

sub max_copies {
    my $me = shift;
    return 1 if !istrue($me->rate);
    return main::max(main::expand_ranges(@{$me->copylist}));
}

sub bytrailingnum {
    my ($anum) = $a =~ m/([0-9.]+)\s*$/;
    my ($bnum) = $b =~ m/([0-9.]+)\s*$/;
    my $rc =  $anum <=> $bnum;
    return $rc if $rc;
    return $a cmp $b;
}

# search keys of an array based on a limited wild card (*)
# and returns the list of keys that match/contain the pattern
sub find_keys {
    my ($me, $pat) = @_;
    my @temp;
    if ($pat =~ s/\*$//) { # if pattern ends in "*"
	@temp = sort bytrailingnum grep (m/^$pat[0-9.]*/, list_keys($me));
    } else {
	@temp = ($pat) if exists $me->{$pat};
    }
    return @temp;
}

# Return a list of sets that the benchmarks are in
sub benchmark_in_sets {
    my ($me, @benchmarks) = @_;
    my %sets;
    for my $bench (@benchmarks) {
	for my $set (keys %{$me->{'benchsets'}}) {
	    $sets{$set}++ if exists $me->{'benchsets'}{$set}{'benchmarks'}{$bench};
	}
    }
    return keys %sets;
}

sub list_keys {
    my ($me) = @_;
    my %seen;
    my @rc;
    my @refs = ();
    if (ref($me) eq 'HASH') {
	if (exists($me->{'refs'}) && ref($me->{'refs'}) eq 'ARRAY') {
	    @refs = @{$me->{'refs'}};
	} else {
	    @refs = ($me);
	}
    } else {
	@refs = (@{$me->refs});
    }

    for my $hash (@refs) {
	for my $key (keys %$hash) {
	    push (@rc, $key) if !$seen{$key}++;
	}
    }
    return sort @rc;
}

sub match_keys {
    my ($me, @pats) = @_;
    my @rc;
    for my $val (list_keys($me)) {
	for my $pat (@pats) {
	    if ($val =~ m/^${pat}[0-9.]*$/) {
		push (@rc, $val);
		next;
	    }
	}
    }
    return sort bytrailingnum @rc;
}

# Build a tree of references
sub ref_tree {
    my ($ref, $label, $leaves, @rest) = @_;
    my %seen;
    my @rc;
    return () unless ref($leaves) eq 'ARRAY';

    for my $leaf (@{$leaves}) {
	next unless defined($leaf);
	next if $seen{$leaf}++;
	$ref->{$leaf} = { } if ref($ref->{$leaf}) ne 'HASH';
	$refmapping{qq/$ref->{$leaf}/} = "$label:$leaf" if (!exists $refmapping{qq/$ref->{$leaf}/});
#print "ref_tree($ref, $label:$leaf, $leaves, [".join(',', @rest)."]) = ".$ref->{$leaf}."\n";
	push (@rc, $ref->{$leaf});
	push (@rc, ref_tree($ref->{$leaf}, "$label:$leaf", @rest)) if @rest > 0;
    }
    return @rc;
}

sub unshift_ref {
    my ($me, @refs) = @_;
    unshift (@{$me->{'refs'}}, @refs);
}
sub shift_ref {
    my ($me) = @_;
    my $out = shift @{$me->{'refs'}};
    return $out;
}
sub push_ref {
    my ($me, @refs) = @_;
    push (@{$me->{'refs'}}, @refs);
}
sub pop_ref {
    my ($me) = @_;
    my $out = pop @{$me->{'refs'}};
    return $out;
}

sub accessor_backend {
    my ($me, $warn, $what, @rest) = @_;
    my $old = undef;
    my @refs;
    my %seen = ();

    return undef unless exists ($me->{'refs'}) && ref($me->{'refs'}) eq 'ARRAY';

    @refs = @{$me->{'refs'}};
    while (defined(my $ref = shift @refs)) {
  	if (exists $ref->{$what}) {
  	    $old = $ref->{$what};
  	    last;
  	} elsif (exists ($ref->{'inherit_from'}) &&
  		 ref($ref->{'inherit_from'}) eq 'ARRAY') {
  	    # Look in places from which we're supposed to inherit settings, but
	    # _don't_ look in the same place twice; that could cause loops
  	    unshift @refs, grep { !exists($seen{$_}) } @{$ref->{'inherit_from'}};
  	}
  	$seen{$ref}++;
    }

    if (!defined($old)) {
	if ($warn) {
            # Believe it or not, but all of the code down to the Log(...) is
            # there just to find the most recent non-eval caller.
	    my ($pkg, $file, $ln, $subroutine, $hasargs,
		$wantarray, $evaltext, $is_require, $hints, $bitmask);
	    my $lvl = 3;
	    while (!defined($file) && $lvl >= 0) {
		($pkg, $file, $ln, $subroutine, $hasargs,
		 $wantarray, $evaltext, $is_require, $hints, $bitmask) = caller($lvl);
#		printf("pkg=%s\tfile=%s\nln=%d\tsubroutine=%s\thasargs=%s\twantarray=%s\nevaltext=%s\nis_require=%d\thints=%s\tbitmask=%08x\n----------\n",
#		       $pkg, $file, $ln, $subroutine, $hasargs,
#		       $wantarray, $evaltext, $is_require, $hints, $bitmask);
		$lvl--;
	    }
	    $file = File::Basename::basename($file) if (defined($file) && $file ne '');
	    Log(0, "WARNING: accessor '$what' not found; called from $pkg::$subroutine (call at $file line $ln) for object ".ref($me)."\n");
	    $DB::single=$DB::signal=1 if $warn;
	}
	return undef if !@rest;
    }

    # Return nothing if the key is not supposed to exist
    return undef if ($old eq '%undef%');

    if (@rest) {
	my $firstref = undef;
	if (ref($me->{'refs'}) eq 'ARRAY') {
	    $firstref = $me->{'refs'}->[0];
	}
	if (!defined($firstref)) {
	    $firstref = $me;
	}
	if (ref($old) eq 'ARRAY') {
	    $firstref->{$what} = [ @rest ];
	} elsif (ref($old) eq 'HASH') {
	    $firstref->{$what} = { @rest };
	} else {
	    $firstref->{$what} = $rest[0];
	    Log(0, "accessor '$what' passed too much data for scalar!\n")
		if @rest > 1;
	}
     }

    return $old;
}

sub accessor {
    my $me = shift;
    $me->accessor_backend(1, @_);
}
sub accessor_nowarn {
    my $me = shift;
    $me->accessor_backend(0, @_);
}

sub pluralize {
    my ($count, $word) = @_;

    my $rc = $count.' '.$word;
    $rc .= 's' if ($count != 1);
    return $rc;
}

# Automagically create new accessor functions for the class
AUTOLOAD {
    use vars qw($AUTOLOAD);
    my $name;
    my ($pkg,$func) = $AUTOLOAD =~ /(.*)::([^:]+)$/;
    if ($func eq 'DESTROY') {
	eval "package $pkg; sub $func {}";
    } else {
	eval "package $pkg; sub $func { shift->accessor('$func', \@_); }";
    }
    goto &$AUTOLOAD;
}

# Alias some main:: package functions into our namespace so we don't have to
# keep calling out the package
sub jp        { main::joinpaths(@_); }
sub istrue    { main::istrue(@_); }
sub Log       { main::Log(@_); }
sub uniq      { main::uniq(@_); }
sub deep_copy { main::deep_copy(@_); }
sub do_exit   { main::do_exit(@_); }
1;
