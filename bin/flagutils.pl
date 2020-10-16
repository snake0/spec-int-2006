#
# flagutils.pl
#
# Copyright (C) 2005-2006 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: flagutils.pl 4619 2006-07-18 21:41:42Z cloyce $

use strict;
use Safe;
use XML::SAX;
use CPUFlagsParser;
use UNIVERSAL qw(isa);
use LWP::UserAgent;
#use LWP::Debug qw(+);

require 'util.pl';

use vars qw($ua);

my %seen_ids = ();

# Master list of all available classes; this must match the DTD for things to
# make sense.
my @classlist = qw(mandatory forbidden
                   portability optimization
                   compiler other
                   unknown);

my $version = '$LastChangedRevision: 4619 $ '; # Make emacs happier
$version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'flagutils.pl'} = $version;

sub update_flags_file {
    my ($config, $bm) = @_;
    my ($bmname) = ($bm =~ /^\d+\.(.*)/);
    $bmname = $bm if $bmname eq '';

    if ($bm ne $::lcsuite && $bm !~ /\.syntax$/) {
      if (!exists $config->{'benchmarks'}->{$bm}) {
          Log(0, "\nERROR: Cannot update flags file for nonexistant benchmark $bm\n");
          return undef;
      }
      if (!isa($config->{'benchmarks'}->{$bm}, "Spec::Benchmark::$bmname")) {
          Log(0, "\nERROR: $bm (".$config->{'benchmarks'}->{$bm}.")is not a Spec::Benchmark::$bmname\n");
          return undef;
      }
    }

    my $flagsfile;
    my $url;
    if ($bm !~ m/^$::lcsuite/) {
        $flagsfile = jp($config->{'benchmarks'}->{$bm}->path, 'Spec', 'flags.xml');
        $flagsfile =~ s#^${ENV{'SPEC'}}[/\\]##;
        $url = $::global_config->{'flag_url_base'}.$bm.'.flags.xml';
    } elsif ($bm eq $::lcsuite) {
        $flagsfile = jp('benchspec', 'flags-mandatory.xml');
        $url = $::global_config->{'flag_url_base'}.$bm.'.flags.xml';
    } elsif ($bm eq $::lcsuite.'.syntax') {
        $flagsfile = jp('bin', 'formats', $::lcsuite.'.syntax');
        $url = $::global_config->{'flag_url_base'}.$bm;
    }

    # $ua is already prepped by get_flag_md5s
    if ($bm =~ /syntax$/) {
      Log(0, "Updating syntax file for $::lcsuite\n");
    } else {
      Log(0, "Updating flags file for $bm\n");
    }
    my $res = $ua->get($url);
    if ($res->is_success) {
        my $flags = $res->content;
        my $ofh = new IO::File '>'.jp($ENV{'SPEC'}, $flagsfile);
        if (!defined($ofh)) {
            Log(0, "\nERROR: Could not open $flagsfile for writing: $!\n");
        } else {
            $ofh->print($flags);
            $ofh->close();
            return $flagsfile;
        }
    } else {
        Log(0, "\nNOTICE: Could not retrieve $url; got\n  ".$res->status_line."\n\n");
    }

    return undef;
}

sub update_flags {
    # Get new flag descriptions and syntax file from SPEC (if different
    # ones are available)
    my ($config, $timeout, $proxy) = @_;
    my %flag_md5s = get_flag_md5s($timeout, $proxy);
    my @got_files;

    if (!%flag_md5s) {
        Log(0, "\nERROR: Got no flag checksums from SPEC!\n");
        return undef;
    }

    # One nice thing is that $ua is all set up now. :)

    # Go through the benchmarks and see if the flags need updating
    foreach my $bm (sort keys %{$config->{'benchmarks'}}) {
        Log(7, "Checking for flag updates for $bm\n");
        if (!exists $flag_md5s{$bm}) {
            Log(0, "NOTICE: SPEC has no flags file for $bm!\n");
            next;
        }
        if ($flag_md5s{$bm} ne $config->{'flaginfo'}->{$bm}->{'md5'}) {
            Log(7, "Flag MD5 mismatch for $bm:\n    $flag_md5s{$bm}\n    ".
                $config->{'flaginfo'}->{$bm}->{'md5'}."\n");
            my $file = update_flags_file($config, $bm);
            push @got_files, $file if (defined($file));
        }
    }

    # Check the suite flags
    if (!exists $flag_md5s{$::lcsuite}) {
        Log(0, "NOTICE: SPEC has no flags file for $::suite!  That's not right...\n");
    } else {
        Log(7, "Checking for suite-wide flag updates for $::suite\n");
        if ($flag_md5s{$::lcsuite} ne $::global_config->{'flaginfo'}->{'suite'}->{'md5'}) {
            Log(7, "Flag MD5 mismatch for $::suite:\n    $flag_md5s{$::lcsuite}\n    ".
                $::global_config->{'flaginfo'}->{'suite'}->{'md5'}."\n");
            my $file = update_flags_file($config, $::lcsuite);
            push @got_files, $file if (defined($file));
        }
    }

    # Check the syntax file, but don't require it to be present
    if (exists $flag_md5s{$::lcsuite.'.syntax'}) {
        Log(7, "Checking for syntax file updates for $::suite\n");
        my $newmd5 = $flag_md5s{$::lcsuite.'.syntax'};
        my $oldmd5 = $::file_md5{jp($ENV{'SPEC'}, 'bin', 'formats', $::lcsuite.'.syntax')};
        if ($newmd5 ne $oldmd5) {
            Log(7, "Syntax file MD5 mismatch:\n    $newmd5\n    $oldmd5\n");
            my $file = update_flags_file($config, $::lcsuite.'.syntax');
            push @got_files, $file if (defined($file));
        }
    }

    if (@got_files) {
        Log(7, "Updating MANIFEST with new flag file sums\n");
        update_manifest(@got_files);
    }

    return 1;
}

sub get_flag_md5s {
    # Phone home (to SPEC) to see if the flag descriptions there are different.
    my ($timeout, $proxy) = @_;
    my ($ver, $date);
    my %flag_md5 = ();
    my $url = $::global_config->{'flag_url_base'}.'sums';
    $timeout ||= 30;

    $ua = new_user_agent() unless defined($ua);
    $ua->timeout($timeout);
    $ua->proxy(['http', 'ftp'], $proxy) if (defined($proxy) && $proxy ne '');
    my $res = $ua->get($url);
    if ($res->is_success) {
        foreach my $line (split(/[\r\n]+/, $res->content)) {
            my ($md5, $bm) = split(/\s+/, $line);
            $flag_md5{$bm} = $md5;
        }
    } else {
        Log(0, "\nNOTICE: Could not retrieve flag file checksums; got\n  ".$res->status_line."\n\n");
    }

    return %flag_md5;
}

sub get_flags_file {
    my ($url, $source, $standalone, $timeout, $proxy) = @_;
    my $isuser = ($source eq 'user');

    if (!defined($url) || $url eq '') {
      Log(0, "\nERROR(get_flags_file): Flags file URL is empty!\n");
      return(undef, undef);
    }
    if ($url eq 'noflags') {
      return('', {});
    }

    $timeout ||= 30;

    my $flags = '';
    $ua = new_user_agent() unless defined($ua);
    $ua->timeout($timeout);
    $ua->proxy(['http', 'ftp'], $proxy) if (defined($proxy) && $proxy ne '');
    my $flagsstring = '';
    if ($url !~ m|^[^:]+://|) {
	# Bare path; LWP not needed
	my $ifh = new IO::File '<'.$url;
        if (!defined($ifh)) {
          Log(0, "\nERROR: The specified flags file ($url) could not\n       be opened for reading: $!\n");
          return(undef, undef);
        } else {
          my $oldeol = $/;
          undef $/;
          $flagsstring = <$ifh>;
          $/ = $oldeol;
        }
    } elsif (defined($ua)) {
	if ($url !~ /^(http|ftp|file):/) {
	    Log(0, "\nERROR: Unsupported flags file URL scheme; please use file:, http:, or ftp:.\n");
	    Log(0, "The URL specified was $url\n");
	} else {
	    my $res = $ua->get($url);
	    if ($res->is_success) {
		$flagsstring = $res->content;
	    } else {
		Log(0, "\nERROR: Specified flags URL ($url) could not be retrieved.\n");
		Log(0, "       The error returned was: ".$res->status_line."\n");
                return(undef, undef);
	    }
	}
    } else {
	# A URL was specified, but LWP is not available.
	Log(0, "\nERROR: A flags file URL was specified, but LWP is not available to\n");
	Log(0, "       retrieve it.  Without LWP, only normal files can be specified for\n");
	Log(0, "       a flags URL.\n");
        return(undef, undef);
    }

    # Make sure that the flags file is actually XML
    if ($flagsstring =~ /<flagsdescription>/) {
        $flags = join("\n", split(/(?:\n\r|\n|\r\n|\r)/, $flagsstring, -1));
        return ($flags, parse_flags($flags, $url, $source, $standalone, 'forbid' => ($isuser) ? [ 'mandatory' ] : []));
    } elsif ($flagsstring ne '') {
	Log(0, "\nERROR: A flags file was specified and read, but does not seem to contain\n");
	Log(0, "       valid flag description XML.  The file's content will be ignored.\n");
        return (undef, undef);
    }

}

sub parse_flags {
    # Given a hunk of XML, parse it into the structure that the rest
    # of the tools expect.
    #
    # Options:
    # forbid:   An array ref containing a list of classes not allowed to
    #           appear in the structure.
    my ($flags, $url, $source, $standalone, %opts) = @_;
    my $flagref = undef;
    my $flagmd5 = Digest::MD5::md5_hex($flags);
    %seen_ids = ();     # Flag names need only be unique within a file

    my $flaghandler = new CPUFlagsParser;
    return undef unless defined($flaghandler);

    my $flagparser = XML::SAX::ParserFactory->parser(
                        Handler => $flaghandler
                     );

    if (defined($flagparser)) {
        # Convert the XML into a hash
        eval { $flagparser->parse_string($flags); };
        if ($@) {
            my $err = $@;
            $err =~ s/at \S+flagutils.pl line \d+//gs;
            Log(0, "\nERROR: An error was encountered while parsing the flag description file\n");
            Log(0, "        at $url\n") if ($url ne '');
            Log(0, "       The error message returned by the XML parser was\n");
            Log(0, "$err\n");
            return undef;
        }
        $flagref = $flaghandler->get_flag_ref();
        if (!isa($flagref, 'HASH')) {
            Log(0, "\nERROR: The XML parser did not return the expected data structure type.\n");
            undef $flagref;
        } else {
            # Add the MD5 hash of the flags to the data structure; it will
            # be used by the updater.
            $flagref->{'md5'} = $flagmd5;

            # Check for forbidden classes (if any)
            if (exists($opts{'forbid'}) && isa($opts{'forbid'}, 'ARRAY') &&
                @{$opts{'forbid'}}) {
              # Grab all the flag classes from all the flags
              my %seenclasses = map { $_->{'class'} => 1 } @{$flagref->{'flag'}};
              my $found = 0;
              foreach my $badclass (@{$opts{'forbid'}}) {
                if (exists $seenclasses{$badclass}) {
                  Log(0, "\nERROR: The flag file at\n");
                  Log(0, "       $url\n");
                  Log(0, "     contains one or more flags in the class \"$badclass\", which is not\n");
                  Log(0, "     allowed.\n");
                  $found++;
                }
              }
              do_exit(1) if $found;
            }

            # Check the flags for unknown attributes, etc.
            my $error = 0;
            my $flags = $flagref->{'flag'};
	    foreach my $flag (@{$flags}) {
		my $tmpname = $flag->{'name'};
                if (!exists($flag->{'name'})) {
                    Log(0, "\nERROR: While parsing flags file");
                    Log(0, " at $url") if ($url ne '');
                    Log(0, ":\n");
		    if (exists($flag->{'regexp'})) {
			Log(0, "       Flag with regular expression '".$flag->{'regexp'}."' has no name!\n");
		    } elsif (exists($flag->{'description'})) {
			Log(0, "       Flag with description '".$flag->{'description'}."' has no name!\n");
		    } else {
			Log(0, "       Flag with no name, regexp, or description found!\n");
		    }
                    $error++;
                } else {
		    # If no example is supplied, copy the unmunged name to
		    # example.
		    if (!exists($flag->{'example'})) {
                        $flag->{'example'} = $flag->{'name'};
                        $flag->{'example'} =~ s/^D/-D/;
                        $flag->{'example'} =~ s/^F//i;
                        $flag->{'example'} =~ s/:/=/;
		    }

                    # Strip leading and trailing whitespace from the
                    # description, encode anything that might make an
                    # HTML validator unhappy (like ampersands), and surround
                    # it with <p>..</p> if it looks like there is _no_ markup
                    # present.
                    $flag->{'description'} =~ s/^\s+//s;
                    $flag->{'description'} =~ s/\s+$//s;
                    if ($flag->{'description'} !~ /[<>]/) {
                      $flag->{'description'} = "<p>\n".::escape_HTML($flag->{'description'})."\n</p>";
                    }

                    # If the XML has been validated, this error can't happen
		    if ($seen_ids{$flag->{'name'}}) {
			Log(0, "\nERROR: While parsing flags file");
			Log(0, " at $url") if ($url ne '');
			Log(0, ":\n");
			my $tmpcnt = $seen_ids{$flag->{'name'}};
			Log(0, "       The flag name '$tmpname' is not unique! (".pluralize($tmpcnt + 1, 'occurence').")\n");
			Log(0, "       The post-conversion name is \"".$flag->{'name'}."\".\n");
			Log(0, "       Perhaps they differ only in illegal characters?\n");
			Log(0, "       (See section 3.1 of flag-description.html)\n");

			$error++;
		    }
                    $seen_ids{$flag->{'name'}}++;
                }
                foreach my $inctype (qw(inc_flag inc_text)) {
                    if (exists($flag->{$inctype})
                        && !isa($flag->{$inctype}, 'ARRAY')) {
                        Log(0, "\nERROR: While parsing flags file");
                        Log(0, " at $url") if ($url ne '');
                        Log(0, ":\n");
                        Log(0, "       Includes for flag named '".$flag->{'name'}."' is not an ARRAY\n");

                        $error++;
                    }
                }

                # If the flags file is valid, this error can not occur
                if (isa($flag->{'inc_flag'}, 'ARRAY')) {
                    # Check that the referenced flags have descriptions
                    foreach my $incflagref (@{$flag->{'inc_flag'}}) {
                        if (!isa($incflagref, 'ARRAY')) {
                          Log(0, "\nERROR: While parsing flags file");
                          Log(0, " at $url") if ($url ne '');
                          Log(0, ":\n");
                          Log(0, "       Flag included by name ($incflagref?) for flag named '".$flag->{'name'}."' is not an ARRAY\n");

                          $error++;
                          next;
                        }
                        my ($incflag, $flagtext) = @{$incflagref};
                        if (!exists($flagref->{'flagmap'}->{$incflag})) {
                            Log(0, "\nERROR: While parsing flags file");
                            Log(0, " at $url") if ($url ne '');
                            Log(0, ":\n");
                            Log(0, "       Flag \"$incflag\" referenced by '".$flag->{'name'}."' has no description!\n");

                            $error++;
                        }
                    }
                }

		# Make the default regexp, if necessary
		if (!exists($flag->{'regexp'})) {
		    my $regexp = $tmpname;
                    # See flag-description.html#sect_3.3.1

                    # Step 1-2 (Fix up the initial portion)
                    if ($regexp !~ s/^F-/-/) {
                      if ($regexp !~ s/^f-/[-\/](?i)/) {
                        $regexp = '[-/]'.$regexp;
                      }
                    }

                    # Step 3a (Deal with the = problem)
                    $regexp =~ s/:/=/;

		    # Step 3b (add the match-value stuff)
                    if ($regexp !~ /=/) {
                      $regexp .= '(?:=\S*)?';
                    }

		    # Step 4 (add the trailing delimiter)
                    if ($tmpname =~ /\w$/) {
                      # It ends in a word character, so just use \b
                      $regexp .= '\b';
                    } else {
                      # Use the more complex (but correct) lookahead
                      $regexp .= '(?=\s|$)';
                    }

		    $flag->{'regexp'} = $regexp;
                    Log(95, "Created default regexp '$flag->{'regexp'}' for $flag->{'name'} in $url\n");
		}

                # Now that the regexp is guaranteed to exist, make sure that
                # it's not nothing!
                if ($flag->{'regexp'} eq '') {
                  Log(0, "\nERROR: While parsing flags file");
                  Log(0, " at $url") if ($url ne '');
                  Log(0, ":\n");
                  Log(0, "       Flag \"$tmpname\" has an empty regexp!\n");
                  $error++;
                }

                foreach my $req_attr (qw(class description)) {
                    if (!exists($flag->{$req_attr})) {
                        Log(0, "\nERROR: While parsing flags file");
                        Log(0, " at $url") if ($url ne '');
                        Log(0, ":\n");
                        Log(0, "       Flag named '".$flag->{'name'}."' has no $req_attr attribute!\n");
                        $error++;
                    } elsif ($flag->{$req_attr} eq '') {
                        Log(0, "\nERROR: While parsing flags file");
                        Log(0, " at $url") if ($url ne '');
                        Log(0, ":\n");
                        Log(0, "       Flag named '".$flag->{'name'}."' has an empty $req_attr attribute!\n");
                        $error++;
                    }
                }

                # Make sure that any compiler restrictions refer to actual
                # defined compilers.
                if (exists($flag->{'compilers'})) {
                  if ($flag->{'class'} eq 'compiler') {
                        Log(0, "\nERROR: While parsing flags file");
                        Log(0, " at $url") if ($url ne '');
                        Log(0, ":\n");
                        Log(0, "       The compiler flag named '".$flag->{'name'}."' has a 'compilers' attribute.\n");
                        Log(0, "       This will cause the flag to never match!\n");
                        Log(0, "       Flags in the 'compiler' class must not have compiler restrictions.\n");
                        $error++;
                  }

                  foreach my $compiler (split(/[,\s]+/, $flag->{'compilers'})) {
                    next if $compiler eq 'specpp';      # Implicit
                    if (!exists($flagref->{'flagmap'}->{$compiler})) {
                      Log(0, "\nERROR: While parsing flags file");
                      Log(0, " at $url") if ($url ne '');
                      Log(0, ":\n");
                      Log(0, "       Compiler restriction for \"".$flag->{'name'}."\" references\n");
                      Log(0, "         unknown compiler '$compiler'!\n");
                      $error++;
                    }
                    if (exists($flagref->{'flagmap'}->{$compiler}) &&
                        $flagref->{'flagmap'}->{$compiler}->{'class'} ne 'compiler') {
                      Log(0, "\nERROR: While parsing flags file");
                      Log(0, " at $url") if ($url ne '');
                      Log(0, ":\n");
                      Log(0, "       Compiler restriction for \"".$flag->{'name'}."\" references\n");
                      Log(0, "         flag '$compiler', which is not in the 'compiler' class!\n");
                      $error++;
                    }
                  }
                }

                my @unknown_attrs = grep { !/^(?:includes|addflag|name|regexp|description|class|compilers|precedence|example|ex_replacement|inc_flag|inc_text|display)$/o } keys %{$flag};
                if (@unknown_attrs) {
                    Log(0, "\nWARNING: While parsing flags file");
                    Log(0, " at $url") if ($url ne '');
                    Log(0, ":\n");
                    Log(0, "         Flag named '".$flag->{'name'}."' has unknown attributes:\n");
                    Log(0, "             ".join(', ', @unknown_attrs)."\n");
                }
		# Add the origin of the flag, converting backslashes to slashes
		$url =~ tr#\\#/#;
                $flag->{'origin'} = [ $url, $source ];
            }
            if ($error) {
                undef $flagref;
            }
        }
    }

    return $flagref;
}

sub flags_list {
    # Build a data structure detailing what's known about all the various
    # flags used for a particular benchmark.  The results are stored into
    # the benchmark's configuration area.
    my ($config, $benchopt, $bench, $tune, @flagkeys) = @_;

    return undef unless isa($config->{'flaginfo'}, 'HASH');

    my $s = new Safe 'retmp';
    $s->deny_only('entereval');  # Deny eval-based regexp stuff

    # Get the option lines into an array, being careful to preserve their
    # order.
    ${$s->varglob('opts')}      = [ split(/\n+/, $benchopt) ];
    ${$s->varglob('flagkeys')}  = [ @flagkeys ];
    ${$s->varglob('benchmarks')}= $config->benchmarks;
    ${$s->varglob('flaginfo')}  = $config->{'flaginfo'};
    ${$s->varglob('flaglist')}  = { 'passes' => [] };
    ${$s->varglob('bench')}     = $bench;
    ${$s->varglob('tune')}      = $tune;
    $s->share_from('main', [ 'check_elem', 'isa', 'Log' ]);
    $s->share('do_replacements');
    $s->share('deep_copy');
    $s->share('add_implicit_ref');
    $s->share('find_benchmark');
    my ($opts, $flaglist) = $s->reval(q% 
#-----------------------------------------------------------------------------
    sub kill_flag {
      my ($flaglist, $inforef, $name) = @_;
      my $i = 0;
      for(; $i < @$inforef && $inforef->[$i]->{'name'} ne $name; $i++) {
        # Whee!
      }
      splice @$inforef, $i, 1;
      $flaglist->{'seen_errors'}++;
    }

    my $prev_match = {};
    $flaglist->{'seen_errors'} = 0;

    my $tmpcnt = 0;
    Log(98, "\nOption strings for $bench $tune:\n  ".join("\n  ", map { $tmpcnt++.": $_" } @{$opts})."\n" );

    my $matches = {};
    my @currcompiler = ();
    # Look for options
    foreach my $infokey (@{$flagkeys}) {
	next unless check_elem('ARRAY', $flaginfo, $infokey, 'flag');
	my $inforef = $flaginfo->{$infokey}->{'flag'};

        my $not_done_yet = 1;
        while ($not_done_yet) {
          # This just allows the matching code to ask for a re-run of the
          # current flag list, if some new flag text was inserted.
          $not_done_yet = 0;
          my $count = 0;

          # Search the compilers first (regardless of their interleaving order)
          # and other flags next.
          foreach my $currflag ((grep { $_->{'class'} eq 'compiler' } @{$inforef}),
                                (grep { $_->{'class'} ne 'compiler' } @{$inforef})) {
              my $flagdesc = $currflag->{'name'};
              $count++;
              my $re = $currflag->{'regexp'};
              my @compilers = split(/[\s+,]+/, $currflag->{'compilers'});

              if (!defined($re)) {
                  # This won't happen, since we check for it while
                  # parsing the flags files.
                  Log(0, "\nERROR: Flag \"$flagdesc\" has no regular expression!\n");
                  next;
              }

              my $mode = undef;
              my $currcompiler = undef;
              my @passes = @{$flaglist->{'passes'}};
              for(my $i = 0; $i <= $#{$opts}; $i++) {
                # Skip LINK and COMP lines
                if ($opts->[$i] =~ /^COMP\d*:/) {
                  $mode = 'comp';
                  $currcompiler = undef if $currcompiler eq 'specpp';
                  next;
                } elsif ($opts->[$i] =~ /^FPP\d*:/) {
                  $mode = 'comp';
                  $currcompiler = $currcompiler[$i] = 'specpp';
                  next;
                } elsif ($opts->[$i] =~ /^LINK\d*:/) {
                  $mode = 'link';
                  next;
                } elsif ($opts->[$i] =~ /^ONESTEP(\d*):/) {
                  my $pass = $1;
                  if (!exists($prev_match->{'O::ONESTEP:ONESTEP'})) {
                    my $flagref =
                      [ [ '' ],            # Variables it was found in
                        'ONESTEP',         # Flag text
                        {                  # Flag description entry
                          'name' => 'ONESTEP',
                          'regexp' => 'ONESTEP\b',# keep one_match happy
                          'nomap' => 1,           # Don't try to link to it
                          'display' => 1,         # Do display it
                          'found_in' => 'O',      # A lie of convenience  
                          'origin' => [ undef, 'suite' ] # It came from the suite
                        },
                        [ 'comp', $pass ],
                        [ 'link', $pass ]
                      ];
                    # ONESTEP should always be the first flag
                    unshift @{$flaglist->{'optimization'}}, $flagref;
                    $prev_match->{'O::ONESTEP:ONESTEP'} = $flagref;
                  } else {
                    # When ONESTEP is first, it saves a lot of time looking
                    # for it. :)
                    push @{$flaglist->{'optimization'}->[0]}, [ 'comp', $pass ], [ 'link', $pass ];
                  }
                  $mode = 'both';
                  next;
                }
                
                # If the current compiler is set for this line, it should be
                # used for matching subsequent lines, until another compiler
                # is seen.
                if (defined($currcompiler[$i])) {
                  $currcompiler = $currcompiler[$i];
                }

                # Skip empty lines.  This is done here so that even when
                # compiler lines are gone, their settings can still be used.
                next unless defined($opts->[$i]);

                # If the current flag specifies a compiler list, make sure
                # that it ONLY matches flags when that compiler has been
                # seen.
                next if (
                         @compilers
                         && (   (
                                 defined($currcompiler)
                                 && $currcompiler ne ''
                                 && !grep { $_ eq $currcompiler } @compilers
                                 )
                             || (!defined($currcompiler)
                                 || $currcompiler eq ''
                                )
                             )
                        );

                # Go ahead and decompose the option line into the type,
                # the variable name, and the contents.
                my ($type, $pass, $var, $thing) = ($opts->[$i] =~ /^([A-Z]+)(\d*): ([^=]+)=\"(.*)\"$/);
                push @passes, $pass if defined($pass) && $pass ne '' && !grep { $pass == $_ } @passes;
                my @usedin = ( [ $mode, $pass ] );
                if ($mode eq 'both') {  
                  # Cursed ONESTEP!
                  @usedin = ( [ 'comp', $pass ], [ 'link', $pass ]);
                }

                next unless (defined($thing) && $thing ne '');
                if (!defined($type) ||
                    $type !~ /^[CPO]$/) {
                    Log(0, "\nERROR: Unknown variable type \"$type\" found.  Skipping option line.\n");
                    next;
                }
                if (!defined($var) || $var eq '') {
                    Log(0, "\nERROR: Variable name is empty.  Skipping option line.\n");
                    next;
                }

                # Find a marker to use.
                my $marker = '';
                Log(99, "Looking for marker to use in \"$thing\"; starting at YxY${marker}XyX\n");
                while ($thing =~ /YxY${marker}XyX/ && $marker - $$ < 1000) {
                  $marker++;
                }
                if ($marker >= 1000) {
                  Log(0, "ERROR: Could not find unique marker to use in \"$thing\"\n");
                  last;     # $flag is already undef
                }
                $marker = "YxY${marker}XyX";
                Log(99, "Chose marker \"$marker\"\n");

                my ($flag, @replacements) = (1);
                while (defined($re) && defined($flag)) {
                    # This is the same thing as below in one_match.  If I
                    # could get it inlined, I would, because when I break
                    # it out into a sub there's a noticeable performance
                    # degradation (here, not there).
                    $flag = undef;
                    @replacements = ();

                    if ($thing =~ s/($re)/$marker/) {
                        no strict 'refs';
                        $flag = $1;
                        # Loop to add all captured groups
                        # to the list of values to return
                        for(my $i = 2; $i <= $#+; $i++) {
                            push @replacements, ${$i};
                        }
                    }
                    last unless defined($flag); # No match => no marker
                    Log(97, "Flag named \"$flagdesc\" matched: \"$flag\" in $type:$var (line $i; $mode pass $pass)");
                    Log(97, " (replacements: '".join("', '", @replacements)."')") if (@replacements);
                    Log(97, "\n");

                    # NOT GOOD if the matched bit is empty
                    if ($flag eq '') {
                      Log(0, "\n\nERROR: The regexp for \"$flagdesc\" matched, but matched no text.  This\n");
                      Log(0, "      can cause infinite loops, so \"$flagdesc\" will be removed.\n");
                      Log(0, "      Reformatting the results with a fixed flag description file should\n");
                      Log(0, "      result in readable results.\n\n");
                      # It's important to delete this flag if the set will
                      # be re-run.  So...
                      kill_flag($flaglist, $inforef, $flagdesc);
                      undef $re;
                      # Remove the marker
                      $thing =~ s/$marker//;
                      last;
                    }

                    if ($currflag->{'class'} eq 'compiler') {
                        $currcompiler[$i] = $currflag->{'name'};
                    }

                    # Handle included text, if any.
                    # This is done even if the flag has matched before so that
                    # split-out flags will still have their used-in information
                    # saved properly.
                    if (exists($currflag->{'inc_text'})
                        && isa($currflag->{'inc_text'}, 'ARRAY')) {
                        Log(95, "Flag text substitution: pre-sub: \"$thing\"\n");
                        my $subcount = 1;
                        foreach my $repl (@{$currflag->{'inc_text'}}) {
                          my $newthing = do_replacements($repl, @replacements);
                          $thing =~ s/$marker/$newthing $marker/;
                          Log(95, "Flag text substitution: repl $subcount (\"$repl\"): \"$thing\"\n");
                          $subcount++;
                        }
                        Log(95, "Flag text substitution: post-sub: \"$thing\"\n");
                        $not_done_yet++;
                    }

                    # Remove the marker, as it's no longer useful
                    $thing =~ s/$marker//;

                    # Don't store the matched flag if this rule has
                    # matched the same thing on a previous instance of
                    # this line.
                    # This can often happen when using non-language-
                    # specific flags like OPTIMIZE, which will show up
                    # in both compilation and link sections.
                    if (exists($prev_match->{"$type:$var:$flag:".$currflag->{'name'}})
                        && $prev_match->{"$type:$var:$flag:".$currflag->{'name'}}) {
                        Log(97, "  *** Avoiding duplication of flag match ($mode pass $pass)\n");
                        push @{$prev_match->{"$type:$var:$flag:".$currflag->{'name'}}}, @usedin;
                        next;
                    }


                    # Keep track of how many times this regexp matches
                    $matches->{$flagdesc}++;
                    if ($matches->{$flagdesc} >= 100) {
                        # There shouldn't be 100 flags for a particular
                        # benchmark, let alone 100 matches, so...
                        Log(0, "\n\nERROR: Flag \"$flagdesc\" has matched more than 100 times; it will be\n");
                        Log(0, "      removed, and the currently generated results will be bogus.\n");
                        Log(0, "      Reformatting the results with a fixed flag description file should\n");
                        Log(0, "      result in readable results.\n\n");
                        # It's important to delete this flag if the set will
                        # be re-run.  So...
                        kill_flag($flaglist, $inforef, $flagdesc);
                        undef $re;
                        last;
                    }

                    # Make a copy of the flag description, and fix up the
                    # description.
                    my $flagdesc_copy = deep_copy($currflag);
                    $flagdesc_copy->{'description'} = do_replacements($flagdesc_copy->{'description'}, @replacements);
                    $flagdesc_copy->{'found_in'} = $type;
                    $flagdesc_copy->{'compiler_used'} = $currcompiler;

                    # List it
                    my $flagref = [ [ $var ], $flag, $flagdesc_copy, @usedin ];
                    push @{$flaglist->{$currflag->{'class'}}}, $flagref;
                    $flaglist->{'seenflags'}->{$currflag}->{'flag'} = $flagref;
                    $prev_match->{"$type:$var:$flag:".$currflag->{'name'}} = $flagref;

                    if ($var eq 'LD' &&
                        isa($flaglist->{'optimization'}->[0], 'ARRAY') &&
                        $flaglist->{'optimization'}->[0]->[1] eq 'ONESTEP') {
                      # Cursed ONESTEP!  Make another copy with LD replaced
                      # by the proper language compiler.
                      # This will allow the compiler invocation to be merged
                      # if both base and peak are run, and only one uses
                      # ONESTEP.
                      my $fakedflag = deep_copy($flagref);
                      # Figure out what compiler variable WOULD have been
                      # used were ONESTEP not involved.
                      if (exists($benchmarks->{$bench})) {
                        my ($benchlang) = @{$benchmarks->{$bench}->{'BENCHLANG'}};
                        if ($benchlang && $benchlang ne '?') {
                          $var = $benchlang.'C';
                          $fakedflag->[0] = [ $var ];
                          push @{$flaglist->{$currflag->{'class'}}}, $fakedflag;
                          $prev_match->{"$type:$var:$flag:".$currflag->{'name'}} = $fakedflag;
                        }
                      }
                    }

                    if ($inforef eq 'user') {
                        # For user flags, store (for later extraction and
                        # display) the included flags (all of them)
                        foreach my $refflag (find_flags_refs($inforef, $currflag)) {
                            $flaglist->{'seenflags'}->{$refflag}->{'flag'} = $refflag;
                            add_implicit_ref($flaglist, $refflag, $currflag, $bench, $tune);
                        }
                    }
                }

                if ($thing !~ /^\s*$/) {
                  # Put the option line back together for the benefit of
                  # subsequent checks...
                  $opts->[$i] = "$type$pass: $var=\"$thing\"";
                } else {
                  # Mark it so that it can be skipped quickly
                  $opts->[$i] = undef;
                }
              }
              $flaglist->{'passes'} = [ sort { $a <=> $b } @passes ];
          }
          Log(96, "Saw $count flags for $bench from $infokey flags\n");
        }
    }

    return ($opts, $flaglist);
#-----------------------------------------------------------------------------
%);
    if ($@) {
      Log(0, "ERROR: While parsing flags, the compartment returned:\n        $@\n\n");
      return undef;
    }

    # Go through the lines again and put any remainders into "unknown"
    # Only do one instance of each variable per type, since the "flag"
    # is everything that's left.
    foreach my $type (qw(C P O)) {
	my %seen = ();
        my $mode = undef;
	foreach my $unk (grep { /^(?:LINK\d*:|COMP\d*:|ONESTEP\d*:|$type\d*: \S+=\"\s*[^\"])/ } @{$opts}) {
            # Skip LINK and COMP lines
            if ($unk =~ /^(?:COMP|FPP)\d*:/) {
              $mode = 'comp';
              next;
            } elsif ($unk =~ /^LINK\d*:/) {
              $mode = 'link';
              next;
            } elsif ($unk =~ /^ONESTEP\d*:/) {
              $mode = 'both';
              next;
            }
	    if ($unk =~ /^$type(\d*): (\S+)=\"\s*(.*?)\s*\"$/) {
                my ($pass, $var, $val) = ($1, $2, $3);
		next if $val eq ''; # No flags left
		if ($seen{$var}) { # Already processed this one... maybe
                  # Find ones that match var and val and append mode and pass
                  # info to them
                  my $gotone = 0;
                  foreach my $seenref (grep { match_var($_, $var) && $_->[1] eq $val } @{$flaglist->{$type}->{'unknown'}}) {
                    push @$seenref, [ $mode, $pass ];
                    $gotone++;
                  }
                  next if $gotone;      # Fall through if not
                }
                my $flagref = [ [ $var ], $val, undef ];
                if ($mode eq 'both') {
                  # Cursed ONESTEP!
                  push @$flagref, [ 'comp', $pass ], [ 'link', $pass ];
                } else {
                  push @$flagref, [ $mode, $pass ];
                }
		$seen{$var} = $flagref;
		push @{$flaglist->{'unknown'}}, $flagref
	    }
	}
    }

    # Now go through and add one dummy "No flags used" to any sections that
    # don't have any flags.
    foreach my $class (@classlist) {
      next if $class =~ /^(?:unknown|forbidden)$/o;

      # This is inside the loop so that it's a different instance for each
      # class.
      my $noflag = [ [ '' ],            # Variables it was found in
                     'No flags used',   # Flag text
                      {                 # Flag description entry
                       'name' => 'XXX_noflags',
                       'regexp' => 'No flags used\b', # For one_match
                       'nomap' => 1,                  # Don't try to link to it
                       'display' => 1,                # Do display it
                       'origin' => [ undef, 'suite' ] # It came from the suite
                     }
                   ];

      if (isa($flaglist->{$class}, 'ARRAY')) {
        # Sections (like optimization) that are an EMPTY array still get the
        # no flag marking.  It's done this way to not change the ref (in case
        # anyone else is looking).
        @{$flaglist->{$class}} = ( $noflag ) unless @{$flaglist->{$class}};
      } else {
        $flaglist->{$class} = [ $noflag ];
      }
    }

#print Data::Dumper->Dump([$flaglist],[qw(flaglist)])."\n"; exit;
    return $flaglist;
}

sub one_match {
  my ($flag, $thing) = @_;

  my $s = new Safe 'retmp';
  $s->deny_only('entereval');  # Deny eval-based regexp stuff
  ${$s->varglob('re')} = $flag->{'regexp'};
  ${$s->varglob('thing')} = $thing;

  my ($matched, $leftovers, @replacements) = $s->reval(q% 
#-----------------------------------------------------------------------------
    my $flag = undef;
    my @replacements = ();
    # This is the same thing as above in flags_list.  If I could get it
    # inlined, I would, because when I break it out into a sub there's
    # a noticeable performance degradation (in flags_list, not here).
    if ($thing =~ s/($re)//) {
        no strict 'refs';
        $flag = $1;
        # Loop to add all captured groups
        # to the list of values to return
        for(my $i = 2; $i <= $#+; $i++) {
            push @replacements, ${$i};
        }
    }
    return ($flag, $thing, @replacements);
#-----------------------------------------------------------------------------
  %);
  if ($@) {
    Log(0, "ERROR: While attempting to match $flag->{'name'} to \"$thing\";\n  the compartment returned:\n        $@\n\n");
    return ();
  }
  if ($leftovers !~ /^\s*$/) {
    Log(0, "WARNING: Incomplete match for $flag->{'name'} to \"$thing\";\n  the leftovers were \"$leftovers\"\n\n");
  }
  if ($thing !~ /^\Q$matched\E$/) {
    Log(0, "Notice: Text matched ($matched) is not contained in original\n  text ($thing).\n\n");
  }
  return @replacements;
}

sub reduce_flags {
    # Given a set of flags (as assembled by flags_list), aggregate the common
    # flags by language for all tuning levels as well as each level
    # individually.
    # Returns a hash struct keyed by flag class, much like the input.
    my ($r) = @_;
    my %seen_bench = ();
    my %seen_tune = ();
    my %class2type = ( 'portability' => 'P', 'optimization' => 'O',
                       'compiler' => 'C' );
    my $rc = { 'var2desc' => {
                               'CC'        => 'C benchmarks',
                               'CXX'       => 'C++ benchmarks',
                               'CXXC'      => 'C++ benchmarks',
                               'F77'       => 'FORTRAN77 benchmarks',
                               'F77C'      => 'FORTRAN77 benchmarks',
                               'FC'        => 'Fortran benchmarks',
		               'CC FC'     => 'Benchmarks using both Fortran and C',
		               'CC F77'    => 'Benchmarks using both Fortran and C',
		               'CC F77C'   => 'Benchmarks using both Fortran and C',
		               'CXX FC'    => 'Benchmarks using both Fortran and C++',
		               'CXXC FC'   => 'Benchmarks using both Fortran and C++',
		               'CXX F77'   => 'Benchmarks using both Fortran and C++',
		               'CXXC F77C' => 'Benchmarks using both Fortran and C++',
			   }
             };

    # Get a list of benchmark counts by language.  This can't be done using the
    # flags list, as there may be some benchmarks (like those neither built
    # nor run) that don't have any entries in that structure at all.
    foreach my $bench (keys %{$r->{'benchmarks'}}) {
        my $langs = join(' ', map { ($_ eq 'F77' || $_ eq 'CXX') ? $_ : "${_}C" } sort @{$r->{'benchmarks'}->{$bench}->{'BENCHLANG'}});
        if ($::lcsuite eq 'cpu2006') {
            # There's no F77 in CPU2006; only F
            $langs =~ s/F77C?/FC/g;
        }
        $rc->{'benchcounts'}->{$langs}++;
    }

    # First, go through all flags and build a per-benchmark per-tune list
    # of flags used.  Each entry will have the flags concatenated as one
    # string (for comparison), the languages used, a list of the individual
    # flag text, and a list of references to the flag structures.
    foreach my $bench (keys %{$r->{'results'}}) {
	next unless defined($r->{'results'}->{$bench}) && isa($r->{'results'}->{$bench}, 'HASH');
	my $benchref = $r->{'results'}->{$bench};
	foreach my $tune (keys %{$benchref}) {
	    next unless defined($benchref->{$tune}) && isa($benchref->{$tune}, 'HASH');
	    my $tuneref = $benchref->{$tune};
	    next unless exists($tuneref->{'flags'}) && isa($tuneref->{'flags'}, 'HASH');
            my @passes = ::get_pass_list($r, $bench, $tune);
	    $tuneref = $tuneref->{'flags'};
	    foreach my $class (@classlist) {
		next unless ::check_elem('ARRAY', $tuneref, $class);
		my $type = $class2type{$class};
		my $classref = $tuneref->{$class};
		$rc->{'benchlist'}->{$bench}++;
		$rc->{'maxbench'} = length($bench) if length($bench) > $rc->{'maxbench'};
		$rc->{'tunelist'}->{$tune}++;
		$rc->{'maxtune'} = length($tune) if length($tune) > $rc->{'maxtune'};
		$rc->{'classlist'}->{$class}++;
		foreach my $flag (@{$classref}) {
		    if ($class eq 'compiler') {
                        my @names = @{$flag->[0]};
			if (grep { /^(CC|CXXC?|F77C?|FC)$/ } @names) {
                            # Deal with old (pre-kit 93) results.
                            # All newer results will have just
                            # "CXX" instead of "CXXC".
                            map { s/(CXX|F77)C/$1/g } @names;
			    push @{$rc->{'compilers'}->{$bench}->{$tune}}, @names;
			    map { $rc->{'compilers'}->{'all'}->{$_}++ } @names;
			}
		    }
		    my ($mismatch, $tmpflag, $markup);
		    if ($class eq 'unknown' || $class eq 'forbidden') {
			$tmpflag = "\"$flag->[1]\" (in ".join(',', @{$flag->[0]}).")";
			$mismatch = 0;
                        $markup = '';
		    } else {
			($mismatch, $tmpflag, $markup) = ::markup_flag($flag, 'passes' => \@passes, 'seen_errors' => $tuneref->{'seen_errors'}, 'classes' => [ $type ]);
		    }
		    push @{$rc->{'stringlist'}->{$class}->{$bench}->{$tune}}, $tmpflag;
		    push @{$rc->{'markup'}->{$class}->{$bench}->{$tune}}, $markup;
		    push @{$rc->{'flaglist'}->{$class}->{$bench}->{$tune}}, $flag;
		    push @{$rc->{'mismatches'}->{$class}->{$bench}->{$tune}}, $mismatch;
		    $rc->{'mismatch'}->{$class}->{$bench}->{$tune} += $mismatch;
		}
                $rc->{'string'}->{$class}->{$bench}->{$tune} = join(' ', @{$rc->{'stringlist'}->{$class}->{$bench}->{$tune}});
	    }
	    if (isa($rc->{'compilers'}->{$bench}->{$tune}, 'ARRAY')) {
                # Make sure that the list of compilers is unique.  Sure, you'd
                # never have two compilers listed on the same line on PURPOSE,
                # but this is probably a point of confusion that n00bs will
                # hit often.  (Hopefully not more than once per n00b.)
                my %compilers = map { $_ => 1 } @{$rc->{'compilers'}->{$bench}->{$tune}};
                @{$rc->{'compilers'}->{$bench}->{$tune}} = sort keys %compilers;
		my $langs = join(' ', @{$rc->{'compilers'}->{$bench}->{$tune}});
		$rc->{'langs'}->{$bench}->{$tune} = $langs;
		$rc->{'langlist'}->{$langs}++;
		if (!exists($rc->{'var2desc'}->{$langs})) {
		    Log(0, "ERROR: No var2desc member for \"$langs\"; please update flagutils.pl\n");
		} else {
		    my $langlen = length($rc->{'var2desc'}->{$langs});
		    $rc->{'maxlang'} = $langlen if $langlen > $rc->{'maxlang'};
		}
	    }
	}
    }

    # Next, if basepeak is used, fake up (or replace) peak flag records.
    # Do it on a per-benchmark basis and let the flag coalescing do its thing.
    if ($r->{'basepeak'} && grep { $_ eq 'peak' } @{$r->{'tunelist'}}) {
      my @bp_bench = ();
      if ($r->{'basepeak'} == 1) {
        # Whole-suite basepeak
        @bp_bench = keys %{$r->{'results'}};
      } elsif ($r->{'basepeak'} == 2) {
        for my $bench (keys %{$r->{'results'}}) {
          next unless ::check_elem('ARRAY', $r, 'results', $bench, 'peak', 'data');
          next unless isa($r->{'results'}{$bench}{'peak'}{'data'}->[0], 'HASH');
          next unless $r->{'results'}{$bench}{'peak'}{'data'}->[0]->{'basepeak'};
          push @bp_bench, $bench;
        }
      }
      my $onetune = 'base';     # (keys %{$rc->{'tunelist'}})[0];
      $rc->{'tunelist'}->{'peak'}++;
      $rc->{'classlist'}->{'optimization'}++;
      foreach my $bench (@bp_bench) {
        # Remove peak flags for this benchmark from _all_ other classes of
        # flags; if there are unknown, forbidden, etc, the base will cover
        # them.
        foreach my $tmpclass (keys %{$rc->{'classlist'}}) {
          foreach my $thing (qw(string stringlist markup flaglist
                                mismatches mismatch)) {
            next unless ::check_elem('HASH', $rc, $thing, $tmpclass, $bench);
            delete $rc->{$thing}->{$tmpclass}->{$bench}->{'peak'};
          }
        }
        $rc->{'langs'}->{$bench}->{'peak'} = $rc->{'langs'}->{$bench}->{$onetune};
        $rc->{'stringlist'}->{'optimization'}->{$bench}->{'peak'} = [ 'basepeak = yes' ];
        $rc->{'markup'}->{'optimization'}->{$bench}->{'peak'} = [ undef ];
        # Fake up a flag description:
        $rc->{'flaglist'}->{'optimization'}->{$bench}->{'peak'} = [
          [ [ '' ],            # Variables it was found in
            'basepeak = yes',  # Flag text
            {                  # Flag description entry
              'name' => 'basepeak',
              'regexp' => 'basepeak = yes\b',# Necessary, or one_match complains
              'nomap' => 1,                  # Don't try to link to it
              'display' => 1,                # Do display it
              'origin' => [ undef, 'suite' ] # It came from the suite
            }
          ]
        ];
        $rc->{'string'}->{'optimization'}->{$bench}->{'peak'} = 'basepeak = yes';
        $rc->{'mismatches'}->{'optimization'}->{$bench}->{'peak'} = [ 0 ];
        $rc->{'mismatch'}->{'optimization'}->{$bench}->{'peak'} = 0;
      }
    }

    # Third, go through and check for instances of the same flag being used
    # in more than one variable.  For those cases, eliminate the duplicates,
    # keeping one structure that has all of the "used in" variables listed,
    # as well as a unified list of phases in which it was used.
    foreach my $bench (keys %{$rc->{'benchlist'}}) {
      foreach my $tune (keys %{$rc->{'tunelist'}}) {
        # It's okay to do this one class at a time, since a given flag will
        # always only be in one class.
        foreach my $class (keys %{$rc->{'classlist'}}) {
          next unless check_elem('ARRAY', $rc, 'flaglist', $class, $bench, $tune);
          my $flaglistref = $rc->{'flaglist'}->{$class}->{$bench}->{$tune};
          my @kill = ();        # List of indices to remove

          # Go through the flag list and get counts of each flag used.
          my %counts = ();
          for(my $i = 0; $i < @{$flaglistref}; $i++) {
            my $flag = $flaglistref->[$i];
            next unless isa($flag, 'ARRAY');
            if ($class =~ /(?:forbidden|unknown)/) {
              # These are treated specially because they're not real flags
              my $key = $flag->[1].' (in '.join(',', @{$flag->[0]}).')';
              $counts{$key}->[0]++;
              $counts{$key}->[1] = $key;        # Why not?
              push @{$counts{$key}->[2]}, $i;
            } else {
              next unless isa($flag->[2], 'HASH');
              # Because all of the flag name, matched text, description, and
              # mismatch must match, make that the key.
              my $key = $flag->[2]->{'name'}.$flag->[1].$rc->{'mismatches'}->{$class}->{$bench}->{$tune}->[$i].$flag->[2]->{'description'};
              $counts{$key}->[0]++;
              $counts{$key}->[1] = $flag->[2]->{'name'}; # Redundant, yes.
              push @{$counts{$key}->[2]}, $i;
            }
          }

          # Now iterate over the flags that have more than one instance.
          foreach my $key (sort keys %counts) {
            my ($count, $flagname, $indices) = @{$counts{$key}};
            if ($count <= 1) {
              delete $counts{$key};
              next;
            }
            # Here's a flag that matched multiple times; mash it all together
            my $dstidx = $counts{$key}->[2]->[0];
            my %langs = map { $_ => 1 } @{$flaglistref->[$dstidx]->[0]};
            for(my $i = 1; $i < @{$counts{$key}->[2]}; $i++) {
              my $srcidx = $counts{$key}->[2]->[$i];
              foreach my $var (@{$flaglistref->[$srcidx]->[0]}) {
                $langs{$var}++;
              }
              merge_passlist($flaglistref, $dstidx, $srcidx);
              push @kill, $srcidx;
            }
            @{$flaglistref->[$dstidx]->[0]} = sort keys %langs;
          }

          # Now for the indices listed in @kill, kill!
          # Strings and markup will be handled in a bit.
          my $killed = 0;
          foreach my $killidx (sort { $b <=> $a } @kill) {
            foreach my $thing (qw(flaglist mismatches)) {
              splice @{$rc->{$thing}->{$class}->{$bench}->{$tune}}, $killidx, 1;
            }
            $killed++;
          }

          if ($killed) {
            my $type = $class2type{$class};
            my @passes = ::get_pass_list($r, $bench, $tune);
            # Regenerate the other stuff
            foreach my $thing (qw(stringlist markup)) {
              @{$rc->{$thing}->{$class}->{$bench}->{$tune}} = ();
            }
            for(my $i = 0; $i < @{$flaglistref}; $i++) {
              my ($mismatch, $tmpflag, $markup);
              if ($class =~ /(?:unknown|forbidden)/) {
                # Once again, not real flags
                $tmpflag = '"'.$flaglistref->[$i]->[1].'" (in '.join(', ', @{$flaglistref->[$i]->[0]}).')';
                $markup = '';
              } else {
                ($mismatch, $tmpflag, $markup) = ::markup_flag($flaglistref->[$i], 'passes' => \@passes, 'classes' => [ $type ]);
              }
              push @{$rc->{'stringlist'}->{$class}->{$bench}->{$tune}}, $tmpflag;
              push @{$rc->{'markup'}->{$class}->{$bench}->{$tune}}, $markup;
            }
            $rc->{'string'}->{$class}->{$bench}->{$tune} = join(' ', @{$rc->{'stringlist'}->{$class}->{$bench}->{$tune}});
          }
        }
      }
    }

    # Handy shorthand; no need to sprinkle keys %{...} around everywhere
    $rc->{'tunecount'}  = (keys %{$rc->{'tunelist'}}) +0;
    $rc->{'benchcount'} = (keys %{$rc->{'benchlist'}})+0;
    $rc->{'classcount'} = (keys %{$rc->{'classlist'}})+0;
    $rc->{'langcount'}  = (keys %{$rc->{'langlist'}}) +0;

    # For each class of flags, make a count of each set seen.
    my %counts = ();
    my %bench2key = ();
    foreach my $class (keys %{$rc->{'classlist'}}) {
	foreach my $bench (keys %{$rc->{'benchlist'}}) {
	    foreach my $tune (keys %{$rc->{'tunelist'}}) {
		next unless check_elem(undef, $rc, 'langs', $bench, $tune);
		next unless check_elem(undef, $rc, 'string', $class, $bench, $tune);
		next unless check_elem('ARRAY', $rc, 'flaglist', $class, $bench, $tune);
		my $langs = $rc->{'langs'}->{$bench}->{$tune};
                # It's not sufficient to use just the string, as it's not
                # guaranteed to be unique.
		my $optstring = $rc->{'string'}->{$class}->{$bench}->{$tune};
                my $flaglistref = $rc->{'flaglist'}->{$class}->{$bench}->{$tune};
                my $key = '';
                # All this stuff is to make sure that different flags with the
                # same text aren't counted together.
                for(my $i = 0; $i <= $#{$flaglistref}; $i++) {
                  $key .= $flaglistref->[$i]->[2]->{'name'};
                  $key .= $flaglistref->[$i]->[1];
                  $key .= $rc->{'mismatches'}->{$class}->{$bench}->{$tune}->[$i];
                  $key .= $flaglistref->[$i]->[2]->{'description'};
                  $key .= ' ';
                }
                $bench2key{$class.$bench.$tune} = $key;
		$counts{$class}->{$tune}->{$langs}->{$key}++;
	    }
	}
    }

    # Now for each class + language combo, find the string with the highest
    # count.  If it is higher than 1, remove all the flags that use it.
    # This is per-language flag aggregation.
    foreach my $class (keys %counts) {
	if ($class eq 'unknown' ||
	    $class eq 'forbidden') {
	    # Repetition is a form of emphasis; do NOT collapse unknown
	    # or forbidden flags.
	    next;
	}
	foreach my $tune (keys %{$counts{$class}}) {
	    foreach my $langs (keys %{$counts{$class}->{$tune}}) {
		next unless defined($langs) && $langs ne '';
		my @popular = sort { $counts{$class}->{$tune}->{$langs}->{$b} <=> $counts{$class}->{$tune}->{$langs}->{$a} } keys %{$counts{$class}->{$tune}->{$langs}};
                my $popular_count = $counts{$class}->{$tune}->{$langs}->{$popular[0]};

		if (@popular == 1 &&
                    $popular_count == $rc->{'benchcounts'}->{$langs}) {
		    # There is one common set of flags used, and it is used
                    # for ALL benchmarks of the current language.  That means
		    # that for this tuning level in this class, everything is
		    # the same.
		    $rc->{'langmatch'}->{$class}->{$tune}->{$langs} = 1;
		} else {
		    if ($popular_count > 1 &&
                        $popular_count < $rc->{'benchcounts'}->{$langs}) {
                      # There are at least _some_ common flags, but not all
                      # of the benchmarks use them.
                      $rc->{'langmatch'}->{$class}->{$tune}->{$langs} = 2;
                    } else {
                      $rc->{'langmatch'}->{$class}->{$tune}->{$langs} = 0;
                    }
		}
                # The idea is to removing entries that match the most popular
                # one ONLY if the "most popular" one means more than one
                # benchmark (langmatch==2) OR 'langmatch' is set.
		if ($popular[0] ne '' &&
                    $rc->{'langmatch'}->{$class}->{$tune}->{$langs}) {
		    # Go through and remove entries in $rc which match the
		    # most popular string.
		    foreach my $bench (keys %{$rc->{'benchlist'}}) {
			next unless check_elem(undef, $rc, 'string', $class, $bench, $tune);
                        # In order to be eliminated, the benchmark's
                        # 1. language must match
                        # 2. flag text must match
                        # 3. flag descriptions must match
                        # It's easiest to compare the "key" generated earlier
                        # with the contents of $popular[0] in order to match
                        # #2 and #3.
			if ($rc->{'langs'}->{$bench}->{$tune} eq $langs &&
                            $bench2key{$class.$bench.$tune} eq $popular[0]) {
			    # It's a match!  Remove it.
			    foreach my $thing (qw(markup stringlist string flaglist mismatch mismatches)) {
				next unless check_elem(undef, $rc, $thing, $class, $bench, $tune);
				$rc->{'bylang'}->{$thing}->{$class}->{$tune}->{$langs} = $rc->{$thing}->{$class}->{$bench}->{$tune};
				delete $rc->{$thing}->{$class}->{$bench}->{$tune};
				if (keys %{$rc->{$thing}->{$class}->{$bench}} == 0) {
				    # No more elements, so remove it too.
				    delete $rc->{$thing}->{$class}->{$bench};
				}
			    }
			}
		    }
		}
	    }
	}
    }

    # See if the various tuning level flags are all the same.  If so, make
    # a notation in the appropriate section.  It's up to the formatters
    # whether or not they output collapsed or per-tuning sections.

    # Do the per-language comparison
    foreach my $langs (keys %{$rc->{'langlist'}}) {
	my $reftune = (keys %{$rc->{'tunelist'}})[0];
	foreach my $tune (keys %{$rc->{'tunelist'}}) {
	    next if $tune eq $reftune;
	    foreach my $class (keys %{$rc->{'classlist'}}) {
		next if $class eq 'compilers';  # Note: not 'compiler'

                # Make sure that there are flag lists for each tune.
		my $reftuneok = ::check_elem('HASH', $counts{$class}, $reftune, $langs);
		my $tuneok =    ::check_elem('HASH', $counts{$class}, $tune,    $langs);
                # If one has counts and the other doesn't, they don't match!
                if ($reftuneok != $tuneok) {
                  $rc->{'langmatch'}->{$class}->{'alltune'}->{$langs} = 0;
                  next;
                }

                # So now they either both have flags or they don't.  If they
                # both don't, there's no point in continuing.
                next unless ($reftuneok && $tuneok);

                my @refflags = sort keys %{$counts{$class}->{$reftune}->{$langs}};
                my @tuneflags = sort keys %{$counts{$class}->{$tune}->{$langs}};
                # If there are different numbers of flags, they definitely
                # don't match
                if ($#refflags != $#tuneflags) {
                  $rc->{'langmatch'}->{$class}->{'alltune'}->{$langs} = 0;
                  next;
                }
		if (!defined($rc->{'langmatch'}->{$class}->{'alltune'}->{$langs})) {
		    $rc->{'langmatch'}->{$class}->{'alltune'}->{$langs} = 1;
		}
                for(my $i = 0; $i < @refflags; $i++) {
                  if ($refflags[$i] ne $tuneflags[$i]) {
		    $rc->{'langmatch'}->{$class}->{'alltune'}->{$langs} = 0;
                    last;
                  }
                }
	    }
	}
    }

    # Do the per-benchmark comparison
    foreach my $bench (keys %{$rc->{'benchlist'}}) {
	my $reftune = (keys %{$rc->{'tunelist'}})[0];
	foreach my $tune (keys %{$rc->{'tunelist'}}) {
	    next if $tune eq $reftune;
	    my $langs = $rc->{'langs'}->{$bench}->{$tune};
	    foreach my $class (keys %{$rc->{'classlist'}}) {
		next if $class eq 'compilers';  # Note: not 'compiler'

                # Make sure that there are flag lists for each tune.
                my $reftuneok = ::check_elem('ARRAY', $rc, 'flaglist', $class, $bench, $reftune);
                my $tuneok =    ::check_elem('ARRAY', $rc, 'flaglist', $class, $bench, $tune);

                # If one has flags and the other doesn't, they don't match!
		if ($reftuneok != $tuneok) {
                  $rc->{'benchmatch'}->{$class}->{$bench}->{'alltune'} = 0;
                  next;
                }

                # So now they either both have flags or they don't.  If they
                # both don't, there's no point in continuing.
		next unless ($reftuneok && $tuneok);

                my $flaglistref = $rc->{'flaglist'}->{$class}->{$bench};
                # If there are different numbers of flags, they definitely
                # don't match
                if ($#{$flaglistref->{$reftune}} != $#{$flaglistref->{$tune}}) {
                  $rc->{'benchmatch'}->{$class}->{$bench}->{'alltune'} = 0;
                  next;
                }

		if (!defined($rc->{'benchmatch'}->{$class}->{$bench}->{'alltune'})) {
		    $rc->{'benchmatch'}->{$class}->{$bench}->{'alltune'} = 1;
		}

                my $refref = '';
                my $classref = '';
                # All this stuff is to make sure that different flags with the
                # same text are not merged.  For example, -DSPEC_CPU_SOLARIS for
                # 400.perlbench is different than -DSPEC_CPU_SOLARIS for
                # 403.gcc.
                for(my $i = 0; $i < @{$flaglistref->{$reftune}}; $i++) {
                  $refref .= $flaglistref->{$reftune}->[$i]->[2]->{'name'};
                  $refref .= $flaglistref->{$reftune}->[$i]->[1];
                  $refref .= $rc->{'mismatches'}->{$class}->{$bench}->{$reftune}->[$i];
                  $refref .= $flaglistref->{$reftune}->[$i]->[2]->{'description'};
                  $refref .= ' ';
                  $classref .= $flaglistref->{$tune}->[$i]->[2]->{'name'};
                  $classref .= $flaglistref->{$tune}->[$i]->[1];
                  $classref .= $rc->{'mismatches'}->{$class}->{$bench}->{$tune}->[$i];
                  $classref .= $flaglistref->{$tune}->[$i]->[2]->{'description'};
                  $classref .= ' ';
                }
		if ($refref ne $classref) {
		    $rc->{'benchmatch'}->{$class}->{$bench}->{'alltune'} = 0;
		}
	    }
	}
    }

    # Finally, go through benchmatch and langmatch.  If ANY 'alltune' is set
    # to 0, then a single unified section cannot be output.
    foreach my $class (keys %{$rc->{'classlist'}}) {
      if ($rc->{'tunecount'} <= 1) {
        # Don't "consolidate" sections for runs with only 1 tuning level
        $rc->{'allmatch'}->{$class} = 0;
        next;
      }
      if (!exists($rc->{'allmatch'}->{$class})) {
        $rc->{'allmatch'}->{$class} = 1;
      }
      next if $rc->{'allmatch'}->{$class} == 0; # Don't waste time

      # Go through all of the languages in langmatch
      if (::check_elem('HASH', $rc, 'langmatch', $class, 'alltune')) {
        foreach my $lang (keys %{$rc->{'langmatch'}->{$class}->{'alltune'}}) {
          if (!defined($rc->{'langmatch'}->{$class}->{'alltune'}->{$lang}) ||
              $rc->{'langmatch'}->{$class}->{'alltune'}->{$lang} == 0) {
            $rc->{'allmatch'}->{$class} = 0;
            last;
          }
        }
      }

      # Go through all of the benchmarks in benchmatch
      if (::check_elem('HASH', $rc, 'benchmatch', $class)) {
        foreach my $bench (keys %{$rc->{'benchmatch'}->{$class}}) {
          if (!::check_elem(undef, $rc, 'benchmatch', $class, $bench, 'alltune') ||
              $rc->{'benchmatch'}->{$class}->{$bench}->{'alltune'} == 0) {
            $rc->{'allmatch'}->{$class} = 0;
            last;
          }
        }
      }
    }

    # Now that everything's settled out and been reduced, go through and
    # re-mark the ones which have common flag text with another flag in
    # the same class + (language / benchmark).  This is to allow the
    # compiler to be added to the marking.
    foreach my $class (keys %{$rc->{'classlist'}}) {
      # Go through all of the languages
      if (::check_elem('HASH', $rc, 'bylang', 'flaglist', $class)) {
        foreach my $tune (keys %{$rc->{'bylang'}->{'flaglist'}->{$class}}) {
          next unless ::check_elem('HASH', $rc, 'bylang', 'flaglist', $class, $tune);
          my $tuneref = $rc->{'bylang'}->{'flaglist'}->{$class}->{$tune};
          foreach my $lang (keys %{$tuneref}) {
            next unless isa($tuneref->{$lang}, 'ARRAY');
            # Build a hash with counts of the flag text
            my %counts = ();
            map { $counts{$_->[1]}++ } grep { isa($_, 'ARRAY') } @{$tuneref->{$lang}};
            # Weed out the ones that aren't duplicates
            map { delete $counts{$_} } grep { $counts{$_} <= 1 } keys %counts;
            foreach my $dupflag (keys %counts) {
              munge_markup($dupflag, $tuneref->{$lang}, $r,
                           $rc->{'bylang'}->{'markup'}->{$class}->{$tune}->{$lang},
                           $rc->{'bylang'}->{'stringlist'}->{$class}->{$tune}->{$lang},
                           \$rc->{'bylang'}->{'string'}->{$class}->{$tune}->{$lang});
            }
          }
        }
      }

      # Now, all the benchmarks (if any)
      if (::check_elem('HASH', $rc, 'flaglist', $class)) {
        foreach my $bench (keys %{$rc->{'flaglist'}->{$class}}) {
          next unless ::check_elem('HASH', $rc, 'flaglist', $class, $bench);
          foreach my $tune (keys %{$rc->{'flaglist'}->{$class}->{$bench}}) {
            next unless ::check_elem('ARRAY', $rc, 'flaglist', $class, $bench, $tune);
            my $tuneref = $rc->{'flaglist'}->{$class}->{$bench}->{$tune};
            # Build a hash with counts of the flag text
            my %counts = ();
            map { $counts{$_->[1]}++ } grep { isa($_, 'ARRAY') } @{$tuneref};
            # Weed out the ones that aren't duplicates
            map { delete $counts{$_} } grep { $counts{$_} <= 1 } keys %counts;
            foreach my $dupflag (keys %counts) {
              munge_markup($dupflag, $tuneref, $r,
                           $rc->{'markup'}->{$class}->{$bench}->{$tune},
                           $rc->{'stringlist'}->{$class}->{$bench}->{$tune},
                           \$rc->{'string'}->{$class}->{$bench}->{$tune});
            }
          }
        }
      }
    }

#print Data::Dumper->Dump([$rc], ['rc'])."\n"; exit;
    return $rc;
}

sub munge_markup {
  my ($dupflag, $flaglist, $r, $markuplist, $stringlist, $stringref) = @_;

  my $munged = 0;
  for(my $i = 0; $i < @{$flaglist}; $i++) {
    next unless isa($flaglist->[$i], 'ARRAY');
    my $flagref = $flaglist->[$i];
    next unless $flagref->[1] eq $dupflag;
    # If there's no compiler name, then there's really not
    # anything that can be done.
    next unless (defined($flagref->[2]->{'compiler_used'}) ||
                 $flagref->[2]->{'compiler_used'} ne '');
    my $markup = $markuplist->[$i];
    my $compiler = $flagref->[2]->{'compiler_used'};
    my $compiler_name;
    if (::check_elem('HASH', $r, 'flaginfo', 'user', 'flagmap', $compiler)) {
      # Get the example text from the flag named in the
      # 'compiler_used' field.
      $compiler_name = $r->{'flaginfo'}->{'user'}->{'flagmap'}->{$compiler}->{'example'};
    }
    if (!defined($compiler_name) || $compiler_name eq '') {
      # What else can we do?  There's no entry for it!
      $compiler_name = $compiler;
    }
    # Fix up the markup
    $markuplist->[$i] = "($compiler_name)".$markup;
    # Fix up the stringlist
    $stringlist->[$i] = $flagref->[1]."($compiler_name)".$markup;
    $munged++;
  }
  if ($munged) {
    # Regenerate the class string
    $$stringref  = join(' ', @{$stringlist});
  }
}

sub do_replacements {
    # Given a string and a list of values, replace $1, ... with the elements
    # of the list.  Backslash escapes are honored.
    my ($str, @repl) = @_;

    $str =~ s/(?:\\(.)|(\$){?(\d+)}?)/($2 eq '$') ? $repl[$3 - 1] : $1/eg;

    return $str;
}

sub find_flags_refs {
    my ($flaghash, $flag, $seen) = @_;

    $seen = {} unless isa($seen, 'HASH');

    return unless exists($flag->{'includes'});

    foreach my $flagname (@{$flag->{'includes'}}) {
	next if ($seen->{$flagname});
	if (!exists($flaghash->{$flagname})) {
	    Log(0, "\nWARNING: Flag '".$flag->{'name'}."' references unknown flag '$flagname'\n");
	    next;
	}
	if (!isa($flaghash->{$flagname}, 'HASH')) {
	    Log(0, "\nERROR: Flag '$flagname' isn't a HASH.  How did that happen?\n");
	    next;
	}
	$seen->{$flagname}++;
	if (exists($flaghash->{$flagname}->{'includes'})) {
	    find_flags_refs($flaghash, $flaghash->{$flagname}, $seen);
	}
    }

    return map { $flaghash->{$_} } sort keys %{$seen};
}

sub add_implicit_ref {
    my ($r, $refflag, $currflag, $bench, $tune) = @_;
    my $refs = undef;

    $r->{'seenflags'}->{$refflag}->{'refs'} = [] unless (ref($r->{'seenflags'}->{$refflag}->{'refs'}) eq 'ARRAY'); 
    $refs = $r->{'seenflags'}->{$refflag}->{'refs'}; 
    if (!grep { $_->[3] eq $bench } @{$refs}) {
        push @{$refs}, [ 'implicit', '', $currflag, $bench, $tune ];
    }
}

sub search_flags_byclass {
    my ($result, $class, $bench, $tune) = @_;

    return undef unless ::check_elem('HASH', $result, 'results');

    my @bench = (defined($bench) && $bench ne '') ? ($bench) : (keys %{$result->{'results'}});
    # Return true if there are any flags in $class in the result
    foreach my $bench (@bench) {
      next unless ::check_elem('HASH', $result, 'results', $bench);
      my @tunes = (defined($tune) && $tune ne '') ? ($tune) : (keys %{$result->{'results'}->{$bench}});
      foreach my $tune (@tunes) {
        next unless ::check_elem('HASH', $result, 'results', $bench, $tune, 'flags');
        if (exists($result->{'results'}->{$bench}->{$tune}->{'flags'}->{$class})) {
          ::Log(97, "Found \"$class\" flags for $bench:$tune\n");
          return 1;
        }
      }
    }
    return 0;
}

sub search_reduced_flags_byclass {
    my ($result, $class, $bench, $tune) = @_;

    return undef unless ::check_elem('HASH', $result, 'reduced_flags');
    my $rf = $result->{'reduced_flags'};

    my @bench = (defined($bench) && $bench ne '') ? ($bench) : (keys %{$rf->{'benchlist'}});
    # Return true if there are any flags in $class in the result
    foreach my $bench (@bench) {
      next unless ::check_elem('HASH', $rf, 'flags', $bench);
      my @tunes = (defined($tune) && $tune ne '') ? ($tune) : (keys %{$rf->{'tunelist'}});
      foreach my $tune (@tunes) {
        next unless ::check_elem('HASH', $rf, 'flags', $bench, $tune);
        if (exists($rf->{'flags'}->{$bench}->{$tune}->{$class})) {
          ::Log(97, "Found \"$class\" flags for $bench:$tune\n");
          return 1;
        }
      }
    }
    return 0;
}

sub get_flags_byclass {
    my ($result, $class, $bench, $tune, @types) = @_;

    # Return list of references to flags of class $class
    return () unless ::check_elem('ARRAY', $result, 'results', $bench, $tune, 'flags', $class);

    my @rc = ();
    if (@types) {
      foreach my $type (@types) {
        push @rc, grep { $_->{'found_in'} eq $type } @{$result->{'results'}->{$bench}->{$tune}->{'flags'}->{$class}};
      }
    } else {
      push @rc, @{$result->{'results'}->{$bench}->{$tune}->{'flags'}->{$class}};
    }
    return @rc;
}

sub get_pass_list {
    my ($result, $bench, $tune) = @_;

    # Return list of references to flags of class $class
    return () unless ::check_elem('ARRAY', $result, 'results', $bench, $tune, 'flags', 'passes');
    return @{$result->{'results'}->{$bench}->{$tune}->{'flags'}->{'passes'}};
}

sub markup_flag {
  my ($flag, %opts) = @_;
  # Mark a flag if its class is mismatched, whether it is used in all
  # link/compilation passes, etc.
  my $mismatch = 0;
  my $markup = '';
  my $seen_error = $opts{'seen_error'};
  my $passes = $opts{'passes'};
  $passes = [ $passes ] if (!isa($passes, 'ARRAY'));
  my @classes = grep { defined } @{$opts{'classes'}} if isa($opts{'classes'}, 'ARRAY');
  if (@classes && !grep { /^[CP]$/ } @classes) {
    # Only portability flags may not appear on compiler lines
    push @classes, 'C';
  }

  # Skip the exercise if it's just for a placeholder.
  return(0, $flag->[1], '') if $flag->[1] eq 'No flags used';

  # Check to see if the flag was in a variable in one of the allowed
  # classes (if any are specified).
  if (@classes && !grep { $flag->[2]->{'found_in'} eq $_ } @classes) {
    $mismatch = 1;
    $markup .= '(*)';
  }

  # Mark is as a non-displayed flag if it is.
  if ($flag->[2]->{'display'} == 0) {
    $markup .= '(NODISPLAY)';
  }

  return ($mismatch, $flag->[1].$markup, $markup) if !isa($passes, 'ARRAY');

  my $passcount = scalar(grep { defined } @{$passes});
  $passcount++ if $passcount == 0;

  # Now try to figure out whether and how to mark up which phase(s) and
  # pass(es) a particular flag was used.

  # Make an array, indexed by pass, with a bitmap indicating which phases
  # were seen for each pass.
  my @usage = ();
  my %phasemap = ( 'comp' => 1, 'link' => 2 );
  my %add = ('comp' => [], 'link' => [] );
  my %sum;
  my %seen;
  foreach my $passref (@{$flag}[3..$#{$flag}]) {
    next if !isa($passref, 'ARRAY');
    my ($phase, $pass) = @$passref;
    if (!exists $phasemap{$phase}) {
      ::Log(0, "WARNING: Unknown build phase ($phase) found in flag structure.\n");
      next;
    }
    next if exists($seen{$phase.$pass});
    $seen{$phase.$pass} = 1;
    $usage[$pass+0] |= $phasemap{$phase};
    push @{$add{$phase}}, $pass;
    $sum{$phase}++;
  }

  if ($sum{'comp'} == $passcount && $sum{'link'} == $passcount) {
    # Done!  Nothing to add.
    return ($mismatch, $flag->[1].$markup, $markup);
  }

  my %used = ('comp' => 0, 'link' => 0);
  # Figure out how to concisely display exactly where each flag was used.
  foreach my $phase (qw(comp link)) {
    next unless @{$add{$phase}};
    $used{$phase} = 1;
    if ($sum{$phase} == $passcount) {
      $sum{$phase} = '';
      next;
    }
    my ($multiple, $str) = ::nicejoin(@{$add{$phase}});
    if ($str ne '') {
      $used{$phase} = 2;
      $sum{$phase}  = 'pass';
      $sum{$phase} .= 'es' if $multiple;
      $sum{$phase} .= ' '.$str;
    }
  }

  if ($used{'comp'} == 1 && $used{'link'} == 0) {
    # Compilation only
    if (0) {
      # Removed -- too much detail
      $markup .= '(compilation)';
    }
  } elsif ($used{'comp'} == 0 && $used{'link'} == 1) {
    # Link only
    if (0) {
      # Removed -- too much detail
      $markup .= '(link)';
    }
  } elsif (($used{'comp'} == 1 && $used{'link'} == 2) ||
           ($used{'comp'} == 2 && $used{'link'} == 1)) {
    # One with no pass listed, and one with.  This is a problem.
    ::Log(0, "ERROR: Flag options error.  A pass-usage mismatch was detected\n");
    ::Log(0, "       for \"$flag->[1]\" in ".join(',', @{$flag->[0]})."\n");

    ::Log(0, "        The flag was used in ".pluralize($used{'comp'}, 'compilation phase')." and ".pluralize($used{'link'}, 'link phase')."\n");
    if ($seen_error) {
      ::Log(0, "  Normally, this error should never occur.  Flag parsing errors seen\n");
      ::Log(0, "  previously may cause this error, so the flag file MUST be corrected and\n");
      ::Log(0, "  this result reformatted");
    } else {
      ::Log(0, "  This is probably an internal tools error that should never happen.\n");
      ::Log(0, "  Please report this error to ${main::lcsuite}support\@spec.org.  Please\n");
      ::Log(0, "  include the config file used to generate this result, the command line\n");
      ::Log(0, "  used to generate the result, and the flags file used.\n");
    }
  } elsif ($used{'comp'} == 2 && $used{'link'} == 2 &&
      $sum{'comp'} eq $sum{'link'}) {
    # Used in both phases; list only the passes.
    $markup .= '('.$sum{'comp'}.')';
  } elsif ($sum{'comp'} ne '' && $sum{'link'} eq '') {
    # Compilation only
    if (0) {
      # Removed -- too much detail
      $markup .= '(compilation';
      $markup .= ' '.$sum{'comp'} if $sum{'comp'} ne '';
      $markup .= ')';
    } else {
      $markup .= '('.$sum{'comp'}.')' if $sum{'comp'} ne '';
    }
  } elsif ($sum{'comp'} eq '' && $sum{'link'} ne '') {
    # Link only
    if (0) {
      # Removed -- too much detail
      $markup .= '(link';
      $markup .= ' '.$sum{'link'} if $sum{'link'} ne '';
      $markup .= ')';
    } else {
      $markup .= '('.$sum{'link'}.')' if $sum{'link'} ne '';
    }
  } elsif ($sum{'comp'} eq '' && $sum{'link'} eq '') {
    ::Log(0, "ERROR: Flag accounting error; a flag was found that was not used for\n");
    ::Log(0, "       compilation or link.\n");
    ::Log(0, "  This is probably an internal tools error that should never happen.\n");
    ::Log(0, "  Please report this error to ${main::lcsuite}support\@spec.org.  Please\n");
    ::Log(0, "  include the config file used to generate this result, the command line\n");
    ::Log(0, "  used to generate the result, and the flags file used.\n");
  }

  return ($mismatch, $flag->[1].$markup, $markup);
}

sub nicejoin {
  my (@list) = @_;
  my @rc = ();
  my $multiple = @list > 1;
  return @list unless @list;
  return (0, @list) if !$multiple;

  # Make sure that the list is ordered.  This ensures that the code below
  # for making backwards ranges will never be used, but I'm not going to
  # rip it out (it works) because I might want to be able to recognize
  # descending ranges someday
  @list = sort { $a <=> $b } @list;

  # Now look for sequences
  my $start = 0;
  my $reverse = $list[0] > $list[1];
  my @ranges = ();
  for(my $i = 0; $i < $#list - 1; $i++) {
      $start = $i if (!defined($start));
      if ((!$reverse && ($list[$i]+1 != $list[$i+1]) ||
           ($reverse && ($list[$i]-1 != $list[$i+1])))) {
          # This is the end of the current sequence
          if ($i == $start) {
              # Singleton
              push @ranges, [ $list[$i] ];
          } elsif (abs($i - $start) == 1) {
              # Range of two; record them as singletons
              push @ranges, [ $list[$start] ], [ $list[$i] ];
          } else {
              # Range
              push @ranges, [ $list[$start], $list[$i] ];
          }
          $start = undef;
      }
  }
  # Take care of the last element
  if (!defined($start) || $#list == $start) {
      # Singleton
      push @ranges, [ $list[$#list] ];
  } elsif (abs($#list - $start) == 1) {
      # Range of two; record them as singletons
      push @ranges, [ $list[$start] ], [ $list[$#list] ];
  } else {
      # Range
      push @ranges, [ $list[$start], $list[$#list] ];
  }

  # Now render it
  foreach my $rref (@ranges) {
    push @rc, join('-', @{$rref}) if ref($rref) eq 'ARRAY';
  }

  return ($multiple, join(', ', @rc));
}

sub makeanchor {
    my ($anchor) = @_;
    $anchor =~ s/C\+\+/CXX/g;
    $anchor =~ tr/A-Za-z0-9//cd;
    $anchor = "_$anchor" unless $anchor =~ /^[0-9A-Za-z:_]/;

    return $anchor;
}

sub make_flag_id {
  my ($flag, $opttag, $text, $forceadd) = @_;
  return undef unless isa($flag, 'HASH');
  my $sum = '';
  if (!defined($text) || $text eq $flag->{'example'}) {
    # The default case should be no text
    $text = '';
  } else {
    $sum = ::md5scalardigest($text);
  }
  my $origin = $flag->{'origin'}->[1];
  if ($origin eq 'user' || $origin eq 'suite' || $forceadd) {
    $opttag =~ tr/A-Za-z0-9:_/_/c;
    $origin .= "_$opttag" if defined($opttag) && $opttag ne '';
  }
  my $id = $origin.'_'.$flag->{'name'};
  $id .= "_$sum" if ($sum ne '');
  $id = "b$id" if $id !~ /^[A-Za-z:_]/;
  if (exists($flag->{'idcache'}) && isa ($flag->{'idcache'}, 'HASH') &&
      exists($flag->{'idcache'}->{$text})) {
      # This is not for speed (otherwise it'd be farther up); it's for
      # consistency.  We'd use a sort of first-come-first-served approach to
      # handing out IDs, but that's dependent on the order in which the IDs
      # are requested, and they'll probably be different in each of the
      # formatters.
      return $flag->{'idcache'}->{$text.':'.$id};
  }
  $flag->{'idcache'}->{$text.':'.$id} = $id;
  return $id;
}

sub check_same_flags {
  # Given a hash ref whose values are array refs which contain strings,
  # determine if all the lists are the same.
  my ($lines) = @_;

  return 0 unless isa($lines, 'HASH');

  my $same = 1;
  # Make sure all the strings are the same.
  my $reference = undef;
  my @items = keys %{$lines};
  my $ref_item = $items[0];
  while ($same && @items) {
    my $item = shift(@items);
    if (!isa($lines->{$item}, 'ARRAY')) {
      # Something is messed up.
      $same = 0;
      next;
    }
    if (!defined($reference)) {
      $reference = join(' ', @{$lines->{$item}});
    } else {
      my $candidate = join(' ', @{$lines->{$item}});
      $same = 0 if ($reference ne $candidate);
    }
  }
  return $same ? $lines->{$ref_item} : undef;
}

sub match_var {
  # Check to see if $var is in a particular flag's list of "used in" variables
  my ($flag, $var) = @_;

  return grep { $_ eq $var } @{$flag->[0]};
}

sub merge_passlist {
  # Merge the list of phases/passes used by two flags
  my ($flaglist, $dstidx, $srcidx) = @_;
  my %passes = ();

  foreach my $flagidx ($srcidx, $dstidx) {
    for(my $i = 3; $i < @{$flaglist->[$flagidx]}; $i++) {
      $passes{join(':', @{$flaglist->[$flagidx]->[$i]})}++;
    }
  }

  # Remove the current list of phases/passes
  splice @{$flaglist->[$dstidx]}, 3;

  foreach my $phase (sort keys %passes) {
    push @{$flaglist->[$dstidx]}, [ split(/:/, $phase, 2) ];
  }
}

1;
