#
#  asc.pl - produces ASCII output
#  Copyright (C) 1999-2006 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Authors:  Christopher Chan-Nui
#            Cloyce D. Spradling
#
# $Id: asc.pl 4626 2006-07-19 20:29:42Z cloyce $

use strict;
use UNIVERSAL qw(isa);
require 'util.pl';
require 'flagutils.pl';

use vars qw($name $extension $synonyms);

$name      = 'ASCII';
$extension = 'txt';
$synonyms  = { map { lc($_) => 1 } ($name, $extension, qw(text asc)) };
my $asc_version = '$LastChangedRevision: 4626 $ '; # Make emacs happier
$asc_version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$Spec::Format::asc::part_of_all = 1;

$::tools_versions{'asc.pl'} = $asc_version;

my $debug = 0;
my %trademarks_done = ();
my %code2mark = ( 'r' => '(R)',
                  't' => '(TM)',
                  's' => '(SM)',
                );

sub format {
    my($me, $r, $fn) = @_;
    my (@output, @errors);
    my (%seen, $temp, $bench, $name, @values, @errmsg);
    my @nc = ::allof($r->{'nc'});
    $r->{'table'} = 0 if (@nc);
    my $invalid = ($r->{'invalid'} ||
		   (isa($r->{'errors'}, 'ARRAY') && @{$r->{'errors'}}));
    %trademarks_done = ();
    if ($invalid) {
	push (@errors, '#' x 78);
	push (@errors, sprintf ("# %-74s #", '  ' . 'INVALID RUN -- ' x 4 . 'INVALID RUN'));
	push (@errors, sprintf ("# %-74s #", ''));

	for ($r->errors) {
	    push (@errors, sprintf ("# %-74s #", $_));
	}

	push (@errors, sprintf ("# %-74s #", ''));
	push (@errors, sprintf ("# %-74s #", '  ' . 'INVALID RUN -- ' x 4 . 'INVALID RUN'));
	push (@errors, '#' x 78);
    }

    # Collect (possibly multi-line) info
    my @hw_vendor    = ::allof($r->hw_vendor);
    my @hw_model     = ::allof($r->hw_model);
    my @license_num  = ::allof($r->license_num);
    my @test_date    = ::allof($r->test_date);
    my @hw_avail     = ::allof($r->hw_avail);
    my @sw_avail     = ::allof($r->sw_avail);
    my @tester       = ::allof($r->{'tester'});
    my @test_sponsor = @hw_vendor;
    if (exists($r->{'test_sponsor'})) {
      @test_sponsor = ::allof($r->{'test_sponsor'});
      @test_sponsor = @hw_vendor if ($test_sponsor[0] =~ /^(|--)$/);
    }
    # Empty out the contents of tester if it's empty or if it matches the
    # contents of test_sponsor.
    if (@tester &&
        ($tester[0] =~ /^(|--)$/ || join('', @tester) eq join('', @test_sponsor))) {
        @tester = ();
    }

    my $header_string = 'SPEC ' . $r->metric .  ' Summary';
    push @output, center( fixup_trademarks($header_string) );
    if (@hw_vendor > 1 || @hw_model > 1) {
	push @output, map { center($_) } @hw_vendor;
	push @output, map { center($_) } @hw_model;
    } else {
        push @output, center($hw_vendor[0].' '.$hw_model[0]);
    }
    push @output, center( $r->datestr );
    push (@output, '');

    # Note some important stuff
    push @output, sprintf("%7s License #%-4d  Test date: %-10s   Hardware availability: %s",
			  $::suite, shift(@license_num), shift(@test_date),
                          shift(@hw_avail));
    while (@license_num || @test_date || @hw_avail) {
        push @output, sprintf("                 %-4d             %-10s                          %s",
                              shift(@license_num), shift(@test_date),
                              shift(@hw_avail));
    }

    push @output, sprintf("Test sponsor: %-31s  Software availability: %s",
			  shift(@test_sponsor), shift(@sw_avail));
    while (@test_sponsor || @sw_avail) {
        push @output, sprintf("              %-28s                         %s",
                              shift(@test_sponsor), shift(@sw_avail));
    }

    if (@tester) {
        push @output, 'Tested by: '.shift(@tester);
        push @output, map { '           '.$_ } @tester if @tester;
    }

    push (@output, '');

    # Note the reason for NC, if any.
    if (@nc) {
	push @output, '', '---------------------------------------------------------------------------', '';
	push @output, @nc;
	push @output, '', '---------------------------------------------------------------------------', '', '';
    }

    push @output, screen_format($me, $r, $fn, 1, $invalid, \@nc);

    push (@output, format_info('HARDWARE', [ $r->hardware ]));
    push (@output, format_info('SOFTWARE', [ $r->software ]));

    # Do the notes
    my @notes = @{$r->notes};
    push @output, '';
    foreach my $sectionref (@notes) {
        my ($section, $notesref) = @{$sectionref};
        next unless isa($notesref, 'ARRAY');
        push @output, '', center($section), center('-' x length($section));
        push @output, map { '    '.$_ } @{$notesref};
    }

    # These will be handy for the flags section
    my $rf = $r->{'reduced_flags'};
    return undef unless isa($rf, 'HASH');
    my @benches = sort keys %{$rf->{'benchlist'}};
    my @tunes = sort keys %{$rf->{'tunelist'}};
    my @classes = sort keys %{$rf->{'classlist'}};

    # Do the unknown and forbidden flags; they are uncollapsed, because
    # repetition is a form of emphasis.
    my $maxtitle = 0;
    foreach my $class (qw(forbidden unknown)) {
	next unless ::check_elem(undef, $rf, 'stringlist', $class);
        # Flags of the class exist for at least one benchmark, so
        # make lists of them.  They'll be formatted and output later.
	my $maxtitle = $rf->{'maxbench'} + 3; # 2 = ' ' + ': '
        my $classref = $rf->{'stringlist'}->{$class};
        for my $tune (sort ::bytune @tunes) {
          my $title_printed = 0;
          my $title = ucfirst($tune).' '.ucfirst($class).' Flags';
          for my $bench (sort keys %{$classref}) {
	    my $printed = 0;
            next unless ::check_elem('ARRAY', $classref, $bench, $tune);
            if (!$title_printed) {
                push @output, '', center($title), center('-' x length($title));
                $title_printed = 1;
            }
            push @output, dump_lines(" $bench: ", $maxtitle, $classref->{$bench}->{$tune}), '';
	  }
	}
    }

    # Do all the other flags in a way that aggregates as much as possible.
    my %class2title = ( 'compiler' => 'Compiler Invocation',
                        'portability' => 'Portability Flags',
                        'optimization' => 'Optimization Flags',
                        'other' => 'Other Flags' );
    foreach my $class (qw(compiler portability optimization other)) {
      my $mismatch = 0;
      my $printed_title = 0;
      my $onetune = $tunes[0];
      my %langstodo = map { $_ => 1 } keys %{$rf->{'langlist'}};
      my %donebench = ();

      # Go through the langs and print the ones that match.
      foreach my $lang (sort ::bylang keys %langstodo) {
        my $printed_lang = 0;

        # First dump all class flags that are common across all tuning levels
        if ($rf->{'allmatch'}->{$class} == 1 &&
          ::check_elem('HASH', $rf, 'langmatch', $class, 'alltune') &&
          ::check_elem('HASH', $rf, 'bylang', 'stringlist', $class, $onetune)) {
          if (exists($rf->{'langmatch'}->{$class}->{'alltune'}->{$lang}) &&
              $rf->{'langmatch'}->{$class}->{'alltune'}->{$lang} &&
              # There might _not_ be an entry for a particular language if, for
              # the same flag (like -DSPEC_CPU_WINDOWS) one benchmark calls
              # it portability and another calls it mandatory.  This is
              # incorrect, but it's no fault of the user.
              ::check_elem('ARRAY', $rf, 'bylang', 'stringlist', $class, $onetune, $lang) &&
              @{$rf->{'bylang'}->{'stringlist'}->{$class}->{$onetune}->{$lang}}) {
            if (!$printed_title) {
                push @output, '', center($class2title{$class}), center('-' x length($class2title{$class}));
                $printed_title = 1;
            }
            my @strings = ();
            my $flags = $rf->{'bylang'}->{'flaglist'}->{$class}->{$onetune}->{$lang};
            my $strings = $rf->{'bylang'}->{'stringlist'}->{$class}->{$onetune}->{$lang};
            for(my $i = 0; $i < @{$flags}; $i++) {
              next unless ($flags->[$i]->[2]->{'display'} || $r->{'review'});
              push @strings, $strings->[$i];
            }
            my $langtitle = $rf->{'var2desc'}->{$lang};
            if ($rf->{'langmatch'}->{$class}->{$onetune}->{$lang} == 2) {
              $langtitle .= ' (except as noted below)';
            }
            push @output, dump_lines($langtitle.': ', 5, \@strings, { 'title_alone' => 1 }), '';
            $printed_lang = 1;
            delete $langstodo{$lang};
            if (::check_elem(undef, $rf, 'bylang', 'mismatch', $class, $onetune, $lang)) {
              $mismatch += $rf->{'bylang'}->{'mismatch'}->{$class}->{$onetune}->{$lang};
            }
          }
        }

        # Do the benchmarks of $lang that matched across tuning levels
        if ($rf->{'allmatch'}->{$class} == 1 &&
            ::check_elem('HASH', $rf, 'stringlist', $class)) {
          my $classref = $rf->{'stringlist'}->{$class};
          foreach my $bench (sort keys %{$classref}) {
            next unless # the following six conditions are true:
               (
                $rf->{'langs'}->{$bench}->{$onetune} eq $lang &&
                ::check_elem(undef, $rf, 'benchmatch', $class, $bench, 'alltune') &&
                $rf->{'benchmatch'}->{$class}->{$bench}->{'alltune'} &&
                ::check_elem('ARRAY', $rf, 'flaglist', $class, $bench, $onetune) &&
                isa($rf->{'flaglist'}->{$class}->{$bench}->{$onetune}, 'ARRAY') &&
                @{$rf->{'flaglist'}->{$class}->{$bench}->{$onetune}}
               );
            if (!$printed_title) {
                push @output, '', center($class2title{$class}), center('-' x length($class2title{$class}));
                $printed_title = 1;
            }
            if (!$printed_lang) {
                push @output, $rf->{'var2desc'}->{$lang}.':', '';
                $printed_lang = 1;
            }
            my @strings = ();
            my $flags = $rf->{'flaglist'}->{$class}->{$bench}->{$onetune};
            my $strings = $classref->{$bench}->{$onetune};
            for(my $i = 0; $i < @{$flags}; $i++) {
              next unless ($flags->[$i]->[2]->{'display'} || $r->{'review'});
              push @strings, $strings->[$i];
            }
            push @output, dump_lines(" $bench: ", $rf->{'maxbench'} + 3, \@strings), '';
            if (::check_elem(undef, $rf, 'mismatch', $class, $bench, $onetune)) {
              $mismatch += $rf->{'mismatch'}->{$class}->{$bench}->{$onetune};
            }
            $donebench{$bench}++;
          }
        }
      }

      # Next dump class flags by tuning level, with the common per-language
      # set at the top, followed by benchmark-specific settings
      foreach my $tune (@tunes) {
        my $printed_tune = 0;
        my $classref = undef;
        if (::check_elem('HASH', $rf, 'bylang', 'stringlist', $class, $tune)) {
            $classref = $rf->{'bylang'}->{'stringlist'}->{$class}->{$tune};
        }
        foreach my $lang (sort ::bylang keys %langstodo) {
          my $printed_lang = 0;

          # First check for by-language list
          if (defined($classref) &&
              ::check_elem('ARRAY', $classref, $lang) &&
              @{$classref->{$lang}}) {
            if (!$printed_tune) {
              my $title = ucfirst($tune).' '.$class2title{$class};
              push @output, '', center($title), center('-' x length($title));
              $printed_tune = 1;
            }
            my @strings = ();
            my $flags = $rf->{'bylang'}->{'flaglist'}->{$class}->{$tune}->{$lang};
            for(my $i = 0; $i < @{$flags}; $i++) {
              next if (!$flags->[$i]->[2]->{'display'} && !$r->{'review'});
              push @strings, $classref->{$lang}->[$i];
            }
            my $langtitle = $rf->{'var2desc'}->{$lang};
            if ($rf->{'langmatch'}->{$class}->{$tune}->{$lang} == 2) {
              $langtitle .= ' (except as noted below)';
            }
            push @output, dump_lines($langtitle.': ', 5, \@strings, { 'title_alone' => 1 }), '';
            $printed_lang = 1;
            if (::check_elem(undef, $rf, 'bylang', 'mismatch', $class, $tune, $lang)) {
              $mismatch += $rf->{'bylang'}->{'mismatch'}->{$class}->{$tune}->{$lang};
            }
          }

          # Now do the benchmark-specific list (if any)
          if (::check_elem('HASH', $rf, 'stringlist', $class)) {
            my $classref = $rf->{'stringlist'}->{$class};
            foreach my $bench (sort keys %{$classref}) {
              next if $donebench{$bench};
              next if $rf->{'langs'}->{$bench}->{$tune} ne $lang;
              next unless ::check_elem('ARRAY', $classref, $bench, $tune);
              next unless @{$classref->{$bench}->{$tune}};
              if (!$printed_tune) {
                my $title = ucfirst($tune).' '.$class2title{$class};
                push @output, '', center($title), center('-' x length($title));
                $printed_tune = 1;
              }
              if (!$printed_lang) {
                  push @output, $rf->{'var2desc'}->{$lang}.':', '';
                  $printed_lang = 1;
              }
              my @strings = ();
              my $flags = $rf->{'flaglist'}->{$class}->{$bench}->{$tune};
              my $strings = $classref->{$bench}->{$tune};
              for(my $i = 0; $i < @{$flags}; $i++) {
                next unless ($flags->[$i]->[2]->{'display'} || $r->{'review'});
                push @strings, $strings->[$i];
              }
              push @output, dump_lines(" $bench: ", $rf->{'maxbench'} + 3, \@strings), '';
              if (::check_elem(undef, $rf, 'mismatch', $class, $bench, $tune)) {
                $mismatch += $rf->{'mismatch'}->{$class}->{$bench}->{$tune};
              }
            }
          }
        }
        if ($mismatch) {
          if ($class eq 'optimization') {
            push @output, '(*) Indicates optimization flags found in portability variables';
          } elsif ($class eq 'portability') {
            push @output, '(*) Indicates portability flags found in non-portability variables';
          } elsif ($class eq 'compiler') {
            push @output, '(*) Indicates compiler flags found in non-compiler variables';
          }
        }
        $mismatch = 0;
      }
    }

    if (defined($::website_formatter) && $::website_formatter &&
        defined($r->{'flagsurl'}) && $r->{'flagsurl'} ne '') {
      my $url = $r->{'flagsurl'};
      $url =~ s/\.xml$/\.html/;
      push @output, '', "The full text of the flags file used for this result is available at", $url;
    }

    push @output, '';

    push @output, footer();

    push @output, @errors;
    unshift @output, @errors;
    push @output, '-----------------------------------------------------------------------------';
    push @output, 'For questions about this result, please contact the tester.';
    push @output, 'For other inquiries, please contact webmaster@spec.org.';
    push @output, 'Copyright 2006 Standard Performance Evaluation Corporation';
    push @output, 'Generated on '.&::ctime(time)." by SPEC ${main::suite} ASCII formatter v$asc_version";

    return (\@output, []);
}

sub format_table {
    my ($table, $timelog, $ratiolog, $resultobj, @nc) = @_;
    my @rc;
    for my $benchname (sort keys %$table) {
	my $tr = $table->{$benchname};
	my $array = { 'base' => [], 'peak' => [] };
	$array->{'base'} = [@{$tr->{'base'}}] if isa($tr->{'base'}, 'ARRAY');
	$array->{'peak'} = [@{$tr->{'peak'}}] if isa($tr->{'peak'}, 'ARRAY');
	my ($base, $peak);

	while (@{$array->{'base'}} || @{$array->{'peak'}}) {
	    my $line = sprintf '%-14s ', $benchname;
	    for my $tune (qw(base peak)) {
		my $ref = $array->{$tune};
		if (@$ref) {
		    my ($first, $time, $ratio, $selected, $invalid) = @{shift @$ref};
		    $first = sprintf('%d', $first) if ($first + 0 > 0);
		    $time  = significant(8,3,$timelog->{$tune},$time, @nc) if ($time + 0 > 0);
		    $ratio = significant(8,3,$ratiolog->{$tune},$ratio, @nc) if ($ratio + 0 > 0);
		    my $selectchar = $invalid;
		    if ($resultobj->size eq 'ref') {
			$selectchar = $selected ? '*' : $invalid;
		    }
		    if ($invalid ne 'S') {
			$selectchar = ($invalid == 1) ? 'NR' : $invalid;
			$ratio = '';
		    }
		    $selectchar = ' ' if (@nc);
		    $line .= sprintf '%6s  %9s  %9s %-2s ',  
				$first, $time, $ratio, 
				$selectchar;
		} else {
		    $line .= ' ' x 32;
		}
	    }
	    push (@rc, $line);
	}
    }
    return @rc;
}

sub screen_format {
    # This does the screen format, which is really just the summary table
    my ($me, $r, $fn, $isasc, $invalid, $nc) = @_;

    my @output = ();
    my $what_ = ' Ref. ';
    my $rmode = ' Ratio';
    if (istrue($r->rate)) {
       $what_ = 'Copies';
       $rmode = ' Rate ';
    }

    push (@output,
    '                                  Estimated                       Estimated'
    ) if $invalid;
    push (@output,
    '                Base     Base       Base        Peak     Peak       Peak',
    "Benchmarks     $what_  Run Time    $rmode      $what_  Run Time    $rmode",
    '-------------- ------  ---------  ---------    ------  ---------  ---------');

    my $table    = {};
    my $results  = {};
    my $smallest = { 'base' => { 'time' => 2147483647, # Mr. Big Number
				 'ratio' => 2147483647 }, # Mr. Big Number
                     'peak' => { 'time' => 2147483647, # Mr. Big Number
				 'ratio' => 2147483647 } # Mr. Big Number
		 };
    my %benchseen = ();
    my %tuneseen = ();

    # Go through the benchmarks that have results.  We'll catch the missed
    # ones afterward.
    for my $bench (sort keys %{$r->{'results'}}) {
	my $benchres = $r->{'results'}{$bench};
	for my $tune (sort keys %{$r->{'results'}{$bench}}) {
	    $benchres = $r->{'results'}{$bench}{$tune}{'data'};
	    my $tmp;
	    for my $res (@{$benchres}) {
		# If we don't get here, we haven't "seen" them...
		$benchseen{$bench} = 1 unless exists $benchseen{$bench};
		$tuneseen{$tune} = 1 unless exists $benchseen{$tune};
		if (istrue($r->rate)) {
		    $tmp = [ $res->copies,    $res->reported_time, $res->ratio, $res->selected, $res->valid ];
		} else {
		    $tmp = [ $res->reference, $res->reported_time, $res->ratio, $res->selected, $res->valid ];
		}
		print "\@tmp = (",join(', ', @{$tmp}),")\n" if ($debug & 2);
		if ($tmp->[1] && ($smallest->{$tune}{'time'}  > $tmp->[1])) {
		    $smallest->{$tune}{'time'}  = $tmp->[1];
		}
		if ($tmp->[2] && ($smallest->{$tune}{'ratio'} > $tmp->[2])) {
		    $smallest->{$tune}{'ratio'} = $tmp->[2];
		}
		if ($debug & 2) {
		    print "smallest->{$tune}{time} = ".$smallest->{$tune}{'time'}."\n";
		    print "smallest->{$tune}{ratio} = ".$smallest->{$tune}{'ratio'}."\n";
		}
		push (@{$table->{$bench}{$tune}}, $tmp);
		if ($res->selected) {
		    $tmp->[3] = '*' if ($tmp->[4] eq 'S' && $tmp->[3] ne 'X');
		    push (@{$results->{$bench}{$tune}}, $tmp);
		    $benchseen{$bench} = 'selected';
		}
	    }
	}
    }
    for my $bench (sort keys %{$r->benchmarks}) {
	next if (exists($benchseen{$bench}) &&
		 ($benchseen{$bench} eq 'selected'));
	for my $tune (sort keys %tuneseen) {
	    my $tmp = [ '', '', '', '', 'NR' ];
	    push (@{$table->{$bench}{$tune}}, $tmp) unless $benchseen{$bench};
	    push (@{$results->{$bench}{$tune}}, $tmp);
	}
    }
    my $timelog = { 'base' => $smallest->{'base'}{'time'},
		    'peak' => $smallest->{'peak'}{'time'} };
    my $ratiolog = { 'base' => $smallest->{'base'}{'ratio'},
		     'peak' => $smallest->{'peak'}{'ratio'} };
    $timelog->{'base'}  = log($timelog->{'base'})/log(10) if ($timelog->{'base'} > 0);
    $ratiolog->{'base'} = log($ratiolog->{'base'})/log(10) if ($ratiolog->{'base'} > 0);
    $timelog->{'peak'}  = log($timelog->{'peak'})/log(10) if ($timelog->{'peak'} > 0);
    $ratiolog->{'peak'} = log($ratiolog->{'peak'})/log(10) if ($ratiolog->{'peak'} > 0);
    print "\@timelog=($timelog->{'base'}, $timelog->{'peak'})  \@ratiolog=($ratiolog->{'base'}, $ratiolog->{'peak'})\n" if $debug;


    if ($isasc && $r->table) {
	push (@output, format_table($table, $timelog, $ratiolog, $r, @{$nc}));
	push (@output, '=' x 78);
    }
    push (@output, format_table($results, $timelog, $ratiolog, $r, @{$nc}));

    my $est;
    $est = 'Est. ' if ($invalid);

    my $basemean = $r->basemean;
    if ($basemean =~ /\d/) {
      $basemean = significant(8, 3, $ratiolog->{'base'}, $r->basemean, @{$nc});
    }
    push @output, sprintf (" %-34s%8s", 
                           $est . fixup_trademarks($r->baseunits), $basemean);

    my $peakmean = $r->peakmean;
    if ($peakmean =~ /\d/) {
      $peakmean = significant(8, 3, $ratiolog->{'peak'}, $r->peakmean, @{$nc});
    }
    push @output, sprintf (" %-34s%40s",
                           $est . fixup_trademarks($r->peakunits), $peakmean);

    return @output;
}

sub format_info {
    my ($title, $ref) = @_;
    return () if !@$ref;

    my @output;
    push (@output, '', '', center($title), center('-' x length($title)));
    for my $item (@{$ref}) {
	my ($name, @vals) = @$item;
	if (!@vals) {
	    push (@output, sprintf ('%20.20s: --', $name));
	} else {
	    my $val = shift @vals;
	    push (@output, sprintf ('%20.20s: %s', $name, $val));

	    while (@vals) {
		$val = shift @vals;
		if (ref($val) eq '') {
		    push (@output, sprintf ('%20.20s  %s', '', $val));
		} elsif (::isa($val, 'ARRAY')) {
		    unshift @vals, @{$val};
		}
	    }
	}
    }
    return @output;
}

sub dump_lines {
    my ($title, $len, $strings, $opts) = @_;
    my @output = ();
    my $line = '';
    my $printed = 0;
    $opts = {} unless isa($opts, 'HASH');
    return () unless isa($strings, 'ARRAY') && @$strings;

    if ($opts->{'title_alone'}) {
      $printed = 1;
      push @output, $title;
    }

    foreach my $string (@{$strings}) {
	if ($line eq '') {
	    $line = $string;
	} elsif (length($line.', '.$string) + $len > 78) {
	    push @output, sprintf "%*s%s", $len, ($printed) ? '' : $title, $line;
	    $printed++;
	    $line = $string;
	} else {
            if (0) {
              # No commas; too "confusing"
              $line .= ", $string";
            } else {
              $line .= " $string";
            }
	}
    }
    if ($line ne '') {
	push @output, sprintf "%*s%s", $len, ($printed) ? '' : $title, $line;
    }

    return @output;
}

sub footer {
  return ::trademark_lines('  ', %trademarks_done);
}

# Look in the input string for trademarks and mark them up as appropriate.
# Also keep track of which ones were used so that they can be mentioned in
# the result page footer.
sub fixup_trademarks {
  my ($str) = @_;

  foreach my $tm (sort { length($a) <=> length($b) } keys %::trademarks) {
    next if exists($trademarks_done{$tm});
    if ($str =~ /\b${tm}((?=[^a-zA-Z])|\b)/) {
        $trademarks_done{$tm}++;
        $str =~ s/\b${tm}((?=[^a-zA-Z])|\b)/${tm}$code2mark{$::trademarks{$tm}}/;
    }
  }

  return $str;
}

# significant -- format a floating point value into N significant digits
# width of text string to return
# log of minimum output (how many decimal places do we want)
# log of smallest overall value (how many decimals will we be using for sure)
# value to format
sub significant {
    my ($width, $min_log, $low_log, $value, @nc) = @_;
    print "significant($width, $min_log, $low_log, $value, $debug)\n" if ($debug & 4);
    my ($real_dp, $wanted_dp, $dp, $space, $log);

    return 'NC' if (@nc);
    if ($value == 0) {
	if ($value eq '0' || $value !~ m/^\s*(\+|-)?[0-9.eE+]/) {
	    print "Returning '0.00'\n" if ($debug & 4);
	    return '0.00';
	}
	$log = 0;
    } else {
	$log = &floor(log($value)/log(10)); 
    }
    $min_log--;
    print "  log=$log  min_log=$min_log\n" if ($debug & 4);
    if ($log > $min_log) {
        $value = int($value / (10**($log-$min_log))+.5) * (10**($log-$min_log));
    }
    print "  value=$value\n" if ($debug & 4);
    $dp        = ($low_log>=$min_log)?0:3-$low_log;
    $wanted_dp = ($log>=$min_log)?0:$min_log-$log;
    print "  dp=$dp   wanted_dp=$wanted_dp\n" if ($debug & 4);
    if ($dp > $wanted_dp) {
	$space = $dp - $wanted_dp;
	$real_dp = $wanted_dp;
    } else {
	$space = 0;
	$real_dp = $dp;
    }
    if ($real_dp == 0 && $dp > 0) {
	$space++;
    }
    print "  space=$space  real_dp=$real_dp\n" if ($debug & 4);
    my $retval = sprintf('%*.*f%s', $width-$space, $real_dp, $value, ' ' x $space);
    print "sprintf('%*.*f%s', \$width-\$space, \$real_dp, \$value, ' ' x \$space) =\n" if ($debug & 4);
    print "  sprintf('%*.*f%s', ".($width-$space).", $real_dp, $value, '".(' ' x $space)."') =\n" if ($debug & 4);
    print "    '$retval'\n" if ($debug & 4);
    return $retval;
}

sub floor {
    my ($temp) = @_;
    my $inttemp = int($temp);
    if ($temp != $inttemp) { #  This is a bad test.
	if ($temp > 0) {
	    $temp = $inttemp;
	} else {
	    $temp = $inttemp-1;
	}
    }
    return $temp;
}

sub center  { main::center(@_); }
sub jp { main::jp(@_); }
sub istrue { main::istrue(@_); }

1;
