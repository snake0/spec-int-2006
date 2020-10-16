#
#  csv.pl - produces ASCII CSV output
#  Copyright (C) 2004-2006 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Authors:  Christopher Chan-Nui
#            Cloyce D. Spradling
#
# $Id: csv.pl 4607 2006-07-17 23:36:11Z cloyce $

use strict;

use vars qw($name $extension $synonyms);
use Text::CSV_XS;
use UNIVERSAL qw(isa);

$name      = 'CSV';
$extension = 'csv';
$synonyms  = { map { lc($_) => 1 } ($name, qw(spreadsheet)) };

my $csv_version = '$LastChangedRevision: 4607 $ '; # Make emacs happier
$csv_version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$Spec::Format::csv::non_default = 1; # You must ask for it by name
$Spec::Format::csv::part_of_all = 1; # Included in '-o all'
$::tools_versions{'csv.pl'} = $csv_version;


my $debug = 0;

sub format {
    my($me, $r, $fn) = @_;
    my (@output, @errors);
    my (%seen, $temp, $bench, $name, @values, @errmsg);
    my @nc = ();
    @nc = @{$r->{'nc'}} if ref($r->{'nc'}) eq 'ARRAY';

    my $invalid = ($r->{'invalid'} ||
		   ((ref($r->{'errors'}) eq 'ARRAY') && @{$r->{'errors'}}));
    my $csv = new Text::CSV_XS({ 'binary' => 1 });

    if ($invalid) {
	# The \# are for emacs' benefit
	push @errors, csv_string($csv, '');
	push @errors, csv_string($csv, 'ERRORS');
	push @errors, csv_string($csv, '');
	push @errors, csv_string($csv, '#' x 78);
	push @errors, csv_string($csv, sprintf("#   %-72s \#", 'INVALID RUN ' x 6));
	push @errors, csv_string($csv, sprintf("# %-74s \#", ''));

	for ($r->errors) {
	    push @errors, csv_string($csv, sprintf("# %-74s \#", $_));
	}

	push @errors, csv_string($csv, sprintf("# %-74s \#", ''));
	push @errors, csv_string($csv, sprintf("# %-74s \#", '  ' . 'INVALID RUN ' x 6));
	push @errors, csv_string($csv, '#' x 78);
    }

    # Output validity information as the first line
    push @output, csv_string($csv, 'valid', $invalid ? '0' : '1');

    # Make a list of possible tuning levels
    my @tunelist = grep { !/^base$/oi } @{$::global_config->valid_tunes};
    unshift @tunelist, 'base';

    # Now do the column headings
    my $nCopies = istrue($r->rate) ? '# Copies' : 'Ref Time';
    my $rmode = istrue ($r->rate) ? 'Rate' : 'Ratio';
    my $est = $invalid ? 'Est. ' : '';

    my @headings = ('Benchmark');
    foreach my $tune (@tunelist) {
      my $label = ucfirst($tune);
      # Do the headings
      push @headings, "$label $nCopies", "${est}$label Run Time", "${est}$label $rmode", "$label Selected", "$label Status";
    }
    push @output, csv_string($csv, @headings);

    # And do the data
    my $table    = {};
    my $results  = {};
    my %benchseen = ();
    my %tuneseen = ();

    # Scan through the list of results to figure out the maximum number of
    # iterations.  Otherwise the base holes in peak-only results cannot be
    # made.
    my $iter = 0;
    for my $bench (sort keys %{$r->{'results'}}) {
      my $benchres = $r->{'results'}{$bench};
      for my $tune (@tunelist) {
	next unless ::check_elem('ARRAY', $benchres, $tune, 'data');
	$benchres = $r->{'results'}{$bench}{$tune}{'data'};
	$iter = @{$benchres} if ($iter < @{$benchres});
      }
    }

    # Go through the benchmarks that have results.
    for my $bench (sort keys %{$r->{'results'}}) {
        next unless ::check_elem('HASH', $r, 'results', $bench);
	my $benchres = $r->{'results'}{$bench};
	my $reslist = [];
	for my $tune (@tunelist) {
	    if (::check_elem('ARRAY', $r, 'results', $bench, $tune, 'data')) {
		$benchres = $r->{'results'}{$bench}{$tune}{'data'};
	    } else {
		$benchres = [ ];
	    }
	    
	    for(my $i = 0; $i < $iter; $i++) {
		if (!defined($benchres->[$i])) {
		    push @{$reslist->[$i]}, ('') x 5;
		    next;
		}
		my $res = $benchres->[$i];
		# If we don't get here, we haven't "seen" them...
		$benchseen{$bench} = 1 unless exists $benchseen{$bench};
		$tuneseen{$tune} = 1 unless exists $tuneseen{$tune};
		if (istrue($r->rate)) {
		    push @{$reslist->[$i]}, $res->copies,    $res->reported_time, $res->ratio, $res->selected, $res->valid;
		} else {
		    push @{$reslist->[$i]}, $res->reference, $res->reported_time, $res->ratio, $res->selected, $res->valid;
		}
	    }
	}
	foreach my $res (@{$reslist}) {
	    push @output, csv_string($csv, $bench, @{$res});
	}
    }
    push @output, csv_string($csv, '');

    # Identifying information
    push @output, csv_string($csv, 'Run number:', $::global_config->accessor_nowarn('lognum'));
    push @output, csv_string($csv, '');

    # Now the rest of the stuff that nobody using CSV is interested in...

    # Now the means
    push @output, csv_string($csv, $r->baseunits, $r->basemean);
    push @output, csv_string($csv, $r->peakunits, $r->peakmean);

    # The system description
    push @output, csv_strings($csv, 'Hardware Vendor:', $r->{'hw_vendor'});
    push @output, csv_strings($csv, 'Hardware Model:',  $r->{'hw_model'});
    push @output, csv_strings($csv, 'Date tested:',     $r->test_date);

    # Do the hw_vendor/tester dance
    my ($test_sponsor, $tester) = ($r->{'test_sponsor'}, $r->{'tester'});
    $test_sponsor = $r->{'hw_vendor'} if $test_sponsor =~ /^(|--)$/;
    $tester = $r->{'test_sponsor'} if $tester =~ /^(|--)$/;

    # Note some important stuff
    push @output, csv_strings($csv, "$::suite License:" , $r->license_num);
    push @output, csv_strings($csv, 'Test sponsor:'     , $test_sponsor);
    push @output, csv_strings($csv, 'Tested by:'        , $tester);
    push @output, csv_strings($csv, 'Hardware avail:'   , $r->hw_avail);
    push @output, csv_strings($csv, 'Software avail:'   , $r->sw_avail);
    push @output, csv_string($csv, '');

    # Note the reason for NC, if any.
    if (@nc) {
	push @output, csv_string($csv, 'REASON FOR NONCOMPLIANCE');
	push @output, csv_string($csv, '---------------------------------------------------------------------------');
	foreach my $ncline (@nc) {
	    push @output, csv_string($csv, $ncline);
	}
	push @output, csv_string($csv, '---------------------------------------------------------------------------');
	push @output, csv_string($csv, '');
    }

    push @output, format_info('HARDWARE', $csv, [ $r->hardware ]);
    push @output, format_info('SOFTWARE', $csv, [ $r->software ]);
    my @notes = @{$r->notes};
    foreach my $sectionref (@notes) {
        my ($section, $notesref) = @{$sectionref};
        next unless ref($notesref) eq 'ARRAY';
        push @output, format_info($section, $csv, $notesref);
    }

    push @output, @errors;

    push @output, csv_string($csv, '-----------------------------------------------------------------------------');
    push @output, csv_string($csv, 'For questions about this result, please contact the tester.');
    push @output, csv_string($csv, 'For other inquiries, please contact webmaster@spec.org.');
    push @output, csv_string($csv, 'Copyright 2006 Standard Performance Evaluation Corporation');
    push @output, csv_string($csv, 'Generated on '.&::ctime(time)." by SPEC ${main::suite} CSV formatter v$csv_version");

    return (\@output, []);
}

sub format_info {
    my ($title, $csv, $ref) = @_;
    return () if (ref($ref) ne 'ARRAY') || !@$ref;
    my $isnotes = ($title =~ / NOTES/i);

    my @output;
    push @output, csv_string($csv, '');
    push @output, csv_string($csv, $title);
    push @output, csv_string($csv, '');
    foreach my $item (@{$ref}) {
        my ($name, @vals);
        if (ref($item) eq 'ARRAY') {
          ($name, @vals) = @$item;
        } elsif ($isnotes) {
          $name = '';
          @vals = ($item);
        } else {
          # Ignore it
          next;
        }

        if (!@vals) {
            if (!$isnotes) {
                push @output, csv_string($csv, "${name}:", $name);
            } else {
                push @output, csv_string($csv, '--');
            }
        } else {
            my $val = shift @vals;
            if (!$isnotes) {
                push @output, csv_string($csv, $name, $val);
            } else {
                push @output, csv_string($csv, $val);
            }

            while (@vals) {
                $val = shift @vals;
                if (ref $val eq '') {
                    if (!$isnotes) {
                        push @output, csv_string($csv, '', $val);
                    } else {
                        push @output, csv_string($csv, $val);
                    }
                } elsif (ref $val eq 'ARRAY') {
                    unshift (@vals, @{$val});
                }
            }
        }
    }

    return @output;
}

sub csv_string {
    my ($csv, @items) = @_;

    my $status = $csv->combine(@items);
    if ($status) {
	return $csv->string();
    } else {
	Log(0, "Error creating CSV string: ".$csv->error_input()."\n");
	return '';
    }
}

sub csv_strings {
    my ($csv, @items) = @_;
    my @things = ();

    # Things in @items might be array refs; deal with that eventuality.
    foreach my $item (@items) {
        if (isa($item, 'ARRAY')) {
            push @things, @{$item};
        } else {
            push @things, $item;
        }
    }
    my $status = $csv->combine(@things);
    if ($status) {
	return $csv->string();
    } else {
	Log(0, "Error creating CSV string: ".$csv->error_input()."\n");
	return '';
    }
}

sub center  { main::center(@_); }
sub jp { main::jp(@_); }
sub Log { main::Log(@_); }
sub istrue { main::istrue(@_); }

1;
