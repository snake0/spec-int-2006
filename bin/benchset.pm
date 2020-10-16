#
# benchset.pm
# Copyright (C) 1999-2006 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: benchset.pm 4603 2006-07-17 16:16:52Z cloyce $
#

package Spec::Benchset;
use strict;
use UNIVERSAL qw(isa);
use vars '@ISA';

@ISA = (qw(Spec::Config));

my $version = '$LastChangedRevision: 4603 $ '; # Make emacs happier
$version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'benchset.pm'} = $version;

# Information about what's in the config entries
## these tables are used to give field names to config entries in the report
##               field name     report label         ??
use vars qw(@hardware_info @software_info @extra_info %empty_fields);

# All of the hardware_info items are printed
# Don't forget to update $::info_re (if necessary) to match any new fields
# added!
@hardware_info=(
#    Field name      Field label        Display in detail box?
    ['hw_vendor'   , 'Hardware Vendor'    , 0 ],
    ['hw_model'    , 'Model Name'         , 0 ],
    ['hw_cpu_name' , 'CPU Name'           , 1 ],
    ['hw_cpu_char' , 'CPU Characteristics', 1 ],
    ['hw_cpu_mhz'  , 'CPU MHz'            , 1 ],
    ['hw_fpu'      , 'FPU'                , 1 ],
    ['hw_ncpu'     , 'CPU(s) enabled'     , 1 ],
    ['hw_ncpuorder', 'CPU(s) orderable'   , 1 ],
    ['hw_pcache'   , 'Primary Cache'      , 1 ],
    ['hw_scache'   , 'Secondary Cache'    , 1 ],
    ['hw_tcache'   , 'L3 Cache'           , 1 ],
    ['hw_ocache'   , 'Other Cache'        , 1 ],
    ['hw_memory'   , 'Memory'             , 1 ],
    ['hw_disk'     , 'Disk Subsystem'     , 1 ],
    ['hw_other'    , 'Other Hardware'     , 1 ],
);

# All of the software_info items are printed
# Don't forget to update $::info_re (if necessary) to match any new fields
# added!
@software_info=(
#    Field name           Field label        Display in detail box?
    ['sw_os'            , 'Operating System', 1 ],
    ['sw_compiler'      , 'Compiler'        , 1 ],
    ['sw_auto_parallel' , 'Auto Parallel'   , 1 ],
    ['sw_file'          , 'File System'     , 1 ],
    ['sw_state'         , 'System State'    , 1 ],
    ['sw_base_ptrsize'  , 'Base Pointers'   , 1 ],
    ['sw_peak_ptrsize'  , 'Peak Pointers'   , 1 ],
    ['sw_other'         , 'Other Software'  , 1 ],
);

# Only *some* of the extra_info items are printed, and only some of the time!
# Don't forget to update $::info_re (if necessary) to match any new fields
# added!
@extra_info = (
	       [ 'test_sponsor', 	'Test Sponsor'   ], # Submits the result
	       [ 'license_num', 	'License Number' ],
	       [ 'tester',      	'Tested by'      ], # Runs the test
	       [ 'test_date', 	        'Test Date'      ],
	       [ 'hw_avail', 	        'Hardware Avail' ],
	       [ 'sw_avail', 	        'Software Avail' ],
	       [ 'prepared_by',         'Preparer Name' ],
);

# The various different notes sections, and the order in which they appear
# Note that the name MUST begin with 'notes', or all sorts of other things
# will break.
@::notes_info = (
    [ 'notes_comp',	'Compiler Invocation Notes'	        	],
    [ 'notes_port',	'Portability Notes'		        	],
    [ 'notes_base',	'Base Tuning Notes'		        	],
    [ 'notes_peak',	'Peak Tuning Notes'		        	],
    [ 'notes_os',	'Operating System Notes'	        	],
    [ 'notes_plat',	'Platform Notes'		        	],
    [ 'notes_part',	'Component Notes'		        	],
    [ 'notes',		'General Notes', qr/^(notes)_*()([0-9]*[0-9])$/io ],
);

# Fill in the regexps for the ones that don't already have it
foreach my $notes_info_ref (@::notes_info) {
    # Make the regexps for them ahead of time
    next unless ref($notes_info_ref) eq 'ARRAY';
    next if defined($notes_info_ref->[2]);
    my $tag = $notes_info_ref->[0];
    $notes_info_ref->[2] = qr/^(${tag})_*(.*?)_*([0-9]*[0-9])$/i;
}

@::notes_regexps = map { $_->[2] } @::notes_info;
@::field_order = sort map { $_->[0] } (@hardware_info, @software_info, @extra_info);
$::info_re = qr/^(?:hw_|sw_|license_num|test|prepared|notes|flagsurl)/o;

# Fields which are allowed to be empty (i.e. will not be set to '--')
%empty_fields = ('hw_cpu_char' => 1);

# Fields which are generated, and should therefore not be output to raw or
# back-propagated into config files.
%::generated_fields = ('hw_ncpu' => 1);

sub new {
    my ($class, $config) = @_;
    my $me       = bless {}, $class;

    no strict 'refs';
    $me->{'name'}           = ${"${class}::name"};
    $me->{'units'}          = ${"${class}::units"};
    $me->{'metric'}         = ${"${class}::metric"};
    $me->{'benchmarklist'}  = [@{"${class}::benchmarks"}];
    $me->{'output'}         = defined(${"${class}::output"}) ? ${"${class}::output"} : 0;
    $me->{'no_output'}      = defined(${"${class}::no_output"}) ? ${"${class}::no_output"} : {};
    $me->{'mach'} = 'default';
    $me->{'ext'}  = 'default';
    $me->{'size'} = 'default';
    $me->{'rate'} = 0;
    $me->{'time'} = time;
    $me->{'mean_anyway'} = istrue($config->mean_anyway);
    $me->{'errors'} = [];
    $me->{'calc_errors'} = [];
    $me->{'config'} = $config;
    $me->{'review'} = $config->review;
    $me->{'valid'} = 'X';

    my $tmp = {};
    for (@{$me->{'benchmarklist'}}) {
	$tmp->{$_}++;
    }
    $me->{'benchmarks'}     = $tmp;
    $me->{'refs'} = [ $me ];

    return $me;
}

sub info_format {
    my ($me, @format) = @_;
    my @rc = ();
    for my $format (@format) {
	next if (ref($format) ne 'ARRAY');
	if (ref($format[0]) eq 'ARRAY') {
	    for my $ref (grep { defined($_->[2]) ? $_->[2] : 1 } @$format) {
		my @keys  = $me->match_keys($ref->[0]);
		my @field = ($ref->[1]);
		if (@keys) {
		    for my $key (@keys) {
			my $val = $me->accessor($key);
			my @vals = ($val);
			@vals = grep { defined } @$val if ::isa($val, 'ARRAY');
			for $val (@vals) {
			    if ($val =~ /^\s*$/o) {
				# It contains only whitespace, which would
				# get stomped by the split.
				push @field, ' ';
			    } else {
				push @field, split(/\n/, $val);
			    }
			}
		    }
		} else {
		    push @field, exists($empty_fields{$ref->[0]}) ? '' : '--';
		}
		push (@rc, [@field]);
	    }
	} elsif (!defined($format->[2]) ||
                 (defined($format->[2]) && $format->[2])) {
	    my @keys  = $me->match_keys($format->[0]);
	    my @field = ($format->[1]);
	    if (@keys) {
		for my $key (@keys) {
		    push (@field, split(/\n/, $me->accessor($key)));
		}
	    } else {
		push @field, exists($empty_fields{$format->[0]}) ? '' : '--';
	    }
	    push (@rc, [@field]);
	}
    }
    return @rc;
}

sub hardware {
    my ($me) = @_;
    return $me->info_format(\@hardware_info);
}

sub software {
    my ($me) = @_;
    return $me->info_format(\@software_info);
}

sub notes {
    my ($me) = @_;

    # What a pain these notes are!
    # Okay, each notes item will be a hash with arbitrary keys.  Each of
    # the items in the hash is an array, just like the old notes used to
    # be.  The notes items will be presented in order of appearance in the
    # array.  The arrays (values in the hashes) will be processed in the
    # lexical order of the arbitrary keys in the hashes.

    my @notes = ();
    foreach my $sectionref (@::notes_info) {
        my $notesref = $me->notes_section($sectionref->[0]);
        push @notes, [ $sectionref->[1], $notesref ] if (@{$notesref});
    }
    return \@notes;
}

sub notes_section {
    my ($me, $section) = @_;
    my @notes;

    # See the comments for 'notes' above.

    my $notesref = $me->accessor_nowarn($section);
    return \@notes unless ref($notesref) eq 'HASH';
    for my $key (sort keys %{$notesref}) {
        next unless ref($notesref->{$key}) eq 'ARRAY';
        for my $note (@{$notesref->{$key}}) {
            if (ref($note) eq 'ARRAY') {
                push @notes, $note->[1];
            } else {
                push @notes, $note;
            }
        }
    }
    return \@notes;
}

sub baseunits {
    my ($me) = shift;
    my $rate = $me->rate?'_rate':'';
    return $me->units . $rate . '_base' . $::year;
}

sub peakunits {
    my ($me) = shift;
    my $rate = $me->rate?'_rate':'';
    return $me->units . $rate. $::year;
}

sub datestr {
    my ($me) = shift;
    my $tmp = main::ctime($me->{'time'});
    $tmp =~ tr/\015\012//d;
    return $tmp;
}

sub errors {
    my ($me) = shift;
    my @errors;

    push (@errors, @{$me->{'errors'}}) if ref $me->{'errors'} eq 'ARRAY';

    my $ref = $me->{'results'};
    for my $bench (sort keys %$ref) {
	for my $tune (sort keys %{$ref->{$bench}}) {
	    next if ref($ref->{$bench}{$tune}{'data'}) ne 'ARRAY';
	    for my $res (@{$ref->{$bench}{$tune}{'data'}}) {
		for (@{$res->{'errors'}}) {
		    push (@errors, "Error $bench: $_");
		}
	    }
	}
    }

    grep (s/\s+$//,@errors);

    return @errors;
}

sub which_tunes {
    my ($me) = shift;
    my %tunes;
    my $ref = $me->{'results'};
    for my $bench (sort keys %$ref) {
	for my $tune (sort keys %{$ref->{$bench}}) {
	    $tunes{$tune} = 1;
	}
    }
    return sort ::bytune keys %tunes;
}

sub report {
    my ($me, $benchobjs, $config, $mach, $ext, $size) = @_;
    my $found_one = 0;
    my $result;
    my @bm_list = ();
    # If $config->rawconfig exists, and rawformat is set, assume that it's
    # already been properly munged, and un-munge it.
    my $rawconfig = $config->accessor_nowarn('rawconfig');
    my $txtconfig = '';
    my ($junk1, $junk2);
    if (defined($rawconfig) && $config->rawformat) {
	$rawconfig = join("\n", @{$config->rawconfig});
	$txtconfig = ::decode_decompress($rawconfig);
    } else {
	$txtconfig = $config->rawtxtconfig;
	$rawconfig = ::compress_encode($txtconfig);
    }

    # Make sure that the result gets a copy of flags text and flagsinfo from
    # the config.
    my $flags = '';
    if (exists($config->{'flags'})
        && defined($config->accessor_nowarn('flags'))) {
	$flags = ::compress_encode($config->flags);
	$flags = main::encode_base64($config->flags) if !defined($flags);
    }
    
    $result = bless { 
	'mach'        => '',
	'ext'         => '',
	'size'        => '',
	'rate'        => istrue($config->rate),
	'txtconfig'   => [ split ("\n", $txtconfig, -1) ],
	'rawconfig'   => [ split ("\n", $rawconfig) ],
	'rawflags'    => [ split ("\n", $flags) ],
	'flaginfo'    => $config->accessor_nowarn('flaginfo'),
	'flagsurl'    => $config->accessor_nowarn('flagsurl'),
	'table'       => $me->config->table,
	'name'        => $me->name,
	'units'       => $me->units,
	'metric'      => $me->metric,
	'mean_anyway' => $me->mean_anyway,
	'tunelist'    => $me->config->tunelist,
	'basepeak'    => $config->{'basepeak'},
        'base_copies' => $config->{'copies'},
        'review'      => $config->review,
    }, ref($me);
    # Make a list of keys that must be dumped in the raw file.
    # Without this list, setting one of these things in the config file
    # could cause it to not be dumped in the raw file.
    $result->{'do_dump'} = [ qw(mach ext size rate name units metric
                                tunelist basepeak base_copies) ];

    $config = $me->config;	# This probably isn't necessary
    $result->{'refs'} = [ $result ];
    $result->{'valid'} = 'S';
    $result->mach ($mach) if defined $mach;
    $result->ext  ($ext)  if defined $ext;
    # $size should always be defined.  But just in case...
    if (defined $size) {
      $result->size($size);
    } else {
      $result->size($me->size);
    }

    # Weed out benchmarks that should not be output
    foreach my $bm (keys %{$me->benchmarks}) {
        next if exists($me->{'no_output'}->{$bm});
        push @bm_list, $bm;
        $result->{'benchmarks'}->{$bm} = $me->benchmarks->{$bm};
    }

    for (@bm_list) {
	my $bm =$me->{'benchmarks'}->{$_};
        if (!defined($bm) || !ref($bm)) {
            # The benchmark in question was never actually instantiated.
            # Its entry should not be deleted, because the benchset _DID_
            # call for it.  So insert a bogus ref time for it; the whole
            # result will be invalid, because it was _definitely_ not run.
            $result->{'reference'}->{$_} = 1;
        } else {
            $result->{'reference'}->{$_} = $bm->reference;
        }
    }

    # Now copy benchmark information into object
    for my $bench (@$benchobjs) {
	next if !$result->bench_in($bench);
	$found_one = 1;
	$result->add_results($bench, $config);
    }
    return undef unless $found_one;

    # Setting this allows us to access all settings for all benchmarks from
    # the top level.  Normally this would be a problem (for basepeak, etc),
    # but since we're only interested in the text things, it should be okay
    $config->{'refs'} = [
			 reverse ($config,
				  $config->ref_tree('',
						    ['default', $me->name, 
						     sort @bm_list],
						    ['default', @{$config->valid_tunes} ],
						    ['default', $ext],
						    ['default', $mach])) ];

    # Grab the mail settings...
    $result->{'mail'} = {};
    foreach my $key (qw(mailto mailmethod mailserver mailport username
			lognum logname sendmail mail_reports)) {
	$result->{'mail'}->{$key} = $config->accessor_nowarn($key);
    }
    $result->{'mail'}->{'compress'} = istrue($config->accessor_nowarn('mailcompress'));

    # If different graph settings have been specified, copy them in
    foreach my $what (qw(graph_min graph_max graph_auto)) {
      if (defined($config->accessor_nowarn($what))) {
        $result->{$what} = $config->accessor_nowarn($what);
      }
    }

    # Assemble the hw_ncpu field from the various components
    $result->{'hw_ncpu'} = ::assemble_cpu_description($config);

    # Copy text data into object
    my @keys = sort ::bytag $config->list_keys;

    # Get all the config file indices into the result object.
    foreach my $idx (grep { /^cfidx_/ } @keys) {
	$result->{$idx} = $config->accessor($idx);
    }

    # There are some things that may be set in the config file
    # and also on the command line.  The raw file will reflect the value
    # that is actually used; the config file should be modified accordingly.
    foreach my $item (qw(flagsurl)) {
      my $text = $config->accessor_nowarn($item);
      $text = '' unless defined($text);
      my $oldtag = exists($result->{'cfidx_'.$item}) ? $item : undef;
      if (!::update_stored_config($result, $oldtag, $item, $text, 1, 1)) {
        ::Log(0, "WARNING: Could not update actual value used for \"$item\" in\n          stored config file\n");
      }
    }

    for my $tagref (
          # These are all the things that have a field on the report
          @hardware_info, @software_info, @extra_info,
          map { [ $_ ] } qw(hw_nchips hw_ncores hw_ncoresperchip hw_nthreadspercore)) {
	my $tag = $tagref->[0];
        next if exists($::generated_fields{$tag});      # Already done
	my @data = ();
	for my $key (grep { m/^$tag\d*$/ } @keys) {
            my $index = $result->{'cfidx_'.$key} if exists($result->{'cfidx_'.$key});
            my $val = $config->accessor($key);
            if ($config->info_wrap_columns == 0 ||
                length($val) <= $config->info_wrap_columns) {
                push @data, [ $val, $index, $key ];
            } else {
                ::Log(0, "NOTICE: $key is longer than ".$config->info_wrap_columns." characters and will be split\n");
                my @newlines = ::wrap_lines([$val], $config->info_wrap_columns);
                push @data, [ shift(@newlines), $index, $key, 1 ];
                foreach my $line (@newlines) {
                    push @data, [ $line, $index+1, undef, 1 ];
                }
            }
	}
        @data = ([ exists($empty_fields{$tag}) ? '' : '--', undef, $tag, 0 ]) unless @data;
	if (@data > 1 || $data[0]->[2] ne $tag) {
	    $result->{$tag} = [ ];
            # Go through @data twice.
            my @redo = ();
            # This first pass is to rename/change lines that are pre-existing
	    for (my $i = $#data; $i >= 0; $i--) {
		my ($text, $index, $key, $doupdate) = @{$data[$i]};
		$result->{$tag}->[$i] = $text;
		if (defined($key) && $key ne '') {
                    my $newtag;
                    if (@data > 1) {
                        # There's more than one line, so newtag should have an
                        # index.
                        $newtag = sprintf '%s%03d', $tag, $i;
                    } else {
                        # There's only one line, but it was numbered in the
                        # config file, so we'll be rewriting it to its
                        # non-indexed value.
                        $newtag = $tag;
                    }
                    if ($key ne $newtag &&
                        exists($result->{'cfidx_'.$newtag})) {
                      # Oops... that spot is taken.  Tweak the new tag a bit
                      # to avoid the clash, and put it on the list for
                      # another pass later.
                      push @redo, [ $newtag, $text ];
                      $newtag =~ s/(.)/chr(ord($1) | 0x80)/goe;
                    }
		    $result->{'cfidx_'.$key} = $index if defined($index);
		    if (!::update_stored_config($result, $key, $newtag, $text, $doupdate, 1)) {
			::Log(0, "ERROR: Could not update tag name in stored config file for $tag\n");
		    }
		}
	    }
            # This is not a second pass through @data; some tags may need to
            # be fixed up, if the config file author crafted her config file
            # just right.  See ConfigRewritingWarnings for details.
            if (@redo) {
              foreach my $redoref (@redo) {
                next unless ::isa($redoref, 'ARRAY');
                my ($newtag, $text) = @{$redoref};
                my $oldtag = $newtag;
                $oldtag =~ s/(.)/chr(ord($1) | 0x80)/goe;
                if (!::update_stored_config($result, $oldtag, $newtag, $text, 0, 1)) {
                    ::Log(0, "ERROR: Could not update tag name in stored config file for $tag\n");
                }
              }
            }
            # This second pass is to add lines that are newly generated
	    for (my $i = $#data; $i >= 0; $i--) {
		my ($text, $index, $key, $doupdate) = @{$data[$i]};
		if ((defined($doupdate) && $doupdate) &&
                    (!defined($key) || $key eq '')) {
		    my $newtag = sprintf '%s%03d:%d', $tag, $i, $index;
		    if (!::update_stored_config($result, undef, $newtag, $text, $doupdate, 1)) {
			::Log(0, "ERROR: Could not add new line in stored config file for $tag\n");
		    }
		}
	    }
	} else {
	    my ($text, $index) = @{$data[0]};
	    $result->{$tag} = $text;
	    $result->{'cfidx_'.$tag} = $index if (defined($index) && $index)
	}
    }

    # Notes are a little special.

    # The array refs initially put into $result->{notes*} have three elements:
    # 0. Whether the text line should be edited (1) or just the tag (0)
    # 1. The original tag
    # 2. The note
    # The tag can be undef, in which case a new one needs to be generated
    # and a line inserted into the config file.
    my $safe;
    foreach my $sectionref (@::notes_info) {
        next unless ref($sectionref) eq 'ARRAY';
        my $note_tag = $sectionref->[0];
        my $notere   = $sectionref->[2];
        $result->{$note_tag} = {};
        foreach my $tag (sort ::bytag @keys) {
            next unless $tag =~ m/$notere/;
            my ($key, $idx) = ($2, $3 + 0);
            my $val = $config->accessor($tag);
            if (istrue($config->expand_notes)) {
                ($val, $safe) = ::command_expand($val, $safe, $config);
            }
            $result->{$note_tag}->{$key}->[$idx] = [ 0, $tag, $val ];
        }
        # Now squeeze undefs from all the hashes
        foreach my $key (keys %{$result->{$note_tag}}) {
            my $notesref = $result->{$note_tag}->{$key};
            if (ref($notesref) eq 'ARRAY') {
                ::squeeze_undef($notesref);
            } else {
                # This should never happen
                delete $result->{$note_tag}->{$key};
                next;
            }

            # Run through the notes and make sure that none are multi-line.
            for (my $i = 0; $i < @{$notesref}; $i++) {
                my (undef, $oldtag, $text) = @{$notesref->[$i]};
                if ($text =~ /[\r\n]/) {
                    my @lines = split(/(?:\r\n|\n)/, $text, -1);
                    my $cfline = $result->{'cfidx_'.$oldtag};
                    my @newlines = ([ 1, $oldtag, shift(@lines) ]);
                    my $blockquote = 0;
                    my $endtag = '';
                    if (defined($cfline)) {
                        # Check to see if it's the start of a block quote.
                        # If it is, two more lines will have to be hacked
                        # off after the loop.
                        if ($result->{'txtconfig'}->[$cfline] =~ /^\s*$oldtag\s*=\s*<<(\S+)\s*$/) {
                            $endtag = $1;
                            $blockquote = 1;
                        }
                    }
                    # Make an array where each position indicates whether a
                    # particular config file line has an item associated
                    # with it.  It's almost a reverse mapping of the cfidx_*
                    # values.
                    my $used_cflines = [];
                    map { $used_cflines->[$_] = 1 } map { $result->{$_} } grep { /cfidx_/ } keys %{$result};
                    foreach my $newline (@lines) {
                        push @newlines, [ 0, undef, $newline ];
                        # If this newly-split line is indeed a part of the
                        # config file, then it needs to be excised.
                        if (defined($cfline) &&
                            ($blockquote ||
                             ($result->{'txtconfig'}->[$cfline + 1] =~ $newline
                              && !defined($used_cflines->[$cfline + 1])))) {
                            # Remove the extra line from the config file
                            splice @{$result->{'txtconfig'}}, $cfline + 1, 1;
                            # Adjust the indices
                            ::shift_indices($result, $cfline + 1, -1);
                            $used_cflines = [];
                            map { $used_cflines->[$_] = 1 } map { $result->{$_} } grep { /cfidx_/ } keys %{$result};
                        }
                    }
                    if ($blockquote) {
                        # Hack off two more lines @ $cfline.  Check to make
                        # sure that the second of the two contains the end tag.
                        if ($result->{'txtconfig'}->[$cfline + 2] ne $endtag) {
                            ::Log(0, "WARNING: Block quote end tag \"$endtag\" not found while rewriting\n  config file lines.  Please report this bug to ${main::lcsuite}support\@spec.org!\n");
                        } else {
                            splice @{$result->{'txtconfig'}}, $cfline + 1, 2;
                            ::shift_indices($result, $cfline + 1, -2);
                        }
                    }
                    splice @{$notesref}, $i, 1, @newlines;
                }
            }

            if ($config->notes_wrap_columns > 0) {
                # So that it's possible to fix up the stored config file and the
                # associated indices, wrap the notes lines one by one
                my @newnotes = ();
                foreach my $noteref (@{$notesref}) {
                    my ($edit, $tag, $note) = @$noteref;
                    if (length($note) < $config->notes_wrap_columns) {
                        # It obviously won't be wrapped, right?
                        push @newnotes, $noteref;
                    } else {
                        my @newlines = main::wrap_lines( [ $note ],
                                                         $config->notes_wrap_columns,
                                                         $config->notes_wrap_indent);
                        if (@newlines > 1) {
                            # A line was wrapped
                            push @newnotes, [ 1, $tag, shift(@newlines) ];
                            push @newnotes, map { [ 0, undef, $_ ] } @newlines;
                        } else {
                            push @newnotes, $noteref;
                        }
                    }
                }
                @{$notesref} = @newnotes;
            }
        }
    }

    # This needs to be present.
    $result->{'notes'}->{''} = [] unless ref($result->{'notes'}->{''}) eq 'ARRAY';

    # If there are saved notes from the build, insert them now.
    if ($config->accessor_nowarn('baggage') ne '') {
	unshift @{$result->{'notes'}->{''}}, map { [ 0, undef, $_ ] } split(/(?:\r\n|\n)+/, $config->baggage), "\n";
    }

    # Renumber the notes _now_ so that the config file (and the indices) can
    # be updated.
    ::renumber_notes($result, 3, 0);

    # Blow away the old stored config and replace it with txtconfig
    $result->{'rawconfig'} = [ split(/\n/, ::compress_encode(join("\n", @{$result->{'txtconfig'}}))) ];
    delete $result->{'txtconfig'};
    if (exists($result->{'orig_raw_config'}) && !exists($result->{'origconfig'})) {
	$result->{'origconfig'} = [ split(/\n/, ::compress_encode(join("\n", @{$result->{'orig_raw_config'}}))) ];
    }

    # Reconstitute the compile-time options for the convenience of any
    # formatters that might wish to make use of it.
    for my $bench (keys %{$result->{'compile_options'}}) {
	for my $tune (keys %{$result->{'compile_options'}->{$bench}}) {
	    my $rawopts = $result->{'compile_options'}->{$bench}->{$tune};
	    next unless ($rawopts ne ''); # Skip empty ones
	    my $compopts = ::decode_decompress($rawopts);
	    $rawopts = $compopts unless ($@ || !defined($compopts));
	    $result->{'compile_options'}->{$bench}->{$tune} = $rawopts;
	}
    }

    $result->{'basemean'} = 'Not Run';
    $result->{'peakmean'} = 'Not Run';
    my $peakseen = 0;
    if (grep { /^peak$/o } @{$config->tunelist}) {
      $peakseen = 1;
      # Munge up the results if basepeak is set, and peak was selected to run
      # If global basepeak is 1, we do wholesale base->peak substitution.
      # If global basepeak is 2, we do per-benchmark lowest median selection
      if ($result->{'basepeak'} == 1) {
	::basepeak_munge($result);
      } elsif ($result->{'basepeak'} == 2) {
	my @bp_bench = ();
	for my $bench (keys %{$result->benchmarks}) {
	    next unless istrue($me->{'benchmarks'}->{$bench}->{'basepeak'});
	    push @bp_bench, $bench;
	}
	::basepeak_munge($result, 0, @bp_bench);
      }
      $result->{'peakmean'} = ($result->rate)?$result->calc_mean_rate('peak'):
          $result->calc_mean_speed('peak');
    }
    if (grep { /^base$/o } @{$config->tunelist}) {
      $result->{'basemean'} = ($result->rate)?$result->calc_mean_rate('base'):
                                      $result->calc_mean_speed('base');
    }
    push @{$result->{'do_dump'}}, qw(basemean peakmean);

    # Check for some basic errors
    $result->add_error("'reportable' flag not set during run") if !istrue($config->reportable);

    my $saw_base = 0;
    for my $tune (@{$config->tunelist}) {
	$saw_base++ if ($tune eq 'base');
	for my $bench ($result->insufficient_data($tune)) {
	    $result->add_error("$bench $tune did not have enough runs!\n");
	}
    }
    if (!$saw_base) {
	$result->add_error("No 'base' runs!  Base measurement required!\n");
    }
    if ($result->size ne 'ref') {
	$result->add_error("Input set must be 'ref' for a valid run (set to '".$result->size."' for this run)\n");
    }

    return $result;
}

sub insufficient_data {
    my ($me, $tune) = @_;
    $tune = 'base' if $tune eq '';

    my @which = ();
    for my $bench (keys %{$me->benchmarks}) {
	if (!exists $me->{'results'}{$bench} ||
            !exists $me->{'results'}{$bench}{$tune} ||
            !exists $me->{'results'}{$bench}{$tune}{'data'} ||
	    ref($me->{'results'}{$bench}{$tune}{'data'}) ne 'ARRAY' ||
	    @{$me->{'results'}{$bench}{$tune}{'data'}}+0 < $main::global_config->min_report_runs) {
	    push (@which, $bench);
	}
    }
    return @which;
}

sub add_results {
    my ($me, $bench, $config) = @_;
    my @tunes = ($bench->tune);

    for my $tune (@tunes) {
	my @tmp;
	if (istrue($me->rate)) {
	    @tmp = $bench->result_list;
	} else {
	    @tmp = $bench->result_list(1);
	}
	push (@{$me->{'results'}{$bench->benchmark}{$tune}{'data'}}, @tmp);
	$me->{'compile_options'}->{$bench->benchmark}->{$tune} = $bench->accessor_nowarn('compile_options');
	$me->{'results'}->{$bench->benchmark}->{$tune}->{'flags'} = $config->{$bench->benchmark}->{$tune}->{'flags'};
    }
    $me->{'benchmarks'}->{$bench->benchmark}->{'basepeak'} = $bench->basepeak;
}

sub reference {
    my ($me, $bench) = @_;
    return $me->{'reference'}{$bench};
}

sub valid {
    my ($me, $bench, $tune) = @_;
    return 0 unless exists $me->{'results'}{$bench};
    return 0 unless exists $me->{'results'}{$bench}{$tune};
    my $valid = '?';
    for (@{$me->{'results'}{$bench}{$tune}{'data'}}) {
	$valid = $_->{'valid'};
    }
    return $valid;
}

sub ratio {
    my ($me, $bench, $tune) = @_;
    my $rc = '';
    my $count = 0;
    for (@{$me->{'results'}{$bench}{$tune}{'data'}}) {
	next if ! $_->{'selected'};
	$rc += $_->{'ratio'};
	$count++;
    }
    $rc /= $count if $count;
    return $rc;
}

sub copies {
    my ($me, $bench, $tune) = @_;

    # Base # copies are always the same
    return $me->base_copies if ($tune eq 'base');

    for (@{$me->{'results'}{$bench}{$tune}{'data'}}) {
	next if ! $_->{'selected'};
        if (defined($_->{'clcopies'}) && $_->{'clcopies'} ne '') {
            return $_->{'clcopies'};
        } else {
            return $_->{'copies'};
        }
    }
    return '';
}

sub runtime {
    my ($me, $bench, $tune, $round) = @_;
    my $rc = '';
    my $count = 0;
    for (@{$me->{'results'}{$bench}{$tune}{'data'}}) {
	next if ! $_->{'selected'};
	$rc += $_->{'reported_time'};
	$count++;
    }
    if ($count) {
	$rc /= $count;
	$rc = int($rc + 0.5) if ($round);
    }
    return $rc;
}

sub calc_mean_rate {
    my ($me, $tune) = @_;
    $tune = 'base' if $tune eq '';
    my $sufficient = scalar($me->insufficient_data($tune)) == 0;

    my $per_copy = {};
    for my $bench (keys %{$me->benchmarks}) {
        next unless exists($me->{'results'}->{$bench});
        next unless exists($me->{'results'}->{$bench}->{$tune});
        next unless exists($me->{'results'}->{$bench}->{$tune}->{'data'});
	for my $obj ( @{ $me->{'results'}{$bench}{$tune}{'data'} }) {
	    next unless ($obj->valid eq 'S');
	    push (@{$per_copy->{$obj->copies}{$bench}{'data'}}, $obj);
	}
    }
    my $num_benchmarks = (keys %{$me->benchmarks})+0;
    if ($tune eq 'base') {
	my $nextbest_product = 0;
	my $nextbest_copies  = 0;
	my $best_product = 0;
	my $best_copies  = 0;
	for my $copies (sort { $a <=> $b } keys %$per_copy) {
	    my $copyref = $per_copy->{$copies};
	    my $product = 1;
	    my $count = 0;
	    my $valid = 'S';
	    for my $bench (keys %{$me->benchmarks}) {
		if (! exists $per_copy->{$copies}{$bench}) {
		    $valid = 'X';
		    next;
		}
		my $benchref = $per_copy->{$copies}{$bench};
		$valid = 'X' if (@{$benchref->{'data'}} < $main::global_config->min_report_runs);
		my $tmp = ::median_ratio(1, @{$benchref->{'data'}});
		if (defined($tmp) && ($tmp > 0)) {
		    $product *= $tmp;
		    $count ++;
		}
	    }
	    if ($count) {
		$product = $product ** (1/$count);
		if ($valid eq 'X' && $product > $nextbest_product) {
		    $nextbest_product = $product;
		    $nextbest_copies  = $copies;
		}
		if ($product > $best_product) {
		    $best_product = $product;
		    $best_copies  = $copies;
		}
	    }
	}
	if ($best_copies == 0) {
	    $me->{'valid'} = 'X';
	    $me->add_error("There is no set of valid runs with the same number of copies for base");
            return (istrue($main::global_config->mean_anyway) || $sufficient) ? $nextbest_product : '--';
	}
	return (istrue($main::global_config->mean_anyway) || $sufficient) ? $best_product : '--';
    } else {
	my $best_product = {};
	my $best_copies = {};
	for my $copies (keys %$per_copy) {
	    for my $bench (keys %{$me->benchmarks}) {
		next unless exists $per_copy->{$copies}{$bench};
		my $benchref = $per_copy->{$copies}{$bench};
		# median_ratio wouldn't do the right thing for benchmarks
                # with copies that varied per-iteration.  But that doesn't
                # happen, so this is safe:
		my $product = ::median_ratio(1, @{$benchref->{'data'}});
		if (defined($product) && ($product > 0) &&
		    ($product > $best_product->{$bench})) {
		    $best_product->{$bench} = $product;
		    $best_copies->{$bench}  = $copies;
		}
	    }
	}
	my $product = 1;
	my $count = 0;
	for my $bench (keys %{$me->benchmarks}) {
	    if (exists $best_product->{$bench}) {
		$product *= $best_product->{$bench};
		$count ++;
	    } else {
		$me->{'valid'} = 'X';
		$me->add_error("Complete set of valid runs for peak rate unavailable ($bench missing)");
	    }
	}
	if ($count) {
	    $product = $product ** (1/$count);
	} else {
	    $product = 0;
	}
	return (istrue($main::global_config->mean_anyway) || $sufficient) ? $product : '--';
    }
}

sub add_error {
    my $me = shift;
    push (@{$me->{'errors'}}, @_);
}

sub calc_mean_speed {
    my ($me, $tune) = @_;
    $tune = 'base' if $tune eq '';
    my $sufficient = $me->insufficient_data($tune) == 0;

    my $product = 1;
    my $count = 0;
    for my $bench (keys %{$me->benchmarks}) {
	my @results = ();
        next unless exists($me->{'results'}->{$bench});
        next unless exists($me->{'results'}->{$bench}->{$tune});
        next unless exists($me->{'results'}->{$bench}->{$tune}->{'data'});
	for my $obj ( @{ $me->{'results'}{$bench}{$tune}{'data'} }) {
	    next if ($obj->valid ne 'S' || $obj->copies != 1);
	    push (@results, $obj);
	}
	my $tmp = ::median_ratio(1, @results);
	if (defined $tmp && $tmp > 0) {
	    $product *= $tmp;
	    $count++;
	}
    }
    if ($count == 0) {
      # '--' means 'no'
      return 0 if (istrue($main::global_config->mean_anyway) || $sufficient);
      return '--';
    }
    $product = $product ** (1/$count);
    return (istrue($main::global_config->mean_anyway) || $sufficient) ? $product : '--';
}

sub results_list {
    my ($me) = @_;
    my $benchhash = $me->{'results'};
    return () if ref($benchhash) ne 'HASH';
    my @result;
    for my $tune ('base', 'peak') {
	for my $bench (sort keys %$benchhash) {
	    next if ref($benchhash->{$bench}) ne 'HASH';
	    next if !exists $benchhash->{$bench}{$tune};
            if (!exists($benchhash->{$bench}{$tune}{'data'})) {
                Log(0, "WARNING: No data for $bench:$tune\n");
                next;
            }
	    push (@result, @{$benchhash->{$bench}{$tune}{'data'}});
	}
    }
    return @result;
}

sub benchmark_results_list {
    my ($me, $bench, $tune) = @_;
    my $benchhash = $me->{'results'};
    return () unless isa($benchhash, 'HASH');
    return () unless isa($benchhash->{$bench}, 'HASH');
    return () unless isa($benchhash->{$bench}{$tune}, 'HASH');

    if (!exists($benchhash->{$bench}{$tune}{'data'})) {
	Log(0, "WARNING: No data for $bench:$tune\n");
	return ();
    }
    return @{$benchhash->{$bench}{$tune}{'data'}};
}

sub bench_in {
    my ($me, $bench) = @_;
    return exists $me->benchmarks->{$bench->benchmark} &&
	    $me->mach eq $bench->mach && $me->ext eq $bench->ext &&
	    $me->size eq $bench->size;
}

sub Log    { main::Log(@_); }
sub jp     { main::joinpaths(@_); }
sub istrue { main::istrue(@_); }
sub src    { my $me = shift; jp($me->path, $me->srcdir); }

sub print_bench {
  # This just lists which benchmarks and tuning levels live in the 'results'
  # item of a result object.  It's just for debugging.
  my ($r, $tag) = @_;

  if (!exists($r->{'results'})) {
    print "$tag: none\n";
    return;
  }
  if (ref($r->{'results'}) ne 'HASH') {
    print "$tag: NOT HASH\n";
    return;
  }
  print "$tag: ";
  foreach my $bench (sort keys %{$r->{'results'}}) {
    print "$bench (";
    if (ref($r->{'results'}->{$bench}) ne 'HASH') {
      print "NOT HASH";
    } else {
      print join(', ', sort keys %{$r->{'results'}->{$bench}});
    }
    print "); ";
  }
  print "\n";
}

1;

