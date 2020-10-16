#
#  ps.pl - produces PostScript and PDF output (using the PSPDF API)
#  Copyright (C) 1999-2006 Standard Performance Evaluation Corporation
#   All Rights Reserved
#
#  Authors:  Christopher Chan-Nui
#            Cloyce D. Spradling
#
# $Id: ps.pl 4626 2006-07-19 20:29:42Z cloyce $

use strict;
use PSPDF;
use File::Basename;
use Cwd;
use UNIVERSAL qw(isa);
require 'util.pl';

use vars qw($name $extension $synonyms $invalid $inch $nc $sortdir);

$name      = 'PostScript';
$extension = 'ps';
$synonyms  = { map { lc($_) => 1 } ($name, $extension, qw(printer print)) };

my $ps_version = '$LastChangedRevision: 4626 $ '; # Make emacs happier
$ps_version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$Spec::Format::ps::non_default = 1;  # You must ask for it by name
$Spec::Format::ps::part_of_all = 1;  # Included in '-o all'
$::tools_versions{'ps.pl'} = $ps_version;
my @peak_color =  (0.15, 0.15, 0.70);
my @error_color = (0.87, 0   , 0   );
my $base_shade = 0.9;

my $debug = 0;
my %max;
my %min;
my $min_font_size = 6;
my $fixed_font = 'Courier';
my $prop_font = 'Times-Roman';
my $prop_bold_font = 'Times-Bold';

$invalid   = 0;			# Innocent until proven guilty
$nc   = 0;			# Innocent until proven guilty
$inch = 72;
$sortdir = 0;			# Used in number_tick and byspot

sub format () {
    my($me, $r, $path) = @_;
    my @output = SPEC_report($r, 'PS', $path);
    return (\@output, []);
}

############################################################################
# PS/PDF Formatting routines

my $smallest = {};
my $largest  = {};
my $mode     = 'PS'; # Just the default
# We need a margin, let's use 0.5 inch
my $margin = 0.5 * $inch;

sub debug_clone {
    my ($config, $sb, $st, $tb, $tt) = @_;
    %{$config->{'results'}{$tb}{$tt}{'data'}[0]} = %{$config->{'results'}{$sb}{$st}{'data'}[0]};
    bless $config->{'results'}{$tb}{$tt}{'data'}[0], 
	ref ($config->{'results'}{$sb}{$st}{'data'}[0]);
};

sub SPEC_report {
    my ($config, $mymode, $path) = @_;
    my ($width, $height, $hsize);

    $mode = $mymode;
    $mode = 'PS' if $mode eq '';
    my ($fn, $base_url);
    # Get a non-relative path.
    $path = ::unrel_path($path);
    if (defined($::website_formatter) && $::website_formatter) {
      # Strip the base off of the path, leaving the filename and whatever
      # hierarchy it's in.
      ($fn = $path) =~ s#^$::report_base/*##;
      $fn =~ s/\.${mode}$//i;
      $base_url = $::report_url_base.'/'.$fn;
    } else {
      $base_url = $fn = basename($path, '.'.lc($mode));
    }

    if ($config->{'invalid'} ||
	(((ref($config->{'errors'}) eq 'ARRAY') && @{$config->{'errors'}}))) {
	$invalid = 1;
    }
    if (@{$config->{'nc'}}+0) {
	$nc = 1;
    }
    my $pspdf = new PSPDF $mode;
    if (exists ($pspdf->{'error'}) && $pspdf->{'error'}) {
	# PDF probably couldn't be initialized
	return '';
    }
    $pspdf->set_fontdir ("$ENV{'SPEC'}/bin/fonts") if exists $ENV{'SPEC'};
    my @gmtimes = gmtime(time);
    $gmtimes[5] += 1900;
    $gmtimes[4]++;
    $pspdf->CreationDate(sprintf("D:%04d%02d%02d%02d%02d+00'00'", @gmtimes[5,4,3,2,1,0]));

    $pspdf->Creator("SPEC ${main::suite} tools $::version (PSPDF v$ps_version)");
    # Collect things correctly, even if they're arrays
    my %things = ();
    foreach my $thing (qw( tester hw_vendor test_sponsor hw_model )) {
        next unless exists($config->{$thing});
        $things{$thing} = join(' ', ::allof($config->{$thing}));
    }
    my $title = $things{'hw_vendor'}.': '.$things{'hw_model'};
    $things{'test_sponsor'} = $things{'hw_vendor'} if ($things{'test_sponsor'} =~ /^(|--)$/);
    $things{'tester'} = $things{'test_sponsor'} if ($things{'tester'} =~ /^(|--)$/);
    $pspdf->Author ($things{'tester'});
    if ($things{'test_sponsor'} ne $things{'hw_vendor'}) {
	$title .= ' (test sponsored by '.$things{'test_sponsor'};
        if ($things{'tester'} ne $things{'test_sponsor'}) {
            $title .= '; tested by '.$things{'tester'};
        }
        $title .= ')';
    }
    $pspdf->Title ($title);
    my $subj = sprintf('%sSPEC %s Result', ($invalid) ? 'Invalid ' : '',
		       joinup($config->{'metric'}));
    $pspdf->Subject($subj);

    $pspdf->open();

    ($width, $height) = SPEC_newpage($pspdf, $config);

    $smallest = {};
    $largest  = {};
    %max = ( 'ratio' => { 'base' => 0, 'peak' => 0, 'any' => 0 },
	     'time'  => { 'base' => 0, 'peak' => 0, 'any' => 0 },
	   );
    %min = ( 'ratio' => { 'base' => 1<<30, 'peak' => 1<<30, 'any' => 1<<30 },
	     'time'  => { 'base' => 1<<30, 'peak' => 1<<30, 'any' => 1<<30 },
	   );
    for my $res (sort $config->results_list) {
	my $tune = $res->tune;
	my $tmp;

	my $time  = $res->reported_time;
	my $ratio = $res->ratio;
	# Fill in mins and maxes for ALL results
        if ($time ne '--') {
            $min{'time'}->{$tune}  = $time  if $min{'time'}->{$tune}  > $time;
            $min{'time'}->{'any'}  = $time  if $min{'time'}->{'any'}  > $time;
            $max{'time'}->{$tune}  = $time  if $max{'time'}->{$tune}  < $time;
            $max{'time'}->{'any'}  = $time  if $max{'time'}->{'any'}  < $time;
            if ($res->selected) {
                $smallest->{$tune}{'time'}  = $time  if $smallest->{$tune}{'time'}  < $time;
                $largest->{$tune}{'time'}   = $time  if $largest->{$tune}{'time'}   > $time;
            }
        }
        if ($ratio ne '--') {
            $min{'ratio'}->{$tune} = $ratio if $min{'ratio'}->{$tune} > $ratio;
            $min{'ratio'}->{'any'} = $ratio if $min{'ratio'}->{'any'} > $ratio;
            $max{'ratio'}->{$tune} = $ratio if $max{'ratio'}->{$tune} < $ratio;
            $max{'ratio'}->{'any'} = $ratio if $max{'ratio'}->{'any'} < $ratio;
            if ($res->selected) {
                $smallest->{$tune}{'ratio'} = $ratio if $smallest->{$tune}{'ratio'} < $ratio;
                $largest->{$tune}{'ratio'}  = $ratio if $largest->{$tune}{'ratio'}  > $ratio;
            }
        }
    }

    # Draw the Graph
    $hsize = SPEC_graph ($pspdf, 0, $height, $width, $height, $config);
    return undef unless defined($hsize);
    $height -= $hsize;

    # Draw the Detail boxes
    my $did_continue = 0;
    ($did_continue, $hsize, $height) = SPEC_detail($pspdf, 0, $height, $width, $height, $config);
    return undef unless defined($hsize);
    $height -= $hsize;

    # The subcommittee has decreed that the results table shall be at the
    # top of the second page, with notes, errors, and flags following.
    # I think errors are important, though, so they go first.
    if ((ref($config->{'errors'}) eq 'ARRAY') &&
	(@{$config->{'errors'}}+0 > 0)) {
      my @tmperrs = ();
      foreach my $line (@{$config->{'errors'}}) {
        my $lineref = $line;
        if ($line =~ m#(?:http|ftp)://#i) {
          # Break it up into linked and non-linked sections
          $lineref = [];
          foreach my $chunk (split(m#((?:http|ftp)://[^[:space:]<>()]+)#i, $line)) {
            if ($chunk =~ m#(?:http|ftp)://#i) {
              push @{$lineref}, [ $chunk, $chunk ];
            } else {
              push @{$lineref}, [ undef, $chunk ];
            }
          }
        }
        push @tmperrs, $lineref;
      }
      ($did_continue, $width, $height) = do_section($pspdf, $config,
                                                    $width, $height,
                                                    'title' => 'Errors',
                                                    'title_color' => [ @error_color ],
                                                    'content' => \@tmperrs );
      return 0 if ($width == 0);
    }
    if ($did_continue == 0) {
	# No errors, or the errors didn't spill out onto the second page,
        # so the table gets a fresh page.
	$pspdf->grestore();
	end_page($pspdf);
	($width, $height) = SPEC_newpage($pspdf, $config);
    }

    # Draw the Table
    $height = SPEC_table ($pspdf, 0, $height, $width, $height, $config);
    return undef unless defined($height);

    # Do the notes sections
    foreach my $sectionref (@{$config->notes}) {
        next unless ref($sectionref) eq 'ARRAY';
        my ($section, $notesref) = @{$sectionref};
        next unless @{$notesref};

        my @tmpnotes = ();
        # Scan the notes for linkable (http, ftp) items 
        foreach my $line (@{$notesref}) {
          my $lineref = $line;
          if ($line =~ m#(?:http|ftp)://#i) {
            # Break it up into linked and non-linked sections
            $lineref = [];
            foreach my $chunk (split(m#((?:http|ftp)://[^[:space:]<>()]+)#i, $line)) {
              if ($chunk =~ m#(?:http|ftp)://#i) {
                push @{$lineref}, [ $chunk, $chunk ];
              } else {
                push @{$lineref}, [ undef, $chunk ];
              }
            }
          }
          push @tmpnotes, $lineref;
        }

        # Using a fixed-width font will help preserve notes formatting.
        # Thanks to John Henning for the suggestion.
        ($did_continue, $width, $height) = do_section($pspdf, $config,
                                                      $width, $height,
                                                      'title' => $section,
                                                      'title_size' => 16,
                                                      'content_font' => $fixed_font,
                                                      'content' => \@tmpnotes);
        return 0 if ($width == 0);
    }

    # Do the flags report
    ($did_continue, $width, $height) = SPEC_flags ($pspdf, $config, $width, $height, $fn.'.flags.html', $base_url.'.flags.html');
    return undef unless defined($height);

    # Finally, the footer paragraph with the trademarks, etc.
    ($did_continue, $width, $height) = SPEC_trademarks ($pspdf, $config, $width, $height);
    return undef unless defined($height);

    $pspdf->grestore();
    end_page($pspdf);
    if (@{$config->{'nc'}}+0 == 0) {
      # Now encode/attach the raw file
      if (defined($config->{'compraw'})) {
	  my %whatcomp = ( '*' => 'BASE64', '&' => 'BZIP2', '@' => 'GZIP' );
	  my $whatcomp = 'BASE64';
	  if ($config->{'compraw'} =~ /^([\@\&\*])/) {
	      $whatcomp = $whatcomp{$1};
	  }
          $pspdf->add_raw("${fn}.rsf", ' '.$whatcomp, $config->{'compraw'});
      } elsif (defined($config->{'rawfile'})) {
          $pspdf->add_raw("${fn}.rsf", '', $config->{'rawfile'});
      } else {
          ::Log(0, "Encoding problem?  There is no raw file to embed in the result file.\n");
      }
    }
    $pspdf->close();

    my @unbalanced_saves = $pspdf->get_saves();
    if (@unbalanced_saves) {
	::Log(0, "ERROR: Unbalanced graphics states from ".join(', ', map { ref($_) eq 'ARRAY' && join(':', @{$_}) } @unbalanced_saves)."\n");
	return undef;
    }

    if ($mode eq 'PDF') {
      return $pspdf->output();
    } else {
      return split("\n", $pspdf->output());
    }
}

sub SPEC_newpage {
    my ($p, $config) = @_;

    $p->begin_page('letter');
    $p->set_font($prop_font, 18);

    my $width  = $p->{'width'};
    my $height = $p->{'height'};

    $p->gsave();
    $height -= $margin * 2;
    $width  -= $margin * 2;
    $p->translate($margin, $margin);

    # Draw a box for the page
    $p->setlinewidth(0.4);
    $p->rect(0,0, $width,$height);
    $p->stroke();
    $p->setlinewidth(0.2);

    # Draw the advertisement at the bottom
    my $hsize = 0.5 * $inch;
    SPEC_advertisement($p, 0, 0, $width, $hsize);
    $height -= $hsize;
    $p->translate(0,$hsize);

    $p->{'mx'} = $margin;
    $p->{'my'} = $margin + $hsize;

    # Draw the Banner Bar
    $hsize = 0.5 * $inch;
    SPEC_banner($p, 0, $height-$hsize, $width, $hsize, $config);
    $height -= $hsize;

    # Draw the Title Bar
    $hsize = 0.75 * $inch;
    SPEC_title($p, 0, $height-$hsize, $width, $hsize, $config);
    $height -= $hsize;

    # Draw the Info Bar
    $hsize = 0.125 * $inch;
    $height -= SPEC_infobar($p, 0, $height, $width, $hsize, $config);

    # Draw the NC box
    if (@{$config->{'nc'}}+0) {
	my $fontsize = 14;
	my @nc = @{$config->{'nc'}};
	my $indent = 0.375 * $inch;

	$p->gsave();
	$p->set_font($prop_bold_font, $fontsize);

	# Blaze through and find the longest line -- it'll be used to
	# figure the font size
	my $maxsize = 0;
	my $badsize = 1;
	while ($badsize) {
	    $badsize = 0;
	    foreach my $line (@nc) {
		my $strwidth = $p->stringwidth($line, $fontsize);
		if ($strwidth > ($width - ($indent * 2))) {
		    $badsize = 1;
		    $maxsize = 0;
		    $fontsize--;
		    $p->set_font($prop_bold_font, $fontsize);
		    last;
		} elsif ($strwidth > $maxsize) {
		    $maxsize = $strwidth;
		}
	    }
	}
	my $fonthsize = $p->string_CapHeight() * 2.5;
	$hsize = $fonthsize * (@nc-1) + (0.5 * $inch);
	$p->translate(0, $height-$hsize);
	$p->rect(0,0, $width, $hsize);
	$p->stroke();
	my $currheight = $hsize - (0.25 * $inch);
	foreach my $line (@nc) {
	    $p->set_text_pos(0+$indent, $currheight);
	    $p->show($line);
	    $currheight -= $fonthsize;
	}
	$height -= $hsize;
	$p->grestore();
    }

    return ($width, $height);
}

sub SPEC_logo {
    my ($p, $x, $y, $width, $height) = @_;
    my $fontsize=72;

    $p->gsave();
	$p->rect(0,0, $width,$height);
	$p->stroke();
	my $border = ::min($width, $height) * 0.05;
	$p->translate($x + $border, $y + $border);
	$width -= $border * 2;
	$height -= $border * 2;

        # Find out what the correct scale for the text is
	my $i = 0.5;
	$p->set_font($prop_bold_font, $fontsize);
	my $w = $p->stringwidth('spec');
	$fontsize *= ($width / $w);
	$fontsize = $height * 0.3 if $fontsize > $height * 0.3;

        # Print text for bottom
	$p->set_font($prop_bold_font, $fontsize);
	my $XHeight = $p->string_CapHeight() * 1.1;
	my $Descender = $p->string_Descender() * 1.1;
	$p->set_text_pos($width/2, $Descender /4);
	$p->show_center('spec');

        # Print Grid
	my $gridsize = $height - $fontsize;
	$gridsize = $width if $width < $gridsize;
	$p->translate(($width-$gridsize)/2, $XHeight + $Descender /2);

	for ($i = 0; $i <= 4; $i++) {
	    $p->moveto ($i/4 * $gridsize,                0);
	    $p->lineto ($i/4 * $gridsize,        $gridsize);
	    $p->moveto (               0, $i/4 * $gridsize);
	    $p->lineto (       $gridsize, $i/4 * $gridsize);
	}
	$p->stroke();

        # Print the curve
        $p->setrgbcolor(235,0,0);
	for ($i = 11; $i <= 11; $i++)  {
	    my $x1 = $i/16 * $gridsize;
	    my $y1 =  4/16 * $gridsize;
	    my $x2 = 12/16 * $gridsize; 
	    my $y2 = (16-$i)/16 * $gridsize;
	    $p->moveto ( 3/16 * $gridsize,  3/16 * $gridsize);
	    $p->curveto( $x1,$y1, $x2,$y2,
			13/16 * $gridsize, 13/16 * $gridsize);
	    $p->stroke();
	}
    $p->setgray(0);
    $p->grestore();
    return 1;
}

sub SPEC_banner {
    my ($p, $x, $y, $width, $height, $config) = @_;
    my $bigfontsize = $height * 0.8;
    my $fontsize = $height * 0.2;
    my $copyright_char = '\323';
    my @copyright_msg  = ('Copyright ', 
			  '2006 Standard Performance Evaluation Corporation');
    $p->gsave();
	$p->translate($x,$y);
	$p->rect(0,0, $width,$height);
	$p->stroke();

	if (!$invalid &&		# Invalid results should never get the
	    defined($::website_formatter) &&
	    $::website_formatter &&     # SPEC Seal of Reviewal
            defined($::format_for_publication) &&
            $::format_for_publication) {
	    SPEC_logo($p, 0, 0, $height, $height);
	}

	$p->set_font($prop_bold_font, $bigfontsize);
	$p->set_text_pos($width/2, $height-$bigfontsize*0.9);
	$p->linkto_center('SPEC '.$config->{'metric'}." Result", undef, 'tmsearch' => 1);

	$p->set_font('Symbol', $fontsize);
	my $w = $p->stringwidth($copyright_char);
	$p->set_font($prop_font, $fontsize);
	$w += $p->stringwidth ($copyright_msg[0]);
	$w += $p->stringwidth ($copyright_msg[1]);
	$p->set_text_pos(($width - $w)/2, $height-$bigfontsize*0.9 - $fontsize);
	$p->set_font ($prop_font, $fontsize);
	$p->show($copyright_msg[0]);
	$p->set_font("Symbol", $fontsize);
	$p->show($copyright_char, undef, undef, 'noescape' => 1, 'doencode' => 1);
	$p->set_font ($prop_font, $fontsize);
	$p->show($copyright_msg[1]);
    $p->grestore();
    return 1;
}

sub SPEC_advertisement {
    my ($p, $x, $y, $width, $height) = @_;
    $p->gsave();
    {
	$p->translate($x,$y);
	$p->rect_fill(0,0, $width,$height, 0.95, undef, 1);

	my $font_size = $height / 4;
	$p->set_font($prop_font, $font_size);

	my $margin = $height/8;

	$x = $margin;
	$y = $height - $margin - $p->string_CapHeight();

	$p->set_text_pos($x, $y);

	$p->set_text_pos($width/2, $y);
	$p->show_center('Standard Performance Evaluation Corporation');
        # Address, phone #s removed because they change too often
	$p->continue_text_center('info@spec.org', undef, 'mailto:info@spec.org');
	$p->continue_text_center('http://www.spec.org/', undef, 'http://www.spec.org/');

        # And now the page number
        $y = $height / 2 - ($p->string_CapHeight() / 2);
        $p->set_text_pos($width - ($inch / 32), $y);
        $p->show_right('Page '.$p->{'pagenum'});
    }
    $p->grestore();
    return 1;
}

sub SPEC_infobar {
    my ($p, $x, $y, $width, $hsize, $config) = @_;
    my $height = -$hsize;
    
    # Get all of the "things" to print so we'll know how big to make the
    # boxes.
    my %things = ();
    my $max_lines = 0;
    foreach my $thing (qw( license_num test_date hw_avail sw_avail test_sponsor )) {
        $things{$thing} = [ ::allof($config->{$thing}) ];
        $max_lines = @{$things{$thing}} if (@{$things{$thing}} > $max_lines);
    }
    $things{'tester'} = [ ::allof($config->{'tester'}) ];

    # Does test_sponsor need to be set to hw_vendor?
    if ($things{'test_sponsor'}->[0] =~ /^(|--)$/) {
        @{$things{'test_sponsor'}} = ::allof($config->hw_vendor);
        $max_lines = @{$things{'test_sponsor'}} if (@{$things{'test_sponsor'}} > $max_lines);
    }

    # Figure out whether or not we'll need to print 'tester' separately
    if (@{$things{'tester'}} &&
        $things{'tester'}->[0] !~ /^(|--)$/ &&
        join(' ', @{$things{'tester'}}) ne join(' ', @{$things{'test_sponsor'}})) {
        $max_lines += @{$things{'tester'}};
    } else {
        $things{'tester'} = [];
    }

    #               Tag                       Key             Width
    my @items = ( [ $::suite.' license #:'  , 'license_num' , 0.16 ],
                  [ 'Test sponsor:'         , 'test_sponsor', 0.27 ],
                  [ 'Test date:'            , 'test_date'   , 0.15 ],
                  [ 'Hardware Availability:', 'hw_avail'    , 0.21 ],
                  [ 'Software Availability:', 'sw_avail'    , 0.21 ],
                );
    $p->gsave();
    {
	$p->translate($x,$y);
	$p->rect(0,0, $width, -$hsize * $max_lines);
	$p->stroke();
        $p->setlinewidth(0.1);
	my $border = ::min($width, $hsize) * 0.2;
	$p->set_font($prop_font, $hsize - $border - 0.5);
        my $string_base = (($hsize - $border) / 2) -
	                  (($p->string_CapHeight() + $p->string_Descender) / 2) + 0.5;
        # Make the boxes
        my $xpos = 0;
        foreach my $info (@items) {
            my ($name, $key, $relwidth) = @$info;
            my $mywidth = $relwidth * $width;
            $p->rect($xpos, 0, $mywidth, -$hsize * $max_lines);
            $xpos += $mywidth;
        }
        $p->stroke();
        my %printed = ();
        for(my $i = 1; $i <= $max_lines; $i++) {
            $xpos = 0;
            foreach my $info (@items) {
                my ($name, $key, $relwidth) = @$info;
                my $item = shift @{$things{$key}};
                if (!defined($item) && $key eq 'test_sponsor' &&
                    @{$things{'tester'}}) {
                  # Switch over to printing the tested by info
                  $name = 'Tested by:';
                  $key = 'tester';
                  $item = shift @{$things{$key}};
                }
                my $mywidth = $relwidth * $width;
                if (!$printed{$key}) {
                    $p->set_text_pos($xpos+$border, $string_base + $height);
                    $p->linkto($name);
                    $printed{$key} = 1;
                }
                $p->set_text_pos($xpos+$mywidth-$border, $string_base + $height);
                $p->show_right($item);
                $xpos += $mywidth;
            }
            $height -= $hsize;
        }
    }
    $p->grestore();
    return -($height + $hsize);
}

# Given the upper left corner
sub SPEC_info_box {
    my ($p, $x, $y, $width, $height, $config, $list, $size, $seen) = @_;
    $seen = {} unless ref($seen) eq 'HASH';
#print "\nSPEC_info_box($p, $x, $y, $width, $height, $config, $list, $size)\n";
    my $namewidth = 0;
    my $finished = 1;
    $p->set_font($prop_font, $size*1.2);

    # Do a quick run through the items to find the longest tag string
    for my $item (@$list) {
	my $reftype = ref($item);
	if ($reftype eq 'ARRAY') {
	    my ($name, $key) = @$item;
	    my $w = $p->stringwidth("$name:", $size);
	    $namewidth = $w if $w > $namewidth;
	}
    }

    my $valpos = $namewidth + $width / 20;

    my $hsize = 0;
    for my $item (@$list) {
	my $reftype = ref($item);
	if ($reftype eq 'ARRAY') {
	    my ($name, @vals) = @$item;
            next if $seen->{$name};


	    my $mysize = $size;
	    for my $subval (@vals) {
		my $rc = $p->string_calcsize($subval, $width-$valpos, $mysize, 
			$min_font_size, "Your '$name' field is too long; %s size font will be unreadable!\n");
		return ($finished, 0) if $rc == 0;
		$mysize = $rc if $rc < $mysize;
	    }
            if ($hsize + $size + ($mysize * (@vals-1)) >= $height) {
                # This line would overflow the page; bail
                $finished = 0;
                last;
            }
            $seen->{$name}++;
	    $hsize += $size;

            # The item title
	    $p->set_font($prop_font, $size);
	    $p->set_text_pos($x, $y - $hsize);
	    $p->linkto("$name:");
            # The item content
	    $p->set_font($prop_font, $mysize);
	    for my $subval (@vals) {
                my $dx = 0;
		$p->set_text_pos($x + $valpos, $y - $hsize);
                if ($subval =~ m#(?:http|ftp)://#i) {
                  foreach my $chunk (split(m#((?:http|ftp)://[^[:space:]<>()]+)#i, $subval)) {
                    my $strwidth = $p->stringwidth($chunk);
                    if ($chunk =~ m#(?:http|ftp)://#i) {
                      $p->show($chunk, undef, $chunk);
                    } else {
                      $p->show($chunk);
                    }
                    $dx += $strwidth;
                    $p->set_text_pos($x + $valpos + $dx, $y - $hsize);
                  }
                } else {
                  $p->show($subval);
                }
		$hsize += $mysize;
	    }
	    $hsize -= $mysize if @vals;
	} elsif ($reftype eq '') {
            next if $seen->{$item};
            if ($hsize + ($size * 1.4) >= $height) {
                # This line would overflow the page; bail
                $finished = 0;
                last;
            }
            $seen->{$item}++;
	    $p->set_font($prop_bold_font, $size * 1.2);
	    $hsize += $size * 1.2;
	    $p->set_text_pos($x + $width / 2, $y - $hsize);
	    $hsize += $size * .2; # Nudge it down a little
	    $p->linkto_center($item);
	} else {
	    ::Log(0, "SPEC_info_length: list contains bogus element ref type!\n");
	    return (0, undef);
	}
    }
    return ($finished, $hsize);
}

sub SPEC_graph {
    my ($p, $x, $y, $width, $height, $config) = @_;
    #print "SPEC_graph(p=$p, x=$x, y=$y, width=$width, height=$height, config=$config)\n";

    my @titles;
    my $bar_height = 12;#$inch * 0.25; # One bar (base or peak)
    my $label_height = $bar_height * 2;  # Enough for two bars
    my $label_width  = 0;	# Figured later (space for benchmark name)
    my $graph_x     = $x + $label_width;
    my $graph_y     = 0;
    my $graph_width = $width - $graph_x;
    my $what = 'ratio';               # What's on the graph?
    my $tick_height = $bar_height * 0.375;
#    $width -= 1;		# Border space

    my %benchmarks = map { $_ => {} } keys %{$config->benchmarks};
    my $num_benchmarks = (keys %benchmarks)+0;
    return 0 unless $num_benchmarks;

    my $graph_height = ($bar_height * 2 * $num_benchmarks) + $bar_height * 3;
    if ($height < $graph_height) {
      ::Log(0, "SPEC_graph: not enough vertical room for graph!  $graph_height needed; $height available\n");
      return 0;
    }

    # Map of what we want to the key in the result data that provides the
    # information.
    my %what_key = (
		    'time'  => 'reported_sec',
		    'ratio' => 'ratio',
		   );

    $p->gsave();
    {
	$p->translate($x, $y);
	$p->setlinewidth(0.35);
	$p->rect(0,0, $width, -$graph_height);
	$p->stroke();
	$p->setlinewidth(0.2);

	$p->set_font($prop_bold_font); # Presumably larger than Times-Roman

	# Start with some arbitrary size; both min and max are included in
	# the full list.
	my $size = 14;

	# basemean and peakmean get included in both places because they'll
	# be on the graph no matter what.
	my @results_list = $config->results_list;
	my %results = (
		       'time' => {
				  ( map {
				    $_->benchmark.$_->iteration.$_->tune => [ $_->tune, $_->iteration, significant($_->reported_time) ]
				  } @results_list ),
				  'peakmean' => [ 'overall', 0, significant($config->peakmean) ],
				  'basemean' => [ 'overall', 0, significant($config->basemean) ]
				 },
		       'ratio' => {
				   ( map {
				    $_->benchmark.$_->iteration.$_->tune => [ $_->tune, $_->iteration, significant($_->ratio) ]
				   } @results_list ),
				  'peakmean' => [ 'overall', 0, significant($config->peakmean) ],
				  'basemean' => [ 'overall', 0, significant($config->basemean) ]
				  },
		      );
	# The range of the graph SHOULD BE from the min{what} to the
        # maximum{what}.  That's all.  But people don't like that, so it's
	# mungable.
        # On 20 Dec 2005, the CPU subcommittee voted that the default scale
        # minimum should be zero.
	my $graph_min = 0; # $min{$what}->{'any'};
	my $graph_max = $max{$what}->{'any'};
        my $int_ticks = 0;      # Tick only integers?

        my $auto_adj = $config->accessor_nowarn('graph_auto');
        if (defined($auto_adj) && $auto_adj) {
          $graph_min = $min{$what}->{'any'};
          $graph_max = $max{$what}->{'any'};
        } else {
          my $user_min = $config->accessor_nowarn('graph_min');
          my $user_max = $config->accessor_nowarn('graph_max');
          if (defined($user_min) && $user_min >= 0) {
            if ($auto_adj && $user_min > $min{$what}->{'any'}) {
              ::Log(0, "\nERROR: The specified graph minimum is too large; it will be adjusted.\n");
              $graph_min = $min{$what}->{'any'};
            } else {
              $graph_min = $user_min + 0;
            }
          }
          if (defined($user_max) && $user_max > 0) {
            if ($auto_adj && $user_max < $max{$what}->{'any'}) {
              ::Log(0, "\nERROR: The specified graph maximum is too small; it will be adjusted.\n");
              $graph_max = $max{$what}->{'any'};
            } else {
              $graph_max = $user_max + 0;
            }
          }
        }
        $graph_min = $graph_max if ($graph_min > $graph_max);
	my ($num, $tmpsize);

        # Make sure that there's enough "extra" space at the bottom of the
        # graph to accomodate a label near the original graph_min without
        # spilling over into the benchmark label area.

        $num = significant($graph_min);
        $tmpsize = $p->string_calcsize($num,
                                          $inch * 2, $bar_height / 2, 1,
                                          "ERROR: graph label \"$num\" is too long!");

        # This is overly generous.  Probably.
        my $needed = $p->stringwidth($num, $tmpsize);
        # The scale is not yet finalized, so it's been worked in to the
        # following calculation, which was originally
        # graph_min = ((graph_width + needed) / -scale) + graph_max
        $needed = ($needed * ($graph_max - $graph_min)) / -$graph_width;

        $graph_min += $needed if ($graph_min > $min{$what}->{'any'} + $needed);
	$graph_min = 0 if $graph_min < 0; # Hopefully this won't happen

	my $range = $graph_max - $graph_min;
	$range++ unless $range;   # This shouldn't ever happen...

	# Now that the scale is known, adjust the range so that it fits nicely
	# into a human-friendly fixed interval.
	# For now, let's shoot for around 40 ticks
	my $interval = $range / 100;

	# Figure out the order of magnitude of the graph_max, so we know
	# which place needs to be a multiple of 5 (and thus, what order of
	# magnitude the interval needs to be).
	# The extra math is to adjust for rounding the last visible place.
	my $interval_oom = significant($graph_max, undef, 1) * -1 + 2;

	# Scale so that the ones place is the one to round
	$interval *= (10 ** $interval_oom);

	# Round up to the nearest integer
	$interval = int($interval + 0.5);

	# Make it a multiple of 5.
	$interval += 5 - ($interval % 5);

	# Scale it back down
	$interval /= (10 ** $interval_oom);

        # Make the graph minimums and maximums integers if the range is
        # sufficient (i.e. enough to make at least two ticks).  This ensures
        # that the endpoints are nice integer ticks as well.
        # A nice side-effect is that it'll automagically conform to the
        # interval.
        if ($graph_max - $graph_min > 2) {
          $graph_min = int($graph_min);
          $graph_max = int($graph_max + 1) unless $graph_max == int($graph_max);
          $int_ticks = 1;
        }

	# Now make sure the graph_min and graph_max are multiples of the
	# interval, and the range will take care of itself
	if (int($graph_min / $interval) != $graph_min / $interval) {
	    my $r = $graph_min - (int($graph_min/$interval) * $interval);
	    $graph_min -= $r;
	    $graph_min = 0 if $graph_min < 0;
	}
	if (int($graph_max / $interval) != $graph_max / $interval) {
	    $graph_max = (int($graph_max/$interval + 1) * $interval);
	}

	# In some cases (graph scale!) it may be necessary to show more than
	# three "significant" figures.
	my $min_log = 2;
	while(significant($graph_min, $min_log) == significant($graph_min + $interval, $min_log)) {
	    $min_log++;
	}

	# Figure out the font size for the ticks
	my $longtick = 0;
	for my $num (sort { $a <=> $b } 
		     ($graph_min, $graph_max, map { $results{$what}->{$_}->[2] } keys %{$results{$what}})) {
            $num = significant($num, $min_log);
	    my $tmpsize = $p->string_calcsize($num,
					      $inch * 2, $bar_height / 2, 1,
					      "ERROR: graph label \"$num\" is too long!");
	    if ($tmpsize < $size) {
	        $size = $tmpsize;
	        $longtick = $num;
	    }
        }
	return 0 if $size <= 0;

	# Figure out what size to make the fonts
	$p->set_font($prop_bold_font, undef, 1);
	my $border = $inch / 32;
	my ($textx, $texty) = (($inch * 1) - ($border * 2),
			       ($bar_height * 2) - ($border * 2));
	my $longbench = (keys %benchmarks)[0];
	my $longbenchlen = $p->stringwidth($longbench);
	foreach my $bench (sort keys %benchmarks) {
	  # Find the benchmark with the longest name in the current font
	  # size
	  my $len = $p->stringwidth($bench);
	  if ($len > $longbenchlen) {
	    $longbench = $bench;
	    $longbenchlen = $len;
	  }
	}
	# Now that the longest is known, figure out the size for all based
	# just on that, and set it.
	$tmpsize = $p->string_calcsize($longbench, $textx, $texty, 5,
				       'ERROR: The benchmark label area is too small!');
	$p->set_font($prop_bold_font, $tmpsize, 1);
	# AND cause the font to be set now (rather than at show time)
	$p->set_font_really() unless $mode eq 'PDF';

	# Figure out the minimum width for the label
	for my $bench (sort keys %{$config->benchmarks}) {
	    my $w = $p->stringwidth($bench) + ($border * 3);
	    $label_width = $w if ($w > $label_width);
	}
	$graph_x += $label_width;
	$graph_width -= $label_width;

	my $rate_x = 0;
	if (istrue($config->{'rate'})) {
	    # Make space for the copies; 4 digits should be enough
	    my @copies_width = ($p->stringwidth('Copies', $size) + ($border * 2),
				$p->stringwidth('0000', $size) + ($border * 2));
	    my $copies_width = ::max(@copies_width);
	    $rate_x = $copies_width;
	    $graph_x += $copies_width;
	    $graph_width -= $copies_width;
	}

	# Finally, re-figure the scale and range based on the adjustments
	# made earlier.
	$range = $graph_max - $graph_min;
	$range++ unless $range;   # This shouldn't ever happen...
	my $scale = $graph_width / $range;

	# Set up the benchmark graphs
	# This is done before the ticks because it will destroy the mean
	# lines.  Likewise, we want to have the benchmark graph
	# data overlay the mean lines; thus the split between setup and
	# graphing.
	my $bar_y = $bar_height * -1;
	foreach my $bench (sort keys %benchmarks) {
            $benchmarks{$bench}->{'y'} = $bar_y;  # For making the graph later
	    setup_bm_graph($p, $graph_x, $bar_y, $graph_width, $bar_height, $border, $bench, $rate_x);
	    $bar_y -= $bar_height * 2;
	}

	$p->set_font($prop_bold_font, $size);

	# Make the next-to-scale labels (the font size should be okay)
	if ($rate_x) {
	    my $string_base = ((-$bar_height - $border) / 2) -
		(($p->string_CapHeight() + $p->string_Descender) / 2) + 0.5;
	    $p->moveto($graph_x - $rate_x, 0);
	    $p->rlineto(0, -$bar_height);
	    $p->moveto($graph_x, 0);
	    $p->rlineto(0, -$bar_height);
	    $p->stroke();
	    $p->set_text_pos($graph_x - ($rate_x / 2), $string_base);
	    $p->show_center('Copies');
	}

	# Figure out the font size for the scale and graph numbers
	$size = $p->string_calcsize(significant($graph_max, $min_log), $inch,
				    $bar_height / 2, 5,
				    'ERROR: The graph scale area is too small!');

	$p->set_font($prop_font, $size);
	$p->setlinewidth(0.25);

        if ($config->size eq 'ref') {

            # Do the ticks and numbers for everything
            my $black = 1;
            my $mean = 0;
            my @spots = ();	# List of x values where ticks are
            my %ticked = (); # To keep track of which ticks have been printed
	    my @avoid = ();	# Like @spots, but for number_tick

	    # Generate the list of ticks
	    my @ticks = ();
	    foreach my $num (1 .. ($range / $interval) - 1) {
		push @ticks, $graph_min + ($interval * $num);
	    }

	    # Tick the graph_min; make it shift to the right
	    my $xval = $graph_x;
	    push @avoid, [ $xval, $xval, 'graph_min' ];
	    my $print_num = significant($graph_min, $min_log);
	    $ticked{$print_num}++;
            $print_num = '0' if ($graph_min <= 0.0001);
	    push @spots, number_tick($p, $xval, -$bar_height,
				     $tick_height, 'above',
				     $print_num, \@avoid, 'right',
                                     $tick_height * 0.2);

	    # Tick the graph_max; make it shift to the right
	    $xval = $graph_x + ($graph_max - $graph_min) * $scale;
	    push @avoid, [ $xval, $xval, 'graph_max' ];
	    $print_num = significant($graph_max, $min_log);
	    $ticked{$print_num}++;
	    push @spots, number_tick($p, $xval, -$bar_height,
				     $tick_height, 'above',
				     $print_num, \@avoid, 'left',
                                     $tick_height * 0.2);

            for my $num (@ticks) {
		next if (!defined($num) || $num eq '--');
		$print_num = significant($num, $min_log);
		next if $ticked{$print_num};
		next if ($num > $graph_max);

		my $this_tick = $tick_height;
		$ticked{$print_num}++;
		$xval = $graph_x + ($num - $graph_min) * $scale;
                if (($int_ticks && ($num - int($num) > 0.0001)) ||
                    !label_ok($xval, $p->stringwidth($print_num) * 1.2, \@spots)) {
                  undef $print_num;
                }

		if (!defined($print_num)) {
		    # Ticks without numbers will be smaller by half
		    $this_tick /= 2;
		}

		my $interval = number_tick($p,
					   $xval, -$bar_height,
					   $this_tick, 'above',
					   $print_num, undef, undef,
                                           $tick_height * 0.2);
		push @spots, $interval if defined($interval);
	    }

	    # Draw the mean lines
            $p->gsave();
            {
              my $tmpstr = $config->baseunits.' = '.significant($config->basemean);
              my @avoid_dirs = qw(left right);

              my $basemean = $config->basemean;
              undef $basemean if $basemean eq '--';
              my $basedir = 0;
              my $peakmean = $config->peakmean;
              undef $peakmean unless $peakmean+0 > 0;
              my $peakdir = 1;

              my $min_mean = ::min($basemean, $peakmean);

              # Figure out the font size for the mean labels
              $size = $p->string_calcsize($tmpstr, $inch + $min_mean * $scale,
                                          $bar_height * 0.8);

              if (defined($basemean) && defined($peakmean) &&
                  $basemean > $peakmean) {
                $basedir = 1 - $basedir;
                $peakdir = 1 - $peakdir;
              }

              $p->set_font($prop_bold_font, $size);
              my $tmpwidth = $p->stringwidth($tmpstr);
              $p->setlinewidth(0.5);
              if (defined($peakmean)) {
                $p->gsave();
                {
                  $p->setrgbcolor(@peak_color);
                  $p->setdash(1.5);
                  $tmpstr = $config->peakunits.' = '.significant($peakmean);
                  $xval = $graph_x + ($peakmean - $graph_min) * $scale;
                  if (($peakdir && ($xval + $tmpwidth > $graph_x + $graph_width)) ||
                      (!$peakdir && ($xval - $tmpwidth < $graph_x))) {
                    # Make it dodge to the other side if it won't fit inside
                    # the border.
                    $peakdir = 1 - $peakdir;
                  }
                  number_tick($p,
                              $xval, -$bar_height,
                              $graph_height - ($bar_height * 2) + ($bar_height * 0.10), 'below',
                              $tmpstr, \@avoid, $avoid_dirs[$peakdir],
                              $bar_height * 0.10);
                  push @avoid, [ $xval, $xval, 'peak_mean' ];
                }
                $p->grestore();
              }

              if (defined($basemean)) {
                $tmpstr = $config->baseunits.' = '.significant($basemean);
                $xval = $graph_x + ($basemean - $graph_min) * $scale;
                if (($basedir && ($xval + $tmpwidth > $graph_x + $graph_width)) ||
                    (!$basedir && ($xval - $tmpwidth < $graph_x))) {
                  # Make it dodge to the other side if it won't fit inside
                  # the border.
                  $basedir = 1 - $basedir;
                }
                number_tick($p,
                            $xval, -$bar_height,
                            $graph_height - ($bar_height * 3) + ($bar_height * 0.10), 'below',
                            $tmpstr, \@avoid, $avoid_dirs[$basedir],
                            $bar_height * 0.10);
                push @avoid, [ $xval, $xval, 'base_mean' ];
              }
            }
            $p->grestore();

            # Now do the benchmark graphs
	    %results = %{$config->results};
	    my $geom = {
		'tune_pad'   => $inch / 64, # Space between bars for each tuning level
		'bar_height' => $bar_height,
		'bar_x'      => $graph_x,
		'graph_min'  => $graph_min,
		'graph_max'  => $graph_max,
		'scale'      => $scale,
		'rate_x'     => $rate_x,
		'border'     => $border,
	    };

            # Finally, really draw the graphs
            foreach my $bench (sort keys %results) {
                if (!exists $benchmarks{$bench}) {
                  ::Log(0, "\nERROR: Results from $bench, which is not in the current benchset!\n");
                  return 0;
                }
                $bar_y = $benchmarks{$bench}->{'y'};
                do_bm_graph($p, $bar_y, $geom, $what_key{$what},
                            $results{$bench}, \@avoid);
            }

        }
    }
    $p->grestore();
    return $graph_height;
}

sub do_bm_graph {
    # Actually display the graph of the results data.
    # NOTE: This will need to be changed if more than two tuning levels
    #       are to be plotted at once.
    my ($p, $y, $geom, $what, $results, $avoid) = @_;

    #print "\ndo_bm_graph(p=$p, y=$y, geom=$geom, what=$what, results=$results, avoid=$avoid)\n";

    my ($bar_x, $bar_height, $graph_min, $graph_max, $border) = 
	  @{$geom}{qw(bar_x bar_height graph_min graph_max border)};
    my $mid = $bar_height;
    my $min = 1<<31;
    my $basepeak = undef;	# Unless the results say otherwise...
    my $tick_height = $bar_height * 0.375;
    my %copies = ();
    my $maxcopies = 0;

    # Scan the results to find the min and max copies and whether or not
    # basepeak is going on...
    foreach my $tune (sort keys %{$results}) {
        next unless ref($results->{$tune});
	foreach my $run (@{$results->{$tune}->{'data'}}) {
            # Once it gets set, don't let it get unset (except when the
            # first run says it WAS, and the second run says it WASN'T).
            # It shouldn't happen.
            $basepeak = $run->{'basepeak'} unless defined($basepeak) && ($basepeak == 0);
	    my $data = $run->{$what};
	    $min     = $data if ($min     > $data);
	    if ($run->{'selected'}) {
		$copies{$tune} = $run->{'clcopies'};
		$maxcopies = $copies{$tune} if $copies{$tune} > $maxcopies;
	    }
	}
    }
    # Even if basepeak is set, don't do the basepeak bar unless peak was run
    $basepeak = undef unless exists($results->{'peak'});

    my $copies_size;
    if ($geom->{'rate_x'}) {
	$copies_size = $p->string_calcsize($maxcopies,
					   $geom->{'rate_x'}, $bar_height - ($border * 2),
					   2, 'ERROR: The space for the number of copies is too small!');
    }

    $p->gsave();
    {
        $p->translate($bar_x, $y - ($bar_height * 2));

	# Now get down to plotting the data
	my $dir = 'center';
	my $black = 1;
        foreach my $tune (sort keys %{$results}) {
	    next unless ref($results->{$tune});
	    next if (defined($basepeak) && $basepeak && $tune eq 'base');
	    my $tunemax = 0;
	    my $base_y = $mid;
	    my $this_tick = $tick_height;
	    my @avoid = map { fixup_avoid(-$bar_x, $_) } @{$avoid} if ref($avoid) eq 'ARRAY';

	    if (defined($basepeak) && $basepeak) {
	        $p->setgray(0) unless $black;
		$black = 1;
		$dir = 'center';
		$base_y -= $bar_height / 4;
		$this_tick = $bar_height / 2;
	    } elsif ($tune eq 'peak') {
	        $p->setrgbcolor(@peak_color) if $black;
		$black = 0;
		$dir = 'above';
		$base_y += $geom->{'tune_pad'};
	    } else {
	        $p->setgray(0) unless $black;
		$black = 1;
		$dir = 'below';
		$base_y -= $geom->{'tune_pad'};
	    }

	    if ($geom->{'rate_x'} && exists($copies{$tune})) {
		$p->gsave();
		{
		    my $string_base = $mid -
			(($p->string_CapHeight() + $p->string_Descender) / 2);
		    if ($dir eq 'center') {
			$p->set_text_pos(-$border, $string_base);
		    } elsif ($dir eq 'above') {
			$p->set_text_pos(-$border, $string_base + $bar_height / 2);
		    } elsif ($dir eq 'below') {
			$p->set_text_pos(-$border, $string_base - $bar_height / 2);
		    }
		    $p->set_font(($p->get_font())[0], $copies_size);
		    $p->show_right($copies{$tune});
		}
		$p->grestore();
	    }

	    my @spots = ();
            my $plotted = 0;
            foreach my $run (sort {
		$b->{'selected'} <=> $a->{'selected'} ||
		$a->{'iteration'} <=> $b->{'iteration'}
	    } @{$results->{$tune}->{'data'}}) {
	        my $data = $run->{$what};
                next if ($data < $graph_min);
                if ($data > $graph_max) {
                  # Arrange for a line to appear at least
                  $tunemax = $graph_max;
                  $plotted++;
                  next;
                }
		my $text = significant($data);
		my $sw = $p->stringwidth($text);
	        $tunemax = $data if ($tunemax < $data);
		my $x = ($data - $graph_min) * $geom->{'scale'};
		# Print the data value associated with the tick if possible.
		# (The committee has decided that it is only possible to
		#  print the median.  This is pretty close to reality most
		#  of the time.)
		# This is why it's important to do the selected value first.
		my $tx = number_tick($p, $x, $base_y,
				     $this_tick, $dir,
                                      ($run->{'selected'} &&
				      label_ok($x, $sw, \@spots)) ? $text : undef,
				     \@avoid, undef, $tick_height * 0.2);
		if (defined($tx) && ref($tx) eq 'ARRAY') {
		    push @avoid, $tx;
		    push @spots, $tx;
		}
                $plotted++;
	    }
	    if ($plotted) {
		# Connect the ticks
		my $tick_center = $base_y;
		if (defined($basepeak) && $basepeak) {
		    $tick_center = $mid;
		} elsif ($dir eq 'above' || $dir eq 'center') {
		    $tick_center += $this_tick / 2;
		} else {
		    $tick_center -= $this_tick / 2;
		}
		$p->moveto(($tunemax - $graph_min) * $geom->{'scale'}, $tick_center);
                $p->gsave();
                {
                  if (defined($basepeak) && $basepeak) {
                    $p->setlinewidth(0.5);
                    $p->lineto(0, $tick_center);
                    $p->stroke();
                  } else {
                    $p->setlinewidth(0.35);
                    $p->lineto(0, $tick_center);
                    $p->lineto(0, $mid);
                    $p->stroke();
                  }
                }
                $p->grestore();
	    }
	}
    }
    $p->grestore();
}

sub fixup_avoid {
    # Add a fixed offset to an avoid list.  This takes care of the translation
    # problem...
    my ($offset, $point) = @_;
    my $newpoint;

    if (ref($point) eq 'ARRAY') {
	@{$newpoint} = @{$point};
	$newpoint->[0] += $offset;
	$newpoint->[1] += $offset;
    } else {
	# Easy!
	$newpoint = $point + $offset;
    }
    return $newpoint;
}

sub enclosed_number_tick {
    my ($p, $x, $y, $width, $side, $type, $gray, $str) = @_;
    my $tickwidth = ($side eq 'left') ? -$width : $width;
    $tickwidth /= 2;

#print "enclosed_number_tick($p, $x, $y, $width, $side, $type, $str): $tickwidth\n";
    $p->moveto($x, $y);
    $p->lineto($x + $tickwidth, $y);
    $p->stroke();

    return 0 if (!defined($str) || ($str eq ''));

    my $center = $x + ($tickwidth * 1.5);
    my $radius = $p->string_CapHeight() / 1.5;
    if ($type eq 'circle') {
        $p->circle_fill($center, $y, $radius, $gray, undef, 1);
    } else {
        $p->rect_fill($x + $tickwidth, $y - $radius,
		      $tickwidth, $radius * 2, $gray, undef, 1);
    }
    $p->set_text_pos($center - ($p->stringwidth($str) / 2), $y - ($p->string_CapHeight() / 2));
    $p->show($str);
    return $radius;
}

sub setup_bm_graph {
    my ($p, $x, $y, $width, $bar_height, $border, $benchmark, $rate_x) = @_;
    #print "setup_bm_graph(p=$p, x=$x, y=$y, width=$width, bar_height=$bar_height, border=$border, benchmark=$benchmark, rate_x=$rate_x)\n";

    # Do the shaded bar for base
    $p->rect_fill($x - $rate_x + 0.3, $y - ($bar_height * 2),
		  $width + $rate_x - 0.8, $bar_height,
		  $base_shade);

    # Do the outline for the full bar
    $p->rect(0, $y, $x + $width, -$bar_height * 2);

    # Do the outline for the label area
    $p->rect(0, $y, $x - $rate_x, -$bar_height * 2);
    $p->stroke();

    if ($rate_x) {
	# Do the outline for the # of copies area
	$p->rect($x - $rate_x, $y, $rate_x, -$bar_height * 2);
	$p->stroke();
    }

    # Figure out the bounding box for the label
    my $string_base = $y - ((($bar_height * 2) - $border) / 2) -
                       (($p->string_CapHeight() + ($p->string_Descender / 2)) / 2);

    # Now write in the name of the benchmark
    $p->set_text_pos($x - $rate_x - ($border * 1.5), $string_base);
    $p->linkto_right($benchmark, undef, 'isbench' => 1);
}

sub label_ok {
  my ($xpos, $size, $spots) = @_;

  # Check to see if xpos occupies a position within +/- size of any position
  # in spots
  foreach my $spot (@{$spots}) {
      if (ref($spot) eq 'ARRAY') {
	  next if $xpos > ($spot->[1] + $size / 2);
	  next if $xpos < ($spot->[0] - $size / 2);
      } else {
	  next if $xpos > ($spot + $size / 2);
	  next if $xpos < ($spot - $size / 2);
      }
      return 0;
  }
  return 1;
}

sub number_tick {
    my ($p, $tick_x, $tick_y, $tick_height, $side, $label, $avoid, $dir,
        $addspace) = @_;
    # This makes a vertical tick mark (and optionally a label).  The label
    # text is always centered.
    # tick_y is the start of the tick; so if the label is "above", the
    # tick goes up.  Otherwise it goes down.  Either way, the text is at
    # the endpoint farthest from tick_y.

    #print "number_tick(p=$p, tick_x=$tick_x, tick_y=$tick_y, tick_height=$tick_height, side=$side, label=$label, avoid=$avoid, dir=$dir, addspace=$addspace)\n";
 

   my $text_y = $tick_y;

    if ($side eq 'above') {
        # This is what happens by default
    } elsif ($side eq 'below') {
        $addspace *= -1;
	$tick_height *= -1;
	$text_y -= $p->string_CapHeight() - $p->string_Descender();
    }
    $text_y += $tick_height - $p->string_Descender / 2;
    $addspace = 0 unless defined($addspace);

    # Go ahead and draw the tick
    $p->moveto($tick_x, $tick_y);
    $p->rlineto(0, $tick_height - $addspace);
    $p->stroke();

    # Now the label
    if (defined($label) && $label ne '') {

	# This business is to slide labels to one side or the other if they'll
	# cross over one of the x values in the 'avoid' array.
	my $clash = 0;
	my %shift;
	my $shift = 0;
	my $sw = $p->stringwidth($label.' ') / 2;
	my @sort_try;
	if (defined($avoid) && ref($avoid) eq 'ARRAY') {
	    if (defined($dir) && ($dir eq 'left' || $dir eq 'right')) {
		@sort_try = (0);
	    } else {
		@sort_try = (0, 1);
	    }
	    foreach my $sort_direction (@sort_try) {
		$sortdir = $sort_direction;
		%shift = ('left' => 0, 'right' => 0);
		$clash = 1;
		$shift = 0;
		while ($clash) {
		    $clash = 0;
		    foreach my $x (sort byspot @{$avoid}) {
			my $usedspot = $x;
			my $fromlabel = '';
			my @fromcaller = ();
			if (ref($x) eq 'ARRAY') {
			    $usedspot = ($x->[0] + $x->[1]) / 2;
			    $fromlabel = $x->[2];
			    @fromcaller = @{$x}[3..$#{$x}];
			}
			if (abs($tick_x - $usedspot + $shift{'left'}) < $sw) {
			    $clash = 1;
			    if ($tick_x < $usedspot) {
				# The tick is to the left of the obstruction
				$shift{'left'} -= $sw - abs($tick_x - $usedspot + $shift{'left'});
			    } else {
				# The tick is to the right of the obstruction.
				# So it's stupid to shift more than $sw, but
				# that's what this means.
				$shift{'left'} -= $sw + abs($tick_x - $usedspot + $shift{'left'});
			    }
			}
			if (abs($tick_x - $usedspot + $shift{'right'}) < $sw) {
			    $clash = 1;
			    if ($tick_x < $usedspot) {
				# The tick is to the left of the obstruction.
				# So it's stupid to shift more than $sw, but
				# that's what this means.
				$shift{'right'} += $sw + abs($tick_x - $usedspot + $shift{'right'});
			    } else {
				# The tick is to the right of the obstruction.
				$shift{'right'} += $sw - abs($tick_x - $usedspot + $shift{'right'});
			    }
			}
		    }

		    if ($clash) {
			# Was a direction specified?
			if (defined($dir) && ($dir eq 'left' || $dir eq 'right')) {
			    $shift = $shift{$dir};
			} else {
			    # Try to figure out what the best direction is
			    if (abs($shift{'left'}) < abs($shift{'right'})) {
				$shift = $shift{'left'};
			    } else {
				$shift = $shift{'right'};
			    }
			}
		    }
		}
		$sort_try[$sortdir] = $shift;
	    }
	}
	if (!defined($dir) || ($dir ne 'left' && $dir ne 'right')) {
	    # Pick the direction that shifts the label the least
	    $shift = (sort { abs($a) <=> abs($b) } @sort_try)[0];
	}
	$tick_x += $shift;

	# After all that, show the label
	$p->set_text_pos($tick_x, $text_y);
	$p->show_center($label);
	return [ $tick_x - $sw, $tick_x + $sw, $label, caller() ];
    }
    return undef;
}

sub SPEC_table {
    my ($p, $x, $y, $width, $height, $config) = @_;
    my @titles;
    my $table_width = $width;
    my $label_width = 0;
    my $size = 10;
    my $hsize;
    my $msg = 'Results appear in the order in which they were run. Bold underlined text indicates a median measurement.';
    my $max_range_string = '00000.000';

    # Figure out the minimum width for the label
    for my $bench (sort keys %{$config->benchmarks}) {
	my $w = $p->stringwidth($bench, $size);
      	$label_width = $w if ($w > $label_width);
    }

    if (istrue($config->{'rate'})) {
	@titles = (
	    [ 0, 1,   'Benchmark' ],
	    [ 1, 0.9, 'Copies'    ],
	    [ 1, 0,   'Seconds'   ],
	    [ 1, 0.3, 'Ratio'     ],
	    [ 1, 0,   'Seconds'   ],
	    [ 1, 0.3, 'Ratio'     ],
	    [ 1, 0,   'Seconds'   ],
	    [ 1, 1,   'Ratio'     ],
	    [ 2, 0.9, 'Copies'    ],
	    [ 2, 0,   'Seconds'   ],
	    [ 2, 0.3, 'Ratio'     ],
	    [ 2, 0,   'Seconds'   ],
	    [ 2, 0.3, 'Ratio'     ],
	    [ 2, 0,   'Seconds'   ],
	    [ 2, 1,   'Ratio'     ],
	);
        $max_range_string = '0000.000';
    } else {
	@titles = (
	    [ 0, 1,   'Benchmark' ],
	    [ 1, 0,   'Seconds'   ],
	    [ 1, 0.3, 'Ratio'     ],
	    [ 1, 0,   'Seconds'   ],
	    [ 1, 0.3, 'Ratio'     ],
	    [ 1, 0,   'Seconds'   ],
	    [ 1, 1,   'Ratio'     ],
	    [ 2, 0,   'Seconds'   ],
	    [ 2, 0.3, 'Ratio'     ],
	    [ 2, 0,   'Seconds'   ],
	    [ 2, 0.3, 'Ratio'     ],
	    [ 2, 0,   'Seconds'   ],
	    [ 2, 1,   'Ratio'     ],
	);
    }

    my $num_width   = ($table_width - $label_width) / (@titles-1);
    my @cols = (0);
    for (my $i = 0; $i < @titles; $i++) {
        push @cols, $label_width + ($num_width * $i);
    }

    # If there's not enough room on the page to print the title, the
    # headings, and at least one row of data, just start a new page
    if ($height < 30 + ($size * 3)) {
      $p->grestore();
      end_page($p);
      ($width, $height) = SPEC_newpage($p, $config);
    }

    my $divide_y = undef;
    $p->gsave();
    {
	$p->translate($x, 0);

	$height = SPEC_table_title($p, $width, $height,
				   \@cols, \@titles,
				   $size, $table_width, 0);
	# Remember the y pos so a dividing line can be drawn later
	# (It could be done in SPEC_table_title, but then the shading in the
	#  benchmark name boxes will destroy it.)
	$divide_y = $height;

	# Get ready to make the table
	$size = 10;
	$p->set_font($prop_font);
	my $dp = { 'base' => [ 2147483647, 2147483647 ],
		   'peak' => [ 2147483647, 2147483647 ] };
	for my $bench (sort keys %{$config->benchmarks}) {
	    my $tmp = 0;
	    for my $tune (qw(peak base)) {
		if ($config->valid($bench, $tune)) {
		    $tmp = $config->runtime($bench, $tune);
		    $max{'time'}->{$tune} = $tmp if $tmp > $max{'time'}->{$tune};
		    $dp->{$tune}->[0] = $tmp if $tmp < $dp->{$tune}->[0];
		    $tmp = $config->ratio($bench, $tune);
		    $max{'ratio'}->{$tune} = $tmp if $tmp > $max{'ratio'}->{$tune};
		    $dp->{$tune}->[1] = $tmp if $tmp < $dp->{$tune}->[1];
		}
	    }
	}
        for my $tune (qw(base peak)) {
	    for (my $i = 0; $i < 2; $i++) {
		if ($dp->{$tune}->[$i] && ($dp->{$tune}->[$i] != 2147483647)) {
		    $dp->{$tune}->[$i] = log($dp->{$tune}->[$i])/log(10);
		}
	    }
	}

	if (@{$config->{'nc'}}+0) {
	    # Show them nothing?
	    $dp->{'base'} = [0,0];
	    $dp->{'peak'} = [0,0];
	}

        # Do the table
	$hsize = $size * 1.5;
	my $rowshade = 1;
        my @borders = ();
	my $border = ($num_width - $p->stringwidth($max_range_string)) / 2;
	$border = 0 if ($border < 0); # This would be bad.
	for my $bench (sort keys %{$config->benchmarks}) {
	    # Center the largest possible number...
	    my $string_base = ($hsize / 2) -
		              (($p->string_CapHeight() + $p->string_Descender) / 2) - 1;
	    if ($height <= $size + 13) {
	        # Time for a new page

		# Do the borders that need to be done
		do_borders($p, @borders);
		@borders = ();

		# Do the black dividing lines
		$p->setgray(0);
		$p->setlinewidth(0.3);

		# In PDF, rects are filled by default!
		$p->moveto(0, $height - 13);
		$p->lineto(0, $divide_y);
		$p->lineto($width, $divide_y);
		$p->lineto($width, $height - 13);
		$p->rect(0, $height - 13, $width, 13);
		$p->stroke();

		# And the message about the continuation and format
		$height -= 12;
		$p->string_fit_center("Table continues on next page. $msg",
				      $border, $height,
				      $width - ($border * 2), 10);

	        $p->grestore();	# To match the one at the top of this block
	        $p->grestore();	# To match the one you get from newpage
		end_page($p);
		($width, $height) = SPEC_newpage($p, $config);
		$height = SPEC_table_title($p, $width, $height,
					   \@cols, \@titles,
					    $size, $table_width, 1);
		$divide_y = $height;

		$p->gsave(); # To match the one at the end...
		$p->set_font($prop_font);
	    }
	    $height -= $hsize;
	    my @tmp = ( [ $base_shade, 1, 0, 'left',  $border, $bench, undef, undef, 1 ] );
	    foreach my $tune (qw(base peak)) {
		my $printcount = 0;
		my $ispeak = $tune eq 'peak';

                # This is a little sneaky... fix up the _previous_ column's
                # record so that its right border line is thicker
                $tmp[$#tmp]->[1] = 1;

		if (istrue($config->{'rate'})) {
		    push (@tmp, [ $base_shade, 1, 0, 'right', $border,
				  $config->copies($bench, $tune), undef, $ispeak ]);
		}

		if (grep { /^${tune}$/ } @{$config->tunelist}) {
		    my @results = $config->benchmark_results_list($bench, $tune);
		    my @idx = (0, 1, 2);
                    if (@results+0 > 3) {
                      # Sigh.  Find min/median/max.  We are in _trouble_ if
                      # more than (n-3) of the results are exactly the same.
                      my ($min, $median, $max) = (1 << 31, undef, -1);
                      my $count = 0;
                      foreach my $res (sort { $a->{'iteration'} <=> $b->{'iteration'} } @results) {
                          if ($res->{'selected'} && !defined($idx[1])) {
                              $idx[1] = $count;
                          }
                          if ($res->{'ratio'} < $min) {
                              $min = $res->{'ratio'};
                              $idx[0] = $count;
                          } 
                          if ($res->{'ratio'} > $max) {
                              $max = $res->{'ratio'};
                              $idx[2] = $count;
                          }
                          $count++;
                      }
                    }
		    foreach my $idx (sort @idx) {
			next unless defined($idx);
			next unless isa($results[$idx], 'HASH');
			my $res = $results[$idx];
			if (@{$config->{'nc'}}+0) {
			    push @tmp, [ 0, 0, 0, 'right', $border, 'NC', 0, $ispeak ];
			    push @tmp, [ 0, 0.3, 0, 'right', $border, 'NC', 0, $ispeak ];
			} elsif (!$config->valid($bench, $tune)) {
			    push @tmp, [ 0, 0, 0, 'right', $border, 'X', undef, $ispeak ];
			    push @tmp, [ 0, 0.3, 0, 'right', $border, 'X', undef, $ispeak ];
			} else {
			    push @tmp, [ 0,
                                         0,
					 $res->{'selected'},
					 'decimal',
					 $border,
					 $res->{'reported_time'},
					 $dp->{'base'}->[0],
					 $ispeak ];
			    if ($config->size eq 'ref') {
				push @tmp, [ 0,
                                             0.3,
					     $res->{'selected'},
					     'decimal',
					     $border,
					     $res->{'ratio'},
					     $dp->{'base'}->[1],
					     $ispeak ];
			    } else {
				push @tmp, [ 0, 0.3, 0, 'right', $border, '--', undef, $ispeak ];
			    }
			}
			$printcount += 2;
		    }
		}
		for(; $printcount < 6; $printcount += 2) {
		    # Put in empty columns
		    push (@tmp, [ 0, 0, 0, 'right', $border, '', undef, $ispeak ]);
		    push (@tmp, [ 0, 0.3, 0, 'right', $border, '', undef, $ispeak ]);
#		    push (@tmp, [ 0, 0, 0, 'right', $border, 'run.'.$tune, undef, $ispeak ]);
#		    push (@tmp, [ 0, 0.3, 0, 'right', $border, 'rat.'.$tune, undef, $ispeak ]);
		    }
	    }
            # This is a little sneaky... fix up the _previous_ column's
            # record so that its right border line is thicker
            $tmp[$#tmp]->[1] = 1;

	    for (my $i = 0; $i < @tmp; $i++) {
		my ($shade, $rb, $ul, $align, $off, $val, $arg1, $ispeak, $isbench) = @{$tmp[$i]};
                my $url = undef;
                if ($isbench) {
                    # Link it
                    $url = "http://www.spec.org/auto/$::lcsuite/Docs/${val}.html";
                }

		my $col_width = $cols[$i+1]-$cols[$i];
		if ($shade) {
                    $p->rect_fill($cols[$i], $height, $col_width, $hsize, $shade);
		}
		$p->gsave();
		{
                    $p->setlinewidth(0.10);
                    $p->setgray(0.50);
		    # To have alternating rows shaded, just remove the '0 &&' 
                    if (0 && !$shade && $rowshade) {
                        $p->rect_fill($cols[$i], $height, $col_width, $hsize, 0.93, undef, 1);
                    } else {
                        $p->rect($cols[$i], $height, $col_width, $hsize);
                        $p->stroke();
                    }
                    if ($rb) {
                        # Don't do this now, or the shaded boxes will destroy
                        # half of the line width, and it'll look strange.
                        push @borders, [ $cols[$i] + $col_width, $height, $hsize, $rb ];
                    }
		}
		$p->grestore();
		if ($ispeak) {
		    $p->setrgbcolor(@peak_color);
		}
                if ($ul) {
                    $ul = 0.5;  # Set the underline width
                    $p->set_font($prop_bold_font);
                }
		if ($align eq 'left') {
		    $p->set_text_pos($cols[$i] + $off, $height + $string_base);
		    $p->show($val, $ul, $url);
		} elsif ($align eq 'right') {
		    $p->set_text_pos($cols[$i] + $col_width - $off, $height + $string_base);
		    $p->show_right($val, $ul, $url);
		} elsif ($align eq 'decimal') {
		    $val = significant($val);
		    my $s = figure_dp($val, 3, $arg1);
		    my $w = $p->stringwidth($s);
		    $p->set_text_pos($cols[$i] + $col_width - $off - $w, $height + $string_base);
		    $p->show_right($val, $ul, $url);
		}
		$p->setgray(0) if $ispeak;
                $p->set_font($prop_font) if $ul;
	    }
        }

	# Do those darker borders
	do_borders($p, @borders);

	# Do the black dividing lines
	$p->setgray(0);
	$p->setlinewidth(0.3);

	# In PDF, rects are filled by default!
	$p->moveto(0, $height - 13);
	$p->lineto(0, $divide_y);
	$p->lineto($width, $divide_y);
	$p->lineto($width, $height - 13);
	$p->rect(0, $height - 13, $width, 13);
	$p->stroke();

	# And the message about the continuation and format
	$height -= 12;
	$p->string_fit_center($msg,  $border, $height,
			      $width - ($border * 2), 10);
    }
    $p->grestore();
    return $height;
}

sub do_borders {
    my ($p, @borders) = @_;

    # Do the darker borders
    if (@borders) {
	$p->setgray(0);
	#$p->setrgbcolor(@error_color);
	for my $borderref (@borders) {
	    my ($x, $y, $h, $width) = @{$borderref};
	    $p->setlinewidth($width);
	    $p->moveto($x, $y);
	    $p->rlineto(0, $h);
	}
	$p->stroke();
    }
}

sub SPEC_table_title {
    my ($p, $width, $height, $cols, $titles, $size, $table_width, $cont) = @_;

    # Add a dummy title to ensure that the last title label gets printed
    push @{$titles}, [ 0, undef ];
    push @{$cols}, $cols->[$#{$cols}];

    # Do the title
    $p->moveto(0, $height);
    $p->lineto($width, $height);
    $p->stroke();
    $height -= 20;
    $p->set_font($prop_bold_font, 16);
    $p->set_text_pos($width/2, $height);
    if ($cont) {
        $p->linkto_center('Results Table (Continued)');
    } else {
        $p->linkto_center('Results Table');
    }
    $height -= 10;

    # Do the title boxes

    $height -= ($size * 1.5);

    $p->set_font($prop_bold_font, 8.5);
    my $num_titles = @{$titles}+0;
    my ($font, $fontsize) = $p->get_font();
    my $currstate = 0;
    my $titlebarx = 0;
    my @borders = ();
    for (my $i = 0; $i < $num_titles; $i++) {
	my $col_width = $cols->[$i+1] - $cols->[$i];
	my ($title, $rb, $str) = @{$titles->[$i]};
	$p->gsave();
	{
	    $p->setgray(0);
	    if ($title == 1) {
		# Base, so shade it
		$p->rect_fill($cols->[$i], $height, $col_width, $size * 2,
			      $base_shade, undef, 1);
	    } else {
		$p->rect($cols->[$i], $height, $col_width, $size * 2);
		$p->stroke();
	    }
	}
	$p->grestore();
	if ($title != $currstate) {
	    if ($currstate == 0) {
		$titlebarx = $cols->[$i];
	    } else {
		# The end of a title bar; draw it and reset the state
		my $barwidth = $cols->[$i] - $titlebarx;
		# Plop down a box to remove the lines drawn previously
		$p->rect_fill($titlebarx, $height + $size,
			      $barwidth, $size,
			      ($currstate == 1) ? $base_shade : 1, undef, 1);

		$p->set_text_pos($titlebarx + ($barwidth / 2),
				 $height + $size - $p->string_Descender());
		my $text;
		if ($currstate == 1) {
		    $text = 'Base';
		} elsif ($currstate == 2) {
		    $text = 'Peak';
		} else {
		    $text = $currstate;	# This should never happen
		}
		if ($currstate == 2) {
		    $p->gsave();
		    $p->setrgbcolor(@peak_color);
		}
		$p->show_center($text);
		$p->grestore() if ($currstate == 2);
		$titlebarx = $cols->[$i];
	    }
	    $currstate = $title;
	}
        if ($rb) {
            push @borders, [ $cols->[$i] + $col_width, $height,
                             ($rb < 1) ? $size : $size * 2, # Hack here!
                             $rb ];
        }
	if (defined($str)) {
	    if ($currstate == 2) {
		$p->gsave();
		$p->setrgbcolor(@peak_color);
	    }
	    $p->set_text_pos($cols->[$i] + ($col_width / 2), $height - $p->string_Descender());
	    $p->linkto_center($str);
	    $p->grestore() if ($currstate == 2);
	}
    }

    # Draw a slightly darker line to separate the heading from the data
    $p->setgray(0);
    $p->moveto(0, $height);
    $p->lineto($width, $height);
    $p->stroke();

    do_borders($p, @borders) if (@borders);

    $p->set_font($font, $fontsize);
    return $height;
}

# This is an odd one, the Y position is the TOP of the box, not the bottom
# as with all of the others, and it returns 0 on error, otherwise it 
# returns the length (height) used.
sub SPEC_detail {
    my ($p, $x, $y, $width, $height, $config) = @_;
    my ($str, $size, $len);
    my $fontsize = 10;
    my ($left_len, $right_len, $l_finished, $r_finished) = (0,0,0,0);
    my $did_continue = 0;
    $p->gsave();
    {
      my %seen = ();
      while (!($l_finished && $r_finished)) {
        $p->set_font($prop_font, 10);
        my $border = 10;

        my $split = $width * 0.5;
        if (!$l_finished) {
          my $title = ($did_continue) ? 'Hardware (Continued)' : 'Hardware';
          ($l_finished, $left_len) = SPEC_info_box ($p, $x+$border, $y, 
                                                    $split-$border*2,
                                                    $height - $border,
                                                    $config, [
                                                    $title, $config->hardware,
                                                    ],
                                                    $fontsize,
                                                    \%seen);
          return (0, 0, $height) if $left_len == 0;
        } else {
          $left_len = 0;
        }

        if (!$r_finished) {
          my $title = ($did_continue) ? 'Software (Continued)' : 'Software';
          ($r_finished, $right_len) = SPEC_info_box ($p, $x+$border+$split, $y, 
                                                     $width-$split-$border*2,
                                                     $height - $border,
                                                     $config,
                                                     [
                                                     $title, $config->software,
                                                     ],
                                                     $fontsize,
                                                     \%seen);
          return (0, 0, $height) if $left_len == 0;
        } else {
          $right_len = 0;
        }

        $len = ::max($left_len, $right_len);
        $len += $border;

        # Note that there is a continuation if necessary
        my $iscont = 'Continued on next page';
        if (!($l_finished && $r_finished)) {
          $p->gsave();
          $p->set_font($prop_bold_font);
          my $tmpsize = $p->string_calcsize($iscont, $split - ($border * 2),
                                            $border * 0.6);
          $p->set_font($prop_bold_font, $tmpsize);
          if (!$l_finished) {
            $p->set_text_pos($x + ($split / 2) - $border,
                                   $y - $len - ($p->string_Descender * 1.1));
            $p->show_center($iscont);
          }
          if (!$r_finished) {
            $p->set_text_pos($x + $split + ($split / 2) - $border,
                             $y - $len - ($p->string_Descender * 1.1));
            $p->show_center($iscont);
          }
          $p->grestore();
        }

        $p->rect($x, $y-$len, $width, $len);
        $p->rect($x, $y-$len, $split, $len);
        $p->stroke();

        # Start a new page if one of the boxes isn't done
        if (!($l_finished && $r_finished)) {
            $p->grestore(); # Match the one before the loop
            $p->grestore(); # Match the one from newpage
            end_page($p);
            ($width, $height) = SPEC_newpage($p, $config);
            $did_continue = 1;
            $p->gsave(); # Match the one after the loop
            $y = $height;
        }
      }
    }
    $p->grestore();
    return ($did_continue, $len, $height);
}

sub SPEC_title_right {
    my ($p, $x, $y, $width, $height, $config) = @_;
    my ($str, $size);
    $p->gsave();
	$p->translate($x,$y);
        # Shaded area for base
        $p->rect_fill(0, 0, $width, $height / 2, 0.9);
        # Box for the whole thing
	$p->rect(0,0, $width,$height);
	$p->stroke();

        # Leave a little space around the sides
	my $border = ::min($width, $height) * 0.1;
	$p->translate ($border, 0);
	$width  -= $border * 2;
        my $initial_size = 18;

	$p->set_font($prop_font, $initial_size);
        $size = $initial_size;

        # Figure out the size of the font that we're going to use for all of
        # the metrics (needed so that the things can be nicely centered
        # vertically)
        my $basemeanstr = $config->basemean;
        $basemeanstr = significant($basemeanstr) if $basemeanstr =~ /\d/;
        my $peakmeanstr = $config->peakmean;
        $peakmeanstr = significant($peakmeanstr) if $peakmeanstr =~ /\d/;
        foreach my $tmpstr ($config->peakunits.' = '.$peakmeanstr,
			    $config->baseunits.' = '.$basemeanstr) {
	  my $tmpsize = $p->string_calcsize($tmpstr, $width, $height / 2, 0.1,
					    "ERROR: metric string plus mean (\"$tmpstr\") is too long!");
	  $size = $tmpsize if ($tmpsize < $size && $tmpsize > 0);
	}

	return 0 if ($size <= 0);
	$p->set_font($prop_font, $size, 1);
        my $string_base = ($height / 4) - ($p->string_CapHeight() / 2);

        $p->setrgbcolor(@peak_color);
	$p->set_text_pos(0, $height/2 + $string_base);
        $p->linkto($config->peakunits .' = ', undef, 'tmsearch' => 1);

        $str = (@{$config->{'nc'}}+0) ? 'NC' : $peakmeanstr;
	$p->set_text_pos($width, $height/2 + $string_base);
        $p->show_right($str);

        $p->setgray(0);
	$p->set_text_pos(0, $string_base);
        $p->linkto($config->baseunits .' = ', undef, 'tmsearch' => 1);

        $str = (@{$config->{'nc'}}+0) ? 'NC' : significant($config->basemean);
	$p->set_text_pos($width, $string_base);
        $p->show_right($str);

    $p->grestore();
    return 1;
}

sub SPEC_title_left {
    my ($p, $x, $y, $width, $height, $config) = @_;
    my $initial_size = 25;  # Pretty big, eh?

    $p->gsave();
	$p->translate($x,$y);
        # Make the box outline
	$p->rect(0,0, $width,$height);
	$p->stroke();

	$p->set_font($prop_font, $initial_size);

        # Arrange for a little space on the sides
	my $border = ::min($width, $height) * 0.1;
	$width  -= $border * 2;
	$p->translate ($border, 1);

        # Treat the two areas separately in terms of font size.

        my $size = $p->strings_fit(0, $height / 2, $width, $height / 2,
				   $min_font_size, $initial_size,
				   "Your hw_vendor line is too long!\n",
				   $config->{'hw_vendor'});
	return 0 if ($size <= 0);

        $size = $p->strings_fit(0, 0, $width, $height / 2,
				$min_font_size, $initial_size,
				"Your hw_model line is too long!\n",
				$config->{'hw_model'});
	return 0 if ($size <= 0);

    $p->grestore();
    return 1;
}
sub SPEC_title {
    my ($p, $x, $y, $width, $height, $config) = @_;
    my $rc = 0;
    my $split = $width * 0.56;
    $rc = SPEC_title_left ($p, $x,            $y, $split, $height, $config);
    return 0 if $rc == 0;
    $rc = SPEC_title_right($p, $split, $y, $width-$split, $height, $config);
    return $rc;
}

sub SPEC_flags {
  my ($p, $r, $width, $height, $flagfile, $flagurl) = @_;
  my $did_continue;
  my @output = ();
  my %opts = ();

  if (defined($::website_formatter) && $::website_formatter) {
    $flagfile = $flagurl;
  }

  # These will be handy for the flags section
  my $rf = $r->{'reduced_flags'};
  return undef unless isa($rf, 'HASH');
  my @benches = sort keys %{$rf->{'benchlist'}};
  my @tunes = sort keys %{$rf->{'tunelist'}};
  my @classes = sort keys %{$rf->{'classlist'}};
  my $indent = $rf->{'maxbench'} + $rf->{'maxtune'} + 5;

  # Do the unknown and forbidden flags.
  foreach my $class (qw(forbidden unknown)) {
      next unless ::check_elem(undef, $rf, 'flaglist', $class);
      # Flags of the class exist for at least one benchmark, so
      # make lists of them.  They'll be formatted and output later.
      my $classref = $rf->{'flaglist'}->{$class};
      @output = ();
      for my $tune (sort @tunes) {
        for my $bench (sort keys %{$classref}) {
            next unless ::check_elem('ARRAY', $classref, $bench, $tune);
            my @flags = @{$classref->{$bench}->{$tune}};
            if ($class eq 'unknown') {
              # This just looks scary because we want the flags and variable
              # names in a fixed-width font, and everything else proportional.
              # And because the "unknown" flags may be long, we take care to
              # break them up.
              my @tmpflags = ();
              foreach my $flagref (@flags) {
                my @lines = ::wrap_lines([ $flagref->[1] ],
                                         78 - $indent,
                                         '');
                my $tmpflag = [
                                [ '"',           'font' => $prop_font ],
                                [ shift(@lines), 'font' => $fixed_font, 'mid_flag' => @lines+0 ]
                              ];
                while (@lines) {
                  push @tmpflags, $tmpflag;
                  $tmpflag = [
                               [ shift(@lines), 'font' => $fixed_font, 'mid_flag' => 1 ]
                             ];
                }
                if ($indent + length($tmpflag->[0]->[0]) + length("\" (in ".join(', ', @{$flagref->[0]}).")") > 78) {
                  # Just tack on the quote and put the variable name on the
                  # next line.
                  push @{$tmpflag}, [ '"', 'font' => $prop_font, 'mid_flag' => 1 ];
                  push @tmpflags, $tmpflag;
                  $tmpflag =  [ 
                                [ '(in ',         'font' => $prop_font ],
                                [ join(', ', @{$flagref->[0]}), 'font' => $fixed_font ],
                                [ ')',            'font' => $prop_font ]
                              ];
                } else {
                  push @{$tmpflag}, [ '" (in ',       'font' => $prop_font ],
                                    [ join(', ', @{$flagref->[0]}), 'font' => $fixed_font ],
                                    [ ')',            'font' => $prop_font ];
                }
                push @tmpflags, $tmpflag;
              }
              @flags = @tmpflags;
            }
            push @output, dump_lines($bench,
                                     $indent, $bench, $tune,
                                     { 'url' => $flagfile }, @flags), '';
	}
        if (@output) {
          # Some flags were done; output the section
          %opts = ('title' => ucfirst($tune).' '.ucfirst($class).' Flags',
                   'content' => \@output,
                   'content_font' => $fixed_font,
                   'border' => 2);
          if ($class eq 'forbidden') {
            $opts{'title_color'} = [ @error_color ];
          }
          my $cont;
          ($cont, $width, $height) = do_section($p, $r, $width, $height, %opts);
          $did_continue += $cont;
          return undef if ($width == 0);
        }
      }
    }

    # Do all the other flags in a way that aggregates as much as possible.
    my %class2title = ( 'compiler' => 'Compiler Invocation',
                        'portability' => 'Portability Flags',
                        'optimization' => 'Optimization Flags',
                        'other' => 'Other Flags' );
    foreach my $class (qw(compiler portability optimization other)) {
      %opts = (
                'title' => $class2title{$class},
                'content_font' => $fixed_font,
                'border' => 2,
              );
      @output = ();
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
            ::check_elem('HASH', $rf, 'bylang', 'flaglist', $class, $onetune)) {
          if (exists($rf->{'langmatch'}->{$class}->{'alltune'}->{$lang}) &&
              $rf->{'langmatch'}->{$class}->{'alltune'}->{$lang} &&
              # There might _not_ be an entry for a particular language if, for
              # the same flag (like -DSPEC_CPU_WINDOWS) one benchmark calls
              # it portability and another calls it mandatory.  This is
              # incorrect, but it's no fault of the user.
              ::check_elem('ARRAY', $rf, 'bylang', 'flaglist', $class, $onetune, $lang) &&
              @{$rf->{'bylang'}->{'flaglist'}->{$class}->{$onetune}->{$lang}}) {
            my @flags = ();
            for(my $i = 0; $i < @{$rf->{'bylang'}->{'flaglist'}->{$class}->{$onetune}->{$lang}}; $i++) { 
              my $flag = $rf->{'bylang'}->{'flaglist'}->{$class}->{$onetune}->{$lang}->[$i];
              next unless ($flag->[2]->{'display'} || $r->{'review'});
              my $markup = $rf->{'bylang'}->{'markup'}->{$class}->{$onetune}->{$lang}->[$i];
              # In order to get the pre-formatted string to be used, make a
              # new copy of the flag, with just the second element replaced.
              my @newflag = @{$flag};
              $newflag[1] = [ $flag->[1], $markup ];
              push @flags, \@newflag;
            }
            my $langname = $lang;
            $langname =~ s/ /_/g;
            my $langtitle = $rf->{'var2desc'}->{$lang};
            if ($rf->{'langmatch'}->{$class}->{$onetune}->{$lang} == 2) {
              $langtitle .= ' (except as noted below)';
            }
            push @output, dump_lines($langtitle,
                                     5,
                                     $langname,
                                     'ALL',
                                     { 'noaddbench' => 1, 'url' => $flagfile,
                                       'title_alone' => 1, 'link_title' => 1 },
                                     @flags), '';
            $printed_lang = 1;
            delete $langstodo{$lang};
            if (::check_elem(undef, $rf, 'bylang', 'mismatch', $class, $onetune, $lang)) {
              $mismatch += $rf->{'bylang'}->{'mismatch'}->{$class}->{$onetune}->{$lang};
            }
          }
        }

        # Do the benchmarks that matched across tuning levels
        if ($rf->{'allmatch'}->{$class} == 1 &&
            ::check_elem('HASH', $rf, 'stringlist', $class)) {
          my $classref = $rf->{'flaglist'}->{$class};
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
            my @flags = ();
            my $flags = $rf->{'flaglist'}->{$class}->{$bench}->{$onetune};
            for(my $i = 0; $i < @{$flags}; $i++) {
              next unless ($flags->[$i]->[2]->{'display'} || $r->{'review'});
              my $markup = $rf->{'markup'}->{$class}->{$bench}->{$onetune}->[$i];
              # In order to get the pre-formatted string to be used, make a
              # new copy of the flag, with just the second element replaced.
              my @newflag = @{$flags->[$i]};
              $newflag[1] = [ $flags->[$i]->[1], $markup ];
              push @flags, \@newflag;
            }
            if (!$printed_lang) {
              my $url = "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#".::makeanchor($rf->{'var2desc'}->{$lang});
              push @output, [ [ $url, $rf->{'var2desc'}->{$lang}, 'font' => $prop_font, 'subsection_title' => 1 ], [ undef, ': ', 'font' => $prop_font ] ], '';
              $printed_lang = 1;
            }
            push @output, dump_lines(' '.$bench,
                                     $rf->{'maxbench'} + 3,
                                     $bench,
                                     'ALL',
                                     { 'noaddbench' => 1, 'url' => $flagfile },
                                     @flags), '';
            if (::check_elem(undef, $rf, 'mismatch', $class, $bench, $onetune)) {
              $mismatch += $rf->{'mismatch'}->{$class}->{$bench}->{$onetune};
            }
            $donebench{$bench}++;
          }
        }
      }
      if (@output) {
        # Some non-tune-specific flags are mentioned, so output them.
        if ($mismatch) {
          if ($class eq 'optimization') {
            push @output, '', [ [ undef, '(*) Indicates an optimization flag that was found in a portability variable.', 'font' => $prop_font ] ];
          } elsif ($class eq 'portability') {
            push @output, '', [ [ undef, '(*) Indicates a portability flag that was found in a non-portability variable.', 'font' => $prop_font ] ];
          } elsif ($class eq 'compiler') {
            push @output, '', [ [ undef, '(*) Indicates a compiler flag that was found in a non-compiler variable.', 'font' => $prop_font ] ];
          }
        }
        $opts{'content'} = \@output;
        my $cont;
        ($cont, $width, $height) = do_section($p, $r, $width, $height, %opts);
        $did_continue += $cont;
        return undef if ($width == 0);
        @output = ();
        $mismatch = 0;
      }

      # Next dump class flags by tuning level, with the common per-language
      # set at the top, followed by benchmark-specific settings
      foreach my $tune (@tunes) {
        # First check for by-language list
        my $classref = undef;
        if (::check_elem('HASH', $rf, 'bylang', 'flaglist', $class, $tune)) {
          $classref = $rf->{'bylang'}->{'flaglist'}->{$class}->{$tune};
        }
        foreach my $lang (sort ::bylang keys %langstodo) {
          my $printed_lang = 0;
          if (defined($classref) &&
              ::check_elem('ARRAY', $classref, $lang) &&
              @{$classref->{$lang}}) {
            my @flags = ();
            for(my $i = 0; $i < @{$classref->{$lang}}; $i++) {
              my $flag = $classref->{$lang}->[$i];
              next unless ($flag->[2]->{'display'} || $r->{'review'});
              my $markup = $rf->{'bylang'}->{'markup'}->{$class}->{$tune}->{$lang}->[$i];
              my @newflag = @{$flag};
              $newflag[1] = [ $flag->[1], $markup ];
              push @flags, \@newflag;
            }
            my $langname = $lang;
            $langname =~ s/ /_/g;
            my $langtitle = $rf->{'var2desc'}->{$lang};
            if ($rf->{'langmatch'}->{$class}->{$tune}->{$lang} == 2) {
              $langtitle .= ' (except as noted below)';
            }
            push @output, dump_lines($langtitle,
                                     5,
                                     '',
                                     $langname.$tune,
                                     { 'url' => $flagfile,
                                       'title_alone' => 1, 'link_title' => 1 },
                                     @flags), '';
            $printed_lang = 1;
            if (::check_elem(undef, $rf, 'bylang', 'mismatch', $class, $tune, $lang)) {
              $mismatch += $rf->{'bylang'}->{'mismatch'}->{$class}->{$tune}->{$lang};
            }
          }

          # Now do the benchmark-specific list (if any)
          if (::check_elem('HASH', $rf, 'flaglist', $class)) {
            my $classref = $rf->{'flaglist'}->{$class};
            foreach my $bench (sort keys %{$classref}) {
              next if $donebench{$bench};
              next if $rf->{'langs'}->{$bench}->{$tune} ne $lang;
              next unless ::check_elem('ARRAY', $classref, $bench, $tune);
              next unless @{$classref->{$bench}->{$tune}};
              my @flags = ();
              for(my $i = 0; $i < @{$classref->{$bench}->{$tune}}; $i++) {
                my $flag = $rf->{'flaglist'}->{$class}->{$bench}->{$tune}->[$i];
                next unless ($flag->[2]->{'display'} || $r->{'review'});
                my $markup = $rf->{'markup'}->{$class}->{$bench}->{$tune}->[$i];
                my @newflag = @{$flag};
                $newflag[1] = [ $flag->[1], $markup ];
                push @flags, \@newflag;
  #'from_bench' => $tune.join('', @{$flag->[0]}),
  #$opts{'from_bench'} .= $bench if ($origin =~ /^(?:user|suite)/);
              }
              if (!$printed_lang) {
                my $url = "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#".::makeanchor($rf->{'var2desc'}->{$lang});
                push @output, [ [ $url, $rf->{'var2desc'}->{$lang}, 'font' => $prop_font, 'subsection_title' => 1 ], [ undef, ': ', 'font' => $prop_font ] ], '';
                $printed_lang = 1;
              }
              push @output, dump_lines(' '.$bench,
                                       $rf->{'maxbench'} + 3,
                                       $bench, $tune,
                                       { 'addvar' => 1, 'url' => $flagfile },
                                       @flags), '';
              if (::check_elem(undef, $rf, 'mismatch', $class, $bench, $tune)) {
                $mismatch += $rf->{'mismatch'}->{$class}->{$bench}->{$tune};
              }
            }
          }
        }
        if (@output) {
          if ($mismatch) {
            if ($class eq 'optimization') {
              push @output, '', [ [ undef, '(*) Indicates an optimization flag that was found in a portability variable.', 'font' => $prop_font ] ];
            } elsif ($class eq 'portability') {
              push @output, '', [ [ undef, '(*) Indicates a portability flag that was found in a non-portability variable.', 'font' => $prop_font ] ];
            } elsif ($class eq 'compiler') {
              push @output, '', [ [ undef, '(*) Indicates a compiler flag that was found in a non-compiler variable.', 'font' => $prop_font ] ];
            }
          }
          $opts{'title'} = ucfirst($tune).' '.$class2title{$class};
          $opts{'content'} = \@output;
          my $cont;
          ($cont, $width, $height) = do_section($p, $r, $width, $height, %opts);
          $did_continue += $cont;
          return undef if ($width == 0);
          @output = ();
          $mismatch = 0;
        }
      }
    }

    if (defined($::website_formatter) && $::website_formatter &&
        defined($r->{'flagsurl'}) && $r->{'flagsurl'} ne '') {
      my $url = $r->{'flagsurl'};
      $url =~ s/\.xml$/\.html/;
      # Calling do_section is a LOT easier than worrying about orphans
      # and other fun stuff like that.
      my $cont = 0;
      ($cont, $width, $height) = do_section($p, $r, $width, $height,
         'content' => [ '',
                       [ [ $url, $url, 'font' => $fixed_font ] ]
                      ],
         'title' => 'The full text of the flags file used for this result is available at',
         'title_font' => $prop_font,
         'title_pad' => 0,
         'title_size' => 10,
         'no_link_title' => 1,
         'indent' => 10,
         );
      $did_continue += $cont;
      return undef if ($width == 0);
    }

    return($did_continue, $width, $height);
}

sub dump_lines {
    my ($title, $len, $bench, $tune, $opts, @flags) = @_;
    my @output = ();
    my @line = ();
    my $line = '';      # Just for measuring...
    my $printed = 0;
    my $flagfile = $opts->{'url'};

    if ($opts->{'title_alone'}) {
      my $url = undef;
      if ($opts->{'link_title'}) {
        $url = "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#".::makeanchor($title);
      }
      push @output, [ [ $url, $title, 'font' => $prop_font, 'subsection_title' => 1 ], [ undef, ': ', 'font' => $prop_font ] ];
      $printed = 1;
    }

    foreach my $flag (@flags) {
        my ($doul, $string, $url, $markup);
        if (isa($flag, 'ARRAY')) {
          # It could be a flag, or some text with attributes
          if (isa($flag->[2], 'HASH')) {
            # It's a real flag
            # Strive for consistency -- the user flags are not underlined
            # in HTML.
            #$doul = ($flag->[2]->{'origin'}->[1] eq 'user') * 0.5;
            $doul = 0;
            my $from = $tune;
            if ($opts->{'addvar'}) {
              $from .= join('', @{$flag->[0]});
            }
            if (!$opts->{'noaddbench'}) {
              $from .= $bench if $doul || $flag->[2]->{'origin'}->[1] eq 'suite';
            }
            if (exists $flag->[2]->{'nomap'}) {
              # This is a pseudo-flag added by the tools, so just
              # display the name.  (This should never happen here.)
              $url = undef;
              $doul = 0;
            } else {
              $url = 1;
            }
            if (isa($flag->[1], 'ARRAY')) {
              ($string, $markup) = @{$flag->[1]};
              $url = $flagfile.'#'.::make_flag_id($flag->[2], $from, $string, 1) if defined($url);
              $string = [ [ $string, 'doul' => $doul, 'url' => $url, 'font' => $fixed_font ] ];
              push @{$string}, [ $markup, 'font' => $prop_font ] if ($markup ne '');
            } else {
              $string = $flag->[1];
              $url = $flagfile.'#'.::make_flag_id($flag->[2], $from, $string, 1) if defined($url);
              $markup = undef;
            }
            $printed += add_flag_to_line($string, $len, $title, \@output, \@line, \$line, 'doul' => $doul, 'url' => $url, 'printed' => $printed, 'link_title' => $opts->{'link_title'});
          } else {
            # Text with attributes
            $printed += add_flag_to_line($flag, $len, $title, \@output, \@line, \$line, 'doul' => $doul, 'url' => $url, 'printed' => $printed, 'link_title' => $opts->{'link_title'});
          }
        } else {
          # Just a string
          $doul = 0;
          $url = undef;
          $string = $flag;
          $printed += add_flag_to_line($string, $len, $title, \@output, \@line, \$line, 'doul' => $doul, 'url' => $url, 'printed' => $printed, 'link_title' => $opts->{'link_title'});
        }
    }
    if (@line) {
      if ($printed) {
        push @output, [ [ undef, '', 'width' => $len, 'font' => $prop_font ], @line ];
      } else {
        my $url = undef;
        if ($opts->{'link_title'}) {
          $url = "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#".::makeanchor($title);
        }
        push @output, [ [ $url, $title, 'width' => $len, 'font' => $prop_font, 'subsection_title' => 1 ], [ undef, ': ', 'font' => $prop_font ], @line ];
      }
    }

    return @output;
}

sub add_flag_to_line {
  my ($string, $len, $title, $output, $line, $curline, %opts) = @_;
  my $url      = $opts{'url'};
  my $doul     = $opts{'doul'};
  my $printed  = $opts{'printed'};
  my @strings  = ();
  my $titleurl = undef;
  if ($opts{'link_title'}) {
    $titleurl = "http://www.spec.org/auto/$::lcsuite/Docs/result-fields.html#".::makeanchor($title);
  }
  if (isa($string, 'ARRAY')) {
    # If the "string" is an array, it's a bunch of [ string, attr ] pairs
    @strings = @{$string};      # For later
    $string = join('', map { $_->[0] } @strings);       # For computing length
  }
  if (!@{$line}) {
      $$curline = $string;
      if (@strings) {
        @{$line} = ();
        foreach my $part (@strings) {
          my ($str, %opts) = @{$part};
          push @{$line}, [ $opts{'url'}, $str, %opts ];
        }
      } else {
        @{$line} = ( [ $url, $string, 'doul' => $doul, 'font' => $fixed_font ] );
      }
  } elsif (length($$curline.', '.$string) + $len > 78) {
      # If the last bit of the last line has 'mid_flag' set in its options,
      # don't append a comma.
      my (undef, undef, %tmpopts) = @{$line->[$#{$line}]};
      if ($tmpopts{'mid_flag'}) {
        # Same as below, just without the trailing comma
        if ($printed) {
          push @{$output}, [ [ undef, '', 'width' => $len, 'font' => $prop_font ], @{$line} ];
        } else {
          push @{$output}, [ [ $titleurl, $title, 'width' => $len, 'font' => $prop_font, 'subsection_title' => 1 ], [ undef, ': ', 'font' => $prop_font ], @{$line} ];
        }
      } else {
          if ($printed) {
            push @{$output}, [ [ undef, '', 'width' => $len, 'font' => $prop_font ], @{$line}, ' ' ];
          } else {
            push @{$output}, [ [ $titleurl, $title, 'width' => $len, 'font' => $prop_font, 'subsection_title' => 1 ], [ undef, ': ', 'font' => $prop_font ], @{$line}, ' ' ];
          }
      }
      $printed++;
      $$curline = $string;
      if (@strings) {
        @{$line} = ();
        foreach my $part (@strings) {
          my ($str, %opts) = @{$part};
          push @{$line}, [ $opts{'url'}, $str, %opts ];
        }
      } else {
        @{$line} = ( [ $url, $string, 'doul' => $doul, 'font' => $fixed_font ] );
      }
  } else {
      $$curline .= ", $string";
      if (@strings) {
        if (0) {
          # No commas -- too "confusing"
          push @{$line}, ', ';
        } else {
          push @{$line}, ' ';
        }
        foreach my $part (@strings) {
          my ($str, %opts) = @{$part};
          push @{$line}, [ $opts{'url'}, $str, %opts ];
        }
      } else {
        if (0) {
          # No commas -- too "confusing"
          push @{$line}, ', ', [ $url, $string, 'doul' => $doul, 'font' => $fixed_font ];
        } else {
          push @{$line}, ' ', [ $url, $string, 'doul' => $doul, 'font' => $fixed_font ];
        }
      }
  }
  return $printed;
}

sub do_section {
  # Dump a section (like errors, notes, flags, etc) with a nice title.
  # The content array can contain either strings or array refs.  Strings are
  # printed as-is.  Array refs are expected to contain pairs of text with
  # URLs that they should be linked to.  Text that should not be linked must
  # have an undef URL.
  my ($p, $config, $width, $height, %params) = @_;

  my ($rc, $x);
  my $did_continue = 0;
  my ($title_height, $line_height);

  if (!exists($params{'title'})) {
    ::Log(0, "ERROR: No section title in call to do_section\n");
    return(0, $width, $height);
  }

  $params{'border'} = 10 unless exists($params{'border'});
  $params{'indent'} = 25 unless exists($params{'indent'});
  $params{'title_color'} = 0 unless exists($params{'title_color'});
  $params{'title_font'} = $prop_bold_font unless exists($params{'title_font'});
  $params{'title_size'} = 20 unless exists($params{'title_size'});
  $params{'title_pad'} = 5 unless exists($params{'title_pad'});
  $params{'content_color'} = 0 unless exists($params{'content_color'});
  $params{'content_font'} = $prop_font unless exists($params{'content_font'});
  $params{'content_size'} = 10 unless exists($params{'content_size'});
  $params{'content_pad'} = 0 unless exists($params{'content_pad'});
  $params{'content'} = [] unless exists($params{'content'});
  my @lines = @{$params{'content'}};

  # The calculation below is a bit of a fudge, but it should do...
  if ($height < $params{'title_size'} + ($params{'content_size'} * 4) + ($params{'title_pad'} * 2) + $params{'content_pad'}) {
    # There's not enough room for the heading and one line of text.
    $p->grestore();
    end_page($p);
    ($width, $height) = SPEC_newpage($p, $config);
    $did_continue = 1;
  }
  $p->gsave();
  {
    $p->set_font($params{'title_font'}, $params{'title_size'});
    $title_height = ::max($params{'title_size'},
                          $p->string_XHeight() - $p->string_Descender());
    $height -= $params{'title_pad'} + $title_height;
    $p->set_text_pos($width/2, $height);
    if (ref($params{'title_color'}) ne 'ARRAY') {
      $p->setgray($params{'title_color'}+0);
    } else {
      $p->setrgbcolor(@{$params{'title_color'}});
    }
    if ($params{'no_link_title'}) {
      $p->show_center($params{'title'});
    } else {
      $p->linkto_center($params{'title'});
    }

    if (ref($params{'content_color'}) ne 'ARRAY') {
      $p->setgray($params{'content_color'}+0);
    } else {
      $p->setrgbcolor(@{$params{'content_color'}});
    }
    $p->set_font($params{'content_font'}, $params{'content_size'});
    $line_height = $p->string_XHeight() - $p->string_Descender();
    $line_height = ::max($params{'content_size'},
                         $p->string_XHeight() - $p->string_Descender());
    $height -= $params{'title_pad'} + $line_height;

    my $saved_subsection = undef;
    for(my $i = 0; $i < @lines; $i++) {
      my $line = $lines[$i];
      my ($urls, $text, $item_params, $tmpstr, $subsection_title) = decompose_line($line);

      # If the current line is marked as a subsection header, make sure that
      # there's room for _three_ lines (the header, some info, and a blank).
      my $required_height = $line_height;
      if ($subsection_title) {
        $saved_subsection = undef;      # Will be set later, if necessary
        $required_height *= 3;
      }
      if ($height < $required_height) {
        $p->set_font($prop_font, 8);
        $p->set_text_pos($width/2, $line_height / 2);
        $p->show_center('Continued on next page');

        $p->grestore();
        $p->grestore();
        end_page($p);
        ($width, $height) = SPEC_newpage($p, $config);
	$p->gsave();

        $height -= $params{'title_pad'} + $title_height;
        $p->set_font($params{'title_font'}, $params{'title_size'});
        $p->set_text_pos($width/2, $height);
        if (ref($params{'title_color'}) ne 'ARRAY') {
          $p->setgray($params{'title_color'}+0);
        } else {
          $p->setrgbcolor(@{$params{'title_color'}});
        }
        if ($params{'no_link_title'}) {
          $p->show_center($params{'title'}.' (Continued)');
        } else {
          $p->linkto_center($params{'title'}.' (Continued)');
        }
        $height -= $params{'title_pad'} + $line_height;

        if (ref($params{'content_color'}) ne 'ARRAY') {
          $p->setgray($params{'content_color'}+0);
        } else {
          $p->setrgbcolor(@{$params{'content_color'}});
        }
        $p->set_font($params{'content_font'}, $params{'content_size'});

        $did_continue = 1;

        if (defined($saved_subsection)) {
          $i--; # Do the current line next time around
          ($urls, $text, $item_params, $tmpstr) = decompose_line($saved_subsection);
        }
      }
      if ($subsection_title) {
        $saved_subsection = get_subsection($line);
      }
      if (isa($line, 'ARRAY')) {
        # Figure out the font size to use.  This is only a guess (but probably
        # a good one, if the font is constant)
        my $tmpsize = $p->string_calcsize($tmpstr, 
                                          $width - $params{'indent'} - $params{'border'},
                                          $params{'content_size'}, 
                                          $min_font_size,
                                          "A line in your $params{'title'} section is too long!\n");
        return ($did_continue, 0, 0) if ($tmpsize <= 0);
        $p->set_font($params{'content_font'}, $tmpsize);
        $x = $params{'indent'};
        for(my $i = 0; $i < @{$text}; $i++) {
          if ($item_params->[$i]->{'font'} ne '' &&
              $item_params->[$i]->{'font'} ne $params{'content_font'}) {
            $p->set_font($item_params->[$i]->{'font'}, $item_params->[$i]->{'font_size'});
          }
          my $strwidth = $p->stringwidth($text->[$i]);
          if ($item_params->[$i]->{'width'} > 0) {
            # Why use the maximum digit width, when the labels will clearly
            # not be ALL digits?
            # Well, the maximum width (usually 'W') is WAAAAAY too wide.
            # It turns out that (for Times-Roman), the max digit width is
            # much closer to the average width (which is close to what we
            # want).  For a fixed-width font, it won't matter anyway.
            # Plus, if this guess is wrong, we still do the right thing (it
            # just won't look quite right is all).
            my $tmpwidth = $p->string_MaxDigitWidth * $item_params->[$i]->{'width'};
            $strwidth = $tmpwidth if ($strwidth < $tmpwidth);
            $p->set_text_pos($x + $strwidth, $height + ($params{'content_size'}-$tmpsize)/2);
            $p->show_right($text->[$i], $item_params->[$i]->{'doul'}, $urls->[$i]);
          } else {
            $p->set_text_pos($x, $height + ($params{'content_size'}-$tmpsize)/2);
            $p->show($text->[$i], $item_params->[$i]->{'doul'}, $urls->[$i]);
          }
          $x += $strwidth;
          $p->set_font($params{'content_font'}, $tmpsize);
        }
      } else {
        $rc = $p->string_fit($line, $params{'indent'}, $height, 
                             $width - $params{'indent'} - $params{'border'},
                             $params{'content_size'}, 
                             $min_font_size,
                             "A line in your $params{'title'} section is too long!\n");
        return ($did_continue, 0, 0) if $rc == 0;
      }
      $height -= $line_height;
    }
    #if (0 && $height > $line_height * 4) {
    if ($height > $params{'title_size'} + ($params{'content_size'} * 4) + ($params{'title_pad'} * 2) + $params{'content_pad'}) {
      # Only draw the lower boundary if there's enough space left to start
      # another section.
      $p->moveto(0, $height);
      $p->lineto($width, $height);
      $p->stroke();
    }
  }
  $p->grestore();

  return($did_continue, $width, $height);

}

sub get_subsection {
  my ($line) = @_;
  # Get the part of the line marked as a subsection (if any)
  my $cont = [ undef, ' (continued):' ];

  if (isa($line, 'ARRAY')) {
    foreach my $pair (@{$line}) {
      if (isa($pair, 'ARRAY')) {
        my ($url, $tmptext, %item_params) = @{$pair};
        if ($item_params{'subsection_title'}) {
          if (exists $item_params{'font'}) {
            return [ $pair, [ @$cont, 'font' => $item_params{'font'} ] ];
          } else {
            return [ $pair, $cont ]
          }
        }
      }
    }
  }

  return undef;
}

sub decompose_line {
  my ($line) = @_;
  # Break a line down into its constituent bits
  my ($urls, $text, $item_params) = ([], [], []);
  my $tmpstr = '';
  my $subsection_title = 0;

  if (isa($line, 'ARRAY')) {
    foreach my $pair (@{$line}) {
      if (isa($pair, 'ARRAY')) {
        my ($url, $tmptext, %item_params) = @{$pair};
        push @{$urls}, $url;
        push @{$text}, $tmptext;
        push @{$item_params}, \%item_params;
        $tmpstr .= $tmptext;
        $subsection_title += $item_params{'subsection_title'};
      } else {
        push @{$urls}, undef;
        push @{$text}, $pair;
        push @{$item_params}, {};
      }
    }
  }

  return ($urls, $text, $item_params, $tmpstr, $subsection_title);
}


sub istrue {
    return main::istrue(@_);
}

sub significant {
    my ($value, $min_log, $returnlog) = @_;
    print "significant(value=\"$value\", min_log=\"$min_log\", returnlog=\"$returnlog\")\n" if ($debug & 4);
    my ($log);
    $min_log = 2 if !defined $min_log;
    if ($value == 0) {
	if ($value eq '0' || $value !~ /[\d.]/) {
	    print "  not a number; returning \"0.00\"\n" if ($debug & 4);
	    return '0.00';
	}
	$log = 0;
    } elsif ($value > 0) {
	$log = &floor(log($value)/log(10)); 
    } else {
	::Log(0, "ERROR: Can't take log of negative number ($value); called from ".join(':', caller())."\n");
	return $value;
    }
    #$min_log--;

    if ($returnlog) {
	print "  returnlog set; returning \"$log\"\n" if ($debug & 4);
	return $log;
    }

    $value = int($value / (10**($log-$min_log))+.5) * (10**($log-$min_log));
    print "  value rounded to \"$value\"\n" if ($debug & 4);

    # The number has been changed; re-figure its log (it'll change if, for
    # example, a number less than 1 gets rounded up to 1)
    if ($value > 0) {
	$log = &floor(log($value)/log(10));
	print "  new log = $log\n" if ($debug & 4);
    }
    if ($log < $min_log) {
	$value = sprintf ("%.*f", $min_log-$log, $value) if ($log < $min_log);
	print "  reformatted value = \"$value\"\n" if ($debug & 4);
    }
    print "  returning \"$value\"\n" if ($debug & 4);
    return $value;
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

# Figure out how many (if any) decimal places to invisibly tack on to
# make sure that decimal alignment happens.
sub figure_dp {
    my ($value, $min_log, $low_log) = @_;
    return '' unless ($value ne '');
    print " figure_dp($value, $min_log, $low_log);\n" if ($debug & 4);
    my $s = '';
    my ($real_dp, $wanted_dp, $dp, $space, $log);
    if ($value == 0) {
	if ($value !~ m/^\s*(\+|-)?[0-9.eE+]/) {
	    print "Returning '$value'\n" if ($debug & 4);
	    return $value;
	}
	$log = 0;
    } else {
	$log = &floor(log($value)/log(10)); 
    }
    $min_log--;
    print "  log=$log  min_log=$min_log\n" if ($debug & 4);
    print "  value=$value\n" if ($debug & 4);
    $dp        = ($low_log >= $min_log)? 0 : (3 - $low_log);
    $wanted_dp = ($log >= $min_log) ? 0 : ($min_log - $log);
    print "  dp=$dp   wanted_dp=$wanted_dp\n" if ($debug & 4);
    if ($dp > $wanted_dp) {
	$space = $dp - $wanted_dp;
	$real_dp = $wanted_dp;
    } else {
	$space = 0;
	$real_dp = $dp;
    }
    if ($real_dp == 0 && $dp > 0) {
	$s .= '.';
    }
    print "  space=$space  real_dp=$real_dp\n" if ($debug & 4);
    $s .= '0' x ($space) if $space > 0;
    print "  returning \"$s\"\n" if ($debug & 4);
    return $s;
}

sub pluralize {
  return main::pluralize(@_);
}

sub dist {
  my ($x1, $y1, $x2, $y2) = @_;
  my $dx = $x2 - $x1;
  my $dy = $y2 - $y1;

  return sqrt(($dx * $dx) + ($dy * $dy));
}

sub joinup {
  my ($ref) = @_;

  return $ref unless ref($ref);
  if (ref($ref) eq 'ARRAY') {
    return join(' ', @{$ref});
  }
}

sub end_page {
    my ($p) = @_;

    my $width  = $p->{'width'};
    my $height = $p->{'height'};
    my ($tmpwidth, $rc);

    if ($invalid) {
	$p->gsave();
            $p->setrgbcolor(@error_color);
	    $p->set_font($prop_font);
	    $p->rotate(50);
	    $tmpwidth = dist(0,0, $width, $height) * 0.75;
	    $rc = $p->string_fit('Invalid Run', $tmpwidth * 0.25, 0,
	                            $tmpwidth, 144, undef, undef, -render => 1);
	$p->grestore();
    }
    if ($nc) {
	$p->gsave();
            $p->setrgbcolor(@error_color);
	    $p->set_font($prop_font);
	    $p->rotate(50);
	    $tmpwidth = dist(0,0, $width, $height) * 0.75;
	    $rc = $p->string_fit('Non-Compliant', $tmpwidth * 0.25, 0,
				    $tmpwidth, 144);
	$p->grestore();
    }
    $p->end_page();
}

sub byspot {
    my ($ax, $bx);
    if (ref($a) eq 'ARRAY') {
	$ax = ($a->[0] + $a->[1]) / 2;
    } else {
	$ax = $a;
    }
    if (ref($b) eq 'ARRAY') {
	$bx = ($b->[0] + $b->[1]) / 2;
    } else {
	$bx = $b;
    }
    if ($sortdir) {
	return $bx <=> $ax;
    } else {
	return $ax <=> $bx;
    }
}

sub SPEC_trademarks {
  my ($p, $result, $width, $height) = @_;

  my $did_continue = 0;

  my @lines = ::trademark_lines('', $p->trademarks_done());

  my $longest_line = (sort { length($b) <=> length($a) } @lines)[0];

  $p->set_font($prop_font, 10);
  my $line_height = ($p->string_CapHeight() - $p->string_Descender()) * 1.2;
  my $required_height = $line_height * (@lines+1.5);

  if ($height < $required_height) {
    # There's not enough room for the paragraph.
    $p->grestore();
    end_page($p);
    ($width, $height) = SPEC_newpage($p, $result);
    $did_continue = 1;
    $p->set_font($prop_font, 10);
  }
  my $base_x = ($width - $p->stringwidth($longest_line)) / 2;
  $p->gsave();
  {
      # Print the lines one at a time, starting from the BOTTOM and working
      # up.
      my $y = $line_height / 2;
      foreach my $line (reverse @lines) {
        $p->set_text_pos($base_x, $y);
        $p->show($line);
        $y += $line_height;
      }
      $p->moveto(0, $y + ($line_height / 2));
      $p->rlineto($width, 0);
      $p->stroke();
  }
  $p->grestore();

  return($did_continue, $width, $height);

}

# Stolen from Math::Complex and Math::Trig, though they could've come
# from a textbook, too.
sub pi { 4 * atan2(1, 1) }
sub rad2deg { (360 / (2 * pi)) * $_[0] }
