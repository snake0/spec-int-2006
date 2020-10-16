#!/usr/bin/perl
#
# mkgraphs.pl - make graphs like the HTML format uses.
# No support is provided for this script.
#
# Copyright (C) 1999-2006 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: mkgraphs.pl 3626 2006-01-17 14:16:50Z cloyce $


use File::Basename;
use MIME::Base64;
use GD;

my @invalid_gif = qw(
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
a9k5XLpglboc3i172RxgCnOYxOROAQAAOw==
);
my $twidth = 300;		# Height will be figgerd out later
my $debug = 0;			# Set to at least 3 for full output

# Make the 'invalid run' image
my $gifname = $ENV{'SPEC'}.'/images/invalid.gif';
if (! -f $gifname) {
    $invalid_gif = decode_base64(join('', @invalid_gif)) unless defined $invalid_gif;
    my $igfh = new IO::File (">$gifname");
    if (!defined($igfh)) {
	print  "Couldn't open $gifname for output: $!\n";
    } else {
	print $igfh $invalid_gif;
	$igfh->close();
    }
}

# Draw a bunch of pre-made scales
$font = gdSmallFont;
for($i = 5; $i <= 20; $i+=5) {
    draw_scale(0, $i, $ENV{'SPEC'}.'/images/', sprintf("scale.%03d",$i));
}
for($i = 10; $i <= 200; $i+=5) {
    draw_scale(1, $i, $ENV{'SPEC'}.'/images/', sprintf("rscale.%03d",$i));
}
# Some big rates, just to test...
for($i = 50000; $i <= 60000; $i+=1000) {
    draw_scale(1, $i, $ENV{'SPEC'}.'/images/', sprintf("rscale.%03d",$i));
}

draw_scale(1, $i, $ENV{'SPEC'}.'/images/', sprintf("rscale.%03d",$i));
sub draw_scale {
    my ($rate, $maxratio, $path, $name) = @_;
    return if (-f "${path}${name}.gif");
    my $xpos = 0;
    my $skip = 0;
    my $minor_ticks = 10;
    $theight = 13 + $font->height;
    # Set up the image object
    my $im = new GD::Image($twidth, $theight*($debug+1));
    my ($white, $black, $base, $peak) = (
					 $im->colorAllocate(255,255,255),
					 $im->colorAllocate(0,  0,  0),
					 $im->colorAllocate(0,  0,  0),
					 $im->colorAllocate(127,127,127),
			  );
    $im->transparent($white);
    $theight--;

    # Figure out what the scale should be.  This must match the code in
    # the HTML formatter.
    my $scale = $twidth / $maxratio;
    print "old $maxratio, scale $scale\n" if $debug;
    while ($scale < (length("$maxratio") * $font->width * 2)) {
	last if ($scale > (length("$maxratio") * $font->width * 1.8));
	$skip+=5;
	$scale = $twidth / int($maxratio / $skip + 0.5);
    }
    print "pre-tweak $maxratio, skip $skip, scale $scale\n" if $debug;
    if ($skip && ($maxratio % $skip)) {
	$maxratio += $skip - ($maxratio % $skip);
	$scale = $twidth / int($maxratio / $skip + 0.5);
    }
    $skip = 1 unless $skip;

    # Tweak the # of little ticks to have between the big ones
    if ($skip > 5) {
	$minor_ticks = (($maxratio / $skip) > 10) ? 5 : 10;
    } else {
	$minor_ticks = 10;
    }
    $minor_ticks /= 2 if (($scale % $minor_ticks) &&
			  !($minor_ticks % 2) &&
			  (($scale / $minor_ticks) < 5));
    print "post-tweak $maxratio, skip $skip, scale $scale, minor_ticks $minor_ticks\n" if $debug;

    # The base line
    $im->line(0, $theight, $twidth, $theight, $black);

    # Now for the tick marks and numbers
    my $cx = 0;
    for(my $tick = 0; $tick <= ($maxratio / $skip); $tick++) {
	my $tx = $cx;
	my $tickval = $tick * $skip;
	my $tickstr = ($rate) ? ($maxratio - $tickval) : $tickval;
	my $tickwidth = length("$tickstr") * $font->width;
	$tx -= int($tickwidth / 2 + 0.5) unless ($tick == 0);
	$tx -= int($tickwidth / 2 + 0.5) if ($tick >= ($maxratio/$skip));
	$im->string($font, $tx, 0, $tickstr, $black);
	$im->line($cx, $theight-12, $cx, $theight, $black); # Major tick
	my $mtx = $cx;
	for my $i (2..$minor_ticks) {
	    $mtx += $scale/$minor_ticks;
	    $im->line(int($mtx + 0.5), $theight-6,
		      int($mtx + 0.5), $theight,
		      $black); # Minor tick
	}
	$cx += $scale;
    }
    # Do the last big tick
    $im->line($twidth-1, $theight-12, $twidth-1, $theight, $black);

    if ($debug) {
	# Debugging stuff
	$im->string($font, 0, $theight*1, "maxratio = $maxratio", $black);
	$im->string($font, 0, $theight*1.5, "skip = $skip", $black);
	$im->string($font, 0, $theight*2, "scale = $scale", $black);
	$im->string($font, 0, $theight*2.5, "maj = ".($maxratio/$skip), $black);
	$im->string($font, 0, $theight*3, "minor = $minor_ticks", $black);
    }

    my $fname = "${path}${name}.gif";
    my $fh = new IO::File ">$fname";
    if (defined($fh)) {
        binmode $fh;
	print $fh $im->gif;
	close $fh;
    } else {
	print "\nCouldn't open $fname for writing: $!\n";
	return undef;
    }
    return $fname;
}
