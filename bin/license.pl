#!/spec/cpu2006/bin/specperl
#!/spec/cpu2006/bin/specperl -d
#!/usr/bin/perl
#
#  license.pl - a tool for approving the license for SPEC benchmarks.
#  Copyright (C) 2004-2006 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
#  Authors: Bodo Parady, Cloyce D. Spradling
#
use strict;
use Digest::MD5;
use Config;
use vars qw($md5 $stampfile);

die "There's no LICENSE file in this directory!\nStopped" unless (-f 'LICENSE');

$md5 = Digest::MD5->new;

$stampfile='spec_license_stamp';

for(my $i = 0; $i < 24; $i++) {
  print "\n";   # Poor man's screen clear
}

pager('LICENSE');
print "\nIf you agree to the terms of the above SPEC license, please enter \"yes\": ";
my $resp = <>;
chomp($resp);
die "\n\nLicense not accepted -- aborting installation.\nStopped" unless ($resp eq 'yes');

open(STAMP,"+>$stampfile") || die "Can't open $stampfile for writing: $!\nStopped";

print STAMP 'License accepted: ',scalar(localtime),"\n";
# This stuff should be in the path.  If not... oh, well.
if (open(INFO, 'who am i|')) {
    print STAMP <INFO>;
}
# Just in case the 'who am i' fails, grub around in the environment a bit
foreach my $envvar (qw(LOGNAME USER USERNAME)) {
    if (exists($ENV{$envvar}) && ($ENV{$envvar} ne '')) {
	print STAMP "$envvar: ",$ENV{$envvar},"\n";
    }
}
if (open(INFO, 'uname -a|')) {
    print STAMP <INFO>;
}
if (open(INFO, 'hostid|')) {
    print STAMP <INFO>;
}
close(INFO);

seek(STAMP, 0, 0);
$md5->reset;
$md5->addfile(*STAMP);
seek(STAMP, 0, 2);		# Make sure it's at the end
print STAMP "MD5 ",$md5->hexdigest,"\n";
close(STAMP);

sub pager {
    my ($file) = @_;

    my $pager = $Config{'pager'}; # Hopefully a reasonable default
    $pager = 'more' unless (-x $pager);

    if (system($pager, $file)) {
	die "Error running $pager: $!\nAborting installation.\nStopped" ;
    }
}
