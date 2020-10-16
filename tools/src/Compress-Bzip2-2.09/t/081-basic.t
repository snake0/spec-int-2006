# -*- mode: perl -*-

## this is the 02basic.t test from Compress::Bzip2 1.03 with no changes
## (except for comments)

# simple compression/decompression

use strict;

use Test::More tests => 208;
use Compress::Bzip2 qw(compress decompress);


# globals

my $Level = 1;
my ($In,$Out) = (0,0);
my @AlNum = ('A'..'Z','a'..'z','0'..'9',' ');


# subs

sub try {
  my $str = shift;
  my $comp = compress($str,$Level);
  return 0 if not defined $comp;
  my $orig = decompress($comp);
  return 0 if not defined $orig;
  $In  += length $orig;
  $Out += length $comp;
  return $orig eq $str ? 1 : 0;
}


# tests

# some short strings

my $sum;
ok(try(''),'empty string');
ok((eval { try(undef) }, $@),'undef fails');
$sum = 0;
$sum += try($_) for @AlNum;
is($sum,scalar(@AlNum),'alphanumerics');
ok(try('FOO'),'FOO');
ok(try('bar'),'bar');
ok(try('          '),'spaces');

# references are supposed to work too

my $str = 'reference test';
$sum = 0;
for(1..5) {
  $str = \$str;
  $sum += try($str);
}
is($sum,5,'reference tests');

# random strings

$sum = 0;
for my $random(1..100) {
  $Level = 9 if 80 == $random;
  my $str = '';
  $str .= $AlNum[rand @AlNum] for 1..rand 100;
  $sum += try($str);
}
is($sum,100,'100 random strings');

# long strings with repetition

($In,$Out) = (0,0);
for my $random(1..100) {
  $Level = 1 if 20 == $random;
  my $str = '';
  $str .= ($AlNum[rand @AlNum] x rand 1000) for 1..100+rand 900;
  ok(try($str),"long string $random");
}
#diag(sprintf "compression ratio %.2f%%",100*$Out/$In);

# binary strings

($In,$Out) = (0,0);
for my $random(1..100) {
  $Level = 9 if 80 == $random;
  my $str = '';
  $str .= chr(rand 256) for 1..1000+rand 9000;
  ok(try($str),"binary string $random");
}
#diag(sprintf "compression ratio %.2f%%",100*$Out/$In);

