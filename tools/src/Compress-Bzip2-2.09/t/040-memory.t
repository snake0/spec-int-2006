# -*- mode: perl -*-

use Test::More tests => 10;
#use Test::More qw(no_plan);

BEGIN {
  use_ok('Compress::Bzip2', qw(:utilities :bzip1));
};


my $string = q/
Twas brillig and the slithy toves
did gire and gimble in the wabe
All mimsey were the borogroves
and the Momewrathes outgrabe
    /;

my $compress = memBzip( $string );
my $uncompress = memBunzip( $compress );

ok( $compress ne $string, "string was not inouted" );
ok( length($compress)-10 < length($string), "string compression - ".length($compress).' vs '.length($string) );
ok( $uncompress eq $string, "uncompressed is same as the original" );

my $string10 = $string x 10;
my $compress10 = memBzip( $string10 );
my $uncompress10 = memBunzip( $compress10 );

ok( $compress10 ne $string10, "x10 string was not inouted" );
ok( length($compress10) < length($string10), "x10 string compression - ".length($compress10).' vs '.length($string10) );
ok( $uncompress10 eq $string10, "x10 uncompressed is same as the original" );

$compress = compress( $string );
$uncompress = decompress( $compress );

ok( $compress ne $string, "bzip1 string was not inouted" );
ok( length($compress)-10 < length($string), "bzip1 string compression - ".length($compress).' vs '.length($string) );
ok( $uncompress eq $string, "bzip1 decompress is same as the original" );
