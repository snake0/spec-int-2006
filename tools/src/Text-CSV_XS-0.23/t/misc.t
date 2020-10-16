# -*- perl -*-

require 5.004;
use strict;

require Text::CSV_XS;


my($testNum) = 0;
sub Test($) {
    my($result) = shift;
    $testNum++;
    print(($result ? "" : "not "), "ok $testNum\n");
    $result;
}

$| = 1;
print "1..15\n";

my(@binFields) = ("abc\0def\n\rghi", "ab\"ce,\032\"'", "\377");

my($csv) = Text::CSV_XS->new({'binary' => 1});
Test($csv->combine(@binFields)) or print "Failed to encode binary fields\n";
my($string) = $csv->string();
Test($string eq qq("abc"0def\n\rghi","ab""ce,\032""'",\377))
    or printf("Encode: Expected \n%s\n, got \n%s\n",
             unpack("H*", qq("abc"0def\n\rghi","ab""ce,\032""'")),
             unpack("H*", $string));
Test($csv->parse($string)) or print "Failed to decode binary fields\n";
Test($csv->fields() == @binFields) or print "Wrong number of fields.\n";
Test(($csv->fields())[0] eq $binFields[0])
    or printf("Field 0: Expected %s, got %s.\n",
	      $binFields[0], ($csv->fields())[0]);
Test(($csv->fields())[1] eq $binFields[1])
    or printf("Field 1: Expected %s, got %s.\n",
	      $binFields[1], ($csv->fields())[1]);
Test(($csv->fields())[2] eq $binFields[2])
    or printf("Field 1: Expected %s, got %s.\n",
	      $binFields[1], ($csv->fields())[1]);
$csv->{'eol'} = "\r\n";
Test($csv->combine(@binFields)) or print "Failed to encode binary fields\n";
$string = $csv->string();
Test($string eq qq("abc"0def\n\rghi","ab""ce,\032""'",\377\r\n))
    or printf("Encode: Expected \n%s\n, got \n%s\n",
             unpack("H*", qq("abc"0def\n\rghi","ab""ce,\032""'")),
             unpack("H*", $string));
$csv->{'eol'} = "\n";
Test($csv->combine(@binFields)) or print "Failed to encode binary fields\n";
$string = $csv->string();
Test($string eq qq("abc"0def\n\rghi","ab""ce,\032""'",\377\n))
    or printf("Encode: Expected \n%s\n, got \n%s\n",
             unpack("H*", qq("abc"0def\n\rghi","ab""ce,\032""'")),
             unpack("H*", $string));
$csv->{'quote_char'} = undef;
Test($csv->combine("abc","def","ghi"));
Test($csv->string() eq qq(abc,def,ghi\n));


# Ken's test
{
    my $csv2 = Text::CSV_XS->new({'always_quote' => 1});
    Test($csv2->combine("abc","def","ghi"));
    Test($csv2->string() eq qq("abc","def","ghi"))
	or printf("Expected %s, got %s.\n", qq("abc","def","ghi"),
		  $csv2->string());
}
