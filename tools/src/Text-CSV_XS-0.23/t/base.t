# -*- perl -*-

require 5.004;
use strict;

use vars qw($loaded);

BEGIN { $| = 1; print "1..28\n"; }
END {print "not ok 1\n" unless $loaded;}
use Text::CSV_XS;
$loaded = 1;
print "ok 1\n";


my($testNum) = 1;
sub Test($) {
    my($result) = shift;
    $testNum++;
    print(($result ? "" : "not "), "ok $testNum\n");
    $result;
}


#
# empty subclass test
#
package Empty_Subclass;
@Empty_Subclass::ISA = qw(Text::CSV_XS);
package main;

#
#    Important: Do not modify these tests unless you have a good
#    reason. This file ought to guarantee compatibility to Text::CSV.
#
my($empty) = Empty_Subclass->new();
Test(ref($empty) eq 'Empty_Subclass')
    or printf("Expected subclass %s, got %s\n",
	      'Empty_Subclass', ref($empty));
Test($empty->version() == Text::CSV_XS->version())
    or printf("Expected version %s, got %s\n",
	      &Text::CSV_XS->version(), $empty->version());
Test($empty->parse(''))
    or print("Subclass parse() failed.\n");
Test($empty->combine(''))
    or printf("Subclass combine() failed.\n");

my $csv = Text::CSV_XS->new();

#
#    Important: Do not modify these tests unless you have a good
#    reason. This file ought to guarantee compatibility to Text::CSV.
#
Test(!$csv->combine())  # fail - missing argument
    or print "Missing argument, but no failure\n";
Test(!$csv->combine('abc', "def\n", 'ghi'))  # fail - bad character
    or print "Bad character, but no failure\n";
Test($csv->combine('') && ($csv->string eq q()))  # succeed
    or printf("Expected %s, got %s\n", q(), $csv->string());
Test($csv->combine('', ' ') && ($csv->string eq q(," ")))  # succeed
    or printf("Expected %s, got %s\n", q("",""), $csv->string());
Test($csv->combine('', 'I said, "Hi!"', '') &&
     ($csv->string eq q(,"I said, ""Hi!""",)))  # succeed
    or printf("Expected %s, got %s\n", q("","I said, ""Hi!""",""),
	      $csv->string());
Test($csv->combine('"', 'abc') && ($csv->string eq q("""",abc)))  # succeed
    or printf("Expected %s, got %s\n", q("""","abc"), $csv->string());
Test($csv->combine(',') && ($csv->string eq q(",")))  # succeed
    or printf("Expected %s, got %s\n", q("""","abc"), $csv->string());
Test($csv->combine('abc', '"') && ($csv->string eq q(abc,"""")))  # succeed
    or printf("Expected %s, got %s\n", q("abc",""""), $csv->string());
Test($csv->combine('abc', 'def', 'ghi', 'j,k') &&
     ($csv->string eq q(abc,def,ghi,"j,k")))  # succeed
    or printf("Expected %s, got %s\n", q(abc,def,ghi,"j,k"),
	      $csv->string());
Test($csv->combine("abc\tdef", 'ghi') &&
     ($csv->string eq qq("abc\tdef",ghi)))  # succeed
    or printf("Expected %s, got %s\n", qq("abc\tdef","ghi"),
	      $csv->string());
Test(!$csv->parse())
    or print "Missing argument, but no failure\n";
Test(!$csv->parse('"abc'))
    or print("Missing closing double-quote, but no failure\n");
Test(!$csv->parse('ab"c'))
    or print("Double quote outside of double-quotes, but no failure.\n");
Test(!$csv->parse('"ab"c"'))
    or print("Bad character sequence, but no failure.\n");
Test(!$csv->parse(qq("abc\nc")))
    or print("Bad character, but no failure.\n");
Test(!$csv->status())
    or print("Wrong status\n");
Test($csv->parse(q(",")) and ($csv->fields())[0] eq ',')  # success
    or printf("Expected field 0 to be ',', got %s\n", ($csv->fields())[0]);
Test($csv->parse(qq("","I said,\t""Hi!""","")));
Test(($csv->fields())[0] eq '')
    or printf("Expected field 0 to be '', got %s\n",
	      ($csv->fields())[0]);
Test(($csv->fields())[1] eq qq(I said,\t"Hi!"))
    or printf("Expected field 1 to be '%s', got %s\n",
              qq(I said,\t"Hi!"), ($csv->fields())[1]);
Test(($csv->fields())[2] eq '')
    or printf("Expected field 2 to be '', got %s\n",
	      ($csv->fields())[2]);
Test($csv->status())
    or print("Wrong status\n");


# Are Integers and Reals quoted?
#
#    Important: Do not modify these tests unless you have a good
#    reason. This file ought to guarantee compatibility to Text::CSV.
#
Test($csv->combine('', 2, 3.4, 'a', 'a b')
     && ($csv->string eq q(,2,3.4,a,"a b")))  # succeed
    or printf("Expected %s, got %s\n", q(""), $csv->string());
