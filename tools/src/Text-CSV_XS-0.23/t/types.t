# -*- perl -*-

require 5.004;
use strict;

use Text::CSV_XS ();


{
    my $testNum = 0;
    sub Test($) {
	my($result) = shift;
	$testNum++;
	print(($result ? "" : "not "), "ok $testNum\n");
	$result;
    }
}

$| = 1;
$^W = 1;


print "1..12\n";

my $csv = Text::CSV_XS->new({'types' => [Text::CSV_XS::IV(),
					 Text::CSV_XS::PV(),
					 Text::CSV_XS::NV()]});
Test($csv);
Test(@{$csv->{'types'}} == 3);
Test($csv->{'types'}->[0] == Text::CSV_XS::IV()  and
     $csv->{'types'}->[1] == Text::CSV_XS::PV()  and
     $csv->{'types'}->[2] == Text::CSV_XS::NV());
Test(length($csv->{'_types'}) == 3);
Test($csv->{'_types'} eq
     chr(Text::CSV_XS::IV()) . chr(Text::CSV_XS::PV()) .
     chr(Text::CSV_XS::NV()));

Test($csv->combine('', '', '1.00'));
Test($csv->string() eq ",,1.00");
my $warning;
$SIG{__WARN__} = sub { $warning = shift };
Test($csv->parse($csv->string()));
Test($warning =~ /numeric/);
my @fields = $csv->fields();
Test($fields[0] eq '0');
Test($fields[1] eq '');
Test($fields[2] eq '1');
