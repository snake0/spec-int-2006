# -*- perl -*-

require 5.004;
use strict;
use Text::CSV_XS ();

#
#	Some assorted examples from the modules history
#


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
print "1..7\n";



#
#	"Pavel Kotala" <pkotala@logis.cz>
#
{
    my $csv = Text::CSV_XS->new({'quote_char' => '"',
				 'escape_char' => '\\',
				 'sep_char' => ';',
				 'binary' => 1});
    Test($csv);
    my @list = ("c:\\winnt", "text");
    Test($csv->combine(@list));
    my $line = $csv->string();
    Test($line);
    Test($csv->parse($line));
    my @olist = $csv->fields();
    Test(@list == @olist);
    Test($list[0] eq $olist[0]);
    Test($list[1] eq $olist[1]);
}
