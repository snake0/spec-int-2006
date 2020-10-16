# -*- perl -*-

require 5.004;
use strict;

require Text::CSV_XS;
require PerlBenchmark;


my(@fields) = ("Wiedmann", "Jochen", "Am Eisteich 9", "72555 Metzingen",
	       "Germany", "+49 7123 14881", "joe\@ispsoft,de");

my($csv) = Text::CSV_XS->new();
my($count) = 10000;

print "Testing row creation speed ...\n";
my($t1) = Benchmark->new();
for (my($i) = 0;  $i < $count;  $i++) {
    $csv->combine(@fields);
}
my($td) = Benchmark::timediff(Benchmark->new(), $t1);
my($dur) = $td->cpu_a;
printf("$count rows created in %.1f cpu+sys seconds (%d per sec)\n\n",
       $dur, $count / $dur);

print "Testing row parsing speed ...\n";
my($str) = $csv->string();
$t1 = Benchmark->new();
for (my($i) = 0;  $i < $count;  $i++) {
    $csv->parse($str);
}
$td = Benchmark::timediff(Benchmark->new(), $t1);
$dur = $td->cpu_a;
printf("$count rows parsed in %.1f cpu+sys seconds (%d per sec)\n\n",
       $dur, $count / $dur);

