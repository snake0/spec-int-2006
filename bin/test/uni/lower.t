BEGIN {
  use TestInit;
#    chdir 't' if -d 't';
#    @INC = qw(../lib uni .);
push @INC, 'uni';
    require "case.pl";
}

casetest("Lower", \%utf8::ToSpecLower, sub { lc $_[0] });

