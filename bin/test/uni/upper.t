BEGIN {
  use TestInit;
#    chdir 't' if -d 't';
#    @INC = qw(../lib uni .);
push @INC, 'uni';
    require "case.pl";
}

casetest("Upper", \%utf8::ToSpecUpper, sub { uc $_[0] });

