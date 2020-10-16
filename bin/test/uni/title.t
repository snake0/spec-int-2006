BEGIN {
  use TestInit;
#    chdir 't' if -d 't';
#    @INC = qw(../lib uni .);
push @INC, 'uni';
    require "case.pl";
}

casetest("Title", \%utf8::ToSpecTitle, sub { ucfirst $_[0] });

