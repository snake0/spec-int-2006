package Text::CSV_XS;

# Copyright (c) 1998 Jochen Wiedmann. All rights reserved.
#
# Portions Copyright (c) 1997 Alan Citterman. All rights reserved.
#
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

################################################################################
# HISTORY
#
# Written by:
#    Jochen Wiedmann <joe@ispsoft.de>
#
# Based on Text::CSV by:
#    Alan Citterman <alan@mfgrtl.com>
#
############################################################################

require 5.004;
use strict;


use DynaLoader ();

use vars qw($VERSION @ISA);

$VERSION =     '0.23';
@ISA =         qw(DynaLoader);


sub PV () { 0 }
sub IV () { 1 }
sub NV () { 2 }


############################################################################
#
# version
#
#    class/object method expecting no arguments and returning the version
#    number of Text::CSV.  there are no side-effects.
#
############################################################################
sub version {
  return $VERSION;
}


############################################################################
#
# new
#
#    class/object method expecting no arguments and returning a reference to
#    a newly created Text::CSV object.
#
############################################################################

sub new ($;$) {
    my $proto = shift;  my $attr = shift || {};
    my $class = ref($proto) || $proto;
    my $self = { 'quote_char' => '"',
		 'escape_char' => '"',
		 'sep_char' => ',',
		 'eol' => '',
		 %$attr };
    bless $self, $class;
    if (exists($self->{'types'})) {
	$self->types($self->{'types'});
    }
    $self;
}


############################################################################
#
# status
#
#    object method returning the success or failure of the most recent
#    combine() or parse().  there are no side-effects.
############################################################################

sub status ($) {
  my $self = shift;
  return $self->{'_STATUS'};
}


############################################################################
#
# error_input
#
#    object method returning the first invalid argument to the most recent
#    combine() or parse().  there are no side-effects.
############################################################################

sub error_input ($) {
  my $self = shift;
  return $self->{'_ERROR_INPUT'};
}


############################################################################
#
# string
#
#    object method returning the result of the most recent combine() or the
#    input to the most recent parse(), whichever is more recent.  there are
#    no side-effects.
#
############################################################################

sub string ($) {
  my $self = shift;
  return $self->{'_STRING'};
}


############################################################################
# fields
#
#    object method returning the result of the most recent parse() or the
#    input to the most recent combine(), whichever is more recent.  there
#    are no side-effects.
#
############################################################################

sub fields ($) {
  my $self = shift;
  if (ref($self->{'_FIELDS'})) {
    return @{$self->{'_FIELDS'}};
  }
  return undef;
}


############################################################################
#
# combine
#
#    object method returning success or failure.  the given arguments are
#    combined into a single comma-separated value.  failure can be the
#    result of no arguments or an argument containing an invalid character.
#    side-effects include:
#      setting status()
#      setting fields()
#      setting string()
#      setting error_input()
#
############################################################################

sub combine ($@) {
  my $self = shift;
  my @part = @_;
  my($str) = '';
  my($ref) = \$str;
  $self->{'_FIELDS'} = \@part;
  $self->{'_ERROR_INPUT'} = undef;
  $self->{'_STATUS'} =
      (@part > 0)  &&  $self->Encode(\$str, \@part, 0, $self->{'eol'});
  $self->{'_STRING'} = $str;
  $self->{'_STATUS'};
}


############################################################################
#
# parse
#
#    object method returning success or failure.  the given argument is
#    expected to be a valid comma-separated value.  failure can be the
#    result of no arguments or an argument containing an invalid sequence
#    of characters. side-effects include:
#      setting status()
#      setting fields()
#      setting string()
#      setting error_input()
#
#############################################################################

sub parse ($$) {
  my($self, $str) = @_;
  my($fields) = [];
  $self->{'_STRING'} = $self->{'ERROR_INPUT'} = $str;
  $self->{'_STATUS'} = 0;
  $self->{'_FIELDS'} = undef;
  if (defined($str)  &&  $self->Decode($str, $fields, 0)) {
      $self->{'_FIELDS'} = $fields;
      $self->{'_STATUS'} = 1;
  }
  return ($self->{'_STATUS'});
}


############################################################################
#
#    Name:    print (Instance method)
#
#    Purpose: Similar to combine, but the fields are encoded to an
#             IO stream or something similar. To be precise: An
#             object supporting a "print" method.
#
#    Inputs:  $self   - Instance
#             $io     - IO handle or similar object
#             $fields - Array ref to array of fields
#
#    Returns: TRUE for success, FALSE otherwise. In the latter case
#             you may look at $self->error_input() or check the IO
#             object for errors.
#
############################################################################

# sub print ($$$) {
#     my($self, $io, $fields) = @_;
#     $self->{'_ERROR_INPUT'} = undef;
#     $self->{'_STRING'} = undef;
#     $self->{'_FIELDS'} = $fields;
#     $self->{'_STATUS'} = $self->Encode($io, $fields, 1, $self->{'eol'});
# }


############################################################################
#
#    Name:    getline (Instance method)
#
#    Purpose: Similar to parse, but the fields are decoded from an
#             IO stream or something similar. To be precise: An
#             object supporting a "getline" method.
#
#             Note that it may happen that multiple lines are read,
#             if the fields contain line feeds and we are in binary
#             mode. For example, MS Excel creates such files!
#
#    Inputs:  $self   - Instance
#             $io     - IO handle or similar object
#
#    Returns: Array ref of fields for success, undef otherwise.
#             In the latter case you may look at $self->error_input()
#             or check the IO object for errors.
#
############################################################################

# sub getline ($$) {
#     my($self, $io) = @_;
#     my($fields) = [];
#     $self->{'_ERROR_INPUT'} = undef;
#     $self->{'_STRING'} = undef;
#     $self->{'_FIELDS'} = $fields;
#     if ($self->{'_STATUS'} = $self->Decode($io, $fields, 1)) {
# 	return $fields;
#     }
#     return undef;
# }


bootstrap Text::CSV_XS $VERSION;

sub types {
    my $self = shift;
    if (@_) {
	if (my $types = shift) {
	    $self->{'_types'} = join("", map{ chr($_) } @$types);
	    $self->{'types'} = $types;
	} else {
	    delete $self->{'types'};
	    delete $self->{'_types'};
	    undef;
	}
    } else {
	$self->{'types'};
    }
}

1;

__END__

=head1 NAME

Text::CSV_XS - comma-separated values manipulation routines


=head1 SYNOPSIS

 use Text::CSV_XS;

 $csv = Text::CSV_XS->new();           # create a new object
 $csv = Text::CSV_XS->new(\%attr);     # create a new object

 $status = $csv->combine(@columns);    # combine columns into a string
 $line = $csv->string();               # get the combined string

 $status = $csv->parse($line);         # parse a CSV string into fields
 @columns = $csv->fields();            # get the parsed fields

 $status = $csv->status();             # get the most recent status
 $bad_argument = $csv->error_input();  # get the most recent bad argument

 $status = $csv->print($io, $columns); # Write an array of fields immediately
                                       # to a file $io

 $columns = $csv->getline($io);        # Read a line from file $io, parse it
                                       # and return an array ref of fields

 $csv->types(\@t_array);               # Set column types


=head1 DESCRIPTION

Text::CSV_XS provides facilities for the composition and decomposition of
comma-separated values.  An instance of the Text::CSV_XS class can combine
fields into a CSV string and parse a CSV string into fields.


=head1 FUNCTIONS

=over 4

=item version()

(Class method) Returns the current module version.

=item new(\%attr)

(Class method) Returns a new instance of Text::CSV_XS. The objects
attributes are described by the (optional) hash ref C<\%attr>.
Currently the following attributes are available:

=over 8

=item quote_char

The char used for quoting fields containing blanks, by default the
double quote character (C<">). A value of undef suppresses
quote chars. (For simple cases only).

=item eol

An end-of-line string to add to rows, usually C<undef> (nothing,
default), C<"\012"> (Line Feed) or C<"\015\012"> (Carriage Return,
Line Feed)

=item escape_char

The char used for escaping certain characters inside quoted fields,
by default the same character. (C<">)

=item sep_char

The char used for separating fields, by default a comme. (C<,>)

=item binary

If this attribute is TRUE, you may use binary characters in quoted fields,
including line feeds, carriage returns and NUL bytes. (The latter must
be escaped as C<"0>.) By default this feature is off.

=item types

A set of column types; this attribute is immediately passed to the
I<types> method below. You must not set this attribute otherwise,
except for using the I<types> method. For details see the description
of the I<types> method below.

=item always_quote

By default the generated fields are quoted only, if they need to, for
example, if they contain the separator. If you set this attribute to
a TRUE value, then all fields will be quoted. This is typically easier
to handle in external applications. (Poor creatures who aren't using
Text::CSV_XS. :-)

=back

To sum it up,

 $csv = Text::CSV_XS->new();

is equivalent to

 $csv = Text::CSV_XS->new({
     'quote_char'  => '"',
     'escape_char' => '"',
     'sep_char'    => ',',
     'binary'      => 0
 });

=item combine

 $status = $csv->combine(@columns);

This object function constructs a CSV string from the arguments, returning
success or failure.  Failure can result from lack of arguments or an argument
containing an invalid character.  Upon success, C<string()> can be called to
retrieve the resultant CSV string.  Upon failure, the value returned by
C<string()> is undefined and C<error_input()> can be called to retrieve an
invalid argument.

=item print

 $status = $csv->print($io, $columns);

Similar to combine, but it expects an array ref as input (not an array!)
and the resulting string is not really created, but immediately written
to the I<$io> object, typically an IO handle or any other object that
offers a I<print> method. Note, this implies that the following is wrong:

 open(FILE, ">whatever");
 $status = $csv->print(\*FILE, $columns);

The glob C<\*FILE> is not an object, thus it doesn't have a print
method. The solution is to use an IO::File object or to hide the
glob behind an IO::Wrap object. See L<IO::File(3)> and L<IO::Wrap(3)>
for details.

For performance reasons the print method doesn't create a result string.
In particular the I<$csv-E<gt>string()>, I<$csv-E<gt>status()>,
I<$csv->fields()> and I<$csv-E<gt>error_input()> methods are meaningless
after executing this method.

=item string

 $line = $csv->string();

This object function returns the input to C<parse()> or the resultant CSV
string of C<combine()>, whichever was called more recently.

=item parse

 $status = $csv->parse($line);

This object function decomposes a CSV string into fields, returning
success or failure.  Failure can result from a lack of argument or the
given CSV string is improperly formatted.  Upon success, C<fields()> can
be called to retrieve the decomposed fields .  Upon failure, the value
returned by C<fields()> is undefined and C<error_input()> can be called
to retrieve the invalid argument.

You may use the I<types()> method for setting column types. See the
description below.


=item getline

 $columns = $csv->getline($io);

This is the counterpart to print, like parse is the counterpart to
combine: It reads a row from the IO object $io using $io->getline()
and parses this row into an array ref. This array ref is returned
by the function or undef for failure.

The I<$csv-E<gt>string()>, I<$csv-E<gt>fields()> and I<$csv-E<gt>status()>
methods are meaningless, again.

=item types

 $csv->types(\@tref);

This method is used to force that columns are of a given type. For
example, if you have an integer column, two double columns and a
string column, then you might do a

 $csv->types([Text::CSV_XS::IV(),
              Text::CSV_XS::NV(),
              Text::CSV_XS::NV(),
              Text::CSV_XS::PV()]);

Column types are used only for decoding columns, in other words
by the I<parse()> and I<getline()> methods.

You can unset column types by doing a

 $csv->types(undef);

or fetch the current type settings with

 $types = $csv->types();

=item fields

 @columns = $csv->fields();

This object function returns the input to C<combine()> or the resultant
decomposed fields of C<parse()>, whichever was called more recently.

=item status

 $status = $csv->status();

This object function returns success (or failure) of C<combine()> or
C<parse()>, whichever was called more recently.

=item error_input

 $bad_argument = $csv->error_input();

This object function returns the erroneous argument (if it exists) of
C<combine()> or C<parse()>, whichever was called more recently.

=back

=head1 EXAMPLE

  require Text::CSV_XS;

  my $csv = Text::CSV_XS->new;

  my $column = '';
  my $sample_input_string = '"I said, ""Hi!""",Yes,"",2.34,,"1.09"';
  if ($csv->parse($sample_input_string)) {
    my @field = $csv->fields;
    my $count = 0;
    for $column (@field) {
      print ++$count, " => ", $column, "\n";
    }
    print "\n";
  } else {
    my $err = $csv->error_input;
    print "parse() failed on argument: ", $err, "\n";
  }

  my @sample_input_fields = ('You said, "Hello!"',
			     5.67,
			     'Surely',
			     '',
			     '3.14159');
  if ($csv->combine(@sample_input_fields)) {
    my $string = $csv->string;
    print $string, "\n";
  } else {
    my $err = $csv->error_input;
    print "combine() failed on argument: ", $err, "\n";
  }

=head1 CAVEATS

This module is based upon a working definition of CSV format which may not be
the most general.

=over 4

=item 1 

Allowable characters within a CSV field include 0x09 (tab) and the inclusive
range of 0x20 (space) through 0x7E (tilde). In binary mode all characters
are accepted, at least in quoted fields:

=item 2

A field within CSV may be surrounded by double-quotes. (The quote char)

=item 3

A field within CSV must be surrounded by double-quotes to contain a comma.
(The separator char)

=item 4

A field within CSV must be surrounded by double-quotes to contain an embedded
double-quote, represented by a pair of consecutive double-quotes. In binary
mode you may additionally use the sequence C<"0> for representation of a
NUL byte.

=item 5

A CSV string may be terminated by 0x0A (line feed) or by 0x0D,0x0A
(carriage return, line feed).

=head1 AUTHOR

Alan Citterman F<E<lt>alan@mfgrtl.comE<gt>> wrote the original Perl
module. Please don't send mail concerning Text::CSV_XS to Alan, as
he's not involved in the C part which is now the main part of the
module.

Jochen Wiedmann F<E<lt>joe@ispsoft.deE<gt>> rewrote the encoding and
decoding in C by implementing a simple finite-state machine and added
the variable quote, escape and separator characters, the binary mode
and the print and getline methods.

=head1 SEE ALSO

L<perl(1)>, L<IO::File(3)>, L<IO::Wrap(3)>

=cut
