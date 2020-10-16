#=======================================================================
#    ____  ____  _____              _    ____ ___   ____
#   |  _ \|  _ \|  ___|  _   _     / \  |  _ \_ _| |___ \
#   | |_) | | | | |_    (_) (_)   / _ \ | |_) | |    __) |
#   |  __/| |_| |  _|    _   _   / ___ \|  __/| |   / __/
#   |_|   |____/|_|     (_) (_) /_/   \_\_|  |___| |_____|
#
#   A Perl Module Chain to faciliate the Creation and Modification
#   of High-Quality "Portable Document Format (PDF)" Files.
#
#   Copyright 1999-2005 Alfred Reibenschuh <areibens@cpan.org>.
#
#=======================================================================
#
#   This library is free software; you can redistribute it and/or
#   modify it under the terms of the GNU Lesser General Public
#   License as published by the Free Software Foundation; either
#   version 2 of the License, or (at your option) any later version.
#
#   This library is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#   Lesser General Public License for more details.
#
#   You should have received a copy of the GNU Lesser General Public
#   License along with this library; if not, write to the
#   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
#   Boston, MA 02111-1307, USA.
#
#   $Id: Markup.pm,v 1.5 2005/03/14 22:01:06 fredo Exp $
#
#=======================================================================
package PDF::API2::Content::Text::Markup::XML;

BEGIN {

    use strict;
    use vars qw(@ISA);

    use XML::Parser;

    @ISA = qw(XML::Parser);

}

sub new {
    my $class=shift @_;
    my $self = $class->SUPER::new(
        Handlers => {
            Start   => \&xmlStartTag,
            End     => \&xmlEndTag,
            Char    => \&xmlChars,
        },
        ProtocolEncoding => 'UTF-8',
        ErrorContext => 3,
        Pkg => $class,
    );
    $self->{pdfxmlstream}=[];
    return($self);
}

sub pushstream {
    my $self=shift @_;
    push @{$self->{pdfxmlstream}},@_;
}
sub popstream {
    my $self=shift @_;
    return(pop @{$self->{pdfxmlstream}});
}
sub returnstreamref {
    my $self=shift @_;
    return($self->{pdfxmlstream});
}

sub xmlStartTag {
    my ($self,$tag,%attr)=@_;
    PDF::API2::Content::Text::Markup::XML::pushstream($self,[$tag,{%attr}]);
}


sub xmlEndTag {
    my ($self,$tag)=@_;
    PDF::API2::Content::Text::Markup::XML::pushstream($self,[$tag]);
}


sub xmlChars {
    my ($self,$chars)=@_;
    if($chars=~m|^[\s\012\015]+$|o) {
        $chars=' ';
        my $prev=PDF::API2::Content::Text::Markup::XML::popstream($self);
        if(ref $prev) {
            $chars=$prev;
        } else {
            PDF::API2::Content::Text::Markup::XML::pushstream($self,$prev);
        }
    } else {
        chomp($chars);
        $chars=~s|\s+| |go;
    }
    PDF::API2::Content::Text::Markup::XML::pushstream($self,$chars);
}


#=========================================================================================#

package PDF::API2::Content::Text::Markup;

BEGIN {

    use strict;
    use vars qw(@ISA $VERSION);

    use PDF::API2::Util;
    use PDF::API2::Basic::PDF::Utils;
    use PDF::API2::Content;

    @ISA = qw(PDF::API2::Content);

    ( $VERSION ) = '$Revision: 1.5 $' =~ /Revision: (\S+)\s/; # $Date: 2005/03/14 22:01:06 $
    
}

sub _parse_style {
    # string should be 
    my $string=shift @_;
    my @opts=split(/\s*;\s*/,$string);
    my $style={};
    foreach my $line (@opts) {
        my ($key,$value) = split(/\s*:\s*/,$line);
        $key=~s|^\s+||go;
        $key=~s|\s+$||go;
        $value=~s|^\s+||go;
        $value=~s|\s+$||go;
        $style->{$key} = [map { $_=~s|^\s+||go; $_=~s|\s+$||go; $_; } split(/,/,$value)];
    }
    return($style);
}

sub _parse_markup {
    my ($self,$str)=@_;
    
    my $xml = PDF::API2::Content::Text::Markup::XML->new;
    $xml->parse($str);
    my $stream=$xml->returnstreamref;

    foreach my $t (@{$stream}) {
        if((ref($t) eq 'ARRAY') && (scalar @{$t} > 1)) {
            #print "<$t->[0]";
            #if(scalar keys %{$t->[1]} > 0) {
            #    print ' '.join(' ', map { "$_=\"$t->[1]->{$_}\"" } keys %{$t->[1]});
            #}
            #print '>';
        } elsif((ref($t) eq 'ARRAY') && (scalar @{$t} == 1)) {
            #print "</$t->[0]>";
        } else {
            #print $t; # needs xml-escape !!
        }
    }
    return();
}


1;

__END__

=head1 AUTHOR

some

=head1 HISTORY

    $Log: Markup.pm,v $
    Revision 1.5  2005/03/14 22:01:06  fredo
    upd 2005

    Revision 1.4  2004/06/15 09:14:41  fredo
    removed cr+lf

    Revision 1.3  2004/06/07 19:44:36  fredo
    cleaned out cr+lf for lf

    Revision 1.2  2004/02/01 21:32:29  fredo
    added basic (yet unfinished) parsing code

    Revision 1.1  2004/01/29 09:03:02  fredo
    genesis


=cut
