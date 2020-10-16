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
#   $Id: symbols.pm,v 1.8 2005/03/14 22:01:06 fredo Exp $
#
#=======================================================================
package PDF::API2::Content::symbols;

my $symbol={
    'arrow' => {
        'centerpoint'   => [ 0, -1 ],   #   center of symbol
        'endpoint'      => [ 0,  0 ],   #   end of symbol
        'startpoint'    => [ 0, -2 ],   #   start of symbol (connector)
        'orientation'   => 0,           #   in degrees (0=upright)
        'stream'        =>              #   symbol description
            '0 -2 m -1 -3 l 0 0 l 1 3 l c',
    },
    'arrowfull' => {
        'centerpoint'   => [ 0, -2 ],   #   center of symbol
        'endpoint'      => [ 0,  0 ],   #   end of symbol
        'startpoint'    => [ 0, -3 ],   #   start of symbol (connector)
        'orientation'   => 0,           #   in degrees (0=upright)
        'stream'        =>              #   symbol description
            '0 -3 m -1 -3 l 0 0 l 1 3 l c',
    },
};

$symbol->{'->'} = $symbol->{arrow};
$symbol->{'|>'} = $symbol->{arrowfull};

1;

__END__

=head1 AUTHOR

alfred reibenschuh

=head1 HISTORY

    $Log: symbols.pm,v $
    Revision 1.8  2005/03/14 22:01:06  fredo
    upd 2005

    Revision 1.7  2004/06/15 09:14:41  fredo
    removed cr+lf

    Revision 1.6  2004/06/07 19:44:36  fredo
    cleaned out cr+lf for lf

    Revision 1.5  2004/01/29 08:37:46  fredo
    updated comments, added symbol aliases

    Revision 1.4  2003/12/08 13:05:31  Administrator
    corrected to proper licencing statement

    Revision 1.3  2003/11/30 17:24:34  Administrator
    merged into default

    Revision 1.2.2.1  2003/11/30 16:56:34  Administrator
    merged into default

    Revision 1.2  2003/11/29 23:08:40  Administrator
    added arrow and arrowfull definitions

    Revision 1.1  2003/11/29 22:27:46  Administrator
    Added CVS Id/Log


=cut
