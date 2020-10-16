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
#   $Id: Util.pm,v 1.20 2005/03/15 00:59:43 fredo Exp $
#
#=======================================================================
package PDF::API2::Util;

no warnings qw[ recursion uninitialized ];

BEGIN {

    use utf8;
    use Encode qw(:all);

    use vars qw(
        $VERSION 
        @ISA 
        @EXPORT 
        @EXPORT_OK 
        %colors 
        $key_var 
        $key_var2 
        %u2n 
        %n2u 
        %u2n_o 
        %n2u_o 
        $pua
        $uuu
        %PaperSizes
    );
    use Math::Trig;
    use List::Util qw(min max);
    use PDF::API2::Basic::PDF::Utils;
    use PDF::API2::Basic::PDF::Filter;

    use POSIX qw( HUGE_VAL floor );

    use Exporter;
    @ISA = qw(Exporter);
    @EXPORT = qw(
        pdfkey
        pdfkey2
        float floats floats5 intg intgs
        mMin mMax
        HSVtoRGB RGBtoHSV HSLtoRGB RGBtoHSL RGBtoLUM
        namecolor namecolor_cmyk namecolor_lab optInvColor defineColor
        dofilter unfilter
        nameByUni uniByName initNameTable defineName
        page_size
        getPaperSizes
    );
    @EXPORT_OK = qw(
        pdfkey
        pdfkey2
        digest digestx digest16 digest32
        float floats floats5 intg intgs
        mMin mMax
        cRGB cRGB8 RGBasCMYK
        HSVtoRGB RGBtoHSV HSLtoRGB RGBtoHSL RGBtoLUM
        namecolor namecolor_cmyk namecolor_lab optInvColor defineColor
        dofilter unfilter
        nameByUni uniByName initNameTable defineName
        page_size
    );


    %PaperSizes=(
        '4a'        =>  [ 4760  , 6716  ],
        '2a'        =>  [ 3368  , 4760  ],
        'a0'        =>  [ 2380  , 3368  ],
        'a1'        =>  [ 1684  , 2380  ],
        'a2'        =>  [ 1190  , 1684  ],
        'a3'        =>  [ 842   , 1190  ],
        'a4'        =>  [ 595   , 842   ],
        'a5'        =>  [ 421   , 595   ],
        'a6'        =>  [ 297   , 421   ],
        '4b'        =>  [ 5656  , 8000  ],
        '2b'        =>  [ 4000  , 5656  ],
        'b0'        =>  [ 2828  , 4000  ],
        'b1'        =>  [ 2000  , 2828  ],
        'b2'        =>  [ 1414  , 2000  ],
        'b3'        =>  [ 1000  , 1414  ],
        'b4'        =>  [ 707   , 1000  ],
        'b5'        =>  [ 500   , 707   ],
        'b6'        =>  [ 353   , 500   ],
        'letter'    =>  [ 612   , 792   ],
        'broadsheet'    =>  [ 1296  , 1584  ],
        'ledger'    =>  [ 1224  , 792   ],
        'tabloid'   =>  [ 792   , 1224  ],
        'legal'     =>  [ 612   , 1008  ],
        'executive' =>  [ 522   , 756   ],
        '36x36'     =>  [ 2592  , 2592  ],
    );

    no warnings qw[ recursion uninitialized ];

    ( $VERSION ) = '$Revision: 1.20 $' =~ /Revision: (\S+)\s/; # $Date: 2005/03/15 00:59:43 $

    $key_var='CBA';
    $key_var2=0;

    $pua=0xE000;

    %u2n_o=();
    %n2u_o=();

    $uuu={g=>{},u=>{}};
    foreach my $dir (@INC) {
        if(-f "$dir/PDF/API2/Resource/uniglyph.txt")
        {
            my ($fh,$line);
            open($fh,"$dir/PDF/API2/Resource/uniglyph.txt");
            while($line=<$fh>)
            {
                next if($line=~m|^#|);
                chomp($line);
                $line=~s|\s+\#.+$||go;
                my ($uni,$name,$prio)=split(/\s+;\s+/,$line);
                $uni=hex($uni);
                $uuu->{u}->{$uni}||=[];
                $uuu->{g}->{$name}||=[];
                push @{$uuu->{u}->{$uni}},{uni=>$uni,name=>$name,prio=>$prio};    
                push @{$uuu->{g}->{$name}},{uni=>$uni,name=>$name,prio=>$prio};    
            }
            close($fh);
            last;
        }
    }
    foreach my $k (sort {$a<=>$b} keys %{$uuu->{u}})
    {
        $u2n_o{$k}=$uuu->{u}->{$k}->[0]->{name};
    }
    foreach my $k (keys %{$uuu->{g}})
    {
        my($r)=sort {$a->{prio}<=>$b->{prio}} @{$uuu->{g}->{$k}};
        $n2u_o{$k}=$r->{uni};
    }
    $uuu=undef;

    %u2n=%u2n_o;
    %n2u=%n2u_o;

    %colors=(
        'aliceblue'                => '#EFF7FF',   #
        'antiquewhite'             => '#F9EAD7',   #
        'antiquewhite1'            => '#FFEEDB',   #
        'antiquewhite2'            => '#EDDFCC',   #
        'antiquewhite3'            => '#CDBFB0',   #
        'antiquewhite4'            => '#8A8278',   #
        'aqua'                     => '#00FFFF',   #
        'aquamarine'               => '#7FFFD4',   #
        'aquamarine1'              => '#7FFFD4',   #
        'aquamarine2'              => '#76EDC5',   #
        'aquamarine3'              => '#66CDAA',   #
        'aquamarine4'              => '#458A74',   #
        'azure'                    => '#EFFFFF',   #
        'azure1'                   => '#EFFFFF',   #
        'azure2'                   => '#E0EDED',   #
        'azure3'                   => '#C0CDCD',   #
        'azure4'                   => '#828A8A',   #
        'beige'                    => '#F4F4DC',   #
        'bisque'                   => '#FFE4C3',   #
        'bisque1'                  => '#FFE4C3',   #
        'bisque2'                  => '#EDD5B6',   #
        'bisque3'                  => '#CDB69E',   #
        'bisque4'                  => '#8A7D6B',   #
        'black'                    => '#000000',   #
        'blanchedalmond'           => '#FFEACD',   #
        'blue'                     => '#0000FF',   #
        'blue1'                    => '#0000FF',   #
        'blue2'                    => '#0000ED',   #
        'blue3'                    => '#0000CD',   #
        'blue4'                    => '#00008A',   #
        'blueviolet'               => '#9F5E9F',   #
        'brass'                    => '#B4A642',   #
        'brightgold'               => '#D9D918',   #
        'bronze'                   => '#8B7852',   #
        'bronzeii'                 => '#A67D3D',   #
        'brown'                    => '#A52929',   #
        'brown1'                   => '#FF4040',   #
        'brown2'                   => '#ED3B3B',   #
        'brown3'                   => '#CD3333',   #
        'brown4'                   => '#8A2222',   #
        'burlywood'                => '#DEB786',   #
        'burlywood1'               => '#FFD39B',   #
        'burlywood2'               => '#EDC490',   #
        'burlywood3'               => '#CDAA7D',   #
        'burlywood4'               => '#8A7354',   #
        'cadetblue'                => '#5E9EA0',   #
        'cadetblue1'               => '#98F4FF',   #
        'cadetblue2'               => '#8DE5ED',   #
        'cadetblue3'               => '#7AC4CD',   #
        'cadetblue4'               => '#52858A',   #
        'chartreuse'               => '#7FFF00',   #
        'chartreuse1'              => '#7FFF00',   #
        'chartreuse2'              => '#76ED00',   #
        'chartreuse3'              => '#66CD00',   #
        'chartreuse4'              => '#458A00',   #
        'chocolate'                => '#D2691D',   #
        'chocolate1'               => '#FF7F23',   #
        'chocolate2'               => '#ED7620',   #
        'chocolate3'               => '#CD661C',   #
        'chocolate4'               => '#8A4512',   #
        'coolcopper'               => '#D98618',   #
        'coral'                    => '#FF7F4F',   #
        'coral1'                   => '#FF7255',   #
        'coral2'                   => '#ED6A4F',   #
        'coral3'                   => '#CD5A45',   #
        'coral4'                   => '#8A3E2E',   #
        'cornflowerblue'           => '#6394EC',   #
        'cornsilk'                 => '#FFF7DC',   #
        'cornsilk1'                => '#FFF7DC',   #
        'cornsilk2'                => '#EDE7CD',   #
        'cornsilk3'                => '#CDC7B1',   #
        'cornsilk4'                => '#8A8778',   #
        'crimson'                  => '#DC143C',   #
        'cyan'                     => '#00FFFF',   #
        'cyan1'                    => '#00FFFF',   #
        'cyan2'                    => '#00EDED',   #
        'cyan3'                    => '#00CDCD',   #
        'cyan4'                    => '#008A8A',   #
        'darkblue'                 => '#00008A',   #
        'darkcyan'                 => '#008A8A',   #
        'darkgoldenrod'            => '#B7850B',   #
        'darkgoldenrod1'           => '#FFB80E',   #
        'darkgoldenrod2'           => '#EDAD0D',   #
        'darkgoldenrod3'           => '#CD940C',   #
        'darkgoldenrod4'           => '#8A6507',   #
        'darkgray'                 => '#A9A9A9',   #
        'darkgreen'                => '#006300',   #
        'darkgrey'                 => '#A9A9A9',   #
        'darkkhaki'                => '#BCB66B',   #
        'darkmagenta'              => '#8A008A',   #
        'darkolivegreen'           => '#546B2E',   #
        'darkolivegreen1'          => '#CAFF70',   #
        'darkolivegreen2'          => '#BBED68',   #
        'darkolivegreen3'          => '#A2CD59',   #
        'darkolivegreen4'          => '#6E8A3D',   #
        'darkorange'               => '#FF8B00',   #
        'darkorange1'              => '#FF7F00',   #
        'darkorange2'              => '#ED7600',   #
        'darkorange3'              => '#CD6600',   #
        'darkorange4'              => '#8A4500',   #
        'darkorchid'               => '#9931CC',   #
        'darkorchid1'              => '#BE3EFF',   #
        'darkorchid2'              => '#B13AED',   #
        'darkorchid3'              => '#9A31CD',   #
        'darkorchid4'              => '#68218A',   #
        'darkred'                  => '#8A0000',   #
        'darksalmon'               => '#E8957A',   #
        'darkseagreen'             => '#8EBB8E',   #
        'darkseagreen1'            => '#C0FFC0',   #
        'darkseagreen2'            => '#B4EDB4',   #
        'darkseagreen3'            => '#9BCD9B',   #
        'darkseagreen4'            => '#698A69',   #
        'darkslateblue'            => '#483D8A',   #
        'darkslategray'            => '#2E4E4E',   #
        'darkslategray1'           => '#97FFFF',   #
        'darkslategray2'           => '#8CEDED',   #
        'darkslategray3'           => '#79CDCD',   #
        'darkslategray4'           => '#518A8A',   #
        'darkslategrey'            => '#2E4E4E',   #
        'darkturquoise'            => '#00CED1',   #
        'darkviolet'               => '#9300D3',   #
        'darkwood'                 => '#845D42',   #
        'deeppink'                 => '#FF1492',   #
        'deeppink1'                => '#FF1492',   #
        'deeppink2'                => '#ED1188',   #
        'deeppink3'                => '#CD1076',   #
        'deeppink4'                => '#8A0A4F',   #
        'deepskyblue'              => '#00BEFF',   #
        'deepskyblue1'             => '#00BEFF',   #
        'deepskyblue2'             => '#00B1ED',   #
        'deepskyblue3'             => '#009ACD',   #
        'deepskyblue4'             => '#00688A',   #
        'dimgray'                  => '#696969',   #
        'dimgrey'                  => '#696969',   #
        'dodgerblue'               => '#1D8FFF',   #
        'dodgerblue1'              => '#1D8FFF',   #
        'dodgerblue2'              => '#1B85ED',   #
        'dodgerblue3'              => '#1774CD',   #
        'dodgerblue4'              => '#104D8A',   #
        'dustyrose'                => '#846262',   #
        'feldspar'                 => '#D19175',   #
        'firebrick'                => '#B12121',   #
        'firebrick1'               => '#FF2F2F',   #
        'firebrick2'               => '#ED2B2B',   #
        'firebrick3'               => '#CD2525',   #
        'firebrick4'               => '#8A1919',   #
        'flesh'                    => '#F4CCB0',   #
        'floralwhite'              => '#FFF9EF',   #
        'forestgreen'              => '#218A21',   #
        'fuchsia'                  => '#FF00FF',   #
        'gainsboro'                => '#DCDCDC',   #
        'ghostwhite'               => '#F7F7FF',   #
        'gold'                     => '#FFD700',   #
        'gold1'                    => '#FFD700',   #
        'gold2'                    => '#EDC900',   #
        'gold3'                    => '#CDAD00',   #
        'gold4'                    => '#8A7500',   #
        'goldenrod'                => '#DAA51F',   #
        'goldenrod1'               => '#FFC024',   #
        'goldenrod2'               => '#EDB421',   #
        'goldenrod3'               => '#CD9B1C',   #
        'goldenrod4'               => '#8A6914',   #
        'gray'                     => '#7F7F7F',   #
        'gray0'                    => '#000000',   #
        'gray1'                    => '#020202',   #
        'gray10'                   => '#191919',   #
        'gray100'                  => '#FFFFFF',   #
        'gray11'                   => '#1B1B1B',   #
        'gray12'                   => '#1E1E1E',   #
        'gray13'                   => '#202020',   #
        'gray14'                   => '#232323',   #
        'gray15'                   => '#252525',   #
        'gray16'                   => '#282828',   #
        'gray17'                   => '#2A2A2A',   #
        'gray18'                   => '#2D2D2D',   #
        'gray19'                   => '#2F2F2F',   #
        'gray2'                    => '#050505',   #
        'gray20'                   => '#333333',   #
        'gray21'                   => '#363636',   #
        'gray22'                   => '#383838',   #
        'gray23'                   => '#3B3B3B',   #
        'gray24'                   => '#3D3D3D',   #
        'gray25'                   => '#404040',   #
        'gray26'                   => '#424242',   #
        'gray27'                   => '#454545',   #
        'gray28'                   => '#474747',   #
        'gray29'                   => '#4A4A4A',   #
        'gray3'                    => '#070707',   #
        'gray30'                   => '#4C4C4C',   #
        'gray31'                   => '#4E4E4E',   #
        'gray32'                   => '#515151',   #
        'gray33'                   => '#535353',   #
        'gray34'                   => '#565656',   #
        'gray35'                   => '#585858',   #
        'gray36'                   => '#5B5B5B',   #
        'gray37'                   => '#5D5D5D',   #
        'gray38'                   => '#606060',   #
        'gray39'                   => '#626262',   #
        'gray4'                    => '#0A0A0A',   #
        'gray40'                   => '#666666',   #
        'gray41'                   => '#696969',   #
        'gray42'                   => '#6B6B6B',   #
        'gray43'                   => '#6E6E6E',   #
        'gray44'                   => '#707070',   #
        'gray45'                   => '#737373',   #
        'gray46'                   => '#757575',   #
        'gray47'                   => '#787878',   #
        'gray48'                   => '#7A7A7A',   #
        'gray49'                   => '#7D7D7D',   #
        'gray5'                    => '#0C0C0C',   #
        'gray50'                   => '#7F7F7F',   #
        'gray51'                   => '#818181',   #
        'gray52'                   => '#848484',   #
        'gray53'                   => '#868686',   #
        'gray54'                   => '#898989',   #
        'gray55'                   => '#8B8B8B',   #
        'gray56'                   => '#8E8E8E',   #
        'gray57'                   => '#909090',   #
        'gray58'                   => '#939393',   #
        'gray59'                   => '#959595',   #
        'gray6'                    => '#0E0E0E',   #
        'gray60'                   => '#999999',   #
        'gray61'                   => '#9C9C9C',   #
        'gray62'                   => '#9E9E9E',   #
        'gray63'                   => '#A1A1A1',   #
        'gray64'                   => '#A3A3A3',   #
        'gray65'                   => '#A6A6A6',   #
        'gray66'                   => '#A8A8A8',   #
        'gray67'                   => '#ABABAB',   #
        'gray68'                   => '#ADADAD',   #
        'gray69'                   => '#B0B0B0',   #
        'gray7'                    => '#111111',   #
        'gray70'                   => '#B2B2B2',   #
        'gray71'                   => '#B4B4B4',   #
        'gray72'                   => '#B7B7B7',   #
        'gray73'                   => '#B9B9B9',   #
        'gray74'                   => '#BCBCBC',   #
        'gray75'                   => '#BEBEBE',   #
        'gray76'                   => '#C1C1C1',   #
        'gray77'                   => '#C3C3C3',   #
        'gray78'                   => '#C6C6C6',   #
        'gray79'                   => '#C9C9C9',   #
        'gray8'                    => '#141414',   #
        'gray80'                   => '#CCCCCC',   #
        'gray81'                   => '#CFCFCF',   #
        'gray82'                   => '#D1D1D1',   #
        'gray83'                   => '#D4D4D4',   #
        'gray84'                   => '#D6D6D6',   #
        'gray85'                   => '#D9D9D9',   #
        'gray86'                   => '#DBDBDB',   #
        'gray87'                   => '#DEDEDE',   #
        'gray88'                   => '#E0E0E0',   #
        'gray89'                   => '#E2E2E2',   #
        'gray9'                    => '#161616',   #
        'gray90'                   => '#E5E5E5',   #
        'gray91'                   => '#E7E7E7',   #
        'gray92'                   => '#EAEAEA',   #
        'gray93'                   => '#ECECEC',   #
        'gray94'                   => '#EFEFEF',   #
        'gray95'                   => '#F1F1F1',   #
        'gray96'                   => '#F4F4F4',   #
        'gray97'                   => '#F6F6F6',   #
        'gray98'                   => '#F9F9F9',   #
        'gray99'                   => '#FBFBFB',   #
        'green'                    => '#007F00',   #
        'green1'                   => '#00FF00',   #
        'green2'                   => '#00ED00',   #
        'green3'                   => '#00CD00',   #
        'green4'                   => '#008A00',   #
        'greencopper'              => '#846262',   #
        'greenyellow'              => '#D19175',   #
        'grey'                     => '#BDBDBD',   #
        'grey0'                    => '#000000',   #
        'grey1'                    => '#020202',   #
        'grey10'                   => '#191919',   #
        'grey100'                  => '#FFFFFF',   #
        'grey11'                   => '#1B1B1B',   #
        'grey12'                   => '#1E1E1E',   #
        'grey13'                   => '#202020',   #
        'grey14'                   => '#232323',   #
        'grey15'                   => '#252525',   #
        'grey16'                   => '#282828',   #
        'grey17'                   => '#2A2A2A',   #
        'grey18'                   => '#2D2D2D',   #
        'grey19'                   => '#2F2F2F',   #
        'grey2'                    => '#050505',   #
        'grey20'                   => '#333333',   #
        'grey21'                   => '#363636',   #
        'grey22'                   => '#383838',   #
        'grey23'                   => '#3B3B3B',   #
        'grey24'                   => '#3D3D3D',   #
        'grey25'                   => '#404040',   #
        'grey26'                   => '#424242',   #
        'grey27'                   => '#454545',   #
        'grey28'                   => '#474747',   #
        'grey29'                   => '#4A4A4A',   #
        'grey3'                    => '#070707',   #
        'grey30'                   => '#4C4C4C',   #
        'grey31'                   => '#4E4E4E',   #
        'grey32'                   => '#515151',   #
        'grey33'                   => '#535353',   #
        'grey34'                   => '#565656',   #
        'grey35'                   => '#585858',   #
        'grey36'                   => '#5B5B5B',   #
        'grey37'                   => '#5D5D5D',   #
        'grey38'                   => '#606060',   #
        'grey39'                   => '#626262',   #
        'grey4'                    => '#0A0A0A',   #
        'grey40'                   => '#666666',   #
        'grey41'                   => '#696969',   #
        'grey42'                   => '#6B6B6B',   #
        'grey43'                   => '#6E6E6E',   #
        'grey44'                   => '#707070',   #
        'grey45'                   => '#737373',   #
        'grey46'                   => '#757575',   #
        'grey47'                   => '#787878',   #
        'grey48'                   => '#7A7A7A',   #
        'grey49'                   => '#7D7D7D',   #
        'grey5'                    => '#0C0C0C',   #
        'grey50'                   => '#7F7F7F',   #
        'grey51'                   => '#818181',   #
        'grey52'                   => '#848484',   #
        'grey53'                   => '#868686',   #
        'grey54'                   => '#898989',   #
        'grey55'                   => '#8B8B8B',   #
        'grey56'                   => '#8E8E8E',   #
        'grey57'                   => '#909090',   #
        'grey58'                   => '#939393',   #
        'grey59'                   => '#959595',   #
        'grey6'                    => '#0E0E0E',   #
        'grey60'                   => '#999999',   #
        'grey61'                   => '#9C9C9C',   #
        'grey62'                   => '#9E9E9E',   #
        'grey63'                   => '#A1A1A1',   #
        'grey64'                   => '#A3A3A3',   #
        'grey65'                   => '#A6A6A6',   #
        'grey66'                   => '#A8A8A8',   #
        'grey67'                   => '#ABABAB',   #
        'grey68'                   => '#ADADAD',   #
        'grey69'                   => '#B0B0B0',   #
        'grey7'                    => '#111111',   #
        'grey70'                   => '#B2B2B2',   #
        'grey71'                   => '#B4B4B4',   #
        'grey72'                   => '#B7B7B7',   #
        'grey73'                   => '#B9B9B9',   #
        'grey74'                   => '#BCBCBC',   #
        'grey75'                   => '#BEBEBE',   #
        'grey76'                   => '#C1C1C1',   #
        'grey77'                   => '#C3C3C3',   #
        'grey78'                   => '#C6C6C6',   #
        'grey79'                   => '#C9C9C9',   #
        'grey8'                    => '#141414',   #
        'grey80'                   => '#CCCCCC',   #
        'grey81'                   => '#CFCFCF',   #
        'grey82'                   => '#D1D1D1',   #
        'grey83'                   => '#D4D4D4',   #
        'grey84'                   => '#D6D6D6',   #
        'grey85'                   => '#D9D9D9',   #
        'grey86'                   => '#DBDBDB',   #
        'grey87'                   => '#DEDEDE',   #
        'grey88'                   => '#E0E0E0',   #
        'grey89'                   => '#E2E2E2',   #
        'grey9'                    => '#161616',   #
        'grey90'                   => '#E5E5E5',   #
        'grey91'                   => '#E7E7E7',   #
        'grey92'                   => '#EAEAEA',   #
        'grey93'                   => '#ECECEC',   #
        'grey94'                   => '#EFEFEF',   #
        'grey95'                   => '#F1F1F1',   #
        'grey96'                   => '#F4F4F4',   #
        'grey97'                   => '#F6F6F6',   #
        'grey98'                   => '#F9F9F9',   #
        'grey99'                   => '#FBFBFB',   #
        'honeydew'                 => '#EFFFEF',   #
        'honeydew1'                => '#EFFFEF',   #
        'honeydew2'                => '#E0EDE0',   #
        'honeydew3'                => '#C0CDC0',   #
        'honeydew4'                => '#828A82',   #
        'hotpink'                  => '#FF69B4',   #
        'hotpink1'                 => '#FF6EB4',   #
        'hotpink2'                 => '#ED6AA7',   #
        'hotpink3'                 => '#CD5F8F',   #
        'hotpink4'                 => '#8A3A61',   #
        'indianred'                => '#F4CCB0',   #
        'indianred1'               => '#FF6A6A',   #
        'indianred2'               => '#ED6262',   #
        'indianred3'               => '#CD5454',   #
        'indianred4'               => '#8A3A3A',   #
        'indigo'                   => '#4B0081',   #
        'ivory'                    => '#FFFFEF',   #
        'ivory1'                   => '#FFFFEF',   #
        'ivory2'                   => '#EDEDE0',   #
        'ivory3'                   => '#CDCDC0',   #
        'ivory4'                   => '#8A8A82',   #
        'khaki'                    => '#EFE68B',   #
        'khaki1'                   => '#FFF58E',   #
        'khaki2'                   => '#EDE684',   #
        'khaki3'                   => '#CDC573',   #
        'khaki4'                   => '#8A854D',   #
        'lavender'                 => '#E6E6F9',   #
        'lavenderblush'            => '#FFEFF4',   #
        'lavenderblush1'           => '#FFEFF4',   #
        'lavenderblush2'           => '#EDE0E5',   #
        'lavenderblush3'           => '#CDC0C4',   #
        'lavenderblush4'           => '#8A8285',   #
        'lawngreen'                => '#7CFB00',   #
        'lemonchiffon'             => '#FFF9CD',   #
        'lemonchiffon1'            => '#FFF9CD',   #
        'lemonchiffon2'            => '#EDE8BE',   #
        'lemonchiffon3'            => '#CDC9A5',   #
        'lemonchiffon4'            => '#8A8870',   #
        'lightblue'                => '#ADD8E6',   #
        'lightblue1'               => '#BEEEFF',   #
        'lightblue2'               => '#B1DFED',   #
        'lightblue3'               => '#9ABFCD',   #
        'lightblue4'               => '#68828A',   #
        'lightcoral'               => '#EF7F7F',   #
        'lightcyan'                => '#E0FFFF',   #
        'lightcyan1'               => '#E0FFFF',   #
        'lightcyan2'               => '#D1EDED',   #
        'lightcyan3'               => '#B4CDCD',   #
        'lightcyan4'               => '#7A8A8A',   #
        'lightgoldenrod'           => '#EDDD81',   #
        'lightgoldenrod1'          => '#FFEB8A',   #
        'lightgoldenrod2'          => '#EDDC81',   #
        'lightgoldenrod3'          => '#CDBD70',   #
        'lightgoldenrod4'          => '#8A804C',   #
        'lightgoldenrodyellow'     => '#F9F9D2',   #
        'lightgray'                => '#D3D3D3',   #
        'lightgreen'               => '#8FED8F',   #
        'lightgrey'                => '#D3D3D3',   #
        'lightpink'                => '#FFB5C0',   #
        'lightpink1'               => '#FFAEB8',   #
        'lightpink2'               => '#EDA2AD',   #
        'lightpink3'               => '#CD8B94',   #
        'lightpink4'               => '#8A5E65',   #
        'lightsalmon'              => '#FFA07A',   #
        'lightsalmon1'             => '#FFA07A',   #
        'lightsalmon2'             => '#ED9472',   #
        'lightsalmon3'             => '#CD8061',   #
        'lightsalmon4'             => '#8A5642',   #
        'lightseagreen'            => '#1FB1AA',   #
        'lightskyblue'             => '#86CEF9',   #
        'lightskyblue1'            => '#B0E2FF',   #
        'lightskyblue2'            => '#A4D3ED',   #
        'lightskyblue3'            => '#8CB5CD',   #
        'lightskyblue4'            => '#5F7B8A',   #
        'lightslateblue'           => '#8370FF',   #
        'lightslategray'           => '#778799',   #
        'lightslategrey'           => '#778799',   #
        'lightsteelblue'           => '#B0C3DE',   #
        'lightsteelblue1'          => '#CAE1FF',   #
        'lightsteelblue2'          => '#BBD2ED',   #
        'lightsteelblue3'          => '#A2B4CD',   #
        'lightsteelblue4'          => '#6E7B8A',   #
        'lightyellow'              => '#FFFFE0',   #
        'lightyellow1'             => '#FFFFE0',   #
        'lightyellow2'             => '#EDEDD1',   #
        'lightyellow3'             => '#CDCDB4',   #
        'lightyellow4'             => '#8A8A7A',   #
        'lime'                     => '#00FF00',   #
        'limegreen'                => '#31CD31',   #
        'linen'                    => '#F9EFE6',   #
        'magenta'                  => '#FF00FF',   #
        'magenta1'                 => '#FF00FF',   #
        'magenta2'                 => '#ED00ED',   #
        'magenta3'                 => '#CD00CD',   #
        'magenta4'                 => '#8A008A',   #
        'mandarianorange'          => '#8D2222',   #
        'maroon'                   => '#7F0000',   #
        'maroon1'                  => '#FF34B2',   #
        'maroon2'                  => '#ED2FA7',   #
        'maroon3'                  => '#CD288F',   #
        'maroon4'                  => '#8A1B61',   #
        'mediumaquamarine'         => '#66CDAA',   #
        'mediumblue'               => '#0000CD',   #
        'mediumorchid'             => '#B954D3',   #
        'mediumorchid1'            => '#E066FF',   #
        'mediumorchid2'            => '#D15EED',   #
        'mediumorchid3'            => '#B451CD',   #
        'mediumorchid4'            => '#7A378A',   #
        'mediumpurple'             => '#9270DB',   #
        'mediumpurple1'            => '#AB81FF',   #
        'mediumpurple2'            => '#9F79ED',   #
        'mediumpurple3'            => '#8868CD',   #
        'mediumpurple4'            => '#5C478A',   #
        'mediumseagreen'           => '#3CB271',   #
        'mediumslateblue'          => '#7B68ED',   #
        'mediumspringgreen'        => '#00F99A',   #
        'mediumturquoise'          => '#48D1CC',   #
        'mediumvioletred'          => '#C61584',   #
        'midnightblue'             => '#2E2E4E',   #
        'mintcream'                => '#F4FFF9',   #
        'mistyrose'                => '#FFE4E1',   #
        'mistyrose1'               => '#FFE4E1',   #
        'mistyrose2'               => '#EDD5D2',   #
        'mistyrose3'               => '#CDB6B4',   #
        'mistyrose4'               => '#8A7D7B',   #
        'moccasin'                 => '#FFE4B4',   #
        'navajowhite'              => '#FFDEAD',   #
        'navajowhite1'             => '#FFDEAD',   #
        'navajowhite2'             => '#EDCFA1',   #
        'navajowhite3'             => '#CDB28A',   #
        'navajowhite4'             => '#8A795D',   #
        'navy'                     => '#00007F',   #
        'navyblue'                 => '#00007F',   #
        'neonblue'                 => '#4C4CFF',   #
        'neonpink'                 => '#FF6EC6',   #
        'none'                     => '#000000',   #
        'oldlace'                  => '#FCF4E6',   #
        'olive'                    => '#7F7F00',   #
        'olivedrab'                => '#6B8D22',   #
        'olivedrab1'               => '#BFFF3E',   #
        'olivedrab2'               => '#B2ED3A',   #
        'olivedrab3'               => '#9ACD31',   #
        'olivedrab4'               => '#698A21',   #
        'orange'                   => '#FFA500',   #
        'orange1'                  => '#FFA500',   #
        'orange2'                  => '#ED9A00',   #
        'orange3'                  => '#CD8400',   #
        'orange4'                  => '#8A5900',   #
        'orangered'                => '#FF4500',   #
        'orangered1'               => '#FF4500',   #
        'orangered2'               => '#ED4000',   #
        'orangered3'               => '#CD3700',   #
        'orangered4'               => '#8A2400',   #
        'orchid'                   => '#DA70D6',   #
        'orchid1'                  => '#FF82F9',   #
        'orchid2'                  => '#ED7AE8',   #
        'orchid3'                  => '#CD69C9',   #
        'orchid4'                  => '#8A4788',   #
        'palegoldenrod'            => '#EDE7AA',   #
        'palegreen'                => '#98FB98',   #
        'palegreen1'               => '#9AFF9A',   #
        'palegreen2'               => '#8FED8F',   #
        'palegreen3'               => '#7CCD7C',   #
        'palegreen4'               => '#538A53',   #
        'paleturquoise'            => '#AFEDED',   #
        'paleturquoise1'           => '#BAFFFF',   #
        'paleturquoise2'           => '#AEEDED',   #
        'paleturquoise3'           => '#95CDCD',   #
        'paleturquoise4'           => '#668A8A',   #
        'palevioletred'            => '#DB7092',   #
        'palevioletred1'           => '#FF81AB',   #
        'palevioletred2'           => '#ED799F',   #
        'palevioletred3'           => '#CD6888',   #
        'palevioletred4'           => '#8A475C',   #
        'papayawhip'               => '#FFEED5',   #
        'peachpuff'                => '#FFDAB8',   #
        'peachpuff1'               => '#FFDAB8',   #
        'peachpuff2'               => '#EDCBAD',   #
        'peachpuff3'               => '#CDAF94',   #
        'peachpuff4'               => '#8A7765',   #
        'peru'                     => '#CD843F',   #
        'pink'                     => '#FFBFCB',   #
        'pink1'                    => '#FFB4C4',   #
        'pink2'                    => '#EDA9B7',   #
        'pink3'                    => '#CD909E',   #
        'pink4'                    => '#8A626C',   #
        'plum'                     => '#DDA0DD',   #
        'plum1'                    => '#FFBAFF',   #
        'plum2'                    => '#EDAEED',   #
        'plum3'                    => '#CD95CD',   #
        'plum4'                    => '#8A668A',   #
        'powderblue'               => '#B0E0E6',   #
        'purple'                   => '#7F007F',   #
        'purple1'                  => '#9B2FFF',   #
        'purple2'                  => '#902BED',   #
        'purple3'                  => '#7D25CD',   #
        'purple4'                  => '#54198A',   #
        'quartz'                   => '#D9D9F2',   #
        'red'                      => '#FF0000',   #
        'red1'                     => '#FF0000',   #
        'red2'                     => '#ED0000',   #
        'red3'                     => '#CD0000',   #
        'red4'                     => '#8A0000',   #
        'richblue'                 => '#5858AB',   #
        'rosybrown'                => '#BB8E8E',   #
        'rosybrown1'               => '#FFC0C0',   #
        'rosybrown2'               => '#EDB4B4',   #
        'rosybrown3'               => '#CD9B9B',   #
        'rosybrown4'               => '#8A6969',   #
        'royalblue'                => '#4169E1',   #
        'royalblue1'               => '#4876FF',   #
        'royalblue2'               => '#436EED',   #
        'royalblue3'               => '#3A5ECD',   #
        'royalblue4'               => '#26408A',   #
        'saddlebrown'              => '#8A4512',   #
        'salmon'                   => '#F97F72',   #
        'salmon1'                  => '#FF8B69',   #
        'salmon2'                  => '#ED8161',   #
        'salmon3'                  => '#CD7053',   #
        'salmon4'                  => '#8A4C39',   #
        'sandybrown'               => '#F3A45F',   #
        'seagreen'                 => '#2D8A56',   #
        'seagreen1'                => '#53FF9F',   #
        'seagreen2'                => '#4DED93',   #
        'seagreen3'                => '#43CD7F',   #
        'seagreen4'                => '#2D8A56',   #
        'seashell'                 => '#FFF4ED',   #
        'seashell1'                => '#FFF4ED',   #
        'seashell2'                => '#EDE5DE',   #
        'seashell3'                => '#CDC4BE',   #
        'seashell4'                => '#8A8581',   #
        'sienna'                   => '#A0512C',   #
        'sienna1'                  => '#FF8147',   #
        'sienna2'                  => '#ED7942',   #
        'sienna3'                  => '#CD6839',   #
        'sienna4'                  => '#8A4725',   #
        'silver'                   => '#BFBFBF',   #
        'skyblue'                  => '#86CEEA',   #
        'skyblue1'                 => '#86CEFF',   #
        'skyblue2'                 => '#7EBFED',   #
        'skyblue3'                 => '#6CA6CD',   #
        'skyblue4'                 => '#4A708A',   #
        'slateblue'                => '#6A59CD',   #
        'slateblue1'               => '#826FFF',   #
        'slateblue2'               => '#7A67ED',   #
        'slateblue3'               => '#6958CD',   #
        'slateblue4'               => '#473C8A',   #
        'slategray'                => '#707F8F',   #
        'slategray1'               => '#C5E2FF',   #
        'slategray2'               => '#B8D3ED',   #
        'slategray3'               => '#9FB5CD',   #
        'slategray4'               => '#6C7B8A',   #
        'slategrey'                => '#707F8F',   #
        'snow'                     => '#FFF9F9',   #
        'snow1'                    => '#FFF9F9',   #
        'snow2'                    => '#EDE8E8',   #
        'snow3'                    => '#CDC9C9',   #
        'snow4'                    => '#8A8888',   #
        'springgreen'              => '#00FF7F',   #
        'springgreen1'             => '#00FF7F',   #
        'springgreen2'             => '#00ED76',   #
        'springgreen3'             => '#00CD66',   #
        'springgreen4'             => '#008A45',   #
        'steelblue'                => '#4681B4',   #
        'steelblue1'               => '#62B7FF',   #
        'steelblue2'               => '#5BACED',   #
        'steelblue3'               => '#4E93CD',   #
        'steelblue4'               => '#36638A',   #
        'summersky'                => '#38B0DE',   #
        'tan'                      => '#D2B48B',   #
        'tan1'                     => '#FFA54E',   #
        'tan2'                     => '#ED9A49',   #
        'tan3'                     => '#CD843F',   #
        'tan4'                     => '#8A592A',   #
        'teal'                     => '#007F7F',   #
        'thistle'                  => '#D8BED8',   #
        'thistle1'                 => '#FFE1FF',   #
        'thistle2'                 => '#EDD2ED',   #
        'thistle3'                 => '#CDB4CD',   #
        'thistle4'                 => '#8A7B8A',   #
        'tomato'                   => '#FF6247',   #
        'tomato1'                  => '#FF6247',   #
        'tomato2'                  => '#ED5B42',   #
        'tomato3'                  => '#CD4E39',   #
        'tomato4'                  => '#8A3625',   #
        'turquoise'                => '#40E0D0',   #
        'turquoise1'               => '#00F4FF',   #
        'turquoise2'               => '#00E5ED',   #
        'turquoise3'               => '#00C4CD',   #
        'turquoise4'               => '#00858A',   #
        'violet'                   => '#ED81ED',   #
        'violetred'                => '#D01F8F',   #
        'violetred1'               => '#FF3E95',   #
        'violetred2'               => '#ED3A8B',   #
        'violetred3'               => '#CD3178',   #
        'violetred4'               => '#8A2151',   #
        'wheat'                    => '#F4DEB2',   #
        'wheat1'                   => '#FFE6B9',   #
        'wheat2'                   => '#EDD8AE',   #
        'wheat3'                   => '#CDB995',   #
        'wheat4'                   => '#8A7E66',   #
        'white'                    => '#FFFFFF',   #
        'whitesmoke'               => '#F4F4F4',   #
        'yellow'                   => '#FFFF00',   #
        'yellow1'                  => '#FFFF00',   #
        'yellow2'                  => '#EDED00',   #
        'yellow3'                  => '#CDCD00',   #
        'yellow4'                  => '#8A8A00',   #
        'yellowgreen'              => '#99CC31',   #
    );
}

sub pdfkey {
    return($PDF::API2::Util::key_var++);
}

sub pdfkey2 {
    return($PDF::API2::Util::key_var.($PDF::API2::Util::key_var2++));
}

sub digestx {
    my $len=shift @_;
    my $mask=$len-1;
    my $ddata=join('',@_);
    my $mdkey='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789gT';
    my $xdata="0" x $len;
    my $off=0;
    my $set;
    foreach $set (0..(length($ddata)<<1)) {
        $off+=vec($ddata,$set,4);
        $off+=vec($xdata,($set & $mask),8);
        vec($xdata,($set & ($mask<<1 |1)),4)=vec($mdkey,($off & 0x7f),4);
    }

#   foreach $set (0..$mask) {
#       vec($xdata,$set,8)=(vec($xdata,$set,8) & 0x7f) | 0x40;
#   }

#   $off=0;
#   foreach $set (0..$mask) {
#       $off+=vec($xdata,$set,8);
#       vec($xdata,$set,8)=vec($mdkey,($off & 0x3f),8);
#   }

    return($xdata);
}

sub digest {
    return(digestx(32,@_));
}

sub digest16 {
    return(digestx(16,@_));
}

sub digest32 {
    return(digestx(32,@_));
}

sub xlog10 {
    my $n = shift;
    if($n) {
            return log(abs($n))/log(10);
    } else { return 0; }
}

sub float {
    my $f=shift @_;
    my $mxd=shift @_||4;
    $f=0 if(abs($f)<0.0000000000000001);
    my $ad=floor(xlog10($f)-$mxd);
    if(abs($f-int($f)) < (10**(-$mxd))) {
        # just in case we have an integer
        return sprintf('%i',$f);
    } elsif($ad>0){
        return sprintf('%f',$f);
    } else {
        return sprintf('%.'.abs($ad).'f',$f);
    }
}
sub floats { return map { float($_); } @_; }
sub floats5 { return map { float($_,5); } @_; }


sub intg {
    my $f=shift @_;
    return sprintf('%i',$f);
}
sub intgs { return map { intg($_); } @_; }

sub mMin {
    my $n=HUGE_VAL;
    map { $n=($n>$_) ? $_ : $n } @_;
    return($n);
}

sub mMax {
    my $n=-(HUGE_VAL);
    map { $n=($n<$_) ? $_ : $n } @_;
    return($n);
}

sub cRGB {
    my @cmy=(map { 1-$_ } @_);
    my $k=mMin(@cmy);
    return((map { $_-$k } @cmy),$k);
}

sub cRGB8 {
    return cRGB(map { $_/255 } @_);
}

sub RGBtoLUM {
    my ($r,$g,$b)=@_;
    return($r*0.299+$g*0.587+$b*0.114);
}

sub RGBasCMYK {
    my @rgb=@_;
    my @cmy=(map { 1-$_ } @rgb);
    my $k=mMin(@cmy)*0.44;
    return((map { $_-$k } @cmy),$k);
}

sub HSVtoRGB {
    my ($h,$s,$v)=@_;
    my ($r,$g,$b,$i,$f,$p,$q,$t);

    if( $s == 0 ) {
        ## achromatic (grey)
        return ($v,$v,$v);
    }

    $h %= 360;
    $h /= 60;       ## sector 0 to 5
    $i = POSIX::floor( $h );
    $f = $h - $i;   ## factorial part of h
    $p = $v * ( 1 - $s );
    $q = $v * ( 1 - $s * $f );
    $t = $v * ( 1 - $s * ( 1 - $f ) );

    if($i<1) {
        $r = $v;
        $g = $t;
        $b = $p;
    } elsif($i<2){
        $r = $q;
        $g = $v;
        $b = $p;
    } elsif($i<3){
        $r = $p;
        $g = $v;
        $b = $t;
    } elsif($i<4){
        $r = $p;
        $g = $q;
        $b = $v;
    } elsif($i<5){
        $r = $t;
        $g = $p;
        $b = $v;
    } else {
        $r = $v;
        $g = $p;
        $b = $q;
    }
    return ($r,$g,$b);
}
sub _HSVtoRGB { # test
    my ($h,$s,$v)=@_;
    my ($r,$g,$b,$i,$f,$p,$q,$t);

    if( $s == 0 ) {
        ## achromatic (grey)
        return ($v,$v,$v);
    }
    
    $h %= 360;
    
    $r = 2*cos(deg2rad($h));
    $g = 2*cos(deg2rad($h+120));
    $b = 2*cos(deg2rad($h+240));

    $p = max($r,$g,$b);
    $q = min($r,$g,$b);
    ($p,$q) = map { ($_<0 ? 0 : ($_>1 ? 1 : $_)) } ($p,$q);
    $f = $p - $q;
    
    #if($p>=$v) {
    #    ($r,$g,$b) = map { $_*$v/$p } ($r,$g,$b);
    #} else {
    #    ($r,$g,$b) = map { $_*$p/$v } ($r,$g,$b);
    #}
    #
    #if($f>=$s) {
    #    ($r,$g,$b) = map { (($_-$q/2)*$f/$s)+$q/2 } ($r,$g,$b);
    #} else {
    #    ($r,$g,$b) = map { (($_-$q/2)*$s/$f)+$q/2 } ($r,$g,$b);
    #}

    ($r,$g,$b) = map { ($_<0 ? 0 : ($_>1 ? 1 : $_)) } ($r,$g,$b);

    return ($r,$g,$b);
}

sub RGBquant ($$$) {
    my($q1,$q2,$h)=@_;
    while($h<0){$h+=360;}
    $h%=360;
    if ($h<60) {
        return($q1+(($q2-$q1)*$h/60));
    } elsif ($h<180) {
        return($q2);
    } elsif ($h<240) {
        return($q1+(($q2-$q1)*(240-$h)/60));
    } else {
        return($q1);
    }
}

sub RGBtoHSV {
    my ($r,$g,$b)=@_;
    my ($h,$s,$v,$min,$max,$delta);

    $min= mMin($r,$g,$b);
    $max= mMax($r,$g,$b);

    $v = $max;

    $delta = $max - $min;

    if( $delta > 0.000000001 ) {
        $s = $delta / $max;
    } else {
        $s = 0;
        $h = 0;
        return($h,$s,$v);
    }

    if( $r == $max ) {
        $h = ( $g - $b ) / $delta;
    } elsif( $g == $max ) {
        $h = 2 + ( $b - $r ) / $delta;
    } else {
        $h = 4 + ( $r - $g ) / $delta;
    }
    $h *= 60;
    if( $h < 0 ) {$h += 360;}
    return($h,$s,$v);
}

sub RGBtoHSL {
    my ($r,$g,$b)=@_;
    my ($h,$s,$v,$l,$min,$max,$delta);

    $min= mMin($r,$g,$b);
    $max= mMax($r,$g,$b);
    ($h,$s,$v)=RGBtoHSV($r,$g,$b);
    $l=($max+$min)/2.0;
        $delta = $max - $min;
    if($delta<0.00000000001){
        return(0,0,$l);
    } else {
        if($l<=0.5){
            $s=$delta/($max+$min);
        } else {
            $s=$delta/(2-$max-$min);
        }
    }
    return($h,$s,$l);
}

sub HSLtoRGB {
    my($h,$s,$l,$r,$g,$b,$p1,$p2)=@_;
    if($l<=0.5){
        $p2=$l*(1+$s);
    } else {
        $p2=$l+$s-($l*$s);
    }
    $p1=2*$l-$p2;
    if($s<0.0000000000001){
        $r=$l; $g=$l; $b=$l;
    } else {
        $r=RGBquant($p1,$p2,$h+120);
        $g=RGBquant($p1,$p2,$h);
        $b=RGBquant($p1,$p2,$h-120);
    }
    return($r,$g,$b);
}

sub optInvColor {
    my ($r,$g,$b) = @_;

    my $ab = (0.2*$r) + (0.7*$g) + (0.1*$b);

    if($ab > 0.45) {
        return(0,0,0);
    } else {
        return(1,1,1);
    }
}

sub defineColor {
    my ($name,$mx,$r,$g,$b)=@_;
    $colors{$name}||=[ map {$_/$mx} ($r,$g,$b) ];
    return($colors{$name});
}

sub rgbHexValues {
    my $name=lc(shift @_);
    my ($r,$g,$b);
    if(length($name)<5) {       # zb. #fa4,          #cf0
        $r=hex(substr($name,1,1))/0xf;
        $g=hex(substr($name,2,1))/0xf;
        $b=hex(substr($name,3,1))/0xf;
    } elsif(length($name)<8) {  # zb. #ffaa44,       #ccff00
        $r=hex(substr($name,1,2))/0xff;
        $g=hex(substr($name,3,2))/0xff;
        $b=hex(substr($name,5,2))/0xff;
    } elsif(length($name)<11) { # zb. #fffaaa444,    #cccfff000
        $r=hex(substr($name,1,3))/0xfff;
        $g=hex(substr($name,4,3))/0xfff;
        $b=hex(substr($name,7,3))/0xfff;
    } else {            # zb. #ffffaaaa4444, #ccccffff0000
        $r=hex(substr($name,1,4))/0xffff;
        $g=hex(substr($name,5,4))/0xffff;
        $b=hex(substr($name,9,4))/0xffff;
    }
    return($r,$g,$b);
}
sub cmykHexValues {
    my $name=lc(shift @_);
    my ($c,$m,$y,$k);
    if(length($name)<6) {       # zb. %cmyk
        $c=hex(substr($name,1,1))/0xf;
        $m=hex(substr($name,2,1))/0xf;
        $y=hex(substr($name,3,1))/0xf;
        $k=hex(substr($name,4,1))/0xf;
    } elsif(length($name)<10) { # zb. %ccmmyykk
        $c=hex(substr($name,1,2))/0xff;
        $m=hex(substr($name,3,2))/0xff;
        $y=hex(substr($name,5,2))/0xff;
        $k=hex(substr($name,7,2))/0xff;
    } elsif(length($name)<14) { # zb. %cccmmmyyykkk
        $c=hex(substr($name,1,3))/0xfff;
        $m=hex(substr($name,4,3))/0xfff;
        $y=hex(substr($name,7,3))/0xfff;
        $k=hex(substr($name,10,3))/0xfff;
    } else {            # zb. %ccccmmmmyyyykkkk
        $c=hex(substr($name,1,4))/0xffff;
        $m=hex(substr($name,5,4))/0xffff;
        $y=hex(substr($name,9,4))/0xffff;
        $k=hex(substr($name,13,4))/0xffff;
    }
    return($c,$m,$y,$k);
}
sub hsvHexValues {
    my $name=lc(shift @_);
    my ($h,$s,$v);
    if(length($name)<5) {
        $h=360*hex(substr($name,1,1))/0x10;
        $s=hex(substr($name,2,1))/0xf;
        $v=hex(substr($name,3,1))/0xf;
    } elsif(length($name)<8) {
        $h=360*hex(substr($name,1,2))/0x100;
        $s=hex(substr($name,3,2))/0xff;
        $v=hex(substr($name,5,2))/0xff;
    } elsif(length($name)<11) {
        $h=360*hex(substr($name,1,3))/0x1000;
        $s=hex(substr($name,4,3))/0xfff;
        $v=hex(substr($name,7,3))/0xfff;
    } else {
        $h=360*hex(substr($name,1,4))/0x10000;
        $s=hex(substr($name,5,4))/0xffff;
        $v=hex(substr($name,9,4))/0xffff;
    }
    return($h,$s,$v);
}
sub labHexValues {
    my $name=lc(shift @_);
    my ($l,$a,$b);
    if(length($name)<5) {
        $l=100*hex(substr($name,1,1))/0xf;
        $a=(200*hex(substr($name,2,1))/0xf)-100;
        $b=(200*hex(substr($name,3,1))/0xf)-100;
    } elsif(length($name)<8) {
        $l=100*hex(substr($name,1,2))/0xff;
        $a=(200*hex(substr($name,3,2))/0xff)-100;
        $b=(200*hex(substr($name,5,2))/0xff)-100;
    } elsif(length($name)<11) {
        $l=100*hex(substr($name,1,3))/0xfff;
        $a=(200*hex(substr($name,4,3))/0xfff)-100;
        $b=(200*hex(substr($name,7,3))/0xfff)-100;
    } else {
        $l=100*hex(substr($name,1,4))/0xffff;
        $a=(200*hex(substr($name,5,4))/0xffff)-100;
        $b=(200*hex(substr($name,9,4))/0xffff)-100;
    }
    return($l,$a,$b);
}

sub namecolor {
    my $name=shift @_;
    unless(ref $name) {
        $name=lc($name);
        $name=~s/[^\#!%\&\$a-z0-9]//go;
    }
    if($name=~/^[a-z]/) { # name spec.
        return(namecolor($colors{$name}));
    } elsif($name=~/^#/) { # rgb spec.
        return(floats5(rgbHexValues($name)));
    } elsif($name=~/^%/) { # cmyk spec.
        return(floats5(cmykHexValues($name)));
    } elsif($name=~/^!/) { # hsv spec.
        return(floats5(HSVtoRGB(hsvHexValues($name))));
    } elsif($name=~/^&/) { # hsl spec.
        return(floats5(HSLtoRGB(hsvHexValues($name))));
    } else { # or it is a ref ?
        return(floats5(@{$name || [0.5,0.5,0.5]}));
    }
}
sub namecolor_cmyk {
    my $name=shift @_;
    unless(ref $name) {
        $name=lc($name);
        $name=~s/[^\#!%\&\$a-z0-9]//go;
    }
    if($name=~/^[a-z]/) { # name spec.
        return(namecolor_cmyk($colors{$name}));
    } elsif($name=~/^#/) { # rgb spec.
        return(floats5(RGBasCMYK(rgbHexValues($name))));
    } elsif($name=~/^%/) { # cmyk spec.
        return(floats5(cmykHexValues($name)));
    } elsif($name=~/^!/) { # hsv spec.
        return(floats5(RGBasCMYK(HSVtoRGB(hsvHexValues($name)))));
    } elsif($name=~/^&/) { # hsl spec.
        return(floats5(RGBasCMYK(HSLtoRGB(hsvHexValues($name)))));
    } else { # or it is a ref ?
        return(floats5(RGBasCMYK(@{$name || [0.5,0.5,0.5]})));
    }
}
sub namecolor_lab {
    my $name=shift @_;
    unless(ref $name) {
        $name=lc($name);
        $name=~s/[^\#!%\&\$a-z0-9]//go;
    }
    if($name=~/^[a-z]/) { # name spec.
        return(namecolor_lab($colors{$name}));
    } elsif($name=~/^\$/) { # lab spec.
        return(floats5(labHexValues($name)));
    } elsif($name=~/^#/) { # rgb spec.
        my ($h,$s,$v)=RGBtoHSV(rgbHexValues($name));
        my $a=cos(deg2rad $h)*$s*100;
        my $b=sin(deg2rad $h)*$s*100;
        my $l=100*$v;
        return(floats5($l,$a,$b));
    } elsif($name=~/^!/) { # hsv spec.
        # fake conversion
        my ($h,$s,$v)=hsvHexValues($name);
        my $a=cos(deg2rad $h)*$s*100;
        my $b=sin(deg2rad $h)*$s*100;
        my $l=100*$v;
        return(floats5($l,$a,$b));
    } elsif($name=~/^&/) { # hsl spec.
        my ($h,$s,$v)=hsvHexValues($name);
        my $a=cos(deg2rad $h)*$s*100;
        my $b=sin(deg2rad $h)*$s*100;
        ($h,$s,$v)=RGBtoHSV(HSLtoRGB($h,$s,$v));
        my $l=100*$v;
        return(floats5($l,$a,$b));
    } else { # or it is a ref ?
        my ($h,$s,$v)=RGBtoHSV(@{$name || [0.5,0.5,0.5]});
        my $a=cos(deg2rad $h)*$s*100;
        my $b=sin(deg2rad $h)*$s*100;
        my $l=100*$v;
        return(floats5($l,$a,$b));
    }
}

sub unfilter 
{
    my ($filter,$stream)=@_;

    if(defined $filter) 
    {
        # we need to fix filter because it MAY be
        # an array BUT IT COULD BE only a name
        if(ref($filter)!~/Array$/) 
        {
               $filter = PDFArray($filter);
        }
        my @filts;
        my ($hasflate) = -1;
        my ($temp, $i, $temp1);

        @filts=(map { ("PDF::API2::Basic::PDF::".($_->val))->new } $filter->elementsof);

        foreach my $f (@filts) 
        {
            $stream = $f->infilt($stream, 1);
        }
    }
    return($stream);
}

sub dofilter {
    my ($filter,$stream)=@_;

    if((defined $filter) ) {
        # we need to fix filter because it MAY be
        # an array BUT IT COULD BE only a name
        if(ref($filter)!~/Array$/) {
               $filter = PDFArray($filter);
        }
        my @filts;
        my ($hasflate) = -1;
        my ($temp, $i, $temp1);

        @filts=(map { ("PDF::API2::Basic::PDF::".($_->val))->new } $filter->elementsof);

        foreach my $f (@filts) {
            $stream = $f->outfilt($stream, 1);
        }
    }
    return($stream);
}

sub nameByUni {
  my ($e)=@_;
  return($u2n{$e} || sprintf('uni%04X',$e));
}

sub uniByName {
  my ($e)=@_;
  if($e=~/^uni([0-9A-F]{4})$/) {
    return(hex($1));
  }
  return($n2u{$e} || undef);
}

sub initNameTable {
    %u2n=(); %u2n=%u2n_o;
    %n2u=(); %n2u=%n2u_o;
    $pua=0xE000;
    1;
}
sub defineName {
    my $name=shift @_;
    return($n2u{$name}) if(defined $n2u{$name});

    while(defined $u2n{$pua}) { $pua++; }

    $u2n{$pua}=$name;
    $n2u{$name}=$pua;

    return($pua);
}

sub page_size {
    my ($x1,$y1,$x2,$y2) = @_;
    if(defined $x2) {
        # full bbox
        return($x1,$y1,$x2,$y2);
    } elsif(defined $y1) {
        # half bbox
        return(0,0,$x1,$y1);
    } elsif(defined $PaperSizes{lc($x1)}) {
        # textual spec.
        return(0,0,@{$PaperSizes{lc($x1)}});
    } elsif($x1=~/^[\d\.]+$/) {
        # single quadratic
        return(0,0,$x1,$x1);
    } else {
        # pdf default.
        return(0,0,612,792);
    }
}

sub getPaperSizes 
{
    my %h=();
    foreach my $k (keys %PaperSizes)
    {
    $h{$k}=[@{$PaperSizes{$k}}];
    }
    return(%h);
}


1;


__END__

function xRGBhex_to_aRGBhex ( $hstring, $lightness = 1.0 ) {

    $color=hexdec($hstring);

    $r=(($color & 0xff0000) >> 16)/255;
    $g=(($color & 0xff00) >> 8)/255;
    $b=($color & 0xff)/255;

    $rgbmax=max($r,$g,$b);

    $rgbmin=min($r,$g,$b);

    $rgbavg=($r+$g+$b)/3.0;


    if($rgbmin==$rgbmax) {
        return $hstring;
    }

    if ( $r == $rgbmax ) {
        $h = ( $g - $b ) / ( $rgbmax - $rgbmin );
    } elseif ( $g == $rgbmax ) {
        $h = 2.0 + ( $b - $r ) / ( $rgbmax - $rgbmin );
    } elseif ( $b == $rgbmax ) {
        $h = 4.0 + ( $r - $g ) / ( $rgbmax - $rgbmin );
    }
    if ( $h >= 6.0 ) {
        $h-=6.0;
    } elseif ( $h < 0.0 ) {
        $h+=6.0;
    }
    $s = ( $rgbmax - $rgbmin ) / $rgbmax;
    $s = $s>0.8 ? $s : 0.8;
    $ab = (0.3*$r) + (0.5*$g) + (0.2*$b);
    $v=$lightness*(pow($ab,(1/3)));

    $i=floor($h);
    $f=$h-$i;
    $p=$v*(1.0-$s);
    $q=$v*(1.0-$s*$f);
    $t=$v*(1.0-$s+$s*$f);

    if ($i==0) {
        return sprintf("%02X%02X%02X",$v*255,$t*255,$p*255);
    } elseif ($i==1) {
        return sprintf("%02X%02X%02X",$q*255,$v*255,$p*255);
    } elseif ($i==2) {
        return sprintf("%02X%02X%02X",$p*255,$v*255,$t*255);
    } elseif ($i==3) {
        return sprintf("%02X%02X%02X",$p*255,$q*255,$v*255);
    } elseif ($i==4) {
        return sprintf("%02X%02X%02X",$t*255,$p*255,$v*255);
    } else {
        return sprintf("%02X%02X%02X",$v*255,$p*255,$q*255);
    }
}


function RGBhex_bwinv ( $hstring ) {
        $color=hexdec($hstring);

        $r=(($color & 0xff0000) >> 16)/255;
        $g=(($color & 0xff00) >> 8)/255;
        $b=($color & 0xff)/255;

    $ab = (0.2*$r) + (0.7*$g) + (0.1*$b);

    if($ab > 0.45) {
        return "000000";
    } else {
        return "FFFFFF";
    }
}

=head1 NAME

PDF::API2::Util - utility package for often use methods across the package.

=head1 PREDEFINED COLORS

=over 4

=item %sizes = getPaperSizes();

Will retrive the registered papersizes of PDF::API2.

    print Dumper(\%sizes);
    $VAR1={
        '4a'        =>  [ 4760  , 6716  ],
        '2a'        =>  [ 3368  , 4760  ],
        'a0'        =>  [ 2380  , 3368  ],
        'a1'        =>  [ 1684  , 2380  ],
        'a2'        =>  [ 1190  , 1684  ],
        'a3'        =>  [ 842   , 1190  ],
        'a4'        =>  [ 595   , 842   ],
        'a5'        =>  [ 421   , 595   ],
        'a6'        =>  [ 297   , 421   ],
        '4b'        =>  [ 5656  , 8000  ],
        '2b'        =>  [ 4000  , 5656  ],
        'b0'        =>  [ 2828  , 4000  ],
        'b1'        =>  [ 2000  , 2828  ],
        'b2'        =>  [ 1414  , 2000  ],
        'b3'        =>  [ 1000  , 1414  ],
        'b4'        =>  [ 707   , 1000  ],
        'b5'        =>  [ 500   , 707   ],
        'b6'        =>  [ 353   , 500   ],
        'letter'    =>  [ 612   , 792   ],
        'broadsheet'    =>  [ 1296  , 1584  ],
        'ledger'    =>  [ 1224  , 792   ],
        'tabloid'   =>  [ 792   , 1224  ],
        'legal'     =>  [ 612   , 1008  ],
        'executive' =>  [ 522   , 756   ],
        '36x36'     =>  [ 2592  , 2592  ],
    };

=back

=head1 PREDEFINED COLORS

aliceblue antiquewhite antiquewhite1 antiquewhite2 antiquewhite3 antiquewhite4 aqua aquamarine
aquamarine1 aquamarine2 aquamarine3 aquamarine4 azure azure1 azure2 azure3 azure4 beige bisque bisque1
bisque2 bisque3 bisque4 black blanchedalmond blue blue1 blue2 blue3 blue4 blueviolet brass brightgold
bronze bronzeii brown brown1 brown2 brown3 brown4 burlywood burlywood1 burlywood2 burlywood3 burlywood4
cadetblue cadetblue1 cadetblue2 cadetblue3 cadetblue4 chartreuse chartreuse1 chartreuse2 chartreuse3
chartreuse4 chocolate chocolate1 chocolate2 chocolate3 chocolate4 coolcopper coral coral1 coral2 coral3
coral4 cornflowerblue cornsilk cornsilk1 cornsilk2 cornsilk3 cornsilk4 crimson cyan cyan1 cyan2 cyan3
cyan4 darkblue darkcyan darkgoldenrod darkgoldenrod1 darkgoldenrod2 darkgoldenrod3 darkgoldenrod4
darkgray darkgreen darkgrey darkkhaki darkmagenta darkolivegreen darkolivegreen1 darkolivegreen2
darkolivegreen3 darkolivegreen4 darkorange darkorange1 darkorange2 darkorange3 darkorange4 darkorchid
darkorchid1 darkorchid2 darkorchid3 darkorchid4 darkred darksalmon darkseagreen darkseagreen1
darkseagreen2 darkseagreen3 darkseagreen4 darkslateblue darkslategray darkslategray1 darkslategray2
darkslategray3 darkslategray4 darkslategrey darkturquoise darkviolet darkwood deeppink deeppink1
deeppink2 deeppink3 deeppink4 deepskyblue deepskyblue1 deepskyblue2 deepskyblue3 deepskyblue4 dimgray
dimgrey dodgerblue dodgerblue1 dodgerblue2 dodgerblue3 dodgerblue4 dustyrose feldspar firebrick
firebrick1 firebrick2 firebrick3 firebrick4 flesh floralwhite forestgreen fuchsia gainsboro ghostwhite
gold gold1 gold2 gold3 gold4 goldenrod goldenrod1 goldenrod2 goldenrod3 goldenrod4 gray gray0 gray1
gray10 gray100 gray11 gray12 gray13 gray14 gray15 gray16 gray17 gray18 gray19 gray2 gray20 gray21
gray22 gray23 gray24 gray25 gray26 gray27 gray28 gray29 gray3 gray30 gray31 gray32 gray33 gray34 gray35
gray36 gray37 gray38 gray39 gray4 gray40 gray41 gray42 gray43 gray44 gray45 gray46 gray47 gray48 gray49
gray5 gray50 gray51 gray52 gray53 gray54 gray55 gray56 gray57 gray58 gray59 gray6 gray60 gray61 gray62
gray63 gray64 gray65 gray66 gray67 gray68 gray69 gray7 gray70 gray71 gray72 gray73 gray74 gray75 gray76
gray77 gray78 gray79 gray8 gray80 gray81 gray82 gray83 gray84 gray85 gray86 gray87 gray88 gray89 gray9
gray90 gray91 gray92 gray93 gray94 gray95 gray96 gray97 gray98 gray99 green green1 green2 green3 green4
greencopper greenyellow grey grey0 grey1 grey10 grey100 grey11 grey12 grey13 grey14 grey15 grey16
grey17 grey18 grey19 grey2 grey20 grey21 grey22 grey23 grey24 grey25 grey26 grey27 grey28 grey29 grey3
grey30 grey31 grey32 grey33 grey34 grey35 grey36 grey37 grey38 grey39 grey4 grey40 grey41 grey42 grey43
grey44 grey45 grey46 grey47 grey48 grey49 grey5 grey50 grey51 grey52 grey53 grey54 grey55 grey56 grey57
grey58 grey59 grey6 grey60 grey61 grey62 grey63 grey64 grey65 grey66 grey67 grey68 grey69 grey7 grey70
grey71 grey72 grey73 grey74 grey75 grey76 grey77 grey78 grey79 grey8 grey80 grey81 grey82 grey83 grey84
grey85 grey86 grey87 grey88 grey89 grey9 grey90 grey91 grey92 grey93 grey94 grey95 grey96 grey97 grey98
grey99 honeydew honeydew1 honeydew2 honeydew3 honeydew4 hotpink hotpink1 hotpink2 hotpink3 hotpink4
indianred indianred1 indianred2 indianred3 indianred4 indigo ivory ivory1 ivory2 ivory3 ivory4 khaki
khaki1 khaki2 khaki3 khaki4 lavender lavenderblush lavenderblush1 lavenderblush2 lavenderblush3
lavenderblush4 lawngreen lemonchiffon lemonchiffon1 lemonchiffon2 lemonchiffon3 lemonchiffon4 lightblue
lightblue1 lightblue2 lightblue3 lightblue4 lightcoral lightcyan lightcyan1 lightcyan2 lightcyan3
lightcyan4 lightgoldenrod lightgoldenrod1 lightgoldenrod2 lightgoldenrod3 lightgoldenrod4
lightgoldenrodyellow lightgray lightgreen lightgrey lightpink lightpink1 lightpink2 lightpink3
lightpink4 lightsalmon lightsalmon1 lightsalmon2 lightsalmon3 lightsalmon4 lightseagreen lightskyblue
lightskyblue1 lightskyblue2 lightskyblue3 lightskyblue4 lightslateblue lightslategray lightslategrey
lightsteelblue lightsteelblue1 lightsteelblue2 lightsteelblue3 lightsteelblue4 lightyellow lightyellow1
lightyellow2 lightyellow3 lightyellow4 lime limegreen linen magenta magenta1 magenta2 magenta3 magenta4
mandarianorange maroon maroon1 maroon2 maroon3 maroon4 mediumaquamarine mediumblue mediumorchid
mediumorchid1 mediumorchid2 mediumorchid3 mediumorchid4 mediumpurple mediumpurple1 mediumpurple2
mediumpurple3 mediumpurple4 mediumseagreen mediumslateblue mediumspringgreen mediumturquoise
mediumvioletred midnightblue mintcream mistyrose mistyrose1 mistyrose2 mistyrose3 mistyrose4 moccasin
navajowhite navajowhite1 navajowhite2 navajowhite3 navajowhite4 navy navyblue neonblue neonpink none
oldlace olive olivedrab olivedrab1 olivedrab2 olivedrab3 olivedrab4 orange orange1 orange2 orange3
orange4 orangered orangered1 orangered2 orangered3 orangered4 orchid orchid1 orchid2 orchid3 orchid4
palegoldenrod palegreen palegreen1 palegreen2 palegreen3 palegreen4 paleturquoise paleturquoise1
paleturquoise2 paleturquoise3 paleturquoise4 palevioletred palevioletred1 palevioletred2 palevioletred3
palevioletred4 papayawhip peachpuff peachpuff1 peachpuff2 peachpuff3 peachpuff4 peru pink pink1 pink2
pink3 pink4 plum plum1 plum2 plum3 plum4 powderblue purple purple1 purple2 purple3 purple4 quartz red
red1 red2 red3 red4 richblue rosybrown rosybrown1 rosybrown2 rosybrown3 rosybrown4 royalblue royalblue1
royalblue2 royalblue3 royalblue4 saddlebrown salmon salmon1 salmon2 salmon3 salmon4 sandybrown seagreen
seagreen1 seagreen2 seagreen3 seagreen4 seashell seashell1 seashell2 seashell3 seashell4 sienna sienna1
sienna2 sienna3 sienna4 silver skyblue skyblue1 skyblue2 skyblue3 skyblue4 slateblue slateblue1
slateblue2 slateblue3 slateblue4 slategray slategray1 slategray2 slategray3 slategray4 slategrey snow
snow1 snow2 snow3 snow4 springgreen springgreen1 springgreen2 springgreen3 springgreen4 steelblue
steelblue1 steelblue2 steelblue3 steelblue4 summersky tan tan1 tan2 tan3 tan4 teal thistle thistle1
thistle2 thistle3 thistle4 tomato tomato1 tomato2 tomato3 tomato4 turquoise turquoise1 turquoise2
turquoise3 turquoise4 violet violetred violetred1 violetred2 violetred3 violetred4 wheat wheat1 wheat2
wheat3 wheat4 white whitesmoke yellow yellow1 yellow2 yellow3 yellow4 yellowgreen

B<Please Note:> This is an amalgamation of the X11, SGML and (X)HTML specification sets.

=head1 PREDEFINED GLYPH-NAMES

a a1 a10 a100 a101 a102 a103 a104 a105 a106 a107 a108 a11 a117 a118 a119 a12 a120 a121 a122 a123 a124
a125 a126 a127 a128 a129 a13 a130 a131 a132 a133 a134 a135 a136 a137 a138 a139 a14 a140 a141 a142 a143
a144 a145 a146 a147 a148 a149 a15 a150 a151 a152 a153 a154 a155 a156 a157 a158 a159 a16 a160 a162 a165
a166 a167 a168 a169 a17 a170 a171 a172 a173 a174 a175 a176 a177 a178 a179 a18 a180 a181 a182 a183 a184
a185 a186 a187 a188 a189 a19 a190 a191 a192 a193 a194 a195 a196 a197 a198 a199 a2 a20 a200 a201 a202 a203
a204 a205 a206 a21 a22 a23 a24 a25 a26 a27 a28 a29 a3 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a4 a40 a41
a42 a43 a44 a45 a46 a47 a48 a49 a5 a50 a51 a52 a53 a54 a55 a56 a57 a58 a59 a6 a60 a61 a62 a63 a64 a65 a66
a67 a68 a69 a7 a70 a72 a74 a75 a78 a79 a8 a81 a82 a83 a84 a85 a86 a87 a88 a89 a9 a90 a91 a92 a93 a94 a95
a96 a97 a98 a99 aacgr aacute Aacutesmall abreve acirc acircumflex Acircumflexsmall acute acutecomb
Acutesmall acy adieresis Adieresissmall ae aeacute aelig AEsmall afii00208 afii10017 afii10018 afii10019
afii10020 afii10021 afii10022 afii10023 afii10024 afii10025 afii10026 afii10027 afii10028 afii10029
afii10030 afii10031 afii10032 afii10033 afii10034 afii10035 afii10036 afii10037 afii10038 afii10039
afii10040 afii10041 afii10042 afii10043 afii10044 afii10045 afii10046 afii10047 afii10048 afii10049
afii10050 afii10051 afii10052 afii10053 afii10054 afii10055 afii10056 afii10057 afii10058 afii10059
afii10060 afii10061 afii10062 afii10063 afii10064 afii10065 afii10066 afii10067 afii10068 afii10069
afii10070 afii10071 afii10072 afii10073 afii10074 afii10075 afii10076 afii10077 afii10078 afii10079
afii10080 afii10081 afii10082 afii10083 afii10084 afii10085 afii10086 afii10087 afii10088 afii10089
afii10090 afii10091 afii10092 afii10093 afii10094 afii10095 afii10096 afii10097 afii10098 afii10099
afii10100 afii10101 afii10102 afii10103 afii10104 afii10105 afii10106 afii10107 afii10108 afii10109
afii10110 afii10145 afii10146 afii10147 afii10148 afii10192 afii10193 afii10194 afii10195 afii10196
afii10831 afii10832 afii10846 afii299 afii300 afii301 afii57381 afii57388 afii57392 afii57393 afii57394
afii57395 afii57396 afii57397 afii57398 afii57399 afii57400 afii57401 afii57403 afii57407 afii57409
afii57410 afii57411 afii57412 afii57413 afii57414 afii57415 afii57416 afii57417 afii57418 afii57419
afii57420 afii57421 afii57422 afii57423 afii57424 afii57425 afii57426 afii57427 afii57428 afii57429
afii57430 afii57431 afii57432 afii57433 afii57434 afii57440 afii57441 afii57442 afii57443 afii57444
afii57445 afii57446 afii57448 afii57449 afii57450 afii57451 afii57452 afii57453 afii57454 afii57455
afii57456 afii57457 afii57458 afii57470 afii57505 afii57506 afii57507 afii57508 afii57509 afii57511
afii57512 afii57513 afii57514 afii57519 afii57534 afii57636 afii57645 afii57658 afii57664 afii57665
afii57666 afii57667 afii57668 afii57669 afii57670 afii57671 afii57672 afii57673 afii57674 afii57675
afii57676 afii57677 afii57678 afii57679 afii57680 afii57681 afii57682 afii57683 afii57684 afii57685
afii57686 afii57687 afii57688 afii57689 afii57690 afii57694 afii57695 afii57700 afii57705 afii57716
afii57717 afii57718 afii57723 afii57793 afii57794 afii57795 afii57796 afii57797 afii57798 afii57799
afii57800 afii57801 afii57802 afii57803 afii57804 afii57806 afii57807 afii57839 afii57841 afii57842
afii57929 afii61248 afii61289 afii61352 afii61573 afii61574 afii61575 afii61664 afii63167 afii64937 agr
agrave Agravesmall airplane alefsym aleph alpha alphatonos amacr amacron amalg amp ampersand ampersandit
ampersanditlc ampersandsmall and ang ang90 angle angleleft angleright angmsd angsph angst anoteleia aogon
aogonek ap ape apos approxequal aquarius aries aring aringacute Aringsmall arrowboth arrowdblboth
arrowdbldown arrowdblleft arrowdblright arrowdblup arrowdown arrowdwnleft1 arrowdwnrt1 arrowhorizex
arrowleft arrowleftdwn1 arrowleftup1 arrowright arrowrtdwn1 arrowrtup1 arrowup arrowupdn arrowupdnbse
arrowupleft1 arrowuprt1 arrowvertex asciicircum asciitilde Asmall ast asterisk asteriskmath asuperior
asymp at atilde Atildesmall auml b backslash ballpoint bar barb2down barb2left barb2ne barb2nw barb2right
barb2se barb2sw barb2up barb4down barb4left barb4ne barb4nw barb4right barb4se barb4sw barb4up barwed
bcong bcy bdash1 bdash2 bdown bdquo becaus bell bepsi bernou beta beth bgr blank bleft bleftright blk12
blk14 blk34 block bne bnw bomb book bottom bowtie box2 box3 box4 boxcheckbld boxdl boxdr boxh boxhd boxhu
boxshadowdwn boxshadowup boxul boxur boxv boxvh boxvl boxvr boxxmarkbld bprime braceex braceleft
braceleftbt braceleftmid bracelefttp braceright bracerightbt bracerightmid bracerighttp bracketleft
bracketleftbt bracketleftex bracketlefttp bracketright bracketrightbt bracketrightex bracketrighttp breve
Brevesmall bright brokenbar brvbar bse bsim bsime Bsmall bsol bsuperior bsw budleafne budleafnw budleafse
budleafsw bull bullet bump bumpe bup bupdown c cacute cancer candle cap capricorn caret caron Caronsmall
carriagereturn ccaron ccedil ccedilla Ccedillasmall ccirc ccircumflex cdot cdotaccent cedil cedilla
Cedillasmall cent centinferior centoldstyle centsuperior chcy check checkbld chi cir circ circle circle2
circle4 circle6 circledown circleleft circlemultiply circleplus circleright circleshadowdwn
circleshadowup circlestar circleup circumflex Circumflexsmall cire clear club clubs colon colone
colonmonetary comma commaaccent commainferior command commasuperior commat comp compfn cong congruent
conint coprod copy copyright copyrightsans copyrightserif copysr crarr crescentstar cross crossceltic
crossmaltese crossoutline crossshadow crosstar2 Csmall cuepr cuesc cularr cup cupre curarr curren
currency cuspopen cuspopen1 cuvee cuwed cyrbreve cyrflex d dagger daggerdbl daleth darr darr2 dash dashv
dblac dblgrave dcaron dcroat dcy deg degree deleteleft deleteright delta dgr dharl dharr diam diamond
diams die dieresis dieresisacute dieresisgrave Dieresissmall dieresistonos divide divonx djcy dkshade
dlarr dlcorn dlcrop dnblock dodecastar3 dollar dollarinferior dollaroldstyle dollarsuperior dong dot
dotaccent Dotaccentsmall dotbelowcomb DotDot dotlessi dotlessj dotmath drarr drcorn drcrop droplet dscy
Dsmall dstrok dsuperior dtri dtrif dzcy e eacgr eacute Eacutesmall ebreve ecaron ecir ecirc ecircumflex
Ecircumflexsmall ecolon ecy edieresis Edieresissmall edot edotaccent eeacgr eegr efDot egr egrave
Egravesmall egs eight eightinferior eightoclock eightoldstyle eightsans eightsansinv eightsuperior
element elevenoclock ell ellipsis els emacr emacron emdash empty emptyset emsp emsp13 emsp14 endash eng
ensp envelopeback envelopefront eogon eogonek epsi epsilon epsilontonos epsis equal equals equiv
equivalence erDot escape esdot Esmall estimated esuperior eta etatonos eth Ethsmall euml euro excl exclam
exclamdbl exclamdown exclamdownsmall exclamsmall exist existential f fcy female ff ffi ffl fi figuredash
filecabinet filetalltext filetalltext1 filetalltext3 filledbox filledrect five fiveeighths fiveinferior
fiveoclock fiveoldstyle fivesans fivesansinv fivesuperior fl flag flat floppy3 floppy5 florin fnof folder
folderopen forall fork four fourinferior fouroclock fouroldstyle foursans foursansinv foursuperior frac12
frac13 frac14 frac15 frac16 frac18 frac23 frac25 frac34 frac35 frac38 frac45 frac56 frac58 frac78
fraction franc frasl frown frownface Fsmall g gamma gammad gbreve gcaron gcedil gcirc gcircumflex
gcommaaccent gcy gdot gdotaccent ge gel gemini germandbls ges Gg ggr gimel gjcy gl gnE gnsim gradient
grave gravecomb Gravesmall greater greaterequal gsdot gsim Gsmall gt guillemotleft guillemotright
guilsinglleft guilsinglright gvnE h H18533 H18543 H18551 H22073 hairsp hamilt handhalt handok handptdwn
handptleft handptright handptup handv handwrite handwriteleft hardcy harddisk harr harrw hbar hcirc
hcircumflex head2down head2left head2right head2up heart hearts hellip hexstar2 hookabovecomb horbar
hourglass house Hsmall hstrok hungarumlaut Hungarumlautsmall hybull hyphen hypheninferior hyphensuperior
i iacgr iacute Iacutesmall ibreve icirc icircumflex Icircumflexsmall icy idiagr idieresis Idieresissmall
idigr Idot Idotaccent iecy iexcl iff Ifraktur igr igrave Igravesmall ij ijlig imacr imacron image incare
infin infinity inodot int intcal integral integralbt integralex integraltp intersection invbullet
invcircle invsmileface iocy iogon iogonek iota iotadieresis iotadieresistonos iotatonos iquest isin
Ismall isuperior itilde iukcy iuml j jcirc jcircumflex jcy jsercy Jsmall jukcy k kappa kappav kcedil
kcommaaccent kcy keyboard kgr kgreen kgreenlandic khcy khgr kjcy Ksmall l lAarr lacute lagran lambda lang
laquo larr larr2 larrhk larrlp larrtl lcaron lcedil lceil lcommaaccent lcub lcy ldot ldquo ldquor le
leafccwne leafccwnw leafccwse leafccwsw leafne leafnw leafse leafsw leg leo les less lessequal lfblock
lfloor lg lgr lhard lharu lhblk libra lira ljcy ll lmidot lnE lnsim logicaland logicalnot logicalor longs
lowast lowbar loz lozenge lozenge4 lozenge6 lozf lpar lrarr2 lrhar2 lsaquo lsh lsim lslash Lslashsmall
Lsmall lsqb lsquo lsquor lstrok lsuperior lt lthree ltimes ltri ltrie ltrif ltshade lvnE m macr macron
Macronsmall mailboxflagdwn mailboxflagup mailbxopnflgdwn mailbxopnflgup male malt map marker mcy mdash
mgr micro mid middot minus minusb minute mldr mnplus models mouse2button Msmall msuperior mu multiply
mumap musicalnote musicalnotedbl n nabla nacute nap napos napostrophe natur nbsp ncaron ncedil
ncommaaccent ncong ncy ndash ne nearr nequiv neutralface nexist nge nges ngr ngt nharr ni nine
nineinferior nineoclock nineoldstyle ninesans ninesansinv ninesuperior njcy nlarr nldr nle nles nlt nltri
nltrie nmid not notelement notequal notin notsubset npar npr npre nrarr nrtri nrtrie nsc nsce nsim nsime
Nsmall nsub nsube nsup nsupe nsuperior ntilde Ntildesmall nu num numbersign numero numsp nvdash nwarr o
oacgr oacute Oacutesmall oast obreve ocir ocirc ocircumflex Ocircumflexsmall octastar2 octastar4 ocy
odash odblac odieresis Odieresissmall odot oe oelig OEsmall ogon ogonek Ogoneksmall ogr ograve
Ogravesmall ohacgr ohgr ohm ohorn ohungarumlaut olarr oline om omacr omacron omega omega1 omegatonos
omicron omicrontonos ominus one onedotenleader oneeighth onefitted onehalf oneinferior oneoclock
oneoldstyle onequarter onesans onesansinv onesuperior onethird openbullet oplus or orarr order ordf
ordfeminine ordm ordmasculine orthogonal oS oslash oslashacute Oslashsmall Osmall osol osuperior otilde
Otildesmall otimes ouml overline p par para paragraph parenleft parenleftbt parenleftex parenleftinferior
parenleftsuperior parenlefttp parenright parenrightbt parenrightex parenrightinferior parenrightsuperior
parenrighttp part partialdiff pc pcy pencil pennant pentastar2 percent percnt period periodcentered
periodinferior periodsuperior permil perp perpendicular perthousand peseta pgr phgr phi phi1 phis phiv
phmmat phone pi pisces piv planck plus plusb plusdo plusminus plusmn pound pr prescription prime prnsim
prod product prop propersubset propersuperset proportional prsim psgr psi Psmall puncsp q Qsmall query
quest question questiondown questiondownsmall questionsmall quiltsquare2 quiltsquare2inv quot quotedbl
quotedblbase quotedblleft quotedbllftbld quotedblright quotedblrtbld quoteleft quotereversed quoteright
quotesinglbase quotesingle r rAarr racute radic radical radicalex rang raquo rarr rarr2 rarrhk rarrlp
rarrtl rarrw rcaron rcedil rceil rcommaaccent rcub rcy rdquo rdquor readingglasses real rect reflexsubset
reflexsuperset reg registercircle registered registersans registerserif registersquare revlogicalnot
rfloor Rfraktur rgr rhard rharu rho rhombus4 rhombus6 rhov ring ring2 ring4 ring6 ringbutton2 Ringsmall
rlarr2 rlhar2 rosette rosettesolid rpar rsaquo rsh Rsmall rsqb rsquo rsquor rsuperior rtblock rthree
rtimes rtri rtrie rtrif rupiah rx s sacute saggitarius samalg sbquo sc scaron Scaronsmall sccue scedil
scedilla scirc scircumflex scissors scissorscutting scnsim scommaaccent scorpio scsim scy sdot sdotb
second sect section semi semicolon setmn seven seveneighths seveninferior sevenoclock sevenoldstyle
sevensans sevensansinv sevensuperior sextile SF010000 SF020000 SF030000 SF040000 SF050000 SF060000
SF070000 SF080000 SF090000 SF100000 SF110000 SF190000 SF200000 SF210000 SF220000 SF230000 SF240000
SF250000 SF260000 SF270000 SF280000 SF360000 SF370000 SF380000 SF390000 SF400000 SF410000 SF420000
SF430000 SF440000 SF450000 SF460000 SF470000 SF480000 SF490000 SF500000 SF510000 SF520000 SF530000
SF540000 sfgr sgr shade sharp shchcy shcy shy sigma sigma1 sigmav sim sime similar six sixinferior
sixoclock sixoldstyle sixsans sixsansinv sixsuperior skullcrossbones slash smile smileface snowflake
softcy sol space spade spades sqcap sqcup sqsub sqsube sqsup sqsupe squ square square2 square4 square6
squf Ssmall sstarf ssuperior star starf starofdavid starshadow sterling sub sube subnE suchthat sum
summation sun sung sunshine sup sup1 sup2 sup3 supe supnE szlig t tapereel target tau taurus tbar tcaron
tcedil tcommaaccent tcy tdot telephonesolid telhandsetcirc telrec tenoclock tensans tensansinv tgr there4
therefore theta theta1 thetas thetasym thetav thgr thinsp thkap thksim thorn Thornsmall three
threeeighths threeinferior threeoclock threeoldstyle threequarters threequartersemdash threesans
threesansinv threesuperior thumbdown thumbup tilde tildecomb Tildesmall times timesb tonos top tprime
trade trademark trademarksans trademarkserif triagdn triaglf triagrt triagup trie tristar2 tscy tshcy
Tsmall tstrok tsuperior twelveoclock twixt two twodotenleader twoinferior twooclock twooldstyle twosans
twosansinv twosuperior twothirds u uacgr uacute Uacutesmall uarr uarr2 ubrcy ubreve ucirc ucircumflex
Ucircumflexsmall ucy udblac udiagr udieresis Udieresissmall udigr ugr ugrave Ugravesmall uharl uharr
uhblk uhorn uhungarumlaut ulcorn ulcrop umacr umacron uml underscore underscoredbl union universal uogon
uogonek upblock uplus upsi upsih upsilon Upsilon1 upsilondieresis upsilondieresistonos upsilontonos
urcorn urcrop uring Usmall utilde utri utrif uuml v varr vcy vdash veebar vellip verbar vineleafboldne
vineleafboldnw vineleafboldse vineleafboldsw virgo vltri vprime vprop vrtri Vsmall Vvdash w wacute wcirc
wcircumflex wdieresis wedgeq weierp weierstrass wgrave wheel windowslogo wreath Wsmall x xcirc xdtri xgr
xi xmarkbld xrhombus Xsmall xutri y yacute Yacutesmall yacy ycirc ycircumflex ycy ydieresis
Ydieresissmall yen ygrave yicy yinyang Ysmall yucy yuml z zacute zcaron Zcaronsmall zcy zdot zdotaccent
zero zeroinferior zerooldstyle zerosans zerosansinv zerosuperior zeta zgr zhcy Zsmall zwnj

B<Please Note:> You may notice that apart from the 'AGL/WGL4', names from the XML, (X)HTML and SGML
specification sets have been included to enable interoperability towards PDF.

=head1 HISTORY

    $Log: Util.pm,v $
    Revision 1.20  2005/03/15 00:59:43  fredo
    cleanup

    Revision 1.19  2005/03/14 23:51:47  fredo
    beatification

    Revision 1.18  2005/03/14 22:01:06  fredo
    upd 2005

    Revision 1.17  2005/02/11 18:48:36  fredo
    added getPaperSizes method to help fix PDF::Report.

    Revision 1.16  2004/12/31 03:12:46  fredo
    no message

    Revision 1.15  2004/12/29 01:14:01  fredo
    fixed no warn for recursion

    Revision 1.14  2004/12/16 00:30:52  fredo
    added no warn for recursion

    Revision 1.13  2004/11/22 02:34:13  fredo
    moved unicode+glyphs to better maintainable uniglyph.txt

    Revision 1.12  2004/06/21 22:33:37  fredo
    added basic pattern/shading handling

    Revision 1.11  2004/06/15 09:11:38  fredo
    removed cr+lf

    Revision 1.10  2004/06/07 19:44:12  fredo
    cleaned out cr+lf for lf

    Revision 1.9  2004/02/12 14:39:22  fredo
    start work on better HSV code

    Revision 1.8  2004/02/10 15:53:57  fredo
    corrected pdfkeys

    Revision 1.7  2004/02/05 22:21:48  fredo
    fixed lab behavior

    Revision 1.6  2004/02/05 16:13:23  fredo
    fixed namecolor methods

    Revision 1.5  2004/02/05 11:28:48  fredo
    simplified namecolor,
    added *_lab/*_cmyk methods,
    corrected rgb->cmyk conversion to practical parameters

    Revision 1.4  2003/12/08 13:05:19  Administrator
    corrected to proper licencing statement

    Revision 1.3  2003/11/30 17:20:10  Administrator
    merged into default

    Revision 1.2.2.1  2003/11/30 16:56:22  Administrator
    merged into default

    Revision 1.2  2003/11/30 11:32:17  Administrator
    added CVS id/log


=cut