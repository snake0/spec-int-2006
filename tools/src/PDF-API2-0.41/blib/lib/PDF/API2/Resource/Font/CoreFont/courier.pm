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
#   $Id: courier.pm,v 1.7 2005/03/14 22:01:28 fredo Exp $
#
#=======================================================================

$fonts->{courier}= {
    'fontname' => 'Courier',
    'type' => 'Type1',
    'apiname' => 'Co1',
    'ascender' => '629',
    'capheight' => '562',
    'descender' => '-157',
    'iscore' => '1',
    'isfixedpitch' => '1',
    'italicangle' => '0',
    'missingwidth' => '600',
    'underlineposition' => '-100',
    'underlinethickness' => '50',
    'xheight' => '426',
    'firstchar' => '32',
    'lastchar' => '255',
    'fontbbox' => [-23, -250, 715, 805],
    'char' => [
        '.notdef',           # C+00, U+0000
        '.notdef',           # C+01, U+0000
        '.notdef',           # C+02, U+0000
        '.notdef',           # C+03, U+0000
        '.notdef',           # C+04, U+0000
        '.notdef',           # C+05, U+0000
        '.notdef',           # C+06, U+0000
        '.notdef',           # C+07, U+0000
        '.notdef',           # C+08, U+0000
        '.notdef',           # C+09, U+0000
        '.notdef',           # C+0A, U+0000
        '.notdef',           # C+0B, U+0000
        '.notdef',           # C+0C, U+0000
        '.notdef',           # C+0D, U+0000
        '.notdef',           # C+0E, U+0000
        '.notdef',           # C+0F, U+0000
        '.notdef',           # C+10, U+0000
        '.notdef',           # C+11, U+0000
        '.notdef',           # C+12, U+0000
        '.notdef',           # C+13, U+0000
        '.notdef',           # C+14, U+0000
        '.notdef',           # C+15, U+0000
        '.notdef',           # C+16, U+0000
        '.notdef',           # C+17, U+0000
        '.notdef',           # C+18, U+0000
        '.notdef',           # C+19, U+0000
        '.notdef',           # C+1A, U+0000
        '.notdef',           # C+1B, U+0000
        '.notdef',           # C+1C, U+0000
        '.notdef',           # C+1D, U+0000
        '.notdef',           # C+1E, U+0000
        '.notdef',           # C+1F, U+0000
        'space',             # C+20, U+0020
        'exclam',            # C+21, U+0021
        'quotedbl',          # C+22, U+0022
        'numbersign',        # C+23, U+0023
        'dollar',            # C+24, U+0024
        'percent',           # C+25, U+0025
        'ampersand',         # C+26, U+0026
        'quoteright',        # C+27, U+2019
        'parenleft',         # C+28, U+0028
        'parenright',        # C+29, U+0029
        'asterisk',          # C+2A, U+002A
        'plus',              # C+2B, U+002B
        'comma',             # C+2C, U+002C
        'hyphen',            # C+2D, U+002D
        'period',            # C+2E, U+002E
        'slash',             # C+2F, U+002F
        'zero',              # C+30, U+0030
        'one',               # C+31, U+0031
        'two',               # C+32, U+0032
        'three',             # C+33, U+0033
        'four',              # C+34, U+0034
        'five',              # C+35, U+0035
        'six',               # C+36, U+0036
        'seven',             # C+37, U+0037
        'eight',             # C+38, U+0038
        'nine',              # C+39, U+0039
        'colon',             # C+3A, U+003A
        'semicolon',         # C+3B, U+003B
        'less',              # C+3C, U+003C
        'equal',             # C+3D, U+003D
        'greater',           # C+3E, U+003E
        'question',          # C+3F, U+003F
        'at',                # C+40, U+0040
        'A',                 # C+41, U+0041
        'B',                 # C+42, U+0042
        'C',                 # C+43, U+0043
        'D',                 # C+44, U+0044
        'E',                 # C+45, U+0045
        'F',                 # C+46, U+0046
        'G',                 # C+47, U+0047
        'H',                 # C+48, U+0048
        'I',                 # C+49, U+0049
        'J',                 # C+4A, U+004A
        'K',                 # C+4B, U+004B
        'L',                 # C+4C, U+004C
        'M',                 # C+4D, U+004D
        'N',                 # C+4E, U+004E
        'O',                 # C+4F, U+004F
        'P',                 # C+50, U+0050
        'Q',                 # C+51, U+0051
        'R',                 # C+52, U+0052
        'S',                 # C+53, U+0053
        'T',                 # C+54, U+0054
        'U',                 # C+55, U+0055
        'V',                 # C+56, U+0056
        'W',                 # C+57, U+0057
        'X',                 # C+58, U+0058
        'Y',                 # C+59, U+0059
        'Z',                 # C+5A, U+005A
        'bracketleft',       # C+5B, U+005B
        'backslash',         # C+5C, U+005C
        'bracketright',      # C+5D, U+005D
        'asciicircum',       # C+5E, U+005E
        'underscore',        # C+5F, U+005F
        'quoteleft',         # C+60, U+2018
        'a',                 # C+61, U+0061
        'b',                 # C+62, U+0062
        'c',                 # C+63, U+0063
        'd',                 # C+64, U+0064
        'e',                 # C+65, U+0065
        'f',                 # C+66, U+0066
        'g',                 # C+67, U+0067
        'h',                 # C+68, U+0068
        'i',                 # C+69, U+0069
        'j',                 # C+6A, U+006A
        'k',                 # C+6B, U+006B
        'l',                 # C+6C, U+006C
        'm',                 # C+6D, U+006D
        'n',                 # C+6E, U+006E
        'o',                 # C+6F, U+006F
        'p',                 # C+70, U+0070
        'q',                 # C+71, U+0071
        'r',                 # C+72, U+0072
        's',                 # C+73, U+0073
        't',                 # C+74, U+0074
        'u',                 # C+75, U+0075
        'v',                 # C+76, U+0076
        'w',                 # C+77, U+0077
        'x',                 # C+78, U+0078
        'y',                 # C+79, U+0079
        'z',                 # C+7A, U+007A
        'braceleft',         # C+7B, U+007B
        'bar',               # C+7C, U+007C
        'braceright',        # C+7D, U+007D
        'asciitilde',        # C+7E, U+007E
        '.notdef',           # C+7F, U+0000
        'Euro',              # C+80, U+20AC
        'bullet',            # C+81, U+2022
        'quotesinglbase',    # C+82, U+201A
        'florin',            # C+83, U+0192
        'quotedblbase',      # C+84, U+201E
        'ellipsis',          # C+85, U+2026
        'dagger',            # C+86, U+2020
        'daggerdbl',         # C+87, U+2021
        'circumflex',        # C+88, U+02C6
        'perthousand',       # C+89, U+2030
        'Scaron',            # C+8A, U+0160
        'guilsinglleft',     # C+8B, U+2039
        'OE',                # C+8C, U+0152
        'bullet',            # C+8D, U+2022
        'Zcaron',            # C+8E, U+017D
        'bullet',            # C+8F, U+2022
        'bullet',            # C+90, U+2022
        'quoteleft',         # C+91, U+2018
        'quoteright',        # C+92, U+2019
        'quotedblleft',      # C+93, U+201C
        'quotedblright',     # C+94, U+201D
        'bullet',            # C+95, U+2022
        'endash',            # C+96, U+2013
        'emdash',            # C+97, U+2014
        'tilde',             # C+98, U+02DC
        'trademark',         # C+99, U+2122
        'scaron',            # C+9A, U+0161
        'guilsinglright',    # C+9B, U+203A
        'oe',                # C+9C, U+0153
        'bullet',            # C+9D, U+2022
        'zcaron',            # C+9E, U+017E
        'Ydieresis',         # C+9F, U+0178
        'space',             # C+A0, U+0020
        'exclamdown',        # C+A1, U+00A1
        'cent',              # C+A2, U+00A2
        'sterling',          # C+A3, U+00A3
        'fraction',          # C+A4, U+2044
        'yen',               # C+A5, U+00A5
        'florin',            # C+A6, U+0192
        'section',           # C+A7, U+00A7
        'currency',          # C+A8, U+00A4
        'quotesingle',       # C+A9, U+0027
        'quotedblleft',      # C+AA, U+201C
        'guillemotleft',     # C+AB, U+00AB
        'guilsinglleft',     # C+AC, U+2039
        'guilsinglright',    # C+AD, U+203A
        'fi',                # C+AE, U+FB01
        'fl',                # C+AF, U+FB02
        'degree',            # C+B0, U+00B0
        'endash',            # C+B1, U+2013
        'dagger',            # C+B2, U+2020
        'daggerdbl',         # C+B3, U+2021
        'periodcentered',    # C+B4, U+00B7
        'mu',                # C+B5, U+00B5
        'paragraph',         # C+B6, U+00B6
        'bullet',            # C+B7, U+2022
        'quotesinglbase',    # C+B8, U+201A
        'quotedblbase',      # C+B9, U+201E
        'quotedblright',     # C+BA, U+201D
        'guillemotright',    # C+BB, U+00BB
        'ellipsis',          # C+BC, U+2026
        'perthousand',       # C+BD, U+2030
        'threequarters',     # C+BE, U+00BE
        'questiondown',      # C+BF, U+00BF
        'Agrave',            # C+C0, U+00C0
        'grave',             # C+C1, U+0060
        'acute',             # C+C2, U+00B4
        'circumflex',        # C+C3, U+02C6
        'tilde',             # C+C4, U+02DC
        'macron',            # C+C5, U+00AF
        'breve',             # C+C6, U+02D8
        'dotaccent',         # C+C7, U+02D9
        'dieresis',          # C+C8, U+00A8
        'Eacute',            # C+C9, U+00C9
        'ring',              # C+CA, U+02DA
        'cedilla',           # C+CB, U+00B8
        'Igrave',            # C+CC, U+00CC
        'hungarumlaut',      # C+CD, U+02DD
        'ogonek',            # C+CE, U+02DB
        'caron',             # C+CF, U+02C7
        'emdash',            # C+D0, U+2014
        'Ntilde',            # C+D1, U+00D1
        'Ograve',            # C+D2, U+00D2
        'Oacute',            # C+D3, U+00D3
        'Ocircumflex',       # C+D4, U+00D4
        'Otilde',            # C+D5, U+00D5
        'Odieresis',         # C+D6, U+00D6
        'multiply',          # C+D7, U+00D7
        'Oslash',            # C+D8, U+00D8
        'Ugrave',            # C+D9, U+00D9
        'Uacute',            # C+DA, U+00DA
        'Ucircumflex',       # C+DB, U+00DB
        'Udieresis',         # C+DC, U+00DC
        'Yacute',            # C+DD, U+00DD
        'Thorn',             # C+DE, U+00DE
        'germandbls',        # C+DF, U+00DF
        'agrave',            # C+E0, U+00E0
        'AE',                # C+E1, U+00C6
        'acircumflex',       # C+E2, U+00E2
        'ordfeminine',       # C+E3, U+00AA
        'adieresis',         # C+E4, U+00E4
        'aring',             # C+E5, U+00E5
        'ae',                # C+E6, U+00E6
        'ccedilla',          # C+E7, U+00E7
        'Lslash',            # C+E8, U+0141
        'Oslash',            # C+E9, U+00D8
        'OE',                # C+EA, U+0152
        'ordmasculine',      # C+EB, U+00BA
        'igrave',            # C+EC, U+00EC
        'iacute',            # C+ED, U+00ED
        'icircumflex',       # C+EE, U+00EE
        'idieresis',         # C+EF, U+00EF
        'eth',               # C+F0, U+00F0
        'ae',                # C+F1, U+00E6
        'ograve',            # C+F2, U+00F2
        'oacute',            # C+F3, U+00F3
        'ocircumflex',       # C+F4, U+00F4
        'dotlessi',          # C+F5, U+0131
        'odieresis',         # C+F6, U+00F6
        'divide',            # C+F7, U+00F7
        'lslash',            # C+F8, U+0142
        'oslash',            # C+F9, U+00F8
        'oe',                # C+FA, U+0153
        'germandbls',        # C+FB, U+00DF
        'udieresis',         # C+FC, U+00FC
        'yacute',            # C+FD, U+00FD
        'thorn',             # C+FE, U+00FE
        'ydieresis',         # C+FF, U+00FF
    ],
    'wx' => {
        'a'                  => 600,
        'A'                  => 600,
        'Aacute'             => 600,
        'aacute'             => 600,
        'abreve'             => 600,
        'Abreve'             => 600,
        'acircumflex'        => 600,
        'Acircumflex'        => 600,
        'acute'              => 600,
        'adieresis'          => 600,
        'Adieresis'          => 600,
        'AE'                 => 600,
        'ae'                 => 600,
        'agrave'             => 600,
        'Agrave'             => 600,
        'amacron'            => 600,
        'Amacron'            => 600,
        'ampersand'          => 600,
        'Aogonek'            => 600,
        'aogonek'            => 600,
        'aring'              => 600,
        'Aring'              => 600,
        'asciicircum'        => 600,
        'asciitilde'         => 600,
        'asterisk'           => 600,
        'at'                 => 600,
        'atilde'             => 600,
        'Atilde'             => 600,
        'B'                  => 600,
        'b'                  => 600,
        'backslash'          => 600,
        'bar'                => 600,
        'braceleft'          => 600,
        'braceright'         => 600,
        'bracketleft'        => 600,
        'bracketright'       => 600,
        'breve'              => 600,
        'brokenbar'          => 600,
        'bullet'             => 600,
        'C'                  => 600,
        'c'                  => 600,
        'cacute'             => 600,
        'Cacute'             => 600,
        'caron'              => 600,
        'Ccaron'             => 600,
        'ccaron'             => 600,
        'ccedilla'           => 600,
        'Ccedilla'           => 600,
        'cedilla'            => 600,
        'cent'               => 600,
        'circumflex'         => 600,
        'colon'              => 600,
        'comma'              => 600,
        'commaaccent'        => 600,
        'copyright'          => 600,
        'currency'           => 600,
        'd'                  => 600,
        'D'                  => 600,
        'dagger'             => 600,
        'daggerdbl'          => 600,
        'dcaron'             => 600,
        'Dcaron'             => 600,
        'dcroat'             => 600,
        'Dcroat'             => 600,
        'degree'             => 600,
        'Delta'              => 600,
        'dieresis'           => 600,
        'divide'             => 600,
        'dollar'             => 600,
        'dotaccent'          => 600,
        'dotlessi'           => 600,
        'e'                  => 600,
        'E'                  => 600,
        'eacute'             => 600,
        'Eacute'             => 600,
        'ecaron'             => 600,
        'Ecaron'             => 600,
        'ecircumflex'        => 600,
        'Ecircumflex'        => 600,
        'edieresis'          => 600,
        'Edieresis'          => 600,
        'Edotaccent'         => 600,
        'edotaccent'         => 600,
        'egrave'             => 600,
        'Egrave'             => 600,
        'eight'              => 600,
        'ellipsis'           => 600,
        'Emacron'            => 600,
        'emacron'            => 600,
        'emdash'             => 600,
        'endash'             => 600,
        'eogonek'            => 600,
        'Eogonek'            => 600,
        'equal'              => 600,
        'Eth'                => 600,
        'eth'                => 600,
        'Euro'               => 600,
        'exclam'             => 600,
        'exclamdown'         => 600,
        'F'                  => 600,
        'f'                  => 600,
        'fi'                 => 600,
        'five'               => 600,
        'fl'                 => 600,
        'florin'             => 600,
        'four'               => 600,
        'fraction'           => 600,
        'G'                  => 600,
        'g'                  => 600,
        'Gbreve'             => 600,
        'gbreve'             => 600,
        'gcommaaccent'       => 600,
        'Gcommaaccent'       => 600,
        'germandbls'         => 600,
        'grave'              => 600,
        'greater'            => 600,
        'greaterequal'       => 600,
        'guillemotleft'      => 600,
        'guillemotright'     => 600,
        'guilsinglleft'      => 600,
        'guilsinglright'     => 600,
        'H'                  => 600,
        'h'                  => 600,
        'hungarumlaut'       => 600,
        'hyphen'             => 600,
        'I'                  => 600,
        'i'                  => 600,
        'Iacute'             => 600,
        'iacute'             => 600,
        'Icircumflex'        => 600,
        'icircumflex'        => 600,
        'idieresis'          => 600,
        'Idieresis'          => 600,
        'Idotaccent'         => 600,
        'igrave'             => 600,
        'Igrave'             => 600,
        'Imacron'            => 600,
        'imacron'            => 600,
        'Iogonek'            => 600,
        'iogonek'            => 600,
        'j'                  => 600,
        'J'                  => 600,
        'k'                  => 600,
        'K'                  => 600,
        'Kcommaaccent'       => 600,
        'kcommaaccent'       => 600,
        'L'                  => 600,
        'l'                  => 600,
        'lacute'             => 600,
        'Lacute'             => 600,
        'lcaron'             => 600,
        'Lcaron'             => 600,
        'lcommaaccent'       => 600,
        'Lcommaaccent'       => 600,
        'less'               => 600,
        'lessequal'          => 600,
        'logicalnot'         => 600,
        'lozenge'            => 600,
        'Lslash'             => 600,
        'lslash'             => 600,
        'M'                  => 600,
        'm'                  => 600,
        'macron'             => 600,
        'minus'              => 600,
        'mu'                 => 600,
        'multiply'           => 600,
        'n'                  => 600,
        'N'                  => 600,
        'nacute'             => 600,
        'Nacute'             => 600,
        'ncaron'             => 600,
        'Ncaron'             => 600,
        'ncommaaccent'       => 600,
        'Ncommaaccent'       => 600,
        'nine'               => 600,
        'notequal'           => 600,
        'ntilde'             => 600,
        'Ntilde'             => 600,
        'numbersign'         => 600,
        'o'                  => 600,
        'O'                  => 600,
        'oacute'             => 600,
        'Oacute'             => 600,
        'Ocircumflex'        => 600,
        'ocircumflex'        => 600,
        'Odieresis'          => 600,
        'odieresis'          => 600,
        'oe'                 => 600,
        'OE'                 => 600,
        'ogonek'             => 600,
        'ograve'             => 600,
        'Ograve'             => 600,
        'ohungarumlaut'      => 600,
        'Ohungarumlaut'      => 600,
        'Omacron'            => 600,
        'omacron'            => 600,
        'one'                => 600,
        'onehalf'            => 600,
        'onequarter'         => 600,
        'onesuperior'        => 600,
        'ordfeminine'        => 600,
        'ordmasculine'       => 600,
        'Oslash'             => 600,
        'oslash'             => 600,
        'Otilde'             => 600,
        'otilde'             => 600,
        'P'                  => 600,
        'p'                  => 600,
        'paragraph'          => 600,
        'parenleft'          => 600,
        'parenright'         => 600,
        'partialdiff'        => 600,
        'percent'            => 600,
        'period'             => 600,
        'periodcentered'     => 600,
        'perthousand'        => 600,
        'plus'               => 600,
        'plusminus'          => 600,
        'Q'                  => 600,
        'q'                  => 600,
        'question'           => 600,
        'questiondown'       => 600,
        'quotedbl'           => 600,
        'quotedblbase'       => 600,
        'quotedblleft'       => 600,
        'quotedblright'      => 600,
        'quoteleft'          => 600,
        'quoteright'         => 600,
        'quotesinglbase'     => 600,
        'quotesingle'        => 600,
        'R'                  => 600,
        'r'                  => 600,
        'racute'             => 600,
        'Racute'             => 600,
        'radical'            => 600,
        'rcaron'             => 600,
        'Rcaron'             => 600,
        'rcommaaccent'       => 600,
        'Rcommaaccent'       => 600,
        'registered'         => 600,
        'ring'               => 600,
        'S'                  => 600,
        's'                  => 600,
        'Sacute'             => 600,
        'sacute'             => 600,
        'Scaron'             => 600,
        'scaron'             => 600,
        'Scedilla'           => 600,
        'scedilla'           => 600,
        'Scommaaccent'       => 600,
        'scommaaccent'       => 600,
        'section'            => 600,
        'semicolon'          => 600,
        'seven'              => 600,
        'six'                => 600,
        'slash'              => 600,
        'space'              => 600,
        'sterling'           => 600,
        'summation'          => 600,
        'T'                  => 600,
        't'                  => 600,
        'Tcaron'             => 600,
        'tcaron'             => 600,
        'tcommaaccent'       => 600,
        'Tcommaaccent'       => 600,
        'Thorn'              => 600,
        'thorn'              => 600,
        'three'              => 600,
        'threequarters'      => 600,
        'threesuperior'      => 600,
        'tilde'              => 600,
        'trademark'          => 600,
        'two'                => 600,
        'twosuperior'        => 600,
        'u'                  => 600,
        'U'                  => 600,
        'Uacute'             => 600,
        'uacute'             => 600,
        'ucircumflex'        => 600,
        'Ucircumflex'        => 600,
        'udieresis'          => 600,
        'Udieresis'          => 600,
        'Ugrave'             => 600,
        'ugrave'             => 600,
        'uhungarumlaut'      => 600,
        'Uhungarumlaut'      => 600,
        'umacron'            => 600,
        'Umacron'            => 600,
        'underscore'         => 600,
        'Uogonek'            => 600,
        'uogonek'            => 600,
        'Uring'              => 600,
        'uring'              => 600,
        'V'                  => 600,
        'v'                  => 600,
        'w'                  => 600,
        'W'                  => 600,
        'X'                  => 600,
        'x'                  => 600,
        'y'                  => 600,
        'Y'                  => 600,
        'yacute'             => 600,
        'Yacute'             => 600,
        'Ydieresis'          => 600,
        'ydieresis'          => 600,
        'yen'                => 600,
        'Z'                  => 600,
        'z'                  => 600,
        'zacute'             => 600,
        'Zacute'             => 600,
        'Zcaron'             => 600,
        'zcaron'             => 600,
        'Zdotaccent'         => 600,
        'zdotaccent'         => 600,
        'zero'               => 600,
    },
};


__END__

=head1 AUTHOR

alfred reibenschuh

=head1 HISTORY

    $Log: courier.pm,v $
    Revision 1.7  2005/03/14 22:01:28  fredo
    upd 2005

    Revision 1.6  2004/12/16 01:09:12  fredo
    updated to new adobe afm

    Revision 1.5  2004/06/15 09:14:53  fredo
    removed cr+lf

    Revision 1.4  2004/06/07 19:44:43  fredo
    cleaned out cr+lf for lf

    Revision 1.3  2003/12/08 13:06:01  Administrator
    corrected to proper licencing statement

    Revision 1.2  2003/11/30 17:32:49  Administrator
    merged into default

    Revision 1.1.1.1.2.2  2003/11/30 16:57:05  Administrator
    merged into default

    Revision 1.1.1.1.2.1  2003/11/30 15:52:21  Administrator
    added CVS id/log


=cut
