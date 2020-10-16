use ExtUtils::MakeMaker;
WriteMakefile(
    'NAME' => 'XML::SAX::ExpatXS',
    'VERSION_FROM' => 'lib/XML/SAX/ExpatXS.pm',
    'PREREQ_PM' => {
		    'XML::SAX' => 0.12,
		   },
    'ABSTRACT_FROM' => 'lib/XML/SAX/ExpatXS.pm',
    'AUTHOR' => 'Matt Sergeant <matt AT sergeant DOT org>',
    LIBS => ($^O ne 'MSWin32') ? '-lexpat' : '../../output/lib/libexpatMT.lib',
    INC => ($^O eq 'MSWin32') ? '-I../../output/include' : undef,
);

# the following part is borrowed from XML::SAX
sub MY::install {
    package MY;
    my $script = shift->SUPER::install(@_);
    if (1 || ExtUtils::MakeMaker::prompt("Do you want to alter ParserDetails.ini?", "Y") =~ /^y/i) {
        $script =~ s/install :: (.*)$/install :: $1 install_expat_xs/m;
        $script .= <<"INSTALL";

install_expat_xs :
\t\@\$(PERL) -MXML::SAX -e "XML::SAX->add_parser(q(XML::SAX::ExpatXS))->save_parsers()"

INSTALL

    }

    return $script;
}
