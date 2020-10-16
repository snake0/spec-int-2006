$benchnum  = '437';
$benchname = 'leslie3d';
$exename   = 'leslie3d';
$benchlang = 'F';
@base_exe  = ($exename);

$reltol = {'default' => undef};
$abstol = {'default' => undef};
$floatcompare = 1;

@sources = qw( tml.f );

sub invoke {
    my ($me) = @_;
    my $name;
    my @rc;

    my $exe = $me->exe_file;
    for ($me->input_files_base) {
        if (($name) = m/(.*).in$/) {
            push (@rc, { 'command' => $exe, 
                         'args'    => [ ], 
                         'input'   => $_,
                         'output'  => "$name.stdout",
                         'error'   => "$name.err",
                        });
        }
    }
    return @rc;
}

1;
