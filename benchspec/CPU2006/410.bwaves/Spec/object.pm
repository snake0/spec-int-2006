$benchnum  = '410';
$benchname = 'bwaves';
$exename   = 'bwaves';
$benchlang = 'F77';
@base_exe  = ($exename);

$reltol = {'bwaves2.out'   => 0.015,
           'bwaves3.out'   => 0.000001,
	   'default' => undef};

$abstol = {'bwaves.out'   => 1.0e-16, 
           'default' => undef};

@sources = qw( block_solver.f flow_lam.f flux_lam.f 
		jacobian_lam.f shell_lam.f );

sub invoke {
    my ($me) = @_;
    my $name = $me->name;
    return ({ 'command' => $me->exe_file, 
		 'args'    => [ ], 
		 'error'   => "$name.err",
		});
}

1;
