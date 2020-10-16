$benchnum  = '434';
$benchname = 'zeusmp';
$exename   = 'zeusmp';
$benchlang = 'F77';
@base_exe  = ($exename);

$reltol = {'tsl000aa'   => 0.001, 'default' => undef};
$abstol = {'tsl000aa'   => 0.001, 'default' => undef};

@sources = qw( advx1.f advx2.f advx3.f avisc.f avisc_d.f bndyflgs.f bval3d.f
               bvalemf.f ct.f dataio.f diverg.f empty.f findno.f
               forces.f forces_d.f fourn.f ggen.f grdv.f hdfall.f hsmoc.f
               intchk.f lorentz.f lorentz_d.f maxmin.f mnmx.f momx1.f momx2.f
               momx3.f movie.f msave.f mstart.f newdt.f newgrid.f newvg.f
               newx1.f newx2.f nudt.f pdv.f pdv_d.f gpbv.f pressure.f printd.f
               restart.f setup.f spenergy.f srcstep.f strtoi.f tslice.f
               transprt.f tranx1.f tranx2.f tranx3.f zeusmp.F blast.f textdmp.f
               linpck.f );

sub invoke {
    my ($me) = @_;
    my $name = $me->name;
    return ({ 'command' => $me->exe_file, 
		 'args'    => [ ], 
		 'output'  => "$name.stdout",
		 'error'   => "$name.err",
		});
}

1;

