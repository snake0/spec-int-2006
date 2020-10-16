$benchnum  = '435';
$benchname = 'gromacs';
$exename   = 'gromacs';

# $benchlang is a statement about what rules the tools should apply to the
# source.  It is _NOT_ a statement about how the authors, the project leader,
# the CPU subcommittee, or SPEC view the program as a whole.
$benchlang = 'F77,C';

@base_exe  = ($exename);

$reltol = 0.0125;
$abstol = {'gromacs.out'   => undef, 'default' => undef};

@sources=qw(flincs.f fsettle.F fshake.f innerf.f
	    flincsd.f fsettled.F fshaked.f
	    3dview.c atomprop.c binio.c block_tx.c bondfree.c buffer.c
	    calcgrid.c calch.c calcmu.c calcvir.c clincs.c comlib.c
	    confio.c constr.c copyrite.c coupling.c csettle.c disre.c
	    do_fit.c do_gct.c dummies.c ebin.c edsam.c enxio.c ewald.c
	    ewald_util.c f77_wrappers.c fatal.c ffscanf.c fftgrid.c filenm.c
	    fnbf.c force.c futil.c gbutil.c gctio.c genalg.c ghat.c
	    glaasje.c gmx_system_xdr.c gmxfio.c ifunc.c index.c init.c
	    init_sh.c innerc.c invblock.c ionize.c libxdrf.c macros.c
	    main.c maths.c matio.c md.c mdatom.c mdebin.c mdrun.c memdump.c
	    minimize.c mshift.c mvdata.c mvxvf.c names.c network.c nrama.c
	    nrjac.c nrnb.c ns.c nsb.c nsgrid.c orires.c pargs.c pbc.c
	    pdbio.c pme.c poisson.c pppm.c princ.c psgather.c pssolve.c
	    psspread.c pull.c pullinit.c pullio.c pullutil.c rando.c
	    random.c rbin.c rdgroup.c readinp.c relax_sh.c replace.c
	    rmpbc.c shakef.c shift_util.c sim_util.c smalloc.c sortwater.c
	    splittop.c stat.c statutil.c strdb.c string2.c symtab.c
	    synclib.c tables.c tgroup.c tpxio.c trnio.c trxio.c txtdump.c
	    typedefs.c update.c vcm.c vec.c viewit.c wgms.c wman.c
	    wnblist.c writeps.c xdrd.c xtcio.c xutils.c xvgr.c);

$need_math = 'yes';

$bench_cflags = '-I. -DHAVE_CONFIG_H';

sub invoke {
    my ($me) = @_;
    my $name;
    my @rc;

    my $exe = $me->exe_file;
    for ($me->input_files_base) {
        if (($name) = m/(.*).tpr$/) {
            push (@rc, { 'command' => $exe, 
                         'args'    => [ '-silent', '-deffnm', $name, '-nice', '0' ], 
                         'error'   => "$name.err",
                        });
        }
    }
    return @rc;
}


1;
