$benchnum  = '416';
$benchname = 'gamess';
$exename   = 'gamess';
$benchlang = 'F77';
@base_exe  = ($exename);
$bench_fppflags = '-DSPEC_CPU_NO_HOLLERITH';

$reltol = 0.0001;
$abstol = 1.0e-5;
$ignorecase = 1;

@sources =  qw(
      aldeci.F algnci.F basecp.F basext.f bashuz.f bashz2.f basn21.F basn31.f 
      bassto.F blas.F ccaux.f ccsdt.F chgpen.F cisgrd.F cosmo.F cphf.F cpmchf.f 
      cprohf.F ddi.F delocl.F dft.F dftaux.F dftexc.F dftfun.f dftgrd.F 
      dftint.F dgeev.f dmulti.F drc.F dummygetenv.F ecp.F ecpder.F ecplib.F 
      ecppot.f efdrvr.F efelec.f efgrd2.F efgrda.F efgrdb.F efgrdc.F efinp.F 
      efinta.F efintb.F efpaul.F efpcm.F efpcov.F eigen.F eomcc.F ffield.F frfmt.F
      fsodci.F gamess.F globop.F gradex.F grd1.F grd2a.F grd2b.f grd2c.F guess.F
      gugdga.F gugdgb.F gugdm.F gugdm2.F gugdrt.F gugem.F gugsrt.F gvb.F hess.F
      hss1a.F hss1b.F hss2a.F hss2b.F inputa.F inputb.F inputc.F int1.F int2a.F
      int2b.f iolib.F lagran.F local.F loccd.F locpol.F mccas.F mcjac.f mcpinp.F
      mcpint.F mcplib.f mcqdpt.F mcqdwt.f mcqud.F mcscf.F mctwo.F morokm.F mp2.F
      mp2ddi.F mp2grd.F mpcdat.f mpcgrd.F mpcint.F mpcmol.F mpcmsc.F mthlib.F
      nameio.F nmr.F olix.f ordint.F ormas1.F parley.F pcm.F pcmcav.f pcmcv2.F
      pcmder.F pcmdis.F pcmief.F pcmpol.F pcmvch.F prpel.F prplib.F prppop.F 
      qeigen.F qfmm.F qmfm.F qmmm.F qrel.F raman.F rhfuhf.F rxncrd.F ryspol.f 
      scflib.F scfmi.F scrf.F sobrt.F soffac.F solib.F sozeff.F statpt.F surf.F 
      symorb.F symslc.F tdhf.F trans.F trfdm2.F trnstn.F trudge.F umpddi.F unport.F 
      vibanl.F vscf.F zheev.F zmatrx.F
              );

$need_math = 'yes';

sub invoke {
    my ($me) = @_;
    my $name;
    my @rc;
    my $input;

    my $exe = $me->exe_file;
    for ($me->input_files_base) {
        if (($name) = m/(.*).inp$/) {
            $input = $name;
            push (@rc, { 'command' => $exe, 
                         'args'    => [ ],
                         'output'  => "$input.out",
                         'input'   => "$input.config",
                         'error'   => "$input.err",
                 });
        }
    }
    return @rc;
}

1;
