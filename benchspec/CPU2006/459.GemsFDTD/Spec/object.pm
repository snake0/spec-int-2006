$benchnum  = '459';
$benchname = 'GemsFDTD';
$exename   = 'GemsFDTD';
$benchlang = 'F';
@base_exe  = ($exename);

$obiwan = 1;
$reltol = 1e-9;  
$abstol = 1e-10;

@sources = qw( 
             errorcheck.f90
             parameter.f90
             globalvar.f90
             excite.f90
             fourier_transf.f90
             huygens.F90
             posvector.f90
             NFT.F90
             readline.f90
             PEC.f90
             UPML.F90
             calcflops.f90
             progress.f90
             update.F90
             leapfrog.f90
             readdata.f90
             GemsFDTD.f90
             timerRoutine.f90
                        );

sub invoke {
    my ($me) = @_;
    my $name;
    my @rc;

    my $exe = $me->exe_file;
    for ($me->input_files_base) {
	if (($name) = m/(.*).in$/) {
	    push (@rc, { 'command' => $exe, 
			 'args'    => [ ],
			 'output'  => "$name.log",
			 'error'   => "$name.err",
			});
	}
    }
    return @rc;
}


%deps = (
  'GemsFDTD.f90' => 
               [
                 'parameter.f90',
                 'readdata.f90',
                 'leapfrog.f90'
               ],
  'NFT.F90' => 
               [
                 'parameter.f90',
                 'globalvar.f90',
                 'posvector.f90',
                 'fourier_transf.f90',
                 'errorcheck.f90',
                 'huygens.F90',
                 'excite.f90'
               ],
  'PEC.f90' => 
               [
                 'parameter.f90',
                 'errorcheck.f90',
                 'readline.f90',
                 'globalvar.f90'
               ],
  'UPML.F90' => 
               [
                 'parameter.f90',
                 'globalvar.f90',
                 'errorcheck.f90',
                 'PEC.f90'
               ],
  'calcflops.f90' => 
               [
                 'globalvar.f90',
                 'parameter.f90',
                 'huygens.F90',
                 'UPML.F90',
                 'NFT.F90'
               ],
  'excite.f90' => 
               [
                 'parameter.f90',
                 'globalvar.f90'
               ],
  'fourier_transf.f90' => 
               [
                 'parameter.f90'
               ],
  'globalvar.f90' => 
               [
                 'parameter.f90'
               ],
  'huygens.F90' => 
               [
                 'parameter.f90',
                 'globalvar.f90',
                 'excite.f90',
                 'errorcheck.f90'
               ],
  'leapfrog.f90' => 
               [
                 'errorcheck.f90',
                 'parameter.f90',
                 'huygens.F90',
                 'update.F90',
                 'UPML.F90',
                 'PEC.f90',
                 'NFT.F90',
                 'calcflops.f90',
                 'globalvar.f90',
                 'progress.f90'
               ],
  'posvector.f90' => 
               [
                 'parameter.f90'
               ],
  'progress.f90' => 
               [
                 'parameter.f90',
                 'globalvar.f90'
               ],
  'readdata.f90' => 
               [
                 'parameter.f90',
                 'globalvar.f90',
                 'errorcheck.f90',
                 'huygens.F90',
                 'UPML.F90',
                 'excite.f90',
                 'NFT.F90',
                 'PEC.f90',
                 'progress.f90'
               ],
  'update.F90' => 
               [
                 'parameter.f90',
                 'globalvar.f90'
               ],
);


1;

