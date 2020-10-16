$benchnum  = '465';
$benchname = 'tonto';
$exename   = 'tonto';
$benchlang = 'F';
@base_exe  = ($exename);

$reltol = 0.006;

@sources=(qw(
            types.F90
            system.F90
            str.F90
            int.F90
            real.F90
            intvec.F90
            realvec.F90
            binmat.F90
            cpxvec.F90
            realmat.F90
            parallel.F90
            intmat.F90
            buffer.F90
            intvecvec.F90
            strvec.F90
            binvec.F90
            unitnumber.F90
            textfile.F90
            opvector.F90
            file.F90
            cpxmat.F90
            opmatrix.F90
            archive.F90
            diis.F90
            reflection.F90
            cif.F90
            cpxmat3.F90
            realmat3.F90
            spacegroup.F90
            slatershell.F90
            slatershellvec.F90
            interpolator.F90
            slaterbasis.F90
            shell1.F90
            unitcell.F90
            shell.F90
            shellvec.F90
            basis.F90
            coppensorbital.F90
            coppensorbitalvec.F90
            coppensbasis.F90
            atom.F90
            reflectionvec.F90
            rys.F90
            cpxmat4.F90
            gaussian.F90
            gaussian2.F90
            realmat4.F90
            intmat3.F90
            shell2.F90
            atomvec.F90
            crystal.F90
            scfdata.F90
            basisvec.F90
            realmat5.F90
            gaussian4.F90
            shell4.F90
            time.F90
            colour.F90
            colourfunction.F90
            intvecmat3.F90
            intvecintvechash.F90
            marchingcube.F90
            plotgrid.F90
            isosurface.F90
            realmat3vec.F90
            cluster.F90
            cpxmat5.F90
            shellpair.F90
            shellpairvec.F90
            coppensbasisvec.F90
            irrep.F90
            irrepvec.F90
            pointgroup.F90
            slaterbasisvec.F90
            shell1quartet.F90
            roby.F90
            dftgrid.F90
            mol.F90
            mol_main.F90
            run_mol.F90
            blas.f90
            lapack.f90
           ));

$bench_fppflags = '-w -DUSE_PRE_AND_POST_CONDITIONS -DUSE_ERROR_MANAGEMENT -m literal.pm -m tonto.pm';

%deps = (
   'archive.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'opvector.F90', 
           'file.F90', 'str.F90', 
           'textfile.F90', 'opmatrix.F90'
           ],

   'atom.F90' => 
           [
           'types.F90', 'system.F90', 
           'slaterbasis.F90', 'shell1.F90', 
           'intvec.F90', 'real.F90', 
           'opvector.F90', 'realvec.F90', 
           'int.F90', 'unitcell.F90', 
           'basis.F90', 'textfile.F90', 
           'str.F90', 'strvec.F90', 
           'coppensbasis.F90', 'opmatrix.F90', 
           'realmat.F90', 'cpxvec.F90'
           ],

   'atomvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'slaterbasis.F90', 'cif.F90', 
           'shell2.F90', 'intvec.F90', 
           'real.F90', 'realvec.F90', 
           'int.F90', 'atom.F90', 
           'str.F90', 'textfile.F90', 
           'intvecvec.F90', 'strvec.F90', 
           'coppensbasis.F90', 'realmat.F90', 
           'interpolator.F90'
           ],

   'basis.F90' => 
           [
           'types.F90', 'system.F90', 
           'str.F90', 'textfile.F90', 
           'strvec.F90', 'shellvec.F90', 
           'shell.F90', 'binvec.F90'
           ],

   'basisvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'int.F90', 'str.F90', 
           'textfile.F90', 'strvec.F90', 
           'basis.F90'
           ],

   'binmat.F90' => 
           [
           'types.F90', 'system.F90'
           ],

   'binmat3.F90' => 
           [
           'types.F90', 'system.F90'
           ],

   'binvec.F90' => 
           [
           'types.F90', 'system.F90'
           ],

   'buffer.F90' => 
           [
           'types.F90', 'system.F90', 
           'str.F90'
           ],

   'cif.F90' => 
           [
           'types.F90', 'system.F90', 'realvec.F90', 
           'intvec.F90', 'textfile.F90', 
           'strvec.F90', 'realmat.F90'
           ],

   'cluster.F90' => 
           [
           'types.F90', 'system.F90', 
           'crystal.F90', 'intvec.F90', 
           'binvec.F90', 'real.F90', 
           'realvec.F90', 'int.F90', 
           'atom.F90', 'unitcell.F90', 
           'intmat.F90', 'realmat3.F90', 
           'realmat3vec.F90', 'str.F90', 
           'textfile.F90', 'intvecvec.F90', 
           'atomvec.F90', 'realmat.F90'
           ],

   'colour.F90' => 
           [
           'types.F90', 'system.F90', 
           'textfile.F90', 'str.F90', 
           'intmat.F90'
           ],

   'colourfunction.F90' => 
           [
   'types.F90', 
           'system.F90', 'realvec.F90', 
           'intvec.F90', 'str.F90', 
           'textfile.F90', 'colour.F90', 
           'real.F90', 'realmat.F90'
           ],

   'coppensbasis.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'coppensorbital.F90', 
           'coppensorbitalvec.F90', 'textfile.F90', 
           'str.F90', 'strvec.F90', 
           'binvec.F90', 'interpolator.F90'
           ],

   'coppensbasisvec.F90' => 
           [
   'types.F90', 
           'system.F90', 'int.F90', 
           'str.F90', 'textfile.F90', 
           'strvec.F90', 'coppensbasis.F90'
           ],

   'coppensorbital.F90' => 
           [
   'types.F90', 
           'system.F90', 'realvec.F90', 
           'intvec.F90', 'int.F90', 
           'str.F90', 'textfile.F90', 
           'strvec.F90', 'real.F90'
           ],

   'coppensorbitalvec.F90' => 
           [
   'types.F90', 
           'system.F90', 'int.F90', 
           'str.F90', 'textfile.F90', 
           'coppensorbital.F90'
           ],

   'cpxmat.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'int.F90', 
           'realmat.F90', 'real.F90', 
           'cpxvec.F90'
           ],

   'cpxmat3.F90' => 
           [
           'types.F90', 'system.F90'
           ],

   'cpxmat4.F90' => 
           [
           'types.F90', 'system.F90'
           ],

   'cpxmat5.F90' => 
           [
           'types.F90', 'system.F90'
           ],

   'cpxvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'int.F90'
           ],

   'crystal.F90' => 
           [
           'types.F90', 'system.F90', 
           'reflection.F90', 'cif.F90', 
           'intvec.F90', 'binvec.F90', 
           'cpxmat.F90', 'real.F90', 
           'cpxmat3.F90', 'realvec.F90', 
           'spacegroup.F90', 'int.F90', 
           'atom.F90', 'unitcell.F90', 
           'reflectionvec.F90', 'realmat3.F90', 
           'str.F90', 'textfile.F90', 
           'strvec.F90', 'atomvec.F90', 
           'archive.F90', 'realmat.F90', 
           'cpxvec.F90'
           ],

   'dftgrid.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'int.F90', 
           'parallel.F90', 'atom.F90', 
           'gaussian.F90', 'realmat3.F90', 
           'shell1.F90', 'textfile.F90', 
           'str.F90', 'atomvec.F90', 
           'real.F90', 'archive.F90', 
           'realmat.F90', 'cpxvec.F90'
           ],

   'diis.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'textfile.F90', 
           'int.F90', 'real.F90', 
           'realmat.F90', 'archive.F90'
           ],

   'file.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'intvec.F90', 
           'int.F90', 'parallel.F90', 
           'cpxvec.F90', 'unitnumber.F90'
           ],

   'gaussian.F90' => 
           [
           'types.F90', 'system.F90', 
           'intvec.F90', 'str.F90', 
           'textfile.F90', 'int.F90'
           ],

   'gaussian2.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'rys.F90', 
           'int.F90', 'gaussian.F90', 
           'intmat.F90', 'realmat3.F90', 
           'intvec.F90', 'textfile.F90', 
           'realmat.F90', 'cpxmat3.F90', 
           'cpxvec.F90'
           ],

   'gaussian4.F90' => 
           [
           'types.F90', 'system.F90', 
           'intvec.F90', 'textfile.F90', 
           'rys.F90', 'int.F90', 
           'realmat5.F90'
           ],

   'int.F90' => 
           [
   'types.F90', 
           'system.F90', 'str.F90'
           ],

   'interpolator.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'intvec.F90', 
           'str.F90', 'textfile.F90', 
           'real.F90'
           ],

   'intmat.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'intvec.F90', 
           'int.F90', 'binmat.F90', 
           'real.F90'
           ],

   'intmat3.F90' => 
           [
           'types.F90', 'system.F90'
           ],

   'intmat4.F90' => 
           [
           'types.F90', 'system.F90'
           ],

   'intvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'int.F90', 'real.F90'
           ],

   'intvec3inthash.F90' => 
           [
   'types.F90', 
           'system.F90', 'intvec.F90'
           ],

   'intvecinthash.F90' => 
           [
   'types.F90', 
           'system.F90', 'intvec.F90', 
           'intmat.F90'
           ],

   'intvecintvechash.F90' => 
           [
   'types.F90', 
           'system.F90', 'intvec.F90', 
           'intmat.F90'
           ],

   'intvecmat3.F90' => 
           [
           'types.F90', 'system.F90', 
           'intvec.F90'
           ],

   'intvecvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'intvec.F90'
           ],

   'irrep.F90' => 
           [
           'types.F90', 'system.F90', 
           'realmat3.F90', 'realvec.F90'
           ],

   'irrepvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'irrep.F90'
           ],

   'isosurface.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'int.F90', 
           'colourfunction.F90', 'intvecmat3.F90', 
           'intmat.F90', 'realmat3.F90', 
           'intvec.F90', 'realmat4.F90', 
           'intvecintvechash.F90', 'textfile.F90', 
           'str.F90', 'marchingcube.F90', 
           'atomvec.F90', 'plotgrid.F90', 
           'realmat.F90', 'real.F90'
           ],

   'marchingcube.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'int.F90', 
           'str.F90', 'textfile.F90'
           ],

   'marchingcubevec.F90' => 
           [
   'types.F90', 
           'system.F90', 'str.F90', 
           'textfile.F90', 'marchingcube.F90'
           ],

   'mol.F90' => 
           [
   'types.F90', 
           'system.F90', 'basisvec.F90', 
           'shell1.F90', 'shell2.F90', 
           'shell4.F90', 'plotgrid.F90', 
           'cpxmat3.F90', 'cpxmat4.F90', 
           'cpxmat5.F90', 'time.F90', 
           'atom.F90', 'reflectionvec.F90', 
           'diis.F90', 'textfile.F90', 
           'str.F90', 'shell.F90', 
           'atomvec.F90', 'cpxvec.F90', 
           'scfdata.F90', 'crystal.F90', 
           'cif.F90', 'intvec.F90', 
           'cpxmat.F90', 'real.F90', 
           'shellpair.F90', 'opvector.F90', 
           'shellpairvec.F90', 'realvec.F90', 
           'coppensbasisvec.F90', 'int.F90', 
           'pointgroup.F90', 'slaterbasisvec.F90', 
           'parallel.F90', 'shell1quartet.F90', 
           'intmat.F90', 'isosurface.F90', 
           'basis.F90', 'realmat3.F90', 
           'file.F90', 'realmat4.F90', 
           'realmat5.F90', 'intvecvec.F90', 
           'shellvec.F90', 'cluster.F90', 
           'roby.F90', 'strvec.F90', 
           'dftgrid.F90', 'archive.F90', 
           'opmatrix.F90', 'realmat.F90'
           ],

   'mol_main.F90' => 
           [
           'types.F90', 'system.F90', 
           'scfdata.F90', 'basisvec.F90', 
           'crystal.F90', 'cif.F90', 
           'shell2.F90', 'shell4.F90', 
           'cpxmat3.F90', 'realvec.F90', 
           'int.F90', 'time.F90', 
           'atom.F90', 'isosurface.F90', 
           'realmat3.F90', 'realmat4.F90', 
           'str.F90', 'textfile.F90', 
           'cluster.F90', 'atomvec.F90', 
           'realmat.F90', 'archive.F90', 
           'opmatrix.F90', 'mol.F90'
           ],

   'opmatrix.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'textfile.F90', 
           'cpxmat.F90', 'real.F90', 
           'realmat.F90'
           ],

   'opvector.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90'
           ],

   'parallel.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'intvec.F90', 
           'realmat.F90', 'cpxvec.F90'
           ],

   'plotgrid.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'int.F90', 
           'intvec.F90', 'textfile.F90', 
           'str.F90', 'atomvec.F90', 
           'realmat.F90'
           ],

   'pointgroup.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'int.F90', 
           'intmat.F90', 'realmat3.F90', 
           'intvec.F90', 'str.F90', 
           'textfile.F90', 'irrepvec.F90', 
           'realmat.F90'
           ],

   'real.F90' => 
           [
           'types.F90', 'system.F90', 
           'int.F90', 'str.F90'
           ],

   'realmat.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'intvec.F90', 
           'str.F90', 'int.F90', 
           'binmat.F90', 'real.F90', 
           'cpxvec.F90'
           ],

   'realmat3.F90' => 
           [
           'types.F90', 'system.F90', 
           'realmat.F90'
           ],

   'realmat3vec.F90' => 
           [
           'types.F90', 'system.F90', 
           'realmat3.F90'
           ],

   'realmat4.F90' => 
           [
           'types.F90', 'system.F90'
           ],

   'realmat4vec.F90' => 
           [
           'types.F90', 'system.F90', 
           'realmat4.F90'
           ],

   'realmat5.F90' => 
           [
           'types.F90', 'system.F90'
           ],

   'realmatmat.F90' => 
           [
           'types.F90', 'system.F90'
           ],

   'realmatvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'intmat3.F90', 'int.F90', 
           'realmat.F90', 'intmat.F90'
           ],

   'realvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'intvec.F90', 'str.F90', 
           'int.F90', 'real.F90'
           ],

   'realvecvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90'
           ],

   'reflection.F90' => 
           [
           'types.F90', 'system.F90', 
           'str.F90', 'textfile.F90', 
           'strvec.F90'
           ],

   'reflectionvec.F90' => 
           [
   'types.F90', 
           'system.F90', 'realvec.F90', 
           'reflection.F90', 'int.F90', 
           'str.F90', 'textfile.F90', 
           'real.F90', 'archive.F90', 
           'realmat.F90', 'intmat.F90'
           ],

   'roby.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'int.F90', 
           'atom.F90', 'intmat.F90', 
           'intvec.F90', 'intvecvec.F90', 
           'textfile.F90', 'str.F90', 
           'strvec.F90', 'atomvec.F90', 
           'realmat.F90', 'opmatrix.F90', 
           'real.F90'
           ],

   'run_atom.F90' => 
           [
   'types.F90', 
           'system.F90', 'textfile.F90', 
           'atom.F90'
           ],

   'run_atomvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'textfile.F90', 'atomvec.F90'
           ],

   'run_basis.F90' => 
           [
   'types.F90', 
           'system.F90', 'textfile.F90', 
           'basis.F90'
           ],

   'run_basisvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'textfile.F90'
           ],

   'run_buffer.F90' => 
           [
    'types.F90', 
           'system.F90', 'str.F90', 
           'textfile.F90', 'buffer.F90'
           ],

   'run_cif.F90' => 
           [
    'types.F90', 
           'system.F90', 'realvec.F90', 
           'textfile.F90', 'strvec.F90', 
           'cif.F90'
           ],

   'run_coppensbasisvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'textfile.F90'
           ],

   'run_cpx.F90' => 
           [
    'types.F90', 
           'system.F90', 'textfile.F90'
           ],

   'run_crystal.F90' => 
           [
           'types.F90', 'system.F90', 
           'textfile.F90', 'crystal.F90'
           ],

   'run_dftgrid.F90' => 
           [
           'types.F90', 'system.F90', 
           'textfile.F90', 'atomvec.F90', 
           'dftgrid.F90'
           ],

   'run_file.F90' => 
           [
    'types.F90', 
           'system.F90', 'textfile.F90', 
           'parallel.F90', 'str.F90', 
           'file.F90'
           ],

   'run_gaussian.F90' => 
           [
           'types.F90', 'system.F90', 
           'textfile.F90', 'gaussian.F90'
           ],

   'run_gaussian2.F90' => 
           [
           'types.F90', 'system.F90', 
           'textfile.F90', 'dftgrid.F90', 
           'gaussian.F90', 'realmat.F90', 
           'gaussian2.F90', 'test.F90'
           ],

   'run_int.F90' => 
           [
    'types.F90', 
           'system.F90', 'textfile.F90', 
           'int.F90'
           ],

   'run_intvec.F90' => 
           [
   'types.F90', 
           'system.F90', 'textfile.F90'
           ],

   'run_isosurface.F90' => 
           [
           'types.F90', 'system.F90', 
           'textfile.F90', 'isosurface.F90'
           ],

   'run_mol.F90' => 
           [
   'types.F90', 
           'system.F90', 'textfile.F90', 
           'parallel.F90', 'mol_main.F90'
           ],

   'run_pointgroup.F90' => 
           [
           'types.F90', 'system.F90', 
           'textfile.F90'
           ],

   'run_real.F90' => 
           [
    'types.F90', 
           'system.F90', 'textfile.F90', 
           'real.F90'
           ],

   'run_realmat.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'int.F90', 
           'textfile.F90'
           ],

   'run_realmatvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'textfile.F90'
           ],

   'run_realvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'textfile.F90', 'real.F90'
           ],

   'run_shell.F90' => 
           [
   'types.F90', 
           'system.F90', 'textfile.F90'
           ],

   'run_shell1.F90' => 
           [
   'types.F90', 
           'system.F90', 'textfile.F90'
           ],

   'run_shell4.F90' => 
           [
   'types.F90', 
           'system.F90', 'realvec.F90', 
           'realmat4.F90', 'int.F90', 
           'textfile.F90'
           ],

   'run_shellvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'textfile.F90'
           ],

   'run_slaterbasisvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'textfile.F90'
           ],

   'run_spacegroup.F90' => 
           [
           'types.F90', 'system.F90', 
           'textfile.F90'
           ],

   'run_str.F90' => 
           [
    'types.F90', 
           'system.F90', 'textfile.F90'
           ],

   'run_strvec.F90' => 
           [
    'types.F90', 
           'system.F90', 'textfile.F90'
           ],

   'run_textfile.F90' => 
           [
           'types.F90', 'system.F90', 
           'parallel.F90'
           ],

   'rys.F90' => 
           [
   'types.F90', 
           'system.F90', 'realvec.F90'
           ],

   'scfdata.F90' => 
           [
           'types.F90', 'system.F90', 
           'diis.F90', 'int.F90', 
           'str.F90', 'textfile.F90', 
           'crystal.F90', 'real.F90'
           ],

   'shell.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'int.F90', 
           'str.F90', 'textfile.F90', 
           'strvec.F90'
           ],

   'shell1.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'int.F90', 
           'intmat.F90', 'intvec.F90', 
           'textfile.F90', 'str.F90', 
           'realmat.F90'
           ],

   'shell1quartet.F90' => 
           [
   'types.F90', 
           'system.F90', 'realvec.F90', 
           'rys.F90', 'int.F90', 
           'intmat.F90', 'realmat3.F90', 
           'realmat4.F90', 'shell2.F90', 
           'intmat3.F90', 'intvec.F90', 
           'textfile.F90', 'shell.F90', 
           'realmat.F90'
           ],

   'shell2.F90' => 
           [
           'types.F90', 'system.F90', 
           'rys.F90', 'shell1.F90', 
           'intvec.F90', 'cpxmat3.F90', 
           'realvec.F90', 'cpxmat4.F90', 
           'int.F90', 'gaussian2.F90', 
           'intmat.F90', 'realmat3.F90', 
           'realmat4.F90', 'intmat3.F90', 
           'textfile.F90', 'realmat.F90', 
           'cpxvec.F90'
           ],

   'shell4.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'rys.F90', 
           'int.F90', 'gaussian4.F90', 
           'intmat.F90', 'shell1.F90', 
           'realmat3.F90', 'intmat3.F90', 
           'realmat4.F90', 'intvec.F90', 
           'textfile.F90', 'realmat5.F90', 
           'realmat.F90'
           ],

   'shellpair.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'intmat3.F90', 
           'intvec.F90', 'int.F90', 
           'shell.F90', 'intmat.F90'
           ],

   'shellpairvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'shellpair.F90'
           ],

   'shellquartet.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'rys.F90', 
           'int.F90', 'intmat.F90', 
           'realmat3.F90', 'realmat4.F90', 
           'intvec.F90', 'intmat3.F90', 
           'shell2.F90', 'shell.F90', 
           'shellpair.F90', 'realmat.F90'
           ],

   'shellvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'int.F90', 'str.F90', 
           'textfile.F90', 'shell.F90'
           ],

   'slaterbasis.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'textfile.F90', 
           'str.F90', 'strvec.F90', 
           'slatershellvec.F90', 'binvec.F90', 
           'interpolator.F90'
           ],

   'slaterbasisvec.F90' => 
           [
   'types.F90', 
           'system.F90', 'int.F90', 
           'str.F90', 'textfile.F90', 
           'strvec.F90', 'slaterbasis.F90'
           ],

   'slatershell.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'intvec.F90', 
           'int.F90', 'str.F90', 
           'textfile.F90', 'strvec.F90', 
           'real.F90', 'realmat.F90'
           ],

   'slatershellvec.F90' => 
           [
   'types.F90', 
           'system.F90', 'int.F90', 
           'str.F90', 'textfile.F90', 
           'slatershell.F90'
           ],

   'spacegroup.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'int.F90', 
           'cif.F90', 'realmat3.F90', 
           'buffer.F90', 'intvec.F90', 
           'str.F90', 'textfile.F90', 
           'strvec.F90', 'binvec.F90', 
           'realmat.F90'
           ],

   'str.F90' => 
           [
   'types.F90', 
           'system.F90'
           ],

   'strvec.F90' => 
           [
           'types.F90', 'system.F90', 
           'str.F90'
           ],

   'system.F90' => 
           [
           'types.F90'
           ],

   'test.F90' => 
           [
           'types.F90', 'system.F90'
           ],

   'textfile.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'int.F90', 
           'parallel.F90', 'intmat.F90', 
           'buffer.F90', 'intvec.F90', 
           'intvecvec.F90', 'str.F90', 
           'strvec.F90', 'binvec.F90', 
           'realmat.F90', 'real.F90', 
           'cpxvec.F90', 'unitnumber.F90'
           ],

   'time.F90' => 
           [
           'types.F90', 'system.F90'
           ],

   'unitcell.F90' => 
           [
           'types.F90', 'system.F90', 
           'realvec.F90', 'str.F90', 
           'textfile.F90', 'real.F90', 
           'cif.F90'
           ],

   'unitnumber.F90' => 
           [
           'types.F90', 'system.F90'
           ],
);


sub invoke {
    my ($me) = @_;
    my $exe = $me->exe_file;
    my @rc;

    # There's always at least one command to run
    push (@rc, { 'command' => $exe, 
                 'args'    => [ ], 
                 'output'  => "tonto.out",
                 'error'   => "tonto.err",
                });

    # Allow for at least the possibility of running other jobs, even though
    # that would almost certainly require a change to the Fortran code.
    for ($me->input_files_base) {
        next if (m#[\\/]#o);    # Exclude files in subdirectories
        if (($name) = m/(.*)\.in$/) {
            push (@rc, { 'command' => $exe, 
                         'args'    => [ ], 
                         'input'   => $_,
                         'output'  => "$name.out",
                         'error'   => "$name.err",
                        });
        }
    }
    return @rc;
}

1;
