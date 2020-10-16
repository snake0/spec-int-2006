!+
! NAME
!       GemsFDTD - PSCI 3D FDTD (Yee scheme) code for the Maxwell equations
!
! BACKGROUND
!  
!       GemsFDTD is based on the PSCI FD-TD 3D code pscyee developed in the
!       Large Scale FD-TD project. It was mainly written by Ulf Andersson(ulfa)
!       and Gunnar Ledfelt. Features available in pscyee is:
!
!          Yee's Leap-frog scheme in 3D
!          Huygens' surfaces
!          UPML according Gedney
!          A time-domain near-to-far-field transformation
!
! DESCRIPTION
!       The bases of the FD-TD method is thoroughly described in the book by 
!       Taflove. The details for each part are described in their modules.
!  
! METHOD
!       First calls parser which read the main input file and then calls
!       leapfrog which performs the timestepping. This is surrounded by a
!       timing of the entire code.
!
! FILES
!       The main input file is read by parser. It must be called yee.dat
!       It may point to further input files for PEC.
!
! ERRORS
!       No error handling
!
! SEE ALSO
!       leapfrog_mod and readdata_mod
!       The book by Allen Taflove: "Computational Electrodynamics: 
!       The Finite-Difference Time-Domain Method", Artech House, 2000.
!
! HISTORY
!       Version       Date                 Name
!       Comments
!       -------------------------------------------
!	$Log: GemsFDTD.f90,v $
!	Revision 1.2  2003/09/26 20:04:13  ulfa
!	Added reference in comment to Tafloves book.
!	
!	Revision 1.1  2003/09/23 14:06:51  ulfa
!	Initial version.
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

PROGRAM GemsFDTD

USE parameter_mod, ONLY : sfp
USE readdata_mod, ONLY  : parser, parser_file
USE leapfrog_mod, ONLY  : leapfrog

IMPLICIT NONE

!------------------------------------------------------------------------------
!                     V a r i a b l e s
!------------------------------------------------------------------------------

integer              :: nx = 20             ! Grid size in X direction
integer              :: ny = 20             ! Grid size in Y direction
integer              :: nz = 20             ! Grid size in Z direction
real(kind=sfp)       :: ti, tf              ! Timing variables

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

! To see whether the code was even started before it crashes! (ulfa)
write(*,*) 'Welcome to GemsFDTD'
write(*,*)
 
!SPEC: remove timer: SPEC does its own  - jh/12/feb/04
!CALL timer(ti, 0.0_sfp)

parser_file = 'yee.dat'
CALL parser(nx,ny,nz)

CALL leapfrog(nx,ny,nz)

!SPEC: remove timer: SPEC does its own  - jh/12/feb/04
!CALL timer(tf, ti) 
!
!write(*,'(A,F10.2,A)') '   (GemsFDTD: The entire code took ', tf, ' s.)'
!if (tf<0.0) then
!  write(*,*) 'Time is negative!, tf = ', tf
!end if

END PROGRAM GemsFDTD
