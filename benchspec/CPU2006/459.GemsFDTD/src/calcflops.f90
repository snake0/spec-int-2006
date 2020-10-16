!+
! NAME
!       calcflops_mod - performance calculation
!
! DESCRIPTION
!       Module for calculating the performance of the code 
!
! PUBLIC
!       Subroutine calcflops
!
! HISTORY
!
!       Version       Date                 Name
!       Comments
!       -------------------------------------------
!	$Log: calcflops.f90,v $
!	Revision 1.1  2003/09/23 14:06:50  ulfa
!	Initial version.
!	
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE calcflops_mod

IMPLICIT NONE

PUBLIC calcflops

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       calcflops - Calculate Flop/s
!
! DESCRIPTION
!       Calculate the performance of the code in MFlop/s
!
! METHOD
!       Given nx, ny, nz and nts, the number of floating-point operations
!       (FLOP:s) performed in update, huygens, ABC and near-to-far-field 
!       modules is calculated. This is then divided with the results of the 
!       timing (t_final) to get the performance.
!
!       The calculation of FLOP:s for update, huygens, mur and PML has been 
!       confirmed by tests on the Cray J932 using the hpm tool. This was
!       done in fridas predecessor pscyee when we did not use modules.
!
!       The calculation of FLOP:s for the near-to-far-field transforms was
!       confirmed by using hpmcount on the IBM pwr3.
!
!       complex multiplication is counted as 6 FLOP:s
!       complex additions is counted as 2 FLOP:s
!
!       exp, sin, cos, divide and square root are all counted as one FLOP.
!
! SYNOPSIS
!       < Start clock in the beginning of the second time step with
!       CALL timer(t_start, 0.0) >
!       < After completion of the timestepping, stop the clock with
!       CALL timer(t_final, t_start) >
!       CALL calcflops(nx,ny,nz,t_final)
!         integer, intent(in) :: nx, ny, nz
!         real, intent(in)    :: t_final
!
!       If t_final<=0 we assume that no timing has been performed
!
! SEE ALSO
!       timerRoutine.f90
!
! HISTORY
!       Written by Ulf Andersson
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE calcflops(nx,ny,nz,t_final)

USE globalvar_mod, ONLY : nts, NF_Type, OBC_Type, OBC_UPML
USE parameter_mod, ONLY : rfp, sfp, dfp
USE huygens_mod, ONLY   : Huy_db, HuyPulsetype
USE UPML_mod, ONLY      : UPML_get_pml_cells
USE NFT_mod, ONLY       : NFT_get_Nangle_and_NFT_db

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in)        :: nx, ny, nz
real(kind=sfp), intent(in) :: t_final

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: nxl, nyl, nzl, nxp, nyp, nzp
integer :: H_Flop_per_cell, E_Flop_per_cell,                                  &
           UpdEFlop, UpdHFlop, UpdFlop, ABCFlop,                              &
           HuyFlop, excflop,                                                  &
           UPMLceller, NF_Flop, totFlop
real(kind=rfp) :: MFlops
integer :: calc_pml_cells 
integer :: Nangle, NF_db

!------------------------------------------------------------------------------
!                     F o r m a t  s t r i n g s
!------------------------------------------------------------------------------

character (len=*), parameter :: F200='(A,I12,A)'

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

nxl = nx      ! In the calculations below all variables on
nyl = ny      ! the r.h.s. must be of type integer(kind=8)
nzl = nz
!--- Update --- number of a.o. ---
E_Flop_per_cell = 18
H_Flop_per_cell = 18
UpdEFlop = (nyl-1)*(nzl-1) * E_Flop_per_cell/3 + & ! Leftovers in calcmet = 5 &
           (nxl-1)*(nzl-1) * E_Flop_per_cell/3 + & ! ---------- = ----------  &
           (nxl-1)*(nyl-1) * E_Flop_per_cell/3 + & ! ---------- = ----------  &
           (nxl-1)*(nyl-1) * (nzl-1) * E_Flop_per_cell   ! The fused loops     
UpdHFlop = nxl*nyl*nzl * H_Flop_per_cell
UpdFlop  = UpdEFlop + UpdHFlop
!--- ABC --- number of a.o. ---
select case (OBC_Type)
  case(OBC_UPML)
    calc_pml_cells = UPML_get_pml_cells()
    nxp = nxl+2*calc_pml_cells
    nyp = nyl+2*calc_pml_cells
    nzp = nzl+2*calc_pml_cells
    UPMLceller = nxp*nyp*nzp - nxl*nyl*nzl
    ABCFlop = UPMLceller*90   ! (8+7)*6 a.o. in all UPML cells
  case default ! PEC and PMC
    ABCFlop = 0
  end select

!--- Huygens --- number of a.o. ---  
if (HuyPulsetype<=0) then    ! No huygens
  HuyFlop = 0
else ! Assumes no_Huy_applies==1. Assumes that the source term is evaluated at 
     ! all places during all time step (i.e. ignoring the where statements)
  select case (HuyPulsetype) ! Comparisons not counted.
  case (1)
    excflop = 2 ! sin counted as one a.o.
  case (2)
    excflop = 2 ! cos counted as one a.o.
  case (3)
    excflop = 0
  case (4)
    excflop = 12 ! 3 cos counted as one a.o. each.
  case (5)
    excflop = 13 ! sin and 3 cos counted as one each.
  case (6)
    excflop = 5 ! exp, div and **2 counted as one a.o. each. negation counted! 
  case (7)                           
    excflop = 8 ! sin, exp, div and **2 counted as one a.o. each.
  case (8)                           
    excflop = 5 ! 3 exp counted as one a.o. each.
  case (9)                           
    excflop = 2 ! sin counted as one a.o.
  case (10)                          
    excflop = 2 ! sin counted as one a.o. Ramp ignored!
  case (11)                          
    excflop = 7 ! exp, div and **2 counted as one a.o. each. 
  case (12)                          
    excflop = 2 ! sin counted as one a.o.
  case (13)                          
    excflop = 2 ! sin counted as one a.o. Ramp ignored!
  case (14)                          
    excflop = 9 ! NOT correctly counted!!
    write(*,*) 'NOTE! Using approximate value (9) for excflop in calcflops!'
  case (15)                          
    excflop = 6
  case default
    excflop = 2
    write(*,*) 'NOTE! Using default value (2) for excflop in calcflops!'
  end select
  HuyFlop = (nyl-2*Huy_db+2) * (nzl-2*Huy_db+1) * 4*(excFlop+3) +             &
            (nyl-2*Huy_db+1) * (nzl-2*Huy_db+2) * 4*(excFlop+3) +             &
            (nxl-2*Huy_db+2) * (nzl-2*Huy_db+1) * 4*(excFlop+3) +             &
            (nxl-2*Huy_db+1) * (nzl-2*Huy_db+2) * 4*(excFlop+3) +             &
            (nxl-2*Huy_db+2) * (nyl-2*Huy_db+1) * 4*(excFlop+3) +             &
            (nxl-2*Huy_db+1) * (nyl-2*Huy_db+2) * 4*(excFlop+3)    
            ! excFlop+3 a.o. Two subs/adds and one mult equals 3 a.o.
            ! I do not include FLOPS that the compiler ought to get rid of
            ! d[xyz]inv*dtd[mu/eps][EH]pol
end if

!--- near-to-far-field transforms --- number of a.o. ---  
if (NF_Type==2) then
  call NFT_get_Nangle_and_NFT_db(Nangle,NF_db)
  !! Only FLOP:s _in_ the direction loop in NFT_Store in NFT.f90 are included
  NF_Flop=( 30*((nxl-2*NF_db+1)*(nzl-2*NF_db+2)   &  ! Ex and Hz on Y1 and Y2 &
              + (nzl-2*NF_db+1)*(nxl-2*NF_db+2)   &  ! Ez and Hx on Y1 and Y2 &
              + (nyl-2*NF_db+1)*(nzl-2*NF_db+2)   &  ! Ey and Hz on X1 and X2 &
              + (nzl-2*NF_db+1)*(nyl-2*NF_db+2))+ &  ! Ez and Hy on X1 and X2 &
               ((nxl-2*NF_db+1)*(nyl-2*NF_db+2)   &  ! Ex and Hy on Z1 and Z2 &
              + (nyl-2*NF_db+1)*(nxl-2*NF_db+2))  &  ! Ey and Hx on Z1 and Z2 &
            *40) * Nangle
else
  NF_Flop = 0
end if

!--- Total number of a.o. in one iteration ---  
totFlop = UpdFlop+ABCFlop+HuyFlop+NF_flop

write(*,'(A)') '-----------------------------------------------------'
write(*,'(A)') 'calcflops:'
write(*,F200) '   Number of a.o. per time step (Upd) =', UpdFlop, ' Flop.'
write(*,F200) '   Number of a.o. per time step (ABC) =', ABCFlop, ' Flop.'
write(*,F200) '   Number of a.o. per time step (Huy) =', HuyFlop, ' Flop.'
write(*,F200) '   Number of a.o. per time step (NFX) =', NF_Flop, ' Flop.'
write(*,F200) '   Number of a.o. per time step (tot) =', totFlop, ' Flop.'
if (t_final>0) then
  !! Clock is started in second step!
  !! Use of real is to avoid integer overflow when using integer(kind=4)
  MFlops = real(totFlop)*real(nts-1)/(t_final*1000000) 
  write(*,'(A,F8.2,A)') '    PERFORM. FOR MAINLOOP =', MFlops, ' MFlop/s.'
  if (t_final<100000.0) then
    write(*,  '(A,F8.2,A)') '    USERTIME FOR MAINLOOP =', t_final, ' sec.'
  else
    write(*,  '(A,F8.0,A)') '    USERTIME FOR MAINLOOP =',                    &
                            t_final/60.0, ' min.'
  end if
end if
write(*,'(A)') ' - - - - - - - - - - - - - - - - - - - - - - - - - - '

!--- Write warning messages ---  
if (HuyPulsetype>0) then
  write(*,*) 'Note that #FLOP:s performed in huygens are only estimated'
end if

if (OBC_Type==OBC_UPML) then
  write(*,*) 'Note that the number of FLOP:S for UPML is only approximate'
end if

write(*,'(A)') '-----------------------------------------------------'

END SUBROUTINE calcflops

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE calcflops_mod
