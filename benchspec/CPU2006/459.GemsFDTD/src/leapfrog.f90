!+
! NAME
!       leapfrog_mod - Leap-frog module
!
! DESCRIPTION
!       Contains the Subroutine leapfrog       
!
! PUBLIC
!       SUBROUTINE leapfrog
!
! HISTORY
!       Version       Date                 Name
!       Comments
!       -------------------------------------------
!	$Log: leapfrog.f90,v $
!	Revision 1.4  2003/09/26 22:05:21  ulfa
!	Removed a missplaced comment.
!	
!	Revision 1.3  2003/09/26 21:50:38  ulfa
!	Improved sdout info.
!	
!	Revision 1.2  2003/09/26 20:15:18  ulfa
!	Added global computation of #bytes allocated.
!	
!	Revision 1.1  2003/09/23 14:06:51  ulfa
!	Initial version.
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE leapfrog_mod

IMPLICIT NONE

PUBLIC leapfrog

PRIVATE 

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       leapfrog - Leap-frog scheme
!
! DESCRIPTION
!       1) calls init routines for the modules used
!       2) allocate the field variables
!       3) performs explicit timestepping for nts time steps
!       4) writes desired output
!       5) calls end routines for the modules used
!
! METHOD
!       During timestepping these SUBROUTINES are called
!          timer                          if (ts==2)
!          updateH_homo                   (always)
!          UPMLupdateH                    if (OBC_Type==OBC_UPML)
!          HuygensH                       if (HuyPulsetype>0)
!          ------------- Formally, a half time step is taken here -------------
!          updateE_homo                   (always)
!          UPMLupdateE                    if (OBC_Type==OBC_UPML)
!          HuygensE                       if (HuyPulsetype>0)
!          PECApply                       if PEC_found
!          NFT_Store                      if (NF_Type==2)
!          progress                       (always)
!
! SYNOPSIS
!       CALL leapfrog(nx,ny,nz)
!         integer, intent(in) :: nx, ny, nz
!
! ERRORS
!       Uses the errorcheck module
!
! HISTORY
!       Written by Ulf Andersson
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE leapfrog(nx,ny,nz)
USE errorcheck_mod, ONLY : check_allocate, check_deallocate, AllocStatus
USE errorcheck_mod, ONLY : fatal, warning
USE parameter_mod, ONLY  : rfp, sfp, dfp, bytes_per_float
USE huygens_mod, ONLY    : HuyPulsetype, HuygensH, HuygensE, Huygens_end
USE update_mod, ONLY     : Update_init, UpdateH_homo, UpdateE_homo
USE UPML_mod, ONLY       : UPMLinit, UPMLend, UPMLupdateH, UPMLupdateE
USE PEC_mod, ONLY        : PEC_found, PECinit, PECend, PECapply
USE NFT_mod, ONLY        : NFT_Init, NFT_Store, NFT_Print
USE calcflops_mod, ONLY  : calcflops
USE globalvar_mod, ONLY  : dx, dy, dz, nts, dt,                               &
                           xstop, xstart, ystop, ystart, zstop, zstart,       &
                           OBC_Type, OBC_UPML, NF_Type, bytes_allocated
USE progress_mod, ONLY   : progress, progress_awtic, progress_awtwb

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: nx, ny, nz

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e st
!------------------------------------------------------------------------------

integer        :: ts  ! time step
integer        :: allocstat
integer        :: nf_Err ! Near-to-far-field error variable

real(kind=sfp) :: t_start, t_final

! Field variables
real(kind=rfp), dimension(:,:,:), allocatable :: Ex, Ey, Ez, Hx, Hy, Hz
integer :: bytes, bytes2

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------
!
! Initialize leap-frog update 
!
CALL Update_init()
!
! Initialize PEC
!
if (PEC_found) then
  CALL PECinit(nx,ny,nz)
end if
!
! Initialize ABC
if (OBC_Type==OBC_UPML) then
  CALL UPMLinit(nx,ny,nz)
end if
!
! Initialize near-to-far-field transforms
!
if (NF_Type==2) then
  CALL NFT_Init(nx,ny,nz,nf_Err)
end if
if (nf_Err/=0) then
  write(*,*) 'FATAL ERROR in SUBROUTINE NFT_Init, nf_Err = ', nf_Err
  write(*,*) '(see comments in NFT_init for an explanation)'
  stop
end if
!
! Allocate memory for the electromagnetic fields. Do this last so that other
! initializations may use temporary memory. Note that we waste some memory
! when allocating the E fields. The following fields are never used:
!   Ex(   xstop+1    ,ystart:ystop+1,zstart:zstop+1)
!   Ey(xstart:xstop+1,   ystop+1    ,zstart:zstop+1)
!   Ez(xstart:xstop+1,ystart:ystop+1,   zstop+1    )
! By doing this we get the same size for all E fields.
!
write(*,'(A,I3)') ' Size of leading dimension for H-fields = ', xstop+1-xstart

allocate( Hx(xstart:xstop  ,ystart:ystop  ,zstart:zstop  ), STAT=allocstat )
call check_allocate(allocstat, 'Hx', fatal)

allocate( Hy(xstart:xstop  ,ystart:ystop  ,zstart:zstop  ), STAT=allocstat )
call check_allocate(allocstat, 'Hy', fatal)

allocate( Hz(xstart:xstop  ,ystart:ystop  ,zstart:zstop  ), STAT=allocstat )
call check_allocate(allocstat, 'Hz', fatal)

write(*,'(A,I3)') ' Size of leading dimension for E-fields = ', xstop+2-xstart

allocate( Ex(xstart:xstop+1,ystart:ystop+1,zstart:zstop+1), STAT=allocstat )
call check_allocate(allocstat, 'Ex', fatal)

allocate( Ey(xstart:xstop+1,ystart:ystop+1,zstart:zstop+1), STAT=allocstat )
call check_allocate(allocstat, 'Ey', fatal)

allocate( Ez(xstart:xstop+1,ystart:ystop+1,zstart:zstop+1), STAT=allocstat )
call check_allocate(allocstat, 'Ez', fatal)

bytes = (xstop-xstart+1)*(ystop-ystart+1)*(zstop-zstart+1)*3 + & ! H-fields   &
        (xstop-xstart+2)*(ystop-ystart+2)*(zstop-zstart+2)*3     ! E-fields 
bytes = bytes*bytes_per_float
bytes_allocated = bytes_allocated + bytes
write(*,*) 'Fields allocated in leapfrog_mod, bytes used = ', bytes
bytes2 = (nx*ny*nz*3 + (nx+1)*(ny+1)*(nz+1)*3)*bytes_per_float
write(*,*) '(', bytes-bytes2, ' bytes of this is due to UPML)'
write(*,*)
write(*,'(A,I5)') ' Total amount of Mbytes allocated = ',                     &
           bytes_allocated/(1024*1024)
write(*,*)
!
! Initialize the fields 
!
Ex = 0.0_rfp ; Ey = 0.0_rfp ; Ez = 0.0_rfp
Hx = 0.0_rfp ; Hy = 0.0_rfp ; Hz = 0.0_rfp

! Only perform initialization. This is useful if a flop counter tool is used.
if (progress_awtwb) then                                             ! (ulfa)
  write(*,*) 'TERMINATING, because PROGRESS/Awtwb was set'
  stop
end if

!-------------- TIMESTEPPING -----------------

do ts = 1,nts

  if (ts==1) then
    write(*,*) '============================================================'
    write(*,*) 'Entering timestepping loop'
    write(*,*) 'Total number of iterations are ', nts
  end if

! Start timer. The first time step may differ from the others in performance,
! since it does not have any preceding time step. We do not want to pollute
! the timing by including it.
  if (ts==2) then                                    
    write(*,*) 'Starting timer in the beginning of the second time step'
!   !SPEC: no, don't; SPEC prefers to do its own timing jh/12/feb/04
!   !CALL timer(t_start,0.0_sfp)
  end if

  CALL updateH_homo(nx,ny,nz,Hx,Hy,Hz,Ex,Ey,Ez)

  if (OBC_Type==OBC_UPML) then
    CALL UPMLupdateH(nx,ny,nz,Hx,Hy,Hz,Ex,Ey,Ez)
  end if
  
  if (HuyPulsetype>0) then
    CALL HuygensH(ts,Hx,Hy,Hz)
  end if

!
!-------------- Formally, a half time step is taken here -----------------
!

  CALL updateE_homo(nx,ny,nz,Hx,Hy,Hz,Ex,Ey,Ez)

  if (OBC_Type==OBC_UPML) then
    CALL UPMLupdateE(nx,ny,nz,Hx,Hy,Hz,Ex,Ey,Ez)
  end if

  if (HuyPulsetype>0) then
    CALL HuygensE(ts,Ex,Ey,Ez)
  end if

  if (PEC_found) then
    CALL PECapply(Ex,Ey,Ez)
  end if

  if (NF_Type==2) then
    CALL NFT_Store(Ex,Ey,Ez,Hx,Hy,Hz,(ts-1)*dt,ts)
  end if

  CALL progress(ts)

end do ! Main loop ends here
! =+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Do not insert anything here! It would disrupt the timing.
if (nts>1) then              ! Timer not started if we only made one time step!
!  !SPEC  CALL timer(t_final,t_start)! Stop the timer 
end if

write(*,*) 'Ending timestepping loop' 

if (nts>1) then  ! Calculate the performance
!  !SPEC CALL calcflops(nx,ny,nz,t_final)
end if

!--------------------- Output and deallocation --------------------------------

! Excluding post-processing. This is useful if a flop counter tool is used.
if (progress_awtic) then                                           ! (ulfa)
  write(*,*) 'TERMINATING, because PROGRESS/Awtic was set'
  stop
end if

! Output and deallocation for near-to-far-field transforms.
if (NF_Type==2) then
  CALL NFT_Print()
end if

!--------------------- Deallocate memory --------------------------------------


deallocate( Ex, STAT=allocstat )
call check_deallocate(allocstat, 'Ex', warning)
deallocate( Ey, STAT=allocstat )
call check_deallocate(allocstat, 'Ey', warning)
deallocate( Ez, STAT=allocstat )
call check_deallocate(allocstat, 'Ez', warning)

deallocate( Hx, STAT=allocstat )
call check_deallocate(allocstat, 'Hx', warning)
deallocate( Hy, STAT=allocstat )
call check_deallocate(allocstat, 'Hy', warning)
deallocate( Hz, STAT=allocstat )
call check_deallocate(allocstat, 'Hz', warning)

call UPMLend

if (HuyPulsetype>0) then
  call Huygens_end
end if

if (PEC_found) then
  call PECend
end if

CALL AllocStatus

END SUBROUTINE leapfrog

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE leapfrog_mod
