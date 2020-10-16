!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       TIMER - A timer
!
! DESCRIPTION
!       Real time clock in seconds
!
! METHOD
!       Uses the f90 intrinsic system_clock. 
!
! SYNOPSIS
!       call timer(return_time, initial_time)
!         real, intent(in)  :: initial_time
!         real, intent(out) :: return_time
!
! HISTORY
!       Version       Date                 Name
!       Comments
!       -------------------------------------------
!       $Log: timerRoutine.f90,v $
!       Revision 1.1  2003/09/23 14:06:51  ulfa
!       Initial version.
!
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE timer(return_time, initial_time)

IMPLICIT NONE

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

real, intent(in)  :: initial_time
real, intent(out) :: return_time

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: count, rate, max

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

!SPEC: SPEC does its own timing, so the following is commented out.
!But if it is desired to turn it back on (for example, during a
!debugging effort), then comment the following two lines and uncomment
!the third.  jh/12/feb/2004, 11/sep/2004
rate=0
count=0
!call system_clock( COUNT=count, COUNT_RATE=rate, COUNT_MAX=max )

if (rate/=0) then
  return_time = real(count)/real(rate) - initial_time 
else
  return_time = 0.0
  write(*,*) 'WARNING! No clock available (in timerRoutine.f90)'
end if

END SUBROUTINE timer
