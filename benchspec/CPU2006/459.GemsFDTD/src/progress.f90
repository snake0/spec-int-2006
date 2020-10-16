!+
! NAME
!       progress_mod - A progress module
!
! DESCRIPTION
!       Module for checking the progress. Presently it contains one PUBLIC
!       subroutine which writes messages to standard out when a certain time 
!       has passed and/or a certain number of iterations have been taken.
!
! PUBLIC
!       SUBROUTINE Progress
!       real(kind=rfp) :: progress_check_no   
!       logical        :: progress_awtic
!       logical        :: progress_awtwb
!
! HISTORY
!       Version       Date                 Name
!       Comments
!       -------------------------------------------
!	$Log: progress.f90,v $
!	Revision 1.3  2005/04/05 10:33:42  ulfa
!	Superfluous lines have been removed.
!	
!	Revision 1.2  2003/09/23 14:13:56  ulfa
!	Removed timer from progress.f90 (i.e. Removed keyword PROGRESS/Time)
!	
!	Revision 1.1  2003/09/23 14:06:51  ulfa
!	Initial version.
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE progress_mod
USE parameter_mod, ONLY : rfp

IMPLICIT NONE

PUBLIC progress

real(kind=rfp), PUBLIC :: progress_check_no   = 0.0_rfp 
logical,        PUBLIC :: progress_awtic      = .false.
logical,        PUBLIC :: progress_awtwb      = .false.

PRIVATE

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Progress - A progress module
!
! DESCRIPTION
!       Writes messages to standard out when a certain time has passed and/or
!       a certain number of iterations have been taken.
!
! METHOD
!       See comments in the code.
!
! SYNOPSIS
!       CALL progress(ts)
!         integer, intent(in)  :: ts
!
! ERRORS
!       Not handled
!
! HISTORY
!       Written by Lennart Hellström
!       Further developed by Ulf Andersson
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE progress(ts) 

USE globalvar_mod, ONLY : nts

!------------------------------------------------------------------------------
!                     A r g u m e n t s
!------------------------------------------------------------------------------

integer, intent(in)  :: ts

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

if (progress_check_no >= 1.0_rfp) then
  if (mod(ts, NINT(progress_check_no)) == 0 ) then
    write(*,*) 'Iteration no. ', ts, ' has been completed'
  end if
else if (progress_check_no < 1.0_rfp .AND. progress_check_no > 0.0_rfp) then
! If progress_check_no<1, it should be interpreted as a percentage of 
! the total number of time steps (ulfa). The max function is needed to avoid
! mod(ts,0) when nts*progress_check_no<1.
  if (mod(ts, max(1,NINT(nts*progress_check_no))) == 0 ) then
    write(*,*) 'Iteration no. ', ts, ' has been completed'
  end if
end if

END SUBROUTINE progress

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE progress_mod
