!+
! NAME
!       Errorcheck_mod - Error handling module
!
! DESCRIPTION
!       Uses IOSTAT and ALLOCSTAT to handle errors
!
! PUBLIC
!       SUBROUTINE Check_open
!       SUBROUTINE Check_close
!       SUBROUTINE Check_read
!       SUBROUTINE Check_write
!       SUBROUTINE Check_allocate
!       SUBROUTINE Check_deallocate
!       SUBROUTINE AllocStatus
!
!
! HISTORY
!       Version       Date                 Name
!       Comments
!       -------------------------------------------
!       Written by by Lennart Hellström
!-     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE Errorcheck_mod

IMPLICIT NONE

PUBLIC  ::                                                             &
     Check_write,                                                      &
     Check_read,                                                       &
     Check_open,                                                       &
     Check_close,                                                      &
     Check_allocate,                                                   &
     Check_deallocate,                                                 &
     AllocStatus,                                                      &
     NumberOfAlloc,                                                    &
     NumberOfDealloc

!------------------------------------------------------------------------------
!                     M o d u l e   P a r a m e t e r s 
!------------------------------------------------------------------------------

integer, parameter :: WARNING = 1
integer, parameter :: NORMAL  = 2
integer, parameter :: FATAL   = 3

!------------------------------------------------------------------------------
!                     M o d u l e   V a r i a b l e s 
!------------------------------------------------------------------------------

integer, save      :: NumberOfAlloc   = 0
integer, save      :: NumberOfDealloc = 0

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       AllocStatus - Print a message about the number of allocs and deallocs
!
! DESCRIPTION
!       
!       Print a message about the number of allocated and deallocated
!       calls. 
!
!       Note:
! 
!         This assumes that check_allocate and check_deallocate are
!         called after every allocate and deallocate call.
!
! SYNOPSIS
!       CALL AllocStatus
!
! HISTORY
!       Written by Sandy Sefi 
!       Minor update by Ulf Andersson 2000-07-20
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Allocstatus

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

if (NumberOfAlloc /= NumberOfDealloc) then
  write(*,*) ' WARNING! Mismatch in number of calls to check_allocate and &
             &check_deallocate'
  write(*,*) 'diff = ', (NumberOfAlloc-NumberOfDealloc)
  write(*,*) 'NumberOfAlloc   = ', NumberOfAlloc 
  write(*,*) 'NumberOfDealloc = ', NumberOfDealloc 
else
  write(*,*)  'No. of Allocations (',NumberOfAlloc, ')/(',NumberOfDealloc,') &
             &deallocations.  Ok ' 
end if

END SUBROUTINE Allocstatus

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Check_write - Checks iostat on write
!
! DESCRIPTION
!       Checks if an error has occurred from Writing on 
!       external and internal files.
!       If ios = 0 no error 
!       If ios > 0 an error has occurred
!       If ios < 0 End of file or end of line 
!
! SYNOPSIS
!       CALL Check_write(ios,outputstr,errortype)
!         integer, intent(in) :: ios
!         integer, intent(in) :: errortype  
!         character(len=*), intent(in) :: inputstr
!
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Check_write(ios, outputstr, errortype)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: ios
integer, intent(in) :: errortype ! NORMAL, FATAL, WARNING
character(len=*), intent(in) :: outputstr

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

if (ios > 0) then
  if (errortype == FATAL) then
    write(*,*) 'EXECUTION HALTED ! (in Check_write)'
    write(*,*) 'Error during write to file ', trim(outputstr)
    write(*,*) 'The value of ios was:', ios
    stop
  end if
  if (errortype == NORMAL) then
  end if
  if (errortype == WARNING) then
    write(*,*) 'WARNING! Error during write to file ', trim(outputstr)
    write(*,*) 'The value of ios was:', ios
  end if
end if

if (ios < 0) then
  if (errortype == FATAL) then
    write(*,*) 'EXECUTION HALTED ! (in Check_write)'
    write(*,*) 'End of file or end of record found when'
    write(*,*) 'trying to write to file ', trim(outputstr)
    write(*,*) 'The value of ios was:', ios
    stop
  end if
  if (errortype == NORMAL) then
  end if
  if (errortype == WARNING) then
    write(*,*) 'WARNING !'
    write(*,*) 'End of file or end of record found when'
    write(*,*) 'trying to write to file ', trim(outputstr)
    write(*,*) 'The value of ios was:', ios
  end if
end if

END SUBROUTINE Check_write

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Check_read - Checks iostat on read
!
! DESCRIPTION
!       Checks if an error has occurred from reading on external and 
!       internal files.
!       If ios = 0 no error 
!       If ios > 0 an error has occurred
!       If ios < 0 End of file or end of line 
!
! SYNOPSIS
!       CALL Check_read(ios,outputstr,errortype)
!         integer, intent(in) :: ios
!         integer, intent(in) :: errortype  
!         character(len=*), intent(in) :: outputstr
!
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Check_read(ios, outputstr, errortype)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: ios
integer, intent(in) :: errortype ! NORMAL, FATAL, WARNING
character(len=*), intent(in) :: outputstr

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

if (ios > 0) then
  if (errortype == FATAL) then
    write(*,*) 'EXECUTION HALTED ! (in Check_read)'
    write(*,*) 'Error during read of file ', trim(outputstr)
    write(*,*) 'The value of ios was:', ios
    stop
  end if
  if (errortype == NORMAL) then
  end if
  if (errortype == WARNING) then
    write(*,*) 'WARNING! Error during read of file ', trim(outputstr)
    write(*,*) 'The value of ios was:', ios
  end if
end if

if (ios < 0) then
  if (errortype == FATAL) then
    write(*,*) 'EXECUTION HALTED ! (in Check_read)'
    write(*,*) 'ERROR! End of file or end of record found where'
    write(*,*) 'data was expected in file ', trim(outputstr)
    write(*,*) 'The value of ios was:', ios
    stop
  end if
  if (errortype == NORMAL) then
  end if
  if (errortype == WARNING) then
    write(*,*) 'WARNING! End of file or end of record found where'
    write(*,*) 'data was expected in file ', trim(outputstr)
    write(*,*) 'The value of ios was:', ios
  end if
end if

END SUBROUTINE Check_read

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Check_open
!
! DESCRIPTION
!       Checks if an error has occurred from opening an external file.
!       If ios = 0 no error 
!       If ios > 0 an error has occurred
!
! SYNOPSIS
!       CALL Check_open(ios,inputstr,errortype)
!         integer, intent(in) :: ios
!         integer, intent(in) :: errortype
!         character(len=*), intent(in) :: outputstr
!  
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Check_open(ios, outputstr, errortype)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: ios
integer, intent(in) :: errortype ! NORMAL, FATAL, WARNING
character(len=*), intent(in) :: outputstr

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

if (ios > 0 .AND. errortype == FATAL) then
  write(*,*) 'EXECUTION HALTED ! (in Check_open)'
  write(*,*) 'Application could not open file ', trim(outputstr)
  write(*,*) 'The value of ios was:', ios
  stop
end if
if (ios > 0 .AND. errortype == NORMAL) then
end if
if (ios > 0 .AND. errortype == WARNING) then
  write(*,*) 'WARNING! Program could not open file ', trim(outputstr)
  write(*,*) 'The value of ios was:', ios
end if

END SUBROUTINE Check_open

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Check_close
!
! DESCRIPTION
!       Checks if an error has occurred from operating on external and 
!       internal files.
!       If ios = 0 no error 
!       If ios > 0 an error has occurred
!
! SYNOPSIS
!       CALL Check_close(ios,inputstr,errortype)
!         integer, intent(in) :: ios
!         integer, intent(in) :: errortype
!         character(len=*), intent(in) :: outputstr
!  
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Check_close(ios, outputstr, errortype)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: ios
integer, intent(in) :: errortype ! NORMAL, FATAL, WARNING
character(len=*), intent(in) :: outputstr

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

if (ios > 0 .AND. errortype == FATAL) then
  write(*,*) 'EXECUTION HALTED ! (in Check_close)'
  write(*,*) 'Program could not close file  ', trim(outputstr)
  write(*,*) 'The value of ios was:', ios
  stop
end if
if (ios > 0 .AND. errortype == NORMAL) then
end if
if (ios > 0 .AND. errortype == WARNING) then
  write(*,*) 'WARNING! Program could not close file  ', trim(outputstr)
  write(*,*) 'The value of ios was:', ios
end if

END SUBROUTINE Check_close  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Check_allocate - Checks status of allocate
!
! DESCRIPTION
!       Checks if an error has occurred when allocating memory
!       If stat = 0 no error 
!       If stat > 0 an error has occurred
!
!       The value of stat when an error has occurred is (probably) machine
!       dependent. On IBM's XL Fortran for AIX, Version 3 Release 2, we have:
!
!         stat=1: Error in system routine attempting to do allocation
!         stat=2: An invalid data object has been specified for allocation
!         stat=3: Both error conditions 1 and 2 have occurred
!
! SYNOPSIS
!       Check_allocate(stat, outputstr, errortype)
!         integer, intent(in) :: stat
!         integer, intent(in) :: errortype
!         character(len=*), intent(in) :: outputstr
!  
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Check_allocate(stat, outputstr, errortype)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: stat
integer, intent(in) :: errortype ! NORMAL, FATAL, WARNING
character(len=*), intent(in) :: outputstr

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

if (stat /= 0 ) then 
  if (errortype == FATAL) then
    write(*,*) 'EXECUTION HALTED ! (in Check_allocate)'
    write(*,*) 'Error when allocating memory for array(s) ', trim(outputstr)
    write(*,*) 'The value of stat was:', stat
    stop
  end if
  if (errortype == NORMAL) then
  end if
  if (errortype == WARNING) then
    write(*,*) 'WARNING !'
    write(*,*) 'Error when allocating memory for array(s) ', trim(outputstr)
    write(*,*) 'The value of stat was:', stat
  end if
else
  NumberOfAlloc = NumberOfAlloc + 1
end if

END SUBROUTINE Check_allocate 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!        Check_deallocate - Checks status of allocate
!
! DESCRIPTION
!       Checks if an error has occurred when allocating memory
!       If stat = 0 no error 
!       If stat > 0 an error has occurred
!
! SYNOPSIS
!       Check_deallocate(stat, outputstr, errortype)
!         integer, intent(in) :: stat
!         integer, intent(in) :: errortype
!         character(len=*), intent(inout) :: outputstr
!  
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Check_deallocate(stat, outputstr, errortype)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: stat
integer, intent(in) :: errortype ! NORMAL, FATAL, WARNING
character(len=*), intent(in) :: outputstr

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

if (stat /= 0 ) then 
  if (errortype == FATAL) then
    write(*,*) 'EXECUTION HALTED ! (in Check_deallocate)'
    write(*,*) 'Error when deallocating memory for array(s) ', trim(outputstr)
    write(*,*) 'The value of stat was:', stat
    stop
  end if
  if (errortype == NORMAL) then
  end if
  if (errortype == WARNING) then
    write(*,*) 'WARNING !'
    write(*,*) 'Error when deallocating memory for array(s) ', trim(outputstr)
    write(*,*) 'The value of stat was:', stat
  end if
else
  NumberOfDealloc = NumberOfDealloc + 1
end if

END SUBROUTINE Check_deallocate 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE Errorcheck_mod
