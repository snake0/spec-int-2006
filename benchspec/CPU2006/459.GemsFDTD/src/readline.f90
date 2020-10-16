!+
! NAME
!       Readline_mod - Module for reading lines in input files
!
! DESCRIPTION
!       Module for reading lines in input files
!
! PUBLIC
!       SUBROUTINE readline
!       
! HISTORY
!       Version       Date                 Name
!       Comments
!       -------------------------------------------
!	$Log: readline.f90,v $
!	Revision 1.1  2003/09/23 14:06:51  ulfa
!	Initial version.
!	
!-     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE Readline_mod

IMPLICIT NONE

PUBLIC readline

PRIVATE

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME   
!       Readline - Reads the next non empty line from an input file 
!
! DESCRIPTION
!       Reads lines from an opened input file until a non empty line is found.
!       A line consisting of only comments is considered empty. The unit number
!       of the file is an optional parameter. It is set to 9 if it is not
!       present in the call.
!
!       If the non empty line matches one of the keywords in the second
!       parameter (an array of character(line_length)) it sets line='' and 
!       goes back one record. The reason for this procedure is: this routine
!       may be called from one of the Read_* SUBROUTINES in readdata_mod
!       which is searching for a possible Secondary keyword. If a PRIMARY 
!       keyword is found, then the Read_* will finish and return to the
!       subroutine Parser. Parser must now be able to reread the PRIMARY
!       keyword.
!
!       If EOF is found it sets line = 'EOF'.
!
!       Any non empty line that is found is interpreted as data and returned
!       (as line). The receiving routine is responsible for deciding if the 
!       format of the data is OK. 
!
! METHOD
!       Comments and horizontal tab:s are replaced by blanks. Leading blanks 
!       are removed. 
!
! SYNOPSIS
!       CALL Readline(line_length,keywords,line,unit)
!         integer                                          :: line_length
!         character(line_length), dimension(:), intent(in) :: keywords
!         character(line_length), intent(out)              :: line
!         integer, optional                                :: unit
!  
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Lennart Hellström and Ulf Andersson
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Readline(line_length,keywords,line,unit)

!------------------------------------------------------------------------------
!                     A r g u m e n t s
!------------------------------------------------------------------------------

integer                                          :: line_length
character(line_length), dimension(:), intent(in) :: keywords
character(line_length), intent(out)              :: line
integer, optional                                :: unit

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer      :: ii, ios
integer      :: unit_number 
character(7) :: format_string
character(4) :: char_length

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

if (present(unit)) then
  unit_number = unit
else
  unit_number = 9
end if

write(char_length,'(I4)') line_length
format_string = '(A' // trim(char_length) // ')'

do 
  read(unit_number,format_string,IOSTAT=ios) line
  !! End of line or other error when ios<0 , return from subroutine.
  !! We can assume that the only possible error at this level will be
  !! end of file due to the fact that we are only reading characters.

  !! Assuming the above turned out wrong !
  !! An ios greater than 0 was returned when no EOL was entered
  !! on the Sun/4.0 compiler. 
  if (ios < 0) then     
    line = 'EOF'
    exit
  elseif (ios > 0) then
    write(*,*) 'Warning! Something wrong in file with unit = ', unit_number
    write(*,*) 'ios = ', ios
  end if

  !! If comment found replace everything from '!' with blanks
  if (index(line,'!')>0) then
    line(index(line,'!'):line_length)=repeat(' ',line_length-index(line,'!')+1)
  end if

  !! Replace horizontal tabs (ASCII code 9) with blanks (ASCII code 32)
  !! This is performed since SGI machines crash when opening files
  !! with a tab in the file name. (This was the first bug reported 
  !! by the end users. It was reported by Stefan Persson, ESB.)  
  do ii=1,line_length
    if (IACHAR(line(ii:ii))==9) then
      line(ii:ii) = ' '
    end if
  end do

  !! Make all leading blanks trailing. 
  line = adjustl(line)
  !! There is no point in trimming since line=trim(line) does not change line
!                                                                   ! (ulfa)
  !! Check if we have found a keyword.
  if (checkifkeyword(line_length,line, keywords)) then 
    backspace(unit_number) ! Keyword found, go back one record and return
    line = ''       
    exit
  end if

  if (line/='') then    ! Data found, return.
    exit
  end if
end do

END SUBROUTINE Readline

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Check_if_keyword - Checks if a read line is a keyword
!
! DESCRIPTION
!       Checks if a read line is a keyword
!      
! SYNOPSIS
!       CALL Check_if_keyword(line_length,line,keywords)
!         integer, intent(in)                  :: line_length
!         character(line_length), intent(out)  :: line
!         character(line_length), dimension(:) :: keywords
! 
! RETURN VALUE
!       The value returned tells if the variable line is a keyword
!       Line is a keyword if the function returns .TRUE.
!
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Lennart Hellström
!       Parameter line_length added by ulfa 2000-03-03
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

LOGICAL FUNCTION Checkifkeyword(line_length,line,keywords)

!------------------------------------------------------------------------------
!                     A r g u m e n t s
!------------------------------------------------------------------------------

integer, intent(in)                              :: line_length
character(line_length), intent(in)               :: line
character(line_length), dimension(:), intent(in) :: keywords

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: i, nkeywords

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

nkeywords = size(keywords)
checkifkeyword = .FALSE.

do i=1,nkeywords
  if (line==keywords(i)) then
    checkifkeyword = .TRUE.
    exit
  end if
end do

END FUNCTION CheckIfKeyword

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE Readline_mod
