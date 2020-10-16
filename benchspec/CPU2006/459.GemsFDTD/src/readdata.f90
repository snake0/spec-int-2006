!+
! NAME
!       Readdata_mod  - Module for reading the main input data file
!
! DESCRIPTION
!       Read the parser_file and performs initialization of global variables
!
! PUBLIC
!       SUBROUTINE Parser
!
! HISTORY
!       Version       Date                 Name
!       Comments
!       -------------------------------------------
!       $Log: readdata.f90,v $
!       Revision 1.4  2005/04/05 10:33:13  ulfa
!       More consistent capitalization used.
!
!       Revision 1.3  2003/09/26 19:40:48  ulfa
!       Improved keyword checking procedure.
!
!       Revision 1.2  2003/09/23 14:13:57  ulfa
!       Removed timer from progress.f90 (i.e. Removed keyword PROGRESS/Time)
!
!       Revision 1.1  2003/09/23 14:06:51  ulfa
!       Initial version.
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE Readdata_mod

USE parameter_mod, ONLY  : rfp, kfp, two_byte 
USE globalvar_mod, ONLY  : OBC_UPML
USE errorcheck_mod, ONLY : fatal, warning, check_open,check_close,            &
                           check_allocate, check_deallocate

IMPLICIT NONE

Private

Public  Parser

character(len=80), PUBLIC :: parser_file = '!'

!
! Readdata Parameters
!
integer, parameter                    :: nkeywords = 9

!
! Readdata Variables
!
real(kind=rfp)                        :: CFL
character(80), PARAMETER, dimension(1:nkeywords) :: keywords =                &
 (/'PROBLEMSIZE                                  ',                           &
   'CELLSIZE                                     ',                           &
   'NSTEP                                        ',                           &
   'PLANEWAVE                                    ',                           &
   'PROGRESS                                     ',                           &
   'OUTERBOUNDARY                                ',                           &
   'CFL                                          ',                           &
   'PEC                                          ',                           &
   'NFTRANS_TD                                   '/)

real(kind=rfp), dimension(3)          :: par_Epol
character(80)                         :: line
integer, dimension(1:nkeywords)       :: usedkeywords
logical                               :: bWrongSpell
! Beware of using dx, dy, dz in any checking operations due to the
! fact that they might not have been read yet.


CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Parser
!
! DESCRIPTION
!       Reads data from the main input file and for each found keyword it
!       calls a specific subroutine. Writes read variables to screen.
!       Initializes some variables.
!
! SYNOPSIS
!       CALL Parser(nx,ny,nz)
!         integer, intent(inout) :: nx, ny, nz
!
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Parser(nx,ny,nz)

USE parameter_mod, ONLY : eps0, mu0
USE globalvar_mod, ONLY : glo_init, OBC_Type, c0, Z0
USE huygens_mod, ONLY   : Huygens_init, HuyPulseType
USE UPML_mod, ONLY      : UPML_get_pml_cells

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(inout) :: nx, ny, nz

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer       :: ii, ios
integer       :: par_pml_cells
character(80) :: keyword

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

! Speed-of-light and impedance of vacuum
c0 = sqrt(1/(eps0*mu0)) ! 2.99792458E+8_rfp   
Z0 = sqrt(mu0/eps0)       

bWrongSpell = .false.

!----------------------- Read Indata file -------------------------------------

usedkeywords = 0

CFL = sqrt(3.0_rfp)/2.0_rfp ! default value

open(unit=9,file=parser_file, status='old', IOSTAT=ios)
call check_open(ios, parser_file,fatal)

do
  read(9,'(A80)',IOSTAT=ios) keyword
  if (ios/=0) then
    close(9)
    exit
  end if

  !! If comment found replace everything from '!' with blanks

  if (index(keyword,'!')>0) then
    keyword(index(keyword,'!'):80) =repeat(' ',80-index(keyword,'!')+1)
  end if

  !! Replace horizontal tabs (ASCII code 9) with spaces (ASCII code 32)
  !! This is performed since SGI machines crash when opening files
  !! with a tab in the file name.
  do ii=1,80
    if (IACHAR(line(ii:ii))==9) then
      line(ii:ii) = ' '
    end if
  end do

  !! Make all leading blanks trailing and then trim

  keyword = trim(adjustl(keyword))

  select case (keyword)

  case (trim(keywords(1))) ! PROBLEMSIZE
    call Read_problemsize(nx, ny, nz)
  case (trim(keywords(2))) ! 'CELLSIZE'
    call Read_cellsize
  case (trim(keywords(3))) ! 'NSTEP'
    call Read_nstep
  case (trim(keywords(4))) ! 'PLANEWAVE'
    call Read_planewave(keyword)
  case (trim(keywords(5))) ! 'PROGRESS'
    call Read_progress
  case (trim(keywords(6))) ! 'OUTERBOUNDARY'
    call Read_outerboundary
  case (trim(keywords(7))) ! 'CFL'
    call Read_cfl
  case (trim(keywords(8))) ! 'PEC'
    call Read_pec
  case (trim(keywords(9))) !'NFTRANS_TD'
    call Read_nftrans_td
  case default
    if ( .not. trim(keyword) == '' ) then
      write(*,*)
      write(*,*) 'ERROR! Keyword ', trim(keyword), ' is not recognized'
      bWrongSpell = .true.
    end if
  end select
end do

close(unit=9, IOSTAT=ios)
call check_close(ios, parser_file, warning)

! Check that all keywords were present in yee.dat. keyword no 5 (PROGRESS)
! is the only keyword that is voluntary.
write(*,*)
do ii=1,nkeywords
  if ((usedkeywords(ii)==0).and.(ii/=5)) then
    write(*,*) 'FATAL ERROR! Primary keyword ', trim(keywords(ii)),           &
               ' not present in ', parser_file
    write(*,*)
    stop
  end if
end do

write(*,*)
if (bWrongSpell) then
  write(*,*)
  write(*,*) 'Warning! Unrecognized lines encountered'
  write(*,*) 'Check for spelling errors in keywords'
end if

if (OBC_Type==OBC_UPML) then
  par_pml_cells = UPML_get_pml_cells()
else
  par_pml_cells = 0
end if
call glo_init(nx,ny,nz,par_pml_cells,CFL)
! Initialize Huygens
! Must be here and not in SUBROUTINE Read_planewave because keywords can be in
! arbitrary order and nx, ny, nz must be known before Huygens_init is called.
if (HuyPulseType>0) then
  call Huygens_init(nx,ny,nz,par_Epol)
end if

END SUBROUTINE Parser

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Read_problemsize - Reads problem size data from main input file
!
! DESCRIPTION
!       Called from Parser to read data under the keyword PROBLEMSIZE
!       Calls read_line to get the next none blank or none commented line
!       If data is found and the format is ok it sets usedkeywords=1
!
! SYNOPSIS
!       CALL Read_problemsize(nx, ny, nz)
!         integer, intent(out) :: nx, ny, nz
!
! ERRORS
!       Wrong format halts the program
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Read_problemsize(nx, ny, nz)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(out) :: nx, ny, nz

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: ios

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

call read_line
if (line/='' .and. line/='EOF') then
  read(line,*,IOSTAT=ios) nx,ny,nz
  call Check_ios(ios,'PROBLEMSIZE')
  usedkeywords(1) = 1
end if
write(*,*) '*******PROBLEMSIZE*************************'
write(*,*) 'problemsize:', nx, ny, nz

END SUBROUTINE Read_problemsize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Read_cellsize
!
! DESCRIPTION
!       Called from Parser to read data under the keyword CELLSIZE
!       Calls read_line to get the next none blank or none commented line
!       If data is found and the format is ok it sets usedkeywords=1
!
! SYNOPSIS
!       CALL Read_cellsize
!
! ERRORS
!       Wrong format halts the program
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Read_cellsize()

USE globalvar_mod, ONLY : dx, dy, dz

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: ios

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

call read_line
if (line/='' .and. line/='EOF') then
  read(line,*,IOSTAT=ios) dx, dy, dz
  call Check_ios(ios,'CELLSIZE')
  usedkeywords(2) = 1
end if
write(*,*) '*******CELLSIZE****************************'
write(*,*) 'dx:', dx
write(*,*) 'dy:', dy
write(*,*) 'dz:', dz

END SUBROUTINE Read_cellsize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Read_nstep
!
! DESCRIPTION
!       Called from Parser to read data under the keyword NSTEP
!       Calls read_line to get the next none blank or none commented line
!       If data is found and the format is ok it sets usedkeywords=1
!
! SYNOPSIS
!       CALL Read_nstep
!
! ERRORS
!       Wrong format halts the program
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Read_nstep()

USE globalvar_mod, ONLY : nts

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: ios

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

call read_line
if (line/='' .and. line/='EOF') then
  read(line,*,IOSTAT=ios) nts
  call Check_ios(ios,'NSTEP')
  usedkeywords(3) = 1
end if
write(*,*) '*******NSTEP*******************************'
write(*,*) 'nstep:', nts

END SUBROUTINE Read_nstep

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Read_planewave
!
! DESCRIPTION
!       Called from Parser to read data under the keywords PLANEWAVE and
!       INITIALFIELD.
!       Calls read_line to get the next none blank or none commented line
!       If data is found and the format is ok it sets usedkeywords=1
!
! METHOD
!       The input parameter PRIMARY is used to decide which of the two
!       possible PRIMARY keywords was found.
!
! SYNOPSIS
!       CALL Read_planewave(PRIMARY)
!         character(80) :: PRIMARY
!
! ERRORS
!       Wrong format halts the program
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Read_planewave(PRIMARY)

USE parameter_mod, ONLY : pi
USE huygens_mod, ONLY   : Huy_setparvar, Huy_db, HuyPulseType
USE excite_mod, ONLY    : excite_max_no_param, Excitation_check_keyword

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

character(80) :: PRIMARY

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer                                        :: ios, sphere_chosen
integer                                        :: parPulseType, local_PulseType
integer                                        :: no_param
real(kind=rfp), dimension(excite_max_no_param) :: par_param
real(kind=rfp)                                 :: theta, psi, phi, Emag
real(kind=rfp), dimension(3)                   :: par_wavedir
real(kind=rfp), dimension(3)                   :: par_X0
real(kind=rfp), dimension(3)                   :: local_Epol
character(len=80)                              :: secondary
real(kind=rfp)                                 :: par_phi

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

sphere_chosen =  0
par_param     = -1111.1_rfp
par_X0(1)     = -1111.1_rfp
par_X0(2)     = -1111.1_rfp
par_X0(3)     = -1111.1_rfp
Emag          =  1.0_rfp
par_phi       = -1111.1_rfp

do
  call read_line
  call Excitation_check_keyword(line,local_PulseType,no_param)
  if (local_PulseType/=0) then
    parPulseType = local_PulseType
    secondary = line
    if (no_param>0) then
      call read_line
      if (line/='' .and. line/='EOF') then
        read(line,*,IOSTAT=ios) par_param(1:no_param)   ! Read the parameters
        call Check_ios(ios,trim(PRIMARY)//', '//trim(secondary))
      end if
    end if
  else
    select case (line)
    case ('Directioncart')
      secondary = line
      call read_line
      if (line/='' .and. line/='EOF') then
        read(line,*,IOSTAT=ios)  par_wavedir(1:3)
        call Check_ios(ios,trim(PRIMARY)//', '//trim(secondary))
        if (sqrt(DOT_PRODUCT(par_wavedir,par_wavedir)) < EPSILON(0.0_rfp)) then
          write(*,*) 'ERROR! '//trim(PRIMARY)//', '//trim(secondary)
          write(*,*) 'Null-vector could not be used in this context.'
          stop
        end if
      end if
      !! Phi may be needed by NF transforms.
      if (par_wavedir(1) /= 0.0_rfp .or. par_wavedir(2) /= 0.0_rfp) then
        par_phi = acos( par_wavedir(1)/                                       &
                    sqrt(par_wavedir(1)**2+par_wavedir(2)**2) )
        if (par_wavedir(2) < 0.0_rfp) then
          par_phi = 2*pi - par_phi
        end if
        
      end if
    case ('Epolcart')
      secondary = line
      call read_line
      if (line/='' .and. line/='EOF') then
        read(line,*,IOSTAT=ios)  local_Epol(1:3)
        call Check_ios(ios,trim(PRIMARY)//', '//trim(secondary))
        if (sqrt(DOT_PRODUCT(local_Epol,local_Epol)) < EPSILON(0.0_rfp)) then
          write(*,*) 'ERROR! '//trim(PRIMARY)//', '//trim(secondary)
          write(*,*) 'Null-vector could not be used in this context.'
          stop
        end if
        sphere_chosen = 0
      end if
    case ('Directionsphr')
      secondary = line
      call read_line
      if (line/='' .and. line/='EOF') then
        read(line,*,IOSTAT=ios)  theta, phi
        call Check_ios(ios,trim(PRIMARY)//', '//trim(secondary))
        theta = theta*pi/180.0_rfp
        phi   = phi  *pi/180.0_rfp
        par_wavedir(1) = sin(theta)*cos(phi)
        par_wavedir(2) = sin(theta)*sin(phi)
        par_wavedir(3) = cos(theta)
      end if
      par_phi = phi   ! May be needed by NF transforms
    case ('Emagsphr')
      secondary = line
      call read_line
      if (line/='' .and. line/='EOF') then
        read(line,*,IOSTAT=ios) Emag
        call Check_ios(ios,trim(PRIMARY)//', '//trim(secondary))
        if (ABS(Emag) < EPSILON(0.0_rfp)) then
          write(*,*) 'ERROR! '//trim(PRIMARY)//', '//trim(secondary)
          write(*,*) 'The value 0.0 could not be used in this context.'
          stop
        end if
      end if
    case ('Epolsphr')
      secondary = line
      call read_line
      if (line/='' .and. line/='EOF') then
        read(line,*,IOSTAT=ios)  psi
        call Check_ios(ios,trim(PRIMARY)//', '//trim(secondary))
        psi = psi*pi/180.0_rfp
        local_Epol(1) = cos(psi)*cos(theta)*cos(phi) - sin(psi)*sin(phi)
        local_Epol(2) = cos(psi)*cos(theta)*sin(phi) + sin(psi)*cos(phi)
        local_Epol(3) = -1.0_rfp*cos(psi)*sin(theta)
        sphere_chosen = 1
      end if
    case ('X0')
      secondary = line
      call read_line
      if (line/='' .and. line/='EOF') then
        read(line,*,IOSTAT=ios)  par_X0(1:3)
        call Check_ios(ios,trim(PRIMARY)//', '//trim(secondary))
      end if
    case ('Disttobound')
      secondary = line
      if (PRIMARY=='INITIALFIELD') then
        write(*,*) 'Warning! Secondary keyword Disttobound is not used by '
        write(*,*) trim(PRIMARY), ' Value will be ignored.'
        call read_line
      else ! if (PRIMARY=='PLANEWAVE') then
        call read_line
        if (line/='' .and. line/='EOF') then
          read(line,*,IOSTAT=ios) Huy_db
          call Check_ios(ios,trim(PRIMARY)//', '//trim(secondary))
          if (Huy_db<3) then
            write(*,*) 'Warning! Distance to boundaries must be >= 3'
            write(*,*) 'Execution halted.'
            stop
          end if
        end if
      end if ! if (PRIMARY=='INITIALFIELD')
    case ('EOF')
      exit
    case ('')
      exit
    case default
      bWrongSpell = .true.
      write(*,*) 'Warning! The characters ',trim(line),' are not recognized as'
      write(*,*) 'a secondary or primary keyword'
    end select
  end if ! (PulseType\=0) then
end do

! Multiply with the magnitude if spherical coordinates are used
if (sphere_chosen==1) then
  local_Epol = local_Epol*Emag
end if

! Normalization of wavedirection i.e. propagationdirection of incoming wave.
par_wavedir = par_wavedir/sqrt(dot_product(par_wavedir,par_wavedir))

!Check that the E-polarization is perpendicular to the direction of propagation
if ( abs(dot_product(par_wavedir,local_Epol))/                                &
     sqrt(dot_product(local_Epol,local_Epol)) >= 1.0E-6_rfp ) then
  stop ' E-polarization is not perpendicular to direction of propagation'
end if

usedkeywords(4) = 1
!! Call Huy_setparvar to set the private variables in the Huygens module
HuyPulseType = parPulseType
par_Epol = local_Epol
call Huy_setparvar(par_wavedir, par_X0, par_param, par_phi)

write(*,*) '*******PLANEWAVE*************************'
write(*,*) 'wavedir:', par_wavedir
write(*,*) 'Epol:', par_Epol
write(*,*) 'PulseType:', HuyPulseType
write(*,*) 'param:',par_param
if (par_X0(1)/=-1111.1_rfp) then
  write(*,*) 'X0:', par_X0
end if
write(*,*) 'Disttobound:', Huy_db

END SUBROUTINE Read_planewave

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Read_outerboundary
!
! DESCRIPTION
!       Called from Parser to read data under the keyword OUTERBOUNDARY
!       Calls read_line to get the next none blank or none commented line
!       If data is found and the format is ok it sets usedkeywords=1
!
! SYNOPSIS
!       CALL Read_outerboundary
!
! ERRORS
!       Wrong format halts the program
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Read_outerboundary()

USE globalvar_mod, ONLY : OBC_Type
USE UPML_mod, ONLY      : UPML_setparvar

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer              :: ios
integer              :: par_proftype
integer              :: par_pml_cells
real(kind=rfp)       :: par_R0

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

par_proftype = 0
par_pml_cells = 0
par_R0 = 0.0_rfp

call read_line
if (line/='' .and. line/='EOF') then
  select case (line)
  case ('Upmllow')     ! uniaxial PML
    OBC_Type  = OBC_UPML
    par_pml_cells = 4
    par_R0 = 1.0_rfp
    par_proftype = 4
  case ('Upmlmed')
    OBC_Type  = OBC_UPML
    par_pml_cells = 8
    par_R0 = 0.01_rfp
    par_proftype = 4
  case ('Upmlhigh')
    OBC_Type  = OBC_UPML
    par_pml_cells = 12
    par_R0 = 0.0001_rfp
    par_proftype = 4
  case ('Upmlspec')
    OBC_Type  = OBC_UPML
    call read_line
    if (line/='' .and. line/='EOF') then
      read(line,*,IOSTAT=ios) par_pml_cells
      call Check_ios(ios,'OUTERBOUNDARY, par_pml_cells')
      if (par_pml_cells<0) then
        write(*,*) 'pml_cells must not be a negativ number'
        stop
      end if
    end if
    call read_line
    if (line/='' .and. line/='EOF') then
      read(line,*,IOSTAT=ios) par_R0
      call Check_ios(ios,'OUTERBOUNDARY, R0')
      if (par_R0 > 1) then
        write(*,*) 'R0 must not be larger than 100%'
        stop
      end if
    end if
    call read_line
    if (line/='' .and. line/='EOF') then
      read(line,*,IOSTAT=ios) par_proftype
      call Check_ios(ios,'OUTERBOUNDARY, prof_type')
    end if

  case default
    bWrongSpell = .true.
    write(*,*) 'Warning! The characters ', trim(line), ' are not recognized as'
    write(*,*) 'a secondary or primary keyword'
  end select
end if

call UPML_setparvar(par_pml_cells, par_R0, par_proftype)

usedkeywords(6) = 1

write(*,*) '*******OUTERBOUNDARY***********************'
write(*,*) '#cells:', par_pml_cells
write(*,*) 'R0: ', par_R0
write(*,*) 'proftype:', par_proftype

END SUBROUTINE Read_outerboundary



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Read_cfl
!
! DESCRIPTION
!       Called from Parser to read data under the keyword CFL
!       Calls read_line to get the next none blank or none commented line
!       If data is found and the format is ok it sets usedkeywords=1
!
! SYNOPSIS
!       CALL Read_cfl
!
! ERRORS
!       Wrong format halts the program
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Read_cfl()

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: ios

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

call read_line
if (line/='' .and. line/='EOF') then
  read(line,*,IOSTAT=ios)  CFL
  call Check_ios(ios,'CFL')
  if (CFL>=1) then
    ! If we have a stand-alone FETD problem but want to use frida anyway 
    ! we may want to have a CFL larger than one. 
    write(*,*) 'WARNING! CFL >= 1, works only for stand-alone FETD'
!    stop
  end if
  usedkeywords(7) = 1
end if
write(*,*) '*******CFL*********************************'
write(*,*) 'CFL:', CFL

END SUBROUTINE Read_cfl


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Read_nftrans_td
!
! DESCRIPTION
!       Called from Parser to read data under the keyword NFTRANS_TD
!       Calls read_line to get the next none blank or none commented line
!       If data is found and the format is ok it sets usedkeywords=1
!
! SYNOPSIS
!       CALL Read_nftrans_td
!
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Lennart Hellström, modified by Åke Rydell
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Read_nftrans_td()

USE globalvar_mod, ONLY : NF_Type
USE NFT_mod, ONLY       : NFT_setparvar

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer           :: par_NFTdb
integer           :: par_nth, par_nphi
real(kind=rfp)    :: par_th1, par_th2, par_phi1, par_phi2
character(len=80) :: par_filename
integer           :: ios
logical           :: par_monostat

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

par_nth = 1
par_nphi = 1
par_th1 = 0
par_th2 = 0
par_phi1 = 0
par_phi2 = 0
par_filename = ''
par_monostat = .false.
NF_Type = 2

do
  call read_line
  select case (line)
  case ('Disttobound')
    call read_line
    if (line/='' .and. line/='EOF') then
      read(line,*,IOSTAT=ios) par_NFTdb
      call Check_ios(ios,'NFTRANS_TD, Disttobound')
    end if
  case ('Theta_angles_no')
    call read_line
    if (line/='' .and. line/='EOF') then
      read(line,*,IOSTAT=ios) par_nth
      call Check_ios(ios,'NFTRANS_TD, Theta_angles_no')
      if (par_nth < 1) then
        par_nth = 1
      end if
    end if
  case ('Theta_interval')
    call read_line
    if (line/='' .and. line/='EOF') then
      if (par_nth == 1) then
        read(line,*,IOSTAT=ios) par_th1
        call Check_ios(ios,'NFTRANS_TD, Theta_interval')
        par_th2 = par_th1
      end if
      if (par_nth >= 2) then
        read(line,*,IOSTAT=ios) par_th1, par_th2
        call Check_ios(ios,'NFTRANS_TD, Theta_interval')
      end if
    end if
  case ('Phi_angles_no')
    call read_line
    if (line/='' .and. line/='EOF') then
      read(line,*,IOSTAT=ios) par_nphi
      call Check_ios(ios,'NFTRANS_TD, Phi_angles_no')
      if (par_nphi < 1) then
        par_nphi = 1
      end if
    end if
  case ('Phi_interval')
    call read_line
    if (line/='' .and. line/='EOF') then
      if (par_nphi == 1) then
        read(line,*,IOSTAT=ios) par_phi1
        call Check_ios(ios,'NFTRANS_TD, Phi_interval')
        par_phi2 = par_phi1
      end if
      if (par_nphi >= 2) then
        read(line,*,IOSTAT=ios) par_phi1, par_phi2
        call Check_ios(ios,'NFTRANS_TD, Phi_interval')
      end if
    end if
  case ('Monostatic')
    par_monostat = .true.
  case ('Filenamebase')
    call read_line
    if (line/='' .and. line/='EOF') then
      par_filename = line
    end if
  case ('EOF')
    exit
  case ('')
    exit
  case default
    bWrongSpell = .true.
    write(*,*) 'Warning! The characters ', trim(line), ' are not recognized as'
    write(*,*) 'a secondary or primary keyword'
  end select
end do
usedkeywords(9) = 1
call NFT_setparvar(par_NFTdb, par_nth, par_nphi, par_th1, par_th2,            &
                   par_phi1, par_phi2, par_monostat, par_filename)

write(*,*) '*******NFTRANS_TD**************************'
write(*,*) 'Disttobound:', par_NFTdb
if (par_monostat) then
  write(*,*) 'Monostatic'
else
  write(*,*) 'Theta_angles_no:', par_nth
  write(*,*) 'Theta_interval:', par_th1, par_th2
  write(*,*) 'Phi_angles_no:', par_nphi
  write(*,*) 'Phi_interval:', par_phi1, par_phi2
end if
write(*,*) 'Filenamebase: ' , trim(par_filename)

END SUBROUTINE Read_nftrans_td

SUBROUTINE Read_line(unit)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------
integer, optional :: unit
!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: ii, ios
integer :: unit_number

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

if (present(unit)) then
   unit_number = unit
else
  unit_number = 9
end if

do
  read(unit_number,'(A80)',IOSTAT=ios) line
  !! End of line or other error when ios<0 , return from subroutine.
  !! We can assume that the only possible error at this level will be
  !! end of file due to the fact that we are only reading characters.
  if (ios<0) then
    line = 'EOF'
    exit
  end if

  !! If comment found replace everything from '!' with blancs
  if (index(line,'!')>0) then
    line(index(line,'!'):80) =repeat(' ',80-index(line,'!')+1)
  end if

  !! Replace horizontal tabs (ASCII code 9) with spaces (ASCII cose 32)
  !! This is performed since SGI machines crash when opening files
  !! with a tab in the file name. (This was the first bug reported
  !! by the end users. It was reported by Stefan Persson, ESB.)
  do ii=1,80
    if (IACHAR(line(ii:ii))==9) then
      line(ii:ii) = ' '
    end if
  end do

  !! Make all leading blancs trailing and then trim
  line = trim(adjustl(line))


  if (check_if_keyword()) then
    backspace(unit_number) ! Keyword found, go back one record and return
    line = ''
    exit
  end if

  if (line/='') then    ! Data found, return.
    exit
  end if
end do

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Check_if_keyword - Checks if a read line is a keyword
!
! DESCRIPTION
!       Checks if a read line is a keyword
!
! SYNOPSIS
!       CALL Check_if_keyword(is_keyword)
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
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

LOGICAL FUNCTION Check_if_keyword()

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: i
character(len=80) temp1, temp2 ! AA Buggig SGI
!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

check_if_keyword = .FALSE.
temp1 = trim(line) ! AA Buggig SGI
do i=1,nkeywords
  temp2 = trim(keywords(i)) ! AA Buggig SGI
  if (temp1==temp2) then
!AA  if (trim(line)==trim(keywords(i))) then
    check_if_keyword = .TRUE.
    exit
  end if
end do

END FUNCTION Check_if_keyword

END SUBROUTINE Read_line

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Check_ios - Check for error while reading line
!
! DESCRIPTION
!       Checks if an error has occurred from reading the variable line
!       and stops execution if that is the case.
!
! SYNOPSIS
!       CALL Check_ios(ios,inputstr)
!         integer,intent(in) :: ios
!         character(len=*), intent(in) :: inputstr
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Check_ios(ios, inputstr)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in)           :: ios
character(len=*), intent(in) :: inputstr

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

if (ios/=0) then
  write(*,*) 'FATAL ERROR! Wrong format under keyword '//trim(inputstr)
  stop
end if

END SUBROUTINE Check_ios

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Read_pec -
!
! DESCRIPTION
!       Called from Parser to read data under the keyword PEC
!       Calls read_line to get the next none blank or none commented line
!       If data is found and the format is ok it sets usedkeywords=1
!
!       Updated by Anders Ålund to save memory by first counting the number
!       of components in the files and then allocate memory.
!
! SYNOPSIS
!       CALL Read_pec
!
! ERRORS
!       Use errorcheck_mod
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Read_Pec()

USE PEC_mod, ONLY : PECsetparsedvalues

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

type :: TPec
  integer(kind=two_byte), dimension(5) :: comp
  type(Tpec), pointer :: next
end type TPec

Type :: TPEC_file
  character(len=80)         :: name
  type(TPEC_file), pointer  :: next
End Type

type(Tpec), pointer       :: pecs, first_pec, nextpec
type(TPEC_file), pointer  :: first_file, pec_files, nextfile

integer(kind=two_byte), dimension(:,:), pointer :: exportpecs

integer :: ios, count, stat, unit
character(80) :: filename

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

nullify(first_file)
nullify(pec_files)
nullify(exportpecs)
nullify(pecs)
nullify(first_pec)
unit = 9
count = 0

do
  call read_line(unit)
  select case (line)
  case ('Filename')
    call read_files
  case ('Components')
    call read_comp
  case ('EOF')
    exit
  case ('')
    exit
  end select
end do

pec_files => first_file

do while ( associated(pec_files))
  filename = pec_files%name
  open(unit=17,file=filename, status='old', IOSTAT=ios)
  call check_open(ios, filename,fatal)
  unit = 17
  call count_comp
  close(17)
  pec_files => pec_files%next
end do

if (count == 0) then
  write(*,*) 'WARNING! No values was found under keyword PEC'
end if

usedkeywords(8) = 1

write(*,*) '***************PEC*************************'
write(*,*) 'Number of components: ', count

allocate(exportpecs(count,5), STAT=stat)
call check_allocate(stat,'exportpecs in read_pec',fatal)
exportpecs = 0
pecs => first_pec
count = 0
do while ( associated(pecs) )
  count = count + 1
  exportpecs(count,1:5) = pecs%comp(1:5)
  pecs => pecs%next
end do

pec_files => first_file

do while ( associated(pec_files))
  filename = pec_files%name
  open(unit=17,file=filename, status='old', IOSTAT=ios)
  call check_open(ios, filename,fatal)
  unit = 17
  call read_fcomp
  close(17)
  pec_files => pec_files%next
end do

pecs => first_pec

do while ( associated(pecs) )
  nextpec  => pecs%next
  deallocate(pecs , STAT=stat)
  call check_deallocate(stat,'pecs  in read_pec',warning)
  pecs => nextpec
end do

pec_files => first_file

do while ( associated(pec_files) )
  nextfile  => pec_files%next
  deallocate(pec_files , STAT=stat)
  call check_deallocate(stat,'pec_files  in read_pec',warning)
  pec_files => nextfile
end do

if ( associated(exportpecs)) then
  call PECsetparsedvalues(exportpecs)
end if

nullify(exportpecs)

CONTAINS

subroutine read_comp()
character(80) :: templine
do
  call read_line(unit)
  select case (line)
  case ('Filename')
    backspace(unit)
    exit
  case ('Components')

  case ('EOF')
    exit
  case ('')
    exit
  case default
    if (associated(pecs)) then
      allocate(pecs%next,STAT=stat)
      call check_allocate(stat,'pecs%next in read_pec',fatal)
      nullify(pecs%next%next)
      pecs => pecs%next
      pecs%comp = 0
    else
      allocate(pecs,STAT=stat)
      call check_allocate(stat,'pecs in read_pec',fatal)
      nullify(pecs%next)
      pecs%comp = 0
      first_pec => pecs
    end if
    templine = line
    read(templine,*,IOSTAT=ios) pecs%comp(1:5)
    if (ios /= 0) then
      read(line,*,IOSTAT=ios) pecs%comp(1), pecs%comp(2), pecs%comp(3),       &
           pecs%comp(4)
      call Check_ios(ios,'Components, PEC')
      pecs%comp(5) = -999
    end if

    count = count + 1
  end select
end do

end subroutine read_comp

subroutine read_fcomp()
character(80) :: templine
integer, dimension(5) :: comp

do
  call read_line(unit)
  select case (line)
  case ('Filename')
    backspace(unit)
    exit
  case ('Components')

  case ('EOF')
    exit
  case ('')
    exit
  case default
    templine = line
    read(templine,*,IOSTAT=ios) comp(1:5)
    if (ios /= 0) then
      read(line,*,IOSTAT=ios) comp(1), comp(2), comp(3), comp(4)
      call Check_ios(ios,'Components, PEC')
      comp(5) = -999
    end if
    count = count + 1
    exportpecs(count,1:5) = comp(1:5)
  end select
end do

end subroutine read_fcomp

subroutine count_comp()
character(80) :: templine
integer, dimension(5) :: comp
do
  call read_line(unit)
  select case (line)
  case ('Filename')
    backspace(unit)
    exit
  case ('Components')

  case ('EOF')
    exit
  case ('')
    exit
  case default
    templine = line
    read(templine,*,IOSTAT=ios) comp(1:5)
    if (ios /= 0) then
      read(line,*,IOSTAT=ios) comp(1), comp(2), comp(3), comp(4)
      call Check_ios(ios,'Components, PEC')
      comp(5) = -999
    end if

    count = count + 1
  end select
end do

end subroutine count_comp

Subroutine read_files
integer :: counter

allocate(Pec_files,STAT=stat)
call check_allocate(stat,'Pec_files in read_files',fatal)
nullify(Pec_files%next)
first_file => Pec_files
counter = 0

do
  call read_line(unit)
  select case (line)
  case ('Components')
    backspace(9)
    exit
  case ('', 'EOF')
    exit
  case default
    if ( counter > 0 ) then
      allocate(Pec_files%next,STAT=stat)
      call check_allocate(stat,'Pec_files%next in read_pec',fatal)
      nullify(Pec_files%next%next)
      Pec_files => Pec_files%next
    end if
    Pec_files%name = line
    counter  = counter + 1
  end select
end do
if (counter==0) then
  write(*,*) 'Error! No file was found under keyword PEC, Filename '
  stop
end if
End Subroutine read_files

END SUBROUTINE Read_pec

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Read_progress - reads the value under keyword PROGRESS
!
! DESCRIPTION
!       Reads the value under keyword PROGRESS
!
! SYNOPSIS
!       CALL Read_progress
!
! ERRORS
!       Wrong format halts the program
!
! HISTORY
!       Written by Lennart Hellström
!       progress_Awtic and progress_Awtwb added by ulfa 2002-01-24/25
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Read_progress()

USE progress_mod, ONLY : progress_check_no, progress_awtic, progress_awtwb

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: ios

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

do
  call read_line
  select case (line)
  case ('Iteration')
    call read_line
    if (line/='' .and. line/='EOF') then
      read(line,*,IOSTAT=ios)   progress_check_no
      call Check_ios(ios,'PROGRESS,Iteration')
      usedkeywords(5) = 1
    end if
  case ('Awtic') ! Awtic = Abort when timestepping is completed
    progress_awtic = .true.
  case ('Awtwb') ! Awtwb = Abort when timestepping will begin
    progress_awtwb = .true.
  case ('EOF')
    exit
  case ('')
    exit
  case default
    bWrongSpell = .true.
    write(*,*) 'Warning! The characters ', trim(line), ' are not recognized as'
    write(*,*) 'a secondary or primary keyword'
  end select
end do

write(*,*) '*******PROGRESS****************************'
write(*,*) 'progress,iteration:', progress_check_no
if (progress_awtwb) then
  write(*,*) 'Code will be aborted when initialization is complete.'
else if (progress_awtic) then
  write(*,*) 'Code will be aborted when timestepping is complete.'
end if

END SUBROUTINE Read_progress

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE Readdata_mod
