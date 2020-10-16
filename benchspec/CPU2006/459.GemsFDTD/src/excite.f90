!+
! NAME
!       excite_mod - General excitation module
!
! DESCRIPTION
!       The excite_mod module can and should be used by any module that needs
!       excitation. It contains a (Fortran-)function with a number of 
!       predefined time dependent excitation functions, f(t), where f has no
!       unit.
!
!       The peak values (max(abs(f(t))) of these pulses are in most cases = 1. 
!       The exceptions are the double exponential, the derived Gaussian
!       and the two modulated pulses. In the case of the modulated pulses, 
!       f(t)=g(t)*h(t), the peak value of g(t) and h(t) are = 1. In case 
!       of the derived Gaussian it is the peak value of the primitive
!       function F(t) (a Gaussian) that is = 1. 
!
!       All pulses are (and shall be) defined so that they are zero for 
!       negative time, i.e. f(t)=0 if t<0. With the exception of pulse 2,
!       the unitary pulse (no. 3), the Gaussian pulse (no. 6) and the 
!       derived Gaussian pulse (no. 11) we also have f(0)=0.
!
! PUBLIC
!       FUNCTION excitation
!       integer, parameter :: excite_max_no_param = 3
!       SUBROUTINE Excitation_check_keyword
!
! SEE ALSO
!       globalvar_mod and huygens_mod
!
! PHYSICS
!       Some important relations that you should know by heart, but in
!       case you do not, here they are:
!
!       \omega = 2 * \pi * f = 2 * \pi * c / \lambda  = k * c 
!
!       (i.e. c = f * \lambda  and  k = 2 * \pi / \lambda)
!
!       where c = speed of light [m/s]
!             f = frequency [Hz=1/s]
!             \omega = angular frequency [rad/s]
!             \lambda = wavelength [m]
!
! HISTORY
!       Version       Date                 Name
!       Comments
!       -------------------------------------------
!	$Log: excite.f90,v $
!	Revision 1.2  2005/04/05 10:44:10  ulfa
!	Superfluous lines have been removed.
!	
!	Revision 1.1  2003/09/23 14:06:50  ulfa
!	Initial version.
!	
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE excite_mod

USE parameter_mod, ONLY : rfp

IMPLICIT NONE

PUBLIC excitation, Excitation_check_keyword

integer, parameter, PUBLIC :: excite_max_no_param = 3
  ! Presently no excitation function needs more than three parameters. 
  ! If the need arises, excite_max_no_param may be increased.

PRIVATE

integer, parameter :: nkeywords = 14

character(80), PARAMETER, dimension(1:nkeywords) :: keywords =                &
 (/'Sinebump                                     ',                           &
   'Cosine_hp                                    ',                           &
   'Unitary                                      ',                           &
   'Moore                                        ',                           &
   'Mooremod                                     ',                           &
   'Gauss                                        ',                           &
   'Gaussmod                                     ',                           &
   'Doubleexp                                    ',                           &
   'Cw                                           ',                           &
   'Cw_ramp                                      ',                           &
   'Gaussder                                     ',                           &
   'Rect                                         ',                           &
   'Sinus                                        ',                           &
   'Freqstep                                     '/)

integer, PARAMETER, dimension(1:nkeywords) :: no_param_vector =               &
(/ 1, 1, 1, 1, 1, 2, 3, 2, 1, 3, 2, 1, 1, 3 /)

INTERFACE excitation
  MODULE PROCEDURE excitation_array, excitation_vector, excitation_scalar
END INTERFACE

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       excitation_array - Excitation function for arrays
!
! DESCRIPTION
!       Excitation function for arrays.
!
! SYNOPSIS
!       FUNCTION excitation_array( t, exc_param, PulseType )
!         real(kind=rfp), dimension(:,:), intent(in) :: t
!         real(kind=rfp), dimension(excite_max_no_param), intent(in)::exc_param
!         integer, intent(in) :: PulseType
!
! RETURN VALUES
!       real(kind=rfp), dimension(size(t,1),size(t,2)) :: excitation_array
!
! ERRORS
!       The program stops if a non-defined pulse-type-integer is given
!
! SEE ALSO
!       excitation_vector & excitation_scalar
!
! HISTORY
!       Written by Ulf Andersson 99-06-08. Concept suggested by Torleif Martin.
!       Created by extracting Uin from Huygens. 
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION excitation_array( t, exc_param, PulseType )
USE parameter_mod, ONLY : half, one, two, pi
USE globalvar_mod, ONLY : c0

!------------------------------------------------------------------------------
!                     A r g u m e n t s
!------------------------------------------------------------------------------

real(kind=rfp), dimension(:,:), intent(in) :: t
real(kind=rfp), dimension(excite_max_no_param), intent(in) :: exc_param
integer, intent(in) :: PulseType

!------------------------------------------------------------------------------
!                     R e t u r n  t y p e
!------------------------------------------------------------------------------

real(kind=rfp), dimension(size(t,1),size(t,2)) :: excitation_array

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

real(kind=rfp) :: a1, a3, a4, t_off  ! Used by case 14

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

excitation_array = 0.0_rfp

select case (Pulsetype)
!----------------------------------------------------
! Pulsetype 1: A single sine bump 
! ============
! exc_param(1) = f
! exc_param(2:) are not used
!
case (1)
  where ( t <= half/exc_param(1) .and. t>=0 )
    excitation_array = sin( two*pi*exc_param(1)*t)
  end where

!----------------------------------------------------
! Pulsetype 2: A single cosine bump
! ============
! exc_param(1) = f
! exc_param(2:) are not used
!
! Note: This pulse is discont. at t=0 and t=lambda/c0
case (2)
  where ( t <= half/exc_param(1) .and. t>=0 )
    excitation_array = cos( two*pi*exc_param(1)*t)
  end where

!----------------------------------------------------
! Pulsetype 3: A unitary pulse
! ============
! exc_param(1) = t_0 = the length (in seconds) of the unitary pulse
! exc_param(2:) are not used
!
case (3)
  where ( t <= exc_param(1) .and. t>=0 )
    excitation_array = one
  end where
  

!----------------------------------------------------
! Pulsetype 4: A smooth compact pulse
! ============
! Taken from Moore et al., page 1804, IEEE Trans. Ant. and Propagat., Vol. 36,
! No. 12, Dec. 1998, where it is used as a point source term is a study of
! the efficiency of different absorbing boundary conditions
! Notice that we divide with 32 while they in the paper divide
! by 320. We choose 32 to get a peak value of 1 (ulfa & ledfelt)
!
! exc_param(1) = t_0 (called \tau in Moore et al. (41b))
! exc_param(2:) are not used
!
case (4)
  where ( t <= exc_param(1) .and. t>=0.0_rfp )
    excitation_array = one /32.0_rfp*( 10.0_rfp                               &
                          - 15.0_rfp*cos( 2.0_rfp*pi*t/exc_param(1) )         &
                          +  6.0_rfp*cos( 4.0_rfp*pi*t/exc_param(1) )         &
                          -          cos( 6.0_rfp*pi*t/exc_param(1) ) )
  end where

!----------------------------------------------------
! Pulsetype 5: A smooth compact pulse (no. 4) with a sine modulation
! ============
! exc_param(1) = t_0 (called \tau in Moore et al. (41b))
! exc_param(2:) are not used
!
case (5)
  where ( t <= exc_param(1) .and. t>=0.0_rfp )
    excitation_array = sin( 2.0_rfp*pi*t/exc_param(1) )/32.0_rfp*(10.0_rfp    &
                           - 15.0_rfp*cos( 2.0_rfp*pi*t/exc_param(1) )        &
                           +  6.0_rfp*cos( 4.0_rfp*pi*t/exc_param(1) )        &
                           -          cos( 6.0_rfp*pi*t/exc_param(1) ) )
  end where

!----------------------------------------------------
! Pulsetype 6: A Gauss-pulse 
! ============
! exc_param(1) = t_0
! exc_param(2) = t_w
! exc_param(3:) are not used
!
case (6)
  where ( t>=0.0_rfp )
    excitation_array = exp( -((t-exc_param(1))/exc_param(2))**2 )
  end where

!----------------------------------------------------
! Pulsetype 7: A modulated Gauss-pulse
! ============
! exc_param(1) = f
! exc_param(2) = t_0 (peak time for Gaussian)
! exc_param(3) = t_w (pulse-width of Gaussian)
!
case (7)
  where ( t>=0.0_rfp )
    excitation_array = sin(two*pi*exc_param(1)*t) *                           &
                       exp( -((t-exc_param(2))/exc_param(3))**2 )
  end where

!----------------------------------------------------
! Pulsetype 8: A double exponential pulse 
! ============
! exc_param(1) = t_1 
! exc_param(2) = t_2 
! exc_param(3:) are not used
!
case (8)
  where ( t>=0.0_rfp )
    excitation_array = exp(-t/exc_param(1))-exp(-t/(exc_param(2)))
  end where

!----------------------------------------------------
! Pulsetype 9: Continuous Wave (CW)
! ============
! exc_param(1) = f
! exc_param(2:) are not used
!
case (9)
  where ( t>=0.0_rfp )
    excitation_array = sin(two*pi*exc_param(1)*t)
  end where

!----------------------------------------------------
! Pulsetype 10: Ramped Continuous Wave (CW)
! ============
! exc_param(1) = f
! exc_param(2) = t_0
! exc_param(3) = t_w
!
case (10)
  where ( t>exc_param(2) )
    excitation_array = sin(two*pi*exc_param(1)*t)
  elsewhere 
    excitation_array = sin(two*pi*exc_param(1)*t) *                           &
                       exp( -((t-exc_param(2))/exc_param(3))**2 )
  end where
  where ( t<0.0_rfp )
    excitation_array = 0.0_rfp
  end where

!----------------------------------------------------
! Pulsetype 11: Derivative of A Gauss-pulse 
! ============
! exc_param(1) = t_0
! exc_param(2) = t_w
! exc_param(3:) are not used
!
case (11)
  where ( t>=0.0_rfp )
    excitation_array = -two*(t-exc_param(1))/exc_param(2)*                    &
                        exp( -((t-exc_param(1))/exc_param(2))**2 )
  end where

!----------------------------------------------------
! Pulsetype 12: "Sågtandspuls"
! ============
! exc_param(1) = f
! exc_param(2:) are not used
!
case (12)
  where ( t>=0.0_rfp )
    excitation_array = sin(two*pi*exc_param(1)*t)
  end where

  where (excitation_array>0.5_rfp)
    excitation_array = 1.0_rfp
  end where
  where (excitation_array<-0.5_rfp)
    excitation_array = -1.0_rfp
  end where
  where ((excitation_array<0.5_rfp) .and. (excitation_array>-0.5_rfp))
    excitation_array = 0.0_rfp
  end where

!----------------------------------------------------
! Pulsetype 13: "Ramped Sinus with one parameter" (needed by Bosse)
! ============
! exc_param(1) = pulse length (in meters!!!!)
! exc_param(2:) are not used
!
case (13)
  where  ( (0.0_rfp <= t).and.( exc_param(1) >=c0*t))
    excitation_array = sin(two*pi/exc_param(1)*c0*t)*                         &
         exp(-(sqrt(-log(0.001_rfp))/exc_param(1)*c0*(-t+exc_param(1)/c0))**2)
  end where
  where (exc_param(1) < c0*t)
    excitation_array = sin(two*pi/exc_param(1)*c0*t)
  end where

!----------------------------------------------------
! Pulsetype 14: "Frequency step" (used in IMPACT)
! ============
! exc_param(1) = lower frequency limit (Hz)
! exc_param(2) = upper frequency limit (Hz)
! exc_param(3) = time offset (s)

case(14)
  a1 = exc_param(2)*2.0_rfp*pi
!  a2 = 10.0_rfp/a1
  a3 = exc_param(1)*2.0_rfp*pi
  a4 = 10.0_rfp/a3
  t_off = exc_param(3)
  
  where ( t >= 0.0_rfp  )
    excitation_array = sin(a1*(t-t_off-a4))/(a1*(t-t_off-a4))-                &
                       a3/a1*sin(a3*(t-t_off-a4))/(a3*(t-t_off-a4))
  end where

case default
  write(*,*) ' FATAL ERROR! Wrong number of pulse in excitation ', Pulsetype
  stop

end select

END FUNCTION excitation_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       excitation_vector - Excitation function for vectors
!
! DESCRIPTION
!       Excitation function for vectors
!
! METHOD
!       Calls the function excitation_array. It would probably be more 
!       efficient to copy the code in excitation_array, but this would
!       mean that a new excitation must be added in more than one place
!       which is begging for problems.
!
! SYNOPSIS
!       FUNCTION excitation_array( t, exc_param, PulseType )
!         real(kind=rfp), intent(in) :: t
!         real(kind=rfp), dimension(excite_max_no_param), intent(in)::exc_param
!         integer, intent(in) :: PulseType
!
! RETURN VALUES
!       real(kind=rfp), dimension(size(t,1)) :: excitation_vector
!
! SEE ALSO
!       excitation_array
!
! HISTORY
!       Written by Ulf Andersson 99-06-21
!
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION excitation_vector( t, exc_param, PulseType )

!------------------------------------------------------------------------------
!                     A r g u m e n t s
!------------------------------------------------------------------------------

real(kind=rfp), dimension(:), intent(in) :: t
real(kind=rfp), dimension(excite_max_no_param), intent(in) :: exc_param
integer, intent(in) :: PulseType

!------------------------------------------------------------------------------
!                     R e t u r n  t y p e
!------------------------------------------------------------------------------

real(kind=rfp), dimension(size(t,1)) :: excitation_vector

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

real(kind=rfp), dimension(size(t,1),1) :: temp
real(kind=rfp), dimension(size(t,1),1) :: t_dummy

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

t_dummy(:,1) = t
temp = excitation_array(t_dummy, exc_param, PulseType)
excitation_vector = temp(:,1)

END FUNCTION excitation_vector

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       excitation_scalar - Excitation function for scalars
!
! DESCRIPTION
!       Excitation function for scalars
!
! METHOD
!       Calls the function excitation_array. It would probably be more 
!       efficient to copy the code in excitation_array, but this would
!       mean that a new excitation must be added in more than one place
!       which is begging for problems.
!
! SYNOPSIS
!       FUNCTION excitation_array( t, exc_param, PulseType )
!         real(kind=rfp), intent(in) :: t
!         real(kind=rfp), dimension(excite_max_no_param), intent(in)::exc_param
!         integer, intent(in) :: PulseType
!
! RETURN VALUES
!       real(kind=rfp) :: excitation_scalar
!
! SEE ALSO
!       excitation_array
!
! HISTORY
!       Written by Ulf Andersson 99-06-21
!
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION excitation_scalar( t, exc_param, PulseType )

!------------------------------------------------------------------------------
!                     A r g u m e n t s
!------------------------------------------------------------------------------

real(kind=rfp), intent(in) :: t
real(kind=rfp), dimension(excite_max_no_param), intent(in) :: exc_param
integer, intent(in) :: PulseType

!------------------------------------------------------------------------------
!                     R e t u r n  t y p e
!------------------------------------------------------------------------------

real(kind=rfp) :: excitation_scalar

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

real(kind=rfp), dimension(1,1) :: temp
real(kind=rfp), dimension(1,1) :: t_dummy

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

t_dummy(1,1) = t
temp = excitation_array(t_dummy, exc_param, PulseType)
excitation_scalar = temp(1,1)

END FUNCTION excitation_scalar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Excitation_check_keyword - Checks for excitation keyword
!
! DESCRIPTION
!       Given a character string line, this routines checks if this string
!       equals any of the strings used for the different available excitations
!       The identification number for the excitation is returned in PulseType
!       PulseType is set to zero if now match is found.
!       The number of parameters needed for this pulse is returned in no_param
!
! SYNOPSIS
!       CALL Excitation_check_keyword(line,PulseType,no_param)
!
! SEE ALSO
!       All the different readdata routines.
!
! HISTORY
!       Written by Ulf Andersson 2000-01-15
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Excitation_check_keyword(line,PulseType,no_param)

!------------------------------------------------------------------------------
!                     A r g u m e n t s
!------------------------------------------------------------------------------

character(80), intent(in)  :: line
integer, intent(out) :: PulseType, no_param

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: ii

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

PulseType = 0
no_param = 0
do ii=1,nkeywords
  if (trim(keywords(ii))==line) then
    PulseType = ii
    no_param = no_param_vector(ii)
  end if
end do

END SUBROUTINE Excitation_check_keyword

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE excite_mod
