!+
! NAME
!       posvector_mod - Functions using the type "POSVECTOR"
!
! DESCRIPTION
!       Functions using the type "POSVECTOR"
!
! PUBLIC
!       The following is public and can be called by USEing this module:
!
!       TYPE(POSVECTOR)
!       Operator(-)
!       FUNCTION Phase
!       FUNCTION PhaseRefl
!       FUNCTION dotprod
!       FUNCTION SUMSUM
!
! HISTORY
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE posvector_mod

USE parameter_mod, ONLY : rfp, eps0

IMPLICIT NONE


PUBLIC :: POSVECTOR, OPERATOR(-), Phase, PhaseRefl, dotprod, SUMSUM
PUBLIC :: SetRvector, Setposvector, Getposvector

PRIVATE

!------------------------------------------------------------------------------
!                     T y p e  d e f i n i t i o n s
!------------------------------------------------------------------------------

TYPE POSVECTOR
! PRIVATE       

  REAL(KIND=rfp) :: x
  REAL(KIND=rfp) :: y
  REAL(KIND=rfp) :: z
END TYPE POSVECTOR

!------------------------------------------------------------------------------
!                     I n t e r f a c e  b l o c k s
!------------------------------------------------------------------------------

! Interfaces (for generic functions)
! Operator definitions (new operators or overloaded existing ones):
! Interface that allows substraction of an array of Posvector with a 
! Posvector:

INTERFACE OPERATOR(-)
  MODULE PROCEDURE Posvectorminus
END INTERFACE

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Phase - Calculates a complex phase factor (jkr' dot r_hat)
!
! DESCRIPTION
!       Internal function that calculates the phase factor used in the free 
!       space Greens function in the Near- to far zone transformation.
!
! METHOD
!       computes the phase factor (jk r' dot r_hat) where jk is a scalar, r'
!       is an array of vectors and r_hat is a vector
!
! SYNOPSIS
!       Phase(JK,Rhat,Rarray)
!
! RETURN VALUES
!       Rout is a complex 2-dim array
!
! ERRORS
!       NA
!
! SEE ALSO
!       NA
!
! HISTORY
!       Version   Date     Comment                 Name
!       -------   ----     -------                 ----
!         1.0   990521    Initial version       Torleif Martin
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION Phase(JK,Rhat,Rarray) RESULT ( Rout )

!------------------------------------------------------------------------------
!                     A r g u m e n t s
!------------------------------------------------------------------------------

! Arguments with intent in 
! jk is the wavenumber mult. with complex j:
COMPLEX(KIND=rfp),                  INTENT(IN) :: JK    
TYPE (POSVECTOR),                   INTENT(IN) :: Rhat    ! Far zone direction.
TYPE (POSVECTOR), DIMENSION(:,:),   INTENT(IN) :: Rarray  ! r_prime array

!------------------------------------------------------------------------------
!                     R e t u r n  t y p e
!------------------------------------------------------------------------------

COMPLEX(KIND=rfp), DIMENSION(size(Rarray,1),size(Rarray,2)) :: Rout

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

Rout = JK * (Rhat%x * Rarray%x  +  Rhat%y * Rarray%y  +  Rhat%z * Rarray%z)

END FUNCTION Phase

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       PhaseRefl - Calculates a complex phase factor (jkr' dot r_hat) for
!       a reflected wave. 
!
! DESCRIPTION
!       Internal function that calculates the phase factor used in the 
!       Greens function including reflection in a ground plane in the 
!       Near- to far zone transformation.
!
! METHOD
!       computes the phase factor (jk r' dot r_hat) where jk is a scalar, r'
!       is an array of vectors and r_hat is a vector. Note: the z-direction
!       of the wave is reversed -r_hat%z ! Also note that for phaseses where
!       Rarray has a reference point in free space, the Rarray should have its
!       reference point mirrored in the groundplane.
!
! SYNOPSIS
!       PhaseRefl(JK,Rhat,Rarray)
!
! RETURN VALUES
!       Rout is a complex 2-dim array
!
! ERRORS
!       NA
!
! SEE ALSO
!       Function Phase
!
! HISTORY
!       Version   Date     Comment                 Name
!       -------   ----     -------                 ----
!         1.0   990926    Initial version       Torleif Martin
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION PhaseRefl(JK,Rhat,Rarray) RESULT ( Rout )

!------------------------------------------------------------------------------
!                     A r g u m e n t s
!------------------------------------------------------------------------------

! Arguments with intent in 
! jk is the wavenumber mult. with complex j:
COMPLEX(KIND=rfp),                  INTENT(IN) :: JK    
TYPE (POSVECTOR),                   INTENT(IN) :: Rhat    ! Far zone direction.
TYPE (POSVECTOR), DIMENSION(:,:),   INTENT(IN) :: Rarray  ! r_prime array

!------------------------------------------------------------------------------
!                     R e t u r n  t y p e
!------------------------------------------------------------------------------

COMPLEX(KIND=rfp), DIMENSION(size(Rarray,1),size(Rarray,2)) :: Rout

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

Rout = JK * (Rhat%x * Rarray%x  +  Rhat%y * Rarray%y  -  Rhat%z * Rarray%z)

END FUNCTION PhaseRefl

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Posvectorminus - Internal function for R1 - R2 
!
! DESCRIPTION
!       Calculates the R1 - R2 where R1 is an array of position vectors
!       and where R2 is a single position vector
!
! METHOD
!       R1-R2
!
! SYNOPSIS
!       C = Posvectorminus (A,B) used in interface
!
! RETURN VALUES
!       C is an array of type Posvector
!
! ERRORS
!       NA
!
! SEE ALSO
!       NA
!
! HISTORY
!       Version   Date     Comment                 Name
!       -------   ----     -------                 ----
!       1.0       990521   Initial version         Torleif Martin
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION Posvectorminus (A,B) RESULT(C)

!------------------------------------------------------------------------------
!                     A r g u m e n t s
!------------------------------------------------------------------------------

! Arguments with intent in

TYPE (POSVECTOR), DIMENSION(:,:),   INTENT(IN) :: A  
TYPE (POSVECTOR),                   INTENT(IN) :: B

!------------------------------------------------------------------------------
!                     R e t u r n  t y p e
!------------------------------------------------------------------------------

TYPE (POSVECTOR), DIMENSION(size(A,1),size(A,2))  :: C

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

C%x = A%x - B%x
C%y = A%y - B%y
C%z = A%z - B%z

END FUNCTION Posvectorminus

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       dotprod - Function that calculates scalar product
!
!
! DESCRIPTION
!       Calculates the scalar product R dot Rhat used in the NFT.f90.
!
! METHOD
!       Computes the scalar product (R dot Rhat)/(c0*dt) where R = RYexhz1,
!       etc, is an array of vectors and Rhat is a vector. R is the position 
!       vector for the surface current and Rhat is a unit vector to the
!       observation point.
!
! SYNOPSIS
!       dotprod(Rarray,Rhat)
!         type (posvector), dimension(:,:), intent(in) :: Rarray
!         type (posvector),                 intent(in) :: Rhat
!
! RETURN VALUES
!       Rout is a real 2-dim. array
!
! HISTORY
!       Written by Åke Rydell
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION dotprod(Rarray,Rhat) result(Rout)

!------------------------------------------------------------------------------
!                     A r g u m e n t s
!------------------------------------------------------------------------------

type (posvector), dimension(:,:), intent(in) :: Rarray  ! r_prime
type (posvector),                 intent(in) :: Rhat    ! Far zone direction

!------------------------------------------------------------------------------
!                          R e t u r n  t y p e
!------------------------------------------------------------------------------

real(kind=rfp), dimension(size(Rarray,1),size(Rarray,2)) :: Rout

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

Rout = (Rarray%x * Rhat%x + Rarray%y * Rhat%y + Rarray%z * Rhat%z)

END FUNCTION dotprod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       SUMSUM - Integrates SUM( J exp(jkr'. r))ds
!
! DESCRIPTION
!       This function was introduced as an attempt to shorten the horrendous
!       compilation time when performing optimization. Tests established that
!       it took almost two second per occurrence to optimize lines like
!       SUM(SUM(HzY1(k,:,:) * exp(Phase(jk,Rhat,RYhzex1)),1),1).
!       There were forty such lines in nff_print, which have now been replaced
!       with calls to this function.
!
! METHOD
!       Only one line of code: Out = SUM(SUM(J(:,:)*exp(Phase(jk,Rhat,R)),1),1)
!
! SYNOPSIS
!       FUNCTION SUMSUM(J,jk,Rhat,R) RESULT (Out)
!         COMPLEX(KIND=rfp),  DIMENSION(:,:), INTENT(IN) :: J
!         COMPLEX(KIND=rfp)                 , INTENT(IN) :: jk
!         TYPE(POSVECTOR)                   , INTENT(IN) :: Rhat   
!         TYPE(POSVECTOR),    DIMENSION(:,:), INTENT(IN) :: R
!
! RETURN VALUES
!       COMPLEX(KIND=rfp) :: Out
!
! ERRORS
!       NA
!
! SEE ALSO
!       NFF_Print
!
! HISTORY
!       Version   Date     Comment                 Name
!       -------   ----     -------                 ----
!         1.0     00-02-16 Initial version         Ulf Andersson
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION SUMSUM(J,jk,Rhat,R) RESULT (Out)

!------------------------------------------------------------------------------
!                     A r g u m e n t s
!------------------------------------------------------------------------------

COMPLEX(KIND=rfp),  DIMENSION(:,:), INTENT(IN) :: J
COMPLEX(KIND=rfp)                 , INTENT(IN) :: jk
TYPE(POSVECTOR)                   , INTENT(IN) :: Rhat    !Position vectors
TYPE(POSVECTOR),    DIMENSION(:,:), INTENT(IN) :: R

!------------------------------------------------------------------------------
!                     R e t u r n  t y p e
!------------------------------------------------------------------------------

COMPLEX(KIND=rfp) :: Out

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

Out = SUM(SUM(J*exp(Phase(jk,Rhat,R)),1),1)

END FUNCTION SUMSUM

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       SetRvector
!
! DESCRIPTION
!       Calculates the position vectors on a surface plane.
!
! METHOD
!       Sets the coordinates of the position vector array at
!       the equivalent surface current positions.
!
! SYNOPSIS
!       SetRvector(na,nb,a1,b1,c,dx,dy,dz,plane)
!
! RETURN VALUES
!       Rout is a real 2-dim array
!
! ERRORS
!       NA
!
! SEE ALSO
!       NA
!
! HISTORY
!       Version   Date     Comment                 Name
!       -------   ----     -------                 ----
!         1.0   000503    Initial version       Torleif Martin
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION SetRvector(na,nb,a1,b1,c,dx,dy,dz,plane) RESULT ( Rout )

!------------------------------------------------------------------------------
!                     A r g u m e n t s
!------------------------------------------------------------------------------

! Arguments with intent in 

INTEGER,                            INTENT(IN) :: na  ! Numb of cells in a-dir
INTEGER,                            INTENT(IN) :: nb  ! Numb of cells in b-dir
REAL(kind=rfp),                     INTENT(IN) :: a1  ! Start pos a-dir
REAL(kind=rfp),                     INTENT(IN) :: b1  ! Start pos b-dir
REAL(kind=rfp),                     INTENT(IN) :: c   ! Constant cell coord.
REAL(kind=rfp),                     INTENT(IN) :: dx,dy,dz ! Cellsize
character,                          INTENT(IN) :: plane ! Orientation

!------------------------------------------------------------------------------
!                     R e t u r n  t y p e
!------------------------------------------------------------------------------

TYPE (POSVECTOR), DIMENSION(na,nb)             :: Rout 


!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------
INTEGER                                      :: i,j,k      !Loop var.

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------



SELECT CASE(plane)
    CASE('X') ! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      DO k=1,nb
        Rout(:,k)%y = ((/(j,j=0,na-1)/) + a1)*dy
      END DO
      DO j=1,na
        Rout(j,:)%z = ((/(k,k=0,nb-1)/) + b1)*dz
      END DO
! X-coordinate:
      Rout%x = c*dx

    CASE('Y') ! YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY

      DO k=1,nb
        Rout(:,k)%x = ((/(i,i=0,na-1)/) + a1)*dx
      END DO
      DO i=1,na
        Rout(i,:)%z = ((/(k,k=0,nb-1)/) + b1)*dz
      END DO
! Y-coordinate:
      Rout%y = c*dy

    CASE('Z') ! ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ
 
      DO j=1,nb
        Rout(:,j)%x = ((/(i,i=0,na-1)/) + a1)*dx
      END DO
      DO i=1,na
        Rout(i,:)%y = ((/(j,j=0,nb-1)/) + b1)*dy
      END DO
! Z-coordinate:
      Rout%z = c*dz

END SELECT

END FUNCTION SetRvector

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Setposvector
!
! DESCRIPTION
!       Sets the three coordinates of a posvector type.
!
! METHOD
!       Plain assignment.
!
! SYNOPSIS
!       Rout = Setposvector(x,y,z)
!
! RETURN VALUES
!       Rout is a "scalar" of position type (actually a 3D-vector)
!
! ERRORS
!       NA
!
! SEE ALSO
!       NA
!
! HISTORY
!       Version   Date     Comment                 Name
!       -------   ----     -------                 ----
!         1.0   000503    Initial version       Torleif Martin
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION Setposvector(x,y,z) RESULT ( Rout )

!------------------------------------------------------------------------------
!                     A r g u m e n t s
!------------------------------------------------------------------------------

! Arguments with intent in 

REAL(kind=rfp),          INTENT(IN) :: x,y,z ! coordinates

!------------------------------------------------------------------------------
!                     R e t u r n  t y p e
!------------------------------------------------------------------------------

TYPE (POSVECTOR)                    :: Rout 

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------
Rout%x = x
Rout%y = y
Rout%z = z

END FUNCTION Setposvector

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Getposvector
!
! DESCRIPTION
!       Gets the three coordinates of a posvector type.
!
! METHOD
!       Plain assignment.
!
! SYNOPSIS
!       CALL Getposvector(r,x,y,z)
!
! ERRORS
!       NA
!
! SEE ALSO
!       NA
!
! HISTORY
!       Version   Date     Comment                 Name
!       -------   ----     -------                 ----
!         1.0   000503    Initial version       Torleif Martin
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Getposvector(r, x,y,z)

!------------------------------------------------------------------------------
!                     A r g u m e n t s
!------------------------------------------------------------------------------

TYPE(posvector), INTENT(in)  :: r
REAL(kind=rfp),  INTENT(out) :: x,y,z

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

x = R%x
y = R%y
z = R%z

END SUBROUTINE Getposvector

END MODULE posvector_mod
