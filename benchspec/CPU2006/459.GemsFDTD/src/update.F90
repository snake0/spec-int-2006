!+
! NAME
!       update_mod - leap-frog update module
!
! DESCRIPTION
!       Module for the leap-frog update. Routines for homogeneous materials are
!       included in this module. 
!
! PUBLIC
!       Subroutine update_init
!       Subroutine updateH_homo
!       Subroutine updateE_homo
!
! HISTORY
!       Version       Date                 Name
!       Comments
!       -------------------------------------------
!	$Log: update.f90,v $
!	Revision 1.2  2003/09/26 19:40:13  ulfa
!	Removed calcmet 3 and 4. They were not used anyway.
!	
!	Revision 1.1  2003/09/23 14:06:52  ulfa
!	Initial version.
!	
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE update_mod

USE parameter_mod, ONLY : rfp

IMPLICIT NONE

PRIVATE
!
! Update Variables
!
real(kind=rfp) :: Cbdx, Cbdy, Cbdz
real(kind=rfp) :: Dbdx, Dbdy, Dbdz

PUBLIC update_init, updateH_homo, updateE_homo

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       updateH_homo - leap-frog update of the H-fields for homo. materials.
!
! DESCRIPTION
!       Performs the leap-frog update of the H-fields according to
!       formula (3.26) in Taflove's book. The terminology used is the
!       same, i.e. the coefficients defined in (3.28) are used. (Note that
!       we have switched the use of C and D as compared with Taflove.) 
!       However, they are constant in the code, i.e. this routine is for homo-
!       geneous materials. The updated H-fields are: H?(1:nx,1:ny,1:nz)
!
! METHOD
!       The update is performed with fused triple-nested do-loops
!
! SYNOPSIS
!       CALL updateH_homo(nx,ny,nz,Hx,Hy,Hz,Ex,Ey,Ez)
!         integer, intent(in) :: nx, ny, nz
!         real(kind=rfp), intent(in   ),                                      &
!           dimension(xstart:xstop+1,ystart:ystop+1,zstart:zstop+1) :: Ex,Ey,Ez
!         real(kind=rfp), intent(inout),                                      &
!           dimension(xstart:xstop  ,ystart:ystop  ,zstart:zstop  ) :: Hx,Hy,Hz
!
! SEE ALSO
!       updateE_homo
!
! HISTORY
!       Written by Ulf Andersson
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE updateH_homo(nx,ny,nz,Hx,Hy,Hz,Ex,Ey,Ez)
USE globalvar_mod, ONLY : xstart, ystart, zstart, xstop, ystop, zstop

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: nx, ny, nz
real(kind=rfp), intent(in),                                                   &
         dimension(xstart:xstop+1,ystart:ystop+1,zstart:zstop+1) :: Ex, Ey, Ez
real(kind=rfp), intent(inout),                                                &
         dimension(xstart:xstop  ,ystart:ystop  ,zstart:zstop  ) :: Hx, Hy, Hz

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: i, j, k

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

#ifndef SPEC_CPU
!$OMP PARALLEL DO PRIVATE(i,j,k),                                             &
#endif
#ifndef SPEC_CPU
!$OMP SHARED(nx,ny,nz,Cbdx,Cbdy,Cbdz,Ex,Ey,Ez,Hx,Hy,Hz)
#endif
do k=1,nz
  do j=1,ny
    do i=1,nx
      Hx(i,j,k) = Hx(i,j,k) +                                                 &
                (  (Ey(i,j,k+1)-Ey(i,j  ,k))*Cbdz +                           &
                   (Ez(i,j,k  )-Ez(i,j+1,k))*Cbdy  )
      Hy(i,j,k) = Hy(i,j,k) +                                                 &
                (  (Ez(i+1,j,k)-Ez(i,j,k  ))*Cbdx +                           &
                   (Ex(i  ,j,k)-Ex(i,j,k+1))*Cbdz  )
      Hz(i,j,k) = Hz(i,j,k) +                                                 &
                (  (Ex(i,j+1,k)-Ex(i  ,j,k))*Cbdy +                           &
                   (Ey(i,j  ,k)-Ey(i+1,j,k))*Cbdx  )
    end do
  end do
end do
#ifndef SPEC_CPU
!$OMP END PARALLEL DO
#endif

END SUBROUTINE updateH_homo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       updateE_homo - leap-frog update of the E-fields for homo. materials.
!
! DESCRIPTION
!       Performs the leap-frog update of the E-fields according to
!       formula (3.27) in Taflove's book. The terminology used is the
!       same, i.e. the coefficients defined in (3.29) are used.  (Note that
!       we have switched the use of C and D as compared with Taflove.)
!       However, they are constant in the code, i.e. this routine is for homo-
!       geneous materials. The updated E-fields are 
!
!       Ex(1:nx,2:ny,2:nz), Ey(2:nx,1:ny,2:nz) and Ez(2:nx,2:ny,1:nz)
!
!       The components omitted by the update are treated by ABC routines
!
! METHOD
!       The update is performed with fused triple-nested do-loops
!       Three double-nested do loops are needed to handle the leftovers.
!
! SYNOPSIS
!       CALL updateE_homo(nx,ny,nz,Hx,Hy,Hz,Ex,Ey,Ez)
!         integer, intent(in) :: nx, ny, nz
!         real(kind=rfp), intent(inout),                                      &
!           dimension(xstart:xstop+1,ystart:ystop+1,zstart:zstop+1) :: Ex,Ey,Ez
!         real(kind=rfp), intent(in),                                         &
!           dimension(xstart:xstop  ,ystart:ystop  ,zstart:zstop  ) :: Hx,Hy,Hz
!
! SEE ALSO
!       updateH_homo
!
! HISTORY
!       Written by Ulf Andersson
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE updateE_homo(nx,ny,nz,Hx,Hy,Hz,Ex,Ey,Ez)
USE globalvar_mod, ONLY : xstart, ystart, zstart, xstop, ystop, zstop

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: nx, ny, nz
real(kind=rfp), intent(inout),                                                &
         dimension(xstart:xstop+1,ystart:ystop+1,zstart:zstop+1) :: Ex, Ey, Ez
real(kind=rfp), intent(in),                                                   &
         dimension(xstart:xstop  ,ystart:ystop  ,zstart:zstop  ) :: Hx, Hy, Hz

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: i, j, k

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

!
! First the leftovers ...
!
#ifndef SPEC_CPU
!$OMP PARALLEL 
#endif
#ifndef SPEC_CPU
!$OMP DO PRIVATE(j,k)
#endif
do k=2,nz
  do j=2,ny
    Ex(1,j,k) = Ex(1,j,k) +                                                   &
              (  (Hz(1,j,k  )-Hz(1,j-1,k))*Dbdy +                             &
                 (Hy(1,j,k-1)-Hy(1,j  ,k))*Dbdz  )
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

#ifndef SPEC_CPU
!$OMP DO PRIVATE(i,k)
#endif
do k=2,nz
  do i=2,nx  
    Ey(i,1,k) = Ey(i,1,k) +                                                   &
              (  (Hx(i  ,1,k)-Hx(i,1,k-1))*Dbdz +                             &
                 (Hz(i-1,1,k)-Hz(i,1,k  ))*Dbdx  )
  end do
end do                    
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

#ifndef SPEC_CPU
!$OMP DO PRIVATE(i,j)
#endif
do j=2,ny
  do i=2,nx 
    Ez(i,j,1) = Ez(i,j,1) +                                                   &
              (  (Hy(i,j  ,1)-Hy(i-1,j,1))*Dbdx +                             &
                 (Hx(i,j-1,1)-Hx(i  ,j,1))*Dbdy  )
  end do
end do   
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif
!
!   ... and then the fused loops
!
#ifndef SPEC_CPU
!$OMP DO PRIVATE(i,j,k)
#endif
do k=2,nz
  do j=2,ny 
    do i=2,nx 
      Ex(i,j,k) = Ex(i,j,k) +                                                 &
                (  (Hz(i,j,k  )-Hz(i,j-1,k))*Dbdy +                           &
                   (Hy(i,j,k-1)-Hy(i,j  ,k))*Dbdz  )               
      Ey(i,j,k) = Ey(i,j,k) +                                                 &
                (  (Hx(i  ,j,k)-Hx(i,j,k-1))*Dbdz +                           &
                   (Hz(i-1,j,k)-Hz(i,j,k  ))*Dbdx  )               
      Ez(i,j,k) = Ez(i,j,k) +                                                 &
                (  (Hy(i,j  ,k)-Hy(i-1,j,k))*Dbdx +                           &
                   (Hx(i,j-1,k)-Hx(i  ,j,k))*Dbdy  )
    end do
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO
#endif
#ifndef SPEC_CPU
!$OMP END PARALLEL 
#endif

END SUBROUTINE updateE_homo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       update_init - Initializes update variables
!
! DESCRIPTION
!       Initializes update variables
!
! METHOD
!       Cbdx, Cbdy, Cbdz, Dbdx, Dbdy and Dbdz are given constant (free space) 
!       values.
!
! SYNOPSIS
!       CALL update_init()
!
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Update_init()

USE globalvar_mod, ONLY : dxinv, dyinv, dzinv, dtdmu, dtdeps

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

Cbdx = dtdmu*dxinv
Cbdy = dtdmu*dyinv
Cbdz = dtdmu*dzinv

Dbdx = dtdeps*dxinv
Dbdy = dtdeps*dyinv
Dbdz = dtdeps*dzinv

END SUBROUTINE Update_init

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE update_mod
