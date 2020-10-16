!+
! NAME
!       UPML_mod - Absorbing boundary conditions using uniaxial PML
!
! DESCRIPTION
!       Absorbing boundary conditions using uniaxial PML. 
!
! METHOD
!       Absorbing boundary conditions using uniaxial PML according to Gedney,
!       IEEE Trans. Ant. Prop., vol. 44, no. 12, pp 1630-1639, Dec. 1996 and
!       Taflove (ed.), Advances in Computational Electrodynamics, 
!       Sect. 5.4-5.9, 1998.
!
! PUBLIC
!       SUBROUTINE UPMLinit
!       SUBROUTINE UPMLupdateH
!       SUBROUTINE UPMLupdateE
!       SUBROUTINE UPMLend
!       SUBROUTINE UPML_setparvar
!       FUNCTION   UPML_get_pml_cells
!
! HISTORY
!       Version       Date                 Name
!       Comments
!       -------------------------------------------
!	$Log: UPML.f90,v $
!	Revision 1.2  2003/09/26 20:15:18  ulfa
!	Added global computation of #bytes allocated.
!	
!	Revision 1.1  2003/09/23 14:06:52  ulfa
!	Initial version.
!	
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE UPML_mod
USE parameter_mod, ONLY : rfp

IMPLICIT NONE

PUBLIC UPMLupdateH, UPMLupdateE, UPMLinit, UPMLend, UPML_setparvar,           &
       UPML_get_pml_cells

PRIVATE
!
! Variables read from parser
!
integer        :: pml_cells = 0  ! Number of pml_cells
integer        :: prof_type      ! Type of profile for sigma
real(kind=rfp) :: R0             ! % of desired reflection
!
! Field variables:
real(kind=rfp), dimension(:,:,:), allocatable :: Dx_ilow, Dx_ihigh
real(kind=rfp), dimension(:,:,:), allocatable :: Dy_ilow, Dy_ihigh
real(kind=rfp), dimension(:,:,:), allocatable :: Dz_ilow, Dz_ihigh
real(kind=rfp), dimension(:,:,:), allocatable :: Dx_jlow, Dx_jhigh
real(kind=rfp), dimension(:,:,:), allocatable :: Dy_jlow, Dy_jhigh
real(kind=rfp), dimension(:,:,:), allocatable :: Dz_jlow, Dz_jhigh
real(kind=rfp), dimension(:,:,:), allocatable :: Dx_klow, Dx_khigh
real(kind=rfp), dimension(:,:,:), allocatable :: Dy_klow, Dy_khigh
real(kind=rfp), dimension(:,:,:), allocatable :: Dz_klow, Dz_khigh

real(kind=rfp), dimension(:,:,:), allocatable :: Bx_ilow, Bx_ihigh
real(kind=rfp), dimension(:,:,:), allocatable :: By_ilow, By_ihigh
real(kind=rfp), dimension(:,:,:), allocatable :: Bz_ilow, Bz_ihigh
real(kind=rfp), dimension(:,:,:), allocatable :: Bx_jlow, Bx_jhigh
real(kind=rfp), dimension(:,:,:), allocatable :: By_jlow, By_jhigh
real(kind=rfp), dimension(:,:,:), allocatable :: Bz_jlow, Bz_jhigh
real(kind=rfp), dimension(:,:,:), allocatable :: Bx_klow, Bx_khigh
real(kind=rfp), dimension(:,:,:), allocatable :: By_klow, By_khigh
real(kind=rfp), dimension(:,:,:), allocatable :: Bz_klow, Bz_khigh

! Updating coefficients:
real(kind=rfp), dimension(:), allocatable :: axe, aye, aze
real(kind=rfp), dimension(:), allocatable :: bxe, bye, bze
real(kind=rfp), dimension(:), allocatable :: cxe, cye, cze
real(kind=rfp), dimension(:), allocatable :: fxe, fye, fze

real(kind=rfp), dimension(:), allocatable :: axh, ayh, azh
real(kind=rfp), dimension(:), allocatable :: bxh, byh, bzh
real(kind=rfp), dimension(:), allocatable :: cxh, cyh, czh
real(kind=rfp), dimension(:), allocatable :: fxh, fyh, fzh

! Absorption coefficients:
real(kind=rfp), dimension(:), allocatable :: sigmx, sigmx_star 
real(kind=rfp), dimension(:), allocatable :: sigmy, sigmy_star
real(kind=rfp), dimension(:), allocatable :: sigmz, sigmz_star

real(kind=rfp), dimension(:), allocatable :: kappax, kappax_star
real(kind=rfp), dimension(:), allocatable :: kappay, kappay_star
real(kind=rfp), dimension(:), allocatable :: kappaz, kappaz_star

real(kind=rfp) :: epsinv
real(kind=rfp) :: muinv

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       UPML_setparvar - Sets parsed variables
!
! DESCRIPTION
!       Sets the private declared variables parsed in module readdata
!
! SYNOPSIS
!       CALL UPML_setparvar(par_pml_cells, par_R0, par_proftype)
!         integer, intent(in) :: par_pml_cells
!         real(kind=rfp), intent(in) :: par_R0    ! % of desired reflection
!         integer, intent(in) :: par_proftype     ! Type of profile for sigma
!
! ERRORS
!       No error handling
!
! HISTORY
!       Written by 흆e Rydell
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE UPML_setparvar(par_pml_cells, par_R0, par_proftype)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in)        :: par_pml_cells
real(kind=rfp), intent(in) :: par_R0
integer, intent(in)        :: par_proftype

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

pml_cells = par_pml_cells
prof_type = par_proftype
R0 = par_R0

END SUBROUTINE UPML_setparvar


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       UPMLallocate - Allocates variables for UPML
!
! DESCRIPTION
!       Allocations are collected here. Called from UPMLinit
!
! SYNOPSIS
!       call UPMLallocate(nx,ny,nz)
!         integer, intent(in) :: nx, ny, nz
!
! SEE ALSO
!       UPMLinit, UPMLend
!
! ERRORS
!       Uses errorcheck module
!
! HISTORY
!       Written by 흆e Rydell
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE UPMLallocate(nx,ny,nz)
USE parameter_mod, ONLY  : bytes_per_float
USE globalvar_mod, ONLY  : xstart, ystart, zstart, xstop, ystop, zstop,       &
                           bytes_allocated
USE errorcheck_mod, ONLY : check_allocate

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: nx, ny, nz

!------------------------------------------------------------------------------
!                      L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: allocstat, bytes

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

bytes = 0
allocate(Bx_ilow(xstart:1,ystart:ystop,zstart:zstop), STAT=allocstat)
call check_allocate(allocstat,'Bx_ilow',3)
allocate(By_ilow(xstart:1,ystart:ystop,zstart:zstop), STAT=allocstat)
call check_allocate(allocstat,'By_ilow',3)
allocate(Bz_ilow(xstart:1,ystart:ystop,zstart:zstop), STAT=allocstat)
call check_allocate(allocstat,'Bz_ilow',3)
bytes = bytes + (1-xstart+1)*(ystop-ystart+1)*(zstop-zstart+1)*3

allocate(Bx_ihigh(nx+1:xstop,ystart:ystop,zstart:zstop), STAT=allocstat) 
call check_allocate(allocstat,'Bx_ihigh',3)
allocate(By_ihigh(nx+1:xstop,ystart:ystop,zstart:zstop), STAT=allocstat)
call check_allocate(allocstat,'By_ihigh',3)
allocate(Bz_ihigh(nx+1:xstop,ystart:ystop,zstart:zstop), STAT=allocstat)
call check_allocate(allocstat,'Bz_ihigh',3)
bytes = bytes + (xstop-nx)*(ystop-ystart+1)*(zstop-zstart+1)*3

allocate(Bx_jlow(1:nx,ystart:1,zstart:zstop), STAT=allocstat)
call check_allocate(allocstat,'Bx_jlow',3)
allocate(By_jlow(1:nx,ystart:1,zstart:zstop), STAT=allocstat)
call check_allocate(allocstat,'By_jlow',3)
allocate(Bz_jlow(1:nx,ystart:1,zstart:zstop), STAT=allocstat)
call check_allocate(allocstat,'Bz_jlow',3)
bytes = bytes + nx*(1-ystart+1)*(zstop-zstart+1)*3

allocate(Bx_jhigh(1:nx,ny+1:ystop,zstart:zstop), STAT=allocstat) 
call check_allocate(allocstat,'Bx_jhigh',3)
allocate(By_jhigh(1:nx,ny+1:ystop,zstart:zstop), STAT=allocstat)
call check_allocate(allocstat,'By_jhigh',3)
allocate(Bz_jhigh(1:nx,ny+1:ystop,zstart:zstop), STAT=allocstat)
call check_allocate(allocstat,'Bz_jhigh',3)
bytes = bytes + nx*(ystop-ny)*(zstop-zstart+1)*3

allocate(Bx_klow(1:nx,1:ny,zstart:1), STAT=allocstat)
call check_allocate(allocstat,'Bx_klow',3)
allocate(By_klow(1:nx,1:ny,zstart:1), STAT=allocstat)
call check_allocate(allocstat,'By_klow',3)
allocate(Bz_klow(1:nx,1:ny,zstart:1), STAT=allocstat)
call check_allocate(allocstat,'Bz_klow',3)
bytes = bytes + nx*ny*(1-zstart+1)*3

allocate(Bx_khigh(1:nx,1:ny,nz+1:zstop), STAT=allocstat) 
call check_allocate(allocstat,'Bx_khigh',3)
allocate(By_khigh(1:nx,1:ny,nz+1:zstop), STAT=allocstat)
call check_allocate(allocstat,'By_khigh',3)
allocate(Bz_khigh(1:nx,1:ny,nz+1:zstop), STAT=allocstat)
call check_allocate(allocstat,'Bz_khigh',3)
bytes = bytes + nx*ny*(zstop-nz)*3

!------------------------------
allocate(Dx_ilow(xstart:1,ystart:ystop+1,zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'Dx_ilow',3)
allocate(Dy_ilow(xstart:1,ystart:ystop+1,zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'Dy_ilow',3)
allocate(Dz_ilow(xstart:1,ystart:ystop+1,zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'Dz_ilow',3)
bytes = bytes + (1-xstart+1)*(ystop-ystart+2)*(zstop-zstart+2)*3

allocate(Dx_ihigh(nx+1:xstop+1,ystart:ystop+1,zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'Dx_ihigh',3)
allocate(Dy_ihigh(nx+1:xstop+1,ystart:ystop+1,zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'Dy_ihigh',3)
allocate(Dz_ihigh(nx+1:xstop+1,ystart:ystop+1,zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'Dz_ihigh',3)
bytes = bytes + (xstop-nx+1)*(ystop-ystart+2)*(zstop-zstart+2)*3

allocate(Dx_jlow(1:nx,ystart:1,zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'Dx_jlow',3)
allocate(Dy_jlow(1:nx,ystart:1,zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'Dy_jlow',3)
allocate(Dz_jlow(1:nx,ystart:1,zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'Dz_jlow',3)
bytes = bytes + nx*(1-ystart+1)*(zstop-zstart+2)*3

allocate(Dx_jhigh(1:nx,ny+1:ystop+1,zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'Dx_jhigh',3)
allocate(Dy_jhigh(1:nx,ny+1:ystop+1,zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'Dy_jhigh',3)
allocate(Dz_jhigh(1:nx,ny+1:ystop+1,zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'Dz_jhigh',3)
bytes = bytes + nx*(ystop-ny+1)*(zstop-zstart+2)*3

allocate(Dx_klow(1:nx,1:ny,zstart:1), STAT=allocstat)
call check_allocate(allocstat,'Dx_klow',3)
allocate(Dy_klow(1:nx,1:ny,zstart:1), STAT=allocstat)
call check_allocate(allocstat,'Dy_klow',3)
allocate(Dz_klow(1:nx,1:ny,zstart:1), STAT=allocstat)
call check_allocate(allocstat,'Dz_klow',3)
bytes = bytes + nx*ny*(1-zstart+1)*3

allocate(Dx_khigh(1:nx,1:ny,nz+1:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'Dx_khigh',3)
allocate(Dy_khigh(1:nx,1:ny,nz+1:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'Dy_khigh',3)
allocate(Dz_khigh(1:nx,1:ny,nz+1:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'Dz_khigh',3)
bytes = bytes + nx*ny*(zstop-nz+1)*3

!------------------------------
allocate(sigmx(xstart:xstop+1), STAT=allocstat)
call check_allocate(allocstat,'sigmx',3)
allocate(sigmy(ystart:ystop+1), STAT=allocstat)
call check_allocate(allocstat,'sigmy',3)
allocate(sigmz(zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'sigmz',3)

allocate(sigmx_star(xstart:xstop+1), STAT=allocstat)
call check_allocate(allocstat,'sigmx_star',3)
allocate(sigmy_star(ystart:ystop+1), STAT=allocstat)
call check_allocate(allocstat,'sigmy_star',3)
allocate(sigmz_star(zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'sigmz_star',3)
!------------------------------
allocate(kappax(xstart:xstop+1), STAT=allocstat)
call check_allocate(allocstat,'kappax',3)
allocate(kappay(ystart:ystop+1), STAT=allocstat)
call check_allocate(allocstat,'kappay',3)
allocate(kappaz(zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'kappaz',3)

allocate(kappax_star(xstart:xstop+1), STAT=allocstat)
call check_allocate(allocstat,'kappax_star',3)
allocate(kappay_star(ystart:ystop+1), STAT=allocstat)
call check_allocate(allocstat,'kappay_star',3)
allocate(kappaz_star(zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'kappaz_star',3)
!------------------------------
allocate(axe(xstart:xstop+1), STAT=allocstat)
call check_allocate(allocstat,'axe',3)
allocate(aye(ystart:ystop+1), STAT=allocstat)
call check_allocate(allocstat,'aye',3)
allocate(aze(zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'aze',3)

allocate(bxe(xstart:xstop+1), STAT=allocstat)
call check_allocate(allocstat,'bxe',3)
allocate(bye(ystart:ystop+1), STAT=allocstat)
call check_allocate(allocstat,'bye',3)
allocate(bze(zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'bze',3)

allocate(cxe(xstart:xstop+1), STAT=allocstat)
call check_allocate(allocstat,'cxe',3)
allocate(cye(ystart:ystop+1), STAT=allocstat)
call check_allocate(allocstat,'cye',3)
allocate(cze(zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'cze',3)

allocate(fxe(xstart:xstop+1), STAT=allocstat)
call check_allocate(allocstat,'fxe',3)
allocate(fye(ystart:ystop+1), STAT=allocstat)
call check_allocate(allocstat,'fye',3)
allocate(fze(zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'fze',3)
!------------------------------
allocate(axh(xstart:xstop+1), STAT=allocstat)
call check_allocate(allocstat,'axh',3)
allocate(ayh(ystart:ystop+1), STAT=allocstat)
call check_allocate(allocstat,'ayh',3)
allocate(azh(zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'azh',3)

allocate(bxh(xstart:xstop+1), STAT=allocstat)
call check_allocate(allocstat,'bxh',3)
allocate(byh(ystart:ystop+1), STAT=allocstat)
call check_allocate(allocstat,'byh',3)
allocate(bzh(zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'bzh',3)

allocate(cxh(xstart:xstop+1), STAT=allocstat)
call check_allocate(allocstat,'cxh',3)
allocate(cyh(ystart:ystop+1), STAT=allocstat)
call check_allocate(allocstat,'cyh',3)
allocate(czh(zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'czh',3)

allocate(fxh(xstart:xstop+1), STAT=allocstat)
call check_allocate(allocstat,'fxh',3)
allocate(fyh(ystart:ystop+1), STAT=allocstat)
call check_allocate(allocstat,'fyh',3)
allocate(fzh(zstart:zstop+1), STAT=allocstat)
call check_allocate(allocstat,'fzh',3)

bytes = bytes*bytes_per_float
write(*,*) 'Fields allocated in UPML_mod, bytes used = ', bytes
bytes_allocated = bytes_allocated + bytes

END SUBROUTINE UPMLallocate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       UPMLinit - Initializes the UPML absorption variables
!
! DESCRIPTION
!       Initializes the UPML variables and updating coefficients. The profile
!       for sigma is a polynomial of order prof_type. If prof_type==0 then
!       geometric progression is used. Variables are allocated in UPMLallocate.
!
! METHOD
!       Sigma ranges from ?start to *stop+1. The higher end is filled first
!       and the lower end is the filled by mirroring the higher end.
!       Sigma(1) and sigma(n+1) both lie on the border between the interior
!       and the UPML region. They are both equal to zero since the fields in
!       cell 1 are split between UPML and the interior region, unlike those
!       in cell n?+1.
!
! SYNOPSIS
!       call UPMLinit(nx,ny,nz)
!         integer, intent(in) :: nx, ny, nz
!
! ERRORS
!       The code stops if incorrect parameters are specified or if mat_type = 4
!
! SEE ALSO
!       UPMLupdateH, UPMLupdateE, UPMLallocate
!
! HISTORY
!       Written by Ulf Andersson and 흆e Rydell
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE UPMLinit(nx,ny,nz)
USE parameter_mod, ONLY : eps0, mu0
USE globalvar_mod, ONLY : xstart, ystart, zstart, xstop, ystop, zstop, dx,    &
                          dy, dz, dxinv, dyinv, dzinv, dt, c0
USE PEC_mod,      ONLY : PEC_found

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: nx, ny, nz

!------------------------------------------------------------------------------
!                      L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

real(kind=rfp), parameter :: g = 2.15_rfp  ! Magic number for geom. progr.
integer                   :: i, j, k, m
integer                   :: xpml_start, ypml_start, zpml_start
real(kind=rfp)            :: sigma0, gpfact
real(kind=rfp)            :: rho, sigma_max_X, sigma_max_Y, sigma_max_Z
real(kind=rfp)            :: deltaX, deltaY, deltaZ
real(kind=rfp)            :: kappamax, q

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

call UPMLallocate(nx,ny,nz)   ! Allocate variables

! Initialize fields:

Dx_ilow  = 0.0_rfp;  Dy_ilow  = 0.0_rfp;  Dz_ilow  = 0.0_rfp
Dx_ihigh = 0.0_rfp;  Dy_ihigh = 0.0_rfp;  Dz_ihigh = 0.0_rfp
Dx_jlow  = 0.0_rfp;  Dy_jlow  = 0.0_rfp;  Dz_jlow  = 0.0_rfp
Dx_jhigh = 0.0_rfp;  Dy_jhigh = 0.0_rfp;  Dz_jhigh = 0.0_rfp
Dx_klow  = 0.0_rfp;  Dy_klow  = 0.0_rfp;  Dz_klow  = 0.0_rfp
Dx_khigh = 0.0_rfp;  Dy_khigh = 0.0_rfp;  Dz_khigh = 0.0_rfp

Bx_ilow  = 0.0_rfp;  By_ilow  = 0.0_rfp;  Bz_ilow  = 0.0_rfp
Bx_ihigh = 0.0_rfp;  By_ihigh = 0.0_rfp;  Bz_ihigh = 0.0_rfp
Bx_jlow  = 0.0_rfp;  By_jlow  = 0.0_rfp;  Bz_jlow  = 0.0_rfp
Bx_jhigh = 0.0_rfp;  By_jhigh = 0.0_rfp;  Bz_jhigh = 0.0_rfp
Bx_klow  = 0.0_rfp;  By_klow  = 0.0_rfp;  Bz_klow  = 0.0_rfp
Bx_khigh = 0.0_rfp;  By_khigh = 0.0_rfp;  Bz_khigh = 0.0_rfp

write(*,*)
write(*,*) 'pml_cells = ',  pml_cells
R0 = R0*0.01_rfp
! write(*,*) 'R0 = ', R0
! Exact comparison between real*8 and real*8 values should be OK here (ulfa)
if ((dx/=dy).or.(dx/=dz)) then
  write(*,*) 'FATAL ERROR! PML assumes dx=dy=dz!'
  stop 
end if

xpml_start = nx+1
ypml_start = ny+1
zpml_start = nz+1

deltaX = pml_cells/dxinv
! write(*,*) 'deltaX = ', deltaX
 
deltaY = pml_cells/dyinv
! write(*,*) 'deltaY = ', deltaY

deltaZ = pml_cells/dzinv
! write(*,*) 'deltaZ = ', deltaZ

! Initializations of sigma and sigma_star:

sigmx = 0.0_rfp;  sigmx_star = 0.0_rfp
sigmy = 0.0_rfp;  sigmy_star = 0.0_rfp
sigmz = 0.0_rfp;  sigmz_star = 0.0_rfp

! Initialize variables to their default values (used in regions
! where the corresponding sigma is 0):

kappax = 1.0_rfp;  kappax_star = 1.0_rfp
kappay = 1.0_rfp;  kappay_star = 1.0_rfp
kappaz = 1.0_rfp;  kappaz_star = 1.0_rfp

axe = 1.0_rfp;  axh = 1.0_rfp
aye = 1.0_rfp;  ayh = 1.0_rfp
aze = 1.0_rfp;  azh = 1.0_rfp

bxe = dt;  bxh = dt
bye = dt;  byh = dt
bze = dt;  bzh = dt

cxe = 1.0_rfp/dt;  cxh = 1.0_rfp/dt
cye = 1.0_rfp/dt;  cyh = 1.0_rfp/dt
cze = 1.0_rfp/dt;  czh = 1.0_rfp/dt

fxe = 1.0_rfp/dt;  fxh = 1.0_rfp/dt
fye = 1.0_rfp/dt;  fyh = 1.0_rfp/dt
fze = 1.0_rfp/dt;  fzh = 1.0_rfp/dt

!****************************Higher PML*******************************!

! Sigma and sigma_star are the same but they are
! shifted half a cell size.

!-----------------------Geometric progression--------------------------

q =  1.0_rfp    ! Used for kappa

if (prof_type==0) then

  write(*,*) 'Using geometric progression profile for sigma'

  sigma0 = eps0*c0*log(g)/(2.0_rfp*dx*(g**PML_cells-1))*log(R0)
  gpfact = sigma0*(g-1)/(sqrt(g)*log(g))

  do i=xpml_start,xstop+1
    rho = (i-xpml_start)*dx

    sigmx(i)      = -gpfact*g**(rho/dx)
    sigmx_star(i) = -gpfact*g**(rho/dx+0.5)

    kappax(i)      = q**(rho/dx)
    kappax_star(i) = q**(rho/dx+0.5_rfp)
  end do

  sigmx(xpml_start) = -sigma0*(sqrt(g)-1)/log(g)

! Note that sigmx_star(xstop+1) is never used since it lies
! outside the PML layer
!  sigma_max_X = sigmx(xstop+1)

  sigma0 = ((eps0*c0*log(g))/(2.0_rfp*dy*(g**PML_cells-1)))*log(R0)
  gpfact = sigma0*(g-1)/(sqrt(g)*log(g))

  do j=ypml_start,ystop+1
    rho = (j-ypml_start)*dy

    sigmy(j)      = -gpfact*g**(rho/dy)
    sigmy_star(j) = -gpfact*g**(rho/dy+0.5_rfp)

    kappay(j)      = q**(rho/dy)
    kappay_star(j) = q**(rho/dy+0.5_rfp)
  end do

  sigmy(ypml_start) = -sigma0*(sqrt(g)-1)/log(g)

! Note that sigmy_star(ystop+1) is never used since it lies
! outside the PML layer
!  sigma_max_Y = sigmy(ystop+1)

  sigma0 = ((eps0*c0*log(g))/(2.0_rfp*dz*(g**PML_cells-1)))*log(R0)
  gpfact = sigma0*(g-1)/(sqrt(g)*log(g))

  do k=zpml_start,zstop+1
    rho = (k-zpml_start)*dz

    sigmz(k)      = -gpfact*g**(rho/dz)
    sigmz_star(k) = -gpfact*g**(rho/dz+0.5_rfp)

    kappaz(k)      = q**(rho/dz)
    kappaz_star(k) = q**(rho/dz+0.5_rfp)
  end do

  sigmz(zpml_start) = -sigma0*(sqrt(g)-1)/log(g)

! Note that sigmz_star(zstop+1) is never used since it lies
! outside the PML layer
!  sigma_max_Z = sigmz(zstop+1)

!-------------------------Polynomial profile---------------------------

else

  m = prof_type   ! m is the degree of the polynomial
  write(*,*) 'Using polynomial profile for sigma, with degree', m

  kappamax = 1.0_rfp

  sigma_max_X = -(m+1)*eps0*c0*log(R0)/(2*deltaX) ! log is nat. log. (ln)

  do i=xpml_start,xstop+1
    rho = (i-xpml_start)*dx

    sigmx(i)      = (sigma_max_X/(dx*(m+1)*deltaX**prof_type))*               &
                    ((rho+dx/2)**(m+1)-(rho-dx/2)**(m+1))
    sigmx_star(i) = (sigma_max_X/(dx*(m+1)*deltaX**prof_type))*               &
                    ((rho+dx)**(m+1)-rho**(m+1))

    kappax(i)      = 1.0_rfp + (kappamax-1.0_rfp)*(rho/deltaX)**m
    kappax_star(i) = 1.0_rfp + (kappamax-1.0_rfp)*(rho/deltaX + 0.5_rfp*dx)**m
  end do

  sigmx(xpml_start) = sigma_max_X/(deltaX**m*(m+1)*dx)*(dx/2)**(m+1)

! Note that sigmx_star(xstop+1) is never used since it lies
! outside the PML layer

  sigma_max_Y = -(m+1)*eps0*c0*log(R0)/(2*deltaY) ! log is nat. log. (ln)

  do j=ypml_start,ystop+1
    rho = (j-ypml_start)*dy

    sigmy(j)      = (sigma_max_Y/(dy*(m+1)*deltaY**prof_type))*               &
                    ((rho+dy/2)**(m+1)-(rho-dy/2)**(m+1))
    sigmy_star(j) = (sigma_max_Y/(dy*(m+1)*deltaY**prof_type))*               &
                    ((rho+dy)**(m+1)-rho**(m+1))

    kappay(j)      = 1.0_rfp + (kappamax-1.0_rfp)*(rho/deltaY)**m
    kappay_star(j) = 1.0_rfp + (kappamax-1.0_rfp)*(rho/deltaY + 0.5_rfp*dy)**m
  end do

  sigmy(ypml_start) = sigma_max_Y/(deltaY**m*(m+1)*dy)*(dy/2)**(m+1)

! Note that sigmy_star(ystop+1) is never used since it lies
! outside the PML layer

  sigma_max_Z = -(m+1)*eps0*c0*log(R0)/(2*deltaZ) ! log is nat. log. (ln)

  do k=zpml_start,zstop+1
    rho = (k-zpml_start)*dz

    sigmz(k)      = (sigma_max_Z/(dz*(m+1)*deltaZ**prof_type))*               &
                    ((rho+dz/2)**(m+1)-(rho-dz/2)**(m+1))
    sigmz_star(k) = (sigma_max_Z/(dz*(m+1)*deltaZ**prof_type))*               &
                    ((rho+dz)**(m+1)-rho**(m+1))

    kappaz(k)      = 1.0_rfp + (kappamax-1.0_rfp)*(rho/deltaZ)**m
    kappaz_star(k) = 1.0_rfp + (kappamax-1.0_rfp)*(rho/deltaZ + 0.5_rfp*dz)**m
  end do

  sigmz(zpml_start) = sigma_max_Z/(deltaZ**m*(m+1)*dz)*(dz/2)**(m+1)

! Note that sigmz_star(zstop+1) is never used since it lies
! outside the PML layer

end if

!***************************Lower PML********************************!
! (Mirroring of higher PML)

do i=xstart,1
  sigmx(i)       = sigmx(xstop-pml_cells+2-i)
  sigmx_star(i)  = sigmx_star(xstop-pml_cells+1-i)

  kappax(i)      = kappax(xstop-pml_cells+2-i)
  kappax_star(i) = kappax_star(xstop-pml_cells+1-i)
end do

do j=ystart,1
  sigmy(j)       = sigmy(ystop-pml_cells+2-j)
  sigmy_star(j)  = sigmy_star(ystop-pml_cells+1-j)

  kappay(j)      = kappay(ystop-pml_cells+2-j)
  kappay_star(j) = kappay_star(ystop-pml_cells+1-j)
end do

do k=zstart,1
  sigmz(k)       = sigmz(zstop-pml_cells+2-k)
  sigmz_star(k)  = sigmz_star(zstop-pml_cells+1-k)

  kappaz(k)      = kappaz(zstop-pml_cells+2-k)
  kappaz_star(k) = kappaz_star(zstop-pml_cells+1-k)
end do

!******************************************************************************

! Updating coefficients for the electric and magnetic fields:

! For higher and lower UPML, otherwise default values as above.
! (Sigma and sigma_star are the same but they are shifted 0.5 cell size.)

! Note that the coefficients beginning with c or f and ending in e are used
! for all fields situated at the edge of a cell (in their respective
! direction). Those ending in an h are used for fields at the center of a cell
! (also E fields). These coefficients should be renamed to avoid confusion.

do i=xpml_start,xstop+1
  axe(i) = (2.0_rfp*eps0*kappax(i)-sigmx(i)*dt)/                              &
           (2.0_rfp*eps0*kappax(i)+sigmx(i)*dt)
  axh(i) = (2.0_rfp*eps0*kappax_star(i)-sigmx_star(i)*dt)/                    &
           (2.0_rfp*eps0*kappax_star(i)+sigmx_star(i)*dt)

  bxe(i) = 2.0_rfp*eps0*dt / (2.0_rfp*eps0*kappax(i)+sigmx(i)*dt)
  bxh(i) = 2.0_rfp*eps0*dt / (2.0_rfp*eps0*kappax_star(i)+sigmx_star(i)*dt)

  cxe(i) = kappax(i)/dt + sigmx(i)/(2.0_rfp*eps0)
  cxh(i) = kappax_star(i)/dt + sigmx_star(i)/(2.0_rfp*eps0)

  fxe(i) = kappax(i)/dt - sigmx(i)/(2.0_rfp*eps0)
  fxh(i) = kappax_star(i)/dt - sigmx_star(i)/(2.0_rfp*eps0)
end do

do i=xstart,1   ! Mirroring
  axe(i) = axe(xstop-pml_cells+2-i)
  bxe(i) = bxe(xstop-pml_cells+2-i)
  cxe(i) = cxe(xstop-pml_cells+2-i)
  fxe(i) = fxe(xstop-pml_cells+2-i)

  axh(i) = axh(xstop-pml_cells+1-i)
  bxh(i) = bxh(xstop-pml_cells+1-i)
  cxh(i) = cxh(xstop-pml_cells+1-i)
  fxh(i) = fxh(xstop-pml_cells+1-i)
end do
!---------
do j=ypml_start,ystop+1
  aye(j) = (2.0_rfp*eps0*kappay(j)-sigmy(j)*dt)/                              &
           (2.0_rfp*eps0*kappay(j)+sigmy(j)*dt)
  ayh(j) = (2.0_rfp*eps0*kappay_star(j)-sigmy_star(j)*dt)/                    &
           (2.0_rfp*eps0*kappay_star(j)+sigmy_star(j)*dt)

  bye(j) = 2.0_rfp*eps0*dt / (2.0_rfp*eps0*kappay(j)+sigmy(j)*dt)
  byh(j) = 2.0_rfp*eps0*dt / (2.0_rfp*eps0*kappay_star(j)+sigmy_star(j)*dt)

  cye(j) = kappay(j)/dt + sigmy(j)/(2.0_rfp*eps0)
  cyh(j) = kappay_star(j)/dt + sigmy_star(j)/(2.0_rfp*eps0)

  fye(j) = kappay(j)/dt - sigmy(j)/(2.0_rfp*eps0)
  fyh(j) = kappay_star(j)/dt - sigmy_star(j)/(2.0_rfp*eps0)
end do

do j=ystart,1   ! Mirroring
  aye(j) = aye(ystop-pml_cells+2-j)
  bye(j) = bye(ystop-pml_cells+2-j)
  cye(j) = cye(ystop-pml_cells+2-j)
  fye(j) = fye(ystop-pml_cells+2-j)

  ayh(j) = ayh(ystop-pml_cells+1-j)
  byh(j) = byh(ystop-pml_cells+1-j)
  cyh(j) = cyh(ystop-pml_cells+1-j)
  fyh(j) = fyh(ystop-pml_cells+1-j)
end do
!---------
do k=zpml_start,zstop+1
  aze(k) = (2.0_rfp*eps0*kappaz(k)-sigmz(k)*dt)/                              &
           (2.0_rfp*eps0*kappaz(k)+sigmz(k)*dt)
  azh(k) = (2.0_rfp*eps0*kappaz_star(k)-sigmz_star(k)*dt)/                    &
           (2.0_rfp*eps0*kappaz_star(k)+sigmz_star(k)*dt)

  bze(k) = 2.0_rfp*eps0*dt / (2.0_rfp*eps0*kappaz(k)+sigmz(k)*dt)
  bzh(k) = 2.0_rfp*eps0*dt / (2.0_rfp*eps0*kappaz_star(k)+sigmz_star(k)*dt)

  cze(k) = kappaz(k)/dt + sigmz(k)/(2.0_rfp*eps0)
  czh(k) = kappaz_star(k)/dt + sigmz_star(k)/(2.0_rfp*eps0)

  fze(k) = kappaz(k)/dt - sigmz(k)/(2.0_rfp*eps0)
  fzh(k) = kappaz_star(k)/dt - sigmz_star(k)/(2.0_rfp*eps0)
end do

do k=zstart,1   ! Mirroring
  aze(k) = aze(zstop-pml_cells+2-k)
  bze(k) = bze(zstop-pml_cells+2-k)
  cze(k) = cze(zstop-pml_cells+2-k)
  fze(k) = fze(zstop-pml_cells+2-k)

  azh(k) = azh(zstop-pml_cells+1-k)
  bzh(k) = bzh(zstop-pml_cells+1-k)
  czh(k) = czh(zstop-pml_cells+1-k)
  fzh(k) = fzh(zstop-pml_cells+1-k)
end do

epsinv = 1.0_rfp/eps0
muinv  = 1.0_rfp/mu0

END SUBROUTINE UPMLinit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       UPMLupdateH - Updates the H-fields in the UPML layers
!
! DESCRIPTION
!       Updates the H-fields in the UPML layers
!
! METHOD
!       Corners and edges are merged with sides so that there are six
!       PML blocks. These six blocks are:
!
!         Block 1: i=xstart,0   ; j=ystart,ystop ; k=zstart,zstop
!         Block 2: i=nx+1,xstop ; j=ystart,ystop ; k=zstart,zstop
!         Block 3: i=1,nx       ; j=ystart,0     ; k=zstart,zstop
!         Block 4: i=1,nx       ; j=ny+1,ystop   ; k=zstart,zstop
!         Block 5: i=1,nx       ; j=1,ny         ; k=zstart,0
!         Block 6: i=1,nx       ; j=1,ny         ; k=nz+1,zstop
!
!       The blocks are updated in order. The coefficients are initialized in
!       UPMLinit.
!       Note that the H fields on the lower border to UPML, Hx(1,:,:),
!       Hy(:,1,:) and Hz(:,:,1) belong to the interior region but they
!       should really be updated by UPML (just like Hx(nx+1,:,:) etc).
!       Hence the contribution to these fields from update is subtracted
!       here and they are updated again.
!
! SYNOPSIS
!       call UPMLupdateH(nx,ny,nz,Hx,Hy,Hz,Ex,Ey,Ez)
!          integer, intent(in) :: nx, ny, nz
!          real(kind=rfp), intent(inout), dimension(xstart:xstop+1,           &
!                          ystart:ystop+1,zstart:zstop+1) :: Ex, Ey, Ez
!          real(kind=rfp), intent(inout), dimension(xstart:xstop,             &
!                          ystart:ystop  ,zstart:zstop  ) :: Hx, Hy, Hz
!
! ERRORS
!       No error handling
!
! SEE ALSO
!       UPMLupdateE & UPMLinit
!
! HISTORY
!       Written by 흆e Rydell
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE UPMLupdateH(nx,ny,nz,Hx,Hy,Hz,Ex,Ey,Ez)
USE globalvar_mod, ONLY : xstart, ystart, zstart, xstop, ystop, zstop,        &
                          dxinv, dyinv, dzinv, dtdmu

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: nx, ny, nz
real(kind=rfp), intent(inout),                                                &
         dimension(xstart:xstop+1,ystart:ystop+1,zstart:zstop+1) :: Ex, Ey, Ez
real(kind=rfp), intent(inout),                                                &
         dimension(xstart:xstop  ,ystart:ystop  ,zstart:zstop  ) :: Hx, Hy, Hz

!------------------------------------------------------------------------------
!                      L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: i, j, k
real(kind=rfp) :: Bxold, Byold, Bzold

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

!*********************** Block 1 (ilow) ************************

#ifndef SPEC_CPU
!$OMP PARALLEL 
#endif

#ifndef SPEC_CPU
!$OMP DO PRIVATE(i,j,k,Bxold,Byold,Bzold)
#endif
do k=zstart,zstop
  do j=ystart,ystop
    do i=xstart,0

      Bxold = Bx_ilow(i,j,k)

      Bx_ilow(i,j,k) = ayh(j) * Bx_ilow(i,j,k) +                              &
                       byh(j) * ((Ey(i,j,k+1)-Ey(i,j,k  ))*dzinv +            &
                                 (Ez(i,j,k  )-Ez(i,j+1,k))*dyinv)
                                                       
      Hx(i,j,k) = azh(k) * Hx(i,j,k) +                                        &
                  bzh(k) * (cxe(i)*Bx_ilow(i,j,k) - fxe(i)*Bxold) * muinv
      !------
      Byold = By_ilow(i,j,k)

      By_ilow(i,j,k) = azh(k) * By_ilow(i,j,k) +                              &
                       bzh(k) * ((Ez(i+1,j,k)-Ez(i,j,k  ))*dxinv +            &
                                 (Ex(i,j,k  )-Ex(i,j,k+1))*dzinv)

      Hy(i,j,k) = axh(i) * Hy(i,j,k) +                                        &
                  bxh(i) * (cye(j)*By_ilow(i,j,k) - fye(j)*Byold) * muinv
      !------
      Bzold = Bz_ilow(i,j,k)

      Bz_ilow(i,j,k) = axh(i) * Bz_ilow(i,j,k) +                              &
                       bxh(i) * ((Ex(i,j+1,k)-Ex(i,j,k  ))*dyinv +            &
                                 (Ey(i,j,k  )-Ey(i+1,j,k))*dxinv)

      Hz(i,j,k) = ayh(j) * Hz(i,j,k) +                                        &
                  byh(j) * (cze(k)*Bz_ilow(i,j,k) - fze(k)*Bzold) * muinv
    end do
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

!
! Hx at i=1 is already updated in updateH_homo but it should be updated 
! according to UPML. The updating is canceled here and done again using UPML.

i = 1
#ifndef SPEC_CPU
!$OMP DO PRIVATE(j,k,Bxold)
#endif
do k=1,nz
  do j=1,ny

    Hx(i,j,k) = Hx(i,j,k) - dtdmu*((Ey(i,j,k+1)-Ey(i,j  ,k))*dzinv +          &
                                   (Ez(i,j,k  )-Ez(i,j+1,k))*dyinv)

    Bxold = Bx_ilow(i,j,k)

    Bx_ilow(i,j,k) = ayh(j) * Bx_ilow(i,j,k) +                                &
                     byh(j) * ((Ey(i,j,k+1)-Ey(i,j,k  ))*dzinv +              &
                               (Ez(i,j,k  )-Ez(i,j+1,k))*dyinv)
                                                       
    Hx(i,j,k) = azh(k) * Hx(i,j,k) +                                          &
                bzh(k) * (cxe(i)*Bx_ilow(i,j,k) - fxe(i)*Bxold) * muinv
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

!************************ Block 2 (ihigh) ************************

#ifndef SPEC_CPU
!$OMP DO PRIVATE(i,j,k,Bxold,Byold,Bzold)
#endif
do k=zstart,zstop
  do j=ystart,ystop
    do i=nx+1,xstop

      Bxold = Bx_ihigh(i,j,k)

      Bx_ihigh(i,j,k) = ayh(j) * Bx_ihigh(i,j,k) +                            &
                        byh(j) * ((Ey(i,j,k+1)-Ey(i,j,k  ))*dzinv +           &
                                  (Ez(i,j,k  )-Ez(i,j+1,k))*dyinv)

      Hx(i,j,k) = azh(k) * Hx(i,j,k) +                                        &
                  bzh(k) * (cxe(i)*Bx_ihigh(i,j,k) - fxe(i)*Bxold) * muinv
      !------
      Byold = By_ihigh(i,j,k)

      By_ihigh(i,j,k) = azh(k) * By_ihigh(i,j,k) +                            &
                        bzh(k) * ((Ez(i+1,j,k)-Ez(i,j,k  ))*dxinv +           &
                                  (Ex(i,j,k  )-Ex(i,j,k+1))*dzinv)

      Hy(i,j,k) = axh(i) * Hy(i,j,k) +                                        &
                  bxh(i) * (cye(j)*By_ihigh(i,j,k) - fye(j)*Byold) * muinv
      !------
      Bzold = Bz_ihigh(i,j,k)

      Bz_ihigh(i,j,k) = axh(i) * Bz_ihigh(i,j,k) +                            &
                        bxh(i) * ((Ex(i,j+1,k)-Ex(i,j,k  ))*dyinv +           &
                                  (Ey(i,j,k  )-Ey(i+1,j,k))*dxinv)

      Hz(i,j,k) = ayh(j) * Hz(i,j,k) +                                        &
                  byh(j) * (cze(k)*Bz_ihigh(i,j,k) - fze(k)*Bzold) * muinv
    end do
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

!********************* Block 3 (jlow) ************************

#ifndef SPEC_CPU
!$OMP DO PRIVATE(i,j,k,Bxold,Byold,Bzold)
#endif
do k=zstart,zstop
  do j=ystart,0
    do i=1,nx

      Bxold = Bx_jlow(i,j,k)

      Bx_jlow(i,j,k) = ayh(j) * Bx_jlow(i,j,k) +                              &
                       byh(j) * ((Ey(i,j,k+1)-Ey(i,j,k  ))*dzinv +            &
                                 (Ez(i,j,k  )-Ez(i,j+1,k))*dyinv)

      Hx(i,j,k) = azh(k) * Hx(i,j,k) +                                        &
                  bzh(k) * (cxe(i)*Bx_jlow(i,j,k) - fxe(i)*Bxold) * muinv
      !------
      Byold = By_jlow(i,j,k)

      By_jlow(i,j,k) = azh(k) * By_jlow(i,j,k) +                              &
                       bzh(k) * ((Ez(i+1,j,k)-Ez(i,j,k  ))*dxinv +            &
                                 (Ex(i,j,k  )-Ex(i,j,k+1))*dzinv)

      Hy(i,j,k) = axh(i) * Hy(i,j,k) +                                        &
                  bxh(i) * (cye(j)*By_jlow(i,j,k) - fye(j)*Byold) * muinv
      !------
      Bzold = Bz_jlow(i,j,k)

      Bz_jlow(i,j,k) = axh(i) * Bz_jlow(i,j,k) +                              &
                       bxh(i) * ((Ex(i,j+1,k)-Ex(i,j,k  ))*dyinv +            &
                                 (Ey(i,j,k  )-Ey(i+1,j,k))*dxinv)

      Hz(i,j,k) = ayh(j) * Hz(i,j,k) +                                        &
                  byh(j) * (cze(k)*Bz_jlow(i,j,k) - fze(k)*Bzold) * muinv
    end do
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

!
! Hy at j=1 is already updated in updateH_homo but it should be updated 
! according to UPML. The updating is canceled here and done again using UPML.
j=1
#ifndef SPEC_CPU
!$OMP DO PRIVATE(i,k,Byold)
#endif
do k=1,nz
  do i=1,nx

    Hy(i,j,k) = Hy(i,j,k) - dtdmu*((Ez(i+1,j,k)-Ez(i,j,k  ))*dxinv +          &
                                   (Ex(i,j,k  )-Ex(i,j,k+1))*dzinv)

    Byold = By_jlow(i,j,k)

    By_jlow(i,j,k) = azh(k) * By_jlow(i,j,k) +                                &
                     bzh(k) * ((Ez(i+1,j,k)-Ez(i,j,k  ))*dxinv +              &
                               (Ex(i,j,k  )-Ex(i,j,k+1))*dzinv)

    Hy(i,j,k) = axh(i) * Hy(i,j,k) +                                          &
                bxh(i) * (cye(j)*By_jlow(i,j,k) - fye(j)*Byold) * muinv

  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

!********************** Block 4 (jhigh) ************************

#ifndef SPEC_CPU
!$OMP DO PRIVATE(i,j,k,Bxold,Byold,Bzold)
#endif
do k=zstart,zstop
  do j=ny+1,ystop
    do i=1,nx

      Bxold = Bx_jhigh(i,j,k)

      Bx_jhigh(i,j,k) = ayh(j) * Bx_jhigh(i,j,k) +                            &
                        byh(j) * ((Ey(i,j,k+1)-Ey(i,j,k  ))*dzinv +           &
                                  (Ez(i,j,k  )-Ez(i,j+1,k))*dyinv)

      Hx(i,j,k) = azh(k) * Hx(i,j,k) +                                        &
                  bzh(k) * (cxe(i)*Bx_jhigh(i,j,k) - fxe(i)*Bxold) * muinv
      !------
      Byold = By_jhigh(i,j,k)

      By_jhigh(i,j,k) = azh(k) * By_jhigh(i,j,k) +                            &
                        bzh(k) * ((Ez(i+1,j,k)-Ez(i,j,k  ))*dxinv +           &
                                  (Ex(i,j,k  )-Ex(i,j,k+1))*dzinv)

      Hy(i,j,k) = axh(i) * Hy(i,j,k) +                                        &
                  bxh(i) * (cye(j)*By_jhigh(i,j,k) - fye(j)*Byold) * muinv
      !------
      Bzold = Bz_jhigh(i,j,k)

      Bz_jhigh(i,j,k) = axh(i) * Bz_jhigh(i,j,k) +                            &
                        bxh(i) * ((Ex(i,j+1,k)-Ex(i,j,k  ))*dyinv +           &
                                  (Ey(i,j,k  )-Ey(i+1,j,k))*dxinv)

      Hz(i,j,k) = ayh(j) * Hz(i,j,k) +                                        &
                  byh(j) * (cze(k)*Bz_jhigh(i,j,k) - fze(k)*Bzold) * muinv
    end do   
  end do 
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

!********************** Block 5 (klow) ************************

#ifndef SPEC_CPU
!$OMP DO PRIVATE(i,j,k,Bxold,Byold,Bzold)
#endif
do k=zstart,0
  do j=1,ny
    do i=1,nx

      Bxold = Bx_klow(i,j,k)

      Bx_klow(i,j,k) = ayh(j) * Bx_klow(i,j,k) +                              &
                       byh(j) * ((Ey(i,j,k+1)-Ey(i,j,k  ))*dzinv +            &
                                 (Ez(i,j,k  )-Ez(i,j+1,k))*dyinv)

      Hx(i,j,k) = azh(k) * Hx(i,j,k) +                                        &
                  bzh(k) * (cxe(i)*Bx_klow(i,j,k) - fxe(i)*Bxold) * muinv
      !------
      Byold = By_klow(i,j,k)

      By_klow(i,j,k) = azh(k) * By_klow(i,j,k) +                              &
                       bzh(k) * ((Ez(i+1,j,k)-Ez(i,j,k  ))*dxinv +            &
                                 (Ex(i,j,k  )-Ex(i,j,k+1))*dzinv)

      Hy(i,j,k) = axh(i) * Hy(i,j,k) +                                        &
                  bxh(i) * (cye(j)*By_klow(i,j,k) - fye(j)*Byold) * muinv
      !------
      Bzold = Bz_klow(i,j,k)

      Bz_klow(i,j,k) = axh(i) * Bz_klow(i,j,k) +                              &
                       bxh(i) * ((Ex(i,j+1,k)-Ex(i,j,k  ))*dyinv +            &
                                 (Ey(i,j,k  )-Ey(i+1,j,k))*dxinv)

      Hz(i,j,k) = ayh(j) * Hz(i,j,k) +                                        &
                  byh(j) * (cze(k)*Bz_klow(i,j,k) - fze(k)*Bzold) * muinv
    end do
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

!
! Hz at k=1 is already updated in updateH_homo but it should be updated 
! according to UPML. The updating is canceled here and done again using UPML.
k=1
#ifndef SPEC_CPU
!$OMP DO PRIVATE(i,j,Bzold)
#endif
do j=1,ny
  do i=1,nx

    Hz(i,j,k) = Hz(i,j,k) - dtdmu*((Ex(i,j+1,k)-Ex(i,j,k  ))*dyinv +          &
                                   (Ey(i,j,k  )-Ey(i+1,j,k))*dxinv)

    Bzold = Bz_klow(i,j,k)

    Bz_klow(i,j,k) = axh(i) * Bz_klow(i,j,k) +                                &
                     bxh(i) * ((Ex(i,j+1,k)-Ex(i,j,k  ))*dyinv +              &
                               (Ey(i,j,k  )-Ey(i+1,j,k))*dxinv)

    Hz(i,j,k) = ayh(j) * Hz(i,j,k) +                                          &
                byh(j) * (cze(k)*Bz_klow(i,j,k) - fze(k)*Bzold) * muinv
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

!********************** Block 6 (khigh) ************************

#ifndef SPEC_CPU
!$OMP DO PRIVATE(i,j,k,Bxold,Byold,Bzold)
#endif
do k=nz+1,zstop
  do j=1,ny
    do i=1,nx

      Bxold = Bx_khigh(i,j,k)

      Bx_khigh(i,j,k) = ayh(j) * Bx_khigh(i,j,k) +                            &
                        byh(j) * ((Ey(i,j,k+1)-Ey(i,j,k  ))*dzinv +           &
                                  (Ez(i,j,k  )-Ez(i,j+1,k))*dyinv)

      Hx(i,j,k) = azh(k) * Hx(i,j,k) +                                        &
                  bzh(k) * (cxe(i)*Bx_khigh(i,j,k) - fxe(i)*Bxold) * muinv
      !------
      Byold = By_khigh(i,j,k)

      By_khigh(i,j,k) = azh(k) * By_khigh(i,j,k) +                            &
                        bzh(k) * ((Ez(i+1,j,k)-Ez(i,j,k  ))*dxinv +           &
                                  (Ex(i,j,k  )-Ex(i,j,k+1))*dzinv)

      Hy(i,j,k) = axh(i) * Hy(i,j,k) +                                        &
                  bxh(i) * (cye(j)*By_khigh(i,j,k) - fye(j)*Byold) * muinv
      !------
      Bzold = Bz_khigh(i,j,k)

      Bz_khigh(i,j,k) = axh(i) * Bz_khigh(i,j,k) +                            &
                        bxh(i) * ((Ex(i,j+1,k)-Ex(i,j,k  ))*dyinv +           &
                                  (Ey(i,j,k  )-Ey(i+1,j,k))*dxinv)

      Hz(i,j,k) = ayh(j) * Hz(i,j,k) +                                        &
                  byh(j) * (cze(k)*Bz_khigh(i,j,k) - fze(k)*Bzold) * muinv
    end do
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif
#ifndef SPEC_CPU
!$OMP END PARALLEL 
#endif

END SUBROUTINE UPMLupdateH

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       UPMLupdateE - Update the E-fields in the UPML layers for vacuum
!
! DESCRIPTION
!       Update the E-fields in the UPML layers for vacuum
!
! METHOD
!       Corners and edges are merged with sides so that there are six
!       PML blocks. These six blocks are:
!
!         Block 1: i=xstart+1,0 ; j=ystart+1,ystop ; k=zstart+1,zstop
!         Block 2: i=nx+1,xstop ; j=ystart+1,ystop ; k=zstart+1,zstop
!         Block 3: i=1,nx       ; j=ystart+1,0     ; k=zstart+1,zstop
!         Block 4: i=1,nx       ; j=ny+1,ystop     ; k=zstart+1,zstop
!         Block 5: i=1,nx       ; j=1,ny           ; k=zstart+1,0
!         Block 6: i=1,nx       ; j=1,ny           ; k=nz+1,zstop
!
!       The above list is the start and stop of each index where all
!       three (physical) E-fields shall be updated with a leap-frog scheme.
!       The difference as compared to the corresponding list in 
!       UPMLupdateH is that here all ?start is replace by ?start+1.
!
!       Note that fields on the border between PML and the interior must
!       be updated as PML fields.
!
!       The blocks are updated in order.
!       The coefficients are initialized in UPMLinit.
!
! SYNOPSIS
!       call UPMLupdateE(nx,ny,nz,Hx,Hy,Hz,Ex,Ey,Ez)
!         integer, intent(in) :: nx, ny, nz
!         real(kind=rfp), intent(inout), dimension(xstart:xstop+1,            &
!                         ystart:ystop+1,zstart:zstop+1) :: Ex, Ey, Ez
!         real(kind=rfp), intent(inout), dimension(xstart:xstop,              &
!                         ystart:ystop  ,zstart:zstop  ) :: Hx, Hy, Hz
!
! ERRORS
!       No error handling.
!
! SEE ALSO
!       UPMLupdateH & UPMLinit
!
!       Written by 흆e Rydell
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE UPMLupdateE(nx,ny,nz,Hx,Hy,Hz,Ex,Ey,Ez)
USE globalvar_mod, ONLY : xstart, ystart, zstart, xstop, ystop, zstop,        &
                          dxinv, dyinv, dzinv

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: nx, ny, nz
real(kind=rfp), intent(inout),                                                &
         dimension(xstart:xstop+1,ystart:ystop+1,zstart:zstop+1) :: Ex, Ey, Ez
real(kind=rfp), intent(inout),                                                &
         dimension(xstart:xstop  ,ystart:ystop  ,zstart:zstop  ) :: Hx, Hy, Hz

!------------------------------------------------------------------------------
!                      L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: i, j, k
real(kind=rfp) :: Dxold, Dyold, Dzold

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

!********************** Block 1 (ilow) ************************

#ifndef SPEC_CPU
!$OMP PARALLEL PRIVATE(i,j,k,Dxold,Dyold,Dzold)
#endif
#ifndef SPEC_CPU
!$OMP DO 
#endif
do k=zstart+1,zstop
  do j=ystart+1,ystop
    do i=xstart+1,0

      Dxold = Dx_ilow(i,j,k)

      Dx_ilow(i,j,k) = aye(j) * Dx_ilow(i,j,k) +                              &
                       bye(j) * ((Hz(i,j,k  )-Hz(i,j-1,k))*dyinv +            &
                                 (Hy(i,j,k-1)-Hy(i,j,k  ))*dzinv)

      Ex(i,j,k) = aze(k) * Ex(i,j,k) +                                        &
                  bze(k) * (cxh(i)*Dx_ilow(i,j,k) - fxh(i)*Dxold) * epsinv
      !------
      Dyold = Dy_ilow(i,j,k)

      Dy_ilow(i,j,k) = aze(k) * Dy_ilow(i,j,k) +                              &
                       bze(k) * ((Hx(i,j,k  )-Hx(i,j,k-1))*dzinv +            &
                                 (Hz(i-1,j,k)-Hz(i,j,k  ))*dxinv)

      Ey(i,j,k) = axe(i) * Ey(i,j,k) +                                        &
                  bxe(i) * (cyh(j)*Dy_ilow(i,j,k) - fyh(j)*Dyold) * epsinv
      !------
      Dzold = Dz_ilow(i,j,k)

      Dz_ilow(i,j,k) = axe(i) * Dz_ilow(i,j,k) +                              &
                       bxe(i) * ((Hy(i,j,k  )-Hy(i-1,j,k))*dxinv +            &
                                 (Hx(i,j-1,k)-Hx(i,j,k  ))*dyinv)

      Ez(i,j,k) = aye(j) * Ez(i,j,k) +                                        &
                  bye(j) * (czh(k)*Dz_ilow(i,j,k) - fzh(k)*Dzold) * epsinv
    end do
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

!
! Update the fields on the boundary between Yee and PML
!
i = 1
#ifndef SPEC_CPU
!$OMP DO 
#endif
do k=1,nz
  do j=1,ny

    Dyold = Dy_ilow(i,j,k)

    Dy_ilow(i,j,k) = aze(k) * Dy_ilow(i,j,k) +                                &
                     bze(k) * ((Hx(i,j,k  )-Hx(i,j,k-1))*dzinv +              &
                               (Hz(i-1,j,k)-Hz(i,j,k  ))*dxinv)

    Ey(i,j,k) = axe(i) * Ey(i,j,k) +                                          &
                bxe(i) * (cyh(j)*Dy_ilow(i,j,k) - fyh(j)*Dyold) * epsinv
    !------
    Dzold = Dz_ilow(i,j,k)

    Dz_ilow(i,j,k) = axe(i) * Dz_ilow(i,j,k) +                                &
                     bxe(i) * ((Hy(i,j,k  )-Hy(i-1,j,k))*dxinv +              &
                               (Hx(i,j-1,k)-Hx(i,j,k  ))*dyinv)

    Ez(i,j,k) = aye(j) * Ez(i,j,k) +                                          &
                bye(j) * (czh(k)*Dz_ilow(i,j,k) - fzh(k)*Dzold) * epsinv
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif
!
! Update fields in the first PML cell. Two of three are PEC! 
!
i = xstart
#ifndef SPEC_CPU
!$OMP DO 
#endif
do k=zstart+1,zstop
  do j=ystart+1,ystop

    Dxold = Dx_ilow(i,j,k)

    Dx_ilow(i,j,k) = aye(j) * Dx_ilow(i,j,k) +                                &
                     bye(j) * ((Hz(i,j,k  )-Hz(i,j-1,k))*dyinv +              &
                               (Hy(i,j,k-1)-Hy(i,j,k  ))*dzinv)

    Ex(i,j,k) = aze(k) * Ex(i,j,k) +                                          &
                bze(k) * (cxh(i)*Dx_ilow(i,j,k) - fxh(i)*Dxold) * epsinv

!         Ey(i,j,k) = 0
!         Ez(i,j,k) = 0

  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

j = ystart
#ifndef SPEC_CPU
!$OMP DO 
#endif
do k=zstart+1,zstop
  do i=xstart+1,0

    Dyold = Dy_ilow(i,j,k)

    Dy_ilow(i,j,k) = aze(k) * Dy_ilow(i,j,k) +                                &
                     bze(k) * ((Hx(i,j,k  )-Hx(i,j,k-1))*dzinv +              &
                               (Hz(i-1,j,k)-Hz(i,j,k  ))*dxinv)

    Ey(i,j,k) = axe(i) * Ey(i,j,k) +                                          &
                bxe(i) * (cyh(j)*Dy_ilow(i,j,k) - fyh(j)*Dyold) * epsinv

!         Ex(i,j,k) = 0
!         Ez(i,j,k) = 0

  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

k = zstart
#ifndef SPEC_CPU
!$OMP DO 
#endif
do j=ystart+1,ystop
  do i=xstart+1,0

    Dzold = Dz_ilow(i,j,k)

    Dz_ilow(i,j,k) = axe(i) * Dz_ilow(i,j,k) +                                &
                     bxe(i) * ((Hy(i,j,k  )-Hy(i-1,j,k))*dxinv +              &
                               (Hx(i,j-1,k)-Hx(i,j,k  ))*dyinv)

    Ez(i,j,k) = aye(j) * Ez(i,j,k) +                                          &
                bye(j) * (czh(k)*Dz_ilow(i,j,k) - fzh(k)*Dzold) * epsinv

!         Ex(i,j,k) = 0
!         Ey(i,j,k) = 0

  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

!********************** Block 2 (ihigh) ************************

#ifndef SPEC_CPU
!$OMP DO 
#endif
do k=zstart+1,zstop
  do j=ystart+1,ystop
    do i=nx+1,xstop

      Dxold = Dx_ihigh(i,j,k)

      Dx_ihigh(i,j,k) = aye(j) * Dx_ihigh(i,j,k) +                            &
                        bye(j) * ((Hz(i,j,k  )-Hz(i,j-1,k))*dyinv +           &
                                  (Hy(i,j,k-1)-Hy(i,j,k  ))*dzinv)

      Ex(i,j,k) = aze(k) * Ex(i,j,k) +                                        &
                  bze(k) * (cxh(i)*Dx_ihigh(i,j,k) - fxh(i)*Dxold) * epsinv
      !------
      Dyold = Dy_ihigh(i,j,k)

      Dy_ihigh(i,j,k) = aze(k) * Dy_ihigh(i,j,k) +                            &
                        bze(k) * ((Hx(i,j,k  )-Hx(i,j,k-1))*dzinv +           &
                                  (Hz(i-1,j,k)-Hz(i,j,k  ))*dxinv)

      Ey(i,j,k) = axe(i) * Ey(i,j,k) +                                        &
                  bxe(i) * (cyh(j)*Dy_ihigh(i,j,k) - fyh(j)*Dyold) * epsinv
      !------
      Dzold = Dz_ihigh(i,j,k)

      Dz_ihigh(i,j,k) = axe(i) * Dz_ihigh(i,j,k) +                            &
                        bxe(i) * ((Hy(i,j,k  )-Hy(i-1,j,k))*dxinv +           &
                                  (Hx(i,j-1,k)-Hx(i,j,k  ))*dyinv)

      Ez(i,j,k) = aye(j) * Ez(i,j,k) +                                        &
                  bye(j) * (czh(k)*Dz_ihigh(i,j,k) - fzh(k)*Dzold) * epsinv
    end do
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

!
! Update fields in the first PML cell. Two of three are PEC! 
!
j = ystart
#ifndef SPEC_CPU
!$OMP DO 
#endif
do k=zstart+1,zstop
  do i=nx+1,xstop

    Dyold = Dy_ihigh(i,j,k)

    Dy_ihigh(i,j,k) = aze(k) * Dy_ihigh(i,j,k) +                              &
                      bze(k) * ((Hx(i,j,k  )-Hx(i,j,k-1))*dzinv +             &
                                (Hz(i-1,j,k)-Hz(i,j,k  ))*dxinv)

    Ey(i,j,k) = axe(i) * Ey(i,j,k) +                                          &
                bxe(i) * (cyh(j)*Dy_ihigh(i,j,k) - fyh(j)*Dyold) * epsinv

!         Ex(i,j,k) = 0
!         Ez(i,j,k) = 0

  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

k = zstart
#ifndef SPEC_CPU
!$OMP DO 
#endif
do j=ystart+1,ystop
  do i=nx+1,xstop

    Dzold = Dz_ihigh(i,j,k)

    Dz_ihigh(i,j,k) = axe(i) * Dz_ihigh(i,j,k) +                              &
                      bxe(i) * ((Hy(i,j,k  )-Hy(i-1,j,k))*dxinv +             &
                                (Hx(i,j-1,k)-Hx(i,j,k  ))*dyinv)

    Ez(i,j,k) = aye(j) * Ez(i,j,k) +                                          &
                bye(j) * (czh(k)*Dz_ihigh(i,j,k) - fzh(k)*Dzold) * epsinv

!         Ex(i,j,k) = 0
!         Ey(i,j,k) = 0

  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

!********************** Block 3 (jlow) ************************

#ifndef SPEC_CPU
!$OMP DO 
#endif
do k=zstart+1,zstop
  do j=ystart+1,0
    do i=1,nx

      Dxold = Dx_jlow(i,j,k)

      Dx_jlow(i,j,k) = aye(j) * Dx_jlow(i,j,k) +                              &
                       bye(j) * ((Hz(i,j,k  )-Hz(i,j-1,k))*dyinv +            &
                                 (Hy(i,j,k-1)-Hy(i,j,k  ))*dzinv)

      Ex(i,j,k) = aze(k) * Ex(i,j,k) +                                        &
                  bze(k) * (cxh(i)*Dx_jlow(i,j,k) - fxh(i)*Dxold) * epsinv
      !------
      Dyold = Dy_jlow(i,j,k)

      Dy_jlow(i,j,k) = aze(k) * Dy_jlow(i,j,k) +                              &
                       bze(k) * ((Hx(i,j,k  )-Hx(i,j,k-1))*dzinv +            &
                                 (Hz(i-1,j,k)-Hz(i,j,k  ))*dxinv)

      Ey(i,j,k) = axe(i) * Ey(i,j,k) +                                        &
                  bxe(i) * (cyh(j)*Dy_jlow(i,j,k) - fyh(j)*Dyold) * epsinv
      !------
      Dzold = Dz_jlow(i,j,k)

      Dz_jlow(i,j,k) = axe(i) * Dz_jlow(i,j,k) +                              &
                       bxe(i) * ((Hy(i,j,k  )-Hy(i-1,j,k))*dxinv +            &
                                 (Hx(i,j-1,k)-Hx(i,j,k  ))*dyinv)

      Ez(i,j,k) = aye(j) * Ez(i,j,k) +                                        &
                  bye(j) * (czh(k)*Dz_jlow(i,j,k) - fzh(k)*Dzold) * epsinv
    end do
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

!
! Update the fields on the boundary between Yee and PML.
! (Note that Ez(1,1,:) are excluded from this block since these fields
! were updated in Block 1).
j = 1
#ifndef SPEC_CPU
!$OMP DO 
#endif
do k=1,nz
  do i=1,nx

    Dxold = Dx_jlow(i,j,k)

    Dx_jlow(i,j,k) = aye(j) * Dx_jlow(i,j,k) +                                &
                     bye(j) * ((Hz(i,j,k  )-Hz(i,j-1,k))*dyinv +              &
                               (Hy(i,j,k-1)-Hy(i,j,k  ))*dzinv)

    Ex(i,j,k) = aze(k) * Ex(i,j,k) +                                          &
                bze(k) * (cxh(i)*Dx_jlow(i,j,k) - fxh(i)*Dxold) * epsinv
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

j = 1
#ifndef SPEC_CPU
!$OMP DO 
#endif
do k=1,nz
  do i=2,nx

    Dzold = Dz_jlow(i,j,k)

    Dz_jlow(i,j,k) = axe(i) * Dz_jlow(i,j,k) +                                &
                     bxe(i) * ((Hy(i,j,k  )-Hy(i-1,j,k))*dxinv +              &
                               (Hx(i,j-1,k)-Hx(i,j,k  ))*dyinv)

    Ez(i,j,k) = aye(j) * Ez(i,j,k) +                                          &
                bye(j) * (czh(k)*Dz_jlow(i,j,k) - fzh(k)*Dzold) * epsinv
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif
!
! Update fields in the first PML cell. Two of three are PEC!
!
j = ystart
#ifndef SPEC_CPU
!$OMP DO 
#endif
do k=zstart+1,zstop
  do i=1,nx

    Dyold = Dy_jlow(i,j,k)

    Dy_jlow(i,j,k) = aze(k) * Dy_jlow(i,j,k) +                                &
                     bze(k) * ((Hx(i,j,k  )-Hx(i,j,k-1))*dzinv +              &
                               (Hz(i-1,j,k)-Hz(i,j,k  ))*dxinv)

    Ey(i,j,k) = axe(i) * Ey(i,j,k) +                                          &
                bxe(i) * (cyh(j)*Dy_jlow(i,j,k) - fyh(j)*Dyold) * epsinv

!         Ex(i,j,k) = 0
!         Ez(i,j,k) = 0

  end do
end do   
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

k = zstart
#ifndef SPEC_CPU
!$OMP DO 
#endif
do j=ystart+1,0
  do i=1,nx

    Dzold = Dz_jlow(i,j,k)

    Dz_jlow(i,j,k) = axe(i) * Dz_jlow(i,j,k) +                                &
                     bxe(i) * ((Hy(i,j,k  )-Hy(i-1,j,k))*dxinv +              &
                               (Hx(i,j-1,k)-Hx(i,j,k  ))*dyinv)

    Ez(i,j,k) = aye(j) * Ez(i,j,k) +                                          &
                bye(j) * (czh(k)*Dz_jlow(i,j,k) - fzh(k)*Dzold) * epsinv

!         Ex(i,j,k) = 0
!         Ey(i,j,k) = 0

  end do
end do   
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

!********************** Block 4 (jhigh) ************************

#ifndef SPEC_CPU
!$OMP DO 
#endif
do k=zstart+1,zstop
  do j=ny+1,ystop
    do i=1,nx

      Dxold = Dx_jhigh(i,j,k)

      Dx_jhigh(i,j,k) = aye(j) * Dx_jhigh(i,j,k) +                            &
                        bye(j) * ((Hz(i,j,k  )-Hz(i,j-1,k))*dyinv +           &
                                  (Hy(i,j,k-1)-Hy(i,j,k  ))*dzinv)

      Ex(i,j,k) = aze(k) * Ex(i,j,k) +                                        &
                  bze(k) * (cxh(i)*Dx_jhigh(i,j,k) - fxh(i)*Dxold) * epsinv
      !------
      Dyold = Dy_jhigh(i,j,k)

      Dy_jhigh(i,j,k) = aze(k) * Dy_jhigh(i,j,k) +                            &
                        bze(k) * ((Hx(i,j,k  )-Hx(i,j,k-1))*dzinv +           &
                                  (Hz(i-1,j,k)-Hz(i,j,k  ))*dxinv)

      Ey(i,j,k) = axe(i) * Ey(i,j,k) +                                        &
                  bxe(i) * (cyh(j)*Dy_jhigh(i,j,k) - fyh(j)*Dyold) * epsinv
      !------
      Dzold = Dz_jhigh(i,j,k)

      Dz_jhigh(i,j,k) = axe(i) * Dz_jhigh(i,j,k) +                            &
                        bxe(i) * ((Hy(i,j,k  )-Hy(i-1,j,k))*dxinv +           &
                                  (Hx(i,j-1,k)-Hx(i,j,k  ))*dyinv)

      Ez(i,j,k) = aye(j) * Ez(i,j,k) +                                        &
                  bye(j) * (czh(k)*Dz_jhigh(i,j,k) - fzh(k)*Dzold) * epsinv
    end do
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif
!
! Update fields in the first PML cell. Two of three are PEC! 
!

k = zstart
#ifndef SPEC_CPU
!$OMP DO 
#endif
do j=ny+1,ystop
  do i=1,nx

    Dzold = Dz_jhigh(i,j,k)

    Dz_jhigh(i,j,k) = axe(i) * Dz_jhigh(i,j,k) +                              &
                      bxe(i) * ((Hy(i,j,k  )-Hy(i-1,j,k))*dxinv +             &
                                (Hx(i,j-1,k)-Hx(i,j,k  ))*dyinv)

    Ez(i,j,k) = aye(j) * Ez(i,j,k) +                                          &
                bye(j) * (czh(k)*Dz_jhigh(i,j,k) - fzh(k)*Dzold) * epsinv

!         Ex(i,j,k) = 0
!         Ey(i,j,k) = 0

  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

!********************* Block 5 (klow) ************************

#ifndef SPEC_CPU
!$OMP DO 
#endif
do k=zstart+1,0
  do j=1,ny
    do i=1,nx

      Dxold = Dx_klow(i,j,k)

      Dx_klow(i,j,k) = aye(j) * Dx_klow(i,j,k) +                              &
                       bye(j) * ((Hz(i,j,k  )-Hz(i,j-1,k))*dyinv +            &
                                 (Hy(i,j,k-1)-Hy(i,j,k  ))*dzinv)

      Ex(i,j,k) = aze(k) * Ex(i,j,k) +                                        &
                  bze(k) * (cxh(i)*Dx_klow(i,j,k) - fxh(i)*Dxold) * epsinv
      !------
      Dyold = Dy_klow(i,j,k)

      Dy_klow(i,j,k) = aze(k) * Dy_klow(i,j,k) +                              &
                       bze(k) * ((Hx(i,j,k  )-Hx(i,j,k-1))*dzinv +            &
                                 (Hz(i-1,j,k)-Hz(i,j,k  ))*dxinv)

      Ey(i,j,k) = axe(i) * Ey(i,j,k) +                                        &
                  bxe(i) * (cyh(j)*Dy_klow(i,j,k) - fyh(j)*Dyold) * epsinv
      !------
      Dzold = Dz_klow(i,j,k)

      Dz_klow(i,j,k) = axe(i) * Dz_klow(i,j,k) +                              &
                       bxe(i) * ((Hy(i,j,k  )-Hy(i-1,j,k))*dxinv +            &
                                 (Hx(i,j-1,k)-Hx(i,j,k  ))*dyinv)

      Ez(i,j,k) = aye(j) * Ez(i,j,k) +                                        &
                  bye(j) * (czh(k)*Dz_klow(i,j,k) - fzh(k)*Dzold) * epsinv
    end do
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

!
! Update the fields on the boundary between Yee and PML.
! (Note that Ex(:,1,1) and Ey(1,:,1) have already been updated in
! previous blocks and are not included here.)
k = 1
#ifndef SPEC_CPU
!$OMP DO 
#endif
do j=2,ny
  do i=1,nx

    Dxold = Dx_klow(i,j,k)

    Dx_klow(i,j,k) = aye(j) * Dx_klow(i,j,k) +                                &
                     bye(j) * ((Hz(i,j,k  )-Hz(i,j-1,k))*dyinv +              &
                               (Hy(i,j,k-1)-Hy(i,j,k  ))*dzinv)

    Ex(i,j,k) = aze(k) * Ex(i,j,k) +                                          &
                bze(k) * (cxh(i)*Dx_klow(i,j,k) - fxh(i)*Dxold) * epsinv
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

k = 1
#ifndef SPEC_CPU
!$OMP DO 
#endif
do j=1,ny
  do i=2,nx

    Dyold = Dy_klow(i,j,k)

    Dy_klow(i,j,k) = aze(k) * Dy_klow(i,j,k) +                                &
                     bze(k) * ((Hx(i,j,k  )-Hx(i,j,k-1))*dzinv +              &
                               (Hz(i-1,j,k)-Hz(i,j,k  ))*dxinv)

    Ey(i,j,k) = axe(i) * Ey(i,j,k) +                                          &
                bxe(i) * (cyh(j)*Dy_klow(i,j,k) - fyh(j)*Dyold) * epsinv
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif
!
! Update fields in the first PML cell. Two of three are PEC! 
!
k = zstart
#ifndef SPEC_CPU
!$OMP DO 
#endif
do j=1,ny
  do i=1,nx
    
    Dzold = Dz_klow(i,j,k)

    Dz_klow(i,j,k) = axe(i) * Dz_klow(i,j,k) +                                &
                     bxe(i) * ((Hy(i,j,k  )-Hy(i-1,j,k))*dxinv +              &
                               (Hx(i,j-1,k)-Hx(i,j,k  ))*dyinv)

    Ez(i,j,k) = aye(j) * Ez(i,j,k) +                                          &
                bye(j) * (czh(k)*Dz_klow(i,j,k) - fzh(k)*Dzold) * epsinv

!         Ex(i,j,k) = 0
!         Ey(i,j,k) = 0

  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif

!********************** Block 6 (khigh) ************************

#ifndef SPEC_CPU
!$OMP DO 
#endif
do k=nz+1,zstop
  do j=1,ny
    do i=1,nx     

      Dxold = Dx_khigh(i,j,k)

      Dx_khigh(i,j,k) = aye(j) * Dx_khigh(i,j,k) +                            &
                        bye(j) * ((Hz(i,j,k  )-Hz(i,j-1,k))*dyinv +           &
                                  (Hy(i,j,k-1)-Hy(i,j,k  ))*dzinv)

      Ex(i,j,k) = aze(k) * Ex(i,j,k) +                                        &
                  bze(k) * (cxh(i)*Dx_khigh(i,j,k) - fxh(i)*Dxold) * epsinv
      !------
      Dyold = Dy_khigh(i,j,k)

      Dy_khigh(i,j,k) = aze(k) * Dy_khigh(i,j,k) +                            &
                        bze(k) * ((Hx(i,j,k  )-Hx(i,j,k-1))*dzinv +           &
                                  (Hz(i-1,j,k)-Hz(i,j,k  ))*dxinv)

      Ey(i,j,k) = axe(i) * Ey(i,j,k) +                                        &
                  bxe(i) * (cyh(j)*Dy_khigh(i,j,k) - fyh(j)*Dyold) * epsinv
      !------
      Dzold = Dz_khigh(i,j,k)

      Dz_khigh(i,j,k) = axe(i) * Dz_khigh(i,j,k) +                            &
                        bxe(i) * ((Hy(i,j,k  )-Hy(i-1,j,k))*dxinv +           &
                                  (Hx(i,j-1,k)-Hx(i,j,k  ))*dyinv)

      Ez(i,j,k) = aye(j) * Ez(i,j,k) +                                        &
                  bye(j) * (czh(k)*Dz_khigh(i,j,k) - fzh(k)*Dzold) * epsinv
    end do
  end do
end do
#ifndef SPEC_CPU
!$OMP END DO NOWAIT
#endif
#ifndef SPEC_CPU
!$OMP END PARALLEL 
#endif

! PEC in the stop+1 cells
!
! As long as the fields are initialized to zero they will stay zero in the
! stop+1 cells. The PEC for the lower end is spread out on the six 
! different PML blocks.
! 

END SUBROUTINE UPMLupdateE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       UPML_get_pml_cells - Access function for PRIVATE variable pml_cells
!
! DESCRIPTION
!       A one line access function for PRIVATE variable pml_cells.
!
! METHOD
!       Sets UPML_get_pml_cells = pml_cells
!
! SYNOPSIS
!       FUNCTION UPML_get_pml_cells()
!
! RETURN VALUES
!       integer :: UPML_get_pml_cells
!
! ERRORS
!       No error handling
!
! SEE ALSO
!       UPML_setparvar
!
! HISTORY
!       Written by Ulf Andersson 99-12-01
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION UPML_get_pml_cells()

!------------------------------------------------------------------------------
!                     R e t u r n  t y p e
!------------------------------------------------------------------------------

integer :: UPML_get_pml_cells

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

UPML_get_pml_cells = pml_cells

END FUNCTION UPML_get_pml_cells


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       UPMLend - Deallocates memory
!
! DESCRIPTION
!       Deallocates memory used by UPML
!
! SYNOPSIS
!       call UPMLend
!
! ERRORS
!       Uses errorcheck module
!
! SEE ALSO
!       UPMLallocate, UPMLinit, UPMLupdateH & UPMLupdateE
!
! HISTORY
!       Written by 흆e Rydell
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE UPMLend
USE errorcheck_mod, ONLY : check_deallocate

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: allocstat

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

deallocate(Bx_ilow, STAT=allocstat)
call check_deallocate(allocstat,'Bx_ilow',1)
deallocate(By_ilow, STAT=allocstat)
call check_deallocate(allocstat,'By_ilow',1)
deallocate(Bz_ilow, STAT=allocstat)
call check_deallocate(allocstat,'Bz_ilow',1)

deallocate(Bx_ihigh, STAT=allocstat)
call check_deallocate(allocstat,'Bx_ihigh',1)
deallocate(By_ihigh, STAT=allocstat)
call check_deallocate(allocstat,'By_ihigh',1)
deallocate(Bz_ihigh, STAT=allocstat)
call check_deallocate(allocstat,'Bz_ihigh',1)

deallocate(Bx_jlow, STAT=allocstat)
call check_deallocate(allocstat,'Bx_jlow',1)
deallocate(By_jlow, STAT=allocstat)
call check_deallocate(allocstat,'By_jlow',1)
deallocate(Bz_jlow, STAT=allocstat)
call check_deallocate(allocstat,'Bz_jlow',1)

deallocate(Bx_jhigh, STAT=allocstat)
call check_deallocate(allocstat,'Bx_jhigh',1)
deallocate(By_jhigh, STAT=allocstat)
call check_deallocate(allocstat,'By_jhigh',1)
deallocate(Bz_jhigh, STAT=allocstat)
call check_deallocate(allocstat,'Bz_jhigh',1)

deallocate(Bx_klow, STAT=allocstat)
call check_deallocate(allocstat,'Bx_klow',1)
deallocate(By_klow, STAT=allocstat)
call check_deallocate(allocstat,'By_klow',1)
deallocate(Bz_klow, STAT=allocstat)
call check_deallocate(allocstat,'Bz_klow',1)

deallocate(Bx_khigh, STAT=allocstat)
call check_deallocate(allocstat,'Bx_khigh',1) 
deallocate(By_khigh, STAT=allocstat)
call check_deallocate(allocstat,'By_khigh',1)
deallocate(Bz_khigh, STAT=allocstat)
call check_deallocate(allocstat,'Bz_khigh',1)

deallocate(Dx_ilow, STAT=allocstat)
call check_deallocate(allocstat,'Dx_ilow',1)
deallocate(Dy_ilow, STAT=allocstat)
call check_deallocate(allocstat,'Dy_ilow',1)
deallocate(Dz_ilow, STAT=allocstat)
call check_deallocate(allocstat,'Dz_ilow',1)

deallocate(Dx_ihigh, STAT=allocstat)
call check_deallocate(allocstat,'Dx_ihigh',1) 
deallocate(Dy_ihigh, STAT=allocstat)
call check_deallocate(allocstat,'Dy_ihigh',1)
deallocate(Dz_ihigh, STAT=allocstat)
call check_deallocate(allocstat,'Dz_ihigh',1)

deallocate(Dx_jlow, STAT=allocstat)
call check_deallocate(allocstat,'Dx_jlow',1)
deallocate(Dy_jlow, STAT=allocstat)
call check_deallocate(allocstat,'Dy_jlow',1)
deallocate(Dz_jlow, STAT=allocstat)
call check_deallocate(allocstat,'Dz_jlow',1)

deallocate(Dx_jhigh, STAT=allocstat)
call check_deallocate(allocstat,'Dx_jhigh',1)
deallocate(Dy_jhigh, STAT=allocstat)
call check_deallocate(allocstat,'Dy_jhigh',1)
deallocate(Dz_jhigh, STAT=allocstat)
call check_deallocate(allocstat,'Dz_jhigh',1)

deallocate(Dx_klow, STAT=allocstat)
call check_deallocate(allocstat,'Dx_klow',1)
deallocate(Dy_klow, STAT=allocstat)
call check_deallocate(allocstat,'Dy_klow',1)
deallocate(Dz_klow, STAT=allocstat)
call check_deallocate(allocstat,'Dz_klow',1)

deallocate(Dx_khigh, STAT=allocstat)
call check_deallocate(allocstat,'Dx_khigh',1)
deallocate(Dy_khigh, STAT=allocstat)
call check_deallocate(allocstat,'Dy_khigh',1)
deallocate(Dz_khigh, STAT=allocstat)
call check_deallocate(allocstat,'Dz_khigh',1)
!--------------------
deallocate( sigmx, STAT=allocstat )
call check_deallocate(allocstat,'sigmx',1)
deallocate( sigmy, STAT=allocstat )
call check_deallocate(allocstat,'sigmy',1)
deallocate( sigmz, STAT=allocstat )
call check_deallocate(allocstat,'sigmz',1)

deallocate( sigmx_star, STAT=allocstat )
call check_deallocate(allocstat,'sigmx_star',1)
deallocate( sigmy_star, STAT=allocstat )
call check_deallocate(allocstat,'sigmy_star',1)
deallocate( sigmz_star, STAT=allocstat )
call check_deallocate(allocstat,'sigmz_star',1)

deallocate(kappax, STAT=allocstat)
call check_deallocate(allocstat,'kappax',1)
deallocate(kappay, STAT=allocstat)
call check_deallocate(allocstat,'kappay',1)
deallocate(kappaz, STAT=allocstat)
call check_deallocate(allocstat,'kappaz',1)

deallocate(kappax_star, STAT=allocstat)
call check_deallocate(allocstat,'kappax_star',1)
deallocate(kappay_star, STAT=allocstat)
call check_deallocate(allocstat,'kappay_star',1)
deallocate(kappaz_star, STAT=allocstat)
call check_deallocate(allocstat,'kappaz_star',1)
!--------------------
deallocate(axe, STAT=allocstat)
call check_deallocate(allocstat,'axe',1)
deallocate(aye, STAT=allocstat)
call check_deallocate(allocstat,'aye',1)
deallocate(aze, STAT=allocstat)
call check_deallocate(allocstat,'aze',1)

deallocate(bxe, STAT=allocstat)
call check_deallocate(allocstat,'bxe',1)
deallocate(bye, STAT=allocstat)
call check_deallocate(allocstat,'bye',1)
deallocate(bze, STAT=allocstat)
call check_deallocate(allocstat,'bze',1)

deallocate(cxe, STAT=allocstat)
call check_deallocate(allocstat,'cxe',1)
deallocate(cye, STAT=allocstat)
call check_deallocate(allocstat,'cye',1)
deallocate(cze, STAT=allocstat)
call check_deallocate(allocstat,'cze',1)

deallocate(fxe, STAT=allocstat)
call check_deallocate(allocstat,'fxe',1)
deallocate(fye, STAT=allocstat)
call check_deallocate(allocstat,'fye',1)
deallocate(fze, STAT=allocstat)
call check_deallocate(allocstat,'fze',1)

deallocate(axh, STAT=allocstat)
call check_deallocate(allocstat,'axh',1)
deallocate(ayh, STAT=allocstat)
call check_deallocate(allocstat,'ayh',1)
deallocate(azh, STAT=allocstat)
call check_deallocate(allocstat,'azh',1)

deallocate(bxh, STAT=allocstat)
call check_deallocate(allocstat,'bxh',1)
deallocate(byh, STAT=allocstat)
call check_deallocate(allocstat,'byh',1)
deallocate(bzh, STAT=allocstat)
call check_deallocate(allocstat,'bzh',1)

deallocate(cxh, STAT=allocstat)
call check_deallocate(allocstat,'cxh',1)
deallocate(cyh, STAT=allocstat)
call check_deallocate(allocstat,'cyh',1)
deallocate(czh, STAT=allocstat)
call check_deallocate(allocstat,'czh',1)

deallocate(fxh, STAT=allocstat)
call check_deallocate(allocstat,'fxh',1)
deallocate(fyh, STAT=allocstat)
call check_deallocate(allocstat,'fyh',1)
deallocate(fzh, STAT=allocstat)
call check_deallocate(allocstat,'fzh',1)

END SUBROUTINE UPMLend

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE UPML_mod
