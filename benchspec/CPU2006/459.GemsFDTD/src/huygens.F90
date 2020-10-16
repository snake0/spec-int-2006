!+
! NAME
!       Huygens_mod - Plane wave excitation modules
!
! DESCRIPTION
!       Module for generation of incident fields.
!
! METHOD
!       This Huygens[EH] routines are intended to be useful also in a parallel
!       code. This is the main reason for using precalculated index values
!       (DB, dblxp1 etc.)
!
! PUBLIC
!       SUBROUTINE HuygensH
!       SUBROUTINE HuygensE
!       SUBROUTINE Huygens_init
!       SUBROUTINE Huy_setparvar
!       SUBROUTINE Huygens_end
!       INTEGER :: Huy_db, HuyPulsetype
!       real(kind=rfp), dimension(excite_max_no_param) :: Huy_param 
!       real(kind=rfp), dimension(3,2) :: Huy_wavedir 
!       real(kind=rfp) :: Huy_phi 
!
! HISTORY
!       Version       Date                 Name
!       Comments
!       -------------------------------------------
!	$Log: huygens.f90,v $
!	Revision 1.2  2003/09/26 20:15:18  ulfa
!	Added global computation of #bytes allocated.
!	
!	Revision 1.1  2003/09/23 14:06:51  ulfa
!	Initial version.
!	
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE Huygens_mod
USE parameter_mod, ONLY : rfp, two
USE globalvar_mod, ONLY : dx, dy, dz, xstart, xstop, ystart, ystop, zstart,   &
                          zstop, dt, dxinv, dyinv, dzinv, dtdeps, dtdmu,      &
                          pec_gp_zindex
USE excite_mod, ONLY : excite_max_no_param

IMPLICIT NONE

PUBLIC HuygensH, HuygensE, Huygens_init, Huy_setparvar, Huygens_end

integer, PUBLIC        :: Huy_db = 3 ! used by calcflops
integer, PUBLIC        :: HuyPulsetype=0 ! used by leapfrog
real(kind=rfp), dimension(excite_max_no_param), PUBLIC :: Huy_param 
                                               ! pulse parameters used by NFCW
real(kind=rfp), dimension(3,2), PUBLIC :: Huy_wavedir !wave-direction. Used by
                  ! near-to-far-field transforms if keyword monostatic is given
real(kind=rfp), PUBLIC :: Huy_phi   ! May be needed by NF transforms

PRIVATE

integer :: dblx, dblxp1, dbly, dblyp1, dblz, dblzp1
integer :: dbhx, dbhxp1, dbhy, dbhyp1, dbhz, dbhzp1
integer :: ih, no_Huy_applies = 1
real(kind=rfp), dimension(3,2) :: X0      ! wave-offset
real(kind=rfp), dimension(3,2) :: k_div_c0
real(kind=rfp), dimension(2)   :: coeff_X_Hy,coeff_X_Hz, coeff_X_Ey,coeff_X_Ez
real(kind=rfp), dimension(2)   :: coeff_Y_Hx,coeff_Y_Hz, coeff_Y_Ex,coeff_Y_Ez
real(kind=rfp), dimension(2)   :: coeff_Z_Hx,coeff_Z_Hy, coeff_Z_Ex,coeff_Z_Ey

real(kind=rfp), dimension(:,:,:), allocatable :: xsi_lowX_Hy,  xsi_lowX_Ey 
real(kind=rfp), dimension(:,:,:), allocatable :: xsi_lowX_Hz,  xsi_lowX_Ez 
real(kind=rfp), dimension(:,:,:), allocatable :: xsi_highX_Hy, xsi_highX_Ey
real(kind=rfp), dimension(:,:,:), allocatable :: xsi_highX_Hz, xsi_highX_Ez
                                                                         
real(kind=rfp), dimension(:,:,:), allocatable :: xsi_lowY_Hx,  xsi_lowY_Ex 
real(kind=rfp), dimension(:,:,:), allocatable :: xsi_lowY_Hz,  xsi_lowY_Ez 
real(kind=rfp), dimension(:,:,:), allocatable :: xsi_highY_Hx, xsi_highY_Ex
real(kind=rfp), dimension(:,:,:), allocatable :: xsi_highY_Hz, xsi_highY_Ez
                                                                         
real(kind=rfp), dimension(:,:,:), allocatable :: xsi_lowZ_Hx,  xsi_lowZ_Ex 
real(kind=rfp), dimension(:,:,:), allocatable :: xsi_lowZ_Hy,  xsi_lowZ_Ey 
real(kind=rfp), dimension(:,:,:), allocatable :: xsi_highZ_Hx, xsi_highZ_Ex
real(kind=rfp), dimension(:,:,:), allocatable :: xsi_highZ_Hy, xsi_highZ_Ey

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Huy_setparvar - Sets parsed variables
!
! DESCRIPTION
!       Set the private declared variables parsed in module readdata
!
! SYNOPSIS
!       CALL Huy_setparvar(par_wavedir,par_X0,par_param,par_phi)
!         real(kind=rfp), dimension(3), intent(in) :: par_wavedir 
!         real(kind=rfp), dimension(3), intent(in) :: par_X0
!         real(kind=rfp), dimension(excite_max_no_param), intent(in)::par_param
!         real(kind=rfp),               intent(in) :: par_phi
!
! ERRORS
!       No error handling 
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Huy_setparvar(par_wavedir,par_X0,par_param,par_phi)
USE globalvar_mod, ONLY : c0

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

real(kind=rfp), dimension(3), intent(in)                   :: par_wavedir 
real(kind=rfp), dimension(3), intent(in)                   :: par_X0      
real(kind=rfp), dimension(excite_max_no_param), intent(in) :: par_param
real(kind=rfp),               intent(in)                   :: par_phi

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

Huy_wavedir(:,1) = par_wavedir
k_div_c0(:,1) = Huy_wavedir(:,1)/c0
X0(:,1) = par_X0       
Huy_param = par_param
Huy_phi = par_phi

END SUBROUTINE Huy_setparvar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Huygens_init - Huygens initialization
!
! DESCRIPTION
!       Huygens initialization
!
! SYNOPSIS
!       CALL Huygens_init(nx,ny,nz,Epol)
!         integer, intent(in) :: nx, ny, nz
!         real(kind=rfp), dimension(3), intent(in) :: Epol 
!
!       (called from readdata/parser)
!
! HISTORY
!       Written by Anders Ålund
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Huygens_init(nx,ny,nz,Epol)
USE parameter_mod, ONLY  : bytes_per_float, eps0, mu0, half
USE globalvar_mod, ONLY  : c0, Z0, bytes_allocated
USE excite_mod, ONLY     : excitation
USE errorcheck_mod, ONLY : check_allocate

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: nx, ny, nz
real(kind=rfp), dimension(3), intent(in) :: Epol ! electric polarization

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

real(kind=rfp), dimension(3) :: Hpol ! magnetic polarization
real(kind=rfp), dimension(3) :: X0_def
real(kind=rfp), dimension(3,2) :: dtdmuEpol, dtdepsHpol
real(kind=rfp) :: excite0

real(kind=rfp), dimension(nx) :: Xvec
real(kind=rfp), dimension(ny) :: Yvec
real(kind=rfp), dimension(nz) :: Zvec
real(kind=rfp), dimension(ny,nz) :: Xx, Yx, Zx
real(kind=rfp), dimension(nx,nz) :: Xy, Yy, Zy
real(kind=rfp), dimension(nx,ny) :: Xz, Yz, Zz
integer :: allocstat, ii, ih, bytes

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

if (Huy_db >= min(0.5_rfp*nx, 0.5_rfp*ny, 0.5_rfp*nz)) then 
  write(*,*) 'Warning! Distance to boundaries must be <'
  write(*,*) 'min(nx/2, ny/2, nz/2)'
  write(*,*) 'Execution halted.'
  stop
end if

! Set values for X0_def
if (Huy_wavedir(1,1) >= 0) then
  X0_def(1) = (Huy_db-1)*dx
else
  X0_def(1) = (nx-Huy_db+1)*dx
end if
if (Huy_wavedir(2,1) >= 0) then
  X0_def(2) = (Huy_db-1)*dy
else
  X0_def(2) = (ny-Huy_db+1)*dy
end if
if (Huy_wavedir(3,1) >= 0) then
  X0_def(3) = (Huy_db-1)*dz
else
  X0_def(3) = (nz-Huy_db+1)*dz
end if

! Check if X0 is read. If not set defaults.
if (X0(1,1)==-1111.1_rfp.AND.X0(2,1)==-1111.1_rfp.AND.X0(3,1)==-1111.1_rfp)then
  X0(:,1) = X0_def
  write(*,*) 'No X0 value given. Default values for X0 will be used:'
  write(*,'(A, 3D14.7)') 'X0 = ', X0_def
end if

! Check if X0 has a permitted value
if (dot_product(Huy_wavedir(:,1),(X0(:,1)-X0_def)) > 0 ) then
  write(*,*) 'Warning! User given XO has illegal value.'
  write(*,*) 'Default values for X0 will be used:'
  write(*,'(A, 3D14.7)') 'X0 = ', X0_def
  X0(:,1) = X0_def
end if

! Check if pl is read. If not set defaults.
if (Huy_param(1)==-1111.1_rfp) then
  Huy_param(1) = 20.0_rfp*min(dx, dy, dz) 
end if

!
! calculate magnetic polarization as cross product of Huy_wavedir and Epol.
!
Hpol(1) = Huy_wavedir(2,1)*Epol(3)-Huy_wavedir(3,1)*Epol(2)
Hpol(2) = Huy_wavedir(3,1)*Epol(1)-Huy_wavedir(1,1)*Epol(3)
Hpol(3) = Huy_wavedir(1,1)*Epol(2)-Huy_wavedir(2,1)*Epol(1)
Z0 = sqrt(mu0/eps0)
Hpol = Hpol/Z0
!
! Initialize dtdmuEpol and dtdepsHpol
!
dtdmuEpol(:,1)  = dtdmu*Epol
dtdepsHpol(:,1) = dtdeps*Hpol
!
! Initialize array limits 
!
dblx   = Huy_db
dblxp1 = dblx+1
dblz   = Huy_db
dblzp1 = dblz+1
dbhx   = nx-Huy_db+1
dbhxp1 = dbhx+1
dbhz   = nz-Huy_db+1
dbhzp1 = dbhz+1
dbly   = Huy_db
dbhy   = ny-Huy_db+1
dblyp1 = dbly+1
dbhyp1 = dbhy+1
!
! PEC groundplane
!
if (pec_gp_zindex/=0) then
  !! Mirror X0 in the PEC plane
  X0(1,2) = X0(1,1)
  X0(2,2) = X0(2,1)
  X0(3,2) = two*(pec_gp_zindex-1)*dz - X0(3,1)
  !! Mirror k in the PEC plane
  Huy_wavedir(1,2) = Huy_wavedir(1,1)
  Huy_wavedir(2,2) = Huy_wavedir(2,1)
  Huy_wavedir(3,2) = -Huy_wavedir(3,1)
  k_div_c0(:,2) = Huy_wavedir(:,2)/c0
  !! Mirror E in the PEC plane
  dtdmuEpol(1,2) = -dtdmuEpol(1,1)
  dtdmuEpol(2,2) = -dtdmuEpol(2,1)
  dtdmuEpol(3,2) =  dtdmuEpol(3,1)
  !! Calculate H as cross product of Huy_wavedir and Epol.
  Hpol(1) =  Huy_wavedir(2,2)*Epol(3)+Huy_wavedir(3,2)*Epol(2)
  Hpol(2) = -Huy_wavedir(3,2)*Epol(1)-Huy_wavedir(1,2)*Epol(3)
  Hpol(3) = -Huy_wavedir(1,2)*Epol(2)+Huy_wavedir(2,2)*Epol(1)
  Hpol = Hpol/Z0
  dtdepsHpol(:,2) = dtdeps*Hpol
  !! Make sure that we loop twice over the application of Huygens surfaces.
  no_Huy_applies = 2
  !! Avoid part under PEC surface
  dblz   = max(dblz,pec_gp_zindex)
  dblzp1 = max(dblzp1,pec_gp_zindex)
end if
!
! Warn the user if there is a discontinuity at t=0! We assume that all pulses
! have a peak of about 1. Pulses 2 and 3 are designed to be discontinuous.
! Hence there is no need to warn for them.
!
excite0 = excitation( 0.0_rfp, Huy_param, HuyPulseType )
if ( excite0 > 10.0_rfp**(-precision(1.0_rfp)).and.                           &
     HuyPulseType/=2.and.HuyPulseType/=3 ) then
  write(*,*) '==============================================================='
  write(*,*) 'WARNING! Your choice of pulse has given a discontinuity at t=0!'
  write(*,*) 'f(t)==0 for t<0, and f(0) = ', excite0
  write(*,*) '==============================================================='
end if
!
! Compute coefficients
!
do ih=1,no_Huy_applies  ! no_Huy_applies is equal to 1 or 2
  coeff_X_Hy(ih) = dxinv*dtdmuEpol(3,ih)
  coeff_X_Hz(ih) = dxinv*dtdmuEpol(2,ih)
  coeff_Y_Hx(ih) = dyinv*dtdmuEpol(3,ih)
  coeff_Y_Hz(ih) = dyinv*dtdmuEpol(1,ih)
  coeff_Z_Hx(ih) = dzinv*dtdmuEpol(2,ih)
  coeff_Z_Hy(ih) = dzinv*dtdmuEpol(1,ih)

  coeff_X_Ey(ih) = dxinv*dtdepsHpol(3,ih)
  coeff_X_Ez(ih) = dxinv*dtdepsHpol(2,ih)
  coeff_Y_Ex(ih) = dyinv*dtdepsHpol(3,ih)
  coeff_Y_Ez(ih) = dyinv*dtdepsHpol(1,ih)
  coeff_Z_Ex(ih) = dzinv*dtdepsHpol(2,ih)
  coeff_Z_Ey(ih) = dzinv*dtdepsHpol(1,ih)
end do
!
! Allocate retarded time arrays
!
bytes = 0
! -------- Allocate arrays used in SUBROUTINE HuygensH

! Warning! Do not assume that dbhzp1==dbhz+1. PEC plane changes this!
! In lieu of future changes, it seems safest to not assume dbhxp1==dbhx+1 or
! dbhyp1==dbyz+1, even though it is true today (2003-07-03), ulfa

! low X
allocate(xsi_lowX_Hy(dbhy-dblyp1+1,dbhz-dblz+1,no_Huy_applies), STAT=allocstat)
call check_allocate(allocstat,'xsi_lowX_Hy',1)
bytes = bytes + no_Huy_applies*(dbhy-dblyp1+1)*(dbhz-dblz+1)

allocate(xsi_lowX_Hz(dbhy-dbly+1,dbhz-dblzp1+1,no_Huy_applies), STAT=allocstat)
call check_allocate(allocstat,'xsi_lowX_Hz',1)
bytes = bytes + no_Huy_applies*(dbhy-dbly+1)*(dbhz-dblzp1+1)

! high X
allocate(xsi_highX_Hy(dbhy-dblyp1+1,dbhz-dblz+1,no_Huy_applies),STAT=allocstat)
call check_allocate(allocstat,'xsi_highX_Hy',1)
bytes = bytes + no_Huy_applies*(dbhy-dblyp1+1)*(dbhz-dblz+1)

allocate(xsi_highX_Hz(dbhy-dbly+1,dbhz-dblzp1+1,no_Huy_applies),STAT=allocstat)
call check_allocate(allocstat,'xsi_highX_Hz',1)
bytes = bytes + no_Huy_applies*(dbhy-dbly+1)*(dbhz-dblzp1+1)

! low Y
allocate(xsi_lowY_Hx(dbhx-dblxp1+1,dbhz-dblz+1,no_Huy_applies), STAT=allocstat)
call check_allocate(allocstat,'xsi_lowY_Hx',1)
bytes = bytes + no_Huy_applies*(dbhx-dblxp1+1)*(dbhz-dblz+1)

allocate(xsi_lowY_Hz(dbhx-dblx+1,dbhz-dblzp1+1,no_Huy_applies), STAT=allocstat)
call check_allocate(allocstat,'xsi_lowY_Hz',1)
bytes = bytes + no_Huy_applies*(dbhx-dblx+1)*(dbhz-dblzp1+1)

! high Y
allocate(xsi_highY_Hx(dbhx-dblxp1+1,dbhz-dblz+1,no_Huy_applies),STAT=allocstat)
call check_allocate(allocstat,'xsi_highY_Hx',1)
bytes = bytes + no_Huy_applies*(dbhx-dblxp1+1)*(dbhz-dblz+1)

allocate(xsi_highY_Hz(dbhx-dblx+1,dbhz-dblzp1+1,no_Huy_applies),STAT=allocstat)
call check_allocate(allocstat,'xsi_highY_Hz',1)
bytes = bytes + no_Huy_applies*(dbhx-dblx+1)*(dbhz-dblzp1+1)

! low Z
allocate(xsi_lowZ_Hx(dbhx-dblxp1+1,dbhy-dbly+1,no_Huy_applies), STAT=allocstat)
call check_allocate(allocstat,'xsi_lowZ_Hx',1)
bytes = bytes + no_Huy_applies*(dbhx-dblxp1+1)*(dbhy-dbly+1)

allocate(xsi_lowZ_Hy(dbhx-dblx+1,dbhy-dblyp1+1,no_Huy_applies), STAT=allocstat)
call check_allocate(allocstat,'xsi_lowZ_Hy',1)
bytes = bytes + no_Huy_applies*(dbhx-dblx+1)*(dbhy-dblyp1+1)

! high Z
allocate(xsi_highZ_Hx(dbhx-dblxp1+1,dbhy-dbly+1,no_Huy_applies),STAT=allocstat)
call check_allocate(allocstat,'xsi_highZ_Hx',1)
bytes = bytes + no_Huy_applies*(dbhx-dblxp1+1)*(dbhy-dbly+1)

allocate(xsi_highZ_Hy(dbhx-dblx+1,dbhy-dblyp1+1,no_Huy_applies),STAT=allocstat)
call check_allocate(allocstat,'xsi_highZ_Hy',1)
bytes = bytes + no_Huy_applies*(dbhx-dblx+1)*(dbhy-dblyp1+1)

! -------- Allocate arrays used in SUBROUTINE HuygensE

! low X
allocate(xsi_lowX_Ey(dbhy-dbly+1,dbhz-dblzp1+1,no_Huy_applies), STAT=allocstat)
call check_allocate(allocstat,'xsi_lowX_Ey',1)
bytes = bytes + no_Huy_applies*(dbhy-dbly+1)*(dbhz-dblzp1+1)

allocate(xsi_lowX_Ez(dbhy-dblyp1+1,dbhz-dblz+1,no_Huy_applies), STAT=allocstat)
call check_allocate(allocstat,'xsi_lowX_Ez',1)
bytes = bytes + no_Huy_applies*(dbhy-dblyp1+1)*(dbhz-dblz+1)

! high X
allocate(xsi_highX_Ey(dbhy-dbly+1,dbhz-dblzp1+1,no_Huy_applies),STAT=allocstat)
call check_allocate(allocstat,'xsi_highX_Ey',1)
bytes = bytes + no_Huy_applies*(dbhy-dbly+1)*(dbhz-dblzp1+1)

allocate(xsi_highX_Ez(dbhy-dblyp1+1,dbhz-dblz+1,no_Huy_applies),STAT=allocstat)
call check_allocate(allocstat,'xsi_highX_Ez',1)
bytes = bytes + no_Huy_applies*(dbhy-dblyp1+1)*(dbhz-dblz+1)

! low Y
allocate(xsi_lowY_Ex(dbhx-dblx+1,dbhz-dblzp1+1,no_Huy_applies), STAT=allocstat)
call check_allocate(allocstat,'xsi_lowY_Ex',1)
bytes = bytes + no_Huy_applies*(dbhx-dblx+1)*(dbhz-dblzp1+1)

allocate(xsi_lowY_Ez(dbhx-dblxp1+1,dbhz-dblz+1,no_Huy_applies), STAT=allocstat)
call check_allocate(allocstat,'xsi_lowY_Ez',1)
bytes = bytes + no_Huy_applies*(dbhx-dblxp1+1)*(dbhz-dblz+1)

! high Y
allocate(xsi_highY_Ex(dbhx-dblx+1,dbhz-dblzp1+1,no_Huy_applies),STAT=allocstat)
call check_allocate(allocstat,'xsi_highY_Ex',1)
bytes = bytes + no_Huy_applies*(dbhx-dblx+1)*(dbhz-dblzp1+1)

allocate(xsi_highY_Ez(dbhx-dblxp1+1,dbhz-dblz+1,no_Huy_applies),STAT=allocstat)
call check_allocate(allocstat,'xsi_highY_Ez',1)
bytes = bytes + no_Huy_applies*(dbhx-dblxp1+1)*(dbhz-dblz+1)

! low Z
allocate(xsi_lowZ_Ex(dbhx-dblx+1,dbhy-dblyp1+1,no_Huy_applies), STAT=allocstat)
call check_allocate(allocstat,'xsi_lowZ_Ex',1)
bytes = bytes + no_Huy_applies*(dbhx-dblx+1)*(dbhy-dblyp1+1)

allocate(xsi_lowZ_Ey(dbhx-dblxp1+1,dbhy-dbly+1,no_Huy_applies), STAT=allocstat)
call check_allocate(allocstat,'xsi_lowZ_Ey',1)
bytes = bytes + no_Huy_applies*(dbhx-dblxp1+1)*(dbhy-dbly+1)

! high Z
allocate(xsi_highZ_Ex(dbhx-dblx+1,dbhy-dblyp1+1,no_Huy_applies),STAT=allocstat)
call check_allocate(allocstat,'xsi_highZ_Ex',1)
bytes = bytes + no_Huy_applies*(dbhx-dblx+1)*(dbhy-dblyp1+1)

allocate(xsi_highZ_Ey(dbhx-dblxp1+1,dbhy-dbly+1,no_Huy_applies),STAT=allocstat)
call check_allocate(allocstat,'xsi_highZ_Ey',1)
bytes = bytes + no_Huy_applies*(dbhx-dblxp1+1)*(dbhy-dbly+1)


bytes = bytes*bytes_per_float
write(*,*) 'Ret. time arrays allocated in Huygens_mod, bytes used = ', bytes
bytes_allocated = bytes_allocated + bytes

!
! Compute retarded time arrays
!
do ii=1,nx
  Xvec(ii) = (ii-1)*dx
end do

do ii=1,ny
  Yvec(ii) = (ii-1)*dy
end do

do ii=1,nz
  Zvec(ii) = (ii-1)*dz
end do

do ih=1,no_Huy_applies  ! no_Huy_applies is equal to 1 or 2
  
! ------------------------ Low x Huygens Surface ---------------------------
  Yx(1:ny,:) = spread(Yvec(1:ny),2,nz)  
  Zx(:,1:nz) = spread(Zvec(1:nz),1,ny)
  Xx = Xvec(dblx)

  xsi_lowX_Hy(:,:,ih) =                                                       &
       k_div_c0(1,ih)*(Xx(dblyp1:dbhy,dblz:dbhz)-        X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yx(dblyp1:dbhy,dblz:dbhz)-        X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zx(dblyp1:dbhy,dblz:dbhz)+dz*half-X0(3,ih))  

  xsi_lowX_Hz(:,:,ih) =                                                       &
       k_div_c0(1,ih)*(Xx(dbly:dbhy,dblzp1:dbhz)-        X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yx(dbly:dbhy,dblzp1:dbhz)+dy*half-X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zx(dbly:dbhy,dblzp1:dbhz)-        X0(3,ih)) 

! ------------------------ High x Huygens Surface --------------------------
  Xx = Xvec(dbhxp1)

  xsi_highX_Hy(:,:,ih) =                                                      &
       k_div_c0(1,ih)*(Xx(dblyp1:dbhy,dblz:dbhz)-        X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yx(dblyp1:dbhy,dblz:dbhz)-        X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zx(dblyp1:dbhy,dblz:dbhz)+dz*half-X0(3,ih)) 

  xsi_highX_Hz(:,:,ih) =                                                      &
       k_div_c0(1,ih)*(Xx(dbly:dbhy,dblzp1:dbhz)-        X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yx(dbly:dbhy,dblzp1:dbhz)+dy*half-X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zx(dbly:dbhy,dblzp1:dbhz)-        X0(3,ih)) 

! ------------------------ Low y Huygens Surface ---------------------------
  Xy(1:nx,:) = spread(Xvec(1:nx),2,nz)
  Zy(:,1:nz) = spread(Zvec(1:nz),1,nx)
  Yy = Yvec(dbly)

  xsi_lowY_Hx(:,:,ih) =                                                       &
       k_div_c0(1,ih)*(Xy(dblxp1:dbhx,dblz:dbhz)-        X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yy(dblxp1:dbhx,dblz:dbhz)-        X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zy(dblxp1:dbhx,dblz:dbhz)+dz*half-X0(3,ih)) 

  xsi_lowY_Hz(:,:,ih) =                                                       &
       k_div_c0(1,ih)*(Xy(dblx:dbhx,dblzp1:dbhz)+dx*half-X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yy(dblx:dbhx,dblzp1:dbhz)-        X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zy(dblx:dbhx,dblzp1:dbhz)-        X0(3,ih)) 

! ------------------------ High y Huygens Surface --------------------------
  Yy = Yvec(dbhyp1)

  xsi_highY_Hx(:,:,ih) =                                                      &
       k_div_c0(1,ih)*(Xy(dblxp1:dbhx,dblz:dbhz)-        X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yy(dblxp1:dbhx,dblz:dbhz)-        X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zy(dblxp1:dbhx,dblz:dbhz)+dz*half-X0(3,ih)) 

  xsi_highY_Hz(:,:,ih) =                                                      &
       k_div_c0(1,ih)*(Xy(dblx:dbhx,dblzp1:dbhz)+dx*half-X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yy(dblx:dbhx,dblzp1:dbhz)-        X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zy(dblx:dbhx,dblzp1:dbhz)-        X0(3,ih)) 

! ------------------------ Low z Huygens Surface ---------------------------
  Xz(1:nx,:) = spread(Xvec(1:nx),2,ny)
  Yz(:,1:ny) = spread(Yvec(1:ny),1,nx)
  Zz = Zvec(dblz)

  xsi_lowZ_Hx(:,:,ih) =                                                       &
       k_div_c0(1,ih)*(Xz(dblxp1:dbhx,dbly:dbhy)-        X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yz(dblxp1:dbhx,dbly:dbhy)+dy*half-X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zz(dblxp1:dbhx,dbly:dbhy)-        X0(3,ih)) 

  xsi_lowZ_Hy(:,:,ih) =                                                       &
       k_div_c0(1,ih)*(Xz(dblx:dbhx,dblyp1:dbhy)+dx*half-X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yz(dblx:dbhx,dblyp1:dbhy)-        X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zz(dblx:dbhx,dblyp1:dbhy)-        X0(3,ih)) 

! ------------------------ High z Huygens Surface --------------------------
  Zz = Zvec(dbhzp1)

  xsi_highZ_Hx(:,:,ih) =                                                      &
       k_div_c0(1,ih)*(Xz(dblxp1:dbhx,dbly:dbhy)-        X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yz(dblxp1:dbhx,dbly:dbhy)+dy*half-X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zz(dblxp1:dbhx,dbly:dbhy)-        X0(3,ih)) 

  xsi_highZ_Hy(:,:,ih) =                                                      &
       k_div_c0(1,ih)*(Xz(dblx:dbhx,dblyp1:dbhy)+dx*half-X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yz(dblx:dbhx,dblyp1:dbhy)-        X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zz(dblx:dbhx,dblyp1:dbhy)-        X0(3,ih)) 

! E update
! ------------------------ Low x Huygens Surface ---------------------------
  Yx(1:ny,:) = spread(Yvec(1:ny),2,nz)  
  Zx(:,1:nz) = spread(Zvec(1:nz),1,ny)
  Xx = Xvec(dblx)

  xsi_lowX_Ey(:,:,ih) =                                                       &
       k_div_c0(1,ih)*(Xx(dbly:dbhy,dblzp1:dbhz)+dx*half-X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yx(dbly:dbhy,dblzp1:dbhz)+dy*half-X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zx(dbly:dbhy,dblzp1:dbhz)-        X0(3,ih))  

  xsi_lowX_Ez(:,:,ih) =                                                       &
       k_div_c0(1,ih)*(Xx(dblyp1:dbhy,dblz:dbhz)+dx*half-X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yx(dblyp1:dbhy,dblz:dbhz)-        X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zx(dblyp1:dbhy,dblz:dbhz)+dz*half-X0(3,ih)) 

! ------------------------ High x Huygens Surface --------------------------
  Xx = Xvec(dbhx)

  xsi_highX_Ey(:,:,ih) =                                                      &
       k_div_c0(1,ih)*(Xx(dbly:dbhy,dblzp1:dbhz)+dx*half-X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yx(dbly:dbhy,dblzp1:dbhz)+dy*half-X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zx(dbly:dbhy,dblzp1:dbhz)-        X0(3,ih)) 

  xsi_highX_Ez(:,:,ih) =                                                      &
       k_div_c0(1,ih)*(Xx(dblyp1:dbhy,dblz:dbhz)+dx*half-X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yx(dblyp1:dbhy,dblz:dbhz)-        X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zx(dblyp1:dbhy,dblz:dbhz)+dz*half-X0(3,ih)) 

! ------------------------ Low y Huygens Surface ---------------------------
  Xy(1:nx,:) = spread(Xvec(1:nx),2,nz)
  Zy(:,1:nz) = spread(Zvec(1:nz),1,nx)
  Yy = Yvec(dbly)

  xsi_lowY_Ex(:,:,ih) =                                                       &
       k_div_c0(1,ih)*(Xy(dblx:dbhx,dblzp1:dbhz)+dx*half-X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yy(dblx:dbhx,dblzp1:dbhz)+dy*half-X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zy(dblx:dbhx,dblzp1:dbhz)-        X0(3,ih)) 

  xsi_lowY_Ez(:,:,ih) =                                                       &
       k_div_c0(1,ih)*(Xy(dblxp1:dbhx,dblz:dbhz)-        X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yy(dblxp1:dbhx,dblz:dbhz)+dy*half-X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zy(dblxp1:dbhx,dblz:dbhz)+dz*half-X0(3,ih)) 

! ------------------------ High y Huygens Surface --------------------------
  Yy = Yvec(dbhy)

  xsi_highY_Ex(:,:,ih) =                                                      &
       k_div_c0(1,ih)*(Xy(dblx:dbhx,dblzp1:dbhz)+dx*half-X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yy(dblx:dbhx,dblzp1:dbhz)+dy*half-X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zy(dblx:dbhx,dblzp1:dbhz)-        X0(3,ih)) 

  xsi_highY_Ez(:,:,ih) =                                                      &
       k_div_c0(1,ih)*(Xy(dblxp1:dbhx,dblz:dbhz)-        X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yy(dblxp1:dbhx,dblz:dbhz)+dy*half-X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zy(dblxp1:dbhx,dblz:dbhz)+dz*half-X0(3,ih)) 

! ------------------------ Low z Huygens Surface ---------------------------
  Xz(1:nx,:) = spread(Xvec(1:nx),2,ny)
  Yz(:,1:ny) = spread(Yvec(1:ny),1,nx)
  Zz = Zvec(dblz)

  xsi_lowZ_Ex(:,:,ih) =                                                       &
       k_div_c0(1,ih)*(Xz(dblx:dbhx,dblyp1:dbhy)+dx*half-X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yz(dblx:dbhx,dblyp1:dbhy)-        X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zz(dblx:dbhx,dblyp1:dbhy)+dz*half-X0(3,ih)) 

  xsi_lowZ_Ey(:,:,ih) =                                                       &
       k_div_c0(1,ih)*(Xz(dblxp1:dbhx,dbly:dbhy)-        X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yz(dblxp1:dbhx,dbly:dbhy)+dy*half-X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zz(dblxp1:dbhx,dbly:dbhy)+dz*half-X0(3,ih)) 

! ------------------------ High z Huygens Surface --------------------------
  Zz = Zvec(dbhz)

  xsi_highZ_Ex(:,:,ih) =                                                      &
       k_div_c0(1,ih)*(Xz(dblx:dbhx,dblyp1:dbhy)+dx*half-X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yz(dblx:dbhx,dblyp1:dbhy)-        X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zz(dblx:dbhx,dblyp1:dbhy)+dz*half-X0(3,ih)) 

  xsi_highZ_Ey(:,:,ih) =                                                      &
       k_div_c0(1,ih)*(Xz(dblxp1:dbhx,dbly:dbhy)-        X0(1,ih)) +          &
       k_div_c0(2,ih)*(Yz(dblxp1:dbhx,dbly:dbhy)+dy*half-X0(2,ih)) +          &
       k_div_c0(3,ih)*(Zz(dblxp1:dbhx,dbly:dbhy)+dz*half-X0(3,ih)) 

end do

END SUBROUTINE Huygens_init

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       HuygensH - Huygens H-field update
!
! DESCRIPTION
!       PUBLIC Huygens update function
!
! METHOD (OpenMP)
!       Some components on the edges belong to two surfaces. This is the 
!       reason for having three separate OMP SECTIONS with four sections each.
!       Hence, a better speed-up that four cannot be expected. To create
!       twelve independent sections we need to treat the edges separately.
!
! SYNOPSIS
!       CALL HuygensH(ts,Hx,Hy,Hz)
!         integer, intent(in) :: ts
!         real(kind=rfp), intent(inout),                                      &
!             dimension(xstart:xstop,ystart:ystop,zstart:zstop) :: Hx, Hy, Hz
!       
! ERRORS
!       No error handling
!
! SEE ALSO
!       HuygensE
!
! HISTORY
!       First version written by Gunnar Ledfelt and Ulf Andersson Dec. 1995
!       OpenMP directives added by ulfa 2003-02-05
!       Optimized (precalculated retarded times) by ulfa May 2003
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE HuygensH(ts,Hx,Hy,Hz)
USE parameter_mod, ONLY : half
USE excite_mod, ONLY    : excitation

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: ts
real(kind=rfp), intent(inout),                                                &
                dimension(xstart:xstop,ystart:ystop,zstart:zstop) :: Hx, Hy, Hz

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

real(kind=rfp) :: t

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

t = (ts-1)*dt

#ifndef SPEC_CPU
!$OMP PARALLEL SHARED(ih) 
#endif
do ih=1,no_Huy_applies  ! no_Huy_applies is equal to 1 or 2
! ------------------------ Low x Huygens Surface ---------------------------
#ifndef SPEC_CPU
!$OMP SECTIONS 
#endif
#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Hy(dblx,dblyp1:dbhy,dblz:dbhz) = Hy(dblx,dblyp1:dbhy,dblz:dbhz) -           &
       coeff_X_Hy(ih)*excitation(t-xsi_lowX_Hy(:,:,ih),Huy_param,HuyPulseType)

#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Hz(dblx,dbly:dbhy,dblzp1:dbhz) = Hz(dblx,dbly:dbhy,dblzp1:dbhz) +           &
       coeff_X_Hz(ih)*excitation(t-xsi_lowX_Hz(:,:,ih),Huy_param,HuyPulseType)
 
! ------------------------ High x Huygens Surface --------------------------
#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Hy(dbhx,dblyp1:dbhy,dblz:dbhz) = Hy(dbhx,dblyp1:dbhy,dblz:dbhz) +           &
       coeff_X_Hy(ih)*excitation(t-xsi_highX_Hy(:,:,ih),Huy_param,HuyPulseType)

#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Hz(dbhx,dbly:dbhy,dblzp1:dbhz) = Hz(dbhx,dbly:dbhy,dblzp1:dbhz) -           &
       coeff_X_Hz(ih)*excitation(t-xsi_highX_Hz(:,:,ih),Huy_param,HuyPulseType)

#ifndef SPEC_CPU
!$OMP END SECTIONS
#endif
! ------------------------ Low y Huygens Surface ---------------------------
#ifndef SPEC_CPU
!$OMP SECTIONS 
#endif
#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Hx(dblxp1:dbhx,dbly,dblz:dbhz) = Hx(dblxp1:dbhx,dbly,dblz:dbhz) +           &
       coeff_Y_Hx(ih)*excitation(t-xsi_lowY_Hx(:,:,ih),Huy_param,HuyPulseType)

#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Hz(dblx:dbhx,dbly,dblzp1:dbhz) = Hz(dblx:dbhx,dbly,dblzp1:dbhz) -           &
       coeff_Y_Hz(ih)*excitation(t-xsi_lowY_Hz(:,:,ih),Huy_param,HuyPulseType)


! ------------------------ High y Huygens Surface --------------------------
#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Hx(dblxp1:dbhx,dbhy,dblz:dbhz) = Hx(dblxp1:dbhx,dbhy,dblz:dbhz) -           &
       coeff_Y_Hx(ih)*excitation(t-xsi_highY_Hx(:,:,ih),Huy_param,HuyPulseType)

#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Hz(dblx:dbhx,dbhy,dblzp1:dbhz) = Hz(dblx:dbhx,dbhy,dblzp1:dbhz) +           &
       coeff_Y_Hz(ih)*excitation(t-xsi_highY_Hz(:,:,ih),Huy_param,HuyPulseType)


#ifndef SPEC_CPU
!$OMP END SECTIONS
#endif
! ------------------------ Low z Huygens Surface ---------------------------
#ifndef SPEC_CPU
!$OMP SECTIONS
#endif
#ifndef SPEC_CPU
!$OMP SECTION
#endif

  if (dblz > pec_gp_zindex) then   ! Avoid part under PEC surface

    Hx(dblxp1:dbhx,dbly:dbhy,dblz) = Hx(dblxp1:dbhx,dbly:dbhy,dblz) -         &
        coeff_Z_Hx(ih)*excitation(t-xsi_lowZ_Hx(:,:,ih),Huy_param,HuyPulseType)
!$ end if
#ifndef SPEC_CPU
!$OMP SECTION
#endif
!$ if (dblz > pec_gp_zindex) then   ! Avoid part under PEC surface

    Hy(dblx:dbhx,dblyp1:dbhy,dblz) = Hy(dblx:dbhx,dblyp1:dbhy,dblz) +         &
        coeff_Z_Hy(ih)*excitation(t-xsi_lowZ_Hy(:,:,ih),Huy_param,HuyPulseType)
  end if

! ------------------------ High z Huygens Surface --------------------------
#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Hx(dblxp1:dbhx,dbly:dbhy,dbhz) = Hx(dblxp1:dbhx,dbly:dbhy,dbhz) +           &
       coeff_Z_Hx(ih)*excitation(t-xsi_highZ_Hx(:,:,ih),Huy_param,HuyPulseType)

#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Hy(dblx:dbhx,dblyp1:dbhy,dbhz) = Hy(dblx:dbhx,dblyp1:dbhy,dbhz) -           &
       coeff_Z_Hy(ih)*excitation(t-xsi_highZ_Hy(:,:,ih),Huy_param,HuyPulseType)

#ifndef SPEC_CPU
!$OMP END SECTIONS
#endif
end do ! ih loop
#ifndef SPEC_CPU
!$OMP END PARALLEL
#endif

END SUBROUTINE HuygensH

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       HuygensE - Huygens E-field update
!
! DESCRIPTION
!       PUBLIC Huygens update function
!
! METHOD (OpenMP)
!       Some components on the edges belong to two surfaces. This is the 
!       reason for having three separate OMP SECTIONS with four sections each.
!       Hence, a better speed-up that four cannot be expected. To create
!       twelve independent sections we need to treat the edges separately.
!
! SYNOPSIS
!       CALL HuygensE(ts,Ex,Ey,Ez)
!         integer, intent(in) :: ts
!         real(kind=rfp), intent(inout),                                      &
!           dimension(xstart:xstop+1,ystart:ystop+1,zstart:zstop+1) :: Ex,Ey,Ez
!       
! ERRORS
!       No error handling
!
! SEE ALSO
!       HuygensH
!
! HISTORY
!       First version written by Gunnar Ledfelt and Ulf Andersson Dec. 1995
!       OpenMP directives added by ulfa 2003-02-05
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE HuygensE(ts,Ex,Ey,Ez)
USE parameter_mod, ONLY : half
USE excite_mod, ONLY    : excitation

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: ts
real(kind=rfp), intent(inout),                                                &
        dimension(xstart:xstop+1,ystart:ystop+1,zstart:zstop+1) :: Ex, Ey, Ez

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

real(kind=rfp) :: t

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

t = ts*dt - dt*half

#ifndef SPEC_CPU
!$OMP PARALLEL SHARED(ih)
#endif
do ih=1,no_Huy_applies
! ------------------------ Low x Huygens Surface ---------------------------
#ifndef SPEC_CPU
!$OMP SECTIONS 
#endif
#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Ey(dblx,dbly:dbhy,dblzp1:dbhz) = Ey(dblx,dbly:dbhy,dblzp1:dbhz) +           &
       coeff_X_Ey(ih)*excitation(t-xsi_lowX_Ey(:,:,ih),Huy_param,HuyPulseType)

#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Ez(dblx,dblyp1:dbhy,dblz:dbhz) = Ez(dblx,dblyp1:dbhy,dblz:dbhz) -           &
       coeff_X_Ez(ih)*excitation(t-xsi_lowX_Ez(:,:,ih),Huy_param,HuyPulseType)

! ------------------------ High x Huygens Surface --------------------------
#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Ey(dbhxp1,dbly:dbhy,dblzp1:dbhz) = Ey(dbhxp1,dbly:dbhy,dblzp1:dbhz) -       &
       coeff_X_Ey(ih)*excitation(t-xsi_highX_Ey(:,:,ih),Huy_param,HuyPulseType)

#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Ez(dbhxp1,dblyp1:dbhy,dblz:dbhz) = Ez(dbhxp1,dblyp1:dbhy,dblz:dbhz) +       &
       coeff_X_Ez(ih)*excitation(t-xsi_highX_Ez(:,:,ih),Huy_param,HuyPulseType)

#ifndef SPEC_CPU
!$OMP END SECTIONS
#endif

! ------------------------ Low y Huygens Surface ---------------------------
#ifndef SPEC_CPU
!$OMP SECTIONS 
#endif
#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Ex(dblx:dbhx,dbly,dblzp1:dbhz) = Ex(dblx:dbhx,dbly,dblzp1:dbhz) -           &
       coeff_Y_Ex(ih)*excitation(t-xsi_lowY_Ex(:,:,ih),Huy_param,HuyPulseType)
#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Ez(dblxp1:dbhx,dbly,dblz:dbhz) = Ez(dblxp1:dbhx,dbly,dblz:dbhz) +           &
       coeff_Y_Ez(ih)*excitation(t-xsi_lowY_Ez(:,:,ih),Huy_param,HuyPulseType)

! ------------------------ High y Huygens Surface --------------------------
#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Ex(dblx:dbhx,dbhyp1,dblzp1:dbhz) = Ex(dblx:dbhx,dbhyp1,dblzp1:dbhz) +       &
       coeff_Y_Ex(ih)*excitation(t-xsi_highY_Ex(:,:,ih),Huy_param,HuyPulseType)
#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Ez(dblxp1:dbhx,dbhyp1,dblz:dbhz) = Ez(dblxp1:dbhx,dbhyp1,dblz:dbhz) -       &
       coeff_Y_Ez(ih)*excitation(t-xsi_highY_Ez(:,:,ih),Huy_param,HuyPulseType)

#ifndef SPEC_CPU
!$OMP END SECTIONS
#endif

! ------------------------ Low z Huygens Surface ---------------------------
#ifndef SPEC_CPU
!$OMP SECTIONS 
#endif
#ifndef SPEC_CPU
!$OMP SECTION
#endif

  if (dblz > pec_gp_zindex) then   ! Avoid part under PEC surface
    Ex(dblx:dbhx,dblyp1:dbhy,dblz) = Ex(dblx:dbhx,dblyp1:dbhy,dblz) +         &
        coeff_Z_Ex(ih)*excitation(t-xsi_lowZ_Ex(:,:,ih),Huy_param,HuyPulseType)

!$ end if
#ifndef SPEC_CPU
!$OMP SECTION
#endif
!$ if (dblz > pec_gp_zindex) then   ! Avoid part under PEC surface

    Ey(dblxp1: dbhx,dbly:dbhy,dblz) = Ey(dblxp1:dbhx,dbly:dbhy,dblz) -        &
        coeff_Z_Ey(ih)*excitation(t-xsi_lowZ_Ey(:,:,ih),Huy_param,HuyPulseType)
  end if

! ------------------------ High z Huygens Surface --------------------------
#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Ex(dblx:dbhx,dblyp1:dbhy,dbhzp1) = Ex(dblx:dbhx,dblyp1:dbhy,dbhzp1) -       &
       coeff_Z_Ex(ih)*excitation(t-xsi_highZ_Ex(:,:,ih),Huy_param,HuyPulseType)

#ifndef SPEC_CPU
!$OMP SECTION
#endif
  Ey(dblxp1:dbhx,dbly:dbhy,dbhzp1) = Ey(dblxp1:dbhx,dbly:dbhy,dbhzp1) +       &
       coeff_Z_Ey(ih)*excitation(t-xsi_highZ_Ey(:,:,ih),Huy_param,HuyPulseType)

#ifndef SPEC_CPU
!$OMP END SECTIONS
#endif
end do ! ih loop
#ifndef SPEC_CPU
!$OMP END PARALLEL
#endif

END SUBROUTINE HuygensE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Huygens_end - Deallocate private Huygens data
!
! DESCRIPTION
!       Deallocate private Huygens data (retarded times arrays)
!
! SYNOPSIS
!       CALL Huygens_end
!
! ERRORS
!       Uses the errorcheck module
!
! HISTORY
!       Written by Ulf Andersson, May 2003
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Huygens_end
USE errorcheck_mod, ONLY : check_deallocate

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: allocstat

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

deallocate( xsi_lowX_Hy, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_lowX_Hy', 1)
deallocate( xsi_lowX_Hz, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_lowX_Hz', 1)

deallocate( xsi_highX_Hy, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_highX_Hy', 1)
deallocate( xsi_highX_Hz, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_highX_Hz', 1)


deallocate( xsi_lowY_Hx, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_lowY_Hx', 1)
deallocate( xsi_lowY_Hz, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_lowY_Hz', 1)

deallocate( xsi_highY_Hx, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_highY_Hx', 1)
deallocate( xsi_highY_Hz, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_highY_Hz', 1)


deallocate( xsi_lowZ_Hx, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_lowZ_Hx', 1)
deallocate( xsi_lowZ_Hy, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_lowZ_Hy', 1)

deallocate( xsi_highZ_Hx, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_highZ_Hx', 1)
deallocate( xsi_highZ_Hy, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_highZ_Hy', 1)



deallocate( xsi_lowX_Ey, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_lowX_Ey', 1)
deallocate( xsi_lowX_Ez, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_lowX_Ez', 1)

deallocate( xsi_highX_Ey, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_highX_Ey', 1)
deallocate( xsi_highX_Ez, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_highX_Ez', 1)


deallocate( xsi_lowY_Ex, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_lowY_Ex', 1)
deallocate( xsi_lowY_Ez, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_lowY_Ez', 1)

deallocate( xsi_highY_Ex, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_highY_Ex', 1)
deallocate( xsi_highY_Ez, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_highY_Ez', 1)


deallocate( xsi_lowZ_Ex, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_lowZ_Ex', 1)
deallocate( xsi_lowZ_Ey, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_lowZ_Ey', 1)

deallocate( xsi_highZ_Ex, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_highZ_Ex', 1)
deallocate( xsi_highZ_Ey, STAT=allocstat )
call check_deallocate(allocstat, 'xsi_highZ_Ey', 1)


END SUBROUTINE Huygens_end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE Huygens_mod
