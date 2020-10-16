!+
! NAME
!       NFT_mod - Near-to-far-field transformation in the time domain
!
! DESCRIPTION
!       Calculates near-to-far-field transform in the time domain.
!       These routines are called from leapfrog if NF_Type == 2.
!
! PUBLIC
!       NFT_Init      - initializes the arrays
!       NFT_Store     - integrates surface currents on a surface enclosing
!                       the object
!       NFT_Print     - prints the results to file
!       NFT_setparvar - sets parsed variables
!       NFT_get_Nangle_and_NFT_db
!
! HISTORY
!       Version       Date                 Name
!       Comments
!       -------------------------------------------
!	$Log: NFT.f90,v $
!	Revision 1.5  2005/04/05 10:34:00  ulfa
!	Superfluous line has been removed.
!	
!	Revision 1.4  2003/09/26 21:49:08  ulfa
!	Using write(9,*) was not a good idea. Output for complex values are
!	polluted by parentheses. Switched to write(9,'(4E20.12)')
!	
!	Revision 1.3  2003/09/26 20:15:18  ulfa
!	Added global computation of #bytes allocated.
!	
!	Revision 1.2  2003/09/26 19:55:48  ulfa
!	Switched to write(9,*) from write(9,'(2E15.6)) to avoid loss of
!	precision.
!	
!	Revision 1.1  2003/09/23 14:06:51  ulfa
!	Initial version.
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE NFT_mod

USE parameter_mod, ONLY : rfp, kfp, pi, eps0, mu0
USE globalvar_mod, ONLY : xstart, xstop, ystart, ystop, zstart, zstop,        &
                          dt, dx, dy, dz, nts, Z0, c0
USE posvector_mod

IMPLICIT NONE

PUBLIC :: NFT_Init, NFT_Store, NFT_Print, NFT_setparvar,                      &
          NFT_get_Nangle_and_NFT_db

PRIVATE

character(len=80) :: Outfilenamebase = ''

integer :: X1, Xn, Y1, Yn, Z1, Zn       ! Coordinates of surfaces
integer :: In, Jn, Kn                   ! No. of cells along surfaces
integer :: nth, nphi, Nangle            ! No. of angles
integer :: NFT_db                       ! No. of cells from boundary
integer :: delay

real(kind=rfp) :: toffset
real(kind=rfp) :: th1, th2, phi1, phi2  ! First and last angles
real(kind=rfp) :: dSX, dSY, dSZ         ! Surface elements

logical :: monostat   ! Monostatic RCS

real(kind=rfp), dimension(:,:), allocatable :: Angle     ! Angle array

! Time-domain arrays:
real(kind=rfp), dimension(:),   allocatable :: Ein
real(kind=rfp), dimension(:,:), allocatable :: Eth, Ephi
real(kind=rfp), dimension(:,:), allocatable :: Fth, Fphi, Fth_tau, Fphi_tau

integer, dimension(:,:,:), allocatable :: neyX1, neyX2, nezX1, nezX2
integer, dimension(:,:,:), allocatable :: nexY1, nexY2, nezY1, nezY2
integer, dimension(:,:,:), allocatable :: nexZ1, nexZ2, neyZ1, neyZ2

integer, dimension(:,:,:), allocatable :: nhyX1, nhyX2, nhzX1, nhzX2
integer, dimension(:,:,:), allocatable :: nhxY1, nhxY2, nhzY1, nhzY2
integer, dimension(:,:,:), allocatable :: nhxZ1, nhxZ2, nhyZ1, nhyZ2

real(kind=rfp), dimension(:,:,:), allocatable :: taueyX1, taueyX2
real(kind=rfp), dimension(:,:,:), allocatable :: tauezX1, tauezX2
real(kind=rfp), dimension(:,:,:), allocatable :: tauexY1, tauexY2
real(kind=rfp), dimension(:,:,:), allocatable :: tauezY1, tauezY2
real(kind=rfp), dimension(:,:,:), allocatable :: tauexZ1, tauexZ2
real(kind=rfp), dimension(:,:,:), allocatable :: taueyZ1, taueyZ2

real(kind=rfp), dimension(:,:,:), allocatable :: tauhyX1, tauhyX2
real(kind=rfp), dimension(:,:,:), allocatable :: tauhzX1, tauhzX2
real(kind=rfp), dimension(:,:,:), allocatable :: tauhxY1, tauhxY2
real(kind=rfp), dimension(:,:,:), allocatable :: tauhzY1, tauhzY2
real(kind=rfp), dimension(:,:,:), allocatable :: tauhxZ1, tauhxZ2
real(kind=rfp), dimension(:,:,:), allocatable :: tauhyZ1, tauhyZ2

real(kind=rfp), dimension(:), allocatable :: exth, eyth, ezth, hxth, hyth
real(kind=rfp), dimension(:), allocatable :: exphi, eyphi, hxphi, hyphi, hzphi

type(posvector), dimension(:,:), allocatable :: RXeyhz1,RXeyhz2,RXezhy1,RXezhy2
type(posvector), dimension(:,:), allocatable :: RXhzey1,RXhzey2,RXhyez1,RXhyez2
type(posvector), dimension(:,:), allocatable :: RYexhz1,RYexhz2,RYezhx1,RYezhx2
type(posvector), dimension(:,:), allocatable :: RYhzex1,RYhzex2,RYhxez1,RYhxez2
type(posvector), dimension(:,:), allocatable :: RZexhy1,RZexhy2,RZeyhx1,RZeyhx2
type(posvector), dimension(:,:), allocatable :: RZhyex1,RZhyex2,RZhxey1,RZhxey2

! Frequency domain variables:
complex(kind=kfp), dimension(:),   allocatable :: Ein_f
complex(kind=kfp), dimension(:,:), allocatable :: Eth_f, Ephi_f
complex(kind=kfp), dimension(:,:), allocatable :: Fth_f, Fphi_f
complex(kind=kfp), dimension(:,:), allocatable :: Fth_tau_f, Fphi_tau_f

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       NFT_setparvar - Sets parsed variables
!
! DESCRIPTION
!       Set the private declared variables parsed in module readdata.
!
! SYNOPSIS
!       CALL NFT_setparvar(par_NFTdb,par_nth,par_nphi,par_th1,par_th2,
!                          par_phi1,par_phi2,par_monostat,par_filename)
!       integer, intent(in)           :: par_NFTdb, par_nth, par_nphi
!       real(kind=rfp),    intent(in) :: par_th1, par_th2, par_phi1, par_phi2
!       logical,           intent(in) :: par_monostat
!       character(len=80), intent(in) :: par_filename
!
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE NFT_setparvar(par_NFTdb,par_nth,par_nphi,par_th1,par_th2,          &
                         par_phi1,par_phi2,par_monostat,par_filename)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer,           intent(in) :: par_NFTdb, par_nth, par_nphi
real(kind=rfp),    intent(in) :: par_th1, par_th2, par_phi1, par_phi2
logical,           intent(in) :: par_monostat
character(len=80), intent(in) :: par_filename

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

NFT_db  = par_NFTdb
nth     = par_nth
nphi    = par_nphi
th1     = par_th1
th2     = par_th2
phi1    = par_phi1
phi2    = par_phi2
monostat = par_monostat
Outfilenamebase  = par_filename

END SUBROUTINE NFT_setparvar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       dft_call - Fourier transform of the time-domain fields
!
! DESCRIPTION
!       Contains initializations and calls to fourier_transf_mod. Called from
!       NFT_Print to calculate Fourier transforms of Ein, Fth, Fphi, Fth_tau
!       and Fphi_tau to the frequency domain (for each direction n). The
!       transformed fields are called Ein_f, Fth_f, etc.
!
! SYNOPSIS
!       call dft_call(Ein,Fth,Fphi,Fth_tau,Fphi_tau)
!         real(kind=rfp), intent(in), dimension(:) :: Ein
!         real(kind=rfp), intent(in),
!           dimension(:,:) :: Fth, Fphi, Fth_tau, Fphi_tau
!
! ERRORS
!       Uses errorcheck module
!
! SEE ALSO
!       idft_call, NFT_Print
!
! HISTORY
!       Written by Åke Rydell
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE dft_call(Ein,Fth,Fphi,Fth_tau,Fphi_tau)

USE fourier_transf_mod, ONLY : trf_tab_type, trf_data_type, trf_work_type,    &
                               fourier_init, fourier, fourier_end
USE errorcheck_mod, ONLY     : check_allocate, check_deallocate

!------------------------------------------------------------------------------
!                     A r g u m e n t s              
!------------------------------------------------------------------------------

real(kind=rfp), intent(in), dimension(:)   :: Ein
real(kind=rfp), intent(in), dimension(:,:) :: Fth, Fphi, Fth_tau, Fphi_tau

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

real(kind=rfp),  dimension(:,:,:), allocatable :: Etmp
type fac1_type
  type(trf_tab_type)  :: trf_tab
  type(trf_data_type) :: trf_data
  type(trf_work_type) :: trf_work
end type fac1_type
type(fac1_type) :: fac1
integer         :: m1, m2, nc, n_trf, info, allocstat

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

! Fourier transform of incoming pulse Ein(t):
m1 = 1            ! Number of directions
m2 = size(Ein)    ! Number of time samples
nc = 1            ! Number of components per time vector (scalar -> 1)
n_trf = m1*nc     ! total number of transforms

! Allocations and computation of tables
call fourier_init(m2,1,fac1%trf_tab, fac1%trf_data, fac1%trf_work,info)

allocate(Etmp(1,m2,1), STAT=allocstat)
call check_allocate(allocstat,'Etmp', 1)

Etmp(1,:,1) = Ein
call fourier('A','2',fac1%trf_tab,Etmp, fac1%trf_data, fac1%trf_work,info)

allocate(Ein_f(m2/2+1), STAT=allocstat)
call check_allocate(allocstat,'Ein_f', 1)

Ein_f = fac1%trf_data%Y_C(1,:)

! Termination and deallocation
call fourier_end(fac1%trf_tab, fac1%trf_data, fac1%trf_work,info)

deallocate(Etmp, STAT=allocstat)
call check_deallocate(allocstat, 'Etmp', 1)

! DFT of Fth, Fphi, Fth_tau, Fphi_tau to the frequency domain:
m1 = size(Fth,1)
m2 = size(Fth,2)
nc = 1
n_trf = m1*nc

call fourier_init(m2,n_trf,fac1%trf_tab, fac1%trf_data, fac1%trf_work,info)

!--- Fth:
allocate(Etmp(m1,m2,1), STAT=allocstat)
call check_allocate(allocstat,'Etmp', 1)

Etmp(:,:,1) = Fth
call fourier('A','2',fac1%trf_tab,Etmp, fac1%trf_data, fac1%trf_work,info)

deallocate(Etmp, STAT=allocstat)
call check_deallocate(allocstat, 'Etmp', 1)

allocate(Fth_f(m1,m2/2+1), STAT=allocstat)
call check_allocate(allocstat,'Fth_f', 1)

Fth_f = fac1%trf_data%Y_C

!--- Fphi:
allocate(Etmp(m1,m2,1), STAT=allocstat)
call check_allocate(allocstat,'Etmp', 1)

Etmp(:,:,1) = Fphi
call fourier('A','2',fac1%trf_tab,Etmp, fac1%trf_data, fac1%trf_work,info)

deallocate(Etmp, STAT=allocstat)
call check_deallocate(allocstat, 'Etmp', 1)

allocate(Fphi_f(m1,m2/2+1), STAT=allocstat)
call check_allocate(allocstat,'Fphi_f', 1)

Fphi_f = fac1%trf_data%Y_C

!--- Fth_tau:
allocate(Etmp(m1,m2,1), STAT=allocstat)
call check_allocate(allocstat,'Etmp', 1)

Etmp(:,:,1) = Fth_tau
call fourier('A','2',fac1%trf_tab,Etmp, fac1%trf_data, fac1%trf_work,info)

deallocate(Etmp, STAT=allocstat)
call check_deallocate(allocstat, 'Etmp', 1)

allocate(Fth_tau_f(m1,m2/2+1), STAT=allocstat)
call check_allocate(allocstat,'Fth_tau_f', 1)

Fth_tau_f = fac1%trf_data%Y_C

!--- Fphi_tau:
allocate(Etmp(m1,m2,1), STAT=allocstat)
call check_allocate(allocstat,'Etmp', 1)

Etmp(:,:,1) = Fphi_tau
call fourier('A','2',fac1%trf_tab,Etmp, fac1%trf_data, fac1%trf_work,info)

deallocate(Etmp, STAT=allocstat)
call check_deallocate(allocstat, 'Etmp', 1)

allocate(Fphi_tau_f(m1,m2/2+1), STAT=allocstat)
call check_allocate(allocstat,'Fphi_tau_f', 1)

Fphi_tau_f =  fac1%trf_data%Y_C

!---
call fourier_end(fac1%trf_tab, fac1%trf_data, fac1%trf_work,info)

END SUBROUTINE dft_call

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       idft_call - Inverse Fourier transform of the freq. domain fields
!
! DESCRIPTION
!       Contains initializations and calls to fourier_transf_mod. Called from
!       NFT_Print to calculate inverse Fourier transforms of Eth_f and Ephi_f
!       to the time domain (for each direction n). The transformed fields
!       are called Eth and Ephi.
!
! SYNOPSIS
!       call idft_call(Eth_f,Ephi_f)
!         complex(kind=kfp), intent(in), dimension(:,:) :: Eth_f, Ephi_f
!
! ERRORS
!       Uses errorcheck module
!
! SEE ALSO
!       dft_call, NFT_Print
!
! HISTORY
!       Written by Åke Rydell
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE idft_call(Eth_f,Ephi_f)

USE fourier_transf_mod, ONLY : trf_tab_type, trf_data_type, trf_work_type,    &
                               fourier_init, fourier, fourier_end
USE errorcheck_mod, ONLY     : check_allocate, check_deallocate

!------------------------------------------------------------------------------
!                     A r g u m e n t s              
!------------------------------------------------------------------------------

complex(kind=kfp), intent(in), dimension(:,:) :: Eth_f, Ephi_f

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

real(kind=rfp),  dimension(:,:,:), allocatable :: Etmp
type fac1_type
  type(trf_tab_type)  :: trf_tab
  type(trf_data_type) :: trf_data
  type(trf_work_type) :: trf_work
end type fac1_type
type(fac1_type) :: fac1
integer         :: m1, m2, nc, n_trf, info, le, allocstat

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

m1 = size(Eth_f,1)
m2 = size(Eth_f,2)
nc = 1              ! Number of components per freq. vector (scalar -> 1)
n_trf = m1*nc       ! Total number of transforms to perform
le = 2*m2 - 2       ! Number of time elements

call fourier_init(le,n_trf,fac1%trf_tab, fac1%trf_data, fac1%trf_work,info)

!--- Eth_f:
allocate(Etmp(m1,le,1), STAT=allocstat)
call check_allocate(allocstat,'Etmp', 1)

fac1%trf_data%Y_C = Eth_f
call fourier('S','2',fac1%trf_tab,Etmp, fac1%trf_data, fac1%trf_work,info)

allocate(Eth(m1,le), STAT=allocstat)
call check_allocate(allocstat,'Eth', 1)

Eth = Etmp(:,:,1)

deallocate(Etmp, STAT=allocstat)
call check_deallocate(allocstat, 'Etmp', 1)

!--- Ephi_f:
allocate(Etmp(m1,le,1), STAT=allocstat)
call check_allocate(allocstat,'Etmp', 1)

fac1%trf_data%Y_C = Ephi_f
call fourier('S','2',fac1%trf_tab,Etmp, fac1%trf_data, fac1%trf_work,info)

allocate(Ephi(m1,le), STAT=allocstat)
call check_allocate(allocstat,'Ephi', 1)

Ephi = Etmp(:,:,1)

deallocate(Etmp, STAT=allocstat)
call check_deallocate(allocstat, 'Etmp', 1)

!---
call fourier_end(fac1%trf_tab, fac1%trf_data, fac1%trf_work,info)

END SUBROUTINE idft_call

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       NFT_Init - Initializes variables
!
! DESCRIPTION
!       Initializes position arrays and calculates retarded times. Called
!       before time loop.
!
! SYNOPSIS
!       call NFT_Init(nx,ny,nz,nf_Err)
!         integer, intent(in)  :: nx, ny, nz
!         integer, intent(out) :: nf_Err
!
! ERRORS
!       nf_Err = 0  :  no error
!       nf_Err = 1  :  Number of cells from boundary too large
!       nf_Err = 2  :  Number of angles are negative or zero
!       nf_Err = 3  :  theta1 > theta2 or phi1 > phi2
!
! SEE ALSO
!       NFT_Store, NFT_Print
!
! HISTORY
!       Written by Åke Rydell
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE NFT_Init(nx,ny,nz,nf_Err)
USE parameter_mod, ONLY  : bytes_per_float
USE errorcheck_mod, ONLY : check_allocate, check_deallocate
USE globalvar_mod, ONLY  : bytes_allocated
USE huygens_mod, ONLY    : Huy_wavedir, Huy_phi

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in)  :: nx, ny, nz
integer, intent(out) :: nf_Err

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: i, j, n, minmiddle, allocstat, bytes
real(kind=rfp)    :: th, fi, kvad
real(kind=rfp), dimension(:,:), allocatable :: Tr, Tt   ! Temporary variables
real(kind=rfp), dimension(:),   allocatable :: thv,phiv ! Angle vectors
!                                         Real boundary numbers:
REAL(KIND=rfp)                              :: X1r,Xnr
REAL(KIND=rfp)                              :: Y1r,Ynr
REAL(KIND=rfp)                              :: Z1r,Znr

type(posvector) :: R0, Rhat   ! Position vector

!------------------------------------------------------------------------------
!                     E x e c u t a b l e  c o d e
!------------------------------------------------------------------------------

! -------------------------Initial error-check:--------------------------------

nf_Err = 0

! Check the minimum distance between boundary and middle of comput. vol.
minmiddle = min(nx,ny)
minmiddle = min(nz,minmiddle)

! If the space between the edge and the n-f transf. surface
! is less than half the shortest length it will cause problems:
if (NFT_db >= minmiddle/2) then
  nf_Err = 1
end if

! No. of angles must be a positive number:
if (nth < 1 .or. nphi < 1) then
  nf_Err = 2
end if

if (th1 > th2 .or. phi1 > phi2) then
  nf_Err = 3
end if

! Monostatic:
if (monostat) then
  if (th1 /= 0 .or. th2 /= 0 .or. phi1 /= 0 .or. phi2 /=0) then
    write(*,*) 'Warning! Angles must not be specified when keyword'
    write(*,*) 'monostatic is used. The specified angles are ignored!'
  end if
  nth = 1
  nphi = 1
  ! Using degrees since conversion to radians is done below
  th1 = 180.0_rfp/pi*acos(-Huy_wavedir(3,1))   ! Opposite to Huy_wavedir
  th2 = th1
  if (Huy_phi /= -1111.1_rfp) then
    phi1 = (Huy_phi + pi)*180.0_rfp/pi   ! (phi is in radians.)
  else
    phi1 = 0.0_rfp
    write(*,*) 'Using phi = 0 degrees for monostatic RCS.'
  end if
  if (Huy_wavedir(2,1) < 0.0_rfp) then
    phi1 = 360.0_rfp - phi1
  end if
  if (phi1 > 360.0_rfp) then
    phi1 = phi1 - 360.0_rfp
  end if
  phi2 = phi1
end if   ! Monostatic

Z0 = sqrt(mu0/eps0)    ! Impedance of free space

! Set min and max boundaries for far field transform. The transform will be
! performed NFT_db cells from the outer boundary:

! Lower limit of NF-surface:
X1 = NFT_db
Y1 = NFT_db
Z1 = NFT_db

! Upper limit of NF-surface:
Xn = nx-NFT_db+1
Yn = ny-NFT_db+1
Zn = nz-NFT_db+1

! Number of cells along NF-surfaces:
In = Xn-X1
Jn = Yn-Y1
Kn = Zn-Z1

Nangle = nth*nphi      ! No. of directions

! NOT temporary arrays
allocate(nexY1(In,Kn+1,Nangle), nexY2(In,Kn+1,Nangle),                        &
         nezY1(In+1,Kn,Nangle), nezY2(In+1,Kn,Nangle),                        &
         neyX1(Jn,Kn+1,Nangle), neyX2(Jn,Kn+1,Nangle),                        &
         nezX1(Jn+1,Kn,Nangle), nezX2(Jn+1,Kn,Nangle),                        &
         nexZ1(In,Jn+1,Nangle), nexZ2(In,Jn+1,Nangle),                        &
         neyZ1(In+1,Jn,Nangle), neyZ2(In+1,Jn,Nangle),                        &
         STAT=allocstat)
call check_allocate(allocstat, 'nexY1,... ', 3)

allocate(nhxY1(In+1,Kn,Nangle), nhxY2(In+1,Kn,Nangle),                        &
         nhzY1(In,Kn+1,Nangle), nhzY2(In,Kn+1,Nangle),                        &
         nhyX1(Jn+1,Kn,Nangle), nhyX2(Jn+1,Kn,Nangle),                        &
         nhzX1(Jn,Kn+1,Nangle), nhzX2(Jn,Kn+1,Nangle),                        &
         nhxZ1(In+1,Jn,Nangle), nhxZ2(In+1,Jn,Nangle),                        &
         nhyZ1(In,Jn+1,Nangle), nhyZ2(In,Jn+1,Nangle),                        &
         STAT=allocstat)
call check_allocate(allocstat, 'nhxY1,... ', 3)
bytes = ( In*(Kn+1)*2+Kn*(In+1)*2+                                            &
          Jn*(Kn+1)*2+Kn*(Jn+1)*2+                                            &
          In*(Jn+1)*2+Jn*(In+1)*2 )*Nangle*8 ! 8=4*2, 4 bytes/integer and
                                             !        2 allocate statements
allocate(tauexY1(In,Kn+1,Nangle), tauexY2(In,Kn+1,Nangle),                    &
         tauezY1(In+1,Kn,Nangle), tauezY2(In+1,Kn,Nangle),                    &
         taueyX1(Jn,Kn+1,Nangle), taueyX2(Jn,Kn+1,Nangle),                    &
         tauezX1(Jn+1,Kn,Nangle), tauezX2(Jn+1,Kn,Nangle),                    &
         tauexZ1(In,Jn+1,Nangle), tauexZ2(In,Jn+1,Nangle),                    &
         taueyZ1(In+1,Jn,Nangle), taueyZ2(In+1,Jn,Nangle),                    &
         STAT=allocstat)
call check_allocate(allocstat, 'tauexY1,...', 3)

allocate(tauhxY1(In+1,Kn,Nangle), tauhxY2(In+1,Kn,Nangle),                    &
         tauhzY1(In,Kn+1,Nangle), tauhzY2(In,Kn+1,Nangle),                    &
         tauhyX1(Jn+1,Kn,Nangle), tauhyX2(Jn+1,Kn,Nangle),                    &
         tauhzX1(Jn,Kn+1,Nangle), tauhzX2(Jn,Kn+1,Nangle),                    &
         tauhxZ1(In+1,Jn,Nangle), tauhxZ2(In+1,Jn,Nangle),                    &
         tauhyZ1(In,Jn+1,Nangle), tauhyZ2(In,Jn+1,Nangle),                    &
         STAT=allocstat)
call check_allocate(allocstat, 'tauhxY1,...', 3)

bytes = bytes + ( In*(Kn+1)*2+Kn*(In+1)*2+                                    &
                  Jn*(Kn+1)*2+Kn*(Jn+1)*2+                                    &
                  In*(Jn+1)*2+Jn*(In+1)*2 )*Nangle*2*bytes_per_float

! Position vectors:
!------------------------------------------------------------------------------

allocate(RXeyhz1(Jn,Kn+1),RXeyhz2(Jn,Kn+1),RXezhy1(Jn+1,Kn),RXezhy2(Jn+1,Kn), &
         STAT=allocstat)
call check_allocate(allocstat, 'RXeyhz1, RXeyhz2, RXezhy1, RXezhy2', 3)
allocate(RXhzey1(Jn,Kn+1),RXhzey2(Jn,Kn+1),RXhyez1(Jn+1,Kn),RXhyez2(Jn+1,Kn), &
         STAT=allocstat)
call check_allocate(allocstat, 'RXhzey1, RXhzey2, RXhyez1, RXhyez2', 3)
allocate(RYexhz1(In,Kn+1),RYexhz2(In,Kn+1),RYezhx1(In+1,Kn),RYezhx2(In+1,Kn), &
         STAT=allocstat)
call check_allocate(allocstat, 'RYexhz1, RYexhz2, RYezhx1, RYezhx2', 3)
allocate(RYhzex1(In,Kn+1),RYhzex2(In,Kn+1),RYhxez1(In+1,Kn),RYhxez2(In+1,Kn), &
         STAT=allocstat)
call check_allocate(allocstat, 'RYhzex1, RYhzex2, RYhxez1, RYhxez2', 3)
allocate(RZexhy1(In,Jn+1),RZexhy2(In,Jn+1),RZeyhx1(In+1,Jn),RZeyhx2(In+1,Jn), &
         STAT=allocstat)
call check_allocate(allocstat, 'RZexhy1, RZexhy2, RZeyhx1, RZeyhx2', 3)
allocate(RZhyex1(In,Jn+1),RZhyex2(In,Jn+1),RZhxey1(In+1,Jn),RZhxey2(In+1,Jn), &
         STAT=allocstat)
call check_allocate(allocstat, 'RZhyex1, RZhyex2, RZhxey1, RZhxey2', 3)
bytes = bytes + ( In*(Kn+1)*4+Kn*(In+1)*4+                                    &
                  Jn*(Kn+1)*4+Kn*(Jn+1)*4+                                    &
                  In*(Jn+1)*4+Jn*(In+1)*4 )*3*bytes_per_float
                                          ! 3 comp. in type posvector.
! Components of Green's function
allocate(exth(Nangle), eyth(Nangle), ezth(Nangle), hxth(Nangle),              &
         hyth(Nangle), exphi(Nangle), eyphi(Nangle), hxphi(Nangle),           &
         hyphi(Nangle), hzphi(Nangle), STAT=allocstat)
call check_allocate(allocstat, 'exth, etc.', 3)
!bytes = bytes + Nangle*10 ! 1D arrays ignored

!------------------------------------------------------------------------------

! Temporary arrays:
allocate(Tr(max(In, Jn, Kn)+1, max(In, Jn, Kn)+1), STAT=allocstat)
call check_allocate(allocstat, 'Tr', 3)
allocate(Tt(max(In, Jn, Kn)+1, max(In, Jn, Kn)+1), STAT=allocstat)
call check_allocate(allocstat, 'Tt', 3)

allocate(Angle(nth*nphi,2), STAT=allocstat)
call check_allocate(allocstat, 'Angle', 3)

allocate(thv(1:nth), phiv(1:nphi), STAT=allocstat)
call check_allocate(allocstat, 'thv, phiv', 3)

! Angle vectors:
if (nth > 1) then
  do i=1,nth
    thv(i) = (th1 + (i-1)*(th2-th1)/(nth-1))*pi/180.0_rfp
  end do
else
  thv(1) =  th1*pi/180.0_rfp
end if

if (nphi > 1) then
  do i=1,nphi
    phiv(i) = (phi1 + (i-1)*(phi2-phi1)/(nphi-1))*pi/180.0_rfp
  end do
else
  phiv(1) =  phi1*pi/180.0_rfp
end if

! Position of the phase reference point:

R0 = Setposvector((nx+1)*0.5_rfp*dx,(ny+1)*0.5_rfp*dy,(nz+1)*0.5_rfp*dz)

! Convert boundaries to real values
X1r = REAL(X1)
Xnr = REAL(Xn)
Y1r = REAL(Y1)
Ynr = REAL(Yn)
Z1r = REAL(Z1)
Znr = REAL(Zn)

! Prepare position vectors:----------------------------------------------



! X-surfaces: ----
RXeyhz1 = SetRvector(Jn,Kn+1,Y1r+0.5_rfp,Z1r,X1r-0.5_rfp,dx,dy,dz,'X')
RXeyhz2 = SetRvector(Jn,Kn+1,Y1r+0.5_rfp,Z1r,Xnr+0.5_rfp,dx,dy,dz,'X')
RXhzey1 = SetRvector(Jn,Kn+1,Y1r+0.5_rfp,Z1r,X1r,dx,dy,dz,'X')
RXhzey2 = SetRvector(Jn,Kn+1,Y1r+0.5_rfp,Z1r,Xnr,dx,dy,dz,'X')
!-----
RXezhy1 = SetRvector(Jn+1,Kn,Y1r,Z1r+0.5_rfp,X1r-0.5_rfp,dx,dy,dz,'X')
RXezhy2 = SetRvector(Jn+1,Kn,Y1r,Z1r+0.5_rfp,Xnr+0.5_rfp,dx,dy,dz,'X')
RXhyez1 = SetRvector(Jn+1,Kn,Y1r,Z1r+0.5_rfp,X1r,dx,dy,dz,'X')
RXhyez2 = SetRvector(Jn+1,Kn,Y1r,Z1r+0.5_rfp,Xnr,dx,dy,dz,'X')
!-----

! Y-surfaces: ----
RYexhz1 = SetRvector(In,Kn+1,X1r+0.5_rfp,Z1r,Y1r-0.5_rfp,dx,dy,dz,'Y')
RYexhz2 = SetRvector(In,Kn+1,X1r+0.5_rfp,Z1r,Ynr+0.5_rfp,dx,dy,dz,'Y')
RYhzex1 = SetRvector(In,Kn+1,X1r+0.5_rfp,Z1r,Y1r,dx,dy,dz,'Y')
RYhzex2 = SetRvector(In,Kn+1,X1r+0.5_rfp,Z1r,Ynr,dx,dy,dz,'Y')
!-----
RYezhx1 = SetRvector(In+1,Kn,X1r,Z1r+0.5_rfp,Y1r-0.5_rfp,dx,dy,dz,'Y')
RYezhx2 = SetRvector(In+1,Kn,X1r,Z1r+0.5_rfp,Ynr+0.5_rfp,dx,dy,dz,'Y')
RYhxez1 = SetRvector(In+1,Kn,X1r,Z1r+0.5_rfp,Y1r,dx,dy,dz,'Y')
RYhxez2 = SetRvector(In+1,Kn,X1r,Z1r+0.5_rfp,Ynr,dx,dy,dz,'Y')

! Z-surfaces:-----
RZexhy1 = SetRvector(In,Jn+1,X1r+0.5_rfp,Y1r,Z1r-0.5_rfp,dx,dy,dz,'Z')
RZexhy2 = SetRvector(In,Jn+1,X1r+0.5_rfp,Y1r,Znr+0.5_rfp,dx,dy,dz,'Z')
RZhyex1 = SetRvector(In,Jn+1,X1r+0.5_rfp,Y1r,Z1r,dx,dy,dz,'Z')
RZhyex2 = SetRvector(In,Jn+1,X1r+0.5_rfp,Y1r,Znr,dx,dy,dz,'Z')
!-----
RZeyhx1 = SetRvector(In+1,Jn,X1r,Y1r+0.5_rfp,Z1r-0.5_rfp,dx,dy,dz,'Z')
RZeyhx2 = SetRvector(In+1,Jn,X1r,Y1r+0.5_rfp,Znr+0.5_rfp,dx,dy,dz,'Z')
RZhxey1 = SetRvector(In+1,Jn,X1r,Y1r+0.5_rfp,Z1r,dx,dy,dz,'Z')
RZhxey2 = SetRvector(In+1,Jn,X1r,Y1r+0.5_rfp,Znr,dx,dy,dz,'Z')
! End Position vectors: ---------



! Set phase reference point at R0: ----------------
RXeyhz1 = RXeyhz1 - R0
RXeyhz2 = RXeyhz2 - R0
RXhzey1 = RXhzey1 - R0
RXhzey2 = RXhzey2 - R0
RXezhy1 = RXezhy1 - R0
RXezhy2 = RXezhy2 - R0
RXhyez1 = RXhyez1 - R0
RXhyez2 = RXhyez2 - R0

!--------End X ------------------------------------------


RYexhz1 = RYexhz1 - R0
RYexhz2 = RYexhz2 - R0
RYhzex1 = RYhzex1 - R0
RYhzex2 = RYhzex2 - R0
RYezhx1 = RYezhx1 - R0
RYezhx2 = RYezhx2 - R0
RYhxez1 = RYhxez1 - R0
RYhxez2 = RYhxez2 - R0

!--------End Y ------------------------------------------


RZexhy1 = RZexhy1 - R0
RZexhy2 = RZexhy2 - R0
RZhyex1 = RZhyex1 - R0
RZhyex2 = RZhyex2 - R0
RZeyhx1 = RZeyhx1 - R0
RZeyhx2 = RZeyhx2 - R0
RZhxey1 = RZhxey1 - R0
RZhxey2 = RZhxey2 - R0

!--------End Z ------------------------------------------

! Create angle array. Angle(m,2), m=nth*nphi
n=1
do i=1,nth
  do j=1,nphi
    Angle(n,1) = thv(i)
    Angle(n,2) = phiv(j)
    n=n+1
  end do
end do

do n = 1,Nangle
  th = Angle(n,1)
  fi = Angle(n,2)

  exth(n) =  cos(fi)*cos(th)
  eyth(n) =  sin(fi)*cos(th)
  ezth(n) = -sin(th)
  hxth(n) = -sin(fi)/Z0
  hyth(n) =  cos(fi)/Z0
! hzth =  0.0
  exphi(n) = -sin(fi)
  eyphi(n) =  cos(fi)
! ezphi =  0.0
  hxphi(n) = -cos(fi)*cos(th)/Z0
  hyphi(n) = -sin(fi)*cos(th)/Z0
  hzphi(n) =  sin(th)/Z0
end do   ! Angle loop

! Arbitrary time offset to obs. point
kvad = In**2 + Jn**2 + Kn**2
toffset = 0.5_rfp * sqrt(kvad)/(dt*c0)*max(dx,dy,dz) + 4
          ! No. of time steps along half the diagonal
delay = 2 * (int(toffset)+1)

! Calculating retarded times for each direction:
! ==============================================

do n = 1,Nangle
  th = Angle(n,1)
  fi = Angle(n,2)

  Rhat = Setposvector(cos(fi)*sin(th),sin(fi)*sin(th),cos(th))


! Below 0.5 is added to the time delays for the E fields since they are
! calculated half a time step after the H fields.

! H FIELDS:
!---------

! X1 surface-------------------------------------------------------------------
! Hy
  Tr(1:Jn+1,1:Kn) = dotprod(RXezhy1,Rhat) / (c0*dt)
  Tt(1:Jn+1,1:Kn) = toffset - Tr(1:Jn+1,1:Kn) + 0.5_rfp
  nhyX1(:,:,n) = nint(Tt(1:Jn+1,1:Kn))
  tauhyX1(:,:,n) = Tt(1:Jn+1,1:Kn) - nhyX1(:,:,n)

! Hz
  Tr(1:Jn,1:Kn+1) = dotprod(RXeyhz1,Rhat) / (c0*dt)
  Tt(1:Jn,1:Kn+1) = toffset - Tr(1:Jn,1:Kn+1) + 0.5_rfp
  nhzX1(:,:,n) = nint(Tt(1:Jn,1:Kn+1))
  tauhzX1(:,:,n) = Tt(1:Jn,1:Kn+1) - nhzX1(:,:,n)

! X2 surface-------------------------
! Hy
  Tr(1:Jn+1,1:Kn) = dotprod(RXezhy2,Rhat) / (c0*dt)
  Tt(1:Jn+1,1:Kn) = toffset - Tr(1:Jn+1,1:Kn) + 0.5_rfp
  nhyX2(:,:,n) = nint(Tt(1:Jn+1,1:Kn))
  tauhyX2(:,:,n) = Tt(1:Jn+1,1:Kn) - nhyX2(:,:,n)

! Hz
  Tr(1:Jn,1:Kn+1) = dotprod(RXeyhz2,Rhat) / (c0*dt)
  Tt(1:Jn,1:Kn+1) = toffset - Tr(1:Jn,1:Kn+1) + 0.5_rfp
  nhzX2(:,:,n) = nint(Tt(1:Jn,1:Kn+1))
  tauhzX2(:,:,n) = Tt(1:Jn,1:Kn+1) - nhzX2(:,:,n)

! Y1 surface-------------------------------------------------------------------
! Hx
  Tr(1:In+1,1:Kn) = dotprod(RYezhx1,Rhat) / (c0*dt)
  Tt(1:In+1,1:Kn) = toffset - Tr(1:In+1,1:Kn) + 0.5_rfp
  nhxY1(:,:,n) = nint(Tt(1:In+1,1:Kn))
  tauhxY1(:,:,n) = Tt(1:In+1,1:Kn) - nhxY1(:,:,n)

! Hz
  Tr(1:In,1:Kn+1) = dotprod(RYexhz1,Rhat) / (c0*dt)
  Tt(1:In,1:Kn+1) = toffset - Tr(1:In,1:Kn+1) + 0.5_rfp
  nhzY1(:,:,n) = nint(Tt(1:In,1:Kn+1))
  tauhzY1(:,:,n) = Tt(1:In,1:Kn+1) - nhzY1(:,:,n)

! Y2 surface-------------------------
! Hx
  Tr(1:In+1,1:Kn) = dotprod(RYezhx2,Rhat) / (c0*dt)
  Tt(1:In+1,1:Kn) = toffset - Tr(1:In+1,1:Kn) + 0.5_rfp 
  nhxY2(:,:,n) = nint(Tt(1:In+1,1:Kn))
  tauhxY2(:,:,n) = Tt(1:In+1,1:Kn) - nhxY2(:,:,n)

! Hz
  Tr(1:In,1:Kn+1) = dotprod(RYexhz2,Rhat) / (c0*dt)
  Tt(1:In,1:Kn+1) = toffset - Tr(1:In,1:Kn+1) + 0.5_rfp
  nhzY2(:,:,n) = nint(Tt(1:In,1:Kn+1))
  tauhzY2(:,:,n) = Tt(1:In,1:Kn+1) - nhzY2(:,:,n)

! Z1 surface-------------------------------------------------------------------

! Hy

  Tr(1:In,1:Jn+1) = dotprod(RZexhy1,Rhat) / (c0*dt)
  Tt(1:In,1:Jn+1) = toffset - Tr(1:In,1:Jn+1) + 0.5_rfp
  nhyZ1(:,:,n) = nint(Tt(1:In,1:Jn+1))
  tauhyZ1(:,:,n) = Tt(1:In,1:Jn+1) - nhyZ1(:,:,n)

! Hx
  Tr(1:In+1,1:Jn) = dotprod(RZeyhx1,Rhat) / (c0*dt)
  Tt(1:In+1,1:Jn) = toffset - Tr(1:In+1,1:Jn) + 0.5_rfp
  nhxZ1(:,:,n) = nint(Tt(1:In+1,1:Jn))
  tauhxZ1(:,:,n) = Tt(1:In+1,1:Jn) - nhxZ1(:,:,n)

! Z2 surface-------------------------
! Hy
  Tr(1:In,1:Jn+1) = dotprod(RZexhy2,Rhat) / (c0*dt)
  Tt(1:In,1:Jn+1) = toffset - Tr(1:In,1:Jn+1) + 0.5_rfp
  nhyZ2(:,:,n) = nint(Tt(1:In,1:Jn+1))
  tauhyZ2(:,:,n) = Tt(1:In,1:Jn+1) - nhyZ2(:,:,n)

! Hx
  Tr(1:In+1,1:Jn) = dotprod(RZeyhx2,Rhat) / (c0*dt)
  Tt(1:In+1,1:Jn) = toffset - Tr(1:In+1,1:Jn) + 0.5_rfp
  nhxZ2(:,:,n) = nint(Tt(1:In+1,1:Jn))
  tauhxZ2(:,:,n) = Tt(1:In+1,1:Jn) - nhxZ2(:,:,n)

! The times are calculated at the H fields positions but they are used to
! extract the E fields.

! E FIELDS:
!---------
! X1 surface-------------------------------------------------------------------
! Ey
  Tr(1:Jn,1:Kn+1) = dotprod(RXhzey1,Rhat) / (c0*dt)
  Tt(1:Jn,1:Kn+1) = toffset - Tr(1:Jn,1:Kn+1)
  neyX1(:,:,n) = nint(Tt(1:Jn,1:Kn+1))
  taueyX1(:,:,n) = Tt(1:Jn,1:Kn+1) - neyX1(:,:,n)

! Ez
  Tr(1:Jn+1,1:Kn) = dotprod(RXhyez1,Rhat) / (c0*dt)
  Tt(1:Jn+1,1:Kn) = toffset - Tr(1:Jn+1,1:Kn)
  nezX1(:,:,n) = nint(Tt(1:Jn+1,1:Kn))
  tauezX1(:,:,n) = Tt(1:Jn+1,1:Kn) - nezX1(:,:,n)

! X2 surface-------------------------
! Ey
  Tr(1:Jn,1:Kn+1) = dotprod(RXhzey2,Rhat) / (c0*dt)
  Tt(1:Jn,1:Kn+1) = toffset - Tr(1:Jn,1:Kn+1)
  neyX2(:,:,n) = nint(Tt(1:Jn,1:Kn+1))
  taueyX2(:,:,n) = Tt(1:Jn,1:Kn+1) - neyX2(:,:,n)

! Ez
  Tr(1:Jn+1,1:Kn) = dotprod(RXhyez2,Rhat) / (c0*dt)
  Tt(1:Jn+1,1:Kn) = toffset - Tr(1:Jn+1,1:Kn)
  nezX2(:,:,n) = nint(Tt(1:Jn+1,1:Kn))
  tauezX2(:,:,n) = Tt(1:Jn+1,1:Kn) - nezX2(:,:,n)

! Y1 surface-------------------------------------------------------------------
! Ex
  Tr(1:In,1:Kn+1) = dotprod(RYhzex1,Rhat) / (c0*dt)
  Tt(1:In,1:Kn+1) = toffset - Tr(1:In,1:Kn+1)
  nexY1(:,:,n) = nint(Tt(1:In,1:Kn+1))
  tauexY1(:,:,n) = Tt(1:In,1:Kn+1) - nexY1(:,:,n)

! Ez
  Tr(1:In+1,1:Kn) = dotprod(RYhxez1,Rhat) / (c0*dt)
  Tt(1:In+1,1:Kn) = toffset - Tr(1:In+1,1:Kn)
  nezY1(:,:,n) = nint(Tt(1:In+1,1:Kn))
  tauezY1(:,:,n) = Tt(1:In+1,1:Kn) - nezY1(:,:,n)

! Y2 surface-------------------------
! Ex
  Tr(1:In,1:Kn+1) = dotprod(RYhzex2,Rhat) / (c0*dt)
  Tt(1:In,1:Kn+1) = toffset - Tr(1:In,1:Kn+1)
  nexY2(:,:,n) = nint(Tt(1:In,1:Kn+1))
  tauexY2(:,:,n) = Tt(1:In,1:Kn+1) - nexY2(:,:,n)

! Ez
  Tr(1:In+1,1:Kn) = dotprod(RYhxez2,Rhat) / (c0*dt)
  Tt(1:In+1,1:Kn) = toffset - Tr(1:In+1,1:Kn)
  nezY2(:,:,n) = nint(Tt(1:In+1,1:Kn))
  tauezY2(:,:,n) = Tt(1:In+1,1:Kn) - nezY2(:,:,n)

! Z1 surface-------------------------------------------------------------------
! Ey
  Tr(1:In+1,1:Jn) = dotprod(RZhxey1,Rhat) / (c0*dt)
  Tt(1:In+1,1:Jn) = toffset - Tr(1:In+1,1:Jn)
  neyZ1(:,:,n) = nint(Tt(1:In+1,1:Jn))
  taueyZ1(:,:,n) = Tt(1:In+1,1:Jn) - neyZ1(:,:,n)

! Ex
  Tr(1:In,1:Jn+1) = dotprod(RZhyex1,Rhat) / (c0*dt)
  Tt(1:In,1:Jn+1) = toffset - Tr(1:In,1:Jn+1)
  nexZ1(:,:,n) = nint(Tt(1:In,1:Jn+1))
  tauexZ1(:,:,n) = Tt(1:In,1:Jn+1) - nexZ1(:,:,n)

! Z2 surface-------------------------
! Ey
  Tr(1:In+1,1:Jn) = dotprod(RZhxey2,Rhat) / (c0*dt)
  Tt(1:In+1,1:Jn) = toffset - Tr(1:In+1,1:Jn)
  neyZ2(:,:,n) = nint(Tt(1:In+1,1:Jn))
  taueyZ2(:,:,n) = Tt(1:In+1,1:Jn) - neyZ2(:,:,n)

! Ex
  Tr(1:In,1:Jn+1) = dotprod(RZhyex2,Rhat) / (c0*dt)
  Tt(1:In,1:Jn+1) = toffset - Tr(1:In,1:Jn+1)
  nexZ2(:,:,n) = nint(Tt(1:In,1:Jn+1))
  tauexZ2(:,:,n) = Tt(1:In,1:Jn+1) - nexZ2(:,:,n)

end do   ! Angle loop----------------------------------------------------------

dSX = dy*dz       ! Surface patches
dSY = dx*dz
dSZ = dx*dy

! Make sure vectors have even number of time elements.
! This is necessary for the FFT routine in NFT_Print.
if (mod(nts + delay,2) /= 0) then
  delay = delay + 1
end if

!----------------------------------------------

deallocate(Tr, STAT=allocstat)
call check_deallocate(allocstat, 'Tr', 1)
deallocate(Tt, STAT=allocstat)
call check_deallocate(allocstat, 'Tt', 1)
deallocate(Angle, STAT=allocstat)
call check_deallocate(allocstat, 'Angle', 1)
deallocate(thv, phiv, STAT=allocstat)
call check_deallocate(allocstat, 'thv, phiv', 1)

!----------------------------------------------

allocate(Ein(nts+delay), STAT=allocstat)
call check_allocate(allocstat, 'Ein', 3)
allocate(Fth(Nangle, nts+delay), Fphi(Nangle, nts+delay),                     &
         Fth_tau(Nangle, nts+delay), Fphi_tau(Nangle, nts+delay),             &
         STAT=allocstat)
call check_allocate(allocstat, 'Fth, Fphi, Fth_tau, Fphi_tau', 3)
bytes = bytes + Nangle*(nts+delay)*4*bytes_per_float
write(*,*) 'Arrays allocated in NFT_mod, bytes used = ', bytes
bytes_allocated = bytes_allocated + bytes

Ein      = 0.0_rfp
Fth      = 0.0_rfp
Fphi     = 0.0_rfp
Fth_tau  = 0.0_rfp
Fphi_tau = 0.0_rfp

END SUBROUTINE NFT_Init

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       NFT_Store - Calculates the far field arrays
!
! DESCRIPTION
!       Calculates the vector potential arrays for the far field by
!       integrating the surface currents on a surface enclosing the object.
!       Called each time step.
!
! SYNOPSIS
!       call NFT_Store(Ex,Ey,Ez,Hx,Hy,Hz,t,ts)
!       real(kind=rfp), intent(in),
!         dimension(xstart:xstop+1,ystart:ystop+1,zstart:zstop+1) :: Ex, Ey, Ez
!       real(kind=rfp), intent(in),
!         dimension(xstart:xstop,ystart:ystop,zstart:zstop)       :: Hx, Hy, Hz
!       real(kind=rfp), intent(in)                                :: t
!       integer, intent(in)                                       :: ts
!
! ERRORS
!       No error handling
!
! SEE ALSO
!       NFT_Init, NFT_Print
!
! HISTORY
!       Written by Åke Rydell
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE NFT_Store(Ex,Ey,Ez,Hx,Hy,Hz,t,ts)

USE excite_mod,  ONLY : excitation
USE huygens_mod, ONLY : Huy_param, HuyPulseType

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

real(kind=rfp), intent(in),                                                   &
     dimension(xstart:xstop+1,ystart:ystop+1,zstart:zstop+1) :: Ex, Ey, Ez
real(kind=rfp), intent(in),                                                   &
     dimension(xstart:xstop,ystart:ystop,zstart:zstop)       :: Hx, Hy, Hz
real(kind=rfp), intent(in)                                   :: t
integer, intent(in)                                          :: ts

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

real(kind=rfp) :: ehtmp
integer        :: m, i, j, k, n, nt

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

nt = ts    ! Time step

#ifndef SPEC_CPU
!$OMP PARALLEL DO PRIVATE(i,j,k,m,n,ehtmp)
#endif
do n = 1,Nangle     ! Loop over directions, Nangle is often a small number

! Loops over surfaces

! X surfaces:******************************************************************

  do j=Y1,Yn
    do k=Z1,Zn-1
!  Ez, Hy on X1
      m = nt + nezX1(j-Y1+1,k-Z1+1,n)
      ehtmp = Hy(X1-1,j,k)*ezth(n)*dSX
      Fth(n,m)     = Fth(n,m)     - ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) - ehtmp*tauezX1(j-Y1+1,k-Z1+1,n)

      !!  ezphi = 0

      m = nt + nhyX1(j-Y1+1,k-Z1+1,n)
      ehtmp = Ez(X1,j,k)*hyth(n)*dSX
      Fth(n,m)     = Fth(n,m)     - ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) - ehtmp*tauhyX1(j-Y1+1,k-Z1+1,n)

      ehtmp = Ez(X1,j,k)*hyphi(n)*dSX
      Fphi(n,m)     = Fphi(n,m)     - ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) - ehtmp*tauhyX1(j-Y1+1,k-Z1+1,n)
!  Ez, Hy on X2
      m = nt + nezX2(j-Y1+1,k-Z1+1,n)
      ehtmp = Hy(Xn,j,k)*ezth(n)*dSX
      Fth(n,m)     = Fth(n,m)     + ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) + ehtmp*tauezX2(j-Y1+1,k-Z1+1,n)

      !!  ezphi = 0

      m = nt + nhyX2(j-Y1+1,k-Z1+1,n)
      ehtmp = Ez(Xn,j,k)*hyth(n)*dSX
      Fth(n,m)     = Fth(n,m)     + ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) + ehtmp*tauhyX2(j-Y1+1,k-Z1+1,n)

      ehtmp = Ez(Xn,j,k)*hyphi(n)*dSX
      Fphi(n,m)     = Fphi(n,m)     + ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) + ehtmp*tauhyX2(j-Y1+1,k-Z1+1,n)
    end do     ! End 1st X loop
  end do

  do j=Y1,Yn-1
    do k=Z1,Zn
!  Ey, Hz on X1
      m = nt + neyX1(j-Y1+1,k-Z1+1,n)
      ehtmp = Hz(X1-1,j,k)*eyth(n)*dSX
      Fth(n,m)     = Fth(n,m)     + ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) + ehtmp*taueyX1(j-Y1+1,k-Z1+1,n)

      ehtmp = Hz(X1-1,j,k)*eyphi(n)*dSX
      Fphi(n,m)     = Fphi(n,m)     + ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) + ehtmp*taueyX1(j-Y1+1,k-Z1+1,n)

      m = nt + nhzX1(j-Y1+1,k-Z1+1,n)
      !!   hzth = 0

      ehtmp = Ey(X1,j,k)*hzphi(n)*dSX
      Fphi(n,m)     = Fphi(n,m)     + ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) + ehtmp*tauhzX1(j-Y1+1,k-Z1+1,n)
!  Ey, Hz on X2
      m = nt + neyX2(j-Y1+1,k-Z1+1,n)
      ehtmp = Hz(Xn,j,k)*eyth(n)*dSX
      Fth(n,m)     = Fth(n,m)     - ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) - ehtmp*taueyX2(j-Y1+1,k-Z1+1,n)

      ehtmp = Hz(Xn,j,k)*eyphi(n)*dSX
      Fphi(n,m)     = Fphi(n,m)     - ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) - ehtmp*taueyX2(j-Y1+1,k-Z1+1,n)

      m = nt + nhzX2(j-Y1+1,k-Z1+1,n)
      !!   hzth = 0

      ehtmp = Ey(Xn,j,k)*hzphi(n)*dSX
      Fphi(n,m)     = Fphi(n,m)     - ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) - ehtmp*tauhzX2(j-Y1+1,k-Z1+1,n)
    end do      ! End 2nd loop over X1, X2
  end do

! Y surfaces:****************************************************

  do i=X1,Xn-1
    do k=Z1,Zn
!  Ex, Hz on Y1
      m = nt + nexY1(i-X1+1,k-Z1+1,n)
      ehtmp = Hz(i,Y1-1,k)*exth(n)*dSY
      Fth(n,m)     = Fth(n,m)     - ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) - ehtmp*tauexY1(i-X1+1,k-Z1+1,n)

      ehtmp = Hz(i,Y1-1,k)*exphi(n)*dSY
      Fphi(n,m)     = Fphi(n,m)     - ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) - ehtmp*tauexY1(i-X1+1,k-Z1+1,n)

      m = nt + nhzY1(i-X1+1,k-Z1+1,n)
      !! hzth = 0

      ehtmp = Ex(i,Y1,k)*hzphi(n)*dSY
      Fphi(n,m)     = Fphi(n,m)     - ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) - ehtmp*tauhzY1(i-X1+1,k-Z1+1,n)
!  Ex, Hz on Y2
      m = nt + nexY2(i-X1+1,k-Z1+1,n)

      ehtmp = Hz(i,Yn,k)*exth(n)*dSY
      Fth(n,m)     = Fth(n,m)     + ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) + ehtmp*tauexY2(i-X1+1,k-Z1+1,n)

      ehtmp = Hz(i,Yn,k)*exphi(n)*dSY
      Fphi(n,m)     = Fphi(n,m)     + ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) + ehtmp*tauexY2(i-X1+1,k-Z1+1,n)

      m = nt + nhzY2(i-X1+1,k-Z1+1,n)
      !! hzth = 0

      ehtmp = Ex(i,Yn,k)*hzphi(n)*dSY
      Fphi(n,m)     = Fphi(n,m)     + ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) + ehtmp*tauhzY2(i-X1+1,k-Z1+1,n)
    end do     ! End 1st Y loop
  end do

  do i=X1,Xn
    do k=Z1,Zn-1
!  Ez, Hx on Y1 
      m = nt + nezY1(i-X1+1,k-Z1+1,n)
      !! ezphi = 0

      ehtmp = Hx(i,Y1-1,k)*ezth(n)*dSY
      Fth(n,m)     = Fth(n,m)     + ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) + ehtmp*tauezY1(i-X1+1,k-Z1+1,n)

      m = nt + nhxY1(i-X1+1,k-Z1+1,n)

      ehtmp = Ez(i,Y1,k)*hxth(n)*dSY
      Fth(n,m)     = Fth(n,m)     + ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) + ehtmp*tauhxY1(i-X1+1,k-Z1+1,n)

      ehtmp = Ez(i,Y1,k)*hxphi(n)*dSY
      Fphi(n,m)     = Fphi(n,m)     + ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) + ehtmp*tauhxY1(i-X1+1,k-Z1+1,n)
!  Ez, Hx on Y2
      m = nt + nezY2(i-X1+1,k-Z1+1,n)
      !! ezphi = 0

      ehtmp = Hx(i,Yn,k)*ezth(n)*dSY
      Fth(n,m)     = Fth(n,m)     - ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) - ehtmp*tauezY2(i-X1+1,k-Z1+1,n)

      m = nt + nhxY2(i-X1+1,k-Z1+1,n)

      ehtmp = Ez(i,Yn,k)*hxth(n)*dSY
      Fth(n,m)     = Fth(n,m)     - ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) - ehtmp*tauhxY2(i-X1+1,k-Z1+1,n)

      ehtmp = Ez(i,Yn,k)*hxphi(n)*dSY
      Fphi(n,m)     = Fphi(n,m)     - ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) - ehtmp*tauhxY2(i-X1+1,k-Z1+1,n)
    end do      ! End 2nd loop over Y1, Y2
  end do

! Z surfaces*******************************************************************

  do i=X1,Xn-1
    do j=Y1,Yn
!  Ex, Hy on Z1
      m = nt + nexZ1(i-X1+1,j-Y1+1,n)
      ehtmp = Hy(i,j,Z1-1)*exth(n)*dSZ
      Fth(n,m)     = Fth(n,m)     + ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) + ehtmp*tauexZ1(i-X1+1,j-Y1+1,n)

      ehtmp = Hy(i,j,Z1-1)*exphi(n)*dSZ
      Fphi(n,m)     = Fphi(n,m)     + ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) + ehtmp*tauexZ1(i-X1+1,j-Y1+1,n)

      m = nt + nhyZ1(i-X1+1,j-Y1+1,n)
      ehtmp = Ex(i,j,Z1)*hyth(n)*dSZ
      Fth(n,m)     = Fth(n,m)     + ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) + ehtmp*tauhyZ1(i-X1+1,j-Y1+1,n)

      ehtmp = Ex(i,j,Z1)*hyphi(n)*dSZ
      Fphi(n,m)     = Fphi(n,m)     + ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) + ehtmp*tauhyZ1(i-X1+1,j-Y1+1,n)
!  Ex, Hy on Z2
      m = nt + nexZ2(i-X1+1,j-Y1+1,n)
      ehtmp = Hy(i,j,Zn)*exth(n)*dSZ
      Fth(n,m)     = Fth(n,m)     - ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) - ehtmp*tauexZ2(i-X1+1,j-Y1+1,n)

      ehtmp = Hy(i,j,Zn)*exphi(n)*dSZ
      Fphi(n,m)     = Fphi(n,m)     - ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) - ehtmp*tauexZ2(i-X1+1,j-Y1+1,n)

      m = nt + nhyZ2(i-X1+1,j-Y1+1,n)
      ehtmp = Ex(i,j,Zn)*hyth(n)*dSZ
      Fth(n,m)     = Fth(n,m)     - ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) - ehtmp*tauhyZ2(i-X1+1,j-Y1+1,n)

      ehtmp = Ex(i,j,Zn)*hyphi(n)*dSZ
      Fphi(n,m)     = Fphi(n,m)     - ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) - ehtmp*tauhyZ2(i-X1+1,j-Y1+1,n)
    end do      ! End 1st Z loop
  end do

  do i=X1,Xn
    do j=Y1,Yn-1
!  Ey, Hx on Z1
      m = nt + neyZ1(i-X1+1,j-Y1+1,n)
      ehtmp = Hx(i,j,Z1-1)*eyth(n)*dSZ
      Fth(n,m)     = Fth(n,m)     - ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) - ehtmp*taueyZ1(i-X1+1,j-Y1+1,n)

      ehtmp = Hx(i,j,Z1-1)*eyphi(n)*dSZ
      Fphi(n,m)     = Fphi(n,m)     - ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) - ehtmp*taueyZ1(i-X1+1,j-Y1+1,n)

      m = nt + nhxZ1(i-X1+1,j-Y1+1,n)
      ehtmp = Ey(i,j,Z1)*hxth(n)*dSZ
      Fth(n,m)     = Fth(n,m)     - ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) - ehtmp*tauhxZ1(i-X1+1,j-Y1+1,n)

      ehtmp = Ey(i,j,Z1)*hxphi(n)*dSZ
      Fphi(n,m)     = Fphi(n,m)     - ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) - ehtmp*tauhxZ1(i-X1+1,j-Y1+1,n)

!  Ey, Hx on Z2
      m = nt + neyZ2(i-X1+1,j-Y1+1,n)
      ehtmp = Hx(i,j,Zn)*eyth(n)*dSZ
      Fth(n,m)     = Fth(n,m)     + ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) + ehtmp*taueyZ2(i-X1+1,j-Y1+1,n)

      ehtmp = Hx(i,j,Zn)*eyphi(n)*dSZ
      Fphi(n,m)     = Fphi(n,m)     + ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) + ehtmp*taueyZ2(i-X1+1,j-Y1+1,n)

      m = nt + nhxZ2(i-X1+1,j-Y1+1,n)
      ehtmp = Ey(i,j,Zn)*hxth(n)*dSZ
      Fth(n,m)     = Fth(n,m)     + ehtmp
      Fth_tau(n,m) = Fth_tau(n,m) + ehtmp*tauhxZ2(i-X1+1,j-Y1+1,n)

      ehtmp = Ey(i,j,Zn)*hxphi(n)*dSZ
      Fphi(n,m)     = Fphi(n,m)     + ehtmp
      Fphi_tau(n,m) = Fphi_tau(n,m) + ehtmp*tauhxZ2(i-X1+1,j-Y1+1,n)
    end do      ! End 2nd loop over Z1, Z2
  end do
end do   ! Angle loop
#ifndef SPEC_CPU
!$OMP END PARALLEL DO
#endif

! Store incident pulse 
if (HuyPulseType/=0) then
  Ein(nt+1) = excitation(t, Huy_param, HuyPulseType)
else 
  if (nt==1) then
    write(*,*) 'NOTE! No value for Ein available in NFT since '
    write(*,*) 'Huygens is not used as excitation. (Ein==1 is used.)'
  end if
  Ein(nt+1) = 1.0_rfp
end if

END SUBROUTINE NFT_Store

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       NFT_Print - Print results to file
!
! DESCRIPTION
!       Interpolates the far field arrays, calculates the E field and
!       prints the results to file. Called after timestepping.
!
! SYNOPSIS
!       call NFT_Print
!
! ERRORS
!       Uses errorcheck module
!
! SEE ALSO
!       NFT_Init, NFT_Store
!
! HISTORY
!       Written by Åke Rydell
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE NFT_Print

USE errorcheck_mod, ONLY : check_open, check_close, check_write,              &
                           check_allocate, check_deallocate

!------------------------------------------------------------------------------
!                        L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer                                   :: i, n, ios, nf, allocstat
real(kind=rfp), dimension(:), allocatable :: freq          ! Freq. vector
real(kind=rfp)                            :: fstep         ! Freq. step
complex(kind=kfp)                         :: jj, Fthtot_f, Fphitot_f
!complex(kind=kfp)                         :: etmp
character(len=80)                         :: FileNFTOut    ! Output filename

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

! Deallocate memory:

deallocate(RXeyhz1,RXeyhz2,RXezhy1,RXezhy2, STAT=allocstat)
call check_deallocate(allocstat,'RXeyhz1,RXeyhz2,RXezhy1,RXezhy2', 1)
deallocate(RXhzey1,RXhzey2,RXhyez1,RXhyez2, STAT=allocstat)
call check_deallocate(allocstat,'RXhzey1,RXhzey2,RXhyez1,RXhyez2', 1)
deallocate(RYexhz1,RYexhz2,RYezhx1,RYezhx2, STAT=allocstat)
call check_deallocate(allocstat,'RYexhz1,RYexhz2,RYezhx1,RYezhx2', 1)
deallocate(RYhzex1,RYhzex2,RYhxez1,RYhxez2, STAT=allocstat)
call check_deallocate(allocstat,'RYhzex1,RYhzex2,RYhxez1,RYhxez2', 1)
deallocate(RZexhy1,RZexhy2,RZeyhx1,RZeyhx2, STAT=allocstat)
call check_deallocate(allocstat,'RZexhy1,RZexhy2,RZeyhx1,RZeyhx2', 1)
deallocate(RZhyex1,RZhyex2,RZhxey1,RZhxey2, STAT=allocstat)
call check_deallocate(allocstat,'RZhyex1,RZhyex2,RZhxey1,RZhxey2', 1)

deallocate(nexY1,nezY1,nexY2,nezY2,neyX1,nezX1,neyX2,nezX2,neyZ1,             &
           nexZ1,neyZ2,nexZ2, STAT=allocstat)
call check_deallocate(allocstat, 'nexY1,...', 1)
deallocate(nhxY1,nhzY1,nhxY2,nhzY2,nhyX1,nhzX1,nhyX2,nhzX2,nhyZ1,             &
           nhxZ1,nhyZ2,nhxZ2, STAT=allocstat)
call check_deallocate(allocstat, 'nhxY1,... ', 1)

deallocate(tauexY1,tauezY1,tauexY2,tauezY2,taueyX1,tauezX1,taueyX2,           &
           tauezX2,taueyZ1,tauexZ1,taueyZ2,tauexZ2, STAT=allocstat)
call check_deallocate(allocstat, 'tauexY1,...', 1)
deallocate(tauhxY1,tauhzY1,tauhxY2,tauhzY2,tauhyX1,tauhzX1,tauhyX2,           &
           tauhzX2,tauhyZ1,tauhxZ1,tauhyZ2,tauhxZ2, STAT=allocstat)
call check_deallocate(allocstat, 'tauhxY1,...', 1)

deallocate(exth, eyth, ezth, hxth, hyth, exphi, eyphi,                        &
           hxphi, hyphi, hzphi, STAT=allocstat)
call check_deallocate(allocstat, 'exth, ...', 1)

!------------------------------------------------------------------------------

nf = (nts+delay)/2 + 1               ! No. of freq. samples
fstep = 1.0_rfp/((nts+delay)*dt)     ! Frequency step

jj = (0.0_rfp, 1.0_rfp)      ! Imaginary unit

allocate(Eth_f(Nangle, nf), Ephi_f(Nangle, nf), STAT=allocstat)
call check_allocate(allocstat, 'Eth_f, Ephi_f', 3)
allocate(freq(nf), STAT=allocstat)
call check_allocate(allocstat, 'freq', 3)

! Create freq. vector
do i = 1,nf
  freq(i) = (i-1)*fstep
end do

! Fourier transform of Ein, Fth, Fphi, Fth_tau, Fphi_tau to the freq. domain
call dft_call(Ein,Fth,Fphi,Fth_tau,Fphi_tau)

! E field in the frequency domain
do n = 1,Nangle
  do i = 1, nf
    Fthtot_f   = Fth_f(n,i) - 2.0_rfp*pi*jj*dt*freq(i)*Fth_tau_f(n,i)
    Eth_f(n,i) = - mu0*jj*freq(i)*0.5_rfp*Fthtot_f

    Fphitot_f   = Fphi_f(n,i) - 2.0_rfp*pi*jj*dt*freq(i)*Fphi_tau_f(n,i)
    Ephi_f(n,i) = - mu0*jj*freq(i)*0.5_rfp*Fphitot_f
  end do
end do

! Inverse Fourier transform of E fields to time domain
call idft_call(Eth_f,Ephi_f)

! Print results to file:-------------------------------------------------------

FileNFTOut = trim(adjustL(Outfilenamebase))//'.nft'

open(unit=9,file=FileNFTOut, iostat = ios)
call check_open(ios, FileNFTOut, 3)

write(9,'(4I5)', iostat = ios) nth, nphi, nf, nts+delay
call check_write(ios, FileNFTOut, 3)

write(9,'(6E15.6)', iostat = ios) th1, th2, phi1, phi2, toffset, dt
call check_write(ios, FileNFTOut, 3)

! Incoming E field in freq. domain:
do i = 1,size(Ein_f)
  write(9,'(2E20.12)', iostat = ios) Ein_f(i)
  call check_write(ios, FileNFTOut, 3)
end do

! E fields in freq. domain:
do n = 1,Nangle
  do i = 1,size(Eth_f,2)
    write(9,'(4E20.12)', iostat = ios) Eth_f(n,i), Ephi_f(n,i)
    call check_write(ios, FileNFTOut, 3)
  end do
end do

! E fields in time domain:
do n = 1,Nangle
  do i = 1,size(Eth,2)
    write(9,'(2E20.12)', iostat = ios) Eth(n,i), Ephi(n,i)
    call check_write(ios, FileNFTOut, 3)
  end do
end do

! Close output file:
close(9, iostat = ios)
call check_close(ios, FileNFTOut, 1)

!------------------------------------------------------------------------------

deallocate(Ein, STAT=allocstat)
call check_deallocate(allocstat, 'Ein', 1)
deallocate(Fth, Fphi, Fth_tau, Fphi_tau, STAT=allocstat)
call check_deallocate(allocstat, 'Fth, Fphi, Fth_tau, Fphi_tau', 1)
deallocate(Eth, STAT=allocstat)
call check_deallocate(allocstat, 'Eth', 1)
deallocate(Ephi, STAT=allocstat)
call check_deallocate(allocstat, 'Ephi', 1)
deallocate(Ein_f, STAT=allocstat)
call check_deallocate(allocstat, 'Ein_f', 1)
deallocate(Fth_f, STAT=allocstat)
call check_deallocate(allocstat, 'Fth_f', 1)
deallocate(Fphi_f, STAT=allocstat)
call check_deallocate(allocstat, 'Fphi_f', 1)
deallocate(Fth_tau_f, STAT=allocstat)
call check_deallocate(allocstat, 'Fth_tau_f', 1)
deallocate(Fphi_tau_f, STAT=allocstat)
call check_deallocate(allocstat, 'Fphi_tau_f', 1)
deallocate(Eth_f, Ephi_f, STAT=allocstat)
call check_deallocate(allocstat, 'Eth_f, Ephi_f', 1)
deallocate(freq, STAT=allocstat)
call check_deallocate(allocstat, 'freq', 1)

END SUBROUTINE NFT_Print

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       NF_get_Nangle_and_NFT_db - returns Nangle and NFT_db 
!
! DESCRIPTION
!      Returns the value of PRIVATE variables Nangle and NFT_db.
!     
! SYNOPSIS
!       CALL NFF_get_Nangle_and_NFT_db(loc_Nangle,loc_NFT_db)
!         integer, intent(out) :: loc_Nangle, loc_NFT_db
!
! SEE ALSO
!       SUBROUTINE calcflops in module calcflops_mod in file calcflops.f90
!
! HISTORY
!       Written by Ulf Andersson 2002-01-23
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE NFT_get_Nangle_and_NFT_db(loc_Nangle,loc_NFT_db)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(out) :: loc_Nangle, loc_NFT_db

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

loc_Nangle = Nangle
loc_NFT_db = NFT_db

END SUBROUTINE NFT_get_Nangle_and_NFT_db

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE NFT_mod
