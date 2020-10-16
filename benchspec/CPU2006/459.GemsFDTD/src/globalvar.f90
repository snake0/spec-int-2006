!+
! NAME
!       globalvar_mod - Global variables..
!
! DESCRIPTION
!
!       This module is intended to contain global variables.
!       They cannot be parameters since they depend on input data,
!       but they are not supposed to change after they have been initialized.
!
! PUBLIC 
!       SUBROUTINE glo_init
!
!       integer, parameter :: NDIM = 3
!       real(kind=rfp) :: dx, dy, dz
!       integer        :: nts
!       real(kind=rfp) :: dxinv, dyinv, dzinv 
!       real(kind=rfp) :: dt, dtdmu, dtdeps
!       integer        :: xstart, ystart, zstart, xstop, ystop, zstop
!       real(kind=rfp) :: c0, Z0 
!       integer        :: OBC_Type 
!       integer, parameter :: OBC_MUR, OBC_PML, OBC_PEC, OBC_PMC, OBC_UPML
!       integer        :: NF_type 
!       character(80)  :: Aux
!       integer :: pec_gp_zindex
!       integer :: bytes_allocated
! 
! SEE ALSO
!       parameter_mod
!
! HISTORY
!       Version       Date                 Name
!       Comments
!       -------------------------------------------
!	$Log: globalvar.f90,v $
!	Revision 1.2  2003/09/26 20:15:18  ulfa
!	Added global computation of #bytes allocated.
!	
!	Revision 1.1  2003/09/23 14:06:51  ulfa
!	Initial version.
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE globalvar_mod

USE parameter_mod, ONLY : rfp, kfp, mu0, eps0, one

IMPLICIT NONE

PRIVATE
PUBLIC :: glo_init

PUBLIC :: NDIM, dx, dy, dz, nts, dxinv, dyinv, dzinv, dt, dtdmu, dtdeps
PUBLIC :: xstart, ystart, zstart, xstop, ystop, zstop
PUBLIC :: Z0, c0, OBC_Type, OBC_MUR, OBC_PML, OBC_PEC, OBC_PMC, OBC_UPML
PUBLIC :: NF_Type, Aux, pec_gp_zindex, bytes_allocated

!==============================================================================
! The number of dimensions
!==============================================================================
integer, parameter :: NDIM = 3
!==============================================================================
! Variables given in yee.dat
!==============================================================================
real(kind=rfp) :: dx = 1.0_rfp    ! spatial discretization in X
real(kind=rfp) :: dy = 1.0_rfp    ! spatial discretization in Y
real(kind=rfp) :: dz = 1.0_rfp    ! spatial discretization in Z
integer        :: nts = 10        ! number of timesteps
!==============================================================================
! Variables calculated from variables given in yee.dat
!==============================================================================
real(kind=rfp) :: dxinv, dyinv, dzinv   ! inverse of spatial discretization
real(kind=rfp) :: dt, dtdmu, dtdeps
integer        :: xstart, ystart, zstart, xstop, ystop, zstop
!==============================================================================
! Variables calculated from parameters in parameter
!==============================================================================
real(kind=rfp) :: c0
real(kind=rfp) :: Z0
!==============================================================================
! Variable to determine what type of boundary condition is in use
!==============================================================================
integer        :: OBC_Type
!==============================================================================
! Parameters for boundary condition types
!==============================================================================
INTEGER, PARAMETER :: OBC_MUR=-1, OBC_PML=-2, OBC_PEC=-3, OBC_PMC=-4
INTEGER, PARAMETER :: OBC_UPML=-5
                              ! only UPML is included in GemsFDTD
!==============================================================================
! Variable to determine which near-to-far-field transform to use
!==============================================================================
integer        :: NF_Type = 0 ! 0=>none 1=>FD 2=>TD 3=>CW
                              ! only TD is included in GemsFDTD
!==============================================================================
! Variable to be used when putting temporary fixes in the code.
!==============================================================================
character(80)  :: Aux
!==============================================================================
! Location (z index) and type of PEC ground plane. Not included in GemsFDTD
!==============================================================================
integer :: pec_gp_zindex = 0 ! 0 means no PEC groundplane.
!==============================================================================
! Variable to hold the total size of allocated arrays.
!==============================================================================
integer :: bytes_allocated = 0

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       glo_init - Global variables initialization
!
! DESCRIPTION
!       Global variables initialization
!
! SYNOPSIS
!       SUBROUTINE glo_init(nx,ny,nz,pml_cells,CFL)
!         integer, intent(in) :: nx, ny, nz, pml_cells
!         real(kind=rfp), intent(in) :: CFL
!
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE glo_init(nx,ny,nz,pml_cells,CFL)

!------------------------------------------------------------------------------
!                     A r g u m e n t s
!------------------------------------------------------------------------------

integer, intent(in) :: nx, ny, nz, pml_cells
real(kind=rfp), intent(in) :: CFL

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

dxinv = 1.0_rfp/dx 
dyinv = 1.0_rfp/dy 
dzinv = 1.0_rfp/dz 
dt = CFL / (c0*sqrt( dxinv**2+dyinv**2+dzinv**2))
write(*,*) 'dt*c0 = ', dt*c0 
write(*,'(a,f5.2)')                                                           &
     ' Number of timesteps to propagate one (x)-cell: ', one/(dt*c0*dxinv)
dtdmu  = dt/mu0
dtdeps = dt/eps0

! Sizes of field arrays when PML is used 
xstart = -pml_cells+1
ystart = -pml_cells+1
zstart = -pml_cells+1
xstop  = nx+pml_cells
ystop  = ny+pml_cells
zstop  = nz+pml_cells

END SUBROUTINE glo_init

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE globalvar_mod
