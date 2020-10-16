!+
! NAME
!       parameter_mod - Global parameters
!
! DESCRIPTION
!       Module for global parameters
!
!       Global variables can be found in the module globalvar_mod
!
! PUBLIC
!       real(kind=rfp), parameter :: pi = 3.141592653589793_rfp
!       real(kind=rfp), parameter :: eps0 = 8.8541878E-12_rfp  
!       real(kind=rfp), parameter :: mu0  = 1.256637061E-6_rfp 
!
!       complex(kind=kfp), parameter :: jj=(0.0_rfp,1.0_rfp)   
!
!       real(kind=rfp), parameter :: zero = 0.0_rfp
!       real(kind=rfp), parameter :: half = 0.5_rfp
!       real(kind=rfp), parameter :: one  = 1.0_rfp
!       real(kind=rfp), parameter :: two  = 2.0_rfp
!       real(kind=rfp), parameter :: four  = 4.0_rfp
!       real(kind=rfp), parameter :: minus_one  = -1.0_rfp
!
! SEE ALSO
!       globalvar_mod
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE parameter_mod

IMPLICIT NONE

! Floating point precision parameters
integer, parameter :: sfp=selected_real_kind(6,37)  ! 32-bit precision
integer, parameter :: dfp=selected_real_kind(13,99) ! 64-bit precision
integer, parameter :: rfp=dfp ! 64-bit precision is used for computations
integer, parameter :: bytes_per_float = 8   
integer, parameter :: kfp=selected_real_kind(13,99) ! 64-bit precision
integer, parameter :: bytes_per_cmplx = 16  
integer, parameter :: one_byte = selected_int_kind(2)   !   One byte integer
integer, parameter :: two_byte = selected_int_kind(3)   !   Two byte integer

! Physical constants
real(kind=rfp), parameter :: eps0 = 8.8541878E-12_rfp  ! permittivity in vaccum
real(kind=rfp), parameter :: mu0  = 1.256637061E-6_rfp ! permeability in vaccum

! Mathematical constants
real(kind=rfp), parameter :: pi = 3.141592653589793_rfp
complex(kind=kfp), parameter :: jj=(0.0_rfp,1.0_rfp)   ! sqrt(-1)

! Short names for some common real values 
real(kind=rfp), parameter :: zero = 0.0_rfp
real(kind=rfp), parameter :: half = 0.5_rfp
real(kind=rfp), parameter :: one  = 1.0_rfp
real(kind=rfp), parameter :: two  = 2.0_rfp
real(kind=rfp), parameter :: four  = 4.0_rfp
real(kind=rfp), parameter :: minus_one  = -1.0_rfp

END MODULE parameter_mod
