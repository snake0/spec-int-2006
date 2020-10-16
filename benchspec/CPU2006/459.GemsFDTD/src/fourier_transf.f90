!+
! NAME
!       fourier_transf - This module is used for Fourier Transforms
!
! DESCRIPTION
!       This module contains routines to perform both
!       "Analysis" of real (time) data, giving a conjugate even result and
!       "Synthesis" of conjugate even (freq) data, giving a real (time) result.
!
!        The transforms are done on an m1 x m2 x nc array in either of the
!        directions m1 or m2. Hence, sampled time history on a m2 x nc array
!        with m1 time samples can be performed in parallel and with only one
!        call to the main routines, 'fourier_init' for initialization,
!        'ft' for the Fourier transform, and 'ftterm' for deallocations.
!
!        NOTE. The routines require an even number for the transform length.
!        However, the transform length does not have to be a power-of-two.
!
!        The routines are written as good as it gets in terms of performance.
!        The kernel of the code, the last routine (fft), is written to 
!        maintain decent vector lengths.
!
!        The memory requirements are roughly four times the size of the transf.
!        variable, and hence not an in-situ implementation. On the other
!        hand, the results do not come out in bit reversed order.
!        The memory requirements can be lowered if the variable is multi-
!        dimensional. i.e. fourier(X(1:nts,:,:)) => 
!                          do i,j = : fourier(X(1:nts,i,j))
!
! PUBLIC
!       By USEing this module the types rft_tab_type, trf_tab_type, 
!       trf_data_type and trf_work_type are made public
!
!       SUBROUTINE  fourier_init
!       SUBROUTINE  fourier_end
!       SUBROUTINE  fourier
!
!       
! SYNOPSIS
!       A typical sequence of declarations and calls is
!
!        type fac1_type
!          type(trf_tab_type)  :: trf_tab
!          type(trf_data_type) :: trf_data
!          type(trf_work_type) :: trf_work
!        end type fac1_type
!        
!        type(fac1_type) :: fac1
!        character :: dir,ans
!        integer :: m1,m2,nc,m,n,n_trf,info
!        real(kind=rfp), dimension(:,:,:), allocatable :: X
!
!
!        allocate(X(1:m1,1:m2,1:nc))  ! real array
!        X = ...
!
!        call fourier_init(m,n_trf,fac1%trf_tab, & ! Initialization, ie 
!                          fac1%trf_data,        & ! allocations and
!                          fac1%trf_work,info)     ! comput. of tables
!
!        call fourier('A','1',fac1%trf_tab,X,    & ! Analysis in 1st dim of X
!                        fac1%trf_data,          & ! in : X
!                        fac1%trf_work,info)     & ! out: fac1%trf_data%Y_C
!
!        call fourier('S','1',fac1%trf_tab,X,   & ! Synthesis in 1st dim of Y_C
!                        fac1%trf_data,         & ! in : fac1%trf_data%Y_C
!                        fac1%trf_work,info)    & ! out: X
!        
!        call fourier_end(fac1%trf_tab,       & ! Termination
!                         fac1%trf_data,      & ! deallocation of the 
!                         fac1%trf_work,info) & ! data used by the routines
!        
! NOTE
!        If the size of X is (m1 x m2 x nc) and Analysis is performed in the
!        first direction,  the size of fac1%trf_data%Y_C is 
!        ( m1/2+1 x m2 x nc) and vice versa.
!
! ACKNOWLEDGMENTS
!        These routines have kindly been put to GEMS disposal by the 
!        Dr Kurt Otto and Dr Sverker Holmgren at Uppsala University
!
! HISTORY
!       Version       Date                 Name
!       Comments
!       -------------------------------------------
!       $Log: fourier_transf.f90,v $
!       Revision 1.1  2003/09/23 14:06:50  ulfa
!       Initial version.
!
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE fourier_transf_mod

USE parameter_mod, ONLY :  rfp, kfp, pi

IMPLICIT NONE

PRIVATE

!-----------------------------------------------------------------------------
CHARACTER(LEN=75), PRIVATE, SAVE :: RCSID =                                   &
     '$Id: fourier_transf.f90,v 1.1 2003/09/23 14:06:50 ulfa Exp $'
!-----------------------------------------------------------------------------

! Tables for Fast Fourier Transform
type fft_tab_type  
  private
  complex(kind=kfp), dimension(:), pointer :: omega,bar_omega
end type fft_tab_type

! Tables for FRactional Fourier Transform
type frft_tab_type 
  private
  complex(kind=kfp), dimension(:), pointer :: t,bar_t,ft_bar_t,ft_t
  logical :: power_of_two
  type(fft_tab_type) :: fft_tab
end type frft_tab_type

! Tables for Real Fourier Transform
type rft_tab_type  
  private
  complex(kind=kfp), dimension(:), pointer :: r2c,c2r
  type(frft_tab_type) :: frft_tab
end type rft_tab_type

! Transform tables housing the above types
type trf_tab_type  
  real(kind=rfp), dimension(:), pointer :: s
  complex(kind=kfp), dimension(:), pointer :: w,bar_w
  type(rft_tab_type) :: rft_tab
end type trf_tab_type

! This type houses the output/input when Analysis/Synthesis is performed
type trf_data_type  
  logical :: real_trf
  complex(kind=kfp), dimension(:,:), pointer :: Y_C
end type trf_data_type

! This type is only for work-data during computations
type trf_work_type  
  integer :: old,new
  complex(kind=kfp), dimension(:,:,:), pointer :: Z
  real(kind=rfp), dimension(:,:), pointer :: V_R
end type trf_work_type


PUBLIC trf_tab_type, trf_data_type, trf_work_type
PUBLIC rft_tab_type
PUBLIC fourier_init, fourier_end, fourier

CONTAINS

! ----------------------------------------------------------------------
! alog2 computes the base-two logarithm of the integer i.
! ----------------------------------------------------------------------
function alog2(i)

  integer, intent(in) :: i
  real(kind=rfp) :: alog2

  alog2 = log(real(i,rfp))/log(real(2,rfp))

end function alog2

! ----------------------------------------------------------------------
! exchng exchanges the values of the integers i1 and i2.
! ----------------------------------------------------------------------
subroutine exchng(i1,i2)
  integer :: i1,i2
  integer :: temp

    temp = i1; i1 = i2; i2 = temp

end subroutine exchng

! ----------------------------------------------------------------------
! rou computes a table of the roots of unity.
! ----------------------------------------------------------------------
subroutine rou(tab)
  complex(kind=kfp), &              ! The table of size m that                &
  dimension(0:), intent(out) :: tab ! contains exp(i*2*pi*j/m), j=0,m-1
  integer :: m,j
  complex(kind=kfp) :: phase

  m = size(tab,1)
  do j=0,m-1
    phase = cmplx(0.0_rfp,2*pi*j/m,kfp)
    tab(j) = exp(phase)
  end do
end subroutine rou

! ----------------------------------------------------------------------
!  -- LAPACK auxiliary routines (version 1.1) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992
! ----------------------------------------------------------------------
!  lsame returns .true. if ca is the same letter as cb regardless of case.
! ----------------------------------------------------------------------
function lsame(ca,cb)

  logical :: lsame
  character, intent(in) :: ca,cb
  integer :: inta,intb,zcode      ! Local Scalars

  lsame = (ca==cb)  ! Test if the characters are equal

  if (lsame) then
    return
  end if


    zcode = ichar('Z')  ! Now test for equivalence if both are alphabetic.

    ! Use 'Z' rather than 'A' so that ASCII can be detected on Prime
    ! machines, on which ICHAR returns a value with bit 8 set.
    ! ICHAR('A') on Prime machines returns 193 which is the same as
    ! ICHAR('A') on an EBCDIC machine.

    inta = ichar(ca)
    intb = ichar(cb)

    if (zcode==90 .or. zcode==122) then             ! ASCII assumed, ZCODE is
                                                    ! the ASCII code of either
      if (inta>=97 .and. inta<=122) then
        inta = inta-32                              ! lower or upper case 'Z'.
      end if
      if (intb>=97 .and. intb<=122) then
        intb = intb-32  
      end if
                                                    
    else if (zcode==233 .or. zcode==169) then       !  EBCDIC assumed, ZCODE 
                                                    ! is the EBCDIC code of
      if (inta>=129 .and. inta<=137 .or. &          ! either lower or upper   &
          inta>=145 .and. inta<=153 .or. &          ! upper case 'Z'.         &
          inta>=162 .and. inta<=169) then
        inta = inta + 64
      end if
      if (intb>=129 .and. intb<=137 .or.                                      &
          intb>=145 .and. intb<=153 .or.                                      &
          intb>=162 .and. intb<=169) then
        intb = intb + 64
      end if

    else if (zcode==218 .or. zcode==250) then         ! ASCII assumed, on Prime
                                                      ! machines,  ZCODE is the
      if (inta>=225 .and. inta<=250) then
        inta = inta - 32 ! the ASCII code plus 128
      end if
      if (intb>=225 .and. intb<=250) then
        intb = intb - 32                              ! of either lower or
      end if                                          ! upper case 'Z'.

    end if
    lsame = inta==intb
end function lsame

! ----------------------------------------------------------------------
!  lsamen tests if the first n letters of ca are the same as the
!  first n letters of cb, regardless of case.
!  lsamen returns .true. if ca and cb are equivalent except for case
!  and .false. otherwise.  lsamen also returns .false. if len(ca)
!  or len(cb) is less than n.
! ----------------------------------------------------------------------
function lsamen(n,ca,cb)
  logical :: lsamen

  integer, intent(in) :: n  ! number of chars in ca and cb to be compared.
  character(len=n), intent(in) :: ca,cb

  integer :: i
  lsamen = .false.
  if (len(ca)<n .or. len(cb)<n) then
    return
  end if

    do i=1,n  ! Do for each character in the two strings.

      if (.not.lsame(ca(i:i),cb(i:i))) then ! Test if the characters are
        return                              ! equal using lsame.
      end if
    end do

    lsamen = .true.
end function lsamen

! ----------------------------------------------------------------------
!  xerbla is an error handler.
!  It is called by a routine if an input argument has an
!  invalid value. A message is printed and execution stops.
! ----------------------------------------------------------------------
subroutine xerbla(rname,info)

  character(len=6), intent(in) :: rname  ! name of routine which called xerbla
  integer, intent(in) :: info            ! Pos of the invalid argument in the
                                         !  argument list of calling routine.
  character(len=*), parameter :: f9 =                                         &
       "(' ** On entry to ',a6,' argument number ',i2,' had an illegal value')"
    write(*,*) ' FATAL ERROR in module fourier_transf_mod!'
    write(*,f9) rname,info
    stop
end subroutine xerbla

! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
subroutine fourier_init(m,n_trf,trf_tab,trf_data,trf_work,info)

  integer, intent(in) :: m,n_trf
  integer, intent(out) :: info
  type(trf_tab_type), intent(out) :: trf_tab
  type(trf_data_type) :: trf_data
  type(trf_work_type) :: trf_work

    if (mod(m,2)==0) then
      trf_data%real_trf = .false.
      allocate(trf_data%Y_C(1:n_trf,1:m/2+1))
      call rftin(m,n_trf,trf_tab%rft_tab,trf_work%V_R,trf_work%Z,info)
    else
      info = -1
      call xerbla('FTIN  ',-info)
    end if
end subroutine fourier_init

! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
subroutine fourier_end(trf_tab,trf_data,trf_work,info)

  integer, intent(out) :: info
  type(trf_tab_type) :: trf_tab
  type(trf_data_type) :: trf_data
  type(trf_work_type) :: trf_work

    deallocate(trf_data%Y_C)
    call rftterm(trf_tab%rft_tab,trf_work%V_R,trf_work%Z,info)
  end subroutine fourier_end

! ----------------------------------------------------------------------
! This subroutine is the main calling routine that calls the real work-horses,
! frft and fft. frft does also call fft and hence, fft is the true core
! 
! fourier returns a normalized answer both for Analysis and Synthesis, which 
! is not the case for the Matlab function fft which does not normalize at all.
! Matlab ifft however, 'normalizes' with the factor 1/n.
! ----------------------------------------------------------------------
subroutine fourier(mode,dir,trf_tab,X,trf_data,trf_work,info)

  character, intent(in) :: mode,dir
  integer, intent(out) :: info
  real(kind=rfp), dimension(1:,1:,1:) :: X
  type(trf_tab_type), intent(in) :: trf_tab
  type(trf_data_type) :: trf_data
  type(trf_work_type) :: trf_work

  integer :: nc,m1,m2,m,n,row,j,k
  real(kind=rfp) :: fac   ! This is the factorization factor

    m1 = ubound(X,1)
    m2 = ubound(X,2)
    nc = ubound(X,3)

    if (lsame(dir,'1')) then
      m = m1
      n = (m2-1)*nc
    else if (lsame(dir,'2')) then
      m = m2
      n = (m1-1)*nc
    else
      info = -2
      call xerbla('FT    ',-info)
    end if
    fac = sqrt(1.0_rfp/m)   ! factorization factor
    if (lsame(mode,'A')) then
      if (lsame(dir,'1')) then
        do j=1,m1
          do row=1,nc
            trf_work%V_R(row:n+row:nc,j-1) = X(j,:,row)
          end do
        end do
      else if (lsame(dir,'2')) then
        do k=1,m2
          do row=1,nc
            trf_work%V_R(row:n+row:nc,k-1) = X(:,k,row)
          end do
        end do
      end if
      call rft('A',trf_tab%rft_tab,trf_work%V_R,trf_work%old,                 &
               trf_work%new,trf_work%Z,info)
      trf_data%Y_C = fac*trf_work%Z(:,0:m/2,trf_work%new)   ! Note the
    else if (lsame(mode,'S')) then                          ! factorization
      trf_work%old = 0
      trf_work%Z(:,0:m/2,trf_work%old) = trf_data%Y_C
      call rft('S',trf_tab%rft_tab,trf_work%V_R,trf_work%old,                 &
               trf_work%new,trf_work%Z,info)
      if (lsame(dir,'1')) then
        do j=1,m1
          do row=1,nc
            X(j,:,row) = fac*trf_work%V_R(row:n+row:nc,j-1)  ! Note the
          end do                                             ! factorization
        end do
      else if (lsame(dir,'2')) then
        do k=1,m2
          do row=1,nc
            X(:,k,row) = fac*trf_work%V_R(row:n+row:nc,k-1)  ! Note the
          end do                                             ! factorization
        end do
      end if
    else
      info = -1
      call xerbla('FT    ',-info)
    end if
end subroutine fourier

! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
subroutine rftin(m,n_trf,rft_tab,V,Z,info)

  integer, intent(in) :: m,n_trf
  integer, intent(out) :: info
  real(kind=rfp), dimension(:,:), pointer :: V
  complex(kind=kfp), dimension(:,:,:), pointer :: Z
  type(rft_tab_type), intent(out) :: rft_tab

  integer :: j
  complex(kind=kfp) :: phase

    if (mod(m,2)==0) then
      allocate(V(1:n_trf,0:m-1))
      allocate(rft_tab%r2c(0:m/2-1),rft_tab%c2r(0:m/2-1))
      do j=0,m/2-1
        phase = cmplx(0.0_rfp,2*pi*j/m,kfp)
        rft_tab%r2c(j) = (0.0_rfp,1.0_rfp)*exp(-phase)
      end do
      rft_tab%c2r = conjg(rft_tab%r2c)
      call frftin(m/2,1,n_trf,rft_tab%frft_tab,Z,info)
     else
      info = -1
      call xerbla('RFTIN ',-info)
    end if
  end subroutine rftin

subroutine frftin(m,n_trf_lb,n_trf_ub,frft_tab,Z,info)

  integer, intent(in) :: m,n_trf_lb,n_trf_ub
  integer, intent(out) :: info
  complex(kind=kfp), dimension(:,:,:), pointer :: Z
  type(frft_tab_type), intent(out) :: frft_tab

  integer :: mhat,old,new,j
  complex(kind=kfp) :: phase

    mhat = 2**(ceiling(alog2(m))+1)
    if (m==mhat/2) then
      frft_tab%power_of_two = .true.
      call fftin(m,frft_tab%fft_tab,info)
      allocate(Z(n_trf_lb:n_trf_ub,0:m,0:1))
    else
      frft_tab%power_of_two = .false.
      allocate(frft_tab%t(0:m-1),frft_tab%ft_bar_t(0:mhat-1))
      allocate(frft_tab%bar_t(0:m-1),frft_tab%ft_t(0:mhat-1))
      call fftin(mhat,frft_tab%fft_tab,info)
      allocate(Z(1:1,0:mhat-1,0:1))
      do j=0,m-1
        phase = cmplx(0.0_rfp,pi*j*j/m,kfp)
        frft_tab%t(j) = exp(-phase)
      end do
      frft_tab%bar_t = conjg(frft_tab%t)
      frft_tab%ft_t(0:m-1) = frft_tab%t
      frft_tab%ft_t(m:mhat-m) = 0.0_kfp
      frft_tab%ft_t(mhat-1:mhat-m+1:-1) = frft_tab%t(1:m-1)
!      frft_tab%ft_t(mhat-m+1:mhat-1) = frft_tab%t(m-1:1:-1)
      frft_tab%ft_bar_t = conjg(frft_tab%ft_t)
      old = 0
      Z(1,:,old) = frft_tab%ft_bar_t
      call fft('A',frft_tab%fft_tab,old,new,Z,info)
      frft_tab%ft_bar_t = Z(1,:,new)/mhat
      Z(1,:,old) = frft_tab%ft_t
      call fft('A',frft_tab%fft_tab,old,new,Z,info)
      frft_tab%ft_t = Z(1,:,new)/mhat
      deallocate(Z)
      allocate(Z(n_trf_lb:n_trf_ub,0:mhat-1,0:1))
    end if
  end subroutine frftin
!
!
  subroutine fftin(m,fft_tab,info)
!
  integer, intent(in) :: m
  integer, intent(out) :: info
  type(fft_tab_type), intent(out) :: fft_tab
!
! ----------------------------------------------------------------------
!
! Purpose
!
! fftin computes tables of the roots of unity for the fast Fourier
! transform of size m, and the inverse fast Fourier transform,
! where m is a power of two.
!
! Arguments
!
! m       (input) integer
!         The size of the tables
!
! fft_tab (output) type(fft_tab_type)
!         Trigonometric tables for FFT
!
!        %omega  complex array, dimension(0:m-1)
!         Trigonometric table for FFT analysis
!
!        %bar_omega  complex array, dimension(0:m-1)
!         Trigonometric table for FFT synthesis
!
! info    (output) integer
!         = 0  : Successful exit
!         = -i : The i:th argument has an illegal value.
!
! Calls
!
! alog2, xerbla, rou
!
! ----------------------------------------------------------------------
!
    if (m==2**nint(alog2(m))) then
      allocate(fft_tab%omega(0:m-1),fft_tab%bar_omega(0:m-1))
      call rou(fft_tab%bar_omega)
      fft_tab%omega = conjg(fft_tab%bar_omega)
    else
      info = -1
      call xerbla('FFTIN ',-info)
    end if
  end subroutine fftin
!
!
  subroutine rftterm(rft_tab,V,Z,info)
!
  integer, intent(out) :: info
  real(kind=rfp), dimension(:,:), pointer :: V
  complex(kind=kfp), dimension(:,:,:), pointer :: Z
  type(rft_tab_type) :: rft_tab
!
!
    deallocate(V)
    deallocate(rft_tab%r2c,rft_tab%c2r)
    call frftterm(rft_tab%frft_tab,Z,info)
  end subroutine rftterm
!
!
  subroutine frftterm(frft_tab,Z,info)
!
  integer, intent(out) :: info
  complex(kind=kfp), dimension(:,:,:), pointer :: Z
  type(frft_tab_type) :: frft_tab
!
!
    deallocate(Z)
    if (frft_tab%power_of_two) then
      call fftterm(frft_tab%fft_tab,info)
    else
      deallocate(frft_tab%t,frft_tab%ft_bar_t)
      deallocate(frft_tab%bar_t,frft_tab%ft_t)
      call fftterm(frft_tab%fft_tab,info)
    end if
  end subroutine frftterm
!
!
  subroutine fftterm(fft_tab,info)
!
  integer, intent(out) :: info
  type(fft_tab_type) :: fft_tab
!
! ----------------------------------------------------------------------
!
! Purpose
!
! fftterm deallocates the tables defined by fftin.
!
! Arguments
!
! fft_tab (input) type(fft_tab_type)
!         The tables that are to be deallocated.
!
! info    (output) integer
!         = 0  : Successful exit
!         = -i : The i:th argument has an illegal value.
!
! ----------------------------------------------------------------------
!
    deallocate(fft_tab%omega,fft_tab%bar_omega)
    info = 0
  end subroutine fftterm
!
!
  subroutine rft(mode,tab,V,old,new,Z,info)
!
  character, intent(in) :: mode
  integer :: old
  integer, intent(out) :: new,info
  real(kind=rfp), dimension(1:,0:) :: V
  complex(kind=kfp), dimension(1:,0:,0:) :: Z
  type(rft_tab_type), intent(in) :: tab
!
  integer :: m,j
!
    m = size(V,2)
    if (lsame(mode,'A')) then
      old = 0
      do j=0,m/2-1
        Z(:,j,old) = cmplx(V(:,2*j),V(:,2*j+1),kfp)
      end do
      call frft('A',m/2,tab%frft_tab,old,new,Z,info)
!
! Unscramble the result of the half-size FTs and extract
! the transforms. Start with vector elements 0 and m/2
!
      Z(:,0,old)   = cmplx(real(Z(:,0,new)) + aimag(Z(:,0,new)),0.0_rfp,kfp)
      Z(:,m/2,old) = cmplx(real(Z(:,0,new)) - aimag(Z(:,0,new)),0.0_rfp,kfp)
!
! and continue with the intermediate vector elements.
!
      do j=1,m/2-1
        Z(:,j,old) = 0.5_rfp*(Z(:,j,new) + conjg(Z(:,m/2-j,new)) -            &
                     tab%r2c(j)*(Z(:,j,new) - conjg(Z(:,m/2-j,new))))
      end do
      call exchng(new,old)
    else if (lsame(mode,'S')) then
      if (old==0) then
        new = 1
      else
        new = 0
      end if
!
! Scramble the data.
!
      do j=0,m/2-1
        Z(:,j,new) = Z(:,j,old) + conjg(Z(:,m/2-j,old)) -                     &
                     tab%c2r(j)*(Z(:,j,old) - conjg(Z(:,m/2-j,old)))
      end do
      call exchng(new,old)
      call frft('S',m/2,tab%frft_tab,old,new,Z,info)
      do j=0,m/2-1
        V(:,2*j) = real(Z(:,j,new))
        V(:,2*j+1) = aimag(Z(:,j,new))
      end do
    else
      info = -1
      call xerbla('RFT   ',-info)
    end if
  end subroutine rft
!
!
  subroutine frft(mode,m,tab,old,new,Z,info)
!
  character, intent(in) :: mode
  integer, intent(in) :: m
  integer :: old
  integer, intent(out) :: new,info
  complex(kind=kfp), dimension(1:,0:,0:) :: Z
  type(frft_tab_type), intent(in) :: tab
!
! ----------------------------------------------------------------------
!
! Purpose
!
! frft computes the unnormalized FrFT or inverse FrFT of the rows
! of the matrix Z(:,:,old). The transforms are returned in the rows
! of Z(:,:,new). In the algorithm, FFTs and inverse FFTs of size mhat
! are utilized, where mhat=2**ceiling(log2(m)+1).
!
! Arguments
!
! mode    (input) character
!         = 'A' : Perform FrFT analysis.
!         = 'S' : Perform FrFT synthesis.
!
! m       (input) integer
!         The size of the transforms
!
! tab     (input) type(frft_tab_type)
!         Tables for FrFT
!
!        %t  complex array, dimension(0:)
!         the first m elements of the mhat-vector "t"
!         used for FrFT analysis.
!
!        %bar_t  complex array, dimension(0:)
!         the first m elements of the mhat-vector "bar t"
!         used for FrFT synthesis.
!
!        %ft_bar_t  complex array, dimension(0:)
!         the FT of the mhat-vector "bar t"
!         used for FrFT analysis.
!
!        %ft_t  complex array, dimension(0:)
!         the FT of the mhat-vector "t"
!         used for FrFT synthesis.
!
!        %power_of_two  logical
!         = .true. if m is a power of two.
!
!        %fft_tab  type(fft_tab_type)
!         Trigonometric tables for FFT
!
! old     (input) integer
!         Index pointer to the data, see 'Z'. old = 0 or 1.
!
! new     (output) integer
!         Index pointer to the result, see 'Z'. new = 0 or 1.
!
! Z       (input/output) complex array, dimension (1:,0:,0:)
!         Z(:,0:,old) contains the data. On return,
!         Z(:,0:,new) contains the result.
!
! info    (output) integer
!         = 0  : Successful exit
!         = -i : The i:th argument has an illegal value.
!
! Calls
!
! lsame, xerbla, fft
!
! ----------------------------------------------------------------------
!
  integer :: mhat,j
!
    if (tab%power_of_two) then
      call fft(mode,tab%fft_tab,old,new,Z,info)
    else
!
! Initialization
!
      mhat = size(Z,2)
      if (old==0) then
        new = 1
      else
        new = 0
      end if
      if (lsame(mode,'A')) then
!
! Scale the data by "t".
!
        do j=0,m-1
          Z(:,j,old) = tab%t(j)*Z(:,j,old)
        end do
!
! Expand the rows of Z.
!
        Z(:,m:mhat-1,old) = 0.0_kfp
!
! Compute the FTs of size mhat.
!
        call fft('A',tab%fft_tab,old,new,Z,info)
!
! Scale by the FT of "bar t".
!
        do j=0,mhat-1
          Z(:,j,old) = tab%ft_bar_t(j)*Z(:,j,new)
        end do
!
! Compute the inverse FTs of size mhat.
!
        call fft('S',tab%fft_tab,old,new,Z,info)
!
! Scale by "t" again.
!
        do j=0,m-1
          Z(:,j,new) = tab%t(j)*Z(:,j,new)
        end do
      else if (lsame(mode,'S')) then
!
! Scale the data by "bar t".
!
        do j=0,m-1
          Z(:,j,old) = tab%bar_t(j)*Z(:,j,old)
        end do
!
! Expand the rows of Z.
!
        Z(:,m:mhat-1,old) = 0.0_kfp
!
! Compute the FTs of size mhat.
!
        call fft('A',tab%fft_tab,old,new,Z,info)
!
! Scale by the FT of "t".
!
        do j=0,mhat-1
          Z(:,j,old) = tab%ft_t(j)*Z(:,j,new)
        end do
!
! Compute the inverse FTs of size mhat.
!
        call fft('S',tab%fft_tab,old,new,Z,info)
!
! Scale by "bar t" again.
!
        do j=0,m-1
          Z(:,j,new) = tab%bar_t(j)*Z(:,j,new)
        end do
      else
        info = -1
        call xerbla('FRFT  ',-info)
      end if
    end if
  end subroutine frft
!
!
  subroutine fft(mode,tab,old,new,Z,info)
!
  character, intent(in) :: mode
  integer :: old
  integer, intent(out) :: new,info
  complex(kind=kfp), dimension(1:,0:,0:) :: Z
  type(fft_tab_type), intent(in) :: tab
!
! ----------------------------------------------------------------------
!
! Purpose
!
! fft computes the unnormalized FFT or inverse FFT of the rows
! of the matrix Z(:,:,old). The transforms are returned in the rows
! of Z(:,:,new). An autosort version of the FFT algorithm is used.
! Also, a combined algorithm of the type developed by Roberts is
! employed to enhance vector/parallel performance.
!
! Arguments
!
! mode    (input) character
!         = 'A' : Perform FFT analysis.
!         = 'S' : Perform FFT synthesis.
!
! tab     (input) type(fft_tab_type)
!         Trigonometric tables for FFT
!
!        %omega  complex array, dimension(0:)
!         Trigonometric table for FFT analysis
!
!        %bar_omega  complex array, dimension(0:)
!         Trigonometric table for FFT synthesis
!
! old     (input) integer
!         Index pointer to the data, see 'Z'. old = 0 or 1.
!
! new     (output) integer
!         Index pointer to the result, see 'Z'. new = 0 or 1.
!
! Z       (input/output) complex array, dimension (1:,0:,0:)
!         Z(:,0:,old) contains the data. On return,
!         Z(:,0:,new) contains the result.
!
! info    (output) integer
!         = 0  : Successful exit
!         = -i : The i:th argument has an illegal value.
!
! Calls
!
! lsame, xerbla, alog2, exchng
!
! ----------------------------------------------------------------------
!
  integer :: mh,lm,i2l,i2l1,m2l1,l,j,k,inc1,inc2,inc3,inc4
  complex(kind=kfp), dimension(0:ubound(tab%omega,1)) :: omega
  complex(kind=kfp), dimension(1:size(Z,1)) :: U
!
! Initialization
!
    if (lsame(mode,'A')) then
      omega = tab%omega
    else if (lsame(mode,'S')) then
      omega = tab%bar_omega
    else
      info = -1
      call xerbla('FFT   ',-info)
    end if
    mh = size(Z,2)/2
    lm = nint(alog2(mh))+1
    if (old==0) then
      new = 1
    else
      new = 0
    end if
!
! Perform the first step of the transforms.
!
    Z(:,0:2*mh-2:2,new) = Z(:,0:mh-1,old) + Z(:,mh:2*mh-1,old)
    Z(:,1:2*mh-1:2,new) = Z(:,0:mh-1,old) - Z(:,mh:2*mh-1,old)
!
! Perform half of the remaining steps.
!
    i2l  = 1
    m2l1 = mh
    do l=1,(lm-1)/2
      call exchng(new,old)
      i2l  = i2l*2
      i2l1 = i2l*2
      m2l1 = m2l1/2
      do j=0,m2l1-1
        do k=0,i2l-1
          inc1 = k
          inc2 = inc1+mh
          inc3 = k
          inc4 = inc3+i2l
          U = omega(k*m2l1)*Z(:,inc2+i2l*j,old)
          Z(:,inc3+i2l1*j,new) = Z(:,inc1+i2l*j,old) + U
          Z(:,inc4+i2l1*j,new) = Z(:,inc1+i2l*j,old) - U
        end do
      end do
    end do
!
! Perform the final steps.
!
    do l=(lm-1)/2+1,lm-1
      call exchng(new,old)
      i2l  = i2l*2
      i2l1 = i2l*2
      m2l1 = m2l1/2
      do k=0,i2l-1
        inc1 = k
        inc2 = inc1+mh
        inc3 = k
        inc4 = inc3+i2l
        do j=0,m2l1-1
          U = omega(k*m2l1)*Z(:,inc2+i2l*j,old)
          Z(:,inc3+i2l1*j,new) = Z(:,inc1+i2l*j,old) + U
          Z(:,inc4+i2l1*j,new) = Z(:,inc1+i2l*j,old) - U
        end do
      end do
    end do
  end subroutine fft

END MODULE fourier_transf_mod
