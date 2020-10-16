!+
! NAME
!       PEC_mod - PEC module
!
! DESCRIPTION
!       Module for treating PEC (incl. routines for locating patches)
!       The structure of this module stems from the times before it was a 
!       module. Hence the concept CALL count-allocate-CALL read is used.
!       Now, when we use modules it is possible to e.g. merge all patch finding
!       subroutines into one or even to put all this code in PECinit. (ulfa)
!
! PUBLIC
!       SUBROUTINE PECinit
!       SUBROUTINE PECapply
!       SUBROUTINE PECend
!       SUBROUTINE PECsetparsedvalues
!       integer, dimension(:,:), allocatable :: PEC_Ex, PEC_Ey, PEC_Ez
!       integer, dimension(:,:), allocatable :: Patch_Jx, Patch_Jy, Patch_Jz
!       integer, dimension(3) :: PEC_size, Patch_size
!       integer :: PEC_high_i, PEC_high_j, PEC_high_k
!       integer :: PEC_low_i, PEC_low_j, PEC_low_k
!       integer :: PEC_surfcurrsize
!
! SEE ALSO
!       PECplate_mod which treats PEC given as a collection of surfaces.
!       UPML_mod which is the only ABC that can treat PEC extending to inf.
!
! HISTORY
!       Version       Date                 Name
!       Comments
!       -------------------------------------------
!	$Log: PEC.f90,v $
!	Revision 1.4  2005/04/05 10:44:33  ulfa
!	Removed ref. to D10 document.
!	
!	Revision 1.3  2005/04/05 10:34:18  ulfa
!	Added comments.
!	
!	Revision 1.2  2003/09/26 21:50:17  ulfa
!	Added one more line of info on stdout.
!	
!	Revision 1.1  2003/09/23 14:06:51  ulfa
!	Initial version.
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE PEC_mod
USE parameter_mod, ONLY : rfp, one_byte, two_byte

IMPLICIT NONE

PUBLIC PECinit, PECapply, PECend, PECsetparsedvalues
!
! Public variables
!
logical, PUBLIC :: PEC_found = .false.
integer, dimension(:,:), allocatable, PUBLIC :: PEC_Ex, PEC_Ey, PEC_Ez
integer, dimension(:,:), allocatable, PUBLIC :: Patch_Jx, Patch_Jy, Patch_Jz
integer, dimension(3), PUBLIC :: PEC_size, Patch_size
integer, PUBLIC :: PEC_high_i, PEC_high_j, PEC_high_k
integer, PUBLIC :: PEC_low_i, PEC_low_j, PEC_low_k
integer, PUBLIC :: PEC_surfcurrsize


PRIVATE
!
! PEC variables
!
integer(kind=two_byte), dimension(:,:), pointer :: parsed_pec   
integer, dimension(:,:), allocatable :: TMP_Patch_Jx
integer, dimension(:,:), allocatable :: TMP_Patch_Jy
integer, dimension(:,:), allocatable :: TMP_Patch_Jz
integer(kind=one_byte), dimension(:,:,:,:), allocatable :: Tmp_patches

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       PECinit - Allocate and initialize private PEC data
!
! DESCRIPTION
!       Allocate and initialize private PEC data
!       
! METHOD
!       1) Counts, allocates and read a PEC file.
!       2) Finds surface patches. Due to memory constraints, this is done
!        in three separate steps for X patches, Y patches and Z patches.
!        This makes it possible to use the same temporary array. 
!
! SYNOPSIS
!       CALL PECinit(nx,ny,nz)
!         integer, intent(in) :: nx, ny, nz
!
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Ulf Andersson
!
!       The code in this PEC module was developed during the one billion 
!       cells Saab 2000 calculation which was performed in the autumn of 1998.
!       This was during the days of fridas predecessor pscyee and prior to our 
!       use of modules. During this calculation it was a critical issue, not to
!       waste memory. This caused us to introduce the temporary arrays 
!       TMP_Patch_J[x-z]. Below is an attempt to explain this.
!
!       The temporary array Tmp_patches has the size
!       4*(nx+1)*(ny+1)*(nz+1). It was made a one byte integer array to
!       minimize storage. I do not remember if this was necessary. The 
!       calculation was performed using 32-bit precision. This meant that each
!       field component used 4*(nx+2)*(ny+2)*(nz+2) bytes including the 
!       ghost cells in our parallel implementation, i.e. they were slightly 
!       larger than the array Tmp_patches. Hence, they did not fit into the 
!       deallocated area left by Tmp_patches. This caused swapping that 
!       severely lessened the code performance. I am afraid I do not remember 
!       the details of this affair.
!
!       This explanation/description was inserted by ulfa in order to motivate 
!       the use of the temporary arrays TMP_Patch_J[x-z].
!
!       A necessary fact for this to work is that the total size of the three
!       arrays Patch_J[x-z] is lesser that the size of Tmp_patches. This is a
!       reasonable assumption since the arrays Patch_J[x-z] are 2D.
!
!       In retrospect one may wonder why the gap in the heap wasn't simply
!       swapped to disc and remained there. I have no answer to this.
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE PECinit(nx,ny,nz)
USE errorcheck_mod, ONLY : check_allocate, check_deallocate

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: nx, ny, nz

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: allocstat

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------
 
CALL CountPEC(nx,ny,nz)

allocate( PEC_Ex(PEC_size(1),3), STAT=allocstat ) 
call check_allocate(allocstat,'PEC_Ex',3)
PEC_Ex = 0

allocate( PEC_Ey(PEC_size(2),3), STAT=allocstat ) 
call check_allocate(allocstat,'PEC_Ey',3)
PEC_Ey = 0

allocate( PEC_Ez(PEC_size(3),3), STAT=allocstat ) 
call check_allocate(allocstat,'PEC_Ez',3)
PEC_Ez = 0

CALL ReadPEC(nx,ny,nz)
!
! Allocate temporary array Tmp_patches, which is used when finding patches.
! We allocate Tmp_patches from zero due to the -1 occurring in the indexes 
! when filling up Tmp_patches. We allocate to n[xyz]+1 because the highest
! possible value in PEC_E[xyz] is n[xyz]+1. (There can be no patches with
! indexes 0 or n[xyz]+1 because that is outside the domain. However, by
! allocating Tmp_patches in this way we avoid having to treat PEC components
! close to the outer boundary as special cases when filling up Tmp_patches.)
!
allocate( Tmp_patches(0:nx+1,0:ny+1,0:nz+1,4), STAT=allocstat ) 
call check_allocate(allocstat,'Tmp_patches',3)
Tmp_patches = 0
!
! Find X patches on the surface of the PEC object(s)
!

write(*,*) 'Searching for patches in PECinit:'

CALL Count_XPatches(nx,ny,nz)

allocate ( TMP_Patch_Jx(Patch_size(1),3), STAT=allocstat ) 
call check_allocate(allocstat,'TMP_Patch_Jx',3)
TMP_Patch_Jx = 0

CALL Store_Patches(nx,ny,nz,Patch_size(1),TMP_Patch_Jx)
!
! Find Y patches on the surface of the PEC object(s)
!
Tmp_patches = 0
CALL Count_YPatches(nx,ny,nz)

allocate ( TMP_Patch_Jy(Patch_size(2),3), STAT=allocstat ) 
call check_allocate(allocstat,'TMP_Patch_Jy',3)
TMP_Patch_Jy = 0

CALL Store_Patches(nx,ny,nz,Patch_size(2),TMP_Patch_Jy)
!
! Find Z patches on the surface of the PEC object(s)
!
Tmp_patches = 0
CALL Count_ZPatches(nx,ny,nz)

allocate ( TMP_Patch_Jz(Patch_size(3),3), STAT=allocstat ) 
call check_allocate(allocstat,'TMP_Patch_Jz',3)
TMP_Patch_Jz = 0

CALL Store_Patches(nx,ny,nz,Patch_size(3),TMP_Patch_Jz)

deallocate( Tmp_patches, STAT=allocstat )
call check_deallocate(allocstat,'Tmp_patches',1)
!
! The temporary arrays TMP_Patch_J* are needed to avoid gaps in the heap.
! see HISTORY in the head for a more detailed discussion.
!
allocate ( Patch_Jx(Patch_size(1),3), STAT=allocstat ) 
call check_allocate(allocstat,'Patch_Jx',3)

Patch_Jx = TMP_Patch_Jx

allocate ( Patch_Jy(Patch_size(2),3), STAT=allocstat ) 
call check_allocate(allocstat,'Patch_Jy',3)
Patch_Jy = TMP_Patch_Jy

allocate ( Patch_Jz(Patch_size(3),3), STAT=allocstat )
call check_allocate(allocstat,'Patch_Jz',3)
Patch_Jz = TMP_Patch_Jz

deallocate ( TMP_Patch_Jx , STAT=allocstat )
call check_deallocate(allocstat,'TMP_Patch_Jx',1)
deallocate ( TMP_Patch_Jy , STAT=allocstat )
call check_deallocate(allocstat,'TMP_Patch_Jy',1)
deallocate ( TMP_Patch_Jz , STAT=allocstat )
call check_deallocate(allocstat,'TMP_Patch_Jz',1)

! want to set this before writing
PEC_surfcurrsize = 2*sum(Patch_size)

END SUBROUTINE PECinit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       CountPEC - Counts the lines in a PEC array
!
! DESCRIPTION
!       Reads the PEC array and counts the occurrences of
!       Ex, Ey and Ez values that should be zeroed. The result
!       is stored in PEC_size as
!
!         PEC_size(1)    number of Ex points that are PEC
!         PEC_size(2)    number of Ey points that are PEC
!         PEC_size(3)    number of Ez points that are PEC
!
! METHOD
!       see DESCRIPTION and ERRORS
!
! SYNOPSIS
!       CALL CountPEC(nx,ny,nz)
!         integer, intent(in) :: nx, ny, nz
!       allocate( PEC_Ex(PEC_size(1),3) ) ; PEC_Ex = 0
!       allocate( PEC_Ey(PEC_size(2),3) ) ; PEC_Ey = 0
!       allocate( PEC_Ez(PEC_size(3),3) ) ; PEC_Ez = 0
!       CALL ReadPEC(nx,ny,nz)
!
! ERRORS 
!       Lines with an incorrect identification number p (the first number)
!       are not counted. Lines with incorrect I, J or K-index values are 
!       not counted. A warning message is printed for each incorrect line.
!       Uses the errorcheck module 
!
! HISTORY
!       Written by Ulf Andersson
!       Updated to handle comments in PEC file by ulfa 2000-05-10
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE CountPEC(nx,ny,nz)
USE Readline_mod, ONLY : readline

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: nx, ny, nz

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

logical                     :: ERROR
integer, dimension(4)       :: one_line
integer                     :: i

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

ERROR = .FALSE.
PEC_size = 0
do i=1, size(parsed_pec,1)
  one_line = parsed_pec(i,1:4)
   
  !! Check if the P value is correct. It should be 1<=P<=3
  
  if ((one_line(1)<1).or.(one_line(1)>3)) then 
    write(*,*) 'Warning!, Incorrect P value in geometry file ', one_line(1)
    ERROR = .TRUE.
  end if

  !! Check that the I, J and K indexes are valid

  if ((one_line(2)<1).or.(one_line(2)>nx+1)) then
    write(*,*) 'Warning!, Incorrect I-index in geometry file ', one_line(2)
    ERROR = .TRUE.
  end if
  if ((one_line(2)==nx+1).and.(one_line(1)==1)) then
    write(*,*) 'Warning!, Incorrect I-index in geometry file '
    write(*,*) 'I-index must not be nx+1 for an Ex component (p=1)'
    ERROR = .TRUE.
  end if

  if ((one_line(3)<1).or.(one_line(3)>ny+1)) then
    write(*,*) 'Warning!, Incorrect J-index in geometry file ', one_line(3)
    ERROR = .TRUE.
  end if
  if ((one_line(3)==ny+1).and.(one_line(1)==2)) then
    write(*,*) 'Warning!, Incorrect J-index in geometry file '
    write(*,*) 'J-index must not be ny+1 for an Ey component (p=2)'
    ERROR = .TRUE.
  end if

  if ((one_line(4)<1).or.(one_line(4)>nz+1)) then
    write(*,*) 'Warning!, Incorrect K-index in geometry file ', one_line(4) 
    ERROR = .TRUE.
  end if
  if ((one_line(4)==nz+1).and.(one_line(1)==3)) then
    write(*,*) 'Warning!, Incorrect K-index in geometry file '
    write(*,*) 'K-index must not be nz+1 for an Ez component (p=3)'
    ERROR = .TRUE.
  end if
  
  if (ERROR) then
    ERROR = .FALSE.
  else
    PEC_size(one_line(1)) = PEC_size(one_line(1)) + 1
  end if
end do

END SUBROUTINE CountPEC

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       ReadPEC - Reads a PEC array
!
! DESCRIPTION
!       Reads a PEC array.
!
! METHOD
!       Reads through a PEC array and stores the points
!       that are PEC points in three separate arrays, one for each
!       E-field.
!
!       PEC_Ex(:,1) stores the I index (1<=I<=nx)
!       PEC_Ex(:,2) stores the J index (1<=J<=ny)
!       PEC_Ex(:,3) stores the K index (1<=K<=nz)
!
!       The input format is expected to be:
!
!       P I J K
!
!       where 1<=P<=3
!
! SYNOPSIS
!       CALL ReadPEC(nx,ny,nz)
!         integer, intent(in) :: nx, ny, nz
!       CALL CountPEC(nx,ny,nz)
!       allocate( PEC_Ex(PEC_size(1),3) ) ; PEC_Ex = 0
!       allocate( PEC_Ey(PEC_size(2),3) ) ; PEC_Ey = 0
!       allocate( PEC_Ez(PEC_size(3),3) ) ; PEC_Ez = 0
!
! ERRORS
!       Lines with an incorrect identification number p (the first number)
!       are ignored. Lines with incorrect I, J or K-index values are ignored.
!       No warning messages are printed since that was done by CountPEC.
!       Uses the errorcheck module        
!
! HISTORY
!       Written by Ulf Andersson
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE ReadPEC(nx,ny,nz)
USE errorcheck_mod, ONLY : check_allocate, check_deallocate, fatal, normal

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: nx, ny, nz

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

logical                              :: ERROR
integer, dimension(3)                :: counter
integer, dimension(4)                :: one_line
integer                              :: i, allocstat
integer, dimension(:,:), allocatable :: PEC_tillf

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

counter = 0
ERROR = .FALSE.

do i=1, size(parsed_pec,1)
  one_line = parsed_pec(i,1:4)

  !! Check that the I-, J- and K-indexes are valid

  if ((one_line(2)<1).or.(one_line(2)>nx+1)) then
    ERROR = .TRUE.
  end if
  if ((one_line(2)==nx+1).and.(one_line(1)==1)) then
    ERROR = .TRUE.
  end if

  if ((one_line(3)<1).or.(one_line(3)>ny+1)) then
    ERROR = .TRUE.
  end if
  if ((one_line(3)==ny+1).and.(one_line(1)==2)) then
    ERROR = .TRUE.
  end if

  if ((one_line(4)<1).or.(one_line(4)>nz+1)) then
    ERROR = .TRUE.
  end if
  if ((one_line(4)==nz+1).and.(one_line(1)==3)) then
    ERROR = .TRUE.
  end if
  
  if (ERROR) then
    ERROR = .FALSE.
  else
    if (one_line(1)==1) then
      counter(1) = counter(1) + 1
      PEC_Ex(counter(1),1:3) = one_line(2:4)
    else if (one_line(1)==2) then
      counter(2) = counter(2) + 1
      PEC_Ey(counter(2),1:3) = one_line(2:4)
    else if (one_line(1)==3) then
      counter(3) = counter(3) + 1
      PEC_Ez(counter(3),1:3) = one_line(2:4)
!   else incorrect p value
!
!     No point in writing a warning since that was done by CountPEC 
!
    end if
  end if
    
end do ! line loop

!
! Sort the entries so that the inner index changes most rapidly
!
! Sort3 sorts on the two first columns, with highest priority on the first.
! However, we want to sort PEC_E[xyz] after the third and final column.
! For pure laziness we use an existing routine

allocate( PEC_tillf(PEC_size(1),3), STAT=allocstat ) 
call check_allocate(allocstat,'PEC_tillf',fatal)
PEC_tillf(:,1) = PEC_Ex(:,3)
PEC_tillf(:,2) = PEC_Ex(:,2)
PEC_tillf(:,3) = PEC_Ex(:,1)
call sort3(PEC_tillf,PEC_size(1))
PEC_Ex(:,1) = PEC_tillf(:,3)
PEC_Ex(:,2) = PEC_tillf(:,2)
PEC_Ex(:,3) = PEC_tillf(:,1)
deallocate( PEC_tillf, STAT=allocstat ) 
call check_deallocate(allocstat,'PEC_tillf',normal)

allocate( PEC_tillf(PEC_size(2),3), STAT=allocstat ) 
call check_allocate(allocstat,'PEC_tillf',fatal)
PEC_tillf(:,1) = PEC_Ey(:,3)
PEC_tillf(:,2) = PEC_Ey(:,2)
PEC_tillf(:,3) = PEC_Ey(:,1)
call sort3(PEC_tillf,PEC_size(2))
PEC_Ey(:,1) = PEC_tillf(:,3)
PEC_Ey(:,2) = PEC_tillf(:,2)
PEC_Ey(:,3) = PEC_tillf(:,1)
deallocate( PEC_tillf, STAT=allocstat ) 
call check_deallocate(allocstat,'PEC_tillf',normal)

allocate( PEC_tillf(PEC_size(3),3), STAT=allocstat ) 
call check_allocate(allocstat,'PEC_tillf',fatal)
PEC_tillf(:,1) = PEC_Ez(:,3)
PEC_tillf(:,2) = PEC_Ez(:,2)
PEC_tillf(:,3) = PEC_Ez(:,1)
call sort3(PEC_tillf,PEC_size(3))
PEC_Ez(:,1) = PEC_tillf(:,3)
PEC_Ez(:,2) = PEC_tillf(:,2)
PEC_Ez(:,3) = PEC_tillf(:,1)
deallocate( PEC_tillf, STAT=allocstat ) 
call check_deallocate(allocstat,'PEC_tillf',normal)

!
! Safety test. Should never happen. If it does then the code is incorrectly 
! written. (ulfa) 
!
if (any(PEC_size/=counter)) then
  write(*,*) 'FATAL ERROR! PEC_size differs from counter in ReadPEC'
  write(*,*) 'PEC_size = ', PEC_size
  write(*,*) 'counter  = ', counter
  stop
end if

END SUBROUTINE ReadPEC

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       PECapply - Apply PEC BC by putting the appropriate E-fields to zero
!
! DESCRIPTION
!       Goes through the arrays PEC_Ex, PEC_Ey and PEC_Ez to find all the
!       E-field components that should be set to zero.
!
! METHOD
!       see DESCRIPTION
!
! SYNOPSIS
!       CALL PECapply(Ex,Ey,Ez)
!         real(kind=rfp),                                                     &
!                dimension(xstart:xstop+1,ystart:ystop+1,zstart:zstop+1),     &
!                intent(inout) :: Ex, Ey, Ez
!
! ERRORS
!       No error handling
!
! SEE ALSO
!       The subroutine ReadPEC in which PEC_Ex, PEC_Ey and PEC_Ez are given
!       their values.
!
! HISTORY
!       Written by Ulf Andersson
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
SUBROUTINE PECapply(Ex,Ey,Ez)
USE globalvar_mod, ONLY : xstart, ystart, zstart, xstop, ystop, zstop

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

real(kind=rfp), dimension(xstart:xstop+1,ystart:ystop+1,zstart:zstop+1),      &
                intent(inout) :: Ex, Ey, Ez

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: ijk

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

!
! Putting Ex-values to zero
!
do ijk=1,PEC_size(1)
  Ex(PEC_Ex(ijk,1),PEC_Ex(ijk,2),PEC_Ex(ijk,3)) = 0
end do
!
! Putting Ey-values to zero
!
do ijk=1,PEC_size(2)
  Ey(PEC_Ey(ijk,1),PEC_Ey(ijk,2),PEC_Ey(ijk,3)) = 0
end do
!
! Putting Ez-values to zero
!
do ijk=1,PEC_size(3)
  Ez(PEC_Ez(ijk,1),PEC_Ez(ijk,2),PEC_Ez(ijk,3)) = 0
end do

END SUBROUTINE PECapply

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Count_XPatches - Count the number of patches with normal=(+-1,0,0)
!
! DESCRIPTION
!       Given a list of Ey- and Ez-fields (in PEC_Ey and PEC_Ez) that belong 
!       to a PEC surface, Count_XPatches finds the number of patches with 
!       normals=(+-1,0,0). A Hx-field is the center of a surface patch if it
!       is surrounded by four E-fields that belong to the surface.
!
! METHOD
!       Set Tmp_patches=0 (done already in PECinit!!!)
!
!       Sweeps through PEC_Ez and sets
!         Tmp_patches(I,J  ,K,1) = 1
!         Tmp_patches(I,J-1,K,3) = 1
!       for every (I,J,K) triplet in PEC_Ez
!
!       Sweeps through PEC_Ey and sets
!         Tmp_patches(I,J,K  ,4) = 1
!         Tmp_patches(I,J,K-1,2) = 1
!       for every (I,J,K) triplet in PEC_Ey
!
!       Then sweeps through Tmp_patches and count the patches.
!       A patch is identified by  all(Tmp_patches(ii,jj,kk,1:4)==1)==.TRUE.
!
!       Note that the values in Tmp_patches are reused in Store_Patches.
!       To simplify identification of patches in Store_Patches we set
!       Tmp_patches(ii,jj,kk,1)=4 for every patch.
!
!       The number of patches are stored in Patch_size(1)
!
!       Note that the method used has the advantage that it can handle
!       multiple occurrences of the same index triplet in the PEC_E* variables.
! 
! SYNOPSIS
!       Tmp_patches = 0
!       CALL Count_XPatches(nx,ny,nz)
!         integer, intent(in) :: nx, ny, nz
!       allocate ( TMP_Patch_Jx(Patch_size(1),3) ) ; TMP_Patch_Jx = 0
!       CALL Store_Patches(nx,ny,nz,Patch_size(1),TMP_Patch_Jx)
!
! ERRORS
!       No error handling
!
! SEE ALSO
!       Store_Patches, Count_YPatches and Count_ZPatches
!
!       Count_XPatches is very similar to Count_YPatches and Count_ZPatches.
!       It would be possible to write one subroutine that treats all three 
!       cases but in the interest of clarity of the code I (ulfa) chose not to.
!
! HISTORY
!       Written by Ulf Andersson
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Count_XPatches(nx,ny,nz)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: nx, ny, nz

!------------------------------------------------------------------------------
!                     L o c a l  p a r a m e t e r s
!------------------------------------------------------------------------------

integer :: ii, jj, kk, count

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

do kk=1,PEC_size(3)
  Tmp_patches(PEC_Ez(kk,1)  ,PEC_Ez(kk,2)  ,PEC_Ez(kk,3)  ,1) = 1
  Tmp_patches(PEC_Ez(kk,1)  ,PEC_Ez(kk,2)-1,PEC_Ez(kk,3)  ,3) = 1
end do
do jj=1,PEC_size(2)
  Tmp_patches(PEC_Ey(jj,1)  ,PEC_Ey(jj,2)  ,PEC_Ey(jj,3)  ,4) = 1
  Tmp_patches(PEC_Ey(jj,1)  ,PEC_Ey(jj,2)  ,PEC_Ey(jj,3)-1,2) = 1
end do
count = 0

do kk=1,nz
  do jj=1,ny
    do ii=1,nx
!    if (sum(Tmp_patches(ii,jj,kk,:))==4) then ! This is far too slow (ulfa)
      if ( (Tmp_patches(ii,jj,kk,1)==1).and.                                  &
           (Tmp_patches(ii,jj,kk,2)==1).and.                                  &
           (Tmp_patches(ii,jj,kk,3)==1).and.                                  &
           (Tmp_patches(ii,jj,kk,4)==1) ) then
        count = count+1
        Tmp_patches(ii,jj,kk,1) = 4
      end if
    end do
  end do
end do
write(*,*) count, 'X-patches found'
Patch_size(1) = count

END SUBROUTINE Count_XPatches

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Count_YPatches - Count the number of patches with normal=(0,+-1,0)
!
! DESCRIPTION
!       Given a list of Ex- and Ez-fields (in PEC_Ex and PEC_Ez) that belong 
!       to a PEC surface, Count_YPatches finds the number of patches with 
!       normals=(0,+-1,0). A Hy-field is the center of a surface patch if it
!       is surrounded by four E-fields that belong to the surface.
!
! METHOD
!       Set Tmp_patches=0 (done already in PECinit!!!)
!
!       Sweeps through PEC_Ex and sets
!         Tmp_patches(I,J,K  ,1) = 1
!         Tmp_patches(I,J,K-1,3) = 1
!       for every (I,J,K) triplet in PEC_Ex
!
!       Sweeps through PEC_Ez and sets
!         Tmp_patches(I  ,J,K,4) = 1
!         Tmp_patches(I-1,J,K,2) = 1
!       for every (I,J,K) triplet in PEC_Ez
!
!       Then sweeps through Tmp_patches and count the patches.
!       A patch is identified by  all(Tmp_patches(ii,jj,kk,1:4)==1)==.TRUE.
!
!       Note that the values in Tmp_patches are reused in Store_Patches.
!       To simplify identification of patches in Store_Patches we set
!       Tmp_patches(ii,jj,kk,1)=4 for every patch.
!
!       The number of patches are stored in Patch_size(2)
!
!       Note that the method used has the advantage that it can handle
!       multiple occurrences of the same index triplet in the PEC_E* variables.
! 
! SYNOPSIS
!       Tmp_patches = 0
!       CALL Count_YPatches(nx,ny,nz)
!         integer, intent(in) :: nx, ny, nz
!       allocate ( TMP_Patch_Jy(Patch_size(2),3) ) ; TMP_Patch_Jy = 0
!       CALL Store_Patches(nx,ny,nz,Patch_size(2),TMP_Patch_Jy)
!
! ERRORS
!       No error handling
!
! SEE ALSO
!       Store_Patches, Count_XPatches and Count_ZPatches
!
!       Count_YPatches is very similar to Count_XPatches and Count_ZPatches.
!       It would be possible to write one subroutine that treats all three 
!       cases but in the interest of clarity of the code I (ulfa) chose not to.
!
! HISTORY
!       Written by Ulf Andersson
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Count_YPatches(nx,ny,nz)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: nx, ny, nz

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: ii, jj, kk, count

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

do ii=1,PEC_size(1)
  Tmp_patches(PEC_Ex(ii,1)  ,PEC_Ex(ii,2)  ,PEC_Ex(ii,3)  ,1) = 1
  Tmp_patches(PEC_Ex(ii,1)  ,PEC_Ex(ii,2)  ,PEC_Ex(ii,3)-1,3) = 1
end do
do kk=1,PEC_size(3)
  Tmp_patches(PEC_Ez(kk,1)  ,PEC_Ez(kk,2)  ,PEC_Ez(kk,3)  ,4) = 1
  Tmp_patches(PEC_Ez(kk,1)-1,PEC_Ez(kk,2)  ,PEC_Ez(kk,3)  ,2) = 1
end do
count = 0
do kk=1,nz
  do jj=1,ny
    do ii=1,nx
      if ( (Tmp_patches(ii,jj,kk,1)==1).and.                                  &
           (Tmp_patches(ii,jj,kk,2)==1).and.                                  &
           (Tmp_patches(ii,jj,kk,3)==1).and.                                  &
           (Tmp_patches(ii,jj,kk,4)==1) ) then
        count = count+1
        Tmp_patches(ii,jj,kk,1) = 4
      end if
    end do
  end do
end do
write(*,*) count, 'Y-patches found'
Patch_size(2) = count

END SUBROUTINE Count_YPatches

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Count_ZPatches - Count the number of patches with a normal=(0,0,+-1)
!
! DESCRIPTION
!       Given a list of Ex- and Ey-fields (in PEC_Ex and PEC_Ey) that belong 
!       to a PEC surface, Count_XPatches finds the number of patches with 
!       normals=(0,0,+-1). A Hz-field is the center of a surface patch if it
!       is surrounded by four E-fields that belong to the surface.
!
! METHOD
!       Set Tmp_patches=0 (done already in PECinit!!!)
!
!       Sweeps through PEC_Ex and sets
!         Tmp_patches(I,J  ,K,4) = 1
!         Tmp_patches(I,J-1,K,2) = 1
!       for every (I,J,K) triplet in PEC_Ex
!
!       Sweeps through PEC_Ey and sets
!         Tmp_patches(I  ,J,K,1) = 1
!         Tmp_patches(I-1,J,K,3) = 1
!       for every (I,J,K) triplet in PEC_Ey
!
!       Then sweeps through Tmp_patches and count the patches.
!       A patch is identified by  all(Tmp_patches(ii,jj,kk,1:4)==1)==.TRUE.
!
!       Note that the values in Tmp_patches are reused in Store_Patches.
!       To simplify identification of patches in Store_Patches we set
!       Tmp_patches(ii,jj,kk,1)=4 for every patch.
!
!       The number of patches are stored in Patch_size(3)
!
!       Note that the method used has the advantage that it can handle
!       multiple occurrences of the same index triplet in the PEC_E* variables.
! 
! SYNOPSIS
!       Tmp_patches = 0
!       CALL Count_ZPatches(nx,ny,nz)
!         integer, intent(in) :: nx, ny, nz
!       allocate ( TMP_Patch_Jz(Patch_size(3),3) ) ; TMP_Patch_Jz = 0
!       CALL Store_Patches(nx,ny,nz,Patch_size(3),TMP_Patch_Jz)
!
! ERRORS
!       No error handling
!
! SEE ALSO
!       Store_Patches, Count_XPatches and Count_YPatches
!
!       Count_ZPatches is very similar to Count_XPatches and Count_YPatches.
!       It would be possible to write one subroutine that treats all three 
!       cases but in the interest of clarity of the code I (ulfa) chose not to.
!
! HISTORY
!       Written by Ulf Andersson
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Count_ZPatches(nx,ny,nz)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: nx, ny, nz

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: ii, jj, kk, count

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

Tmp_patches = 0
do ii=1,PEC_size(1)
  Tmp_patches(PEC_Ex(ii,1)  ,PEC_Ex(ii,2)  ,PEC_Ex(ii,3)  ,4) = 1
  Tmp_patches(PEC_Ex(ii,1)  ,PEC_Ex(ii,2)-1,PEC_Ex(ii,3)  ,2) = 1
end do
do jj=1,PEC_size(2)
  Tmp_patches(PEC_Ey(jj,1)  ,PEC_Ey(jj,2)  ,PEC_Ey(jj,3)  ,1) = 1
  Tmp_patches(PEC_Ey(jj,1)-1,PEC_Ey(jj,2)  ,PEC_Ey(jj,3)  ,3) = 1
end do

count = 0
do kk=1,nz
  do jj=1,ny
    do ii=1,nx
      if ( (Tmp_patches(ii,jj,kk,1)==1).and.                                  &
           (Tmp_patches(ii,jj,kk,2)==1).and.                                  &
           (Tmp_patches(ii,jj,kk,3)==1).and.                                  &
           (Tmp_patches(ii,jj,kk,4)==1) ) then
        count = count+1
        Tmp_patches(ii,jj,kk,1) = 4
      end if
    end do
  end do
end do
write(*,*) count, 'Z-patches found'
Patch_size(3) = count 

END SUBROUTINE Count_ZPatches

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       Store_Patches - writes all patches into a Patch_J array.
!
! DESCRIPTION
!       Given a 4D array (Tmp_patches) that identifies patches as those 
!       (I,J,K) indexes for which Tmp_patches(I,J,K,1)==4 Store_Patches
!       writes the indexes of the patches into the array Patch_J which 
!       is assumed to be of the correct size.       
!
! SYNOPSIS
!       CALL Count_XPatches(nx,ny,nz)
!       allocate ( TMP_Patch_Jx(Patch_size(1),3) ) ; TMP_Patch_Jx = 0
!       CALL Store_Patches(nx,ny,nz,Patch_size(1),TMP_Patch_Jx)
!         integer, intent(in) :: nx, ny, nz
!         integer, intent(in) :: Tmp_patch_size
!         integer, dimension(Tmp_patch_size,3), intent(out) :: Patch_J
!! Find Y patches on the surface
!       Tmp_patches = 0
!       CALL Count_YPatches(nx,ny,nz)
!       allocate ( TMP_Patch_Jy(Patch_size(2),3) ) ; TMP_Patch_Jy = 0
!       CALL Store_Patches(nx,ny,nz,Patch_size(2),TMP_Patch_Jy)
!         integer, intent(in) :: nx, ny, nz
!         integer, intent(in) :: Tmp_patch_size
!         integer, dimension(Tmp_patch_size,3), intent(out) :: Patch_J
!! Find Z patches on the surface
!       Tmp_patches = 0
!       CALL Count_ZPatches(nx,ny,nz)
!       allocate ( TMP_Patch_Jz(Patch_size(3),3) ) ; TMP_Patch_Jz = 0
!       CALL Store_Patches(nx,ny,nz,Patch_size(3),TMP_Patch_Jz)
!         integer, intent(in) :: nx, ny, nz
!         integer, intent(in) :: Tmp_patch_size
!         integer, dimension(Tmp_patch_size,3), intent(out) :: Patch_J
!
!       I.e. Store_Patches may be called with any of the three patch-arrays
!       Patch_Jx, Patch_Jy and Patch_Jz. Make sure to supply the correct
!       size for the Patch_J variables.
!
! ERRORS
!       No error handling
!
! SEE ALSO
!       Count_XPatches, Count_YPatches and Count_ZPatches
!
! HISTORY
!       Written by Ulf Andersson
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Store_Patches(nx,ny,nz,Tmp_patch_size,Patch_J)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer, intent(in) :: nx, ny, nz
integer, intent(in) :: Tmp_patch_size
integer, dimension(Tmp_patch_size,3), intent(out) :: Patch_J

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: ii, jj, kk, count

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

count = 0
do ii=1,nx
  do jj=1,ny
    do kk=1,nz
      if (Tmp_patches(ii,jj,kk,1)==4) then
        count = count+1
        Patch_J(count,1) = ii
        Patch_J(count,2) = jj
        Patch_J(count,3) = kk
      end if
    end do
  end do
end do

!
! Safety check, should never happen if the code is correct
!
if (Tmp_patch_size/=count) then
  write(*,*) 'WARNING! Error in Store_Patches'
  write(*,*) 'Tmp_patch_size = ', Tmp_patch_size, ' count = ', count
end if

END SUBROUTINE Store_Patches

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       PECend - Deallocate private PEC data
!
! DESCRIPTION
!       Deallocate private PEC data
!
! SYNOPSIS
!       CALL PECend
!
! ERRORS
!       Uses the errorcheck module
!
! HISTORY
!       Written by Ulf Andersson
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE PECend
USE errorcheck_mod, ONLY : check_deallocate

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: allocstat

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

deallocate( PEC_Ex , STAT = allocstat)
call check_deallocate(allocstat, 'PEC_Ex', 1)
deallocate( PEC_Ey , STAT = allocstat)
call check_deallocate(allocstat, 'PEC_Ey', 1)
deallocate( PEC_Ez , STAT = allocstat)
call check_deallocate(allocstat, 'PEC_Ez', 1)

deallocate( Patch_Jx , STAT = allocstat )
call check_deallocate(allocstat,'Patch_Jx',1)
deallocate( Patch_Jy , STAT = allocstat )
call check_deallocate(allocstat,'Patch_Jy',1)
deallocate( Patch_Jz , STAT = allocstat )
call check_deallocate(allocstat,'Patch_Jz',1)

deallocate(parsed_pec , STAT = allocstat )
call check_deallocate(allocstat,'parsed_pec in PEC_mod',1)

END SUBROUTINE PECend

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       PECsetparsedvalues - Sets parsed values
!
! DESCRIPTION
!       Points to an array that was allocated in readdata_mod
!
! SYNOPSIS
!       CALL PECsetparsedvalues(pecs)
!       integer, dimension(:,:),pointer :: pecs 
!
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Lennart Hellström
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE PECsetparsedvalues(pecs)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer(kind=two_byte), dimension(:,:),pointer :: pecs 

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

parsed_pec => pecs
PEC_found = .TRUE.

END SUBROUTINE PECsetparsedvalues

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       sort3 - Sorts on the two first columns of A(n,3)
!
! DESCRIPTION
!       Sorts on the two first columns of A(n,3), with highest priority on the 
!       first column.
! 
! SYNOPSIS
!       CALL sort3(A,n)
!         integer :: n
!         integer, dimension(n,3) :: A
!
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Gunnar Ledfelt
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE sort3(A,n)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer :: n
integer, dimension(n,3) :: A

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: i

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------
 
do i = n/2, 1, -1
  call sift3(i,n,A,n)
end do

do i = 1, n
  call swap3(1,n-i+1,A,n)
  call sift3(1,n-i,A,n)
end do

END SUBROUTINE sort3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       swap3 - swap A(i,1:3) and A(j,1:3)
!
! DESCRIPTION
!       swap A(i,1:3) and A(j,1:3)
!
! SYNOPSIS
!       CALL swap3(i,j,A,n)
!         integer :: i, j, n
!         integer, dimension(n,3) :: A
!
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Gunnar Ledfelt
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE swap3(i,j,A,n)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer :: i, j, n
integer, dimension(n,3) :: A

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: t1, t2, t3
 
!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------
 
t1 = A(i,1)
t2 = A(i,2)
t3 = A(i,3)

A(i,1) = A(j,1)
A(i,2) = A(j,2)
A(i,3) = A(j,3)

A(j,1) = t1
A(j,2) = t2
A(j,3) = t3

END SUBROUTINE swap3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       sift3 - TBC
!
! DESCRIPTION
!       TBC
!
! SYNOPSIS
!       CALL sift3(start,stop,A,n)
!         integer :: start, stop, n
!         integer, dimension(n,3) :: A
!
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Gunnar Ledfelt
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE sift3(start,stop,A,n)

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer :: start, stop, n
integer, dimension(n,3) :: A

!------------------------------------------------------------------------------
!                     L o c a l  v a r i a b l e s
!------------------------------------------------------------------------------

integer :: lstart
integer :: min

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------
 
lstart = start

do while (2*lstart <= stop)
  min = 2*lstart
  if( (min+1) <= stop ) then              ! if there are two children
    if( largerthan3(min+1,min,A,n) ) then ! choose the largest of them
      min = min+1
    end if
  end if
  !! Now we got the index for (the smallest) child
  if( largerthan3(min,lstart,A,n) ) then  ! if the parent is larger
    call swap3(min,lstart,A,n)            ! than the largest child
    lstart = min                          ! swap them and flag this
  else                                    ! downsifted value as candidate
    return                                ! for another sift.
  end if                                  ! if not, return
end do

END SUBROUTINE sift3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+
! NAME
!       largerthan3 -  if A(i,1:3) > A(j,1:3) largerthan is true
!
! DESCRIPTION
!       if A(i,1:3) > A(j,1:3) largerthan is true
!
! METHOD
!       if A(i,1)>A(j,1) OR ( A(i,1)==A(j,1) AND A(i,2)>A(j,2) ) THEN
!         largerthan3 is TRUE
!
! SYNOPSIS
!       FUNCTION largerthan3(i,j,A,n)
!         integer :: i, j, n
!         integer, dimension(n,3) :: A
!
! RETURN VALUES
!       logical :: largerthan3
!
! ERRORS
!       No error handling
!
! HISTORY
!       Written by Gunnar Ledfelt
!-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION largerthan3(i,j,A,n)  ! if A(i) > A(j) largerthan is true

!------------------------------------------------------------------------------
!                     A r g u m e n t s             
!------------------------------------------------------------------------------

integer :: i, j, n
integer, dimension(n,3) :: A

!------------------------------------------------------------------------------
!                     R e t u r n  t y p e
!------------------------------------------------------------------------------

logical :: largerthan3

!------------------------------------------------------------------------------
!                     E x e c u t a b l e   c o d e
!------------------------------------------------------------------------------

if ( A(i,1) > A(j,1) ) then       ! if first part of key largest
  largerthan3 = .true.
elseif ( A(i,1) < A(j,1) ) then   ! if first part of key smaller
  largerthan3 = .false.
else                              ! if first part of key equal
  if ( A(i,2) > A(j,2) ) then     ! if second part of key largest
    largerthan3 = .true.
  else                            ! if second part of key not largest
    largerthan3 = .false.
  end if
end if

END FUNCTION largerthan3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE PEC_mod
