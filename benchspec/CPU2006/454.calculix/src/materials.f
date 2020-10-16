!
!     CalculiX - A 3-dimensional finite element program
!              Copyright (C) 1998 Guido Dhondt
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation(version 2);
!     
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of 
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program; if not, write to the Free Software
!     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
      subroutine materials(text,textpart,matname,nmat,nmat_,
     &  istep,istat,in,n)
!
!     reading the input deck: *MATERIAL
!
      implicit none
!
      character*20 matname(*)
      character*40 textpart(16)
      character*132 text
!
      integer nmat,nmat_,istep,istat,in,n,key,i
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in materials: *MATERIAL should be placed'
         write(*,*) '  before all step definitions'
         stop
      endif
!
      nmat=nmat+1
      if(nmat.gt.nmat_) then
         write(*,*) '*ERROR in materials: increase nmat_'
         stop
      endif
!
      do i=2,n
         if(textpart(i)(1:5).eq.'NAME=') then
            matname(nmat)=textpart(i)(6:25)
            exit
         endif
      enddo
!
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end

