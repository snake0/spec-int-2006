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
      subroutine loadadd(nelement,label,value,nelemload,sideload,
     &  xload,nload,nload_,iamload,iamplitude,nam)
!
!     adds a dload condition to the data base
!
      implicit none
!
      integer nelemload(2,*),iamload(2,*)
!
      integer nelement,nload,nload_,j,iamplitude,nam
!
      real*8 xload(2,*)
!
      real*8 value
!
      character*5 label,sideload(*)
!
      do j=1,nload
         if((nelemload(1,j).eq.nelement).and.
     &      (sideload(j).eq.label)) then
            xload(1,j)=value
            if(nam.gt.0) iamload(1,j)=iamplitude
            return
         endif
      enddo
      nload=nload+1
      if(nload.gt.nload_) then
         write(*,*) '*ERROR in loadadd: increase nload_'
         stop
      endif
      nelemload(1,nload)=nelement
      sideload(nload)=label
      xload(1,nload)=value
      if(nam.gt.0) iamload(1,nload)=iamplitude
!
      return
      end

