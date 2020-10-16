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
      subroutine orientations(text,textpart,orname,orab,norien,
     &  norien_,istep,istat,in,n)
!
!     reading the input deck: *ORIENTATION
!
      implicit none
!
      real*8 orab(7,*)
!
      character*20 orname(*)
      character*40 textpart(16)
      character*132 text
!
      integer norien,norien_,istep,istat,in,n,key,i
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in orientations: *ORIENTATION should be'
         write(*,*) '  placed before all step definitions'
         stop
      endif
!
      norien=norien+1
      if(norien.gt.norien_) then
         write(*,*) '*ERROR in orientations: increase norien_'
         stop
      endif
!
!     rectangular coordinate system: orab(7,norien)=1
!     cylindrical coordinate system: orab(7,norien)=-1
!     default is rectangular
!
      orab(7,norien)=1.d0
!
      do i=2,n
         if(textpart(i)(1:5).eq.'NAME=') then
            orname(norien)=textpart(i)(6:25)
         elseif(textpart(i)(1:7).eq.'SYSTEM=') then
            if(textpart(i)(8:18).eq.'CYLINDRICAL') then
               orab(7,norien)=-1.d0
            endif
         endif
      enddo
!
      call getnewline(text,textpart,istat,in,n,key)
      if((istat.lt.0).or.(key.eq.1)) then
         write(*,*)'*ERROR in orientations: definition of the following'
         write(*,*) '  orientation is not complete: ',orname(norien)
         stop
      endif
!
      do i=1,6
         read(textpart(i),'(f40.0)',iostat=istat) orab(i,norien)
         if(istat.gt.0) call inputerror(text)
      enddo
!
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end

