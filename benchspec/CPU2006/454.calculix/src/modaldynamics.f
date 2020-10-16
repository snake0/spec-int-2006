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
      subroutine modaldynamics(text,textpart,nmethod,tinc,tper,iexpl,
     &  istep,istat,in,n)
!
!     reading the input deck: *MODAL DYNAMIC
!
      implicit none
!
      character*40 textpart(16)
      character*132 text
!
      integer nmethod,istep,istat,in,n,key,iexpl
!
      real*8 tinc,tper
!
      iexpl=0
!
      if(istep.lt.1) then
         write(*,*) '*ERROR in modaldynamics: *MODAL DYNAMIC can only'
         write(*,*) '  be used within a STEP'
         stop
      endif
!
      call getnewline(text,textpart,istat,in,n,key)
      if((istat.lt.0).or.(key.eq.1)) then
         write(*,*) '*ERROR in modaldynamics: definition not complete'
         write(*,*) '       card image:'
         write(*,'(a132)') text
         stop
      endif
      read(textpart(1),'(f40.0)',iostat=istat)tinc
      if(istat.gt.0) call inputerror(text)
      read(textpart(2),'(f40.0)',iostat=istat)tper
      if(istat.gt.0) call inputerror(text)
!
      nmethod=4
!
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end

