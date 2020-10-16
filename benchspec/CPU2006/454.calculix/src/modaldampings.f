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
      subroutine modaldampings(text,textpart,nmethod,alpham,betam,istep,
     &  istat,in,n)
!
!     reading the input deck: *MODAL DAMPING
!
      implicit none
!
      character*40 textpart(16)
      character*132 text
!
      integer nmethod,istep,istat,in,n,key
!
      real*8 alpham,betam
!
      if(istep.lt.1) then
         write(*,*) '*ERROR in modaldampings: *MODAL DAMPING can only'
         write(*,*) '  be used within a STEP'
         stop
      endif
!
      call getnewline(text,textpart,istat,in,n,key)
      if((istat.lt.0).or.(key.eq.1)) then
         write(*,*) '*ERROR in modaldampings: definition not complete'
         write(*,*) '       card image:'
         write(*,'(a132)') text
         stop
      endif
      read(textpart(1),'(f40.0)',iostat=istat) alpham
      if(istat.gt.0) call inputerror(text)
      read(textpart(2),'(f40.0)',iostat=istat) betam
      if(istat.gt.0) call inputerror(text)
!
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end

