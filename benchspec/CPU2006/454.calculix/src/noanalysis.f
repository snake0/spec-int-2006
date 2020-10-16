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
      subroutine noanalysis(text,textpart,nmethod,iperturb,istep,
     &  istat,in,n)
!
!     reading the input deck: *NO ANALYSIS
!
      implicit none
!
      character*40 textpart(16)
      character*132 text
!
      integer nmethod,iperturb,istep,istat,in,n,key
!
      if(istep.lt.1) then
         write(*,*)'*ERROR in noanalysis: *NO ANALYSIS can only be used'
         write(*,*) '  within a STEP'
         stop
      endif
!
      write(*,*) '*WARNING: no analysis option was chosen'
!
      nmethod=0
      iperturb=0
!
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end

