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
      subroutine physicalconstants(text,textpart,physcon,
     &  istep,istat,in,n)
!
!     reading the input deck: *PHYSICAL CONSTANTS
!
      implicit none
!
      character*40 textpart(16)
      character*132 text
!
      integer i,istep,istat,in,n,key
!
      real*8 physcon(2)
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in physicalconstants: *PHYSICAL CONSTANTS'
         write(*,*) '        should only be used before the first STEP'
         stop
      endif
!
      do i=2,n
         if(textpart(i)(1:13).eq.'ABSOLUTEZERO=') then
            read(textpart(i)(14:40),'(f27.0)',iostat=istat) physcon(1)
            if(istat.gt.0) call inputerror(text)
         elseif(textpart(i)(1:16).eq.'STEFANBOLTZMANN=') then
            read(textpart(i)(17:40),'(f24.0)',iostat=istat) physcon(1)
            if(istat.gt.0) call inputerror(text)
         endif
      enddo
!
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end







