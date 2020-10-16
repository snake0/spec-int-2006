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
      subroutine depvars(text,textpart,nelcon,nmat,
     &        nstate_,istep,istat,in,n)
!
!     reading the input deck: *CREEP
!
      implicit none
!
      integer nelcon(2,*),nmat,istep,nstate_,
     &  in,n,key,istat,nstate
!
      character*40 textpart(16)
      character*132 text
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in depvars: *DEPVAR should be placed'
         write(*,*) '  before all step definitions'
         stop
      endif
!
      if(nmat.eq.0) then
         write(*,*) '*ERROR in depvars: *DEPVAR should be preceded'
         write(*,*) '  by a *MATERIAL card'
         stop
      endif
!
      if(nelcon(1,nmat).gt.-100) then
         write(*,*) '*ERROR in depvars: *DEPVAR should be preceded'
         write(*,*) '  by an *USER MATERIAL card'
         stop
      endif
!
      call getnewline(text,textpart,istat,in,n,key)
      if((istat.lt.0).or.(key.eq.1)) then
         write(*,*) '*ERROR in depvars: incomplete definition'
         stop
      endif
      read(textpart(1),'(i40)',iostat=istat) nstate
      if(istat.gt.0) call inputerror(text)
      nstate_=max(nstate_,nstate)
!
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end

