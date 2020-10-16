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
      subroutine buckles(text,textpart,nmethod,nev,tol,ncv,mxiter,
     &  nforc,nload,ithermal,iprestr,om,bodyf,t0,t1,nk,iperturb,
     &  istep,istat,in,n)
!
!     reading the input deck: *BUCKLE
!
      implicit none
!
      character*40 textpart(16)
      character*132 text
!
      integer nmethod,nev,istep,istat,in,n,key,ncv,mxiter,
     &  nforc,nload,ithermal,iprestr,i,nk,iperturb
!
      real*8 tol,om,bodyf(3),t0(*),t1(*)
!
      if(istep.lt.1) then
         write(*,*) '*ERROR in buckles: *BUCKLE can only be used'
         write(*,*) '  within a STEP'
         stop
      endif
!
      nmethod=3
      if(iperturb.gt.1) iperturb=0
!
      call getnewline(text,textpart,istat,in,n,key)
      if((istat.lt.0).or.(key.eq.1)) then
         write(*,*) '*ERROR in buckles: definition not complete'
         write(*,*) '  Card image:'
         write(*,'(a132)') text
         stop
      endif
      read(textpart(1),'(i40)',iostat=istat) nev
      if(istat.gt.0) call inputerror(text)
      if(nev.le.0) then
         write(*,*) '*ERROR in buckles: less than 1 eigenvalue re
     &quested'
         stop
      endif
      read(textpart(2),'(f40.10)',iostat=istat) tol
      if(istat.gt.0) call inputerror(text)
      if(tol.le.0.) then
         tol=1.d-2
      endif
      read(textpart(3),'(i40)',iostat=istat) ncv
      if(istat.gt.0) call inputerror(text)
      if(ncv.le.0) then
         ncv=4*nev
      endif
      ncv=ncv+nev
      read(textpart(4),'(i40)',iostat=istat) mxiter
      if(istat.gt.0) call inputerror(text)
      if(mxiter.le.0) then
         mxiter=1000
      endif
!
!     removing the natural boundary conditions
!
      nforc=0
      nload=0
      iprestr=0
      om=0.d0
      bodyf(1)=0.d0
      bodyf(2)=0.d0
      bodyf(3)=0.d0
      if(ithermal.eq.1) then
         do i=1,nk
            t1(i)=t0(i)
         enddo
      endif
!
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end


