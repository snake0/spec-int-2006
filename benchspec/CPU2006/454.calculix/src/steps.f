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
      subroutine steps(text,textpart,iperturb,iprestr,om,bodyf,
     &  nforc,nload,ithermal,t0,t1,nk,istep,istat,in,n,jmax,ctrl)
!
!     reading the input deck: *STEP
!
      implicit none
!
      character*40 textpart(16)
      character*132 text
!
      integer iperturb,nforc,nload,ithermal,nk,istep,istat,in,n,key,
     &  i,j,iprestr,jmax
!
      real*8 t0(*),t1(*),om,bodyf(3),ctrl(26)
!
      if(iperturb.lt.2) iperturb=0
      istep=istep+1
      jmax=10
!
      do i=2,n
         if(textpart(i)(1:12).eq.'PERTURBATION') then
            iperturb=1
!
!           removing the present loading
!
            nforc=0
c            nload=0
            iprestr=0
c            om=0.d0
            bodyf(1)=0.d0
            bodyf(2)=0.d0
            bodyf(3)=0.d0
            if(ithermal.eq.1) then
               do j=1,nk
                  t1(j)=t0(j)
               enddo
            endif
!
         elseif(textpart(i)(1:6).eq.'NLGEOM') then
!
!           geometrically nonlinear calculations
!
            if(iperturb.eq.0) then
               iperturb=2
            elseif(iperturb.eq.1) then
               write(*,*) '*ERROR in steps: PERTURBATION and NLGEOM'
               write(*,*) '       are mutually exclusive; card image:'
               write(*,'(a132)') text
               stop
            endif
!
!          to ensure linear calculations for 1d and 2d elements and 
!          for nonlinear MPCs, the
!          convergence criteria were set extremely high. If nonlinear
!          calculations are requested, these criteria must be reset
!
            if(ctrl(19).eq.1.d+30) then
               ctrl(19)=0.005
               ctrl(20)=0.01
            endif
!
         elseif(textpart(i)(1:4).eq.'INC=') then
!
!           maximum number of increments
!
            read(textpart(i)(5:40),'(i36)',iostat=istat) jmax
            if(istat.gt.0) call inputerror(text)
         endif
      enddo
!
!     to ensure linear calculations for 1d and 2d elements and 
!     for nonlinear MPCs, the
!     convergence criteria were set extremely high. If nonlinear
!     calculations are requested, these criteria must be reset
!
      if((iperturb.eq.3).and.(ctrl(19).eq.1.d+30)) then
         ctrl(19)=0.005
         ctrl(20)=0.01
      endif
!
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end




