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
      subroutine dynamics(text,textpart,nmethod,iperturb,tinc,tper,
     &  tmin,tmax,idrct,alpha,haftol,iexpl,isolver,istep,istat,in,n)
!
!     reading the input deck: *DYNAMIC
!
!     isolver=0: SPOOLES
!             2: iterative solver with diagonal scaling
!             3: iterative solver with Cholesky preconditioning
!
      implicit none
!
      character*33 solver
      character*40 textpart(16)
      character*132 text
!
      integer nmethod,istep,istat,in,n,key,i,iperturb,idrct,iexpl,
     &  isolver
!
      real*8 tinc,tper,tmin,tmax,alpha,haftol
!
      if(istep.lt.1) then
         write(*,*) '*ERROR in dynamics: *DYNAMIC can only'
         write(*,*) '  be used within a STEP'
         stop
      endif
!
!     only nonlinear analysis allowd for this procedure
!
      if(iperturb.lt.2) iperturb=2
!
!     default values
!
      idrct=0
      alpha=-0.05d0
      haftol=0.005d0
      tmin=0.d0
      tmax=0.d0
!
!     default solver
!
      solver(1:7)='spooles'
      isolver=0
!
      do i=2,n
         if(textpart(i)(1:6).eq.'ALPHA=') then
            read(textpart(i)(7:40),'(f34.0)',iostat=istat) alpha
            if(istat.gt.0) call inputerror(text)
            if(alpha.lt.-1.d0/3.d0) then
               write(*,*) '*WARNING in dynamics: alpha is smaller'
               write(*,*) '  than -1/3 and is reset to -1/3'
               alpha=-1.d0/3.d0
            elseif(alpha.gt.0.d0) then
               write(*,*) '*WARNING in dynamics: alpha is greater'
               write(*,*) '  than 0 and is reset to 0'
               alpha=0.d0
            endif
!
         elseif(textpart(i)(1:7).eq.'HAFTOL=') then
            read(textpart(i)(8:40),'(f33.0)',iostat=istat) haftol
            if(haftol.lt.0.d0) then
               write(*,*) '*WARNING in dynamics: haftol must be'
               write(*,*) '  positive; default value haftol=0.005'
               write(*,*) '  is used'
               haftol=0.005d0
            endif
!
         elseif(textpart(i)(1:8).eq.'EXPLICIT') then
            iexpl=1
         elseif(textpart(i)(1:6).eq.'DIRECT') then
            idrct=1
         elseif(textpart(i)(1:7).eq.'SOLVER=') then
            read(textpart(i)(8:40),'(a33)') solver
         endif
      enddo
!
      if(solver(1:16).eq.'ITERATIVESCALING') then
         isolver=2
      elseif(solver(1:17).eq.'ITERATIVECHOLESKY') then
         isolver=3
      endif
!
      call getnewline(text,textpart,istat,in,n,key)
      if((istat.lt.0).or.(key.eq.1)) then
         if(iperturb.ge.2) then
            write(*,*)'*WARNING in dynamics: a nonlinear geometric analy
     &sis is requested'
            write(*,*) '         but no time increment nor step is speci
     &fied'
            write(*,*) '         the defaults (1,1) are used'
            tinc=1.d0
            tper=1.d0
            tmin=1.d-5
            tmax=1.d+30
         endif
         return
      endif
!
      read(textpart(1),'(f40.0)',iostat=istat) tinc
      if(istat.gt.0) call inputerror(text)
      read(textpart(2),'(f40.0)',iostat=istat) tper
      if(istat.gt.0) call inputerror(text)
      read(textpart(3),'(f40.0)',iostat=istat) tmin
      if(istat.gt.0) call inputerror(text)
      read(textpart(4),'(f40.0)',iostat=istat) tmax
      if(istat.gt.0) call inputerror(text)
!
      if(tinc.le.0.d0) then
         write(*,*)'*ERROR in dynamics: initial increment size is negati
     &ve'
      endif
      if(tper.le.0.d0) then
         write(*,*) '*ERROR in dynamics: step size is negative'
      endif
      if(tinc.gt.tper) then
         write(*,*)'*ERROR in dynamics: initial increment size exceeds s
     &tep size'
      endif
!      
      if(idrct.ne.1) then
         if(dabs(tmin).lt.1.d-10) then
            tmin=min(tinc,1.d-5*tper)
         endif
         if(dabs(tmax).lt.1.d-10) then
            tmax=1.d+30
         endif
      endif
!
      nmethod=4
!
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end

