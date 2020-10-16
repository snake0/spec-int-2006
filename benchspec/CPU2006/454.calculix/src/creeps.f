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
      subroutine creeps(text,textpart,nelcon,nmat,ntmat_,npmat_,
     &        plicon,nplicon,elcon,iplas,iperturb,nstate_,ncmat_,
     &        matname,istep,istat,in,n)
!
!     reading the input deck: *CREEP
!
      implicit none
!
      logical iso
!
      character*20 matname(*)
!
      integer nelcon(2,*),nmat,ntmat_,ntmat,istep,npmat_,nstate_,
     &  in,n,key,i,j,iplas,iperturb,istat,nplicon(0:ntmat_,*),ncmat_
!
      real*8 temperature,elcon(0:ncmat_,ntmat_,*),
     &  plicon(0:2*npmat_,ntmat_,*)
!
      character*40 textpart(16)
      character*132 text
!
      iso=.true.
      ntmat=0
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in creeps: *CREEP should be placed'
         write(*,*) '  before all step definitions'
         stop
      endif
!
      if(nmat.eq.0) then
         write(*,*) '*ERROR in creeps: *CREEP should be preceded'
         write(*,*) '  by a *MATERIAL card'
         stop
      endif
!
c      if((nelcon(1,nmat).ne.2).and.(nelcon(1,nmat).ne.-51)) then
c         write(*,*) '*ERROR in creeps: *CREEP should be preceded'
c         write(*,*) '  by an *ELASTIC,TYPE=ISO card'
c         stop
c      endif
!
!     check for anisotropic creep: assumes a ucreep routine
!
      if((nelcon(1,nmat).ne.2).and.(nelcon(1,nmat).ne.-51)) then
         if(nelcon(1,nmat).ne.9) then
            write(*,*) '*ERROR in creeps: *CREEP should be preceded'
            write(*,*) '       by an *ELASTIC,TYPE=ISO card,'
            write(*,*) '       or an *ELASTIC,TYPE=ORTHO card'
            stop
         endif
         iperturb=3
         nstate_=max(nstate_,7)
         do i=2,n
            if(textpart(i)(1:8).eq.'LAW=USER') then
               nelcon(1,nmat)=-109
               exit
            endif
         enddo
         if(nelcon(1,nmat).ne.-109) then
            write(*,*) '*ERROR in creeps: for orthotropic materials'
            write(*,*) '       LAW=USER must be specified on the'
            write(*,*) '       *CREEP card'
            stop
         endif
         if(matname(nmat)(10:20).ne.'           ') then
            write(*,*) '*ERROR in creeps: the material name for an'
            write(*,*) '       elastically anisotropic material with'
            write(*,*) '       isotropic creep must not exceed 9'
            write(*,*) '       characters'
            stop
         else
            matname(nmat)(12:20)=matname(nmat)(1:9)
            matname(nmat)(1:11)='ANISO_CREEP'
         endif
         return
      endif
!
!     if the *CREEP card is not preceded by a *PLASTIC card, a zero
!     yield surface is assumed
!
      if(nelcon(1,nmat).ne.-51) then
         nplicon(0,nmat)=1
         nplicon(1,nmat)=2
         plicon(0,1,nmat)=0.d0
         plicon(1,1,nmat)=0.d0
         plicon(2,1,nmat)=0.d0
         plicon(3,1,nmat)=0.d0
         plicon(4,1,nmat)=10.d10
      endif
!
      iperturb=3
      iplas=1
      nelcon(1,nmat)=-52
      nstate_=max(nstate_,13)
!
      do i=2,n
         if(textpart(i)(1:8).eq.'LAW=USER') then
            do j=1,nelcon(2,nmat)
               elcon(3,j,nmat)=-1.d0
            enddo
            call getnewline(text,textpart,istat,in,n,key)
            return
         endif
      enddo
!
      do
         call getnewline(text,textpart,istat,in,n,key)
         if((istat.lt.0).or.(key.eq.1)) return
         ntmat=ntmat+1
         if(ntmat.gt.ntmat_) then
            write(*,*) '*ERROR in creeps: creep data must be given'
            write(*,*) '       for the same temperature data points'
            write(*,*) '       as the elastic data; alternatively,'
            write(*,*) '       you can write a creep user routine'
            stop
         endif
         do i=1,2
            read(textpart(i),'(f40.0)',iostat=istat) 
     &          elcon(i+2,ntmat,nmat)
            if(istat.gt.0) call inputerror(text)
         enddo
         if(textpart(3).ne.
     &        '                                        ') then
            read(textpart(3),'(f40.0)',iostat=istat) temperature
            if(istat.gt.0) call inputerror(text)
         else
            temperature=0.d0
         endif
         if(temperature.ne.elcon(0,ntmat,nmat)) then
            write(*,*) '*ERROR in creeps: creep data must be given'
            write(*,*) '       for the same temperature data points'
            write(*,*) '       as the elastic data; alternatively,'
            write(*,*) '       you can write a creep user routine'
            stop
         endif
      enddo
!
      return
      end

