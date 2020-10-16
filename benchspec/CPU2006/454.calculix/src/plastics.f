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
      subroutine plastics(text,textpart,nelcon,nmat,ntmat_,npmat_,
     &        plicon,nplicon,plkcon,nplkcon,iplas,iperturb,nstate_,
     &        istep,istat,in,n)
!
!     reading the input deck: *PLASTIC
!
      implicit none
!
      logical iso
!
      integer nelcon(2,*),nmat,ntmat_,ntmat,npmat_,npmat,istep,
     &  in,n,key,i,nplicon(0:ntmat_,*),nplkcon(0:ntmat_,*),
     &  iplas,iperturb,istat,nstate_
!
      real*8 plicon(0:2*npmat_,ntmat_,*),plkcon(0:2*npmat_,ntmat_,*),
     & temperature
!
      character*40 textpart(16)
      character*132 text
!
      iso=.true.
!
      ntmat=0
      npmat=0
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in plastics: *PLASTIC should be placed'
         write(*,*) '  before all step definitions'
         stop
      endif
!
      if(nmat.eq.0) then
         write(*,*) '*ERROR in plastics: *PLASTIC should be preceded'
         write(*,*) '  by a *MATERIAL card'
         stop
      endif
!
      if(nelcon(1,nmat).ne.2) then
         write(*,*) '*ERROR in plastics: *PLASTIC should be preceded'
         write(*,*) '  by an *ELASTIC,TYPE=ISO card'
         stop
      endif
!
      iperturb=3
      iplas=1
      nelcon(1,nmat)=-51
      nstate_=max(nstate_,13)
!
      do i=2,n
         if(textpart(i)(1:10).eq.'HARDENING=') then
            if(textpart(i)(11:19).eq.'KINEMATIC') then
               iso=.false.
            elseif(textpart(i)(11:18).eq.'COMBINED') then
               iso=.false.
            elseif(textpart(i)(11:14).eq.'USER') then
               call getnewline(text,textpart,istat,in,n,key)
               return
            endif
            exit
         endif
      enddo
!
      if(iso) then
!
!        isotropic hardening coefficients
!
         do
            call getnewline(text,textpart,istat,in,n,key)
            if((istat.lt.0).or.(key.eq.1)) return
            read(textpart(3),'(f40.0)',iostat=istat) temperature
            if(istat.gt.0) call inputerror(text)
!
!           first temperature
!
            if(ntmat.eq.0) then
               npmat=0
               ntmat=ntmat+1
               if(ntmat.gt.ntmat_) then
                  write(*,*) '*ERROR in plastics: increase ntmat_'
                  stop
               endif
               nplicon(0,nmat)=ntmat
               plicon(0,ntmat,nmat)=temperature
!
!           new temperature
!
            elseif(plicon(0,ntmat,nmat).ne.temperature) then
               npmat=0
               ntmat=ntmat+1
               if(ntmat.gt.ntmat_) then
                  write(*,*) '*ERROR in plastics: increase ntmat_'
                  stop
               endif
               nplicon(0,nmat)=ntmat
               plicon(0,ntmat,nmat)=temperature
            endif
            do i=1,2
               read(textpart(i),'(f40.0)',iostat=istat) 
     &              plicon(2*npmat+i,ntmat,nmat)
               if(istat.gt.0) call inputerror(text)
            enddo
            npmat=npmat+1
            if(npmat.gt.npmat_) then
               write(*,*) '*ERROR in plastics: increase npmat_'
               stop
            endif
            nplicon(ntmat,nmat)=npmat
         enddo
      else
!
!        kinematic hardening coefficients
!
         do
            call getnewline(text,textpart,istat,in,n,key)
            if((istat.lt.0).or.(key.eq.1)) return
            read(textpart(3),'(f40.0)',iostat=istat) temperature
            if(istat.gt.0) call inputerror(text)
!
!           first temperature
!
            if(ntmat.eq.0) then
               npmat=0
               ntmat=ntmat+1
               if(ntmat.gt.ntmat_) then
                  write(*,*) '*ERROR in plastics: increase ntmat_'
                  stop
               endif
               nplkcon(0,nmat)=ntmat
               plkcon(0,ntmat,nmat)=temperature
!
!           new temperature
!
            elseif(plkcon(0,ntmat,nmat).ne.temperature) then
               npmat=0
               ntmat=ntmat+1
               if(ntmat.gt.ntmat_) then
                  write(*,*) '*ERROR in plastics: increase ntmat_'
                  stop
               endif
               nplkcon(0,nmat)=ntmat
               plkcon(0,ntmat,nmat)=temperature
            endif
            do i=1,2
               read(textpart(i),'(f40.0)',iostat=istat) 
     &              plkcon(2*npmat+i,ntmat,nmat)
               if(istat.gt.0) call inputerror(text)
            enddo
            npmat=npmat+1
            if(npmat.gt.npmat_) then
               write(*,*) '*ERROR in plastics: increase npmat_'
               stop
            endif
            nplkcon(ntmat,nmat)=npmat
         enddo
      endif
!
      return
      end

