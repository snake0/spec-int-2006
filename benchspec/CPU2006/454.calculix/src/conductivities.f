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
      subroutine conductivities(text,textpart,cocon,ncocon,
     &  nmat,ntmat_,istep,istat,in,n)
!
!     reading the input deck: *CONDUCTIVITY
!
      implicit none
!
      integer ncocon(2,*),nmat,ntmat,ntmat_,istep,istat,in,n,
     &  i,ityp,key
!
      real*8 cocon(0:6,ntmat_,*)
!
      character*40 textpart(16)
      character*132 text
!
      ntmat=0
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in conductivities: *CONDUCTIVITY should be'
         write(*,*) '  placed before all step definitions'
         stop
      endif
!
      if(nmat.eq.0) then
         write(*,*)'*ERROR IN conductivities: *CONDUCTIVITY should be'
         write(*,*) '  preceded by a *MATERIAL card'
         stop
      endif
!
      ityp=1
!
      do i=2,n
         if(textpart(i)(1:5).eq.'TYPE=') then
            if(textpart(i)(6:8).eq.'ISO') then
               ityp=1
            elseif(textpart(i)(6:10).eq.'ORTHO') then
               ityp=3
            elseif(textpart(i)(6:10).eq.'ANISO') then
               ityp=6
            endif
         endif
      enddo
!
      ncocon(1,nmat)=ityp
!
      if(ityp.eq.1) then
         do
            call getnewline(text,textpart,istat,in,n,key)
            if((istat.lt.0).or.(key.eq.1)) return
            ntmat=ntmat+1
            ncocon(2,nmat)=ntmat
            if(ntmat.gt.ntmat_) then
               write(*,*) '*ERROR in conductivities: increase ntmat_'
               stop
            endif
            do i=1,1
               read(textpart(i),'(f40.0)',iostat=istat)
     &             cocon(i,ntmat,nmat)
               if(istat.gt.0) call inputerror(text)
            enddo
            read(textpart(2),'(f40.0)',iostat=istat) cocon(0,ntmat,nmat)
            if(istat.gt.0) call inputerror(text)
         enddo
      elseif(ityp.eq.3) then
         do
            call getnewline(text,textpart,istat,in,n,key)
            if((istat.lt.0).or.(key.eq.1)) return
            ntmat=ntmat+1
            ncocon(2,nmat)=ntmat
            if(ntmat.gt.ntmat_) then
               write(*,*) '*ERROR in conductivities: increase ntmat_'
               stop
            endif
            do i=1,3
               read(textpart(i),'(f40.0)',iostat=istat)
     &             cocon(i,ntmat,nmat)
               if(istat.gt.0) call inputerror(text)
            enddo
            read(textpart(4),'(f40.0)',iostat=istat) cocon(0,ntmat,nmat)
            if(istat.gt.0) call inputerror(text)
         enddo
      elseif(ityp.eq.6) then
         do
            call getnewline(text,textpart,istat,in,n,key)
            if((istat.lt.0).or.(key.eq.1)) return
            ntmat=ntmat+1
            ncocon(2,nmat)=ntmat
            if(ntmat.gt.ntmat_) then
               write(*,*) '*ERROR in conductivities: increase ntmat_'
               stop
            endif
            do i=1,6
               read(textpart(i),'(f40.0)',iostat=istat)
     &             cocon(i,ntmat,nmat)
               if(istat.gt.0) call inputerror(text)
            enddo
            read(textpart(7),'(f40.0)',iostat=istat) cocon(0,ntmat,nmat)
            if(istat.gt.0) call inputerror(text)
         enddo
      endif
!
      return
      end

