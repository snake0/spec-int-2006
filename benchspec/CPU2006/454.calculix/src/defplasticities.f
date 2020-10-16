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
      subroutine defplasticities(text,textpart,elcon,nelcon,
     &  nmat,ntmat_,ncmat_,istep,istat,in,n,iperturb)
!
!     reading the input deck: *DEFORMATION PLASTICITY
!
      implicit none
!
      integer nelcon(2,*),nmat,ntmat,ntmat_,istep,istat,in,
     &  n,key,i,iperturb,iend,ncmat_
!
      real*8 elcon(0:ncmat_,ntmat_,*)
!
      character*40 textpart(16)
      character*132 text
!
      ntmat=0
      iperturb=3
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in defplasticities: *DEFORMATION PLASTICITY'
         write(*,*) '  should be placed before all step definitions'
         stop
      endif
!
      if(nmat.eq.0) then
         write(*,*) '*ERROR in defplasticities: *DEFORMATION PLASTICITY'
         write(*,*) '  should bepreceded by a *MATERIAL card'
         stop
      endif
!
      nelcon(1,nmat)=-50
!
      iend=5
      do
         call getnewline(text,textpart,istat,in,n,key)
         if((istat.lt.0).or.(key.eq.1)) return
         ntmat=ntmat+1
         nelcon(2,nmat)=ntmat
         if(ntmat.gt.ntmat_) then
            write(*,*) '*ERROR in defplasticities: increase ntmat_'
            stop
         endif
         do i=1,iend
            read(textpart(i),'(f40.0)',iostat=istat) elcon(i,ntmat,nmat)
            if(istat.gt.0) call inputerror(text)
         enddo
         read(textpart(6),'(f40.0)',iostat=istat) elcon(0,ntmat,nmat)
         if(istat.gt.0) call inputerror(text)
      enddo
!
      return
      end

