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
      subroutine matdata_co(cocon,ncocon,imat,iorien,pgauss,orab,ntmat_,
     &  coconloc,mattyp,t1l)
!
      implicit none
!
!     determines the conductivity in an integration point with
!     coordinates pgauss
!
      integer ncocon(2,*),imat,iorien,j,k,mattyp,kal(2,6),
     &  j1,j2,j3,j4,jj,ntmat_,
     &  id,seven
!
      real*8 cocon(0:6,ntmat_,*),orab(7,*),coconloc(6),t1l,
     &  skl(3,3),xa(3,3),pgauss(3)
!
      data kal /1,1,2,2,3,3,1,2,1,3,2,3/
!
      seven=7
!
!
!     calculating the conductivity coefficients
!
      call ident2(cocon(0,1,imat),t1l,ncocon(2,imat),seven,id)
      if(ncocon(2,imat).eq.0) then
         do k=1,6
            coconloc(k)=0.d0
         enddo
         continue
      elseif(ncocon(2,imat).eq.1) then
         do k=1,ncocon(1,imat)
            coconloc(k)=cocon(k,1,imat)
         enddo
      elseif(id.eq.0) then
         do k=1,ncocon(1,imat)
            coconloc(k)=cocon(k,1,imat)
         enddo
      elseif(id.eq.ncocon(2,imat)) then
         do k=1,ncocon(1,imat)
            coconloc(k)=cocon(k,id,imat)
         enddo
      else
         do k=1,ncocon(1,imat)
            coconloc(k)=(cocon(k,id,imat)+
     &           (cocon(k,id+1,imat)-cocon(k,id,imat))/
     &           (cocon(0,id+1,imat)-cocon(0,id,imat)))
     &           
         enddo
      endif
!
!     determining the type: isotropic, orthotropic or anisotropic
!
      if(ncocon(1,imat).le.1) then
         mattyp=1
      elseif(ncocon(1,imat).le.3) then
         mattyp=2
      else
         mattyp=3
      endif
!
!     transformation due to special orientation
!     only allowed for linear elastic materials
!
      if((iorien.eq.0).or.(mattyp.eq.1)) return
!
!     calculating the transformation matrix
!
      call transformatrix(orab(1,iorien),pgauss,skl)
!
!     modifying the conductivity constants
!
         if(mattyp.eq.2) then
            do j=4,6
               coconloc(j)=0.d0
            enddo
         endif
!
         xa(1,1)=coconloc(1)
         xa(1,2)=coconloc(2)
         xa(1,3)=coconloc(4)
         xa(2,1)=coconloc(2)
         xa(2,2)=coconloc(3)
         xa(2,3)=coconloc(5)
         xa(3,1)=coconloc(4)
         xa(3,2)=coconloc(5)
         xa(3,3)=coconloc(6)
!
         do jj=1,6
            coconloc(jj)=0.d0
            j1=kal(1,jj)
            j2=kal(2,jj)
            do j3=1,3
               do j4=1,3
                  coconloc(jj)=coconloc(jj)+
     &                 xa(j3,j4)*skl(j1,j3)*skl(j2,j4)
               enddo
            enddo
         enddo
!
      mattyp=3
!
      return
      end



