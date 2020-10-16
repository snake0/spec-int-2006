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
      subroutine shape20h(xi,et,ze,xl,xsj,shp)
!
!     shape functions and derivatives for a 20-node quadratic
!     isoparametric brick element. Watch out: 0<=xi<=2, -1<=et,ze<=1 !!!
!
      implicit none
!
      integer i,j,k
!
      real*8 shp(4,20),xs(3,3),xsi(3,3),xl(3,20),sh(3)
!
      real*8 xi,et,ze,xsj,omg,omh,omr,opg,oph,opr,
     &  tpgphpr,tmgphpr,tmgmhpr,tpgmhpr,tpgphmr,tmgphmr,tmgmhmr,tpgmhmr
!
!     shape functions and their glocal derivatives
!
      omg=1.d0-xi
      omh=1.d0-et
      omr=1.d0-ze
      opg=1.d0+xi
      oph=1.d0+et
      opr=1.d0+ze
      tpgphpr=2.d0+xi+et+ze
      tmgphpr=2.d0-xi+et+ze
      tmgmhpr=2.d0-xi-et+ze
      tpgmhpr=2.d0+xi-et+ze
      tpgphmr=2.d0+xi+et-ze
      tmgphmr=2.d0-xi+et-ze
      tmgmhmr=2.d0-xi-et-ze
      tpgmhmr=2.d0+xi-et-ze
!
!     local derivatives of the shape functions: xi-derivative
!
      shp(1, 1)=omh*omr*(tpgphpr-omg)/8.d0
      shp(1, 2)=(opg-tmgphpr)*omh*omr/8.d0
      shp(1, 3)=(opg-tmgmhpr)*oph*omr/8.d0
      shp(1, 4)=oph*omr*(tpgmhpr-omg)/8.d0
      shp(1, 5)=omh*opr*(tpgphmr-omg)/8.d0
      shp(1, 6)=(opg-tmgphmr)*omh*opr/8.d0
      shp(1, 7)=(opg-tmgmhmr)*oph*opr/8.d0
      shp(1, 8)=oph*opr*(tpgmhmr-omg)/8.d0
      shp(1, 9)=(omg-opg)*omh*omr/4.d0
      shp(1,10)=omh*oph*omr/4.d0
      shp(1,11)=(omg-opg)*oph*omr/4.d0
      shp(1,12)=-omh*oph*omr/4.d0
      shp(1,13)=(omg-opg)*omh*opr/4.d0
      shp(1,14)=omh*oph*opr/4.d0
      shp(1,15)=(omg-opg)*oph*opr/4.d0
      shp(1,16)=-omh*oph*opr/4.d0
      shp(1,17)=-omr*opr*omh/4.d0
      shp(1,18)=omr*opr*omh/4.d0
      shp(1,19)=omr*opr*oph/4.d0
      shp(1,20)=-omr*opr*oph/4.d0
!
!     local derivatives of the shape functions: eta-derivative
!
      shp(2, 1)=omg*omr*(tpgphpr-omh)/8.d0
      shp(2, 2)=opg*omr*(tmgphpr-omh)/8.d0
      shp(2, 3)=opg*(oph-tmgmhpr)*omr/8.d0
      shp(2, 4)=omg*(oph-tpgmhpr)*omr/8.d0
      shp(2, 5)=omg*opr*(tpgphmr-omh)/8.d0
      shp(2, 6)=opg*opr*(tmgphmr-omh)/8.d0
      shp(2, 7)=opg*(oph-tmgmhmr)*opr/8.d0
      shp(2, 8)=omg*(oph-tpgmhmr)*opr/8.d0
      shp(2, 9)=-omg*opg*omr/4.d0
      shp(2,10)=(omh-oph)*opg*omr/4.d0
      shp(2,11)=omg*opg*omr/4.d0
      shp(2,12)=(omh-oph)*omg*omr/4.d0
      shp(2,13)=-omg*opg*opr/4.d0
      shp(2,14)=(omh-oph)*opg*opr/4.d0
      shp(2,15)=omg*opg*opr/4.d0
      shp(2,16)=(omh-oph)*omg*opr/4.d0
      shp(2,17)=-omr*opr*omg/4.d0
      shp(2,18)=-omr*opr*opg/4.d0
      shp(2,19)=omr*opr*opg/4.d0
      shp(2,20)=omr*opr*omg/4.d0
!
!     local derivatives of the shape functions: zeta-derivative
!
      shp(3, 1)=omg*omh*(tpgphpr-omr)/8.d0
      shp(3, 2)=opg*omh*(tmgphpr-omr)/8.d0
      shp(3, 3)=opg*oph*(tmgmhpr-omr)/8.d0
      shp(3, 4)=omg*oph*(tpgmhpr-omr)/8.d0
      shp(3, 5)=omg*omh*(opr-tpgphmr)/8.d0
      shp(3, 6)=opg*omh*(opr-tmgphmr)/8.d0
      shp(3, 7)=opg*oph*(opr-tmgmhmr)/8.d0
      shp(3, 8)=omg*oph*(opr-tpgmhmr)/8.d0
      shp(3, 9)=-omg*opg*omh/4.d0
      shp(3,10)=-omh*oph*opg/4.d0
      shp(3,11)=-omg*opg*oph/4.d0
      shp(3,12)=-omh*oph*omg/4.d0
      shp(3,13)=omg*opg*omh/4.d0
      shp(3,14)=omh*oph*opg/4.d0
      shp(3,15)=omg*opg*oph/4.d0
      shp(3,16)=omh*oph*omg/4.d0
      shp(3,17)=(omr-opr)*omg*omh/4.d0
      shp(3,18)=(omr-opr)*opg*omh/4.d0
      shp(3,19)=(omr-opr)*opg*oph/4.d0
      shp(3,20)=(omr-opr)*omg*oph/4.d0
!
!     shape functions
!
      shp(4, 1)=-omg*omh*omr*tpgphpr/8.d0
      shp(4, 2)=-opg*omh*omr*tmgphpr/8.d0
      shp(4, 3)=-opg*oph*omr*tmgmhpr/8.d0
      shp(4, 4)=-omg*oph*omr*tpgmhpr/8.d0
      shp(4, 5)=-omg*omh*opr*tpgphmr/8.d0
      shp(4, 6)=-opg*omh*opr*tmgphmr/8.d0
      shp(4, 7)=-opg*oph*opr*tmgmhmr/8.d0
      shp(4, 8)=-omg*oph*opr*tpgmhmr/8.d0
      shp(4, 9)=omg*opg*omh*omr/4.d0
      shp(4,10)=omh*oph*opg*omr/4.d0
      shp(4,11)=omg*opg*oph*omr/4.d0
      shp(4,12)=omh*oph*omg*omr/4.d0
      shp(4,13)=omg*opg*omh*opr/4.d0
      shp(4,14)=omh*oph*opg*opr/4.d0
      shp(4,15)=omg*opg*oph*opr/4.d0
      shp(4,16)=omh*oph*omg*opr/4.d0
      shp(4,17)=omr*opr*omg*omh/4.d0
      shp(4,18)=omr*opr*opg*omh/4.d0
      shp(4,19)=omr*opr*opg*oph/4.d0
      shp(4,20)=omr*opr*omg*oph/4.d0
!
!     computation of the local derivative of the global coordinates
!     (xs)
!
      do i=1,3
        do j=1,3
          xs(i,j)=0.d0
          do k=1,20
            xs(i,j)=xs(i,j)+xl(i,k)*shp(j,k)
          enddo
        enddo
      enddo
!
!     computation of the jacobian determinant
!
      xsj=xs(1,1)*(xs(2,2)*xs(3,3)-xs(2,3)*xs(3,2))
     &   -xs(1,2)*(xs(2,1)*xs(3,3)-xs(2,3)*xs(3,1))
     &   +xs(1,3)*(xs(2,1)*xs(3,2)-xs(2,2)*xs(3,1))
!
!     computation of the global derivative of the local coordinates
!     (xsi) (inversion of xs)
!
      xsi(1,1)=(xs(2,2)*xs(3,3)-xs(3,2)*xs(2,3))/xsj
      xsi(1,2)=(xs(1,3)*xs(3,2)-xs(1,2)*xs(3,3))/xsj
      xsi(1,3)=(xs(1,2)*xs(2,3)-xs(2,2)*xs(1,3))/xsj
      xsi(2,1)=(xs(2,3)*xs(3,1)-xs(2,1)*xs(3,3))/xsj
      xsi(2,2)=(xs(1,1)*xs(3,3)-xs(3,1)*xs(1,3))/xsj
      xsi(2,3)=(xs(1,3)*xs(2,1)-xs(1,1)*xs(2,3))/xsj
      xsi(3,1)=(xs(2,1)*xs(3,2)-xs(3,1)*xs(2,2))/xsj
      xsi(3,2)=(xs(1,2)*xs(3,1)-xs(1,1)*xs(3,2))/xsj
      xsi(3,3)=(xs(1,1)*xs(2,2)-xs(2,1)*xs(1,2))/xsj
!
!     computation of the global derivatives of the shape functions
!
      do k=1,20
        do j=1,3
          sh(j)=shp(1,k)*xsi(1,j)+shp(2,k)*xsi(2,j)+shp(3,k)*xsi(3,j)
        enddo
        do j=1,3
          shp(j,k)=sh(j)
        enddo
      enddo
c      open(20,file='output.dat',status='unknown')
c      write(20,*) xi,et,ze
c      do i=1,4
c         do j=1,20
c            write(20,'(2i5,e11.4)') i,j,shp(i,j)
c         enddo
c      enddo
!
      return
      end
