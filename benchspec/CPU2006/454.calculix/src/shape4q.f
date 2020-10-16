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
      subroutine shape4q(xi,et,xl,xsj,shp)
!
      implicit none
!
      integer i,j,k
!
      real*8 shp(4,8),xs(3,2),xsi(2,3),xl(3,8),sh(3),xsj(3)
!
      real*8 xi,et
!
!     shape functions and their glocal derivatives for an element
!     described with two local parameters and three global ones.
!
!     local derivatives of the shape functions: xi-derivative
!      
      shp(1,1)=-(1.d0-et)/4.d0
      shp(1,2)=(1.d0-et)/4.d0
      shp(1,3)=(1.d0+et)/4.d0
      shp(1,4)=-(1.d0+et)/4.d0
!
!     local derivatives of the shape functions: eta-derivative
!
      shp(2,1)=-(1.d0-xi)/4.d0
      shp(2,2)=-(1.d0+xi)/4.d0
      shp(2,3)=(1.d0+xi)/4.d0
      shp(2,4)=(1.d0-xi)/4.d0
!
!     shape functions
!
      shp(4,1)=(1.d0-xi)*(1.d0-et)/4.d0
      shp(4,2)=(1.d0+xi)*(1.d0-et)/4.d0
      shp(4,3)=(1.d0+xi)*(1.d0+et)/4.d0
      shp(4,4)=(1.d0-xi)*(1.d0+et)/4.d0
!
!     computation of the local derivative of the global coordinates
!     (xs)
!
      do i=1,3
        do j=1,2
          xs(i,j)=0.d0
          do k=1,4
            xs(i,j)=xs(i,j)+xl(i,k)*shp(j,k)
          enddo
        enddo
      enddo
!
!     computation of the jacobian determinant
!
      xsj(1)=xs(2,1)*xs(3,2)-xs(3,1)*xs(2,2)
      xsj(2)=xs(1,2)*xs(3,1)-xs(3,2)*xs(1,1)
      xsj(3)=xs(1,1)*xs(2,2)-xs(2,1)*xs(1,2)
!
!     computation of the global derivative of the local coordinates
!     (xsi) (inversion of xs)
!
      xsi(1,1)=xs(2,2)/xsj(3)
      xsi(2,1)=-xs(2,1)/xsj(3)
      xsi(1,2)=-xs(1,2)/xsj(3)
      xsi(2,2)=xs(1,1)/xsj(3)
      xsi(1,3)=-xs(2,2)/xsj(1)
      xsi(2,3)=xs(2,1)/xsj(1)
!
!     computation of the global derivatives of the shape functions
!
      do k=1,4
        do j=1,3
          sh(j)=shp(1,k)*xsi(1,j)+shp(2,k)*xsi(2,j)
        enddo
        do j=1,3
          shp(j,k)=sh(j)
        enddo
      enddo
!
      return
      end
