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
      subroutine umpc_user(x,u,f,a,jdof,n)
!
!     updates the coefficients in a user mpc
!
!     INPUT:
!
!     x(3,n)             Carthesian coordinates of the nodes in the
!                        user mpc.
!     u(3,n)             Actual displacements of the nodes in the
!                        user mpc.     
!     n                  number of terms in the user mpc
!
!     OUTPUT:
!
!     f                  Actual value of the mpc. If the mpc is
!                        exactly satisfied, this value is zero
!     a(n)               coefficients of the linearized mpc
!     jdof               degrees of freedom of the mpc terms
!
      implicit none
!
      integer jdof(*),n
!
      real*8 x(3,*),u(3,*),f,a(*)
!
!
      return
      end













