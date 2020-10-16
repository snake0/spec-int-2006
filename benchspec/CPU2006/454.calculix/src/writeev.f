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
      subroutine writeev(x,nx)
!
!     writes the eigenvalues to unit 3 and replaces the 
!     eigenvalue by its square root = frequency (in rad/time)
!
      implicit none
!
      integer j,nx
      real*8 x(nx),pi
!
      pi=4.d0*datan(1.d0)
!
      write(5,*)
      write(5,*) '    E I G E N V A L U E   O U T P U T'
      write(5,*)
      write(5,*) 'MODE NO   EIGENVALUE           FREQUENCY'
      write(5,*) '                       (RAD/TIME)    (CYCLES/TIME)'
      write(5,*)
      do j=1,nx
         write(5,'(i7,3(2x,e12.5))') j,x(j),dsqrt(x(j)),
     &     dsqrt(x(j))/(2.d0*pi)
         x(j)=dsqrt(x(j))
      enddo
!
      return
      end

