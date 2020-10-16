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
      subroutine headings(text,textpart,istat,in,n)
!
!     reading the input deck: *HEADING
!
      implicit none
!
      character*40 textpart(16)
      character*132 text
!
      integer istat,in,n,key
!
      do
         call getnewline(text,textpart,istat,in,n,key)
         if((key.ne.0).or.(istat.lt.0))exit
      enddo
!
      return
      end

