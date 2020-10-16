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
      subroutine matdata_tg(imat,ntmat_,t1l,shcon,nshcon,sph)
!
      implicit none
!
!     determines the density, the specific heat and the conductivity 
!     in an integration point with coordinates pgauss
!
      integer imat,ntmat_,id,nshcon(*),two
!
      real*8 t1l,shcon(0:1,ntmat_,*),sph
!
      two=2
!
!     calculating the specific heat (needed for the capacity matrix)
!
      call ident2(shcon(0,1,imat),t1l,nshcon(imat),two,id)
      if(nshcon(imat).eq.0) then
         continue
      elseif(nshcon(imat).eq.1) then
         sph=shcon(1,1,imat)
      elseif(id.eq.0) then
         sph=shcon(1,1,imat)
      elseif(id.eq.nshcon(imat)) then
         sph=shcon(1,id,imat)
      else
         sph=shcon(1,id,imat)+
     &        (shcon(1,id+1,imat)-shcon(1,id,imat))*
     &        (t1l-shcon(0,id,imat))/
     &        (shcon(0,id+1,imat)-shcon(0,id,imat))
      endif
!
      return
      end







