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
      subroutine plcopy(plcon,nplcon,plconloc,npmat_,ntmat_,
     &  imat,itemp,nelem,kin)
!
!     copies the hardening data for material imat and temperature
!     itemp from plcon into plconloc if the number of data points does
!     not exceed 20. Else, the equivalent plastic strain range is
!     divided into 19 intervals and the values are interpolated.
!     Attention: in plcon the odd storage spaces contain the Von
!                Mises stress, the even ones the equivalent plastic
!                strain. For plconloc, this order is reversed.
!
      implicit none
!
      integer imat,ndata,ntmat_,npmat_,nplcon(0:ntmat_,*),nelem,
     &  kin,k,itemp
!
      real*8 eplmin,eplmax,depl,epla,plcon(0:2*npmat_,ntmat_,*),
     &  plconloc(82),dummy
!
      ndata=nplcon(itemp,imat)
!
      if(ndata.le.20) then
         if(kin.eq.0) then
            do k=1,ndata
               plconloc(2*k-1)=plcon(2*k,1,imat)
               plconloc(2*k)=plcon(2*k-1,1,imat)
            enddo
            plconloc(81)=real(ndata)+0.5d0
         else
            do k=1,ndata
               plconloc(39+2*k)=plcon(2*k,1,imat)
               plconloc(40+2*k)=plcon(2*k-1,1,imat)
            enddo
            plconloc(82)=real(ndata)+0.5d0
         endif
      else
         if(kin.eq.0) then
            eplmin=plcon(2,1,imat)
            eplmax=plcon(2*nplcon(1,imat),1,imat)-1.d-10
            depl=(eplmax-eplmin)/19.d0
            do k=1,20
               epla=eplmin+(k-1)*depl
               call plinterpol(plcon,nplcon,itemp,
     &              plconloc(2*k),dummy,npmat_,ntmat_,imat,nelem,epla)
               plconloc(2*k-1)=epla
            enddo
            plconloc(81)=20.5d0
         else
            eplmin=plcon(2,1,imat)
            eplmax=plcon(2*nplcon(1,imat),1,imat)-1.d-10
            depl=(eplmax-eplmin)/19.d0
            do k=1,20
               epla=eplmin+(k-1)*depl
               call plinterpol(plcon,nplcon,itemp,
     &            plconloc(40+2*k),dummy,npmat_,ntmat_,imat,nelem,epla)
               plconloc(39+2*k)=epla
            enddo
         endif
         plconloc(82)=20.5d0
      endif
!
      return
      end
