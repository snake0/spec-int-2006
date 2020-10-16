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
      subroutine skip(nset,nalset,nload,nforc,nboun,nflow,nk,ne,nkon,
     &  mint_,nmpc,memmpc_,nmat,ntmat_,npmat_,ncmat_,norien,ntrans,nam,
     &  noprint,neprint,nlabel,ncs_,ne1d,ne2d,infree,nmethod,
     &  iperturb,nener,iplas,ithermal,nstate_,iprestr)
!
      implicit none
!
      integer nset,nalset,nload,nforc,nboun,nflow,nk,ne,nkon,mint_,
     &  nmpc,memmpc_,nmat,ntmat_,npmat_,ncmat_,norien,ntrans,nam,
     &  noprint,neprint,nlabel,ncs_,ne1d,ne2d,infree(4),i,
     &  nmethod,iperturb,nener,iplas,ithermal,nstate_,iprestr,i4,
     &  maxamta
!
      character*3 c3
      character*4 c4
      character*5 c5
      character*8 c8
      character*20 c20
      character*21 c21
!
      real*8 r8
!
!        skipping the next entries
!     
      read(15)(c21,i=1,nset)
      read(15)(i4,i=1,nset)
      read(15)(i4,i=1,nset)
      do i=1,nalset
         read(15)i4
      enddo
      read(15)(r8,i=1,3*nk)
      read(15)(i4,i=1,nkon)
      read(15)(i4,i=1,ne)
      read(15)(c8,i=1,ne)
      read(15)(i4,i=1,nboun)
      read(15)(i4,i=1,nboun)
      read(15)(r8,i=1,nboun)
      read(15)(i4,i=1,nboun)
      read(15)(i4,i=1,nboun)
      if(nam.gt.0) read(15)(i4,i=1,nboun)
      read(15)(i4,i=1,nboun)
      read(15)(i4,i=1,nboun)
      read(15)(r8,i=1,nboun)
      read(15)(i4,i=1,nmpc)
      read(15)(c20,i=1,nmpc)
      read(15)(i4,i=1,nmpc)
      read(15)(i4,i=1,nmpc)
      read(15)(i4,i=1,3*memmpc_)
      read(15)(r8,i=1,memmpc_)
      read(15)(i4,i=1,nforc)
      read(15)(i4,i=1,nforc)
      read(15)(r8,i=1,nforc)
      read(15)(i4,i=1,nforc)
      read(15)(i4,i=1,nforc)
      if(nam.gt.0) read(15)(i4,i=1,nforc)
      read(15)(r8,i=1,nforc)
      read(15)(i4,i=1,2*nload)
      read(15)(c5,i=1,nload)
      read(15)(r8,i=1,2*nload)
      if(nam.gt.0) read(15)(i4,i=1,2*nload)
      read(15)(r8,i=1,2*nload)
      read(15) (r8,i=1,3),(r8,i=1,3),r8,r8,i4
      read(15) (r8,i=1,3),(r8,i=1,3),i4
      if(iprestr.gt.0) read(15) (r8,i=1,6*ne)
      read(15)(i4,i=1,2*nflow)
      read(15)(r8,i=1,nflow)
      if(nam.gt.0) read(15)(i4,i=1,nflow)
      read(15)(r8,i=1,nflow)
      read(15)(i4,i=1,noprint)
      read(15)(i4,i=1,neprint)
      read(15)(c4,i=1,nlabel)
      read(15)(c4,i=1,nlabel)
      read(15)(r8,i=1,(ncmat_+1)*ntmat_*nmat)
      read(15)(i4,i=1,2*nmat)
      read(15)(r8,i=1,2*ntmat_*nmat)
      read(15)(i4,i=1,nmat)
      read(15)(r8,i=1,2*ntmat_*nmat)
      read(15)(i4,i=1,nmat)
      read(15)(r8,i=1,7*ntmat_*nmat)
      read(15)(i4,i=1,nmat)
      read(15)(r8,i=1,7*ntmat_*nmat)
      read(15)(i4,i=1,2*nmat)
      read(15)(r8,i=1,nmat)
      read(15)(r8,i=1,2)
      if(iplas.ne.0)then
         read(15)(r8,i=1,(2*npmat_+1)*ntmat_*nmat)
         read(15)(i4,i=1,(ntmat_+1)*nmat)
         read(15)(r8,i=1,(2*npmat_+1)*ntmat_*nmat)
         read(15)(i4,i=1,(ntmat_+1)*nmat)
      endif
      if(norien.ne.0)then
         read(15)(c20,i=1,norien)
         read(15)(r8,i=1,7*norien)
         read(15)(i4,i=1,ne)
      endif
      if(ntrans.ne.0)then
         read(15)(r8,i=1,7*ntrans)
         read(15)(i4,i=1,2*nk)
      endif
      if(nam.gt.0)then
         read(15)(c20,i=1,nam)
         read(15)(i4,i=1,3*nam-1)
         maxamta=2*i4
         read(15)i4
         read(15)(r8,i=1,maxamta)
      endif
      if(ithermal.gt.0)then
         if((ne1d.gt.0).or.(ne2d.gt.0))then
            read(15)(r8,i=1,3*nk)
            read(15)(r8,i=1,3*nk)
         else
            read(15)(r8,i=1,nk)
            read(15)(r8,i=1,nk)
         endif
         if(nam.gt.0) read(15)(i4,i=1,nk)
         read(15)(r8,i=1,nk)
      endif
      read(15)(c20,i=1,nmat)
      read(15)(i4,i=1,ne)
      read(15)(r8,i=1,4*nk)
      if((nmethod.eq.4).or.((nmethod.eq.1).and.(iperturb.ge.2))) 
     &     then
         read(15)(r8,i=1,4*nk)
      endif
      read(15)(i4,i=1,nk)
      if((ne1d.gt.0).or.(ne2d.gt.0))then
         read(15)(i4,i=1,2*nkon)
         read(15)(r8,i=1,infree(1)-1)
         read(15)(i4,i=1,infree(2)-1)
         read(15)(r8,i=1,2*nkon)
         read(15)(r8,i=1,2*ne)
         read(15)(i4,i=1,infree(4))
         read(15)(i4,i=1,3*(infree(3)-1))
         read(15)(i4,i=1,infree(4))
      endif
      if(ncs_.gt.0)then
         read(15)(i4,i=1,ncs_)
         read(15)(i4,i=1,5)
         read(15)(r8,i=1,7)
      endif
      read(15)(r8,i=1,6*mint_*ne)
      read(15)(r8,i=1,6*mint_*ne)
      if(nener.eq.1) read(15)(r8,i=1,mint_*ne)
      if(nstate_.gt.0) read(15)(r8,i=1,nstate_*mint_*ne)
      read(15) (r8,i=1,26)
      read(15) r8
      read(15) c3
      read(15) r8
!
      return
      end
