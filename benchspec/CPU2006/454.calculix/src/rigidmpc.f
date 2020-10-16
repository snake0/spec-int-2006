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
      subroutine rigidmpc(ipompc,nodempc,coefmpc,irefnode,irotnode,
     &  labmpc,nmpc,nmpc_,mpcfree,ikmpc,ilmpc,nk,nk_,nodeboun,ndirboun,
     &  ikboun,ilboun,nboun,nboun_,node)
!
!     generates three rigid body MPC's for node "node" about reference
!     (translational) node irefnode and rotational node irotnode 
!
      implicit none
!
      character*20 labmpc(*)
!
      integer ipompc(*),nodempc(3,*),nmpc,nmpc_,mpcfree,nk,nk_,ikmpc(*),
     &  ilmpc(*),node,id,mpcfreeold,j,idof,l,nodeboun(*),
     &  ndirboun(*),ikboun(*),ilboun(*),nboun,nboun_,irefnode,
     &  irotnode
!
      real*8 coefmpc(3,*)
!
      nk=nk+1
      if(nk.gt.nk_) then
         write(*,*) '*ERROR in rigidmpc: increase nk_'
         stop
      endif
      do j=1,3
         idof=7*(node-1)+j
         call nident(ikmpc,idof,nmpc,id)
         if(id.gt.0) then
            if(ikmpc(id).eq.idof) then
               cycle
            endif
         endif
         nmpc=nmpc+1
         if(nmpc.gt.nmpc_) then
            write(*,*) '*ERROR in rigidmpc: increase nmpc_'
            stop
         endif
!
         ipompc(nmpc)=mpcfree
         labmpc(nmpc)='RIGID               '
!
         do l=nmpc,id+2,-1
            ikmpc(l)=ikmpc(l-1)
            ilmpc(l)=ilmpc(l-1)
         enddo
         ikmpc(id+1)=idof
         ilmpc(id+1)=nmpc
!
         nodempc(1,mpcfree)=node
         nodempc(2,mpcfree)=j
         mpcfree=nodempc(3,mpcfree)
         nodempc(1,mpcfree)=irefnode
         nodempc(2,mpcfree)=j
         mpcfree=nodempc(3,mpcfree)
         nodempc(1,mpcfree)=irotnode
         nodempc(2,mpcfree)=1
         mpcfree=nodempc(3,mpcfree)
         nodempc(1,mpcfree)=irotnode
         nodempc(2,mpcfree)=2
         mpcfree=nodempc(3,mpcfree)
         nodempc(1,mpcfree)=irotnode
         nodempc(2,mpcfree)=3
         mpcfree=nodempc(3,mpcfree)
         nodempc(1,mpcfree)=nk
         nodempc(2,mpcfree)=j
         mpcfreeold=mpcfree
         mpcfree=nodempc(3,mpcfree)
         nodempc(3,mpcfreeold)=0
         idof=7*(nk-1)+j
         call nident(ikboun,idof,nboun,id)
         nboun=nboun+1
         if(nboun.gt.nboun_) then
            write(*,*) '*ERROR in rigidmpc: increase nboun_'
            stop
         endif
         nodeboun(nboun)=nk
         ndirboun(nboun)=j
c         idof=3*(nk-1)+j
c         call nident(ikboun,idof,nboun-1,id)
         do l=nboun,id+2,-1
            ikboun(l)=ikboun(l-1)
            ilboun(l)=ilboun(l-1)
         enddo
         ikboun(id+1)=idof
         ilboun(id+1)=nboun
       enddo
!
       return
       end


