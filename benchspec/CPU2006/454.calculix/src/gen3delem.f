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
      subroutine gen3delem(kon,ipkon,lakon,ne,ipompc,nodempc,coefmpc,
     &  nmpc,nmpc_,mpcfree,ikmpc,ilmpc,labmpc,ikboun,ilboun,nboun,
     &  nboun_,nodeboun,ndirboun,xboun,iamboun,nam,
     &  inotr,trab,nk,nk_,iponoel,inoel,iponor,xnor,thicke,thickn,
     &  knor,istep,offset,t0,t1,ikforc,ilforc,rig,nforc,
     &  nforc_,nodeforc,ndirforc,xforc,iamforc,nelemload,sideload,
     &  nload,ithermal,ntrans,co,ixfree,ikfree,inoelfree,iponoelmax,
     &  iperturb,tinc,tper,tmin,tmax,ctrl)
!
!     generates three-dimensional elements:
!         for isochoric elements
!         for plane stress
!         for plane strain
!         for plate and shell elements
!         for beam elements
!
      implicit none
!
      logical axial
!
      character*5 sideload(*)
      character*8 lakon(*)
      character*20 labmpc(*)
!
      integer ipompc(*),nodempc(3,*),nmpc,nmpc_,mpcfree,ikmpc(*),
     &  ilmpc(*),kon(*),ipkon(*),ne,mpc,indexe,i,j,k,node,idof,
     &  id,mpcfreeold,ikboun(*),ilboun(*),nboun,nboun_,kflag,idummy,
     &  iterm(500),nterm,neigh(7,8),l,m,nodeboun(*),ndirboun(*),nk,
     &  nk_,index,iponoel(*),inoel(3,*),inoelfree,ielem,nelshell,
     &  irefnode,irotnode,nodes(3,8),nodeb(8,3),istep,
     &  ikforc(*),ilforc(*),nodeforc(*),ndirforc(*),iamforc(*),
     &  nelemload(*),nforc,nforc_,i1,i2,ithermal,nload,iamboun(*),
     &  ntrans,inotr(2,*),iamplitude,nam,nodel(8),indexx,indexk,
     &  iponoelmax,nexp,ifix,idir,nbounold,nemin,iperturb,nnor,
     &  numnod,itransaxial,mpcfreenew,index1,index2,newnode,nope,
     &  indexes,rig(*)
!
      integer iel(100),jl(100),ial(100),nel,iponor(2,*),knor(*),
     & jact,ixfree,ikfree,ndepnodes,idepnodes(80),ifi(100),imax
!
      real*8 coefmpc(*),thicke(2,*),thicks(8),xnors(3,8),
     &  xno(3,100),xnor(*),xnoref(3),coloc8(2,8),xl(3,8),dmax,
     &  xta(3,100),xn1(3,100),thickn(2,*),tinc,tper,tmin,tmax,
     &  xnorb(6,3),thickb(2,3),coloc3(3),offset(2,*),t0(*),t1(*),
     &  xforc(*),trab(7,*),val,xi,et,dd,dot,co(3,*),xboun(*),
     &  sc,thl1(100),thl2(100),off1(100),off2(100),dc(8),ds(8),
     &  dot1,dot2,pi,ctrl(26)
!
      data neigh /1,9,2,17,5,12,4,2,9,1,18,6,10,3,
     &            3,11,4,19,7,10,2,4,11,3,20,8,12,1,
     &            5,13,6,17,1,16,8,6,13,5,18,2,14,7,
     &            7,15,8,19,3,14,6,8,15,7,20,4,16,5/
!
      data coloc3 /-1.d0,0.d0,1.d0/
      data coloc8 /-1.d0,-1.d0,1.d0,-1.d0,1.d0,1.d0,-1.d0,1.d0,
     &            0.d0,-1.d0,1.d0,0.d0,0.d0,1.d0,-1.d0,0.d0/
!
      pi=4.d0*datan(1.d0)
!
!     catalogueing the element per node relationship for shell/beam
!     elements and transferring the nodal thickness to the elements
!
!     inoelfree=1 means that there is at least one 1D or 2D element
!     in the structure. Otherwise inoelfree=0.
!
      if((istep.eq.1).and.(inoelfree.eq.1)) then
!
         itransaxial=0
!
         do i=1,ne
            if(ipkon(i).lt.0) cycle
            if(lakon(i)(1:2).ne.'C3') then
               if(lakon(i)(1:1).eq.'B') then
                  numnod=3
               else
                  numnod=8
               endif
               indexe=ipkon(i)
               do j=1,numnod
                  node=kon(indexe+j)
                  iponoelmax=max(iponoelmax,node)
                  inoel(1,inoelfree)=i
                  inoel(2,inoelfree)=j
                  inoel(3,inoelfree)=iponoel(node)
                  iponoel(node)=inoelfree
                  inoelfree=inoelfree+1
                  if(thickn(1,node).gt.0.d0)
     &                 thicke(1,indexe+j)=thickn(1,node)
                  if(thickn(2,node).gt.0.d0)
     &                 thicke(2,indexe+j)=thickn(2,node)
                  if(thicke(1,indexe+j).le.0.d0) then
                     if(lakon(i)(1:1).eq.'C') then
                        thicke(1,indexe+j)=1.d0
                     else
                        write(*,*)'*ERROR in gen3delem: first thickness'
                        write(*,*)'       in element ',i,' is zero'
                        stop
                     endif
                  endif
                  if((lakon(i)(1:1).eq.'B').and.
     &                 (thicke(2,indexe+j).le.0.d0)) then
                     write(*,*) '*ERROR in gen3delem: second thickness'
                     write(*,*) '       in beam element ',i,' is zero'
                     stop
                  endif
               enddo
            endif
         enddo
!
!        checking whether any rotational degrees of freedom are fixed
!        by SPC's, MPC's or loaded by bending moments or torques
!        in the end, rig(i)=0 is no rigid knot is defined in node i,
!        else rig(i)=the rotational node of the knot. The value -1 is
!        a dummy. 
!
         do i=1,nboun
            if(ndirboun(i).gt.3) rig(nodeboun(i))=-1
         enddo
         do i=1,nforc
            if(ndirforc(i).gt.3) rig(nodeforc(i))=-1
         enddo
         do i=1,nmpc
            index=ipompc(i)
            do
               if(index.eq.0) exit
               if(nodempc(2,index).gt.3) then
                  rig(nodempc(1,index))=-1
               endif
               index=nodempc(3,index)
            enddo
         enddo
!
!     calculating the normals in nodes belonging to shells/beams
!
         do i=1,nk
            ndepnodes=0
            index=iponoel(i)
            if(index.eq.0) cycle
!
!           nexp indicates how many times the node was expanded
!
            nexp=0
!
!           nnor indicates whether the expanded nodes lie on a point
!           (nnor=0, only for plane stress, plane strain or axisymmetric
!           elements, on a line (nnor=1) or in a plane (nnor=2)
!
            nnor=0
!
!          locating the shell elements to which node i belongs
!
            nel=0
            do
               if(index.eq.0) exit
               ielem=inoel(1,index)
               if(lakon(ielem)(1:1).ne.'B') then
                  if(lakon(ielem)(1:1).eq.'S') nnor=1
                  indexe=ipkon(ielem)
                  nel=nel+1
                  if(nel.gt.100) then
                     write(*,*) '*ERROR in gen3delem: more than 100'
                     write(*,*) '       shell elements share the'
                     write(*,*) '       same node'
                     stop
                  endif
                  j=inoel(2,index)
                  jl(nel)=j
                  iel(nel)=ielem
                  thl1(nel)=thicke(1,indexe+j)
                  off1(nel)=offset(1,ielem)
               endif
               index=inoel(3,index)
            enddo
!
            if(nel.gt.0) then
               do j=1,nel
                  ial(j)=0
               enddo
!
!        estimate the normal
!
               do j=1,nel
                  indexe=ipkon(iel(j))
                  indexx=iponor(1,indexe+jl(j))
                  if(indexx.ge.0) then
                     do k=1,3
                        xno(k,j)=xnor(indexx+k)
                     enddo
                     ifi(j)=1
                     cycle
                  else
                     ifi(j)=0
                  endif
                  xi=coloc8(1,jl(j))
                  et=coloc8(2,jl(j))
                  do k=1,8
                     node=kon(indexe+k)
                     do l=1,3
                        xl(l,k)=co(l,node)
                     enddo
                  enddo
                  call norshell(xi,et,xl,xno(1,j))
                  dd=dsqrt(xno(1,j)**2+xno(2,j)**2+xno(3,j)**2)
                  if(dd.lt.1.d-10) then
                     write(*,*) '*ERROR in gen3delem: size of estimated'
                     write(*,*)'    shell normal is smaller than 1.e-10'
                     stop
                  endif
                  do k=1,3
                     xno(k,j)=xno(k,j)/dd
                  enddo
               enddo
!
               do
!
!           determining a fixed normal which was not treated yet,
!           or, if none is left, the minimum element number of all
!           elements containing node i and for which no normal was
!           determined yet
!
                  ifix=0
                  nemin=ne+1
                  do j=1,nel
                     if(ial(j).ne.0) cycle
                     if(ifi(j).eq.1) then
                        jact=j
                        ifix=1
                        exit
                     endif
                  enddo
                  if(ifix.eq.0) then
                     do j=1,nel
                        if(ial(j).eq.0) then
                           if(iel(j).lt.nemin) then
                              nemin=iel(j)
                              jact=j
                           endif
                        endif
                     enddo
                     if(nemin.eq.ne+1) exit
                  endif
!
                  do j=1,3
                     xnoref(j)=xno(j,jact)
                  enddo
!
!           determining all elements whose normal in node i makes an
!           angle smaller than 0.5 or 20 degrees with the reference normal,
!           depending whether the reference normal was given by the
!           user or is being calculated; the thickness and offset must
!           also fit.
!
                  do j=1,nel
                     if(ial(j).eq.2) cycle
                     if(j.eq.jact) then
                        ial(jact)=1
                     else
                        dot=xno(1,j)*xnoref(1)+xno(2,j)*xnoref(2)+
     &                       xno(3,j)*xnoref(3)
                        if(ifix.eq.0) then
                           if(dot.gt.0.939693d0)then
                              if((dabs(thl1(j)-thl1(jact)).lt.1.d-10)
     &                           .and.
     &                           (dabs(off1(j)-off1(jact)).lt.1.d-10)
     &                           .and.
     &                           (lakon(iel(j)).eq.lakon(iel(jact))))
     &                             ial(j)=1
                           else
                              if((lakon(iel(j))(1:1).eq.'S').and.
     &                           (lakon(iel(jact))(1:1).eq.'S')) then
c                                 nnor=2
!
!                                if the normals have the opposite
!                                direction, the expanded nodes are on a
!                                straight line
!
                                 if(dot.gt.-0.999962) then
                                    nnor=2
                                 else
                                    write(*,*) '*INFO in gen3delem: in 
     &some nodes opposite normals are defined'
                                 endif
                              endif
                           endif
                        else
                           if(dot.gt.0.999962d0) then
                              if((dabs(thl1(j)-thl1(jact)).lt.1.d-10)
     &                           .and.
     &                           (dabs(off1(j)-off1(jact)).lt.1.d-10)
     &                           .and.
     &                           (lakon(iel(j)).eq.lakon(iel(jact))))
     &                             ial(j)=1
                           else
                              if((lakon(iel(j))(1:1).eq.'S').and.
     &                           (lakon(iel(jact))(1:1).eq.'S')) then
c                                 nnor=2
!
!                                if the normals have the opposite
!                                direction, the expanded nodes are on a
!                                straight line
!
                                 if(dot.gt.-0.999962) then
                                    nnor=2
                                 else
                                    write(*,*) '*INFO in gen3delem: in 
     &some nodes opposite normals are defined'
                                 endif
                              endif
                           endif
                        endif
                     endif
                  enddo
!
!           determining the mean normal for the selected elements
!
                  if(ifix.eq.0) then
                     do j=1,3
                        xnoref(j)=0.d0
                     enddo
                     do j=1,nel
                        if(ial(j).eq.1) then
                           do k=1,3
                              xnoref(k)=xnoref(k)+xno(k,j)
                           enddo
                        endif
                     enddo
                     dd=dsqrt(xnoref(1)**2+xnoref(2)**2+xnoref(3)**2)
                     if(dd.lt.1.d-10) then
                        write(*,*) '*ERROR in gen3delem: size of'
                        write(*,*) '        estimated shell normal is'
                        write(*,*) '       smaller than 1.e-10'
                        stop
                     endif
                     do j=1,3
                        xnoref(j)=xnoref(j)/dd
                     enddo
                  endif
!
!           updating the pointers iponor
!            
                  nexp=nexp+1
                  do j=1,nel
                     if(ial(j).eq.1) then
                        ial(j)=2
                        if(ifix.eq.0) then
                           iponor(1,ipkon(iel(j))+jl(j))=ixfree
                        elseif(j.ne.jact) then
                           iponor(1,ipkon(iel(j))+jl(j))=
     &                          iponor(1,ipkon(iel(jact))+jl(jact)) 
                        endif
                        iponor(2,ipkon(iel(j))+jl(j))=ikfree
                     endif
                  enddo
!
!           storing the normal in xnor and generating 3 nodes
!           for knor
!     
                  if(ifix.eq.0) then
                     do j=1,3
                        xnor(ixfree+j)=xnoref(j)
                     enddo
                     ixfree=ixfree+3
                  endif
!
                  do k=1,3
                     nk=nk+1
                     if(nk.gt.nk_) then
                        write(*,*) '*ERROR in nodes: increase nk_'
                        stop
                     endif
                     knor(ikfree+k)=nk
!
!                    for plane stress, plane strain and axisymmetric
!                    elements only the middle node is included in the
!                    rigid body definition
! 
                     if((lakon(iel(jact))(2:2).ne.'P').and.
     &                  (lakon(iel(jact))(2:2).ne.'A')) then
                        idepnodes(ndepnodes+1)=nk
                        ndepnodes=ndepnodes+1
                     elseif(k.eq.2) then
c                        if(jl(jact).le.4) then
                           idepnodes(ndepnodes+1)=nk
                           ndepnodes=ndepnodes+1
c                        endif
                     endif
                  enddo
                  ikfree=ikfree+3
               enddo
            endif
!
            nelshell=nel+1
!
!        locating the beam elements to which node i belongs
!
            index=iponoel(i)
            do
               if(index.eq.0) exit
               ielem=inoel(1,index)
               if(lakon(ielem)(1:1).eq.'B') then
                  indexe=ipkon(ielem)
                  nel=nel+1
                  if(nel.gt.100) then
                     write(*,*) '*ERROR in gen3delem: more than 100'
                     write(*,*) '        beam/shell elements share'
                     write(*,*) '        the same node'
                     stop
                  endif
                  j=inoel(2,index)
                  jl(nel)=j
                  iel(nel)=ielem
                  thl1(nel)=thicke(1,indexe+j)
                  thl2(nel)=thicke(2,indexe+j)
                  off1(nel)=offset(1,ielem)
                  off2(nel)=offset(2,ielem)
               endif
               index=inoel(3,index)
            enddo
!
            if(nel.ge.nelshell) then
               nnor=2
               do j=nelshell,nel
                  ial(j)=0
               enddo
!
!           estimate the normal
!
               do j=nelshell,nel
                  xi=coloc3(jl(j))
                  indexe=ipkon(iel(j))
                  do k=1,3
                     node=kon(indexe+k)
                     do l=1,3
                        xl(l,k)=co(l,node)
                     enddo
                  enddo
!
!           determining the tangent vector xta
!
                  do k=1,3
                     xta(k,j)=(xi-0.5d0)*xl(k,1)-2.d0*xi*xl(k,2)+
     &                    (xi+0.5d0)*xl(k,3)
                  enddo
                  dd=dsqrt(xta(1,j)**2+xta(2,j)**2+xta(3,j)**2)
                  if(dd.lt.1.d-10) then
                     write(*,*) '*ERROR in gen3delem: size of estimated'
                     write(*,*)'   shell tangent is smaller than 1.e-10'
                     stop
                  endif
                  do k=1,3
                     xta(k,j)=xta(k,j)/dd
                  enddo
!
!           check whether normal was defined with *NORMAL and
!           determine vector n1
!
                  if(iponor(1,indexe+jl(j)).ge.0) then
                     indexx=iponor(1,indexe+jl(j))
                     if(dabs(xnor(indexx+4)**2+xnor(indexx+5)**2+
     &                    xnor(indexx+6)**2-1.d0).lt.1.d-5) then
                        do k=1,3
                           xno(k,j)=xnor(indexx+3+k)
                        enddo
                        ifi(j)=1
                        cycle
                     endif
                     do k=1,3
                        xn1(k,j)=xnor(indexx+k)
                     enddo
                  else
                     xn1(1,j)=0.d0
                     xn1(2,j)=0.d0
                     xn1(3,j)=-1.d0
                  endif
!
!           normal (=n2) = xta x xn1
!
                  xno(1,j)=xta(2,j)*xn1(3,j)-xta(3,j)*xn1(2,j)
                  xno(2,j)=xta(3,j)*xn1(1,j)-xta(1,j)*xn1(3,j)
                  xno(3,j)=xta(1,j)*xn1(2,j)-xta(2,j)*xn1(1,j)
                  dd=dsqrt(xno(1,j)**2+xno(2,j)**2+xno(3,j)**2)
                  if(dd.lt.1.d-10) then
                     write(*,*) '*ERROR in gen3delem: size of estimated'
                     write(*,*)'    shell normal is smaller than 1.e-10'
                     stop
                  endif
                  do k=1,3
                     xno(k,j)=xno(k,j)/dd
                  enddo
               enddo
!
               do
!
!           determining a fixed normal which was not treated yet,
!           or, if none is left, the minimum element number of all
!           elements containing node i and for which no normal was
!           determined yet
!
                  ifix=0
                  nemin=ne+1
                  do j=nelshell,nel
                     if(ial(j).ne.0) cycle
                     if(ifi(j).eq.1) then
                        jact=j
                        ifix=1
                        exit
                     endif
                  enddo
                  if(ifix.eq.0) then
                     do j=nelshell,nel
                        if(ial(j).eq.0) then
                           if(iel(j).lt.nemin) then
                              nemin=iel(j)
                              jact=j
                           endif
                        endif
                     enddo
                     if(nemin.eq.ne+1) exit
                  endif
!
!           the reference normal is the one on the element with the
!           smallest element number
!
                  do j=1,3
                     xnoref(j)=xno(j,jact)
                  enddo
!
!           determining all elements whose normal in node i makes an
!           angle smaller than 0.5 or 20 degrees with the reference normal,
!           depending whether the reference normal was given by the
!           user or is being calculated; the thickness and offset must
!           also fit.
!
                  do j=nelshell,nel
                     if(j.eq.jact) then
                        ial(jact)=1
                     else
                        dot1=xno(1,j)*xnoref(1)+xno(2,j)*xnoref(2)+
     &                       xno(3,j)*xnoref(3)
                        dot2=xta(1,j)*xta(1,jact)+xta(2,j)*xta(2,jact)+
     &                       xta(3,j)*xta(3,jact)
                        if(ifix.eq.0) then
                           if((dot1.gt.0.939693d0).and.
     &                        (dot2.gt.0.939693d0)) then
                              if((dabs(thl1(j)-thl1(jact)).lt.1.d-10)
     &                           .and.
     &                           (dabs(thl2(j)-thl2(jact)).lt.1.d-10)
     &                           .and.
     &                           (dabs(off1(j)-off1(jact)).lt.1.d-10)
     &                           .and.
     &                           (dabs(off2(j)-off2(jact)).lt.1.d-10)
     &                           .and.
     &                   (lakon(iel(j))(8:8).eq.lakon(iel(jact))(8:8)))
     &                             ial(j)=1
                              endif
                        else
                           if((dot1.gt.0.999962d0).and.
     &                        (dot2.gt.0.999962d0)) then
                              if((dabs(thl1(j)-thl1(jact)).lt.1.d-10)
     &                           .and.
     &                           (dabs(thl2(j)-thl2(jact)).lt.1.d-10)
     &                           .and.
     &                           (dabs(off1(j)-off1(jact)).lt.1.d-10)
     &                           .and.
     &                           (dabs(off2(j)-off2(jact)).lt.1.d-10)
     &                           .and.
     &                   (lakon(iel(j))(8:8).eq.lakon(iel(jact))(8:8)))
     &                             ial(j)=1
                           endif
                        endif
                     endif
                  enddo
!
!           determining the mean normal for the selected elements
!
                  if(ifix.eq.0) then
                     do j=1,3
                        xnoref(j)=0.d0
                     enddo
                     do j=nelshell,nel
                        if(ial(j).eq.1) then
                           do k=1,3
                              xnoref(k)=xnoref(k)+xno(k,j)
                           enddo
                        endif
                     enddo
                  endif
!
!              calculating the mean tangent
!
                  do j=nelshell,nel
                     if((ial(j).eq.1).and.(j.ne.jact)) then
                        do k=1,3
                           xta(k,jact)=xta(k,jact)+xta(k,j)
                        enddo
                     endif
                  enddo
                  dd=dsqrt(xta(1,jact)**2+xta(2,jact)**2+xta(3,jact)**2)
                  if(dd.lt.1.d-10) then
                     write(*,*) '*ERROR in gen3delem: size of mean'
                     write(*,*)'    beam tangent is smaller than 1.e-10'
                     stop
                  endif
                  do k=1,3
                     xta(k,jact)=xta(k,jact)/dd
                  enddo
!
!              taking care that the mean normal is orthogonal towards
!              the mean tangent
!
                  dd=xnoref(1)*xta(1,jact)+xnoref(2)*xta(2,jact)+
     &               xnoref(3)*xta(3,jact)
                  do j=1,3
                     xnoref(j)=xnoref(j)-dd*xta(j,jact)
                  enddo
                  dd=dsqrt(xnoref(1)**2+xnoref(2)**2+xnoref(3)**2)
                  if(dd.lt.1.d-10) then
                     write(*,*) '*ERROR in gen3delem: size of'
                     write(*,*) '        estimated beam normal is'
                     write(*,*) '        smaller than 1.e-10'
                     stop
                  endif
                  do j=1,3
                     xnoref(j)=xnoref(j)/dd
                  enddo
!
!              calculating xn1 = xn2  x tangent              
!
                 xn1(1,jact)=xnoref(2)*xta(3,jact)-xnoref(3)*xta(2,jact)
                 xn1(2,jact)=xnoref(3)*xta(1,jact)-xnoref(1)*xta(3,jact)
                 xn1(3,jact)=xnoref(1)*xta(2,jact)-xnoref(2)*xta(1,jact)
!
!              storing the normals in xnor and generating 8 nodes
!              for knor
!
                  nexp=nexp+1
                  do j=nelshell,nel
                     if(ial(j).eq.1) then
                        ial(j)=2
                        if(ifix.eq.0) then
                           iponor(1,ipkon(iel(j))+jl(j))=ixfree
                        else
                           iponor(1,ipkon(iel(j))+jl(j))=
     &                          iponor(1,ipkon(iel(jact))+jl(jact))
                        endif
                        iponor(2,ipkon(iel(j))+jl(j))=ikfree
                     endif
                  enddo
!
                  do j=1,3
                     xnor(ixfree+j)=xn1(j,jact)
                  enddo
                  do j=1,3
                     xnor(ixfree+3+j)=xnoref(j)
                  enddo
                  ixfree=ixfree+6
                  do k=1,8
                     nk=nk+1
                     if(nk.gt.nk_) then
                        write(*,*) '*ERROR in nodes: increase nk_'
                        stop
                     endif
                     knor(ikfree+k)=nk
                     idepnodes(ndepnodes+k)=nk
                  enddo
                  ikfree=ikfree+8
                  ndepnodes=ndepnodes+8
               enddo
            endif
!
!           check whether the user has specified rotational degrees
!           of freedom; if so, a rigid MPC must be defined
!
            if(rig(i).ne.0) then
               if(nnor.lt.1) then
                  rig(i)=0
               elseif(nexp.le.1) then
                  nexp=2
               endif
            endif
c            if((nexp.le.1).and.(nnor.ge.1)) then
c               do j=4,6
c                  idof=7*(i-1)+j
c                  call nident(ikboun,idof,nboun,id)
c                  if(id.gt.0) then
c                     if(ikboun(id).eq.idof) then
c                        nexp=2
c                        exit
c                     endif
c                  endif
c                  idof=7*(i-1)+j
c                  call nident(ikforc,idof,nforc,id)
c                  if(id.gt.0) then
c                     if(ikforc(id).eq.idof) then
c                        nexp=2
c                        exit
c                     endif
c                  endif
c               enddo
c            endif
!
!        generate rigid MPC's if necessary
!
            if(nexp.gt.1) then
               if(iperturb.eq.0) then
                  iperturb=2
                  tinc=1.d0
                  tper=1.d0
                  tmin=1.d-5
                  tmax=1.d+30
                  ctrl(19)=1.d+30
                  ctrl(20)=1.d+30
               elseif(iperturb.eq.1) then
                  write(*,*) '*ERROR in gen3delem: the expansion of'
                  write(*,*) '       1D/2D elements has led to the'
                  write(*,*) '       creating of rigid body MPCs.'
                  write(*,*) '       This is not allowed in a'
                  write(*,*) '       perturbation analysis. Please'
                  write(*,*) '       generate a truely 3D structure'
                  stop
               endif
               irefnode=i
               if(nnor.eq.0) then
!
!                 the node belongs to plane stress, plane strain
!                 or axisymmetric elements only. These are only linked
!                 through the node in the midplane: the nodes
!                 coincide.
!
                  do k=1,ndepnodes
                     node=idepnodes(k)
                     do j=1,2
                        idof=7*(node-1)+j
                        call nident(ikmpc,idof,nmpc,id)
                        nmpc=nmpc+1
                        if(nmpc.gt.nmpc_) then
                           write(*,*) 
     &                          '*ERROR in rigidmpc: increase nmpc_'
                           stop
                        endif
!
                        ipompc(nmpc)=mpcfree
                        labmpc(nmpc)='                    '
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
                        coefmpc(mpcfree)=1.d0
                        mpcfree=nodempc(3,mpcfree)
                        nodempc(1,mpcfree)=irefnode
                        nodempc(2,mpcfree)=j
                        coefmpc(mpcfree)=-1.d0
                        mpcfreeold=mpcfree
                        mpcfree=nodempc(3,mpcfree)
                        nodempc(3,mpcfreeold)=0
                     enddo
                  enddo
               else
!
                  nk=nk+1
                  if(nk.gt.nk_) then
                     write(*,*) '*ERROR in rigidbodies: increase nk_'
                     stop
                  endif
                  irotnode=nk
                  rig(i)=irotnode
                  do k=1,ndepnodes
                     call rigidmpc(ipompc,nodempc,coefmpc,irefnode,
     &                    irotnode,
     &                    labmpc,nmpc,nmpc_,mpcfree,ikmpc,ilmpc,nk,nk_,
     &                    nodeboun,ndirboun,ikboun,ilboun,nboun,nboun_,
     &                    idepnodes(k))
                  enddo
               endif
               if(nnor.eq.1) then
!
!                 generate an additional SPC or MPC for rigid body nodes
!                 lying on a line to prevent rotation about the
!                 line
!
                  dmax=0.d0
                  imax=0
                  do j=1,3
                     if(dabs(xnoref(j)).gt.dmax) then
                        dmax=dabs(xnoref(j))
                        imax=j
                     endif
                  enddo
!
!                 check whether a SPC suffices
!
c                  if(dabs(1.d0-dmax).lt.1.d-5) then
                  if(dabs(1.d0-dmax).lt.1.d-3) then
                     write(*,*) 'additional SPC! ',i,irotnode,imax
                     val=0.d0
                     if(nam.gt.0) iamplitude=0
                     call bounadd(irotnode,imax,imax,val,nodeboun,
     &                    ndirboun,xboun,nboun,nboun_,iamboun,
     &                    iamplitude,nam,ipompc,nodempc,coefmpc,
     &                    nmpc,nmpc_,mpcfree,inotr,trab,ntrans,
     &                    ikboun,ilboun,ikmpc,ilmpc,co,nk,nk_,labmpc)
                  else
!
!                    check whether the rotational degree of freedom
!                    imax is fixed through a SPC
!
                     idof=7*(i-1)+3+imax
                     call nident(ikboun,idof,nboun,id)
                     if((id.gt.0).and.(ikboun(id).eq.idof)) then
!
!                       imax fixed: do not generate MPC
!                        
                     else
                        write(*,*) '*ERROR in gen3delem: this branch'
                        write(*,*) '       does not work yet: contact'
                        write(*,*) '       the author'
                        idof=0
                        if(idof.eq.0) stop
c
                        idof=7*(irotnode-1)+imax
                        call nident(ikmpc,idof,nmpc,id)
                        nmpc=nmpc+1
                        if(nmpc.gt.nmpc_) then
                           write(*,*) 
     &                       '*ERROR in rigidmpc: increase nmpc_'
                           stop
                        endif
!
                        ipompc(nmpc)=mpcfree
                        labmpc(nmpc)='                    '
!
                        do l=nmpc,id+2,-1
                           ikmpc(l)=ikmpc(l-1)
                           ilmpc(l)=ilmpc(l-1)
                        enddo
                        ikmpc(id+1)=idof
                        ilmpc(id+1)=nmpc
!
                        nodempc(1,mpcfree)=irotnode
                        nodempc(2,mpcfree)=imax
                        coefmpc(mpcfree)=xnoref(imax)
                        mpcfree=nodempc(3,mpcfree)
                        imax=imax+1
                        if(imax.gt.3) imax=imax-3
                        nodempc(1,mpcfree)=irotnode
                        nodempc(2,mpcfree)=imax
                        coefmpc(mpcfree)=xnoref(imax)
                        mpcfree=nodempc(3,mpcfree)
                        imax=imax+1
                        if(imax.gt.3) imax=imax-3
                        nodempc(1,mpcfree)=irotnode
                        nodempc(2,mpcfree)=imax
                        coefmpc(mpcfree)=xnoref(imax)
                        mpcfreeold=mpcfree
                        mpcfree=nodempc(3,mpcfree)
                        nodempc(3,mpcfreeold)=0
                     endif
                  endif
               endif
c            else
c               rig(i)=' '
            endif
         enddo
      endif
!
      if(istep.eq.1) then
!
         do i=1,ne
            if(ipkon(i).lt.0) cycle
!
!        incompressible elements
!
            if(lakon(i)(1:7).eq.'C3D20RH') then
               indexe=ipkon(i)
               do j=1,8
                  node=kon(indexe+j)
                  mpc=0
                  do k=1,3
                     idof=7*(node-1)+k
                     call nident(ikmpc,idof,nmpc,id)
                     if(id.gt.0) then
                        if(ikmpc(id).eq.idof) then
                           if(labmpc(ilmpc(i))(1:9).eq.'ISOCHORIC') then
!
!                          existing mpc
!
                              mpc=ilmpc(id)
                              exit
                           else
                              cycle
                           endif
                        endif
                     endif
!
!                 new mpc
!
                     nmpc=nmpc+1
                     if(nmpc.gt.nmpc_) then
                        write(*,*) '*ERROR in gen3delem: increase nmpc_'
                        stop
                     endif
                     mpc=nmpc
                     do l=nmpc,id+2,-1
                        ikmpc(l)=ikmpc(l-1)
                        ilmpc(l)=ilmpc(l-1)
                     enddo
                     ikmpc(id+1)=idof
                     ilmpc(id+1)=nmpc
                  enddo
!
                  if(mpc.eq.0) then
                     write(*,*) '*WARNING in gen3delem: no free DOF in'
                     write(*,*) '         node ',node,' for isochoric'
                     write(*,*) '         MPC application'
                     cycle
                  endif
!
                  if(ipompc(mpc).eq.0) then
!
!                 new mpc
!
                     labmpc(mpc)(1:9)='ISOCHORIC'
                     ipompc(mpc)=mpcfree
                     nodempc(1,mpcfree)=node
                     nodempc(2,mpcfree)=k
                     mpcfree=nodempc(3,mpcfree)
!
                     do k=1,7
                        do l=1,3
                           nodempc(1,mpcfree)=kon(indexe+neigh(k,j))
                           nodempc(2,mpcfree)=l
                           mpcfree=nodempc(3,mpcfree)
                        enddo
                     enddo
!
!                 add nonhomogeneous term
!                  
                     nk=nk+1
                     if(nk.gt.nk_) then
                        write(*,*) '*ERROR in gen3delem: increase nk_'
                        stop
                     endif
                     mpcfreeold=mpcfree
                     mpcfree=nodempc(3,mpcfree)
                     nodempc(3,mpcfreeold)=0
                     idof=7*(nk-1)+k
                     call nident(ikboun,idof,nboun,id)
                     nboun=nboun+1
                     if(nboun.gt.nboun_) then
                        write(*,*)'*ERROR in gen3delem: increase nboun_'
                        stop
                     endif
                     nodeboun(nboun)=nk
                     ndirboun(nboun)=j
                     do l=nboun,id+2,-1
                        ikboun(l)=ikboun(l-1)
                        ilboun(l)=ilboun(l-1)
                     enddo
                     ikboun(id+1)=idof
                     ilboun(id+1)=nboun
!
                  else
!
                     index=nodempc(3,ipompc(mpc))
                     nterm=0
                     do
                        if(index.eq.0) exit
                        nterm=nterm+1
                        if(nterm.gt.500) then
                           write(*,*) '*ERROR in gen3delem:'
                           write(*,*) '       increase nterm_'
                           stop
                        endif
                        iterm(nterm)=
     &                       7*(nodempc(1,index)-1)+nodempc(2,index)
                        index=nodempc(3,nodempc(3,nodempc(3,index)))
                     enddo
                     kflag=1
                     call isortii(iterm,idummy,nterm,kflag)
!
                     do k=1,7
                        do l=1,3
                           m=7*(kon(indexe+neigh(k,j))-1)+l
                           call nident(iterm,m,nterm,id)
                           if(id.ne.0) then
                              if(iterm(id).eq.m) then
                                 cycle
                              endif
                           endif
                           mpcfreeold=mpcfree
                           mpcfree=nodempc(3,mpcfree)
                           nodempc(3,mpcfree)=ipompc(mpc)
                           ipompc(mpc)=mpcfreeold
                           nodempc(1,mpcfreeold)=kon(indexe+neigh(k,j))
                           nodempc(2,mpcfreeold)=l
                        enddo
                     enddo
!
                  endif
               enddo
            elseif((lakon(i)(1:2).eq.'CP').or.
     &             (lakon(i)(1:1).eq.'S').or.
     &             (lakon(i)(1:2).eq.'CA')) then
!
!              check for axial elements  
!
               if(lakon(i)(1:2).eq.'CA') then
                  axial=.true.
               else
                  axial=.false.
               endif
!
               indexe=ipkon(i)
!
!           localizing the nodes, thicknesses and normals for the
!           2-D element
!            
               do j=1,8
                  nodel(j)=kon(indexe+j)
                  indexx=iponor(1,indexe+j)
                  indexk=iponor(2,indexe+j)
                  thicks(j)=thicke(1,indexe+j)
                  do k=1,3
                     xnors(k,j)=xnor(indexx+k)
                  enddo
                  do k=1,3
                     nodes(k,j)=knor(indexk+k)
                  enddo
                  if(ntrans.gt.0) then
c                     if(.not.axial) then
                        do k=1,3
                           inotr(1,nodes(k,j))=inotr(1,nodel(j))
                        enddo
c                     else
c                        do k=1,3
c                           inotr(1,nodes(k,j))=itransaxial
c                        enddo
c                     endif
                  endif
               enddo
!
!           generating the 3-D element topology for shell and plane
!           stress/strain elements
!
               if(lakon(i)(1:2).ne.'CA') then
                  do k=1,4
                     kon(indexe+k)=nodes(1,k)
!
                     do j=1,3
                        co(j,nodes(1,k))=co(j,nodel(k))
     &                       -thicks(k)*xnors(j,k)*(.5d0+offset(1,i))
                     enddo
                  enddo
                  do k=1,4
                     kon(indexe+4+k)=nodes(3,k)
                     do j=1,3
                        co(j,nodes(3,k))=co(j,nodel(k))
     &                       +thicks(k)*xnors(j,k)*(.5d0-offset(1,i))
                     enddo
                  enddo
                  do k=5,8
                     kon(indexe+4+k)=nodes(1,k)
                     do j=1,3
                        co(j,nodes(1,k))=co(j,nodel(k))
     &                       -thicks(k)*xnors(j,k)*(.5d0+offset(1,i))
                     enddo
                  enddo
                  do k=5,8
                     kon(indexe+8+k)=nodes(3,k)
                     do j=1,3
                        co(j,nodes(3,k))=co(j,nodel(k))
     &                       +thicks(k)*xnors(j,k)*(.5d0-offset(1,i))
                     enddo
                  enddo
                  do k=1,4
                     kon(indexe+16+k)=nodes(2,k)
                     do j=1,3
                        co(j,nodes(2,k))=co(j,nodel(k))
     &                       -thicks(k)*xnors(j,k)*offset(1,i)
                     enddo
                  enddo
               else
!
!           generating the 3-D element topology for axisymmetric elements
!
                  do k=1,8
                     dc(k)=dcos(thicks(k)/2.d0)
                     ds(k)=dsin(thicks(k)/2.d0)
                  enddo
                  do k=1,4
                     kon(indexe+k)=nodes(1,k)
                     co(1,nodes(1,k))=co(1,nodel(k))*dc(k)
                     co(2,nodes(1,k))=co(2,nodel(k))
                     co(3,nodes(1,k))=-co(1,nodel(k))*ds(k)
                  enddo
                  do k=1,4
                     kon(indexe+4+k)=nodes(3,k)
                     co(1,nodes(3,k))=co(1,nodel(k))*dc(k)
                     co(2,nodes(3,k))=co(2,nodel(k))
                     co(3,nodes(3,k))=co(1,nodel(k))*ds(k)
                  enddo
                  do k=5,8
                     kon(indexe+4+k)=nodes(1,k)
                     co(1,nodes(1,k))=co(1,nodel(k))*dc(k)
                     co(2,nodes(1,k))=co(2,nodel(k))
                     co(3,nodes(1,k))=-co(1,nodel(k))*ds(k)
                  enddo
                  do k=5,8
                     kon(indexe+8+k)=nodes(3,k)
                     co(1,nodes(3,k))=co(1,nodel(k))*dc(k)
                     co(2,nodes(3,k))=co(2,nodel(k))
                     co(3,nodes(3,k))=co(1,nodel(k))*ds(k)
                  enddo
                  do k=1,4
                     kon(indexe+16+k)=nodes(2,k)
                     co(1,nodes(2,k))=co(1,nodel(k))
                     co(2,nodes(2,k))=co(2,nodel(k))
                     co(3,nodes(2,k))=0.d0
                  enddo
               endif
!
!           additional SPC's due to plane strain/plane stress/axisymmetric
!           conditions
!
               do j=1,8
                  if(lakon(i)(1:1).ne.'S') then
!
!                    fixing the middle plane
!
                     if(rig(nodel(j)).eq.0) then
                        val=0.d0
                        k=3
                        if(nam.gt.0) iamplitude=0
                        call bounadd(nodes(2,j),k,k,val,nodeboun,
     &                       ndirboun,xboun,nboun,nboun_,iamboun,
     &                       iamplitude,nam,ipompc,nodempc,coefmpc,
     &                       nmpc,nmpc_,mpcfree,inotr,trab,ntrans,
     &                       ikboun,ilboun,ikmpc,ilmpc,co,nk,nk_,
     &                       labmpc)
                     endif
!
!                    specifying that the side planes do the same
!                    as the middle plane (in all directions for
!                    plane strain and axisymmetric elements, in the
!                    plane for plane stress elements)
!
                     do l=1,3,2
                        newnode=nodes(l,j)
                        do idir=1,3
                           if((idir.eq.3).and.(lakon(i)(1:3).eq.'CPS'))
     &                        cycle
                           idof=7*(newnode-1)+idir
                           call nident(ikmpc,idof,nmpc,id)
                           if((id.le.0).or.(ikmpc(id).ne.idof)) then
                              nmpc=nmpc+1
                              if(nmpc.gt.nmpc_) then
                                 write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                                 stop
                              endif
                              labmpc(nmpc)='                    '
                              ipompc(nmpc)=mpcfree
                              do m=nmpc,id+2,-1
                                 ikmpc(m)=ikmpc(m-1)
                                 ilmpc(m)=ilmpc(m-1)
                              enddo
                              ikmpc(id+1)=idof
                              ilmpc(id+1)=nmpc
                              nodempc(1,mpcfree)=newnode
                              nodempc(2,mpcfree)=idir
                              coefmpc(mpcfree)=1.d0
                              mpcfree=nodempc(3,mpcfree)
                              if(mpcfree.eq.0) then
                                 write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                                 stop
                              endif
                              nodempc(1,mpcfree)=nodes(2,j)
                              if((lakon(i)(2:2).eq.'A').and.(idir.eq.3))
     &                           then
                                 nodempc(2,mpcfree)=1
                              else
                                 nodempc(2,mpcfree)=idir
                              endif
                              if(lakon(i)(2:2).eq.'A') then
                                 if(idir.eq.1) then
                                    coefmpc(mpcfree)=-dc(j)
                                 elseif(idir.eq.2) then
                                    coefmpc(mpcfree)=-1.d0
                                 else
                                    if(l.eq.1) then
                                       coefmpc(mpcfree)=ds(j)
                                    else
                                       coefmpc(mpcfree)=-ds(j)
                                    endif
                                 endif
                              else
                                 coefmpc(mpcfree)=-1.d0
                              endif
                              mpcfreenew=nodempc(3,mpcfree)
                              if(mpcfreenew.eq.0) then
                                 write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                                 stop
                              endif
                              nodempc(3,mpcfree)=0
                              mpcfree=mpcfreenew
                           endif
                        enddo
                     enddo
                  endif
               enddo
!            
            elseif(lakon(i)(1:1).eq.'B') then
               indexe=ipkon(i)
!
!           localizing the nodes, thicknesses and normals for the
!           beam element
!            
               do j=1,3
                  nodel(j)=kon(indexe+j)
                  indexx=iponor(1,indexe+j)
                  indexk=iponor(2,indexe+j)
                  thickb(1,j)=thicke(1,indexe+j)
                  thickb(2,j)=thicke(2,indexe+j)
                  do k=1,6
                     xnorb(k,j)=xnor(indexx+k)
                  enddo
                  do k=1,8
                     nodeb(k,j)=knor(indexk+k)
                  enddo
                  if(ntrans.gt.0) then
                     do k=1,8
                        inotr(1,nodeb(k,j))=inotr(1,nodel(j))
                     enddo
                  endif
               enddo
!
!           generating the 3-D element topology for beam elements
!
               if(lakon(i)(8:8).eq.'R') then
                  kon(indexe+1)=nodeb(1,1)
                  do j=1,3
                     co(j,nodeb(1,1))=co(j,nodel(1))
     &                    -thickb(1,1)*xnorb(j,1)*(.5d0+offset(1,i))
     &                    +thickb(2,1)*xnorb(j+3,1)*(.5d0-offset(2,i))
                  enddo
                  kon(indexe+2)=nodeb(1,3)
                  do j=1,3
                     co(j,nodeb(1,3))=co(j,nodel(3))
     &                    -thickb(1,3)*xnorb(j,3)*(.5d0+offset(1,i))
     &                    +thickb(2,3)*xnorb(j+3,3)*(.5d0-offset(2,i))
                  enddo
                  kon(indexe+3)=nodeb(2,3)
                  do j=1,3
                     co(j,nodeb(2,3))=co(j,nodel(3))
     &                    -thickb(1,3)*xnorb(j,3)*(.5d0+offset(1,i))
     &                    -thickb(2,3)*xnorb(j+3,3)*(.5d0+offset(2,i))
                  enddo
                  kon(indexe+4)=nodeb(2,1)
                  do j=1,3
                     co(j,nodeb(2,1))=co(j,nodel(1))
     &                    -thickb(1,1)*xnorb(j,1)*(.5d0+offset(1,i))
     &                    -thickb(2,1)*xnorb(j+3,1)*(.5d0+offset(2,i))
                  enddo
                  kon(indexe+5)=nodeb(4,1)
                  do j=1,3
                     co(j,nodeb(4,1))=co(j,nodel(1))
     &                    +thickb(1,1)*xnorb(j,1)*(.5d0-offset(1,i))
     &                    +thickb(2,1)*xnorb(j+3,1)*(.5d0-offset(2,i))
                  enddo
                  kon(indexe+6)=nodeb(4,3)
                  do j=1,3
                     co(j,nodeb(4,3))=co(j,nodel(3))
     &                    +thickb(1,3)*xnorb(j,3)*(.5d0-offset(1,i))
     &                    +thickb(2,3)*xnorb(j+3,3)*(.5d0-offset(2,i))
                  enddo
                  kon(indexe+7)=nodeb(3,3)
                  do j=1,3
                     co(j,nodeb(3,3))=co(j,nodel(3))
     &                    +thickb(1,3)*xnorb(j,3)*(.5d0-offset(1,i))
     &                    -thickb(2,3)*xnorb(j+3,3)*(.5d0+offset(2,i))
                  enddo
                  kon(indexe+8)=nodeb(3,1)
                  do j=1,3
                     co(j,nodeb(3,1))=co(j,nodel(1))
     &                    +thickb(1,1)*xnorb(j,1)*(.5d0-offset(1,i))
     &                    -thickb(2,1)*xnorb(j+3,1)*(.5d0+offset(2,i))
                  enddo
                  kon(indexe+9)=nodeb(1,2)
                  do j=1,3
                     co(j,nodeb(1,2))=co(j,nodel(2))
     &                    -thickb(1,2)*xnorb(j,2)*(.5d0+offset(1,i))
     &                    +thickb(2,2)*xnorb(j+3,2)*(.5d0-offset(2,i))
                  enddo
                  kon(indexe+10)=nodeb(5,3)
                  do j=1,3
                     co(j,nodeb(5,3))=co(j,nodel(3))
     &                    -thickb(1,3)*xnorb(j,3)*(.5d0+offset(1,i))
     &                    -thickb(2,3)*xnorb(j+3,3)*offset(2,i)
                  enddo
                  kon(indexe+11)=nodeb(2,2)
                  do j=1,3
                     co(j,nodeb(2,2))=co(j,nodel(2))
     &                    -thickb(1,2)*xnorb(j,2)*(.5d0+offset(1,i))
     &                    -thickb(2,2)*xnorb(j+3,2)*(.5d0+offset(2,i))
                  enddo
                  kon(indexe+12)=nodeb(5,1)
                  do j=1,3
                     co(j,nodeb(5,1))=co(j,nodel(1))
     &                    -thickb(1,1)*xnorb(j,1)*(.5d0+offset(1,i))
     &                    -thickb(2,1)*xnorb(j+3,1)*offset(2,i)
                  enddo
                  kon(indexe+13)=nodeb(4,2)
                  do j=1,3
                     co(j,nodeb(4,2))=co(j,nodel(2))
     &                    +thickb(1,2)*xnorb(j,2)*(.5d0-offset(1,i))
     &                    +thickb(2,2)*xnorb(j+3,2)*(.5d0-offset(2,i))
                  enddo
                  kon(indexe+14)=nodeb(7,3)
                  do j=1,3
                     co(j,nodeb(7,3))=co(j,nodel(3))
     &                    +thickb(1,3)*xnorb(j,3)*(.5d0-offset(1,i))
     &                    -thickb(2,3)*xnorb(j+3,3)*offset(2,i)
                  enddo
                  kon(indexe+15)=nodeb(3,2)
                  do j=1,3
                     co(j,nodeb(3,2))=co(j,nodel(2))
     &                    +thickb(1,2)*xnorb(j,2)*(.5d0-offset(1,i))
     &                    -thickb(2,2)*xnorb(j+3,2)*(.5d0+offset(2,i))
                  enddo
                  kon(indexe+16)=nodeb(7,1)
                  do j=1,3
                     co(j,nodeb(7,1))=co(j,nodel(1))
     &                    +thickb(1,1)*xnorb(j,1)*(.5d0-offset(1,i))
     &                    -thickb(2,1)*xnorb(j+3,1)*offset(2,i)
                  enddo
                  kon(indexe+17)=nodeb(8,1)
                  do j=1,3
                     co(j,nodeb(8,1))=co(j,nodel(1))
     &                    -thickb(1,1)*xnorb(j,1)*offset(1,i)
     &                    +thickb(2,1)*xnorb(j+3,1)*(.5d0-offset(2,i))
                  enddo
                  kon(indexe+18)=nodeb(8,3)
                  do j=1,3
                     co(j,nodeb(8,3))=co(j,nodel(3))
     &                    -thickb(1,3)*xnorb(j,3)*offset(1,i)
     &                    +thickb(2,3)*xnorb(j+3,3)*(.5d0-offset(2,i))
                  enddo
                  kon(indexe+19)=nodeb(6,3)
                  do j=1,3
                     co(j,nodeb(6,3))=co(j,nodel(3))
     &                    -thickb(1,3)*xnorb(j,3)*offset(1,i)
     &                    -thickb(2,3)*xnorb(j+3,3)*(.5d0+offset(2,i))
                  enddo
                  kon(indexe+20)=nodeb(6,1)
                  do j=1,3
                     co(j,nodeb(6,1))=co(j,nodel(1))
     &                    -thickb(1,1)*xnorb(j,1)*offset(1,i)
     &                    -thickb(2,1)*xnorb(j+3,1)*(.5d0+offset(2,i))
                  enddo
               else
!
!                 circular cross section
!
                  sc=.5d0/dsqrt(2.d0)
                  kon(indexe+1)=nodeb(1,1)
                  do j=1,3
                     co(j,nodeb(1,1))=co(j,nodel(1))
     &                    -thickb(1,1)*xnorb(j,1)*(sc+offset(1,i))
     &                    +thickb(2,1)*xnorb(j+3,1)*(sc-offset(2,i))
                  enddo
                  kon(indexe+2)=nodeb(1,3)
                  do j=1,3
                     co(j,nodeb(1,3))=co(j,nodel(3))
     &                    -thickb(1,3)*xnorb(j,3)*(sc+offset(1,i))
     &                    +thickb(2,3)*xnorb(j+3,3)*(sc-offset(2,i))
                  enddo
                  kon(indexe+3)=nodeb(2,3)
                  do j=1,3
                     co(j,nodeb(2,3))=co(j,nodel(3))
     &                    -thickb(1,3)*xnorb(j,3)*(sc+offset(1,i))
     &                    -thickb(2,3)*xnorb(j+3,3)*(sc+offset(2,i))
                  enddo
                  kon(indexe+4)=nodeb(2,1)
                  do j=1,3
                     co(j,nodeb(2,1))=co(j,nodel(1))
     &                    -thickb(1,1)*xnorb(j,1)*(sc+offset(1,i))
     &                    -thickb(2,1)*xnorb(j+3,1)*(sc+offset(2,i))
                  enddo
                  kon(indexe+5)=nodeb(4,1)
                  do j=1,3
                     co(j,nodeb(4,1))=co(j,nodel(1))
     &                    +thickb(1,1)*xnorb(j,1)*(sc-offset(1,i))
     &                    +thickb(2,1)*xnorb(j+3,1)*(sc-offset(2,i))
                  enddo
                  kon(indexe+6)=nodeb(4,3)
                  do j=1,3
                     co(j,nodeb(4,3))=co(j,nodel(3))
     &                    +thickb(1,3)*xnorb(j,3)*(sc-offset(1,i))
     &                    +thickb(2,3)*xnorb(j+3,3)*(sc-offset(2,i))
                  enddo
                  kon(indexe+7)=nodeb(3,3)
                  do j=1,3
                     co(j,nodeb(3,3))=co(j,nodel(3))
     &                    +thickb(1,3)*xnorb(j,3)*(sc-offset(1,i))
     &                    -thickb(2,3)*xnorb(j+3,3)*(sc+offset(2,i))
                  enddo
                  kon(indexe+8)=nodeb(3,1)
                  do j=1,3
                     co(j,nodeb(3,1))=co(j,nodel(1))
     &                    +thickb(1,1)*xnorb(j,1)*(sc-offset(1,i))
     &                    -thickb(2,1)*xnorb(j+3,1)*(sc+offset(2,i))
                  enddo
                  kon(indexe+9)=nodeb(1,2)
                  do j=1,3
                     co(j,nodeb(1,2))=co(j,nodel(2))
     &                    -thickb(1,2)*xnorb(j,2)*(sc+offset(1,i))
     &                    +thickb(2,2)*xnorb(j+3,2)*(sc-offset(2,i))
                  enddo
                  kon(indexe+10)=nodeb(5,3)
                  do j=1,3
                     co(j,nodeb(5,3))=co(j,nodel(3))
     &                    -thickb(1,3)*xnorb(j,3)*(.5d0+offset(1,i))
     &                    -thickb(2,3)*xnorb(j+3,3)*offset(2,i)
                  enddo
                  kon(indexe+11)=nodeb(2,2)
                  do j=1,3
                     co(j,nodeb(2,2))=co(j,nodel(2))
     &                    -thickb(1,2)*xnorb(j,2)*(sc+offset(1,i))
     &                    -thickb(2,2)*xnorb(j+3,2)*(sc+offset(2,i))
                  enddo
                  kon(indexe+12)=nodeb(5,1)
                  do j=1,3
                     co(j,nodeb(5,1))=co(j,nodel(1))
     &                    -thickb(1,1)*xnorb(j,1)*(.5d0+offset(1,i))
     &                    -thickb(2,1)*xnorb(j+3,1)*offset(2,i)
                  enddo
                  kon(indexe+13)=nodeb(4,2)
                  do j=1,3
                     co(j,nodeb(4,2))=co(j,nodel(2))
     &                    +thickb(1,2)*xnorb(j,2)*(sc-offset(1,i))
     &                    +thickb(2,2)*xnorb(j+3,2)*(sc-offset(2,i))
                  enddo
                  kon(indexe+14)=nodeb(7,3)
                  do j=1,3
                     co(j,nodeb(7,3))=co(j,nodel(3))
     &                    +thickb(1,3)*xnorb(j,3)*(.5d0-offset(1,i))
     &                    -thickb(2,3)*xnorb(j+3,3)*offset(2,i)
                  enddo
                  kon(indexe+15)=nodeb(3,2)
                  do j=1,3
                     co(j,nodeb(3,2))=co(j,nodel(2))
     &                    +thickb(1,2)*xnorb(j,2)*(sc-offset(1,i))
     &                    -thickb(2,2)*xnorb(j+3,2)*(sc+offset(2,i))
                  enddo
                  kon(indexe+16)=nodeb(7,1)
                  do j=1,3
                     co(j,nodeb(7,1))=co(j,nodel(1))
     &                    +thickb(1,1)*xnorb(j,1)*(.5d0-offset(1,i))
     &                    -thickb(2,1)*xnorb(j+3,1)*offset(2,i)
                  enddo
                  kon(indexe+17)=nodeb(8,1)
                  do j=1,3
                     co(j,nodeb(8,1))=co(j,nodel(1))
     &                    -thickb(1,1)*xnorb(j,1)*offset(1,i)
     &                    +thickb(2,1)*xnorb(j+3,1)*(.5d0-offset(2,i))
                  enddo
                  kon(indexe+18)=nodeb(8,3)
                  do j=1,3
                     co(j,nodeb(8,3))=co(j,nodel(3))
     &                    -thickb(1,3)*xnorb(j,3)*offset(1,i)
     &                    +thickb(2,3)*xnorb(j+3,3)*(.5d0-offset(2,i))
                  enddo
                  kon(indexe+19)=nodeb(6,3)
                  do j=1,3
                     co(j,nodeb(6,3))=co(j,nodel(3))
     &                    -thickb(1,3)*xnorb(j,3)*offset(1,i)
     &                    -thickb(2,3)*xnorb(j+3,3)*(.5d0+offset(2,i))
                  enddo
                  kon(indexe+20)=nodeb(6,1)
                  do j=1,3
                     co(j,nodeb(6,1))=co(j,nodel(1))
     &                    -thickb(1,1)*xnorb(j,1)*offset(1,i)
     &                    -thickb(2,1)*xnorb(j+3,1)*(.5d0+offset(2,i))
                  enddo
               endif
            endif
!
            if(lakon(i)(1:5).eq.'CPE8R') then
               lakon(i)(1:7)='C3D20RE'
            elseif(lakon(i)(1:3).eq.'CPE') then
               lakon(i)(1:7)='C3D20 E'
            elseif(lakon(i)(1:5).eq.'CPS8R') then
               lakon(i)(1:7)='C3D20RS'
            elseif(lakon(i)(1:3).eq.'CPS') then
               lakon(i)(1:7)='C3D20 S'
            elseif(lakon(i)(1:5).eq.'CAX8R') then
               lakon(i)(1:7)='C3D20RA'
            elseif(lakon(i)(1:2).eq.'CA') then
               lakon(i)(1:7)='C3D20 A'
            elseif(lakon(i)(1:3).eq.'S8R') then
               lakon(i)(1:7)='C3D20RL'
            elseif(lakon(i)(1:1).eq.'S') then
               lakon(i)(1:7)='C3D20 L'
            elseif(lakon(i)(1:4).eq.'B32R') then
               lakon(i)(1:7)='C3D20RB'
            elseif(lakon(i)(1:1).eq.'B') then
               lakon(i)(1:7)='C3D20 B'
            endif
         enddo
      endif
!
!        generating MPC's to connect shells and beams with solid
!        elements
!
      if((inoelfree.ne.0).and.(istep.eq.1)) then
         do i=1,ne
            indexes=ipkon(i)
            if(indexes.lt.0) cycle
            if((lakon(i)(4:4).ne.'8').and.
     &           (lakon(i)(4:4).ne.'1').and.
     &           (lakon(i)(7:7).ne.' ')) cycle
            if(lakon(i)(4:4).eq.'8') then
               nope=8
            elseif(lakon(i)(4:4).eq.'1') then
               nope=10
            else
               nope=20
            endif
            do l=1,nope
               node=kon(indexes+l)
               if(node.le.iponoelmax) then
                  if(rig(node).eq.0) then
                     index2=iponoel(node)
                     ielem=inoel(1,index2)
                     indexe=ipkon(ielem)
                     j=inoel(2,index2)
                     indexk=iponor(2,indexe+j)
!
!                    2d shell element
!
                     if(lakon(ielem)(7:7).eq.'L') then
                        newnode=knor(indexk+1)
                        do idir=1,3
                           idof=7*(newnode-1)+idir
                           call nident(ikmpc,idof,nmpc,id)
                           if((id.le.0).or.(ikmpc(id).ne.idof)) then
                              nmpc=nmpc+1
                              if(nmpc.gt.nmpc_) then
                                 write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                                 stop
                              endif
                              labmpc(nmpc)='                    '
                              ipompc(nmpc)=mpcfree
                              do j=nmpc,id+2,-1
                                 ikmpc(j)=ikmpc(j-1)
                                 ilmpc(j)=ilmpc(j-1)
                              enddo
                              ikmpc(id+1)=idof
                              ilmpc(id+1)=nmpc
                              nodempc(1,mpcfree)=newnode
                              nodempc(2,mpcfree)=idir
                              coefmpc(mpcfree)=1.d0
                              mpcfree=nodempc(3,mpcfree)
                              if(mpcfree.eq.0) then
                                 write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                                 stop
                              endif
                              nodempc(1,mpcfree)=knor(indexk+3)
                              nodempc(2,mpcfree)=idir
                              coefmpc(mpcfree)=1.d0
                              mpcfree=nodempc(3,mpcfree)
                              if(mpcfree.eq.0) then
                                 write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                                 stop
                              endif
                              nodempc(1,mpcfree)=node
                              nodempc(2,mpcfree)=idir
                              coefmpc(mpcfree)=-2.d0
                              mpcfreenew=nodempc(3,mpcfree)
                              if(mpcfreenew.eq.0) then
                                 write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                                 stop
                              endif
                              nodempc(3,mpcfree)=0
                              mpcfree=mpcfreenew
                           endif
                        enddo
                     elseif(lakon(ielem)(7:7).eq.'B') then
!
!                       1d beam element
!
                        newnode=knor(indexk+1)
                        do idir=1,3
                           idof=7*(newnode-1)+idir
                           call nident(ikmpc,idof,nmpc,id)
                           if((id.le.0).or.(ikmpc(id).ne.idof)) then
                              nmpc=nmpc+1
                              if(nmpc.gt.nmpc_) then
                                 write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                                 stop
                              endif
                              labmpc(nmpc)='                    '
                              ipompc(nmpc)=mpcfree
                              do j=nmpc,id+2,-1
                                 ikmpc(j)=ikmpc(j-1)
                                 ilmpc(j)=ilmpc(j-1)
                              enddo
                              ikmpc(id+1)=idof
                              ilmpc(id+1)=nmpc
                              nodempc(1,mpcfree)=newnode
                              nodempc(2,mpcfree)=idir
                              coefmpc(mpcfree)=1.d0
                              mpcfree=nodempc(3,mpcfree)
                              if(mpcfree.eq.0) then
                                 write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                                 stop
                              endif
                              do k=2,4
                                 nodempc(1,mpcfree)=knor(indexk+k)
                                 nodempc(2,mpcfree)=idir
                                 coefmpc(mpcfree)=1.d0
                                 mpcfree=nodempc(3,mpcfree)
                                 if(mpcfree.eq.0) then
                                    write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                                    stop
                                 endif
                              enddo
                              nodempc(1,mpcfree)=node
                              nodempc(2,mpcfree)=idir
                              coefmpc(mpcfree)=-4.d0
                              mpcfreenew=nodempc(3,mpcfree)
                              if(mpcfreenew.eq.0) then
                                 write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                                 stop
                              endif
                              nodempc(3,mpcfree)=0
                              mpcfree=mpcfreenew
                           endif
                        enddo
                     else
!
!                       2d plane stress, plane strain or axisymmetric
!                       element
!
                        newnode=knor(indexk+2)
c                        do idir=1,3
                        do idir=1,2
                           idof=7*(newnode-1)+idir
                           call nident(ikmpc,idof,nmpc,id)
                           if((id.le.0).or.(ikmpc(id).ne.idof)) then
                              nmpc=nmpc+1
                              if(nmpc.gt.nmpc_) then
                                 write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                                 stop
                              endif
                              labmpc(nmpc)='                    '
                              ipompc(nmpc)=mpcfree
                              do j=nmpc,id+2,-1
                                 ikmpc(j)=ikmpc(j-1)
                                 ilmpc(j)=ilmpc(j-1)
                              enddo
                              ikmpc(id+1)=idof
                              ilmpc(id+1)=nmpc
                              nodempc(1,mpcfree)=newnode
                              nodempc(2,mpcfree)=idir
                              coefmpc(mpcfree)=1.d0
                              mpcfree=nodempc(3,mpcfree)
                              if(mpcfree.eq.0) then
                                 write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                                 stop
                              endif
                              nodempc(1,mpcfree)=node
                              nodempc(2,mpcfree)=idir
                              coefmpc(mpcfree)=-1.d0
                              mpcfreenew=nodempc(3,mpcfree)
                              if(mpcfreenew.eq.0) then
                                 write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                                 stop
                              endif
                              nodempc(3,mpcfree)=0
                              mpcfree=mpcfreenew
                           endif
                        enddo
                     endif
                  endif
               endif
            enddo
         enddo
      endif
!
      if(inoelfree.ne.0) then
!
!           multiplying existing boundary conditions
!
         nbounold=nboun
         do i=1,nbounold
            node=nodeboun(i)
            if(node.gt.iponoelmax) then
               if(ndirboun(i).gt.3) then
                  write(*,*) '*WARNING: in gen3delem: node ',i,
     &                   ' does not'
                  write(*,*) '       belong to a beam nor shell'
                  write(*,*) '       element and consequently has no'
                  write(*,*) '       rotational degrees of freedom'
               endif
               cycle
            endif
            index=iponoel(node)
            if(index.eq.0) then
               if(ndirboun(i).gt.3) then
                  write(*,*) '*WARNING: in gen3delem: node ',i,
     &                 ' does not'
                  write(*,*) '       belong to a beam nor shell'
                  write(*,*) '       element and consequently has no'
                  write(*,*) '       rotational degrees of freedom'
               endif
               cycle
            endif
            ielem=inoel(1,index)
            j=inoel(2,index)
            indexe=ipkon(ielem)
            indexk=iponor(2,indexe+j)
            idir=ndirboun(i)
            val=xboun(i)
            if(nam.gt.0) iamplitude=iamboun(i)
!
            if(rig(node).ne.0) then
               if(idir.gt.3) then
c               node=knor(indexk+1)
c               idof=7*(node-1)+1
c               call nident(ikmpc,idof,nmpc,id)
c               if((id.eq.0).or.(ikmpc(id).ne.idof)) then
c                  write(*,*) '*ERROR in gen3delem: RIGID BODY MPC'
c                  write(*,*) '       is lacking'
c                  stop
c               endif
c               mpc=ilmpc(id)
cc               val=0.d0
cc               if(idir.le.3) then
cc                  irefnode=nodempc(1,nodempc(3,ipompc(mpc)))
cc                  call bounadd(irefnode,idir,idir,val,nodeboun,
cc     &                 ndirboun,xboun,nboun,nboun_,iamboun,
cc     &                 iamplitude,nam,ipompc,nodempc,coefmpc,
cc     &                 nmpc,nmpc_,mpcfree,inotr,trab,ntrans,
cc     &                 ikboun,ilboun,ikmpc,ilmpc,co,nk,nk_,labmpc)
cc               else
                  j=idir-3
                  irotnode=rig(node)
c                  irotnode=nodempc(1,nodempc(3,nodempc(3,ipompc(mpc))))
                  call bounadd(irotnode,j,j,val,nodeboun,
     &                 ndirboun,xboun,nboun,nboun_,iamboun,
     &                 iamplitude,nam,ipompc,nodempc,coefmpc,
     &                 nmpc,nmpc_,mpcfree,inotr,trab,ntrans,
     &                 ikboun,ilboun,ikmpc,ilmpc,co,nk,nk_,labmpc)
               endif
            else
!
!                    2d element shell element: generate MPC's
!
               if(lakon(ielem)(7:7).eq.'L') then
                  newnode=knor(indexk+1)
                  idof=7*(newnode-1)+idir
                  call nident(ikmpc,idof,nmpc,id)
                  if((id.le.0).or.(ikmpc(id).ne.idof)) then
                     nmpc=nmpc+1
                     if(nmpc.gt.nmpc_) then
                        write(*,*) 
     &                       '*ERROR in gen3delem: increase nmpc_'
                        stop
                     endif
                     labmpc(nmpc)='                    '
                     ipompc(nmpc)=mpcfree
                     do j=nmpc,id+2,-1
                        ikmpc(j)=ikmpc(j-1)
                        ilmpc(j)=ilmpc(j-1)
                     enddo
                     ikmpc(id+1)=idof
                     ilmpc(id+1)=nmpc
                     nodempc(1,mpcfree)=newnode
                     nodempc(2,mpcfree)=idir
                     coefmpc(mpcfree)=1.d0
                     mpcfree=nodempc(3,mpcfree)
                     if(mpcfree.eq.0) then
                        write(*,*) 
     &                       '*ERROR in gen3delem: increase nmpc_'
                        stop
                     endif
                     nodempc(1,mpcfree)=knor(indexk+3)
                     nodempc(2,mpcfree)=idir
                     coefmpc(mpcfree)=1.d0
                     mpcfree=nodempc(3,mpcfree)
                     if(mpcfree.eq.0) then
                        write(*,*) 
     &                       '*ERROR in gen3delem: increase nmpc_'
                        stop
                     endif
                     nodempc(1,mpcfree)=node
                     nodempc(2,mpcfree)=idir
                     coefmpc(mpcfree)=-2.d0
                     mpcfreenew=nodempc(3,mpcfree)
                     if(mpcfreenew.eq.0) then
                        write(*,*) 
     &                       '*ERROR in gen3delem: increase nmpc_'
                        stop
                     endif
                     nodempc(3,mpcfree)=0
                     mpcfree=mpcfreenew
                  endif
               elseif(lakon(ielem)(7:7).eq.'B') then
!
!                       1d beam element: generate MPC's
!
                  newnode=knor(indexk+1)
                  idof=7*(newnode-1)+idir
                  call nident(ikmpc,idof,nmpc,id)
                  if((id.le.0).or.(ikmpc(id).ne.idof)) then
                     nmpc=nmpc+1
                     if(nmpc.gt.nmpc_) then
                        write(*,*) 
     &                       '*ERROR in gen3delem: increase nmpc_'
                        stop
                     endif
                     labmpc(nmpc)='                    '
                     ipompc(nmpc)=mpcfree
                     do j=nmpc,id+2,-1
                        ikmpc(j)=ikmpc(j-1)
                        ilmpc(j)=ilmpc(j-1)
                     enddo
                     ikmpc(id+1)=idof
                     ilmpc(id+1)=nmpc
                     nodempc(1,mpcfree)=newnode
                     nodempc(2,mpcfree)=idir
                     coefmpc(mpcfree)=1.d0
                     mpcfree=nodempc(3,mpcfree)
                     if(mpcfree.eq.0) then
                        write(*,*) 
     &                       '*ERROR in gen3delem: increase nmpc_'
                        stop
                     endif
                     do k=2,4
                        nodempc(1,mpcfree)=knor(indexk+k)
                        nodempc(2,mpcfree)=idir
                        coefmpc(mpcfree)=1.d0
                        mpcfree=nodempc(3,mpcfree)
                        if(mpcfree.eq.0) then
                           write(*,*) 
     &                          '*ERROR in gen3delem: increase nmpc_'
                           stop
                        endif
                     enddo
                     nodempc(1,mpcfree)=node
                     nodempc(2,mpcfree)=idir
                     coefmpc(mpcfree)=-4.d0
                     mpcfreenew=nodempc(3,mpcfree)
                     if(mpcfreenew.eq.0) then
                        write(*,*) 
     &                       '*ERROR in gen3delem: increase nmpc_'
                        stop
                     endif
                     nodempc(3,mpcfree)=0
                     mpcfree=mpcfreenew
                  endif
               else
!
!                       2d plane stress, plane strain or axisymmetric
!                       element: SPC
!
                  node=knor(indexk+2)
                  call bounadd(node,idir,idir,val,nodeboun,
     &                 ndirboun,xboun,nboun,nboun_,iamboun,
     &                 iamplitude,nam,ipompc,nodempc,coefmpc,
     &                 nmpc,nmpc_,mpcfree,inotr,trab,ntrans,
     &                 ikboun,ilboun,ikmpc,ilmpc,co,nk,nk_,labmpc)
               endif
            endif
         enddo
!
!        updating the MPCs: establishing links between the user
!        defined nodes and the newly generated nodes (mid-nodes
!        for 2d elements, mean of corner nodes for 1d elements)
!
         do i=1,nmpc
            index1=ipompc(i)
            do
               node=nodempc(1,index1)
               if(node.le.iponoelmax) then
                  if(rig(node).ne.0) then
                     if(nodempc(2,index1).gt.3) then
                        nodempc(1,index1)=rig(node)
                        nodempc(2,index1)=nodempc(2,index1)-3
                     endif
                  else
                     index2=iponoel(node)
                     ielem=inoel(1,index2)
                     indexe=ipkon(ielem)
                     j=inoel(2,index2)
                     indexk=iponor(2,indexe+j)
!
!                    2d element shell element
!
                     if(lakon(ielem)(7:7).eq.'L') then
                        newnode=knor(indexk+1)
                        idir=nodempc(2,index1)
                        idof=7*(newnode-1)+idir
                        call nident(ikmpc,idof,nmpc,id)
                        if((id.le.0).or.(ikmpc(id).ne.idof)) then
                           nmpc=nmpc+1
                           if(nmpc.gt.nmpc_) then
                              write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                              stop
                           endif
                           labmpc(nmpc)='                    '
                           ipompc(nmpc)=mpcfree
                           do j=nmpc,id+2,-1
                              ikmpc(j)=ikmpc(j-1)
                              ilmpc(j)=ilmpc(j-1)
                           enddo
                           ikmpc(id+1)=idof
                           ilmpc(id+1)=nmpc
                           nodempc(1,mpcfree)=newnode
                           nodempc(2,mpcfree)=idir
                           coefmpc(mpcfree)=1.d0
                           mpcfree=nodempc(3,mpcfree)
                           if(mpcfree.eq.0) then
                              write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                              stop
                           endif
                           nodempc(1,mpcfree)=knor(indexk+3)
                           nodempc(2,mpcfree)=idir
                           coefmpc(mpcfree)=1.d0
                           mpcfree=nodempc(3,mpcfree)
                           if(mpcfree.eq.0) then
                              write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                              stop
                           endif
                           nodempc(1,mpcfree)=node
                           nodempc(2,mpcfree)=idir
                           coefmpc(mpcfree)=-2.d0
                           mpcfreenew=nodempc(3,mpcfree)
                           if(mpcfreenew.eq.0) then
                              write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                              stop
                           endif
                           nodempc(3,mpcfree)=0
                           mpcfree=mpcfreenew
                        endif
                     elseif(lakon(ielem)(7:7).eq.'B') then
!
!                       1d beam element
!
                        newnode=knor(indexk+1)
                        idir=nodempc(2,index1)
                        idof=7*(newnode-1)+idir
                        call nident(ikmpc,idof,nmpc,id)
                        if((id.le.0).or.(ikmpc(id).ne.idof)) then
                           nmpc=nmpc+1
                           if(nmpc.gt.nmpc_) then
                              write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                              stop
                           endif
                           labmpc(nmpc)='                    '
                           ipompc(nmpc)=mpcfree
                           do j=nmpc,id+2,-1
                              ikmpc(j)=ikmpc(j-1)
                              ilmpc(j)=ilmpc(j-1)
                           enddo
                           ikmpc(id+1)=idof
                           ilmpc(id+1)=nmpc
                           nodempc(1,mpcfree)=newnode
                           nodempc(2,mpcfree)=idir
                           coefmpc(mpcfree)=1.d0
                           mpcfree=nodempc(3,mpcfree)
                           if(mpcfree.eq.0) then
                              write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                              stop
                           endif
                           do k=2,4
                              nodempc(1,mpcfree)=knor(indexk+k)
                              nodempc(2,mpcfree)=idir
                              coefmpc(mpcfree)=1.d0
                              mpcfree=nodempc(3,mpcfree)
                              if(mpcfree.eq.0) then
                                 write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                                 stop
                              endif
                           enddo
                           nodempc(1,mpcfree)=node
                           nodempc(2,mpcfree)=idir
                           coefmpc(mpcfree)=-4.d0
                           mpcfreenew=nodempc(3,mpcfree)
                           if(mpcfreenew.eq.0) then
                              write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                              stop
                           endif
                           nodempc(3,mpcfree)=0
                           mpcfree=mpcfreenew
                        endif
                     else
!
!                       2d plane stress, plane strain or axisymmetric
!                       element
!
                        newnode=knor(indexk+2)
                        idir=nodempc(2,index1)
                        idof=7*(newnode-1)+idir
                        call nident(ikmpc,idof,nmpc,id)
                        if(((id.le.0).or.(ikmpc(id).ne.idof)).and.
     &                     (idir.ne.3)) then
                           nmpc=nmpc+1
                           if(nmpc.gt.nmpc_) then
                              write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                              stop
                           endif
                           labmpc(nmpc)='                    '
                           ipompc(nmpc)=mpcfree
                           do j=nmpc,id+2,-1
                              ikmpc(j)=ikmpc(j-1)
                              ilmpc(j)=ilmpc(j-1)
                           enddo
                           ikmpc(id+1)=idof
                           ilmpc(id+1)=nmpc
                           nodempc(1,mpcfree)=newnode
                           nodempc(2,mpcfree)=idir
                           coefmpc(mpcfree)=1.d0
                           mpcfree=nodempc(3,mpcfree)
                           if(mpcfree.eq.0) then
                              write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                              stop
                           endif
                           nodempc(1,mpcfree)=node
                           nodempc(2,mpcfree)=idir
                           coefmpc(mpcfree)=-1.d0
                           mpcfreenew=nodempc(3,mpcfree)
                           if(mpcfreenew.eq.0) then
                              write(*,*) 
     &                             '*ERROR in gen3delem: increase nmpc_'
                              stop
                           endif
                           nodempc(3,mpcfree)=0
                           mpcfree=mpcfreenew
                        endif
                     endif
                  endif
               endif
               index1=nodempc(3,index1)
               if(index1.eq.0) exit
            enddo
         enddo
!
!        updating the temperatures
!
         if(ithermal.ne.0) then
            do i=1,iponoelmax
               i1=i+nk_
               i2=i+2*nk_
               index=iponoel(i)
               do
                  if(index.eq.0) exit
                  ielem=inoel(1,index)
                  j=inoel(2,index)
                  indexe=ipkon(ielem)
                  indexk=iponor(2,indexe+j)
                  if((lakon(ielem)(7:7).eq.'E').or.
     &               (lakon(ielem)(7:7).eq.'A').or.
     &               (lakon(ielem)(7:7).eq.'S')) then
                     do k=1,3
                        node=knor(indexk+k)
                        t0(node)=t0(i)
                        t1(node)=t1(i)
                     enddo
                  elseif(lakon(ielem)(7:7).eq.'L') then
                     node=knor(indexk+1)
                     t0(node)=t0(i)
     &               -t0(i1)*thicke(1,indexe+j)*(0.5d0+offset(1,ielem))
                     t1(node)=t1(i)
     &               -t1(i1)*thicke(1,indexe+j)*(0.5d0+offset(1,ielem))
                     node=knor(indexk+2)
                     t0(node)=t0(i)
                     t1(node)=t1(i)
                     node=knor(indexk+3)
                     t0(node)=t0(i)
     &               +t0(i1)*thicke(1,indexe+j)*(0.5d0-offset(1,ielem))
                     t1(node)=t1(i)
     &               +t1(i1)*thicke(1,indexe+j)*(0.5d0-offset(1,ielem))
                  elseif(lakon(ielem)(7:7).eq.'B') then
                     node=knor(indexk+1)
                     t0(node)=t0(i)
     &               -t0(i2)*thicke(1,indexe+j)*(0.5d0+offset(1,ielem))
     &               +t0(i1)*thicke(2,indexe+j)*(0.5d0+offset(2,ielem))
                     t1(node)=t1(i)
     &               -t1(i2)*thicke(1,indexe+j)*(0.5d0+offset(1,ielem))
     &               +t1(i1)*thicke(2,indexe+j)*(0.5d0+offset(2,ielem))
                     node=knor(indexk+2)
                     t0(node)=t0(i)
     &               -t0(i2)*thicke(1,indexe+j)*(0.5d0+offset(1,ielem))
     &               -t0(i1)*thicke(2,indexe+j)*(0.5d0+offset(2,ielem))
                     t1(node)=t1(i)
     &               -t1(i2)*thicke(1,indexe+j)*(0.5d0+offset(1,ielem))
     &               -t1(i1)*thicke(2,indexe+j)*(0.5d0+offset(2,ielem))
                     node=knor(indexk+3)
                     t0(node)=t0(i)
     &               +t0(i2)*thicke(1,indexe+j)*(0.5d0+offset(1,ielem))
     &               -t0(i1)*thicke(2,indexe+j)*(0.5d0+offset(2,ielem))
                     t1(node)=t1(i)
     &               +t1(i2)*thicke(1,indexe+j)*(0.5d0+offset(1,ielem))
     &               -t1(i1)*thicke(2,indexe+j)*(0.5d0+offset(2,ielem))
                     node=knor(indexk+4)
                     t0(node)=t0(i)
     &               +t0(i2)*thicke(1,indexe+j)*(0.5d0+offset(1,ielem))
     &               +t0(i1)*thicke(2,indexe+j)*(0.5d0+offset(2,ielem))
                     t1(node)=t1(i)
     &               +t1(i2)*thicke(1,indexe+j)*(0.5d0+offset(1,ielem))
     &               +t1(i1)*thicke(2,indexe+j)*(0.5d0+offset(2,ielem))
                     node=knor(indexk+5)
                     t0(node)=t0(i)
     &               -t0(i2)*thicke(1,indexe+j)*(0.5d0+offset(1,ielem))
                     t1(node)=t1(i)
     &               -t1(i2)*thicke(1,indexe+j)*(0.5d0+offset(1,ielem))
                     node=knor(indexk+6)
                     t0(node)=t0(i)
     &               -t0(i1)*thicke(2,indexe+j)*(0.5d0+offset(2,ielem))
                     t1(node)=t1(i)
     &               -t1(i1)*thicke(2,indexe+j)*(0.5d0+offset(2,ielem))
                     node=knor(indexk+7)
                     t0(node)=t0(i)
     &               +t0(i2)*thicke(1,indexe+j)*(0.5d0+offset(1,ielem))
                     t1(node)=t1(i)
     &               +t1(i2)*thicke(1,indexe+j)*(0.5d0+offset(1,ielem))
                     node=knor(indexk+8)
                     t0(node)=t0(i)
     &               +t0(i1)*thicke(2,indexe+j)*(0.5d0+offset(2,ielem))
                     t1(node)=t1(i)
     &               +t1(i1)*thicke(2,indexe+j)*(0.5d0+offset(2,ielem))
                  endif
                  if(rig(node).eq.0) exit
                  index=inoel(3,index)
               enddo
            enddo
         endif
!
!        updating the concentrated loading
!
         do i=1,nforc
            node=nodeforc(i)
            if(node.gt.iponoelmax) then
               if(ndirboun(i).gt.3) then
                  write(*,*) '*WARNING: in gen3delem: node ',i,
     &                   ' does not'
                  write(*,*) '       belong to a beam nor shell'
                  write(*,*) '       element and consequently has no'
                  write(*,*) '       rotational degrees of freedom'
               endif
               cycle
            endif
            index=iponoel(node)
            if(index.eq.0) then
               if(ndirboun(i).gt.3) then
                  write(*,*) '*WARNING: in gen3delem: node ',i,
     &                 ' does not'
                  write(*,*) '       belong to a beam nor shell'
                  write(*,*) '       element and consequently has no'
                  write(*,*) '       rotational degrees of freedom'
               endif
               cycle
            endif
            ielem=inoel(1,index)
            j=inoel(2,index)
            indexe=ipkon(ielem)
            indexk=iponor(2,indexe+j)
            if(nam.gt.0) iamplitude=iamboun(i)
            idir=ndirforc(i)
!
            if(rig(node).ne.0) then
               if(idir.gt.3) then
c               node=knor(indexk+1)
c               idof=7*(node-1)+1
c               call nident(ikmpc,idof,nmpc,id)
c               if((id.eq.0).or.(ikmpc(id).ne.idof)) then
c                  write(*,*) '*ERROR in gen3delem: RIGID BODY MPC'
c                  write(*,*) '       is lacking'
c                  stop
c               endif
c               mpc=ilmpc(id)
               val=xforc(i)
               j=idir-3
               irotnode=rig(node)
c               irotnode=nodempc(1,nodempc(3,nodempc(3,ipompc(mpc))))
               call forcadd(irotnode,j,val,nodeforc,
     &              ndirforc,xforc,nforc,nforc_,iamforc,
     &              iamplitude,nam,ntrans,trab,inotr,co,
     &              ikforc,ilforc)
               endif
!
!           1d beam element
!
            elseif(lakon(ielem)(7:7).eq.'B') then
               if(j.eq.2) then
                  do k=1,4
                     node=knor(indexk+k)
                     val=xforc(i)/4.d0
                     call forcadd(node,idir,val,nodeforc,
     &                    ndirforc,xforc,nforc,nforc_,iamforc,
     &                    iamplitude,nam,ntrans,trab,inotr,co,
     &                    ikforc,ilforc)
                  enddo
               else
                  do k=1,8
                     node=knor(indexk+k)
                     if(k.le.4) then
                        val=-xforc(i)/12.d0
                     else
                        val=xforc(i)/3.d0
                     endif
                     call forcadd(node,idir,val,nodeforc,
     &                    ndirforc,xforc,nforc,nforc_,iamforc,
     &                    iamplitude,nam,ntrans,trab,inotr,co,
     &                    ikforc,ilforc)
                  enddo
               endif
!
!              2d shell element
!
            elseif(lakon(ielem)(7:7).eq.'L') then
               if(j.gt.4) then
                  do k=1,3,2
                     node=knor(indexk+k)
                     val=xforc(i)/2.d0
                     call forcadd(node,idir,val,nodeforc,
     &                    ndirforc,xforc,nforc,nforc_,iamforc,
     &                    iamplitude,nam,ntrans,trab,inotr,co,
     &                    ikforc,ilforc)
                  enddo
               else
                  do k=1,3
                     node=knor(indexk+k)
                     if(k.ne.2) then
                        val=xforc(i)/6.d0
                     else
                        val=xforc(i)*2.d0/3.d0
                     endif
                     call forcadd(node,idir,val,nodeforc,
     &                    ndirforc,xforc,nforc,nforc_,iamforc,
     &                    iamplitude,nam,ntrans,trab,inotr,co,
     &                    ikforc,ilforc)
                  enddo
               endif
!
!              2d plane strain, plane stress or axisymmetric 
!              element
!
            else
               node=knor(indexk+2)
               val=xforc(i)
               if(lakon(ielem)(7:7).eq.'A') then
                  val=val*thicke(1,indexe+j)/(2.d0*pi)
               endif
               call forcadd(node,idir,val,nodeforc,
     &              ndirforc,xforc,nforc,nforc_,iamforc,
     &              iamplitude,nam,ntrans,trab,inotr,co,
     &              ikforc,ilforc)
            endif
         enddo
      endif
!
      return
      end


