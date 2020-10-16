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
      subroutine radflowload(itg,matg,ntg,ntr,ntm,nodeflow,xflowact,
     &  ac,bc,nload,sideload,nelemload,xloadact,lakon,ipiv,ntmat_,vold,
     &  shcon,nshcon,ipkon,kon,co,pmid,e1,e2,e3,iptr,
     &  kontri,ntri,nloadtr,tarea,tenv,physcon,erad,f,ft,
     &  dist,idist,area,nflow)
!
      implicit none
!
      logical covered(10,10)
!
      character*5 sideload(*)
      character*8 lakonl,lakon(*)
!
      integer itg(*),matg(*),ntg,ntr,nodeflow(2,*),nflow,nload,
     &  nelemload(2,*),nope,nopes,mint2d,nrhs,ipiv(*),info,i,j,k,l,
     &  node,imat,ntmat_,id1,id2,id,ntm,ifaceq(8,6),ifacet(6,4),
     &  ifacew(8,5),node1,node2,nshcon(*),nelem,ig,index,konl(20),
     &  ipkon(*),kon(*),ncovered,kontri(3,*),iptr(*),nloadtr(*),
     &  i1,j1,istart,iend,jstart,jend,imin,imid,imax,iphi,ipsi,
     &  k1,kflag,idist(*),ndist,i2,i3,ng,idi,idj,ntri
!
      real*8 xflowact(*),ac(ntm,*),bc(ntm,1),xloadact(2,*),cp,h(2),
     &  xl2(0:3,8),coords(3),dxsj2,temp,xi,et,weight,xsj2(3),
     &  gastemp,vold(0:3,*),shcon(0:1,ntmat_,*),co(3,*),shp2(4,8),
     &  pmid(3,*),e3(4,*),e1(3,*),e2(3,*),p1(3),p2(3),p3(3),
     &  areamean,coordsmean(3),tarea(*),tenv(*),
     &  erad(*),q,fenv,e,ec,physcon(2),psimin,psimax,phimin,
     &  phimid,phimax,dummy,a(3,3),b(3,3),c(3,3),p1loc(3),p2loc(3),
     &  p3loc(3),ipphi(3),ippsi(3),c1,pphi(3),ppsi(3),ft(ntri,*),
     &  f(ntr,*),psi,phi,dint,cospsij,dir(3),dirloc(3),dist(*),
     &  area(*),dd,p21(3),p32(3)
!
      real*8 gauss2d1(2,1),gauss2d2(2,4),gauss2d3(2,9),gauss2d4(2,1),
     &  gauss2d5(2,3),gauss3d1(3,1),gauss3d2(3,8),gauss3d3(3,27),
     &  gauss3d4(3,1),gauss3d5(3,4),gauss3d6(3,15),gauss3d7(3,2),
     &  gauss3d8(3,9),gauss3d9(3,18),weight2d1(1),weight2d2(4),
     &  weight2d3(9),weight2d4(1),weight2d5(3),weight3d1(1),
     &  weight3d2(8),weight3d3(27),weight3d4(1),weight3d5(4),
     &  weight3d6(15),weight3d7(2),weight3d8(9),weight3d9(18)
!
      include "gauss.f"
!
      data ifaceq /4,3,2,1,11,10,9,12,
     &            5,6,7,8,13,14,15,16,
     &            1,2,6,5,9,18,13,17,
     &            2,3,7,6,10,19,14,18,
     &            3,4,8,7,11,20,15,19,
     &            4,1,5,8,12,17,16,20/
      data ifacet /1,3,2,7,6,5,
     &             1,2,4,5,9,8,
     &             2,3,4,6,10,9,
     &             1,4,3,8,10,7/
      data ifacew /1,3,2,9,8,7,0,0,
     &             4,5,6,10,11,12,0,0,
     &             1,2,5,4,7,14,10,13,
     &             2,3,6,5,8,15,11,14,
     &             4,6,3,1,12,15,9,13/
!
      dint=1.d0
      c1=1.d0
!
!     solving for the gas temperatures in forced convection
!
      if(ntg.gt.0) then
!
!        initialization of ac and bc
!
         do i=1,ntg
            do j=1,ntg
               ac(i,j)=0.d0
            enddo
            bc(i,1)=0.d0
         enddo
!
!        mass flow
!
         do i=1,nflow
            node1=nodeflow(1,i)
            node2=nodeflow(2,i)
!
!           check whether the nodes were used in the flux
!           definitions
!
            call nident(itg,node1,ntg,id1)
            if(id1.le.0) then
               cycle
            elseif(itg(id1).ne.node1) then
               cycle
            endif
            call nident(itg,node2,ntg,id2)
            if(id2.le.0) then
               cycle
            elseif(itg(id2).ne.node2) then
               cycle
            endif
!
            ac(id1,id1)=xflowact(i)
            ac(id2,id1)=-xflowact(i)
         enddo
!
!        specific heat of the gas
!               
         do i=1,ntg
            node=itg(i)
            gastemp=vold(0,node)
            imat=matg(i)
            call matdata_tg(imat,ntmat_,gastemp,shcon,nshcon,cp)
            do j=1,ntg
               ac(i,j)=ac(i,j)*cp
            enddo
         enddo
!
!        convection with the walls
!
         do i=1,nload
            if(sideload(i)(3:4).eq.'FC') then
               nelem=nelemload(1,i)
               lakonl=lakon(nelem)
               node=nelemload(2,i)
               call nident(itg,node,ntg,id)
!
!              calculate the area
!
               read(sideload(i)(2:2),'(i1)') ig
!
!              number of nodes and integration points in the face
!
               if(lakonl(4:4).eq.'2') then
                  nope=20
                  nopes=8
               elseif(lakonl(4:4).eq.'8') then
                  nope=8
                  nopes=4
               elseif(lakonl(4:5).eq.'10') then
                  nope=10
                  nopes=6
               elseif(lakonl(4:4).eq.'4') then
                  nope=4
                  nopes=3
               elseif(lakonl(4:5).eq.'15') then
                  nope=15
               else
                  nope=6
               endif
!
               if(lakonl(4:5).eq.'8R') then
                  mint2d=1
               elseif((lakonl(4:4).eq.'8').or.(lakonl(4:6).eq.'20R'))
     &            then
                  mint2d=4
               elseif(lakonl(4:4).eq.'2') then
                  mint2d=9
               elseif(lakonl(4:5).eq.'10') then
                  mint2d=3
               elseif(lakonl(4:4).eq.'4') then
                  mint2d=1
               endif
!
               if(lakonl(4:4).eq.'6') then
                  mint2d=1
                  if(ig.le.2) then
                     nopes=3
                  else
                     nopes=4
                  endif
               endif
               if(lakonl(4:5).eq.'15') then
                  if(ig.le.2) then
                     mint2d=3
                     nopes=6
                  else
                     mint2d=4
                     nopes=8
                  endif
               endif
!
!              connectivity of the element
!
               index=ipkon(nelem)
               if(index.lt.0) then
                  write(*,*) '*ERROR in radflowload: element ',nelem
                  write(*,*) '       is not defined'
                  stop
               endif
               do k=1,nope
                  konl(k)=kon(index+k)
               enddo
!
!              coordinates of the nodes belonging to the face
!
               if((nope.eq.20).or.(nope.eq.8)) then
                  do k=1,nopes
                     xl2(0,k)=vold(0,konl(ifaceq(k,ig)))
                     do j=1,3
                        xl2(j,k)=co(j,konl(ifaceq(k,ig)))+
     &                       vold(j,konl(ifaceq(k,ig)))
                     enddo
                  enddo
               elseif((nope.eq.10).or.(nope.eq.4)) then
                  do k=1,nopes
                     xl2(0,k)=vold(0,konl(ifacet(k,ig)))
                     do j=1,3
                        xl2(j,k)=co(j,konl(ifacet(k,ig)))+
     &                       vold(j,konl(ifacet(k,ig)))
                     enddo
                  enddo
               else
                  do k=1,nopes
                     xl2(0,k)=vold(0,konl(ifacew(k,ig)))
                     do j=1,3
                        xl2(j,k)=co(j,konl(ifacew(k,ig)))+
     &                       vold(j,konl(ifacew(k,ig)))
                     enddo
                  enddo
               endif
!
!              integration to obtain the area and the mean
!              temperature
!
               do l=1,mint2d
                  if((lakonl(4:5).eq.'8R').or.
     &                 ((lakonl(4:4).eq.'6').and.(nopes.eq.4))) then
                     xi=gauss2d1(1,l)
                     et=gauss2d1(2,l)
                     weight=weight2d1(l)
                  elseif((lakonl(4:4).eq.'8').or.
     &                    (lakonl(4:6).eq.'20R').or.
     &                    ((lakonl(4:5).eq.'15').and.(nopes.eq.8))) then
                     xi=gauss2d2(1,l)
                     et=gauss2d2(2,l)
                     weight=weight2d2(l)
                  elseif(lakonl(4:4).eq.'2') then
                     xi=gauss2d3(1,l)
                     et=gauss2d3(2,l)
                     weight=weight2d3(l)
                  elseif((lakonl(4:5).eq.'10').or.
     &                    ((lakonl(4:5).eq.'15').and.(nopes.eq.6))) then
                     xi=gauss2d5(1,l)
                     et=gauss2d5(2,l)
                     weight=weight2d5(l)
                  elseif((lakonl(4:4).eq.'4').or.
     &                    ((lakonl(4:4).eq.'6').and.(nopes.eq.3))) then
                     xi=gauss2d4(1,l)
                     et=gauss2d4(2,l)
                     weight=weight2d4(l)
                  endif
!
                  if(nopes.eq.8) then
                     call shape8q(xi,et,xl2,xsj2,shp2)
                  elseif(nopes.eq.4) then
                     call shape4q(xi,et,xl2,xsj2,shp2)
                  elseif(nopes.eq.6) then
                     call shape6tri(xi,et,xl2,xsj2,shp2)
                  else
                     call shape3tri(xi,et,xl2,xsj2,shp2)
                  endif
!
                  dxsj2=dsqrt(xsj2(1)*xsj2(1)+xsj2(2)*xsj2(2)+
     &                 xsj2(3)*xsj2(3))
!
                  temp=0.d0
                  do k=1,3
                     coords(k)=0.d0
                  enddo
                  do j=1,nopes
                     temp=temp+xl2(0,j)*shp2(4,j)
                     do k=1,3
                        coords(k)=coords(k)+xl2(k,j)*shp2(4,j)
                     enddo
                  enddo
!
                  if(sideload(i)(5:5).ne.'N') then
                     h(1)=xloadact(1,i)
                     ac(id,id)=ac(id,id)+h(1)*dxsj2*weight
                     bc(id,1)=bc(id,1)+shp2(4,l)*h(1)*dxsj2*weight
                  else
                  endif
               enddo
            endif
         enddo
!
!        solving the system
!
         nrhs=1
         call dgesv(ntg,nrhs,ac,ntm,ipiv,bc,ntm,info)
         if(info.ne.0) then
            write(*,*) '*ERROR in radflowload: singular matrix'
            stop
         endif
!
         do i=1,ntg
            vold(0,itg(i))=bc(i,1)
         enddo
!
      endif
!
!     cavity ratiation
!            
      if(ntr.gt.0) then
         ng=10
!
!        calculating the momentaneous center of the triangles,
!        area of the triangles and normal to the triangles
!
         do i=1,ntri
            i1=kontri(1,i)
            i2=kontri(2,i)
            i3=kontri(3,i)
            do j=1,3
               p1(j)=co(j,i1)
               p2(j)=co(j,i2)
               p3(j)=co(j,i3)
               pmid(j,i)=(p1(j)+p2(j)+p3(j))/3.d0
               p21(j)=p2(j)-p1(j)
               p32(j)=p3(j)-p2(j)
            enddo
!
!           normal to the triangle
!
            e3(1,i)=p21(2)*p32(3)-p32(2)*p21(3)
            e3(2,i)=p21(3)*p32(1)-p32(3)*p21(1)
            e3(3,i)=p21(1)*p32(2)-p32(1)*p21(2)
!
            dd=dsqrt(e3(1,i)*e3(1,i)+e3(2,i)*e3(2,i)+
     &               e3(3,i)*e3(3,i))
!
            do j=1,3
               e3(j,i)=e3(j,i)/dd
            enddo
!
!           area of the triangle
!
            area(i)=dd/2.d0
!
!           unit vector parallel to side 1-2
!            
            dd=dsqrt(p21(1)*p21(1)+p21(2)*p21(2)+p21(3)*p21(3))
            do j=1,3
               e1(j,i)=p21(j)/dd
            enddo
!
!           unit vector orthogonal to e1 and e3
!
            e2(1,i)=e3(2,i)*e1(3,i)-e3(3,i)*e1(2,i)
            e2(2,i)=e3(3,i)*e1(1,i)-e3(1,i)*e1(3,i)
            e2(3,i)=e3(1,i)*e1(2,i)-e3(2,i)*e1(1,i)
!
!           the fourth component in e3 is the constant term in the
!           equation of the triangle plane in the form
!           e3(1)*x+e3(2)*y+e3(3)*z+e3(4)=0
!
            e3(4,i)=-(e3(1,i)*p1(1)+e3(2,i)*p1(2)
     &                 +e3(3,i)*p1(3))
         enddo
!
!        determine the geometrical factors
!
!        initialization of the fields
!
         do i=1,ntri
            do j=1,ntri
               ft(i,j)=-1.d0
            enddo
         enddo
         do i=1,ntr
            do j=1,ntr
               f(i,j)=0.d0
            enddo
         enddo
         do iphi=1,4*ng
            do ipsi=1,ng
               covered(iphi,ipsi)=.false.
            enddo
         enddo
         ncovered=0
!
         do i=1,ntri
!
!           checking which triangles face triangle i
!
            ndist=0
            do j=1,ntri
               if(pmid(1,j)*e3(1,i)+pmid(2,j)*e3(2,i)+
     &            pmid(3,j)*e3(3,i)+e3(4,i).le.0.d0) cycle
               if(pmid(1,i)*e3(1,j)+pmid(2,i)*e3(2,j)+
     &            pmid(3,i)*e3(3,j)+e3(4,j).le.0.d0) cycle
               ndist=ndist+1
               dist(ndist)=(pmid(1,j)-pmid(1,i))**2+
     &                     (pmid(2,j)-pmid(2,i))**2+
     &                     (pmid(3,j)-pmid(3,i))**2
               idist(ndist)=j
            enddo
!
!           ordering the triangles
!
            kflag=2
            call dsort(dist,idist,ndist,kflag)
!
            do k1=1,ndist
               j=idist(k1)
c               if(ft(j,i).lt.0.d0) then
!
!                 determine the direction vector in global coordinates
!                  
                  do k=1,3
                     dir(k)=(pmid(k,j)-pmid(k,i))/dist(k1)
                  enddo
!
!                 direction vector in local coordinates of triangle i
!                  
                  dirloc(1)=dir(1)*e1(1,i)+dir(2)*e1(2,i)+dir(3)*e1(3,i)
                  dirloc(2)=dir(1)*e2(1,i)+dir(2)*e2(2,i)+dir(3)*e2(3,i)
                  dirloc(3)=dir(1)*e3(1,i)+dir(2)*e3(2,i)
     &                     +dir(3)*e3(3,i)
!
!                 check whether this direction was already covered
!
                  psi=acos(dirloc(3))
                  phi=atan2(dirloc(2)/dsin(psi),dirloc(1)/dsin(psi))
                  iphi=int(phi/dint)+1
                  ipsi=int(psi/dint)+1
                  if(covered(iphi,ipsi)) cycle
!
!                 calculating the contribution
!
                  cospsij=-dir(1)*e3(1,j)-dir(2)*e3(2,j)
     &                    -dir(3)*e3(3,j)
                  ft(i,j)=dirloc(3)*cospsij*area(j)/dist(k1)
!
!                 localizing which surface interaction the
!                 triangle interaction is part of
!
                  call nident(iptr,i,ntri,idi)
                  call nident(iptr,j,ntri,idj)
                  f(idi-1,idj-1)=f(idi-1,idj-1)+ft(i,j)
!
!                 check the coverage
!
!                 normed local coordinates of the nodes of triangle j
!
                  do k=1,3
                     p1(k)=co(k,kontri(1,j))-pmid(k,i)
                  enddo
                  p1loc(1)=p1(1)*e1(1,i)+p1(2)*e1(2,i)+p1(3)*e1(3,i)
                  p1loc(2)=p1(1)*e2(1,i)+p1(2)*e2(2,i)+p1(3)*e2(3,i)
                  p1loc(3)=p1(1)*e3(1,i)+p1(2)*e3(2,i)+p1(3)*e3(3,i)
                  dd=dsqrt(p1loc(1)*p1loc(1)+p1loc(2)*p1loc(2)+
     &                     p1loc(3)*p1loc(3))
                  do k=1,3
                     p1loc(k)=p1loc(k)/dd
                  enddo
                  ppsi(1)=acos(p1loc(3))
                  pphi(1)=atan2(p1loc(2)/dsin(ppsi(1)),
     &                          p1loc(1)/dsin(ppsi(1)))
                  ipphi(1)=int(pphi(1)/dint)+1
                  ippsi(1)=int(ppsi(1)/dint)+1
!
                  do k=1,3
                     p2(k)=co(k,kontri(2,j))-pmid(k,i)
                  enddo
                  p2loc(1)=p2(1)*e1(1,i)+p2(2)*e1(2,i)+p2(3)*e1(3,i)
                  p2loc(2)=p2(1)*e2(1,i)+p2(2)*e2(2,i)+p2(3)*e2(3,i)
                  p2loc(3)=p2(1)*e3(1,i)+p2(2)*e3(2,i)+p2(3)*e3(3,i)
                  dd=dsqrt(p2loc(1)*p2loc(1)+p2loc(2)*p2loc(2)+
     &                     p2loc(3)*p2loc(3))
                  do k=1,3
                     p2loc(k)=p2loc(k)/dd
                  enddo
                  ppsi(2)=acos(p2loc(3))
                  pphi(2)=atan2(p2loc(2)/dsin(ppsi(2)),
     &                          p2loc(1)/dsin(ppsi(2)))
                  ipphi(2)=int(pphi(2)/dint)+1
                  ippsi(2)=int(ppsi(2)/dint)+1
!
                  do k=1,3
                     p3(k)=co(k,kontri(3,j))-pmid(k,i)
                  enddo
                  p3loc(1)=p3(1)*e1(1,i)+p3(2)*e1(2,i)+p3(3)*e1(3,i)
                  p3loc(2)=p3(1)*e2(1,i)+p3(2)*e2(2,i)+p3(3)*e2(3,i)
                  p3loc(3)=p3(1)*e3(1,i)+p3(2)*e3(2,i)+p3(3)*e3(3,i)
                  dd=dsqrt(p3loc(1)*p3loc(1)+p3loc(2)*p3loc(2)+
     &                     p3loc(3)*p3loc(3))
                  do k=1,3
                     p3loc(k)=p3loc(k)/dd
                  enddo
                  ppsi(3)=acos(p3loc(3))
                  pphi(3)=atan2(p3loc(2)/dsin(ppsi(3)),
     &                          p3loc(1)/dsin(ppsi(3)))
                  ipphi(3)=int(pphi(3)/dint)+1
                  ippsi(3)=int(ppsi(3)/dint)+1
!
!                 equation of the planes connecting pmid(i) with
!                 p1(j),p2(j) and p3(j)
!
                  a(1,2)=p1loc(2)*p2loc(3)-p2loc(2)*p1loc(3)
                  b(1,2)=p1loc(3)*p2loc(1)-p2loc(3)*p1loc(1)
                  c(1,2)=p1loc(1)*p2loc(2)-p2loc(1)*p1loc(2)
!
                  a(2,3)=p2loc(2)*p3loc(3)-p3loc(2)*p2loc(3)
                  b(2,3)=p2loc(3)*p3loc(1)-p3loc(3)*p2loc(1)
                  c(2,3)=p2loc(1)*p3loc(2)-p3loc(1)*p2loc(2)
!
                  a(3,1)=p3loc(2)*p1loc(3)-p1loc(2)*p3loc(3)
                  b(3,1)=p3loc(3)*p1loc(1)-p1loc(3)*p3loc(1)
                  c(3,1)=p3loc(1)*p1loc(2)-p1loc(1)*p3loc(2)
!
                  a(2,1)=a(1,2)
                  b(2,1)=b(1,2)
                  c(2,1)=c(1,2)
                  a(3,2)=a(2,3)
                  b(3,2)=b(2,3)
                  c(3,2)=c(2,3)
                  a(1,3)=a(3,1)
                  b(1,3)=b(3,1)
                  c(1,3)=c(3,1)
!
!                 determining maxima and minima
!
                  phimin=7.d0
                  phimax=0.d0
                  do k=1,3
                     if(pphi(k).lt.phimin) then
                        phimin=pphi(k)
                        imin=k
                     endif
                     if(pphi(k).gt.phimax) then
                        phimax=pphi(k)
                        imax=k
                     endif
                  enddo
!
                  if(((imin.eq.1).and.(imax.eq.2)).or.
     &               ((imin.eq.2).and.(imax.eq.1))) then
                     imid=3
                     phimid=pphi(3)
                  elseif(((imin.eq.2).and.(imax.eq.3)).or.
     &                   ((imin.eq.3).and.(imax.eq.2))) then
                     imid=1
                      phimid=pphi(1)
                 else
                     imid=2
                     phimid=pphi(2)
                 endif
!
                 istart=int((phimin+dint/2.d0)/dint)+1
                 iend=int((phimid+dint/2.d0)/dint)
                 do i1=istart,iend
                    phi=c1*(i1-0.5d0)
                    psimin=(-a(imin,imid)*dcos(phi)
     &                      +b(imin,imid)*dsin(phi))/c(imin,imid)
                    psimax=(-a(imin,imax)*dcos(phi)
     &                      +b(imin,imax)*dsin(phi))/c(imin,imax)
                    if(psimin.gt.psimax) then
                       dummy=psimin
                       psimin=psimax
                       psimax=dummy
                    endif
                    jstart=int((psimin+dint/2.d0)/dint)+1
                    jend=int((psimax+dint/2.d0)/dint)
                    do j1=jstart,jend
                       covered(i1,j1)=.true.
                    enddo
                    ncovered=ncovered+jend-jstart+1
                 enddo
!
                 istart=int((phimid+dint/2.d0)/dint)+1
                 iend=int((phimax+dint/2.d0)/dint)
                 do i1=istart,iend
                    phi=c1*(i1-0.5d0)
                    psimin=(-a(imid,imax)*dcos(phi)
     &                      +b(imid,imax)*dsin(phi))/c(imid,imax)
                    psimax=(-a(imin,imax)*dcos(phi)
     &                      +b(imin,imax)*dsin(phi))/c(imin,imax)
                    if(psimin.gt.psimax) then
                       dummy=psimin
                       psimin=psimax
                       psimax=dummy
                    endif
                    jstart=int((psimin+dint/2.d0)/dint)+1
                    jend=int((psimax+dint/2.d0)/dint)
                    do j1=jstart,jend
                       covered(i1,j1)=.true.
                    enddo
                    ncovered=ncovered+jend-jstart+1
                 enddo
                 if(ncovered.eq.4*ng*ng)exit
!
c               endif
            enddo
         enddo
!
!        initialization of ac and bc
!
         do i=1,ntr
            do j=1,ntr
               ac(i,j)=0.d0
            enddo
            bc(i,1)=0.d0
         enddo
!
!        filling ac and bc
!
         do i1=1,ntr
            ac(i1,i1)=1.d0
            i=nloadtr(i1)
            nelem=nelemload(1,i)
            lakonl=lakon(nelem)
!
!              calculate the mean temperature of the face
!
            read(sideload(i)(2:2),'(i1)') ig
!
!              number of nodes and integration points in the face
!
            if(lakonl(4:4).eq.'2') then
               nope=20
               nopes=8
            elseif(lakonl(4:4).eq.'8') then
               nope=8
               nopes=4
            elseif(lakonl(4:5).eq.'10') then
               nope=10
               nopes=6
            elseif(lakonl(4:4).eq.'4') then
               nope=4
               nopes=3
            elseif(lakonl(4:5).eq.'15') then
               nope=15
            else
               nope=6
            endif
!
            if(lakonl(4:5).eq.'8R') then
               mint2d=1
            elseif((lakonl(4:4).eq.'8').or.(lakonl(4:6).eq.'20R'))
     &              then
               mint2d=4
            elseif(lakonl(4:4).eq.'2') then
               mint2d=9
            elseif(lakonl(4:5).eq.'10') then
               mint2d=3
            elseif(lakonl(4:4).eq.'4') then
               mint2d=1
            endif
!
            if(lakonl(4:4).eq.'6') then
               mint2d=1
               if(ig.le.2) then
                  nopes=3
               else
                  nopes=4
               endif
            endif
            if(lakonl(4:5).eq.'15') then
               if(ig.le.2) then
                  mint2d=3
                  nopes=6
               else
                  mint2d=4
                  nopes=8
               endif
            endif
!
!              connectivity of the element
!
            index=ipkon(nelem)
            if(index.lt.0) then
               write(*,*) '*ERROR in radflowload: element ',nelem
               write(*,*) '       is not defined'
               stop
            endif
            do k=1,nope
               konl(k)=kon(index+k)
            enddo
!
!              coordinates of the nodes belonging to the face
!
            if((nope.eq.20).or.(nope.eq.8)) then
               do k=1,nopes
                  xl2(0,k)=vold(0,konl(ifaceq(k,ig)))
                  do j=1,3
                     xl2(j,k)=co(j,konl(ifaceq(k,ig)))+
     &                    vold(j,konl(ifaceq(k,ig)))
                  enddo
               enddo
            elseif((nope.eq.10).or.(nope.eq.4)) then
               do k=1,nopes
                  xl2(0,k)=vold(0,konl(ifacet(k,ig)))
                  do j=1,3
                     xl2(j,k)=co(j,konl(ifacet(k,ig)))+
     &                    vold(j,konl(ifacet(k,ig)))
                  enddo
               enddo
            else
               do k=1,nopes
                  xl2(0,k)=vold(0,konl(ifacew(k,ig)))
                  do j=1,3
                     xl2(j,k)=co(j,konl(ifacew(k,ig)))+
     &                    vold(j,konl(ifacew(k,ig)))
                  enddo
               enddo
            endif
!
!              integration to obtain the center of gravity and the mean
!              temperature
!
            do k=1,3
               coordsmean(k)=0.d0
            enddo
            areamean=0.d0
            tarea(i1)=0.d0
            do l=1,mint2d
               if((lakonl(4:5).eq.'8R').or.
     &              ((lakonl(4:4).eq.'6').and.(nopes.eq.4))) then
                  xi=gauss2d1(1,l)
                  et=gauss2d1(2,l)
                  weight=weight2d1(l)
               elseif((lakonl(4:4).eq.'8').or.
     &                 (lakonl(4:6).eq.'20R').or.
     &                 ((lakonl(4:5).eq.'15').and.(nopes.eq.8))) then
                  xi=gauss2d2(1,l)
                  et=gauss2d2(2,l)
                  weight=weight2d2(l)
               elseif(lakonl(4:4).eq.'2') then
                  xi=gauss2d3(1,l)
                  et=gauss2d3(2,l)
                  weight=weight2d3(l)
               elseif((lakonl(4:5).eq.'10').or.
     &                 ((lakonl(4:5).eq.'15').and.(nopes.eq.6))) then
                  xi=gauss2d5(1,l)
                  et=gauss2d5(2,l)
                  weight=weight2d5(l)
               elseif((lakonl(4:4).eq.'4').or.
     &                 ((lakonl(4:4).eq.'6').and.(nopes.eq.3))) then
                  xi=gauss2d4(1,l)
                  et=gauss2d4(2,l)
                  weight=weight2d4(l)
               endif
!
               if(nopes.eq.8) then
                  call shape8q(xi,et,xl2,xsj2,shp2)
               elseif(nopes.eq.4) then
                  call shape4q(xi,et,xl2,xsj2,shp2)
               elseif(nopes.eq.6) then
                  call shape6tri(xi,et,xl2,xsj2,shp2)
               else
                  call shape3tri(xi,et,xl2,xsj2,shp2)
               endif
!
               dxsj2=dsqrt(xsj2(1)*xsj2(1)+xsj2(2)*xsj2(2)+
     &              xsj2(3)*xsj2(3))
!
               temp=0.d0
               do k=1,3
                  coords(k)=0.d0
               enddo
               do j=1,nopes
                  temp=temp+xl2(0,j)*shp2(4,j)
                  do k=1,3
                     coords(k)=coords(k)+xl2(k,j)*shp2(4,j)
                  enddo
               enddo
!
               tarea(i1)=tarea(i1)+temp*dxsj2*weight
               areamean=areamean+dxsj2*weight
               do k=1,3
                  coordsmean(k)=coordsmean(k)+coords(k)
               enddo
            enddo
            tarea(i1)=tarea(i1)/areamean-physcon(1)
            do k=1,3
               coordsmean(k)=coordsmean(k)/areamean
            enddo
!
!              radiation coefficient
!
            if(sideload(i)(5:5).ne.'N') then
               erad(i1)=xloadact(1,i)
            else
            endif
!
            e=erad(i1)
            ec=1.d0-e
            fenv=1.d0
!
            do j1=1,ntr
               ac(i1,j1)=ec*f(i1,j1)
               fenv=fenv-f(i1,j1)
            enddo
            bc(i1,1)=e*physcon(2)*tarea(i1)**4+
     &           ec*fenv*tenv(i1)**4
         enddo
!
!        solving the system
!
         nrhs=1
         call dgesv(ntg,nrhs,ac,ntm,ipiv,bc,ntm,info)
         if(info.ne.0) then
            write(*,*) '*ERROR in radflowload: singular matrix'
            stop
         endif
!
!        calculating the flux and transforming the flux into an
!        equivalent temperature
!
         do i=1,ntr
            q=bc(i,1)
            do j=1,ntr
               if(i.eq.j)cycle
               q=q-f(i,j)*bc(j,1)
            enddo
            xloadact(2,nloadtr(i))=
     &          (tarea(i)**4-q/(erad(i)*physcon(2)))**0.25
         enddo
!
      endif
!
      return
      end








