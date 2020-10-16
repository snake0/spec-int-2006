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
      subroutine e_c3d_rhs(co,nk,konl,lakonl,p1,p2,omx,bodyfx,ibod,
     &  ff,nelem,nmethod,rhcon,ielmat,ntmat_,vold,iperturb,nelemload,
     &  sideload,xload,nload,idist)
!
!     computation of the rhs for the element with
!     the topology in konl: only for nonlinear calculations (i.e.
!     the field ff contains no temperature and eigenstress 
!     contributions)
!
!     RESTRICTION: the only material parameter occurring in the
!     loading is the density. In the present subroutine the density
!     is assumed to be temperature INDEPENDENT.
!
      implicit none
!
      character*5 sideload(*)
      character*8 lakonl
!
      integer nk,konl(20),ielmat(*),ibod,ifaceq(8,6),nelemload(2,*),
     &  nelem,nmethod,iperturb,nload,idist,i,i1,j1,jj,jj1,id,kk,
     &  ipointer,nope,nopes,j,k,ntmat_,i2,imat,ii,ig,mint2d,mint3d,
     &  ifacet(6,4),ifacew(8,5)
!
      real*8 co(3,*),p1(3),p2(3),omx,bodyfx(3),rhcon(0:1,ntmat_,*),
     &  bodyf(3),om,rho,bf(3),q(3),shpj(4,20),xl(3,20),
     &  shp(4,20),voldl(3,20),xl2(3,8),xsj2(3),shp2(4,8),vold(0:3,*),
     &  xload(2,*),xi,et,ze,const,xsj,ff(60),weight
!
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
         mint3d=1
      elseif((lakonl(4:4).eq.'8').or.(lakonl(4:6).eq.'20R')) then
         mint2d=4
         mint3d=8
      elseif(lakonl(4:4).eq.'2') then
         mint2d=9
         mint3d=27
      elseif(lakonl(4:5).eq.'10') then
         mint2d=3
         mint3d=4
      elseif(lakonl(4:4).eq.'4') then
         mint2d=1
         mint3d=1
      elseif(lakonl(4:5).eq.'15') then
         mint3d=9
      else
         mint3d=2
      endif
!
!     computation of the coordinates of the local nodes
!
      do i=1,nope
        do j=1,3
          xl(j,i)=co(j,konl(i))
        enddo
      enddo
!
!       initialisation for distributed forces
!
      if(idist.ne.0) then
         do i=1,3*nope
            ff(i)=0.d0
         enddo
      endif
!
!     displacements for 2nd order static and modal theory
!
      if(iperturb.ne.0) then
         do i1=1,nope
            do i2=1,3
               voldl(i2,i1)=vold(i2,konl(i1))
            enddo
         enddo
      endif
!
!     calculating the density: no temperature dependence assumed!
!     otherwise cfr. e_c3d.f
!
      imat=ielmat(nelem)
      rho=rhcon(1,1,imat)
!
!     computation of the body forces
!
      if(ibod.ne.0) then
         om=omx*rho
         do ii=1,3
            bodyf(ii)=bodyfx(ii)*rho
         enddo
      endif
!
!     computation of the rhs: loop over the Gauss points
!
      do kk=1,mint3d
         if(lakonl(4:5).eq.'8R') then
            xi=gauss3d1(1,kk)
            et=gauss3d1(2,kk)
            ze=gauss3d1(3,kk)
            weight=weight3d1(kk)
         elseif((lakonl(4:4).eq.'8').or.(lakonl(4:6).eq.'20R')) 
     &      then
            xi=gauss3d2(1,kk)
c            if(nope.eq.20) xi=xi+1.d0
            et=gauss3d2(2,kk)
            ze=gauss3d2(3,kk)
            weight=weight3d2(kk)
         elseif(lakonl(4:4).eq.'2') then
c            xi=gauss3d3(1,kk)+1.d0
            xi=gauss3d3(1,kk)
            et=gauss3d3(2,kk)
            ze=gauss3d3(3,kk)
            weight=weight3d3(kk)
         elseif(lakonl(4:5).eq.'10') then
            xi=gauss3d5(1,kk)
            et=gauss3d5(2,kk)
            ze=gauss3d5(3,kk)
            weight=weight3d5(kk)
         elseif(lakonl(4:4).eq.'4') then
            xi=gauss3d4(1,kk)
            et=gauss3d4(2,kk)
            ze=gauss3d4(3,kk)
            weight=weight3d4(kk)
         elseif(lakonl(4:5).eq.'15') then
            xi=gauss3d8(1,kk)
            et=gauss3d8(2,kk)
            ze=gauss3d8(3,kk)
            weight=weight3d8(kk)
         else
            xi=gauss3d7(1,kk)
            et=gauss3d7(2,kk)
            ze=gauss3d7(3,kk)
            weight=weight3d7(kk)
         endif
!
!           calculation of the shape functions and their derivatives
!           in the gauss point
!
            if(nope.eq.20) then
               call shape20h(xi,et,ze,xl,xsj,shp)
            elseif(nope.eq.8) then
               call shape8h(xi,et,ze,xl,xsj,shp)
            elseif(nope.eq.10) then
               call shape10tet(xi,et,ze,xl,xsj,shp)
            elseif(nope.eq.4) then
               call shape4tet(xi,et,ze,xl,xsj,shp)
            elseif(nope.eq.15) then
               call shape15w(xi,et,ze,xl,xsj,shp)
            else
               call shape6w(xi,et,ze,xl,xsj,shp)
            endif
!
!           check the jacobian determinant
!
            if(xsj.lt.1.d-20) then
              write(*,*) '*WARNING in e_c3d: nonpositive jacobian'
              write(*,*) '         determinant in element',nelem
              write(*,*)
              xsj=dabs(xsj)
              nmethod=0
            endif
!
!           computation of the right hand side
!
!           body forces
!
            if(ibod.ne.0) then
!
!              incorporating the jacobian determinant in the shape
!              functions
!
               do i1=1,nope
                  shpj(4,i1)=shp(4,i1)*xsj
               enddo
!
               if(om.gt.0.d0) then
                  do i1=1,3
!
!                   computation of the global coordinates of the gauss
!                   point
!
                     q(i1)=0.d0
                     if(iperturb.eq.0) then
                        do j1=1,nope
                           q(i1)=q(i1)+shp(4,j1)*xl(i1,j1)
                        enddo
                     else
                        do j1=1,nope
                           q(i1)=q(i1)+shp(4,j1)*
     &                          (xl(i1,j1)+voldl(i1,j1))
                        enddo
                     endif
!                       
                     q(i1)=q(i1)-p1(i1)
                  enddo
                  const=q(1)*p2(1)+q(2)*p2(2)+q(3)*p2(3)
!
!                 inclusion of the centrifugal force into the body force
!
                  do i1=1,3
                     bf(i1)=bodyf(i1)+(q(i1)-const*p2(i1))*om
                  enddo
               else
                  do i1=1,3
                     bf(i1)=bodyf(i1)
                  enddo
               endif
               jj1=1
               do jj=1,nope
                  ff(jj1)=ff(jj1)+bf(1)*shpj(4,jj)*weight
                  ff(jj1+1)=ff(jj1+1)+bf(2)*shpj(4,jj)*weight
                  ff(jj1+2)=ff(jj1+2)+bf(3)*shpj(4,jj)*weight
                  jj1=jj1+3
               enddo
            endif
!
      enddo
!
!     distributed loads
!
      if(nload.eq.0) then
         return
      endif
      call nident2(nelemload,nelem,nload,id)
      do
         if((id.eq.0).or.(nelemload(1,id).ne.nelem)) exit
         read(sideload(id)(2:2),'(i1)') ig
!
!         treatment of wedge faces
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
          if((nope.eq.20).or.(nope.eq.8)) then
            if(iperturb.eq.0) then
               do i=1,nopes
                  do j=1,3
                     xl2(j,i)=co(j,konl(ifaceq(i,ig)))
                  enddo
               enddo
            else
               do i=1,nopes
                  do j=1,3
                     xl2(j,i)=co(j,konl(ifaceq(i,ig)))+
     &                    vold(j,konl(ifaceq(i,ig)))
                  enddo
               enddo
            endif
         elseif((nope.eq.10).or.(nope.eq.4)) then
            if(iperturb.eq.0) then
               do i=1,nopes
                  do j=1,3
                     xl2(j,i)=co(j,konl(ifacet(i,ig)))
                  enddo
               enddo
            else
               do i=1,nopes
                  do j=1,3
                     xl2(j,i)=co(j,konl(ifacet(i,ig)))+
     &                    vold(j,konl(ifacet(i,ig)))
                  enddo
               enddo
            endif
         else
            if(iperturb.eq.0) then
               do i=1,nopes
                  do j=1,3
                     xl2(j,i)=co(j,konl(ifacew(i,ig)))
                  enddo
               enddo
            else
               do i=1,nopes
                  do j=1,3
                     xl2(j,i)=co(j,konl(ifacew(i,ig)))+
     &                    vold(j,konl(ifacew(i,ig)))
                  enddo
               enddo
            endif
         endif
!
         do i=1,mint2d
            if((lakonl(4:5).eq.'8R').or.
     &           ((lakonl(4:4).eq.'6').and.(nopes.eq.4))) then
               xi=gauss2d1(1,i)
               et=gauss2d1(2,i)
               weight=weight2d1(i)
            elseif((lakonl(4:4).eq.'8').or.
     &              (lakonl(4:6).eq.'20R').or.
     &              ((lakonl(4:5).eq.'15').and.(nopes.eq.8))) then
               xi=gauss2d2(1,i)
               et=gauss2d2(2,i)
               weight=weight2d2(i)
            elseif(lakonl(4:4).eq.'2') then
               xi=gauss2d3(1,i)
               et=gauss2d3(2,i)
               weight=weight2d3(i)
            elseif((lakonl(4:5).eq.'10').or.
     &              ((lakonl(4:5).eq.'15').and.(nopes.eq.6))) then
               xi=gauss2d5(1,i)
               et=gauss2d5(2,i)
               weight=weight2d5(i)
            elseif((lakonl(4:4).eq.'4').or.
     &              ((lakonl(4:4).eq.'6').and.(nopes.eq.3))) then
               xi=gauss2d4(1,i)
               et=gauss2d4(2,i)
               weight=weight2d4(i)
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
            do k=1,nopes
               if((nope.eq.20).or.(nope.eq.8)) then
                  ipointer=(ifaceq(k,ig)-1)*3
               elseif((nope.eq.10).or.(nope.eq.4)) then
                  ipointer=(ifacet(k,ig)-1)*3
               else
                  ipointer=(ifacew(k,ig)-1)*3
               endif
               ff(ipointer+1)=ff(ipointer+1)-shp2(4,k)*xload(1,id)
     &              *xsj2(1)*weight
               ff(ipointer+2)=ff(ipointer+2)-shp2(4,k)*xload(1,id)
     &              *xsj2(2)*weight
               ff(ipointer+3)=ff(ipointer+3)-shp2(4,k)*xload(1,id)
     &              *xsj2(3)*weight
            enddo
         enddo
!
         id=id-1
      enddo
!
      return
      end
