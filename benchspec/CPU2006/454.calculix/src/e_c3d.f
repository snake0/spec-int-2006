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
      subroutine e_c3d(co,nk,konl,lakonl,p1,p2,omx,bodyfx,ibod,s,sm,f,
     &  ff,nelem,nmethod,elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,
     &  ielmat,ielorien,norien,orab,ntmat_,
     &  t0,t1,ithermal,prestr,iprestr,vold,iperturb,nelemload,
     &  sideload,xload,nload,idist,sti,stx,eei,iexpl,plicon,
     &  nplicon,plkcon,nplkcon,xstiff,npmat_,dtime,
     &  matname,mint_,ncmat_,mass,stiffness,buckling,rhs,intscheme)
!
!     computation of the element matrix and rhs for the element with
!     the topology in konl
!
!     f: rhs with temperature and eigenstress contribution: for linear 
!        calculations only
!     ff: rhs without temperature and eigenstress contribution
!
!     nmethod=0: check for positive Jacobian
!     nmethod=1: stiffness matrix + right hand side
!     nmethod=2: stiffness matrix + mass matrix
!     nmethod=3: static stiffness + buckling stiffness
!     nmethod=4: right hand side (linear, iperturb=0)
!
      implicit none
!
      logical mass,stiffness,buckling,rhs
!
      character*5 sideload(*)
      character*8 lakonl
      character*20 matname(*),amat
!
      integer konl(20),ifaceq(8,6),nelemload(2,*),nk,ibod,nelem,nmethod,
     &  mattyp,ithermal,iprestr,iperturb,nload,idist,i,j,k,l,i1,i2,j1,
     &  k1,l1,ii,jj,ii1,jj1,id,ipointer,ig,m1,m2,m3,m4,kk,
     &  nelcon(2,*),nrhcon(*),nalcon(2,*),ielmat(*),ielorien(*),
     &  ntmat_,nope,nopes,norien,icmdl,ihyper,iexpl,kode,imat,mint2d,
     &  mint3d,mint_,ifacet(6,4),nopev,iorien,istiff,ncmat_,
     &  ifacew(8,5),intscheme,n,ipointeri,ipointerj,iii1,jjj1,n1
!
      integer nplicon(0:ntmat_,*),nplkcon(0:ntmat_,*),npmat_
!
      real*8 co(3,*),xl(3,20),shp(4,20),
     &  s(60,60),w(3,3),p1(3),p2(3),f(60),bodyf(3),bodyfx(3),ff(60),
     &  bf(3),q(3),shpj(4,20),elcon(0:ncmat_,ntmat_,*),
     &  rhcon(0:1,ntmat_,*),xkl(3,3),
     &  alcon(0:6,ntmat_,*),alzero(*),orab(7,*),t0(*),t1(*),
     &  anisox(3,3,3,3),beta(6),prestr(6,*),voldl(3,20),vo(3,3),
     &  xl2(3,8),xsj2(3),shp2(4,8),vold(0:3,*),xload(2,*),v(3,3,3,3),
     &  om,omx,e,un,al,um,xi,et,ze,tt,const,xsj,xsjj,sm(60,60),
     &  sti(6,mint_,*),stx(6,mint_,*),s11,s22,s33,s12,s13,s23,s11b,
     &  s22b,s33b,s12b,s13b,s23b,eei(6,mint_,*),t0l,t1l,stre(6),
     &  senergy,senergyb,rho,elas(21),alph(6),summass,summ,
     &  sume,factorm,factore,alp,elconloc(21),eth(6),exx,eyy,ezz,
     &  exy,exz,eyz,am1,weight,pgauss(3),dmass,xl1(3,8),term
!
      real*8 gauss2d1(2,1),gauss2d2(2,4),gauss2d3(2,9),gauss2d4(2,1),
     &  gauss2d5(2,3),gauss3d1(3,1),gauss3d2(3,8),gauss3d3(3,27),
     &  gauss3d4(3,1),gauss3d5(3,4),gauss3d6(3,15),gauss3d7(3,2),
     &  gauss3d8(3,9),gauss3d9(3,18),weight2d1(1),weight2d2(4),
     &  weight2d3(9),weight2d4(1),weight2d5(3),weight3d1(1),
     &  weight3d2(8),weight3d3(27),weight3d4(1),weight3d5(4),
     &  weight3d6(15),weight3d7(2),weight3d8(9),weight3d9(18)
!
      real*8 plicon(0:2*npmat_,ntmat_,*),plkcon(0:2*npmat_,ntmat_,*),
     &  xstiff(21,mint_,*),
     &  plconloc(82),dtime
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
c      if(nmethod.eq.5) then
c         intscheme=1
c         nmethod=2
c      else
c         intscheme=0
c      endif
c!
c      mass=.false.
c      stiffness=.false.
c      buckling=.false.
c      rhs=.false.
c!
c      if(nmethod.eq.1) then
c         stiffness=.true.
c         rhs=.true.
c      elseif(nmethod.eq.2) then
c         mass=.true.
c         stiffness=.true.
c      elseif(nmethod.eq.3) then
c         stiffness=.true.
c         buckling=.true.
c      elseif(nmethod.eq.4) then
c         rhs=.true.
c      endif
!
      summass=0.d0
!
      imat=ielmat(nelem)
      amat=matname(imat)
      if(norien.gt.0) then
         iorien=ielorien(nelem)
      else
         iorien=0
      endif
!
      if(lakonl(4:4).eq.'2') then
         nope=20
         nopev=8
         nopes=8
      elseif(lakonl(4:4).eq.'8') then
         nope=8
         nopev=8
         nopes=4
      elseif(lakonl(4:5).eq.'10') then
         nope=10
         nopev=4
         nopes=6
      elseif(lakonl(4:4).eq.'4') then
         nope=4
         nopev=4
         nopes=3
      elseif(lakonl(4:5).eq.'15') then
         nope=15
         nopev=6
      else
         nope=6
         nopev=6
      endif
!
      if(intscheme.eq.0) then
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
      else
         if((lakonl(4:4).eq.'8').or.(lakonl(4:4).eq.'2')) then
            mint3d=27
         elseif((lakonl(4:5).eq.'10').or.(lakonl(4:4).eq.'4')) then
            mint3d=15
         else
            mint3d=18
         endif
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
      if(nelcon(1,imat).lt.0) then
         ihyper=1
      else
         ihyper=0
      endif
!
!       initialisation for distributed forces
!
      if(rhs) then
        if(idist.ne.0) then
          do i=1,3*nope
            f(i)=0.d0
            ff(i)=0.d0
          enddo
        endif
      endif
!
!     displacements for 2nd order static and modal theory
!
      if((iperturb.ne.0).and.stiffness.and.(.not.buckling)) then
         do i1=1,nope
            do i2=1,3
               voldl(i2,i1)=vold(i2,konl(i1))
            enddo
         enddo
      endif
!
!     initialisation of sm
!
      if(mass.or.buckling) then
        do i=1,3*nope
          do j=1,3*nope
            sm(i,j)=0.d0
          enddo
        enddo
      endif
!
!     initialisation of s
!
      do i=1,3*nope
        do j=1,3*nope
          s(i,j)=0.d0
        enddo
      enddo
!
!     computation of the matrix: loop over the Gauss points
!
      do kk=1,mint3d
         if(intscheme.eq.0) then
            if(lakonl(4:5).eq.'8R') then
               xi=gauss3d1(1,kk)
               et=gauss3d1(2,kk)
               ze=gauss3d1(3,kk)
               weight=weight3d1(kk)
            elseif((lakonl(4:4).eq.'8').or.(lakonl(4:6).eq.'20R')) 
     &              then
               xi=gauss3d2(1,kk)
c               if(nope.eq.20) xi=xi+1.d0
               et=gauss3d2(2,kk)
               ze=gauss3d2(3,kk)
               weight=weight3d2(kk)
            elseif(lakonl(4:4).eq.'2') then
c               xi=gauss3d3(1,kk)+1.d0
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
         else
            if((lakonl(4:4).eq.'8').or.(lakonl(4:4).eq.'2')) then
c               xi=gauss3d3(1,kk)+1.d0
               xi=gauss3d3(1,kk)
               et=gauss3d3(2,kk)
               ze=gauss3d3(3,kk)
               weight=weight3d3(kk)
            elseif((lakonl(4:5).eq.'10').or.(lakonl(4:4).eq.'4')) then
               xi=gauss3d6(1,kk)
               et=gauss3d6(2,kk)
               ze=gauss3d6(3,kk)
               weight=weight3d6(kk)
            else
               xi=gauss3d9(1,kk)
               et=gauss3d9(2,kk)
               ze=gauss3d9(3,kk)
               weight=weight3d9(kk)
            endif
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
         if((iperturb.ne.0).and.stiffness.and.(.not.buckling))
     &        then
!
!              stresses for 2nd order static and modal theory
!
            s11=sti(1,kk,nelem)
            s22=sti(2,kk,nelem)
            s33=sti(3,kk,nelem)
            s12=sti(4,kk,nelem)
            s13=sti(5,kk,nelem)
            s23=sti(6,kk,nelem)
         endif
!
!           calculating the temperature in the integration
!           point
!
         t0l=0.d0
         t1l=0.d0
         if(ithermal.eq.1) then
            if(lakonl(4:5).eq.'8 ') then
               do i1=1,nope
                  t0l=t0l+t0(konl(i1))/8.d0
                  t1l=t1l+t1(konl(i1))/8.d0
               enddo
            elseif(lakonl(4:6).eq.'20 ') then
               call lintemp(t0,t1,konl,nope,kk,t0l,t1l)
            else
               do i1=1,nope
                  t0l=t0l+shp(4,i1)*t0(konl(i1))
                  t1l=t1l+shp(4,i1)*t1(konl(i1))
               enddo
            endif
         elseif(ithermal.ge.2) then
            if(lakonl(4:5).eq.'8 ') then
               do i1=1,nope
                  t0l=t0l+t0(konl(i1))/8.d0
                  t1l=t1l+vold(0,konl(i1))/8.d0
               enddo
            elseif(lakonl(4:6).eq.'20 ') then
               call lintemp_th(t0,vold,konl,nope,kk,t0l,t1l)
            else
               do i1=1,nope
                  t0l=t0l+shp(4,i1)*t0(konl(i1))
                  t1l=t1l+shp(4,i1)*vold(0,konl(i1))
               enddo
            endif
         endif
         tt=t1l-t0l
!
!           calculating the coordinates of the integration point
!           for material orientation purposes (for cylindrical
!           coordinate systems)
!
         if(iorien.gt.0) then
            do j=1,3
               pgauss(j)=0.d0
               do i1=1,nope
                  pgauss(j)=pgauss(j)+shp(4,i1)*co(j,konl(i1))
               enddo
            enddo
         endif
!
!           for deformation plasticity: calculating the Jacobian
!           and the inverse of the deformation gradient
!           needed to convert the stiffness matrix in the spatial
!           frame of reference to the material frame
!
         kode=nelcon(1,imat)
!
!           material data and local stiffness matrix
!
         istiff=1
         call materialdata(elcon,nelcon,rhcon,nrhcon,alcon,nalcon,
     &        imat,amat,iorien,pgauss,orab,ntmat_,elas,alph,rho,
     &        nelem,ithermal,alzero,mattyp,t0l,t1l,
     &        ihyper,istiff,elconloc,eth,kode,plicon,
     &        nplicon,plkcon,nplkcon,npmat_,
     &        plconloc,mint_,dtime,nelem,kk,
     &        xstiff,ncmat_)
!
         if(mattyp.eq.1) then
            e=elas(1)
            un=elas(2)
            um=e/(1.d0+un)
            al=un*um/(1.d0-2.d0*un)
            um=um/2.d0
         elseif(mattyp.eq.2) then
            call orthotropic(elas,anisox)
         else
            call anisotropic(elas,anisox)
         endif
!
!           initialisation for the body forces
!
         om=omx*rho
         if(rhs) then
            if(ibod.ne.0) then
               do ii=1,3
                  bodyf(ii)=bodyfx(ii)*rho
               enddo
            endif
         endif
!
         if(rhs) then
!
!             information for the rhs
!
!             residual stresses
!
            if((iprestr.eq.1).or.(ithermal.eq.1)) then
               if(iprestr.eq.0) then
                  do ii=1,6
                     beta(ii)=0.d0
                  enddo
               else
                  do ii=1,6
                     beta(ii)=-prestr(ii,nelem)
                  enddo
               endif
            endif
!
!             calculation of the thermal stresses in an undeformed body 
!             assumption; beta corresponds to initial stresses.
!
            if(ithermal.eq.1) then
               if(ihyper.eq.0) then
                  icmdl=2
                  call linel(ithermal,mattyp,beta,al,um,am1,alph,tt,
     &                 elas,icmdl,exx,eyy,ezz,exy,exz,eyz,stre,
     &                 anisox)
               endif
            endif
!
         elseif(buckling) then
!
!              buckling stresses
!
            s11b=stx(1,kk,nelem)
            s22b=stx(2,kk,nelem)
            s33b=stx(3,kk,nelem)
            s12b=stx(4,kk,nelem)
            s13b=stx(5,kk,nelem)
            s23b=stx(6,kk,nelem)
!
         endif
!
!           incorporating the jacobian determinant in the shape
!           functions
!
         xsjj=dsqrt(xsj)
         do i1=1,nope
            shpj(1,i1)=shp(1,i1)*xsjj
            shpj(2,i1)=shp(2,i1)*xsjj
            shpj(3,i1)=shp(3,i1)*xsjj
            shpj(4,i1)=shp(4,i1)*xsj
         enddo
!
!           determination of the stiffness, and/or mass and/or
!           buckling matrix
!
         if(stiffness.or.mass.or.buckling) then
!
            if((iperturb.eq.0).or.buckling)
     &           then
               jj1=1
               do jj=1,nope
!
                  ii1=1
                  do ii=1,jj
!
!                   all products of the shape functions for a given ii
!                   and jj
!
                     do i1=1,3
                        do j1=1,3
                           w(i1,j1)=shpj(i1,ii)*shpj(j1,jj)
                        enddo
                     enddo
!
!                   the following section calculates the static
!                   part of the stiffness matrix which, for buckling 
!                   calculations, is done in a preliminary static
!                   call
!
                     if(.not.buckling) then
!
                        if(mattyp.eq.1) then
!
                           s(ii1,jj1)=s(ii1,jj1)+(al*w(1,1)+
     &                          um*(2.d0*w(1,1)+w(2,2)+w(3,3)))*weight
                           s(ii1,jj1+1)=s(ii1,jj1+1)+(al*w(1,2)+
     &                          um*w(2,1))*weight
                           s(ii1,jj1+2)=s(ii1,jj1+2)+(al*w(1,3)+
     &                          um*w(3,1))*weight
                           s(ii1+1,jj1)=s(ii1+1,jj1)+(al*w(2,1)+
     &                          um*w(1,2))*weight
                           s(ii1+1,jj1+1)=s(ii1+1,jj1+1)+(al*w(2,2)+
     &                          um*(2.d0*w(2,2)+w(1,1)+w(3,3)))*weight
                           s(ii1+1,jj1+2)=s(ii1+1,jj1+2)+(al*w(2,3)+
     &                          um*w(3,2))*weight
                           s(ii1+2,jj1)=s(ii1+2,jj1)+(al*w(3,1)+
     &                          um*w(1,3))*weight
                           s(ii1+2,jj1+1)=s(ii1+2,jj1+1)+(al*w(3,2)+
     &                          um*w(2,3))*weight
                           s(ii1+2,jj1+2)=s(ii1+2,jj1+2)+(al*w(3,3)+
     &                          um*(2.d0*w(3,3)+w(2,2)+w(1,1)))*weight
!
                        elseif(mattyp.eq.2) then
!
                           s(ii1,jj1)=s(ii1,jj1)+(elas(1)*w(1,1)+
     &                          elas(7)*w(2,2)+elas(8)*w(3,3))*weight
                           s(ii1,jj1+1)=s(ii1,jj1+1)+(elas(2)*w(1,2)+
     &                          elas(7)*w(2,1))*weight
                           s(ii1,jj1+2)=s(ii1,jj1+2)+(elas(4)*w(1,3)+
     &                          elas(8)*w(3,1))*weight
                           s(ii1+1,jj1)=s(ii1+1,jj1)+(elas(7)*w(1,2)+
     &                          elas(2)*w(2,1))*weight
                           s(ii1+1,jj1+1)=s(ii1+1,jj1+1)+
     &                          (elas(7)*w(1,1)+
     &                          elas(3)*w(2,2)+elas(9)*w(3,3))*weight
                           s(ii1+1,jj1+2)=s(ii1+1,jj1+2)+
     &                          (elas(5)*w(2,3)+
     &                          elas(9)*w(3,2))*weight
                           s(ii1+2,jj1)=s(ii1+2,jj1)+
     &                          (elas(8)*w(1,3)+
     &                          elas(4)*w(3,1))*weight
                           s(ii1+2,jj1+1)=s(ii1+2,jj1+1)+
     &                          (elas(9)*w(2,3)+
     &                          elas(5)*w(3,2))*weight
                           s(ii1+2,jj1+2)=s(ii1+2,jj1+2)+
     &                          (elas(8)*w(1,1)+
     &                          elas(9)*w(2,2)+elas(6)*w(3,3))*weight
!
                        else
!
                           do i1=1,3
                              do j1=1,3
                                 do k1=1,3
                                    do l1=1,3
                                       s(ii1+i1-1,jj1+j1-1)=
     &                                      s(ii1+i1-1,jj1+j1-1)
     &                                      +anisox(i1,k1,j1,l1)
     &                                      *w(k1,l1)*weight
                                    enddo
                                 enddo
                              enddo
                           enddo
!
                        endif
!
!                     mass matrix
!
                        if(mass) then
                           sm(ii1,jj1)=sm(ii1,jj1)
     &                          +rho*shpj(4,ii)*shp(4,jj)*weight
                           sm(ii1+1,jj1+1)=sm(ii1,jj1)
                           sm(ii1+2,jj1+2)=sm(ii1,jj1)
                        endif
!
                     else
!
!                     buckling matrix  
!
                        senergyb=
     &                       (s11b*w(1,1)+s12b*(w(1,2)+w(2,1))
     &                       +s13b*(w(1,3)+w(3,1))+s22b*w(2,2)
     &                       +s23b*(w(2,3)+w(3,2))+s33b*w(3,3))*weight
                        sm(ii1,jj1)=sm(ii1,jj1)-senergyb
                        sm(ii1+1,jj1+1)=sm(ii1+1,jj1+1)-senergyb
                        sm(ii1+2,jj1+2)=sm(ii1+2,jj1+2)-senergyb
!
                     endif
!
                     ii1=ii1+3
                  enddo
                  jj1=jj1+3
               enddo
            else
!
!               stiffness matrix for static and modal
!               2nd order calculations
!
!               large displacement stiffness
!               
               do i1=1,3
                  do j1=1,3
                     vo(i1,j1)=0.d0
                     do k1=1,nope
                        vo(i1,j1)=vo(i1,j1)+shp(j1,k1)*voldl(i1,k1)
                     enddo
                  enddo
               enddo
!
               if(mattyp.eq.1) then
                  call wcoef(v,vo,al,um)
               endif
!
!               calculating the total mass of the element for
!               lumping purposes: only for explicit nonlinear
!               dynamic calculations
!
               if(mass.and.(iexpl.eq.1)) then
                  summass=summass+rho*xsj
               endif
!
               jj1=1
               do jj=1,nope
!
                  ii1=1
                  do ii=1,jj
!
!                   all products of the shape functions for a given ii
!                   and jj
!
                     do i1=1,3
                        do j1=1,3
                           w(i1,j1)=shpj(i1,ii)*shpj(j1,jj)
                        enddo
                     enddo
!
                     if(mattyp.eq.1) then
!
                        do m1=1,3
                           do m2=1,3
                              do m3=1,3
                                 do m4=1,3
                                    s(ii1+m2-1,jj1+m1-1)=
     &                                   s(ii1+m2-1,jj1+m1-1)
     &                                   +v(m4,m3,m2,m1)*w(m4,m3)*weight
                                 enddo
                              enddo
                           enddo
                        enddo
!                      
                     elseif(mattyp.eq.2) then
!
                        call orthonl(w,vo,elas,s,ii1,jj1,weight)
!
                     else
!
                      do i1=1,3
                        iii1=ii1+i1-1
                        do j1=1,3
                          jjj1=jj1+j1-1
                          do k1=1,3
                            do l1=1,3
                              s(iii1,jjj1)=s(iii1,jjj1)
     &                         +anisox(i1,k1,j1,l1)*w(k1,l1)*weight
                              do m1=1,3
                                s(iii1,jjj1)=s(iii1,jjj1)
     &                              +anisox(i1,k1,m1,l1)*w(k1,l1)
     &                                 *vo(j1,m1)*weight
     &                              +anisox(m1,k1,j1,l1)*w(k1,l1)
     &                                 *vo(i1,m1)*weight
                                do n1=1,3
                                  s(iii1,jjj1)=s(iii1,jjj1)
     &                                  +anisox(m1,k1,n1,l1)
     &                                  *w(k1,l1)*vo(i1,m1)*vo(j1,n1)
     &                                  *weight
                                enddo
                              enddo
                            enddo
                          enddo
                        enddo
                      enddo
!SPEC: The immediately preceding loop nest is also available in 
!SPEC: program-generated (much longer) form from the author's 
!SPEC: website (see 454.calculix/Docs) in file anisonl.f
!SPEC:
!SPEC:                   call anisonl(w,vo,elas,s,ii1,jj1,weight)
!SPEC:
                     endif
!
!                   stress stiffness
!
                     senergy=
     &                    (s11*w(1,1)+s12*(w(1,2)+w(2,1))
     &                    +s13*(w(1,3)+w(3,1))+s22*w(2,2)
     &                    +s23*(w(2,3)+w(3,2))+s33*w(3,3))*weight
                     s(ii1,jj1)=s(ii1,jj1)+senergy
                     s(ii1+1,jj1+1)=s(ii1+1,jj1+1)+senergy
                     s(ii1+2,jj1+2)=s(ii1+2,jj1+2)+senergy
!
!                   mass matrix
!
                     if(mass) then
                        sm(ii1,jj1)=sm(ii1,jj1)
     &                       +rho*shpj(4,ii)*shp(4,jj)*weight
                        sm(ii1+1,jj1+1)=sm(ii1,jj1)
                        sm(ii1+2,jj1+2)=sm(ii1,jj1)
                     endif
!
!                    stiffness contribution of centrifugal forces
!
                     if(mass.and.(om.gt.0.d0)) then
                        dmass=shpj(4,ii)*shp(4,jj)*weight*om
                        do m1=1,3
                           s(ii1+m1-1,jj1+m1-1)=s(ii1+m1-1,jj1+m1-1)-
     &                          dmass
                           do m2=1,3
                              s(ii1+m1-1,jj1+m2-1)=s(ii1+m1-1,jj1+m2-1)+
     &                             dmass*p2(m1)*p2(m2)
                           enddo
                        enddo
                     endif
!
                     ii1=ii1+3
                  enddo
                  jj1=jj1+3
               enddo
            endif
!
         endif
!
!           computation of the right hand side
!
         if(rhs) then
!
!             body forces
!
            if(ibod.ne.0) then
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
                  f(jj1)=f(jj1)+bf(1)*shpj(4,jj)*weight
                  f(jj1+1)=f(jj1+1)+bf(2)*shpj(4,jj)*weight
                  f(jj1+2)=f(jj1+2)+bf(3)*shpj(4,jj)*weight
                  ff(jj1)=ff(jj1)+bf(1)*shpj(4,jj)*weight
                  ff(jj1+1)=ff(jj1+1)+bf(2)*shpj(4,jj)*weight
                  ff(jj1+2)=ff(jj1+2)+bf(3)*shpj(4,jj)*weight
                  jj1=jj1+3
               enddo
            endif
!
!             thermal stresses and/or residual stresses
!
            if((ithermal.ne.0).or.(iprestr.ne.0)) then
               do jj=1,6
                  beta(jj)=beta(jj)*xsj
               enddo
               jj1=1
               do jj=1,nope
                  f(jj1)=f(jj1)+(shp(1,jj)*beta(1)+
     &                 (shp(2,jj)*beta(4)+shp(3,jj)*beta(5))/2.d0)
     &                 *weight
                  f(jj1+1)=f(jj1+1)+(shp(2,jj)*beta(2)+
     &                 (shp(1,jj)*beta(4)+shp(3,jj)*beta(6))/2.d0)
     &                 *weight
                  f(jj1+2)=f(jj1+2)+(shp(3,jj)*beta(3)+
     &                 (shp(1,jj)*beta(5)+shp(2,jj)*beta(6))/2.d0)
     &                 *weight
                  jj1=jj1+3
               enddo
            endif
!
         endif
!
      enddo
!
c      write(*,'(6(1x,e11.4))') ((s(i1,j1),i1=1,j1),j1=1,60)
c      write(*,*)
c
      if(.not.buckling) then
!
!       distributed loads
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
                if(mass) then
                   do i=1,nopes
                      do j=1,3
                         xl1(j,i)=co(j,konl(ifaceq(i,ig)))
                      enddo
                   enddo
                endif
                do i=1,nopes
                   do j=1,3
                      xl2(j,i)=co(j,konl(ifaceq(i,ig)))+
     &                     vold(j,konl(ifaceq(i,ig)))
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
                if(mass) then
                   do i=1,nopes
                      do j=1,3
                         xl1(j,i)=co(j,konl(ifacet(i,ig)))
                      enddo
                   enddo
                endif
                do i=1,nopes
                   do j=1,3
                      xl2(j,i)=co(j,konl(ifacet(i,ig)))+
     &                     vold(j,konl(ifacet(i,ig)))
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
                if(mass) then
                   do i=1,nopes
                      do j=1,3
                         xl1(j,i)=co(j,konl(ifacew(i,ig)))
                      enddo
                   enddo
                endif
                do i=1,nopes
                   do j=1,3
                      xl2(j,i)=co(j,konl(ifacew(i,ig)))+
     &                     vold(j,konl(ifacew(i,ig)))
                   enddo
                enddo
             endif
          endif
!
          do i=1,mint2d
             if((lakonl(4:5).eq.'8R').or.
     &            ((lakonl(4:4).eq.'6').and.(nopes.eq.4))) then
                xi=gauss2d1(1,i)
                et=gauss2d1(2,i)
                weight=weight2d1(i)
             elseif((lakonl(4:4).eq.'8').or.
     &               (lakonl(4:6).eq.'20R').or.
     &               ((lakonl(4:5).eq.'15').and.(nopes.eq.8))) then
                xi=gauss2d2(1,i)
                et=gauss2d2(2,i)
                weight=weight2d2(i)
             elseif(lakonl(4:4).eq.'2') then
                xi=gauss2d3(1,i)
                et=gauss2d3(2,i)
                weight=weight2d3(i)
             elseif((lakonl(4:5).eq.'10').or.
     &               ((lakonl(4:5).eq.'15').and.(nopes.eq.6))) then
                xi=gauss2d5(1,i)
                et=gauss2d5(2,i)
                weight=weight2d5(i)
             elseif((lakonl(4:4).eq.'4').or.
     &               ((lakonl(4:4).eq.'6').and.(nopes.eq.3))) then
                xi=gauss2d4(1,i)
                et=gauss2d4(2,i)
                weight=weight2d4(i)
             endif
!
             if(rhs) then
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
                   f(ipointer+1)=f(ipointer+1)-shp2(4,k)*xload(1,id)
     &                  *xsj2(1)*weight
                   f(ipointer+2)=f(ipointer+2)-shp2(4,k)*xload(1,id)
     &                  *xsj2(2)*weight
                   f(ipointer+3)=f(ipointer+3)-shp2(4,k)*xload(1,id)
     &                  *xsj2(3)*weight
                   ff(ipointer+1)=ff(ipointer+1)-shp2(4,k)*xload(1,id)
     &                  *xsj2(1)*weight
                   ff(ipointer+2)=ff(ipointer+2)-shp2(4,k)*xload(1,id)
     &                  *xsj2(2)*weight
                   ff(ipointer+3)=ff(ipointer+3)-shp2(4,k)*xload(1,id)
     &                  *xsj2(3)*weight
                enddo
!
!            stiffness contribution of the distributed load 
!
             elseif(mass) then
                if(nopes.eq.8) then
                   call shape8q(xi,et,xl1,xsj2,shp2)
                elseif(nopes.eq.4) then
                   call shape4q(xi,et,xl1,xsj2,shp2)
                elseif(nopes.eq.6) then
                   call shape6tri(xi,et,xl1,xsj2,shp2)
                else
                   call shape3tri(xi,et,xl1,xsj2,shp2)
                endif
!
!               calculation of the deformation gradient
!
                do k=1,3
                   do l=1,3
                      xkl(k,l)=0.d0
                      do ii=1,nopes
                         xkl(k,l)=xkl(k,l)+shp2(l,ii)*xl2(k,ii)
                      enddo
                   enddo
                enddo
!
                do ii=1,nopes
                   if((nope.eq.20).or.(nope.eq.8)) then
                      ipointeri=(ifaceq(ii,ig)-1)*3
                   elseif((nope.eq.10).or.(nope.eq.4)) then
                     ipointeri=(ifacet(ii,ig)-1)*3
                   else
                      ipointeri=(ifacew(ii,ig)-1)*3
                   endif
                   do jj=1,nopes
                      if((nope.eq.20).or.(nope.eq.8)) then
                         ipointerj=(ifaceq(jj,ig)-1)*3
                      elseif((nope.eq.10).or.(nope.eq.4)) then
                         ipointerj=(ifacet(jj,ig)-1)*3
                      else
                         ipointerj=(ifacew(jj,ig)-1)*3
                      endif
                      do k=1,3
                         do l=1,3
                            if(k.eq.l) cycle
                            if(k*l.eq.2) then
                               n=3
                            elseif(k*l.eq.3) then
                               n=2
                            else
                               n=1
                            endif
                            term=weight*xload(1,id)*shp2(4,jj)*
     &                       (xsj2(1)*
     &                        (xkl(n,2)*shp2(3,ii)-xkl(n,3)*shp2(2,ii))+
     &                        xsj2(2)*
     &                        (xkl(n,3)*shp2(1,ii)-xkl(n,1)*shp2(3,ii))+
     &                        xsj2(3)*
     &                        (xkl(n,1)*shp2(2,ii)-xkl(n,2)*shp2(1,ii)))
                            if(ipointeri+k.le.ipointerj+l) then
                               s(ipointeri+k,ipointerj+l)=
     &                              s(ipointeri+k,ipointerj+l)+term/2.d0
                            else
                               s(ipointerj+l,ipointeri+k)=
     &                              s(ipointerj+l,ipointeri+k)+term/2.d0
                            endif
                         enddo
                      enddo
                   enddo
                enddo
!
             endif
          enddo
!
          id=id-1
       enddo
!
      elseif(mass.and.(iexpl.eq.1)) then
!
!        scaling the diagonal terms of the mass matrix such that the total mass
!        is right (LUMPING; for explicit dynamic calculations)
!
         sume=0.d0
         summ=0.d0
         do i=1,3*nopev,3
            sume=sume+sm(i,i)
         enddo
         do i=3*nopev+1,3*nope,3
            summ=summ+sm(i,i)
         enddo
!
         if(nope.eq.20) then
c            alp=.2215d0
            alp=.2917d0
!              maybe alp=.2917d0 is better??
         elseif(nope.eq.10) then
            alp=0.1203d0
         elseif(nope.eq.15) then
            alp=0.2141d0
         endif
!
         if((nope.eq.20).or.(nope.eq.10).or.(nope.eq.15)) then
            factore=summass*alp/(1.d0+alp)/sume
            factorm=summass/(1.d0+alp)/summ
         else
            factore=summass/sume
         endif
!
         do i=1,3*nopev,3
            sm(i,i)=sm(i,i)*factore
            sm(i+1,i+1)=sm(i,i)
            sm(i+2,i+2)=sm(i,i)
         enddo
         do i=3*nopev+1,3*nope,3
            sm(i,i)=sm(i,i)*factorm
            sm(i+1,i+1)=sm(i,i)
            sm(i+2,i+2)=sm(i,i)
         enddo
!
      endif
!
      return
      end

