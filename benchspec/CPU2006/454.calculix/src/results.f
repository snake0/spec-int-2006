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
      subroutine results(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nelemprint,
     &  neprint,stx,elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,
     &  ielmat,ielorien,norien,orab,ntmat_,t0,t1,ithermal,prestr,
     &  iprestr,noelplab,nodeflab,eei,een,iperturb,f,fn,
     &  nactdof,iout,qa,noprint,nodeprint,vold,b,nodeboun,ndirboun,
     &  xboun,nboun,ipompc,nodempc,coefmpc,labmpc,nmpc,nmethod,vmax,neq,
     &  veold,accold,bet,gam,dtime,plicon,nplicon,plkcon,nplkcon,
     &  xstateini,xstiff,xstate,npmat_,epn,matname,mint_,ielas,icmd,
     &  ncmat_,nstate_,stiini,vini,ikboun,ilboun,ener,enern,sti,
     &  xstaten,eeiini,enerini,cocon,ncocon)
!
!     calculates and prints the displacements, temperatures and forces 
!     at the nodes and the stress and  strain  at the reduced integration 
!     points and at the nodes
!
!     iout=-1: v is assumed to be known and is used to
!              calculate strains, stresses..., no result output
!     iout=0: v is calculated from the system solution
!             and strains, stresses.. are calculated, no result output
!     iout=1:  v is calculated from the system solution and strains,
!              stresses.. are calculated, requested results output
!     iout=2: v is assumed to be known and is used to 
!             calculate strains, stresses..., requested results output
!
      implicit none
!
      logical calcul_fn,calcul_f,calcul_cauchy,calcul_qa,cauchy
!
      character*4 noelplab(*),nodeflab(*)
      character*8 lakon(*)
      character*20 amat,matname(*),labmpc(*)
!
      integer kon(*),konl(20),inum(*),nelemprint(*),
     &  nelcon(2,*),nrhcon(*),nalcon(2,*),ielmat(*),ielorien(*),
     &  ntmat_,ipkon(*),nactdof(0:3,*),nodeprint(*),nodeboun(*),
     &  ndirboun(*),ipompc(*),nodempc(3,*),nelem,ikboun(*),ilboun(*),
     &  ncocon(2,*)
!
      integer nk,ne,neprint,mattyp,ithermal,iprestr,i,j,k,m1,m2,jj,
     &  i1,m3,m4,kk,iener,indexe,nope,norien,iperturb,iout,noprint,
     &  nal,icmd,ihyper,nboun,nmpc,nmethod,ist,ndir,node,index,
     &  neq,kode,imat,mint3d,mint_,nopev,nfield,ndim,iorien,ielas,
     &  istiff,icmdl,ncmat_,nstate_,incrementalmpc,lb,l,jmin,jmax
!
      integer nplicon(0:ntmat_,*),nplkcon(0:ntmat_,*),npmat_
!
      real*8 co(3,*),v(0:3,*),shp(4,20),stiini(6,mint_,*),
     &  stx(6,mint_,*),stn(6,*),xl(3,20),vl(0:3,20),stre(9),
     &  elcon(0:ncmat_,ntmat_,*),rhcon(0:1,ntmat_,*),
     &  alcon(0:6,ntmat_,*),vini(0:3,*),
     &  alzero(*),orab(7,*),elas(21),alph(6),rho,f(*),fn(0:3,*),
     &  skl(3,3),anisox(3,3,3,3),beta(6),q(0:3,20),vkl(0:3,3),
     &  t0(*),t1(*),prestr(6,*),eei(6,mint_,*),een(6,*),ckl(3,3),
     &  vold(0:3,*),b(*),xboun(*),coefmpc(*),vmax,eloc(9),veold(0:3,*),
     &  accold(0:3,*),elconloc(21),eth(6),xkl(3,3),voldl(0:3,20),epn(*),
     &  xikl(3,3),ener(mint_,*),enern(*),sti(6,mint_,*),
     &  eeiini(6,mint_,*),enerini(mint_,*),cocon(0:6,ntmat_,*)
!
      real*8 e,un,al,um,am1,am2,xi,et,ze,tt,exx,eyy,ezz,exy,exz,eyz,
     &  xsj,qa,vj,t0l,t1l,bet,gam,dtime,forcempc,scal1,scal2,bnac,
     &  fixed_disp,weight,pgauss(3),vij,xkapp(6)
!
      real*8 plicon(0:2*npmat_,ntmat_,*),plkcon(0:2*npmat_,ntmat_,*),
     &  xstiff(21,mint_,*),xstate(nstate_,mint_,*),plconloc(82),
     &  vokl(3,3),xstateini(nstate_,mint_,*),vikl(3,3),
     &  xstaten(nstate_,*)
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
      if(ithermal.le.1) then
         jmin=1
         jmax=3
      elseif(ithermal.eq.2) then
         jmin=0
         jmax=0
      else
         jmin=0
         jmax=3
      endif
!
      if((iout.ne.2).and.(iout.ne.-1)) then
!
         if((nmethod.ne.4).or.(iperturb.le.1)) then
!
!     extracting the displacement information from the solution
!
            do i=1,nk
               do j=jmin,jmax
                  if(nactdof(j,i).ne.0) then
                     v(j,i)=b(nactdof(j,i))
                  else
                     v(j,i)=0.d0
                  endif
               enddo
            enddo
!
!     for static perturbation steps v represents the incremental
!     displacements. For the total displacement vold must be added.
!
            if((iperturb.ne.0).and.(nmethod.eq.1)) then
               vmax=0.d0
               do i=1,nk
                  do j=jmin,jmax
                     vmax=max(vmax,dabs(v(j,i)))
c                     write(*,*) i,j,vold(j,i),v(j,i),vmax
                     v(j,i)=v(j,i)+vold(j,i)
                  enddo
               enddo
            endif
!
         else
!
!           direct integration dynamic step
!           b contains the acceleration increment
!
            if((ithermal.le.1).or.(ithermal.eq.3)) then
               scal1=bet*dtime*dtime
               scal2=gam*dtime
               vmax=0.d0
               do i=1,nk
                  do j=1,3
                     if(nactdof(j,i).ne.0) then
                        bnac=b(nactdof(j,i))
                        v(j,i)=vold(j,i)+scal1*bnac
                        vmax=max(vmax,dabs(scal1*bnac))
                        veold(j,i)=veold(j,i)+scal2*bnac
                        accold(j,i)=accold(j,i)+bnac
                     endif
                  enddo
               enddo
            endif
            if(ithermal.ge.2) then
               do i=1,nk
                  if(nactdof(0,i).ne.0) then
                     bnac=b(nactdof(0,i))
                     v(0,i)=vold(0,i)+bnac
                     vmax=max(vmax,dabs(bnac))
                     veold(0,i)=veold(0,i)+bnac/dtime
                  endif
               enddo
            endif
         endif
!
      endif
!
!        initialization
!
      calcul_fn=.false.
      calcul_f=.false.
      calcul_qa=.false.
      calcul_cauchy=.false.
!

!     determining which quantities have to be calculated
!
      if((iperturb.ge.2).or.((iperturb.eq.0).and.(iout.lt.0))) then
         if(iout.lt.1) then
            calcul_fn=.true.
            calcul_f=.true.
            calcul_qa=.true.
         else    
            calcul_cauchy=.true.
         endif
      endif
      if(((noelplab(5).eq.'RF').or.(nodeflab(5).eq.'RF'))
     &     .and.(iout.gt.0)) calcul_fn=.true.
!
!     initializing fn
!
      if(calcul_fn) then
         do i=1,nk
            do j=1,3
               fn(j,i)=0.d0
            enddo
         enddo
      endif
!
!     initializing f
!
      if(calcul_f) then
         do i=1,neq
            f(i)=0.d0
         enddo
      endif
!
!     inserting the boundary conditions
!
      do i=1,nboun
         if(ndirboun(i).gt.3) cycle
         fixed_disp=xboun(i)
         if((nmethod.eq.4).and.(iperturb.gt.1)) then
            ndir=ndirboun(i)
            node=nodeboun(i)
            accold(ndir,node)=(xboun(i)-v(ndir,node))/
     &        (bet*dtime*dtime)
            veold(ndir,node)=veold(ndir,node)+
     &        gam*dtime*accold(ndir,node)
         endif
         v(ndirboun(i),nodeboun(i))=fixed_disp
c         write(*,*) 'boun ',nodeboun(i),ndirboun(i),fixed_disp
      enddo
!
!     inserting the mpc information
!     the parameter incrementalmpc indicates whether the
!     incremental displacements enter the mpc or the total 
!     displacements (incrementalmpc=0)
!
      do i=1,nmpc
         if((labmpc(i)(1:20).eq.'                    ').or.
     &      (labmpc(i)(1:6).eq.'CYCLIC').or.
     &      (labmpc(i)(1:5).eq.'RIGID').or.
     &      (labmpc(i)(1:9).eq.'SUBCYCLIC')) then
            incrementalmpc=0
         else
            incrementalmpc=1
         endif
         ist=ipompc(i)
         node=nodempc(1,ist)
         ndir=nodempc(2,ist)
         index=nodempc(3,ist)
         fixed_disp=0.d0
         if(index.ne.0) then
            do
               if(incrementalmpc.eq.0) then
                  fixed_disp=fixed_disp-coefmpc(index)*
     &                 v(nodempc(2,index),nodempc(1,index))
               else
c                  write(*,102) 
c     &             nodempc(1,index),nodempc(2,index),
c     &             v(nodempc(2,index),nodempc(1,index)),
c     &             vold(nodempc(2,index),nodempc(1,index))
c 102              format('results,node,dir,newvalue,oldvalue',2i5,
c     &                2(1x,e11.4))
                  fixed_disp=fixed_disp-coefmpc(index)*
     &                 (v(nodempc(2,index),nodempc(1,index))-
     &                  vold(nodempc(2,index),nodempc(1,index)))
c                  write(*,*) fixed_disp,
c     & vold(nodempc(2,index),nodempc(1,index)),
c     & v(nodempc(2,index),nodempc(1,index))
               endif
               index=nodempc(3,index)
               if(index.eq.0) exit
            enddo
         endif
         fixed_disp=fixed_disp/coefmpc(ist)
c                  write(*,*) fixed_disp
         if(incrementalmpc.eq.1) then
            fixed_disp=fixed_disp+vold(ndir,node)
c            write(*,*) 'results ',i,vold(ndir,node),fixed_disp
         endif
         if((nmethod.eq.4).and.(iperturb.gt.1)) then
            accold(ndir,node)=(fixed_disp-v(ndir,node))/
     &        (bet*dtime*dtime)
            veold(ndir,node)=veold(ndir,node)+
     &        gam*dtime*accold(ndir,node)
         endif
         v(ndir,node)=fixed_disp
c         write(*,102) node,ndir,v(ndir,node),vold(ndir,node)
      enddo
!
!     check whether there are any strain output requests
!
      if ((noelplab(7).eq.'ENER').or.(nodeflab(7).eq.'ENER')) then
         iener=1
      else
         iener=0
      endif
!
      qa=0.d0
      nal=0
!
!     calculation of the stresses in the integration points
! 
      if((ithermal.le.1).or.(ithermal.eq.3)) then
!
      do i=1,ne
!
         if(ipkon(i).lt.0) cycle
         imat=ielmat(i)
         amat=matname(imat)
         if(norien.gt.0) then
            iorien=ielorien(i)
         else
            iorien=0
         endif
!
c         if(ipkon(i).lt.0) cycle
         indexe=ipkon(i)
         if(lakon(i)(4:4).eq.'2') then
            nope=20
         elseif(lakon(i)(4:4).eq.'8') then
            nope=8
         elseif(lakon(i)(4:5).eq.'10') then
            nope=10
         elseif(lakon(i)(4:4).eq.'4') then
            nope=4
         elseif(lakon(i)(4:5).eq.'15') then
            nope=15
         else
            nope=6
         endif
!
         if(lakon(i)(4:5).eq.'8R') then
            mint3d=1
         elseif((lakon(i)(4:4).eq.'8').or.
     &          (lakon(i)(4:6).eq.'20R')) then
            mint3d=8
         elseif(lakon(i)(4:4).eq.'2') then
            mint3d=27
         elseif(lakon(i)(4:5).eq.'10') then
            mint3d=4
         elseif(lakon(i)(4:4).eq.'4') then
            mint3d=1
         elseif(lakon(i)(4:5).eq.'15') then
            mint3d=9
         else
            mint3d=2
         endif
!
         do j=1,nope
            konl(j)=kon(indexe+j)
            do k=1,3
               xl(k,j)=co(k,konl(j))
               vl(k,j)=v(k,konl(j))
               voldl(k,j)=vold(k,konl(j))
            enddo
         enddo
!
!        check for hyperelastic material
!
         if(nelcon(1,imat).lt.0) then
            ihyper=1
         else
            ihyper=0
         endif
!
!        q contains the nodal forces per element; initialisation of q
!
         if((iperturb.ge.2).or.((iperturb.eq.0).and.(iout.lt.1))) then
            do m1=1,nope
               do m2=1,3
                  q(m2,m1)=fn(m2,konl(m1))
               enddo
            enddo
         endif
!
         do jj=1,mint3d
            if(lakon(i)(4:5).eq.'8R') then
               xi=gauss3d1(1,jj)
               et=gauss3d1(2,jj)
               ze=gauss3d1(3,jj)
               weight=weight3d1(jj)
            elseif((lakon(i)(4:4).eq.'8').or.
     &             (lakon(i)(4:6).eq.'20R'))
     &        then
               xi=gauss3d2(1,jj)
c               if(nope.eq.20) xi=xi+1.d0
               et=gauss3d2(2,jj)
               ze=gauss3d2(3,jj)
               weight=weight3d2(jj)
            elseif(lakon(i)(4:4).eq.'2') then
c               xi=gauss3d3(1,jj)+1.d0
               xi=gauss3d3(1,jj)
               et=gauss3d3(2,jj)
               ze=gauss3d3(3,jj)
               weight=weight3d3(jj)
            elseif(lakon(i)(4:5).eq.'10') then
               xi=gauss3d5(1,jj)
               et=gauss3d5(2,jj)
               ze=gauss3d5(3,jj)
               weight=weight3d5(jj)
            elseif(lakon(i)(4:4).eq.'4') then
               xi=gauss3d4(1,jj)
               et=gauss3d4(2,jj)
               ze=gauss3d4(3,jj)
               weight=weight3d4(jj)
            elseif(lakon(i)(4:5).eq.'15') then
               xi=gauss3d8(1,jj)
               et=gauss3d8(2,jj)
               ze=gauss3d8(3,jj)
               weight=weight3d8(jj)
            else
               xi=gauss3d7(1,jj)
               et=gauss3d7(2,jj)
               ze=gauss3d7(3,jj)
               weight=weight3d7(jj)
            endif
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
!                 vkl(m2,m3) contains the derivative of the m2-
!                 component of the displacement with respect to
!                 direction m3
!
            do m2=1,3
               do m3=1,3
                  vkl(m2,m3)=0.d0
               enddo
            enddo
!
            do m1=1,nope
               do m2=1,3
                  do m3=1,3
                     vkl(m2,m3)=vkl(m2,m3)+shp(m3,m1)*vl(m2,m1)
                  enddo
               enddo
            enddo
!
            kode=nelcon(1,imat)
!
!                 calculating the strain
!
!                 attention! exy,exz and eyz are engineering strains!
!
            exx=vkl(1,1)
            eyy=vkl(2,2)
            ezz=vkl(3,3)
            exy=vkl(1,2)+vkl(2,1)
            exz=vkl(1,3)+vkl(3,1)
            eyz=vkl(2,3)+vkl(3,2)
!
!                 Lagrangian/Eulerian strain tensor for geometric
!                 nonlinear calculations
!
            if(iperturb.ge.2) then
!
!                    Lagrangian strain
!
               exx=exx+(vkl(1,1)**2+vkl(2,1)**2+vkl(3,1)**2)/2.d0
               eyy=eyy+(vkl(1,2)**2+vkl(2,2)**2+vkl(3,2)**2)/2.d0
               ezz=ezz+(vkl(1,3)**2+vkl(2,3)**2+vkl(3,3)**2)/2.d0
               exy=exy+vkl(1,1)*vkl(1,2)+vkl(2,1)*vkl(2,2)+
     &              vkl(3,1)*vkl(3,2)
               exz=exz+vkl(1,1)*vkl(1,3)+vkl(2,1)*vkl(2,3)+
     &              vkl(3,1)*vkl(3,3)
               eyz=eyz+vkl(1,2)*vkl(1,3)+vkl(2,2)*vkl(2,3)+
     &              vkl(3,2)*vkl(3,3)
!
            endif
!
!                 storing the local strains
!
            if((iperturb.ne.0).or.(iout.ge.0)) then
               eloc(1)=exx
               eloc(2)=eyy
               eloc(3)=ezz
               eloc(4)=exy/2.d0
               eloc(5)=exz/2.d0
               eloc(6)=eyz/2.d0
            else
!
!                    linear iteration within a nonlinear increment:
!                    tangent matrix is calculated at strain at
!                    the end of the previous increment
!
               do m1=1,6
                  eloc(m1)=eei(m1,jj,i)
               enddo
            endif
!
!                 calculating the deformation gradient (needed to
!                 convert the element stiffness matrix from spatial
!                 coordinates to material coordinates
!                 deformation plasticity)
!
            if((kode.eq.-50).or.(kode.le.-100)) then
!
!                    calculating the deformation gradient
!
               xkl(1,1)=vkl(1,1)+1
               xkl(2,2)=vkl(2,2)+1.
               xkl(3,3)=vkl(3,3)+1.
               xkl(1,2)=vkl(1,2)
               xkl(1,3)=vkl(1,3)
               xkl(2,3)=vkl(2,3)
               xkl(2,1)=vkl(2,1)
               xkl(3,1)=vkl(3,1)
               xkl(3,2)=vkl(3,2)
!
!                    calculating the Jacobian
!
               vj=xkl(1,1)*(xkl(2,2)*xkl(3,3)-xkl(2,3)*xkl(3,2))
     &              -xkl(1,2)*(xkl(2,1)*xkl(3,3)-xkl(2,3)*xkl(3,1))
     &              +xkl(1,3)*(xkl(2,1)*xkl(3,2)-xkl(2,2)*xkl(3,1))
!
!              inversion of the deformation gradient (only for
!              deformation plasticity)
!
               if(kode.eq.-50) then
!
                  ckl(1,1)=(xkl(2,2)*xkl(3,3)-xkl(2,3)*xkl(3,2))/vj
                  ckl(2,2)=(xkl(1,1)*xkl(3,3)-xkl(1,3)*xkl(3,1))/vj
                  ckl(3,3)=(xkl(1,1)*xkl(2,2)-xkl(1,2)*xkl(2,1))/vj
                  ckl(1,2)=(xkl(1,3)*xkl(3,2)-xkl(1,2)*xkl(3,3))/vj
                  ckl(1,3)=(xkl(1,2)*xkl(2,3)-xkl(2,2)*xkl(1,3))/vj
                  ckl(2,3)=(xkl(2,1)*xkl(1,3)-xkl(1,1)*xkl(2,3))/vj
                  ckl(2,1)=(xkl(3,1)*xkl(2,3)-xkl(2,1)*xkl(3,3))/vj
                  ckl(3,1)=(xkl(2,1)*xkl(3,2)-xkl(2,2)*xkl(3,1))/vj
                  ckl(3,2)=(xkl(3,1)*xkl(1,2)-xkl(1,1)*xkl(3,2))/vj
!
!                 converting the Lagrangian strain into Eulerian
!                 strain (only for deformation plasticity)
!
                  cauchy=.false.
                  call str2mat(eloc,ckl,vj,cauchy)
               endif
!                        
            endif
!
!                 calculating fields for incremental plasticity
!
            if(kode.le.-100) then
!
!              calculating the deformation gradient at the
!              start of the increment
!
!              calculating the displacement gradient at the
!              start of the increment
!
               do m2=1,3
                  do m3=1,3
                     vikl(m2,m3)=0.d0
                  enddo
               enddo
!
               do m1=1,nope
                  do m2=1,3
                     do m3=1,3
                        vikl(m2,m3)=vikl(m2,m3)
     &                       +shp(m3,m1)*vini(m2,konl(m1))
                     enddo
                  enddo
               enddo
!
!              calculating the deformation gradient of the old
!              fields
!
               xikl(1,1)=vikl(1,1)+1
               xikl(2,2)=vikl(2,2)+1.
               xikl(3,3)=vikl(3,3)+1.
               xikl(1,2)=vikl(1,2)
               xikl(1,3)=vikl(1,3)
               xikl(2,3)=vikl(2,3)
               xikl(2,1)=vikl(2,1)
               xikl(3,1)=vikl(3,1)
               xikl(3,2)=vikl(3,2)
!
!              calculating the Jacobian
!
               vij=xikl(1,1)*(xikl(2,2)*xikl(3,3)
     &              -xikl(2,3)*xikl(3,2))
     &              -xikl(1,2)*(xikl(2,1)*xikl(3,3)
     &              -xikl(2,3)*xikl(3,1))
     &              +xikl(1,3)*(xikl(2,1)*xikl(3,2)
     &              -xikl(2,2)*xikl(3,1))
!
!              stresses at the start of the increment
!
               do m1=1,6
                  stre(m1)=stiini(m1,jj,i)
               enddo
!
            endif
!
!                 prestress values
!
            if(iprestr.eq.0) then
               do kk=1,6
                  beta(kk)=0.d0
               enddo
            else
               do kk=1,6
                  beta(kk)=-prestr(kk,i)
               enddo
            endif
!
            if(ithermal.ge.1) then
!
!              calculating the temperature difference in
!              the integration point
!
               t0l=0.d0
               t1l=0.d0
               if(ithermal.eq.1) then
                  if(lakon(i)(4:5).eq.'8 ') then
                     do i1=1,nope
                        t0l=t0l+t0(konl(i1))/8.d0
                        t1l=t1l+t1(konl(i1))/8.d0
                     enddo
                  elseif(lakon(i)(4:6).eq.'20 ') then
                     call lintemp(t0,t1,konl,nope,jj,t0l,t1l)
                  else
                     do i1=1,nope
                        t0l=t0l+shp(4,i1)*t0(konl(i1))
                        t1l=t1l+shp(4,i1)*t1(konl(i1))
                     enddo
                  endif
               elseif(ithermal.ge.2) then
                  if(lakon(i)(4:5).eq.'8 ') then
                     do i1=1,nope
                        t0l=t0l+t0(konl(i1))/8.d0
                        t1l=t1l+vold(0,konl(i1))/8.d0
                     enddo
                  elseif(lakon(i)(4:6).eq.'20 ') then
                     call lintemp_th(t0,vold,konl,nope,jj,t0l,t1l)
                  else
                     do i1=1,nope
                        t0l=t0l+shp(4,i1)*t0(konl(i1))
                        t1l=t1l+shp(4,i1)*vold(0,konl(i1))
                     enddo
                  endif
               endif
               tt=t1l-t0l
            endif
!
!                 calculating the coordinates of the integration point
!                 for material orientation purposes (for cylindrical
!                 coordinate systems)
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
!                 material data; for linear elastic materials
!                 this includes the calculation of the stiffness
!                 matrix
!
            istiff=0
!
            call materialdata(elcon,nelcon,rhcon,nrhcon,alcon,
     &           nalcon,imat,amat,iorien,pgauss,orab,ntmat_,
     &           elas,alph,rho,i,ithermal,alzero,mattyp,
     &           t0l,t1l,ihyper,istiff,
     &           elconloc,eth,kode,plicon,nplicon,
     &           plkcon,nplkcon,npmat_,
     &           plconloc,mint_,dtime,i,jj,
     &           xstiff,ncmat_)
!
            if((ihyper.eq.0).and.
     &           ((iperturb.ne.0).or.(iout.ge.0))) then
               if(mattyp.eq.1) then
                  e=elas(1)
                  un=elas(2)
                  al=un*e/(1.d0+un)/(1.d0-2.d0*un)
                  um=e/2.d0/(1.d0+un)
                  am1=al+2.d0*um
                  am2=2.d0*um
               elseif(mattyp.eq.3) then
                  call anisotropic(elas,anisox)
               endif
!
               icmdl=3
               call linel(ithermal,mattyp,beta,al,um,am1,alph,tt,
     &              elas,icmdl,exx,eyy,ezz,exy,exz,eyz,stre,
     &              anisox)
!
            elseif(ihyper.ne.0) then
               mattyp=3
               call hyperel(elconloc,elas,eloc,kode,eth,ithermal,
     &              icmd,beta,stre,xkl,ckl,vj,xikl,vij,
     &              plconloc,xstate,xstateini,ielas,
     &              amat,t1l,dtime,i,jj,nstate_,mint_,
     &              iorien,pgauss,orab)
               do m1=1,21
                  xstiff(m1,jj,i)=elas(m1)
               enddo
            endif
!
            if((iperturb.eq.0).and.(iout.lt.0)) then
!
!                    if the forced displacements were changed at
!                    the start of a nonlinear step, the nodal
!                    forces due do this displacements are 
!                    calculated in a purely linear way, and
!                    the first iteration is purely linear in order
!                    to allow the displacements to redistribute
!                    in a quasi-static way (only applies to
!                    quasi-static analyses (*STATIC))
!
               do m2=1,3
                  do m3=1,3
                     vokl(m2,m3)=0.d0
                  enddo
               enddo
!
               do m1=1,nope
                  do m2=1,3
                     do m3=1,3
                        vokl(m2,m3)=vokl(m2,m3)+
     &                       shp(m3,m1)*voldl(m2,m1)
                     enddo
                  enddo
               enddo
!
               eloc(1)=exx-vokl(1,1)
               eloc(2)=eyy-vokl(2,2)
               eloc(3)=ezz-vokl(3,3)
               eloc(4)=exy-(vokl(1,2)+vokl(2,1))
               eloc(5)=exz-(vokl(1,3)+vokl(3,1))
               eloc(6)=eyz-(vokl(2,3)+vokl(3,2))
               if(mattyp.eq.1) then
                  e=elas(1)
                  un=elas(2)
                  um=e/(1.d0+un)
                  al=un*um/(1.d0-2.d0*un)
                  um=um/2.d0
                  am1=al*(eloc(1)+eloc(2)+eloc(3))
                  stre(1)=am1+2.d0*um*eloc(1)
                  stre(2)=am1+2.d0*um*eloc(2)
                  stre(3)=am1+2.d0*um*eloc(3)
                  stre(4)=um*eloc(4)
                  stre(5)=um*eloc(5)
                  stre(6)=um*eloc(6)
               elseif(mattyp.eq.2) then
                  stre(1)=eloc(1)*elas(1)+eloc(2)*elas(2)
     &                 +eloc(3)*elas(4)
                  stre(2)=eloc(1)*elas(2)+eloc(2)*elas(3)
     &                 +eloc(3)*elas(5)
                  stre(3)=eloc(1)*elas(4)+eloc(2)*elas(5)
     &                 +eloc(3)*elas(6)
                  stre(4)=eloc(4)*elas(7)
                  stre(5)=eloc(5)*elas(8)
                  stre(6)=eloc(6)*elas(9)
               elseif(mattyp.eq.3) then
                  stre(1)=eloc(1)*elas(1)+eloc(2)*elas(2)+
     &                 eloc(3)*elas(4)+eloc(4)*elas(7)+
     &                 eloc(5)*elas(11)+eloc(6)*elas(16)
                  stre(2)=eloc(1)*elas(2)+eloc(2)*elas(3)+
     &                 eloc(3)*elas(5)+eloc(4)*elas(8)+
     &                 eloc(5)*elas(12)+eloc(6)*elas(17)
                  stre(3)=eloc(1)*elas(4)+eloc(2)*elas(5)+
     &                 eloc(3)*elas(6)+eloc(4)*elas(9)+
     &                 eloc(5)*elas(13)+eloc(6)*elas(18)
                  stre(4)=eloc(1)*elas(7)+eloc(2)*elas(8)+
     &                 eloc(3)*elas(9)+eloc(4)*elas(10)+
     &                 eloc(5)*elas(14)+eloc(6)*elas(19)
                  stre(5)=eloc(1)*elas(11)+eloc(2)*elas(12)+
     &                 eloc(3)*elas(13)+eloc(4)*elas(14)+
     &                 eloc(5)*elas(15)+eloc(6)*elas(20)
                  stre(6)=eloc(1)*elas(16)+eloc(2)*elas(17)+
     &                 eloc(3)*elas(18)+eloc(4)*elas(19)+
     &                 eloc(5)*elas(20)+eloc(6)*elas(21)
               endif
            endif
! 
!           updating the energy
!
            if(iener.eq.1) then
               ener(jj,i)=enerini(jj,i)+
     &           ((eloc(1)-eeiini(1,jj,i))*(stre(1)+stiini(1,jj,i))+
     &            (eloc(2)-eeiini(2,jj,i))*(stre(2)+stiini(2,jj,i))+
     &            (eloc(3)-eeiini(3,jj,i))*(stre(3)+stiini(3,jj,i)))
     &            /2.d0+
     &            (eloc(4)-eeiini(4,jj,i))*(stre(4)+stiini(4,jj,i))+
     &            (eloc(5)-eeiini(5,jj,i))*(stre(5)+stiini(5,jj,i))+
     &            (eloc(6)-eeiini(6,jj,i))*(stre(6)+stiini(6,jj,i))
c               write(*,*) 'energy',jj,i,ener(jj,i)
            endif
!
            eei(1,jj,i)=eloc(1)
            eei(2,jj,i)=eloc(2)
            eei(3,jj,i)=eloc(3)
            eei(4,jj,i)=eloc(4)
            eei(5,jj,i)=eloc(5)
            eei(6,jj,i)=eloc(6)
!
            skl(1,1)=stre(1)
            skl(2,2)=stre(2)
            skl(3,3)=stre(3)
            skl(2,1)=stre(4)
            skl(3,1)=stre(5)
            skl(3,2)=stre(6)
!
            stx(1,jj,i)=skl(1,1)
            stx(2,jj,i)=skl(2,2)
            stx(3,jj,i)=skl(3,3)
            stx(4,jj,i)=skl(2,1)
            stx(5,jj,i)=skl(3,1)
            stx(6,jj,i)=skl(3,2)
!
            skl(1,2)=skl(2,1)
            skl(1,3)=skl(3,1)
            skl(2,3)=skl(3,2)
!
!                 calculation of the nodal forces
!
            if(calcul_fn)then
!
!                    calculating fn using skl
!
               do m1=1,nope
                  do m2=1,3
!
!                          linear elastic part
!                           
                     do m3=1,3
                        fn(m2,konl(m1))=fn(m2,konl(m1))+
     &                       xsj*skl(m2,m3)*shp(m3,m1)*weight
                     enddo
!
!                          nonlinear geometric part
!
                     if(iperturb.ge.2) then
                        do m3=1,3
                           do m4=1,3
                              fn(m2,konl(m1))=fn(m2,konl(m1))+
     &                             xsj*skl(m4,m3)*weight*
     &                             (vkl(m2,m4)*shp(m3,m1)+
     &                             vkl(m2,m3)*shp(m4,m1))/2.d0
                           enddo
                        enddo
                     endif
!
                  enddo
               enddo
            endif
!
!           calculation of the Cauchy stresses
!
            if(calcul_cauchy) then
!
!              changing the displacement gradients into
!              deformation gradients
!
               if(kode.ne.-50) then
                  xkl(1,1)=vkl(1,1)+1
                  xkl(2,2)=vkl(2,2)+1.
                  xkl(3,3)=vkl(3,3)+1.
                  xkl(1,2)=vkl(1,2)
                  xkl(1,3)=vkl(1,3)
                  xkl(2,3)=vkl(2,3)
                  xkl(2,1)=vkl(2,1)
                  xkl(3,1)=vkl(3,1)
                  xkl(3,2)=vkl(3,2)
!
                  vj=xkl(1,1)*(xkl(2,2)*xkl(3,3)-xkl(2,3)*xkl(3,2))
     &                 -xkl(1,2)*(xkl(2,1)*xkl(3,3)-xkl(2,3)*xkl(3,1))
     &                 +xkl(1,3)*(xkl(2,1)*xkl(3,2)-xkl(2,2)*xkl(3,1))
               endif
!
               do m1=1,3
                  do m2=1,m1
                     ckl(m1,m2)=0.d0
                     do m3=1,3
                        do m4=1,3
                           ckl(m1,m2)=ckl(m1,m2)+
     &                          skl(m3,m4)*xkl(m1,m3)*xkl(m2,m4)
                        enddo
                     enddo
                     ckl(m1,m2)=ckl(m1,m2)/vj
                  enddo
               enddo
!
               stx(1,jj,i)=ckl(1,1)
               stx(2,jj,i)=ckl(2,2)
               stx(3,jj,i)=ckl(3,3)
               stx(4,jj,i)=ckl(2,1)
               stx(5,jj,i)=ckl(3,1)
               stx(6,jj,i)=ckl(3,2)
            endif
!
         enddo
!
!        q contains the contributions to the nodal force in the nodes
!        belonging to the element at stake from other elements (elements
!        already treated). These contributions have to be
!        subtracted to get the contributions attributable to the element
!        at stake only
!
         if(calcul_qa) then
            do m1=1,nope
               do m2=1,3
                  qa=qa+dabs(fn(m2,konl(m1))-q(m2,m1))
               enddo
            enddo
            nal=nal+3*nope
         endif
      enddo
!
      endif
!
!     calculation of temperatures and thermal flux
!
      if(ithermal.ge.2) then
!
      do i=1,ne
!
         if(ipkon(i).lt.0) cycle
         imat=ielmat(i)
         amat=matname(imat)
         if(norien.gt.0) then
            iorien=ielorien(i)
         else
            iorien=0
         endif
!
         indexe=ipkon(i)
         if(lakon(i)(4:4).eq.'2') then
            nope=20
         elseif(lakon(i)(4:4).eq.'8') then
            nope=8
         elseif(lakon(i)(4:5).eq.'10') then
            nope=10
         elseif(lakon(i)(4:4).eq.'4') then
            nope=4
         elseif(lakon(i)(4:5).eq.'15') then
            nope=15
         else
            nope=6
         endif
!
         if(lakon(i)(4:5).eq.'8R') then
            mint3d=1
         elseif((lakon(i)(4:4).eq.'8').or.
     &          (lakon(i)(4:6).eq.'20R')) then
            mint3d=8
         elseif(lakon(i)(4:4).eq.'2') then
            mint3d=27
         elseif(lakon(i)(4:5).eq.'10') then
            mint3d=4
         elseif(lakon(i)(4:4).eq.'4') then
            mint3d=1
         elseif(lakon(i)(4:5).eq.'15') then
            mint3d=9
         else
            mint3d=2
         endif
!
         do j=1,nope
            konl(j)=kon(indexe+j)
            do k=1,3
               xl(k,j)=co(k,konl(j))
            enddo
            vl(0,j)=v(0,konl(j))
            voldl(0,j)=vold(0,konl(j))
         enddo
!
!        q contains the nodal forces per element; initialisation of q
!
         if((iperturb.ge.2).or.((iperturb.eq.0).and.(iout.lt.1))) then
            do m1=1,nope
               q(0,m1)=fn(0,konl(m1))
            enddo
         endif
!
         do jj=1,mint3d
            if(lakon(i)(4:5).eq.'8R') then
               xi=gauss3d1(1,jj)
               et=gauss3d1(2,jj)
               ze=gauss3d1(3,jj)
               weight=weight3d1(jj)
            elseif((lakon(i)(4:4).eq.'8').or.
     &             (lakon(i)(4:6).eq.'20R'))
     &        then
               xi=gauss3d2(1,jj)
               et=gauss3d2(2,jj)
               ze=gauss3d2(3,jj)
               weight=weight3d2(jj)
            elseif(lakon(i)(4:4).eq.'2') then
               xi=gauss3d3(1,jj)
               et=gauss3d3(2,jj)
               ze=gauss3d3(3,jj)
               weight=weight3d3(jj)
            elseif(lakon(i)(4:5).eq.'10') then
               xi=gauss3d5(1,jj)
               et=gauss3d5(2,jj)
               ze=gauss3d5(3,jj)
               weight=weight3d5(jj)
            elseif(lakon(i)(4:4).eq.'4') then
               xi=gauss3d4(1,jj)
               et=gauss3d4(2,jj)
               ze=gauss3d4(3,jj)
               weight=weight3d4(jj)
            elseif(lakon(i)(4:5).eq.'15') then
               xi=gauss3d8(1,jj)
               et=gauss3d8(2,jj)
               ze=gauss3d8(3,jj)
               weight=weight3d8(jj)
            else
               xi=gauss3d7(1,jj)
               et=gauss3d7(2,jj)
               ze=gauss3d7(3,jj)
               weight=weight3d7(jj)
            endif
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
!                 vkl(m2,m3) contains the derivative of the m2-
!                 component of the displacement with respect to
!                 direction m3
!
            do m3=1,3
               vkl(0,m3)=0.d0
            enddo
!
            do m1=1,nope
               do m3=1,3
                  vkl(0,m3)=vkl(0,m3)+shp(m3,m1)*vl(0,m1)
               enddo
            enddo
!
            kode=nelcon(1,imat)
!
!           storing the temperature gradients
!
            eloc(7)=vkl(0,1)
            eloc(8)=vkl(0,2)
            eloc(9)=vkl(0,3)
!
!              calculating the temperature difference in
!              the integration point
!
            t1l=0.d0
            if(lakon(i)(4:5).eq.'8 ') then
               do i1=1,nope
                  t1l=t1l+vold(0,konl(i1))/8.d0
               enddo
            elseif(lakon(i)(4:6).eq.'20 ') then
               call lintemp_th(t0,vold,konl,nope,jj,t0l,t1l)
            else
               do i1=1,nope
                  t1l=t1l+shp(4,i1)*vold(0,konl(i1))
               enddo
            endif
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
!                 material data; for linear elastic materials
!                 this includes the calculation of the stiffness
!                 matrix
!
            call matdata_co(cocon,ncocon,imat,iorien,pgauss,orab,ntmat_,
     &           xkapp,mattyp,t1l)
!
            if(mattyp.eq.1) then
               do m1=1,3
                  stre(6+m1)=xkapp(1)*vkl(0,m1)
               enddo
            elseif(mattyp.eq.2) then
               stre(7)=xkapp(1)*vkl(0,1)
               stre(8)=xkapp(2)*vkl(0,2)
               stre(9)=xkapp(3)*vkl(0,3)
            else
               stre(7)=xkapp(1)*vkl(0,1)+xkapp(2)*vkl(0,2)+
     &                 xkapp(4)*vkl(0,3)
               stre(8)=xkapp(2)*vkl(0,1)+xkapp(3)*vkl(0,2)+
     &                 xkapp(5)*vkl(0,3)
               stre(9)=xkapp(4)*vkl(0,1)+xkapp(5)*vkl(0,2)+
     &                 xkapp(6)*vkl(0,3)
            endif
! 
!           updating the energy
!
c            if(iener.eq.1) then
c               ener(jj,i)=enerini(jj,i)+
c     &           ((eloc(1)-eeiini(1,jj,i))*(stre(1)+stiini(1,jj,i))+
c     &            (eloc(2)-eeiini(2,jj,i))*(stre(2)+stiini(2,jj,i))+
c     &            (eloc(3)-eeiini(3,jj,i))*(stre(3)+stiini(3,jj,i)))
c     &            /2.d0+
c     &            (eloc(4)-eeiini(4,jj,i))*(stre(4)+stiini(4,jj,i))+
c     &            (eloc(5)-eeiini(5,jj,i))*(stre(5)+stiini(5,jj,i))+
c     &            (eloc(6)-eeiini(6,jj,i))*(stre(6)+stiini(6,jj,i))
c               write(*,*) 'energy',jj,i,ener(jj,i)
c            endif
!
!           dimension of stx should be 9!!!!!!
!
c            stx(7,jj,i)=skl(1,1)
c            stx(8,jj,i)=skl(2,2)
c            stx(9,jj,i)=skl(3,3)
!
!           calculation of the nodal flux
!
            if(calcul_fn)then
!
!                    calculating fn using skl
!
               do m1=1,nope
                  do m3=1,3
                     fn(0,konl(m1))=fn(0,konl(m1))+
     &                    xsj*stre(m3)*shp(m3,m1)*weight
                  enddo
               enddo
            endif
         enddo
!
!        q contains the contributions to the nodal force in the nodes
!        belonging to the element at stake from other elements (elements
!        already treated). These contributions have to be
!        subtracted to get the contributions attributable to the element
!        at stake only
!
         if(calcul_qa) then
            do m1=1,nope
               do m2=1,3
                  qa=qa+dabs(fn(m2,konl(m1))-q(m2,m1))
               enddo
            enddo
            nal=nal+3*nope
         endif
      enddo
!
      endif
!
      if(calcul_qa) qa=qa/nal
!
!     subtracting the mpc force (for each linear mpc there is one
!     force; the actual force in a node belonging to the mpc is
!     obtained by multiplying this force with the nodal coefficient.
!     The force has to be subtracted from f, since it does not
!     appear on the rhs of the equations system
!
        do i=1,nmpc
            ist=ipompc(i)
            node=nodempc(1,ist)
            ndir=nodempc(2,ist)
            forcempc=fn(ndir,node)/coefmpc(ist)
            fn(ndir,node)=0.d0
            index=nodempc(3,ist)
            if(index.eq.0) cycle
            do
               node=nodempc(1,index)
               ndir=nodempc(2,index)
               fn(ndir,node)=fn(ndir,node)-coefmpc(index)*forcempc
               index=nodempc(3,index)
               if(index.eq.0) exit
            enddo
         enddo
!
!     calculating the system force vector
!
      if(calcul_f) then
         do i=1,nk
            do j=1,3
c               write(*,500) i,j,fn(j,i)
c 500           format(i5,',',i5,',',e11.4)
               if(nactdof(j,i).ne.0) then
                  f(nactdof(j,i))=fn(j,i)
               endif
            enddo
         enddo
!
!     subtracting the mpc force (for each linear mpc there is one
!     force; the actual force in a node belonging to the mpc is
!     obtained by multiplying this force with the nodal coefficient.
!     The force has to be subtracted from f, since it does not
!     appear on the rhs of the equations system
!
c         do i=1,nmpc
c            ist=ipompc(i)
c            node=nodempc(1,ist)
c            ndir=nodempc(2,ist)
c            forcempc=fn(ndir,node)/coefmpc(ist)
c            index=nodempc(3,ist)
c            if(index.eq.0) cycle
c            do
c               node=nodempc(1,index)
c               ndir=nodempc(2,index)
c               j=nactdof(ndir,node)
c
c to be modified for nodes which do not belong to any elements:
c  thus there is no stress and no force is calculated
c  example: rigid body motion
c
c               if(j.ne.0) then
c                  f(j)=f(j)-coefmpc(index)*forcempc
c               endif
c               index=nodempc(3,index)
c               if(index.eq.0) exit
c            enddo
c         enddo
      endif
!
!     no print requests
!
      if(iout.le.0) return
!
!     nodal printing requests
!
      write(*,*) 'Printing nodal quantities'
      write(*,*)
!
      if(noprint.ne.0) then
         if(noelplab(1).eq.'U   ') then
            write(5,*)
            write(5,*) 'displacements (vx,vy,vz)'
            write(5,*)
            do i=1,noprint
               write(5,'(i5,1p,3(1x,e11.4))') nodeprint(i),
     &              (v(j,nodeprint(i)),j=1,3)
            enddo
         endif
!
         if(noelplab(2).eq.'NT  ') then
            write(5,*)
            write(5,*) 'temperature'
            write(5,*)
            if(ithermal.le.1) then
               do i=1,noprint
                  write(5,'(i5,1x,1p,e11.4)') nodeprint(i),
     &                 t1(nodeprint(i))
               enddo
            else
               do i=1,noprint
                  write(5,'(i5,1x,1p,e11.4)') nodeprint(i),
     &                 vold(0,nodeprint(i))
               enddo
            endif
         endif
         
!
         if(noelplab(5).eq.'RF  ') then
            write(5,*)
            write(5,*) 'forces (fx,fy,fz)'
            write(5,*)
            do i=1,noprint
               write(5,'(i5,1p,3(1x,e11.4))') nodeprint(i),
     &              (fn(j,nodeprint(i)),j=1,3)
            enddo
         endif
      endif
!
!     elemental printing requests
!
      write(*,*) 'Printing element quantities'
      write(*,*)
!
      if(neprint.ne.0) then
         if(noelplab(3).eq.'S   ') then
            write(5,*)
            write(5,*) 
     &         'stresses (elem, integ.pnt.,sxx,syy,szz,sxy,sxz,syz)'
            write(5,*)
            do i=1,neprint
               nelem=nelemprint(i)
               if(ipkon(nelem).lt.0) cycle
               if(lakon(nelem)(4:5).eq.'8R') then
                  mint3d=1
               elseif((lakon(nelem)(4:4).eq.'8').or.
     &                (lakon(nelem)(4:6).eq.'20R')) then
                  mint3d=8
               elseif(lakon(nelem)(4:4).eq.'2') then
                  mint3d=27
               elseif(lakon(nelem)(4:5).eq.'10') then
                  mint3d=4
               elseif(lakon(nelem)(4:4).eq.'4') then
                  mint3d=1
               elseif(lakon(nelem)(4:5).eq.'15') then
                  mint3d=9
               else
                  mint3d=2
               endif
               do j=1,mint3d
                  write(5,'(2i5,1p,6(1x,e11.4))') nelem,j,
     &                 (stx(k,j,nelem),k=1,6)
               enddo
            enddo
         endif
!
         if(noelplab(4).eq.'E   ') then
            write(5,*)
            write(5,*) 
     &         'strains (elem, integ.pnt.,exx,eyy,ezz,exy,exz,eyz)'
            write(5,*)
            do i=1,neprint
               nelem=nelemprint(i)
               if(ipkon(nelem).lt.0) cycle
               if(lakon(nelem)(4:5).eq.'8R') then
                  mint3d=1
               elseif((lakon(nelem)(4:4).eq.'8').or.
     &                (lakon(nelem)(4:6).eq.'20R')) then
                  mint3d=8
               elseif(lakon(nelem)(4:4).eq.'2') then
                  mint3d=27
               elseif(lakon(nelem)(4:5).eq.'10') then
                  mint3d=4
               elseif(lakon(nelem)(4:4).eq.'4') then
                  mint3d=1
               elseif(lakon(nelem)(4:5).eq.'15') then
                  mint3d=9
               else
                  mint3d=2
               endif
               do j=1,mint3d
                  write(5,'(2i5,1p,6(1x,e11.4))') nelem,j,
     &                 (eei(k,j,nelem),k=1,6)
               enddo
            enddo
         endif
!
         if(noelplab(6).eq.'PE  ') then
            write(5,*)
            write(5,*) 
     &         'equivalent plastic strain (elem, integ.pnt.,pe)'
            write(5,*)
            do i=1,neprint
               nelem=nelemprint(i)
               if(ipkon(nelem).lt.0) cycle
               if(lakon(nelem)(4:5).eq.'8R') then
                  mint3d=1
               elseif((lakon(nelem)(4:4).eq.'8').or.
     &                (lakon(nelem)(4:6).eq.'20R')) then
                  mint3d=8
               elseif(lakon(nelem)(4:4).eq.'2') then
                  mint3d=27
               elseif(lakon(nelem)(4:5).eq.'10') then
                  mint3d=4
               elseif(lakon(nelem)(4:4).eq.'4') then
                  mint3d=1
               elseif(lakon(nelem)(4:5).eq.'15') then
                  mint3d=9
               else
                  mint3d=2
               endif
               do j=1,mint3d
                  write(5,'(2i5,1p,6(1x,e11.4))') nelem,j,
     &                 xstate(1,j,nelem)
               enddo
            enddo
         endif
!
         if(noelplab(7).eq.'ENER') then
            write(5,*)
            write(5,*) 
     &         'energy density (elem, integ.pnt.,energy)'
            write(5,*)
            do i=1,neprint
               nelem=nelemprint(i)
               if(ipkon(nelem).lt.0) cycle
               if(lakon(nelem)(4:5).eq.'8R') then
                  mint3d=1
               elseif((lakon(nelem)(4:4).eq.'8').or.
     &                (lakon(nelem)(4:6).eq.'20R')) then
                  mint3d=8
               elseif(lakon(nelem)(4:4).eq.'2') then
                  mint3d=27
               elseif(lakon(nelem)(4:5).eq.'10') then
                  mint3d=4
               elseif(lakon(nelem)(4:4).eq.'4') then
                  mint3d=1
               elseif(lakon(nelem)(4:5).eq.'15') then
                  mint3d=9
               else
                  mint3d=2
               endif
               do j=1,mint3d
                  write(5,'(2i5,1p,6(1x,e11.4))') nelem,j,
     &                 ener(j,nelem)
               enddo
            enddo
         endif
!
         if(noelplab(8).eq.'SDV ') then
            do l=1,(nstate_+5)/6
               lb=(l-1)*6
               write(5,*)
               if(l.eq.(nstate_+5)/6) then
                  write(5,300) (lb+k,k=1,nstate_-lb)
               else
                  write(5,300) (lb+k,k=1,6)
               endif
 300           format('internal state variables (elem, integ.pnt.)',
     &             6(i2))
               write(5,*)
               do i=1,neprint
                  nelem=nelemprint(i)
                  if(ipkon(nelem).lt.0) cycle
                  if(lakon(nelem)(4:5).eq.'8R') then
                     mint3d=1
                  elseif((lakon(nelem)(4:4).eq.'8').or.
     &                    (lakon(nelem)(4:6).eq.'20R')) then
                     mint3d=8
                  elseif(lakon(nelem)(4:4).eq.'2') then
                     mint3d=27
                  elseif(lakon(nelem)(4:5).eq.'10') then
                     mint3d=4
                  elseif(lakon(nelem)(4:4).eq.'4') then
                     mint3d=1
                  elseif(lakon(nelem)(4:5).eq.'15') then
                     mint3d=9
                  else
                     mint3d=2
                  endif
                  do j=1,mint3d
                     if(l.eq.(nstate_+5)/6) then
                        write(5,'(2i5,1p,6(1x,e11.4))') nelem,j,
     &                       (xstate(lb+k,j,nelem),k=1,nstate_-lb)
                     else
                        write(5,'(2i5,1p,6(1x,e11.4))') nelem,j,
     &                       (xstate(lb+k,j,nelem),k=1,6)
                     endif
                  enddo
               enddo
            enddo
         endif
!
      endif
!
!     determining the stresses in the nodes for output in frd format
!
      if(nodeflab(3).eq.'S   ') then
         nfield=6
         ndim=6
         call extrapolate(stx,stn,ipkon,inum,kon,lakon,nfield,nk,
     &        ne,mint_,ndim)
      endif
!
!     determining the strains in the nodes for output in frd format
!
      if(nodeflab(4).eq.'E   ') then
         nfield=6
         ndim=6
         call extrapolate(eei,een,ipkon,inum,kon,lakon,nfield,nk,
     &        ne,mint_,ndim)
      endif
!
!     determining the plastic equivalent strain in the nodes 
!     for output in frd format
!
      if(nodeflab(6).eq.'PE  ') then
         nfield=1
         ndim=nstate_
         call extrapolate(xstate,epn,ipkon,inum,kon,lakon,nfield,nk,
     &        ne,mint_,ndim)
      endif
!
!     determining the total energy in the nodes 
!     for output in frd format
!
      if(nodeflab(7).eq.'ENER') then
         nfield=1
         ndim=1
         call extrapolate(ener,enern,ipkon,inum,kon,lakon,nfield,nk,
     &        ne,mint_,ndim)
      endif
!
!     determining the internal state variables in the nodes 
!     for output in frd format
!
      if(nodeflab(8).eq.'SDV ') then
         nfield=nstate_
         ndim=nstate_
         call extrapolate(xstate,xstaten,ipkon,inum,kon,lakon,nfield,nk,
     &        ne,mint_,ndim)
      endif
!
!     if neither stresses nor strains are requested: calculate
!     inum: used in subroutine frd to determine which nodes are
!     active in the model (there is always frd output, e.g. U or
!     NT; default is U and S)
!
      if((nodeflab(3).ne.'S   ').and.(nodeflab(4).ne.'E   ').and.
     &   (nodeflab(6).ne.'PE  ').and.(nodeflab(7).ne.'ENER').and.
     &   (nodeflab(8).ne.'SDV ')) then
!
         do i=1,nk
            inum(i)=0
         enddo
!
         do i=1,ne
!
            if(ipkon(i).lt.0) cycle
            indexe=ipkon(i)
            if(lakon(i)(1:4).eq.'C3D1') then
               nopev=4
            else
               nopev=8
            endif
!
            do j=1,nopev
               inum(kon(indexe+j))=inum(kon(indexe+j))+1
            enddo
         enddo
!
!        determining the midside nodes
!
         do i=1,ne
!
            if(ipkon(i).lt.0) cycle
            indexe=ipkon(i)
            if(lakon(i)(4:4).eq.'2') then
               nope=20
               nopev=8
            elseif(lakon(i)(4:4).eq.'8') then
               nope=8
               nopev=8
            elseif(lakon(i)(4:5).eq.'10') then
               nope=10
               nopev=4
            elseif(lakon(i)(4:4).eq.'4') then
               nope=4
               nopev=4
            elseif(lakon(i)(4:5).eq.'15') then
               nope=15
               nopev=6
            else
               nope=6
               nopev=6
            endif
!
            do j=nopev,nope
               if(inum(kon(indexe+j)).eq.0) inum(kon(indexe+j))=-1
            enddo
         enddo
      endif
!
      return
      end
