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
      subroutine materialdata(elcon,nelcon,rhcon,nrhcon,alcon,nalcon,
     &  imat,amat,iorien,pgauss,orab,ntmat_,elas,alph,rho,i,ithermal,
     &  alzero,mattyp,t0l,t1l,ihyper,istiff,
     &  elconloc,eth,kode,plicon,
     &  nplicon,plkcon,nplkcon,npmat_,
     &  plconloc,mint_,dtime,iel,iint,xstiff,
     &  ncmat_)
!
      implicit none
!
!     determines the material data for element i
!
!     for linear elastic materials:
!       istiff=0 or istiff=1: interpolation of the material data
!                             this includes automatically the
!                             stiffness matrix
!     for nonlinear materials:
!       istiff=0: only interpolation of material data
!       istiff=1: copy the consistent tangent matrix from the field
!                 xstiff and check for zero entries
!
      character*20 amat
!
      integer nelcon(2,*),nrhcon(*),nalcon(2,*),
     &  imat,iorien,ithermal,i,j,k,mattyp,kal(2,6),
     &  kel(4,21),j1,j2,j3,j4,j5,j6,j7,j8,jj,ntmat_,
     &  istiff,nelconst,ihyper,kode,itemp,kin,nelas,
     &  iel,iint,mint_,ncmat_,id,two,seven
!
      integer nplicon(0:ntmat_,*),nplkcon(0:ntmat_,*),npmat_
!
      real*8 elcon(0:ncmat_,ntmat_,*),rhcon(0:1,ntmat_,*),
     &  alcon(0:6,ntmat_,*),eth(6),xstiff(21,mint_,*),
     &  orab(7,*),elas(21),alph(6),alzero(*),rho,t0l,t1l,
     &  skl(3,3),xa(3,3),ya(3,3,3,3),
     &  elconloc(21),emax,pgauss(3)
!
      real*8 plicon(0:2*npmat_,ntmat_,*),plkcon(0:2*npmat_,ntmat_,*),
     &  plconloc(82),dtime
!
      data kal /1,1,2,2,3,3,1,2,1,3,2,3/
!
      data kel /1,1,1,1,1,1,2,2,2,2,2,2,1,1,3,3,2,2,3,3,3,3,3,3,
     &          1,1,1,2,2,2,1,2,3,3,1,2,1,2,1,2,1,1,1,3,2,2,1,3,
     &          3,3,1,3,1,2,1,3,1,3,1,3,1,1,2,3,2,2,2,3,3,3,2,3,
     &          1,2,2,3,1,3,2,3,2,3,2,3/
!
      two=2
      seven=7
!
!     nelconst: # constants read from file
!     nelas: # constants in the local tangent stiffness matrix
!
      nelconst=nelcon(1,imat)
      nelas=nelcon(1,imat)
!
      if(nelconst.lt.0) then
!
!        hyperelastic material
!
         if(nelconst.eq.-1) then
            nelconst=3
         elseif(nelconst.eq.-2) then
            nelconst=3
         elseif(nelconst.eq.-3) then
            nelconst=2
         elseif(nelconst.eq.-4) then
            nelconst=3
         elseif(nelconst.eq.-5) then
            nelconst=6
         elseif(nelconst.eq.-6) then
            nelconst=9
         elseif(nelconst.eq.-7) then
            nelconst=3
         elseif(nelconst.eq.-8) then
            nelconst=7
         elseif(nelconst.eq.-9) then
            nelconst=12
         elseif(nelconst.eq.-10) then
            nelconst=2
         elseif(nelconst.eq.-11) then
            nelconst=4
         elseif(nelconst.eq.-12) then
            nelconst=6
         elseif(nelconst.eq.-13) then
            nelconst=5
         elseif(nelconst.eq.-14) then
            nelconst=6
         elseif(nelconst.eq.-15) then
            nelconst=3
         elseif(nelconst.eq.-16) then
            nelconst=6
         elseif(nelconst.eq.-17) then
            nelconst=9
         elseif(nelconst.eq.-50) then
            nelconst=5
         elseif(nelconst.eq.-51) then
            nelconst=2
         elseif(nelconst.eq.-52) then
            nelconst=4
         elseif(nelconst.le.-100) then
            nelconst=-nelconst-100
         endif
!
         nelas=21
!
      endif
!
!     calculating the density (needed for the mass matrix and
!     gravity or centrifugal loading)
!
      if(ithermal.eq.0) then
         rho=rhcon(1,1,imat)
      else
         call ident2(rhcon(0,1,imat),t1l,nrhcon(imat),two,id)
         if(nrhcon(imat).eq.0) then
            continue
         elseif(nrhcon(imat).eq.1) then
            rho=rhcon(1,1,imat)
         elseif(id.eq.0) then
            rho=rhcon(1,1,imat)
         elseif(id.eq.nrhcon(imat)) then
            rho=rhcon(1,id,imat)
         else
            rho=rhcon(1,id,imat)+
     &           (rhcon(1,id+1,imat)-rhcon(1,id,imat))*
     &           (t1l-rhcon(0,id,imat))/
     &           (rhcon(0,id+1,imat)-rhcon(0,id,imat))
         endif
      endif
!
!     for nonlinear behavior (nonlinear geometric or
!     nonlinear material behavior): copy the stiffness matrix
!     from the last stress calculation
!
      if((istiff.eq.1).and.(ihyper.ne.0)) then
         do j=1,21
            elas(j)=xstiff(j,iint,iel)
         enddo
c         write(*,*) 'stiffness in materialdata'
c         write(*,*) (elas(j),j=1,21)
c         write(*,*)
      else
!cccccccccccccccccccccccccccc
!
!        in case no initial temperatures are defined, the calculation
!        is assumed athermal, and the first available set material
!        constants are used
!
      if(ithermal.eq.0) then
         if(ihyper.ne.1) then
            do k=1,nelconst
               elas(k)=elcon(k,1,imat)
            enddo
         else
            do k=1,nelconst
               elconloc(k)=elcon(k,1,imat)
            enddo
!
            itemp=1
!
            if((kode.lt.-50).and.(kode.gt.-100)) then
               plconloc(1)=0.d0
               plconloc(2)=0.d0
               plconloc(3)=0.d0
               plconloc(81)=nplicon(1,imat)+0.5d0
               plconloc(82)=nplkcon(1,imat)+0.5d0
!
!              isotropic hardening
!
               if(nplicon(1,imat).ne.0) then
                  kin=0
                  call plcopy(plicon,nplicon,plconloc,npmat_,ntmat_,
     &                 imat,itemp,i,kin)
               endif
!
!           kinematic hardening
!
               if(nplkcon(1,imat).ne.0) then
                  kin=1
                  call plcopy(plkcon,nplkcon,plconloc,npmat_,ntmat_,
     &                 imat,itemp,i,kin)
               endif
!
            endif
!
         endif
c         rho=rhcon(1,1,imat)
      else
!
!        calculating the expansion coefficients
!
         call ident2(alcon(0,1,imat),t1l,nalcon(2,imat),seven,id)
         if(nalcon(2,imat).eq.0) then
            do k=1,6
               alph(k)=0.d0
            enddo
            continue
         elseif(nalcon(2,imat).eq.1) then
            do k=1,nalcon(1,imat)
               alph(k)=alcon(k,1,imat)*(t1l-alzero(imat))
            enddo
         elseif(id.eq.0) then
            do k=1,nalcon(1,imat)
               alph(k)=alcon(k,1,imat)*(t1l-alzero(imat))
            enddo
         elseif(id.eq.nalcon(2,imat)) then
            do k=1,nalcon(1,imat)
               alph(k)=alcon(k,id,imat)*(t1l-alzero(imat))
            enddo
         else
            do k=1,nalcon(1,imat)
               alph(k)=(alcon(k,id,imat)+
     &              (alcon(k,id+1,imat)-alcon(k,id,imat))*
     &              (t1l-alcon(0,id,imat))/
     &              (alcon(0,id+1,imat)-alcon(0,id,imat)))
     &              *(t1l-alzero(imat))
            enddo
         endif
!
!        subtracting the initial temperature influence       
!
         call ident2(alcon(0,1,imat),t0l,nalcon(2,imat),seven,id)
         if(nalcon(2,imat).eq.0) then
            continue
         elseif(nalcon(2,imat).eq.1) then
            do k=1,nalcon(1,imat)
               alph(k)=alph(k)-alcon(k,1,imat)*(t0l-alzero(imat))
            enddo
         elseif(id.eq.0) then
            do k=1,nalcon(1,imat)
               alph(k)=alph(k)-alcon(k,1,imat)*(t0l-alzero(imat))
            enddo
         elseif(id.eq.nalcon(2,imat)) then
            do k=1,nalcon(1,imat)
               alph(k)=alph(k)-alcon(k,id,imat)*(t0l-alzero(imat))
            enddo
         else
            do k=1,nalcon(1,imat)
               alph(k)=alph(k)-(alcon(k,id,imat)+
     &              (alcon(k,id+1,imat)-alcon(k,id,imat))*
     &              (t0l-alcon(0,id,imat))/
     &              (alcon(0,id+1,imat)-alcon(0,id,imat)))
     &              *(t0l-alzero(imat))
            enddo
         endif
!
!        for hyperelastic materials: storing the thermal strains
!
         if(ihyper.eq.1) then
            if(nalcon(1,imat).eq.1) then
               do k=1,3
                  eth(k)=alph(1)
               enddo
               do k=4,6
                  eth(k)=0.d0
               enddo
            elseif(nalcon(1,imat).eq.3) then
               do k=1,3
                  eth(k)=alph(k)
               enddo
               do k=4,6
                  eth(k)=0.d0
               enddo
            else
               do k=1,6
                  eth(k)=alph(k)
               enddo
            endif
         endif
!
!        the expansion coefficient per element is defined as the
!        mean thermal strain divided by the mean temperature
!        difference
!
         do k=1,nalcon(1,imat)
            if(dabs(t1l-t0l).lt.1.d-4) then
               alph(k)=0.d0
            else
               alph(k)=alph(k)/(t1l-t0l)
            endif
         enddo
!
!        calculating the hardening coefficients
!
!        for the calculation of the stresses, the whole curve
!        has to be stored:
!          plconloc(2*k-1), k=1...10: equivalent plastic strain values
!          plconloc(2*k),k=1...10:    corresponding k-values
!          plconloc(19+2*k),k=1...10: equivalent plastic strain values
!          plconloc(20+2*k),k=1...10: corresponding h-values
!
!        initialization
!
         if((kode.lt.-50).and.(kode.gt.-100)) then
            plconloc(1)=0.d0
            plconloc(2)=0.d0
            plconloc(3)=0.d0
            plconloc(81)=nplicon(1,imat)+0.5d0
            plconloc(82)=nplkcon(1,imat)+0.5d0
!
!        isotropic hardening
!
            if(nplicon(1,imat).ne.0) then
!
               if(nplicon(0,imat).eq.1) then
                  id=-1
               else
                  call ident2(plicon(0,1,imat),t1l,nplicon(0,imat),
     &                        2*npmat_+1,id)
               endif
!
               if(nplicon(0,imat).eq.0) then
                  continue
               elseif((nplicon(0,imat).eq.1).or.(id.eq.0).or.
     &                 (id.eq.nplicon(0,imat))) then
                  if(id.le.0) then
                     itemp=1
                  else
                     itemp=id
                  endif
                  kin=0
                  call plcopy(plicon,nplicon,plconloc,npmat_,ntmat_,
     &                 imat,itemp,i,kin)
                  if((id.eq.0).or.(id.eq.nplicon(0,imat))) then
                  endif
               else
                  kin=0
                  call plmix(plicon,nplicon,plconloc,npmat_,ntmat_,
     &                 imat,id+1,t1l,i,kin)
               endif
            endif
!
!        kinematic hardening
!
            if(nplkcon(1,imat).ne.0) then
!
               if(nplkcon(0,imat).eq.1) then
                  id=-1
               else
                  call ident2(plkcon(0,1,imat),t1l,nplkcon(0,imat),
     &                        2*npmat_+1,id)
               endif
!
               if(nplkcon(0,imat).eq.0) then
                  continue
               elseif((nplkcon(0,imat).eq.1).or.(id.eq.0).or.
     &                 (id.eq.nplkcon(0,imat))) then
                  if(id.le.0)then
                     itemp=1
                  else
                     itemp=id
                  endif
                  kin=1
                  call plcopy(plkcon,nplkcon,plconloc,npmat_,ntmat_,
     &                 imat,itemp,i,kin)
                  if((id.eq.0).or.(id.eq.nplkcon(0,imat))) then
                  endif
               else
                  kin=1
                  call plmix(plkcon,nplkcon,plconloc,npmat_,ntmat_,
     &                 imat,id+1,t1l,i,kin)
               endif
            endif
         endif
!
!        calculating the elastic constants
!
         call ident2(elcon(0,1,imat),t1l,nelcon(2,imat),ncmat_+1,id)
         if(nelcon(2,imat).eq.0) then
            continue
         elseif(nelcon(2,imat).eq.1) then
            if(ihyper.ne.1) then
               do k=1,nelconst
                  elas(k)=elcon(k,1,imat)
               enddo
            else
               do k=1,nelconst
                  elconloc(k)=elcon(k,1,imat)
               enddo
            endif
         elseif(id.eq.0) then
            if(ihyper.ne.1) then
               do k=1,nelconst
                  elas(k)=elcon(k,1,imat)
               enddo
            else
               do k=1,nelconst
                  elconloc(k)=elcon(k,1,imat)
               enddo
            endif
         elseif(id.eq.nelcon(2,imat)) then
            if(ihyper.ne.1) then
               do k=1,nelconst
                  elas(k)=elcon(k,id,imat)
               enddo
            else
               do k=1,nelconst
                  elconloc(k)=elcon(k,id,imat)
               enddo
            endif
         else
            if(ihyper.ne.1) then
               do k=1,nelconst
                  elas(k)=elcon(k,id,imat)+
     &                 (elcon(k,id+1,imat)-elcon(k,id,imat))*
     &                 (t1l-elcon(0,id,imat))/
     &                 (elcon(0,id+1,imat)-elcon(0,id,imat))
               enddo
            else
               do k=1,nelconst
                  elconloc(k)=elcon(k,id,imat)+
     &                 (elcon(k,id+1,imat)-elcon(k,id,imat))*
     &                 (t1l-elcon(0,id,imat))/
     &                 (elcon(0,id+1,imat)-elcon(0,id,imat))
               enddo
            endif
         endif
      endif
!ccccccccccccccccccc
      endif
!
!     check whether an anisotropic type can be reduced to an
!     orthotropic type (especially for hyperelastic materials, which
!     are considered anisotropic from the start)
!
!     not applicable to hyperelastic materials when called by
!     subroutine results (istiff=0)
!
      if((ihyper.ne.0).and.(istiff.ne.1)) return
!
      if(nelas.eq.21) then
         emax=0.
         do j=1,21
            emax=max(emax,dabs(elas(j)))
         enddo
         do j=7,9
            if(dabs(elas(j)).gt.emax*1.d-10) then
               emax=-1.d0
               exit
            endif
         enddo
         if(emax.ge.0.d0) then
            do j=11,14
               if(dabs(elas(j)).gt.emax*1.d-10) then
                  emax=-1.d0
                  exit
               endif
            enddo
         endif
         if(emax.ge.0.d0) then
            do j=16,20
               if(dabs(elas(j)).gt.emax*1.d-10) then
                  emax=-1.d0
                  exit
               endif
            enddo
         endif
         if(emax.ge.0.d0) then
            nelas=9
            elas(7)=elas(10)
            elas(8)=elas(15)
            elas(9)=elas(21)
         endif
      endif
!
!     determining the type: isotropic, orthotropic or anisotropic
!
      if((nelas.le.2).and.(nalcon(1,imat).le.1)) then
         mattyp=1
      elseif((nelas.le.9).and.(nalcon(1,imat).le.3)) then
         mattyp=2
         if(nelas.eq.2) then
            elas(7)=elas(1)/(2.d0*(1.d0+elas(2)))
            elas(4)=2.d0*elas(7)*elas(2)/(1.d0-2.d0*elas(2))
            elas(1)=elas(4)+2.d0*elas(7)
            elas(2)=elas(4)
            elas(3)=elas(1)
            elas(5)=elas(4)
            elas(6)=elas(1)
            elas(8)=elas(7)
            elas(9)=elas(7)
         elseif(nalcon(1,imat).eq.1) then
            alph(2)=alph(1)
            alph(3)=alph(1)
         endif
      else
         mattyp=3
         if(nelas.eq.2) then
            elas(10)=elas(1)/(2.d0*(1.d0+elas(2)))
            elas(4)=2.d0*elas(7)*elas(2)/(1.d0-2.d0*elas(2))
            elas(1)=elas(4)+2.d0*elas(7)
            elas(2)=elas(4)
            elas(3)=elas(1)
            elas(5)=elas(4)
            elas(6)=elas(1)
            do j=7,9
               elas(j)=0.d0
            enddo
            do j=11,14
               elas(j)=0.d0
            enddo
            elas(15)=elas(10)
            do j=16,20
               elas(j)=0.d0
            enddo
            elas(21)=elas(10)
         elseif(nelas.eq.9) then
            elas(21)=elas(9)
            elas(15)=elas(8)
            elas(10)=elas(7)
            do j=7,9
               elas(j)=0.d0
            enddo
            do j=11,14
               elas(j)=0.d0
            enddo
            do j=16,20
               elas(j)=0.d0
            enddo
         elseif(nalcon(1,imat).eq.1) then
            alph(2)=alph(1)
            alph(3)=alph(1)
            do j=4,6
               alph(j)=0.d0
            enddo
         elseif(nalcon(1,imat).eq.3) then
            do j=4,6
               alph(j)=0.d0
            enddo
         endif
      endif
!
!     transformation due to special orientation
!     only allowed for linear elastic materials
!
      if((iorien.eq.0).or.(mattyp.eq.1).or.(ihyper.ne.0)) return
!
!     calculating the transformation matrix
!
      call transformatrix(orab(1,iorien),pgauss,skl)
!
!     transforming the elastic coefficients
!
      if(mattyp.eq.2) then
         call orthotropic(elas,ya)
      else
         call anisotropic(elas,ya)
      endif
!
      do jj=1,21
         j1=kel(1,jj)
         j2=kel(2,jj)
         j3=kel(3,jj)
         j4=kel(4,jj)
         elas(jj)=0.d0
         do j5=1,3
            do j6=1,3
               do j7=1,3
                  do j8=1,3
                     elas(jj)=elas(jj)+ya(j5,j6,j7,j8)*
     &                    skl(j1,j5)*skl(j2,j6)*skl(j3,j7)*skl(j4,j8)
                  enddo
               enddo
            enddo
         enddo
      enddo
!
!     modifying the thermal constants
!
      if(ithermal.eq.1) then
!
!        transforming the expansion coefficients
!         
         if(mattyp.eq.2) then
            do j=4,6
               alph(j)=0.d0
            enddo
         endif
!
         xa(1,1)=alph(1)
         xa(1,2)=alph(4)
         xa(1,3)=alph(5)
         xa(2,1)=alph(4)
         xa(2,2)=alph(2)
         xa(2,3)=alph(6)
         xa(3,1)=alph(5)
         xa(3,2)=alph(6)
         xa(3,3)=alph(3)
!
         do jj=1,6
            alph(jj)=0.d0
            j1=kal(1,jj)
            j2=kal(2,jj)
            do j3=1,3
               do j4=1,3
                  alph(jj)=alph(jj)+
     &                 xa(j3,j4)*skl(j1,j3)*skl(j2,j4)
               enddo
            enddo
         enddo
      endif
!
      mattyp=3
!      ielorien(i)=0   ! to check!
!
      return
      end
