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
      subroutine umat_tension_only(amat,iel,iint,kode,elconloc,eloc,eth,
     &        beta,xokl,voj,xkl,vj,ithermal,t1l,dtime,icmd,ielas,mint_,
     &        nstate_,xstateini,xstate,stre,stiff,iorien,pgauss,orab)
!
!     calculates stiffness and stresses for a user defined material
!     law
!
!     icmd=3: calcutates stress at mechanical strain
!     else: calculates stress at mechanical strain and the stiffness
!           matrix
!
!     INPUT:
!
!     amat               material name
!     iel                element number
!     iint               integration point number
!
!     kode               material type (-100-#of constants entered
!                        under *USER MATERIAL): can be used for materials
!                        with varying number of constants
!
!     elconloc(21)       user defined constants defined by the keyword
!                        card *USER MATERIAL (max. 21, actual # =
!                        -kode-100), interpolated for the
!                        actual temperature t1l
!
!     eloc(6)            Lagrange strain tensor (component order:
!                        11,22,33,12,13,23)
!     eth(6)             thermal strain tensor
!     beta(6)            residual stress tensor (the stress entered under
!                        the keyword *INITIAL CONDITIONS,TYPE=STRESS)
!
!     xokl(3,3)          deformation gradient at the start of the increment
!     voj                Jacobian at the start of the increment
!     xkl(3,3)           deformation gradient at the end of the increment
!     vj                 Jacobian at the end of the increment
!
!     ithermal           0: no thermal effects are taken into account
!                        1: thermal effects are taken into account (triggered
!                        by the keyword *INITIAL CONDITIONS,TYPE=TEMPERATURE)
!     t1l                temperature at the end of the increment
!     dtime              time length of the increment
!
!     icmd               not equal to 3: calculate stress and stiffness
!                                        at mechanical strain
!                        3: calculate only stress at mechanical strain
!     ielas              0: no elastic iteration: irreversible effects
!                        are allowed
!                        1: elastic iteration, i.e. no irreversible
!                           deformation allowed
!
!     mint_              max. # of integration points per element in the
!                        model
!     nstate_            max. # of state variables in the model
!
!     xstateini(nstate_,mint_,# of elements)
!                        state variables at the start of the increment
!     xstate(nstate_,mint_,# of elements)
!                        actual calculated value of the
!                        state variables at the end of the increment
!
!     stre(6)            Piola-Kirchhoff stress of the second kind
!                        at the start of the increment
!
!     iorien             number of the local coordinate axis system
!                        in the integration point at stake (takes the value
!                        0 if no local system applies)
!     pgauss(3)          global coordinates of the integration point
!     orab(7,*)          description of all local coordinate systems.
!                        If a local coordinate system applies the global 
!                        tensors can be obtained by premultiplying the local
!                        tensors with skl(3,3). skl is  determined by calling
!                        the subroutine transformatrix: 
!                        call transformatrix(orab(1,iorien),pgauss,skl)
!
!
!     OUTPUT:
!
!     xstate(nstate_,mint_,# of elements)
!                        updated state variables at the end of the increment
!     stre(6)            Piola-Kirchhoff stress of the second kind at the
!                        end of the increment
!     stiff(21):         consistent tangent stiffness matrix in the material
!                        frame of reference at the end of the increment. In
!                        other words: the derivative of the PK2 stress with
!                        respect to the Lagrangian strain tensor. The matrix
!                        is supposed to be symmetric, only the upper half is
!                        to be given in the same order as for a fully
!                        anisotropic elastic material (*ELASTIC,TYPE=ANISO).
!
      implicit none
!
      character*20 amat
!
      integer ithermal,icmd,kode,ielas,iel,iint,nstate_,mint_,i,iorien
!
      integer j,k,l,kflag,three,kn(42),kl(36),nu,m1,m,n,is(3)
!
      real*8 elconloc(21),stiff(21),eloc(6),eth(6),beta(6),stre(6),
     &  vj,t1l,dtime,xkl(3,3),xokl(3,3),voj,pi,
     &  emec(6),xstate(nstate_,mint_,*),xstateini(nstate_,mint_,*),
     &  streo(6),e(6),c2(6),dkl(6),em1(6),em2(6),
     &  em3(6),dijkl(21),dl,dd1,dd2,dd3,s(3),d(6),dd,xi,aijkl(21),
     &  dmde(21),xie(6),bb,cc,xm,xn,tt,z(3),v1,v2,v3,d1,d2,d3,em(6),
     &  eep(21),a(6),pgauss(3),orab(7,*)
!
      data kn /1,1,1,2,2,2,1,3,2,3,3,3,1,4,2,4,3,4,4,4,1,5,2,5,3,5,
     &  4,5,5,5,1,6,2,6,3,6,4,6,5,6,6,6/
!
      data kl /1,2,4,7,11,16,2,3,5,8,12,17,4,5,6,9,13,18,
     &         7,8,9,10,14,19,11,12,13,14,15,20,16,17,18,19,20,21/
!
!     second and fourth rank unit tensor
!
      data dkl /1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
      data dijkl /1.d0,0.d0,1.d0,2*0.d0,1.d0,3*0.d0,0.5d0,4*0.d0,
     &  0.5d0,5*0.d0,0.5d0/
!
!     for tension-only materials no residual stress is allowed
!     thus far
!
      if(beta(1)**2+beta(2)**2+beta(3)**2+beta(4)**2+beta(5)**2+
     &   beta(6)**2.gt.1.d-20) then
         write(*,*) '*ERROR in umat_tension_only: residual stress'
         write(*,*) '       for a tension-only material should be zero'
         stop
      endif
!
      pi=4.d0*datan(1.d0)
      three=3
!
!     determining the mechanical strain
!
      if(ithermal.eq.1) then
         do i=1,6
            emec(i)=eloc(i)-eth(i)
         enddo
      else
         do i=1,6
            emec(i)=eloc(i)
         enddo
      endif
c      write(*,*)
c      write(*,100) (emec(i),i=1,6)
 100  format('emec ',6(1x,d20.13))
!
!     determining the Lagrangian strain tensor
!
      e(1)=emec(1)
      e(2)=emec(2)
      e(3)=emec(3)
      e(4)=emec(4)
      e(5)=emec(5)
      e(6)=emec(6)
c      write(*,101) (e(i),i=1,6)
c 101  format('e ',6(1x,e11.4))
!
!     calculation of the stress (linear orthotropic
!     stress-strain relationship
!
      streo(1)=elconloc(1)*e(1)+elconloc(2)*e(2)+
     &     elconloc(4)*e(3)
      streo(2)=elconloc(2)*e(1)+elconloc(3)*e(2)+
     &     elconloc(5)*e(3)
      streo(3)=elconloc(4)*e(1)+elconloc(5)*e(2)+
     &     elconloc(6)*e(3)
      streo(4)=2.d0*elconloc(7)*e(4)
      streo(5)=2.d0*elconloc(8)*e(5)
      streo(6)=2.d0*elconloc(9)*e(6)
c      write(*,107) (streo(i),i=1,6)
c 107  format('streo ',6(1x,e11.4))
!
!     calculation of the principal stresses
!
      dd=dsqrt((streo(1)-streo(2))**2+4.d0*streo(4)**2)
      s(1)=(streo(1)+streo(2)+dd)/2.d0
      s(2)=(streo(1)+streo(2)-dd)/2.d0
      s(3)=0.d0
c      write(*,507) (s(i),i=1,3)
c 507  format('principal stress ',6(1x,e11.4))
!
      do i=1,6
         em1(i)=0.d0
         em2(i)=0.d0
         em3(i)=0.d0
      enddo
      em3(3)=1.d0
!
!     equal eigenvalues
!
      if(dd.lt.1.d-10) then
         em1(1)=1.d0
         em2(2)=1.d0
      else
!
!        different eigenvalues
!
         if(dabs(streo(1)-s(1)).lt.1.d-10) then
c            write(*,*) streo(1),s(1)
            a(1)=1.d0
            a(2)=streo(4)/(s(1)-streo(2))
            a(3)=0.d0
         else
c            write(*,*) streo(1),s(1)
            a(1)=streo(4)/(s(1)-streo(1))
            a(2)=1.d0
            a(3)=0.d0
         endif
         dd=dsqrt(a(1)*a(1)+a(2)*a(2))
         a(1)=a(1)/dd
         a(2)=a(2)/dd
         em1(1)=a(1)*a(1)
         em1(2)=a(2)*a(2)
         em1(4)=a(1)*a(2)
c      write(*,307) (a(i),i=1,3)
c 307  format('eigenvector 1 ',6(1x,e11.4))
!
         if(dabs(streo(1)-s(2)).lt.1.d-10) then
c            write(*,*) streo(1),s(2)
            a(1)=1.d0
            a(2)=streo(4)/(s(2)-streo(2))
            a(3)=0.d0
         else
c            write(*,*) streo(1),s(2)
            a(1)=streo(4)/(s(2)-streo(1))
            a(2)=1.d0
            a(3)=0.d0
         endif
         dd=dsqrt(a(1)*a(1)+a(2)*a(2))
         a(1)=a(1)/dd
         a(2)=a(2)/dd
         em2(1)=a(1)*a(1)
         em2(2)=a(2)*a(2)
         em2(4)=a(1)*a(2)
c      write(*,407) (a(i),i=1,3)
c 407  format('eigenvector 2 ',6(1x,e11.4))
      endif
!
!     ordering the principal stresses
!
      is(1)=1
      is(2)=2
      is(3)=3
      kflag=2
      call dsort(s,is,three,kflag)
!
!     check whether any principal stress is negative
!
      if((s(1).ge.0.d0).or.(dabs(s(1)).le.0.01*s(3)).or.
     &  (ielas.eq.1)) then
!
!        no wrinkling
! 
c         write(*,*) 'ielas=',ielas,' NO WRINKLING'
         do i=1,6
            stre(i)=streo(i)
         enddo
c      write(*,123) (stre(i),i=1,6)
c 123  format('stre ',6(1x,e11.4))
         if(icmd.ne.3) then
            do i=1,6
               stiff(i)=elconloc(i)
            enddo
            do i=7,9
               stiff(i)=0.d0
            enddo
            stiff(10)=elconloc(7)
            do i=11,14
               stiff(i)=0.d0
            enddo
            stiff(15)=elconloc(8)
            do i=16,20
               stiff(i)=0.d0
            enddo
            stiff(21)=elconloc(9)
c            write(*,122) (stiff(i),i=1,21)
c 122        format('stiff ',6(1x,e11.4))
         endif
      else
!
!        one principal stress value is negative
! 
!        storing the basis matrix corresponding to the negative
!        eigenvalue in em
!
c         write(*,*) 'WRINKLING!'
         if(is(1).eq.1) then
            do i=1,6
               em(i)=em1(i)
            enddo
         elseif(is(1).eq.2) then
            do i=1,6
               em(i)=em2(i)
            enddo
         else
            do i=1,6
               em(i)=em3(i)
            enddo
         endif
c         write(*,109) (em(i),i=1,6)
c 109     format('em ',6(1x,e11.4))
!         
!        calculation of m1:elconloc 
!         (m1: eigenvalue base matrix corresponding to the negative
!              stress
!          elconloc:  linear orthotropic elasticity matrix)
!          
         d(1)=elconloc(1)*em(1)+elconloc(2)*em(2)+elconloc(4)*em(3)
         d(2)=elconloc(2)*em(1)+elconloc(3)*em(2)+elconloc(5)*em(3)
         d(3)=elconloc(4)*em(1)+elconloc(5)*em(2)+elconloc(6)*em(3)
         d(4)=2.d0*elconloc(7)*em(4)
         d(5)=2.d0*elconloc(8)*em(5)
         d(6)=2.d0*elconloc(9)*em(6)
c         write(*,110) (d(i),i=1,6)
c 110     format('d ',6(1x,e11.4))
!
!        calculation of m1:elconloc:m1
!
         dd=em(1)*d(1)+em(2)*d(2)+em(3)*d(3)+
     &      2.d0*(em(4)*d(4)+em(5)*d(5)+em(6)*d(6))
         xi=-s(1)/dd
c         write(*,111) dd,xi
c 111     format('dd,xi ',2(1x,e11.4))
!
!        stress calculation
!
         do i=1,6
            emec(i)=e(i)+xi*em(i)
         enddo
         stre(1)=elconloc(1)*emec(1)+elconloc(2)*emec(2)+
     &        elconloc(4)*emec(3)
         stre(2)=elconloc(2)*emec(1)+elconloc(3)*emec(2)+
     &        elconloc(5)*emec(3)
         stre(3)=elconloc(4)*emec(1)+elconloc(5)*emec(2)+
     &        elconloc(6)*emec(3)
         stre(4)=2.d0*elconloc(7)*emec(4)
         stre(5)=2.d0*elconloc(8)*emec(5)
         stre(6)=2.d0*elconloc(9)*emec(6)
c         write(*,112) (stre(i),i=1,6)
c 112     format('stre ',6(1x,e11.4))
!
!        principal Lagrange strains
!
         z(1)=em1(1)*e(1)+em1(2)*e(2)+em1(3)*e(3)+
     &        2.d0*(em1(4)*e(4)+em1(5)*e(5)+em1(6)*e(6))
         z(2)=em2(1)*e(1)+em2(2)*e(2)+em2(3)*e(3)+
     &        2.d0*(em2(4)*e(4)+em2(5)*e(5)+em2(6)*e(6))
         z(3)=em3(1)*e(1)+em3(2)*e(2)+em3(3)*e(3)+
     &        2.d0*(em3(4)*e(4)+em3(5)*e(5)+em3(6)*e(6))
c         write(*,121) z(1),z(2),z(3)
c 121     format('principal strains ',3(1x,e11.4))
!
         if(icmd.ne.3) then
!
!           derivative of c*c w.r.t. c
!
            aijkl(1)=2.d0*e(1)
            aijkl(2)=0.d0
            aijkl(3)=2.d0*e(2)
            aijkl(4)=0.d0
            aijkl(5)=0.d0
            aijkl(6)=2.d0*e(3)
            aijkl(7)=e(4)
            aijkl(8)=e(4)
            aijkl(9)=0.d0
            aijkl(10)=(e(1)+e(2))/2.d0
            aijkl(11)=e(5)
            aijkl(12)=0.d0
            aijkl(13)=e(5)
            aijkl(14)=e(6)/2.d0
            aijkl(15)=(e(1)+e(3))/2.d0
            aijkl(16)=0.d0
            aijkl(17)=e(6)
            aijkl(18)=e(6)
            aijkl(19)=e(5)/2.d0
            aijkl(20)=e(4)/2.d0
            aijkl(21)=(e(2)+e(3))/2.d0
c            write(*,115) (aijkl(i),i=1,21)
c 115        format('aijkl ',6(1x,e11.4))
!
!           derivative of m1 w.r.t. e
!    
            dl=(z(is(1))-z(is(2)))*(z(is(1))-z(is(3)))
            nu=0
            do j=1,21
               k=kn(nu+1)
               l=kn(nu+2)
               nu=nu+2
               dmde(j)=(aijkl(j)-2.d0*
     &       (z(1)*em1(k)*em1(l)+z(2)*em2(k)*em2(l)+z(3)*em3(k)*em3(l))
     &          -(z(is(2))+z(is(3)))*
     &       (dijkl(j)-(em1(k)*em1(l)+em2(k)*em2(l)+em3(k)*em3(l)))
     &          )/dl
            enddo
c            write(*,116) (dmde(i),i=1,21)
c 116        format('dmde ',6(1x,e11.4))
!
!        derivative of xi w.r.t. e
!
            nu=0
            do m1=1,6
               i=kl(nu+1)
               j=kl(nu+2)
               k=kl(nu+3)
               l=kl(nu+4)
               m=kl(nu+5)
               n=kl(nu+6)
               nu=nu+6
               xie(m1)=((-
     &          (dmde(i)*streo(1)+dmde(j)*streo(2)+dmde(k)*streo(3)+
     &        2.d0*(dmde(l)*streo(4)+dmde(m)*streo(5)+dmde(n)*streo(6)))
     &          -d(m1))*dd
     &          +2.d0*s(1)*(dmde(i)*d(1)+dmde(j)*d(2)+dmde(k)*d(3)+
     &          2.d0*(dmde(l)*d(4)+dmde(m)*d(5)+dmde(n)*d(6))))/dd**2
            enddo
c            write(*,113) (xie(i),i=1,6)
c 113        format('xie ',6(1x,e11.4))
!
!           derivative of e prime with respect to e
!
            nu=0
            do j=1,21
               k=kn(nu+1)
               l=kn(nu+2)
               nu=nu+2
               eep(j)=dijkl(j)
            enddo
c            write(*,117) (eep(i),i=1,21)
c 117        format('eep1 ',6(1x,e11.4))
            nu=0
            do j=1,21
               k=kn(nu+1)
               l=kn(nu+2)
               nu=nu+2
               eep(j)=xie(k)*em(l)
            enddo
c            write(*,118) (eep(i),i=1,21)
c 118        format('eep2 ',6(1x,e11.4))
            nu=0
            do j=1,21
               k=kn(nu+1)
               l=kn(nu+2)
               nu=nu+2
               eep(j)=xi*dmde(j)
            enddo
c            write(*,119) (eep(i),i=1,21)
c 119        format('eep3 ',6(1x,e11.4))
            nu=0
            do j=1,21
               k=kn(nu+1)
               l=kn(nu+2)
               nu=nu+2
               eep(j)=dijkl(j)+xie(k)*em(l)+xi*dmde(j)
            enddo
c            write(*,120) (eep(i),i=1,21)
c 120        format('eep ',6(1x,e11.4))
!
!           stiffness matrix
!
ccc
            do i=1,6
               stiff(i)=elconloc(i)
            enddo
            do i=7,9
               stiff(i)=0.d0
            enddo
            stiff(10)=elconloc(7)
            do i=11,14
               stiff(i)=0.d0
            enddo
            stiff(15)=elconloc(8)
            do i=16,20
               stiff(i)=0.d0
            enddo
            stiff(21)=elconloc(9)
cccc
c            stiff(1)=
c     &        elconloc(1)*eep(1)+elconloc(2)*eep(2)+elconloc(4)*eep(4)
c            stiff(2)=
c     &        elconloc(1)*eep(2)+elconloc(2)*eep(3)+elconloc(4)*eep(5)
c            stiff(3)=
c     &        elconloc(2)*eep(2)+elconloc(3)*eep(3)+elconloc(5)*eep(5)
c            stiff(4)=
c     &        elconloc(1)*eep(4)+elconloc(2)*eep(5)+elconloc(4)*eep(6)
c            stiff(5)=
c     &        elconloc(2)*eep(4)+elconloc(3)*eep(5)+elconloc(5)*eep(6)
c            stiff(6)=
c     &        elconloc(4)*eep(4)+elconloc(5)*eep(5)+elconloc(6)*eep(6)
c            stiff(7)=
c     &        elconloc(1)*eep(7)+elconloc(2)*eep(8)+elconloc(4)*eep(9)
c            stiff(8)=
c     &        elconloc(2)*eep(7)+elconloc(3)*eep(8)+elconloc(5)*eep(9)
c            stiff(9)=
c     &        elconloc(4)*eep(7)+elconloc(5)*eep(8)+elconloc(6)*eep(9)
c            stiff(10)=
c     &        2.d0*elconloc(7)*eep(10)
c            stiff(11)=
c     &       elconloc(1)*eep(11)+elconloc(2)*eep(12)+elconloc(4)*eep(13)
c            stiff(12)=
c     &       elconloc(2)*eep(11)+elconloc(3)*eep(12)+elconloc(5)*eep(13)
c            stiff(13)=
c     &       elconloc(4)*eep(11)+elconloc(5)*eep(12)+elconloc(6)*eep(13)
c            stiff(14)=
c     &        2.d0*elconloc(7)*eep(14)
c            stiff(15)=
c     &        2.d0*elconloc(8)*eep(15)
c            stiff(16)=
c     &       elconloc(1)*eep(16)+elconloc(2)*eep(17)+elconloc(4)*eep(18)
c            stiff(17)=
c     &       elconloc(2)*eep(16)+elconloc(3)*eep(17)+elconloc(5)*eep(18)
c            stiff(18)=
c     &       elconloc(4)*eep(16)+elconloc(5)*eep(17)+elconloc(6)*eep(18)
c            stiff(19)=
c     &        2.d0*elconloc(7)*eep(19)
c            stiff(20)=
c     &        2.d0*elconloc(8)*eep(20)
c            stiff(21)=
c     &        2.d0*elconloc(9)*eep(21)
!
c            write(*,114) (stiff(i),i=1,21)
c 114        format('stiff ',6(1x,e11.4))
         endif
      endif
!
      return
      end


