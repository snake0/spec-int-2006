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
      subroutine umat_ideal_gas(amat,iel,iint,kode,elconloc,eloc,eth,
     &        beta,xokl,voj,xkl,vj,ithermal,t1l,dtime,icmd,ielas,mint_,
     &        nstate_,xstateini,xstate,stre,stiff,iorien,pgauss,orab)
!
!     calculates stiffness and stresses for an ideal gas
!
!     constants:
!        (1): density at the start of the finite element calculation
!             (not temperature dependent!)
!        (2): specific gas constant (not temperature dependent)
!
!     the temperature must be given in Kelvin
!
!     some values for the specific gas constant, in J/(kg K) =
!     m**2/(s**2*K)  (Ref: Dubbel)
!
!     Ammoniak       (NH3)     488.18
!     Acetylen       (C2H2)    319.60
!     Äthan          (C2H6)    276.74
!     Äthylen        (C2H4)    296.65
!     Argon          (Ar)      208.20
!     Helium         (He)     2079.01
!     Kohlendioxid   (CO2)     188.88
!     Kohlenoxid     (CO)      296.85
!     Luft                     287.04
!     Methan         (CH4)     518.67
!     Sauerstof      (O2)      259.88
!     Schwefeldioxid (SO2)     129.84
!     Stickstoff     (N2)      296.75
!     Wasserdampf    (H2O)     461.50
!     Wasserstoff    (H2)     4124.19
!
!     icmd=3: calculates stress at mechanical strain
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
!                        3: calculate only stress
!     ielas              0: no elastic iteration: irreversible effects
!                        are allowed
!                        1: elastic iteration, i.e. no irreversible
!                           deformation allowed
!
!     xstateini(nstate_,mint_,# of elements)
!                        state variables at the start of the increment
!     xstate(nstate_,mint_,# of elements)
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
      integer ithermal,icmd,kode,ielas,iel,iint,nstate_,mint_,kk(84),
     &  i,k,l,m,n,nt,iorien
!
      real*8 elconloc(21),stiff(21),eloc(6),eth(6),beta(6),stre(6),
     &  vj,t1l,dtime,xkl(3,3),xokl(3,3),voj,c(6),vj2,ci(3,3),
     &  x,pgauss(3),orab(7,*)
!
      real*8 xstate(nstate_,mint_,*),xstateini(nstate_,mint_,*)
!
      data kk /1,1,1,1,1,1,2,2,2,2,2,2,1,1,3,3,2,2,3,3,3,3,3,3,
     &  1,1,1,2,2,2,1,2,3,3,1,2,1,2,1,2,1,1,1,3,2,2,1,3,3,3,1,3,
     &  1,2,1,3,1,3,1,3,1,1,2,3,2,2,2,3,3,3,2,3,1,2,2,3,1,3,2,3,
     &  2,3,2,3/
!
!     right Cauchy-Green tensor (eloc contains the Lagrange strain)
!
      c(1)=2.d0*eloc(1)+1.d0
      c(2)=2.d0*eloc(2)+1.d0
      c(3)=2.d0*eloc(3)+1.d0
      c(4)=2.d0*eloc(4)
      c(5)=2.d0*eloc(5)
      c(6)=2.d0*eloc(6)
      if((iel.eq.1).and.(iint.eq.1)) then
      write(*,*) 'Lagrange strain'
      write(*,'(6(1x,e11.4))') (eloc(i),i=1,6)
      endif
!
!     inversion of the right Cauchy-Green tensor
!
      vj2=vj*vj
      ci(1,1)=(c(2)*c(3)-c(6)*c(6))/vj2
      ci(2,2)=(c(1)*c(3)-c(5)*c(5))/vj2
      ci(3,3)=(c(1)*c(2)-c(4)*c(4))/vj2
      ci(1,2)=(c(5)*c(6)-c(4)*c(3))/vj2
      ci(1,3)=(c(4)*c(6)-c(2)*c(5))/vj2
      ci(2,3)=(c(4)*c(5)-c(1)*c(6))/vj2
      ci(2,1)=ci(1,2)
      ci(3,1)=ci(1,3)
      ci(3,2)=ci(2,3)
      if((iel.eq.1).and.(iint.eq.1)) then
      write(*,*) 'inverse Cauchy-Green'
      write(*,'(6(1x,e11.4))') ((ci(i,k),i=1,k),k=1,3)
      endif
!
      x=elconloc(1)*elconloc(2)*t1l
!
      stre(1)=-x*ci(1,1)
      stre(2)=-x*ci(2,2)
      stre(3)=-x*ci(3,3)
      stre(4)=-x*ci(1,2)
      stre(5)=-x*ci(1,3)
      stre(6)=-x*ci(2,3)
      if((iel.eq.1).and.(iint.eq.1)) then
      write(*,*) 'PK2'
      write(*,'(6(1x,e11.4))') (stre(i),i=1,6)
      endif
!
      if(icmd.ne.3) then
!
!        calculation of the stiffness matrix
!
         nt=0
         do i=1,21
            k=kk(nt+1)
            l=kk(nt+2)
            m=kk(nt+3)
            n=kk(nt+4)
            nt=nt+4
            stiff(i)=x*(ci(k,m)*ci(l,n)+ci(k,n)*ci(l,m))
c     &         +x*ci(k,l)*ci(m,n)/100.d0
         enddo
      if((iel.eq.1).and.(iint.eq.1)) then
      write(*,*) 'stiffness'
      write(*,'(6(1x,e11.4))') (stiff(i),i=1,21)
      endif
c         stiff(1)=2.d0*x
c         stiff(2)=0.d0
c         stiff(3)=2.d0*x
c         stiff(4)=0.d0
c         stiff(5)=0.d0
c         stiff(6)=2.d0*x
c         stiff(7)=0.d0
c         stiff(8)=0.d0
c         stiff(9)=0.d0
c         stiff(10)=x
c         stiff(11)=0.d0
c         stiff(12)=0.d0
c         stiff(13)=0.d0
c         stiff(14)=0.d0
c         stiff(15)=x
c         stiff(16)=0.d0
c         stiff(17)=0.d0
c         stiff(18)=0.d0
c         stiff(19)=0.d0
c         stiff(20)=0.d0
c         stiff(21)=x
!
      endif
!
      return
      end
