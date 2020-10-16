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
      subroutine umat_lin_iso_el(amat,iel,iint,kode,elconloc,eloc,eth,
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
      integer ithermal,icmd,kode,ielas,iel,iint,nstate_,mint_,i,iorien
!
      real*8 elconloc(21),stiff(21),eloc(6),eth(6),beta(6),stre(6),
     &  vj,t1l,dtime,xkl(3,3),xokl(3,3),voj,emec(6),pgauss(3),orab(7,*)
!
      real*8 xstate(nstate_,mint_,*),xstateini(nstate_,mint_,*)
!
      real*8 e,un,al,um,am1,am2
!
!     insert here code to calculate the stresses
!
      e=elconloc(1)
      un=elconloc(2)
      al=un*e/(1.d0+un)/(1.d0-2.d0*un)
      um=e/2.d0/(1.d0+un)
      am1=al+2.d0*um
      am2=2.d0*um
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
!      
      stre(1)=am1*emec(1)+al*(emec(2)+emec(3))-beta(1)
      stre(2)=am1*emec(2)+al*(emec(1)+emec(3))-beta(2)
      stre(3)=am1*emec(3)+al*(emec(1)+emec(2))-beta(3)
      stre(4)=am2*emec(4)-beta(4)
      stre(5)=am2*emec(5)-beta(5)
      stre(6)=am2*emec(6)-beta(6)
!
      if(icmd.ne.3) then
!
!        insert here code to calculate the stiffness matrix
!
         stiff(1)=al+2.d0*um
         stiff(2)=al
         stiff(3)=al+2.d0*um
         stiff(4)=al
         stiff(5)=al
         stiff(6)=al+2.d0*um
         stiff(7)=0.d0
         stiff(8)=0.d0
         stiff(9)=0.d0
         stiff(10)=um
         stiff(11)=0.d0
         stiff(12)=0.d0
         stiff(13)=0.d0
         stiff(14)=0.d0
         stiff(15)=um
         stiff(16)=0.d0
         stiff(17)=0.d0
         stiff(18)=0.d0
         stiff(19)=0.d0
         stiff(20)=0.d0
         stiff(21)=um
      endif
!
      return
      end
