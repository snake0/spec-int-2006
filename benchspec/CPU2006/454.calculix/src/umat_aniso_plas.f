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
      subroutine umat_aniso_plas(amat,iel,iint,kode,elconloc,eloc,
     &        eth,beta,xokl,voj,xkl,vj,ithermal,t1l,dtime,icmd,ielas,
     &        mint_,nstate_,xstateini,xstate,stre,stiff,iorien,pgauss,
     &        orab)
!
!     calculates stiffness and stresses for an elastically orthotropic
!     material with isotropic yield surface and isotropic hardening
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
      logical creep
!
      character*20 amat
!
      integer ithermal,icmd,kode,ielas,iel,iint,nstate_,mint_,iorien
!
      integer i,j,k,l,ipiv(6),info,neq,lda,ldb,j1,j2,j3,j4,j5,j6,j7,j8,
     &  nrhs,iplas,kel(4,21)
!
      real*8 ep0(6),al10,al20(6),eeq,ep(6),al1,b,Pn(6),QSn(6),
     &  al2(6),dg,ddg,ca,cn,c(21),r0,x(21),cm1(21),h1,h2,
     &  q1,q2(6),stri(6),htri,sg(6),r(13),au1(21),au2(21),
     &  ee(6),dd,gl(6,6),gr(6,6),c0,c1,c2,c3,c4,c5,c6,sgold(6),
     &  skl(3,3),gcreep,gm1,ya(3,3,3,3),d1,d2,dsg,detc,strinv
!
      real*8 elconloc(21),stiff(21),eloc(6),eth(6),beta(6),stre(6),
     &  vj,t1l,dtime,xkl(3,3),xokl(3,3),voj,pgauss(3),orab(7,*)
!
      real*8 xstate(nstate_,mint_,*),xstateini(nstate_,mint_,*)
!
      data kel /1,1,1,1,1,1,2,2,2,2,2,2,1,1,3,3,2,2,3,3,3,3,3,3,
     &          1,1,1,2,2,2,1,2,3,3,1,2,1,2,1,2,1,1,1,3,2,2,1,3,
     &          3,3,1,3,1,2,1,3,1,3,1,3,1,1,2,3,2,2,2,3,3,3,2,3,
     &          1,2,2,3,1,3,2,3,2,3,2,3/
!
      c0=dsqrt(2.d0/3.d0)
      c1=2.d0/3.d0
      c2=-1.d0/3.d0
!
!     elastic constants
!
      if(iorien.gt.0) then
!
         call transformatrix(orab(1,iorien),pgauss,skl)
!
         call orthotropic(elconloc,ya)
!
         do j=1,21
            j1=kel(1,j)
            j2=kel(2,j)
            j3=kel(3,j)
            j4=kel(4,j)
            c(j)=0.d0
            do j5=1,3
               do j6=1,3
                  do j7=1,3
                     do j8=1,3
                        c(j)=c(j)+ya(j5,j6,j7,j8)*
     &                       skl(j1,j5)*skl(j2,j6)*skl(j3,j7)*skl(j4,j8)
                     enddo
                  enddo
               enddo
            enddo
         enddo
!
      else
         do i=1,9
            c(i)=elconloc(i)
         enddo
      endif
!
!     state variables
!
!     equivalent plastic strain
!
      eeq=xstateini(1,iint,iel)
!
!     plastic strain
!
      do i=1,6
         ep0(i)=xstateini(1+i,iint,iel)
      enddo
!
!     isotropic hardening variable
!
      al10=xstateini(8,iint,iel)
!
!     kinematic hardening variable
!
      do i=1,6
         al20(i)=xstateini(8+i,iint,iel)
      enddo
!
      if((iint.eq.1).and.(iel.eq.1)) then
      write(*,*) 'element, int.point,kstep,kinc ',iel,iint
      endif
!
!     elastic strains
!
      do i=1,6
         ee(i)=eloc(i)-ep0(i)
      enddo
      if(ithermal.ne.0) then
         do i=1,6
            ee(i)=ee(i)-eth(i)
         enddo
      endif
      if((iint.eq.1).and.(iel.eq.1)) then
      write(*,*) 'eloc ',(eloc(i),i=1,6)
      write(*,*) 'ep0 ',(ep0(i),i=1,6)
      write(*,*) 'ee ',(ee(i),i=1,6)
      endif
!
!     (visco)plastic constants
!
      r0=elconloc(10)
      d1=elconloc(11)
      d2=elconloc(12)
      ca=c0/(elconloc(13)*dtime)
      cn=elconloc(14)
!
      if(ca.lt.0.d0) then
         creep=.false.
      else
         creep=.true.
      endif
!
      h1=d1
      h2=2.d0*d2/3.d0
!
!     stress state variables q1 and q2
!
      q1=-d1*al10
      do i=1,6
         q2(i)=-d2*al20(i)
      enddo
      if((iint.eq.1).and.(iel.eq.1)) then
      write(*,200) q1
 200  format('q10 ',/,(6(1x,e11.4)))
      write(*,201) (q2(i),i=1,6)
 201  format('q20 ',/,(6(1x,e11.4)))
      endif
!
!     global trial stress tensor
!
      if(iorien.gt.0) then
         stri(1)=c(1)*ee(1)+c(2)*ee(2)+c(4)*ee(3)+
     &     2.d0*(c(7)*ee(4)+c(11)*ee(5)+c(16)*ee(6))
     &     +beta(1)
         stri(2)=c(2)*ee(1)+c(3)*ee(2)+c(5)*ee(3)+
     &     2.d0*(c(8)*ee(4)+c(12)*ee(5)+c(17)*ee(6))
     &     +beta(2)
         stri(3)=c(4)*ee(1)+c(5)*ee(2)+c(6)*ee(3)+
     &     2.d0*(c(9)*ee(4)+c(13)*ee(5)+c(18)*ee(6))
     &     +beta(3)
         stri(4)=c(7)*ee(1)+c(8)*ee(2)+c(9)*ee(3)+
     &     2.d0*(c(10)*ee(4)+c(14)*ee(5)+c(19)*ee(6))
     &     +beta(4)
         stri(5)=c(11)*ee(1)+c(12)*ee(2)+c(13)*ee(3)+
     &     2.d0*(c(14)*ee(4)+c(15)*ee(5)+c(20)*ee(6))
     &     +beta(5)
         stri(6)=c(16)*ee(1)+c(17)*ee(2)+c(18)*ee(3)+
     &     2.d0*(c(19)*ee(4)+c(20)*ee(5)+c(21)*ee(6))
     &     +beta(6)
      else
         stri(1)=c(1)*ee(1)+c(2)*ee(2)+c(4)*ee(3)+beta(1)
         stri(2)=c(2)*ee(1)+c(3)*ee(2)+c(5)*ee(3)+beta(1)
         stri(3)=c(4)*ee(1)+c(5)*ee(2)+c(6)*ee(3)+beta(1)
         stri(4)=2.d0*c(7)*ee(4)+beta(4)
         stri(5)=2.d0*c(8)*ee(5)+beta(5)
         stri(6)=2.d0*c(9)*ee(6)+beta(6)
      endif
      if((iint.eq.1).and.(iel.eq.1)) then
      write(*,*) 'stri ',(stri(i),i=1,6)
      endif
!
!     stress radius (only deviatoric part of stress enters)
!
c      do i=1,6
c         sgold(i)=0.d0
c      enddo
      strinv=(stri(1)+stri(2)+stri(3))/3.d0
      do i=1,3
         sg(i)=stri(i)-strinv+q2(i)
      enddo
      do i=4,6
         sg(i)=stri(i)+q2(i)
      enddo
      dsg=dsqrt(sg(1)*sg(1)+sg(2)*sg(2)+sg(3)*sg(3)+
     &       2.d0*(sg(4)*sg(4)+sg(5)*sg(5)+sg(6)*sg(6)))      
!
!     evaluation of the yield surface
!
      htri=dsg+c0*(q1-r0)
      if((iint.eq.1).and.(iel.eq.1)) then
         write(*,*) 'htri ',htri
      endif
!
!     check whether plasticity occurs
!
      if(htri.gt.0.d0) then
         iplas=1
      else
         iplas=0
      endif
!
      if((iplas.eq.0).or.(ielas.eq.1)) then
!
!        elastic stress
!
         do i=1,6
            stre(i)=stri(i)
         enddo
      if((iint.eq.1).and.(iel.eq.1)) then
         write(*,*) ' stress '
         write(*,'(6(1x,e11.4))') (stre(i),i=1,6)
      endif
!
!        elastic stiffness
!
         if(icmd.ne.3) then
            if(iorien.gt.0) then
               do i=1,21
                  stiff(i)=c(i)
               enddo
            else
               stiff(1)=c(1)
               stiff(2)=c(2)
               stiff(3)=c(3)
               stiff(4)=c(4)
               stiff(5)=c(5)
               stiff(6)=c(6)
               stiff(7)=0.d0
               stiff(8)=0.d0
               stiff(9)=0.d0
               stiff(10)=c(7)
               stiff(11)=0.d0
               stiff(12)=0.d0
               stiff(13)=0.d0
               stiff(14)=0.d0
               stiff(15)=c(8)
               stiff(16)=0.d0
               stiff(17)=0.d0
               stiff(18)=0.d0
               stiff(19)=0.d0
               stiff(20)=0.d0
               stiff(21)=c(9)
            endif
            if((iint.eq.1).and.(iel.eq.1)) then
               write(*,*) 'stiffness '
               write(*,'(6(1x,e11.4))') (stiff(i),i=1,21)
            endif
         endif
!
         return
      endif
!
!     plastic deformation
!
      neq=6
      nrhs=1
      lda=6
      ldb=6
!
!     initializing the state variables
!
      do i=1,6
         ep(i)=ep0(i)
      enddo
      al1=al10
      do i=1,6
         al2(i)=al20(i)
      enddo
      dg=0.d0
      ddg=0.d0
!
!           determining the inverse of c
!
      if(iorien.gt.0) then
!     
!           solve gl:C=gr
!
         gl(1,1)=c(1) 
         gl(1,2)=c(2) 
         gl(2,2)=c(3) 
         gl(1,3)=c(4) 
         gl(2,3)=c(5) 
         gl(3,3)=c(6) 
         gl(1,4)=c(7) 
         gl(2,4)=c(8) 
         gl(3,4)=c(9) 
         gl(4,4)=c(10)
         gl(1,5)=c(11)
         gl(2,5)=c(12)
         gl(3,5)=c(13)
         gl(4,5)=c(14)
         gl(5,5)=c(15)
         gl(1,6)=c(16)
         gl(2,6)=c(17)
         gl(3,6)=c(18)
         gl(4,6)=c(19)
         gl(5,6)=c(20)
         gl(6,6)=c(21)
         do i=1,6
            do j=1,i-1
               gl(i,j)=gl(j,i)
            enddo
         enddo
         do i=1,6
            do j=4,6
               gl(i,j)=2.d0*gl(i,j)
            enddo
         enddo
         do i=1,6
            do j=1,6
               gr(i,j)=0.d0
            enddo
            if(i.le.3) then
               gr(i,i)=1.d0
            else
               gr(i,i)=0.5d0
            endif
         enddo
         nrhs=6
         call dgesv(neq,nrhs,gl,lda,ipiv,gr,ldb,info)
         if(info.ne.0) then
            write(*,*) '*ERROR in sc.f: linear equation solver'
            write(*,*) '       exited with error: info = ',info
            stop
         endif
         nrhs=1
         cm1(1)=gr(1,1)
         cm1(2)=gr(1,2)
         cm1(3)=gr(2,2)
         cm1(4)=gr(1,3)
         cm1(5)=gr(2,3)
         cm1(6)=gr(3,3)
         cm1(7)=gr(1,4)
         cm1(8)=gr(2,4)
         cm1(9)=gr(3,4)
         cm1(10)=gr(4,4)
         cm1(11)=gr(1,5)
         cm1(12)=gr(2,5)
         cm1(13)=gr(3,5)
         cm1(14)=gr(4,5)
         cm1(15)=gr(5,5)
         cm1(16)=gr(1,6)
         cm1(17)=gr(2,6)
         cm1(18)=gr(3,6)
         cm1(19)=gr(4,6)
         cm1(20)=gr(5,6)
         cm1(21)=gr(6,6)
      else
         detc=c(1)*(c(3)*c(6)-c(5)*c(5))-
     &        c(2)*(c(2)*c(6)-c(4)*c(5))+
     &        c(4)*(c(2)*c(5)-c(4)*c(3))
         cm1(1)=(c(3)*c(6)-c(5)*c(5))/detc
         cm1(2)=(c(5)*c(4)-c(2)*c(6))/detc
         cm1(3)=(c(1)*c(6)-c(4)*c(4))/detc
         cm1(4)=(c(2)*c(5)-c(3)*c(4))/detc
         cm1(5)=(c(2)*c(4)-c(1)*c(5))/detc
         cm1(6)=(c(1)*c(3)-c(2)*c(2))/detc
         cm1(7)=1.d0/(4.d0*c(7))
         cm1(8)=1.d0/(4.d0*c(8))
         cm1(9)=1.d0/(4.d0*c(9))
      endif
!
!     loop
!
      if((iint.eq.1).and.(iel.eq.1)) then
         write(*,202) dg
 202     format('dg ',/,(6(1x,e11.4)))
      endif
      do
!
!        elastic strains
!
         do i=1,6
            ee(i)=eloc(i)-ep(i)
         enddo
         if(ithermal.ne.0) then
            do i=1,6
               ee(i)=ee(i)-eth(i)
            enddo
         endif
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,605) (eloc(i),i=1,6)
 605        format('eloc ',/,(6(1x,e11.4)))
         endif
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,606) (ep(i),i=1,6)
 606        format('ep ',/,(6(1x,e11.4)))
         endif
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,607) (ee(i),i=1,6)
 607        format('ee ',/,(6(1x,e11.4)))
         endif
!
!        stress state variables q1 and q2
!
         q1=-d1*al1
         do i=1,6
            q2(i)=-d2*al2(i)
         enddo
!     
!        global trial stress tensor
!     
         if(iorien.gt.0) then
            stri(1)=c(1)*ee(1)+c(2)*ee(2)+c(4)*ee(3)+
     &           2.d0*(c(7)*ee(4)+c(11)*ee(5)+c(16)*ee(6))
     &           +beta(1)
            stri(2)=c(2)*ee(1)+c(3)*ee(2)+c(5)*ee(3)+
     &           2.d0*(c(8)*ee(4)+c(12)*ee(5)+c(17)*ee(6))
     &           +beta(2)
            stri(3)=c(4)*ee(1)+c(5)*ee(2)+c(6)*ee(3)+
     &           2.d0*(c(9)*ee(4)+c(13)*ee(5)+c(18)*ee(6))
     &           +beta(3)
            stri(4)=c(7)*ee(1)+c(8)*ee(2)+c(9)*ee(3)+
     &           2.d0*(c(10)*ee(4)+c(14)*ee(5)+c(19)*ee(6))
     &           +beta(4)
            stri(5)=c(11)*ee(1)+c(12)*ee(2)+c(13)*ee(3)+
     &           2.d0*(c(14)*ee(4)+c(15)*ee(5)+c(20)*ee(6))
     &           +beta(5)
            stri(6)=c(16)*ee(1)+c(17)*ee(2)+c(18)*ee(3)+
     &           2.d0*(c(19)*ee(4)+c(20)*ee(5)+c(21)*ee(6))
     &           +beta(6)
         else
            stri(1)=c(1)*ee(1)+c(2)*ee(2)+c(4)*ee(3)+beta(1)
            stri(2)=c(2)*ee(1)+c(3)*ee(2)+c(5)*ee(3)+beta(1)
            stri(3)=c(4)*ee(1)+c(5)*ee(2)+c(6)*ee(3)+beta(1)
            stri(4)=2.d0*c(7)*ee(4)+beta(4)
            stri(5)=2.d0*c(8)*ee(5)+beta(5)
            stri(6)=2.d0*c(9)*ee(6)+beta(6)
         endif
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,805) (stri(i),i=1,6)
 805        format('stri ',/,(6(1x,e11.4)))
         endif
!     
!        stress radius (only deviatoric part of stress enters)
!     
         strinv=(stri(1)+stri(2)+stri(3))/3.d0
         do i=1,3
            sg(i)=stri(i)-strinv+q2(i)
         enddo
         do i=4,6
            sg(i)=stri(i)+q2(i)
         enddo
         dsg=dsqrt(sg(1)*sg(1)+sg(2)*sg(2)+sg(3)*sg(3)+
     &        2.d0*(sg(4)*sg(4)+sg(5)*sg(5)+sg(6)*sg(6)))      
!     
!        evaluation of the yield surface
!
      if((iint.eq.1).and.(iel.eq.1)) then
         write(*,611) dsg,q1,r0,c0
 611     format('dsg,q1,r0,c0,al1,d1 ',/,(6(1x,e11.4)))
         write(*,612) (q2(i),i=1,6)
 612     format('q2 ',/,(6(1x,e11.4)))
      endif
         if(creep) then
            htri=dsg+c0*(q1-r0-(ca*dg)**(1.d0/cn))
         else
            htri=dsg+c0*(q1-r0)
         endif
!
         do i=1,6
            sg(i)=sg(i)/dsg
         enddo
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,905) (sg(i),i=1,6)
 905        format('sg ',/,(6(1x,e11.4)))
         endif
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,203) htri
 203        format('htri ',/,(6(1x,e11.4)))
         endif
!
!        determining the residual matrix
!
         do i=1,6  
            r(i)=ep0(i)-ep(i)+dg*sg(i)
         enddo
         r(7)=al10-al1+dg*c0
         do i=1,6
            r(7+i)=al20(i)-al2(i)+dg*sg(i)
         enddo
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,205) (r(i),i=1,13)
 205        format('r ',/,(6(1x,e11.4)))
         endif
!     
!           check convergence
!     
         if((htri.le.1.d-5).or.(dabs(ddg).lt.1.d-3*dabs(dg))) then
            dd=0.d0
            do i=1,13
               dd=dd+r(i)*r(i)
            enddo
            dd=sqrt(dd)
            if(dd.le.1.d-10) then
               if((iint.eq.1).and.(iel.eq.1)) then
                  write(*,*) 'CONVERGENCE!'
               endif
               exit
            endif
         endif
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,*) 'no convergence'
         endif
!
!           determining b.x
!
         b=dg/dsg
!
         x(1)=b*(c1-sg(1)*sg(1))
         x(2)=b*(c2-sg(1)*sg(2))
         x(3)=b*(c1-sg(2)*sg(2))
         x(4)=b*(c2-sg(1)*sg(3))
         x(5)=b*(c2-sg(2)*sg(3))
         x(6)=b*(c1-sg(3)*sg(3))
         x(7)=-b*sg(1)*sg(4)
         x(8)=-b*sg(2)*sg(4)
         x(9)=-b*sg(3)*sg(4)
         x(10)=b*(.5d0-sg(4)*sg(4))
         x(11)=-b*sg(1)*sg(5)
         x(12)=-b*sg(2)*sg(5)
         x(13)=-b*sg(3)*sg(5)
         x(14)=-b*sg(4)*sg(5)
         x(15)=b*(.5d0-sg(5)*sg(5))
         x(16)=-b*sg(1)*sg(6)
         x(17)=-b*sg(2)*sg(6)
         x(18)=-b*sg(3)*sg(6)
         x(19)=-b*sg(4)*sg(6)
         x(20)=-b*sg(5)*sg(6)
         x(21)=b*(.5d0-sg(6)*sg(6))
!
         do i=1,21
            au1(i)=h2*x(i)
         enddo
         au1(1)=au1(1)+1.d0
         au1(3)=au1(3)+1.d0
         au1(6)=au1(6)+1.d0
         au1(10)=au1(10)+.5d0
         au1(15)=au1(15)+.5d0
         au1(21)=au1(21)+.5d0
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,811) (au1(i),i=1,21)
 811        format('au1 ',/,(6(1x,e11.4)))
         endif
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,811) (cm1(i),i=1,21)
 812        format('cm1 ',/,(6(1x,e11.4)))
         endif
!
!           filling the LHS
!
         if(iorien.gt.0) then
            gl(1,1)=au1(1)*cm1(1)+au1(2)*cm1(2)+au1(4)*cm1(4)+
     &           2.d0*(au1(7)*cm1(7)+au1(11)*cm1(11)+au1(16)*cm1(16))+
     &           x(1)
            gl(1,2)=au1(1)*cm1(2)+au1(2)*cm1(3)+au1(4)*cm1(5)+
     &           2.d0*(au1(7)*cm1(8)+au1(11)*cm1(12)+au1(16)*cm1(17))+
     &           x(2)
            gl(2,2)=au1(2)*cm1(2)+au1(3)*cm1(3)+au1(5)*cm1(5)+
     &           2.d0*(au1(8)*cm1(8)+au1(12)*cm1(12)+au1(17)*cm1(17))+
     &           x(3)
            gl(1,3)=au1(1)*cm1(4)+au1(2)*cm1(5)+au1(4)*cm1(6)+
     &           2.d0*(au1(7)*cm1(9)+au1(11)*cm1(13)+au1(16)*cm1(18))+
     &           x(4)
            gl(2,3)=au1(2)*cm1(4)+au1(3)*cm1(5)+au1(5)*cm1(6)+
     &           2.d0*(au1(8)*cm1(9)+au1(12)*cm1(13)+au1(17)*cm1(18))+
     &           x(5)
            gl(3,3)=au1(4)*cm1(4)+au1(5)*cm1(5)+au1(6)*cm1(6)+
     &           2.d0*(au1(9)*cm1(9)+au1(13)*cm1(13)+au1(18)*cm1(18))+
     &           x(6)
            gl(1,4)=au1(1)*cm1(7)+au1(2)*cm1(8)+au1(4)*cm1(9)+
     &           2.d0*(au1(7)*cm1(10)+au1(11)*cm1(14)+au1(16)*cm1(19))+
     &           x(7)
            gl(2,4)=au1(2)*cm1(7)+au1(3)*cm1(8)+au1(5)*cm1(9)+
     &           2.d0*(au1(8)*cm1(10)+au1(12)*cm1(14)+au1(17)*cm1(19))+
     &           x(8)
            gl(3,4)=au1(4)*cm1(7)+au1(5)*cm1(8)+au1(6)*cm1(9)+
     &           2.d0*(au1(9)*cm1(10)+au1(13)*cm1(14)+au1(18)*cm1(19))+
     &           x(9)
            gl(4,4)=au1(7)*cm1(7)+au1(8)*cm1(8)+au1(9)*cm1(9)+
     &           2.d0*(au1(10)*cm1(10)+au1(14)*cm1(14)+au1(19)*cm1(19))+
     &           x(10)
            gl(1,5)=au1(1)*cm1(11)+au1(2)*cm1(12)+au1(4)*cm1(13)+
     &           2.d0*(au1(7)*cm1(14)+au1(11)*cm1(15)+au1(16)*cm1(20))+
     &           x(11)
            gl(2,5)=au1(2)*cm1(11)+au1(3)*cm1(12)+au1(5)*cm1(13)+
     &           2.d0*(au1(8)*cm1(14)+au1(12)*cm1(15)+au1(17)*cm1(20))+
     &           x(12)
            gl(3,5)=au1(4)*cm1(11)+au1(5)*cm1(12)+au1(6)*cm1(13)+
     &           2.d0*(au1(9)*cm1(14)+au1(13)*cm1(15)+au1(18)*cm1(20))+
     &           x(13)
            gl(4,5)=au1(7)*cm1(11)+au1(8)*cm1(12)+au1(9)*cm1(13)+
     &           2.d0*(au1(10)*cm1(14)+au1(14)*cm1(15)+au1(19)*cm1(20))+
     &           x(14)
            gl(5,5)=au1(11)*cm1(11)+au1(12)*cm1(12)+au1(13)*cm1(13)+
     &           2.d0*(au1(14)*cm1(14)+au1(15)*cm1(15)+au1(20)*cm1(20))+
     &           x(15)
            gl(1,6)=au1(1)*cm1(16)+au1(2)*cm1(17)+au1(4)*cm1(18)+
     &           2.d0*(au1(7)*cm1(19)+au1(11)*cm1(20)+au1(16)*cm1(21))+
     &           x(16)
            gl(2,6)=au1(2)*cm1(16)+au1(3)*cm1(17)+au1(5)*cm1(18)+
     &           2.d0*(au1(8)*cm1(19)+au1(12)*cm1(20)+au1(17)*cm1(21))+
     &           x(17)
            gl(3,6)=au1(4)*cm1(16)+au1(5)*cm1(17)+au1(6)*cm1(18)+
     &           2.d0*(au1(9)*cm1(19)+au1(13)*cm1(20)+au1(18)*cm1(21))+
     &           x(18)
            gl(4,6)=au1(7)*cm1(16)+au1(8)*cm1(17)+au1(9)*cm1(18)+
     &           2.d0*(au1(10)*cm1(19)+au1(14)*cm1(20)+au1(19)*cm1(21))+
     &           x(19)
            gl(5,6)=au1(11)*cm1(16)+au1(12)*cm1(17)+au1(13)*cm1(18)+
     &           2.d0*(au1(14)*cm1(19)+au1(15)*cm1(20)+au1(20)*cm1(21))+
     &           x(20)
            gl(6,6)=au1(16)*cm1(16)+au1(17)*cm1(17)+au1(18)*cm1(18)+
     &           2.d0*(au1(19)*cm1(19)+au1(20)*cm1(20)+au1(21)*cm1(21))+
     &           x(21)
            do i=1,6
               do j=1,i-1
                  gl(i,j)=gl(j,i)
               enddo
            enddo
            do i=1,6
               do j=4,6
                  gl(i,j)=2.d0*gl(i,j)
               enddo
            enddo
         else
            gl(1,1)=au1(1)*cm1(1)+au1(2)*cm1(2)+au1(4)*cm1(4)+x(1)
            gl(1,2)=au1(1)*cm1(2)+au1(2)*cm1(3)+au1(4)*cm1(5)+x(2)
            gl(2,2)=au1(2)*cm1(2)+au1(3)*cm1(3)+au1(5)*cm1(5)+x(3)
            gl(1,3)=au1(1)*cm1(4)+au1(2)*cm1(5)+au1(4)*cm1(6)+x(4)
            gl(2,3)=au1(2)*cm1(4)+au1(3)*cm1(5)+au1(5)*cm1(6)+x(5)
            gl(3,3)=au1(4)*cm1(4)+au1(5)*cm1(5)+au1(6)*cm1(6)+x(6)
            gl(1,4)=2.d0*au1(7)*cm1(7)+x(7)
            gl(2,4)=2.d0*au1(8)*cm1(7)+x(8)
            gl(3,4)=2.d0*au1(9)*cm1(7)+x(9)
            gl(4,4)=2.d0*au1(10)*cm1(7)+x(10)
            gl(1,5)=2.d0*au1(11)*cm1(8)+x(11)
            gl(2,5)=2.d0*au1(12)*cm1(8)+x(12)
            gl(3,5)=2.d0*au1(13)*cm1(8)+x(13)
            gl(4,5)=2.d0*au1(14)*cm1(8)+x(14)
            gl(5,5)=2.d0*au1(15)*cm1(8)+x(15)
            gl(1,6)=2.d0*au1(16)*cm1(9)+x(16)
            gl(2,6)=2.d0*au1(17)*cm1(9)+x(17)
            gl(3,6)=2.d0*au1(18)*cm1(9)+x(18)
            gl(4,6)=2.d0*au1(19)*cm1(9)+x(19)
            gl(5,6)=2.d0*au1(20)*cm1(9)+x(20)
            gl(6,6)=2.d0*au1(21)*cm1(9)+x(21)
            do i=1,6
               do j=1,i-1
                  gl(i,j)=gl(j,i)
               enddo
            enddo
            do i=1,6
               do j=4,6
                  gl(i,j)=2.d0*gl(i,j)
               enddo
            enddo
         endif
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,813) ((gl(i,j),j=1,6),i=1,6)
 813        format('gl ',/,(6(1x,e11.4)))
         endif
!
!           filling the RHS
!
         do i=1,6
            gr(i,1)=sg(i)
         enddo
!     
!           solve gl:(P:n)=gr
!
         call dgesv(neq,nrhs,gl,lda,ipiv,gr,ldb,info)
         if(info.ne.0) then
            write(*,*) '*ERROR in sc.f: linear equation solver'
            write(*,*) '       exited with error: info = ',info
            stop
         endif
!
         do i=1,6
            Pn(i)=gr(i,1)
         enddo
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,411) (Pn(i),i=1,6)
 411        format('Pn ',/,(6(1x,e11.4)))
         endif
!
c         c3=-1.d0/(a+b)
         c3=-h2/(1.d0+b*h2)
         QSn(1)=c3*(x(1)*Pn(1)+x(2)*Pn(2)+x(4)*Pn(3)+
     &        2.d0*(x(7)*Pn(4)+x(11)*Pn(5)+x(16)*Pn(6)))+sg(1)*h2
         QSn(2)=c3*(x(2)*Pn(1)+x(3)*Pn(2)+x(5)*Pn(3)+
     &        2.d0*(x(8)*Pn(4)+x(12)*Pn(5)+x(17)*Pn(6)))+sg(2)*h2
         QSn(3)=c3*(x(4)*Pn(1)+x(5)*Pn(2)+x(6)*Pn(3)+
     &        2.d0*(x(9)*Pn(4)+x(13)*Pn(5)+x(18)*Pn(6)))+sg(3)*h2
         QSn(4)=c3*(x(7)*Pn(1)+x(8)*Pn(2)+x(9)*Pn(3)+
     &        2.d0*(x(10)*Pn(4)+x(14)*Pn(5)+x(19)*Pn(6)))+sg(4)*h2
         QSn(5)=c3*(x(11)*Pn(1)+x(12)*Pn(2)+x(13)*Pn(3)+
     &        2.d0*(x(14)*Pn(4)+x(15)*Pn(5)+x(20)*Pn(6)))+sg(5)*h2
         QSn(6)=c3*(x(16)*Pn(1)+x(17)*Pn(2)+x(18)*Pn(3)+
     &        2.d0*(x(19)*Pn(4)+x(20)*Pn(5)+x(21)*Pn(6)))+sg(6)*h2
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,412) (QSn(i),i=1,6)
 412        format('QSn ',/,(6(1x,e11.4)))
         endif
!
!           calculating the creep contribution
!
         if(creep) then
            if(dg.gt.0.d0) then
               gcreep=c0*ca/cn*(dg*ca)**(1.d0/cn-1.d0)
            else
!     
!              for gamma ein default of 1.d-10 is taken to
!              obtain a finite gradient
!
               gcreep=c0*ca/cn*(1.d-10*ca)**(1.d0/cn-1.d0)
            endif
         endif
!
!           calculating the correction to the consistency parameter
!
         gm1=Pn(1)*sg(1)+Pn(2)*sg(2)+Pn(3)*sg(3)+
     &        2.d0*(Pn(4)*sg(4)+Pn(5)*sg(5)+Pn(6)*sg(6))+
     &        c1*h1+
     &        QSn(1)*sg(1)+QSn(2)*sg(2)+QSn(3)*sg(3)+
     &        2.d0*(QSn(4)*sg(4)+QSn(5)*sg(5)+QSn(6)*sg(6))
         if(creep) then
            gm1=1.d0/(gm1+gcreep)
         else
            gm1=1.d0/gm1
         endif
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,512) gm1
 512        format('gm1 ',/,(6(1x,e11.4)))
         endif
         ddg=gm1*(htri-(Pn(1)*r(1)+Pn(2)*r(2)+Pn(3)*r(3)+
     &        2.d0*(Pn(4)*r(4)+Pn(5)*r(5)+Pn(6)*r(6))+
     &        c0*h1*r(7)+
     &        QSn(1)*r(8)+QSn(2)*r(9)+QSn(3)*r(10)+
     &        2.d0*(QSn(4)*r(11)+QSn(5)*r(12)+QSn(6)*r(13))))
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,313) ddg
 313        format('ddg ',/,(6(1x,e11.4)))
         endif
!
!        updating the residual matrix
!
         do i=1,6
            r(i)=r(i)+ddg*sg(i)
         enddo
         r(7)=r(7)+ddg*c0
         do i=1,6
            r(7+i)=r(7+i)+ddg*sg(i)
         enddo
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,210) (r(i),i=1,13)
 210        format('r up ',/,(6(1x,e11.4)))
         endif
!
!        update the plastic strain
!
         gr(1,1)=au1(1)*r(1)+au1(2)*r(2)+au1(4)*r(3)+
     &        2.d0*(au1(7)*r(4)+au1(11)*r(5)+au1(16)*r(6))
     &        -h2*(x(1)*r(8)+x(2)*r(9)+x(4)*r(10)+
     &        2.d0*(x(7)*r(11)+x(11)*r(12)+x(16)*r(13)))
         gr(2,1)=au1(2)*r(1)+au1(3)*r(2)+au1(5)*r(3)+
     &        2.d0*(au1(8)*r(4)+au1(12)*r(5)+au1(17)*r(6))
     &        -h2*(x(2)*r(8)+x(3)*r(9)+x(5)*r(10)+
     &        2.d0*(x(8)*r(11)+x(12)*r(12)+x(17)*r(13)))
         gr(3,1)=au1(4)*r(1)+au1(5)*r(2)+au1(6)*r(3)+
     &        2.d0*(au1(9)*r(4)+au1(13)*r(5)+au1(18)*r(6))
     &        -h2*(x(4)*r(8)+x(5)*r(9)+x(6)*r(10)+
     &        2.d0*(x(9)*r(11)+x(13)*r(12)+x(18)*r(13)))
         gr(4,1)=au1(7)*r(1)+au1(8)*r(2)+au1(9)*r(3)+
     &        2.d0*(au1(10)*r(4)+au1(14)*r(5)+au1(19)*r(6))
     &        -h2*(x(7)*r(8)+x(8)*r(9)+x(9)*r(10)+
     &        2.d0*(x(10)*r(11)+x(14)*r(12)+x(19)*r(13)))
         gr(5,1)=au1(11)*r(1)+au1(12)*r(2)+au1(13)*r(3)+
     &        2.d0*(au1(14)*r(4)+au1(15)*r(5)+au1(20)*r(6))
     &        -h2*(x(11)*r(8)+x(12)*r(9)+x(13)*r(10)+
     &        2.d0*(x(14)*r(11)+x(15)*r(12)+x(20)*r(13)))
         gr(6,1)=au1(16)*r(1)+au1(17)*r(2)+au1(18)*r(3)+
     &        2.d0*(au1(19)*r(4)+au1(20)*r(5)+au1(21)*r(6))
     &        -h2*(x(16)*r(8)+x(17)*r(9)+x(18)*r(10)+
     &        2.d0*(x(19)*r(11)+x(20)*r(12)+x(21)*r(13)))
!
         call dgetrs('No transpose',neq,nrhs,gl,lda,ipiv,gr,ldb,info)
         if(info.ne.0) then
            write(*,*) '*ERROR in sc.f: linear equation solver'
            write(*,*) '       exited with error: info = ',info
            stop
         endif
!
         if(iorien.gt.0) then
            ep(1)=ep(1)+cm1(1)*gr(1,1)+cm1(2)*gr(2,1)+cm1(4)*gr(3,1)+
     &           2.d0*(cm1(7)*gr(4,1)+cm1(11)*gr(5,1)+cm1(16)*gr(6,1))
            ep(2)=ep(2)+cm1(2)*gr(1,1)+cm1(3)*gr(2,1)+cm1(5)*gr(3,1)+
     &           2.d0*(cm1(8)*gr(4,1)+cm1(12)*gr(5,1)+cm1(17)*gr(6,1))
            ep(3)=ep(3)+cm1(4)*gr(1,1)+cm1(5)*gr(2,1)+cm1(6)*gr(3,1)+
     &           2.d0*(cm1(9)*gr(4,1)+cm1(13)*gr(5,1)+cm1(18)*gr(6,1))
            ep(4)=ep(4)+cm1(7)*gr(1,1)+cm1(8)*gr(2,1)+cm1(9)*gr(3,1)+
     &           2.d0*(cm1(10)*gr(4,1)+cm1(14)*gr(5,1)+cm1(19)*gr(6,1))
            ep(5)=ep(5)+cm1(11)*gr(1,1)+cm1(12)*gr(2,1)+cm1(13)*gr(3,1)+
     &           2.d0*(cm1(14)*gr(4,1)+cm1(15)*gr(5,1)+cm1(20)*gr(6,1))
            ep(6)=ep(6)+cm1(16)*gr(1,1)+cm1(17)*gr(2,1)+cm1(18)*gr(3,1)+
     &           2.d0*(cm1(19)*gr(4,1)+cm1(20)*gr(5,1)+cm1(21)*gr(6,1))
         else
            ep(1)=ep(1)+cm1(1)*gr(1,1)+cm1(2)*gr(2,1)+cm1(4)*gr(3,1)
            ep(2)=ep(2)+cm1(2)*gr(1,1)+cm1(3)*gr(2,1)+cm1(5)*gr(3,1)
            ep(3)=ep(3)+cm1(4)*gr(1,1)+cm1(5)*gr(2,1)+cm1(6)*gr(3,1)
            ep(4)=ep(4)+2.d0*cm1(7)*gr(4,1)
            ep(5)=ep(5)+2.d0*cm1(8)*gr(5,1)
            ep(6)=ep(6)+2.d0*cm1(9)*gr(6,1)
         endif
!
!        update the isotropic hardening variable
!
         al1=al1+r(7)
!
!        update the kinematic hardening variables
!
c         c4=a/(a+b)
c         c6=b/(a+b)
         c4=1.d0/(1.d0+b*h2)
         c6=c4*b*h2
         c5=c6/3.d0
         au2(1)=c4+c5+c6*sg(1)*sg(1)
         au2(2)=c5+c6*sg(1)*sg(2)
         au2(3)=c4+c5+c6*sg(2)*sg(2)
         au2(4)=c5+c6*sg(1)*sg(3)
         au2(5)=c5+c6*sg(2)*sg(3)
         au2(6)=c4+c5+c6*sg(3)*sg(3)
         au2(7)=c6*sg(1)*sg(4)
         au2(8)=c6*sg(2)*sg(4)
         au2(9)=c6*sg(3)*sg(4)
         au2(10)=c4/2.d0+c6*sg(4)*sg(4)
         au2(11)=c6*sg(1)*sg(5)
         au2(12)=c6*sg(2)*sg(5)
         au2(13)=c6*sg(3)*sg(5)
         au2(14)=c6*sg(4)*sg(5)
         au2(15)=c4/2.d0+c6*sg(5)*sg(5)
         au2(16)=c6*sg(1)*sg(6)
         au2(17)=c6*sg(2)*sg(6)
         au2(18)=c6*sg(3)*sg(6)
         au2(19)=c6*sg(4)*sg(6)
         au2(20)=c6*sg(5)*sg(6)
         au2(21)=c4/2.d0+c6*sg(6)*sg(6)
!
         al2(1)=al2(1)+au2(1)*r(8)+au2(2)*r(9)+au2(4)*r(10)+
     &        2.d0*(au2(7)*r(11)+au2(11)*r(12)+au2(16)*r(13))
     &        -c4*(x(1)*gr(1,1)+x(2)*gr(2,1)+x(4)*gr(3,1)+
     &        2.d0*(x(7)*gr(4,1)+x(11)*gr(5,1)+x(16)*gr(6,1)))
         al2(2)=al2(2)+au2(2)*r(8)+au2(3)*r(9)+au2(5)*r(10)+
     &        2.d0*(au2(8)*r(11)+au2(12)*r(12)+au2(17)*r(13))
     &        -c4*(x(2)*gr(1,1)+x(3)*gr(2,1)+x(5)*gr(3,1)+
     &        2.d0*(x(8)*gr(4,1)+x(12)*gr(5,1)+x(17)*gr(6,1)))
         al2(3)=al2(3)+au2(4)*r(8)+au2(5)*r(9)+au2(6)*r(10)+
     &        2.d0*(au2(9)*r(11)+au2(13)*r(12)+au2(18)*r(13))
     &        -c4*(x(4)*gr(1,1)+x(5)*gr(2,1)+x(6)*gr(3,1)+
     &        2.d0*(x(9)*gr(4,1)+x(13)*gr(5,1)+x(18)*gr(6,1)))
         al2(4)=al2(4)+au2(7)*r(8)+au2(8)*r(9)+au2(9)*r(10)+
     &        2.d0*(au2(10)*r(11)+au2(14)*r(12)+au2(19)*r(13))
     &        -c4*(x(7)*gr(1,1)+x(8)*gr(2,1)+x(9)*gr(3,1)+
     &        2.d0*(x(10)*gr(4,1)+x(14)*gr(5,1)+x(19)*gr(6,1)))
         al2(5)=al2(5)+au2(11)*r(8)+au2(12)*r(9)+au2(13)*r(10)+
     &        2.d0*(au2(14)*r(11)+au2(15)*r(12)+au2(20)*r(13))
     &        -c4*(x(11)*gr(1,1)+x(12)*gr(2,1)+x(13)*gr(3,1)+
     &        2.d0*(x(14)*gr(4,1)+x(15)*gr(5,1)+x(20)*gr(6,1)))
         al2(6)=al2(6)+au2(16)*r(8)+au2(17)*r(9)+au2(18)*r(10)+
     &        2.d0*(au2(19)*r(11)+au2(20)*r(12)+au2(21)*r(13))
     &        -c4*(x(16)*gr(1,1)+x(17)*gr(2,1)+x(18)*gr(3,1)+
     &        2.d0*(x(19)*gr(4,1)+x(20)*gr(5,1)+x(21)*gr(6,1)))
!
!        update the consistency parameter
!
         dg=dg+ddg
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,*) 'ep ',(ep(i),i=1,6)
            write(*,211) al1
 211        format('al1new ',/,(6(1x,e11.4)))
            write(*,212) (al2(i),i=1,6)
 212        format('al2new ',/,(6(1x,e11.4)))
            write(*,213) dg
 213        format('dg ',/,(6(1x,e11.4)))
         endif
!
!        end of major loop
!
      enddo
!
!     storing the stress
!
      do i=1,6
         stre(i)=stri(i)
      enddo
!
!     calculating the tangent stiffness matrix
!      
      if(icmd.ne.3) then
!
!        determining p
!
         gr(1,1)=au1(1) 
         gr(1,2)=au1(2) 
         gr(2,2)=au1(3) 
         gr(1,3)=au1(4) 
         gr(2,3)=au1(5) 
         gr(3,3)=au1(6) 
         gr(1,4)=au1(7) 
         gr(2,4)=au1(8) 
         gr(3,4)=au1(9) 
         gr(4,4)=au1(10)
         gr(1,5)=au1(11)
         gr(2,5)=au1(12)
         gr(3,5)=au1(13)
         gr(4,5)=au1(14)
         gr(5,5)=au1(15)
         gr(1,6)=au1(16)
         gr(2,6)=au1(17)
         gr(3,6)=au1(18)
         gr(4,6)=au1(19)
         gr(5,6)=au1(20)
         gr(6,6)=au1(21)
         do i=1,6
            do j=1,i-1
               gr(i,j)=gr(j,i)
            enddo
         enddo
         nrhs=6
!
         call dgetrs('No transpose',neq,nrhs,gl,lda,ipiv,gr,ldb,info)
         if(info.ne.0) then
            write(*,*) '*ERROR in sc.f: linear equation solver'
            write(*,*) '       exited with error: info = ',info
            stop
         endif
!
      if((iint.eq.1).and.(iel.eq.1)) then
         write(*,714) ((gr(i,j),j=1,6),i=1,6)
 714     format('gr ',/,(6(1x,e11.4)))
      endif
         stiff(1)=gr(1,1)-gm1*Pn(1)*Pn(1)
         stiff(2)=gr(1,2)-gm1*Pn(1)*Pn(2)
         stiff(3)=gr(2,2)-gm1*Pn(2)*Pn(2)
         stiff(4)=gr(1,3)-gm1*Pn(1)*Pn(3)
         stiff(5)=gr(2,3)-gm1*Pn(2)*Pn(3)
         stiff(6)=gr(3,3)-gm1*Pn(3)*Pn(3)
         stiff(7)=gr(1,4)-gm1*Pn(1)*Pn(4)
         stiff(8)=gr(2,4)-gm1*Pn(2)*Pn(4)
         stiff(9)=gr(3,4)-gm1*Pn(3)*Pn(4)
         stiff(10)=gr(4,4)-gm1*Pn(4)*Pn(4)
         stiff(11)=gr(1,5)-gm1*Pn(1)*Pn(5)
         stiff(12)=gr(2,5)-gm1*Pn(2)*Pn(5)
         stiff(13)=gr(3,5)-gm1*Pn(3)*Pn(5)
         stiff(14)=gr(4,5)-gm1*Pn(4)*Pn(5)
         stiff(15)=gr(5,5)-gm1*Pn(5)*Pn(5)
         stiff(16)=gr(1,6)-gm1*Pn(1)*Pn(6)
         stiff(17)=gr(2,6)-gm1*Pn(2)*Pn(6)
         stiff(18)=gr(3,6)-gm1*Pn(3)*Pn(6)
         stiff(19)=gr(4,6)-gm1*Pn(4)*Pn(6)
         stiff(20)=gr(5,6)-gm1*Pn(5)*Pn(6)
         stiff(21)=gr(6,6)-gm1*Pn(6)*Pn(6)
!
         if((iint.eq.1).and.(iel.eq.1)) then
            write(*,*) 'stiffness '
            write(*,'(6(1x,e11.4))') (stiff(i),i=1,21)
         endif
      endif
      if((iint.eq.1).and.(iel.eq.1)) then
         write(*,311) q1
 311     format('q1new ',/,(6(1x,e11.4)))
         write(*,312) (q2(i),i=1,6)
 312     format('q2new ',/,(6(1x,e11.4)))
      endif
      if((iint.eq.1).and.(iel.eq.1)) then
      write(*,*) ' stress '
      write(*,'(6(1x,e11.4))') (stri(i),i=1,6)
         write(*,214) dg,dtime
 214     format('dg ',/,(6(1x,e11.4)))
      endif
!
!     updating the state variables
!
      xstate(1,iint,iel)=eeq+c0*dg
      do i=1,6
         xstate(1+i,iint,iel)=ep(i)
      enddo
      xstate(8,iint,iel)=al1
      do i=1,6
         xstate(8+i,iint,iel)=al2(i)
      enddo
!
      return
      end
!
!     subroutine to solve a set of linear equations with
!     a general real matrix
!
!
