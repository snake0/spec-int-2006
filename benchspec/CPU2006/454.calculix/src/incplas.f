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
      subroutine incplas(elconloc,plconloc,xstate,xstateini,
     &  elas,eloc,eth,ithermal,icmd,beta,stre,vj,kode,
     &  ielas,amat,t1l,dtime,iel,iint,nstate_,mint_)
!
!     calculates stiffness and stresses for the incremental plasticity
!     material law (Ref: J.C. Simo, A framework for finite strain
!     elastoplasticity, Comp. Meth. Appl. Mech. Engng., 66(1988)199-219
!     and 68(1988)1-31)
!
!     icmd=3: calcutates stress at mechanical strain
!     else: calculates stress at mechanical strain and the stiffness
!           matrix
!
!     the stresses in the routine proposed by Simo are Kirchhoff 
!     stresses. Since the stress in the hardening laws are Chauchy
!     stresses, they are converted into Kirchhoff stress by 
!     multiplication with the Jacobian determinant
!
      implicit none
!
      logical user_hardening,user_creep
!
      character*20 amat
!
      integer ithermal,icmd,i,j,k,l,m,n,nt,kk(84),kode,
     &  niso,nkin,ielas,iel,iint,nstate_,mint_,id
!
      real*8 elconloc(21),elas(21),eloc(6),eth(6),beta(6),stre(6),
     &  vj,plconloc(82),stbl(6),epl,stril(6),xitril(6),
     &  ee,un,um,al,xk,cop,umb,umbb,dxitril,f0,d0,f1,d1,d2,xg(3,3),
     &  xs(3,3),xx(3,3),xn(3,3),xd(3,3),cpl(6),c(6),ci(6),
     &  c1,c2,c3,c4,c5,c6,c7,c8,c9,cplb(6),stblb(6),
     &  ftrial,vjj,xiso(20),yiso(20),xkin(20),ykin(20),
     &  fiso,dfiso,fkin,dfkin,fiso0,fkin0,ep,t1l,dtime,
     &  epini,a1,a2,a3,svm,dsvm,xxa,xxn,vj2,vj23,dcopold,dcopnew,
     &  cop1,cop2,fu1,fu2,fu,dcop
!
      real*8 xstate(nstate_,mint_,*),xstateini(nstate_,mint_,*)
!
      real*8 g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,
     &  g17,g18,g28,g29,g30,g31,g32,g33
!
      data kk /1,1,1,1,1,1,2,2,2,2,2,2,1,1,3,3,2,2,3,3,3,3,3,3,
     &  1,1,1,2,2,2,1,2,3,3,1,2,1,2,1,2,1,1,1,3,2,2,1,3,3,3,1,3,
     &  1,2,1,3,1,3,1,3,1,1,2,3,2,2,2,3,3,3,2,3,1,2,2,3,1,3,2,3,
     &  2,3,2,3/
!
      data xg /1.,0.,0.,0.,1.,0.,0.,0.,1./
!
!     localizing the plastic fields
!
      do i=1,6
         cpl(i)=xstateini(1+i,iint,iel)
         stbl(i)=xstateini(7+i,iint,iel)
      enddo
      epl=xstateini(1,iint,iel)
      epini=xstateini(1,iint,iel)
!
      ee=elconloc(1)
      un=elconloc(2)
      um=ee/(1.d0+un)
      al=um*un/(1.d0-2.d0*un)
      xk=al+um/3.d0
      um=um/2.d0
!
      ep=epl
!
!     right Cauchy-Green tensor (eloc contains the Lagrange strain)
!
      c(1)=2.d0*eloc(1)+1.d0
      c(2)=2.d0*eloc(2)+1.d0
      c(3)=2.d0*eloc(3)+1.d0
      c(4)=2.d0*eloc(4)
      c(5)=2.d0*eloc(5)
      c(6)=2.d0*eloc(6)
!
!     calculating the Jacobian
!
      vj=dsqrt(c(1)*(c(2)*c(3)-c(6)*c(6))
     &        -c(4)*(c(4)*c(3)-c(6)*c(5))
     &        +c(5)*(c(4)*c(6)-c(2)*c(5)))
!
!     approximation of the pure mechanical Jacobian?
!
      if(ithermal.ne.0) then
         vjj=vj*(1.d0-2.d0*eth(1))**1.5
      else
         vjj=vj
      endif
!
!     check for user subroutines
!
      if((plconloc(81).lt.0.8d0).and.(plconloc(82).lt.0.8d0)) then
         user_hardening=.true.
      else
         user_hardening=.false.
      endif
      if(kode.eq.-52) then
         if(elconloc(3).lt.0.d0) then
            user_creep=.true.
         else
            user_creep=.false.
            xxa=elconloc(3)
            xxn=elconloc(4)
            a1=xxa*dtime
            a2=xxn*a1
            a3=1.d0/xxn
         endif
      endif
!
!        inversion of the right Cauchy-Green tensor
!
      vj2=vj*vj
      ci(1)=(c(2)*c(3)-c(6)*c(6))/vj2
      ci(2)=(c(1)*c(3)-c(5)*c(5))/vj2
      ci(3)=(c(1)*c(2)-c(4)*c(4))/vj2
      ci(4)=(c(5)*c(6)-c(4)*c(3))/vj2
      ci(5)=(c(4)*c(6)-c(2)*c(5))/vj2
      ci(6)=(c(4)*c(5)-c(1)*c(6))/vj2
!
!        reducing the plastic right Cauchy-Green tensor and
!        the back stress to "isochoric" quantities (b stands
!        for bar)
!
      vj23=vj**(2.d0/3.d0)
      do i=1,6
         cplb(i)=cpl(i)/vj23
         stblb(i)=stbl(i)/vj23
      enddo
!
!        calculating the (n+1) trace and the (n+1) deviation of 
!        the (n) "isochoric" plastic right Cauchy-Green tensor
!
      umb=(c(1)*cplb(1)+c(2)*cplb(2)+c(3)*cplb(3)+
     &     2.d0*(c(4)*cplb(4)+c(5)*cplb(5)+c(6)*cplb(6)))/3.d0
      do i=1,6
         cplb(i)=cplb(i)-umb*ci(i)
      enddo
!
!        calculating the (n+1) trace and the (n+1) deviation of 
!        the (n) "isochoric" back stress tensor
!
      umbb=(c(1)*stblb(1)+c(2)*stblb(2)+c(3)*stblb(3)+
     &     2.d0*(c(4)*stblb(4)+c(5)*stblb(5)+c(6)*stblb(6)))/3.d0
      do i=1,6
         stblb(i)=stblb(i)-umbb*ci(i)
      enddo
!
!        calculating the trial stress
!
      do i=1,6
         stril(i)=um*cplb(i)
      enddo
!
!        calculating the trial radius vector of the yield surface
!
      do i=1,6
         xitril(i)=stril(i)-stblb(i)
      enddo
      g1=c(6)
      g2=xitril(6)
      g3=xitril(3)
      g4=xitril(2)
      g5=c(5)
      g6=xitril(5)
      g7=xitril(4)
      g8=c(4)
      g9=c(3)
      g10=c(2)
      g11=c(1)
      g12=xitril(1)
      g13=g12*g11
      g14=g10*g4
      g15=g9*g3
      g16=g8*g7
      g17=g6*g5
      g18=g2*g1
      g28=4*(g16 + g15)
      g29=4*g13
      g30=4*g14
      g31=4*g6*g1
      g32=4*g8*g2
      g33=4*g7*g5
      dxitril=dsqrt(g13*g13 + g14*g14 + g15*g15 + g16*(g30 + g29 + 2*
     &     g16) + g17*(g29 + g28 + 2*g17) + g18*(g30 + g28 + 2*
     &     g18 + 4*g17) + g11*g7*(g31 + 2*g10*g7) + g9*g6*(g32 + 
     &     2*g11*g6) + g10*g2*(g33 + 2*g9*g2) + g8*g4*(g31 + 2*
     &     g12*g8) + g12*g5*(g32 + 2*g5*g3) + g3*g1*(g33 + 2*g4*
     &     g1))
!
!        restoring the hardening curves for the actual temperature
!        plconloc contains the true stresses. By multiplying by
!        the Jacobian, yiso and ykin are Kirchhoff stresses, as
!        required by the hyperelastic theory (cf. Simo, 1988).
!
!
      niso=int(plconloc(81))
      nkin=int(plconloc(82))
      if(niso.ne.0) then
         do i=1,niso
            xiso(i)=plconloc(2*i-1)
            yiso(i)=vj*plconloc(2*i)
         enddo
      endif
      if(nkin.ne.0) then
         do i=1,nkin
            xkin(i)=plconloc(39+2*i)
            ykin(i)=vj*plconloc(40+2*i)
         enddo
      endif
!
!     check for yielding
!
      if(user_hardening) then
         call uhardening(amat,iel,iint,t1l,epini,ep,dtime,
     &        fiso,dfiso,fkin,dfkin)
         fiso=fiso*vj
      else
         if(niso.ne.0) then
            call ident(xiso,ep,niso,id)
            if(id.eq.0) then
               fiso=yiso(1)
            elseif(id.eq.niso) then
               fiso=yiso(niso)
            else
               dfiso=(yiso(id+1)-yiso(id))/(xiso(id+1)-xiso(id))
               fiso=yiso(id)+dfiso*(ep-xiso(id))
            endif
         elseif(nkin.ne.0) then
            fiso=ykin(1)
         else
            fiso=0.d0
         endif
      endif
!
      ftrial=dxitril-dsqrt(2.d0/3.d0)*fiso
      if((ftrial.le.0.d0).or.(ielas.eq.1)) then
!
!        no plastic deformation
!        beta contains the Cauchy residual stresses
!
         c8=xk*(vjj*vjj-1.d0)/2.d0
!
!           residual stresses are de facto PK2 stresses
!           (Piola-Kirchhoff of the second kind)
!
         stre(1)=c8*ci(1)+stril(1)+beta(1)
         stre(2)=c8*ci(2)+stril(2)+beta(2)
         stre(3)=c8*ci(3)+stril(3)+beta(3)
         stre(4)=c8*ci(4)+stril(4)+beta(4)
         stre(5)=c8*ci(5)+stril(5)+beta(5)
         stre(6)=c8*ci(6)+stril(6)+beta(6)
!
         if(icmd.ne.3) then
!
            umb=um*umb
!
!           calculating the local stiffness matrix
!
            xg(1,1)=(c(2)*c(3)-c(6)*c(6))/vj2
            xg(2,2)=(c(1)*c(3)-c(5)*c(5))/vj2
            xg(3,3)=(c(1)*c(2)-c(4)*c(4))/vj2
            xg(1,2)=(c(5)*c(6)-c(4)*c(3))/vj2
            xg(1,3)=(c(4)*c(6)-c(2)*c(5))/vj2
            xg(2,3)=(c(4)*c(5)-c(1)*c(6))/vj2
            xg(2,1)=xg(1,2)
            xg(3,1)=xg(1,3)
            xg(3,2)=xg(2,3)
!
            xs(1,1)=stril(1)
            xs(2,2)=stril(2)
            xs(3,3)=stril(3)
            xs(1,2)=stril(4)
            xs(2,1)=stril(4)
            xs(1,3)=stril(5)
            xs(3,1)=stril(5)
            xs(2,3)=stril(6)
            xs(3,2)=stril(6)
!
            nt=0
            do i=1,21
               k=kk(nt+1)
               l=kk(nt+2)
               m=kk(nt+3)
               n=kk(nt+4)
               nt=nt+4
               elas(i)=umb*(xg(k,m)*xg(l,n)+xg(k,n)*xg(l,m)-
     &              2.d0*xg(k,l)*xg(m,n)/3.d0)
     &              -2.d0*(xs(k,l)*xg(m,n)+xg(k,l)*xs(m,n))/3.d0
     &              +xk*vjj*vjj*xg(k,l)*xg(m,n)
     &              -xk*(vjj*vjj-1.d0)*(xg(k,m)*xg(l,n)
     &              +xg(k,n)*xg(l,m))/2.d0
            enddo
!
         endif
!
         return
      endif
!
!        plastic deformation
!
      umb=um*umb
      umbb=umb-umbb
!
!        calculating the consistency parameter
!
      c1=2.d0/3.d0
      c2=dsqrt(c1)
      c3=c1/um
      c4=c2/um
!
      cop=0.d0
      dcopold=0.d0
!
      loop: do
         ep=epl+c2*cop
!
         if(user_hardening) then
            call uhardening(amat,iel,iint,t1l,epini,ep,dtime,
     &           fiso,dfiso,fkin,dfkin)
            fiso=fiso*vj
            dfiso=dfiso*vj
            fkin=fkin*vj
            dfkin=dfkin*vj
         else
            if(niso.ne.0) then
               call ident(xiso,ep,niso,id)
               if(id.eq.0) then
                  fiso=yiso(1)
                  dfiso=0.d0
               elseif(id.eq.niso) then
                  fiso=yiso(niso)
                  dfiso=0.d0
               else
                  dfiso=(yiso(id+1)-yiso(id))/(xiso(id+1)-xiso(id))
                  fiso=yiso(id)+dfiso*(ep-xiso(id))
               endif
            elseif(nkin.ne.0) then
               fiso=ykin(1)
               dfiso=0.d0
            else
               fiso=0.d0
               dfiso=0.d0
            endif
!
            if(nkin.ne.0) then
               call ident(xkin,ep,nkin,id)
               if(id.eq.0) then
                  fkin=ykin(1)
                  dfkin=0.d0
               elseif(id.eq.nkin) then
                  fkin=ykin(nkin)
                  dfkin=0.d0
               else
                  dfkin=(ykin(id+1)-ykin(id))/(xkin(id+1)-xkin(id))
                  fkin=ykin(id)+dfkin*(ep-xkin(id))
               endif
            elseif(niso.ne.0) then
               fkin=yiso(1)
               dfkin=0.d0
            else
               fkin=0.d0
               dfkin=0.d0
            endif
         endif
!
         if(dabs(cop).lt.1.d-10) then
            fiso0=fiso
            fkin0=fkin
         endif
!
         if(kode.eq.-51) then
            dcopnew=(ftrial-c2*(fiso-fiso0)
     &           -umbb*(2.d0*cop+c4*(fkin-fkin0)))/
     &           (-c1*dfiso-umbb*(2.d0+c3*dfkin))
         else
            if(user_creep) then
               call ucreep(amat,iel,iint,t1l,epini,ep,dtime,svm,dsvm)
               svm=svm*vj
               dsvm=dsvm*vj
            else
               if(ep.le.epini) then
                  svm=0.d0
                  dsvm=1.d10
               else
                  svm=(ep-epini)/a1
                  dsvm=1.d0/svm
                  svm=svm**a3*vj
                  dsvm=svm*dsvm/a2
               endif
            endif
            dcopnew=(ftrial-c2*(fiso-fiso0)
     &           -umbb*(2.d0*cop+c4*(fkin-fkin0))-c2*svm)/
     &           (-c1*dfiso-umbb*(2.d0+c3*dfkin)-c1*dsvm)
         endif
         cop=cop-dcopnew
!
!        treatment of functions similar to dsqrt(cop) near cop=0
!
         if(cop.le.0.d0) then
            cop=max(0.d0,(cop+dcopnew)/2.d0)
            dcopnew=cop/2.d0
            if((dabs(dcopnew).lt.cop*1.d-4).or.
     &           (dabs(dcopnew).lt.1.d-10)) then
               exit
            else
               cycle
            endif
         endif
!
         if((dabs(dcopnew).lt.cop*1.d-4).or.
     &      (dabs(dcopnew).lt.1.d-10)) exit
!
!        check for endless iterations due to hysteresis behavior
!        of the Newton-Raphson algorithm
!
         if(dabs(dcopnew+dcopold).ge.1.d-10) then
            dcopold=dcopnew
         else
            cop1=cop
            cop2=cop+dcopnew
            cop=(cop1+cop2)/2.d0
            if(kode.eq.-51) then
               fu2=dcopnew*(-c1*dfiso-umbb*(2.d0+c3*dfkin))
            else
               fu2=dcopnew*(-c1*dfiso-umbb*(2.d0+c3*dfkin)-c1*dsvm)
            endif
            fu1=-fu2
            do
               ep=epl+c2*cop
!
               if(user_hardening) then
                  call uhardening(amat,iel,iint,t1l,epini,ep,dtime,
     &                 fiso,dfiso,fkin,dfkin)
                  fiso=fiso*vj
                  fkin=fkin*vj
               else
                  if(niso.ne.0) then
                     call ident(xiso,ep,niso,id)
                     if(id.eq.0) then
                        fiso=yiso(1)
                     elseif(id.eq.niso) then
                        fiso=yiso(niso)
                     else
                        dfiso=(yiso(id+1)-yiso(id))/
     &                        (xiso(id+1)-xiso(id))
                        fiso=yiso(id)+dfiso*(ep-xiso(id))
                     endif
                  elseif(nkin.ne.0) then
                     fiso=ykin(1)
                  else
                     fiso=0.d0
                  endif
!
                  if(nkin.ne.0) then
                     call ident(xkin,ep,nkin,id)
                     if(id.eq.0) then
                        fkin=ykin(1)
                     elseif(id.eq.nkin) then
                        fkin=ykin(nkin)
                     else
                        dfkin=(ykin(id+1)-ykin(id))/
     &                        (xkin(id+1)-xkin(id))
                        fkin=ykin(id)+dfkin*(ep-xkin(id))
                     endif
                  elseif(niso.ne.0) then
                     fkin=yiso(1)
                  else
                     fkin=0.d0
                  endif
               endif
!
               if(dabs(cop).lt.1.d-10) then
                  fiso0=fiso
                  fkin0=fkin
               endif
!
               if(kode.eq.-51) then
                  fu=(ftrial-c2*(fiso-fiso0)
     &                 -umbb*(2.d0*cop+c4*(fkin-fkin0)))
               else
                  if(user_creep) then
                     call ucreep(amat,iel,iint,t1l,epini,
     &                           ep,dtime,svm,dsvm)
                     svm=svm*vj
                  else
                     if(ep.le.epini) then
                        svm=0.d0
                     else
                        svm=(ep-epini)/a1
                        svm=svm**a3*vj
                     endif
                  endif
                  fu=(ftrial-c2*(fiso-fiso0)
     &                 -umbb*(2.d0*cop+c4*(fkin-fkin0))-c2*svm)
               endif
!
               if(fu*fu1.ge.0.d0) then
                  cop1=cop
                  fu1=fu
               else
                  cop2=cop
                  fu2=fu
               endif
               cop=(cop1+cop2)/2.d0
               dcop=(cop2-cop1)/2.d0
               if((dabs(dcop).lt.cop*1.d-4).or.
     &              (dabs(dcop).lt.1.d-10)) exit loop
            enddo
         endif
!
      enddo loop
!
!        updating the equivalent plastic strain
!
      epl=epl+c2*cop
!
!        updating the back stress
!
      c5=2.d0*umbb*cop/dxitril
      c6=c5/(3.d0*um)
      c7=c6*dfkin*vj23
      do i=1,6
         stbl(i)=stbl(i)+c7*xitril(i)
      enddo
!
!        updating the stress
!        vjj: Jacobian of the mechanical deformation gradient
!             (without temperature)
!        vj: Jacobian of the total deformation gradient
!
!        updating the stress
!
      c8=xk*(vjj*vjj-1.d0)/2.d0
!
      do i=1,6
         stre(i)=c8*ci(i)+beta(i)+stril(i)-c5*xitril(i)
      enddo
!
!        updating the plastic right Cauchy-Green tensor
!
      c9=c6*3.d0*vj23
      do i=1,6
         cpl(i)=cpl(i)-c9*xitril(i)
      enddo
!
      if(icmd.ne.3) then
!
!        calculating the local stiffness matrix
!
         xg(1,1)=(c(2)*c(3)-c(6)*c(6))/vj2
         xg(2,2)=(c(1)*c(3)-c(5)*c(5))/vj2
         xg(3,3)=(c(1)*c(2)-c(4)*c(4))/vj2
         xg(1,2)=(c(5)*c(6)-c(4)*c(3))/vj2
         xg(1,3)=(c(4)*c(6)-c(2)*c(5))/vj2
         xg(2,3)=(c(4)*c(5)-c(1)*c(6))/vj2
         xg(2,1)=xg(1,2)
         xg(3,1)=xg(1,3)
         xg(3,2)=xg(2,3)
!                                
         xs(1,1)=stril(1)
         xs(2,2)=stril(2)
         xs(3,3)=stril(3)
         xs(1,2)=stril(4)
         xs(2,1)=stril(4)
         xs(1,3)=stril(5)
         xs(3,1)=stril(5)
         xs(2,3)=stril(6)
         xs(3,2)=stril(6)
!
         f0=2.d0*umbb*cop/dxitril
         d0=1.d0+(dfkin/um+dfiso/umbb)/3.d0
!
!        creep contribution
!
         if(kode.eq.-52) then
            d0=d0+dsvm/(3.d0*umbb)
         endif
!
         f1=1.d0/d0-f0
         d1=2.d0*f1*umbb-((1.d0+dfkin/(3.d0*um))/d0-1.d0)*
     &      4.d0*cop*dxitril/3.d0
         d2=2d0*dxitril*f1
!
         xx(1,1)=xitril(1)
         xx(2,2)=xitril(2)
         xx(3,3)=xitril(3)
         xx(1,2)=xitril(4)
         xx(2,1)=xitril(4)
         xx(1,3)=xitril(5)
         xx(3,1)=xitril(5)
         xx(2,3)=xitril(6)
         xx(3,2)=xitril(6)
!
         xn(1,1)=xitril(1)/dxitril
         xn(2,2)=xitril(2)/dxitril
         xn(3,3)=xitril(3)/dxitril
         xn(1,2)=xitril(4)/dxitril
         xn(2,1)=xitril(4)/dxitril
         xn(1,3)=xitril(5)/dxitril
         xn(3,1)=xitril(5)/dxitril
         xn(2,3)=xitril(6)/dxitril
         xn(3,2)=xitril(6)/dxitril
!
         do i=1,3
            do j=i,3
               xd(i,j)=xn(i,1)*xn(1,j)*c(1)+xn(i,1)*xn(2,j)*c(4)+
     &                 xn(i,1)*xn(3,j)*c(5)+xn(i,2)*xn(1,j)*c(4)+
     &                 xn(i,2)*xn(2,j)*c(2)+xn(i,2)*xn(3,j)*c(6)+
     &                 xn(i,3)*xn(1,j)*c(5)+xn(i,3)*xn(2,j)*c(6)+
     &                 xn(i,3)*xn(3,j)*c(3)
            enddo
         enddo
         xd(2,1)=xd(1,2)
         xd(3,1)=xd(1,3)
         xd(3,2)=xd(2,3)
!
!        deviatoric part
!
         c1=(xd(1,1)*c(1)+xd(2,2)*c(2)+xd(3,3)*c(3)+
     &      2.d0*(xd(1,2)*c(4)+xd(1,3)*c(5)+xd(2,3)*c(6)))/3.d0
         do i=1,3
            do j=i,3
               xd(i,j)=xd(i,j)-c1*xg(i,j)
            enddo
         enddo
         xd(2,1)=xd(1,2)
         xd(3,1)=xd(1,3)
         xd(3,2)=xd(2,3)
!
         nt=0
         do i=1,21
            k=kk(nt+1)
            l=kk(nt+2)
            m=kk(nt+3)
            n=kk(nt+4)
            nt=nt+4
            elas(i)=(umb-f0*umbb)*(xg(k,m)*xg(l,n)+xg(k,n)*xg(l,m)-
     &        2.d0*xg(k,l)*xg(m,n)/3.d0)
     &        -2.d0*(xs(k,l)*xg(m,n)+xg(k,l)*xs(m,n))/3.d0
     &        +f0*2.d0*(xx(k,l)*xg(m,n)+xg(k,l)*xx(m,n))/3.d0
     &        -d1*xn(k,l)*xn(m,n)-d2*(xn(k,l)*xd(m,n)+
     &        xd(k,l)*xn(m,n))/2.d0+xk*vjj*vjj*xg(k,l)*xg(m,n)
     &        -xk*(vjj*vjj-1.d0)*(xg(k,m)*xg(l,n)+xg(k,n)*xg(l,m))/2.d0
         enddo
!
      endif
!
!        updating the plastic fields
!
      do i=1,6
         xstate(1+i,iint,iel)=cpl(i)
         xstate(7+i,iint,iel)=stbl(i)
      enddo
      xstate(1,iint,iel)=epl
!
      return
      end
