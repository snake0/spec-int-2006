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
      subroutine mafillpr(co,nk,kon,ipkon,lakon,
     &  ne,nodeboun,ndirboun,xboun,nboun,
     &  ipompc,nodempc,coefmpc,nmpc,nodeforc,ndirforc,xforc,
     &  nforc,nelemload,sideload,xload,nload,p1,p2,om,bodyf,
     &  ad,au,b,nactdof,icol,jq,neq,nmethod,ikmpc,ilmpc,ikboun,
     &  ilboun,elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,ielmat,
     &  ielorien,norien,orab,ntmat_,t0,t1,
     &  ithermal,prestr,iprestr,vold,iperturb,stx,sti,eei,iexpl,
     &  plicon,nplicon,plkcon,nplkcon,xstiff,npmat_,
     &  dtime,matname,mint_,ncmat_,mass,stiffness,buckling,rhs,
     &  intscheme)
!
!     filling the stiffness matrix in profile format
!
      implicit none
!
      logical mass,stiffness,buckling,rhs
!
      character*8 lakon(*)
      character*20 matname(*)
!
      integer kon(*),nodeboun(*),ndirboun(*),ipompc(*),nodempc(3,*),
     &  nodeforc(*),ndirforc(*),nelemload(2,*),icol(*),jq(*),ikmpc(*),
     &  ilmpc(*),ikboun(*),ilboun(*),nactdof(0:3,*),konl(20),
     &  nelcon(2,*),
     &  nrhcon(*),nalcon(2,*),ielmat(*),ielorien(*),ntmat_,ipkon(*)
!
      integer nk,ne,nboun,nmpc,nforc,nload,neq,nmethod,ithermal,
     &  iprestr,iperturb,i,j,k,l,m,ibod,idist,jdof1,jdof2,jj,ll,id,ist,
     &  ist1,ist2,index,index1,index2,mpc1,mpc2,id1,id2,ierror,node1,
     &  node2,idof1,idof2,idof3,jdof,kflag,indexe,nope,norien,iexpl,
     &  mint_,ncmat_,intscheme
!
      integer nplicon(0:ntmat_,*),nplkcon(0:ntmat_,*),npmat_,i0
!
      real*8 co(3,*),xboun(*),coefmpc(*),xforc(*),xload(2,*),p1(3),
     &  p2(3),ad(*),au(*),b(*),bodyf(3),elcon(0:ncmat_,ntmat_,*),
     &  rhcon(0:1,ntmat_,*),alcon(0:6,ntmat_,*),alzero(*),orab(7,*),
     &  t0(*),t1(*),prestr(6,*),vold(0:3,*),s(60,60),f(60),sm(60,60),
     &  stx(6,mint_,*),sti(6,mint_,*),eei(6,mint_,*),ff(60)
!
      real*8 plicon(0:2*npmat_,ntmat_,*),plkcon(0:2*npmat_,ntmat_,*),
     &  xstiff(21,mint_,*)
!
      real*8 om,value,dd,dtime
!
      character*5 sideload(*)
!
      kflag=2
      i0=0
!
!     initializing the matrices
!
      do i=1,neq
         ad(i)=0.d0
         b(i)=0.d0
      enddo
      do i=1,icol(neq)
         au(i)=0.d0
      enddo
!
!     check for the presence of body forces
!
      dd=dsqrt(bodyf(1)**2+bodyf(2)**2+bodyf(3)**2)
      if((dd.lt.1.d-20).and.(om.lt.1.d-20)) then
        ibod=0
      else
        ibod=1
      endif
!
!     distributed forces (body forces or thermal loads or
!     residual stresses or distributed face loads)
!
      if((ibod.ne.0).or.(ithermal.ne.0).or.
     &   (iprestr.ne.0).or.(nload.ne.0)) then
         idist=1
      else
         idist=0
      endif
!
!     loop over all elements
!
!     initialisation of the error parameter
!
      ierror=0
!
      do i=1,ne
!
        if(ipkon(i).lt.0) cycle
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
        do j=1,nope
          konl(j)=kon(indexe+j) 
        enddo
!
        call e_c3d(co,nk,konl,lakon(i),p1,p2,om,bodyf,ibod,s,sm,f,ff,
     &    i,nmethod,elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,
     &    ielmat,ielorien,norien,orab,ntmat_,
     &    t0,t1,ithermal,prestr,iprestr,vold,iperturb,
     &    nelemload,sideload,xload,nload,idist,sti,stx,eei,iexpl,
     &    plicon,
     &    nplicon,plkcon,nplkcon,xstiff,npmat_,dtime,
     &    matname,mint_,ncmat_,mass,stiffness,buckling,rhs,intscheme)
!
        do jj=1,3*nope
!
          j=(jj-1)/3+1
          k=jj-3*(j-1)
!
          node1=kon(indexe+j)
          jdof1=nactdof(k,node1)
!
          do ll=jj,3*nope
!
            l=(ll-1)/3+1
            m=ll-3*(l-1)
!
            node2=kon(indexe+l)
            jdof2=nactdof(m,node2)
!
!           check whether one of the DOF belongs to a SPC or MPC
!
            if((jdof1.ne.0).and.(jdof2.ne.0)) then
               call add_pr(au,ad,icol,jq,jdof1,jdof2,s(jj,ll),jj,ll)
            elseif((jdof1.ne.0).or.(jdof2.ne.0)) then
!
!              idof1: genuine DOF
!              idof2: nominal DOF of the SPC/MPC
!
               if(jdof1.eq.0) then
                  idof1=jdof2
                  idof2=(node1-1)*7+k
               else
                  idof1=jdof1
                  idof2=(node2-1)*7+m
               endif
               if(nmpc.gt.0) then
                  call nident(ikmpc,idof2,nmpc,id)
                  if((id.gt.0).and.(ikmpc(id).eq.idof2)) then
!
!                    regular DOF / MPC
!
                     id=ilmpc(id)
                     ist=ipompc(id)
                     index=nodempc(3,ist)
                     if(index.eq.0) cycle
                     do
                        idof2=nactdof(nodempc(2,index),nodempc(1,index))
                        value=-coefmpc(index)*s(jj,ll)/coefmpc(ist)
                        if(idof1.eq.idof2) value=2.d0*value
                        if(idof2.ne.0) then
                           call add_pr(au,ad,icol,jq,idof1,idof2,value,
     &                       i0,i0)
                        else
                           idof2=7*(nodempc(1,index)-1)+nodempc(2,index)
                           call nident(ikboun,idof2,nboun,id)
                           b(idof1)=b(idof1)-xboun(ilboun(id))*value
                        endif
                        index=nodempc(3,index)
                        if(index.eq.0) exit
                     enddo
                     cycle
                  endif
               endif
!
!              regular DOF / SPC
!
               call nident(ikboun,idof2,nboun,id)
               b(idof1)=b(idof1)-xboun(ilboun(id))*s(jj,ll)
            else
               idof1=(node1-1)*7+k
               idof2=(node2-1)*7+m
               mpc1=0
               mpc2=0
               if(nmpc.gt.0) then
                  call nident(ikmpc,idof1,nmpc,id1)
                  if((id1.gt.0).and.(ikmpc(id1).eq.idof1)) mpc1=1
                  call nident(ikmpc,idof2,nmpc,id2)
                  if((id2.gt.0).and.(ikmpc(id2).eq.idof2)) mpc2=1
               endif
               if((mpc1.eq.1).and.(mpc2.eq.1)) then
                  id1=ilmpc(id1)
                  id2=ilmpc(id2)
                  if(id1.eq.id2) then
!
!                    MPC id1 / MPC id1
!
                     ist=ipompc(id1)
                     index1=nodempc(3,ist)
                     if(index1.eq.0) cycle
                     do
                        idof1=nactdof(nodempc(2,index1),
     &                                nodempc(1,index1))
                        index2=index1
                        do
                           idof2=nactdof(nodempc(2,index2),
     &                                   nodempc(1,index2))
                           value=coefmpc(index1)*coefmpc(index2)*
     &                          s(jj,ll)/coefmpc(ist)/coefmpc(ist)
                           if((idof1.ne.0).and.(idof2.ne.0)) then
                              call add_pr(au,ad,icol,jq,idof1,idof2,
     &                                    value,i0,i0)
                           elseif((idof1.ne.0).or.(idof2.ne.0)) then
                              if(idof2.ne.0) then
                                 idof3=idof2
                                 idof2=7*(nodempc(1,index1)-1)+
     &                                 nodempc(2,index1)
                              else
                                 idof3=idof1
                                 idof2=7*(nodempc(1,index2)-1)+
     &                                 nodempc(2,index2)
                              endif
                              call nident(ikboun,idof2,nboun,id)
                              b(idof3)=b(idof3)-value*xboun(ilboun(id))
                           endif
!
                           index2=nodempc(3,index2)
                           if(index2.eq.0) exit
                        enddo
                        index1=nodempc(3,index1)
                        if(index1.eq.0) exit
                     enddo
                   else
!
!                    MPC id1 / MPC id2
!
                     ist1=ipompc(id1)
                     index1=nodempc(3,ist1)
                     if(index1.eq.0) cycle
                     do
                        idof1=nactdof(nodempc(2,index1),
     &                                nodempc(1,index1))
                        ist2=ipompc(id2)
                        index2=nodempc(3,ist2)
                        if(index2.eq.0) then
                           index1=nodempc(3,index1)
                           if(index1.eq.0) then
                              exit
                           else
                              cycle
                           endif
                        endif
                        do
                           idof2=nactdof(nodempc(2,index2),
     &                                   nodempc(1,index2))
                           value=coefmpc(index1)*coefmpc(index2)*
     &                          s(jj,ll)/coefmpc(ist1)/coefmpc(ist2)
                           if(idof1.eq.idof2) value=2.d0*value
                           if((idof1.ne.0).and.(idof2.ne.0)) then
                              call add_pr(au,ad,icol,jq,idof1,idof2,
     &                                    value,i0,i0)
                           elseif((idof1.ne.0).or.(idof2.ne.0)) then
                              if(idof2.ne.0) then
                                 idof3=idof2
                                 idof2=7*(nodempc(1,index1)-1)+
     &                                 nodempc(2,index1)
                              else
                                 idof3=idof1
                                 idof2=7*(nodempc(1,index2)-1)+
     &                                 nodempc(2,index2)
                              endif
                              call nident(ikboun,idof2,nboun,id)
                              b(idof3)=b(idof3)-value*xboun(ilboun(id))
                           endif
!
                           index2=nodempc(3,index2)
                           if(index2.eq.0) exit
                        enddo
                        index1=nodempc(3,index1)
                        if(index1.eq.0) exit
                     enddo
                  endif
               elseif((mpc1.eq.1).or.(mpc2.eq.1)) then
                  if(mpc1.eq.1) then
!
!                    MPC id1 / SPC
!
                     call nident(ikboun,idof2,nboun,id2)
                     idof2=ilboun(id2)
                     ist1=ipompc(id1)
                     index1=nodempc(3,ist1)
                     if(index1.eq.0) cycle
                     do
                        idof1=nactdof(nodempc(2,index1),
     &                                nodempc(1,index1))
                        if(idof1.ne.0) then
                           b(idof1)=b(idof1)+xboun(idof2)*
     &                      coefmpc(index1)*s(jj,ll)/coefmpc(ist1)
                        endif
                        index1=nodempc(3,index1)
                        if(index1.eq.0) exit
                     enddo
                  elseif(mpc2.eq.1) then
!
!                    MPC id2 / SPC
!
                     call nident(ikboun,idof1,nboun,id1)
                     idof1=ilboun(id1)
                     ist2=ipompc(id2)
                     index2=nodempc(3,ist2)
                     if(index2.eq.0) cycle
                     do
                        idof2=nactdof(nodempc(2,index2),
     &                                nodempc(1,index2))
                        if(idof2.ne.0) then
                           b(idof2)=b(idof2)+xboun(idof1)*
     &                      coefmpc(index2)*s(jj,ll)/coefmpc(ist2)
                        endif
                        index2=nodempc(3,index2)
                        if(index2.eq.0) exit
                     enddo
                  endif
               endif
            endif
          enddo
!
!         distributed forces
!
          if(idist.ne.0) then
            if(jdof1.eq.0) then
               if(nmpc.ne.0) then
                  idof1=(node1-1)*7+k
                  call nident(ikmpc,idof1,nmpc,id)
                  if((id.gt.0).and.(ikmpc(id).eq.idof1)) then
                     id=ilmpc(id)
                     ist=ipompc(id)
                     index=nodempc(3,ist)
                     if(index.eq.0) cycle
                     do
                        jdof1=nactdof(nodempc(2,index),
     &                                nodempc(1,index))
                        if(jdof1.ne.0) then
                           b(jdof1)=b(jdof1)-coefmpc(index)*f(jj)/
     &                              coefmpc(ist)
                        endif
                        index=nodempc(3,index)
                        if(index.eq.0) exit
                     enddo
                  endif
               endif
               cycle
            endif
            b(jdof1)=b(jdof1)+f(jj)
          endif
        enddo
      enddo
!
!     point forces
!      
      do i=1,nforc
        jdof=nactdof(ndirforc(i),nodeforc(i))
        if(jdof.ne.0) then
          b(jdof)=b(jdof)+xforc(i)
        endif
      enddo
!
      if(ierror.ne.0) then
         write(*,*) '*ERROR: the error parameter is nonzero'
         write(*,*) '        fatal error; stop'
         stop
      endif
!
      return
      end













