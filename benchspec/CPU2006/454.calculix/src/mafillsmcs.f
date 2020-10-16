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
      subroutine mafillsmcs(co,nk,kon,ipkon,lakon,ne,nodeboun,ndirboun,
     &  xboun,nboun,
     &  ipompc,nodempc,coefmpc,nmpc,nodeforc,ndirforc,xforc,
     &  nforc,nelemload,sideload,xload,nload,p1,p2,om,bodyf,
     &  ad,au,b,bb,nactdof,icol,jq,irow,neq,nzl,nmethod,
     &  ikmpc,ilmpc,ikboun,ilboun,elcon,nelcon,rhcon,
     &  nrhcon,alcon,nalcon,alzero,ielmat,ielorien,norien,orab,ntmat_,
     &  t0,t1,ithermal,prestr,
     &  iprestr,vold,iperturb,sti,nzs,stx,adb,aub,eei,iexpl,plicon,
     &  nplicon,plkcon,nplkcon,xstiff,npmat_,dtime,
     &  matname,mint_,ics,ns,nm,ncmat_,labmpc,mass,stiffness,buckling,
     &  rhs,intscheme)
!
!     filling the stiffness matrix in spare matrix format (sm)
!     for cyclic symmetry calculations
!
      implicit none
!
      logical mass,stiffness,buckling,rhs
!
      character*8 lakon(*)
      character*20 matname(*),labmpc(*)
!
      integer kon(*),nodeboun(*),ndirboun(*),ipompc(*),nodempc(3,*),
     &  nodeforc(*),ndirforc(*),nelemload(2,*),icol(*),jq(*),ikmpc(*),
     &  ilmpc(*),ikboun(*),ilboun(*),nactdof(0:3,*),konl(20),irow(*),
     &  nelcon(2,*),nrhcon(*),nalcon(2,*),ielmat(*),ielorien(*),
     &  ipkon(*),ns(5),ics(*)
!
      integer nk,ne,nboun,nmpc,nforc,nload,neq,nzl,nmethod,
     &  ithermal,iprestr,iperturb,nzs,i,j,k,l,m,ibod,idist,jj,
     &  ll,id,id1,id2,ist,ist1,ist2,index,jdof1,jdof2,idof1,idof2,
     &  mpc1,mpc2,index1,index2,node1,node2,kflag,
     &  ntmat_,indexe,nope,norien,iexpl,mint_,i0,nm,inode,icomplex,
     &  inode1,icomplex1,inode2,icomplex2,ner,ncmat_,intscheme
!
      integer nplicon(0:ntmat_,*),nplkcon(0:ntmat_,*),npmat_
!
      real*8 co(3,*),xboun(*),coefmpc(*),xforc(*),xload(2,*),p1(3),
     &  p2(3),ad(*),au(*),b(*),bodyf(3),bb(*),
     &  t0(*),t1(*),prestr(6,*),vold(0:3,*),s(60,60),f(60),ff(60),
     &  sti(6,mint_,*),sm(60,60),stx(6,mint_,*),adb(*),aub(*),
     &  elcon(0:ncmat_,ntmat_,*),rhcon(0:1,ntmat_,*),
     &  alcon(0:6,ntmat_,*),
     &  alzero(*),orab(7,*),eei(6,mint_,*)
!
      real*8 plicon(0:2*npmat_,ntmat_,*),plkcon(0:2*npmat_,ntmat_,*),
     &  xstiff(21,mint_,*),
     &  pi,theta,ct,st,c2t,s2t
!
      real*8 om,valu2,value,dtime,walue,walu2
!
      character*5 sideload(*)
!
!     calculating the scaling factors for the cyclic symmetry calculation
!
c      do i=1,nmpc
c         write(*,*) i,labmpc(i)
c         index=ipompc(i)
c         do
c            write(*,'(i5,1x,i5,1x,e11.4)') nodempc(1,index),
c     &        nodempc(2,index),coefmpc(index)
c            index=nodempc(3,index)
c            if(index.eq.0) exit
c         enddo
c         write(*,*)
c      enddo
!
      pi=4.d0*datan(1.d0)
      theta=nm*2.d0*pi/ns(1)
      ct=dcos(theta)
      st=dsin(theta)
      c2t=dcos(2.d0*theta)
      s2t=dsin(2.d0*theta)
!
      kflag=2
      i0=0
!
!     determining nzl and nzs
!
      do i=neq,1,-1
         if(icol(i).gt.0) then
            nzl=i
            exit
         endif
      enddo
!
!     initializing the matrices
!
      do i=1,neq
         ad(i)=0.d0
      enddo
      do i=1,nzs
         au(i)=0.d0
      enddo
!
      do i=1,neq
         adb(i)=0.d0
      enddo
      do i=1,nzs
         aub(i)=0.d0
      enddo
!
      ner=neq/2
!
!     loop over all elements
!
!     initialisation of the error parameter
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
        call e_c3d(co,nk,konl,lakon(i),p1,p2,om,bodyf,ibod,s,sm,f,ff,i,
     &          nmethod,elcon,nelcon,rhcon,nrhcon,alcon,nalcon,
     &          alzero,ielmat,ielorien,norien,orab,ntmat_,
     &          t0,t1,ithermal,prestr,iprestr,vold,iperturb,
     &          nelemload,sideload,xload,nload,idist,sti,stx,
     &          eei,iexpl,plicon,
     &          nplicon,plkcon,nplkcon,xstiff,npmat_,
     &          dtime,matname,mint_,ncmat_,mass,stiffness,buckling,rhs,
     &          intscheme)
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
               call add_sm_ei(au,ad,aub,adb,jq,irow,jdof1,jdof2,
     &              s(jj,ll),sm(jj,ll),jj,ll)
               call add_sm_ei(au,ad,aub,adb,jq,irow,jdof1+ner,jdof2+ner,
     &              s(jj,ll),sm(jj,ll),jj,ll)
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
!
               if(nmpc.gt.0) then
                  call nident(ikmpc,idof2,nmpc,id)
                  if((id.gt.0).and.(ikmpc(id).eq.idof2)) then
!
!                    regular DOF / MPC
!
                     id1=ilmpc(id)
                     ist=ipompc(id1)
                     index=nodempc(3,ist)
                     if(index.eq.0) cycle
                     do
                        inode=nodempc(1,index)
c                        call nident(ics,inode,ns(4),id)
                        icomplex=0
c                        write(*,*) id1,labmpc(id1)(1:9)
                        if(labmpc(id1)(1:6).eq.'CYCLIC') then
                           icomplex=1
                        elseif(labmpc(id1)(1:9).eq.'SUBCYCLIC') then
                           call nident(ics,inode,ns(4),id)
                           if(id.gt.0) then
                              if(ics(id).eq.inode) then
                                 icomplex=1
                              endif
                           endif
                        endif
                        idof2=nactdof(nodempc(2,index),inode)
                        if(idof2.ne.0) then
                           value=-coefmpc(index)*s(jj,ll)/coefmpc(ist)
                           valu2=-coefmpc(index)*sm(jj,ll)/
     &                          coefmpc(ist)
                           if(idof1.eq.idof2) then
                              value=2.d0*value
                              valu2=2.d0*valu2
                           endif
                           if(icomplex.eq.0) then
                              call add_sm_ei(au,ad,aub,adb,jq,irow,
     &                            idof1,idof2,value,valu2,i0,i0)
                              call add_sm_ei(au,ad,aub,adb,jq,irow,
     &                            idof1+ner,idof2+ner,value,valu2,i0,i0)
                           else
                              walue=value*ct
                              walu2=valu2*ct
                              call add_sm_ei(au,ad,aub,adb,jq,irow,
     &                             idof1,idof2,walue,walu2,i0,i0)
                              call add_sm_ei(au,ad,aub,adb,jq,irow,
     &                            idof1+ner,idof2+ner,walue,walu2,i0,i0)
                              if(idof1.ne.idof2) then
                                 walue=value*st
                                 walu2=valu2*st
                                 call add_sm_ei(au,ad,aub,adb,jq,irow,
     &                                idof1,idof2+ner,walue,walu2,i0,i0)
                                 walue=-walue
                                 walu2=-walu2
                                 call add_sm_ei(au,ad,aub,adb,jq,irow,
     &                                idof1+ner,idof2,walue,walu2,i0,i0)
                              endif
                           endif
                        endif
                        index=nodempc(3,index)
                        if(index.eq.0) exit
                     enddo
                     cycle
                  endif
               endif
!
            else
               idof1=(node1-1)*7+k
               idof2=(node2-1)*7+m
!
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
                        inode1=nodempc(1,index1)
c                        call nident(ics,inode1,ns(4),id)
                        icomplex1=0
                        if(labmpc(id1)(1:6).eq.'CYCLIC') then
                           icomplex1=1
                        elseif(labmpc(id1)(1:9).eq.'SUBCYCLIC') then
                           call nident(ics,inode1,ns(4),id)
                           if(id.gt.0) then
                              if(ics(id).eq.inode1) then
                                 icomplex1=1
                              endif
                           endif
                        endif
                        idof1=nactdof(nodempc(2,index1),inode1)
                        index2=index1
                        do
                           inode2=nodempc(1,index2)
c                           call nident(ics,inode2,ns(4),id)
                           icomplex2=0
                           if(labmpc(id1)(1:6).eq.'CYCLIC') then
                              icomplex2=1
                           elseif(labmpc(id1)(1:9).eq.'SUBCYCLIC') then
                              call nident(ics,inode2,ns(4),id)
                              if(id.gt.0) then
                                 if(ics(id).eq.inode2) then
                                    icomplex2=1
                                 endif
                              endif
                           endif
                           idof2=nactdof(nodempc(2,index2),inode2)
                           if((idof1.ne.0).and.(idof2.ne.0)) then
                              value=coefmpc(index1)*coefmpc(index2)*
     &                             s(jj,ll)/coefmpc(ist)/coefmpc(ist)
                              valu2=coefmpc(index1)*coefmpc(index2)*
     &                             sm(jj,ll)/coefmpc(ist)/coefmpc(ist)
                              if(((icomplex1.eq.0).and.
     &                          (icomplex2.eq.0)).or.
     &                          ((icomplex1.ne.0).and.
     &                           (icomplex2.ne.0))) then
                                 call add_sm_ei(au,ad,aub,adb,jq,
     &                            irow,idof1,idof2,value,valu2,i0,i0)
                                 call add_sm_ei(au,ad,aub,adb,jq,
     &                                irow,idof1+ner,idof2+ner,value,
     &                                valu2,i0,i0)
                              elseif((icomplex1.eq.0).or.
     &                          (icomplex2.eq.0)) then
                                 walue=value*ct
                                 walu2=valu2*ct
                                 call add_sm_ei(au,ad,aub,adb,jq,irow,
     &                                idof1,idof2,walue,walu2,i0,i0)
                                 call add_sm_ei(au,ad,aub,adb,jq,irow,
     &                            idof1+ner,idof2+ner,walue,walu2,i0,i0)
                                 if(icomplex2.ne.0) then
                                    walue=value*st
                                    walu2=valu2*st
                                 else
                                    walue=-value*st
                                    walu2=-valu2*st
                                 endif
c                                    walue=value*st
c                                    walu2=valu2*st
                                 call add_sm_ei(au,ad,aub,adb,jq,irow,
     &                                idof1,idof2+ner,walue,walu2,i0,i0)
                                 walue=-walue
                                 walu2=-walu2
                                 call add_sm_ei(au,ad,aub,adb,jq,irow,
     &                                idof1+ner,idof2,walue,walu2,i0,i0)
                              endif
                           endif
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
                        inode1=nodempc(1,index1)
c                        call nident(ics,inode1,ns(4),id)
                        icomplex1=0
                        if(labmpc(id1)(1:6).eq.'CYCLIC') then
                           icomplex1=1
                        elseif(labmpc(id1)(1:9).eq.'SUBCYCLIC') then
                           call nident(ics,inode1,ns(4),id)
                           if(id.gt.0) then
                              if(ics(id).eq.inode1) then
                                 icomplex1=1
                              endif
                           endif
                        endif
                        idof1=nactdof(nodempc(2,index1),inode1)
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
                           inode2=nodempc(1,index2)
c                           call nident(ics,inode2,ns(4),id)
                           icomplex2=0
                           if(labmpc(id2)(1:6).eq.'CYCLIC') then
                              icomplex2=1
                           elseif(labmpc(id2)(1:9).eq.'SUBCYCLIC') then
                              call nident(ics,inode2,ns(4),id)
                              if(id.gt.0) then
                                 if(ics(id).eq.inode2) then
                                    icomplex2=1
                                 endif
                              endif
                           endif
                           idof2=nactdof(nodempc(2,index2),inode2)
                           if((idof1.ne.0).and.(idof2.ne.0)) then
                              value=coefmpc(index1)*coefmpc(index2)*
     &                             s(jj,ll)/coefmpc(ist1)/coefmpc(ist2)
                              valu2=coefmpc(index1)*coefmpc(index2)*
     &                             sm(jj,ll)/coefmpc(ist1)/coefmpc(ist2)
                              if(idof1.eq.idof2) then
                                 value=2.d0*value
                                 valu2=2.d0*valu2
                              endif
                              if(((icomplex1.eq.0).and.
     &                          (icomplex2.eq.0)).or.
     &                          ((icomplex1.ne.0).and.
     &                           (icomplex2.ne.0))) then
                                 call add_sm_ei(au,ad,aub,adb,jq,
     &                            irow,idof1,idof2,value,valu2,i0,i0)
                                 call add_sm_ei(au,ad,aub,adb,jq,
     &                                irow,idof1+ner,idof2+ner,value,
     &                                valu2,i0,i0)
                              elseif((icomplex1.eq.0).or.
     &                          (icomplex2.eq.0)) then
                                 walue=value*ct
                                 walu2=valu2*ct
                                 call add_sm_ei(au,ad,aub,adb,jq,irow,
     &                                idof1,idof2,walue,walu2,i0,i0)
                                 call add_sm_ei(au,ad,aub,adb,jq,irow,
     &                            idof1+ner,idof2+ner,walue,walu2,i0,i0)
                                 if(idof1.ne.idof2) then
                                    if(icomplex2.ne.0) then
                                       walue=value*st
                                       walu2=valu2*st
                                    else
                                       walue=-value*st
                                       walu2=-valu2*st
                                    endif
c                                       walue=value*st
c                                       walu2=valu2*st
                                    call add_sm_ei(au,ad,aub,adb,jq,
     &                                   irow,idof1,idof2+ner,walue,
     &                                   walu2,i0,i0)
                                    walue=-walue
                                    walu2=-walu2
                                    call add_sm_ei(au,ad,aub,adb,jq,
     &                                   irow,idof1+ner,idof2,walue,
     &                                   walu2,i0,i0)
                                 endif
                              endif
                           endif
                           index2=nodempc(3,index2)
                           if(index2.eq.0) exit
                        enddo
                        index1=nodempc(3,index1)
                        if(index1.eq.0) exit
                     enddo
                  endif
               endif
            endif
          enddo
!
        enddo
      enddo
!
      return
      end
