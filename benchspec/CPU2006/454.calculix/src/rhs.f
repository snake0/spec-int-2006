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
      subroutine rhs(co,nk,kon,ipkon,lakon,ne,
     &  ipompc,nodempc,coefmpc,nmpc,nodeforc,ndirforc,xforc,
     &  nforc,nelemload,sideload,xload,nload,p1,p2,om,bodyf,
     &  bb,nactdof,neq,nmethod,
     &  ikmpc,ilmpc,elcon,nelcon,rhcon,nrhcon,alcon,
     &  nalcon,alzero,ielmat,ielorien,norien,orab,ntmat_,t0,t1,ithermal,
     &  prestr,iprestr,vold,iperturb,iexpl,plicon,
     &  nplicon,plkcon,nplkcon,npmat_)
!
!     filling the right hand side load vector b
!
!     b contains the contributions due to mechanical forces only
!
      implicit none
!
      character*5 sideload(*)
      character*8 lakon(*)
!
      integer kon(*),ipompc(*),nodempc(3,*),
     &  nodeforc(*),ndirforc(*),nelemload(2,*),ikmpc(*),
     &  ilmpc(*),nactdof(0:3,*),konl(20),nelcon(2,*),
     &  nrhcon(*),nalcon(2,*),ielmat(*),ielorien(*),ipkon(*)
!
      integer nk,ne,nmpc,nforc,nload,neq,nmethod,
     &  ithermal,iprestr,iperturb,i,j,k,ibod,idist,jj,
     &  id,ist,index,jdof1,jdof,node1,ntmat_,indexe,nope,norien,
     &  iexpl,idof1
!
      integer nplicon(0:ntmat_,*),nplkcon(0:ntmat_,*),npmat_
!
      real*8 co(3,*),coefmpc(*),xforc(*),xload(2,*),p1(3),
     &  p2(3),bb(*),bodyf(3),elcon(0:21,ntmat_,*),rhcon(0:1,ntmat_,*),
     &  alcon(0:6,ntmat_,*),alzero(*),orab(7,*),
     &  t0(*),t1(*),prestr(6,*),vold(0:3,*),ff(60)
!
      real*8 plicon(0:2*npmat_,ntmat_,*),plkcon(0:2*npmat_,ntmat_,*)
!
      real*8 om,dd
!
      do i=1,neq
         bb(i)=0.d0
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
!        distributed forces (body forces or thermal loads or
!        residual stresses or distributed face loads)
!
      if((ibod.ne.0).or.(ithermal.ne.0).or.
     &     (iprestr.ne.0).or.(nload.ne.0)) then
         idist=1
      else
         idist=0
      endif
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
         call e_c3d_rhs(co,nk,konl,lakon(i),p1,p2,om,bodyf,ibod,
     &        ff,i,nmethod,rhcon,ielmat,ntmat_,vold,iperturb,
     &        nelemload,sideload,xload,nload,idist)
!
         do jj=1,3*nope
!
            j=(jj-1)/3+1
            k=jj-3*(j-1)
!
            node1=kon(indexe+j)
            jdof1=nactdof(k,node1)
!
!            distributed forces
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
                        do
                           jdof1=nactdof(nodempc(2,index),
     &                          nodempc(1,index))
                           if(jdof1.ne.0) then
                              bb(jdof1)=bb(jdof1)-coefmpc(index)*ff(jj)/
     &                             coefmpc(ist)
                           endif
                           index=nodempc(3,index)
                           if(index.eq.0) exit
                        enddo
                     endif
                  endif
                  cycle
               endif
               bb(jdof1)=bb(jdof1)+ff(jj)
            endif
!
         enddo
      enddo
!
!        point forces
!      
      do i=1,nforc
         jdof=nactdof(ndirforc(i),nodeforc(i))
         if(jdof.ne.0) then
            bb(jdof)=bb(jdof)+xforc(i)
         endif
      enddo
!
      return
      end
