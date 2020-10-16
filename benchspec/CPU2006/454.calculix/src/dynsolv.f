!
!     CalculiX - 3-dimensional finite element program
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
      subroutine dynsolv(b,z,d,zeta,nev,neq,tinc,jinc,jout,vold,
     &  xforcold,nodeforc,ndirforc,xforc,iamforc,nforc,xloadold,xload,
     &  iamload,nelemload,sideload,nload,omold,om,iamom,bodyfold,bodyf,
     &  iambodyf,t1old,t1,iamt1,nk,amta,namta,nam,ampli,aa,bb,bj,v,
     &  nodeboun,ndirboun,xboun,nboun,ipompc,nodempc,coefmpc,labmpc,
     &  nmpc,
     &  nactdof,nodeprint,noprint,iperturb,nmethod,co,kon,ipkon,lakon,
     &  ne,stn,
     &  nelemprint,neprint,stx,elcon,nelcon,rhcon,nrhcon,alcon,
     &  nalcon,alzero,ielmat,ielorien,norien,orab,ntmat_,
     &  t0,ithermal,kode,cv,cd,inum,prestr,iprestr,
     &  ikmpc,ilmpc,ikboun,ilboun,p1,p2,noelplab,nodeflab,
     &  eei,een,sti,f,fn,xforcact,xloadact,omact,bodyfact,t1act,
     &  xbounold,xbounact,iamboun,iexpl,plicon,nplicon,plkcon,nplkcon,
     &  xstateini,xstiff,xstate,npmat_,epn,matname,mint_,ncmat_,nstate_,
     &  stiini,vini,ener,enern,xstaten,ttime,eeiini,enerini,cocon,
     &  ncocon)
!
!     dynamic solution using modal decomposition
!
      implicit none
!
      character*4 noelplab(*),nodeflab(*)
      character*5 sideload(*)
      character*8 lakon(*)
      character*20 matname(*),labmpc(*)
!
      integer jinc,iamforc(*),nforc,iamload(2,*),nload,iamom,iambodyf,
     &  iamt1(*),nk,namta(3,*),nam,i,j,nodeboun(*),ndirboun(*),nboun,
     &  ipompc(*),nmpc,nactdof(0:3,*),nodeprint(*),noprint,iperturb,
     &  nmethod,kon(*),ne,nelemprint(*),neprint,ithermal,
     &  kode,k,l,m,jout,nodempc(3,*),nev,neq,inum(*),iprestr,
     &  ikmpc(*),ilmpc(*),ikboun(*),ilboun(*),nelcon(2,*),nrhcon(*),
     &  ndirforc(*),nelemload(2,*),nodeforc(*),nalcon(2,*),ielmat(*),
     &  ielorien(*),ntmat_,ipkon(*),norien,iout,iamboun(*),iexpl,
     &  mint_,ielas,icmd,ncmat_,nstate_
!
      integer nplicon(0:ntmat_,*),nplkcon(0:ntmat_,*),npmat_,ncocon(2,*)
!
      real*8 b(*),z(neq,*),d(*),zeta(*),time,tinc,vold(0:3,*),
     &  xforcold(*),xforc(*),xloadold(2,*),xload(2,*),omold,om,
     &  bodyfold(3),
     &  bodyf(3),t1old(*),t1(*),amta(2,*),ampli(*),aa(nev,*),bb(nev,*),
     &  v(0:3,*),xboun(*),coefmpc(*),co(3,*),stn(6,*),
     &  stx(6,mint_,*),elcon(0:ncmat_,ntmat_,*),rhcon(0:1,ntmat_,*),
     &  t0(*),h1,h2,h3,h4,h5,h6,sum,tstart,alcon(0:6,ntmat_,*),
     &  tend,aai,bbi,bj(*),cv(*),cd(*),zetaj,dj,fsub,fsuper,fcrit,ddj,
     &  prestr(6,*),p1(3),p2(3),eei(3,8,*),een(6,*),epn(*),
     &  sti(6,8,*),alzero(*),orab(7,*),vmax,f(*),fn(0:3,*),qa,
     &  xforcact(*),xloadact(2,*),omact,bodyfact(3),t1act(*),reltime,
     &  xbounold(*),xbounact(*),stiini(6,mint_,*),vini(0:3,*),
     &  ener(mint_,*),enern(*),eeiini(6,mint_,*),enerini(mint_,*),
     &  cocon(0:6,ntmat_,*)
!
      real*8 plicon(0:2*npmat_,ntmat_,*),plkcon(0:2*npmat_,ntmat_,*),
     &  xstate(nstate_,mint_,*),xstateini(nstate_,mint_,*),
     &  xstiff(21,mint_,*),xstaten(nstate_,*)
!
!     the following fields are dummy arguments for the results call
!
      real*8 veold,accold,bet,gam,dtime,ttime
!
      external fsub,fsuper,fcrit
!
      time=0.d0
!
!     calculating the instantaneous loads (forces, surface loading, 
!     centrifugal and gravity loading or temperature) at time 0
!
      call tempload(xforcold,xforc,xforcact,iamforc,nforc,xloadold,
     &  xload,xloadact,iamload,nload,omold,om,omact,iamom,bodyfold,
     &  bodyf,bodyfact,iambodyf,t1old,t1,t1act,iamt1,nk,
     &  amta,namta,nam,ampli,time,reltime,ttime,dtime,ithermal,nmethod,
     &  xbounold,xboun,xbounact,iamboun,nboun)
!
!     calculating the instantaneous loading vector at time 0
!
      call rhs(co,nk,kon,ipkon,lakon,ne,
     &  ipompc,nodempc,coefmpc,nmpc,nodeforc,ndirforc,xforcact,
     &  nforc,nelemload,sideload,xloadact,nload,p1,p2,omact,bodyfact,
     &  b,nactdof,neq,nmethod,
     &  ikmpc,ilmpc,elcon,nelcon,rhcon,nrhcon,alcon,
     &  nalcon,alzero,ielmat,ielorien,norien,orab,ntmat_,t0,t1act,
     &  ithermal,
     &  prestr,iprestr,vold,iperturb,iexpl,plicon,
     &  nplicon,plkcon,nplkcon,npmat_)
!
      do i=1,nev
         aa(i,1)=0.d0
         do j=1,neq
            aa(i,1)=aa(i,1)+z(j,i)*b(j)
         enddo
      enddo
!
      do k=0,jinc-jout,jout
         do l=1,jout
            m=k+l
            time=m*tinc
            dtime=time
!
!     calculating the instantaneous loads (forces, surface loading, 
!     centrifugal and gravity loading or temperature)
!
            call tempload(xforcold,xforc,xforcact,iamforc,nforc,
     &           xloadold,xload,xloadact,iamload,nload,omold,om,omact,
     &           iamom,bodyfold,bodyf,bodyfact,iambodyf,t1old,t1,t1act,
     &           iamt1,nk,amta,namta,nam,ampli,time,
     &           reltime,ttime,dtime,ithermal,nmethod,
     &           xbounold,xboun,xbounact,iamboun,nboun)
!
!     calculating the instantaneous loading vector
!
            call rhs(co,nk,kon,ipkon,lakon,ne,
     &           ipompc,nodempc,coefmpc,nmpc,nodeforc,ndirforc,xforcact,
     &           nforc,nelemload,sideload,xloadact,nload,p1,p2,omact,
     &           bodyfact,b,nactdof,neq,nmethod,
     &           ikmpc,ilmpc,elcon,nelcon,rhcon,nrhcon,
     &           alcon,nalcon,alzero,ielmat,ielorien,norien,orab,ntmat_,
     &           t0,t1act,ithermal,prestr,
     &           iprestr,vold,iperturb,iexpl,plicon,
     &           nplicon,plkcon,nplkcon,
     &           npmat_)
!
            do i=1,nev
               aa(i,m+1)=0.d0
               do j=1,neq
                  aa(i,m+1)=aa(i,m+1)+z(j,i)*b(j)
               enddo
               bb(i,m)=(aa(i,m+1)-aa(i,m))/tinc
               aa(i,m)=aa(i,m+1)-bb(i,m)*time
            enddo
         enddo
!
         do l=1,nev
            zetaj=zeta(l)
            dj=d(l)
!
!          subcritical damping
!
            if(zetaj.lt.1.d0-1.d-6) then
               ddj=dj*dsqrt(1.d0-zetaj*zetaj)
               h1=zetaj*dj
               h2=h1*h1+ddj*ddj
               h3=h1*h1-ddj*ddj
               h4=2.d0*h1*ddj/h2
               sum=0.d0
               do i=1,m
                  aai=aa(l,i)
                  bbi=bb(l,i)
                  tstart=time-i*tinc
                  tend=time-(i-1)*tinc
                  sum=sum+fsub(time,tend,aai,bbi,ddj,
     &                 h1,h2,h3,h4)
                  sum=sum-fsub(time,tstart,aai,bbi,ddj,
     &                 h1,h2,h3,h4)
               enddo
               bj(l)=sum/ddj+dexp(-h1*time)*(dcos(ddj*time)+zetaj/
     &              dsqrt(1.d0-zetaj*zetaj)*dsin(ddj*time))*cd(l)+
     &              dexp(-h1*time)*dsin(ddj*time)*cv(l)/ddj
!
!          supercritical damping
!
            elseif(zetaj.gt.1.d0+1.d-6) then
               ddj=dj*dsqrt(zetaj*zetaj-1.d0)
               h1=ddj-zetaj*dj
               h2=ddj+zetaj*dj
               h3=1.d0/h1
               h4=1.d0/h2
               h5=h3*h3
               h6=h4*h4
               sum=0.d0
               do i=1,m
                  aai=aa(l,i)
                  bbi=bb(l,i)
                  tstart=time-i*tinc
                  tend=time-(i-1)*tinc
                  sum=sum+fsuper(time,tend,aai,bbi,
     &                 h1,h2,h3,h4,h5,h6)
                  sum=sum-fsuper(time,tstart,aai,bbi,
     &                 h1,h2,h3,h4,h5,h6)
               enddo
               bj(l)=sum/(2.d0*ddj)+(dexp(h1*time)+dexp(-h2*time))*cd(l)
     &              /2.d0+zetaj*(dexp(h1*time)-dexp(-h2*time))/(2.d0*
     &              dsqrt(zetaj*zetaj-1.d0))*cd(l)+(dexp(h1*time)-
     &              dexp(-h2*time))*cv(l)/(2.d0*ddj)
!
!             critical damping
!
            else
               h1=zetaj*dj
               h2=1.d0/h1
               h3=h2*h2
               h4=h2*h3
               sum=0.d0
               do i=1,m
                  aai=aa(l,i)
                  bbi=bb(l,i)
                  tstart=time-i*tinc
                  tend=time-(i-1)*tinc
                  sum=sum+fcrit(time,tend,aai,bbi,zetaj,dj,ddj,
     &                 h1,h2,h3,h4)
                  sum=sum-fcrit(time,tstart,aai,bbi,zetaj,dj,ddj,
     &                 h1,h2,h3,h4)
               enddo
               bj(l)=sum+dexp(-h1*time)*
     &              ((1.d0+h1*time)*cd(l)+time*cv(l))
            endif
         enddo
!
!       composing the response
!        
         do i=1,neq
            b(i)=0.d0
            do j=1,nev
               b(i)=b(i)+bj(j)*z(i,j)
            enddo
         enddo
!
         iout=1
!
         call results(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nelemprint,
     &        neprint,
     &        stx,elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,
     &        ielmat,ielorien,norien,orab,ntmat_,t0,t1,
     &        ithermal,prestr,iprestr,noelplab,nodeflab,eei,een,
     &        iperturb,f,fn,nactdof,iout,qa,noprint,
     &        nodeprint,vold,b,nodeboun,ndirboun,xboun,nboun,
     &        ipompc,nodempc,coefmpc,labmpc,nmpc,nmethod,vmax,neq,
     &        veold,accold,bet,gam,dtime,plicon,nplicon,plkcon,nplkcon,
     &        xstateini,xstiff,xstate,npmat_,epn,matname,mint_,ielas,
     &        icmd,ncmat_,nstate_,stiini,vini,ikboun,ilboun,ener,
     &        enern,sti,xstaten,eeiini,enerini,cocon,ncocon)
!
         kode=kode+1
!
         call frd(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nmethod,kode,
     &     nodeflab,een,t1,fn,time,epn,ielmat,matname,enern,xstaten,
     &     nstate_)
!
      enddo
!
      return
      end
