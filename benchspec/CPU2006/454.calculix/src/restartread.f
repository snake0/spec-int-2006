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
      subroutine restartread(istep,nset,nload,nforc, nboun,nk,ne,
     &  nmpc,nalset,nmat,ntmat_,npmat_,norien,nam,noprint,neprint,mint_,
     &  ntrans,ncs_,namtot_,ncmat_,memmpc_,ne1d,ne2d,nflow,nlabel,iplas,
     &  nkon,ithermal,nmethod,iperturb,nstate_,nener,set,istartset,
     &  iendset,ialset,co,kon,ipkon,lakon,nodeboun,ndirboun,iamboun,
     &  xboun,ikboun,ilboun,ipompc,nodempc,coefmpc,labmpc,ikmpc,ilmpc,
     &  nodeforc,ndirforc,iamforc,xforc,ikforc,ilforc,nelemload,iamload,
     &  sideload,xload,nodeprint,nelemprint,elcon,nelcon,rhcon,nrhcon,
     &  alcon,nalcon,alzero,plicon,nplicon,plkcon,nplkcon,orname,orab,
     &  ielorien,trab,inotr,amname,amta,namta,t0,t1,iamt1,veold,
     &  ielmat,matname,noelplab,nodeflab,vold,nodebounold,
     &  ndirbounold,xbounold,xforcold,xloadold,t1old,eei,xflowold,
     &  iponor,xnor,knor,thickn,thicke,offset,iponoel,inoel,rig,
     &  nodeflow,iamflow,xflow,shcon,nshcon,cocon,ncocon,ics,sti,
     &  ener,xstate,jobnamec,infree,nnn,irestartstep,prestr,iprestr,p1,
     &  p2,om,iamom,omold,bodyf,iambodyf,bodyfold,ttime,qaold,ns,csab,
     &  output,physcon,ctrl)
!
      implicit none
!
      character*3 output
      character*4 noelplab(*),nodeflab(*)
      character*5 sideload(*)
      character*8 lakon(*)
      character*20 labmpc(*),orname(*),amname(*),matname(*)
      character*21 set(*)
      character*132 fnrstrt,jobnamec
!
      integer istep,nset,nload,nforc,nboun,nk,ne,nmpc,nalset,nmat,
     &  ntmat_,npmat_,norien,nam,noprint,neprint,mint_,ntrans,ncs_,
     &  namtot_,ncmat_,memmpc_,ne1d,ne2d,nflow,nlabel,iplas,nkon,
     &  ithermal,nmethod,iperturb,nstate_,istartset(*),iendset(*),
     &  ialset(*),kon(*),ipkon(*),nodeboun(*),ndirboun(*),iamboun(*),
     &  ikboun(*),ilboun(*),ipompc(*),nodempc(*),ikmpc(*),ilmpc(*),
     &  nodeforc(*),ndirforc(*),iamforc(*),ikforc(*),ilforc(*),
     &  nelemload(*),iamload(*),nodeprint(*),nelemprint(*),nelcon(*),
     &  nrhcon(*),nalcon(*),nplicon(*),nplkcon(*),ielorien(*),inotr(*),
     &  namta(*),iamt1(*),ielmat(*),nodebounold(*),ndirbounold(*),
     &  iponor(*),knor(*),iponoel(*),inoel(*),rig(*),nodeflow(*),
     &  iamflow(*),nshcon(*),ncocon(*),ics(*),infree(*),nnn(*),i,ipos,
     &  nener,irestartstep,istat,iprestr,iamom,iambodyf,
     &  ns(5)
!
      real*8 co(*),xboun(*),coefmpc(*),xforc(*),xload(*),elcon(*),
     &  rhcon(*),alcon(*),alzero(*),plicon(*),plkcon(*),orab(*),
     &  trab(*),amta(*),t0(*),t1(*),veold(*),
     &  vold(*),xbounold(*),xforcold(*),xloadold(*),t1old(*),eei(*),
     &  xflowold(*),xnor(*),thickn(*),thicke(*),offset(*),xflow(*),
     &  shcon(*),cocon(*),sti(*),ener(*),xstate(*),
     &  prestr(*),p1(3),p2(3),om,omold,bodyf(3),bodyfold(3),ttime,
     &  qaold,csab(7),physcon(2),ctrl(26)
!
      ipos=index(jobnamec,char(0))
      fnrstrt(1:ipos-1)=jobnamec(1:ipos-1)
      fnrstrt(ipos:ipos+5)=".rstrt"
      do i=ipos+6,132
         fnrstrt(i:i)=' '
      enddo
!
      open(15,file=fnrstrt,ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
!
      do
!
         read(15,iostat=istat)istep
         if(istat.lt.0) then
            write(*,*) '*ERROR in restartshort: requested step'
            write(*,*) '       is not in the restart file'
            stop
         endif
!
!        set size
!
         read(15)nset
         read(15)nalset
!
!        load size
!
         read(15)nload
         read(15)nforc
         read(15)nboun
         read(15)nflow
!
!        mesh size
!
         read(15)nk
         read(15)ne
         read(15)nkon
         read(15)mint_
!
!        constraint size
!
         read(15)nmpc
         read(15)memmpc_
!
!        material size
!
         read(15)nmat
         read(15)ntmat_
         read(15)npmat_
         read(15)ncmat_
!
!        transformation size
!
         read(15)norien
         read(15)ntrans
!
!        amplitude size
!
         read(15)nam
         read(15)namtot_
!
!        print size
!
         read(15)noprint
         read(15)neprint
         read(15)nlabel
!
!        cyclic symmetry size
!
         read(15)ncs_
!
!        1d and 2d element size
!
         read(15)ne1d 
         read(15)ne2d 
         read(15)(infree(i),i=1,4)
!
!        procedure info
!
         read(15)nmethod
         read(15)iperturb
         read(15)nener
         read(15)iplas
         read(15)ithermal
         read(15)nstate_
         read(15)iprestr
!
         if(istep.eq.irestartstep) exit
!
!        skipping the next entries
! 
         call skip(nset,nalset,nload,nforc,nboun,nflow,nk,ne,nkon,
     &     mint_,nmpc,memmpc_,nmat,ntmat_,npmat_,ncmat_,norien,ntrans,
     &     nam,noprint,neprint,nlabel,ncs_,ne1d,ne2d,infree,nmethod,
     &     iperturb,nener,iplas,ithermal,nstate_,iprestr)
!
      enddo
!
!     sets
!
      read(15)(set(i),i=1,nset)
      read(15)(istartset(i),i=1,nset)
      read(15)(iendset(i),i=1,nset)
      do i=1,nalset
         read(15)ialset(i)
      enddo
!
!     mesh
!
      read(15)(co(i),i=1,3*nk)
      read(15)(kon(i),i=1,nkon)
      read(15)(ipkon(i),i=1,ne)
      read(15)(lakon(i),i=1,ne)
!
!     single point constraints
!
      read(15)(nodeboun(i),i=1,nboun)
      read(15)(ndirboun(i),i=1,nboun)
      read(15)(xboun(i),i=1,nboun)
      read(15)(ikboun(i),i=1,nboun)
      read(15)(ilboun(i),i=1,nboun)
      if(nam.gt.0) read(15)(iamboun(i),i=1,nboun)
      read(15)(nodebounold(i),i=1,nboun)
      read(15)(ndirbounold(i),i=1,nboun)
      read(15)(xbounold(i),i=1,nboun)
!
!     multiple point constraints
!
      read(15)(ipompc(i),i=1,nmpc)
      read(15)(labmpc(i),i=1,nmpc)
      read(15)(ikmpc(i),i=1,nmpc)
      read(15)(ilmpc(i),i=1,nmpc)
      read(15)(nodempc(i),i=1,3*memmpc_)
      read(15)(coefmpc(i),i=1,memmpc_)
!
!     point forces
!
      read(15)(nodeforc(i),i=1,nforc)
      read(15)(ndirforc(i),i=1,nforc)
      read(15)(xforc(i),i=1,nforc)
      read(15)(ikforc(i),i=1,nforc)
      read(15)(ilforc(i),i=1,nforc)
      if(nam.gt.0) read(15)(iamforc(i),i=1,nforc)
      read(15)(xforcold(i),i=1,nforc)
!
!     distributed loads
!
      read(15)(nelemload(i),i=1,2*nload)
      read(15)(sideload(i),i=1,nload)
      read(15)(xload(i),i=1,2*nload)
      if(nam.gt.0) read(15)(iamload(i),i=1,2*nload)
      read(15)(xloadold(i),i=1,2*nload)
      read(15) (p1(i),i=1,3),(p2(i),i=1,3),om,omold,iamom
      read(15) (bodyf(i),i=1,3),(bodyfold(i),i=1,3),iambodyf
!
!     prestress
!
      if(iprestr.gt.0) read(15) (prestr(i),i=1,6*ne)
!
!     gas flow
!
      read(15)(nodeflow(i),i=1,2*nflow)
      read(15)(xflow(i),i=1,nflow)
      if(nam.gt.0) read(15)(iamflow(i),i=1,nflow)
      read(15)(xflowold(i),i=1,nflow)
!
!     print statements
!
      read(15)(nodeprint(i),i=1,noprint)
      read(15)(nelemprint(i),i=1,neprint)
!
!     labels
!
      read(15)(noelplab(i),i=1,nlabel)
      read(15)(nodeflab(i),i=1,nlabel)
!
!     elastic constants
!
      read(15)(elcon(i),i=1,(ncmat_+1)*ntmat_*nmat)
      read(15)(nelcon(i),i=1,2*nmat)
!
!     density
!
      read(15)(rhcon(i),i=1,2*ntmat_*nmat)
      read(15)(nrhcon(i),i=1,nmat)
!
!     specific heat
!
      read(15)(shcon(i),i=1,2*ntmat_*nmat)
      read(15)(nshcon(i),i=1,nmat)
!
!     conductivity
!
      read(15)(cocon(i),i=1,7*ntmat_*nmat)
      read(15)(ncocon(i),i=1,nmat)
!
!     expansion coefficients
!
      read(15)(alcon(i),i=1,7*ntmat_*nmat)
      read(15)(nalcon(i),i=1,2*nmat)
      read(15)(alzero(i),i=1,nmat)
!
!     physical constants
!
      read(15)(physcon(i),i=1,2)
!
!     plastic data
!
      if(iplas.ne.0)then
         read(15)(plicon(i),i=1,(2*npmat_+1)*ntmat_*nmat)
         read(15)(nplicon(i),i=1,(ntmat_+1)*nmat)
         read(15)(plkcon(i),i=1,(2*npmat_+1)*ntmat_*nmat)
         read(15)(nplkcon(i),i=1,(ntmat_+1)*nmat)
      endif
!
!     material orientation
!
      if(norien.ne.0)then
         read(15)(orname(i),i=1,norien)
         read(15)(orab(i),i=1,7*norien)
         read(15)(ielorien(i),i=1,ne)
      endif
!
!     transformations
!
      if(ntrans.ne.0)then
         read(15)(trab(i),i=1,7*ntrans)
         read(15)(inotr(i),i=1,2*nk)
      endif
!
!     amplitudes
!
      if(nam.gt.0)then
         read(15)(amname(i),i=1,nam)
         read(15)(namta(i),i=1,3*nam-1)
         read(15) namta(3*nam)
         read(15)(amta(i),i=1,2*namta(3*nam-1))
      endif
!
!     temperatures
!
      if(ithermal.gt.0)then
         if((ne1d.gt.0).or.(ne2d.gt.0))then
            read(15)(t0(i),i=1,3*nk)
            read(15)(t1(i),i=1,3*nk)
         else
            read(15)(t0(i),i=1,nk)
            read(15)(t1(i),i=1,nk)
         endif
         if(nam.gt.0) read(15)(iamt1(i),i=1,nk)
         read(15)(t1old(i),i=1,nk)
      endif
!
!     materials
!
      read(15)(matname(i),i=1,nmat)
      read(15)(ielmat(i),i=1,ne)
!
!     displacement, velocity and acceleration
!
      read(15)(vold(i),i=1,4*nk)
      if((nmethod.eq.4).or.((nmethod.eq.1).and.(iperturb.ge.2))) then
         read(15)(veold(i),i=1,4*nk)
      endif
!
!     reordering
!
      read(15)(nnn(i),i=1,nk)
!
!     1d and 2d elements
!
      if((ne1d.gt.0).or.(ne2d.gt.0))then
         read(15)(iponor(i),i=1,2*nkon)
         read(15)(xnor(i),i=1,infree(1)-1)
         read(15)(knor(i),i=1,infree(2)-1)
         read(15)(thicke(i),i=1,2*nkon)
         read(15)(offset(i),i=1,2*ne)
         read(15)(iponoel(i),i=1,infree(4))
         read(15)(inoel(i),i=1,3*(infree(3)-1))
         read(15)(rig(i),i=1,infree(4))
      endif
!
!     cyclic symmetry
!
      if(ncs_.gt.0)then
         read(15)(ics(i),i=1,ncs_)
         write(15)(ns(i),i=1,5)
         write(15)(csab(i),i=1,7)
      endif
!
!     integration point variables
!
      read(15)(sti(i),i=1,6*mint_*ne)
      read(15)(eei(i),i=1,6*mint_*ne)
      if((nodeflab(7).eq."ENER").or.(noelplab(7).eq."ENER")) then
         read(15)(ener(i),i=1,mint_*ne)
      endif
      if(nstate_.gt.0)then
         read(15)(xstate(i),i=1,nstate_*mint_*ne)
      endif
!
!     control parameters
!
      read(15) (ctrl(i),i=1,26)
      read(15) qaold
      read(15) output
      read(15) ttime
!
      close(15)
!
      return
      end



















