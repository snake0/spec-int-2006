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
      subroutine restartwrite(istepnew,nset,nload,nforc, nboun,nk,ne,
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
     &  ener,xstate,jobnamec,infree,nnn,prestr,iprestr,p1,p2,om,iamom,
     &  omold,bodyf,iambodyf,bodyfold,ttime,qaold,ns,csab,output,
     &  physcon,ctrl)
!
      implicit none
!
      logical ex,op
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
     &  nener,iamom,iambodyf,ns(5),iprestr,j,itoread,ipassed,istepnew,
     &  istat
!
      real*8 co(*),xboun(*),coefmpc(*),xforc(*),xload(*),elcon(*),
     &  rhcon(*),alcon(*),alzero(*),plicon(*),plkcon(*),orab(*),
     &  trab(*),amta(*),t0(*),t1(*),prestr(*),veold(*),
     &  vold(*),xbounold(*),xforcold(*),xloadold(*),t1old(*),eei(*),
     &  xflowold(*),xnor(*),thickn(*),thicke(*),offset(*),xflow(*),
     &  shcon(*),cocon(*),sti(*),ener(*),xstate(*),p1(3),p2(3),om,
     &  omold,bodyf(3),bodyfold(3),qaold,csab(7),physcon(2),ctrl(26),
     &  ttime
!
      ipos=index(jobnamec,char(0))
      fnrstrt(1:ipos-1)=jobnamec(1:ipos-1)
      fnrstrt(ipos:ipos+5)=".rstrt"
      do i=ipos+6,132
         fnrstrt(i:i)=' '
      enddo
!
!     check whether the restart file exists and is opened
!
      inquire(15,EXIST=ex)
      inquire(15,OPENED=op)
!
      if((ex).and.(.not.op)) then
         open(15,file=fnrstrt,ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
!
!        look for the right step
!
         itoread=0
         ipassed=0
!
         loop: do j=1,2
            do 
!
               read(15,iostat=istat)istep
               if(istat.lt.0) then
                  exit loop
               endif
!
!              reading the number of sets
!
               read(15)nset
               read(15)nalset
!
!              load size
!
               read(15)nload
               read(15)nforc
               read(15)nboun
               read(15)nflow
!
!              mesh size
!
               read(15)nk
               read(15)ne
               read(15)nkon
               read(15)mint_
!
!              constraint size
!
               read(15)nmpc
               read(15)memmpc_
!
!              material size
!
               read(15)nmat
               read(15)ntmat_
               read(15)npmat_
               read(15)ncmat_
!
!              transformation size
!
               read(15)norien
               read(15)ntrans
!
!              amplitude size
!
               read(15)nam
               read(15)namtot_
!
!              print size
!
               read(15)noprint
               read(15)neprint
               read(15)nlabel
!
!              cyclic symmetry size
!
               read(15)ncs_
!
!              1d and 2d element size
!
               read(15)ne1d 
               read(15)ne2d 
               read(15)(infree(i),i=1,4)
!
!              procedure info
!
               read(15)nmethod
               read(15)iperturb
               read(15)nener
               read(15)iplas
               read(15)ithermal
               read(15)nstate_
               read(15)iprestr
!
               if(j.eq.1) then
                  if(istep.lt.istepnew) then
!
!                    skipping the next entries
!     
                     call skip(nset,nalset,nload,nforc,nboun,nflow,nk,
     &                    ne,nkon,mint_,nmpc,memmpc_,nmat,ntmat_,npmat_,
     &                    ncmat_,norien,ntrans,nam,noprint,neprint,
     &                    nlabel,ncs_,ne1d,ne2d,infree,nmethod,iperturb,
     &                    nener,iplas,ithermal,nstate_,iprestr)
                     itoread=itoread+1
                  else
                     rewind(15)
                     if(itoread.eq.0) then
                        exit loop
                     else
                        exit
                     endif
                  endif
               else
!
!                 kipping the next entries
!     
                  call skip(nset,nalset,nload,nforc,nboun,nflow,nk,
     &                 ne,nkon,mint_,nmpc,memmpc_,nmat,ntmat_,npmat_,
     &                 ncmat_,norien,ntrans,nam,noprint,neprint,nlabel,
     &                 ncs_,ne1d,ne2d,infree,nmethod,iperturb,nener,
     &                 iplas,ithermal,nstate_,iprestr)
                  ipassed=ipassed+1
                  if(ipassed.eq.itoread) exit
               endif
            enddo
         enddo loop
      endif
!
      if(.not.ex) then
         open(15,file=fnrstrt,ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
      endif
!
      write(15)istepnew
!
!     set size
!
      write(15)nset
      write(15)nalset
!
!     load size
!
      write(15)nload
      write(15)nforc
      write(15)nboun
      write(15)nflow
!
!     mesh size
!
      write(15)nk
      write(15)ne
      write(15)nkon
      write(15)mint_
!
!     constraint size
!
      write(15)nmpc
      write(15)memmpc_
!
!     material size
!
      write(15)nmat
      write(15)ntmat_
      write(15)npmat_
      write(15)ncmat_
!
!     transformation size
!
      write(15)norien
      write(15)ntrans
!
!     amplitude size
!
      write(15)nam
      write(15)namtot_
!
!     print size
!
      write(15)noprint
      write(15)neprint
      write(15)nlabel
!
!     cyclic symmetry size
!
      write(15)ncs_
!
!     1d and 2d element size
!
      write(15)ne1d 
      write(15)ne2d 
      write(15)(infree(i),i=1,4)
!
!     procedure info
!
      write(15)nmethod
      write(15)iperturb
      write(15)nener
      write(15)iplas
      write(15)ithermal
      write(15)nstate_
      write(15)iprestr
!
!     sets
!
      write(15)(set(i),i=1,nset)
      write(15)(istartset(i),i=1,nset)
      write(15)(iendset(i),i=1,nset)
!
!     watch out: the statement
!        write(15)(ialset(i),i=nalset)    (short form)
!     needs less space to store than
!        do i=1,nalset
!           write(15) ialset(i)           (long form)
!        enddo
!     but cannot be accessed by read statements of the form
!        do i=1,nalset
!           read(15) im0
!        enddo
!     as needed in routine restartshort. Therefore the long form
!     is used for ialset.
!
      do i=1,nalset
         write(15) ialset(i)
      enddo
!
!     mesh
!
      write(15)(co(i),i=1,3*nk)
      write(15)(kon(i),i=1,nkon)
      write(15)(ipkon(i),i=1,ne)
      write(15)(lakon(i),i=1,ne)
!
!     single point constraints
!
      write(15)(nodeboun(i),i=1,nboun)
      write(15)(ndirboun(i),i=1,nboun)
      write(15)(xboun(i),i=1,nboun)
      write(15)(ikboun(i),i=1,nboun)
      write(15)(ilboun(i),i=1,nboun)
      if(nam.gt.0) write(15)(iamboun(i),i=1,nboun)
      write(15)(nodebounold(i),i=1,nboun)
      write(15)(ndirbounold(i),i=1,nboun)
      write(15)(xbounold(i),i=1,nboun)
!
!     multiple point constraints
!
      write(15)(ipompc(i),i=1,nmpc)
      write(15)(labmpc(i),i=1,nmpc)
      write(15)(ikmpc(i),i=1,nmpc)
      write(15)(ilmpc(i),i=1,nmpc)
      write(15)(nodempc(i),i=1,3*memmpc_)
      write(15)(coefmpc(i),i=1,memmpc_)
!
!     point forces
!
      write(15)(nodeforc(i),i=1,nforc)
      write(15)(ndirforc(i),i=1,nforc)
      write(15)(xforc(i),i=1,nforc)
      write(15)(ikforc(i),i=1,nforc)
      write(15)(ilforc(i),i=1,nforc)
      if(nam.gt.0) write(15)(iamforc(i),i=1,nforc)
      write(15)(xforcold(i),i=1,nforc)
!
!     distributed loads
!
      write(15)(nelemload(i),i=1,2*nload)
      write(15)(sideload(i),i=1,nload)
      write(15)(xload(i),i=1,2*nload)
      if(nam.gt.0) write(15)(iamload(i),i=1,2*nload)
      write(15)(xloadold(i),i=1,2*nload)
      write(15) (p1(i),i=1,3),(p2(i),i=1,3),om,omold,iamom
      write(15) (bodyf(i),i=1,3),(bodyfold(i),i=1,3),iambodyf
!
!     prestress
!
      if(iprestr.gt.0) write(15) (prestr(i),i=1,6*ne)
!
!     gas flow
!
      write(15)(nodeflow(i),i=1,2*nflow)
      write(15)(xflow(i),i=1,nflow)
      if(nam.gt.0) write(15)(iamflow(i),i=1,nflow)
      write(15)(xflowold(i),i=1,nflow)
!
!     print statements
!
      write(15)(nodeprint(i),i=1,noprint)
      write(15)(nelemprint(i),i=1,neprint)
!
!     labels
!
      write(15)(noelplab(i),i=1,nlabel)
      write(15)(nodeflab(i),i=1,nlabel)
!
!     elastic constants
!
      write(15)(elcon(i),i=1,(ncmat_+1)*ntmat_*nmat)
      write(15)(nelcon(i),i=1,2*nmat)
!
!     density
!
      write(15)(rhcon(i),i=1,2*ntmat_*nmat)
      write(15)(nrhcon(i),i=1,nmat)
!
!     specific heat
!
      write(15)(shcon(i),i=1,2*ntmat_*nmat)
      write(15)(nshcon(i),i=1,nmat)
!
!     conductivity
!
      write(15)(cocon(i),i=1,7*ntmat_*nmat)
      write(15)(ncocon(i),i=1,nmat)
!
!     expansion coefficients
!
      write(15)(alcon(i),i=1,7*ntmat_*nmat)
      write(15)(nalcon(i),i=1,2*nmat)
      write(15)(alzero(i),i=1,nmat)
!
!     physical constants
!
      write(15)(physcon(i),i=1,2)
!
!     plastic data
!
      if(iplas.ne.0)then
         write(15)(plicon(i),i=1,(2*npmat_+1)*ntmat_*nmat)
         write(15)(nplicon(i),i=1,(ntmat_+1)*nmat)
         write(15)(plkcon(i),i=1,(2*npmat_+1)*ntmat_*nmat)
         write(15)(nplkcon(i),i=1,(ntmat_+1)*nmat)
      endif
!
!     material orientation
!
      if(norien.ne.0)then
         write(15)(orname(i),i=1,norien)
         write(15)(orab(i),i=1,7*norien)
         write(15)(ielorien(i),i=1,ne)
      endif
!
!     transformations
!
      if(ntrans.ne.0)then
         write(15)(trab(i),i=1,7*ntrans)
         write(15)(inotr(i),i=1,2*nk)
      endif
!
!     amplitudes
!
      if(nam.gt.0)then
         write(15)(amname(i),i=1,nam)
         write(15)(namta(i),i=1,3*nam-1)
         write(15) namta(3*nam)
         write(15)(amta(i),i=1,2*namta(3*nam-1))
      endif
!
!     temperatures
!
      if(ithermal.gt.0)then
         if((ne1d.gt.0).or.(ne2d.gt.0))then
            write(15)(t0(i),i=1,3*nk)
            write(15)(t1(i),i=1,3*nk)
         else
            write(15)(t0(i),i=1,nk)
            write(15)(t1(i),i=1,nk)
         endif
         if(nam.gt.0) write(15)(iamt1(i),i=1,nk)
         write(15)(t1old(i),i=1,nk)
      endif
!
!     materials
!
      write(15)(matname(i),i=1,nmat)
      write(15)(ielmat(i),i=1,ne)
!
!     displacement, velocity and acceleration
!
      write(15)(vold(i),i=1,4*nk)
      if((nmethod.eq.4).or.((nmethod.eq.1).and.(iperturb.ge.2))) then
         write(15)(veold(i),i=1,4*nk)
      endif
!
!     reordering
!
      write(15)(nnn(i),i=1,nk)
!
!     1d and 2d elements
!
      if((ne1d.gt.0).or.(ne2d.gt.0))then
         write(15)(iponor(i),i=1,2*nkon)
         write(15)(xnor(i),i=1,infree(1)-1)
         write(15)(knor(i),i=1,infree(2)-1)
         write(15)(thicke(i),i=1,2*nkon)
         write(15)(offset(i),i=1,2*ne)
         write(15)(iponoel(i),i=1,infree(4))
         write(15)(inoel(i),i=1,3*(infree(3)-1))
         write(15)(rig(i),i=1,infree(4))
      endif
!
!     cyclic symmetry
!
      if(ncs_.gt.0)then
         write(15)(ics(i),i=1,ncs_)
         write(15)(ns(i),i=1,5)
         write(15)(csab(i),i=1,7)
      endif
!
!     integration point variables
!
      write(15)(sti(i),i=1,6*mint_*ne)
      write(15)(eei(i),i=1,6*mint_*ne)
      if((nodeflab(7).eq."ENER").or.(noelplab(7).eq."ENER")) then
         write(15)(ener(i),i=1,mint_*ne)
      endif
      if(nstate_.gt.0)then
         write(15)(xstate(i),i=1,nstate_*mint_*ne)
      endif
!
!     control parameters
!
      write(15) (ctrl(i),i=1,26)
      write(15) qaold
      write(15) output
      write(15) ttime
!
c      close(15)
!
      return
      end



















