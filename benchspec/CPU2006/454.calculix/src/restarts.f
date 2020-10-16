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
      subroutine restarts(istep,nset,nload,nforc, nboun,nk,ne,
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
     &  ener,xstate,jobnamec,infree,nnn,irstrt,text,textpart,istat,in,n,
     &  key,prestr,iprestr,p1,
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
      character*40 textpart(16)
      character*132 jobnamec,text
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
     &  iamflow(*),nshcon(*),ncocon(*),ics(*),infree(*),nnn(*),
     &  nener,irestartstep,irestartread,irstrt,istat,in,n,i,key,
     &  iprestr,iamom,iambodyf,ns(5)
!
      real*8 co(*),xboun(*),coefmpc(*),xforc(*),xload(*),elcon(*),
     &  rhcon(*),alcon(*),alzero(*),plicon(*),plkcon(*),orab(*),
     &  trab(*),amta(*),t0(*),t1(*),prestr(*),veold(*),
     &  vold(*),xbounold(*),xforcold(*),xloadold(*),t1old(*),eei(*),
     &  xflowold(*),xnor(*),thickn(*),thicke(*),offset(*),xflow(*),
     &  shcon(*),cocon(*),sti(*),ener(*),xstate(*),p1(3),p2(3),om,
     &  omold,bodyf(3),bodyfold(3),ttime,qaold,csab(7),physcon(2),
     &  ctrl(26)
!
      irestartread=0
!
      do i=1,n
         if(textpart(i)(1:4).eq.'READ') then
            irestartread=1
            irestartstep=1
         elseif(textpart(i)(1:5).eq.'STEP=') then
            read(textpart(i)(6:40),'(i35)',iostat=istat) irestartstep
            if(istat.gt.0) call inputerror(text)
         elseif(textpart(i)(1:5).eq.'WRITE') then
            irstrt=1
         elseif(textpart(i)(1:10).eq.'FREQUENCY=') then
            read(textpart(i)(11:40),'(i30)',iostat=istat) irstrt
            if(istat.gt.0) call inputerror(text)
         endif
      enddo
!
      if(irestartread.eq.1) then
        call restartread(istep,nset,nload,nforc, nboun,nk,ne,
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
      endif
!
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end


