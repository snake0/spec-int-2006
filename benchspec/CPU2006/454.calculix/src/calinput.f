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
      subroutine calinput(co,nk,kon,ipkon,lakon,nkon,
     &  ne,nodeboun,ndirboun,xboun,nboun,
     &  ipompc,nodempc,coefmpc,nmpc,nmpc_,nodeforc,ndirforc,xforc,nforc,
     &  nforc_,nelemload,sideload,xload,nload,nload_,p1,p2,om,bodyf,
     &  nodeprint,noprint,nelemprint,neprint,mpcfree,nboun_,
     &  nev,set,istartset,iendset,ialset,
     &  nset,nalset,elcon,nelcon,rhcon,nrhcon,alcon,nalcon,
     &  alzero,t0,t1,
     &  matname,ielmat,orname,orab,ielorien,amname,amta,namta,nam,
     &  nmethod,iamforc,iamload,iamom,iambodyf,iamt1,
     &  ithermal,iperturb,istat,istep,nmat,ntmat_,norien,
     &  prestr,iprestr,in,isolver,tol,ncv,mxiter,veold,tinc,tper,
     &  alpham,betam,noelplab,nodeflab,jout,nlabel,idrct,jmax,
     &  tmin,tmax,iexpl,alpha,haftol,iamboun,plicon,nplicon,plkcon,
     &  nplkcon,iplas,npmat_,mint_,nk_,trab,inotr,ntrans,ikboun,
     &  ilboun,ikmpc,ilmpc,ics,dcs,ncs_,namtot_,ns,nstate_,ncmat_,iumat,
     &  csab,labmpc,iponor,xnor,knor,thickn,thicke,ikforc,ilforc,
     &  offset,iponoel,inoel,rig,infree,nshcon,shcon,cocon,ncocon,
     &  physcon,nflow,nodeflow,xflow,iamflow,
     &  nflow_,ctrl)
!
      implicit none
!
!     nmethod: 0:no analysis; 1:static; 2:frequency; 3:buckling;
!              4:linear dynamic
!     ithermal: 0:no thermal stresses; 1:thermal stresses;
!     iprestr: 0: no residual stresses; 1: residual stresses;
!     iperturb: 0:no perturbation; 1:perturbation; 2: nonlinear
!               geometric analysis; 3: material and geometrical
!               nonlinearities
!     istep: step number
!     istat: error indicator (<0:EOF, >0: input error)
!
      logical boun_flag,cload_flag,dload_flag,temp_flag,elprint_flag,
     &  nodeprint_flag,elfile_flag,nodefile_flag,node_flag,flow_flag,
     &  dflux_flag,cflux_flag,film_flag,radiate_flag
!
      character*4 noelplab(*),nodeflab(*)
      character*5 sideload(*)
      character*8 lakon(*)
      character*20 matname(*),orname(*),amname(*),labmpc(*)
      character*21 set(*),leftset,rightset
      character*40 textpart(16)
      character*132 text
!
      integer kon(*),nodeboun(*),ndirboun(*),ipompc(*),nodempc(3,*),
     &  nodeforc(*),ndirforc(*),nelemload(2,*),nodeprint(*),
     &  nelemprint(*),
     &  istartset(*),iendset(*),ialset(*),ipkon(*),ics(*),ns(5),
     &  nelcon(2,*),nrhcon(*),nalcon(2,*),ielmat(*),ielorien(*),
     &  namta(3,*),iamforc(*),iamload(2,*),iamom,iambodyf,iamt1(*),
     &  iamboun(*),inotr(2,*),ikboun(*),ilboun(*),ikmpc(*),ilmpc(*),
     &  iponor(2,*),knor(*),ikforc(*),ilforc(*),iponoel(*),inoel(3,*),
     &  infree(4),ixfree,ikfree,inoelfree,iponoelmax,rig(*),nshcon(*),
     &  ncocon(2,*),nodeflow(2,*),iamflow(*)
!
      integer nalset,nalset_,nmat,nmat_,ntmat_,norien,norien_,
     &  nmethod,nk,ne,nboun,nmpc,nmpc_,mpcfree,i,istat,in,n,
     &  key,nk_,ne_,nboun_,ncv,mxiter,ncs_,namtot_,nstate_,
     &  isolver,ithermal,iperturb,iprestr,istep,nev,nkon,
     &  noprint,neprint,nload,nload_,nforc,nforc_,nlabel,iumat,
     &  nset,nset_,noprint_,neprint_,kflag,nam,nam_,jout,ncmat_,
     &  ierror,idrct,jmax,iexpl,iplas,npmat_,mint_,ntrans,ntrans_,
     &  M_or_SPC,nplicon(0:ntmat_,*),nplkcon(0:ntmat_,*),nflow,
     &  nflow_
!
      real*8 co(3,*),xboun(*),coefmpc(*),xforc(*),
     &  xload(2,*),p1(3),p2(3),bodyf(3),alzero(*),offset(2,*),
     &  elcon(0:ncmat_,ntmat_,*),rhcon(0:1,ntmat_,*),
     &  alcon(0:6,ntmat_,*),thicke(2,*),thickn(2,*),xnor(*),
     &  t1(*),orab(7,*),prestr(6,*),amta(2,*),
     &  veold(0:3,*),t0(*),plicon(0:2*npmat_,ntmat_,*),
     &  plkcon(0:2*npmat_,ntmat_,*),trab(7,*),dcs(*),csab(7),
     &  shcon(0:1,ntmat_,*),cocon(0:6,ntmat_,*),
     &  xflow(*),ctrl(26)
!
      real*8 tol,om,tinc,tper,alpham,betam,tmin,tmax,
     &  alpha,haftol,dd,tietol,physcon(2)
!
      save leftset
!
      ixfree=infree(1)
      ikfree=infree(2)
      inoelfree=infree(3)
      iponoelmax=infree(4)
!
      kflag=2
      isolver=0
      jout=0
      iexpl=0
      ns(2)=-1
!
!     the following flag is used to check whether any SPC's or MPC's
!     are used before transformation definitions
!
      M_or_SPC=0
!
!     the flags indicate whether some specific keyword cards already
!     occurred (needed to determine the effect of OP=NEW or to check
!     whether the element or nodal output selection should be reset)
!
      boun_flag=.false.
      cload_flag=.false.
      dload_flag=.false.
      temp_flag=.false.
      elprint_flag=.false.
      nodeprint_flag=.false.
      elfile_flag=.false.
      nodefile_flag=.false.
      flow_flag=.false.
      film_flag=.false.
      dflux_flag=.false.
      radiate_flag=.false.
      cflux_flag=.false.
!
      noprint_=noprint
      neprint_=neprint
!
      noprint=0
      neprint=0
!
      if(istep.eq.0) then
!
!        initializing the maxima
!
         ne_=ne
         nset_=nset
         nalset_=nalset
         nmat_=nmat
         norien_=norien
         ntrans_=ntrans
         nam_=nam
!
         nmethod=0
!
         ne=0
         nset=0
         nalset=0
         nmat=0
         norien=0
         ntrans=0
         nam=0
!
         do i=1,ne_
            ipkon(i)=-1
         enddo
!
         do i=1,nlabel
            nodeflab(i)='    '
            noelplab(i)='    '
         enddo
!
!        opening the input  and output file
!
      endif
!
      call getnewline(text,textpart,istat,in,n,key)
!
      loop: do
!
         if(istat.lt.0) then
               call frdclose()
               stop
         endif
!
         if(textpart(1)(1:10).eq.'*AMPLITUDE') then
            call amplitudes(text,textpart,amname,amta,namta,nam,
     &        nam_,namtot_,istep,istat,in,n)
!
         elseif(textpart(1)(1:12).eq.'*BEAMSECTION') then
            call beamsections(text,textpart,set,istartset,iendset,
     &           ialset,nset,ielmat,matname,nmat,ielorien,orname,norien,
     &           thicke,kon,ipkon,nk,nk_,iponor,xnor,ixfree,
     &           offset,lakon,istep,istat,in,n)
!
         elseif(textpart(1)(1:9).eq.'*BOUNDARY') then
            M_or_SPC=1
            call boundaries(text,textpart,set,istartset,iendset,
     &        ialset,nset,nodeboun,ndirboun,xboun,nboun,nboun_,nk,
     &        iamboun,amname,nam,ipompc,nodempc,coefmpc,nmpc,nmpc_,
     &        mpcfree,inotr,trab,ntrans,ikboun,ilboun,ikmpc,ilmpc,
     &        nk_,co,labmpc,boun_flag,istep,istat,in,n)
            boun_flag=.true.
!
         elseif(textpart(1)(1:7).eq.'*BUCKLE') then
            call buckles(text,textpart,nmethod,nev,tol,ncv,mxiter,
     &        nforc,nload,ithermal,iprestr,om,bodyf,t0,t1,nk,iperturb,
     &        istep,istat,in,n)
!
         elseif(textpart(1)(1:6).eq.'*CFLUX') then
            call cfluxes(text,textpart,set,istartset,iendset,
     &        ialset,nset,nodeforc,ndirforc,xforc,nforc,nforc_,iamforc,
     &        amname,nam,ntrans,trab,inotr,co,ikforc,ilforc,nk,
     &        cflux_flag,istep,istat,in,n)
            cflux_flag=.true.

         elseif(textpart(1)(1:6).eq.'*CLOAD') then
            call cloads(text,textpart,set,istartset,iendset,
     &        ialset,nset,nodeforc,ndirforc,xforc,nforc,nforc_,
     &        iamforc,amname,nam,ntrans,trab,inotr,co,ikforc,ilforc,
     &        nk,cload_flag,istep,istat,in,n)
            cload_flag=.true.
!
         elseif(textpart(1)(1:13).eq.'*CONDUCTIVITY') then
            call conductivities(text,textpart,cocon,ncocon,
     &           nmat,ntmat_,istep,istat,in,n)
!
         elseif(textpart(1)(1:9).eq.'*CONTROLS') then
            call controlss(text,textpart,ctrl,istep,istat,in,n)
!
         elseif(textpart(1)(1:32).eq.'*COUPLEDTEMPERATURE-DISPLACEMENT')
     &           then
            call couptempdisps(text,textpart,nmethod,iperturb,isolver,
     &           istep,istat,in,n,tinc,tper,tmin,tmax,idrct,ithermal)
!
         elseif(textpart(1)(1:6).eq.'*CREEP') then
            call creeps(text,textpart,nelcon,nmat,ntmat_,npmat_,
     &        plicon,nplicon,elcon,iplas,iperturb,nstate_,ncmat_,
     &        matname,istep,istat,in,n)
!
         elseif(textpart(1)(1:16).eq.'*CYCLICHARDENING') then
            call cychards(text,textpart,nelcon,nmat,ntmat_,
     &        npmat_,plicon,nplicon,istep,istat,in,n)
!
         elseif(textpart(1)(1:20).eq.'*CYCLICSYMMETRYMODEL') then
            call cycsymmods(text,textpart,set,istartset,iendset,
     &        ialset,nset,leftset,rightset,tietol,co,nk,ipompc,nodempc,
     &        coefmpc,nmpc,nmpc_,ikmpc,ilmpc,mpcfree,dcs(1),dcs(ncs_+1),
     &        ics(1),ics(ncs_+1),ics(2*ncs_+1),dcs(2*ncs_+1),
     &        dcs(3*ncs_+1),ns,ncs_,csab,labmpc,istep,istat,in,n)
!
         elseif(textpart(1)(1:22).eq.'*DEFORMATIONPLASTICITY') then
            call defplasticities(text,textpart,elcon,nelcon,
     &        nmat,ntmat_,ncmat_,istep,istat,in,n,iperturb)
!
         elseif(textpart(1)(1:8).eq.'*DENSITY') then
            call densities(text,textpart,rhcon,nrhcon,
     &        nmat,ntmat_,istep,istat,in,n)
!
         elseif(textpart(1)(1:7).eq.'*DEPVAR') then
            call depvars(text,textpart,nelcon,nmat,
     &           nstate_,istep,istat,in,n)
!
         elseif(textpart(1)(1:6).eq.'*DFLUX') then
            call dfluxes(text,textpart,set,istartset,iendset,
     &        ialset,nset,nelemload,sideload,xload,nload,nload_,
     &        ielmat,ntmat_,iamload,
     &        amname,nam,lakon,ne,dflux_flag,istep,istat,in,n)
            dflux_flag=.true.
!
         elseif(textpart(1)(1:6).eq.'*DLOAD') then
            call dloads(text,textpart,set,istartset,iendset,
     &        ialset,nset,nelemload,sideload,xload,nload,nload_,
     &        p1,p2,om,bodyf,ielmat,ntmat_,iamload,iamom,
     &        iambodyf,amname,nam,lakon,ne,dload_flag,istep,istat,in,n)
            dload_flag=.true.
!
         elseif(textpart(1)(1:8).eq.'*DYNAMIC') then
            call dynamics(text,textpart,nmethod,iperturb,tinc,tper,
     &        tmin,tmax,idrct,alpha,haftol,iexpl,isolver,istep,
     &        istat,in,n)
!
         elseif(textpart(1)(1:8).eq.'*ELASTIC') then
            call elastics(text,textpart,elcon,nelcon,
     &        nmat,ntmat_,ncmat_,istep,istat,in,n)
!
         elseif(textpart(1)(1:8).eq.'*ELEMENT') then
            call elements(text,textpart,kon,ipkon,lakon,nkon,
     &        ne,ne_,set,istartset,
     &        iendset,ialset,nset,nset_,nalset,nalset_,
     &        mint_,ixfree,iponor,xnor,istep,istat,in,n)
!
         elseif(textpart(1)(1:7).eq.'*ELFILE') then
            node_flag=.false.
            call noelfiles(text,textpart,set,istartset,iendset,ialset,
     &        nset,nset_,nalset,nodeprint,noprint,noprint_,jout,
     &        nodeflab,nmethod,nodefile_flag,elfile_flag,node_flag,
     &        istep,istat,in,n)
            elfile_flag=.true.
!
         elseif(textpart(1)(1:8).eq.'*ELPRINT') then
            call elprints(text,textpart,set,istartset,iendset,ialset,
     &        nset,nset_,nalset,nelemprint,neprint,neprint_,jout,
     &        noelplab,nmethod,elprint_flag,istep,istat,in,n)
            elprint_flag=.true.
!
         elseif(textpart(1)(1:6).eq.'*ELSET') then
            call noelsets(text,textpart,set,istartset,iendset,ialset,
     &        nset,nset_,nalset,nalset_,nk,ne,istep,istat,in,n)
!
         elseif(textpart(1)(1:8).eq.'*ENDSTEP') then
            exit
!
         elseif(textpart(1)(1:9).eq.'*EQUATION') then
            M_or_SPC=1
            call equations(text,textpart,ipompc,nodempc,coefmpc,
     &        nmpc,nmpc_,mpcfree,nk,co,trab,inotr,ntrans,ikmpc,ilmpc,
     &        labmpc,istep,istat,in,n)
!
         elseif(textpart(1)(1:10).eq.'*EXPANSION') then
            call expansions(text,textpart,alcon,nalcon,
     &        alzero,nmat,ntmat_,istep,istat,in,n)
!
         elseif(textpart(1)(1:5).eq.'*FILM') then
            call films(text,textpart,set,istartset,iendset,
     &        ialset,nset,nelemload,sideload,xload,nload,nload_,
     &        ielmat,ntmat_,iamload,amname,nam,lakon,ne,film_flag,
     &        istep,istat,in,n)
            film_flag=.true.
!
         elseif(textpart(1)(1:10).eq.'*FREQUENCY') then
            call frequencies(text,textpart,nmethod,
     &        nev,tol,ncv,mxiter,iperturb,istep,istat,in,n)
!
         elseif(textpart(1)(1:8).eq.'*HEADING') then
            call headings(text,textpart,istat,in,n)
!
         elseif(textpart(1)(1:13).eq.'*HEATTRANSFER') then
            call heattransfers(text,textpart,nmethod,iperturb,isolver,
     &           istep,istat,in,n,tinc,tper,tmin,tmax,idrct,ithermal)
!
         elseif(textpart(1)(1:13).eq.'*HYPERELASTIC') then
            call hyperelastics(text,textpart,elcon,nelcon,
     &        nmat,ntmat_,ncmat_,istep,istat,in,n,iperturb)
!
         elseif(textpart(1)(1:10).eq.'*HYPERFOAM') then
            call hyperfoams(text,textpart,elcon,nelcon,
     &        nmat,ntmat_,ncmat_,istep,istat,in,n,iperturb)
!
         elseif(textpart(1)(1:18).eq.'*INITIALCONDITIONS') then
            call initialconditions(text,textpart,set,istartset,
     &        iendset,ialset,nset,t0,t1,prestr,iprestr,ithermal,
     &        veold,inoelfree,nk_,istep,istat,in,n)
!
         elseif(textpart(1)(1:13).eq.'*MASSFLOWRATE') then
            call massflowrates(text,textpart,nodeflow,xflow,
     &        iamflow,nflow,nflow_,amname,nam,flow_flag,
     &        istep,istat,in,n)
            flow_flag=.true.
!
         elseif(textpart(1)(1:9).eq.'*MATERIAL') then
            call materials(text,textpart,matname,nmat,nmat_,
     &        istep,istat,in,n)
!
         elseif(textpart(1)(1:13).eq.'*MODALDAMPING') then
            call modaldampings(text,textpart,nmethod,alpham,betam,istep,
     &        istat,in,n)
!
         elseif(textpart(1)(1:13).eq.'*MODALDYNAMIC') then
            call modaldynamics(text,textpart,nmethod,tinc,tper,iexpl,
     &        istep,istat,in,n)
!
         elseif(textpart(1)(1:4).eq.'*MPC') then
            call mpcs(text,textpart,set,istartset,iendset,
     &           ialset,nset,nset_,nalset,nalset_,ipompc,nodempc,
     &           coefmpc,labmpc,nmpc,nmpc_,mpcfree,ikmpc,ilmpc,lakon,
     &           ipkon,kon,nk,nk_,nodeboun,ndirboun,ikboun,ilboun,
     &           nboun,nboun_,iperturb,ne_,co,xboun,ctrl,istep,istat,
     &           in,n)
!
         elseif(textpart(1)(1:11).eq.'*NOANALYSIS') then
            call noanalysis(text,textpart,nmethod,iperturb,istep,
     &        istat,in,n)
!
         elseif(textpart(1)(1:15).eq.'*NODALTHICKNESS') then
            call nodalthicknesses(text,textpart,set,istartset,iendset,
     &           ialset,nset,thickn,nk,istep,istat,in,n)
!
         elseif((textpart(1)(1:5).eq.'*NODE').and.
     &          (textpart(1)(1:10).ne.'*NODEPRINT').and.
     &          (textpart(1)(1:9).ne.'*NODEFILE')) then
            call nodes(text,textpart,co,nk,nk_,set,istartset,iendset,
     &        ialset,nset,nset_,nalset,nalset_,istep,istat,in,n)
!
         elseif(textpart(1)(1:9).eq.'*NODEFILE') then
            node_flag=.true.
            call noelfiles(text,textpart,set,istartset,iendset,ialset,
     &        nset,nset_,nalset,nodeprint,noprint,noprint_,jout,
     &        nodeflab,nmethod,nodefile_flag,elfile_flag,node_flag,
     &        istep,istat,in,n)
            nodefile_flag=.true.
!
         elseif(textpart(1)(1:10).eq.'*NODEPRINT') then
            call nodeprints(text,textpart,set,istartset,iendset,ialset,
     &        nset,nset_,nalset,nodeprint,noprint,noprint_,jout,
     &        noelplab,nodeprint_flag,istep,istat,in,n)
            nodeprint_flag=.true.
!
         elseif(textpart(1)(1:7).eq.'*NORMAL') then
            call normals(text,textpart,iponor,xnor,ixfree,
     &           ipkon,kon,nk,nk_,ne,lakon,istep,istat,in,n)
!
         elseif(textpart(1)(1:5).eq.'*NSET') then
            call noelsets(text,textpart,set,istartset,iendset,ialset,
     &        nset,nset_,nalset,nalset_,nk,ne,istep,istat,in,n)
!
         elseif(textpart(1)(1:12).eq.'*ORIENTATION') then
            call orientations(text,textpart,orname,orab,norien,
     &        norien_,istep,istat,in,n)
!
         elseif(textpart(1)(1:18).eq.'*PHYSICALCONSTANTS') then
            call physicalconstants(text,textpart,physcon,
     &        istep,istat,in,n)
!
         elseif(textpart(1)(1:8).eq.'*PLASTIC') then
            call plastics(text,textpart,nelcon,nmat,ntmat_,npmat_,
     &        plicon,nplicon,plkcon,nplkcon,iplas,iperturb,nstate_,
     &        istep,istat,in,n)
!
         elseif(textpart(1)(1:8).eq.'*RADIATE') then
            call radiates(text,textpart,set,istartset,iendset,
     &        ialset,nset,nelemload,sideload,xload,nload,nload_,
     &        ielmat,ntmat_,iamload,amname,nam,lakon,ne,radiate_flag,
     &        istep,istat,in,n)
            radiate_flag=.true.
!
         elseif(textpart(1)(1:10).eq.'*RIGIDBODY') then
            call rigidbodies(text,textpart,set,istartset,iendset,
     &           ialset,nset,nset_,nalset,nalset_,ipompc,nodempc,
     &           coefmpc,labmpc,nmpc,nmpc_,mpcfree,ikmpc,ilmpc,lakon,
     &           ipkon,kon,nk,nk_,nodeboun,ndirboun,ikboun,ilboun,
     &           nboun,nboun_,iperturb,ne_,ctrl,istep,istat,in,n)
!
         elseif(textpart(1)(1:26).eq.'*SELECTCYCLICSYMMETRYMODES') then
            call selcycsymmods(text,textpart,ns,ics,leftset,istartset,
     &        iendset,ialset,ipompc,nodempc,coefmpc,nmpc,nmpc_,ikmpc,
     &        ilmpc,mpcfree,csab,set,nset,labmpc,istep,istat,in,n)
!
         elseif(textpart(1)(1:13).eq.'*SHELLSECTION') then
            call shellsections(text,textpart,set,istartset,iendset,
     &        ialset,nset,ielmat,matname,nmat,ielorien,orname,
     &        norien,thicke,kon,ipkon,offset,istep,istat,in,n)
!
         elseif(textpart(1)(1:13).eq.'*SOLIDSECTION') then
            call solidsections(text,textpart,set,istartset,iendset,
     &        ialset,nset,ielmat,matname,nmat,ielorien,orname,
     &        norien,lakon,thicke,kon,ipkon,istep,istat,in,n)
!
         elseif(textpart(1)(1:12).eq.'*SPECIFICHEAT') then
            call specificheats(text,textpart,shcon,nshcon,
     &           nmat,ntmat_,istep,istat,in,n)
!
         elseif(textpart(1)(1:7).eq.'*STATIC') then
            call statics(text,textpart,nmethod,iperturb,isolver,istep,
     &        istat,in,n,tinc,tper,tmin,tmax,idrct)
!
         elseif(textpart(1)(1:5).eq.'*STEP') then
            call steps(text,textpart,iperturb,iprestr,om,bodyf,nforc,
     &                 nload,ithermal,t0,t1,nk,istep,istat,in,n,jmax,
     &                 ctrl)
!
         elseif(textpart(1)(1:8).eq.'*SURFACE') then
            call surfaces(text,textpart,set,istartset,iendset,ialset,
     &           nset,nset_,nalset,nalset_,nk,ne,istep,istat,in,n)
!
         elseif(textpart(1)(1:12).eq.'*TEMPERATURE') then
            call temperatures(text,textpart,set,istartset,iendset,
     &        ialset,nset,t0,t1,nk,ithermal,iamt1,amname,nam,
     &        inoelfree,nk_,nmethod,temp_flag,istep,istat,in,n)
            temp_flag=.true.
!
         elseif(textpart(1)(1:4).eq.'*TIE') then
            call ties(text,textpart,leftset,rightset,tietol,istep,
     &           istat,in,n)
!
         elseif(textpart(1)(1:10).eq.'*TRANSFORM') then
            if(M_or_SPC.eq.1) then
               write(*,*) '*WARNING in calinput: SPCs or MPCs have'
               write(*,*) '         been defined before the definition'
               write(*,*) '         of a transformation'
            endif
            call transforms(text,textpart,trab,ntrans,ntrans_,
     &        inotr,set,istartset,iendset,ialset,nset,istep,istat,
     &        in,n)
!
         elseif(textpart(1)(1:13).eq.'*USERMATERIAL') then
            call usermaterials(text,textpart,elcon,nelcon,
     &        nmat,ntmat_,ncmat_,iperturb,iumat,istep,istat,in,n)
!
         elseif(textpart(1)(1:7).eq.'*VISCO') then
            call viscos(text,textpart,nmethod,iperturb,isolver,istep,
     &        istat,in,n,tinc,tper,tmin,tmax,idrct)
!
         elseif(text(1:40).eq.'                                        '
     &       .and.
     &       text(41:80).eq.'                                        '
     &       .and.
     &       text(81:120).eq.'                                        '
     &       .and.
     &       text(121:132).eq.'            '
     &       )
     &       then
            call getnewline(text,textpart,istat,in,n,key)
!     
         else
            write(*,*) '*WARNING in calinput. Card image cannot be inter
     &preted:'
            write(*,'(a132)') text
            call getnewline(text,textpart,istat,in,n,key)
         endif
!           
      enddo loop
!
!     expanding the 1-D and 2-D elements to volume elements
!     treating the incompressibility constraint
!
      call gen3delem(kon,ipkon,lakon,ne,ipompc,nodempc,coefmpc,
     &  nmpc,nmpc_,mpcfree,ikmpc,ilmpc,labmpc,ikboun,ilboun,nboun,
     &  nboun_,nodeboun,ndirboun,xboun,iamboun,nam,
     &  inotr,trab,nk,nk_,iponoel,inoel,iponor,xnor,thicke,thickn,
     &  knor,istep,offset,t0,t1,ikforc,ilforc,rig,nforc,
     &  nforc_,nodeforc,ndirforc,xforc,iamforc,nelemload,sideload,
     &  nload,ithermal,ntrans,co,ixfree,ikfree,inoelfree,iponoelmax,
     &  iperturb,tinc,tper,tmin,tmax,ctrl)
!
      infree(1)=ixfree
      infree(2)=ikfree
      infree(3)=inoelfree
      infree(4)=iponoelmax
!
!     default file printing options and check of the selected options
!
      i=0
      do
         i=i+1
         if(i.gt.nlabel) then
            nodeflab(1)='U   '
            nodeflab(3)='S   '
            jout=max(jout,1)
            exit
         endif
         if(nodeflab(i).ne.'    ') exit
      enddo
!
      if(iplas.eq.0) then
         if((nodeflab(6).eq.'PE  ').or.(noelplab(6).eq.'PE  ')) then
            write(*,*) '*WARNING in calinput: PE-output requested'
            write(*,*) '         yet no (visco)plastic calculation'
            nodeflab(6)='    '
            noelplab(6)='    '
         endif
      endif
!
      if(ithermal.eq.0) then
         if((nodeflab(2).eq.'NT  ').or.(noelplab(2).eq.'NT  ')) then
            write(*,*) '*WARNING in calinput: temperature output'
            write(*,*) '         requested, yet no thermal loading'
            write(*,*) '         active'
            nodeflab(2)='    '
            noelplab(2)='    '
         endif
      endif
!
!     check whether a material was assigned to each active element
!
      ierror=0
      do i=1,ne
         if(ipkon(i).lt.0) cycle
         if(ielmat(i).eq.0) then
            ierror=1
            write(*,*) '*ERROR in calinput: no material was assigned'
            write(*,*) '       to element ',i
         endif
      enddo
      if(ierror.eq.1) stop
!
!     check whether the density was defined for dynamic calculations
!
      dd=dsqrt(bodyf(1)**2+bodyf(2)**2+bodyf(3)**2)
      if((dd.gt.1.d-20).or.(om.gt.1.d-20).or.
     &   (nmethod.eq.2).or.(nmethod.eq.4)) then
         ierror=0
         do i=1,nmat
            if(nrhcon(i).eq.0) then
               ierror=1
               write(*,*) '*ERROR in calinput: no density was assigned'
               write(*,*) '       to material ',i,'in a dynamic'
               write(*,*) '       calculation or a calculation with'
               write(*,*) '       centrifugal or gravitational loads'
            endif
         enddo
         if(ierror.eq.1) stop
      endif
!
!     check whether in case of cyclic symmetry the frequency procedure
!     is chosen
!
      if((ns(2).ne.-1).and.(nmethod.ne.2)) then
         write(*,*) '*ERROR in calinput: the only valid procedure'
         write(*,*) '       for cyclic symmetry calculations in'
         write(*,*) '       CalculiX is *FREQUENCY'
         stop
      endif
!
!     sorting the elements with distributed loads
!
      if(nload.gt.0) then
         call isortidc(nelemload,xload,sideload,nload,kflag)
      endif
!
      write(*,*)
      write(*,*) 'STEP ',istep
      write(*,*)
      if(nmethod.eq.0) then
         write(*,*) 'No analysis was selected'
      elseif(nmethod.eq.1) then
         write(*,*) 'Static analysis was selected'
      elseif(nmethod.eq.2) then
         write(*,*) 'Frequency analysis was selected'
      elseif(nmethod.eq.3) then
         write(*,*) 'Buckling analysis was selected'
      elseif(nmethod.eq.4) then
         write(*,*) 'Linear dynamic analysis was selected'
      endif
      write(*,*)
      if(iperturb.eq.1) then
         write(*,*) 'Perturbation parameter is active'
         write(*,*)
      elseif(iperturb.eq.2) then
         write(*,*) 'Nonlinear geometric effects are taken into account'
         write(*,*)
      elseif(iperturb.eq.3) then
         write(*,*) 'Nonlinear geometric effects and nonlinear '
         write(*,*) 'material laws are taken into account'
         write(*,*)
      endif
!
      return
      end
