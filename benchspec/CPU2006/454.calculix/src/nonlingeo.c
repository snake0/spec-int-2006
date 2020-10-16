/*     CalculiX - A 3-dimensional finite element program                   */
/*              Copyright (C) 1998 Guido Dhondt                          */

/*     This program is free software; you can redistribute it and/or     */
/*     modify it under the terms of the GNU General Public License as    */
/*     published by the Free Software Foundation(version 2);    */
/*                    */

/*     This program is distributed in the hope that it will be useful,   */
/*     but WITHOUT ANY WARRANTY; without even the implied warranty of    */ 
/*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the      */
/*     GNU General Public License for more details.                      */

/*     You should have received a copy of the GNU General Public License */
/*     along with this program; if not, write to the Free Software       */
/*     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.         */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "CalculiX.h"

void nonlingeo(double *co, int *nk, int *kon, int *ipkon, char *lakon,
             int lakonLen, int *ne,
             int *nodeboun, int *ndirboun, double *xboun, int *nboun,
             int *ipompc, int **nodempcp, double **coefmpcp, char *labmpc,
             int labmpcLen, int *nmpc,
             int *nodeforc, int *ndirforc,double *xforc, int *nforc,
             int *nelemload, char *sideload, int sideloadLen, double *xload,
             int *nload, double *p1, double *p2, double *om, double *bodyf,
             double *ad, double *au, double *b, int *nactdof,
             int **icolp, int *jq, int **irowp, int *neq, int *nzl,
             int *nmethod, int *ikmpc, int *ilmpc, int *ikboun,
             int *ilboun,
             double *elcon, int *nelcon, double *rhcon, int *nrhcon,
             double *alcon, int *nalcon, double *alzero, int *ielmat,
             int *ielorien, int *norien, double *orab, int *ntmat,
             double *t0, double *t1, double *t1old,
             int *ithermal,double *prestr, int *iprestr,  
             double *vold,int *iperturb, double *sti, int *nzs, 
             int *nodeprint, int *noprint, int *nelemprint, int *neprint,
             int *kode, double *adb, double *aub,  
             char *noelplab, int noelplabLen, char *nodeflab, int nodeflabLen, int *idrct,
             int *jmax, int *jout, double *tinc, double *tper,
             double *tmin, double *tmax, double *eei, double *xbounold,
             double *xforcold, double *xloadold, double *omold,
             double *bodyfold, double *veold, double *accold,
             char *amname, int amnameLen, double *amta, int *namta, int *nam,
             int *iamforc, int *iamload, int *iamom, int *iambodyf,
             int *iamt1, double *alpha, double *haftol, int *iexpl,
             int *iamboun, double *plicon, int *nplicon, double *plkcon,
             int *nplkcon,
             double *xstate, int *npmat_, int *istep, double *ttime,
             char *matname, int matnameLen, double *qaold, int *mint_,
             int *isolver, int *ncmat_, int *nstate_, int *iumat,
             int *ns, double *csab, int *nkon, double *ener, int *mpcinfo,
             int *nnn, char *output, int outputLen,
             int *nodeflow, int *iamflow, double *xflow,
             double *shcon, int *nshcon, double *cocon, int *ncocon,
             double *physcon, double *xflowold, int *nflow, double *ctrl){

  int *inum=NULL,k,iout=0,icntrl,j=0,jprint=0,l,jnz=0,
       icutb=0,istab=0,nmethodact=1,i0,ir,ip,ic,il,ig,ia,
       newinc,iperturb_sav,ilin,*icol=NULL,*irow=NULL,ielas=0,icmd=0,
       memmpc_,mpcfree,icascade,maxlenmpc,*nodempc=NULL,*iaux=NULL,
       *nodempcref=NULL,nmpcref,memmpcref_,mpcfreeref,*itg=NULL,
       *matg=NULL,ntg,ntr,ntm,*iptri=NULL,*kontri=NULL,*nloadtr=NULL,
       *ipiv=NULL,*idist=NULL,ntri;
  int mass=0, stiffness=1, buckling=0, rhsi=1, intscheme=0;
  double *stn=NULL,*v=NULL,*een=NULL,vmax,*epn=NULL,df,dc,db,dd,
         *f=NULL,*fn=NULL,qa,qam,dtheta,theta,err,ram=0.,ram1=0.,ram2=0.,cam,
         uam=0.,*vini=NULL,*ac=NULL,ran,can,qa0,qau,rap,ea,cae,ral,
         *finc=NULL,*t1act=NULL,c1,c2,qamold,*xbounact=NULL,*bc=NULL,
         *xforcact=NULL,*xloadact=NULL,bodyfact[3],omact,*fext=NULL,
         reltime,time,bet,gam,*aux1=NULL,*aux2=NULL,dtime, *fini=NULL,
         *fextini=NULL,*veini=NULL,*accini=NULL,*xstateini=NULL,
         *ampli=NULL,dextrapol,scal1,scal2,*eeiini=NULL,*t1ini=NULL,
         *xbounini=NULL,dev,*xstiff=NULL,*stx=NULL,*stiini=NULL,
         *enern=NULL,*coefmpc=NULL,*aux=NULL,*xstaten=NULL,
         *coefmpcref=NULL,*enerini=NULL,*xflowact=NULL,*area=NULL,
         *tarea=NULL,*tenv=NULL,*dist=NULL,*erad=NULL,*pmid=NULL,
         *ft=NULL,*fij=NULL,*e1=NULL,*e2=NULL,*e3=NULL;

  i0=ctrl[0];ir=ctrl[1];ip=ctrl[2];ic=ctrl[3];il=ctrl[4];ig=ctrl[5];ia=ctrl[7];
  df=ctrl[10];dc=ctrl[11];db=ctrl[12];dd=ctrl[16];
  ran=ctrl[18];can=ctrl[19];qa0=ctrl[20];qau=ctrl[21];rap=ctrl[22];
  ea=ctrl[23];cae=ctrl[24];ral=ctrl[25];

  memmpc_=mpcinfo[0];mpcfree=mpcinfo[1];icascade=mpcinfo[2];
  maxlenmpc=mpcinfo[3];

  icol=*icolp;irow=*irowp;

  nodempc=*nodempcp;coefmpc=*coefmpcp;
  if(icascade==2){
      nmpcref=*nmpc;memmpcref_=memmpc_;mpcfreeref=mpcfree;
      nodempcref=NNEW(int,3*memmpc_);
      for(k=0;k<3*memmpc_;k++){nodempcref[k]=nodempc[k];}
      coefmpcref=NNEW(double,memmpc_);
      for(k=0;k<memmpc_;k++){coefmpcref[k]=coefmpc[k];}
  }

  /* allocating a field for the stiffness matrix */

  xstiff=NNEW(double,21**mint_**ne);

  /* allocating force fields */

  f=NNEW(double,*neq);
  finc=NNEW(double,*neq);
  fext=NNEW(double,*neq);

  b=NNEW(double,*neq);
  vini=NNEW(double,4**nk);

  aux=NNEW(double,7*maxlenmpc);
  iaux=NNEW(int,maxlenmpc);

  /* allocating fields for the actual external loading */

  xbounact=NNEW(double,*nboun);
  xbounini=NNEW(double,*nboun);
  for(k=0;k<*nboun;++k){xbounact[k]=xbounold[k];}
  xforcact=NNEW(double,*nforc);
  xloadact=NNEW(double,2**nload);

  /* for thermal calculations: forced convection and cavity
     radiation*/

  if(*ithermal>1){
      xflowact=NNEW(double,*nflow);
      itg=NNEW(int,*nload);
      matg=NNEW(int,*nload);
      iptri=NNEW(int,*nload);
      kontri=NNEW(int,6**nload);
      nloadtr=NNEW(int,*nload);
#if defined(SPEC_CPU_NAGF95)
      FORTRAN(envtemp,(itg,matg,&ntg,&ntr,sideload,nelemload,
		       ipkon,kon,lakon,ielmat,ne,nload,iptri,
                       kontri,&ntri,nloadtr, 
                       sideloadLen,lakonLen));
#else
      FORTRAN(envtemp,(itg,matg,&ntg,&ntr,sideload,nelemload,
		       ipkon,kon,lakon,ielmat,ne,nload,iptri,
                       kontri,&ntri,nloadtr));
#endif
      RENEW(itg,int,ntg);
      RENEW(matg,int,ntg);
      RENEW(iptri,int,ntri);
      RENEW(kontri,int,3*ntri);
      RENEW(nloadtr,int,ntr);

      area=NNEW(double,ntri);
      pmid=NNEW(double,3*ntri);
      e1=NNEW(double,3*ntri);
      e2=NNEW(double,3*ntri);
      e3=NNEW(double,4*ntri);
      ft=NNEW(double,ntri*ntri);
      dist=NNEW(double,ntri);
      idist=NNEW(int,ntri);

      fij=NNEW(double,ntr*ntr);
      tarea=NNEW(double,ntr);
      tenv=NNEW(double,ntr);
      erad=NNEW(double,ntr);
      if(ntg>ntr){
	  ntm=ntg;}
      else{
	  ntm=ntr;}
      ac=NNEW(double,ntm*ntm);
      bc=NNEW(double,ntm);
      ipiv=NNEW(int,ntm);
  }

  if(*ithermal==1){
    t1ini=NNEW(double,*nk);
    t1act=NNEW(double,*nk);
    for(k=0;k<*nk;++k){t1act[k]=t1old[k];}
  }

  /* allocating a field for the instantaneous amplitude */

  ampli=NNEW(double,*nam);

  /* allocating fields for nonlinear dynamics */

  fini=NNEW(double,*neq);
  if(*nmethod==4){
    aux2=NNEW(double,*neq);
    fextini=NNEW(double,*neq);
    veini=NNEW(double,4**nk);
    accini=NNEW(double,4**nk);
    adb=NNEW(double,*neq);
    aub=NNEW(double,*nzs);
  }

  if(*nstate_!=0){
    xstateini=NNEW(double,*nstate_**mint_**ne);
    for(k=0;k<*nstate_**mint_**ne;++k){
      xstateini[k]=xstate[k];
    }
  }
  eeiini=NNEW(double,6**mint_**ne);
  stiini=NNEW(double,6**mint_**ne);
  if((strcmp1(&nodeflab[24],"ENER")==0)||(strcmp1(&noelplab[24],"ENER")==0))
      enerini=NNEW(double,*mint_**ne);

  qa=*qaold;

  /* normalizing the time */

  dtheta=(*tinc)/(*tper);
  if(dtheta<=1.e-6){
    printf("\n*ERROR in nonlingeo\n");
    printf("increment size smaller than one millionth of step size\n");
    printf("increase increment size\n\n");
  }
  *tmin=*tmin/(*tper);
  *tmax=*tmax/(*tper);
  theta=0.;

  /* calculating an initial flux norm */
  
  if(qau>1.e-10){qam=qau;}
  else if(qa0>1.e-10){qam=qa0;}
  else if(qa>1.e-10){qam=qa;}
  else {qam=1.e-2;}

  /* calculating the initial acceleration at the start of the step
     for dynamic calculations */

  if(*nmethod==4){
    bet=(1.-*alpha)*(1.-*alpha)/4.;
    gam=0.5-*alpha;
    mass=1;

    /* calculating the stiffness and mass matrix 
       the stress must be determined to calculate the 
       stiffness matrix*/

    fn=NNEW(double,4**nk);
    stx=NNEW(double,6**mint_**ne);
    
    iout=-1;
    dtime=1.;
    ielas=1;
    
    /*  updating the nonlinear mpc's (also affects the boundary
	conditions through the nonhomogeneous part of the mpc's)
        if contact arises the number of MPC's can also change */

    if(*ithermal>1){

#if defined(SPEC_CPU_NAGF95)
       FORTRAN(radflowload,(itg,matg,&ntg,&ntr,&ntm,nodeflow,
       xflowact,ac,bc,nload,sideload,nelemload,xloadact,lakon,ipiv,ntmat,vold,
       shcon,nshcon,ipkon,kon,co,pmid,e1,e2,e3,iptri,
       kontri,&ntri,nloadtr,tarea,tenv,physcon,erad,fij,ft,
       dist,idist,area,nflow,sideloadLen,lakonLen));
#else
       FORTRAN(radflowload,(itg,matg,&ntg,&ntr,&ntm,nodeflow,
       xflowact,ac,bc,nload,sideload,nelemload,xloadact,lakon,ipiv,ntmat,vold,
       shcon,nshcon,ipkon,kon,co,pmid,e1,e2,e3,iptri,
       kontri,&ntri,nloadtr,tarea,tenv,physcon,erad,fij,ft,
       dist,idist,area,nflow));
#endif
    }

    if(icascade==2){
	*nmpc=nmpcref;memmpc_=memmpcref_;mpcfree=mpcfreeref;
	RENEW(nodempc,int,3*memmpcref_);
	for(k=0;k<3*memmpcref_;k++){nodempc[k]=nodempcref[k];}
	RENEW(coefmpc,double,memmpcref_);
	for(k=0;k<memmpcref_;k++){coefmpc[k]=coefmpcref[k];}
    }
#if defined(SPEC_CPU_NAGF95)
    FORTRAN(nonlinmpc,(co,vold,ipompc,nodempc,coefmpc,labmpc,
		       nmpc,ikboun,ilboun,nboun,xbounold,aux,iaux,
		       &maxlenmpc,ikmpc,ilmpc,&icascade,
		       kon,ipkon,lakon,ne,labmpcLen,lakonLen));
#else
    FORTRAN(nonlinmpc,(co,vold,ipompc,nodempc,coefmpc,labmpc,
		       nmpc,ikboun,ilboun,nboun,xbounold,aux,iaux,
		       &maxlenmpc,ikmpc,ilmpc,&icascade,
		       kon,ipkon,lakon,ne));
#endif
    if(icascade==2){
	nmpcref=*nmpc;memmpcref_=memmpc_;mpcfreeref=mpcfree;
	RENEW(nodempcref,int,3*memmpc_);
	for(k=0;k<3*memmpc_;k++){nodempcref[k]=nodempc[k];}
	RENEW(coefmpcref,double,memmpc_);
	for(k=0;k<memmpc_;k++){coefmpcref[k]=coefmpc[k];}
    }

    if(icascade>0) remastruct(ipompc,&coefmpc,&nodempc,nmpc,
              &mpcfree,nodeboun,ndirboun,nboun,ikmpc,ilmpc,ikboun,ilboun,
              labmpc,labmpcLen,nk,&memmpc_,&icascade,&maxlenmpc,
              kon,ipkon,lakon,lakonLen,ne,nnn,nactdof,icol,jq,&irow,isolver,
	      neq,nzs,nmethod,&f,&finc,&fext,&b,&aux2,&fini,&fextini,
	      &adb,&aub,ithermal);

#if defined(SPEC_CPU_NAGF95)
    FORTRAN(results,(co,nk,kon,ipkon,lakon,ne,vold,stn,inum,nelemprint,neprint,stx,
	  elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,ielmat,
	  ielorien,norien,orab,ntmat,t0,t1old,ithermal,
	  prestr,iprestr,noelplab,nodeflab,eei,een,iperturb,
	  f,fn,nactdof,&iout,&qa,noprint,nodeprint,vold,b,nodeboun,
	  ndirboun,xbounold,nboun,ipompc,
	  nodempc,coefmpc,labmpc,nmpc,nmethod,&cam,neq,veold,accold,&bet,
          &gam,&dtime,plicon,nplicon,plkcon,nplkcon,
	  xstateini,xstiff,xstate,npmat_,epn,matname,mint_,&ielas,&icmd,
          ncmat_,nstate_,sti,vini,ikboun,ilboun,ener,enern,sti,xstaten,
          eeiini,enerini,cocon,ncocon, 
          lakonLen,noelplabLen,nodeflabLen,labmpcLen,matnameLen));
#else
    FORTRAN(results,(co,nk,kon,ipkon,lakon,ne,vold,stn,inum,nelemprint,neprint,stx,
	  elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,ielmat,
	  ielorien,norien,orab,ntmat,t0,t1old,ithermal,
	  prestr,iprestr,noelplab,nodeflab,eei,een,iperturb,
	  f,fn,nactdof,&iout,&qa,noprint,nodeprint,vold,b,nodeboun,
	  ndirboun,xbounold,nboun,ipompc,
	  nodempc,coefmpc,labmpc,nmpc,nmethod,&cam,neq,veold,accold,&bet,
          &gam,&dtime,plicon,nplicon,plkcon,nplkcon,
	  xstateini,xstiff,xstate,npmat_,epn,matname,mint_,&ielas,&icmd,
          ncmat_,nstate_,sti,vini,ikboun,ilboun,ener,enern,sti,xstaten,
          eeiini,enerini,cocon,ncocon));
#endif
    
    iout=0;
    ielas=0;
    
    free(fn);free(stx);

    if(*iexpl==0){intscheme=1;}

    ad=NNEW(double,*neq);
    au=NNEW(double,*nzs);

#if defined(SPEC_CPU_NAGF95)
    FORTRAN(mafillsm,(co,nk,kon,ipkon,lakon,ne,nodeboun,ndirboun,xbounold,nboun,
	      ipompc,nodempc,coefmpc,nmpc,nodeforc,ndirforc,xforcold,
	      nforc,nelemload,sideload,xloadold,nload,p1,p2,omold,
	      bodyfold,ad,au,finc,fext,nactdof,icol,jq,irow,neq,nzl,
	      &nmethodact,ikmpc,ilmpc,ikboun,ilboun,
	      elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,
	      ielmat,ielorien,norien,orab,ntmat,
	      t0,t1old,ithermal,prestr,iprestr,vold,iperturb,sti,
	      nzs,stx,adb,aub,eei,iexpl,plicon,nplicon,plkcon,nplkcon,
	      xstiff,npmat_,&dtime,matname,mint_,
              ncmat_,&mass,&stiffness,&buckling,&rhsi,&intscheme,
	      physcon,shcon,nshcon,cocon,ncocon,lakonLen,sideloadLen,matnameLen));
#else
    FORTRAN(mafillsm,(co,nk,kon,ipkon,lakon,ne,nodeboun,ndirboun,xbounold,nboun,
	      ipompc,nodempc,coefmpc,nmpc,nodeforc,ndirforc,xforcold,
	      nforc,nelemload,sideload,xloadold,nload,p1,p2,omold,
	      bodyfold,ad,au,finc,fext,nactdof,icol,jq,irow,neq,nzl,
	      &nmethodact,ikmpc,ilmpc,ikboun,ilboun,
	      elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,
	      ielmat,ielorien,norien,orab,ntmat,
	      t0,t1old,ithermal,prestr,iprestr,vold,iperturb,sti,
	      nzs,stx,adb,aub,eei,iexpl,plicon,nplicon,plkcon,nplkcon,
	      xstiff,npmat_,&dtime,matname,mint_,
              ncmat_,&mass,&stiffness,&buckling,&rhsi,&intscheme,
	      physcon,shcon,nshcon,cocon,ncocon));
#endif

    if(nmethodact==0){
      
      /* error occurred in mafill: storing the geometry in frd format */

      ++*kode;
#if defined(SPEC_CPU_NAGF95)
      FORTRAN(out,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nmethod,kode,nodeflab,een,t1,
	   fn,ttime,epn,ielmat,matname,enern,xstaten,nstate_,istep,&j,
		     iperturb,ener,mint_,output,
                     lakonLen,nodeflabLen,matnameLen,outputLen));
#else
      FORTRAN(out,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nmethod,kode,nodeflab,een,t1,
	   fn,ttime,epn,ielmat,matname,enern,xstaten,nstate_,istep,&j,
		     iperturb,ener,mint_,output));
#endif
      
      FORTRAN(stop,());
      
    }

    /* calculating the acceleration at the start of the step.
       This can be different from the acceleration at the end
       of the last step due to a discontinuous loading increase */

    reltime=0.;
    time=0.;

    FORTRAN(tempload,(xforcold,xforc,xforcact,iamforc,nforc,xloadold,xload,
	      xloadact,iamload,nload,omold,om,&omact,iamom,bodyfold,
	      bodyf,bodyfact,iambodyf,t1old,t1,t1act,iamt1,nk,amta,
	      namta,nam,ampli,&time,&reltime,ttime,&dtime,ithermal,nmethod,
              xbounold,xboun,xbounact,iamboun,nboun,xflowold,xflow,xflowact,
	      iamflow,nflow));

    /* determining the external loading vector */

#if defined(SPEC_CPU_NAGF95)
    FORTRAN(rhs,(co,nk,kon,ipkon,lakon,ne,
	 ipompc,nodempc,coefmpc,nmpc,nodeforc,ndirforc,xforcact,
	 nforc,nelemload,sideload,xloadact,nload,p1,p2,&omact,
	 bodyfact,fext,nactdof,neq,
	 &nmethodact,ikmpc,ilmpc,
	 elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,
	 ielmat,ielorien,norien,orab,ntmat,
	 t0,t1act,ithermal,prestr,iprestr,vold,iperturb,
	 iexpl,plicon,nplicon,plkcon,nplkcon,
	 npmat_,lakonLen,sideloadLen));
#else
    FORTRAN(rhs,(co,nk,kon,ipkon,lakon,ne,
	 ipompc,nodempc,coefmpc,nmpc,nodeforc,ndirforc,xforcact,
	 nforc,nelemload,sideload,xloadact,nload,p1,p2,&omact,
	 bodyfact,fext,nactdof,neq,
	 &nmethodact,ikmpc,ilmpc,
	 elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,
	 ielmat,ielorien,norien,orab,ntmat,
	 t0,t1act,ithermal,prestr,iprestr,vold,iperturb,
	 iexpl,plicon,nplicon,plkcon,nplkcon,
	 npmat_));
#endif

    /* mass x acceleration = f(external)-f(internal) */

    for(k=0;k<*neq;++k){
      b[k]=fext[k]-f[k];
    }

    if(*iexpl==0){

      /* a small amount of stiffness is added to the mass matrix
       otherwise the system leads to huge accelerations in 
       case of discontinuous load changes at the start of the step */

/*	dtime=*tinc/10.;
	scal1=bet*dtime*dtime*(1.+*alpha); */
	for(k=0;k<*neq;++k){
	    ad[k]=adb[k]; /* +scal1*ad[k]; */
	}
	for(k=0;k<*nzs;++k){
	    au[k]=aub[k]; /* +scal1*au[k]; */
	}
	if(*isolver==0){
	    spooles(ad,au,b,icol,irow,neq,nzs);
	}
	else{
	    preiter(ad,&au,b,&icol,&irow,neq,nzs,isolver,iperturb);
	}
    }

    else{
      for(k=0;k<*neq;++k){
	b[k]=(fext[k]-f[k])/adb[k];
      }
    }

    for(k=0;k<4**nk;++k){
      if(nactdof[k]!=0){accold[k]=b[nactdof[k]-1];}
    }

    free(ad);free(au);
    mass=0;intscheme=0;

  }

  if(*iexpl!=0) icmd=3;

  /**************************************************************/
  /* starting the loop over the increments                      */
  /**************************************************************/

  while(dtheta>1.e-6){
    
    /* previous increment converged: update the initial values */

    newinc=1;

    if(icutb==0){
      j++;
      jprint++;
      for(k=0;k<4**nk;++k){vini[k]=vold[k];}
      for(k=0;k<*nboun;++k){xbounini[k]=xbounact[k];}
      if(*ithermal==1){
	for(k=0;k<*nk;++k){t1ini[k]=t1act[k];}
      }
	for(k=0;k<*neq;++k){
	  fini[k]=f[k];
	}
      if(*nmethod==4){
	for(k=0;k<4**nk;++k){
	  veini[k]=veold[k];
	  accini[k]=accold[k];
	}
	for(k=0;k<*neq;++k){
	    /*  fini[k]=f[k];*/
	  fextini[k]=fext[k];
	}
      }
      for(k=0;k<6**mint_**ne;++k){
	eeiini[k]=eei[k];
	stiini[k]=sti[k];
      }
      if((strcmp1(&nodeflab[24],"ENER")==0)||(strcmp1(&noelplab[24],"ENER")==0))
	  for(k=0;k<*mint_**ne;++k){enerini[k]=ener[k];}
      if(*nstate_!=0){
	for(k=0;k<*nstate_**mint_**ne;++k){
	  xstateini[k]=xstate[k];
	}
      }	
    }

    /* check for max. # of increments */

    if(j>*jmax){
      printf("*ERROR: max. # of increments reached\n\n");
      FORTRAN(stop,());
    }
    printf("increment %d attempt %d \n",j,icutb+1);
    printf("increment size= %e\n",dtheta**tper);
    printf("sum of previous increments=%e\n\n",theta**tper);

    qamold=qam;

    /* determining the actual loads at the end of the new increment*/

    reltime=theta+dtheta;
    time=reltime**tper;
    dtime=dtheta**tper;

    FORTRAN(tempload,(xforcold,xforc,xforcact,iamforc,nforc,xloadold,xload,
	      xloadact,iamload,nload,omold,om,&omact,iamom,bodyfold,
	      bodyf,bodyfact,iambodyf,t1old,t1,t1act,iamt1,nk,amta,
	      namta,nam,ampli,&time,&reltime,ttime,&dtime,ithermal,nmethod,
              xbounold,xboun,xbounact,iamboun,nboun,xflowold,xflow,xflowact,
              iamflow,nflow));

    if(*ithermal>1){
#if defined(SPEC_CPU_NAGF95)
       FORTRAN(radflowload,(itg,matg,&ntg,&ntr,&ntm,nodeflow,
       xflowact,ac,bc,nload,sideload,nelemload,xloadact,lakon,ipiv,ntmat,vold,
       shcon,nshcon,ipkon,kon,co,pmid,e1,e2,e3,iptri,
       kontri,&ntri,nloadtr,tarea,tenv,physcon,erad,fij,ft,
       dist,idist,area,nflow,sideloadLen,lakonLen));
#else
       FORTRAN(radflowload,(itg,matg,&ntg,&ntr,&ntm,nodeflow,
       xflowact,ac,bc,nload,sideload,nelemload,xloadact,lakon,ipiv,ntmat,vold,
       shcon,nshcon,ipkon,kon,co,pmid,e1,e2,e3,iptri,
       kontri,&ntri,nloadtr,tarea,tenv,physcon,erad,fij,ft,
       dist,idist,area,nflow));
#endif
    }

    /*  updating the nonlinear mpc's (also affects the boundary
	conditions through the nonhomogeneous part of the mpc's) */

    if(icascade==2){
	*nmpc=nmpcref;memmpc_=memmpcref_;mpcfree=mpcfreeref;
	RENEW(nodempc,int,3*memmpcref_);
	for(k=0;k<3*memmpcref_;k++){nodempc[k]=nodempcref[k];}
	RENEW(coefmpc,double,memmpcref_);
	for(k=0;k<memmpcref_;k++){coefmpc[k]=coefmpcref[k];}
    }
#if defined(SPEC_CPU_NAGF95)
    FORTRAN(nonlinmpc,(co,vold,ipompc,nodempc,coefmpc,labmpc,
		       nmpc,ikboun,ilboun,nboun,xbounact,aux,iaux,
		       &maxlenmpc,ikmpc,ilmpc,&icascade,
		       kon,ipkon,lakon,ne,labmpcLen,lakonLen));
#else
    FORTRAN(nonlinmpc,(co,vold,ipompc,nodempc,coefmpc,labmpc,
		       nmpc,ikboun,ilboun,nboun,xbounact,aux,iaux,
		       &maxlenmpc,ikmpc,ilmpc,&icascade,
		       kon,ipkon,lakon,ne));
#endif
    if(icascade==2){
	nmpcref=*nmpc;memmpcref_=memmpc_;mpcfreeref=mpcfree;
	RENEW(nodempcref,int,3*memmpc_);
	for(k=0;k<3*memmpc_;k++){nodempcref[k]=nodempc[k];}
	RENEW(coefmpcref,double,memmpc_);
	for(k=0;k<memmpc_;k++){coefmpcref[k]=coefmpc[k];}
    }

    if(icascade>0) remastruct(ipompc,&coefmpc,&nodempc,nmpc,
	  &mpcfree,nodeboun,ndirboun,nboun,ikmpc,ilmpc,ikboun,ilboun,
	  labmpc,labmpcLen,nk,&memmpc_,&icascade,&maxlenmpc,
	  kon,ipkon,lakon,lakonLen,ne,nnn,nactdof,icol,jq,&irow,isolver,
	  neq,nzs,nmethod,&f,&finc,&fext,&b,&aux2,&fini,&fextini,
	      &adb,&aub,ithermal);

    /* check whether the forced displacements changed; if so, and
       if the procedure is static, the first iteration has to be
       purely linear elastic, in order to get an equilibrium
       displacement field; otherwise huge (maybe nonelastic)
       stresses may occur, jeopardizing convergence */

    ilin=0;
    dev=0.;
    for(k=0;k<*nboun;++k){
      err=fabs(xbounact[k]-xbounini[k]);
      if(err>dev){dev=err;}
    }
    if(dev>1.e-10) ilin=1;
    printf("ilin=%d\n",ilin);

    /* prediction of the kinematic vectors  */

    v=NNEW(double,4**nk);

    uam=0.;
    if(*nmethod==4){
      
/*      scal1=(0.5)*dtime*dtime;
	scal2=(1.)*dtime;*/
	scal1=0.5*(1.-2.*bet)*dtime*dtime;
	scal2=(1.-gam)*dtime;
	
	for(k=0;k<4**nk;++k){
	    dextrapol=dtime*veold[k]+scal1*accold[k];
	    if(fabs(dextrapol)>uam) {uam=fabs(dextrapol);}
	    v[k]=vold[k]+dextrapol;
	    veold[k]=veold[k]+scal2*accold[k];
	    accold[k]=0.;
	}
    }
    
    /* for the static case: extrapolation of the previous increment
       (if any within the same step) */
    
    else{
      if(*nstate_==0){
	  /*if(j>1){*/
	    for(k=0;k<4**nk;++k){
		dextrapol=dtime*veold[k];
		if(fabs(dextrapol)>uam) {uam=fabs(dextrapol);}	
		v[k]=vold[k]+dextrapol;
	    }
	}
	else{
	    for(k=0;k<4**nk;++k){
		v[k]=vold[k];
	    }
	}
    }
    
    fn=NNEW(double,4**nk);
    stx=NNEW(double,6**mint_**ne);

    /* determining the internal forces at the start of the increment

       for a static calculation with increased forced displacements
       the linear strains are calculated corresponding to

       the displacements at the end of the previous increment, extrapolated
       if appropriate (for nondispersive media) +
       the forced displacements at the end of the present increment +
       the temperatures at the end of the present increment (this sum is
       v) -
       the displacements at the end of the previous increment (this is vold)

       these linear strains are converted in stresses by multiplication
       with the tangent element stiffness matrix and converted into nodal
       forces. 

       this boils down to the fact that the effect of forced displacements
       should be handled in a purely linear way at the
       start of a new increment, in order to speed up the convergence and
       (for dissipative media) guarantee smooth loading within the increment.

       for all other cases the nodal force calculation is based on
       the true stresses derived from the appropriate strain tensor taking
       into account the extrapolated displacements at the end of the 
       previous increment + the forced displacements and the temperatures
       at the end of the present increment */

    iout=-1;
    iperturb_sav=*iperturb;

    /* first iteration in first increment: elastic tangent */

    ielas=1;

    if((*nmethod!=4)&&(ilin==1)){
      *iperturb=0;

         for(k=0;k<*neq;++k){b[k]=f[k];}
#if defined(SPEC_CPU_NAGF95)
      FORTRAN(results,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nelemprint,neprint,stx,
	       elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,ielmat,
	       ielorien,norien,orab,ntmat,t1ini,t1act,ithermal,
	       prestr,iprestr,noelplab,nodeflab,eei,een,iperturb,
	       f,fn,nactdof,&iout,&qa,noprint,nodeprint,vold,b,nodeboun,
	       ndirboun,xbounact,nboun,ipompc,
	       nodempc,coefmpc,labmpc,nmpc,nmethod,&cam,neq,veold,accold,
	       &bet,&gam,&dtime,plicon,nplicon,plkcon,nplkcon,
	       xstateini,xstiff,xstate,npmat_,epn,matname,mint_,&ielas,
               &icmd, ncmat_,nstate_,stiini,vini,ikboun,ilboun,ener,enern,
               sti,xstaten,eeiini,enerini,cocon,ncocon, 
               lakonLen,noelplabLen,nodeflabLen,labmpcLen,matnameLen));
#else
      FORTRAN(results,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nelemprint,neprint,stx,
	       elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,ielmat,
	       ielorien,norien,orab,ntmat,t1ini,t1act,ithermal,
	       prestr,iprestr,noelplab,nodeflab,eei,een,iperturb,
	       f,fn,nactdof,&iout,&qa,noprint,nodeprint,vold,b,nodeboun,
	       ndirboun,xbounact,nboun,ipompc,
	       nodempc,coefmpc,labmpc,nmpc,nmethod,&cam,neq,veold,accold,
	       &bet,&gam,&dtime,plicon,nplicon,plkcon,nplkcon,
	       xstateini,xstiff,xstate,npmat_,epn,matname,mint_,&ielas,
               &icmd, ncmat_,nstate_,stiini,vini,ikboun,ilboun,ener,enern,
               sti,xstaten,eeiini,enerini,cocon,ncocon));
#endif
      
      /* check whether any displacements or temperatures are changed
	 in the new increment */
      
      for(k=0;k<*neq;++k){f[k]=f[k]+b[k];}

    }
    else{

#if defined(SPEC_CPU_NAGF95)
      FORTRAN(results,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nelemprint,neprint,stx,
	       elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,ielmat,
	       ielorien,norien,orab,ntmat,t0,t1act,ithermal,
	       prestr,iprestr,noelplab,nodeflab,eei,een,iperturb,
	       f,fn,nactdof,&iout,&qa,noprint,nodeprint,vold,b,nodeboun,
	       ndirboun,xbounact,nboun,ipompc,
	       nodempc,coefmpc,labmpc,nmpc,nmethod,&cam,neq,veold,accold,
	       &bet,&gam,&dtime,plicon,nplicon,plkcon,nplkcon,
	       xstateini,xstiff,xstate,npmat_,epn,matname,mint_,&ielas,
               &icmd,ncmat_,nstate_,stiini,vini,ikboun,ilboun,ener,enern,
               sti,xstaten,eeiini,enerini,cocon,ncocon, 
               lakonLen,noelplabLen,nodeflabLen,labmpcLen,matnameLen));
#else
      FORTRAN(results,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nelemprint,neprint,stx,
	       elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,ielmat,
	       ielorien,norien,orab,ntmat,t0,t1act,ithermal,
	       prestr,iprestr,noelplab,nodeflab,eei,een,iperturb,
	       f,fn,nactdof,&iout,&qa,noprint,nodeprint,vold,b,nodeboun,
	       ndirboun,xbounact,nboun,ipompc,
	       nodempc,coefmpc,labmpc,nmpc,nmethod,&cam,neq,veold,accold,
	       &bet,&gam,&dtime,plicon,nplicon,plkcon,nplkcon,
	       xstateini,xstiff,xstate,npmat_,epn,matname,mint_,&ielas,
               &icmd,ncmat_,nstate_,stiini,vini,ikboun,ilboun,ener,enern,
               sti,xstaten,eeiini,enerini,cocon,ncocon));
#endif

      for(k=0;k<4**nk;++k){
	vold[k]=v[k];
      }

      for(k=0;k<6**mint_**ne;++k){
	sti[k]=stx[k];
      }

    }

    ielas=0;
    iout=0;

    free(fn);free(stx);free(v);

    /***************************************************************/
    /* iteration counter and start of the loop over the iterations */
    /***************************************************************/

    l=1;
    icntrl=0;

    while(icntrl==0){

      printf("iteration %d\n\n",l);

    /*  updating the nonlinear mpc's (also affects the boundary
	conditions through the nonhomogeneous part of the mpc's) */

      if(newinc!=1){

	  if(*ithermal>1){
#if defined(SPEC_CPU_NAGF95)
             FORTRAN(radflowload,(itg,matg,&ntg,&ntr,&ntm,
             nodeflow,
	     xflowact,ac,bc,nload,sideload,nelemload,xloadact,lakon,ipiv,
             ntmat,vold,shcon,nshcon,ipkon,kon,co,pmid,e1,e2,e3,
             iptri,kontri,&ntri,nloadtr,tarea,tenv,physcon,erad,fij,ft,
	     dist,idist,area,nflow,sideloadLen,lakonLen));
#else
             FORTRAN(radflowload,(itg,matg,&ntg,&ntr,&ntm,
             nodeflow,
	     xflowact,ac,bc,nload,sideload,nelemload,xloadact,lakon,ipiv,
             ntmat,vold,shcon,nshcon,ipkon,kon,co,pmid,e1,e2,e3,
             iptri,kontri,&ntri,nloadtr,tarea,tenv,physcon,erad,fij,ft,
	     dist,idist,area,nflow));
#endif
          }

	  if(icascade==2){
	      *nmpc=nmpcref;memmpc_=memmpcref_;mpcfree=mpcfreeref;
	      RENEW(nodempc,int,3*memmpcref_);
	      for(k=0;k<3*memmpcref_;k++){nodempc[k]=nodempcref[k];}
	      RENEW(coefmpc,double,memmpcref_);
	      for(k=0;k<memmpcref_;k++){coefmpc[k]=coefmpcref[k];}
	  }
#if defined(SPEC_CPU_NAGF95)
	  FORTRAN(nonlinmpc,(co,vold,ipompc,nodempc,coefmpc,labmpc,
			     nmpc,ikboun,ilboun,nboun,xbounact,aux,iaux,
	                     &maxlenmpc,ikmpc,ilmpc,&icascade,
			     kon,ipkon,lakon,ne,labmpcLen,lakonLen));
#else
	  FORTRAN(nonlinmpc,(co,vold,ipompc,nodempc,coefmpc,labmpc,
			     nmpc,ikboun,ilboun,nboun,xbounact,aux,iaux,
	                     &maxlenmpc,ikmpc,ilmpc,&icascade,
			     kon,ipkon,lakon,ne));
#endif
	  if(icascade==2){
	      nmpcref=*nmpc;memmpcref_=memmpc_;mpcfreeref=mpcfree;
	      RENEW(nodempcref,int,3*memmpc_);
	      for(k=0;k<3*memmpc_;k++){nodempcref[k]=nodempc[k];}
	      RENEW(coefmpcref,double,memmpc_);
	      for(k=0;k<memmpc_;k++){coefmpcref[k]=coefmpc[k];}
	  }

	if(icascade>0) remastruct(ipompc,&coefmpc,&nodempc,nmpc,
	  &mpcfree,nodeboun,ndirboun,nboun,ikmpc,ilmpc,ikboun,ilboun,
	  labmpc,labmpcLen,nk,&memmpc_,&icascade,&maxlenmpc,
	  kon,ipkon,lakon,lakonLen,ne,nnn,nactdof,icol,jq,&irow,isolver,
	  neq,nzs,nmethod,&f,&finc,&fext,&b,&aux2,&fini,&fextini,
	  &adb,&aub,ithermal);
      }

      if(*iexpl==0){

	/* calculating the local stiffness matrix and external loading */

	ad=NNEW(double,*neq);
	au=NNEW(double,*nzs);

	nmethodact=1;

#if defined(SPEC_CPU_NAGF95)
	FORTRAN(mafillsm,(co,nk,kon,ipkon,lakon,ne,nodeboun,ndirboun,xbounact,nboun,
		  ipompc,nodempc,coefmpc,nmpc,nodeforc,ndirforc,xforcact,
		  nforc,nelemload,sideload,xloadact,nload,p1,p2,&omact,
		  bodyfact,ad,au,finc,fext,nactdof,icol,jq,irow,neq,nzl,
		  &nmethodact,ikmpc,ilmpc,ikboun,ilboun,
		  elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,
		  ielmat,ielorien,norien,orab,ntmat,
		  t0,t1act,ithermal,prestr,iprestr,vold,iperturb,sti,
		  nzs,stx,adb,aub,eei,iexpl,plicon,nplicon,plkcon,nplkcon,
		  xstiff,npmat_,&dtime,matname,mint_,
                  ncmat_,&mass,&stiffness,&buckling,&rhsi,&intscheme,
                  physcon,shcon,nshcon,cocon,ncocon,lakonLen,sideloadLen,matnameLen));
#else
	FORTRAN(mafillsm,(co,nk,kon,ipkon,lakon,ne,nodeboun,ndirboun,xbounact,nboun,
		  ipompc,nodempc,coefmpc,nmpc,nodeforc,ndirforc,xforcact,
		  nforc,nelemload,sideload,xloadact,nload,p1,p2,&omact,
		  bodyfact,ad,au,finc,fext,nactdof,icol,jq,irow,neq,nzl,
		  &nmethodact,ikmpc,ilmpc,ikboun,ilboun,
		  elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,
		  ielmat,ielorien,norien,orab,ntmat,
		  t0,t1act,ithermal,prestr,iprestr,vold,iperturb,sti,
		  nzs,stx,adb,aub,eei,iexpl,plicon,nplicon,plkcon,nplkcon,
		  xstiff,npmat_,&dtime,matname,mint_,
                  ncmat_,&mass,&stiffness,&buckling,&rhsi,&intscheme,
                  physcon,shcon,nshcon,cocon,ncocon));
#endif

	*iperturb=iperturb_sav;

	/* residual for a static analysis (only for first iteration
           in a new increment); in all other cases the residual is
           determined after calling results.f */

	if(newinc==1){
	    if(*nmethod!=4){
		for(k=0;k<*neq;++k){
		    b[k]=fext[k]-f[k];
		}
	    }
	    
	    /* residual for implicit dynamics */
	    
	    else {
		for(k=0;k<4**nk;++k){
		    if(nactdof[k]!=0){aux2[nactdof[k]-1]=accold[k];}
		}
		FORTRAN(op,(neq,aux1,aux2,b,adb,aub,icol,irow,nzl)); 
		scal1=1.+*alpha;
		for(k=0;k<*neq;++k){
		    b[k]=scal1*(fext[k]-f[k])-*alpha*(fextini[k]-fini[k])-b[k];
		}
	    }
	}
      }

      /* residual for explicit dynamics */
      
      else{

	/* calculating the external loading 

	   This is only done once per increment. In reality, the
           external loading is a function of vold (specifically,
           the body forces and surface loading). This effect is
           neglected, since the increment size in dynamic explicit
           calculations is usually small */

	if(newinc==1){ 
#if defined(SPEC_CPU_NAGF95)
	  FORTRAN(rhs,(co,nk,kon,ipkon,lakon,ne,
		  ipompc,nodempc,coefmpc,nmpc,nodeforc,ndirforc,xforcact,
		  nforc,nelemload,sideload,xloadact,nload,p1,p2,&omact,
		  bodyfact,fext,nactdof,neq,
		  &nmethodact,ikmpc,ilmpc,
		  elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,
		  ielmat,ielorien,norien,orab,ntmat,
		  t0,t1act,ithermal,prestr,iprestr,vold,iperturb,
		  iexpl,plicon,nplicon,plkcon,nplkcon,
		  npmat_,lakonLen,sideloadLen));
#else
	  FORTRAN(rhs,(co,nk,kon,ipkon,lakon,ne,
		  ipompc,nodempc,coefmpc,nmpc,nodeforc,ndirforc,xforcact,
		  nforc,nelemload,sideload,xloadact,nload,p1,p2,&omact,
		  bodyfact,fext,nactdof,neq,
		  &nmethodact,ikmpc,ilmpc,
		  elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,
		  ielmat,ielorien,norien,orab,ntmat,
		  t0,t1act,ithermal,prestr,iprestr,vold,iperturb,
		  iexpl,plicon,nplicon,plkcon,nplkcon,
		  npmat_));
#endif
	  for(k=0;k<4**nk;++k){
	    if(nactdof[k]!=0){aux2[nactdof[k]-1]=accold[k];}
	  }
	  scal1=1.+*alpha;
	  for(k=0;k<*neq;++k){
	    b[k]=scal1*(fext[k]-f[k])-*alpha*(fextini[k]-fini[k])
	      -adb[k]*aux2[k];
	  }
	} 
      }

      newinc=0;
    
      if((nmethodact==0)||(*nmethod==0)){

	/* error occurred in mafill: storing the geometry in frd format */

	++*kode;
	inum=NNEW(int,*nk);for(k=0;k<*nk;k++) inum[k]=1;
#if defined(SPEC_CPU_NAGF95)
	FORTRAN(out,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nmethod,kode,nodeflab,een,t1,
             fn,ttime,epn,ielmat,matname,enern,xstaten,nstate_,istep,&j,
		     iperturb,ener,mint_,output,
		     lakonLen,nodeflabLen,matnameLen,outputLen));
#else
	FORTRAN(out,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nmethod,kode,nodeflab,een,t1,
             fn,ttime,epn,ielmat,matname,enern,xstaten,nstate_,istep,&j,
		     iperturb,ener,mint_,output));
#endif
	free(inum);FORTRAN(stop,());

      }
      else{

	/* implicit step (static or dynamic */

	if(*iexpl==0){
	  if(*nmethod==4){
	    scal1=bet*dtime*dtime*(1.+*alpha);
	    for(k=0;k<*neq;++k){
	      ad[k]=adb[k]+scal1*ad[k];
	    }
	    for(k=0;k<*nzs;++k){
	      au[k]=aub[k]+scal1*au[k];
	    }
	  }

	  if(*isolver==0){
	    spooles(ad,au,b,icol,irow,neq,nzs);
	  }
	  else{
	    preiter(ad,&au,b,&icol,&irow,neq,nzs,isolver,iperturb);
	  }

	  free(ad);free(au); 
	}

	/* explicit dynamic step */

	else{
	  for(k=0;k<*neq;++k){
	    b[k]=b[k]/adb[k];
	  }
	}

	/* calculating the displacements, stresses and forces */

	v=NNEW(double,4**nk);
	stx=NNEW(double,6**mint_**ne);
	fn=NNEW(double,4**nk);

#if defined(SPEC_CPU_NAGF95)
	FORTRAN(results,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nelemprint,neprint,stx,
		elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,ielmat,
		ielorien,norien,orab,ntmat,t0,t1act,ithermal,
		prestr,iprestr,noelplab,nodeflab,eei,een,iperturb,
		f,fn,nactdof,&iout,&qa,noprint,nodeprint,vold,b,nodeboun,
		ndirboun,xbounact,nboun,ipompc,
	        nodempc,coefmpc,labmpc,nmpc,nmethod,&cam,neq,veold,accold,
                &bet,&gam,&dtime,plicon,nplicon,plkcon,nplkcon,
	        xstateini,xstiff,xstate,npmat_,epn,matname,mint_,&ielas,
                &icmd,ncmat_,nstate_,stiini,vini,ikboun,ilboun,ener,enern,
                sti,xstaten,eeiini,enerini,cocon,ncocon, 
                lakonLen,noelplabLen,nodeflabLen,labmpcLen,matnameLen));
#else
	FORTRAN(results,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nelemprint,neprint,stx,
		elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,ielmat,
		ielorien,norien,orab,ntmat,t0,t1act,ithermal,
		prestr,iprestr,noelplab,nodeflab,eei,een,iperturb,
		f,fn,nactdof,&iout,&qa,noprint,nodeprint,vold,b,nodeboun,
		ndirboun,xbounact,nboun,ipompc,
	        nodempc,coefmpc,labmpc,nmpc,nmethod,&cam,neq,veold,accold,
                &bet,&gam,&dtime,plicon,nplicon,plkcon,nplkcon,
	        xstateini,xstiff,xstate,npmat_,epn,matname,mint_,&ielas,
                &icmd,ncmat_,nstate_,stiini,vini,ikboun,ilboun,ener,enern,
                sti,xstaten,eeiini,enerini,cocon,ncocon));
#endif

	if(cam>uam){uam=cam;}

	if(qau<1.e-10){
	  if(qa>ea*qam){qam=(qamold*jnz+qa)/(jnz+1);}
	  else {qam=qamold;}
	}

	for(k=0;k<4**nk;++k){
	  vold[k]=v[k];
	}
	for(k=0;k<6**mint_**ne;++k){
	  sti[k]=stx[k];
	}

	free(v);free(stx);free(fn);

	/* calculating the maximum residual */

	ram2=ram1;
	ram1=ram;
	ram=0.;

	/* residual for a static analysis */

	if(*nmethod!=4){
	  for(k=0;k<*neq;++k){
	    b[k]=fext[k]-f[k];
	  }
	}

	/* residual for implicit dynamics */

	else if(*iexpl==0){
	  for(k=0;k<4**nk;++k){
	    if(nactdof[k]!=0){aux2[nactdof[k]-1]=accold[k];}
	  }
	  FORTRAN(op,(neq,aux1,aux2,b,adb,aub,icol,irow,nzl));
	  scal1=1.+*alpha;
	  for(k=0;k<*neq;++k){
	    b[k]=scal1*(fext[k]-f[k])-*alpha*(fextini[k]-fini[k])-b[k];
	  }
	}

	/* residual for explicit dynamics */
      
	else{
	  for(k=0;k<4**nk;++k){
	    if(nactdof[k]!=0){aux2[nactdof[k]-1]=accold[k];}
	  }
	  scal1=1.+*alpha;
	  for(k=0;k<*neq;++k){
	    b[k]=scal1*(fext[k]-f[k])-*alpha*(fextini[k]-fini[k])
	      -adb[k]*aux2[k];
	  }
	}

	for(k=0;k<*neq;++k){
	  err=fabs(b[k]);
	  if(err>ram){ram=err;}
	}

	/* next line is inserted to cope with stress-less
           temperature calculations */

	if(ram<1.e-6) ram=0.;

#ifndef SPEC_CPU
/* reduce i/o, but leave a tiny bit behind for possible debug usefulness */
	printf("average force= %f\n",qa);
	printf("time avg. forc= %f\n",qam);
#endif
	printf("largest residual force= %f\n",ram);
#ifndef SPEC_CPU
	printf("largest increment of disp.= %f\n",uam);
	printf("largest correction to disp= %f\n",cam);
#endif

	if(qa>ea*qam){
	  if(l<=ip){c1=ran;}
	  else{c1=rap;}
	  c2=can;
	}
	else{
	  c1=ea;
	  c2=cae;
	}

	if(ram1<ram2){ram2=ram1;}
	if((ram<=c1*qam)&&
	  ((cam<=c2*uam)||(ram*cam/ram2<c2*uam)||(ram<=ral*qam)||
             (qa<=ea*qam))){

	  /* increment convergence reached */

	  *ttime=*ttime+dtime;
	  FORTRAN(writesummary,(istep,&j,&icutb,&l,ttime,&time,&dtime));

	  icntrl=1;
	  icutb=0;
	  theta=theta+dtheta;

	  /* defining a mean "velocity" for static calculations: is used to
	     extrapolate the present results for next increment */

	  if(*nmethod != 4){
	    for(k=0;k<4**nk;++k){
	      veold[k]=(vold[k]-vini[k])/dtime;
	    }
	  }

	  /* check whether next increment size must be decreased */

	  if(l>il){
	    dtheta=dtheta*db;
	    printf("convergence; dtheta is decreased to %f\n\n",dtheta**tper);
	  }
	  
	  /* check whether next increment size can be increased */
	  
	  else if(l<=ig){
	    if((istab==1)&&(*idrct==0)){
	      dtheta=dtheta*dd;
	      printf("convergence; dtheta is increased to %f\n\n",dtheta**tper);
	    }
	    else{
	      istab=1;
	      printf("convergence\n\n");
	    }
	  }
	  else{
	    istab=0;
	    printf("convergence\n\n");
	  }

	  if((dtheta>*tmax)&&(*idrct==0)){
	    dtheta=*tmax;
	    printf("dtheta exceeds thetamax and is decreased to %f\n\n",dtheta**tper);
	  }
	  if(dtheta>1.-theta){
	    dtheta=1.-theta;
	    printf("dtheta exceeds the remainder of the step and is decreased to %f\n\n",dtheta**tper);
	  }
	}
	else{

	  /* check for the amount of iterations */

	  if(l>ic){
	    printf("*ERROR: too many iterations needed\n\n");
	    FORTRAN(stop,());
	  }	

	  /* check for diverging residuals */

	  if(l>=i0){
	    if((ram1>ram2)&&(ram>ram2)&&(ram>c1*qam)){
	      if(*idrct==1) {
		printf("*ERROR: solution seems to diverge; please try \n");
		printf("automatic incrementation; program stops\n\n");
		FORTRAN(stop,());
	      }
	      else {
		dtheta=dtheta*df;
		printf("divergence; dtheta is decreased to %f\n",dtheta**tper);
		printf("the increment is reattempted\n\n");
		istab=0;
		if(dtheta<*tmin){
		  printf("*ERROR: increment size smaller than minimum\n\n");
		  FORTRAN(stop,());
		}
		icntrl=1;
		icutb++;
		if(icutb>ia){
		  printf("*ERROR: too many cutbacks\n\n");
		  FORTRAN(stop,());
		}
		continue;
	      }
	    }
	  }

	  /* check for too slow convergence */

	  if(l>=ir){
	    printf("estimated number of iterations till convergence = %d\n",
		   (int)ceil(l+log(ran*qam/ram)/log(ram/ram1)));
	    if((l+log(ran*qam/ram)/log(ram/ram1))>ic){
	      if(*idrct!=1){
		dtheta=dtheta*dc;
		printf("too slow convergence; dtheta is decreased to %f\n",dtheta**tper);
		printf("the increment is reattempted\n\n");
		istab=0;
		if(dtheta<*tmin){
		  printf("*ERROR: increment size smaller than minimum\n\n");
		  FORTRAN(stop,());
		}
		icntrl=1;
		icutb++;
		if(icutb>ia){
		  printf("*ERROR: too many cutbacks\n\n");
		  FORTRAN(stop,());
		}
		continue;
	      }
	    }
	  }

	  printf("no convergence\n\n");

	  l++;
			 
	}
      }
    }

    /*********************************************************/
    /*   end of the iteration loop                          */
    /*********************************************************/

    /* icutb=0 means that the iterations in the increment converged,
       icutb!=0 indicates that the increment has to be reiterated with
                another increment size (dtheta) */

    if((qa>ea*qam)&&(icutb==0)){jnz++;}

    if(icutb!=0){
      for(k=0;k<4**nk;++k){vold[k]=vini[k];}
      for(k=0;k<*nboun;++k){xbounact[k]=xbounini[k];}
      if(*ithermal==1){
	for(k=0;k<*nk;++k){t1act[k]=t1ini[k];}
      }
	for(k=0;k<*neq;++k){
	  f[k]=fini[k];
	}
      if(*nmethod==4){
	for(k=0;k<4**nk;++k){
	  veold[k]=veini[k];
	  accold[k]=accini[k];
	}
	for(k=0;k<*neq;++k){
	    /*  f[k]=fini[k];*/
	  fext[k]=fextini[k];
	}
      }
      for(k=0;k<6**mint_**ne;++k){
	eei[k]=eeiini[k];
	sti[k]=stiini[k];
      }
      if((strcmp1(&nodeflab[24],"ENER")==0)||(strcmp1(&noelplab[24],"ENER")==0))
	  for(k=0;k<*mint_**ne;++k){ener[k]=enerini[k];}
      if(*nstate_!=0){
	for(k=0;k<*nstate_**mint_**ne;++k){
	  xstate[k]=xstateini[k];
	}	
      }

      qam=qamold;
    }
    else{
      /*      if(*ithermal==1){
	for(k=0;k<*nk;++k){t1old[k]=t1act[k];}
      }*/
    }
    
    if((*jout==jprint)&&(icutb==0)){

      jprint=0;

      /* calculating the displacements and the stresses and storing */
      /* the results in frd format  */
	
      v=NNEW(double,4**nk);
      fn=NNEW(double,4**nk);
      stn=NNEW(double,6**nk);
      inum=NNEW(int,*nk);
      stx=NNEW(double,6**mint_**ne);
      
      if(strcmp1(&nodeflab[12],"E   ")==0) een=NNEW(double,6**nk);
      if(strcmp1(&nodeflab[20],"PE  ")==0) epn=NNEW(double,*nk);
      if(strcmp1(&nodeflab[24],"ENER")==0) enern=NNEW(double,*nk);
      if(strcmp1(&nodeflab[28],"SDV ")==0) xstaten=NNEW(double,*nstate_**nk);
      
      for(k=0;k<4**nk;++k){
	v[k]=vold[k];
      }
      iout=2;
      icmd=3;
      
#if defined(SPEC_CPU_NAGF95)
      FORTRAN(results,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nelemprint,neprint,stx,
	      elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,ielmat,
	      ielorien,norien,orab,ntmat,t0,t1act,ithermal,
	      prestr,iprestr,noelplab,nodeflab,eei,een,iperturb,
	      f,fn,nactdof,&iout,&qa,noprint,nodeprint,vold,b,nodeboun,
	      ndirboun,xbounact,nboun,ipompc,
	      nodempc,coefmpc,labmpc,nmpc,nmethod,&vmax,neq,veold,accold,
              &bet,&gam,&dtime,plicon,nplicon,plkcon,nplkcon,
	      xstateini,xstiff,xstate,npmat_,epn,matname,mint_,&ielas,&icmd,
	      ncmat_,nstate_,stiini,vini,ikboun,ilboun,ener,enern,sti,
              xstaten,eeiini,enerini,cocon,ncocon, 
              lakonLen,noelplabLen,nodeflabLen,labmpcLen,matnameLen));
#else
      FORTRAN(results,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nelemprint,neprint,stx,
	      elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,ielmat,
	      ielorien,norien,orab,ntmat,t0,t1act,ithermal,
	      prestr,iprestr,noelplab,nodeflab,eei,een,iperturb,
	      f,fn,nactdof,&iout,&qa,noprint,nodeprint,vold,b,nodeboun,
	      ndirboun,xbounact,nboun,ipompc,
	      nodempc,coefmpc,labmpc,nmpc,nmethod,&vmax,neq,veold,accold,
              &bet,&gam,&dtime,plicon,nplicon,plkcon,nplkcon,
	      xstateini,xstiff,xstate,npmat_,epn,matname,mint_,&ielas,&icmd,
	      ncmat_,nstate_,stiini,vini,ikboun,ilboun,ener,enern,sti,
              xstaten,eeiini,enerini,cocon,ncocon));
#endif

      iout=0;
      if(*iexpl==0) icmd=0;
      
      ++*kode;
      if(ns[4]>1){
	frdcyc(co,nk,kon,ipkon,lakon,lakonLen,ne,v,stn,inum,nmethod,kode,nodeflab,nodeflabLen,een,
	       t1act,fn,ttime,epn,ielmat,matname,matnameLen,ns,csab,nkon,enern,xstaten,
               nstate_,istep,&j,iperturb,ener,mint_,output,outputLen);
      }
      else{
#if defined(SPEC_CPU_NAGF95)
	FORTRAN(out,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nmethod,kode,
		     nodeflab,een,t1act,fn,ttime,epn,ielmat,matname,enern,
		     xstaten,nstate_,istep,&j,iperturb,ener,mint_,output,
		     lakonLen,nodeflabLen,matnameLen,outputLen));
#else
	FORTRAN(out,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nmethod,kode,
		     nodeflab,een,t1act,fn,ttime,epn,ielmat,matname,enern,
		     xstaten,nstate_,istep,&j,iperturb,ener,mint_,output));
#endif
      }
      
      free(v);free(fn);free(stn);free(inum);free(stx);
      
      if(strcmp1(&nodeflab[12],"E   ")==0) free(een);
      if(strcmp1(&nodeflab[20],"PE  ")==0) free(epn);
      if(strcmp1(&nodeflab[24],"ENER")==0) free(enern);
       if(strcmp1(&nodeflab[28],"SDV ")==0) free(xstaten);
   }
    
  }

  /*********************************************************/
  /*   end of the increment loop                          */
  /*********************************************************/

  free(finc);

  if(jprint!=0){

  /* calculating the displacements and the stresses and storing  
     the results in frd format */
  
    v=NNEW(double,4**nk);
    fn=NNEW(double,4**nk);
    stn=NNEW(double,6**nk);
    inum=NNEW(int,*nk);
    stx=NNEW(double,6**mint_**ne);
  
    if(strcmp1(&nodeflab[12],"E   ")==0) een=NNEW(double,6**nk);
    if(strcmp1(&nodeflab[20],"PE  ")==0) epn=NNEW(double,*nk);
    if(strcmp1(&nodeflab[24],"ENER")==0) enern=NNEW(double,*nk);
    if(strcmp1(&nodeflab[28],"SDV ")==0) xstaten=NNEW(double,*nstate_**nk);
    
    for(k=0;k<4**nk;++k){
      v[k]=vold[k];
    }
    iout=2;
    icmd=3;

#if defined(SPEC_CPU_NAGF95)
    FORTRAN(results,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nelemprint,neprint,stx,
	    elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,ielmat,
	    ielorien,norien,orab,ntmat,t0,t1,ithermal,
	    prestr,iprestr,noelplab,nodeflab,eei,een,iperturb,
	    f,fn,nactdof,&iout,&qa,noprint,nodeprint,vold,b,nodeboun,
	    ndirboun,xbounact,nboun,ipompc,
	    nodempc,coefmpc,labmpc,nmpc,nmethod,&vmax,neq,veold,accold,
            &bet,&gam,&dtime,plicon,nplicon,plkcon,nplkcon,
	    xstateini,xstiff,xstate,npmat_,epn,matname,mint_,&ielas,&icmd,
            ncmat_,nstate_,stiini,vini,ikboun,ilboun,ener,enern,sti,
            xstaten,eeiini,enerini,cocon,ncocon, 
            lakonLen,noelplabLen,nodeflabLen,labmpcLen,matnameLen));
#else
    FORTRAN(results,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nelemprint,neprint,stx,
	    elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,ielmat,
	    ielorien,norien,orab,ntmat,t0,t1,ithermal,
	    prestr,iprestr,noelplab,nodeflab,eei,een,iperturb,
	    f,fn,nactdof,&iout,&qa,noprint,nodeprint,vold,b,nodeboun,
	    ndirboun,xbounact,nboun,ipompc,
	    nodempc,coefmpc,labmpc,nmpc,nmethod,&vmax,neq,veold,accold,
            &bet,&gam,&dtime,plicon,nplicon,plkcon,nplkcon,
	    xstateini,xstiff,xstate,npmat_,epn,matname,mint_,&ielas,&icmd,
            ncmat_,nstate_,stiini,vini,ikboun,ilboun,ener,enern,sti,
            xstaten,eeiini,enerini,cocon,ncocon));
#endif
    
    iout=0;
    if(*iexpl==0) icmd=0;
    
    ++*kode;
    if(ns[4]>1){
      frdcyc(co,nk,kon,ipkon,lakon,lakonLen,ne,v,stn,inum,nmethod,kode,nodeflab,nodeflabLen,een,
	     t1act,fn,ttime,epn,ielmat,matname,matnameLen,ns,csab,nkon,enern,xstaten,
             nstate_,istep,&j,iperturb,ener,mint_,output,outputLen);
    }
    else{
#if defined(SPEC_CPU_NAGF95)
      FORTRAN(out,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nmethod,kode,nodeflab,
		   een,t1act,fn,ttime,epn,ielmat,matname,enern,xstaten,
		   nstate_,istep,&j,iperturb,ener,mint_,output,
		   lakonLen,nodeflabLen,matnameLen,outputLen));
#else
      FORTRAN(out,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nmethod,kode,nodeflab,
		   een,t1act,fn,ttime,epn,ielmat,matname,enern,xstaten,
		   nstate_,istep,&j,iperturb,ener,mint_,output));
#endif
    }
    
    free(v);free(fn);free(stn);free(inum);free(stx);
    
    if(strcmp1(&nodeflab[12],"E   ")==0) free(een);
    if(strcmp1(&nodeflab[20],"PE  ")==0) free(epn);
    if(strcmp1(&nodeflab[24],"ENER")==0) free(enern);
    if(strcmp1(&nodeflab[28],"SDV ")==0) free(xstaten);

  }

  /* updating the loading at the end of the step; 
     important in case the amplitude at the end of the step
     is not equal to one */

  for(k=0;k<*nboun;++k){xbounold[k]=xbounact[k];}
  for(k=0;k<*nforc;++k){xforcold[k]=xforcact[k];}
  for(k=0;k<2**nload;++k){xloadold[k]=xloadact[k];}
  if(*ithermal>1){for(k=0;k<*nflow;++k){xflowold[k]=xflowact[k];}}
  if(*ithermal==1){
    for(k=0;k<*nk;++k){t1old[k]=t1act[k];}
  }
  *omold=omact;
  for(k=0;k<3;++k){bodyfold[k]=bodyfact[k];}

  *qaold=qa;

  free(f);free(b);free(xbounact);free(xforcact);free(xloadact);free(xflowact);
  free(fext);free(ampli);free(xbounini);free(xstiff);
  if(*ithermal==1){free(t1act);free(t1ini);}

  if(*ithermal>1){
      free(itg);free(matg);free(iptri);free(kontri);free(nloadtr);
      free(area);free(pmid);free(ft);
      free(dist);free(idist);free(fij);free(tarea);free(tenv);
      free(erad);free(ac);free(bc);free(ipiv);free(e1);free(e2);free(e3);
  }

  free(fini);  
  if(*nmethod==4){
    free(aux2);free(fextini);free(veini);free(accini);
    free(adb);free(aub);free(aux1);
  }
  free(eeiini);free(stiini);
  if((strcmp1(&nodeflab[24],"ENER")==0)||(strcmp1(&noelplab[24],"ENER")==0))
      free(enerini);
  if(*nstate_!=0){free(xstateini);}

  free(aux);free(iaux);free(vini);

  *icolp=icol;*irowp=irow;

  if(icascade==2){
      *nmpc=nmpcref;memmpc_=memmpcref_;mpcfree=mpcfreeref;
      RENEW(nodempc,int,3*memmpcref_);
      for(k=0;k<3*memmpcref_;k++){nodempc[k]=nodempcref[k];}
      RENEW(coefmpc,double,memmpcref_);
      for(k=0;k<memmpcref_;k++){coefmpc[k]=coefmpcref[k];}
      free(nodempcref);free(coefmpcref);
  }

  mpcinfo[0]=memmpc_;mpcinfo[1]=mpcfree;mpcinfo[2]=icascade;
  mpcinfo[3]=maxlenmpc;

  *nodempcp=nodempc;*coefmpcp=coefmpc;
  
  return;
}
