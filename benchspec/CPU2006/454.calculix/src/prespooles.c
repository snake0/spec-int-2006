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
#include <misc.h>
#include <FrontMtx.h>
#include <SymbFac.h>
#include "CalculiX.h"

void prespooles(double *co, int *nk, int *kon, int *ipkon, char *lakon,
             int lakonLen, int *ne,
             int *nodeboun, int *ndirboun, double *xboun, int *nboun,
             int *ipompc, int *nodempc, double *coefmpc, char *labmpc,
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
             char *noelplab, int noelplabLen, char *nodeflab, int nodeflabLen, double *eei,
             int *iexpl, double *plicon, int *nplicon, double *plkcon,
             int *nplkcon,
             double *xstate, int *npmat_, char *matname, int matnameLen, int *isolver,
             int *mint_, int *ncmat_, int *nstate_, int *ns, double *csab,
             int *nkon, double *ener, double *xbounold,
             double *xforcold, double *xloadold, double *omold,
             double *bodyfold, char *amname, int amnameLen, double *amta, int *namta,
             int *nam, int *iamforc, int *iamload, int *iamom, int *iambodyf,
             int *iamt1, int *iamboun, double *ttime, char *output, int outputLen){

  int *inum=NULL,k,iout=1,*icol=NULL,*irow=NULL,ielas,icmd,istep=1,iinc=1;
  int mass=0, stiffness=1, buckling=0, rhsi=1, intscheme=0,*ncocon=NULL,
    *iamflow=NULL,nflow,*nshcon=NULL;
  double *stn=NULL,*v=NULL,*een=NULL,vmax,*xstiff=NULL,*stiini=NULL,
         *f=NULL,*fn=NULL,qa,*bb=NULL,*epn=NULL,*xstateini=NULL,
         *vini=NULL,*stx=NULL,*enern=NULL,*xbounact=NULL,*xforcact=NULL,
         *xloadact=NULL,*t1act=NULL,omact,bodyfact[3],*ampli=NULL,
         *xstaten=NULL,*eeiini=NULL,*enerini=NULL,*cocon=NULL,
         *xflowold=NULL,*xflow=NULL,*xflowact=NULL,*shcon=NULL,*physcon=NULL;

  /* dummy arguments for the results call */

  double *veold=NULL,*accold=NULL,bet,gam,dtime,time=1.,reltime=1.;

  icol=*icolp;
  irow=*irowp;

  /* allocating fields for the actual external loading */

  xbounact=NNEW(double,*nboun);
  for(k=0;k<*nboun;++k){xbounact[k]=xbounold[k];}
  xforcact=NNEW(double,*nforc);
  xloadact=NNEW(double,2**nload);
  if(*ithermal==1){
    t1act=NNEW(double,*nk);
    for(k=0;k<*nk;++k){t1act[k]=t1old[k];}
  }

  /* allocating a field for the instantaneous amplitude */

  ampli=NNEW(double,*nam);

  FORTRAN(tempload,(xforcold,xforc,xforcact,iamforc,nforc,xloadold,xload,
	      xloadact,iamload,nload,omold,om,&omact,iamom,bodyfold,
	      bodyf,bodyfact,iambodyf,t1old,t1,t1act,iamt1,nk,amta,
	      namta,nam,ampli,&time,&reltime,ttime,&dtime,ithermal,nmethod,
              xbounold,xboun,xbounact,iamboun,nboun,xflowold,xflow,xflowact,
	      iamflow,&nflow));
  *ttime=*ttime+1.;

  ad=NNEW(double,*neq);
  au=NNEW(double,*nzs);
  b=NNEW(double,*neq);
  bb=NNEW(double,*neq);

#if defined(SPEC_CPU_NAGF95)
  FORTRAN(mafillsm,(co,nk,kon,ipkon,lakon,ne,nodeboun,ndirboun,xbounact,nboun,
	    ipompc,nodempc,coefmpc,nmpc,nodeforc,ndirforc,xforcact,
	    nforc,nelemload,sideload,xloadact,nload,p1,p2,&omact,bodyfact,
	    ad,au,b,bb,nactdof,icol,jq,irow,neq,nzl,nmethod,
	    ikmpc,ilmpc,ikboun,ilboun,
	    elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,ielmat,
	    ielorien,norien,orab,ntmat,
	    t0,t1act,ithermal,prestr,iprestr,vold,iperturb,sti,
	    nzs,stx,adb,aub,eei,iexpl,plicon,nplicon,plkcon,nplkcon,
	    xstiff,npmat_,&dtime,matname,mint_,
            ncmat_,&mass,&stiffness,&buckling,&rhsi,&intscheme,physcon,
            shcon,nshcon,cocon,ncocon,lakonLen,sideloadLen,matnameLen));
#else
  FORTRAN(mafillsm,(co,nk,kon,ipkon,lakon,ne,nodeboun,ndirboun,xbounact,nboun,
	    ipompc,nodempc,coefmpc,nmpc,nodeforc,ndirforc,xforcact,
	    nforc,nelemload,sideload,xloadact,nload,p1,p2,&omact,bodyfact,
	    ad,au,b,bb,nactdof,icol,jq,irow,neq,nzl,nmethod,
	    ikmpc,ilmpc,ikboun,ilboun,
	    elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,ielmat,
	    ielorien,norien,orab,ntmat,
	    t0,t1act,ithermal,prestr,iprestr,vold,iperturb,sti,
	    nzs,stx,adb,aub,eei,iexpl,plicon,nplicon,plkcon,nplkcon,
	    xstiff,npmat_,&dtime,matname,mint_,
            ncmat_,&mass,&stiffness,&buckling,&rhsi,&intscheme,physcon,
            shcon,nshcon,cocon,ncocon));
#endif
  
  free(bb);

  if(*nmethod!=0){

    if(*isolver==0){
      spooles(ad,au,b,icol,irow,neq,nzs);
    }
    else{
      preiter(ad,&au,b,&icol,&irow,neq,nzs,isolver,iperturb);
    }

    free(ad);free(au);

    /* calculating the displacements and the stresses and storing */
    /* the results in frd format for each valid eigenmode */

    v=NNEW(double,4**nk);
    fn=NNEW(double,4**nk);
    stn=NNEW(double,6**nk);
    inum=NNEW(int,*nk);
    stx=NNEW(double,6**mint_**ne);
  
    if(strcmp1(&nodeflab[12],"E   ")==0) een=NNEW(double,6**nk);
    if(strcmp1(&nodeflab[24],"ENER")==0) enern=NNEW(double,*nk);

    if((strcmp1(&nodeflab[24],"ENER")==0)||(strcmp1(&noelplab[24],"ENER")==0)){
	eeiini=NNEW(double,6**mint_**ne);
	stiini=NNEW(double,6**mint_**ne);
	enerini=NNEW(double,*mint_**ne);}

#if defined(SPEC_CPU_NAGF95)
    FORTRAN(results,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nelemprint,neprint,stx,
	    elcon,nelcon,rhcon,nrhcon,alcon,nalcon,alzero,ielmat,
	    ielorien,norien,orab,ntmat,t0,t1,ithermal,
	    prestr,iprestr,noelplab,nodeflab,eei,een,iperturb,
            f,fn,nactdof,&iout,&qa,noprint,nodeprint,vold,b,nodeboun,
	    ndirboun,xboun,nboun,ipompc,
	    nodempc,coefmpc,labmpc,nmpc,nmethod,&vmax,neq,veold,accold,&bet,
            &gam,&dtime,plicon,nplicon,plkcon,nplkcon,
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
	    ndirboun,xboun,nboun,ipompc,
	    nodempc,coefmpc,labmpc,nmpc,nmethod,&vmax,neq,veold,accold,&bet,
            &gam,&dtime,plicon,nplicon,plkcon,nplkcon,
	    xstateini,xstiff,xstate,npmat_,epn,matname,mint_,&ielas,&icmd,
            ncmat_,nstate_,stiini,vini,ikboun,ilboun,ener,enern,sti,
            xstaten,eeiini,enerini,cocon,ncocon));
#endif

    if((strcmp1(&nodeflab[24],"ENER")==0)||(strcmp1(&noelplab[24],"ENER")==0)){
	free(eeiini);free(stiini);free(enerini);}

    for(k=0;k<4**nk;++k){
      vold[k]=v[k];
    }
    for(k=0;k<6**mint_**ne;++k){
      sti[k]=stx[k];
    }

    ++*kode;

    /* for cyclic symmetric sectors: duplicating the results */

    if(ns[4]>1){
      frdcyc(co,nk,kon,ipkon,lakon,lakonLen,ne,v,stn,inum,nmethod,kode,nodeflab,nodeflabLen,een,t1,
		   fn,ttime,epn,ielmat,matname,matnameLen,ns,csab,nkon,enern,xstaten,
                   nstate_,&istep,&iinc,iperturb,ener,mint_,output,outputLen);
    }
    else{
#if defined(SPEC_CPU_NAGF95)
	FORTRAN(out,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nmethod,kode,nodeflab,een,t1,
		     fn,ttime,epn,ielmat,matname,enern,xstaten,nstate_,&istep,&iinc,
		     iperturb,ener,mint_,output,
                     lakonLen,nodeflabLen,matnameLen,outputLen));
#else
	FORTRAN(out,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nmethod,kode,nodeflab,een,t1,
		     fn,ttime,epn,ielmat,matname,enern,xstaten,nstate_,&istep,&iinc,
		     iperturb,ener,mint_,output));
#endif
    }

    free(v);free(stn);free(inum);
    free(b);free(stx);free(fn);

    if(strcmp1(&nodeflab[12],"E   ")==0) free(een);
    if(strcmp1(&nodeflab[24],"ENER")==0) free(enern);

  }
  else {

    /* error occurred in mafill: storing the geometry in frd format */

    ++*kode;
    inum=NNEW(int,*nk);for(k=0;k<*nk;k++) inum[k]=1;
#if defined(SPEC_CPU_NAGF95)
    FORTRAN(out,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nmethod,kode,nodeflab,een,t1,
         fn,ttime,epn,ielmat,matname,enern,xstaten,nstate_,&istep,&iinc,
		     iperturb,ener,mint_,output,
                     lakonLen,nodeflabLen,matnameLen,outputLen));
#else
    FORTRAN(out,(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nmethod,kode,nodeflab,een,t1,
         fn,ttime,epn,ielmat,matname,enern,xstaten,nstate_,&istep,&iinc,
		     iperturb,ener,mint_,output));
#endif
    free(inum);FORTRAN(stop,());

  }

  /* updating the loading at the end of the step; 
     important in case the amplitude at the end of the step
     is not equal to one */

  for(k=0;k<*nboun;++k){xbounold[k]=xbounact[k];}
  for(k=0;k<*nforc;++k){xforcold[k]=xforcact[k];}
  for(k=0;k<2**nload;++k){xloadold[k]=xloadact[k];}
  if(*ithermal==1){
    for(k=0;k<*nk;++k){t1old[k]=t1act[k];}
  }
  *omold=omact;
  for(k=0;k<3;++k){bodyfold[k]=bodyfact[k];}

  free(xbounact);free(xforcact);free(xloadact);free(t1act);free(ampli);

  *icolp=icol;
  *irowp=irow;
 
  return;
}
