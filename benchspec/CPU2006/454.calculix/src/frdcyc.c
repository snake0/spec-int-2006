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
#include <string.h>
#include "CalculiX.h"

void frdcyc(double *co,int *nk,int *kon,int *ipkon,char *lakon,int lakonLen, int *ne,double *v,
            double *stn,int *inum,int *nmethod,int *kode,char *nodeflab,
            int nodeflabLen, double *een,double *t1,double *fn,double *time,double *epn,
            int *ielmat,char *matname, int matnameLen, int *ns, double *csab, int *nkon,
            double *enern, double *xstaten, int *nstate_, int *istep,
            int *iinc, int *iperturb, double *ener, int *mint_, char *output, int outputLen){

  /* duplicates fields for static cyclic symmetric calculations */

  char *lakont=NULL;
  int nkt,icntrl,*kont=NULL,*ipkont=NULL,*inumt=NULL,*ielmatt=NULL,net,i,l,
     imag=0;
  double *vt=NULL,*fnt=NULL,*stnt=NULL,*eent=NULL,*cot=NULL,*t1t=NULL,
         *epnt=NULL,*enernt=NULL,*xstatent=NULL,theta,pi,t[3];

  pi=4.*atan(1.);

  cot=NNEW(double,3**nk*ns[4]);

  if(strcmp1(&nodeflab[0],"U   ")==0)
    vt=NNEW(double,4**nk*ns[4]);
  if(strcmp1(&nodeflab[4],"NT  ")==0)
    t1t=NNEW(double,*nk*ns[4]);
  if(strcmp1(&nodeflab[8],"S   ")==0)
    stnt=NNEW(double,6**nk*ns[4]);
  if(strcmp1(&nodeflab[12],"E   ")==0)
    eent=NNEW(double,6**nk*ns[4]);
  if(strcmp1(&nodeflab[16],"RF  ")==0)
    fnt=NNEW(double,4**nk*ns[4]);
  if(strcmp1(&nodeflab[20],"PE  ")==0)
    epnt=NNEW(double,*nk*ns[4]);
  if(strcmp1(&nodeflab[24],"ENER")==0)
    enernt=NNEW(double,*nk*ns[4]);
  if(strcmp1(&nodeflab[28],"SDV ")==0)
    xstatent=NNEW(double,*nstate_**nk*ns[4]);

  /* the topology only needs duplication the first time it is
     stored in the frd file (*kode=1) */

  if(*kode==1){
    kont=NNEW(int,*nkon*ns[4]);
    ipkont=NNEW(int,*ne*ns[4]);
    lakont=NNEW(char,8**ne*ns[4]);
    ielmatt=NNEW(int,*ne*ns[4]);
  }
  inumt=NNEW(int,*nk*ns[4]);
  
  nkt=ns[4]**nk;
  net=ns[4]**ne;

  /* copying the coordinates of the first sector */
  
  for(l=0;l<3**nk;l++){cot[l]=co[l];}

  /* copying the topology of the first sector */
  
  if(*kode==1){
      for(l=0;l<*nkon;l++){kont[l]=kon[l];}
      for(l=0;l<*ne;l++){ipkont[l]=ipkon[l];}
      for(l=0;l<8**ne;l++){lakont[l]=lakon[l];}
      for(l=0;l<*ne;l++){ielmatt[l]=ielmat[l];}
  }  

  /* generating the coordinates for the other sectors */
  
  icntrl=1;
  
#if defined(SPEC_CPU_NAGF95)
  FORTRAN(rectcyl,(cot,v,fn,stn,een,csab,nk,&icntrl,t,nodeflab,&imag,nodeflabLen));
#else
  FORTRAN(rectcyl,(cot,v,fn,stn,een,csab,nk,&icntrl,t,nodeflab,&imag));
#endif
  
  for(i=1;i<ns[4];i++){
    
      theta=i*2.*pi/ns[0];
    
      for(l=0;l<*nk;l++){
	  cot[3*l+i*3**nk]=cot[3*l];
	  cot[1+3*l+i*3**nk]=cot[1+3*l]-theta;
	  cot[2+3*l+i*3**nk]=cot[2+3*l];
      }

      if(*kode==1){

	  for(l=0;l<*nkon;l++){kont[l+i**nkon]=kon[l]+i**nk;}
	  for(l=0;l<*ne;l++){
	      if(ipkon[l]>=0) ipkont[l+i**ne]=ipkon[l]+i**nkon;
	      else ipkont[l+i**ne]=-1;
	  }
	  for(l=0;l<8**ne;l++){lakont[l+i*8**ne]=lakon[l];}
	  for(l=0;l<*ne;l++){ielmatt[l+i**ne]=ielmat[l];}
      }
  }
    
  icntrl=-1;
    
#if defined(SPEC_CPU_NAGF95)
  FORTRAN(rectcyl,(cot,vt,fnt,stnt,eent,csab,&nkt,&icntrl,t,nodeflab,&imag,nodeflabLen));
#else
  FORTRAN(rectcyl,(cot,vt,fnt,stnt,eent,csab,&nkt,&icntrl,t,nodeflab,&imag));
#endif
  
  /* mapping the results to the other sectors */
  
  for(l=0;l<*nk;l++){inumt[l]=inum[l];}
  
  icntrl=2;
  
#if defined(SPEC_CPU_NAGF95)
  FORTRAN(rectcyl,(co,v,fn,stn,een,csab,nk,&icntrl,t,nodeflab,&imag,nodeflabLen));
#else
  FORTRAN(rectcyl,(co,v,fn,stn,een,csab,nk,&icntrl,t,nodeflab,&imag));
#endif
  
  if(strcmp1(&nodeflab[0],"U   ")==0)
    for(l=0;l<4**nk;l++){vt[l]=v[l];};
  if(strcmp1(&nodeflab[4],"NT  ")==0)
    for(l=0;l<*nk;l++){t1t[l]=t1[l];};
  if(strcmp1(&nodeflab[8],"S   ")==0)
    for(l=0;l<6**nk;l++){stnt[l]=stn[l];};
  if(strcmp1(&nodeflab[12],"E   ")==0)
    for(l=0;l<6**nk;l++){eent[l]=een[l];};
  if(strcmp1(&nodeflab[16],"RF  ")==0)
    for(l=0;l<4**nk;l++){fnt[l]=fn[l];};
  if(strcmp1(&nodeflab[20],"PE  ")==0)
    for(l=0;l<*nk;l++){epnt[l]=epn[l];};
  if(strcmp1(&nodeflab[24],"ENER")==0)
    for(l=0;l<*nk;l++){enernt[l]=enern[l];};
  if(strcmp1(&nodeflab[28],"SDV ")==0)
    for(l=0;l<*nstate_**nk;l++){xstatent[l]=xstaten[l];};
  
  for(i=1;i<ns[4];i++){
    
    for(l=0;l<*nk;l++){inumt[l+i**nk]=inum[l];}
    
    if(strcmp1(&nodeflab[0],"U   ")==0){
      for(l=0;l<4**nk;l++){
	vt[l+4**nk*i]=v[l];
      }
    }
    
    if(strcmp1(&nodeflab[4],"NT  ")==0){
      for(l=0;l<*nk;l++){
	t1t[l+*nk*i]=t1[l];
      }
    }
    
    if(strcmp1(&nodeflab[8],"S   ")==0){
      for(l=0;l<6**nk;l++){
	stnt[l+6**nk*i]=stn[l];
      }
    }
    
    if(strcmp1(&nodeflab[12],"E   ")==0){
      for(l=0;l<6**nk;l++){
	eent[l+6**nk*i]=een[l];
      }
    }
    
    if(strcmp1(&nodeflab[16],"RF  ")==0){
      for(l=0;l<4**nk;l++){
	fnt[l+4**nk*i]=fn[l];
      }
    }
    
    if(strcmp1(&nodeflab[20],"PE  ")==0){
      for(l=0;l<*nk;l++){
	epnt[l+*nk*i]=epn[l];
      }
    } 
    
    if(strcmp1(&nodeflab[24],"ENER")==0){
      for(l=0;l<*nk;l++){
	enernt[l+*nk*i]=enern[l];
      }
    } 
    
    if(strcmp1(&nodeflab[28],"SDV ")==0){
      for(l=0;l<*nstate_**nk;l++){
	xstatent[l+*nstate_**nk*i]=xstaten[l];
      }
    } 
   
  }
  
  icntrl=-2;
  
#if defined(SPEC_CPU_NAGF95)
  FORTRAN(rectcyl,(cot,vt,fnt,stnt,eent,csab,&nkt,&icntrl,t,nodeflab,&imag,nodeflabLen));
#else
  FORTRAN(rectcyl,(cot,vt,fnt,stnt,eent,csab,&nkt,&icntrl,t,nodeflab,&imag));
#endif
  
#if defined(SPEC_CPU_NAGF95)
  FORTRAN(out,(cot,&nkt,kont,ipkont,lakont,&net,vt,stnt,inumt,nmethod,kode,
	       nodeflab,eent,t1t,fnt,time,epnt,ielmatt,matname,enernt,
               xstatent,nstate_,istep,iinc,iperturb,ener,mint_,output, 
               lakonLen, nodeflabLen, matnameLen, outputLen));
#else
  FORTRAN(out,(cot,&nkt,kont,ipkont,lakont,&net,vt,stnt,inumt,nmethod,kode,
	       nodeflab,eent,t1t,fnt,time,epnt,ielmatt,matname,enernt,
               xstatent,nstate_,istep,iinc,iperturb,ener,mint_,output));
#endif
  
  if(strcmp1(&nodeflab[0],"U   ")==0) free(vt);
  if(strcmp1(&nodeflab[4],"NT  ")==0) free(t1t);
  if(strcmp1(&nodeflab[8],"S   ")==0) free(stnt);
  if(strcmp1(&nodeflab[12],"E   ")==0) free(eent);
  if(strcmp1(&nodeflab[16],"RF  ")==0) free(fnt);
  if(strcmp1(&nodeflab[20],"PE  ")==0) free(epnt);
  if(strcmp1(&nodeflab[24],"ENER")==0) free(enernt);
  if(strcmp1(&nodeflab[28],"SDV ")==0) free(xstatent);

  if(*kode==1){
    free(kont);free(ipkont);free(lakont);free(ielmatt);
  }
  free(inumt);free(cot);
  return;
}

