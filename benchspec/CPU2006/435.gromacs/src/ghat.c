/*
 * $Id: ghat.c,v 1.5 2002/02/28 10:32:04 spoel Exp $
 * 
 *                This source code is part of
 * 
 *                 G   R   O   M   A   C   S
 * 
 *          GROningen MAchine for Chemical Simulations
 * 
 *                        VERSION 3.1
 * Copyright (c) 1991-2001, University of Groningen, The Netherlands
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * If you want to redistribute modifications, please consider that
 * scientific software is very special. Version control is crucial -
 * bugs must be traceable. We will be happy to consider code for
 * inclusion in the official distribution, but derived work must not
 * be called official GROMACS. Details are found in the README & COPYING
 * files - if they are missing, get the official version at www.gromacs.org.
 * 
 * To help us fund GROMACS development, we humbly ask that you cite
 * the papers on the package - you can find them in the top README file.
 * 
 * For more info, check our website at http://www.gromacs.org
 * 
 * And Hey:
 * Getting the Right Output Means no Artefacts in Calculating Stuff
 */
static char *SRCID_ghat_c = "$Id: ghat.c,v 1.5 2002/02/28 10:32:04 spoel Exp $";
#include <stdio.h>
#include "typedefs.h"
#include "futil.h"
#include "vec.h"
#include "physics.h"
#include "shift_util.h"
#include "fftgrid.h"
#include "xvgr.h"

void symmetrize_ghat(int nx,int ny,int nz,real ***ghat)
{
  int  i,j,k;
  int  iip,jjp,kkp;
  real ggg;

  /* fprintf(stderr,"Symmetrizing Ghat function\n");   */
  /* Only the lower octant of the rectangle has been saved,
   * so we must construct the other 7 octants by symmetry operations.
   */
  for(i=0; (i<=nx/2); i++) {
    iip = (nx-i) % nx;
    for(j=0; (j<=ny/2); j++) {
      jjp = (ny-j) % ny;
      for(k=0; (k<=nz/2); k++) {
	kkp = (nz-k) % nz;
	ggg                 = ghat[i][j][k];
	ghat[i]  [jjp][k]   = ggg;
	ghat[i]  [j]  [kkp] = ggg;
	ghat[i]  [jjp][kkp] = ggg;
	ghat[iip][j]  [k]   = ggg;
	ghat[iip][jjp][k]   = ggg;
	ghat[iip][j]  [kkp] = ggg;
	ghat[iip][jjp][kkp] = ggg;
      }
    }
  }
}

void mk_ghat(FILE *fp,int nx,int ny,int nz,real ***ghat,
	     rvec box,real r1,real rc,bool bSym,bool bOld)
{
  int  ix,iy,iz;
  int  ixmax,iymax,izmax;
  real k2,ggg;
  rvec k,lll;
  
  calc_lll(box,lll);
    
  if (bSym) {
    ixmax=nx/2+1;
    iymax=ny/2+1;
    izmax=nz/2+1;
  }
  else {
    ixmax=nx;
    iymax=ny;
    izmax=nz;
  }
  /* Loop over lattice vectors in fourier space */    
  for(ix=0; (ix < ixmax); ix++) {
    for(iy=0; (iy < iymax); iy++) {
      for(iz=0; (iz < izmax); iz++) {
	calc_k(lll,ix,iy,iz,nx,ny,nz,k);
	k2 = iprod(k,k);
	if ((ix == 0) && (iy == 0) && (iz == 0))
	  ggg = 0.0;
	else {
	  if (bOld)
	    ggg = gk(sqrt(k2),rc,r1)/(k2*EPSILON0);
	  else
	    ggg = gknew(sqrt(k2),rc,r1)/(k2*EPSILON0);
	}
	ghat[ix][iy][iz]=ggg;
      }
    }
  }
  if (bSym)
    symmetrize_ghat(nx,ny,nz,ghat);
}

void wr_ghat(char *fn,int n1max,int n2max,int n3max,real h1,real h2,real h3,
	     real ***ghat,int nalias,int porder,int niter,bool bSym,rvec beta,
	     real r1,real rc,real pval,real zval,real eref,real qopt)
{
  FILE *fp;
  int  N1MAX,N2MAX,N3MAX;
  bool bNL=FALSE;
  real rx,ry,rz;
  int  ii,jj,kk,nn;
  
  fp = ffopen(fn,"w");
  fprintf(fp,"%8d  %8d  %8d  %15.10e  %15.10e %15.10e\n",
	  n1max,n2max,n3max,h1,h2,h3);
  fprintf(fp,"%8d  %8d  %8d  %8d  %15.10e  %15.10e  %15.10e\n",
	  nalias,porder,niter,bSym,beta[XX],beta[YY],beta[ZZ]);
  fprintf(fp,"%10g  %10g  %10g  %10g  %10g  %10g\n",
	  rc,r1,pval,zval,eref,qopt);
  
  if (bSym) {
    N1MAX = n1max/2+1;
    N2MAX = n2max/2+1;
    N3MAX = n3max/2+1;
  }
  else {
    N1MAX = n1max;
    N2MAX = n2max;
    N3MAX = n3max;
  }
  for(ii=0; (ii<N1MAX); ii++) {
    for(jj=0; (jj<N2MAX); jj++) {
      for(kk=0,nn=1; (kk<N3MAX); kk++,nn++) { 
	bNL=FALSE;
	fprintf(fp,"  %12.5e",ghat[ii][jj][kk]);
	if ((nn % 6) == 0) {
	  fprintf(fp,"\n");
	  bNL=TRUE;
	}
      }
      if (!bNL)
	fprintf(fp,"\n");
    }
  }
  fclose(fp);
  
  fp=xvgropen("ghat.xvg","G-Hat","k","gk");
  for(ii=0; (ii<N1MAX); ii++) {
    rx=sqr((real)(ii*h1));
    for(jj=0; (jj<N2MAX); jj++) {
      ry=rx+sqr((real)(jj*h2));
      for(kk=0; (kk<N3MAX); kk++) {
	rz=ry+sqr((real)(kk*h3));
	fprintf(fp,"%10g  %10g\n",sqrt(rz),ghat[ii][jj][kk]);
      }
    }
  }
  fclose(fp);
}

void pr_scalar_gk(char *fn,int nx,int ny,int nz,rvec box,real ***ghat)
{
  FILE *fp;
  int  ii,jj,kk;
  real k1;
  rvec k,lll;
  
  calc_lll(box,lll);
  
  fp=xvgropen(fn,"G-Hat","k","gk");
  for(ii=0; (ii<nx); ii++) {
    for(jj=0; (jj<ny); jj++) {
      for(kk=0; (kk<nz); kk++) {
	calc_k(lll,ii,jj,kk,nx,ny,nz,k);
	k1 = norm(k);
	fprintf(fp,"%10g  %10g\n",k1,ghat[ii][jj][kk]);
      }
    }
  }
  fclose(fp);
}

real ***rd_ghat(FILE *log,char *fn,ivec igrid,rvec gridspace,
		rvec beta,int *porder,real *r1,real *rc)
{
  FILE   *in;
  real   ***gh;
  double gx,gy,gz,alX,alY,alZ,ddd;
  double acut,pval,zval,eref,qopt,r11;
  int    nalias,niter,bSym;
  int    ix,iy,iz,ixmax,iymax,izmax;
  
  in=ffopen(fn,"r");
  fscanf(in,"%d%d%d%lf%lf%lf",&ix,&iy,&iz,&gx,&gy,&gz);
  igrid[XX]=ix, igrid[YY]=iy, igrid[ZZ]=iz;
  gridspace[XX]=gx,  gridspace[YY]=gy,  gridspace[ZZ]=gz;
  fscanf(in,"%d%d%d%d%lf%lf%lf",&nalias,porder,&niter,&bSym,&alX,&alY,&alZ);
  fscanf(in,"%lf%lf%lf%lf%lf%lf",&acut,&r11,&pval,&zval,&eref,&qopt);
  *r1 = r11;
  *rc = acut;
  
  fprintf(log,"\nOpened %s for reading ghat function\n",fn);
  fprintf(log,"gridsize: %10d %10d %10d\n",ix,iy,iz);
  fprintf(log,"spacing:  %10g %10g %10g\n",gx,gy,gz);
  fprintf(log,"    nalias    porder     niter      bSym      beta[X-Z]\n"
	  "%10d%10d%10d%10d%10g%10g%10g\n",
	  nalias,*porder,niter,bSym,alX,alY,alZ);
  fprintf(log,"      acut        r1      pval      zval      eref      qopt\n"
	  "%10g%10g%10g%10g%10g%10g\n",acut,*r1,pval,zval,eref,qopt);
  fflush(log);
  
  beta[XX] = alX;
  beta[YY] = alY;
  beta[ZZ] = alZ;
  
  gh      = mk_rgrid(ix,iy,iz);
  if (bSym) {
    ixmax=igrid[XX]/2+1;
    iymax=igrid[YY]/2+1;
    izmax=igrid[ZZ]/2+1;
  }
  else {
    ixmax=igrid[XX];
    iymax=igrid[YY];
    izmax=igrid[ZZ];
  }
  fprintf(log,"Reading ghat of %d %d %d\n",ixmax,iymax,izmax);
  for(ix=0; (ix<ixmax); ix++)
    for(iy=0; (iy<iymax); iy++)
      for(iz=0; (iz<izmax); iz++) {
	fscanf(in,"%lf",&ddd);
	gh[ix][iy][iz] = ddd;
      }
  ffclose(in);

  wr_ghat("output.hat",igrid[XX],igrid[YY],igrid[ZZ],gx,gy,gz,gh,
	  nalias,*porder,niter,bSym,beta,
	  *r1,*rc,pval,zval,eref,qopt);
    
  if (bSym) 
    symmetrize_ghat(igrid[XX],igrid[YY],igrid[ZZ],gh);
  
  fprintf(log,"\nSuccessfully read ghat function!\n");
  
  
  return gh;
}

