/*
 * $Id: pme.c,v 1.40 2002/02/28 10:32:05 spoel Exp $
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
 * GROup of MAchos and Cynical Suckers
 */
static char *SRCID_pme_c = "$Id: pme.c,v 1.40 2002/02/28 10:32:05 spoel Exp $";
/* IMPORTANT FOR DEVELOPERS:
 *
 * Triclinic pme stuff isn't entirely trivial, and we've experienced
 * some bugs during development (many of them due to me). To avoid
 * this in the future, please check the following things if you make
 * changes in this file:
 *
 * 1. You should obtain identical (at least to the PME precision)
 *    energies, forces, and virial for
 *    a rectangular box and a triclinic one where the z (or y) axis is
 *    tilted a whole box side. For instance you could use these boxes:
 *
 *    rectangular       triclinic
 *     2  0  0           2  0  0
 *     0  2  0           0  2  0
 *     0  0  6           2  2  6
 *
 * 2. You should check the energy conservation in a triclinic box.
 *
 * It might seem an overkill, but better safe than sorry.
 * /Erik 001109
 */ 

#include <stdio.h>
#include <math.h>
#include "typedefs.h"
#include "txtdump.h"
#include "vec.h"
#include "gmxcomplex.h"
#include "smalloc.h"
#include "futil.h"
#include "shift_util.h"
#include "ewald_util.h"
#include "fftgrid.h"
#include "fatal.h"
#include "ewald.h"
#include "pme.h"
#include "network.h"
#include "physics.h"
#include "nrnb.h"
#include "copyrite.h"

#define DFT_TOL 1e-7

void my_range_check(char *s,int i,int nr,char *file,int line)
{
  int c;
  
  if ((i<0) || (i>=nr)) {
    fprintf(stdlog,"%s = %d should be in 0 .. %d [FILE %s, LINE %d]\n",
	    s,i,nr-1,file,line);
  }
}
#define range_check(i,nr) my_range_check(#i,i,nr,__FILE__,__LINE__)

void calc_recipbox(matrix box,matrix recipbox)
{
  /* Save some time by assuming upper right part is zero */

  real tmp=1.0/(box[XX][XX]*box[YY][YY]*box[ZZ][ZZ]);

  recipbox[XX][XX]=box[YY][YY]*box[ZZ][ZZ]*tmp;
  recipbox[XX][YY]=0;
  recipbox[XX][ZZ]=0;
  recipbox[YY][XX]=-box[YY][XX]*box[ZZ][ZZ]*tmp;
  recipbox[YY][YY]=box[XX][XX]*box[ZZ][ZZ]*tmp;
  recipbox[YY][ZZ]=0;
  recipbox[ZZ][XX]=(box[YY][XX]*box[ZZ][YY]-box[YY][YY]*box[ZZ][XX])*tmp;
  recipbox[ZZ][YY]=-box[ZZ][YY]*box[XX][XX]*tmp;
  recipbox[ZZ][ZZ]=box[XX][XX]*box[YY][YY]*tmp;
}


void calc_idx(int natoms,matrix recipbox,
	      rvec x[],rvec fractx[],ivec idx[],int nx,int ny,int nz,
	      int nnx[],int nny[],int nnz[])
{
  int  i;
  int  *idxptr,tix,tiy,tiz;
  real *xptr,tx,ty,tz;
  real rxx,ryx,ryy,rzx,rzy,rzz;
#if (defined __GNUC__ && (defined i386 || defined __386__) && !defined DOUBLE && defined USE_X86TRUNC)
  int x86_cw,x86_cwsave;

  asm("fnstcw %0" : "=m" (*&x86_cwsave));
  x86_cw = x86_cwsave | 3072;
  asm("fldcw %0" : : "m" (*&x86_cw));
  #define x86trunc(a,b) asm("fld %1\nfistpl %0\n" : "=m" (*&b) : "f" (a));
#endif
 
  rxx = recipbox[XX][XX];
  ryx = recipbox[YY][XX];
  ryy = recipbox[YY][YY];
  rzx = recipbox[ZZ][XX];
  rzy = recipbox[ZZ][YY];
  rzz = recipbox[ZZ][ZZ];

  for(i=0; (i<natoms); i++) {
    xptr   = x[i];
    idxptr = idx[i];
    
    /* Fractional coordinates along box vectors */
    tx = nx + nx * ( xptr[XX] * rxx + xptr[YY] * ryx + xptr[ZZ] * rzx );
    ty = ny + ny * (                  xptr[YY] * ryy + xptr[ZZ] * rzy );
    tz = nz + nz * (                                   xptr[ZZ] * rzz );
    
#if (defined __GNUC__ && (defined i386 || defined __386__) && !defined DOUBLE && defined USE_X86TRUNC)
    x86trunc(tx,tix);
    x86trunc(ty,tiy);
    x86trunc(tz,tiz);
#else
    tix = (int)(tx);
    tiy = (int)(ty);
    tiz = (int)(tz);
#endif

    fractx[i][XX] = tx - tix;
    fractx[i][YY] = ty - tiy;
    fractx[i][ZZ] = tz - tiz;   
    
    idxptr[XX] = nnx[tix];
    idxptr[YY] = nny[tiy];
    idxptr[ZZ] = nnz[tiz];
#ifdef DEBUG
    range_check(idxptr[XX],nx);
    range_check(idxptr[YY],ny);
    range_check(idxptr[ZZ],nz);
#endif
  }  
#if (defined __GNUC__ && (defined i386 || defined __386__) && !defined DOUBLE && defined USE_X86TRUNC)  
  asm("fldcw %0" : : "m" (*&x86_cwsave));
#endif

}

void sum_qgrid(t_commrec *cr,t_nsborder *nsb,t_fftgrid *grid,bool bForward)
{
  static bool bFirst=TRUE;
  static t_fft_r *tmp;
  int i;
  static int localsize;
  static int maxproc;

#if (defined USE_MPI && ! defined WITHOUT_FFTW)
  if(bFirst) {
    localsize=grid->la12r*grid->pfft.local_nx;
    if(!grid->workspace)
      snew(tmp,localsize);
    maxproc=grid->nx/grid->pfft.local_nx;
  }
  /* NOTE: FFTW doesnt necessarily use all processors for the fft;
     * above I assume that the ones that do have equal amounts of data.
     * this is bad since its not guaranteed by fftw, but works for now...
     * This will be fixed in the next release.
     */
  bFirst=FALSE;
  if(grid->workspace)
    tmp=grid->workspace;
  if(bForward) { /* sum contributions to local grid */
    for(i=0;i<maxproc;i++) {
      MPI_Reduce(grid->ptr+i*localsize, /*ptr arithm.     */
		 tmp,localsize,      
		 GMX_MPI_REAL,MPI_SUM,i,MPI_COMM_WORLD);
    }
    if(cr->nodeid<maxproc)
      memcpy(grid->ptr+cr->nodeid*localsize,tmp,localsize*sizeof(t_fft_r));
  }
  else { /* distribute local grid to all processors */
    for(i=0;i<maxproc;i++)
      MPI_Bcast(grid->ptr+i*localsize, /* ptr arithm     */
		localsize,       
		GMX_MPI_REAL,i,MPI_COMM_WORLD);
  }
#else
  fatal_error(0,"Parallel grid summation requires MPI and FFTW.\n");    
#endif
}

void spread_q_bsplines(t_fftgrid *grid,ivec idx[],real charge[],
		       splinevec theta,int nr,int order,
		       int nnx[],int nny[],int nnz[])
{
  /* spread charges from home atoms to local grid */
  t_fft_r *ptr;
  int      i,j,k,n,*i0,*j0,*k0,*ii0,*jj0,*kk0,ithx,ithy,ithz;
  int      nx,ny,nz,la2,la12,xidx,yidx,zidx;
  int      norder,norder1,*idxptr,ind0;
  real     valx,valxy,qn;
  real     *thx,*thy,*thz;
  
  clear_fftgrid(grid);
  unpack_fftgrid(grid,&nx,&ny,&nz,&la2,&la12,TRUE,&ptr);
  ii0   = nnx+nx+1-order;
  jj0   = nny+ny+1-order;
  kk0   = nnz+nz+1-order;
  thx   = theta[XX];
  thy   = theta[YY];
  thz   = theta[ZZ];
  
  for(n=0; (n<nr); n++) {
    qn     = charge[n];
    idxptr = idx[n];
    
    if (fabs(qn) > GMX_REAL_MIN) {
      xidx    = idxptr[XX];
      yidx    = idxptr[YY];
      zidx    = idxptr[ZZ];
#ifdef DEBUG
      range_check(xidx,nx);
      range_check(yidx,ny);
      range_check(zidx,nz);
#endif
      i0      = ii0+xidx; /* Pointer arithmetic */
      norder  = n*order;
      norder1 = norder+order;
      
      for(ithx=norder; (ithx<norder1); ithx++,i0++) {
	i    = *i0;
	j0   = jj0+yidx; /* Pointer arithmetic */
	valx = qn*thx[ithx];
	
	for(ithy=norder; (ithy<norder1); ithy++,j0++) {
	  j     = *j0;
	  k0    = kk0+zidx; /* Pointer arithmetic */
	  valxy = valx*thy[ithy];
	  ind0  = INDEX(i,j,0);
	  
	  for(ithz=norder; (ithz<norder1); ithz++,k0++) {
	    k = *k0;
#ifdef DEBUG
	    range_check(i,nx);
	    range_check(j,ny);
	    range_check(k,nz);
	    range_check(ind0+k,grid->nptr);
#endif
	    ptr[ind0+k] += valxy*thz[ithz];
	  }
	}
      }
    }
  }
}

real solve_pme(t_fftgrid *grid,real ewaldcoeff,real vol,
	       splinevec bsp_mod,matrix recipbox,
	       matrix vir,t_commrec *cr)
{
  /* do recip sum over local cells in grid */
  t_fft_c *ptr,*p0;
  int     nx,ny,nz,la2,la12;
  int     kx,ky,kz,idx,idx0,maxkx,maxky,maxkz,kystart=0,kyend=0;
  real    m2,mx,my,mz;
  real    factor=M_PI*M_PI/(ewaldcoeff*ewaldcoeff);
  real    ets2,struct2,vfactor,ets2vf;
  real    eterm,d1,d2,energy=0;
  real    denom;
  real    bx,by;
  real    mhx,mhy,mhz;
  real    virxx=0,virxy=0,virxz=0,viryy=0,viryz=0,virzz=0;
  bool    bPar = PAR(cr);
  real rxx,ryx,ryy,rzx,rzy,rzz;
  
  unpack_fftgrid(grid,&nx,&ny,&nz,&la2,&la12,FALSE,(t_fft_r **)&ptr);
   
  rxx = recipbox[XX][XX];
  ryx = recipbox[YY][XX];
  ryy = recipbox[YY][YY];
  rzx = recipbox[ZZ][XX];
  rzy = recipbox[ZZ][YY];
  rzz = recipbox[ZZ][ZZ];
 
  maxkx = (nx+1)/2;
  maxky = (ny+1)/2;
  maxkz = nz/2+1;
    
  if (bPar) { /* transpose X & Y and only sum local cells */
#if (defined USE_MPI && !defined WITHOUT_FFTW)
    kystart = grid->pfft.local_y_start_after_transpose;
    kyend   = kystart+grid->pfft.local_ny_after_transpose;
    if (debug)
      fprintf(debug,"solve_pme: kystart = %d, kyend=%d\n",kystart,kyend);
#else
    fatal_error(0,"Parallel PME attempted without MPI and FFTW");
#endif /* end of parallel case loop */
  }
  else {
    kystart = 0;
    kyend   = ny;
  }
  
  for(ky=kystart; (ky<kyend); ky++) {  /* our local cells */
    
    if(ky<maxky)
      my = ky;
    else
      my = (ky-ny);
    by = M_PI*vol*bsp_mod[YY][ky];
    
    for(kx=0; (kx<nx); kx++) {    
      if(kx < maxkx)
	mx = kx;
      else
	mx = (kx-nx);

      mhx = mx * rxx;
      mhy = mx * ryx + my * ryy;

      bx = bsp_mod[XX][kx];
      
      if (bPar)
	p0 = ptr + INDEX(ky,kx,0); /* Pointer Arithmetic */
      else
	p0 = ptr + INDEX(kx,ky,0); /* Pointer Arithmetic */

      for(kz=0; (kz<maxkz); kz++,p0++)  {
	if ((kx==0) && (ky==0) && (kz==0))
	  continue;
	d1      = p0->re;
	d2      = p0->im;
	mz      = kz;

	mhz = mx * rzx + my * rzy + mz * rzz;

	m2      = mhx*mhx+mhy*mhy+mhz*mhz;
	denom   = m2*bx*by*bsp_mod[ZZ][kz];
	eterm   = ONE_4PI_EPS0*exp(-factor*m2)/denom;
	p0->re  = d1*eterm;
	p0->im  = d2*eterm;
	
	struct2 = d1*d1+d2*d2;
	if ((kz > 0) && (kz < (nz+1)/2))
	  struct2*=2;
	ets2     = eterm*struct2;
	vfactor  = (factor*m2+1)*2.0/m2;
	energy  += ets2;
	
	ets2vf   = ets2*vfactor;
	virxx   += ets2vf*mhx*mhx-ets2;
	virxy   += ets2vf*mhx*mhy;   
	virxz   += ets2vf*mhx*mhz;  
	viryy   += ets2vf*mhy*mhy-ets2;
	viryz   += ets2vf*mhy*mhz;
	virzz   += ets2vf*mhz*mhz-ets2;
      }
    }
  }
    
  /* Update virial with local values. The virial is symmetric by definition.
   * this virial seems ok for isotropic scaling, but I'm
   * experiencing problems on semiisotropic membranes.
   * IS THAT COMMENT STILL VALID??? (DvdS, 2001/02/07).
   */
  vir[XX][XX] = 0.25*virxx;
  vir[YY][YY] = 0.25*viryy;
  vir[ZZ][ZZ] = 0.25*virzz;
  vir[XX][YY] = vir[YY][XX] = 0.25*virxy;
  vir[XX][ZZ] = vir[ZZ][XX] = 0.25*virxz;
  vir[YY][ZZ] = vir[ZZ][YY] = 0.25*viryz;
   
  /* This energy should be corrected for a charged system */
  return(0.5*energy);
}

void gather_f_bsplines(t_fftgrid *grid,matrix recipbox,
		       ivec idx[],rvec f[],real *charge,splinevec theta,
		       splinevec dtheta,int nr,int order,
		       int nnx[],int nny[],int nnz[])
{
  /* sum forces for local particles */  
  int     i,j,k,n,*i0,*j0,*k0,*ii0,*jj0,*kk0,ithx,ithy,ithz;
  int     nx,ny,nz,la2,la12;
  t_fft_r *ptr;
  int     xidx,yidx,zidx;
  real    tx,ty,dx,dy,qn;
  real    fx,fy,fz,gval,tgz,dgz;
  real    gval1,gval2,gval3,gval4;
  real    fxy1,fz1;
  real    *thx,*thy,*thz,*dthx,*dthy,*dthz;
  int     sn,norder,norder1,*idxptr,ind0;
  real    rxx,ryx,ryy,rzx,rzy,rzz;

  unpack_fftgrid(grid,&nx,&ny,&nz,&la2,&la12,TRUE,&ptr);
 
  thx  = theta[XX];
  thy  = theta[YY];
  thz  = theta[ZZ];
  dthx = dtheta[XX];
  dthy = dtheta[YY];
  dthz = dtheta[ZZ];
  ii0  = nnx+nx+1-order;
  jj0  = nny+ny+1-order;
  kk0  = nnz+nz+1-order;
  
  rxx = recipbox[XX][XX];
  ryx = recipbox[YY][XX];
  ryy = recipbox[YY][YY];
  rzx = recipbox[ZZ][XX];
  rzy = recipbox[ZZ][YY];
  rzz = recipbox[ZZ][ZZ];


  for(n=0; (n<nr); n++) {
    qn     = charge[n];
    fx      = 0.0;
    fy      = 0.0;
    fz      = 0.0;

    if (fabs(qn) > GMX_REAL_MIN) {
      idxptr = idx[n];
      xidx = idxptr[XX];
      yidx = idxptr[YY];
      zidx = idxptr[ZZ];
#ifdef DEBUG
      range_check(xidx,nx);
      range_check(yidx,ny);
      range_check(zidx,nz);
#endif
      
      i0      = ii0+xidx;   /* Pointer arithemtic */
      norder  = n*order;
      norder1 = norder+order;
      for(ithx=norder; (ithx<norder1); ithx++,i0++) {
        i     = *i0;
        tx    = thx[ithx];
        dx    = dthx[ithx];
        j0    = jj0+yidx;   /* Pointer arithemtic */

        if (order == 4) {
          for(ithy=norder; (ithy<norder1); ithy++,j0++) {
            j     = *j0;
            ty    = thy[ithy];
            dy    = dthy[ithy];
            k0    = kk0+zidx;     /* Pointer arithemtic */
            ind0  = INDEX(i,j,0);
            gval1 = ptr[ind0+k0[0]];
            gval2 = ptr[ind0+k0[1]];
            gval3 = ptr[ind0+k0[2]];
            gval4 = ptr[ind0+k0[3]];
            
            ithz  = norder;
            
            /* First iteration */
            fxy1  = thz[ithz]*gval1;
            fz1   = dthz[ithz]*gval1;
            ithz++;
            
            /* Second iteration */
            fxy1 += thz[ithz]*gval2;
            fz1  += dthz[ithz]*gval2;
            ithz++;
            
            /* Third iteration */
            fxy1 += thz[ithz]*gval3;
            fz1  += dthz[ithz]*gval3;
            ithz++;
            
            /* Fourth iteration */
            fxy1 += thz[ithz]*gval4;
            fz1  += dthz[ithz]*gval4;
            fx    = fx+dx*ty*fxy1;
            fy    = fy+tx*dy*fxy1;
            fz    = fz+tx*ty*fz1;    
          } 
        }
	   else {
        	  for(ithy=norder; (ithy<norder1); ithy++,j0++) {
            j     = *j0;
            ty    = thy[ithy];
            dy    = dthy[ithy];
            k0    = kk0+zidx; /* Pointer arithemtic */
            ind0  = INDEX(i,j,0);
            fxy1 = fz1 = 0;
            for(ithz=norder; (ithz<norder1); ithz++,k0++) {
              k     = *k0;
#ifdef DEBUG
	      range_check(i,nx);
	      range_check(j,ny);
	      range_check(k,nz);
	      range_check(ind0+k,grid->nptr);
#endif            
              gval  = ptr[ind0+k];
              fxy1 += thz[ithz]*gval;
              fz1  += dthz[ithz]*gval;
            }
            fx += dx*ty*fxy1;
            fy += tx*dy*fxy1;
            fz += tx*ty*fz1; 
          } 
        } 
      }
      f[n][XX] -= qn*( fx*nx*rxx );
      f[n][YY] -= qn*( fx*nx*ryx + fy*ny*ryy );
      f[n][ZZ] -= qn*( fx*nx*rzx + fy*ny*rzy + fz*nz*rzz );
    }
  }
  /* Since the energy and not forces are interpolated
   * the net force might not be exactly zero.
   * This can be solved by also interpolating F, but
   * that comes at a cost.
   * A better hack is to remove the net force every
   * step, but that must be done at a higher level
   * since this routine doesn't see all atoms if running
   * in parallel. Don't know how important it is?  EL 990726
   */
}


void make_bsplines(splinevec theta,splinevec dtheta,int order,int nx,int ny,
		   int nz,rvec fractx[],ivec idx[],real charge[],int nr)
{
  /* construct splines for local atoms */
  int  i,j,k,l;
  real dr,div,rcons;
  real *data,*ddata,*xptr;

  for(i=0; (i<nr); i++) {
    if (fabs(charge[i]) > GMX_REAL_MIN) {
      xptr = fractx[i];
      for(j=0; (j<DIM); j++) {
	dr  = xptr[j];
	
	/* dr is relative offset from lower cell limit */
	data=&(theta[j][i*order]);
	data[order-1]=0;
	data[1]=dr;
	data[0]=1-dr;
		
	for(k=3; (k<order); k++) {
	  div=1.0/(k-1.0);    
	  data[k-1]=div*dr*data[k-2];
	  for(l=1; (l<(k-1)); l++)
	    data[k-l-1]=div*((dr+l)*data[k-l-2]+(k-l-dr)*
			     data[k-l-1]);
	  data[0]=div*(1-dr)*data[0];
	}
	/* differentiate */
	ddata    = &(dtheta[j][i*order]);
	ddata[0] = -data[0];
	for(k=1; (k<order); k++)
	  ddata[k]=data[k-1]-data[k];
		
	div=1.0/(order-1);
	data[order-1]=div*dr*data[order-2];
	for(l=1; (l<(order-1)); l++)
	  data[order-l-1]=div*((dr+l)*data[order-l-2]+
			       (order-l-dr)*data[order-l-1]);
	data[0]=div*(1-dr)*data[0]; 
      }
    }
  }
}

    
void make_dft_mod(real *mod,real *data,int ndata)
{
  int i,j;
  real sc,ss,arg;
    
  for(i=0;i<ndata;i++) {
    sc=ss=0;
    for(j=0;j<ndata;j++) {
      arg=(2.0*M_PI*i*j)/ndata;
      sc+=data[j]*cos(arg);
      ss+=data[j]*sin(arg);
    }
    mod[i]=sc*sc+ss*ss;
  }
  for(i=0;i<ndata;i++)
    if(mod[i]<1e-7)
      mod[i]=(mod[i-1]+mod[i+1])*0.5;
}



void make_bspline_moduli(splinevec bsp_mod,int nx,int ny,int nz,int order)
{
  int nmax=max(nx,max(ny,nz));
  real *data,*ddata,*bsp_data;
  int i,k,l;
  real div;
    
  snew(data,order);
  snew(ddata,order);
  snew(bsp_data,nmax);

  data[order-1]=0;
  data[1]=0;
  data[0]=1;
	    
  for(k=3;k<order;k++) {
    div=1.0/(k-1.0);
    data[k-1]=0;
    for(l=1;l<(k-1);l++)
      data[k-l-1]=div*(l*data[k-l-2]+(k-l)*data[k-l-1]);
    data[0]=div*data[0];
  }
  /* differentiate */
  ddata[0]=-data[0];
  for(k=1;k<order;k++)
    ddata[k]=data[k-1]-data[k];
  div=1.0/(order-1);
  data[order-1]=0;
  for(l=1;l<(order-1);l++)
    data[order-l-1]=div*(l*data[order-l-2]+(order-l)*data[order-l-1]);
  data[0]=div*data[0]; 

  for(i=0;i<nmax;i++)
    bsp_data[i]=0;
  for(i=1;i<=order;i++)
    bsp_data[i]=data[i-1];
    
  make_dft_mod(bsp_mod[XX],bsp_data,nx);
  make_dft_mod(bsp_mod[YY],bsp_data,ny);
  make_dft_mod(bsp_mod[ZZ],bsp_data,nz);

  sfree(data);
  sfree(ddata);
  sfree(bsp_data);
}

/* Global variables! Yucky... */
static    t_fftgrid *grid=NULL;
/*static    int  nx,ny,nz;*/
static    int  *nnx,*nny,*nnz;
static    ivec *idx=NULL;
static    rvec *fractx; /* Fractional coordinate relative to the
			 * lower cell boundary 
			 */
static    matrix    recipbox;
static    splinevec theta;
static    splinevec dtheta;
static    splinevec bsp_mod;


void init_pme(FILE *log,t_commrec *cr,
	      int nkx,int nky,int nkz,int pme_order,int homenr,
	      bool bOptFFT,int ewald_geometry)
{
  int i;
  bool bPar;

#ifdef WITHOUT_FFTW
  fatal_error(0,"PME used, but GROMACS was compiled without FFTW support!\n");
#endif
  fprintf(log,"Will do PME sum in reciprocal space.\n");
  please_cite(log,"Essman95a");

  if(ewald_geometry==eewg3DC) {
    fprintf(log,"Using the Ewald3DC correction for systems with a slab geometry.\n");
    please_cite(log,"In-Chul99a");
  }

  bPar = cr && (cr->nnodes>1);
  if (bPar) {
    fprintf(log,"Parallelized PME sum used.\n");
    if ((nkx % cr->nnodes) != 0)
      fprintf(log,"Warning: For load balance, "
	      "fourier_nx should be divisible by NNODES\n");
  } 
 
  /* allocate space for things */
  snew(bsp_mod[XX],nkx);
  snew(bsp_mod[YY],nky);
  snew(bsp_mod[ZZ],nkz);
  for(i=0;i<DIM;i++) {
    snew(theta[i],pme_order*homenr); 
    snew(dtheta[i],pme_order*homenr);
  }
  snew(fractx,homenr); 

  snew(idx,homenr);
  snew(nnx,3*nkx);
  snew(nny,3*nky);
  snew(nnz,3*nkz);
  for(i=0; (i<3*nkx); i++)
    nnx[i] = i % nkx;
  for(i=0; (i<3*nky); i++)
    nny[i] = i % nky;
  for(i=0; (i<3*nkz); i++)
    nnz[i] = i % nkz;

  grid=mk_fftgrid(log,bPar,nkx,nky,nkz,bOptFFT);

  make_bspline_moduli(bsp_mod,nkx,nky,nkz,pme_order);   
}

t_fftgrid *spread_on_grid(FILE *logfile,   int homenr,
			  int pme_order,   rvec x[],
			  real charge[],   matrix box,
			  bool bGatherOnly)
{ 
  int nx,ny,nz,la2,la12;
  t_fft_r *ptr;
  
  /* Unpack structure */
  unpack_fftgrid(grid,&nx,&ny,&nz,&la2,&la12,TRUE,&ptr);
  
  /* Inverse box */
  calc_recipbox(box,recipbox); 

  if (!bGatherOnly) {
    /* Compute fftgrid index for all atoms, with help of some extra variables */
    calc_idx(homenr,recipbox,x,fractx,idx,nx,ny,nz,nnx,nny,nnz);
    
    /* make local bsplines  */
    make_bsplines(theta,dtheta,pme_order,nx,ny,nz,fractx,idx,charge,homenr);
    
    /* put local atoms on grid. */
    spread_q_bsplines(grid,idx,charge,theta,homenr,pme_order,nnx,nny,nnz);
  }
  return grid;
}

real do_pme(FILE *logfile,   bool bVerbose,
	    t_inputrec *ir,  rvec x[],
	    rvec f[],        real charge[],
	    matrix box,	     t_commrec *cr,
	    t_nsborder *nsb, t_nrnb *nrnb,    
	    matrix vir,      real ewaldcoeff,
	    bool bGatherOnly)
{ 
  static  real energy = 0;
  int     i,ntot,npme;
  int     nx,ny,nz,la12,la2;
  t_fft_r *ptr;
  real    vol;
  static  int *orderlist = NULL;

  /* Unpack structure */
  unpack_fftgrid(grid,&nx,&ny,&nz,&la2,&la12,TRUE,&ptr);
  
  /* Spread the charges on a grid */
  (void) spread_on_grid(logfile,HOMENR(nsb),ir->pme_order,
			x+START(nsb),charge+START(nsb),box,bGatherOnly);

  if (!bGatherOnly) {
    inc_nrnb(nrnb,eNR_SPREADQBSP,
	     ir->pme_order*ir->pme_order*ir->pme_order*HOMENR(nsb));
    
    
    /* sum contributions to local grid from other nodes */
    if (PAR(cr))
      sum_qgrid(cr,nsb,grid,TRUE);
 
       /* do 3d-fft */ 
    gmxfft3D(grid,FFTW_FORWARD,cr);
   
    /* solve in k-space for our local cells */
    vol = det(box);
    energy=solve_pme(grid,ewaldcoeff,vol,bsp_mod,recipbox,vir,cr);
     inc_nrnb(nrnb,eNR_SOLVEPME,nx*ny*nz*0.5);
    /* do 3d-invfft */
    gmxfft3D(grid,FFTW_BACKWARD,cr);
    
    /* distribute local grid to all nodes */
    if (PAR(cr))
      sum_qgrid(cr,nsb,grid,FALSE);
      
    ntot  = grid->nxyz;  
    npme  = ntot*log((real)ntot)/(cr->nnodes*log(2.0));
    inc_nrnb(nrnb,eNR_FFT,2*npme);
  }
  /* interpolate forces for our local atoms */
  gather_f_bsplines(grid,recipbox,idx,f+START(nsb),charge+START(nsb),
		    theta,dtheta,HOMENR(nsb),ir->pme_order,
		    nnx,nny,nnz);

  inc_nrnb(nrnb,eNR_GATHERFBSP,
	   ir->pme_order*ir->pme_order*ir->pme_order*HOMENR(nsb));

  return energy;  
}




