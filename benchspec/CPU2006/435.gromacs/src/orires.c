/*
 * $Id: orires.c,v 1.6 2002/02/28 10:49:29 spoel Exp $
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
 * Gnomes, ROck Monsters And Chili Sauce
 */
static char *SRCID_orires_c = "$Id: orires.c,v 1.6 2002/02/28 10:49:29 spoel Exp $";
#include "typedefs.h"
#include "smalloc.h"
#include "vec.h"
#include "nrjac.h"
#include "network.h"
#include "orires.h"
#include "do_fit.h"
#include "main.h"

void init_orires(FILE *log,int nfa,t_iatom forceatoms[],t_iparams ip[],
		 rvec *xref,t_mdatoms *md,t_inputrec *ir,
		 t_commrec *mcr,t_fcdata *fcd)
{
  int  i,j,d,ex,nr,*nr_ex;
  real mtot;
  rvec com;
  t_oriresdata *od;
  
  od = &(fcd->orires);
  od->fc  = ir->orires_fc;
  od->nex = 0;
  od->S   = NULL;

  if (ir->orires_tau > 0)
    od->edt = exp(-ir->delta_t/ir->orires_tau);
  else
    od->edt = 0;
  od->edt1 = 1 - od->edt;
  od->exp_min_t_tau = 1.0;
  od->nr = nfa/3;
  
  if (od->nr == 0)
    return;

  nr_ex = NULL;

  for(i=0; i<nfa; i+=3) {
    ex = ip[forceatoms[i]].orires.ex;
    if (ex >= od->nex) {
      srenew(nr_ex,ex+1);
      for(j=od->nex; j<ex+1; j++)
	nr_ex[j] = 0;
      od->nex = ex+1;
    }
    nr_ex[ex]++;
  }
  snew(od->S,od->nex);
  /* When not doing time averaging, the instaneous and time averaged data
   * are indentical and the pointers can point to the same memory.
   */
  snew(od->Dinsl,od->nr);
  if (mcr)
    snew(od->Dins,od->nr);
  else
    od->Dins = od->Dinsl;
  if (fabs(ir->orires_tau) < GMX_REAL_MIN)
    od->Dtav = od->Dins;
  else
    snew(od->Dtav,od->nr);
  snew(od->oinsl,od->nr);
  if (mcr)
    snew(od->oins,od->nr);
  else
    od->oins = od->oinsl;
  if ( fabs(ir->orires_tau) < GMX_REAL_MIN)
    od->otav = od->oins;
  else
    snew(od->otav,od->nr);
  snew(od->tmp,od->nex);
  snew(od->TMP,od->nex);
  for(ex=0; ex<od->nex; ex++) {
    snew(od->TMP[ex],5);
    for(i=0; i<5; i++)
      snew(od->TMP[ex][i],5);
  }

  od->nref = 0;
  for(i=0; i<md->nr; i++)
    if (md->cORF[i] == 0)
      od->nref++;
  snew(od->mref,od->nref);
  snew(od->xref,od->nref);
  snew(od->xtmp,od->nref);

  /* Determine the reference structure on the master node.
   * Copy it to the other nodes after checking multi compatibility,
   * so we are sure the subsystems match before copying.
   */
  clear_rvec(com);
  mtot = 0.0;
  j = 0;
  for(i=0; i<md->nr; i++) {
    if (md->cORF[i] == 0) {
      od->mref[j] = md->massT[i];
      if (mcr==NULL || MASTER(mcr)) {
	copy_rvec(xref[i],od->xref[j]);
	for(d=0; d<DIM; d++)
	  com[d] += od->mref[j]*xref[i][d];
      }
      mtot += od->mref[j];
      j++;
    }
  }
  od->invmref = 1.0/mtot;
  svmul(od->invmref,com,com);
  if (mcr==NULL || MASTER(mcr))
    for(j=0; j<od->nref; j++)
      rvec_dec(od->xref[j],com);
  
  fprintf(log,"Found %d orientation experiments\n",od->nex);
  for(i=0; i<od->nex; i++)
    fprintf(log,"  experiment %d has %d restraints\n",i+1,nr_ex[i]);

  sfree(nr_ex);

  fprintf(log,"  the fit group consists of %d atoms and has total mass %g\n",
	  od->nref,mtot);
  
  if (mcr) {
    fprintf(log,"  the orientation restraints are ensemble averaged over %d systems\n",mcr->nnodes);

    check_multi_int(log,mcr,fcd->orires.nr,
		    "the number of orientation restraints");
    check_multi_int(log,mcr,fcd->orires.nref,
		    "the number of fit atoms for orientation restraining");
    /* Copy the reference coordinates from the master to the other nodes */
    gmx_sum(DIM*fcd->orires.nref,fcd->orires.xref[0],mcr);
  }
}

void print_orires_log(FILE *log,t_fcdata *fcd)
{
  int           ex,i,j,nrot;
  bool          bZero;
  matrix        S,TMP;
  t_oriresdata  *od;
  static double **M=NULL,*eig,**v;
  
  od = &(fcd->orires);

  if (M == NULL) {
    snew(M,DIM);
    for(i=0; i<DIM; i++)
      snew(M[i],DIM);
    snew(eig,DIM);
    snew(v,DIM);
    for(i=0; i<DIM; i++)
      snew(v[i],DIM);
  }
  
  for(ex=0; ex<od->nex; ex++) {
    /* Rotate the S tensor back to the reference frame */
    mmul(od->R,od->S[ex],TMP);
    mtmul(TMP,od->R,S);
    for(i=0; i<DIM; i++)
      for(j=0; j<DIM; j++)
	M[i][j] = S[i][j];
    
    jacobi(M,DIM,eig,v,&nrot);

    j=0;
    for(i=1; i<DIM; i++)
      if (sqr(eig[i]) > sqr(eig[j]))
	j=i;
    
    fprintf(log,"  Orientation experiment %d:\n",ex+1);
    fprintf(log,"    order parameter: %g\n",eig[j]);
    for(i=0; i<DIM; i++)
      fprintf(log,"    eig: %6.3f   %6.3f %6.3f %6.3f\n",
	      (fabs(eig[j])>GMX_REAL_MIN) ? eig[i]/eig[j] : 0,v[XX][i],v[YY][i],v[ZZ][i]);
    fprintf(log,"\n");
  }
}

real calc_orires_dev(t_commrec *mcr,
		     int nfa,t_iatom forceatoms[],t_iparams ip[],
		     t_mdatoms *md,rvec x[],t_fcdata *fcd)
{
  int          fa,d,i,j,type,ex,nref;
  real         edt,edt1,invn,pfac,r2,invr,corrfac,weight,wsv2,sw,dev;
  tensor       *S,R,TMP;
  rvec5        *Dinsl,*Dins,*Dtav,*rhs;
  real         *mref,***T;
  rvec         *xref,*xtmp,com,r_unrot,r;
  t_oriresdata *od;
  bool         bTAV;
  static real  two_thr=2.0/3.0;

  od = &(fcd->orires);

  bTAV = (fabs(od->edt)>GMX_REAL_MIN);
  edt  = od->edt;
  edt1 = od->edt1;
  S    = od->S;
  Dinsl= od->Dinsl;
  Dins = od->Dins;
  Dtav = od->Dtav;
  T    = od->TMP;
  rhs  = od->tmp;
  nref = od->nref;
  mref = od->mref;
  xref = od->xref;
  xtmp = od->xtmp;
  
  od->exp_min_t_tau *= edt;

  if (mcr)
    invn = 1.0/mcr->nnodes;
  else
    invn = 1.0;

  j=0;
  for(i=0; i<md->nr; i++)
    if (md->cORF[i] == 0) {
      copy_rvec(x[i],xtmp[j]);
      for(d=0; d<DIM; d++)
	com[d] += mref[j]*xref[j][d];
      j++;
    }
  svmul(od->invmref,com,com);
  for(j=0; j<nref; j++)
    rvec_dec(xtmp[j],com);
  /* Calculate the rotation matrix to rotate x to the reference orientation */
  calc_fit_R(nref,mref,xref,xtmp,R);
  copy_mat(R,od->R);

  d = 0;
  for(fa=0; fa<nfa; fa+=3) {
    type = forceatoms[fa];
    rvec_sub(x[forceatoms[fa+1]],x[forceatoms[fa+2]],r_unrot);
    mvmul(R,r_unrot,r);
    r2   = norm2(r);
    invr = invsqrt(r2);
    /* Calculate the prefactor for the D tensor, this includes the factor 3! */
    pfac = ip[type].orires.c*invr*invr*3;
    for(i=0; i<ip[type].orires.pow; i++)
      pfac *= invr;
    Dinsl[d][0] = pfac*(2*r[0]*r[0] + r[1]*r[1] - r2);
    Dinsl[d][1] = pfac*(2*r[0]*r[1]);
    Dinsl[d][2] = pfac*(2*r[0]*r[2]);
    Dinsl[d][3] = pfac*(2*r[1]*r[1] + r[0]*r[0] - r2);
    Dinsl[d][4] = pfac*(2*r[1]*r[2]);

    if (mcr)
      for(i=0; i<5; i++)
	Dins[d][i] = Dinsl[d][i]*invn;
    
    d++;
  }
  
  if (mcr)
    gmx_sum(5*od->nr,Dins[0],mcr);
  
  /* Correction factor to correct for the lack of history for short times */
  corrfac = 1.0/(1.0-od->exp_min_t_tau);
  
  /* Calculate the order tensor S for each experiment via optimization */
  for(ex=0; ex<od->nex; ex++)
    for(i=0; i<5; i++) {
      rhs[ex][i] = 0;
      for(j=0; j<=i; j++)
	T[ex][i][j] = 0;
    }
  d = 0;
  for(fa=0; fa<nfa; fa+=3) {
    if (bTAV)
      for(i=0; i<5; i++)
	Dtav[d][i] = edt*Dtav[d][i] + edt1*Dins[d][i];

    type   = forceatoms[fa];
    ex     = ip[type].orires.ex;
    weight = ip[type].orires.kfac;
    /* Calculate the vector rhs and half the matrix T for the 5 equations */
    for(i=0; i<5; i++) {
      rhs[ex][i] += Dtav[d][i]*ip[type].orires.obs*weight;
      for(j=0; j<=i; j++)
	T[ex][i][j] += Dtav[d][i]*Dtav[d][j]*weight;
    }
    d++;
  }
  /* Now we have all the data we can calculate S */
  for(ex=0; ex<od->nex; ex++) {
    /* Correct corrfac and copy one half of T to the other half */
    for(i=0; i<5; i++) {
      rhs[ex][i]  *= corrfac;
      T[ex][i][i] *= sqr(corrfac);
      for(j=0; j<i; j++) {
	T[ex][i][j] *= sqr(corrfac);
	T[ex][j][i]  = T[ex][i][j];
      }
    }
    m_inv_gen(T[ex],5,T[ex]);
    /* Calculate the orientation tensor S for this experiment */
    S[ex][0][0] = 0;
    S[ex][0][1] = 0;
    S[ex][0][2] = 0;
    S[ex][1][1] = 0;
    S[ex][1][2] = 0;
    for(i=0; i<5; i++) {
      S[ex][0][0] += 1.5*T[ex][0][i]*rhs[ex][i];
      S[ex][0][1] += 1.5*T[ex][1][i]*rhs[ex][i];
      S[ex][0][2] += 1.5*T[ex][2][i]*rhs[ex][i];
      S[ex][1][1] += 1.5*T[ex][3][i]*rhs[ex][i];
      S[ex][1][2] += 1.5*T[ex][4][i]*rhs[ex][i];
    }
    S[ex][1][0] = S[ex][0][1];
    S[ex][2][0] = S[ex][0][2];
    S[ex][2][1] = S[ex][1][2];
    S[ex][2][2] = -S[ex][0][0] - S[ex][1][1];
  }
  
  wsv2 = 0;
  sw   = 0;
  
  d = 0;
  for(fa=0; fa<nfa; fa+=3) {
    type = forceatoms[fa];
    ex = ip[type].orires.ex;

    od->otav[d] = two_thr*
      corrfac*(S[ex][0][0]*Dtav[d][0] + S[ex][0][1]*Dtav[d][1] +
	       S[ex][0][2]*Dtav[d][2] + S[ex][1][1]*Dtav[d][3] +
	       S[ex][1][2]*Dtav[d][4]);
    if (bTAV)
      od->oins[d] = two_thr*(S[ex][0][0]*Dins[d][0] + S[ex][0][1]*Dins[d][1] +
			     S[ex][0][2]*Dins[d][2] + S[ex][1][1]*Dins[d][3] +
			     S[ex][1][2]*Dins[d][4]);
    if (mcr)
      /* When ensemble averaging is used recalculate the local orientation
       * for output to the energy file.
       */
      od->oinsl[d] = two_thr*
	(S[ex][0][0]*Dinsl[d][0] + S[ex][0][1]*Dinsl[d][1] +
	 S[ex][0][2]*Dinsl[d][2] + S[ex][1][1]*Dinsl[d][3] +
	 S[ex][1][2]*Dinsl[d][4]);
    
    dev = od->otav[d] - ip[type].orires.obs;
    
    wsv2 += ip[type].orires.kfac*sqr(dev);
    sw   += ip[type].orires.kfac;
    
    d++;
  }
  od->rmsdev = sqrt(wsv2/sw);
  
  /* Rotate the S matrices back, so we get the correct grad(tr(S D)) */
  for(ex=0; ex<od->nex; ex++) {
    tmmul(R,S[ex],TMP);
    mmul(TMP,R,S[ex]);
  }

  return od->rmsdev;
  
  /* Approx. 120*nfa/3 flops */
}

real orires(int nfa,t_iatom forceatoms[],t_iparams ip[],
	    rvec x[],rvec f[],t_forcerec *fr,t_graph *g,
	    matrix box,real lambda,real *dvdlambda,
	    t_mdatoms *md,int ngrp,real egnb[],real egcoul[],
	    t_fcdata *fcd)
{
  atom_id      ai,aj;
  int          fa,d,i,type,ex,power,ki;
  ivec         dt;
  real         r2,invr,invr2,fc,smooth_fc,dev,devins,pfac;
  rvec         r,Sr,fij;
  real         vtot;
  t_oriresdata *od;
  bool         bTAV;

  vtot = 0;
  od = &(fcd->orires);

  if (fabs(od->fc) > GMX_REAL_MIN) {
    bTAV = (fabs(od->edt) > GMX_REAL_MIN);
    
    /* Smoothly switch on the restraining when time averaging is used */
    smooth_fc = od->fc*(1.0 - od->exp_min_t_tau);
    
    d = 0;
    for(fa=0; fa<nfa; fa+=3) {
      type  = forceatoms[fa];
      ai    = forceatoms[fa+1];
      aj    = forceatoms[fa+2];
      rvec_sub(x[ai],x[aj],r);
      r2    = norm2(r);
      invr  = invsqrt(r2);
      invr2 = invr*invr;
      ex    = ip[type].orires.ex;
      power = ip[type].orires.pow;
      fc    = smooth_fc*ip[type].orires.kfac;
      dev   = od->otav[d] - ip[type].orires.obs;
      
      /* NOTE: there is no real potential when time averaging is applied */
      vtot += 0.5*fc*sqr(dev);
      
      if (bTAV) {
	/* Calculate the force as the sqrt of tav times instantaneous */
	devins = od->oins[d] - ip[type].orires.obs;
	if (dev*devins <= 0)
	  dev = 0;
	else {
	  dev = sqrt(dev*devins);
	  if (devins < 0)
	    dev = -dev;
	}
      }
      
      pfac  = fc*ip[type].orires.c*invr2;
      for(i=0; i<power; i++)
	pfac *= invr;
      mvmul(od->S[ex],r,Sr);
      for(i=0; i<DIM; i++)
	fij[i] = -pfac*dev*(4*Sr[i] - 2*(2+power)*invr2*iprod(Sr,r)*r[i]);
      
      ivec_sub(SHIFT_IVEC(g,ai),SHIFT_IVEC(g,aj),dt);
      ki=IVEC2IS(dt);
      
      for(i=0; i<DIM; i++) {
	f[ai][i]               += fij[i];
	f[aj][i]               -= fij[i];
	fr->fshift[ki][i]      += fij[i];
	fr->fshift[CENTRAL][i] -= fij[i];
      }
      d++;
    }
  }
  
  return vtot;
  
  /* Approx. 80*nfa/3 flops */
}
