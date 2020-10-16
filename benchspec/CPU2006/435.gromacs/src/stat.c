/*
 * $Id: stat.c,v 1.24 2002/02/28 10:49:30 spoel Exp $
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
static char *SRCID_stat_c = "$Id: stat.c,v 1.24 2002/02/28 10:49:30 spoel Exp $";
#include <string.h>
#include <stdio.h>
#include "typedefs.h"
#include "sysstuff.h"
#include "gmxfio.h"
#include "fatal.h"
#include "network.h"
#include "txtdump.h"
#include "names.h"
#include "physics.h"
#include "vec.h"
#include "maths.h"
#include "mvdata.h"
#include "main.h"
#include "force.h"
#include "nrnb.h"
#include "vcm.h"
#include "smalloc.h"
#include "futil.h"
#include "network.h"
#include "rbin.h"
#include "tgroup.h"
#include "xtcio.h"
#include "trnio.h"
#include "statutil.h"

void global_stat(FILE *log,
		 t_commrec *cr,real ener[],
		 tensor fvir,tensor svir,
		 t_grpopts *opts,t_groups *grps,
		 t_nrnb *mynrnb,t_nrnb nrnb[],
		 t_vcm *vcm,real *terminate)
{
  static t_bin *rb=NULL; 
  static int   *itc;
  int    iterminate,imu,ie,ifv,isv,icm,imass,ica;
  int    icj=-1,ici=-1,icx=-1;
  int    in[MAXNODES];
  int    inn[egNR];
  int    j;
  
  if (rb==NULL) {
    rb=mk_bin();
    snew(itc,opts->ngtc);
  }
  else
    reset_bin(rb);
  
  /* Reset nrnb stuff */
  for(j=0; (j<cr->nnodes); j++)
    init_nrnb(&(nrnb[j]));
  cp_nrnb(&(nrnb[cr->nodeid]),mynrnb);
  
  /* This routine copies all the data to be summed to one big buffer
   * using the t_bin struct. 
   */
  where();
  ie  = add_binr(log,rb,F_NRE,ener);
  where();
  ifv = add_binr(log,rb,DIM*DIM,fvir[0]);
  where();
  isv = add_binr(log,rb,DIM*DIM,svir[0]);
  where();
  for(j=0; (j<cr->nnodes); j++)
    in[j] = add_bind(log,rb,eNRNB,nrnb[j].n);
  where();
  for(j=0; (j<opts->ngtc); j++) 
    itc[j]=add_binr(log,rb,DIM*DIM,grps->tcstat[j].ekin[0]);
  where();
  for(j=0; (j<egNR); j++)
    inn[j]=add_binr(log,rb,grps->estat.nn,grps->estat.ee[j]);
  where();
  icm   = add_binr(log,rb,DIM*vcm->nr,vcm->group_p[0]);
  where();
  imass = add_binr(log,rb,vcm->nr,vcm->group_mass);
  where();
  if (vcm->mode == ecmANGULAR) {
    icj   = add_binr(log,rb,DIM*vcm->nr,vcm->group_j[0]);
    where();
    icx   = add_binr(log,rb,DIM*vcm->nr,vcm->group_x[0]);
    where();
    ici   = add_binr(log,rb,DIM*DIM*vcm->nr,vcm->group_i[0][0]);
    where();
  }
  ica   = add_binr(log,rb,1,&(grps->cosacc.mvcos));
  where();
  iterminate = add_binr(log,rb,1,terminate);
  
  /* Global sum it all */
  sum_bin(rb,cr);
  where();
  
  /* Extract all the data locally */
  extract_binr(rb,ie  ,F_NRE,ener);
  extract_binr(rb,ifv ,DIM*DIM,fvir[0]);
  extract_binr(rb,isv ,DIM*DIM,svir[0]);
  for(j=0; (j<cr->nnodes); j++)
    extract_bind(rb,in[j],eNRNB,nrnb[j].n);
  for(j=0; (j<opts->ngtc); j++) 
    extract_binr(rb,itc[j],DIM*DIM,grps->tcstat[j].ekin[0]);
  for(j=0; (j<egNR); j++)
    extract_binr(rb,inn[j],grps->estat.nn,grps->estat.ee[j]);
  extract_binr(rb,icm,DIM*vcm->nr,vcm->group_p[0]);
  where();
  extract_binr(rb,imass,vcm->nr,vcm->group_mass);
  where();
  if (vcm->mode == ecmANGULAR) {
    extract_binr(rb,icj,DIM*vcm->nr,vcm->group_j[0]);
    where();
    extract_binr(rb,icx,DIM*vcm->nr,vcm->group_x[0]);
    where();
    extract_binr(rb,ici,DIM*DIM*vcm->nr,vcm->group_i[0][0]);
    where();
  }
  extract_binr(rb,ica,1,&(grps->cosacc.mvcos));
  where();
  extract_binr(rb,iterminate,1,terminate);
  where();

  /* Small hack for temp only */
  ener[F_TEMP]/=cr->nnodes;
}

int do_per_step(int step,int nstep)
{
  if (nstep != 0) 
    return ((step % nstep)==0); 
  else 
    return 0;
}

static void moveit(FILE *log,
		   int left,int right,char *s,rvec xx[],t_nsborder *nsb)
{
  rvec  *temp;
  int   i,m,bP,start,homenr;
    
  if (!xx) 
    return;

  start=nsb->index[nsb->nodeid];
  homenr=nsb->homenr[nsb->nodeid];
#ifdef DEBUG
  fprintf(log,"Moving %s for trajectory file, start=%d, homenr=%d\n",
	  s,start,homenr);
#endif
  snew(temp,homenr);
  for(i=0; (i<homenr); i++)
    copy_rvec(xx[start+i],temp[i]);

  move_rvecs(log,FALSE,FALSE,left,right,xx,NULL,nsb->nnodes-1,nsb,NULL);
  
  for(i=0; (i<homenr); i++) {
    bP=0;
    for(m=0; (m<DIM); m++)
      if (fabs(xx[start+i][m] - temp[i][m]) > GMX_REAL_MIN)
	bP=1;
    if (bP && log)
      fprintf(log,"%s[%5d] before: (%8.3f,%8.3f,%8.3f)"
	      " After: (%8.3f,%8.3f,%8.3f)\n",
	      s,start+i,temp[i][XX],temp[i][YY],temp[i][ZZ],
	      xx[start+i][XX],xx[start+i][YY],xx[start+i][ZZ]);
  }
  sfree(temp);
}

int write_traj(FILE *log,t_commrec *cr,
	       char *traj,t_nsborder *nsb,
	       int step,real t,real lambda,t_nrnb nrnb[],
	       int natoms,rvec *xx,rvec *vv,rvec *ff,matrix box)
{
  static int fp=-1;
  
  if ((fp == -1) && MASTER(cr)) {
#ifdef DEBUG
    fprintf(log,"Going to open trajectory file: %s\n",traj);
#endif
    fp = open_trn(traj,"w");
  }
  
#define MX(xvf) moveit(log,cr->left,cr->right,#xvf,xvf,nsb)
  if (cr->nnodes > 1) {
    MX(xx);
    MX(vv);
    MX(ff);
  }
  if ((xx || vv || ff) && MASTER(cr)) {
    fwrite_trn(fp,step,t,lambda,box,natoms,xx,vv,ff);
    gmx_fio_flush(fp);
  }
  return fp;
}

/* XDR stuff for compressed trajectories */
static int xd;

void write_xtc_traj(FILE *log,t_commrec *cr,
		    char *xtc_traj,t_nsborder *nsb,t_mdatoms *md,
		    int step,real t,rvec *xx,matrix box,real prec)
{
  static bool bFirst=TRUE;
  static rvec *x_sel;
  static int  natoms;
  int    i,j;
  
  if ((bFirst) && MASTER(cr)) {
#ifdef DEBUG
    fprintf(log,"Going to open compressed trajectory file: %s\n",xtc_traj);
#endif
    xd = open_xtc(xtc_traj,"w");
    
    /* Count the number of atoms in the selection */
    natoms=0;
    for(i=0; (i<md->nr); i++)
      if (md->cXTC[i] == 0)
	natoms++;
    if(log)
        fprintf(log,"There are %d atoms in your xtc output selection\n",natoms);
    if (natoms != md->nr)
      snew(x_sel,natoms);
    
    bFirst=FALSE;
  }
  
  if (cr->nnodes > 1) {
    MX(xx);
  }
  
  if ((xx) && MASTER(cr)) {
    if (natoms == md->nr)
      x_sel = xx;
    else {
      /* We need to copy everything into a temp array */
      for(i=j=0; (i<md->nr); i++) {
	if (md->cXTC[i] == 0) {
	  copy_rvec(xx[i],x_sel[j]);
	  j++;
	}
      }
    }
    if (write_xtc(xd,natoms,step,t,box,x_sel,prec) == 0)
      fatal_error(0,"XTC error");
  }
}

void close_xtc_traj(void)
{
  close_xtc(xd);
}



