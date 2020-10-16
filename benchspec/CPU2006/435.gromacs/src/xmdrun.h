/*
 * $Id: xmdrun.h,v 1.5 2002/02/28 10:54:46 spoel Exp $
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
 * Gromacs Runs One Microsecond At Cannonball Speeds
 */

#ifndef _xmdrun_h
#define _xmdrun_h

static char *SRCID_xmdrun_h = "$Id: xmdrun.h,v 1.5 2002/02/28 10:54:46 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include "typedefs.h"
#include "network.h"
#include "tgroup.h"
#include "filenm.h"
#include "nsb.h"
#include "mshift.h"
#include "force.h"
#include "time.h"
#include "edsam.h"
#include "mdebin.h"
#include "vcm.h"
#include "dummies.h"

/* This	file contains XMDRUN datatypes and function prototypes, grouped
 * neatly according to parts of the functionalisty
 */
 
/* SHELL MD STUFF */
typedef struct {
  int     nnucl;
  atom_id shell;	        /* The shell id				*/
  atom_id nucl1,nucl2,nucl3;	/* The nuclei connected to the shell	*/
  real    k;		        /* force constant		        */
  real    k_1;		        /* 1 over force constant		*/
} t_shell;

/* Initialization function */
extern t_shell *init_shells(FILE *log,int start,int homenr,
			    t_idef *idef,t_mdatoms *md,int *nshell);

/* Optimize shell positions */
extern int relax_shells(FILE *log,t_commrec *cr,t_commrec *mcr,bool bVerbose,
			int mdstep,t_parm *parm,bool bDoNS,bool bStopCM,
			t_topology *top,real ener[],t_fcdata *fcd,
			rvec x[],rvec vold[],rvec v[],rvec vt[],rvec f[],
			rvec buf[],t_mdatoms *md,t_nsborder *nsb,t_nrnb *nrnb,
			t_graph *graph,t_groups *grps,tensor vir_part,
			tensor pme_vir_part,bool bShell,
			int nshell,t_shell shells[],t_forcerec *fr,
			char *traj,real t,real lambda,rvec mu_tot,
			int natoms,matrix box,bool *bConverged);

/* GENERAL COUPLING THEORY (GCT) STUFF */
enum { eoPres, eoEpot, eoVir, eoDist, eoMu, eoForce, eoFx, eoFy, eoFz,
       eoPx, eoPy, eoPz, 
       eoPolarizability, eoDipole, eoObsNR, 
       eoMemory=eoObsNR, eoInter, eoUseVirial,  eoNR };
extern char *eoNames[eoNR];

typedef struct {
  int  at_i,at_j;   	/* Atom type # for i and j                   	*/
  int  eObs;		/* Observable to couple to              	*/
  bool bPrint;		/* Does this struct have to be printed		*/
  real c6,c12;		/* Actual value of params			*/
  real xi_6,xi_12;	/* Constants for coupling C6 and C12 		*/
} t_coupl_LJ;

typedef struct {
  int  at_i,at_j;   	/* Atom type # for i and j                   	*/
  int  eObs;		/* Observable to couple to              	*/
  bool bPrint;		/* Does this struct have to be printed		*/
  real a,b,c;		/* Actual value of params			*/
  real xi_a,xi_b,xi_c;	/* Constants for coupling A, B and C 		*/
} t_coupl_BU;

typedef struct {
  int  at_i;		/* Atom type					*/
  int  eObs;		/* Observable to couple to              	*/
  bool bPrint;		/* Does this struct have to be printed		*/
  real Q;		/* Actual value of charge			*/
  real xi_Q;		/* Constant for coupling Q			*/
} t_coupl_Q;

typedef struct {
  int       type;	/* Type number in the iparams struct	*/
  int       eObs;       /* Observable to couple to              */
  t_iparams xi;	        /* Parameters that need to be changed	*/
  t_iparams iprint;
} t_coupl_iparams;

typedef struct {
  real       act_value[eoObsNR];
  real       av_value [eoObsNR];
  real       ref_value[eoObsNR];
  bool       bObsUsed[eoObsNR];
  int        nLJ,nBU,nQ,nIP;
  t_coupl_LJ *tcLJ;
  t_coupl_BU *tcBU;
  t_coupl_Q  *tcQ;
  t_coupl_iparams *tIP;
  int        nmemory;
  bool       bInter;
  bool       bVirial;
} t_coupl_rec;

extern void write_gct(char *fn,t_coupl_rec *tcr,t_idef *idef);

extern void read_gct(char *fn,t_coupl_rec *tcr);

extern void comm_tcr(FILE *log,t_commrec *cr,t_coupl_rec **tcr);

extern void copy_ff(t_coupl_rec *tcr,t_forcerec *fr,t_mdatoms *md,
		    t_idef *idef);

extern t_coupl_rec *init_coupling(FILE *log,int nfile,t_filenm fnm[],
				  t_commrec *cr,t_forcerec *fr,t_mdatoms *md,
				  t_idef *idef);
				  
extern void calc_force(int natom,rvec f[],rvec fff[]);

extern void calc_f_dev(int natoms,real charge[],rvec x[],rvec f[],
		       t_idef *idef,real *xiH,real *xiS);

extern void do_coupling(FILE *log,int nfile,t_filenm fnm[],
			t_coupl_rec *tcr,real t,int step,real ener[],
			t_forcerec *fr,t_inputrec *ir,bool bMaster,
			t_mdatoms *md,t_idef *idef,real mu_aver,int nmols,
			t_commrec *cr,matrix box,tensor virial,
			tensor pres,rvec mu_tot,
			rvec x[],rvec f[],bool bDoIt);

/* IONIZATION STUFF. */
extern void ionize(FILE *log,t_mdatoms *md,char **atomname[],
		   real t,t_inputrec *ir,rvec x[],rvec v[],
		   int start,int end,matrix box,t_commrec *cr);

/* CODE TO ADD SPECIAL 2-DIMENSIONAL LENNARD-JONES CORRECTION TO FORCES AND ENERGY */
extern void do_glas(FILE *log,int start,int homenr,rvec x[],rvec f[],
		    t_forcerec *fr,t_mdatoms *md,int atnr,t_inputrec *ir,
		    real ener[]);

extern real mol_dipole(int k0,int k1,atom_id ma[],rvec x[],real q[]);
/* Calculate total dipole for group of atoms */

extern real calc_mu_aver(t_commrec *cr,t_nsborder *nsb,rvec x[],real q[],rvec mu,
			 t_topology *top,t_mdatoms *md,int gnx,atom_id grpindex[]);
/* Compute average dipole */

/********************************************************************/
/* Force field scanning stuff */
extern void update_forcefield(int nfile,t_filenm fnm[],t_forcerec *fr);
/* Modify the parameters */

extern void print_forcefield(FILE *fp,real energy,int natoms,rvec f[],rvec fshake[],
			     rvec x[],t_block *mols,real mass[]);
/* Print results */

extern void set_ffvars(real ff_tol,real ff_epot,real ff_npow,bool ff_bComb,
		       real ff_fmax,bool ff_bLogEps,real ratio);
/* Set variables for force scanning */

#endif	/* _xmdrun_h */
