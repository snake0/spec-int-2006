/*
 * $Id: mdebin.h,v 1.21 2002/02/28 21:55:49 spoel Exp $
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

#ifndef _mdebin_h
#define _mdebin_h

static char *SRCID_mdebin_h = "$Id: mdebin.h,v 1.21 2002/02/28 21:55:49 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "typedefs.h"
#include "sysstuff.h"
#include "ebin.h"
#include "enxio.h"

typedef struct {
  t_ebin *ebin;
  int    ie,ib,isvir,ifvir,ipres,ivir,isurft,itc,iu,imu,ivcos,ivisc;
  int    nE,nEg,nEc,nTC,nU;
  int    *igrp;
} t_mdebin;

extern t_mdebin *init_mdebin(int fp_ene,t_groups *grps,t_atoms *atoms,
			     t_idef *idef,bool bLR,bool BLJLR,bool bBHAM,
			     bool b14,bool bFEP,bool bPcoupl,bool
			     bDispCorr,bool bTriclinic,bool bNoseHoover, t_commrec *cr);
/* Initiate MD energy bin and write header to energy file. */

extern void upd_mdebin(t_mdebin *md,FILE *fp_dgdl,
		       real tmass,int step,real time,
		       real ener[],
		       matrix box,
		       tensor svir,
		       tensor fvir,
		       tensor vir,
		       tensor pres,
		       t_groups *grps,
		       rvec mu_tot, bool bNoseHoover);
     
extern void print_ebin_header(FILE *log,int steps,real time,
			      real lamb,real SAfactor);

extern void print_ebin(int fp_ene,bool bEne,bool bDR,bool bOR,
		       FILE *log,int steps,real time,
		       int mode,bool bCompact,
		       t_mdebin *md,t_fcdata *fcd,t_atoms *atoms);

#endif	/* _mdebin_h */

