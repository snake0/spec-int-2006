/*
 * $Id: mvdata.h,v 1.7 2002/02/28 21:55:50 spoel Exp $
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

#ifndef _mvdata_h
#define _mvdata_h

static char *SRCID_mvdata_h = "$Id: mvdata.h,v 1.7 2002/02/28 21:55:50 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


#include "typedefs.h"
#include "nsb.h"

extern void ld_data(int left,int right,t_parm *parm,t_nsborder *nsb,
		    t_topology *top,rvec **x,rvec **v);

extern void mv_data(int left,int right,t_parm *parm,t_nsborder *nsb,
		    t_topology *top,rvec x[],rvec v[]);

extern void move_cgcm(FILE *log,t_commrec *cr,rvec cg_cm[],int nload[]);
		     
extern void move_rvecs(FILE *log,bool bForward,bool bSum,
		       int left,int right,rvec vecs[],rvec buf[],
		       int shift,t_nsborder *nsb,t_nrnb *nrnb);

extern void move_x(FILE *log,
		   int left,int right,rvec x[],t_nsborder *nsb,t_nrnb *nrnb);
		    
extern void move_f(FILE *log,
		   int left,int right,rvec f[],rvec fadd[],
		   t_nsborder *nsb,t_nrnb *nrnb);
		    
#endif	/* _mvdata_h */
