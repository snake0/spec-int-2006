/*
 * $Id: nrnb.h,v 1.11 2002/02/28 21:55:50 spoel Exp $
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

#ifndef _nrnb_h
#define _nrnb_h

static char *SRCID_nrnb_h = "$Id: nrnb.h,v 1.11 2002/02/28 21:55:50 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "typedefs.h"

extern void init_nrnb(t_nrnb *nrnb);

extern void cp_nrnb(t_nrnb *dest, t_nrnb *src);

extern void add_nrnb(t_nrnb *dest, t_nrnb *s1, t_nrnb *s2);

extern void print_nrnb(FILE *out, t_nrnb *nrnb);

extern void _inc_nrnb(t_nrnb *nrnb,int enr,int inc,char *file,int line);

#if DEBUG_NRNB
#define inc_nrnb(nrnb,enr,inc) _inc_nrnb(nrnb,enr,inc,__FILE__,__LINE__)
#else
#define inc_nrnb(nrnb,enr,inc) (nrnb)->n[enr] += inc
#endif

extern void print_perf(FILE *out,double nodetime,double realtime,real runtime,
		       t_nrnb *nrnb,int nprocs);

extern void pr_load(FILE *log,int nprocs,t_nrnb nrnb[]);
/* Print detailed load balancing info */

extern int cost_nrnb(int enr);
/* Cost in i860 cycles of this component of MD */

extern char *nrnb_str(int enr);
/* Name of this component */

#endif	/* _nrnb_h */
