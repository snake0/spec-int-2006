/*
 * $Id: ifunc.h,v 1.13 2002/02/28 21:55:53 spoel Exp $
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
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifndef _ifunc_h
#define _ifunc_h


typedef real t_ifunc(int nbonds,t_iatom iatoms[],t_iparams *iparams,
                     rvec x[],rvec f[],t_forcerec *fr,t_graph *g,
		     matrix box,real lambd,real *dvdlambda,
		     t_mdatoms *md,int ngrp,real egnb[],real egcoul[],
		     t_fcdata *fcd);
/*
 * The function type t_ifunc() calculates one interaction, using iatoms[] 
 * and iparams. Within the function the number of atoms to be used is 
 * known. Within the function only the atomid part of the iatoms[] array 
 * is supplied, not the type field (see also t_ilist). The function 
 * returns the potential energy. The coordinates in x are such that
 * no calculation of PBC is necessary.
 */

#define IF_NULL       0
#define IF_BOND       1
#define IF_DUMMY      1<<1
#define IF_CONSTRAINT 1<<2
#define IF_CONNECT    1<<3
#define IF_BTYPE      1<<5
#define IF_ATYPE      1<<6
/* These flags tell to some of the routines what can be done with this
 * item in the list. If flags & IF_BOND, then bonded interactions will
 * be calculated. If flags & IF_CONNECT this link specifies a connection 
 * (chemical bond) between two particles. By specifying this here, we can 
 * keep all the information in one place.
 */
typedef struct
{
  char    *name;	/* the name of this function			*/
  char    *longname;    /* The name for printing etc.                   */
  int     nratoms;	/* nr of atoms needed for this function		*/
  int     nrfpA,nrfpB;  /* number of parameters for this function.      */
                        /* this corresponds to the number of params in  */
                        /* iparams struct! (see idef.h)                 */
  /* A and B are for normal and free energy components respectively.    */
  unsigned long   flags;        /* Flags (see above)                            */
  int     nrnb_ind;     /* index for nrnb (-1 if unknown)               */
  t_ifunc *ifunc;	/* the function it self				*/
} t_interaction_function;

#define NRFP(ftype) (interaction_function[(ftype)].nrfpA+interaction_function[(ftype)].nrfpB)
#define NRAL(ftype) (interaction_function[(ftype)].nratoms)

#define IS_CHEMBOND(ftype) (interaction_function[(ftype)].nratoms==2 && interaction_function[(ftype)].flags & IF_CONNECT)
/* IS_CHEMBOND tells if function type ftype represents a chemical bond */

extern t_interaction_function interaction_function[F_NRE];
/* initialised interaction functions descriptor				*/

#endif

