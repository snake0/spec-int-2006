/*
 * $Id: random.h,v 1.12 2002/02/28 21:55:50 spoel Exp $
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

#ifndef _random_h
#define _random_h

static char *SRCID_random_h = "$Id: random.h,v 1.12 2002/02/28 21:55:50 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


#include <typedefs.h>

extern real gauss(real am, real sd, int *ig);
/* Generate a gaussian number with:
 * am = center of the distribution
 * sd = standard deviation
 * ig = the random number seed.
 */

extern int make_seed(void);
/* Make a random seed: (time+getpid) % 1000000 */

extern real rando(int *ig);
/* Generate a random number 0 <= r < 1. ig is the (address of) the
 * seed variable.
 */

extern void grp_maxwell(t_block *grp,real tempi[],int nrdf[],int seed,
			t_atoms *atoms,rvec v[]);
/* Generate for each group in grp a temperature.
 * When seed = -1, set the seed to make_seed.
 */

extern void maxwell_speed(real tempi,int nrdf,int seed, 
			  t_atoms *atoms, rvec v[]);
/* Generate velocites according to a maxwellian distribution */

extern real calc_cm(FILE *log,int natoms,real mass[],rvec x[],rvec v[],
		    rvec xcm,rvec vcm,rvec acm,matrix L);
/* Calculate the c.o.m. position, velocity, acceleration and the
 * moment of Inertia. Returns the total mass.
 */

extern void stop_cm(FILE *log,int natoms,real mass[],rvec x[],rvec v[]);

#endif	/* _random_h */
