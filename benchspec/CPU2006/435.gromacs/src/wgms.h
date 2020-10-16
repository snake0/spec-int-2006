/*
 * $Id: wgms.h,v 1.7 2002/02/28 21:55:52 spoel Exp $
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

#ifndef _wgms_h
#define _wgms_h

static char *SRCID_wgms_h = "$Id: wgms.h,v 1.7 2002/02/28 21:55:52 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include "typedefs.h"

extern void write_gms(FILE *fp,int natoms,rvec x[],matrix box);
/* Write a gromos-87 trajectory frame (10f8.3) + box size 
 * If box == NULL it is not written
 */

extern void write_gms_ndx(FILE *fp,int isize,atom_id index[],
			  rvec x[],matrix box);
/* Write a gromos-87 trajectory frame (10f8.3) + box size for
 * a subset of the atoms.
 * If box == NULL it is not written
 */

#endif
