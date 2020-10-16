/*
 * $Id: wnblist.h,v 1.4 2002/02/28 10:32:06 spoel Exp $
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
 * Gromacs Runs On Most of All Computer Systems
 */

#ifndef _wnblist_h
#define _wnblist_h

static char *SRCID_wnblist_h = "$Id: wnblist.h,v 1.4 2002/02/28 10:32:06 spoel Exp $";

#include "stdio.h"
#include "typedefs.h"

extern void dump_nblist(FILE *out,t_forcerec *fr,int nDNL);

extern void read_nblist(FILE *in,FILE *out,int **mat,int natoms);

#endif	/* _wnblist_h */
