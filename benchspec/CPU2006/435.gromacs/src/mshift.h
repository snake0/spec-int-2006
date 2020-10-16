/*
 * $Id: mshift.h,v 1.9 2002/02/28 21:55:50 spoel Exp $
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

#ifndef _mshift_h
#define _mshift_h

static char *SRCID_mshift_h = "$Id: mshift.h,v 1.9 2002/02/28 21:55:50 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


#include "typedefs.h"

extern t_graph *mk_graph(t_idef *idef,int natoms,bool bShakeOnly,bool bSettle);
/* Build a graph from an idef description. The graph can be used
 * to generate mol-shift indices.
 * If bShakeOnly, only the connections in the shake list are used.
 * If bSettle && bShakeOnly the settles are used too.
 */
extern void done_graph(t_graph *g);
/* Free the memory in g */
 
extern void p_graph(FILE *log,char *title,t_graph *g);
/* Print a graph to log */

extern void mk_mshift(FILE *log,t_graph *g,matrix box,rvec x[]);
/* Calculate the mshift codes, based on the connection graph in g. */

extern void shift_atom(t_graph *g,matrix box,rvec x[],rvec x_s[],atom_id ai);
/* Shift single atom ai */

extern void shift_x(t_graph *g,matrix box,rvec x[],rvec x_s[]);
/* Add the shift vector to x, and store in x_s (may be same array as x) */

extern void shift_self(t_graph *g,matrix box,rvec x[]);
/* Id. but in place */

extern void unshift_atom(t_graph *g,matrix box,rvec x[],rvec x_s[],atom_id ai);
/* Unshift single atom ai */

extern void unshift_x(t_graph *g,matrix box,rvec x[],rvec x_s[]);
/* Subtract the shift vector from x_s, and store in x (may be same array) */

extern void unshift_self(t_graph *g,matrix box,rvec x[]);
/* Id, but in place */

#endif	/* _mshift_h */
