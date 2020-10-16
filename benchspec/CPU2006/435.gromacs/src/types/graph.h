/*
 * $Id: graph.h,v 1.7 2002/02/28 21:55:53 spoel Exp $
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

#include "fatal.h"

typedef struct {
  int      maxbond;     /* Max number of bonds per atom                 */
  int      nnodes;	/* The number of nodes				*/
  int      nbound;	/* The number of nodes with edges		*/
  int      start;	/* The first atom in this graph			*/
  int      end;		/* The last atom in this graph			*/
  int      *nedge;	/* For each node the number of edges		*/
  atom_id  **edge;	/* For each node, the actual edges (bidirect.)	*/
  ivec     *ishift;	/* Shift for each particle              	*/
} t_graph;



#define SHIFT_IVEC(g,i) ((g)->ishift[(i)-(g)->start])

