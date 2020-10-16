/*
 * $Id: invblock.c,v 1.6 2002/02/28 10:49:23 spoel Exp $
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
 * Gyas ROwers Mature At Cryogenic Speed
 */
static char *SRCID_invblock_c = "$Id: invblock.c,v 1.6 2002/02/28 10:49:23 spoel Exp $";
#include "typedefs.h"
#include "smalloc.h"
#include "invblock.h"

atom_id *make_invblock(t_block *block,int nr)
{
  int i,j;
  atom_id *invblock;
  
  snew(invblock,nr);
  for (i=0; i<nr; i++) invblock[i]=NO_ATID; /* Mark unused numbers */
  j=1;
  for (i=0; i<block->nr; i++)
    for (j=block->index[i]; j<(int)block->index[i+1]; j++) 
      invblock[block->a[j]]=i;
  return invblock;
}

