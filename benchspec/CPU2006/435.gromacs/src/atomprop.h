/*
 * $Id: atomprop.h,v 1.6 2002/02/28 21:55:48 spoel Exp $
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
 * Grunge ROck MAChoS
 */
static char *SRCID_atomprop_h = "$Id: atomprop.h,v 1.6 2002/02/28 21:55:48 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

extern real get_mass(char *resnm, char *atomnm);
/* search the mass belonging to residue and atom,
   note that the longest match is returned */

extern real get_vdw(char *resnm, char *atomnm, real default_r);
/* search the vdw radius belonging to residue and atom ,
   note that the longest match is returned */
   
extern real get_dgsolv(char *resnm, char *atomnm, real default_s);
/* search the Delta G solv belonging to residue and atom ,
   note that the longest match is returned */
