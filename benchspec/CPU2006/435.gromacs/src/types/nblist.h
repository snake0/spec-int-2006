/*
 * $Id: nblist.h,v 1.11 2002/02/28 21:55:53 spoel Exp $
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

typedef struct {
  int     il_code;             /* Code that determines the innerloop    */
                               /* corresponding to codes in nrnb.h      */
  int     nri,maxnri;          /* Current/max number of i particles	*/
  int     nrj,maxnrj;	       /* Current/max number of j particles	*/
  int     maxlen;              /* maxnr of j atoms for a single i atom 	*/
  int     solvent;             /* type of solvent optimization          */
  atom_id *iinr;	       /* The i-elements			*/
  int     *gid;                /* Index in energy arrays                */
  int     *shift;              /* Shift vector index                    */
  int     *jindex;             /* Index in jjnr                         */
  atom_id *jjnr;	       /* The j-atom list                       */
  int     *nsatoms;            /* list with number of atoms for general */
                               /* solvents. There are two entries for 	*/
                               /* each molecule - first is total natoms */
                               /* and second how many at the beginning 	*/
                               /* have LJ interactions.                 */
                               /* This is NOT used for water!           */
#ifdef USE_THREADS
  int      count;              /* counter to multithread the innerloops */
  pthread_mutex_t *mtx;        /* mutex to lock the counter             */
#else
  int      pad1,*pad2;         /* padding to make size constant         */
#endif
} t_nblist;

/* For atom I =  nblist->iinr[N] (0 <= N < nblist->nri) there can be
 * several neighborlists (N's), for different energy groups (gid) and
 * different shifts (shift).
 * For corresponding J atoms for each list are are:
 * nblist->jjnr[JI]
 * with nblist->jindex[N] <= JI < nblist->jindex[N+1]
 *
 * Clear?
 */










