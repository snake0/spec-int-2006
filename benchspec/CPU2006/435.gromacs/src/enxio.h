/*
 * $Id: enxio.h,v 1.8 2002/02/28 21:55:49 spoel Exp $
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

#ifndef _enxio_h
#define _enxio_h

static char *SRCID_enxio_h = "$Id: enxio.h,v 1.8 2002/02/28 21:55:49 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


#ifdef CPLUSPLUS
external "C" {
#endif

  /**************************************************************
   *
   * The routines in the corresponding c-file enxio.c
   * are based on the lower level routines in gmxfio.c
   * The integer file pointer returned from open_enx
   * can also be used with the routines in gmxfio.h
   *
   **************************************************************/

#include "sysstuff.h"
#include "typedefs.h"
  
  /* 
   * Index for the additional blocks in the energy file.
   * Blocks can be added without sacrificing backward and forward
   * compatibility of the energy files.
   */
  enum {
    enxOR,   /* Time and ensemble averaged data for orientation restraints */
    enxORI,  /* Instantaneous data for orientation restraints              */
    enxNR    /* Total number of extra blocks in the current code,
              * note that the enxio code can read files written by
	      * future code which contain more blocks.
	      */
  };

  typedef struct {
    real     t;	       /* Timestamp of this frame		           */
    int      step;     /* MD step				           */
    int      nre;      /* Number of energies			           */
    int      ndisre;   /* Number of distance restraints	                   */
    int      nblock;   /* Number of following energy blocks                */
    int      *nr;      /* Number of things in additional blocks (nblock)   */
    int      e_size;   /* Size (in bytes) of energies		           */
    int      d_size;   /* Size (in bytes) of disre blocks	           */
    int      nr_alloc; /* Allocated size of nr and block                   */
    int      e_alloc;  /* Allocated size (in elements) of ener             */
    int      d_alloc;  /* Allocated size (in elements) of rav and rt       */
    int      *b_alloc; /* Allocated size (in elements) of each block       */
    t_energy *ener;    /* The energies                                     */
    real     *rav;     /* Time averaged data for distance restraints       */
    real     *rt;      /* Instantaneous data for distance restraints       */
    real     **block;  /* Additional energy blocks ( nblock x b_alloc[b])  */
  } t_enxframe;

  /* 
   * An energy file is read like this:
   *
   * int fp;
   * t_enxframe *fr;
   *
   * fp = open_enx(...);
   * do_enxnms(fp,...);
   * snew(fr,1);
   * while (do_enx(fp,fr)) {
   * ...
   * }
   * free_enxframe(fr);
   * sfree(fr);
   */
  
  /* New energy reading and writing interface */
  extern void free_enxframe(t_enxframe *fr);
  /* Frees all allocated memory in fr */

  extern int open_enx(char *fn,char *mode);
  
  extern void close_enx(int fp_ene);
  
  extern void do_enxnms(int fp_ene,int *nre,char ***nms);
  
  extern bool do_enx(int fp_ene,t_enxframe *fr);
  /* Reads enx_frames, memory in fr is (re)allocated if necessary */

#ifdef CPLUSPLUS
}
#endif

#endif	/* _enerio_h */
