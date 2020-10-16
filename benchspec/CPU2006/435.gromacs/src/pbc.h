/*
 * $Id: pbc.h,v 1.25 2002/02/28 21:55:50 spoel Exp $
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

#ifndef _pbc_h
#define _pbc_h

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <math.h>
#include "types/simple.h"
#include "sysstuff.h"
#include "typedefs.h"

#ifdef CPLUSPLUS
extern "C" { 
#endif

#define BOX_MARGIN 0.5001
  /* margin factor for checking if the box is too skewed */

#define TRICLINIC(box) ( (fabs(box[YY][XX]) > GMX_REAL_MIN) || (fabs(box[ZZ][XX]) > GMX_REAL_MIN) || (fabs(box[ZZ][YY]) > GMX_REAL_MIN))

#define NTRICIMG 14
#define NCUCVERT 24
#define NCUCEDGE 36

  extern char *check_box(matrix box);
  /* Returns NULL if the box is supported by Gromacs.
   * Otherwise is returns a string with the problem.
   */

  extern void init_pbc(matrix box);
  /* Initiate the periodic boundary conditions. */
  
  extern void pbc_dx(const rvec x1, const rvec x2, rvec dx);
  /* Calculate the correct distance vector from x1 and x2 and put it in
   * dx. init_pbc must be called before ever calling this routine
   * (this is done by put_charge_groups_in_box).
   */

  extern bool image_rect(ivec xi,ivec xj,ivec box_size,
			 real rlong2,int *shift,real *r2);
  /* Calculate the distance between xi and xj for a rectangular box.
   * When the distance is SMALLER than rlong2 return TRUE, return
   * the shift code in shift and the distance in r2. When the distance is
   * >= rlong2 return FALSE;
   * It is assumed that rlong2 is scaled the same way as the ivecs xi and xj.
   */

  extern bool image_tri(ivec xi,ivec xj,imatrix box,
			real rlong2,int *shift,real *r2);
  /* Calculate the distance between xi and xj for a triclinic box.
   * When the distance is SMALLER than rlong2 return TRUE, return
   * the shift code in shift and the distance in r2. When the distance is
   * >= rlong2 return FALSE;
   * It is assumed that rlong2 is scaled the same way as the ivecs xi and xj.
   */
  
  extern bool image_cylindric(ivec xi,ivec xj,ivec box_size,real rlong2,
			      int *shift,real *r2);
  /* Calculate the distance between xi and xj for a rectangular box
   * using a cylindric cutoff for long-range only.
   * When the distance is SMALLER than rlong2 (in X and Y dir.)
   * return TRUE, return
   * the shift code in shift and the distance in r2. When the distance is
   * >= rlong2 return FALSE;
   * It is assumed that rlong2 is scaled the same way as the ivecs xi and xj.
   */

  extern void calc_shifts(matrix box,rvec box_size,rvec shift_vec[]);
  /* This routine calculates ths shift vectors necessary to use the
   * ns routine. Note that for the truncated octahedron case too many
   * shift vectors can be calculated: The ones for which exactly
   * 2 of the k,l,m indexes are not 0 (12 vectors lying along the box
   * edges. This can be compensated for by removing all the shift_vecs with
   * (k+l+m) even. This is a feature of the way in which the counting is 
   * done. It implies that, when using truncated octahedron,
   * the shift codes 1,3,5,7,9,11,15,17,19,21,23,25 should never occur,
   * that is, every second entry, EXCEPT the central box.
   */

  extern void calc_cgcm(FILE *log,int cg0,int cg1,t_block *cgs,
			rvec pos[],rvec cg_cm[]);
  /* Routine to compute centers of geometry of charge groups. No periodicity
   * is used.
   */
  
  extern void put_charge_groups_in_box (FILE *log,int cg0,int cg1,
					matrix box,rvec box_size,t_block *cgs,
					rvec pos[],
					rvec cg_cm[]);
			    
  /* This routine puts charge groups in the periodic box, keeping them
   * together.
   */

  extern void calc_box_center(matrix box,rvec box_center);
  /* Calculates the center of the box */

  extern void calc_triclinic_images(matrix box,rvec img[]);
  /* Calculates the NTRICIMG box images */

  extern void calc_compact_unitcell_vertices(matrix box,rvec vert[]);
  /* Calculates the NCUCVERT vertices of a compact unitcell */
  
  extern int *compact_unitcell_edges(void);
  /* Return an array of unitcell edges of length NCUCEDGE*2,
   * this is an index in vert[], which is calculated by calc_unitcell_vertices.
   * The index consists of NCUCEDGE pairs of vertex indices.
   * The index does not change, so it needs to be retrieved only once.
   */

  extern void put_atoms_in_box(matrix box,int natoms,rvec x[]);
  /* This puts ALL atoms in the box, not caring about charge groups!
   * Also works for triclinic cells.
   */
  
  extern void put_atoms_in_triclinic_unitcell(matrix box,int natoms,rvec x[]);
  /* This puts ALL atoms in the triclinic unit cell, centered around the
   * box center as calculated by calc_box_center.
   */

  extern void put_atoms_in_compact_unitcell(matrix box,int natoms,rvec x[]);
  /* This puts ALL atoms at the closest distance for the center of the box
   * as calculated by calc_box_center.
   */
  
  extern void set_gmx_full_pbc(FILE *fp);
  /* Turn on full PBS calculation in the GROMACS bonded routines */
  
#ifdef CPLUSPLUS
}
#endif

#endif	/* _pbc_h */
