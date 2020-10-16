/*
 * $Id: fftgrid.h,v 1.12 2002/02/28 21:55:49 spoel Exp $
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

#ifndef _fftgrid_h
#define _fftgrid_h

static char *SRCID_fftgrid_h = "$Id: fftgrid.h,v 1.12 2002/02/28 21:55:49 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include "typedefs.h"
#include "fftw_wrapper.h"
#include "gmxcomplex.h"
#include "network.h"

#define WITHOUT_FFTW

/* Use FFTW */

#ifndef WITHOUT_FFTW
typedef t_complex t_fft_c;
typedef real      t_fft_r;
#else /* NO FFTW PRESENT! */
typedef struct {
  real re, im;
} t_fft_c;
typedef real t_fft_r;
typedef enum {
  FFTW_FORWARD = -1, FFTW_BACKWARD = 1
} fftw_direction;
#endif

#define INDEX(i,j,k)             ((i)*la12+(j)*la2+(k))      

typedef struct {
  int local_nx,local_x_start,local_ny_after_transpose;
  int local_y_start_after_transpose,total_local_size;
} t_parfft;

typedef struct {
    t_fft_r *ptr;
    t_fft_r *localptr;
    t_fft_r *workspace;    
    int      nx,ny,nz,la2r,la2c,la12r,la12c;
  int      nptr,nxyz;
#ifndef WITHOUT_FFTW
    rfftwnd_plan     plan_fw;
    rfftwnd_plan     plan_bw;
#ifdef USE_MPI
    rfftwnd_mpi_plan plan_mpi_fw;
    rfftwnd_mpi_plan plan_mpi_bw;
    t_parfft         pfft;
#endif
#endif
} t_fftgrid;

extern t_fftgrid *mk_fftgrid(FILE *fp,bool bParallel,int nx,int ny,
			     int nz,bool bOptFFT);
/* Create an FFT grid (1 Dimensional), to be indexed by the INDEX macro 
 * Setup FFTW plans and extract local sizes for the grid.
 * If the file pointer is given, information is printed to it.
 */

extern void done_fftgrid(t_fftgrid *grid);
/* And throw it away again */

extern void gmxfft3D(t_fftgrid *grid,int dir,t_commrec *cr);
/* Do the FFT, direction may be either 
 * FFTW_FORWARD (sign -1) for real -> complex transform 
 * FFTW_BACKWARD (sign 1) for complex -> real transform
 */
 
extern void clear_fftgrid(t_fftgrid *grid);
/* Set it to zero */

extern void unpack_fftgrid(t_fftgrid *grid,int *nx,int *ny,int *nz,
			   int *la2, int *la12,bool bReal, t_fft_r **ptr);

/* Get the values for the constants into local copies */




/************************************************************************
 * 
 * For backward compatibility (for testing the ewald code vs. PPPM etc)
 * some old grid routines are retained here.
 *
 ************************************************************************/
 
extern real ***mk_rgrid(int nx,int ny,int nz);

extern void free_rgrid(real ***grid,int nx,int ny);

extern real print_rgrid(FILE *fp,char *title,int nx,int ny,int nz,
			real ***grid);

extern void print_rgrid_pdb(char *fn,int nx,int ny,int nz,real ***grid);

extern t_complex ***mk_cgrid(int nx,int ny,int nz);

extern void free_cgrid(t_complex ***grid,int nx,int ny);

extern t_complex print_cgrid(FILE *fp,char *title,int nx,int ny,int nz,
			   t_complex ***grid);

extern void clear_cgrid(int nx,int ny,int nz,t_complex ***grid);

extern void clear_rgrid(int nx,int ny,int nz,real ***grid);

#endif






