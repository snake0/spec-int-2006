/*
 * $Id: gbutil.c,v 1.13 2002/02/28 10:49:23 spoel Exp $
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
 * Great Red Owns Many ACres of Sand 
 */
static char *SRCID_gbutil_c = "$Id: gbutil.c,v 1.13 2002/02/28 10:49:23 spoel Exp $";
#include <math.h>
#include "macros.h"
#include "vec.h"
#include "fatal.h"
#include "gstat.h"
#include "pbc.h"

static real dist2(rvec x,rvec y,matrix box)
{
  rvec dx;
  
  pbc_dx(x,y,dx);
  
  return norm2(dx);
}

real distance_to_z(rvec x)
{
  return (sqr(x[XX])+sqr(x[YY]));
} /*distance_to_z()*/

static void low_rotate_conf(int natom,rvec *x,real alfa, real beta,real gamma)
{
  int  i;
  rvec x_old;
  
  for (i=0; i<natom; i++) { 
    copy_rvec(x[i],x_old);
    /*calculate new x[i] by rotation alfa around the x-axis*/
    x[i][XX]=x_old[XX];
    x[i][YY]=x_old[YY]*cos(alfa)+x_old[ZZ]*sin(alfa);
    x[i][ZZ]=x_old[ZZ]*cos(alfa)-x_old[YY]*sin(alfa);
    copy_rvec(x[i],x_old);
    /*calculate new x[i] by rotation beta around the y-axis*/
    x[i][XX]=x_old[XX]*cos(beta)-x_old[ZZ]*sin(beta);
    x[i][YY]=x_old[YY];
    x[i][ZZ]=x_old[ZZ]*cos(beta)+x_old[XX]*sin(beta);
    copy_rvec(x[i],x_old);
    /*calculate new x[i] by rotation gamma around the z-axis*/
    x[i][XX]=x_old[XX]*cos(gamma)+x_old[YY]*sin(gamma);
    x[i][YY]=x_old[YY]*cos(gamma)-x_old[XX]*sin(gamma);
    x[i][ZZ]=x_old[ZZ];
  }
}

void rotate_conf(int natom,rvec *x,rvec *v,real alfa, real beta,real gamma)
{
  if (x)
    low_rotate_conf(natom,x,alfa,beta,gamma);
  if (v)
    low_rotate_conf(natom,v,alfa,beta,gamma);
}


void orient(int natom,rvec *x,rvec *v, rvec angle,matrix box)
{
  real longest,rij,rzi;
  int  i,j,m,max_i=0,max_j=0;
  rvec origin;
  int  temp;
  real alfa=0,beta=0,gamma=0;
  
  /*first i am going to look for the longest atom-atom distance*/
  longest=dist2(x[0],x[1],box);
  i=0;
  j=1;
  for (i=0;(i<natom);i++) {
    for (j=0;(j<natom);j++) {
      rij=dist2(x[i],x[j],box);
      if (rij>longest) {
	max_i=i;
	max_j=j;
	longest=rij;
      }
    }
  }
  /* first check if x[max_i]<x[max_j] else swap*/
  if (x[max_i][2]>x[max_j][2]) {
    temp=max_i;
    max_i=max_j;
    max_j=temp;
  }
  
  /*set the origin to x[i]*/
  for(m=0;(m<DIM);m++) 
    origin[m]=x[max_i][m];
  for(i=0;(i<natom);i++)
    for(m=0;(m<DIM);m++)
      x[i][m]-=origin[m];
      
  /* calculate the rotation angles alfa(x_axis) and beta(y_axis)
   * the rotation angles must be calculated clockwise looking 
   * along the rotation axis to the origin*
   * alfa (x-axis)
   */
  alfa=atan(x[max_j][ZZ]/x[max_j][YY])-M_PI_2;
  beta=M_PI_2-atan(x[max_j][ZZ]/x[max_j][XX]);
  rotate_conf(natom,x,v,alfa,beta,gamma);
  
  /* now search the longest distance for rotation along the z_axis */
  longest=distance_to_z(x[0]);
  max_i=0;
  for (i=1;(i<natom);i++) {
    rzi=distance_to_z(x[i]);
    if (rzi>longest) {
      longest = rzi;
      max_i=i;
    }
  }
  gamma=atan(x[max_i][YY]/x[max_i][XX])-M_PI_2;
  rotate_conf(natom,x,v,0,0,gamma);
  angle[0]=alfa;
  angle[1]=beta;
  angle[2]=gamma;
} /*orient()*/


void genconf(t_atoms *atoms,rvec *x,rvec *v,real *r,matrix box,ivec n_box)
{
  int     i,ix,iy,iz,m,j,imol,offset;
  rvec    delta;
  int     nmol;
  
  nmol=n_box[XX]*n_box[YY]*n_box[ZZ];
  
  /*print message*/
  fprintf(stderr,"Generating configuration\n");
  imol=0;
  for(ix=0; (ix < n_box[XX]); ix++) {
    delta[XX]=ix*box[XX][XX];
    for(iy=0; (iy < n_box[YY]); iy++) {
      delta[YY]=iy*box[YY][YY];
      for(iz=0; (iz < n_box[ZZ]); iz++) {
	delta[ZZ]=iz*box[ZZ][ZZ];
	offset=imol*atoms->nr;
	for (i=0;(i < atoms->nr);i++) {
	  for (m=0;(m < DIM);m++)
	    x[offset+i][m]=delta[m]+x[i][m];
	  if (v) 
	    for (m=0;(m < DIM);m++)
	      v[offset+i][m]=v[i][m];
	  r[offset+i]=r[i];
        }
	imol++;
      }
    }
  }
  for (i=1;(i<nmol);i++) {
    int offs    = i*atoms->nr;
    int offsres = i*atoms->nres;
    for (j=0;(j<atoms->nr);j++) {
      atoms->atomname[offs+j]  = atoms->atomname[j];
      atoms->atom[offs+j].resnr = atoms->atom[j].resnr+offsres;
      atoms->resname[atoms->atom[offs+j].resnr]=
	atoms->resname[atoms->atom[j].resnr];
    }
  }
  atoms->nr*=nmol;
  atoms->nres*=nmol;
  for(i=0; i<DIM; i++)
    for(j=0; j<DIM; j++)
      box[j][i]*=n_box[j];
} /*genconf()*/

/*gen_box() generates a box around a configuration*/
void gen_box(int NTB,int natoms,rvec *x, matrix box,rvec box_space,
	     bool bCenter)
{
  int i,m;
  rvec xmin, xmax;
  real max_box;
  
  /*calculate minimum and maximum x[0..DIM-1]*/
  for (m=0;(m<DIM);m++)
    xmin[m]=xmax[m]=x[0][m];
  for (i=1;(i < natoms); i++) 
    for (m=0;m<DIM;m++) {
      xmin[m]=min(xmin[m],x[i][m]);
      xmax[m]=max(xmax[m],x[i][m]);
    }
    
  /*calculate the new box sizes for cubic and octahedral ...*/
  for (m=0; (m<DIM);m++)
    box[m][m]=xmax[m]-xmin[m]+2*box_space[m];
 
  /*calculate the box size if NTB=1 (truncated octahedron)*/
  if (NTB==1) {
    max_box=box[0][0];
    for(m=0;(m<DIM);m++)
      max_box=max(max_box,box[m][m]); 
    for (m=0;(m<DIM);m++)
      box[m][m]=max_box;
  }
  
  /*move the molecule to the center of the box*/
  if (bCenter)
    for(i=0;(i<natoms);i++)
      for (m=0;(m<DIM);m++) {
	x[i][m]+=0.5*(box[m][m]-xmin[m]-xmax[m]);
      }


#ifdef DEBUG 
  /* print data to check this */
  print_stat(x,natoms,box);
#endif
}/*gen_box()*/

