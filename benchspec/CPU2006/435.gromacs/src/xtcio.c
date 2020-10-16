/*
 * $Id: xtcio.c,v 1.14 2002/02/28 10:49:35 spoel Exp $
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
static char *SRCID_xtcio_c = "$Id: xtcio.c,v 1.14 2002/02/28 10:49:35 spoel Exp $";
#include <string.h>
#include "typedefs.h"
#include "xdrf.h"
#include "gmxfio.h"
#include "xtcio.h"
#include "smalloc.h"
#include "vec.h"
#include "futil.h"
#include "fatal.h"

#define XTC_MAGIC 1995

int open_xtc(char *fn,char *mode)
{
  return gmx_fio_open(fn,mode);
}

void close_xtc(int fp)
{
  gmx_fio_close(fp);
}

static void check_xtc_magic(int magic)
{
  if (magic != XTC_MAGIC) 
    fatal_error(0,"Magic Number Error in XTC file (read %d, should be %d)",
		magic,XTC_MAGIC);
}

int xtc_check(char *str,bool bResult,char *file,int line)
{
  if (!bResult) {
    if (debug)
      fprintf(debug,"\nXTC error: read/write of %s failed, "
	      "source file %s, line %d\n",str,file,line);
    return 0;
  }
  return 1;
}

void xtc_check_fat_err(char *str,bool bResult,char *file,int line)
{
  if (!bResult) {
    fatal_error(0,"XTC read/write of %s failed, "
		"source file %s, line %d\n",str,file,line);
  }
}

static int xtc_header(XDR *xd,int *magic,int *natoms,int *step,real *time,
		      bool *bOK)
{
  int result;

  if (xdr_int(xd,magic) == 0)
    return 0;
  result=XTC_CHECK("natoms", xdr_int(xd,natoms));  /* number of atoms */
  if (result)
    result=XTC_CHECK("step",   xdr_int(xd,step));    /* frame number    */
  if (result)
    result=XTC_CHECK("time",   xdr_real(xd,time));   /* time            */
  *bOK=(result!=0);

  return result;
}

static int xtc_coord(XDR *xd,int *natoms,matrix box,rvec *x,real *prec)
{
  int i,j,result;
  
  /* box */
  result=1;
  for(i=0; ((i<DIM) && result); i++)
    for(j=0; ((j<DIM) && result); j++)
      result=XTC_CHECK("box",xdr_real(xd,&(box[i][j])));
  
  if (result)
    /* coordinates     */
    result=XTC_CHECK("x",xdr3drcoord(xd,x[0],natoms,prec)); 
  
  return result;
}

static int xtc_io(XDR *xd,int *magic,
		  int *natoms,int *step,real *time,
		  matrix box,rvec *x,real *prec,bool *bOK)
{
  if (!xtc_header(xd,magic,natoms,step,time,bOK))
    return 0;
  return xtc_coord(xd,natoms,box,x,prec);
}

int write_xtc(int fp,
	      int natoms,int step,real time,
	      matrix box,rvec *x,real prec)
{
  int magic_number = XTC_MAGIC;
  XDR *xd;
  bool bDum;

  xd = gmx_fio_getxdr(fp);
  /* write magic number and xtc identidier */
  if (!xtc_header(xd,&magic_number,&natoms,&step,&time,&bDum))
    return 0;
    
  /* write data */
  return xtc_coord(xd,&natoms,box,x,&prec);
}

int read_first_xtc(int fp,int *natoms,int *step,real *time,
		   matrix box,rvec **x,real *prec,bool *bOK)
{
  int magic;
  XDR *xd;
  
  *bOK=TRUE;
  xd = gmx_fio_getxdr(fp);
  
  /* read header and malloc x */
  if ( !xtc_header(xd,&magic,natoms,step,time,bOK))
    return 0;
    
  /* Check magic number */
  check_xtc_magic(magic);
  
  snew(*x,*natoms);

  *bOK=xtc_coord(xd,natoms,box,*x,prec);
  
  return *bOK;
}

int read_next_xtc(int fp,
		  int natoms,int *step,real *time,
		  matrix box,rvec *x,real *prec,bool *bOK)
{
  int magic;
  int n;
  XDR *xd;

  *bOK=TRUE;
  xd = gmx_fio_getxdr(fp);
  
  /* read header */
  if (!xtc_header(xd,&magic,&n,step,time,bOK))
    return 0;
  if (n>natoms)
    fatal_error(0, "Frame contains more atoms (%d) than expected (%d)", 
		n, natoms);
    
  /* Check magic number */
  check_xtc_magic(magic);

  *bOK=xtc_coord(xd,&natoms,box,x,prec);

  return *bOK;
}

