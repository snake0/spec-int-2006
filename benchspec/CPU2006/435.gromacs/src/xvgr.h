/*
 * $Id: xvgr.h,v 1.14 2002/02/28 21:55:52 spoel Exp $
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

#ifndef _xvgr_h
#define _xvgr_h

static char *SRCID_xvgr_h = "$Id: xvgr.h,v 1.14 2002/02/28 21:55:52 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef CPLUSPLUS
extern "C" {
#endif

#include "sysstuff.h"
#include "typedefs.h"
#include "viewit.h"

/***************************************************
 *            XVGR   DEFINITIONS
 ***************************************************/
enum {
  elNone, elSolid, elDotted, elDashed, 
  elLongDashed, elDotDashed, elNR
};
/* xvgr line-styles */

enum {  
  ecWhite, ecFrank, ecBlack=ecFrank,
  ecRed, ecGreen, ecBlue, ecYellow, ecBrown, ecGray, 
  ecPurple, ecLightBlue, ecViolet, ecHolland, ecLila, ecDarkGray, 
  ecAquamarine, ecOlive, ecNR
};
/* xvgr line-colors */

enum {
  eppNone, eppColor, eppPattern, eppNR
};
/* xvgr pattern type */

enum {
  evView, evWorld, evNR
};
/* view type */

/***************************************************
 *            XVGR   ROUTINES
 ***************************************************/

extern FILE *xvgropen(char *fn,char *title,char *xaxis,char *yaxis);
/* Open a file, and write a title, and axis-labels in Xvgr format */

extern void xvgr_view(FILE *out,real xmin,real ymin,real xmax,real ymax);
/* Set the view in xvgr */

extern void xvgr_world(FILE *out,real xmin,real ymin,real xmax,real ymax);
/* Set the world in xvgr */

extern void xvgr_legend(FILE *out,int nsets,char *setname[]);
/* Make a legend box, and also modifies the view to make room for the legend */

extern void xvgr_line_props(FILE *out,int NrSet,int LineStyle,int LineColor);
/* Set xvgr line styles and colors */

extern void xvgr_box(FILE *out,
		     int LocType,
		     real xmin,real ymin,real xmax,real ymax,
		     int LineStyle,int LineWidth,int LineColor,
		     int BoxFill,int BoxColor,int BoxPattern);
/* Make a box */

extern int read_xvg(char *fn,real ***y,int *ny);
/* Read an xvg file for post processing. The number of rows is returned
 * fn is the filename, y is a pointer to a 2D array (to be allocated by
 * the routine) ny is the number of columns (including X if appropriate)
 */
 
extern void write_xvg(char *fn,char *title,int nx,int ny,real **y,char **leg);
/* Write a two D array (y) of dimensions nx rows times
 * ny columns to a file. If leg != NULL it will be written too.
 */

/****************************************************
 *           Some statistics utilities 
 ****************************************************/
extern void lsq_y_ax(int n, real x[], real y[], real *a);
/* Fit a straight line y=ax thru the n data points x,y. */

extern real lsq_y_ax_b(int n, real x[], real y[], real *a, real *b);
/* Fit a straight line y=ax+b thru the n data points x,y.
 * Returns the "fit quality" sigma = sqrt(chi^2/(n-2)).
 */

#ifdef CPLUSPLUS
}
#endif

#endif	/* _xvgr_h */
