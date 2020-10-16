/*
 * $Id: writeps.h,v 1.10 2002/02/28 21:55:52 spoel Exp $
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

#ifndef _writeps_h
#define _writeps_h

static char *SRCID_writeps_h = "$Id: writeps.h,v 1.10 2002/02/28 21:55:52 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include "typedefs.h"

typedef enum { 
  eXCenter, eXLeft, eXRight
} eXPos;

typedef enum { 
  eYCenter, eYTop,  eYBottom
} eYPos; 

enum { 
  efontTIMES, efontTIMESITALIC, efontTIMESBOLD, efontTIMESBOLDITALIC,
  efontHELV,  efontHELVITALIC,  efontHELVBOLD,  efontHELVBOLDITALIC,
  efontCOUR,  efontCOURITALIC,  efontCOURBOLD,  efontCOURBOLDITALIC,
  efontNR };

extern char *fontnm[efontNR];

extern FILE *ps_open(char *fn,real x1,real y1,real x2,real y2);

extern void ps_linewidth(FILE *ps,int lw);
extern void ps_color(FILE *ps,real r,real g,real b);
extern void ps_rgb(FILE *ps,t_rgb *rgb);

extern void ps_rgb_box(FILE *ps,t_rgb *rgb);
extern void ps_rgb_nbox(FILE *ps,t_rgb *rgb,real n);
extern void ps_init_rgb_box(FILE *ps,real xbox, real ybox);
extern void ps_init_rgb_nbox(FILE *ps,real xbox, real ybox);

extern void ps_lineto(FILE *ps,real x,real y);
extern void ps_linerel(FILE *ps,real dx,real dy);

extern void ps_moveto(FILE *ps,real x,real y);
extern void ps_moverel(FILE *ps,real dx,real dy);

extern void ps_line(FILE *ps,real x1,real y1,real x2,real y2);

extern void ps_box(FILE *ps,real x1,real y1,real x2,real y2);
extern void ps_fillbox(FILE *ps,real x1,real y1,real x2,real y2);

extern void ps_arc(FILE *ps,real x1,real y1,real rad,real a0,real a1);
extern void ps_fillarc(FILE *ps,real x1,real y1,real rad,real a0,real a1);
extern void ps_arcslice(FILE *ps,real xc,real yc,
			real rad1,real rad2,real a0,real a1);
extern void ps_fillarcslice(FILE *ps,real xc,real yc,
			    real rad1,real rad2,real a0,real a1);

extern void ps_circle(FILE *ps,real x1,real y1,real rad);

extern void ps_font(FILE *ps,int font,real size);
extern void ps_strfont(FILE *ps,char *font,real size);

extern void ps_text(FILE *ps,real x1,real y1,char *str);
extern void ps_ctext(FILE *ps,real x1,real y1,char *str,int expos);

extern void ps_close(FILE *ps);

extern void ps_rotate(FILE *ps,bool bPlus);
/* Rotate over 90 (bPlus) or -90 (!bPlus) degrees */
extern void ps_translate(FILE *ps,real x,real y);

extern void ps_setorigin(FILE *ps);
extern void ps_unsetorigin(FILE *ps);

extern void viewps(char *fn);

extern void ps_comment(FILE *ps,char *s);

#endif	/* _writeps_h */
