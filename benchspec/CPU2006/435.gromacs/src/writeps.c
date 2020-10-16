/*
 * $Id: writeps.c,v 1.12 2002/02/28 10:49:31 spoel Exp $
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
static char *SRCID_writeps_c = "$Id: writeps.c,v 1.12 2002/02/28 10:49:31 spoel Exp $";
#include <stdio.h>
#include <math.h>

#include "futil.h"
#include "fatal.h"
#include "copyrite.h"
#include "writeps.h"
#include "smalloc.h"

static int maxrgb=0;
static int   nrgb=0;
static t_rgb *rgb=NULL;

FILE *ps_open(char *fn,real x1,real y1,real x2,real y2)
{
  FILE *ps;
  
  ps=ffopen(fn,"w");
  fprintf(ps,"%%!PS-Adobe-2.0 EPSF-1.2\n");
  fprintf(ps,"%%%%Creator: GROMACS\n");
  fprintf(ps,"%%%%Title: %s\n",fn);
  fprintf(ps,"%%%%BoundingBox: %g %g %g %g\n",x1,y1,x2,y2);
  fprintf(ps,"%%%%EndComments\n");
  fprintf(ps,"/m {moveto} bind def\n");
  fprintf(ps,"/l {lineto} bind def\n");
  fprintf(ps,"/rm {rmoveto} bind def\n");
  fprintf(ps,"/r  {rlineto} bind def\n");
  fprintf(ps,"/f {fill} bind def\n");
  fprintf(ps,"/s {stroke} bind def\n");

  if (nrgb > 0) {
    fprintf(stderr,"Warning: resetting color table in %s when opening %s\n",
	    __FILE__,fn);
    nrgb=0;
  }
      
  return ps;
}

void ps_linewidth(FILE *ps, int lw)
{
  fprintf(ps,"%d setlinewidth\n",lw);
}

static void ps_defcolor(FILE *ps,real r,real g,real b,char *cname)
{
  fprintf(ps,"/%s {%g %g %g setrgbcolor} bind def\n",cname,r,g,b);
}

static void ps_selcolor(FILE *ps,char *cname)
{
  fprintf(ps,"%s\n",cname);
}

static char *i2a(int i)
{
  static char buf[12];
  
  sprintf(buf,"C%d",i);
  
  return buf;
}

static int search_col(FILE *ps,real r,real g,real b)
{
  int  i;
  
  for(i=0; (i<nrgb); i++)
    if ( (fabs(rgb[i].r-r) < GMX_REAL_EPS) &&
	 (fabs(rgb[i].g-g) < GMX_REAL_EPS) &&
	 (fabs(rgb[i].b-b) < GMX_REAL_EPS))
      return i;
  
  if (nrgb >= maxrgb) 
    {
      maxrgb+=100;
      srenew(rgb,maxrgb);
    }
  
  ps_defcolor(ps,r,g,b,i2a(nrgb));
  fprintf(ps,"/B%d {%s b} bind def\n",nrgb,i2a(nrgb));
  rgb[i].r=r;
  rgb[i].g=g;
  rgb[i].b=b;
  nrgb++;
  
  return nrgb-1;
}

void ps_color(FILE *ps,real r,real g,real b)
{
  ps_selcolor(ps,i2a(search_col(ps,r,g,b)));
}

void ps_rgb(FILE *ps,t_rgb *rgb)
{
  ps_color(ps,rgb->r,rgb->g,rgb->b);
}

static real gen_ybox=0;

void ps_init_rgb_nbox(FILE *ps,real xbox, real ybox)
{
  gen_ybox=ybox;
  fprintf(ps,"/by {def currentpoint "
	  "%g y r %g %g r %g y neg r %g %g r f y add moveto} bind def\n",
	  0.0,xbox,0.0,0.0,-xbox,0.0);
  /* macro bn is used in ps_rgb_nbox to draw rectangular boxes */
}

void ps_rgb_nbox(FILE *ps,t_rgb *rgb,real n)
{
  int i;
  
  if ( n>2 ) {
    ps_rgb(ps,rgb);
    fprintf(ps,"/y %g by\n",n*gen_ybox);
    /* macro by is defined in ps_init_rgb_nbox */
  } else
    for (i=0; (i<n); i++)
      ps_rgb_box(ps, rgb);
  
}

void ps_init_rgb_box(FILE *ps,real xbox, real ybox)
{
  fprintf(ps,"/b {currentpoint "
	  "%g %g r %g %g r %g %g r %g %g r f %g add moveto} bind def\n",
	  0.0,ybox,xbox,0.0,0.0,-ybox,-xbox,0.0,ybox);
  /* macro b is used in search_col to define macro B */
}

void ps_rgb_box(FILE *ps,t_rgb *rgb)
{
  fprintf(ps,"B%d\n",search_col(ps,rgb->r,rgb->g,rgb->b));
  /* macro B is defined in search_col from macro b */
}

void ps_lineto(FILE *ps,real x,real y)
{
  fprintf(ps,"%g %g l\n",x,y);
}

void ps_linerel(FILE *ps,real dx,real dy)
{
  fprintf(ps,"%g %g r\n",dx,dy);
}

void ps_moveto(FILE *ps,real x,real y)
{
  fprintf(ps,"%g %g m\n",x,y);
}

void ps_moverel(FILE *ps,real dx,real dy)
{
  fprintf(ps,"%g %g rm\n",dx,dy);
}

void ps_line(FILE *ps,real x1,real y1,real x2,real y2)
{
  ps_moveto(ps,x1,y1);
  ps_lineto(ps,x2,y2);
  fprintf(ps,"s\n");
}

static void do_box(FILE *ps,real x1,real y1,real x2,real y2)
{
  ps_moveto(ps,x1,y1);
  ps_linerel(ps,0,(real)(y2-y1));
  ps_linerel(ps,(real)(x2-x1),0);
  ps_linerel(ps,0,(real)(y1-y2));
  ps_linerel(ps,(real)(x1-x2),0);
}

void ps_box(FILE *ps,real x1,real y1,real x2,real y2)
{
  do_box(ps,x1,y1,x2,y2);
  fprintf(ps,"s\n");
}

void ps_fillbox(FILE *ps,real x1,real y1,real x2,real y2)
{
  do_box(ps,x1,y1,x2,y2);
  fprintf(ps,"f\n");
}

void ps_arc(FILE *ps,real x1,real y1,real rad,real a0,real a1)
{
  fprintf(ps,"%g %g %g %g %g arc s\n",x1,y1,rad,a0,a1);
}

void ps_fillarc(FILE *ps,real x1,real y1,real rad,real a0,real a1)
{
  fprintf(ps,"%g %g %g %g %g arc f\n",x1,y1,rad,a0,a1);
}

void ps_arcslice(FILE *ps,real xc,real yc,
		 real rad1,real rad2,real a0,real a1)
{
  fprintf(ps,"newpath %g %g %g %g %g arc %g %g %g %g %g arcn closepath s\n",
	  xc,yc,rad1,a0,a1,xc,yc,rad2,a1,a0);
}
  
void ps_fillarcslice(FILE *ps,real xc,real yc,
		     real rad1,real rad2,real a0,real a1)
{
  fprintf(ps,"newpath %g %g %g %g %g arc %g %g %g %g %g arcn closepath f\n",
	  xc,yc,rad1,a0,a1,xc,yc,rad2,a1,a0);
}
  
void ps_circle(FILE *ps,real x1,real y1,real rad)
{
  ps_arc(ps,x1,y1,rad,0,360);
}

char *fontnm[efontNR] = { 
  "Times-Roman","Times-Italic",     "Times-Bold",    "Times-BoldItalic",
  "Helvetica",  "Helvetica-Oblique","Helvetica-Bold","Helvetica-BoldOblique",
  "Courier",    "Courier-Oblique",  "Courier-Bold",  "Courier-BoldOblique"
};

void ps_font(FILE *ps,int font,real size)
{
  
  if ((font < 0) || (font > efontNR)) {
    fprintf(stderr,"Invalid Font: %d, using %s\n",font,fontnm[0]);
    font=0;
  }
  fprintf(ps,"/%s findfont\n",fontnm[font]);
  fprintf(ps,"%g scalefont setfont\n",size);
}

void ps_strfont(FILE *ps,char *font,real size)
{
  fprintf(ps,"/%s findfont\n",font);
  fprintf(ps,"%g scalefont setfont\n",size);
}

void ps_text(FILE *ps,real x1,real y1,char *str)
{
  ps_moveto(ps,x1,y1);
  fprintf(ps,"(%s) show\n",str);
}

void ps_rotate(FILE *ps,bool bPlus)
{
  if (bPlus) 
    fprintf(ps,"612.5 0 translate 90 rotate\n");
  else
    fprintf(ps,"-90 rotate -612.5 0 translate\n");
}

void ps_ctext(FILE *ps,real x1,real y1,char *str,int expos)
{
  if (expos == eXLeft) {
    ps_text(ps,x1,y1,str);
    return;
  }
  ps_moveto(ps,x1,y1);
  fprintf(ps,"(%s) stringwidth\n",str);
  switch (expos) {
  case eXLeft:
    fprintf(ps,"exch 0 exch pop exch\n");
    break;
  case eXCenter:
    fprintf(ps,"exch 2 div neg exch\n");
    break;
  case eXRight:
    fprintf(ps,"exch neg exch\n");
    break;
  default:
    fatal_error(0,"invalid position index (expos=%d)",expos);
  }
  fprintf(ps,"rmoveto (%s) show\n",str);
}

void ps_translate(FILE *ps,real x,real y)
{
  fprintf(ps,"%g %g translate\n",x,y);
}

static int ostack=0;

void ps_setorigin(FILE *ps)
{
  fprintf(ps,"currentpoint dup 3 -1 roll dup 4 1 roll exch translate\n");
  ostack++;
}

void ps_unsetorigin(FILE *ps)
{
  if (ostack <= 0)
    fatal_error(0,"No origin on stack!\n");
  fprintf(ps,"neg exch neg exch translate\n");
  ostack--;
}

void ps_close(FILE *ps)
{
  fprintf(ps,"%%showpage\n");
  fprintf(ps,"%%%%EOF\n");
  fclose(ps);
}

void ps_comment(FILE *ps,char *s)
{
  fprintf(ps,"%%%% %s\n",s);
}
