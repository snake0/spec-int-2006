/*
 * $Id: xvgr.c,v 1.26 2002/02/28 10:49:35 spoel Exp $
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
static char *SRCID_xvgr_c = "$Id: xvgr.c,v 1.26 2002/02/28 10:49:35 spoel Exp $";
#include <string.h>
#include <ctype.h>
#include "sysstuff.h"
#include "string2.h"
#include "futil.h"
#include "statutil.h"
#include "copyrite.h"
#include "smalloc.h"
#include "xvgr.h"
#include "viewit.h"
#include "vec.h"

static bool use_xmgr()
{
  char *env;
  bool bXMGR;
#ifdef SPEC_CPU
  env = NULL;
#else
  env = getenv("GMX_VIEW_XVG");
#endif
  return (env!=NULL && strcmp(env,"xmgr")==0);
} 

FILE *xvgropen(char *fn,char *title,char *xaxis,char *yaxis)
{
  FILE *xvgr;
  time_t t;
  
  xvgr=(FILE *)ffopen(fn,"w");
  fprintf(xvgr,"# This file was created by %s\n",Program());
  fprintf(xvgr,"# which is part of G R O M A C S:\n");
  fprintf(xvgr,"# %s\n",bromacs());
  time(&t);
  fprintf(xvgr,"# All this happened at: %s",ctime(&t));
  fprintf(xvgr,"#\n");
  fprintf(xvgr,"@    title \"%s\"\n",title);
  fprintf(xvgr,"@    xaxis  label \"%s\"\n",xaxis);
  fprintf(xvgr,"@    yaxis  label \"%s\"\n",yaxis);
  if (use_xmgr())
    fprintf(xvgr,"@TYPE nxy\n");
  else
    fprintf(xvgr,"@TYPE xy\n");
  
  return xvgr;
}

void xvgr_view(FILE *out,real xmin,real ymin,real xmax,real ymax)
{
  fprintf(out,"@ view %g, %g, %g, %g\n",xmin,ymin,xmax,ymax);
}

void xvgr_world(FILE *out,real xmin,real ymin,real xmax,real ymax)
{
  fprintf(out,"@ world xmin %g\n"
	  "@ world ymin %g\n"
	  "@ world xmax %g\n"
	  "@ world ymax %g\n",xmin,ymin,xmax,ymax);
}

void xvgr_legend(FILE *out,int nsets,char *setname[])
{
  int i;
  
  xvgr_view(out,0.15,0.15,0.75,0.85);
  fprintf(out,"@ legend on\n");
  fprintf(out,"@ legend box on\n");
  fprintf(out,"@ legend loctype view\n");
  fprintf(out,"@ legend %g, %g\n",0.78,0.8);
  fprintf(out,"@ legend length %d\n",2);
  for(i=0; (i<nsets); i++)
    if (setname[i]) {
      if (use_xmgr())
	fprintf(out,"@ legend string %d \"%s\"\n",i,setname[i]);
      else
	fprintf(out,"@ s%d legend \"%s\"\n",i,setname[i]);
    }
}

void xvgr_line_props(FILE *out, int NrSet, int LineStyle, int LineColor)
{
  fprintf(out, "@    with g0\n");
  fprintf(out, "@    s%d linestyle %d\n", NrSet, LineStyle);
  fprintf(out, "@    s%d color %d\n", NrSet, LineColor);
}

static char *LocTypeStr[] = { "view", "world" };
static char *BoxFillStr[] = { "none", "color", "pattern" };
 
void xvgr_box(FILE *out,
	      int LocType,
	      real xmin,real ymin,real xmax,real ymax,
	      int LineStyle,int LineWidth,int LineColor,
	      int BoxFill,int BoxColor,int BoxPattern)
{
  fprintf(out,"@with box\n");
  fprintf(out,"@    box on\n");
  fprintf(out,"@    box loctype %s\n",LocTypeStr[LocType]);
  fprintf(out,"@    box %g, %g, %g, %g\n",xmin,ymin,xmax,ymax);
  fprintf(out,"@    box linestyle %d\n",LineStyle);
  fprintf(out,"@    box linewidth %d\n",LineWidth);
  fprintf(out,"@    box color %d\n",LineColor);
  fprintf(out,"@    box fill %s\n",BoxFillStr[BoxFill]);
  fprintf(out,"@    box fill color %d\n",BoxColor);
  fprintf(out,"@    box fill pattern %d\n",BoxPattern);
  fprintf(out,"@box def\n");
}

void lsq_y_ax(int n, real x[], real y[], real *a)
{
  int    i;
  double xx,yx;

  yx=xx=0.0;
  for (i=0; i<n; i++) {
    yx+=y[i]*x[i];
    xx+=x[i]*x[i];
  }
  *a=yx/xx;
}

real lsq_y_ax_b(int n, real x[], real y[], real *a, real *b)
{
  int    i;
  double yx,xx,sx,sy,chi2;

  yx=xx=sx=sy=0.0;
  for (i=0; i<n; i++) {
    yx+=y[i]*x[i];
    xx+=x[i]*x[i];
    sx+=x[i];
    sy+=y[i];
  }
  *a=(n*yx-sy*sx)/(n*xx-sx*sx);
  *b=(sy-(*a)*sx)/n;

  chi2=0;
  for(i=0; i<n; i++)
    chi2+=sqr(y[i]-((*a)*x[i]+(*b)));
  
  if (n > 2)
    return sqrt(chi2/(n-2));
  else
    return 0;
}

static char *fgets3(FILE *fp)
{
  static char *ptr = NULL;
  static int  len  = STRLEN;
  char *p;
  int  slen;
  
  if (ptr==NULL)
    snew(ptr,len);

  if (fgets(ptr,len-1,fp) == NULL)
    return NULL;
  p = ptr;
  while ((strchr(ptr,'\n') == NULL) && (!feof(fp))) {
    /* This line is longer than len characters, let's increase len! */
    len += STRLEN;
    p   += STRLEN;
    srenew(ptr,len);
    if (fgets(p-1,STRLEN,fp) == NULL)
      break;
  }
  slen = strlen(ptr);
  if (ptr[slen-1] == '\n')
    ptr[slen-1] = '\0';
  return ptr;
}

static int wordcount(char *ptr)
{
  int i,n,is[2];
  int cur=0;
#define prev (1-cur)
  
  if (strlen(ptr) == 0)
    return 0;
  /* fprintf(stderr,"ptr='%s'\n",ptr); */
  n=1;
  for(i=0; (ptr[i] != '\0'); i++) {
    is[cur] = isspace(ptr[i]);
    if ((i > 0)  && (is[cur] && !is[prev]))
      n++;
    cur=prev;
  }
  return n;
}

int read_xvg(char *fn,real ***y,int *ny)
{
  FILE   *fp;
  char   *ptr;
  char   *base=NULL;
  char   *fmt=NULL;
  int    k,line=0,nny,nx,maxx,rval;
  double lf;
  real   **yy=NULL;
  
  *ny  = 0;
  nny  = 0;
  nx   = 0;
  maxx = 0;
  fp   = ffopen(fn,"r");
  while ((ptr = fgets3(fp)) != NULL) {
    line++;
    trim(ptr);
    if ((ptr[0] != '@') && (ptr[0] != '#')) {
      if (nny == 0) {
	(*ny) = nny = wordcount(ptr);
	/* fprintf(stderr,"There are %d columns in your file\n",nny);*/
	if (nny == 0)
	  return 0;
	snew(yy,nny);
	snew(fmt,3*nny+1);
	snew(base,3*nny+1);
      }
      /* Allocate column space */
      if (nx >= maxx) {
	maxx+=1024;
	for(k=0; (k<nny); k++)
	  srenew(yy[k],maxx);
      }
      /* Initiate format string */
      fmt[0]  = '\0';
      base[0] = '\0';
      
      /* fprintf(stderr,"ptr='%s'\n",ptr);*/
      for(k=0; (k<nny); k++) {
	strcpy(fmt,base);
	strcat(fmt,"%lf");
	rval = sscanf(ptr,fmt,&lf);
	/* fprintf(stderr,"rval = %d\n",rval);*/
	if ((rval == EOF) || (rval == 0))
	  break;
	yy[k][nx] = lf;
	srenew(fmt,3*(nny+1)+1);
	srenew(base,3*nny+1);
	strcat(base,"%*s");
      }
      if (k != nny) {
	fprintf(stderr,"Only %d columns on line %d in file %s\n",
		k,line,fn);
	for( ; (k<nny); k++)
	  yy[k][nx] = 0.0;
      }
      nx++;
    }
  }
  ffclose(fp);
  
  *y = yy;
  
  return nx;
}

void write_xvg(char *fn,char *title,int nx,int ny,real **y,char **leg)
{
  FILE *fp;
  int  i,j;
  
  fp=xvgropen(fn,title,"X","Y");
  if (leg)
    xvgr_legend(fp,ny-1,leg);
  for(i=0; (i<nx); i++) {
    for(j=0; (j<ny); j++) {
      fprintf(fp,"  %12.5e",y[j][i]);
    }
    fprintf(fp,"\n");
  }
  fclose(fp);
}

