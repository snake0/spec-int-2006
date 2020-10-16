/*
 * $Id: strdb.c,v 1.10 2002/02/28 10:49:30 spoel Exp $
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
static char *SRCID_strdb_c = "$Id: strdb.c,v 1.10 2002/02/28 10:49:30 spoel Exp $";
#include <stdio.h>
#include <stdlib.h>
#include "string2.h"
#include "futil.h"
#include "smalloc.h"
#include "fatal.h"
#include "strdb.h"

bool get_a_line(FILE *fp,char line[],int n)
{
  static char *line0=NULL;
  static int nalloc=0;
  char *dum;
  
  if (n>nalloc) {
    nalloc=n;
    srenew(line0,nalloc+1);
  }
  do {
    if (!fgets(line0,n+1,fp)) {
      return FALSE;
    }
    dum=strchr(line0,'\n');
    if (dum) 
      dum[0]='\0';
    else if (strlen(line0)==n) {
      fprintf(stderr,"Warning: line length exceeds buffer length (%d), data might be corrupted\n",n);
      line0[n-1] ='\0';
    } else
      fprintf(stderr,"Warning: file does not end with a newline, last line:\n%s\n",
	      line0);
    dum=strchr(line0,';');
    if (dum) 
      dum[0]='\0';
    strcpy(line,line0);
    dum=line0;
    ltrim(dum);
  } while (dum[0] == '\0'); 

  return TRUE;
}

bool get_header(char line[],char *header)
{
  char temp[STRLEN],*dum;

  strcpy(temp,line);
  dum=strchr(temp,'[');
  if (dum==NULL)
    return FALSE;
  dum[0]=' ';
  dum=strchr(temp,']');
  if (dum==NULL) {
    fatal_error(0,"header is not terminated on line:\n'%s'\n",line); 
    return FALSE;
  }
  dum[0]='\0';
  if (sscanf(temp,"%s%*s",header) != 1)
    return FALSE;

  return TRUE;
}

int get_strings(char *db,char ***strings)
{
  FILE *in;
  char **ptr;
  char buf[256];
  int  i,nstr;

  in=libopen(db);
  
  set_warning_line(db,1);
  if (fscanf(in,"%d",&nstr) != 1) {
    sprintf(warn_buf,"File %s is empty",db);
    warning(NULL);
    fclose(in);
    return 0;
  }
  snew(ptr,nstr);
  for(i=0; (i<nstr); i++) {
    fscanf(in,"%s",buf);
#ifdef DEBUG
    fprintf(stderr,"Have read: %s\n",buf);
#endif
    ptr[i] = strdup(buf);
  }
  fclose(in);

  *strings=ptr;
  
  return nstr;
}

int search_str(int nstr,char **str,char *key)
{
  int i;

  /* Linear search */
  for(i=0; (i<nstr); i++)
    if (strcasecmp(str[i],key)==0)
      return i;

  return -1;
}

int fget_lines(FILE *in,char ***strings)
{
  char **ptr;
  char buf[256];
  int  i,nstr;
  
  fgets(buf,255,in);  
  if (sscanf(buf,"%d",&nstr) != 1) {
    sprintf(warn_buf,"File is empty");
    warning(NULL);
    fclose(in);
    
    return 0;
  }
  snew(ptr,nstr);
  for(i=0; (i<nstr); i++) {
    fgets2(buf,255,in);
    ptr[i] = strdup(buf);
  }
  
  (*strings) = ptr;
  
  return nstr;
}

int get_lines(char *db,char ***strings)
{
  FILE *in;
  int  nstr;
  
  set_warning_line(db,1);
  in   = libopen(db);
  nstr = fget_lines(in,strings);
  fclose(in);

  return nstr;
}

int get_file(char *db,char ***strings)
{
  FILE *in;
  char **ptr=NULL;
  char buf[256];
  int  i,nstr,maxi;

  in=libopen(db);
  
  i=maxi=0;
  while (fgets2(buf,255,in)) {
    if (i>=maxi) {
      maxi+=50;
      srenew(ptr,maxi);
    }
    ptr[i] = strdup(buf);
    i++;
  }
  nstr=i;
  fclose(in);
  srenew(ptr,nstr);
  *strings=ptr;
  
  return nstr;
}

