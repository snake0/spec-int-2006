/*
 * $Id: symtab.c,v 1.8 2002/02/28 10:49:30 spoel Exp $
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
static char *SRCID_symtab_c = "$Id: symtab.c,v 1.8 2002/02/28 10:49:30 spoel Exp $";
#include <stdio.h>
#include <string.h>
#include "sysstuff.h"
#include "string2.h"
#include "assert.h"
#include "typedefs.h"
#include "fatal.h"
#include "smalloc.h"
#include "txtdump.h"
#include "symtab.h"
#include "macros.h"

#define	BUFSIZE			1024
#define	TABLESIZE		5

static char *trim_string(char *s)
     /*
      * Returns a pointer to a static area which contains a copy 
      * of s without leading or trailing spaces. Strings are
      * truncated to BUFSIZE positions.
      */      
{
  static char buf[BUFSIZE];
  int len,i;
  
  for (; (*s)&&((*s)==' '); s++);
  for (len=strlen(s); (len>0); len--) if (s[len-1]!=' ') break;
  if (len>=BUFSIZE) len=BUFSIZE-1;
  for (i=0; i<len; i++) buf[i]=*(s++);
  buf[i]=0;
  return buf;
}

int lookup_symtab(t_symtab *symtab,char **name)
{
  int base,index;
  t_symbuf *symbuf;
  
  base=0;
  index=0;
  symbuf=symtab->symbuf;
  while (symbuf!=NULL) {
    index=name-symbuf->buf;
    if ( ( index >= 0 ) && ( index < symbuf->bufsize ) )
      return index+base;
    else {
      base+=symbuf->bufsize;
      symbuf=symbuf->next;
    }
  }
  fatal_error(0,"symtab lookup \"%s\" not found",*name);
  return -1;
}

char **get_symtab_handle(t_symtab *symtab,int name)
{
  t_symbuf *symbuf;
  
  symbuf=symtab->symbuf;
  while (symbuf!=NULL) {
    if (name<symbuf->bufsize)
      return &(symbuf->buf[name]);
    else {
      name-=symbuf->bufsize;
      symbuf=symbuf->next;
    }
  }
  fatal_error(0,"symtab get_symtab_handle %d not found",name);
  return NULL;
}

static t_symbuf *new_symbuf(void)
{
  t_symbuf *symbuf;

  snew(symbuf,1);
  symbuf->bufsize=TABLESIZE;
  snew(symbuf->buf,symbuf->bufsize);
  symbuf->next=NULL;

  return symbuf;
}

static char **enter_buf(t_symtab *symtab,char *name)
{
  int      i;
  t_symbuf *symbuf;
  bool     bCont;
  
  if (symtab->symbuf == NULL)
    symtab->symbuf=new_symbuf();

  symbuf=symtab->symbuf;
  do {
    for(i=0; (i < symbuf->bufsize); i++) {
      if (symbuf->buf[i]==NULL) {
	symtab->nr++;
	symbuf->buf[i]=strdup(name);
	return &(symbuf->buf[i]);
      } else if (strcmp(symbuf->buf[i],name)==0)
	return &(symbuf->buf[i]);
    }
    if (symbuf->next != NULL) {
      symbuf=symbuf->next;
      bCont = TRUE;
    }
    else
      bCont = FALSE;
  } while (bCont);

  symbuf->next=new_symbuf();
  symbuf=symbuf->next;

  symtab->nr++;
  symbuf->buf[0]=strdup(name);
  return &(symbuf->buf[0]);
}

char **put_symtab(t_symtab *symtab,char *name)
{
  return enter_buf(symtab,trim_string(name));
}

void open_symtab(t_symtab *symtab)
{
  symtab->nr=0;
  symtab->symbuf=NULL;
}

void close_symtab(t_symtab *symtab)
{
}

void done_symtab(t_symtab *symtab)
{
  int i;
  t_symbuf *symbuf,*freeptr;
  
  close_symtab(symtab);
  symbuf=symtab->symbuf;
  while (symbuf!=NULL) {
    for (i=0; (i < symbuf->bufsize) && (i < symtab->nr); i++)
      sfree(symbuf->buf[i]);
    symtab->nr-=i;
    sfree(symbuf->buf);
    freeptr=symbuf;
    symbuf=symbuf->next;
    sfree(freeptr);
  }
  symtab->symbuf=NULL;
  assert(symtab->nr==0);
}

void free_symtab(t_symtab *symtab)
{
  t_symbuf *symbuf,*freeptr;
  
  close_symtab(symtab);
  symbuf=symtab->symbuf;
  while (symbuf!=NULL) {
    symtab->nr-=min(symbuf->bufsize,symtab->nr);
    freeptr=symbuf;
    symbuf=symbuf->next;
    sfree(freeptr);
  }
  symtab->symbuf=NULL;
  assert(symtab->nr==0);
}

void pr_symtab(FILE *fp,int indent,char *title,t_symtab *symtab)
{
  int i,j,nr;
  t_symbuf *symbuf;
  
  if (available(fp,symtab,title))
    {
      indent=pr_title_n(fp,indent,title,symtab->nr);
      i=0;
      nr=symtab->nr;
      symbuf=symtab->symbuf;
      while (symbuf!=NULL)
        {
          for (j=0; (j < symbuf->bufsize) && (j < nr); j++)
            {
              if(fp)
              {
                  pr_indent(fp,indent);
                  fprintf(fp,"%s[%d]=\"%s\"\n",title,i++,symbuf->buf[j]);
              }
            }
          nr-=j;
          symbuf=symbuf->next;
        }
      assert(nr==0);
    }
}
