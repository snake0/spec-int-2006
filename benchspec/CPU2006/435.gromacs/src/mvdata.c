/*
 * $Id: mvdata.c,v 1.10 2002/02/28 10:49:28 spoel Exp $
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
 * Gyas ROwers Mature At Cryogenic Speed
 */
static char *SRCID_mvdata_c = "$Id: mvdata.c,v 1.10 2002/02/28 10:49:28 spoel Exp $";
#include <sysstuff.h>
#include <string.h>
#include "typedefs.h"
#include "main.h"
#include "assert.h"
#include "mvdata.h"
#include "network.h"
#include "smalloc.h"
#include "fatal.h"
#include "symtab.h"
#include "vec.h"
#include "tgroup.h"
#include "block_tx.h"

static void ld_nsb(int src,t_nsborder *nsb)
{
  blockrx(src,nsb->nnodes);
  blockrx(src,nsb->shift);
  blockrx(src,nsb->bshift);
  blockrx(src,nsb->nstDlb);
  blockrx(src,nsb->cgtotal);
  blockrx(src,nsb->natoms);
  nblockrx(src,MAXNODES,nsb->homenr);
  nblockrx(src,MAXNODES,nsb->index);
  nblockrx(src,MAXNODES,nsb->cgload);
  nblockrx(src,MAXNODES,nsb->workload);
}

static char **ld_string(int src,t_symtab *symtab)
{
  int name;
  
  blockrx(src,name);
  return get_symtab_handle(symtab,name);
}

static int ld_strings(int src,t_symtab *symtab,char ****nm)
{
  int  i,nr;
  int  *handle;
  char ***NM;

  blockrx(src,nr);
  snew(handle,nr);
  nblockrx(src,nr,handle);

  snew(*nm,nr);
  NM=*nm;
  for (i=0; (i<nr); i++) 
    NM[i]=get_symtab_handle(symtab,handle[i]);
  sfree(handle);

  return nr;
}

static void ld_symtab(int src,t_symtab *symtab)
{
  int i,nr,len;
  
  blockrx(src,symtab->nr);
  nr=symtab->nr;
  snew(symtab->symbuf,1);
  symtab->symbuf->bufsize=nr;
  snew(symtab->symbuf->buf,nr);
  for (i=0; i<nr; i++)
    {
      blockrx(src,len);
      snew(symtab->symbuf->buf[i],len);
      nblockrx(src,len,symtab->symbuf->buf[i]);
    }
}

static void ld_grps(int src,t_grps grps[])
{
  int i;
  
  for(i=0; (i<egcNR); i++) {
    blockrx(src,grps[i].nr);
    snew(grps[i].nm_ind,grps[i].nr);
    nblockrx(src,grps[i].nr,grps[i].nm_ind);
  }
  for( ; (i<egcNR); i++) {
    grps[i].nr=0;
    grps[i].nm_ind=NULL;
  }
}

static void ld_atoms(int src,t_symtab *symtab,t_atoms *atoms)
{
  int atomnr;

  blockrx(src,atoms->nr);
  snew(atoms->atom,atoms->nr);
  nblockrx(src,atoms->nr,atoms->atom);
  atomnr=ld_strings(src,symtab,&atoms->atomname);
  assert(atomnr==atoms->nr);
  atoms->nres=ld_strings(src,symtab,&atoms->resname);
  atoms->ngrpname=ld_strings(src,symtab,&atoms->grpname);
  ld_grps(src,atoms->grps);
  ld_block(src,&atoms->excl);
}

static void ld_vectors(int src,rvec x[],rvec v[])
{
  int natoms;
  
  blockrx(src,natoms);
  nblockrx(src,natoms,x);
  nblockrx(src,natoms,v);
}

static void ld_ilist(int src,t_ilist *ilist)
{
  blockrx(src,ilist->nr);
  nblockrx(src,MAXNODES,ilist->multinr);
  snew(ilist->iatoms,ilist->nr);
  nblockrx(src,ilist->nr,ilist->iatoms);
}

static void ld_idef(int src,t_idef *idef)
{
  int i;
  
  blockrx(src,idef->ntypes);
  blockrx(src,idef->atnr);
  snew(idef->functype,idef->ntypes);
  snew(idef->iparams,idef->ntypes);
  nblockrx(src,idef->ntypes,idef->functype);
  nblockrx(src,idef->ntypes,idef->iparams);
  for(i=0; (i<F_NRE); i++)
    ld_ilist(src,&idef->il[i]);
}

static void ld_grpopts(int src,t_grpopts *g)
{
  blockrx(src,g->ngtc);
  blockrx(src,g->ngacc);
  blockrx(src,g->ngfrz);
  blockrx(src,g->ngener);
  snew(g->nrdf,g->ngtc);
  snew(g->tau_t,g->ngtc);
  snew(g->ref_t,g->ngtc);
  snew(g->acc,g->ngacc);
  snew(g->nFreeze,g->ngfrz);
  snew(g->eg_excl,g->ngener*g->ngener);
  nblockrx(src,g->ngtc,g->nrdf);
  nblockrx(src,g->ngtc,g->tau_t);
  nblockrx(src,g->ngtc,g->ref_t);
  nblockrx(src,g->ngacc,g->acc);
  nblockrx(src,g->ngfrz,g->nFreeze);
  nblockrx(src,g->ngener*g->ngener,g->eg_excl);
}

static void ld_cosines(int src,t_cosines *cs)
{
  blockrx(src,cs->n);
  snew(cs->a,cs->n);
  snew(cs->phi,cs->n);
  if (cs->n > 0) {
    nblockrx(src,cs->n,cs->a);
    nblockrx(src,cs->n,cs->phi);
  }
}

static void ld_parm(int src,t_parm *parm)
{
  int i;
  
  blockrx(src,*parm);
  ld_grpopts(src,&(parm->ir.opts));
  for(i=0; (i<DIM); i++) {
    ld_cosines(src,&(parm->ir.ex[i]));
    ld_cosines(src,&(parm->ir.et[i]));
  }
}

void ld_data(int left,int right,t_parm *parm,t_nsborder *nsb,
	     t_topology *top,rvec **x,rvec **v)
{
  int i;
  
  ld_parm(left,parm);
  if (debug) fprintf(stdlog,"after ld_parm");
  ld_nsb(left,nsb);
  if (debug) fprintf(stdlog,"after ld_nsb");
  ld_symtab(left,&top->symtab);
  if (debug) fprintf(stdlog,"after ld_symtab");
  top->name=ld_string(left,&top->symtab);
  if (debug) fprintf(stdlog,"after ld_name");
  ld_atoms(left,&top->symtab,&top->atoms);
  if (debug) fprintf(stdlog,"after ld_atoms");
  ld_idef(left,&top->idef);
  if (debug) fprintf(stdlog,"after ld_idef");
  for (i=0; (i<ebNR); i++) 
    ld_block(left,&top->blocks[i]);
  if (debug) fprintf(stdlog,"after ld_block");
  snew(*x,top->atoms.nr);
  snew(*v,top->atoms.nr);
  ld_vectors(left,*x,*v);
  if (debug) fprintf(stdlog,"after ld_vectors");
}

static void mv_grpopts(int dest,t_grpopts *g)
{
  blocktx(dest,g->ngtc);
  blocktx(dest,g->ngacc);
  blocktx(dest,g->ngfrz);
  blocktx(dest,g->ngener);
  nblocktx(dest,g->ngtc,g->nrdf);
  nblocktx(dest,g->ngtc,g->tau_t);
  nblocktx(dest,g->ngtc,g->ref_t);
  nblocktx(dest,g->ngacc,g->acc);
  nblocktx(dest,g->ngfrz,g->nFreeze);
  nblocktx(dest,g->ngener*g->ngener,g->eg_excl);
}

static void mv_cosines(int dest,t_cosines *cs)
{
  blocktx(dest,cs->n);
  if (cs->n > 0) {
    nblocktx(dest,cs->n,cs->a);
    nblocktx(dest,cs->n,cs->phi);
  }
}

static void mv_parm(int dest,t_parm *parm)
{
  int i;
  
  blocktx(dest,*parm);
  mv_grpopts(dest,&(parm->ir.opts));
  for(i=0; (i<DIM); i++) {
    mv_cosines(dest,&(parm->ir.ex[i]));
    mv_cosines(dest,&(parm->ir.et[i]));
  }
}

static void mv_nsb(int dest,t_nsborder *nsb)
{
  blocktx(dest,nsb->nnodes);
  blocktx(dest,nsb->shift);
  blocktx(dest,nsb->bshift);
  blocktx(dest,nsb->nstDlb);
  blocktx(dest,nsb->cgtotal);
  blocktx(dest,nsb->natoms);
  nblocktx(dest,MAXNODES,nsb->homenr);
  nblocktx(dest,MAXNODES,nsb->index);
  nblocktx(dest,MAXNODES,nsb->cgload);
  nblocktx(dest,MAXNODES,nsb->workload);
}

static void mv_string(int dest,t_symtab *symtab,char **s)
{
  int handle;
  
  handle=lookup_symtab(symtab,s);
  blocktx(dest,handle);
}

static void mv_strings(int dest,t_symtab *symtab,int nr,
                       char ***nm)
{
  int i;
  int *handle;

  snew(handle,nr);
  for(i=0; (i<nr); i++)
    handle[i]=lookup_symtab(symtab,nm[i]);
  blocktx(dest,nr);
  nblocktx(dest,nr,handle);
  sfree(handle);
}

static void mv_symtab(int dest,t_symtab *symtab)
{
  int i,nr,len;
  struct symbuf *symbuf;
  
  blocktx(dest,symtab->nr);
  nr=symtab->nr;
  symbuf=symtab->symbuf;
  while (symbuf!=NULL)
    {
      for (i=0; (i<symbuf->bufsize)&&(i<nr); i++)
        {
          len=strlen(symbuf->buf[i])+1;
          blocktx(dest,len);
          nblocktx(dest,len,symbuf->buf[i]);
        }
      nr-=i;
      symbuf=symbuf->next;
    }
  assert(nr==0);
}

static void mv_grps(int dest,t_grps grps[])
{
  int i;
  
  for(i=0; (i<egcNR); i++) {
    blocktx(dest,grps[i].nr);
    nblocktx(dest,grps[i].nr,grps[i].nm_ind);
  }
}


static void mv_atoms(int dest,t_symtab *symtab,t_atoms *atoms)
{
  int nr;
  
  nr=atoms->nr;
  blocktx(dest,nr);
  nblocktx(dest,nr,atoms->atom);
  mv_strings(dest,symtab,atoms->nr,atoms->atomname);
  mv_strings(dest,symtab,atoms->nres,atoms->resname);
  mv_strings(dest,symtab,atoms->ngrpname,atoms->grpname);
  mv_grps(dest,atoms->grps);
  mv_block(dest,&atoms->excl);
}

static void mv_vectors(int dest,int natoms,rvec x[],rvec v[])
{
  blocktx(dest,natoms);
  nblocktx(dest,natoms,x);
  nblocktx(dest,natoms,v);
}

static void mv_ilist(int dest,t_ilist *ilist)
{
  blocktx(dest,ilist->nr);
  nblocktx(dest,MAXNODES,ilist->multinr);
  nblocktx(dest,ilist->nr,ilist->iatoms);
}

static void mv_idef(int dest,t_idef *idef)
{
  int i;
  
  blocktx(dest,idef->ntypes);
  blocktx(dest,idef->atnr);
  nblocktx(dest,idef->ntypes,idef->functype);
  nblocktx(dest,idef->ntypes,idef->iparams);
  for(i=0; (i<F_NRE); i++)
    mv_ilist(dest,&idef->il[i]);
}

void mv_data(int left,int right,t_parm *parm,t_nsborder *nsb,
             t_topology *top,rvec x[],rvec v[])
{
  int i;
  
  mv_parm(right,parm);
  mv_nsb(right,nsb);
  mv_symtab(right,&top->symtab);
  mv_string(right,&top->symtab,top->name);
  mv_atoms(right,&top->symtab,&top->atoms);
  mv_idef(right,&top->idef);
  for (i=0; (i<ebNR); i++) 
    mv_block(right,&top->blocks[i]);
  mv_vectors(right,top->atoms.nr,x,v);
}

