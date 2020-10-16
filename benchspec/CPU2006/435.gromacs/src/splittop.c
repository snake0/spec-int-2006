/*
 * $Id: splittop.c,v 1.7 2002/02/28 10:32:06 spoel Exp $
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
 * GROup of MAchos and Cynical Suckers
 */
static char *SRCID_splittop_c = "$Id: splittop.c,v 1.7 2002/02/28 10:32:06 spoel Exp $";
#include "sysstuff.h"
#include "typedefs.h"
#include "splittop.h"
#include "smalloc.h"
#include "fatal.h"
#include "main.h"
#include "dummies.h"

void create_dummylist(int nindex, int *list,
		      int *targetn, int **listptr)
{
  int i,j,k,inr;
  int minidx;
  int *newlist;

  /* remove duplicates */
  for(i=0;i<nindex;i++) {
    inr=list[i];
    for(j=i+1;j<nindex;j++) {
      if(list[j]==inr) {
	for(k=j;k<nindex-1;k++)
	  list[k]=list[k+1];
	nindex--;
      }
    }
  }

  *targetn=nindex;
  snew(newlist,nindex);
  
  /* sort into the new array */
  for(i=0;i<nindex;i++) {
    inr=-1;
    for(j=0;j<nindex;j++)
      if(list[j]>0 && (inr==-1 || list[j]<list[inr])) 
	inr=j; /* smallest so far */
    newlist[i]=list[inr];
    list[inr]=-1;
  }
  *listptr=newlist;
}
  

bool setup_parallel_dummies(t_idef *idef,t_commrec *cr,t_nsborder *nsb,
			    t_comm_dummies *dummycomm)
{
  int i,inr,j,k,ftype;
  int minidx,minhome,ihome;
  int nra,nrd,nconstr;
  bool found=FALSE;
  t_iatom   *ia;
  int *idxprevdum;
  int *idxnextdum;
  int *idxprevconstr;
  int *idxnextconstr;
  int  nprevdum=0,nnextdum=0;
  int  nprevconstr=0,nnextconstr=0;

#define BUFLEN 100
  
  snew(idxprevdum,BUFLEN);
  snew(idxnextdum,BUFLEN);
  snew(idxprevconstr,BUFLEN);
  snew(idxnextconstr,BUFLEN);  

  for(ftype=0; (ftype<F_NRE); ftype++) {
    if (interaction_function[ftype].flags & IF_DUMMY) {
      nra    = interaction_function[ftype].nratoms;
      nrd    = idef->il[ftype].nr;
      ia     = idef->il[ftype].iatoms;
      
      for(i=0; (i<nrd); ) {
	
	/* The dummy and constructing atoms */
	if (ftype==F_DUMMY2)
	  nconstr=2;
	else if(ftype==F_DUMMY4FD)
	  nconstr=4;
	else
	  nconstr=3;
	
	minidx=ia[1];
	for(j=2;j<nconstr+2;j++) 
	  if(ia[j]<minidx)
	    minidx=ia[j];

	minhome=0;
	while(minidx>=(nsb->index[minhome]+nsb->homenr[minhome]))
          minhome++;

	if(minhome==cr->nodeid) {
	  /* This is my dummy interaction - but is the dummy local?
	   * If not, he must be on the next node (error otherwise)
	   * (but we do account for the cyclic ring structure)
	   */
	  if(ia[1]<nsb->index[cr->nodeid] ||
	     ia[1]>=(nsb->index[cr->nodeid]+nsb->homenr[cr->nodeid])) {
	    if((nnextdum%BUFLEN)==0 && nnextdum>0)
	      srenew(idxnextdum,nnextdum+BUFLEN);
	    idxnextdum[nnextdum++]=ia[1];
	    found=TRUE;
	  }
	  for(j=2;j<nconstr+2;j++) {
	    inr=ia[j];
	    ihome=0;
	    while(inr>=(nsb->index[ihome]+nsb->homenr[ihome]))
	      ihome++;
	    if( ihome>(cr->nodeid+1))
	      fatal_error(0,"Dummy particle %d and its constructing"
			  " atoms are not on the same or adjacent\n" 
			  " nodes. This is necessary to avoid a lot\n"
			  " of extra communication. The easiest way"
			  " to ensure this is to place dummies\n"
			  " close to the constructing atoms.\n"
			  " Sorry, but you will have to rework your topology!\n",
			  ia[1]);
	    else if(ihome==((cr->nodeid+1)%cr->nnodes)) {
	      if((nnextconstr%BUFLEN)==0 && nnextconstr>0)
		srenew(idxnextconstr,nnextconstr+BUFLEN);
	      idxnextconstr[nnextconstr++]=ia[j];
	      found=TRUE;
	    }
	  }
	} else if(minhome==((cr->nodeid-1+cr->nnodes)%cr->nnodes)) {
	  /* Not our dummy, but we might be involved */
	  if(ia[1]>=nsb->index[cr->nodeid] &&
	     (ia[1]<(nsb->index[cr->nodeid]+nsb->homenr[cr->nodeid]))) {
	    if((nprevdum%BUFLEN)==0 && nprevdum>0)
	      srenew(idxprevdum,nprevdum+BUFLEN);
	    idxprevdum[nprevdum++]=ia[1];
	    found=TRUE;
	  }
	  for(j=2;j<nconstr+2;j++) {
	    inr=ia[j];
	    if(ia[j]>=nsb->index[cr->nodeid] &&
	       (ia[1]<(nsb->index[cr->nodeid]+nsb->homenr[cr->nodeid]))) {
	      if((nprevconstr%BUFLEN)==0 && nprevconstr>0)
		srenew(idxprevconstr,nprevconstr+BUFLEN);
	      idxprevconstr[nprevconstr++]=ia[j];
	      found=TRUE;
	    }
	  }
	}
	/* Increment loop variables */
	i  += nra+1;
	ia += nra+1;
      }
    }
  }

  create_dummylist(nprevdum,idxprevdum,
		   &(dummycomm->nprevdum),&(dummycomm->idxprevdum));
  create_dummylist(nnextdum,idxnextdum,
		   &(dummycomm->nnextdum),&(dummycomm->idxnextdum));
  create_dummylist(nprevconstr,idxprevconstr,
		   &(dummycomm->nprevconstr),&(dummycomm->idxprevconstr));
  create_dummylist(nnextconstr,idxnextconstr,
		   &(dummycomm->nnextconstr),&(dummycomm->idxnextconstr));

  sfree(idxprevdum);
  sfree(idxnextdum);
  sfree(idxprevconstr);
  sfree(idxnextconstr);

  return found;
#undef BUFLEN
}



  

static void split_ilist(FILE *log,t_ilist *il,t_commrec *cr)
{
  t_iatom *ia;
  int     i,start,end,nr;
  
  if (cr->nodeid == 0)
    start=0;
  else
    start=il->multinr[cr->nodeid-1];
  end=il->multinr[cr->nodeid];
  
  nr=end-start;
  if (nr < 0)
    fatal_error(0,"Negative number of atoms (%d) on node %d\n"
		"You have probably not used the same value for -np with grompp"
		" and mdrun",
		nr,cr->nodeid);
  snew(ia,nr);

  for(i=0; (i<nr); i++)
    ia[i]=il->iatoms[start+i];

  sfree(il->iatoms);
  il->iatoms=ia;
  
  for(i=0; (i<MAXNODES); i++)
    il->multinr[i]=nr;
  il->nr=nr;
}

static void split_idef(FILE *log,t_idef *idef,t_commrec *cr)
{
  int i;
  
  for(i=0; (i<F_NRE); i++)
    split_ilist(log,&idef->il[i],cr);
}
	
void mdsplit_top(FILE *log,t_topology *top,t_commrec *cr,
		 t_nsborder *nsb, bool *bParallelDummies,
		 t_comm_dummies *dummycomm)
{
  if (cr->nnodes < 2)
    return;

  *bParallelDummies=setup_parallel_dummies(&(top->idef),cr,nsb,dummycomm);
  
  split_idef(log,&top->idef,cr);
#ifdef DEBUG
  pr_idef(log,0,"After Split",&(top->idef));
#endif
}
