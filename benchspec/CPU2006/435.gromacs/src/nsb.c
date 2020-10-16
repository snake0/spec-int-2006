/*
 * $Id: nsb.c,v 1.7 2002/02/28 10:32:05 spoel Exp $
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
static char *SRCID_nsb_c = "$Id: nsb.c,v 1.7 2002/02/28 10:32:05 spoel Exp $";
#include "sysstuff.h"
#include "assert.h"
#include "typedefs.h"
#include "smalloc.h"
#include "invblock.h"
#include "macros.h"
#include "main.h"
#include "nsb.h"
#include "ns.h"

void calc_nsbshift(FILE *fp,t_nsborder *nsb)
{
  int i;
  int lastcg,targetcg,nshift,naaj;
  
  nsb->bshift=0;
  for(i=1; (i<nsb->nnodes); i++) {
    targetcg = nsb->workload[i-1];
    for(nshift=i; (nshift > 0) && (nsb->cgload[nshift-1] > targetcg); nshift--)
      ;
    nsb->bshift=max(nsb->bshift,i-nshift);
  }

  nsb->shift=(nsb->nnodes+1)/2;
  for(i=0; (i<nsb->nnodes); i++) {
    lastcg=nsb->cgload[i]-1;
    naaj=calc_naaj(lastcg,nsb->cgtotal);
    targetcg=(lastcg+naaj) % nsb->cgtotal;
    
    /* Search until we find the target charge group */
    for(nshift=0; (nshift < nsb->nnodes) && (targetcg > nsb->cgload[nshift]);
	nshift++)
      ;
    /* Now compute the shift, that is the difference in node index */
    nshift=((nshift-i+nsb->nnodes) % nsb->nnodes);
    
    if (fp)
      fprintf(fp,"CPU=%3d, lastcg=%5d, targetcg=%5d, myshift=%5d\n",
	      i,lastcg,targetcg,nshift);
	    
    /* It's the largest shift that matters */
    nsb->shift=max(nshift,nsb->shift);
  }
  if (fp)
    fprintf(fp,"nsb->shift = %3d, nsb->bshift=%3d\n",
	    nsb->shift,nsb->bshift);
}

void calc_nsb(FILE *fp,t_block *cgs,int nnodes,t_nsborder *nsb,int nstDlb)
{
  int  i,cg0;
  
  /* Clean! */
  for(i=0; (i<MAXNODES); i++) 
    nsb->homenr[i]=nsb->index[i]=nsb->cgload[i]=nsb->workload[i]=0;
  
  nsb->nnodes=nnodes;
  nsb->nstDlb=nstDlb;
  nsb->cgtotal=cgs->nr;
  nsb->natoms=cgs->nra;
  for(i=0; (i<nnodes); i++) {
    cg0              = (i > 0) ? cgs->multinr[i-1] : 0;
    nsb->cgload[i]   = cgs->multinr[i];
    nsb->workload[i] = cgs->multinr[i];
    nsb->index[i]    = cgs->index[cg0];
    nsb->homenr[i]   = cgs->index[cgs->multinr[i]]-nsb->index[i];
  }
  calc_nsbshift(fp,nsb);
}

void print_nsb(FILE *fp,char *title,t_nsborder *nsb)
{
  int i;

    if(fp)
    {
        fprintf(fp,"%s\n",title);
        fprintf(fp,"nsb->nodeid:     %5d\n",nsb->nodeid);
        fprintf(fp,"nsb->nnodes:  %5d\n",nsb->nnodes);
        fprintf(fp,"nsb->cgtotal: %5d\n",nsb->cgtotal);
        fprintf(fp,"nsb->natoms:  %5d\n",nsb->natoms);
        fprintf(fp,"nsb->shift:   %5d\n",nsb->shift);
        fprintf(fp,"nsb->bshift:  %5d\n",nsb->bshift);
        
        fprintf(fp,"Nodeid   index  homenr  cgload  workload\n");
        for(i=0; (i<nsb->nnodes); i++)
            fprintf(fp,"%6d%8d%8d%8d%10d\n",i,
                    nsb->index[i],nsb->homenr[i],nsb->cgload[i],nsb->workload[i]);
        fprintf(fp,"\n");
    }
}

