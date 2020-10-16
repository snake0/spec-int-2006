/*
 * $Id: ebin.h,v 1.7 2002/02/28 21:55:49 spoel Exp $
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
 * Grunge ROck MAChoS
 */

#ifndef _ebin_h
#define _ebin_h

static char *SRCID_ebin_h = "$Id: ebin.h,v 1.7 2002/02/28 21:55:49 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "sysstuff.h"
#include "typedefs.h"
	
typedef struct {
  int      nener;
  char     **enm;
  t_energy *e;
} t_ebin;

enum { eprNORMAL, eprAVER, eprRMS, eprNR };

extern t_ebin *mk_ebin(void);
/* Create an energy bin */

extern int get_ebin_space(t_ebin *eb,int nener,char *enm[]);
/* Create space in the energy bin and register names.
 * The enm array must be static, because the contents are not copied,
 * but only the pointers.
 * Function returns an index number that must be used in subsequent
 * calls to add_ebin.
 */

extern void add_ebin(t_ebin *eb,int index,int nener,real ener[],int step);
/* Add nener reals (eg. energies, box-lengths, pressures) to the
 * energy bin at position index. 
 */
 
extern void pr_ebin(FILE *fp,t_ebin *eb,int index,int nener,int nperline,
		    int prmode,int tsteps,bool bPrHead);
/* Print the contents of the energy bin. If nener = -1 ALL energies from
 * index to the end will be printed. We will print nperline entries on a text
 * line (advisory <= 5). prmode may be any of the above listed enum values.
 * tsteps is used only when eprAVER or eprRMS is set.
 * If bPrHead than the header is printed.
 */



#endif	/* _ebin_h */
