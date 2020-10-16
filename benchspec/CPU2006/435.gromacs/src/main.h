/*
 * $Id: main.h,v 1.13 2002/02/28 21:55:49 spoel Exp $
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

#ifndef _main_h
#define _main_h

static char *SRCID_main_h = "$Id: main.h,v 1.13 2002/02/28 21:55:49 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


#include <stdio.h>
#include "network.h"

extern FILE *stdlog;
extern int  gmx_parallel; /* 1 when running in parallel */

extern char *par_fn(char *base,int ftp,t_commrec *cr);
/* Add processor id in the filename right before the extension */

extern void open_log(char *fn,t_commrec *cr);
/* Open the log file, if necessary (nprocs > 1) the logfile name is
 * communicated around the ring.
 */

extern void check_multi_int(FILE *log,t_commrec *mcr,int val,char *name);
/* Check if val is the same on all processors for a mdrun -multi run
 * The string name is used to print to the log file and in a fatal error
 * if the val's don't match.
 */

extern t_commrec *init_multisystem(t_commrec *cr,int nfile,t_filenm fnm[]);
/* Returns copy of the cr commrec to be used for simulating a system
 * of cr->nnodes linked subsystems,
 * cr is modified to be non-parallel:
 *   cr->nnodes = 1;
 *   cr->nodeid = 0;
 */

extern t_commrec *init_par(int *argc,char ***argv_ptr);
/* Initiate the parallel computer. Return the communication record
 * (see network.h). The command line arguments are communicated so that they can be
 * parsed on each processor.
 * Arguments are the number of command line arguments, and a pointer to the
 * array of argument strings.
 */

#endif	/* _main_h */
