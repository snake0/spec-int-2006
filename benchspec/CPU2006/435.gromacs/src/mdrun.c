/*
 * $Id: mdrun.c,v 1.62 2002/02/28 10:54:43 spoel Exp $
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
 * GROningen Mixture of Alchemy and Childrens' Stories
 */
static char *SRCID_mdrun_c = "$Id: mdrun.c,v 1.62 2002/02/28 10:54:43 spoel Exp $";

#include <string.h>

#include "typedefs.h"
#include "macros.h"
#include "copyrite.h"
#include "main.h"
#include "statutil.h"
#include "futil.h"
#include "edsam.h"
#include "mdrun.h"
#include "xmdrun.h"
/* afm stuf */
#include "pull.h"

int main(int argc,char *argv[])
{
  static char *desc[] = {
    "The mdrun program is the main computational chemistry engine",
    "within GROMACS. Obviously, it performs Molecular Dynamics simulations,",
    "but it can also perform Brownian Dynamics and Langevin Dynamics",
    "as well as Conjugate Gradient or Steepest Descents energy minimization.",
    "Normal mode analysis is another option. In this case mdrun",
    "builds a Hessian matrix from single conformation.",
    "For usual Normal Modes-like calculations, make sure that",
    "the structure provided is properly energy-minimised.",
    "The generated matrix can be diagonalized by g_nmeig.[PAR]"
    "The mdrun program reads the run input file ([TT]-s[tt]) and distributes the",
    "topology over nodes if needed. The coordinates are passed",
    "around, so that computations can begin.",
    "First a neighborlist is made, then the forces are computed.",
    "The forces are globally summed, and the velocities and",
    "positions are updated. If necessary shake is performed to constrain",
    "bond lengths and/or bond angles.",
    "Temperature and Pressure can be controlled using weak coupling to a",
    "bath.[PAR]",
    "mdrun produces at least three output file, plus one log file",
    "([TT]-g[tt]) per node.",
    "The trajectory file ([TT]-o[tt]), contains coordinates, velocities and",
    "optionally forces.",
    "The structure file ([TT]-c[tt]) contains the coordinates and",
    "velocities of the last step.",
    "The energy file ([TT]-e[tt]) contains energies, the temperature,",
    "pressure, etc, a lot of these things are also printed in the log file",
    "of node 0.",
    "Optionally coordinates can be written to a compressed trajectory file",
    "([TT]-x[tt]).[PAR]",
    "When running in parallel with PVM or an old version of MPI the",
    "[TT]-np[tt] option must be given to indicate the number of",
    "nodes.[PAR]",
    "The option [TT]-dgdl[tt] is only used when free energy perturbation is",
    "turned on.[PAR]",
    "With [TT]-rerun[tt] an input trajectory can be given for which ",
    "forces and energies will be (re)calculated. Neighbor searching will be",
    "performed for every frame, unless [TT]nstlist[tt] is zero",
    "(see the [TT].mdp[tt] file).[PAR]",
    "ED (essential dynamics) sampling is switched on by using the [TT]-ei[tt]",
    "flag followed by an [TT].edi[tt] file.",
    "The [TT].edi[tt] file can be produced using options in the essdyn",
    "menu of the WHAT IF program. mdrun produces a [TT].edo[tt] file that",
    "contains projections of positions, velocities and forces onto selected",
    "eigenvectors.[PAR]",
    "The -table option can be used to pass mdrun a formatted table with",
    "user-defined potential functions. The file is read from either the",
    "current directory or from the GMXLIB directory. A number of preformatted",
    "tables are presented in the GMXLIB dir, for 6-8, 6-9, 6-10, 6-11, 6-12",
    "Lennard Jones potentials with normal Coulomb.[PAR]",
    "The options [TT]-pi[tt], [TT]-po[tt], [TT]-pd[tt], [TT]-pn[tt] are used",
    "for potential of mean force calculations and umbrella sampling.",
    "See manual.[PAR]",
    "When mdrun receives a TERM signal, it will set nsteps to the current",
    "step plus one. When mdrun receives a USR1 signal, it will set nsteps",
    "to the next multiple of nstxout after the current step.",
    "In both cases all the usual output will be written to file.",
    "When running with MPI, a signal to one of the mdrun processes",
    "is sufficient, this signal should not be sent to mpirun or",
    "the mdrun process that is the parent of the others.",
    "Finally some experimental algorithms can be tested when the",
    "appropriate options have been given. Currently under",
    "investigation are: polarizibility, glass simulations, ",
    "Free energy perturbation, X-Ray bombardments",
    "and parallel independent simulations."
  };
  t_commrec    *cr,*mcr;
  static t_filenm fnm[] = {
    { efTPX, NULL,      NULL,       ffREAD },
    { efTRN, "-o",      NULL,       ffWRITE },
    { efXTC, "-x",      NULL,       ffOPTWR },
    { efSTO, "-c",      "confout",  ffWRITE },
    { efENX, "-e",      "ener",     ffWRITE },
    { efLOG, "-g",      "md",       ffWRITE },
    { efXVG, "-dgdl",   "dgdl",     ffOPTWR },
    { efXVG, "-table",  "table",    ffOPTRD },
    { efTRX, "-rerun",  "rerun",    ffOPTRD },
    { efEDI, "-ei",     "sam",      ffOPTRD },
    { efEDO, "-eo",     "sam",      ffOPTWR },
    { efGCT, "-j",      "wham",     ffOPTRD },
    { efGCT, "-jo",     "bam",      ffOPTRD },
    { efXVG, "-ffout",  "gct",      ffOPTWR },
    { efXVG, "-devout", "deviatie", ffOPTWR },
    { efXVG, "-runav",  "runaver",  ffOPTWR },
    { efPPA, "-pi",     "pull",     ffOPTRD },
    { efPPA, "-po",     "pullout",  ffOPTWR },
    { efPDO, "-pd",     "pull",     ffOPTWR },
    { efNDX, "-pn",     "pull",     ffOPTRD },
    { efMTX, "-mtx",    "nm",       ffOPTWR },
    { efOUT, "-spec",   "spec",     ffWRITE }
  };
#define NFILE asize(fnm)

  /* Command line options ! */
  static bool bVerbose     = FALSE;
  static bool bCompact     = TRUE;
  static bool bSepDVDL     = FALSE;
  static bool bMultiSim    = FALSE;
  static bool bGlas        = FALSE;
  static bool bIonize      = FALSE;
  
  static int  nDLB=0; 
  static int  nnodes=1;
  static int  nstepout=10;
  static t_pargs pa[] = {
    { "-np",      FALSE, etINT, {&nnodes},
      "Number of nodes, must be the same as used for grompp" },
    { "-v",       FALSE, etBOOL,{&bVerbose},  
      "Be loud and noisy" },
    { "-compact", FALSE, etBOOL,{&bCompact},  
      "Write a compact log file" },
    { "-multi",   FALSE, etBOOL,{&bMultiSim}, 
      "Do multiple simulations in parallel (only with -np > 1)" },
    { "-glas",    FALSE, etBOOL,{&bGlas},
      "Do glass simulation with special long range corrections" },
    { "-ionize",  FALSE, etBOOL,{&bIonize},
      "Do a simulation including the effect of an X-Ray bombardment on your system" },
    { "-sepdvdl", FALSE, etBOOL,{&bSepDVDL},
      "HIDDENWrite separate V and dVdl terms for each interaction and node(!) to log file(s)" },
    { "-stepout", FALSE, etINT, {&nstepout},
      "HIDDENFrequency of writing the remaining runtime" }
  };
  t_edsamyn edyn;
  unsigned long Flags;
  unsigned long pca_quiet;
  int           i;
  
  cr = init_par(&argc,&argv);
  edyn.bEdsam=FALSE;

#ifndef SPEC_CPU
  if (MASTER(cr))
    CopyRight(stderr,argv[0]);
#endif
  
  pca_quiet = PCA_QUIET;

  /* Pre-parse arguments on head node to check if the -v flag (verbose) was specified */
  if(MASTER(cr))
  {
      for(i=1;i<argc;i++)
      {
          if(!strncmp("-v",argv[i],2))
          {
              pca_quiet = 0;
          }
      }
  }

  parse_common_args(&argc,argv,
		    PCA_KEEP_ARGS | PCA_NOEXIT_ON_ARGS | PCA_BE_NICE |
		    PCA_CAN_SET_DEFFNM | pca_quiet,
		    NFILE,fnm,asize(pa),pa,asize(desc),desc,0,NULL);
  bVerbose = bVerbose && MASTER(cr);
    
#ifndef USE_MPI
  if (nnodes > 1) 
    fatal_error(0,"GROMACS compiled without MPI support - can't do parallel runs");
#endif

  if(bVerbose)
      open_log(ftp2fn(efLOG,NFILE,fnm),cr);

  if (bMultiSim && PAR(cr))
    mcr = init_multisystem(cr,NFILE,fnm);
  else
    mcr = NULL;

  if (MASTER(cr) && stdlog != NULL) {
      CopyRight(stdlog,argv[0]);
      please_cite(stdlog,"Lindahl2001a");
      please_cite(stdlog,"Berendsen95a");
  }
  
  if (opt2bSet("-ei",NFILE,fnm)) 
    ed_open(NFILE,fnm,&edyn);
    
  Flags = opt2bSet("-rerun",NFILE,fnm) ? MD_RERUN : 0;
  Flags = Flags | (bSepDVDL ? MD_SEPDVDL : 0);
  
  Flags = (Flags | 
	   (bIonize   ? MD_IONIZE   : 0) |
	   (bMultiSim ? MD_MULTISIM : 0) |
	   (bGlas     ? MD_GLAS     : 0));

  mdrunner(cr,mcr,NFILE,fnm,bVerbose,bCompact,nDLB,nstepout,&edyn,Flags);
  
  if (gmx_parallel)
    gmx_finalize();

  if (MASTER(cr) && bVerbose)
  {
      thanx(stderr);
  }
  
  return 0;
}

