/*
 * $Id: maths.h,v 1.8 2002/02/28 21:55:49 spoel Exp $
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

#ifndef _maths_h
#define _maths_h

static char *SRCID_maths_h = "$Id: maths.h,v 1.8 2002/02/28 21:55:49 spoel Exp $";
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


#include <math.h>
#include "typedefs.h"

#ifndef M_PI
#define	M_PI		3.14159265358979323846
#endif

#ifndef M_PI_2
#define	M_PI_2		1.57079632679489661923
#endif

#ifndef M_2PI
#define	M_2PI		6.28318530718
#endif

#ifndef M_SQRT2
#define M_SQRT2 sqrt(2.0)
#endif

#ifdef CPLUSPLUS
extern "C" {
#endif

extern	int		gmx_nint(real a);
extern  real            sign(real x,real y);

extern  real            gmx_erf(real x);
extern  real            gmx_erfc(real x);

#ifdef CPLUSPLUS
}
#endif

#endif	/* _maths_h */
