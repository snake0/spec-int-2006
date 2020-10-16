/****************************************************************************
 *                  optout.h
 *
 * This module contains all defines, typedefs, and prototypes for OPTOUT.CPP.
 *
 * from Persistence of Vision(tm) Ray Tracer version 3.6.
 * Copyright 1991-2003 Persistence of Vision Team
 * Copyright 2003-2004 Persistence of Vision Raytracer Pty. Ltd.
 *---------------------------------------------------------------------------
 * NOTICE: This source code file is provided so that users may experiment
 * with enhancements to POV-Ray and to port the software to platforms other
 * than those supported by the POV-Ray developers. There are strict rules
 * regarding how you are permitted to use this file. These rules are contained
 * in the distribution and derivative versions licenses which should have been
 * provided with this file.
 *
 * These licences may be found online, linked from the end-user license
 * agreement that is located at http://www.povray.org/povlegal.html
 *---------------------------------------------------------------------------
 * This program is based on the popular DKB raytracer version 2.12.
 * DKBTrace was originally written by David K. Buck.
 * DKBTrace Ver 2.0-2.12 were written by David K. Buck & Aaron A. Collins.
 *---------------------------------------------------------------------------
 * $File: //depot/povray/3.5/source/optout.h $
 * $Revision: #82 $
 * $Change: 3010 $
 * $DateTime: 2004/07/27 14:06:43 $
 * $Author: thorsten $
 * $Log$
 *****************************************************************************/


#ifndef OPTOUT_H
#define OPTOUT_H

// Please put everything that isn't a preprocessor directive in this
// file into SKIP_COMPLEX_OPTOUT_H sections like the one below! [trf]
#ifndef SKIP_COMPLEX_OPTOUT_H
#include "povray.h"

BEGIN_POV_NAMESPACE

#endif

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

/* These are used by OPTOUT.C and the machine specific modules */

#define POV_RAY_IS_OFFICIAL 1
#define POV_RAY_VERSION "3.6.1"
#define POV_RAY_COPYRIGHT "Copyright 1991-2003 Persistence of Vision Team\nCopyright 2003-2004 Persistence of Vision Raytracer Pty. Ltd."
#define OFFICIAL_VERSION_NUMBER 361
#define OFFICIAL_VERSION_NUMBER_HEX 0x0361

#define DISTRIBUTION_MESSAGE_1 "This is an unofficial version prepared by the POV-Ray Team for SPEC."
#define DISTRIBUTION_MESSAGE_2 " If you have any questions about POV-Ray, please direct them to"
#define DISTRIBUTION_MESSAGE_3 " thorsten@povray.org. All other questions should be directed to SPEC."


/*****************************************************************************
* Global variables
******************************************************************************/

/* These are available for GUI environments that may display them in a credits dialog */
#ifndef SKIP_COMPLEX_OPTOUT_H

extern const char *Primary_Developers[];
extern const char *Contributing_Authors[];
extern const INTERSECTION_STATS_INFO intersection_stats[];

END_POV_NAMESPACE

#endif

#endif
