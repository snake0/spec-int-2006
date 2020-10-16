/****************************************************************************
 *                  interior.h
 *
 * This module contains all defines, typedefs, and prototypes for INTERIOR.CPP.
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
 * $File: //depot/povray/3.5/source/interior.h $
 * $Revision: #12 $
 * $Change: 2929 $
 * $DateTime: 2004/07/01 14:01:40 $
 * $Author: calimet $
 * $Log$
 *****************************************************************************/


#ifndef INTERIOR_H
#define INTERIOR_H

#include "media.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/


/*****************************************************************************
* Global typedefs
******************************************************************************/


/*****************************************************************************
* Global variables
******************************************************************************/



/*****************************************************************************
* Global functions
******************************************************************************/

void Init_Interior (INTERIOR *);

INTERIOR *Create_Interior (void);
INTERIOR *Copy_Interior (INTERIOR *);
INTERIOR *Copy_Interior_Pointer (INTERIOR *);
void Destroy_Interior (INTERIOR *);

void Transform_Interior (INTERIOR *, TRANSFORM *);

MATERIAL *Create_Material (void);
MATERIAL *Copy_Material (MATERIAL *);
void Destroy_Material (MATERIAL *);

END_POV_NAMESPACE

#endif
