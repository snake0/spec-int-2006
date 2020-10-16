/****************************************************************************
 *                  parstxtr.h
 *
 * This header file is included by all all language parsing C modules in
 * POV-Ray.
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
 * $File: //depot/povray/3.5/source/parstxtr.h $
 * $Revision: #14 $
 * $Change: 2929 $
 * $DateTime: 2004/07/01 14:01:40 $
 * $Author: calimet $
 * $Log$
 *****************************************************************************/

#ifndef PARSTXTR_H
#define PARSTXTR_H

#include "atmosph.h"
#include "interior.h"

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

extern TEXTURE *Default_Texture;



/*****************************************************************************
* Global functions
******************************************************************************/

TEXTURE *Parse_Texture (void);
void Parse_Pigment (PIGMENT **);
void Parse_Tnormal (TNORMAL **);
void Parse_Finish (FINISH **);
void Parse_Media (IMEDIA **);
void Parse_Interior (INTERIOR **);
void Parse_Media_Density_Pattern (PIGMENT **);
FOG *Parse_Fog (void);
RAINBOW *Parse_Rainbow (void);
SKYSPHERE *Parse_Skysphere (void);
IMAGE *Parse_Image (int);
void Parse_Material(MATERIAL *);
void Parse_PatternFunction(TPATTERN *);

END_POV_NAMESPACE

#endif
