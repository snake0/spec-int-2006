/****************************************************************************
 *                  parsestr.h
 *
 * This module contains all defines, typedefs, and prototypes for parsestr.cpp.
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
 * $File: //depot/povray/3.5/source/parsestr.h $
 * $Revision: #10 $
 * $Change: 2929 $
 * $DateTime: 2004/07/01 14:01:40 $
 * $Author: calimet $
 * $Log$
 *****************************************************************************/


#ifndef PARSESTR_H
#define PARSESTR_H

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

char *Parse_C_String(bool pathname = false);
UCS2 *Parse_String(bool pathname = false);

UCS2 *String_To_UCS2(char *str, bool pathname = false);
char *UCS2_To_String(UCS2 *str, bool pathname = false);

UCS2 *UCS2_strcat(UCS2 *s1, UCS2 *s2);
int UCS2_strlen(UCS2 *str);
int UCS2_strcmp(UCS2 *s1, UCS2 *s2);
void UCS2_strcpy(UCS2 *s1, UCS2 *s2);
void UCS2_strncpy(UCS2 *s1, UCS2 *s2, int n);
void UCS2_strupr(UCS2 *str);
void UCS2_strlwr(UCS2 *str);

END_POV_NAMESPACE

#endif
