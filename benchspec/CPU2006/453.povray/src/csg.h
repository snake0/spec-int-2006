/****************************************************************************
 *                  csg.h
 *
 * This module contains all defines, typedefs, and prototypes for CSG.CPP.
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
 * $File: //depot/povray/3.5/source/csg.h $
 * $Revision: #13 $
 * $Change: 2929 $
 * $DateTime: 2004/07/01 14:01:40 $
 * $Author: calimet $
 * $Log$
 *****************************************************************************/


#ifndef CSG_H
#define CSG_H

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Global preprocessor defines
******************************************************************************/

#define UNION_OBJECT        (IS_COMPOUND_OBJECT)
#define MERGE_OBJECT        (IS_COMPOUND_OBJECT)
#define INTERSECTION_OBJECT (IS_COMPOUND_OBJECT)

/* CSG types */

#define CSG_UNION_TYPE             1
#define CSG_INTERSECTION_TYPE      2
#define CSG_DIFFERENCE_TYPE        4
#define CSG_MERGE_TYPE             8
#define CSG_SINGLE_TYPE           16



/*****************************************************************************
* Global typedefs
******************************************************************************/

typedef struct CSG_Struct CSG;

struct CSG_Struct
{
  COMPOUND_FIELDS
  int do_split;
};



/*****************************************************************************
* Global variables
******************************************************************************/

extern METHODS CSG_Intersection_Methods;
extern METHODS CSG_Merge_Methods;
extern METHODS CSG_Union_Methods;



/*****************************************************************************
* Global functions
******************************************************************************/

CSG *Create_CSG_Union (void);
CSG *Create_CSG_Merge (void);
CSG *Create_CSG_Intersection (void);
void Compute_CSG_BBox (OBJECT *Object);
void Determine_CSG_Textures(CSG *Csg, VECTOR IPoint, int *Count, TEXTURE **Textures, DBL *Weights);

END_POV_NAMESPACE

#endif
