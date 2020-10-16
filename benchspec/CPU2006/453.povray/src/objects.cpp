/****************************************************************************
 *               objects.cpp
 *
 * This module implements the methods for objects and composite objects.
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
 * $File: //depot/povray/3.5/source/objects.cpp $
 * $Revision: #26 $
 * $Change: 3010 $
 * $DateTime: 2004/07/27 14:06:43 $
 * $Author: thorsten $
 * $Log$
 *****************************************************************************/

#include "frame.h"
#include "povray.h"
#include "vector.h"
#include "interior.h"
#include "objects.h"
#include "texture.h"
#include "matrices.h"

BEGIN_POV_NAMESPACE

/*****************************************************************************
* Local preprocessor defines
******************************************************************************/

/*****************************************************************************
* Local typedefs
******************************************************************************/

/*****************************************************************************
* Local variables
******************************************************************************/

unsigned int Number_of_istacks = 0; // GLOBAL VARIABLE
unsigned int Max_Intersections = 64; // GLOBAL VARIABLE
ISTACK *free_istack; // GLOBAL VARIABLE

/*****************************************************************************
* Static functions
******************************************************************************/

static OBJECT *Copy_Bound_Clip (OBJECT *Old);
static void create_istack (void);



/*****************************************************************************
*
* FUNCTION
*
*   Intersection
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

bool Intersection (INTERSECTION *Ray_Intersection, OBJECT *Object, RAY *Ray)
{
  ISTACK *Depth_Stack;
  INTERSECTION *Local;
  DBL Closest = HUGE_VAL;

  if (Object == NULL)
  {
    return (false);
  }

  if (!Ray_In_Bound (Ray,Object->Bound))
  {
    return (false);
  }

  Depth_Stack = open_istack ();

  if (All_Intersections (Object, Ray, Depth_Stack))
  {
    while ((Local = pop_entry(Depth_Stack)) != NULL)
    {
      if (Local->Depth < Closest)
      {
        *Ray_Intersection = *Local;

        Closest = Local->Depth;
      }
    }

    close_istack (Depth_Stack);

    return (true);
  }
  else
  {
    close_istack (Depth_Stack);

    return (false);
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   Inside_Object
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

bool Inside_Object (VECTOR IPoint, OBJECT *Object)
{
  OBJECT *Sib;

  for (Sib = Object->Clip; Sib != NULL; Sib = Sib->Sibling)
  {
    if (!Inside_Object(IPoint, Sib))
    {
      return(false);
    }
  }

  return (Inside(IPoint,Object));
}



/*****************************************************************************
*
* FUNCTION
*
*   Ray_In_Bound
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

bool Ray_In_Bound (RAY *Ray, OBJECT *Bounding_Object)
{
  OBJECT *Bound;
  INTERSECTION Local;

  for (Bound = Bounding_Object; Bound != NULL; Bound = Bound->Sibling)
  {
    Increase_Counter(stats[Bounding_Region_Tests]);

    if (!Intersection (&Local, Bound, Ray))
    {
      if (!Inside_Object(Ray->Initial, Bound))
      {
        return (false);
      }
    }

    Increase_Counter(stats[Bounding_Region_Tests_Succeeded]);
  }

  return (true);
}



/*****************************************************************************
*
* FUNCTION
*
*   Point_In_Clip
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

bool Point_In_Clip (VECTOR IPoint, OBJECT *Clip)
{
  OBJECT *Local_Clip;

  for (Local_Clip = Clip; Local_Clip != NULL; Local_Clip = Local_Clip->Sibling)
  {
    Increase_Counter(stats[Clipping_Region_Tests]);

    if (!Inside_Object(IPoint, Local_Clip))
    {
      return (false);
    }

    Increase_Counter(stats[Clipping_Region_Tests_Succeeded]);
  }

  return (true);
}



/*****************************************************************************
*
* FUNCTION
*
*   Translate_Object
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

void Translate_Object (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans)
{
  OBJECT *Sib;

  if (Object == NULL)
  {
    return;
  }

  for (Sib = Object->Bound; Sib != NULL; Sib = Sib->Sibling)
  {
    Translate_Object(Sib, Vector, Trans);
  }

  if (Object->Clip != Object->Bound)
  {
    for (Sib = Object->Clip; Sib != NULL; Sib = Sib->Sibling)
    {
      Translate_Object(Sib, Vector, Trans);
    }
  }

  /* NK 1998 added if */
  if (!Test_Flag(Object, UV_FLAG))
  {
    Transform_Textures(Object->Texture, Trans);
    Transform_Textures(Object->Interior_Texture, Trans);
  }

	if (Object->UV_Trans == NULL)
		Object->UV_Trans = Create_Transform();
	Compose_Transforms(Object->UV_Trans, Trans);

  Transform_Interior(Object->Interior, Trans);

  Translate(Object, Vector, Trans);
}



/*****************************************************************************
*
* FUNCTION
*
*   Rotate_Object
*
* INPUT
*
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

void Rotate_Object (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans)
{
  OBJECT *Sib;

  if (Object == NULL)
  {
    return;
  }

  for (Sib = Object->Bound; Sib != NULL; Sib = Sib->Sibling)
  {
    Rotate_Object(Sib, Vector, Trans);
  }

  if (Object->Clip != Object->Bound)
  {
    for (Sib = Object->Clip; Sib != NULL; Sib = Sib->Sibling)
    {
      Rotate_Object(Sib, Vector, Trans);
    }
  }

  /* NK 1998 added if */
  if (!Test_Flag(Object, UV_FLAG))
  {
    Transform_Textures(Object->Texture, Trans);
    Transform_Textures(Object->Interior_Texture, Trans);
  }

	if (Object->UV_Trans == NULL)
		Object->UV_Trans = Create_Transform();
	Compose_Transforms(Object->UV_Trans, Trans);

  Transform_Interior(Object->Interior, Trans);

  Rotate(Object, Vector, Trans);
}



/*****************************************************************************
*
* FUNCTION
*
*   Scale_Object
*
* INPUT
*
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

void Scale_Object (OBJECT *Object, VECTOR Vector, TRANSFORM *Trans)
{
  OBJECT *Sib;

  if (Object == NULL)
  {
    return;
  }

  for (Sib = Object->Bound; Sib != NULL; Sib = Sib->Sibling)
  {
    Scale_Object(Sib, Vector, Trans);
  }

  if (Object->Clip != Object->Bound)
  {
    for (Sib = Object->Clip; Sib != NULL; Sib = Sib->Sibling)
    {
      Scale_Object(Sib, Vector, Trans);
    }
  }

  /* NK 1998 added if */
  if (!Test_Flag(Object, UV_FLAG))
  {
    Transform_Textures(Object->Texture, Trans);
    Transform_Textures(Object->Interior_Texture, Trans);
  }

	if (Object->UV_Trans == NULL)
		Object->UV_Trans = Create_Transform();
	Compose_Transforms(Object->UV_Trans, Trans);

  Transform_Interior(Object->Interior, Trans);

  Scale(Object, Vector, Trans);
}



/*****************************************************************************
*
* FUNCTION
*
*   Transform_Object
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

void Transform_Object (OBJECT *Object, TRANSFORM *Trans)
{
  OBJECT *Sib;

  if (Object == NULL)
  {
    return;
  }

  for (Sib = Object->Bound; Sib != NULL; Sib = Sib->Sibling)
  {
    Transform_Object(Sib, Trans);
  }

  if (Object->Clip != Object->Bound)
  {
    for (Sib = Object->Clip; Sib != NULL; Sib = Sib->Sibling)
    {
      Transform_Object(Sib, Trans);
    }
  }

  /* NK 1998 added if */
  if (!Test_Flag(Object, UV_FLAG))
  {
    Transform_Textures(Object->Texture, Trans);
    Transform_Textures(Object->Interior_Texture, Trans);
  }

	if (Object->UV_Trans == NULL)
		Object->UV_Trans = Create_Transform();
	Compose_Transforms(Object->UV_Trans, Trans);

  Transform_Interior(Object->Interior, Trans);

  Transform(Object,Trans);
}



/*****************************************************************************
*
* FUNCTION
*
*   Invert_Object
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

void Invert_Object (OBJECT *Object)
{
  if (Object == NULL)
  {
    return;
  }

  Invert (Object);
}



/*****************************************************************************
*
* FUNCTION
*
*   Copy_Bound_Clip
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static OBJECT *Copy_Bound_Clip (OBJECT *Old)
{
  OBJECT *Current, *New, *Prev, *First;

  First = Prev = NULL;

  for (Current = Old; Current != NULL; Current = Current->Sibling)
  {
    New = Copy_Object (Current);

    if (First == NULL)
    {
      First = New;
    }

    if (Prev != NULL)
    {
      Prev->Sibling = New;
    }

    Prev = New;
  }

  return (First);
}



/*****************************************************************************
*
* FUNCTION
*
*   Copy_Object
*
* INPUT
*   
* OUTPUT
*   
* RETURNS
*   
* AUTHOR
*
*   POV-Ray Team
*   
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

OBJECT *Copy_Object (OBJECT *Old)
{
  OBJECT *New;

  if (Old == NULL)
  {
    return (NULL);
  }

  New = (OBJECT *)Copy(Old);

 /*
  * The following copying of OBJECT_FIELDS is redundant if Copy
  * did *New = *Old but we cannot assume it did. It is safe for
  * Copy to do *New = *Old but it should not otherwise
  * touch OBJECT_FIELDS.
  */

  New->Methods = Old->Methods;
  New->Type    = Old->Type;
  New->Sibling = Old->Sibling;
  New->Texture = Old->Texture;
  New->Bound   = Old->Bound;
  New->Clip    = Old->Clip;
  New->BBox    = Old->BBox;
  New->Flags   = Old->Flags;
  New->LLights = NULL;  /* Important */

  New->Sibling = NULL;  /* Important */

  New->Texture = Copy_Textures (Old->Texture);
  New->Interior_Texture = Copy_Textures (Old->Interior_Texture);

  New->Bound   = Copy_Bound_Clip (Old->Bound);

  New->Interior = Copy_Interior(Old->Interior);

  /* NK 1998 */
  New->UV_Trans = Copy_Transform(Old->UV_Trans);
  /* NK ---- */

  if (Old->Bound != Old->Clip)
  {
    New->Clip  = Copy_Bound_Clip (Old->Clip);
  }
  else
  {
    New->Clip  = New->Bound;
  }

  return (New);
}



/*****************************************************************************
*
* FUNCTION
*
*   Destroy_Object
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

void Destroy_Single_Object (OBJECT **ObjectPtr)
{
  OBJECT *Object;

  Object = *ObjectPtr;

  Destroy_Textures(Object->Texture);

  Destroy_Object(Object->Bound);

  Destroy_Interior((INTERIOR *)Object->Interior);

	/* NK 1998 */
  Destroy_Transform(Object->UV_Trans);

  Destroy_Object (Object->Bound);
  Destroy_Interior((INTERIOR *)Object->Interior);

  if (Object->Bound != Object->Clip)
  {
    Destroy_Object(Object->Clip);
  }

  *ObjectPtr = Object->Sibling;

  Destroy(Object);
}

void Destroy_Object (OBJECT *Object)
{
  OBJECT *Sib;

  while (Object != NULL)
  {
    Destroy_Textures(Object->Texture);
    Destroy_Textures(Object->Interior_Texture);
    Destroy_Object(Object->Bound);

    Destroy_Interior((INTERIOR *)Object->Interior);

	/* NK 1998 */
    Destroy_Transform(Object->UV_Trans);

    if (Object->Bound != Object->Clip)
    {
      Destroy_Object(Object->Clip);
    }

    Sib = Object->Sibling;

    Destroy(Object);

    Object = Sib;
  }
}



/*****************************************************************************
*
* FUNCTION
*
*   create_istack
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

static void create_istack()
{
  ISTACK *New;
  int i;

  New = (ISTACK *)POV_MALLOC(sizeof (ISTACK), "istack");

  New->next = free_istack;

  free_istack = New;

  New->istack = (INTERSECTION *)POV_MALLOC(Max_Intersections * sizeof (INTERSECTION), "istack entries");
  New->max_entries = Max_Intersections;

  // make sure we have valid NULL pointers [trf]
  for(i = 0; i < New->max_entries; i++)
    New->istack[i].Object = NULL;

  Number_of_istacks++;
}




/*****************************************************************************
*
* FUNCTION
*
*   Destroy_IStacks
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

void Destroy_IStacks()
{
  ISTACK *istk, *temp;

  istk = free_istack;

  while (istk != NULL)
  {
    temp = istk;

    istk = istk->next;

    POV_FREE (temp->istack);

    POV_FREE (temp);
  }

  free_istack = NULL;
}



/*****************************************************************************
*
* FUNCTION
*
*   open_sstack
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

ISTACK *open_istack()
{
  ISTACK *istk;

  if (free_istack == NULL)
  {
    create_istack ();
  }

  istk = free_istack;

  free_istack = istk->next;

  istk->top_entry = 0;

  return (istk);
}



/*****************************************************************************
*
* FUNCTION
*
*   close_istack
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

void close_istack (ISTACK *istk)
{
  istk->next = free_istack;

  free_istack = istk;
}


/*****************************************************************************
*
* FUNCTION
*
*   incstack
*
* INPUT
*
* OUTPUT
*
* RETURNS
*
* AUTHOR
*
*   POV-Ray Team
*
* DESCRIPTION
*
*   -
*
* CHANGES
*
*   -
*
******************************************************************************/

void incstack(ISTACK *istk)
{
	if(++istk->top_entry >= istk->max_entries)
	{
		istk->top_entry--;
		Increase_Counter(stats[Istack_overflows]);
	}
}

/*****************************************************************************
*
* FUNCTION
*
*   Default_UVCoord
*
* INPUT
*
*   Object  - Pointer to blob structure
*   Inter   - Pointer to intersection
*
* OUTPUT
*
*
* RETURNS
*
* AUTHOR
*
*   Nathan Kopp
*
* DESCRIPTION
*   This is used as a default UVCoord function for objects where UVCoordinates
*   are not defined.  It instead returns the XY coordinates of the intersection.
*
* CHANGES
*
*
******************************************************************************/

void Default_UVCoord(UV_VECT Result, OBJECT * /*Object*/, INTERSECTION *Inter)
{
  Result[U] = Inter->IPoint[X];
  Result[V] = Inter->IPoint[Y];
}

END_POV_NAMESPACE
