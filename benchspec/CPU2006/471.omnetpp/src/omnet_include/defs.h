//==========================================================================
//   DEFS.H - header for
//                             OMNeT++
//            Discrete System Simulation in C++
//
//
//  Defines of global interest
//
//==========================================================================

/*--------------------------------------------------------------*
  Copyright (C) 1992-2003 Andras Varga

  This file is distributed WITHOUT ANY WARRANTY. See the file
  `license' for details on this and other legal matters.
*--------------------------------------------------------------*/

#ifndef __DEFS_H
#define __DEFS_H

#include <stddef.h>   // size_t
#if defined(SPEC_CPU)
#include <cmath>     // HUGE_VAL
#else
#include <math.h>     // HUGE_VAL
#endif

// OMNeT++ version -- must match NEDC_VERSION in nedc source!
#define OMNETPP_VERSION 0x0203

//=== Windows DLL IMPORT/EXPORT stuff
#ifndef __WIN32__
#  if defined(_WIN32) || defined(WIN32)
#    define __WIN32__
#  endif
#endif

// OPP_DLLIMPORT/EXPORT are empty if not needed
#if defined(__WIN32__) && defined(WIN32_DLL)
#  define OPP_DLLIMPORT  __declspec(dllimport)
#  define OPP_DLLEXPORT  __declspec(dllexport)
#else
#  define OPP_DLLIMPORT
#  define OPP_DLLEXPORT
#endif

// SIM_API, ENVIR_API etc are also empty if not needed
#ifdef BUILDING_SIM
#  define SIM_API  OPP_DLLEXPORT
#else
#  define SIM_API  OPP_DLLIMPORT
#endif

// we need this because cenvir.h is in our directory
#ifdef BUILDING_ENVIR
#  define ENVIR_API  OPP_DLLEXPORT
#else
#  define ENVIR_API  OPP_DLLIMPORT
#endif


//=== NULL
#ifndef NULL
#define NULL ((void*)0)
#endif

// obsolete:
// #define NO(cXX)   ((cXX *)NULL)

//=== other common defines

#ifndef PI
#define PI        3.141592653589793
#endif

#ifndef Min
#define Min(a,b)     ( (a)<(b) ? (a) : (b) )
#define Max(a,b)     ( (a)>(b) ? (a) : (b) )
#endif

#define sgn(x)       ((x)==0 ? 0 : ((x)<0 ? -1 : 1))

#ifndef NDEBUG
#define ASSERT(expr)  \
  ((void) ((expr) ? 0 : \
           (opp_error("ASSERT: condition %s false, %s line %d", \
                             #expr, __FILE__, __LINE__), 0)))
#else
#define ASSERT(expr)  ((void)0)
#endif


/**
 * Modelled time.
 */
typedef double       simtime_t;

#define MAXTIME      HUGE_VAL


//
// backwards compatibility defines
//
#define isA()          className()
// cQueue:
#define insertHead(a)  insert(a)
#define peekTail()     tail()
#define peekHead()     head()
#define getTail()      pop()
// cSimulation
#define lastModuleIndex() lastModuleId()

// following ones became inner classes:
#define cKSplitIterator       cKSplit::Iterator
#define sGrid                 cKSplit::Grid
#define cQueueIterator        cQueue::Iterator
#define cLinkedListIterator   cLinkedList::Iterator
#define sXElem                cPar::ExprElem
#define cMessageHeapIterator  cMessageHeap::Iterator
#define sTopoLink             cTopology::Link
#define sTopoLinkIn           cTopology::LinkIn
#define sTopoLinkOut          cTopology::LinkOut
#define sTopoNode             cTopology::Node

//
// memory mgmt functions for void* pointers  (used by cLinkedList and cPar)
//

/**
 * Prototype for functions that are called by some objects (cPar, cLinkedList)
 * to free up user-defined data structures.
 * @ingroup EnumsTypes
 */
typedef void (*VoidDelFunc)(void *);

/**
 * Prototype for functions that are called by some objects (cPar, cLinkedList)
 * to duplicate user-defined data structures.
 * @ingroup EnumsTypes
 */
typedef void *(*VoidDupFunc)(void *);

//
// used by cPar expressions
//

/**
 * Prototype for mathematical functions that can be used in reverse
 * Polish expressions (see ExprElem).
 * @ingroup EnumsTypes
 */
typedef double (*MathFunc)(...);

/**
 * Prototype for mathematical functions taking no arguments
 * that can be used in reverse Polish expressions (see ExprElem).
 * @ingroup EnumsTypes
 */
typedef double (*MathFuncNoArg)();

/**
 * Prototype for mathematical functions taking one argument
 * that can be used in reverse Polish expressions (see ExprElem).
 * @ingroup EnumsTypes
 */
typedef double (*MathFunc1Arg)(double);

/**
 * Prototype for mathematical functions taking two arguments
 * that can be used in reverse Polish expressions (see ExprElem).
 * @ingroup EnumsTypes
 */
typedef double (*MathFunc2Args)(double,double);

/**
 * Prototype for mathematical functions taking three arguments
 * that can be used in reverse Polish expressions (see ExprElem).
 * @ingroup EnumsTypes
 */
typedef double (*MathFunc3Args)(double,double,double);

/**
 * Prototype for mathematical functions taking four arguments
 * that can be used in reverse Polish expressions (see ExprElem).
 * @ingroup EnumsTypes
 */
typedef double (*MathFunc4Args)(double,double,double,double);

#endif

