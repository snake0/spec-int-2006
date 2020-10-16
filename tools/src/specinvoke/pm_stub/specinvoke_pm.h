/*
 * specinvoke - run and accurately time lists of commands
 * Copyright(C) 1998-2003 Standard Performance Evaluation Corporation
 * All Rights Reserved
 *
 * specinvoke_pm.h: Performance monitor-specific structure definitions and
 *                  Function prototypes
 *
 * $Id: specinvoke_pm.h 2522 2005-04-07 05:54:49Z cloyce $
 */

#ifndef _SPECINVOKE_PM_H

#define _SPECINVOKE_PM_H

/* A descriptive string that tells what this specially instrumented version
 * is good for.  Run 'specinvoke_pm -v' to see it.
 */
#define PM_DESC "  Stubs only; no actual monitoring available.\n"

/* The performance monitor probably has some state that you'd like to save.
 * A pointer to this structure is passed around to _all_ of the pm-related
 * functions.  Within it, you can do whatever you want.
 *
 * Since this is the dummy "do-nothing" version, it just has a placeholder.
 */
typedef struct pm_info_s {
  int placeholder;
} pm_info_t;

#endif
