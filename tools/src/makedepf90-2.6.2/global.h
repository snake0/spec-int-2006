/* 
 * Copyright (C) 2000 Erik Edelmann <eedelman@pcu.helsinki.fi>
 *
 *     This program is free software;  you  can  redistribute  it
 *     and/or modify it under the terms of the GNU General Public
 *     License as published  by  the  Free  Software  Foundation;
 *     either  version  2 of the License, or (at your option) any
 *     later version.
 *
 *     This program is distributed in the hope that  it  will  be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS  FOR  A  PARTICULAR
 *     PURPOSE.   See  the  GNU  General  Public License for more
 *     details.
 *
 *     You should have received a copy of the GNU General  Public
 *     License along with this program; if not, write to the Free
 *     Software Foundation, Inc., 59  Temple  Place,  Suite  330,
 *     Boston, MA  02111-1307  USA
 */
#ifndef GLOBAL_H_
#define GLOBAL_H_

#include "config.h"
#include "list.h"

#ifndef HAVE_STRCASECMP
    int strcasecmp (const char *s1, const char *s2)
#endif

#ifdef DEBUG 
# define DEBUG_CODE(x)  x
#else
# define DEBUG_CODE(x)
#endif

#if defined(DEBUG) && defined(__GNUC__)
# include <stdio.h>
# define DEBUG_PRINT(fmt, args...) \
    fprintf(stderr, "DEBUG: %s: ", __FUNCTION__); fprintf(stderr, fmt, ## args);
#else
# define DEBUG_PRINT(fmt, x)
#endif


#define MODFILE_NAME_LEN 256 
#define MODFILE_FMT_DEFAULT "%f.o"
#define MAX_STRING_LEN 8192    
#define INCLUDE_RECURSION_LIMIT 15
#define RULE_LENGTH 256
#define LINK_RULE_DEFAULT "$(FC) -o $@ $(FFLAGS) $(LDFLAGS) $(FOBJ) $(LIBS)"

typedef enum {false = 0, true = 1} bool;

typedef enum {SUFFIX, FIXED, FREE, UNKNOWN} SourceFmt;

typedef struct {
    bool warn;          /* Write "extra" warnings */
    char *modfile_fmt;  /* format of '*.mod-file' names */
    List *ignore_mods;   /* Modules to be ignored */
    SourceFmt src_fmt;
    bool create_obj;    /* Create list of *.o-files and linking rule for the 
                         * executable */
    char *exe_name;     /* Name of the executable */
    char *link_rule;    
    bool coco;          /* Look for coco set-files */
    char *obj_dir;      /* Assume object files are stored in this directory */
    bool src_dep;       /* List the source file in the dependencys */
    bool spec;          /* Generate Perl-style deps for SPEC benchmarks */
} Options;

extern Options options;


#endif /* GLOBAL_H_ */
