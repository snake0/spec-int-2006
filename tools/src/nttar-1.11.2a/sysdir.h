#ifdef __MSDOS__
#include "msd_dir.h"
#define NLENGTH(direct) ((direct)->d_namlen)

#else /* not __MSDOS__ */
#ifdef WINDOWSNT
#include "wnt_dir.h"
#define NLENGTH(direct) ((direct)->d_namlen)

#else /* not WINDOWSNT */
#if defined(DIRENT) || defined(_POSIX_VERSION)
#include <dirent.h>
#define NLENGTH(direct) (strlen((direct)->d_name))
#else /* not (DIRENT or _POSIX_VERSION) */
#define dirent direct
#define NLENGTH(direct) ((direct)->d_namlen)
#ifdef SYSNDIR
#include <sys/ndir.h>
#endif /* SYSNDIR */
#ifdef SYSDIR
#include <sys/dir.h>
#endif /* SYSDIR */
#ifdef NDIR
#include <ndir.h>
#endif /* NDIR */
#endif /* DIRENT or _POSIX_VERSION */

#endif /* not WINDOWSNT */
#endif /* not __MSDOS__ */
