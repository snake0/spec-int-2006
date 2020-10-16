/*
 * specinvoke - run and accurately time lists of commands
 * Copyright(C) 1998-2003 Standard Performance Evaluation Corporation
 *
 * specinvoke.h: Program-wide definitions and headers
 *
 * $Id: specinvoke.h 3478 2005-12-13 00:22:38Z cloyce $
 */

#include "config.h"

/* The text which will be substituted with the copy number in the executed
 * command
 */
#define COPYNUMVAR "$SPECCOPYNUM"
#define OLDCOPYNUMVAR "$SPECUSERNUM"

/* The text which will be substituted with the bind value in the executed
 * command
 */
#define BINDSTRVAR "$BIND"

#if defined(PERFMON)
# include "specinvoke_pm.h"
#endif

#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#ifdef HAVE_SYS_TIMEB_H
#include <sys/timeb.h>
#endif

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

#ifdef _WIN32
#include <windows.h>
#include <direct.h>
#include "getopt.h"
#define HAVE_NON_BLOCKING_WAIT
#define chdir _chdir
typedef long pid_t;
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_WAITPID
#define HAVE_NON_BLOCKING_WAIT
#endif

#if !defined(HAVE_STRERROR)
#define STRERROR(x) ""
#else
#define STRERROR(x) strerror(x)
#endif

/* longs should handle 32 bits */
typedef struct spec_time_s {
    time_t sec;
    time_t nsec; /* NOTE: These are nanoseconds */
} spec_time_t;

typedef struct command_info_s {
    int  num;
    char *dir;
    char *input;
    char *output;
    char *err;
    char *cmd;
} command_info_t;

typedef struct copy_info_s {
    unsigned int num;
    spec_time_t  start_time;
    spec_time_t  stop_time;
    pid_t        pid;
    int          index;
    char        *bind;
    char        *dir;
} copy_info_t;

typedef struct specinvoke_state_s {
  char **invoke_args;
  char **command_ptr;
  char  *shell;
  int    redir;    /* Whether or not to do I/O redirection ourselves */
  int    dry_run;  /* Fake the commands? */
  time_t delay;    /* Delay (in milliseconds) between starting new jobs */
  enum { CLOSE, NUL, ZEROFILE } no_stdin;
  FILE *outfp;
#if defined(PERFMON)
  pm_info_t pm_state;
#endif
} specinvoke_state_t;

/* Prototypes */
void  usage(char *name, specinvoke_state_t *state);
pid_t invoke(copy_info_t *ui, command_info_t *ci, char **env,
	     specinvoke_state_t *si);
pid_t dry_invoke(copy_info_t *ui, command_info_t *ci, char **env,
		 specinvoke_state_t *si);
void  gettime(spec_time_t *t);
void  subtime(spec_time_t *a, spec_time_t *b, spec_time_t *c);
long  wait_for_next (long *status, int nowait);
void  init_state(specinvoke_state_t *si);
void  init_os(int num);
void  reinit_os();
void  cleanup_os();
char *specstrstr(char *haystack, char *needle);
int   specstrncmp(char *a, char *b, int len);
int   specstrncpy(char *dst, char *src, int len);
int   specmillisleep(time_t milliseconds);
void  specinvoke_exit(int code, specinvoke_state_t *state);
char *make_number_buf(unsigned num);
char *sub_strings(char *src, char *find, char *replace);

#if defined(PERFMON)
/* Protoypes for performance monitor hooks.  See pm_stub/dummy.c for info. */
void pm_init(pm_info_t *pm_state);
void pm_getopt(int *argc, char ***argv, char ***env, pm_info_t *pm_state);
void pm_usage(pm_info_t *pm_state);
void pm_exit(int exit_code, pm_info_t *pm_state);
void pm_pre_run(command_info_t *command_info, copy_info_t *copy_info,
		pm_info_t *pm_state);
void pm_post_run(command_info_t *command_info, copy_info_t *copy_info,
		 pm_info_t *pm_state);
void pm_pre_iter(command_info_t *command_info, copy_info_t *copy_info,
		 pm_info_t *pm_state);
void pm_post_iter(command_info_t *command_info, copy_info_t *copy_info,
		  pm_info_t *pm_state);
void pm_pre_spawn(command_info_t *command, copy_info_t *copy,
		   pm_info_t *pm_state);
void pm_pre_exec(command_info_t *command, copy_info_t *copy,
		 pm_info_t *pm_state);
void pm_post_spawn(pid_t pid, command_info_t *command, copy_info_t *copy,
		   pm_info_t *pm_state);
void pm_post_exit(pid_t pid, copy_info_t *copy, pm_info_t *pm_state);
#endif
