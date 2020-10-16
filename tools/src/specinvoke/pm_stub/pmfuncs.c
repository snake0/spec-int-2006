/*
 * specinvoke - run and accurately time lists of commands
 * Copyright(C) 1998-2003 Standard Performance Evaluation Corporation
 *
 * pmfuncs.c: Placeholders and semi-documentation for the performance monitor
 *            hooks.
 *
 * $Id: pmfuncs.c 3339 2005-11-08 23:09:40Z cloyce $
 */

#if defined(PERFMON)

#include "config.h"
#include "specinvoke.h"
#include <stdio.h>

/* Function to initialize the pm_state structure. */
void pm_init(pm_info_t *pm_state) {
  /* When this is called, the pm_state structure (however you've defined it
   * in specinvoke_pm.h) is guaranteed to exist, but that's all.
   */

  /* This example is stupidly simple. */
  pm_state->placeholder = 0;

  fprintf(stderr, "pm_init(%p) called!\n", (void *)pm_state);
  fprintf(stderr, "pm_state placeholder value is %d\n", pm_state->placeholder);
}

/* Function for processing command-line arguments. */
void pm_getopt(int *argc, char ***argv, char ***env, pm_info_t *pm_state) {
  /* If you need to process some command line options, do it here.
   *
   * You MUST ensure that argc and argv are both munged in such a way that
   * a subsequent call to getopt (in specinvoke.c) will be able to work
   * properly.
   */
  int i;
  char **curr;

  fprintf(stderr, "pm_getopt(%p, %p, %p, %p) called!\n",
	  (void *)argc, (void *)argv, (void *)env, (void *)pm_state);
  fprintf(stderr, "Got %d args:\n", *argc);
  i = 0;
  curr = *argv;
  while (curr && *curr) {
    fprintf(stderr, " #%d: %s\n", i++, *curr);
    curr++;
  }
  if (*env != 0) {
    fprintf(stderr, "I've got an environment, too!  See:\n");
    i = 0;
    curr = *env;
    while (curr && *curr) {
      fprintf(stderr, " #%d: %s\n", i++, *curr);
      curr++;
    }
  }
  pm_state->placeholder++;
  fprintf(stderr, "pm_state placeholder value is %d\n", pm_state->placeholder);
}

/* PM-specific usage message */
void pm_usage(pm_info_t *pm_state) {
  /* This is where you should print out command line flags that are
   * interpreted by pm_getopt above.  The output from this is printed
   * immediately after the options list from the main usage() routine.
   */
  fprintf(stderr, "pm_usage(%p) called!\n", (void *)pm_state);
  pm_state->placeholder++;
  fprintf(stderr, "pm_state placeholder value is %d\n", pm_state->placeholder);
}

/* This is called immediately before an exit any time
 * after pm_init has been called */
void pm_exit(int exit_code, pm_info_t *pm_state) {
  fprintf(stderr, "pm_exit(%d, %p) called!\n", exit_code, (void *)pm_state);
  pm_state->placeholder = -1;
  fprintf(stderr, "pm_state placeholder value is %d\n", pm_state->placeholder);
}

/* Pre-run hook; this is called before any runs are done */
void pm_pre_run(command_info_t *command_info, copy_info_t *copy_info,
		pm_info_t *pm_state) {
  fprintf(stderr, "pm_pre_run(%p, %p, %p) called!\n",
	  (void *)command_info, (void *)copy_info, (void *)pm_state);
  pm_state->placeholder++;
  fprintf(stderr, "pm_state placeholder value is %d\n", pm_state->placeholder);
}

/* Post-run hook; this is called after all iterations are finished */
void pm_post_run(command_info_t *command_info, copy_info_t *copy_info,
		 pm_info_t *pm_state) {
  fprintf(stderr, "pm_post_run(%p, %p, %p) called!\n",
	  (void *)command_info, (void *)copy_info, (void *)pm_state);
  pm_state->placeholder++;
  fprintf(stderr, "pm_state placeholder value is %d\n", pm_state->placeholder);
}

/* Pre-iteration hook; this is called at the start of each iteration */
void pm_pre_iter(command_info_t *command_info, copy_info_t *copy_info,
		 pm_info_t *pm_state) {
  fprintf(stderr, "pm_pre_iter(%p, %p, %p) called!\n",
	  (void *)command_info, (void *)copy_info, (void *)pm_state);
  pm_state->placeholder++;
  fprintf(stderr, "pm_state placeholder value is %d\n", pm_state->placeholder);
}

/* Post-iteration hook; this is called at the end of each iteration */
void pm_post_iter(command_info_t *command_info, copy_info_t *copy_info,
		  pm_info_t *pm_state) {
  fprintf(stderr, "pm_post_iter(%p, %p, %p) called!\n",
	  (void *)command_info, (void *)copy_info, (void *)pm_state);
  pm_state->placeholder++;
  fprintf(stderr, "pm_state placeholder value is %d\n", pm_state->placeholder);
}

/* Pre-invoke hook; this is called immediately before the child worker is
 * spawned */
void pm_pre_spawn(command_info_t *command_info, copy_info_t *copy_info,
		  pm_info_t *pm_state) {
  fprintf(stderr, "pm_pre_spawn(%p, %p, %p) called!\n",
	  (void *)command_info, (void *)copy_info, (void *)pm_state);
  pm_state->placeholder++;
  fprintf(stderr, "pm_state placeholder value is %d\n", pm_state->placeholder);
}

/* This is called BY THE CHILD PROCESS immediately before calling exec()
 * Because of the semantics of CreateProcess(), this hook is not available
 * under Windows
 */
void pm_pre_exec(command_info_t *command_info, copy_info_t *copy_info,
		 pm_info_t *pm_state) {
  fprintf(stderr, "pm_pre_exec(%p, %p, %p) called!\n",
	  (void *)command_info, (void *)copy_info, (void *)pm_state);
  pm_state->placeholder++;
  fprintf(stderr, "pm_state placeholder value is %d\n", pm_state->placeholder);
}

/* Post-invoke hook; this is called immediately after fork() returns
 * with the PID of the child process
 */
void pm_post_spawn(pid_t pid, command_info_t *command_info,
		   copy_info_t *copy_info, pm_info_t *pm_state) {
  fprintf(stderr, "pm_post_spawn(%lu, %p, %p, %p) called!\n",
	  (unsigned long) pid, (void *)command_info, (void *)copy_info,
	  (void *)pm_state);
  pm_state->placeholder++;
  fprintf(stderr, "pm_state placeholder value is %d\n", pm_state->placeholder);
}

/* Post-exit hook; this is called after a child exits */
void pm_post_exit(pid_t pid, copy_info_t *copy_info, pm_info_t *pm_state) {
  fprintf(stderr, "pm_post_spawn(%lu, %p, %p) called!\n",
	  (unsigned long) pid, (void *)copy_info, (void *)pm_state);
  pm_state->placeholder++;
  fprintf(stderr, "pm_state placeholder value is %d\n", pm_state->placeholder);
}

#endif

