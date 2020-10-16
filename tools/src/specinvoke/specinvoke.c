/*
 * specinvoke - run and accurately time lists of commands
 * Copyright(C) 1998-2003 Standard Performance Evaluation Corporation
 * All Rights Reserved
 *
 * specinvoke.c: The platform-independent bits
 *
 * $Id: specinvoke.c 4205 2006-05-11 15:24:04Z cloyce $
 */

#include "specinvoke.h"


/* These are defined in the platform-specific modules */
extern char **invoke_args;
extern char **command_ptr;
extern char  *shell;

static char *svnrev    = "$Revision: 4205 $";
static char *rcsid     = "$Id: specinvoke.c 4205 2006-05-11 15:24:04Z cloyce $";


int main (int argc, char *argv[], char *env[]) {
    FILE *fp;
    char *cname = "speccmds.cmd";
    char *resultname = NULL;
    char *dir = NULL;
    char *outputname = NULL;
    char *errname = NULL;
    int iterations = 1;
    int i, c;
    int concurrent = 0;
    int run;
    int version = 0, line = 0, active_copies = 0;
    int end_of_file;
    int child_error = 0, total_child_error = 0;
    int child_return = 0;
    spec_time_t whole_start_time, whole_stop_time, run_start_time, run_stop_time;
    spec_time_t whole_elapsed_time, run_elapsed_time;
    int num_commands, num_copies, cur_copy;
    char command[8192];
    command_info_t *command_info;
    copy_info_t *copy_info;
    specinvoke_state_t state; /* General state and settings info */

    char *versionid = (char *)malloc((strlen(svnrev) - 10) * sizeof(char));
    char *tmpptr;
    strcpy(versionid, svnrev + 11);
    tmpptr = strrchr(versionid, ' ');
    if (tmpptr) *tmpptr = 0;

    /* Initialize a couple of non-platform-specific values */
    state.redir   = 1;
    state.dry_run = 0;
    state.delay   = (time_t)0;
    state.no_stdin= CLOSE;
    state.outfp   = stdout;
    init_state(&state);

#if defined(PERFMON)
    /* Init the performance monitor state */
    pm_init(&state.pm_state);

    /* Allow the perfmon code to process argc and argv first, because our
     * getopt trashes them both.
     */
    pm_getopt(&argc, &argv, &env, &state.pm_state);
#endif

    /* Parse arguments */
    while ((c = getopt(argc, argv, "vnd:i:o:e:f:c:s:A?hErS:NZC")) != EOF) {
	switch (c) {
	    case 'v': version++;                     break;
	    case 'n': state.dry_run++;               break;
	    case 'd': dir         = optarg;          break;
	    case 'i': iterations  = atoi(optarg);    break;
	    case 'o': outputname  = optarg;          break;
	    case 'e': errname     = optarg;          break;
	    case 'f': cname       = optarg;          break;
	    case 'c': concurrent  = atoi(optarg);    break;
	    case 's': state.shell = optarg;          break;
            case 'A': specinvoke_exit(0, &state);    break;
	    case '?':
	    case 'h': usage(argv[0], &state);        break;
	    case 'E': child_return = 1;              break;
	    case 'r': state.redir = 1 - state.redir; break;
	    case 'S': state.delay = atoi(optarg);    break;
	    case 'N': state.no_stdin = NUL;          break;
	    case 'Z': state.no_stdin = ZEROFILE;     break;
	    case 'C': state.no_stdin = CLOSE;        break;
	}
    }

    if (argv[optind] != NULL)
	cname = argv[optind++];

    if (version) {
	printf ("Version: %s\n", versionid);
	if (version > 1) {
	    printf ("  RCSID: %s\n", rcsid);
	}
#if defined(PERFMON)
	printf ("  Performance monitoring version:\n");
	puts (PM_DESC);
#endif
	specinvoke_exit(0, &state);
    }

    if (iterations < 1) {
	printf ("Iterations must be >= 1!\n");
	usage(argv[0], &state);
    }

    if (!state.dry_run) {
	if (dir != NULL && chdir(dir)) {
	    printf("Can't chdir to '%s': %s(%d)\n",
			    dir, STRERROR(errno), errno);
	    specinvoke_exit(1, &state);
	}

	if (errname != NULL) {
	    if (freopen(errname, "w", stderr) == NULL) {
		printf("Can't open new stderr '%s': %s(%d)\n",
				errname, STRERROR(errno), errno);
		specinvoke_exit (1, &state);
	    }
	}
	if (outputname) {
	    if (freopen(outputname, "w", stdout) == NULL) {
		printf("Can't open new stdout '%s': %s(%d)\n",
				outputname, STRERROR(errno), errno);
		specinvoke_exit (1, &state);
	    }
	}

        /* Make a copy of cname and change ".cmd" (if any) to ".out". */
        i = strlen(cname);
        resultname = (char *)malloc(sizeof(char) * (i + 5));
        if (resultname == NULL) {
            printf ("Can't allocate memory for a copy of the command file name\n");
            specinvoke_exit (1, &state);
        }
        specstrncpy(resultname, cname, i);
        *(resultname + i) = '\0';
        if (i > 3 && !specstrncmp(resultname + i - 4, ".cmd", 4)) {
            /* Yay! Just replace the .cmd with .out */
            specstrncpy(resultname + i - 4, ".out", 4);
        } else {
            /* Append it */
            strcat(resultname, ".out");
        }
        state.outfp = fopen(resultname, "w");
        if (state.outfp == NULL) {
            printf ("Can't open result output file '%s': %s(%d)\n", resultname,
                    STRERROR(errno), errno);
            specinvoke_exit (1, &state);
        }
    }


    fp = fopen (cname, "r");
    if (fp == NULL) {
	printf ("Can't open command file '%s': %s(%d)\n", cname, STRERROR(errno), errno);
	specinvoke_exit (1, &state);
    }

    /* Find out how many commands we are going to run */
    num_commands = 0;
    num_copies    = 0;
    while (fgets(command, sizeof(command), fp) != NULL) {
	char *ptr = command;
	if (strcmp(command, "__END__") == 0)
	    break;
	while (*ptr == ' ' || *ptr == '\t')
	    ptr++;
	if (*ptr == '-') {
	  ptr++;
	  if (*ptr == 'C' || *ptr == 'u') {
	    num_copies++;
	    continue;
	  } else if (*ptr == 'b') {
            /* Ignore it; it's not a command and shouldn't increase the number
             * of copies, either.
             */
	    continue;
	  } else if (*ptr == 'S') {
	    state.delay = atoi(ptr+1);
	    continue;
	  }
	}
	if (*ptr == '#' || *ptr == '\0')
	    continue;
	num_commands++;
    }

    /* No -C lines?  No problem... still run at least one instance */
    if (num_copies <= 0) {
      num_copies = 1;
    }

    /* Make sure that we really are at the end of file */
    if (!feof(fp)) {
	printf ("Error counting entries in command file '%s': %s(%d)\n", 
		cname, STRERROR(errno), errno);
	specinvoke_exit (1, &state);
    }


    if (concurrent)
	num_copies = concurrent;

    if (num_copies <= 0) {
	printf ("No '-C' lines in command file or '-c' flag on command line!\n");
	specinvoke_exit (1, &state);
    }

    if (num_commands <= 0) {
	printf ("No commands to issue!\n");
	specinvoke_exit (1, &state);
    }

    command_info = (command_info_t *)malloc(num_commands * sizeof(struct command_info_s));
    memset(command_info, 0, num_commands * sizeof(struct command_info_s));
    copy_info = (copy_info_t *)malloc(num_copies * sizeof(struct copy_info_s));
    memset(copy_info, 0, num_copies * sizeof(struct copy_info_s));
    for (i = 0; i < num_copies; i++) {
	copy_info[i].num = i;
    }
    for (i = 0; i < num_commands; i++) {
	command_info[i].num = i;
    }
    rewind(fp);

    i = 0;
    cur_copy = 0;
    while (fgets(command, sizeof(command), fp) != NULL) {
	int len;
	int non_command_line = 0;
	char *ptr = command;
	line++;
	if (strcmp(command, "__END__") == 0)
	    break;
	while (*ptr == ' ' || *ptr == '\t')
	    ptr++;
	if (*ptr == '#' || *ptr == '\0')
	    continue;
	len = strlen(command);
	while ((len > 0) && 
	       (((command[len-1] == '\n') || (command[len-1] == '\r')) ||
		((command[len-1] == ' ') || (command[len-1] == '\t'))))
	    command[--len] = 0;

	command_info[i].dir = 
	command_info[i].input = 
	command_info[i].output = 
	command_info[i].err = NULL;

	while (*ptr == '-') {
	    char *arg=NULL;
	    int option = *++ptr;

            ptr++;
	    if (option != 'b' && *ptr != ' ') {
		printf ("Illegally formatted option '%c' on line %d (missing space)\n", 
			    option, line);
		specinvoke_exit (1, &state);
	    }
	    while (*ptr == ' ' || *ptr == '\t') {
		ptr++;
	    }
	    arg = ptr++;
	    while (*ptr != ' ' && *ptr != '\t' && *ptr != '\0') {
		ptr++;
	    }
	    if (option != 'b' && *ptr != '\0')
		*ptr++ = '\0';
	    switch (option) {
		case 'c': command_info[i].dir       = strdup(arg); break;
		case 'i': command_info[i].input     = strdup(arg); break;
		case 'o': command_info[i].output    = strdup(arg); break;
		case 'e': command_info[i].err       = strdup(arg); break;
		case 'u': 
		case 'C': 
		    non_command_line = 1;
		    if (concurrent)
			break;
		    if (cur_copy >= num_copies) {
			printf ("-C is only allowed once at the beginning of a line!\n");
			specinvoke_exit (1, &state);
		    }
		    copy_info[cur_copy++].dir = strdup(arg);
                    break;
		case 'b': 
		    non_command_line = 1;
		    if (concurrent)
			break;
		    if (cur_copy >= num_copies) {
			printf ("-b is only allowed once at the beginning of a line!\n");
			specinvoke_exit (1, &state);
		    }
		    copy_info[cur_copy].bind = strdup(arg);
                    break;
	        case 'S':
		    non_command_line = 1;
		    break;
		default:
		    printf ("Illegal option '%c' on line %d\n", 
				option, line);
		    specinvoke_exit (1, &state);
	    }
	}
	if (!non_command_line) {
	    command_info[i].cmd = strdup(ptr);
	    i++;
	}
    }
    if (i != num_commands) {
	printf("Error: Too few commands read from the command file; %d read, should be %d\n", i, num_commands);
        specinvoke_exit (1, &state);
    }

    if (!feof(fp)) {
	printf ("Error reading entries in command file '%s': %s(%d)\n", 
		cname, STRERROR(errno), errno);
	specinvoke_exit (1, &state);
    }

    fclose(fp);

    init_os(num_commands * num_copies + 1); /* Total commands + slop */


    if (!state.dry_run) {
	fprintf (state.outfp, "running commands in %s %d times\n", 
		 cname, iterations);

#if defined(PERFMON)
	pm_pre_run(command_info, copy_info, &state.pm_state);
#endif

	gettime(&whole_start_time);
	fprintf (state.outfp, "runs started at %u, %u, %s", 
		 (unsigned int) whole_start_time.sec, 
		 (unsigned int) whole_start_time.nsec,
		 ctime(&whole_start_time.sec));
    }

    /* Perform actual run */
    for (run = 1; run <= iterations; run++) {
	long status;
	int line;
	int cindex = 0;
	pid_t pid;

	reinit_os();

	line = 0;
	end_of_file = 0;

	if (!state.dry_run) {
#if defined(PERFMON)
	  pm_pre_iter(command_info, copy_info, &state.pm_state);
#endif
	  gettime(&run_start_time);
	  fprintf (state.outfp,
                   "run %d started at %u, %u, %s", run, 
		   (unsigned int) run_start_time.sec, 
		   (unsigned int) run_start_time.nsec,
		   ctime(&run_start_time.sec));
	} else {
	  if (num_copies > 1) {
	    printf("# Fake output for rate runs will not necessarily clearly represent the order\n");
	    printf("# that workloads would be run in a real run.\n");
	  }
	  if (state.dry_run < 2) {
	    printf("# Use another -n on the command line to see chdir commands\n");
	  }
	}

	for (i = 0; i < num_copies; i++) {
	    pid_t pid;
	    if (cindex >= num_commands)
		break;
	    pid = invoke( &copy_info[i], &command_info[cindex], env, &state );
	    if (concurrent)
		cindex++;
	    copy_info[i].index = 1;
	    copy_info[i].pid = pid;
	    if (state.delay > 0) {
	      if (state.dry_run) {
		printf ("# sleep for %ld milliseconds\n", (long)state.delay);
	      } else {
		specmillisleep(state.delay);
	      }
	    }
	}
	if (concurrent)
	    copy_info[0].index = cindex;

	active_copies = num_copies;
	while (active_copies) {
	    int found;
	    int cindex;
	    spec_time_t stop_time;
	    if (state.dry_run) {
		status = 0;
		pid = copy_info[active_copies-1].pid;
	    } else {
		pid = wait_for_next(&status, 0);
	    }
	    if (pid == -1) {
	      fprintf (stderr, "wait_for_next returned %d: %s (%d)\n",
		       pid, STRERROR(errno), errno);
	      specinvoke_exit (1, &state);
	    }
	    if (status != 0)
		child_error++;
	    gettime(&stop_time);
	    found = 0;
	    for (i = 0; i < num_copies; i++) {
		if (pid == copy_info[i].pid) {
		    cindex = (concurrent)?0:i;
		    found = 1;
		    if (!state.dry_run) {
			spec_time_t elapsed;
			subtime(&stop_time, &copy_info[i].start_time, 
				&elapsed);
#if defined(PERFMON)
			pm_post_exit(pid, &copy_info[i], &state.pm_state);
#endif
			fprintf (state.outfp,
                                 "child finished: %u, %u, %u, sec=%u, nsec=%u, pid=%ld, rc=%u\n",
				 copy_info[i].num,
				 (unsigned int) stop_time.sec, 
				 (unsigned int) stop_time.nsec, 
				 (unsigned int) elapsed.sec,
				 (unsigned int) elapsed.nsec,
				 (long)pid, (unsigned int) status);
		    }
		    if (copy_info[cindex].index < num_commands) {
		      pid = invoke (&copy_info[i], 
				    &command_info[copy_info[cindex].index], 
				    env, &state );
		      copy_info[i].pid = pid;
		      copy_info[cindex].index++;
		    } else {
			copy_info[i].pid = -1;
			active_copies--;
		    }
		    break;
		}
	    }
	    if (!found) {
		fprintf (stderr, "child with unknown pid %d exited!\n", pid);
		specinvoke_exit (1, &state);
	    }
	}
#if defined(PERFMON)
	pm_post_iter(command_info, copy_info, &state.pm_state);
#endif

	if (state.dry_run)
	    continue;

	if (child_error) {
	    total_child_error += child_error;
	    printf ("error: %d children finished with errors\n", child_error);
	}

	gettime(&run_stop_time);
	fprintf (state.outfp,
                 "run %d finished at: %u, %u, %s", run, 
		 (unsigned int) run_stop_time.sec, 
		 (unsigned int) run_stop_time.nsec, ctime(&run_stop_time.sec));
	subtime(&run_stop_time, &run_start_time, &run_elapsed_time);
	fprintf (state.outfp,
                 "run %d elapsed time: %u, %u, %u.%09u\n", run, 
		 (unsigned int) run_elapsed_time.sec,
		 (unsigned int) run_elapsed_time.nsec,
		 (unsigned int) run_elapsed_time.sec,
		 (unsigned int) run_elapsed_time.nsec);
    }
    if (state.dry_run) {
      specinvoke_exit (0, &state);
    }
    gettime(&whole_stop_time);
    fprintf (state.outfp,
             "runs finished at %u, %u, %s", (unsigned int) whole_stop_time.sec, 
	     (unsigned int) whole_stop_time.nsec, ctime(&whole_stop_time.sec));
    subtime(&whole_stop_time, &whole_start_time, &whole_elapsed_time);
    fprintf (state.outfp,
             "runs elapsed time: %u, %u, %u.%09u\n", 
	     (unsigned int) whole_elapsed_time.sec,
	     (unsigned int) whole_elapsed_time.nsec,
	     (unsigned int) whole_elapsed_time.sec,
	     (unsigned int) whole_elapsed_time.nsec);
    if (total_child_error) {
	fprintf (state.outfp,
                 "error: a total of %d children finished with errors\n", 
		 total_child_error);
    }
    fflush(state.outfp);

#if defined(PERFMON)
    pm_post_run(command_info, copy_info, &state.pm_state);
#endif

    cleanup_os();

    /* Bye bye */
    if (child_return) {
	return !!total_child_error;
    }
    return 0;
}


/* 
 * Subtract one time from another, used to calculate elapsed times.
 * This does not handle any edge conditions at all 
 */
void subtime(spec_time_t *a, spec_time_t *b, spec_time_t *c) {
    c->nsec = a->nsec - b->nsec;
    c->sec = a->sec - b->sec;
    if (c->nsec < 0) {
	c->sec--;
	c->nsec += 1000000000;
    }
}

/* 
 * Print out a usage message 
 */
void usage(char *name, specinvoke_state_t *state) {
    printf ("Usage: %s [options] [instructionfile]\n"
	    "           -i #            iterations\n"
	    "           -c #            concurrent processes (in command file, overrides -C\n"
            "                                             and requires -c on each command)\n"
	    "           -E              return non-zero exit code if child does\n"
	    "           -s shell        shell to invoke\n"
	    "           -f file         instruction file\n"
	    "           -o file         output file\n"
	    "           -e file         error file\n"
	    "           -d dir          change to dir first\n"
	    "           -n[n]           print a 'dry_run' of commands\n"
	    "           -A              return 0 error code\n"
	    "           -S msecs        sleep between spawning copies (in milliseconds)\n"
            "           -r              don't do I/O redirection ($command already has it)\n"
            "           -N              open null device when no input file is specified\n"
            "           -Z              use zero-length file when no input file is specified\n"
            "           -C              (default) close stdin when no input file is specified\n"
	    "           -h              this message\n"
	   ,name);
#if defined(PERFMON)
    pm_usage(&(state->pm_state));
#endif
    specinvoke_exit (0, state);
}

pid_t dry_invoke(copy_info_t *ui, command_info_t *ci, char **env,
		 specinvoke_state_t *si) {
    static pid_t pid = 1000;
    char *dir = (ui->dir == NULL) ? ci->dir : ui->dir,
         *tmpcmd = NULL,
         *cmd = ci->cmd,
         *numbuf = NULL;
    
    printf("# Starting run for copy #%u\n", ui->num);

    gettime(&(ui->start_time));
    if (dir && si->dry_run >= 2) {
	printf ("cd %s\n", dir);
    }

    /* Do bind variable subtitution */
    if (ui->bind != 0) {
        tmpcmd = sub_strings(cmd, BINDSTRVAR, ui->bind);
        cmd = tmpcmd;
    }

    /* Do copy number subtitution */
    numbuf = make_number_buf(ui->num);
    if (numbuf != NULL) {
        tmpcmd = sub_strings(cmd, COPYNUMVAR, numbuf);
        if (cmd != ci->cmd)
          free(cmd);
        cmd = sub_strings(tmpcmd, OLDCOPYNUMVAR, numbuf);
        free(tmpcmd);
        free(numbuf);
    } else {
        /* Something bad happened, but attempt to press on anyway */
        cmd = strdup(ci->cmd);
    }

    printf ("%s", cmd);
    free(cmd); /* No sense in wasting memory... */
    if (si->redir) {
      if (ci->input) {
          printf (" < %s", ci->input);
      }
      if (ci->output) {
          printf (" > %s", ci->output);
      }
      if (ci->err) {
#ifdef _WIN32
          printf (" 2> %s", ci->err);
#else
          printf (" 2>> %s", ci->err);
#endif
      }
    }
    printf ("\n");
    return pid++;
}

/* Here are a couple of standard string functions, reimplemented here so
   that they are portable and can be debugged once.
*/
char *specstrstr(char *haystack, char *needle) {
  int needlelen = 0,
      haylen = 0;
  char *look = haystack;

  if (haystack == NULL)
    return NULL;
  if (needle == NULL)
    return haystack;

  needlelen = strlen(needle);
  haylen = strlen(haystack);
  /* Look for the first character in needle in haystack.  For specinvoke,
     it's going to be far more common for the needle *not* to be found.
  */
  while(*look && ((look - haystack) <= (haylen - needlelen))) {
    if (*look == *needle)
      break;
    look++;
  }
  /* Now go into dumb mode, where we do a full compare on every location */
  while(*look && ((look - haystack) <= (haylen - needlelen))) {
    if (!specstrncmp(look, needle, needlelen))
      return look;
    look++;
  }
  return NULL;
}

/* This isn't a full implementation of strncmp.  In particular, it only
   outputs whether or not the strings are similar, and makes no qualitative
   comparisons.
*/
int specstrncmp(char *a, char *b, int len) {
  char *tmpa = a,
       *tmpb = b;
  int i = 0;

  if ((a == NULL) && (b == NULL))
    return 0;
  if ((a == NULL) || (b == NULL))
    return 1;

  while(*tmpa && *tmpb && (i <= len)) {
    if (*tmpa != *tmpb)
      return 1;
    i++;
    tmpa++;
    tmpb++;
  }
  return 0;
}

int specstrncpy(char *dst, char *src, int len) {
  /* Copy at most len bytes of src to dst, but don't copy the null byte
     at the end. */
  int i = 0;

  while (*src && (i < len)) {
    *dst++ = *src++;
    i++;
  }
  return i;
}

void specinvoke_exit(int code, specinvoke_state_t *state)
{
#if defined(PERFMON)
  if (state != (specinvoke_state_t *)0) {
    pm_exit(code, &(state->pm_state));
  } else {
    fprintf(stderr, "specinvoke_exit got NULL state pointer; pm_exit could not be called.\n");
  }
#endif
  exit(code);
}

char *make_number_buf(unsigned num) {
    /* Make a buffer and copy the string representation of 'num' into it. */
    int buf_size = 1;
    char *numbuf;

    /* Now this part could be replaced with log(num)/log(10), but for the
       tiny range of numbers that we'll be looking at, some conditionals
       and a little insurance will be plenty.
     */
    if (num < 10) {
        buf_size++;
    } else if (num < 100) {
        buf_size += 2;
    } else if (num < 1000) {
        buf_size += 3;
    } else if (num < 10000) {
        /* Approaching the realm of fantasy... */
        buf_size += 4;
    } else if (num < 100000) {
        /* Almost there... */
        buf_size += 5;
    } else if (num < 1000000) {
        /* Here we are! */
        buf_size += 6;
    } else {
        /* This is the insurance.  It should last until an unsigned int is
         * longer than 64 bits.
         */
        buf_size += 20;
    }

    numbuf = (char *)malloc(buf_size * sizeof(char));
    memset(numbuf, 0, buf_size * sizeof(char));
    /* sprintf() is used for ease of porting */
    sprintf(numbuf, "%u", num);

    return(numbuf);
}

char *sub_strings(char *src, char *find, char *replace) {
    /* Make a copy of src, replacing occurances of find with replace */
    int replen, findlen, srclen;
    int newlen = 0;
    int found = 0;
    char *tmp = src;
    char *last = src;
    char *rc = NULL;
    char *tmprc = NULL;

    if (src == NULL) return NULL;
    if (find == NULL) return src;

    replen = strlen(replace);
    findlen = strlen(find);
    srclen = strlen(src);

    /* Insure++ complains that realloc() frees a NULL pointer.  So make rc
     * non-NULL.  This handles the allocation that would be necessary in
     * the tmp==last section below.
     */
    rc = (char *)malloc(replen * sizeof(char) + 1);
    if (rc == NULL)  {
	printf("Can't malloc %d bytes: %s(%u)\n",
	       replen * (int)sizeof(char), STRERROR(errno), (unsigned)errno);
	specinvoke_exit(1, 0);
    }

    /* Find find */
    tmp = specstrstr(src, find);
    found = (tmp != NULL);
    while (tmp != NULL) {
        /* Copy the bit into rc that hasn't yet been copied. */
        if (tmp == last) {
            /* It's at the beginning of the string; just copy */
            memset(rc, 0, replen * sizeof(char) + 1);
            newlen = specstrncpy(rc, replace, replen);
        } else {
            tmprc = (char *)realloc(rc,
	                            sizeof(char) * (newlen + (tmp - last) + replen + 1));
            if (tmprc == NULL) {
                printf("Can't realloc %d bytes for %p: %s(%d)\n",
		       (int)sizeof(char) * (newlen + (tmp - last) + replen + 1),
		       rc, STRERROR(errno), errno);
                specinvoke_exit(1, 0);
            }
            rc = tmprc;
            newlen += specstrncpy(rc + newlen, last, tmp - last);
            newlen += specstrncpy(rc + newlen, replace, replen);
        }
        last = tmp + findlen;
        tmp = specstrstr(last, find);
    }
    /* Finish the end of the string, if applicable */
    if (last != src && last < (src + srclen)) {
	tmprc = (char *)realloc(rc, sizeof(char) * (newlen + (srclen - (last - src)) + 1));
	if (tmprc == NULL) {
	    printf("Can't realloc %d bytes for %p: %s(%d)\n",
		   (int)sizeof(char) * (newlen + (srclen - (last - src)) + 1),
		   rc, STRERROR(errno), errno);
	    specinvoke_exit(1, 0);
	}
	rc = tmprc;
	newlen += specstrncpy(rc + newlen, last, (srclen - (last - src)));
    }

    /* Maybe find wasn't found */
    if (!found) {
        return(strdup(src));
    } else {
	*(rc + newlen) = '\0';
        return(rc);
    }
}

