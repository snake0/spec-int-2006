/*
 * specinvoke - run and accurately time lists of commands
 * Copyright(C) 1998-2003 Standard Performance Evaluation Corporation
 * All Rights Reserved
 *
 * win32.c: Windows-specific functions
 *
 * $Id: win32.c 3478 2005-12-13 00:22:38Z cloyce $
 */

#include "specinvoke.h"

static char *versionid="1.0";
static char *rcsid="$Id: win32.c 3478 2005-12-13 00:22:38Z cloyce $";

HANDLE *win32_handles;
long   *win32_pids;
long   win32_count;

void init_state(specinvoke_state_t *si) {
  /* These values are not actually used... */
  si->invoke_args = NULL;
  si->command_ptr = NULL;
  si->shell       = NULL;
}

void die (char *msg) {
    int errnum = GetLastError ();
    LPVOID lpMsgBuf;
    FormatMessage(
        FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
	NULL,
        GetLastError(),
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
        (LPTSTR) &lpMsgBuf,
	0,
	NULL );
	
    // Display the string.
    // MessageBox( NULL, lpMsgBuf, "GetLastError", MB_OK|MB_ICONINFORMATION );
    if (lpMsgBuf == NULL)
	lpMsgBuf = "";
    fprintf (stderr, "%s: %s(%d)\n", msg, lpMsgBuf, errnum);

    // Free the buffer.
    LocalFree( lpMsgBuf );

    specinvoke_exit(1, (specinvoke_state_t *)NULL);
}


void init_os(int num) {
    win32_handles = (HANDLE *)malloc(sizeof(HANDLE)*(num+1));
    win32_pids    = (long   *)malloc(sizeof(DWORD)*(num+1));
    win32_count   = 0;
    if (win32_handles == NULL || win32_pids == NULL) {
	fprintf (stderr, "Unable to allocate space for handle array\n");
	specinvoke_exit(1, (specinvoke_state_t *)NULL);
    }
}
void reinit_os() {
    win32_count   = 0;
}

void cleanup_os(int num) {
}

pid_t invoke(copy_info_t *ui, command_info_t *ci, char **env,
	     specinvoke_state_t *si) {
    PROCESS_INFORMATION stProcInfo;
    STARTUPINFO         stStartInfo;
    SECURITY_ATTRIBUTES stSecAttr;
    char *dir = (ui->dir == NULL)?ci->dir:ui->dir;
    char *numbuf = NULL,
         *cmd = ci->cmd,
	 *tmpcmd = NULL;

    if (si->dry_run)
	return dry_invoke(ui, ci, env, si);

    gettime(&(ui->start_time));

    if (dir && _chdir(dir)) {
	fprintf (stderr, "Can't change directory to '%s': %s(%d)\n", 
		 ci->dir, strerror(errno), errno);
	specinvoke_exit (1, si);
    }

    stSecAttr.nLength = sizeof (stSecAttr);
    stSecAttr.lpSecurityDescriptor = NULL;
    stSecAttr.bInheritHandle = TRUE;

    memset (&stStartInfo, 0, sizeof (stStartInfo)); /* Clear the block */
    stStartInfo.cb = sizeof (stStartInfo);          /* Set the structure size */
    stStartInfo.dwFlags = STARTF_USESTDHANDLES;     /* Use handles I pass it */

#if 0
    stStartInfo.hStdInput  = INVALID_HANDLE_VALUE;
    stStartInfo.hStdOutput = INVALID_HANDLE_VALUE;
    stStartInfo.hStdError  = INVALID_HANDLE_VALUE;
#endif

    /* Do bind variable subtitution */
    if (ui->bind != 0) {
        tmpcmd = sub_strings(cmd, BINDSTRVAR, ui->bind);
        if (cmd != ci->cmd)
          free(cmd);
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
    }

    if (cmd == ci->cmd) {
      /* Make a free()able copy */
      cmd = strdup(ci->cmd);
    }

    if (si->redir) {
      if (ci->err) {
	stStartInfo.hStdError = CreateFile (
		ci->err,                      /* pointer to name of the file */
		GENERIC_WRITE,                /* access (read-write) mode */
		FILE_SHARE_READ | FILE_SHARE_DELETE, /* share mode */
		&stSecAttr,                   /* pointer to security attrib. */
		CREATE_ALWAYS,                /* how to create */
		FILE_ATTRIBUTE_NORMAL,        /* file attributes */
		NULL);
	if (stStartInfo.hStdError == INVALID_HANDLE_VALUE) {
	  die("Can't open Error Handle");
	}
      }
      if (ci->input) {
	stStartInfo.hStdInput = CreateFile (
		ci->input,              /* pointer to name of the file */
		GENERIC_READ,           /* access (read-write) mode */
		FILE_SHARE_READ | FILE_SHARE_DELETE, /* share mode */
		&stSecAttr,             /* pointer to security attributes */
		OPEN_EXISTING,          /* how to create */
		FILE_ATTRIBUTE_NORMAL,  /* file attributes */
		NULL);
	if (stStartInfo.hStdInput == INVALID_HANDLE_VALUE) {
	  die("Can't open Input Handle");
	}
      } else {
	if (si->no_stdin == NUL) {
	  /* Open NUL: for stdin */
	  /* XXX Will this even work? */
	  stStartInfo.hStdInput = CreateFile (
                  "NUL:",                 /* pointer to name of the file */
		  GENERIC_READ,           /* access (read-write) mode */
                  FILE_SHARE_READ | FILE_SHARE_DELETE, /* share mode */
		  &stSecAttr,             /* pointer to security attributes */
		  OPEN_EXISTING,          /* how to create */
		  FILE_ATTRIBUTE_NORMAL,  /* file attributes */
		  NULL);
	  if (stStartInfo.hStdInput == INVALID_HANDLE_VALUE) {
	    die("Can't open Input Handle");
	  }
	} else if (si->no_stdin == ZEROFILE) {
	  /* Create a zero-length file for stdin */
	  char tmpfile[255];
	  sprintf(tmpfile, "spec_empty_file.%u.%ld", ui->num, (long)(ui->pid));
	  stStartInfo.hStdInput = CreateFile (
                  tmpfile,                /* pointer to name of the file */
		  GENERIC_READ,           /* access (read-write) mode */
                  FILE_SHARE_READ | FILE_SHARE_DELETE, /* share mode */
		  &stSecAttr,             /* pointer to security attributes */
		  CREATE_ALWAYS,          /* how to create */
		  FILE_ATTRIBUTE_TEMPORARY,     /* file attributes */
		  NULL);
	  if (stStartInfo.hStdInput == INVALID_HANDLE_VALUE) {
	    die("Can't open Input Handle");
	  }
	} else {
	  /* Just use regular stdin */
	  stStartInfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
	}
      }
      if (ci->output) {
	stStartInfo.hStdOutput = CreateFile (
		ci->output,                   /* pointer to name of the file */
		GENERIC_WRITE,                /* access (read-write) mode */
                FILE_SHARE_READ | FILE_SHARE_DELETE, /* share mode */
		&stSecAttr,                   /* ptr to security attributes */
		CREATE_ALWAYS,                /* how to create */
		FILE_ATTRIBUTE_NORMAL,        /* file attributes */
		NULL);
	if (stStartInfo.hStdOutput == INVALID_HANDLE_VALUE) {
	  die("Can't open Output Handle");
	}
      }
    }

#if defined(PERFMON)
      pm_pre_spawn(ci, ui, &(si->pm_state));
#endif
    if (CreateProcess (
	    NULL,                     /* Command to execute */
	    cmd,                      /* Command line args */
	    NULL,                     /* Default process security */
	    NULL,                     /* Default thread security */
	    TRUE,                     /* Must be TRUE to use std handles */
	    NORMAL_PRIORITY_CLASS,    /* No special scheduling */
	    NULL,                     /* Inherit our environment block */
	    NULL,                     /* Inherit our currrent directory */
	    &stStartInfo,             /* Start info*/
	    &stProcInfo)) {           /* <- Process info */
#if defined(PERFMON)
      pm_post_spawn(stProcInfo.dwProcessId, ci, ui, &(si->pm_state));
#endif
	win32_handles[win32_count] = stProcInfo.hProcess;
	win32_pids[win32_count]    = stProcInfo.dwProcessId;
    } else {
	die("Can't create process");
    }
    if (ci->output) CloseHandle (stStartInfo.hStdOutput);
    if (ci->input)  CloseHandle (stStartInfo.hStdInput);
    if (ci->err)    CloseHandle (stStartInfo.hStdError);

    ui->pid = win32_pids[win32_count];
    fprintf (si->outfp, "child started: %u, %u, %u, pid=%ld, '%s'\n", ui->num,
             ui->start_time.sec, ui->start_time.nsec, (long)(ui->pid), cmd);

    free(cmd); /* No sense wasting memory */

    return win32_pids[win32_count++];
}

long wait_for_next (long *status, int nowait) {
    DWORD rc;
    long pid;
    int index;
    if (win32_count == 0) {
	    return 0;
    }
    rc = WaitForMultipleObjects (win32_count, win32_handles, FALSE, nowait?0:INFINITE);
    if (rc >= WAIT_OBJECT_0 && rc < WAIT_OBJECT_0 + win32_count) {
	index = rc - WAIT_OBJECT_0;
	if (!GetExitCodeProcess(win32_handles[index], status)) {
	    die("Can't get exit code!");
	}
	if (*status == STILL_ACTIVE) {
		return 0;
	}
	pid = win32_pids[index];
	CloseHandle (win32_handles[index]);
	win32_handles[index] = win32_handles[--win32_count];
	win32_pids   [index] = win32_pids   [win32_count];
    } else if (rc >= WAIT_ABANDONED_0 && rc < WAIT_ABANDONED_0 + win32_count) {
	int errnum = GetLastError ();
	index = rc - WAIT_ABANDONED_0;
	fprintf (stderr, "Handle abandoned! (pid=%ld) %d\n", win32_pids[index],
                 errnum);
	pid = win32_pids[index];
	CloseHandle (win32_handles[index]);
	win32_handles[index] = win32_handles[--win32_count];
	win32_pids   [index] = win32_pids   [win32_count];
	specinvoke_exit(1, (specinvoke_state_t *)NULL);
    } else if (rc == WAIT_TIMEOUT) {
	if (!nowait) {
	    fprintf (stderr, "Time out!  How did THAT happen?\n");
	    specinvoke_exit(1, (specinvoke_state_t *)NULL);
	}
    } else if (rc == WAIT_FAILED) {
	die("WaitForMultipleObjects failed!");
    } else {
	int errnum = GetLastError ();
	fprintf (stderr, "WaitForMultipleObjects had weird return code %d! (%d)\n",
                 rc, errnum);
    }
    return pid;
}

void gettime(spec_time_t *t) {
    struct _timeb tb;
    _ftime(&tb);
    t->sec  = tb.time + (tb.timezone * 60);
    t->nsec = tb.millitm * 1000000;
}

int specmillisleep(time_t milliseconds) {
  Sleep(milliseconds);
  return(0);
}
