/* Compute MD5 checksum of files or strings according to the definition
   of MD5 in RFC 1321 from April 1992.
   Copyright (C) 95, 1996 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

/* Written by Ulrich Drepper <drepper@gnu.ai.mit.edu>.  */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <getopt.h>
#include <stdio.h>
#include <sys/types.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

/* Unconditional for now, since it's POSIX. */
#include <dirent.h>

#include "long-options.h"
#include "md5.h"
#include "getline.h"
#include "system.h"
#include "error.h"
#include "xmalloc.h"

#ifndef PATH_MAX
# define PATH_MAX 1024
#endif

#if defined(_WIN32)
# define MYSTAT _stat
# ifndef S_ISDIR
#  define S_ISDIR(x) (((x) & _S_IFDIR) != 0)
# endif
# define STRDUP _strdup
#else
# define MYSTAT stat
# define STRDUP strdup
#endif

/* Most systems do not distinguish between external and internal
   text representations.  */
#if UNIX || __UNIX__ || unix || __unix__ || _POSIX_VERSION
# define OPENOPTS(BINARY) "r"
#else
# ifdef MSDOS
#  define TEXT1TO1 "rb"
#  define TEXTCNVT "r"
# else
#  if defined VMS
#   define TEXT1TO1 "rb", "ctx=stm"
#   define TEXTCNVT "r", "ctx=stm"
#  else
    /* The following line is intended to evoke an error.
       Using #error is not portable enough.  */
    "Cannot determine system type."
#  endif
# endif
# define OPENOPTS(BINARY) ((BINARY) != 0 ? TEXT1TO1 : TEXTCNVT)
#endif

#if _LIBC || STDC_HEADERS
# define TOLOWER(c) tolower (c)
#else
# define TOLOWER(c) (ISUPPER (c) ? tolower (c) : (c))
#endif

/* The minimum length of a valid digest line in a file produced
   by `md5sum FILE' and read by `md5sum --check'.  This length does
   not include any newline character at the end of a line.  */
#define MIN_DIGEST_LINE_LENGTH (32 /* message digest length */ \
				+ 2 /* blank and binary indicator */ \
				+ 3 /* blank and size and blank */ \
				+ 1 /* minimum filename length */ )

/* Nonzero if any of the files read were the standard input. */
static int have_read_stdin;

/* With --check, don't generate any output.
   The exit code indicates success or failure.  */
static int status_only = 0;

/* With --check, print a message to standard error warning about each
   improperly formatted MD5 checksum line.  */
static int warn = 0;

/* Whether to expect or output "normal" format files */
static int normal_lines = 1;

/* The name this program was run with.  */
char *program_name;

static const struct option long_options[] =
{
  { "binary", no_argument, 0, 'b' },
  { "check", no_argument, 0, 'c' },
  { "status", no_argument, 0, 2 },
  { "string", required_argument, 0, 1 },
  { "text", no_argument, 0, 't' },
  { "warn", no_argument, 0, 'w' },
  { "extended", no_argument, 0, 'n' },
  { NULL, 0, NULL, 0 }
};

static void read_tree(const char *path, int *endpos, int *new_end, char ***files);

static void
usage (int status)
{
  if (status != 0)
    fprintf (stderr, _("Try `%s --help' for more information.\n"),
	     program_name);
  else
    {
      printf (_("\
Usage: %s [OPTION] [FILE]...\n\
  or:  %s [OPTION] --check [FILE]\n\
Print or check MD5 checksums.\n\
With no FILE, or when FILE is -, read standard input.\n\
\n\
  -b, --binary            read files in binary mode\n\
  -c, --check             check MD5 sums against given list\n\
  -t, --text              read files in text mode (default)\n\
  -e, --extended          read or write checksum files with embedded sizes\n\
\n\
The following two options are useful only when verifying checksums:\n\
      --status            don't output anything, status code shows success\n\
  -w, --warn              warn about improperly formated MD5 checksum lines\n\
\n\
      --help              display this help and exit\n\
      --version           output version information and exit\n\
\n\
The sums are computed as described in RFC 1321.  When checking, the input\n\
should be a former output of this program.  The default mode is to print\n\
a line with checksum, a character indicating type (`*' for binary, ` ' for\n\
text), and name for each FILE.\n"),
	      program_name, program_name, program_name);
      puts (_("\nReport bugs to textutils-bugs@gnu.ai.mit.edu"));
    }

  exit (status == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
}

static int
split_3 (char *s, size_t s_len, char **u, int *binary, size_t *size, char **w)
{
  size_t i;
  int filename_has_newline = 0;
  char *endptr;

#define ISWHITE(c) ((c) == ' ' || (c) == '\t')

  i = 0;
  while (ISWHITE (s[i]))
    ++i;

  /* The line must have at least 35 (36 if the first is a backslash)
     more characters to contain correct message digest information.
     Ignore this line if it is too short.  */
  if (!(s_len - i >= MIN_DIGEST_LINE_LENGTH + (-normal_lines * 3)
	|| (s[i] == '\\' && s_len - i >= 1 + MIN_DIGEST_LINE_LENGTH + (-normal_lines * 3))))
    return 1;

  if (s[i] == '\\')
    {
      ++i;
      filename_has_newline = 1;
    }
  *u = &s[i];

  /* The first field has to be the 32-character hexadecimal
     representation of the message digest.  If it is not followed
     immediately by a white space it's an error.  */
  i += 32;
  if (!ISWHITE (s[i]))
    return 1;

  s[i++] = '\0';

  if (s[i] != ' ' && s[i] != '*')
    return 1;
 /* For the purposes of SPEC CPU, _all_ files are binary. */
  *binary = (s[i++] == '*') || 1;

  if (!normal_lines) {
    /* Process the size field */
    if (!ISWHITE(s[i++]))
      return 1;
    *size = (size_t)strtoull(s+i, &endptr, 16);
    if (endptr)
      i = endptr - s;
    if (!ISWHITE(s[i++]))
      return 1;
  }

  /* All characters between the type indicator and end of line are
     significant -- that includes leading and trailing white space.  */
  *w = &s[i];

  if (filename_has_newline)
    {
      /* Translate each `\n' string in the file name to a NEWLINE,
	 and each `\\' string to a backslash.  */

      char *dst = &s[i];

      while (i < s_len)
	{
	  switch (s[i])
	    {
	    case '\\':
	      if (i == s_len - 1)
		{
		  /* A valid line does not end with a backslash.  */
		  return 1;
		}
	      ++i;
	      switch (s[i++])
		{
		case 'n':
		  *dst++ = '\n';
		  break;
		case '\\':
		  *dst++ = '\\';
		  break;
		default:
		  /* Only `\' or `n' may follow a backslash.  */
		  return 1;
		}
	      break;

	    case '\0':
	      /* The file name may not contain a NUL.  */
	      return 1;
	      break;

	    default:
	      *dst++ = s[i++];
	      break;
	    }
	}
      *dst = '\0';
    }
  return 0;
}

static int
hex_digits (const char *s)
{
  while (*s)
    {
      if (!ISXDIGIT (*s))
        return 0;
      ++s;
    }
  return 1;
}

/* An interface to md5_stream.  Operate on FILENAME (it may be "-") and
   put the result in *MD5_RESULT.  Return non-zero upon failure, zero
   to indicate success.  */

static int
md5_file (const char *filename, int binary, size_t *size, unsigned char *md5_result)
{
  FILE *fp;
  int err;

  if (strcmp (filename, "-") == 0)
    {
      have_read_stdin = 1;
      fp = stdin;
    }
  else
    {
      /* OPENOPTS is a macro.  It varies with the system.
	 Some systems distinguish between internal and
	 external text representations.  */

      fp = fopen (filename, OPENOPTS (binary));
      if (fp == NULL)
	{
	  error (0, errno, "%s", filename);
	  return 1;
	}
    }

  err = md5_stream (fp, size, md5_result);
  if (err)
    {
      error (0, errno, "%s", filename);
      if (fp != stdin)
	fclose (fp);
      return 1;
    }

  if (fp != stdin && fclose (fp) == EOF)
    {
      error (0, errno, "%s", filename);
      return 1;
    }

  return 0;
}

static int
md5_check (const char *checkfile_name)
{
  FILE *checkfile_stream;
  int n_properly_formated_lines = 0;
  int n_mismatched_checksums = 0;
  int n_open_or_read_failures = 0;
  int n_size_mismatches = 0;
  unsigned char md5buffer[16];
  size_t line_number;
  char *line;
  size_t line_chars_allocated;
  size_t size;

  if (strcmp (checkfile_name, "-") == 0)
    {
      have_read_stdin = 1;
      checkfile_name = _("standard input");
      checkfile_stream = stdin;
    }
  else
    {
      checkfile_stream = fopen (checkfile_name, "r");
      if (checkfile_stream == NULL)
	{
	  error (0, errno, "%s", checkfile_name);
	  return 1;
	}
    }

  line_number = 0;
  line = NULL;
  line_chars_allocated = 0;
  do
    {
      char *filename;
      int binary;
      char *md5num;
      int err;
      int line_length;
      size_t file_len = 0;

      ++line_number;

      line_length = getline (&line, &line_chars_allocated, checkfile_stream);
      if (line_length <= 0)
	break;

      /* Ignore comment lines, which begin with a '#' character.  */
      if (line[0] == '#')
	continue;

      /* Remove any trailing EOLs.  */
      while (line[line_length - 1] == '\n' || line[line_length - 1] == '\r')
	line[--line_length] = '\0';

      err = split_3 (line, line_length, &md5num, &binary, &size, &filename);
      if (err || !hex_digits (md5num))
	{
	  if (warn)
	    {
	      error (0, 0,
		     _("%s: %lu: improperly formatted MD5 checksum line"),
		     checkfile_name, (unsigned long) line_number);
	    }
	}
      else
	{
	  static const char bin2hex[] = { '0', '1', '2', '3',
					  '4', '5', '6', '7',
					  '8', '9', 'a', 'b',
					  'c', 'd', 'e', 'f' };
	  int fail;

	  ++n_properly_formated_lines;

	  fail = md5_file (filename, binary, &file_len, md5buffer);

	  if (fail || (!normal_lines && file_len != size))
	    {
	      if (fail) {
		++n_open_or_read_failures;
		if (!status_only)
		  {
		    printf (_("%s: FAILED open or read\n"), filename);
		    fflush (stdout);
		  }
	      } else {
		++n_size_mismatches;
		if (!status_only)
		  {
		    printf (_("%s: FAILED size mismatch\n"), filename);
		    fflush (stdout);
		  }
	      }
	    }
	  else
	    {
	      size_t cnt;
	      /* Compare generated binary number with text representation
		 in check file.  Ignore case of hex digits.  */
	      for (cnt = 0; cnt < 16; ++cnt)
		{
		  if (TOLOWER (md5num[2 * cnt]) != bin2hex[md5buffer[cnt] >> 4]
		      || (TOLOWER (md5num[2 * cnt + 1])
			  != (bin2hex[md5buffer[cnt] & 0xf])))
		    break;
		}
	      if (cnt != 16)
		++n_mismatched_checksums;

	      if (!status_only)
		{
		  printf ("%s: %s\n", filename,
			  (cnt != 16 ? _("FAILED") : _("OK")));
		  fflush (stdout);
		}
	    }
	}
    }
  while (!feof (checkfile_stream) && !ferror (checkfile_stream));

  if (line)
    free (line);

  if (ferror (checkfile_stream))
    {
      error (0, 0, _("%s: read error"), checkfile_name);
      return 1;
    }

  if (checkfile_stream != stdin && fclose (checkfile_stream) == EOF)
    {
      error (0, errno, "%s", checkfile_name);
      return 1;
    }

  if (n_properly_formated_lines == 0)
    {
      /* Warn if no tests are found.  */
      error (0, 0, _("%s: no properly formatted MD5 checksum lines found"),
	     checkfile_name);
    }
  else
    {
      if (!status_only)
	{
	  int n_computed_checkums = (n_properly_formated_lines
				     - n_open_or_read_failures);

	  if (n_open_or_read_failures > 0)
	    {
	      error (0, 0,
		   _("WARNING: %d of %d listed %s could not be read\n"),
		     n_open_or_read_failures, n_properly_formated_lines,
		     (n_properly_formated_lines == 1
		      ? _("file") : _("files")));
	    }

	  if (n_mismatched_checksums > 0)
	    {
	      error (0, 0,
		   _("WARNING: %d of %d computed %s did NOT match"),
		     n_mismatched_checksums, n_computed_checkums,
		     (n_computed_checkums == 1
		      ? _("checksum") : _("checksums")));
	    }

	  if (n_size_mismatches > 0)
	    {
	      error (0, 0,
		   _("WARNING: %d of %d file %s did NOT match"),
		     n_size_mismatches, n_properly_formated_lines,
		     (n_properly_formated_lines == 1
		      ? _("size") : _("sizes")));
	    }
	}
    }

  return ((n_properly_formated_lines > 0 && n_mismatched_checksums == 0
	   && n_size_mismatches == 0 && n_open_or_read_failures == 0) ? 0 : 1);
}

int
main (int argc, char **argv)
{
  unsigned char md5buffer[16];
  int do_check = 0;
  int opt;
  char **string = NULL;
  size_t n_strings = 0;
  size_t i;
  size_t err = 0;
  int file_type_specified = 0;
  int end, space;
  char **files;


  /* Text is default of the Plumb/Lankester format.  */
  /* We like binary better anyway. */
  int binary = 1;

  /* Setting values of global variables.  */
  program_name = argv[0];
  setlocale (LC_ALL, "");
  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);

  parse_long_options (argc, argv, "md5sum", GNU_PACKAGE, VERSION, usage);

  while ((opt = getopt_long (argc, argv, "bctwe", long_options, NULL))
	 != EOF)
    switch (opt)
      {
      case 0:			/* long option */
	break;
      case 1: /* --string */
	{
	  if (string == NULL)
	    string = (char **) xmalloc ((argc - 1) * sizeof (char *));

	  if (optarg == NULL)
	    optarg = "";
	  string[n_strings++] = optarg;
	}
	break;
      case 'b': /* --binary */
	file_type_specified = 1;
	binary = 1;
	break;
      case 'c':
	do_check = 1;
	break;
      case 2:
	status_only = 1;
	warn = 0;
	break;
      case 't': /* --text */
	file_type_specified = 1;
	binary = 0;
	break;
      case 'w':
	status_only = 0;
	warn = 1;
	break;
      case 'e':
	normal_lines = 0;
	break;
      default:
	usage (EXIT_FAILURE);
      }

  if (file_type_specified && do_check)
    {
      error (0, 0, _("the --binary and --text options are meaningless when \
verifying checksums"));
      usage (EXIT_FAILURE);
    }

  if (n_strings > 0 && do_check)
    {
      error (0, 0,
	     _("the --string and --check options are mutually exclusive"));
      usage (EXIT_FAILURE);
    }

  if (status_only && !do_check)
    {
      error (0, 0,
       _("the --status option is meaningful only when verifying checksums"));
      usage (EXIT_FAILURE);
    }

  if (warn && !do_check)
    {
      error (0, 0,
       _("the --warn option is meaningful only when verifying checksums"));
      usage (EXIT_FAILURE);
    }

  if (n_strings > 0)
    {
      if (optind < argc)
	{
	  error (0, 0, _("no files may be specified when using --string"));
	  usage (EXIT_FAILURE);
	}
      for (i = 0; i < n_strings; ++i)
	{
	  size_t cnt;
	  md5_buffer (string[i], strlen (string[i]), md5buffer);

	  for (cnt = 0; cnt < 16; ++cnt)
	    printf ("%02x", md5buffer[cnt]);

	  printf ("  \"%s\"\n", string[i]);
	}
    }
  else if (do_check)
    {
      if (optind + 1 < argc)
	{
	  error (0, 0,
		 _("only one argument may be specified when using --check"));
	  usage (EXIT_FAILURE);
	}

      err = md5_check ((optind == argc) ? "-" : argv[optind]);
    }
  else
    {
      if (optind == argc)
	argv[argc++] = "-";

      /* Copy argc and argv into something that can be modified */
      end = space = argc - optind;
      files = (char **)malloc(sizeof(char *) * end);
      for (i = 0; i < end; i++) {
	files[i] = STRDUP(argv[optind + i]);
      }

      for (i = 0; i < end; i++)
	{
	  int fail;
	  char *file = files[i];
	  size_t size = 0;
	  struct MYSTAT statbuf;

	  fail = MYSTAT(file, &statbuf);
	  if (fail != 0 && strcmp(file, "-")) {
	    error(EXIT_FAILURE, errno, "stat %s", file);
	  }
	  if (S_ISDIR(statbuf.st_mode)) {
	    read_tree(file, &end, &space, &files);
	    continue;
	  }

	  fail = md5_file (file, binary, &size, md5buffer);
	  err |= fail;
	  if (!fail)
	    {
	      size_t i;

	      /* Output a leading backslash if the file name contains
		 a newline.  */
	      if (strchr (file, '\n'))
		putchar ('\\');

	      for (i = 0; i < 16; ++i)
		printf ("%02x", md5buffer[i]);

	      putchar (' ');
	      if (binary)
		putchar ('*');
	      else
		putchar (' ');

	      if (!normal_lines) {
		putchar (' ');
		printf ("%08X", size);
		putchar (' ');
	      }

	      /* Translate each NEWLINE byte to the string, "\\n",
		 and each backslash to "\\\\".  */
	      for (i = 0; i < strlen (file); ++i)
		{
		  switch (file[i])
		    {
		    case '\n':
		      fputs ("\\n", stdout);
		      break;

		    case '\\':
		      fputs ("\\\\", stdout);
		      break;

		    default:
		      putchar (file[i]);
		      break;
		    }
		}
	      putchar ('\n');
	    }
	}
    }

  if (fclose (stdout) == EOF)
    error (EXIT_FAILURE, errno, _("write error"));

  if (have_read_stdin && fclose (stdin) == EOF)
    error (EXIT_FAILURE, errno, _("standard input"));

  exit (err == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
}

/* Expand the directory tree under path, appending the path list to files. */
static void read_tree(const char *path, int *endpos, int *new_end, char ***files) {
  DIR *dirptr;
  struct dirent *ent;
  struct MYSTAT st;
  int rc = 0;
  char pathbuf[PATH_MAX];
  char *fnstart;
  static int depth = 0;

  depth++;

  dirptr = opendir(path);
  if (dirptr == NULL) {
    error(EXIT_FAILURE, errno, "opendir(%s)", path);
  }


  errno = 0;
  strcpy(pathbuf, path);
  strcat(pathbuf, "/");
  fnstart = pathbuf + strlen(path) + 1;
  
  while((ent = readdir(dirptr)) != NULL) {
    if (strcmp(ent->d_name, ".\0") && strcmp(ent->d_name, "..\0")) {
      strcpy(fnstart, ent->d_name);

      rc = MYSTAT(pathbuf, &st);
      if (rc != 0) {
	error(EXIT_FAILURE, errno, "stat of %s", pathbuf);
      }
      if (S_ISDIR(st.st_mode)) {
	read_tree(pathbuf, endpos, new_end, files);
      } else {
	if (*endpos >= *new_end) {
	  /* Since pointers are small, add room for another 256 entries */
	  *files = xrealloc(*files, sizeof(char **) * (*endpos + 256));
	  *new_end = *endpos + 256;
	}
	(*files)[*endpos] = STRDUP(pathbuf);
	if ((*files)[*endpos] != NULL) {
	  (*endpos)++;
	} else {
	  error(EXIT_FAILURE, errno, "read_tree: error allocating memory");
	}
      }
    }
    errno = 0; /* To be able to detect errors from readdir() */
  }
  if (ent == NULL && errno != 0) {
    error(EXIT_FAILURE, errno, "readdir");
  } else {
    closedir(dirptr);
  }
  depth--;
}
