# backupfile.m4 serial 5
dnl Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
dnl This file is free software, distributed under the terms of the GNU
dnl General Public License.  As a special exception to the GNU General
dnl Public License, this file may be distributed as part of a program
dnl that contains a configuration script generated by Autoconf, under
dnl the same distribution terms as the rest of that program.

AC_DEFUN([gl_BACKUPFILE],
[
  dnl Prerequisites of lib/backupfile.c.
  AC_REQUIRE([AC_HEADER_DIRENT])
  AC_REQUIRE([gl_CHECK_TYPE_STRUCT_DIRENT_D_INO])
  AC_REQUIRE([gl_AC_DOS])
  AC_REQUIRE([AC_SYS_LONG_FILE_NAMES])
  AC_CHECK_HEADERS_ONCE(unistd.h)
  AC_CHECK_FUNCS(pathconf)
])