/*
  Standard prototype header for switchable prototype support

  Define USE_PROTOTYPES if ANSI C style function
  prototypes should be used.

  Define DECLARE_PROCEDURES if non-returning functions should
  be declared 'void'

  These are defined automatically for C++ and __STDC__

  To use prototypes, declare routines as 'type name _P_((args));'
  */
#ifndef __PROTO_H__
#define __PROTO_H__

#ifndef USE_PROTOTYPES
#ifdef __cplusplus
#define USE_PROTOTYPES
#endif
#ifdef __STDC__
#define USE_PROTOTYPES
#endif
#ifdef WINDOWSNT
#define USE_PROTOTYPES
#endif
#endif

#ifndef DECLARE_PROCEDURES
#ifdef __cplusplus
#define DECLARE_PROCEDURES
#endif
#ifdef __STDC__
#define DECLARE_PROCEDURES
#endif
#ifdef WINDOWSNT
#define DECLARE_PROCEDURES
#endif
#endif

#ifdef USE_PROTOTYPES
#define _P_(x) x
#else
#define _P_(x) ()
#endif

#ifdef DECLARE_PROCEDURES
#define _VOID_ void
#else
#define _VOID_
#endif

/* If using GNU, then support inline function declarations. */
#ifndef INLINE
#ifdef __GNUC__
#define INLINE __inline__
#else
#define INLINE
#endif
#endif

/* If you are compiling with a non-C calling convention but need to
   declare vararg routines differently, put it here */
#ifndef _VARARGS_
#ifdef WINDOWSNT
#define _VARARGS_ __cdecl
#else
#define _VARARGS_
#endif
#endif

/* If you are providing a function to something that will call the
   function back (like a signal handler and signal(), or main()) its calling
   convention must be whatever standard the libraries expect */
#ifndef _CALLBACK_
#ifdef WINDOWSNT
#define _CALLBACK_ __cdecl
#else
#define _CALLBACK_
#endif
#endif

#endif /* __PROTO_H__ */
