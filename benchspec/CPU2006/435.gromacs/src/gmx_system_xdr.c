#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

#include "gmx_system_xdr.h"

#ifdef USE_GMX_XDR

/* NB - THIS FILE IS ONLY USED ON MICROSOFT WINDOWS, since that
 * system doesn't provide any standard XDR system libraries. It will
 * most probably work on other platforms too, but make sure you
 * test that the xtc files produced are ok before using it. 
 *
 * This header file contains Gromacs versions of the definitions for 
 * Sun External Data Representation (XDR) headers and routines.
 *
 * On most UNIX systems this is already present as part of your 
 * system libraries, but since we want to make Gromacs portable to
 * platforms like Microsoft Windows we have created a private version
 * of the necessary routines and distribute them with the Gromacs source.
 * 
 * Although the rest of Gromacs is GPL, you can copy and use the XDR 
 * routines in any way you want as long as you obey Sun's license:
 *
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */ 



/*
 * for unit alignment
 */
static char xdr_zero[BYTES_PER_XDR_UNIT] = {0, 0, 0, 0};

static xdr_uint32_t xdr_swapbytes(xdr_uint32_t x)
{
  xdr_uint32_t y;
  int i;
  char *px=(char *)&x;
  char *py=(char *)&y;
  
  for(i=0;i<4;i++)
    py[i]=px[3-i];
  
  return y;
}

static xdr_uint32_t xdr_htonl(xdr_uint32_t x)
{
  short s=0x0F00;
  if( *((char *)&s)==(char)0x0F) {
    /* bigendian, do nothing */
    return x;
  } else {
    /* smallendian,swap bytes */
    return xdr_swapbytes(x);
  }
}

static xdr_uint32_t xdr_ntohl(xdr_uint32_t x)
{
  short s=0x0F00;
  if( *((char *)&s)==(char)0x0F) {
    /* bigendian, do nothing */
    return x;
  } else {
    /* smallendian, swap bytes */
    return xdr_swapbytes(x);
  }
}


/*
 * Free a data structure using XDR
 * Not a filter, but a convenient utility nonetheless
 */
void
xdr_free (xdrproc_t proc, char *objp)
{
  XDR x;

  x.x_op = XDR_FREE;
  (*proc) (&x, objp);
}

/*
 * XDR nothing
 */
bool_t
xdr_void (void)
{
  return TRUE;
}

/*
 * XDR integers
 */
bool_t
xdr_int (XDR *xdrs, int *ip)
{
  long l;

  switch (xdrs->x_op)
    {
    case XDR_ENCODE:
      l = (long) *ip;
      return xdr_putlong (xdrs, &l);

    case XDR_DECODE:
      if (!xdr_getlong (xdrs, &l))
	{
	  return FALSE;
	}
      *ip = (int) l;
    case XDR_FREE:
      return TRUE;
    }
  return FALSE;
}


/*
 * XDR unsigned integers
 */
bool_t
xdr_u_int (XDR *xdrs, unsigned int *up)
{
  unsigned long l;

  switch (xdrs->x_op)
    {
    case XDR_ENCODE:
      l = (unsigned long) * up;
      return xdr_putlong (xdrs, (long *)&l);

    case XDR_DECODE:
      if (!xdr_getlong (xdrs, (long *)&l))
	{
	  return FALSE;
	}
      *up = (unsigned int) l;
    case XDR_FREE:
      return TRUE;
    }
}


/*
 * XDR long integers
 * The definition of xdr_long() is kept for backward
 * compatibility. Instead xdr_int() should be used.
 */
bool_t
xdr_long (XDR *xdrs, long *lp)
{

  if (xdrs->x_op == XDR_ENCODE
      && (sizeof (xdr_int32_t) == sizeof (long)
	  || (xdr_int32_t) *lp == *lp))
    return xdr_putlong (xdrs, lp);

  if (xdrs->x_op == XDR_DECODE)
    return xdr_getlong (xdrs, lp);

  if (xdrs->x_op == XDR_FREE)
    return TRUE;

  return FALSE;
}


/*
 * XDR unsigned long integers
 * The definition of xdr_u_long() is kept for backward
 * compatibility. Instead xdr_u_int() should be used.
 */
bool_t
xdr_u_long (XDR *xdrs, unsigned long *ulp)
{
  switch (xdrs->x_op)
    {
    case XDR_DECODE:
      {
	long int tmp;

	if (xdr_getlong (xdrs, &tmp) == FALSE)
	  return FALSE;

	*ulp = (xdr_uint32_t) tmp;
	return TRUE;
      }

    case XDR_ENCODE:
      if (sizeof (xdr_uint32_t) != sizeof (unsigned long)
	  && (xdr_uint32_t) *ulp != *ulp)
	return FALSE;

      return xdr_putlong (xdrs, (long *) ulp);

    case XDR_FREE:
      return TRUE;
    }
  return FALSE;
}


/*
 * XDR short integers
 */
bool_t
xdr_short (XDR *xdrs, short *sp)
{
  long l;

  switch (xdrs->x_op)
    {
    case XDR_ENCODE:
      l = (long) *sp;
      return xdr_putlong (xdrs, &l);

    case XDR_DECODE:
      if (!xdr_getlong (xdrs, &l))
	{
	  return FALSE;
	}
      *sp = (short) l;
      return TRUE;

    case XDR_FREE:
      return TRUE;
    }
  return FALSE;
}


/*
 * XDR unsigned short integers
 */
bool_t
xdr_u_short (XDR *xdrs, unsigned short *usp)
{
  unsigned long l;

  switch (xdrs->x_op)
    {
    case XDR_ENCODE:
      l = (unsigned long) * usp;
      return xdr_putlong (xdrs, (long *)&l);

    case XDR_DECODE:
      if (!xdr_getlong (xdrs, (long *)&l))
	{
	  return FALSE;
	}
      *usp = (unsigned short) l;
      return TRUE;

    case XDR_FREE:
      return TRUE;
    }
  return FALSE;
}


/*
 * XDR a char
 */
bool_t
xdr_char (XDR *xdrs, char *cp)
{
  int i;

  i = (*cp);
  if (!xdr_int (xdrs, &i))
    {
      return FALSE;
    }
  *cp = i;
  return TRUE;
}

/*
 * XDR an unsigned char
 */
bool_t
xdr_u_char (XDR *xdrs, unsigned char *cp)
{
  unsigned int u;

  u = (*cp);
  if (!xdr_u_int (xdrs, &u))
    {
      return FALSE;
    }
  *cp = u;
  return TRUE;
}

/*
 * XDR booleans
 */
bool_t
xdr_bool (XDR *xdrs, int *bp)
{
#define XDR_FALSE	((long) 0)
#define XDR_TRUE	((long) 1)
  long lb;

  switch (xdrs->x_op)
    {
    case XDR_ENCODE:
      lb = *bp ? XDR_TRUE : XDR_FALSE;
      return xdr_putlong (xdrs, &lb);

    case XDR_DECODE:
      if (!xdr_getlong (xdrs, &lb))
	{
	  return FALSE;
	}
      *bp = (lb == XDR_FALSE) ? FALSE : TRUE;
      return TRUE;

    case XDR_FREE:
      return TRUE;
    }
  return FALSE;
#undef XDR_FALSE
#undef XDR_TRUE
}



/*
 * XDR opaque data
 * Allows the specification of a fixed size sequence of opaque bytes.
 * cp points to the opaque object and cnt gives the byte length.
 */
bool_t
xdr_opaque (XDR *xdrs, char *cp, unsigned int cnt)
{
  unsigned int rndup;
  static char crud[BYTES_PER_XDR_UNIT];

  /*
   * if no data we are done
   */
  if (cnt == 0)
    return TRUE;

  /*
   * round byte count to full xdr units
   */
  rndup = cnt % BYTES_PER_XDR_UNIT;
  if (rndup > 0)
    rndup = BYTES_PER_XDR_UNIT - rndup;

  switch (xdrs->x_op)
    {
    case XDR_DECODE:
      if (!xdr_getbytes (xdrs, cp, cnt))
	{
	  return FALSE;
	}
      if (rndup == 0)
	return TRUE;
      return xdr_getbytes (xdrs, (char *)crud, rndup);

    case XDR_ENCODE:
      if (!xdr_putbytes (xdrs, cp, cnt))
	{
	  return FALSE;
	}
      if (rndup == 0)
	return TRUE;
      return xdr_putbytes (xdrs, xdr_zero, rndup);

    case XDR_FREE:
      return TRUE;
    }
  return FALSE;
}


/*
 * XDR null terminated ASCII strings
 * xdr_string deals with "C strings" - arrays of bytes that are
 * terminated by a NULL character.  The parameter cpp references a
 * pointer to storage; If the pointer is null, then the necessary
 * storage is allocated.  The last parameter is the max allowed length
 * of the string as specified by a protocol.
 */
bool_t
xdr_string (xdrs, cpp, maxsize)
     XDR *xdrs;
     char **cpp;
     unsigned int maxsize;
{
  char *sp = *cpp;	/* sp is the actual string pointer */
  unsigned int size;
  unsigned int nodesize;

  /*
   * first deal with the length since xdr strings are counted-strings
   */
  switch (xdrs->x_op)
    {
    case XDR_FREE:
      if (sp == NULL)
	{
	  return TRUE;		/* already free */
	}
      /* fall through... */
    case XDR_ENCODE:
      if (sp == NULL)
	return FALSE;
      size = strlen (sp);
      break;
    case XDR_DECODE:
      break;
    }
  if (!xdr_u_int (xdrs, &size))
    {
      return FALSE;
    }
  if (size > maxsize)
    {
      return FALSE;
    }
  nodesize = size + 1;

  /*
   * now deal with the actual bytes
   */
  switch (xdrs->x_op)
    {
    case XDR_DECODE:
      if (nodesize == 0)
	{
	  return TRUE;
	}
      if (sp == NULL)
	*cpp = sp = (char *) malloc (nodesize);
      if (sp == NULL)
	{
	  (void) fputs ("xdr_string: out of memory\n", stderr);
	  return FALSE;
	}
      sp[size] = 0;
      /* fall into ... */

    case XDR_ENCODE:
      return xdr_opaque (xdrs, sp, size);

    case XDR_FREE:
      free (sp);
      *cpp = NULL;
      return TRUE;
    }
  return FALSE;
}



/* Floating-point stuff */

bool_t
xdr_float(xdrs, fp)
     XDR *xdrs;
     float *fp;
{
	switch (xdrs->x_op) {

	case XDR_ENCODE:
		if (sizeof(float) == sizeof(long))
			return (xdr_putlong(xdrs, (long *)fp));
		else if (sizeof(float) == sizeof(int)) {
			long tmp = *(int *)fp;
			return (xdr_putlong(xdrs, &tmp));
		}
		break;

	case XDR_DECODE:
		if (sizeof(float) == sizeof(long))
			return (xdr_getlong(xdrs, (long *)fp));
		else if (sizeof(float) == sizeof(int)) {
			long tmp;
			if (xdr_getlong(xdrs, &tmp)) {
				*(int *)fp = tmp;
				return (TRUE);
			}
		}
		break;

	case XDR_FREE:
		return (TRUE);
	}
	return (FALSE);
}


bool_t
xdr_double(xdrs, dp)
     XDR *xdrs;
     double *dp;
{

  /* Windows and some other systems dont define double-precision
   * word order in the header files, so unfortunately we have
   * to calculate it!
   */
  static int LSW=-1; /* Least significant fp word */
  
  if(LSW<0) {
    double x=0.987654321; /* Just a number */

    /* Possible representations in IEEE double precision: 
     * (S=small endian, B=big endian)
     * 
     * Byte order, Word order, Hex
     *     S           S       b8 56 0e 3c dd 9a ef 3f    
     *     B           S       3c 0e 56 b8 3f ef 9a dd
     *     S           B       dd 9a ef 3f b8 56 0e 3c
     *     B           B       3f ef 9a dd 3c 0e 56 b8
     */ 
    
    unsigned char ix = *((char *)&x);
    
    if(ix==0xdd || ix==0x3f)
      LSW=1;  /* Big endian word order */
    else if(ix==0xb8 || ix==0x3c)
      LSW=0;  /* Small endian word order */
    else { /* Catch strange errors */
      printf("Error when detecting floating-point word order.\n"
	     "Do you have a non-IEEE system?\n"
	     "If possible, use the XDR libraries provided with your system,\n"
	     "instead of the Gromacs fallback XDR source.\n");
      exit(0);
    }
  }  
  
  switch (xdrs->x_op) {
    
  case XDR_ENCODE:
    if (2*sizeof(long) == sizeof(double)) {
      long *lp = (long *)dp;
      return (xdr_putlong(xdrs, lp+!LSW) &&
	      xdr_putlong(xdrs, lp+LSW));
    } else if (2*sizeof(int) == sizeof(double)) {
      int *ip = (int *)dp;
      long tmp[2];
      tmp[0] = ip[!LSW];
      tmp[1] = ip[LSW];
      return (xdr_putlong(xdrs, tmp) &&
	      xdr_putlong(xdrs, tmp+1));
    }
    break;
    
  case XDR_DECODE:
    if (2*sizeof(long) == sizeof(double)) {
      long *lp = (long *)dp;
      return (xdr_getlong(xdrs, lp+!LSW) &&
	      xdr_getlong(xdrs, lp+LSW));
    } else if (2*sizeof(int) == sizeof(double)) {
      int *ip = (int *)dp;
      long tmp[2];
      if (xdr_getlong(xdrs, tmp+!LSW) &&
	  xdr_getlong(xdrs, tmp+LSW)) {
	ip[0] = tmp[0];
	ip[1] = tmp[1];
	return (TRUE);
      }
    }
    break;
    
  case XDR_FREE:
    return (TRUE);
  }
  return (FALSE);
}


/* Array routines */

/*
 * xdr_vector():
 *
 * XDR a fixed length array. Unlike variable-length arrays,
 * the storage of fixed length arrays is static and unfreeable.
 * > basep: base of the array
 * > size: size of the array
 * > elemsize: size of each element
 * > xdr_elem: routine to XDR each element
 */
bool_t
xdr_vector (xdrs, basep, nelem, elemsize, xdr_elem)
     XDR *xdrs;
     char *basep;
     unsigned int nelem;
     unsigned int elemsize;
     xdrproc_t xdr_elem;
{
#define LASTUNSIGNED	((unsigned int)0-1)
  unsigned int i;
  char *elptr;

  elptr = basep;
  for (i = 0; i < nelem; i++)
    {
      if (!(*xdr_elem) (xdrs, elptr, LASTUNSIGNED))
	{
	  return FALSE;
	}
      elptr += elemsize;
    }
  return TRUE;
#undef LASTUNSIGNED
}



static bool_t xdrstdio_getlong (XDR *, long *);
static bool_t xdrstdio_putlong (XDR *, long *);
static bool_t xdrstdio_getbytes (XDR *, char *, unsigned int);
static bool_t xdrstdio_putbytes (XDR *, char *, unsigned int);
static unsigned int xdrstdio_getpos (XDR *);
static bool_t xdrstdio_setpos (XDR *, unsigned int);
static xdr_int32_t *xdrstdio_inline (XDR *, int);
static void xdrstdio_destroy (XDR *);
static bool_t xdrstdio_getint32 (XDR *, xdr_int32_t *);
static bool_t xdrstdio_putint32 (XDR *, xdr_int32_t *);

/*
 * Ops vector for stdio type XDR
 */
static const struct xdr_ops xdrstdio_ops =
{
  xdrstdio_getlong,		/* deserialize a long int */
  xdrstdio_putlong,		/* serialize a long int */
  xdrstdio_getbytes,       	/* deserialize counted bytes */
  xdrstdio_putbytes,     	/* serialize counted bytes */
  xdrstdio_getpos,		/* get offset in the stream */
  xdrstdio_setpos,		/* set offset in the stream */
  xdrstdio_inline,		/* prime stream for inline macros */
  xdrstdio_destroy,		/* destroy stream */
  xdrstdio_getint32,	/* deserialize a int */
  xdrstdio_putint32		/* serialize a int */
};

/*
 * Initialize a stdio xdr stream.
 * Sets the xdr stream handle xdrs for use on the stream file.
 * Operation flag is set to op.
 */
void
xdrstdio_create (XDR *xdrs, FILE *file, enum xdr_op op)
{
  xdrs->x_op = op;
  /* We have to add the const since the `struct xdr_ops' in `struct XDR'
     is not `const'.  */
  xdrs->x_ops = (struct xdr_ops *) &xdrstdio_ops;
  xdrs->x_private = (char *) file;
  xdrs->x_handy = 0;
  xdrs->x_base = 0;
}

/*
 * Destroy a stdio xdr stream.
 * Cleans up the xdr stream handle xdrs previously set up by xdrstdio_create.
 */
static void
xdrstdio_destroy (XDR *xdrs)
{
  (void) fflush ((FILE *) xdrs->x_private);
  /* xx should we close the file ?? */
}

static bool_t
xdrstdio_getlong (XDR *xdrs, long *lp)
{
  xdr_int32_t mycopy;

  if (fread ((char *) & mycopy, 4, 1, (FILE *) xdrs->x_private) != 1)
    return FALSE;
  *lp = (xdr_int32_t) xdr_ntohl (mycopy);
  return TRUE;
}

static bool_t
xdrstdio_putlong (XDR *xdrs, long *lp)
{
  long mycopy = xdr_htonl (*lp);
  lp = &mycopy;
  if (fwrite ((char *) lp, 4, 1, (FILE *) xdrs->x_private) != 1)
    return FALSE;
  return TRUE;
}

static bool_t
xdrstdio_getbytes (XDR *xdrs, char *addr, unsigned int len)
{
  if ((len != 0) && (fread (addr, (int) len, 1,
			    (FILE *) xdrs->x_private) != 1))
    return FALSE;
  return TRUE;
}

static bool_t
xdrstdio_putbytes (XDR *xdrs, char *addr, unsigned int len)
{
  if ((len != 0) && (fwrite (addr, (int) len, 1,
			     (FILE *) xdrs->x_private) != 1))
    return FALSE;
  return TRUE;
}

static unsigned int
xdrstdio_getpos (XDR *xdrs)
{
  return (unsigned int) ftell ((FILE *) xdrs->x_private);
}

static bool_t
xdrstdio_setpos (XDR *xdrs, unsigned int pos)
{
  return fseek ((FILE *) xdrs->x_private, (long) pos, 0) < 0 ? FALSE : TRUE;
}

static xdr_int32_t *
xdrstdio_inline (XDR *xdrs, int len)
{
  /*
   * Must do some work to implement this: must insure
   * enough data in the underlying stdio buffer,
   * that the buffer is aligned so that we can indirect through a
   * long *, and stuff this pointer in xdrs->x_buf.  Doing
   * a fread or fwrite to a scratch buffer would defeat
   * most of the gains to be had here and require storage
   * management on this buffer, so we don't do this.
   */
  return NULL;
}

static bool_t
xdrstdio_getint32 (XDR *xdrs, xdr_int32_t *ip)
{
  xdr_int32_t mycopy;

  if (fread ((char *) &mycopy, 4, 1, (FILE *) xdrs->x_private) != 1)
    return FALSE;
  *ip = xdr_ntohl (mycopy);
  return TRUE;
}

static bool_t
xdrstdio_putint32 (XDR *xdrs, xdr_int32_t *ip)
{
  xdr_int32_t mycopy = xdr_htonl (*ip);

  ip = &mycopy;
  if (fwrite ((char *) ip, 4, 1, (FILE *) xdrs->x_private) != 1)
    return FALSE;
  return TRUE;
}


/* Unimplemented routines - check the sunrpc still in glibc source,
 * and copy them here if you need them. Make sure that long long and
 * similar datatypes are portable, though, and that they produce the
 * right size on the system you intend to use it on.
 *
 * xdr_hyper, xdr_u_hyper, xdr_longlong_t, xdr_u_longlong_t
 * xdr_enum, xdr_bytes, xdr_union, xdr_netobj, xdr_wrapstring,
 * xdr_array.  / Erik Lindahl 2002-03-25
 */


#else /* dummy to avoid warning about empty object file */
static int gmxdummy;
#endif
