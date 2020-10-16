/*
 * $Id: gmxfio.h,v 1.10 2002/02/28 21:55:49 spoel Exp $
 * 
 *                This source code is part of
 * 
 *                 G   R   O   M   A   C   S
 * 
 *          GROningen MAchine for Chemical Simulations
 * 
 *                        VERSION 3.1
 * Copyright (c) 1991-2001, University of Groningen, The Netherlands
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * If you want to redistribute modifications, please consider that
 * scientific software is very special. Version control is crucial -
 * bugs must be traceable. We will be happy to consider code for
 * inclusion in the official distribution, but derived work must not
 * be called official GROMACS. Details are found in the README & COPYING
 * files - if they are missing, get the official version at www.gromacs.org.
 * 
 * To help us fund GROMACS development, we humbly ask that you cite
 * the papers on the package - you can find them in the top README file.
 * 
 * For more info, check our website at http://www.gromacs.org
 * 
 * And Hey:
 * Grunge ROck MAChoS
 */

#ifndef _gmxfio_h
#define _gmxfio_h

static char *SRCID_gmxfio_h = "$Id: gmxfio.h,v 1.10 2002/02/28 21:55:49 spoel Exp $";

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include "sysstuff.h"
#include "typedefs.h"
#include "xdrf.h"

/* Enumerated for different items in files */
enum { eitemHEADER, eitemIR, eitemBOX, 
       eitemTOP, eitemX, eitemV, eitemF, eitemNR };
       
/* Enumerated for data types in files */
enum { eioREAL, eioINT,   eioNUCHAR, eioUSHORT, 
       eioRVEC, eioNRVEC, eioIVEC,  eioSTRING, eioNR };

/* Functions for reading and writing data */
typedef bool do_func(void *item,int nitem,int eio,
		     char *desc,char *srcfile,int line);
		     
/* Global variables defined in gmxfio.h */
extern do_func *do_read;
extern do_func *do_write;
extern char *itemstr[eitemNR];
extern char *comment_str[eitemNR];

/********************************************************
 * Open and Close 
 ********************************************************/

extern int gmx_fio_open(char *fn,char *mode);
/* Open a new file for reading or writing.
 * The file type will be deduced from the file name.
 * If fn is NULL, stdin / stdout will be used for Ascii I/O (TPA type)
 * mode may be "r", "w", or "a". You should append a "b" to the mode
 * if you are writing a binary file, but the routine will also 
 * doublecheck it and try to do it if you forgot. This has no effect on
 * unix, but is important on windows.
 */
 
extern void gmx_fio_close(int fp);
/* Close the file corresponding to fp (if not stdio)
 * The routine will exit when an invalid fio is handled.
 */

extern void gmx_fio_select(int fp);
/* This routine sets the global variables do_read and do_write
 * to point to the correct routines for fp.
 */

/********************************************************
 * Change properties of the open file
 ********************************************************/

extern void gmx_fio_setprecision(int fio,bool bDouble);
/* Select the floating point precision for reading and writing files */

extern char *gmx_fio_getname(int fio);
/* Return the filename corresponding to the fio index */

extern int gmx_fio_getftp(int fio);
/* Return the filetype corresponding to the fio index */

extern void gmx_fio_setftp_fio(int fio,int ftp);
/* And set it */

extern void gmx_fio_setdebug(int fio,bool bDebug);
/* Set the debug mode */

extern bool gmx_fio_getdebug(int fio);
/* Return  whether debug mode is on in fio  */

extern bool gmx_fio_getread(int fio);
/* Return  whether read mode is on in fio  */

/***************************************************
 * FILE Operations
 ***************************************************/

extern void gmx_fio_rewind(int fio);
/* Rewind the tpa file in fio */

extern void gmx_fio_flush(int fio);
/* Flush the fio */

extern off_t gmx_fio_ftell(int fio);
/* Return file position if possible */

extern void gmx_fio_seek(int fio,off_t fpos);
/* Set file position if possible, quit otherwise */

extern FILE *gmx_fio_getfp(int fio);
/* Return the file pointer itself */

extern XDR *gmx_fio_getxdr(int fio);
/* Return the file pointer itself */

extern void set_comment(char *comment);
/* Add this to the comment string for debugging */

extern void unset_comment(void);
/* Remove previously set comment */


/********************************************************
 * Dirty C macros... Try this in FORTRAN 
 * (Oh, and you can do structured programming in C too) 
 *********************************************************/
#define do_real(item)         (bRead ?\
  do_read ((void *)&(item),1,eioREAL,(#item),__FILE__,__LINE__) : \
  do_write((void *)&(item),1,eioREAL,(#item),__FILE__,__LINE__))
  
#define do_int(item)          (bRead ?\
  do_read ((void *)&(item),1,eioINT,(#item),__FILE__,__LINE__) :\
  do_write((void *)&(item),1,eioINT,(#item),__FILE__,__LINE__))
  
#define do_nuchar(item,n)     (bRead ?\
  do_read ((void *)(item),n,eioNUCHAR,(#item),__FILE__,__LINE__) :\
  do_write((void *)(item),n,eioNUCHAR,(#item),__FILE__,__LINE__))
  
#define do_ushort(item)          (bRead ?\
  do_read ((void *)&(item),1,eioUSHORT,(#item),__FILE__,__LINE__) :\
  do_write((void *)&(item),1,eioUSHORT,(#item),__FILE__,__LINE__))
  
#define do_rvec(item)         (bRead ?\
  do_read ((void *)(item),1,eioRVEC,(#item),__FILE__,__LINE__) :\
  do_write((void *)(item),1,eioRVEC,(#item),__FILE__,__LINE__))
  
#define do_ivec(item)         (bRead ?\
  do_read ((void *)(item),1,eioIVEC,(#item),__FILE__,__LINE__) :\
  do_write((void *)(item),1,eioIVEC,(#item),__FILE__,__LINE__))
  
#define do_string(item)       (bRead ?\
  do_read ((void *)(item),1,eioSTRING,(#item),__FILE__,__LINE__) :\
  do_write((void *)(item),1,eioSTRING,(#item),__FILE__,__LINE__))
  
#define ndo_real(item,n,bOK) {\
  bOK=TRUE;\
  for(i=0; (i<n); i++) {\
    char buf[128];\
    sprintf(buf,"%s[%d]",#item,i);\
    bOK = bOK && (bRead ?\
      do_read ((void *)&((item)[i]),1,eioREAL,buf,__FILE__,__LINE__):\
      do_write((void *)&(item[i]),1,eioREAL,buf,__FILE__,__LINE__));\
  }\
}
     
#define ndo_int(item,n,bOK)  {\
  bOK=TRUE;\
  for(i=0; (i<n); i++) {\
    char buf[128];\
    sprintf(buf,"%s[%d]",#item,i);\
    bOK = bOK && (bRead ?\
      do_read ((void *)&(item[i]),1,eioINT,buf,__FILE__,__LINE__):\
      do_write((void *)&(item[i]),1,eioINT,buf,__FILE__,__LINE__));\
  }\
}
  
#define ndo_rvec(item,n)      (bRead ?\
  do_read ((void *)(item),n,eioNRVEC,(#item),__FILE__,__LINE__) :\
  do_write((void *)(item),n,eioNRVEC,(#item),__FILE__,__LINE__))
  
#define ndo_ivec(item,n,bOK) {\
  bOK=TRUE;\
  for(i=0; (i<n); i++) {\
    char buf[128];\
    sprintf(buf,"%s[%d]",#item,i);\
    bOK = bOK && (bRead ?\
      do_read ((void *)(item)[i],1,eioIVEC,buf,__FILE__,__LINE__):\
      do_write((void *)(item)[i],1,eioIVEC,buf,__FILE__,__LINE__));\
  }\
}
  
#define ndo_string(item,n,bOK) {\
  bOK=TRUE;\
  for(i=0; (i<n); i++) {\
    char buf[128];\
    sprintf(buf,"%s[%d]",#item,i);\
    bOK = bOK && (bRead ?\
      do_read ((void *)(item)[i],1,eioSTRING,buf,__FILE__,__LINE__):\
      do_write((void *)(item)[i],1,eioSTRING,buf,__FILE__,__LINE__));\
  }\
}

#endif
