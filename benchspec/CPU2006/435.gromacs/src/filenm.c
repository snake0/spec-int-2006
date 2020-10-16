/*
 * $Id: filenm.c,v 1.61 2002/02/28 10:49:22 spoel Exp $
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
 * Great Red Owns Many ACres of Sand 
 */
static char *SRCID_filenm_c = "$Id: filenm.c,v 1.61 2002/02/28 10:49:22 spoel Exp $";
#include <string.h>
#include "sysstuff.h"
#include "typedefs.h"
#include "smalloc.h"
#include "string2.h"
#include "fatal.h"
#include "filenm.h"
#include "futil.h"
#include "wman.h"
#include "macros.h"

/* XDR should be available on all platforms now, 
 * but we keep the possibility of turning it off...
 */
#define USE_XDR

/* Use bitflag ... */
#define IS_SET(fn) ((fn.flag & ffSET) != 0)
#define IS_OPT(fn) ((fn.flag & ffOPT) != 0)
#define UN_SET(fn) (fn.flag = (fn.flag & ~ffSET))
#define DO_SET(fn) (fn.flag = (fn.flag |  ffSET))

enum { eftASC, eftBIN, eftXDR, eftGEN, eftNR };

/* To support multiple file types with one general (eg TRX) we have 
 * these arrays.
 */
static    int trxs[]={
#ifdef USE_XDR 
  efXTC, efTRR, 
#endif
  efTRJ, efGRO, efG96, efPDB, efG87 };
#define NTRXS asize(trxs)

static    int trns[]={ 
#ifdef USE_XDR
  efTRR, 
#endif
  efTRJ };
#define NTRNS asize(trns)

static    int stos[]={ efGRO, efG96, efPDB, efBRK, efENT};
#define NSTOS asize(stos)

static    int stxs[]={ efGRO, efG96, efPDB, efBRK, efENT,
#ifdef USE_XDR 
		       efTPR, 
#endif 
		       efTPB, efTPA };
#define NSTXS asize(stxs)

static    int enxs[]={ 
#ifdef USE_XDR
  efEDR, 
#endif
  efENE };
#define NENXS asize(enxs)

static    int tpxs[]={ 
#ifdef USE_XDR
  efTPR, 
#endif
  efTPB, efTPA };
#define NTPXS asize(tpxs)

static    int tpss[]={ 
#ifdef USE_XDR
  efTPR, 
#endif
  efTPB, efTPA, efGRO, efG96, efPDB, efBRK, efENT };
#define NTPSS asize(tpss)

typedef struct {
  int  ftype;
  char *ext;
  char *defnm;
  char *defopt;
  char *descr;
  int  ntps;
  int  *tps;
} t_deffile;

/* this array should correspond to the enum in include/types/filenm.h */
static t_deffile deffile[efNR] = {
  { eftASC, ".mdp", "grompp", "-f", "grompp input file with MD parameters"   },
  { eftASC, ".gct", "gct",    "-f", "General coupling stuff"                 },
  { eftGEN, ".???", "traj",   "-f", "Generic trajectory: xtc trr trj gro g96 pdb", NTRXS, trxs },
  { eftGEN, ".???", "traj",   NULL, "Full precision trajectory: trr trj",          NTRNS, trns },
  { eftXDR, ".trr", "traj",   NULL, "Trajectory in portable xdr format"      },
  { eftBIN, ".trj", "traj",   NULL, "Trajectory file (architecture specific)"         },
  { eftXDR, ".xtc", "traj",   NULL, "Compressed trajectory (portable xdr format)"},
  { eftASC, ".g87", "gtraj",  NULL, "Gromos-87 ASCII trajectory format"      },
  { eftGEN, ".???", "ener",   NULL, "Generic energy: edr ene",                     NENXS, enxs },
  { eftXDR, ".edr", "ener",   NULL, "Energy file in portable xdr format"     },
  { eftBIN, ".ene", "ener",   NULL, "Energy file"                            },
  { eftGEN, ".???", "conf",   "-c", "Generic structure: gro g96 pdb tpr tpb tpa",  NSTXS, stxs },
  { eftGEN, ".???", "out",    "-o", "Generic structure: gro g96 pdb",              NSTOS, stos },
  { eftASC, ".gro", "conf",   "-c", "Coordinate file in Gromos-87 format"    },
  { eftASC, ".g96", "conf",   "-c", "Coordinate file in Gromos-96 format"    },
  { eftASC, ".pdb", "eiwit",  "-f", "Protein data bank file"                 },
  { eftASC, ".brk", "eiwit",  "-f", "Brookhaven data bank file"              },
  { eftASC, ".ent", "eiwit",  "-f", "Entry in the protein date bank"         },
  { eftASC, ".log", "run",    "-l", "Log file"                               },
  { eftASC, ".xvg", "graph",  "-o", "xvgr/xmgr file"                         },
  { eftASC, ".out", "hello",  "-o", "Generic output file"                    },
  { eftASC, ".ndx", "index",  "-n", "Index file",                            },
  { eftASC, ".top", "topol",  "-p", "Topology file"                          },
  { eftASC, ".itp", "topinc", NULL, "Include file for topology"              },
  { eftGEN, ".???", "topol",  "-s", "Generic run input: tpr tpb tpa",              NTPXS, tpxs },
  { eftGEN, ".???", "topol",  "-s", "Structure+mass(db): tpr tpb tpa gro g96 pdb", NTPSS, tpss },
  { eftXDR, ".tpr", "topol",  "-s", "Portable xdr run input file"            },
  { eftASC, ".tpa", "topol",  "-s", "Ascii run input file"                   },
  { eftBIN, ".tpb", "topol",  "-s", "Binary run input file"                  },
  { eftASC, ".tex", "doc",    "-o", "LaTeX file"                             },
  { eftASC, ".rtp", "residue",NULL, "Residue Type file used by pdb2gmx"      },
  { eftASC, ".atp", "atomtp", NULL, "Atomtype file used by pdb2gmx"          },
  { eftASC, ".hdb", "polar",  NULL, "Hydrogen data base"                     },
  { eftASC, ".dat", "nnnice", NULL, "Generic data file"                      },
  { eftASC, ".dlg", "user",   NULL, "Dialog Box data for ngmx"               },
  { eftASC, ".map", "ss",     NULL, "File that maps matrix data to colors"   },
  { eftASC, ".eps", "plot",   NULL, "Encapsulated PostScript (tm) file"      },
  { eftASC, ".mat", "ss",     NULL, "Matrix Data file"			     },
  { eftASC, ".m2p", "ps",     NULL, "Input file for mat2ps"                  },
  { eftBIN, ".mtx", "hessian","-m", "Hessian matrix"                         },
  { eftASC, ".edi", "sam",    NULL, "ED sampling input"                      },
  { eftASC, ".edo", "sam",    NULL, "ED sampling output"                     },
  { eftASC, ".ppa", "pull",   NULL, "Pull parameters"                        },
  { eftASC, ".pdo", "pull",   NULL, "Pull data output"                       },
  { eftASC, ".hat", "gk",     NULL, "Fourier transform of spread function"   },
  { eftASC, ".xpm", "root",   NULL, "X PixMap compatible matrix file"        }
};

static char *default_file_name=NULL;

#define NZEXT 2
char *z_ext[NZEXT] = { ".gz", ".Z" };

void set_default_file_name(char *name)
{
  int i;

  default_file_name = strdup(name);

  for(i=0; i<efNR; i++)
    deffile[i].defnm = default_file_name;
}

char *ftp2ext(int ftp)
{
  if ((0 <= ftp) && (ftp < efNR))
    return deffile[ftp].ext+1;
  else
    return "unknown";
}

char *ftp2desc(int ftp)
{
  if ((0 <= ftp) && (ftp < efNR))
    return deffile[ftp].descr;
  else
    return "unknown filetype";
}

char *ftp2ftype(int ftp)
{
  if ((ftp >= 0) && (ftp < efNR)) {
    switch (deffile[ftp].ftype) {
    case eftASC: return "ASCII";
    case eftBIN: return "Binary";
    case eftXDR: return "XDR portable";
    case eftGEN: return "";
    default: fatal_error(0,"DEATH HORROR: Unknown filetype in ftp2ftype (%d)",
			 deffile[ftp].ftype);
      break;
    }
  }
  return "unknown";
}


char *ftp2defnm(int ftp)
{
  static char buf[256];
  
  if ((0 <= ftp) && (ftp < efNR)) {
    sprintf(buf,"%s",deffile[ftp].defnm);
    return buf;
  } else
    return NULL;
}

void pr_def(FILE *fp,int ftp)
{
  t_deffile *df;
  char *s=NULL,*ext,*desc,*flst;
  
  df=&(deffile[ftp]);
  /* find default file extension and \tt-ify description */
  flst="";
  if (df->ntps) {
    ext = deffile[df->tps[0]].ext;
    desc= strdup(df->descr);
    s = strstr(desc,": ")+1;
    if (s) {
      s[0] = '\0';
      s++;
      snew(flst,strlen(s)+6);
      strcpy(flst, " \\tt ");
      strcat(flst, s);
    }
  } else {
    ext = df->ext;
    desc= df->descr;
  }
  /* now skip dot */
  if (ext[0])
    ext++;
  else
    ext="";
  /* set file contents type */
  switch (df->ftype) {
  case eftASC: s="Asc";
    break;
  case eftBIN: s="Bin";
    break;
  case eftXDR: s="xdr";
    break;
  case eftGEN: s="";
    break;
  default: 
    fatal_error(0,"Unimplemented filetype %d %d",ftp,df->ftype);
  }
  fprintf(fp,"\\tt %8s & \\tt %3s & %3s & \\tt %2s & %s%s \\\\[-0.1ex]\n",
	  df->defnm, ext, s, df->defopt ? df->defopt : "", 
	  check_tex(desc),check_tex(flst));
}

void pr_fns(FILE *fp,int nf,t_filenm tfn[])
{
  int  i,j;
  char buf[256],*wbuf;
#define OPTLEN 4
#define NAMELEN 14
  fprintf(fp,"%6s %12s  %-12s  %s\n",
	  "Option","Filename","Type","Description");
  fprintf(fp,"------------------------------------------------------------\n");
  for(i=0; (i<nf); i++) {
    sprintf(buf, "%4s %14s  %-12s  %s\n", tfn[i].opt,tfn[i].fn,
	    fileopt(tfn[i].flag),deffile[tfn[i].ftp].descr);
    if ( (strlen(tfn[i].opt)>OPTLEN) && 
	 (strlen(tfn[i].opt)<=((OPTLEN+NAMELEN)-strlen(tfn[i].fn))) ) {
      for(j=strlen(tfn[i].opt); 
	  j<strlen(buf)-(strlen(tfn[i].opt)-OPTLEN)+1; j++)
	buf[j]=buf[j+strlen(tfn[i].opt)-OPTLEN];
    }
    wbuf=wrap_lines(buf,80,35);
    fprintf(fp,"%s",wbuf);
    sfree(wbuf);
  }
  fprintf(fp,"\n");
  fflush(fp);
}

void pr_fopts(FILE *fp,int nf,t_filenm tfn[], int shell)
{
  int i,j;
  
  switch (shell) {
  case eshellCSH:
    for(i=0; (i<nf); i++) {
      fprintf(fp," \"n/%s/f:*.",tfn[i].opt);
      if (deffile[tfn[i].ftp].ntps) {
	fprintf(fp,"{");
	for(j=0; j<deffile[tfn[i].ftp].ntps; j++) {
	  if (j>0)
	    fprintf(fp,",");
	  fprintf(fp,"%s",deffile[deffile[tfn[i].ftp].tps[j]].ext+1);
	}
	fprintf(fp,"}");
      } else
	fprintf(fp,"%s",deffile[tfn[i].ftp].ext+1);
      fprintf(fp,"{");
      for(j=0; j<NZEXT; j++)
	fprintf(fp,",%s",z_ext[j]);
      fprintf(fp,"}/\"");
    }
    break;
  case eshellBASH:
    for(i=0; (i<nf); i++) {
	fprintf(fp,"%s) COMPREPLY=( $(compgen -X '!*.",tfn[i].opt);
      if (deffile[tfn[i].ftp].ntps) {
	fprintf(fp,"+(");
	for(j=0; j<deffile[tfn[i].ftp].ntps; j++) {
	  if (j>0)
	    fprintf(fp,"|");
	  fprintf(fp,"%s",deffile[deffile[tfn[i].ftp].tps[j]].ext+1);
	}
	fprintf(fp,")");
      } else
	fprintf(fp,"%s",deffile[tfn[i].ftp].ext+1);
      fprintf(fp,"*(");
      for(j=0; j<NZEXT; j++) {
	if (j>0)
	  fprintf(fp,"|");
	fprintf(fp,"%s",z_ext[j]);
      }
      fprintf(fp,")' -f $c ; compgen -S '/' -X '.*' -d $c ));;\n");
    }
    break;
  case eshellZSH:
    for(i=0; (i<nf); i++) {
      fprintf(fp,"- 'c[-1,%s]' -g '*.",tfn[i].opt);
      if (deffile[tfn[i].ftp].ntps) {
	fprintf(fp,"(");
	for(j=0; j<deffile[tfn[i].ftp].ntps; j++) {
	  if (j>0)
	    fprintf(fp,"|");
	  fprintf(fp,"%s",deffile[deffile[tfn[i].ftp].tps[j]].ext+1);
	}
	fprintf(fp,")");
      } else
	fprintf(fp,"%s",deffile[tfn[i].ftp].ext+1);
      fprintf(fp,"(");
      for(j=0; j<NZEXT; j++)
	fprintf(fp,"|%s",z_ext[j]);
      fprintf(fp,") *(/)' ");
    }
    break;
  }
}

static void check_opts(int nf,t_filenm fnm[])
{
  int       i;
  t_deffile *df;
  
  for(i=0; (i<nf); i++) {
    df=&(deffile[fnm[i].ftp]);
    if (fnm[i].opt == NULL) {
      if (df->defopt == NULL)
	fatal_error(0,"No default cmd-line option for %s (type %d)\n",
		    deffile[fnm[i].ftp].ext,fnm[i].ftp);
      else
	fnm[i].opt=df->defopt;
    }
  }
}

int fn2ftp(char *fn)
{
  int  i,len;
  char *feptr,*eptr;
  
  if (!fn)
    return efNR;

  len=strlen(fn);
  if ((len >= 4) && (fn[len-4] == '.'))
    feptr=&(fn[len-4]);
  else
    return efNR;
  
  for(i=0; (i<efNR); i++)
    if ((eptr=deffile[i].ext) != NULL)
      if (strcasecmp(feptr,eptr)==0)
	break;
      
  return i;
}

static void set_extension(char *buf,int ftp)
{
  int len,extlen;
  t_deffile *df;

  /* check if extension is already at end of filename */
  df=&(deffile[ftp]);
  len=strlen(buf);
  extlen = strlen(df->ext);
  if ((len <= extlen) || (strcasecmp(&(buf[len-extlen]),df->ext) != 0))
    strcat(buf,df->ext);
}

static void set_grpfnm(t_filenm *fnm,char *name,bool bCanNotOverride)
{
  char buf[256],buf2[256];
  int  i,type;
  bool bValidExt;
  int  nopts;
  int  *ftps;
  
  nopts = deffile[fnm->ftp].ntps;
  ftps  = deffile[fnm->ftp].tps;
  if ((nopts == 0) || (ftps == NULL))
    fatal_error(0,"DEATH HORROR ERROR in %s:%d",__FILE__,__LINE__);

  bValidExt = FALSE;
  if (name && (bCanNotOverride || (default_file_name == NULL))) {
    strcpy(buf,name);
    /* First check whether we have a valid filename already */
    type = fn2ftp(name);
    for(i=0; (i<nopts) && !bValidExt; i++)
      if (type == ftps[i])
	bValidExt = TRUE;
  } else
    /* No name given, set the default name */
    strcpy(buf,ftp2defnm(fnm->ftp));
  
  if (!bValidExt && (fnm->flag & ffREAD)) { 
    /* for input-files only: search for filenames in the directory */ 
    for(i=0; (i<nopts) && !bValidExt; i++) {
      type = ftps[i];
      strcpy(buf2,buf);
      set_extension(buf2,type);
      if (fexist(buf2)) {
	bValidExt = TRUE;
	strcpy(buf,buf2);
      }
    }
  }

  if (!bValidExt)
    /* Use the first extension type */
    set_extension(buf,ftps[0]);

  fnm->fn = strdup(buf);
}

static void set_filenm(t_filenm *fnm,char *name,bool bCanNotOverride)
{
  /* Set the default filename, extension and option for those fields that 
   * are not already set. An extension is added if not present, if fn = NULL
   * or empty, the default filename is given.
   */
  char buf[256];
  int  i,len,extlen;

  if ((fnm->ftp < 0) || (fnm->ftp >= efNR))
    fatal_error(0,"file type out of range (%d)",fnm->ftp);

  if ((fnm->flag & ffREAD) && name && fexist(name)) {
    /* check if filename ends in .gz or .Z, if so remove that: */
    len    = strlen(name);
    for (i=0; i<NZEXT; i++) {
      extlen = strlen(z_ext[i]);
      if (len > extlen)
	if (strcasecmp(name+len-extlen,z_ext[i]) == 0) {
	  name[len-extlen]='\0';
	  break;
	}
    }
  }
  
  if (deffile[fnm->ftp].ntps)
    set_grpfnm(fnm,name,bCanNotOverride);
  else {
    if ((name != NULL) && (bCanNotOverride || (default_file_name == NULL)))
      strcpy(buf,name);
    else
      strcpy(buf,deffile[fnm->ftp].defnm);
    set_extension(buf,fnm->ftp);
    
    fnm->fn=strdup(buf);
  }
}

static void set_filenms(int nf,t_filenm fnm[])
{
  int i;

  for(i=0; (i<nf); i++)
    if (!IS_SET(fnm[i]))
      set_filenm(&(fnm[i]),fnm[i].fn,FALSE);
}

void parse_file_args(int *argc,char *argv[],int nf,t_filenm fnm[],
		     bool bKeep)
{
  int  i,j;
  bool *bRemove;

  check_opts(nf,fnm);
  
  for(i=0; (i<nf); i++)
    UN_SET(fnm[i]);
    
  if (*argc > 1) {
    snew(bRemove,(*argc)+1);
    i=1;
    do {
      for(j=0; (j<nf); j++) {
	if (strcmp(argv[i],fnm[j].opt) == 0) {
	  DO_SET(fnm[j]);
	  bRemove[i]=TRUE;
	  i++;
	  if ((i < *argc) && (argv[i][0] != '-')) {
	    set_filenm(&fnm[j],argv[i],TRUE);
	    bRemove[i]=TRUE;
	    i++;
	  }
	  else
	    set_filenm(&fnm[j],fnm[j].fn,FALSE);

	  break;
	}
      }
      /* No file found corresponding to option argv[i] */
      if (j == nf)
	i++;
    } while (i < *argc);
    
    if (!bKeep) {
      /* Remove used entries */
      for(i=j=0; (i<=*argc); i++) {
	if (!bRemove[i])
	  argv[j++]=argv[i];
      }
      (*argc)=j-1;
    }
    sfree(bRemove);
  }
  
  set_filenms(nf,fnm);
}

char *opt2fn(char *opt,int nfile,t_filenm fnm[])
{
  int i;
  
  for(i=0; (i<nfile); i++)
    if (strcmp(opt,fnm[i].opt)==0)
      return fnm[i].fn;

  fprintf(stderr,"No option %s\n",opt);
  return NULL;
}

char *ftp2fn(int ftp,int nfile,t_filenm fnm[])
{
  int i;
  
  for(i=0; (i<nfile); i++)
    if (ftp == fnm[i].ftp)
      return fnm[i].fn;
  
  fprintf(stderr,"ftp2fn: No filetype %s\n",deffile[ftp].ext);
  return NULL;
}

bool ftp2bSet(int ftp,int nfile,t_filenm fnm[])
{
  int i;
  
  for(i=0; (i<nfile); i++)
    if (ftp == fnm[i].ftp)
      return (bool) IS_SET(fnm[i]);
      
  fprintf(stderr,"ftp2bSet: No filetype %s\n",deffile[ftp].ext);
  
  return FALSE;
}

bool opt2bSet(char *opt,int nfile,t_filenm fnm[])
{
  int i;
  
  for(i=0; (i<nfile); i++)
    if (strcmp(opt,fnm[i].opt)==0)
      return (bool) IS_SET(fnm[i]);

  fprintf(stderr,"No option %s\n",opt);
  
  return FALSE;
}

char *opt2fn_null(char *opt,int nfile,t_filenm fnm[])
{
  int i;
  
  for(i=0; (i<nfile); i++)
    if (strcmp(opt,fnm[i].opt)==0) {
      if (IS_OPT(fnm[i]) && !IS_SET(fnm[i]))
	return NULL;
      else
	return fnm[i].fn;
    }
  fprintf(stderr,"No option %s\n",opt);
  return NULL;
}

char *ftp2fn_null(int ftp,int nfile,t_filenm fnm[])
{
  int i;
  
  for(i=0; (i<nfile); i++)
    if (ftp == fnm[i].ftp) {
      if (IS_OPT(fnm[i]) && !IS_SET(fnm[i]))
	return NULL;
      else
	return fnm[i].fn;
    }
  fprintf(stderr,"ftp2fn: No filetype %s\n",deffile[ftp].ext);
  return NULL;
}

static void add_filters(char *filter,int *n,int nf,int ftp[])
{
  char buf[8];
  int  i;

  sprintf(filter,"*.{");  
  for(i=0; (i<nf); i++) {
    sprintf(buf,"%s",ftp2ext(ftp[i]));
    if (*n > 0)
      strcat(filter,",");
    strcat(filter,buf);
    (*n) ++;
  }
  strcat(filter,"}");
}

char *ftp2filter(int ftp)
{
  int    n;
  static char filter[128];

  filter[0] = '\0';  
  n         = 0;
  switch (ftp) {
  case efENX:
    add_filters(filter,&n,NENXS,enxs);
    break;
  case efTRX:
    add_filters(filter,&n,NTRXS,trxs);
    break;
  case efTRN:
    add_filters(filter,&n,NTRNS,trns);
    break;
  case efSTO:
    add_filters(filter,&n,NSTOS,stos);
    break;
  case efSTX:
    add_filters(filter,&n,NSTXS,stxs);
    break;
  case efTPX:
    add_filters(filter,&n,NTPXS,tpxs);
    break;
  default:
    sprintf(filter,"*%s",ftp2ext(ftp));
    break;
  }
  return filter;
}

bool is_optional(t_filenm *fnm)
{
  return ((fnm->flag & ffOPT) == ffOPT);
}

bool is_output(t_filenm *fnm)
{
  return ((fnm->flag & ffWRITE) == ffWRITE);
}

bool is_set(t_filenm *fnm)
{
  return ((fnm->flag & ffSET) == ffSET);
}  


