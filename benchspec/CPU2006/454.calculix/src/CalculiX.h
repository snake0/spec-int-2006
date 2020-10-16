/*     CALCULIX - A 3-dimensional finite element program                 */
/*              Copyright (C) 1998 Guido Dhondt                          */

/*     This program is free software; you can redistribute it and/or     */
/*     modify it under the terms of the GNU General Public License as    */
/*     published by the Free Software Foundation; either version 2 of    */
/*     the License, or (at your option) any later version.               */

/*     This program is distributed in the hope that it will be useful,   */
/*     but WITHOUT ANY WARRANTY; without even the implied warranty of    */ 
/*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the      */
/*     GNU General Public License for more details.                      */

/*     You should have received a copy of the GNU General Public License */
/*     along with this program; if not, write to the Free Software       */
/*     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.         */

#if !defined SPEC_CPU
#define Linux 1
#define IRIX 2
#define IRIX64 3
#define HP 4

#if ARCH == Linux
#define FORTRAN(A,B) A##_  B
#elif ARCH == IRIX || ARCH == IRIX64
#define FORTRAN(A,B) A##_##B
#elif ARCH == HP
#define FORTRAN(A,B) A##B
#endif
#endif /* ! SPEC_CPU */


#if defined(SPEC_CPU_NO_APPEND_UNDERSCORE) || \
    (defined(SPEC_CPU_WINDOWS) && !defined(SPEC_CPU_APPEND_UNDERSCORE))
#define FORTRAN(A,B) A##B
#else
# define FORTRAN(A,B) A##_  B
#endif

#define NNEW(a,b) (a *)u_calloc((b),sizeof(a))
#define RENEW(a,b,c) a=(b *) realloc((b *)(a),(c)*sizeof(b))

#if defined(SPEC_CPU_NAGF95)
void FORTRAN(allocation,(int *nload_,int *nforc_,int *nboun_,
             int *nk_,int *ne_,int *nmpc_,int *nset_,int *nalset_,
	     int *nmat_,int *ntmat_,int *npmat_,int *norien_,int *nam_,
             int *noprint_,int *neprint_,int *mint_,int *ntrans_,
             int *in,int *itread,char *set,int *meminset,
             int *rmeminset, int *ncs_, int *namtot_, int *ncmat_,
             int *memmpc_, int *ne1d, int *ne2d, int *nflow_, int setLen));
#else
void FORTRAN(allocation,(int *nload_,int *nforc_,int *nboun_,
             int *nk_,int *ne_,int *nmpc_,int *nset_,int *nalset_,
	     int *nmat_,int *ntmat_,int *npmat_,int *norien_,int *nam_,
             int *noprint_,int *neprint_,int *mint_,int *ntrans_,
             int *in,int *itread,char *set,int *meminset,
             int *rmeminset, int *ncs_, int *namtot_, int *ncmat_,
             int *memmpc_, int *ne1d, int *ne2d, int *nflow_));
#endif

void arpack(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	     int lakonLen, int *ne, 
	     int *nodeboun, int *ndirboun, double *xboun, int *nboun, 
	     int *ipompc, int *nodempc, double *coefmpc, char *labmpc,
             int labmpcLen, int *nmpc, 
	     int *nodeforc, int *ndirforc,double *xforc, int *nforc, 
	     int *nelemload, char *sideload, int sideloadLen, double *xload,
	     int *nload, double *p1, double *p2, double *om, double *bodyf, 
	     double *ad, double *au, double *b, int *nactdof, 
	     int *icol, int *jq, int *irow, int *neq, int *nzl, 
	     int *nmethod, int *ikmpc, int *ilmpc, int *ikboun, 
	     int *ilboun, 
	     double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	     double *alcon, int *nalcon, double *alzero, int *ielmat,
	     int *ielorien, int *norien, double *orab, int *ntmat_,
	     double *t0, double *t1, double *t1old,
	     int *ithermal,double *prestr, int *iprestr, 
	     double *vold,int *iperturb, double *sti, int *nzs, 
	     int *nodeprint, int *noprint, int *nelemprint, int *neprint,
	     int *kode, double *adb, double *aub,
	     int *nev, double *tol, int *mxiter, int *ncv,
	     char *noelplab, int noelplabLen, char *nodeflab, int nodeflabLen, double *eei,
             int *iexpl, double *plicon, int *nplicon, double *plkcon,
             int *nplkcon,
             double *xstate, int *npmat_, char *matname, int matnameLen, int *mint_,
             int *ncmat_, int *nstate_, double *ener, char *jobnamec,
             int jobnamecLen, char *output);

void arpackbu(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	     int lakonLen, int *ne, 
	     int *nodeboun, int *ndirboun, double *xboun, int *nboun, 
	     int *ipompc, int *nodempc, double *coefmpc, char *labmpc,
             int labmpcLen, int *nmpc, 
	     int *nodeforc, int *ndirforc,double *xforc, int *nforc, 
	     int *nelemload, char *sideload, int sideloadLen, double *xload,
	     int *nload, double *p1, double *p2, double *om, double *bodyf, 
	     double *ad, double *au, double *b, int *nactdof, 
	     int *icol, int *jq, int *irow, int *neq, int *nzl, 
	     int *nmethod, int *ikmpc, int *ilmpc, int *ikboun, 
	     int *ilboun, 
	     double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	     double *alcon, int *nalcon, double *alzero, int *ielmat,
	     int *ielorien, int *norien, double *orab, int *ntmat_,
	     double *t0, double *t1, double *t1old, 
	     int *ithermal,double *prestr, int *iprestr, 
	     double *vold,int *iperturb, double *sti, int *nzs, 
	     int *nodeprint, int *noprint, int *nelemprint, int *neprint,
	     int *kode, double *adb, double *aub,
	     int *nev, double *tol, int *mxiter, int *ncv,
	     char *noelplab, int noelplabLen, char *nodeflab, int nodeflabLen, double *eei,
             int *iexpl, double *plicon, int *nplicon, double *plkcon,
             int *nplkcon,
             double *xstate, int *npmat_, char *matname, int matnameLen, int *mint_,
             int *ncmat_, int *nstate_, double *ener, char *output);

void arpackcs(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	     int lakonLen, int *ne, 
	     int *nodeboun, int *ndirboun, double *xboun, int *nboun, 
	     int *ipompc, int *nodempc, double *coefmpc, char *labmpc,
             int labmpcLen, int *nmpc, 
	     int *nodeforc, int *ndirforc,double *xforc, int *nforc, 
	     int *nelemload, char *sideload, int sideloadLen, double *xload,
	     int *nload, double *p1, double *p2, double *om, double *bodyf, 
	     double *ad, double *au, double *b, int *nactdof, 
	     int *icol, int *jq, int *irow, int *neq, int *nzl, 
	     int *nmethod, int *ikmpc, int *ilmpc, int *ikboun, 
	     int *ilboun, 
	     double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	     double *alcon, int *nalcon, double *alzero, int *ielmat,
	     int *ielorien, int *norien, double *orab, int *ntmat_,
	     double *t0, double *t1, double *t1old,
	     int *ithermal,double *prestr, int *iprestr, 
	     double *vold,int *iperturb, double *sti, int *nzs, 
	     int *nodeprint, int *noprint, int *nelemprint, int *neprint,
	     int *kode, double *adb, double *aub,
	     int *nev, double *tol, int *mxiter, int *ncv,
	     char *noelplab, int noelplabLen, char *nodeflab, int nodeflabLen, double *eei,
             int *iexpl, double *plicon, int *nplicon, double *plkcon,
             int *nplkcon,
             double *xstate, int *npmat_, char *matname, int matnameLen, int *mint_,
             int *ics, int *ns, int *mpcend, int *ncmat_, int *nstate_,
             double *csab, int *nkon, double *ener, char *jobnamec,
             int jobnamecLen, char *output);

#if defined(SPEC_CPU_NAGF95)
void FORTRAN(calinput,(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	       int *nkon, int *ne,
	       int *nodeboun, int *ndirboun, double *xboun, int *nboun,
	       int *ipompc, int *nodempc, double *coefmpc, int *nmpc,
	       int *nmpc_,int *nodeforc, int *ndirforc, double *xforc,
	       int *nforc, int *nforc_, int *nelemload, char *sideload,
	       double *xload, int *nload, int *nload_,double *p1, double *p2,
	       double *om, double *bodyf, int *nodeprint, int *noprint, 
	       int *nelemprint, int *neprint, int *mpcfree, int *nboun_,
	       int *nev, char *set, int *istartset, 
	       int *iendset, int *ialset, int *nset, int *nalset,
	       double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	       double *alcon, int *nalcon, double *alzero, double *t0,
	       double *t1, char *matname,
	       int *ielmat, char *orname, double *orab, int *ielorien,
	       char *amname, double *amta, int *namta, int *nam,
	       int *nmethod, int *iamforc, int *iamload, int *iamom,
	       int *iambodyf, int *iamt1,int *ithermal, int *iperturb, 
	       int *istat, int *istep, int *nmat,
	       int *ntmat_, int *norien, double *prestr, int *iprestr,
	       int *in, int *isolver, double *tol, int *ncv, 
	       int *mxiter, double *veold, double *tinc,
	       double *tper, double *alpham, double *betam, char *noelplab, 
               char *nodeflab, int *jout, int *nlabel,
	       int *idrct, int *jmax, double *tmin, double *tmax,
	       int *iexpl, double *alpha, double *haftol, int *iamboun,
	       double *plicon, int *nplicon, double *plkcon, int *nplkcon,
	       int *iplas, int *npmat_, int *mint_, int *nk_,
	       double *trab, int *inotr, int *ntrans, int *ikboun,
               int *ilboun, int *ikmpc, int *ilmpc, int *ics,
	       double *dcs, int *ncs_, int *namtot_, int *ns,
               int *nstate_, int *ncmat_, int *iumat, double *csab,
               char *labmpc, int *iponor,double *xnor,int *knor,
	       double *thickn,double *thicke,int *ikforc,int *ilforc,
               double *offset,int *iponoel,int *inoel,int *rig,
               int *infree, int *nshcon, double *shcon, double *cocon,
               int *ncocon, 
	       double *physcon, int *nflow, int *nodeflow, double *xflow,
               int *iamflow, int *nflow_, double *ctrl, int lakonLen, int sideloadLen, 
               int setLen, int matnameLen, int ornameLen, int amnameLen, 
               int noelplabLen, int nodeflabLen, int labmpcLen));    
#else
void FORTRAN(calinput,(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	       int *nkon, int *ne,
	       int *nodeboun, int *ndirboun, double *xboun, int *nboun,
	       int *ipompc, int *nodempc, double *coefmpc, int *nmpc,
	       int *nmpc_,int *nodeforc, int *ndirforc, double *xforc,
	       int *nforc, int *nforc_, int *nelemload, char *sideload, 
	       double *xload, int *nload, int *nload_,double *p1, double *p2,
	       double *om, double *bodyf, int *nodeprint, int *noprint, 
	       int *nelemprint, int *neprint, int *mpcfree, int *nboun_,
	       int *nev, char *set, int *istartset, 
	       int *iendset, int *ialset, int *nset, int *nalset,
	       double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	       double *alcon, int *nalcon, double *alzero, double *t0,
	       double *t1, char *matname,
	       int *ielmat, char *orname, double *orab, int *ielorien,
	       char *amname, double *amta, int *namta, int *nam,
	       int *nmethod, int *iamforc, int *iamload, int *iamom,
	       int *iambodyf, int *iamt1,int *ithermal, int *iperturb, 
	       int *istat, int *istep, int *nmat,
	       int *ntmat_, int *norien, double *prestr, int *iprestr,
	       int *in, int *isolver, double *tol, int *ncv, 
	       int *mxiter, double *veold, double *tinc,
	       double *tper, double *alpham, double *betam, char *noelplab, 
               char *nodeflab, int *jout, int *nlabel,
	       int *idrct, int *jmax, double *tmin, double *tmax,
	       int *iexpl, double *alpha, double *haftol, int *iamboun,
	       double *plicon, int *nplicon, double *plkcon, int *nplkcon,
	       int *iplas, int *npmat_, int *mint_, int *nk_,
	       double *trab, int *inotr, int *ntrans, int *ikboun,
               int *ilboun, int *ikmpc, int *ilmpc, int *ics,
	       double *dcs, int *ncs_, int *namtot_, int *ns,
               int *nstate_, int *ncmat_, int *iumat, double *csab,
               char *labmpc, int *iponor,double *xnor,int *knor,
	       double *thickn,double *thicke,int *ikforc,int *ilforc,
               double *offset,int *iponoel,int *inoel,int *rig,
               int *infree, int *nshcon, double *shcon, double *cocon,
               int *ncocon, 
	       double *physcon, int *nflow, int *nodeflow, double *xflow,
               int *iamflow, int *nflow_, double *ctrl));    
#endif

void cascade(int *ipompc, double **coefmpcp, int **nodempcp, int *nmpc,
   int *mpcfree, int *nodeboun, int *ndirboun, int*nboun, int*ikmpc,
   int *ilmpc, int *ikboun, int *ilboun, int *mpcend, int *mpcmult,
   char *labmpc, int labmpcLen, int *nk, int *memmpc_, int *icascade, int *maxlenmpc,
   int *callfrommain);

#if defined(SPEC_CPU_NAGF95)
void FORTRAN(cprint,(char *text, int *before, int *after, int textLen));
#else
void FORTRAN(cprint,(char *text, int *before, int *after));
#endif

void FORTRAN(datri,(double *au, double *aa, double *ad, int *icol, int *neq, 
	    int *flg));

void FORTRAN(dasol,(double *au, double *aa, double *ad, double *b, int *icol, 
	    int *neq, double *energy));

void FORTRAN(dsaupd,(int *ido, char *bmat, int *n, char *which, int *nev,
	     double *tol, double *resid, int *ncv, double *z, int *ldz,
	     int *iparam, int *ipntr, double *workd, double *workl,
	     int *lworkl, int *info));

void FORTRAN(dseupd,(int *, char *, int *, double *, double *,
	     int *, double *, char *, int *, char *, 
	     int *, double *, double *, int *, double *,
	     int *, int *, int *, double *,
	     double *, int *, int *));

void dyna(double *co, int *nk, int *kon, int *ipkon, char *lakon, int lakonLen, int *ne, 
	  int *nodeboun, int *ndirboun, double *xboun, int *nboun,
	  int *ipompc, int *nodempc, double *coefmpc, char *labmpc, int labmpcLen, int *nmpc, 
	  int *nodeforc,int *ndirforc,double *xforc, int *nforc, 
	  int *nelemload, char *sideload, int sideloadLen, double *xload,
	  int *nload, double *p1, double *p2, double *om, double *bodyf, 
	  int *nactdof, int *neq, int *nzl,int *icol, int *irow, 
	  int *nmethod, int *ikmpc, int *ilmpc, int *ikboun, 
	  int *ilboun,
	  double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	  double *alcon, int *nalcon, double *alzero, int *ielmat,
	  int *ielorien, int *norien, double *orab, int *ntmat_,
	  double *t0, 
	  double *t1,int *ithermal,double *prestr, int *iprestr, 
	  double *vold,int *iperturb, double *sti, int *nzs, 
	  double *tinc, double *tper, double *alpham, double *betam,
	  double *veold, char *amname, int amnameLen, double *amta,
	  int *namta, int *nam, int *iamforc, int *iamload,
	  int *iamom, int *iambodyf, int *iamt1, 
	  int *jout, int *nodeprint, int *noprint, int *nelemprint,
	  int *neprint, int *kode, char *noelplab, int noelplabLen, char *nodeflab,
	  int nodeflabLen, double *eei, double *xforcold, double *xloadold,
          double *omold, double *bodyfold, double *t1old, int *iamboun,
          double *xbounold, int *iexpl, double *plicon, int *nplicon,
          double *plkcon,int *nplkcon,
          double *xstate, int *npmat_, char *matname, int matnameLen, int *mint_,
          int *ncmat_, int *nstate_, double *ener, char *jobnamec,
          int jobnamecLen, double *ttime);

#if defined(SPEC_CPU_NAGF95)
void FORTRAN(dynsolv,(double *b, double *z, double *d, double *zeta, int *nev,
	      int *neq, double *tinc, int *jinc, int *jout, double *vold,
	      double *xforcold, int *nodeforc, int *ndirforc,
	      double *xforc, int *iamforc, int *nforc, double *xloadold,
	      double *xload, int *iamload, int *nelemload, char *sideload, 
	      int *nload, double *omold, double *om, int *iamom,
	      double *bodyfold, double *bodyf, int *iambodyf, double *t1old,
	      double *t1, int *iamt1, int *nk, double *amta, int *namta, 
	      int *nam, double *ampli, double *aa, double *bb, double *bj, 
	      double *v, int *nodeboun, int *ndirboun, double *xboun, 
	      int *nboun, int *ipompc, int *nodempc, double *coefmpc, 
              char *labmpc,
	      int *nmpc, int *nactdof, int *nodeprint, int *noprint,
	      int *iperturb, int *nmethod, double *co, int *kon,
	      int *ipkon, char *lakon,
	      int *ne, double *stn, int *nelemprint, int *neprint,
	      double *stx,
	      double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	      double *alcon, int *nalcon, double *alzero, int *ielmat,
	      int *ielorien, int *norien, double *orab, int *ntmat_,
	      double *t0, int *ithermal, int *kode, double *cv,
	      double *cd, int *inum, double *prestr, int *iprestr,
	      int *ikmpc, int *ilmpc, int *ikboun, int *ilboun,
	      double *p1, double *p2, char *noelplab, 
	      char *nodeflab, double *eei, double *een,
	      double *sti, double *f, double *fn, double *xforcact,
	      double *xloadact, double *omact, double *bodyfact,
              double *t1act, double *xbounold, double *xbounact,
              int *iamboun, int *iexpl, double *plicon, int *nplicon,
              double *plkcon,
	      int *nplkcon, double *xstateini,double *xstiff,
              double *xstate, int *npmat_, double *epn, char *matname,
              int *mint_, int *ncmat_, int *nstate_, double *stiini,
              double *vini, double *ener, double *enern, double *xstaten,
              double *ttime, double *eeiini, double *enerini,double *cocon,
              int *ncocon, int sideloadLen, int labmpcLen, int lakonLen, 
              int noelplabLen, int nodeflabLen, int matnameLen));
#else
void FORTRAN(dynsolv,(double *b, double *z, double *d, double *zeta, int *nev,
	      int *neq, double *tinc, int *jinc, int *jout, double *vold,
	      double *xforcold, int *nodeforc, int *ndirforc,
	      double *xforc, int *iamforc, int *nforc, double *xloadold,
	      double *xload, int *iamload, int *nelemload, char *sideload, 
	      int *nload, double *omold, double *om, int *iamom,
	      double *bodyfold, double *bodyf, int *iambodyf, double *t1old,
	      double *t1, int *iamt1, int *nk, double *amta, int *namta, 
	      int *nam, double *ampli, double *aa, double *bb, double *bj, 
	      double *v, int *nodeboun, int *ndirboun, double *xboun, 
	      int *nboun, int *ipompc, int *nodempc, double *coefmpc, 
              char *labmpc,
	      int *nmpc, int *nactdof, int *nodeprint, int *noprint,
	      int *iperturb, int *nmethod, double *co, int *kon,
	      int *ipkon, char *lakon,
	      int *ne, double *stn, int *nelemprint, int *neprint,
	      double *stx,
	      double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	      double *alcon, int *nalcon, double *alzero, int *ielmat,
	      int *ielorien, int *norien, double *orab, int *ntmat_,
	      double *t0, int *ithermal, int *kode, double *cv,
	      double *cd, int *inum, double *prestr, int *iprestr,
	      int *ikmpc, int *ilmpc, int *ikboun, int *ilboun,
	      double *p1, double *p2, char *noelplab, 
	      char *nodeflab, double *eei, double *een,
	      double *sti, double *f, double *fn, double *xforcact,
	      double *xloadact, double *omact, double *bodyfact,
              double *t1act, double *xbounold, double *xbounact,
              int *iamboun, int *iexpl, double *plicon, int *nplicon,
              double *plkcon,
	      int *nplkcon, double *xstateini,double *xstiff,
              double *xstate, int *npmat_, double *epn, char *matname,
              int *mint_, int *ncmat_, int *nstate_, double *stiini,
              double *vini, double *ener, double *enern, double *xstaten,
              double *ttime, double *eeiini, double *enerini,double *cocon,
              int *ncocon));
#endif

#if defined(SPEC_CPU_NAGF95)
void FORTRAN(envtemp,(int *itg,int *matg,int *ntg,int *ntr,char *sideload,
                      int *nelemload,int *ipkon,int *kon,char *lakon,
                      int *ielmat,int *ne, int *nload, int *iptri,
                      int *kontri, int *ntri, int *nloadtr, int sideloadLen, 
                      int lakonLen));
#else
void FORTRAN(envtemp,(int *itg,int *matg,int *ntg,int *ntr,char *sideload,
                      int *nelemload,int *ipkon,int *kon,char *lakon,
                      int *ielmat,int *ne, int *nload, int *iptri,
                      int *kontri, int *ntri, int *nloadtr));
#endif

#if defined(SPEC_CPU_NAGF95)
void FORTRAN(frd,(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	  int *ne, double *v, 
	  double *stn, int *inum, int *nmethod, 
	  int *kode, char *nodeflab, double *een, double *t1,
          double *fn, double *time, double *epl,int *ielmat,char *matname,
          double *enern, double *xstaten, int *nstate_, int *istep,
          int *iinc, int lakonLen, int nodeflabLen, int matnameLen));
#else
void FORTRAN(frd,(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	  int *ne, double *v, 
	  double *stn, int *inum, int *nmethod, 
	  int *kode, char *nodeflab, double *een, double *t1,
          double *fn, double *time, double *epl,int *ielmat,char *matname,
          double *enern, double *xstaten, int *nstate_, int *istep,
          int *iinc));
#endif

void FORTRAN(frdclose,());

void frdcyc(double *co,int *nk,int *kon,int *ipkon,char *lakon,int lakonLen, int *ne,double *v,
	    double *stn,int *inum,int *nmethod,int *kode,char *nodeflab,
	    int nodeflabLen, double *een,double *t1,double *fn,double *time,double *epn,
	    int *ielmat,char *matname, int matnameLen, int *ns, double *csab, int *nkon,
	    double *enern, double *xstaten, int *nstate_, int *istep,
            int *iinc, int *iperturb, double *ener, int *mint_, char *output, int outputLen);

void FORTRAN(getfneig,(char *fneig));

void insert(int *ipointer, int **mast1p, int **mast2p, int *i1,
	    int *i2, int *ifree, int *nzs_);

void FORTRAN(isortii,(int *ix, int *iy, int *n, int *kflag));

void FORTRAN(iter,(double *coef, int *jcoef, int *ndim, int *n, int *p,
	   int *ip, double *u, double *ubar, double *rhs, double *wksp,
	   int *iwksp, int *nw, int *inw, int *iparm, double *rparm));

#if defined(SPEC_CPU_NAGF95)
void FORTRAN(mafillpr,(double *co, int *nk, int *kon, int *ipkon, char *lakon, 
	       int *ne, 
	       int *nodeboun, int *ndirboun, double *xboun, int *nboun, 
	       int *ipompc, int *nodempc, double *coefmpc, int *nmpc, 
	       int *nodeforc, int *ndirforc, double *xforc, int *nforc, 
	       int *nelemload, char *sideload, double *xload, int *nload, 
	       double *p1, double *p2, double *om, 
	       double *bodyf, double *ad, double *au, double *b,
	       int *nactdof, 
	       int *icol, int *jq, int *neq, int *nmethod, int *ikmpc, 
	       int *ilmpc, int *ikboun, int *ilboun,
	       double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	       double *alcon, int *nalcon, double *alzero, int *ielmat,
	       int *ielorien, int *norien, double *orab, int *ntmat_,
	       double *t0, double *t1,
	       int *ithermal,double *prestr, int *iprestr, double *vold, 
	       int *iperturb,double *stx, double *sti, double *eei,
               int *iexpl, double *plicon,
               int *nplicon,double *plkcon,int *nplkcon,
               double *xstiff,
	       int *npmat_, double *dtime, char *matname, int *mint_,
               int *ncmat_,int *mass,int *stiffness,int *buckling,int *rhs,
               int *intscheme, int lakonLen, int sideloadLen, int matnameLen));
#else
void FORTRAN(mafillpr,(double *co, int *nk, int *kon, int *ipkon, char *lakon, 
	       int *ne, 
	       int *nodeboun, int *ndirboun, double *xboun, int *nboun, 
	       int *ipompc, int *nodempc, double *coefmpc, int *nmpc, 
	       int *nodeforc, int *ndirforc, double *xforc, int *nforc, 
	       int *nelemload, char *sideload, double *xload, int *nload, 
	       double *p1, double *p2, double *om, 
	       double *bodyf, double *ad, double *au, double *b,
	       int *nactdof, 
	       int *icol, int *jq, int *neq, int *nmethod, int *ikmpc, 
	       int *ilmpc, int *ikboun, int *ilboun,
	       double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	       double *alcon, int *nalcon, double *alzero, int *ielmat,
	       int *ielorien, int *norien, double *orab, int *ntmat_,
	       double *t0, double *t1,
	       int *ithermal,double *prestr, int *iprestr, double *vold, 
	       int *iperturb,double *stx, double *sti, double *eei,
               int *iexpl, double *plicon,
               int *nplicon,double *plkcon,int *nplkcon,
               double *xstiff,
	       int *npmat_, double *dtime, char *matname, int *mint_,
               int *ncmat_,int *mass,int *stiffness,int *buckling,int *rhs,
               int *intscheme));
#endif

#if defined(SPEC_CPU_NAGF95)
void FORTRAN(mafillsm,(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	       int *ne, int *nodeboun, int *ndirboun, double *xboun, 
	       int *nboun,int *ipompc, int *nodempc, double *coefmpc, 
	       int *nmpc, int *nodeforc,int *ndirforc,
	       double *xforc, int *nforc, int *nelemload, char *sideload,
	       double *xload,
	       int *nload, double *p1, double *p2, double *om, double *bodyf, 
	       double *ad, double *au, double *b, double *bb, int *nactdof, 
	       int *icol, int *jq, int *irow, int *neq, int *nzl, 
	       int *nmethod, int *ikmpc, int *ilmpc, int *ikboun, 
	       int *ilboun,
	       double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	       double *alcon, int *nalcon, double *alzero, int *ielmat,
	       int *ielorien, int *norien, double *orab, int *ntmat_,
	       double *t0, double *t1, int *ithermal,
	       double *prestr, int *iprestr, double *vold,
	       int *iperturb, double *sti, int *nzs, double *stx,
	       double *adb, double *aub, double *eei, int *iexpl,
               double *plicon,
               int *nplicon,double *plkcon,int *nplkcon,
               double *xstiff, 
	       int *npmat_, double *dtime, char *matname, int *mint_,
               int *ncmat_,int *mass,int *stiffness,int *buckling,int *rhs,
               int *intscheme, double *physcon, double *shcon, int *nshcon,
               double *cocon, int *ncocon, int lakonLen, int sideloadLen, 
               int matnameLen));
#else
void FORTRAN(mafillsm,(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	       int *ne, int *nodeboun, int *ndirboun, double *xboun, 
	       int *nboun,int *ipompc, int *nodempc, double *coefmpc, 
	       int *nmpc, int *nodeforc,int *ndirforc,
	       double *xforc, int *nforc, int *nelemload, char *sideload,
	       double *xload,
	       int *nload, double *p1, double *p2, double *om, double *bodyf, 
	       double *ad, double *au, double *b, double *bb, int *nactdof, 
	       int *icol, int *jq, int *irow, int *neq, int *nzl, 
	       int *nmethod, int *ikmpc, int *ilmpc, int *ikboun, 
	       int *ilboun,
	       double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	       double *alcon, int *nalcon, double *alzero, int *ielmat,
	       int *ielorien, int *norien, double *orab, int *ntmat_,
	       double *t0, double *t1, int *ithermal,
	       double *prestr, int *iprestr, double *vold,
	       int *iperturb, double *sti, int *nzs, double *stx,
	       double *adb, double *aub, double *eei, int *iexpl,
               double *plicon,
               int *nplicon,double *plkcon,int *nplkcon,
               double *xstiff, 
	       int *npmat_, double *dtime, char *matname, int *mint_,
               int *ncmat_,int *mass,int *stiffness,int *buckling,int *rhs,
               int *intscheme, double *physcon, double *shcon, int *nshcon,
               double *cocon, int *ncocon));
#endif

#if defined(SPEC_CPU_NAGF95)
void FORTRAN(mafillsmcs,(double *co, int *nk, int *kon, int *ipkon, 
               char *lakon,
	       int *ne, int *nodeboun, int *ndirboun, double *xboun, 
	       int *nboun,int *ipompc, int *nodempc, double *coefmpc, 
	       int *nmpc, int *nodeforc,int *ndirforc,
	       double *xforc, int *nforc, int *nelemload, char *sideload,
	       double *xload,
	       int *nload, double *p1, double *p2, double *om, double *bodyf, 
	       double *ad, double *au, double *b, double *bb, int *nactdof, 
	       int *icol, int *jq, int *irow, int *neq, int *nzl, 
	       int *nmethod, int *ikmpc, int *ilmpc, int *ikboun, 
	       int *ilboun,
	       double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	       double *alcon, int *nalcon, double *alzero, int *ielmat,
	       int *ielorien, int *norien, double *orab, int *ntmat_,
	       double *t0, double *t1, int *ithermal,
	       double *prestr, int *iprestr, double *vold,
	       int *iperturb, double *sti, int *nzs, double *stx,
	       double *adb, double *aub, double *eei, int *iexpl,
               double *plicon,
               int *nplicon,double *plkcon,int *nplkcon,
              double *xstiff, 
	       int *npmat_, double *dtime, char *matname, int *mint_,
               int *ics, int *ns, int *nm, int *ncmat_,char *labmpc,int *mass,
               int *stiffness,int *buckling,int *rhs,
               int *intscheme, int lakonLen, int sideloadLen, int matnameLen));
#else
void FORTRAN(mafillsmcs,(double *co, int *nk, int *kon, int *ipkon, 
               char *lakon,
	       int *ne, int *nodeboun, int *ndirboun, double *xboun, 
	       int *nboun,int *ipompc, int *nodempc, double *coefmpc, 
	       int *nmpc, int *nodeforc,int *ndirforc,
	       double *xforc, int *nforc, int *nelemload, char *sideload,
	       double *xload,
	       int *nload, double *p1, double *p2, double *om, double *bodyf, 
	       double *ad, double *au, double *b, double *bb, int *nactdof, 
	       int *icol, int *jq, int *irow, int *neq, int *nzl, 
	       int *nmethod, int *ikmpc, int *ilmpc, int *ikboun, 
	       int *ilboun,
	       double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	       double *alcon, int *nalcon, double *alzero, int *ielmat,
	       int *ielorien, int *norien, double *orab, int *ntmat_,
	       double *t0, double *t1, int *ithermal,
	       double *prestr, int *iprestr, double *vold,
	       int *iperturb, double *sti, int *nzs, double *stx,
	       double *adb, double *aub, double *eei, int *iexpl,
               double *plicon,
               int *nplicon,double *plkcon,int *nplkcon,
              double *xstiff, 
	       int *npmat_, double *dtime, char *matname, int *mint_,
               int *ics, int *ns, int *nm, int *ncmat_,char *labmpc,int *mass,
               int *stiffness,int *buckling,int *rhs,
               int *intscheme));
#endif

void mastruct(int *nk, int *kon, int *ipkon, char*lakon, int *ne,
	      int *nodeboun, int *ndirboun, int *nboun, int *ipompc,
	      int *nodempc, int *nmpc, int *nactdof, int *icol,
	      int *jq, int **mast1p, int **irowp, int *isolver, int *neq,
	      int *nnn, int *ikmpc, int *ilmpc, int *ikcol,
	      int *ipointer, int *nsky, int *nzs, int *nmethod,
              int *ithermal);

void mastructcs(int *nk, int *kon, int *ipkon, char *lakon,
	       int lakonLen, int *ne, int *nodeboun,
	       int *ndirboun, int *nboun, int *ipompc, int *nodempc,
	       int *nmpc, int *nactdof, int *icol, int *jq, int **mast1p,
	       int **irowp, int *isolver, int *neq, int *nnn, 
	       int *ikmpc, int *ilmpc, int *ikcol, int *ipointer,
	       int *nsky, int *nzs, int *nmethod, int *ics, int *ns,
               char *labmpc, int labmpcLen);

void FORTRAN(nident,(int *x, int *px, int *n, int *id));

void nonlingeo(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	     int lakonLen, int *ne, 
	     int *nodeboun, int *ndirboun, double *xboun, int *nboun, 
	     int *ipompc, int **nodempcp, double **coefmpcp, char *labmpc,
             int labmpcLen, int *nmpc, 
	     int *nodeforc, int *ndirforc,double *xforc, int *nforc, 
	     int *nelemload, char *sideload, int sideloadLen, double *xload,
	     int *nload, double *p1, double *p2, double *om, double *bodyf, 
	     double *ad, double *au, double *b, int *nactdof, 
	     int **icolp, int *jq, int **irowp, int *neq, int *nzl, 
	     int *nmethod, int *ikmpc, int *ilmpc, int *ikboun, 
	     int *ilboun,
	     double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	     double *alcon, int *nalcon, double *alzero, int *ielmat,
	     int *ielorien, int *norien, double *orab, int *ntmat_,
	     double *t0, double *t1, double *t1old, 
	     int *ithermal,double *prestr, int *iprestr, 
	     double *vold,int *iperturb, double *sti, int *nzs, 
	     int *nodeprint, int *noprint, int *nelemprint, int *neprint,
	     int *kode, double *adb, double *aub, 
	     char *noelplab, int noelplabLen, char *nodeflab, int nodeflabLen, int *idrct,
	     int *jmax, int *jout, double *tinc, double *tper,
	     double *tmin, double *tmax, double *eei, double *xbounold,
	     double *xforcold, double *xloadold, double *omold,
             double *bodyfold, double *veold, double *accold,
             char *amname, int amnameLen, double *amta, int *namta, int *nam,
             int *iamforc, int *iamload, int *iamom, int *iambodyf,
             int *iamt1, double *alpha, double *haftol, int *iexpl,
	     int *iamboun, double *plicon, int *nplicon, double *plkcon,
	     int *nplkcon,
             double *xstate, int *npmat_, int *istep, double *ttime,
	     char *matname, int matnameLen, double *qaold, int *mint_,
             int *isolver, int *ncmat_, int *nstate_, int *iumat,
             int *ns, double *csab, int *nkon, double *ener, int *mpcinfo,
             int *nnn, char *output, int outputLen,
             int *nodeflow, int *iamflow, double *xflow,
             double *shcon, int *nshcon, double *cocon, int *ncocon,
             double *physcon, double *xflowold, int *nflow, double *ctrl);

#if defined(SPEC_CPU_NAGF95)
void FORTRAN(nonlinmpc,(double *co,double *vold,int *ipompc,int *nodempc,
		   double *coefmpc,char *labmpc,int *nmpc,int *ikboun,
		   int *ilboun,int *nboun,double *xbounact,double *aux,
		   int *iaux, int *maxlenmpc, int *ikmpc, int *ilmpc,
                   int *icascade, int *kon, int *ipkon, char *lakon,
		   int *ne, int labmpcLen, int lakonLen));
#else
void FORTRAN(nonlinmpc,(double *co,double *vold,int *ipompc,int *nodempc,
		   double *coefmpc,char *labmpc,int *nmpc,int *ikboun,
		   int *ilboun,int *nboun,double *xbounact,double *aux,
		   int *iaux, int *maxlenmpc, int *ikmpc, int *ilmpc,
                   int *icascade, int *kon, int *ipkon, char *lakon,
		   int *ne));
#endif

void FORTRAN(op,(int *, double *, double *, double *, double *, double *, int *,
	 int *, int *));

#if defined(SPEC_CPU_NAGF95)
void FORTRAN(openfile,(char *jobname, char *output, int jobnameLen, int outputLen));
#else
void FORTRAN(openfile,(char *jobname, char *output));
#endif

#if defined(SPEC_CPU_NAGF95)
void FORTRAN(out,(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	  int *ne, double *v, 
	  double *stn, int *inum, int *nmethod, 
	  int *kode, char *nodeflab, double *een, double *t1,
          double *fn, double *time, double *epl,int *ielmat,char *matname,
          double *enern, double *xstaten, int *nstate_, int *istep,
          int *iinc, int *iperturb, double *ener, int *mint_, char *output, 
	  int lakonLen, int nodeflabLen, int matnameLen, int outputLen));
#else
void FORTRAN(out,(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	  int *ne, double *v, 
	  double *stn, int *inum, int *nmethod, 
	  int *kode, char *nodeflab, double *een, double *t1,
          double *fn, double *time, double *epl,int *ielmat,char *matname,
          double *enern, double *xstaten, int *nstate_, int *istep,
          int *iinc, int *iperturb, double *ener, int *mint_, char *output));
#endif

int cgsolver(double *A, double *x, double *b, int neq, int len, int *ia, int *iz, 
				double *eps, int *niter, int precFlg);

void preiter(double *ad, double **aup, double *b, int **icolp, int **irowp, 
	     int *neq, int *nzs, int *isolver, int *iperturb);
  
void prespooles(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	     int lakonLen, int *ne, 
	     int *nodeboun, int *ndirboun, double *xboun, int *nboun, 
	     int *ipompc, int *nodempc, double *coefmpc, char *labmpc,
             int labmpcLen, int *nmpc, 
	     int *nodeforc, int *ndirforc,double *xforc, int *nforc, 
	     int *nelemload, char *sideload, int sideloadLen, double *xload,
	     int *nload, double *p1, double *p2, double *om, double *bodyf, 
	     double *ad, double *au, double *b, int *nactdof, 
	     int **icolp, int *jq, int **irowp, int *neq, int *nzl, 
	     int *nmethod, int *ikmpc, int *ilmpc, int *ikboun, 
	     int *ilboun,
	     double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	     double *alcon, int *nalcon, double *alzero, int *ielmat,
	     int *ielorien, int *norien, double *orab, int *ntmat_,
	     double *t0, double *t1, double *t1old, 
	     int *ithermal,double *prestr, int *iprestr, 
	     double *vold,int *iperturb, double *sti, int *nzs, 
	     int *nodeprint, int *noprint, int *nelemprint, int *neprint,
	     int *kode, double *adb, double *aub, 
	     char *noelplab, int noelplabLen, char *nodeflab, int nodeflabLen, double *eei,
             int *iexpl, double *plicon, int *nplicon, double *plkcon,
             int *nplkcon,
             double *xstate, int *npmat_, char *matname, int matnameLen, int *isolver,
	     int *mint_, int *ncmat_, int *nstate_, int *ns, double *csab,
             int *nkon, double *ener, double *xbounold,
	     double *xforcold, double *xloadold, double *omold,
             double *bodyfold, char *amname, int amnameLen, double *amta, int *namta,
             int *nam, int *iamforc, int *iamload, int *iamom, int *iambodyf,
             int *iamt1, int *iamboun, double *ttime, char *output, int outputLen);

void profile(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	     int lakonLen, int *ne, 
	     int *nodeboun, int *ndirboun, double *xboun, int *nboun, 
	     int *ipompc, int *nodempc, double *coefmpc, char *labmpc,
             int labmpcLen, int *nmpc, 
	     int *nodeforc, int *ndirforc,double *xforc, int *nforc, 
	     int *nelemload, char *sideload, int sideloadLen, double *xload,
	     int *nload, double *p1, double *p2, double *om, double *bodyf, 
	     double *ad, double *au, double *b, int *nactdof, 
	     int *icol, int *jq, int *neq, 
	     int *nmethod, int *ikmpc, int *ilmpc, int *ikboun, 
	     int *ilboun, 
	     double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	     double *alcon, int *nalcon, double *alzero, int *ielmat,
	     int *ielorien, int *norien, double *orab, int *ntmat_,
	     double *t0,double *t1,  double *t1old,int *ithermal,
             double *prestr, int *iprestr, 
	     double *vold,int *iperturb, double *sti, 
	     int *nodeprint, int *noprint, int *nelemprint, int *neprint,
	     int *kode, int *nsky, char *noelplab, int noelplabLen, char *nodeflab,
	     int nodeflabLen, double *eei, int *iexpl, 
             double *plicon, int *nplicon, double *plkcon,
             int *nplkcon,
             double *xstate, int *npmat_, char *matname, int matnameLen, int *mint_,
             int *ncmat_, int *nstate_, int *ns, double *csab,int *nkon,
             double *ener, double *xbounold,
	     double *xforcold, double *xloadold, double *omold,
             double *bodyfold, char *amname, int amnameLen, double *amta, int *namta,
             int *nam, int *iamforc, int *iamload, int *iamom, int *iambodyf,
             int *iamt1, int *iamboun, double *ttime, char *output, int outputLen);

#if defined(SPEC_CPU_NAGF95)
void FORTRAN(radflowload,(int *itg, int *matg, int *ntg, int *ntr,int *ntm,
       int *nodeflow,double *xflowact,double *ac,double *bc,int *nload,
       char *sideload,int *nelemload,double *xloadact,char *lakon,int *ipiv,
       int *ntmat_,double *vold,double *shcon,int *nshcon,int *ipkon,
       int *kon,double *co,double *pmid,double *e1,double *e2,double *pnor,
       int *iptr,int *kontri,int *ntri,
       int *nloadtr,double *tarea,double *tenv,double *physcon,double *erad,
       double *fij,double *ft,double *dist,int *idist,double *area,
       int *nflow, int sideloadLen, int lakonLen));
#else
void FORTRAN(radflowload,(int *itg, int *matg, int *ntg, int *ntr,int *ntm,
       int *nodeflow,double *xflowact,double *ac,double *bc,int *nload,
       char *sideload,int *nelemload,double *xloadact,char *lakon,int *ipiv,
       int *ntmat_,double *vold,double *shcon,int *nshcon,int *ipkon,
       int *kon,double *co,double *pmid,double *e1,double *e2,double *pnor,
       int *iptr,int *kontri,int *ntri,
       int *nloadtr,double *tarea,double *tenv,double *physcon,double *erad,
       double *fij,double *ft,double *dist,int *idist,double *area,
       int *nflow));
#endif

void FORTRAN(rearrange,(double *au,int *irow,int *icol,int *ndim,int *neq));


#if defined(SPEC_CPU_NAGF95)
void FORTRAN(rectcyl,(double *co,double *v,double *fn,double *stn,
		      double *een,double *csab,int *nk, int *icntrl,
		      double *t, char *nodeflab, int *imag, int nodeflabLen));
#else
void FORTRAN(rectcyl,(double *co,double *v,double *fn,double *stn,
		      double *een,double *csab,int *nk, int *icntrl,
		      double *t, char *nodeflab, int *imag));
#endif

void remastruct(int *ipompc, double **coefmpcp, int **nodempcp, int *nmpc,
              int *mpcfree, int *nodeboun, int *ndirboun, int *nboun,
              int *ikmpc, int *ilmpc, int *ikboun, int *ilboun,
              char *labmpc, int labmpcLen, int *nk,
              int *memmpc_, int *icascade, int *maxlenmpc,
              int *kon, int *ipkon, char *lakon, int lakonLen, int *ne, int *nnn,
              int *nactdof, int *icol, int *jq, int **irowp, int *isolver,
              int *neq, int *nzs,int *nmethod, double **fp, double **fincp,
              double **fextp, double **bp, double **aux2p, double **finip,
              double **fextinip,double **adbp, double **aubp, int *ithermal);

#if defined(SPEC_CPU_NAGF95)
void FORTRAN(renumber,(int *nk, int *kon, int *ipkon, char *lakon, int *ne, 
	       int *ipompc,
	       int *nodempc, int *nmpc, int *nnn, int *npn, int *adj,
	       int *xadj, int *iw, int *mmm, int *xnpn, int lakonLen));
#else
void FORTRAN(renumber,(int *nk, int *kon, int *ipkon, char *lakon, int *ne, 
	       int *ipompc,
	       int *nodempc, int *nmpc, int *nnn, int *npn, int *adj,
	       int *xadj, int *iw, int *mmm, int *xnpn));
#endif

#if defined(SPEC_CPU_NAGF95)
void FORTRAN(results,(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	     int *ne, double *v, double *stn, int *inum, 
	     int *nelemprint, int *neprint, double *stx,
	     double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	     double *alcon, int *nalcon, double *alzero, int *ielmat,
	     int *ielorien, int *norien, double *orab, int *ntmat_,
	     double *t0, double *t1,int *ithermal,double *prestr, 
             int *iprestr, char *noelplab,char *nodeflab, double *eei, 
             double *een, int *iperturb,double *f, double *fn, int *nactdof,
             int *iout, double *qa, int *noprint, int *nodeprint,
	     double *vold, double *b, int *nodeboun, int *ndirboun,
	     double *xboun, int *nboun, int *ipompc, int *nodempc,
	     double *coefmpc, char *labmpc, int *nmpc, int *nmethod, 
             double *vmax,int *neq, double *veold, double *accold,
	     double *beta, double *gamma, double *dtime, double *plicon,
             int *nplicon,double *plkcon,int *nplkcon,
             double *xstateini,double *xstiff,double *xstate, int *npmat_,
	     double *epl, char *matname, int *mint_, int *ielas,
	     int *icmd, int *ncmat_, int *nstate_, double *stiini,
	     double *vini, int *ikboun, int *ilboun, double *ener,
	     double *enern, double *sti, double *xstaten, double *eeiini,
             double *enerini, double *cocon, int *ncocon, int lakonLen, 
             int noelplabLen, int nodeflabLen, int labmpcLen, int matnameLen));
#else
void FORTRAN(results,(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	     int *ne, double *v, double *stn, int *inum, 
	     int *nelemprint, int *neprint, double *stx,
	     double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	     double *alcon, int *nalcon, double *alzero, int *ielmat,
	     int *ielorien, int *norien, double *orab, int *ntmat_,
	     double *t0, double *t1,int *ithermal,double *prestr, 
             int *iprestr, char *noelplab,char *nodeflab, double *eei, 
             double *een, int *iperturb,double *f, double *fn, int *nactdof,
             int *iout, double *qa, int *noprint, int *nodeprint,
	     double *vold, double *b, int *nodeboun, int *ndirboun,
	     double *xboun, int *nboun, int *ipompc, int *nodempc,
	     double *coefmpc, char *labmpc, int *nmpc, int *nmethod, 
             double *vmax,int *neq, double *veold, double *accold,
	     double *beta, double *gamma, double *dtime, double *plicon,
             int *nplicon,double *plkcon,int *nplkcon,
             double *xstateini,double *xstiff,double *xstate, int *npmat_,
	     double *epl, char *matname, int *mint_, int *ielas,
	     int *icmd, int *ncmat_, int *nstate_, double *stiini,
	     double *vini, int *ikboun, int *ilboun, double *ener,
	     double *enern, double *sti, double *xstaten, double *eeiini,
             double *enerini, double *cocon, int *ncocon));
#endif

#if defined(SPEC_CPU_NAGF95)
void FORTRAN(rhs,(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	       int *ne,int *ipompc, int *nodempc, double *coefmpc, 
	       int *nmpc, int *nodeforc,int *ndirforc,
	       double *xforc, int *nforc, int *nelemload, char *sideload,
	       double *xload,int *nload, double *p1, double *p2, double *om,
               double *bodyf, double *bb, int *nactdof, int *neq, 
	       int *nmethod, int *ikmpc, int *ilmpc,
	       double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	       double *alcon, int *nalcon, double *alzero, int *ielmat,
	       int *ielorien, int *norien, double *orab, int *ntmat_,
	       double *t0, double *t1, int *ithermal,double *prestr, 
               int *iprestr, double *vold,int *iperturb,int *iexpl,
               double *plicon,int *nplicon,double *plkcon,int *nplkcon,
               int *npmat_, int lakonLen, int sideloadLen));
#else
void FORTRAN(rhs,(double *co, int *nk, int *kon, int *ipkon, char *lakon,
	       int *ne,int *ipompc, int *nodempc, double *coefmpc, 
	       int *nmpc, int *nodeforc,int *ndirforc,
	       double *xforc, int *nforc, int *nelemload, char *sideload,
	       double *xload,int *nload, double *p1, double *p2, double *om,
               double *bodyf, double *bb, int *nactdof, int *neq, 
	       int *nmethod, int *ikmpc, int *ilmpc,
	       double *elcon, int *nelcon, double *rhcon, int *nrhcon,
	       double *alcon, int *nalcon, double *alzero, int *ielmat,
	       int *ielorien, int *norien, double *orab, int *ntmat_,
	       double *t0, double *t1, int *ithermal,double *prestr, 
               int *iprestr, double *vold,int *iperturb,int *iexpl,
               double *plicon,int *nplicon,double *plkcon,int *nplkcon,
               int *npmat_));
#endif

void FORTRAN(spcmatch,(double *xboun, int *nodeboun, int *ndirboun, int *nboun,
	       double *xbounold, int *nodebounold, int *ndirbounold,
	       int *nbounold, int *ikboun, int *ilboun, double *vold,
	       double *reorder, int *nreorder));

void spooles(double *ad, double *au, double *b,
	     int *icol, int *irow, int *neq, int *nzs);

void FORTRAN(stop,());

int strcmp1(const char *s1, const char *s2);

int strcpy1(char *s1, const char *s2, int length);

void FORTRAN(tempload,(double *xforcold, double *xforc, double *xforcact,
               int *iamforc, int *nforc, double *xloadold, double *xload,
               double *xloadact, int *iamload, int *nload, double *omold,
               double *om, double *omact, int *iamom, double *bodyfold,
               double *bodyf, double *bodyfact, int *iambodyf, 
               double *t1old, double *t1, double *t1act, int *iamt1,
               int *nk, double *amta, int *namta, int *nam, double *ampli,
               double *time, double *reltime, double *ttime, double *dtime,
               int *ithermal, int *nmethod,
	       double *xbounold, double *xboun, double *xbounact,
	       int *iamboun, int *nboun, double *xflowold, double *xflow,
               double *xflowact,int *iamflow,int *nflow));

void *u_calloc(size_t num,size_t size);

void FORTRAN(writebv,(double *, int *));

void FORTRAN(writeev,(double *, int *));

void FORTRAN(writeevcs,(double *, int *, int *));

void FORTRAN(writempc,(int *, int *, double *, char *,int *));

void FORTRAN(writesummary,(int *istep, int *j, int *icutb, int *l, double *ttime,
		   double *time, double *dtime));



