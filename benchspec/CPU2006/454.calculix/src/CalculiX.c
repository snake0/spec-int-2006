/*     CalculiX - A 3-dimensional finite element program                 */
/*              Copyright (C) 1998 Guido Dhondt                          */

/*     This program is free software; you can redistribute it and/or     */
/*     modify it under the terms of the GNU General Public License as    */
/*     published by the Free Software Foundation(version 2);    */
/*                                                                       */

/*     This program is distributed in the hope that it will be useful,   */
/*     but WITHOUT ANY WARRANTY; without even the implied warranty of    */ 
/*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the      */
/*     GNU General Public License for more details.                      */

/*     You should have received a copy of the GNU General Public License */
/*     along with this program; if not, write to the Free Software       */
/*     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.         */

#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "CalculiX.h"

int main(int argc,char *argv[])
{
  int *kon=NULL, *nodeboun=NULL, *ndirboun=NULL, *ipompc=NULL,
    *nodempc=NULL, *nodeforc=NULL, *ndirforc=NULL,
    *nelemload=NULL, *nodeprint=NULL, *nelemprint=NULL,
    *nnn=NULL, *nactdof=NULL, *icol=NULL,*ics=NULL,
    *jq=NULL, *mast1=NULL, *irow=NULL, *rig=NULL,
    *ikmpc=NULL, *ilmpc=NULL, *ikboun=NULL, *ilboun=NULL,
    *npn=NULL, *adj=NULL, *xadj=NULL, *iw=NULL, *nreorder=NULL,
    *mmm=NULL, *xnpn=NULL, *ikcol=NULL, *ipointer=NULL,
    *istartset=NULL, *iendset=NULL, *ialset=NULL, *ielmat=NULL,
    *ielorien=NULL, *nrhcon=NULL, *nodebounold=NULL, *ndirbounold=NULL,
    *nelcon=NULL, *nalcon=NULL, *iamforc=NULL,  *iamload=NULL,
    *iamt1=NULL, *namta=NULL, *ipkon=NULL, *iamboun=NULL,
    *nplicon=NULL, *nplkcon=NULL, *inotr=NULL, *iponor=NULL, *knor=NULL,
    *ikforc=NULL, *ilforc=NULL, *iponoel=NULL, *inoel=NULL, *nshcon=NULL,
    *ncocon=NULL,  *nodeflow=NULL,*iamflow=NULL;

double *co=NULL, *xboun=NULL, *coefmpc=NULL, *xforc=NULL,
      *xload=NULL, *ad=NULL, *au=NULL, *xbounold=NULL, *xforcold=NULL,
      *b=NULL, *vold=NULL, *sti=NULL, *xloadold=NULL, *xnor=NULL,
      *reorder=NULL,*dcs=NULL, *thickn=NULL, *thicke=NULL, *offset=NULL,
      *elcon=NULL, *rhcon=NULL, *alcon=NULL, *alzero=NULL, *t0=NULL, *t1=NULL,
      *prestr=NULL, *orab=NULL, *amta=NULL, *veold=NULL, *accold=NULL,
      *adb=NULL, *aub=NULL, *t1old=NULL, *eei=NULL, *plicon=NULL, *plkcon=NULL,
      *xstate=NULL, *trab=NULL, *ener=NULL, *shcon=NULL, *cocon=NULL,
      *xflow=NULL,*xflowold=NULL;

 double ctrl[26]={4.5,8.5,9.5,16.5,10.5,4.5,0.,5.5,0.,0.,0.25,0.5,0.75,0.,0.,0.,1.5,0.,0.005,0.01,0.,0.,0.02,1.e-5,1.e-3,1.e-8};

char *sideload=NULL, *set=NULL, *matname=NULL, *orname=NULL, *amname=NULL,
      *noelplab=NULL, *nodeflab=NULL, *lakon=NULL, *labmpc=NULL, 
      jobnamec[132]="",jobnamef[132]="",output[3]="frd";

int sideloadLen=5, setLen=21, matnameLen=20, ornameLen=20, amnameLen=20, noelplabLen=4, 
    nodeflabLen=4, lakonLen=8, labmpcLen=20, jobnamecLen=132, jobnamefLen=132, outputLen=3;

int nk,ne,nboun,nmpc,nforc,nload,noprint,neprint,nset,nalset,nsky,
  nmethod,neq=0,i,mpcfree=1,nev,j,nzl,nam,nbounold=0,nforcold=0,nloadold=0,
  k,nzs,nmpc_=0,nload_=0,nforc_=0,istep,istat,nboun_=0,nflowold=0,
  iperturb=0,nmat,ntmat_=0,norien,ithermal,iprestr,in,kode,isolver=0,
  mxiter,ncv,iamom,iambodyf,jout,nlabel,nkon=0,idrct,jmax,iexpl,
  iplas=0,npmat_=0,mint_=0,ntrans,mpcend,namtot_=0,ns[5],iumat=0,mpcmult,
  icascade,maxlenmpc,mpcinfo[4],ne1d=0,ne2d=0,infree[4]={0,0,0,0},
  callfrommain,nflow,nflow_=0,jin=0;

int itread,*meminset=NULL,*rmeminset=NULL;

int nzs_,nk_=0,ne_=0,nset_=0,nalset_=0,nmat_=0,norien_=0,nam_=0,noprint_=0,
    neprint_=0,ntrans_=0,ncs_=0,nstate_=0,ncmat_=0,memmpc_=0;

double p1[3],p2[3],bodyf[3],om,omold,tol,bodyfold[3],
  tinc,tper,tmin,tmax,alpham=0.,betam=0.,alpha,haftol,ttime=0.,
  qaold=0.,csab[7],physcon[2];

#ifdef SPEC_CPU
FILE *fpfor_ztest;
int  intfor_ztest;
#endif

if(argc==1){printf("Usage: CalculiX.exe -i jobname\n");FORTRAN(stop,());}
else{
  for(i=1;i<argc;i++){
    if(strcmp1(argv[i],"-i")==0) {
    strcpy(jobnamec,argv[i+1]);strcpy1(jobnamef,argv[i+1],132);jin++;break;}
  }
  if(jin==0){strcpy(jobnamec,argv[1]);strcpy1(jobnamef,argv[1],132);}

  for(i=1;i<argc;i++){
    if(strcmp1(argv[i],"-o")==0) {
    strcpy(output,argv[i+1]);break;}
  }
}

#ifdef SPEC_CPU
/* Test whether the portability switch -DSPEC_CPU_NOZMODIFIER needs */
/* to be set.  SPEC has modified CalculiX and SPOOLES to use %zd        */
/* when printing pointers; but not all compilers support this.  So, for */
/* those compilers a portability switch is provided.  Find out whether  */
/* that switch should be used today.  --jh 17 Feb 2005                  */

intfor_ztest = 3;
fpfor_ztest = fopen ("SPECtestformatmodifier_z.txt","w");
fprintf (fpfor_ztest, "The following line should contain a single '3':\n");

#ifdef SPEC_CPU_NOZMODIFIER
fprintf (fpfor_ztest, "%d\n", (size_t) intfor_ztest);
#else
fprintf (fpfor_ztest, "%zd\n", (size_t) intfor_ztest);
#endif /* SPEC_CPU_NOZMODIFIER */

fprintf (fpfor_ztest, "If it said anything else (such as 'zd') try compiling\n");
fprintf (fpfor_ztest, "with -DSPEC_CPU_NOZMODIFIER\n");
fclose(fpfor_ztest);
#endif /* SPEC_CPU */ 

#if defined(SPEC_CPU_NAGF95)
FORTRAN(openfile,(jobnamef,output,132,3));
#else
FORTRAN(openfile,(jobnamef,output));
#endif


printf("\n************************************************************\n\n");
printf("CalculiX DEVELOPMENT VERSION, Copyright(C) 1998 Guido Dhondt\n");
printf("CalculiX comes with ABSOLUTELY NO WARRANTY. This is free\n");
printf("software, and you are welcome to redistribute it under\n");
printf("certain conditions, see gpl.htm\n\n");
printf("************************************************************\n\n");

istep=0;
istat=0;
ithermal=0;
iprestr=0;
in=1;
kode=0;
ns[0]=0;
ns[4]=1;

/* conservative estimate of the fields to be allocated */

for(itread=0;itread<2;itread++){

#if defined(SPEC_CPU_NAGF95)
  FORTRAN(allocation,(&nload_,&nforc_,&nboun_,&nk_,&ne_,&nmpc_,&nset_,&nalset_,
   &nmat_,&ntmat_,&npmat_,&norien_,&nam_,&noprint_,&neprint_,&mint_,&ntrans_,
   &in,&itread,set,meminset,rmeminset,&ncs_,&namtot_,&ncmat_,&memmpc_,&ne1d,
   &ne2d,&nflow_, setLen));
#else
  FORTRAN(allocation,(&nload_,&nforc_,&nboun_,&nk_,&ne_,&nmpc_,&nset_,&nalset_,
   &nmat_,&ntmat_,&npmat_,&norien_,&nam_,&noprint_,&neprint_,&mint_,&ntrans_,
   &in,&itread,set,meminset,rmeminset,&ncs_,&namtot_,&ncmat_,&memmpc_,&ne1d,
   &ne2d,&nflow_));
#endif

  if(itread==0){
    setLen = 21*nset_; // added by yusuf
    set=NNEW(char,21*nset_);
    meminset=NNEW(int,nset_);
    rmeminset=NNEW(int,nset_);}
  else{free(set);free(meminset);free(rmeminset);}
}

nzs_=20000000;

nload=0;nforc=0;nboun=0;nk=0;nmpc=0;nflow=0;
nlabel=8;

for(k=0;k<3;++k){
  p1[k]=0.;
  p2[k]=0.;
  bodyf[k]=0.;
}
om=0.;
iamom=0;
iambodyf=0;

while(istat>=0) {

  /* in order to reduce the number of variables to be transferred to
     the subroutines, the max. field sizes are (for most fields) copied
     into the real sizes */

  nzs=nzs_;
  noprint=noprint_;
  neprint=neprint_;

  if(istep == 0) {
    ne=ne_;
    nset=nset_;
    nalset=nalset_;
    nmat=nmat_;
    norien=norien_;
    ntrans=ntrans_;
    nam=nam_;

    /* allocating space before the first step */

    /* coordinates and topology */

    co=NNEW(double,3*nk_);
    kon=NNEW(int,20*ne_);
    ipkon=NNEW(int,ne_);
    lakonLen = 8*ne_; // added by yusuf
    lakon=NNEW(char,8*ne_);

    /* fields for 1-D and 2-D elements */

    if((ne1d!=0)||(ne2d!=0)){
	iponor=NNEW(int,40*ne_);
	for(i=0;i<40*ne_;i++) iponor[i]=-1;
	xnor=NNEW(double,36*ne1d+24*ne2d);
	knor=NNEW(int,24*(ne1d+ne2d));
	thickn=NNEW(double,2*nk_);
	thicke=NNEW(double,40*ne_);
	offset=NNEW(double,2*ne_);
	iponoel=NNEW(int,nk_);
	inoel=NNEW(int,9*ne1d+24*ne2d);
	rig=NNEW(int,nk_);
	infree[2]=1;
    }

    /* SPC's */

    nodeboun=NNEW(int,nboun_);
    ndirboun=NNEW(int,nboun_);
    iamboun=NNEW(int,nboun_);
    xboun=NNEW(double,nboun_);
    ikboun=NNEW(int,nboun_);
    ilboun=NNEW(int,nboun_);

    /* MPC's */

    ipompc=NNEW(int,nmpc_);
    nodempc=NNEW(int,3*memmpc_);
    for(i=0;i<3*memmpc_;i+=3){nodempc[i+2]=i/3+2;}
    nodempc[3*memmpc_-1]=0;
    coefmpc=NNEW(double,memmpc_);
    labmpcLen = 20*nmpc_; // added by yusuf
    labmpc=NNEW(char,20*nmpc_);
    ikmpc=NNEW(int,nmpc_);
    ilmpc=NNEW(int,nmpc_);


    /* nodal loads */

    nodeforc=NNEW(int,nforc_);
    ndirforc=NNEW(int,nforc_);
    iamforc=NNEW(int,nforc_);
    xforc=NNEW(double,nforc_);
    ikforc=NNEW(int,nforc_);
    ilforc=NNEW(int,nforc_);

    /* distributed loads */

    nelemload=NNEW(int,2*nload_);
    iamload=NNEW(int,2*nload_);
    sideloadLen = 5*nload_; // added by yusuf
    sideload=NNEW(char,5*nload_);
    xload=NNEW(double,2*nload_);

    /* mass flow rate */

    nodeflow=NNEW(int,2*nflow_);
    iamflow=NNEW(int,nflow_);
    xflow=NNEW(double,nflow_);

    /* printing output */

    nodeprint=NNEW(int,noprint);
    nelemprint=NNEW(int,neprint);

    /* set definitions */

    setLen = 21*nset; // added by yusuf
    set=NNEW(char,21*nset);
    istartset=NNEW(int,nset);
    iendset=NNEW(int,nset);
    ialset=NNEW(int,nalset);

    /* (hyper)elastic constants */

    elcon=NNEW(double,(ncmat_+1)*ntmat_*nmat);
    nelcon=NNEW(int,2*nmat);

    /* density */

    rhcon=NNEW(double,2*ntmat_*nmat);
    nrhcon=NNEW(int,nmat);

    /* specific heat */

    shcon=NNEW(double,2*ntmat_*nmat);
    nshcon=NNEW(int,nmat);

    /* thermal expansion coefficients */

    alcon=NNEW(double,7*ntmat_*nmat);
    nalcon=NNEW(int,2*nmat);
    alzero=NNEW(double,nmat);

    /* conductivity */

    cocon=NNEW(double,7*ntmat_*nmat);
    ncocon=NNEW(int,2*nmat);

    /* isotropic and kinematic hardening coefficients*/

    plicon=NNEW(double,(2*npmat_+1)*ntmat_*nmat);
    nplicon=NNEW(int,(ntmat_+1)*nmat);
    plkcon=NNEW(double,(2*npmat_+1)*ntmat_*nmat);
    nplkcon=NNEW(int,(ntmat_+1)*nmat);

    /* material orientation */

    ornameLen = 20*norien; // added by yusuf
    orname=NNEW(char,20*norien);
    orab=NNEW(double,7*norien);
    ielorien=NNEW(int,ne_);

    /* transformations */

    trab=NNEW(double,7*ntrans);
    inotr=NNEW(int,2*nk_);

    /* amplitude definitions */

    amnameLen = 20*nam; // added by yusuf
    amname=NNEW(char,20*nam);
    amta=NNEW(double,2*namtot_);
    namta=NNEW(int,3*nam);

    /* temperatures */

    if((ne1d==0)&&(ne2d==0)){
	t0=NNEW(double,nk_);
	t1=NNEW(double,nk_);}
    else{
	t0=NNEW(double,3*nk_);
	t1=NNEW(double,3*nk_);}
    iamt1=NNEW(int,nk_);

    prestr=NNEW(double,6*ne_);
    veold=NNEW(double,4*nk_);

    ielmat=NNEW(int,ne_);

    matnameLen = 20*nmat; // added by yusuf
    matname=NNEW(char,20*nmat);

    noelplabLen = 4*nlabel; // added by yusuf
    noelplab=NNEW(char,4*nlabel);
    nodeflabLen = 4*nlabel; // added by yusuf
    nodeflab=NNEW(char,4*nlabel);

    /* temporary fields for cyclic symmetry calculations */

    if(ncs_>0){
      ics=NNEW(int,3*ncs_);
      dcs=NNEW(double,4*ncs_);
    }

  }
  else {

    /* allocating and reallocating space for subsequent steps */

    if((nmethod != 4) && ((nmethod != 1) || (iperturb < 2))){
      veold=NNEW(double,4*nk_);
    }
    else{
      RENEW(veold,double,4*nk_);
    }

    if(nmethod != 4){free(accold);}

    RENEW(nodeboun,int,nboun_);
    RENEW(ndirboun,int,nboun_);
    RENEW(xboun,double,nboun_);
    RENEW(ikboun,int,nboun_);
    RENEW(ilboun,int,nboun_);

    RENEW(nodeforc,int,nforc_);
    RENEW(ndirforc,int,nforc_);
    RENEW(xforc,double,nforc_);
    RENEW(ikforc,int,nforc_);
    RENEW(ilforc,int,nforc_);

    RENEW(nelemload,int,2*nload_);
    sideloadLen = 5*nload_; // added by yusuf
    RENEW(sideload,char,5*nload_);
    RENEW(xload,double,2*nload_);

    RENEW(nodeflow,int,2*nflow_);
    RENEW(xflow,double,nflow_);

    RENEW(nodeprint,int,noprint);
    RENEW(nelemprint,int,neprint);

    if(nam > 0) {
      RENEW(iamforc,int,nforc_);
      RENEW(iamload,int,2*nload_);
      RENEW(iamboun,int,nboun_);
      RENEW(iamflow,int,nflow_);
    }

    RENEW(ipompc,int,nmpc_);
/*    RENEW(nodempc,int,3*memmpc_);
    for(i=3*mpcend;i<3*memmpc_;i+=3){nodempc[i+2]=i/3+2;}
    nodempc[3*memmpc_-1]=0;
    RENEW(coefmpc,double,memmpc_);*/
    labmpcLen = 20*nmpc_; // added by yusuf
    RENEW(labmpc,char,20*nmpc_);
    RENEW(ikmpc,int,nmpc_);
    RENEW(ilmpc,int,nmpc_);

    if(ntrans > 0){
      RENEW(inotr,int,2*nk_);
    }

    RENEW(co,double,3*nk_);

    if(ithermal != 0){
	if((ne1d==0)&&(ne2d==0)){
	    RENEW(t0,double,nk_);
	    RENEW(t1,double,nk_);
	}
      if(nam > 0) {RENEW(iamt1,int,nk_);}
    }

  }

  /* reading the input file */

#if defined(SPEC_CPU_NAGF95)
  FORTRAN(calinput,(co,&nk,kon,ipkon,lakon,&nkon,&ne,
            nodeboun,ndirboun,xboun,&nboun,
	    ipompc,nodempc,coefmpc,&nmpc,&nmpc_,nodeforc,ndirforc,xforc,&nforc,
	    &nforc_,nelemload,sideload,xload,&nload,&nload_,p1,p2,&om,bodyf,
	    nodeprint,&noprint,nelemprint,&neprint,&mpcfree,&nboun_,
	    &nev,set,istartset,iendset,
	    ialset,&nset,&nalset,elcon,nelcon,rhcon,nrhcon,alcon,nalcon,
	    alzero,t0,t1,matname,ielmat,orname,orab,ielorien,amname,
            amta,namta,&nam,&nmethod,iamforc,iamload,&iamom,&iambodyf,iamt1,
	    &ithermal,&iperturb,&istat,&istep,&nmat,&ntmat_,&norien,prestr,
	    &iprestr,&in,&isolver,&tol,&ncv,&mxiter,veold,&tinc,&tper,
	    &alpham,&betam,noelplab,nodeflab,&jout,&nlabel,&idrct,
	    &jmax,&tmin,&tmax,&iexpl,&alpha,&haftol,iamboun,plicon,nplicon,
	    plkcon,nplkcon,&iplas,&npmat_,&mint_,&nk_,trab,inotr,&ntrans,
	    ikboun,ilboun,ikmpc,ilmpc,ics,dcs,&ncs_,&namtot_,ns,&nstate_,
	    &ncmat_,&iumat,csab,labmpc,iponor,xnor,knor,thickn,thicke,
	    ikforc,ilforc,offset,iponoel,inoel,rig,infree,nshcon,shcon,
            cocon,ncocon,physcon,&nflow,nodeflow,xflow,iamflow,&nflow_,
            ctrl, lakonLen, sideloadLen, setLen, matnameLen, ornameLen, 
            amnameLen, noelplabLen, nodeflabLen, labmpcLen));
#else
  FORTRAN(calinput,(co,&nk,kon,ipkon,lakon,&nkon,&ne,
            nodeboun,ndirboun,xboun,&nboun,
	    ipompc,nodempc,coefmpc,&nmpc,&nmpc_,nodeforc,ndirforc,xforc,&nforc,
	    &nforc_,nelemload,sideload,xload,&nload,&nload_,p1,p2,&om,bodyf,
	    nodeprint,&noprint,nelemprint,&neprint,&mpcfree,&nboun_,
	    &nev,set,istartset,iendset,
	    ialset,&nset,&nalset,elcon,nelcon,rhcon,nrhcon,alcon,nalcon,
	    alzero,t0,t1,matname,ielmat,orname,orab,ielorien,amname,
            amta,namta,&nam,&nmethod,iamforc,iamload,&iamom,&iambodyf,iamt1,
	    &ithermal,&iperturb,&istat,&istep,&nmat,&ntmat_,&norien,prestr,
	    &iprestr,&in,&isolver,&tol,&ncv,&mxiter,veold,&tinc,&tper,
	    &alpham,&betam,noelplab,nodeflab,&jout,&nlabel,&idrct,
	    &jmax,&tmin,&tmax,&iexpl,&alpha,&haftol,iamboun,plicon,nplicon,
	    plkcon,nplkcon,&iplas,&npmat_,&mint_,&nk_,trab,inotr,&ntrans,
	    ikboun,ilboun,ikmpc,ilmpc,ics,dcs,&ncs_,&namtot_,ns,&nstate_,
	    &ncmat_,&iumat,csab,labmpc,iponor,xnor,knor,thickn,thicke,
	    ikforc,ilforc,offset,iponoel,inoel,rig,infree,nshcon,shcon,
            cocon,ncocon,physcon,&nflow,nodeflow,xflow,iamflow,&nflow_,
            ctrl));
#endif

  if(istep == 1) {

    /* reallocating space in the first step */

    /* allocating and initializing fields pointing to the previous step */

    vold=NNEW(double,4*nk);
    sti=NNEW(double,6*mint_*ne);

    /* residual stresses */

    if(iprestr > 0) {
	RENEW(prestr,double,6*ne);
	for(i=0;i<ne;i++){
	    for(j=0;j<mint_;j++){
		for(k=0;k<6;k++){
		    sti[6*mint_*i+6*j+k]=prestr[6*i+k];
		}
	    }
	}
    }
    else {
	free(prestr);
    }

    if((strcmp1(&nodeflab[24],"ENER")==0)||(strcmp1(&noelplab[24],"ENER")==0))
	ener=NNEW(double,mint_*ne);

    nodebounold=NNEW(int,nboun);
    ndirbounold=NNEW(int,nboun);
    xbounold=NNEW(double,nboun);
    xforcold=NNEW(double,nforc);
    xloadold=NNEW(double,2*nload);
    if(ithermal>1) xflowold=NNEW(double,nflow);
    for(i=0;i<3;i++) bodyfold[i]=0;
    omold=0;
    if(ithermal==1){
      t1old=NNEW(double,nk);
      for(i=0;i<nk;i++) t1old[i]=t0[i];
    }

    /* strains */

    eei=NNEW(double,6*mint_*ne);

    /* element definition */

    RENEW(kon,int,nkon);
    RENEW(ipkon,int,ne);
    lakonLen = 8*ne; // added by yusuf
    RENEW(lakon,char,8*ne);

    /* fields for 1-D and 2-D elements */

    if((ne1d!=0)||(ne2d!=0)){
	RENEW(iponor,int,2*nkon);
	RENEW(xnor,double,infree[0]-1);
	RENEW(knor,int,infree[1]-1);
	free(thickn);
	RENEW(thicke,double,2*nkon);
	RENEW(offset,double,2*ne);
	RENEW(inoel,int,3*(infree[2]-1));
	RENEW(iponoel,int,infree[3]);
	RENEW(rig,int,infree[3]);
    }

    /* set definitions */ 

    setLen = 21*nset; // added by yusuf
    RENEW(set,char,21*nset);
    RENEW(istartset,int,nset);
    RENEW(iendset,int,nset);
    RENEW(ialset,int,nalset);

    /* material properties */

    RENEW(elcon,double,(ncmat_+1)*ntmat_*nmat);
    RENEW(nelcon,int,2*nmat);

    RENEW(rhcon,double,2*ntmat_*nmat);
    RENEW(nrhcon,int,nmat);

    RENEW(alcon,double,7*ntmat_*nmat);
    RENEW(nalcon,int,2*nmat);
    RENEW(alzero,double,nmat);

    matnameLen = 20*nmat; // added by yusuf
    RENEW(matname,char,20*nmat);
    RENEW(ielmat,int,ne);

    /* allocating space for the state variables */

    if(nstate_>0){
      xstate=NNEW(double,nstate_*mint_*ne);
    }

    if(iplas!=0){
      RENEW(plicon,double,(2*npmat_+1)*ntmat_*nmat);
      RENEW(nplicon,int,(ntmat_+1)*nmat);
      RENEW(plkcon,double,(2*npmat_+1)*ntmat_*nmat);
      RENEW(nplkcon,int,(ntmat_+1)*nmat);

      /* initializing the plastic state variables */

      for(i=0;i<ne;i++){
	if(nelcon[ielmat[i]*2-2]<-50){
	  for(j=0;j<mint_;j++){
	    xstate[13*mint_*i+13*j]=0;
	    for(k=1;k<4;k++){
	      xstate[13*mint_*i+13*j+k]=1;
	    }
	    for(k=4;k<13;k++){
	      xstate[13*mint_*i+13*j+k]=0;
	    }
	  }
	}
      }
    }
    else{
      free(plicon);free(nplicon);free(plkcon);free(nplkcon);
    }

    /* material orientation */

    if(norien > 0) {
      ornameLen = 20*norien; // added by yusuf
      RENEW(orname,char,20*norien);
      RENEW(ielorien,int,ne);
      RENEW(orab,double,7*norien);
    }
    else {
      free(orname);
      free(ielorien);
      free(orab);
    }

    /* amplitude definitions */

    if(nam > 0) {
      amnameLen = 20*nam; // added by yusuf
      RENEW(amname,char,20*nam);
      RENEW(namta,int,3*nam);
      RENEW(amta,double,2*namta[3*nam-2]);
    }
    else {
      free(amname);
      free(amta);
      free(namta);
      free(iamforc);
      free(iamload);
      free(iamboun);
    }

    if(ntrans > 0){
      RENEW(trab,double,7*ntrans);
    }
    else{free(trab);free(inotr);}

    if(ithermal == 0){free(t0);free(t1);}
    if((ithermal == 0)||(nam<=0)){free(iamt1);}

    if(ncs_>0){
      RENEW(ics,int,ncs_);
      free(dcs);}

  }
  else{

    /* reallocating space in all but the first step (>1) */

    RENEW(vold,double,4*nk);

    /* if the SPC boundary conditions were changed in the present step,
       they have to be rematched with those in the last step. Removed SPC 
       boundary conditions do not appear any more (this is different from
       forces and loads, where removed forces or loads are reset to zero;
       a removed SPC constraint does not have a numerical value any more) */
       
    reorder=NNEW(double,nboun);
    nreorder=NNEW(int,nboun);
    if(nbounold<nboun){
      RENEW(xbounold,double,nboun);
      RENEW(nodebounold,int,nboun);
      RENEW(ndirbounold,int,nboun);
    }
    FORTRAN(spcmatch,(xboun,nodeboun,ndirboun,&nboun,xbounold,nodebounold,
	      ndirbounold,&nbounold,ikboun,ilboun,vold,reorder,nreorder));
    RENEW(xbounold,double,nboun);
    RENEW(nodebounold,int,nboun);
    RENEW(ndirbounold,int,nboun);
    free(reorder); free(nreorder);

    /* for additional forces or loads in the present step, the
       corresponding slots in the force and load fields of the
       previous steps are initialized */

    RENEW(xforcold,double,nforc);
    for(i=nforcold;i<nforc;i++) xforcold[i]=0;

    RENEW(xloadold,double,2*nload);
    for(i=2*nloadold;i<2*nload;i++) xloadold[i]=0;

    if(ithermal>1){
	RENEW(xflowold,double,nflow);
	for(i=nflowold;i<nflow;i++) xflowold[i]=0;}

    if(ithermal==1){
      RENEW(t1old,double,nk);
    }
    
  }

  /* reallocating fields for all steps (>=1) */

  RENEW(co,double,3*nk);

  RENEW(nodeboun,int,nboun);
  RENEW(ndirboun,int,nboun);
  RENEW(xboun,double,nboun);
  RENEW(ikboun,int,nboun);
  RENEW(ilboun,int,nboun);
    
  RENEW(nodeforc,int,nforc);
  RENEW(ndirforc,int,nforc);
  RENEW(xforc,double,nforc);
  RENEW(ikforc,int,nforc);
  RENEW(ilforc,int,nforc);

  RENEW(nelemload,int,2*nload);
  sideloadLen = 5*nload_; // added by yusuf
  RENEW(sideload,char,5*nload);
  RENEW(xload,double,2*nload);

  RENEW(nodeprint,int,noprint);
  RENEW(nelemprint,int,neprint);

  RENEW(ipompc,int,nmpc);

  /* initial velocities and accelerations */

  if((nmethod == 4) || ((nmethod == 1) && (iperturb >= 2))) {
    RENEW(veold,double,4*nk);
  }
  else {free(veold);}

  if(nmethod == 4) {
    accold=NNEW(double,4*nk);
  }

  if(nam > 0) {
    RENEW(iamforc,int,nforc);
    RENEW(iamload,int,2*nload);
    RENEW(iamboun,int,nboun);
  }

  /* temperature loading */
  
  if(ithermal == 1){
      if((ne1d==0)&&(ne2d==0)){
	  RENEW(t0,double,nk);
	  RENEW(t1,double,nk);
      }
    if(nam > 0) {RENEW(iamt1,int,nk);}
  }

  if(ntrans > 0){
    RENEW(inotr,int,2*nk);
  }

  /* decascading MPC's and renumbering the equations: only necessary
  if MPC's changed */

  if((istep == 1)||(ntrans>0)) {

    /* decascading the MPC's */

    printf("Decascading the MPC's\n\n");

    callfrommain=1;
    cascade(ipompc,&coefmpc,&nodempc,&nmpc,
	    &mpcfree,nodeboun,ndirboun,&nboun,ikmpc,
	    ilmpc,ikboun,ilboun,&mpcend,&mpcmult,
	    labmpc,labmpcLen, &nk,&memmpc_,&icascade,&maxlenmpc,&callfrommain);

    /* reallocating nodempc and coefmpc */
 
    /* RENEW(nodempc,int,3*mpcend);
       RENEW(coefmpc,double,mpcend);*/
    labmpcLen = 20*nmpc; // added by yusuf
    RENEW(labmpc,char,20*nmpc);
    RENEW(ikmpc,int,nmpc);
    RENEW(ilmpc,int,nmpc);

    if(istep==1) nnn=NNEW(int,nk);
    else RENEW(nnn,int,nk);
    for(i=1;i<=nk;++i)
	nnn[i-1]=i;
	
    if(icascade==0){

	/* renumbering the nodes */
	
	printf("Renumbering the nodes to decrease the profile:\n");
	
	npn=NNEW(int,20*ne+mpcend);
	adj=NNEW(int,380*ne+mpcmult);
	xadj=NNEW(int,nk+1);
	iw=NNEW(int,3*nk+1);
	mmm=NNEW(int,nk);
	xnpn=NNEW(int,ne+nmpc+1);
	
#if defined(SPEC_CPU_NAGF95)
	FORTRAN(renumber,(&nk,kon,ipkon,lakon,&ne,ipompc,nodempc,&nmpc,nnn,
			  npn,adj,xadj,iw,mmm,xnpn, lakonLen));
#else
	FORTRAN(renumber,(&nk,kon,ipkon,lakon,&ne,ipompc,nodempc,&nmpc,nnn,
			  npn,adj,xadj,iw,mmm,xnpn));
#endif
	
	free(npn);free(adj);free(xadj);free(iw);free(mmm);free(xnpn);
    }
  }

  /* determining the matrix structure: changes if SPC's have changed */
  
  if(icascade==0) printf("Determining the structure of the matrix:\n");
  
  nactdof=NNEW(int,4*nk);
  
  if(isolver!=1) {
      mast1=NNEW(int,nzs);
      irow=NNEW(int,nzs);
  }
  
  if(ns[1]==-1){
      
      icol=NNEW(int,4*nk);
      jq=NNEW(int,4*nk+1);
      ikcol=NNEW(int,4*nk);
      ipointer=NNEW(int,4*nk);
      
      if(icascade==0){
	  mastruct(&nk,kon,ipkon,lakon,&ne,nodeboun,ndirboun,&nboun,ipompc,
		   nodempc,&nmpc,nactdof,icol,jq,&mast1,&irow,&isolver,&neq,nnn,
		   ikmpc,ilmpc,ikcol,ipointer,&nsky,&nzs,&nmethod,&ithermal);
      }
      else{neq=1;}
  }
  else{
      
      icol=NNEW(int,8*nk);
      jq=NNEW(int,8*nk+1);
      ikcol=NNEW(int,8*nk);
      ipointer=NNEW(int,8*nk);
      
      mastructcs(&nk,kon,ipkon,lakon,lakonLen, &ne,nodeboun,ndirboun,&nboun,
		 ipompc,nodempc,&nmpc,nactdof,icol,jq,&mast1,&irow,&isolver,
		 &neq,nnn,ikmpc,ilmpc,ikcol,ipointer,&nsky,&nzs,&nmethod,
		 ics,ns,labmpc, labmpcLen);}
  
  free(ikcol);free(ipointer);
  
  if(isolver!=1) {
      free(mast1);
      RENEW(irow,int,nzs);
  }

  /* nmethod=1: static analysis   */
  /* nmethod=2: frequency analysis  */
  /* nmethod=3: buckling analysis */
  /* nmethod=4: linear dynamic analysis */

  if((nmethod<=1)||(iperturb>1))
    {
      if(isolver==1){

	profile(co,&nk,kon,ipkon,lakon,lakonLen,&ne,nodeboun,ndirboun,xboun,&nboun, 
	     ipompc,nodempc,coefmpc,labmpc,labmpcLen,&nmpc,nodeforc,ndirforc,xforc,
             &nforc, 
	     nelemload,sideload,sideloadLen,xload,&nload,p1,p2,&om,bodyf, 
	     ad,au,b,nactdof,icol,jq,&neq,&nmethod,ikmpc, 
	     ilmpc,ikboun,ilboun,elcon,nelcon,rhcon,nrhcon,alcon,nalcon,
             alzero,ielmat,ielorien,&norien,orab,&ntmat_,
             t0,t1,t1old,&ithermal,prestr,&iprestr, 
	     vold,&iperturb,sti,nodeprint,&noprint,nelemprint,&neprint,
	     &kode,&nsky,noelplab,noelplabLen,nodeflab,nodeflabLen,eei,&iexpl,
	     plicon,nplicon,plkcon,
             nplkcon,xstate,&npmat_,matname,matnameLen,&mint_,&ncmat_,&nstate_,ns,csab,
	     &nkon,ener,xbounold,xforcold,xloadold,&omold,bodyfold,amname,amnameLen,
	     amta,namta,&nam,iamforc,iamload,&iamom,&iambodyf,iamt1,iamboun,
             &ttime,output,outputLen);

      }

      else if(iperturb<2){
	
	prespooles(co,&nk,kon,ipkon,lakon,lakonLen,&ne,nodeboun,ndirboun,xboun,&nboun, 
	     ipompc,nodempc,coefmpc,labmpc,labmpcLen,&nmpc,nodeforc,ndirforc,xforc,
             &nforc, 
	     nelemload,sideload,sideloadLen,xload,&nload,p1,p2,&om,bodyf, 
	     ad,au,b,nactdof,&icol,jq,&irow,&neq,&nzl,&nmethod,ikmpc, 
	     ilmpc,ikboun,ilboun,elcon,nelcon,rhcon,nrhcon,
	     alcon,nalcon,alzero,ielmat,ielorien,&norien,orab,&ntmat_,
             t0,t1,t1old,&ithermal,prestr,&iprestr, 
	     vold,&iperturb,sti,&nzs,nodeprint,&noprint,nelemprint,&neprint,
	     &kode,adb,aub,noelplab,noelplabLen,nodeflab,nodeflabLen,eei,&iexpl,plicon,
             nplicon,plkcon,nplkcon,xstate,&npmat_,matname,matnameLen,
	     &isolver,&mint_,&ncmat_,&nstate_,ns,csab,&nkon,ener,
             xbounold,xforcold,xloadold,&omold,bodyfold,amname,amnameLen,amta,namta,
             &nam,iamforc,iamload,&iamom,&iambodyf,iamt1,iamboun,&ttime,
             output,outputLen);

      }

      else{

	mpcinfo[0]=memmpc_;mpcinfo[1]=mpcfree;mpcinfo[2]=icascade;
	mpcinfo[3]=maxlenmpc;

	nonlingeo(co,&nk,kon,ipkon,lakon,lakonLen,&ne,nodeboun,ndirboun,xboun,&nboun, 
	     ipompc,&nodempc,&coefmpc,labmpc,labmpcLen,&nmpc,nodeforc,ndirforc,xforc,
             &nforc, 
	     nelemload,sideload,sideloadLen,xload,&nload,p1,p2,&om,bodyf, 
	     ad,au,b,nactdof,&icol,jq,&irow,&neq,&nzl,&nmethod,ikmpc, 
	     ilmpc,ikboun,ilboun,elcon,nelcon,rhcon,nrhcon,
	     alcon,nalcon,alzero,ielmat,ielorien,&norien,orab,&ntmat_,
             t0,t1,t1old,&ithermal,prestr,&iprestr, 
	     vold,&iperturb,sti,&nzs,nodeprint,&noprint,nelemprint,&neprint,
	     &kode,adb,aub,noelplab,noelplabLen,nodeflab,nodeflabLen,&idrct,&jmax,
	     &jout,&tinc,&tper,&tmin,&tmax,eei,xbounold,xforcold,xloadold,
	     &omold,bodyfold,veold,accold,amname,amnameLen,amta,namta,
	     &nam,iamforc,iamload,&iamom,&iambodyf,iamt1,&alpha,&haftol,
             &iexpl,iamboun,plicon,nplicon,plkcon,nplkcon,
	     xstate,&npmat_,&istep,&ttime,matname,matnameLen,&qaold,&mint_,
	     &isolver,&ncmat_,&nstate_,&iumat,ns,csab,&nkon,ener,
	     mpcinfo,nnn,output,outputLen,nodeflow,iamflow,
             xflow,shcon,nshcon,cocon,ncocon,physcon,xflowold,&nflow,ctrl);

	memmpc_=mpcinfo[0];mpcfree=mpcinfo[1];icascade=mpcinfo[2];
        maxlenmpc=mpcinfo[3];


      }
    }
  else if(nmethod==2)
    {  
    }
  else if(nmethod==3)
    {
    }
  else if(nmethod==4)
    {
    }

  free(nactdof);
  free(icol);
  free(jq);
  if(isolver!=1){free(irow);}

  /* deleting the perturbation loads and temperatures */

  if((iperturb == 1)&&(nmethod==3)) {
    nforc=0;
    nload=0;
    om=0.;
    for(k=0;k<3;++k){
      bodyf[k]=0.;
    }
    if(ithermal == 1) {
      for(k=0;k<nk;++k){
	t1[k]=t0[k];
      }
    }
  }

  else{
    nbounold=nboun;
    nforcold=nforc;
    nloadold=nload;
    if(ithermal>1)nflowold=nflow;

    /* resetting the amplitude to none except for time=total time amplitudes */

    if(nam > 0) {
	for (i=0;i<nboun;i++) {
	    if(namta[3*iamboun[i]-1]!=1) iamboun[i]=0;
	}
	for (i=0;i<nforc;i++){
	    if(namta[3*iamforc[i]-1]!=1) iamforc[i]=0;
	}
	for (i=0;i<2*nload;i++){
	    if(namta[3*iamload[i]-1]!=1) iamload[i]=0;
	}
	if(namta[3*iamom-1]!=1) iamom=0;
	if(namta[3*iambodyf-1]!=1) iambodyf=0;
	if(ithermal==1) {
	    if(namta[3*iamt1[i]-1]!=1) iamt1[i]=0;
	}
    }
  }

}

 FORTRAN(frdclose,());

 return 0;
      
}




