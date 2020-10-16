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

#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))

void mastruct(int *nk, int *kon, int *ipkon, char *lakon, int *ne,
	      int *nodeboun, int *ndirboun, int *nboun, int *ipompc,
	      int *nodempc, int *nmpc, int *nactdof, int *icol,
	      int *jq, int **mast1p, int **irowp, int *isolver, int *neq,
	      int *nnn, int *ikmpc, int *ilmpc, int *ikcol,
	      int *ipointer, int *nsky, int *nzs, int *nmethod,
              int *ithermal){

  int i,j,k,l,jj,ll,id,index,jdof1,jdof2,idof1,idof2,mpc1,mpc2,id1,id2,
    ist1,ist2,node1,node2,isubtract,nmast,ifree,istart,istartold,itot,
    index1,index2,m,node,nzs_,ist,kflag,indexe,nope,isize,*mast1=NULL,
    *irow=NULL,nsky_exp,nsky_inc;

  /* the indices in the comments follow FORTRAN convention, i.e. the
     fields start with 1 */

  mast1=*mast1p;
  irow=*irowp;

  kflag=2;
  nzs_=*nzs;

  /* initialisation of nactmpc */

  for(i=0;i<4**nk;++i){nactdof[i]=0;}

  /* determining the mechanical active degrees of freedom due to elements */

  if((*ithermal<2)||(*ithermal==3)){
      for(i=0;i<*ne;++i){
	  
	  if(ipkon[i]<0) continue;
	  indexe=ipkon[i];
	  if(strcmp1(&lakon[8*i+3],"2")==0)nope=20;
	  else if (strcmp1(&lakon[8*i+3],"8")==0)nope=8;
	  else if (strcmp1(&lakon[8*i+3],"10")==0)nope=10;
	  else if (strcmp1(&lakon[8*i+3],"4")==0)nope=4;
	  else if (strcmp1(&lakon[8*i+3],"15")==0)nope=15;
	  else {nope=6;}
	  
	  for(j=0;j<nope;++j){
	      node=kon[indexe+j]-1;
	      for(k=1;k<4;++k){
		  nactdof[4*node+k]=1;
	      }
	  }
      }
  }

  /* determining the thermal active degrees of freedom due to elements */

  if(*ithermal>1){
      for(i=0;i<*ne;++i){
	  
	  if(ipkon[i]<0) continue;
	  indexe=ipkon[i];
	  if(strcmp1(&lakon[8*i+3],"2")==0)nope=20;
	  else if (strcmp1(&lakon[8*i+3],"8")==0)nope=8;
	  else if (strcmp1(&lakon[8*i+3],"10")==0)nope=10;
	  else if (strcmp1(&lakon[8*i+3],"4")==0)nope=4;
	  else if (strcmp1(&lakon[8*i+3],"15")==0)nope=15;
	  else {nope=6;}
	  
	  for(j=0;j<nope;++j){
	      node=kon[indexe+j]-1;
	      nactdof[4*node]=1;
	  }
      }
  }

  /* determining the active degrees of freedom due to mpc's */

  for(i=0;i<*nmpc;++i){
      index=ipompc[i]-1;
      do{
	  nactdof[4*nodempc[3*index]+nodempc[3*index+1]-4]=1;
	  index=nodempc[3*index+2];
	  if(index==0) break;
	  index--;
      }while(1);
  }
	   
  /* subtracting the SPC and MPC nodes */

  for(i=0;i<*nboun;++i){
      if(ndirboun[i]>3){continue;}
      nactdof[4*(nodeboun[i]-1)+ndirboun[i]]=0;
  }

  for(i=0;i<*nmpc;++i){
    index=ipompc[i]-1;
    nactdof[4*nodempc[3*index]+nodempc[3*index+1]-4]=0;
  }
  
  /* numbering the active degrees of freedom */
  
  *neq=0;
  if((*ithermal<2)||(*ithermal==3)){
      for(i=0;i<*nk;++i){
	  for(j=1;j<4;++j){
	      if(nactdof[4*nnn[i]+j-4]!=0){
		  ++*neq;
		  nactdof[4*nnn[i]+j-4]=*neq;
	      }
	  }
      }
  }
  if(*ithermal>1){
      for(i=0;i<*nk;++i){
	  if(nactdof[4*nnn[i]-4]!=0){
	      ++*neq;
	      nactdof[4*nnn[i]-4]=*neq;
	  }
      }
  }
  
  ifree=0;
  
  /* determining the position of the skyline only */
  
  if(*isolver==1){
    
    /* ipointer(i) = first nonzero row number in column i */
    
    for(i=0;i<4**nk;++i){ipointer[i]=i+1;}
    
    for(i=0;i<*ne;++i){
      
      if(ipkon[i]<0) continue;
      indexe=ipkon[i];
      if(strcmp1(&lakon[8*i+3],"2")==0)nope=20;
      else if (strcmp1(&lakon[8*i+3],"8")==0)nope=8;
      else if (strcmp1(&lakon[8*i+3],"10")==0)nope=10;
      else if (strcmp1(&lakon[8*i+3],"4")==0)nope=4;
      else if (strcmp1(&lakon[8*i+3],"15")==0)nope=15;
      else {nope=6;}
      
      for(jj=0;jj<3*nope;++jj){
	
	j=jj/3;
	k=jj-3*j;
	
	node1=kon[indexe+j];
	jdof1=nactdof[4*(node1-1)+k+1];
	
	for(ll=jj;ll<3*nope;++ll){
	  
	  l=ll/3;
	  m=ll-3*l;
	  
	  node2=kon[indexe+l];
	  jdof2=nactdof[4*(node2-1)+m+1];
	  
	  /* check whether one of the DOF belongs to a SPC or MPC */
	  
	  if((jdof1!=0)&&(jdof2!=0)){
	    ipointer[max(jdof1,jdof2)-1]=
	      min(ipointer[max(jdof1,jdof2)-1],min(jdof1,jdof2));
	  }
	  else if((jdof1!=0)||(jdof2!=0)){
	    
	    /* idof1: genuine DOF
	       idof2: nominal DOF of the SPC/MPC */
	    
	    if(jdof1==0){
	      idof1=jdof2;
	      idof2=7*node1+k-6;}
	    else{
	      idof1=jdof1;
	      idof2=7*node2+m-6;}
	    
	    if(*nmpc>0){
	      
	      FORTRAN(nident,(ikmpc,&idof2,nmpc,&id));
	      if((id>0)&&(ikmpc[id-1]==idof2)){
		
		/* regular DOF / MPC */
		
		id=ilmpc[id-1];
		ist=ipompc[id-1];
		index=nodempc[3*ist-1];
		if(index==0) continue;
		while(1){
		  idof2=nactdof[4*nodempc[3*index-3]+nodempc[3*index-2]-4];
		  if(idof2!=0){
		    ipointer[max(idof1,idof2)-1]=
		      min(ipointer[max(idof1,idof2)-1],min(idof1,idof2));
		  }
		  index=nodempc[3*index-1];
		  if(index==0) break;
		}
		continue;
	      }
	    }
	  }
	  
	  else{
	    idof1=7*node1+k-6;
	    idof2=7*node2+m-6;
	    mpc1=0;
	    mpc2=0;
	    if(*nmpc>0){
	      FORTRAN(nident,(ikmpc,&idof1,nmpc,&id1));
	      if((id1>0)&&(ikmpc[id1-1]==idof1)) mpc1=1;
	      FORTRAN(nident,(ikmpc,&idof2,nmpc,&id2));
	      if((id2>0)&&(ikmpc[id2-1]==idof2)) mpc2=1;
	    }
	    if((mpc1==1)&&(mpc2==1)){
	      id1=ilmpc[id1-1];
	      id2=ilmpc[id2-1];
	      if(id1==id2){
		
		/* MPC id1 / MPC id1 */
		
		ist=ipompc[id1-1];
		index1=nodempc[3*ist-1];
		if(index1==0) continue;
		while(1){
		  idof1=nactdof[4*nodempc[3*index1-3]+nodempc[3*index1-2]-4];
		  index2=index1;
		  while(1){
		    idof2=nactdof[4*nodempc[3*index2-3]+nodempc[3*index2-2]-4];
		    if((idof1!=0)&&(idof2!=0)){
		      ipointer[max(idof1,idof2)-1]=
			min(ipointer[max(idof1,idof2)-1],min(idof1,idof2));}
		    index2=nodempc[3*index2-1];
		    if(index2==0) break;
		  }
		  index1=nodempc[3*index1-1];
		  if(index1==0) break;
		}
	      }
	      
	      else{
		
		/* MPC id1 /MPC id2 */
		
		ist1=ipompc[id1-1];
		index1=nodempc[3*ist1-1];
		if(index1==0) continue;
		while(1){
		  idof1=nactdof[4*nodempc[3*index1-3]+nodempc[3*index1-2]-4];
		  ist2=ipompc[id2-1];
		  index2=nodempc[3*ist2-1];
		  if(index2==0){
		    index1=nodempc[3*index1-1];
		    if(index1==0){break;}
		    else{continue;}
		  }
		  while(1){
		    idof2=nactdof[4*nodempc[3*index2-3]+nodempc[3*index2-2]-4];
		    if((idof1!=0)&&(idof2!=0)){
		      ipointer[max(idof1,idof2)-1]=
			min(ipointer[max(idof1,idof2)-1],min(idof1,idof2));}
		    index2=nodempc[3*index2-1];
		    if(index2==0) break;
		  }
		  index1=nodempc[3*index1-1];
		  if(index1==0) break;
		}
	      }
	    }
	  }
	}
      }
    }
    
    /* defining icol and jq
       
       jq(i) = first nonzero row element in column i
       icol(i) = length of skyline up to and including column i */
    
    icol[0]=0;
    for(i=1;i<*neq;++i){
      jq[i]=ipointer[i];
      icol[i]=icol[i-1]+i-jq[i]+1;
    }
    *nsky=icol[*neq-1];

    if(*neq==0){
      printf("*ERROR: no degrees of freedom in the model\n");
      FORTRAN(stop,());
    }

    printf("number of equations\n");
    printf("%d\n",*neq);
    printf("total length of the skyline\n");
    printf("%d\n\n",icol[*neq-1]);
    
  }
  else{

    /* determining the position of each nonzero matrix element

       mast1(ipointer(i)) = first nonzero row in column i
       irow(ipointer(i))  points to further nonzero elements in 
                             column i */
      
    for(i=0;i<4**nk;++i){ipointer[i]=0;}

    /* mechanical entries */
    
    if((*ithermal<2)||(*ithermal==3)){

    for(i=0;i<*ne;++i){
      
      if(ipkon[i]<0) continue;
      indexe=ipkon[i];
      if(strcmp1(&lakon[8*i+3],"2")==0)nope=20;
      else if (strcmp1(&lakon[8*i+3],"8")==0)nope=8;
      else if (strcmp1(&lakon[8*i+3],"10")==0)nope=10;
      else if (strcmp1(&lakon[8*i+3],"4")==0)nope=4;
      else if (strcmp1(&lakon[8*i+3],"15")==0)nope=15;
      else {nope=6;}
      
      for(jj=0;jj<3*nope;++jj){
	
	j=jj/3;
	k=jj-3*j;
	
	node1=kon[indexe+j];
	jdof1=nactdof[4*(node1-1)+k+1];
	
	for(ll=jj;ll<3*nope;++ll){
	  
	  l=ll/3;
	  m=ll-3*l;
	  
	  node2=kon[indexe+l];
	  jdof2=nactdof[4*(node2-1)+m+1];
	  
	  /* check whether one of the DOF belongs to a SPC or MPC */
	  
	  if((jdof1!=0)&&(jdof2!=0)){
	    insert(ipointer,&mast1,&irow,&jdof1,&jdof2,&ifree,&nzs_);
	  }
	  else if((jdof1!=0)||(jdof2!=0)){
	    
	    /* idof1: genuine DOF
	       idof2: nominal DOF of the SPC/MPC */
	    
	    if(jdof1==0){
	      idof1=jdof2;
	      idof2=7*node1+k-6;}
	    else{
	      idof1=jdof1;
	      idof2=7*node2+m-6;}
	    
	    if(*nmpc>0){
	      
	      FORTRAN(nident,(ikmpc,&idof2,nmpc,&id));
	      if((id>0)&&(ikmpc[id-1]==idof2)){
		
		/* regular DOF / MPC */
		
		id=ilmpc[id-1];
		ist=ipompc[id-1];
		index=nodempc[3*ist-1];
		if(index==0) continue;
		while(1){
		  idof2=nactdof[4*nodempc[3*index-3]+nodempc[3*index-2]-4];
		  if(idof2!=0){
		    insert(ipointer,&mast1,&irow,&idof1,&idof2,&ifree,&nzs_);
		  }
		  index=nodempc[3*index-1];
		  if(index==0) break;
		}
		continue;
	      }
	    }
	  }
	  
	  else{
	    idof1=7*node1+k-6;
	    idof2=7*node2+m-6;
	    mpc1=0;
	    mpc2=0;
	    if(*nmpc>0){
	      FORTRAN(nident,(ikmpc,&idof1,nmpc,&id1));
	      if((id1>0)&&(ikmpc[id1-1]==idof1)) mpc1=1;
	      FORTRAN(nident,(ikmpc,&idof2,nmpc,&id2));
	      if((id2>0)&&(ikmpc[id2-1]==idof2)) mpc2=1;
	    }
	    if((mpc1==1)&&(mpc2==1)){
	      id1=ilmpc[id1-1];
	      id2=ilmpc[id2-1];
	      if(id1==id2){
		
		/* MPC id1 / MPC id1 */
		
		ist=ipompc[id1-1];
		index1=nodempc[3*ist-1];
		if(index1==0) continue;
		while(1){
		  idof1=nactdof[4*nodempc[3*index1-3]+nodempc[3*index1-2]-4];
		  index2=index1;
		  while(1){
		    idof2=nactdof[4*nodempc[3*index2-3]+nodempc[3*index2-2]-4];
		    if((idof1!=0)&&(idof2!=0)){
		      insert(ipointer,&mast1,&irow,&idof1,&idof2,&ifree,&nzs_);}
		    index2=nodempc[3*index2-1];
		    if(index2==0) break;
		  }
		  index1=nodempc[3*index1-1];
		  if(index1==0) break;
		}
	      }
	      
	      else{
		
		/* MPC id1 /MPC id2 */
		
		ist1=ipompc[id1-1];
		index1=nodempc[3*ist1-1];
		if(index1==0) continue;
		while(1){
		  idof1=nactdof[4*nodempc[3*index1-3]+nodempc[3*index1-2]-4];
		  ist2=ipompc[id2-1];
		  index2=nodempc[3*ist2-1];
		  if(index2==0){
		    index1=nodempc[3*index1-1];
		    if(index1==0){break;}
		    else{continue;}
		  }
		  while(1){
		    idof2=nactdof[4*nodempc[3*index2-3]+nodempc[3*index2-2]-4];
		    if((idof1!=0)&&(idof2!=0)){
		      insert(ipointer,&mast1,&irow,&idof1,&idof2,&ifree,&nzs_);}
		    index2=nodempc[3*index2-1];
		    if(index2==0) break;
		  }
		  index1=nodempc[3*index1-1];
		  if(index1==0) break;
		}
	      }
	    }
	  }
	}
      }
    }

    }

    /* thermal entries*/

    if(*ithermal>1){

    for(i=0;i<*ne;++i){
      
      if(ipkon[i]<0) continue;
      indexe=ipkon[i];
      if(strcmp1(&lakon[8*i+3],"2")==0)nope=20;
      else if (strcmp1(&lakon[8*i+3],"8")==0)nope=8;
      else if (strcmp1(&lakon[8*i+3],"10")==0)nope=10;
      else if (strcmp1(&lakon[8*i+3],"4")==0)nope=4;
      else if (strcmp1(&lakon[8*i+3],"15")==0)nope=15;
      else {nope=6;}
      
      for(jj=0;jj<nope;++jj){
	
	j=jj;
	
	node1=kon[indexe+j];
	jdof1=nactdof[4*(node1-1)];
	
	for(ll=jj;ll<nope;++ll){
	  
	  l=ll;
	  
	  node2=kon[indexe+l];
	  jdof2=nactdof[4*(node2-1)];
	  
	  /* check whether one of the DOF belongs to a SPC or MPC */
	  
	  if((jdof1!=0)&&(jdof2!=0)){
	    insert(ipointer,&mast1,&irow,&jdof1,&jdof2,&ifree,&nzs_);
	  }
	  else if((jdof1!=0)||(jdof2!=0)){
	    
	    /* idof1: genuine DOF
	       idof2: nominal DOF of the SPC/MPC */
	    
	    if(jdof1==0){
	      idof1=jdof2;
	      idof2=7*node1-7;}
	    else{
	      idof1=jdof1;
	      idof2=7*node2-7;}
	    
	    if(*nmpc>0){
	      
	      FORTRAN(nident,(ikmpc,&idof2,nmpc,&id));
	      if((id>0)&&(ikmpc[id-1]==idof2)){
		
		/* regular DOF / MPC */
		
		id=ilmpc[id-1];
		ist=ipompc[id-1];
		index=nodempc[3*ist-1];
		if(index==0) continue;
		while(1){
		  idof2=nactdof[4*nodempc[3*index-3]+nodempc[3*index-2]-4];
		  if(idof2!=0){
		    insert(ipointer,&mast1,&irow,&idof1,&idof2,&ifree,&nzs_);
		  }
		  index=nodempc[3*index-1];
		  if(index==0) break;
		}
		continue;
	      }
	    }
	  }
	  
	  else{
	    idof1=7*node1-7;
	    idof2=7*node2-7;
	    mpc1=0;
	    mpc2=0;
	    if(*nmpc>0){
	      FORTRAN(nident,(ikmpc,&idof1,nmpc,&id1));
	      if((id1>0)&&(ikmpc[id1-1]==idof1)) mpc1=1;
	      FORTRAN(nident,(ikmpc,&idof2,nmpc,&id2));
	      if((id2>0)&&(ikmpc[id2-1]==idof2)) mpc2=1;
	    }
	    if((mpc1==1)&&(mpc2==1)){
	      id1=ilmpc[id1-1];
	      id2=ilmpc[id2-1];
	      if(id1==id2){
		
		/* MPC id1 / MPC id1 */
		
		ist=ipompc[id1-1];
		index1=nodempc[3*ist-1];
		if(index1==0) continue;
		while(1){
		  idof1=nactdof[4*nodempc[3*index1-3]+nodempc[3*index1-2]-4];
		  index2=index1;
		  while(1){
		    idof2=nactdof[4*nodempc[3*index2-3]+nodempc[3*index2-2]-4];
		    if((idof1!=0)&&(idof2!=0)){
		      insert(ipointer,&mast1,&irow,&idof1,&idof2,&ifree,&nzs_);}
		    index2=nodempc[3*index2-1];
		    if(index2==0) break;
		  }
		  index1=nodempc[3*index1-1];
		  if(index1==0) break;
		}
	      }
	      
	      else{
		
		/* MPC id1 /MPC id2 */
		
		ist1=ipompc[id1-1];
		index1=nodempc[3*ist1-1];
		if(index1==0) continue;
		while(1){
		  idof1=nactdof[4*nodempc[3*index1-3]+nodempc[3*index1-2]-4];
		  ist2=ipompc[id2-1];
		  index2=nodempc[3*ist2-1];
		  if(index2==0){
		    index1=nodempc[3*index1-1];
		    if(index1==0){break;}
		    else{continue;}
		  }
		  while(1){
		    idof2=nactdof[4*nodempc[3*index2-3]+nodempc[3*index2-2]-4];
		    if((idof1!=0)&&(idof2!=0)){
		      insert(ipointer,&mast1,&irow,&idof1,&idof2,&ifree,&nzs_);}
		    index2=nodempc[3*index2-1];
		    if(index2==0) break;
		  }
		  index1=nodempc[3*index1-1];
		  if(index1==0) break;
		}
	      }
	    }
	  }
	}
      }
    }

    }
    
    /* ordering the nonzero nodes in the SUPERdiagonal columns
       mast1 contains the row numbers column per column,
       irow the column numbers */

    for(i=0;i<*neq;++i){
      itot=0;
      if(ipointer[i]==0){
	printf("error in mastruct: zero column\n");
	FORTRAN(stop,());
      }
      istart=ipointer[i];
      while(1){
	++itot;
	ikcol[itot-1]=mast1[istart-1];
	istart=irow[istart-1];
	if(istart==0) break;
      }
      FORTRAN(isortii,(ikcol,icol,&itot,&kflag));
      istart=ipointer[i];
      for(j=0;j<itot-1;++j){
	mast1[istart-1]=ikcol[j];
	istartold=istart;
	istart=irow[istart-1];
	irow[istartold-1]=i+1;
      }
      mast1[istart-1]=ikcol[itot-1];
      irow[istart-1]=i+1;
    }

    /* definiing icol and jq */

    *nsky=0;
    nsky_exp=0;
    for(i=1;i<*neq;++i){
      nsky_inc=i+1-mast1[ipointer[i]-1];
      if(2147483647-*nsky<nsky_inc){
	++nsky_exp;
	nsky_inc=nsky_inc-2147483647;
      }
      *nsky=*nsky+nsky_inc;
    }

    if(*neq==0){
      printf("*ERROR: no degrees of freedom in the model\n");
      FORTRAN(stop,());
    }

    printf("number of equations\n");
    printf("%d\n",*neq);
    printf("number of nonzero matrix elements\n");
    printf("%d\n",ifree);
    printf("total length of the skyline\n");
    printf("%d*2147483647+%d\n",nsky_exp,*nsky);
    printf("percentage of nonzero skyline elements\n");
    printf("%f\n\n",((double)ifree)/
	   ((double)(*nsky+*neq)+nsky_exp*(double)(2147483647))*100.);

    /* new meaning of icol,j1,mast1,irow:

       - irow is going to contain the row numbers of the SUBdiagonal
         nonzero's, column per column
       - mast1 contains the column numbers
       - icol(i)=# SUBdiagonal nonzero's in column i
       - jq(i)= location in field irow of the first SUBdiagonal
         nonzero in column i

	 */

    nmast=ifree;

    /* switching from a SUPERdiagonal inventory to a SUBdiagonal one */

    FORTRAN(isortii,(mast1,irow,&nmast,&kflag));
    
    /* filtering out the diagonal elements and generating icol and jq */

    isubtract=0;
    for(i=1;i<*neq;++i){icol[i]=0;}
    k=0;
    for(i=0;i<nmast;++i){
      if(mast1[i]==irow[i]){++isubtract;}
      else{
	mast1[i-isubtract]=mast1[i];
	irow[i-isubtract]=irow[i];
	if(k!=mast1[i]){
	  for(l=k;l<mast1[i];++l){jq[l]=i+1-isubtract;}
	  k=mast1[i];
	}
	++icol[k-1];
      }
    }
    nmast=nmast-isubtract;
    for(l=k;l<*neq+1;++l){jq[l]=nmast+1;}

    for(i=0;i<*neq;++i){
      if(jq[i+1]-jq[i]>0){
	isize=jq[i+1]-jq[i];
	FORTRAN(isortii,(&irow[jq[i]-1],&mast1[jq[i]-1],&isize,&kflag));
      }
    }

    *nzs=jq[*neq-1]-1;

  }

  *mast1p=mast1;
  *irowp=irow;

  return;

}
	  
/*

What follows is the original FORTRAN code. The C Code is a one-to-one
manual translation of the FORTRAN code. However, the FORTRAN code might
be easier to understand.

!
!     CalculiX - A 3-dimensional finite element program
!              Copyright (C) 1998 Guido Dhondt
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation(version 2);
!     
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of 
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program; if not, write to the Free Software
!     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
      subroutine mastruct(nk,kon,ipkon,lakon,ne,nodeboun,ndirboun,
     &  nboun,ipompc,
     &  nodempc,nmpc,nactdof,icol,jq,mast1,irow,isolver,neq,nnn,
     &  ikmpc,ilmpc,ikcol,ipointer,nsky,nzs,nmethod)
!
      implicit none
!
      character*6 lakon(*)
!
      integer kon(*),nodeboun(*),ndirboun(*),nodempc(3,*),ipompc(*),
     &  nactdof(3,*),icol(*),jq(*),ipointer(*),nnn(*),ikmpc(*),ilmpc(*),
     &  ikcol(*),mast1(*),irow(*),ipkon(*)
!
      integer nk,ne,nboun,nmpc,isolver,neq,nsky,nzs,i,j,k,l,jj,ll,id,
     &  index,jdof1,jdof2,idof1,idof2,mpc1,mpc2,id1,id2,ist1,ist2,node1,
     &  node2,isubtract,nmast,ifree,istart,istartold,itot,index1,index2,
     &  m,node,nzs_,ist,kflag,nmethod,indexe,nope,nsky_exp,nsky_inc
!
      kflag=2
      nzs_=nzs
!
!     initialisation of nactmpc
!
      do i=1,nk
        do j=1,3
          nactdof(j,i)=0
        enddo
      enddo
!
!     determining the active degrees of freedom due to elements
!
      do i=1,ne
!
         if(ipkon(i).lt.0) cycle
         indexe=ipkon(i)
         if((lakon(i).eq.'C3D20R').or.(lakon(i).eq.'C3D20 ')) then
            nope=20
         elseif((lakon(i).eq.'C3D8R ').or.(lakon(i).eq.'C3D8  ')) then
            nope=8
         else
            nope=10
         endif
!
         do j=1,nope
            node=kon(indexe+j)
            do k=1,3
               nactdof(k,node)=1
            enddo
         enddo
      enddo
!
!     determining the active degrees of freedom due to mpc's
!
      do i=1,nmpc
         index=ipompc(i)
         do
            nactdof(nodempc(2,index),nodempc(1,index))=1
            index=nodempc(3,index)
	    if(index.eq.0) exit
	 enddo
      enddo
!      
!     subtracting the SPC and MPC nodes
!
      do i=1,nboun
        nactdof(ndirboun(i),nodeboun(i))=0
      enddo
!
      do i=1,nmpc
         index=ipompc(i)
         nactdof(nodempc(2,index),nodempc(1,index))=0
      enddo
!
!     numbering the active degrees of freedom
!
      neq=0
      do i=1,nk
        do j=1,3
          if(nactdof(j,nnn(i)).ne.0) then
            neq=neq+1
            nactdof(j,nnn(i))=neq
          endif
        enddo
      enddo
!
      ifree=0
!
!     determining the position of the skyline only
!
      if(isolver.eq.1) then
!
!       ipointer(i) = first nonzero row number in column i
!
        do i=1,3*nk
          ipointer(i)=i
        enddo
!
        do i=1,ne
!
          if(ipkon(i).lt.0) cycle
          indexe=ipkon(i)
          if((lakon(i).eq.'C3D20R').or.(lakon(i).eq.'C3D20 ')) then
             nope=20
          elseif((lakon(i).eq.'C3D8R ').or.(lakon(i).eq.'C3D8  ')) then
             nope=8
          else
             nope=10
          endif
!
          do jj=1,3*nope
!
            j=(jj-1)/3+1
            k=jj-3*(j-1)
!
            node1=kon(indexe+j)
            jdof1=nactdof(k,node1)
!
            do ll=jj,3*nope
!
              l=(ll-1)/3+1
              m=ll-3*(l-1)
!
              node2=kon(indexe+l)
              jdof2=nactdof(m,node2)
!
!             check whether one of the DOF belongs to a SPC or MPC
!
              if((jdof1.ne.0).and.(jdof2.ne.0)) then
                 ipointer(max(jdof1,jdof2))=
     &             min(ipointer(max(jdof1,jdof2)),min(jdof1,jdof2))
              elseif((jdof1.ne.0).or.(jdof2.ne.0)) then
!
!              idof1: genuine DOF
!              idof2: nominal DOF of the SPC/MPC
!
                 if(jdof1.eq.0) then
                    idof1=jdof2
                    idof2=(node1-1)*3+k
                 else
                    idof1=jdof1
                    idof2=(node2-1)*3+m
                 endif
                 if(nmpc.gt.0) then
                    call nident(ikmpc,idof2,nmpc,id)
                    if((id.gt.0).and.(ikmpc(id).eq.idof2)) then
!
!                    regular DOF / MPC
!
                       id=ilmpc(id)
                       ist=ipompc(id)
                       index=nodempc(3,ist)
		       if(index.eq.0) cycle
                       do
                          idof2=nactdof(nodempc(2,index),
     &                                  nodempc(1,index))
                          if(idof2.ne.0) then
                             ipointer(max(idof1,idof2))=
     &                         min(ipointer(max(idof1,idof2)),
     &                             min(idof1,idof2))
                          endif
                          index=nodempc(3,index)
                          if(index.eq.0) exit
                       enddo
                       cycle
                    endif
                 endif
!
              else
                 idof1=(node1-1)*3+k
                 idof2=(node2-1)*3+m
                 mpc1=0
                 mpc2=0
                 if(nmpc.gt.0) then
                    call nident(ikmpc,idof1,nmpc,id1)
                    if((id1.gt.0).and.(ikmpc(id1).eq.idof1)) mpc1=1
                    call nident(ikmpc,idof2,nmpc,id2)
                    if((id2.gt.0).and.(ikmpc(id2).eq.idof2)) mpc2=1
                 endif
                 if((mpc1.eq.1).and.(mpc2.eq.1)) then
                    id1=ilmpc(id1)
                    id2=ilmpc(id2)
                    if(id1.eq.id2) then
!
!                    MPC id1 / MPC id1
!
                       ist=ipompc(id1)
                       index1=nodempc(3,ist)
		       if(index1.eq.0) cycle
                       do
                          idof1=nactdof(nodempc(2,index1),
     &                                nodempc(1,index1))
                          index2=index1
                          do
                             idof2=nactdof(nodempc(2,index2),
     &                                   nodempc(1,index2))
                             if((idof1.ne.0).and.(idof2.ne.0))
     &                         ipointer(max(idof1,idof2))=
     &                         min(ipointer(max(idof1,idof2)),
     &                             min(idof1,idof2))
                             index2=nodempc(3,index2)
                             if(index2.eq.0) exit
                          enddo
                          index1=nodempc(3,index1)
                          if(index1.eq.0) exit
                       enddo
                     else
!
!                    MPC id1 / MPC id2
!
                       ist1=ipompc(id1)
                       index1=nodempc(3,ist1)
		       if(index1.eq.0) cycle
                       do
                          idof1=nactdof(nodempc(2,index1),
     &                                nodempc(1,index1))
                          ist2=ipompc(id2)
                          index2=nodempc(3,ist2)
                          if(index2.eq.0) then
                             index1=nodempc(3,index1)
                             if(index1.eq.0) then
                                exit
                             else
                                cycle
                             endif
                          endif
                          do
                             idof2=nactdof(nodempc(2,index2),
     &                                   nodempc(1,index2))
                             if((idof1.ne.0).and.(idof2.ne.0))
     &                         ipointer(max(idof1,idof2))=
     &                         min(ipointer(max(idof1,idof2)),
     &                             min(idof1,idof2))
                             index2=nodempc(3,index2)
                             if(index2.eq.0) exit
                          enddo
                          index1=nodempc(3,index1)
                          if(index1.eq.0) exit
                       enddo
                    endif
                 endif
              endif
            enddo
          enddo
        enddo
!
!       defining icol and jq
!
!       jq(i) = first nonzero row element in column i
!       icol(i) = length of skyline up to and including column i
!
        icol(1)=0
        do i=2,neq
          jq(i)=ipointer(i)
          icol(i)=icol(i-1)+i-jq(i)
        enddo
        nsky=icol(neq)
!
        write(*,*) 'number of equations'
        write(*,*) neq
        write(*,*) 'total length of the skyline'
        write(*,*) icol(neq)
        write(*,*)
!
      else
!
!     determining the position of each nonzero matrix element
!
!       mast1(ipointer(i)) = first nonzero row in column i
!       irow(ipointer(i)) points to further nonzero elements in
!         column i
!
        do i=1,3*nk
          ipointer(i)=0
        enddo
!
        do i=1,ne
!
          if(ipkon(i).lt.0) cycle
          indexe=ipkon(i)
          if((lakon(i).eq.'C3D20R').or.(lakon(i).eq.'C3D20 ')) then
             nope=20
          elseif((lakon(i).eq.'C3D8R ').or.(lakon(i).eq.'C3D8  ')) then
             nope=8
          else
             nope=10
          endif
!
          do jj=1,3*nope
!
            j=(jj-1)/3+1
            k=jj-3*(j-1)
!
            node1=kon(indexe+j)
            jdof1=nactdof(k,node1)
!
            do ll=jj,3*nope
!
              l=(ll-1)/3+1
              m=ll-3*(l-1)
!
              node2=kon(indexe+l)
              jdof2=nactdof(m,node2)
!
!             check whether one of the DOF belongs to a SPC or MPC
!
              if((jdof1.ne.0).and.(jdof2.ne.0)) then
                 call insert(ipointer,mast1,irow,
     &                       jdof1,jdof2,ifree,nzs_)
              elseif((jdof1.ne.0).or.(jdof2.ne.0)) then
!
!              idof1: genuine DOF
!              idof2: nominal DOF of the SPC/MPC
!
                 if(jdof1.eq.0) then
                    idof1=jdof2
                    idof2=(node1-1)*3+k
                 else
                    idof1=jdof1
                    idof2=(node2-1)*3+m
                 endif
                 if(nmpc.gt.0) then
                    call nident(ikmpc,idof2,nmpc,id)
                    if((id.gt.0).and.(ikmpc(id).eq.idof2)) then
!
!                    regular DOF / MPC
!
                       id=ilmpc(id)
                       ist=ipompc(id)
                       index=nodempc(3,ist)
		       if(index.eq.0) cycle
                       do
                          idof2=nactdof(nodempc(2,index),
     &                                  nodempc(1,index))
                          if(idof2.ne.0) then
                             call insert(ipointer,mast1,irow,
     &                                   idof1,idof2,ifree,nzs_)
                          endif
                          index=nodempc(3,index)
                          if(index.eq.0) exit
                       enddo
                       cycle
                    endif
                 endif
!
              else
                 idof1=(node1-1)*3+k
                 idof2=(node2-1)*3+m
                 mpc1=0
                 mpc2=0
                 if(nmpc.gt.0) then
                    call nident(ikmpc,idof1,nmpc,id1)
                    if((id1.gt.0).and.(ikmpc(id1).eq.idof1)) mpc1=1
                    call nident(ikmpc,idof2,nmpc,id2)
                    if((id2.gt.0).and.(ikmpc(id2).eq.idof2)) mpc2=1
                 endif
                 if((mpc1.eq.1).and.(mpc2.eq.1)) then
                    id1=ilmpc(id1)
                    id2=ilmpc(id2)
                    if(id1.eq.id2) then
!
!                    MPC id1 / MPC id1
!
                       ist=ipompc(id1)
                       index1=nodempc(3,ist)
		       if(index1.eq.0) cycle
                       do
                          idof1=nactdof(nodempc(2,index1),
     &                                nodempc(1,index1))
                          index2=index1
                          do
                             idof2=nactdof(nodempc(2,index2),
     &                                   nodempc(1,index2))
                             if((idof1.ne.0).and.(idof2.ne.0))
     &                         call insert(ipointer,mast1,irow,
     &                                   idof1,idof2,ifree,nzs_)
                             index2=nodempc(3,index2)
                             if(index2.eq.0) exit
                          enddo
                          index1=nodempc(3,index1)
                          if(index1.eq.0) exit
                       enddo
                     else
!
!                    MPC id1 / MPC id2
!
                       ist1=ipompc(id1)
                       index1=nodempc(3,ist1)
		       if(index1.eq.0) cycle
                       do
                          idof1=nactdof(nodempc(2,index1),
     &                                nodempc(1,index1))
                          ist2=ipompc(id2)
                          index2=nodempc(3,ist2)
                          if(index2.eq.0) then
                             index1=nodempc(3,index1)
                             if(index1.eq.0) then
                                exit
                             else
                                cycle
                             endif
                          endif
                          do
                             idof2=nactdof(nodempc(2,index2),
     &                                   nodempc(1,index2))
                             if((idof1.ne.0).and.(idof2.ne.0))
     &                          call insert(ipointer,mast1,irow,
     &                                   idof1,idof2,ifree,nzs_)
                             index2=nodempc(3,index2)
                             if(index2.eq.0) exit
                          enddo
                          index1=nodempc(3,index1)
                          if(index1.eq.0) exit
                       enddo
                    endif
                 endif
              endif
            enddo
          enddo
        enddo
!
!       ordering the nonzero nodes in the SUPERdiagonal columns
!       mast1 contains the row numbers column per column,
!       irow the column numbers
!
        do i=1,neq
          itot=0
          if(ipointer(i).eq.0) then
            write(*,*) 'error in mastruct: zero column'
            stop
          endif
          istart=ipointer(i)
          do
            itot=itot+1
            ikcol(itot)=mast1(istart)
            istart=irow(istart)
            if(istart.eq.0) exit
          enddo
          call isortii(ikcol,icol,itot,kflag)
          istart=ipointer(i)
          do j=1,itot-1
            mast1(istart)=ikcol(j)
            istartold=istart
            istart=irow(istart)
            irow(istartold)=i
          enddo
          mast1(istart)=ikcol(itot)
          irow(istart)=i
        enddo
!
!       defining icol and jq
!
        nsky=0
	nsky_exp=0
        do i=2,neq
          nsky_inc=i-mast1(ipointer(i))
          if(2147483647-nsky.lt.nsky_inc) then
	     nsky_exp=nsky_exp+1
	     nsky_inc=nsky_inc-2147483647
	  endif
	  nsky=nsky+nsky_inc
        enddo
!
        write(*,*) 'number of equations'
        write(*,*) neq
        write(*,*) 'number of nonzero matrix elements'
        write(*,*) ifree
        write(*,*) 'total length of the skyline'
        write(*,*) nsky_exp,'*2147483647+',nsky
        write(*,*) 'percentage of nonzero skyline elements'
        write(*,*) real(ifree)/
     &     (real(nsky+neq)+nsky_exp*real(2147483647))*100.
        write(*,*)
!
!       new meaning of icol,j1,mast1,irow:
!       - irow is going to contain the row numbers of the SUBdiagonal
!         nonzero's, column per column
!       - mast1 contains the column numbers
!       - icol(i)=# SUBdiagonal nonzero's in column i
!       - jq(i)= location in field irow of the first SUBdiagonal
!         nonzero in column i
!
        nmast=ifree
!
!       switching from a SUPERdiagonal inventary to a SUBdiagonal one
!
        call isortii(mast1,irow,nmast,kflag)
!
!       filtering out the diagonal elements and generating icol and jq
!
        isubtract=0
        do i=1,neq
           icol(i)=0
        enddo
        k=0
        do i=1,nmast
           if(mast1(i).eq.irow(i)) then
              isubtract=isubtract+1
           else
              mast1(i-isubtract)=mast1(i)
              irow(i-isubtract)=irow(i)
              if(k.ne.mast1(i)) then
                 do l=k+1,mast1(i)
                    jq(l)=i-isubtract
                 enddo
                 k=mast1(i)
              endif
              icol(k)=icol(k)+1
           endif
        enddo
        nmast=nmast-isubtract
        do l=k+1,neq+1
           jq(l)=nmast+1
        enddo
!
        do i=1,neq
           if(jq(i+1)-jq(i).gt.0) then
              call isortii(irow(jq(i)),mast1(jq(i)),jq(i+1)-jq(i),
     &          kflag)
           endif
        enddo
!
        nzs=jq(neq)-1
!
      endif
!
      return
      end

      */
