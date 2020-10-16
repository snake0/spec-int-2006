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

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "CalculiX.h"

#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))

void cascade(int *ipompc, double **coefmpcp, int **nodempcp, int *nmpc,
   int *mpcfree, int *nodeboun, int *ndirboun, int*nboun, int*ikmpc,
   int *ilmpc, int *ikboun, int *ilboun, int *mpcend, int *mpcmult,
   char *labmpc, int labmpcLen, int *nk, int *memmpc_, int *icascade, int *maxlenmpc,
   int *callfrommain){

 /*   detects cascaded mpc's and decascades them; checks multiple
     occurrence of the same dependent DOF's in different mpc/spc's

     data structure of ipompc,coefmpc,nodempc:
       for each mpc, e.g. i, 
         -the nodes are stored in nodempc(1,ipompc(i)),
          nodempc(1,nodempc(3,ipompc(i))),
          nodempc(1,nodempc(3,nodempc(3,ipompc(i))))... till
          nodempc(3,nodempc(3,nodempc(3,.......))))))=0;
         -the corresponding directions in nodempc(2,ipompc(i)),
          nodempc(2,nodempc(3,ipompc(i))),.....
         -the corresponding coefficient in coefmpc(ipompc(i)),
          coefmpc(nodempc(3,ipompc(i))),.....
       the mpc is written as a(1)u(i1,j1)+a(2)u(i2,j2)+...
       +....a(k)u(ik,jk)=0, the first term is the dependent term,
       the others are independent, at least after execution of the
       present routine. The mpc's must be homogeneous, otherwise a
       error message is generated and the program stops. */

    int i,j,index,id,idof,nterm,irow,icolumn,node,idir,idepend,
        irownl,icolnl,*ipointer=NULL,*icoef=NULL,ifree,*nodempc=NULL,
	*indepdof=NULL,nindep,ispooles,iexpand,ichange,indexold,ip1,
        mpc,indexnew,index1,index2,index1old,index2old,*jmpc=NULL,nl;

    double coef,*xcoef=NULL,*coefmpc=NULL,b;

    nodempc=*nodempcp;
    coefmpc=*coefmpcp;

    jmpc=NNEW(int,*nmpc);
    *icascade=0;
    idepend=0;

/*        check whether a node is used as a dependent node in a MPC
	  and in a SPC */

    for(i=0;i<*nmpc;i++){
	if(*nboun>0){
	    FORTRAN(nident,(ikboun,&ikmpc[i],nboun,&id));}
	else{id=0;}
	if(id>0){
	    if(ikboun[id-1]==ikmpc[i]){
		printf("*ERROR in cascade: the DOF corresponding to \n node %d in direction %d is detected on the \n dependent side of a MPC and a SPC\n",
		       (ikmpc[i]-1)/7+1,ikmpc[i]-3*((ikmpc[i]-1)/7));
		FORTRAN(stop,());
	    }
	}
    }

/*     check whether there are user mpc's other than MEANROT:
       return if called from Calculix.c   */

    for(i=0;i<*nmpc;i++){

        /* linear mpc */

	if((strcmp1(&labmpc[20*i],"                    ")==0) ||
	   (strcmp1(&labmpc[20*i],"CYCLIC")==0) ||
	   (strcmp1(&labmpc[20*i],"SUBCYCLIC")==0)) jmpc[i]=0;

        /* nonlinear mpc */

	else if((strcmp1(&labmpc[20*i],"RIGID")==0) ||
	   (strcmp1(&labmpc[20*i],"PLANE")==0) ||
	   (strcmp1(&labmpc[20*i],"STRAIGHT")==0)) jmpc[i]=1;

        /* user mpc */

	else{
	    jmpc[i]=1;
	    *icascade=1;
	}
    }

/*     decascading */

    ispooles=0;

    /* decascading using simple substitution */

    do{
        ichange=0;
	/*  printf("ichange=%d\n",ichange);*/
        for(i=0;i<*nmpc;i++){
	    if(jmpc[i]==1) nl=1;
	    else nl=0;
	    iexpand=0;
	    index=nodempc[3*ipompc[i]-1];
	    if(index==0) continue;
	    do{
		idof=(nodempc[3*index-3]-1)*7+nodempc[3*index-2];
		FORTRAN(nident,(ikmpc,&idof,nmpc,&id));
		if((id>0)&&(ikmpc[id-1]==idof)){
		    
		    /* a term on the independent side of the MPC is
		       detected as dependent node in another MPC */
		    
		    /* printf("*INFO in cascade: DOF %d of node %d is expanded\n",
			   nodempc[3*index-2],nodempc[3*index-3]);
		    ichange=1;iexpand=1; */
		    indexold=nodempc[3*index-1];
		    coef=coefmpc[index-1];
		    mpc=ilmpc[id-1];

                    /* no expansion of there is a dependence of a
                       nonlinear MPC on another linear or nonlinear MPC
                       and the call is from main */ 

		    if((jmpc[mpc-1]==1)||(nl==1)){
			*icascade=2;
			if(idepend==0){
			    printf("*INFO in cascade: linear MPCs and\n");
			    printf("       nonlinear MPCs depend on each other\n\n");
			    idepend=1;}
			/* FORTRAN(stop,()); */
			if(*callfrommain==1){
			    index=nodempc[3*index-1];
			    if(index!=0) continue;
			    else break;}
			ip1=i+1;
			/*FORTRAN(writempc,(ipompc,nodempc,coefmpc,&ip1));
			  FORTRAN(writempc,(ipompc,nodempc,coefmpc,&mpc));*/
		    }
#ifndef SPEC_CPU
/* reduce i/o */
		    printf("*INFO in cascade: DOF %d of node %d is expanded\n",
			   nodempc[3*index-2],nodempc[3*index-3]);
#endif
		    ichange=1;iexpand=1;
		    if((strcmp1(&labmpc[20*i],"                    ")==0)&&
		       (strcmp1(&labmpc[20*(mpc-1)],"CYCLIC")==0))
			strcpy1(&labmpc[20*i],"SUBCYCLIC",9);
		    indexnew=ipompc[mpc-1];
		    coef=-coef/coefmpc[indexnew-1];
		    indexnew=nodempc[3*indexnew-1];
		    do{
			coefmpc[index-1]=coef*coefmpc[indexnew-1];
			nodempc[3*index-3]=nodempc[3*indexnew-3];
			nodempc[3*index-2]=nodempc[3*indexnew-2];
			indexnew=nodempc[3*indexnew-1];
			if(indexnew!=0){
			    nodempc[3*index-1]=*mpcfree;
			    index=*mpcfree;
			    *mpcfree=nodempc[3**mpcfree-1];
			    if(*mpcfree==0){
				*mpcfree=*memmpc_+1;
				nodempc[3*index-1]=*mpcfree;
				*memmpc_=(int)(1.1**memmpc_);
				printf("*INFO in cascade: reallocating nodempc; new size = %d\n",*memmpc_);
				RENEW(nodempc,int,3**memmpc_);
				RENEW(coefmpc,double,*memmpc_);
				for(j=*mpcfree;j<*memmpc_;j++){
				    nodempc[3*j-1]=j+1;
				}
				nodempc[3**memmpc_-1]=0;
			    }
			    continue;
			}
			else{
			    nodempc[3*index-1]=indexold;
			    break;
			}
		    }while(1);
		    break;
		}
		else{
		    index=nodempc[3*index-1];
		    if(index!=0) continue;
		    else break;
		}
	    }while(1);
	    if(iexpand==0) continue;
	    
	    /* one term of the mpc was expanded 
	       collecting terms corresponding to the same DOF */
	    
	    index1=ipompc[i];
	    do{
		index2old=index1;
		index2=nodempc[3*index1-1];
		if(index2==0) break;
		do{
		    if((nodempc[3*index1-3]==nodempc[3*index2-3])&&
		       (nodempc[3*index1-2]==nodempc[3*index2-2])){
			coefmpc[index1-1]+=coefmpc[index2-1];
			nodempc[3*index2old-1]=nodempc[3*index2-1];
			nodempc[3*index2-1]=*mpcfree;
			*mpcfree=index2;
			index2=nodempc[3*index2old-1];
			if(index2==0) break;
		    }
		    else{
			index2old=index2;
			index2=nodempc[3*index2-1];
			if(index2==0) break;
		    }
		}while(1);
		index1=nodempc[3*index1-1];
		if(index1==0) break;
	    }while(1);
	    
	    /* check for zero coefficients on the dependent and
	       independent side */
	    
	    index1=ipompc[i];
	    index1old=0;
	    do {
		if(fabs(coefmpc[index1-1])<1.e-10){
		    if(index1old==0){
			printf("*ERROR in cascade: zero coefficient on the\n");
			printf("       dependent side of an equation\n");
			FORTRAN(stop,());
		    }
		    else{
			nodempc[3*index1old-1]=nodempc[3*index1-1];
			nodempc[3*index1-1]=*mpcfree;
			*mpcfree=index1;
			index1=nodempc[3*index1old-1];
		    }
		}
		else{
		    index1old=index1;
		    index1=nodempc[3*index1-1];
		}
		if(index1==0) break;
	    }while(1);
        }
        if(ichange==0) break;
    }while(1);
    
/*     determining the effective size of nodempc and coefmpc for
       the reallocation*/

    *mpcend=0;
    *mpcmult=0;
    *maxlenmpc=0;
/*    printf("overview:\n");*/
    for(i=0;i<*nmpc;i++){
	index=ipompc[i];
	*mpcend=max(*mpcend,index);
	nterm=1;
	while(1){
	    /*     printf("%d,%d,%d,%f\n",i+1,nodempc[3*index-3],nodempc[3*index-2],
		   coefmpc[index-1]);*/
	    index=nodempc[3*index-1];
	    if(index==0){
		*mpcmult+=nterm*(nterm-1);
		*maxlenmpc=max(*maxlenmpc,nterm);
		break;
	    }
	    *mpcend=max(*mpcend,index);
	    nterm++;
	}
    }

    free(jmpc);

/*    *mpcfree=0;*/
    *nodempcp=nodempc;
    *coefmpcp=coefmpc;
    
    /*   for(i=0;i<*nmpc;i++){
	ip1=i+1;
	FORTRAN(writempc,(ipompc,nodempc,coefmpc,labmpc,&ip1));
	}*/
    
    return;
}
/*
      subroutine cascade(ipompc,coefmpc,nodempc,nmpc,mpcfree,
     &                   nodeboun,ndirboun,nboun,ikmpc,ilmpc,
     &                   ikboun,ilboun,mpcend,mpcmult,labmpc,
     &                   nk,ipointer,icoef,xcoef,memmpc_)
!
!     detects cascaded mpc's and decascades them; checks multiple
!     occurrence of the same dependent DOF's in different mpc/spc's
!
!     data structure of ipompc,coefmpc,nodempc:
!       for each mpc, e.g. i, 
!         -the nodes are stored in nodempc(1,ipompc(i)),
!          nodempc(1,nodempc(3,ipompc(i))),
!          nodempc(1,nodempc(3,nodempc(3,ipompc(i))))... till
!          nodempc(3,nodempc(3,nodempc(3,.......))))))=0;
!         -the corresponding directions in nodempc(2,ipompc(i)),
!          nodempc(2,nodempc(3,ipompc(i))),.....
!         -the corresponding coefficient in coefmpc(ipompc(i)),
!          coefmpc(nodempc(3,ipompc(i))),.....
!       the mpc is written as a(1)u(i1,j1)+a(2)u(i2,j2)+...
!       +....a(k)u(ik,jk)=0, the first term is the dependent term,
!       the others are independent, at least after execution of the
!       present routine. The mpc's must be homogeneous, otherwise a
!       error message is generated and the program stops.
!
      implicit none
!
      character*20 labmpc(*)
!
      integer nodeboun(*),ndirboun(*),ipompc(*),nodempc(3,*),
     &  ikmpc(*),ilmpc(*),ikboun(*),ilboun(*),nmpc,mpcfree,nboun,
     &  i,j,index,id,idof,kflag,mpcend,mpcmult,
     &  nterm,icascade,nk,irow,icolumn,node,idir,memmpc_,
     &  irownl,icolnl
!
      integer ipointer(*),icoef(2,*),ifree
!
      real*8 coefmpc(*),coef,xcoef(*)
!
      icascade=0
      kflag=2
!
      do i=1,nmpc
!
!        check whether a node is used as a dependent node in a MPC
!        and in a SPC
!
         if(nboun.gt.0) then
            call nident(ikboun,ikmpc(i),nboun,id)
         else
            id=0
         endif
!
         if(id.gt.0) then
            if(ikboun(id).eq.ikmpc(i)) then
               write(*,101)
     &              (ikmpc(i)-1)/3+1, ikmpc(i)-3*((ikmpc(i)-1)/3)
               stop
            endif
         endif
      enddo
!
!     check whether at least one MPC depends on another one
!
      loop:do i=1,nmpc
         do
            index=nodempc(3,ipompc(i))
            if(index.eq.0) cycle loop
            do
               idof=(nodempc(1,index)-1)*3+nodempc(2,index)
               call nident(ikmpc,idof,nmpc,id)
               if(id.gt.0)then
                  if(ikmpc(id).eq.idof) then
                     icascade=1
                     exit loop
                  endif
               endif
               index=nodempc(3,index)
               if(index.ne.0) then
                  cycle
               else
                  cycle loop
               endif
            enddo
!
         enddo
      enddo loop
!
      do i=1,nmpc
         write(*,*) i,ikmpc(i),ilmpc(i)
      enddo
      write(*,*) 'icascade',icascade
!
!     decascading
!
!     initialization
!
      if(icascade.eq.1) then
         do i=1,3*nk
            ipointer(i)=0
         enddo
         ifree=1
!
!     building the data base
!
         do i=1,nmpc
            index=ipompc(i)
            do
               idof=3*(nodempc(1,index)-1)+nodempc(2,index)
               icoef(1,ifree)=i
               icoef(2,ifree)=ipointer(idof)
               xcoef(ifree)=coefmpc(index)
               ipointer(idof)=ifree
               ifree=ifree+1
               index=nodempc(3,index)
               if(index.eq.0) exit
            enddo
         enddo
!
!     reinitialize nodempc
!
         do j=1,memmpc_
            nodempc(3,j)=j+1
         enddo
         nodempc(3,memmpc_)=0
         mpcfree=1
         do j=1,nmpc
            ipompc(j)=0
         enddo
!
!     filling the left hand side
!
         do i=1,nmpc
            idof=ikmpc(i)
            icolumn=ilmpc(i)
            if(labmpc(icolumn)(1:5).eq.'RIGID') then
               icolnl=1
            else
               icolnl=0
            endif
            index=ipointer(idof)
            do
               irow=icoef(1,index)
!
!              check interaction of linear and nonlinear MPCs
!
               if(irow.ne.icolumn) then
                  if(labmpc(irow)(1:5).eq.'RIGID') then
                     irownl=1
                  else
                     irownl=0
                  endif
                  if((irownl.eq.1).or.(icolnl.eq.1)) then
                     icascade=2
                  endif
               endif
!
!              check interaction of linear and cyclic MPCs
!
               if((labmpc(irow).eq.'                    ').and.
     &            (labmpc(icolumn)(1:6).eq.'CYCLIC'))
     &            labmpc(irow)(1:9)='SUBCYCLIC'
               coef=xcoef(index)
               if((labmpc(irow).eq.'                    ').and.
     &              (labmpc(icolumn)(1:6).eq.'CYCLIC'))
     &              labmpc(irow)(1:9)='SUBCYCLIC'
               write(*,*) ' lhs ',irow,icolumn,coef
               index=icoef(2,index)
               if(index.eq.0) exit
            enddo
            ipointer(idof)=0
         enddo
!
!     filling the right hand side
!
         do idof=1,3*nk
            if(ipointer(idof).gt.0) then
!
!           new right hand side column
!
               index=ipointer(idof)
               do
                  irow=icoef(1,index)
                  coef=xcoef(index)
                  write(*,*) ' rhs ', irow,idof,coef
                  index=icoef(2,index)
                  if(index.eq.0) exit
               enddo
!
!           solve ax=b
!
               do j=1,nmpc
c                 if(dabs(b(j)).gt.1.d-10) then
                  if(33.gt.1.d-10) then
                     nodempc(3,mpcfree)=ipompc(j)
                     node=int((idof+2)/3)
                     idir=idof-3*(node-1)
                     nodempc(1,mpcfree)=node
                     nodempc(2,mpcfree)=idir
c                    coefmpc(mpcfree)=b(j)
                     coefmpc(mpcfree)=33333333333333.
                     ipompc(j)=mpcfree
                     mpcfree=mpcfree+1
                     if(mpcfree.gt.memmpc_) write(*,*) 'reallocate'
                  endif
               enddo
            endif
         enddo
!
!     diagonal terms
!
         do i=1,nmpc
            j=ilmpc(i)
            idof=ikmpc(i)
            node=int((idof+2)/3)
            idir=idof-3*(node-1)
            nodempc(3,mpcfree)=ipompc(j)
            nodempc(1,mpcfree)=node
            nodempc(2,mpcfree)=idir
            coefmpc(mpcfree)=1.d0
            ipompc(j)=mpcfree
            mpcfree=mpcfree+1
            if(mpcfree.gt.memmpc_) write(*,*) 'reallocate'
         enddo
!
      endif
!
!     determining the effective size of nodempc and coefmpc for
!     the reallocation
!
      write(*,*) 'summary'
      mpcend=0
      mpcmult=0
      do i=1,nmpc
         index=ipompc(i)
         mpcend=max(mpcend,index)
         nterm=1
         do
            write(*,*) i,nodempc(1,index),nodempc(2,index),
     &           coefmpc(index)
            index=nodempc(3,index)
            if(index.eq.0) then
               mpcmult=mpcmult+nterm*(nterm-1)
               exit
            endif
            mpcend=max(mpcend,index)
            nterm=nterm+1
         enddo
      enddo
!
 101  format(/,'*ERROR in cascade: the DOF corresponding to',
     &           /,'node',i5,' in direction',i5,' is detected on',
     &           /,'the dependent side of a MPC and a SPC')
!
      i=1
      if(i.eq.1) stop
      return
      end
*/












