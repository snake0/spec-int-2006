/*     CalculiX - A 3-dimensional finite element program                   */
/*              Copyright (C) 1998 Guido Dhondt                          */

/*     This program is free software; you can redistribute it and/or     */
/*     modify it under the terms of the GNU General Public License as    */
/*     published by the Free Software Foundation(version 2);    */
/*                    */

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
#include <misc.h>
#include <FrontMtx.h>
#include <SymbFac.h>
#include "CalculiX.h"

void spooles(double *ad, double *au, double *b, int *icol, int *irow, 
	     int *neq, int *nzs){
  
  int ipoint,ipo;
  DenseMtx        *mtxB, *mtxX ;
  Chv             *rootchv ;
  ChvManager      *chvmanager  ;
  SubMtxManager   *mtxmanager  ;
  FrontMtx        *frontmtx ;
  InpMtx          *mtxA ;
  double          tau = 100.;
  double          cpus[10] ;
  ETree           *frontETree ;
  FILE            *msgFile ;
  Graph           *graph ;
  int             jrow, jrhs, msglvl=0, ncol, nedges,error, 
                  nent, neqns, nrhs, nrow, pivotingflag=1, seed=7892713, 
                  symmetryflag=0, type=1,row,col,maxdomainsize,maxzeros,
                  maxsize;
  int             *newToOld, *oldToNew ;
  int             stats[20] ;
  IV              *newToOldIV, *oldToNewIV ;
  IVL             *adjIVL, *symbfacIVL ;

  /* solving the system of equations using spooles */
#ifndef SPEC_CPU
  printf("Solving the system of equations using spooles\n\n");
#endif
/*
   --------------------------------------------------
   all-in-one program to solve A X = B
   (1) read in matrix entries and form DInpMtx object
   (2) form Graph object
   (3) order matrix and form front tree
   (4) get the permutation, permute the matrix and 
       front tree and get the symbolic factorization
   (5) compute the numeric factorization
   (6) read in right hand side entries
   (7) compute the solution

   created -- 98jun04, cca
   --------------------------------------------------
*/
    if ( (msgFile = fopen("spooles.out", "a")) == NULL ) {
      fprintf(stderr, "\n fatal error in spooles.c"
	      "\n unable to open file spooles.out\n") ;
    }
/*
   --------------------------------------------
   STEP 1: read the entries from the input file 
           and create the InpMtx object
   --------------------------------------------
*/
    nrow=*neq;
    ncol=*neq;
    nent=*nzs+*neq;
    neqns=nrow;
    ipoint=0;
    mtxA = InpMtx_new() ;
    InpMtx_init(mtxA, INPMTX_BY_ROWS, type, nent, neqns) ;
    for(row=0;row<nrow;row++){
      InpMtx_inputRealEntry(mtxA,row,row,ad[row]);
      for(ipo=ipoint;ipo<ipoint+icol[row];ipo++){
	col=irow[ipo]-1;
	InpMtx_inputRealEntry(mtxA,row,col,au[ipo]);
      }
      ipoint=ipoint+icol[row];
    }
	
    InpMtx_changeStorageMode(mtxA, INPMTX_BY_VECTORS) ;
    if ( msglvl > 1 ) {
      fprintf(msgFile, "\n\n input matrix") ;
      InpMtx_writeForHumanEye(mtxA, msgFile) ;
      fflush(msgFile) ;
    }
/*--------------------------------------------------------------------*/
/*
   -------------------------------------------------
   STEP 2 : find a low-fill ordering
   (1) create the Graph object
   (2) order the graph using multiple minimum degree
   -------------------------------------------------
*/
    graph = Graph_new() ;
    adjIVL = InpMtx_fullAdjacency(mtxA) ;
    nedges = IVL_tsize(adjIVL) ;
    Graph_init2(graph, 0, neqns, 0, nedges, neqns, nedges, adjIVL,
		NULL, NULL) ;
    if ( msglvl > 1 ) {
      fprintf(msgFile, "\n\n graph of the input matrix") ;
      Graph_writeForHumanEye(graph, msgFile) ;
      fflush(msgFile) ;
    }
    /*maxdomainsize=neqns/100;*/
    /* frontETree = orderViaMMD(graph, seed, msglvl, msgFile) ; */
    /* frontETree = orderViaND(graph,maxdomainsize,seed,msglvl,msgFile); */
    /*frontETree = orderViaMS(graph,maxdomainsize,seed,msglvl,msgFile);*/
    maxdomainsize=800;maxzeros=1000;maxsize=64;
    frontETree=orderViaBestOfNDandMS(graph,maxdomainsize,maxzeros,
      maxsize,seed,msglvl,msgFile);
    if ( msglvl > 1 ) {
      fprintf(msgFile, "\n\n front tree from ordering") ;
      ETree_writeForHumanEye(frontETree, msgFile) ;
      fflush(msgFile) ;
}
/*--------------------------------------------------------------------*/
/*
   -----------------------------------------------------
   STEP 3: get the permutation, permute the matrix and 
           front tree and get the symbolic factorization
   -----------------------------------------------------
*/
    oldToNewIV = ETree_oldToNewVtxPerm(frontETree) ;
    oldToNew = IV_entries(oldToNewIV) ;
    newToOldIV = ETree_newToOldVtxPerm(frontETree) ;
    newToOld   = IV_entries(newToOldIV) ;
    ETree_permuteVertices(frontETree, oldToNewIV) ;
    InpMtx_permute(mtxA, oldToNew, oldToNew) ;
    InpMtx_mapToUpperTriangle(mtxA) ;
    InpMtx_changeCoordType(mtxA,INPMTX_BY_CHEVRONS);
    InpMtx_changeStorageMode(mtxA,INPMTX_BY_VECTORS);
    symbfacIVL = SymbFac_initFromInpMtx(frontETree, mtxA) ;
    if ( msglvl > 1 ) {
      fprintf(msgFile, "\n\n old-to-new permutation vector") ;
      IV_writeForHumanEye(oldToNewIV, msgFile) ;
      fprintf(msgFile, "\n\n new-to-old permutation vector") ;
      IV_writeForHumanEye(newToOldIV, msgFile) ;
      fprintf(msgFile, "\n\n front tree after permutation") ;
      ETree_writeForHumanEye(frontETree, msgFile) ;
      fprintf(msgFile, "\n\n input matrix after permutation") ;
      InpMtx_writeForHumanEye(mtxA, msgFile) ;
      fprintf(msgFile, "\n\n symbolic factorization") ;
      IVL_writeForHumanEye(symbfacIVL, msgFile) ;
      fflush(msgFile) ;
    }
/*--------------------------------------------------------------------*/
/*
   ------------------------------------------
   STEP 4: initialize the front matrix object
   ------------------------------------------
*/
    frontmtx = FrontMtx_new() ;
    mtxmanager = SubMtxManager_new() ;
    SubMtxManager_init(mtxmanager, NO_LOCK, 0) ;
    FrontMtx_init(frontmtx, frontETree, symbfacIVL, type, symmetryflag, 
		  FRONTMTX_DENSE_FRONTS, pivotingflag, NO_LOCK, 0, NULL, 
		  mtxmanager, msglvl, msgFile) ;
/*--------------------------------------------------------------------*/
/*
   -----------------------------------------
   STEP 5: compute the numeric factorization
   -----------------------------------------
*/
    chvmanager = ChvManager_new() ;
    ChvManager_init(chvmanager, NO_LOCK, 1) ;
    DVfill(10, cpus, 0.0) ;
    IVfill(20, stats, 0) ;
    rootchv = FrontMtx_factorInpMtx(frontmtx, mtxA, tau, 0.0, chvmanager,
				    &error,cpus, stats, msglvl, msgFile) ;
    ChvManager_free(chvmanager) ;
    if ( msglvl > 1 ) {
      fprintf(msgFile, "\n\n factor matrix") ;
      FrontMtx_writeForHumanEye(frontmtx, msgFile) ;
      fflush(msgFile) ;
    }
    if ( rootchv != NULL ) {
      fprintf(msgFile, "\n\n matrix found to be singular\n") ;
      exit(-1) ;
    }
    if(error>=0){
      fprintf(msgFile,"\n\nerror encountered at front %d",error);
      exit(-1);
    }
/*--------------------------------------------------------------------*/
/*
   --------------------------------------
   STEP 6: post-process the factorization
   --------------------------------------
*/
    FrontMtx_postProcess(frontmtx, msglvl, msgFile) ;
    if ( msglvl > 1 ) {
      fprintf(msgFile, "\n\n factor matrix after post-processing") ;
      FrontMtx_writeForHumanEye(frontmtx, msgFile) ;
      fflush(msgFile) ;
    }
/*--------------------------------------------------------------------*/
/*
   -----------------------------------------
   STEP 7: read the right hand side matrix B
   -----------------------------------------
*/
    nrhs=1;
    mtxB = DenseMtx_new() ;
    DenseMtx_init(mtxB, type, 0, 0, neqns, nrhs, 1, neqns) ;
    DenseMtx_zero(mtxB) ;
    for ( jrow = 0 ; jrow < nrow ; jrow++ ) {
      for ( jrhs = 0 ; jrhs < nrhs ; jrhs++ ) {
	DenseMtx_setRealEntry(mtxB, jrow, jrhs, b[jrow]) ;
      }
    }

    if ( msglvl > 1 ) {
      fprintf(msgFile, "\n\n rhs matrix in original ordering") ;
      DenseMtx_writeForHumanEye(mtxB, msgFile) ;
      fflush(msgFile) ;
    }
/*--------------------------------------------------------------------*/
/*
   ---------------------------------------------------------
   STEP 8: permute the right hand side into the new ordering
   ---------------------------------------------------------
*/
    DenseMtx_permuteRows(mtxB, oldToNewIV) ;
    if ( msglvl > 1 ) {
      fprintf(msgFile, "\n\n right hand side matrix in new ordering") ;
      DenseMtx_writeForHumanEye(mtxB, msgFile) ;
      fflush(msgFile) ;
    }
/*--------------------------------------------------------------------*/
/*
   -------------------------------
   STEP 9: solve the linear system
   -------------------------------
*/
    mtxX = DenseMtx_new() ;
    DenseMtx_init(mtxX, type, 0, 0, neqns, nrhs, 1, neqns) ;
    DenseMtx_zero(mtxX) ;
    FrontMtx_solve(frontmtx, mtxX, mtxB, mtxmanager,cpus, msglvl, msgFile) ;
    if ( msglvl > 1 ) {
      fprintf(msgFile, "\n\n solution matrix in new ordering") ;
      DenseMtx_writeForHumanEye(mtxX, msgFile) ;
      fflush(msgFile) ;
    }
/*--------------------------------------------------------------------*/
/*
   --------------------------------------------------------
   STEP 10: permute the solution into the original ordering
   --------------------------------------------------------
*/
    DenseMtx_permuteRows(mtxX, newToOldIV) ;
    /* *ipb=DenseMtx_entries(mtxX); */
    if ( msglvl > 1 ) {
      fprintf(msgFile, "\n\n solution matrix in original ordering") ;
      DenseMtx_writeForHumanEye(mtxX, msgFile) ;
      fflush(msgFile) ;
    }
/*--------------------------------------------------------------------*/

    for ( jrow = 0 ; jrow < nrow ; jrow++ ) {
      b[jrow]=DenseMtx_entries(mtxX)[jrow];
    }

/*
   -----------
   free memory
   -----------
*/
    FrontMtx_free(frontmtx) ;
    DenseMtx_free(mtxX) ;
    DenseMtx_free(mtxB) ;
    IV_free(newToOldIV) ;
    IV_free(oldToNewIV) ;
    InpMtx_free(mtxA) ;
    ETree_free(frontETree) ;
    IVL_free(symbfacIVL) ;
    SubMtxManager_free(mtxmanager) ;
    Graph_free(graph) ;

/*--------------------------------------------------------------------*/

    fclose(msgFile);
    
  return;
}
