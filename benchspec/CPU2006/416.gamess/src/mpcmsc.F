C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C  3 SEP 03 - MWS - FOCK2: REMOVE TRANSLATION VECTOR
C 16 JUN 03 - HL  - INCREASE THE SIZE OF NHCO(4,200)
C 12 DEC 02 - MWS - MPCPRP: SUPPRESS MAKEFP OUTPUT UNLESS THAT RUNTYP
C  7 AUG 02 - HL  - MPCPRP: USE AM1 AND PM3 CHARGES TO MAKE EFP
C 25 JUN 01 - MWS - ALTER COMMON BLOCK WFNOPT
C  7 FEB 97 - MWS - DIPOLE: REWRITE LOOPS CLEANER FOR CRAY
C 26 NOV 96 - DGF - MPCG: GENERALIZE FOR RHF/ROHF/UHF/ADDING GVB
C 13 JUN 96 - VAG - INTRODUCE CITYP VARIABLE
C  6 APR 95 - MWS - MPCPRP: USE FORMAT STATEMENT
C 13 FEB 95 - MWS - MPCGU: DELETE SPURIOUS CALL TO RETFM
C 11 NOV 94 - MWS - REMOVE FTNCHEK WARNINGS
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C 17 MAR 92 - MWS - ELIMINATE EPRINT ROUTINE
C 21 MAR 92 - JHJ - MISCELLANEOUS MOPAC ROUTINES ADDED TO GAMESS
C
C         THE FOLLOWING STATEMENT APPEARED IN MOPAC 6.0 AND
C         IS REPRODUCED HERE TO COMPLY WITH SAID STATEMENT.
C
C         NOTICE OF PUBLIC DOMAIN NATURE OF THIS PROGRAM
C
C      'THIS COMPUTER PROGRAM IS A WORK OF THE UNITED STATES
C       GOVERNMENT AND AS SUCH IS NOT SUBJECT TO PROTECTION BY
C       COPYRIGHT (17 U.S.C. # 105.)  ANY PERSON WHO FRAUDULENTLY
C       PLACES A COPYRIGHT NOTICE OR DOES ANY OTHER ACT CONTRARY
C       TO THE PROVISIONS OF 17 U.S. CODE 506(C) SHALL BE SUBJECT
C       TO THE PENALTIES PROVIDED THEREIN.  THIS NOTICE SHALL NOT
C       BE ALTERED OR REMOVED FROM THIS SOFTWARE AND IS TO BE ON
C       ALL REPRODUCTIONS.'
C
C*MODULE MPCMSC  *DECK CHRGE
      SUBROUTINE CHRGE(P,Q)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MXATM=500)
      DIMENSION P(*),Q(*)
      COMMON /MOLKST/ NUMAT,NAT(MXATM),NFIRST(MXATM),NMIDLE(MXATM),
     1                NLAST(MXATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
C***********************************************************************
C
C      CHRGE STORES IN Q THE TOTAL ELECTRON DENSITIES ON THE ATOMS
C
C      ON INPUT P      = DENSITY MATRIX
C
C      ON OUTPUT Q     = ATOM ELECTRON DENSITIES
C
C***********************************************************************
      K=0
      DO 10 I=1,NUMAT
         IA=NFIRST(I)
         IB=NLAST(I)
         Q(I)=0.0D+00
         DO 10 J=IA,IB
            K=K+J
   10 Q(I)=Q(I)+P(K)
      RETURN
      END
C*MODULE MPCMSC  *DECK DIPOLE
      SUBROUTINE DIPOLE(DIPVAL,P,COORD,CHARG)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      DIMENSION P(*),COORD(3,*)
      DIMENSION DIP(4,3),HYF(107,2)
C
      CHARACTER*241 KEYWRD
      LOGICAL FORCE, CHARGD
C
      COMMON /MOLKST/ NUMAT,NAT(MXATM),NFIRST(MXATM), NMIDLE(MXATM),
     1                NLAST(MXATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /DIPSTO/ UX,UY,UZ,Q(MXATM)
      COMMON /KEYWRD/ KEYWRD
      COMMON /MOLMEC/ HTYPE(4),NHCO(4,200),NNHCO,ITYPE
      COMMON /ISTOPE/ AMS(107)
      COMMON /MULTIP/ DD(107), QQ(107), AM(107), AD(107), AQ(107)
      COMMON /XYZPRP/ CENTER(3)
     *               ,DIPVEC(3)
     *               ,QXX,QYY,QZZ,QXY,QXZ,QYZ
     *               ,QMXX,QMYY,QMZZ,QMXY,QMXZ,QMYZ
     *               ,OXXX,OXXY,OXXZ,OXYY,OYYY,OYYZ
     *               ,OXZZ,OYZZ,OZZZ,OXYZ
     *               ,OMXXX,OMXXY,OMXXZ,OMXYY,OMYYY
     *               ,OMYYZ,OMXZZ,OMYZZ,OMZZZ,OMXYZ
C
      SAVE HYF, DIP, WTMOL, CHARGD, FORCE
C
      DATA HYF(1,1)     / 0.0D+00           /
      DATA   HYF(1,2) /0.0D+00     /
      DATA   HYF(5,2) /6.520587D+00/
      DATA   HYF(6,2) /4.253676D+00/
      DATA   HYF(7,2) /2.947501D+00/
      DATA   HYF(8,2) /2.139793D+00/
      DATA   HYF(9,2) /2.2210719D+00/
      DATA   HYF(14,2)/6.663059D+00/
      DATA   HYF(15,2)/5.657623D+00/
      DATA   HYF(16,2)/6.345552D+00/
      DATA   HYF(17,2)/2.522964D+00/
C***********************************************************************
C     DIPOLE CALCULATES DIPOLE MOMENTS
C
C  ON INPUT P     = DENSITY MATRIX
C           Q     = TOTAL ATOMIC CHARGES, (NUCLEAR + ELECTRONIC)
C           NUMAT = NUMBER OF ATOMS IN MOLECULE
C           NAT   = ATOMIC NUMBERS OF ATOMS
C           NFIRST= START OF ATOM ORBITAL COUNTERS
C           COORD = COORDINATES OF ATOMS
C
C  OUTPUT  DIPOLE = DIPOLE MOMENT
C***********************************************************************
C
C     IN THE ZDO APPROXIMATION, ONLY TWO TERMS ARE RETAINED IN THE
C     CALCULATION OF DIPOLE MOMENTS.
C     1. THE POINT CHARGE TERM (INDEPENDENT OF PARAMETERIZATION).
C     2. THE ONE-CENTER HYBRIDIZATION TERM, WHICH ARISES FROM MATRIX
C     ELEMENTS OF THE FORM <NS/R/NP>. THIS TERM IS A FUNCTION OF
C     THE SLATER EXPONENTS (ZS,ZP) AND IS THUS DEPENDENT ON PARAMETER-
C     IZATION. THE HYBRIDIZATION FACTORS (HYF(I)) USED IN THIS SUB-
C     ROUTINE ARE CALCULATED FROM THE FOLLOWING FORMULAE.
C     FOR SECOND ROW ELEMENTS <2S/R/2P>
C     HYF(I)= 469.56193322*(SQRT(((ZS(I)**5)*(ZP(I)**5)))/
C           ((ZS(I) + ZP(I))**6))
C     FOR THIRD ROW ELEMENTS <3S/R/3P>
C     HYF(I)=2629.107682607*(SQRT(((ZS(I)**7)*(ZP(I)**7)))/
C           ((ZS(I) + ZP(I))**8))
C     FOR FOURTH ROW ELEMENTS AND UP :
C     HYF(I)=2*(2.10716)*DD(I)
C     WHERE DD(I) IS THE CHARGE SEPARATION IN ATOMIC UNITS
C
C     REFERENCES:
C     J.A.POPLE & D.L.BEVERIDGE: APPROXIMATE M.O. THEORY
C     S.P.MCGLYNN, ET AL: APPLIED QUANTUM CHEMISTRY
C
C
      DO 10 I=2,107
         HYF(I,1)= 5.0832D+00*DD(I)
   10 CONTINUE
      WTMOL=0.0D+00
      SUM=0.0D+00
      DO 20 I=1,NUMAT
         WTMOL=WTMOL+AMS(NAT(I))
         SUM=SUM+Q(I)
   20 CONTINUE
      CHARG=SUM
      CHARGD=(ABS(SUM).GT.0.5D+00)
      FORCE=(INDEX(KEYWRD,'FORCE') +INDEX(KEYWRD,'IRC').NE. 0)
      KTYPE=1
      IF(ITYPE.EQ.4) KTYPE=2
C
C   NEED TO RESET ION'S POSITION SO CENTER OF MASS IS AT THE ORIGIN.
C
      IF(.NOT.FORCE.AND.CHARGD) THEN
         CALL VCLR(CENTER,1,3)
         DO 40 I=1,3
            DO 30 J=1,NUMAT
               CENTER(I)=CENTER(I)+AMS(NAT(J))*COORD(I,J)
   30       CONTINUE
   40    CONTINUE
         CALL DSCAL(3,1.0D+00/WTMOL,CENTER,1)
         DO 60 I=1,3
            DO 50 J=1,NUMAT
               COORD(I,J)=COORD(I,J)-CENTER(I)
   50       CONTINUE
   60    CONTINUE
      END IF
C
      CALL VCLR(DIP,1,4*3)
      DO 190 I=1,NUMAT
         NI=NAT(I)
         IA=NFIRST(I)
         L=NLAST(I)-IA
         DO 130 J=1,L
            K=((IA+J)*(IA+J-1))/2+IA
            DIP(J,2)=DIP(J,2)-HYF(NI,KTYPE)*P(K)
  130    CONTINUE
         DO 170 J=1,3
            DIP(J,1)=DIP(J,1)+4.803D+00*Q(I)*COORD(J,I)
  170    CONTINUE
  190 CONTINUE
C
      DO 200 J=1,3
         DIP(J,3)=DIP(J,2)+DIP(J,1)
  200 CONTINUE
      DO 210 J=1,3
         DIP(4,J)=SQRT(DIP(1,J)**2+DIP(2,J)**2+DIP(3,J)**2)
  210 CONTINUE
C
      DIPVEC(1)=DIP(1,3)
      DIPVEC(2)=DIP(2,3)
      DIPVEC(3)=DIP(3,3)
C
C     STORE DIPOLE MOMENT COMPONENTS IN UX,UY,UZ FOR USE IN
C     ASSIGNING CHARGES DETERMINED FROM THE ESP. BHB
C
      UX=DIP(1,3)
      UY=DIP(2,3)
      UZ=DIP(3,3)
C
      DIPVAL = DIP(4,3)
      RETURN
      END
C*MODULE MPCMSC  *DECK FOCK1
      SUBROUTINE FOCK1(FJ,FK,PTOT,PA,HFCO)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
C     NOTE THAT FOR RHF,UHF,ROHF, -FJ- AND -FK- ARE EQUIVALENT BY CALL,
C     WHILE FOR GVB, THEY ARE DISTINCT STORAGE ARRAYS.
C
      DIMENSION FJ(*),FK(*),PTOT(*),PA(*)
      DIMENSION QTOT(MXATM), QA(MXATM)
C
      COMMON /GAUSS / FN1(107),FN2(107)
      COMMON /MOLKST/ NUMAT,NAT(MXATM),NFIRST(MXATM),NMIDLE(MXATM),
     *                NLAST(MXATM), NORBS, NELECS,NALPHA,NBETA,
     *                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /TWOELE/ GSS(107),GSP(107),GPP(107),GP2(107),HSP(107),
     *                GSD(107),GPD(107),GDD(107)
      PARAMETER (ONE=1.0D+00)
C
C *** COMPUTE THE REMAINING CONTRIBUTIONS TO THE ONE-CENTRE ELEMENTS.
C     ALL STATEMENTS "IF(HFCO.NE.ONE) FK(I)=FJ(I)" ARE ADDED TO COPE
C     WITH THE HIDDEN EQUIVALENCE. (FK AND FJ ARE THE SAME IF NOT GVB.)
C
      CALL CHRGE(PTOT,QTOT)
      CALL CHRGE(PA,QA)
C
      DO 100 II=1,NUMAT
         IA=NFIRST(II)
         IB=NMIDLE(II)
         IC=NLAST(II)
         NI=NAT(II)
         DTPOP=0.0D+00
         PTPOP=0.0D+00
         PAPOP=0.0D+00
         GO TO (100,40,30,30,30,20,20,20,20,20)IC-IA+2
   20    DTPOP=PTOT(( IC   *(IC+1))/2)
     *        +PTOT(((IC-1)* IC   )/2)
     1        +PTOT(((IC-2)*(IC-1))/2)
     *        +PTOT(((IC-3)*(IC-2))/2)
     2        +PTOT(((IC-4)*(IC-3))/2)
   30    PTPOP=PTOT(( IB   *(IB+1))/2)
     *        +PTOT(((IB-1)* IB   )/2)
     1        +PTOT(((IB-2)*(IB-1))/2)
         PAPOP=PA(( IB   *(IB+1))/2)
     *        +PA(((IB-1)* IB   )/2)
     1        +PA(((IB-2)*(IB-1))/2)
   40    CONTINUE
         IF(NI.EQ.1)THEN
            SUM=0.0D+00
         ELSE
            SUM2=0.0D+00
            SUM1=0.0D+00
            DO 60 I=IA,IB
               IM1=I-1
               DO 50 J=IA,IM1
   50          SUM1=SUM1+PTOT(J+(I*(I-1))/2)**2
   60       SUM2=SUM2+PTOT((I*(I+1))/2)**2
            SUM=SUM1*2.0D+00+SUM2
            SUM=SQRT(SUM)-QTOT(II)*0.5D+00
         END IF
         SUM=SUM*FN1(NI)
C
C     F(S,S)
C
         KA=(IA*(IA+1))/2
         FJ(KA)=FJ(KA) + PTOT(KA)*GSS(NI) + PTPOP*GSP(NI)
     1   +DTPOP*GSD(NI)
       IF(HFCO.NE.ONE) FK(KA)=FJ(KA)
         FK(KA)=FK(KA) + HFCO*(PA(KA)*GSS(NI) + PAPOP*HSP(NI))
         IF (NI.LT.3) GO TO 100
         IPLUS=IA+1
         L=KA
         DO 70 J=IPLUS,IB
            M=L+IA
            L=L+J
C
C     F(P,P)
C
            FJ(L)=FJ(L) + PTOT(KA)*GSP(NI) + PTOT(L)*GPP(NI)
     1      +(PTPOP-PTOT(L))*GP2(NI)+DTPOP*GPD(NI)
          IF(HFCO.NE.ONE) FK(L)=FJ(L)
            FK(L)=FK(L)+HFCO*(PA(KA)*HSP(NI)+PA(L)*GPP(NI)
     1      +0.5D+00*(PAPOP-PA(L))*(GPP(NI)-GP2(NI)))
C
C     F(S,P)
C
           FJ(M)=FJ(M)+2.0D+00*PTOT(M)*HSP(NI)
         IF(HFCO.NE.ONE) FK(M)=FJ(M)
           FK(M)=FK(M)+HFCO*PA(M)*(HSP(NI)+GSP(NI))
   70   CONTINUE
C
C     F(P,P*)
C
         IMINUS=IB-1
         DO 80 J=IPLUS,IMINUS
            ICC=J+1
            DO 80 L=ICC,IB
               M=(L*(L-1))/2+J
               FJ(M)=FJ(M)+PTOT(M)*(GPP(NI)-GP2(NI))
             IF(HFCO.NE.ONE) FK(M)=FJ(M)
               FK(M)=FK(M)+HFCO*0.5D+00*PA(M)*(GPP(NI)+GP2(NI))
   80    CONTINUE
         DO 90 J=IB+1,IC
            M=(J*(J+1))/2
            FJ(M)=FJ(M)+PTOT(KA)*GSD(NI)+PTPOP*GPD(NI)+DTPOP*GDD(NI)
          IF(HFCO.NE.ONE) FK(M)=FJ(M)
            FK(M)=FK(M)+HFCO*PA(M)*GDD(NI)
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
C*MODULE MPCMSC  *DECK FOCK2
      SUBROUTINE FOCK2(FJ,FK,PTOT,P,PTOT2,W,NUMAT,
     *                 NFIRST,NMIDLE,NLAST,HFCO)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MXATM=500, MAXORB=4*MXATM)
C
      LOGICAL LID
C
C     NOTE THAT FOR RHF,UHF,ROHF, -FJ- AND -FK- ARE EQUIVALENT BY CALL,
C     WHILE FOR GVB, THEY ARE DISTINCT STORAGE ARRAYS
C
      DIMENSION FJ(*), FK(*), PTOT(*), NFIRST(*), NMIDLE(*),
     1          NLAST(*), P(*), W(*), PTOT2(NUMAT,16)
      DIMENSION IFACT(MAXORB),I1FACT(MAXORB),JINDEX(256),
     *          IJPERM(10), LLPERM(10),PK(16),PJA(16),PJB(16),
     *          MMPERM(10)
C
      COMMON /MOLMEC/ HTYPE(4),NHCO(4,200),NNHCO,ITYPE
      COMMON /NUMCAL/ NUMCAL
C
      SAVE IFACT,I1FACT,IONE,LID,ICALCN,JINDEX,IJPERM,LLPERM,
     *     MMPERM
C
      DATA ICALCN/0/
C***********************************************************************
C
C FOCK2 FORMS THE TWO-ELECTRON TWO-CENTER REPULSION PART OF THE FOCK
C MATRIX
C ON INPUT  PTOT = TOTAL DENSITY MATRIX.
C           P    = ALPHA OR BETA DENSITY MATRIX.
C           W    = TWO-ELECTRON INTEGRAL MATRIX.
C
C  ON OUTPUT F   = PARTIAL FOCK MATRIX
C***********************************************************************
      IONE=1
      LID=.TRUE.
C
      IF(ICALCN.NE.NUMCAL) THEN
         ICALCN=NUMCAL
C
C   SET UP ARRAY OF LOWER HALF TRIANGLE INDICES (PASCAL'S TRIANGLE)
C
         DO 10 I=1,MAXORB
            IFACT(I)=(I*(I-1))/2
   10    I1FACT(I)=IFACT(I)+I
C
C   SET UP GATHER-SCATTER TYPE ARRAYS FOR USE WITH TWO-ELECTRON
C   INTEGRALS.  JINDEX ARE THE INDICES OF THE J-INTEGRALS FOR ATOM I
C   INTEGRALS.
C
         M=0
         DO 20 I=1,4
            DO 20 J=1,4
               IJ=MIN(I,J)
               JI=I+J-IJ
               DO 20 K=1,4
                  IK=MIN(I,K)
                  DO 20 L=1,4
                     M=M+1
                     KL=MIN(K,L)
                     LK=K+L-KL
                     JL=MIN(J,L)
   20    JINDEX(M)=(IFACT(JI) + IJ)*10 + IFACT(LK) + KL - 10
         L=0
         DO 30 I=1,4
            I1=(I-1)*4
            DO 30 J=1,I
               I1=I1+1
               L=L+1
               IJPERM(L)=I1
               MMPERM(L)=IJPERM(L)-16
               LLPERM(L)=(I1-1)*16
   30    CONTINUE
         L=0
         DO 40 I=1,10
            M=MMPERM(I)
            L=LLPERM(I)
            DO 40 K=1,16
               L=L+1
               M=M+16
   40    CONTINUE
         IONE=1
      END IF
C
C      END OF INITIALIZATION
C
      IF(ITYPE.EQ.4) GO TO 200
C
C     START OF MNDO, AM1, OR PM3 OPTION
C
      KK=0
      L=0
      DO 60 I=1,NUMAT
         IA=NFIRST(I)
         IB=NLAST(I)
         M=0
         DO 50 J=IA,IB
            DO 50 K=IA,IB
               M=M+1
               JK=MIN(J,K)
               KJ=K+J-JK
               JK=JK+(KJ*(KJ-1))/2
               PTOT2(I,M)=PTOT(JK)
   50    CONTINUE
   60 CONTINUE
      DO 190 II=1,NUMAT
         IA=NFIRST(II)
         IB=NLAST(II)
C
C  IF NUMAT=2 THEN WE ARE IN A DERIVATIVE OR IN A MOLECULE CALCULATION
C
         IF(NUMAT.NE.2)THEN
            IMINUS=II-IONE
         ELSE
            IMINUS=II-1
         END IF
         DO 180 JJ=1,IMINUS
            JA=NFIRST(JJ)
            JB=NLAST(JJ)
            JC=NMIDLE(JJ)
            IF(LID) THEN
               IF(IB-IA.GE.3.AND.JB-JA.GE.3)THEN
C
C                         HEAVY-ATOM  - HEAVY-ATOM
C
C   EXTRACT COULOMB TERMS
C
                  DO 70 I=1,16
                     PJA(I)=PTOT2(II,I)
   70             PJB(I)=PTOT2(JJ,I)
C
C  COULOMB TERMS
C
                  CALL JAB(IA,JA,PJA,PJB,W(KK+1),FJ)
C
C  EXCHANGE TERMS
C
C
C  EXTRACT INTERSECTION OF ATOMS II AND JJ IN THE SPIN DENSITY MATRIX
C
                  L=0
                  DO 80 I=IA,IB
                     I1=IFACT(I)+JA
                     DO 80 J=I1,I1+3
                        L=L+1
   80             PK(L)=P(J)
                  CALL KAB(IA,JA,PK,W(KK+1),FK,HFCO)
                  KK=KK+100
               ELSE IF(IB-IA.GE.3.AND.JA.EQ.JB)THEN
C
C                         LIGHT-ATOM  - HEAVY-ATOM
C
C
C   COULOMB TERMS
C
                  SUMDIA=0.0D+00
                  SUMOFF=0.0D+00
                  LL=I1FACT(JA)
                  K=0
                  DO 100 I=0,3
                     J1=IFACT(IA+I)+IA-1
                     DO 90 J=0,I-1
                        K=K+1
                        J1=J1+1
                        FJ(J1)=FJ(J1)+PTOT(LL)*W(KK+K)
   90                SUMOFF=SUMOFF+PTOT(J1)*W(KK+K)
                     J1=J1+1
                     K=K+1
                     FJ(J1)=FJ(J1)+PTOT(LL)*W(KK+K)
  100             SUMDIA=SUMDIA+PTOT(J1)*W(KK+K)
                  FJ(LL)=FJ(LL)+SUMOFF*2.0D+00+SUMDIA
C
C  EXCHANGE TERMS
C
C
C  EXTRACT INTERSECTION OF ATOMS II AND JJ IN THE SPIN DENSITY MATRIX
C
                  K=0
                  DO 120 I=IA,IB
                     I1=IFACT(I)+JA
                     SUM=0.0D+00
                     DO 110 J=IA,IB
                        K=K+1
                        J1=IFACT(J)+JA
  110                SUM=SUM+P(J1)*W(KK+JINDEX(K))
  120             FK(I1)=FK(I1)+HFCO*SUM
                  KK=KK+10
               ELSE IF(JB-JA.GE.3.AND.IA.EQ.IB)THEN
C
C                         HEAVY-ATOM - LIGHT-ATOM
C
C
C   COULOMB TERMS
C
                  SUMDIA=0.0D+00
                  SUMOFF=0.0D+00
                  LL=I1FACT(IA)
                  K=0
                  DO 140 I=0,3
                     J1=IFACT(JA+I)+JA-1
                     DO 130 J=0,I-1
                        K=K+1
                        J1=J1+1
                        FJ(J1)=FJ(J1)+PTOT(LL)*W(KK+K)
  130                SUMOFF=SUMOFF+PTOT(J1)*W(KK+K)
                     J1=J1+1
                     K=K+1
                     FJ(J1)=FJ(J1)+PTOT(LL)*W(KK+K)
  140             SUMDIA=SUMDIA+PTOT(J1)*W(KK+K)
                  FJ(LL)=FJ(LL)+SUMOFF*2.0D+00+SUMDIA
C
C  EXCHANGE TERMS
C
C
C  EXTRACT INTERSECTION OF ATOMS II AND JJ IN THE SPIN DENSITY MATRIX
C
                  K=IFACT(IA)+JA
                  J=0
                  DO 160 I=K,K+3
                     SUM=0.0D+00
                     DO 150 L=K,K+3
                        J=J+1
  150                SUM=SUM+P(L)*W(KK+JINDEX(J))
  160             FK(I)=FK(I)+HFCO*SUM
                  KK=KK+10
               ELSE IF(JB.EQ.JA.AND.IA.EQ.IB)THEN
C
C                         LIGHT-ATOM - LIGHT-ATOM
C
                  I1=I1FACT(IA)
                  J1=I1FACT(JA)
                  IJ=I1+JA-IA
                  FJ(I1)=FJ(I1)+PTOT(J1)*W(KK+1)
                  FJ(J1)=FJ(J1)+PTOT(I1)*W(KK+1)
                  FK(IJ)=FK(IJ)+HFCO*P(IJ)*W(KK+1)
                  KK=KK+1
               END IF
            ELSE
C---               DO 170 I=IA,IB
C---                  KA=IFACT(I)
C---                  DO 170 J=IA,I
C---                     KB=IFACT(J)
C---                     IJ=KA+J
C---                     AA=2.0D+00
C---                     IF (I.EQ.J) AA=1.0D+00
C---                     DO 170 K=JA,JC
C---                        KC=IFACT(K)
C---                        IF(I.GE.K) THEN
C---                           IK=KA+K
C---                        ELSE
C---                           IK=0
C---                        END IF
C---                        IF(J.GE.K) THEN
C---                           JK=KB+K
C---                        ELSE
C---                           JK=0
C---                        END IF
C---                        DO 170 L=JA,K
C---                           IF(I.GE.L) THEN
C---                              IL=KA+L
C---                           ELSE
C---                              IL=0
C---                           END IF
C---                           IF(J.GE.L) THEN
C---                              JL=KB+L
C---                           ELSE
C---                              JL=0
C---                           END IF
C---                           KL=KC+L
C---                           BB=2.0D+00
C---                           IF (K.EQ.L) BB=1.0D+00
C---                           KK=KK+1
C---                           AJ=WJ(KK)
C---                           AK=WK(KK)
C---C
C---C  A  IS THE REPULSION INTEGRAL (I,J/K,L) WHERE ORBITALS I AND J ARE
C---C  ON ATOM II, AND ORBITALS K AND L ARE ON ATOM JJ.
C---C  AA AND BB ARE CORRECTION FACTORS SINCE
C---C  (I,J/K,L)=(J,I/K,L)=(I,J/L,K)=(J,I/L,K)
C---C  IJ IS THE LOCATION OF THE MATRIX ELEMENTS BETWEEN ATOMIC ORBITALS
C---C  I AND J.  SIMILARLY FOR IK ETC.
C---C
C---C THIS FORMS THE TWO-ELECTRON TWO-CENTER REPULSION PART OF THE FOCK
C---C MATRIX.  THE CODE HERE IS HARD TO FOLLOW,
C---C AND IMPOSSIBLE TO MODIFY!,
C---C BUT IT WORKS,
C---                           IF(KL.LE.IJ)THEN
C---                              IF(I.EQ.K.AND.AA+BB.LT.2.1D+00)THEN
C---                                 BB=BB*0.5D+00
C---                                 AA=AA*0.5D+00
C---                                 F(IJ)=F(IJ)+BB*AJ*PTOT(KL)
C---                                 F(KL)=F(KL)+AA*AJ*PTOT(IJ)
C---                              ELSE
C---                                 F(IJ)=F(IJ)+BB*AJ*PTOT(KL)
C---                                 F(KL)=F(KL)+AA*AJ*PTOT(IJ)
C---                                 A=AK*AA*BB*0.25D+00
C---                                 F(IK)=F(IK)-A*P(JL)
C---                                 F(IL)=F(IL)-A*P(JK)
C---                                 F(JK)=F(JK)-A*P(IL)
C---                                 F(JL)=F(JL)-A*P(IK)
C---                              END IF
C---                           END IF
C---  170          CONTINUE
C   THE ARRAY WJ AND WK IS NOT INITIALIZED, AND
C   PROBABLY WE NEVER EXECUTED THIS CODE ABOVE.
                WRITE(6,*) 'NO CODE IN -FOCK2-',JL,JC
                CALL ABRT
            END IF
  180    CONTINUE
  190 CONTINUE
      RETURN
C
C                    START OF MINDO/3 OPTION
C
  200 KR=0
      DO 230 II=1,NUMAT
         IA=NFIRST(II)
         IB=NLAST(II)
         IM1=II-IONE
         DO 220 JJ=1,IM1
            KR=KR+1
C---            IF(LID)THEN
            ELREP=W(KR)
            ELEXC=ELREP
C---            ELSE
C---               ELREP=WJ(KR)
C---               ELEXC=WK(KR)
C---            END IF
            JA=NFIRST(JJ)
            JB=NLAST(JJ)
            DO 210 I=IA,IB
               KA=IFACT(I)
               KK=KA+I
               DO 210 K=JA,JB
                  LL=I1FACT(K)
                  IK=KA+K
                  FJ(KK)=FJ(KK)+PTOT(LL)*ELREP
                  FJ(LL)=FJ(LL)+PTOT(KK)*ELREP
  210       FK(IK)=FK(IK)+HFCO*P(IK)*ELEXC
  220    CONTINUE
  230 CONTINUE
      RETURN
      END
C*MODULE MPCMSC  *DECK JAB
      SUBROUTINE JAB(IA,JA,PJA,PJB,W,F)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PJA(16), PJB(16), W(*), F(*), SUMA(10), SUMB(10)
      SUMA( 1)=
     1+PJA( 1)*W(  1)+PJA( 2)*W( 11)+PJA( 3)*W( 31)+PJA( 4)*W( 61)
     2+PJA( 5)*W( 11)+PJA( 6)*W( 21)+PJA( 7)*W( 41)+PJA( 8)*W( 71)
     3+PJA( 9)*W( 31)+PJA(10)*W( 41)+PJA(11)*W( 51)+PJA(12)*W( 81)
     4+PJA(13)*W( 61)+PJA(14)*W( 71)+PJA(15)*W( 81)+PJA(16)*W( 91)
      SUMA( 2)=
     1+PJA( 1)*W(  2)+PJA( 2)*W( 12)+PJA( 3)*W( 32)+PJA( 4)*W( 62)
     2+PJA( 5)*W( 12)+PJA( 6)*W( 22)+PJA( 7)*W( 42)+PJA( 8)*W( 72)
     3+PJA( 9)*W( 32)+PJA(10)*W( 42)+PJA(11)*W( 52)+PJA(12)*W( 82)
     4+PJA(13)*W( 62)+PJA(14)*W( 72)+PJA(15)*W( 82)+PJA(16)*W( 92)
      SUMA( 3)=
     1+PJA( 1)*W(  3)+PJA( 2)*W( 13)+PJA( 3)*W( 33)+PJA( 4)*W( 63)
     2+PJA( 5)*W( 13)+PJA( 6)*W( 23)+PJA( 7)*W( 43)+PJA( 8)*W( 73)
     3+PJA( 9)*W( 33)+PJA(10)*W( 43)+PJA(11)*W( 53)+PJA(12)*W( 83)
     4+PJA(13)*W( 63)+PJA(14)*W( 73)+PJA(15)*W( 83)+PJA(16)*W( 93)
      SUMA( 4)=
     1+PJA( 1)*W(  4)+PJA( 2)*W( 14)+PJA( 3)*W( 34)+PJA( 4)*W( 64)
     2+PJA( 5)*W( 14)+PJA( 6)*W( 24)+PJA( 7)*W( 44)+PJA( 8)*W( 74)
     3+PJA( 9)*W( 34)+PJA(10)*W( 44)+PJA(11)*W( 54)+PJA(12)*W( 84)
     4+PJA(13)*W( 64)+PJA(14)*W( 74)+PJA(15)*W( 84)+PJA(16)*W( 94)
      SUMA( 5)=
     1+PJA( 1)*W(  5)+PJA( 2)*W( 15)+PJA( 3)*W( 35)+PJA( 4)*W( 65)
     2+PJA( 5)*W( 15)+PJA( 6)*W( 25)+PJA( 7)*W( 45)+PJA( 8)*W( 75)
     3+PJA( 9)*W( 35)+PJA(10)*W( 45)+PJA(11)*W( 55)+PJA(12)*W( 85)
     4+PJA(13)*W( 65)+PJA(14)*W( 75)+PJA(15)*W( 85)+PJA(16)*W( 95)
      SUMA( 6)=
     1+PJA( 1)*W(  6)+PJA( 2)*W( 16)+PJA( 3)*W( 36)+PJA( 4)*W( 66)
     2+PJA( 5)*W( 16)+PJA( 6)*W( 26)+PJA( 7)*W( 46)+PJA( 8)*W( 76)
     3+PJA( 9)*W( 36)+PJA(10)*W( 46)+PJA(11)*W( 56)+PJA(12)*W( 86)
     4+PJA(13)*W( 66)+PJA(14)*W( 76)+PJA(15)*W( 86)+PJA(16)*W( 96)
      SUMA( 7)=
     1+PJA( 1)*W(  7)+PJA( 2)*W( 17)+PJA( 3)*W( 37)+PJA( 4)*W( 67)
     2+PJA( 5)*W( 17)+PJA( 6)*W( 27)+PJA( 7)*W( 47)+PJA( 8)*W( 77)
     3+PJA( 9)*W( 37)+PJA(10)*W( 47)+PJA(11)*W( 57)+PJA(12)*W( 87)
     4+PJA(13)*W( 67)+PJA(14)*W( 77)+PJA(15)*W( 87)+PJA(16)*W( 97)
      SUMA( 8)=
     1+PJA( 1)*W(  8)+PJA( 2)*W( 18)+PJA( 3)*W( 38)+PJA( 4)*W( 68)
     2+PJA( 5)*W( 18)+PJA( 6)*W( 28)+PJA( 7)*W( 48)+PJA( 8)*W( 78)
     3+PJA( 9)*W( 38)+PJA(10)*W( 48)+PJA(11)*W( 58)+PJA(12)*W( 88)
     4+PJA(13)*W( 68)+PJA(14)*W( 78)+PJA(15)*W( 88)+PJA(16)*W( 98)
      SUMA( 9)=
     1+PJA( 1)*W(  9)+PJA( 2)*W( 19)+PJA( 3)*W( 39)+PJA( 4)*W( 69)
     2+PJA( 5)*W( 19)+PJA( 6)*W( 29)+PJA( 7)*W( 49)+PJA( 8)*W( 79)
     3+PJA( 9)*W( 39)+PJA(10)*W( 49)+PJA(11)*W( 59)+PJA(12)*W( 89)
     4+PJA(13)*W( 69)+PJA(14)*W( 79)+PJA(15)*W( 89)+PJA(16)*W( 99)
      SUMA(10)=
     1+PJA( 1)*W( 10)+PJA( 2)*W( 20)+PJA( 3)*W( 40)+PJA( 4)*W( 70)
     2+PJA( 5)*W( 20)+PJA( 6)*W( 30)+PJA( 7)*W( 50)+PJA( 8)*W( 80)
     3+PJA( 9)*W( 40)+PJA(10)*W( 50)+PJA(11)*W( 60)+PJA(12)*W( 90)
     4+PJA(13)*W( 70)+PJA(14)*W( 80)+PJA(15)*W( 90)+PJA(16)*W(100)
      SUMB( 1)=
     1+PJB( 1)*W(  1)+PJB( 2)*W(  2)+PJB( 3)*W(  4)+PJB( 4)*W(  7)
     2+PJB( 5)*W(  2)+PJB( 6)*W(  3)+PJB( 7)*W(  5)+PJB( 8)*W(  8)
     3+PJB( 9)*W(  4)+PJB(10)*W(  5)+PJB(11)*W(  6)+PJB(12)*W(  9)
     4+PJB(13)*W(  7)+PJB(14)*W(  8)+PJB(15)*W(  9)+PJB(16)*W( 10)
      SUMB( 2)=
     1+PJB( 1)*W( 11)+PJB( 2)*W( 12)+PJB( 3)*W( 14)+PJB( 4)*W( 17)
     2+PJB( 5)*W( 12)+PJB( 6)*W( 13)+PJB( 7)*W( 15)+PJB( 8)*W( 18)
     3+PJB( 9)*W( 14)+PJB(10)*W( 15)+PJB(11)*W( 16)+PJB(12)*W( 19)
     4+PJB(13)*W( 17)+PJB(14)*W( 18)+PJB(15)*W( 19)+PJB(16)*W( 20)
      SUMB( 3)=
     1+PJB( 1)*W( 21)+PJB( 2)*W( 22)+PJB( 3)*W( 24)+PJB( 4)*W( 27)
     2+PJB( 5)*W( 22)+PJB( 6)*W( 23)+PJB( 7)*W( 25)+PJB( 8)*W( 28)
     3+PJB( 9)*W( 24)+PJB(10)*W( 25)+PJB(11)*W( 26)+PJB(12)*W( 29)
     4+PJB(13)*W( 27)+PJB(14)*W( 28)+PJB(15)*W( 29)+PJB(16)*W( 30)
      SUMB( 4)=
     1+PJB( 1)*W( 31)+PJB( 2)*W( 32)+PJB( 3)*W( 34)+PJB( 4)*W( 37)
     2+PJB( 5)*W( 32)+PJB( 6)*W( 33)+PJB( 7)*W( 35)+PJB( 8)*W( 38)
     3+PJB( 9)*W( 34)+PJB(10)*W( 35)+PJB(11)*W( 36)+PJB(12)*W( 39)
     4+PJB(13)*W( 37)+PJB(14)*W( 38)+PJB(15)*W( 39)+PJB(16)*W( 40)
      SUMB( 5)=
     1+PJB( 1)*W( 41)+PJB( 2)*W( 42)+PJB( 3)*W( 44)+PJB( 4)*W( 47)
     2+PJB( 5)*W( 42)+PJB( 6)*W( 43)+PJB( 7)*W( 45)+PJB( 8)*W( 48)
     3+PJB( 9)*W( 44)+PJB(10)*W( 45)+PJB(11)*W( 46)+PJB(12)*W( 49)
     4+PJB(13)*W( 47)+PJB(14)*W( 48)+PJB(15)*W( 49)+PJB(16)*W( 50)
      SUMB( 6)=
     1+PJB( 1)*W( 51)+PJB( 2)*W( 52)+PJB( 3)*W( 54)+PJB( 4)*W( 57)
     2+PJB( 5)*W( 52)+PJB( 6)*W( 53)+PJB( 7)*W( 55)+PJB( 8)*W( 58)
     3+PJB( 9)*W( 54)+PJB(10)*W( 55)+PJB(11)*W( 56)+PJB(12)*W( 59)
     4+PJB(13)*W( 57)+PJB(14)*W( 58)+PJB(15)*W( 59)+PJB(16)*W( 60)
      SUMB( 7)=
     1+PJB( 1)*W( 61)+PJB( 2)*W( 62)+PJB( 3)*W( 64)+PJB( 4)*W( 67)
     2+PJB( 5)*W( 62)+PJB( 6)*W( 63)+PJB( 7)*W( 65)+PJB( 8)*W( 68)
     3+PJB( 9)*W( 64)+PJB(10)*W( 65)+PJB(11)*W( 66)+PJB(12)*W( 69)
     4+PJB(13)*W( 67)+PJB(14)*W( 68)+PJB(15)*W( 69)+PJB(16)*W( 70)
      SUMB( 8)=
     1+PJB( 1)*W( 71)+PJB( 2)*W( 72)+PJB( 3)*W( 74)+PJB( 4)*W( 77)
     2+PJB( 5)*W( 72)+PJB( 6)*W( 73)+PJB( 7)*W( 75)+PJB( 8)*W( 78)
     3+PJB( 9)*W( 74)+PJB(10)*W( 75)+PJB(11)*W( 76)+PJB(12)*W( 79)
     4+PJB(13)*W( 77)+PJB(14)*W( 78)+PJB(15)*W( 79)+PJB(16)*W( 80)
      SUMB( 9)=
     1+PJB( 1)*W( 81)+PJB( 2)*W( 82)+PJB( 3)*W( 84)+PJB( 4)*W( 87)
     2+PJB( 5)*W( 82)+PJB( 6)*W( 83)+PJB( 7)*W( 85)+PJB( 8)*W( 88)
     3+PJB( 9)*W( 84)+PJB(10)*W( 85)+PJB(11)*W( 86)+PJB(12)*W( 89)
     4+PJB(13)*W( 87)+PJB(14)*W( 88)+PJB(15)*W( 89)+PJB(16)*W( 90)
      SUMB(10)=
     1+PJB( 1)*W( 91)+PJB( 2)*W( 92)+PJB( 3)*W( 94)+PJB( 4)*W( 97)
     2+PJB( 5)*W( 92)+PJB( 6)*W( 93)+PJB( 7)*W( 95)+PJB( 8)*W( 98)
     3+PJB( 9)*W( 94)+PJB(10)*W( 95)+PJB(11)*W( 96)+PJB(12)*W( 99)
     4+PJB(13)*W( 97)+PJB(14)*W( 98)+PJB(15)*W( 99)+PJB(16)*W(100)
      I=0
      DO 10 I5=1,4
         IIA=IA+I5-1
         IJA=JA+I5-1
         IOFF=(IIA*(IIA-1))/2+IA-1
         JOFF=(IJA*(IJA-1))/2+JA-1
         DO 10 I6=1,I5
            IOFF=IOFF+1
            JOFF=JOFF+1
            I=I+1
            F(IOFF)=F(IOFF)+SUMB(I)
   10 F(JOFF)=F(JOFF)+SUMA(I)
      RETURN
      END
C*MODULE MPCMSC  *DECK KAB
      SUBROUTINE KAB(IA,JA,PK,W,F,HFCO)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PK(*), W(*), F(*), SUM(16)
C
      SUM( 1)=
     1+PK( 1)*W(  1)+PK( 2)*W(  2)+PK( 3)*W(  4)+PK( 4)*W(  7)
     2+PK( 5)*W( 11)+PK( 6)*W( 12)+PK( 7)*W( 14)+PK( 8)*W( 17)
     3+PK( 9)*W( 31)+PK(10)*W( 32)+PK(11)*W( 34)+PK(12)*W( 37)
     4+PK(13)*W( 61)+PK(14)*W( 62)+PK(15)*W( 64)+PK(16)*W( 67)
      SUM( 2)=
     1+PK( 1)*W(  2)+PK( 2)*W(  3)+PK( 3)*W(  5)+PK( 4)*W(  8)
     2+PK( 5)*W( 12)+PK( 6)*W( 13)+PK( 7)*W( 15)+PK( 8)*W( 18)
     3+PK( 9)*W( 32)+PK(10)*W( 33)+PK(11)*W( 35)+PK(12)*W( 38)
     4+PK(13)*W( 62)+PK(14)*W( 63)+PK(15)*W( 65)+PK(16)*W( 68)
      SUM( 3)=
     1+PK( 1)*W(  4)+PK( 2)*W(  5)+PK( 3)*W(  6)+PK( 4)*W(  9)
     2+PK( 5)*W( 14)+PK( 6)*W( 15)+PK( 7)*W( 16)+PK( 8)*W( 19)
     3+PK( 9)*W( 34)+PK(10)*W( 35)+PK(11)*W( 36)+PK(12)*W( 39)
     4+PK(13)*W( 64)+PK(14)*W( 65)+PK(15)*W( 66)+PK(16)*W( 69)
      SUM( 4)=
     1+PK( 1)*W(  7)+PK( 2)*W(  8)+PK( 3)*W(  9)+PK( 4)*W( 10)
     2+PK( 5)*W( 17)+PK( 6)*W( 18)+PK( 7)*W( 19)+PK( 8)*W( 20)
     3+PK( 9)*W( 37)+PK(10)*W( 38)+PK(11)*W( 39)+PK(12)*W( 40)
     4+PK(13)*W( 67)+PK(14)*W( 68)+PK(15)*W( 69)+PK(16)*W( 70)
      SUM( 5)=
     1+PK( 1)*W( 11)+PK( 2)*W( 12)+PK( 3)*W( 14)+PK( 4)*W( 17)
     2+PK( 5)*W( 21)+PK( 6)*W( 22)+PK( 7)*W( 24)+PK( 8)*W( 27)
     3+PK( 9)*W( 41)+PK(10)*W( 42)+PK(11)*W( 44)+PK(12)*W( 47)
     4+PK(13)*W( 71)+PK(14)*W( 72)+PK(15)*W( 74)+PK(16)*W( 77)
      SUM( 6)=
     1+PK( 1)*W( 12)+PK( 2)*W( 13)+PK( 3)*W( 15)+PK( 4)*W( 18)
     2+PK( 5)*W( 22)+PK( 6)*W( 23)+PK( 7)*W( 25)+PK( 8)*W( 28)
     3+PK( 9)*W( 42)+PK(10)*W( 43)+PK(11)*W( 45)+PK(12)*W( 48)
     4+PK(13)*W( 72)+PK(14)*W( 73)+PK(15)*W( 75)+PK(16)*W( 78)
      SUM( 7)=
     1+PK( 1)*W( 14)+PK( 2)*W( 15)+PK( 3)*W( 16)+PK( 4)*W( 19)
     2+PK( 5)*W( 24)+PK( 6)*W( 25)+PK( 7)*W( 26)+PK( 8)*W( 29)
     3+PK( 9)*W( 44)+PK(10)*W( 45)+PK(11)*W( 46)+PK(12)*W( 49)
     4+PK(13)*W( 74)+PK(14)*W( 75)+PK(15)*W( 76)+PK(16)*W( 79)
      SUM( 8)=
     1+PK( 1)*W( 17)+PK( 2)*W( 18)+PK( 3)*W( 19)+PK( 4)*W( 20)
     2+PK( 5)*W( 27)+PK( 6)*W( 28)+PK( 7)*W( 29)+PK( 8)*W( 30)
     3+PK( 9)*W( 47)+PK(10)*W( 48)+PK(11)*W( 49)+PK(12)*W( 50)
     4+PK(13)*W( 77)+PK(14)*W( 78)+PK(15)*W( 79)+PK(16)*W( 80)
      SUM( 9)=
     1+PK( 1)*W( 31)+PK( 2)*W( 32)+PK( 3)*W( 34)+PK( 4)*W( 37)
     2+PK( 5)*W( 41)+PK( 6)*W( 42)+PK( 7)*W( 44)+PK( 8)*W( 47)
     3+PK( 9)*W( 51)+PK(10)*W( 52)+PK(11)*W( 54)+PK(12)*W( 57)
     4+PK(13)*W( 81)+PK(14)*W( 82)+PK(15)*W( 84)+PK(16)*W( 87)
      SUM(10)=
     1+PK( 1)*W( 32)+PK( 2)*W( 33)+PK( 3)*W( 35)+PK( 4)*W( 38)
     2+PK( 5)*W( 42)+PK( 6)*W( 43)+PK( 7)*W( 45)+PK( 8)*W( 48)
     3+PK( 9)*W( 52)+PK(10)*W( 53)+PK(11)*W( 55)+PK(12)*W( 58)
     4+PK(13)*W( 82)+PK(14)*W( 83)+PK(15)*W( 85)+PK(16)*W( 88)
      SUM(11)=
     1+PK( 1)*W( 34)+PK( 2)*W( 35)+PK( 3)*W( 36)+PK( 4)*W( 39)
     2+PK( 5)*W( 44)+PK( 6)*W( 45)+PK( 7)*W( 46)+PK( 8)*W( 49)
     3+PK( 9)*W( 54)+PK(10)*W( 55)+PK(11)*W( 56)+PK(12)*W( 59)
     4+PK(13)*W( 84)+PK(14)*W( 85)+PK(15)*W( 86)+PK(16)*W( 89)
      SUM(12)=
     1+PK( 1)*W( 37)+PK( 2)*W( 38)+PK( 3)*W( 39)+PK( 4)*W( 40)
     2+PK( 5)*W( 47)+PK( 6)*W( 48)+PK( 7)*W( 49)+PK( 8)*W( 50)
     3+PK( 9)*W( 57)+PK(10)*W( 58)+PK(11)*W( 59)+PK(12)*W( 60)
     4+PK(13)*W( 87)+PK(14)*W( 88)+PK(15)*W( 89)+PK(16)*W( 90)
      SUM(13)=
     1+PK( 1)*W( 61)+PK( 2)*W( 62)+PK( 3)*W( 64)+PK( 4)*W( 67)
     2+PK( 5)*W( 71)+PK( 6)*W( 72)+PK( 7)*W( 74)+PK( 8)*W( 77)
     3+PK( 9)*W( 81)+PK(10)*W( 82)+PK(11)*W( 84)+PK(12)*W( 87)
     4+PK(13)*W( 91)+PK(14)*W( 92)+PK(15)*W( 94)+PK(16)*W( 97)
      SUM(14)=
     1+PK( 1)*W( 62)+PK( 2)*W( 63)+PK( 3)*W( 65)+PK( 4)*W( 68)
     2+PK( 5)*W( 72)+PK( 6)*W( 73)+PK( 7)*W( 75)+PK( 8)*W( 78)
     3+PK( 9)*W( 82)+PK(10)*W( 83)+PK(11)*W( 85)+PK(12)*W( 88)
     4+PK(13)*W( 92)+PK(14)*W( 93)+PK(15)*W( 95)+PK(16)*W( 98)
      SUM(15)=
     1+PK( 1)*W( 64)+PK( 2)*W( 65)+PK( 3)*W( 66)+PK( 4)*W( 69)
     2+PK( 5)*W( 74)+PK( 6)*W( 75)+PK( 7)*W( 76)+PK( 8)*W( 79)
     3+PK( 9)*W( 84)+PK(10)*W( 85)+PK(11)*W( 86)+PK(12)*W( 89)
     4+PK(13)*W( 94)+PK(14)*W( 95)+PK(15)*W( 96)+PK(16)*W( 99)
      SUM(16)=
     1+PK( 1)*W( 67)+PK( 2)*W( 68)+PK( 3)*W( 69)+PK( 4)*W( 70)
     2+PK( 5)*W( 77)+PK( 6)*W( 78)+PK( 7)*W( 79)+PK( 8)*W( 80)
     3+PK( 9)*W( 87)+PK(10)*W( 88)+PK(11)*W( 89)+PK(12)*W( 90)
     4+PK(13)*W( 97)+PK(14)*W( 98)+PK(15)*W( 99)+PK(16)*W(100)
      IF(IA.GT.JA) THEN
         M=0
         DO 10 J1=IA,IA+3
            J=(J1*(J1-1))/2
            DO 10 J2=JA,JA+3
               M=M+1
               J3=J+J2
   10    F(J3)=F(J3)+HFCO*SUM(M)
      ELSE
C
C   IA IS LESS THAN JA, THEREFORE USE OTHER HALF OF TRIANGLE
C
         M=0
         DO 20 J1=IA,IA+3
            DO 20 J2=JA,JA+3
               M=M+1
               J3=(J2*(J2-1))/2+J1
   20    F(J3)=F(J3)+HFCO*SUM(M)
      END IF
      RETURN
      END
C*MODULE MPCMSC  *DECK MPCG
      SUBROUTINE MPCG(FJ,FK,P,PA,PB,PTOT2,W,L2,HFCO)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      LOGICAL OUT,GOPARR,DSKWRK,MASWRK,ROUHF
C
      DIMENSION FJ(L2),FK(L2),P(L2),PA(L2),PB(L2),W(N2EL),PTOT2(NAT,16)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MOLKST/ NUMAT,MNAT(MXATM),NFIRST(MXATM),NMIDLE(MXATM),
     *                NLAST(MXATM),NORBS,NELECS,NALPHA,NBETA,NCLOSE,
     *                NOPEN,NDUMY,FRACT
      COMMON /N2ELCT/ N2EL
      COMMON /NUMCAL/ NUMCAL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      PARAMETER (ONE=1.0D+00, TOEV=27.211652D+00, TOHART=ONE/TOEV)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DEBUG /8HDEBUG   /
#else
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      DATA DEBUG_STR/"DEBUG   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA UHF,ROHF/8HUHF     ,8HROHF    /
#else
      CHARACTER*8 :: UHF_STR
      EQUIVALENCE (UHF, UHF_STR)
      CHARACTER*8 :: ROHF_STR
      EQUIVALENCE (ROHF, ROHF_STR)
      DATA UHF_STR,ROHF_STR/"UHF     ","ROHF    "/
#endif
C
C MPCG CALCULATES FOCK MATRICES AS FOLLOWS:
C RHF: FJ AND FK ARE THE SAME MATRIX F
C ROHF/UHF: FJ IS FA (ALPHA) AND FK IS FB (BETA)
C GVB: "CLOSED" SHELL- FJ AND FK ARE THE SAME MATRIX F
C      "OPEN" SHELLS: FJ IS THE "J" PART OF F AND
C                     FK IS THE "K" PART OF F AND
C      F=J+K, J INCLUDING COULOMB INTERGRALS AND K- EXCHANGE.
C PA AND PB ARE USED ONLY WITH ROHF/UHF.
C HFCO IS (P IS THE TOTAL DENSITY):
C RHF: (-1/2) (F= SUM {P(JINT+HFCO*KINT))}
C ROHF/UHF: (-1): FA=SUM {P*JINT+PA*HFCO*KINT))},
C                 FB=SUM {P*JINT+PB*HFCO*KINT))}
C GVB: "CLOSED" SHELL AS RHF
C      OPEN SHELLS: (1): FJ=SUM {P*JINT}, FK=HFCO*SUM {P*KINT}
C
      NUMCAL = 1
C
      OUT = NPRINT.EQ.5 .OR. EXETYP.EQ. DEBUG .AND. MASWRK
      ROUHF=SCFTYP.EQ.UHF.OR.SCFTYP.EQ.ROHF
C
      CALL VCLR(FJ,1,L2)
      IF(HFCO.EQ.ONE.OR.ROUHF) CALL VCLR(FK,1,L2)
C
C     ----- CALCULATE THE 2-ELECTRON PART OF THE FOCK MATRIX -----
C
      IF(ROUHF) THEN
         CALL VADD(PA,1,PB,1,P,1,L2)
         CALL FOCK2(FJ,FJ,P,PA,PTOT2,W,NUMAT,NFIRST,NMIDLE,NLAST,HFCO)
         CALL FOCK1(FJ,FJ,P,PA,HFCO)
         CALL FOCK2(FK,FK,P,PB,PTOT2,W,NUMAT,NFIRST,NMIDLE,NLAST,HFCO)
         CALL FOCK1(FK,FK,P,PB,HFCO)
      ELSE
         CALL FOCK2(FJ,FK,P,P,PTOT2,W,NUMAT,NFIRST,NMIDLE,NLAST,HFCO)
         CALL FOCK1(FJ,FK,P,P,HFCO)
      END IF
C
C     ----- CONVERT BACK TO A.U.'S -----
C
      CALL DSCAL(L2,TOHART,FJ,1)
      IF(HFCO.EQ.ONE.OR.ROUHF) CALL DSCAL(L2,TOHART,FK,1)
C
C     ----- OPTIONAL PRINTOUT -----
C
      IF(OUT) THEN
         IF(ROUHF) THEN
            WRITE(IW,9040) 'ALPHA'
         ELSE
            IF(HFCO.EQ.ONE) THEN
               WRITE(IW,9040) 'EXCH.'
            ELSE
               WRITE(IW,9040) ' '
            ENDIF
         ENDIF
         CALL PRTRIL(FJ,NUM)
         IF(ROUHF) THEN
            WRITE(IW,9040) 'BETA'
            CALL PRTRIL(FK,NUM)
         ELSE
            IF(HFCO.EQ.ONE) THEN
               WRITE(IW,9040) 'COUL.'
               CALL PRTRIL(FK,NUM)
            ENDIF
         END IF
      END IF
      RETURN
C
 9040 FORMAT(/10X,A5,' TWO-ELECTRON PART OF FOCK MATRIX (MPCG)')
      END
C*MODULE MPCMSC  *DECK MPCPRP
      SUBROUTINE MPCPRP(RHO,PRNT)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION MAKEFP
C
      LOGICAL RHO,PRNT,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500, MXAO=2047)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      CHARACTER*4 ELEMNT(106)
C
      COMMON /CORE  / CORE(107)
      COMMON /DIPSTO/ UX,UY,UZ,Q2(MXATM)
      COMMON /EFPPRT/ NOPRT(MXATM),MIDPRT(MXATM),NUMFFD(MXATM),
     *                INOPRT(2*MXATM),LSTGRP(20,MXATM+1),
     *                ISUM(20,MXFGPT),GRPSUM(20,4),NMDFFD
      COMMON /FMCOM / X(1)
      COMMON /MASSES/ ZMASS(MXATM)
      COMMON /MPCGEO/ GEO(3,MXATM)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MOLKST/ NUMAT,INAT(MXATM),NFIRST(MXATM),NMIDLE(MXATM),
     1                NLAST(MXATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNLAB/ TITLE(10),ANAM(MXATM),BNAM(MXATM),BFLAB(MXAO)
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
      COMMON /XYZPRP/ XP,YP,ZP
     *               ,DMX,DMY,DMZ
     *               ,QXX,QYY,QZZ,QXY,QXZ,QYZ
     *               ,QMXX,QMYY,QMZZ,QMXY,QMXZ,QMYZ
     *               ,OXXX,OXXY,OXXZ,OXYY,OYYY,OYYZ
     *               ,OXZZ,OYZZ,OZZZ,OXYZ
     *               ,OMXXX,OMXXY,OMXXZ,OMXYY,OMYYY
     *               ,OMYYZ,OMXZZ,OMYZZ,OMZZZ,OMXYZ
C
      DIMENSION Q(MXATM)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RHF,CHECK/8HRHF     ,8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      CHARACTER*8 :: RHF_STR
      EQUIVALENCE (RHF, RHF_STR)
      DATA RHF_STR,CHECK_STR/"RHF     ","CHECK   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA MAKEFP/8HMAKEFP  /
#else
      CHARACTER*8 :: MAKEFP_STR
      EQUIVALENCE (MAKEFP, MAKEFP_STR)
      DATA MAKEFP_STR/"MAKEFP  "/
#endif
C
C     ----- MOPAC PROPERTIES -----
C
      IF(.NOT.RHO) RETURN
C
C     ----- ALLOCATE MEMORY FOR DENSITY MATRICIES -----
C
      L1 = NUM
      L2 = (L1*L1+L1)/2
      CALL VALFM(LOADFM)
      LP   = LOADFM + 1
      LPB  = LP     + L2
      LAST = LPB    + L2
      NEED = LAST - LOADFM - 1
      CALL GETFM(NEED)
      IF(EXETYP.EQ.CHECK)THEN
        CALL RETFM(NEED)
        RETURN
      END IF
C
      CALL WFNDEN(X(LP),X(LPB),L2)
      IF(SCFTYP.NE.RHF) CALL VADD(X(LP),1,X(LPB),1,X(LP),1,L2)
C
C     ----- FILL ELEMNT ARRAY -----
C
      CALL SETLAB(3,ELEMNT)
C
C     ----- CALCULATE ZDO-CHARGES AND PRINT -----
C
      CALL CHRGE(X(LP),Q)
C
C HUI LI
C     ----- USE THESE CHARGE TO GENERATE EFP FORMATED MONOPOLES
C
      CALL PREFIN
C
C         DETERMINE WHICH ATOM-POINTS SHOULD NOT BE PRINTED OUT
C         ACCORDING TO THE INFORMATION FROM NOPRT(I)
C
         DO IAT = 1, MXATM
           IF(NOPRT(IAT).GT.0) INOPRT(NOPRT(IAT))=1
         ENDDO
C
C
C HUI LI  FIND THE ATOM-POINTS WHOSE CHARGES(MONOPOLE)
C         WILL BE COLLECTED AND ADDED TO THE ATOM
C         DEFINED IN LSTGRP(*,MXATM+1)
C
        DO LATM=1,NAT
          IF(NMDFFD.GT.0) THEN
            DO NMD = 1, NMDFFD
                DO K = 1, LSTGRP(NMD,2)
                  IF(LSTGRP(NMD,K+2).EQ.LATM)
     *                 ISUM(NMD,LATM)=1
                ENDDO
                IF(LSTGRP(NMD,1).EQ.LATM)
     *                LSTGRP(NMD,MXATM+1)=LATM
            ENDDO
          END IF
        ENDDO
C
C
C HUI LI   MODIFY THE CHARGE AT THE ATOM
C          DEFINED IN LSTGRP(*,MXATM+1) SO THE
C          EFP HAS AN INTEGER CHARGE
C
      IF(NMDFFD.GT.0) THEN
        DO NMD = 1, NMDFFD
          GRPSUM(NMD,1)=0.0D+00
          DO I = 1, NAT
            IF(ISUM(NMD,I).GT.0) THEN
              GRPSUM(NMD,1)=GRPSUM(NMD,1)+CORE(INAT(I))-Q(I)
            END IF
          ENDDO
          Q(LSTGRP(NMD,MXATM+1)) =
     *           Q(LSTGRP(NMD,MXATM+1)) - GRPSUM(NMD,1)
          IF(MASWRK)WRITE(IW,*)' '
          IF(MASWRK)WRITE(IW,'(A/,1X,A/,A,2X,F14.10/,A)')
     *' EFP/FORCE FIELD CONNECTION ATOM  ',
     * ANAM(LSTGRP(NMD,MXATM+1)),
     *' COLLECTED A MONOPOLE :',GRPSUM(NMD,1),
     *' FROM ATOMS:'
C
          DO I = 1, NAT
            IF(ISUM(NMD,I).GT.0  .AND. MASWRK)
     *        WRITE(IW,'(1X,A)') ANAM(I)
          ENDDO
          IF(MASWRK)WRITE(IW,*)' '
        ENDDO
      END IF
C
C HUI LI  PRINT 'NOPRINT' MESSAGE
      DO I=1,NAT
        IF(INOPRT(I).GT.0 .AND. MASWRK)WRITE(IW,'(A,1X,A,1X,A)')
     *     ' ATOM',ANAM(I),' WILL NOT BE PRINTED/PUNCHED OUT'
      ENDDO
C
C        PUNCH OUT CHARGES AS EFP MONOPOLE
C
      IF(RUNTYP.NE.MAKEFP) GO TO 800
C
      IDMP =4
      CALL SEQOPN(IDMP,'IRCDATA','NEW',.FALSE.,'FORMATTED')
C
      WRITE(IP  ,9050)
      WRITE(IDMP,9050)
 9050 FORMAT('EFFECTIVE FRAGMENTS INPUT FILE'/' $FRAGNAME')
      WRITE(IP  ,*) 'TITLE'
      WRITE(IDMP,*) 'TITLE'
      WRITE(IP  ,9201)
      WRITE(IDMP,9201)
9201  FORMAT(' COORDINATES ')
      DO I=1,NAT
C          SKIP PRINT OUT
         IF(INOPRT(I).LT.0)THEN
         WRITE(IP  ,9214)
     *     ANAM(I),(C(J,I),J=1,3),ZMASS(I),ZAN(I)
         WRITE(IDMP,9214)
     *     ANAM(I),(C(J,I),J=1,3),ZMASS(I),ZAN(I)
         END IF
      ENDDO
      WRITE(IP  ,9206)
      WRITE(IDMP,9206)
 9206 FORMAT(' STOP')
      WRITE(IP  ,9207)
      WRITE(IDMP,9207)
 9207 FORMAT(' MONOPOLES ')
      DO I=1,NAT
C
C          SKIP PRINT OUT
         IF(INOPRT(I).LT.0)THEN
         WRITE(IP  ,9209)
     *  ANAM(I),CORE(INAT(I))-Q(I)-ZAN(I),ZAN(I)
         WRITE(IDMP,9209)
     *  ANAM(I),CORE(INAT(I))-Q(I)-ZAN(I),ZAN(I)
         END IF
      ENDDO
 9209 FORMAT(A4,4X,F15.10,F10.5)
      WRITE(IP  ,9206)
      WRITE(IDMP,9206)
 9214 FORMAT(A4,4X,3F15.10,F12.7,F5.1)
C
      CALL SEQREW(IDMP)
C
  800 CONTINUE
      IF(MASWRK .AND. PRNT)THEN
         WRITE(IW,9010)
         WRITE(IW,9020)
         DO 80 I=1,NAT
C HUI LI   SKIP PRINT OUT
         IF(INOPRT(I).LT.0)THEN
            L=INAT(I)
            Q2(I)=CORE(L) - Q(I)
            WRITE(IW,'(I12,9X,A4,4X,F13.4,F16.4)')
     *                      I,ELEMNT(L),Q2(I),Q(I)
         END IF
   80    CONTINUE
      END IF
C
C     ----- CALCULATE ZDO-DIPOLE AND PRINT -----
C
      CALL DIPOLE(DIP,X(LP),GEO,CHARGE)
C
C     ----- DIPOLE CHANGES GEO SO WE REFILL IT -----
C
      CALL RDMOL
C
      IF(MASWRK .AND. PRNT) THEN
         IPOINT=1
         WRITE(IW,9030)
         WRITE(IW,9040) IPOINT,XP,YP,ZP,CHARGE,DMX,DMY,DMZ,DIP
         WRITE(IP,9060) IPOINT,XP,YP,ZP,DMX,DMY,DMZ
      END IF
C
      CALL RETFM(NEED)
C
      IF (MASWRK .AND. PRNT)
     *WRITE(IW,FMT='('' ...... END OF PROPERTY EVALUATION ......'')')
      CALL TIMIT(1)
      RETURN
C
 9010 FORMAT(/20X,13("-"),/20X,"MOPAC CHARGES"/20X,13("-"))
 9020 FORMAT(8X,' ATOM NO.   TYPE          CHARGE        ATOM'
     *     ,'  ELECTRON DENSITY')
 9030 FORMAT(/10X,21('-'),/10X,'ELECTROSTATIC MOMENTS',
     *      /10X,21('-'),/)
 9040 FORMAT(1X,'POINT',I4,11X,'X',11X,'Y',11X,'Z (ANGS)',6X,
     *       'CHARGE'/13X,3F12.6,6X,F6.2,1X,'(A.U.)'/
     *       9X,'DX',10X,'DY',10X,'DZ',9X,'/D/  (DEBYE)'/
     *       1X,4F12.6)
 9060 FORMAT(1X,'MOMENTS AT POINT',I5,' X,Y,Z=',3F10.6/
     *       1X,'DIPOLE     ',3F10.6)
C
      END
