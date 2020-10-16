C 12 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C  3 SEP 03 - MWS - DCART: REMOVE TRANSLATION VECTOR
C 16 JUN 03 - HL  - DCART: INCLUDE MM GRAD. CORR, NEW DIHED
C 25 JUN 01 - MWS - ALTER COMMON BLOCK WFNOPT
C 28 SEP 97 - MWS - CONVERT PARALLEL CALLS FROM TCGMSG TO DDI
C 26 NOV 96 - DGF - ADJUST CALLS OF FOCK2
C 13 JUN 96 - VAG - ADD VARIABLE FOR CI TYPE TO SCFOPT COMMON
C 12 NOV 94 - MWS - REMOVE FTNCHEK WARNINGS
C 27 OCT 94 - MWS - DHC: FIX SCFOPT COMMON
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C 16 MAR 92 - JHJ - NEW MODULE FOR MOPAC GRADIENT COMPUTATION
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
C*MODULE MPCGRD  *DECK ANALYT
      SUBROUTINE ANALYT(PSUM,PALPHA,PBETA,COORD,NAT,JJA,JJD,
     1                  IIA,IID,ENG)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION COORD(3,*),ENG(3), PSUM(*), PALPHA(*), PBETA(*),NAT(*)
C                                                                      *
C         CALCULATION OF ANALYTICAL DERIVATIVES                        *
C                                                                      *
C
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
C COMMON BLOCKS 'OWNED' BY REST OF PROGRAM.
C
      COMMON /CORE  / CORE(107)
      COMMON /BETAS / BETAS(107),BETAP(107),BETAD(107)
      COMMON /ALPHA / ALPA(107)
      COMMON /TWOEL3/ F03(107)
      COMMON /NATORB/ NATORB(107)
      COMMON /ALPHA3/ ALP3(153)
      COMMON /IDEAS / FN1(107,10),FN2(107,10),FN3(107,10)
      COMMON /NATYPE/ NZTYPE(107),MTYPE(30),LTYPE
      COMMON /BETA3 / BETA3(153)
      COMMON /VSIPS / VS(107),VP(107),VD(107)
C
C COMMON BLOCKS 'OWNED' BY ANT
C
      COMMON /DERIVS/ DS(16),DG(22),DR(100),TDX(3),TDY(3),TDZ(3)
      COMMON /EXTRA/  G(22), TXYZ(9)
C
C ON RETURN, ENG HOLDS ANALYTICAL DERIVATIVES
C
      COMMON /FORCE3/ IDMY(5),I3N,IX
      DIMENSION EAA(3),EAB(3),ENUC(3), BI(4), BJ(4)
      SAVE AM1, MINDO3
      LOGICAL AM1, MINDO3
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA KMINDO,KAM1,KPM3/4HMIND,4HAM1 ,4HPM3 /
#else
      CHARACTER*4 :: KPM3_STR
      EQUIVALENCE (KPM3, KPM3_STR)
      CHARACTER*8 :: KMINDO_STR
      EQUIVALENCE (KMINDO, KMINDO_STR)
      CHARACTER*8 :: KAM1_STR
      EQUIVALENCE (KAM1, KAM1_STR)
      DATA KMINDO_STR,KAM1_STR,KPM3_STR/"MIND","AM1 ","PM3 "/
#endif
C
      AM1 = (MPCTYP.EQ.KAM1.OR.MPCTYP.EQ.KPM3)
      MINDO3 = (MPCTYP.EQ.KMINDO)
C
      DR1=0.0D+00
      ALPH1=0.0D+00
      ALPH2=0.0D+00
C
      A0=0.529167D+00
      JD=JJD-JJA+1
      JA=1
      ID=IID-IIA+1+JD
      IA=JD+1
      DO 10 J=1,3
         EAA(J)=0.0D+00
         EAB(J)=0.0D+00
         ENUC(J)=0.0D+00
         ENG(J)=0.0D+00
   10 CONTINUE
      I=2
      NI=NAT(I)
      ISTART=NZTYPE(NI)*4-3
      J=1
      NJ=NAT(J)
      JSTART=NZTYPE(NJ)*4-3
      R2=(COORD(1,I)-COORD(1,J))**2+(COORD(2,I)-COORD(2,J))**2
     1   + (COORD(3,I)-COORD(3,J))**2
      RIJ=SQRT(R2)
      R0=RIJ/A0
      RR=R2/(A0*A0)
      DO 150 IX=1,3
         DEL1=COORD(IX,I)-COORD(IX,J)
         TERMAA=0.0D+00
         TERMAB=0.0D+00
         ISP=0
         IOL=0
C   THE FIRST DERIVATIVES OF OVERLAP INTEGRALS
         DO 30 K=IA,ID
            KA=K-IA
            KG=ISTART+KA
            DO 30 L=JA,JD
               LA=L-JA
               LG=JSTART+LA
               IOL=IOL+1
               DS(IOL)=0.0D+00
               IF(KA.EQ.0.AND.LA.EQ.0) THEN
C   (S/S) TERM
                  IF(ABS(DEL1).LE.1.0D-06) GO TO 30
                  IS=1
               ELSEIF(KA.EQ.0.AND.LA.GT.0) THEN
C   (S/P) TERM
                  IS=3
                  IF(IX.EQ.LA) GO TO 20
                  IF(ABS(DEL1).LE.1.0D-06) GO TO 30
                  IS=2
                  DEL2=COORD(LA,I)-COORD(LA,J)
               ELSEIF(KA.GT.0.AND.LA.EQ.0) THEN
C   (P/S) TERM
                  IS=5
                  IF(IX.EQ.KA) GO TO 20
                  IF(ABS(DEL1).LE.1.0D-06) GO TO 30
                  IS=4
                  DEL2=COORD(KA,I)-COORD(KA,J)
               ELSE
C   (P/P) TERM
                  IF(KA.EQ.LA) THEN
C    P/P
                     IS=9
                     IF(IX.EQ.KA) GO TO 20
                     IF(ABS(DEL1).LE.1.0D-06) GO TO 30
C    P'/P'
                     IS=8
                     DEL2=COORD(KA,I)-COORD(KA,J)
                  ELSEIF(IX.NE.KA.AND.IX.NE.LA) THEN
C    P'/P"
                     IF(ABS(DEL1).LE.1.0D-06) GO TO 30
                     IS=7
                     DEL2=COORD(KA,I)-COORD(KA,J)
                     DEL3=COORD(LA,I)-COORD(LA,J)
                  ELSE
C    P/P' OR P'/P
                     DEL2=COORD(KA+LA-IX,I)-COORD(KA+LA-IX,J)
                     IS=6
                  ENDIF
               ENDIF
C
C        CALCULATE OVERLAP DERIVATIVES, STORE RESULTS IN DS
C
   20          CALL DERS(KG,LG,RR,DEL1,DEL2,DEL3,IS,IOL)
   30    CONTINUE
         IF(.NOT.MINDO3.AND.IX.EQ.1) READ (2) (G(I22),I22=1,22)
         IF(.NOT.MINDO3) CALL DELRI(DG,NI,NJ,R0,DEL1)
         IXCOPY = IX
         CALL DELMOL(COORD,I,J,NI,NJ,IA,ID,JA,JD,IXCOPY,RIJ,DEL1,ISP)
C
C   THE FIRST DERIVATIVE OF NUCLEAR REPULSION TERM
         IF(MINDO3)THEN
            II=MAX(NI,NJ)
            NBOND=(II*(II-1))/2+NI+NJ-II
            ALPHA=0
            IF(NBOND.LT.154)THEN
               ALPHA=ALP3(NBOND)
            ELSE
               ALPH1=100.0D+00
               ALPH2=100.0D+00
               IF(NATORB(NI).EQ.0) ALPH1=ALPA(NI)
               IF(NATORB(NJ).EQ.0) ALPH2=ALPA(NJ)
            ENDIF
            C2=(7.1995D+00/F03(NI)+7.1995D+00/F03(NJ))**2
            C1=DEL1/RIJ*CORE(NI)*CORE(NJ)*14.399D+00
            C3=DEL1/RIJ*ABS(CORE(NI)*CORE(NJ))*14.399D+00
            IF(NBOND.EQ.22.OR.NBOND.EQ.29)THEN
               TERMNC=-C1*ALPHA*(1.0D+00/RIJ**2 - RIJ*(RIJ**2+
     *C2)**(-1.5D+00)
     1 +  1.0D+00/RIJ - 1.0D+00/SQRT(RIJ**2+C2)) * EXP(-RIJ) -
     2C1*RIJ*(RIJ**2+C2)**(-1.5D+00)
            ELSEIF (RIJ.LT.1.0D+00.AND.NATORB(NI)*NATORB(NJ).EQ.0) THEN
               TERMNC=0.0D+00
            ELSEIF(NBOND.GE.154) THEN
C
C  SPECIAL CASE INVOLVING SPARKLES
C
               EXP1=EXP(-MIN(ALPH1*RIJ,20.0D+00))
               EXP2=EXP(-MIN(ALPH2*RIJ,20.0D+00))
               PART1=-C3*(1.0D+00/RIJ**2 - RIJ*(RIJ**2+C2)**(-1.5D+00))
     1*(EXP1+EXP2)
               PART2=-C3*(1.0D+00/RIJ -1.0D+00/SQRT(RIJ**2+C2))
     1*(ALPH1*EXP1 + ALPH2*EXP2)
               PART3=-C1*RIJ*(RIJ**2+C2)**(-1.5D+00)
               TERMNC=PART1+PART2+PART3
C#            WRITE(6,'(4F13.6)')PART1,PART2,PART3,TERMNC
            ELSE
               TERMNC=-C1*(1.0D+00/RIJ**2-RIJ*(RIJ**2+C2)**(-1.5D+00)+
     1ALPHA/RIJ - ALPHA/SQRT(RIJ**2+C2)) * EXP(-ALPHA*RIJ) -
     2C1*RIJ*(RIJ**2+C2)**(-1.5D+00)
            ENDIF
            DR1=DEL1/RIJ*14.399D+00*RIJ*(RIJ**2+C2)**(-1.5D+00)
         ELSE
C
C      CORE-CORE TERMS, MNDO AND AM1
C
C
C  SPECIAL TREATMENT FOR N-H AND O-H TERMS
C
            IF(RIJ.LT.1.0D+00.AND.NATORB(NI)*NATORB(NJ).EQ.0)THEN
               TERMNC=0.0D+00
               GOTO 50
            ENDIF
            C1=CORE(NI)*CORE(NJ)
            IF(NI.EQ.1.AND.(NJ.EQ.7.OR.NJ.EQ.8)) THEN
               F3=1.0D+00+EXP(-ALPA(1)*RIJ)+RIJ*EXP(-ALPA(NJ)*RIJ)
               DD=(DG(1)*F3-G(1)*(DEL1/RIJ)*(ALPA(1)*EXP(-ALPA(1)*RIJ)
     1 +(ALPA(NJ)*RIJ-1.0D+00)*EXP(-ALPA(NJ)*RIJ)))*C1
            ELSEIF((NI.EQ.7.OR.NI.EQ.8).AND.NJ.EQ.1) THEN
               F3=1.0D+00+EXP(-ALPA(1)*RIJ)+RIJ*EXP(-ALPA(NI)*RIJ)
               DD=(DG(1)*F3-G(1)*(DEL1/RIJ)*(ALPA(1)*EXP(-ALPA(1)*RIJ)
     1 +(ALPA(NI)*RIJ-1.0D+00)*EXP(-ALPA(NI)*RIJ)))*C1
            ELSE
C#            ELSEIF(NATORB(NI)+NATORB(NJ).EQ.0) THEN
C
C  SPECIAL CASE OF TWO SPARKLES
C
               PART1=DG(1)*C1
               PART2=-(G(1)*(DEL1/RIJ)*(ALPA(NI)*EXP(-ALPA(NI)*RIJ)
     *                                 +ALPA(NJ)*EXP(-ALPA(NJ)*RIJ)))
     *                            *ABS(C1)
               PART3=DG(1)*(EXP(-ALPA(NI)*RIJ)
     *                     +EXP(-ALPA(NJ)*RIJ))*ABS(C1)
               DD=PART1+PART2+PART3
C#            WRITE(6,'(4F13.6)')PART1,PART2,PART3,DD
C#            ELSE
C
C   THE GENERAL CASE
C
C#               F3=1.0D+00+EXP(-ALPA(NI)*RIJ)+EXP(-ALPA(NJ)*RIJ)
C#               DD=(DG(1)*F3-G(1)*(DEL1/RIJ)*(ALPA(NI)*EXP(-ALPA(NI)*RI
C#     1J) +ALPA(NJ)*EXP(-ALPA(NJ)*RIJ)))*C1
            ENDIF
            TERMNC=DD
         ENDIF
C
C   ****   START OF THE AM1 SPECIFIC DERIVATIVE CODE   ***
C
C      ANALYT=-A*(1/(R*R)+2.0D+00*B*(R-C)/R)*EXP(-B*(R-C)**2)
C
C    ANALYTICAL DERIVATIVES
C
         IF( AM1 )THEN
            ANAM1=0.0D+00
            DO 40 IG=1,10
               IF(ABS(FN1(NI,IG)).GT.0.0D+00)
     1ANAM1=ANAM1+FN1(NI,IG)*
     2(1.0D+00/(RIJ*RIJ)+2.0D+00*FN2(NI,IG)*(RIJ-FN3(NI,IG))/RIJ)*
     3EXP(MAX(-30.0D+00,-FN2(NI,IG)*(RIJ-FN3(NI,IG))**2))
               IF(ABS(FN1(NJ,IG)).GT.0.0D+00)
     1ANAM1=ANAM1+FN1(NJ,IG)*
     2(1.0D+00/(RIJ*RIJ)+2.0D+00*FN2(NJ,IG)*(RIJ-FN3(NJ,IG))/RIJ)*
     3EXP(MAX(-30.0D+00,-FN2(NJ,IG)*(RIJ-FN3(NJ,IG))**2))
   40       CONTINUE
            ANAM1=ANAM1*CORE(NI)*CORE(NJ)
            TERMNC=TERMNC-ANAM1*DEL1/RIJ
         ENDIF
C
C   ****   END OF THE AM1 SPECIFIC DERIVATIVE CODE   ***
C
   50    CONTINUE
C
C   COMBINE TOGETHER THE OVERLAP DERIVATIVE PARTS
C
         IF(MINDO3)THEN
            II=MAX(NI,NJ)
            NBOND=(II*(II-1))/2+NI+NJ-II
            IF(NBOND.GT.153)GOTO 60
            BI(1)=BETA3(NBOND)*VS(NI)*2.0D+00
            BI(2)=BETA3(NBOND)*VP(NI)*2.0D+00
            BI(3)=BI(2)
            BI(4)=BI(2)
            BJ(1)=BETA3(NBOND)*VS(NJ)*2.0D+00
            BJ(2)=BETA3(NBOND)*VP(NJ)*2.0D+00
            BJ(3)=BJ(2)
            BJ(4)=BJ(2)
   60       CONTINUE
         ELSE
            BI(1)=BETAS(NI)
            BI(2)=BETAP(NI)
            BI(3)=BI(2)
            BI(4)=BI(2)
            BJ(1)=BETAS(NJ)
            BJ(2)=BETAP(NJ)
            BJ(3)=BJ(2)
            BJ(4)=BJ(2)
         ENDIF
C
C       CODE COMMON TO MINDO/3, MNDO, AND AM1
C
         IOL=0
         DO 70 K=IA,ID
            DO 70 L=JA,JD
               LK=L+K*(K-1)/2
               TERMK=BI(K-IA+1)
               TERML=BJ(L-JA+1)
               IOL=IOL+1
               TERMAB=TERMAB+(TERMK+TERML)
     1*PSUM(LK)*DS(IOL)
   70    CONTINUE
         IF(MINDO3)THEN
C
C        FIRST, CORE-ELECTRON ATTRACTION DERIVATIVES (MINDO/3)
C
C          ATOM CORE I AFFECTING A.O.S ON J
            DO 80 M=JA,JD
               MN=(M*(M+1))/2
   80       TERMAB=TERMAB+CORE(NI)*PSUM(MN)*DR1
C          ATOM CORE J AFFECTING A.O.S ON I
            DO 90 M=IA,ID
               MN=(M*(M+1))/2
   90       TERMAB=TERMAB+CORE(NJ)*PSUM(MN)*DR1
C
C   NOW FOR COULOMB AND EXCHANGE TERMS (MINDO/3)
C
            DO 100 I1=IA,ID
               II=(I1*(I1+1))/2
               DO 100 J1=JA,JD
                  JJ=(J1*(J1+1))/2
                  IJ=J1+(I1*(I1-1))/2
C
C           COULOMB TERM
C
                  TERMAA=TERMAA-PSUM(II)*DR1*PSUM(JJ)
C
C           EXCHANGE TERM
C
                  TERMAA=TERMAA+(PALPHA(IJ)*PALPHA(IJ)
     *                          +PBETA (IJ)*PBETA (IJ))*DR1
  100       CONTINUE
         ELSE
C
C        FIRST, CORE-ELECTRON ATTRACTION DERIVATIVES (MNDO AND AM1)
C
C          ATOM CORE I AFFECTING A.O.S ON J
            ISP=0
            DO 110 M=JA,JD
               BB=1.0D+00
               DO 110 N=M,JD
                  MN=M+N*(N-1)/2
                  ISP=ISP+1
                  TERMAB=TERMAB-BB*CORE(NI)*PSUM(MN)*DR(ISP)
  110       BB=2.0D+00
C          ATOM CORE J AFFECTING A.O.S ON I
            K=MAX(JD-JA+1,1)
            K=(K*(K+1))/2
            ISP=-K+1
            DO 120 M=IA,ID
               BB=1.0D+00
               DO 120 N=M,ID
                  MN=M+N*(N-1)/2
                  ISP=ISP+K
                  TERMAB=TERMAB-BB*CORE(NJ)*PSUM(MN)*DR(ISP)
  120       BB=2.0D+00
            ISP=0
C
C   NOW FOR COULOMB AND EXCHANGE TERMS (MNDO AND AM1)
C
            DO 140 K=IA,ID
               AA=1.0D+00
               KK=(K*(K-1))/2
               DO 140 L=K,ID
                  LL=(L*(L-1))/2
                  DO 130 M=JA,JD
                     BB=1.0D+00
                     DO 130 N=M,JD
                        ISP=ISP+1
                        KL=K+LL
                        MN=M+N*(N-1)/2
C
C    COULOMB TERM
C
                        TERMAA=TERMAA+AA*BB*PSUM(KL)*PSUM(MN)*DR(ISP)
                        MK=M+KK
                        NK=N+KK
                        ML=M+LL
                        NL=N+LL
C
C    EXCHANGE TERM
C
                        TERMAA= TERMAA-0.5D+00*AA*BB*
     *                          (PALPHA(MK)*PALPHA(NL)
     *                          +PALPHA(NK)*PALPHA(ML)
     *                          +PBETA (MK)*PBETA (NL)
     *                          +PBETA (NK)*PBETA (ML))*DR(ISP)
  130             BB=2.0D+00
  140       AA=2.0D+00
C           END OF MNDO AND AM1 SPECIFIC CODE
         ENDIF
         EAA(IX)=EAA(IX)+TERMAA
         EAB(IX)=EAB(IX)+TERMAB
         ENUC(IX)=ENUC(IX)+TERMNC
  150 CONTINUE
C
      DO 180 J=1,3
         ENG(J)=EAA(J)+EAB(J)+ENUC(J)
         ENG(J) = -ENG(J)*23.061D+00
  180 CONTINUE
      RETURN
      END
C*MODULE MPCGRD  *DECK DCART
      SUBROUTINE DCART(COORD,DXYZ,P,PA,PB,L2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500)
      PARAMETER (TOANGS=0.52917724924D+00, TOKCAL=627.51D+00)
      CHARACTER*241 KEYWRD
      LOGICAL FORCE, MAKEP, ANADER
C
      DIMENSION P(L2),PA(L2),PB(L2),COORD(3,*),DXYZ(3,*)
      DIMENSION PDI(171),PADI(171),PBDI(171),
     *          CDI(3,2),NDI(2),LSTOR1(6), LSTOR2(6), ENG(3)
C
      COMMON /MOLKST/ NUMAT,NAT(MXATM),NFIRST(MXATM),NMIDLE(MXATM),
     1                NLAST(MXATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /KEYWRD/ KEYWRD
      COMMON /UCELL / L1L,L2L,L3L,L1U,L2U,L3U
      COMMON /DCARTC/ K1L,K2L,K3L,K1U,K2U,K3U
      COMMON /MOLMEC/ HTYPE(4),NHCO(4,200),NNHCO,ITYPE
C
      EQUIVALENCE (LSTOR1(1),L1L), (LSTOR2(1), K1L)
C
      SAVE CHNGE, CHNGE2, ANADER, FORCE
C
C    DCART CALCULATES THE DERIVATIVES OF THE ENERGY WITH RESPECT TO THE
C          CARTESIAN COORDINATES. THIS IS DONE BY FINITE DIFFERENCES.
C
C    THE MAIN ARRAYS IN DCART ARE:
C        DXYZ   ON EXIT CONTAINS THE CARTESIAN DERIVATIVES.
C
C CHNGE IS A MACHINE-PRECISION DEPENDENT CONSTANT
C
      CHNGE = 1.0D-04
      CHNGE2=CHNGE*0.5D+00
C
      AA = 0.0D+00
      ANADER= (INDEX(KEYWRD,'ANALYT') .NE. 0)
      FORCE = .TRUE.
      NCELLS=(L1U-L1L+1)*(L2U-L2L+1)*(L3U-L3L+1)
C
      DO 10 I=1,6
         LSTOR2(I)=LSTOR1(I)
   10 LSTOR1(I)=0
      IOFSET=(NCELLS+1)/2
      NUMTOT=NUMAT*NCELLS
      DO 20 I=1,NUMTOT
         DO 20 J=1,3
   20 DXYZ(J,I)=0.0D+00
C
      IF(ANADER) REWIND 2
C
      DO 130 II=1,NUMAT
         III=NCELLS*(II-1)+IOFSET
         IM1=II
         IF=NFIRST(II)
         IM=NMIDLE(II)
         IL=NLAST(II)
         NDI(2)=NAT(II)
         DO 30 I=1,3
   30    CDI(I,2)=COORD(I,II)
         DO 130 JJ=1,IM1
            JJJ=NCELLS*(JJ-1)
C  FORM DIATOMIC MATRICES
            JF=NFIRST(JJ)
            JM=NMIDLE(JJ)
            JL=NLAST(JJ)
C   GET FIRST ATOM
            NDI(1)=NAT(JJ)
            MAKEP=.TRUE.
            DO 120 IK=K1L,K1U
               DO 120 JK=K2L,K2U
                  DO 120 KL=K3L,K3U
                     JJJ=JJJ+1
                     DO L=1,3
                        CDI(L,1)=COORD(L,JJ)
                     ENDDO
                     IF(.NOT. MAKEP) GOTO 90
                     MAKEP=.FALSE.
                     IJ=0
                     DO 50 I=JF,JL
                        K=I*(I-1)/2+JF-1
                        DO 50 J=JF,I
                           IJ=IJ+1
                           K=K+1
                           PADI(IJ)=PA(K)
                           PBDI(IJ)=PB(K)
   50                PDI(IJ)=P(K)
C GET SECOND ATOM FIRST ATOM INTERSECTION
                     DO 80 I=IF,IL
                        L=I*(I-1)/2
                        K=L+JF-1
                        DO 60 J=JF,JL
                           IJ=IJ+1
                           K=K+1
                           PADI(IJ)=PA(K)
                           PBDI(IJ)=PB(K)
   60                   PDI(IJ)=P(K)
                        K=L+IF-1
                        DO 70 L=IF,I
                           K=K+1
                           IJ=IJ+1
                           PADI(IJ)=PA(K)
                           PBDI(IJ)=PB(K)
   70                   PDI(IJ)=P(K)
   80                CONTINUE
   90                CONTINUE
                     IF(II.EQ.JJ) GOTO  120
                     IF(ANADER)THEN
                        CALL ANALYT(PDI,PADI,PBDI,CDI,NDI,JF,JL,IF,IL
     1,                 ENG)
                        DO 100 K=1,3
                           DXYZ(K,III)=DXYZ(K,III)-ENG(K)
  100                   DXYZ(K,JJJ)=DXYZ(K,JJJ)+ENG(K)
                     ELSE
                        IF( .NOT. FORCE) THEN
                           CDI(1,1)=CDI(1,1)+CHNGE2
                           CDI(2,1)=CDI(2,1)+CHNGE2
                           CDI(3,1)=CDI(3,1)+CHNGE2
                           CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,IF,
     1                              IM,IL,AA)
                        ENDIF
                        DO 110 K=1,3
                           IF( FORCE )THEN
                              CDI(K,2)=CDI(K,2)-CHNGE2
                              CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,
     1                                 IF,IM,IL,AA)
                           ENDIF
                           CDI(K,2)=CDI(K,2)+CHNGE
                           CALL DHC(PDI,PADI,PBDI,CDI,NDI,JF,JM,JL,IF,
     1                              IM,IL,EE)
                           CDI(K,2)=CDI(K,2)-CHNGE2
                           IF( .NOT. FORCE) CDI(K,2)=CDI(K,2)-CHNGE2
                           DERIV=(AA-EE)*23.061D+00/CHNGE
                           DXYZ(K,III)=DXYZ(K,III)-DERIV
                           DXYZ(K,JJJ)=DXYZ(K,JJJ)+DERIV
  110                   CONTINUE
                     ENDIF
  120       CONTINUE
  130 CONTINUE
C
      IF(NNHCO.NE.0)THEN
C
C   NOW ADD IN MOLECULAR-MECHANICS CORRECTION TO THE H-N-C=O TORSION
C
         DEL=1.D-8
         DO 160 I=1,NNHCO
            DO 150 J=1,4
               DO 140 K=1,3
                  COORD(K,NHCO(J,I))=COORD(K,NHCO(J,I))-DEL
                  CALL DIHED(COORD,NHCO(1,I),NHCO(2,I),NHCO(3,I),NHCO(4,
     1I),ANGLE)
                  REFH=HTYPE(ITYPE)*SIN(ANGLE)**2
                  COORD(K,NHCO(J,I))=COORD(K,NHCO(J,I))+DEL*2.D0
                  CALL DIHED(COORD,NHCO(1,I),NHCO(2,I),NHCO(3,I),NHCO(4,
     1I),ANGLE)
                  COORD(K,NHCO(J,I))=COORD(K,NHCO(J,I))-DEL
                  HEAT=HTYPE(ITYPE)*SIN(ANGLE)**2
                  SUM=(REFH-HEAT)/(2.D0*DEL)
                  DXYZ(K,NHCO(J,I))=DXYZ(K,NHCO(J,I))-SUM
  140          CONTINUE
  150       CONTINUE
  160    CONTINUE
      ENDIF
C
C
C
C
      DO 170 I=1,6
  170 LSTOR1(I)=LSTOR2(I)
C
      DO 115 J=1,NUMAT
        DO 114 I=1,3
          DXYZ(I,J)=DXYZ(I,J)*TOANGS/TOKCAL
  114   CONTINUE
  115 CONTINUE
C
      IF (ANADER) REWIND 2
      RETURN
      END
C*MODULE MPCGRD  *DECK DELMOL
      SUBROUTINE DELMOL(COORD,I,J,NI,NJ,IA,ID,JA,JD,IX,RIJ,TOMB,ISP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION COORD(3,25)
      COMMON /DERIVS/ DS(16),DG(22),DR(100),TDX(3),TDY(3),TDZ(3)
      COMMON /EXTRA/  G(22),TX(3),TY(3),TZ(3)
      IF(NI.GT.1.OR.NJ.GT.1) CALL ROTAT(COORD,I,J,IX,RIJ,TOMB,2)
      IB=MAX(IA,ID)
      JB=MAX(JA,JD)
      DO 10 K=IA,IB
         KK=K-IA
         DO 10 L=K,IB
            LL=L-IA
            DO 10 M=JA,JB
               MM=M-JA
               DO 10 N=M,JB
                  NN=N-JA
                  ISP=ISP+1
                  IF(NN.EQ.0)THEN
                     IF(LL.EQ.0) THEN
C   (SS/SS)
                        DR(ISP)=DG(1)
                     ELSEIF(KK.EQ.0) THEN
C   (SP/SS)
                        DR(ISP)=DG(2)*TX(LL)+G(2)*TDX(LL)
                     ELSE
C   (PP/SS)
                        DR(ISP)=DG(3)*TX(KK)*TX(LL)
     1       +G(3)*(TDX(KK)*TX(LL)+TX(KK)*TDX(LL))
     2       +DG(4)*(TY(KK)*TY(LL)+TZ(KK)*TZ(LL))
     3       +G(4)*(TDY(KK)*TY(LL)+TY(KK)*TDY(LL)
     4             +TDZ(KK)*TZ(LL)+TZ(KK)*TDZ(LL))
                     ENDIF
                  ELSEIF(MM.EQ.0) THEN
                     IF(LL.EQ.0) THEN
C   (SS/SP)
                        DR(ISP)=DG(5)*TX(NN)+G(5)*TDX(NN)
                     ELSEIF(KK.EQ.0) THEN
C   (SP/SP)
                        DR(ISP)=DG(6)*TX(LL)*TX(NN)
     1       +G(6)*(TDX(LL)*TX(NN)+TX(LL)*TDX(NN))
     2       +DG(7)*(TY(LL)*TY(NN)+TZ(LL)*TZ(NN))
     3       +G(7)*(TDY(LL)*TY(NN)+TY(LL)*TDY(NN)
     4             +TDZ(LL)*TZ(NN)+TZ(LL)*TDZ(NN))
                     ELSE
C   (PP/SP)
                        DR(ISP)=DG(8)*TX(KK)*TX(LL)*TX(NN)
     1       +G(8)*(TDX(KK)*TX(LL)*TX(NN)+TX(KK)*TDX(LL)*TX(NN)
     2             +TX(KK)*TX(LL)*TDX(NN))
     3       +DG(9)*(TY(KK)*TY(LL)+TZ(KK)*TZ(LL))*TX(NN)
     4       +G(9)*((TDY(KK)*TY(LL)+TY(KK)*TDY(LL)
     5              +TDZ(KK)*TZ(LL)+TZ(KK)*TDZ(LL))*TX(NN)
     6             +(TY(KK)*TY(LL)+TZ(KK)*TZ(LL))*TDX(NN))
     7       +DG(10)*(TX(KK)*(TY(LL)*TY(NN)+TZ(LL)*TZ(NN))
     8               +TX(LL)*(TY(KK)*TY(NN)+TZ(KK)*TZ(NN)))
     9       +G(10)*(TDX(KK)*(TY(LL)*TY(NN)+TZ(LL)*TZ(NN))
     1              +TDX(LL)*(TY(KK)*TY(NN)+TZ(KK)*TZ(NN))
     2              +TX(KK)*(TDY(LL)*TY(NN)+TY(LL)*TDY(NN)
     3                      +TDZ(LL)*TZ(NN)+TZ(LL)*TDZ(NN))
     4              +TX(LL)*(TDY(KK)*TY(NN)+TY(KK)*TDY(NN)
     5                      +TDZ(KK)*TZ(NN)+TZ(KK)*TDZ(NN)))
                     ENDIF
                  ELSEIF(LL.EQ.0) THEN
C   (SS/PP)
                     DR(ISP)=DG(11)*TX(MM)*TX(NN)
     1       +G(11)*(TDX(MM)*TX(NN)+TX(MM)*TDX(NN))
     2       +DG(12)*(TY(MM)*TY(NN)+TZ(MM)*TZ(NN))
     3       +G(12)*(TDY(MM)*TY(NN)+TY(MM)*TDY(NN)
     4              +TDZ(MM)*TZ(NN)+TZ(MM)*TDZ(NN))
                  ELSEIF(KK.EQ.0) THEN
C   (SP/PP)
                     DR(ISP)=DG(13)*TX(LL)*TX(MM)*TX(NN)
     1       +G(13)*(TDX(LL)*TX(MM)*TX(NN)+TX(LL)*TDX(MM)*TX(NN)
     2              +TX(LL)*TX(MM)*TDX(NN))
     3       +DG(14)*TX(LL)*(TY(MM)*TY(NN)+TZ(MM)*TZ(NN))
     4       +G(14)*(TDX(LL)*(TY(MM)*TY(NN)+TZ(MM)*TZ(NN))
     5              +TX(LL)*(TDY(MM)*TY(NN)+TY(MM)*TDY(NN)
     6                      +TDZ(MM)*TZ(NN)+TZ(MM)*TDZ(NN)))
     7       +DG(15)*(TY(LL)*(TY(MM)*TX(NN)+TY(NN)*TX(MM))
     8               +TZ(LL)*(TZ(MM)*TX(NN)+TZ(NN)*TX(MM)))
     9       +G(15)*(TDY(LL)*(TY(MM)*TX(NN)+TY(NN)*TX(MM))
     1              +TDZ(LL)*(TZ(MM)*TX(NN)+TZ(NN)*TX(MM))
     2              +TY(LL)*(TDY(MM)*TX(NN)+TY(MM)*TDX(NN)
     3                      +TDY(NN)*TX(MM)+TY(NN)*TDX(MM))
     4              +TZ(LL)*(TDZ(MM)*TX(NN)+TZ(MM)*TDX(NN)
     5                      +TDZ(NN)*TX(MM)+TZ(NN)*TDX(MM)))
                  ELSE
C   (PP/PP)
                     DR(ISP)=DG(16)*TX(KK)*TX(LL)*TX(MM)*TX(NN)
     1       +G(16)*(TDX(KK)*TX(LL)*TX(MM)*TX(NN)
     2              +TX(KK)*TDX(LL)*TX(MM)*TX(NN)
     3              +TX(KK)*TX(LL)*TDX(MM)*TX(NN)
     4              +TX(KK)*TX(LL)*TX(MM)*TDX(NN))
     5       +DG(17)*(TY(KK)*TY(LL)+TZ(KK)*TZ(LL))*TX(MM)*TX(NN)
     6       +G(17)*((TDY(KK)*TY(LL)+TY(KK)*TDY(LL)
     7               +TDZ(KK)*TZ(LL)+TZ(KK)*TDZ(LL))*TX(MM)*TX(NN)
     8              +(TY(KK)*TY(LL)+TZ(KK)*TZ(LL))
     9              *(TDX(MM)*TX(NN)+TX(MM)*TDX(NN)))
     1       +DG(18)*TX(KK)*TX(LL)*(TY(MM)*TY(NN)+TZ(MM)*TZ(NN))
     2       +G(18)*((TDX(KK)*TX(LL)+TX(KK)*TDX(LL))
     3                 *(TY(MM)*TY(NN)+TZ(MM)*TZ(NN))
     4              +TX(KK)*TX(LL)*(TDY(MM)*TY(NN)+TY(MM)*TDY(NN)
     5                             +TDZ(MM)*TZ(NN)+TZ(MM)*TDZ(NN)))
                     DR(ISP)=DR(ISP)
     1       +DG(19)*(TY(KK)*TY(LL)*TY(MM)*TY(NN)
     2                  +TZ(KK)*TZ(LL)*TZ(MM)*TZ(NN))
     3       +G(19)*(TDY(KK)*TY(LL)*TY(MM)*TY(NN)
     4                 +TY(KK)*TDY(LL)*TY(MM)*TY(NN)
     5                 +TY(KK)*TY(LL)*TDY(MM)*TY(NN)
     6                 +TY(KK)*TY(LL)*TY(MM)*TDY(NN)
     7                 +TDZ(KK)*TZ(LL)*TZ(MM)*TZ(NN)
     8                 +TZ(KK)*TDZ(LL)*TZ(MM)*TZ(NN)
     9                 +TZ(KK)*TZ(LL)*TDZ(MM)*TZ(NN)
     1                 +TZ(KK)*TZ(LL)*TZ(MM)*TDZ(NN))
     2       +DG(20)*(TX(KK)*(TX(MM)*(TY(LL)*TY(NN)+TZ(LL)*TZ(NN))
     3                          +TX(NN)*(TY(LL)*TY(MM)+TZ(LL)*TZ(MM)))
     4                  +TX(LL)*(TX(MM)*(TY(KK)*TY(NN)+TZ(KK)*TZ(NN))
     5                          +TX(NN)*(TY(KK)*TY(MM)+TZ(KK)*TZ(MM))))
C      TO AVOID COMPILER DIFFICULTIES THIS IS DIVIDED
                     TEMP1=
     *              TDX(KK)*(TX(MM)*(TY(LL)*TY(NN)+TZ(LL)*TZ(NN))
     *                      +TX(NN)*(TY(LL)*TY(MM)+TZ(LL)*TZ(MM)))
     *             +TDX(LL)*(TX(MM)*(TY(KK)*TY(NN)+TZ(KK)*TZ(NN))
     *                      +TX(NN)*(TY(KK)*TY(MM)+TZ(KK)*TZ(MM)))
     *              +TX(KK)*(TDX(MM)*(TY(LL)*TY(NN)+TZ(LL)*TZ(NN))
     *                      +TDX(NN)*(TY(LL)*TY(MM)+TZ(LL)*TZ(MM)))
     *              +TX(LL)*(TDX(MM)*(TY(KK)*TY(NN)+TZ(KK)*TZ(NN))
     *                      +TDX(NN)*(TY(KK)*TY(MM)+TZ(KK)*TZ(MM)))
                     TEMP2= TX(KK)*(TX(MM)*
     *  (TDY(LL)*TY(NN)+TY(LL)*TDY(NN)+TDZ(LL)*TZ(NN)+TZ(LL)*TDZ(NN))
     *                             +TX(NN)*
     *  (TDY(LL)*TY(MM)+TY(LL)*TDY(MM)+TDZ(LL)*TZ(MM)+TZ(LL)*TDZ(MM)))
     *                     +TX(LL)*(TX(MM)*
     *  (TDY(KK)*TY(NN)+TY(KK)*TDY(NN)+TDZ(KK)*TZ(NN)+TZ(KK)*TDZ(NN))
     *                             +TX(NN)*
     *  (TDY(KK)*TY(MM)+TY(KK)*TDY(MM)+TDZ(KK)*TZ(MM)+TZ(KK)*TDZ(MM)))
C
                     DR(ISP)=DR(ISP)+G(20)*(TEMP1+TEMP2)
                     DR(ISP)=DR(ISP)
     1       +DG(21)*(TY(KK)*TY(LL)*TZ(MM)*TZ(NN)
     2                 +TZ(KK)*TZ(LL)*TY(MM)*TY(NN))
     3       +G(21)*(TDY(KK)*TY(LL)*TZ(MM)*TZ(NN)
     4                 +TY(KK)*TDY(LL)*TZ(MM)*TZ(NN)
     5                 +TY(KK)*TY(LL)*TDZ(MM)*TZ(NN)
     6                 +TY(KK)*TY(LL)*TZ(MM)*TDZ(NN)
     7                 +TDZ(KK)*TZ(LL)*TY(MM)*TY(NN)
     8                 +TZ(KK)*TDZ(LL)*TY(MM)*TY(NN)
     9                 +TZ(KK)*TZ(LL)*TDY(MM)*TY(NN)
     1                 +TZ(KK)*TZ(LL)*TY(MM)*TDY(NN))
                     DR(ISP)=DR(ISP)
     1       +DG(22)*(TY(KK)*TZ(LL)+TZ(KK)*TY(LL))
     2                 *(TY(MM)*TZ(NN)+TZ(MM)*TY(NN))
     3       +G(22)*((TDY(KK)*TZ(LL)+TY(KK)*TDZ(LL)
     4                  +TDZ(KK)*TY(LL)+TZ(KK)*TDY(LL))
     5                 *(TY(MM)*TZ(NN)+TZ(MM)*TY(NN))
     6                 +(TY(KK)*TZ(LL)+TZ(KK)*TY(LL))
     7                 *(TDY(MM)*TZ(NN)+TY(MM)*TDZ(NN)
     8                  +TDZ(MM)*TY(NN)+TZ(MM)*TDY(NN)))
                  ENDIF
   10 CONTINUE
      RETURN
      END
C*MODULE MPCGRD  *DECK DELRI
      SUBROUTINE DELRI(DG,NI,NJ,RR,DEL1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION DG(22)
C
      COMMON /MULTIP/ DD(107),QQ(107),BDD(107,3)
      COMMON /NATORB/ NATORB(107)
C
C
C    ON INPUT NI = ATOMIC NUMBER OF FIRST ATOM
C             NJ = ATOMIC NUMBER OF SECOND ATOM
C             RR = INTERATOMIC DISTANCE IN BOHRS
C
C
      DZE =0.0D+00
      QZZE=0.0D+00
      QXXE=0.0D+00
C
      A0=0.529167D+00
      TERM=(27.21D+00*DEL1)/(RR*A0*A0)
      DA=DD(NI)
      DB=DD(NJ)
      QA=QQ(NI)
      QB=QQ(NJ)
C
C   HYDROGEN-HYDROGEN
C
      AEE=0.25D+00*(1.0D+00/BDD(NI,1)+1.0D+00/BDD(NJ,1))**2
      EE    =-RR/(SQRT(RR**2+AEE))**3
      DG(1)=TERM*EE
      IF(NATORB(NI).LE.2.AND.NATORB(NJ).LE.2) RETURN
C
C   HEAVY ATOM-HYDROGEN
C
      IF(NATORB(NI).LE.2) GO TO 10
      ADE=0.25D+00*(1.0D+00/BDD(NI,2)+1.0D+00/BDD(NJ,1))**2
      AQE=0.25D+00*(1.0D+00/BDD(NI,3)+1.0D+00/BDD(NJ,1))**2
      DZE   = (RR+DA)/(SQRT((RR+DA)**2+ADE))**3
     1       -(RR-DA)/(SQRT((RR-DA)**2+ADE))**3
      QZZE  =-(RR+2.0D+00*QA)/(SQRT((RR+2.0D+00*QA)**2+AQE))**3
     1       -(RR-2.0D+00*QA)/(SQRT((RR-2.0D+00*QA)**2+AQE))**3
     2       +(2.0D+00*RR)/(SQRT(RR**2+AQE))**3
      QXXE  =-(2.0D+00*RR)/(SQRT(RR**2+4.0D+00*QA**2+AQE))**3
     1       +(2.0D+00*RR)/(SQRT(RR**2+AQE))**3
      DG(2)=-(TERM*DZE)/2.0D+00
      DG(3)=TERM*(EE+QZZE/4.0D+00)
      DG(4)=TERM*(EE+QXXE/4.0D+00)
      IF(NATORB(NJ).LE.2) RETURN
C
C   HYDROGEN-HEAVY ATOM
C
   10 AED=0.25D+00*(1.0D+00/BDD(NI,1)+1.0D+00/BDD(NJ,2))**2
      AEQ=0.25D+00*(1.0D+00/BDD(NI,1)+1.0D+00/BDD(NJ,3))**2
      EDZ   = (RR-DB)/(SQRT((RR-DB)**2+AED))**3
     1       -(RR+DB)/(SQRT((RR+DB)**2+AED))**3
      EQZZ  =-(RR-2.0D+00*QB)/(SQRT((RR-2.0D+00*QB)**2+AEQ))**3
     1       -(RR+2.0D+00*QB)/(SQRT((RR+2.0D+00*QB)**2+AEQ))**3
     2       +(2.0D+00*RR)/(SQRT(RR**2+AEQ))**3
      EQXX  =-(2.0D+00*RR)/(SQRT(RR**2+4.0D+00*QB**2+AEQ))**3
     1       +(2.0D+00*RR)/(SQRT(RR**2+AEQ))**3
      DG(5)=-(TERM*EDZ)/2.0D+00
      DG(11)=TERM*(EE+EQZZ/4.0D+00)
      DG(12)=TERM*(EE+EQXX/4.0D+00)
      IF(NATORB(NI).LE.2) RETURN
C
C   HEAVY ATOM-HEAVY ATOM
C
      ADD=0.25D+00*(1.0D+00/BDD(NI,2)+1.0D+00/BDD(NJ,2))**2
      ADQ=0.25D+00*(1.0D+00/BDD(NI,2)+1.0D+00/BDD(NJ,3))**2
      AQD=0.25D+00*(1.0D+00/BDD(NI,3)+1.0D+00/BDD(NJ,2))**2
      AQQ=0.25D+00*(1.0D+00/BDD(NI,3)+1.0D+00/BDD(NJ,3))**2
      DXDX  =-(2.0D+00*RR)/(SQRT(RR**2+(DA-DB)**2+ADD))**3
     1       +(2.0D+00*RR)/(SQRT(RR**2+(DA+DB)**2+ADD))**3
      DZDZ  =-(RR+DA-DB)/(SQRT((RR+DA-DB)**2+ADD))**3
     1       -(RR-DA+DB)/(SQRT((RR-DA+DB)**2+ADD))**3
     2       +(RR-DA-DB)/(SQRT((RR-DA-DB)**2+ADD))**3
     3       +(RR+DA+DB)/(SQRT((RR+DA+DB)**2+ADD))**3
      DZQXX = 2.0D+00*(RR+DA)/(SQRT((RR+DA)**2+4.0D+00*QB**2+ADQ))**3
     1       -2.0D+00*(RR-DA)/(SQRT((RR-DA)**2+4.0D+00*QB**2+ADQ))**3
     2       -2.0D+00*(RR+DA)/(SQRT((RR+DA)**2+ADQ))**3
     3       +2.0D+00*(RR-DA)/(SQRT((RR-DA)**2+ADQ))**3
      QXXDZ = 2.0D+00*(RR-DB)/(SQRT((RR-DB)**2+4.0D+00*QA**2+AQD))**3
     1       -2.0D+00*(RR+DB)/(SQRT((RR+DB)**2+4.0D+00*QA**2+AQD))**3
     2       -2.0D+00*(RR-DB)/(SQRT((RR-DB)**2+AQD))**3
     3       +2.0D+00*(RR+DB)/(SQRT((RR+DB)**2+AQD))**3
      DZQZZ = (RR+DA-2.0D+00*QB)/(SQRT((RR+DA-2.0D+00*QB)**2+ADQ))**3
     1       -(RR-DA-2.0D+00*QB)/(SQRT((RR-DA-2.0D+00*QB)**2+ADQ))**3
     2       +(RR+DA+2.0D+00*QB)/(SQRT((RR+DA+2.0D+00*QB)**2+ADQ))**3
     3       -(RR-DA+2.0D+00*QB)/(SQRT((RR-DA+2.0D+00*QB)**2+ADQ))**3
     4       +2.0D+00*(RR-DA)/(SQRT((RR-DA)**2+ADQ))**3
     5       -2.0D+00*(RR+DA)/(SQRT((RR+DA)**2+ADQ))**3
      QZZDZ = (RR+2.0D+00*QA-DB)/(SQRT((RR+2.0D+00*QA-DB)**2+AQD))**3
     1       -(RR+2.0D+00*QA+DB)/(SQRT((RR+2.0D+00*QA+DB)**2+AQD))**3
     2       +(RR-2.0D+00*QA-DB)/(SQRT((RR-2.0D+00*QA-DB)**2+AQD))**3
     3       -(RR-2.0D+00*QA+DB)/(SQRT((RR-2.0D+00*QA+DB)**2+AQD))**3
     4       -2.0D+00*(RR-DB)/(SQRT((RR-DB)**2+AQD))**3
     5       +2.0D+00*(RR+DB)/(SQRT((RR+DB)**2+AQD))**3
      QXXQXX=-(2.0D+00*RR)/(SQRT(RR**2+4.0D+00*(QA-QB)**2+AQQ))**3
     1       -(2.0D+00*RR)/(SQRT(RR**2+4.0D+00*(QA+QB)**2+AQQ))**3
     2       +(4.0D+00*RR)/(SQRT(RR**2+4.0D+00*QA**2+AQQ))**3
     3       +(4.0D+00*RR)/(SQRT(RR**2+4.0D+00*QB**2+AQQ))**3
     4       -(4.0D+00*RR)/(SQRT(RR**2+AQQ))**3
      QXXQYY=-(4.0D+00*RR)/
     *       (SQRT(RR**2+4.0D+00*QA**2+4.0D+00*QB**2+AQQ))**3
     1       +(4.0D+00*RR)/(SQRT(RR**2+4.0D+00*QA**2+AQQ))**3
     2       +(4.0D+00*RR)/(SQRT(RR**2+4.0D+00*QB**2+AQQ))**3
     3       -(4.0D+00*RR)/(SQRT(RR**2+AQQ))**3
      QXXQZZ=
     1     -2.0D+00*(RR-2.0D+00*QB)/(SQRT((RR-2.0D+00*QB)**2+4.0D+00*
     *QA**2+AQQ))**3
     2     -2.0D+00*(RR+2.0D+00*QB)/(SQRT((RR+2.0D+00*QB)**2+4.0D+00*
     *QA**2+AQQ))**3
     3       +2.0D+00*(RR-2.0D+00*QB)/(SQRT((RR-2.0D+00*QB)**2+AQQ))**3
     4       +2.0D+00*(RR+2.0D+00*QB)/(SQRT((RR+2.0D+00*QB)**2+AQQ))**3
     5       +(4.0D+00*RR)/(SQRT(RR**2+4.0D+00*QA**2+AQQ))**3
     6       -(4.0D+00*RR)/(SQRT(RR**2+AQQ))**3
      QZZQXX=
     1     -2.0D+00*(RR+2.0D+00*QA)/(SQRT((RR+2.0D+00*QA)**2+4.0D+00*
     *QB**2+AQQ))**3
     2     -2.0D+00*(RR-2.0D+00*QA)/(SQRT((RR-2.0D+00*QA)**2+4.0D+00*
     *QB**2+AQQ))**3
     3      +2.0D+00*(RR+2.0D+00*QA)/(SQRT((RR+2.0D+00*QA)**2+AQQ))**3
     4      +2.0D+00*(RR-2.0D+00*QA)/(SQRT((RR-2.0D+00*QA)**2+AQQ))**3
     5      +(4.0D+00*RR)/(SQRT(RR**2+4.0D+00*QB**2+AQQ))**3
     6      -(4.0D+00*RR)/(SQRT(RR**2+AQQ))**3
      QZZQZZ=
     1 -(RR+2.0D+00*QA-2.0D+00*QB)/(SQRT((RR+2.0D+00*QA-2.0D+00*QB)**2
     *+AQQ))**3
     2 -(RR+2.0D+00*QA+2.0D+00*QB)/(SQRT((RR+2.0D+00*QA+2.0D+00*QB)**2
     *+AQQ))**3
     3 -(RR-2.0D+00*QA-2.0D+00*QB)/(SQRT((RR-2.0D+00*QA-2.0D+00*QB)**2
     *+AQQ))**3
     4 -(RR-2.0D+00*QA+2.0D+00*QB)/(SQRT((RR-2.0D+00*QA+2.0D+00*QB)**2
     *+AQQ))**3
     5      +2.0D+00*(RR-2.0D+00*QA)/(SQRT((RR-2.0D+00*QA)**2+AQQ))**3
     6      +2.0D+00*(RR+2.0D+00*QA)/(SQRT((RR+2.0D+00*QA)**2+AQQ))**3
     7      +2.0D+00*(RR-2.0D+00*QB)/(SQRT((RR-2.0D+00*QB)**2+AQQ))**3
     8      +2.0D+00*(RR+2.0D+00*QB)/(SQRT((RR+2.0D+00*QB)**2+AQQ))**3
     9      -(4.0D+00*RR)/(SQRT(RR**2+AQQ))**3
      DXQXZ = 2.0D+00*(RR-QB)/(SQRT((RR-QB)**2+(DA-QB)**2+ADQ))**3
     1       -2.0D+00*(RR+QB)/(SQRT((RR+QB)**2+(DA-QB)**2+ADQ))**3
     2       -2.0D+00*(RR-QB)/(SQRT((RR-QB)**2+(DA+QB)**2+ADQ))**3
     3       +2.0D+00*(RR+QB)/(SQRT((RR+QB)**2+(DA+QB)**2+ADQ))**3
      QXZDX = 2.0D+00*(RR+QA)/(SQRT((RR+QA)**2+(QA-DB)**2+AQD))**3
     1       -2.0D+00*(RR-QA)/(SQRT((RR-QA)**2+(QA-DB)**2+AQD))**3
     2       -2.0D+00*(RR+QA)/(SQRT((RR+QA)**2+(QA+DB)**2+AQD))**3
     3       +2.0D+00*(RR-QA)/(SQRT((RR-QA)**2+(QA+DB)**2+AQD))**3
      QXZQXZ=-2.0D+00*(RR+QA-QB)/(SQRT((RR+QA-QB)**2+(QA-QB)**2+AQQ))**3
     1       +2.0D+00*(RR+QA+QB)/(SQRT((RR+QA+QB)**2+(QA-QB)**2+AQQ))**3
     2       +2.0D+00*(RR-QA-QB)/(SQRT((RR-QA-QB)**2+(QA-QB)**2+AQQ))**3
     3       -2.0D+00*(RR-QA+QB)/(SQRT((RR-QA+QB)**2+(QA-QB)**2+AQQ))**3
     4       +2.0D+00*(RR+QA-QB)/(SQRT((RR+QA-QB)**2+(QA+QB)**2+AQQ))**3
     5       -2.0D+00*(RR+QA+QB)/(SQRT((RR+QA+QB)**2+(QA+QB)**2+AQQ))**3
     6       -2.0D+00*(RR-QA-QB)/(SQRT((RR-QA-QB)**2+(QA+QB)**2+AQQ))**3
     7       +2.0D+00*(RR-QA+QB)/(SQRT((RR-QA+QB)**2+(QA+QB)**2+AQQ))**3
      DG(6)=(TERM*DZDZ)/4.0D+00
      DG(7)=(TERM*DXDX)/4.0D+00
      DG(8)=-TERM*(EDZ/2.0D+00+QZZDZ/8.0D+00)
      DG(9)=-TERM*(EDZ/2.0D+00+QXXDZ/8.0D+00)
      DG(10)=-(TERM*QXZDX)/8.0D+00
      DG(13)=-TERM*(DZE/2.0D+00+DZQZZ/8.0D+00)
      DG(14)=-TERM*(DZE/2.0D+00+DZQXX/8.0D+00)
      DG(15)=-(TERM*DXQXZ)/8.0D+00
      DG(16)=TERM*(EE+EQZZ/4.0D+00+QZZE/4.0D+00+QZZQZZ/16.0D+00)
      DG(17)=TERM*(EE+EQZZ/4.0D+00+QXXE/4.0D+00+QXXQZZ/16.0D+00)
      DG(18)=TERM*(EE+EQXX/4.0D+00+QZZE/4.0D+00+QZZQXX/16.0D+00)
      DG(19)=TERM*(EE+EQXX/4.0D+00+QXXE/4.0D+00+QXXQXX/16.0D+00)
      DG(20)=(TERM*QXZQXZ)/16.0D+00
      DG(21)=TERM*(EE+EQXX/4.0D+00+QXXE/4.0D+00+QXXQYY/16.0D+00)
      DG(22)=TERM*(QXXQXX-QXXQYY)/32.0D+00
      RETURN
      END
C*MODULE MPCGRD  *DECK DERS
      SUBROUTINE DERS(M,N,RR,DEL1,DEL2,DEL3,IS,IOL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C                                                                      *
C    ON INPUT M    = INDEX OF FIRST ATOMIC ORBITAL                     *
C             N    = INDEX OF SECOND ATOMIC ORBITAL                    *
C             RR   = SQUARE IF INTERATOMIC DIATANCE (IN BOHR)          *
C             DEL1 = CATERSIAN DISTANCE IN DERIVATIVE DIRECTION        *
C             DEL2 = CARTESIAN DISTANCE IN M A.O.'S DIRECTION          *
C             DEL3 = CARTESIAN DISTANCE IN N A.O.'S DIRECTION          *
C             IS   = INDICATES TYPE OF A.O.-A.O. INTERACTION           *
C                  = 1 S/S, 2 S/P', 3 S/P, 4 P'/S, 5 P/S, 6 P/P',      *
C                    7 P'/P", 8 P'P', 9 P/P                            *
C             IOL  = INDEX FOR STORING DERIVATIVES IN DS               *
C                                                                      *
      COMMON /DERIVS/ DS(16),DG(22),DR(100),TDX(3),TDY(3),TDZ(3)
      COMMON /TEMP/  CG(60,6),ZG(60,6)
      DIMENSION SS(6,6)
      A0=0.529167D+00
      DO 110 I=1,6
         DO 110 J=1,6
            SS(I,J)=0.0D+00
            APB=ZG(M,I)*ZG(N,J)
            AMB=ZG(M,I)+ZG(N,J)
            ADB=APB/AMB
            ADR=MIN(ADB*RR,35.0D+00)
            GO TO (10,20,30,40,50,60,70,80,90),IS
   10       ABN=-2.0D+00*ADB*DEL1/(A0**2)
            GO TO 100
   20       ABN=-4.0D+00*(ADB**2)*DEL1*DEL2/(SQRT(ZG(N,J))*(A0**3))
            GO TO 100
   30       ABN=(2.0D+00*ADB/(SQRT(ZG(N,J))*A0))*
     1 (1.0D+00-2.0D+00*ADB*(DEL1**2)/(A0**2))
            GO TO 100
   40       ABN=4.0D+00*(ADB**2)*DEL1*DEL2/(SQRT(ZG(M,I))*(A0**3))
            GO TO 100
   50       ABN=-(2.0D+00*ADB/(SQRT(ZG(M,I))*A0))*
     1 (1.0D+00-2.0D+00*ADB*(DEL1**2)/(A0**2))
            GO TO 100
   60       ABN=-(4.0D+00*(ADB**2)*DEL2/(SQRT(APB)*(A0**2)))*
     1 (1.0D+00-2.0D+00*ADB*(DEL1**2)/(A0**2))
            GO TO 100
   70       ABN=8.0D+00*(ADB**3)*DEL1*DEL2*DEL3/(SQRT(APB)*(A0**4))
            GO TO 100
   80       ABN=-(8.0D+00*(ADB**2)*DEL1/(SQRT(APB)*(A0**2)))*
     1 (0.5D+00-ADB*(DEL2**2)/(A0**2))
            GO TO 100
   90       ABN=-(8.0D+00*(ADB**2)*DEL1/(SQRT(APB)*(A0**2)))*
     1 (1.5D+00-ADB*(DEL1**2)/(A0**2))
  100       SS(I,J)=SQRT((2.0D+00*SQRT(APB)/AMB)**3)*EXP(-ADR)*ABN
  110 CONTINUE
      DO 120 I=1,6
         DO 120 J=1,6
            DS(IOL)=DS(IOL)+SS(I,J)*CG(M,I)*CG(N,J)
  120 CONTINUE
      RETURN
      END
C*MODULE MPCGRD  *DECK DHC
      SUBROUTINE DHC(P,PA,PB,XI,NAT,IF,IM,IL,JF,JM,JL,DENER)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL UHFFLG
C
      DIMENSION P(*),PA(*),PB(*),XI(3,*),NAT(*)
      DIMENSION NFIRST(2),NMIDLE(2),NLAST(2),H(171),SHMAT(9,9),F(171),
     *          E1B(10),E2A(10),W(100),PTOT2(2,16)
C
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      PARAMETER (ONE=1.0D+00, HALF=0.5D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA UHF,ROHF,GVB /8HUHF     ,8HROHF    ,8HGVB     /
#else
      CHARACTER*8 :: GVB_STR
      EQUIVALENCE (GVB, GVB_STR)
      CHARACTER*8 :: UHF_STR
      EQUIVALENCE (UHF, UHF_STR)
      CHARACTER*8 :: ROHF_STR
      EQUIVALENCE (ROHF, ROHF_STR)
      DATA UHF_STR,ROHF_STR,GVB_STR/"UHF     ","ROHF    ","GVB     "/
#endif
C
C  DHC CALCULATES THE ENERGY CONTRIBUTIONS FROM THOSE PAIRS OF ATOMS
C         THAT HAVE BEEN MOVED BY SUBROUTINE DERIV.
C
      UHFFLG = SCFTYP.EQ.UHF  .OR.  SCFTYP.EQ.ROHF
      IF(UHFFLG) THEN
          HFCO=-ONE
      ELSE
          IF(SCFTYP.EQ.GVB) THEN
              HFCO=ONE
          ELSE
              HFCO=-HALF
          ENDIF
      ENDIF
      NFIRST(1)=1
      NMIDLE(1)=IM-IF+1
      NLAST(1)=IL-IF+1
      NFIRST(2)=NLAST(1)+1
      NMIDLE(2)=NFIRST(2)+JM-JF
      NLAST(2)=NFIRST(2)+JL-JF
      LINEAR=(NLAST(2)*(NLAST(2)+1))/2
      DO 10 I=1,LINEAR
         F(I)=0.0D+00
   10 H(I)=0.0D+00
      DO 20 I=1,LINEAR
   20 F(I)=H(I)
      JA=NFIRST(2)
      JB=NLAST(2)
      JC=NMIDLE(2)
      IA=NFIRST(1)
      IB=NLAST(1)
      IC=NMIDLE(1)
      J=2
      I=1
      NJ=NAT(2)
      NI=NAT(1)
      CALL H1ELEC(NI,NJ,XI(1,1),XI(1,2),SHMAT)
      IF(NAT(1).EQ.102.OR.NAT(2).EQ.102) THEN
         K=(JB*(JB+1))/2
         DO 30 J=1,K
   30    H(J)=0.0D+00
      ELSE
         J1=0
         DO 40 J=JA,JB
            JJ=J*(J-1)/2
            J1=J1+1
            I1=0
            DO 40 I=IA,IB
               JJ=JJ+1
               I1=I1+1
               H(JJ)=SHMAT(I1,J1)
               F(JJ)=SHMAT(I1,J1)
   40    CONTINUE
      ENDIF
      KR=1
      CALL ROTATE(NJ,NI,XI(1,2),XI(1,1),W(KR),KR,E2A,E1B,ENUCLR,
     *            100.0D+00)
C
C    * ENUCLR IS SUMMED OVER CORE-CORE REPULSION INTEGRALS.
C
      I2=0
      DO 70 I1=IA,IC
         II=I1*(I1-1)/2+IA-1
         DO 60 J1=IA,I1
            II=II+1
            I2=I2+1
            H(II)=H(II)+E1B(I2)
            F(II)=F(II)+E1B(I2)
   60    CONTINUE
   70 CONTINUE
      DO  80 I1=IC+1,IB
         II=(I1*(I1+1))/2
         F(II)=F(II)+E1B(1)
         H(II)=H(II)+E1B(1)
   80 CONTINUE
      I2=0
      DO 90 I1=JA,JC
         II=I1*(I1-1)/2+JA-1
         DO 90 J1=JA,I1
            II=II+1
            I2=I2+1
            H(II)=H(II)+E2A(I2)
   90 F(II)=F(II)+E2A(I2)
      DO 100 I1=JC+1,JB
         II=(I1*(I1+1))/2
         F(II)=F(II)+E2A(1)
         H(II)=H(II)+E2A(1)
  100 CONTINUE
      IF(UHFFLG) THEN
          CALL FOCK2(F,F,P,PA,PTOT2,W,2,NFIRST,NMIDLE,NLAST,HFCO)
      ELSE
          CALL FOCK2(F,F,P,P,PTOT2,W,2,NFIRST,NMIDLE,NLAST,HFCO)
      ENDIF
      EE=HELECT(NLAST(2),PA,H,F)
C
      IF(UHFFLG) THEN
         DO 110 I=1,LINEAR
  110    F(I)=H(I)
         CALL FOCK2(F,F,P,PB,PTOT2,W,2,NFIRST,NMIDLE,NLAST,HFCO)
         EE=EE+HELECT(NLAST(2),PB,H,F)
      ELSE
         EE=EE+EE
      ENDIF
      DENER=EE+ENUCLR
      RETURN
      END
C*MODULE MPCGRD  *DECK HELECT
      FUNCTION HELECT(N,P,H,F)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P(*), H(*), F(*)
C***********************************************************************
C
C    SUBROUTINE CALCULATES THE ELECTRONIC ENERGY OF THE SYSTEM IN EV.
C
C    ON ENTRY N = NUMBER OF ATOMIC ORBITALS.
C             P = DENSITY MATRIX, PACKED, LOWER TRIANGLE.
C             H = ONE-ELECTRON MATRIX, PACKED, LOWER TRIANGLE.
C             F = TWO-ELECTRON MATRIX, PACKED, LOWER TRIANGLE.
C    ON EXIT
C        HELECT = ELECTRONIC ENERGY.
C
C    NO ARGUMENTS ARE CHANGED.
C
C***********************************************************************
      ED=0.0D+00
      EE=0.0D+00
      K=0
      NN=N+1
      DO 20 I=2,NN
         K=K+1
         JJ=I-1
         ED=ED+P(K)*(H(K)+F(K))
         IF (I.EQ.NN) GO TO 20
         DO 10 J=1,JJ
            K=K+1
   10    EE=EE+P(K)*(H(K)+F(K))
   20 CONTINUE
      EE=EE+0.5D+00*ED
      HELECT=EE
      RETURN
C
      END
C*MODULE MPCGRD  *DECK MPCGRD
      SUBROUTINE MPCGRD(SCFTYP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL BETA
C
      PARAMETER (MXATM=500, HALF=0.50D+00)
C
      COMMON /FMCOM / X(1)
      COMMON /FUNCT / E,EG(3,MXATM)
      COMMON /MPCGEO/ GEO(3,MXATM)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA UHF,ROHF,CHECK/8HUHF     ,8HROHF    ,8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      CHARACTER*8 :: UHF_STR
      EQUIVALENCE (UHF, UHF_STR)
      CHARACTER*8 :: ROHF_STR
      EQUIVALENCE (ROHF, ROHF_STR)
      DATA UHF_STR,ROHF_STR,CHECK_STR/"UHF     ","ROHF    ","CHECK   "/
#endif
C
      BETA = SCFTYP.EQ.UHF  .OR.  SCFTYP.EQ.ROHF
C
      L1 = NUM
      L2 = (L1*(L1+1))/2
      CALL VALFM(LOADFM)
C
      LP      =  1     + LOADFM
      LPA     =  LP    + L2
      LPB     =  LPA   + L2
      LAST    =  LPB   + L2 + 1
C
      NEED = LAST - LP
      IF(EXETYP.EQ.CHECK)GO TO 900
      CALL GETFM(NEED)
C
      CALL VCLR(X(LP),1,L2)
      CALL VCLR(X(LPA),1,L2)
      CALL VCLR(X(LPB),1,L2)
C
      IF(BETA) THEN
C
C     ----- ALPHA + BETA DENSITY = TOTAL DENSITY -----
C
         CALL DAREAD(IDAF,IODA,X(LPA),L2,16,0)
         CALL DAREAD(IDAF,IODA,X(LPB),L2,20,0)
         CALL VADD(X(LPA),1,X(LPB),1,X(LP),1,L2)
      ELSE
C
C     ----- ALPHA AND BETA DENSITY = 0.5 TOTAL DENSITY -----
C
         CALL DAREAD(IDAF,IODA,X(LP),L2,16,0)
         CALL DCOPY(L2,X(LP),1,X(LPA),1)
         CALL DSCAL(L2,HALF,X(LPA),1)
         CALL DCOPY(L2,X(LPA),1,X(LPB),1)
      ENDIF
C
      CALL SETUPG
      CALL DCART(GEO,EG,X(LP),X(LPA),X(LPB),L2)
C
      CALL RETFM(NEED)
C
  900 CONTINUE
      RETURN
      END
C*MODULE MPCGRD  *DECK ROTAT
      SUBROUTINE ROTAT(COORD,I,J,IX,RIJ,DEL1,IDX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /DERIVS/ DS(16),DG(22),DR(100),TDX(3),TDY(3),TDZ(3)
      COMMON /EXTRA/  G(22),TX(3),TY(3),TZ(3)
      DIMENSION COORD(3,25)
      XD=COORD(1,I)-COORD(1,J)
      YD=COORD(2,I)-COORD(2,J)
      ZD=COORD(3,I)-COORD(3,J)
      RXY=SQRT(XD*XD+YD*YD)
      RYZ=SQRT(YD*YD+ZD*ZD)
      RZX=SQRT(ZD*ZD+XD*XD)
      DO 10 IJK=1,3
         TX(IJK)=0.0D+00
         TY(IJK)=0.0D+00
         TZ(IJK)=0.0D+00
         TDX(IJK)=0.0D+00
         TDY(IJK)=0.0D+00
         TDZ(IJK)=0.0D+00
   10 CONTINUE
      IF(RXY.LT.1.0D-04) THEN
C   MOLECULAR Z AXIS IS PARALLEL TO DIATOMIC Z AXIS
         TX(3)=1.0D+00
         IF(ZD.LT.0.0D+00) TX(3)=-1.0D+00
         TY(2)=1.0D+00
         TZ(1)=TX(3)
         IF(IDX.EQ.1) RETURN
         IF(IX.EQ.1) TDX(1)=1.0D+00/RIJ
         IF(IX.EQ.2) TDX(2)=1.0D+00/RIJ
         IF(IX.EQ.1) TDZ(3)=-1.0D+00/RIJ
         IF(IX.EQ.2) TDY(3)=-TX(3)/RIJ
      ELSEIF(RYZ.LT.1.0D-04) THEN
C   MOLECULAR X AXIS IS PARALLEL TO DIATOMIC Z AXIS
         TX(1)=1.0D+00
         IF(XD.LT.0.0D+00) TX(1)=-1.0D+00
         TY(2)=TX(1)
         TZ(3)=1.0D+00
         IF(IDX.EQ.1) RETURN
         IF(IX.EQ.2) TDX(2)=1.0D+00/RIJ
         IF(IX.EQ.3) TDX(3)=1.0D+00/RIJ
         IF(IX.EQ.2) TDY(1)=-1.0D+00/RIJ
         IF(IX.EQ.3) TDZ(1)=-TX(1)/RIJ
      ELSEIF(RZX.LT.1.0D-04) THEN
C   MOLECULAR Y AXIS IS PARALLEL TO DIATOMIC Z AXIS
         TX(2)=1.0D+00
         IF(YD.LT.0.0D+00) TX(2)=-1.0D+00
         TY(1)=-TX(2)
         TZ(3)=1.0D+00
         IF(IDX.EQ.1) RETURN
         IF(IX.EQ.1) TDX(1)=1.0D+00/RIJ
         IF(IX.EQ.3) TDX(3)=1.0D+00/RIJ
         IF(IX.EQ.1) TDY(2)=1.0D+00/RIJ
         IF(IX.EQ.3) TDZ(2)=-TX(2)/RIJ
      ELSE
         TX(1)=XD/RIJ
         TX(2)=YD/RIJ
         TX(3)=ZD/RIJ
         TZ(3)=RXY/RIJ
         TY(1)=-TX(2)*SIGN(+1.0D+00,TX(1))/TZ(3)
         TY(2)=ABS(TX(1)/TZ(3))
         TY(3)=0.0D+00
         TZ(1)=-TX(1)*TX(3)/TZ(3)
         TZ(2)=-TX(2)*TX(3)/TZ(3)
         IF(IDX.EQ.1) RETURN
         TERM=DEL1/(RIJ*RIJ)
         IF(IX.EQ.1)THEN
            TDX(1)=1.0D+00/RIJ-TX(1)*TERM
            TDX(2)=-TX(2)*TERM
            TDX(3)=-TX(3)*TERM
            TDZ(3)=TX(1)/RXY-TZ(3)*TERM
         ELSEIF(IX.EQ.2) THEN
            TDX(1)=-TX(1)*TERM
            TDX(2)=1.0D+00/RIJ-TX(2)*TERM
            TDX(3)=-TX(3)*TERM
            TDZ(3)=TX(2)/RXY-TZ(3)*TERM
         ELSEIF(IX.EQ.3)THEN
            TDX(1)=-TX(1)*TERM
            TDX(2)=-TX(2)*TERM
            TDX(3)=1.0D+00/RIJ-TX(3)*TERM
            TDZ(3)=-TZ(3)*TERM
         ENDIF
         TDY(1)=-TDX(2)/TZ(3)+TX(2)*TDZ(3)/TZ(3)**2
         IF(TX(1).LT.0.0D+00) TDY(1)=-TDY(1)
         TDY(2)=TDX(1)/TZ(3)-TX(1)*TDZ(3)/TZ(3)**2
         IF(TX(1).LT.0.0D+00) TDY(2)=-TDY(2)
         TDY(3)=0.0D+00
         TDZ(1)=-TX(3)*TDX(1)/TZ(3)-TX(1)*TDX(3)/TZ(3)
     1 +TX(1)*TX(3)*TDZ(3)/TZ(3)**2
         TDZ(2)=-TX(3)*TDX(2)/TZ(3)-TX(2)*TDX(3)/TZ(3)
     1 +TX(2)*TX(3)*TDZ(3)/TZ(3)**2
      ENDIF
      RETURN
      END
C*MODULE MPCGRD  *DECK DIHED
      SUBROUTINE DIHED(COORD,IP1,IP2,IP3,IP4,ANGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION COORD(3,*)
C
C     COMPUTE THE DIHEDRAL ANGLE FORMED BY POINTS IP1, IP2, IP3 AND IP4
C     BY H.L.
C
C     COMPUTE THE COEFFICIENTS FOR PLANE ONE FORMED BY IP1, IP2 AND IP3
C
      X1=COORD(1,IP1)-COORD(1,IP2)
      Y1=COORD(2,IP1)-COORD(2,IP2)
      Z1=COORD(3,IP1)-COORD(3,IP2)
      X2=COORD(1,IP3)-COORD(1,IP2)
      Y2=COORD(2,IP3)-COORD(2,IP2)
      Z2=COORD(3,IP3)-COORD(3,IP2)
C
      A1=Y1*Z2-Y2*Z1
      B1=-(X1*Z2-X2*Z1)
      C1=X1*Y2-X2*Y1
C
C     COMPUTE THE COEFFICIENTS FOR PLANE TWO FORMED BY P2, P3 AND P4
C
      X1=COORD(1,IP2)-COORD(1,IP3)
      Y1=COORD(2,IP2)-COORD(2,IP3)
      Z1=COORD(3,IP2)-COORD(3,IP3)
      X2=COORD(1,IP4)-COORD(1,IP3)
      Y2=COORD(2,IP4)-COORD(2,IP3)
      Z2=COORD(3,IP4)-COORD(3,IP3)
C
      A2=Y1*Z2-Y2*Z1
      B2=-(X1*Z2-X2*Z1)
      C2=X1*Y2-X2*Y1
C
C     FINALLY COMPUTE THE DIHEDRAL ANGLE BETWEEN THE TWO PLANES
C
      ANGLE=A1*A2+B1*B2+C1*C2
      ANGLE=ANGLE/SQRT(A1**2+B1**2+C1**2)/SQRT(A2**2+B2**2+C2**2)
      IF(ANGLE.GT.1.0D+00)ANGLE=1.0D+00
      IF(ANGLE.LT.-1.0D+00)ANGLE=-1.0D+00
      ANGLE=ACOS(ANGLE)
C
C     NOTE: FOR MMOK USAGE THE SIGN OF THE DIHEDRAL ANGLE IS
C           NOT IMPORTANT, SINCE MMOK ONLY NEEDS SIN(ANGLE)**2
C
      RETURN
      END
