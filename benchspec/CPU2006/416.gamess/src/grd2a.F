C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C  4 NOV 03 - TJP - JKDMEM:  VECTOR MACHINE SPECIFIC PIPELINE LENGTH
C  3 SEP 03 - SPW - JKDER: CHANGES FOR CIS GRADIENT; ADDED DABCIS
C  3 JUL 03 - JMM - SUPPRESS PRINTING FOR MONTE CARLO JOBS
C 14 JAN 03 - JI  - HOOKS TO ORMAS CI STEP
C 12 DEC 02 - CMA - SYNCH UP MP2PAR COMMON
C  7 AUG 02 - JMS - HIGHER PERFORMANCE SP GRADIENT
C 20 JUN 02 - YA  - IMPROVED FINE SCHWARZ SCREENING
C 22 MAY 02 - MWS - JKDMC: ONLY MASTER NODE READS DM2, FIX GENCI'S GRAD
C 17 APR 02 - MWS - SYNCH UP FRGINF COMMON
C 26 MAR 02 - MWS - USE NEW CODE TO PROJECT T+R FROM G+GF DFT GRADIENTS,
C                   GRIDFREE UNROT DELETED, T PROJ REMOVED FROM DFTGRD
C 24 JAN 02 - CMA - CHANGES TO JKDER TO ALLOW UMP2, NEW DABUMP
C  1 AUG 01 - MWS - JKDER: ADJUSTMENTS FOR VERY BIG EXPONENTS
C 27 JUN 01 - JI  - TWEAKS FOR GENERAL CI
C 25 JUN 01 - MWS - ALTER COMMON BLOCK WFNOPT
C 13 JUN 01 - TT,SY,MK,DGF - JKDER: ADD CALL FOR GRID DFT GRADIENT
C 20 FEB 01 - MWS - PAD PAULMO COMMON
C 11 OCT 00 - MWS - UPDATE THE DETWFN COMMON
C 25 MAR 00 - KRG - UNROT: CORRECTIONS FOR DFT GRADIENT
C 16 FEB 00 - MWS - JKDER: ZERO INITIAL MCSCF DENSITY ARRAYS
C 21 DEC 99 - MWS - MAKE GUGWFN COMMON CONSISTENT
C  6 JUN 99 - KRG - DABDFT: DOUBLE STAR SHOULD BE MULTIPLY
C  9 APR 99 - KRG - JKDER: DFT MODS; DFTDAB,DFTGRD,UNROT: NEW DFT CODE
C 12 NOV 98 - GDF - CHANGE BIT PACKING TO ISHIFT
C 13 APR 98 - MWS - JKDER: SET ACTIVE SPACE FOR DETERMINANT CI
C 28 SEP 97 - MWS - CONVERT PARALLEL CALLS FROM TCGMSG TO DDI
C 19 FEB 97 - MWS - JKDMEM: FIX DM2 SHELL BUFFER SIZE FOR S+P+SP BASES
C  8 JAN 97 - GMC - JKDMC: CHANGES FOR DROPPING MCSCF CORES
C 18 DEC 96 - JHJ - JKDER: CALL TO DABPAU
C 17 OCT 96 - MWS - DFINAL: REMOVE CALL TO EGOUT
C 11 JUL 96 - MWS - UPDATE COMMON BLOCK MP2PAR.
C 13 JUN 96 - SPW - DABMP2,JKDER: CHANGES FOR MP2 FROZEN CORE GRADIENT
C 18 APR 96 - GMC - JKDMC: CHANGE CORE COUNT FOR FOCAS OPTION
C  5 MAR 96 - MWS - CHANGE SHELL SYMMETRY PACKING COMMONS
C 11 NOV 95 - SPW - JKDER: MAKE CHANGES FOR MP2 GRADIENTS
C 26 JUL 95 - MWS - JKDER: INCLUDE NEXT TYPE AS OCCUPIED IN MCSCF
C  5 APR 95 - MWS - JKDMEM: SPELL IF00 AS IFXX FOR DEC F90 BUGLET
C 11 NOV 94 - MWS - REMOVE FTNCHEK WARNINGS
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C  5 AUG 94 - MWS - JKDMC: USE DOUBLE LABEL PACKING
C 20 JUL 94 - BMB - FINISHED CONVERSION FROM HONDO 8
C
C   THIS FILE CONTAINS THE MAIN DRIVER FOR THE TWO ELECTRON GRADIENT AS
C   WELL AS MANY ROUTINES WHICH ARE SHARED BY THE POPLE PACKAGE AND THE
C   RYS POLYNOMIAL PACKAGE.  THE DRIVER IS BASED ON THE OLD GAMESS
C   DRIVER, BUT ALMOST EVERYTHING ELSE IS NEW FROM HONDO8.
C
C*MODULE GRD2A   *DECK DABCLU
      SUBROUTINE DABCLU(II,JJ,KK,LL,UHFTYP,DA,DB,DAB,DABMAX,Q4,POPLE)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION DA(*),DB(*),DAB(*)
      LOGICAL UHFTYP,POPLE
C
      PARAMETER (MXGTOT=5000, MXSH=1000, MXAO=2047)
C
      LOGICAL OUT,DBG
      COMMON /DSHLT / RTOL,DTOL,VTOL1,VTOL2,VTOLS,OUT,DBG
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /INDD80/ IMAX,JMAX,KKMAX,LMAX
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /SHLLMN/ IGXYZ(4,35),JGXYZ(4,35),KGXYZ(4,35),LGXYZ(4,35)
      COMMON /SHLNRM/ PNRM(35)
      LOGICAL IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
      COMMON /SHLEQU/ IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
C
CJMS  LABELLED COMMON GSPG80 DEFINED FOR COMPUTATIONAL EFFICIENCY.
CJMS  FOR SP BASES ONLY, IT CONTAINS THE E ARRAY WHICH IS THE DAB
CJMS  ARRAY WITH INDICES IN REVERSE ORDER: E(I,J,K,L)= DAB(L,K,J,I)
CJMS  AND IS USED IN SUB JKDG80 (MODULE GRD2B). IT ORIGINATES IN:
CJMS
CJMS     1. SUBS DABCLU, DABDFT, DABGVB, DABMC, DABMP2 AND DABUMP
CJMS        (MODULE GRD2A) AND SUB DABPAU (MODULE EFPAUL) WHICH ARE
CJMS        ALL CALLED BY SUB JKDER (MODULE GRD2A)
CJMS
CJMS     2. SUB DABCLU (MODULE GRD2A) WHICH IS CALLED BY SUB EFDEN OF
CJMS        MODULE EFGRD2
CJMS
CJMS     3. SUB PAR2PDM (MODULE MP2DDI) WHICH IS CALLED BY SUB PJKDMP2
CJMS        OF MODULE MP2DDI
C
      COMMON /GSPG80/ E(4,4,4,4)
C
      PARAMETER (ZER=0.0D+00, PT5=0.5D+00, P25=.25D+00, F04=4.0D+00)
C
C     ----- THIS ROUTINE FORMS THE PRODUCT OF DENSITY       -----
C           MATRICES FOR USE IN FORMING THE TWO ELECTRON
C           GRADIENT.  VALID FOR CLOSED AND OPEN SHELL SCF.
C
      DABMAX= ZER
      MINI= KMIN(II)
      MINJ= KMIN(JJ)
      MINK= KMIN(KK)
      MINL= KMIN(LL)
      LOCI= KLOC(II)-MINI
      LOCJ= KLOC(JJ)-MINJ
      LOCK= KLOC(KK)-MINK
      LOCL= KLOC(LL)-MINL
      IF(POPLE) THEN
         DO 110 L=1,LMAX
            DO 110 K=1,KKMAX
               KAL= MAX0(LOCK+K,LOCL+L)
               KIL= MIN0(LOCK+K,LOCL+L)
               DO 110 J=1,JMAX
                  DO 110 I=1,IMAX
                     IAJ= MAX0(LOCI+I,LOCJ+J)
                     IIJ= MIN0(LOCI+I,LOCJ+J)
                     IN = IAJ
                     JN = IIJ
                     KN = KAL
                     LN = KIL
                     IF(IN.LT.KN .OR.(IN.EQ.KN .AND. JN.LT.LN)) THEN
                        IN = KAL
                        JN = KIL
                        KN = IAJ
                        LN = IIJ
                     ENDIF
C        -----  NOW CALCULATE DENSITY MATRIX CONTRIBUTION.      -----
C        -----  (IJ/KL),(IJ/IL),(IJ/JL),(IJ/KJ)                 -----
C        -----  EIGHT DISTINCT INTEGRALS                        -----
C        -----  CONTRIBUTION TO THE ENERGY                      -----
C        -----  4(IJ)(KL) - (IK)(JL) - (JK)(IL)                 -----
                     IJ = IA(IN)+JN
                     IK = IA(IN)+KN
                     IL = IA(IN)+LN
                     JK = IA(MAX0(JN,KN))+MIN0(JN,KN)
                     JL = IA(JN)+LN
                     IF(JN.LT.KN) JL = IA(MAX0(JN,LN))+MIN0(JN,LN)
                     KL = IA(KN)+LN
                     DF1= DA(IJ)*DA(KL)
                     DQ1= DA(IK)*DA(JL)+DA(IL)*DA(JK)
                     IF(UHFTYP) DQ1=DQ1+DB(IK)*DB(JL)+DB(IL)*DB(JK)
                     DF1=(DF1-P25*DQ1)*Q4
                     IF(DABMAX.LT. ABS(DF1)) DABMAX= ABS(DF1)
                     E(I,J,K,L)= DF1
                     IF(OUT) WRITE(IW,9010) II,JJ,KK,LL,I,J,K,L,DF1
  110    CONTINUE
      ELSE
C
C D AND HIGHER FUNCTIONS OR HONDO ONLY RUNS
C
         MAXI= KMAX(II)
         MAXJ= KMAX(JJ)
         MAXK= KMAX(KK)
         MAXL= KMAX(LL)
         DO 210 I=MINI,MAXI
            P1I = PNRM(I)
            JMAX= MAXJ
            IF(IIEQJJ) JMAX= I
            DO 210 J=MINJ,JMAX
               P2J = P1I*PNRM(J)
               IAJ= MAX0(LOCI+I,LOCJ+J)
               IIJ= MIN0(LOCI+I,LOCJ+J)
               KMMAX=MAXK
               IF(IJEQKL) KMMAX= I
               DO 210 K=MINK,KMMAX
                  P3K = P2J*PNRM(K)
                  LMAX= MAXL
                  IF(KKEQLL) LMAX= K
                  IF(IJEQKL .AND. K.EQ.I) LMAX= J
                  DO 210 L=MINL,LMAX
                     P4L= P3K*PNRM(L)
                     KAL= MAX0(LOCK+K,LOCL+L)
                     KIL= MIN0(LOCK+K,LOCL+L)
                     IN = IAJ
                     JN = IIJ
                     KN = KAL
                     LN = KIL
                     IF(IN.LT.KN .OR.(IN.EQ.KN .AND. JN.LT.LN)) THEN
                        IN = KAL
                        JN = KIL
                        KN = IAJ
                        LN = IIJ
                     ENDIF
                     IJ = IA(IN)+JN
                     IK = IA(IN)+KN
                     IL = IA(IN)+LN
                     JK = IA(MAX0(JN,KN))+MIN0(JN,KN)
                     JL = IA(JN)+LN
                     IF(JN.LT.KN) JL = IA(MAX0(JN,LN))+MIN0(JN,LN)
                     KL = IA(KN)+LN
                     DF1= DA(IJ)*DA(KL)
                     DQ1= DA(IK)*DA(JL)+DA(IL)*DA(JK)
                     IF(UHFTYP) DQ1=DQ1+DB(IK)*DB(JL)+DB(IL)*DB(JK)
                     DF1= DF1*F04-DQ1
                     IF(JN.EQ.IN               ) DF1= DF1*PT5
                     IF(LN.EQ.KN               ) DF1= DF1*PT5
                     IF(KN.EQ.IN .AND. LN.EQ.JN) DF1= DF1*PT5
                     IF(DABMAX.LT. ABS(DF1)) DABMAX= ABS(DF1)
C
C IGXYZ AND J, K, AND L ARE SET UP IN JKDNDX
C
                     IJKL=IGXYZ(1,I)+JGXYZ(1,J)+KGXYZ(1,K)+LGXYZ(1,L)
                     DAB(IJKL)= DF1*P4L
                     IF(OUT) WRITE(IW,9020) II,JJ,KK,LL,I,J,K,L,IJKL,DF1
  210    CONTINUE
      ENDIF
      RETURN
 9010 FORMAT(' -DABCLU,POPLE- ',4I4,4I3,D20.12)
 9020 FORMAT(' -DABCLU,HONDO- ',4I4,4I3,I5,D20.12)
      END
C*MODULE GRD2A   *DECK DABDFT
      SUBROUTINE DABDFT(II,JJ,KK,LL,UHFTYP,DA,DB,DAB,DABMAX,Q4,POPLE)
C
C     ----- THIS ROUTINE FORMS THE PRODUCT OF DENSITY       -----
C           MATRICES FOR USE IN FORMING THE TWO ELECTRON
C           GRADIENT.  VALID FOR CLOSED AND OPEN SHELL SCF.
C           THE PORTIONS OF THE DENSITY MATRIX THAT CORRESPOND TO
C           HARTREE-FOCK EXCHANGE ARE "MODIFIED" TO WORK WITH
C           DFT.  OFTEN THIS MEANS THAT THEY ARE SET TO ZERO
C     NOTE THAT THIS ROUTINE IS A CLOSE COUSIN TO -DFTCLU- ROUTINE
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION DA(*),DB(*),DAB(*)
      LOGICAL UHFTYP,POPLE
C
      PARAMETER (MXGTOT=5000, MXSH=1000, MXAO=2047)
C
      COMMON /DFTPAR/ DFTTYP(20),EXENA,EXENB,EXENC,IDFT34,NAUXFUN,
     *                                                    NAUXSHL
      LOGICAL OUT,DBG
      COMMON /DSHLT / RTOL,DTOL,VTOL1,VTOL2,VTOLS,OUT,DBG
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /INDD80/ IMAX,JMAX,KKMAX,LMAX
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /SHLLMN/ IGXYZ(4,35),JGXYZ(4,35),KGXYZ(4,35),LGXYZ(4,35)
      COMMON /SHLNRM/ PNRM(35)
      LOGICAL IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
      COMMON /SHLEQU/ IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
C
CJMS  LABELLED COMMON GSPG80 DEFINED FOR COMPUTATIONAL EFFICIENCY.
CJMS  FOR SP BASES ONLY, IT CONTAINS THE E ARRAY WHICH IS THE DAB
CJMS  ARRAY WITH INDICES IN REVERSE ORDER: E(I,J,K,L)= DAB(L,K,J,I)
CJMS  AND IS USED IN SUB JKDG80 (MODULE GRD2B). IT ORIGINATES IN:
CJMS
CJMS     1. SUBS DABCLU, DABDFT, DABGVB, DABMC, DABMP2 AND DABUMP
CJMS        (MODULE GRD2A) AND SUB DABPAU (MODULE EFPAUL) WHICH ARE
CJMS        ALL CALLED BY SUB JKDER (MODULE GRD2A)
CJMS
CJMS     2. SUB DABCLU (MODULE GRD2A) WHICH IS CALLED BY SUB EFDEN OF
CJMS        MODULE EFGRD2
CJMS
CJMS     3. SUB PAR2PDM (MODULE MP2DDI) WHICH IS CALLED BY SUB PJKDMP2
CJMS        OF MODULE MP2DDI
C
      COMMON /GSPG80/ E(4,4,4,4)
C
      PARAMETER (ZER=0.0D+00, PT5=0.5D+00, P25=.25D+00, F04=4.0D+00)
C
      QFS= DFTTYP(3)*P25
C
      DABMAX= ZER
      MINI= KMIN(II)
      MINJ= KMIN(JJ)
      MINK= KMIN(KK)
      MINL= KMIN(LL)
      LOCI= KLOC(II)-MINI
      LOCJ= KLOC(JJ)-MINJ
      LOCK= KLOC(KK)-MINK
      LOCL= KLOC(LL)-MINL
      IF(POPLE) THEN
         DO 110 L=1,LMAX
            DO 110 K=1,KKMAX
               KAL= MAX0(LOCK+K,LOCL+L)
               KIL= MIN0(LOCK+K,LOCL+L)
               DO 110 J=1,JMAX
                  DO 110 I=1,IMAX
                     IAJ= MAX0(LOCI+I,LOCJ+J)
                     IIJ= MIN0(LOCI+I,LOCJ+J)
                     IN = IAJ
                     JN = IIJ
                     KN = KAL
                     LN = KIL
                     IF(IN.LT.KN .OR.(IN.EQ.KN .AND. JN.LT.LN)) THEN
                        IN = KAL
                        JN = KIL
                        KN = IAJ
                        LN = IIJ
                     ENDIF
C        -----  NOW CALCULATE DENSITY MATRIX CONTRIBUTION.      -----
C        -----  (IJ/KL),(IJ/IL),(IJ/JL),(IJ/KJ)                 -----
C        -----  EIGHT DISTINCT INTEGRALS                        -----
C        -----  CONTRIBUTION TO THE ENERGY                      -----
C        -----  4(IJ)(KL) - (IK)(JL) - (JK)(IL)                 -----
                     IJ = IA(IN)+JN
                     KL = IA(KN)+LN
                     DF1= DA(IJ)*DA(KL)
                     IF(QFS.NE.ZER) THEN
                        IK = IA(IN)+KN
                        IL = IA(IN)+LN
                        JK = IA(MAX0(JN,KN))+MIN0(JN,KN)
                        JL = IA(JN)+LN
                        IF(JN.LT.KN) JL = IA(MAX0(JN,LN))+MIN0(JN,LN)
                        DQ1= DA(IK)*DA(JL)+DA(IL)*DA(JK)
                        IF(UHFTYP) DQ1=DQ1+DB(IK)*DB(JL)+DB(IL)*DB(JK)
                        DF1= DF1-QFS*DQ1
                     ENDIF
                     DF1= DF1*Q4
                     IF(DABMAX.LT. ABS(DF1)) DABMAX= ABS(DF1)
                     E(I,J,K,L)= DF1
                     IF(OUT) WRITE(IW,9010) II,JJ,KK,LL,I,J,K,L,DF1
  110    CONTINUE
      ELSE
C
C D AND HIGHER FUNCTIONS OR HONDO ONLY RUNS
C
         MAXI= KMAX(II)
         MAXJ= KMAX(JJ)
         MAXK= KMAX(KK)
         MAXL= KMAX(LL)
         DO 210 I=MINI,MAXI
            P1I = PNRM(I)
            JMAX= MAXJ
            IF(IIEQJJ) JMAX= I
            DO 210 J=MINJ,JMAX
               P2J = P1I*PNRM(J)
               IAJ= MAX0(LOCI+I,LOCJ+J)
               IIJ= MIN0(LOCI+I,LOCJ+J)
               KMMAX=MAXK
               IF(IJEQKL) KMMAX= I
               DO 210 K=MINK,KMMAX
                  P3K = P2J*PNRM(K)
                  LMAX= MAXL
                  IF(KKEQLL) LMAX= K
                  IF(IJEQKL .AND. K.EQ.I) LMAX= J
                  DO 210 L=MINL,LMAX
                     P4L= P3K*PNRM(L)
                     KAL= MAX0(LOCK+K,LOCL+L)
                     KIL= MIN0(LOCK+K,LOCL+L)
                     IN = IAJ
                     JN = IIJ
                     KN = KAL
                     LN = KIL
                     IF(IN.LT.KN .OR.(IN.EQ.KN .AND. JN.LT.LN)) THEN
                        IN = KAL
                        JN = KIL
                        KN = IAJ
                        LN = IIJ
                     ENDIF
                     IJ = IA(IN)+JN
                     KL = IA(KN)+LN
                     DF1= DA(IJ)*DA(KL)
                     IF(QFS.NE.ZER) THEN
                        IK = IA(IN)+KN
                        IL = IA(IN)+LN
                        JK = IA(MAX0(JN,KN))+MIN0(JN,KN)
                        JL = IA(JN)+LN
                        IF(JN.LT.KN) JL = IA(MAX0(JN,LN))+MIN0(JN,LN)
                        DQ1= DA(IK)*DA(JL)+DA(IL)*DA(JK)
                        IF(UHFTYP) DQ1=DQ1+DB(IK)*DB(JL)+DB(IL)*DB(JK)
                        DF1= DF1-QFS*DQ1
                     ENDIF
                     DF1= DF1*F04
                     IF(JN.EQ.IN               ) DF1= DF1*PT5
                     IF(LN.EQ.KN               ) DF1= DF1*PT5
                     IF(KN.EQ.IN .AND. LN.EQ.JN) DF1= DF1*PT5
                     IF(DABMAX.LT. ABS(DF1)) DABMAX= ABS(DF1)
C
C IGXYZ AND J, K, AND L ARE SET UP IN JKDNDX
C
                     IJKL=IGXYZ(1,I)+JGXYZ(1,J)+KGXYZ(1,K)+LGXYZ(1,L)
                     DAB(IJKL)= DF1*P4L
                     IF(OUT) WRITE(IW,9020) II,JJ,KK,LL,I,J,K,L,IJKL,DF1
  210    CONTINUE
      ENDIF
      RETURN
 9010 FORMAT(' -DABDFT,POPLE- ',4I4,4I3,D20.12)
 9020 FORMAT(' -DABDFT,HONDO- ',4I4,4I3,I5,D20.12)
      END
C*MODULE GRD2A   *DECK DABGVB
      SUBROUTINE DABGVB(II,JJ,KK,LL,NOCORE,NOOPEN,DA,V,NUM,DAB,
     *                  DABMAX,Q4,POPLE)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION DA(*),V(NUM,*),DAB(*)
      LOGICAL NOCORE,NOOPEN,POPLE
C
      PARAMETER (MXGTOT=5000, MXSH=1000, MXAO=2047)
C
      LOGICAL OUT,DBG
      COMMON /DSHLT / RTOL,DTOL,VTOL1,VTOL2,VTOLS,OUT,DBG
      COMMON /GVBWFN/ CICOEF(2,12),F(25),ALPHA(325),BETA(325),NO(10),
     *                NCO,NSETO,NOPEN,NPAIR,NMOGVB,NCONF(MXAO),NHAM
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /INDD80/ IMAX,JMAX,KKMAX,LMAX
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /SHLLMN/ IGXYZ(4,35),JGXYZ(4,35),KGXYZ(4,35),LGXYZ(4,35)
      LOGICAL IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
      COMMON /SHLEQU/ IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
      COMMON /SHLNRM/ PNRM(35)
C
CJMS  LABELLED COMMON GSPG80 DEFINED FOR COMPUTATIONAL EFFICIENCY.
CJMS  FOR SP BASES ONLY, IT CONTAINS THE E ARRAY WHICH IS THE DAB
CJMS  ARRAY WITH INDICES IN REVERSE ORDER: E(I,J,K,L)= DAB(L,K,J,I)
CJMS  AND IS USED IN SUB JKDG80 (MODULE GRD2B). IT ORIGINATES IN:
CJMS
CJMS     1. SUBS DABCLU, DABDFT, DABGVB, DABMC, DABMP2 AND DABUMP
CJMS        (MODULE GRD2A) AND SUB DABPAU (MODULE EFPAUL) WHICH ARE
CJMS        ALL CALLED BY SUB JKDER (MODULE GRD2A)
CJMS
CJMS     2. SUB DABCLU (MODULE GRD2A) WHICH IS CALLED BY SUB EFDEN OF
CJMS        MODULE EFGRD2
CJMS
CJMS     3. SUB PAR2PDM (MODULE MP2DDI) WHICH IS CALLED BY SUB PJKDMP2
CJMS        OF MODULE MP2DDI
C
      COMMON /GSPG80/ E(4,4,4,4)
C
      PARAMETER (ZER=0.0D+00, PT5=0.5D+00, F04=4.0D+00)
C
      DABMAX= ZER
      MINI= KMIN(II)
      MINJ= KMIN(JJ)
      MINK= KMIN(KK)
      MINL= KMIN(LL)
      LOCI= KLOC(II)-MINI
      LOCJ= KLOC(JJ)-MINJ
      LOCK= KLOC(KK)-MINK
      LOCL= KLOC(LL)-MINL
      NCO1= NCO+1
      IF(POPLE) THEN
         DO 110 L=1,LMAX
            DO 110 K=1,KKMAX
               KAL= MAX0(LOCK+K,LOCL+L)
               KIL= MIN0(LOCK+K,LOCL+L)
               DO 110 J=1,JMAX
                  DO 110 I=1,IMAX
                     IAJ= MAX0(LOCI+I,LOCJ+J)
                     IIJ= MIN0(LOCI+I,LOCJ+J)
                     IN = IAJ
                     JN = IIJ
                     KN = KAL
                     LN = KIL
                     IF(IN.LT.KN .OR.(IN.EQ.KN .AND. JN.LT.LN)) THEN
                        IN = KAL
                        JN = KIL
                        KN = IAJ
                        LN = IIJ
                     ENDIF
C        -----  NOW CALCULATE DENSITY MATRIX CONTRIBUTION.      -----
C        -----  (IJ/KL),(IJ/IL),(IJ/JL),(IJ/KJ)                 -----
C        -----  EIGHT DISTINCT INTEGRALS                        -----
C        -----  CONTRIBUTION TO THE ENERGY                      -----
C        -----  4(IJ)(KL) - (IK)(JL) - (JK)(IL)                 -----
                     IJ = IA(IN)+JN
                     IK = IA(IN)+KN
                     IL = IA(IN)+LN
                     JK = IA(MAX0(JN,KN))+MIN0(JN,KN)
                     JL = IA(JN)+LN
                     IF(JN.LT.KN) JL = IA(MAX0(JN,LN))+MIN0(JN,LN)
                     KL = IA(KN)+LN
                     DF1= ZER
                     IF(NOCORE .AND. NOOPEN) GO TO 109
                     DQ1= ZER
                     IF(NOCORE) GO TO 103
                     DF1= DA(IJ)*DA(KL)*ALPHA(1)
                     DQ1=(DA(IK)*DA(JL)+DA(IL)*DA(JK))*BETA(1)
                     IF(NOOPEN) GO TO 107
                     DO 101 IO=NCO1,NMOGVB
                        IOJO = IA(NCONF(IO))+1
      DF1= DF1+(DA(IJ)*V(KN,IO)*V(LN,IO)+
     2          DA(KL)*V(IN,IO)*V(JN,IO))*ALPHA(IOJO)
      DQ1= DQ1+(V(JN,IO)*(DA(IK)*V(LN,IO)+DA(IL)*V(KN,IO))+
     2          V(IN,IO)*(DA(JL)*V(KN,IO)+DA(JK)*V(LN,IO)))*BETA(IOJO)
  101                CONTINUE
  103                CONTINUE
                     IF(NOOPEN) GO TO 107
                     DO 105 IO=NCO1,NMOGVB
                        IOF = NCONF(IO)
                        DO 105 JO=NCO1,NMOGVB
                           JOF = NCONF(JO)
                           IOJO= IA(MAX0(IOF,JOF))+MIN0(IOF,JOF)
      DF1= DF1+(V(IN,IO)*V(JN,IO)*V(KN,JO)*V(LN,JO))*ALPHA(IOJO)
      DQ1= DQ1+ V(IN,IO)*V(JN,JO)*
     2         (V(KN,IO)*V(LN,JO)+V(LN,IO)*V(KN,JO))*BETA(IOJO)
  105                CONTINUE
  107                CONTINUE
                     DF1=(DF1+DF1+DQ1)*Q4
                     IF(DABMAX.LT. ABS(DF1)) DABMAX= ABS(DF1)
  109                CONTINUE
                     E(I,J,K,L)= DF1
                     IF(OUT) WRITE(IW,9010) II,JJ,KK,LL,I,J,K,L,DF1
  110    CONTINUE
      ELSE
C
C D AND HIGHER FUNCTIONS OR HONDO ONLY RUNS
C
         MAXI= KMAX(II)
         MAXJ= KMAX(JJ)
         MAXK= KMAX(KK)
         MAXL= KMAX(LL)
         DO 210 I=MINI,MAXI
            P1I = PNRM(I)
            JMAX= MAXJ
            IF(IIEQJJ) JMAX= I
            DO 210 J=MINJ,JMAX
               P2J = P1I*PNRM(J)
               IAJ= MAX0(LOCI+I,LOCJ+J)
               IIJ= MIN0(LOCI+I,LOCJ+J)
               KMMAX=MAXK
               IF(IJEQKL) KMMAX= I
               DO 210 K=MINK,KMMAX
                  P3K = P2J*PNRM(K)
                  LMAX= MAXL
                  IF(KKEQLL) LMAX= K
                  IF(IJEQKL .AND. K.EQ.I) LMAX= J
                  DO 210 L=MINL,LMAX
                     P4L= P3K*PNRM(L)
                     KAL= MAX0(LOCK+K,LOCL+L)
                     KIL= MIN0(LOCK+K,LOCL+L)
                     IN = IAJ
                     JN = IIJ
                     KN = KAL
                     LN = KIL
                     IF(IN.LT.KN .OR.(IN.EQ.KN .AND. JN.LT.LN)) THEN
                        IN = KAL
                        JN = KIL
                        KN = IAJ
                        LN = IIJ
                     ENDIF
                     IJ = IA(IN)+JN
                     IK = IA(IN)+KN
                     IL = IA(IN)+LN
                     JK = IA(MAX0(JN,KN))+MIN0(JN,KN)
                     JL = IA(JN)+LN
                     IF(JN.LT.KN) JL = IA(MAX0(JN,LN))+MIN0(JN,LN)
                     KL = IA(KN)+LN
C
C CONTRIBUTION FROM CLOSED SHELLS
C
                     DF1= ZER
                     IF(NOCORE .AND. NOOPEN) GO TO 209
                     DQ1= ZER
                     IF(NOCORE) GO TO 203
                     DF1= DA(IJ)*DA(KL)*ALPHA(1)
                     DQ1=(DA(IK)*DA(JL)+DA(IL)*DA(JK))*BETA(1)
                     IF(NOOPEN) GO TO 207
C
C INTERACTION BETWEEN CORE AND OPEN/PAIR SPACE
C
                     DO 201 IO=NCO1,NMOGVB
                        IOJO = IA(NCONF(IO))+1
      DF1= DF1+(DA(IJ)*V(KN,IO)*V(LN,IO)+
     2          DA(KL)*V(IN,IO)*V(JN,IO))*ALPHA(IOJO)
      DQ1= DQ1+(V(JN,IO)*(DA(IK)*V(LN,IO)+DA(IL)*V(KN,IO))+
     2          V(IN,IO)*(DA(JK)*V(LN,IO)+DA(JL)*V(KN,IO)))*BETA(IOJO)
  201                CONTINUE
  203                CONTINUE
                     IF(NOOPEN) GO TO 207
C
C CONTRIBUTION FROM OPEN SHELLS AND PAIRS
C
                     DO 205 IO=NCO1,NMOGVB
                        IOF = NCONF(IO)
                        DO 205 JO=NCO1,NMOGVB
                           JOF = NCONF(JO)
                           IOJO= IA(MAX0(IOF,JOF))+MIN0(IOF,JOF)
      DF1= DF1+(V(IN,IO)*V(JN,IO)*V(KN,JO)*V(LN,JO))*ALPHA(IOJO)
      DQ1= DQ1+ V(IN,IO)*V(JN,JO)*
     2         (V(KN,IO)*V(LN,JO)+V(LN,IO)*V(KN,JO))*BETA(IOJO)
  205                CONTINUE
  207                CONTINUE
                     DF1=(DF1+DF1+DQ1)*F04
                     IF(JN.EQ.IN               ) DF1= DF1*PT5
                     IF(LN.EQ.KN               ) DF1= DF1*PT5
                     IF(KN.EQ.IN .AND. LN.EQ.JN) DF1= DF1*PT5
                     IF(DABMAX.LT. ABS(DF1)) DABMAX= ABS(DF1)
  209                CONTINUE
                     IJKL=IGXYZ(1,I)+JGXYZ(1,J)+KGXYZ(1,K)+LGXYZ(1,L)
                     DAB(IJKL)= DF1*P4L
                     IF(OUT) WRITE(IW,9020) II,JJ,KK,LL,I,J,K,L,IJKL,DF1
  210    CONTINUE
      ENDIF
      RETURN
 9010 FORMAT(' -DABGVB,POPLE- ',4I4,4I3,D20.12)
 9020 FORMAT(' -DABGVB,HONDO- ',4I4,4I3,I5,D20.12)
      END
C*MODULE GRD2A   *DECK DABMC
      SUBROUTINE DABMC(II,JJ,KK,LL,DC,DV,DM2,NMCC,NCI,MAXSHL,DAB,
     *                 DABMAX,Q4,POPLE,P1,P2,P3)
C
C     ----- THIS ROUTINE FORMS THE PRODUCT OF DENSITY    -----
C           MATRICES FOR USE IN FORMING THE TWO ELECTRON
C           GRADIENT.  VALID FOR MCSCF WAVEFUNCTION.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION DC(*),DV(*),DM2(MAXSHL,MAXSHL,MAXSHL,MAXSHL),DAB(*)
      LOGICAL POPLE,P1,P2,P3
C
      PARAMETER (MXGTOT=5000, MXSH=1000, MXAO=2047)
C
      LOGICAL OUT,DBG
      COMMON /DSHLT / RTOL,DTOL,VTOL1,VTOL2,VTOLS,OUT,DBG
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /SHLLMN/ IGXYZ(4,35),JGXYZ(4,35),KGXYZ(4,35),LGXYZ(4,35)
      COMMON /SHLNRM/ PNRM(35)
      LOGICAL IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
      COMMON /SHLEQU/ IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
C
CJMS  LABELLED COMMON GSPG80 DEFINED FOR COMPUTATIONAL EFFICIENCY.
CJMS  FOR SP BASES ONLY, IT CONTAINS THE E ARRAY WHICH IS THE DAB
CJMS  ARRAY WITH INDICES IN REVERSE ORDER: E(I,J,K,L)= DAB(L,K,J,I)
CJMS  AND IS USED IN SUB JKDG80 (MODULE GRD2B). IT ORIGINATES IN:
CJMS
CJMS     1. SUBS DABCLU, DABDFT, DABGVB, DABMC, DABMP2 AND DABUMP
CJMS        (MODULE GRD2A) AND SUB DABPAU (MODULE EFPAUL) WHICH ARE
CJMS        ALL CALLED BY SUB JKDER (MODULE GRD2A)
CJMS
CJMS     2. SUB DABCLU (MODULE GRD2A) WHICH IS CALLED BY SUB EFDEN OF
CJMS        MODULE EFGRD2
CJMS
CJMS     3. SUB PAR2PDM (MODULE MP2DDI) WHICH IS CALLED BY SUB PJKDMP2
CJMS        OF MODULE MP2DDI
C
      COMMON /GSPG80/ E(4,4,4,4)
C
      PARAMETER (ZER=0.0D+00, PT5=0.5D+00, P25=.25D+00, F04=4.0D+00)
C
      DABMAX= ZER
      MINI= KMIN(II)
      MINJ= KMIN(JJ)
      MINK= KMIN(KK)
      MINL= KMIN(LL)
      MAXI= KMAX(II)
      MAXJ= KMAX(JJ)
      MAXK= KMAX(KK)
      MAXL= KMAX(LL)
      LOCI= KLOC(II)-MINI
      LOCJ= KLOC(JJ)-MINJ
      LOCK= KLOC(KK)-MINK
      LOCL= KLOC(LL)-MINL
      IF(POPLE) THEN
         DO 110 L=MINL,MAXL
            DO 110 K=MINK,MAXK
               KNK= K
               LNL= L
               IF(P2) THEN
                  KNK= L
                  LNL= K
               ENDIF
               KAL= MAX0(LOCK+K,LOCL+L)
               KIL= MIN0(LOCK+K,LOCL+L)
               DO 110 J=MINJ,MAXJ
                  DO 110 I=MINI,MAXI
                     INI= I
                     JNJ= J
                     IF(P1) THEN
                        INI= J
                        JNJ= I
                     ENDIF
                     IAJ= MAX0(LOCI+I,LOCJ+J)
                     IIJ= MIN0(LOCI+I,LOCJ+J)
                     IN = IAJ
                     JN = IIJ
                     KN = KAL
                     LN = KIL
                     IF(IN.LT.KN .OR.(IN.EQ.KN .AND. JN.LT.LN)) THEN
                        IN = KAL
                        JN = KIL
                        KN = IAJ
                        LN = IIJ
                     ENDIF
                     IJ = IA(IN)+JN
                     IK = IA(IN)+KN
                     IL = IA(IN)+LN
                     JK = IA(MAX0(JN,KN))+MIN0(JN,KN)
                     JL = IA(JN)+LN
                     IF(JN.LT.KN) JL = IA(MAX0(JN,LN))+MIN0(JN,LN)
                     KL = IA(KN)+LN
                     DF1= ZER
                     DQ1= ZER
                     IF(NMCC.EQ.0) GO TO 101
C
C      -----  CONTRIBUTION CORE - CORE  ---------
C
                     DF1= DC(IJ)*DC(KL)
                     DQ1= DC(IK)*DC(JL)+DC(IL)*DC(JK)
                     IF(NCI.EQ.0) GO TO 103
C
C      -----  CONTRIBUTION CORE - VALENCE ---------
C
                     DF1= DF1+DC(IJ)*DV(KL)+DC(KL)*DV(IJ)
                     DQ1= DQ1+DC(IK)*DV(JL)+DC(IL)*DV(JK)+
     2                        DC(JL)*DV(IK)+DC(JK)*DV(IL)
  101                CONTINUE
C
C      -----  CONTRIBUTION VALENCE - VALENCE ---------
C
                     IN = INI
                     JN = JNJ
                     KN = KNK
                     LN = LNL
                     IF(P3) THEN
                        IN = KNK
                        JN = LNL
                        KN = INI
                        LN = JNJ
                        IF(P1.OR.P2) THEN
                           IN = LNL
                           JN = KNK
                           KN = JNJ
                           LN = INI
                        ENDIF
                     ENDIF
                     DF1= DF1+DM2(IN,JN,KN,LN)
  103                CONTINUE
                     DF1=(DF1-P25*DQ1)*Q4
                     IF(DABMAX.LT. ABS(DF1)) DABMAX= ABS(DF1)
                     E(I,J,K,L)= DF1
                     IF(OUT) WRITE(IW,9010) II,JJ,KK,LL,I,J,K,L,DF1
  110    CONTINUE
      ELSE
C
C D AND HIGHER FUNCTIONS OR HONDO ONLY RUNS
C
         DO 210 I=MINI,MAXI
            P1I = PNRM(I)
            JMAX= MAXJ
            IF(IIEQJJ) JMAX= I
            DO 210 J=MINJ,JMAX
               P2J = P1I*PNRM(J)
               IAJ= MAX0(LOCI+I,LOCJ+J)
               IIJ= MIN0(LOCI+I,LOCJ+J)
               KMMAX=MAXK
               IF(IJEQKL) KMMAX= I
               DO 210 K=MINK,KMMAX
                  P3K = P2J*PNRM(K)
                  LMAX= MAXL
                  IF(KKEQLL) LMAX= K
                  IF(IJEQKL .AND. K.EQ.I) LMAX= J
                  DO 210 L=MINL,LMAX
                     P4L= P3K*PNRM(L)
                     KAL= MAX0(LOCK+K,LOCL+L)
                     KIL= MIN0(LOCK+K,LOCL+L)
                     IN = IAJ
                     JN = IIJ
                     KN = KAL
                     LN = KIL
                     IF(IN.LT.KN .OR.(IN.EQ.KN .AND. JN.LT.LN)) THEN
                        IN = KAL
                        JN = KIL
                        KN = IAJ
                        LN = IIJ
                     ENDIF
                     IJ = IA(IN)+JN
                     IK = IA(IN)+KN
                     IL = IA(IN)+LN
                     JK = IA(MAX0(JN,KN))+MIN0(JN,KN)
                     JL = IA(JN)+LN
                     IF(JN.LT.KN) JL = IA(MAX0(JN,LN))+MIN0(JN,LN)
                     KL = IA(KN)+LN
                     DF1= ZER
                     DQ1= ZER
                     IF(NMCC.EQ.0) GO TO 201
C
C      -----  CONTRIBUTION CORE - CORE  ---------
C
                     DF1= DC(IJ)*DC(KL)
                     DQ1= DC(IK)*DC(JL)+DC(IL)*DC(JK)
                     IF(NCI.EQ.0) GO TO 203
C
C      -----  CONTRIBUTION CORE - VALENCE ---------
C
                     DF1= DF1+DC(IJ)*DV(KL)+DC(KL)*DV(IJ)
                     DQ1= DQ1+DC(IK)*DV(JL)+DC(IL)*DV(JK)+
     2                        DC(JL)*DV(IK)+DC(JK)*DV(IL)
  201                CONTINUE
C
C      -----  CONTRIBUTION VALENCE - VALENCE ---------
C
                     DF1= DF1+DM2(I,J,K,L)
  203                CONTINUE
                     DF1= DF1*F04-DQ1
                     IF(JN.EQ.IN               ) DF1= DF1*PT5
                     IF(LN.EQ.KN               ) DF1= DF1*PT5
                     IF(KN.EQ.IN .AND. LN.EQ.JN) DF1= DF1*PT5
                     IF(DABMAX.LT. ABS(DF1)) DABMAX= ABS(DF1)
                     IJKL=IGXYZ(1,I)+JGXYZ(1,J)+KGXYZ(1,K)+LGXYZ(1,L)
                     DAB(IJKL)= DF1*P4L
                     IF(OUT) WRITE(IW,9020) II,JJ,KK,LL,I,J,K,L,IJKL,DF1
  210    CONTINUE
      ENDIF
      RETURN
 9010 FORMAT(' -DABMC ,POPLE- ',4I4,4I3,D20.12)
 9020 FORMAT(' -DABMC ,HONDO- ',4I4,4I3,I5,D20.12)
      END
C*MODULE GRD2A   *DECK DABMCI
      SUBROUTINE DABMCI(V,GIN,GOUT,NCI,II,NUM,MAXSHL,NMCC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MXGTOT=5000, MXSH=1000)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      DIMENSION V(NUM,*)
      DIMENSION GIN(NCI,NCI,NCI,NCI)
      DIMENSION GOUT(MAXSHL,NCI,NCI,NCI)
      PARAMETER (ZERO=0.0D+00)
C
C     ----- PARTIAL TRANSFORMATION OF G(IJKL) TO G(MU,J,K,L) -----
C
      IX = KLOC(II)-KMIN(II)
      DO 30 L = 1, NCI
      DO 30 K = 1, NCI
      DO 30 J = 1, NCI
        DO 20 M = KMIN(II),KMAX(II)
        IN = IX + M
        DUM  = ZERO
        DO 10 I = 1, NCI
   10     DUM = DUM + GIN(I,J,K,L)* V(IN,I+NMCC)
   20   GOUT(M,J,K,L)=DUM
   30 CONTINUE
C
      RETURN
      END
C*MODULE GRD2A   *DECK DABMCJ
      SUBROUTINE DABMCJ(V,GIN,GOUT,NCI,II,JJ,NUM,MAXSHL,NMCC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MXGTOT=5000, MXSH=1000)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      DIMENSION V(NUM,*)
      DIMENSION GIN(MAXSHL,NCI,NCI,NCI)
      DIMENSION GOUT(MAXSHL,MAXSHL,NCI,NCI)
      PARAMETER (ZERO=0.0D+00)
C
C     ----- PARTIAL TRANSFORMATION OF G(MU,JKL) IN G(MU,NU,K,L) -----
C
      JX = KLOC(JJ)-KMIN(JJ)
      DO 30 L = 1, NCI
      DO 30 K = 1, NCI
      DO 30 M = KMIN(II),KMAX(II)
        DO 20 N = KMIN(JJ),KMAX(JJ)
        JN = JX + N
        DUM  = ZERO
        DO 10 J = 1, NCI
   10     DUM = DUM + GIN(M,J,K,L) * V(JN,J+NMCC)
   20   GOUT(M,N,K,L)=DUM
   30 CONTINUE
C
      RETURN
      END
C*MODULE GRD2A   *DECK DABMCK
      SUBROUTINE DABMCK(V,GIN,GOUT,NCI,II,JJ,KK,NUM,MAXSHL,NMCC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MXGTOT=5000, MXSH=1000)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      DIMENSION V(NUM,*)
      DIMENSION GIN(MAXSHL,MAXSHL,NCI,NCI)
      DIMENSION GOUT(MAXSHL,MAXSHL,MAXSHL,NCI)
      PARAMETER (ZERO=0.0D+00)
C
C     ----- PARTIAL TRANSFORMATION OF G(M,N,KL) IN G(M,N,P,L) -----
C
      KX = KLOC(KK)-KMIN(KK)
      DO 30 L = 1, NCI
      DO 30 N = KMIN(JJ),KMAX(JJ)
      DO 30 M = KMIN(II),KMAX(II)
        DO 20 NP = KMIN(KK),KMAX(KK)
        KN = KX + NP
        DUM  = ZERO
        DO 10 K = 1, NCI
   10     DUM = DUM + GIN(M,N,K,L) * V(KN,K+NMCC)
   20   GOUT(M,N,NP,L)=DUM
   30 CONTINUE
C
      RETURN
      END
C*MODULE GRD2A   *DECK DABMCL
      SUBROUTINE DABMCL(V,GIN,GOUT,NCI,II,JJ,KK,LL,NUM,MAXSHL,NMCC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MXGTOT=5000, MXSH=1000)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      DIMENSION V(NUM,*)
      DIMENSION GIN(MAXSHL,MAXSHL,MAXSHL,NCI)
      DIMENSION GOUT(MAXSHL,MAXSHL,MAXSHL,MAXSHL)
      PARAMETER (ZERO=0.0D+00)
C
C     ----- FINAL TRANSFORMATION OF G(M,N,P,L) IN G(M,N,P,Q) -----
C
      LX = KLOC(LL)-KMIN(LL)
      DO 30 NP =KMIN(KK),KMAX(KK)
      DO 30 N  =KMIN(JJ),KMAX(JJ)
      DO 30 M  =KMIN(II),KMAX(II)
        DO 20 NQ =KMIN(LL),KMAX(LL)
        LN = LX + NQ
        DUM  = ZERO
        DO 10 L = 1, NCI
   10     DUM = DUM + GIN(M,N,NP,L) * V(LN,L+NMCC)
   20   GOUT(M,N,NP,NQ)=DUM
   30 CONTINUE
C
      RETURN
      END
C*MODULE GRD2A   *DECK DABMP2
      SUBROUTINE DABMP2(II,JJ,KK,LL,DM2,C,PHF,PMP2,DAB,DABMAX,
     *                  L1,L2,Q4,POPLE)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION DM2(L2,L1,*),C(L1,*),PHF(L2),PMP2(L2),DAB(*)
      LOGICAL POPLE
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXAO=2047)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /INDD80/ LA,LB,LC,LD
      COMMON /MP2DM2/ NOC1,NOC2,ISTEP,NOC3,NOC4
      COMMON /MP2PAR/ OSPT,TOL,METHOD,NWORD,MPPROP,NACORE,NBCORE,
     *                NOA,NOB,NORB,NBF,NOMIT,MOCPHF,MAXITC
      LOGICAL SOME,OUT,DBUG
      COMMON /MP2PRT/ SOME,OUT,DBUG
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CCG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      LOGICAL IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
      COMMON /SHLEQU/ IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
      COMMON /SHLLMN/ IGXYZ(4,35),JGXYZ(4,35),KGXYZ(4,35),LGXYZ(4,35)
      COMMON /SHLNRM/ PNRM(35)
C
CJMS  LABELLED COMMON GSPG80 DEFINED FOR COMPUTATIONAL EFFICIENCY.
CJMS  FOR SP BASES ONLY, IT CONTAINS THE E ARRAY WHICH IS THE DAB
CJMS  ARRAY WITH INDICES IN REVERSE ORDER: E(I,J,K,L)= DAB(L,K,J,I)
CJMS  AND IS USED IN SUB JKDG80 (MODULE GRD2B). IT ORIGINATES IN:
CJMS
CJMS     1. SUBS DABCLU, DABDFT, DABGVB, DABMC, DABMP2 AND DABUMP
CJMS        (MODULE GRD2A) AND SUB DABPAU (MODULE EFPAUL) WHICH ARE
CJMS        ALL CALLED BY SUB JKDER (MODULE GRD2A)
CJMS
CJMS     2. SUB DABCLU (MODULE GRD2A) WHICH IS CALLED BY SUB EFDEN OF
CJMS        MODULE EFGRD2
CJMS
CJMS     3. SUB PAR2PDM (MODULE MP2DDI) WHICH IS CALLED BY SUB PJKDMP2
CJMS        OF MODULE MP2DDI
C
      COMMON /GSPG80/ E(4,4,4,4)
C
      PARAMETER (ZER=0.0D+00, PT5=0.5D+00, P25=.25D+00, F04=4.0D+00)
C
C     ----- FORM TWO-PARTICLE DENSITY MATRIX FOR MP2 GRADIENT -----
C
      DABMAX= ZER
      MINI= KMIN(II)
      MINJ= KMIN(JJ)
      MINK= KMIN(KK)
      MINL= KMIN(LL)
      LOCI= KLOC(II)-MINI
      LOCJ= KLOC(JJ)-MINJ
      LOCK= KLOC(KK)-MINK
      LOCL= KLOC(LL)-MINL
      NOC = NOC1-1
      IF(POPLE) THEN
         DO 110 L=1,LD
            NNU= LOCL+L
            DO 110 K=1,LC
               NMU= LOCK+K
               MUNU=IA(MAX0(NMU,NNU))+MIN0(NMU,NNU)
               DO 110 J=1,LB
                  NSI= LOCJ+J
                  DO 110 I=1,LA
                     NLA= LOCI+I
                     LASI=IA(MAX0(NLA,NSI))+MIN0(NLA,NSI)
C
C                     WRITE(IW,*) 'NLA,NSI,NMU,NNU,LASI,MUNU=',
C     *                            NLA,NSI,NMU,NNU,LASI,MUNU
C
C     ----- TRANSFORM FINAL INDEX OF THE NON-SEPARABLE TERM -----
C
                     DF1= ZER
                     DO 101 MI=NOC1,NOC2
C                        WRITE(IW,*) 'MI=',MI, ' MI-NOC1+1=',MI-NOC
C                        WRITE(IW,*) 'DFAC,DM2*C,DM2*C=',
C     *                               DFAC,DM2(LASI,NMU,MI-NOC),
C     *                               C(NNU,MI+NACORE)
C                        WRITE(IW,*)  DM2(LASI,NNU,MI-NOC),
C     *                               C(NMU,MI+NACORE)
  101                DF1= DF1+DM2(LASI,NMU,MI-NOC)*C(NNU,MI+NACORE)+
     2                        DM2(LASI,NNU,MI-NOC)*C(NMU,MI+NACORE)
                     DF1= DF1*PT5
C                     WRITE(IW,*) 'DFAC=',DF1*F04
C
C     ----- ADD SEPARABLE TERM (IF NEEDED) -----
C
                     IF(ISTEP.EQ.1) THEN
                        MUSI= IA(MAX0(NMU,NSI))+MIN0(NMU,NSI)
                        LANU= IA(MAX0(NLA,NNU))+MIN0(NLA,NNU)
                        MULA= IA(MAX0(NMU,NLA))+MIN0(NMU,NLA)
                        NUSI= IA(MAX0(NNU,NSI))+MIN0(NNU,NSI)
C                        T01= DF1
                        DF1= DF1+(PHF (MUNU)+PMP2(MUNU))*PHF (LASI)+
     2                            PHF (MUNU)*PMP2(LASI)
                        DQ1=     (PHF (MUSI)+PMP2(MUSI))*PHF (LANU)+
     2                            PHF (MUSI)*PMP2(LANU)+
     3                           (PHF (MULA)+PMP2(MULA))*PHF (NUSI)+
     4                            PHF (MULA)*PMP2(NUSI)
                        DF1= DF1-P25*DQ1
C
C                        SEPDFAC=(DF1-T01)*F04
C                        WRITE(IW,*) 'SEP CONTRIBUTION=',SEPDFAC
                     ENDIF
                     DF1= DF1*Q4
                     IF(DABMAX.LT. ABS(DF1)) DABMAX = ABS(DF1)
                     E(I,J,K,L)= DF1
                     IF(OUT) WRITE(IW,9010) II,JJ,KK,LL,I,J,K,L,DF1
  110    CONTINUE
      ELSE
C
C D AND HIGHER FUNCTIONS OR HONDO ONLY RUNS
C
         MAXI= KMAX(II)
         MAXJ= KMAX(JJ)
         MAXK= KMAX(KK)
         MAXL= KMAX(LL)
         DO 210 I=MINI,MAXI
            P1I = PNRM(I)
            NLA = LOCI+I
            JMAX= MAXJ
            IF(IIEQJJ) JMAX= I
            DO 210 J=MINJ,JMAX
               P2J = P1I*PNRM(J)
               NSI = LOCJ+J
               LASI= IA(MAX0(NLA,NSI))+MIN0(NLA,NSI)
               KKMAX=MAXK
               IF(IJEQKL) KKMAX= I
               DO 210 K=MINK,KKMAX
                  P3K = P2J*PNRM(K)
                  NMU = LOCK+K
                  LMAX= MAXL
                  IF(KKEQLL) LMAX= K
                  IF(IJEQKL .AND. K.EQ.I) LMAX= J
                  DO 210 L=MINL,LMAX
                     P4L = P3K*PNRM(L)
                     NNU = LOCL+L
                     MUNU= IA(MAX0(NMU,NNU))+MIN0(NMU,NNU)
C
C                     WRITE(IW,*) 'NLA,NSI,NMU,NNU,LASI,MUNU=',
C     *                            NLA,NSI,NMU,NNU,LASI,MUNU
C
C     ----- TRANSFORM NON-SEPARABLE TERM   -----
C
                     DF1= ZER
                     DO 201 MI=NOC1,NOC2
C                        WRITE(IW,*) 'MI=',MI, ' MI-NOC1+1=',MI-NOC
C                        WRITE(IW,*) 'DFAC,DM2*C,DM2*C=',
C     *                               DFAC,DM2(LASI,NMU,MI-NOC),
C     *                               C(NNU,MI+NACORE)
C                        WRITE(IW,*)  DM2(LASI,NNU,MI-NOC),
C     *                               C(NMU,MI+NACORE)
  201                DF1= DF1+DM2(LASI,NMU,MI-NOC)*C(NNU,MI+NACORE)+
     2                        DM2(LASI,NNU,MI-NOC)*C(NMU,MI+NACORE)
                     DF1= DF1*PT5
C                     WRITE(IW,*) 'DFAC=',DF1*F04
C
C     ----- ADD SEPARABLE TERM (IF NEEDED) -----
C
                     IF(ISTEP.EQ.1) THEN
                        MUSI= IA(MAX0(NMU,NSI))+MIN0(NMU,NSI)
                        LANU= IA(MAX0(NLA,NNU))+MIN0(NLA,NNU)
                        MULA= IA(MAX0(NMU,NLA))+MIN0(NMU,NLA)
                        NUSI= IA(MAX0(NNU,NSI))+MIN0(NNU,NSI)
C                        WRITE(IW,*) 'MUSI,LANU,MULA,NUSI=',
C     *                               MUSI,LANU,MULA,NUSI
C
                        DF1= DF1+(PHF (MUNU)+PMP2(MUNU))*PHF (LASI)+
     2                            PHF (MUNU)*PMP2(LASI)
                        DQ1=     (PHF (MUSI)+PMP2(MUSI))*PHF (LANU)+
     2                            PHF (MUSI)*PMP2(LANU)+
     3                           (PHF (MULA)+PMP2(MULA))*PHF (NUSI)+
     4                            PHF (MULA)*PMP2(NUSI)
                        DF1= DF1-P25*DQ1
                     ENDIF
                     DF1= DF1*F04
                     IF(NMU .EQ.NNU ) DF1= DF1*PT5
                     IF(NLA .EQ.NSI ) DF1= DF1*PT5
                     IF(MUNU.EQ.LASI) DF1= DF1*PT5
C                     WRITE(IW,*) '** DFAC=',DF1
C
                     IF(DABMAX.LT. ABS(DF1)) DABMAX= ABS(DF1)
                     IJKL=IGXYZ(1,I)+JGXYZ(1,J)+KGXYZ(1,K)+LGXYZ(1,L)
                     DAB(IJKL)= DF1*P4L
                     IF(OUT) WRITE(IW,9020) II,JJ,KK,LL,I,J,K,L,IJKL,DF1
 210     CONTINUE
      ENDIF
      RETURN
 9010 FORMAT(' -DABMP2,POPLE- ',4I4,4I3,D20.12)
 9020 FORMAT(' -DABMP2,HONDO- ',4I4,4I3,I5,D20.12)
      END
C*MODULE GRD2A   *DECK DABUMP
      SUBROUTINE DABUMP(II,JJ,KK,LL,DM2A,DM2B,CA,CB,PHFA,PHFB,PMP2A,
     *                  PMP2B,DAB,DABMAX,L1,L2,Q4,POPLE)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION DM2A(L2,L1,*),CA(L1,*),PHFA(L2),PMP2A(L2),DAB(*),
     *          DM2B(L2,L1,*),CB(L1,*),PHFB(L2),PMP2B(L2)
      LOGICAL POPLE
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXAO=2047)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /INDD80/ LA,LB,LC,LD
      COMMON /MP2DM2/ NOC1,NOC2,ISTEP,NOC3,NOC4
      COMMON /MP2PAR/ OSPT,TOL,METHOD,NWORD,MPPROP,NACORE,NBCORE,
     *                NOA,NOB,NORB,NBF,NOMIT,MOCPHF,MAXITC
      LOGICAL SOME,OUT,DBUG
      COMMON /MP2PRT/ SOME,OUT,DBUG
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CCG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      LOGICAL IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
      COMMON /SHLEQU/ IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
      COMMON /SHLLMN/ IGXYZ(4,35),JGXYZ(4,35),KGXYZ(4,35),LGXYZ(4,35)
      COMMON /SHLNRM/ PNRM(35)
C
CJMS  LABELLED COMMON GSPG80 DEFINED FOR COMPUTATIONAL EFFICIENCY.
CJMS  FOR SP BASES ONLY, IT CONTAINS THE E ARRAY WHICH IS THE DAB
CJMS  ARRAY WITH INDICES IN REVERSE ORDER: E(I,J,K,L)= DAB(L,K,J,I)
CJMS  AND IS USED IN SUB JKDG80 (MODULE GRD2B). IT ORIGINATES IN:
CJMS
CJMS     1. SUBS DABCLU, DABDFT, DABGVB, DABMC, DABMP2 AND DABUMP
CJMS        (MODULE GRD2A) AND SUB DABPAU (MODULE EFPAUL) WHICH ARE
CJMS        ALL CALLED BY SUB JKDER (MODULE GRD2A)
CJMS
CJMS     2. SUB DABCLU (MODULE GRD2A) WHICH IS CALLED BY SUB EFDEN OF
CJMS        MODULE EFGRD2
CJMS
CJMS     3. SUB PAR2PDM (MODULE MP2DDI) WHICH IS CALLED BY SUB PJKDMP2
CJMS        OF MODULE MP2DDI
C
      COMMON /GSPG80/ E(4,4,4,4)
C
      PARAMETER (ZER=0.0D+00, PT5=0.5D+00, F04=4.0D+00)
C
C     ----- FORM TWO-PARTICLE DENSITY MATRIX FOR MP2 GRADIENT -----
C
      DABMAX= ZER
      MINI= KMIN(II)
      MINJ= KMIN(JJ)
      MINK= KMIN(KK)
      MINL= KMIN(LL)
      LOCI= KLOC(II)-MINI
      LOCJ= KLOC(JJ)-MINJ
      LOCK= KLOC(KK)-MINK
      LOCL= KLOC(LL)-MINL
      N1L = NOC1-1
      N3L = NOC3-1
      IF(POPLE) THEN
         DO 110 L=1,LD
            NNU = LOCL+L
            DO 110 K=1,LC
               NMU = LOCK+K
               MUNU= IA(MAX0(NMU,NNU))+MIN0(NMU,NNU)
               DO 110 J=1,LB
                  NSI = LOCJ+J
                  DO 110 I=1,LA
                     NLA = LOCI+I
                     LASI= IA(MAX0(NLA,NSI))+MIN0(NLA,NSI)
C
C     ----- TRANSFORM FINAL INDEX OF THE NON-SEPARABLE TERM -----
C
                     DF1= ZER
                     DO 101 MI=NOC1,NOC2
  101                DF1= DF1+DM2A(LASI,NMU,MI-N1L)*CA(NNU,MI+NACORE)
     *                       +DM2A(LASI,NNU,MI-N1L)*CA(NMU,MI+NACORE)
                     DO 102 MI=NOC3,NOC4
  102                DF1= DF1+DM2B(LASI,NMU,MI-N3L)*CB(NNU,MI+NBCORE)
     *                       +DM2B(LASI,NNU,MI-N3L)*CB(NMU,MI+NBCORE)
                     DF1= DF1*PT5
C
C     ----- ADD SEPARABLE TERM (IF NEEDED) -----
C
                     IF(ISTEP.EQ.1) THEN
                        MUSI= IA(MAX0(NMU,NSI))+MIN0(NMU,NSI)
                        LANU= IA(MAX0(NLA,NNU))+MIN0(NLA,NNU)
                        MULA= IA(MAX0(NMU,NLA))+MIN0(NMU,NLA)
                        NUSI= IA(MAX0(NNU,NSI))+MIN0(NNU,NSI)
C
CJMS  LET'S SAVE A FEW MULTIPLICATIONS
C
C                       SEPDFAC =     FOUR*PHFA (MUNU)*PHFA(LASI)
C    *                              + FOUR*PHFA (MUNU)*PHFB(LASI)
C    *                              + FOUR*PHFB (MUNU)*PHFA(LASI)
C    *                              + FOUR*PHFB (MUNU)*PHFB(LASI)
C    *                              - TWO*PHFA (MUSI)*PHFA(LANU)
C    *                              - TWO*PHFB (MUSI)*PHFB(LANU)
C    *                              - TWO*PHFA (MULA)*PHFA(NUSI)
C    *                              - TWO*PHFB (MULA)*PHFB(NUSI)
C    *                              + FOUR*PMP2A(MUNU)*PHFA(LASI)
C    *                              + FOUR*PMP2A(MUNU)*PHFB(LASI)
C    *                              + FOUR*PMP2B(MUNU)*PHFA(LASI)
C    *                              + FOUR*PMP2B(MUNU)*PHFB(LASI)
C    *                              + FOUR*PMP2A(LASI)*PHFA(MUNU)
C    *                              + FOUR*PMP2A(LASI)*PHFB(MUNU)
C    *                              + FOUR*PMP2B(LASI)*PHFA(MUNU)
C    *                              + FOUR*PMP2B(LASI)*PHFB(MUNU)
C    *                              -  TWO*PMP2A(MUSI)*PHFA(LANU)
C    *                              -  TWO*PMP2B(MUSI)*PHFB(LANU)
C    *                              -  TWO*PMP2A(MULA)*PHFA(NUSI)
C    *                              -  TWO*PMP2B(MULA)*PHFB(NUSI)
C    *                              -  TWO*PMP2A(NUSI)*PHFA(MULA)
C    *                              -  TWO*PMP2B(NUSI)*PHFB(MULA)
C    *                              -  TWO*PMP2A(LANU)*PHFA(MUSI)
C    *                              -  TWO*PMP2B(LANU)*PHFB(MUSI)
C
                        SF1 =(PMP2A(MUNU)+PHFA (MUNU)+
     2                        PMP2B(MUNU)+PHFB (MUNU))*PHFA(LASI)+
     3                       (PMP2A(MUNU)+PHFA (MUNU)+
     4                        PMP2B(MUNU)+PHFB (MUNU))*PHFB(LASI)+
     5                       (PMP2A(LASI)+PMP2B(LASI))*PHFA(MUNU)+
     6                       (PMP2A(LASI)+PMP2B(LASI))*PHFB(MUNU)
                        SH1 =(PMP2A(MUSI)+PHFA (MUSI))*PHFA(LANU)+
     2                       (PMP2A(MULA)+PHFA (MULA))*PHFA(NUSI)+
     3                       (PMP2B(MUSI)+PHFB (MUSI))*PHFB(LANU)+
     4                       (PMP2B(MULA)+PHFB (MULA))*PHFB(NUSI)+
     5                        PMP2A(NUSI)*PHFA(MULA)+
     6                        PMP2B(NUSI)*PHFB(MULA)+
     7                        PMP2A(LANU)*PHFA(MUSI)+
     8                        PMP2B(LANU)*PHFB(MUSI)
                        SEPDFAC=(SF1-PT5*SH1)
                        DF1= DF1+SEPDFAC
                        SEPDFAC= SEPDFAC*F04
                     END IF
                     DF1= DF1*Q4
                     IF(DABMAX.LT. ABS(DF1)) DABMAX= ABS(DF1)
                     E(I,J,K,L)= DF1
                     IF(OUT) WRITE(IW,9010) II,JJ,KK,LL,I,J,K,L,DF1
  110    CONTINUE
      ELSE
C
C D AND HIGHER FUNCTIONS OR HONDO ONLY RUNS
C
         MAXI= KMAX(II)
         DO 210 I=MINI,MAXI
            P1I = PNRM(I)
            NLA = LOCI+I
            MAXJ= KMAX(JJ)
            IF(IIEQJJ) MAXJ= I
            DO 210 J=MINJ,MAXJ
               P2J = P1I*PNRM(J)
               NSI = LOCJ+J
               LASI= IA(MAX0(NLA,NSI))+MIN0(NLA,NSI)
               MAXK= KMAX(KK)
               IF(IJEQKL) MAXK= I
               DO 210 K=MINK,MAXK
                  P3K = P2J*PNRM(K)
                  NMU = LOCK+K
                  MAXL= KMAX(LL)
                  IF(KKEQLL) MAXL= K
                  IF(IJEQKL .AND. K.EQ.I) MAXL= J
                  DO 210 L=MINL,MAXL
                     P4L = P3K*PNRM(L)
                     NNU = LOCL+L
                     MUNU= IA(MAX0(NMU,NNU))+MIN0(NMU,NNU)
C
C     ----- TRANSFORM NON-SEPARABLE TERM   -----
C
                     DF1= ZER
                     DO 201 MI=NOC1,NOC2
  201                DF1= DF1+DM2A(LASI,NMU,MI-N1L)*CA(NNU,MI+NACORE)
     *                       +DM2A(LASI,NNU,MI-N1L)*CA(NMU,MI+NACORE)
                     DO 202 MI=NOC3,NOC4
  202                DF1= DF1+DM2B(LASI,NMU,MI-N3L)*CB(NNU,MI+NBCORE)
     *                       +DM2B(LASI,NNU,MI-N3L)*CB(NMU,MI+NBCORE)
                     DF1= DF1*PT5
C
C     ----- ADD SEPARABLE TERM (IF NEEDED) -----
C
                     IF(ISTEP.EQ.1) THEN
                        MUSI= IA(MAX0(NMU,NSI))+MIN0(NMU,NSI)
                        LANU= IA(MAX0(NLA,NNU))+MIN0(NLA,NNU)
                        MULA= IA(MAX0(NMU,NLA))+MIN0(NMU,NLA)
                        NUSI= IA(MAX0(NNU,NSI))+MIN0(NNU,NSI)
C
CJMS  LET'S SAVE A FEW MULTIPLICATIONS
C
C                       SEPDFAC =          PHFA (MUNU)*PHFA(LASI)
C    *                              +      PHFA (MUNU)*PHFB(LASI)
C    *                              +      PHFB (MUNU)*PHFA(LASI)
C    *                              +      PHFB (MUNU)*PHFB(LASI)
C    *                              -  PT5*PHFA (MUSI)*PHFA(LANU)
C    *                              -  PT5*PHFB (MUSI)*PHFB(LANU)
C    *                              -  PT5*PHFA (MULA)*PHFA(NUSI)
C    *                              -  PT5*PHFB (MULA)*PHFB(NUSI)
C    *                              +      PMP2A(MUNU)*PHFA(LASI)
C    *                              +      PMP2A(MUNU)*PHFB(LASI)
C    *                              +      PMP2B(MUNU)*PHFA(LASI)
C    *                              +      PMP2B(MUNU)*PHFB(LASI)
C    *                              +      PMP2A(LASI)*PHFA(MUNU)
C    *                              +      PMP2A(LASI)*PHFB(MUNU)
C    *                              +      PMP2B(LASI)*PHFA(MUNU)
C    *                              +      PMP2B(LASI)*PHFB(MUNU)
C    *                              -  PT5*PMP2A(MUSI)*PHFA(LANU)
C    *                              -  PT5*PMP2B(MUSI)*PHFB(LANU)
C    *                              -  PT5*PMP2A(MULA)*PHFA(NUSI)
C    *                              -  PT5*PMP2B(MULA)*PHFB(NUSI)
C    *                              -  PT5*PMP2A(NUSI)*PHFA(MULA)
C    *                              -  PT5*PMP2B(NUSI)*PHFB(MULA)
C    *                              -  PT5*PMP2A(LANU)*PHFA(MUSI)
C    *                              -  PT5*PMP2B(LANU)*PHFB(MUSI)
C                       SEPDFAC= SEPDFAC*F04
C
                        SF1 =(PMP2A(MUNU)+PHFA (MUNU)+
     2                        PMP2B(MUNU)+PHFB (MUNU))*PHFA(LASI)+
     3                       (PMP2A(MUNU)+PHFA (MUNU)+
     4                        PMP2B(MUNU)+PHFB (MUNU))*PHFB(LASI)+
     5                       (PMP2A(LASI)+PMP2B(LASI))*PHFA(MUNU)+
     6                       (PMP2A(LASI)+PMP2B(LASI))*PHFB(MUNU)
                        SH1 =(PMP2A(MUSI)+PHFA (MUSI))*PHFA(LANU)+
     2                       (PMP2A(MULA)+PHFA (MULA))*PHFA(NUSI)+
     3                       (PMP2B(MUSI)+PHFB (MUSI))*PHFB(LANU)+
     4                       (PMP2B(MULA)+PHFB (MULA))*PHFB(NUSI)+
     5                        PMP2A(NUSI)*PHFA(MULA)+
     6                        PMP2B(NUSI)*PHFB(MULA)+
     7                        PMP2A(LANU)*PHFA(MUSI)+
     8                        PMP2B(LANU)*PHFB(MUSI)
                        SEPDFAC=(SF1-PT5*SH1)
                        DF1= DF1+SEPDFAC
                        SEPDFAC= SEPDFAC*F04
                     END IF
                     DF1= DF1*F04
                     IF(NMU .EQ.NNU ) DF1= DF1*PT5
                     IF(NLA .EQ.NSI ) DF1= DF1*PT5
                     IF(MUNU.EQ.LASI) DF1= DF1*PT5
                     IF(DABMAX.LT. ABS(DF1)) DABMAX= ABS(DF1)
                     IJKL=IGXYZ(1,I)+JGXYZ(1,J)+KGXYZ(1,K)+LGXYZ(1,L)
                     DAB(IJKL)= DF1*P4L
                     IF(OUT) WRITE(IW,9020) II,JJ,KK,LL,I,J,K,L,IJKL,DF1
  210    CONTINUE
      END IF
C
      RETURN
 9010 FORMAT(' -DABUMP,POPLE- ',4I4,4I3,   D20.12)
 9020 FORMAT(' -DABUMP,HONDO- ',4I4,4I3,I5,D20.12)
      END
C*MODULE GRD2A   *DECK DDEBUT
      SUBROUTINE DDEBUT(IA,DA,DB,L1,L2,L3,NOCORE,NOOPEN,HFSCF,
     *                  UHFTYP,ROGVB)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXAO=2047)
C
      LOGICAL NOCORE,NOOPEN
      LOGICAL HFSCF,UHFTYP,ROGVB
C
      DIMENSION DA(L2),DB(L2),IA(*)
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /GVBWFN/ CICOEF(2,12),F(25),ALPHA(325),BETA(325),NO(10),
     *                NCO,NSETO,NOPEN,NPAIR,NMOGVB,NCONF(MXAO),NHAM
C
C     ---- READ IN 1 BODY DENSITY MATRICES FOR RHF,UHF,ROHF ----
C     DA = TOTAL (ALPHA+BETA) DENSITY, DB = SPIN (ALPHA-BETA) DENSITY
C
      IF(HFSCF) THEN
         CALL DAREAD(IDAF,IODA,DA,L2,16,0)
         IF(UHFTYP) THEN
            CALL DAREAD(IDAF,IODA,DB,L2,20,0)
            DO 120 I = 1,L2
               DUMA = DA(I)
               DUMB = DB(I)
               DA(I) = DUMA+DUMB
               DB(I) = DUMA-DUMB
  120       CONTINUE
         END IF
      END IF
C
C     ----- SET UP CORE DENSITY MATRIX IN -DA- FOR GVB -----
C     SNEAKY, THE MO VECTORS ARE STORED IN SQUARE DB.
C
      IF (ROGVB) THEN
         CALL DAREAD(IDAF,IODA,DB,L3,15,0)
         CALL DENCOR(DA,DB,IA,L1)
         NOCORE = NCO .EQ. 0
         NOOPEN = NSETO .EQ. 0 .AND. NPAIR .EQ. 0
      END IF
C
      RETURN
      END
C*MODULE GRD2A   *DECK DENCOR
      SUBROUTINE DENCOR(DA,V,IA,L1)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXAO=2047)
C
      COMMON /GVBWFN/ CICOEF(2,12),F(25),ALPHA(325),BETA(325),NO(10),
     *                NCO,NSETO,NOPEN,NPAIR,NMOGVB,NCONF(MXAO),NHAM
C
      DIMENSION DA(*),V(L1,*),IA(*)
C
C     ----- CALCULATE THE GVB CORE ORBITAL DENSITY MATRIX -----
C
      L2 = (L1*L1+L1)/2
      CALL VCLR(DA,1,L2)
      IF(NCO.EQ.0) RETURN
C
      DO 140 I = 1,L1
         DO 120 J = 1,I
            IJ = IA(I) + J
            DO 100 K = 1,NCO
               DA(IJ) = DA(IJ) + F(1)*V(I,K)*V(J,K)
  100       CONTINUE
  120    CONTINUE
  140 CONTINUE
      RETURN
      END
C*MODULE GRD2A   *DECK DFINAL
      SUBROUTINE DFINAL(INDEX)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500)
C
      COMMON /FUNCT / EHF,EG(3*MXATM)
      COMMON /GRAD  / DE(3*MXATM)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RESTAR/ TIMLIM,IREST,NREC,INTLOC,IST,JST,KST,LST
      COMMON /TMVALS/ TI,TX,TIM
C
C     ----- SAVE INCOMPLETE GRADIENT -----
C
      IF (INDEX .EQ. 1) GO TO 100
      IREST = 4
      CALL DAWRIT(IDAF,IODA,DE,3*NAT,3,0)
      CALL TSECND(TIM)
      IF (TIM .GE. TIMLIM .AND. MASWRK) WRITE (IW,9008)
     *    IREST,IST,JST,KST,LST
      RETURN
C
C     ----- GRADIENT VECTOR HAS BEEN COMPLETED -----
C
  100 CONTINUE
      NCOORD = 3*NAT
      CALL DCOPY(NCOORD,DE,1,EG,1)
      CALL DAWRIT(IDAF,IODA,EG,NCOORD,3,0)
      IREST = 5
      IST = 1
      JST = 1
      KST = 1
      LST = 1
      RETURN
C
 9008 FORMAT(1X,'WARNING.......THIS JOB MUST BE RESTARTED .....'/
     *       1X,'ENTER IREST=',I5,' IN $CONTRL'/
     *       1X,'ENTER IST,JST,KST,LST=',4I5,' IN $INTGRL')
      END
C*MODULE GRD2A   *DECK DFTGRD
      SUBROUTINE DFTGRD(FOCK,DERIV,V,G,WORK,H,GRADS,NAUXFUN)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      COMMON /ATLIM / LIMLOW(MXATM),LIMSUP(MXATM)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      DIMENSION FOCK(NUM+NAUXFUN,NUM+NAUXFUN)
      DIMENSION DERIV(NUM+NAUXFUN,NUM+NAUXFUN)
      DIMENSION V(NUM+NAUXFUN,NUM+NAUXFUN),G(NUM,NUM)
      DIMENSION H(NUM),WORK(NUM+NAUXFUN,NUM+NAUXFUN),GRADS(3,MXATM)
C     THIS VALUE DEPENDS ON HOW WE BUILD UP THE GRADIENT
      PARAMETER(VALUE=4.0D+00)
C     THIS VALUE IS THE NUMBER OF DIMENSIONS (3 WORKS WELL FOR EARTH)
      PARAMETER(NDIM=3)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA    ROHF  /8HROHF    /,UHF  /8HUHF     /
#else
      CHARACTER*8 :: ROHF_STR
      EQUIVALENCE (ROHF, ROHF_STR)
      CHARACTER*8 :: UHF_STR
      EQUIVALENCE (UHF, UHF_STR)
      DATA    ROHF_STR/"ROHF    "/,UHF_STR/"UHF     "/
#endif
C
      L2=(NUM+NUM*NUM)/2
      L3=NUM*NUM
      L4=NUM+NAUXFUN
      L5=(L4+L4*L4)/2
      L6=L4*L4
C
C     THIS APPROXIMATES I=1,OCCUPIED   ((DI/DX) FOCK(DFT ONLY) I)
C
      CALL AOLIM()
      CALL VCLR(GRADS,1,3*MXATM)
      IF(SCFTYP .EQ. ROHF .OR. SCFTYP .EQ. UHF) THEN
          NORBS=2
      ELSE
          NORBS=1
      END IF
      SCALE=VALUE/NORBS
C
C     READ IN LCAO COEF. V MATRIX
C
      DO 500 IAB=1,NORBS
        IF(IAB .EQ. 1) NOCC=NA
        IF(IAB .EQ. 2) NOCC=NB
        IF(NAUXFUN .EQ. 0) THEN
           CALL DAREAD (IDAF,IODA,V   ,L3,15+(IAB-1)*4,0)
        ELSE
           CALL DAREAD (IDAF,IODA,WORK,L3,15+(IAB-1)*4,0)
           CALL DFTRN4(WORK,V,NUM,NAUXFUN)
           CALL DAREAD(IDAF,IODA,
     *          V(1,NUM+1),L4*NAUXFUN,343,0)
        END IF
C
C     READ IN R MATRIX AND CONVERT TO MO'S
C
        CALL DAREAD(IDAF,IODA,FOCK,L6,340+(IAB-1)*1,0)
        CALL DFTTRN2(FOCK,V,WORK,L4)
C
C       READ IN DERIVATIVE MATRIX (I D/DX J) AND COVERT TO (MO D/DX AO)
C
        DO 1000 IXYZ=1,NDIM
          IF(NAUXFUN .EQ. 0) THEN
            CALL DAREAD (IDAF,IODA,DERIV,L2,84+(IXYZ-1)*1 ,0)
          ELSE
            CALL DAREAD (IDAF,IODA,DERIV,L5,344+(IXYZ-1)*1,0)
          END IF
          CALL EXPND (DERIV,WORK,L4,1)
          CALL MRTRBR(V,L4,L4,L4,WORK,L4,L4,DERIV,L4)
C
C         MAKE G(J,I)=SUM OF M  (M D/DX J)(M FOCK I)
C
          IF(NAUXFUN .EQ. 0) THEN
            CALL MRTRBR(DERIV,NUM,NUM,NUM,FOCK,NUM,NUM,G,NUM)
          ELSE
            DO 75 I=1,NUM
              DO 85 J=1,NUM
                G(J,I)=0.0D+00
                DO 95 M=1,L4
                   G(J,I)=G(J,I)+DERIV(M,J)*FOCK(M,I)
  95            CONTINUE
  85          CONTINUE
  75        CONTINUE
          END IF
C
C         MAKE H(J)=SUM OF I OCCUPIED  V(J,I)*G(J,I)
C
          CALL VCLR(H,1,NUM)
          DO 1500 I=1,NOCC
            DO 1501 J=1,NUM
              H(J)=H(J)+V(J,I)*G(J,I)
 1501       CONTINUE
 1500     CONTINUE
C
C         PLUG VALUE*H(J) INTO THE DERIVATIVE ON THE ATOM THAT HAS J
C         LOOP OVER ATOM J AND THEN ORBITALS I ON AROM J
C
          DO 2000 J=1,NAT
            DO 2001 I=LIMLOW(J),LIMSUP(J)
              GRADS(IXYZ,J)=GRADS(IXYZ,J)+SCALE*H(I)
 2001       CONTINUE
 2000     CONTINUE
 1000   CONTINUE
  500 CONTINUE
      END
C*MODULE GRD2A   *DECK JKDATM
      SUBROUTINE JKDATM(II,JJ,KK,LL)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MXSH=1000, MXGTOT=5000)
C
      LOGICAL DBUG
      LOGICAL OUT
      LOGICAL SKIPI,SKIPJ,SKIPK,SKIPL
      LOGICAL IANDJ,IANDK,IANDL,JANDK,JANDL,KANDL
      COMMON /DSHLT / RTOL,DTOL,VTOL1,VTOL2,VTOLS,OUT,DBUG
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON/DERSKP/IIAT,JJAT,KKAT,LLAT,SKIPI,SKIPJ,SKIPK,SKIPL
      COMMON/DERINV/INVTYP
C
      SKIPI=.TRUE.
      SKIPJ=.TRUE.
      SKIPK=.TRUE.
      SKIPL=.TRUE.
      IIAT=KATOM(II)
      JJAT=KATOM(JJ)
      KKAT=KATOM(KK)
      LLAT=KATOM(LL)
      IANDJ=IIAT.EQ.JJAT
      IANDK=IIAT.EQ.KKAT
      IANDL=IIAT.EQ.LLAT
      JANDK=JJAT.EQ.KKAT
      JANDL=JJAT.EQ.LLAT
      KANDL=KKAT.EQ.LLAT
C
      IF(.NOT.IANDJ) GO TO 500
      IF(.NOT.IANDK) GO TO 200
      IF(.NOT.IANDL) GO TO 100
C
C     ----- IAT = JAT = KAT = LAT ----- (IAT,IAT/IAT,IAT) -----
C
      INVTYP=1
      GO TO 1500
  100 CONTINUE
C
C     ----- IAT = JAT = KAT ; LAT ----- (IAT,IAT/IAT,LAT) -----
C
      SKIPL=.FALSE.
      INVTYP=2
      GO TO 1500
  200 IF(.NOT.IANDL) GO TO 300
C
C     ----- IAT = JAT = LAT ; KAT ----- (IAT,IAT/KAT,IAT) -----
C
      SKIPK=.FALSE.
      INVTYP=3
      GO TO 1500
  300 IF(.NOT.KANDL) GO TO 400
C
C     ----- IAT = JAT ; KAT = LAT ----- (IAT,IAT/KAT,KAT) -----
C
      SKIPK=.FALSE.
      SKIPL=.FALSE.
      INVTYP=4
      GO TO 1500
  400 CONTINUE
C
C     ----- IAT = JAT ; KAT ; LAT ----- (IAT,IAT/KAT,LAT) -----
C
      SKIPK=.FALSE.
      SKIPL=.FALSE.
      INVTYP=5
      GO TO 1500
  500 IF(.NOT.IANDK) GO TO 800
      IF(.NOT.IANDL) GO TO 600
C
C     ----- IAT = KAT = LAT ; JAT ----- (IAT,JAT/IAT,IAT) -----
C
      SKIPJ=.FALSE.
      INVTYP=6
      GO TO 1500
  600 IF(.NOT.JANDL) GO TO 700
C
C     ----- IAT = KAT ; JAT = LAT ----- (IAT,JAT/IAT,JAT) -----
C
      SKIPJ=.FALSE.
      SKIPL=.FALSE.
      INVTYP=7
      GO TO 1500
  700 CONTINUE
C
C     ----- IAT = KAT ; JAT ; LAT ----- (IAT,JAT/IAT,LAT) -----
C
      SKIPJ=.FALSE.
      SKIPL=.FALSE.
      INVTYP=8
      GO TO 1500
  800 IF(.NOT.IANDL) GO TO 1000
      IF(.NOT.JANDK) GO TO 900
C
C     ----- IAT = LAT ; JAT = KAT ----- (IAT,JAT/JAT,IAT) -----
C
      SKIPJ=.FALSE.
      SKIPK=.FALSE.
      INVTYP=9
      GO TO 1500
  900 CONTINUE
C
C     ----- IAT = LAT ; JAT , KAT ----- (IAT,JAT/KAT,IAT) -----
C
      SKIPJ=.FALSE.
      SKIPK=.FALSE.
      INVTYP=10
      GO TO 1500
 1000 IF(.NOT.JANDK) GO TO 1200
      IF(.NOT.JANDL) GO TO 1100
C
C     ----- IAT ; JAT = JAT = JAT ----- (IAT,JAT/JAT,JAT) -----
C
      SKIPI=.FALSE.
      INVTYP=11
      GO TO 1500
 1100 CONTINUE
C
C     ----- IAT ; JAT = KAT ; LAT ----- (IAT,JAT/JAT,LAT) -----
C
      SKIPI=.FALSE.
      SKIPL=.FALSE.
      INVTYP=12
      GO TO 1500
 1200 IF(.NOT.JANDL) GO TO 1300
C
C     ----- JAT = LAT ; IAT ; KAT ----- (IAT,JAT/KAT,JAT) -----
C
      SKIPI=.FALSE.
      SKIPK=.FALSE.
      INVTYP=13
      GO TO 1500
 1300 IF(.NOT.KANDL) GO TO 1400
C
C     ----- KAT = LAT ; IAT ; JAT ----- (IAT,JAT/KAT,KAT) -----
C
      SKIPI=.FALSE.
      SKIPJ=.FALSE.
      INVTYP=14
      GO TO 1500
 1400 CONTINUE
C
C     ----- IAT ; JAT ; KAT ; LAT ----- (IAT,JAT/KAT,LAT) -----
C
      SKIPI=.FALSE.
      SKIPJ=.FALSE.
      SKIPK=.FALSE.
      INVTYP=15
C
 1500 CONTINUE
C
C     ----- FOR DEBUGGING PURPOSES CALCULATE ALL TERMS -----
C
      IF(DBUG) THEN
        SKIPI=.FALSE.
        SKIPJ=.FALSE.
        SKIPK=.FALSE.
        SKIPL=.FALSE.
        INVTYP=16
      END IF
C
      IF(OUT) WRITE(IW,9999) II,JJ,KK,LL,SKIPI,SKIPJ,SKIPK,SKIPL
      RETURN
 9999 FORMAT(/,' -- II,JJ,KK,LL =',4I3,' SKIPI,J,K,L =',4L3)
      END
C*MODULE GRD2A   *DECK JKDER
      SUBROUTINE JKDER
C
C       THE DRIVER FOR THE TWO ELECTRON GRADIENT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION METHMC
C
      LOGICAL SKIPI,SKIPJ,SKIPK,SKIPL,NOCORE,NOOPEN
      LOGICAL HFSCF,UHFTYP,ROGVB,PACK2E,POPLE,HONDO
      LOGICAL GOPARR,DSKWRK,MASWRK,NXT
      LOGICAL SOME,OUT,DBG,MP2,MC,PER1,PER2,PER3,UMP2
      LOGICAL CANONC,FCORE,FORS,NOCI,EKT,LINSER,LCIS
C
      PARAMETER (MXGTOT=5000, MXSH=1000, MXATM=500, MXAO=2047)
      PARAMETER (MXRT=100, MXNORO=250)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION M0(48),M1(48),M2(48),M3(48)
C
      COMMON /DERSKP/ IIAT,JJAT,KKAT,LLAT,SKIPI,SKIPJ,SKIPK,SKIPL
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,SDET,SZDET,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORBDT,
     *                NADET,NBDET,KDET,KSTDET,IROOT,IPURES,MAXW1,NITER,
     *                MAXP,NCIDET,IGPDET,KSTSYM,NFTGCI
      COMMON /DFGRID/ DFTTHR,SW0,NDFTFG,NRAD,NTHE,NPHI,NRAD0,NTHE0,NPHI0
      COMMON /DFTPAR/ DFTTYP(20),EXENA,EXENB,EXENC,IDFT34,NAUXFUN,
     *                                                    NAUXSHL
      COMMON /DLT   / LAT,LBT,LCT,LDT
      COMMON /DSHLNO/ LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJD,KLD,IJ,KL
      COMMON /DSHLT / RTOL,DTOL,VTOL1,VTOL2,VTOLS,OUT,DBG
      COMMON /FMCOM / X(1)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /GRAD  / DE(3,MXATM)
      COMMON /GUGWFN/ NFZC,NMCC,NDOC,NAOS,NBOS,NALP,NVAL,NNEXT,NFZV,
     *                IFORS,IEXCIT,ICICI,NOIRR
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,IPOPLE
      COMMON /INTOPT/ ISCHWZ,IECP,NECP,IEFLD
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /MCINP / METHMC,CISTEP,ACURCY,ENGTOL,DAMP,MICIT,NWRDMC,
     *                NORBMC,NOROT(2,MXNORO),MOFRZ(15),NPFLG(10),
     *                NOFO,CANONC,FCORE,FORS,NOCI,EKT,LINSER
      COMMON /MP2DM2/ NOC1,NOC2,ISTEP,NOC3,NOC4
      COMMON /MP2PAR/ OSPT,TOL,METHOD,NWDMP2,MPPROP,NACORE,NBCORE,
     *                NOA,NOB,NORB,NBF,NOMIT,MOCPHF,MAXITC
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PAULMO/ CENTNM(MXFGPT),CENTCD(3,MXFGPT),NORBEF(MXFRG),
     *                NPBF(MXFRG),NTMO
      COMMON /RESTAR/ TIMLIM,IREST,NREC,INTLOC,IST,JST,KST,LST
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /SHLBAS/ MAXTYP,MAXNUM
      COMMON /SIMDAT/ NACC,NREJ,IGOMIN,NRPA,IBWM,NACCT,NREJT,NRPAT,
     *                NPRTGO,IDPUNC,IGOFLG
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                T(432),INVT(48),NT
      COMMON /TMVALS/ TI,TX,TIM
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      COMMON/DERMEM/IWFN,IXCH,INIJG,IGINT,IFINT,ISINT,IIJKLG,
     1 IDAB,ICHRG,IXY,IXZ,IYZ,IX,IY,IZ,ISJ,ISK,ISL,IGIJKL,IGNKL,IGNM,
     2 IDIJ,IDKL,IB00,IB01,IB10,IC00,ID00,IF00,
     3 IAAI,IAAJ,IBBK,IBBL,IFI,IFJ,IFK,IFL,
     4 ISII,ISJJ,ISKK,ISLL,ISIJ,ISIK,ISIL,ISJK,ISJL,ISKL,
     5 IDIJSI,IDIJSJ,IDKLSK,IDKLSL,IABV,ICV,IRW
      COMMON/INDD80/IMAX,JMAX,KKKMAX,LMAX
C
      PARAMETER (RLN10=2.30258D+00)
      PARAMETER (TEN=10.0D+00, ONE=1.0D+00)
      PARAMETER (TENM9=1.0D-09, TENM11=1.0D-11)
      PARAMETER (TENM20=1.0D-20, PT5=0.5D+00, TENM12=1.0D-12)
C
      DIMENSION LENSHL(5)
      DATA LENSHL/1,4,10,20,35/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK,GRD2,DEBUG/8HCHECK   ,8HGRD2    ,8HDEBUG   /
      DATA RHF,UHF,ROHF,GVB
     *    /8HRHF     ,8HUHF     ,8HROHF    ,8HGVB     /
#else
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      CHARACTER*8 :: GRD2_STR
      EQUIVALENCE (GRD2, GRD2_STR)
      DATA CHECK_STR,GRD2_STR,DEBUG_STR/"CHECK   ","GRD2    ",
     * "DEBUG   "/
      CHARACTER*8 :: RHF_STR
      EQUIVALENCE (RHF, RHF_STR)
      CHARACTER*8 :: ROHF_STR
      EQUIVALENCE (ROHF, ROHF_STR)
      CHARACTER*8 :: UHF_STR
      EQUIVALENCE (UHF, UHF_STR)
      CHARACTER*8 :: GVB_STR
      EQUIVALENCE (GVB, GVB_STR)
      DATA RHF_STR,UHF_STR,ROHF_STR,GVB_STR
     *    /"RHF     ","UHF     ","ROHF    ","GVB     "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RMC/8HMCSCF    /
      DATA ALDET,GUGA,GENCI,ORMAS
     *       /8HALDET   ,8HGUGA    ,8HGENCI   ,8HORMAS   /
#else
      CHARACTER*8 :: RMC_STR
      EQUIVALENCE (RMC, RMC_STR)
      CHARACTER*8 :: ALDET_STR
      EQUIVALENCE (ALDET, ALDET_STR)
      CHARACTER*8 :: GUGA_STR
      EQUIVALENCE (GUGA, GUGA_STR)
      CHARACTER*8 :: GENCI_STR
      EQUIVALENCE (GENCI, GENCI_STR)
      CHARACTER*8 :: ORMAS_STR
      EQUIVALENCE (ORMAS, ORMAS_STR)
      DATA RMC_STR/"MCSCF   " /
      DATA ALDET_STR,GUGA_STR,GENCI_STR,ORMAS_STR
     *       /"ALDET   ","GUGA    ","GENCI   ","ORMAS   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CIS/8HCIS     /
#else
      CHARACTER*8 :: CIS_STR
      EQUIVALENCE (CIS, CIS_STR)
      DATA CIS_STR/"CIS     "/
#endif
C
C     ----- THIS IS THE MAIN 2E- GRADIENT DRIVER -----
C
      DBG = EXETYP.EQ.DEBUG
      OUT = EXETYP.EQ.GRD2.OR.NPRINT.EQ.-4
      SOME = MASWRK  .AND.  NPRINT.NE.-5
      IF(SOME) WRITE (IW,9008)
C
C        INITIALIZE PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      MINE = -1
C
C     ----- SET STARTING PARAMETERS -----
C
      HONDO = .TRUE.
      POPLE = IPOPLE.EQ.1
C
C      CUTOFF IS THE SCHWARZ SCREENING CUT OFF
C      DABCUT IS THE TWO PARTICLE DENSITY CUT OFF
C
      CUTOFF=TENM9
      IF(.NOT.POPLE) CUTOFF=CUTOFF/TEN
      CUTOFF2=CUTOFF/2.0D+00
C
      ZBIG = 0.0D+00
      DO ISH=1,NSHELL
         I1=KSTART(ISH)
         I2=I1+KNG(ISH)-1
         DO IG=I1,I2
            IF(EX(IG).GT.ZBIG) ZBIG = EX(IG)
         ENDDO
      ENDDO
      DABCUT=TENM11
      IF(ZBIG.GT.1.0D+06) DABCUT = DABCUT/TEN
      IF(ZBIG.GT.1.0D+07) DABCUT = DABCUT/TEN
C
C      VTOLS ARE CUT OFFS USED BY THE POPLE PACKAGE
C      CURRENT VALUES ARE FROM HONDO 8, SEE G92 FOR OTHER POSSIBILITIES
C
      VTOL1 = TENM12
      VTOL2 = TENM12
      VTOLS = TENM20
      DTOL = TEN**(-ITOL)
      RTOL = RLN10*ITOL
C
C      INITIALIZE THE INTEGRAL BLOCK COUNTERS TO ZERO
C
      IISKIP1= 0
      IISKIP2= 0
      IDID = 0
C
      LCIS = .FALSE.
      IF(CITYP.EQ.CIS) LCIS = .TRUE.
C
      MC    = SCFTYP.EQ.RMC
      MP2   = SCFTYP.EQ.RHF .AND.  MPLEVL.EQ.2
      UMP2  = SCFTYP.EQ.UHF .AND.  MPLEVL.EQ.2
      HFSCF = MPLEVL.EQ.0   .AND.   CITYP.NE.CIS .AND.
     *        (SCFTYP.EQ.RHF .OR. SCFTYP.EQ.UHF .OR. SCFTYP.EQ.ROHF)
      UHFTYP=                     SCFTYP.EQ.UHF .OR. SCFTYP.EQ.ROHF
      ROGVB = SCFTYP.EQ.GVB
C
C     ----- SET POINTERS FOR PARTITIONING OF MEMORY -----
C
      L1 = NUM
      L2 = (NUM*NUM+NUM)/2
      L3 = NUM*NUM
      NSH2=(NSHELL*NSHELL+NSHELL)/2
C
      DO 100 I = 1,NUM
         IA(I) = (I*I-I)/2
  100 CONTINUE
C
C     ----- READ IN 1E-GRADIENT -----
C
      CALL DAREAD(IDAF,IODA,DE,3*NAT,3,0)
      IF (GOPARR) CALL DSCAL(3*NAT,ONE/NPROC,DE,1)
C
C     ----- GRID-DFT EXCHANGE-CORRELATION -----
C     -----    DERIVATIVE CONTRIBUTION    -----
C
      IF (NDFTFG.EQ.1) CALL DFTDER
C
C              CALCULATE THE LARGEST SHELL TYPE
C
      CALL BASCHK(MAXTYP)
      MAXSHL = LENSHL(MAXTYP+1)
C              DO AT LEAST AN L SHELL
      IF (MAXSHL.LT.4) MAXSHL=4
C
C       IF WE ARE USING THE POPLE PACKAGE AND DO NOT HAVE ANY SHELLS
C       LARGER THAN AN L-SHELL THEN SKIP THE SETUP FOR THE RYS PACKAGE
C
      IF (POPLE.AND.MAXTYP.LT.2) HONDO = .FALSE.
C
C      CALCULATE THE NUMBER OF MCSCF CORE AND ACTIVE ORBITALS
C
      NCI = 0
      IF(MC) THEN
         IF(CISTEP.EQ.ALDET) NCI = NACT
         IF(CISTEP.EQ.GENCI) NCI = NACT
         IF(CISTEP.EQ.ORMAS) NCI = NACT
         IF(CISTEP.EQ.GUGA)  NCI = NDOC+NAOS+NBOS+NALP+NVAL+NNEXT
         IF(CISTEP.EQ.ALDET) NMCCOR = NCORSV
         IF(CISTEP.EQ.GENCI) NMCCOR = NCORSV
         IF(CISTEP.EQ.ORMAS) NMCCOR = NCORSV
         IF(CISTEP.EQ.GUGA)  NMCCOR = NMCC
      END IF
C
C     FIGURE OUT THE MEMORY WE NEED FOR STORING DENSITY MATRIX
C     AND OTHER WAVEFUNCTION INFORMATION. -JKDMEM- ALLOCATES
C     MEMORY FOR DERIVATIVE COMPUTATION AND 2ND ORDER DENSITY
C     AFTER -LENGTH- WORDS.
C
      LENRHF = L2
      LENUHF = L2+L2
      LENGVB = L2+L3
      LENMC  = L2+L2+L3+(NCI*NCI*NCI*NCI)
      LENMC1 = NINTMX+NINTMX+(NCI*NCI+NCI)/2
      LENMC2 = MAXSHL*NCI   *NCI   *NCI
     *       + MAXSHL*MAXSHL*NCI   *NCI
     *       + MAXSHL*MAXSHL*MAXSHL*NCI
     *       + MAXSHL*MAXSHL*MAXSHL*MAXSHL
      LENMC  = LENMC+MAX(LENMC1,LENMC2)
      LENMP  = L2+L2+L3
      LENUMP = 2*LENMP
      LENCIS = L2+L2+L2+L3
C
                  LENGTH=LENRHF
      IF (UHFTYP) LENGTH=LENUHF
      IF (ROGVB)  LENGTH=LENGVB
      IF (MC)     LENGTH=LENMC
      IF (MP2)    LENGTH=LENMP
      IF (UMP2)   LENGTH=LENUMP
      IF (LCIS)   LENGTH=LENCIS
      IF (NTMO.GT.0) LENGTH = LENGTH + L2
C
C       CALCULATE THE AMOUNT OF MEMORY NEEDED AND SET THE POINTERS
C       FOR BOTH PACKAGES
C
      CALL VALFM(LOADFM)
      CALL JKDMEM(1,LOADFM,IADDR,LENGTH,MINXYZ,MAXXYZ,MINVEC,POPLE,
     *            MP2.OR.UMP2)
C
C     ----- CARRY OUT SET UP TASKS -----
C
      NOCORE = .FALSE.
      NOOPEN = .FALSE.
C
C          MEMORY ALLOCATION FOR MCSCF FUNCTIONS
C
      IF (MC) THEN
         NEED = IADDR - LOADFM -1
         CALL GETFM(NEED)
C
         K10  = IWFN
         K20  = K10 + L2
         K30  = K20 + L2
         K40  = K30 + L3
         K51  = K40 + NCI*NCI*NCI*NCI
         K61  = K51 + (NCI*NCI+NCI)/2
         K71  = K61 + NINTMX
         LAST = K71 + NINTMX
C
         IF (EXETYP .EQ. CHECK) GO TO 600
C
C     ----- READ MCSCF WAVEFUNCTION INFORMATION -----
C
         CALL JKDMC(X(K10),X(K20),X(K30),X(K40),
     *              X(K51),X(K61),X(K71),IA,NUM,NMCCOR,NCI)
C
         K50  = K40 + NCI   *NCI   *NCI   *NCI
         K60  = K50 + MAXSHL*NCI   *NCI   *NCI
         K70  = K60 + MAXSHL*MAXSHL*NCI   *NCI
         K80  = K70 + MAXSHL*MAXSHL*MAXSHL*NCI
         LAST = K80 + MAXSHL*MAXSHL*MAXSHL*MAXSHL
         CALL VCLR(X(K50),1,MAXSHL*NCI   *NCI   *NCI   )
         CALL VCLR(X(K60),1,MAXSHL*MAXSHL*NCI   *NCI   )
         CALL VCLR(X(K70),1,MAXSHL*MAXSHL*MAXSHL*NCI   )
         CALL VCLR(X(K80),1,MAXSHL*MAXSHL*MAXSHL*MAXSHL)
         CALL BASCHK(MAXANG)
         NSHLMX=((MAXANG+1)*(MAXANG+2))/2
         IF(NSHLMX.LE.4) NSHLMX=4
         NGIJKL = NSHLMX**4
         CALL VCLR(X(IDAB),1,NGIJKL)
      END IF
C
C     ----- CLOSED SHELL -MP2- WAVEFUNCTIONS -----
C
      IF (.NOT.MP2) GO TO 355
C
      NOASAV=NOA
      NOA=NOA-NACORE
      NOC1 = 1
      ISTEP= 0
      LPHF   = IWFN
      LPMP2  = LPHF  + L2
      LVEC   = LPMP2 + L2
      LAST   = LVEC  + L3
      LMPDM2 = IADDR
C
      NEED1= IADDR - LOADFM -1
      CALL GETFM(NEED1)
C
C        THE FINAL ARRAY IS THE MP2 DENSITY, WHICH IS PROPORTIONAL
C        TO N**4 WORDS.  THIS MUST BE SLICED UP INTO PASSES OVER
C        THE OCCUPIED ORBITALS, CONTAINING -NUMCO- EACH.
C
      CALL VALFM(LOADFM)
      CALL GOTFM(NGOTMX)
      NMIN = L2*L1*1
      IF(NMIN.GT.NGOTMX) THEN
         WRITE(IW,9997) NMIN-NGOTMX
         CALL ABRT
      END IF
      NUMCO=MIN(NOA,NGOTMX/(L2*L1))
C
      NMIN  = NEED1 + L2*L1*1
      NMAX  = NEED1 + L2*L1*NOA
      NUSED = NEED1 + L2*L1*NUMCO
      NPASS = (NOA-1)/NUMCO + 1
      IF(SOME) WRITE(IW,9998) NOA,NMIN,NMAX,NUSED,NPASS,NUMCO
      IF(NPASS.GT.1) THEN
         NINCR=L2*L1
         IF(SOME) WRITE(IW,9996) NINCR
      END IF
C
      CALL VALFM(LOADFM)
      LAST = LMPDM2 + L2*L1*NUMCO
      NEED2= LAST - LOADFM -1
      CALL GETFM(NEED2)
      NEED = NEED1 + NEED2
      IF(EXETYP.EQ.CHECK) GO TO 600
C
C     ----- READ WAVEFUNCTION INFORMATION -----
C     THIS POINT IS RETURNED TO FOR EACH PASS OVER OCCUPIED MO-S
C
  350 CONTINUE
      ISTEP = ISTEP+1
      NOC2 = NOC1+NUMCO-1
      NOC2 = MIN(NOA,NOC2)
      CALL JKDMP2(X(LPHF),X(LPMP2),X(LVEC),X(LMPDM2),L3,L2,L1)
C
      GO TO 375
C
C     ----- OPEN SHELL -MP2- WAVEFUNCTIONS -----
C
  355 CONTINUE
      IF (.NOT.UMP2) GO TO 365
C
      NOCA=NOA-NACORE
      NOCB=NOB-NBCORE
      NOC1 = 1
      NOC3 = 1
      ISTEP= 0
      LPHFA  = IWFN
      LPHFB  = LPHFA  + L2
      LPMP2A = LPHFB  + L2
      LPMP2B = LPMP2A + L2
      LVECA  = LPMP2B + L2
      LVECB  = LVECA  + L3
      LAST   = LVECB  + L3
      LMPDM2 = IADDR
C
      NEED1= IADDR - LOADFM -1
      CALL GETFM(NEED1)
C
C        LMPDM2 CONTAINS THE MP2 2 E- DENSITY, WHICH IS PROPORTIONAL
C        TO N**4 WORDS.  THIS MUST BE SLICED UP INTO PASSES OVER
C        THE OCCUPIED ORBITALS, CONTAINING -NUMCO- EACH.
C
      CALL VALFM(LOADFM)
      CALL GOTFM(NGOTMX)
      NMIN = L2*L1*2
      IF(NMIN.GT.NGOTMX) THEN
         WRITE(IW,9997) NMIN-NGOTMX
         CALL ABRT
      END IF
      NOCX=MAX(NOCA,NOCB)
      NUMCO=MIN(NOCX,NGOTMX/(L2*L1*2))
C
      NMIN  = NEED1 + L2*L1*2
      NMAX  = NEED1 + L2*L1*2*NOCX
      NUSED = NEED1 + L2*L1*2*NUMCO
      NPASS = (NOCX-1)/NUMCO + 1
      IF(SOME) WRITE(IW,9995) NOCA,NOCB,NMIN,NMAX,NUSED,NPASS,NUMCO
      IF(NPASS.GT.1) THEN
         NINCR=L2*L1*2
         IF(SOME) WRITE(IW,9996) NINCR
      END IF
C
      CALL VALFM(LOADFM)
      LMPDM2B = LMPDM2 + L2*L1*NUMCO
      LAST    = LMPDM2B+ L2*L1*NUMCO
      NEED2   = LAST - LOADFM -1
      CALL GETFM(NEED2)
      NEED    = NEED1 + NEED2
      IF(EXETYP.EQ.CHECK) GO TO 600
C
C     ----- READ WAVEFUNCTION INFORMATION -----
C     THIS POINT IS RETURNED TO FOR EACH PASS OVER OCCUPIED MO-S
C
  360 CONTINUE
      ISTEP = ISTEP+1
      NOC2  = MIN(NOCA,NOC1+NUMCO-1)
      NOC4  = MIN(NOCB,NOC3+NUMCO-1)
C
      CALL JKDUMP(X(LPHFA),X(LPMP2A),X(LVECA),X(LPHFB),X(LPMP2B),
     *            X(LVECB),X(LMPDM2),X(LMPDM2B),L3,L2,L1)
C
      GO TO 375
C
C     ----- CLOSED SHELL CIS WAVEFUNCTIONS -----
C
  365 CONTINUE
      IF (.NOT.LCIS) GO TO 375
C
      NOASAV=NOA
      NOA=NOA-NACORE
      NOC1  = 1
      NOC2  = NOA
      ISTEP = 1
C
      LPHF   = IWFN
      LPCIS  = LPHF  + L2
      LTCIS  = LPCIS + L2
      LAST   = LTCIS + L3
C
      NEED= LAST - LOADFM -1
      CALL GETFM(NEED)
C
      IF(EXETYP.EQ.CHECK) GO TO 600
C
C     ----- READ WAVEFUNCTION INFORMATION -----
C
      CALL JKDCIS(X(LPHF),X(LPCIS),X(LTCIS),L3,L2)
C
C     NOW HANDLE SCF (RHF, UHF, ROHF, GVB) WAVEFUNCTIONS
C
  375 CONTINUE
C
      IF (HFSCF  .OR.  ROGVB) THEN
         LDEN = IWFN
         LVEC = LDEN + L2
         LAST = LVEC + MAX(L2,L3)
C
C     ADD IN DFT MEMORY
C
         L4=NUM+NAUXFUN
         L6=L4*L4
         IF((IADDR-LOADFM).LT.(5*L6+L4+3*MXATM)  .AND.
     *       DFTTYP(1).NE.0.0D+00) THEN
C
C          GENERALLY, THE MEMORY THAT WE NEED FOR DFT IS LESS THAN THAT
C          NEEDED FOR NON-DFT RUNS, SO ALLOCATING IT IS NOT A PROBLEM
C          UNLESS IT'S A RUN WITH AUX FUNCTIONS
C
           IADDR=LOADFM+5*L6+L4+3*MXATM
           IDFT1=LOADFM+1
           IDFT2=IDFT1+L6
           IDFT3=IDFT2+L6
           IDFT4=IDFT3+L6
           IDFT5=IDFT4+L6
           IDFT6=IDFT5+L6
           IDFT7=IDFT6+L4
         ELSE
           IDFT1=LOADFM+1
           IDFT2=IDFT1+L6
           IDFT3=IDFT2+L6
           IDFT4=IDFT3+L6
           IDFT5=IDFT4+L6
           IDFT6=IDFT5+L6
           IDFT7=IDFT6+L4
         END IF
C
C        TOTAL USED FOR DFT = IDFT7+3*MXATM
C
         NEED=IADDR-LOADFM
         CALL GETFM(NEED)
         IF (EXETYP .EQ. CHECK) GO TO 600
C
C     ----- READ WAVEFUNCTION INFORMATION -----
C
         CALL DDEBUT(IA,X(LDEN),X(LVEC),L1,L2,L3,NOCORE,NOOPEN,
     *               HFSCF,UHFTYP,ROGVB)
C
         IF (NTMO.GT.0) THEN
            CALL DAREAD(IDAF,IODA,X(LVEC),L2,79,0)
         END IF
      END IF
C
C        READ IN THE EXCHANGE INTEGRALS FROM DISK. IF THEY WERE NOT
C        PREVIOUSLY COMPUTED, THEN JUST SET THE ARRAY TO ONE, WHICH
C        EFFECTIVELY DEACTIVATES THE SCHWARZ SCREENING
C
      IF(ISCHWZ.EQ.1) THEN
         CALL DAREAD(IDAF,IODA,X(IXCH),NSH2,54,0)
      ELSE
         DO 400 I=0,NSH2-1
            X(IXCH+I) = ONE
  400    CONTINUE
      END IF
C
C     ----- GET SYMMETRY MAPPING OF SHELLS -----
C
      CALL JKDSET
C
C     ----- PREPARE FOR USE OF G80 DERIVATIVE ROUTINES -----
C
      IF (POPLE) CALL GAMGEN(1)
C
C        SET UP THE 1-ELECTRON CHARGE DISTRIBUTION
C
      IF (HONDO) CALL OEDHND(X(INIJG),X(ICHRG))
C
C        SQUARE DTOL FOR USE IN JKDSPD
C
      DTOL = DTOL*DTOL
      NC=1
      LDF=1
C
C     ----- I SHELL -----
C
      DO 560 II = IST,NSHELL
         DO 160 IT = 1,NT
            ID = MAPSHL(II,IT)
            IF (ID .GT. II) GO TO 560
            M0(IT) = ID
  160    CONTINUE
         IF(MC.AND.NCI.GT.0) CALL DABMCI(X(K30),X(K40),X(K50),NCI,
     *                                   II,NUM,MAXSHL,NMCCOR)
C
C     ----- J SHELL -----
C
        J0 = JST
        DO 540 JJ = J0,II
          JST = 1
          DO 220 IT = 1,NT
           JD = MAPSHL(JJ,IT)
           IF (JD .GT. II) GO TO 540
           ID = M0(IT)
           IF (ID .GE. JD) GO TO 200
           ND = ID
           ID = JD
           JD = ND
  200      IF (ID .EQ. II .AND. JD .GT. JJ) GO TO 540
           M1(IT) = ID
           M2(IT) = JD
  220     CONTINUE
C
C     ----- GO PARALLEL! -----
C
          IF (NXT .AND. GOPARR) THEN
             MINE = MINE + 1
             IF (MINE.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
             IF (NEXT.NE.MINE) GO TO 540
          END IF
C
          IF (MC.AND.NCI.GT.0)
     1         CALL DABMCJ(X(K30),X(K50),X(K60),NCI,
     2                     II,JJ,NUM,MAXSHL,NMCCOR)
C
C        GET IJ CHARGE DISTRIBUTION
C        REALLY WE JUST SET THE POINTERS TO THE CHARGE DISTRIBUTION
C
          IF (HONDO) THEN
            IIJJ=IA(MAX0(II,JJ))+MIN0(II,JJ)
            CALL OEDRD(X(INIJG),NIJ,NIJ0,IIJJ)
            IF(NIJ.EQ.0) GO TO 540
          END IF
C
C     ----- K SHELL -----
C
          K0 = KST
          DO 520 KK = K0,II
            KST = 1
            DO 260 IT = 1,NT
             KD = MAPSHL(KK,IT)
             IF (KD .GT. II) GO TO 520
             M3(IT) = KD
  260       CONTINUE
            IF(MC.AND.NCI.GT.0)
     1         CALL DABMCK(X(K30),X(K60),X(K70),NCI,
     2                     II,JJ,KK,NUM,MAXSHL,NMCCOR)
C
C     ----- L SHELL -----
C
            L0 = LST
            MAXLL = KK
            IF (KK .EQ. II) MAXLL = JJ
            DO 500 LL = L0,MAXLL
              LST = 1
              N4 = 0
              DO 340 IT = 1,NT
               LD = MAPSHL(LL,IT)
               IF (LD .GT. II) GO TO 500
               KD = M3(IT)
               IF (KD .GE. LD) GO TO 300
               ND = KD
               KD = LD
               LD = ND
  300          ID = M1(IT)
               JD = M2(IT)
               IF (ID .NE. II .AND. KD .NE. II) GO TO 340
               IF (KD .LT. ID) GO TO 320
               IF (KD .EQ. ID .AND. LD .LE. JD) GO TO 320
               ND = ID
               ID = KD
               KD = ND
               ND = JD
               JD = LD
               LD = ND
  320          IF (JD .LT. JJ) GO TO 340
               IF (JD .GT. JJ) GO TO 500
               IF (KD .LT. KK) GO TO 340
               IF (KD .GT. KK) GO TO 500
               IF (LD .LT. LL) GO TO 340
               IF (LD .GT. LL) GO TO 500
               N4 = N4+1
  340         CONTINUE
C
C     ----- GO PARALLEL! -----
C
              IF ((.NOT.NXT) .AND. GOPARR) THEN
                 IPCOUNT = IPCOUNT + 1
                 IF (MOD(IPCOUNT,NPROC).NE.0) GO TO 500
              END IF
C
C     ----- CALCULATE Q4 FACTOR FOR THIS GROUP OF SHELLS -----
C
              Q4 = NT
              Q4 = Q4 / N4
C
C     ----- DECIDE ON DERIVATIVE INTEGRAL METHOD -----
C     ANY PURELY SP SET OF SHELLS CAN BE DONE WITH THE FASTER
C     POPLE/SCHLEGEL ROTATION ALGORITHM.  INTEGRALS INVOLVING
C     D AND HIGHER FUNCTIONS MUST USE RYS POLYNOMIAL CODE.
C
              POPLE = .TRUE.
              IF(IPOPLE.EQ.0) POPLE=.FALSE.
              IF(KTYPE(II).GT.2) POPLE=.FALSE.
              IF(KTYPE(JJ).GT.2) POPLE=.FALSE.
              IF(KTYPE(KK).GT.2) POPLE=.FALSE.
              IF(KTYPE(LL).GT.2) POPLE=.FALSE.
C
C         IMPLEMENT INTEGRAL SCREENING HERE USING EXCHANGE INTEGRALS
C         SEE H.HORN, H.WEISS, M.HAESER, M.EHRIG, R.AHLRICHS
C             J.COMPUT.CHEM. 12, 1058-1064(1991)
C         REGARDING THE ESTIMATION FORMULA (31) THAT IS USED HERE.
C
              IJIJ=IA(MAX0(II,JJ))+MIN0(II,JJ)
              KLKL=IA(MAX0(KK,LL))+MIN0(KK,LL)
              GMAX=(X(IXCH+IJIJ-1)*X(IXCH+KLKL-1))
C
C                COARSE SCREENING, ON JUST THE INTEGRAL VALUE
C
              IF (GMAX.LT.CUTOFF) THEN
                 IISKIP1 = IISKIP1+1
                 GO TO 500
              END IF
C
           ISH=II
           JSH=JJ
           KSH=KK
           LSH=LL
C
           IF (POPLE) THEN
             AX1=PT5
             IF(ISH.NE.JSH) AX1=AX1+AX1
             IF(KSH.NE.LSH) AX1=AX1+AX1
             IF(ISH.NE.KSH.OR.JSH.NE.LSH) AX1=AX1+AX1
             Q4 = Q4*AX1
             INEW=ISH
             JNEW=JSH
             KNEW=KSH
             LNEW=LSH
             IMAX=KTYPE(INEW)-1
             JMAX=KTYPE(JNEW)-1
             KKKMAX=KTYPE(KNEW)-1
             LMAX=KTYPE(LNEW)-1
             PER1=.FALSE.
             PER2=.FALSE.
             PER3=.FALSE.
             IF (IMAX.LT.JMAX) THEN
               INEW = JSH
               JNEW = ISH
               PER1 = .TRUE.
             END IF
             IF (KKKMAX.LT.LMAX) THEN
               KNEW = LSH
               LNEW = KSH
               PER2 = .TRUE.
             END IF
             IF ((IMAX+JMAX).LT.(KKKMAX+LMAX)) THEN
               ID = INEW
               INEW = KNEW
               KNEW = ID
               ID = JNEW
               JNEW = LNEW
               LNEW = ID
               PER3 = .TRUE.
             END IF
             IMAX=3*(KTYPE(INEW)-1)+1
             JMAX=3*(KTYPE(JNEW)-1)+1
             KKKMAX=3*(KTYPE(KNEW)-1)+1
             LMAX=3*(KTYPE(LNEW)-1)+1
             JTYPE=(IMAX+JMAX+KKKMAX+KKKMAX+LMAX-2)/3
             IAT = KATOM(INEW)
             JAT = KATOM(JNEW)
             KAT = KATOM(KNEW)
             LAT = KATOM(LNEW)
             IF ((IAT.EQ.JAT).AND.(IAT.EQ.KAT).AND.(IAT.EQ.LAT))
     1           GO TO 500
           ELSE
C
C     ----- GET -KL- CHARGE DISTRIBUTION -----
C       ACTUALLY JUST THE POINTERS
C
              KKLL=IA(MAX0(KK,LL))+MIN0(KK,LL)
              CALL OEDRD(X(INIJG),NKL,NKL0,KKLL)
              IF(NKL.EQ.0) GO TO 500
C
C     ----- SELECT CENTERS FOR DERIVATIVES -----
C
              CALL JKDATM(ISH,JSH,KSH,LSH)
              IF(SKIPI.AND.SKIPJ.AND.SKIPK.AND.SKIPL) GO TO 500
C
C     ----- SET INDICES FOR SHELL BLOCK -----
C
              CALL JKDSHL(ISH,JSH,KSH,LSH)
              CALL JKDNDX(X(IIJKLG))
              INEW = ISH
              JNEW = JSH
              KNEW = KSH
              LNEW = LSH
           END IF
C
C     ----- OBTAIN 2 BODY DENSITY FOR THIS SHELL BLOCK -----
C
         IF (NTMO.GT.0) THEN
            CALL DABPAU(INEW,JNEW,KNEW,LNEW,UHFTYP,X(LDEN),
     *                  X(LVEC),X(IDAB),DABMAX,Q4,POPLE)
            GO TO 111
         END IF
         IF(HFSCF .AND. DFTTYP(3).NE.1.0D+00)CALL DABDFT
     *                        (INEW,JNEW,KNEW,LNEW,UHFTYP,X(LDEN),
     *                         X(LVEC),X(IDAB),DABMAX,Q4,POPLE)
         IF(HFSCF .AND. DFTTYP(3).EQ.1.0D+00) CALL DABCLU
     *                        (INEW,JNEW,KNEW,LNEW,UHFTYP,X(LDEN),
     *                         X(LVEC),X(IDAB),DABMAX,Q4,POPLE)
         IF(ROGVB) CALL DABGVB(INEW,JNEW,KNEW,LNEW,NOCORE,NOOPEN,
     *                         X(LDEN),X(LVEC),NUM,X(IDAB),
     *                         DABMAX,Q4,POPLE)
         IF(MC) THEN
            IF(NCI.GT.0) CALL DABMCL(X(K30),X(K70),X(K80),NCI,
     *                               ISH,JSH,KSH,LSH,NUM,MAXSHL,NMCCOR)
            CALL DABMC(INEW,JNEW,KNEW,LNEW,X(K10),X(K20),X(K80),NMCCOR,
     *                 NCI,MAXSHL,X(IDAB),DABMAX,Q4,POPLE,
     *                 PER1,PER2,PER3)
         END IF
         IF(MP2) THEN
            CALL DABMP2(INEW,JNEW,KNEW,LNEW,X(LMPDM2),X(LVEC),X(LPHF),
     *                  X(LPMP2),X(IDAB),DABMAX,L1,L2,Q4,POPLE)
         END IF
         IF(UMP2) THEN
            CALL DABUMP(INEW,JNEW,KNEW,LNEW,X(LMPDM2),X(LMPDM2B),
     *                  X(LVECA),X(LVECB),X(LPHFA),X(LPHFB),X(LPMP2A),
     *                  X(LPMP2B),X(IDAB),DABMAX,L1,L2,Q4,POPLE)
         END IF
         IF(LCIS) THEN
            CALL DABCIS(INEW,JNEW,KNEW,LNEW,X(LPHF),X(LPCIS),
     *                  X(LTCIS),X(IDAB),DABMAX,L1,L2,Q4,POPLE)
         END IF
  111    CONTINUE
C
C                FINE SCREENING, ON INTEGRAL VALUE TIMES DENSITY FACTOR
C
         IF(DABMAX*GMAX.LT.CUTOFF2) THEN
            IISKIP2 = IISKIP2+1
            GO TO 500
         END IF
C
C     ----- EVALUATE DERIVATIVE INTEGRAL, AND ADD TO THE GRADIENT -----
C
         IDID = IDID+1
         IF(POPLE) THEN
            CALL JKDG80(DABMAX,INEW,JNEW,KNEW,LNEW,
     *                  JTYPE,IAT,JAT,KAT,LAT)
         ELSE
            CALL JKDSPD(NIJ0,NKL,NKL0,X(ICHRG),
     *      X(IGINT),X(IFINT),X(ISINT),X(IIJKLG),X(IGIJKL),
     1      X(IGNKL),X(IGNM),X(IXY),X(IXZ),X(IYZ),X(IX),X(IY),X(IZ),
     2      X(ISJ),X(ISK),X(ISL),X(IB00),X(IB01),X(IB10),X(IC00),
     3      X(ID00),X(IF00),X(IDIJ),X(IDKL),X(IDIJSI),X(IDIJSJ),
     4      X(IDKLSK),X(IDKLSL),X(IABV),X(ICV),X(IRW),X(IAAI),X(IAAJ),
     5      X(IBBK),X(IBBL),X(IFI),X(IFJ),X(IFK),X(IFL),X(ISII),X(ISJJ),
     6      X(ISKK),X(ISLL),X(ISIJ),X(ISIK),X(ISIL),X(ISJK),X(ISJL),
     7      X(ISKL),X(IDAB),MAXXYZ,FC,NC,DF,LDF,NBF,DDA,Q4,MINVEC,
     8      DABCUT,DABMAX)
         END IF
C
C     ----- END OF *SHELL* LOOPS -----
C
  500 CONTINUE
  520 CONTINUE
  540 CONTINUE
      IF (TIM .GE. TIMLIM) GO TO 600
  560 CONTINUE
C
C     ----- GO BACK FOR ANOTHER MP2 PASS, IF NECESSARY -----
C
      IF(MP2) THEN
         NOC1 = NOC2+1
         IF(NOC1.LE.NOA) GO TO 350
      END IF
      IF(UMP2) THEN
         NOC1 = NOC2+1
         NOC3 = NOC4+1
         IF(NOC1.LE.NOCA) GO TO 360
         IF(NOC3.LE.NOCB) GO TO 360
      END IF
C
C     ----- FINISH UP THE FINAL GRADIENT -----
C
C           GLOBAL SUM CONTRIBUTIONS FROM EACH NODE
C
      IF (GOPARR) THEN
         IF (NXT) CALL DDI_DLBRESET
         CALL DDI_GSUMF(1600,DE,3*NAT)
         CALL DDI_GSUMI(1601,IISKIP1,1)
         CALL DDI_GSUMI(1601,IISKIP2,1)
         CALL DDI_GSUMI(1602,IDID,1)
      END IF
C
C           ADD IN THE REST OF THE DFT GRADIENT
C
      IF(DFTTYP(1) .NE. 0.0D+00) THEN
         CALL DFTGRD(X(IDFT1),X(IDFT2),X(IDFT3),X(IDFT4),X(IDFT5),
     *               X(IDFT6),X(IDFT7),NAUXFUN)
         CALL VADD(X(IDFT7),1,DE,1,DE,1,3*NAT)
      END IF
C
C           SYMMETRIZE SKELETON GRADIENT VECTOR
C
      CALL SYMEG(DE)
C
C           PROJECT ROTATIONAL CONTAMINANT FROM DFT GRADIENTS
C
      IF(NDFTFG.EQ.1  .OR.  DFTTYP(1).NE.0.0D+00) THEN
         NCCF = 3*NAT + 6*NFRG
         CALL VALFM(LOADFM)
         LX   = LOADFM+ 1
         LGTOT= LX    + NCCF
         LRM  = LGTOT + NCCF
         LP   = LRM   + 3*NAT + 21*NFRG
         LAST = LP    + NCCF*NCCF
         NEEDP= LAST-LOADFM-1
         CALL GETFM(NEEDP)
         CALL PRJGRD(DE,X(LX),X(LGTOT),X(LRM),X(LP),3*NAT,NCCF)
         CALL RETFM(NEEDP)
      END IF
C
      CALL DFINAL(1)
C
C     ----- DEALLOCATE MEMORY -----
C
  600 CONTINUE
      IF(LAST.GT.0) LAST=0
      CALL RETFM(NEED)
      IF (MASWRK .AND. NPRTGO.NE.2) THEN
        IF(SOME) WRITE(IW,9999) IISKIP1,IISKIP2,IDID
        WRITE(IW,FMT='(/'' ...... END OF 2-ELECTRON GRADIENT ......'')')
      END IF
      CALL TEXIT(1,4)
      IF(MP2) NOA=NOASAV
      RETURN
C
 9008 FORMAT(/10X,22("-")/10X,"GRADIENT OF THE ENERGY"/10X,22("-"))
 9995 FORMAT(1X,'UMP2 MEMORY ALLOCATION FOR 2E- GRADIENT INTEGRALS.'/
     *       1X,'THERE ARE',I5,' ALPHA OCCUPIED MO-S AND',I5,
     *          ' BETA OCCUPIED MO-S'/
     *       1X,'MINIMUM MEMORY =',I15,
     *          ' (ONE OCCUPIED MO/DERIVATIVE INTEGRAL PASS)'/
     *       1X,'MAXIMUM MEMORY =',I15,
     *          ' (ALL OCCUPIED MO-S IN A SINGLE PASS)'/
     *       1X,'   MEMORY USED =',I15,', WITH ',I5,
     *          ' DERIVATIVE PASSES,',I5,' MO-S/PASS')
 9996 FORMAT(1X,'INCREASE MEMORY ',I15,
     *          ' WORDS TO REDUCE PASS COUNT BY ONE.')
 9997 FORMAT(/' NOT ENOUGH MEMORY FOR -2DM- CONTRIBUTION'/
     *        ' INCREASE MEMORY SIZE BY AT LEAST ',I10,' WORDS')
 9998 FORMAT(1X,'MP2 MEMORY ALLOCATION FOR 2E- GRADIENT INTEGRALS.  (',
     *          I5,' OCCUPIED MO-S)'/
     *       1X,'MINIMUM MEMORY =',I15,
     *          ' (ONE OCCUPIED MO/DERIVATIVE INTEGRAL PASS)'/
     *       1X,'MAXIMUM MEMORY =',I15,
     *          ' (ALL OCCUPIED MO-S IN A SINGLE PASS)'/
     *       1X,'   MEMORY USED =',I15,', WITH ',I5,
     *          ' DERIVATIVE PASSES,',I5,' MO-S/PASS')
 9999 FORMAT(1X,'THE COARSE/FINE SCHWARZ SCREENINGS SKIPPED ',I10,'/',
     *          I10,' BLOCKS.'/
     *     1X,'THE NUMBER OF GRADIENT INTEGRAL BLOCKS COMPUTED WAS',I10)
      END
C*MODULE GRD2A   *DECK JKDMC
      SUBROUTINE JKDMC(DC,DV,V,GIJKL,GIJ,IX,XX,IA,NUM,NMCC,NCI)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL PACK2E,GOPARR,DSKWRK,MASWRK,DWSAVE,FOCAS,SOSCF,DROPC
      COMMON /CASOPT/ CASHFT,CASDII,NRMCAS,FOCAS,SOSCF,DROPC
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG80
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCKLAB/ LABSIZ
C
      DIMENSION GIJKL(NCI,NCI,NCI,NCI),GIJ(*),DC(*),DV(*),V(NUM,*),IA(*)
      DIMENSION XX(*),IX(*)
C
      PARAMETER (ZERO=0.0D+00)
C
C      READ IN THE MCSCF FIRST ORDER DENSITY MATRIX
C
      NORBS = NCI + NMCC
      NGIJ = (NORBS*NORBS+NORBS)/2
      CALL DAREAD(IDAF,IODA,DC,NGIJ,68,0)
C
C      NOW REODER GIJ TO GET RID OF THE CORE DENSITY
C
      N = 1
      DO 10 I=(NMCC+1),NORBS
        DO 10 J=(NMCC+1),I
          IJ = IA(I) + J
          GIJ(N) = DC(IJ)
          N = N + 1
   10 CONTINUE
C
      IF (DROPC) THEN
         NCORE = NMCC
         NMCC = 0
      END IF
C
C     ----- READ ACTIVE ORBITAL SECTION OF 2E- DENSITY FROM DISK -----
C     EACH NODE MAY OR MAY NOT HAVE A FULL COPY OF THIS MATRIX,
C     DEPENDING ON THE METHOD USED, SO IT IS SAFEST TO READ ON
C     THE MASTER NODE ONLY, AND BROADCAST IT.
C
      DWSAVE=DSKWRK
      DSKWRK=.FALSE.
C
      IF(MASWRK) THEN
         CALL VCLR(GIJKL,1,NCI*NCI*NCI*NCI)
         CALL SEQREW(NFT15)
  100    CONTINUE
         CALL PREAD(NFT15,XX,IX,NXX,NINTMX)
         IF(NXX.NE.0) THEN
            MGIJKL=IABS(NXX)
            IF(MGIJKL.GT.NINTMX) CALL ABRT
            DO 110 MG=1,MGIJKL
               GGIJKL=XX(MG)
C
               NPACK = MG
               IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                  LABEL1 = IX( 2*NPACK - 1 )
                  LABEL2 = IX( 2*NPACK     )
                  IPACK = ISHFT( LABEL1, -16 )
                  JPACK = IAND( LABEL1, 65535 )
                  KPACK = ISHFT( LABEL2, -16 )
                  LPACK = IAND( LABEL2, 65535 )
#endif
#if defined(SPEC_CPU_ILP64)
                  LABEL = IX(NPACK)
                  IPACK = ISHFT( LABEL, -48 )
                  JPACK = IAND( ISHFT( LABEL, -32 ), 65535 )
                  KPACK = IAND( ISHFT( LABEL, -16 ), 65535 )
                  LPACK = IAND( LABEL, 65535 )
#endif
               ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                  LABEL = IX(NPACK)
                  IPACK = ISHFT( LABEL, -24 )
                  JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
                  KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
                  LPACK = IAND( LABEL, 255 )
#endif
#if defined(SPEC_CPU_ILP64)
                  IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                     LABEL = IX( NPACK/2 )
                     IPACK = IAND( ISHFT( LABEL, -24 ), 255 )
                     JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
                     KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
                     LPACK = IAND( LABEL, 255 )
                  ELSE
                     LABEL = IX( (NPACK/2)+1 )
                     IPACK = ISHFT( LABEL, -56 )
                     JPACK = IAND( ISHFT( LABEL, -48 ), 255 )
                     KPACK = IAND( ISHFT( LABEL, -40 ), 255 )
                     LPACK = IAND( ISHFT( LABEL, -32 ), 255 )
                  END IF
#endif
               END IF
               IG = IPACK - NMCC
               JG = JPACK - NMCC
               KG = KPACK - NMCC
               LG = LPACK - NMCC
C
               IF (IG.LT.1) GO TO 110
               IF (JG.LT.1) GO TO 110
               IF (KG.LT.1) GO TO 110
               IF (LG.LT.1) GO TO 110
               GIJKL(LG,KG,IG,JG) = GGIJKL
               GIJKL(LG,KG,JG,IG) = GGIJKL
               GIJKL(KG,LG,IG,JG) = GGIJKL
               GIJKL(KG,LG,JG,IG) = GGIJKL
               GIJKL(IG,JG,KG,LG) = GGIJKL
               GIJKL(IG,JG,LG,KG) = GGIJKL
               GIJKL(JG,IG,KG,LG) = GGIJKL
               GIJKL(JG,IG,LG,KG) = GGIJKL
  110       CONTINUE
            IF(NXX.GT.0) GO TO 100
         END IF
      END IF
C
      IF(GOPARR) CALL DDI_BCAST(2009,'F',GIJKL,NCI*NCI*NCI*NCI,MASTER)
      DSKWRK = DWSAVE
C
      NUM3= NUM*NUM
      CALL DAREAD(IDAF,IODA,V,NUM3,15,0)
C
C     -----  FORM CORE 1E- DENSITY IN AO BASIS -----
C
      IF(DROPC) NMCC = NCORE
      IF(NMCC.GT.0) THEN
         DO I=1,NUM
            DO J=1,I
               DUM=ZERO
               DO K=1,NMCC
                  DUM=DUM+V(I,K)*V(J,K)
               ENDDO
               IJ=IA(I)+J
               DC(IJ)=DUM+DUM
            ENDDO
         ENDDO
      END IF
C
C     ---- BACKTRANSFORM 1E- DENSITY OVER ACTIVE ORBS TO AO BASIS ----
C
      IF (NCI.GT.0) THEN
         DO I=1,NUM
            DO J=1,I
               DUM=ZERO
               DO K=1,NCI
                  DO L=1,NCI
                     KL=IA(MAX(K,L))+MIN(K,L)
                     DUM=DUM+V(I,K+NMCC)*V(J,L+NMCC)*GIJ(KL)
                  ENDDO
               ENDDO
               IJ=IA(I)+J
               DV(IJ)=DUM
            ENDDO
         ENDDO
      END IF
      RETURN
      END
C*MODULE GRD2A   *DECK JKDSET
      SUBROUTINE JKDSET
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MXGTOT=5000, MXSH=1000, MXATM=500)
C
      LOGICAL SP
C
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                T(432),INVT(48),NT
C
      DIMENSION MI(48)
      DATA NTYP  /6/
C
      DO 270 IITYP=1,NTYP
        DO 260 II=1,NSHELL
          LIT=KTYPE(II)
          SP =KTYPE(II).EQ.2.AND.KMIN(II).EQ.1
          IF(SP) LIT=NTYP
          IF(LIT.NE.IITYP) GO TO 260
          DO 210 IT=1,NT
            ID=MAPSHL(II,IT)
            IF( ID.GT.II   ) GO TO 260
  210     MI(IT)=ID
C
          IF(NT.EQ.1) GO TO 260
          DO 230 IT=2,NT
            MAX=IT-1
            DO 220 JT=1,MAX
              IF(MI(JT).NE.MI(IT)) GO TO 220
              MI(IT)=0
  220       CONTINUE
  230     CONTINUE
C
  260   CONTINUE
  270 CONTINUE
C
      RETURN
      END
C*MODULE GRD2A   *DECK JKDSHL
      SUBROUTINE JKDSHL(ISH,JSH,KSH,LSH)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MXGTOT=5000, MXSH=1000)
C
      LOGICAL IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
      LOGICAL SPI,SPJ,SPK,SPL,SPIJ,SPKL,SPIJKL
      LOGICAL EXPNDI,EXPNDK
      COMMON /DSHLNO/ LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJD,KLD,IJ,KL
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON/SHLTYP/SPI,SPJ,SPK,SPL,SPIJ,SPKL,SPIJKL
      COMMON/SHLXPN/EXPNDI,EXPNDK
      COMMON/SHLNUM/NUMI,NUMJ,NUMK,NUML,IJKL
      COMMON/SHLEQU/IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
C
      IIEQJJ=ISH.EQ.JSH
      KKEQLL=KSH.EQ.LSH
      IJEQKL=ISH.EQ.KSH.AND.JSH.EQ.LSH
      IJGTKL=MAX0(ISH,JSH).GT.MAX0(KSH,LSH)
      IJLTKL=MAX0(ISH,JSH).LT.MAX0(KSH,LSH)
C
C     ----- ISHELL -----
C
      LIT=KTYPE(ISH)
      MINI=KMIN(ISH)
      MAXI=KMAX(ISH)
      NUMI=MAXI-MINI+1
      LOCI=KLOC(ISH)-MINI
      SPI=LIT.EQ.2.AND.MINI.EQ.1
C
C     ----- JSHELL -----
C
      LJT=KTYPE(JSH)
      MINJ=KMIN(JSH)
      MAXJ=KMAX(JSH)
      NUMJ=MAXJ-MINJ+1
      LOCJ=KLOC(JSH)-MINJ
      SPJ=LJT.EQ.2.AND.MINJ.EQ.1
      SPIJ=SPI.OR.SPJ
      EXPNDI=LIT.GE.LJT
C
C     ----- KSHELL -----
C
      LKT=KTYPE(KSH)
      MINK=KMIN(KSH)
      MAXK=KMAX(KSH)
      NUMK=MAXK-MINK+1
      LOCK=KLOC(KSH)-MINK
      SPK=LKT.EQ.2.AND.MINK.EQ.1
C
C     ----- LSHELL -----
C
      LLT=KTYPE(LSH)
      MINL=KMIN(LSH)
      MAXL=KMAX(LSH)
      NUML=MAXL-MINL+1
      LOCL=KLOC(LSH)-MINL
      SPL=LLT.EQ.2.AND.MINL.EQ.1
      SPKL=SPK.OR.SPL
      SPIJKL=SPIJ.OR.SPKL
      EXPNDK=LKT.GE.LLT
C
      RETURN
      END
C*MODULE GRD2A   *DECK JKDNDX
      SUBROUTINE JKDNDX(IJKLG)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL SKIPI,SKIPJ,SKIPK,SKIPL
      LOGICAL IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
C--   LOGICAL IEQJ0,KEQL0,SAME0,FIRST
      COMMON /DSHLNO/ LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJD,KLD,IJ,KL
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON/SHLNUM/NUMI,NUMJ,NUMK,NUML,IJKL
      COMMON/SHLLMN/IGXYZ(4,35),JGXYZ(4,35),KGXYZ(4,35),LGXYZ(4,35)
      COMMON/SHLEQU/IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
C--   COMMON/SHLOLD/NUMI0,NUMJ0,NUMK0,NUML0,IEQJ0,KEQL0,SAME0,FIRST
      COMMON/DERPAR/IDER,JDER,KDER,LDER,NDER
      COMMON/DERSKP/IIAT,JJAT,KKAT,LLAT,SKIPI,SKIPJ,SKIPK,SKIPL
      DIMENSION IJKLX(35),IJKLY(35),IJKLZ(35)
      DIMENSION IJKLN(5)
      DIMENSION IJKLG(4,*)
C
      DATA IJKLN /   1,  4, 10, 20, 35/
      DATA IJKLX /   0,  1,  0,  0,  2,  0,  0,  1,  1,  0,
     1               3,  0,  0,  2,  2,  1,  0,  1,  0,  1,
     2               4,  0,  0,  3,  3,  1,  0,  1,  0,  2,
     3               2,  0,  2,  1,  1/
      DATA IJKLY /   0,  0,  1,  0,  0,  2,  0,  1,  0,  1,
     1               0,  3,  0,  1,  0,  2,  2,  0,  1,  1,
     2               0,  4,  0,  1,  0,  3,  3,  0,  1,  2,
     3               0,  2,  1,  2,  1/
      DATA IJKLZ /   0,  0,  0,  1,  0,  0,  2,  0,  1,  1,
     1               0,  0,  3,  0,  1,  0,  1,  2,  2,  1,
     2               0,  0,  4,  0,  1,  0,  1,  3,  3,  0,
     3               2,  2,  1,  1,  2/
C
C...  SAMTYP=NUMI.EQ.NUMI0.AND.NUMJ.EQ.NUMJ0.AND.
C... 1       NUMK.EQ.NUMK0.AND.NUML.EQ.NUML0          .AND.(.NOT.FIRST)
C...  SAMSYM=(IIEQJJ.EQV.IEQJ0).AND.
C... 1       (KKEQLL.EQV.KEQL0).AND.(IJEQKL.EQV.SAME0).AND.(.NOT.FIRST)
C--   NUMI0=NUMI
C--   NUMJ0=NUMJ
C--   NUMK0=NUMK
C--   NUML0=NUML
C--   IEQJ0=IIEQJJ
C--   KEQL0=KKEQLL
C--   SAME0=IJEQKL
C--   FIRST=.FALSE.
C.... IF(SAMTYP.AND.SAMSYM) RETURN
C
      IDER=NDER
      JDER=NDER
      KDER=NDER
      LDER=NDER
      IF(SKIPI) IDER=0
      IF(SKIPJ) JDER=0
      IF(SKIPK) KDER=0
      IF(SKIPL) LDER=0
      LJTMOD=LJT + JDER
      LKTMOD=LKT + KDER
      LLTMOD=LLT + LDER
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS -----
C
      NI=NUML*NUMK*NUMJ
      DO 10 I=MINI,MAXI
   10 IGXYZ(1,I)=NI*(I-MINI)+1
      LLKJT=LLTMOD*LKTMOD*LJTMOD
      DO 20 I=1,IJKLN(LIT)
        IGXYZ(2,I)=IJKLX(I)*LLKJT+1
        IGXYZ(3,I)=IJKLY(I)*LLKJT+1
   20   IGXYZ(4,I)=IJKLZ(I)*LLKJT+1
C
      NJ=NUML*NUMK
      DO 30 J=MINJ,MAXJ
   30 JGXYZ(1,J)=NJ*(J-MINJ)
      LLKT=LLTMOD*LKTMOD
      DO 40 J=1,IJKLN(LJT)
        JGXYZ(2,J)=IJKLX(J)*LLKT
        JGXYZ(3,J)=IJKLY(J)*LLKT
   40   JGXYZ(4,J)=IJKLZ(J)*LLKT
C
C     ----- PREPARE INDICES FOR PAIRS OF (K,L) FUNCTIONS -----
C
      NK=NUML
      DO 110 K=MINK,MAXK
  110 KGXYZ(1,K)=NK*(K-MINK)
      DO 120 K=1,IJKLN(LKT)
        KGXYZ(2,K)=IJKLX(K)*LLTMOD
        KGXYZ(3,K)=IJKLY(K)*LLTMOD
  120   KGXYZ(4,K)=IJKLZ(K)*LLTMOD
C
      NL=1
      DO L=MINL,MAXL
         LGXYZ(1,L)=NL*(L-MINL)
      ENDDO
      DO L=1,IJKLN(LLT)
         LGXYZ(2,L)=IJKLX(L)
         LGXYZ(3,L)=IJKLY(L)
         LGXYZ(4,L)=IJKLZ(L)
      ENDDO
C
C     ----- PREPARE INDICES FOR (IJ/KL) -----
C
      IJKL=0
      DO 240 I=MINI,MAXI
        JMAX=MAXJ
        IF(IIEQJJ) JMAX=I
        DO 230 J=MINJ,JMAX
          KMAX=MAXK
          IF(IJEQKL) KMAX=I
          DO 220 K=MINK,KMAX
            LMAX=MAXL
            IF(KKEQLL           ) LMAX=K
            IF(IJEQKL.AND.K.EQ.I) LMAX=J
            DO 210 L=MINL,LMAX
              IJKL=IJKL+1
              NN=((IGXYZ(1,I)+JGXYZ(1,J))+KGXYZ(1,K))+LGXYZ(1,L)
              NX=((IGXYZ(2,I)+JGXYZ(2,J))+KGXYZ(2,K))+LGXYZ(2,L)
              NY=((IGXYZ(3,I)+JGXYZ(3,J))+KGXYZ(3,K))+LGXYZ(3,L)
              NZ=((IGXYZ(4,I)+JGXYZ(4,J))+KGXYZ(4,K))+LGXYZ(4,L)
              IJKLG(1,IJKL)=   NN
              IJKLG(2,IJKL)=3*(NX-1)+1
              IJKLG(3,IJKL)=3*(NY-1)+2
              IJKLG(4,IJKL)=3*(NZ-1)+3
  210       CONTINUE
  220     CONTINUE
  230   CONTINUE
  240 CONTINUE
C
C     ----- SET NUMBER OF QUADRATURE POINTS -----
C
      NROOTS=(LIT+LJT+LKT+LLT-2 + NDER )/2
C
      RETURN
      END
C*MODULE GRD2A   *DECK JKDMEM
      SUBROUTINE JKDMEM(MDER,LOADFM,IADDR,LENGTH,MINXYZ,MAXXYZ,MINVEC,
     *                  POPLE,MP2)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXGTOT=5000, MXSH=1000)
C
      LOGICAL SOME,OUT,DBG,NORM,POPLE,MP2,GOPARR,DSKWRK,MASWRK
C
      DIMENSION LENSHL(5)
C
      COMMON /DSHLT / RTOL,DTOL,VTOL1,VTOL2,VTOLS,OUT,DBG
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON/SHLBAS/MAXTYP,MAXNUM
      COMMON/SHLNRM/PNRM(35)
      COMMON/DERPAR/IDER,JDER,KDER,LDER,NDER
      COMMON/DERMEM/IWFN,IXCH,INIJG,IGINT,IFINT,ISINT,IIJKLG,
     1 IDAB,ICHRG,IXY,IXZ,IYZ,IX,IY,IZ,ISJ,ISK,ISL,IGIJKL,IGNKL,IGNM,
     2 IDIJ,IDKL,IB00,IB01,IB10,IC00,ID00,IFXX,
     3 IAAI,IAAJ,IBBK,IBBL,IFI,IFJ,IFK,IFL,
     4 ISII,ISJJ,ISKK,ISLL,ISIJ,ISIK,ISIL,ISJK,ISJL,ISKL,
     5 IDIJSI,IDIJSJ,IDKLSK,IDKLSL,IABV,ICV,IRW
C
      PARAMETER (LENVEC=255)
      PARAMETER(ONE=1.0D+00,SQRT3=1.73205080756888D+00)
      PARAMETER(SQRT5=2.23606797749979D+00,SQRT7=2.64575131106459D+00)
      DATA LENSHL /1,4,10,20,35/
C
      SOME = MASWRK  .AND.  NPRINT.NE.-5
C
C       FIND OUT HOW MUCH MEMORY IS AVAILABLE
C
      CALL GOTFM(NGOTMX)
C
      MAXVEC = LENVEC/3
      MINVEC = IGETGRDVECLEN(MAXVEC)
C
C     ----- THIS PROGRAM FOR DERIVATIVES -----
C
      NDER=MDER
      IF(NDER.NE.1.AND.NDER.NE.2) NDER=0
      NDER0=0
      NDER1=0
      NDER2=0
      IF(OUT              ) NDER0=1
      IF(OUT.AND.NDER.GE.1) NDER1=12
      IF(OUT.AND.NDER.EQ.2) NDER2=78
      IF(        NDER.EQ.2) NDER1=12
C
C     ----- CHECK MAXIMUM ANGULAR MOMENTUM -----
C
      CALL BASCHK(LMAX)
      MAXTYP=LMAX+1
      IF (MAXTYP.GT.5) THEN
         WRITE(IW,9999)
         CALL ABRT
      END IF
      MINXYZ=(4*MAXTYP -2 +NDER)/2
C
C     ----- GET NUMBER OF PRIMITIVE CHARGE DISTRIBUTIONS -----
C
      NIJG=0
      DO 20 II=1,NSHELL
        DO 20 JJ=1,II
          NIJG=NIJG+KNG(II)*KNG(JJ)
   20 CONTINUE
C
C     -----  AT THIS POINT IT IS GOOD TO REMEMBER THAT    -----
C            -MAXTYP- = HIGHEST SHELL ANGULAR MOMENTUM
C            -MAXFUN- = NUMBER OF FUNCTIONS WITH ANGULAR
C                       MOMENTUM LESS OR EQUAL TO -MAXTYP-
C            -MAXNUM- = NUMBER OF FUNCTIONS WITH ANGULAR
C                       MOMENTUM         EQUAL TO -MAXTYP-
C            -MAXXYZ- = MAXIMUM NUMBER OF PRIMITIVE INTEGRALS
C                       THAT CAN BE HANDLED IN ONE -VECTOR-
C            -NUMXYZ- = ACTUAL MAXIMUM LENGTH OF ONE -VECTOR-
C            -MAXXYZ- = IT IS NUMXYZ/3 . SINCE THE X, Y, AND Z
C                       COMPONENTS ARE TREATED AS A SINGLE VECTOR,
C                       -MAXXYZ- IS THE NUMBER OF (PRIMITIVE-ROOTS)
C                       COMBINATIONS WHICH CAN BE TREATED IN ONE
C                       VECTOR. FOR -SSSS- INTEGRALS WHICH REQUIRE
C                       ONE RYS ROOT, MAXXYZ HAPPENS TO COINCIDE WITH
C                       THE NUMBER OF PRIMITIVE INTEGRALS TREATED IN
C                       ONE VECTOR. FOR -DDDD- INTEGRALS WHICH
C                       REQUIRE FIVE RYS ROOTS, THE NUMBER OF PRIMITIVE
C                       INTEGRALS TREATED IN ONE VECTOR IS -MAXXYZ-/5 .
C
C
C     ----- SET NORMALIZATION CONSTANTS -----
C
      MAXFUN=LENSHL(MAXTYP)
      DO 100 I=1,MAXFUN
  100 PNRM(I)=ONE
      NORM=NORMF.NE.1.OR.NORMP.NE.1
      IF(.NOT.NORM) GO TO 180
C
      SQRT53=SQRT5/SQRT3
      DO 170 I=1,MAXFUN
        IF (I.EQ.1.OR.I.EQ.2.OR.I.EQ.5.OR.I.EQ.11.OR.I.EQ.21) THEN
           FI = ONE
        ELSE IF (I.EQ.8.OR.I.EQ.20.OR.I.EQ.33) THEN
           FI=FI*SQRT3
        ELSE IF (I.EQ.14) THEN
           FI=FI*SQRT5
        ELSE IF (I.EQ.24) THEN
           FI=FI*SQRT7
        ELSE IF (I.EQ.30) THEN
           FI=FI*SQRT53
        END IF
        PNRM(I)=FI
  170 CONTINUE
C
C        ALWAYS ALLOCATE -L- SHELLS IF -P- IS HIGHEST ANGULAR MOMENTUM
C
  180 CONTINUE
      IDUM=MAXTYP-1
      MAXNUM=((IDUM+1)*(IDUM+2))/2
      IF(MAXNUM.LT.4) MAXNUM=4
      NGIJKL=(MAXNUM**4)
C
C     ----- FOR DERIVATIVES -----
C
      MODTYP=MAXTYP+NDER
C
C     ----- CALCULATE VECTOR LENGTH AND SET CORE POINTERS -----
C
      LVAR=0
      LFIX=LENGTH
      LFIX=LFIX  +(NSHELL*(NSHELL+1))/2
C
C     ----- -SP- FUNCTIONS FOR FIRST DERIVATIVES ARE SPECIAL -----
C
      IWFN  = LOADFM + 1
      IXCH  = IWFN + LENGTH
      IDAB  = IXCH + (NSHELL*(NSHELL+1))/2
      ILAST = IDAB + NGIJKL
      INEED = ILAST- LOADFM
      IADDR = ILAST
      IF (POPLE.AND.MAXTYP.LT.3.AND.NDER.EQ.1) GO TO 300
C
      LFIX=LFIX+( (NSHELL*(NSHELL+1))/2 )*2
      LFIX=LFIX+NGIJKL*(NDER0+NDER1+NDER2)
      LFIX=LFIX+NGIJKL* 4
      LFIX=LFIX+NGIJKL
      LFIX=LFIX+NIJG*15
      LVAR=     ( MODTYP**2       * MODTYP**2       )*3
      LVAR=LVAR+( MODTYP**2       *(MODTYP+MODTYP-1))*3
      LVAR=LVAR+((MODTYP+MODTYP-1)*(MODTYP+MODTYP-1))*3
      LVAR=LVAR+( MODTYP**2                         )*3
      LVAR=LVAR+((MODTYP+MODTYP-1)                  )*3
      LVAR=LVAR+((MODTYP+MODTYP-1)* 3               )*3
      LVAR=LVAR+(  3                                )*3
      LVAR=LVAR+(  9                                )
      LVAR=LVAR+(  4                                )
      LVAR=LVAR+(  5                                )
      LVAR=LVAR+( 18                                )
      LVAR=LVAR+(  2                                )
      LVAR=LVAR+(  4                                )*3
      LVAR=LVAR+( MODTYP**2       * MODTYP**2       )*3*14
C
      MAXXYZ=(NGOTMX-LFIX-1)/LVAR
      IF(MP2) MAXXYZ = MIN(MAXXYZ,2*MINXYZ)
      MINMEM = (MINXYZ*LVAR)+1+LFIX
      IF(MAXXYZ.LT.MINXYZ) THEN
         IF(MASWRK) WRITE(IW,9998) MINMEM
         CALL ABRT
      END IF
      IF(.NOT.MP2  .AND.  SOME) WRITE(IW,9994) MINMEM
      IF(MAXXYZ.GT.MAXVEC) MAXXYZ=MAXVEC
C
C     X(IWFN  ) = WAVEFUNCTION DATA
C     X(IXCH  ) = EXCHANGE INTEGRAL THRESHOLD
C     X(INIJG ) = CHARGE DISTRIBUTION POINTERS
C     X(IGINT ) = ELECTRON REPULSION INTEGRALS
C     X(IFINT ) = FIRST DERIVATIVE INTEGRALS
C     X(ISINT ) = SECOND DERIVATIVE INTEGRALS
C     X(IIJKLG) = INDICES
C     X(IDAB  ) = DENSITY ARRAY -DAB-
C     X(ICHRG ) = CHARGE DISTRIBUTION PARAMETERS
C     X(IXY   ) = TEMPORARY ARRAY WHEN FORMING DERIVATIVE INTEGRALS
C     X(IXZ   ) = TEMPORARY ARRAY WHEN FORMING DERIVATIVE INTEGRALS
C     X(IYZ   ) = TEMPORARY ARRAY WHEN FORMING DERIVATIVE INTEGRALS
C     X(IX    ) = TEMPORARY ARRAY WHEN FORMING DERIVATIVE INTEGRALS
C     X(IY    ) = TEMPORARY ARRAY WHEN FORMING DERIVATIVE INTEGRALS
C     X(IZ    ) = TEMPORARY ARRAY WHEN FORMING DERIVATIVE INTEGRALS
C     X(ISJ   ) = TEMPORARY ARRAY WHEN -SP- SHELLS
C     X(ISK   ) = TEMPORARY ARRAY WHEN -SP- SHELLS
C     X(ISL   ) = TEMPORARY ARRAY WHEN -SP- SHELLS
C     X(IGIJKL) = ( 2-D , 4 CENTERS ) INTEGRALS
C     X(IGNKL ) = ( 2-D , 3 CENTERS ) INTEGRALS
C     X(IGNM  ) = ( 2-D , 2 CENTERS ) INTEGRALS
C     X(IDIJ  ) = CONTRACTION DENSITY FOR -IJ- CHARGE DISTRIBUTION
C     X(IDKL  ) = CONTRACTION DENSITY FOR -KL- CHARGE DISTRIBUTION
C     X(IB00  ) = -B00-
C     X(IB01  ) = -B01-
C     X(IB10  ) = -B10-
C     X(IC00  ) = -C00-
C     X(ID00  ) = -D00-
C     X(IFXX  ) = -F00-
C     X(IDIJSI) = SCALING FACTOR FOR -S- FUNCTION OF AN -SP- II SHELL
C     X(IDIJSJ) = SCALING FACTOR FOR -S- FUNCTION OF AN -SP- JJ SHELL
C     X(IDKLSK) = SCALING FACTOR FOR -S- FUNCTION OF AN -SP- KK SHELL
C     X(IDKLSL) = SCALING FACTOR FOR -S- FUNCTION OF AN -SP- LL SHELL
C     X(IABV  ) = -AB- VECTOR FOR PRIMITIVE INTEGRALS
C     X(ICV   ) = -CV- VECTOR FOR PRIMITIVE INTEGRALS
C     X(IRW   ) = -RW- VECTOR FOR RYS ROOTS AND WEIGHTS
C     X(IAAI  ) = EXPONENT FOR DERIVATIVE OF II SHELL
C     X(IAAJ  ) = EXPONENT FOR DERIVATIVE OF JJ SHELL
C     X(IBBK  ) = EXPONENT FOR DERIVATIVE OF KK SHELL
C     X(IBBL  ) = EXPONENT FOR DERIVATIVE OF LL SHELL
C     X(IFI   ) = FIRST DERIVATIVE WRT. II OF ( 2-D , 4 CENTERS ) INT.
C     X(IFJ   ) = FIRST DERIVATIVE WRT. JJ OF ( 2-D , 4 CENTERS ) INT.
C     X(IFK   ) = FIRST DERIVATIVE WRT. KK OF ( 2-D , 4 CENTERS ) INT.
C     X(IFL   ) = FIRST DERIVATIVE WRT. LL OF ( 2-D , 4 CENTERS ) INT.
C     X(ISII  ) = SECOND DER. WRT II AND II OF ( 2-D , 4 CENTERS ) INT.
C     X(ISJJ  ) = SECOND DER. WRT JJ AND JJ OF ( 2-D , 4 CENTERS ) INT.
C     X(ISKK  ) = SECOND DER. WRT KK AND KK OF ( 2-D , 4 CENTERS ) INT.
C     X(ISLL  ) = SECOND DER. WRT LL AND LL OF ( 2-D , 4 CENTERS ) INT.
C     X(ISIJ  ) = SECOND DER. WRT II AND JJ OF ( 2-D , 4 CENTERS ) INT.
C     X(ISIK  ) = SECOND DER. WRT II AND KK OF ( 2-D , 4 CENTERS ) INT.
C     X(ISIL  ) = SECOND DER. WRT II AND LL OF ( 2-D , 4 CENTERS ) INT.
C     X(ISJK  ) = SECOND DER. WRT JJ AND KK OF ( 2-D , 4 CENTERS ) INT.
C     X(ISJL  ) = SECOND DER. WRT JJ AND LL OF ( 2-D , 4 CENTERS ) INT.
C     X(ISKL  ) = SECOND DER. WRT KK AND LL OF ( 2-D , 4 CENTERS ) INT.
C
      INIJG =IXCH  + (NSHELL*(NSHELL+1))/2
      IGINT =INIJG +(((NSHELL*(NSHELL+1))/2)*2)/NWDVAR
      IFINT =IGINT +  NGIJKL*NDER0
      ISINT =IFINT +  NGIJKL*NDER1
      IIJKLG=ISINT +  NGIJKL*NDER2
      IDAB  =IIJKLG+  NGIJKL*4/NWDVAR
      ICHRG =IDAB  +  NGIJKL
      IXY   =ICHRG +  NIJG  *15
      IXZ   =IXY   +(  1                                )*MAXXYZ
      IYZ   =IXZ   +(  1                                )*MAXXYZ
      IX    =IYZ   +(  1                                )*MAXXYZ
      IY    =IX    +(  1                                )*MAXXYZ
      IZ    =IY    +(  1                                )*MAXXYZ
      ISJ   =IZ    +(  1                                )*MAXXYZ
      ISK   =ISJ   +(  1                                )*MAXXYZ
      ISL   =ISK   +(  1                                )*MAXXYZ
      IGIJKL=ISL   +(  1                                )*MAXXYZ
      IGNKL =IGIJKL+( MODTYP**2       * MODTYP**2       )*MAXXYZ*3
      IGNM  =IGNKL +( MODTYP**2       *(MODTYP+MODTYP-1))*MAXXYZ*3
      IDIJ  =IGNM  +((MODTYP+MODTYP-1)*(MODTYP+MODTYP-1))*MAXXYZ*3
      IDKL  =IDIJ  +( MODTYP**2                         )*MAXXYZ*3
      IB00  =IDKL  +((MODTYP+MODTYP-1)                  )*MAXXYZ*3
      IB01  =IB00  +((MODTYP+MODTYP-1)                  )*MAXXYZ*3
      IB10  =IB01  +((MODTYP+MODTYP-1)                  )*MAXXYZ*3
      IC00  =IB10  +((MODTYP+MODTYP-1)                  )*MAXXYZ*3
      ID00  =IC00  +(  1                                )*MAXXYZ*3
      IFXX  =ID00  +(  1                                )*MAXXYZ*3
      IDIJSI=IFXX  +(  1                                )*MAXXYZ*3
      IDIJSJ=IDIJSI+(  1                                )*MAXXYZ
      IDKLSK=IDIJSJ+(  1                                )*MAXXYZ
      IDKLSL=IDKLSK+(  1                                )*MAXXYZ
      IABV  =IDKLSL+(  1                                )*MAXXYZ
      ICV   =IABV  +(  5                                )*MAXXYZ
      IRW   =ICV   +( 18                                )*MAXXYZ
      IAAI  =IRW   +(  2                                )*MAXXYZ
      IAAJ  =IAAI  +(  1                                )*MAXXYZ*3
      IBBK  =IAAJ  +(  1                                )*MAXXYZ*3
      IBBL  =IBBK  +(  1                                )*MAXXYZ*3
      IFI   =IBBL  +(  1                                )*MAXXYZ*3
      IFJ   =IFI   +( MODTYP**2       * MODTYP**2       )*MAXXYZ*3
      IFK   =IFJ   +( MODTYP**2       * MODTYP**2       )*MAXXYZ*3
      IFL   =IFK   +( MODTYP**2       * MODTYP**2       )*MAXXYZ*3
      ISII  =IFL   +( MODTYP**2       * MODTYP**2       )*MAXXYZ*3
      ISJJ  =ISII  +( MODTYP**2       * MODTYP**2       )*MAXXYZ*3
      ISKK  =ISJJ  +( MODTYP**2       * MODTYP**2       )*MAXXYZ*3
      ISLL  =ISKK  +( MODTYP**2       * MODTYP**2       )*MAXXYZ*3
      ISIJ  =ISLL  +( MODTYP**2       * MODTYP**2       )*MAXXYZ*3
      ISIK  =ISIJ  +( MODTYP**2       * MODTYP**2       )*MAXXYZ*3
      ISIL  =ISIK  +( MODTYP**2       * MODTYP**2       )*MAXXYZ*3
      ISJK  =ISIL  +( MODTYP**2       * MODTYP**2       )*MAXXYZ*3
      ISJL  =ISJK  +( MODTYP**2       * MODTYP**2       )*MAXXYZ*3
      ISKL  =ISJL  +( MODTYP**2       * MODTYP**2       )*MAXXYZ*3
      ILAST =ISKL  +( MODTYP**2       * MODTYP**2       )*MAXXYZ*3
      INEED =ILAST - LOADFM
      IADDR =ILAST
C
      IF(.NOT.MP2  .AND.  SOME) WRITE(IW,9997) INEED
      IF(OUT) WRITE(IW,9996) IWFN,IXCH,INIJG,IGINT,IFINT,ISINT,IIJKLG,
     1 IDAB,ICHRG,IXY,IXZ,IYZ,IX,IY,IZ,ISJ,ISK,ISL,IGIJKL,IGNKL,IGNM,
     2 IDIJ,IDKL,IB00,IB01,IB10,IC00,ID00,IFXX,
     3 IAAI,IAAJ,IBBK,IBBL,IFI,IFJ,IFK,IFL,
     4 ISII,ISJJ,ISKK,ISLL,ISIJ,ISIK,ISIL,ISJK,ISJL,ISKL,
     5 IDIJSI,IDIJSJ,IDKLSK,IDKLSL,IABV,ICV,IRW
      RETURN
C
  300 CONTINUE
      IF(OUT) WRITE(IW,9995) IWFN,IXCH,INEED,NGOTMX
      RETURN
C
 9999 FORMAT(' GRADIENTS ARE LIMITED TO G AND LOWER FUNCTIONS ')
 9998 FORMAT(/,' NOT ENOUGH MEMORY FOR THE TWO-ELECTRON GRADIENT',/,
     1         ' YOU WILL NEED AT LEAST ',I10,' WORDS.')
 9997 FORMAT(' USING ',I10,' WORDS OF MEMORY.')
 9996 FORMAT(
     1 ' IWFN  ',I8,' IXCH  ',I8,' INIJG ',I8,' IGINT ',I8,' IFINT ',I8,
     1 ' ISINT ',I8,' IIJKLG',I8,/,
     1 ' IDAB  ',I8,' ICHRG ',I8,' IXY   ',I8,' IXZ   ',I8,' IYZ   ',I8,
     1 ' IX    ',I8,' IY    ',I8,' IZ    ',I8,/,
     1 ' ISJ   ',I8,' ISK   ',I8,' ISL   ',I8,
     1 ' IGIJKL',I8,' IGNKL ',I8,' IGNM  ',I8,/,
     2 ' IDIJ  ',I8,' IDKL  ',I8,' IB00  ',I8,' IB01  ',I8,' IB10  ',I8,
     3 ' IFI   ',I8,' IFJ   ',I8,' IFK   ',I8,' IFL   ',I8,/,
     4 ' ISII  ',I8,' ISJJ  ',I8,' ISKK  ',I8,' ISLL  ',I8,
     4 ' ISIJ  ',I8,' ISIK  ',I8,' ISIL  ',I8,' ISJK  ',I8,/
     4 ' ISJL  ',I8,' ISKL  ',I8,/,
     5 ' IDIJSI',I8,' IDIJSJ',I8,' IDKLSK',I8,' IDKLSL',I8,/,
     5 ' IABV  ',I8,' ICV   ',I8,' IRW   ',I8)
 9995 FORMAT(' SPECIAL -SP- ROUTINES USED.',
     1       ' IWFN,IXCH,INEED,MAXFM = ',4I8)
 9994 FORMAT(/,' THE MINIMUM MEMORY REQUIRED FOR THIS RUN IS ',I10,
     1         ' WORDS.')
      END
C*MODULE GRD2A   *DECK OEDHND
      SUBROUTINE OEDHND(NIJG,DCHRG)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXGTOT=5000, MXSH=1000, MXAO=2047)
C
      COMMON /DSHLNO/ LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJD,KLD,IJ,KL
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      DIMENSION NIJG(2,*), DCHRG(15,*)
C
C     ----- ONE-ELECTRON CHARGE DISTRIBUTION AND DERIVATIVES -----
C
      NIJ0=0
      DO 9000 II=1,NSHELL
        DO 8000 JJ=1,II
          ISH=II
          JSH=JJ
          CALL OEDSHL(ISH,JSH,DCHRG,NIJ0)
          IIJJ=IA(MAX0(II,JJ))+MIN0(II,JJ)
          NIJG(1,IIJJ)=NIJ0
          NIJG(2,IIJJ)=NIJ
          NIJ0=NIJ0+NIJ
 8000   CONTINUE
 9000 CONTINUE
C
      RETURN
      END
C*MODULE GRD2A   *DECK OEDRD
      SUBROUTINE OEDRD(NIJG,NIJ,NIJ0,IIJJ)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION NIJG(2,*)
C
      NIJ0=NIJG(1,IIJJ)
      NIJ =NIJG(2,IIJJ)
      RETURN
      END
C*MODULE GRD2A   *DECK OEDSHL
      SUBROUTINE OEDSHL(ISH,JSH,DCHRG,NIJ0)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXGTOT=5000, MXSH=1000, MXGSH=30, MXATM=500)
C
      DIMENSION GA(MXGSH),CCA(MXGSH),CCAS(MXGSH),
     *          GB(MXGSH),CCB(MXGSH),CCBS(MXGSH)
      DIMENSION CSPDFG(MXGTOT,5)
      DIMENSION DCHRG(15,*)
C
      LOGICAL IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
      LOGICAL SPI,SPJ,SPK,SPL,SPIJ,SPKL,SPIJKL
      LOGICAL EXPNDI,EXPNDK,OUT,DBG
C
      COMMON /DSHLNO/ LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJD,KLD,IJ,KL
      COMMON /DSHLT / RTOL,DTOL,VTOL1,VTOL2,VTOLS,OUT,DBG
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON/SHLEQU/IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
      COMMON/SHLNUM/NUMI,NUMJ,NUMK,NUML,IJKL
      COMMON/SHLTYP/SPI,SPJ,SPK,SPL,SPIJ,SPKL,SPIJKL
      COMMON/SHLXPN/EXPNDI,EXPNDK
C
      EQUIVALENCE (CSPDFG(1,1),CS(1))
C
      PARAMETER (ONE=1.0D+00)
C
      IIEQJJ=ISH.EQ.JSH
C
C     ----- ISHELL -----
C
      I=KATOM(ISH)
      XI=C(1,I)
      YI=C(2,I)
      ZI=C(3,I)
      I1=KSTART(ISH)
      I2=I1+KNG(ISH)-1
      LIT=KTYPE(ISH)
      MINI=KMIN(ISH)
      MAXI=KMAX(ISH)
      NUMI=MAXI-MINI+1
      LOCI=KLOC(ISH)-MINI
      SPI=LIT.EQ.2.AND.MINI.EQ.1
      NGA=0
      DO 10 I=I1,I2
        NGA=NGA+1
        GA(NGA)=EX(I)
        CCA(NGA)=CSPDFG(I,LIT)
        IF(SPI) CCAS(NGA)=CSPDFG(I,1)/CSPDFG(I,2)
   10 CONTINUE
C
C     ----- JSHELL -----
C
      J=KATOM(JSH)
      XJ=C(1,J)
      YJ=C(2,J)
      ZJ=C(3,J)
      J1=KSTART(JSH)
      J2=J1+KNG(JSH)-1
      LJT=KTYPE(JSH)
      MINJ=KMIN(JSH)
      MAXJ=KMAX(JSH)
      NUMJ=MAXJ-MINJ+1
      LOCJ=KLOC(JSH)-MINJ
      SPJ=LJT.EQ.2.AND.MINJ.EQ.1
      NGB=0
      DO 20 J=J1,J2
        NGB=NGB+1
        GB(NGB)=EX(J)
        CCB(NGB)=CSPDFG(J,LJT)
        IF(SPJ) CCBS(NGB)=CSPDFG(J,1)/CSPDFG(J,2)
   20 CONTINUE
      RRI=((XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2)
      SPIJ=SPI.OR.SPJ
      EXPNDI=LIT.GE.LJT
C
C     ----- -IJ- CHARGE DISTRIBUTION -----
C
      XC=XI
      YC=YI
      ZC=ZI
      DXIJ=XI-XJ
      DYIJ=YI-YJ
      DZIJ=ZI-ZJ
      IF(EXPNDI) GO TO 100
      XC=XJ
      YC=YJ
      ZC=ZJ
      DXIJ=XJ-XI
      DYIJ=YJ-YI
      DZIJ=ZJ-ZI
  100 CONTINUE
C
C     ----- - I- PRIMITIVE           -----
C
      NIJ=0
      DO 300 IA=1,NGA
        AI=GA(IA)
        ARRI=AI*RRI
        AXI=AI*XI
        AYI=AI*YI
        AZI=AI*ZI
        CCI=CCA(IA)
C
C     ----- - J- PRIMITIVE           -----
C
        DO 200 JB=1,NGB
          AJ=GB(JB)
          AA=AI+AJ
          AA1=ONE/AA
          DUM=AJ*ARRI*AA1
          IF(DUM.GT.RTOL) GO TO 200
          DAEXPA=CCI*CCB(JB)* EXP(-DUM)*AA1
          DUM=  ABS(DAEXPA)
          IF(DUM.LE.DTOL) GO TO 200
C
          NIJ=NIJ+1
          DCHRG( 1,NIJ+NIJ0)= DAEXPA
          DCHRG( 2,NIJ+NIJ0)= AA
          DCHRG( 3,NIJ+NIJ0)=(AXI+AJ*XJ)*AA1
          DCHRG( 4,NIJ+NIJ0)=(AYI+AJ*YJ)*AA1
          DCHRG( 5,NIJ+NIJ0)=(AZI+AJ*ZJ)*AA1
          DCHRG( 6,NIJ+NIJ0)= XC
          DCHRG( 7,NIJ+NIJ0)= YC
          DCHRG( 8,NIJ+NIJ0)= ZC
          DCHRG( 9,NIJ+NIJ0)= DXIJ
          DCHRG(10,NIJ+NIJ0)= DYIJ
          DCHRG(11,NIJ+NIJ0)= DZIJ
          DCHRG(12,NIJ+NIJ0)= AI+AI
          DCHRG(13,NIJ+NIJ0)= AJ+AJ
          IF(SPI) DCHRG(14,NIJ+NIJ0)=CCAS(IA)
          IF(SPJ) DCHRG(15,NIJ+NIJ0)=CCBS(JB)
C
  200   CONTINUE
  300 CONTINUE
      RETURN
      END
C*MODULE GRD2A   *DECK DABCIS
      SUBROUTINE DABCIS(II,JJ,KK,LL,PHF,PCIS,TCIS,DAB,DABMAX,
     *                  L1,L2,Q4,POPLE)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION PHF(L2),PCIS(L2),TCIS(L1,*),DAB(*)
      LOGICAL POPLE,CISSNG
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXAO=2047)
C
      COMMON /CISMLT/ CISSNG
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /INDD80/ LA,LB,LC,LD
      LOGICAL SOME,OUT,DBUG
      COMMON /MP2PRT/ SOME,OUT,DBUG
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CCG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      LOGICAL IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
      COMMON /SHLEQU/ IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
      COMMON /SHLLMN/ IGXYZ(4,35),JGXYZ(4,35),KGXYZ(4,35),LGXYZ(4,35)
      COMMON /SHLNRM/ PNRM(35)
C
CJMS  LABELLED COMMON GSPG80 DEFINED FOR COMPUTATIONAL EFFICIENCY.
CJMS  FOR SP BASES ONLY, IT CONTAINS THE E ARRAY WHICH IS THE DAB
CJMS  ARRAY WITH INDICES IN REVERSE ORDER: E(I,J,K,L)= DAB(L,K,J,I)
CJMS  AND IS USED IN SUB JKDG80 (MODULE GRD2B). IT ORIGINATES IN:
CJMS
CJMS     1. SUBS DABCLU, DABDFT, DABGVB, DABMC, DABMP2 AND DABUMP
CJMS        (MODULE GRD2A) AND SUB DABPAU (MODULE EFPAUL) WHICH ARE
CJMS        ALL CALLED BY SUB JKDER (MODULE GRD2A)
CJMS
CJMS     2. SUB DABCLU (MODULE GRD2A) WHICH IS CALLED BY SUB EFDEN OF
CJMS        MODULE EFGRD2
CJMS
CJMS     3. SUB PAR2PDM (MODULE MP2DDI) WHICH IS CALLED BY SUB PJKDMP2
CJMS        OF MODULE MP2DDI
C
      COMMON /GSPG80/ E(4,4,4,4)
C
      PARAMETER (ZER=0.0D+00, PT5=0.5D+00, P25=.25D+00, F04=4.0D+00)
C
C     ----- FORM TWO-PARTICLE DENSITY MATRIX FOR CIS GRADIENT -----
C
      DT1 = ZER
      DABMAX= ZER
      MINI= KMIN(II)
      MINJ= KMIN(JJ)
      MINK= KMIN(KK)
      MINL= KMIN(LL)
      LOCI= KLOC(II)-MINI
      LOCJ= KLOC(JJ)-MINJ
      LOCK= KLOC(KK)-MINK
      LOCL= KLOC(LL)-MINL
C
      IF(POPLE) THEN
         DO 110 L=1,LD
            NNU= LOCL+L
            DO 110 K=1,LC
               NMU= LOCK+K
               MUNU=IA(MAX0(NMU,NNU))+MIN0(NMU,NNU)
               DO 110 J=1,LB
                  NSI= LOCJ+J
                  DO 110 I=1,LA
                     NLA= LOCI+I
                     LASI=IA(MAX0(NLA,NSI))+MIN0(NLA,NSI)
C
C                    WRITE(IW,*) 'NLA,NSI,NMU,NNU,LASI,MUNU=',
C    &                            NLA,NSI,NMU,NNU,LASI,MUNU
C
                     MUSI = IA(MAX0(NMU,NSI)) + MIN0(NMU,NSI)
                     LANU = IA(MAX0(NLA,NNU)) + MIN0(NLA,NNU)
                     MULA = IA(MAX0(NMU,NLA)) + MIN0(NMU,NLA)
                     NUSI = IA(MAX0(NNU,NSI)) + MIN0(NNU,NSI)
C
                     DF1 =  (PHF(MUNU)+PCIS(MUNU))*PHF(LASI)
     &                    +  PHF(MUNU)*PCIS(LASI)
C
                     DQ1 =  (PHF(MUSI)+PCIS(MUSI))*PHF(LANU)
     &                    +  PHF(MUSI)*PCIS(LANU)
     &                    + (PHF(MULA)+PCIS(MULA))*PHF(NUSI)
     &                    +  PHF(MULA)*PCIS(NUSI)
C
                     IF(CISSNG) THEN
                        DT1 =  TCIS(NMU,NNU)*TCIS(NLA,NSI)
     &                       + TCIS(NNU,NMU)*TCIS(NLA,NSI)
     &                       + TCIS(NNU,NMU)*TCIS(NSI,NLA)
     &                       + TCIS(NMU,NNU)*TCIS(NSI,NLA)
                     END IF
C
                     DT2 =  TCIS(NMU,NLA)*TCIS(NNU,NSI)
     &                    + TCIS(NLA,NMU)*TCIS(NSI,NNU)
     &                    + TCIS(NMU,NSI)*TCIS(NNU,NLA)
     &                    + TCIS(NSI,NMU)*TCIS(NLA,NNU)
C
                     DF1 = DF1 - P25*DQ1 + DT1 - PT5*DT2
C
                     DF1 = DF1*Q4
                     IF(DABMAX.LT. ABS(DF1)) DABMAX = ABS(DF1)
                     E(I,J,K,L)= DF1
                     IF(OUT) WRITE(IW,9010) II,JJ,KK,LL,I,J,K,L,DF1
  110    CONTINUE
      ELSE
C
C D AND HIGHER FUNCTIONS OR HONDO ONLY RUNS
C
         MAXI= KMAX(II)
         MAXJ= KMAX(JJ)
         MAXK= KMAX(KK)
         MAXL= KMAX(LL)
         DO 210 I=MINI,MAXI
            P1I = PNRM(I)
            NLA = LOCI+I
            JMAX= MAXJ
            IF(IIEQJJ) JMAX= I
            DO 210 J=MINJ,JMAX
               P2J = P1I*PNRM(J)
               NSI = LOCJ+J
               LASI= IA(MAX0(NLA,NSI))+MIN0(NLA,NSI)
               KKMAX=MAXK
               IF(IJEQKL) KKMAX= I
               DO 210 K=MINK,KKMAX
                  P3K = P2J*PNRM(K)
                  NMU = LOCK+K
                  LMAX= MAXL
                  IF(KKEQLL) LMAX= K
                  IF(IJEQKL .AND. K.EQ.I) LMAX= J
                  DO 210 L=MINL,LMAX
                     P4L = P3K*PNRM(L)
                     NNU = LOCL+L
                     MUNU= IA(MAX0(NMU,NNU))+MIN0(NMU,NNU)
C
C                     WRITE(IW,*) 'NLA,NSI,NMU,NNU,LASI,MUNU=',
C     &                            NLA,NSI,NMU,NNU,LASI,MUNU
C
                     MUSI= IA(MAX0(NMU,NSI))+MIN0(NMU,NSI)
                     LANU= IA(MAX0(NLA,NNU))+MIN0(NLA,NNU)
                     MULA= IA(MAX0(NMU,NLA))+MIN0(NMU,NLA)
                     NUSI= IA(MAX0(NNU,NSI))+MIN0(NNU,NSI)
C
C                    WRITE(IW,*) 'MUSI,LANU,MULA,NUSI=',
C     &                              MUSI,LANU,MULA,NUSI
C
                     DF1 =  (PHF(MUNU)+PCIS(MUNU))*PHF(LASI)
     &                    +  PHF(MUNU)*PCIS(LASI)
C
                     DQ1 =  (PHF(MUSI)+PCIS(MUSI))*PHF(LANU)
     &                    +  PHF(MUSI)*PCIS(LANU)
     &                    + (PHF(MULA)+PCIS(MULA))*PHF(NUSI)
     &                    +  PHF(MULA)*PCIS(NUSI)
C
                     IF(CISSNG) THEN
                        DT1 =  TCIS(NMU,NNU)*TCIS(NLA,NSI)
     &                       + TCIS(NNU,NMU)*TCIS(NLA,NSI)
     &                       + TCIS(NNU,NMU)*TCIS(NSI,NLA)
     &                       + TCIS(NMU,NNU)*TCIS(NSI,NLA)
                     END IF
C
                     DT2 =  TCIS(NMU,NLA)*TCIS(NNU,NSI)
     &                    + TCIS(NLA,NMU)*TCIS(NSI,NNU)
     &                    + TCIS(NMU,NSI)*TCIS(NNU,NLA)
     &                    + TCIS(NSI,NMU)*TCIS(NLA,NNU)
C
                     DF1 = DF1 - P25*DQ1 + DT1 - PT5*DT2
C
                     DF1= DF1*F04
                     IF(NMU .EQ.NNU ) DF1= DF1*PT5
                     IF(NLA .EQ.NSI ) DF1= DF1*PT5
                     IF(MUNU.EQ.LASI) DF1= DF1*PT5
C
C                     WRITE(IW,*) '** DFAC=',DF1
C
                     IF(DABMAX.LT. ABS(DF1)) DABMAX= ABS(DF1)
                     IJKL=IGXYZ(1,I)+JGXYZ(1,J)+KGXYZ(1,K)+LGXYZ(1,L)
                     DAB(IJKL)= DF1*P4L
                     IF(OUT) WRITE(IW,9020) II,JJ,KK,LL,I,J,K,L,IJKL,DF1
 210     CONTINUE
      ENDIF
      RETURN
 9010 FORMAT(' -DABCIS,POPLE- ',4I4,4I3,D20.12)
 9020 FORMAT(' -DABCIS,HONDO- ',4I4,4I3,I5,D20.12)
      END
