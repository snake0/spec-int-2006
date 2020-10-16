C  9 dec 03 - mws - synch common block runopt
C  4 Nov 03 - DGF - change default for MCP2E
C  3 Sep 03 - MWS - include changes dated 10-Nov-01 to 6-Jun-02
C  6 Jun 02 - DGF - implement form factor method for SO-MCQDPT
C 14 Feb 02 - DGF - add runtyp=transitn restart
C  9 Jan 02 - DGF - allow several SO ISF parameters in one batch
C  8 Jan 02 - DGF - implement IREST=1,2 for transition moment runs
C 23 Nov 01 - TN  - changes for DK
C 10 Nov 01 - DGF - implement MCP 2e core-active SOC integrals
C 25 Oct 01 - HW,DGF - pass spin-orbit MCQDPT ISF parameters
C 19 Sep 01 - MWS - convert mxaoci paramter to mxao
C  6 Sep 01 - HU  - TRNMOMX: SAVE DSKWRK FOR MPIVOX, mpivox:synch mq2par
C 25 Jun 01 - MWS - alter common block wfnopt
C 13 Jun 01 - DGF - changes for SO-MRMP
C 11 May 01 - MWS - TRNMOMX: correct default for NUMCI input
C 20 Feb 01 - MWS - TRNCMO,TRNMOMX: change cmo checks, set tole smaller
C 26 Oct 00 - MWS - introduce mxao parameter, variables in bcast calls
C 11 Jun 00 - DGF,SK - DIPMOM: changed a format, TRNSTX: fix mult. bug
C  1 May 00 - MWS - no symmetry option in trfopt common
C 25 Mar 00 - DGF - code 1e NESC SOC
C 10 Jan 00 - MWS - synchronize relwfn common
C 22 Dec 99 - DGF - changes from 13/may/98 to 1/nov/99 brought online
C  1 Nov 99 - DGF - TRNMOMX: input checks for RESC SOC calculations
C 14 May 99 - DGF - remove orbital reordering, add lame csf option
C 19 Apr 99 - DGF - orbital ordering for TRNCMO'ed $VECs (GUGA irrep)
C 20 Dec 98 - DGF - SAVCIV: "contract CI" - don't save small coef CSFs
C 27 SEP 98 - MWS - TRNSTX: KILL FOCI/SOCI WITH TWO VECTOR SETS
C  1 Aug 98 - DGF - TRNCMO: comment zeroing out matrix elements
C  7 Jul 98 - DGF - add noirr to gugwfn and detwfn
C 14 May 98 - DGF - remarry the transition moment codes
C 13 May 98 - DGF - add selection rules for transition moment
C  6 APR 98 - MWS - TRNSTX: RESTORE FIX OF 24 MAY 95
C  6 Jan 98 - DGF - adapt SK's changes to the transition moment code
C 28 SEP 97 - MWS - CONVERT PARALLEL CALLS FROM TCGMSG TO DDI
C 16 JUL 97 - HTY - ADD TRNAO TO COMPUTE TDM IN AO BASIS
C  2 APR 97 - DGF - ADD INTERFACE TO FURLANI'S SOCC CODE
C  3 FEB 97 - MWS - SAVCIV: PRINT ENERGIES FOR SPIN ORBIT CORRECTLY
C 11 Sep 96 - SK  - Fix some bugs in TRNMOM.
C 11 Sep 96 - SK  - Delete some lines of "CALL TIMIT()".
C 31 Jan 96 - SK  - Expand spin-multiplicity for SPNORB.
C 24 MAY 95 - MWS - TRNSTX: ALLOW FOR USE OF DISTRIBUTED AO INTS
C  7 Mar 95 - SK  - Remove SPNORB part from TRNSTX
C 22 JAN 95 - SK  - MODIFY FOR SPNORB IMPROVEMENTS
C 29 DEC 94 - SK  - MODIFY FOR SPNORB IMPROVEMENTS
C 13 DEC 94 - MWS - TRNSTX: TEST FOR SPD BASIS ONLY
C 28 NOV 94 - MWS - TRNCMO: CORRECT CASE OF ZERO CORES, TWO VEC GROUPS
C 12 NOV 94 - MWS - REMOVE FTNCHEK WARNINGS
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C  1 JUN 94 - MWS - TRNSTX: USE TRFMCX TRANSFORMATION, RUN IN PARALLEL
C  9 DEC 93 - MWS - TRNSTX,TRNMOM: CHANGE DIPOLE DAF RECORD NUMBERS
C 16 JUL 93 - MWS - INCREASE MAXIMUM CI ROOTS TO 100
C  2 APR 92 - MWS,TLW - COMMON ENRGYS MADE PURE FLOATING POINT
C 17 MAR 92 - MWS - TRNRDM: USE FNDGRP TO POSITION INPUT
C 12 MAR 92 - MWS - REDIMENSION TO 500 ATOMS
C  3 MAR 92 - MWS - TRNORB: USE QMTSYM TO MAKE -Q- MATRIX
C 11 JAN 92 - TLW - SAVCIM,TRNMOM,TRNRDM: MAKE READS PARALLEL
C 10 JAN 92 - TLW - CHANGE CLOSE TO CALL SEQCLO;REWIND TO CALL SEQREW
C 10 JAN 92 - MWS,TLW - CHANGE OPENCF TO SEQOPN
C  8 JAN 92 - TLW - MAKE WRITES PARALLEL; ADD COMMON PAR
C  2 JUL 91 - MWS - TRNORB: CHANGE USAGE OF ORTHO ROUTINE
C 15 NOV 90 - SK  - TRNSTX: SPINORBT RUNS ACTUALLY CAN USE DIFF MO-S
C 15 OCT 90 - MWS - TRNSTX: EVALUATE NUCLEAR REPULSION
C  7 AUG 90 - TLW - ADD CF AND CG TO COMMON NSHEL
C  3 AUG 90 - MWS - TRNCMO: ENSURE IDENTICAL PHASE IN CORE SPACE
C  2 FEB 90 - MWS - TRNORB: CALL STFASE, TRNCMO: ALSO CHECK DIAGONAL
C                   OF OVERLAP MATRIX, SMALL FROM 1E-8 TO 1E-6
C 24 JAN 90 - MWS - TRNORB: USE CLENMO WITH INPUT TOLERANCES
C  7 JAN 90 - MWS - INCORPORATE SHIRO'S 1988 CHANGES FOR S-O,
C                   READ DRT INPUT FROM $DRT1 AND/OR $DRT2
C 26 SEP 89 - MWS - ADD NFT13,NFT14 TO /CIFILS/
C 16 MAR 89 - MWS - SWAP WRITE AND TIMIT IN TRNSTX
C 15 DEC 88 - MWS - CHANGE CALLS TO TRNSF
C 12 OCT 88 - MWS - CHANGES RELATING TO NEW MCSCF CODE: AO LIMIT FROM
C                   128 TO 256, NCORBS ADDED TO /ORBSET/, REQUEST
C                   REVERSE CANONICAL ORDER TRANFORMATION.
C 10 AUG 88 - MWS - MXSH,MXGSH,MXGTOT FROM 120,10,440 TO 1000,30,5000
C  9 JUL 88 - MWS - PASS IROOTS TO DIAGONALIZATION (IN NUMCI)
C 30 MAY 88 - MWS - USE PARAMETERS TO DIMENSION COMMON
C  8 MAY 88 - MWS - UP AO-S FROM 255 TO 2047
C  2 APR 88 - MWS - REMOVE ARGUMENTS FROM STANDV CALL
C  2 APR 88 - MWS - MOVE MO READING AFTER 1E- INTEGRALS
C  1 MAR 88 - STE - CLARIFY VARIABLE USAGE
C 25 FEB 88 - MWS - INCORPORATE INTO PRODUCTION VERSION OF GAMESS
C 23 JUN 86 - SK  - FIX BUG IN DIPVEL
C 26 MAR 86 - SK  - IMPLEMENT NEW MODULE FOR TRANSITION MOMENTS
C
C*MODULE TRNSTN  *DECK DIPVEL
      SUBROUTINE DIPVEL(EXETYP,DDX,DDY,DDZ,L1,L2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL IANDJ,DBG,NORM,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500, MXSH=1000, MXGTOT=5000)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
      COMMON /SYMIND/ TOL,II,JJ,LIT,LJT,MINI,MINJ,MAXI,MAXJ,IANDJ
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, TWO=2.0D+00)
      PARAMETER (SQRT3=1.73205080756888D+00, RLN10=2.30258D+00,
     *           SQRT5=2.23606797749979D+00, SQRT7=2.64575131106459D+00)
C
      DIMENSION DDX(L2),DDY(L2),DDZ(L2)
C
C     This routine is set up by using INT1/STANDV routine:
C     Dipople velocity is given as linear combination of overlap
C     integrals...
C
C     LPSUM = SPD   (1+3+6=10)
C           = SPDFG (1+3+6+10+15=35)
C     LPDIM = SPD    max=6    6*6 = 36
C           = SPDFG  max=15  15*15=225
C     LPMAX = SPD    3 types of functions:  3*3 = 9
C           = SPDFG  5 types of functions:  5*5 = 25
C     LINT  = SPD    Prepared for 5 types of integrals.
C                    <I/J>, <I/J+1>, <I+1/J>, <I/J-1>, and <I-1/J>.
C                     1- 9,   10-18,   19-27,   28-36,       37-45.
C           = SPDFG  <I/J>, <I/J+1>, <I+1/J>, <I/J-1>, and <I-1/J>.
C                     1-25,   26-50,   51-75,  76-100,     101-125.
C
      PARAMETER (LPSUM=35, LPDIM=15, LPDIM2=LPDIM*LPDIM, NSPDF=5,
     *           NSPDF2=NSPDF*NSPDF)
      DIMENSION IX(LPSUM),IY(LPSUM),IZ(LPSUM),
     *          JX(LPSUM),JY(LPSUM),JZ(LPSUM),
     *          DAX(LPDIM2),DAY(LPDIM2),DAZ(LPDIM2),DIJ(LPDIM2),
     *          IJX(LPDIM2),IJY(LPDIM2),IJZ(LPDIM2),
     *          XIN(NSPDF2*5),YIN(NSPDF2*5),ZIN(NSPDF2*5)
C
C     Old indices:
C     DATA IX / 1, 4, 1, 1, 7, 1, 1, 4, 4, 1/
C     DATA IY / 1, 1, 4, 1, 1, 7, 1, 4, 1, 4/
C     DATA IZ / 1, 1, 1, 4, 1, 1, 7, 1, 4, 4/
C     DATA JX / 0, 1, 0, 0, 2, 0, 0, 1, 1, 0/
C     DATA JY / 0, 0, 1, 0, 0, 2, 0, 1, 0, 1/
C     DATA JZ / 0, 0, 0, 1, 0, 0, 2, 0, 1, 1/
C               s  x  y  z xx yy zz xy xz yz  etc.
      DATA IX / 1, 6, 1, 1,11, 1, 1, 6, 6, 1,
     *         16, 1, 1,11,11, 6, 1, 6, 1, 6,
     *         21, 1, 1,16,16, 6, 1, 6, 1,11,
     *         11, 1,11, 6, 6/
      DATA IY / 1, 1, 6, 1, 1,11, 1, 6, 1, 6,
     *          1,16, 1, 6, 1,11,11, 1, 6, 6,
     *          1,21, 1, 6, 1,16,16, 1, 6,11,
     *          1,11, 6,11, 6/
      DATA IZ / 1, 1, 1, 6, 1, 1,11, 1, 6, 6,
     *          1, 1,16, 1, 6, 1, 6,11,11, 6,
     *          1, 1,21, 1, 6, 1, 6,16,16, 1,
     *         11,11, 6, 6,11/
C
      DATA JX / 0, 1, 0, 0, 2, 0, 0, 1, 1, 0,
     *          3, 0, 0, 2, 2, 1, 0, 1, 0, 1,
     *          4, 0, 0, 3, 3, 1, 0, 1, 0, 2,
     *          2, 0, 2, 1, 1/
      DATA JY / 0, 0, 1, 0, 0, 2, 0, 1, 0, 1,
     *          0, 3, 0, 1, 0, 2, 2, 0, 1, 1,
     *          0, 4, 0, 1, 0, 3, 3, 0, 1, 2,
     *          0, 2, 1, 2, 1/
      DATA JZ / 0, 0, 0, 1, 0, 0, 2, 0, 1, 1,
     *          0, 0, 3, 0, 1, 0, 1, 2, 2, 1,
     *          0, 0, 4, 0, 1, 0, 1, 3, 3, 0,
     *          2, 2, 1, 1, 2/
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DEBUG/8HDEBUG   /, DBUGME/8HDIPVEL  /
#else
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA DEBUG_STR/"DEBUG   "/, DBUGME_STR/"DIPVEL  "/
#endif
C
C     ----- CALCULATE DIPOLE VELOCITY INTEGRALS. -----
C           revised OVERLAP routine in INT1.
C
      DBG = EXETYP.EQ.DEBUG   .OR.  EXETYP.EQ.DBUGME
     *      .AND. MASWRK
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
      CALL VCLR(DDX,1,L2)
      CALL VCLR(DDY,1,L2)
      CALL VCLR(DDZ,1,L2)
      DUM1 = ZERO
      DUM2 = ZERO
C
C     ----- I SHELL
C
      DO 720 II = 1,NSHELL
      I = KATOM(II)
      XI = C(1,I)
      YI = C(2,I)
      ZI = C(3,I)
      I1 = KSTART(II)
      I2 = I1+KNG(II)-1
      LIT = KTYPE(II)
      MINI = KMIN(II)
      MAXI = KMAX(II)
      LOCI = KLOC(II)-MINI
C
C     ----- J SHELL
C
      DO 700 JJ = 1,II
      J = KATOM(JJ)
      XJ = C(1,J)
      YJ = C(2,J)
      ZJ = C(3,J)
      J1 = KSTART(JJ)
      J2 = J1+KNG(JJ)-1
      LJT = KTYPE(JJ)
      MINJ = KMIN(JJ)
      MAXJ = KMAX(JJ)
      LOCJ = KLOC(JJ)-MINJ
      NROOTS = (LIT+LJT-2)/2+1
      RR = (XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
      IANDJ = II .EQ. JJ
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      IJ = 0
      MAX = MAXJ
      DO 170 I = MINI,MAXI
         NX = IX(I)
         NY = IY(I)
         NZ = IZ(I)
         IF (IANDJ) MAX = I
         DO 160 J = MINJ,MAX
            IJ = IJ+1
            IJX(IJ) = NX+JX(J)
            IJY(IJ) = NY+JY(J)
            IJZ(IJ) = NZ+JZ(J)
  160    CONTINUE
  170 CONTINUE
      DO 180 I = 1,IJ
         DAX(I) = ZERO
         DAY(I) = ZERO
         DAZ(I) = ZERO
  180 CONTINUE
C
C     ----- I PRIMITIVE
C
      DO 660 IG = I1,I2
      AI = EX(IG)
      ARRI = AI*RR
      AXI = AI*XI
      AYI = AI*YI
      AZI = AI*ZI
      CSI = CS(IG)
      CPI = CP(IG)
      CDI = CD(IG)
      CFI = CF(IG)
      CGI = CG(IG)
C
C     ----- J PRIMITIVE
C
      DO 640 JG = J1,J2
      AJ = EX(JG)
      AA = AI+AJ
      AA1 = ONE/AA
      DUM = AJ*ARRI*AA1
      IF (DUM .GT. TOL) GO TO 640
      FAC = EXP(-DUM)
      CSJ = CS(JG)
      CPJ = CP(JG)
      CDJ = CD(JG)
      CFJ = CF(JG)
      CGJ = CG(JG)
      AX = (AXI+AJ*XJ)*AA1
      AY = (AYI+AJ*YJ)*AA1
      AZ = (AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR
C
      MAX = MAXJ
      NN = 0
      DUM1 = ZERO
      DUM2 = ZERO
      DO 420 I = MINI,MAXI
C               S   PX  PY  PZ  DXX DYY DZZ DXY DXZ DYX
C        GO TO (200,220,280,280,240,280,280,260,280,280),I
         IF (I.EQ.1) DUM1=CSI*FAC
         IF (I.EQ.2) DUM1=CPI*FAC
         IF (I.EQ.5) DUM1=CDI*FAC
         IF ((I.EQ. 8).AND.NORM) DUM1=DUM1*SQRT3
         IF (I.EQ.11) DUM1=CFI*FAC
         IF ((I.EQ.14).AND.NORM) DUM1=DUM1*SQRT5
         IF ((I.EQ.20).AND.NORM) DUM1=DUM1*SQRT3
         IF (I.EQ.21) DUM1=CGI*FAC
         IF ((I.EQ.24).AND.NORM) DUM1=DUM1*SQRT7
         IF ((I.EQ.30).AND.NORM) DUM1=DUM1*SQRT5/SQRT3
         IF ((I.EQ.33).AND.NORM) DUM1=DUM1*SQRT3
C
         IF (IANDJ) MAX = I
         DO 420 J = MINJ,MAX
C                  S   PX  PY  PZ  DXX DYY DZZ DXY DXZ DYX
C           GO TO (300,340,400,400,360,400,400,380,400,400),J
            IF (J.EQ.1) THEN
               DUM2=DUM1*CSJ
               ELSE IF (J.EQ.2) THEN
                DUM2=DUM1*CPJ
                ELSE IF (J.EQ.5) THEN
                 DUM2=DUM1*CDJ
                 ELSE IF ((J.EQ.8).AND.NORM) THEN
                  DUM2=DUM2*SQRT3
                  ELSE IF (J.EQ.11) THEN
                   DUM2=DUM1*CFJ
                   ELSE IF ((J.EQ.14).AND.NORM) THEN
                    DUM2=DUM2*SQRT5
                    ELSE IF ((J.EQ.20).AND.NORM) THEN
                     DUM2=DUM2*SQRT3
                     ELSE IF (J.EQ.21) THEN
                      DUM2=DUM1*CGJ
                      ELSE IF ((J.EQ.24).AND.NORM) THEN
                       DUM2=DUM2*SQRT7
                       ELSE IF ((J.EQ.30).AND.NORM) THEN
                        DUM2=DUM2*SQRT5/SQRT3
                        ELSE IF ((J.EQ.33).AND.NORM) THEN
                         DUM2=DUM2*SQRT3
               END IF
            NN = NN+1
            DIJ(NN) = DUM2
  420       CONTINUE
C
C     ----- DIPOLE VELOCITY
C
      T = SQRT(AA1)
      TJP = -TWO*AJ*T
      TIP = -TWO*AI*T
      X0 = AX
      Y0 = AY
      Z0 = AZ
      IN = -NSPDF
C
      DO 480 I = 1,LIT
         IN = IN+NSPDF
         NI = I
         DO 470 J = 1,LJT
            JN = IN+J
C
C           <I/J>.
C
            NJ = J
            CALL STVINT
            XIN(JN) = XINT*T
            YIN(JN) = YINT*T
            ZIN(JN) = ZINT*T
C
C           <I/J+1>.
C
            NJ = J + 1
            CALL STVINT
            JNTMP = JN+NSPDF2
            XIN(JNTMP) = XINT*TJP
            YIN(JNTMP) = YINT*TJP
            ZIN(JNTMP) = ZINT*TJP
C
C           <I/J-1>.
C
            NJ = J - 1
            JNTMP = JN+NSPDF2*2
            IF (NJ .GT. 0) THEN
               CALL STVINT
               TDUM = (J-1)*T
               XIN(JNTMP) = XINT*TDUM
               YIN(JNTMP) = YINT*TDUM
               ZIN(JNTMP) = ZINT*TDUM
            ELSE
               XIN(JNTMP) = ZERO
               YIN(JNTMP) = ZERO
               ZIN(JNTMP) = ZERO
            END IF
C
C           <I+1/J>.
C
            NJ = J
            NI = I + 1
            CALL STVINT
            JNTMP = JN+NSPDF2*3
            XIN(JNTMP) = XINT*TIP
            YIN(JNTMP) = YINT*TIP
            ZIN(JNTMP) = ZINT*TIP
C
C           <I-1/J>.
C
            NI = I - 1
            JNTMP = JN+NSPDF2*4
            IF (NI .GT. 0) THEN
               CALL STVINT
               TDUM = (I-1)*T
               XIN(JNTMP) = XINT*TDUM
               YIN(JNTMP) = YINT*TDUM
               ZIN(JNTMP) = ZINT*TDUM
            ELSE
               XIN(JNTMP) = ZERO
               YIN(JNTMP) = ZERO
               ZIN(JNTMP) = ZERO
            END IF
            NI = I
  470    CONTINUE
  480 CONTINUE
C
      IJD = 0
      MAX = MAXJ
      DO 620 I = MINI,MAXI
         IF (IANDJ) MAX = I
         DO 600 J = MINJ,MAX
            IJD = IJD+1
            NX = IJX(IJD)
            NY = IJY(IJD)
            NZ = IJZ(IJD)
            NTMP2= NSPDF2*2
C                      D/DX
            DUMJMX = XIN(NX+NSPDF2) + XIN(NX+NTMP2)
            DAX(IJD) = DAX(IJD) + DIJ(IJD)*DUMJMX*YIN(NY)*ZIN(NZ)
C                      D/DY
            DUMJMY = YIN(NY+NSPDF2) + YIN(NY+NTMP2)
            DAY(IJD) = DAY(IJD) + DIJ(IJD)*XIN(NX)*DUMJMY*ZIN(NZ)
C                      D/DZ
            DUMJMZ = ZIN(NZ+NSPDF2) + ZIN(NZ+NTMP2)
            DAZ(IJD) = DAZ(IJD) + DIJ(IJD)*XIN(NX)*YIN(NY)*DUMJMZ
  600    CONTINUE
  620 CONTINUE
C                               END OF PRIMITIVE LOOPS
  640 CONTINUE
  660 CONTINUE
C
C     ----- SET UP D/DX, D/DY, AND D/DZ MATRICES
C
      MAX = MAXJ
      NN = 0
      DO 690 I = MINI,MAXI
         LI = LOCI+I
         IN = (LI*(LI-1))/2
         IF (IANDJ) MAX = I
         DO 680 J = MINJ,MAX
            LJ = LOCJ+J
            JN = LJ+IN
            NN = NN+1
            DDX(JN) = DAX(NN)
            DDY(JN) = DAY(NN)
            DDZ(JN) = DAZ(NN)
  680    CONTINUE
  690 CONTINUE
C                               END OF SHELL LOOPS
  700 CONTINUE
  720 CONTINUE
C
      IF(DBG) THEN
         WRITE(IW,*) 'D/DX DIPOLE VELOCITY INTEGRALS'
         CALL PRTRIL(DDX,L1)
         WRITE(IW,*) 'D/DY DIPOLE VELOCITY INTEGRALS'
         CALL PRTRIL(DDY,L1)
         WRITE(IW,*) 'D/DZ DIPOLE VELOCITY INTEGRALS'
         CALL PRTRIL(DDZ,L1)
      END IF
C
      RETURN
      END
C*MODULE TRNSTN  *DECK SAVCIM
      SUBROUTINE SAVCIM(NFTCSF,NWKSST,IROOTS,DEIG,ENGYST,IVCORB,EORB,
     *                  EREF0,NROWS,CSMALL,NOSYM,MPLEVL)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      DIMENSION TITLE(10),TITLE1(10),NWKSST(*),IROOTS(*),DEIG(*),
     *          ENGYST(MXRTTRN,*),EREF0(MXRTTRN,*),
     *          IVCORB(NUM,*),EORB(*)
C
      PARAMETER (MXAO=2047, MXATM=500)
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /FMCOM / H(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,MA,MB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /LOOPS / DUM(4),THRSEM,IDUM(12),INTSAD,NINTMX,NGRPS
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /ORBSET/ NORBMX,NORBS,NCORBS,NLEVS,NA,NB,NC,NSYM,MSYM,
     *                IDOCC,IVAL,IMCC,ISYM(MXAO),ICODE(MXAO),
     *                NLCS(MXAO),LEVPT(MXAO),LEVNR(MXAO),IOUT(MXAO),
     *                NREFS,IEXCT,NFOCI,INTACT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,
     *                MXRTTRN,NSTAT
C
C     ----- MEMORY ROUTINE FOR SAVCIV -----
C     THIS ROUTINE ALSO READS THE NECESSARY CI COEFFICIENTS
C     FROM FILE 12, AND DRT INFORMATION FROM FILE 11, BEFORE
C     CALLING SAVCIV TO STORE THE NECESSARY INFORMATION.
C
C     NOTE THAT NFT12 IS OVERWRITTEN ON THE SECOND PASS!
C
      IF (MASWRK) WRITE (IW,9000)
C
C     ----- READ HEADER OF THE -DRT- FILE -----
C
      CALL SEQREW(NFT11)
      READ (NFT11) NORBMX,NORBS,NSYM,NROWS,NWKS,LEVFRM,NEMEMX,NREFS,
     *             IEXCT,NFOCI,INTACT,NCORBS
      TITLE1(1)=LEVFRM
      READ (NFT11) TITLE1
      IF(MASWRK) WRITE (IW,9020) TITLE1,NWKS
      NWKSST(ICI) = NWKS
C
C     ----- GET FAST MEMORY -----
C
      NROWS4 = NROWS*4
      NKL = NSYM*NORBS
      NIJ = (NORBS*NORBS+NORBS)/2
C
      CALL VALFM(LOADFM)
      IH1  = 1 + LOADFM
      IH2  = IH1 + (NROWS4-1)/NWDVAR + 1
      IH3  = IH2 + (NROWS -1)/NWDVAR + 1
      IH4  = IH3 + (NROWS -1)/NWDVAR + 1
      IH6  = IH4 + (NROWS -1)/NWDVAR + 1
      IH7  = IH6 + (NROWS -1)/NWDVAR + 1
      IH8  = IH7 + (NROWS -1)/NWDVAR + 1
      IH9  = IH8 + (NROWS -1)/NWDVAR + 1
      IH11 = IH9 + (NWKS  -1)/NWDVAR + 1
      IH12 = IH11+ (NROWS4-1)/NWDVAR + 1
      IH13 = IH12+ (NIJ   -1)/NWDVAR + 1
      IH14 = IH13+ (NIJ   -1)/NWDVAR + 1
      IH15 = IH14+ (NKL   -1)/NWDVAR + 1
      IH16 = IH15+ (NKL   -1)/NWDVAR + 1
      IH17 = IH16+ (200   -1)/NWDVAR + 1
      IH18 = IH17+ (200   -1)/NWDVAR + 1
      IHCI = IH18+ (200   -1)/NWDVAR + 1
      LAST= IHCI + NWKS*IROOTS(ICI)
      NEED = LAST-LOADFM
      CALL GETFM(NEED)
C
C     ----- READ IN -DRT- DATA -----
C         H(IH1) =IARC
C         H(IH2) =NABCA
C         H(IH3) =NABCB
C         H(IH4) =NABCS
C         H(IH6) =NLWKS
C         H(IH7) =NUWKS
C         H(IH8) =PUWK
C         H(IH9) =INDX
C         H(IH11)=IWGHT
C         H(IH12)=IJADD
C         H(IH13)=IJGRP
C         H(IH14)=KADD
C         H(IH15)=LADD
C         H(IH16)=INEXT
C         H(IH17)=JMNNXT
C         H(IH18)=JMXNXT
C         H(IHCI)=THE -CICOEF- VECTOR
C
      NEXT = 0
      CALL DRTTAP(NFT11,NROWS,NROWS4,NKL,NIJ,H(IH2),H(IH3),H(IH4),
     *            H(IH1),H(IH6),H(IH7),H(IH8),H(IH9),H(IH11),H(IH12),
     *            H(IH13),H(IH14),H(IH15),H(IH16),H(IH17),H(IH18),
     *            NWKS,NEXT,NGRPS,NINTMX)
      CALL SEQREW(NFT11)
C
C     ----- READ HEADER RECORD OF CI VECTOR TAPE -----
C
      CALL SEQREW(NFT12)
      READ (NFT12) MSTATE,NWKSX,TITLE,TITLE1
      IF(MASWRK) WRITE (IW,9040) TITLE,TITLE1,MSTATE,NWKSX
      IF(NWKSST(ICI).NE.NWKSX) THEN
         IF (MASWRK) WRITE(IW,9220) NWKSST(ICI),NWKSX
         CALL ABRT
      ENDIF
C
C     ----- READ IN THE CI COEFFICIENTS FOR THE DESIRED STATE -----
C
      IF(IROOTS(ICI).GT.MSTATE) THEN
         IF (MASWRK) WRITE(IW,9200) MSTATE,IROOTS(ICI)
         CALL ABRT
      END IF
      IH = IHCI - NWKS
      DO 100 KSTAT=1,IROOTS(ICI)
         IH = IH + NWKS
         CALL SQREAD(NFT12,H(IH),NWKS)
  100 CONTINUE
      CALL SEQREW(NFT12)
C
C     ----- STORE CSF'S AND CI-COEFFICIENTS INTO FILE NFTCSF -----
C
      CALL SAVCIV(EXETYP,NFTCSF,NROWS,NWKS,H(IH1),H(IHCI),NROWS4,DEIG,
     *            ENGYST,IROOTS,IVCORB,NWKSST(ICI),EORB,EREF0,
     *            CSMALL,NOSYM,MPLEVL)
C
C     ----- RESET FAST MEMORY -----
C
      CALL RETFM(NEED)
C
      IF (MASWRK) WRITE(IW,9060)
      CALL TIMIT(1)
      RETURN
C
 9000 FORMAT(/10X,21("-")/10X,'STORE CSF INFORMATION'/10X,20("-"))
 9020 FORMAT(/1X,'READING THE DRT FILE'/1X,'TITLE=',10A8/
     +     28H NUMBER OF CONFIGURATIONS = ,I10)
 9040 FORMAT(/1X,'READING THE CI VECTOR FILE'/
     *       1X,'RUN TITLE=',10A8/1X,'DRT TITLE=',10A8/
     *       1X,I5,' STATES WERE COMPUTED, NWKS=',I10)
 9060 FORMAT(1X,'...... DONE STORING CSF INFORMATION ......')
 9200 FORMAT(1X,'***** ERROR ***** YOU WANT TO COMPUTE A TRANSITION'/
     *       1X,'INVOLVING STATE',I4,' BUT ASKED FOR ONLY',I4,' ROOTS.')
 9220 FORMAT(1X,'***** ERROR ***** DIFFERENT NUMBERS OF WALKS:',2I5)
      END
C*MODULE TRNSTN  *DECK SAVCIV
      SUBROUTINE SAVCIV(EXETYP,NFTCSF,NROWS,NWKS,IARC,CICOEF,NROWS4,DEIG
     *                 ,ENGYST,IROOTS,IVCORB,NWKSS,EORB,EREF0,
     *                  CSMALL,NOSYM,MPLEVL)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL DBG,GOPARR,DSKWRK,MASWRK
C
      DIMENSION IARC(NROWS4),CICOEF(NWKS,*),DEIG(*),
     *          ENGYST(MXRTTRN,*),EREF0(MXRTTRN,*),
     *          IROOTS(*),IVCORB(NUM,*),EORB(*)
C
      PARAMETER (MXAO=2047, MXATM=500)
C
      DIMENSION ISHIFT(4),ICASE(MXAO),IECONF(MXAO),LEVIR(MXAO)
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /ORBSET/ NORBMX,NORBS,NCORBS,NLEVS,NA,NB,NC,NSYM,MSYM,
     *                IDOCC,IVAL,IMCC,ISYM(MXAO),ICODE(MXAO),
     *                NLCS(MXAO),LEVPT(MXAO),LEVNR(MXAO),IOUT(MXAO),
     *                NREFS,IEXCT,NFOCI,INTACT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,MA,MB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,
     *                MXRTTRN,NSTAT
C
      PARAMETER (ONE=1.0D+00,ZERO=0.0D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DIPMOM/8HDM      /,DEBUG/8HDEBUG   /, DBUGME/8HSAVCIV  /,
     *     ZEFF/8HHSOZEFF /
#else
      CHARACTER*8 :: DIPMOM_STR
      EQUIVALENCE (DIPMOM, DIPMOM_STR)
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      CHARACTER*8 :: ZEFF_STR
      EQUIVALENCE (ZEFF, ZEFF_STR)
      DATA DIPMOM_STR/"DM      "/,DEBUG_STR/"DEBUG   "/,
     *     DBUGME_STR/"SAVCIV  "/,
     *     ZEFF_STR/"HSOZEFF "/
#endif
C
C     ----- STORE CI VECTOR AND SELECTED CSF INFORMATION -----
C     THIS WRITES THE CI COEFFICIENTS FOR THE STATE OF INTEREST,
C     AND THE NECESSARY INFORMATION ABOUT THE CSF-S, TO FILE NFTCSF.
C     NOTE THAT NFTCSF IS 17 ON ici=1, 12 ON ici=2.
C     This is not true when RUNTYP=SPINORBT.
C
      IF (MASWRK) THEN
          WRITE(IW,9000) IROOTS(ICI),NFTCSF
          WRITE(IW,9002) (ENGYST(I,ICI),I=1,IROOTS(ICI))
          ENDIF
C
      DBG = (EXETYP.EQ.DEBUG   .OR.   EXETYP.EQ.DBUGME) .AND. MASWRK
      DO 100 K = 1,4
         ISHIFT(K) = (K-1)*NROWS
  100 CONTINUE
C
C     ----- STORE THE CI VECTOR FOR TRANSITION MOMENTS -----
C
      IF(OPERR.EQ.DIPMOM) THEN
         CALL SEQREW(NFTCSF)
         DO 120 K=1,IROOTS(ICI)
            CALL SQWRIT(NFTCSF,CICOEF(1,K),NWKS)
  120       CONTINUE
         IF(NUMVEC.EQ.1) THEN
            DO 150 I=1,NWKS
               CICOEF(I,1) = ONE
  150          CONTINUE
            CALL SQWRIT(NFTCSF,CICOEF(1,1),NWKS)
         END IF
      END IF
C
      IF(DBG) THEN
         WRITE(IW,9300) NROWS,NORBMX,NLEVS,NA,NB,NC,NORBS,NCORBS
         WRITE(IW,9302) (IOUT(I),I=1,NORBMX)
         WRITE(IW,9303) (LEVNR(I),I=1,NLEVS)
         WRITE(IW,9309)
         DO 180 K=1,4
            IS=ISHIFT(K)+1
            IE=IS+NROWS-1
            WRITE(IW,9310) (IARC(I),I=IS,IE)
  180    CONTINUE
      END IF
C
C     ----- GET ORDERING OF THE ORBITALS -----
C
      DO 200 I=1,NORBS
         IORB = 2**30
         DO 210 J=1,NORBMX
            IF(IOUT(J).EQ.I) IORB=J
  210    CONTINUE
         IVCORB(I,ICI)=IORB
  200 CONTINUE
      IF(DBG) WRITE(IW,9304) (IVCORB(I,ICI),I=1,NORBS)
C
C     IF JUST ONE SET OF ORBITALS, THE CSF OVERLAPS ARE UNITY,
C     AND THESE WERE ALREADY WRITTEN OUT ABOVE, SO WE CAN QUIT.
C
      IF(OPERR.EQ.DIPMOM  .AND.  NUMVEC.EQ.1) RETURN
C
      IF(MPLEVL.NE.0) THEN
         EFC=ZERO
         DO I=1,NFZC
           EFC=EFC+2*EORB(I)
         ENDDO
      ENDIF
C
C     ----- GENERATE THE ELECTRONIC CONFIGURATIONS -----
C
      NSAVE=0
      IWKS = 0
      LEV = 1
      LEVM = 1
      IR0 = 1
  330 CONTINUE
      IF (LEV .EQ. NLEVS) GO TO 400
      LEVIR(LEV) = IR0
      LEVM = LEV
      LEV = LEVM+1
      LEVIR(LEV) = LEVNR(LEV)+1
  340 CONTINUE
      IR0 = LEVIR(LEV)
      NPTX = LEVPT(LEV)
      NPTM = LEVPT(LEVM)
      IRM = LEVIR(LEVM)
  350 CONTINUE
      IR0 = IR0-1
      IF (IR0 .EQ. 0) GO TO 550
      NPT = IR0+NPTX
      DO 360 K = 1,4
         IARPT = NPT+ISHIFT(K)
         JARC = IARC(IARPT)
         IF (JARC .EQ. 0) GO TO 360
         JARC = JARC-NPTM
         ICASE(LEVM) = K
         IF (IRM .EQ. JARC) GO TO 330
  360 CONTINUE
      GO TO 350
C
  400 CONTINUE
      IOPEN=0
      DO 480 ILEV = 1,NORBS
         ICAS = ICASE(ILEV)
         GO TO (410,420,420,430), ICAS
C
  410    IOCC = 0
         GO TO 440
C
  420    IOCC = 1
         IOPEN=IOPEN+1
         GO TO 440
C
  430    IOCC = 2
  440    CONTINUE
         IORB=IVCORB(ILEV,ICI) - NCORBS
         IECONF(IORB) = IOCC
  480 CONTINUE
C
      IWKS = IWKS+1
C
      IF(DBG) THEN
         WRITE(IW,9316) IWKS,CICOEF(IWKS,1),
     *                 (IECONF(IORB),IORB=1,NORBS)
         WRITE(IW,9320) (IVCORB(ILEV,ICI),ICASE(ILEV),ILEV=1,NORBS)
      END IF
C
C     ----- WRITE SPIN-ORBIT COUPLING DRT INFO -----
C
      IF(OPERR.EQ.ZEFF) THEN
         COEFMAX=CICOEF(IWKS,IDAMAX(IROOTS(ICI),CICOEF(IWKS,1),NWKS))
         IF(ABS(COEFMAX).GT.CSMALL.OR.NOSYM.GT.1) THEN
            WRITE(NFTCSF) IOPEN,(ICASE(ILEV),ILEV=1,NORBS),
     *                    (IECONF(IORB),IORB = 1,NORBS),
     *                    (CICOEF(IWKS,K),K=1,IROOTS(ICI))
C           WRITE(6,*)   iwks,'www+CSF',IOPEN,'case',
C    *                    (ICASE(ILEV),ILEV=1,NORBS),
C    *                    'ieconf ',(IECONF(IORB),IORB = 1,NORBS),
C    *                    (CICOEF(IWKS,K),K=1,IROOTS(ICI))
            NSAVE=NSAVE+1
            IF(MPLEVL.NE.0) THEN
              ECONF=EFC+GETECSF(NORBS,EORB(1+NFZC),IECONF)
              DO K=1,IROOTS(ICI)
                W=CICOEF(IWKS,K)
                EREF0(K,ICI)=EREF0(K,ICI)+W*W*ECONF
              ENDDO
            ENDIF
         ENDIF
      END IF
C
C     ----- COMPUTE CSF OVERLAPS FOR TRANSITION MOMENTS -----
C
      IF(OPERR.EQ.DIPMOM) THEN
         OVLP=ONE
         DO 510 IORB=1,NORBS
            IF(IECONF(IORB).EQ.1) OVLP=OVLP*DEIG(IORB+NCORBS)
            IF(IECONF(IORB).EQ.2)
     *            OVLP=OVLP*DEIG(IORB+NCORBS)*DEIG(IORB+NCORBS)
  510    CONTINUE
         CICOEF(IWKS,1)=OVLP
         IF(OVLP.LT.1.0D-10) THEN
            IF (MASWRK) WRITE(IW,*)
     *         'CSF OVERLAP TOO SMALL, IWKS,OVLP=',IWKS,OVLP
            CALL ABRT
         END IF
      END IF
C
C     ----- LOOP BACK FOR MORE CSF-S IF NOT DONE YET -----
C
  550 CONTINUE
      LEV = LEVM
      LEVM = LEV -1
      IF (LEVM .GT. 0) GO TO 340
C
C     ----- NOW DONE WITH ALL CONFIGURATIONS -----
C
      IF(NWKS.NE.IWKS) THEN
         IF (MASWRK) WRITE(IW,*)
     *      'PROBLEMS IN SAVCIV, NWKS.NE.IWKS',NWKS,IWKS
         CALL ABRT
         STOP
      END IF
C
C     ----- STORE CSF OVERLAP INTEGRALS -----
C
      IF(OPERR.EQ.DIPMOM) THEN
         CALL SQWRIT(NFTCSF,CICOEF(1,1),NWKS)
         IF(DBG) WRITE(IW,9322) ICI
         IF(DBG) WRITE(IW,9324) (CICOEF(IWKS,1),IWKS=1,NWKS)
      END IF
      IF(OPERR.EQ.ZEFF) THEN
         IF(MASWRK) WRITE(IW,9400) CSMALL,NSAVE,NWKS
         NWKSS=NSAVE
      ENDIF
      RETURN
C
 9000 FORMAT(/1X,'STORING CSF AND CI COEF. INFORMATION FOR ',
     *        I4,' STATES ON FILE',I4,'.',
     *       /5X,'ENERGIES ARE ...')
 9002 FORMAT(1X,5F15.7)
 9300 FORMAT(/1X,'DEBUG:NROWS,NORBMX,NLEVS=',3I7/
     *        1X,'NA,NB,NC=',3I7,'  NORBS,NCORBS=',2I7)
 9302 FORMAT(1X,'  IOUT=',15I4)
 9303 FORMAT(1X,' LEVNR=',15I4)
 9304 FORMAT(1X,'IVCORB=',15I4)
 9309 FORMAT(1X,'  IARC=')
 9310 FORMAT(1X,15I4)
 9316 FORMAT(1X,'CSF NO.',I5,' C=',F10.6,1X,100I1/1X,28I1)
 9320 FORMAT(1X,'MO=CASE: ',20(I4,'=',I1))
 9322 FORMAT(/1X,'OVERLAPS AMONG CSF''S OF STATE ',I3)
 9324 FORMAT(1X,10F12.6)
 9400 FORMAT(1X,'SAVING CSFS WITH COEFS LARGER THAN ',1P,E8.2/,
     *       1X,'SAVED CSFS: ',I10,/1X,'TOTAL CSFS: ',I10,/)
      END
C*MODULE TRNSTN  *DECK TRNCMO
      SUBROUTINE TRNCMO(EXETYP,IA,EIG,SCR,WRK,S,D,U,V,VST1,VST2,
     *                  L1,L2,L3,N1,PRTCMO,DEIG)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DBG,PRTCMO,GOPARR,DSKWRK,MASWRK
C
      DIMENSION IA(L1),EIG(N1),SCR(N1,8),WRK(L1,L1),S(L2),D(N1,N1),
     *          U(N1,N1),V(N1,N1),VST1(L1,N1),VST2(L1,N1),DEIG(N1)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,
     *                MXRTTRN,NSTAT
C
      PARAMETER (ZERO=0.0D+00,ONE=1.0D+00,SMALL=1.0D-06,THRSH=1.0D-05)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DEBUG/8HDEBUG   /, DBUGME/8HTRNCMO  /
#else
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA DEBUG_STR/"DEBUG   "/, DBUGME_STR/"TRNCMO  "/
#endif
C
C     ----- GENERATE CORRESPONDING ORBITALS -----
C
C     THERE MUST BE THE SAME NUMBER OF OCCUPIED ORBITALS IN
C     THE TWO SETS OF VECTORS, VST1 AND VST2.  THE FIRST
C     NFZC OF THESE TWO SETS ARE IDENTICAL (NFZC MAY BE ZERO).
C     THE TWO ORIGINAL SETS OF ORBITALS ARE ASSUMED TO BE
C     ON THE DAF, IN RECORDS 15 AND 19.  ON EXIT, THESE TWO
C     SETS OF ORBITALS ARE REPLACED BY THE CORRESPONDING ORBITALS.
C     THE CMO OVERLAPS ARE PLACED IN THE ARRAY DEIG ON EXIT.
C
C        REFERENCE:
C     H.F.KING, R.E.STANTON, H.KIM, R.E.WYATT, R.G.PARR
C     J.CHEM.PHYS.  47, 1936-1941(1967)
C
      IF (MASWRK) WRITE(IW,9000)
      IF(NUMVEC.EQ.1) GO TO 500
C
      NZERO = 0
      II = 0
      DO 100 I = 1,L1
         IA(I)=II
         II = II + I
  100 CONTINUE
C
      NAOS=L1
      NACT=NOCC-NFZC
      IF (MASWRK) WRITE(IW,9010) NFZC,NACT
      IF(NACT.EQ.0) THEN
         IF (MASWRK) WRITE(IW,*) 'ERROR, THERE ARE NO ACTIVE ORBITALS!'
         CALL ABRT
         STOP
      END IF
      DBG = ( EXETYP.EQ.DEBUG  .OR.  EXETYP.EQ.DBUGME )  .AND. MASWRK
C
      CALL DAREAD(IDAF,IODA,S,L2,12,0)
      CALL DAREAD(IDAF,IODA,VST1,L3,15,0)
      CALL DAREAD(IDAF,IODA,VST2,L3,19,0)
C
C        ENSURE SAME PHASE FOR CORE MO-S
C
      NPHAZ=0
      DO 150 MO=1,NFZC
         DUM = DDOT(L1,VST1(1,MO),1,VST2(1,MO),1)
         IF(DUM.LT.ZERO) THEN
            CALL DSCAL(L1,-ONE,VST2(1,MO),1)
            NPHAZ=NPHAZ+1
         END IF
  150 CONTINUE
      IF(NPHAZ.GT.0) CALL DAWRIT(IDAF,IODA,VST2,L3,19,0)
C
      IF(DBG) THEN
         WRITE(IW,FMT='(/1X,''STATE 1 VECTORS'')')
         CALL PRSQL(VST1,NOCC,L1,L1)
         WRITE(IW,FMT='(/1X,''STATE 2 VECTORS'')')
         CALL PRSQL(VST2,NOCC,L1,L1)
         WRITE(IW,FMT='(/1X,''AO OVERLAP MATRIX'')')
         CALL PRTRI(S,L1)
      END IF
C
C     ----- COMPUTE D = VST2-DAGGER * S * VST1 -----
C     D IS THE OVERLAP BETWEEN THE TWO SETS OF ORBITALS
C
      DO 230 L = 1,NAOS
         DO 220 I = 1,NOCC
            DUM = ZERO
            DO 210 K = 1,NAOS
               KL= IA(K)+L
               IF(K.LT.L) KL= IA(L)+K
               DUM = DUM+VST2(K,I)*S(KL)
  210       CONTINUE
            WRK(I,L) = DUM
  220    CONTINUE
  230 CONTINUE
C
      DO 290 I = 1,NOCC
         DO 270 J = 1,NOCC
            DUM = ZERO
            DO 260 L = 1,NAOS
              DUM = DUM+WRK(I,L)*VST1(L,J)
  260       CONTINUE
            D(I,J) = DUM
  270    CONTINUE
  290 CONTINUE
C
      IF(DBG) THEN
         WRITE(IW,FMT='(/1X,''MO OVERLAPS'')')
         CALL PRSQ(D,NOCC,NOCC,NOCC)
      END IF
C
C     ----- VERIFY THAT THE FIRST NFZC ORBITALS ARE IDENTICAL -----
C
      IF(NFZC.EQ.0) GO TO 360
      IMAX=0
      JMAX=0
      XMAX=ZERO
      DO 330 I=1,NFZC
         DO 320 J=1,NFZC
            IF(I.EQ.J) THEN
               TEST = ABS(D(I,I) - ONE)
            ELSE
               TEST = ABS(D(I,J))
            END IF
            IF(TEST.GT.XMAX) THEN
               XMAX=TEST
               IMAX=I
               JMAX=J
            END IF
  320    CONTINUE
  330 CONTINUE
C
      IF(XMAX.GT.SMALL) THEN
         IF (MASWRK) WRITE(IW,9020) XMAX,IMAX,JMAX
         CALL PRSQ(D,NOCC,NOCC,NOCC)
         CALL ABRT
         STOP
      END IF
C
C     ----- COMPACT THE D MATRIX TO ITS ACTIVE PORTION -----
C
  360 CONTINUE
      NPFZC = NFZC +1
      DO 380 J=NPFZC,NOCC
         DO 370 I=NPFZC,NOCC
            DUM = D(I,J)
            IF(I.EQ.J) THEN
               TEST = ABS(DUM - ONE)
            ELSE
               TEST = ABS(DUM)
            END IF
            IF(TEST.GT.SMALL) NZERO=NZERO+1
            D(I-NFZC,J-NFZC)=DUM
  370    CONTINUE
  380 CONTINUE
C
      IF(DBG) THEN
         WRITE(IW,FMT='(/1X,''D COMPACTED TO ACTIVE SPACE'')')
         CALL PRSQ(D,NACT,NACT,NOCC)
      END IF
C
C     ----- CALCULATE D-DAGGER*D (IN S) -----
C
      IJ = 0
      DO 450 I = 1,NACT
         DO 440 J = 1,I
            DUM = ZERO
            DO 420 K = 1,NACT
               DUM = DUM+D(K,I)*D(K,J)
  420       CONTINUE
            IJ = IJ+1
            IF(ABS(DUM).LT.SMALL) DUM=ZERO
Cnb         to do or not to do??
            S(IJ)=DUM
  440       CONTINUE
  450 CONTINUE
C
      IF(DBG) THEN
         WRITE(IW,FMT='(/1X,''D+*D MATRIX'')')
         CALL PRTRI(S,NACT)
      END IF
C
C     ----- DID THE USER INPUT IDENTICAL $VEC GROUPS? -----
C
      IF(NZERO.GT.0) GO TO 600
      NUMVEC=1
      IF (MASWRK) WRITE(IW,9030)
C
  500 CONTINUE
      DO 520 I=1,NOCC
         DEIG(I) = ONE
  520 CONTINUE
      IF (MASWRK) THEN
         WRITE(IW,9040)
         WRITE(IW,9050) (DEIG(I),I=1,NOCC)
      END IF
      RETURN
C     ******
C
C     ----- DIAGONALIZE D-DAGGER*D -----
C
  600 CONTINUE
      IERR=0
      CALL GLDIAG(NOCC,NACT,NACT,S,SCR,EIG,V,IERR,IA)
      IF(IERR.NE.0) THEN
         IF(MASWRK) WRITE(IW,*) 'GLDIAG FAILED IN TRNCMO'
         CALL ABRT
      END IF
      IF(DBG) THEN
         WRITE(IW,FMT='(/1X,''V MATRIX'')')
         CALL PRSQ(V,NACT,NACT,NOCC)
      END IF
      DO 610 I = 1,NACT
         IF(EIG(I).LT.ZERO) GO TO 8000
         DEIG(I)=SQRT(EIG(I))
  610 CONTINUE
C
C     ----- COMPUTE U = D * V * EIG**-1/2 -----
C
      DO 730 I=1,NOCC
         DO 720 J=1,NACT
            DUM = ZERO
            EINV = ONE/DEIG(J)
            DO 710 K=1,NACT
               DUM = DUM + D(I,K)*V(K,J)*EINV
  710       CONTINUE
            IF(ABS(DUM).LT.SMALL) DUM=ZERO
Cnb         to do or not to do??
            U(I,J) = DUM
  720    CONTINUE
  730 CONTINUE
C
      IF(DBG) THEN
         WRITE(IW,FMT='(/1X,''U MATRIX'')')
         CALL PRSQ(U,NACT,NACT,NOCC)
      END IF
C
C     ----- CORRESPONDING ORBITALS FOR STATE 1 -----
C     VST1 = VST1 * V
C
      DO 830 I=1,NAOS
         DO 810 J=1,NACT
            DUM = ZERO
            DO 800 K=1,NACT
               DUM = DUM + VST1(I,K+NFZC)*V(K,J)
  800       CONTINUE
            IF(ABS(DUM).LT.SMALL) DUM=ZERO
Cnb         to do or not to do??
            SCR(J,1) = DUM
  810    CONTINUE
         DO 820 J=1,NACT
            VST1(I,J+NFZC) = SCR(J,1)
  820    CONTINUE
  830 CONTINUE
C
      CALL DAWRIT(IDAF,IODA,VST1,L3,15,0)
      IF(DBG  .OR.  PRTCMO) THEN
         WRITE(IW,FMT='(/1X,''STATE 1 CMO-S'')')
         CALL PRSQL(VST1,NOCC,L1,L1)
      END IF
C
C     ----- CORRESPONDING ORBITALS FOR STATE 2 -----
C     VST2 = VST2 * U
C
      DO 930 I=1,NAOS
         DO 910 J=1,NACT
            DUM = ZERO
            DO 900 K=1,NACT
               DUM = DUM + VST2(I,K+NFZC)*U(K,J)
  900       CONTINUE
            IF(ABS(DUM).LT.SMALL) DUM=ZERO
Cnb         to do or not to do??
            SCR(J,1) = DUM
  910    CONTINUE
         DO 920 J=1,NACT
            VST2(I,J+NFZC) = SCR(J,1)
  920    CONTINUE
  930 CONTINUE
C
      CALL DAWRIT(IDAF,IODA,VST2,L3,19,0)
      IF(DBG  .OR.  PRTCMO) THEN
         WRITE(IW,FMT='(/1X,''STATE 2 CMO-S'')')
         CALL PRSQL(VST2,NOCC,L1,L1)
      END IF
C
C     ----- ADD UNIT CORE OVERLAPS -----
C
      IF(NFZC.EQ.0) GO TO 1100
      DO 1030 J=NACT,1,-1
         DEIG(J+NFZC) = DEIG(J)
 1030 CONTINUE
      DO 1040 J=1,NFZC
         DEIG(J)=ONE
 1040 CONTINUE
C
C     ----- PRINT THE CMO OVERLAP INTEGRALS -----
C
 1100 CONTINUE
      IF (MASWRK) THEN
         WRITE(IW,9040)
         WRITE(IW,9050) (DEIG(I),I=1,NOCC)
      END IF
C
C     ----- COMPUTE D = VST2-DAGGER * S * VST1 -----
C     AT THIS POINT, THE CMO-S SHOULD BE GENERATED.  CHECK THIS.
C     D IS THE OVERLAP BETWEEN THE TWO SETS OF CMO-S, WHICH
C     SHOULD BE DIAGONAL, WITH DEIG ON THE DIAGONAL.
C
      CALL DAREAD(IDAF,IODA,S,L2,12,0)
      DO 1110 I=1,NAOS
         IA(I) = (I*I-I)/2
 1110 CONTINUE
      NERR=0
C
      DO 1150 J = 1,NAOS
         DO 1140 I = 1,NOCC
            DUM = ZERO
            DO 1130 K = 1,NAOS
               KJ= IA(K)+J
               IF(K.LT.J) KJ= IA(J)+K
               DUM = DUM+VST2(K,I)*S(KJ)
 1130       CONTINUE
            WRK(I,J) = DUM
 1140    CONTINUE
 1150 CONTINUE
C
      DO 1190 I = 1,NOCC
         DO 1180 J = 1,NOCC
            DUM = ZERO
            DO 1170 K = 1,NAOS
              DUM = DUM+WRK(I,K)*VST1(K,J)
 1170       CONTINUE
            IF(I.EQ.J) THEN
               TEST = DUM - DEIG(J)
            ELSE
               TEST = DUM
            END IF
            IF(ABS(TEST).GT.THRSH) NERR = NERR+1
            D(I,J) = DUM
 1180    CONTINUE
 1190 CONTINUE
C
      IF(NERR.GT.0) THEN
         IF (MASWRK) THEN
            WRITE(IW,9060) NERR
            WRITE(IW,FMT='(/1X,''CMO OVERLAP MATRIX'')')
         END IF
         CALL PRSQ(D,NOCC,NOCC,NOCC)
         CALL ABRT
         STOP
      END IF
C
      IF(DBG) THEN
         WRITE(IW,FMT='(/1X,''CMO OVERLAP MATRIX'')')
         CALL PRSQ(D,NOCC,NOCC,NOCC)
         WRITE(IP,*) ' $VEC1'
         CALL PUSQL(VST1,N1,L1,L1)
         WRITE(IP,*) ' $END'
         WRITE(IP,*) ' $VEC2'
         CALL PUSQL(VST2,N1,L1,L1)
         WRITE(IP,*) ' $END'
      END IF
C
      RETURN
C
 8000 IF (MASWRK) THEN
         WRITE (IW,9100)
         WRITE (IW,9050) (EIG(I),I=1,NOCC)
      END IF
      CALL ABRT
      RETURN
C
 9000 FORMAT(//10X,22("-")/10X,'CORRESPONDING ORBITALS'/10X,22("-"))
 9010 FORMAT(1X,'THERE ARE',I5,' FROZEN CORE ORBITALS, AND',
     *          I5,' ACTIVE ORBITALS')
 9020 FORMAT(1X,'***** ERROR ***** THE CORE ORBITALS ARE NOT IDENTICAL'/
     *       1X,'MAX.DEVIATION FROM UNIT MATRIX IS',1P,E12.4,0P,
     *          ' AT I,J=',2I6/
     *      1X,'IF YOU THINK YOUR INPUT ORBITALS ARE REALLY IDENTICAL,'/
     *       1X,' ADJUST PARAMETERS TOLZ/TOLE IN $TRANST DOWNWARD.'/
     *       1X,'ORBITAL OVERLAP MATRIX IS:')
 9030 FORMAT(1X,'ABANDONING THE CORRESPONDING MO COMPUTATION...'/
     *       1X,'YOU HAVE INPUT IDENTICAL $VEC GROUPS.')
 9040 FORMAT(1X,'THE CORRESPONDING ORBITAL OVERLAPS ARE')
 9050 FORMAT(1X,5F13.10)
 9060 FORMAT(1X,'***** ERROR ***** CMO OVERLAP MATRIX IS NOT CORRECT.'/
     *    1X,'THIS SHOULD BE DIAGONAL, WITH OVERLAPS ON THE DIAGONAL.'/
     *    1X,'THERE ARE',I10,' MISTAKES IN THIS MATRIX.')
 9100 FORMAT(1X,' SOME EIGENVALUES OF D+*D MATRIX ARE NEGATIVE.')
      END
C*MODULE TRNSTN  *DECK TRNLOO
      SUBROUTINE TRNLOO(INDX,NABC,IARC,NLWKS,NUWKS,PUWK,IWGHT,NROWS,
     *                  ISYM,LEVPT,LEVNR,NROWS4,XMOINT,YMOINT,ZMOINT,
     *                  TDM,NOCC,NOCC2,CI1VEC,CI2VEC,CSFOV,NCIVEC,
     *                  DMX,DMY,DMZ,IVCORB,DEIG)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER PUWK
      LOGICAL GOPARR,DSKWRK,MASWRK
      PARAMETER (MXAO=2047)
C
C....  THIS ROUTINE IS A STRIPPED DOWN VERSION OF LOOPY THAT ONLY
C....  HANDLES THE ONE-ELECTRON LOOPS
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /LOOPS1/ ACOF,NWKS,NORBS,IB,JB,IUWK,JUWK,NUWK,NLWK,IPAD(2)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      DIMENSION XMOINT(NOCC,NOCC),YMOINT(NOCC,NOCC),ZMOINT(NOCC,NOCC),
     *          TDM(NOCC2),CI1VEC(NCIVEC),CI2VEC(NCIVEC),CSFOV(NCIVEC),
     *          ISEGM(MXAO),JSEGM(MXAO),IMAIN(MXAO),ISUB(MXAO),
     *          ACOEF(MXAO),IUWKMN(MXAO),IUWKSB(MXAO),ISHIFT(4),
     *          ISYM(MXAO),LEVPT(MXAO),LEVNR(MXAO),NABC(NROWS),
     *          IARC(NROWS4),NLWKS(NROWS),INDX(NCIVEC),NUWKS(NROWS),
     *          PUWK(NROWS),IWGHT(NROWS4),JSEGNR(3),JSEGPT(3),IARCMN(21)
     *         ,IARCSB(21),NXTSEG(21),JXT(21),JMN(21),JSB(21),
     *          COEFFS(20,5),CFS(100),IVCORB(*),DEIG(*)
C
      EQUIVALENCE (COEFFS(1,1),CFS(1))
      EQUIVALENCE (JXT(1),NXTSEG(1))
      EQUIVALENCE (JMN(1),IARCMN(1))
      EQUIVALENCE (JSB(1),IARCSB(1))
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, TWO=2.0D+00)
C
      DATA JSEGNR/7,14,21/
      DATA JXT/0,0,0,2,2,3,3,0,0,2,2,2,2,3,0,0,3,3,3,3,2/
      DATA JMN/2,3,4,3,4,2,4,1,2,1,2,3,4,2,1,3,1,2,3,4,3/
      DATA JSB/2,3,4,1,2,1,3,3,4,1,2,3,4,3,2,4,1,2,3,4,2/
C
      JSEGPT(1) = 0
      DO 100 I = 1,2
  100 JSEGPT(I+1) = JSEGNR(I)
      DO 120 I = 1,2
      DO 120 J = 1,5
  120 COEFFS(I,J) = ZERO
C
      CALL VCLR(TDM,1,NOCC2)
C
C....  THESE ARE THE POSSIBLE SEGMENT COEFFICIENTS
C
      DO 140 I = 3,20
      A = I-2
      COEFFS(I,1) = ONE/A
      COEFFS(I,2) = -ONE/A
      COEFFS(I,3) = SQRT((A+ONE)/A)
      COEFFS(I,4) = SQRT(A/(A+ONE))
      COEFFS(I,5) = SQRT(A*(A+TWO))/(A+ONE)
  140 CONTINUE
      DO 160 K = 1,4
      ISHIFT(K) = (K-1)*NROWS
  160 CONTINUE
      NLEVS = NORBS+1
C
C....  START THE MAIN LOOP
C
      DO 640 LEVI = 2,NLEVS
      LEV = LEVI
      LEVM = LEV-1
      JSM = ISYM(LEVM)
      IAD = LEVM
      NR = LEVNR(LEV)
      NPT = LEVPT(LEV)
      DO 620 IROW = 1,NR
      NPT = NPT+1
      ISEGM(LEV) = 1
      ISEG = 1
      IMN = NPT
      ISB = NPT
      KSEG = 0
      KSEGMX = JSEGNR(ISEG)
      IUWKMN(LEV) = PUWK(NPT)
      IUWKSB(LEV) = PUWK(NPT)
      IMAIN(LEV) = NPT
      ISUB(LEV) = NPT
      NUWK = NUWKS(NPT)
      ACOEF(LEV) = ONE
C
C....  SEARCH FOR NEXT LOOP SEGMENT
C
  180 KSEG = KSEG + 1
      IF (KSEG .GT. KSEGMX) GO TO 600
      KMN = IARCMN(KSEG)
      IARPT = IMN + ISHIFT(KMN)
      KMN = IARC(IARPT)
      IF (KMN .EQ. 0) GO TO 180
      KSB = IARCSB(KSEG)
      JARPT = ISB + ISHIFT(KSB)
      KSB = IARC(JARPT)
      IF (KSB .EQ. 0) GO TO 180
      JSEGM(LEV) = KSEG
      IUWKMN(LEVM) = IUWKMN(LEV) + IWGHT(IARPT)
      IUWKSB(LEVM) = IUWKSB(LEV) + IWGHT(JARPT)
C
C....  HAVING FOUND A VALID SEGMENT, UPDATE THE VALUE OF THE COEFFICIENT
C
      GO TO (200,200,260,200,340,200,320,200,300,200,380,220,220,240,
     +     200,360,200,220,400,220,280),KSEG
C
  200 ACOEF(LEVM) = ACOEF(LEV)
      GO TO 420
C
  220 ACOEF(LEVM) = -ACOEF(LEV)
      GO TO 420
C
  240 IA = NABC(IMN)+2
      ACOEF(LEVM) = ACOEF(LEV)*CFS(IA)
      GO TO 420
C
  260 ACOEF(LEVM) = ACOEF(LEV)+ACOEF(LEV)
      GO TO 420
C
  280 IA = NABC(IMN)+24
      ACOEF(LEVM) = ACOEF(LEV)*CFS(IA)
      GO TO 420
C
  300 IA = NABC(IMN)+42
      ACOEF(LEVM) = ACOEF(LEV)*CFS(IA)
      GO TO 420
C
  320 IA = NABC(IMN)+43
      ACOEF(LEVM) = ACOEF(LEV)*CFS(IA)
      GO TO 420
C
  340 IA = NABC(IMN)+62
      ACOEF(LEVM) = ACOEF(LEV)*CFS(IA)
      GO TO 420
C
  360 IA = NABC(IMN)+63
      ACOEF(LEVM) = ACOEF(LEV)*CFS(IA)
      GO TO 420
C
  380 IA = NABC(IMN)+81
      ACOEF(LEVM) = ACOEF(LEV)*CFS(IA)
      GO TO 420
C
  400 IA = NABC(IMN)+83
      ACOEF(LEVM) = ACOEF(LEV)*CFS(IA)
      GO TO 420
C
  420 CONTINUE
      IF (NXTSEG(KSEG) .GT. 0) GO TO 580
      IF (ISYM(LEVM) .NE. JSM) GO TO 180
      IF (KMN-KSB) 440,560,440
C
C....  UPDATE ALL VARIABLES FOR THE NEW SEGMENT
C
  440 LEVL = LEVM
      KSEGMX = 4
  460 LEV = LEVM
      LEVM = LEV-1
      IF (LEVM .LE. 0) THEN
         IF (MASWRK) WRITE (IW,*) 'PROBLEMS WITH PARTIAL SPACE'
         CALL ABRT
         STOP
      END IF
      KSEG = 0
      IMN = KMN
      IMAIN(LEV) = KMN
      ISB = KSB
      ISUB(LEV) = KSB
  500 KSEG = KSEG + 1
      IF (KSEG .GT. KSEGMX) GO TO 540
      IARPT = IMN + ISHIFT(KSEG)
      KMN = IARC(IARPT)
      IF (KMN .LE. 0) GO TO 500
      JARPT = ISB + ISHIFT(KSEG)
      KSB = IARC(JARPT)
      IF (KSB .LE. 0) GO TO 500
      JSEGM(LEV) = KSEG
      IMAIN(LEVM) = KMN
      IUWKMN(LEVM) = IUWKMN(LEV)+IWGHT(IARPT)
      ISUB(LEVM) = KSB
      IUWKSB(LEVM) = IUWKSB(LEV)+IWGHT(JARPT)
      IF (KMN-KSB) 460,520,460
C
C....  A LOOP HAS BEEN CONSTRUCTED; FIND ITS CONTRIBUTION AND ADD
C....  TO THE DENSTIY MATRIX
C
  520 IB = IAD
      JB = LEVL
      NLWK = NLWKS(KMN)
      IUWK = IUWKMN(LEVM)
      JUWK = IUWKSB(LEVM)
      ACOF = ACOEF(LEVL)
C
      CALL TRNMAK(XMOINT,YMOINT,ZMOINT,TDM,NOCC,NOCC2,CI1VEC,CI2VEC,
     *            CSFOV,INDX,NCIVEC,DMX,DMY,DMZ,IVCORB,DEIG)
      GO TO 500
C
  540 IF (LEV .EQ. LEVL) GO TO 600
      LEVM = LEV
      LEV = LEVM+1
      IMN = IMAIN(LEV)
      ISB = ISUB(LEV)
      KSEG = JSEGM(LEV)
      GO TO 500
C
C....  A LOOP HAS BEEN CONSTRUCTED; ADD ITS CONTRIBUTION TO THE DENSITY
C....  MATRIX
C
  560 IB = IAD
      JB = LEVM
      NLWK = NLWKS(KMN)
      IUWK = IUWKMN(LEVM)
      JUWK = IUWKSB(LEVM)
      ACOF = ACOEF(LEVM)
C
      CALL TRNMAK(XMOINT,YMOINT,ZMOINT,TDM,NOCC,NOCC2,CI1VEC,CI2VEC,
     *            CSFOV,INDX,NCIVEC,DMX,DMY,DMZ,IVCORB,DEIG)
      GO TO 180
C
  580 CONTINUE
      LEV = LEVM
      LEVM = LEV - 1
      ISEG = NXTSEG(KSEG)
      ISEGM(LEV) = ISEG
      KSEG = JSEGPT(ISEG)
      IMN = KMN
      IMAIN(LEV) = KMN
      ISB = KSB
      ISUB(LEV) = KSB
      KSEGMX = JSEGNR(ISEG)
      GO TO 180
C
  600 CONTINUE
      IF (LEV .EQ. LEVI) GO TO 620
      LEVM = LEV
      LEV = LEVM + 1
      ISEG = ISEGM(LEV)
      IMN = IMAIN(LEV)
      ISB = ISUB(LEV)
      KSEG = JSEGM(LEV)
      KSEGMX = JSEGNR(ISEG)
      GO TO 180
C
  620 CONTINUE
  640 CONTINUE
      RETURN
C
      END
C*MODULE TRNSTN  *DECK TRNMAK
      SUBROUTINE TRNMAK(XMOINT,YMOINT,ZMOINT,TDM,N1,N2,CI1VEC,CI2VEC,
     *                  CSFOV,INDX,NCIVEC,DMX,DMY,DMZ,IVCORB,DEIG)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXAO=2047)
C
      DIMENSION XMOINT(N1,N1),YMOINT(N1,N1),ZMOINT(N1,N1),TDM(N2),
     *          CI1VEC(NCIVEC),CI2VEC(NCIVEC),CSFOV(NCIVEC),
     *          INDX(NCIVEC),IVCORB(*),DEIG(*)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /LOOPS1/ ACOEF,NWKS,NORBS,IB,JB,IUWK,JUWK,NUWK,NLWK,
     *                IHAI,IHC
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
C
C     ----- FORM TRANSITION DENSITY MATRIX AND TRANSITION DIPOLE -----
C
C     IIMO=IVCORB(IB,ici1)
C     JJMO=IVCORB(JB,ici2)
      IIMO=IVCORB(IB)
      JJMO=IVCORB(JB)
      FACTII = ONE/DEIG(IIMO)
      FACTJJ = ONE/DEIG(JJMO)
      IND = IUWK
      JND = JUWK
      DCIJ = ZERO
      DCJI = ZERO
C
      DO 120 I=1,NLWK
C        if(mod(i,nproc).ne.me) goto 115
         II = INDX(IND)
         JJ = INDX(JND)
         DO 110 J=1,NUWK
            DCIJ = DCIJ + CI1VEC(II)*CI2VEC(JJ)*CSFOV(II)*FACTII
            IF(II.NE.JJ)
     *      DCJI = DCJI + CI1VEC(JJ)*CI2VEC(II)*CSFOV(JJ)*FACTJJ
            II = II + 1
            JJ = JJ + 1
  110    CONTINUE
C 115    CONTINUE
         IND = IND + 1
         JND = JND + 1
  120 CONTINUE
C     call ddi_gsumf(2300,DCIJ,1)
C     call ddi_gsumf(2301,DCJI,1)
C
      TIJ = DCIJ+DCJI
      IJ = IA(IB) + JB
      TDM(IJ)  = TDM(IJ) + ACOEF*TIJ
C
      XDCMO = DCIJ*XMOINT(IIMO,JJMO) + DCJI*XMOINT(JJMO,IIMO)
      YDCMO = DCIJ*YMOINT(IIMO,JJMO) + DCJI*YMOINT(JJMO,IIMO)
      ZDCMO = DCIJ*ZMOINT(IIMO,JJMO) + DCJI*ZMOINT(JJMO,IIMO)
C
      DMX = DMX + ACOEF * XDCMO
      DMY = DMY + ACOEF * YDCMO
      DMZ = DMZ + ACOEF * ZDCMO
C
      RETURN
      END
C*MODULE TRNSTN  *DECK DIPMOM
      SUBROUTINE DIPMOM(NFT17,NROWS,NWKS,EXETYP,ENGYST,NWKSST,MULST,
     *                  IROOTS,IVCORB,DEIG,ISTSYM,IRRR,NOSYM,IPRHSO)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL DBG,GOPARR,DSKWRK,MASWRK,SSS(3),SYRYES
C
      PARAMETER (MXATM=500, MXAO=2047)
C
      CHARACTER LENSYM(3)
      DIMENSION ITAG(3,2)
      DIMENSION MAP(MXAO),ENGYST(MXRTTRN,*),ISTSYM(MXRTTRN,*),
     *          NWKSST(*),MULST(*),
     *          IROOTS(*),IVCORB(NUM,*),DEIG(*),IRRR(3)
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /LOOPS1/ ACOEF,NNWKS,NNORBS,IB,JB,IUWK,JUWK,NUWK,NLWK,
     *                IPAD(2)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /ORBSET/ NORBMX,NORBS,NCORBS,NLEVS,NNA,NNB,NNC,NSYM,MSYM,
     *                IDOCC,IVAL,IMCC,ISYM(MXAO),ICODE(MXAO),
     *                NLCS(MXAO),LEVPT(MXAO),LEVNR(MXAO),IOUT(MXAO),
     *                NREFS,IEXCT,NFOCI,INTACT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,
     *                MXRTTRN,NSTAT
      COMMON /XYZPRP/ XP,YP,ZP,
     *                DIPMX,DIPMY,DIPMZ,
     *                QXX,QYY,QZZ,QXY,QXZ,QYZ,
     *                QMXX,QMYY,QMZZ,QMXY,QMXZ,QMYZ,
     *                OXXX,OXXY,OXXZ,OXYY,OYYY,OYYZ,
     *                OXZZ,OYZZ,OZZZ,OXYZ,
     *                OMXXX,OMXXY,OMXXZ,OMXYY,OMYYY,
     *                OMYYZ,OMXZZ,OMYZZ,OMZZZ,OMXYZ
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, TWO=2.0D+00,
     *           THREE=3.0D+00, EIGHT=8.0D+00,
     *           PLANCK=6.626176D-27, CLIGHT=2.9979925D+10,
     *           BOHR=5.291771D-09, ECHARG=4.803242D-10,
     *           EV=27.212D+00, DEBYE=2.541766D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DEBUG/8HDEBUG   /, DBUGME/8HDIPMOM  /
#else
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA DEBUG_STR/"DEBUG   "/, DBUGME_STR/"DIPMOM  "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA ITAG/4H   X,4H   Y,4H   Z,
     *          4HD/DX,4HD/DY,4HD/DZ/
#else
      CHARACTER*4 :: ITAG_STR(3,2)
      EQUIVALENCE (ITAG, ITAG_STR)
      DATA ITAG_STR/"   X","   Y","   Z",
     *          "D/DX","D/DY","D/DZ"/
#endif
C
C     ----- COMPUTE THE TRANSITION MOMENT -----
C     BOTH LENGTH (DIPOLE) AND VELOCITY FORMS ARE COMPUTED.
C
      DBG = EXETYP.EQ.DEBUG  .OR.  EXETYP.EQ.DBUGME
     *      .AND. MASWRK
      ICI1=1
      ICI2=NUMCI
C
      PI = ACOS(-ONE)
      NSTATE = MAX(IROOTS(ICI1),IROOTS(ICI2))
      IF (MASWRK) WRITE(IW,9000)
C
C     ----- GROW MEMORY -----
C
      L1 = NUM
      L2 = (L1*L1+L1)/2
      L3 = L1*L1
      N1 = NOCC
      N2 = (N1*N1+N1)/2
      N3 = N1*N1
C
      CALL VALFM(LOADFM)
C                   FOR DIPOLE INTEGRALS, AND TRANSFORMATION
      LXMO   = LOADFM + 1
      LYMO   = LXMO   + N3
      LZMO   = LYMO   + N3
      LVST1  = LZMO   + N3
      LVST2  = LVST1  + L3
      LXAO   = LVST2  + L3
      LYAO   = LXAO   + L2
      LZAO   = LYAO   + L2
      LTDM   = LZAO   + L2
      LWRK1  = LTDM   + MAX(N2,L2)
      LWRK2  = LWRK1  + NORBS*NORBS
      LAST   = LWRK2  + NORBS*L1
C                   FOR DRT INFORMATION
      IH1    = LAST
      IH2    = IH1    + (NROWS-1)/NWDVAR +1
      IH3    = IH2    + (NROWS*4-1)/NWDVAR +1
      IH4    = IH3    + (NROWS-1)/NWDVAR +1
      IH5    = IH4    + (NROWS-1)/NWDVAR +1
      IH6    = IH5    + (NROWS-1)/NWDVAR +1
      IHA    = IH6    + (NROWS*4-1)/NWDVAR +1
C                   FOR CI COEFS, AND CSF OVERLAPS
      LCI1   = IHA    + (NWKS-1)/NWDVAR +1
      LCI2   = LCI1   + NWKS*NSTATE
      LOVER  = LCI2   + NWKS*NSTATE
      LASTX  = LOVER  + NWKS
      NEED = LASTX- LOADFM
      CALL GETFM(NEED)
      IF(MASWRK) WRITE(IW,9002) NEED
C
C     ----- RECOVER THE TWO SETS OF MOLECULAR ORBITALS FROM DAF -----
C
      CALL DAREAD(IDAF,IODA,X(LVST1),L3,15,0)
      CALL DAREAD(IDAF,IODA,X(LVST2),L3,19,0)
      IF(DBG) THEN
         WRITE(IW,FMT='(1X,''STATE 1 CMO-S'')')
         CALL PRSQL(X(LVST1),N1,L1,L1)
         WRITE(IW,FMT='(1X,''STATE 2 CMO-S'')')
         CALL PRSQL(X(LVST2),N1,L1,L1)
      END IF
C
C     ----- RECOVER STATE 1'S CI COEFS, AND CSF OVERLAPS -----
C
      IF(MASWRK) WRITE(IW,9021) IROOTS(ICI1)
      IF(DBG) WRITE(IW,FMT='(1X,''STATE 1 CI VECTOR'')')
      CALL SEQREW(NFT17)
      ITEMP = LCI1
      DO 120 K=1,IROOTS(ICI1)
         CALL SQREAD(NFT17,X(ITEMP),NWKS)
         IF(DBG) WRITE(IW,9120) (X(ITEMP-1+I),I=1,NWKS)
         ITEMP=ITEMP +NWKS
  120    CONTINUE
C
      CALL SQREAD(NFT17,X(LOVER),NWKS)
      IF(DBG) THEN
         WRITE(IW,FMT='(1X,''STATE 1 CSF OVERLAPS'')')
         WRITE(IW,9120) (X(LOVER-1+I),I=1,NWKS)
         ENDIF
C
C     ----- RECOVER STATE 2'S CI COEFFICIENTS -----
C
      IF(NUMCI.EQ.2) THEN
         IF(MASWRK) WRITE(IW,9022) IROOTS(ICI2)
         CALL SEQREW(NFT12)
         IF(DBG) WRITE(IW,FMT='(1X,''STATE 2 CI VECTOR'')')
         ITEMP = LCI2
         DO 140 K=1,IROOTS(ICI2)
            CALL SQREAD(NFT12,X(ITEMP),NWKS)
            IF(DBG) WRITE(IW,9120) (X(ITEMP-1+I),I=1,NWKS)
            ITEMP = ITEMP +NWKS
  140    CONTINUE
      ELSE
         LCI2=LCI1
      ENDIF
C     CALL TIMIT(1)
C
C     ----- LOOP OVER BOTH METHODS -----
C     IFORM=1 COMPUTES THE LENGTH FORM   - HERMITIAN OPERATOR
C     IFORM=2 COMPUTES THE VELOCITY FORM - ANTIHERMITIAN OPERATOR
C
      IFORM=1
      NDAFX = 95
      NDAFY = 96
      NDAFZ = 97
C
      IF(MASWRK) CALL SOCINFO(NOSYM,IPRHSO)
C
  200 CONTINUE
      IF(MASWRK) THEN
         IF(IFORM.EQ.1) THEN
            WRITE(IW,9020)
            WRITE(IP,7200)
         ELSE
            WRITE(IW,9030)
            WRITE(IP,7202)
         ENDIF
      END IF
C
C     ---- RECOVER APPROPRIATE PROPERTIES INTEGRALS FROM DAF -----
C
      CALL DAREAD(IDAF,IODA,X(LXAO),L2,NDAFX,0)
      CALL DAREAD(IDAF,IODA,X(LYAO),L2,NDAFY,0)
      CALL DAREAD(IDAF,IODA,X(LZAO),L2,NDAFZ,0)
C
C     ----- TRANSFORM THE AO INTEGRALS TO MO INTEGRALS -----
C
      CALL TMOINT(X(LXMO),X(LVST1),X(LVST2),X(LXAO),
     *            ITAG(1,IFORM),IFORM,N1,L1,L2,DBG)
      CALL TMOINT(X(LYMO),X(LVST1),X(LVST2),X(LYAO),
     *            ITAG(2,IFORM),IFORM,N1,L1,L2,DBG)
      CALL TMOINT(X(LZMO),X(LVST1),X(LVST2),X(LZAO),
     *            ITAG(3,IFORM),IFORM,N1,L1,L2,DBG)
C
C     Loop over states:
C     DBG = .true.
C     ------------
      ITEMP1 = LCI1 -NWKS
      DO 100 IRT1=1,IROOTS(ICI1)
         ITEMP1 = ITEMP1 +NWKS
         ITEMP2 = LCI2 -NWKS
         DO 100 IRT2=1,IROOTS(ICI2)
            ITEMP2 = ITEMP2 +NWKS
            IF(IRT1.GT.IRT2) GO TO 100
C
            CALL CISOL(SSS(1),ISTSYM(IRT1,ICI1),-1,ISTSYM(IRT2,ICI2),
     *                 IRRR,0)
            CALL CISOL(SSS(3),ISTSYM(IRT1,ICI1), 0,ISTSYM(IRT2,ICI2),
     *                 IRRR,0)
            CALL CISOL(SSS(2),ISTSYM(IRT1,ICI1), 1,ISTSYM(IRT2,ICI2),
     *                 IRRR,0)
            SYRYES=SSS(1).OR.SSS(2).OR.SSS(3)
            DO 110 MM=1,3
               IF(SSS(MM)) THEN
                  LENSYM(MM)='C'
               ELSE
                  LENSYM(MM)='Z'
               ENDIF
  110       CONTINUE
C           write(iw,*) 'symmetries (x,y,z)',sss(1),sss(2),sss(3)
C
C           always irt1 < irt2:
C           write(iw,9101) irt1,irt2
C9101 format(/1x,'------------- irt1, irt2 =',2i5)
C
            DMX = ZERO
            DMY = ZERO
            DMZ = ZERO
            DELENG = ABS(ENGYST(IRT1,ICI1)-ENGYST(IRT2,ICI2))
            EXC = EV * DELENG
            ERG = ECHARG * ECHARG / BOHR
            FREQ = DELENG * ERG / PLANCK
            OMEGA = FREQ / CLIGHT
            IF(DBG) THEN
               WRITE(IW,FMT='(1X,''STATE 1 IVCORB'')')
               WRITE(IW,9110) (IVCORB(I,ICI1),I=1,NOCC-NFZC)
               WRITE(IW,FMT='(1X,''STATE 2 IVCORB'')')
               WRITE(IW,9110) (IVCORB(I,ICI2),I=1,NOCC-NFZC)
               WRITE(IW,FMT='(1X,''CMO OVERLAPS'')')
               WRITE(IW,9120) (DEIG(I),I=1,NOCC)
            END IF
C
C           write(iw,*) 'node',me,'cond',.not.syryes.and.nosym.eq.0
            IF(.NOT.SYRYES.AND.NOSYM.EQ.0) GOTO 300
C
C     ----- READ -DRT- DATA -----
C
            CALL SEQREW(NFT11)
            READ (NFT11)
            READ (NFT11)
            NROWS4 = NROWS*4
            CALL DRTDM(X(IHA),X(IH1),X(IH2),X(IH3),X(IH4),X(IH5),X(IH6)
     *                ,MAP,ISYM,LEVNR,LEVPT,NROWS,NROWS4,NWKS,NFT11)
C
C     ----- TRNLOO GENERATES THE GUGA LOOPS,
C           AND MAKES THE TRANSITION DENSITY -----
C
            NNWKS  = NWKS
            NNORBS = NORBS
C
C           note that ivcorb is passed only of ici1
C           because there is only one $DRT read ivcorb(1,ici1) and
C           ivcorb(1,ici2) are identical?
C           This may be actually wrong if more than one $DRT is present.
C
            CALL TRNLOO(X(IHA),X(IH1),X(IH2),X(IH3),X(IH4),X(IH5),X(IH6)
     *                 ,NROWS,ISYM,LEVPT,LEVNR,NROWS4,X(LXMO),X(LYMO),
     *                  X(LZMO),X(LTDM),NOCC,N2,X(ITEMP1),X(ITEMP2),
     *                  X(LOVER),NWKS,DMX,DMY,DMZ,IVCORB(1,ICI1),DEIG)
C
            IF(IFORM.EQ.1) THEN
C
C           FORM TRANSITION DENSITY MATRIX IN CMO AND AO BASIS
C
               CALL TRNAO(NORBS,L1,L2,N1,N2,IVCORB(1,ICI1),X(LTDM),
     *                    X(LWRK1),X(LWRK2),X(LVST1),X(LVST2),X(LXAO),
     *                    X(LYAO),X(LZAO),X(LXMO),X(LYMO),X(LZMO),DBG)
            ENDIF
C
  300       CONTINUE
C
            IF(IFORM.EQ.1) THEN
C
C     ----- PREPARE FINAL LENGTH FORM RESULTS -----
C
               DIP = SQRT(DMX*DMX + DMY*DMY + DMZ*DMZ)
               WMX = DMX*DEBYE
               WMY = DMY*DEBYE
               WMZ = DMZ*DEBYE
               WIP = DIP*DEBYE
C
C     ----- OSCILLATOR STRENGTH AND EINSTEIN COEFFS -----
C
C           THE FORMULA FOR F ASSUMES DEL-E,<R> IN ATOMIC UNITS.
C
C                 8 * PI**3
C           B = -------------  * <R>**2
C                3 * H**2 * C
C
C                8 * PI * H * FREQ**3
C           A = ---------------------- * B
C                      C**2
C
C          THE ABOVE FORMULAE ASSUME G=1 FOR BOTH STATES.
C
               F = TWO * DELENG * DIP * DIP/THREE
               FACT = (DEBYE * 1.0D-18 / PLANCK)
               FACT = EIGHT * PI * PI * PI * FACT * FACT
               FACT = FACT/(THREE*CLIGHT)
               B = FACT * DIP * DIP
               A = EIGHT * PI * DELENG * ERG * OMEGA * OMEGA * B
               IF(MASWRK) THEN
                  IF(IRT1.EQ.IRT2) WRITE(IW,9005)
                  WRITE(IW,9010) IRT1,IRT2,MULST(ICI1),MULST(ICI2),
     *                           NWKSST(ICI1),NWKSST(ICI2),ENGYST(IRT1,
     *                           ICI1),ENGYST(IRT2,ICI2),FREQ,OMEGA,EXC
                  WRITE(IW,9200) LENSYM(1),LENSYM(2),LENSYM(3),XP,YP,ZP,
     *                           DMX,DMY,DMZ,DIP,WMX,WMY,WMZ,WIP
                  IF(IRT1.NE.IRT2) WRITE(IW,9210) F,A,B
               ENDIF
               WRITE(IP,7220) IRT1,IRT2,NWKS
               WRITE(IP,7222) ENGYST(IRT1,ICI1),ENGYST(IRT2,ICI2)
               WRITE(IP,7224) DMX,DMY,DMZ
C
C     ----- DONE WITH LENGTH FORM, GET READY FOR VELOCITY FORM -----
C
            ELSE
C
C     ----- FINAL RESULTS FOR VELOCITY FORM -----
C
               DIP=SQRT(DMX*DMX+DMY*DMY+DMZ*DMZ)
C
C     ----- OSCILLATOR STRENGTH -----
C
               IF(ABS(DELENG).GT.1.0D-03) THEN
                  F = TWO*DIP*DIP / (THREE*DELENG)
               ELSE
                  F = -10.0D+00
               END IF
C
               IF(MASWRK) THEN
                  IF(IRT1.EQ.IRT2) WRITE(IW,9005)
                  WRITE(IW,9010) IRT1,IRT2,MULST(ICI1),MULST(ICI2),
     *                           NWKSST(ICI1),NWKSST(ICI2),ENGYST(IRT1,
     *                           ICI1),ENGYST(IRT2,ICI2),FREQ,OMEGA,EXC
                  WRITE(IW,9300) LENSYM(1),LENSYM(2),LENSYM(3),
     *                                   DMX,DMY,DMZ,DIP
                  IF(IRT1.NE.IRT2) WRITE(IW,9310) F
               ENDIF
               WRITE(IP,7220) IRT1,IRT2,NWKS
               WRITE(IP,7222) ENGYST(IRT1,ICI1),ENGYST(IRT2,ICI2)
               WRITE(IP,7224) DMX,DMY,DMZ
C
            ENDIF
C
            CALL TIMIT(1)
            CALL FLSHBF(IW)
            CALL FLSHBF(IP)
  100 CONTINUE
      WRITE(IP,7240)
C
      IF(IFORM.EQ.1) THEN
         IFORM=2
         NDAFX = 84
         NDAFY = 85
         NDAFZ = 86
         GO TO 200
         ENDIF
C
      CALL SEQREW(NFT11)
      CALL SEQREW(NFT12)
      CALL SEQREW(NFT17)
      CALL RETFM(NEED)
C     CALL TIMIT(1)
      RETURN
 7200 FORMAT(' $TDIPOLE')
 7202 FORMAT(' $TVELOCITY')
 7220 FORMAT(3I10)
 7222 FORMAT(2F20.10)
 7224 FORMAT(3F20.10)
 7240 FORMAT(' $END')
 9000 FORMAT(/10X,18('-')/10X,'TRANSITION MOMENTS'/10X,18('-')/)
 9002 FORMAT(1X,I10,' WORDS OF MEMORY ARE REQUIRED.',/)
 9005 FORMAT(/1X,'THE NEXT PAIR ARE THE SAME STATE, SO THIS IS AN',
     *          ' EXPECTATION VALUE,'/
     *        1X,'RATHER THAN A TRANSITION MOMENT.')
 9010 FORMAT(/1X,'CI STATE NUMBER=',2I3,' STATE MULTIPLICITY=',2I3/
     *        1X,'NUMBER OF CSF-S=',2I10,
     *      /1X,'STATE ENERGIES    ',2F20.10,
     *      /1X,'TRANSITION ENERGY=',1P,E12.4,0P,' [1/SEC] =',F12.2,
     *          ' [1/CM] =',F12.2,' [EV]')
 9020 FORMAT(/10X,'---- LENGTH FORM ----')
 9021 FORMAT(1X,'RECOVER CI INFORMATION OF STATE 1.  IROOTS=',I5)
 9022 FORMAT(1X,'RECOVER CI INFORMATION OF STATE 2.  IROOTS=',I5)
 9030 FORMAT(/10X,'---- VELOCITY FORM ----')
 9110 FORMAT(1X,15I4)
 9120 FORMAT(1X,6F12.9)
 9200 FORMAT(1X,25X,'X [',A1,']',7X,'Y [',A1,']',7X,'Z [',A1,']',7X,
     *              'NORM'/
     *       1X,'CENTER OF MASS    =',3F12.6,12X,' BOHR'/
     *       1X,'TRANSITION DIPOLE =',4F12.6,' E*BOHR'/
     *       1X,'TRANSITION DIPOLE =',4F12.6,' DEBYE')
 9210 FORMAT(1X,'OSCILLATOR STRENGTH =',F12.6/
     *       1X,'EINSTEIN COEFFICIENTS: A=',1P,E12.4,
     *          ' 1/SEC; B=',E12.4,' SEC/G')
 9300 FORMAT(1X,28X,'X [',A1,']',5X,'Y [',A1,']',5X,'Z [',A1,']',5X,
     *              'NORM'/
     *       1X,'DIPOLE VELOCITY <D/DQ> =',4F10.6,' E/BOHR')
 9310 FORMAT(1X,'OSCILLATOR STRENGTH IS  ',F10.6)
      END
C*MODULE TRNSTN  *DECK TRNORB
      SUBROUTINE TRNORB(SCR,S,Q,VST1,VST2,L0,L1,L2,L3,TOLZ,TOLE,ORBIS)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL ORBIS
      CHARACTER*8 VEC1,VEC2
C
      DIMENSION SCR(L1,8),S(L2),Q(L3),VST1(L1,L1),VST2(L1,L1)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,
     *                MXRTTRN,NSTAT
C
C     CHARACTER*8 cvec
C     EQUIVALENCE (cvec,vec1)
C
      DATA VEC1,VEC2/' $VEC1  ',' $VEC2  '/
C
C     THIS ROUTINE READS THE ORBITALS FOR BOTH STATES,
C     THEN GUARANTEES ORTHONORMALITY OF BOTH SETS.
C
C     ortho generates faecal virtual orbitals so introduce a new option
C     if TOLE<0 then set TOLE=-TOLE and read all orbitals from $VECs
C
      NOCHRE=NOCC
      IF(TOLE.LT.0.0D+00) THEN
         TOLE=-TOLE
         NOCHRE=L1
      ENDIF
C
C            READ THE MO-S FOR STATE ONE, AND ORTHONORMALIZE.
C
C     if(mplevl.gt.0.and.numvec.gt.0) then
C     IF(numvec.LE.9) THEN
C         WRITE(UNIT=CVEC(1:8),FMT='(A5,I1,A2)') ' $VEC',ivec,'  '
C     ELSE
C         WRITE(UNIT=CVEC(1:8),FMT='(A5,I2,A1)') ' $VEC',Ivec,' '
C     ENDIF
C     endif
      IF(ORBIS) THEN
         CALL DAREAD(IDAF,IODA,VST1,L3,15,0)
      ELSE
         CALL TRNRDM(IR,IW,VEC1,L1,NOCHRE,VST1)
      ENDIF
      CALL ORTHO(Q,S,VST1,SCR,NOCHRE,L0,L1,L2,L1)
      CALL TFSQB(VST1,Q,SCR,L0,L1,L1)
      CALL STFASE(VST1,L1,L1,L1)
      CALL CLENMO(VST1,L1,L1,TOLZ,TOLE,IW,.FALSE.)
      CALL DAWRIT(IDAF,IODA,VST1,L3,15,0)
C
C            READ THE MO-S FOR STATE TWO, AND ORTHONORMALIZE.
C            THESE ARE TEMPORARILY STORED IN THE BETA MO SPOT
C
      IF(NUMVEC.EQ.2) THEN
C        only numvec=1 is supported with orbis at presens
         CALL TRNRDM(IR,IW,VEC2,L1,NOCHRE,VST2)
         CALL ORTHO(Q,S,VST2,SCR,NOCHRE,L0,L1,L2,L1)
         CALL TFSQB(VST2,Q,SCR,L0,L1,L1)
         CALL STFASE(VST2,L1,L1,L1)
         CALL CLENMO(VST2,L1,L1,TOLZ,TOLE,IW,.FALSE.)
         CALL DAWRIT(IDAF,IODA,VST2,L3,19,0)
      ELSE
         CALL DAWRIT(IDAF,IODA,VST1,L3,19,0)
      END IF
C
      RETURN
      END
C*MODULE TRNSTN  *DECK TRNRDM
      SUBROUTINE TRNRDM(IR,IW,VECNAM,NAOS,NMOS,VEC)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*8 VECNAM
      LOGICAL GOPARR,DSKWRK,MASWRK
      DIMENSION VEC(NAOS,NMOS)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C     ----- READ THE MO-S FOR A SINGLE STATE -----
C
      IZERO = 0
      IONE  = 1
      ITWO  = 2
#if defined(SPEC_CPU)
      IEND = 0
#endif
C
C     ----- POSITION INPUT TO THE DESIRED $VEC GROUP -----
C
      CALL SEQREW(IR)
      CALL FNDGRP(IR,VECNAM,IEOF)
      IF(IEOF.NE.0) GO TO 400
C
C     ----- READ IN THE ORBITALS -----
C
C MASTER WORK
C
      IF(MASWRK) THEN
      JJ = 0
      ICC = 0
      DO 280 J = 1,NMOS
         IMAX = 0
         IC = 0
  240    CONTINUE
            IMIN = IMAX+1
            IMAX = IMAX+5
            IC = IC+1
            IF(IMAX .GT. NAOS) IMAX = NAOS
            READ(IR,9010,ERR=300,END=300) JJ,ICC,(VEC(I,J),I=IMIN,IMAX)
            IF(JJ.EQ.MOD(J,100)  .AND.  ICC.EQ.IC) GO TO 260
               IF(MASWRK) WRITE(IW,9060) IC,J
               IF (GOPARR) CALL DDI_BCAST(350,'I',ITWO,1,MASTER)
               CALL ABRT
  260       CONTINUE
         IF(IMAX .LT. NAOS) GO TO 240
  280 CONTINUE
      IF (GOPARR) CALL DDI_BCAST(350,'I',IZERO,1,MASTER)
C
C SLAVE WORK, 0/1/2 MEANS OK/EOF OR OTHER ERROR/BOOBOO IN CONTENTS
C
      ELSE
         IF (GOPARR) CALL DDI_BCAST(350,'I',IEND,1,MASTER)
         IF (IEND.EQ.IONE) CALL ABRT
         IF (IEND.EQ.ITWO) CALL ABRT
      END IF
C
C  GIVE VECTORS TO ALL PROCESSES
C
      IF (GOPARR) CALL DDI_BCAST(351,'F',VEC,NMOS*NAOS,MASTER)
      RETURN
C
C     ----- BOOBOO OF SOME SORT IN READING THE COEFFS -----
C
  300 CONTINUE
      IF (MASWRK) THEN
         IF (GOPARR) CALL DDI_BCAST(350,'I',IONE,1,MASTER)
         WRITE(IW,9040) J,VECNAM,NMOS,NAOS
      END IF
      CALL ABRT
      STOP
C
C     ----- THE DESIRED $VEC GROUP WAS NOT FOUND -----
C
  400 CONTINUE
      IF (MASWRK) WRITE(IW,9020) VECNAM
      CALL ABRT
      STOP
C
 9010 FORMAT(I2,I3,5E15.0)
C
 9020 FORMAT(1X,'**** ERROR *****, THE',A8,'VECTOR GROUP WAS NOT FOUND')
 9040 FORMAT(1X,'**** ERROR *****, ERROR READING MO=',I10/
     *       1X,'GROUP=',A8,' EXPECTING NMOS,NAOS=',2I10)
 9060 FORMAT(1X,'***** ERROR *****, MO-S OUT OF ORDER'/
     *       1X,'CHECK CARD',I5,' OF MO',I5)
      END
C*MODULE TRNSTN  *DECK TRNSTX
      SUBROUTINE TRNSTX(NFT17,DEIG,ENGYST,NWKSST,MULST,IROOTS,IVCORB,
     *                  ISTSYM,MAXL,IRRR,NROWS,NOSYM,IPRHSO)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,
     *                MXRTTRN,NSTAT
C
      COMMON /XYZPRP/ XP,YP,ZP,
     *                DIPMX,DIPMY,DIPMZ,
     *                QXX,QYY,QZZ,QXY,QXZ,QYZ,
     *                QMXX,QMYY,QMZZ,QMXY,QMXZ,QMYZ,
     *                OXXX,OXXY,OXXZ,OXYY,OYYY,OYYZ,
     *                OXZZ,OYZZ,OZZZ,OXYZ,
     *                OMXXX,OMXXY,OMXXZ,OMXYY,OMYYY,
     *                OMYYZ,OMXZZ,OMYZZ,OMZZZ,OMXYZ
C
      DIMENSION DEIG(NUM),ENGYST(MXRTTRN,NUMCI),ISTSYM(MXRTTRN,*),
     *          NWKSST(NUMCI),MULST(NUMCI),IROOTS(NUMCI),
     *          IVCORB(NUM,NUMCI),IRRR(3)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA ELMOM/8HELMOM   /
#else
      CHARACTER*8 :: ELMOM_STR
      EQUIVALENCE (ELMOM, ELMOM_STR)
      DATA ELMOM_STR/"ELMOM   "/
#endif
C
C     ----- MAIN DRIVER FOR TRANSITION MOMENTS ------
C
C     THIS MODULE WAS WRITTEN BY
C        DR. SHIRO KOSEKI
C        DEPARTMENT OF CHEMISTRY
C        NORTH DAKOTA STATE UNIVERSITY
C        FARGO N.D. 58105
C     DURING FEBRUARY AND MARCH OF 1986.
C
C        THE T.M. MAY BE OBTAINED BETWEEN ANY TWO
C        CI STATES GAMESS CAN COMPUTE, USING A COMMON SET OF MO-S.
C
C        THE T.M. MAY ALSO BE OBTAINED FOR TWO
C        CI STATES, COMPUTED IN DIFFERENT ORBITALS, IF 1) THE
C        TWO STATES SHARE A COMMON SET OF FROZEN CORE ORBITALS
C        AND 2) IF A COMPLETE ACTIVE SPACE CI IS BEING USED.
C        A REFERENCE FOR THE LATTER OPTION IS
C           B.H.LENGSFIELD III, J.J.JAFRI, D.H.PHILLIPS,
C           C.W.BAUSCHLICHER, JR.  J.CHEM.PHYS. 74,6849-6856(1981)
C
C     DEBUG FOR PARTICULAR ROUTINES IS AVAILABLE BY SETTING
C          EXETYP=TRNCMO  (CORRESPONDING ORBITALS)
C                =SAVCIV  (PRESERVING CSF DATA)
C                =DIPVEL  (DIPOLE VELOCITY INTEGRALS)
C                =TRNMOM  (COMPUTATION OF THE T.M.)
C
C     ----- COMPUTE DIPOLE INTEGRALS -----
C
      L1 = NUM
      L2 = (L1*L1+L1)/2
C     L3 = L1*L1
      CALL VALFM(LOADFM)
      LX   = LOADFM + 1
      LY   = LX     + L2
      LZ   = LY     + L2
      LMW  = LZ     + L2
C
                    LAST = LMW + 36*3
      IF(MAXL.EQ.3) LAST = LMW + 100*3
      IF(MAXL.EQ.4) LAST = LMW + 225*3
C
      NEED = LAST - LOADFM
      CALL GETFM(NEED)
      CALL CALCOM(XP,YP,ZP)
      CALL PRCALC(ELMOM,X(LX),X(LMW),3,L2)
      CALL DAWRIT(IDAF,IODA,X(LX),L2,95,0)
      CALL DAWRIT(IDAF,IODA,X(LY),L2,96,0)
      CALL DAWRIT(IDAF,IODA,X(LZ),L2,97,0)
      CALL RETFM(NEED)
      IF (MASWRK) WRITE(IW,9100)
      CALL TIMIT(1)
C
C     ----- COMPUTE DIPOLE VELOCITY INTEGRALS -----
C
      CALL VALFM(LOADFM)
      LDDX = LOADFM + 1
      LDDY = LDDX   + L2
      LDDZ = LDDY   + L2
      LAST = LDDZ   + L2
      NEED = LAST - LOADFM
      CALL GETFM(NEED)
      CALL DIPVEL(EXETYP,X(LDDX),X(LDDY),X(LDDZ),L1,L2)
      CALL DAWRIT(IDAF,IODA,X(LDDX),L2,84,0)
      CALL DAWRIT(IDAF,IODA,X(LDDY),L2,85,0)
      CALL DAWRIT(IDAF,IODA,X(LDDZ),L2,86,0)
      CALL RETFM(NEED)
      IF (MASWRK) WRITE(IW,9110)
      CALL TIMIT(1)
C
C     ----- COMPUTE THE TRANSITION MOMENT -----
C
      CALL DIPMOM(NFT17,NROWS,NWKSST(1),EXETYP,ENGYST,NWKSST,MULST,
     *            IROOTS,IVCORB,DEIG,ISTSYM,IRRR,NOSYM,IPRHSO)
      RETURN
C
 9100 FORMAT(/1X,'...... DONE WITH DIPOLE INTEGRALS ......')
 9110 FORMAT(/1X,'...... DONE WITH DIPOLE VELOCITY INTEGRALS ......')
      END
C
C*MODULE TRNSTN  *DECK TRNAO
      SUBROUTINE TRNAO(NORBS,L1,L2,N1,N2,IVCORB,TDM,WRK1,WRK2,
     *                 VST1,VST2,DAOX,DAOY,DAOZ,DMOX,DMOY,DMOZ,DBG)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXAO=2047)
C
      LOGICAL DBG,GOPARR,DSKWRK,MASWRK
C
      DIMENSION IVCORB(*),TDM(N2),WRK1(NORBS,NORBS),
     *          WRK2(NORBS,L1),VST1(L1,N1),VST2(L1,N1),
     *          DAOX(L2),DAOY(L2),DAOZ(L2),
     *          DMOX(N1,N1),DMOY(N1,N1),DMOZ(N1,N1)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (ZERO=0.0D+00, HALF=0.5D+00, DEBYE=2.541766D+00)
C
C     ----- CALCULATE TRANSITION DENSITY MATRIX IN AO BASIS -----
C           CODED BY HENGTAI YU, DEPARTMENT OF CHEMISTRY,
C                    UNIVERSITY OF SHERBROOKE, CANADA.
C
C     NORBS - ORBITALS IN CI CALCULATION, NOT INCLUDING FROZEN CORE
C     L1 -- TOTAL NUMBER OF ATOMIC ORBITALS
C     N1 -- TOTAL NUMBER OF OCCUPIED ORBITALS
C     IVCORB -- PERMUTATION MATRIX TO CORRESPONDING MO ORDER
C     TDM -- TRANSITION DENSITY IN MO BASIS
C     VST1,VST2 -- CORRESPONDING MOS
C     DAOX,DAOY,DAOZ -- AO DIPOLE INTEGRALS
C     DMOX,DMOY,DMOZ -- MO DIPOLE INTEGRALS
C
C     VERIFY TRANSITION MOMENT USING TDM IN MO BASIS
C
      IF(MASWRK  .AND.  DBG) THEN
         TDX = ZERO
         TDY = ZERO
         TDZ = ZERO
         IJ = 0
         DO 110 I=1,NORBS
            DO 100 J=1,I
               IJ = IJ + 1
               II = IVCORB(I)
               JJ = IVCORB(J)
               TDX = TDX + TDM(IJ)*DMOX(II,JJ)
               TDY = TDY + TDM(IJ)*DMOY(II,JJ)
               TDZ = TDZ + TDM(IJ)*DMOZ(II,JJ)
  100       CONTINUE
  110    CONTINUE
         TDX=TDX*DEBYE
         TDY=TDY*DEBYE
         TDZ=TDZ*DEBYE
         TDP = SQRT(TDX*TDX+TDY*TDY+TDZ*TDZ)
         WRITE(IW,9000)
         WRITE(IW,9020) TDX,TDY,TDZ,TDP
      END IF
C
C     FORM CORRECT TDM MATRIX IN MO BASIS
C
      NORBS2 = (NORBS*NORBS+NORBS)/2
      CALL DSCAL(NORBS2,HALF,TDM,1)
      DO 170 I = 1, NORBS
         II = IA(I) + I
         TDM(II) = TDM(II) + TDM(II)
  170 CONTINUE
C
      IF(MASWRK  .AND.  DBG) THEN
         WRITE(IW,9030)
         CALL PRTRI(TDM,NORBS)
      END IF
C
C     PERMUTE TDM INTO CORRESPONDING ORBITAL ORDER
C
      NFZC = N1-NORBS
      DO 210 I=1,NORBS
         DO 200 J=1,NORBS
            II = IVCORB(I)-NFZC
            JJ = IVCORB(J)-NFZC
            IJ =  IA(J)+I
            IF(J.LT.I) IJ = IA(I)+J
            WRK1(II,JJ) = TDM(IJ)
  200    CONTINUE
  210 CONTINUE
C
C     TRANSFORM TDM IN CORRESPONDING MO BASIS TO AO BASIS
C     BY TDM_AO = VST1 * TDM_MO * VST2-DAGGER
C
      CALL MRARTR(WRK1,NORBS,NORBS,NORBS,
     *            VST2(1,NFZC+1),L1,L1,WRK2,NORBS)
      IJ = 0
      DO 460 I=1,L1
         DO 450 J=1,I
            IJ = IJ + 1
            SUM= ZERO
            DO 420 K=1,NORBS
               SUM = SUM + VST1(I,K+NFZC)*WRK2(K,J)
 420        CONTINUE
            TDM(IJ) = SUM
 450     CONTINUE
 460  CONTINUE
C
      CALL DAWRIT(IDAF,IODA,TDM,L2,250,0)
C
      IF(MASWRK  .AND.  DBG) THEN
         WRITE(IW,9010)
         TDX=TRACEP(TDM,DAOX,L1)*DEBYE
         TDY=TRACEP(TDM,DAOY,L1)*DEBYE
         TDZ=TRACEP(TDM,DAOZ,L1)*DEBYE
         TDP = SQRT(TDX*TDX+TDY*TDY+TDZ*TDZ)
         WRITE(IW,9020) TDX,TDY,TDZ,TDP
         WRITE(IW,9040)
         CALL PRTRI(TDM,L1)
      END IF
C
 9000 FORMAT(/5X,'TRANSITION DIPOLE FROM TDM IN MO BASIS'/
     *        5X,'--------------------------------------')
 9010 FORMAT(/5X,'TRANSITION DIPOLE FROM TDM IN AO BASIS'/
     *        5X,'--------------------------------------')
 9020 FORMAT(1X,'TDX=',F12.7,' TDY=',F12.7,' TDZ=',F12.7,
     *          '   TDM=',F12.7,' DEBYE')
 9030 FORMAT(/10X,"TRANSITION DENSITY MATRIX IN MO BASIS"/10X,37("-"))
 9040 FORMAT(/10X,"TRANSITION DENSITY MATRIX IN AO BASIS"/10X,37("-"))
      RETURN
      END
C
C*MODULE TRNSTN  *DECK CITROX
      SUBROUTINE CITROX(NFT17,L1,NOSYM,DEIG,ENGYST,EORB,EREF0,EREFCAS,
     *                  NWKSST,MULST,IROOTS,IVEX,NSTATES,IVCORB,DEVE,
     *                  NROWS,CSMALL,ISODIAG)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, MXRT=100, ONE=1.0D+00)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,SVDSKW
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /ENRGYS/ ENUCR,EELCT,ETOT,SZ,SZZ,ECORE,ESCF,EERD,E1,E2,
     *                VEN,VEE,EPOT,EKIN,ESTATE(MXRT),STATN
      COMMON /FMCOM / X(1)
      COMMON /GUGWFN/ NFZC1,NMCC,NDOC,NAOS,NBOS,NALP,NVAL,NEXT,NFZV,
     *                IFORS,IEXCIT,ICICI,NOIRR
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RESTAR/ TIMLIM,IREST,NREC,INTLOC,IST,JST,KST,LST
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /TMVALS/ TI,TX,TIM
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,
     *                MXRTTRN,NSTAT
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      DIMENSION DEIG(L1),ENGYST(MXRTTRN,NUMCI),EREF0(MXRTTRN,NUMCI),
     *          EREFCAS(MXRTTRN,NUMCI),DEVE(MXRTTRN,NUMCI),
     *          NWKSST(NUMCI),MULST(NUMCI), IROOTS(NUMCI),IVEX(NUMCI),
     *          NSTATES(NUMCI),IVCORB(L1,NUMCI),EORB(L1)
C
      CHARACTER*8 CDRT
      CHARACTER*2 ANG
      EQUIVALENCE (CDRT,DRTNAM)
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA BREIT/8HHSO2FF  /,DIPMOM/8HDM      /,ZEFF/8HHSOZEFF /
#else
      CHARACTER*8 :: BREIT_STR
      EQUIVALENCE (BREIT, BREIT_STR)
      CHARACTER*8 :: DIPMOM_STR
      EQUIVALENCE (DIPMOM, DIPMOM_STR)
      CHARACTER*8 :: ZEFF_STR
      EQUIVALENCE (ZEFF, ZEFF_STR)
      DATA BREIT_STR/"HSO2FF  "/,DIPMOM_STR/"DM      "/,
     *     ZEFF_STR/"HSOZEFF "/
#endif
C
C     the following pertains only to PB
      MAXBIT=64/NWDVAR
      MAXBI2=MAXBIT/2
      MAXCSF=2**MAXBI2-1
      MAXNAO=(MAXBIT-12)/2
C     12 bits/word are used elsefore, it is connected to maxcp and mxprm
C
C     ----- GROW MEMORY FOR ORBITAL SELECTION -----
C
      L3=L1*L1
      CALL VALFM(LOADFM)
      LVST1 = LOADFM+1
      LVST2 = LVST1 + L3
      LAST  = LVST2 + L3
      NEED  = LAST-LOADFM-1
      CALL GETFM(NEED)
      IF(NPRINT.GT.7) THEN
         NPRDRT=1
      ELSE
         NPRDRT=0
      ENDIF
      SVDSKW = DSKWRK
      IF(GOPARR) DSKWRK = .TRUE.
      CALL SEQOPN(NFT17,'CSFSAVE','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQREW(NFT17)
      NFTCSF=NFT17
C
C     ----- CARRY OUT A CI for each spin multiplicity -----
C
      DO 150 ICI=1,NUMCI
         ANG='TH'
         IF(ICI.EQ.1) ANG='ST'
         IF(ICI.EQ.2) ANG='ND'
         IF(ICI.EQ.3) ANG='RD'
         IF(IVEX(ICI).EQ.2) THEN
C
C              USE 2ND SET OF ORBITALS FOR THIS CI
C
            CALL DAREAD(IDAF,IODA,X(LVST1),L3,15,0)
            CALL DAREAD(IDAF,IODA,X(LVST2),L3,19,0)
            CALL DAWRIT(IDAF,IODA,X(LVST1),L3,19,0)
            CALL DAWRIT(IDAF,IODA,X(LVST2),L3,15,0)
         ENDIF
         ICIOUT=ICI
         IF(OPERR.EQ.DIPMOM) ICIOUT=1
         IF(ICIOUT.LE.9) THEN
            WRITE(UNIT=CDRT(1:8),FMT='(A3,I1)') 'DRT',ICIOUT
         ELSE
            WRITE(UNIT=CDRT(1:8),FMT='(A3,I2)') 'DRT',ICIOUT
         ENDIF
C
         IF(MASWRK) WRITE(IW,9080) ICI,ANG,'CI'
C
         NSTAT=1
         CALL DRTGEN(NPRDRT,DRTNAM)
         IF(MPLEVL.NE.0.AND.MASWRK.AND.MULST(ICI).NE.MUL) THEN
            IF(MASWRK) WRITE(IW,9210)
            CALL ABRT
         ENDIF
         MULST(ICI) = MUL
         IF(NSTAT.LT.0.AND.NOSYM.EQ.0) THEN
            IF(MASWRK) WRITE(IW,9300)
            NOSYM=1
         ENDIF
         IF(ICI.NE.1) THEN
            IF(MULST(ICI).LT.MULST(ICI-1)) THEN
               IF(MASWRK) WRITE(IW,9220)
               CALL ABRT
            ENDIF
         ENDIF
         IF(NFZC1.NE.NFZC) THEN
           WRITE(IW,9305) ICI,NFZC,NFZC1
           CALL ABRT
         ENDIF
C
C        check number of orbitals:
C
         CALL SEQREW(NFT11)
         READ(NFT11) NCHK1,NCHK2,NSYM1
         IF(NCHK1.NE.NOCC) THEN
            IF (MASWRK) WRITE(IW,9085) NOCC,NCHK1,NCHK2,NSYM1
            CALL ABRT
            ENDIF
         NAO = NDOC + NAOS + NBOS + NALP + NVAL + NEXT
C        drtind sets nstat to nwks
         NSTAT=ABS(NSTAT)
         IF(OPERR.EQ.BREIT.AND.(NAO.GT.MAXNAO.OR.NSTAT.GT.MAXCSF)) THEN
            IF(MASWRK) WRITE(IW,9170) MAXNAO,MAXCSF,MAXBIT,NAO,NSTAT
            CALL ABRT
         END IF
         IF(TIM.GT.TIMLIM) GO TO 800
C
         CALL TRFMCX(0,0,0,0,.FALSE.,.TRUE.,
     *               .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.)
         IF(TIM.GT.TIMLIM) GO TO 800
C
         CALL GUGSRT(0,.FALSE.)
         IF(TIM.GT.TIMLIM) GO TO 800
C
         CALL GUGAEM(0)
         IF(TIM.GT.TIMLIM) GO TO 800
C
C     FOR SPIN-ORBIT, THE CSF'S ARE CHANGED AND SO THE OLD CI
C     VECTOR FILE MUST BE DESTROYED, NOT USED AS THE GUESS.
C
         CALL SEQREW(NFT12)
         WRITE(NFT12) 0,0
         CALL SEQREW(NFT12)
         NSTAT=MAX(IROOTS(ICI),NSTATES(ICI))
         CALL GUGADG(0)
C        STORE ENERGY DATA:
         IF(ISODIAG.EQ.0.AND.MPLEVL.EQ.0)
     *      CALL DCOPY(IROOTS(ICI),ESTATE(1),1,ENGYST(1,ICI),1)
         IF(MPLEVL.NE.0.AND.MASWRK) THEN
           WRITE(IW,9400)
           WRITE(IW,9420) (ESTATE(I),I=1,IROOTS(ICI))
           WRITE(IW,9410)
           WRITE(IW,9420) (EREFCAS(I,ICI),I=1,IROOTS(ICI))
           CALL DCOPY(IROOTS(ICI),EREFCAS(1,ICI),1,DEVE(1,ICI),1)
           CALL DAXPY(IROOTS(ICI),-ONE,ESTATE,1,DEVE(1,ICI),1)
C          this computes deviations between CAS and MCQDPT
         ENDIF
         IF(TIM.GT.TIMLIM) GO TO 800
C
C     ----- STORE RESULTS ON A SPECIAL FILE -----
C
         IF(OPERR.EQ.BREIT) THEN
            CALL SAVCIC(NFT17,NWKSST,IROOTS)
         ELSE
            IF(OPERR.EQ.ZEFF) WRITE(NFT17) DRTNAM
            CALL SAVCIM(NFTCSF,NWKSST,IROOTS,DEIG,ENGYST,IVCORB,EORB,
     *                  EREF0,NROWS,CSMALL,NOSYM,MPLEVL)
            IF(OPERR.EQ.DIPMOM) NFTCSF=NFT12
         ENDIF
C
C              PUT THE TWO SETS OF VECTORS BACK WHERE THEY BELONG
C
         IF(IVEX(ICI).EQ.2) THEN
            CALL DAREAD(IDAF,IODA,X(LVST1),L3,15,0)
            CALL DAREAD(IDAF,IODA,X(LVST2),L3,19,0)
            CALL DAWRIT(IDAF,IODA,X(LVST1),L3,19,0)
            CALL DAWRIT(IDAF,IODA,X(LVST2),L3,15,0)
         ENDIF
  150 CONTINUE
      IF(MPLEVL.NE.0.AND.MASWRK) THEN
         WRITE(IW,9500)
         DO ICI=1,NUMCI
           WRITE(IW,*) 'CI RUN ',ICI
           WRITE(IW,9510) (DEVE(I,ICI),I=1,IROOTS(ICI))
         ENDDO
      ENDIF
      CALL RETFM(NEED)
      IF(NOSYM.EQ.-1) NOSYM=0
      DSKWRK = SVDSKW
C
      CALL TIMIT(1)
      RETURN
C
  800 CONTINUE
      IF (MASWRK) WRITE(IW,9130) RUNTYP
      CALL ABRT
C
      RETURN
C
 9080 FORMAT(//5X,25("-")/5X,I2,'-',A2,A7,' CALCULATION',/5X,25("-"))
 9085 FORMAT(/1X,'ERROR: CHECK THE NUMBER OF ACTIVE ORBITALS...',
     *       /30X,'NOCC=',I5,'     NFT11:',2I5,' NSYM',I5)
 9130 FORMAT(1X,'TIME USED EXCEEDS INPUT TIMLIM,'/
     *       1X,'SOME RESTART FOR RUNTYP=',A8,' IS IMPLEMENTED')
 9170 FORMAT(/1X,' AT MOST ',I2,' ACTIVE ORBITALS AND',I10,
     *       ' CSFS ARE ALLOWED FOR ',/1X,I2,' BITS AN INTEGER WHEREAS',
     *       ' YOU ',/1X,'HAVE GOT ',I2,' ACTIVE ORBITALS AND',I10,
     *       ' CSFS',/)
 9210 FORMAT(/1X,'MULTIPLICITIES DIFFER IN $MCQD AND $DRT.')
 9220 FORMAT(/1X,'MULTIPLICITIES MUST BE GIVEN IN INCREASING ORDER.')
 9300 FORMAT(/1X,'GUGA REPORTED MO IRREP CONTAMINATION. NO SPACE',
     *           ' SYMMETRY WILL BE USED.'/
     *        1X,'SET NOSYM TO -1 TO OVERRIDE THIS',/)
 9305 FORMAT(//1X,'*** ERROR ***'/1X,'RUNNING CI COMPUTATION NUMBER',I4/
     *       1X,'NFZC=',I5,' IN $TRNSTN GROUP, BUT'/
     *       1X,'NFZC=',I5,' IN THE $DRT GROUP. THESE MUST MATCH.')
 9400 FORMAT(/1X,'CAS STATE ENERGIES ($DRT):')
 9410 FORMAT(/1X,'CAS STATE ENERGIES ($MCQDPT):')
 9420 FORMAT(1X,5F15.7)
 9500 FORMAT(/1X,'DEVIATIONS BETWEEN CAS ENERGIES IN $DRT AND $MCQDPT')
 9510 FORMAT(1X,5F15.9)
C9520 format(1x,'Warning!! The deviation is large.')
      END
C*MODULE TRNSTN  *DECK TRNMOMX
      SUBROUTINE TRNMOMX
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C         this is the only place where mxspin is needed
C
      PARAMETER (MXATM=500, MXSPIN=256, MXRT=100)
      COMMON /FMCOM / X(1)
C
      MAXATM = MXATM
      MAXRT  = MXRT
      CALL VALFM(LOADFM)
      LIROOTS  = LOADFM   + 1
      LNSTATES = LIROOTS  + MXSPIN
      LIVEX    = LNSTATES + MXSPIN
      LENGYST  = LIVEX    + MXSPIN
      LL2VAL   = LENGYST  + MXSPIN*MAXRT
      LZEFF    = LL2VAL   + MXSPIN
      LAST     = LZEFF    + MAXATM
      NEED = LAST - LOADFM - 1
      CALL GETFM(NEED)
      CALL TRNMOMXX(MXSPIN,MAXATM,MAXRT,X(LIROOTS),X(LNSTATES),X(LIVEX),
     *              X(LENGYST),X(LZEFF),X(LL2VAL))
      CALL RETFM(NEED)
      RETURN
      END
C*MODULE TRNSTN  *DECK TRNMOMXX
      SUBROUTINE TRNMOMXX(MXSPIN,MAXATM,MAXRT,IROOTS,NSTATES,IVEX,
     *                    ENGYST,ZEFF,L2VAL)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, MAXCP=4096, MXAO=2047, MXRT=100)
C
      DIMENSION IROOTS(MXSPIN),NSTATES(MXSPIN),IVEX(MXSPIN),
     *          ENGYST(MAXRT,MXSPIN),L2VAL(MXSPIN),ZEFF(MAXATM)
      DIMENSION IRRR(3),IRRL(3),PISFSO(100)
C
      LOGICAL PRTCMO,GOPARR,DSKWRK,MASWRK,PRTPRM,ADD2E,TMOMNT,SAVDSK,
     *        SVDSKW,SKIPDM,SLOWFF,PHYSRC,LINEAR,GSYLYES(3),ORBIS,
     *        UNCON,SVDMAS
C
      CHARACTER*3 WORDS(-1:3)
C
      COMMON /ENRGYS/ ENUCR,EELCT,ETOT,SZ,SZZ,ECORE,ESCF,EERD,E1,E2,
     *                VEN,VEE,EPOT,EKIN,ESTATE(MXRT),STATN
      COMMON /FMCOM / X(1)
      COMMON /GUGWFN/ NNFZC,NNMCC,NNDOC,NNAOS,NNBOS,NNALP,NNVAL,
     *                NNEXT,NNFZV,IFORS,IEXCIT,ICICI,NOIRR
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /INTOPT/ ISCHWZ,IECP,NECP,IEFLD
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /ORBSET/ NORBMX,NORBS,NCORBS,NLEVS,MA,MB,NC,NSYM,MSYM,
     *                IDOCC,IVAL,IMCC,ISYM(MXAO),ICODE(MXAO),
     *                NLCS(MXAO),LEVPT(MXAO),LEVNR(MXAO),IOUT(MXAO),
     *                NREFS,IEXCT,NFOCI,INTACT
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RELWFN/ RMETHOD,QRQMT,CLIG,CLIG2,QRTOL,IQRORD,MODQR,NESOC,
     *                NRATOM,NUMU,NQMTR,NQRDAF,MORDA,NDARELB
      COMMON /RESTAR/ TIMLIM,IREST,NREC,INTLOC,IST,JST,KST,LST
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /SOGUG/  CP(MAXCP),NUNIQ,LIOBP,ISODAF,NSODA,LSTREC,JSODAF,
     *                JSODA,NRECJ
      COMMON /TMVALS/ TI,TX,TIM
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,
     *                MXRTTRN,NSTAT
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
      COMMON /ZMAT  / NZMAT,NZVAR,NVAR,NSYMC,LINEAR
C
      PARAMETER (ONE=1.0D+00,ZERO=0.0D+00)
C
      PARAMETER (NNAM=41)
      DIMENSION QNAM(NNAM), KQNAM(NNAM)
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QNAM/8HNFZC    ,8HNUMVEC  ,8HNUMCI   ,8HNOCC    ,8HIROOTS  ,
     *          8HPRTCMO  ,8HZEFF    ,8HTOLZ    ,8HTOLE    ,8HPRTPRM  ,
     *          8HSYMTOL  ,8HTMOMNT  ,8HRSTATE  ,8HZEFTYP  ,8HNSTATE  ,
     *          8HETOL    ,8HOPERAT  ,8HADD2E   ,8HSAVDSK  ,8HPARMP   ,
     *          8HACTION  ,8HIVEX    ,8HRECLEN  ,8HNOSYM   ,8HIPRHSO  ,
     *          8HSKIPDM  ,8HMINMO   ,8HSLOWFF  ,8HPHYSRC  ,8HMODPAR  ,
     *          8HJZ      ,8HENGYST  ,8HMCP2E   ,8HONECNT  ,8HNISFSO  ,
     *          8HPISFSO  ,8HIREST   ,8HLVAL    ,8HHSOTOL  ,8HEEQTOL  ,
     *          8HNFFBUF  /
#else
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"NFZC    ","NUMVEC  ","NUMCI   ","NOCC    ",
     *          "IROOTS  ",
     *          "PRTCMO  ","ZEFF    ","TOLZ    ","TOLE    ","PRTPRM  ",
     *          "SYMTOL  ","TMOMNT  ","RSTATE  ","ZEFTYP  ","NSTATE  ",
     *          "ETOL    ","OPERAT  ","ADD2E   ","SAVDSK  ","PARMP   ",
     *          "ACTION  ","IVEX    ","RECLEN  ","NOSYM   ","IPRHSO  ",
     *          "SKIPDM  ","MINMO   ","SLOWFF  ","PHYSRC  ","MODPAR  ",
     *          "JZ      ","ENGYST  ","MCP2E   ","ONECNT  ","NISFSO  ",
     *          "PISFSO  ","IREST   ","LVAL    ","HSOTOL  ","EEQTOL  ",
     *          "NFFBUF  "/
#endif
      DATA KQNAM/1,1,1,1,-1,0,-3,3,3,0,3,0,1,5,-1,3,5,0,0,1,1,-1,1,1,1,
     *           0,1,0,0,1,1,-3,1,1,1,-3,1,-1,3,3,1/
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA GUGA/8HGUGA    /,USUAL /8HNORMAL  /,DOREAD/8HREAD    /,
     *     DOSAVE/8HSAVE    /,
     *     DIPMOM/8HDM      /,RESC,ANESC/8HRESC    ,8HNESC    /,
     *     TRANST/8HTRANST  /,ZEF/8HHSOZEFF /,  BREIT/8HHSO2FF  /,
     *     ZEFF2/8HHSO2    /,ZEFF3/8HHSO2P   /,ZTRUE/8HTRUE    /,
     *     CHECK/8HCHECK   /,ZEFF1/8HHSO1    /,ANONE/8HNONE    /
#else
      CHARACTER*8 :: GUGA_STR
      EQUIVALENCE (GUGA, GUGA_STR)
      CHARACTER*8 :: USUAL_STR
      EQUIVALENCE (USUAL, USUAL_STR)
      CHARACTER*8 :: DOREAD_STR
      EQUIVALENCE (DOREAD, DOREAD_STR)
      CHARACTER*8 :: DOSAVE_STR
      EQUIVALENCE (DOSAVE, DOSAVE_STR)
      CHARACTER*8 :: DIPMOM_STR
      EQUIVALENCE (DIPMOM, DIPMOM_STR)
      CHARACTER*8 :: ANESC_STR
      EQUIVALENCE (ANESC, ANESC_STR)
      CHARACTER*8 :: TRANST_STR
      EQUIVALENCE (TRANST, TRANST_STR)
      CHARACTER*8 :: ZEF_STR
      EQUIVALENCE (ZEF, ZEF_STR)
      CHARACTER*8 :: BREIT_STR
      EQUIVALENCE (BREIT, BREIT_STR)
      CHARACTER*8 :: ZEFF2_STR
      EQUIVALENCE (ZEFF2, ZEFF2_STR)
      CHARACTER*8 :: ZEFF3_STR
      EQUIVALENCE (ZEFF3, ZEFF3_STR)
      CHARACTER*8 :: ZTRUE_STR
      EQUIVALENCE (ZTRUE, ZTRUE_STR)
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      CHARACTER*8 :: ZEFF1_STR
      EQUIVALENCE (ZEFF1, ZEFF1_STR)
      CHARACTER*8 :: ANONE_STR
      EQUIVALENCE (ANONE, ANONE_STR)
      CHARACTER*8 :: RESC_STR
      EQUIVALENCE (RESC, RESC_STR)
      DATA GUGA_STR/"GUGA    "/,USUAL_STR/"NORMAL  "/,
     *     DOREAD_STR/"READ    "/,
     *     DOSAVE_STR/"SAVE    "/,
     *     DIPMOM_STR/"DM      "/,
     *     RESC_STR,ANESC_STR/"RESC    ","NESC    "/,
     *     TRANST_STR/"TRANST  "/,ZEF_STR/"HSOZEFF "/,  
     *     BREIT_STR/"HSO2FF  "/, ZEFF2_STR/"HSO2    "/,
     *     ZEFF3_STR/"HSO2P   "/,ZTRUE_STR/"TRUE    "/, 
     *     CHECK_STR/"CHECK   "/,ZEFF1_STR/"HSO1    "/,
     *     ANONE_STR/"NONE    "/
#endif
      DATA NMOFZC/0/, NMODOC/0/, NMOACT/0/, NMOEXT/0/
      DATA WORDS/'1E','NO','1E','2E','P2E'/
C
      IF (MASWRK) WRITE(IW,9010)
C
      IF(MPLEVL.EQ.0  .AND.  CITYP.NE.GUGA) THEN
         IF (MASWRK) WRITE(IW,9030)
         CALL ABRT
      END IF
      UNCON=RMETHOD.NE.ANONE.AND.MOD(MODQR,2).EQ.1
C
C     ----- READ THE INPUT CONTROLLING THIS RUN -----
C
      KQNAM(5) = 10*MXSPIN + 1
      KQNAM(7) = 10*MXATM  + 3
      KQNAM(15)= 10*MXSPIN + 1
      KQNAM(22)= 10*MXSPIN + 1
      KQNAM(32)= 10*MXRT*MXSPIN + 3
      KQNAM(36)= 10*100 + 3
      KQNAM(38)= 10*MXSPIN + 1
      NFZC   = NUM
      NOCC   = NUM
C             numvec=-1 means take the orbitals from first MCQDPT run
      NUMVEC = 1
      IF(MPLEVL.NE.0) NUMVEC=-1
C             default for NUMCI will be set later
      NUMCI  = 0
      IGAMMA = 0
C     0 means use the default
      DO 112 I=1,MXSPIN
         IROOTS(I) = 1
         NSTATES(I) = 1
         IVEX(I)=1
C        if(mplevl.ne.0) ivex(i)=i
         L2VAL(I)=-1
  112    CONTINUE
C     l2val: eigenvalues of L2 operator, L: L^2 == L(L+1)
C     these values if assigned for each $DRT group can be used in
C     atomic calculations as the following selection rule applies:
C     |L-L'|<=1, where L and L' are L-values of bra and ket.
C     It is the sole resposibility of the user to use this rule.
C     Default: -1, do not use this property.
      IVEX(1)=1
      IF(MPLEVL.EQ.0) IVEX(2)=2
      PRTCMO = .FALSE.
      ZEFTYP = ZTRUE
      TOLZ=1.0D-08
      TOLE=1.0D-06
      PRTPRM = .FALSE.
      SLOWFF = .FALSE.
      PHYSRC = .FALSE.
      SYMTOL = 1.0D-04
      HSOTOL = 1.0D-01
C     hsotol is in cm-1, it is symtol*Hso matrix element
C     used solely for statistics, now only printed if debugging
      TMOMNT = .FALSE.
      NZSPIN = 1001
      ETOL   = 1.0D+02
      OPERR  = DIPMOM
      SKIPDM = .TRUE.
      JZOPT  = 0
      IF(LINEAR) JZOPT=1
      EEQTOL=1.0D-06
C
C     Threshold to force two state energies as they appear in the energy
C     denominators (not CSF energies, only state energies) to be exactly
C     equal.
C     This is used to avoid computation of redundant 2-body diagrams,
C     mostly in atoms (or Oh/Ih; non-Abelian groups in C1 symmetry;
C     a certain type of intruder state avoidance technique).
C     semi-undocumented option to run several PARISF values in one batch
C
      NISFSO=0
      PISFSO(1)=ZERO
      MODPAR=3
C        Parallel modes
C        0 bit (the lowest) - if set, use dyn. load balancing if avail.
C        1 bit              - if set, distribute ints, dup otherwise
      IPRHSO = 0
C        Selector of the Hso matrix elements output style
C             -1    no single elements
C              0    k-(2S+1)Gi (MS=ms)  CI state labelling (term-like)
C              1    NST=j S=s MS=ms     CI state labelling (numbers)
C
C            NEXT PARAMETER IS AN UNDOCUMENTED DEBUGGING TOOL, AS THE
C            OMISSION OF THE 2E- TERM FROM operr=hso2ff SHOULD THEN
C            EXACTLY REPRODUCE THE operr=hso1 VALUES USING TRUE ZNUC.
      ADD2E=.TRUE.
      SAVDSK=.FALSE.
      NOSYM=0
C
C     -2 means all ms (ie 0 and 1)
C
      MSONLY=-2
C                  set nuclear charges to illegal value
      DO I=1,NAT
         ZEFF(I) = -1.0D+00
      END DO
C     buffer size,
C     nact=nocc-nfzc
C     nffbuf=nact**2 is the upper bound.
C     In most cases nffbuf=nact is enough.
C     0 means do not use FF driven algorithm
C     -1 value will be replaced below unless set by the user.
      NFFBUF=-1
C
C     mcp2e=0 do not add MCP 2e core-active contribution
C     mcp2e=1 add this contribution
C     mcp2e=2 add this and the usual valence only 2e contribution
      MCP2E=0
      IF(IECP.EQ.5) MCP2E=1
C
C     ionecnt=0 do not use one-centre approximation for SOC 2e integrals
C           = 1 compute one-centre 1e and one-centre 2e SOc integrals
C           = 2 compute all 1e and one-centre 2e integrals
      IONECNT=0
C
      ACTION=USUAL
C     can take three values:
C     normal - calculate FFs and SOC
C     save   - calculate FFs and SOC + save FFs to disk for reuse
C     read   - read FFs from disk and calculate SOC
C
      MINMO=1
C     the smallest number of MOs in 1 pass during 4-index transformation
C
      JREST=0
C
C     At present JREST=1 is only implemented for SO-MQCDPT.
C     How restart works:
C     Spin-free MCQDPT results needed for restart are saved to file 50,
C     so that consequent separate run can use them for SO-MCQDPT.
C     At present no check of data correctness between runs is done.
C     NB: in order for restart to work, the following files need to be
C     provided from the preceding run in the directory where the job
C     is running:
C     .F17 CSF info
C     .F40 CSF and CI state info
C     .F50 MCQDPT data
C     .F60 2e integrals in MO basis
C     Optionally .F31,.F32,.F33 can be given for SOC integral restart.
C
      ENGYST(1,1)=ONE
      JRET = 0
      CALL NAMEIO(IR,JRET,TRANST,NNAM,QNAM,KQNAM,
     *            NFZC,NUMVEC,NUMCI,NOCC,IROOTS,
     *            PRTCMO,ZEFF,TOLZ,TOLE,PRTPRM,
     *            SYMTOL,TMOMNT,NZSPIN,ZEFTYP,NSTATES,
     *            ETOL,OPERR,ADD2E,SAVDSK,MSONLY,
     *            ACTION,IVEX,IGAMMA,NOSYM,IPRHSO,
     *            SKIPDM,MINMO,SLOWFF,PHYSRC,MODPAR,
     *            JZOPT,ENGYST,MCP2E,IONECNT,NISFSO,
     *            PISFSO,JREST,L2VAL,HSOTOL,EEQTOL,NFFBUF,
     *            0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0)
C
      IF(JRET.GT.1) THEN
         IF (MASWRK) WRITE(IW,9040) TRANST
         CALL ABRT
      END IF
      IF(JRET.EQ.1) THEN
         IF (MASWRK) WRITE(IW,9050) TRANST
         CALL ABRT
      END IF
C
      IF(NUMCI.EQ.0) THEN
         IF(OPERR.EQ.DIPMOM) THEN
            NUMCI=1
         ELSE
            NUMCI=2
         END IF
      END IF
C
      OPSAVE = OPERR
      MAXNCO=1
      IF(OPERR.EQ.ZEFF1) THEN
         OPERR=ZEF
         MAXNCO=1
      ENDIF
      IF(OPERR.EQ.ZEFF2) THEN
         OPERR=ZEF
         MAXNCO=2
      ENDIF
      IF(OPERR.EQ.ZEFF3) THEN
         OPERR=ZEF
         MAXNCO=3
      ENDIF
      NOSYMTRZE=0
      IF(NESOC.LT.0) THEN
         NOSYMTRZE=1
         NESOC=-NESOC
      ENDIF
      IF(NESOC.GT.MAXNCO) NESOC=MAXNCO
      IF(MSONLY.EQ.-2.AND.OPERR.EQ.ZEF.AND.MPLEVL.EQ.0) MSONLY=0
      IF(MSONLY.EQ.-2.AND.MPLEVL.NE.0) MSONLY=3
      IF(OPERR.EQ.ZEF.AND.MSONLY.EQ.3.AND.MAXNCO.EQ.1) MSONLY=1
C     if(nesoc.eq.2.and.rmethod.eq.resc.and.maxnco.eq.2.and.numvec.eq.2)
C    * then
C        quid facturi sumus, Domine? non possimus hoc facere.
C        nosymtrze=1
C     END IF
C
      IF(RMETHOD.EQ.RESC.AND.MASWRK) WRITE(IW,9100) NESOC
C
      IF(RMETHOD.EQ.RESC.AND.NESOC.GT.0.AND.OPERR.NE.ZEF.AND.
     *   OPERR.NE.DIPMOM) THEN
         WRITE(IW,9105)
         CALL ABRT
      ENDIF
C
      IF(RMETHOD.EQ.ANESC .AND.  NESOC.GT.1) THEN
         IF(MASWRK) WRITE(IW,*)
     *         'ERROR: NESC 2E CORRECTIONS ARE NOT CODED YET'
         CALL ABRT
      END IF
      IF(OPERR.NE.ZEF.AND.MCP2E.NE.0) THEN
         WRITE(IW,9110)
         CALL ABRT
      ENDIF
      IF(RMETHOD.EQ.ANESC.AND.NESOC.GT.1.AND.MCP2E.NE.0) THEN
         WRITE(IW,9115)
         CALL ABRT
      ENDIF
      IF(MAXNCO.EQ.2.AND.MCP2E.EQ.1) THEN
         WRITE(IW,9120)
         CALL ABRT
      ENDIF
C
      IF(UNCON .AND.  NESOC.GT.1) THEN
         IF(MASWRK) WRITE(IW,9220)
         CALL ABRT
      END IF
      IF(MPLEVL.NE.0.AND.(ABS(NUMVEC).LT.1.OR.ABS(NUMVEC).GT.NUMCI))THEN
         IF(MASWRK) WRITE(IW,9055)
         CALL ABRT
      END IF
C
C     if true, orbitals are read not from $VEC1,2 but are taken from CAS
C
      ORBIS=.FALSE.
      IF(NUMVEC.LT.0.AND.MPLEVL.EQ.0) THEN
         ORBIS=.TRUE.
         NUMVEC=ABS(NUMVEC)
         IF(NUMVEC.NE.1) THEN
            IF(MASWRK) WRITE(IW,9230)
            CALL ABRT
         ENDIF
         IF(MASWRK) WRITE(IW,*) '$VEC WILL BE READ FROM DAF'
      ENDIF
      CALL BASCHK(MAXL)
      DO 115 I=1,NUMCI
         IF(NSTATES(I).LT.IROOTS(I)) NSTATES(I)=IROOTS(I)
         IF(NUMVEC.EQ.1.AND.MPLEVL.EQ.0) IVEX(I)=1
  115 CONTINUE
      IF(OPERR.NE.DIPMOM) CALL SETZEFF(ZEFTYP,ZEFF)
      LAMECSF=0
      IF(NOSYM.GT.0) LAMECSF=1
      IF(NOSYM.EQ.3) NOSYM=-1
      IF(MPLEVL.NE.0.AND.NFFBUF.LT.0) NFFBUF=3*(NOCC-NFZC)
      IF(MASWRK) THEN
         WRITE(IW,9250) OPSAVE,NFZC,NOCC,NUMCI,NUMVEC,SYMTOL,
     *                  IPRHSO,NOSYM,ETOL,TMOMNT,SKIPDM,PRTPRM,
     *                  SLOWFF,PHYSRC,MODPAR,JZOPT,MSONLY,ZEFTYP,
     *                  MCP2E,IONECNT,NFFBUF,EEQTOL
         IF(LINEAR)     WRITE(IW,9256) (L2VAL(I),I=1,NUMCI)
         WRITE(IW,9251) (IROOTS(I),I=1,NUMCI)
         WRITE(IW,9252) (NSTATES(I),I=1,NUMCI)
         WRITE(IW,9254) (IVEX(I),I=1,NUMCI)
         IF(OPERR.NE.DIPMOM) WRITE(IW,9260) (ZEFF(I),I=1,NAT)
      ENDIF
      IF(ACTION.NE.USUAL.AND.ACTION.NE.DOREAD.AND.ACTION.NE.DOSAVE) THEN
         IF(MASWRK) WRITE(IW,9070) ACTION
         CALL ABRT
      ENDIF
      IF(OPERR.EQ.BREIT.AND.(MSONLY.GT.1.OR.MSONLY.LT.-2).OR.OPERR.EQ.
     *   ZEF.AND.(MSONLY.LT.-1.OR.MSONLY.GT.3.OR.MSONLY.EQ.2)) THEN
         IF(MASWRK) WRITE(IW,*) 'ILLEGAL MS CHOSEN',MSONLY
         CALL ABRT
      ENDIF
      IF(MPLEVL.EQ.0.AND.NUMVEC.NE.1.AND.NUMVEC.NE.2) THEN
         IF(MASWRK) WRITE(IW,9060)
         CALL ABRT
      ENDIF
      IF(NUMVEC.EQ.2  .AND.  (IFORS.EQ.0  .AND.  NNEXT.GT.0)) THEN
         IF (MASWRK) WRITE(IW,9045)
         CALL ABRT
         STOP
      END IF
      IF(TMOMNT) SKIPDM=.FALSE.
C
C     nrecj=11
      NRECJ=6
      MAXIVEX=NUMVEC
      IF(MPLEVL.NE.0) MAXIVEX=NUMCI
      MXRTTRN=IROOTS(1)
      DO 117 ICI=1,NUMCI
         IF(IROOTS(ICI).GT.MXRTTRN) MXRTTRN=IROOTS(ICI)
         IF(IVEX(ICI).GT.MAXIVEX.OR.IVEX(ICI).LT.1) THEN
             IF(MASWRK) WRITE(IW,9080)
             CALL ABRT
         ENDIF
         IF(MAX(IROOTS(ICI),NSTATES(ICI)).GT.MXRT) THEN
             IF(MASWRK) WRITE(IW,9090) MXRT
             CALL ABRT
         ENDIF
  117 CONTINUE
      NUMVECG=NUMVEC
      IF(MPLEVL.NE.0) NUMVECG=1
C     the second set of DEIG will contain ONE's (when needed)
      CALL VALFM(LOADFM)
      JSODA= LOADFM+1
      LDEIG= JSODA+(NUMCI*NRECJ-1)/NWDVAR+1
      LENGYST = LDEIG + NUM+(NOCC-NFZC)*(NUMVECG-1)
      LNWKSST = LENGYST + MXRTTRN*NUMCI
      LMULST  = LNWKSST + (NUMCI-1)/NWDVAR+1
      LIROOTS = LMULST  + (NUMCI-1)/NWDVAR+1
      LIVEX   = LIROOTS + (NUMCI-1)/NWDVAR+1
      LNSTATES= LIVEX   + (NUMCI-1)/NWDVAR+1
      IF(OPERR.EQ.BREIT) THEN
         LIOBP= LNSTATES+ (NUMCI-1)/NWDVAR+1
         LAST = LIOBP   + (2*NUMCI-1)/NWDVAR+1
C     2 comes from having to store ms=s,s-1
      ELSE
         LIVCORB = LNSTATES+ (NUMCI-1)/NWDVAR+1
         LAST = LIVCORB + (NUM*NUMCI-1)/NWDVAR+1
      ENDIF
      LEREF0=LAST
      IF(MPLEVL.NE.0) THEN
         LHMP2= LEREF0  + MXRTTRN*NUMCI
         LEREFCAS= LHMP2   + MXRTTRN*MXRTTRN*NUMCI
         LEORB= LEREFCAS+ MXRTTRN*NUMCI
         LDEVE= LEORB + NUM
         LAST = LDEVE + MXRTTRN*NUMCI
      ELSE
         LEREFCAS= LAST
         LEORB= LAST
         LDEVE= LAST
      ENDIF
      NEED  = LAST-LOADFM-1
      CALL GETFM(NEED)
C     if(maswrk) write(iw,9300) need
      CALL ICOPY(NUMCI,IROOTS,1,X(LIROOTS),1)
      CALL ICOPY(NUMCI,NSTATES,1,X(LNSTATES),1)
      CALL ICOPY(NUMCI,IVEX,1,X(LIVEX),1)
      IF(ENGYST(1,1).EQ.ONE) THEN
C            no sane energy can be equal to +1
C            normal run, CI energies are put on the diagonal
         ISODIAG=0
      ELSE
C        abnormal run, CI energies are supplied by the user
C        CI is performed to get the wavefunction
C        engyst is declared as MXRT
C        This comes from Univ. of Tokyo, one suggested use is
C        to put MCQDPT2 energies on the diagonal and use CAS
C        wavefunction to get SOC. comprehendent potentes.
C
         ISODIAG=1
         CALL DCOPY(MXRTTRN*NUMCI,ENGYST,1,X(LENGYST),1)
      ENDIF
C
C     ----- COMPUTE ONE ELECTRON INTEGRALS -----
C
      IF(IREST.LE.0) CALL ONEEI
C
C     ----- EVALUATE NUCLEAR REPULSION -----
C
      ENUCR = ENUC(NAT,ZAN,C)
C
      JSODAF=40
C
C     note that the logical record size is set to 0, this is currently
C     OK as it is not used in RAOPEN2 but maybe troublesome in future
C     the desired record size is not known ahead of time anyway
C
C     the contents of file 40: (for each spin multiplicity)
C      1. CSF info for SOFFAC Ms=S     multiplicity 1
C      2. CSF info for SOFFAC Ms=S-1                1
Cc     3. CSF info for SOFFAC Ms=S-2                1 not used now
C      4. Occupation numbers for Ms=S               1
C      5. irreducible representations for Ms=S      1
C         for each CSF (Abelian groups)
C      6. irreps for CI states                      1
C      7. CSF info for SOFFAC Ms=S     multiplicity 2
C     etc
C
C     NB: in case of MCQDPT restarts we do not save JRECLN and JRECST.
C     With the present implementation of RAOPEN2 it works fine, but
C     only for reading file 40 (that was generated in a separate run).
C     Any write attempt is expected to result in a crash or worse in
C     wrong run.  Note that appropriate IODA-like array is saved and
C     loaded anyway.
C
      SVDSKW=DSKWRK
      DSKWRK=.TRUE.
      CALL RAOPEN2(JSODAF,X(JSODA),0,NUMCI*NRECJ,0,0,NPRINT)
      DSKWRK=SVDSKW
      IF(TIM.GT.TIMLIM) GO TO 800
      NFT17 = 17
      NOSYMSAV=NOSYM
      L1=NUM
      CALL TRANSINI(PRTCMO,TOLZ,TOLE,L1,X(LDEIG),ORBIS)
C
      CSMALL=SYMTOL/(NOCC-NFZC)
      IF(MAXNCO.EQ.2) CSMALL=CSMALL/2
C     this csmall needs to be levelled with small in SOLOOP
C
      IF(MPLEVL.NE.0) THEN
         CALL VCLR(X(LEREF0),1,MXRTTRN*NUMCI)
         SVDSKW = DSKWRK
         IF(GOPARR) DSKWRK = .TRUE.
         CALL MPIVOX(NFT17,L1,NQMT,NOSYM,X(LEREFCAS),X(LHMP2),X(LEORB),
     *               X(LEREF0),X(LENGYST),X(LNWKSST),X(LMULST),
     *               X(LIROOTS),X(LIVEX),X(LNSTATES),X(LIVCORB),CSMALL,
     *               ISODIAG,NMOFZC,NMODOC,NMOACT,NMOEXT,IFORB,EDSHFT,
     *               JREST,JSODA,NUMCI*NRECJ)
         DSKWRK = SVDSKW
      ELSE
         EDSHFT=ZERO
      ENDIF
      IF(NISFSO.EQ.0) THEN
         NISFSO=1
         PISFSO(1)=EDSHFT
      ENDIF
      IF(MPLEVL.EQ.0.OR.MOD(MODPAR/16,2).EQ.1) THEN
         CALL VCLR(X(LEREF0),1,MXRTTRN*NUMCI)
      CALL CITROX(NFT17,L1,NOSYM,X(LDEIG),X(LENGYST),X(LEORB),X(LEREF0),
     *            X(LEREFCAS),X(LNWKSST),X(LMULST),X(LIROOTS),X(LIVEX),
     *            X(LNSTATES),X(LIVCORB),X(LDEVE),NROWS,CSMALL,ISODIAG)
      ENDIF
      IF(TIM.GT.TIMLIM) GO TO 800
      IF(EXETYP.NE.CHECK) CALL SETSOSYM(LSTSYM,LIRCIOR,NEEDSYM,X(LMULST)
     *                                 ,X(LIROOTS),IRRL,IRRR,GSYLYES)
      IF(NOSYM.GT.0) THEN
         GSYLYES(1)=.TRUE.
         GSYLYES(2)=.TRUE.
         GSYLYES(3)=.TRUE.
      ENDIF
      IF(NOSYMSAV.NE.NOSYM.AND.NOSYM.NE.0.AND.MASWRK)
     *   WRITE(IW,9500) NOSYM
C     dome data is lost for the last write to that file if not flushed??
C     FLSHBF is a nasty little bigot ignoring calls from slaves.
      SVDMAS=MASWRK
      IF(GOPARR) MASWRK=.TRUE.
      CALL FLSHBF(NFT17)
      MASWRK=SVDMAS
      SVDSKW = DSKWRK
      DSKWRK=.TRUE.
      IF(OPERR.EQ.DIPMOM) THEN
         IF (MASWRK) WRITE(IW,9013)
         CALL TRNSTX(NFT17,X(LDEIG),X(LENGYST),X(LNWKSST),X(LMULST),
     *               X(LIROOTS),X(LIVCORB),X(LSTSYM),MAXL,IRRR,NROWS,
     *               NOSYM,IPRHSO)
C
C        trnstx runs in parallel with all nodes doing the same thing
C        since the preceding CIs take much more time why bother?
C
      ELSE IF(OPERR.EQ.ZEF) THEN
         MP2SO=MSONLY
         IF(MPLEVL.EQ.0) MP2SO=0
         IF(MP2SO.EQ.0) NFFBUF=0
         IF (MASWRK) THEN
            IF(MPLEVL.EQ.0) THEN
               WRITE(IW,9015)
               IF(MAXNCO.EQ.1) WRITE(IW,9017)
               IF(MAXNCO.EQ.2) WRITE(IW,9018)
               IF(MAXNCO.EQ.3) WRITE(IW,9019)
            ELSE
               WRITE(IW,9025) MPLEVL
               WRITE(IW,9026) WORDS(MAXNCO),WORDS(MP2SO)
               IF(MP2SO.EQ.3)  WRITE(IW,9028) 'MP2 CORE'
               IF(MP2SO.EQ.-1) WRITE(IW,9028) 'CAS CORE'
            ENDIF
         ENDIF
         CALL SOZEFX(NFT17,PRTPRM,ADD2E,ZEFF,TMOMNT,SKIPDM,NZSPIN,MAXL,
     *              NFOCI,ETOL,X(LDEIG),X(LENGYST),X(LEREF0),X(LEREFCAS)
     *              ,X(LHMP2),X(LNWKSST),X(LMULST),X(LIROOTS),X(LIVCORB)
     *              ,X(LIVEX),X(LSTSYM),X(LIRCIOR),IRRL,IRRR,GSYLYES,
     *               NOSYM,MAXNCO,IPRHSO,HSOTOL,JZOPT,MODPAR,LAMECSF,
     *               MINMO,NOSYMTRZE,MP2SO,NMOFZC,NMODOC,NMOACT,NMOEXT,
     *               MCP2E,IONECNT,NISFSO,PISFSO,L2VAL,EEQTOL,NFFBUF)
      ELSE IF(OPERR.EQ.BREIT) THEN
         IF (MASWRK) WRITE(IW,9027)
         CALL SOBRTX(NFT17,PRTPRM,ADD2E,ZEFF,SAVDSK,MSONLY,ACTION,MAXL,
     *               NZSPIN,X(LDEIG),X(LENGYST),X(LNWKSST),X(LMULST),
     *               X(LIROOTS),X(LIVEX),X(LIOBP),X(LSTSYM),X(LIRCIOR),
     *               IRRL,GSYLYES,IGAMMA,NOSYM,SLOWFF,PHYSRC,IPRHSO,
     *               HSOTOL,MODPAR,IONECNT)
      ELSE
         IF(MASWRK) WRITE(IW,9000) OPERR
      ENDIF
C
      DSKWRK = SVDSKW
      CALL RETSOSYM(NEEDSYM)
      CALL SEQCLO(NFT17,'KEEP')
      CALL DDI_SYNC(353)
      CALL RACLOS(JSODAF,'KEEP')
  800 CONTINUE
      CALL RETFM(NEED)
      IF (MASWRK) WRITE(IW,9210)
      CALL TIMIT(1)
      RETURN
 9000 FORMAT(/1X,'UNKNOWN OPERAT=',A8)
 9010 FORMAT(/5X,24("-")/5X,'TRANSITION MOMENT DRIVER',/5X,24("-"))
 9013 FORMAT(/5X,27("-")/5X,'RADIATIVE TRANSITION MOMENT',
     *       /13X,'SHIRO KOSEKI',/5X,27(1H-))
 9015 FORMAT(/5X,43("-"),/12X,'DIRECT SPIN-ORBIT COUPLING',
     *       /9X,'(SHIRO KOSEKI AND DMITRI FEDOROV)')
 9017 FORMAT(/11X,'  ONE ELECTRON SO OPERATOR.',/5X,43("-")/)
 9018 FORMAT(/11X,'FULL PAULI-BREIT HAMILTONIAN.',/5X,43("-")/)
 9019 FORMAT(/11X,'FULL PAULI-BREIT HAMILTONIAN.',/5X,
     *       'ACTIVE-ACTIVE 2E CONTRIBUTION IS NEGLECTED.',/5X,43(1H-)/)
 9025 FORMAT(/9X,57("-"),/14X,'SPIN-ORBIT MULTI-CONFIGURATION QUASI-',
     *       'DEGENERATE',/10X,'PERTURBATION THEORY SO-MCQDPT',
     *       ' (AKA SO-MRMS) (',I1,'ND ORDER)',/29X,'(DMITRI FEDOROV)')
 9026 FORMAT(/12X,'0TH+1ST ORDER SO HAMILTONIAN: ',A3,
     *       ', 2ND ORDER: ',A3,/24X,'(P2E MEANS PARTIAL 2E)',
     *       /9X,57(1H-))
 9027 FORMAT(/5X,51("-"),
     *       /5X,'FORM FACTOR DRIVEN PAULI-BREIT SPIN-ORBIT COUPLING',
     *       /5X,'        (TOM FURLANI AND DMITRI FEDOROV)',
     *       /5X,51(1H-)/)
 9028 FORMAT(/9X,'PARTIAL 2E INTEGRALS ARE BUILT FROM ',A8)
 9030 FORMAT(1X,'**** ERROR ***** THE ONLY CORRECT CITYP IS CITYP=GUGA')
 9040 FORMAT(1X,'**** ERROR IN ',A8,' GROUP...HALTING.')
 9045 FORMAT(1X,'**** ERROR, A CORRESPONDING ORBITAL TRANSFORMATION'/
     *       1X,'FOR FOCI OR SOCI WILL CHANGE THE WAVEFUNCTION.'/
     *       1X,'FOCI/SOCI REQUIRE USE OF JUST ONE SET OF ORBITALS.')
 9050 FORMAT(1X,A8,' GROUP IS REQUIRED FOR A SPIN-ORBIT RUN.')
 9055 FORMAT(1X,'NUMVEC SHOULD BE EQUAL TO NEGATIVE $MCQD DECK NUMBER,',
     *         /,'WHENCE THE ORBITALS ARE TAKEN (CANONICAL OR NOT).',/)
 9060 FORMAT(1X,'NUMVEC CAN ONLY TAKE VALUES OF 1 OR 2',/)
 9070 FORMAT(1X,'UNRECOGNISED ACTION=',A8,'(NOT NORMAL,READ,SAVE)',/)
 9080 FORMAT(1X,'ILLEGAL IVEX GIVEN',I2)
 9090 FORMAT(1X,'INTERNAL BUFFER FOR CI ROOTS IS TOO SMALL:',I3)
 9100 FORMAT(5X,'RESC SOC CORRECTION LEVEL ',I1,'E')
 9105 FORMAT(5X,'RESC SOC CORRECTIONS ARE ONLY AVAILABLE WITH',/1X,
     *       'HSO1, HSO2 OR HSO2P',/)
 9110 FORMAT(5X,'MCP 2E INTEGRALS CAN ONLY BE USED WITH OPERAT=HSO*',/)
 9115 FORMAT(5X,'MCP 2E INTEGRALS CANNOT BE USED WITH NESC AND ',
     *          'NESOC.GT.1',/)
 9120 FORMAT(5X,'MCP2E=1 AND OPERAT=HSO2 IS AN ILLEGAL COMBINATION',/)
 9210 FORMAT(/1X,'....... DONE WITH TRANSITION MOMENT .......',/)
 9220 FORMAT(/1X,'RELATIVISTIC 2E SOC CORRECTIONS ARE NOT CODED YET',
     *       /1X,'FOR INTERNAL UNCONTRACTION. SET NESOC TO 1.')
 9230 FORMAT(/1X,'ONLY ONE $VEC IS CURRENTLY SUPPORTED WITH NUMVEC<0.')
 9250 FORMAT(/5X,31("-")/5X,'INPUT FOR TRANSITION MOMENT RUN'/
     *        5X,31(1H-)/
     *        5X,'OPERAT=',A8,5X,'NFZC  =',I8,5X,'NOCC  =',I8/
     *        5X,'NUMCI =',I8,5X,'NUMVEC=',I8,5X,'SYMTOL=',1P,E10.1/
     *        5X,'IPRHSO=',I8,5X,'NOSYM =',I8,5X,'ETOL  =',F13.6/
     *        5X,'TMOMNT=',L8,5X,'SKIPDM=',L8,5X,'PRTPRM=',L8/
     *        5X,'SLOWFF=',L8,5X,'PHYSRC=',L8,5X,'MODPAR=',I8/
     *        5X,'JZ    =',I8,5X,'PARMP =',I8,5X,'ZEFTYP=',A8/
     *        5X,'MCP2E =',I8,5X,'ONECNT=',I8,5X,'NFFBUF=',I8/
     *        5X,'EEQTOL=',E11.2,1X/)
 9251 FORMAT( 5X,'IROOTS=',7X,20I3)
 9252 FORMAT( 5X,'NSTATE=',7X,20I3)
 9254 FORMAT( 5X,'IVEX  =',7X,20I3)
 9256 FORMAT( 5X,'L2VAL =',7X,20I3)
 9260 FORMAT( 5X,'ZEFF  =',5X,10F6.1)
 9500 FORMAT(/5X,'**** WARNING: NOSYM SET TO',I2,' ABOVE (Q.V.)',/)
      END
C*MODULE TRNSTN  *DECK MPIVOX
      SUBROUTINE MPIVOX(NFT17,L1,NQMT,NOSYM,EREFCAS,HMP2,EORB,EREF0,
     *                  ENGYST,NWKSST,MULST,IROOTS,IVEX,NSTATES,IVCORB,
     *                  CSMALL,ISODIAG,NMOFZC,NMODOC,NMOACT,NMOEXT,IFORB
     *                 ,EDSHFT,JREST,JSODA,JSODAN)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXRT=100, MXAO=2047)
      PARAMETER (ZERO=0.0D+00,ONE=1.0D+00)
      LOGICAL GOPARR,DSKWRK,MASWRK,KSTATE,LMQPAR
C
      DIMENSION EREFCAS(MXRTTRN,NUMCI),HMP2(MXRTTRN,MXRTTRN,NUMCI),
     *          EREF0(MXRTTRN,NUMCI),ENGYST(MXRTTRN,NUMCI),
     *          IVCORB(L1,NUMCI),MULST(NUMCI),NWKSST(NUMCI),
     *          IROOTS(NUMCI),NSTATES(NUMCI),IVEX(NUMCI),
     *          EORB(L1)
C
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,MFZC,NUMVEC,ICI,
     *                MXRTTRN,NSTAT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MQ2PAR/ DMQPAR(200),AVECOE(MXRT),
     *                IMQPAR(400),MAINCS( 3),KSTATE(MXRT),LMQPAR(10)
      COMMON /SOMCQD/ MAXSPF,MAXSOC,NCSF,NOCF
      COMMON /MQIOFI/ IDAF50,NAV50,IODA50(400)
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /TMVALS/ TI,TX,TIM
      COMMON /RESTAR/ TIMLIM,IREST,NREC,INTLOC,IST,JST,KST,LST
      COMMON /FMCOM / X(1)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /SYMBLK/ NIRRED,NSALC,NSALC2,NSALC3
      COMMON /GUGWFN/ NFZC,NMCC,NDOC,NAOS,NBOS,NALP,NVAL,NEXT,NFZV,
     *                IFORS,IEXCIT,ICICI,NOIRR
      COMMON /ORBSET/ NORBMX,NORBS,NCORBS,NLEVS,MA,MB,NC,NSYM,MSYM,
     *                IDOCC,IVAL,IMCC,ISYM(MXAO),ICODE(MXAO),
     *                NLCS(MXAO),LEVPT(MXAO),LEVNR(MXAO),IOUT(MXAO),
     *                NREFS,IEXCT,NFOCI,INTACT
      COMMON /SOSYM/  EULANG(4,48),GAM(48,48),IRMON(MXAO)
      CHARACTER*8 CMCQDPT,CVEC,CDRT
      CHARACTER*2 ANG
      EQUIVALENCE (CMCQDPT,RMCQDPT),(CVEC,RVEC),(CDRT,DRTNAM)
C
C     ----- CARRY OUT A MCQDPT for each spin multiplicity -----
C
      L2=(L1*L1+L1)/2
      L3=L1*L1
      IF(JREST.GT.0) THEN
C        the values below are usually set in MCQDPT
C        that we do not run in this case
         IDAF50=50
         CALL VALFM(LOADFM)
         LVST1 = LOADFM+1
         LAST  = LVST1 + L3
         NEED  = LAST-LOADFM-1
         CALL GETFM(NEED)
         CALL MQOPDA(1)
         CALL MQDARE(IDAF50,IODA50,EREFCAS,MXRTTRN*NUMCI,50,0)
         CALL MQDARE(IDAF50,IODA50,HMP2,MXRTTRN*MXRTTRN*NUMCI,51,0)
         CALL MQDARE(IDAF50,IODA50,EREF0,MXRTTRN*NUMCI,52,0)
         CALL MQDARE(IDAF50,IODA50,ENGYST,MXRTTRN*NUMCI,53,0)
         NDSIZE=MQDSIZ(NUMCI,'I')
         CALL MQDARE(IDAF50,IODA50,NWKSST,NDSIZE,54,1)
         NDSIZE=MQDSIZ(NUMCI,'I')
         CALL MQDARE(IDAF50,IODA50,MULST,NDSIZE,55,1)
         NDSIZE=MQDSIZ(L1*NUMCI,'I')
         CALL MQDARE(IDAF50,IODA50,IVCORB,NDSIZE,56,1)
         NDSIZE=MQDSIZ(400,'I')
         CALL MQDARE(IDAF50,IODA50,IMQPAR,NDSIZE,57,1)
         NDSIZE=MQDSIZ(L1,'I')
         CALL MQDARE(IDAF50,IODA50,IRMON,NDSIZE,58,1)
         IFORB =IMQPAR( 1)
         NMOACT=IMQPAR(17)
         NMODOC=IMQPAR(18)
         NMOEXT=IMQPAR(19)
         NMOFZC=IMQPAR(20)
         NDOUB=NMOFZC+NMODOC
         NGO=L1
         NMOEXT=NMOEXT+NQMT-NGO
         NMO=NMOFZC+NMODOC+NMOACT+NMOEXT
         IF(MASWRK) WRITE(IW,9200) NMOFZC,NMODOC,NMOACT,NMOEXT,NMO,NGO
         CALL MQDARE(IDAF50,IODA50,X(JSODA),JSODAN,59,0)
         CALL MQDARE(IDAF50,IODA50,X(LVST1),L1*NMO,60,0)
         CALL DAWRIT(IDAF,IODA,X(LVST1),L3,15,0)
         CALL DAWRIT(IDAF,IODA,X(LVST1),L3,19,0)
         CALL MQDARE(IDAF50,IODA50,EDSHFT,1,61,0)
         CLOSE(UNIT=IDAF50,STATUS='KEEP')
         CALL RETFM(NEED)
         CALL SEQOPN(NFT17,'CSFSAVE','UNKNOWN',.FALSE.,'UNFORMATTED')
         CALL SEQREW(NFT17)
C
C        the parameters below are set for the last MCQDPT. They are
C        used mostly to determine the number of e- etc so that is OK.
C
         NFZC=NDOUB
         NAOS=0
         NBOS=0
         NALP=MULST(NUMCI)-1
         NAEL=IMQPAR(16)-NDOUB*2
         NDOC=(NAEL-NALP)/2
         NVAL=NOCC-NFZC-NALP-NDOC
         NFOCI=0
         IFORS=1
         NUMVEC=1
         RETURN
      ENDIF
Cnb   do not do MCQDPT if ISODIAG=0
      CALL VALFM(LOADFM)
      LVST1 = LOADFM+1
      LHEFF = LVST1 + L3
      LAST  = LHEFF + MXRTTRN*MXRTTRN*4
      NEED  = LAST-LOADFM-1
      CALL GETFM(NEED)
C
      NTOTR=0
      DO I=1,NUMCI
        NTOTR=NTOTR+IROOTS(I)
      ENDDO
C     check if the user provided $ENERGY group
      CALL READCEN(IR,IW,NQMT,X(LVST1),IGOTEN)
      IF(IGOTEN.NE.0) THEN
         CALL DAWRIT(IDAF,IODA,X(LVST1),NQMT,395,0)
         IF(MASWRK) THEN
            WRITE(IW,*) 'ORBITAL ENERGIES ARE TAKEN FROM $ENERGY'
         ENDIF
      ENDIF
C
      CALL VCLR(X(LVST1),1,L3)
C     CALL DAread(IDAF,IODA,x(lVST1),L3,15,0)
C     SVGPAR=GOPARR
      CALL SEQOPN(NFT17,'CSFSAVE','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQREW(NFT17)
      SAVECOE=ZERO
      NOCCSAV=NOCC
      NOCC=0
      IF(IGOTEN.GT.0.AND.NUMVEC.GT.0) NOCC=1
C     this will skip the orbital energy generating step.
  100 CONTINUE
      DO 200 IMP=1,NUMCI
        ICI=IMP
C       switch around MCQDPT NUMCI-th and NUMVEC-th runs in order to
C       use the data saved in MCQDPT run NUMVEC (mostly, the integrals).
C
        IF(IMP.EQ.NUMCI) ICI=ABS(NUMVEC)
        IF(IMP.EQ.ABS(NUMVEC)) ICI=NUMCI
        ANG='TH'
        IF(ICI.EQ.1.OR.MOD(ICI,10).EQ.1.AND.MOD(ICI,100).NE.11) ANG='ST'
        IF(ICI.EQ.2.OR.MOD(ICI,10).EQ.2.AND.MOD(ICI,100).NE.12) ANG='ND'
        IF(ICI.EQ.3.OR.MOD(ICI,10).EQ.3.AND.MOD(ICI,100).NE.13) ANG='RD'
        IVEC=IVEX(ICI)
        WRITE(UNIT=CVEC(1:8),FMT='(A5,I1,A2)') ' $VEC',IVEC,'  '
        IF(ICI.LE.9) THEN
          WRITE(UNIT=CMCQDPT(1:8),FMT='(A4,I1)') 'MCQD',ICI
          WRITE(UNIT=CDRT(1:8),FMT='(A3,I1)') 'DRT',ICI
        ELSE
          WRITE(UNIT=CMCQDPT(1:8),FMT='(A4,I2)') 'MCQD',ICI
          WRITE(UNIT=CDRT(1:8),FMT='(A3,I2)') 'DRT',ICI
        ENDIF
        IF(NUMCI.GT.99) THEN
                        WRITE(UNIT=CDRT(1:8),FMT='(A3,I3)') 'DRT',ICI
                        WRITE(UNIT=CMCQDPT(1:8),FMT='(A3,I1)') 'MCQ',ICI
          IF(ICI.GT. 9) WRITE(UNIT=CMCQDPT(1:8),FMT='(A3,I2)') 'MCQ',ICI
          IF(ICI.GT.99) WRITE(UNIT=CMCQDPT(1:8),FMT='(A3,I3)') 'MCQ',ICI
        ENDIF
        IF(MASWRK) WRITE(IW,9080) ICI,ANG,'MCQDPT'
C       at present MCQDPT is not parallelised.
        NSTAT=MAX(IROOTS(ICI),NSTATES(ICI))
        MXRTSAVE=MXRTTRN
        MXRTTRN=NTOTR
C
        CALL MCQDPT(RMCQDPT,RVEC)
        CALL TIMIT(1)
C
        MXRTTRN=MXRTSAVE
C       GOPARR = SVGPAR
        IFORB =IMQPAR( 1)
        INORB =IMQPAR( 3)
        MULT  =IMQPAR(13)
        NMOACT=IMQPAR(17)
        NMODOC=IMQPAR(18)
        NMOEXT=IMQPAR(19)
        NMOFZC=IMQPAR(20)
        NSTATE=IMQPAR(23)
        EDSHFT=DMQPAR( 8)
        IPURFY=IMQPAR(27)
        NMOEXT=NMOEXT+NQMT-L1
        NMO=NMOFZC+NMODOC+NMOACT+NMOEXT
        NDIM=IROOTS(ICI)
C       ndim0=nstate
        N1=NOCCSAV-MFZC
        IF(NMODOC+NMOFZC.NE.MFZC.OR.NMOACT.NE.N1) THEN
          IF(MASWRK) WRITE(IW,9010) NMODOC+NMOFZC,MFZC,NMOACT,N1
          CALL ABRT
        ENDIF
        IF((IFORB.EQ.0.AND.NUMVEC.LT.0.AND.INORB.GE.0.
     *      OR.IFORB.GT.0.AND.NUMVEC.GT.0) .AND.MASWRK) WRITE(IW,9030)
        IF(NDIM.NE.NSTATE) THEN
          IF(MASWRK) WRITE(IW,9020) NDIM,NSTATE
          CALL ABRT
        ENDIF
        IF(INORB.EQ.1.AND.IGOTEN.NE.0.AND.IPURFY.NE.0) THEN
           IF(MASWRK) WRITE(IW,*) 'NO ORBITAL PURIFICATION WITH $ENERGY'
           CALL ABRT
        ENDIF
        DO I=1,NSTATE
           SAVECOE=SAVECOE+AVECOE(I)
        ENDDO
        IF((IFORB.GT.2.OR.IFORB.EQ.0).AND.NOCC.EQ.0.AND.INORB.GE.0)
     *     GOTO 200
        CALL MQOPDA(1)
        NREC=7
        IF(NUMVEC.LT.0.AND.(IFORB.LE.2.OR.NOCC.EQ.0)) NREC=8
        CALL MQDARE(IDAF50,IODA50,X(LVST1),L1*NMO,NREC,0)
        IF(ICI.EQ.ABS(NUMVEC)) THEN
C
C         7 means those given by the user in $VEC and 8 means canonical
C         note that we read NMO canonical orbitals and other L1-NMO
C         orbitals are taken from those generated as orthogonal
C         compliment to NOCC given by the user in $VECn. Those L1-NMO
C         may not be orthogonal to the first NMO orbitals but they
C         are not used for anything relevant though.
C         WRITE(IW,*) 'MCQDPT MOs 1'
C         CALL PRsq(x(lvst1),L1,l1,l1)
          CALL DAWRIT(IDAF,IODA,X(LVST1),L3,15,0)
          CALL DAWRIT(IDAF,IODA,X(LVST1),L3,19,0)
          CALL MQDARE(IDAF50,IODA50,EORB,NMO,9,0)
        ENDIF
C
        MULST(ICI)=MULT
        CALL MQDARE(IDAF50,IODA50,EREFCAS(1,ICI),NDIM,17,0)
        CALL MQDARE(IDAF50,IODA50,EREF0(1,ICI),NDIM,22,0)
        CALL MQDARE(IDAF50,IODA50,X(LHEFF),NDIM*NDIM*4,23,0)
        DO I=1,NDIM
          DO J=1,NDIM
C           extract the MCQDPT Hamiltonian (0+1+2) order
            HMP2(I,J,ICI)=X(LHEFF+I-1+(J-1)*NDIM+NDIM*NDIM*2)
C           write(6,*) ici,i,j,'readd',HMP2(i,j,ici)
          ENDDO
        ENDDO
        IF(ISODIAG.EQ.0)
     *    CALL MQDARE(IDAF50,IODA50,ENGYST(1,ICI),NDIM,24,0)
C
C     Save CSF information
C
C**** READ IOMAP, ISMAP, NSNSF ***************************
C
        CALL VALFM(LOADFM)
        LIOMAP = LOADFM+1
        LISMAP= LIOMAP+((NMOACT+1)*NOCF-1)/NWDVAR+1
        LNSNSF= LISMAP+((MAXSOC+1)*MAXSPF-1)/NWDVAR+1
        LICASE= LNSNSF+(NOCF+1-1)/NWDVAR+1
        LIECONF=LICASE+(L1-1)/NWDVAR+1
        LCASVEC=LIECONF+(L1-1)/NWDVAR+1
        LISTSYM=LCASVEC+NCSF*NSTATE
        LWEIGHT=LISTSYM+(NSTATE-1)/NWDVAR+1
        LLABMO =LWEIGHT+NIRRED*NSTATE
        LS     =LLABMO+(L1-1)/NWDVAR+1
        LQ     =LS+L2
        LWRK   =LQ+L3
        LAST   =LWRK+L1
        NEED1  = LAST-LOADFM-1
        CALL GETFM(NEED1)
        NDOUB=NMOFZC+NMODOC
        NDSIZE=MQDSIZ((NMOACT+1)*NOCF,'I')
        CALL MQDARE(IDAF50,IODA50,X(LIOMAP),NDSIZE,1,1)
        NDSIZE=MQDSIZ((MAXSOC+1)*MAXSPF,'I')
        CALL MQDARE(IDAF50,IODA50,X(LISMAP),NDSIZE,2,1)
        NDSIZE=MQDSIZ(NOCF+1,'I')
        CALL MQDARE(IDAF50,IODA50,X(LNSNSF),NDSIZE,3,1)
        NDSIZE=NCSF*NSTATE
        CALL MQDARE(IDAF50,IODA50,X(LCASVEC),NDSIZE,18,0)
C       NDSIZE=MQDSIZ(NMO,'I')
C       CALL MQDARE(IDAF50,IODA50,x(lmosym),NDSIZE,10,1)
        WRITE(NFT17) DRTNAM
        CALL DAREAD(IDAF,IODA,X(LS),L2,12,0)
        CALL DAREAD(IDAF,IODA,X(LQ),L3,45,0)
        NSTAT=1
        CALL SYMMOS(X(LLABMO),X(LQ),X(LS),X(LVST1),X(LWRK),L1,L1,
     *              NMO,L1)
C    *              NQMT,L1)
        CALL SAVCSF(NFT17,NSTATE,NDOUB,NOCCSAV,NMO,NCSF,NOCF,MAXSOC,
     *              MAXSPF,X(LIOMAP),X(LISMAP),X(LNSNSF),X(LICASE),
     *              X(LIECONF),X(LCASVEC),X(LISTSYM),X(LWEIGHT),
     *              X(LLABMO),IROOTS,NWKS,CSMALL,NOIRR,NOSYM)
        NWKSST(ICI)=NWKS
        CALL RETFM(NEED1)
C
C       save the orbital ordering in ICASE. MCQDPT actually generates
C       the CSFs with the consequent order of orbitals.
C       Fill in GUGA parameters.
C
        DO I=1,NMOACT
          IVCORB(I,ICI)=I+NDOUB
        ENDDO
        NFZC=NDOUB
        NAOS=0
        NBOS=0
        NALP=MULT-1
        NAEL=IMQPAR(16)-NDOUB*2
        NDOC=(NAEL-NALP)/2
        NVAL=NOCCSAV-NFZC-NALP-NDOC
        NFOCI=0
        IFORS=1
C      write(6,*) 'GUGA data',NFZC,naos,nbos,nalp,ndoc,nval,nfoci,ifors
C       potentially we need IEXCIT
C
        CLOSE(UNIT=IDAF50,STATUS='KEEP')
C       call ddi_sync(353)
        IF(TIM.GT.TIMLIM) GO TO 800
  200 CONTINUE
C
      IF(IFORB.GT.2.AND.ABS(SAVECOE-ONE).GT.1.0D-03.AND.NOCC.EQ.0) THEN
        WRITE(IW,9140) SAVECOE
        CALL ABRT
      ENDIF
C
      IF((IFORB.GT.2.OR.IFORB.EQ.0).AND.NOCC.EQ.0.AND.INORB.GE.0) THEN
         IF(IFORB.GT.2.AND.MASWRK) WRITE(IW,9090)
         IF(IFORB.EQ.0.AND.MASWRK) WRITE(IW,9100)
C        save canonicalised orbitals after the final averaging
         IF(IFORB.GT.2)THEN
           CALL MQOPDA(1)
           CALL MQDARE(IDAF50,IODA50,X(LVST1),L1*NMO,8,0)
           CALL DAWRIT(IDAF,IODA,X(LVST1),L3,15,0)
           CLOSE(UNIT=IDAF50,STATUS='KEEP')
         ENDIF
         NOCC=NOCC+1
         GOTO 100
      ENDIF
C
      NUMVEC=1
      NOCC=NOCCSAV
C
C     save data for restart
C
         CALL MQOPDA(1)
         CALL MQDAWR(IDAF50,IODA50,EREFCAS,MXRTTRN*NUMCI,50,0)
         CALL MQDAWR(IDAF50,IODA50,HMP2,MXRTTRN*MXRTTRN*NUMCI,51,0)
         CALL MQDAWR(IDAF50,IODA50,EREF0,MXRTTRN*NUMCI,52,0)
         CALL MQDAWR(IDAF50,IODA50,ENGYST,MXRTTRN*NUMCI,53,0)
         NDSIZE=MQDSIZ(NUMCI,'I')
         CALL MQDAWR(IDAF50,IODA50,NWKSST,NDSIZE,54,1)
         NDSIZE=MQDSIZ(NUMCI,'I')
         CALL MQDAWR(IDAF50,IODA50,MULST,NDSIZE,55,1)
         NDSIZE=MQDSIZ(L1*NUMCI,'I')
         CALL MQDAWR(IDAF50,IODA50,IVCORB,NDSIZE,56,1)
         NDSIZE=MQDSIZ(400,'I')
         CALL MQDAWR(IDAF50,IODA50,IMQPAR,NDSIZE,57,1)
         NDSIZE=MQDSIZ(L1,'I')
         CALL MQDAWR(IDAF50,IODA50,IRMON,NDSIZE,58,1)
         CALL MQDAWR(IDAF50,IODA50,X(JSODA),JSODAN,59,0)
         CALL DAREAD(IDAF,IODA,X(LVST1),L3,15,0)
         CALL MQDAWR(IDAF50,IODA50,X(LVST1),L1*NMO,60,0)
         CALL MQDAWR(IDAF50,IODA50,EDSHFT,1,61,0)
         CLOSE(UNIT=IDAF50,STATUS='KEEP')
C
      CALL RETFM(NEED)
      RETURN
  800 CONTINUE
      IF (MASWRK) WRITE(IW,9130) RUNTYP
      CALL ABRT
 9010 FORMAT(/1X,'INCOMPATIBLE $DRT AND $MCQDPT, CHECK CORE',
     *       I3,' AND',I3,'AND ACTIVE ',I3,' AND',I3/)
 9020 FORMAT(/1X,'KSTATE ARRAY DISAGREES WITH IROOT',2I3)
 9030 FORMAT(/1X,'WARNING: INCOSISTENT ORBITAL CHOICE IN $MCQDPT AND ',
     *       '$TRANST',/1X,'SPECIFICALLY, IFORB AND NUMVEC OPTIONS.',/)
C9020 format(/1x,'SO-MCQDPT orbital choice:')
C9030 format(/1x,'each MCQDPT run uses its own canonical orbitals.',
C    *       /1x,'SO-MCQDPT run uses the canonical orbitals from ',I3,
C    *           'MCQDPT deck',/)
C9040 format(/1x,'same orbitals are used for either spin-free or',
C    *       /1x,'spin-dependent MCQDPT')
C9050 format(/1x,'each MCQDPT run uses its own canonical orbitals.',/1x,
C    *         'SO-MCQDPT run uses the orbitals provided by the user',/)
 9080 FORMAT(//5X,25("-")/5X,I3,'-',A2,A7,' CALCULATION',/5X,25("-"))
 9090 FORMAT(1X,'CANONICALISED ORBITALS AVERAGED OVER ALL MULTIPLICITES'
     *         ,' HAVE BEEN COMPUTED. ',/1X,
     *          'NOW ACTUAL MCQDPT COMPUTATIONS WILL START.',/)
 9100 FORMAT(1X,'ENERGIES FOR THE SELECTED MULTIPLICITY HAVE BEEN ',
     *          'COMPUTED.',/1X,
     *          'NOW ACTUAL MCQDPT COMPUTATIONS START.',/)
C9100 FORMAT(1X,'Computing canonicalised orbitals averaged over all ',
C    *          'multiplicites',/)
 9130 FORMAT(1X,'TIME USED EXCEEDS INPUT TIMLIM, RERUN THE WHOLE JOB.'/
     *       1X,'NO RESTART FOR RUNTYP=',A8,' IS IMPLEMENTED')
 9140 FORMAT(1X,'WSTATE WEIGHTS DO NOT SUM TO ONE (AFTER NORMALISATION',
     *       /1X,'BY THE TOTAL NUMBER OF ROOTS IN IROOTS)',F9.5/)
 9200 FORMAT(/1X,'--- RESTART ENGAGED: READING DATA:',
     *       /1X,'NMOFZC,NMODOC,NMOACT,NMOEXT,NMO,NGO',6I4,/)
      END
C*MODULE TRNSTN  *DECK TRANSINI
      SUBROUTINE TRANSINI(PRTCMO,TOLZ,TOLE,L1,DEIG,ORBIS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, MXSH=1000, ONE=1.0D+00)
C
      LOGICAL PRTCMO,GOPARR,DSKWRK,MASWRK,SVGPAR,ABEL,ABELPT,PK,PANDK,
     *        BLOCK,DIRTRF,ORBIS
C
      DIMENSION DEIG(L1)
C
      COMMON /FMCOM / X(1)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PKFIL / PK,PANDK,BLOCK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                T(432),INVT(48),NT
      COMMON /TRFOPT/ CUTTRF,NWDTRF,MPTRAN,ITRFAO,NOSYMT,DIRTRF
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,
     *                MXRTTRN,NSTAT
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
      COMMON /RESTAR/ TIMLIM,IREST,NREC,INTLOC,IST,JST,KST,LST
C
C     ----- GROW MEMORY FOR ORBITAL SELECTION -----
C
      L2=(L1*L1+L1)/2
      L3=L1*L1
      N1 = NOCC
      N3 = N1*N1
C
      CALL VALFM(LOADFM)
      LIA   = LOADFM+1
      LIWRK = LIA   + L1
      LEIG  = LIWRK + L1
      LSCR  = LEIG  + L1
      LS    = LSCR  + L1*8
      LQ    = LS    + L2
      LVST1 = LQ    + L3
      LVST2 = LVST1 + L3
      LWRK  = LVST2 + L3
      LD    = LWRK  + L3
      LU    = LD    + N3
      LV    = LU    + N3
      LAST  = LV    + N3
      NEED  = LAST-LOADFM
      CALL GETFM(NEED)
C
C            GET OVERLAPS AND CANONICAL ORBITALS
C     -VST1- IS USED AS SCRATCH STORAGE AT THIS POINT
C
      CALL DAREAD(IDAF,IODA,X(LS),L2,12,0)
      CALL QMTSYM(X(LS),X(LVST1),X(LQ),X(LWRK),X(LSCR),X(LIWRK),L0,L1,
     *            L2,L3,.FALSE.)
      CALL DAWRIT(IDAF,IODA,X(LQ),L3,45,0)
      CALL DAREAD(IDAF,IODA,X(LS),L2,12,0)
      IF(MPLEVL.EQ.0) THEN
C
C       ----- READ IN ONE OR TWO SETS OF ORBITALS -----
C
        CALL TRNORB(X(LSCR),X(LS),X(LQ),X(LVST1),X(LVST2),L0,L1,L2,L3,
     *              TOLZ,TOLE,ORBIS)
C
C       ----- GENERATE THE CORRESPONDING MO-S -----
C
        CALL TRNCMO(EXETYP,X(LIA),X(LEIG),X(LSCR),X(LWRK),X(LS),X(LD),
     *            X(LU),X(LV),X(LVST1),X(LVST2),L1,L2,L3,N1,PRTCMO,DEIG)
      ELSE
C       The orbitals will be got from the MCQDPT code directly
        CALL DACOPY(N1,ONE,DEIG,1)
      ENDIF
      CALL TIMIT(1)
C
C     ----- PREPARE CORRECT INTEGRAL LIST ---
C     THIS MUST BE -J- FORMAT, AND FOR NON-ABELIAN, A C1 LIST
C     FOR PARALLEL RUNS, EACH NODE MAY NEED A COMPLETE AO INTEGRAL FILE
C
      ABEL = ABELPT()
      NTSAVE = NT
      SVGPAR = GOPARR
      IF(ITRFAO.EQ.1) GOPARR = .FALSE.
      PK     = .FALSE.
      PANDK  = .FALSE.
      NOPK   = 1
      IF(.NOT.ABEL) NT = 1
      IF(IREST.LE.1) CALL JANDK
      GOPARR = SVGPAR
      NT     = NTSAVE
      IF (MASWRK) WRITE(IW,9070)
      CALL RETFM(NEED)
      RETURN
 9070 FORMAT(1X,'...... END OF ORBITAL SELECTION ......')
      END
C*MODULE TRNSTN  *DECK SAVCSF
      SUBROUTINE SAVCSF(NFTCSF,NSTATE,NDOUB,NORB,NMO,NCSF,NOCF,MAXSOC,
     *                  MAXSPF,IOMAP,ISMAP,NSNSF,ICASE,IECONF,CASVEC,
     *                  ISTSYM,WEIGHT,LABMO,IROOTS,NWKS,CSMALL,NOIRR,
     *                  NOSYM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MAXCP=4096,MXIRR=14,MXAO=2047)
      LOGICAL GOPARR,DSKWRK,MASWRK,ABEL,ABELPT,SKIPS
      DIMENSION IOMAP(NDOUB+1:NORB+1,NOCF),ISMAP(MAXSOC+1,MAXSPF),
     *          NSNSF(NOCF+1),ICASE(NORB),IECONF(*),CASVEC(NCSF,NSTATE),
     *          ISTSYM(NSTATE),IROOTS(NUMCI),WEIGHT(NIRRED,NSTATE),
     *          LABMO(*)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,
     *                MXRTTRN,NSTAT
      COMMON /SOGUG/  CP(MAXCP),NUNIQ,LIOBP,ISODAF,NSODA,LSTREC,JSODAF,
     *                JSODA,NRECJ
      COMMON /FMCOM / X(1)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /SOSYM/  EULANG(4,48),GAM(48,48),IRMON(MXAO)
      COMMON /SYMMUL/ NIJREP(MXIRR,MXIRR),IJREP(2,MXIRR,MXIRR,MXIRR)
      COMMON /SYMBLK/ NIRRED,NSALC,NSALC2,NSALC3
      COMMON /SYMREP/ IRPNAM(MXIRR),IPA(MXIRR),LAMBDA(MXIRR),
     *                LAMBD0(MXIRR),IADDR1(MXIRR),IADDR2(MXIRR),
     *                IADDR3(MXIRR)
C
C     this apeared to need the usual DSKWRK=.t. trick for parallel runs
C     but it works without it?
C
      ABEL=ABELPT()
      SKIPS=NOIRR.GT.0
      SMALL=SYMTOL
      IF(.NOT.ABEL.AND..NOT.SKIPS.AND.MASWRK) WRITE(IW,*) 'NON-ABEL?'
C     Non-Abelian symmetry will work (by ignoring symmetry).
C     if(ABEL) then
C        CALL RAREAD(JSODAF,X(JSODA),X(LCSFIR),(NWKS-1)/NWDVAR+1,
C    *               (ICI-1)*NRECJ+5,1)
C     else if(.NOT.SKIPS) THEN
C        CALL DAREAD(IDAF,IODA,X(LCLCAO),NUM*NUM,15,0)
C        read MCQDPT orbs
C        CALL DAREAD(IDAF,IODA,X(LS),(NUM*NUM+NUM)/2,12,0)
C        CALL SEREP(NAO,NCORE,X(LFF),X(LCHARR),X(LIRRID),X(LIRROW),
C    *              X(LCLCAO),X(LS),X(LWORK1),X(LFUNSYM),X(LPROJ),
C    *              SMALL,.FALSE.)
C     ENDIF
C     write(6,*) 'www2',(irmon(i),i=1,nmo)
      IF(MASWRK) THEN
         WRITE(IW,9065) NDOUB,NORB-NDOUB,NMO-NORB
         IF(NDOUB.GT.0) WRITE(IW,9070) (LABMO(I),I=1,NDOUB)
         WRITE(IW,9080) (LABMO(I),I=NDOUB+1,NORB)
         WRITE(IW,9100) (LABMO(I),I=NORB+1,NMO)
         WRITE(IW,*) ' '
      ENDIF
      IF(MASWRK) WRITE(IW,9000)
        IF(NSTAT.LT.0.AND.NOSYM.EQ.0) THEN
        IF(MASWRK) WRITE(IW,9300)
        NOSYM=1
      ENDIF
      CALL VCLR(WEIGHT,1,NIRRED*NSTATE)
      NWKS=0
C     n1=norb-ndoub
      DO IOCF=1,NOCF
        NSF=NSNSF(IOCF+1)-NSNSF(IOCF)
        ICSF=NSNSF(IOCF)
        DO IOSF=1,NSF
          ICSF=ICSF+1
          NSOC=MAXSOC-IOMAP(NORB+1,IOCF)
          IOPEN=0
          IRRCSF=1
          DO I=NDOUB+1,NORB
            IOP=IOMAP(I,IOCF)
            IF(IOP.EQ.1) THEN
              ICA1=1
            ELSE IF(IOP.EQ.2) THEN
              IOPEN=IOPEN+1
              NSOC=NSOC+1
              IF(ISMAP(NSOC,IOSF).EQ.2) THEN
                ICA1=2
              ELSE
                ICA1=3
              END IF
C             FIND DIRECT PRODUCT OF IRREDUCIBLE REPRESENTATIONS FOR ALL
C             singly occupied ORBITALS IN A CSF
              IGAM=IRMON(I)
C             IF(LAMBD0(IGAM).EQ.1) THEN
                IRRCSF=IJREP(2,1,IRRCSF,IGAM)
C             ELSE
            ELSE
              ICA1=4
            END IF
            ICASE(I)=ICA1
            IECONF(I)=IOP-1
          ENDDO
          COEFMAX=CASVEC(ICSF,IDAMAX(IROOTS(ICI),CASVEC(ICSF,1),NCSF))
          IF(ABS(COEFMAX).GT.CSMALL.OR.NOSYM.GT.1) THEN
            NWKS=NWKS+1
            WRITE(NFTCSF) IOPEN,(ICASE(I),I=NDOUB+1,NORB),
     *                    (IECONF(I),I=NDOUB+1,NORB),
     *                    (CASVEC(ICSF,J),J=1,IROOTS(ICI))
C       write(6,*) icsf,iocf,'www CSF',iopen,'case',(icase(i),i=ndoub+1,
C    *             norb),' ieconf',(iomap(i,iocf)-1,i=ndoub+1,norb),
C    *             (casvec(icsf,j),j=1,IROOTS(ICI))
          ENDIF
          IF(ABEL) THEN
            DO I=1,NSTATE
              DUM=CASVEC(ICSF,I)
              WEIGHT(IRRCSF,I)=WEIGHT(IRRCSF,I)+DUM*DUM
            ENDDO
          ENDIF
        ENDDO
      ENDDO
      DO I=1,IROOTS(ICI)
        IF(ABELPT()) THEN
          IRREP=0
          DO IG=1,NIRRED
            IF(WEIGHT(IG,I).GT.SMALL) THEN
             IRREP=IOR(ISHFT(IRREP,4),IG)
             IF(MASWRK) WRITE(IW,9060) I,IRPNAM(IG),WEIGHT(IG,I)*1.0D+02
            ENDIF
          ENDDO
          IF(MASWRK) WRITE(IW,*) ' '
          ISTSYM(I)=IRREP
        ELSE
C         for now, leave alone non-Abelian groups
          ISTSYM(I)=0
C     IF(NONABEL) THEN
C        IF(SKIPS) THEN
C           IRET=0
C        ELSE
C           CALL IRRSTATE(IRET,NAO,NCORE,NAEL,MUL,NDET,X(LIGCSF),
C  *               X(LCCI),X(LFF),X(LCHARR),X(LIRRID),X(LIRROW),SMALL)
C         ENDIF
C        IRREP=IRET
C     ENDIF
        ENDIF
      ENDDO
      IF(MASWRK) WRITE(IW,9400) CSMALL,NWKS,NCSF
      CALL RAWRIT(JSODAF,X(JSODA),ISTSYM,(NSTATE-1)/NWDVAR+1,
     *            (ICI-1)*NRECJ+6,1)
      RETURN
 9000 FORMAT(/1X,'CSF STORAGE AND SYMMETRY ANALYSIS FOR SO-MCQDPT')
C9060 FORMAT(1X,'THE PROJECTION OF CI STATE ',I3,' ONTO SPACE SYMMETRY',
C    *         ' ',A4,' WEIGHS',1P,E11.4)
 9060 FORMAT(1X,'CI STATE ',I3,' BELONGS TO ',5(1X,A4,'(',F8.4,'%)')/
     *                                   (30X,5(1X,A4,'(',F8.4,'%)')))
 9065 FORMAT(/1X,'SYMMETRIES FOR THE',I4,' CORE,',I4,' ACTIVE,',
     *       I4,' EXTERNAL MO-S ARE')
 9070 FORMAT(1X,'    CORE=',10(1X,A4,1X)/(10X,10(1X,A4,1X)))
 9080 FORMAT(1X,'  ACTIVE=',10(1X,A4,1X))
 9100 FORMAT(1X,'EXTERNAL=',10(1X,A4,1X)/(10X,10(1X,A4,1X)))
 9300 FORMAT(/1X,'MOS ARE SYMMETRY CONTAMINATED. NO SPACE',
     *           ' SYMMETRY WILL BE USED IN SOC.',/,
     *           'CSF SYMMETRY ANALYSIS BELOW IS APPROXIMATE',/)
 9400 FORMAT(1X,'SAVING CSFS WITH COEFS LARGER THAN ',1P,E8.2/,
     *       1X,'SAVED CSFS: ',I10,/1X,'TOTAL CSFS: ',I10,/)
      END
C*MODULE TRNSTN  *DECK READCEN
      SUBROUTINE READCEN(IR,IW,NMO,ENERGY,IGOTEN)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL GOPARR,DSKWRK,MASWRK
      DIMENSION ENERGY(NMO)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C     ----- READ THE canonical CAS enegies -----
C     return 1 if successfully read, 0 otherwise in IGOTEN.
C
C     ----- POSITION INPUT TO THE DESIRED $VEC GROUP -----
C
      IGOTEN=0
      CALL SEQREW(IR)
      CALL FNDGRP(IR,' $ENERGY',IEOF)
      IF(IEOF.NE.0) GO TO 400
#if defined(SPEC_CPU)
      IEND = 0
#endif
C
C     ----- READ IN THE ORBITALS -----
C
C MASTER WORK
C
      IF(MASWRK) THEN
C        READ(IR,9010,ERR=300,END=300) (idum,jdum,energy(I),I=1,nmo)
         READ(IR,9010,ERR=300,END=300) (IDUM,ENERGY(I),I=1,NMO)
         IF(IW.LE.0) WRITE(IW,*) IDUM
C        write(6,*) 'www: readd',nmo
C        call prsq(energy,nmo,1,1)
         IF (GOPARR) CALL DDI_BCAST(350,'I',0,1,MASTER)
C
C SLAVE WORK, 0/1/2 MEANS OK/EOF OR OTHER ERROR/BOOBOO IN CONTENTS
C
      ELSE
         IF (GOPARR) CALL DDI_BCAST(350,'I',IEND,1,MASTER)
         IF (IEND.EQ.1.OR.IEND.EQ.2) CALL ABRT
      END IF
C
C  GIVE energies TO ALL PROCESSES
C
      IF (GOPARR) CALL DDI_BCAST(351,'F',ENERGY,NMO,MASTER)
      IGOTEN=1
      RETURN
C
C     ----- BOOBOO OF SOME SORT IN READING THE COEFFS -----
C
  300 CONTINUE
      IF (MASWRK.AND.GOPARR) CALL DDI_BCAST(350,'I',1,1,MASTER)
      CALL ABRT
C
C     ----- THE DESIRED GROUP WAS NOT FOUND -----
C
  400 CONTINUE
C
C     IF (MASWRK) WRITE(IW,3)
C   3 FORMAT(1X,'**** ERROR ***** $ENERGY WAS NOT FOUND')
C     CALL ABRT
C
      RETURN
C
 9010 FORMAT(1X,I4,1X,E23.16)
      END
