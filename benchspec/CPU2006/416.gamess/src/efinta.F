C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C 17 APR 02 - MWS - SYNCH UP EFMULT AND FRGINF COMMON
C 26 OCT 00 - MWS - INTRODUCE MXAO PARAMETER
C 16 FEB 00 - VK  - ENUC*: SKIP OVER BUFFER ATOMS WHEN DOING QM/MM
C 28 SEP 97 - MWS - CONVERT PARALLEL CALLS FROM TCGMSG TO DDI
C 18 DEC 96 - JHJ - EFXINT(X=C,D,O,Q): WRITE INTEGRALS TO DAF 89
C 10 SEP 96 - MWS - INCLUDE THIS CODE IN DISTRIBUTION VERSION
C 24 MAY 96 - WC  - DYNAMIC MEMORY FOR QUAD AND OCT VECTORS
C  9 JAN 96 - WC  - REMOVE EXTRANEOUS OUTPUT
C 23 NOV 94 - MWS - REMOVE ALL FTNCHEK ERRORS
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C 20 NOV 91 - JHJ - EFXINT(X=D,O,Q): REPLACED IA ARRAY
C 24 OCT 91 - JHJ - ENUCQ:ADDED DOQUAD TO 380-LOOP.
C 30 JUL 91 - JHJ - EFXINT,ENUCX:ADDED DOX ARRAYS.
C  8 JUL 91 - JHJ - ADDED EFCINT, FROM STANDV.
C  2 JUL 91 - JHJ - ADDED DRG'S ENUCD,Q,AND O, AND ADDED ENUCC.
C  1 JUL 91 - JHJ - GATHERED INTEGRAL ROUTINES INTO THIS MODULE. ADDED
C                   EFMULT COMMON TO INTERFACE WITH EFINP MODULE.
C  9 MAY 90 - DRG - EFDINT, EFQINT AND EFOINT ADDED.  F CODING LEFT IN
C                   EXCEPT FOR CODING TO COMMON NSHEL.
C*MODULE EFINTA  *DECK EFCINT
      SUBROUTINE EFCINT(QQ,GG)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL IANDJ,OUT,NORM,DOUBLE
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
      LOGICAL GOPARR,MASWRK,DSKWRK
C
      DIMENSION QQ(*),GG(*)
C
      DIMENSION DIJ(225),XIN(125),YIN(125),ZIN(125),
     *          IX(35),IY(35),IZ(35),JX(35),JY(35),JZ(35),
     *          IJX(225),IJY(225),IJZ(225)
      DIMENSION CHCINT(225)
      DIMENSION FIJ(225),GIJ(225)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
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
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
      COMMON /SYMIND/ TOL,II,JJ,LIT,LJT,MINI,MINJ,MAXI,MAXJ,IANDJ
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00,
     *           PI212=1.1283791670955D+00, SQRT3=1.73205080756888D+00,
     *           SQRT5=2.23606797749979D+00, SQRT7=2.64575131106459D+00,
     *           RLN10=2.30258D+00)
C
      DATA JX / 0, 1, 0, 0, 2, 0, 0, 1, 1, 0,
     *          3, 0, 0, 2, 2, 1, 0, 1, 0, 1,
     *          4, 0, 0, 3, 3, 1, 0, 1, 0, 2,
     *          2, 0, 2, 1, 1/
      DATA IX / 1, 6, 1, 1,11, 1, 1, 6, 6, 1,
     *         16, 1, 1,11,11, 6, 1, 6, 1, 6,
     *         21, 1, 1,16,16, 6, 1, 6, 1,11,
     *         11, 1,11, 6, 6/
      DATA JY / 0, 0, 1, 0, 0, 2, 0, 1, 0, 1,
     *          0, 3, 0, 1, 0, 2, 2, 0, 1, 1,
     *          0, 4, 0, 1, 0, 3, 3, 0, 1, 2,
     *          0, 2, 1, 2, 1/
      DATA IY / 1, 1, 6, 1, 1,11, 1, 6, 1, 6,
     *          1,16, 1, 6, 1,11,11, 1, 6, 6,
     *          1,21, 1, 6, 1,16,16, 1, 6,11,
     *          1,11, 6,11, 6/
      DATA JZ / 0, 0, 0, 1, 0, 0, 2, 0, 1, 1,
     *          0, 0, 3, 0, 1, 0, 1, 2, 2, 1,
     *          0, 0, 4, 0, 1, 0, 1, 3, 3, 0,
     *          2, 2, 1, 1, 2/
      DATA IZ / 1, 1, 1, 6, 1, 1,11, 1, 6, 6,
     *          1, 1,16, 1, 6, 1, 6,11,11, 6,
     *          1, 1,21, 1, 6, 1, 6,16,16, 1,
     *         11,11, 6, 6,11/
C
      DATA QLIM/1.00D-08/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DBUGME/8HINT1    /
#else
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA DBUGME_STR/"INT1    "/
#endif
C
C    CALCULATE THE CHARGE-CGARGE CONTRIBUTION TO THE ONE ELECTRON
C    INTEGRALS FOR THE EFFECTIVE FRAGMENT POTENTIALS
C
C     ----- INTIALIZE PARALLEL -----
C
      IPCOUNT = ME - 1
C
      NEFC = NMTTPT
      TOL = RLN10*ITOL
      OUT = NPRINT.EQ.3  .OR.  EXETYP.EQ.DBUGME
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
      L1 = NUM
      L2 = (NUM*NUM+NUM)/2
      CALL VCLR(QQ,1,L2)
C
C     ----- ISHELL
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
C     ----- JSHELL
C
         DO 700 JJ = 1,II
C
C     ----- GO PARALLEL! -----
C
            IF (GOPARR) THEN
               IPCOUNT = IPCOUNT + 1
               IF (MOD(IPCOUNT,NPROC).NE.0) GO TO 690
            END IF
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
            DO 160 I = MINI,MAXI
               NX = IX(I)
               NY = IY(I)
               NZ = IZ(I)
               IF (IANDJ) MAX = I
               DO 140 J = MINJ,MAX
                  IJ = IJ+1
                  IJX(IJ) = NX+JX(J)
                  IJY(IJ) = NY+JY(J)
                  IJZ(IJ) = NZ+JZ(J)
  140          CONTINUE
  160       CONTINUE
            DO 180 I = 1,IJ
               CHCINT(I) = ZERO
  180       CONTINUE
C
C     ----- I PRIMITIVE
C
            JGMAX = J2
            DO 520 IG = I1,I2
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
C
C     ----- J PRIMITIVE
C
               IF (IANDJ) JGMAX = IG
               DO 500 JG = J1,JGMAX
                  AJ = EX(JG)
                  AA = AI+AJ
                  AA1 = ONE/AA
                  DUM = AJ*ARRI*AA1
                  IF (DUM .GT. TOL) GO TO 500
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
                  DOUBLE=IANDJ.AND.IG.NE.JG
                  MAX = MAXJ
                  NN = 0
                  DUM1 = ZERO
                  DUM2 = ZERO
                  DO 220 I = MINI,MAXI
                     IF (I.EQ.1) DUM1=CSI*FAC
                     IF (I.EQ.2) DUM1=CPI*FAC
                     IF (I.EQ.5) DUM1=CDI*FAC
                     IF ((I.EQ.8).AND.NORM) DUM1=DUM1*SQRT3
                     IF (I.EQ.11) DUM1=CFI*FAC
                     IF ((I.EQ.14).AND.NORM) DUM1=DUM1*SQRT5
                     IF ((I.EQ.20).AND.NORM) DUM1=DUM1*SQRT3
                     IF (I.EQ.21) DUM1=CGI*FAC
                     IF ((I.EQ.24).AND.NORM) DUM1=DUM1*SQRT7
                     IF ((I.EQ.30).AND.NORM) DUM1=DUM1*SQRT5/SQRT3
                     IF ((I.EQ.33).AND.NORM) DUM1=DUM1*SQRT3
                     IF (IANDJ) MAX = I
                     DO 200 J = MINJ,MAX
                        IF (J.EQ.1) THEN
                           DUM2=DUM1*CSJ
                           IF (DOUBLE) THEN
                              IF (I.LE.1) THEN
                                 DUM2=DUM2+DUM2
                              ELSE
                                 DUM2=DUM2+CSI*CPJ*FAC
                              END IF
                           END IF
                        ELSE IF (J.EQ.2) THEN
                           DUM2=DUM1*CPJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF (J.EQ.5) THEN
                           DUM2=DUM1*CDJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((J.EQ.8).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        ELSE IF (J.EQ.11) THEN
                           DUM2=DUM1*CFJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((J.EQ.14).AND.NORM) THEN
                           DUM2=DUM2*SQRT5
                        ELSE IF ((J.EQ.20).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        ELSE IF (J.EQ.21) THEN
                           DUM2=DUM1*CGJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((J.EQ.24).AND.NORM) THEN
                           DUM2=DUM2*SQRT7
                        ELSE IF ((J.EQ.30).AND.NORM) THEN
                           DUM2=DUM2*SQRT5/SQRT3
                        ELSE IF ((J.EQ.33).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        END IF
                        NN = NN+1
                        DIJ(NN) = DUM2
  200                CONTINUE
  220             CONTINUE
C
C     EFFECTIVE POINT CHARGES INTEGRALS.
C
                  DUM = PI212*AA1
                  DO 400 I=1,IJ
                     FIJ(I) = DIJ(I)*DUM
  400             CONTINUE
                  AAX = AA*AX
                  AAY = AA*AY
                  AAZ = AA*AZ
                  DO 492 IC = 1,NEFC
                     IF(.NOT.DOMONO(IC))GO TO 492
                     ZNUC = -(EFCHG(1,IC)+EFCHG(2,IC))
                     CX = EFC(1,IC)
                     CY = EFC(2,IC)
                     CZ = EFC(3,IC)
                     XX = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
                     IF (NROOTS.LE.3) CALL RT123
                     IF (NROOTS.EQ.4) CALL ROOT4
                     IF (NROOTS.EQ.5) CALL ROOT5
                     MM = 0
                     DO 477 K = 1,NROOTS
                        UU = AA*U(K)
                        WW = W(K)*ZNUC
                        TT = ONE/(AA+UU)
                        T = SQRT(TT)
                        X0 = (AAX+UU*CX)*TT
                        Y0 = (AAY+UU*CY)*TT
                        Z0 = (AAZ+UU*CZ)*TT
                        IN = -5+MM
                        DO 476 I = 1,LIT
                           IN = IN+5
                           NI = I
                           DO 475 J = 1,LJT
                              JN = IN+J
                              NJ = J
                              CALL STVINT
                              XIN(JN) = XINT
                              YIN(JN) = YINT
                              ZIN(JN) = ZINT*WW
  475                      CONTINUE
  476                   CONTINUE
                        MM = MM+25
  477                CONTINUE
                     DO 481 I = 1,IJ
                        NX = IJX(I)
                        NY = IJY(I)
                        NZ = IJZ(I)
                        DUM = ZERO
                        MM = 0
                        DO 479 K = 1,NROOTS
                           DUM = DUM+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM)
                           MM = MM+25
  479                   CONTINUE
                        CHCINT(I) = CHCINT(I)+DUM*FIJ(I)
  481                CONTINUE
C
C ---- SUBTRACT DAMPING FUNCTION TERM ----
C
                     ALFA = EFATRM(IC)
                     BETA = EFBTRM(IC)
                     IF(ABS(ALFA).LE.QLIM)GO TO 492
                     DUM = PI212/(AA+ALFA)
                     DO 482 I=1,IJ
                        GIJ(I) = DIJ(I) * DUM
  482                CONTINUE
                     ZNUC = -EFCHG(1,IC)
                     PCSQ = ((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
                     XX = AA*AA*PCSQ/(AA+ALFA)
                     PREI = EXP(-AA*ALFA*PCSQ/(AA+ALFA))
                     IF (NROOTS.LE.3) CALL RT123
                     IF (NROOTS.EQ.4) CALL ROOT4
                     IF (NROOTS.EQ.5) CALL ROOT5
                     MM = 0
                     DO 485 K = 1,NROOTS
                        UU = (AA+ALFA)*U(K)
                        WW = W(K)*ZNUC
                        TT = ONE/(AA+UU+ALFA)
                        T = SQRT(TT)
                        X0 = (AAX+(UU+ALFA)*CX)*TT
                        Y0 = (AAY+(UU+ALFA)*CY)*TT
                        Z0 = (AAZ+(UU+ALFA)*CZ)*TT
                        IN = -5+MM
                        DO 484 I = 1,LIT
                           IN = IN+5
                           NI = I
                           DO 483 J = 1,LJT
                              JN = IN+J
                              NJ = J
                              CALL STVINT
                              XIN(JN) = XINT
                              YIN(JN) = YINT
                              ZIN(JN) = ZINT*WW
  483                      CONTINUE
  484                   CONTINUE
                        MM = MM+25
  485                CONTINUE
                     DO 489 I = 1,IJ
                        NX = IJX(I)
                        NY = IJY(I)
                        NZ = IJZ(I)
                        DUM = ZERO
                        MM = 0
                        DO 487 K = 1,NROOTS
                           DUM = DUM+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM)
                           MM = MM+25
  487                   CONTINUE
                        DAMPT = -GIJ(I) * PREI * BETA * DUM
                        CHCINT(I) = CHCINT(I) + DAMPT
  489                CONTINUE
  492             CONTINUE
C
C     ----- END PRIMITIVES -----
C
  500          CONTINUE
  520       CONTINUE
C
            MAX = MAXJ
            NN = 0
            DO 620 I = MINI,MAXI
               LI = LOCI+I
               IN = (LI*(LI-1))/2
               IF (IANDJ) MAX = I
               DO 600 J = MINJ,MAX
                  LJ = LOCJ+J
                  JN = LJ+IN
                  NN = NN+1
                  QQ(JN) = QQ(JN) + CHCINT(NN)
  600          CONTINUE
  620       CONTINUE
C
C     ----- END PARALLEL
C
  690    CONTINUE
C
C     ----- END SHELLS -----
C
  700    CONTINUE
  720 CONTINUE
C
C     ----- SUM UP PARTIAL CONTRIBUTIONS IF PARALLEL -----
C
      IF (GOPARR) CALL DDI_GSUMF(920,QQ,L2)
C
      CALL DAREAD(IDAF,IODA,GG,L2,11,0)
      CALL VADD(GG,1,QQ,1,GG,1,L2)
      CALL DAWRIT(IDAF,IODA,GG,L2,11,0)
C
      CALL DAREAD(IDAF,IODA,GG,L2,89,0)
      CALL VADD(GG,1,QQ,1,GG,1,L2)
      CALL DAWRIT(IDAF,IODA,GG,L2,89,0)
C
C     ----- DONE WITH INTEGRALS -----
C
      IF(MASWRK.AND.OUT) THEN
         WRITE(IW,*) 'EFC INTEGRALS'
         CALL PRTRIL(QQ,L1)
         WRITE(IW,9100)
         CALL TIMIT(1)
      END IF
      RETURN
C
 9100 FORMAT(1X,'...... END OF EFC INTEGRALS ......')
      END
C*MODULE EFINTA  *DECK EFDINT
      SUBROUTINE EFDINT(QQ,GG)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL GOPARR,MASWRK,DSKWRK
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
      LOGICAL IANDJ,NORM,DOUBLE
      LOGICAL OUT
C
      DIMENSION QQ(*),GG(*)
C
      DIMENSION CHDINT(100)
      DIMENSION DIJ(100),XIN(432),YIN(432),ZIN(432)
      DIMENSION FIJ(100),GIJ(100)
      DIMENSION IX(20),IY(20),IZ(20),JX(20),JY(20),JZ(20)
      DIMENSION IJX(100),IJY(100),IJZ(100)
C
      PARAMETER (MXSH=1000, MXGTOT=5000)
      PARAMETER (MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /CSSTV / CX,CY,CZ
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
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
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
C
      DATA PI212 /1.1283791670955D+00/
      DATA SQRT3 /1.73205080756888D+00/
      DATA SQRT5 /2.23606797749979D+00/
      DATA JX / 0, 1, 0, 0, 2, 0, 0, 1, 1, 0,
     1          3, 0, 0, 2, 2, 1, 0, 1, 0, 1/
      DATA JY / 0, 0, 1, 0, 0, 2, 0, 1, 0, 1,
     1          0, 3, 0, 1, 0, 2, 2, 0, 1, 1/
      DATA JZ / 0, 0, 0, 1, 0, 0, 2, 0, 1, 1,
     1          0, 0, 3, 0, 1, 0, 1, 2, 2, 1/
      DATA IX / 1, 5, 1, 1, 9, 1, 1, 5, 5, 1,
     1         13, 1, 1, 9, 9, 5, 1, 5, 1, 5/
      DATA IY / 1, 1, 5, 1, 1, 9, 1, 5, 1, 5,
     1          1,13, 1, 5, 1, 9, 9, 1, 5, 5/
      DATA IZ / 1, 1, 1, 5, 1, 1, 9, 1, 5, 5,
     1          1, 1,13, 1, 5, 1, 5, 9, 9, 5/
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DBUGME/8HINT1    /
#else
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA DBUGME_STR/"INT1    "/
#endif
      DATA ZERO,ONE/0.0D+00,1.0D+00/
      DATA RLN10 /2.30258D+00/
      DATA QLIM /1.D-07/
C
C     ----- INTIALIZE PARALLEL -----
C
      IPCOUNT = ME - 1
      NEFD = NMTTPT
      OUT = NPRINT.EQ.3  .OR.  EXETYP.EQ.DBUGME
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
      NUM2=(NUM*(NUM+1))/2
      DO 10 I=1,NUM2
      QQ(I) = ZERO
 10   CONTINUE
C
C     ----- ISHELL
C
      DO 600 II = 1,NSHELL
      IAT= KATOM(II)
      XI = C(1,IAT)
      YI = C(2,IAT)
      ZI = C(3,IAT)
      I1 = KSTART(II)
      I2 = I1+KNG(II)-1
      LIT = KTYPE(II)
      MINI = KMIN(II)
      MAXI = KMAX(II)
      LOCI = KLOC(II)-MINI
C
C     ----- JSHELL
C
      DO 580 JJ = 1,II
C
C     ----- GO PARALLEL! -----
C
      IF (GOPARR) THEN
         IPCOUNT = IPCOUNT + 1
         IF (MOD(IPCOUNT,NPROC).NE.0) GO TO 570
      END IF
      JAT= KATOM(JJ)
      XJ = C(1,JAT)
      YJ = C(2,JAT)
      ZJ = C(3,JAT)
      J1 = KSTART(JJ)
      J2 = J1+KNG(JJ)-1
      LJT = KTYPE(JJ)
      MINJ = KMIN(JJ)
      MAXJ = KMAX(JJ)
      LOCJ = KLOC(JJ)-MINJ
      NROOTS = (LIT+LJT+1-2)/2 + 1
      RR = (XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
      IANDJ = II .EQ. JJ
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      IJ = 0
      MAX = MAXJ
      DO 50 I = MINI,MAXI
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
      IF (IANDJ) MAX = I
      DO 50 J = MINJ,MAX
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
  50  CONTINUE
      DO 60 I=1,IJ
  60  CHDINT(I) = ZERO
C
C     ----- I PRIMITIVE
C
      JGMAX = J2
      DO 520 IG = I1,I2
      AI = EX(IG)
      ARRI = AI*RR
      AXI = AI*XI
      AYI = AI*YI
      AZI = AI*ZI
      CSI = CS(IG)
      CPI = CP(IG)
      CDI = CD(IG)
      CFI=CF(IG)
C
C
C     ----- J PRIMTIVE
C
      IF (IANDJ) JGMAX = IG
      DO 500 JG = J1,JGMAX
      AJ = EX(JG)
      AA = AI+AJ
      AA1 = ONE/AA
      DUM = AJ*ARRI*AA1
      IF (DUM .GT. TOL) GO TO 500
      FAC =  EXP(-DUM)
      CSJ = CS(JG)
      CPJ = CP(JG)
      CDJ = CD(JG)
      CFJ=CF(JG)
      AX = (AXI+AJ*XJ)*AA1
      AY = (AYI+AJ*YJ)*AA1
      AZ = (AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR
C
      DOUBLE=IANDJ.AND.IG.NE.JG
      MAX = MAXJ
      NN = 0
      DO 170 I=MINI,MAXI
      GO TO (70,80,110,110,90,110,110,100,110,110,
     1       102,110,110,104,110,110,110,110,110,106),I
   70 DUM1=CSI*FAC
      GO TO 110
   80 DUM1=CPI*FAC
      GO TO 110
   90 DUM1=CDI*FAC
      GO TO 110
  100 IF(NORM) DUM1=DUM1*SQRT3
      GO TO 110
  102 DUM1=CFI*FAC
      GO TO 110
  104 IF(NORM) DUM1=DUM1*SQRT5
      GO TO 110
  106 IF(NORM) DUM1=DUM1*SQRT3
  110 IF(IANDJ) MAX=I
      DO 170 J=MINJ,MAX
      GO TO (125,130,160,160,140,160,160,150,160,160,
     1       152,160,160,154,160,160,160,160,160,156),J
  125 DUM2=DUM1*CSJ
      IF(.NOT.DOUBLE) GO TO 160
      IF(I.GT.1) GO TO 126
      DUM2=DUM2+DUM2
      GO TO 160
  126 DUM2=DUM2+CSI*CPJ*FAC
      GO TO 160
  130 DUM2=DUM1*CPJ
      IF(DOUBLE) DUM2=DUM2+DUM2
      GO TO 160
  140 DUM2=DUM1*CDJ
      IF(DOUBLE) DUM2=DUM2+DUM2
      GO TO 160
  150 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 160
  152 DUM2=DUM1*CFJ
      IF(DOUBLE) DUM2=DUM2+DUM2
      GO TO 160
  154 IF(NORM) DUM2=DUM2*SQRT5
      GO TO 160
  156 IF(NORM) DUM2=DUM2*SQRT3
  160 NN=NN+1
  170 DIJ(NN)=DUM2
C
C     ..... HELLMANN-FEYNMAN TERM .....
C
      DUM = PI212*AA1
      DUM=DUM+DUM
      DO 380 I = 1,IJ
      FIJ(I) = DIJ(I)*DUM
  380 CONTINUE
      AAX = AA*AX
      AAY = AA*AY
      AAZ = AA*AZ
C
      DO 480 IC = 1,NEFD
      IF(.NOT.DODIPO(IC))GO TO 480
      ALFA = EFATRM(IC)
      BETA = EFBTRM(IC)
      CX   = EFC(1,IC)
      CY   = EFC(2,IC)
      CZ   = EFC(3,IC)
      XMU=EFDIP(1,IC)
      YMU=EFDIP(2,IC)
      ZMU=EFDIP(3,IC)
      DUMA = PI212/(AA+ALFA)
      DUMA=DUMA+DUMA
      DO 385 I = 1,IJ
      GIJ(I) = DIJ(I)*DUMA
  385 CONTINUE
      XX   = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
      IF(NROOTS .LE. 3) CALL RT123
      IF(NROOTS .EQ. 4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         IF (MASWRK) WRITE(IW,9008)
         CALL ABRT
      END IF
      MM = 0
      DO 401 K = 1,NROOTS
      UU = AA*U(K)
      WW = W(K)
      WW=WW*UU
      TT = ONE/(AA+UU)
      T  =  SQRT(TT)
      X0 = (AAX+UU*CX)*TT
      Y0 = (AAY+UU*CY)*TT
      Z0 = (AAZ+UU*CZ)*TT
      IN = -4+MM
      DO 400 I = 1,LIT
      IN = IN+4
      NI = I
      DO 400 J = 1,LJT
      JN = IN+J
      NJ = J
      CALL STVINT
      XIN(JN   ) = XINT
      YIN(JN   ) = YINT
      ZIN(JN   ) = ZINT*WW
      CALL POLXYZ
      XIN(JN+125) = XINT
      YIN(JN+125) = YINT
      ZIN(JN+125) = ZINT*WW
  400 CONTINUE
  401 MM = MM+16
      DO 403 I = 1,IJ
      NX    = IJX(I)
      NY    = IJY(I)
      NZ    = IJZ(I)
      DUMX = ZERO
      DUMY = ZERO
      DUMZ = ZERO
      MM    = 0
      DO 402 K = 1,NROOTS
      DUMX= DUMX+XIN(NX+MM+125)*YIN(NY+MM    )*ZIN(NZ+MM    )
      DUMY= DUMY+XIN(NX+MM    )*YIN(NY+MM+125)*ZIN(NZ+MM    )
      DUMZ= DUMZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+125)
  402 MM  = MM+16
      DUM = FIJ(I)
      CHDINT(I)=CHDINT(I)-DUM*(DUMX*XMU+DUMY*YMU+DUMZ*ZMU)
 403  CONTINUE
C
C  SUBTRACT DAMPING FUNCTION TERM
C
      IF ( ABS(ALFA).LE.QLIM ) GO TO 480
      PCSQ = (AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2
      XX=AA*AA*PCSQ/(AA+ALFA)
      PREI = EXP(-AA*ALFA*PCSQ/(AA+ALFA))
      IF(NROOTS .LE. 3) CALL RT123
      IF(NROOTS .EQ. 4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         IF (MASWRK) WRITE(IW,9008)
         CALL ABRT
      END IF
      MM = 0
      DO 425 K = 1,NROOTS
      UU = (ALFA+AA)*U(K)
      WW = W(K)
      WW = WW*UU
      TT = ONE/(AA+UU+ALFA)
      T  =  SQRT(TT)
      X0 = (AAX+(UU+ALFA)*CX)*TT
      Y0 = (AAY+(UU+ALFA)*CY)*TT
      Z0 = (AAZ+(UU+ALFA)*CZ)*TT
      IN = -4+MM
      DO 405 I = 1,LIT
      IN = IN+4
      NI = I
      DO 405 J = 1,LJT
      JN = IN+J
      NJ = J
      CALL STVINT
      XIN(JN   ) = XINT
      YIN(JN   ) = YINT
      ZIN(JN   ) = ZINT*WW
      CALL POLXYZ
      XIN(JN+125) = XINT
      YIN(JN+125) = YINT
      ZIN(JN+125) = ZINT*WW
  405 CONTINUE
  425 MM = MM+16
      DO 465 I = 1,IJ
      NX    = IJX(I)
      NY    = IJY(I)
      NZ    = IJZ(I)
      DUMX = ZERO
      DUMY = ZERO
      DUMZ = ZERO
      MM    = 0
      DO 445 K = 1,NROOTS
      DUMX= DUMX+XIN(NX+MM+125)*YIN(NY+MM    )*ZIN(NZ+MM    )
      DUMY= DUMY+XIN(NX+MM    )*YIN(NY+MM+125)*ZIN(NZ+MM    )
      DUMZ= DUMZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+125)
  445 MM  = MM+16
      DUM =-GIJ(I)*PREI * BETA
      CHDINT(I)=CHDINT(I)-DUM*(DUMX*XMU+DUMY*YMU+DUMZ*ZMU)
 465  CONTINUE
C
 480  CONTINUE
C-------- END OF EFD CENTERS LOOP
 500  CONTINUE
 520  CONTINUE
C
C     ----- END OF *PRIMITIVE* LOOPS -----
C     ----- SET QQ MATRIX
C
      MAX=MAXJ
      NN=0
      DO 550 I=MINI,MAXI
      LI=LOCI+I
      IN = (LI*(LI-1))/2
      IF(IANDJ) MAX=I
      DO 550 J=MINJ,MAX
      LJ=LOCJ+J
      JN=LJ+IN
      NN=NN+1
      QQ(JN)=QQ(JN) + CHDINT(NN)
  550 CONTINUE
C
C     ----- END PARALLEL
C
  570    CONTINUE
C
C     ----- END SHELLS -----
C
  580 CONTINUE
  600 CONTINUE
C
C     ----- END OF *SHELL* LOOPS -----
C
C     ----- SUM UP PARTIAL CONTRIBUTIONS IF PARALLEL -----
C
      IF (GOPARR) CALL DDI_GSUMF(921,QQ,NUM2)
C
      CALL DAREAD(IDAF,IODA,GG,NUM2,11,0)
      CALL VADD(GG,1,QQ,1,GG,1,NUM2)
      CALL DAWRIT(IDAF,IODA,GG,NUM2,11,NAV)
C
      CALL DAREAD(IDAF,IODA,GG,NUM2,89,0)
      CALL VADD(GG,1,QQ,1,GG,1,NUM2)
      CALL DAWRIT(IDAF,IODA,GG,NUM2,89,0)
C
C     ----- PRINTING SECTION ----
C
      IF(MASWRK.AND.OUT) THEN
         WRITE(6,9380)
         CALL PRTRIL(QQ,NUM)
         WRITE(6,9128)
         CALL TIMIT(1)
      END IF
      RETURN
C
 9008 FORMAT(/' NUMBER OF POLYNOMIAL ROOTS NEEDED (NROOTS) IS GREATER',
     *        ' THAN 6 IN EFDINT.  CALL A PROGRAMMER/QUANTUM CHEMIST.')
 9128 FORMAT(/,' ...... END OF CHARGE DIPOLE INTEGRALS....')
 9380 FORMAT(10X,14("-"),/,10X,"   EFC  MATRIX",' (CHARGE DIPOLE)',
     * /,10X,14(1H-))
      END
C*MODULE EFINTA  *DECK EFOINT
      SUBROUTINE EFOINT(QQ,GG)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
      COMMON /FMCOM / XX(1)
C
      CALL VALFM(LOADFM)
      NEED=10*MXFGPT
      CALL GETFM(NEED)
      CALL EFOIT(QQ,GG,XX(LOADFM+1))
      CALL RETFM(NEED)
C
      RETURN
      END
C*MODULE EFINTA  *DECK EFOIT
      SUBROUTINE EFOIT(QQ,GG,OCT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL IANDJ,NORM,DOUBLE
      LOGICAL OUT
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
      LOGICAL GOPARR,MASWRK,DSKWRK
C
      PARAMETER (MXSH=1000, MXGTOT=5000)
      PARAMETER (MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /CSSTV / CX,CY,CZ
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
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
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
C
      DIMENSION OCT(10,*),CHOINT(100),QQ(1),GG(1)
      DIMENSION DIJ(100),XIN(576),YIN(576),ZIN(576)
      DIMENSION FIJ(100),GIJ(100)
      DIMENSION IX(20),IY(20),IZ(20),JX(20),JY(20),JZ(20)
      DIMENSION IJX(100),IJY(100),IJZ(100),W4(9),W6(9)
C
      DATA PI212 /1.1283791670955D+00/
      DATA SQRT3 /1.73205080756888D+00/
      DATA SQRT5 /2.23606797749979D+00/
      DATA JX / 0, 1, 0, 0, 2, 0, 0, 1, 1, 0,
     1          3, 0, 0, 2, 2, 1, 0, 1, 0, 1/
      DATA JY / 0, 0, 1, 0, 0, 2, 0, 1, 0, 1,
     1          0, 3, 0, 1, 0, 2, 2, 0, 1, 1/
      DATA JZ / 0, 0, 0, 1, 0, 0, 2, 0, 1, 1,
     1          0, 0, 3, 0, 1, 0, 1, 2, 2, 1/
      DATA IX / 1, 5, 1, 1, 9, 1, 1, 5, 5, 1,
     1         13, 1, 1, 9, 9, 5, 1, 5, 1, 5/
      DATA IY / 1, 1, 5, 1, 1, 9, 1, 5, 1, 5,
     1          1,13, 1, 5, 1, 9, 9, 1, 5, 5/
      DATA IZ / 1, 1, 1, 5, 1, 1, 9, 1, 5, 5,
     1          1, 1,13, 1, 5, 1, 5, 9, 9, 5/
C
      DATA ZERO,ONE,FOUR/0.0D+00,1.0D+00,4.0D+00/
      DATA PT5,THREE /0.5D+00,3.D+00/
      DATA FIVE,SIX,EIGHT/5.D+00,6.D+00,8.D+00/
      DATA RLN10 /2.30258D+00/
      DATA QLIM /1.D-07/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DBUGME/8HINT1    /
#else
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA DBUGME_STR/"INT1    "/
#endif
C
C     ----- INTIALIZE PARALLEL -----
C
      IPCOUNT = ME - 1
      NEFO = NMTTPT
      OUT = NPRINT.EQ.3  .OR.  EXETYP.EQ.DBUGME
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
      NUM2=(NUM*(NUM+1))/2
      DO 30 I=1,NUM2
      QQ(I) = ZERO
  30  CONTINUE
C
C  REPLACE THIRD MOMENTS IN EFOCT WITH ELECTRIC OCTUPOLE TENSORS
C
      DO 40 I=1,NEFO
         IF(.NOT.DOOCTU(I))GO TO 40
         XXX = PT5*EFOCT(1,I)
         YYY = PT5*EFOCT(2,I)
         ZZZ = PT5*EFOCT(3,I)
         XXY = PT5*EFOCT(4,I)
         XXZ = PT5*EFOCT(5,I)
         XYY = PT5*EFOCT(6,I)
         YYZ = PT5*EFOCT(7,I)
         XZZ = PT5*EFOCT(8,I)
         YZZ = PT5*EFOCT(9,I)
         XYZ = PT5*EFOCT(10,I)
         DUMX = XXX + XYY + XZZ
         DUMY = XXY + YYY + YZZ
         DUMZ = XXZ + YYZ + ZZZ
         OCT(1,I) = FIVE * XXX - THREE * DUMX
         OCT(2,I) = FIVE * YYY - THREE * DUMY
         OCT(3,I) = FIVE * ZZZ - THREE * DUMZ
         OCT(4,I) = FIVE * XXY - DUMY
         OCT(5,I) = FIVE * XXZ - DUMZ
         OCT(6,I) = FIVE * XYY - DUMX
         OCT(7,I) = FIVE * YYZ - DUMZ
         OCT(8,I) = FIVE * XZZ - DUMX
         OCT(9,I) = FIVE * YZZ - DUMY
         OCT(10,I) = FIVE * XYZ
 40   CONTINUE
C
C     ----- ISHELL
C
      DO 600 II = 1,NSHELL
      IAT= KATOM(II)
      XI = C(1,IAT)
      YI = C(2,IAT)
      ZI = C(3,IAT)
      I1 = KSTART(II)
      I2 = I1+KNG(II)-1
      LIT = KTYPE(II)
      MINI = KMIN(II)
      MAXI = KMAX(II)
      LOCI = KLOC(II)-MINI
C
C     ----- JSHELL
C
      DO 580 JJ = 1,II
C
C     ----- GO PARALLEL! -----
C
      IF (GOPARR) THEN
         IPCOUNT = IPCOUNT + 1
         IF (MOD(IPCOUNT,NPROC).NE.0) GO TO 570
      END IF
      JAT= KATOM(JJ)
      XJ = C(1,JAT)
      YJ = C(2,JAT)
      ZJ = C(3,JAT)
      J1 = KSTART(JJ)
      J2 = J1+KNG(JJ)-1
      LJT = KTYPE(JJ)
      MINJ = KMIN(JJ)
      MAXJ = KMAX(JJ)
      LOCJ = KLOC(JJ)-MINJ
      NROOTS = (LIT+LJT+3-2)/2 + 1
      RR = (XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
      IANDJ = II .EQ. JJ
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      IJ = 0
      MAX = MAXJ
      DO 50 I = MINI,MAXI
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
      IF (IANDJ) MAX = I
      DO 50 J = MINJ,MAX
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
  50  CONTINUE
      DO 60 I=1,IJ
  60  CHOINT(I) = ZERO
C
C     ----- I PRIMITIVE
C
      JGMAX = J2
      DO 520 IG = I1,I2
      AI = EX(IG)
      ARRI = AI*RR
      AXI = AI*XI
      AYI = AI*YI
      AZI = AI*ZI
      CSI = CS(IG)
      CPI = CP(IG)
      CDI = CD(IG)
      CFI=CF(IG)
C
C
C     ----- J PRIMTIVE
C
      IF (IANDJ) JGMAX = IG
      DO 500 JG = J1,JGMAX
      AJ = EX(JG)
      AA = AI+AJ
      AA1 = ONE/AA
      DUM = AJ*ARRI*AA1
      IF (DUM .GT. TOL) GO TO 500
      FAC =  EXP(-DUM)
      CSJ = CS(JG)
      CPJ = CP(JG)
      CDJ = CD(JG)
      CFJ=CF(JG)
      AX = (AXI+AJ*XJ)*AA1
      AY = (AYI+AJ*YJ)*AA1
      AZ = (AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR
C
      DOUBLE=IANDJ.AND.IG.NE.JG
      MAX = MAXJ
      NN = 0
      DO 170 I=MINI,MAXI
      GO TO (70,80,110,110,90,110,110,100,110,110,
     1       102,110,110,104,110,110,110,110,110,106),I
   70 DUM1=CSI*FAC
      GO TO 110
   80 DUM1=CPI*FAC
      GO TO 110
   90 DUM1=CDI*FAC
      GO TO 110
  100 IF(NORM) DUM1=DUM1*SQRT3
      GO TO 110
  102 DUM1=CFI*FAC
      GO TO 110
  104 IF(NORM) DUM1=DUM1*SQRT5
      GO TO 110
  106 IF(NORM) DUM1=DUM1*SQRT3
  110 IF(IANDJ) MAX=I
      DO 170 J=MINJ,MAX
      GO TO (125,130,160,160,140,160,160,150,160,160,
     1       152,160,160,154,160,160,160,160,160,156),J
  125 DUM2=DUM1*CSJ
      IF(.NOT.DOUBLE) GO TO 160
      IF(I.GT.1) GO TO 126
      DUM2=DUM2+DUM2
      GO TO 160
  126 DUM2=DUM2+CSI*CPJ*FAC
      GO TO 160
  130 DUM2=DUM1*CPJ
      IF(DOUBLE) DUM2=DUM2+DUM2
      GO TO 160
  140 DUM2=DUM1*CDJ
      IF(DOUBLE) DUM2=DUM2+DUM2
      GO TO 160
  150 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 160
  152 DUM2=DUM1*CFJ
      IF(DOUBLE) DUM2=DUM2+DUM2
      GO TO 160
  154 IF(NORM) DUM2=DUM2*SQRT5
      GO TO 160
  156 IF(NORM) DUM2=DUM2*SQRT3
  160 NN=NN+1
  170 DIJ(NN)=DUM2
C
C     ..... THIRD DERIVATIVE TERM .....
C     BEWARE BUCKINGHAM QUART. REV.(LONDON) -13-,183(1959)
C     A THREE IN EQU. 15 WAS ABSORBED INTO A FIFTEEN TO GIVE FIVE
C     IN EQU. 16
C
      DUM = PI212*AA1/(THREE*FIVE)
      DO 380 I = 1,IJ
      FIJ(I) = DIJ(I)*DUM
  380 CONTINUE
      AAX = AA*AX
      AAY = AA*AY
      AAZ = AA*AZ
C
      DO 480 IC = 1,NEFO
      IF(.NOT.DOOCTU(IC))GO TO 480
      ALFA = EFATRM(IC)
      BETA = EFBTRM(IC)
      CX   = EFC(1,IC)
      CY   = EFC(2,IC)
      CZ   = EFC(3,IC)
      DUM = PI212/(THREE*FIVE*(AA+ALFA))
      DO 385 I = 1,IJ
      GIJ(I) = DIJ(I)*DUM
  385 CONTINUE
      XX   = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
      IF(NROOTS .LE. 3) CALL RT123
      IF(NROOTS .EQ. 4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         IF (MASWRK) WRITE(IW,9008)
         CALL ABRT
      END IF
      MM = 0
      DO 402 K = 1,NROOTS
      U2 = AA*U(K)
      U4 = U2*U2
      U6 = U4*U2
      WW = W(K)
      W4(K) = -FOUR*WW*U4
      W6(K) = -EIGHT*WW*U6
      TT = ONE/(AA+U2)
      T  =  SQRT(TT)
      X0 = (AAX+U2*CX)*TT
      Y0 = (AAY+U2*CY)*TT
      Z0 = (AAZ+U2*CZ)*TT
      IN = -4+MM
      DO 400 I = 1,LIT
      IN = IN+4
      NI = I
      DO 400 J = 1,LJT
      JN = IN+J
      NJ = J
      CALL STVINT
      XIN(JN   ) = XINT
      YIN(JN   ) = YINT
      ZIN(JN   ) = ZINT
      CALL POLXYZ
      XIN(JN+144) = XINT
      YIN(JN+144) = YINT
      ZIN(JN+144) = ZINT
      CALL EFQXYZ
      XIN(JN+288) = XINT
      YIN(JN+288) = YINT
      ZIN(JN+288) = ZINT
      CALL EFOXYZ
      XIN(JN+432) = XINT
      YIN(JN+432) = YINT
      ZIN(JN+432) = ZINT
  400 CONTINUE
  402 MM = MM+16
      DO 404 I = 1,IJ
      NX    = IJX(I)
      NY    = IJY(I)
      NZ    = IJZ(I)
      DXXX = ZERO
      DYYY = ZERO
      DZZZ = ZERO
      DXXY = ZERO
      DXXZ = ZERO
      DXYY = ZERO
      DYYZ = ZERO
      DXZZ = ZERO
      DYZZ = ZERO
      DXYZ = ZERO
      MM    = 0
      DO 403 K = 1,NROOTS
      DXXX=DXXX+XIN(NX+MM+144)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)*THREE
     1         -XIN(NX+MM+432)*YIN(NY+MM    )*ZIN(NZ+MM    )*W6(K)
      DYYY=DYYY+XIN(NX+MM    )*YIN(NY+MM+144)*ZIN(NZ+MM    )*W4(K)*THREE
     1         -XIN(NX+MM    )*YIN(NY+MM+432)*ZIN(NZ+MM    )*W6(K)
      DZZZ=DZZZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+144)*W4(K)*THREE
     1         -XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+432)*W6(K)
      DXXY=DXXY+XIN(NX+MM    )*YIN(NY+MM+144)*ZIN(NZ+MM    )*W4(K)
     1         -XIN(NX+MM+288)*YIN(NY+MM+144)*ZIN(NZ+MM    )*W6(K)
      DXXZ=DXXZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+144)*W4(K)
     1         -XIN(NX+MM+288)*YIN(NY+MM    )*ZIN(NZ+MM+144)*W6(K)
      DXYY=DXYY+XIN(NX+MM+144)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)
     1         -XIN(NX+MM+144)*YIN(NY+MM+288)*ZIN(NZ+MM    )*W6(K)
      DYYZ=DYYZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+144)*W4(K)
     1         -XIN(NX+MM    )*YIN(NY+MM+288)*ZIN(NZ+MM+144)*W6(K)
      DXZZ=DXZZ+XIN(NX+MM+144)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)
     1         -XIN(NX+MM+144)*YIN(NY+MM    )*ZIN(NZ+MM+288)*W6(K)
      DYZZ=DYZZ+XIN(NX+MM    )*YIN(NY+MM+144)*ZIN(NZ+MM    )*W4(K)
     1         -XIN(NX+MM    )*YIN(NY+MM+144)*ZIN(NZ+MM+288)*W6(K)
      DXYZ=DXYZ-XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W6(K)
  403 MM  = MM+16
      DUM = FIJ(I)
      CHOINT(I) = CHOINT(I) -
     1     DUM * ( DXXX * OCT(1,IC)
     1           + DYYY * OCT(2,IC)
     1           + DZZZ * OCT(3,IC)
     1 + THREE * ( DXXY * OCT(4,IC)
     1           + DXXZ * OCT(5,IC)
     1           + DXYY * OCT(6,IC)
     1           + DYYZ * OCT(7,IC)
     1           + DXZZ * OCT(8,IC)
     1           + DYZZ * OCT(9,IC) )
     1 +   SIX * ( DXYZ * OCT(10,IC) ) )
 404  CONTINUE
C
C  SUBTRACT DAMPING FUNCTION TERM
C
      IF ( ABS(ALFA).LE.QLIM ) GO TO 480
      PCSQ = (AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2
      XX=AA*AA*PCSQ/(AA+ALFA)
      PREI = EXP(-AA*ALFA*PCSQ/(AA+ALFA))
      IF(NROOTS .LE. 3) CALL RT123
      IF(NROOTS .EQ. 4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         IF (MASWRK) WRITE(IW,9008)
         CALL ABRT
      END IF
      MM = 0
      DO 425 K = 1,NROOTS
      U2 = (ALFA+AA)*U(K)
      U4 = U2*U2
      U6 = U4*U2
      WW = W(K)
      W4(K) = -FOUR*WW*U4
      W6(K) = -EIGHT*WW*U6
      TT = ONE/(AA+U2+ALFA)
      T  =  SQRT(TT)
      X0 = (AAX+(U2+ALFA)*CX)*TT
      Y0 = (AAY+(U2+ALFA)*CY)*TT
      Z0 = (AAZ+(U2+ALFA)*CZ)*TT
      IN = -4+MM
      DO 405 I = 1,LIT
      IN = IN+4
      NI = I
      DO 405 J = 1,LJT
      JN = IN+J
      NJ = J
      CALL STVINT
      XIN(JN   ) = XINT
      YIN(JN   ) = YINT
      ZIN(JN   ) = ZINT
      CALL POLXYZ
      XIN(JN+144) = XINT
      YIN(JN+144) = YINT
      ZIN(JN+144) = ZINT
      CALL EFQXYZ
      XIN(JN+288) = XINT
      YIN(JN+288) = YINT
      ZIN(JN+288) = ZINT
      CALL EFOXYZ
      XIN(JN+432) = XINT
      YIN(JN+432) = YINT
      ZIN(JN+432) = ZINT
  405 CONTINUE
  425 MM = MM+16
      DO 465 I = 1,IJ
      NX    = IJX(I)
      NY    = IJY(I)
      NZ    = IJZ(I)
      DXXX = ZERO
      DYYY = ZERO
      DZZZ = ZERO
      DXXY = ZERO
      DXXZ = ZERO
      DXYY = ZERO
      DYYZ = ZERO
      DXZZ = ZERO
      DYZZ = ZERO
      DXYZ = ZERO
      MM    = 0
      DO 445 K = 1,NROOTS
      DXXX=DXXX+XIN(NX+MM+144)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)*THREE
     1         -XIN(NX+MM+432)*YIN(NY+MM    )*ZIN(NZ+MM    )*W6(K)
      DYYY=DYYY+XIN(NX+MM    )*YIN(NY+MM+144)*ZIN(NZ+MM    )*W4(K)*THREE
     1         -XIN(NX+MM    )*YIN(NY+MM+432)*ZIN(NZ+MM    )*W6(K)
      DZZZ=DZZZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+144)*W4(K)*THREE
     1         -XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+432)*W6(K)
      DXXY=DXXY+XIN(NX+MM    )*YIN(NY+MM+144)*ZIN(NZ+MM    )*W4(K)
     1         -XIN(NX+MM+288)*YIN(NY+MM+144)*ZIN(NZ+MM    )*W6(K)
      DXXZ=DXXZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+144)*W4(K)
     1         -XIN(NX+MM+288)*YIN(NY+MM    )*ZIN(NZ+MM+144)*W6(K)
      DXYY=DXYY+XIN(NX+MM+144)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)
     1         -XIN(NX+MM+144)*YIN(NY+MM+288)*ZIN(NZ+MM    )*W6(K)
      DYYZ=DYYZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+144)*W4(K)
     1         -XIN(NX+MM    )*YIN(NY+MM+288)*ZIN(NZ+MM+144)*W6(K)
      DXZZ=DXZZ+XIN(NX+MM+144)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)
     1         -XIN(NX+MM+144)*YIN(NY+MM    )*ZIN(NZ+MM+288)*W6(K)
      DYZZ=DYZZ+XIN(NX+MM    )*YIN(NY+MM+144)*ZIN(NZ+MM    )*W4(K)
     1         -XIN(NX+MM    )*YIN(NY+MM+144)*ZIN(NZ+MM+288)*W6(K)
      DXYZ=DXYZ-XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W6(K)
  445 MM  = MM+16
      DUM =-GIJ(I)*PREI * BETA
      CHOINT(I) = CHOINT(I) -
     1     DUM * ( DXXX * OCT(1,IC)
     1           + DYYY * OCT(2,IC)
     1           + DZZZ * OCT(3,IC)
     1 + THREE * ( DXXY * OCT(4,IC)
     1           + DXXZ * OCT(5,IC)
     1           + DXYY * OCT(6,IC)
     1           + DYYZ * OCT(7,IC)
     1           + DXZZ * OCT(8,IC)
     1           + DYZZ * OCT(9,IC) )
     1 +   SIX * ( DXYZ * OCT(10,IC) ) )
 465  CONTINUE
C
 480  CONTINUE
C-------- END OF EFO CENTERS LOOP
 500  CONTINUE
 520  CONTINUE
C
C     ----- END OF *PRIMITIVE* LOOPS -----
C     ----- SET QQ MATRIX
C
      MAX=MAXJ
      NN=0
      DO 550 I=MINI,MAXI
      LI=LOCI+I
      IN = (LI*(LI-1))/2
      IF(IANDJ) MAX=I
      DO 550 J=MINJ,MAX
      LJ=LOCJ+J
      JN=LJ+IN
      NN=NN+1
      QQ(JN)=QQ(JN) + CHOINT(NN)
  550 CONTINUE
C
C     ----- END PARALLEL
C
  570    CONTINUE
C
C     ----- END SHELLS -----
C
  580 CONTINUE
  600 CONTINUE
C
C     ----- END OF *SHELL* LOOPS -----
C
C     ----- SUM UP PARTIAL CONTRIBUTIONS IF PARALLEL -----
C
      IF (GOPARR) CALL DDI_GSUMF(922,QQ,NUM2)
C
      CALL DAREAD(IDAF,IODA,GG,NUM2,11,0)
      CALL VADD(GG,1,QQ,1,GG,1,NUM2)
      CALL DAWRIT(IDAF,IODA,GG,NUM2,11,NAV)
C
      CALL DAREAD(IDAF,IODA,GG,NUM2,89,0)
      CALL VADD(GG,1,QQ,1,GG,1,NUM2)
      CALL DAWRIT(IDAF,IODA,GG,NUM2,89,0)
C
C     ----- PRINTING SECTION ----
C
      IF(MASWRK.AND.OUT) THEN
         WRITE(IW,9380)
         CALL PRTRIL(QQ,NUM)
         WRITE(IW,9128)
         CALL TIMIT(1)
      END IF
      RETURN
C
 9008 FORMAT(/' NUMBER OF POLYNOMIAL ROOTS NEEDED (NROOTS) IS GREATER',
     *        ' THAN 6 IN EFOINT.  CALL A PROGRAMMER/QUANTUM CHEMIST.')
 9128 FORMAT(/,' ...... END OF CHARGE OCTUPOLE INTEGRALS....')
 9380 FORMAT(10X,14("-"),/,10X,"   EFC  MATRIX",' (CHARGE OCTUPOLE)',
     * /,10X,14(1H-))
      END
C*MODULE EFINTA  *DECK EFOXYZ
      SUBROUTINE EFOXYZ
C
C     ----- GAUSS-HERMITE QUADRATURE USING MINIMUM POINT FORMULA -----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/STV/XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
      COMMON/CSSTV/ CX,CY,CZ
      COMMON/HERMIT/H1,H2(2),H3(3),H4(4),H5(5),H6(6),H7(7)
      COMMON/WERMIT/W1,W2(2),W3(3),W4(4),W5(5),W6(6),W7(7)
      DIMENSION H(21),W(21),MIN(6),MAX(6)
      EQUIVALENCE (H(1),H1),(W(1),W1)
      DATA MIN /1,2,4,7,11,16/
      DATA MAX /1,3,6,10,15,21/
      DATA ZERO /0.0D+00/
      XINT = ZERO
      YINT = ZERO
      ZINT = ZERO
      NPTS = (NI+NJ+3-2)/2+1
      IMIN = MIN(NPTS)
      IMAX = MAX(NPTS)
      DO 340 I = IMIN,IMAX
      DUM = H(I)*T
      PTX = DUM+X0
      PTY = DUM+Y0
      PTZ = DUM+Z0
      PX = PTX-CX
      PY = PTY-CY
      PZ = PTZ-CZ
      PX = PX*PX*PX
      PY = PY*PY*PY
      PZ = PZ*PZ*PZ
      AX = PTX-XI
      AY = PTY-YI
      AZ = PTZ-ZI
      BX = PTX-XJ
      BY = PTY-YJ
      BZ = PTZ-ZJ
      GO TO (180,160,140,120,100),NI
  100 PX = PX*AX
      PY = PY*AY
      PZ = PZ*AZ
  120 PX = PX*AX
      PY = PY*AY
      PZ = PZ*AZ
  140 PX = PX*AX
      PY = PY*AY
      PZ = PZ*AZ
  160 PX = PX*AX
      PY = PY*AY
      PZ = PZ*AZ
  180 GO TO (320,300,280,260,240,220,200),NJ
  200 PX = PX*BX
      PY = PY*BY
      PZ = PZ*BZ
  220 PX = PX*BX
      PY = PY*BY
      PZ = PZ*BZ
  240 PX = PX*BX
      PY = PY*BY
      PZ = PZ*BZ
  260 PX = PX*BX
      PY = PY*BY
      PZ = PZ*BZ
  280 PX = PX*BX
      PY = PY*BY
      PZ = PZ*BZ
  300 PX = PX*BX
      PY = PY*BY
      PZ = PZ*BZ
  320 DUM = W(I)
      XINT = XINT+DUM*PX
      YINT = YINT+DUM*PY
      ZINT = ZINT+DUM*PZ
  340 CONTINUE
      RETURN
      END
C*MODULE EFINTA  *DECK EFQINT
      SUBROUTINE EFQINT(QQ,GG)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
      COMMON /FMCOM / XX(1)
C
      CALL VALFM(LOADFM)
      NEED=6*MXFGPT
      CALL GETFM(NEED)
      CALL EFQIT(QQ,GG,XX(LOADFM+1))
      CALL RETFM(NEED)
C
      RETURN
      END
C*MODULE EFINTA  *DECK EFQIT
      SUBROUTINE EFQIT(QQ,GG,QUAD)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL IANDJ,NORM,DOUBLE
      LOGICAL OUT
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
      LOGICAL GOPARR,MASWRK,DSKWRK
C
      PARAMETER (MXSH=1000, MXGTOT=5000)
      PARAMETER (MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /CSSTV / CX,CY,CZ
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
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
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
C
      DIMENSION QUAD(6,*),CHQINT(100),QQ(1),GG(1)
      DIMENSION DIJ(100),XIN(432),YIN(432),ZIN(432)
      DIMENSION FIJ(100),GIJ(100)
      DIMENSION IX(20),IY(20),IZ(20),JX(20),JY(20),JZ(20)
      DIMENSION IJX(100),IJY(100),IJZ(100),W2(5),W4(5)
C
      DATA PI212 /1.1283791670955D+00/
      DATA SQRT3 /1.73205080756888D+00/
      DATA SQRT5 /2.23606797749979D+00/
      DATA JX / 0, 1, 0, 0, 2, 0, 0, 1, 1, 0,
     1          3, 0, 0, 2, 2, 1, 0, 1, 0, 1/
      DATA JY / 0, 0, 1, 0, 0, 2, 0, 1, 0, 1,
     1          0, 3, 0, 1, 0, 2, 2, 0, 1, 1/
      DATA JZ / 0, 0, 0, 1, 0, 0, 2, 0, 1, 1,
     1          0, 0, 3, 0, 1, 0, 1, 2, 2, 1/
      DATA IX / 1, 5, 1, 1, 9, 1, 1, 5, 5, 1,
     1         13, 1, 1, 9, 9, 5, 1, 5, 1, 5/
      DATA IY / 1, 1, 5, 1, 1, 9, 1, 5, 1, 5,
     1          1,13, 1, 5, 1, 9, 9, 1, 5, 5/
      DATA IZ / 1, 1, 1, 5, 1, 1, 9, 1, 5, 5,
     1          1, 1,13, 1, 5, 1, 5, 9, 9, 5/
      DATA ZERO,ONE,TWO,FOUR/0.0D+00,1.0D+00,2.0D+00,4.0D+00/
      DATA PT5,ONEPT5,THREE /0.5D+00,1.5D+00,3.D+00/
      DATA RLN10 /2.30258D+00/
      DATA QLIM /1.D-07/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DBUGME/8HINT1    /
#else
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA DBUGME_STR/"INT1    "/
#endif
C
C     ----- INTIALIZE PARALLEL -----
C
      IPCOUNT = ME - 1
      NEFQ = NMTTPT
      OUT = NPRINT.EQ.3  .OR.  EXETYP.EQ.DBUGME
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
      NUM2=(NUM*(NUM+1))/2
      DO 10 I=1,NUM2
      QQ(I) = ZERO
 10   CONTINUE
C
C  REPLACE SECOND MOMENTS IN EFQAD WITH ELECTRIC QUADRUPOLE TENSORS
C
      DO 20 I=1,NEFQ
      IF(.NOT.DOQUAD(I))GO TO 20
      XX =  EFQAD(1,I)
      YY =  EFQAD(2,I)
      ZZ =  EFQAD(3,I)
      XY =  EFQAD(4,I)
      XZ =  EFQAD(5,I)
      YZ =  EFQAD(6,I)
      DUM = XX + YY + ZZ
      QUAD(1,I) = (THREE * XX - DUM) * PT5
      QUAD(2,I) = (THREE * YY - DUM) * PT5
      QUAD(3,I) = (THREE * ZZ - DUM) * PT5
      QUAD(4,I) = ONEPT5 * XY
      QUAD(5,I) = ONEPT5 * XZ
      QUAD(6,I) = ONEPT5 * YZ
 20   CONTINUE
C
C     ----- ISHELL
C
      DO 600 II = 1,NSHELL
      IAT= KATOM(II)
      XI = C(1,IAT)
      YI = C(2,IAT)
      ZI = C(3,IAT)
      I1 = KSTART(II)
      I2 = I1+KNG(II)-1
      LIT = KTYPE(II)
      MINI = KMIN(II)
      MAXI = KMAX(II)
      LOCI = KLOC(II)-MINI
C
C     ----- JSHELL
C
      DO 580 JJ = 1,II
C
C     ----- GO PARALLEL! -----
C
            IF (GOPARR) THEN
               IPCOUNT = IPCOUNT + 1
               IF (MOD(IPCOUNT,NPROC).NE.0) GO TO 570
            END IF
      JAT= KATOM(JJ)
      XJ = C(1,JAT)
      YJ = C(2,JAT)
      ZJ = C(3,JAT)
      J1 = KSTART(JJ)
      J2 = J1+KNG(JJ)-1
      LJT = KTYPE(JJ)
      MINJ = KMIN(JJ)
      MAXJ = KMAX(JJ)
      LOCJ = KLOC(JJ)-MINJ
      NROOTS = (LIT+LJT+2-2)/2 + 1
      RR = (XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
      IANDJ = II .EQ. JJ
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      IJ = 0
      MAX = MAXJ
      DO 30 I = MINI,MAXI
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
      IF (IANDJ) MAX = I
      DO 30 J = MINJ,MAX
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
  30  CONTINUE
      DO 60 I=1,IJ
  60  CHQINT(I) = ZERO
C
C     ----- I PRIMITIVE
C
      JGMAX = J2
      DO 520 IG = I1,I2
      AI = EX(IG)
      ARRI = AI*RR
      AXI = AI*XI
      AYI = AI*YI
      AZI = AI*ZI
      CSI = CS(IG)
      CPI = CP(IG)
      CDI = CD(IG)
      CFI=CF(IG)
C
C
C     ----- J PRIMTIVE
C
      IF (IANDJ) JGMAX = IG
      DO 500 JG = J1,JGMAX
      AJ = EX(JG)
      AA = AI+AJ
      AA1 = ONE/AA
      DUM = AJ*ARRI*AA1
      IF (DUM .GT. TOL) GO TO 500
      FAC =  EXP(-DUM)
      CSJ = CS(JG)
      CPJ = CP(JG)
      CDJ = CD(JG)
      CFJ=CF(JG)
      AX = (AXI+AJ*XJ)*AA1
      AY = (AYI+AJ*YJ)*AA1
      AZ = (AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR
C
      DOUBLE=IANDJ.AND.IG.NE.JG
      MAX = MAXJ
      NN = 0
      DO 170 I=MINI,MAXI
      GO TO (70,80,110,110,90,110,110,100,110,110,
     1       102,110,110,104,110,110,110,110,110,106),I
   70 DUM1=CSI*FAC
      GO TO 110
   80 DUM1=CPI*FAC
      GO TO 110
   90 DUM1=CDI*FAC
      GO TO 110
  100 IF(NORM) DUM1=DUM1*SQRT3
      GO TO 110
  102 DUM1=CFI*FAC
      GO TO 110
  104 IF(NORM) DUM1=DUM1*SQRT5
      GO TO 110
  106 IF(NORM) DUM1=DUM1*SQRT3
  110 IF(IANDJ) MAX=I
      DO 170 J=MINJ,MAX
      GO TO (125,130,160,160,140,160,160,150,160,160,
     1       152,160,160,154,160,160,160,160,160,156),J
  125 DUM2=DUM1*CSJ
      IF(.NOT.DOUBLE) GO TO 160
      IF(I.GT.1) GO TO 126
      DUM2=DUM2+DUM2
      GO TO 160
  126 DUM2=DUM2+CSI*CPJ*FAC
      GO TO 160
  130 DUM2=DUM1*CPJ
      IF(DOUBLE) DUM2=DUM2+DUM2
      GO TO 160
  140 DUM2=DUM1*CDJ
      IF(DOUBLE) DUM2=DUM2+DUM2
      GO TO 160
  150 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 160
  152 DUM2=DUM1*CFJ
      IF(DOUBLE) DUM2=DUM2+DUM2
      GO TO 160
  154 IF(NORM) DUM2=DUM2*SQRT5
      GO TO 160
  156 IF(NORM) DUM2=DUM2*SQRT3
  160 NN=NN+1
  170 DIJ(NN)=DUM2
C
C     ..... HELLMANN-FEYNMAN TERM .....
C
      DUM = PI212*AA1/THREE
      DO 380 I = 1,IJ
      FIJ(I) = DIJ(I)*DUM
  380 CONTINUE
      AAX = AA*AX
      AAY = AA*AY
      AAZ = AA*AZ
C
      DO 480 IC = 1,NEFQ
      IF(.NOT.DOQUAD(IC))GO TO 480
      ALFA = EFATRM(IC)
      BETA = EFBTRM(IC)
      CX   = EFC(1,IC)
      CY   = EFC(2,IC)
      CZ   = EFC(3,IC)
      DUMA = PI212/(THREE*(AA+ALFA))
      DO 385 I = 1,IJ
      GIJ(I) = DIJ(I)*DUMA
  385 CONTINUE
      XX   = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
      IF(NROOTS .LE. 3) CALL RT123
      IF(NROOTS .EQ. 4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7)THEN
       IF (MASWRK) WRITE(IW,9008)
       CALL ABRT
      END IF
      MM = 0
      DO 401 K = 1,NROOTS
      U2 = AA*U(K)
      U4 = U2*U2
      WW = W(K)
      W2(K) = TWO *WW*U2
      W4(K) = FOUR*WW*U4
      TT = ONE/(AA+U2)
      T  =  SQRT(TT)
      X0 = (AAX+U2*CX)*TT
      Y0 = (AAY+U2*CY)*TT
      Z0 = (AAZ+U2*CZ)*TT
      IN = -4+MM
      DO 400 I = 1,LIT
      IN = IN+4
      NI = I
      DO 400 J = 1,LJT
      JN = IN+J
      NJ = J
      CALL STVINT
      XIN(JN   ) = XINT
      YIN(JN   ) = YINT
      ZIN(JN   ) = ZINT
      CALL POLXYZ
      XIN(JN+144) = XINT
      YIN(JN+144) = YINT
      ZIN(JN+144) = ZINT
      CALL EFQXYZ
      XIN(JN+288) = XINT
      YIN(JN+288) = YINT
      ZIN(JN+288) = ZINT
  400 CONTINUE
  401 MM = MM+16
      DO 403 I = 1,IJ
      NX    = IJX(I)
      NY    = IJY(I)
      NZ    = IJZ(I)
      DUMXX = ZERO
      DUMYY = ZERO
      DUMZZ = ZERO
      DUMXY = ZERO
      DUMXZ = ZERO
      DUMYZ = ZERO
      MM    = 0
      DO 402 K = 1,NROOTS
      DUMXX= DUMXX-XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            +XIN(NX+MM+288)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)
      DUMYY= DUMYY-XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            +XIN(NX+MM    )*YIN(NY+MM+288)*ZIN(NZ+MM    )*W4(K)
      DUMZZ= DUMZZ-XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            +XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+288)*W4(K)
      DUMXY= DUMXY+XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM    )*W4(K)
      DUMXZ= DUMXZ+XIN(NX+MM+144)*YIN(NY+MM    )*ZIN(NZ+MM+144)*W4(K)
      DUMYZ= DUMYZ+XIN(NX+MM    )*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W4(K)
  402 MM  = MM+16
      DUM = FIJ(I)
      CHQINT(I) = CHQINT(I) -
     1 DUM * ( DUMXX * QUAD(1,IC)
     2       + DUMYY * QUAD(2,IC)
     3       + DUMZZ * QUAD(3,IC)
     4 +TWO* ( DUMXY * QUAD(4,IC)
     5       + DUMXZ * QUAD(5,IC)
     6       + DUMYZ * QUAD(6,IC) ) )
 403  CONTINUE
C
C  SUBTRACT DAMPING FUNCTION TERM
C
      IF ( ABS(ALFA).LE.QLIM ) GO TO 480
      PCSQ = (AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2
      XX=AA*AA*PCSQ/(AA+ALFA)
      PREI = EXP(-AA*ALFA*PCSQ/(AA+ALFA))
      IF(NROOTS .LE. 3) CALL RT123
      IF(NROOTS .EQ. 4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7)THEN
       IF (MASWRK) WRITE(IW,9008)
       CALL ABRT
      END IF
      MM = 0
      DO 425 K = 1,NROOTS
      U2 = (ALFA+AA)*U(K)
      U4 = U2*U2
      WW = W(K)
      W2(K) = TWO *WW*U2
      W4(K) = FOUR*WW*U4
      TT = ONE/(AA+U2+ALFA)
      T  =  SQRT(TT)
      X0 = (AAX+(U2+ALFA)*CX)*TT
      Y0 = (AAY+(U2+ALFA)*CY)*TT
      Z0 = (AAZ+(U2+ALFA)*CZ)*TT
      IN = -4+MM
      DO 405 I = 1,LIT
      IN = IN+4
      NI = I
      DO 405 J = 1,LJT
      JN = IN+J
      NJ = J
      CALL STVINT
      XIN(JN   ) = XINT
      YIN(JN   ) = YINT
      ZIN(JN   ) = ZINT
      CALL POLXYZ
      XIN(JN+144) = XINT
      YIN(JN+144) = YINT
      ZIN(JN+144) = ZINT
      CALL EFQXYZ
      XIN(JN+288) = XINT
      YIN(JN+288) = YINT
      ZIN(JN+288) = ZINT
  405 CONTINUE
  425 MM = MM+16
      DO 465 I = 1,IJ
      NX    = IJX(I)
      NY    = IJY(I)
      NZ    = IJZ(I)
      DUMXX = ZERO
      DUMYY = ZERO
      DUMZZ = ZERO
      DUMXY = ZERO
      DUMXZ = ZERO
      DUMYZ = ZERO
      MM    = 0
      DO 445 K = 1,NROOTS
      DUMXX= DUMXX-XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            +XIN(NX+MM+288)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)
      DUMYY= DUMYY-XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            +XIN(NX+MM    )*YIN(NY+MM+288)*ZIN(NZ+MM    )*W4(K)
      DUMZZ= DUMZZ-XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            +XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+288)*W4(K)
      DUMXY= DUMXY+XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM    )*W4(K)
      DUMXZ= DUMXZ+XIN(NX+MM+144)*YIN(NY+MM    )*ZIN(NZ+MM+144)*W4(K)
      DUMYZ= DUMYZ+XIN(NX+MM    )*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W4(K)
  445 MM  = MM+16
      DUM =-GIJ(I)*PREI * BETA
      CHQINT(I) = CHQINT(I) -
     1 DUM * ( DUMXX * QUAD(1,IC)
     2       + DUMYY * QUAD(2,IC)
     3       + DUMZZ * QUAD(3,IC)
     4 +TWO* ( DUMXY * QUAD(4,IC)
     5       + DUMXZ * QUAD(5,IC)
     6       + DUMYZ * QUAD(6,IC) ) )
 465  CONTINUE
C
 480  CONTINUE
C-------- END OF EFQ CENTERS LOOP
 500  CONTINUE
 520  CONTINUE
C
C     ----- END OF *PRIMITIVE* LOOPS -----
C     ----- SET QQ MATRIX
C
      MAX=MAXJ
      NN=0
      DO 550 I=MINI,MAXI
      LI=LOCI+I
      IN= (LI*(LI-1))/2
      IF(IANDJ) MAX=I
      DO 550 J=MINJ,MAX
      LJ=LOCJ+J
      JN=LJ+IN
      NN=NN+1
      QQ(JN)=QQ(JN) + CHQINT(NN)
  550 CONTINUE
C
C     ----- END PARALLEL
C
  570    CONTINUE
C
C     ----- END SHELLS -----
C
  580 CONTINUE
  600 CONTINUE
C
C     ----- END OF *SHELL* LOOPS -----
C
C     ----- SUM UP PARTIAL CONTRIBUTIONS IF PARALLEL -----
C
      IF (GOPARR) CALL DDI_GSUMF(923,QQ,NUM2)
C
      CALL DAREAD(IDAF,IODA,GG,NUM2,11,0)
      CALL VADD(GG,1,QQ,1,GG,1,NUM2)
      CALL DAWRIT(IDAF,IODA,GG,NUM2,11,NAV)
C
      CALL DAREAD(IDAF,IODA,GG,NUM2,89,0)
      CALL VADD(GG,1,QQ,1,GG,1,NUM2)
      CALL DAWRIT(IDAF,IODA,GG,NUM2,89,0)
C
C     ----- PRINTING SECTION ----
C
      IF(MASWRK.AND.OUT) THEN
         WRITE(IW,9380)
         CALL PRTRIL(QQ,NUM)
         WRITE(IW,9128)
         CALL TIMIT(1)
      END IF
      RETURN
C
 9008 FORMAT(/' NUMBER OF POLYNOMIAL ROOTS NEEDED (NROOTS) IS GREATER',
     *        ' THAN 6 IN EFQINT.  CALL A PROGRAMMER/QUANTUM CHEMIST.')
 9128 FORMAT(/,' ...... END OF CHARGE QUADRUPOLE INTEGRALS....')
 9380 FORMAT(10X,14("-"),/,10X,"   EFC  MATRIX",' (CHARGE QUARUPOLE)',
     * /,10X,14(1H-))
      END
C*MODULE EFINTA  *DECK EFQXYZ
      SUBROUTINE EFQXYZ
C
C     ----- GAUSS-HERMITE QUADRATURE USING MINIMUM POINT FORMULA -----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/STV/XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
      COMMON/CSSTV/ CX,CY,CZ
      COMMON/HERMIT/H1,H2(2),H3(3),H4(4),H5(5),H6(6),H7(7)
      COMMON/WERMIT/W1,W2(2),W3(3),W4(4),W5(5),W6(6),W7(7)
      DIMENSION H(21),W(21),MIN(6),MAX(6)
      EQUIVALENCE (H(1),H1),(W(1),W1)
      DATA MIN /1,2,4,7,11,16/
      DATA MAX /1,3,6,10,15,21/
      DATA ZERO /0.0D+00/
      XINT = ZERO
      YINT = ZERO
      ZINT = ZERO
      NPTS = (NI+NJ+2-2)/2+1
      IMIN = MIN(NPTS)
      IMAX = MAX(NPTS)
      DO 340 I = IMIN,IMAX
      DUM = H(I)*T
      PTX = DUM+X0
      PTY = DUM+Y0
      PTZ = DUM+Z0
      PX = PTX-CX
      PY = PTY-CY
      PZ = PTZ-CZ
      PX = PX*PX
      PY = PY*PY
      PZ = PZ*PZ
      AX = PTX-XI
      AY = PTY-YI
      AZ = PTZ-ZI
      BX = PTX-XJ
      BY = PTY-YJ
      BZ = PTZ-ZJ
      GO TO (180,160,140,120,100),NI
  100 PX = PX*AX
      PY = PY*AY
      PZ = PZ*AZ
  120 PX = PX*AX
      PY = PY*AY
      PZ = PZ*AZ
  140 PX = PX*AX
      PY = PY*AY
      PZ = PZ*AZ
  160 PX = PX*AX
      PY = PY*AY
      PZ = PZ*AZ
  180 GO TO (320,300,280,260,240,220,200),NJ
  200 PX = PX*BX
      PY = PY*BY
      PZ = PZ*BZ
  220 PX = PX*BX
      PY = PY*BY
      PZ = PZ*BZ
  240 PX = PX*BX
      PY = PY*BY
      PZ = PZ*BZ
  260 PX = PX*BX
      PY = PY*BY
      PZ = PZ*BZ
  280 PX = PX*BX
      PY = PY*BY
      PZ = PZ*BZ
  300 PX = PX*BX
      PY = PY*BY
      PZ = PZ*BZ
  320 DUM = W(I)
      XINT = XINT+DUM*PX
      YINT = YINT+DUM*PY
      ZINT = ZINT+DUM*PZ
  340 CONTINUE
      RETURN
      END
C*MODULE EFINTA  *DECK ENUCC
      DOUBLE PRECISION FUNCTION ENUCC(N,Z,C)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      PARAMETER (MXAO=2047, MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFPBUF/ POLCHG(10),NBUFMO,LBUFF(MXAO),LBF,NAPOL,IAPOL(10)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      DIMENSION Z(2),C(3,2)
C
      PARAMETER (ZERO = 0.00D+00, ONE = 1.00D+00)
      PARAMETER (QLIM = 10.00D-08)
C
C     POINT CHARGES NUCLEI INTERACTION
C
      IF(NBUFMO.GT.0) NATAB=N-LBF
      DUM = ZERO
      ENUCC = ZERO
      DO 200 I=1,NMTTPT
        IF(.NOT.DOMONO(I))GO TO 200
        ALFA = EFATRM(I)
        BETA = EFBTRM(I)
C
C     FROZEN BUFFER REGION EXCLUDES SOME OF THE NUCLEI... AND SOME
C     ARE INCLUDED ONLY WITH REDUCED CHARGE.
C     JJ IS A COUNTER, WHILE J IS ATOM NUMBER
C
        DO 100 JJ=1,N
         J=JJ
         ZJ=Z(J)
C
         IF(NBUFMO.GT.0)THEN
           IF (JJ.GT.NATAB+NAPOL) GOTO 100
           IF (JJ.GT.NATAB) THEN
              J=IAPOL(JJ-NATAB)
              ZJ=POLCHG(JJ-NATAB)
           END IF
         END IF
C
          RR = ZERO
          DO 50 K=1,3
            RR = RR + (EFC(K,I) - C(K,J))**2
   50     CONTINUE
          RVAL = SQRT(RR)
          IF(RVAL.LT.QLIM)GO TO 200
          FACT = ONE
          IF(ABS(ALFA).GT.QLIM) FACT = ONE - (BETA*EXP(-ALFA*RR))
          DUM = DUM + FACT*EFCHG(1,I)*ZJ/RVAL
          DUM = DUM +      EFCHG(2,I)*ZJ/RVAL
  100   CONTINUE
  200 CONTINUE
C
      ENUCC = DUM
      RETURN
      END
C*MODULE EFINTA  *DECK ENUCD
      DOUBLE PRECISION FUNCTION ENUCD(N,Z,C)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      PARAMETER (MXAO=2047, MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFPBUF/ POLCHG(10),NBUFMO,LBUFF(MXAO),LBF,NAPOL,IAPOL(10)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      DIMENSION Z(2),C(3,2)
C
      DATA ZERO,ONE/0.0D+00,1.0D+00/
      DATA GLIM/1.D-14/
      DATA QLIM/1.D-07/
C
C.... DIPOLE MOMENT CONTRIBUTION TO NUCLEAR PART ( DAMPING ADDED)
C
      IF(NBUFMO.GT.0) NATAB=N-LBF
      NEFD = NMTTPT
      ENECD = ZERO
      DO 380 I = 1,NEFD
      IF(.NOT.DODIPO(I))GO TO 380
      ALFA = EFATRM(I)
      BETA = EFBTRM(I)
C
C     FROZEN BUFFER REGION EXCLUDES SOME OF THE NUCLEI... AND SOME
C     ARE INCLUDED ONLY WITH REDUCED CHARGE.
C     JJ IS A COUNTER, WHILE J IS ATOM NUMBER
C
         DO 370 JJ = 1,N
         J=JJ
         ZJ=Z(J)
C
         IF(NBUFMO.GT.0)THEN
           IF (JJ.GT.NATAB+NAPOL) GOTO 370
           IF (JJ.GT.NATAB) THEN
              J=IAPOL(JJ-NATAB)
              ZJ=POLCHG(JJ-NATAB)
           END IF
         END IF
C
        DX = C(1,J) - EFC(1,I)
        DY = C(2,J) - EFC(2,I)
        DZ = C(3,J) - EFC(3,I)
        RR = DX*DX + DY*DY +DZ*DZ
        IF(RR.LT.GLIM) GO TO 370
        RR3 = RR*SQRT(RR)
        FACT = ONE
        IF(ABS(ALFA).GT.QLIM) FACT = ONE - (BETA * EXP (-ALFA * RR))
        ENECD = ENECD + ZJ * FACT *
     1    (EFDIP(1,I)*DX + EFDIP(2,I)*DY + EFDIP(3,I)*DZ )/RR3
 370    CONTINUE
 380   CONTINUE
      ENUCD = ENECD
      RETURN
      END
C*MODULE EFINTA  *DECK ENUCO
      DOUBLE PRECISION FUNCTION ENUCO(N,ZNUC,C)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      PARAMETER (MXAO=2047, MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFPBUF/ POLCHG(10),NBUFMO,LBUFF(MXAO),LBF,NAPOL,IAPOL(10)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      DIMENSION ZNUC(2),C(3,2)
C
      DATA ZERO,ONE/0.0D+00,1.0D+00/
      DATA PT5,THREE/0.5D+00,3.D+00/
      DATA FIVE,SIX/5.D+00,6.D+00/
      DATA GLIM/1.D-14/
      DATA QLIM/1.D-07/
C
C....DAMPING ADDED
C
      NEFO = NMTTPT
      TOTAL = ZERO
      IF(NBUFMO.GT.0) NATAB=N-LBF
C
      DO 380 I = 1,NEFO
         IF(.NOT.DOOCTU(I)) GO TO 380
         ALFA = EFATRM(I)
         BETA = EFBTRM(I)
C REPLACE THIRD MOMENTS IN ARRAY SM WITH ELECTRIC OCTUPOLE TENSORS
         XXX2 = EFOCT(1,I)*PT5
         YYY2 = EFOCT(2,I)*PT5
         ZZZ2 = EFOCT(3,I)*PT5
         XXY2 = EFOCT(4,I)*PT5
         XXZ2 = EFOCT(5,I)*PT5
         XYY2 = EFOCT(6,I)*PT5
         YYZ2 = EFOCT(7,I)*PT5
         XZZ2 = EFOCT(8,I)*PT5
         YZZ2 = EFOCT(9,I)*PT5
         XYZ2 = EFOCT(10,I)*PT5
         DUMX = XXX2 + XYY2 + XZZ2
         DUMY = XXY2 + YYY2 + YZZ2
         DUMZ = XXZ2 + YYZ2 + ZZZ2
         XXX2 = FIVE * XXX2 - THREE * DUMX
         YYY2 = FIVE * YYY2 - THREE * DUMY
         ZZZ2 = FIVE * ZZZ2 - THREE * DUMZ
         XXY2 = FIVE * XXY2 - DUMY
         XXZ2 = FIVE * XXZ2 - DUMZ
         XYY2 = FIVE * XYY2 - DUMX
         YYZ2 = FIVE * YYZ2 - DUMZ
         XZZ2 = FIVE * XZZ2 - DUMX
         YZZ2 = FIVE * YZZ2 - DUMY
         XYZ2 = FIVE * XYZ2
C
C     FROZEN BUFFER REGION EXCLUDES SOME OF THE NUCLEI... AND SOME
C     ARE INCLUDED ONLY WITH REDUCED CHARGE.
C     JJ IS A COUNTER, WHILE J IS ATOM NUMBER
C
         DO 370 JJ = 1,N
         J=JJ
         ZNUCJ=ZNUC(J)
C
         IF(NBUFMO.GT.0)THEN
           IF (JJ.GT.NATAB+NAPOL) GOTO 370
           IF (JJ.GT.NATAB) THEN
              J=IAPOL(JJ-NATAB)
              ZNUCJ=POLCHG(JJ-NATAB)
           END IF
         END IF
C
            X = C(1,J) - EFC(1,I)
            Y = C(2,J) - EFC(2,I)
            Z = C(3,J) - EFC(3,I)
            XX = X*X
            YY = Y*Y
            ZZ = Z*Z
            XXX = XX*X
            YYY = YY*Y
            ZZZ = ZZ*Z
            XXY = XX*Y
            XXZ = XX*Z
            XYY = X*YY
            YYZ = YY*Z
            XZZ = X*ZZ
            YZZ = Y*ZZ
            XYZ = X*Y*Z
            R2 = XX + YY + ZZ
            IF(R2.LT.GLIM) GO TO 370
            R7 = R2 * R2 * R2 * SQRT(R2)
            RRX = R2 * X
            RRY = R2 * Y
            RRZ = R2 * Z
            TERM = ZERO
            TERM = TERM +XXX2 * (FIVE*XXX - THREE*RRX)
            TERM = TERM +YYY2 * (FIVE*YYY - THREE*RRY)
            TERM = TERM +ZZZ2 * (FIVE*ZZZ - THREE*RRZ)
            TERM = TERM +XXY2 * (FIVE*XXY - RRY)*THREE
            TERM = TERM +XXZ2 * (FIVE*XXZ - RRZ)*THREE
            TERM = TERM +XYY2 * (FIVE*XYY - RRX)*THREE
            TERM = TERM +YYZ2 * (FIVE*YYZ - RRZ)*THREE
            TERM = TERM +XZZ2 * (FIVE*XZZ - RRX)*THREE
            TERM = TERM +YZZ2 * (FIVE*YZZ - RRY)*THREE
            TERM = TERM +XYZ2 * (FIVE*XYZ)*SIX
            FACT = ONE
            IF(ABS(ALFA).GT.QLIM) FACT = ONE - (BETA * EXP (-ALFA * R2))
            TOTAL = TOTAL + ZNUCJ * FACT * TERM / (R7*FIVE)
 370     CONTINUE
 380  CONTINUE
C
      ENUCO = TOTAL
      RETURN
      END
C*MODULE EFINTA  *DECK ENUCQ
      SUBROUTINE ENUCQ(EQNUC,N,Z,C)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
      COMMON /FMCOM / XX(1)
C
      CALL VALFM(LOADFM)
      NEED=6*MXFGPT
      CALL GETFM(NEED)
      CALL ENUCQA(EQNUC,N,Z,C,XX(LOADFM+1))
      CALL RETFM(NEED)
C
      RETURN
      END
C*MODULE EFINTA  *DECK ENUCQA
      SUBROUTINE ENUCQA(ENUCQ,N,Z,C,QUAD)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      PARAMETER (MXAO=2047, MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION Z(*),C(3,*)
      DIMENSION QUAD(6,*)
C
      COMMON /EFPBUF/ POLCHG(10),NBUFMO,LBUFF(MXAO),LBF,NAPOL,IAPOL(10)
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      DATA ZERO,ONE/0.0D+00,1.0D+00/
      DATA PT5,ONEPT5,THREE,SIX /0.5D+00,1.5D+00,3.D+00,6.D+00/
      DATA GLIM/10.0D-15/
      DATA QLIM/10.0D-8/
C
C REPLACE SECOND MOMENTS IN EFQAD WITH ELECTRIC QUADRUPOLE TENSORS
C
      IF(NBUFMO.GT.0) NATAB=N-LBF
      NEFQ = NMTTPT
      DO 550 I=1,NEFQ
      IF(.NOT.DOQUAD(I))GO TO 550
      XX =  EFQAD(1,I)
      YY =  EFQAD(2,I)
      ZZ =  EFQAD(3,I)
      XY =  EFQAD(4,I)
      XZ =  EFQAD(5,I)
      YZ =  EFQAD(6,I)
      DUM = XX + YY + ZZ
      QUAD(1,I) = (THREE * XX - DUM) * PT5
      QUAD(2,I) = (THREE * YY - DUM) * PT5
      QUAD(3,I) = (THREE * ZZ - DUM) * PT5
      QUAD(4,I) = ONEPT5 * XY
      QUAD(5,I) = ONEPT5 * XZ
      QUAD(6,I) = ONEPT5 * YZ
 550  CONTINUE
C....DAMPING ADDED
      ENEFQ = ZERO
      DO 380 I = 1,NEFQ
      IF(.NOT.DOQUAD(I))GO TO 380
      ALFA = EFATRM(I)
      BETA = EFBTRM(I)
C
C     FROZEN BUFFER REGION EXCLUDES SOME OF THE NUCLEI... AND SOME
C     ARE INCLUDED ONLY WITH REDUCED CHARGE.
C     JJ IS A COUNTER, WHILE J IS ATOM NUMBER
C
      DO 370 JJ = 1,N
      J=JJ
      ZJ=Z(J)
C
      IF(NBUFMO.GT.0)THEN
           IF (JJ.GT.NATAB+NAPOL) GOTO 370
           IF (JJ.GT.NATAB) THEN
              J=IAPOL(JJ-NATAB)
              ZJ=POLCHG(JJ-NATAB)
           END IF
      END IF
C
      D2X = C(1,J) - EFC(1,I)
      D2Y = C(2,J) - EFC(2,I)
      D2Z = C(3,J) - EFC(3,I)
      RR = D2X*D2X + D2Y*D2Y +D2Z*D2Z
      IF(RR.LT.GLIM) GO TO 370
      RR5 = RR * RR * SQRT(RR)
      FACT = ONE
      IF(ABS(ALFA).GT.QLIM) FACT = ONE - (BETA * EXP (-ALFA * RR))
C     ENEFQ=ENEFQ +ZJ*FACT*( QUAD(1,I)*(THREE*D2X*D2X - RR)
C    1                       + QUAD(2,I)*(THREE*D2Y*D2Y - RR)
C    1                       + QUAD(3,I)*(THREE*D2Z*D2Z - RR)
C    1                       + QUAD(4,I)* SIX*D2X*D2Y
C    1                       + QUAD(5,I)* SIX*D2X*D2Z
C    1                       + QUAD(6,I)* SIX*D2Y*D2Z ) /(THREE*RR5)
      ENEFQ=ENEFQ +ZJ*FACT*( QUAD(1,I)*(THREE*D2X*D2X)
     1                       + QUAD(2,I)*(THREE*D2Y*D2Y)
     1                       + QUAD(3,I)*(THREE*D2Z*D2Z)
     1                       + QUAD(4,I)* SIX*D2X*D2Y
     1                       + QUAD(5,I)* SIX*D2X*D2Z
     1                       + QUAD(6,I)* SIX*D2Y*D2Z ) /(THREE*RR5)
 370   CONTINUE
 380   CONTINUE
       ENUCQ = ENEFQ
      RETURN
      END
C*MODULE EFINTA  *DECK POLXYZ
      SUBROUTINE POLXYZ
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     ----- GAUSS-HERMITE QUADRATURE USING MINIMUM POINT FORMULA -----
C
      COMMON/STV/XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
      COMMON/CSSTV/ CX,CY,CZ
      COMMON/HERMIT/H1,H2(2),H3(3),H4(4),H5(5),H6(6),H7(7)
      COMMON/WERMIT/W1,W2(2),W3(3),W4(4),W5(5),W6(6),W7(7)
      DIMENSION H(21),W(21),MIN(6),MAX(6)
      EQUIVALENCE (H(1),H1),(W(1),W1)
      DATA MIN /1,2,4,7,11,16/
      DATA MAX /1,3,6,10,15,21/
      DATA ZERO /0.0D+00/
      XINT=ZERO
      YINT=ZERO
      ZINT=ZERO
      NPTS=(NI+NJ+1-2)/2+1
      IMIN=MIN(NPTS)
      IMAX=MAX(NPTS)
      DO 13 I=IMIN,IMAX
      DUM=H(I)*T
      PTX=DUM+X0
      PTY=DUM+Y0
      PTZ=DUM+Z0
      PX=PTX-CX
      PY=PTY-CY
      PZ=PTZ-CZ
      AX=PTX-XI
      AY=PTY-YI
      AZ=PTZ-ZI
      BX=PTX-XJ
      BY=PTY-YJ
      BZ=PTZ-ZJ
      GO TO (5,4,3,2,1),NI
    1 PX=PX*AX
      PY=PY*AY
      PZ=PZ*AZ
    2 PX=PX*AX
      PY=PY*AY
      PZ=PZ*AZ
    3 PX=PX*AX
      PY=PY*AY
      PZ=PZ*AZ
    4 PX=PX*AX
      PY=PY*AY
      PZ=PZ*AZ
    5 GO TO (12,11,10,9,8,7,6),NJ
    6 PX=PX*BX
      PY=PY*BY
      PZ=PZ*BZ
    7 PX=PX*BX
      PY=PY*BY
      PZ=PZ*BZ
    8 PX=PX*BX
      PY=PY*BY
      PZ=PZ*BZ
    9 PX=PX*BX
      PY=PY*BY
      PZ=PZ*BZ
   10 PX=PX*BX
      PY=PY*BY
      PZ=PZ*BZ
   11 PX=PX*BX
      PY=PY*BY
      PZ=PZ*BZ
   12 DUM=W(I)
      XINT=XINT+DUM*PX
      YINT=YINT+DUM*PY
      ZINT=ZINT+DUM*PZ
   13 CONTINUE
      RETURN
      END
