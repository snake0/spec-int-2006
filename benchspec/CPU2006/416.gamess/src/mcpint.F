C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C 15 MAY 03 - MWS - MODEL CORE POTENTIALS INCLUDED IN GAMESS
C 30 JUN 01 - MK  - MCPINT,MCPPRO: CLEAN UP PRINTING 
C 13 DEC 00 - MK  - USE LU2 FOR UNCONTRACTED BASIS SETS IN MCPINT AND MCPPRO
C 12 DEC 00 - MK  - DO NOT SAVE ON DAF RECORDS 76 AND 77 
C 16 MAR 00 - MK  - AN0MAT AN1MAT COMMENTED OUT
C 31 MAY 99 - CCL - FIXED ERROR IN PROJECTION OPERATOR (WITH F)
C 06 AUG 98 - KM  - QQ AND GG USE INSTEAD FOR X(LL2) ARRAY
C  4 JUN 97 - MK  - H MATRIX PASSED AS PARAMETER
C 18 JUN 94 - DK  - MODEL-POTENTIAL-VERSION 0
C
*MODULE MCPINT  *DECK MCPINT
C
C     --- SET LINES COMMENTED WITH 'CA' TO PRINT A-TERMS MATRIX FOR N=0 AND N=1
C
C     --- MCPINT CALCULATES THE MODEL-POTENTIAL INTEGRALS BASED
C     --- ON THE PARAMETERS A AND ALPHA FOR N=1 AND N=0.
C     --- N=1 LEADS TO OVERLAP-TYPE INTEGRALS AND
C     --- N=0 LEADS TO NUCLEAR-ATTRACTION-TYPE INTEGRALS.
C     --- THE PROJECTION-OPERATOR INTEGRALS ARE CALCULATED IN
C     --- MCPPRO, WHICH IS CALLED AT THE END OF THIS SUBROUTINE.
C     --- THE INTEGRALS ARE CALCULATED USING THE ALGORITHM WHICH
C     --- IS EMPLOYED IN GAMESS FOR ONE-ELECTRON INTEGRALS.
C     --- DK MAY 1994.
C
C     --- CHANGE FROM X(LL2) TO QQ AND GG  [REF EFCINT]
C     --- KM AUG 1998.
C     
      SUBROUTINE MCPINT(QQ,GG,LU2)
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL IANDJ,DBG,NORM,DOUBLE,GOPARR,DSKWRK,MASWRK
      LOGICAL CHK
C     DOUBLE PRECISION NINE
C
      DIMENSION QQ(*),GG(*)
      DIMENSION DIJ(225),XIN(125),YIN(125),ZIN(125),
     *          IX(35),IY(35),IZ(35),JX(35),JY(35),JZ(35),
     *          IJX(225),IJY(225),IJZ(225)
C
C     --- MXMPA:   MAX.NO.OF MOD.POT. A-TERMS FOR GIVEN N-VALUE
C     --- S(I)     OVERLAP-TYPE INTEGRALS FOR SHELL II,JJ
C     --- G(I)     NUCLEAR-ATTRACTION-TYPE INTEGRALS FOR SHELL II,JJ 
C     --- AN0MAT   CONTAINS N=0 A-TERM INTEGRALS
C     --- AN1MAT   CONTAINS N=1 A-TERM INTEGRALS
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
      PARAMETER (MXMPA=3*MXATM)
C     DIMENSION AN0MAT(MXMPA),AN1MAT(MXMPA)
      DIMENSION S(225),G(225)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
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
C     --- MODEL-POTENTIAL PARAMETERS AND CORE-SHELL BASIS-SET INFORMATION
C     --- (SEE SUBROUTINE ECPPAR FOR INPUT) 
C
      COMMON /MMP1  / AN0(MXMPA),ALPN0(MXMPA),AN1(MXMPA),ALPN1(MXMPA),
     2                MPSKIP(MXATM),NOAN0(MXATM),NOAN1(MXATM)
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
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DEBUG/8HDEBUG   /,DBUGME/8HINT1    /
#else
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA DEBUG_STR/"DEBUG   "/,DBUGME_STR/"INT1    "/
#endif
C
      CHK= .FALSE.
      DBG = .FALSE.
C
      TOL = RLN10*ITOL
      IF (EXETYP.EQ.DEBUG  .OR. EXETYP.EQ.DBUGME  .OR.
     *    NPRINT.EQ.-3 .AND. MASWRK) THEN
C        OUT = .TRUE.
         DBG = .TRUE.
      END IF
C     IF ((NPRINT.NE.-5) .AND. MASWRK) WRITE (IW,9000)
      IF (DBG) WRITE (IW,9000)
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C     L1 = NUM
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
               S(I) = ZERO
               G(I) = ZERO
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
C     --- PAA,PX,PY,PZ  EXPONENT, COORDINATES OF THE
C     --- PRODUCT OF THE PRIMITIVES IG,JG   
C     
               IF (IANDJ) JGMAX = IG
               DO 500 JG = J1,JGMAX
                  AJ = EX(JG)
                  PAA = AI+AJ
                  PAA1 = ONE/PAA
                  DUM = AJ*ARRI*PAA1
                  IF (DUM .GT. TOL) GO TO 500
                  PFAC = EXP(-DUM)
                  CSJ = CS(JG)
                  CPJ = CP(JG)
                  CDJ = CD(JG)
                  CFJ = CF(JG)
                  CGJ = CG(JG)
                  PX = (AXI+AJ*XJ)*PAA1
                  PY = (AYI+AJ*YJ)*PAA1
                  PZ = (AZI+AJ*ZJ)*PAA1
C
                  MIN0=1
                  MIN1=1
C
C                 --- SUM OVER ALL ATOMS; SKIP THE ALL-ELECTRON ONES
C
                  DO 480 IMP=1,NAT
                    IF (MPSKIP(IMP).EQ.1) GOTO 480
C                   --- COORDINATES OF MP-ATOM
                    XMP = C(1,IMP)
                    YMP = C(2,IMP)
                    ZMP = C(3,IMP)
C                   --- CORE-CHARGE OF MP-ATOM
                    ZNUC = -ZAN(IMP)
C                   --- NOAN1  NO.OF N=1 A-TERMS
                    MAX1 = MIN1 + NOAN1(IMP) - 1
C
C     --- N=1 LEADS TO OVERLAP-TYPE INTEGRALS AND
C     --- SUM OVER N=1 A-TERMS
C
                    DO 360 KMP=MIN1,MAX1
                      ALPMP=ALPN1(KMP)
                      CMP  =AN1(KMP)
C
C                     --- APPLY THE GAUSSIAN-PRODUCT-THEOREM:
C                     --- ALPMP,CMP  ALPHA-,A-PARAMETER RESP.           
C                     
                      AA = PAA+ALPMP
                      AA1= ONE/AA
                      RRMP=(PX-XMP)**2+(PY-YMP)**2+
     *                     (PZ-ZMP)**2
                      DUM=PAA*ALPMP/AA*RRMP
                      FAC=PFAC*EXP(-DUM)*CMP
                      IF (CHK) WRITE (IW,9800) II,JJ,IG,JG,FAC
                      AX=(PAA*PX+ALPMP*XMP)/AA
                      AY=(PAA*PY+ALPMP*YMP)/AA
                      AZ=(PAA*PZ+ALPMP*ZMP)/AA
C
C                     ----- DENSITY FACTOR
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
                       IF (I.EQ.8.AND.NORM) DUM1=DUM1*SQRT3
                       IF (I.EQ.11) DUM1=CFI*FAC
                       IF (I.EQ.14.AND.NORM) DUM1=DUM1*SQRT5
                       IF (I.EQ.20.AND.NORM) DUM1=DUM1*SQRT3
                       IF (I.EQ.21) DUM1=CGI*FAC
                       IF (I.EQ.24.AND.NORM) DUM1=DUM1*SQRT7
                       IF (I.EQ.30.AND.NORM) DUM1=DUM1*SQRT5/SQRT3
                       IF (I.EQ.33.AND.NORM) DUM1=DUM1*SQRT3
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
                        ELSE IF (J.EQ.8.AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        ELSE IF (J.EQ.11) THEN
                           DUM2=DUM1*CFJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF (J.EQ.14.AND.NORM) THEN
                           DUM2=DUM2*SQRT5
                        ELSE IF (J.EQ.20.AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        ELSE IF (J.EQ.21) THEN
                           DUM2=DUM1*CGJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF (J.EQ.24.AND.NORM) THEN
                           DUM2=DUM2*SQRT7
                        ELSE IF (J.EQ.30.AND.NORM) THEN
                           DUM2=DUM2*SQRT5/SQRT3
                        ELSE IF (J.EQ.33.AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        END IF
                        NN = NN+1
                        DIJ(NN) = DUM2
                       IF (CHK) WRITE(IW,9600)II,JJ,IG,JG,FAC,NN,DIJ(NN)
  200                   CONTINUE
  220                 CONTINUE
C
C                  --- CALCULATION OF THE OVERLAP-INTEGRAL
C
                   T = SQRT(AA1)
                   X0 = AX
                   Y0 = AY
                   Z0 = AZ
                   IN = -5
                   DO 320 I = 1,LIT
                     IN = IN+5
                     NI = I
                     DO 300 J = 1,LJT
                        JN = IN+J
                        NJ = J
                        CALL STVINT
                        XIN(JN) = XINT*T
                        YIN(JN) = YINT*T
                        ZIN(JN) = ZINT*T
  300                CONTINUE
  320              CONTINUE
                  DO 340 I = 1,IJ
                     NX = IJX(I)
                     NY = IJY(I)
                     NZ = IJZ(I)
                     DUM = XIN(NX)*YIN(NY)*ZIN(NZ)
                     S(I) = S(I)+DIJ(I)*DUM*ZNUC
  340             CONTINUE
C               --- END OF SUM OVER N=1 TERMS
  360           CONTINUE
C
                MIN1 = MAX1 + 1
C
C     --- N=0 LEADS TO NUCLEAR-ATTRACTION-TYPE INTEGRALS.
C     --- SUM OVER N=0 A-TERMS
C
C     --- NOAN0  NO.OF N=0 A-TERMS
                MAX0 = MIN0 + NOAN0(IMP) - 1
C
C              --- SUM OVER N=0 A-TERMS
C
                DO 470 KMP=MIN0,MAX0
                  ALPMP=ALPN0(KMP)
                  CMP  =AN0(KMP)
                  AA = PAA+ALPMP
                  AA1= ONE/AA
                  RRMP=(PX-XMP)**2+(PY-YMP)**2+
     *                 (PZ-ZMP)**2
                  DUM=PAA*ALPMP/AA*RRMP
                  FAC=PFAC*EXP(-DUM)*CMP
                  IF (CHK) WRITE (IW,9900) II,JJ,IG,JG,FAC
                  AX=(PAA*PX+ALPMP*XMP)/AA
                  AY=(PAA*PY+ALPMP*YMP)/AA
                  AZ=(PAA*PZ+ALPMP*ZMP)/AA
C
C                 ----- DENSITY FACTOR
C
                  DOUBLE=IANDJ.AND.IG.NE.JG
                  MAX = MAXJ
                  NN = 0
                  DUM1 = ZERO
                  DUM2 = ZERO
                  DO 380 I = MINI,MAXI
                   IF (I.EQ.1) DUM1=CSI*FAC
                   IF (I.EQ.2) DUM1=CPI*FAC
                   IF (I.EQ.5) DUM1=CDI*FAC
                   IF (I.EQ.8.AND.NORM) DUM1=DUM1*SQRT3
                   IF (I.EQ.11) DUM1=CFI*FAC
                   IF (I.EQ.14.AND.NORM) DUM1=DUM1*SQRT5
                   IF (I.EQ.20.AND.NORM) DUM1=DUM1*SQRT3
                   IF (I.EQ.21) DUM1=CGI*FAC
                   IF (I.EQ.24.AND.NORM) DUM1=DUM1*SQRT7
                   IF (I.EQ.30.AND.NORM) DUM1=DUM1*SQRT5/SQRT3
                   IF (I.EQ.33.AND.NORM) DUM1=DUM1*SQRT3
                   IF (IANDJ) MAX = I
                   DO 370 J = MINJ,MAX
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
                      ELSE IF (J.EQ.8.AND.NORM) THEN
                         DUM2=DUM2*SQRT3
                      ELSE IF (J.EQ.11) THEN
                         DUM2=DUM1*CFJ
                         IF (DOUBLE) DUM2=DUM2+DUM2
                      ELSE IF (J.EQ.14.AND.NORM) THEN
                         DUM2=DUM2*SQRT5
                      ELSE IF (J.EQ.20.AND.NORM) THEN
                         DUM2=DUM2*SQRT3
                      ELSE IF (J.EQ.21) THEN
                         DUM2=DUM1*CGJ
                         IF (DOUBLE) DUM2=DUM2+DUM2
                      ELSE IF (J.EQ.24.AND.NORM) THEN
                         DUM2=DUM2*SQRT7
                      ELSE IF (J.EQ.30.AND.NORM) THEN
                         DUM2=DUM2*SQRT5/SQRT3
                      ELSE IF (J.EQ.33.AND.NORM) THEN
                         DUM2=DUM2*SQRT3
                      END IF
                      NN = NN+1
                      DIJ(NN) = DUM2
                      IF (CHK) WRITE(IW,9700) II,JJ,IG,JG,FAC,NN,DIJ(NN)
  370                 CONTINUE
  380               CONTINUE
C
C                 --- CALCULATION OF NUCLEAR-ATTRACTION-TYPE INTEGRAL
C 
                  DUM = PI212*AA1
                  DO 400 I = 1,IJ
                    DIJ(I) = DIJ(I)*DUM
 400              CONTINUE
                  AAX = AA*AX
                  AAY = AA*AY
                  AAZ = AA*AZ
                  CX = XMP
                  CY = YMP
                  CZ = ZMP
                  XX = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
                  IF (NROOTS.LE.3) CALL RT123
                  IF (NROOTS.EQ.4) CALL ROOT4
                  IF (NROOTS.EQ.5) CALL ROOT5
                  MM = 0
                  DO 430 K = 1,NROOTS
                     UU = AA*U(K)
                     WW = W(K)*ZNUC
                     TT = ONE/(AA+UU)
                     T = SQRT(TT)
                     X0 = (AAX+UU*CX)*TT
                     Y0 = (AAY+UU*CY)*TT
                     Z0 = (AAZ+UU*CZ)*TT
                     IN = -5+MM
                     DO 420 I = 1,LIT
                        IN = IN+5
                        NI = I
                        DO 410 J = 1,LJT
                           JN = IN+J
                           NJ = J
                           CALL STVINT
                           XIN(JN) = XINT
                           YIN(JN) = YINT
                           ZIN(JN) = ZINT*WW
  410                   CONTINUE
  420                CONTINUE
                     MM = MM+25
  430             CONTINUE
                  DO 450 I = 1,IJ
                     NX = IJX(I)
                     NY = IJY(I)
                     NZ = IJZ(I)
                     DUM = ZERO
                     MM = 0
                     DO 440 K = 1,NROOTS
                        DUM = DUM+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM)
                        MM = MM+25
  440                CONTINUE
                     G(I) = G(I)+DUM*DIJ(I)
  450             CONTINUE
C     --- END OF SUM OVER N=0 TERMS
  470           CONTINUE
                MIN0 = MAX0 + 1
C     --- END OF SUM OVER ATOMS
  480          CONTINUE
C     ----- END PRIMITIVES -----
  500          CONTINUE
  520       CONTINUE
C
C           --- MODEL-POTENTIAL A-TERM INTEGRALS  
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
C         --- CHECK THE DIMENSIONS (ONLY IF A-MATRICES SHALL BE PRINTED)
*                 IF (JN.GT.MXMPA) THEN
*                    WRITE(IW,*) '*** MODULE MCPINT ***'
*                    WRITE(IW,*) 'DIMENSIONS OF AN0MAT EXCEEDED'
*                    STOP
*                 END IF
*                 AN0MAT(JN)=G(NN)
*                 AN1MAT(JN)=S(NN)
                  QQ(JN) = QQ(JN)+G(NN)+S(NN)
  600          CONTINUE
  620       CONTINUE
C        ----- END SHELLS -----
  700    CONTINUE
  720 CONTINUE
C
      CALL DAREAD(IDAF,IODA,GG,L2,11,0)
      CALL VADD(GG,1,QQ,1,GG,1,L2)
      CALL DAWRIT(IDAF,IODA,GG,LU2,11,0)
C
C     CALL DAREAD(IDAF,IODA,GG,L2,76,0)
C     CALL VADD(GG,1,QQ,1,GG,1,L2)
C     CALL DAWRIT(IDAF,IODA,GG,LU2,76,0)
C
C     ----- DONE WITH MODEL-POTENTIAL A-TERM INTEGRALS -----
C
      IF(DBG) THEN
*        CALL PRTRIL(QQ,L1)
         WRITE(IW,9999)
         CALL TIMIT(1)
         WRITE(IW,9200) 
*        CALL PRTRIL(AN0MAT,L1)
         WRITE(IW,9300) 
*        CALL PRTRIL(AN1MAT,L1)
      END IF
      RETURN
C
 9000 FORMAT(/10X,20("*")/,10X,'MCP A-TERM INTEGRALS'/10X,20("*"))
 9200 FORMAT(/10X,'MCP N=0 A-TERM-CONTRIBUTIONS'/10X,28("-"))
 9300 FORMAT(/10X,'MCP N=1 A-TERM-CONTRIBUTIONS'/10X,28("-"))
 9600 FORMAT(1X,'DBGINT1:II,JJ,IG,JG=',4I3,', FAC=',E12.5,
     *       5X,'DIJ(',I3,')=',E12.5)
 9700 FORMAT(1X,'DBGINT0:II,JJ,IG,JG=',4I3,', FAC=',E12.5,
     *       5X,'DIJ(',I3,')=',E12.5)
 9800 FORMAT(1X,'DBGINT1: II,JJ,IG,JG,      FAC=',4I5,4X,E12.5)
 9900 FORMAT(1X,'DBGINT0: II,JJ,IG,JG,      FAC=',4I5,4X,E12.5)
 9999 FORMAT(1X,5("."),' END OF MCP A-TERM INTEGRALS ',5("."))
      END
C*MODULE MCPINT  *DECK MCPPRO
C
C     --- MCPPRO CALCULATES THE PROJECTION-OPERATOR INTEGRALS
C     --- OF THE MODEL-POTENTIAL METHOD. THE INTEGRALS ARE
C     --- CALCULATED USING THE ALGORITHM FOR OVERLAP INTEGRALS
C     --- IN GAMESS (VERSION1993). 
C
C     --- LINES COMMENTED WITH 'CB' TO PRINT PROJECTION OPERATOR MATRIX
C
      SUBROUTINE MCPPRO(QQ,GG,SMP,LU2,MPSIZ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL IANDJ,NORM,GOPARR,DSKWRK,MASWRK,DBG
C
      DIMENSION QQ(*),GG(*),SMP(MPSIZ,MPSIZ)
      DIMENSION DIJ(225),XIN(125),YIN(125),ZIN(125),
     *          IX(35),IY(35),IZ(35),JX(35),JY(35),JZ(35),
     *          IJX(225),IJY(225),IJZ(225),STMP(225)
      DIMENSION T1(10),T2(10)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
C     --- MXMPA:   MAX.NO.OF MOD.POT. A-TERMS FOR GIVEN N-VALUE
C     --- MXMPSH:  MAX.NO.OF CORE-SHELLS IN ALL MOD.POT.ATOMS
C     --- MXMPGT:  MAX.NO.OF PGTFS FOR ALL CORE-SHELLS IN MOD.POT.ATOMS
C
      PARAMETER (MXMPSH=2*MXATM, MXMPGT=5*MXMPSH)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
      COMMON /SYMIND/ TOL,II,JJ,LIT,LJT,MINI,MINJ,MAXI,MAXJ,IANDJ
C
C     --- MODEL-POTENTIAL PARAMETERS AND CORE-SHELL BASIS-SET INFORMATION
C
      COMMON /MMP2  /BPAR(MXMPSH),EXPMP(MXMPGT),CSMP(MXMPGT),
     2               CPMP(MXMPGT),CDMP(MXMPGT),CFMP(MXMPGT),
     3               MPSKIP(MXATM),NOCOSH(MXATM),MPKSTA(MXMPSH),
     4               MPKNG(MXMPSH),MPKTYP(MXMPSH),MPKMIN(MXMPSH),
     5               MPKMAX(MXMPSH),MPKLOC(MXMPSH)
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, 
     *           SQRT3=1.73205080756888D+00,
     *           SQRT5=2.23606797749979D+00, SQRT7=2.64575131106459D+00,
     *           RLN10=2.30258D+00, PT3=1.00D+00/3.00D+00,PT15=1.50D+00)
      PARAMETER (B23S5=.6708203932499369D+00, PT03=0.3D+00)
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
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DEBUG/8HDEBUG   /,DBUGME/8HINT1    /
#else
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA DEBUG_STR/"DEBUG   "/,DBUGME_STR/"INT1    "/
#endif
C
      TOL = RLN10*ITOL
      DBG = (NPRINT.EQ.3  .OR.  EXETYP.EQ.DEBUG  .OR.  EXETYP.EQ.DBUGME)
     *      .AND. MASWRK
      IF (DBG) WRITE (IW,9000)
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
      L1 = NUM
      L2 = (NUM*NUM+NUM)/2
      CALL VCLR(QQ,1,L2)
C
C     --- CALCULATE THE OVERLAP-INTEGRALS  SMP(LJ,LI) = <JJ,J|II,I>
C     --- WITH  II VALENCE-SHELL, JJ CORE-SHELL
C     --- AND   I,J SPECIFYING THE ANGULAR PART
C
C     ----- ISHELL
C
      DO 580 II = 1,NSHELL
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
C     ----- SUM OVER ALL ATOMS; SKIP THE ALL-ELECTRON ONES
C
        JJ=0
        DO 570 MPAT=1,NAT
          IF (MPSKIP(MPAT).EQ.1) GOTO  570
          MPSHEL=NOCOSH(MPAT)
C
C         --- SUM OVER CORE-SHELLS
          DO 560 JJMP = 1,MPSHEL
C           --- JJ NUMBERS ALL SHELLS CONSECUTIVELY 
            JJ=JJ+1
            XJ = C(1,MPAT)
            YJ = C(2,MPAT)
            ZJ = C(3,MPAT)
            J1 = MPKSTA(JJ)
            J2 = J1+MPKNG(JJ)-1
            LJT = MPKTYP(JJ)
            MINJ = MPKMIN(JJ)
            MAXJ = MPKMAX(JJ)
            LOCJ = MPKLOC(JJ)-MINJ
            RR = (XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
C
C           ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
            IJ = 0
            DO 160 I = MINI,MAXI
               NX = IX(I)
               NY = IY(I)
               NZ = IZ(I)
               DO 140 J = MINJ,MAXJ
                  IJ = IJ+1
                  IJX(IJ) = NX+JX(J)
                  IJY(IJ) = NY+JY(J)
                  IJZ(IJ) = NZ+JZ(J)
C                 --- SET THE TEMPORARY ARRAY STMP TO ZERO
                  STMP(IJ)= ZERO
  140          CONTINUE
  160       CONTINUE
C
C     ----- I PRIMITIVE
C
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
C     ----- J PRIMITIVE
C
               DO 500 JG = J1,J2
                  AJ = EXPMP(JG)
                  AA = AI+AJ
                  AA1 = ONE/AA
                  DUM = AJ*ARRI*AA1
                  IF (DUM .GT. TOL) GO TO 500
                  FAC = EXP(-DUM)
                  CSJ = CSMP(JG)
                  CPJ = CPMP(JG)
                  CDJ = CDMP(JG)
                  CFJ = CFMP(JG)
                  AX = (AXI+AJ*XJ)*AA1
                  AY = (AYI+AJ*YJ)*AA1
                  AZ = (AZI+AJ*ZJ)*AA1
C
C                     ----- DENSITY FACTOR
C
                      NN = 0
                      DUM1 = ZERO
                      DUM2 = ZERO
                      DO 220 I = MINI,MAXI
                       IF (I.EQ.1) DUM1=CSI*FAC
                       IF (I.EQ.2) DUM1=CPI*FAC
                       IF (I.EQ.5) DUM1=CDI*FAC
                       IF (I.EQ.8.AND.NORM) DUM1=DUM1*SQRT3
                       IF (I.EQ.11) DUM1=CFI*FAC
                       IF (I.EQ.14.AND.NORM) DUM1=DUM1*SQRT5
                       IF (I.EQ.20.AND.NORM) DUM1=DUM1*SQRT3
                       IF (I.EQ.21) DUM1=CGI*FAC
                       IF (I.EQ.24.AND.NORM) DUM1=DUM1*SQRT7
                       IF (I.EQ.30.AND.NORM) DUM1=DUM1*SQRT5/SQRT3
                       IF (I.EQ.33.AND.NORM) DUM1=DUM1*SQRT3
                       DO 200 J = MINJ,MAXJ
                        IF (J.EQ.1) THEN
                           DUM2=DUM1*CSJ
                        ELSE IF (J.EQ.2) THEN
                           DUM2=DUM1*CPJ
                        ELSE IF (J.EQ.5) THEN
                           DUM2=DUM1*CDJ
                        ELSE IF (J.EQ.8.AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        ELSE IF (J.EQ.11) THEN
                           DUM2=DUM1*CFJ
                        ELSE IF (J.EQ.14.AND.NORM) THEN
                           DUM2=DUM2*SQRT5
                        ELSE IF (J.EQ.20.AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        END IF
                        NN = NN+1
                        DIJ(NN) = DUM2
  200                   CONTINUE
  220                 CONTINUE
C
C     ----- CALCULATE THE OVERLAP-INTEGRAL
C
                   T = SQRT(AA1)
                   X0 = AX
                   Y0 = AY
                   Z0 = AZ
                   IN = -5
                   DO 320 I = 1,LIT
                     IN = IN+5
                     NI = I
                     DO 300 J = 1,LJT
                        JN = IN+J
                        NJ = J
                        CALL STVINT
                        XIN(JN) = XINT*T
                        YIN(JN) = YINT*T
                        ZIN(JN) = ZINT*T
  300                CONTINUE
  320              CONTINUE
C
                  DO 340 I = 1,IJ
                     NX = IJX(I)
                     NY = IJY(I)
                     NZ = IJZ(I)
                     DUM = XIN(NX)*YIN(NY)*ZIN(NZ)
                     STMP(I) = STMP(I) + DIJ(I)*DUM
 340              CONTINUE
 500           CONTINUE
 520        CONTINUE
C
C     --- CHECK IF THE DIMENSIONS OF SMP(LJ,LI) ARE EXCEEDED
C
            IF ((LOCJ+MAXJ.GT.MPSIZ).OR.(LOCI+MAXI.GT.MPSIZ)) THEN
             WRITE(IW,*) ' THE DIMENSIONS MPSIZ=',MPSIZ
             WRITE(IW,*) ' OF ARRAY SMP HAVE BEEN EXCEEDED'
             WRITE(IW,*) ' *** SEE ROUTINE MCPPRO IN MODULE MCPINT ***'
             WRITE(IW,8723) LOCI,MAXI,LOCJ,MAXJ
 8723 FORMAT(1X,'LOCI=',I8,' MAXI=',I4,' LOCJ=',I8,' MAXJ=',I4)
             CALL ABRT
            END IF
C
C           --- WRITE THE OVERLAP MATRIX ELEMENTS ON SMP
C
            NN = 0
            DO 540 I = MINI,MAXI
               LI = LOCI+I
               DO 530 J = MINJ,MAXJ
                  NN = NN+1
                  LJ = LOCJ+J
                  SMP(LJ,LI)=STMP(NN)
  530          CONTINUE
  540      CONTINUE
C        --- END OF SUM OVER CORE-SHELLS OF GIVEN MP-ATOM
  560    CONTINUE
C     --- END OF SUM OVER ALL ATOMS      [ J SHELL ]
  570    CONTINUE
C     --- END OF SUM OVER VALENCE-SHELLS [ I SHELL ]
  580  CONTINUE
C
C     ---- CALC.THE PROJECTION-OPERATOR CONTRIBUTION FROM    
C     ---- THE OVERLAP-INTEGRALS SMP(LK,LI)
C
C     --- SUM OVER VALENCE-SHELLS II AND JJ
      DO 700 II=1,NSHELL
        DO 690 JJ=1,II
          MINI = KMIN(II)
          MAXI = KMAX(II)
          LOCI = KLOC(II)-MINI
          MINJ = KMIN(JJ)
          MAXJ = KMAX(JJ)
          LOCJ = KLOC(JJ)-MINJ
          IANDJ = II .EQ. JJ
C
          MAX = MAXJ
          NN = 0
          DO 660 I = MINI,MAXI
            LI = LOCI+I
            IN = (LI*(LI-1))/2
            IF (IANDJ) MAX = I
            DO 650 J = MINJ,MAX
              LJ = LOCJ+J
              JN = LJ+IN
              NN = NN+1
C
C             --- SUM OVER ALL ATOMS; SKIP THE ALL-ELECTRON ONES
C             --- KK: CORE-SHELLS CONSECUTIVELY NUMBERED
C
              KK=0
              DO 640 IAT=1,NAT
                IF (MPSKIP(IAT).EQ.1) GOTO 640
                MPSHEL=NOCOSH(IAT)
C               --- SUM OVER THE CORE-SHELLS OF THE MP-ATOM IAT
                DO 630 KKMP=1,MPSHEL
                  KK=KK+1
                  BMP=BPAR(KK)
                  LKKT =MPKTYP(KK)
                  MINMP=MPKMIN(KK)
                  MAXMP=MPKMAX(KK)
                  LOCK =MPKLOC(KK)-MINMP
                  SUM=ZERO
C
C CASE OF CORE D FUNCTIONS 
C
                  IF (LKKT.EQ.3) THEN
                    DO 600 K=1,3
                       LK=LOCK+K+4
                       T1(K)=PT15*SMP(LK,LJ)
  600               CONTINUE
                    DO 602 K=4,6
                       LK=LOCK+K+4
                       T1(K)=SMP(LK,LJ)
  602               CONTINUE
                    DO 605 K=1,6
                       LK=LOCK+K+4
                       T2(K)=SMP(LK,LI)
                       SUM=SUM+T2(K)*T1(K) 
  605               CONTINUE
                    T1K=ZERO
                    T2K=ZERO
                    DO 610 K=1,3
                       T1K=T1K+T1(K)
                       T2K=T2K+T2(K)
  610               CONTINUE
                    SUM=SUM-PT3*T1K*T2K               
C
C CASE OF F CORE FUNCTIONS    
C
                  ELSE IF (LKKT.EQ.4) THEN
                    DO 612 K=1,10
                       LK=LOCK+K+10
  612                  T1(K)=SMP(LK,LJ)
                    DO 614 K=1,10
                       LK=LOCK+K+10
  614                  T2(K)=SMP(LK,LI)
                    DO 616 K=1,10
                       PT12=ONE
                       IF(K.GE.4 .AND. K.LE.9) PT12=1.2D+00
  616                  SUM=SUM+PT12*T1(K)*T2(K)
                       SUM=SUM
     1                        -B23S5 * ( T1(1)*T2(8)+T1(8)*T2(1)
     2                                  +T1(1)*T2(6)+T1(6)*T2(1)
     3                                  +T1(2)*T2(9)+T1(9)*T2(2)
     4                                  +T1(2)*T2(4)+T1(4)*T2(2)
     5                                  +T1(3)*T2(7)+T1(7)*T2(3)
     6                                  +T1(3)*T2(5)+T1(5)*T2(3) )
     7                        -PT03 *  ( T1(8)*T2(6)+T1(6)*T2(8)
     8                                  +T1(9)*T2(4)+T1(4)*T2(9)
     9                                  +T1(7)*T2(5)+T1(5)*T2(7) )
C
C CASE OF S, P CORE FUNCTIONS  
C 
                  ELSE   
                    DO 620 K=MINMP,MAXMP
                    LK=LOCK+K
                    SUM=SUM+SMP(LK,LI)*SMP(LK,LJ)
  620             CONTINUE
                  END IF
                  SUM=SUM*BMP
                  QQ(JN)=QQ(JN)+SUM
C     --- END OF SUM OVER CORE-SHELLS 
  630           CONTINUE
C     --- END OF SUM OVER ATOMS
  640         CONTINUE
C     --- END MINI-MAXI-LOOP FOR SHELL JJ
  650       CONTINUE
C     --- END MINI-MAXI-LOOP FOR SHELL II
  660     CONTINUE
C     --- END OF SUM OVER II-SHELLS
  690   CONTINUE
C     --- END OF SUM OVER JJ-SHELLS
  700 CONTINUE
C
      CALL DAREAD(IDAF,IODA,GG,L2,11,0)
      CALL VADD(GG,1,QQ,1,GG,1,L2)
      CALL DAWRIT(IDAF,IODA,GG,LU2,11,0)
C
C     CALL DAREAD(IDAF,IODA,GG,L2,77,0)
C     CALL VADD(GG,1,QQ,1,GG,1,L2)
C     CALL DAWRIT(IDAF,IODA,GG,LU2,77,0)
C
C     --- PRINT INFORMATION IF REQUIRED 
C
      IF(DBG) THEN
         WRITE(IW,9020)
         CALL PRTRIL(QQ,L1)
         CALL TIMIT(1)
      END IF
      RETURN
C
 9000 FORMAT(/10X,33("*")/,10X,
     *     'MCP PROJECTION-OPERATOR INTEGRALS'/10X,33(1H*))
 9020 FORMAT(/10X,'MCP PROJECTION-OPERATOR CONTRIBUTION MATRIX'
     *     /10X,43(1H-) )
      END
