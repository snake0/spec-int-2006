C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C 17 APR 02 - MWS - SYNCH UP EFMULT AND FRGINF COMMON
C 28 SEP 97 - MWS - CONVERT PARALLEL CALLS FROM TCGMSG TO DDI
C 10 SEP 96 - MWS - INCLUDE THIS CODE IN DISTRIBUTION VERSION
C  9 JAN 96 - WC  - REMOVE EXTRANEOUS OUTPUT
C 23 NOV 94 - MWS - REMOVE ALL FTNCHEK ERRORS
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C 30 JUL 91 - JHJ - EFXDER(X=C,D,Q,O): ADDED DOX ARRAY.
C 23 JUL 91 - JHJ - COLLECTED EFXDER AND XDXYZ INTO THIS MODULE.
C                   CONSTRUCTED EFCDER FROM DRG'S CODE IN TVDER.
C 24 MAY 90 - DRG - EFODER, ODXYZ AND DNUCO FOR EFO GRADIENTS.
C 23 MAY 90 - DRG - DNUCQ FOR EFQ - NUCLEI GRADIENTS.
C 21 MAY 90 - DRG - EFQDER AND QDXYZ TO GET EFQ GRADIENTS.
C 17 MAY 90 - DRG - DNUCD FOR EFD - NUCLEI GRADIENTS.
C 16 MAY 90 - DRG - EFDDER AND PDXYZ TO GET EFD GRADIENTS.
C
C*MODULE EFGRDB  *DECK EFCDER
      SUBROUTINE EFCDER(DM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL ISKIP
      LOGICAL OUT,DBG,NORM,GOPARR,DSKWRK,MASWRK,NXT
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      DIMENSION DM(*)
C
      DIMENSION ISKIP(35),INDX(70)
      DIMENSION IX(35),IY(35),IZ(35),JX(20),JY(20),JZ(20)
      DIMENSION IJG(210),IJX(210),IJY(210),IJZ(210)
      DIMENSION XIN(200),YIN(200),ZIN(200),G(210),DIJ(210)
      DIMENSION FIJ(210),GIJ(210)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /DSTV  / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,
     *                XJ,YJ,ZJ,NI,NJ,CX,CY,CZ
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /GRAD  / DE(3,MXATM)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
      PARAMETER (RLN10=2.30258D+00)
      PARAMETER (PI212=1.1283791670955D+00)
      PARAMETER (SQRT3=1.73205080756888D+00)
      PARAMETER (SQRT5=2.23606797749979D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /, DEBUG/8HDEBUG   /, DBUGME/8HTVDER   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA CHECK_STR/"CHECK   "/, DEBUG_STR/"DEBUG   "/, 
     * DBUGME_STR/"TVDER   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA GRD1/8HGRD1    /
#else
      CHARACTER*8 :: GRD1_STR
      EQUIVALENCE (GRD1, GRD1_STR)
      DATA GRD1_STR/"GRD1    "/
#endif
      DATA INDX / 1, 2, 3, 4, 5, 6, 7, 8, 9,10,
     1           11,12,13,14,15,16,17,18,19,20,
     2           -0,-0,-0,-0,-0,-0,-0,-0,-0,-0,
     3           -0,-0,-0,-0,-0,
     4           -0, 1, 2, 3, 1, 2, 3, 4, 5, 6,
     5            4, 5, 6, 7, 8, 9,10,11,12,13,
     6            7, 8, 9,10,11,12,13,14,15,16,
     7           17,18,19,20,21/
      DATA JX / 0, 1, 0, 0, 2, 0, 0, 1, 1, 0,
     1          3, 0, 0, 2, 2, 1, 0, 1, 0, 1/
      DATA IX / 1, 5, 1, 1, 9, 1, 1, 5, 5, 1,
     1         13, 1, 1, 9, 9, 5, 1, 5, 1, 5,
     2         17, 1, 1,13,13, 5, 1, 5, 1, 9,
     3          9, 1, 9, 5, 5/
      DATA JY / 0, 0, 1, 0, 0, 2, 0, 1, 0, 1,
     1          0, 3, 0, 1, 0, 2, 2, 0, 1, 1/
      DATA IY / 1, 1, 5, 1, 1, 9, 1, 5, 1, 5,
     1          1,13, 1, 5, 1, 9, 9, 1, 5, 5,
     2          1,17, 1, 5, 1,13,13, 1, 5, 9,
     3          1, 9, 5, 9, 5/
      DATA JZ / 0, 0, 0, 1, 0, 0, 2, 0, 1, 1,
     1          0, 0, 3, 0, 1, 0, 1, 2, 2, 1/
      DATA IZ / 1, 1, 1, 5, 1, 1, 9, 1, 5, 5,
     1          1, 1,13, 1, 5, 1, 5, 9, 9, 5,
     2          1, 1,17, 1, 5, 1, 5,13,13, 1,
     3          9, 9, 5, 5, 9/
      DATA QLIM/1.D-08/
C
C
C     INITIALIZATION FOR PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      LCNT = -1
      DBG = .FALSE.
      OUT = EXETYP.EQ.GRD1
      IF (EXETYP.EQ.DEBUG  .OR. EXETYP.EQ.DBUGME  .OR.
     *    NPRINT.EQ.-3) THEN
         OUT = .TRUE.
         DBG = .TRUE.
      END IF
      IF (DBG .AND. MASWRK) WRITE (IW,9008)
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
      IF (EXETYP .EQ. CHECK) GO TO 1120
C
C     GRADIENT CONTRIBUTION FROM EFFECTIVE POINT DIPOLES.
C
C     ----- I SHELL
C
      DO 1060 II = 1,NSHELL
C
C           GO PARALLEL!
C
      IF(NXT .AND. GOPARR) THEN
         LCNT = LCNT + 1
         IF(LCNT.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
         IF(NEXT.NE.LCNT) GO TO 1060
      END IF
      IAT = KATOM(II)
      XI = C(1,IAT)
      YI = C(2,IAT)
      ZI = C(3,IAT)
      I1 = KSTART(II)
      I2 = I1+KNG(II)-1
      LIT = KTYPE(II)+1
      MINI = KMIN(II)
      MAXI = KMAX(II)
      LOCI = KLOC(II)-MINI
      DO 100 I=1,35
 100  ISKIP(I)=.TRUE.
      DO 260 I=MINI,MAXI
      GO TO (110,140,260,260,180,260,260,260,260,260,
     1       220,260,260,260,260,260,260,260,260,260),I
 110  DO 120 K=2,4
 120  ISKIP(K)=.FALSE.
      GO TO 260
 140  DO 160 K=5,10
 160  ISKIP(K)=.FALSE.
      ISKIP(1)=.FALSE.
      GO TO 260
 180  DO 200 K=2,4
 200  ISKIP(K)=.FALSE.
      DO 210 K=11,20
 210  ISKIP(K)=.FALSE.
      GO TO 260
 220  DO 240 K=5,10
 240  ISKIP(K)=.FALSE.
      DO 250 K=21,35
 250  ISKIP(K)=.FALSE.
 260  CONTINUE
C
C     ----- J SHELL
C
      DO 1040 JJ = 1,NSHELL
C
C           GO PARALLEL!
C
        IF((.NOT.NXT) .AND. GOPARR) THEN
           IPCOUNT = IPCOUNT + 1
           IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 1040
        END IF
C
      JAT = KATOM(JJ)
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
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      N0 = 0
      IF (LIT .GE. 4) N0 = 35
      IJ = 0
      DO 340 I = 1,35
      IF (ISKIP(I)) GO TO 340
      IN = INDX(I+N0)
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
      DO 320 J = MINJ,MAXJ
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
      IJG(IJ) = IN+21*(J-MINJ)
  320 CONTINUE
  340 CONTINUE
      DO 360 I = 1,IJ
      N = IJG(I)
  360 G(N) = ZERO
C
C     ----- I PRIMITIVE
C
      DO 840 IG = I1,I2
      AI = EX(IG)
      ARRI = AI*RR
      AXI = AI*XI
      AYI = AI*YI
      AZI = AI*ZI
      DUM = AI+AI
      CSI = CP(IG)
      CPI = CS(IG)*DUM
      IF (LIT .EQ. 4) CPI = CD(IG)
      CDI = CP(IG)*DUM
      IF (LIT .EQ. 5) CDI = CF(IG)
      CFI = CD(IG)*DUM
      CGI = CF(IG)*DUM
C
C     ----- J PRIMITIVE
C
      DO 820 JG = J1,J2
      AJ = EX(JG)
      AA = AI+AJ
      AA1 = ONE/AA
      DUM = AJ*ARRI*AA1
      IF (DUM .GT. TOL) GO TO 820
      FAC = EXP(-DUM)
      CSJ = CS(JG)
      CPJ = CP(JG)
      CDJ = CD(JG)
      CFJ = CF(JG)
      AX = (AXI+AJ*XJ)*AA1
      AY = (AYI+AJ*YJ)*AA1
      AZ = (AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR
C
      NN = 0
      DUM1 = ZERO
      DUM2 = DUM1
      DO 600 I=1,35
      IF(ISKIP(I)) GO TO 600
      GO TO (370,380,420,420,390,420,420,420,420,420,
     1       400,420,420,420,420,420,420,420,420,420,
     2       410,420,420,420,420,420,420,420,420,420,
     3       420,420,420,420,420),I
 370  DUM1=CSI*FAC
      GO TO 420
 380  DUM1=CPI*FAC
      GO TO 420
 390  DUM1=CDI*FAC
      GO TO 420
 400  DUM1=CFI*FAC
      GO TO 420
 410  DUM1 = CGI*FAC
 420  CONTINUE
      DO 580 J=MINJ,MAXJ
      GO TO (430,440,560,560,460,560,560,480,560,560,
     1       500,560,560,520,560,560,560,560,560,540),J
  430 DUM2=DUM1*CSJ
      GO TO 560
  440 DUM2=DUM1*CPJ
      GO TO 560
  460 DUM2=DUM1*CDJ
      GO TO 560
  480 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 560
  500 DUM2 = DUM1 * CFJ
      GO TO 560
  520 DUM2 = DUM2 *SQRT5
      GO TO 560
  540 DUM2 = DUM2 * SQRT3
  560 NN=NN+1
  580 DIJ(NN)=DUM2
  600 CONTINUE
C
C     ....BASIC CHARGE TERM
C
      DUM = PI212*AA1
      DO 700 I=1,IJ
 700  FIJ(I)=DIJ(I)*DUM
      AAX=AA*AX
      AAY=AA*AY
      AAZ=AA*AZ
      DO 818 IC=1,NMTTPT
      IF(.NOT.DOMONO(IC)) GO TO 818
      ZNUC=-(EFCHG(1,IC)+EFCHG(2,IC))
      CX=EFC(1,IC)
      CY=EFC(2,IC)
      CZ=EFC(3,IC)
      XX=AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
C     CALL RT123
      IF(NROOTS.LE.3)CALL RT123
      IF(NROOTS.EQ.4)CALL ROOT4
      IF(NROOTS.EQ.5)CALL ROOT5
      MM=0
      DO 806 K=1,NROOTS
      UU=AA*U(K)
      WW=W(K)*ZNUC
      TT=ONE/(AA+UU)
      T= SQRT(TT)
      X0=(AAX+UU*CX)*TT
      Y0=(AAY+UU*CY)*TT
      Z0=(AAZ+UU*CZ)*TT
      IN=-4+MM
      DO 804 I=1,LIT
      IN=IN+4
      NI=I
      DO 804 J=1,LJT
      JN=IN+J
      NJ=J
      CALL VINT
      XIN(JN)=XINT
      YIN(JN)=YINT
      ZIN(JN)=ZINT*WW
 804  CONTINUE
 806  MM=MM+20
      DO 810 I=1,IJ
      N=IJG(I)
      NX=IJX(I)
      NY=IJY(I)
      NZ=IJZ(I)
      DUM=ZERO
      MM=0
      DO 808 K=1,NROOTS
      DUM=DUM+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM)
 808  MM=MM+20
 810  G(N)=G(N)+DUM*FIJ(I)
C     SUBTRACT DAMPING FUNCTION TERM.
      ALFA = EFATRM(IC)
      BETA = EFBTRM(IC)
      IF (ABS(ALFA).LE.QLIM ) GO TO 818
      DUM=PI212/(AA + ALFA)
      DO 811 I=1,IJ
 811  GIJ(I)=DIJ(I)*DUM
      ZNUC = -EFCHG(1,IC)
      PCSQ = (AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2
      XX=AA*AA*PCSQ/(AA+ALFA)
      PREI = EXP(-AA*ALFA*PCSQ/(AA+ALFA))
C     CALL RT123
      IF(NROOTS.LE.3)CALL RT123
      IF(NROOTS.EQ.4)CALL ROOT4
      IF(NROOTS.EQ.5)CALL ROOT5
      MM=0
      DO 814 K=1,NROOTS
      UU=(ALFA+AA)*U(K)
      WW=W(K)*ZNUC
      TT=ONE/(AA+UU+ALFA)
      T= SQRT(TT)
      X0=(AAX+(UU+ALFA)*CX)*TT
      Y0=(AAY+(UU+ALFA)*CY)*TT
      Z0=(AAZ+(UU+ALFA)*CZ)*TT
      IN=-4+MM
      DO 812 I=1,LIT
      IN=IN+4
      NI=I
      DO 812 J=1,LJT
      JN=IN+J
      NJ=J
      CALL VINT
      XIN(JN)=XINT
      YIN(JN)=YINT
      ZIN(JN)=ZINT*WW
 812  CONTINUE
 814  MM=MM+20
      DO 817 I=1,IJ
      N=IJG(I)
      NX=IJX(I)
      NY=IJY(I)
      NZ=IJZ(I)
      DUM=ZERO
      MM=0
      DO 816 K=1,NROOTS
      DUM=DUM+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM)
 816  MM=MM+20
      DAMPT = -GIJ(I) * PREI * BETA *DUM
 817  G(N)=G(N) + DAMPT
 818  CONTINUE
C
C     ----- END OF PRIMITIVE LOOPS -----
C
  820 CONTINUE
  840 CONTINUE
C
C     ----- FORM INTEGRALS OVER DERIVATIVES -----
C
      NN = 0
      N = 1
      DO 900 J = MINJ,MAXJ
      IF (MINI .GT. 1) GO TO 860
      NN = NN+1
      XIN(NN) = G(N+ 1)
      YIN(NN) = G(N+ 2)
      ZIN(NN) = G(N+ 3)
      IF (MAXI .EQ. 1) GO TO 900
  860 IF (MINI .GT. 2) GO TO 880
      NN = NN+1
      XIN(NN) = (G(N+ 4)-G(N ))
      YIN(NN) = G(N+ 7)
      ZIN(NN) = G(N+ 8)
      NN = NN+1
      XIN(NN) = G(N+ 7)
      YIN(NN) = (G(N+ 5)-G(N ))
      ZIN(NN) = G(N+ 9)
      NN = NN+1
      XIN(NN) = G(N+ 8)
      YIN(NN) = G(N+ 9)
      ZIN(NN) = (G(N+ 6)-G(N ))
      IF (MAXI .EQ. 4) GO TO 900
  880 CONTINUE
      IF (MINI .GT. 5) GO TO 890
      NN = NN+1
      XIN(NN) = (G(N+ 3)-G(N )-G(N ))
      YIN(NN) = G(N+ 6)
      ZIN(NN) = G(N+ 7)
      NN = NN+1
      XIN(NN) = G(N+ 8)
      YIN(NN) = (G(N+ 4)-G(N+ 1)-G(N+ 1))
      ZIN(NN) = G(N+ 9)
      NN = NN+1
      XIN(NN) = G(N+10)
      YIN(NN) = G(N+11)
      ZIN(NN) = (G(N+ 5)-G(N+ 2)-G(N+ 2))
      NN = NN+1
      DUM = ONE
      IF (NORM) DUM = SQRT3
      XIN(NN) = DUM*(G(N+ 6)-G(N+ 1))
      YIN(NN) = DUM*(G(N+ 8)-G(N ))
      ZIN(NN) = DUM* G(N+12)
      NN = NN+1
      XIN(NN) = DUM*(G(N+ 7)-G(N+ 2))
      YIN(NN) = DUM* G(N+12)
      ZIN(NN) = DUM*(G(N+10)-G(N ))
      NN = NN+1
      XIN(NN) = DUM* G(N+12)
      YIN(NN) = DUM*(G(N+ 9)-G(N+ 2))
      ZIN(NN) = DUM*(G(N+11)-G(N+ 1))
      IF(MAXI.EQ.10) GO TO 900
 890  CONTINUE
      NN=NN+1
      XIN(NN)=(G(N+ 6)-G(N   )-G(N   )-G(N   ))
      YIN(NN)= G(N+ 9)
      ZIN(NN)= G(N+10)
      NN=NN+1
      XIN(NN)= G(N+11)
      YIN(NN)=(G(N+ 7)-G(N+ 1)-G(N+ 1)-G(N+ 1))
      ZIN(NN)= G(N+12)
      NN=NN+1
      XIN(NN)= G(N+13)
      YIN(NN)= G(N+14)
      ZIN(NN)=(G(N+ 8)-G(N+ 2)-G(N+ 2)-G(N+ 2))
      NN=NN+1
      DUM=ONE
      IF(NORM) DUM=SQRT5
      XIN(NN)=DUM* (G(N+ 9)-G(N+ 3)-G(N+ 3))
      YIN(NN)=DUM* (G(N+15)-G(N  ))
      ZIN(NN)=DUM* G(N+18)
      NN=NN+1
      XIN(NN)=DUM* (G(N+10)-G(N+ 4)-G(N+ 4))
      YIN(NN)=DUM* G(N+18)
      ZIN(NN)=DUM* (G(N+16)-G(N  ))
      NN=NN+1
      XIN(NN)=DUM* (G(N+15)-G(N+ 1))
      YIN(NN)=DUM* (G(N+11)-G(N+ 3)-G(N+ 3))
      ZIN(NN)=DUM*  G(N+19)
      NN=NN+1
      XIN(NN)=DUM* G(N+19)
      YIN(NN)=DUM* (G(N+12)-G(N+ 5)-G(N+ 5))
      ZIN(NN)=DUM* (G(N+17)-G(N+ 1))
      NN=NN+1
      XIN(NN)=DUM* (G(N+16)-G(N+ 2))
      YIN(NN)=DUM*  G(N+20)
      ZIN(NN)=DUM* (G(N+13)-G(N+ 4)-G(N+ 4))
      NN=NN+1
      XIN(NN)=DUM*  G(N+20)
      YIN(NN)=DUM* (G(N+17)-G(N+ 2))
      ZIN(NN)=DUM* (G(N+14)-G(N+ 5)-G(N+ 5))
      NN=NN+1
      IF(NORM) DUM=DUM*SQRT3
      XIN(NN)=DUM* (G(N+18)-G(N+ 5))
      YIN(NN)=DUM* (G(N+19)-G(N+ 4))
      ZIN(NN)=DUM* (G(N+20)-G(N+ 3))
  900 N = N+21
C
C     ----- CALCULATE CONTRIBUTION TO GRADIENT -----
C
      N = 0
      DO 980 J = MINJ,MAXJ
      JN = LOCJ+J
      DO 980 I = MINI,MAXI
      N = N+1
      IN = LOCI+I
      IF (IN-JN) 920,940,940
  920 ID = JN
      JD = IN
      GO TO 960
  940 ID = IN
      JD = JN
  960 NN = (ID*(ID-1))/2+JD
      DUM = DM(NN)
      DUM = DUM+DUM
      DE(1,IAT) = DE(1,IAT)+DUM*XIN(N)
      DE(2,IAT) = DE(2,IAT)+DUM*YIN(N)
      DE(3,IAT) = DE(3,IAT)+DUM*ZIN(N)
      IF (DBG .AND. MASWRK) WRITE (IW,9108) II,JJ,I,J,NN,DUM,XIN(N),
     *              YIN(N),ZIN(N),IAT,DE(1,IAT),DE(2,IAT),DE(3,IAT)
  980 CONTINUE
      IF (DBG) CALL EGOUT(DE,NAT)
 1040 CONTINUE
 1060 CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
      IF (OUT) THEN
         IF (MASWRK) WRITE (IW,9008)
         CALL EGOUT(DE,NAT)
      END IF
 1120 CONTINUE
      IF (MASWRK.AND.OUT) THEN
         WRITE (IW,9088)
         CALL TIMIT(1)
      ENDIF
      IF(GOPARR .AND. NXT) CALL DDI_DLBRESET
      RETURN
C
 9008 FORMAT(/10X,45("-")/10X,'EFC CONTRIBUTIONS',
     *        ' TO THE GRADIENT'/10X,45(1H-))
 9088 FORMAT(/1X,'..... END OF EFC GRADIENT ......')
 9108 FORMAT(1X,'II,JJ,I,J,NN=',4I5,I6,4E20.12/
     *       1X,'IAT,DE(1,2,3)',18X,I5,3E20.12)
      END
C*MODULE EFGRDB  *DECK EFDDER
      SUBROUTINE EFDDER(DM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL ISKIP
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
      LOGICAL OUT,DBG,NORM,GOPARR,DSKWRK,MASWRK,NXT
C
      DIMENSION DM(*)
      DIMENSION ISKIP(35),INDX(70)
      DIMENSION IX(35),IY(35),IZ(35),JX(20),JY(20),JZ(20)
      DIMENSION IJG(210),IJX(210),IJY(210),IJZ(210)
      DIMENSION XIN(200),YIN(200),ZIN(200),G(210),DIJ(210)
      DIMENSION FIJ(210),GIJ(210)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /DSTV  / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,
     *                XJ,YJ,ZJ,NI,NJ,CX,CY,CZ
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /GRAD  / DE(3,MXATM)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
      PARAMETER (RLN10=2.30258D+00)
      PARAMETER (PI212=1.1283791670955D+00)
      PARAMETER (SQRT3=1.73205080756888D+00)
      PARAMETER (SQRT5=2.23606797749979D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /, DEBUG/8HDEBUG   /, DBUGME/8HTVDER   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA CHECK_STR/"CHECK   "/, DEBUG_STR/"DEBUG   "/, 
     * DBUGME_STR/"TVDER   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA GRD1/8HGRD1    /
#else
      CHARACTER*8 :: GRD1_STR
      EQUIVALENCE (GRD1, GRD1_STR)
      DATA GRD1_STR/"GRD1    "/
#endif
      DATA INDX / 1, 2, 3, 4, 5, 6, 7, 8, 9,10,
     1           11,12,13,14,15,16,17,18,19,20,
     2           -0,-0,-0,-0,-0,-0,-0,-0,-0,-0,
     3           -0,-0,-0,-0,-0,
     4           -0, 1, 2, 3, 1, 2, 3, 4, 5, 6,
     5            4, 5, 6, 7, 8, 9,10,11,12,13,
     6            7, 8, 9,10,11,12,13,14,15,16,
     7           17,18,19,20,21/
      DATA JX / 0, 1, 0, 0, 2, 0, 0, 1, 1, 0,
     1          3, 0, 0, 2, 2, 1, 0, 1, 0, 1/
      DATA IX / 1, 5, 1, 1, 9, 1, 1, 5, 5, 1,
     1         13, 1, 1, 9, 9, 5, 1, 5, 1, 5,
     2         17, 1, 1,13,13, 5, 1, 5, 1, 9,
     3          9, 1, 9, 5, 5/
      DATA JY / 0, 0, 1, 0, 0, 2, 0, 1, 0, 1,
     1          0, 3, 0, 1, 0, 2, 2, 0, 1, 1/
      DATA IY / 1, 1, 5, 1, 1, 9, 1, 5, 1, 5,
     1          1,13, 1, 5, 1, 9, 9, 1, 5, 5,
     2          1,17, 1, 5, 1,13,13, 1, 5, 9,
     3          1, 9, 5, 9, 5/
      DATA JZ / 0, 0, 0, 1, 0, 0, 2, 0, 1, 1,
     1          0, 0, 3, 0, 1, 0, 1, 2, 2, 1/
      DATA IZ / 1, 1, 1, 5, 1, 1, 9, 1, 5, 5,
     1          1, 1,13, 1, 5, 1, 5, 9, 9, 5,
     2          1, 1,17, 1, 5, 1, 5,13,13, 1,
     3          9, 9, 5, 5, 9/
      DATA QLIM/1.D-08/
C
C
C     INITIALIZATION FOR PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      LCNT = -1
      DBG = .FALSE.
      OUT = EXETYP.EQ.GRD1
      IF (EXETYP.EQ.DEBUG  .OR. EXETYP.EQ.DBUGME  .OR.
     *    NPRINT.EQ.-3) THEN
         OUT = .TRUE.
         DBG = .TRUE.
      END IF
      IF (DBG .AND. MASWRK) WRITE (IW,9008)
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
      IF (EXETYP .EQ. CHECK) GO TO 1120
C
C     GRADIENT CONTRIBUTION FROM EFFECTIVE POINT DIPOLES.
C
C     ----- I SHELL
C
      DO 1060 II = 1,NSHELL
C
C           GO PARALLEL!
C
      IF(NXT .AND. GOPARR) THEN
         LCNT = LCNT + 1
         IF(LCNT.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
         IF(NEXT.NE.LCNT) GO TO 1060
      END IF
      IAT = KATOM(II)
      XI = C(1,IAT)
      YI = C(2,IAT)
      ZI = C(3,IAT)
      I1 = KSTART(II)
      I2 = I1+KNG(II)-1
      LIT = KTYPE(II)+1
      MINI = KMIN(II)
      MAXI = KMAX(II)
      LOCI = KLOC(II)-MINI
      DO 100 I=1,35
 100  ISKIP(I)=.TRUE.
      DO 260 I=MINI,MAXI
      GO TO (110,140,260,260,180,260,260,260,260,260,
     1       220,260,260,260,260,260,260,260,260,260),I
 110  DO 120 K=2,4
 120  ISKIP(K)=.FALSE.
      GO TO 260
 140  DO 160 K=5,10
 160  ISKIP(K)=.FALSE.
      ISKIP(1)=.FALSE.
      GO TO 260
 180  DO 200 K=2,4
 200  ISKIP(K)=.FALSE.
      DO 210 K=11,20
 210  ISKIP(K)=.FALSE.
      GO TO 260
 220  DO 240 K=5,10
 240  ISKIP(K)=.FALSE.
      DO 250 K=21,35
 250  ISKIP(K)=.FALSE.
 260  CONTINUE
C
C     ----- J SHELL
C
      DO 1040 JJ = 1,NSHELL
C
C           GO PARALLEL!
C
        IF((.NOT.NXT) .AND. GOPARR) THEN
           IPCOUNT = IPCOUNT + 1
           IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 1040
        END IF
C
      JAT = KATOM(JJ)
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
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      N0 = 0
      IF (LIT .GE. 4) N0 = 35
      IJ = 0
      DO 340 I = 1,35
      IF (ISKIP(I)) GO TO 340
      IN = INDX(I+N0)
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
      DO 320 J = MINJ,MAXJ
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
      IJG(IJ) = IN+21*(J-MINJ)
  320 CONTINUE
  340 CONTINUE
      DO 360 I = 1,IJ
      N = IJG(I)
  360 G(N) = ZERO
C
C     ----- I PRIMITIVE
C
      DO 840 IG = I1,I2
      AI = EX(IG)
      ARRI = AI*RR
      AXI = AI*XI
      AYI = AI*YI
      AZI = AI*ZI
      DUM = AI+AI
      CSI = CP(IG)
      CPI = CS(IG)*DUM
      IF (LIT .EQ. 4) CPI = CD(IG)
      CDI = CP(IG)*DUM
      IF (LIT .EQ. 5) CDI = CF(IG)
      CFI = CD(IG)*DUM
      CGI = CF(IG)*DUM
C
C     ----- J PRIMITIVE
C
      DO 820 JG = J1,J2
      AJ = EX(JG)
      AA = AI+AJ
      AA1 = ONE/AA
      DUM = AJ*ARRI*AA1
      IF (DUM .GT. TOL) GO TO 820
      FAC = EXP(-DUM)
      CSJ = CS(JG)
      CPJ = CP(JG)
      CDJ = CD(JG)
      CFJ = CF(JG)
      AX = (AXI+AJ*XJ)*AA1
      AY = (AYI+AJ*YJ)*AA1
      AZ = (AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR
C
      NN = 0
      DUM1 = ZERO
      DUM2 = DUM1
      DO 600 I=1,35
      IF(ISKIP(I)) GO TO 600
      GO TO (370,380,420,420,390,420,420,420,420,420,
     1       400,420,420,420,420,420,420,420,420,420,
     2       410,420,420,420,420,420,420,420,420,420,
     3       420,420,420,420,420),I
 370  DUM1=CSI*FAC
      GO TO 420
 380  DUM1=CPI*FAC
      GO TO 420
 390  DUM1=CDI*FAC
      GO TO 420
 400  DUM1=CFI*FAC
      GO TO 420
 410  DUM1 = CGI*FAC
 420  CONTINUE
      DO 580 J=MINJ,MAXJ
      GO TO (430,440,560,560,460,560,560,480,560,560,
     1       500,560,560,520,560,560,560,560,560,540),J
  430 DUM2=DUM1*CSJ
      GO TO 560
  440 DUM2=DUM1*CPJ
      GO TO 560
  460 DUM2=DUM1*CDJ
      GO TO 560
  480 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 560
  500 DUM2 = DUM1 * CFJ
      GO TO 560
  520 DUM2 = DUM2 *SQRT5
      GO TO 560
  540 DUM2 = DUM2 * SQRT3
  560 NN=NN+1
  580 DIJ(NN)=DUM2
  600 CONTINUE
C
C     ..... BASIC DIPOLE TERM
C
      DUM = 2*PI212*AA1
      DO 700 I = 1,IJ
  700 FIJ(I) = DIJ(I)*DUM
      AAX = AA*AX
      AAY = AA*AY
      AAZ = AA*AZ
      DO 819 IC = 1,NMTTPT
      IF(.NOT.DODIPO(IC)) GO TO 819
      ALFA = EFATRM(IC)
      BETA = EFBTRM(IC)
      CX = EFC(1,IC)
      CY = EFC(2,IC)
      CZ = EFC(3,IC)
      XMU = EFDIP(1,IC)
      YMU = EFDIP(2,IC)
      ZMU = EFDIP(3,IC)
      DUMA = 2*PI212/(AA+ALFA)
      DO 710 I=1,IJ
      GIJ(I) = DIJ(I)*DUMA
 710  CONTINUE
      XX = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
C     CALL RT123
      IF(NROOTS.LE.3)CALL RT123
      IF(NROOTS.EQ.4)CALL ROOT4
      IF(NROOTS.EQ.5)CALL ROOT5
      MM = 0
      DO 740 K = 1,NROOTS
      UU = AA*U(K)
      WW = W(K)
      WW = WW*UU
      TT = ONE/(AA+UU)
      T = SQRT(TT)
      X0 = (AAX+UU*CX)*TT
      Y0 = (AAY+UU*CY)*TT
      Z0 = (AAZ+UU*CZ)*TT
      IN = -4+MM
      DO 720 I = 1,LIT
      IN = IN+4
      NI = I
      DO 720 J = 1,LJT
      JN = IN+J
      NJ = J
      CALL VINT
      XIN(JN) = XINT
      YIN(JN) = YINT
      ZIN(JN) = ZINT*WW
      CALL PDXYZ
      XIN(JN+100) = XINT
      YIN(JN+100) = YINT
      ZIN(JN+100) = ZINT*WW
  720 CONTINUE
  740 MM = MM+20
      DO 780 I = 1,IJ
      N = IJG(I)
      NX = IJX(I)
      NY = IJY(I)
      NZ = IJZ(I)
      DUMX = ZERO
      DUMY = ZERO
      DUMZ = ZERO
      MM = 0
      DO 760 K = 1,NROOTS
      DUMX = DUMX+XIN(NX+MM+100)*YIN(NY+MM    )*ZIN(NZ+MM    )
      DUMY = DUMY+XIN(NX+MM    )*YIN(NY+MM+100)*ZIN(NZ+MM    )
      DUMZ = DUMZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+100)
  760 MM = MM+20
      DUM = FIJ(I)
  780 G(N) = G(N)-DUM*(DUMX*XMU+DUMY*YMU+DUMZ*ZMU)
C     SUBTRACT DAMPING FUNCTION TERM.
      IF (ABS(ALFA).LE.QLIM ) GO TO 818
      PCSQ = (AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2
      XX=AA*AA*PCSQ/(AA+ALFA)
      PREI = EXP(-AA*ALFA*PCSQ/(AA+ALFA))
C     CALL RT123
      IF(NROOTS.LE.3)CALL RT123
      IF(NROOTS.EQ.4)CALL ROOT4
      IF(NROOTS.EQ.5)CALL ROOT5
      MM=0
      DO 814 K=1,NROOTS
      UU=(ALFA+AA)*U(K)
      WW=W(K)
      WW=WW*UU
      TT=ONE/(AA+UU+ALFA)
      T= SQRT(TT)
      X0=(AAX+(UU+ALFA)*CX)*TT
      Y0=(AAY+(UU+ALFA)*CY)*TT
      Z0=(AAZ+(UU+ALFA)*CZ)*TT
      IN=-4+MM
      DO 812 I=1,LIT
      IN=IN+4
      NI=I
      DO 812 J=1,LJT
      JN=IN+J
      NJ=J
      CALL VINT
      XIN(JN)=XINT
      YIN(JN)=YINT
      ZIN(JN)=ZINT*WW
      CALL PDXYZ
      XIN(JN+100)=XINT
      YIN(JN+100)=YINT
      ZIN(JN+100)=ZINT*WW
 812  CONTINUE
 814  MM=MM+20
      DO 817 I=1,IJ
      N=IJG(I)
      NX=IJX(I)
      NY=IJY(I)
      NZ=IJZ(I)
      DUMX = ZERO
      DUMY = ZERO
      DUMZ = ZERO
      MM=0
      DO 816 K=1,NROOTS
      DUMX = DUMX+XIN(NX+MM+100)*YIN(NY+MM    )*ZIN(NZ+MM    )
      DUMY = DUMY+XIN(NX+MM    )*YIN(NY+MM+100)*ZIN(NZ+MM    )
      DUMZ = DUMZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+100)
 816  MM=MM+20
      DAMPT = -GIJ(I) * PREI * BETA
 817  G(N)=G(N) - DAMPT*(DUMX*XMU+DUMY*YMU+DUMZ*ZMU)
 818  CONTINUE
 819  CONTINUE
C
C     ----- END OF PRIMITIVE LOOPS -----
C
  820 CONTINUE
  840 CONTINUE
C
C     ----- FORM INTEGRALS OVER DERIVATIVES -----
C
      NN = 0
      N = 1
      DO 900 J = MINJ,MAXJ
      IF (MINI .GT. 1) GO TO 860
      NN = NN+1
      XIN(NN) = G(N+ 1)
      YIN(NN) = G(N+ 2)
      ZIN(NN) = G(N+ 3)
      IF (MAXI .EQ. 1) GO TO 900
  860 IF (MINI .GT. 2) GO TO 880
      NN = NN+1
      XIN(NN) = (G(N+ 4)-G(N ))
      YIN(NN) = G(N+ 7)
      ZIN(NN) = G(N+ 8)
      NN = NN+1
      XIN(NN) = G(N+ 7)
      YIN(NN) = (G(N+ 5)-G(N ))
      ZIN(NN) = G(N+ 9)
      NN = NN+1
      XIN(NN) = G(N+ 8)
      YIN(NN) = G(N+ 9)
      ZIN(NN) = (G(N+ 6)-G(N ))
      IF (MAXI .EQ. 4) GO TO 900
  880 CONTINUE
      IF (MINI .GT. 5) GO TO 890
      NN = NN+1
      XIN(NN) = (G(N+ 3)-G(N )-G(N ))
      YIN(NN) = G(N+ 6)
      ZIN(NN) = G(N+ 7)
      NN = NN+1
      XIN(NN) = G(N+ 8)
      YIN(NN) = (G(N+ 4)-G(N+ 1)-G(N+ 1))
      ZIN(NN) = G(N+ 9)
      NN = NN+1
      XIN(NN) = G(N+10)
      YIN(NN) = G(N+11)
      ZIN(NN) = (G(N+ 5)-G(N+ 2)-G(N+ 2))
      NN = NN+1
      DUM = ONE
      IF (NORM) DUM = SQRT3
      XIN(NN) = DUM*(G(N+ 6)-G(N+ 1))
      YIN(NN) = DUM*(G(N+ 8)-G(N ))
      ZIN(NN) = DUM* G(N+12)
      NN = NN+1
      XIN(NN) = DUM*(G(N+ 7)-G(N+ 2))
      YIN(NN) = DUM* G(N+12)
      ZIN(NN) = DUM*(G(N+10)-G(N ))
      NN = NN+1
      XIN(NN) = DUM* G(N+12)
      YIN(NN) = DUM*(G(N+ 9)-G(N+ 2))
      ZIN(NN) = DUM*(G(N+11)-G(N+ 1))
      IF(MAXI.EQ.10) GO TO 900
 890  CONTINUE
      NN=NN+1
      XIN(NN)=(G(N+ 6)-G(N   )-G(N   )-G(N   ))
      YIN(NN)= G(N+ 9)
      ZIN(NN)= G(N+10)
      NN=NN+1
      XIN(NN)= G(N+11)
      YIN(NN)=(G(N+ 7)-G(N+ 1)-G(N+ 1)-G(N+ 1))
      ZIN(NN)= G(N+12)
      NN=NN+1
      XIN(NN)= G(N+13)
      YIN(NN)= G(N+14)
      ZIN(NN)=(G(N+ 8)-G(N+ 2)-G(N+ 2)-G(N+ 2))
      NN=NN+1
      DUM=ONE
      IF(NORM) DUM=SQRT5
      XIN(NN)=DUM* (G(N+ 9)-G(N+ 3)-G(N+ 3))
      YIN(NN)=DUM* (G(N+15)-G(N  ))
      ZIN(NN)=DUM* G(N+18)
      NN=NN+1
      XIN(NN)=DUM* (G(N+10)-G(N+ 4)-G(N+ 4))
      YIN(NN)=DUM* G(N+18)
      ZIN(NN)=DUM* (G(N+16)-G(N  ))
      NN=NN+1
      XIN(NN)=DUM* (G(N+15)-G(N+ 1))
      YIN(NN)=DUM* (G(N+11)-G(N+ 3)-G(N+ 3))
      ZIN(NN)=DUM*  G(N+19)
      NN=NN+1
      XIN(NN)=DUM* G(N+19)
      YIN(NN)=DUM* (G(N+12)-G(N+ 5)-G(N+ 5))
      ZIN(NN)=DUM* (G(N+17)-G(N+ 1))
      NN=NN+1
      XIN(NN)=DUM* (G(N+16)-G(N+ 2))
      YIN(NN)=DUM*  G(N+20)
      ZIN(NN)=DUM* (G(N+13)-G(N+ 4)-G(N+ 4))
      NN=NN+1
      XIN(NN)=DUM*  G(N+20)
      YIN(NN)=DUM* (G(N+17)-G(N+ 2))
      ZIN(NN)=DUM* (G(N+14)-G(N+ 5)-G(N+ 5))
      NN=NN+1
      IF(NORM) DUM=DUM*SQRT3
      XIN(NN)=DUM* (G(N+18)-G(N+ 5))
      YIN(NN)=DUM* (G(N+19)-G(N+ 4))
      ZIN(NN)=DUM* (G(N+20)-G(N+ 3))
  900 N = N+21
C
C     ----- CALCULATE CONTRIBUTION TO GRADIENT -----
C
      N = 0
      DO 980 J = MINJ,MAXJ
      JN = LOCJ+J
      DO 980 I = MINI,MAXI
      N = N+1
      IN = LOCI+I
      IF (IN-JN) 920,940,940
  920 ID = JN
      JD = IN
      GO TO 960
  940 ID = IN
      JD = JN
  960 NN = (ID*(ID-1))/2+JD
      DUM = DM(NN)
      DUM = DUM+DUM
      DE(1,IAT) = DE(1,IAT)+DUM*XIN(N)
      DE(2,IAT) = DE(2,IAT)+DUM*YIN(N)
      DE(3,IAT) = DE(3,IAT)+DUM*ZIN(N)
      IF (DBG .AND. MASWRK) WRITE (IW,9108) II,JJ,I,J,NN,DUM,XIN(N),
     *              YIN(N),ZIN(N),IAT,DE(1,IAT),DE(2,IAT),DE(3,IAT)
  980 CONTINUE
      IF (DBG) CALL EGOUT(DE,NAT)
 1040 CONTINUE
 1060 CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
      IF (OUT) THEN
         IF (MASWRK) WRITE (IW,9008)
         CALL EGOUT(DE,NAT)
      END IF
 1120 CONTINUE
      IF (MASWRK.AND.OUT) THEN
         WRITE (IW,9088)
         CALL TIMIT(1)
      ENDIF
      IF(GOPARR .AND. NXT) CALL DDI_DLBRESET
      RETURN
C
 9008 FORMAT(/10X,45("-")/10X,'EFD CONTRIBUTIONS',
     *        ' TO THE GRADIENT'/10X,45(1H-))
 9088 FORMAT(/1X,'..... END OF EFD GRADIENT ......')
 9108 FORMAT(1X,'II,JJ,I,J,NN=',4I5,I6,4E20.12/
     *       1X,'IAT,DE(1,2,3)',18X,I5,3E20.12)
      END
C*MODULE EFGRDB  *DECK EFODER
      SUBROUTINE EFODER(DM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL ISKIP
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
      LOGICAL OUT,DBG,NORM,GOPARR,DSKWRK,MASWRK,NXT
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION DM(*)
C
      DIMENSION OCT(10,MXFGPT),W4(9),W6(9)
      DIMENSION ISKIP(35),INDX(70)
      DIMENSION IX(35),IY(35),IZ(35),JX(20),JY(20),JZ(20)
      DIMENSION IJG(210),IJX(210),IJY(210),IJZ(210)
      DIMENSION XIN(480),YIN(480),ZIN(480),G(210),DIJ(210)
      DIMENSION FIJ(210),GIJ(210)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /DSTV  / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,
     *                XJ,YJ,ZJ,NI,NJ,CX,CY,CZ
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /GRAD  / DE(3,MXATM)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
C
      PARAMETER (ZERO=0.0D+00,ONE=1.0D+00,FOUR=4.0D+00)
      PARAMETER (THREE=3.0D+00, FIVE=5.0D+00)
      PARAMETER (SIX=6.0D+00,EIGHT=8.0D+00)
      PARAMETER (RLN10=2.30258D+00)
      PARAMETER (PT5=0.5D+00)
      PARAMETER (PI212=1.1283791670955D+00)
      PARAMETER (SQRT3=1.73205080756888D+00)
      PARAMETER (SQRT5=2.23606797749979D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /, DEBUG/8HDEBUG   /, DBUGME/8HTVDER   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA CHECK_STR/"CHECK   "/, DEBUG_STR/"DEBUG   "/,
     *  DBUGME_STR/"TVDER   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA GRD1/8HGRD1    /
#else
      CHARACTER*8 :: GRD1_STR
      EQUIVALENCE (GRD1, GRD1_STR)
      DATA GRD1_STR/"GRD1    "/
#endif
      DATA INDX / 1, 2, 3, 4, 5, 6, 7, 8, 9,10,
     1           11,12,13,14,15,16,17,18,19,20,
     2           -0,-0,-0,-0,-0,-0,-0,-0,-0,-0,
     3           -0,-0,-0,-0,-0,
     4           -0, 1, 2, 3, 1, 2, 3, 4, 5, 6,
     5            4, 5, 6, 7, 8, 9,10,11,12,13,
     6            7, 8, 9,10,11,12,13,14,15,16,
     7           17,18,19,20,21/
      DATA JX / 0, 1, 0, 0, 2, 0, 0, 1, 1, 0,
     1          3, 0, 0, 2, 2, 1, 0, 1, 0, 1/
      DATA IX / 1, 5, 1, 1, 9, 1, 1, 5, 5, 1,
     1         13, 1, 1, 9, 9, 5, 1, 5, 1, 5,
     2         17, 1, 1,13,13, 5, 1, 5, 1, 9,
     3          9, 1, 9, 5, 5/
      DATA JY / 0, 0, 1, 0, 0, 2, 0, 1, 0, 1,
     1          0, 3, 0, 1, 0, 2, 2, 0, 1, 1/
      DATA IY / 1, 1, 5, 1, 1, 9, 1, 5, 1, 5,
     1          1,13, 1, 5, 1, 9, 9, 1, 5, 5,
     2          1,17, 1, 5, 1,13,13, 1, 5, 9,
     3          1, 9, 5, 9, 5/
      DATA JZ / 0, 0, 0, 1, 0, 0, 2, 0, 1, 1,
     1          0, 0, 3, 0, 1, 0, 1, 2, 2, 1/
      DATA IZ / 1, 1, 1, 5, 1, 1, 9, 1, 5, 5,
     1          1, 1,13, 1, 5, 1, 5, 9, 9, 5,
     2          1, 1,17, 1, 5, 1, 5,13,13, 1,
     3          9, 9, 5, 5, 9/
      DATA QLIM/1.D-08/
C
C
C     INITIALIZATION FOR PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      LCNT = -1
      DBG = .FALSE.
      OUT = EXETYP.EQ.GRD1
      IF (EXETYP.EQ.DEBUG  .OR. EXETYP.EQ.DBUGME  .OR.
     *    NPRINT.EQ.-3) THEN
         OUT = .TRUE.
         DBG = .TRUE.
      END IF
      IF (DBG .AND. MASWRK) WRITE (IW,9008)
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
      IF (EXETYP .EQ. CHECK) GO TO 1120
C
C     GRADIENT CONTRIBUTION FROM EFFECTIVE POINT OCTUPOLES.
C
C  REPLACE THIRD MOMENTS IN ARRAY TM WITH ELECTRIC OCTUPOLE TENSORS
C
      DO 80 I=1,NMTTPT
         IF(.NOT.DOOCTU(I)) GO TO 80
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
 80   CONTINUE
C
C     ----- I SHELL
C
      DO 1060 II = 1,NSHELL
C
C           GO PARALLEL!
C
      IF(NXT .AND. GOPARR) THEN
         LCNT = LCNT + 1
         IF(LCNT.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
         IF(NEXT.NE.LCNT) GO TO 1060
      END IF
      IAT = KATOM(II)
      XI = C(1,IAT)
      YI = C(2,IAT)
      ZI = C(3,IAT)
      I1 = KSTART(II)
      I2 = I1+KNG(II)-1
      LIT = KTYPE(II)+1
      MINI = KMIN(II)
      MAXI = KMAX(II)
      LOCI = KLOC(II)-MINI
      DO 100 I=1,35
 100  ISKIP(I)=.TRUE.
      DO 260 I=MINI,MAXI
      GO TO (110,140,260,260,180,260,260,260,260,260,
     1       220,260,260,260,260,260,260,260,260,260),I
 110  DO 120 K=2,4
 120  ISKIP(K)=.FALSE.
      GO TO 260
 140  DO 160 K=5,10
 160  ISKIP(K)=.FALSE.
      ISKIP(1)=.FALSE.
      GO TO 260
 180  DO 200 K=2,4
 200  ISKIP(K)=.FALSE.
      DO 210 K=11,20
 210  ISKIP(K)=.FALSE.
      GO TO 260
 220  DO 240 K=5,10
 240  ISKIP(K)=.FALSE.
      DO 250 K=21,35
 250  ISKIP(K)=.FALSE.
 260  CONTINUE
C
C     ----- J SHELL
C
      DO 1040 JJ = 1,NSHELL
C
C           GO PARALLEL!
C
        IF((.NOT.NXT) .AND. GOPARR) THEN
           IPCOUNT = IPCOUNT + 1
           IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 1040
        END IF
C
      JAT = KATOM(JJ)
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
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      N0 = 0
      IF (LIT .GE. 4) N0 = 35
      IJ = 0
      DO 340 I = 1,35
      IF (ISKIP(I)) GO TO 340
      IN = INDX(I+N0)
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
      DO 320 J = MINJ,MAXJ
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
      IJG(IJ) = IN+21*(J-MINJ)
  320 CONTINUE
  340 CONTINUE
      DO 360 I = 1,IJ
      N = IJG(I)
  360 G(N) = ZERO
C
C     ----- I PRIMITIVE
C
      DO 840 IG = I1,I2
      AI = EX(IG)
      ARRI = AI*RR
      AXI = AI*XI
      AYI = AI*YI
      AZI = AI*ZI
      DUM = AI+AI
      CSI = CP(IG)
      CPI = CS(IG)*DUM
      IF (LIT .EQ. 4) CPI = CD(IG)
      CDI = CP(IG)*DUM
      IF (LIT .EQ. 5) CDI = CF(IG)
      CFI = CD(IG)*DUM
      CGI = CF(IG)*DUM
C
C     ----- J PRIMITIVE
C
      DO 820 JG = J1,J2
      AJ = EX(JG)
      AA = AI+AJ
      AA1 = ONE/AA
      DUM = AJ*ARRI*AA1
      IF (DUM .GT. TOL) GO TO 820
      FAC = EXP(-DUM)
      CSJ = CS(JG)
      CPJ = CP(JG)
      CDJ = CD(JG)
      CFJ = CF(JG)
      AX = (AXI+AJ*XJ)*AA1
      AY = (AYI+AJ*YJ)*AA1
      AZ = (AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR
C
      NN = 0
      DUM1 = ZERO
      DUM2 = DUM1
      DO 600 I=1,35
      IF(ISKIP(I)) GO TO 600
      GO TO (370,380,420,420,390,420,420,420,420,420,
     1       400,420,420,420,420,420,420,420,420,420,
     2       410,420,420,420,420,420,420,420,420,420,
     3       420,420,420,420,420),I
 370  DUM1=CSI*FAC
      GO TO 420
 380  DUM1=CPI*FAC
      GO TO 420
 390  DUM1=CDI*FAC
      GO TO 420
 400  DUM1=CFI*FAC
      GO TO 420
 410  DUM1 = CGI*FAC
 420  CONTINUE
      DO 580 J=MINJ,MAXJ
      GO TO (430,440,560,560,460,560,560,480,560,560,
     1       500,560,560,520,560,560,560,560,560,540),J
  430 DUM2=DUM1*CSJ
      GO TO 560
  440 DUM2=DUM1*CPJ
      GO TO 560
  460 DUM2=DUM1*CDJ
      GO TO 560
  480 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 560
  500 DUM2 = DUM1 * CFJ
      GO TO 560
  520 DUM2 = DUM2 *SQRT5
      GO TO 560
  540 DUM2 = DUM2 * SQRT3
  560 NN=NN+1
  580 DIJ(NN)=DUM2
  600 CONTINUE
C
C     ..... BASIC DIPOLE TERM
C
      DUM = PI212*AA1/(THREE*FIVE)
      DO 700 I = 1,IJ
  700 FIJ(I) = DIJ(I)*DUM
      AAX = AA*AX
      AAY = AA*AY
      AAZ = AA*AZ
      DO 819 IC = 1,NMTTPT
      IF(.NOT.DOOCTU(IC)) GO TO 819
      ALFA = EFATRM(IC)
      BETA = EFBTRM(IC)
      CX = EFC(1,IC)
      CY = EFC(2,IC)
      CZ = EFC(3,IC)
      DUMA = PI212/(THREE*FIVE*(AA+ALFA))
      DO 710 I=1,IJ
      GIJ(I) = DIJ(I)*DUMA
 710  CONTINUE
      XX = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
C     CALL RT123
      IF(NROOTS.LE.3)CALL RT123
      IF(NROOTS.EQ.4)CALL ROOT4
      IF(NROOTS.EQ.5)CALL ROOT5
      IF(NROOTS.EQ.6)CALL ROOT6
      MM = 0
      DO 740 K = 1,NROOTS
      U2 = AA*U(K)
      U4 = U2*U2
      U6 = U4*U2
      WW = W(K)
      W4(K) = -FOUR*WW*U4
      W6(K) = -EIGHT*WW*U6
      TT = ONE/(AA+U2)
      T = SQRT(TT)
      X0 = (AAX+U2*CX)*TT
      Y0 = (AAY+U2*CY)*TT
      Z0 = (AAZ+U2*CZ)*TT
      IN = -4+MM
      DO 720 I = 1,LIT
      IN = IN+4
      NI = I
      DO 720 J = 1,LJT
      JN = IN+J
      NJ = J
      CALL VINT
      XIN(JN) = XINT
      YIN(JN) = YINT
      ZIN(JN) = ZINT
      CALL PDXYZ
      XIN(JN+120) = XINT
      YIN(JN+120) = YINT
      ZIN(JN+120) = ZINT
      CALL QDXYZ
      XIN(JN+240) = XINT
      YIN(JN+240) = YINT
      ZIN(JN+240) = ZINT
      CALL ODXYZ
      XIN(JN+360) = XINT
      YIN(JN+360) = YINT
      ZIN(JN+360) = ZINT
  720 CONTINUE
  740 MM = MM+20
      DO 780 I = 1,IJ
      N = IJG(I)
      NX = IJX(I)
      NY = IJY(I)
      NZ = IJZ(I)
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
      MM = 0
      DO 760 K = 1,NROOTS
      DXXX=DXXX+XIN(NX+MM+120)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)*THREE
     1         -XIN(NX+MM+360)*YIN(NY+MM    )*ZIN(NZ+MM    )*W6(K)
      DYYY=DYYY+XIN(NX+MM    )*YIN(NY+MM+120)*ZIN(NZ+MM    )*W4(K)*THREE
     1         -XIN(NX+MM    )*YIN(NY+MM+360)*ZIN(NZ+MM    )*W6(K)
      DZZZ=DZZZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+120)*W4(K)*THREE
     1         -XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+360)*W6(K)
      DXXY=DXXY+XIN(NX+MM    )*YIN(NY+MM+120)*ZIN(NZ+MM    )*W4(K)
     1         -XIN(NX+MM+240)*YIN(NY+MM+120)*ZIN(NZ+MM    )*W6(K)
      DXXZ=DXXZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+120)*W4(K)
     1         -XIN(NX+MM+240)*YIN(NY+MM    )*ZIN(NZ+MM+120)*W6(K)
      DXYY=DXYY+XIN(NX+MM+120)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)
     1         -XIN(NX+MM+120)*YIN(NY+MM+240)*ZIN(NZ+MM    )*W6(K)
      DYYZ=DYYZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+120)*W4(K)
     1         -XIN(NX+MM    )*YIN(NY+MM+240)*ZIN(NZ+MM+120)*W6(K)
      DXZZ=DXZZ+XIN(NX+MM+120)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)
     1         -XIN(NX+MM+120)*YIN(NY+MM    )*ZIN(NZ+MM+240)*W6(K)
      DYZZ=DYZZ+XIN(NX+MM    )*YIN(NY+MM+120)*ZIN(NZ+MM    )*W4(K)
     1         -XIN(NX+MM    )*YIN(NY+MM+120)*ZIN(NZ+MM+240)*W6(K)
      DXYZ=DXYZ-XIN(NX+MM+120)*YIN(NY+MM+120)*ZIN(NZ+MM+120)*W6(K)
  760 MM = MM+20
      DUM = FIJ(I)
      G(N) = G(N)-
     * DUM * ( DXXX * OCT(1,IC)
     *       + DYYY * OCT(2,IC)
     *       + DZZZ * OCT(3,IC)
     *+THREE*( DXXY * OCT(4,IC)
     *       + DXXZ * OCT(5,IC)
     *       + DXYY * OCT(6,IC)
     *       + DYYZ * OCT(7,IC)
     *       + DXZZ * OCT(8,IC)
     *       + DYZZ * OCT(9,IC) )
     *  +SIX*( DXYZ * OCT(10,IC) ) )
 780  CONTINUE
C     SUBTRACT DAMPING FUNCTION TERM.
      IF (ABS(ALFA).LE.QLIM ) GO TO 818
      PCSQ = (AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2
      XX=AA*AA*PCSQ/(AA+ALFA)
      PREI = EXP(-AA*ALFA*PCSQ/(AA+ALFA))
C     CALL RT123
      IF(NROOTS.LE.3)CALL RT123
      IF(NROOTS.EQ.4)CALL ROOT4
      IF(NROOTS.EQ.5)CALL ROOT5
      IF(NROOTS.EQ.6)CALL ROOT6
      MM=0
      DO 814 K=1,NROOTS
      U2=(ALFA+AA)*U(K)
      U4 = U2*U2
      U6 = U4*U2
      WW = W(K)
      W4(K) = -FOUR*WW*U4
      W6(K) = -EIGHT*WW*U6
      TT=ONE/(AA+U2+ALFA)
      T= SQRT(TT)
      X0=(AAX+(U2+ALFA)*CX)*TT
      Y0=(AAY+(U2+ALFA)*CY)*TT
      Z0=(AAZ+(U2+ALFA)*CZ)*TT
      IN=-4+MM
      DO 812 I=1,LIT
      IN=IN+4
      NI=I
      DO 812 J=1,LJT
      JN=IN+J
      NJ=J
      CALL VINT
      XIN(JN)=XINT
      YIN(JN)=YINT
      ZIN(JN)=ZINT
      CALL PDXYZ
      XIN(JN+120)=XINT
      YIN(JN+120)=YINT
      ZIN(JN+120)=ZINT
      CALL QDXYZ
      XIN(JN+240)=XINT
      YIN(JN+240)=YINT
      ZIN(JN+240)=ZINT
      CALL ODXYZ
      XIN(JN+360)=XINT
      YIN(JN+360)=YINT
      ZIN(JN+360)=ZINT
 812  CONTINUE
 814  MM=MM+20
      DO 817 I=1,IJ
      N=IJG(I)
      NX=IJX(I)
      NY=IJY(I)
      NZ=IJZ(I)
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
      MM=0
      DO 816 K=1,NROOTS
      DXXX=DXXX+XIN(NX+MM+120)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)*THREE
     1         -XIN(NX+MM+360)*YIN(NY+MM    )*ZIN(NZ+MM    )*W6(K)
      DYYY=DYYY+XIN(NX+MM    )*YIN(NY+MM+120)*ZIN(NZ+MM    )*W4(K)*THREE
     1         -XIN(NX+MM    )*YIN(NY+MM+360)*ZIN(NZ+MM    )*W6(K)
      DZZZ=DZZZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+120)*W4(K)*THREE
     1         -XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+360)*W6(K)
      DXXY=DXXY+XIN(NX+MM    )*YIN(NY+MM+120)*ZIN(NZ+MM    )*W4(K)
     1         -XIN(NX+MM+240)*YIN(NY+MM+120)*ZIN(NZ+MM    )*W6(K)
      DXXZ=DXXZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+120)*W4(K)
     1         -XIN(NX+MM+240)*YIN(NY+MM    )*ZIN(NZ+MM+120)*W6(K)
      DXYY=DXYY+XIN(NX+MM+120)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)
     1         -XIN(NX+MM+120)*YIN(NY+MM+240)*ZIN(NZ+MM    )*W6(K)
      DYYZ=DYYZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+120)*W4(K)
     1         -XIN(NX+MM    )*YIN(NY+MM+240)*ZIN(NZ+MM+120)*W6(K)
      DXZZ=DXZZ+XIN(NX+MM+120)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)
     1         -XIN(NX+MM+120)*YIN(NY+MM    )*ZIN(NZ+MM+240)*W6(K)
      DYZZ=DYZZ+XIN(NX+MM    )*YIN(NY+MM+120)*ZIN(NZ+MM    )*W4(K)
     1         -XIN(NX+MM    )*YIN(NY+MM+120)*ZIN(NZ+MM+240)*W6(K)
      DXYZ=DXYZ-XIN(NX+MM+120)*YIN(NY+MM+120)*ZIN(NZ+MM+120)*W6(K)
 816  MM=MM+20
      DUM = -GIJ(I) * PREI * BETA
      G(N)=G(N) -
     * DUM * ( DXXX * OCT(1,IC)
     *       + DYYY * OCT(2,IC)
     *       + DZZZ * OCT(3,IC)
     *+THREE*( DXXY * OCT(4,IC)
     *       + DXXZ * OCT(5,IC)
     *       + DXYY * OCT(6,IC)
     *       + DYYZ * OCT(7,IC)
     *       + DXZZ * OCT(8,IC)
     *       + DYZZ * OCT(9,IC) )
     * +SIX* ( DXYZ * OCT(10,IC) ) )
 817  CONTINUE
 818  CONTINUE
 819  CONTINUE
C
C     ----- END OF PRIMITIVE LOOPS -----
C
  820 CONTINUE
  840 CONTINUE
C
C     ----- FORM INTEGRALS OVER DERIVATIVES -----
C
      NN = 0
      N = 1
      DO 900 J = MINJ,MAXJ
      IF (MINI .GT. 1) GO TO 860
      NN = NN+1
      XIN(NN) = G(N+ 1)
      YIN(NN) = G(N+ 2)
      ZIN(NN) = G(N+ 3)
      IF (MAXI .EQ. 1) GO TO 900
  860 IF (MINI .GT. 2) GO TO 880
      NN = NN+1
      XIN(NN) = (G(N+ 4)-G(N ))
      YIN(NN) = G(N+ 7)
      ZIN(NN) = G(N+ 8)
      NN = NN+1
      XIN(NN) = G(N+ 7)
      YIN(NN) = (G(N+ 5)-G(N ))
      ZIN(NN) = G(N+ 9)
      NN = NN+1
      XIN(NN) = G(N+ 8)
      YIN(NN) = G(N+ 9)
      ZIN(NN) = (G(N+ 6)-G(N ))
      IF (MAXI .EQ. 4) GO TO 900
  880 CONTINUE
      IF (MINI .GT. 5) GO TO 890
      NN = NN+1
      XIN(NN) = (G(N+ 3)-G(N )-G(N ))
      YIN(NN) = G(N+ 6)
      ZIN(NN) = G(N+ 7)
      NN = NN+1
      XIN(NN) = G(N+ 8)
      YIN(NN) = (G(N+ 4)-G(N+ 1)-G(N+ 1))
      ZIN(NN) = G(N+ 9)
      NN = NN+1
      XIN(NN) = G(N+10)
      YIN(NN) = G(N+11)
      ZIN(NN) = (G(N+ 5)-G(N+ 2)-G(N+ 2))
      NN = NN+1
      DUM = ONE
      IF (NORM) DUM = SQRT3
      XIN(NN) = DUM*(G(N+ 6)-G(N+ 1))
      YIN(NN) = DUM*(G(N+ 8)-G(N ))
      ZIN(NN) = DUM* G(N+12)
      NN = NN+1
      XIN(NN) = DUM*(G(N+ 7)-G(N+ 2))
      YIN(NN) = DUM* G(N+12)
      ZIN(NN) = DUM*(G(N+10)-G(N ))
      NN = NN+1
      XIN(NN) = DUM* G(N+12)
      YIN(NN) = DUM*(G(N+ 9)-G(N+ 2))
      ZIN(NN) = DUM*(G(N+11)-G(N+ 1))
      IF(MAXI.EQ.10) GO TO 900
 890  CONTINUE
      NN=NN+1
      XIN(NN)=(G(N+ 6)-G(N   )-G(N   )-G(N   ))
      YIN(NN)= G(N+ 9)
      ZIN(NN)= G(N+10)
      NN=NN+1
      XIN(NN)= G(N+11)
      YIN(NN)=(G(N+ 7)-G(N+ 1)-G(N+ 1)-G(N+ 1))
      ZIN(NN)= G(N+12)
      NN=NN+1
      XIN(NN)= G(N+13)
      YIN(NN)= G(N+14)
      ZIN(NN)=(G(N+ 8)-G(N+ 2)-G(N+ 2)-G(N+ 2))
      NN=NN+1
      DUM=ONE
      IF(NORM) DUM=SQRT5
      XIN(NN)=DUM* (G(N+ 9)-G(N+ 3)-G(N+ 3))
      YIN(NN)=DUM* (G(N+15)-G(N  ))
      ZIN(NN)=DUM* G(N+18)
      NN=NN+1
      XIN(NN)=DUM* (G(N+10)-G(N+ 4)-G(N+ 4))
      YIN(NN)=DUM* G(N+18)
      ZIN(NN)=DUM* (G(N+16)-G(N  ))
      NN=NN+1
      XIN(NN)=DUM* (G(N+15)-G(N+ 1))
      YIN(NN)=DUM* (G(N+11)-G(N+ 3)-G(N+ 3))
      ZIN(NN)=DUM*  G(N+19)
      NN=NN+1
      XIN(NN)=DUM* G(N+19)
      YIN(NN)=DUM* (G(N+12)-G(N+ 5)-G(N+ 5))
      ZIN(NN)=DUM* (G(N+17)-G(N+ 1))
      NN=NN+1
      XIN(NN)=DUM* (G(N+16)-G(N+ 2))
      YIN(NN)=DUM*  G(N+20)
      ZIN(NN)=DUM* (G(N+13)-G(N+ 4)-G(N+ 4))
      NN=NN+1
      XIN(NN)=DUM*  G(N+20)
      YIN(NN)=DUM* (G(N+17)-G(N+ 2))
      ZIN(NN)=DUM* (G(N+14)-G(N+ 5)-G(N+ 5))
      NN=NN+1
      IF(NORM) DUM=DUM*SQRT3
      XIN(NN)=DUM* (G(N+18)-G(N+ 5))
      YIN(NN)=DUM* (G(N+19)-G(N+ 4))
      ZIN(NN)=DUM* (G(N+20)-G(N+ 3))
  900 N = N+21
C
C     ----- CALCULATE CONTRIBUTION TO GRADIENT -----
C
      N = 0
      DO 980 J = MINJ,MAXJ
      JN = LOCJ+J
      DO 980 I = MINI,MAXI
      N = N+1
      IN = LOCI+I
      IF (IN-JN) 920,940,940
  920 ID = JN
      JD = IN
      GO TO 960
  940 ID = IN
      JD = JN
  960 NN = (ID*(ID-1))/2+JD
      DUM = DM(NN)
      DUM = DUM+DUM
      DE(1,IAT) = DE(1,IAT)+DUM*XIN(N)
      DE(2,IAT) = DE(2,IAT)+DUM*YIN(N)
      DE(3,IAT) = DE(3,IAT)+DUM*ZIN(N)
      IF (DBG .AND. MASWRK) WRITE (IW,9108) II,JJ,I,J,NN,DUM,XIN(N),
     *              YIN(N),ZIN(N),IAT,DE(1,IAT),DE(2,IAT),DE(3,IAT)
  980 CONTINUE
      IF (DBG) CALL EGOUT(DE,NAT)
 1040 CONTINUE
 1060 CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
      IF (OUT) THEN
         IF (MASWRK) WRITE (IW,9008)
         CALL EGOUT(DE,NAT)
      END IF
 1120 CONTINUE
      IF (MASWRK.AND.OUT) THEN
         WRITE (IW,9088)
         CALL TIMIT(1)
      ENDIF
      IF(GOPARR .AND. NXT) CALL DDI_DLBRESET
      RETURN
C
 9008 FORMAT(/10X,45("-")/10X,'EFO CONTRIBUTIONS',
     *        ' TO THE GRADIENT'/10X,45(1H-))
 9088 FORMAT(/1X,'..... END OF EFO GRADIENT ......')
 9108 FORMAT(1X,'II,JJ,I,J,NN=',4I5,I6,4E20.12/
     *       1X,'IAT,DE(1,2,3)',18X,I5,3E20.12)
      END
C*MODULE EFGRDB  *DECK EFQDER
      SUBROUTINE EFQDER(DM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL OUT,DBG,NORM,GOPARR,DSKWRK,MASWRK,NXT
      LOGICAL ISKIP
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION DM(*)
C
      DIMENSION QUAD(6,MXFGPT),W2(9),W4(9)
      DIMENSION ISKIP(35),INDX(70)
      DIMENSION IX(35),IY(35),IZ(35),JX(20),JY(20),JZ(20)
      DIMENSION IJG(210),IJX(210),IJY(210),IJZ(210)
      DIMENSION XIN(300),YIN(300),ZIN(300),G(210),DIJ(210)
      DIMENSION FIJ(210),GIJ(210)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /DSTV  / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,
     *                XJ,YJ,ZJ,NI,NJ,CX,CY,CZ
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /GRAD  / DE(3,MXATM)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
C
      PARAMETER (ZERO=0.0D+00,ONE=1.0D+00,TWO=2.0D+00,FOUR=4.0D+00)
      PARAMETER (THREE=3.0D+00)
      PARAMETER (RLN10=2.30258D+00)
      PARAMETER (PT5=0.5D+00,ONEPT5=1.5D+00)
      PARAMETER (PI212=1.1283791670955D+00)
      PARAMETER (SQRT3=1.73205080756888D+00)
      PARAMETER (SQRT5=2.23606797749979D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /, DEBUG/8HDEBUG   /, DBUGME/8HTVDER   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA CHECK_STR/"CHECK   "/, DEBUG_STR/"DEBUG   "/,
     *  DBUGME_STR/"TVDER   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA GRD1/8HGRD1    /
#else
      CHARACTER*8 :: GRD1_STR
      EQUIVALENCE (GRD1, GRD1_STR)
      DATA GRD1_STR/"GRD1    "/
#endif
      DATA INDX / 1, 2, 3, 4, 5, 6, 7, 8, 9,10,
     1           11,12,13,14,15,16,17,18,19,20,
     2           -0,-0,-0,-0,-0,-0,-0,-0,-0,-0,
     3           -0,-0,-0,-0,-0,
     4           -0, 1, 2, 3, 1, 2, 3, 4, 5, 6,
     5            4, 5, 6, 7, 8, 9,10,11,12,13,
     6            7, 8, 9,10,11,12,13,14,15,16,
     7           17,18,19,20,21/
      DATA JX / 0, 1, 0, 0, 2, 0, 0, 1, 1, 0,
     1          3, 0, 0, 2, 2, 1, 0, 1, 0, 1/
      DATA IX / 1, 5, 1, 1, 9, 1, 1, 5, 5, 1,
     1         13, 1, 1, 9, 9, 5, 1, 5, 1, 5,
     2         17, 1, 1,13,13, 5, 1, 5, 1, 9,
     3          9, 1, 9, 5, 5/
      DATA JY / 0, 0, 1, 0, 0, 2, 0, 1, 0, 1,
     1          0, 3, 0, 1, 0, 2, 2, 0, 1, 1/
      DATA IY / 1, 1, 5, 1, 1, 9, 1, 5, 1, 5,
     1          1,13, 1, 5, 1, 9, 9, 1, 5, 5,
     2          1,17, 1, 5, 1,13,13, 1, 5, 9,
     3          1, 9, 5, 9, 5/
      DATA JZ / 0, 0, 0, 1, 0, 0, 2, 0, 1, 1,
     1          0, 0, 3, 0, 1, 0, 1, 2, 2, 1/
      DATA IZ / 1, 1, 1, 5, 1, 1, 9, 1, 5, 5,
     1          1, 1,13, 1, 5, 1, 5, 9, 9, 5,
     2          1, 1,17, 1, 5, 1, 5,13,13, 1,
     3          9, 9, 5, 5, 9/
      DATA QLIM/1.D-08/
C
C
C     INITIALIZATION FOR PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      LCNT = -1
      DBG = .FALSE.
      OUT = EXETYP.EQ.GRD1
      IF (EXETYP.EQ.DEBUG  .OR. EXETYP.EQ.DBUGME  .OR.
     *    NPRINT.EQ.-3) THEN
         OUT = .TRUE.
         DBG = .TRUE.
      END IF
      IF (DBG .AND. MASWRK) WRITE (IW,9008)
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
      IF (EXETYP .EQ. CHECK) GO TO 1120
C
C     GRADIENT CONTRIBUTION FROM EFFECTIVE POINT QUADRUPOLES.
C
C  REPLACE SECOND MOMENTS IN ARRAY SM WITH ELECTRIC QUADRUPOLE TENSORS
C
      DO 80 I=1,NMTTPT
      IF(.NOT.DOQUAD(I)) GO TO 80
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
 80   CONTINUE
C
C     ----- I SHELL
C
      DO 1060 II = 1,NSHELL
C
C           GO PARALLEL!
C
      IF(NXT .AND. GOPARR) THEN
         LCNT = LCNT + 1
         IF(LCNT.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
         IF(NEXT.NE.LCNT) GO TO 1060
      END IF
      IAT = KATOM(II)
      XI = C(1,IAT)
      YI = C(2,IAT)
      ZI = C(3,IAT)
      I1 = KSTART(II)
      I2 = I1+KNG(II)-1
      LIT = KTYPE(II)+1
      MINI = KMIN(II)
      MAXI = KMAX(II)
      LOCI = KLOC(II)-MINI
      DO 100 I=1,35
 100  ISKIP(I)=.TRUE.
      DO 260 I=MINI,MAXI
      GO TO (110,140,260,260,180,260,260,260,260,260,
     1       220,260,260,260,260,260,260,260,260,260),I
 110  DO 120 K=2,4
 120  ISKIP(K)=.FALSE.
      GO TO 260
 140  DO 160 K=5,10
 160  ISKIP(K)=.FALSE.
      ISKIP(1)=.FALSE.
      GO TO 260
 180  DO 200 K=2,4
 200  ISKIP(K)=.FALSE.
      DO 210 K=11,20
 210  ISKIP(K)=.FALSE.
      GO TO 260
 220  DO 240 K=5,10
 240  ISKIP(K)=.FALSE.
      DO 250 K=21,35
 250  ISKIP(K)=.FALSE.
 260  CONTINUE
C
C     ----- J SHELL
C
      DO 1040 JJ = 1,NSHELL
C
C           GO PARALLEL!
C
        IF((.NOT.NXT) .AND. GOPARR) THEN
           IPCOUNT = IPCOUNT + 1
           IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 1040
        END IF
C
      JAT = KATOM(JJ)
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
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      N0 = 0
      IF (LIT .GE. 4) N0 = 35
      IJ = 0
      DO 340 I = 1,35
      IF (ISKIP(I)) GO TO 340
      IN = INDX(I+N0)
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
      DO 320 J = MINJ,MAXJ
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
      IJG(IJ) = IN+21*(J-MINJ)
  320 CONTINUE
  340 CONTINUE
      DO 360 I = 1,IJ
      N = IJG(I)
  360 G(N) = ZERO
C
C     ----- I PRIMITIVE
C
      DO 840 IG = I1,I2
      AI = EX(IG)
      ARRI = AI*RR
      AXI = AI*XI
      AYI = AI*YI
      AZI = AI*ZI
      DUM = AI+AI
      CSI = CP(IG)
      CPI = CS(IG)*DUM
      IF (LIT .EQ. 4) CPI = CD(IG)
      CDI = CP(IG)*DUM
      IF (LIT .EQ. 5) CDI = CF(IG)
      CFI = CD(IG)*DUM
      CGI = CF(IG)*DUM
C
C     ----- J PRIMITIVE
C
      DO 820 JG = J1,J2
      AJ = EX(JG)
      AA = AI+AJ
      AA1 = ONE/AA
      DUM = AJ*ARRI*AA1
      IF (DUM .GT. TOL) GO TO 820
      FAC = EXP(-DUM)
      CSJ = CS(JG)
      CPJ = CP(JG)
      CDJ = CD(JG)
      CFJ = CF(JG)
      AX = (AXI+AJ*XJ)*AA1
      AY = (AYI+AJ*YJ)*AA1
      AZ = (AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR
C
      NN = 0
      DUM1 = ZERO
      DUM2 = DUM1
      DO 600 I=1,35
      IF(ISKIP(I)) GO TO 600
      GO TO (370,380,420,420,390,420,420,420,420,420,
     1       400,420,420,420,420,420,420,420,420,420,
     2       410,420,420,420,420,420,420,420,420,420,
     3       420,420,420,420,420),I
 370  DUM1=CSI*FAC
      GO TO 420
 380  DUM1=CPI*FAC
      GO TO 420
 390  DUM1=CDI*FAC
      GO TO 420
 400  DUM1=CFI*FAC
      GO TO 420
 410  DUM1 = CGI*FAC
 420  CONTINUE
      DO 580 J=MINJ,MAXJ
      GO TO (430,440,560,560,460,560,560,480,560,560,
     1       500,560,560,520,560,560,560,560,560,540),J
  430 DUM2=DUM1*CSJ
      GO TO 560
  440 DUM2=DUM1*CPJ
      GO TO 560
  460 DUM2=DUM1*CDJ
      GO TO 560
  480 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 560
  500 DUM2 = DUM1 * CFJ
      GO TO 560
  520 DUM2 = DUM2 *SQRT5
      GO TO 560
  540 DUM2 = DUM2 * SQRT3
  560 NN=NN+1
  580 DIJ(NN)=DUM2
  600 CONTINUE
C
C     ..... BASIC DIPOLE TERM
C
      DUM = PI212*AA1/THREE
      DO 700 I = 1,IJ
  700 FIJ(I) = DIJ(I)*DUM
      AAX = AA*AX
      AAY = AA*AY
      AAZ = AA*AZ
      DO 819 IC = 1,NMTTPT
      IF(.NOT.DOQUAD(IC)) GO TO 819
      ALFA = EFATRM(IC)
      BETA = EFBTRM(IC)
      CX = EFC(1,IC)
      CY = EFC(2,IC)
      CZ = EFC(3,IC)
      DUMA = PI212/(THREE*(AA+ALFA))
      DO 710 I=1,IJ
      GIJ(I) = DIJ(I)*DUMA
 710  CONTINUE
      XX = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
C     CALL RT123
      IF(NROOTS.LE.3)CALL RT123
      IF(NROOTS.EQ.4)CALL ROOT4
      IF(NROOTS.EQ.5)CALL ROOT5
      MM = 0
      DO 740 K = 1,NROOTS
      U2 = AA*U(K)
      U4 = U2*U2
      WW = W(K)
      W2(K) = TWO *WW*U2
      W4(K) = FOUR*WW*U4
      TT = ONE/(AA+U2)
      T = SQRT(TT)
      X0 = (AAX+U2*CX)*TT
      Y0 = (AAY+U2*CY)*TT
      Z0 = (AAZ+U2*CZ)*TT
      IN = -4+MM
      DO 720 I = 1,LIT
      IN = IN+4
      NI = I
      DO 720 J = 1,LJT
      JN = IN+J
      NJ = J
      CALL VINT
      XIN(JN) = XINT
      YIN(JN) = YINT
      ZIN(JN) = ZINT
      CALL PDXYZ
      XIN(JN+100) = XINT
      YIN(JN+100) = YINT
      ZIN(JN+100) = ZINT
      CALL QDXYZ
      XIN(JN+200) = XINT
      YIN(JN+200) = YINT
      ZIN(JN+200) = ZINT
  720 CONTINUE
  740 MM = MM+20
      DO 780 I = 1,IJ
      N = IJG(I)
      NX = IJX(I)
      NY = IJY(I)
      NZ = IJZ(I)
      DUMXX = ZERO
      DUMYY = ZERO
      DUMZZ = ZERO
      DUMXY = ZERO
      DUMXZ = ZERO
      DUMYZ = ZERO
      MM = 0
      DO 760 K = 1,NROOTS
      DUMXX= DUMXX-XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            +XIN(NX+MM+200)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)
      DUMYY= DUMYY-XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            +XIN(NX+MM    )*YIN(NY+MM+200)*ZIN(NZ+MM    )*W4(K)
      DUMZZ= DUMZZ-XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            +XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+200)*W4(K)
      DUMXY= DUMXY+XIN(NX+MM+100)*YIN(NY+MM+100)*ZIN(NZ+MM    )*W4(K)
      DUMXZ= DUMXZ+XIN(NX+MM+100)*YIN(NY+MM    )*ZIN(NZ+MM+100)*W4(K)
      DUMYZ= DUMYZ+XIN(NX+MM    )*YIN(NY+MM+100)*ZIN(NZ+MM+100)*W4(K)
  760 MM = MM+20
      DUM = FIJ(I)
      G(N) = G(N)-
     1 DUM * ( DUMXX * QUAD(1,IC)
     2       + DUMYY * QUAD(2,IC)
     3       + DUMZZ * QUAD(3,IC)
     4 +TWO* ( DUMXY * QUAD(4,IC)
     5       + DUMXZ * QUAD(5,IC)
     6       + DUMYZ * QUAD(6,IC) ) )
 780  CONTINUE
C     SUBTRACT DAMPING FUNCTION TERM.
      IF (ABS(ALFA).LE.QLIM ) GO TO 818
      PCSQ = (AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2
      XX=AA*AA*PCSQ/(AA+ALFA)
      PREI = EXP(-AA*ALFA*PCSQ/(AA+ALFA))
C     CALL RT123
      IF(NROOTS.LE.3)CALL RT123
      IF(NROOTS.EQ.4)CALL ROOT4
      IF(NROOTS.EQ.5)CALL ROOT5
      MM=0
      DO 814 K=1,NROOTS
      U2=(ALFA+AA)*U(K)
      U4 = U2*U2
      WW = W(K)
      W2(K) = TWO *WW*U2
      W4(K) = FOUR*WW*U4
      TT=ONE/(AA+U2+ALFA)
      T= SQRT(TT)
      X0=(AAX+(U2+ALFA)*CX)*TT
      Y0=(AAY+(U2+ALFA)*CY)*TT
      Z0=(AAZ+(U2+ALFA)*CZ)*TT
      IN=-4+MM
      DO 812 I=1,LIT
      IN=IN+4
      NI=I
      DO 812 J=1,LJT
      JN=IN+J
      NJ=J
      CALL VINT
      XIN(JN)=XINT
      YIN(JN)=YINT
      ZIN(JN)=ZINT
      CALL PDXYZ
      XIN(JN+100)=XINT
      YIN(JN+100)=YINT
      ZIN(JN+100)=ZINT
      CALL QDXYZ
      XIN(JN+200)=XINT
      YIN(JN+200)=YINT
      ZIN(JN+200)=ZINT
 812  CONTINUE
 814  MM=MM+20
      DO 817 I=1,IJ
      N=IJG(I)
      NX=IJX(I)
      NY=IJY(I)
      NZ=IJZ(I)
      DUMXX = ZERO
      DUMYY = ZERO
      DUMZZ = ZERO
      DUMXY = ZERO
      DUMXZ = ZERO
      DUMYZ = ZERO
      MM=0
      DO 816 K=1,NROOTS
      DUMXX= DUMXX-XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            +XIN(NX+MM+200)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)
      DUMYY= DUMYY-XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            +XIN(NX+MM    )*YIN(NY+MM+200)*ZIN(NZ+MM    )*W4(K)
      DUMZZ= DUMZZ-XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            +XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+200)*W4(K)
      DUMXY= DUMXY+XIN(NX+MM+100)*YIN(NY+MM+100)*ZIN(NZ+MM    )*W4(K)
      DUMXZ= DUMXZ+XIN(NX+MM+100)*YIN(NY+MM    )*ZIN(NZ+MM+100)*W4(K)
      DUMYZ= DUMYZ+XIN(NX+MM    )*YIN(NY+MM+100)*ZIN(NZ+MM+100)*W4(K)
 816  MM=MM+20
      DUM = -GIJ(I) * PREI * BETA
      G(N)=G(N) -
     1 DUM * ( DUMXX * QUAD(1,IC)
     2       + DUMYY * QUAD(2,IC)
     3       + DUMZZ * QUAD(3,IC)
     4 +TWO* ( DUMXY * QUAD(4,IC)
     5       + DUMXZ * QUAD(5,IC)
     6       + DUMYZ * QUAD(6,IC) ) )
 817  CONTINUE
 818  CONTINUE
 819  CONTINUE
C
C     ----- END OF PRIMITIVE LOOPS -----
C
  820 CONTINUE
  840 CONTINUE
C
C     ----- FORM INTEGRALS OVER DERIVATIVES -----
C
      NN = 0
      N = 1
      DO 900 J = MINJ,MAXJ
      IF (MINI .GT. 1) GO TO 860
      NN = NN+1
      XIN(NN) = G(N+ 1)
      YIN(NN) = G(N+ 2)
      ZIN(NN) = G(N+ 3)
      IF (MAXI .EQ. 1) GO TO 900
  860 IF (MINI .GT. 2) GO TO 880
      NN = NN+1
      XIN(NN) = (G(N+ 4)-G(N ))
      YIN(NN) = G(N+ 7)
      ZIN(NN) = G(N+ 8)
      NN = NN+1
      XIN(NN) = G(N+ 7)
      YIN(NN) = (G(N+ 5)-G(N ))
      ZIN(NN) = G(N+ 9)
      NN = NN+1
      XIN(NN) = G(N+ 8)
      YIN(NN) = G(N+ 9)
      ZIN(NN) = (G(N+ 6)-G(N ))
      IF (MAXI .EQ. 4) GO TO 900
  880 CONTINUE
      IF (MINI .GT. 5) GO TO 890
      NN = NN+1
      XIN(NN) = (G(N+ 3)-G(N )-G(N ))
      YIN(NN) = G(N+ 6)
      ZIN(NN) = G(N+ 7)
      NN = NN+1
      XIN(NN) = G(N+ 8)
      YIN(NN) = (G(N+ 4)-G(N+ 1)-G(N+ 1))
      ZIN(NN) = G(N+ 9)
      NN = NN+1
      XIN(NN) = G(N+10)
      YIN(NN) = G(N+11)
      ZIN(NN) = (G(N+ 5)-G(N+ 2)-G(N+ 2))
      NN = NN+1
      DUM = ONE
      IF (NORM) DUM = SQRT3
      XIN(NN) = DUM*(G(N+ 6)-G(N+ 1))
      YIN(NN) = DUM*(G(N+ 8)-G(N ))
      ZIN(NN) = DUM* G(N+12)
      NN = NN+1
      XIN(NN) = DUM*(G(N+ 7)-G(N+ 2))
      YIN(NN) = DUM* G(N+12)
      ZIN(NN) = DUM*(G(N+10)-G(N ))
      NN = NN+1
      XIN(NN) = DUM* G(N+12)
      YIN(NN) = DUM*(G(N+ 9)-G(N+ 2))
      ZIN(NN) = DUM*(G(N+11)-G(N+ 1))
      IF(MAXI.EQ.10) GO TO 900
 890  CONTINUE
      NN=NN+1
      XIN(NN)=(G(N+ 6)-G(N   )-G(N   )-G(N   ))
      YIN(NN)= G(N+ 9)
      ZIN(NN)= G(N+10)
      NN=NN+1
      XIN(NN)= G(N+11)
      YIN(NN)=(G(N+ 7)-G(N+ 1)-G(N+ 1)-G(N+ 1))
      ZIN(NN)= G(N+12)
      NN=NN+1
      XIN(NN)= G(N+13)
      YIN(NN)= G(N+14)
      ZIN(NN)=(G(N+ 8)-G(N+ 2)-G(N+ 2)-G(N+ 2))
      NN=NN+1
      DUM=ONE
      IF(NORM) DUM=SQRT5
      XIN(NN)=DUM* (G(N+ 9)-G(N+ 3)-G(N+ 3))
      YIN(NN)=DUM* (G(N+15)-G(N  ))
      ZIN(NN)=DUM* G(N+18)
      NN=NN+1
      XIN(NN)=DUM* (G(N+10)-G(N+ 4)-G(N+ 4))
      YIN(NN)=DUM* G(N+18)
      ZIN(NN)=DUM* (G(N+16)-G(N  ))
      NN=NN+1
      XIN(NN)=DUM* (G(N+15)-G(N+ 1))
      YIN(NN)=DUM* (G(N+11)-G(N+ 3)-G(N+ 3))
      ZIN(NN)=DUM*  G(N+19)
      NN=NN+1
      XIN(NN)=DUM* G(N+19)
      YIN(NN)=DUM* (G(N+12)-G(N+ 5)-G(N+ 5))
      ZIN(NN)=DUM* (G(N+17)-G(N+ 1))
      NN=NN+1
      XIN(NN)=DUM* (G(N+16)-G(N+ 2))
      YIN(NN)=DUM*  G(N+20)
      ZIN(NN)=DUM* (G(N+13)-G(N+ 4)-G(N+ 4))
      NN=NN+1
      XIN(NN)=DUM*  G(N+20)
      YIN(NN)=DUM* (G(N+17)-G(N+ 2))
      ZIN(NN)=DUM* (G(N+14)-G(N+ 5)-G(N+ 5))
      NN=NN+1
      IF(NORM) DUM=DUM*SQRT3
      XIN(NN)=DUM* (G(N+18)-G(N+ 5))
      YIN(NN)=DUM* (G(N+19)-G(N+ 4))
      ZIN(NN)=DUM* (G(N+20)-G(N+ 3))
  900 N = N+21
C
C     ----- CALCULATE CONTRIBUTION TO GRADIENT -----
C
      N = 0
      DO 980 J = MINJ,MAXJ
      JN = LOCJ+J
      DO 980 I = MINI,MAXI
      N = N+1
      IN = LOCI+I
      IF (IN-JN) 920,940,940
  920 ID = JN
      JD = IN
      GO TO 960
  940 ID = IN
      JD = JN
  960 NN = (ID*(ID-1))/2+JD
      DUM = DM(NN)
      DUM = DUM+DUM
      DE(1,IAT) = DE(1,IAT)+DUM*XIN(N)
      DE(2,IAT) = DE(2,IAT)+DUM*YIN(N)
      DE(3,IAT) = DE(3,IAT)+DUM*ZIN(N)
      IF (DBG .AND. MASWRK) WRITE (IW,9108) II,JJ,I,J,NN,DUM,XIN(N),
     *              YIN(N),ZIN(N),IAT,DE(1,IAT),DE(2,IAT),DE(3,IAT)
  980 CONTINUE
      IF (DBG) CALL EGOUT(DE,NAT)
 1040 CONTINUE
 1060 CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
      IF (OUT) THEN
         IF (MASWRK) WRITE (IW,9008)
         CALL EGOUT(DE,NAT)
      END IF
 1120 CONTINUE
      IF (MASWRK.AND.OUT) THEN
         WRITE (IW,9088)
         CALL TIMIT(1)
      ENDIF
      IF(GOPARR .AND. NXT) CALL DDI_DLBRESET
      RETURN
C
 9008 FORMAT(/10X,45("-")/10X,'EFQ CONTRIBUTIONS',
     *        ' TO THE GRADIENT'/10X,45(1H-))
 9088 FORMAT(/1X,'..... END OF EFQ GRADIENT ......')
 9108 FORMAT(1X,'II,JJ,I,J,NN=',4I5,I6,4E20.12/
     *       1X,'IAT,DE(1,2,3)',18X,I5,3E20.12)
      END
C*MODULE EFGRDB  *DECK ODXYZ
      SUBROUTINE ODXYZ
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     ----- GAUSS-HERMITE QUADRATURE USING MINIMUM POINT FORMULA -----
C
      COMMON/DSTV/XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ,
     *            CX,CY,CZ
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
      NPTS=(NI+NJ+3-2)/2+1
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
      PX=PX*PX*PX
      PY=PY*PY*PY
      PZ=PZ*PZ*PZ
      AX=PTX-XI
      AY=PTY-YI
      AZ=PTZ-ZI
      BX=PTX-XJ
      BY=PTY-YJ
      BZ=PTZ-ZJ
      DUM=W(I)
      IF(DUM+PX.EQ.DUM)PX=ZERO
      IF(DUM+PY.EQ.DUM)PY=ZERO
      IF(DUM+PZ.EQ.DUM)PZ=ZERO
      IF(DUM+AX.EQ.DUM)AX=ZERO
      IF(DUM+AY.EQ.DUM)AY=ZERO
      IF(DUM+AZ.EQ.DUM)AZ=ZERO
      IF(DUM+BX.EQ.DUM)BX=ZERO
      IF(DUM+BY.EQ.DUM)BY=ZERO
      IF(DUM+BZ.EQ.DUM)BZ=ZERO
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
   12 XINT=XINT+DUM*PX
      YINT=YINT+DUM*PY
      ZINT=ZINT+DUM*PZ
   13 CONTINUE
      RETURN
      END
C*MODULE EFGRDB  *DECK PDXYZ
      SUBROUTINE PDXYZ
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     ----- GAUSS-HERMITE QUADRATURE USING MINIMUM POINT FORMULA -----
C
      COMMON/DSTV/XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ,
     *            CX,CY,CZ
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
      DUM=W(I)
      IF(DUM+PX.EQ.DUM)PX=ZERO
      IF(DUM+PY.EQ.DUM)PY=ZERO
      IF(DUM+PZ.EQ.DUM)PZ=ZERO
      IF(DUM+AX.EQ.DUM)AX=ZERO
      IF(DUM+AY.EQ.DUM)AY=ZERO
      IF(DUM+AZ.EQ.DUM)AZ=ZERO
      IF(DUM+BX.EQ.DUM)BX=ZERO
      IF(DUM+BY.EQ.DUM)BY=ZERO
      IF(DUM+BZ.EQ.DUM)BZ=ZERO
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
   12 XINT=XINT+DUM*PX
      YINT=YINT+DUM*PY
      ZINT=ZINT+DUM*PZ
   13 CONTINUE
      RETURN
      END
C*MODULE EFGRDB  *DECK QDXYZ
      SUBROUTINE QDXYZ
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/DSTV/XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ,
     *            CX,CY,CZ
      COMMON/HERMIT/H1,H2(2),H3(3),H4(4),H5(5),H6(6),H7(7)
      COMMON/WERMIT/W1,W2(2),W3(3),W4(4),W5(5),W6(6),W7(7)
      DIMENSION H(21),W(21),MIN(6),MAX(6)
      EQUIVALENCE (H(1),H1),(W(1),W1)
      DATA MIN /1,2,4,7,11,16/
      DATA MAX /1,3,6,10,15,21/
      DATA ZERO /0.0D+00/
C
C     ----- GAUSS-HERMITE QUADRATURE USING MINIMUM POINT FORMULA -----
C
      XINT=ZERO
      YINT=ZERO
      ZINT=ZERO
      NPTS=(NI+NJ+2-2)/2+1
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
      PX=PX*PX
      PY=PY*PY
      PZ=PZ*PZ
      AX=PTX-XI
      AY=PTY-YI
      AZ=PTZ-ZI
      BX=PTX-XJ
      BY=PTY-YJ
      BZ=PTZ-ZJ
      DUM=W(I)
      IF(DUM+PX.EQ.DUM)PX=ZERO
      IF(DUM+PY.EQ.DUM)PY=ZERO
      IF(DUM+PZ.EQ.DUM)PZ=ZERO
      IF(DUM+AX.EQ.DUM)AX=ZERO
      IF(DUM+AY.EQ.DUM)AY=ZERO
      IF(DUM+AZ.EQ.DUM)AZ=ZERO
      IF(DUM+BX.EQ.DUM)BX=ZERO
      IF(DUM+BY.EQ.DUM)BY=ZERO
      IF(DUM+BZ.EQ.DUM)BZ=ZERO
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
   12 XINT=XINT+DUM*PX
      YINT=YINT+DUM*PY
      ZINT=ZINT+DUM*PZ
   13 CONTINUE
      RETURN
      END
