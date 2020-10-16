C  3 JUL 03 - JMM - SUPPRESS PRINTING FOR MONTE CARLO JOBS
C  7 AUG 02 - HT  - USE DYNAMIC MEMORY FOR EFP PAULI REPULSION
C 17 APR 02 - MWS - SYNCH UP EFMULT AND FRGINF COMMON
C 20 FEB 01 - PND - EFFT: PRINTING CHANGES
C 28 SEP 97 - MWS - CONVERT PARALLEL CALLS FROM TCGMSG TO DDI
C 16 JUL 97 - GNM - EFOUT: LIMIT PRINTOUT, EFFT: FRAGONLY CHANGES
C 18 DEC 96 - JHJ - EFFT,EFOUT: PAULI REPULSION CHANGES
C 10 SEP 96 - MWS - INCLUDE THIS CODE IN DISTRIBUTION VERSION
C 13 JUN 96 - MWS - DELETE DEAD CODE RMASS
C 24 MAY 96 - WC  - EFPDEF: DELETE DAMPING TERM, AXE EFPEX IN /EFPPAR/,
C                   DYNAMIC MEMORY FOR QUAD AND OCT VECTORS
C  9 JAN 96 - MWS - EXTEND FRGMSS COMMON
C  5 JAN 95 - PND - CHANGES FOR UNSYMMETRIC POLARIZABILITY TENSORS
C 23 NOV 94 - MWS - REMOVE ALL FTNCHEK WARNINGS
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C
C*MODULE EFGRDC  *DECK EFCDEF
      SUBROUTINE EFCDEF(DM,CHDINT,L4)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL NORM,GOPARR,DSKWRK,MASWRK,NXT
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      PARAMETER (MXSH=1000, MXGTOT=5000)
      PARAMETER (MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION DM(*),CHDINT(L4)
C
      DIMENSION DIJ(100),XIN(432),YIN(432),ZIN(432)
      DIMENSION FIJ(100),GIJ(100)
      DIMENSION IX(20),IY(20),IZ(20),JX(20),JY(20),JZ(20)
      DIMENSION IJX(100),IJY(100),IJZ(100)
C
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
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
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
C
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
      DATA PI212 /1.1283791670955D+00/
      DATA SQRT3 /1.73205080756888D+00/
      DATA SQRT5 /2.23606797749979D+00/
      DATA ZERO,ONE/0.0D+00,1.0D+00/
      DATA RLN10 /2.30258D+00/
      DATA QLIM /1.0D-07/
C
C     INITIALIZATION FOR PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      LCNT = -1
      NEFD = NMTTPT
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C     ----- ISHELL
C
      DO 600 II = 1,NSHELL
C
C           GO PARALLEL!
C
      IF(NXT .AND. GOPARR) THEN
         LCNT = LCNT + 1
         IF(LCNT.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
         IF(NEXT.NE.LCNT) GO TO 600
      END IF
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
C     WRITE(6,5432)II,IAT,I1,I2,LIT,MINI,MAXI,LOCI
C5432 FORMAT(8I6)
C
C     ----- JSHELL
C
      DO 580 JJ = 1,NSHELL
C
C           GO PARALLEL!
C
        IF((.NOT.NXT) .AND. GOPARR) THEN
           IPCOUNT = IPCOUNT + 1
           IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 580
        END IF
C
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
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      IJ = 0
      MAX = MAXJ
      DO 50 I = MINI,MAXI
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
      DO 50 J = MINJ,MAX
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
C     WRITE(6,*)'I,J,IJ ',I,J,IJ
  50  CONTINUE
      ICC=1
      DO 60 IC = 1,NEFD
      DO 60 I=1,IJ
      CHDINT(ICC) = ZERO
      ICC=ICC+1
      CHDINT(ICC) = ZERO
      ICC=ICC+1
      CHDINT(ICC) = ZERO
      ICC=ICC+1
  60  CONTINUE
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
C
C     ----- J PRIMITIVE
C
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
      CFJ = CF(JG)
      AX = (AXI+AJ*XJ)*AA1
      AY = (AYI+AJ*YJ)*AA1
      AZ = (AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR
C
      MAX = MAXJ
      NN = 0
      DUM1=ZERO
      DUM2=DUM1
      DO 170 I=MINI,MAXI
      GO TO (70,80,110,110,90,110,110,95,110,110,
     1       102,110,110,104,110,110,110,110,110,108),I
  70  DUM1=CSI*FAC
      GO TO 110
  80  DUM1=CPI*FAC
      GO TO 110
  90  DUM1=CDI*FAC
      GO TO 110
  95  IF(NORM) DUM1=DUM1*SQRT3
      GO TO 110
 102  DUM1=CFI*FAC
      GO TO 110
 104  DUM1 = DUM1 *SQRT5
      GO TO 110
 108  DUM1 = DUM1 * SQRT3
 110  CONTINUE
      DO 170 J=MINJ,MAX
      GO TO (125,130,160,160,140,160,160,150,160,160,
     1       152,160,160,154,160,160,160,160,160,156),J
  125 DUM2=DUM1*CSJ
      GO TO 160
  130 DUM2=DUM1*CPJ
      GO TO 160
  140 DUM2=DUM1*CDJ
      GO TO 160
  150 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 160
  152 DUM2 = DUM1 * CFJ
      GO TO 160
  154 DUM2 = DUM2 *SQRT5
      GO TO 160
  156 DUM2 = DUM2 * SQRT3
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
      ICC=1
      DO 480 IC = 1,NEFD
      IF(.NOT.DOMONO(IC))GO TO 480
      ALFA = EFATRM(IC)
      BETA = EFBTRM(IC)
      CX   = EFC(1,IC)
      CY   = EFC(2,IC)
      CZ   = EFC(3,IC)
      DUMA = PI212/(AA+ALFA)
      DUMA=DUMA+DUMA
      ZNUC = EFCHG(1,IC)+EFCHG(2,IC)
      DO 385 I = 1,IJ
      GIJ(I) = DIJ(I)*DUMA
  385 CONTINUE
      XX   = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
      IF(NROOTS.LE.3) CALL RT123
      IF(NROOTS.EQ.4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         WRITE(IW,9008)
         CALL ABRT
      END IF
      MM = 0
      DO 401 K = 1,NROOTS
      UU = AA*U(K)
      WW = W(K)*ZNUC
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
      ICCT=ICC
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
      CHDINT(ICC)=CHDINT(ICC)-DUM*DUMX
      ICC=ICC+1
      CHDINT(ICC)=CHDINT(ICC)-DUM*DUMY
      ICC=ICC+1
      CHDINT(ICC)=CHDINT(ICC)-DUM*DUMZ
      ICC=ICC+1
 403  CONTINUE
C     WRITE(6,2222)IGG,JGG,ICG,IGI,ICCG,DUMG,DUMYG,CHG
C2222 FORMAT(5I5,3E15.6)
C
C  SUBTRACT DAMPING FUNCTION TERM
C
      IF ( ABS(ALFA).LE.QLIM ) GO TO 480
      ZNUC = EFCHG(1,IC)
      PCSQ = (AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2
      XX=AA*AA*PCSQ/(AA+ALFA)
      PREI = EXP(-AA*ALFA*PCSQ/(AA+ALFA))
      IF(NROOTS.LE.3) CALL RT123
      IF(NROOTS.EQ.4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         WRITE(IW,9008)
         CALL ABRT
      END IF
      MM = 0
      DO 425 K = 1,NROOTS
      UU = (ALFA+AA)*U(K)
      WW = W(K)*ZNUC
      WW = WW*(UU+ALFA)
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
      ICC=ICCT
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
      CHDINT(ICC)=CHDINT(ICC)-DUM*DUMX
      ICC=ICC+1
      CHDINT(ICC)=CHDINT(ICC)-DUM*DUMY
      ICC=ICC+1
      CHDINT(ICC)=CHDINT(ICC)-DUM*DUMZ
      ICC=ICC+1
 465  CONTINUE
C
 480  CONTINUE
 500  CONTINUE
 520  CONTINUE
C
C     ----- END OF *PRIMITIVE* LOOPS -----
C     ----- SET DEF MATRIX
C
      MAX=MAXJ
      ICC=1
      DO 551 IC = 1,NEFD
      IF(.NOT.DOMONO(IC))GO TO 551
      DO 550 I=MINI,MAXI
      LI=LOCI+I
      DO 550 J=MINJ,MAX
      LJ=LOCJ+J
      IF (LI-LJ) 920,940,940
  920 ID = LJ
      JD = LI
      GO TO 960
  940 ID = LI
      JD = LJ
  960 NN = (ID*(ID-1))/2+JD
      DUM = DM(NN)
      DEF(1,IC)=DEF(1,IC) + DUM*CHDINT(ICC)
      ICC=ICC+1
      DEF(2,IC)=DEF(2,IC) + DUM*CHDINT(ICC)
      ICC=ICC+1
      DEF(3,IC)=DEF(3,IC) + DUM*CHDINT(ICC)
      ICC=ICC+1
  550 CONTINUE
  551 CONTINUE
C-------- END OF EFD CENTERS LOOP
C
  580 CONTINUE
  600 CONTINUE
C
      IF(GOPARR .AND. NXT) CALL DDI_DLBRESET
      RETURN
 9008 FORMAT(/' NUMBER OF POLYNOMIAL ROOTS NEEDED (NROOTS) IS GREATER',
     *        ' THAN 6 IN EFDINT.  CALL A PROGRAMMER/QUANTUM CHEMIST.')
      END
C*MODULE EFGRDC  *DECK EFQDEF
      SUBROUTINE EFQDEF(DM,CHOINT,L4)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
      COMMON /FMCOM / XX(1)
C
      CALL VALFM(LOADFM)
      NEED=6*MXFGPT
      CALL GETFM(NEED)
      CALL EFQDE(DM,CHOINT,L4,XX(LOADFM+1))
      CALL RETFM(NEED)
C
      RETURN
      END
C*MODULE EFGRDC  *DECK EFQDE
      SUBROUTINE EFQDE(DM,CHOINT,L4,QUAD)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL NORM,GOPARR,DSKWRK,MASWRK,NXT
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      PARAMETER (MXSH=1000, MXGTOT=5000)
      PARAMETER (MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION DM(*),CHOINT(L4)
C
      DIMENSION QUAD(6,*)
      DIMENSION DIJ(100),XIN(576),YIN(576),ZIN(576)
      DIMENSION FIJ(100),GIJ(100)
      DIMENSION IX(20),IY(20),IZ(20),JX(20),JY(20),JZ(20)
      DIMENSION IJX(100),IJY(100),IJZ(100),W4(9),W6(9)
C
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
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
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
C
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
      DATA PI212 /1.1283791670955D+00/
      DATA SQRT3 /1.73205080756888D+00/
      DATA SQRT5 /2.23606797749979D+00/
      DATA ZERO,ONE,TWO,FOUR/0.0D+00,1.0D+00,2.0D+00,4.0D+00/
      DATA PT5,ONEPT5,THREE /0.5D+00,1.5D+00,3.0D+00/
      DATA EIGHT/8.0D+00/
      DATA RLN10 /2.30258D+00/
      DATA QLIM /1.0D-07/
C
      NEFO = NMTTPT
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C     INITIALIZATION FOR PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      LCNT = -1
C
C  REPLACE SECOND MOMENTS IN EFQAD WITH ELECTRIC QUADRUPOLE TENSORS
C
      DO 20 I=1,NEFO
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
C
C           GO PARALLEL!
C
      IF(NXT .AND. GOPARR) THEN
         LCNT = LCNT + 1
         IF(LCNT.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
         IF(NEXT.NE.LCNT) GO TO 600
      END IF
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
      DO 580 JJ = 1,NSHELL
C
C           GO PARALLEL!
C
        IF((.NOT.NXT) .AND. GOPARR) THEN
           IPCOUNT = IPCOUNT + 1
           IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 580
        END IF
C
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
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      IJ = 0
      MAX = MAXJ
      DO 50 I = MINI,MAXI
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
      DO 50 J = MINJ,MAX
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
  50  CONTINUE
      ICC=1
      DO 60 IC = 1,NEFO
      DO 60 I=1,IJ
      CHOINT(ICC) = ZERO
      ICC=ICC+1
      CHOINT(ICC) = ZERO
      ICC=ICC+1
      CHOINT(ICC) = ZERO
      ICC=ICC+1
  60  CONTINUE
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
C     ----- J PRIMITIVE
C
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
      MAX = MAXJ
      NN = 0
      DUM1=ZERO
      DUM2=DUM1
      DO 170 I=MINI,MAXI
      GO TO (70,80,110,110,90,110,110,95,110,110,
     1       102,110,110,104,110,110,110,110,110,108),I
  70  DUM1=CSI*FAC
      GO TO 110
  80  DUM1=CPI*FAC
      GO TO 110
  90  DUM1=CDI*FAC
      GO TO 110
  95  IF(NORM) DUM1=DUM1*SQRT3
      GO TO 110
 102  DUM1=CFI*FAC
      GO TO 110
 104  DUM1 = DUM1 *SQRT5
      GO TO 110
 108  DUM1 = DUM1 * SQRT3
 110  CONTINUE
      DO 170 J=MINJ,MAX
      GO TO (125,130,160,160,140,160,160,150,160,160,
     1       152,160,160,154,160,160,160,160,160,156),J
  125 DUM2=DUM1*CSJ
      GO TO 160
  130 DUM2=DUM1*CPJ
      GO TO 160
  140 DUM2=DUM1*CDJ
      GO TO 160
  150 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 160
  152 DUM2 = DUM1 * CFJ
      GO TO 160
  154 DUM2 = DUM2 *SQRT5
      GO TO 160
  156 DUM2 = DUM2 * SQRT3
  160 NN=NN+1
  170 DIJ(NN)=DUM2
C
C     ..... THIRD DERIVATIVE TERM .....
C     BEWARE BUCKINGHAM QUART. REV.(LONDON) -13-,183(1959)
C     A THREE IN EQU. 15 WAS ABSORBED INTO A FIFTEEN TO GIVE FIVE
C     IN EQU. 16
C
C     DUM = PI212*AA1/(THREE*FIVE)
      DUM = PI212*AA1/THREE
      DO 380 I = 1,IJ
      FIJ(I) = DIJ(I)*DUM
  380 CONTINUE
      AAX = AA*AX
      AAY = AA*AY
      AAZ = AA*AZ
C
      ICC=1
      DO 480 IC = 1,NEFO
      IF(.NOT.DOQUAD(IC))GO TO 480
      ALFA = EFATRM(IC)
      BETA = EFBTRM(IC)
      CX   = EFC(1,IC)
      CY   = EFC(2,IC)
      CZ   = EFC(3,IC)
C     DUM = PI212/(THREE*FIVE*(AA+ALFA))
      DUM = PI212/(THREE*(AA+ALFA))
      DO 385 I = 1,IJ
      GIJ(I) = DIJ(I)*DUM
  385 CONTINUE
      XX   = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
      IF(NROOTS.LE.3) CALL RT123
      IF(NROOTS.EQ.4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         WRITE(IW,9008)
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
      ICCT=ICC
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
      CHOINT(ICC) = CHOINT(ICC) -
     1     DUM * ( DXXX * QUAD(1,IC)
     1           + DXYY * QUAD(2,IC)
     1           + DXZZ * QUAD(3,IC)
     1 + TWO * ( DXXY * QUAD(4,IC)
     1           + DXXZ * QUAD(5,IC)
     1           + DXYZ * QUAD(6,IC)))
      ICC=ICC+1
      CHOINT(ICC) = CHOINT(ICC) -
     1     DUM * ( DXXY * QUAD(1,IC)
     1           + DYYY * QUAD(2,IC)
     1           + DYZZ * QUAD(3,IC)
     1 + TWO * ( DXYY * QUAD(4,IC)
     1           + DXYZ * QUAD(5,IC)
     1           + DYYZ * QUAD(6,IC)))
      ICC=ICC+1
      CHOINT(ICC) = CHOINT(ICC) -
     1     DUM * ( DXXZ * QUAD(1,IC)
     1           + DYYZ * QUAD(2,IC)
     1           + DZZZ * QUAD(3,IC)
     1 + TWO * ( DXYZ * QUAD(4,IC)
     1           + DXZZ * QUAD(5,IC)
     1           + DYZZ * QUAD(6,IC)))
      ICC=ICC+1
 404  CONTINUE
C
C  SUBTRACT DAMPING FUNCTION TERM
C
      IF ( ABS(ALFA).LE.QLIM ) GO TO 480
      PCSQ = (AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2
      XX=AA*AA*PCSQ/(AA+ALFA)
      PREI = EXP(-AA*ALFA*PCSQ/(AA+ALFA))
      IF(NROOTS.LE.3) CALL RT123
      IF(NROOTS.EQ.4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         WRITE(IW,9008)
         CALL ABRT
      END IF
      MM = 0
      DO 425 K = 1,NROOTS
      U2 = (ALFA+AA)*U(K)
      U4 = U2*U2
      U6 = U4*(U2+ALFA)
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
      ICC=ICCT
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
      CHOINT(ICC) = CHOINT(ICC) -
     1     DUM * ( DXXX * QUAD(1,IC)
     1           + DXYY * QUAD(2,IC)
     1           + DXZZ * QUAD(3,IC)
     1 + TWO * ( DXXY * QUAD(4,IC)
     1           + DXXZ * QUAD(5,IC)
     1           + DXYZ * QUAD(6,IC)))
      ICC=ICC+1
      CHOINT(ICC) = CHOINT(ICC) -
     1     DUM * ( DXXY * QUAD(1,IC)
     1           + DYYY * QUAD(2,IC)
     1           + DYZZ * QUAD(3,IC)
     1 + TWO * ( DXYY * QUAD(4,IC)
     1           + DXYZ * QUAD(5,IC)
     1           + DYYZ * QUAD(6,IC)))
      ICC=ICC+1
      CHOINT(ICC) = CHOINT(ICC) -
     1     DUM * ( DXXZ * QUAD(1,IC)
     1           + DYYZ * QUAD(2,IC)
     1           + DZZZ * QUAD(3,IC)
     1 + TWO * ( DXYZ * QUAD(4,IC)
     1           + DXZZ * QUAD(5,IC)
     1           + DYZZ * QUAD(6,IC)))
      ICC=ICC+1
 465  CONTINUE
C
 480  CONTINUE
 500  CONTINUE
 520  CONTINUE
C
C     ----- END OF *PRIMITIVE* LOOPS -----
C     ----- SET DEF MATRIX
C
      MAX=MAXJ
      NN=0
      ICC=1
C
      DO 551 IC = 1,NEFO
      IF(.NOT.DOQUAD(IC))GO TO 551
      DO 550 I=MINI,MAXI
      LI=LOCI+I
      DO 550 J=MINJ,MAX
      LJ=LOCJ+J
      IF (LI-LJ) 920,940,940
  920 ID = LJ
      JD = LI
      GO TO 960
  940 ID = LI
      JD = LJ
  960 NN = (ID*(ID-1))/2+JD
      DUM = DM(NN)
      DEF(1,IC)=DEF(1,IC) + DUM*CHOINT(ICC)
      ICC=ICC+1
      DEF(2,IC)=DEF(2,IC) + DUM*CHOINT(ICC)
      ICC=ICC+1
      DEF(3,IC)=DEF(3,IC) + DUM*CHOINT(ICC)
      ICC=ICC+1
  550 CONTINUE
  551 CONTINUE
C
C-------- END OF EFO CENTERS LOOP
C
  580 CONTINUE
  600 CONTINUE
C
C     ----- END OF *SHELL* LOOPS -----
C
      IF(GOPARR .AND. NXT) CALL DDI_DLBRESET
      RETURN
 9008 FORMAT(/' NUMBER OF POLYNOMIAL ROOTS NEEDED (NROOTS) IS GREATER',
     *        ' THAN 6 IN EFOINT.  CALL A PROGRAMMER/QUANTUM CHEMIST.')
      END
C*MODULE EFGRDC  *DECK EFDDEF
      SUBROUTINE EFDDEF(DM,CHQINT,L4)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL NORM,GOPARR,DSKWRK,MASWRK,NXT
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      PARAMETER (MXSH=1000, MXGTOT=5000)
      PARAMETER (MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION DM(*),CHQINT(L4)
C
      DIMENSION DIJ(100),XIN(432),YIN(432),ZIN(432)
      DIMENSION FIJ(100),GIJ(100)
      DIMENSION IX(20),IY(20),IZ(20),JX(20),JY(20),JZ(20)
      DIMENSION IJX(100),IJY(100),IJZ(100),W2(5),W4(5)
C
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
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
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
C
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
      DATA PI212 /1.1283791670955D+00/
      DATA SQRT3 /1.73205080756888D+00/
      DATA SQRT5 /2.23606797749979D+00/
      DATA ZERO,ONE,TWO,FOUR/0.0D+00,1.0D+00,2.0D+00,4.0D+00/
      DATA RLN10 /2.30258D+00/
      DATA QLIM /1.0D-07/
C
C     INITIALIZATION FOR PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      LCNT = -1
      NEFQ = NMTTPT
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C     ----- ISHELL
C
      DO 600 II = 1,NSHELL
C
C           GO PARALLEL!
C
      IF(NXT .AND. GOPARR) THEN
         LCNT = LCNT + 1
         IF(LCNT.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
         IF(NEXT.NE.LCNT) GO TO 600
      END IF
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
      DO 580 JJ = 1,NSHELL
C
C           GO PARALLEL!
C
        IF((.NOT.NXT) .AND. GOPARR) THEN
           IPCOUNT = IPCOUNT + 1
           IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 580
        END IF
C
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
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      IJ = 0
      MAX = MAXJ
      DO 30 I = MINI,MAXI
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
      DO 30 J = MINJ,MAX
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
  30  CONTINUE
      ICC=1
      DO 60 IC=1,NEFQ
      DO 60 I=1,IJ
      CHQINT(ICC) = ZERO
      ICC=ICC+1
      CHQINT(ICC) = ZERO
      ICC=ICC+1
      CHQINT(ICC) = ZERO
      ICC=ICC+1
  60  CONTINUE
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
C     ----- J PRIMITIVE
C
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
      MAX = MAXJ
      NN = 0
      DUM1=ZERO
      DUM2=DUM1
      DO 170 I=MINI,MAXI
      GO TO (70,80,110,110,90,110,110,95,110,110,
     1       102,110,110,104,110,110,110,110,110,108),I
  70  DUM1=CSI*FAC
      GO TO 110
  80  DUM1=CPI*FAC
      GO TO 110
  90  DUM1=CDI*FAC
      GO TO 110
  95  IF(NORM) DUM1=DUM1*SQRT3
      GO TO 110
 102  DUM1=CFI*FAC
      GO TO 110
 104  DUM1 = DUM1 *SQRT5
      GO TO 110
 108  DUM1 = DUM1 * SQRT3
 110  CONTINUE
      DO 170 J=MINJ,MAX
      GO TO (125,130,160,160,140,160,160,150,160,160,
     1       152,160,160,154,160,160,160,160,160,156),J
C
  125 DUM2=DUM1*CSJ
      GO TO 160
  130 DUM2=DUM1*CPJ
      GO TO 160
  140 DUM2=DUM1*CDJ
      GO TO 160
  150 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 160
  152 DUM2 = DUM1 * CFJ
      GO TO 160
  154 DUM2 = DUM2 *SQRT5
      GO TO 160
  156 DUM2 = DUM2 * SQRT3
  160 NN=NN+1
  170 DIJ(NN)=DUM2
C
C     ..... HELLMANN-FEYNMAN TERM .....
C
C     DUM = PI212*AA1/THREE
      DUM = PI212*AA1
      DO 380 I = 1,IJ
      FIJ(I) = DIJ(I)*DUM
  380 CONTINUE
      AAX = AA*AX
      AAY = AA*AY
      AAZ = AA*AZ
C
      ICC=1
      DO 480 IC = 1,NEFQ
      IF(.NOT.DODIPO(IC))GO TO 480
      ALFA = EFATRM(IC)
      BETA = EFBTRM(IC)
      CX   = EFC(1,IC)
      CY   = EFC(2,IC)
      CZ   = EFC(3,IC)
C     DUMA = PI212/(THREE*(AA+ALFA))
      DUMA = PI212/(AA+ALFA)
      DO 385 I = 1,IJ
      GIJ(I) = DIJ(I)*DUMA
  385 CONTINUE
      XX   = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
      IF(NROOTS.LE.3) CALL RT123
      IF(NROOTS.EQ.4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         WRITE(IW,9008)
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
      ICCT=ICC
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
      DUMXX= DUMXX+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            -XIN(NX+MM+288)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)
      DUMYY= DUMYY+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            -XIN(NX+MM    )*YIN(NY+MM+288)*ZIN(NZ+MM    )*W4(K)
      DUMZZ= DUMZZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            -XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+288)*W4(K)
      DUMXY= DUMXY-XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM    )*W4(K)
      DUMXZ= DUMXZ-XIN(NX+MM+144)*YIN(NY+MM    )*ZIN(NZ+MM+144)*W4(K)
      DUMYZ= DUMYZ-XIN(NX+MM    )*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W4(K)
  402 MM  = MM+16
      DUM = FIJ(I)
      CHQINT(ICC) = CHQINT(ICC) +
     1 DUM * ( DUMXX * EFDIP(1,IC)
     2       + DUMXY * EFDIP(2,IC)
     3       + DUMXZ * EFDIP(3,IC))
      ICC=ICC+1
      CHQINT(ICC) = CHQINT(ICC) +
     1 DUM * ( DUMXY * EFDIP(1,IC)
     2       + DUMYY * EFDIP(2,IC)
     3       + DUMYZ * EFDIP(3,IC))
      ICC=ICC+1
      CHQINT(ICC) = CHQINT(ICC) +
     1 DUM * ( DUMXZ * EFDIP(1,IC)
     2       + DUMYZ * EFDIP(2,IC)
     3       + DUMZZ * EFDIP(3,IC))
      ICC=ICC+1
 403  CONTINUE
C
C  SUBTRACT DAMPING FUNCTION TERM
C
      IF ( ABS(ALFA).LE.QLIM ) GO TO 480
      PCSQ = (AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2
      XX=AA*AA*PCSQ/(AA+ALFA)
      PREI = EXP(-AA*ALFA*PCSQ/(AA+ALFA))
      IF(NROOTS.LE.3) CALL RT123
      IF(NROOTS.EQ.4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         WRITE(IW,9008)
         CALL ABRT
      END IF
      MM = 0
      DO 425 K = 1,NROOTS
      U2 = (ALFA+AA)*U(K)
      U4 = U2*(U2+ALFA)
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
      ICC=ICCT
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
      DUMXX= DUMXX+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            -XIN(NX+MM+288)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)
      DUMYY= DUMYY+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            -XIN(NX+MM    )*YIN(NY+MM+288)*ZIN(NZ+MM    )*W4(K)
      DUMZZ= DUMZZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            -XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+288)*W4(K)
      DUMXY= DUMXY-XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM    )*W4(K)
      DUMXZ= DUMXZ-XIN(NX+MM+144)*YIN(NY+MM    )*ZIN(NZ+MM+144)*W4(K)
      DUMYZ= DUMYZ-XIN(NX+MM    )*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W4(K)
  445 MM  = MM+16
      DUM =-GIJ(I)*PREI * BETA
      CHQINT(ICC) = CHQINT(ICC) +
     1 DUM * ( DUMXX * EFDIP(1,IC)
     2       + DUMXY * EFDIP(2,IC)
     3       + DUMXZ * EFDIP(3,IC))
      ICC=ICC+1
      CHQINT(ICC) = CHQINT(ICC) +
     1 DUM * ( DUMXY * EFDIP(1,IC)
     2       + DUMYY * EFDIP(2,IC)
     3       + DUMYZ * EFDIP(3,IC))
      ICC=ICC+1
      CHQINT(ICC) = CHQINT(ICC) +
     1 DUM * ( DUMXZ * EFDIP(1,IC)
     2       + DUMYZ * EFDIP(2,IC)
     3       + DUMZZ * EFDIP(3,IC))
      ICC=ICC+1
 465  CONTINUE
C
 480  CONTINUE
 500  CONTINUE
 520  CONTINUE
C
C     ----- END OF *PRIMITIVE* LOOPS -----
C     ----- SET DEF MATRIX
C
      MAX=MAXJ
      NN=0
      ICC=1
C
      DO 551 IC = 1,NEFQ
      IF(.NOT.DODIPO(IC))GO TO 551
      DO 550 I=MINI,MAXI
      LI=LOCI+I
C     IN = (LI*(LI-1))/2
      DO 550 J=MINJ,MAX
      LJ=LOCJ+J
      IF (LI-LJ) 920,940,940
  920 ID = LJ
      JD = LI
      GO TO 960
  940 ID = LI
      JD = LJ
  960 NN = (ID*(ID-1))/2+JD
      DUM = DM(NN)
      DEF(1,IC)=DEF(1,IC) + DUM*CHQINT(ICC)
      ICC=ICC+1
      DEF(2,IC)=DEF(2,IC) + DUM*CHQINT(ICC)
      ICC=ICC+1
      DEF(3,IC)=DEF(3,IC) + DUM*CHQINT(ICC)
      ICC=ICC+1
  550 CONTINUE
  551 CONTINUE
C-------- END OF EFQ CENTERS LOOP
C
  580 CONTINUE
  600 CONTINUE
C
C     ----- END OF *SHELL* LOOPS -----
C
      IF(GOPARR .AND. NXT) CALL DDI_DLBRESET
      RETURN
 9008 FORMAT(/' NUMBER OF POLYNOMIAL ROOTS NEEDED (NROOTS) IS GREATER',
     *        ' THAN 6 IN EFQINT.  CALL A PROGRAMMER/QUANTUM CHEMIST.')
      END
C*MODULE EFGRDC  *DECK EFPDEF
      SUBROUTINE EFPDEF(DM,CHQINT,L4)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 POLNAM
C
      LOGICAL NORM,GOPARR,DSKWRK,MASWRK,NXT
C
      PARAMETER (MXSH=1000, MXGTOT=5000)
      PARAMETER (MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION DM(*),CHQINT(L4)
C
      DIMENSION DIJ(100),XIN(432),YIN(432),ZIN(432)
      DIMENSION FIJ(100)
      DIMENSION IX(20),IY(20),IZ(20),JX(20),JY(20),JZ(20)
      DIMENSION IJX(100),IJY(100),IJZ(100),W2(5),W4(5)
C
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG)
     *                ,EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG)
     *                ,ATORQ(3,MXFRG)
      COMMON /CSSTV / CX,CY,CZ
      COMMON /EFPPAR/ EFP(3,MXFGPT),EFPOL(9,MXFGPT),
     *                ENO,DIND(3,MXFGPT),DINDD(3,MXFGPT),POLNAM(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
C
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
      DATA PI212 /1.1283791670955D+00/
      DATA SQRT3 /1.73205080756888D+00/
      DATA SQRT5 /2.23606797749979D+00/
      DATA ZERO,ONE,TWO,FOUR/0.0D+00,1.0D+00,2.0D+00,4.0D+00/
      DATA PT5/0.5D+00/
      DATA RLN10 /2.30258D+00/
C
C     DO 860 IC=1,NEFP
C     WRITE(6,*)'IN EFPDEF,DIND',DIND(1,IC),DIND(2,IC),DIND(3,IC)
C860  CONTINUE
C     INITIALIZATION FOR PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      LCNT = -1
      NEFP = NPTTPT
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C     ----- ISHELL
C
      DO 600 II = 1,NSHELL
C
C           GO PARALLEL!
C
      IF(NXT .AND. GOPARR) THEN
         LCNT = LCNT + 1
         IF(LCNT.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
         IF(NEXT.NE.LCNT) GO TO 600
      END IF
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
      DO 580 JJ = 1,NSHELL
C
C           GO PARALLEL!
C
        IF((.NOT.NXT) .AND. GOPARR) THEN
           IPCOUNT = IPCOUNT + 1
           IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 580
        END IF
C
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
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      IJ = 0
      MAX = MAXJ
      DO 30 I = MINI,MAXI
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
      DO 30 J = MINJ,MAX
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
  30  CONTINUE
      ICC=1
      DO 60 IC=1,NEFP
      DO 60 I=1,IJ
      CHQINT(ICC) = ZERO
      ICC=ICC+1
      CHQINT(ICC) = ZERO
      ICC=ICC+1
      CHQINT(ICC) = ZERO
      ICC=ICC+1
  60  CONTINUE
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
C     ----- J PRIMITIVE
C
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
      MAX = MAXJ
      NN = 0
      DUM1=ZERO
      DUM2=DUM1
      DO 170 I=MINI,MAXI
      GO TO (70,80,110,110,90,110,110,95,110,110,
     1       102,110,110,104,110,110,110,110,110,108),I
  70  DUM1=CSI*FAC
      GO TO 110
  80  DUM1=CPI*FAC
      GO TO 110
  90  DUM1=CDI*FAC
      GO TO 110
  95  IF(NORM) DUM1=DUM1*SQRT3
      GO TO 110
 102  DUM1=CFI*FAC
      GO TO 110
 104  DUM1 = DUM1 *SQRT5
      GO TO 110
 108  DUM1 = DUM1 * SQRT3
 110  CONTINUE
      DO 170 J=MINJ,MAX
      GO TO (125,130,160,160,140,160,160,150,160,160,
     1       152,160,160,154,160,160,160,160,160,156),J
  125 DUM2=DUM1*CSJ
      GO TO 160
  130 DUM2=DUM1*CPJ
      GO TO 160
  140 DUM2=DUM1*CDJ
      GO TO 160
  150 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 160
  152 DUM2 = DUM1 * CFJ
      GO TO 160
  154 DUM2 = DUM2 *SQRT5
      GO TO 160
  156 DUM2 = DUM2 * SQRT3
  160 NN=NN+1
  170 DIJ(NN)=DUM2
C
C     ..... HELLMANN-FEYNMAN TERM .....
C
C     DUM = PI212*AA1/THREE
      DUM = PI212*AA1
      DO 380 I = 1,IJ
      FIJ(I) = DIJ(I)*DUM
  380 CONTINUE
      AAX = AA*AX
      AAY = AA*AY
      AAZ = AA*AZ
C
      ICC=1
      DO 480 IC = 1,NEFP
      CX   = EFP(1,IC)
      CY   = EFP(2,IC)
      CZ   = EFP(3,IC)
      XX   = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
      IF(NROOTS.LE.3) CALL RT123
      IF(NROOTS.EQ.4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         WRITE(IW,9008)
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
      DUMXX= DUMXX+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            -XIN(NX+MM+288)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)
      DUMYY= DUMYY+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            -XIN(NX+MM    )*YIN(NY+MM+288)*ZIN(NZ+MM    )*W4(K)
      DUMZZ= DUMZZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM    )*W2(K)
     1            -XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+288)*W4(K)
      DUMXY= DUMXY-XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM    )*W4(K)
      DUMXZ= DUMXZ-XIN(NX+MM+144)*YIN(NY+MM    )*ZIN(NZ+MM+144)*W4(K)
      DUMYZ= DUMYZ-XIN(NX+MM    )*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W4(K)
  402 MM  = MM+16
C     DUM = PT5*FIJ(I)
      DUM = FIJ(I)
      CHQINT(ICC) = CHQINT(ICC) +
     1 DUM * ( DUMXX * (DIND(1,IC)+DINDD(1,IC))
     2       + DUMXY * (DIND(2,IC)+DINDD(2,IC))
     3       + DUMXZ * (DIND(3,IC)+DINDD(3,IC)))*PT5
C    1 DUM * ( DUMXX * (DIND(1,IC))
C    2       + DUMXY * (DIND(2,IC))
C    3       + DUMXZ * (DIND(3,IC)))
      ICC=ICC+1
      CHQINT(ICC) = CHQINT(ICC) +
     1 DUM * ( DUMXY * (DIND(1,IC)+DINDD(1,IC))
     2       + DUMYY * (DIND(2,IC)+DINDD(2,IC))
     3       + DUMYZ * (DIND(3,IC)+DINDD(3,IC)))*PT5
C    1 DUM * ( DUMXY * (DIND(1,IC))
C    2       + DUMYY * (DIND(2,IC))
C    3       + DUMYZ * (DIND(3,IC)))
      ICC=ICC+1
      CHQINT(ICC) = CHQINT(ICC) +
     1 DUM * ( DUMXZ * (DIND(1,IC)+DINDD(1,IC))
     2       + DUMYZ * (DIND(2,IC)+DINDD(2,IC))
     3       + DUMZZ * (DIND(3,IC)+DINDD(3,IC)))*PT5
C    1 DUM * ( DUMXZ * (DIND(1,IC))
C    2       + DUMYZ * (DIND(2,IC))
C    3       + DUMZZ * (DIND(3,IC)))
      ICC=ICC+1
 403  CONTINUE
C
 480  CONTINUE
 500  CONTINUE
 520  CONTINUE
C
C     ----- END OF *PRIMITIVE* LOOPS -----
C     ----- SET DEF MATRIX
C
      MAX=MAXJ
      NN=0
      ICC=1
C
      DO 551 IC = 1,NEFP
      ICF=IC+NMTTPT
      DO 550 I=MINI,MAXI
      LI=LOCI+I
      DO 550 J=MINJ,MAX
      LJ=LOCJ+J
      IF (LI-LJ) 920,940,940
  920 ID = LJ
      JD = LI
      GO TO 960
  940 ID = LI
      JD = LJ
  960 NN = (ID*(ID-1))/2+JD
      DUM = DM(NN)
      DEF(1,ICF)=DEF(1,ICF) + DUM*CHQINT(ICC)
      ICC=ICC+1
      DEF(2,ICF)=DEF(2,ICF) + DUM*CHQINT(ICC)
      ICC=ICC+1
      DEF(3,ICF)=DEF(3,ICF) + DUM*CHQINT(ICC)
      ICC=ICC+1
  550 CONTINUE
  551 CONTINUE
C
C-------- END OF EFP CENTERS LOOP
C
  580 CONTINUE
  600 CONTINUE
C
C     ----- END OF *SHELL* LOOPS -----
C
      IF(GOPARR .AND. NXT) CALL DDI_DLBRESET
      RETURN
 9008 FORMAT(/' NUMBER OF POLYNOMIAL ROOTS NEEDED (NROOTS) IS GREATER',
     *        ' THAN 6 IN EFPDEF.  CALL A PROGRAMMER/QUANTUM CHEMIST.')
      END
C*MODULE EFGRDC  *DECK REPDEF
      SUBROUTINE REPDEF(DM,CHDINT,L4)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      CHARACTER*8 REPNAM
C
      LOGICAL NORM,GOPARR,DSKWRK,MASWRK,NXT
C
      DIMENSION DM(*),CHDINT(L4)
C
      DIMENSION DIJ(100),XIN(432),YIN(432),ZIN(432)
      DIMENSION GIJ(100)
      DIMENSION IX(20),IY(20),IZ(20),JX(20),JY(20),JZ(20)
      DIMENSION IJX(100),IJY(100),IJZ(100)
C
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG)
     *                ,EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG)
     *                ,ATORQ(3,MXFRG)
      COMMON /CSSTV / CX,CY,CZ
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /REPPAR/ CREP(3,MXFGPT),CLPR(4*MXFGPT),ZLPR(4*MXFGPT),
     *                NLPR(4*MXFGPT),KFR(MXFGPT),KLR(MXFGPT),
     *                REPNAM(MXFGPT)
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
C
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
      DATA ZERO,ONE,TWO/0.0D+00,1.0D+00,2.0D+00/
      DATA RLN10 /2.30258D+00/
      DATA PI212 /1.1283791670955D+00/
      DATA SQRT3 /1.73205080756888D+00/
      DATA SQRT5 /2.23606797749979D+00/
C
C -----CHARGE-REPULSIVE POTENTIAL INTEGRALS FOR POWERS 0 AND -1 OF R.
C
C
C     INITIALIZATION FOR PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      LCNT = -1
      NREP = NRTTPT
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C     ----- ISHELL
C
      DO 600 II = 1,NSHELL
C
C           GO PARALLEL!
C
      IF(NXT .AND. GOPARR) THEN
         LCNT = LCNT + 1
         IF(LCNT.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
         IF(NEXT.NE.LCNT) GO TO 600
      END IF
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
      DO 580 JJ = 1,NSHELL
C
C           GO PARALLEL!
C
        IF((.NOT.NXT) .AND. GOPARR) THEN
           IPCOUNT = IPCOUNT + 1
           IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 580
        END IF
C
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
      NROOTS = (LIT+LJT-2)/2 + 1
      RR = (XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      IJ = 0
      MAX = MAXJ
      DO 50 I = MINI,MAXI
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
      DO 50 J = MINJ,MAX
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
  50  CONTINUE
      ICC=1
      DO 60 IC = 1,NREP
      DO 60 LTERM=KFR(IC),KLR(IC)
      DO 60 I=1,IJ
      DO 60 IICC=1,3
      CHDINT(ICC) = ZERO
      ICC=ICC+1
  60  CONTINUE
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
C     ----- J PRIMITIVE
C
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
      MAX = MAXJ
      NN = 0
      DUM1=ZERO
      DUM2=DUM1
      DO 170 I=MINI,MAXI
      GO TO (70,80,110,110,90,110,110,95,110,110,
     1       102,110,110,104,110,110,110,110,110,108),I
  70  DUM1=CSI*FAC
      GO TO 110
  80  DUM1=CPI*FAC
      GO TO 110
  90  DUM1=CDI*FAC
      GO TO 110
  95  IF(NORM) DUM1=DUM1*SQRT3
      GO TO 110
 102  DUM1=CFI*FAC
      GO TO 110
 104  DUM1 = DUM1 *SQRT5
      GO TO 110
 108  DUM1 = DUM1 * SQRT3
 110  CONTINUE
      DO 170 J=MINJ,MAX
      GO TO (125,130,160,160,140,160,160,150,160,160,
     1       152,160,160,154,160,160,160,160,160,156),J
  125 DUM2=DUM1*CSJ
      GO TO 160
  130 DUM2=DUM1*CPJ
      GO TO 160
  140 DUM2=DUM1*CDJ
      GO TO 160
  150 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 160
  152 DUM2 = DUM1 * CFJ
      GO TO 160
  154 DUM2 = DUM2 *SQRT5
      GO TO 160
  156 DUM2 = DUM2 * SQRT3
  160 NN=NN+1
  170 DIJ(NN)=DUM2
C
      AAX = AA*AX
      AAY = AA*AY
      AAZ = AA*AZ
C
      ICC=1
      DO 480 IC = 1,NREP
      CX   =CREP(1,IC)
      CY   =CREP(2,IC)
      CZ   =CREP(3,IC)
      PCSQ = (AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2
      DO 480 LTERM=KFR(IC),KLR(IC)
      ALFA = ZLPR(LTERM)
      BETA = CLPR(LTERM)
      PREI = EXP(-AA*ALFA*PCSQ/(AA+ALFA))
      IF(NLPR(LTERM).EQ.2) THEN
C      R TO THE ZERO POWER.
       TT = ONE/(AA+ALFA)
       T  =  SQRT(TT)
       X0 = (AAX+ALFA*CX)*TT
       Y0 = (AAY+ALFA*CY)*TT
       Z0 = (AAZ+ALFA*CZ)*TT
       IN = -4
       DO 206 I = 1,LIT
       IN = IN+4
       NI = I
       DO 206 J = 1,LJT
       JN = IN+J
       NJ = J
       CALL STVINT
       XIN(JN   ) = XINT*T
       YIN(JN   ) = YINT*T
       ZIN(JN   ) = ZINT*T
      CALL POLXYZ
      XIN(JN+125) = XINT*T
      YIN(JN+125) = YINT*T
      ZIN(JN+125) = ZINT*T
  206  CONTINUE
       DO 266 I = 1,IJ
       NX    = IJX(I)
       NY    = IJY(I)
       NZ    = IJZ(I)
      DUMX= XIN(NX+125)*YIN(NY    )*ZIN(NZ    )
      DUMY= XIN(NX    )*YIN(NY+125)*ZIN(NZ    )
      DUMZ= XIN(NX    )*YIN(NY    )*ZIN(NZ+125)
      DUM = TWO*DIJ(I)*ALFA*PREI*BETA
      CHDINT(ICC)=CHDINT(ICC)+DUM*DUMX
C     WRITE(6,*)'IC,LTERM,I,ICC',IC,LTERM,I,ICC
      CHDINT(ICC+1)=CHDINT(ICC+1)+DUM*DUMY
      CHDINT(ICC+2)=CHDINT(ICC+2)+DUM*DUMZ
      ICC=ICC+3
 266   CONTINUE
      ELSE
C      ONE OVER R.
       DUM = PI212/(AA+ALFA)
       DO 385 I = 1,IJ
       GIJ(I) = DIJ(I)*DUM*TWO
  385  CONTINUE
       XX=AA*AA*PCSQ/(AA+ALFA)
       IF(NROOTS.LE.3) CALL RT123
       IF(NROOTS.EQ.4) CALL ROOT4
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
       WW = WW*(UU+ALFA)
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
  405  CONTINUE
  425  MM = MM+16
       DO 403 I = 1,IJ
       NX    = IJX(I)
       NY    = IJY(I)
       NZ    = IJZ(I)
       MM    = 0
      DUMX = ZERO
      DUMY = ZERO
      DUMZ = ZERO
      DO 402 K = 1,NROOTS
      DUMX= DUMX+XIN(NX+MM+125)*YIN(NY+MM    )*ZIN(NZ+MM    )
      DUMY= DUMY+XIN(NX+MM    )*YIN(NY+MM+125)*ZIN(NZ+MM    )
      DUMZ= DUMZ+XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+125)
  402 MM  = MM+16
      DUM = GIJ(I)*PREI*BETA
      CHDINT(ICC)=CHDINT(ICC)+DUM*DUMX
      CHDINT(ICC+1)=CHDINT(ICC+1)+DUM*DUMY
      CHDINT(ICC+2)=CHDINT(ICC+2)+DUM*DUMZ
      ICC=ICC+3
 403  CONTINUE
      END IF
C
 480  CONTINUE
C-------- END OF REP CENTERS LOOP
 500  CONTINUE
 520  CONTINUE
C
C     ----- END OF *PRIMITIVE* LOOPS -----
C
C     ----- SET DEF MATRIX
C
      MAX=MAXJ
      NN=0
      ICC=1
C
      DO 551 IC = 1,NREP
      ICF=IC+NMTTPT+NPTTPT
      DO 551 LTERM = KFR(IC),KLR(IC)
      DO 550 I=MINI,MAXI
      LI=LOCI+I
      DO 550 J=MINJ,MAX
      LJ=LOCJ+J
      IF (LI-LJ) 920,940,940
  920 ID = LJ
      JD = LI
      GO TO 960
  940 ID = LI
      JD = LJ
  960 NN = (ID*(ID-1))/2+JD
      DUM = DM(NN)
      DEF(1,ICF)=DEF(1,ICF) + DUM*CHDINT(ICC)
C     WRITE(6,6565)IC,LTERM,I,J,ICF,ICC
C6565 FORMAT('IC,LTERM,I,J,ICF,ICC',6I4)
      DEF(2,ICF)=DEF(2,ICF) + DUM*CHDINT(ICC+1)
      DEF(3,ICF)=DEF(3,ICF) + DUM*CHDINT(ICC+2)
      ICC=ICC+3
  550 CONTINUE
  551 CONTINUE
C
  580 CONTINUE
  600 CONTINUE
C
      IF(GOPARR .AND. NXT) CALL DDI_DLBRESET
      RETURN
 9008 FORMAT(/' NUMBER OF POLYNOMIAL ROOTS NEEDED (NROOTS) IS GREATER',
     *        ' THAN 6 IN REPINT.  CALL A PROGRAMMER/QUANTUM CHEMIST.')
      END
C*MODULE EFGRDC  *DECK EFOUT
      SUBROUTINE EFOUT(EF3,EF3T,EF3TA)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,MASWRK,DSKWRK
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
      PARAMETER (MXPSH=5*MXPT, MXPG=5*MXPSH)
      PARAMETER (BOHR = 0.52917724924D+00, HARTREE = 6.275095D+02)
C
      DIMENSION EF3(3,MXFGPT),EF3T(3,MXFRG),EF3TA(3)
      DIMENSION DNAM(3)
C
      COMMON /EFPOTD/ METHOD,INABIO,MOVE
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PAULIN/ EX(MXPG,MXFRG),CS(MXPG,MXFRG),CP(MXPG,MXFRG),
     *                CD(MXPG,MXFRG),CF(MXPG,MXFRG),CG(MXPG,MXFRG),
     *                PRNAME(MXFGPT),PRCORD(3,MXFGPT),EFZNUC(MXFGPT),
     *                KSTART(MXPSH,MXFRG),KATOM(MXPSH,MXFRG),
     *                KTYPE(MXPSH,MXFRG),KNG(MXPSH,MXFRG),
     *                KLOC(MXPSH,MXFRG), KMIN(MXPSH,MXFRG),
     *                KMAX(MXPSH,MXFRG),NSHELL(MXFRG),NGAUSS(MXFRG),
     *                NATEF(MXFRG),NUMEF(MXFRG),NTPATM
      COMMON /PAULMO/ CENTNM(MXFGPT),CENTCD(3,MXFGPT),NORB(MXFRG),
     *                NPBF(MXFRG),NTMO
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DNAM /4HE'X ,4HE'Y ,4HE'Z /
#else
      CHARACTER*8 :: DNAM_STR(3)
      EQUIVALENCE (DNAM, DNAM_STR)
      DATA DNAM_STR/"E'X ","E'Y ","E'Z "/
#endif
C
C     ----- PRINT ENERGY GRADIENT VECTOR -----
C     PRINT GRADIENT OF ELECTROSTATIC TERM FOR CLASSICAL POTENTIAL RUNS
C
      IF (METHOD .GE. 4) THEN
         AUFACT = HARTREE/BOHR
         WRITE(IW, 8000)
         DO 50 INF = 1, NFRG
            WRITE (IW, 8010) INF
            WRITE (IW, 8020) (EF3T(J, INF)*AUFACT, J = 1, 3)
            WRITE (IW, 8030) (TORQ(J, INF)*AUFACT, J = 1, 3)
   50    CONTINUE
C         WRITE (IW, 8040) (EF3TA(J)*AUFACT, J=1,3)
         RETURN
      END IF
C
      IF (.NOT. MASWRK) RETURN
      DO 700 INF=1,NFRG
         II=1
         WRITE (IW,9070) INF
         WRITE (IW,9000) (DNAM(J),J=1,3)
         INB=INF
         INAM = 1
C  POSITION POINTER II
         IF (INB.GT.1) THEN
   77       INB=INB-1
            II=II+NMPTS(INB)
            INAM = INAM + NMPTS(INB)
            IF (INB.GT.1) GO TO 77
         END IF
         DO 200 I=1,NMPTS(INF)
            II=II+1
            INAM = INAM + 1
  200    CONTINUE
         INB=INF
         IF (INB.LT.NFRG) THEN
  177       INB=INB+1
            II=II+NMPTS(INB)
            IF(INB.LT.NFRG)GO TO 177
         END IF
      INB=INF
      INAM = 1
      IF(INB.GT.1) THEN
 78    INB=INB-1
       II=II+NPPTS(INB)
       INAM = NPPTS(INB)
       IF(INB.GT.1)GO TO 78
      END IF
      DO 210 I=1,NPPTS(INF)
         II=II+1
         INAM = INAM + 1
  210 CONTINUE
         INB=INF
        IF(INB.LT.NFRG) THEN
 178     INB=INB+1
         II=II+NPPTS(INB)
         IF(INB.LT.NFRG)GO TO 178
        END IF
      INB=INF
      INAM = 1
      IF(INB.GT.1) THEN
 79    INB=INB-1
       II=II+NRPTS(INB)
       INAM = INAM + NRPTS(INB)
       IF(INB.GT.1)GO TO 79
      END IF
      DO 220 I=1,NRPTS(INF)
         II=II+1
         INAM = INAM + 1
  220 CONTINUE
         II = NMTTPT + NPTTPT + NRTTPT
         INB = INF
         INAM1 = 0
         INAM2 = 0
         IF (INB.GT.1) THEN
   80       INB = INB - 1
            II = II + NATEF(INB) + NORB(INB)
            INAM1 = NATEF(INB)
            INAM2 = NORB(INB)
            IF (INB.GT.1) GO TO 80
         END IF
         DO 250 I=1,NATEF(INF)
            II = II+1
            INAM1 = INAM1 + 1
            WRITE (IW,9010) INAM1,PRNAME(INAM1),(EF3(J,II),J=1,3)
  250    CONTINUE
         DO 260 I=1,NORB(INF)
            II = II+1
            INAM2 = INAM2 + 1
  260    CONTINUE
C
         WRITE (IW,9020) (EF3T(J,INF),J=1,3)
         WRITE (IW,9030) (TORQ(J,INF),J=1,3)
C        WRITE (IW,9031) (ATORQ(J,INF),J=1,3)
         WRITE (IW,9035) (EFCENT(J,INF),J=1,3)
 700  CONTINUE
         WRITE(IW,9080)
         WRITE (IW,9040) (EF3TA(J),J=1,3)
      RETURN
C
 8000 FORMAT (/,1X,'GRADIENTS (KCAL/MOL*ANG) WRT. ELECTROSTATIC',
     *         ' TERM...')
 8010 FORMAT (/,1X,'FRAGMENT: ', I3,/
     *          25X,'   DE/DX   ',5X,'   DE/DY   ',5X,'   DE/DZ   ',/
     *          25X,'-----------',5X,'-----------',5X,'-----------')
 8020 FORMAT (5X,'TRANSLATIONAL :     ',F11.7,5X,F11.7,5X,F11.7)
 8030 FORMAT (5X,'   ROTATIONAL :     ',F11.7,5X,F11.7,5X,F11.7)
C 8040 FORMAT (/,25X,'-----------',5X,'-----------',5X,'-----------',/
C     *        5X,'TRANS. + ROT. :     ',F11.7,5X,F11.7,5X,F11.7)
 9000 FORMAT(/6X,'FRAGMENT POINT',7X,A4,14X,A4,14X,A4)
 9010 FORMAT(1X,I4,1X,A8,2X,3F18.9)
 9020 FORMAT(/6X,'TOTALS',4X,3F18.9)
 9030 FORMAT(6X,'TORQUE',4X,3F18.9)
C9031 FORMAT(2X,'AUX TORQUE',4X,3F18.9)
 9035 FORMAT(6X,'EFCENT',4X,3F18.9)
 9040 FORMAT(6X,'FORCE ',4X,3F18.9)
 9070 FORMAT(//2X,'FOR FRAGMENT NUMBER ',I3)
 9080 FORMAT(/,6X,'TOTAL FORCE ON ALL FRAGMENTS')
      END
C*MODULE EFGRDC  *DECK EFFT
      SUBROUTINE EFFT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 REPNAM,FRGNME,POLNAM
C
      LOGICAL GOPARR,MASWRK,DSKWRK
C
      DIMENSION DEFTA(3)
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
      PARAMETER (MXPSH=5*MXPT, MXPG=5*MXPSH)
C
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /EFPPAR/ EFP(3,MXFGPT),EFPOL(9,MXFGPT),
     *                ENO,DIND(3,MXFGPT),DINDD(3,MXFGPT),POLNAM(MXFGPT)
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /INTFRG/ EFF(3,MXFRG)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PAULIN/ EX(MXPG,MXFRG),CS(MXPG,MXFRG),CP(MXPG,MXFRG),
     *                CD(MXPG,MXFRG),CF(MXPG,MXFRG),CG(MXPG,MXFRG),
     *                PRNAME(MXFGPT),PRCORD(3,MXFGPT),EFZNUC(MXFGPT),
     *                KSTART(MXPSH,MXFRG),KATOM(MXPSH,MXFRG),
     *                KTYPE(MXPSH,MXFRG),KNG(MXPSH,MXFRG),
     *                KLOC(MXPSH,MXFRG), KMIN(MXPSH,MXFRG),
     *                KMAX(MXPSH,MXFRG),NSHELL(MXFRG),NGAUSS(MXFRG),
     *                NATEF(MXFRG),NUMEF(MXFRG),NTPATM
C
      COMMON /PAULMO/ CENTNM(MXFGPT),CENTCD(3,MXFGPT),NORB(MXFRG),
     *                NPBF(MXFRG),NTMO
      COMMON /REPPAR/ CREP(3,MXFGPT),CLPR(4*MXFGPT),ZLPR(4*MXFGPT),
     *                NLPR(4*MXFGPT),KFR(MXFGPT),KLR(MXFGPT),
     *                REPNAM(MXFGPT)
      COMMON /SIMDAT/ NACC,NREJ,IGOMIN,NRPA,IBWM,NACCT,NREJT,NRPAT,
     *                NPRTGO,IDPUNC,IGOFLG
      COMMON /ZMTALT/ NZMAT2,NZVAR2,NVAR2,NZMTRD,ICOORD
C
      DATA ZERO/0.0D+00/
C
      DO 454 IM=1,NFRG
      DO 454 J=1,3
         DEFT(J,IM)=ZERO
         TORQ(J,IM)=ZERO
 454  CONTINUE
      DO 455 J=1,3
         DEFTA(J)=ZERO
 455  CONTINUE
C GET TORQUE ON EACH FRAGMENT
C      WRITE(6,*)'IM,II,III,DEF(2,II),EFC(1,III),
C     $ EFCENT(1,INF),TORQ(3,INF)'
      II=0
      III=0
      DO 550 INF=1,NFRG
       DO 540 IM=1,NMPTS(INF)
        II=II+1
        III=III+1
        TORQ(1,INF)=TORQ(1,INF)+DEF(3,II)*(EFC(2,III)-EFCENT(2,INF))
     $      -DEF(2,II)*(EFC(3,III)-EFCENT(3,INF))
        TORQ(2,INF)=TORQ(2,INF)+DEF(1,II)*(EFC(3,III)-EFCENT(3,INF))
     $      -DEF(3,II)*(EFC(1,III)-EFCENT(1,INF))
        TORQ(3,INF)=TORQ(3,INF)+DEF(2,II)*(EFC(1,III)-EFCENT(1,INF))
     $      -DEF(1,II)*(EFC(2,III)-EFCENT(2,INF))
        DEFT(1,INF)=DEFT(1,INF)+DEF(1,II)
        DEFT(2,INF)=DEFT(2,INF)+DEF(2,II)
        DEFT(3,INF)=DEFT(3,INF)+DEF(3,II)
 540   CONTINUE
 550  CONTINUE
C      WRITE(6,*)'IPP,II,III,DEF(2,II),EFP(1,III),
C     $ EFCENT(1,INF),TORQ(3,INF)'
      III=0
      DO 551 INF=1,NFRG
       DO 541 IPP=1,NPPTS(INF)
        II=II+1
        III=III+1
        TORQ(1,INF)=TORQ(1,INF)+DEF(3,II)*(EFP(2,III)-EFCENT(2,INF))
     $      -DEF(2,II)*(EFP(3,III)-EFCENT(3,INF))
        TORQ(2,INF)=TORQ(2,INF)+DEF(1,II)*(EFP(3,III)-EFCENT(3,INF))
     $      -DEF(3,II)*(EFP(1,III)-EFCENT(1,INF))
        TORQ(3,INF)=TORQ(3,INF)+DEF(2,II)*(EFP(1,III)-EFCENT(1,INF))
     $      -DEF(1,II)*(EFP(2,III)-EFCENT(2,INF))
        DEFT(1,INF)=DEFT(1,INF)+DEF(1,II)
        DEFT(2,INF)=DEFT(2,INF)+DEF(2,II)
        DEFT(3,INF)=DEFT(3,INF)+DEF(3,II)
 541   CONTINUE
 551  CONTINUE
C      WRITE(6,*)'IRP,II,III,DEF(2,II),CREP(1,III),
C     $ EFCENT(1,INF),TORQ(3,INF)'
      III=0
      DO 552 INF=1,NFRG
       DO 542 IRP=1,NRPTS(INF)
        II=II+1
        III=III+1
        TORQ(1,INF)=TORQ(1,INF)+DEF(3,II)*(CREP(2,III)-EFCENT(2,INF))
     $      -DEF(2,II)*(CREP(3,III)-EFCENT(3,INF))
        TORQ(2,INF)=TORQ(2,INF)+DEF(1,II)*(CREP(3,III)-EFCENT(3,INF))
     $      -DEF(3,II)*(CREP(1,III)-EFCENT(1,INF))
        TORQ(3,INF)=TORQ(3,INF)+DEF(2,II)*(CREP(1,III)-EFCENT(1,INF))
     $      -DEF(1,II)*(CREP(2,III)-EFCENT(2,INF))
        DEFT(1,INF)=DEFT(1,INF)+DEF(1,II)
        DEFT(2,INF)=DEFT(2,INF)+DEF(2,II)
        DEFT(3,INF)=DEFT(3,INF)+DEF(3,II)
 542   CONTINUE
        DEFT(1,INF)=DEFT(1,INF)+EFF(1,INF)
        DEFT(2,INF)=DEFT(2,INF)+EFF(2,INF)
        DEFT(3,INF)=DEFT(3,INF)+EFF(3,INF)
 552   CONTINUE
C
      III1 = 0
      III2 = 0
      DO INF = 1,NFRG
         DO INATM = 1,NATEF(INF)
            II = II + 1
            III1 = III1 + 1
        TORQ(1,INF)=TORQ(1,INF)+DEF(3,II)*(PRCORD(2,III1)-EFCENT(2,INF))
     $      -DEF(2,II)*(PRCORD(3,III1)-EFCENT(3,INF))
        TORQ(2,INF)=TORQ(2,INF)+DEF(1,II)*(PRCORD(3,III1)-EFCENT(3,INF))
     $      -DEF(3,II)*(PRCORD(1,III1)-EFCENT(1,INF))
        TORQ(3,INF)=TORQ(3,INF)+DEF(2,II)*(PRCORD(1,III1)-EFCENT(1,INF))
     $      -DEF(1,II)*(PRCORD(2,III1)-EFCENT(2,INF))
        DEFT(1,INF)=DEFT(1,INF)+DEF(1,II)
        DEFT(2,INF)=DEFT(2,INF)+DEF(2,II)
        DEFT(3,INF)=DEFT(3,INF)+DEF(3,II)
         END DO
         DO INORB = 1,NORB(INF)
            II = II + 1
            III2 = III2 + 1
        TORQ(1,INF)=TORQ(1,INF)+DEF(3,II)*(CENTCD(2,III2)-EFCENT(2,INF))
     $      -DEF(2,II)*(CENTCD(3,III2)-EFCENT(3,INF))
        TORQ(2,INF)=TORQ(2,INF)+DEF(1,II)*(CENTCD(3,III2)-EFCENT(3,INF))
     $      -DEF(3,II)*(CENTCD(1,III2)-EFCENT(1,INF))
        TORQ(3,INF)=TORQ(3,INF)+DEF(2,II)*(CENTCD(1,III2)-EFCENT(1,INF))
     $      -DEF(1,II)*(CENTCD(2,III2)-EFCENT(2,INF))
        DEFT(1,INF)=DEFT(1,INF)+DEF(1,II)
        DEFT(2,INF)=DEFT(2,INF)+DEF(2,II)
        DEFT(3,INF)=DEFT(3,INF)+DEF(3,II)
         END DO
      END DO
C
C
      DO 560 INF=1,NFRG
      DO 555 J=1,3
      DEFTA(J)=DEFTA(J)+DEFT(J,INF)
 555  CONTINUE
 560  CONTINUE
      DO 561 INF=1,NFRG
      DO 556 J=1,3
      ATORQ(J,INF)=ATORQ(J,INF)+TORQ(J,INF)
 556  CONTINUE
 561  CONTINUE
      DO 661 INF=1,NFRG
      DO 661 J=1,3
      TORQ(J,INF)=ATORQ(J,INF)
 661  CONTINUE
C
      IF(MASWRK .AND.  NPRTGO.NE.2) THEN
         IF (ICOORD .EQ. 4) WRITE (IW,8000)
         WRITE(IW,*)'NOTE: GRADIENT WRT. FROZEN FRAGMENT'
         CALL EFOUT(DEF,DEFT,DEFTA)
      END IF
      RETURN
C
 8000 FORMAT(/,21X,29('-'),
     *       /,21X,'FRAGMENT GRADIENT INFORMATION',
     *       /,21X,29('-'),
     *       /,' ')
      END
C*MODULE EFGRDC  *DECK EFCM
      SUBROUTINE EFCM
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
      COMMON /FRGMSS/ FPMASS(MXPT),FMASS(MXPT,MXFRG),
     *                FPNUC(MXPT),FGNUC(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      DATA ZERO/0.0D+00/
C
      JJ=0
      JM=0
      DO 101 INF=1,NFRG
       FRGMAS(INF)=ZERO
       DO 100 J=1,3
         EFCENT(J,INF)=ZERO
 100   CONTINUE
       DO 50 I=1,NMPTS(INF)
        FRGMAS(INF)=FRGMAS(INF)+FMASS(I,INF)
  50   CONTINUE
       DO 92 IZ=1,NMPTS(INF)
       JJ=JJ+1
       DO 91 J=1,3
       EFCENT(J,INF)=EFCENT(J,INF)+EFC(J,JJ)*FMASS(IZ,INF)
 91    CONTINUE
 92    CONTINUE
C       WRITE(6,*)'FRGMAS(INF)=',FRGMAS(INF)
       DO 95 J=1,3
        EFCENT(J,INF) = EFCENT(J,INF)/FRGMAS(INF)
C       WRITE(6,*)'INF,J,EFCENT(J,INF)',INF,J,EFCENT(J,INF)
C
C      TO TEMPORARILY USE THE SAME POINT OF ROTATION FOR ALL FRAGMENTS,
C      UNCOMMENT THE NEXT LINE.
C      EFCENT(J,INF)=EFCENT(J,1)
C
 95     CONTINUE
C ALSO CALCULATE MOMENTS OF INERTIA
      DO 54 J=1,6
         FRGMI(J,INF)=ZERO
 54   CONTINUE
      DO 55 IZ=1,NMPTS(INF)
         JM=JM+1
         XX = (EFC(1,JM)-EFCENT(1,INF))**2
         XY = (EFC(1,JM)-EFCENT(1,INF))*(EFC(2,JM)-EFCENT(2,INF))
         YY = (EFC(2,JM)-EFCENT(2,INF))**2
         XZ = (EFC(1,JM)-EFCENT(1,INF))*(EFC(3,JM)-EFCENT(3,INF))
         YZ = (EFC(2,JM)-EFCENT(2,INF))*(EFC(3,JM)-EFCENT(3,INF))
         ZZ = (EFC(3,JM)-EFCENT(3,INF))**2
         FRGMI(1,INF)=FRGMI(1,INF)+FMASS(IZ,INF)*(YY+ZZ)
         FRGMI(2,INF)=FRGMI(2,INF)-FMASS(IZ,INF)*XY
         FRGMI(3,INF)=FRGMI(3,INF)+FMASS(IZ,INF)*(XX+ZZ)
         FRGMI(4,INF)=FRGMI(4,INF)-FMASS(IZ,INF)*XZ
         FRGMI(5,INF)=FRGMI(5,INF)-FMASS(IZ,INF)*YZ
         FRGMI(6,INF)=FRGMI(6,INF)+FMASS(IZ,INF)*(YY+XX)
 55    CONTINUE
 101  CONTINUE
      RETURN
      END
C*MODULE EFGRDC  *DECK EFTORD
      SUBROUTINE EFTORD(DM,CHDINT,L4)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL NORM,GOPARR,DSKWRK,MASWRK,NXT
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      PARAMETER (MXSH=1000, MXGTOT=5000)
      PARAMETER (MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION DM(*),CHDINT(L4)
C
      DIMENSION DIJ(100),XIN(432),YIN(432),ZIN(432)
      DIMENSION FIJ(100),GIJ(100)
      DIMENSION IX(20),IY(20),IZ(20),JX(20),JY(20),JZ(20)
      DIMENSION IJX(100),IJY(100),IJZ(100)
C
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG)
     *                ,EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG)
     *                ,ATORQ(3,MXFRG)
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
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
C
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
      DATA PI212 /1.1283791670955D+00/
      DATA SQRT3 /1.73205080756888D+00/
      DATA SQRT5 /2.23606797749979D+00/
      DATA ZERO,ONE/0.0D+00,1.0D+00/
      DATA RLN10 /2.30258D+00/
      DATA QLIM /1.0D-07/
C
      NEFD = NMTTPT
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C     INITIALIZATION FOR PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      LCNT = -1
C
C     ----- ISHELL
C
      DO 600 II = 1,NSHELL
C
C           GO PARALLEL!
C
      IF(NXT .AND. GOPARR) THEN
         LCNT = LCNT + 1
         IF(LCNT.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
         IF(NEXT.NE.LCNT) GO TO 600
      END IF
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
C     WRITE(6,5432)II,IAT,I1,I2,LIT,MINI,MAXI,LOCI
C5432 FORMAT(8I6)
C
C     ----- JSHELL
C
      DO 580 JJ = 1,NSHELL
C
C           GO PARALLEL!
C
        IF((.NOT.NXT) .AND. GOPARR) THEN
           IPCOUNT = IPCOUNT + 1
           IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 580
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
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      IJ = 0
      MAX = MAXJ
      DO 50 I = MINI,MAXI
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
      DO 50 J = MINJ,MAX
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
C     WRITE(6,*)'I,J,IJ ',I,J,IJ
  50  CONTINUE
      ICC=1
      DO 60 IC = 1,NEFD
      DO 60 I=1,IJ
      CHDINT(ICC) = ZERO
      ICC=ICC+1
      CHDINT(ICC) = ZERO
      ICC=ICC+1
      CHDINT(ICC) = ZERO
      ICC=ICC+1
  60  CONTINUE
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
C
C     ----- J PRIMITIVE
C
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
      CFJ = CF(JG)
      AX = (AXI+AJ*XJ)*AA1
      AY = (AYI+AJ*YJ)*AA1
      AZ = (AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR
C
      MAX = MAXJ
      NN = 0
      DUM1=ZERO
      DUM2=DUM1
      DO 170 I=MINI,MAXI
      GO TO (70,80,110,110,90,110,110,95,110,110,
     1       102,110,110,104,110,110,110,110,110,108),I
  70  DUM1=CSI*FAC
      GO TO 110
  80  DUM1=CPI*FAC
      GO TO 110
  90  DUM1=CDI*FAC
      GO TO 110
  95  IF(NORM) DUM1=DUM1*SQRT3
      GO TO 110
 102  DUM1=CFI*FAC
      GO TO 110
 104  DUM1 = DUM1 *SQRT5
      GO TO 110
 108  DUM1 = DUM1 * SQRT3
 110  CONTINUE
C     IF(II.EQ.6)WRITE(6,*)'I,DUM1',I,DUM1
      DO 170 J=MINJ,MAX
      GO TO (125,130,160,160,140,160,160,150,160,160,
     1       152,160,160,154,160,160,160,160,160,156),J
  125 DUM2=DUM1*CSJ
      GO TO 160
  130 DUM2=DUM1*CPJ
      GO TO 160
  140 DUM2=DUM1*CDJ
      GO TO 160
  150 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 160
  152 DUM2 = DUM1 * CFJ
      GO TO 160
  154 DUM2 = DUM2 *SQRT5
      GO TO 160
  156 DUM2 = DUM2 * SQRT3
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
      ICC=1
C     WRITE(6,*)'IG,JG,IC,I,ICC,DUM,DUM(X,Y,Z),CHDINT(ICC)'
C     WRITE(6,*)'IG,JG,I,ICC,DUM,DUMX,CHDINT(ICC)'
      DO 480 IC = 1,NEFD
      IF(.NOT.DODIPO(IC))GO TO 480
      ALFA = EFATRM(IC)
      BETA = EFBTRM(IC)
      CX   = EFC(1,IC)
      CY   = EFC(2,IC)
      CZ   = EFC(3,IC)
      DUMA = PI212/(AA+ALFA)
      DUMA=DUMA+DUMA
C     ZNUC = EFCHG(1,IC)+EFCHG(2,IC)
      DO 385 I = 1,IJ
      GIJ(I) = DIJ(I)*DUMA
  385 CONTINUE
      XX   = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
      IF(NROOTS.LE.3) CALL RT123
      IF(NROOTS.EQ.4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         WRITE(IW,9008)
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
      ICCT=ICC
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
      CHDINT(ICC)=CHDINT(ICC)-DUM*DUMX
      ICC=ICC+1
      CHDINT(ICC)=CHDINT(ICC)-DUM*DUMY
      ICC=ICC+1
      CHDINT(ICC)=CHDINT(ICC)-DUM*DUMZ
      ICC=ICC+1
 403  CONTINUE
C
C  SUBTRACT DAMPING FUNCTION TERM
C
      IF ( ABS(ALFA).LE.QLIM ) GO TO 480
C     ZNUC = EFCHG(1,IC)
      PCSQ = (AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2
      XX=AA*AA*PCSQ/(AA+ALFA)
      PREI = EXP(-AA*ALFA*PCSQ/(AA+ALFA))
      IF(NROOTS.LE.3) CALL RT123
      IF(NROOTS.EQ.4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         WRITE(IW,9008)
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
      ICC=ICCT
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
      CHDINT(ICC)=CHDINT(ICC)-DUM*DUMX
      ICC=ICC+1
      CHDINT(ICC)=CHDINT(ICC)-DUM*DUMY
      ICC=ICC+1
      CHDINT(ICC)=CHDINT(ICC)-DUM*DUMZ
      ICC=ICC+1
 465  CONTINUE
C
 480  CONTINUE
 500  CONTINUE
 520  CONTINUE
C
C     ----- END OF *PRIMITIVE* LOOPS -----
C
      MAX=MAXJ
      ICC=1
      IC=0
C     DO 551 IC = 1,NEFD
      DO 551 INF = 1,NFRG
      DO 551 INP = 1,NMPTS(INF)
      IC=IC+1
      IF(.NOT.DODIPO(IC))GO TO 551
      DO 550 I=MINI,MAXI
      LI=LOCI+I
      DO 550 J=MINJ,MAX
      LJ=LOCJ+J
      IF (LI-LJ) 920,940,940
  920 ID = LJ
      JD = LI
      GO TO 960
  940 ID = LI
      JD = LJ
  960 NN = (ID*(ID-1))/2+JD
      DUM = DM(NN)
      ATORQ(1,INF)=ATORQ(1,INF)-DUM*(CHDINT(ICC+1)*EFDIP(3,IC)
     $           -CHDINT(ICC+2)*EFDIP(2,IC))
      ICC=ICC+1
      ATORQ(2,INF)=ATORQ(2,INF)-DUM*(CHDINT(ICC+1)*EFDIP(1,IC)
     $           -CHDINT(ICC-1)*EFDIP(3,IC))
      ICC=ICC+1
      ATORQ(3,INF)=ATORQ(3,INF)-DUM*(CHDINT(ICC-2)*EFDIP(2,IC)
     $           -CHDINT(ICC-1)*EFDIP(1,IC))
      ICC=ICC+1
  550 CONTINUE
  551 CONTINUE
C-------- END OF EFD CENTERS LOOP
C
  580 CONTINUE
  600 CONTINUE
C
      IF(GOPARR .AND. NXT) CALL DDI_DLBRESET
      RETURN
 9008 FORMAT(/' NUMBER OF POLYNOMIAL ROOTS NEEDED (NROOTS) IS GREATER',
     *        ' THAN 6 IN EFTORD.  CALL A PROGRAMMER/QUANTUM CHEMIST.')
      END
C*MODULE EFGRDC  *DECK EFQTOR
      SUBROUTINE EFQTOR(DM,CHOINT,L4)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
      COMMON /FMCOM / XX(1)
C
      CALL VALFM(LOADFM)
      NEED=6*MXFGPT
      CALL GETFM(NEED)
      CALL EFQTO(DM,CHOINT,L4,XX(LOADFM+1))
      CALL RETFM(NEED)
C
      RETURN
      END
C*MODULE EFGRDC  *DECK EFQTO
      SUBROUTINE EFQTO(DM,CHQINT,L4,QUAD)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL NORM,GOPARR,DSKWRK,MASWRK,NXT
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      PARAMETER (MXSH=1000, MXGTOT=5000)
      PARAMETER (MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION DM(*),CHQINT(L4)
C
      DIMENSION QUAD(6,*)
      DIMENSION DIJ(100),XIN(432),YIN(432),ZIN(432)
      DIMENSION FIJ(100),GIJ(100)
      DIMENSION IX(20),IY(20),IZ(20),JX(20),JY(20),JZ(20)
      DIMENSION IJX(100),IJY(100),IJZ(100),W4(5)
C
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG)
     *                ,EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG)
     *                ,ATORQ(3,MXFRG)
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
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
C
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
      DATA PI212 /1.1283791670955D+00/
      DATA SQRT3 /1.73205080756888D+00/
      DATA SQRT5 /2.23606797749979D+00/
      DATA ZERO,ONE,TWO,FOUR/0.0D+00,1.0D+00,2.0D+00,4.0D+00/
      DATA PT5,ONEPT5,THREE /0.5D+00,1.5D+00,3.0D+00/
      DATA RLN10 /2.30258D+00/
      DATA QLIM /1.0D-07/
C
      NEFQ = NMTTPT
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C     INITIALIZATION FOR PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      LCNT = -1
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
C
C           GO PARALLEL!
C
      IF(NXT .AND. GOPARR) THEN
         LCNT = LCNT + 1
         IF(LCNT.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
         IF(NEXT.NE.LCNT) GO TO 600
      END IF
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
      DO 580 JJ = 1,NSHELL
C
C           GO PARALLEL!
C
        IF((.NOT.NXT) .AND. GOPARR) THEN
           IPCOUNT = IPCOUNT + 1
           IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 580
        END IF
C
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
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      IJ = 0
      MAX = MAXJ
      DO 30 I = MINI,MAXI
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
      DO 30 J = MINJ,MAX
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
  30  CONTINUE
      ICC=1
      DO 60 IC=1,NEFQ
      DO 60 I=1,IJ
      CHQINT(ICC) = ZERO
      ICC=ICC+1
      CHQINT(ICC) = ZERO
      ICC=ICC+1
      CHQINT(ICC) = ZERO
      ICC=ICC+1
  60  CONTINUE
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
C     ----- J PRIMITIVE
C
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
      MAX = MAXJ
      NN = 0
      DUM1=ZERO
      DUM2=DUM1
      DO 170 I=MINI,MAXI
      GO TO (70,80,110,110,90,110,110,95,110,110,
     1       102,110,110,104,110,110,110,110,110,108),I
  70  DUM1=CSI*FAC
      GO TO 110
  80  DUM1=CPI*FAC
      GO TO 110
  90  DUM1=CDI*FAC
      GO TO 110
  95  IF(NORM) DUM1=DUM1*SQRT3
      GO TO 110
 102  DUM1=CFI*FAC
      GO TO 110
 104  DUM1 = DUM1 *SQRT5
      GO TO 110
 108  DUM1 = DUM1 * SQRT3
 110  CONTINUE
      DO 170 J=MINJ,MAX
      GO TO (125,130,160,160,140,160,160,150,160,160,
     1       152,160,160,154,160,160,160,160,160,156),J
C
  125 DUM2=DUM1*CSJ
      GO TO 160
  130 DUM2=DUM1*CPJ
      GO TO 160
  140 DUM2=DUM1*CDJ
      GO TO 160
  150 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 160
  152 DUM2 = DUM1 * CFJ
      GO TO 160
  154 DUM2 = DUM2 *SQRT5
      GO TO 160
  156 DUM2 = DUM2 * SQRT3
  160 NN=NN+1
  170 DIJ(NN)=DUM2
C
C     ..... HELLMANN-FEYNMAN TERM .....
C
C     DUM = PI212*AA1/THREE
C     DUM = PI212*AA1
      DUM = PI212*AA1*TWO/THREE
      DO 380 I = 1,IJ
      FIJ(I) = DIJ(I)*DUM
  380 CONTINUE
      AAX = AA*AX
      AAY = AA*AY
      AAZ = AA*AZ
C
      ICC=1
      DO 480 IC = 1,NEFQ
      IF(.NOT.DOQUAD(IC))GO TO 480
      ALFA = EFATRM(IC)
      BETA = EFBTRM(IC)
      CX   = EFC(1,IC)
      CY   = EFC(2,IC)
      CZ   = EFC(3,IC)
C     DUMA = PI212/(THREE*(AA+ALFA))
C     DUMA = PI212/(AA+ALFA)
      DUMA = PI212/(AA+ALFA)*TWO/THREE
      DO 385 I = 1,IJ
      GIJ(I) = DIJ(I)*DUMA
  385 CONTINUE
      XX   = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
      IF(NROOTS.LE.3) CALL RT123
      IF(NROOTS.EQ.4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         WRITE(IW,9008)
         CALL ABRT
      END IF
      MM = 0
      DO 401 K = 1,NROOTS
      U2 = AA*U(K)
      U4 = U2*U2
      WW = W(K)
C     W2(K) = TWO *WW*U2
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
      ICCT=ICC
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
      DUMXX= DUMXX-XIN(NX+MM+288)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)
      DUMYY= DUMYY-XIN(NX+MM    )*YIN(NY+MM+288)*ZIN(NZ+MM    )*W4(K)
      DUMZZ= DUMZZ-XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+288)*W4(K)
      DUMXY= DUMXY-XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM    )*W4(K)
      DUMXZ= DUMXZ-XIN(NX+MM+144)*YIN(NY+MM    )*ZIN(NZ+MM+144)*W4(K)
      DUMYZ= DUMYZ-XIN(NX+MM    )*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W4(K)
  402 MM  = MM+16
      DUM = FIJ(I)
      CHQINT(ICC)=CHQINT(ICC)+DUM*(DUMYZ*(QUAD(2,IC)-QUAD(3,IC))
     1       +(DUMZZ-DUMYY)*QUAD(6,IC)
     2       +QUAD(4,IC)*DUMXZ-QUAD(5,IC)*DUMXY)
      ICC=ICC+1
      CHQINT(ICC)=CHQINT(ICC)+DUM*(DUMXZ*(QUAD(3,IC)-QUAD(1,IC))
     1       +(DUMXX-DUMZZ)*QUAD(5,IC)
     2       +QUAD(6,IC)*DUMXY-QUAD(4,IC)*DUMYZ)
      ICC=ICC+1
      CHQINT(ICC)=CHQINT(ICC)+DUM*(DUMXY*(QUAD(1,IC)-QUAD(2,IC))
     1       +(DUMYY-DUMXX)*QUAD(4,IC)
     2       +QUAD(5,IC)*DUMYZ-QUAD(6,IC)*DUMXZ)
      ICC=ICC+1
 403  CONTINUE
C
C  SUBTRACT DAMPING FUNCTION TERM
C
      IF ( ABS(ALFA).LE.QLIM ) GO TO 480
      PCSQ = (AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2
      XX=AA*AA*PCSQ/(AA+ALFA)
      PREI = EXP(-AA*ALFA*PCSQ/(AA+ALFA))
      IF(NROOTS.LE.3) CALL RT123
      IF(NROOTS.EQ.4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         WRITE(IW,9008)
         CALL ABRT
      END IF
      MM = 0
      DO 425 K = 1,NROOTS
      U2 = (ALFA+AA)*U(K)
      U4 = U2*U2
      WW = W(K)
C     W2(K) = TWO *WW*U2
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
      ICC=ICCT
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
      DUMXX= DUMXX-XIN(NX+MM+288)*YIN(NY+MM    )*ZIN(NZ+MM    )*W4(K)
      DUMYY= DUMYY-XIN(NX+MM    )*YIN(NY+MM+288)*ZIN(NZ+MM    )*W4(K)
      DUMZZ= DUMZZ-XIN(NX+MM    )*YIN(NY+MM    )*ZIN(NZ+MM+288)*W4(K)
      DUMXY= DUMXY-XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM    )*W4(K)
      DUMXZ= DUMXZ-XIN(NX+MM+144)*YIN(NY+MM    )*ZIN(NZ+MM+144)*W4(K)
      DUMYZ= DUMYZ-XIN(NX+MM    )*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W4(K)
  445 MM  = MM+16
      DUM =-GIJ(I)*PREI * BETA
      CHQINT(ICC)=CHQINT(ICC)+DUM*(DUMYZ*(QUAD(2,IC)-QUAD(3,IC))
     1       +(DUMZZ-DUMYY)*QUAD(6,IC)
     2       +QUAD(4,IC)*DUMXZ-QUAD(5,IC)*DUMXY)
      ICC=ICC+1
      CHQINT(ICC)=CHQINT(ICC)+DUM*(DUMXZ*(QUAD(3,IC)-QUAD(1,IC))
     1       +(DUMXX-DUMZZ)*QUAD(5,IC)
     2       +QUAD(6,IC)*DUMXY-QUAD(4,IC)*DUMYZ)
      ICC=ICC+1
      CHQINT(ICC)=CHQINT(ICC)+DUM*(DUMXY*(QUAD(1,IC)-QUAD(2,IC))
     1       +(DUMYY-DUMXX)*QUAD(4,IC)
     2       +QUAD(5,IC)*DUMYZ-QUAD(6,IC)*DUMXZ)
      ICC=ICC+1
 465  CONTINUE
C
 480  CONTINUE
 500  CONTINUE
 520  CONTINUE
C
C     ----- END OF *PRIMITIVE* LOOPS -----
C     ----- SET DEF MATRIX
C
      MAX=MAXJ
      NN=0
      ICC=1
      IC=0
C     DO 551 IC = 1,NEFQ
      DO 551 INF = 1,NFRG
      DO 551 INP = 1,NMPTS(INF)
      IC=IC+1
      IF(.NOT.DOQUAD(IC))GO TO 551
      DO 550 I=MINI,MAXI
      LI=LOCI+I
C     IN = (LI*(LI-1))/2
      DO 550 J=MINJ,MAX
      LJ=LOCJ+J
      IF (LI-LJ) 920,940,940
  920 ID = LJ
      JD = LI
      GO TO 960
  940 ID = LI
      JD = LJ
  960 NN = (ID*(ID-1))/2+JD
      DUM = DM(NN)
      ATORQ(1,INF)=ATORQ(1,INF) + DUM*CHQINT(ICC)
      ICC=ICC+1
      ATORQ(2,INF)=ATORQ(2,INF) + DUM*CHQINT(ICC)
      ICC=ICC+1
      ATORQ(3,INF)=ATORQ(3,INF) + DUM*CHQINT(ICC)
      ICC=ICC+1
  550 CONTINUE
  551 CONTINUE
C-------- END OF EFQ CENTERS LOOP
C
  580 CONTINUE
  600 CONTINUE
C
C     ----- END OF *SHELL* LOOPS -----
C
      IF(GOPARR .AND. NXT) CALL DDI_DLBRESET
      RETURN
 9008 FORMAT(/' NUMBER OF POLYNOMIAL ROOTS NEEDED (NROOTS) IS GREATER',
     *        ' THAN 6 IN EFQINT.  CALL A PROGRAMMER/QUANTUM CHEMIST.')
      END
C*MODULE EFGRDC  *DECK EFTORP
      SUBROUTINE EFTORP(DM,CHDINT,L4)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 POLNAM
C
      LOGICAL NORM,GOPARR,DSKWRK,MASWRK,NXT
C
      PARAMETER (MXSH=1000, MXGTOT=5000)
      PARAMETER (MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION DM(*),CHDINT(L4)
C
      DIMENSION DIJ(100),XIN(432),YIN(432),ZIN(432)
      DIMENSION FIJ(100)
      DIMENSION IX(20),IY(20),IZ(20),JX(20),JY(20),JZ(20)
      DIMENSION IJX(100),IJY(100),IJZ(100)
C
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG)
     *                ,EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG)
     *                ,ATORQ(3,MXFRG)
      COMMON /CSSTV / CX,CY,CZ
      COMMON /EFPPAR/ EFP(3,MXFGPT),EFPOL(9,MXFGPT),
     *                ENO,DIND(3,MXFGPT),DINDD(3,MXFGPT),POLNAM(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
C
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
      DATA PI212 /1.1283791670955D+00/
      DATA SQRT3 /1.73205080756888D+00/
      DATA SQRT5 /2.23606797749979D+00/
      DATA ZERO,ONE/0.0D+00,1.0D+00/
      DATA PT5/0.5D+00/
      DATA RLN10 /2.30258D+00/
C
C     INITIALIZATION FOR PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      LCNT = -1
C
      NEFD = NPTTPT
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C     ----- ISHELL
C
      DO 600 II = 1,NSHELL
C
C           GO PARALLEL!
C
      IF(NXT .AND. GOPARR) THEN
         LCNT = LCNT + 1
         IF(LCNT.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
         IF(NEXT.NE.LCNT) GO TO 600
      END IF
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
      DO 580 JJ = 1,NSHELL
C
C           GO PARALLEL!
C
        IF((.NOT.NXT) .AND. GOPARR) THEN
           IPCOUNT = IPCOUNT + 1
           IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 580
        END IF
C
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
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      IJ = 0
      MAX = MAXJ
      DO 50 I = MINI,MAXI
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
      DO 50 J = MINJ,MAX
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
  50  CONTINUE
      ICC=1
      DO 60 IC = 1,NEFD
      DO 60 I=1,IJ
      CHDINT(ICC) = ZERO
      ICC=ICC+1
      CHDINT(ICC) = ZERO
      ICC=ICC+1
      CHDINT(ICC) = ZERO
      ICC=ICC+1
  60  CONTINUE
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
C
C     ----- J PRIMITIVE
C
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
      CFJ = CF(JG)
      AX = (AXI+AJ*XJ)*AA1
      AY = (AYI+AJ*YJ)*AA1
      AZ = (AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR
C
      MAX = MAXJ
      NN = 0
      DUM1=ZERO
      DUM2=DUM1
      DO 170 I=MINI,MAXI
      GO TO (70,80,110,110,90,110,110,95,110,110,
     1       102,110,110,104,110,110,110,110,110,108),I
  70  DUM1=CSI*FAC
      GO TO 110
  80  DUM1=CPI*FAC
      GO TO 110
  90  DUM1=CDI*FAC
      GO TO 110
  95  IF(NORM) DUM1=DUM1*SQRT3
      GO TO 110
 102  DUM1=CFI*FAC
      GO TO 110
 104  DUM1 = DUM1 *SQRT5
      GO TO 110
 108  DUM1 = DUM1 * SQRT3
 110  CONTINUE
      DO 170 J=MINJ,MAX
      GO TO (125,130,160,160,140,160,160,150,160,160,
     1       152,160,160,154,160,160,160,160,160,156),J
  125 DUM2=DUM1*CSJ
      GO TO 160
  130 DUM2=DUM1*CPJ
      GO TO 160
  140 DUM2=DUM1*CDJ
      GO TO 160
  150 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 160
  152 DUM2 = DUM1 * CFJ
      GO TO 160
  154 DUM2 = DUM2 *SQRT5
      GO TO 160
  156 DUM2 = DUM2 * SQRT3
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
      ICC=1
      DO 480 IC = 1,NEFD
      CX   = EFP(1,IC)
      CY   = EFP(2,IC)
      CZ   = EFP(3,IC)
      XX   = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
      IF(NROOTS.LE.3) CALL RT123
      IF(NROOTS.EQ.4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         WRITE(IW,9008)
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
      CHDINT(ICC)=CHDINT(ICC)-DUM*DUMX
      ICC=ICC+1
      CHDINT(ICC)=CHDINT(ICC)-DUM*DUMY
      ICC=ICC+1
      CHDINT(ICC)=CHDINT(ICC)-DUM*DUMZ
      ICC=ICC+1
 403  CONTINUE
C
 480  CONTINUE
 500  CONTINUE
 520  CONTINUE
C
C     ----- END OF *PRIMITIVE* LOOPS -----
C
      MAX=MAXJ
      ICC=1
      IC=0
C     DO 551 IC = 1,NEFD
      DO 551 INF = 1,NFRG
      DO 551 INP = 1,NPPTS(INF)
      IC=IC+1
      DO 550 I=MINI,MAXI
      LI=LOCI+I
      DO 550 J=MINJ,MAX
      LJ=LOCJ+J
      IF (LI-LJ) 920,940,940
  920 ID = LJ
      JD = LI
      GO TO 960
  940 ID = LI
      JD = LJ
  960 NN = (ID*(ID-1))/2+JD
      DUM = DM(NN)
      ATORQ(1,INF)=ATORQ(1,INF)-PT5*DUM*(CHDINT(ICC+1)
     $           *(DIND(3,IC)+DINDD(3,IC))
     $           -CHDINT(ICC+2)*(DIND(2,IC)+DINDD(2,IC)))
      ICC=ICC+1
      ATORQ(2,INF)=ATORQ(2,INF)-PT5*DUM*(CHDINT(ICC+1)
     $           *(DIND(1,IC)+DINDD(1,IC))
     $           -CHDINT(ICC-1)*(DIND(3,IC)+DINDD(3,IC)))
      ICC=ICC+1
      ATORQ(3,INF)=ATORQ(3,INF)-PT5*DUM*(CHDINT(ICC-2)
     $           *(DIND(2,IC)+DINDD(2,IC))
     $           -CHDINT(ICC-1)*(DIND(1,IC)+DINDD(1,IC)))
      ICC=ICC+1
  550 CONTINUE
  551 CONTINUE
C-------- END OF EFD CENTERS LOOP
C
  580 CONTINUE
  600 CONTINUE
C
      IF(GOPARR .AND. NXT) CALL DDI_DLBRESET
      RETURN
 9008 FORMAT(/' NUMBER OF POLYNOMIAL ROOTS NEEDED (NROOTS) IS GREATER',
     *        ' THAN 6 IN EFTORP.  CALL A PROGRAMMER/QUANTUM CHEMIST.')
      END
C*MODULE EFGRDC  *DECK EFSXYZ
      SUBROUTINE EFSXYZ
C
C     ----- GAUSS-HERMITE QUADRATURE USING MINIMUM POINT FORMULA -----
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION H(21),W(21),MIN(6),MAX(6)
C
      COMMON/STV/XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
      COMMON/CSSTV/ CX,CY,CZ
      COMMON/HERMIT/H1,H2(2),H3(3),H4(4),H5(5),H6(6),H7(7)
      COMMON/WERMIT/W1,W2(2),W3(3),W4(4),W5(5),W6(6),W7(7)
C
      EQUIVALENCE (H(1),H1),(W(1),W1)
C
      DATA MIN /1,2,4,7,11,16/
      DATA MAX /1,3,6,10,15,21/
      DATA ZERO /0.0D+00/
C
      XINT = ZERO
      YINT = ZERO
      ZINT = ZERO
      NPTS = (NI+NJ+4-2)/2+1
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
      PX = PX*PX*PX*PX
      PY = PY*PY*PY*PY
      PZ = PZ*PZ*PZ*PZ
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
C*MODULE EFGRDC  *DECK EFODEF
      SUBROUTINE EFODEF(DM,CHOINT,L4)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
      COMMON /FMCOM / XX(1)
C
      CALL VALFM(LOADFM)
      NEED=10*MXFGPT
      CALL GETFM(NEED)
      CALL EFODE(DM,CHOINT,L4,XX(LOADFM+1))
      CALL RETFM(NEED)
C
      RETURN
      END
C*MODULE EFGRDC  *DECK EFODE
      SUBROUTINE EFODE(DM,CHOINT,L4,OCT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL NORM,GOPARR,DSKWRK,MASWRK,NXT
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      PARAMETER (MXSH=1000, MXGTOT=5000)
      PARAMETER (MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION DM(*),CHOINT(L4)
C
      DIMENSION OCT(10,*)
      DIMENSION DIJ(100),XIN(720),YIN(720),ZIN(720)
      DIMENSION FIJ(100),GIJ(100)
      DIMENSION IX(20),IY(20),IZ(20),JX(20),JY(20),JZ(20)
      DIMENSION IJX(100),IJY(100),IJZ(100),W6(9),W8(9)
C
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
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
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
C
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
      DATA PI212 /1.1283791670955D+00/
      DATA SQRT3 /1.73205080756888D+00/
      DATA SQRT5 /2.23606797749979D+00/
      DATA ZERO,ONE,TWO/0.0D+00,1.0D+00,2.0D+00/
      DATA PT5,THREE /0.5D+00,3.0D+00/
      DATA FIVE,EIGHT/5.0D+00,8.0D+00/
      DATA RLN10 /2.30258D+00/
      DATA QLIM /1.0D-07/
C
      NEFO = NMTTPT
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C     INITIALIZATION FOR PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      LCNT = -1
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
C
C           GO PARALLEL!
C
      IF(NXT .AND. GOPARR) THEN
         LCNT = LCNT + 1
         IF(LCNT.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
         IF(NEXT.NE.LCNT) GO TO 600
      END IF
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
      DO 580 JJ = 1,NSHELL
C
C           GO PARALLEL!
C
        IF((.NOT.NXT) .AND. GOPARR) THEN
           IPCOUNT = IPCOUNT + 1
           IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 580
        END IF
C
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
      NROOTS = (LIT+LJT+4-2)/2 + 1
      RR = (XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      IJ = 0
      MAX = MAXJ
      DO 50 I = MINI,MAXI
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
      DO 50 J = MINJ,MAX
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
  50  CONTINUE
      ICC=1
      DO 60 IC = 1,NEFO
      DO 60 I=1,IJ
      CHOINT(ICC) = ZERO
      ICC=ICC+1
      CHOINT(ICC) = ZERO
      ICC=ICC+1
      CHOINT(ICC) = ZERO
      ICC=ICC+1
  60  CONTINUE
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
C     ----- J PRIMITIVE
C
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
      MAX = MAXJ
      NN = 0
      DUM1=ZERO
      DUM2=DUM1
      DO 170 I=MINI,MAXI
      GO TO (70,80,110,110,90,110,110,95,110,110,
     1       102,110,110,104,110,110,110,110,110,108),I
  70  DUM1=CSI*FAC
      GO TO 110
  80  DUM1=CPI*FAC
      GO TO 110
  90  DUM1=CDI*FAC
      GO TO 110
  95  IF(NORM) DUM1=DUM1*SQRT3
      GO TO 110
 102  DUM1=CFI*FAC
      GO TO 110
 104  DUM1 = DUM1 *SQRT5
      GO TO 110
 108  DUM1 = DUM1 * SQRT3
 110  CONTINUE
      DO 170 J=MINJ,MAX
      GO TO (125,130,160,160,140,160,160,150,160,160,
     1       152,160,160,154,160,160,160,160,160,156),J
  125 DUM2=DUM1*CSJ
      GO TO 160
  130 DUM2=DUM1*CPJ
      GO TO 160
  140 DUM2=DUM1*CDJ
      GO TO 160
  150 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 160
  152 DUM2 = DUM1 * CFJ
      GO TO 160
  154 DUM2 = DUM2 *SQRT5
      GO TO 160
  156 DUM2 = DUM2 * SQRT3
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
      ICC=1
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
      IF(NROOTS.LE.3) CALL RT123
      IF(NROOTS.EQ.4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         WRITE(IW,9008)
         CALL ABRT
      END IF
      MM = 0
      DO 402 K = 1,NROOTS
      U2 = AA*U(K)
      U4 = U2*U2
      U6 = U4*U2
      U8 = U6*U2
      WW = W(K)
      W6(K) = -EIGHT*WW*U6
      W8(K) = -TWO*EIGHT*WW*U8
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
      CALL EFSXYZ
      XIN(JN+576) = XINT
      YIN(JN+576) = YINT
      ZIN(JN+576) = ZINT
  400 CONTINUE
  402 MM = MM+16
      ICCT=ICC
      DO 404 I = 1,IJ
      NX    = IJX(I)
      NY    = IJY(I)
      NZ    = IJZ(I)
      D1XXX = ZERO
      D2XXX = ZERO
      D3XXX = ZERO
      D1YYY = ZERO
      D2YYY = ZERO
      D3YYY = ZERO
      D1ZZZ = ZERO
      D2ZZZ = ZERO
      D3ZZZ = ZERO
      D1XXY = ZERO
      D2XXY = ZERO
      D3XXY = ZERO
      D1XXZ = ZERO
      D2XXZ = ZERO
      D3XXZ = ZERO
      D1XYY = ZERO
      D2XYY = ZERO
      D3XYY = ZERO
      D1XZZ = ZERO
      D2XZZ = ZERO
      D3XZZ = ZERO
      D1YZZ = ZERO
      D2YZZ = ZERO
      D3YZZ = ZERO
      D1YYZ = ZERO
      D2YYZ = ZERO
      D3YYZ = ZERO
      D1XYZ = ZERO
      D2XYZ = ZERO
      D3XYZ = ZERO
      MM    = 0
      DO 403 K = 1,NROOTS
      D1XXX=D1XXX+XIN(NX+MM+288)*YIN(NY+MM)*ZIN(NZ+MM)*W6(K)*THREE
     1      -XIN(NX+MM+576)*YIN(NY+MM)*ZIN(NZ+MM)*W8(K)
      D2YYY=D2YYY+XIN(NX+MM)*YIN(NY+MM+288)*ZIN(NZ+MM)*W6(K)*THREE
     1      -XIN(NX+MM)*YIN(NY+MM+576)*ZIN(NZ+MM)*W8(K)
      D3ZZZ=D3ZZZ+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM+288)*W6(K)*THREE
     1      -XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM+576)*W8(K)
      D1XXY=D1XXY+XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM)*W6(K)*TWO
     1      -XIN(NX+MM+432)*YIN(NY+MM+144)*ZIN(NZ+MM)*W8(K)
      D2XYY=D2XYY+XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM)*W6(K)*TWO
     1      -XIN(NX+MM+144)*YIN(NY+MM+432)*ZIN(NZ+MM)*W8(K)
      D3XZZ=D3XZZ+XIN(NX+MM+144)*YIN(NY+MM)*ZIN(NZ+MM+144)*W6(K)*TWO
     1      -XIN(NX+MM+144)*YIN(NY+MM)*ZIN(NZ+MM+432)*W8(K)
      D1XXZ=D1XXZ+XIN(NX+MM+144)*YIN(NY+MM)*ZIN(NZ+MM+144)*W6(K)*TWO
     1      -XIN(NX+MM+432)*YIN(NY+MM)*ZIN(NZ+MM+144)*W8(K)
      D2YYZ=D2YYZ+XIN(NX+MM)*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W6(K)*TWO
     1      -XIN(NX+MM)*YIN(NY+MM+432)*ZIN(NZ+MM+144)*W8(K)
      D3YZZ=D3YZZ+XIN(NX+MM)*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W6(K)*TWO
     1      -XIN(NX+MM)*YIN(NY+MM+144)*ZIN(NZ+MM+432)*W8(K)
      D1XYY=D1XYY+XIN(NX+MM)*YIN(NY+MM+288)*ZIN(NZ+MM)*W6(K)
     1      -XIN(NX+MM+288)*YIN(NY+MM+288)*ZIN(NZ+MM)*W8(K)
      D2XXY=D2XXY+XIN(NX+MM+288)*YIN(NY+MM)*ZIN(NZ+MM    )*W6(K)
     1      -XIN(NX+MM+288)*YIN(NY+MM+288)*ZIN(NZ+MM)*W8(K)
      D3XXZ=D3XXZ+XIN(NX+MM+288)*YIN(NY+MM)*ZIN(NZ+MM)*W6(K)
     1      -XIN(NX+MM+288)*YIN(NY+MM)*ZIN(NZ+MM+288)*W8(K)
      D1XZZ=D1XZZ+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM+288)*W6(K)
     1      -XIN(NX+MM+288)*YIN(NY+MM)*ZIN(NZ+MM+288)*W8(K)
      D2YZZ=D2YZZ+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM+288)*W6(K)
     1      -XIN(NX+MM)*YIN(NY+MM+288)*ZIN(NZ+MM+288)*W8(K)
      D3YYZ=D3YYZ+XIN(NX+MM)*YIN(NY+MM+288)*ZIN(NZ+MM)*W6(K)
     1      -XIN(NX+MM)*YIN(NY+MM+288)*ZIN(NZ+MM+288)*W8(K)
      D1YYZ=D1YYZ-XIN(NX+MM+144)*YIN(NY+MM+288)*ZIN(NZ+MM+144)*W8(K)
      D2XXZ=D2XXZ-XIN(NX+MM+288)*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W8(K)
      D3XXY=D3XXY-XIN(NX+MM+288)*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W8(K)
      D1YZZ=D1YZZ-XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM+288)*W8(K)
      D2XZZ=D2XZZ-XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM+288)*W8(K)
      D3XYY=D3XYY-XIN(NX+MM+144)*YIN(NY+MM+288)*ZIN(NZ+MM+144)*W8(K)
      D1XYZ=D1XYZ+XIN(NX+MM)*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W6(K)
     1      -XIN(NX+MM+288)*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W8(K)
      D2XYZ=D2XYZ+XIN(NX+MM+144)*YIN(NY+MM)*ZIN(NZ+MM+144)*W6(K)
     1      -XIN(NX+MM+144)*YIN(NY+MM+288)*ZIN(NZ+MM+144)*W8(K)
      D3XYZ=D3XYZ+XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM)*W6(K)
     1      -XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM+288)*W8(K)
      D1YYY=D1YYY-XIN(NX+MM+144)*YIN(NY+MM+432)*ZIN(NZ+MM)*W8(K)
      D2XXX=D2XXX-XIN(NX+MM+432)*YIN(NY+MM+144)*ZIN(NZ+MM)*W8(K)
      D3XXX=D3XXX-XIN(NX+MM+432)*YIN(NY+MM)*ZIN(NZ+MM+144)*W8(K)
      D1ZZZ=D1ZZZ-XIN(NX+MM+144)*YIN(NY+MM)*ZIN(NZ+MM+432)*W8(K)
      D2ZZZ=D2ZZZ-XIN(NX+MM)*YIN(NY+MM+144)*ZIN(NZ+MM+432)*W8(K)
      D3YYY=D3YYY-XIN(NX+MM)*YIN(NY+MM+432)*ZIN(NZ+MM+144)*W8(K)
  403 MM  = MM+16
      DUM = FIJ(I)
      CHOINT(ICC) = CHOINT(ICC) -
     1     DUM * ( D1XXX * OCT(1,IC)
     1           + D1YYY * OCT(2,IC)
     1           + D1ZZZ * OCT(3,IC)
     1 + THREE * ( D1XXY * OCT(4,IC)
     1           + D1XXZ * OCT(5,IC)
     1           + D1XYY * OCT(6,IC)
     1           + D1YYZ * OCT(7,IC)
     1           + D1XZZ * OCT(8,IC)
     1           + D1YZZ * OCT(9,IC)
     1     + TWO * D1XYZ * OCT(10,IC)))
      ICC=ICC+1
      CHOINT(ICC) = CHOINT(ICC) -
     1     DUM * ( D2XXX * OCT(1,IC)
     1           + D2YYY * OCT(2,IC)
     1           + D2ZZZ * OCT(3,IC)
     1 + THREE * ( D2XXY * OCT(4,IC)
     1           + D2XXZ * OCT(5,IC)
     1           + D2XYY * OCT(6,IC)
     1           + D2YYZ * OCT(7,IC)
     1           + D2XZZ * OCT(8,IC)
     1           + D2YZZ * OCT(9,IC)
     1     + TWO * D2XYZ * OCT(10,IC)))
      ICC=ICC+1
      CHOINT(ICC) = CHOINT(ICC) -
     1     DUM * ( D3XXX * OCT(1,IC)
     1           + D3YYY * OCT(2,IC)
     1           + D3ZZZ * OCT(3,IC)
     1 + THREE * ( D3XXY * OCT(4,IC)
     1           + D3XXZ * OCT(5,IC)
     1           + D3XYY * OCT(6,IC)
     1           + D3YYZ * OCT(7,IC)
     1           + D3XZZ * OCT(8,IC)
     1           + D3YZZ * OCT(9,IC)
     1     + TWO * D3XYZ * OCT(10,IC)))
      ICC=ICC+1
 404  CONTINUE
C
C  SUBTRACT DAMPING FUNCTION TERM
C
      IF ( ABS(ALFA).LE.QLIM ) GO TO 480
      PCSQ = (AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2
      XX=AA*AA*PCSQ/(AA+ALFA)
      PREI = EXP(-AA*ALFA*PCSQ/(AA+ALFA))
      IF(NROOTS.LE.3) CALL RT123
      IF(NROOTS.EQ.4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         WRITE(IW,9008)
         CALL ABRT
      END IF
      MM = 0
      DO 425 K = 1,NROOTS
      U2 = (ALFA+AA)*U(K)
      U4 = U2*U2
      U6 = U4*U2
      U8 = U6*(U2+ALFA)
      WW = W(K)
      W6(K) = -EIGHT*WW*U6
      W8(K) = -TWO*EIGHT*WW*U8
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
      CALL EFSXYZ
      XIN(JN+576) = XINT
      YIN(JN+576) = YINT
      ZIN(JN+576) = ZINT
  405 CONTINUE
  425 MM = MM+16
      ICC=ICCT
      DO 465 I = 1,IJ
      NX    = IJX(I)
      NY    = IJY(I)
      NZ    = IJZ(I)
      D1XXX = ZERO
      D2XXX = ZERO
      D3XXX = ZERO
      D1YYY = ZERO
      D2YYY = ZERO
      D3YYY = ZERO
      D1ZZZ = ZERO
      D2ZZZ = ZERO
      D3ZZZ = ZERO
      D1XXY = ZERO
      D2XXY = ZERO
      D3XXY = ZERO
      D1XXZ = ZERO
      D2XXZ = ZERO
      D3XXZ = ZERO
      D1XYY = ZERO
      D2XYY = ZERO
      D3XYY = ZERO
      D1XZZ = ZERO
      D2XZZ = ZERO
      D3XZZ = ZERO
      D1YZZ = ZERO
      D2YZZ = ZERO
      D3YZZ = ZERO
      D1YYZ = ZERO
      D2YYZ = ZERO
      D3YYZ = ZERO
      D1XYZ = ZERO
      D2XYZ = ZERO
      D3XYZ = ZERO
      MM    = 0
      DO 445 K = 1,NROOTS
      D1XXX=D1XXX+XIN(NX+MM+288)*YIN(NY+MM)*ZIN(NZ+MM)*W6(K)*THREE
     1      -XIN(NX+MM+576)*YIN(NY+MM)*ZIN(NZ+MM)*W8(K)
      D2YYY=D2YYY+XIN(NX+MM)*YIN(NY+MM+288)*ZIN(NZ+MM)*W6(K)*THREE
     1      -XIN(NX+MM)*YIN(NY+MM+576)*ZIN(NZ+MM)*W8(K)
      D3ZZZ=D3ZZZ+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM+288)*W6(K)*THREE
     1      -XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM+576)*W8(K)
      D1XXY=D1XXY+XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM)*W6(K)*TWO
     1      -XIN(NX+MM+432)*YIN(NY+MM+144)*ZIN(NZ+MM)*W8(K)
      D2XYY=D2XYY+XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM)*W6(K)*TWO
     1      -XIN(NX+MM+144)*YIN(NY+MM+432)*ZIN(NZ+MM)*W8(K)
      D3XZZ=D3XZZ+XIN(NX+MM+144)*YIN(NY+MM)*ZIN(NZ+MM+144)*W6(K)*TWO
     1      -XIN(NX+MM+144)*YIN(NY+MM)*ZIN(NZ+MM+432)*W8(K)
      D1XXZ=D1XXZ+XIN(NX+MM+144)*YIN(NY+MM)*ZIN(NZ+MM+144)*W6(K)*TWO
     1      -XIN(NX+MM+432)*YIN(NY+MM)*ZIN(NZ+MM+144)*W8(K)
      D2YYZ=D2YYZ+XIN(NX+MM)*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W6(K)*TWO
     1      -XIN(NX+MM)*YIN(NY+MM+432)*ZIN(NZ+MM+144)*W8(K)
      D3YZZ=D3YZZ+XIN(NX+MM)*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W6(K)*TWO
     1      -XIN(NX+MM)*YIN(NY+MM+144)*ZIN(NZ+MM+432)*W8(K)
      D1XYY=D1XYY+XIN(NX+MM)*YIN(NY+MM+288)*ZIN(NZ+MM)*W6(K)
     1      -XIN(NX+MM+288)*YIN(NY+MM+288)*ZIN(NZ+MM)*W8(K)
      D2XXY=D2XXY+XIN(NX+MM+288)*YIN(NY+MM)*ZIN(NZ+MM    )*W6(K)
     1      -XIN(NX+MM+288)*YIN(NY+MM+288)*ZIN(NZ+MM)*W8(K)
      D3XXZ=D3XXZ+XIN(NX+MM+288)*YIN(NY+MM)*ZIN(NZ+MM)*W6(K)
     1      -XIN(NX+MM+288)*YIN(NY+MM)*ZIN(NZ+MM+288)*W8(K)
      D1XZZ=D1XZZ+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM+288)*W6(K)
     1      -XIN(NX+MM+288)*YIN(NY+MM)*ZIN(NZ+MM+288)*W8(K)
      D2YZZ=D2YZZ+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM+288)*W6(K)
     1      -XIN(NX+MM)*YIN(NY+MM+288)*ZIN(NZ+MM+288)*W8(K)
      D3YYZ=D3YYZ+XIN(NX+MM)*YIN(NY+MM+288)*ZIN(NZ+MM)*W6(K)
     1      -XIN(NX+MM)*YIN(NY+MM+288)*ZIN(NZ+MM+288)*W8(K)
      D1YYZ=D1YYZ-XIN(NX+MM+144)*YIN(NY+MM+288)*ZIN(NZ+MM+144)*W8(K)
      D2XXZ=D2XXZ-XIN(NX+MM+288)*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W8(K)
      D3XXY=D3XXY-XIN(NX+MM+288)*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W8(K)
      D1YZZ=D1YZZ-XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM+288)*W8(K)
      D2XZZ=D2XZZ-XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM+288)*W8(K)
      D3XYY=D3XYY-XIN(NX+MM+144)*YIN(NY+MM+288)*ZIN(NZ+MM+144)*W8(K)
      D1XYZ=D1XYZ+XIN(NX+MM)*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W6(K)
     1      -XIN(NX+MM+288)*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W8(K)
      D2XYZ=D2XYZ+XIN(NX+MM+144)*YIN(NY+MM)*ZIN(NZ+MM+144)*W6(K)
     1      -XIN(NX+MM+144)*YIN(NY+MM+288)*ZIN(NZ+MM+144)*W8(K)
      D3XYZ=D3XYZ+XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM)*W6(K)
     1      -XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM+288)*W8(K)
      D1YYY=D1YYY-XIN(NX+MM+144)*YIN(NY+MM+432)*ZIN(NZ+MM)*W8(K)
      D2XXX=D2XXX-XIN(NX+MM+432)*YIN(NY+MM+144)*ZIN(NZ+MM)*W8(K)
      D3XXX=D3XXX-XIN(NX+MM+432)*YIN(NY+MM)*ZIN(NZ+MM+144)*W8(K)
      D1ZZZ=D1ZZZ-XIN(NX+MM+144)*YIN(NY+MM)*ZIN(NZ+MM+432)*W8(K)
      D2ZZZ=D2ZZZ-XIN(NX+MM)*YIN(NY+MM+144)*ZIN(NZ+MM+432)*W8(K)
      D3YYY=D3YYY-XIN(NX+MM)*YIN(NY+MM+432)*ZIN(NZ+MM+144)*W8(K)
  445 MM  = MM+16
      DUM =-GIJ(I)*PREI * BETA
      CHOINT(ICC) = CHOINT(ICC) -
     1     DUM * ( D1XXX * OCT(1,IC)
     1           + D1YYY * OCT(2,IC)
     1           + D1ZZZ * OCT(3,IC)
     1 + THREE * ( D1XXY * OCT(4,IC)
     1           + D1XXZ * OCT(5,IC)
     1           + D1XYY * OCT(6,IC)
     1           + D1YYZ * OCT(7,IC)
     1           + D1XZZ * OCT(8,IC)
     1           + D1YZZ * OCT(9,IC)
     1     + TWO * D1XYZ * OCT(10,IC)))
      ICC=ICC+1
      CHOINT(ICC) = CHOINT(ICC) -
     1     DUM * ( D2XXX * OCT(1,IC)
     1           + D2YYY * OCT(2,IC)
     1           + D2ZZZ * OCT(3,IC)
     1 + THREE * ( D2XXY * OCT(4,IC)
     1           + D2XXZ * OCT(5,IC)
     1           + D2XYY * OCT(6,IC)
     1           + D2YYZ * OCT(7,IC)
     1           + D2XZZ * OCT(8,IC)
     1           + D2YZZ * OCT(9,IC)
     1     + TWO * D2XYZ * OCT(10,IC)))
      ICC=ICC+1
      CHOINT(ICC) = CHOINT(ICC) -
     1     DUM * ( D3XXX * OCT(1,IC)
     1           + D3YYY * OCT(2,IC)
     1           + D3ZZZ * OCT(3,IC)
     1 + THREE * ( D3XXY * OCT(4,IC)
     1           + D3XXZ * OCT(5,IC)
     1           + D3XYY * OCT(6,IC)
     1           + D3YYZ * OCT(7,IC)
     1           + D3XZZ * OCT(8,IC)
     1           + D3YZZ * OCT(9,IC)
     1     + TWO * D3XYZ * OCT(10,IC)))
      ICC=ICC+1
 465  CONTINUE
C
 480  CONTINUE
 500  CONTINUE
 520  CONTINUE
C
C     ----- END OF *PRIMITIVE* LOOPS -----
C     ----- SET DEF MATRIX
C
      MAX=MAXJ
      NN=0
      ICC=1
C
      DO 551 IC = 1,NEFO
      IF(.NOT.DOOCTU(IC))GO TO 551
      DO 550 I=MINI,MAXI
      LI=LOCI+I
      DO 550 J=MINJ,MAX
      LJ=LOCJ+J
      IF (LI-LJ) 920,940,940
  920 ID = LJ
      JD = LI
      GO TO 960
  940 ID = LI
      JD = LJ
  960 NN = (ID*(ID-1))/2+JD
      DUM = DM(NN)
      DEF(1,IC)=DEF(1,IC) + DUM*CHOINT(ICC)
      ICC=ICC+1
      DEF(2,IC)=DEF(2,IC) + DUM*CHOINT(ICC)
      ICC=ICC+1
      DEF(3,IC)=DEF(3,IC) + DUM*CHOINT(ICC)
      ICC=ICC+1
  550 CONTINUE
  551 CONTINUE
C
C-------- END OF EFO CENTERS LOOP
C
  580 CONTINUE
  600 CONTINUE
C
      IF(GOPARR .AND. NXT) CALL DDI_DLBRESET
      RETURN
 9008 FORMAT(/' NUMBER OF POLYNOMIAL ROOTS NEEDED (NROOTS) IS GREATER',
     *        ' THAN 6 IN EFODEF.  CALL A PROGRAMMER/QUANTUM CHEMIST.')
      END
C*MODULE EFGRDC  *DECK EFTORO
      SUBROUTINE EFTORO(DM,CHOINT,L4)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
      COMMON /FMCOM / XX(1)
C
      CALL VALFM(LOADFM)
      NEED=10*MXFGPT
      CALL GETFM(NEED)
      CALL EFOTO(DM,CHOINT,L4,XX(LOADFM+1))
      CALL RETFM(NEED)
C
      RETURN
      END
C*MODULE EFGRDC  *DECK EFOTO
      SUBROUTINE EFOTO(DM,CHOINT,L4,OCT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      CHARACTER*8 FRGNME
C
      LOGICAL NORM,GOPARR,DSKWRK,MASWRK,NXT
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      PARAMETER (MXSH=1000, MXGTOT=5000)
      PARAMETER (MXATM=500)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION DM(*),CHOINT(L4)
C
      DIMENSION OCT(10,*)
      DIMENSION DIJ(100),XIN(720),YIN(720),ZIN(720)
      DIMENSION FIJ(100),GIJ(100)
      DIMENSION IX(20),IY(20),IZ(20),JX(20),JY(20),JZ(20)
      DIMENSION IJX(100),IJY(100),IJZ(100),W6(9)
C
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG)
     *                ,EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG)
     *                ,ATORQ(3,MXFRG)
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
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
C
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
      DATA PI212 /1.1283791670955D+00/
      DATA SQRT3 /1.73205080756888D+00/
      DATA SQRT5 /2.23606797749979D+00/
      DATA ZERO,ONE,TWO/0.0D+00,1.0D+00,2.0D+00/
      DATA PT5,THREE /0.5D+00,3.0D+00/
      DATA FIVE,EIGHT/5.0D+00,8.0D+00/
      DATA RLN10 /2.30258D+00/
      DATA QLIM /1.0D-07/
C
      NEFO = NMTTPT
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C     INITIALIZATION FOR PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      LCNT = -1
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
C
C           GO PARALLEL!
C
      IF(NXT .AND. GOPARR) THEN
         LCNT = LCNT + 1
         IF(LCNT.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
         IF(NEXT.NE.LCNT) GO TO 600
      END IF
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
      DO 580 JJ = 1,NSHELL
C
C           GO PARALLEL!
C
        IF((.NOT.NXT) .AND. GOPARR) THEN
           IPCOUNT = IPCOUNT + 1
           IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 580
        END IF
C
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
      NROOTS = (LIT+LJT+4-2)/2 + 1
      RR = (XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      IJ = 0
      MAX = MAXJ
      DO 50 I = MINI,MAXI
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
      DO 50 J = MINJ,MAX
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
  50  CONTINUE
      ICC=1
      DO 60 IC = 1,NEFO
      DO 60 I=1,IJ
      CHOINT(ICC) = ZERO
      ICC=ICC+1
      CHOINT(ICC) = ZERO
      ICC=ICC+1
      CHOINT(ICC) = ZERO
      ICC=ICC+1
  60  CONTINUE
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
C     ----- J PRIMITIVE
C
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
      MAX = MAXJ
      NN = 0
      DUM1=ZERO
      DUM2=DUM1
      DO 170 I=MINI,MAXI
      GO TO (70,80,110,110,90,110,110,95,110,110,
     1       102,110,110,104,110,110,110,110,110,108),I
  70  DUM1=CSI*FAC
      GO TO 110
  80  DUM1=CPI*FAC
      GO TO 110
  90  DUM1=CDI*FAC
      GO TO 110
  95  IF(NORM) DUM1=DUM1*SQRT3
      GO TO 110
 102  DUM1=CFI*FAC
      GO TO 110
 104  DUM1 = DUM1 *SQRT5
      GO TO 110
 108  DUM1 = DUM1 * SQRT3
 110  CONTINUE
      DO 170 J=MINJ,MAX
      GO TO (125,130,160,160,140,160,160,150,160,160,
     1       152,160,160,154,160,160,160,160,160,156),J
  125 DUM2=DUM1*CSJ
      GO TO 160
  130 DUM2=DUM1*CPJ
      GO TO 160
  140 DUM2=DUM1*CDJ
      GO TO 160
  150 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 160
  152 DUM2 = DUM1 * CFJ
      GO TO 160
  154 DUM2 = DUM2 *SQRT5
      GO TO 160
  156 DUM2 = DUM2 * SQRT3
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
      ICC=1
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
      IF(NROOTS.LE.3) CALL RT123
      IF(NROOTS.EQ.4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         WRITE(IW,9008)
         CALL ABRT
      END IF
      MM = 0
      DO 402 K = 1,NROOTS
      U2 = AA*U(K)
      U4 = U2*U2
      U6 = U4*U2
C     U8 = U6*U2
      WW = W(K)
      W6(K) = -EIGHT*WW*U6
C     W8(K) = -TWO*EIGHT*WW*U8
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
      ICCT=ICC
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
      DXZZ = ZERO
      DYZZ = ZERO
      DYYZ = ZERO
      DXYZ = ZERO
      MM    = 0
      DO 403 K = 1,NROOTS
      DXXX=DXXX+XIN(NX+MM+432)*YIN(NY+MM)*ZIN(NZ+MM)*W6(K)
      DYYY=DYYY+XIN(NX+MM)*YIN(NY+MM+432)*ZIN(NZ+MM)*W6(K)
      DZZZ=DZZZ+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM+432)*W6(K)
      DXXY=DXXY+XIN(NX+MM+288)*YIN(NY+MM+144)*ZIN(NZ+MM)*W6(K)
      DXYY=DXYY+XIN(NX+MM+144)*YIN(NY+MM+288)*ZIN(NZ+MM)*W6(K)
      DXZZ=DXZZ+XIN(NX+MM+144)*YIN(NY+MM)*ZIN(NZ+MM+288)*W6(K)
      DXXZ=DXXZ+XIN(NX+MM+288)*YIN(NY+MM)*ZIN(NZ+MM+144)*W6(K)
      DYYZ=DYYZ+XIN(NX+MM)*YIN(NY+MM+288)*ZIN(NZ+MM+144)*W6(K)
      DYZZ=DYZZ+XIN(NX+MM)*YIN(NY+MM+144)*ZIN(NZ+MM+288)*W6(K)
      DXYZ=DXYZ+XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W6(K)
  403 MM  = MM+16
      DUM = FIJ(I)
      CHOINT(ICC) = CHOINT(ICC) - THREE*
     1     DUM * ( -OCT(2,IC)*DYYZ
     1            + OCT(3,IC)*DYZZ
     1            - OCT(4,IC)*DXXZ
     1            + OCT(5,IC)*DXXY
     1            - OCT(6,IC)*TWO*DXYZ
     1            + OCT(7,IC)*(DYYY-TWO*DYZZ)
     1            + OCT(8,IC)*TWO*DXYZ
     1            + OCT(9,IC)*(DYYZ*TWO-DZZZ)
     1      + TWO * OCT(10,IC)*(DXYY-DXZZ))
      ICC=ICC+1
      CHOINT(ICC) = CHOINT(ICC) - THREE*
     1     DUM * ( OCT(1,IC)* DXXZ
     1           - OCT(3,IC)* DXZZ
     1           + OCT(4,IC)*TWO*DXYZ
     1           + OCT(5,IC)*(DXZZ*TWO-DXXX)
     1           + OCT(6,IC)* DYYZ
     1           - OCT(7,IC)* DXYY
     1           + OCT(8,IC)*(DZZZ-TWO*DXXZ)
     1           - OCT(9,IC)*TWO*DXYZ
     1     + TWO * OCT(10,IC)*(DYZZ-DXXY))
      ICC=ICC+1
      CHOINT(ICC) = CHOINT(ICC) - DUM*THREE
     1        *  ( -OCT(1,IC)* DXXY
     1            + OCT(2,IC)* DXYY
     1            + OCT(4,IC)*(DXXX-TWO*DXYY)
     1            - OCT(5,IC)*TWO*DXYZ
     1            + OCT(6,IC)*(DXXY*TWO-DYYY)
     1            + OCT(7,IC)*TWO*DXYZ
     1            - OCT(8,IC)* DYZZ
     1            + OCT(9,IC)* DXZZ
     1     + TWO  * OCT(10,IC)*(DXXZ-DYYZ))
      ICC=ICC+1
 404  CONTINUE
C
C  SUBTRACT DAMPING FUNCTION TERM
C
      IF ( ABS(ALFA).LE.QLIM ) GO TO 480
      PCSQ = (AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2
      XX=AA*AA*PCSQ/(AA+ALFA)
      PREI = EXP(-AA*ALFA*PCSQ/(AA+ALFA))
      IF(NROOTS.LE.3) CALL RT123
      IF(NROOTS.EQ.4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.EQ.6) CALL ROOT6
      IF(NROOTS.GE.7) THEN
         WRITE(IW,9008)
         CALL ABRT
      END IF
      MM = 0
      DO 425 K = 1,NROOTS
      U2 = (ALFA+AA)*U(K)
      U4 = U2*U2
      U6 = U4*U2
C     U8 = U6*(U2+ALFA)
      WW = W(K)
      W6(K) = -EIGHT*WW*U6
C     W8(K) = -TWO*EIGHT*WW*U8
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
      ICC=ICCT
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
      DXZZ = ZERO
      DYZZ = ZERO
      DYYZ = ZERO
      DXYZ = ZERO
      MM    = 0
      DO 445 K = 1,NROOTS
      DXXX=DXXX+XIN(NX+MM+432)*YIN(NY+MM)*ZIN(NZ+MM)*W6(K)
      DYYY=DYYY+XIN(NX+MM)*YIN(NY+MM+432)*ZIN(NZ+MM)*W6(K)
      DZZZ=DZZZ+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM+432)*W6(K)
      DXXY=DXXY+XIN(NX+MM+288)*YIN(NY+MM+144)*ZIN(NZ+MM)*W6(K)
      DXYY=DXYY+XIN(NX+MM+144)*YIN(NY+MM+288)*ZIN(NZ+MM)*W6(K)
      DXZZ=DXZZ+XIN(NX+MM+144)*YIN(NY+MM)*ZIN(NZ+MM+288)*W6(K)
      DXXZ=DXXZ+XIN(NX+MM+288)*YIN(NY+MM)*ZIN(NZ+MM+144)*W6(K)
      DYYZ=DYYZ+XIN(NX+MM)*YIN(NY+MM+288)*ZIN(NZ+MM+144)*W6(K)
      DYZZ=DYZZ+XIN(NX+MM)*YIN(NY+MM+144)*ZIN(NZ+MM+288)*W6(K)
      DXYZ=DXYZ+XIN(NX+MM+144)*YIN(NY+MM+144)*ZIN(NZ+MM+144)*W6(K)
  445 MM  = MM+16
      DUM =-GIJ(I)*PREI * BETA
      CHOINT(ICC) = CHOINT(ICC) - THREE*
     1     DUM * ( -OCT(2,IC)* DYYZ
     1            + OCT(3,IC)* DYZZ
     1            - OCT(4,IC)* DXXZ
     1            + OCT(5,IC)* DXXY
     1            - OCT(6,IC)*TWO*DXYZ
     1            + OCT(7,IC)*(DYYY-TWO*DYZZ)
     1            + OCT(8,IC)*TWO*DXYZ
     1            + OCT(9,IC)*(DYYZ*TWO-DZZZ)
     1     + TWO * OCT(10,IC)*(DXYY-DXZZ))
      ICC=ICC+1
      CHOINT(ICC) = CHOINT(ICC) - THREE*
     1     DUM * ( OCT(1,IC)* DXXZ
     1           - OCT(3,IC)* DXZZ
     1           + OCT(4,IC)*TWO*DXYZ
     1           + OCT(5,IC)*(DXZZ*TWO-DXXX)
     1           + OCT(6,IC)* DYYZ
     1           - OCT(7,IC)* DXYY
     1           + OCT(8,IC)*(DZZZ-TWO*DXXZ)
     1           - OCT(9,IC)*TWO*DXYZ
     1     + TWO * OCT(10,IC)*(DYZZ-DXXY))
      ICC=ICC+1
      CHOINT(ICC) = CHOINT(ICC) - DUM*THREE
     1        *  ( -OCT(1,IC)* DXXY
     1            + OCT(2,IC)* DXYY
     1            + OCT(4,IC)*(DXXX-TWO*DXYY)
     1            - OCT(5,IC)*TWO*DXYZ
     1            + OCT(6,IC)*(DXXY*TWO-DYYY)
     1            + OCT(7,IC)*TWO*DXYZ
     1            - OCT(8,IC)* DYZZ
     1            + OCT(9,IC)* DXZZ
     1     + TWO  * OCT(10,IC)*(DXXZ-DYYZ))
      ICC=ICC+1
 465  CONTINUE
C
 480  CONTINUE
 500  CONTINUE
 520  CONTINUE
C
C     ----- END OF *PRIMITIVE* LOOPS -----
C     ----- SET DEF MATRIX
C
      MAX=MAXJ
      NN=0
      ICC=1
C
      IC=0
C     DO 551 IC = 1,NEFO
      DO 551 INF = 1,NFRG
      DO 551 INP = 1,NMPTS(INF)
      IC=IC+1
      IF(.NOT.DOOCTU(IC)) GO TO 551
      DO 550 I=MINI,MAXI
      LI=LOCI+I
      DO 550 J=MINJ,MAX
      LJ=LOCJ+J
      IF (LI-LJ) 920,940,940
  920 ID = LJ
      JD = LI
      GO TO 960
  940 ID = LI
      JD = LJ
  960 NN = (ID*(ID-1))/2+JD
      DUM = DM(NN)
      ATORQ(1,INF)=ATORQ(1,INF) + DUM*CHOINT(ICC)
      ICC=ICC+1
      ATORQ(2,INF)=ATORQ(2,INF) + DUM*CHOINT(ICC)
      ICC=ICC+1
      ATORQ(3,INF)=ATORQ(3,INF) + DUM*CHOINT(ICC)
      ICC=ICC+1
  550 CONTINUE
  551 CONTINUE
C
C-------- END OF EFO CENTERS LOOP
C
  580 CONTINUE
  600 CONTINUE
C
      IF(GOPARR .AND. NXT) CALL DDI_DLBRESET
      RETURN
 9008 FORMAT(/' NUMBER OF POLYNOMIAL ROOTS NEEDED (NROOTS) IS GREATER',
     *        ' THAN 6 IN EFODEF.  CALL A PROGRAMMER/QUANTUM CHEMIST.')
      END
