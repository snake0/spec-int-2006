C 19 NOV 00 - MWS - DZVBAS,N311G,TZVBAS: RETURN NORMALIZED CXINP COEFS
C 13 FEB 99 - AY  - DZV BASIS FOR K,CA ADDED
C  3 DEC 93 - TK  - TZVBAS,TZV002: ADD LI TZV BASIS SET
C  7 APR 92 - MWS - DZVSHL: FIX ITYP DEFN OF LI
C 12 MAR 92 - MWS - REDIMENSION TO 500 ATOMS
C  3 JAN 92 - TLW - MAKE WRITES PARALLEL
C  8 NOV 90 - MWS - RAISE DZV'S IQ WITH DZVBAS, ADD DZV004 (WITH HELP!)
C  7 AUG 90 - TLW - ADD CF AND CG TO COMMON NSHEL
C 10 MAY 90 - MWS - CHANGE ARGUMENTS TO DHBAS, N311G, TZVBAS
C 23 MAR 90 - MK  - NEW CODE FOR TZV BASIS SETS; MODIFICATIONS
C                   FOR DZP, TZP BASIS SETS.
C                   MCBAS AND MCTWO REMOVED (NOW IN TZVBAS, TZV002)
C  7 NOV 89 - MWS - MCTWO: NA P EXPONENT SHOULD BE 0.5526, NOT 0.5226
C 27 FEB 89 - STE - DHBAS,MCBAS,N311G: REWORK CONTRACTION NORMALIZATION
C                   CHANGE DATA STATEMENTS TO PARAMETERS
C 10 AUG 88 - MWS - MXSH,MXGSH,MXGTOT FROM 120,10,440 TO 1000,30,5000
C 29 MAY 88 - MWS - USE PARAMETERS TO DIMENSION COMMON
C 11 JUL 86 - JAB - SANITIZE FLOATING POINT CONSTANTS
C  4 APR 86 - MWS - DIMENSION E,CS,CP TO 23 IN MCTWO
C                   CHANGE DUMMY DIMENSION TO 11 IN N311XC
C 14 OCT 85 - STE - USE GENERIC SQRT AND ABS
C 17 APR 85 - MWS - FINISH TYPING AND PROOFING OF MCLEAN/CHANDLER
C 16 APR 85 - MWS - IMPLEMENT 6-311G FROM GAUSSIAN 82
C 10 NOV 94 - MWS - REMOVE FTNCHEK ERRORS
C 12 APR 85 - MWS - FORM NEW SECTION BY TAKING DUNNING/HAY
C                   BASES OUT OF BASN21, AND ADDING MACLEAN/CHANDLER
C*MODULE BASEXT  *DECK DZVBAS
      SUBROUTINE DZVBAS(NUCZ,CSINP,CPINP,CDINP,SCFAC,IERR1,IERR2,
     *                 INTYP,NANGM,NBFS,MINF,MAXF,LOC,NGAUSS,NS)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      DIMENSION CSINP(*),CPINP(*),CDINP(*),SCFAC(*),
     *          INTYP(*),NANGM(*),NBFS(*),MINF(*),MAXF(*),NS(*)
      DIMENSION EEX(30),CCS(30),CCP(30),CCD(30),
     *          ITYPES(11),NGBFS(11)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, PT5=0.5D+00, PT75=0.75D+00)
      PARAMETER (PI32=5.56832799683170D+00, TOL=1.0D-10)
      PARAMETER (SCALH1=1.20D+00, SCALH2=1.15D+00)
C
C     ----- SET UP THE "DOUBLE ZETA VALENCE" BASIS SET -----
C
      DO 100 I = 1,30
         EEX(I) = ZERO
         CCS(I) = ZERO
         CCP(I) = ZERO
         CCD(I) = ZERO
  100 CONTINUE
C
C     ----- HYDROGEN TO HELIUM -----
C     BECAUSE THE ORIGINAL VERSION OF GAMESS USED THE -31G SCALE
C     FACTOR FOR HYDROGEN, NOT DUNNING'S RECOMENDATION OF 1.2,
C     THE CODE IS LEFT USING THE -31G FACTORS BY DEFAULT.
C
      IF (NUCZ .GT. 2) GO TO 220
      CALL DZV001(EEX,CCS,CCP,NUCZ)
      IF (SCFAC(1) .LE. ZERO) SCFAC(1) = SCALH1
      IF (SCFAC(2) .LE. ZERO) SCFAC(2) = SCALH2
      EEX(1) = EEX(1)*SCFAC(1)**2
      EEX(2) = EEX(2)*SCFAC(1)**2
      EEX(3) = EEX(3)*SCFAC(1)**2
      EEX(4) = EEX(4)*SCFAC(2)**2
      GO TO 300
C
C     ----- LITHIUM TO NEON -----
C
  220 CONTINUE
      IF (NUCZ.GT.10) GO TO 230
      CALL DZV002(EEX,CCS,CCP,CCD,NUCZ)
      GO TO 300
C
C     ----- SODIUM TO ARGON -----
C
  230 CONTINUE
      IF(NUCZ.GT.18) GO TO 240
      CALL DZV003(EEX,CCS,CCP,CCD,NUCZ)
      GO TO 300
C
C     ----- POTASSIUM TO KRYPTON -----
C
  240 CONTINUE
      IF(NUCZ.GT.36) GO TO 250
      CALL DZV004(EEX,CCS,CCP,CCD,NUCZ)
      GO TO 300
C
C     ----- SODIUM TO ARGON -----
C
  250 CONTINUE
      IF (MASWRK) WRITE(6,*)
     *     'DZV BASES EXIST ONLY UP TO KR.'
      CALL BERROR(3)
      STOP
C
C     ----- NOW SET UP EACH SHELL -----
C
  300 CONTINUE
      CALL DZVSHL(NUCZ,MXPASS,ITYPES,NGBFS)
C
      IGX = 0
      IPASS = 0
  310 CONTINUE
      IPASS = IPASS + 1
      IF(IPASS.GT.MXPASS) RETURN
C                         ******
      IGAUSS = NGBFS(IPASS)
      ITYP = ITYPES(IPASS)
C
      NSHELL = NSHELL+1
      IF(NSHELL.GT.MXSH) THEN
         IERR1=1
         RETURN
      END IF
      NS(NAT) = NS(NAT)+1
      KMIN(NSHELL) = MINF(ITYP)
      KMAX(NSHELL) = MAXF(ITYP)
      KSTART(NSHELL) = NGAUSS+1
      KATOM(NSHELL) = NAT
      KTYPE(NSHELL) = NANGM(ITYP)
      INTYP(NSHELL) = ITYP
      KNG(NSHELL) = IGAUSS
      KLOC(NSHELL) = LOC+1
      NGAUSS = NGAUSS+IGAUSS
      IF(NGAUSS.GT.MXGTOT) THEN
         IERR2=1
         RETURN
      END IF
      LOC = LOC+NBFS(ITYP)
      K1 = KSTART(NSHELL)
      K2 = K1+KNG(NSHELL)-1
      DO 720 I = 1,IGAUSS
         K = K1+I-1
         EX(K)    = EEX(IGX+I)
         CSINP(K) = CCS(IGX+I)
         CPINP(K) = CCP(IGX+I)
         CDINP(K) = CCD(IGX+I)
         CS(K) = CSINP(K)
         CP(K) = CPINP(K)
         CD(K) = CDINP(K)
  720 CONTINUE
      IGX = IGX + IGAUSS
C
C     ----- ALWAYS UNNORMALIZE PRIMITIVES -----
C
      DO 740 K = K1,K2
         EE = EX(K)+EX(K)
         FACS = PI32/(EE*SQRT(EE))
         FACP = PT5*FACS/EE
         FACD = PT75*FACS/(EE*EE)
         CS(K) = CS(K)/SQRT(FACS)
         CP(K) = CP(K)/SQRT(FACP)
         CD(K) = CD(K)/SQRT(FACD)
  740 CONTINUE
C
C     ----- IF(NORMF.EQ.0) NORMALIZE BASIS FUNCTIONS. -----
C
      IF (NORMF .EQ. 1) GO TO 820
      FACS = ZERO
      FACP = ZERO
      FACD = ZERO
      DO 780 IG = K1,K2
         DO 770 JG = K1,IG
            EE = EX(IG)+EX(JG)
            FAC = EE*SQRT(EE)
            DUMS = CS(IG)*CS(JG)/FAC
            DUMP = PT5*CP(IG)*CP(JG)/(EE*FAC)
            DUMD = PT75*CD(IG)*CD(JG)/(EE*EE*FAC)
            IF (IG .NE. JG) THEN
               DUMS = DUMS+DUMS
               DUMP = DUMP+DUMP
               DUMD = DUMD+DUMD
            END IF
            FACS = FACS+DUMS
            FACP = FACP+DUMP
            FACD = FACD+DUMD
  770    CONTINUE
  780 CONTINUE
      IF (FACS .GT. TOL) FACS = ONE/SQRT(FACS*PI32)
      IF (FACP .GT. TOL) FACP = ONE/SQRT(FACP*PI32)
      IF (FACD .GT. TOL) FACD = ONE/SQRT(FACD*PI32)
      DO 800 IG = K1,K2
         CS(IG) = CS(IG) * FACS
         CP(IG) = CP(IG) * FACP
         CD(IG) = CD(IG) * FACD
         CSINP(IG) = CSINP(IG) * FACS
         CPINP(IG) = CPINP(IG) * FACP
         CDINP(IG) = CDINP(IG) * FACD
  800 CONTINUE
C
C        AND BRANCH BACK FOR THE NEXT SHELL
C
  820 CONTINUE
      GO TO 310
      END
C*MODULE BASEXT  *DECK DZVSHL
      SUBROUTINE DZVSHL(NUCZ,MXPASS,ITYPES,NGBFS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION ITYPES(11),NGBFS(11)
      DIMENSION KINDS(36),NPASS(8),ITYPS(11,8),NGS(11,8)
C
C       KINDS ARE H-HE, LI, BE-NE, NA-MG, AL-AR, K-CA, SC-ZN, GA-KR
C       NPASS TELLS HOW MANY SHELLS ARE IN EACH BASIS
C
      DATA KINDS/2*1,  2,  7*3,  2*4,  6*5,  2*6,  10*7,  6*8/
      DATA NPASS/2,    5,    5,    0,   10,    9,     0,   11/
C
C       ITYPS DEFINES THE TYPE OF SHELL, 1=S,2=P,3=D
C
      DATA ITYPS/2*1,9*0,   3*1,2*2,6*0,   3*1,2*2,6*0,
     *           11*0,  6*1,4*2,0,  5*1,3*2,1*3,2*0,  11*0,   6*1,4*2,3/
C
C       NGS TELLS THE NUMBER OF GAUSSIANS IN EACH SHELL
C
      DATA NGS/3,1,9*0,    7,2,1,3,1,6*0,  7,2,1,4,1,6*0,
     *         11*0,   5,3,1,1,1,1,4,2,1,1,0,   6,3,3,1,1,5,3,1,3,2*0,   
     *         11*0,   8,2,1,1,1,1,6,3,1,1,5/
C
C        --- PROVIDE "BRAINS" FOR DOUBLE ZETA CONTRACTIONS ---
C
      KIND = KINDS(NUCZ)
      MXPASS = NPASS(KIND)
      DO 100 I=1,MXPASS
         ITYPES(I) = ITYPS(I,KIND)
         NGBFS(I) = NGS(I,KIND)
  100 CONTINUE
      RETURN
      END
C*MODULE BASEXT  *DECK DZV001
      SUBROUTINE DZV001(E,S,P,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION E(20),S(20),P(20)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C     ----- DUNNING/HAY CONTRACTION OF HUZINAGA'S (4S) -----
C
      GO TO (100,120),N
C
C     ----- HYDROGEN (4S)/(2S) -----
C
  100 CONTINUE
      E(1) = 1.336D+01
      S(1) = 0.032828D+00
      E(2) = 2.013D+00
      S(2) = 0.231204D+00
      E(3) = 4.538D-01
      S(3) = 0.817226D+00
      E(4) = 1.233D-01
      S(4) = 1.000000D+00
      E(5) = 1.000D+00
      P(5) = 1.000000D+00
      RETURN
C
C     ----- HE -----
C
  120 CONTINUE
      IF (MASWRK) WRITE(6,*)
     *       'DZV BASIS DOES NOT EXIST FOR HE.'
      CALL BERROR(3)
      RETURN
      END
C*MODULE BASEXT  *DECK DZV002
      SUBROUTINE DZV002(E,S,P,D,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION E(20),S(20),P(20),D(20)
C
C     ----- DUNNING/HAY CONTRACTION OF HUZINAGA'S (9S,5P) -----
C
      NN = N-2
      GO TO (100,120,140,160,180,200,220,240),NN
C
C     ----- LI -----
C
  100 CONTINUE
      E(1) = 9.213D+02
      S(1) = 0.001367D+00
      E(2) = 1.387D+02
      S(2) = 0.010425D+00
      E(3) = 3.194D+01
      S(3) = 0.049859D+00
      E(4) = 9.353D+00
      S(4) = 0.160701D+00
      E(5) = 3.158D+00
      S(5) = 0.344604D+00
      E(6) = 1.157D+00
      S(6) = 0.425197D+00
      E(7) = 4.446D-01
      S(7) = 0.169468D+00
      E(8) = 4.446D-01
      S(8) = -0.222311D+00
      E(9) = 7.666D-02
      S(9) = 1.116477D+00
      E(10) = 2.864D-02
      S(10) = 1.000000D+00
      E(11) = 1.488D+00
      P(11) = 0.038770D+00
      E(12) = 2.667D-01
      P(12) = 0.236257D+00
      E(13) = 7.201D-02
      P(13) = 0.830448D+00
      E(14) = 2.370D-02
      P(14) = 1.000000D+00
      RETURN
C
C     ----- BE -----
C
  120 CONTINUE
      E(1) = 1.741D+03
      S(1) = 0.001305D+00
      E(2) = 2.621D+02
      S(2) = 0.009955D+00
      E(3) = 6.033D+01
      S(3) = 0.048031D+00
      E(4) = 1.762D+01
      S(4) = 0.158577D+00
      E(5) = 5.933D+00
      S(5) = 0.351325D+00
      E(6) = 2.185D+00
      S(6) = 0.427006D+00
      E(7) = 8.590D-01
      S(7) = 0.160490D+00
      E(8) = 2.185D+00
      S(8) = -0.185294D+00
      E(9) = 1.806D-01
      S(9) = 1.057014D+00
      E(10) = 5.835D-02
      S(10) = 1.000000D+00
      E(11) = 6.710D+00
      P(11) = 0.016378D+00
      E(12) = 1.442D+00
      P(12) = 0.091553D+00
      E(13) = 4.103D-01
      P(13) = 0.341469D+00
      E(14) = 1.397D-01
      P(14) = 0.685428D+00
      E(15) = 4.922D-02
      P(15) = 1.000000D+00
      RETURN
C
C     ----- B  -----
C
  140 CONTINUE
      E(1) = 2.788D+03
      S(1) = 0.001288D+00
      E(2) = 4.190D+02
      S(2) = 0.009835D+00
      E(3) = 9.647D+01
      S(3) = 0.047648D+00
      E(4) = 2.807D+01
      S(4) = 0.160069D+00
      E(5) = 9.376D+00
      S(5) = 0.362894D+00
      E(6) = 3.406D+00
      S(6) = 0.433582D+00
      E(7) = 1.306D+00
      S(7) = 0.140082D+00
      E(8) = 3.406D+00
      S(8) = -0.179330D+00
      E(9) = 3.245D-01
      S(9) = 1.062594D+00
      E(10) = 1.022D-01
      S(10) = 1.000000D+00
      E(11) = 1.134D+01
      P(11) = 0.017988D+00
      E(12) = 2.436D+00
      P(12) = 0.110343D+00
      E(13) = 6.836D-01
      P(13) = 0.383072D+00
      E(14) = 2.134D-01
      P(14) = 0.647895D+00
      E(15) = 7.011D-02
      P(15) = 1.000000D+00
      E(16) = 7.000D-01
      D(16) = 1.000000D+00
      RETURN
C
C     ----- C  -----
C
  160 CONTINUE
      E(1) = 4.233D+03
      S(1) = 0.001220D+00
      E(2) = 6.349D+02
      S(2) = 0.009342D+00
      E(3) = 1.461D+02
      S(3) = 0.045452D+00
      E(4) = 4.250D+01
      S(4) = 0.154657D+00
      E(5) = 1.419D+01
      S(5) = 0.358866D+00
      E(6) = 5.148D+00
      S(6) = 0.438632D+00
      E(7) = 1.967D+00
      S(7) = 0.145918D+00
      E(8) = 5.148D+00
      S(8) = -0.168367D+00
      E(9) = 4.962D-01
      S(9) = 1.060091D+00
      E(10) = 1.533D-01
      S(10) = 1.000000D+00
      E(11) = 1.816D+01
      P(11) = 0.018539D+00
      E(12) = 3.986D+00
      P(12) = 0.115436D+00
      E(13) = 1.143D+00
      P(13) = 0.386188D+00
      E(14) = 3.594D-01
      P(14) = 0.640114D+00
      E(15) = 1.146D-01
      P(15) = 1.000000D+00
      E(16) = 7.500D-01
      D(16) = 1.000000D+00
      RETURN
C
C     ----- N -----
C
  180 CONTINUE
      E(1) = 5.909D+03
      S(1) = 0.001190D+00
      E(2) = 8.875D+02
      S(2) = 0.009099D+00
      E(3) = 2.047D+02
      S(3) = 0.044145D+00
      E(4) = 5.984D+01
      S(4) = 0.150464D+00
      E(5) = 2.000D+01
      S(5) = 0.356741D+00
      E(6) = 7.193D+00
      S(6) = 0.446533D+00
      E(7) = 2.686D+00
      S(7) = 0.145603D+00
      E(8) = 7.193D+00
      S(8) = -0.160405D+00
      E(9) = 7.000D-01
      S(9) = 1.058215D+00
      E(10) = 2.133D-01
      S(10) = 1.000000D+00
      E(11) = 2.679D+01
      P(11) = 0.018254D+00
      E(12) = 5.956D+00
      P(12) = 0.116461D+00
      E(13) = 1.707D+00
      P(13) = 0.390178D+00
      E(14) = 5.314D-01
      P(14) = 0.637102D+00
      E(15) = 1.654D-01
      P(15) = 1.000000D+00
      E(16) = 8.000D-01
      D(16) = 1.000000D+00
      RETURN
C
C     ----- O  ------
C
  200 CONTINUE
      E(1) = 7.817D+03
      S(1) = 0.001176D+00
      E(2) = 1.176D+03
      S(2) = 0.008968D+00
      E(3) = 2.732D+02
      S(3) = 0.042868D+00
      E(4) = 8.117D+01
      S(4) = 0.143930D+00
      E(5) = 2.718D+01
      S(5) = 0.355630D+00
      E(6) = 9.532D+00
      S(6) = 0.461248D+00
      E(7) = 3.414D+00
      S(7) = 0.140206D+00
      E(8) = 9.532D+00
      S(8) = -0.154153D+00
      E(9) = 9.398D-01
      S(9) = 1.056914D+00
      E(10) = 2.846D-01
      S(10) = 1.000000D+00
      E(11) = 3.518D+01
      P(11) = 0.019580D+00
      E(12) = 7.904D+00
      P(12) = 0.124200D+00
      E(13) = 2.305D+00
      P(13) = 0.394714D+00
      E(14) = 7.171D-01
      P(14) = 0.627376D+00
      E(15) = 2.137D-01
      P(15) = 1.000000D+00
      E(16) = 8.500D-01
      D(16) = 1.000000D+00
      RETURN
C
C     ----- F  -----
C
  220 CONTINUE
      E(1) = 9.995D+03
      S(1) = 0.001166D+00
      E(2) = 1.506D+03
      S(2) = 0.008876D+00
      E(3) = 3.503D+02
      S(3) = 0.042380D+00
      E(4) = 1.041D+02
      S(4) = 0.142929D+00
      E(5) = 3.484D+01
      S(5) = 0.355372D+00
      E(6) = 1.222D+01
      S(6) = 0.462085D+00
      E(7) = 4.369D+00
      S(7) = 0.140848D+00
      E(8) = 1.222D+01
      S(8) = -0.148452D+00
      E(9) = 1.208D+00
      S(9) = 1.055270D+00
      E(10) = 3.634D-01
      S(10) = 1.000000D+00
      E(11) = 4.436D+01
      P(11) = 0.020876D+00
      E(12) = 1.008D+01
      P(12) = 0.130107D+00
      E(13) = 2.996D+00
      P(13) = 0.396166D+00
      E(14) = 9.383D-01
      P(14) = 0.620404D+00
      E(15) = 2.733D-01
      P(15) = 1.000000D+00
      E(16) = 9.000D-01
      D(16) = 1.000000D+00
      RETURN
C
C     ----- NE -----
C
  240 CONTINUE
      E(1) = 1.210D+04
      S(1) = 0.001200D+00
      E(2) = 1.821D+03
      S(2) = 0.009092D+00
      E(3) = 4.328D+02
      S(3) = 0.041305D+00
      E(4) = 1.325D+02
      S(4) = 0.137867D+00
      E(5) = 4.377D+01
      S(5) = 0.362433D+00
      E(6) = 1.491D+01
      S(6) = 0.472247D+00
      E(7) = 5.127D+00
      S(7) = 0.130035D+00
      E(8) = 1.491D+01
      S(8) = -0.140810D+00
      E(9) = 1.491D+00
      S(9) = 1.053327D+00
      E(10) = 4.468D-01
      S(10) = 1.000000D+00
      E(11) = 5.645D+01
      P(11) = 0.020875D+00
      E(12) = 1.292D+01
      P(12) = 0.130032D+00
      E(13) = 3.865D+00
      P(13) = 0.395679D+00
      E(14) = 1.203D+00
      P(14) = 0.621450D+00
      E(15) = 3.444D-01
      P(15) = 1.000000D+00
      E(16) = 1.000D+00
      D(16) = 1.000000D+00
      RETURN
      END
C*MODULE BASEXT  *DECK DZV003
      SUBROUTINE DZV003(E,S,P,D,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION E(21),S(21),P(21),D(21)
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C     ----- DUNNING/HAY CONTRACTION OF HUZINAGA'S (11S,7P) -----
C
      NN = N-10
      GO TO (100,120,140,160,180,200,220,240),NN
C
  100 CONTINUE
  120 CONTINUE
      IF (MASWRK) WRITE(6,*) 'DZV BASIS DOES NOT EXIST FOR NA,MG.'
      CALL BERROR(3)
      STOP
C
C     ----- AL -----
C
  140 CONTINUE
      E(1) = 2.349D+04
      S(1) = 0.002509D+00
      E(2) = 3.548D+03
      S(2) = 0.018986D+00
      E(3) = 8.235D+02
      S(3) = 0.092914D+00
      E(4) = 2.377D+02
      S(4) = 0.335935D+00
      E(5) = 7.860D+01
      S(5) = 0.647391D+00
      E(6) = 7.860D+01
      S(6) = 0.111937D+00
      E(7) = 2.905D+01
      S(7) = 0.655976D+00
      E(8) = 1.162D+01
      S(8) = 0.283349D+00
      E(9) = 3.465D+00
      S(9) = 1.000000D+00
      E(10) = 1.233D+00
      S(10) = 1.000000D+00
      E(11) = 2.018D-01
      S(11) = 1.000000D+00
      E(12) = 7.805D-02
      S(12) = 1.000000D+00
      E(13) = 1.415D+02
      P(13) = 0.017882D+00
      E(14) = 3.322D+01
      P(14) = 0.120375D+00
      E(15) = 1.039D+01
      P(15) = 0.411580D+00
      E(16) = 3.593D+00
      P(16) = 0.595353D+00
      E(17) = 3.593D+00
      P(17) = 0.211758D+00
      E(18) = 1.242D+00
      P(18) = 0.837795D+00
      E(19) = 3.040D-01
      P(19) = 1.000000D+00
      E(20) = 7.629D-02
      P(20) = 1.000000D+00
      E(21) = 3.000D-01
      D(21) = 1.000000D+00
      RETURN
C
C     ----- SI -----
C
  160 CONTINUE
      E(1) = 2.674D+04
      S(1) = 0.002583D+00
      E(2) = 4.076D+03
      S(2) = 0.019237D+00
      E(3) = 9.533D+02
      S(3) = 0.093843D+00
      E(4) = 2.746D+02
      S(4) = 0.341235D+00
      E(5) = 9.068D+01
      S(5) = 0.641675D+00
      E(6) = 9.068D+01
      S(6) = 0.121439D+00
      E(7) = 3.353D+01
      S(7) = 0.653143D+00
      E(8) = 1.346D+01
      S(8) = 0.277624D+00
      E(9) = 4.051D+00
      S(9) = 1.000000D+00
      E(10) = 1.484D+00
      S(10) = 1.000000D+00
      E(11) = 2.704D-01
      S(11) = 1.000000D+00
      E(12) = 9.932D-02
      S(12) = 1.000000D+00
      E(13) = 1.637D+02
      P(13) = 0.011498D+00
      E(14) = 3.835D+01
      P(14) = 0.077726D+00
      E(15) = 1.202D+01
      P(15) = 0.263595D+00
      E(16) = 4.185D+00
      P(16) = 0.758269D+00
      E(17) = 4.185D+00
      P(17) = -1.173045D+00
      E(18) = 1.483D+00
      P(18) = 1.438335D+00
      E(19) = 3.350D-01
      P(19) = 1.000000D+00
      E(20) = 9.699D-02
      P(20) = 1.000000D+00
      E(21) = 4.000D-01
      D(21) = 1.000000D+00
      RETURN
C
C     ----- P  -----
C
  180 CONTINUE
      E(1) = 3.063D+04
      S(1) = 0.002619D+00
      E(2) = 4.684D+03
      S(2) = 0.019479D+00
      E(3) = 1.094D+03
      S(3) = 0.095207D+00
      E(4) = 3.153D+02
      S(4) = 0.345742D+00
      E(5) = 1.041D+02
      S(5) = 0.636288D+00
      E(6) = 1.041D+02
      S(6) = 0.130706D+00
      E(7) = 3.842D+01
      S(7) = 0.650274D+00
      E(8) = 1.545D+01
      S(8) = 0.272308D+00
      E(9) = 4.656D+00
      S(9) = 1.000000D+00
      E(10) = 1.759D+00
      S(10) = 1.000000D+00
      E(11) = 3.409D-01
      S(11) = 1.000000D+00
      E(12) = 1.238D-01
      S(12) = 1.000000D+00
      E(13) = 1.877D+02
      P(13) = 0.013158D+00
      E(14) = 4.363D+01
      P(14) = 0.090494D+00
      E(15) = 1.360D+01
      P(15) = 0.305054D+00
      E(16) = 4.766D+00
      P(16) = 0.713579D+00
      E(17) = 4.766D+00
      P(17) = -0.792573D+00
      E(18) = 1.743D+00
      P(18) = 1.429987D+00
      E(19) = 4.192D-01
      P(19) = 1.000000D+00
      E(20) = 1.245D-01
      P(20) = 1.000000D+00
      E(21) = 5.000D-01
      D(21) = 1.000000D+00
      RETURN
C
C     ----- S  -----
C
  200 CONTINUE
      E(1) = 3.571D+04
      S(1) = 0.002565D+00
      E(2) = 5.397D+03
      S(2) = 0.019405D+00
      E(3) = 1.250D+03
      S(3) = 0.095595D+00
      E(4) = 3.599D+02
      S(4) = 0.345793D+00
      E(5) = 1.192D+02
      S(5) = 0.635794D+00
      E(6) = 1.192D+02
      S(6) = 0.130096D+00
      E(7) = 4.398D+01
      S(7) = 0.651301D+00
      E(8) = 1.763D+01
      S(8) = 0.271955D+00
      E(9) = 5.420D+00
      S(9) = 1.000000D+00
      E(10) = 2.074D+00
      S(10) = 1.000000D+00
      E(11) = 4.246D-01
      S(11) = 1.000000D+00
      E(12) = 1.519D-01
      S(12) = 1.000000D+00
      E(13) = 2.129D+02
      P(13) = 0.014091D+00
      E(14) = 4.960D+01
      P(14) = 0.096685D+00
      E(15) = 1.552D+01
      P(15) = 0.323874D+00
      E(16) = 5.476D+00
      P(16) = 0.691756D+00
      E(17) = 5.476D+00
      P(17) = -0.626737D+00
      E(18) = 2.044D+00
      P(18) = 1.377051D+00
      E(19) = 5.218D-01
      P(19) = 1.000000D+00
      E(20) = 1.506D-01
      P(20) = 1.000000D+00
      E(21) = 6.000D-01
      D(21) = 1.000000D+00
      RETURN
C
C     ----- CL -----
C
  220 CONTINUE
      E(1) = 4.085D+04
      S(1) = 0.002532D+00
      E(2) = 6.179D+03
      S(2) = 0.019207D+00
      E(3) = 1.425D+03
      S(3) = 0.095257D+00
      E(4) = 4.092D+02
      S(4) = 0.345589D+00
      E(5) = 1.355D+02
      S(5) = 0.636401D+00
      E(6) = 1.355D+02
      S(6) = 0.120956D+00
      E(7) = 5.013D+01
      S(7) = 0.648511D+00
      E(8) = 2.021D+01
      S(8) = 0.275487D+00
      E(9) = 6.283D+00
      S(9) = 1.000000D+00
      E(10) = 2.460D+00
      S(10) = 1.000000D+00
      E(11) = 5.271D-01
      S(11) = 1.000000D+00
      E(12) = 1.884D-01
      S(12) = 1.000000D+00
      E(13) = 2.408D+02
      P(13) = 0.014595D+00
      E(14) = 5.656D+01
      P(14) = 0.099047D+00
      E(15) = 1.785D+01
      P(15) = 0.330562D+00
      E(16) = 6.350D+00
      P(16) = 0.682874D+00
      E(17) = 6.350D+00
      P(17) = -0.561785D+00
      E(18) = 2.403D+00
      P(18) = 1.351901D+00
      E(19) = 6.410D-01
      P(19) = 1.000000D+00
      E(20) = 1.838D-01
      P(20) = 1.000000D+00
      E(21) = 6.500D-01
      D(21) = 1.000000D+00
      RETURN
C
C        ARGON
C
  240 CONTINUE
      IF (MASWRK) WRITE(6,*) 'DZV BASIS DOES NOT EXIST FOR AR.'
      CALL BERROR(3)
      STOP
      END
C*MODULE BASEXT  *DECK DZV004
      SUBROUTINE DZV004(E,S,P,D,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION E(30),S(30),P(30),D(30)
      PARAMETER (ONE=1.0D+00)
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C     ----- BINNING/CURTISS [6S,4P,1D] CONTRACTION -----
C      ----- OF DUNNING'S (14S,11P,5D) PRIMITIVES -----
C             J.COMPUT.CHEM. 11, 1206-1216(1990)
C     THERE ARE FOUR TYPOS IN THE BINNING/CURTISS PAPER,
C     GE:P(18), AS:P(21), BR:S(3),  KR:E(11).
C     THE VALUES HERE ARE CORRECT.
C
      IF(N.GE.21  .AND.  N.LE.30) GO TO 900
      IF(N.LT.21) THEN
         NN = N-18
         S(13) = ONE
         S(14) = ONE
         P(23) = ONE
      END IF
      IF(N.GT.30) THEN
         NN = N-30+2
         S(11) = ONE
         S(12) = ONE
         S(13) = ONE
         S(14) = ONE
         P(24) = ONE
         P(25) = ONE
      END IF
C
      GO TO (100,200,300,400,500,600,700,800),NN
C
C        POTASSIUM
C
  100 CONTINUE
      E(1) = 31478.746764D+00
      E(2) = 4726.8876066D+00
      E(3) = 1075.4345353D+00
      E(4) = 303.39811023D+00
      E(5) = 98.327112831D+00
      E(6) = 33.636222177D+00
      S(1) = 0.0039838653994D+00
      S(2) = 0.030501759762D+00
      S(3) = 0.15073752622D+00
      S(4) = 0.51912939801D+00
      S(5) = 1.0366957005D+00
      S(6) = 0.76398963199D+00
      E(7) = 65.639209962D+00
      E(8) = 7.3162592218D+00
      E(9) = 2.8902580135D+00
      S(7) = -0.28242617106D+00
      S(8) = 1.6914935860D+00
      S(9) = 1.2965331953D+00
      E(10)= 4.5459748965D+00
      E(11)= 0.70404124062D+00
      E(12)= 0.28266888959D+00
      S(10)= -0.0076343555273D+00
      S(11)= 0.025635718960D+00
      S(12)= 0.016606859208D+00
      E(13)= 0.029058164020D+00
      E(14)= 0.012111638157D+00
      E(15)= 361.22492154D+00
      E(16)= 84.670222166D+00
      E(17)= 26.469088236D+00
      E(18)= 9.2658077615D+00
      E(19)= 3.3423388293D+00
      P(15)= 0.020906479823D+00
      P(16)= 0.15043641740D+00
      P(17)= 0.55440061077D+00
      P(18)= 1.0409009991D+00
      P(19)= 0.67825341194D+00
      E(20)= 1.6354616D+00
      E(21)= 0.83196755D+00
      E(22)= 0.43684152D+00
      P(20)= 0.24090185D+00
      P(21)= 0.29942427D+00
      P(22)= 0.44607904D+00
      E(23)= 0.17983732D+00
      E(24)= 0.7968D+00
      E(25)= 2.558D+00
      E(26)= 10.07D+00
      D(24)= 0.423691D+00
      D(25)= 0.216293D+00
      D(26)= 0.0480790D+00
      RETURN
C
C        CALCIUM
C
  200 CONTINUE
      E(1) = 35138.713929D+00
      E(2) = 5276.4111348D+00
      E(3) = 1200.4692589D+00
      E(4) = 338.71810542D+00
      E(5) = 109.85385922D+00
      E(6) = 37.608880299D+00
      S(1) = 0.0039482520740D+00
      S(2) = 0.030234243552D+00
      S(3) = 0.14952019681D+00
      S(4) = 0.51597345713D+00
      S(5) = 1.0339510296D+00
      S(6) = 0.76937933526D+00
      E(7) = 73.107977555D+00
      E(8) = 8.2407705688D+00
      E(9) = 3.2959812993D+00
      S(7) = -0.28268525011D+00
      S(8) = 1.6796092142D+00
      S(9) = 1.2803766016D+00
      E(10)= 5.2341800914D+00
      E(11)= 0.84187220515D+00
      E(12)= 0.36510294029D+00
      S(10)= -0.0076868604561D+00
      S(11)= 0.025382375978D+00
      S(12)= 0.016512171511D+00
      E(13)= 0.051222402884D+00
      E(14)= 0.019825111408D+00
      E(15)= 413.11313893D+00
      E(16)= 96.935786224D+00
      E(17)= 30.372154659D+00
      E(18)= 10.684776830D+00
      E(19)= 3.8821258350D+00
      P(15)= 0.020327135354D+00
      P(16)= 0.14730276362D+00
      P(17)= 0.54887167322D+00
      P(18)= 1.0440659818D+00
      P(19)= 0.68653490684D+00
      E(20)= 1.9099091D+00
      E(21)= 0.97932464D+00
      E(22)= 0.54976886D+00
      P(20)= 0.26866789D+00
      P(21)= 0.29018716D+00
      P(22)= 0.44298103D+00
      E(23)= 0.23123330D+00
      E(24)= 0.8281D+00
      E(25)= 2.768D+00
      E(26)= 10.97D+00
      D(24)= 0.508692D+00
      D(25)= 0.261010D+00
      D(26)= 0.0600080D+00
      RETURN
C
C        GALLIUM
C
  300 CONTINUE
      E(1) = 457600.0D+00
      E(2) = 68470.0D+00
      E(3) = 15590.0D+00
      E(4) = 4450.0D+00
      E(5) = 1472.0D+00
      E(6) = 541.3D+00
      E(7) = 214.8D+00
      E(8) = 88.81D+00
      S(1) = 0.00022088D+00
      S(2) = 0.0017224D+00
      S(3) = 0.0088661D+00
      S(4) = 0.035919D+00
      S(5) = 0.11446D+00
      S(6) = 0.28163D+00
      S(7) = 0.44810D+00
      S(8) = 0.29552D+00
      E(9) = 27.18D+00
      E(10)= 11.54D+00
      S(9) = 0.48025D+00
      S(10)= 1.14780D+00
      E(11)= 3.303D+00
      E(12)= 1.334D+00
      E(13)= 0.1947D+00
      E(14)= 0.07158D+00
      E(15)= 3274.0D+00
      E(16)= 765.4D+00
      E(17)= 241.6D+00
      E(18)= 89.39D+00
      E(19)= 36.36D+00
      E(20)= 15.60D+00
      P(15)= 0.0014743D+00
      P(16)= 0.013270D+00
      P(17)= 0.065384D+00
      P(18)= 0.22961D+00
      P(19)= 0.39929D+00
      P(20)= 0.43593D+00
      E(21)= 6.472D+00
      E(22)= 2.748D+00
      E(23)= 1.090D+00
      P(21)= 0.26105D+00
      P(22)= 0.48347D+00
      P(23)= 0.28721D+00
      E(24)= 0.2202D+00
      E(25)= 0.06130D+00
      E(26)= 59.66D+00
      E(27)= 17.10D+00
      E(28)= 6.030D+00
      E(29)= 2.171D+00
      E(30)= 0.6844D+00
      D(26)= 0.031960D+00
      D(27)= 0.16358D+00
      D(28)= 0.36720D+00
      D(29)= 0.45704D+00
      D(30)= 0.30477D+00
      RETURN
C
C        GERMANIUM
C
  400 CONTINUE
      E(1) = 489000.0D+00
      E(2) = 73100.0D+00
      E(3) = 16640.0D+00
      E(4) = 4742.0D+00
      E(5) = 1569.0D+00
      E(6) = 577.0D+00
      E(7) = 229.0D+00
      E(8) = 94.81D+00
      S(1) = 0.000222D+00
      S(2) = 0.001728D+00
      S(3) = 0.008950D+00
      S(4) = 0.035905D+00
      S(5) = 0.11284D+00
      S(6) = 0.28748D+00
      S(7) = 0.43449D+00
      S(8) = 0.30683D+00
      E(9) = 29.22D+00
      E(10)= 12.45D+00
      S(9) = 0.47759D+00
      S(10)= 1.11445D+00
      E(11)= 3.642D+00
      E(12)= 1.502D+00
      E(13)= 0.2462D+00
      E(14)= 0.09209D+00
      E(15)= 3596.0D+00
      E(16)= 843.7D+00
      E(17)= 266.2D+00
      E(18)= 98.28D+00
      E(19)= 39.93D+00
      E(20)= 17.14D+00
      P(15)= 0.001442D+00
      P(16)= 0.012442D+00
      P(17)= 0.064694D+00
      P(18)= 0.218665D+00
      P(19)= 0.39335D+00
      P(20)= 0.43453D+00
      E(21)= 7.157D+00
      E(22)= 3.068D+00
      E(23)= 1.246D+00
      P(21)= 0.249975D+00
      P(22)= 0.46938D+00
      P(23)= 0.28896D+00
      E(24)= 0.2795D+00
      E(25)= 0.08340D+00
      E(26)= 70.18D+00
      E(27)= 20.07D+00
      E(28)= 7.059D+00
      E(29)= 2.553D+00
      E(30)= 0.8301D+00
      D(26)= 0.02877D+00
      D(27)= 0.15525D+00
      D(28)= 0.36814D+00
      D(29)= 0.46578D+00
      D(30)= 0.28648D+00
      RETURN
C
C        ARSENIC
C
  500 CONTINUE
      E(1) = 526200.0D+00
      E(2) = 78310.0D+00
      E(3) = 17800.0D+00
      E(4) = 5070.0D+00
      E(5) = 1677.0D+00
      E(6) = 617.0D+00
      E(7) = 245.0D+00
      E(8) = 101.5D+00
      S(1) = 0.000220D+00
      S(2) = 0.00172D+00
      S(3) = 0.00889D+00
      S(4) = 0.03535D+00
      S(5) = 0.11226D+00
      S(6) = 0.28625D+00
      S(7) = 0.43406D+00
      S(8) = 0.30880D+00
      E(9) = 31.39D+00
      E(10)= 13.43D+00
      S(9) = 0.47299D+00
      S(10)= 1.12375D+00
      E(11)= 4.0D+00
      E(12)= 1.683D+00
      E(13)= 0.3003D+00
      E(14)= 0.1137D+00
      E(15)= 3909.0D+00
      E(16)= 924.0D+00
      E(17)= 291.8D+00
      E(18)= 107.5D+00
      E(19)= 43.62D+00
      E(20)= 18.73D+00
      P(15)= 0.00133D+00
      P(16)= 0.01140D+00
      P(17)= 0.05981D+00
      P(18)= 0.20511D+00
      P(19)= 0.37432D+00
      P(20)= 0.42141D+00
      E(21)= 7.841D+00
      E(22)= 3.391D+00
      E(23)= 1.405D+00
      P(21)= 0.21901D+00
      P(22)= 0.42955D+00
      P(23)= 0.26400D+00
      E(24)= 0.3441D+00
      E(25)= 0.1068D+00
      E(26)= 81.59D+00
      E(27)= 23.26D+00
      E(28)= 8.148D+00
      E(29)= 2.954D+00
      E(30)= 0.9827D+00
      D(26)= 0.02558D+00
      D(27)= 0.14492D+00
      D(28)= 0.36044D+00
      D(29)= 0.46255D+00
      D(30)= 0.26813D+00
      RETURN
C
C        SELENIUM
C
  600 CONTINUE
      E(1) = 560600.0D+00
      E(2) = 84010.0D+00
      E(3) = 19030.0D+00
      E(4) = 5419.0D+00
      E(5) = 1792.0D+00
      E(6) = 659.5D+00
      E(7) = 262.0D+00
      E(8) = 108.6D+00
      S(1) = 0.00022000D+00
      S(2) = 0.0017000D+00
      S(3) = 0.0088300D+00
      S(4) = 0.035080D+00
      S(5) = 0.11266D+00
      S(6) = 0.28471D+00
      S(7) = 0.43348D+00
      S(8) = 0.31140D+00
      E(9) = 33.66D+00
      E(10)= 14.45D+00
      S(9) = 0.46823D+00
      S(10)= 1.09261D+00
      E(11)= 4.378D+00
      E(12)= 1.876D+00
      E(13)= 0.3592D+00
      E(14)= 0.13670D+00
      E(15)= 4114.0D+00
      E(16)= 980.4D+00
      E(17)= 310.7D+00
      E(18)= 114.20D+00
      E(19)= 46.26D+00
      E(20)= 19.87D+00
      P(15)= 0.0011700D+00
      P(16)= 0.009850D+00
      P(17)= 0.051770D+00
      P(18)= 0.17869D+00
      P(19)= 0.32504D+00
      P(20)= 0.360890D+00
      E(21)= 8.309D+00
      E(22)= 3.598D+00
      E(23)= 1.522D+00
      P(21)= 0.21488D+00
      P(22)= 0.39629D+00
      P(23)= 0.22508D+00
      E(24)= 0.4032D+00
      E(25)= 0.12310D+00
      E(26)= 94.03D+00
      E(27)= 26.79D+00
      E(28)= 9.336D+00
      E(29)= 3.383D+00
      E(30)= 1.1450D+00
      D(26)= 0.020630D+00
      D(27)= 0.12227D+00
      D(28)= 0.31972D+00
      D(29)= 0.41738D+00
      D(30)= 0.23184D+00
      RETURN
C
C        BROMINE
C
  700 CONTINUE
      E(1) = 574300.0D+00
      E(2) = 89070.0D+00
      E(3) = 20210.0D+00
      E(4) = 5736.0D+00
      E(5) = 1899.0D+00
      E(6) = 698.7D+00
      E(7) = 277.8D+00
      E(8) = 115.2D+00
      S(1) = 0.00022040D+00
      S(2) = 0.0016871D+00
      S(3) = 0.0087457D+00
      S(4) = 0.035597D+00
      S(5) = 0.11352D+00
      S(6) = 0.27973D+00
      S(7) = 0.44938D+00
      S(8) = 0.30023D+00
      E(9) = 35.97D+00
      E(10)= 15.50D+00
      S(9) = 0.45726D+00
      S(10)= 1.22978D+00
      E(11)= 4.771D+00
      E(12)= 2.077D+00
      E(13)= 0.4211D+00
      E(14)= 0.1610D+00
      E(15)= 4406.0D+00
      E(16)= 1042.0D+00
      E(17)= 332.1D+00
      E(18)= 121.9D+00
      E(19)= 49.24D+00
      E(20)= 21.16D+00
      P(15)= 0.0013766D+00
      P(16)= 0.012207D+00
      P(17)= 0.060190D+00
      P(18)= 0.22370D+00
      P(19)= 0.40047D+00
      P(20)= 0.44458D+00
      E(21)= 8.836D+00
      E(22)= 3.829D+00
      E(23)= 1.643D+00
      P(21)= 0.27245D+00
      P(22)= 0.49469D+00
      P(23)= 0.25684D+00
      E(24)= 0.465D+00
      E(25)= 0.1427D+00
      E(26)= 108.4D+00
      E(27)= 30.71D+00
      E(28)= 10.66D+00
      E(29)= 3.851D+00
      E(30)= 1.317D+00
      D(26)= 0.021521D+00
      D(27)= 0.13376D+00
      D(28)= 0.36673D+00
      D(29)= 0.49037D+00
      D(30)= 0.26749D+00
      RETURN
C
C        KRYPTON
C
  800 CONTINUE
      E(1) = 605700.0D+00
      E(2) = 90300.0D+00
      E(3) = 20920.0D+00
      E(4) = 5889.0D+00
      E(5) = 1950.0D+00
      E(6) = 718.2D+00
      E(7) = 285.4D+00
      E(8) = 118.6D+00
      S(1) = 0.00022997D+00
      S(2) = 0.0017432D+00
      S(3) = 0.0090459D+00
      S(4) = 0.036901D+00
      S(5) = 0.11743D+00
      S(6) = 0.28617D+00
      S(7) = 0.45408D+00
      S(8) = 0.28501D+00
      E(9) = 38.16D+00
      E(10)= 16.45D+00
      S(9) = 0.46465D+00
      S(10)= 1.24448D+00
      E(11)= 5.211D+00
      E(12)= 2.291D+00
      E(13)= 0.4837D+00
      E(14)= 0.1855D+00
      E(15)= 4678.0D+00
      E(16)= 1120.0D+00
      E(17)= 357.1D+00
      E(18)= 131.4D+00
      E(19)= 52.86D+00
      E(20)= 22.70D+00
      P(15)= 0.0013437D+00
      P(16)= 0.011821D+00
      P(17)= 0.058277D+00
      P(18)= 0.21854D+00
      P(19)= 0.39757D+00
      P(20)= 0.44119D+00
      E(21)= 9.547D+00
      E(22)= 4.167D+00
      E(23)= 1.811D+00
      P(21)= 0.26887D+00
      P(22)= 0.49381D+00
      P(23)= 0.25069D+00
      E(24)= 0.5337D+00
      E(25)= 0.1654D+00
      E(26)= 125.6D+00
      E(27)= 35.31D+00
      E(28)= 12.15D+00
      E(29)= 4.35D+00
      E(30)= 1.494D+00
      D(26)= 0.015695D+00
      D(27)= 0.10301D+00
      D(28)= 0.29968D+00
      D(29)= 0.41171D+00
      D(30)= 0.21548D+00
      RETURN
C
C        TRANSITION METALS
C
  900 CONTINUE
      IF (MASWRK) WRITE(6,*)
     *     'THERE IS NO DZV TRANSITION METAL BASIS SET!'
      CALL BERROR(3)
      STOP
      END
C*MODULE BASEXT  *DECK N311G
      SUBROUTINE N311G(NUCZ,IGAUSS,CSINP,CPINP,IERR1,IERR2,
     *                 INTYP,NANGM,NBFS,MINF,MAXF,LOC,NGAUSS,NS)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION CSINP(*),CPINP(*),
     *          INTYP(*),NANGM(*),NBFS(*),MINF(*),MAXF(*),NS(*)
      DIMENSION E(11),SS(11),PP(11)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
C
      PARAMETER (ZERO = 0.0D+00, ONE = 1.0D+00, PT5 = 0.5D+00,
     *           PI32 = 5.56832799683170D+00, TOL = 1.0D-10)
C
C     ----- SET UP THE 6-311G BASIS SET -----
C     H-NE   N=6    R.KRISHNAN, J.S.BINKLEY, R.SEEGER, J.A.POPLE
C                   J.CHEM.PHYS. 72, 650-654(1980).
C     HE IS UNPUBLISHED, AND WAS OBTAINED FROM GAUSSIAN 82.
C
      IF(IGAUSS.NE.6) CALL BERROR(2)
      DO 100 I = 1,11
         E(I) = ZERO
         SS(I) = ZERO
         PP(I) = ZERO
  100 CONTINUE
      CALL N311XC(E,SS,PP,NUCZ)
C
C     ----- LOOP OVER 3 OR 4 SHELLS. -----
C
      IPASS = 0
      NG=0
  180 IPASS = IPASS+1
C
C     ----- H,HE -----
C
      IF (NUCZ .GT. 2) GO TO 240
      GO TO (180,190,200,220),IPASS
  190 ITYP = 1
      IGAUSS = 3
      GO TO 420
  200 ITYP = 1
      IGAUSS = 1
      GO TO 420
  220 ITYP = 1
      IGAUSS = 1
      GO TO 420
C
C     ----- LI-NE -----
C
  240 IF (NUCZ .GT. 10) CALL BERROR(2)
      GO TO (250,260,280,300),IPASS
  250 ITYP = 1
      IGAUSS = 6
      GO TO 420
  260 ITYP = 6
      IGAUSS = 3
      GO TO 420
  280 ITYP = 6
      IGAUSS = 1
      GO TO 420
  300 ITYP = 6
      IGAUSS = 1
C
  420 CONTINUE
      NSHELL = NSHELL+1
      IF(NSHELL.GT.MXSH) THEN
         IERR1=1
         RETURN
      END IF
      NS(NAT) = NS(NAT)+1
      KMIN(NSHELL) = MINF(ITYP)
      KMAX(NSHELL) = MAXF(ITYP)
      KSTART(NSHELL) = NGAUSS+1
      KATOM(NSHELL) = NAT
      KTYPE(NSHELL) = NANGM(ITYP)
      INTYP(NSHELL) = ITYP
      KNG(NSHELL) = IGAUSS
      KLOC(NSHELL) = LOC+1
      NGAUSS = NGAUSS+IGAUSS
      IF(NGAUSS.GT.MXGTOT) THEN
         IERR2=1
         RETURN
      END IF
      LOC = LOC+NBFS(ITYP)
      K1 = KSTART(NSHELL)
      K2 = K1+KNG(NSHELL)-1
      DO 440 I = 1,IGAUSS
         K = K1+I-1
         EX(K) = E(NG+I)
         CSINP(K) = SS(NG+I)
         CPINP(K) = PP(NG+I)
         CS(K) = CSINP(K)
         CP(K) = CPINP(K)
  440 CONTINUE
C
C     ----- ALWAYS UNNORMALIZE PRIMITIVES... -----
C
      DO 460 K = K1,K2
         EE = EX(K)+EX(K)
         FACS = PI32/(EE*SQRT(EE))
         FACP = PT5*FACS/EE
         CS(K) = CS(K)/SQRT(FACS)
         CP(K) = CP(K)/SQRT(FACP)
  460 CONTINUE
C
C     ----- IF(NORMF.EQ.0) NORMALIZE BASIS FUNCTIONS. -----
C
      IF (NORMF .EQ. 1) GO TO 540
      FACS = ZERO
      FACP = ZERO
      DO 510 IG = K1,K2
         DO 500 JG = K1,IG
            EE = EX(IG)+EX(JG)
            FAC = EE*SQRT(EE)
            DUMS = CS(IG)*CS(JG)/FAC
            DUMP = PT5*CP(IG)*CP(JG)/(EE*FAC)
            IF (IG .EQ. JG) GO TO 480
               DUMS = DUMS+DUMS
               DUMP = DUMP+DUMP
  480       CONTINUE
            FACS = FACS+DUMS
            FACP = FACP+DUMP
  500    CONTINUE
  510 CONTINUE
      IF (FACS .GT. TOL) FACS = ONE/SQRT(FACS*PI32)
      IF (FACP .GT. TOL) FACP = ONE/SQRT(FACP*PI32)
      DO 520 IG = K1,K2
         CS(IG) = CS(IG) * FACS
         CP(IG) = CP(IG) * FACP
         CSINP(IG) = CSINP(IG) * FACS
         CPINP(IG) = CPINP(IG) * FACP
  520 CONTINUE
C
  540 CONTINUE
      NG = NG + IGAUSS
      IF (IPASS .LT. 4) GO TO 180
      RETURN
      END
C*MODULE BASEXT  *DECK N311XC
      SUBROUTINE N311XC(E,CS,CP,IA)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION E(11), CS(11), CP(11)
C
C     PROVIDE EXPONENTS AND CONTRACTION COEFFICIENTS FOR THE 6-311G
C     BASIS SET.  THIS ROUTINE WAS TAKEN FROM GAUSSIAN 82.
C     THE H, LI-NE BASES ARE IN THE LITERATURE, HE IS NOT.
C     R. KRISHNAN, J.S.BINKLEY, R.SEEGER, J.A.POPLE
C     J.CHEM.PHYS.  72, 650-654(1980).
C
      IF(IA.LT.1.OR.IA.GT.10) CALL BERROR(2)
      GO TO (101,102,103,104,105,106,107,108,109,110), IA
C
C     HYDROGEN.
C
  101 E( 1)=+0.338650D+02
      E( 2)=+0.509479D+01
      E( 3)=+0.115879D+01
      E( 4)=+0.325840D+00
      E( 5)=+0.102741D+00
      CS(1)=+0.254938D-01
      CS(2)=+0.190373D+00
      CS(3)=+0.852161D+00
      CS(4)=1.0D+00
      CS(5)=1.0D+00
      RETURN
C
C     HELIUM.
C
  102 E( 1)=+0.981243D+02
      E( 2)=+0.147689D+02
      E( 3)=+0.331883D+01
      E( 4)=+0.874047D+00
      E( 5)=+0.244564D+00
      CS(1)=+0.287452D-01
      CS(2)=+0.208061D+00
      CS(3)=+0.837635D+00
      CS(4)=+1.0D+00
      CS(5)=+1.0D+00
      RETURN
C
C     LITHIUM.
C
  103 E( 1)=+0.900460D+03
      E( 2)=+0.134433D+03
      E( 3)=+0.304365D+02
      E( 4)=+0.862639D+01
      E( 5)=+0.248332D+01
      E( 6)=+0.303179D+00
      E( 7)=+0.486890D+01
      E( 8)=+0.856924D+00
      E( 9)=+0.243227D+00
      E(10)=+0.635070D-01
      E(11)=+0.243683D-01
      CS(1)=+0.228704D-02
      CS(2)=+0.176350D-01
      CS(3)=+0.873434D-01
      CS(4)=+0.280977D+00
      CS(5)=+0.658741D+00
      CS(6)=+0.118712D+00
      CS(7)=+0.933293D-01
      CS(8)=+0.943045D+00
      CS(9)=-0.279827D-02
      CS(10)=1.0D+00
      CS(11)=1.0D+00
      CP(7)=+0.327661D-01
      CP(8)=+0.159792D+00
      CP(9)=+0.885667D+00
      CP(10)=1.0D+00
      CP(11)=1.0D+00
      RETURN
C
C     BERYLLIUM.
C
  104 E( 1)=+0.168280D+04
      E( 2)=+0.251715D+03
      E( 3)=+0.574116D+02
      E( 4)=+0.165171D+02
      E( 5)=+0.485364D+01
      E( 6)=+0.626863D+00
      E( 7)=+0.830938D+01
      E( 8)=+0.174075D+01
      E( 9)=+0.485816D+00
      E(10)=+0.163613D+00
      E(11)=+0.567285D-01
      CS(1)=+0.228574D-02
      CS(2)=+0.175938D-01
      CS(3)=+0.863315D-01
      CS(4)=+0.281835D+00
      CS(5)=+0.640594D+00
      CS(6)=+0.144467D+00
      CS(7)=+0.108621D+00
      CS(8)=+0.927301D+00
      CS(9)=-0.297169D-02
      CS(10)=1.0D+00
      CS(11)=1.0D+00
      CP(7)=+0.361344D-01
      CP(8)=+0.216958D+00
      CP(9)=+0.841839D+00
      CP(10)=1.0D+00
      CP(11)=1.0D+00
      RETURN
C
C     BORON.
C
  105 E( 1)=+0.285889D+04
      E( 2)=+0.428140D+03
      E( 3)=+0.975282D+02
      E( 4)=+0.279693D+02
      E( 5)=+0.821577D+01
      E( 6)=+0.111278D+01
      E( 7)=+0.132415D+02
      E( 8)=+0.300166D+01
      E( 9)=+0.912856D+00
      E(10)=+0.315454D+00
      E(11)=+0.988563D-01
      CS(1)=+0.215375D-02
      CS(2)=+0.165823D-01
      CS(3)=+0.821870D-01
      CS(4)=+0.276618D+00
      CS(5)=+0.629316D+00
      CS(6)=+0.173770D+00
      CS(7)=+0.117443D+00
      CS(8)=+0.918002D+00
      CS(9)=-0.265105D-02
      CS(10)=1.0D+00
      CS(11)=1.0D+00
      CP(7)=+0.418100D-01
      CP(8)=+0.236575D+00
      CP(9)=+0.816214D+00
      CP(10)=1.0D+00
      CP(11)=1.0D+00
      RETURN
C
C     CARBON.
C
  106 E( 1)=+0.456324D+04
      E( 2)=+0.682024D+03
      E( 3)=+0.154973D+03
      E( 4)=+0.444553D+02
      E( 5)=+0.130290D+02
      E( 6)=+0.182773D+01
      E( 7)=+0.209642D+02
      E( 8)=+0.480331D+01
      E( 9)=+0.145933D+01
      E(10)=+0.483456D+00
      E(11)=+0.145585D+00
      CS(1)=+0.196665D-02
      CS(2)=+0.152306D-01
      CS(3)=+0.761269D-01
      CS(4)=+0.260801D+00
      CS(5)=+0.616462D+00
      CS(6)=+0.221006D+00
      CS(7)=+0.114660D+00
      CS(8)=+0.919999D+00
      CS(9)=-0.303068D-02
      CS(10)=1.0D+00
      CS(11)=1.0D+00
      CP(7)=+0.402487D-01
      CP(8)=+0.237594D+00
      CP(9)=+0.815854D+00
      CP(10)=1.0D+00
      CP(11)=1.0D+00
      RETURN
C
C     NITROGEN.
C
  107 E( 1)=+0.629348D+04
      E( 2)=+0.949044D+03
      E( 3)=+0.218776D+03
      E( 4)=+0.636916D+02
      E( 5)=+0.188282D+02
      E( 6)=+0.272023D+01
      E( 7)=+0.306331D+02
      E( 8)=+0.702614D+01
      E( 9)=+0.211205D+01
      E(10)=+0.684009D+00
      E(11)=+0.200878D+00
      CS(1)=+0.196979D-02
      CS(2)=+0.149613D-01
      CS(3)=+0.735006D-01
      CS(4)=+0.248937D+00
      CS(5)=+0.602460D+00
      CS(6)=+0.256202D+00
      CS(7)=+0.111906D+00
      CS(8)=+0.921666D+00
      CS(9)=-0.256919D-02
      CS(10)=1.0D+00
      CS(11)=1.0D+00
      CP(7)=+0.383119D-01
      CP(8)=+0.237403D+00
      CP(9)=+0.817592D+00
      CP(10)=1.0D+00
      CP(11)=1.0D+00
      RETURN
C
C     OXYGEN.
C
  108 E( 1)=+0.858850D+04
      E( 2)=+0.129723D+04
      E( 3)=+0.299296D+03
      E( 4)=+0.873771D+02
      E( 5)=+0.256789D+02
      E( 6)=+0.374004D+01
      E( 7)=+0.421175D+02
      E( 8)=+0.962837D+01
      E( 9)=+0.285332D+01
      E(10)=+0.905661D+00
      E(11)=+0.255611D+00
      CS(1)=+0.189515D-02
      CS(2)=+0.143859D-01
      CS(3)=+0.707320D-01
      CS(4)=+0.240001D+00
      CS(5)=+0.594797D+00
      CS(6)=+0.280802D+00
      CS(7)=+0.113889D+00
      CS(8)=+0.920811D+00
      CS(9)=-0.327447D-02
      CS(10)=1.0D+00
      CS(11)=1.0D+00
      CP(7)=+0.365114D-01
      CP(8)=+0.237153D+00
      CP(9)=+0.819702D+00
      CP(10)=1.0D+00
      CP(11)=1.0D+00
      RETURN
C
C     FLUORINE.
C
  109 E( 1)=+0.114271D+05
      E( 2)=+0.172235D+04
      E( 3)=+0.395746D+03
      E( 4)=+0.115139D+03
      E( 5)=+0.336026D+02
      E( 6)=+0.491901D+01
      E( 7)=+0.554441D+02
      E( 8)=+0.126323D+02
      E( 9)=+0.371756D+01
      E(10)=+0.116545D+01
      E(11)=+0.321892D+00
      CS(1)=+0.180093D-02
      CS(2)=+0.137419D-01
      CS(3)=+0.681334D-01
      CS(4)=+0.233325D+00
      CS(5)=+0.589086D+00
      CS(6)=+0.299505D+00
      CS(7)=+0.114536D+00
      CS(8)=+0.920512D+00
      CS(9)=-0.337804D-02
      CS(10)=1.0D+00
      CS(11)=1.0D+00
      CP(7)=+0.354609D-01
      CP(8)=+0.237451D+00
      CP(9)=+0.820458D+00
      CP(10)=1.0D+00
      CP(11)=1.0D+00
      RETURN
C
C     NEON.
C
  110 E( 1)=+0.139957D+05
      E( 2)=+0.211710D+04
      E( 3)=+0.490425D+03
      E( 4)=+0.143833D+03
      E( 5)=+0.419265D+02
      E( 6)=+0.615684D+01
      E( 7)=+0.691211D+02
      E( 8)=+0.158350D+02
      E( 9)=+0.467326D+01
      E(10)=+0.145756D+01
      E(11)=+0.397057D+00
      CS(1)=+0.183276D-02
      CS(2)=+0.138827D-01
      CS(3)=+0.680687D-01
      CS(4)=+0.231328D+00
      CS(5)=+0.585890D+00
      CS(6)=+0.305883D+00
      CS(7)=+0.119149D+00
      CS(8)=+0.917375D+00
      CS(9)=-0.405839D-02
      CS(10)=1.0D+00
      CS(11)=1.0D+00
      CP(7)=+0.356574D-01
      CP(8)=+0.239477D+00
      CP(9)=+0.818461D+00
      CP(10)=1.0D+00
      CP(11)=1.0D+00
      RETURN
      END
C*MODULE BASEXT  *DECK TZVBAS
      SUBROUTINE TZVBAS(NUCZ,CSINP,CPINP,CDINP,SCFAC,IERR1,IERR2,
     *                  INTYP,NANGM,NBFS,MINF,MAXF,LOC,NGAUSS,NS,POL)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL POL
C
      DIMENSION CSINP(*),CPINP(*),CDINP(*),SCFAC(*),
     *          INTYP(*),NANGM(*),NBFS(*),MINF(*),MAXF(*),NS(*)
      DIMENSION EEX(31),CCS(31),CCP(31),CCD(31)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, PT5=0.5D+00, PT75=0.75D+00)
      PARAMETER (PI32=5.56832799683170D+00, TOL=1.0D-10)
      DATA SCALH1,SCALH2,SCALH3/1.00D+00,1.00D+00,1.00D+00/
C
C     ----- SET UP TRIPLE ZETA QUALITY BASIS SET -----
C     A.D.MCLEAN, G.S.CHANDLER   J.CHEM.PHYS. 72,5639-5648(1980)
C         (6S,5P) CONTRACTIONS FOR NA-AR
C
      DO 100 I = 1,31
      EEX(I) = ZERO
      CCS(I) = ZERO
      CCP(I) = ZERO
  100 CCD(I) = ZERO
      MXPASS = 20
      IF(POL) MXPASS=21
C
C     ----- HYDROGEN -----
C
      IF (NUCZ .GT. 2) GO TO 120
      CALL TZV001(EEX,CCS,CCP,NUCZ)
      IF (SCFAC(1) .LE. ZERO) SCFAC(1) = SCALH1
      IF (SCFAC(2) .LE. ZERO) SCFAC(2) = SCALH2
      IF (SCFAC(3) .LE. ZERO) SCFAC(3) = SCALH3
      EEX(1) = EEX(1)*SCFAC(1)**2
      EEX(2) = EEX(2)*SCFAC(1)**2
      EEX(3) = EEX(3)*SCFAC(1)**2
      EEX(4) = EEX(4)*SCFAC(2)**2
      EEX(5) = EEX(5)*SCFAC(3)**2
      GO TO 160
C
C     ----- LITHIUM TO NEON -----
C
  120 IF (NUCZ .GT. 10) GO TO 140
      CALL TZV002(EEX,CCS,CCP,CCD,NUCZ)
      GO TO 160
C
C     ----- SODIUM TO ARGON ------------
C
  140 IF (NUCZ .GT. 18) GO TO 150
      CALL TZV003(EEX,CCS,CCP,CCD,NUCZ)
      GO TO 160
C
C     ----- POTASSIUM TO ZINC
C
  150 IF(NUCZ.GT.30) CALL BERROR(4)
      MXPASS=21
      CALL TZVTM1(EEX,CCS,CCP,CCD,NUCZ)
C
  160 CONTINUE
C
C     ----- LOOP OVER 3,8, OR 11 SHELLS -----  SP BASIS SETS
C     ----- LOOP OVER 4,9,12 OR 21 SHELLS ----- SPD BASIS SETS
C
      IPASS = 0
  180 IPASS = IPASS+1
      IF (NUCZ .GT. 2) GO TO 240
C
C     ----- H -----
C
      GO TO (180,180,180,180,180,180,180,180,180,
     1 180,180,180,180,180,180,180,180,200,220,230,235),IPASS
  200 ITYP = 1
      IGAUSS = 3
      IG = 0
      GO TO 700
  220 ITYP = 1
      IGAUSS = 1
      IG = 3
      GO TO 700
  230 ITYP = 1
      IGAUSS = 1
      IG = 4
      GO TO 700
  235 ITYP = 2
      IGAUSS = 1
      IG = 5
      GO TO 700
  240 IF (NUCZ .GT. 3) GO TO 360
C
C     ----- LI  ---
C
      GO TO (180,180,180,180,180,180,180,180,180,
     1 180,180,180,180,260,280,300,305,310,320,340),IPASS
  260 ITYP = 1
      IGAUSS = 7
      IG = 0
      GO TO 700
  280 ITYP = 1
      IGAUSS = 1
      IG = 7
      GO TO 700
  300 ITYP = 1
      IGAUSS = 1
      IG = 8
      GO TO 700
  305 ITYP = 1
      IGAUSS = 1
      IG = 9 
      GO TO 700
  310 ITYP = 2
      IGAUSS = 1
      IG = 10
      GO TO 700
  320 ITYP = 2
      IGAUSS = 1
      IG = 11
      GO TO 700
  340 ITYP = 2
      IGAUSS = 1
      IG = 12
      GO TO 700
  360 IF (NUCZ .GT. 4) GO TO 361
C
C     -----  BE ---
C
      GO TO (180,180,180,180,180,180,180,180,180,
     1 180,180,180,261,281,301,306,311,321,341,346,351),IPASS
  261 ITYP = 1
      IGAUSS = 6
      IG = 0
      GO TO 700
  281 ITYP = 1
      IGAUSS = 2
      IG = 6
      GO TO 700
  301 ITYP = 1
      IGAUSS = 1
      IG = 8
      GO TO 700
  306 ITYP = 1
      IGAUSS = 1
      IG = 9
      GO TO 700
  311 ITYP = 1
      IGAUSS = 1
      IG = 10
      GO TO 700
  321 ITYP = 2
      IGAUSS = 3
      IG = 11
      GO TO 700
  341 ITYP = 2
      IGAUSS = 1
      IG = 14
      GO TO 700
  346 ITYP = 2
      IGAUSS = 1
      IG = 15
      GO TO 700
  351 ITYP = 3
      IGAUSS = 1
      IG = 16
      GO TO 700
  361 IF (NUCZ .GT. 10) GO TO 480
C
C     ----- B - TO - NE -----
C
      GO TO (180,180,180,180,180,180,180,180,180,
     1 180,180,180,380,400,420,430,435,440,460,470,475),IPASS
  380 ITYP = 1
      IGAUSS = 6
      IG = 0
      GO TO 700
  400 ITYP = 1
      IGAUSS = 2
      IG = 6
      GO TO 700
  420 ITYP = 1
      IGAUSS = 1
      IG = 8
      GO TO 700
  430 ITYP = 1
      IGAUSS = 1
      IG = 9
      GO TO 700
  435 ITYP = 1
      IGAUSS = 1
      IG = 10
      GO TO 700
  440 ITYP = 2
      IGAUSS = 4
      IG = 11
      GO TO 700
  460 ITYP = 2
      IGAUSS = 1
      IG = 15
      GO TO 700
  470 ITYP = 2
      IGAUSS = 1
      IG = 16
      GO TO 700
  475 ITYP = 3
      IGAUSS = 1
      IG = 17
      GO TO 700
  480 IF (NUCZ .GT. 18) GO TO 961
      IF (NUCZ .GT. 14) GO TO 900
C
C     ----- NA TO SI    -----
C
      GO TO (180,180,180,180,180,180,180,180,180,
     1 500,520,540,560,580,600,620,640,660,680,690,695),IPASS
  500 ITYP = 1
      IGAUSS = 6
      IG = 0
      GO TO 700
  520 ITYP = 1
      IGAUSS = 3
      IG = 6
      GO TO 700
  540 ITYP = 1
      IGAUSS = 1
      IG = 9
      GO TO 700
  560 ITYP = 1
      IGAUSS = 1
      IG = 10
      GO TO 700
  580 ITYP = 1
      IGAUSS = 1
      IG = 11
      GO TO 700
  600 ITYP = 1
      IGAUSS = 1
      IG = 12
      GO TO 700
  620 ITYP = 2
      IGAUSS = 4
      IG = 13
      GO TO 700
  640 ITYP = 2
      IGAUSS = 2
      IG = 17
      GO TO 700
  660 ITYP = 2
      IGAUSS = 1
      IG = 19
      GO TO 700
  680 ITYP = 2
      IGAUSS = 1
      IG = 20
      GO TO 700
  690 ITYP = 2
      IGAUSS = 1
      IG = 21
      GO TO 700
  695 ITYP = 3
      IGAUSS = 1
      IG = 22
      GO TO 700
C
C     ----- P TO AR ----
C
  900 GO TO (180,180,180,180,180,180,180,180,180,
     1 500,520,540,560,580,600,910,920,930,940,950,955),IPASS
  910 ITYP = 2
      IGAUSS = 5
      IG = 13
      GO TO 700
  920 ITYP = 2
      IGAUSS = 2
      IG = 18
      GO TO 700
  930 ITYP = 2
      IGAUSS = 1
      IG = 20
      GO TO 700
  940 ITYP = 2
      IGAUSS = 1
      IG = 21
      GO TO 700
  950 ITYP = 2
      IGAUSS = 1
      IG = 22
      GO TO 700
  955 ITYP = 3
      IGAUSS = 1
      IG = 23
      GO TO 700
  961 IF(NUCZ.GT.20) GOTO 960
C
C     ----- K TO CA ----
C
      GO TO ( 180, 180, 180, 180, 180, 180, 180, 180, 180,
     1       1001,1011,1021,1031,1041,1051,1061,1071,1101,
     2       1111,1121,1131), IPASS
 1001 ITYP=1
      IGAUSS=6
      IG=0
      GO TO 700
 1011 ITYP=1
      IGAUSS=2
      IG=6
      GO TO 700
 1021 ITYP=1
      IGAUSS=1
      IG=8
      GO TO 700
 1031 ITYP=1
      IGAUSS=1
      IG=9
      GO TO 700
 1041 ITYP=1
      IGAUSS=1
      IG=10
      GO TO 700
 1051 ITYP=1
      IGAUSS=1
      IG=11
      GO TO 700
 1061 ITYP=1
      IGAUSS=1
      IG=12
      GO TO 700
 1071 ITYP=1
      IGAUSS=1
      IG=13
      GO TO 700
 1101 ITYP=2
      IGAUSS=3
      IG=14
      GO TO 700
 1111 ITYP=2
      IGAUSS=3
      IG=17
      GO TO 700
 1121 ITYP=2
      IGAUSS=1
      IG=20
      GO TO 700
 1131 ITYP=2
      IGAUSS=2
      IG=21
      GO TO 700
C
C     ----- SC TO ZN ----
C
  960 GO TO (1000,1010,1020,1030,1040,1050,1060,1070,1080,
     1       1090,1100,1110,1120,1130,1140,1150,1160,1170,
     2       1180,1190,1200), IPASS
 1000 ITYP=1
      IGAUSS=5
      IG=0
      GO TO 700
 1010 ITYP=1
      IGAUSS=1
      IG=5
      GO TO 700
 1020 ITYP=1
      IGAUSS=1
      IG=6
      GO TO 700
 1030 ITYP=1
      IGAUSS=1
      IG=7
      GO TO 700
 1040 ITYP=1
      IGAUSS=1
      IG=8
      GO TO 700
 1050 ITYP=1
      IGAUSS=1
      IG=9
      GO TO 700
 1060 ITYP=1
      IGAUSS=1
      IG=10
      GO TO 700
 1070 ITYP=1
      IGAUSS=1
      IG=11
      GO TO 700
 1080 ITYP=1
      IGAUSS=1
      IG=12
      GO TO 700
 1090 ITYP=1
      IGAUSS=1
      IG=13
      GO TO 700
 1100 ITYP=2
      IGAUSS=4
      IG=14
      GO TO 700
 1110 ITYP=2
      IGAUSS=1
      IG=18
      GO TO 700
 1120 ITYP=2
      IGAUSS=1
      IG=19
      GO TO 700
 1130 ITYP=2
      IGAUSS=1
      IG=20
      GO TO 700
 1140 ITYP=2
      IGAUSS=1
      IG=21
      GO TO 700
 1150 ITYP=2
      IGAUSS=1
      IG=22
      GO TO 700
 1160 ITYP=2
      IGAUSS=1
      IG=23
      GO TO 700
 1170 ITYP=2
      IGAUSS=1
      IG=24
      GO TO 700
 1180 ITYP=3
      IGAUSS=4
      IG=25
      GO TO 700
 1190 ITYP=3
      IGAUSS=1
      IG=29
      GO TO 700
 1200 ITYP=3
      IGAUSS=1
      IG=30
C
  700 CONTINUE
      NSHELL = NSHELL+1
      IF(NSHELL.GT.MXSH) THEN
         IERR1=1
         RETURN
      END IF
      NS(NAT) = NS(NAT)+1
      KMIN(NSHELL) = MINF(ITYP)
      KMAX(NSHELL) = MAXF(ITYP)
      KSTART(NSHELL) = NGAUSS+1
      KATOM(NSHELL) = NAT
      KTYPE(NSHELL) = NANGM(ITYP)
      INTYP(NSHELL) = ITYP
      KNG(NSHELL) = IGAUSS
      KLOC(NSHELL) = LOC+1
      NGAUSS = NGAUSS+IGAUSS
      IF(NGAUSS.GT.MXGTOT) THEN
         IERR2=1
         RETURN
      END IF
      LOC = LOC+NBFS(ITYP)
      K1 = KSTART(NSHELL)
      K2 = K1+KNG(NSHELL)-1
      DO 720 I = 1,IGAUSS
      K = K1+I-1
      EX(K) = EEX(IG+I)
      CSINP(K) = CCS(IG+I)
      CPINP(K) = CCP(IG+I)
      CDINP(K) = CCD(IG+I)
      CS(K) = CSINP(K)
      CP(K) = CPINP(K)
  720 CD(K) = CDINP(K)
C
C     ----- ALWAYS UNNORMALIZE PRIMITIVES -----
C
      DO 740 K = K1,K2
      EE = EX(K)+EX(K)
      FACS = PI32/(EE* SQRT(EE))
      FACP = PT5*FACS/EE
      FACD = PT75*FACS/(EE*EE)
      CS(K) = CS(K)/ SQRT(FACS)
      CP(K) = CP(K)/ SQRT(FACP)
  740 CD(K) = CD(K)/ SQRT(FACD)
C
C     ----- IF(NORMF.EQ.0) NORMALIZE BASIS FUNCTIONS -----
C
      IF (NORMF .EQ. 1) GO TO 820
      FACS = ZERO
      FACP = ZERO
      FACD = ZERO
      DO 780 IG = K1,K2
      DO 780 JG = K1,IG
      EE = EX(IG)+EX(JG)
      FAC = EE*SQRT(EE)
      DUMS = CS(IG)*CS(JG)/FAC
      DUMP = PT5*CP(IG)*CP(JG)/(EE*FAC)
      DUMD = PT75*CD(IG)*CD(JG)/(EE*EE*FAC)
      IF (IG .EQ. JG) GO TO 760
      DUMS = DUMS+DUMS
      DUMP = DUMP+DUMP
      DUMD = DUMD+DUMD
  760 FACS = FACS+DUMS
      FACP = FACP+DUMP
      FACD = FACD+DUMD
  780 CONTINUE
      IF (FACS .GT. TOL) FACS = ONE/SQRT(FACS*PI32)
      IF (FACP .GT. TOL) FACP = ONE/SQRT(FACP*PI32)
      IF (FACD .GT. TOL) FACD = ONE/SQRT(FACD*PI32)
      DO 800 IG = K1,K2
         CS(IG) = CS(IG) * FACS
         CP(IG) = CP(IG) * FACP
         CD(IG) = CD(IG) * FACD
         CSINP(IG) = CSINP(IG) * FACS
         CPINP(IG) = CPINP(IG) * FACP
         CDINP(IG) = CDINP(IG) * FACD
  800 CONTINUE
  820 CONTINUE
C
      IF (IPASS .LT. MXPASS) GO TO 180
      RETURN
      END
C*MODULE BASEXT  *DECK TZV001
      SUBROUTINE TZV001(E,S,P,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION E(6),S(6),P(6)
C
C     ----- DUNNING'S CONTRACTION OF HUZINAGA'S (5S) -----
C
C     ----- HYDROGEN (5S)/(3S) -----
C
      GO TO (100,120),N
C
C     ----- H -----
C
  100 CONTINUE
      E(1) = 33.64D+00
      S(1) = 0.025374D+00
      E(2) = 5.058D+00
      S(2) = 0.189684D+00
      E(3) = 1.147D+00
      S(3) = 0.852933D+00
      E(4) = 0.3211D+00
      S(4) = 1.0D+00
      E(5) = 0.1013D+00
      S(5) = 1.0D+00
      E(6) = 1.0D+00
      P(6) = 1.0D+00
      RETURN
C
C     ----- HE -----
C
  120 CONTINUE
      CALL BERROR(4)
      RETURN
      END
C*MODULE BASEXT  *DECK TZV002
      SUBROUTINE TZV002(E,S,P,D,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION E(18),S(18),P(18),D(18)
C
C     ----- DUNNING'S CONTRACTION OF HUZINAGA'S (10S,6P) ----
C
      NN = N-2
      GO TO (100,120,140,160,180,200,220,240),NN
C
C     ----- LI -----
C     THE S BASIS IS FROM DUNNING, THE P BASIS IS FROM MIKE SCHMIDT
C
  100 CONTINUE
      E(1) = 1783.0D+00
      E(2) = 267.1D+00
      E(3) = 60.07D+00
      E(4) = 16.78D+00
      E(5) = 5.403D+00
      E(6) = 1.906D+00
      E(7) = 0.2634D+00
      S(1) = 0.000824D+00
      S(2) = 0.006403D+00
      S(3) = 0.033239D+00
      S(4) = 0.126621D+00
      S(5) = 0.337749D+00
      S(6) = 0.575669D+00
      S(7) = 0.062311D+00
      E(8) = 0.7179D+00
      S(8) = 1.0D+00
      E(9) = 0.07716D+00
      S(9) = 1.0D+00
      E(10) = 0.02854D+00
      S(10) = 1.0D+00
      E(11) = 1.0254D+00
      P(11) = 1.0D+00      
      E(12) = 0.2637D+00
      P(12) = 1.0D+00      
      E(13) = 0.0678D+00
      P(13) = 1.0D+00      
      RETURN
C
C     ----- BE -----
C
  120 CONTINUE
      E(1) = 3630.0D+00
      S(1) = 0.000839D+00
      E(2) = 532.3D+00
      S(2) = 0.006735D+00
      E(3) = 117.8D+00
      S(3) = 0.035726D+00
      E(4) = 32.66D+00
      S(4) = 0.138635D+00
      E(5) = 10.48D+00
      S(5) = 0.385399D+00
      E(6) = 3.668D+00
      S(6) = 0.547688D+00
      E(7) = 3.668D+00
      S(7) = 0.213406D+00
      E(8) = 1.354D+00
      S(8) = 0.814692D+00
      E(9) = 0.3890D+00
      S(9) = 1.0D+00
      E(10) = 0.1502D+00
      S(10) = 1.0D+00
      E(11) = 0.05241D+00
      S(11) = 1.0D+00
      E(12) = 3.202D+00
      P(12) = 0.052912D+00
      E(13) = 0.6923D+00
      P(13) = 0.267659D+00
      E(14) = 0.2016D+00
      P(14) = 0.792085D+00
      E(15) = 0.1183D+00
      P(15) = 1.0D+00
      E(16) = 0.0694D+00
      P(16) = 1.0D+00
      E(17) = 0.32D+00
      D(17) = 1.0D+00
      RETURN
C
C     ----- B  -----
C
  140 CONTINUE
      E(1) = 6250.0D+00
      S(1) = 0.000798D+00
      E(2) = 916.1D+00
      S(2) = 0.006410D+00
      E(3) = 202.2D+00
      S(3) = 0.034299D+00
      E(4) = 55.83D+00
      S(4) = 0.135487D+00
      E(5) = 17.86D+00
      S(5) = 0.388532D+00
      E(6) = 6.253D+00
      S(6) = 0.547758D+00
      E(7) = 6.253D+00
      S(7) = 0.232643D+00
      E(8) = 2.312D+00
      S(8) = 0.797219D+00
      E(9) = 0.6824D+00
      S(9) = 1.0D+00
      E(10) = 0.2604D+00
      S(10) = 1.0D+00
      E(11) = 0.0894D+00
      S(11) = 1.0D+00
      E(12) = 15.46D+00
      P(12) = 0.016822D+00
      E(13) = 3.483D+00
      P(13) = 0.100878D+00
      E(14) = 1.066D+00
      P(14) = 0.336895D+00
      E(15) = 0.3928D+00
      P(15) = 0.672317D+00
      E(16) = 0.1503D+00
      P(16) = 1.0D+00
      E(17) = 0.05722D+00
      P(17) = 1.0D+00
      E(18) = 0.50D+00
      D(18) = 1.0D+00
      RETURN
C
C     ----- C  -----
C
  160 CONTINUE
      E(1) = 9471.0D+00
      S(1) = 0.000776D+00
      E(2) = 1398.0D+00
      S(2) = 0.006218D+00
      E(3) = 307.5D+00
      S(3) = 0.033575D+00
      E(4) = 84.54D+00
      S(4) = 0.134278D+00
      E(5) = 26.91D+00
      S(5) = 0.393668D+00
      E(6) = 9.409D+00
      S(6) = 0.544169D+00
      E(7) = 9.409D+00
      S(7) = 0.248075D+00
      E(8) = 3.500D+00
      S(8) = 0.782844D+00
      E(9) = 1.068D+00
      S(9) = 1.0D+00
      E(10) = 0.4002D+00
      S(10) = 1.0D+00
      E(11) = 0.1351D+00
      S(11) = 1.0D+00
      E(12) = 25.37D+00
      P(12) = 0.016295D+00
      E(13) = 5.776D+00
      P(13) = 0.102098D+00
      E(14) = 1.787D+00
      P(14) = 0.340228D+00
      E(15) = 0.6577D+00
      P(15) = 0.668269D+00
      E(16) = 0.2480D+00
      P(16) = 1.0D+00
      E(17) = 0.09106D+00
      P(17) = 1.0D+00
      E(18) = 0.72D+00
      D(18) = 1.0D+00
      RETURN
C
C     ----- N -----
C
  180 CONTINUE
      E(1) = 13520.0D+00
      S(1) = 0.000760D+00
      E(2) = 1999.0D+00
      S(2) = 0.006076D+00
      E(3) = 440.0D+00
      S(3) = 0.032847D+00
      E(4) = 120.9D+00
      S(4) = 0.132396D+00
      E(5) = 38.470D+00
      S(5) = 0.393261D+00
      E(6) = 13.46D+00
      S(6) = 0.546339D+00
      E(7) = 13.46D+00
      S(7) = 0.252036D+00
      E(8) = 4.993D+00
      S(8) = 0.779385D+00
      E(9) = 1.569D+00
      S(9) = 1.0D+00
      E(10) = 0.5800D+00
      S(10) = 1.0D+00
      E(11) = 0.1923D+00
      S(11) = 1.0D+00
      E(12) = 35.91D+00
      P(12) = 0.016916D+00
      E(13) = 8.480D+00
      P(13) = 0.102200D+00
      E(14) = 2.706D+00
      P(14) = 0.338134D+00
      E(15) = 0.9921D+00
      P(15) = 0.669281D+00
      E(16) = 0.3727D+00
      P(16) = 1.0D+00
      E(17) = 0.1346D+00
      P(17) = 1.0D+00
      E(18) = 0.98D+00
      D(18) = 1.0D+00
      RETURN
C
C     ----- O  ------
C
  200 CONTINUE
      E(1) = 18050.0D+00
      S(1) = 0.000757D+00
      E(2) = 2660.0D+00
      S(2) = 0.006066D+00
      E(3) = 585.7D+00
      S(3) = 0.032782D+00
      E(4) = 160.9D+00
      S(4) = 0.132609D+00
      E(5) = 51.160D+00
      S(5) = 0.396839D+00
      E(6) = 17.90D+00
      S(6) = 0.542572D+00
      E(7) = 17.90D+00
      S(7) = 0.262490D+00
      E(8) = 6.639D+00
      S(8) = 0.769828D+00
      E(9) = 2.077D+00
      S(9) = 1.0D+00
      E(10) = 0.7736D+00
      S(10) = 1.0D+00
      E(11) = 0.2558D+00
      S(11) = 1.0D+00
      E(12) = 49.83D+00
      P(12) = 0.016358D+00
      E(13) = 11.49D+00
      P(13) = 0.106453D+00
      E(14) = 3.609D+00
      P(14) = 0.349302D+00
      E(15) = 1.321D+00
      P(15) = 0.657183D+00
      E(16) = 0.4821D+00
      P(16) = 1.0D+00
      E(17) = 0.1651D+00
      P(17) = 1.0D+00
      E(18) = 1.28D+00
      D(18) = 1.0D+00
      RETURN
C
C     ----- F  -----
C
  220 CONTINUE
      E(1) = 23340.0D+00
      S(1) = 0.000757D+00
      E(2) = 3431.0D+00
      S(2) = 0.006081D+00
      E(3) = 757.7D+00
      S(3) = 0.032636D+00
      E(4) = 209.2D+00
      S(4) = 0.131704D+00
      E(5) = 66.73D+00
      S(5) = 0.396240D+00
      E(6) = 23.37D+00
      S(6) = 0.543672D+00
      E(7) = 23.37D+00
      S(7) = 0.264893D+00
      E(8) = 8.624D+00
      S(8) = 0.767925D+00
      E(9) = 2.692D+00
      S(9) = 1.0D+00
      E(10) = 1.009D+00
      S(10) = 1.0D+00
      E(11) = 0.3312D+00
      S(11) = 1.0D+00
      E(12) = 65.66D+00
      P(12) = 0.016037D+00
      E(13) = 15.22D+00
      P(13) = 0.105697D+00
      E(14) = 4.788D+00
      P(14) = 0.350227D+00
      E(15) = 1.732D+00
      P(15) = 0.658195D+00
      E(16) = 0.6206D+00
      P(16) = 1.0D+00
      E(17) = 0.2070D+00
      P(17) = 1.0D+00
      E(18) = 1.62D+00
      D(18) = 1.0D+00
      RETURN
C
C     ----- NE -----
C
  240 CONTINUE
      E(1) = 28660.0D+00
      S(1) = 0.000767D+00
      E(2) = 4263.0D+00
      S(2) = 0.006068D+00
      E(3) = 946.8D+00
      S(3) = 0.032474D+00
      E(4) = 261.5D+00
      S(4) = 0.131468D+00
      E(5) = 83.34D+00
      S(5) = 0.397723D+00
      E(6) = 29.17D+00
      S(6) = 0.542491D+00
      E(7) = 29.17D+00
      S(7) = 0.269065D+00
      E(8) = 10.76D+00
      S(8) = 0.764121D+00
      E(9) = 3.343D+00
      S(9) = 1.0D+00
      E(10) = 1.241D+00
      S(10) = 1.0D+00
      E(11) = 0.4063D+00
      S(11) = 1.0D+00
      E(12) = 84.84D+00
      P(12) = 0.015550D+00
      E(13) = 19.71D+00
      P(13) = 0.103011D+00
      E(14) = 6.219D+00
      P(14) = 0.349215D+00
      E(15) = 2.211D+00
      P(15) = 0.662839D+00
      E(16) = 0.7853D+00
      P(16) = 1.0D+00
      E(17) = 0.2566D+00
      P(17) = 1.0D+00
      E(18) = 2.0D+00
      D(18) = 1.0D+00
      RETURN
      END
C*MODULE BASEXT  *DECK TZV003
      SUBROUTINE TZV003(E,S,P,D,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     MACLEAN/CHANDLER BASIS SETS.  NA-AR ONLY
C     J.CHEM.PHYS. 72, 5639, (1980)
C     ONLY THE LARGEST BASIS IS HERE. THIS CONTAINS (12S,9P)
C     PRIMITIVES, CONTRACTED TO (6S,5P).  1S PRIMITIVE (AND 1P
C     PRIMITIVE FOR P-AR) ARE SHARED BY TWO CONTRACTIONS, SO
C     THE EFFECTIVE PRIMITIVE BASIS IS (13S,9P) OR (13S,10P)
C
      DIMENSION E(24),S(24),P(24),D(24)
      NN = N-10
      GO TO (100,120,140,160,180,200,220,240),NN
C
C     ----- NA ----
C
100   CONTINUE
      E(1) = 36166.4D+00
      S(1) = 0.001032D+00
      E(2) = 5372.58D+00
      S(2) = 0.008071D+00
      E(3) = 1213.21D+00
      S(3) = 0.042129D+00
      E(4) = 339.62D+00
      S(4) = 0.169789D+00
      E(5) = 109.55D+00
      S(5) = 0.514621D+00
      E(6) = 38.777D+00
      S(6) = 0.379817D+00
      E(7) = 38.777D+00
      S(7) = 0.374762D+00
      E(8) = 14.576D+00
      S(8) = 0.575769D+00
      E(9) = 5.2699D+00
      S(9) = 0.112933D+00
      E(10) = 1.8278D+00
      S(10) = 1.0D+00
      E(11) = 0.6199D+00
      S(11) = 1.0D+00
      E(12) = 0.0572D+00
      S(12) = 1.0D+00
      E(13) = 0.0240D+00
      S(13) = 1.0D+00
      E(14) = 144.645D+00
      P(14) = 0.011485D+00
      E(15) = 33.907D+00
      P(15) = 0.082383D+00
      E(16) = 10.629D+00
      P(16) = 0.319658D+00
      E(17) = 3.8239D+00
      P(17) = 0.701295D+00
      E(18) = 1.4443D+00
      P(18) = 0.638506D+00
      E(19) = 0.5526D+00
      P(19) = 0.425365D+00
      E(20) = 0.1887D+00
      P(20) = 1.0D+00
      E(21) = 0.0465D+00
      P(21) = 1.0D+00
      E(22) = 0.0163D+00
      P(22) = 1.0D+00
      E(23) = 0.157D+00
      D(23) = 1.0D+00
      RETURN
C
C     ----- MG ----
C
 120  CONTINUE
      E(1) = 43866.5D+00
      S(1) = 0.000918D+00
      E(2) = 6605.37D+00
      S(2) = 0.007047D+00
      E(3) = 1513.26D+00
      S(3) = 0.035941D+00
      E(4) = 432.317D+00
      S(4) = 0.141461D+00
      E(5) = 142.149D+00
      S(5) = 0.426764D+00
      E(6) = 51.398D+00
      S(6) = 0.497975D+00
      E(7) = 51.398D+00
      S(7) = 0.251355D+00
      E(8) = 19.920D+00
      S(8) = 0.618671D+00
      E(9) = 8.0247D+00
      S(9) = 0.188417D+00
      E(10) = 2.5082D+00
      S(10) = 1.0D+00
      E(11) = 0.8715D+00
      S(11) = 1.0D+00
      E(12) = 0.1082D+00
      S(12) = 1.0D+00
      E(13) = 0.0401D+00
      S(13) = 1.0D+00
      E(14) = 193.854D+00
      P(14) = 0.010188D+00
      E(15) = 45.442D+00
      P(15) = 0.075360D+00
      E(16) = 14.186D+00
      P(16) = 0.307419D+00
      E(17) = 5.0575D+00
      P(17) = 0.717575D+00
      E(18) = 1.8886D+00
      P(18) = 0.667339D+00
      E(19) = 0.7227D+00
      P(19) = 0.394649D+00
      E(20) = 0.2364D+00
      P(20) = 1.0D+00
      E(21) = 0.0934D+00
      P(21) = 1.0D+00
      E(22) = 0.0348D+00
      P(22) = 1.0D+00
      E(23) = 0.234D+00
      D(23) = 1.0D+00
      RETURN
C
C     ----- AL -----
C
  140 CONTINUE
      E(1) = 54866.49D+00
      S(1) = 0.000839D+00
      E(2) = 8211.77D+00
      S(2) = 0.006527D+00
      E(3) = 1866.18D+00
      S(3) = 0.033666D+00
      E(4) = 531.129D+00
      S(4) = 0.132902D+00
      E(5) = 175.118D+00
      S(5) = 0.401266D+00
      E(6) = 64.0055D+00
      S(6) = 0.531338D+00
      E(7) = 64.0055D+00
      S(7) = 0.202305D+00
      E(8) = 25.2925D+00
      S(8) = 0.624790D+00
      E(9) = 10.5349D+00
      S(9) = 0.227439D+00
      E(10) = 3.2067D+00
      S(10) = 1.0D+00
      E(11) = 1.1526D+00
      S(11) = 1.0D+00
      E(12) = 0.1767D+00
      S(12) = 1.0D+00
      E(13) = 0.0652D+00
      S(13) = 1.0D+00
      E(14) = 259.284D+00
      P(14) = 0.009448D+00
      E(15) = 61.0769D+00
      P(15) = 0.070974D+00
      E(16) = 19.3032D+00
      P(16) = 0.295636D+00
      E(17) = 7.0109D+00
      P(17) = 0.728219D+00
      E(18) = 2.6739D+00
      P(18) = 0.644467D+00
      E(19) = 1.0366D+00
      P(19) = 0.417413D+00
      E(20) = 0.3168D+00
      P(20) = 1.0D+00
      E(21) = 0.1143D+00
      P(21) = 1.0D+00
      E(22) = 0.0414D+00
      P(22) = 1.0D+00
      E(23) = 0.311D+00
      D(23) = 1.0D+00
      RETURN
C
C     ----- SI -----
C
  160 CONTINUE
      E(1) = 69379.23D+00
      S(1) = 0.000757D+00
      E(2) = 10354.94D+00
      S(2) = 0.005932D+00
      E(3) = 2333.88D+00
      S(3) = 0.031088D+00
      E(4) = 657.14D+00
      S(4) = 0.124967D+00
      E(5) = 214.30D+00
      S(5) = 0.386897D+00
      E(6) = 77.629D+00
      S(6) = 0.554888D+00
      E(7) = 77.629D+00
      S(7) = 0.177881D+00
      E(8) = 30.631D+00
      S(8) = 0.627765D+00
      E(9) = 12.801D+00
      S(9) = 0.247623D+00
      E(10) = 3.9269D+00
      S(10) = 1.0D+00
      E(11) = 1.4523D+00
      S(11) = 1.0D+00
      E(12) = 0.2562D+00
      S(12) = 1.0D+00
      E(13) = 0.0943D+00
      S(13) = 1.0D+00
      E(14) = 335.48D+00
      P(14) = 0.008866D+00
      E(15) = 78.90D+00
      P(15) = 0.068299D+00
      E(16) = 24.988D+00
      P(16) = 0.290958D+00
      E(17) = 9.2197D+00
      P(17) = 0.732116D+00
      E(18) = 3.6211D+00
      P(18) = 0.619879D+00
      E(19) = 1.4513D+00
      P(19) = 0.439148D+00
      E(20) = 0.5050D+00
      P(20) = 1.0D+00
      E(21) = 0.1863D+00
      P(21) = 1.0D+00
      E(22) = 0.0654D+00
      P(22) = 1.0D+00
      E(23) = 0.388D+00
      D(23) = 1.0D+00
      RETURN
C
C     ----- P  -----
C
  180 CONTINUE
      E(1) = 77492.43D+00
      S(1) = 0.000787D+00
      E(2) = 11605.79D+00
      S(2) = 0.006107D+00
      E(3) = 2645.96D+00
      S(3) = 0.031373D+00
      E(4) = 754.98D+00
      S(4) = 0.124239D+00
      E(5) = 248.75D+00
      S(5) = 0.380893D+00
      E(6) = 91.157D+00
      S(6) = 0.559812D+00
      E(7) = 91.157D+00
      S(7) = 0.163986D+00
      E(8) = 36.226D+00
      S(8) = 0.625950D+00
      E(9) = 15.211D+00
      S(9) = 0.262196D+00
      E(10) = 4.7138D+00
      S(10) = 1.0D+00
      E(11) = 1.7827D+00
      S(11) = 1.0D+00
      E(12) = 0.3425D+00
      S(12) = 1.0D+00
      E(13) = 0.1246D+00
      S(13) = 1.0D+00
      E(14) = 384.84D+00
      P(14) = 0.003240D+00
      E(15) = 90.552D+00
      P(15) = 0.024925D+00
      E(16) = 28.806D+00
      P(16) = 0.105697D+00
      E(17) = 10.688D+00
      P(17) = 0.263229D+00
      E(18) = 4.2521D+00
      P(18) = 0.719053D+00
      E(19) = 4.2521D+00
      P(19) = -1.612739D+00
      E(20) = 1.7405D+00
      P(20) = 1.205083D+00
      E(21) = 0.5979D+00
      P(21) = 1.0D+00
      E(22) = 0.2292D+00
      P(22) = 1.0D+00
      E(23) = 0.0838D+00
      P(23) = 1.0D+00
      E(24) = 0.465D+00
      D(24) = 1.0D+00
      RETURN
C
C     ----- S  -----
C
  200 CONTINUE
      E(1) = 93413.4D+00
      S(1) = 0.000742D+00
      E(2) = 13961.7D+00
      S(2) = 0.005790D+00
      E(3) = 3169.9D+00
      S(3) = 0.029945D+00
      E(4) = 902.46D+00
      S(4) = 0.118971D+00
      E(5) = 297.16D+00
      S(5) = 0.368273D+00
      E(6) = 108.702D+00
      S(6) = 0.577507D+00
      E(7) = 108.702D+00
      S(7) = 0.142943D+00
      E(8) = 43.155D+00
      S(8) = 0.624606D+00
      E(9) = 18.108D+00
      S(9) = 0.283438D+00
      E(10) = 5.5705D+00
      S(10) = 1.0D+00
      E(11) = 2.1427D+00
      S(11) = 1.0D+00
      E(12) = 0.4340D+00
      S(12) = 1.0D+00
      E(13) = 0.1570D+00
      S(13) = 1.0D+00
      E(14) = 495.04D+00
      P(14) = 0.002720D+00
      E(15) = 117.22D+00
      P(15) = 0.021090D+00
      E(16) = 37.507D+00
      P(16) = 0.092369D+00
      E(17) = 13.910D+00
      P(17) = 0.246781D+00
      E(18) = 5.5045D+00
      P(18) = 0.743840D+00
      E(19) = 5.5045D+00
      P(19) = -1.608719D+00
      E(20) = 2.2433D+00
      P(20) = 1.223255D+00
      E(21) = 0.7762D+00
      P(21) = 1.0D+00
      E(22) = 0.2919D+00
      P(22) = 1.0D+00
      E(23) = 0.1029D+00
      P(23) = 1.0D+00
      E(24) = 0.542D+00
      D(24) = 1.0D+00
      RETURN
C
C     ----- CL ----
C
 220  CONTINUE
      E(1) = 105818.8D+00
      S(1) = 0.000743D+00
      E(2) = 15872.0D+00
      S(2) = 0.005753D+00
      E(3) = 3619.7D+00
      S(3) = 0.029676D+00
      E(4) = 1030.8D+00
      S(4) = 0.118010D+00
      E(5) = 339.91D+00
      S(5) = 0.365230D+00
      E(6) = 124.538D+00
      S(6) = 0.581221D+00
      E(7) = 124.538D+00
      S(7) = 0.137548D+00
      E(8) = 49.514D+00
      S(8) = 0.622881D+00
      E(9) = 20.806D+00
      S(9) = 0.290143D+00
      E(10) = 6.4648D+00
      S(10) = 1.0D+00
      E(11) = 2.5254D+00
      S(11) = 1.0D+00
      E(12) = 0.5378D+00
      S(12) = 1.0D+00
      E(13) = 0.1935D+00
      S(13) = 1.0D+00
      E(14) = 589.78D+00
      P(14) = 0.002760D+00
      E(15) = 139.85D+00
      P(15) = 0.021536D+00
      E(16) = 44.795D+00
      P(16) = 0.095916D+00
      E(17) = 16.612D+00
      P(17) = 0.262315D+00
      E(18) = 6.5995D+00
      P(18) = 0.726811D+00
      E(19) = 6.5995D+00
      P(19) = -1.564294D+00
      E(20) = 2.7141D+00
      P(20) = 1.495778D+00
      E(21) = 0.9528D+00
      P(21) = 1.0D+00
      E(22) = 0.3580D+00
      P(22) = 1.0D+00
      E(23) = 0.1250D+00
      P(23) = 1.0D+00
      E(24) = 0.619D+00
      D(24) = 1.0D+00
      RETURN
C
C     ----- AR ----
C
 240  CONTINUE
      E(1) = 118022.4D+00
      S(1) = 0.000747D+00
      E(2) = 17683.5D+00
      S(2) = 0.005790D+00
      E(3) = 4027.8D+00
      S(3) = 0.029919D+00
      E(4) = 1145.40D+00
      S(4) = 0.119206D+00
      E(5) = 377.16D+00
      S(5) = 0.369028D+00
      E(6) = 138.160D+00
      S(6) = 0.576459D+00
      E(7) = 138.160D+00
      S(7) = 0.143927D+00
      E(8) = 54.989D+00
      S(8) = 0.622938D+00
      E(9) = 23.171D+00
      S(9) = 0.283964D+00
      E(10) = 7.3779D+00
      S(10) = 1.0D+00
      E(11) = 2.9237D+00
      S(11) = 1.0D+00
      E(12) = 0.6504D+00
      S(12) = 1.0D+00
      E(13) = 0.2328D+00
      S(13) = 1.0D+00
      E(14) = 663.06D+00
      P(14) = 0.003082D+00
      E(15) = 157.09D+00
      P(15) = 0.024165D+00
      E(16) = 50.231D+00
      P(16) = 0.108223D+00
      E(17) = 18.635D+00
      P(17) = 0.294192D+00
      E(18) = 7.4465D+00
      P(18) = 0.687862D+00
      E(19) = 7.4465D+00
      P(19) = -1.214482D+00
      E(20) = 3.0957D+00
      P(20) = 1.632370D+00
      E(21) = 1.1065D+00
      P(21) = 1.0D+00
      E(22) = 0.4156D+00
      P(22) = 1.0D+00
      E(23) = 0.1454D+00
      P(23) = 1.0D+00
      E(24) = 0.696D+00
      D(24) = 1.0D+00
      RETURN
      END
C*MODULE BASEXT  *DECK TZVTM1
      SUBROUTINE TZVTM1(E,S,P,D,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     ----- <10S8P3D> CONTRACTION FOR K-ZN .....
C     A.J.H. WACHTERS, J CHEM PHYS, 52, 1033 (1970)
C
C      FOR TM ATOMS WACHTERS 14S,9P,5D MODIFIED AS FOLLOWS ...
C         1. MOST DIFFUSE S REMOVED ..
C         2. ADDITIONAL S SPANNING THE 3S-4S REGION
C         3. 2 ADDITIONAL P'S TO DESCRIBE 4P ...
C         4. (6D/4D,1D,1D) FROM RAPPE ET AL. J.PHYS.CHEM. 85,2607(1981)
C            EXCEPT FOR -ZN- WHERE WACHTER'S (5D/4D,1D) +
C            HAY'S DIFFUSE D ARE USED.
C
      DIMENSION E(31),S(31),P(31),D(31)
      NN = N-18
      GO TO (100,120,130,140,150,160,170,180,190,
     1       200,210,220), NN
C
C     ----- K  ----
C     ----- CONTRACTION 3, TABLE VI IN WACHTERS
C
 100  CONTINUE
      E(1)=150591.0D+00
      E(2)=22629.60D+00
      E(3)=5223.160D+00
      E(4)=1498.060D+00
      E(5)=495.1650D+00
      E(6)=180.7920D+00
      S(1)=0.00026D+00
      S(2)=0.00200D+00
      S(3)=0.01015D+00
      S(4)=0.04043D+00
      S(5)=0.12732D+00
      S(6)=0.29872D+00
      E(7)=71.19400D+00
      E(8)=29.37230D+00
      S(7)=0.41837D+00
      S(8)=0.23705D+00
      E(9)=8.688630D+00
      S(9)=1.00000D+00
      E(10)=3.46382D+00
      S(10)=1.00000D+00
      E(11)=0.811307D+00
      S(11)=1.000000D+00
      E(12)=0.312555D+00
      S(12)=1.000000D+00
      E(13)=0.035668D+00
      S(13)=1.000000D+00
      E(14)=0.016517D+00
      S(14)=1.000000D+00
      E(15)=867.2590D+00
      E(16)=205.2540D+00
      E(17)=65.82140D+00
      P(15)=0.00234D+00
      P(16)=0.01880D+00
      P(17)=0.08668D+00
      E(18)=24.57420D+00
      E(19)=9.877040D+00
      E(20)=4.116930D+00
      P(18)=0.25041D+00
      P(19)=0.42972D+00
      P(20)=0.35117D+00
      E(21)=1.55653D+00
      P(21)=1.00000D+00
      E(22)=0.614068D+00
      E(23)=0.228735D+00
      P(22)=0.568420D+00
      P(23)=0.327430D+00
      RETURN
C
C     ----- CA ----
C     ----- CONTRACTION 3, TABLE VI IN WACHTERS
C
 120  CONTINUE
      E(1)=171384.0D+00
      E(2)=25873.0D+00
      E(3)=5984.90D+00
      E(4)=1712.02D+00
      E(5)=563.304D+00
      E(6)=204.797D+00
      S(1)=0.00025D+00
      S(2)=0.00192D+00
      S(3)=0.00975D+00
      S(4)=0.03915D+00
      S(5)=0.12455D+00
      S(6)=0.29536D+00
      E(7)=80.4187D+00
      E(8)=33.1145D+00
      S(7)=0.41948D+00
      S(8)=0.24271D+00
      E(9)=9.86221D+00
      S(9)=1.00000D+00
      E(10)=3.97775D+00
      S(10)=1.00000D+00
      E(11)=0.977055D+00
      S(11)=1.000000D+00
      E(12)=0.396147D+00
      S(12)=1.000000D+00
      E(13)=0.065938D+00
      S(13)=1.000000D+00
      E(14)=0.026902D+00
      S(14)=1.000000D+00
      E(15)=1000.67D+00
      E(16)=237.310D+00
      E(17)=76.4676D+00
      P(15)=0.00223D+00
      P(16)=0.01792D+00
      P(17)=0.08284D+00
      E(18)=28.7085D+00
      E(19)=11.6294D+00
      E(20)=4.90273D+00
      P(18)=0.24217D+00
      P(19)=0.42397D+00
      P(20)=0.35857D+00
      E(21)=1.92143D+00
      P(21)=1.00000D+00
      E(22)=0.784693D+00
      E(23)=0.308996D+00
      P(22)=0.571600D+00
      P(23)=0.328200D+00
      RETURN
C
C
C ----- SC
C
 130  E(1)=188961.0D+00
      S(1)=0.00025D+00
      E(2)=28491.4D+00
      S(2)=0.00193D+00
      E(3)=6572.43D+00
      S(3)=0.00980D+00
      E(4)=1881.03D+00
      S(4)=0.03940D+00
      E(5)=617.979D+00
      S(5)=0.12532D+00
      E(6)=225.127D+00
      S(6)=1.0D+00
      E(7)=88.5664D+00
      S(7)=1.0D+00
      E(8)=36.5819D+00
      S(8)=1.0D+00
      E(9)=11.0358D+00
      S(9)=1.0D+00
      E(10)=4.49063D+00
      S(10)=1.0D+00
      E(11)=1.12935D+00
      S(11)=1.0D+00
      E(12)=0.454613D+00
      S(12)=1.0D+00
      E(13)=0.188D+00
      S(13)=1.0D+00
      E(14)=0.077533D+00
      S(14)=1.0D+00
      E(15)=1113.82D+00
      P(15)=0.00223D+00
      E(16)=266.244D+00
      P(16)=0.01764D+00
      E(17)=86.5763D+00
      P(17)=0.08123D+00
      E(18)=32.5934D+00
      P(18)=0.24040D+00
      E(19)=13.2190D+00
      P(19)=1.0D+00
      E(20)=5.58357D+00
      P(20)=1.0D+00
      E(21)=2.18594D+00
      P(21)=1.0D+00
      E(22)=0.895011D+00
      P(22)=1.0D+00
      E(23)=0.351975D+00
      P(23)=1.0D+00
      E(24)=0.137D+00
      P(24)=1.0D+00
      E(25)=0.053D+00
      P(25)=1.0D+00
      E(26)=23.2400D+00
      D(26)=1.4363D-02
      E(27)=6.1430D+00
      D(27)=7.8068D-02
      E(28)=2.0070D+00
      D(28)=2.1756D-01
      E(29)=0.6652D+00
      D(29)=3.4980D-01
      E(30)=0.2021D+00
      D(30)=1.0D+00
      E(31)=0.0545D+00
      D(31)=1.0D+00
      RETURN
C
C ----- TI
C
 140  E(1)=206082.0D+00
      S(1)=0.00025D+00
      E(2)=31226.8D+00
      S(2)=0.00193D+00
      E(3)=7199.32D+00
      S(3)=0.00988D+00
      E(4)=2048.75D+00
      S(4)=0.03994D+00
      E(5)=670.790D+00
      S(5)=0.12737D+00
      E(6)=243.650D+00
      S(6)=1.0D+00
      E(7)=95.9250D+00
      S(7)=1.0D+00
      E(8)=39.8101D+00
      S(8)=1.0D+00
      E(9)=12.2205D+00
      S(9)=1.0D+00
      E(10)=5.00882D+00
      S(10)=1.0D+00
      E(11)=1.28569D+00
      S(11)=1.0D+00
      E(12)=0.512806D+00
      S(12)=1.0D+00
      E(13)=0.209D+00
      S(13)=1.0D+00
      E(14)=0.085576D+00
      S(14)=1.0D+00
      E(15)=1264.70D+00
      P(15)=0.00214D+00
      E(16)=301.230D+00
      P(16)=0.01725D+00
      E(17)=96.9777D+00
      P(17)=0.08117D+00
      E(18)=36.3727D+00
      P(18)=0.24143D+00
      E(19)=14.7814D+00
      P(19)=1.0D+00
      E(20)=6.27465D+00
      P(20)=1.0D+00
      E(21)=2.47878D+00
      P(21)=1.0D+00
      E(22)=1.01618D+00
      P(22)=1.0D+00
      E(23)=0.398162D+00
      P(23)=1.0D+00
      E(24)=0.156D+00
      P(24)=1.0D+00
      E(25)=0.0611D+00
      P(25)=1.0D+00
      E(26)=28.1100D+00
      D(26)=1.5315D-02
      E(27)=7.6300D+00
      D(27)=8.4027D-02
      E(28)=2.5280D+00
      D(28)=2.3830D-01
      E(29)=0.8543D+00
      D(29)=3.7744D-01
      E(30)=0.2673D+00
      D(30)=1.0D+00
      E(31)=0.0743D+00
      D(31)=1.0D+00
      RETURN
C
C ----- V
C
 150  E(1)=226878.0D+00
      S(1)=0.00025D+00
      E(2)=33899.6D+00
      S(2)=0.00197D+00
      E(3)=7720.24D+00
      S(3)=0.01017D+00
      E(4)=2191.53D+00
      S(4)=0.04098D+00
      E(5)=719.169D+00
      S(5)=0.12980D+00
      E(6)=262.086D+00
      S(6)=1.0D+00
      E(7)=103.653D+00
      S(7)=1.0D+00
      E(8)=43.2548D+00
      S(8)=1.0D+00
      E(9)=13.5088D+00
      S(9)=1.0D+00
      E(10)=5.56715D+00
      S(10)=1.0D+00
      E(11)=1.45174D+00
      S(11)=1.0D+00
      E(12)=0.574445D+00
      S(12)=1.0D+00
      E(13)=0.231D+00
      S(13)=1.0D+00
      E(14)=0.092820D+00
      S(14)=1.0D+00
      E(15)=1398.43D+00
      P(15)=0.00214D+00
      E(16)=331.571D+00
      P(16)=0.01735D+00
      E(17)=107.002D+00
      P(17)=0.08128D+00
      E(18)=40.3183D+00
      P(18)=0.24130D+00
      E(19)=16.4635D+00
      P(19)=1.0D+00
      E(20)=7.02352D+00
      P(20)=1.0D+00
      E(21)=2.79025D+00
      P(21)=1.0D+00
      E(22)=1.14609D+00
      P(22)=1.0D+00
      E(23)=0.447272D+00
      P(23)=1.0D+00
      E(24)=0.175D+00
      P(24)=1.0D+00
      E(25)=0.068D+00
      P(25)=1.0D+00
      E(26)=33.3600D+00
      D(26)=1.5398D-02
      E(27)=9.3310D+00
      D(27)=8.4640D-02
      E(28)=3.1580D+00
      D(28)=2.4000D-01
      E(29)=1.1130D+00
      D(29)=3.7938D-01
      E(30)=0.3608D+00
      D(30)=1.0D+00
      E(31)=0.1007D+00
      D(31)=1.0D+00
      RETURN
C
C ----- CR
C
 160  E(1)=236658.0D+00
      S(1)=0.00027D+00
      E(2)=35364.0D+00
      S(2)=0.00208D+00
      E(3)=8058.31D+00
      S(3)=0.01071D+00
      E(4)=2294.23D+00
      S(4)=0.04284D+00
      E(5)=756.118D+00
      S(5)=0.13420D+00
      E(6)=277.000D+00
      S(6)=1.0D+00
      E(7)=110.179D+00
      S(7)=1.0D+00
      E(8)=46.3710D+00
      S(8)=1.0D+00
      E(9)=14.8215D+00
      S(9)=1.0D+00
      E(10)=6.13262D+00
      S(10)=1.0D+00
      E(11)=1.62959D+00
      S(11)=1.0D+00
      E(12)=0.641177D+00
      S(12)=1.0D+00
      E(13)=0.253D+00
      S(13)=1.0D+00
      E(14)=0.099511D+00
      S(14)=1.0D+00
      E(15)=1478.77D+00
      P(15)=0.00228D+00
      E(16)=351.490D+00
      P(16)=0.01836D+00
      E(17)=113.826D+00
      P(17)=0.08503D+00
      E(18)=43.1567D+00
      P(18)=0.24761D+00
      E(19)=17.7775D+00
      P(19)=1.0D+00
      E(20)=7.66128D+00
      P(20)=1.0D+00
      E(21)=3.07765D+00
      P(21)=1.0D+00
      E(22)=1.26619D+00
      P(22)=1.0D+00
      E(23)=0.493534D+00
      P(23)=1.0D+00
      E(24)=0.192D+00
      P(24)=1.0D+00
      E(25)=0.075D+00
      P(25)=1.0D+00
      E(26)=37.8900D+00
      D(26)=1.6192D-02
      E(27)=10.5800D+00
      D(27)=9.0071D-02
      E(28)=3.6030D+00
      D(28)=2.5222D-01
      E(29)=1.2700D+00
      D(29)=3.8799D-01
      E(30)=0.4118D+00
      D(30)=1.0D+00
      E(31)=0.1126D+00
      D(31)=1.0D+00
      RETURN
C
C ----- MN
C
 170  E(1)=243694.0D+00
      S(1)=0.00029D+00
      E(2)=35995.0D+00
      S(2)=0.00225D+00
      E(3)=8223.56D+00
      S(3)=0.01152D+00
      E(4)=2353.12D+00
      S(4)=0.04559D+00
      E(5)=780.965D+00
      S(5)=0.14039D+00
      E(6)=288.519D+00
      S(6)=1.0D+00
      E(7)=115.701D+00
      S(7)=1.0D+00
      E(8)=49.1175D+00
      S(8)=1.0D+00
      E(9)=16.0885D+00
      S(9)=1.0D+00
      E(10)=6.70430D+00
      S(10)=1.0D+00
      E(11)=1.80517D+00
      S(11)=1.0D+00
      E(12)=0.703011D+00
      S(12)=1.0D+00
      E(13)=0.273D+00
      S(13)=1.0D+00
      E(14)=0.106385D+00
      S(14)=1.0D+00
      E(15)=1500.39D+00
      P(15)=0.00258D+00
      E(16)=358.800D+00
      P(16)=0.02048D+00
      E(17)=116.699D+00
      P(17)=0.09293D+00
      E(18)=44.6132D+00
      P(18)=0.26074D+00
      E(19)=18.5985D+00
      P(19)=1.0D+00
      E(20)=8.13778D+00
      P(20)=1.0D+00
      E(21)=3.33734D+00
      P(21)=1.0D+00
      E(22)=1.37895D+00
      P(22)=1.0D+00
      E(23)=0.538639D+00
      P(23)=1.0D+00
      E(24)=0.210D+00
      P(24)=1.0D+00
      E(25)=0.082D+00
      P(25)=1.0D+00
      E(26)=42.6300D+00
      D(26)=1.6715D-02
      E(27)=11.9700D+00
      D(27)=9.4015D-02
      E(28)=4.0910D+00
      D(28)=2.6078D-01
      E(29)=1.4500D+00
      D(29)=3.9368D-01
      E(30)=0.4700D+00
      D(30)=1.0D+00
      E(31)=0.1281D+00
      D(31)=1.0D+00
      RETURN
C
C ----- FE
C
 180  E(1)=257539.0D+00
      S(1)=0.00029D+00
      E(2)=38636.9D+00
      S(2)=0.00226D+00
      E(3)=8891.44D+00
      S(3)=0.01152D+00
      E(4)=2544.01D+00
      S(4)=0.04566D+00
      E(5)=844.777D+00
      S(5)=0.14035D+00
      E(6)=312.527D+00
      S(6)=1.0D+00
      E(7)=125.593D+00
      S(7)=1.0D+00
      E(8)=53.4987D+00
      S(8)=1.0D+00
      E(9)=17.7151D+00
      S(9)=1.0D+00
      E(10)=7.37677D+00
      S(10)=1.0D+00
      E(11)=2.01847D+00
      S(11)=1.0D+00
      E(12)=0.779935D+00
      S(12)=1.0D+00
      E(13)=0.298D+00
      S(13)=1.0D+00
      E(14)=0.114220D+00
      S(14)=1.0D+00
      E(15)=1678.40D+00
      P(15)=0.00249D+00
      E(16)=396.392D+00
      P(16)=0.02015D+00
      E(17)=128.588D+00
      P(17)=0.09199D+00
      E(18)=49.1158D+00
      P(18)=0.25991D+00
      E(19)=20.5035D+00
      P(19)=1.0D+00
      E(20)=8.98712D+00
      P(20)=1.0D+00
      E(21)=3.68249D+00
      P(21)=1.0D+00
      E(22)=1.52175D+00
      P(22)=1.0D+00
      E(23)=0.592684D+00
      P(23)=1.0D+00
      E(24)=0.231D+00
      P(24)=1.0D+00
      E(25)=0.0899D+00
      P(25)=1.0D+00
      E(26)=47.1000D+00
      D(26)=1.7641D-02
      E(27)=13.1200D+00
      D(27)=1.0078D-01
      E(28)=4.4780D+00
      D(28)=2.7437D-01
      E(29)=1.5810D+00
      D(29)=4.0147D-01
      E(30)=0.5100D+00
      D(30)=1.0D+00
      E(31)=0.1382D+00
      D(31)=1.0D+00
      RETURN
C
C ----- CO
C
 190  E(1)=270991.0D+00
      S(1)=0.00031D+00
      E(2)=39734.8D+00
      S(2)=0.00242D+00
      E(3)=9057.46D+00
      S(3)=0.01238D+00
      E(4)=2598.21D+00
      S(4)=0.04849D+00
      E(5)=868.200D+00
      S(5)=0.14672D+00
      E(6)=323.431D+00
      S(6)=1.0D+00
      E(7)=130.860D+00
      S(7)=1.0D+00
      E(8)=56.1219D+00
      S(8)=1.0D+00
      E(9)=18.9219D+00
      S(9)=1.0D+00
      E(10)=7.95238D+00
      S(10)=1.0D+00
      E(11)=2.19754D+00
      S(11)=1.0D+00
      E(12)=0.846713D+00
      S(12)=1.0D+00
      E(13)=0.321D+00
      S(13)=1.0D+00
      E(14)=0.122266D+00
      S(14)=1.0D+00
      E(15)=1636.21D+00
      P(15)=0.00296D+00
      E(16)=390.903D+00
      P(16)=0.02336D+00
      E(17)=127.884D+00
      P(17)=0.10343D+00
      E(18)=49.2413D+00
      P(18)=0.27954D+00
      E(19)=20.7512D+00
      P(19)=1.0D+00
      E(20)=9.20368D+00
      P(20)=1.0D+00
      E(21)=3.81779D+00
      P(21)=1.0D+00
      E(22)=1.58762D+00
      P(22)=1.0D+00
      E(23)=0.624660D+00
      P(23)=1.0D+00
      E(24)=0.2458D+00
      P(24)=1.0D+00
      E(25)=0.0967D+00
      P(25)=1.0D+00
      E(26)=51.6900D+00
      D(26)=1.7801D-02
      E(27)=14.7000D+00
      D(27)=1.0460D-01
      E(28)=4.8510D+00
      D(28)=2.9733D-01
      E(29)=1.6430D+00
      D(29)=4.2353D-01
      E(30)=0.5075D+00
      D(30)=1.0D+00
      E(31)=0.1433D+00
      D(31)=1.0D+00
      RETURN
C
C ----- NI
C
 200  E(1)=284878.0D+00
      S(1)=0.00032D+00
      E(2)=41997.9D+00
      S(2)=0.00246D+00
      E(3)=9627.67D+00
      S(3)=0.01254D+00
      E(4)=2761.96D+00
      S(4)=0.04926D+00
      E(5)=920.488D+00
      S(5)=0.14950D+00
      E(6)=341.805D+00
      S(6)=1.0D+00
      E(7)=138.023D+00
      S(7)=1.0D+00
      E(8)=59.2587D+00
      S(8)=1.0D+00
      E(9)=20.3712D+00
      S(9)=1.0D+00
      E(10)=8.5940D+00
      S(10)=1.0D+00
      E(11)=2.39417D+00
      S(11)=1.0D+00
      E(12)=0.918169D+00
      S(12)=1.0D+00
      E(13)=0.346D+00
      S(13)=1.0D+00
      E(14)=0.130176D+00
      S(14)=1.0D+00
      E(15)=1774.18D+00
      P(15)=0.00295D+00
      E(16)=423.403D+00
      P(16)=0.02337D+00
      E(17)=138.311D+00
      P(17)=0.10406D+00
      E(18)=53.1703D+00
      P(18)=0.28226D+00
      E(19)=22.3874D+00
      P(19)=1.0D+00
      E(20)=9.92848D+00
      P(20)=1.0D+00
      E(21)=4.11625D+00
      P(21)=1.0D+00
      E(22)=1.71031D+00
      P(22)=1.0D+00
      E(23)=0.672528D+00
      P(23)=1.0D+00
      E(24)=0.264D+00
      P(24)=1.0D+00
      E(25)=0.104D+00
      P(25)=1.0D+00
      E(26)=58.7300D+00
      D(26)=1.7529D-02
      E(27)=16.7100D+00
      D(27)=1.0041D-01
      E(28)=5.7830D+00
      D(28)=2.7609D-01
      E(29)=2.0640D+00
      D(29)=4.0348D-01
      E(30)=0.6752D+00
      D(30)=1.0D+00
      E(31)=0.1825D+00
      D(31)=1.0D+00
      RETURN
C
C ----- CU
C
 210  E(1)=307637.0D+00
      S(1)=0.00031D+00
      E(2)=46592.9D+00
      S(2)=0.00236D+00
      E(3)=10651.1D+00
      S(3)=0.01213D+00
      E(4)=3043.31D+00
      S(4)=0.04791D+00
      E(5)=1010.62D+00
      S(5)=0.14652D+00
      E(6)=374.252D+00
      S(6)=1.0D+00
      E(7)=150.796D+00
      S(7)=1.0D+00
      E(8)=64.6268D+00
      S(8)=1.0D+00
      E(9)=22.1381D+00
      S(9)=1.0D+00
      E(10)=9.34746D+00
      S(10)=1.0D+00
      E(11)=2.60863D+00
      S(11)=1.0D+00
      E(12)=0.997212D+00
      S(12)=1.0D+00
      E(13)=0.374D+00
      S(13)=1.0D+00
      E(14)=0.140120D+00
      S(14)=1.0D+00
      E(15)=2026.05D+00
      P(15)=0.00267D+00
      E(16)=484.207D+00
      P(16)=0.02133D+00
      E(17)=157.803D+00
      P(17)=0.09690D+00
      E(18)=60.4844D+00
      P(18)=0.27103D+00
      E(19)=25.3593D+00
      P(19)=1.0D+00
      E(20)=11.1685D+00
      P(20)=1.0D+00
      E(21)=4.56411D+00
      P(21)=1.0D+00
      E(22)=1.88440D+00
      P(22)=1.0D+00
      E(23)=0.734735D+00
      P(23)=1.0D+00
      E(24)=0.286D+00
      P(24)=1.0D+00
      E(25)=0.111D+00
      P(25)=1.0D+00
      E(26)=65.8000D+00
      D(26)=1.7079D-02
      E(27)=18.8200D+00
      D(27)=9.9155D-02
      E(28)=6.5380D+00
      D(28)=2.7475D-01
      E(29)=2.3480D+00
      D(29)=4.0401D-01
      E(30)=0.7691D+00
      D(30)=1.0D+00
      E(31)=0.2065D+00
      D(31)=1.0D+00
      RETURN
C
C ----- ZN (1S)
C
 220  E(1)=316336.0D+00
      S(1)=0.00032D+00
      E(2)=48561.0D+00
      S(2)=0.00242D+00
      E(3)=11157.4D+00
      S(3)=0.01241D+00
      E(4)=3205.01D+00
      S(4)=0.04864D+00
      E(5)=1068.58D+00
      S(5)=0.14807D+00
      E(6)=396.394D+00
      S(6)=1.0D+00
      E(7)=159.806D+00
      S(7)=1.0D+00
      E(8)=68.5890D+00
      S(8)=1.0D+00
      E(9)=23.7081D+00
      S(9)=1.0D+00
      E(10)=10.0372D+00
      S(10)=1.0D+00
      E(11)=2.81043D+00
      S(11)=1.0D+00
      E(12)=1.06964D+00
      S(12)=1.0D+00
      E(13)=0.396D+00
      S(13)=1.0D+00
      E(14)=0.146951D+00
      S(14)=1.0D+00
      E(15)=2213.18D+00
      P(15)=0.00262D+00
      E(16)=527.050D+00
      P(16)=0.02091D+00
      E(17)=172.293D+00
      P(17)=0.09501D+00
      E(18)=66.0814D+00
      P(18)=0.26855D+00
      E(19)=27.6863D+00
      P(19)=1.0D+00
      E(20)=12.1841D+00
      P(20)=1.0D+00
      E(21)=4.98796D+00
      P(21)=1.0D+00
      E(22)=2.05791D+00
      P(22)=1.0D+00
      E(23)=0.798609D+00
      P(23)=1.0D+00
      E(24)=0.311D+00
      P(24)=1.0D+00
      E(25)=0.121D+00
      P(25)=1.0D+00
      E(26)=58.4084D+00
      D(26)=0.02759D+00
      E(27)=16.4492D+00
      D(27)=0.14994D+00
      E(28)=5.57570D+00
      D(28)=0.36929D+00
      E(29)=1.88441D+00
      D(29)=0.46343D+00
      E(30)=0.572305D+00
      D(30)=1.0D+00
      E(31)=0.155D+00
      D(31)=1.0D+00
      RETURN
      END
