C 19 NOV 00 - MWS - N21G: RETURN NORMALIZED CXINP COEFS
C 16 JUN 95 - JAB - N21G: SI 6-21G 2P IN PAPER IS NOT NORMALIZED.
C 10 NOV 94 - MWS - REMOVE FTNCHECK WARNING
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C 12 MAR 92 - MWS - REDIMENSION TO 500 ATOMS
C  4 JAN 92 - TLW - MAKE WRITES PARALLEL;ADD COMMON PAR
C  4 MAY 91 - MWS - CORRECT THE 4SP SHELLS FOR COPPER
C  7 AUG 90 - TLW - ADD CF AND CG TO COMMON NSHEL
C 10 MAY 90 - MWS - CHANGE SCALE FACTOR USAGE.
C 10 AUG 88 - MWS - MXSH,MXGSH,MXGTOT FROM 120,10,440 TO 1000,30,5000
C  5 MAY 88 - MWS - CORRECT K,CA,RB,SR (FINAL PAPER.NE.PREPRINT!)
C  6 FEB 88 - MICHAEL RAMEK - 1ST AND 2ND ROW TRANSITION METALS ADDED
C  7 JUL 86 - JAB - SANITIZE FLOATING POINT CONSTANTS
C  4 APR 86 - MWS - DUMMY DIMENSIONS AT 21 IN N21FIV
C 14 OCT 85 - STE - USE GENERIC SQRT,ABS
C 28 AUG 85 - MWS - CORRECT ANTIMONY INNER VALENCE SHELL
C  9 AUG 85 - MWS - ADD 4TH,5TH ROW MAIN GROUP, REORGANIZE THE
C                   FIRST THREE ROWS, VERIFY NORMALIZATION
C 12 APR 85 - MWS - DUNNING/HAY BASES MOVED TO SECTION BASEXT
C  8 APR 85 - MWS - CHANGE ITYP TO MATCH CHANGES IN ROUTINE ATOMS
C 16 NOV 84 - STE - FIX FOR HE IN N21G
C 23 JUN 83 - MWS - BREAK UP BASIS SET SECTION TO SMALLER PIECES
C*MODULE BASN21  *DECK N21G
      SUBROUTINE N21G(NUCZ,IGAUSS,CSINP,CPINP,CDINP,SCFAC,IERR1,IERR2,
     *                INTYP,NANGM,NBFS,MINF,MAXF,LOC,NGAUSS,NS)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DONE,GOPARR,DSKWRK,MASWRK
C
      DIMENSION CSINP(*),CPINP(*),CDINP(*),SCFAC(*),
     *          INTYP(*),NANGM(21),NBFS(21),MINF(21),MAXF(21),NS(*)
      DIMENSION EEX(21),CCS(21),CCP(21),CCD(21)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      DATA ZERO,PT5,ONE/0.0D+00,0.5D+00,1.0D+00/
      DATA PI32/5.56832799683170D+00/
      DATA HSCALE/1.10D+00/
      DATA PT75/0.75D+00/,TM6/1.0D-06/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA TYPS/4H   S/, TYPP/4H   P/, TYPD/4H   D/
#else
      CHARACTER*4 :: TYPS_STR
      EQUIVALENCE (TYPS, TYPS_STR)
      CHARACTER*4 :: TYPP_STR
      EQUIVALENCE (TYPP, TYPP_STR)
      CHARACTER*4 :: TYPD_STR
      EQUIVALENCE (TYPD, TYPD_STR)
      DATA TYPS_STR/"   S"/, TYPP_STR/"   P"/, TYPD_STR/"   D"/
#endif
C
C     ----- SET UP THE N-21G BASIS SET -----
C
C     H-NE   N=3,6  J.S.BINKLEY, J.A.POPLE, W.J.HEHRE
C                   J. CHEM. PHYS. 1980, 102, 939-947.
C     NA-AR  N=3,6  M.S.GORDON, J.S.BINKLEY, J.A.POPLE, W.J.PIETRO,
C                   W.J.HEHRE   J.AM.CHEM.SOC. 1982, 104, 2797-2803.
C     K,CA,GA-KR,RB,SR,IN-XE   N=3    K.D.DOBBS, W.J.HEHRE
C                   J.COMPUT.CHEM. 7, 359-378(1986)
C     SC-ZN  N=3    K.D.DOBBS, W.J.HEHRE
C                   J.COMPUT.CHEM. 8, 861-879(1987)
C     Y-CD   N=3    K.D.DOBBS, W.J.HEHRE
C                   J.COMPUT.CHEM. 8, 880-893(1987)
C
C     THE BASES ARE IN THE ORDER 1S,2SP,3SP,3D,4SP,4D,5SP
C
      ITYP = -2**20
      DO 100 I = 1,21
         EEX(I)=ZERO
         CCS(I)=ZERO
         CCP(I)=ZERO
         CCD(I)=ZERO
  100 CONTINUE
      SCALI=SCFAC(1)
      SCALO=SCFAC(2)
C
C     ----- HYDROGEN AND HELIUM -----
C
      IF (NUCZ .GT. 2) GO TO 120
      CALL N21ONE(EEX,CCS,NUCZ)
      IF(NUCZ.EQ.1  .AND.  SCALI.EQ.ZERO) SCALI=HSCALE
      IF(NUCZ.EQ.1  .AND.  SCALO.EQ.ZERO) SCALO=HSCALE
      GO TO 200
C
C     ----- LITHIUM TO NEON -----
C
  120 CONTINUE
      IF (NUCZ .GT. 10) GO TO 130
      CALL N21TWO(EEX,CCS,CCP,NUCZ,IGAUSS)
      GO TO 200
C
C     ----- SODIUM TO ARGON -----
C
  130 CONTINUE
      IF (NUCZ .GT. 18) GO TO 140
      CALL N21THR(EEX,CCS,CCP,NUCZ,IGAUSS)
      GO TO 200
C
C     ----- POTASSIUM TO KRYPTON -----
C
  140 CONTINUE
      IF(NUCZ.GT.36) GO TO 150
      CALL N21FOU(EEX,CCS,CCP,CCD,NUCZ,IGAUSS)
      GO TO 200
C
C     ----- RUBIDIUM TO XENON -----
C
  150 CONTINUE
      IF(NUCZ.GT.54) CALL BERROR(2)
      CALL N21FIV(EEX,CCS,CCP,CCD,NUCZ,IGAUSS)
C
C     ----- LOOP OVER EACH SHELL -----
C
  200 CONTINUE
      IERR3=0
      NG=0
      SCALE=ONE
      IPASS=0
  210 CONTINUE
      IPASS=IPASS+1
      DONE = .FALSE.
      CALL N21SHL(NUCZ,IPASS,ITYP,IGAUSS,DONE)
      IF(DONE  .AND.  IERR3.NE.0 .AND. MASWRK) WRITE(IW,910)
      IF(DONE  .AND.  IERR3.NE.0) CALL ABRT
      IF(DONE) RETURN
C              ******
C
C     ----- DEFINE THE CURRENT SHELL -----
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
      IF(IGAUSS.EQ.2) SCALE=SCALI
      IF(IGAUSS.EQ.1) SCALE=SCALO
      IF(SCALE.EQ.ZERO) SCALE=ONE
      DO 440 I = 1,IGAUSS
         K = K1+I-1
         EX(K) = EEX(NG+I)*SCALE*SCALE
         CSINP(K) = CCS(NG+I)
         CPINP(K) = CCP(NG+I)
         CDINP(K) = CCD(NG+I)
         CS(K) = CSINP(K)
         CP(K) = CPINP(K)
         CD(K) = CDINP(K)
  440 CONTINUE
C
C     ----- ALWAYS UNNORMALIZE PRIMITIVES -----
C
      DO 460 K = K1,K2
         EE = EX(K)+EX(K)
         FACS = PI32/(EE*SQRT(EE))
         FACP = PT5*FACS/EE
         FACD = PT75*FACS/(EE*EE)
         IF(ITYP.EQ.1  .OR.  ITYP.EQ.6) CS(K) = CS(K)/SQRT(FACS)
         IF(ITYP.EQ.2  .OR.  ITYP.EQ.6) CP(K) = CP(K)/SQRT(FACP)
         IF(ITYP.EQ.3                 ) CD(K) = CD(K)/SQRT(FACD)
  460 CONTINUE
C
C     ----- IF(NORMF.EQ.0) NORMALIZE BASIS FUNCTIONS -----
C
      IF (NORMF .EQ. 1) GO TO 600
      FACS = ZERO
      FACP = ZERO
      FACD = ZERO
      DO 510 IG = K1,K2
         DO 500 JG = K1,IG
            EE = EX(IG)+EX(JG)
            FAC = EE*SQRT(EE)
            DUMS = CS(IG)*CS(JG)/FAC
            DUMP = PT5*CP(IG)*CP(JG)/(EE*FAC)
            DUMD = PT75*CD(IG)*CD(JG)/(EE*EE*FAC)
            IF (IG .EQ. JG) GO TO 480
               DUMS = DUMS+DUMS
               DUMP = DUMP+DUMP
               DUMD = DUMD+DUMD
  480       CONTINUE
            FACS = FACS+DUMS
            FACP = FACP+DUMP
            FACD = FACD+DUMD
  500    CONTINUE
  510 CONTINUE
      IF(ITYP.EQ.1  .OR.  ITYP.EQ.6) FACS=ONE/SQRT(FACS*PI32)
      IF(ITYP.EQ.2  .OR.  ITYP.EQ.6) FACP=ONE/SQRT(FACP*PI32)
      IF(ITYP.EQ.3                 ) FACD=ONE/SQRT(FACD*PI32)
C
C           VERIFY NORMALIZATION ON THE INTERNALLY STORED BASES
C              2P FUNCTION IN SI'S 6-21G BASIS IS NOT QUITE ACCURATE
C
      TOL=TM6
      IF(NUCZ.LE.18  .AND.  IGAUSS.EQ.2) TOL=5.0D+00*TOL
      IF(NUCZ.EQ.14  .AND.  IGAUSS.EQ.6) TOL=5.0D+00*TOL
      IF(ITYP.NE.1  .AND.  ITYP.NE.6) GO TO 520
         TESTS=ABS(ONE-FACS)
         IF(TESTS.LT.TOL) GO TO 520
         TYPE = TYPS
         IF (MASWRK) THEN
            WRITE(IW,900) NUCZ,TYPE,FACS
            WRITE(IW,920) (EX(K),K=K1,K2)
            WRITE(IW,920) (CSINP(K),K=K1,K2)
         ENDIF
         IERR3=IERR3+1
  520 IF(ITYP.NE.2  .AND.  ITYP.NE.6) GO TO 530
         TESTP=ABS(ONE-FACP)
         IF(TESTP.LT.TOL) GO TO 540
         TYPE = TYPP
         IF (MASWRK) THEN
            WRITE(IW,900) NUCZ,TYPE,FACP
            WRITE(IW,920) (EX(K),K=K1,K2)
            WRITE(IW,920) (CPINP(K),K=K1,K2)
         ENDIF
         IERR3=IERR3+1
  530 IF(ITYP.NE.3) GO TO 540
         TESTD=ABS(ONE-FACD)
         IF(TESTD.LT.TOL) GO TO 540
         TYPE = TYPD
         IF (MASWRK) THEN
            WRITE(IW,900) NUCZ,TYPE,FACD
            WRITE(IW,920) (EX(K),K=K1,K2)
            WRITE(IW,920) (CDINP(K),K=K1,K2)
         ENDIF
         IERR3=IERR3+1
C
  540 CONTINUE
      DO 590 IG = K1,K2
         IF(ITYP.EQ.1  .OR.  ITYP.EQ.6) CS(IG) = FACS*CS(IG)
         IF(ITYP.EQ.2  .OR.  ITYP.EQ.6) CP(IG) = FACP*CP(IG)
         IF(ITYP.EQ.3                 ) CD(IG) = FACD*CD(IG)
         IF(ITYP.EQ.1  .OR.  ITYP.EQ.6) CSINP(IG) = FACS*CSINP(IG)
         IF(ITYP.EQ.2  .OR.  ITYP.EQ.6) CPINP(IG) = FACP*CPINP(IG)
         IF(ITYP.EQ.3                 ) CDINP(IG) = FACD*CDINP(IG)
  590 CONTINUE
C
  600 CONTINUE
      NG=NG+IGAUSS
      GO TO 210
  900 FORMAT(1X,'ERROR WITH NORMALIZATION OF N-21G BASIS FUNCTION'/
     *   1X,'NUCZ=',I4,' TYPE=',A4,' NORMALIZATION=',F15.8)
  910 FORMAT(1X,'NORMALIZATION ERROR --- CHECK BASIS SET')
  920 FORMAT(1X,6F16.7)
      END
C*MODULE BASN21  *DECK N21SHL
      SUBROUTINE N21SHL(NUCZ,IPASS,ITYP,IGAUSS,DONE)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL DONE
      DIMENSION MAXPS(9),ITYPE(9,9)
      DATA MAXPS/2,3,4,5,7,6,7,9,8/
      DATA ITYPE/1,1,              7*0,
     *           1,6,6,            6*0,
     *           1,6,6,6,          5*0,
     *           1,6,6,6,6,        4*0,
     *           1,6,6,3,3,6,6,    2*0,
     *           1,6,6,3,6,6,      3*0,
     *           1,6,6,3,6,6,6,    2*0,
     *           1,6,6,3,6,3,3,6,6,
     *           1,6,6,3,6,3,6,6,    0/
C
C     ----- DEFINE CURRENT SHELL PARAMETERS FOR N-21G BASES -----
C        NUCZ=NUCLEAR CHARGE OF THIS ATOM
C        IPASS=NUMBER OF CURRENT SHELL
C        MXPASS=TOTAL NUMBER OF SHELLS ON THIS ATOM
C        ITYP =1(S), 3(D), 6(SP) SHELL
C        IGAUSS=NUMBER OF GAUSSIANS IN CURRENT SHELL
C
      KIND=1
      IF(NUCZ.GT. 2) KIND=2
      IF(NUCZ.GT.10) KIND=3
      IF(NUCZ.GT.18) KIND=4
      IF(NUCZ.GT.20) KIND=5
      IF(NUCZ.GT.30) KIND=6
      IF(NUCZ.GT.36) KIND=7
      IF(NUCZ.GT.38) KIND=8
      IF(NUCZ.GT.48) KIND=9
C
      MXPASS=MAXPS(KIND)
      DONE=.FALSE.
      IF(IPASS.GT.MXPASS) DONE=.TRUE.
      IF(DONE) RETURN
      IF(KIND.EQ.5.OR.KIND.EQ.8)THEN
        IF(IPASS.EQ.(MXPASS-3)) IGAUSS=2
        IF(IPASS.EQ.(MXPASS-2)) IGAUSS=1
      ENDIF
      IF(IPASS.EQ.(MXPASS-1)) IGAUSS=2
      IF(IPASS.EQ.MXPASS    ) IGAUSS=1
      ITYP = ITYPE(IPASS,KIND)
      RETURN
      END
C*MODULE BASN21  *DECK N21ONE
      SUBROUTINE N21ONE(E,S,NUCZ)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION E(3),S(3)
      GO TO (100,200), NUCZ
C
C          HYDROGEN
C
  100 CONTINUE
      E( 1) = 0.450180D+01
      E( 2) = 0.681444D+00
      E( 3) = 0.151398D+00
      S( 1) = 0.156285D+00
      S( 2) = 0.904691D+00
      S( 3) = 1.0D+00
      RETURN
C
C          HELIUM
C
  200 CONTINUE
      E( 1) = 0.136267D+02
      E( 2) = 0.199935D+01
      E( 3) = 0.382993D+00
      S( 1) = 0.175230D+00
      S( 2) = 0.893483D+00
      S( 3) = 1.0D+00
      RETURN
      END
C*MODULE BASN21  *DECK N21TWO
      SUBROUTINE N21TWO(E,S,P,NUCZ,NGAUSS)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION E(9),S(9),P(9)
      IBR = NUCZ-2
      GO TO (100,200,300,400,500,600,700,800), IBR
C
C          LITHIUM
C
  100 CONTINUE
      GO TO ( 110, 110, 130, 140, 110, 160),NGAUSS
  110 CONTINUE
      CALL BERROR(2)
  130 CONTINUE
      E( 1) = 0.368382D+02
      E( 2) = 0.548172D+01
      E( 3) = 0.111327D+01
      E( 4) = 0.540205D+00
      E( 5) = 0.102255D+00
      E( 6) = 0.285645D-01
      S( 1) = 0.696686D-01
      S( 2) = 0.381346D+00
      S( 3) = 0.681702D+00
      S( 4) = -0.263127D+00
      S( 5) = 0.114339D+01
      S( 6) = 1.0D+00
      P( 4) = 0.161546D+00
      P( 5) = 0.915663D+00
      P( 6) = 1.0D+00
      RETURN
  140 CONTINUE
      E( 1) = 0.109353D+03
      E( 2) = 0.164228D+02
      E( 3) = 0.359415D+01
      E( 4) = 0.905297D+00
      E( 5) = 0.540205D+00
      E( 6) = 0.102255D+00
      E( 7) = 0.285645D-01
      S( 1) = 0.190277D-01
      S( 2) = 0.130276D+00
      S( 3) = 0.439082D+00
      S( 4) = 0.557314D+00
      S( 5) = -0.263127D+00
      S( 6) = 0.114339D+01
      S( 7) = 1.0D+00
      P( 5) = 0.161546D+00
      P( 6) = 0.915663D+00
      P( 7) = 1.0D+00
      RETURN
  160 CONTINUE
      E( 1) = 0.642418D+03
      E( 2) = 0.965164D+02
      E( 3) = 0.220174D+02
      E( 4) = 0.617645D+01
      E( 5) = 0.193511D+01
      E( 6) = 0.639577D+00
      E( 7) = 0.540205D+00
      E( 8) = 0.102255D+00
      E( 9) = 0.285645D-01
      S( 1) = 0.215096D-02
      S( 2) = 0.162677D-01
      S( 3) = 0.776383D-01
      S( 4) = 0.246495D+00
      S( 5) = 0.467506D+00
      S( 6) = 0.346915D+00
      S( 7) = -0.263127D+00
      S( 8) = 0.114339D+01
      S( 9) = 1.0D+00
      P( 7) = 0.161546D+00
      P( 8) = 0.915663D+00
      P( 9) = 1.0D+00
      RETURN
C
C          BERYLLIUM
C
  200 CONTINUE
      GO TO ( 210, 210, 230, 240, 210, 260),NGAUSS
  210 CONTINUE
      CALL BERROR(2)
  230 CONTINUE
      E( 1) = 0.718876D+02
      E( 2) = 0.107289D+02
      E( 3) = 0.222205D+01
      E( 4) = 0.129548D+01
      E( 5) = 0.268881D+00
      E( 6) = 0.773501D-01
      S( 1) = 0.644263D-01
      S( 2) = 0.366096D+00
      S( 3) = 0.695934D+00
      S( 4) = -0.421064D+00
      S( 5) = 0.122407D+01
      S( 6) = 1.0D+00
      P( 4) = 0.205132D+00
      P( 5) = 0.882528D+00
      P( 6) = 1.0D+00
      RETURN
  240 CONTINUE
      E( 1) = 0.207980D+03
      E( 2) = 0.314316D+02
      E( 3) = 0.699419D+01
      E( 4) = 0.181295D+01
      E( 5) = 0.129548D+01
      E( 6) = 0.268881D+00
      E( 7) = 0.773501D-01
      S( 1) = 0.180520D-01
      S( 2) = 0.123804D+00
      S( 3) = 0.427580D+00
      S( 4) = 0.569901D+00
      S( 5) = -0.421064D+00
      S( 6) = 0.122407D+01
      S( 7) = 1.0D+00
      P( 5) = 0.205132D+00
      P( 6) = 0.882528D+00
      P( 7) = 1.0D+00
      RETURN
  260 CONTINUE
      E( 1) = 0.126450D+04
      E( 2) = 0.189930D+03
      E( 3) = 0.431275D+02
      E( 4) = 0.120889D+02
      E( 5) = 0.380790D+01
      E( 6) = 0.128266D+01
      E( 7) = 0.129548D+01
      E( 8) = 0.268881D+00
      E( 9) = 0.773501D-01
      S( 1) = 0.194336D-02
      S( 2) = 0.148251D-01
      S( 3) = 0.720662D-01
      S( 4) = 0.237022D+00
      S( 5) = 0.468789D+00
      S( 6) = 0.356382D+00
      S( 7) = -0.421064D+00
      S( 8) = 0.122407D+01
      S( 9) = 1.0D+00
      P( 7) = 0.205132D+00
      P( 8) = 0.882528D+00
      P( 9) = 1.0D+00
      RETURN
C
C          BORON
C
  300 CONTINUE
      GO TO ( 310, 310, 330, 340, 310, 360),NGAUSS
  310 CONTINUE
      CALL BERROR(2)
  330 CONTINUE
      E( 1) = 0.116434D+03
      E( 2) = 0.174314D+02
      E( 3) = 0.368016D+01
      E( 4) = 0.228187D+01
      E( 5) = 0.465248D+00
      E( 6) = 0.124328D+00
      S( 1) = 0.629605D-01
      S( 2) = 0.363304D+00
      S( 3) = 0.697255D+00
      S( 4) = -0.368662D+00
      S( 5) = 0.119944D+01
      S( 6) = 1.0D+00
      P( 4) = 0.231152D+00
      P( 5) = 0.866764D+00
      P( 6) = 1.0D+00
      RETURN
  340 CONTINUE
      E( 1) = 0.345445D+03
      E( 2) = 0.519156D+02
      E( 3) = 0.115337D+02
      E( 4) = 0.301981D+01
      E( 5) = 0.228187D+01
      E( 6) = 0.465248D+00
      E( 7) = 0.124328D+00
      S( 1) = 0.170348D-01
      S( 2) = 0.119266D+00
      S( 3) = 0.424702D+00
      S( 4) = 0.575037D+00
      S( 5) = -0.368662D+00
      S( 6) = 0.119944D+01
      S( 7) = 1.0D+00
      P( 5) = 0.231152D+00
      P( 6) = 0.866764D+00
      P( 7) = 1.0D+00
      RETURN
  360 CONTINUE
      E( 1) = 0.208212D+04
      E( 2) = 0.312310D+03
      E( 3) = 0.708874D+02
      E( 4) = 0.198525D+02
      E( 5) = 0.629161D+01
      E( 6) = 0.212862D+01
      E( 7) = 0.228187D+01
      E( 8) = 0.465248D+00
      E( 9) = 0.124328D+00
      S( 1) = 0.184986D-02
      S( 2) = 0.141277D-01
      S( 3) = 0.692697D-01
      S( 4) = 0.232393D+00
      S( 5) = 0.470154D+00
      S( 6) = 0.360288D+00
      S( 7) = -0.368662D+00
      S( 8) = 0.119944D+01
      S( 9) = 1.0D+00
      P( 7) = 0.231152D+00
      P( 8) = 0.866764D+00
      P( 9) = 1.0D+00
      RETURN
C
C          CARBON
C
  400 CONTINUE
      GO TO ( 410, 410, 430, 440, 410, 460),NGAUSS
  410 CONTINUE
      CALL BERROR(2)
  430 CONTINUE
      E( 1) = 0.172256D+03
      E( 2) = 0.259109D+02
      E( 3) = 0.553335D+01
      E( 4) = 0.366498D+01
      E( 5) = 0.770545D+00
      E( 6) = 0.195857D+00
      S( 1) = 0.617669D-01
      S( 2) = 0.358794D+00
      S( 3) = 0.700713D+00
      S( 4) = -0.395897D+00
      S( 5) = 0.121584D+01
      S( 6) = 1.0D+00
      P( 4) = 0.236460D+00
      P( 5) = 0.860619D+00
      P( 6) = 1.0D+00
      RETURN
  440 CONTINUE
      E( 1) = 0.514836D+03
      E( 2) = 0.773470D+02
      E( 3) = 0.172534D+02
      E( 4) = 0.455754D+01
      E( 5) = 0.366498D+01
      E( 6) = 0.770545D+00
      E( 7) = 0.195857D+00
      S( 1) = 0.165399D-01
      S( 2) = 0.116447D+00
      S( 3) = 0.419945D+00
      S( 4) = 0.580709D+00
      S( 5) = -0.395897D+00
      S( 6) = 0.121584D+01
      S( 7) = 1.0D+00
      P( 5) = 0.236460D+00
      P( 6) = 0.860619D+00
      P( 7) = 1.0D+00
      RETURN
  460 CONTINUE
      E( 1) = 0.304752D+04
      E( 2) = 0.456424D+03
      E( 3) = 0.103653D+03
      E( 4) = 0.292258D+02
      E( 5) = 0.934863D+01
      E( 6) = 0.318904D+01
      E( 7) = 0.366498D+01
      E( 8) = 0.770545D+00
      E( 9) = 0.195857D+00
      S( 1) = 0.182588D-02
      S( 2) = 0.140566D-01
      S( 3) = 0.687570D-01
      S( 4) = 0.230422D+00
      S( 5) = 0.468463D+00
      S( 6) = 0.362780D+00
      S( 7) = -0.395897D+00
      S( 8) = 0.121584D+01
      S( 9) = 1.0D+00
      P( 7) = 0.236460D+00
      P( 8) = 0.860619D+00
      P( 9) = 1.0D+00
      RETURN
C
C          NITROGEN
C
  500 CONTINUE
      GO TO ( 510, 510, 530, 540, 510, 560),NGAUSS
  510 CONTINUE
      CALL BERROR(2)
  530 CONTINUE
      E( 1) = 0.242766D+03
      E( 2) = 0.364851D+02
      E( 3) = 0.781449D+01
      E( 4) = 0.542522D+01
      E( 5) = 0.114915D+01
      E( 6) = 0.283205D+00
      S( 1) = 0.598657D-01
      S( 2) = 0.352955D+00
      S( 3) = 0.706513D+00
      S( 4) = -0.413301D+00
      S( 5) = 0.122442D+01
      S( 6) = 1.0D+00
      P( 4) = 0.237972D+00
      P( 5) = 0.858953D+00
      P( 6) = 1.0D+00
      RETURN
  540 CONTINUE
      E( 1) = 0.715345D+03
      E( 2) = 0.107490D+03
      E( 3) = 0.240414D+02
      E( 4) = 0.639437D+01
      E( 5) = 0.542522D+01
      E( 6) = 0.114915D+01
      E( 7) = 0.283205D+00
      S( 1) = 0.162587D-01
      S( 2) = 0.114910D+00
      S( 3) = 0.417387D+00
      S( 4) = 0.583539D+00
      S( 5) = -0.413301D+00
      S( 6) = 0.122442D+01
      S( 7) = 1.0D+00
      P( 5) = 0.237972D+00
      P( 6) = 0.858953D+00
      P( 7) = 1.0D+00
      RETURN
  560 CONTINUE
      E( 1) = 0.415011D+04
      E( 2) = 0.620084D+03
      E( 3) = 0.141688D+03
      E( 4) = 0.403367D+02
      E( 5) = 0.130267D+02
      E( 6) = 0.447003D+01
      E( 7) = 0.542522D+01
      E( 8) = 0.114915D+01
      E( 9) = 0.283205D+00
      S( 1) = 0.184541D-02
      S( 2) = 0.141645D-01
      S( 3) = 0.686325D-01
      S( 4) = 0.228574D+00
      S( 5) = 0.466162D+00
      S( 6) = 0.365672D+00
      S( 7) = -0.413301D+00
      S( 8) = 0.122442D+01
      S( 9) = 1.0D+00
      P( 7) = 0.237972D+00
      P( 8) = 0.858953D+00
      P( 9) = 1.0D+00
      RETURN
C
C          OXYGEN
C
  600 CONTINUE
      GO TO ( 610, 610, 630, 640, 610, 660),NGAUSS
  610 CONTINUE
      CALL BERROR(2)
  630 CONTINUE
      E( 1) = 0.322037D+03
      E( 2) = 0.484308D+02
      E( 3) = 0.104206D+02
      E( 4) = 0.740294D+01
      E( 5) = 0.157620D+01
      E( 6) = 0.373684D+00
      S( 1) = 0.592394D-01
      S( 2) = 0.351500D+00
      S( 3) = 0.707658D+00
      S( 4) = -0.404453D+00
      S( 5) = 0.122156D+01
      S( 6) = 1.0D+00
      P( 4) = 0.244586D+00
      P( 5) = 0.853955D+00
      P( 6) = 1.0D+00
      RETURN
  640 CONTINUE
      E( 1) = 0.938318D+03
      E( 2) = 0.141662D+03
      E( 3) = 0.318308D+02
      E( 4) = 0.851101D+01
      E( 5) = 0.740294D+01
      E( 6) = 0.157620D+01
      E( 7) = 0.373684D+00
      S( 1) = 0.162714D-01
      S( 2) = 0.114340D+00
      S( 3) = 0.416787D+00
      S( 4) = 0.583808D+00
      S( 5) = -0.404453D+00
      S( 6) = 0.122156D+01
      S( 7) = 1.0D+00
      P( 5) = 0.244586D+00
      P( 6) = 0.853955D+00
      P( 7) = 1.0D+00
      RETURN
  660 CONTINUE
      E( 1) = 0.547227D+04
      E( 2) = 0.817806D+03
      E( 3) = 0.186446D+03
      E( 4) = 0.530230D+02
      E( 5) = 0.171800D+02
      E( 6) = 0.591196D+01
      E( 7) = 0.740294D+01
      E( 8) = 0.157620D+01
      E( 9) = 0.373684D+00
      S( 1) = 0.183217D-02
      S( 2) = 0.141047D-01
      S( 3) = 0.686262D-01
      S( 4) = 0.229376D+00
      S( 5) = 0.466399D+00
      S( 6) = 0.364173D+00
      S( 7) = -0.404453D+00
      S( 8) = 0.122156D+01
      S( 9) = 1.0D+00
      P( 7) = 0.244586D+00
      P( 8) = 0.853955D+00
      P( 9) = 1.0D+00
      RETURN
C
C          FLUORINE
C
  700 CONTINUE
      GO TO ( 710, 710, 730, 740, 710, 760),NGAUSS
  710 CONTINUE
      CALL BERROR(2)
  730 CONTINUE
      E( 1) = 0.413801D+03
      E( 2) = 0.622446D+02
      E( 3) = 0.134340D+02
      E( 4) = 0.977759D+01
      E( 5) = 0.208617D+01
      E( 6) = 0.482383D+00
      S( 1) = 0.585483D-01
      S( 2) = 0.349308D+00
      S( 3) = 0.709632D+00
      S( 4) = -0.407327D+00
      S( 5) = 0.122314D+01
      S( 6) = 1.0D+00
      P( 4) = 0.246680D+00
      P( 5) = 0.852321D+00
      P( 6) = 1.0D+00
      RETURN
  740 CONTINUE
      E( 1) = 0.120491D+04
      E( 2) = 0.181792D+03
      E( 3) = 0.408879D+02
      E( 4) = 0.109630D+02
      E( 5) = 0.977759D+01
      E( 6) = 0.208617D+01
      E( 7) = 0.482383D+00
      S( 1) = 0.160678D-01
      S( 2) = 0.113306D+00
      S( 3) = 0.415239D+00
      S( 4) = 0.585754D+00
      S( 5) = -0.407327D+00
      S( 6) = 0.122314D+01
      S( 7) = 1.0D+00
      P( 5) = 0.246680D+00
      P( 6) = 0.852321D+00
      P( 7) = 1.0D+00
      RETURN
  760 CONTINUE
      E( 1) = 0.678319D+04
      E( 2) = 0.104244D+04
      E( 3) = 0.242398D+03
      E( 4) = 0.696320D+02
      E( 5) = 0.226894D+02
      E( 6) = 0.779636D+01
      E( 7) = 0.977759D+01
      E( 8) = 0.208617D+01
      E( 9) = 0.482383D+00
      S( 1) = 0.188463D-02
      S( 2) = 0.138121D-01
      S( 3) = 0.662493D-01
      S( 4) = 0.221875D+00
      S( 5) = 0.460842D+00
      S( 6) = 0.378453D+00
      S( 7) = -0.407327D+00
      S( 8) = 0.122314D+01
      S( 9) = 1.0D+00
      P( 7) = 0.246680D+00
      P( 8) = 0.852321D+00
      P( 9) = 1.0D+00
      RETURN
C
C          NEON
C
  800 CONTINUE
      GO TO ( 810, 810, 830, 840, 810, 860),NGAUSS
  810 CONTINUE
      CALL BERROR(2)
  830 CONTINUE
      E( 1) = 0.515724D+03
      E( 2) = 0.776538D+02
      E( 3) = 0.168136D+02
      E( 4) = 0.124830D+02
      E( 5) = 0.266451D+01
      E( 6) = 0.606250D+00
      S( 1) = 0.581430D-01
      S( 2) = 0.347951D+00
      S( 3) = 0.710714D+00
      S( 4) = -0.409922D+00
      S( 5) = 0.122431D+01
      S( 6) = 1.0D+00
      P( 4) = 0.247460D+00
      P( 5) = 0.851743D+00
      P( 6) = 1.0D+00
      RETURN
  840 CONTINUE
      E( 1) = 0.151275D+04
      E( 2) = 0.227368D+03
      E( 3) = 0.510739D+02
      E( 4) = 0.137213D+02
      E( 5) = 0.124830D+02
      E( 6) = 0.266451D+01
      E( 7) = 0.606250D+00
      S( 1) = 0.158096D-01
      S( 2) = 0.112430D+00
      S( 3) = 0.414266D+00
      S( 4) = 0.587193D+00
      S( 5) = -0.409922D+00
      S( 6) = 0.122431D+01
      S( 7) = 1.0D+00
      P( 5) = 0.247460D+00
      P( 6) = 0.851743D+00
      P( 7) = 1.0D+00
      RETURN
  860 CONTINUE
      E( 1) = 0.878583D+04
      E( 2) = 0.132390D+04
      E( 3) = 0.300795D+03
      E( 4) = 0.851891D+02
      E( 5) = 0.276534D+02
      E( 6) = 0.953039D+01
      E( 7) = 0.124830D+02
      E( 8) = 0.266451D+01
      E( 9) = 0.606250D+00
      S( 1) = 0.178077D-02
      S( 2) = 0.135790D-01
      S( 3) = 0.670847D-01
      S( 4) = 0.226825D+00
      S( 5) = 0.465053D+00
      S( 6) = 0.368995D+00
      S( 7) = -0.409922D+00
      S( 8) = 0.122431D+01
      S( 9) = 1.0D+00
      P( 7) = 0.247460D+00
      P( 8) = 0.851743D+00
      P( 9) = 1.0D+00
      RETURN
      END
C*MODULE BASN21  *DECK N21THR
      SUBROUTINE N21THR(E,S,P,NUCZ,NGAUSS)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION E(15),S(15),P(15)
      IBR=NUCZ-10
      GO TO (100,200,300,400,500,600,700,800), IBR
C
C          SODIUM
C
  100 CONTINUE
      GO TO (110,110,130,110,110,160), NGAUSS
  110 CONTINUE
      CALL BERROR(2)
  130 CONTINUE
      E( 1) = 0.547613D+03
      E( 2) = 0.820678D+02
      E( 3) = 0.176917D+02
      E( 4) = 0.175407D+02
      E( 5) = 0.379398D+01
      E( 6) = 0.906441D+00
      E( 7) = 0.501824D+00
      E( 8) = 0.609458D-01
      E( 9) = 0.244349D-01
      S( 1) = 0.674911D-01
      S( 2) = 0.393505D+00
      S( 3) = 0.665605D+00
      S( 4) = -0.111937D+00
      S( 5) = 0.254654D+00
      S( 6) = 0.844417D+00
      S( 7) = -0.219660D+00
      S( 8) = 0.108912D+01
      S( 9) = 1.0D+00
      P( 4) = 0.128233D+00
      P( 5) = 0.471533D+00
      P( 6) = 0.604273D+00
      P( 7) = 0.906649D-02
      P( 8) = 0.997202D+00
      P( 9) = 1.0D+00
      RETURN
  160 CONTINUE
      E( 1) = 0.999320D+04
      E( 2) = 0.149989D+04
      E( 3) = 0.341951D+03
      E( 4) = 0.946796D+02
      E( 5) = 0.297345D+02
      E( 6) = 0.100063D+02
      E( 7) = 0.150963D+03
      E( 8) = 0.355878D+02
      E( 9) = 0.111683D+02
      E( 10) = 0.390201D+01
      E( 11) = 0.138177D+01
      E( 12) = 0.466382D+00
      E( 13) = 0.501824D+00
      E( 14) = 0.609458D-01
      E( 15) = 0.244349D-01
      S( 1) = 0.193766D-02
      S( 2) = 0.148070D-01
      S( 3) = 0.727055D-01
      S( 4) = 0.252629D+00
      S( 5) = 0.493242D+00
      S( 6) = 0.313169D+00
      S( 7) = -0.354208D-02
      S( 8) = -0.439588D-01
      S( 9) = -0.109752D+00
      S(10) = 0.187398D+00
      S(11) = 0.646699D+00
      S(12) = 0.306058D+00
      S(13) = -0.219660D+00
      S(14) = 0.108912D+01
      S(15) = 1.0D+00
      P( 7) = 0.500166D-02
      P( 8) = 0.355109D-01
      P( 9) = 0.142825D+00
      P(10) = 0.338620D+00
      P(11) = 0.451579D+00
      P(12) = 0.273271D+00
      P(13) = 0.906649D-02
      P(14) = 0.997202D+00
      P(15) = 1.0D+00
      RETURN
C
C          MAGNESIUM
C
  200 CONTINUE
      GO TO (210,210,230,210,210,260), NGAUSS
  210 CONTINUE
      CALL BERROR(2)
  230 CONTINUE
      E( 1) = 0.652841D+03
      E( 2) = 0.983805D+02
      E( 3) = 0.212996D+02
      E( 4) = 0.233727D+02
      E( 5) = 0.519953D+01
      E( 6) = 0.131508D+01
      E( 7) = 0.611349D+00
      E( 8) = 0.141841D+00
      E( 9) = 0.464011D-01
      S( 1) = 0.675982D-01
      S( 2) = 0.391778D+00
      S( 3) = 0.666661D+00
      S( 4) = -0.110246D+00
      S( 5) = 0.184119D+00
      S( 6) = 0.896399D+00
      P( 4) = 0.121014D+00
      P( 5) = 0.462810D+00
      P( 6) = 0.606907D+00
      S( 7) = -0.361101D+00
      S( 8) = 0.121505D+01
      S( 9) = 1.0D+00
      P( 7) = 0.242633D-01
      P( 8) = 0.986673D+00
      P( 9) = 1.0D+00
      RETURN
  260 CONTINUE
      E( 1) = 0.117228D+05
      E( 2) = 0.175993D+04
      E( 3) = 0.400846D+03
      E( 4) = 0.112807D+03
      E( 5) = 0.359997D+02
      E( 6) = 0.121828D+02
      E( 7) = 0.189180D+03
      E( 8) = 0.452119D+02
      E( 9) = 0.143563D+02
      E( 10) = 0.513886D+01
      E( 11) = 0.190652D+01
      E( 12) = 0.705887D+00
      E( 13) = 0.611349D+00
      E( 14) = 0.141841D+00
      E( 15) = 0.464011D-01
      S( 1) = 0.197783D-02
      S( 2) = 0.151140D-01
      S( 3) = 0.739108D-01
      S( 4) = 0.249191D+00
      S( 5) = 0.487928D+00
      S( 6) = 0.319662D+00
      S( 7) = -0.323717D-02
      S( 8) = -0.410079D-01
      S( 9) = -0.112600D+00
      S(10) = 0.148633D+00
      S(11) = 0.616497D+00
      S(12) = 0.364829D+00
      S(13) = -0.361101D+00
      S(14) = 0.121505D+01
      S(15) = 1.0D+00
      P( 7) = 0.492813D-02
      P( 8) = 0.349888D-01
      P( 9) = 0.140725D+00
      P(10) = 0.333642D+00
      P(11) = 0.444940D+00
      P(12) = 0.269254D+00
      P(13) = 0.242633D-01
      P(14) = 0.986673D+00
      P(15) = 1.0D+00
      RETURN
C
C          ALUMINIUM
C
  300 CONTINUE
      GO TO (310,310,330,310,310,360), NGAUSS
  310 CONTINUE
      CALL BERROR(2)
  330 CONTINUE
      E( 1) = 0.775737D+03
      E( 2) = 0.116952D+03
      E( 3) = 0.253326D+02
      E( 4) = 0.294796D+02
      E( 5) = 0.663314D+01
      E( 6) = 0.172675D+01
      E( 7) = 0.946160D+00
      E( 8) = 0.202506D+00
      E( 9) = 0.639088D-01
      S( 1) = 0.668347D-01
      S( 2) = 0.389061D+00
      S( 3) = 0.669468D+00
      S( 4) = -0.107902D+00
      S( 5) = 0.146245D+00
      S( 6) = 0.923730D+00
      P( 4) = 0.117574D+00
      P( 5) = 0.461174D+00
      P( 6) = 0.605535D+00
      S( 7) = -0.320327D+00
      S( 8) = 0.118412D+01
      S( 9) = 1.0D+00
      P( 7) = 0.519383D-01
      P( 8) = 0.972660D+00
      P( 9) = 1.0D+00
      RETURN
  360 CONTINUE
      E( 1) = 0.139831D+05
      E( 2) = 0.209875D+04
      E( 3) = 0.477705D+03
      E( 4) = 0.134360D+03
      E( 5) = 0.428709D+02
      E( 6) = 0.145189D+02
      E( 7) = 0.239668D+03
      E( 8) = 0.574419D+02
      E( 9) = 0.182859D+02
      E( 10) = 0.659914D+01
      E( 11) = 0.249049D+01
      E( 12) = 0.944545D+00
      E( 13) = 0.946160D+00
      E( 14) = 0.202506D+00
      E( 15) = 0.639088D-01
      S( 1) = 0.194267D-02
      S( 2) = 0.148599D-01
      S( 3) = 0.728494D-01
      S( 4) = 0.246830D+00
      S( 5) = 0.487258D+00
      S( 6) = 0.323496D+00
      S( 7) = -0.292619D-02
      S( 8) = -0.374083D-01
      S( 9) = -0.114487D+00
      S(10) = 0.115635D+00
      S(11) = 0.612595D+00
      S(12) = 0.393799D+00
      S(13) = -0.320327D+00
      S(14) = 0.118412D+01
      S(15) = 1.0D+00
      P( 7) = 0.460285D-02
      P( 8) = 0.331990D-01
      P( 9) = 0.136282D+00
      P(10) = 0.330476D+00
      P(11) = 0.449146D+00
      P(12) = 0.265704D+00
      P(13) = 0.519383D-01
      P(14) = 0.972660D+00
      P(15) = 1.0D+00
      RETURN
C
C          SILICON
C
  400 CONTINUE
      GO TO (410,410,430,410,410,460), NGAUSS
  410 CONTINUE
      CALL BERROR(2)
  430 CONTINUE
      E( 1) = 0.910655D+03
      E( 2) = 0.137336D+03
      E( 3) = 0.297601D+02
      E( 4) = 0.366716D+02
      E( 5) = 0.831729D+01
      E( 6) = 0.221645D+01
      E( 7) = 0.107913D+01
      E( 8) = 0.302422D+00
      E( 9) = 0.933392D-01
      S( 1) = 0.660823D-01
      S( 2) = 0.386229D+00
      S( 3) = 0.672380D+00
      S( 4) = -0.104511D+00
      S( 5) = 0.107410D+00
      S( 6) = 0.951446D+00
      P( 4) = 0.113355D+00
      P( 5) = 0.457578D+00
      P( 6) = 0.607427D+00
      S( 7) = -0.376108D+00
      S( 8) = 0.125165D+01
      S( 9) = 1.0D+00
      P( 7) = 0.671030D-01
      P( 8) = 0.956883D+00
      P( 9) = 1.0D+00
      RETURN
  460 CONTINUE
      E( 1) = 0.161159D+05
      E( 2) = 0.242558D+04
      E( 3) = 0.553867D+03
      E( 4) = 0.156340D+03
      E( 5) = 0.500683D+02
      E( 6) = 0.170178D+02
      E( 7) = 0.292718D+03
      E( 8) = 0.698731D+02
      E( 9) = 0.223363D+02
      E( 10) = 0.815039D+01
      E( 11) = 0.313458D+01
      E( 12) = 0.122543D+01
      E( 13) = 0.107913D+01
      E( 14) = 0.302422D+00
      E( 15) = 0.933392D-01
      S( 1) = 0.195948D-02
      S( 2) = 0.149288D-01
      S( 3) = 0.728478D-01
      S( 4) = 0.246130D+00
      S( 5) = 0.485914D+00
      S( 6) = 0.325002D+00
      S( 7) = -0.278094D-02
      S( 8) = -0.357146D-01
      S( 9) = -0.114985D+00
      S(10) = 0.935634D-01
      S(11) = 0.603017D+00
      S(12) = 0.418959D+00
      S(13) = -0.376108D+00
      S(14) = 0.125165D+01
      S(15) = 1.0D+00
      P( 7) = 0.443826D-02
      P( 8) = 0.326679D-01
      P( 9) = 0.134721D+00
      P(10) = 0.328678D+00
      P(11) = 0.449640D+00
      P(12) = 0.261372D+00
      P(13) = 0.671030D-01
      P(14) = 0.956883D+00
      P(15) = 1.0D+00
      RETURN
C
C          PHOSPHORUS
C
  500 CONTINUE
      GO TO (510,510,530,510,510,560), NGAUSS
  510 CONTINUE
      CALL BERROR(2)
  530 CONTINUE
      E( 1) = 0.105490D+04
      E( 2) = 0.159195D+03
      E( 3) = 0.345304D+02
      E( 4) = 0.442866D+02
      E( 5) = 0.101019D+02
      E( 6) = 0.273997D+01
      E( 7) = 0.121865D+01
      E( 8) = 0.395546D+00
      E( 9) = 0.122811D+00
      S( 1) = 0.655407D-01
      S( 2) = 0.384036D+00
      S( 3) = 0.674541D+00
      S( 4) = -0.102130D+00
      S( 5) = 0.815922D-01
      S( 6) = 0.969788D+00
      S( 7) = -0.371495D+00
      S( 8) = 0.127099D+01
      S( 9) = 1.0D+00
      P( 4) = 0.110851D+00
      P( 5) = 0.456495D+00
      P( 6) = 0.606936D+00
      P( 7) = 0.915823D-01
      P( 8) = 0.934924D+00
      P( 9) = 1.0D+00
      RETURN
  560 CONTINUE
      E( 1) = 0.194133D+05
      E( 2) = 0.290942D+04
      E( 3) = 0.661364D+03
      E( 4) = 0.185759D+03
      E( 5) = 0.591943D+02
      E( 6) = 0.200310D+02
      E( 7) = 0.339478D+03
      E( 8) = 0.810101D+02
      E( 9) = 0.258780D+02
      E( 10) = 0.945221D+01
      E( 11) = 0.366566D+01
      E( 12) = 0.146746D+01
      E( 13) = 0.121865D+01
      E( 14) = 0.395546D+00
      E( 15) = 0.122811D+00
      S( 1) = 0.185160D-02
      S( 2) = 0.142062D-01
      S( 3) = 0.699995D-01
      S( 4) = 0.240079D+00
      S( 5) = 0.484762D+00
      S( 6) = 0.335200D+00
      S( 7) = -0.278217D-02
      S( 8) = -0.360499D-01
      S( 9) = -0.116631D+00
      S(10) = 0.968328D-01
      S(11) = 0.614418D+00
      S(12) = 0.403798D+00
      S(13) = -0.371495D+00
      S(14) = 0.127099D+01
      S(15) = 1.0D+00
      P( 7) = 0.456462D-02
      P( 8) = 0.336936D-01
      P( 9) = 0.139755D+00
      P(10) = 0.339362D+00
      P(11) = 0.450921D+00
      P(12) = 0.238586D+00
      P(13) = 0.915823D-01
      P(14) = 0.934924D+00
      P(15) = 1.0D+00
      RETURN
C
C          SULPHUR
C
  600 CONTINUE
      GO TO (610,610,630,610,610,660), NGAUSS
  610 CONTINUE
      CALL BERROR(2)
  630 CONTINUE
      E( 1) = 0.121062D+04
      E( 2) = 0.182747D+03
      E( 3) = 0.396673D+02
      E( 4) = 0.522236D+02
      E( 5) = 0.119629D+02
      E( 6) = 0.328911D+01
      E( 7) = 0.122384D+01
      E( 8) = 0.457303D+00
      E( 9) = 0.142269D+00
      S( 1) = 0.650071D-01
      S( 2) = 0.382040D+00
      S( 3) = 0.676545D+00
      S( 4) = -0.100310D+00
      S( 5) = 0.650877D-01
      S( 6) = 0.981455D+00
      S( 7) = -0.286089D+00
      S( 8) = 0.122806D+01
      S( 9) = 1.000000D+00
      P( 4) = 0.109646D+00
      P( 5) = 0.457649D+00
      P( 6) = 0.604261D+00
      P( 7) = 0.164777D+00
      P( 8) = 0.870855D+00
      P( 9) = 1.000000D+00
      RETURN
  660 CONTINUE
      E( 1) = 0.219171D+05
      E( 2) = 0.330149D+04
      E( 3) = 0.754146D+03
      E( 4) = 0.212711D+03
      E( 5) = 0.679896D+02
      E( 6) = 0.230515D+02
      E( 7) = 0.423735D+03
      E( 8) = 0.100710D+03
      E( 9) = 0.321599D+02
      E( 10) = 0.118079D+02
      E( 11) = 0.463110D+01
      E( 12) = 0.187025D+01
      E( 13) = 0.122384D+01
      E( 14) = 0.457303D+00
      E( 15) = 0.142269D+00
      S( 1) = 0.186924D-02
      S( 2) = 0.142303D-01
      S( 3) = 0.696962D-01
      S( 4) = 0.238487D+00
      S( 5) = 0.483307D+00
      S( 6) = 0.338074D+00
      S( 7) = -0.237677D-02
      S( 8) = -0.316930D-01
      S( 9) = -0.113317D+00
      S(10) = 0.560900D-01
      S(11) = 0.592255D+00
      S(12) = 0.455006D+00
      S(13) = -0.286089D+00
      S(14) = 0.122806D+01
      S(15) = 1.0D+00
      P( 7) = 0.406101D-02
      P( 8) = 0.306813D-01
      P( 9) = 0.130452D+00
      P(10) = 0.327205D+00
      P(11) = 0.452851D+00
      P(12) = 0.256042D+00
      P(13) = 0.164777D+00
      P(14) = 0.870855D+00
      P(15) = 1.0D+00
      RETURN
C
C          CHLORINE
C
  700 CONTINUE
      GO TO (710,710,730,710,710,760), NGAUSS
  710 CONTINUE
      CALL BERROR(2)
  730 CONTINUE
      E( 1) = 0.137640D+04
      E( 2) = 0.207857D+03
      E( 3) = 0.451554D+02
      E( 4) = 0.608014D+02
      E( 5) = 0.139765D+02
      E( 6) = 0.388710D+01
      E( 7) = 0.135299D+01
      E( 8) = 0.526955D+00
      E( 9) = 0.166714D+00
      S( 1) = 0.645827D-01
      S( 2) = 0.380363D+00
      S( 3) = 0.678190D+00
      S( 4) = -0.987639D-01
      S( 5) = 0.511338D-01
      S( 6) = 0.991337D+00
      S( 7) = -0.222401D+00
      S( 8) = 0.118252D+01
      S( 9) = 1.0D+00
      P( 4) = 0.108598D+00
      P( 5) = 0.458682D+00
      P( 6) = 0.601962D+00
      P( 7) = 0.219216D+00
      P( 8) = 0.822321D+00
      P( 9) = 1.0D+00
      RETURN
  760 CONTINUE
      E( 1) = 0.251801D+05
      E( 2) = 0.378035D+04
      E( 3) = 0.860474D+03
      E( 4) = 0.242145D+03
      E( 5) = 0.773349D+02
      E( 6) = 0.262470D+02
      E( 7) = 0.491765D+03
      E( 8) = 0.116984D+03
      E( 9) = 0.374153D+02
      E( 10) = 0.137834D+02
      E( 11) = 0.545215D+01
      E( 12) = 0.222588D+01
      E( 13) = 0.135299D+01
      E( 14) = 0.526955D+00
      E( 15) = 0.166714D+00
      S( 1) = 0.183296D-02
      S( 2) = 0.140342D-01
      S( 3) = 0.690974D-01
      S( 4) = 0.237452D+00
      S( 5) = 0.483034D+00
      S( 6) = 0.339856D+00
      S( 7) = -0.229739D-02
      S( 8) = -0.307137D-01
      S( 9) = -0.112528D+00
      S(10) = 0.450163D-01
      S(11) = 0.589353D+00
      S(12) = 0.465206D+00
      S(13) = -0.222401D+00
      S(14) = 0.118252D+01
      S(15) = 1.0D+00
      P( 7) = 0.398940D-02
      P( 8) = 0.303177D-01
      P( 9) = 0.129880D+00
      P(10) = 0.327951D+00
      P(11) = 0.453527D+00
      P(12) = 0.252154D+00
      P(13) = 0.219216D+00
      P(14) = 0.822321D+00
      P(15) = 1.0D+00
      RETURN
C
C          ARGON
C
  800 CONTINUE
      GO TO (810,810,830,810,810,860), NGAUSS
  810 CONTINUE
      CALL BERROR(2)
  830 CONTINUE
      E( 1) = 0.155371D+04
      E( 2) = 0.234678D+03
      E( 3) = 0.510121D+02
      E( 4) = 0.700453D+02
      E( 5) = 0.161473D+02
      E( 6) = 0.453492D+01
      E( 7) = 0.154209D+01
      E( 8) = 0.607267D+00
      E( 9) = 0.195373D+00
      S( 1) = 0.641707D-01
      S( 2) = 0.378797D+00
      S( 3) = 0.679752D+00
      S( 4) = -0.974661D-01
      S( 5) = 0.390569D-01
      S( 6) = 0.999916D+00
      S( 7) = -0.176866D+00
      S( 8) = 0.114690D+01
      S( 9) = 1.0D+00
      P( 4) = 0.107619D+00
      P( 5) = 0.459576D+00
      P( 6) = 0.600041D+00
      P( 7) = 0.255687D+00
      P( 8) = 0.789842D+00
      P( 9) = 1.0D+00
      RETURN
  860 CONTINUE
      E( 1) = 0.283483D+05
      E( 2) = 0.425762D+04
      E( 3) = 0.969857D+03
      E( 4) = 0.273263D+03
      E( 5) = 0.873695D+02
      E( 6) = 0.296867D+02
      E( 7) = 0.575891D+03
      E( 8) = 0.136816D+03
      E( 9) = 0.438098D+02
      E( 10) = 0.162094D+02
      E( 11) = 0.646084D+01
      E( 12) = 0.265114D+01
      E( 13) = 0.154209D+01
      E( 14) = 0.607267D+00
      E( 15) = 0.195373D+00
      S( 1) = 0.182526D-02
      S( 2) = 0.139686D-01
      S( 3) = 0.687073D-01
      S( 4) = 0.236204D+00
      S( 5) = 0.482214D+00
      S( 6) = 0.342043D+00
      S( 7) = -0.215972D-02
      S( 8) = -0.290775D-01
      S( 9) = -0.110827D+00
      S(10) = 0.276999D-01
      S(11) = 0.577613D+00
      S(12) = 0.488688D+00
      S(13) = -0.176866D+00
      S(14) = 0.114690D+01
      S(15) = 1.0D+00
      P( 7) = 0.380665D-02
      P( 8) = 0.292305D-01
      P( 9) = 0.126467D+00
      P(10) = 0.323510D+00
      P(11) = 0.454896D+00
      P(12) = 0.256630D+00
      P(13) = 0.255687D+00
      P(14) = 0.789842D+00
      P(15) = 1.0D+00
      RETURN
      END
C*MODULE BASN21  *DECK N21FOU
      SUBROUTINE N21FOU(E,S,P,D,NUCZ,IGAUSS)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION E(15),S(15),P(15),D(15)
C
      IF(IGAUSS.NE.3) CALL BERROR(2)
      IF(NUCZ.EQ.19) GO TO 100
      IF(NUCZ.EQ.20) GO TO 200
      IF(NUCZ.LE.30) CALL N21TM1(E,S,P,D,NUCZ)
      IF(NUCZ.LE.30) RETURN
      IBR=NUCZ-30
      GO TO (300,400,500,600,700,800), IBR
C
C     POTASSIUM
C
  100 CONTINUE
      E( 1)=1721.1755D+00
      E( 2)=260.01633D+00
      E( 3)=56.624554D+00
      S( 1)=0.0648747D+00
      S( 2)=0.3808593D+00
      S( 3)=0.6773681D+00
      E( 4)=71.55720D+00
      E( 5)=15.43894D+00
      E( 6)=4.474551D+00
      S( 4)=-0.1093429D+00
      S( 5)=0.1130640D+00
      S( 6)=0.9462575D+00
      P( 4)=0.1339654D+00
      P( 5)=0.5302673D+00
      P( 6)=0.5117992D+00
      E( 7)=4.121275D+00
      E( 8)=1.188621D+00
      E( 9)=0.3756738D+00
      S( 7)=-0.2699730D+00
      S( 8)=0.3646323D+00
      S( 9)=0.8107533D+00
      P( 7)=0.01994922D+00
      P( 8)=0.4340213D+00
      P( 9)=0.6453226D+00
      E(10)=2.445766D-01
      E(11)=3.897175D-02
      E(12)=1.606255D-02
      S(10)=-2.688250D-01
      S(11)=1.128983D+00
      S(12)=1.0D+00
      P(10)=3.081035D-04
      P(11)=9.998787D-01
      P(12)=1.0D+00
      RETURN
C
C     CALCIUM
C
  200 CONTINUE
      E( 1)=1915.4348D+00
      E( 2)=289.53324D+00
      E( 3)=63.106352D+00
      S( 1)=0.0646237D+00
      S( 2)=0.3798376D+00
      S( 3)=0.6783294D+00
      E( 4)=80.39744D+00
      E( 5)=17.33075D+00
      E( 6)=5.083624D+00
      S( 4)=-0.1093028D+00
      S( 5)=0.1088996D+00
      S( 6)=0.9492768D+00
      P( 4)=0.1354332D+00
      P( 5)=0.5372215D+00
      P( 6)=0.5018044D+00
      E( 7)=4.782229D+00
      E( 8)=1.462558D+00
      E( 9)=0.4792230D+00
      S( 7)=-0.2816074D+00
      S( 8)=0.3410510D+00
      S( 9)=0.8381044D+00
      P( 7)=0.01900928D+00
      P( 8)=0.4360377D+00
      P( 9)=0.6386709D+00
      E(10)=4.396824D-01
      E(11)=5.913040D-02
      E(12)=2.389701D-02
      S(10)=-2.697049D-01
      S(11)=1.113293D+00
      S(12)=1.0D+00
      P(10)=3.081105D-04
      P(11)=9.998964D-01
      P(12)=1.0D+00
      RETURN
C
C     GALLIUM
C
  300 CONTINUE
      E( 1)=4751.8979D+00
      E( 2)=718.92054D+00
      E( 3)=157.44592D+00
      S( 1)=0.0628396D+00
      S( 2)=0.3736112D+00
      S( 3)=0.6843626D+00
      E( 4)=209.5834D+00
      E( 5)=45.69171D+00
      E( 6)=14.13297D+00
      S( 4)=-0.1115162D+00
      S( 5)=0.09269636D+00
      S( 6)=0.9622870D+00
      P( 4)=0.1442658D+00
      P( 5)=0.5731775D+00
      P( 6)=0.4490858D+00
      E( 7)=14.59954D+00
      E( 8)=4.860842D+00
      E( 9)=1.549111D+00
      S( 7)=0.2910292D+00
      S( 8)=-0.3231876D+00
      S( 9)=-0.8643910D+00
      P( 7)=0.02656186D+00
      P( 8)=0.4833137D+00
      P( 9)=0.5924304D+00
      E(10)=21.292530D+00
      E(11)=5.3931662D+00
      E(12)=1.3338828D+00
      D(10)=0.1619895D+00
      D(11)=0.5116739D+00
      D(12)=0.5898732D+00
      E(13)=1.267943D+00
      E(14)=0.1883995D+00
      S(13)=-0.2851306D+00
      S(14)=1.128022D+00
      P(13)=0.03018346D+00
      P(14)=0.9884658D+00
      E(15)=0.05723676D+00
      S(15)=1.0D+00
      P(15)=1.0D+00
      RETURN
C
C     GERMANIUM
C
  400 CONTINUE
      E( 1)=5073.7499D+00
      E( 2)=767.72417D+00
      E( 3)=168.18881D+00
      S( 1)=0.0627249D+00
      S( 2)=0.3731671D+00
      S( 3)=0.6847867D+00
      E( 4)=224.4360D+00
      E( 5)=48.95543D+00
      E( 6)=15.18370D+00
      S( 4)=-0.1115150D+00
      S( 5)=0.09120021D+00
      S( 6)=0.9634491D+00
      P( 4)=0.1446395D+00
      P( 5)=0.5753796D+00
      P( 6)=0.4459949D+00
      E( 7)=15.91257D+00
      E( 8)=5.441437D+00
      E( 9)=1.742603D+00
      S( 7)=-0.2895652D+00
      S( 8)=0.2938828D+00
      S( 9)=0.8891993D+00
      P( 7)=0.02297302D+00
      P( 8)=0.4732446D+00
      P( 9)=0.6032779D+00
      E(10)=24.321421D+00
      E(11)=6.2238135D+00
      E(12)=1.5887375D+00
      D(10)=0.1577985D+00
      D(11)=0.5114922D+00
      D(12)=0.5857703D+00
      E(13)=1.466538D+00
      E(14)=0.2630934D+00
      S(13)=-0.3967339D+00
      S(14)=1.190670D+00
      P(13)=0.02789294D+00
      P(14)=0.9874901D+00
      E(15)=0.08482072D+00
      S(15)=1.0D+00
      P(15)=1.0D+00
      RETURN
C
C     ARSENIC
C
  500 CONTINUE
      E( 1)=5407.6138D+00
      E( 2)=818.17436D+00
      E( 3)=179.26569D+00
      S( 1)=0.0626011D+00
      S( 2)=0.3727790D+00
      S( 3)=0.6851842D+00
      E( 4)=237.7783D+00
      E( 5)=54.25662D+00
      E( 6)=16.32803D+00
      S( 4)=-0.1128384D+00
      S( 5)=0.08722744D+00
      S( 6)=0.9681883D+00
      P( 4)=0.1496798D+00
      P( 5)=0.5623223D+00
      P( 6)=0.4593235D+00
      E( 7)=17.10185D+00
      E( 8)=5.805144D+00
      E( 9)=1.902084D+00
      S( 7)=-0.2914537D+00
      S( 8)=0.2969619D+00
      S( 9)=0.8865791D+00
      P( 7)=0.02568559D+00
      P( 8)=0.4833968D+00
      P( 9)=0.5887854D+00
      E(10)=27.437209D+00
      E(11)=7.0840440D+00
      E(12)=1.8558226D+00
      D(10)=0.1544952D+00
      D(11)=0.5114318D+00
      D(12)=0.5821935D+00
      E(13)=1.675404D+00
      E(14)=0.3416557D+00
      S(13)=-0.5057610D+00
      S(14)=1.251764D+00
      P(13)=0.02528246D+00
      P(14)=0.9874328D+00
      E(15)=0.1136303D+00
      S(15)=1.0D+00
      P(15)=1.0D+00
      RETURN
C
C     SELENIUM
C
  600 CONTINUE
      E( 1)=5751.3215D+00
      E( 2)=870.25721D+00
      E( 3)=190.72949D+00
      S( 1)=0.0624934D+00
      S( 2)=0.3723683D+00
      S( 3)=0.6855799D+00
      E( 4)=255.0164D+00
      E( 5)=55.57654D+00
      E( 6)=17.35661D+00
      S( 4)=-0.1119076D+00
      S( 5)=0.09099936D+00
      S( 6)=0.9636682D+00
      P( 4)=0.1461488D+00
      P( 5)=0.5813714D+00
      P( 6)=0.4374597D+00
      E( 7)=18.44568D+00
      E( 8)=6.328759D+00
      E( 9)=2.096758D+00
      S( 7)=-0.2917925D+00
      S( 8)=0.2846212D+00
      S( 9)=0.8973052D+00
      P( 7)=0.02442141D+00
      P( 8)=0.4833648D+00
      P( 9)=0.5879038D+00
      E(10)=30.627464D+00
      E(11)=7.9712764D+00
      E(12)=2.1348097D+00
      D(10)=0.1519858D+00
      D(11)=0.5116403D+00
      D(12)=0.5786936D+00
      E(13)=1.872633D+00
      E(14)=0.4174736D+00
      S(13)=-0.5677639D+00
      S(14)=1.294127D+00
      P(13)=0.02825548D+00
      P(14)=0.9849060D+00
      E(15)=0.1370907D+00
      S(15)=1.0D+00
      P(15)=1.0D+00
      RETURN
C
C     BROMINE
C
  700 CONTINUE
      E( 1)=6103.2899D+00
      E( 2)=923.69743D+00
      E( 3)=202.52031D+00
      S( 1)=0.0624175D+00
      S( 2)=0.3720414D+00
      S( 3)=0.6858728D+00
      E( 4)=270.6015D+00
      E( 5)=58.25357D+00
      E( 6)=18.46933D+00
      S( 4)=-0.1121487D+00
      S( 5)=0.09314451D+00
      S( 6)=0.9616794D+00
      P( 4)=0.1477514D+00
      P( 5)=0.6010557D+00
      P( 6)=0.4128704D+00
      E( 7)=19.76142D+00
      E( 8)=6.821752D+00
      E( 9)=2.291629D+00
      S( 7)=-0.2938704D+00
      S( 8)=0.2802663D+00
      S( 9)=0.9020357D+00
      P( 7)=0.02500708D+00
      P( 8)=0.4866098D+00
      P( 9)=0.5824234D+00
      E(10)=33.965097D+00
      E(11)=8.9008312D+00
      E(12)=2.4284360D+00
      D(10)=0.1496666D+00
      D(11)=0.5117475D+00
      D(12)=0.5759148D+00
      E(13)=2.131206D+00
      E(14)=0.4993537D+00
      S(13)=-0.6518031D+00
      S(14)=1.336012D+00
      P(13)=0.02870833D+00
      P(14)=0.9840695D+00
      E(15)=0.1647637D+00
      S(15)=1.000000D+00
      P(15)=1.0000000D+00
      RETURN
C
C     KRYPTON
C
  800 CONTINUE
      E( 1)=6446.6307D+00
      E( 2)=976.87570D+00
      E( 3)=214.47955D+00
      S( 1)=0.0625398D+00
      S( 2)=0.3721075D+00
      S( 3)=0.6856107D+00
      E( 4)=287.6446D+00
      E( 5)=62.62009D+00
      E( 6)=19.69174D+00
      S( 4)=-0.1120607D+00
      S( 5)=0.09013913D+00
      S( 6)=0.9643301D+00
      P( 4)=0.1475279D+00
      P( 5)=0.5868918D+00
      P( 6)=0.4295068D+00
      E( 7)=21.12321D+00
      E( 8)=7.303286D+00
      E( 9)=2.488850D+00
      S( 7)=-0.2958173D+00
      S( 8)=0.2792167D+00
      S( 9)=0.9037303D+00
      P( 7)=0.02606955D+00
      P( 8)=0.4922498D+00
      P( 9)=0.5742737D+00
      E(10)=37.368103D+00
      E(11)=9.8543131D+00
      E(12)=2.7327955D+00
      D(10)=0.1479466D+00
      D(11)=0.5121719D+00
      D(12)=0.5729498D+00
      E(13)=2.361374D+00
      E(14)=0.5860160D+00
      S(13)=-0.7202454D+00
      S(14)=1.376846D+00
      P(13)=0.02877518D+00
      P(14)=0.9833391D+00
      E(15)=0.1944473D+00
      S(15)=1.0D+00
      P(15)=1.0D+00
      RETURN
      END
C*MODULE BASN21  *DECK N21TM1
      SUBROUTINE N21TM1(E,S,P,D,NUCZ)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION E(15),S(15),P(15),D(15)
C
C     FIRST TRANSITION SERIES
C
      IBR=NUCZ-20
      IF(IBR.LT.1.OR.IBR.GT.10)CALL BERROR(2)
      GO TO (100,200,300,400,500,600,700,800,900,1000), IBR
C
C     SCANDIUM
C
  100 CONTINUE
      E( 1)=2119.887D+00
      E( 2)=320.4299D+00
      E( 3)=69.89893D+00
      S( 1)=0.0644208D+00
      S( 2)=0.3791603D+00
      S( 3)=0.6789629D+00
      E( 4)=89.76450D+00
      E( 5)=19.38510D+00
      E( 6)=5.731423D+00
      S( 4)=-0.1093837D+00
      S( 5)=0.1050699D+00
      S( 6)=0.9522045D+00
      P( 4)=0.1363278D+00
      P( 5)=0.5418598D+00
      P( 6)=0.4950551D+00
      E( 7)=5.491938D+00
      E( 8)=1.743742D+00
      E( 9)=0.5662273D+00
      S( 7)=-0.2852107D+00
      S( 8)=0.3241555D+00
      S( 9)=0.8565921D+00
      P( 7)=0.01761356D+00
      P( 8)=0.4336448D+00
      P( 9)=0.6425507D+00
      E(10)=5.722215D+00
      E(11)=1.360849D+00
      D(10)=0.2652364D+00
      D(11)=0.8558605D+00
      E(12)=0.3226516D+00
      D(12)=1.0D+00
      E(13)=0.5168015D+00
      E(14)=0.06721404D+00
      S(13)=-0.2626780D+00
      S(14)=1.108079D+00
      P(13)=3.270567D-04
      P(14)=0.9998935D+00
      E(15)=0.02598452D+00
      S(15)=1.0D+00
      P(15)=1.0D+00
      RETURN
C
C     TITANIUM
C
  200 CONTINUE
      E( 1)=2.335020D+03
      E( 2)=3.530441D+02
      E( 3)=7.705845D+01
      S( 1)=6.421660D-02
      S( 2)=3.784120D-01
      S( 3)=6.796813D-01
      E( 4)=9.957387D+01
      E( 5)=2.154671D+01
      E( 6)=6.413965D+00
      S( 4)=-0.1094719D+00
      S( 5)=0.1019427D+00
      S( 6)=0.9546224D+00
      P( 4)=0.1372966D+00
      P( 5)=0.5458753D+00
      P( 6)=0.4890681D+00
      E( 7)=6.238279D+00
      E( 8)=1.996108D+00
      E( 9)=0.6464899D+00
      S( 7)=-0.2861372D+00
      S( 8)=0.3218278D+00
      S( 9)=0.8595511D+00
      P( 7)=0.01923665D+00
      P( 8)=0.4404422D+00
      P( 9)=0.6356195D+00
      E(10)=7.083666D+00
      E(11)=1.709634D+00
      D(10)=0.262921D+00
      D(11)=0.8557721D+00
      E(12)=0.4141225D+00
      D(12)=1.0D+00
      E(13)=0.5732849D+00
      E(14)=0.07311942D+00
      S(13)=-0.24245D+00
      S(14)=1.100075D+00
      P(13)=2.920158D-04
      P(14)=0.9999067D+00
      E(15)=0.02653794D+00
      S(15)=1.0D+00
      P(15)=1.0D+00
      RETURN
C
C     VANADIUM
C
  300 CONTINUE
      E( 1)=2.563877D+03
      E( 2)=3.875340D+02
      E( 3)=8.459823D+01
      S( 1)=0.06394750D+00
      S( 2)=0.3775940D+00
      S( 3)=0.6805421D+00
      E( 4)=1.097938D+02
      E( 5)=2.376921D+01
      E( 6)=7.122961D+00
      S( 4)=-0.1098355D+00
      S( 5)=0.1007070D+00
      S( 6)=0.9556327D+00
      P( 4)=0.1384210D+00
      P( 5)=0.5504894D+00
      P( 6)=0.4824165D+00
      E( 7)=6.981204D+00
      E( 8)=2.219839D+00
      E( 9)=0.719803D+00
      S( 7)=-0.2884588D+00
      S( 8)=0.3364357D+00
      S( 9)=0.8481903D+00
      P( 7)=0.02182075D+00
      P( 8)=0.4567616D+00
      P( 9)=0.6186750D+00
      E(10)=8.342917D+00
      E(11)=2.032944D+00
      D(10)=0.264062D+00
      D(11)=0.8539665D+00
      E(12)=0.4957115D+00
      D(12)=1.0D+00
      E(13)=0.631262D+00
      E(14)=0.08006166D+00
      S(13)=-0.2364899D+00
      S(14)=1.097721D+00
      P(13)=1.899536D-04
      P(14)=0.9999396D+00
      E(15)=0.02886489D+00
      S(15)=1.0D+00
      P(15)=1.0D+00
      RETURN
C
C     CHROMIUM
C
  400 CONTINUE
      E( 1)=2.798294D+03
      E( 2)=4.231370D+02
      E( 3)=9.243886D+01
      S( 1)=0.06382380D+00
      S( 2)=0.3770840D+00
      S( 3)=0.6809889D+00
      E( 4)=1.202806D+02
      E( 5)=2.603727D+01
      E( 6)=7.844172D+00
      S( 4)=-0.1177790D+00
      S( 5)=0.1014311D+00
      S( 6)=0.9571981D+00
      P( 4)=0.1398782D+00
      P( 5)=0.5559834D+00
      P( 6)=0.4748183D+00
      E( 7)=7.793276D+00
      E( 8)=2.497196D+00
      E( 9)=0.8051419D+00
      S( 7)=-0.2888567D+00
      S( 8)=0.3351147D+00
      S( 9)=0.8502481D+00
      P( 7)=0.02218471D+00
      P( 8)=0.4616250D+00
      P( 9)=0.6145386D+00
      E(10)=9.625339D+00
      E(11)=2.362264D+00
      D(10)=0.2655959D+00
      D(11)=0.8521557D+00
      E(12)=0.5770944D+00
      D(12)=1.0D+00
      E(13)=0.7039206D+00
      E(14)=0.08616195D+00
      S(13)=-0.2322508D+00
      S(14)=1.093671D+00
      P(13)=1.799645D-04
      P(14)=0.9999448D+00
      E(15)=0.03219882D+00
      S(15)=1.0D+00
      P(15)=1.0D+00
      RETURN
C
C     MANGANESE
C
  500 CONTINUE
      E( 1)=3.041686D+03
      E( 2)=4.600901D+02
      E( 3)=1.005958D+02
      S( 1)=0.06374490D+00
      S( 2)=0.3767490D+00
      S( 3)=0.6812474D+00
      E( 4)=1.317673D+02
      E( 5)=2.856915D+01
      E( 6)=8.660501D+00
      S( 4)=-0.1102964D+00
      S( 5)=9.818963D-02
      S( 6)=0.9576595D+00
      P( 4)=0.1404540D+00
      P( 5)=0.5578022D+00
      P( 6)=0.4715006D+00
      E( 7)=8.569081D+00
      E( 8)=2.768178D+00
      E( 9)=0.8872882D+00
      S( 7)=-0.2917135D+00
      S( 8)=0.3439630D+00
      S( 9)=0.8451975D+00
      P( 7)=0.02422379D+00
      P( 8)=0.4686598D+00
      P( 9)=0.6074211D+00
      E(10)=1.106884D+01
      E(11)=2.730707D+00
      D(10)=0.2652718D+00
      D(11)=0.8517945D+00
      E(12)=0.6685095D+00
      D(12)=1.0D+00
      E(13)=0.7674426D+00
      E(14)=0.09202527D+00
      S(13)=-0.2300039D+00
      S(14)=1.091450D+00
      P(13)=3.078886D-04
      P(14)=0.9999074D+00
      E(15)=0.0332649D+00
      S(15)=1.0D+00
      P(15)=1.0D+00
      RETURN
C
C     IRON
C
  600 CONTINUE
      E( 1)=3.299184D+03
      E( 2)=4.990886D+02
      E( 3)=1.091614D+02
      S( 1)=0.06358590D+00
      S( 2)=0.3762016D+00
      S( 3)=0.6817845D+00
      E( 4)=1.434652D+02
      E( 5)=3.116858D+01
      E( 6)=9.483612D+00
      S( 4)=-0.1105517D+00
      S( 5)=9.684681D-02
      S( 6)=0.9587974D+00
      P( 4)=0.1411006D+00
      P( 5)=0.5603874D+00
      P( 6)=0.4676444D+00
      E( 7)=9.464565D+00
      E( 8)=3.100373D+00
      E( 9)=0.9864930D+00
      S( 7)=-0.2920555D+00
      S( 8)=0.3375236D+00
      S( 9)=0.8519416D+00
      P( 7)=0.02376201D+00
      P( 8)=0.4689113D+00
      P( 9)=0.6083113D+00
      E(10)=1.235449D+01
      E(11)=3.055605D+00
      D(10)=0.2686110D+00
      D(11)=0.8492717D+00
      E(12)=0.7385909D+00
      D(12)=1.0D+00
      E(13)=0.8534123D+00
      E(14)=0.09881222D+00
      S(13)=-0.2279441D+00
      S(14)=1.088287D+00
      P(13)=-4.262652D-04
      P(14)=1.000124D+00
      E(15)=0.03644214D+00
      S(15)=1.0D+00
      P(15)=1.0D+00
      RETURN
C
C     COBALT
C
  700 CONTINUE
      E( 1)=3.564762D+03
      E( 2)=5.393908D+02
      E( 3)=1.180449D+02
      S( 1)=0.06348660D+00
      S( 2)=0.3758181D+00
      S( 3)=0.6821217D+00
      E( 4)=1.554382D+02
      E( 5)=3.381561D+01
      E( 6)=1.033323D+01
      S( 4)=-0.1109867D+00
      S( 5)=0.09676742D+00
      S( 6)=0.9589921D+00
      P( 4)=0.1420642D+00
      P( 5)=0.5634439D+00
      P( 6)=0.4630244D+00
      E( 7)=10.38152D+00
      E( 8)=3.382714D+00
      E( 9)=1.076954D+00
      S( 7)=-0.2922622D+00
      S( 8)=0.3432507D+00
      S( 9)=0.8469634D+00
      P( 7)=0.02631326D+00
      P( 8)=0.4769170D+00
      P( 9)=0.5991543D+00
      E(10)=1.374070D+01
      E(11)=3.408983D+00
      D(10)=0.2709550D+00
      D(11)=0.8473421D+00
      E(12)=0.8186409D+00
      D(12)=1.0D+00
      E(13)=0.9090155D+00
      E(14)=0.1050406D+00
      S(13)=-0.2174599D+00
      S(14)=1.084998D+00
      P(13)=2.284428D-04
      P(14)=0.9999337D+00
      E(15)=0.03725658D+00
      S(15)=1.0D+00
      P(15)=1.0D+00
      RETURN
C
C     NICKEL
C
  800 CONTINUE
      E( 1)=3.848005D+03
      E( 2)=5.820307D+02
      E( 3)=1.273674D+02
      S( 1)=0.06326610D+00
      S( 2)=0.3751710D+00
      S( 3)=0.6828238D+00
      E( 4)=1.682896D+02
      E( 5)=3.665633D+01
      E( 6)=1.123212D+01
      S( 4)=-0.1111151D+00
      S( 5)=0.09532380D+00
      S( 6)=0.9601613D+00
      P( 4)=0.1424905D+00
      P( 5)=0.5655470D+00
      P( 6)=0.4599926D+00
      E( 7)=1.135877D+01
      E( 8)=3.738846D+00
      E( 9)=1.182463D+00
      S( 7)=-0.2920604D+00
      S( 8)=0.3375407D+00
      S( 9)=0.8525330D+00
      P( 7)=0.02613762D+00
      P( 8)=0.4765980D+00
      P( 9)=0.6003798D+00
      E(10)=1.522069D+01
      E(11)=3.786020D+00
      D(10)=0.2726060D+00
      D(11)=0.8459279D+00
      E(12)=0.9045574D+00
      D(12)=1.0D+00
      E(13)=0.9889038D+00
      E(14)=0.1110250D+00
      S(13)=-0.2136872D+00
      S(14)=1.081933D+00
      P(13)=2.943514D-04
      P(14)=0.9999170D+00
      E(15)=0.03925822D+00
      S(15)=1.0D+00
      P(15)=1.0D+00
      RETURN
C
C     COPPER
C     THE DATA FOR 4SP' AND 4SP'' ARE CORRECTED VALUES, NOT THE
C     PUBLISHED VALUES, AS PROVIDED BY KERWIN DOBBS, MAY 1991.
C
  900 CONTINUE
      E( 1)=4.134302D+03
      E( 2)=6.254912D+02
      E( 3)=1.369556D+02
      S( 1)=0.06318780D+00
      S( 2)=0.3748448D+00
      S( 3)=0.6831002D+00
      E( 4)=1.814960D+02
      E( 5)=3.957431D+01
      E( 6)=1.216246D+01
      S( 4)=-0.1113198D+00
      S( 5)=0.09448679D+00
      S( 6)=0.9608790D+00
      P( 4)=0.1430844D+00
      P( 5)=0.5677561D+00
      P( 6)=0.4567141D+00
      E( 7)=1.235111D+01
      E( 8)=4.049651D+00
      E( 9)=1.279225D+00
      S( 7)=-0.2922231D+00
      S( 8)=0.3429909D+00
      S( 9)=0.8479463D+00
      P( 7)=0.02772714D+00
      P( 8)=0.4835244D+00
      P( 9)=0.5929779D+00
      E(10)=1.675938D+01
      E(11)=4.178977D+00
      D(10)=0.2741125D+00
      D(11)=0.8446245D+00
      E(12)=0.9943270D+00
      D(12)=1.0D+00
      E(13)=1.0482994D+00
      E(14)=0.117118022D+00
      S(13)=-0.205498090D+00
      S(14)=1.07915864D+00
      P(13)=1.98490005D-04
      P(14)=0.999944328D+00
      E(15)=0.0405449869D+00
      S(15)=1.0D+00
      P(15)=1.0D+00
      RETURN
C
C     ZINC
C
 1000 CONTINUE
      E( 1)=4.432288D+03
      E( 2)=6.706601D+02
      E( 3)=1.469024D+02
      S( 1)=0.06309280D+00
      S( 2)=0.3745038D+00
      S( 3)=0.6834160D+00
      E( 4)=1.950042D+02
      E( 5)=4.256889D+01
      E( 6)=1.312143D+01
      S( 4)=-0.1116283D+00
      S( 5)=0.09433553D+00
      S( 6)=0.9611002D+00
      P( 4)=0.1438055D+00
      P( 5)=0.5700019D+00
      P( 6)=0.4533119D+00
      E( 7)=1.340231D+01
      E( 8)=4.399906D+00
      E( 9)=1.385148D+00
      S( 7)=-0.2917811D+00
      S( 8)=0.3426145D+00
      S( 9)=0.8482840D+00
      P( 7)=0.02870528D+00
      P( 8)=0.4862515D+00
      P( 9)=0.5902353D+00
      E(10)=1.836820D+01
      E(11)=4.591304D+00
      D(10)=0.2753856D+00
      D(11)=0.8434773D+00
      E(12)=1.090203D+00
      D(12)=1.0D+00
      E(13)=1.121558D+00
      E(14)=0.1229436D+00
      S(13)=-0.2023706D+00
      S(14)=1.077035D+00
      P(13)=3.440941D-04
      P(14)=0.9999053D+00
      E(15)=4.219327D-02
      S(15)=1.0D+00
      P(15)=1.0D+00
      RETURN
      END
C*MODULE BASN21  *DECK N21FIV
      SUBROUTINE N21FIV(E,S,P,D,NUCZ,IGAUSS)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION E(21),S(21),P(21),D(21)
      IF(IGAUSS.NE.3) CALL BERROR(2)
      IF(NUCZ.EQ.37) GO TO 100
      IF(NUCZ.EQ.38) GO TO 200
      IF(NUCZ.LE.48) CALL N21TM2(E,S,P,D,NUCZ)
      IF(NUCZ.LE.48) RETURN
      IBR=NUCZ-48
      GO TO (300,400,500,600,700,800), IBR
C
C     RUBIDIUM
C
  100 CONTINUE
      E( 1)=6816.7225D+00
      E( 2)=1033.0007D+00
      E( 3)=226.90861D+00
      S( 1)=0.0624962D+00
      S( 2)=0.3719500D+00
      S( 3)=0.6857293D+00
      E( 4)=304.1283D+00
      E( 5)=66.26058D+00
      E( 6)=20.91945D+00
      S( 4)=-0.1123296D+00
      S( 5)=0.09075080D+00
      S( 6)=0.9639410D+00
      P( 4)=0.1484409D+00
      P( 5)=0.5891247D+00
      P( 6)=0.4258251D+00
      E( 7)=22.46533D+00
      E( 8)=7.877468D+00
      E( 9)=2.705271D+00
      S( 7)=-0.3004850D+00
      S( 8)=0.2783566D+00
      S( 9)=0.9076099D+00
      P( 7)=0.02445410D+00
      P( 8)=0.4944538D+00
      P( 9)=0.5718567D+00
      E(10)=40.866031D+00
      E(11)=10.840885D+00
      E(12)=3.0508341D+00
      D(10)=0.1466037D+00
      D(11)=0.5127252D+00
      D(12)=0.5699804D+00
      E(13)=2.692116D+00
      E(14)=0.7230563D+00
      E(15)=0.2598383D+00
      S(13)=-0.3311623D+00
      S(14)=0.5096991D+00
      S(15)=0.6982461D+00
      P(13)=0.01190051D+00
      P(14)=0.4951731D+00
      P(15)=0.5737244D+00
      E(16)=1.897140D-01
      E(17)=3.399726D-02
      E(18)=1.471231D-02
      S(16)=-2.711927D-01
      S(17)=1.141550D+00
      S(18)=1.0D+00
      P(16)=3.081009D-04
      P(17)=9.998654D-01
      P(18)=1.0D+00
      RETURN
C
C     STRONTIUM
C
  200 CONTINUE
      E( 1)=7215.4735D+00
      E( 2)=1092.8519D+00
      E( 3)=239.98182D+00
      S( 1)=0.0622818D+00
      S( 2)=0.3713101D+00
      S( 3)=0.6864439D+00
      E( 4)=322.1246D+00
      E( 5)=70.09046D+00
      E( 6)=22.17641D+00
      S( 4)=-0.1122353D+00
      S( 5)=0.08954360D+00
      S( 6)=0.9648135D+00
      P( 4)=0.1488369D+00
      P( 5)=0.5919466D+00
      P( 6)=0.4221715D+00
      E( 7)=23.92763D+00
      E( 8)=8.475114D+00
      E( 9)=2.942934D+00
      S( 7)=-0.3024718D+00
      S( 8)=0.2700841D+00
      S( 9)=0.9159200D+00
      P( 7)=0.02483697D+00
      P( 8)=0.4934775D+00
      P( 9)=0.5709829D+00
      E(10)=44.566115D+00
      E(11)=11.881489D+00
      E(12)=3.3875579D+00
      D(10)=0.1451271D+00
      D(11)=0.5130677D+00
      D(12)=0.5676640D+00
      E(13)=2.940966D+00
      E(14)=0.8523559D+00
      E(15)=0.3215375D+00
      S(13)=-0.3519846D+00
      S(14)=0.4972546D+00
      S(15)=0.7238598D+00
      P(13)=0.009723336D+00
      P(14)=0.4983220D+00
      P(15)=0.5650561D+00
      E(16)=3.480419D-01
      E(17)=4.817715D-02
      E(18)=2.180335D-02
      S(16)=-2.851468D-01
      S(17)=1.120939D+00
      S(18)=1.0D+00
      P(16)=3.081096D-04
      P(17)=9.998935D-01
      P(18)=1.0D+00
      RETURN
C
C     INDIUM
C
  300 CONTINUE
      E( 1)=12214.547D+00
      E( 2)=1848.9136D+00
      E( 3)=406.36833D+00
      S( 1)=0.0612476D+00
      S( 2)=0.3676754D+00
      S( 3)=0.6901359D+00
      E( 4)=550.4423D+00
      E( 5)=119.7744D+00
      E( 6)=38.66927D+00
      S( 4)=-0.1127094D+00
      S( 5)=0.08344350D+00
      S( 6)=0.9696880D+00
      P( 4)=0.1523703D+00
      P( 5)=0.6096508D+00
      P( 6)=0.3970249D+00
      E( 7)=47.02931D+00
      E( 8)=22.49642D+00
      E( 9)=6.697117D+00
      S( 7)=-0.2758954D+00
      S( 8)=0.05977348D+00
      S( 9)=1.082148D+00
      P( 7)=-0.1408485D+00
      P( 8)=0.5290867D+00
      P( 9)=0.6620681D+00
      E(10)=102.17356D+00
      E(11)=28.394632D+00
      E(12)=8.9248045D+00
      D(10)=0.1205559D+00
      D(11)=0.4884976D+00
      D(12)=0.5850190D+00
      E(13)=6.572360D+00
      E(14)=2.502158D+00
      E(15)=0.9420246D+00
      S(13)=-0.4284831D+00
      S(14)=0.4633644D+00
      S(15)=0.8219679D+00
      P(13)=0.01091305D+00
      P(14)=0.5036759D+00
      P(15)=0.5581809D+00
      E(16)=4.5353637D+00
      E(17)=1.5371481D+00
      E(18)=0.49949226D+00
      D(16)=0.2508574D+00
      D(17)=0.5693113D+00
      D(18)=0.3840635D+00
      E(19)=1.001221D+00
      E(20)=0.1659704D+00
      S(19)=-0.4364172D+00
      S(20)=1.189893D+00
      P(19)=0.02316334D+00
      P(20)=0.9903309D+00
      E(21)=0.05433974D+00
      S(21)=1.0D+00
      P(21)=1.0D+00
      RETURN
C
C     TIN
C
  400 CONTINUE
      E( 1)=12741.674D+00
      E( 2)=1928.4692D+00
      E( 3)=423.80797D+00
      S( 1)=0.0611353D+00
      S( 2)=0.3672929D+00
      S( 3)=0.6905447D+00
      E( 4)=574.2875D+00
      E( 5)=124.9537D+00
      E( 6)=40.39576D+00
      S( 4)=-0.1127462D+00
      S( 5)=0.08286347D+00
      S( 6)=0.9701505D+00
      P( 4)=0.1525798D+00
      P( 5)=0.6110106D+00
      P( 6)=0.3951549D+00
      E( 7)=48.80662D+00
      E( 8)=23.83588D+00
      E( 9)=7.048296D+00
      S( 7)=-0.2824534D+00
      S( 8)=0.06605595D+00
      S( 9)=1.081987D+00
      P( 7)=-0.1509628D+00
      P( 8)=0.5399677D+00
      P( 9)=0.6604823D+00
      E(10)=108.05630D+00
      E(11)=30.131576D+00
      E(12)=9.5300359D+00
      D(10)=0.1198237D+00
      D(11)=0.4875910D+00
      D(12)=0.5849874D+00
      E(13)=6.973378D+00
      E(14)=2.693040D+00
      E(15)=1.025958D+00
      S(13)=-0.4340356D+00
      S(14)=0.4610286D+00
      S(15)=0.8285579D+00
      P(13)=0.01195130D+00
      P(14)=0.5067195D+00
      P(15)=0.5529106D+00
      E(16)=4.9626098D+00
      E(17)=1.7120829D+00
      E(18)=0.57719451D+00
      D(16)=0.2529487D+00
      D(17)=0.5727612D+00
      D(18)=0.3690387D+00
      E(19)=1.131463D+00
      E(20)=0.2034092D+00
      E(21)=0.07056383D+00
      S(19)=-0.5252085D+00
      S(20)=1.229226D+00
      S(21)=1.0D+00
      P(19)=0.02107053D+00
      P(20)=0.9905914D+00
      P(21)=1.0D+00
      RETURN
C
C     ANTIMONY
C
  500 CONTINUE
      E(1)=13289.383D+00
      E(2)=2010.5218D+00
      E(3)=441.69815D+00
      S(1)=0.0609843D+00
      S(2)=0.3668487D+00
      S(3)=0.6910501D+00
      E(4)=598.8890D+00
      E(5)=130.0385D+00
      E(6)=42.13286D+00
      S(4)=-0.1127201D+00
      S(5)=0.08264433D+00
      S(6)=0.9702579D+00
      P(4)=0.1530671D+00
      P(5)=0.6135972D+00
      P(6)=0.3916990D+00
      E(7)=51.51333D+00
      E(8)=24.43595D+00
      E(9)=7.420931D+00
      S(7)=-0.2770433D+00
      S(8)=0.05750323D+00
      S(9)=1.084703D+00
      P(7)=-0.1378699D+00
      P(8)=0.536355D+00
      P(9)=0.6508676D+00
      E(10)=115.80955D+00
      E(11)=32.305835D+00
      E(12)=10.250328D+00
      D(10)=0.1166279D+00
      D(11)=0.4834363D+00
      D(12)=0.5901395D+00
      E(13)=7.314235D+00
      E(14)=2.844053D+00
      E(15)=1.105855D+00
      S(13)=-0.4403812D+00
      S(14)=0.4737341D+00
      S(15)=0.822135D+00
      P(13)=0.01530518D+00
      P(14)=0.5160832D+00
      P(15)=0.538757D+00
      E(16)=5.4862102D+00
      E(17)=1.9216196D+00
      E(18)=0.66606265D+00
      D(16)=0.2483656D+00
      D(17)=0.5743154D+00
      D(18)=0.3643044D+00
      E(19)=1.278637D+00
      E(20)=0.2412321D+00
      E(21)=0.08662967D+00
      S(19)=-0.6016951D+00
      S(20)=1.258692D+00
      S(21)=1.0D+00
      P(19)=0.02225276D+00
      P(20)=0.9896434D+00
      P(21)=1.0D+00
      RETURN
C
C     TELLURIUM
C
  600 CONTINUE
      E(1)=13796.56D+00
      E(2)=2088.8798D+00
      E(3)=459.39319D+00
      S(1)=0.0610862D+00
      S(2)=0.3669629D+00
      S(3)=0.6907944D+00
      E(4)=623.2631D+00
      E(5)=135.36D+00
      E(6)=44.00048D+00
      S(4)=-0.11282D+00
      S(5)=0.08225243D+00
      S(6)=0.9706007D+00
      P(4)=0.1534195D+00
      P(5)=0.6148996D+00
      P(6)=0.3895162D+00
      E(7)=54.19078D+00
      E(8)=25.82039D+00
      E(9)=7.809583D+00
      S(7)=-0.274412D+00
      S(8)=0.05182968D+00
      S(9)=1.087622D+00
      P(7)=-0.1433312D+00
      P(8)=0.5391881D+00
      P(9)=0.6522851D+00
      E(10)=121.4083D+00
      E(11)=34.015217D+00
      E(12)=10.869138D+00
      D(10)=0.1169136D+00
      D(11)=0.4835555D+00
      D(12)=0.5883864D+00
      E(13)=7.764217D+00
      E(14)=3.043932D+00
      E(15)=1.199253D+00
      S(13)=-0.4467306D+00
      S(14)=0.4694704D+00
      S(15)=0.8298105D+00
      P(13)=0.0127462D+00
      P(14)=0.5221231D+00
      P(15)=0.5326614D+00
      E(16)=5.8031113D+00
      E(17)=2.0580658D+00
      E(18)=0.73283021D+00
      D(16)=0.2601939D+00
      D(17)=0.5797758D+00
      D(18)=0.3405580D+00
      E(19)=1.340364D+00
      E(20)=0.2780884D+00
      E(21)=0.09672607D+00
      S(19)=-0.5904699D+00
      S(20)=1.281968D+00
      S(21)=1.0D+00
      P(19)=0.02558834D+00
      P(20)=0.9871016D+00
      P(21)=1.0D+00
      RETURN
C
C     IODINE
C
  700 CONTINUE
      E( 1)=14351.186D+00
      E( 2)=2173.0741D+00
      E( 3)=477.87205D+00
      S( 1)=0.0610028D+00
      S( 2)=0.3666398D+00
      S( 3)=0.6911306D+00
      E( 4)=648.1887D+00
      E( 5)=140.3064D+00
      E( 6)=45.69880D+00
      S( 4)=-0.1128507D+00
      S( 5)=0.08322834D+00
      S( 6)=0.9697516D+00
      P( 4)=0.1541145D+00
      P( 5)=0.6194618D+00
      P( 6)=0.3837583D+00
      E( 7)=56.69469D+00
      E( 8)=27.48875D+00
      E( 9)=8.209096D+00
      S( 7)=-0.2736965D+00
      S( 8)=0.04649968D+00
      S( 9)=1.091576D+00
      P( 7)=-0.1523217D+00
      P( 8)=0.5437686D+00
      P( 9)=0.6561679D+00
      E(10)=128.09026D+00
      E(11)=35.982378D+00
      E(12)=11.551116D+00
      D(10)=0.1158636D+00
      D(11)=0.4820494D+00
      D(12)=0.5894448D+00
      E(13)=8.191679D+00
      E(14)=3.244596D+00
      E(15)=1.300489D+00
      S(13)=-0.4508277D+00
      S(14)=0.4632094D+00
      S(15)=0.8386360D+00
      P(13)=0.01186988D+00
      P(14)=0.5265246D+00
      P(15)=0.5266076D+00
      E(16)=6.1461523D+00
      E(17)=2.2209370D+00
      E(18)=0.80991202D+00
      D(16)=0.2681817D+00
      D(17)=0.5800614D+00
      D(18)=0.3262263D+00
      E(19)=1.451380D+00
      E(20)=0.3281033D+00
      S(19)=-0.6658515D+00
      S(20)=1.328584D+00
      P(19)=0.02754131D+00
      P(20)=0.9851368D+00
      E(21)=0.1150759D+00
      S(21)=1.0D+00
      P(21)=1.0D+00
      RETURN
C
C     XENON
C
  800 CONTINUE
      E( 1)=14902.236D+00
      E( 2)=2256.5383D+00
      E( 3)=496.37317D+00
      S( 1)=0.0609969D+00
      S( 2)=0.3666290D+00
      S( 3)=0.6911155D+00
      E( 4)=673.6611D+00
      E( 5)=145.8491D+00
      E( 6)=47.57708D+00
      S( 4)=-0.1129128D+00
      S( 5)=0.08290529D+00
      S( 6)=0.9700289D+00
      P( 4)=0.1544275D+00
      P( 5)=0.6206174D+00
      P( 6)=0.3820041D+00
      E( 7)=59.16752D+00
      E( 8)=28.61159D+00
      E( 9)=8.596596D+00
      S( 7)=-0.2739774D+00
      S( 8)=0.04553006D+00
      S( 9)=1.092528D+00
      P( 7)=-0.1518572D+00
      P( 8)=0.5471507D+00
      P( 9)=0.6519540D+00
      E(10)=134.91331D+00
      E(11)=37.956387D+00
      E(12)=12.227475D+00
      D(10)=0.1150105D+00
      D(11)=0.4815952D+00
      D(12)=0.5896133D+00
      E(13)=8.638676D+00
      E(14)=3.462818D+00
      E(15)=1.401040D+00
      S(13)=-0.4558408D+00
      S(14)=0.4617355D+00
      S(15)=0.8442883D+00
      P(13)=0.009585056D+00
      P(14)=0.5298193D+00
      P(15)=0.5235985D+00
      E(16)=6.6004928D+00
      E(17)=2.3980513D+00
      E(18)=0.88648239D+00
      D(16)=0.2718844D+00
      D(17)=0.5855569D+00
      D(18)=0.3127456D+00
      E(19)=1.578474D+00
      E(20)=0.3750814D+00
      E(21)=0.1331790D+00
      S(19)=-0.7277718D+00
      S(20)=1.362797D+00
      S(21)=1.0D+00
      P(19)=0.02807244D+00
      P(20)=0.9842642D+00
      P(21)=1.0D+00
      RETURN
      END
C*MODULE BASN21  *DECK N21TM2
      SUBROUTINE N21TM2(E,S,P,D,NUCZ)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION E(21),S(21),P(21),D(21)
C
C     SECOND TRANSITION SERIES
C
      IBR=NUCZ-38
      IF(IBR.LT.1.OR.IBR.GT.10) CALL BERROR(2)
      GO TO (139,140,141,142,143,144,145,146,147,148), IBR
C
C     YTTRIUM
C
  139 CONTINUE
      E( 1)=7.646421D+03
      E( 2)=1.156863D+03
      E( 3)=2.537152D+02
      S( 1)=6.189050D-02
      S( 2)=3.702068D-01
      S( 3)=6.877560D-01
      E( 4)=3.418540D+02
      E( 5)=7.420986D+01
      E( 6)=2.351352D+01
      S( 4)=-1.119001D-01
      S( 5)=8.680524D-02
      S( 6)=9.667847D-01
      P( 4)=1.485717D-01
      P( 5)=5.943067D-01
      P( 6)=4.196040D-01
      E( 7)=1.886260D+01
      E( 8)=1.645405D+01
      E( 9)=3.484500D+00
      S( 7)=-1.477873D+00
      S( 8)=1.347259D+00
      S( 9)=1.006231D+00
      P( 7)=-7.041407D-01
      P( 8)=1.057862D+00
      P( 9)=7.393821D-01
      E(10)=5.035375D+01
      E(11)=1.353078D+01
      E(12)=3.944996D+00
      D(10)=1.367956D-01
      D(11)=5.019062D-01
      D(12)=5.788598D-01
      E(13)=3.221733D+00
      E(14)=1.050705D+00
      E(15)=3.925923D-01
      S(13)=-3.699580D-01
      S(14)=4.308639D-01
      S(15)=8.020874D-01
      P(13)=2.494435D-03
      P(14)=4.537623D-01
      P(15)=6.130679D-01
      E(16)=1.530137D+00
      E(17)=6.300673D-01
      D(16)=3.384033D-01
      D(17)=7.293289D-01
      E(18)=2.165884D-01
      D(18)=1.0D+00
      E(19)=4.327637D-01
      E(20)=5.701219D-02
      S(19)=-3.464582D-01
      S(20)=1.132777D+00
      P(19)=-1.336559D-03
      P(20)=1.000440D+00
      E(21)=2.375370D-02
      S(21)=1.0D+00
      P(21)=1.0D+00
      RETURN
C
C     ZIRCONIUM
C
  140 CONTINUE
      E( 1)=8.084592D+03
      E( 2)=1.221668D+03
      E( 3)=2.676917D+02
      S( 1)=6.157760D-02
      S( 2)=3.693989D-01
      S( 3)=6.887280D-01
      E( 4)=3.610212D+02
      E( 5)=7.830495D+01
      E( 6)=2.484523D+01
      S( 4)=-1.119067D-01
      S( 5)=8.583993D-02
      S( 6)=9.675130D-01
      P( 4)=1.487616D-01
      P( 5)=5.965690D-01
      P( 6)=4.167849D-01
      E( 7)=2.000628D+01
      E( 8)=1.757415D+01
      E( 9)=3.742985D+00
      S( 7)=-1.544349D+00
      S( 8)=1.409596D+00
      S( 9)=1.009349D+00
      P( 7)=-7.568480D-01
      P( 8)=1.112090D+00
      P( 9)=7.368024D-01
      E(10)=5.472323D+01
      E(11)=1.477416D+01
      E(12)=4.358961D+00
      D(10)=1.348240D-01
      D(11)=5.005544D-01
      D(12)=5.787823D-01
      E(13)=3.554788D+00
      E(14)=1.178992D+00
      E(15)=4.446966D-01
      S(13)=-3.793873D-01
      S(14)=4.232847D-01
      S(15)=8.140676D-01
      P(13)=2.599455D-03
      P(14)=4.599758D-01
      P(15)=6.058485D-01
      E(16)=1.862842D+00
      E(17)=6.433135D-01
      D(16)=2.850320D-01
      D(17)=7.972074D-01
      E(18)=1.993954D-01
      D(18)=1.0D+00
      E(19)=5.050488D-01
      E(20)=6.211612D-02
      S(19)=-3.295118D-01
      S(20)=1.120709D+00
      P(19)=-1.248930D-03
      P(20)=1.000384D+00
      E(21)=2.557955D-02
      S(21)=1.0D+00
      P(21)=1.0D+00
      RETURN
C
C     NIOBIUM
C
  141 CONTINUE
      E( 1)=8.466517D+03
      E( 2)=1.281261D+03
      E( 3)=2.812311D+02
      S( 1)=6.180380D-02
      S( 2)=3.698049D-01
      S( 3)=6.880795D-01
      E( 4)=3.794729D+02
      E( 5)=8.233589D+01
      E( 6)=2.622248D+01
      S( 4)=-1.121063D-01
      S( 5)=8.650279D-02
      S( 6)=9.670574D-01
      P( 4)=1.496674D-01
      P( 5)=5.987182D-01
      P( 6)=4.132382D-01
      E( 7)=2.116294D+01
      E( 8)=1.858978D+01
      E( 9)=4.009981D+00
      S( 7)=-1.555131D+00
      S( 8)=1.417939D+00
      S( 9)=1.010620D+00
      P( 7)=-7.554622D-01
      P( 8)=1.113966D+00
      P( 9)=7.327672D-01
      E(10)=5.901219D+01
      E(11)=1.601279D+01
      E(12)=4.777185D+00
      D(10)=1.337104D-01
      D(11)=5.000390D-01
      D(12)=5.775384D-01
      E(13)=3.836375D+00
      E(14)=1.303325D+00
      E(15)=4.934306D-01
      S(13)=-3.891037D-01
      S(14)=4.349696D-01
      S(15)=8.115899D-01
      P(13)=3.290347D-03
      P(14)=4.716451D-01
      P(15)=5.936990D-01
      E(16)=1.970443D+00
      E(17)=6.619347D-01
      D(16)=3.106809D-01
      D(17)=7.800691D-01
      E(18)=2.059972D-01
      D(18)=1.0D+00
      E(19)=5.723734D-01
      E(20)=6.820320D-02
      S(19)=-3.156094D-01
      S(20)=1.114047D+00
      P(19)=-1.133018D-03
      P(20)=1.000338D+00
      E(21)=2.715715D-02
      S(21)=1.0D+00
      P(21)=1.0D+00
      RETURN
C
C     MOLYBDENUM
C
  142 CONTINUE
      E( 1)=8.899491D+03
      E( 2)=1.346764D+03
      E( 3)=2.956352D+02
      S( 1)=6.170640D-02
      S( 2)=3.694536D-01
      S( 3)=6.884343D-01
      E( 4)=3.993139D+02
      E( 5)=8.659356D+01
      E( 6)=2.763904D+01
      S( 4)=-1.121440D-01
      S( 5)=8.601148D-02
      S( 6)=9.674335D-01
      P( 4)=1.500665D-01
      P( 5)=6.007695D-01
      P( 6)=4.103865D-01
      E( 7)=2.250292D+01
      E( 8)=1.949171D+01
      E( 9)=4.278180D+00
      S( 7)=-1.422306D+00
      S( 8)=1.284185D+00
      S( 9)=1.010866D+00
      P( 7)=-6.680661D-01
      P( 8)=1.030346D+00
      P( 9)=7.283480D-01
      E(10)=6.378045D+01
      E(11)=1.737358D+01
      E(12)=5.230784D+00
      D(10)=1.317388D-01
      D(11)=4.985316D-01
      D(12)=5.781775D-01
      E(13)=4.163021D+00
      E(14)=1.435305D+00
      E(15)=5.437821D-01
      S(13)=-3.964233D-01
      S(14)=4.370792D-01
      S(15)=8.148462D-01
      P(13)=2.962629D-03
      P(14)=4.791471D-01
      P(15)=5.864869D-01
      E(16)=2.270937D+00
      E(17)=7.546530D-01
      D(16)=3.112644D-01
      D(17)=7.810342D-01
      E(18)=2.351422D-01
      D(18)=1.0D+00
      E(19)=6.318014D-01
      E(20)=7.325791D-02
      S(19)=-3.033617D-01
      S(20)=1.108413D+00
      P(19)=-1.079133D-03
      P(20)=1.000313D+00
      E(21)=2.802515D-02
      S(21)=1.0D+00
      P(21)=1.0D+00
      RETURN
C
C     TECHNETIUM
C
  143 CONTINUE
      E( 1)=9.329482D+03
      E( 2)=1.412506D+03
      E( 3)=3.102643D+02
      S( 1)=6.171190D-02
      S( 2)=3.693370D-01
      S( 3)=6.884724D-01
      E( 4)=4.188175D+02
      E( 5)=9.125078D+01
      E( 6)=2.911212D+01
      S( 4)=-1.124025D-01
      S( 5)=8.531816D-02
      S( 6)=9.681774D-01
      P( 4)=1.500719D-01
      P( 5)=6.000567D-01
      P( 6)=4.109856D-01
      E( 7)=2.591064D+01
      E( 8)=2.326770D+01
      E( 9)=4.707083D+00
      S( 7)=-1.380446D+00
      S( 8)=1.197900D+00
      S( 9)=1.052649D+00
      P( 7)=-1.655271D+00
      P( 8)=1.986020D+00
      P( 9)=7.290339D-01
      E(10)=6.878375D+01
      E(11)=1.880389D+01
      E(12)=5.705228D+00
      D(10)=1.296930D-01
      D(11)=4.966191D-01
      D(12)=5.795466D-01
      E(13)=4.441138D+00
      E(14)=1.595639D+00
      E(15)=5.955598D-01
      S(13)=-4.041144D-01
      S(14)=4.398379D-01
      S(15)=8.219360D-01
      P(13)=1.229062D-02
      P(14)=4.632067D-01
      P(15)=5.983826D-01
      E(16)=2.599164D+00
      E(17)=8.622757D-01
      D(16)=3.092195D-01
      D(17)=7.829056D-01
      E(18)=2.706073D-01
      D(18)=1.0D+00
      E(19)=6.738812D-01
      E(20)=7.724070D-02
      S(19)=-2.700028D-01
      S(20)=1.099150D+00
      P(19)=-9.197676D-04
      P(20)=1.000264D+00
      E(21)=2.869556D-02
      S(21)=1.0D+00
      P(21)=1.0D+00
      RETURN
C
C     RUTHENIUM
C
  144 CONTINUE
      E( 1)=9.786161D+03
      E( 2)=1.481477D+03
      E( 3)=3.254122D+02
      S( 1)=6.160520D-02
      S( 2)=3.689816D-01
      S( 3)=6.888451D-01
      E( 4)=4.398665D+02
      E( 5)=9.576273D+01
      E( 6)=3.060566D+01
      S( 4)=-1.123912D-01
      S( 5)=8.469449D-02
      S( 6)=9.686379D-01
      P( 4)=1.503791D-01
      P( 5)=6.019294D-01
      P( 6)=4.084639D-01
      E( 7)=2.727737D+01
      E( 8)=2.451082D+01
      E( 9)=5.008946D+00
      S( 7)=-1.395553D+00
      S( 8)=1.210852D+00
      S( 9)=1.054045D+00
      P( 7)=-1.668618D+00
      P( 8)=2.002799D+00
      P( 9)=7.251435D-01
      E(10)=7.398330D+01
      E(11)=2.028149D+01
      E(12)=6.194298D+00
      D(10)=1.277598D-01
      D(11)=4.951474D-01
      D(12)=5.806552D-01
      E(13)=4.765812D+00
      E(14)=1.734531D+00
      E(15)=6.466355D-01
      S(13)=-4.103627D-01
      S(14)=4.480025D-01
      S(15)=8.198083D-01
      P(13)=1.127417D-02
      P(14)=4.727032D-01
      P(15)=5.898430D-01
      E(16)=2.889108D+00
      E(17)=9.539610D-01
      D(16)=3.159938D-01
      D(17)=7.780656D-01
      E(18)=2.958807D-01
      D(18)=1.0D+00
      E(19)=7.406620D-01
      E(20)=8.217096D-02
      S(19)=-2.639655D-01
      S(20)=1.094857D+00
      P(19)=-7.620441D-04
      P(20)=1.000212D+00
      E(21)=3.009659D-02
      S(21)=1.0D+00
      P(21)=1.0D+00
      RETURN
C
C     RHODIUM
C
  145 CONTINUE
      E( 1)=1.021771D+04
      E( 2)=1.548412D+03
      E( 3)=3.404990D+02
      S( 1)=6.173240D-02
      S( 2)=3.691533D-01
      S( 3)=6.885138D-01
      E( 4)=4.607593D+02
      E( 5)=1.003289D+02
      E( 6)=3.213971D+01
      S( 4)=-1.124461D-01
      S( 5)=8.438113D-02
      S( 6)=9.689016D-01
      P( 4)=1.508582D-01
      P( 5)=6.035139D-01
      P( 6)=4.060249D-01
      E( 7)=2.879329D+01
      E( 8)=2.591768D+01
      E( 9)=5.320640D+00
      S( 7)=-1.404091D+00
      S( 8)=1.216169D+00
      S( 9)=1.056555D+00
      P( 7)=-1.712218D+00
      P( 8)=2.047603D+00
      P( 9)=7.229831D-01
      E(10)=7.925597D+01
      E(11)=2.178945D+01
      E(12)=6.697518D+00
      D(10)=1.261896D-01
      D(11)=4.939541D-01
      D(12)=5.813296D-01
      E(13)=5.109748D+00
      E(14)=1.875414D+00
      E(15)=6.995578D-01
      S(13)=-4.126469D-01
      S(14)=4.518853D-01
      S(15)=8.188989D-01
      P(13)=9.374064D-03
      P(14)=4.815544D-01
      P(15)=5.822312D-01
      E(16)=3.190908D+00
      E(17)=1.054575D+00
      D(16)=3.210399D-01
      D(17)=7.738518D-01
      E(18)=3.260791D-01
      D(18)=1.0D+00
      E(19)=8.005711D-01
      E(20)=8.732134D-02
      S(19)=-2.553480D-01
      S(20)=1.091308D+00
      P(19)=-7.759711D-04
      P(20)=1.000212D+00
      E(21)=3.140693D-02
      S(21)=1.0D+00
      P(21)=1.0D+00
      RETURN
C
C     PALLADIUM
C
  146 CONTINUE
      E( 1)=1.072874D+04
      E( 2)=1.624074D+03
      E( 3)=3.567937D+02
      S( 1)=6.142950D-02
      S( 2)=3.683282D-01
      S( 3)=6.895025D-01
      E( 4)=4.824783D+02
      E( 5)=1.050590D+02
      E( 6)=3.368145D+01
      S( 4)=-1.126789D-01
      S( 5)=8.461197D-02
      S( 6)=9.687868D-01
      P( 4)=1.510793D-01
      P( 5)=6.050916D-01
      P( 6)=4.039804D-01
      E( 7)=3.018654D+01
      E( 8)=2.716642D+01
      E( 9)=5.635934D+00
      S( 7)=-1.418547D+00
      S( 8)=1.229444D+00
      S( 9)=1.057083D+00
      P( 7)=-1.709817D+00
      P( 8)=2.049308D+00
      P( 9)=7.186296D-01
      E(10)=8.423691D+01
      E(11)=2.324919D+01
      E(12)=7.196760D+00
      D(10)=1.256429D-01
      D(11)=4.937201D-01
      D(12)=5.803431D-01
      E(13)=5.475374D+00
      E(14)=1.997604D+00
      E(15)=7.439302D-01
      S(13)=-4.172610D-01
      S(14)=4.705878D-01
      S(15)=8.046363D-01
      P(13)=1.158391D-02
      P(14)=4.974548D-01
      P(15)=5.655272D-01
      E(16)=3.473077D+00
      E(17)=1.148050D+00
      D(16)=3.281543D-01
      D(17)=7.680267D-01
      E(18)=3.548106D-01
      D(18)=1.0D+00
      E(19)=8.901632D-01
      E(20)=9.282090D-02
      S(19)=-2.784324D-01
      S(20)=1.093028D+00
      P(19)=-1.271580D-03
      P(20)=1.000332D+00
      E(21)=3.377394D-02
      S(21)=1.0D+00
      P(21)=1.0D+00
      RETURN
C
C     SILVER
C
  147 CONTINUE
      E( 1)=1.119078D+04
      E( 2)=1.695077D+03
      E( 3)=3.726752D+02
      S( 1)=6.149480D-02
      S( 2)=3.684053D-01
      S( 3)=6.893247D-01
      E( 4)=5.046162D+02
      E( 5)=1.098718D+02
      E( 6)=3.529513D+01
      S( 4)=-1.126577D-01
      S( 5)=8.402784D-02
      S( 6)=9.692344D-01
      P( 4)=1.514798D-01
      P( 5)=6.065142D-01
      P( 6)=4.018302D-01
      E( 7)=3.156877D+01
      E( 8)=2.834397D+01
      E( 9)=5.945127D+00
      S( 7)=-1.422028D+00
      S( 8)=1.234098D+00
      S( 9)=1.055683D+00
      P( 7)=-1.673366D+00
      P( 8)=2.018976D+00
      P( 9)=7.126889D-01
      E(10)=8.993335D+01
      E(11)=2.487496D+01
      E(12)=7.738191D+00
      D(10)=1.240159D-01
      D(11)=4.923831D-01
      D(12)=5.814968D-01
      E(13)=5.800256D+00
      E(14)=2.127256D+00
      E(15)=7.935512D-01
      S(13)=-4.196171D-01
      S(14)=4.843501D-01
      S(15)=7.952035D-01
      P(13)=1.430410D-02
      P(14)=5.071943D-01
      P(15)=5.539736D-01
      E(16)=3.796557D+00
      E(17)=1.256644D+00
      D(16)=3.314259D-01
      D(17)=7.651634D-01
      E(18)=3.881333D-01
      D(18)=1.0D+00
      E(19)=9.285445D-01
      E(20)=9.725467D-02
      S(19)=-2.523005D-01
      S(20)=1.087392D+00
      P(19)=-1.480711D-03
      P(20)=1.000388D+00
      E(21)=3.493292D-02
      S(21)=1.0D+00
      P(21)=1.0D+00
      RETURN
C
C     CADMIUM
C
  148 CONTINUE
      E( 1)=1.168609D+04
      E( 2)=1.770111D+03
      E( 3)=3.892090D+02
      S( 1)=6.142650D-02
      S( 2)=3.681567D-01
      S( 3)=6.895722D-01
      E( 4)=5.276004D+02
      E( 5)=1.148329D+02
      E( 6)=3.695829D+01
      S( 4)=-1.125925D-01
      S( 5)=8.326963D-02
      S( 6)=9.697978D-01
      P( 4)=1.518051D-01
      P( 5)=6.077598D-01
      P( 6)=3.999632D-01
      E( 7)=3.301548D+01
      E( 8)=2.954543D+01
      E( 9)=6.278508D+00
      S( 7)=-1.406471D+00
      S( 8)=1.218156D+00
      S( 9)=1.055520D+00
      P( 7)=-1.609024D+00
      P( 8)=1.959568D+00
      P( 9)=7.080271D-01
      E(10)=9.547274D+01
      E(11)=2.648196D+01
      E(12)=8.282886D+00
      D(10)=1.230828D-01
      D(11)=4.916768D-01
      D(12)=5.815408D-01
      E(13)=6.150596D+00
      E(14)=2.259746D+00
      E(15)=8.414261D-01
      S(13)=-4.229209D-01
      S(14)=4.987714D-01
      S(15)=7.850755D-01
      P(13)=1.448229D-02
      P(14)=5.186611D-01
      P(15)=5.426658D-01
      E(16)=4.082141D+00
      E(17)=1.357279D+00
      D(16)=3.379410D-01
      D(17)=7.591679D-01
      E(18)=4.208308D-01
      D(18)=1.0D+00
      E(19)=9.490686D-01
      E(20)=1.014878D-01
      S(19)=-2.215547D-01
      S(20)=1.080944D+00
      P(19)=-1.540266D-03
      P(20)=1.000412D+00
      E(21)=3.598726D-02
      S(21)=1.0D+00
      P(21)=1.0D+00
      RETURN
      END
