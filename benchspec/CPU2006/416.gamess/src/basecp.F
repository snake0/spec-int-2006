C 26 MAR 02 - KRG - USE ABRT CALL
C 20 FEB 01 - MWS - CORRECT A REFERENCE IN COMMENTS
C 19 NOV 00 - MWS - HWBAS,SBKBAS: RETURN NORMALIZED CXINP COEFS
C 31 MAY 95 - MWS - HWSHL: RETURN PURE S SINGLE ZETA ALKALI BASIS
C 21 APR 95 - MWS,TRC - INCLUDE HW SEMICORE BASIS SETS
C 10 NOV 94 - MWS - REMOVE FTNCHECK WARNING
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C 22 JUL 93 - MWS,TRC - SBKSHL,SBKLAN: CHANGES FOR SPLIT D,F LN SHELLS
C 16 JUL 93 - BMB - ADDED LANTHANIDE SBK BASIS SETS
C  1 NOV 92 - MWS - ADD HWSHL, GENERATE 3-21G FROM WITHIN HWBAS
C 12 MAR 92 - MWS - REDIMENSION TO 500 ATOMS
C 10 JAN 92 - TLW - SBKBAS: USE ABRT CALL
C  3 JAN 92 - TLW - MADE WRITES PARALLEL;ADD COMMON PAR
C 14 NOV 91 - MWS - SBKBAS: ZERO OUT D COEFS
C  8 JUN 91 - MWS - BUILD IN SBK BASES FOR K-RN
C  7 AUG 90 - TLW - ADD CF AND CG TO COMMON NSHEL
C 10 MAY 90 - MWS - CLEAN UP A BIT
C  4 OCT 89 - KAN - IMPLEMENT NEW MODULE FOR BUILT IN ECP BASIS SETS
C
C*MODULE BASECP  *DECK HWBAS
      SUBROUTINE HWBAS(NZETA,NUCZ,CSINP,CPINP,CDINP,IERR1,IERR2,
     *                 INTYP,NANGM,NBFS,MINF,MAXF,LOC,NGAUSS,NS,
     *                 EX,CS,CP,CD,KSTART,KATOM,KTYPE,KNG,KLOC,
     *                 KMIN,KMAX,NSHELL,MXGTOT,MXSH)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION CSINP(MXGTOT),CPINP(MXGTOT),CDINP(MXGTOT),
     *          INTYP(*),NANGM(*),NBFS(*),MINF(*),MAXF(*),NS(*),
     *          EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *          KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *          KLOC(MXSH),KMIN(MXSH),KMAX(MXSH)
      DIMENSION EEX(20),CCS(20),CCP(20),CCD(20)
C
      LOGICAL DONE,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (ZERO=0.0D+00, PT5=0.5D+00, PT75=0.75D+00, ONE=1.0D+00,
     *           PI32=5.56832799683170D+00, TM10=1.0D-10)
C
C     ----- SET UP HAY-WADT BASIS SETS -----
C     J.CHEM.PHYS. 82, 270-283, 284-298, 299-310 (1985)
C     NZETA = 1/2 FOR SINGLE OR DOUBLE ZETA CONTRACTIONS
C
C     HAY-WADT EXISTS ONLY FOR NA-BI,
C     SO LIGHT ELEMENTS H-NE SHOULD USE A ALL-ELECTRON BASIS SET.
C     ONLY THE SEMI-CORE TRANSITION METALS ARE HERE, ALL OTHER
C     ELEMENTS ARE FULL-CORE ECPS.  ZN,CD,HG ARE FULL CORE.
C
C     FOR H-NE, THE SZ BASIS COULD BE MINI OR STO-3G, BUT NOW BOMBS.
C     IT IS INTENDED AT PRESENT THAT SZ IS USED ONLY FOR HUCKEL GUESS
C     WHEN USING OTHER ECP BASES, SO BOMBING IS JUST FINE.
C     FOR H-NE, THE DZ BASIS USED IS 3-21G.
C
      IF(NZETA.EQ.1  .AND.  NUCZ.LE.10) THEN
         WRITE(IW,*) 'ERROR, HW MBS DOES NOT EXIST'
         CALL ABRT
         STOP
      END IF
C
      CALL VCLR(EEX,1,20)
      CALL VCLR(CCS,1,20)
      CALL VCLR(CCP,1,20)
      CALL VCLR(CCD,1,20)
C
C     ----- HYDROGEN TO HELIUM -----
C     3-21G SEEMS CONSISTENT WITH DZ SPLIT OF HAY-WADT BASIS
C
      IF(NUCZ.GT.2) GO TO 120
      CALL N21ONE(EEX,CCS,NUCZ)
      GO TO 300
C
C     ----- LITHIUM TO NEON -----
C     3-21G SEEMS CONSISTENT WITH DZ SPLIT OF HAY-WADT BASIS
C
  120 CONTINUE
      IF(NUCZ.GT.10) GO TO 130
      CALL N21TWO(EEX,CCS,CCP,NUCZ,3)
      GO TO 300
C
C     ----- SODIUM TO ARGON -----
C
  130 CONTINUE
      IF(NUCZ.GT.18) GO TO 140
      CALL HWTHR(EEX,CCS,CCP,NUCZ)
      GO TO 300
C
C     ----- POTASSIUM TO KRYPTON -----
C
  140 CONTINUE
      IF(NUCZ.GT.36) GO TO 150
      CALL HWFOR(EEX,CCS,CCP,CCD,NUCZ)
      GO TO 300
C
C     ----- RUBIDIUM TO XENON -----
C
  150 CONTINUE
      IF(NUCZ.GT.54) GO TO 160
      CALL HWFIV(EEX,CCS,CCP,CCD,NUCZ)
      GO TO 300
C
C     ----- CESIUM TO BISMUTH -----
C
  160 CONTINUE
      IF(NUCZ.GT.83) GO TO 170
      IF(NUCZ.GE.58  .AND.  NUCZ.LE.71) THEN
         IF(MASWRK) WRITE(IW,*) 'SO SORRY, -HW- HAS NO LANTHANIDES.'
         CALL ABRT
         STOP
      END IF
      CALL HWSIX(EEX,CCS,CCP,CCD,NUCZ)
      GO TO 300
C
C     ----- HIGHER THAN BISMUTH -----
C
  170 CONTINUE
      IF(MASWRK) WRITE(IW,*) '-HW- BASIS SETS DO NOT EXIST PAST BI'
      CALL ABRT
      STOP
C
C     ----- LOOP OVER EACH SHELL -----
C
  300 CONTINUE
      NG=0
      IPASS=0
      DONE = .FALSE.
      IERR3=0
C
  310 CONTINUE
      IPASS=IPASS+1
      CALL HWSHL(NUCZ,NZETA,IPASS,ITYP,IGAUSS,DONE)
      IF(DONE) THEN
         IF(IERR3.GT.0) THEN
            WRITE(IW,*) 'ERROR IN BUILT-IN HW BASIS SET KILLS JOB.'
            CALL ABRT
         END IF
         RETURN
      END IF
C
      NSHELL = NSHELL+1
      IF(NSHELL.GT.MXSH) THEN
         IERR1 = 1
         RETURN
      END IF
      NS(NAT)        = NS(NAT)+1
      KMIN(NSHELL)   = MINF(ITYP)
      KMAX(NSHELL)   = MAXF(ITYP)
      KSTART(NSHELL) = NGAUSS+1
      KATOM(NSHELL)  = NAT
      KTYPE(NSHELL)  = NANGM(ITYP)
      INTYP(NSHELL)  = ITYP
      KNG(NSHELL)    = IGAUSS
      KLOC(NSHELL)   = LOC+1
      NGAUSS = NGAUSS+IGAUSS
      IF(NGAUSS.GT.MXGTOT) THEN
         IERR2 = 1
         RETURN
      END IF
      LOC = LOC+NBFS(ITYP)
      K1 = KSTART(NSHELL)
      K2 = K1+KNG(NSHELL)-1
      DO 420 I = 1,IGAUSS
         K = K1+I-1
         EX(K)    = EEX(NG+I)
         CS(K)    = CCS(NG+I)
         CP(K)    = CCP(NG+I)
         CD(K)    = CCD(NG+I)
         CSINP(K) = CCS(NG+I)
         CPINP(K) = CCP(NG+I)
         CDINP(K) = CCD(NG+I)
 420  CONTINUE
C
C     ----- UNNORMALIZE GAUSSIAN PRIMITIVES -----
C
      DO 440 K = K1,K2
         EE = EX(K)+EX(K)
         FACS = PI32/(EE*SQRT(EE))
         FACP = PT5*FACS/EE
         FACD = PT75*FACS/(EE*EE)
         IF(ITYP.EQ.1) CS(K) = CS(K)/SQRT(FACS)
         IF(ITYP.EQ.2) CP(K) = CP(K)/SQRT(FACP)
         IF(ITYP.EQ.3) CD(K) = CD(K)/SQRT(FACD)
         IF(ITYP.EQ.6) CS(K) = CS(K)/SQRT(FACS)
         IF(ITYP.EQ.6) CP(K) = CP(K)/SQRT(FACP)
 440  CONTINUE
C
C     ----- NORMALIZE CONTRACTED BASIS FUNCTIONS -----
C
      IF(NORMF.EQ.1) GO TO 600
      FACS = ZERO
      FACP = ZERO
      FACD = ZERO
      DO 510 IG = K1,K2
         DO 500 JG = K1,IG
            EE   = EX(IG)+EX(JG)
            FAC  = EE*SQRT(EE)
            DUMS =      CS(IG)*CS(JG)/FAC
            DUMP =  PT5*CP(IG)*CP(JG)/(EE*FAC)
            DUMD = PT75*CD(IG)*CD(JG)/(EE*EE*FAC)
            IF (IG.EQ.JG) GO TO 480
               DUMS = DUMS+DUMS
               DUMP = DUMP+DUMP
               DUMD = DUMD+DUMD
  480       CONTINUE
            FACS = FACS+DUMS
            FACP = FACP+DUMP
            FACD = FACD+DUMD
  500    CONTINUE
  510 CONTINUE
C
      FAC=ZERO
      IF(ITYP.EQ.1 .AND. FACS.GT.TM10) FACS=ONE/SQRT(FACS*PI32)
      IF(ITYP.EQ.2 .AND. FACP.GT.TM10) FACP=ONE/SQRT(FACP*PI32)
      IF(ITYP.EQ.3 .AND. FACD.GT.TM10) FACD=ONE/SQRT(FACD*PI32)
      IF(ITYP.EQ.6 .AND. FACS.GT.TM10) FACS=ONE/SQRT(FACS*PI32)
      IF(ITYP.EQ.6 .AND. FACP.GT.TM10) FACP=ONE/SQRT(FACP*PI32)
C
C                                 VERIFY NORMALIZATION
C---      IF(NZETA.EQ.1) THEN
C---         IF(ITYP.EQ.1) FAC = FACS
C---         IF(ITYP.EQ.2) FAC = FACP
C---         IF(ITYP.EQ.3) FAC = FACD
C---         IF(ITYP.EQ.6) FAC = ONE + MAX(ABS(FACS-ONE),ABS(FACP-ONE))
C---         TNORM = ABS(ONE-FAC)
C---C
C---         IF(TNORM.GT.1.0D-06) THEN
C---            IF (MASWRK) THEN
C---               WRITE(IW,9000) FAC,NUCZ,IPASS,ITYP
C---               WRITE(IW,9010) (EEX(NG+K),K=1,IGAUSS)
C---               IF(ITYP.EQ.1) WRITE(IW,9010) (CCS(NG+K),K=1,IGAUSS)
C---               IF(ITYP.EQ.2) WRITE(IW,9010) (CCP(NG+K),K=1,IGAUSS)
C---               IF(ITYP.EQ.3) WRITE(IW,9010) (CCD(NG+K),K=1,IGAUSS)
C---               IF(ITYP.EQ.6) WRITE(IW,9010) (CCS(NG+K),K=1,IGAUSS)
C---               IF(ITYP.EQ.6) WRITE(IW,9010) (CCP(NG+K),K=1,IGAUSS)
C---            END IF
C---            IERR3=IERR3+1
C---         END IF
C---      END IF
C
      DO 550 IG = K1,K2
         IF(ITYP.EQ.1) CS(IG) = FACS*CS(IG)
         IF(ITYP.EQ.2) CP(IG) = FACP*CP(IG)
         IF(ITYP.EQ.3) CD(IG) = FACD*CD(IG)
         IF(ITYP.EQ.6) CS(IG) = FACS*CS(IG)
         IF(ITYP.EQ.6) CP(IG) = FACP*CP(IG)
         IF(ITYP.EQ.1) CSINP(IG) = FACS*CSINP(IG)
         IF(ITYP.EQ.2) CPINP(IG) = FACP*CPINP(IG)
         IF(ITYP.EQ.3) CDINP(IG) = FACD*CDINP(IG)
         IF(ITYP.EQ.6) CSINP(IG) = FACS*CSINP(IG)
         IF(ITYP.EQ.6) CPINP(IG) = FACP*CPINP(IG)
  550 CONTINUE
C
  600 CONTINUE
      NG = NG+IGAUSS
      GO TO 310
C
 9000 FORMAT(/1X,'ERROR!!! NORMALIZATION FACTOR=',E16.8/
     *     1X,'FOR ATOM Z=',I3,' SHELL NO.',I3,' ITYP=',I4/
     *     1X,'(ITYP OF 1,2,3,6 MEANS AN S,P,D,L SHELL)'/
     *     1X,'CHECK BUILT IN HW EXPONENTS AND CONT. COEFS')
 9010 FORMAT(5F15.8)
      END
C*MODULE BASECP  *DECK HWSHL
      SUBROUTINE HWSHL(NUCZ,NZETA,IPASS,ITYP,IGAUSS,DONE)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL DONE
      DIMENSION MAXPS1(10),ITYPE1(4,10),NGAUS1(4,10)
      DIMENSION MAXPS2(10),ITYPE2(7,10),NGAUS2(7,10)
C
      DATA MAXPS1/1,1,1,2,4,3,4,3,4,3/
      DATA ITYPE1/1,0,0,0,
     *            6,0,0,0,
     *            1,0,0,0,
     *            1,2,0,0,
     *            1,2,3,1,
     *            3,1,2,0,
     *            1,2,3,1,
     *            3,1,2,0,
     *            1,2,3,1,
     *            3,1,2,0/
      DATA NGAUS1/3,0,0,0,
     *            3,0,0,0,
     *            3,0,0,0,
     *            3,3,0,0,
     *            5,5,5,5,
     *            5,3,2,0,
     *            5,5,4,5,
     *            4,3,3,0,
     *            5,5,3,5,
     *            3,3,3,0/
C
      DATA MAXPS2/2,3,4,4,7,6,7,6,7,6/
      DATA ITYPE2/1,1,0,0,0,0,0,
     *            1,6,6,0,0,0,0,
     *            1,1,2,2,0,0,0,
     *            1,1,2,2,0,0,0,
     *            1,2,3,3,1,1,0,
     *            3,3,1,1,2,2,0,
     *            1,2,3,3,1,1,2,
     *            3,3,1,1,2,2,0,
     *            1,2,3,3,1,1,2,
     *            3,3,1,1,2,2,0/
      DATA NGAUS2/2,1,0,0,0,0,0,
     *            3,2,1,0,0,0,0,
     *            2,1,2,1,0,0,0,
     *            2,1,2,1,0,0,0,
     *            5,4,4,1,4,1,1,
     *            4,1,2,1,1,1,0,
     *            5,4,3,1,4,1,1,
     *            3,1,2,1,2,1,0,
     *            5,4,2,1,4,1,1,
     *            2,1,2,1,2,1,0/
C
C     ----- DEFINE CURRENT SHELL PARAMETERS FOR HW BASIS SETS -----
C        NUCZ  =NUCLEAR CHARGE OF THIS ATOM
C        IPASS =NUMBER OF CURRENT SHELL
C        MXPASS=TOTAL NUMBER OF SHELLS ON THIS ATOM
C        ITYP  =1,2,3,6 FOR S,P,D,L SHELLS
C        IGAUSS=NUMBER OF GAUSSIANS IN CURRENT SHELL
C        KIND =0  MEANS UNDEFINED (E.G. A LANTHANIDE),
C             =1  MEANS  H-HE (NON-HW)
C             =2  MEANS LI-NE (NON-HW)
C             =3  MEANS ALKALI/ALKALI EARTH
C             =4  MEANS AL-CL,GA-KR,IN-XE,TL-BI MAIN GROUP,
C             =5  MEANS 1ST TM ROW SEMI-CORE TRANSITION METAL
C             =6  MEANS 1ST TM ROW FULL-CORE (E.G. ZN)
C             =7  MEANS 2ND TM ROW SEMI-CORE TRANSITION METAL
C             =8  MEANS 2ND TM ROW FULL-CORE (E.G. CD)
C             =9  MEANS 3RD TM ROW SEMI-CORE TRANSITION METAL
C             =10 MEANS 3RD TM ROW FULL-CORE (E.G. HG)
C
      KIND=0
C             H,HE
      IF(NUCZ.GE. 1  .AND.  NUCZ.LE. 2) KIND=1
C             LI-NE
      IF(NUCZ.GE. 3  .AND.  NUCZ.LE.10) KIND=2
C             NA-AR
      IF(NUCZ.GE.11  .AND.  NUCZ.LE.12) KIND=3
      IF(NUCZ.GE.13  .AND.  NUCZ.LE.18) KIND=4
C             K-KR
      IF(NUCZ.GE.19  .AND.  NUCZ.LE.20) KIND=3
      IF(NUCZ.GE.21  .AND.  NUCZ.LE.29) KIND=5
      IF(NUCZ.EQ.30)                    KIND=6
      IF(NUCZ.GE.31  .AND.  NUCZ.LE.36) KIND=4
C             RB-XE
      IF(NUCZ.GE.37  .AND.  NUCZ.LE.38) KIND=3
      IF(NUCZ.GE.39  .AND.  NUCZ.LE.47) KIND=7
      IF(NUCZ.EQ.48)                    KIND=8
      IF(NUCZ.GE.49  .AND.  NUCZ.LE.54) KIND=4
C             CS-RN
      IF(NUCZ.GE.55  .AND.  NUCZ.LE.56) KIND=3
      IF(NUCZ.EQ.57)                    KIND=9
      IF(NUCZ.GE.58  .AND.  NUCZ.LE.71) KIND=0
      IF(NUCZ.GE.72  .AND.  NUCZ.LE.79) KIND=9
      IF(NUCZ.EQ.80)                    KIND=10
      IF(NUCZ.GE.81  .AND.  NUCZ.LE.83) KIND=4
C
      IF(KIND.EQ.0) THEN
         WRITE(6,*) 'HWSHL: PROBLEM WITH Z=',NUCZ
         CALL ABRT
      END IF
C
      IF(NZETA.EQ.1) THEN
         MXPASS=MAXPS1(KIND)
      ELSE
         MXPASS=MAXPS2(KIND)
      END IF
      IF(IPASS.GT.MXPASS) DONE=.TRUE.
      IF(DONE) RETURN
C
      IF(NZETA.EQ.1) THEN
         ITYP   = ITYPE1(IPASS,KIND)
         IGAUSS = NGAUS1(IPASS,KIND)
      ELSE
         ITYP   = ITYPE2(IPASS,KIND)
         IGAUSS = NGAUS2(IPASS,KIND)
      END IF
      RETURN
      END
C*MODULE BASECP  *DECK HWTHR
      SUBROUTINE HWTHR(E,CS,CP,N)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION E(6),CS(6),CP(6)
C
      NN = N-10
      GO TO (11,12,13,14,15,16,17,18), NN
C
C     SODIUM
C
   11 CONTINUE
      E(1) = 0.4972D+00
      E(2) = 0.0560D+00
      E(3) = 0.0221D+00
      E(4) = 0.6697D+00
      E(5) = 0.0636D+00
      E(6) = 0.0204D+00
      CS(1) = -0.1691179D+00
      CS(2) =  0.6749776D+00
      CS(3) =  0.4189118D+00
      CP(4) = -0.0293850D+00
      CP(5) =  0.4357419D+00
      CP(6) =  0.6551108D+00
      RETURN
C
C     MAGNESIUM
C
   12 CONTINUE
      E(1) = 0.7250D+00
      E(2) = 0.1112D+00
      E(3) = 0.0404D+00
      E(4) = 1.2400D+00
      E(5) = 0.1346D+00
      E(6) = 0.0422D+00
      CS(1) = -0.2064601D+00
      CS(2) =  0.5946231D+00
      CS(3) =  0.5308271D+00
      CP(4) = -0.0364350D+00
      CP(5) =  0.4946187D+00
      CP(6) =  0.6045677D+00
      RETURN
C
C     ALUMINUM
C
   13 CONTINUE
      E(1) = 0.9615D+00
      E(2) = 0.1819D+00
      E(3) = 0.0657D+00
      E(4) = 1.9280D+00
      E(5) = 0.2013D+00
      E(6) = 0.0580D+00
      CS(1) = -0.2484069D+00
      CS(2) =  0.6105639D+00
      CS(3) =  0.5443899D+00
      CP(4) = -0.0337570D+00
      CP(5) =  0.4814472D+00
      CP(6) =  0.6281982D+00
      RETURN
C
C     SILICON
C
   14 CONTINUE
      E(1) = 1.2220D+00
      E(2) = 0.2595D+00
      E(3) = 0.09311D+00
      E(4) = 2.5800D+00
      E(5) = 0.2984D+00
      E(6) = 0.08848D+00
      CS(1) = -0.274462D+00
      CS(2) =  0.616689D+00
      CS(3) =  0.558086D+00
      CP(4) = -0.039785D+00
      CP(5) =  0.521997D+00
      CP(6) =  0.587382D+00
      RETURN
C
C     PHOSPHORUS
C
   15 CONTINUE
      E(1) = 1.5160D+00
      E(2) = 0.3369D+00
      E(3) = 0.1211D+00
      E(4) = 3.7050D+00
      E(5) = 0.3934D+00
      E(6) = 0.1190D+00
      CS(1) = -0.2885448D+00
      CS(2) =  0.6396117D+00
      CS(3) =  0.5461777D+00
      CP(4) = -0.0363030D+00
      CP(5) =  0.5335154D+00
      CP(6) =  0.5720504D+00
      RETURN
C
C     SULFUR
C
   16 CONTINUE
      E(1) = 1.8500D+00
      E(2) = 0.4035D+00
      E(3) = 0.1438D+00
      E(4) = 4.9450D+00
      E(5) = 0.4870D+00
      E(6) = 0.1379D+00
      CS(1) = -0.2916700D+00
      CS(2) =  0.6992080D+00
      CS(3) =  0.4901470D+00
      CP(4) = -0.0344310D+00
      CP(5) =  0.5737040D+00
      CP(6) =  0.5410530D+00
      RETURN
C
C     CHLORINE
C
   17 CONTINUE
      E(1) = 2.2310D+00
      E(2) = 0.4720D+00
      E(3) = 0.1631D+00
      E(4) = 6.2960D+00
      E(5) = 0.6333D+00
      E(6) = 0.1819D+00
      CS(1) = -0.295892D+00
      CS(2) =  0.757313D+00
      CS(3) =  0.435100D+00
      CP(4) = -0.034865D+00
      CP(5) =  0.556255D+00
      CP(6) =  0.556588D+00
      RETURN
C
C     ARGON
C
   18 CONTINUE
      E(1) = 2.6130D+00
      E(2) = 0.5736D+00
      E(3) = 0.2014D+00
      E(4) = 7.8600D+00
      E(5) = 0.7387D+00
      E(6) = 0.2081D+00
      CS(1) = -0.2977400D+00
      CS(2) =  0.7399851D+00
      CS(3) =  0.4553460D+00
      CP(4) = -0.0319740D+00
      CP(5) =  0.5826147D+00
      CP(6) =  0.5321287D+00
      RETURN
      END
C*MODULE BASECP  *DECK HWFOR
      SUBROUTINE HWFOR(E,CS,CP,CD,NUCZ)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION E(*),CS(*),CP(*),CD(*)
C
C     ----- HAY,WADT BASIS FOR FOURTH ROW ELEMENTS -----
C
      IF(NUCZ.GE.21  .AND.  NUCZ.LE.30) THEN
         CALL HWTM1(E,CS,CP,CD,NUCZ)
         RETURN
      END IF
C
      NN=0
      IF(NUCZ.LE.20) NN = NUCZ-18
      IF(NUCZ.GE.31) NN = NUCZ-28
C
      GO TO (19,20,31,32,33,34,35,36), NN
C
C     POTASSIUM
C
   19 CONTINUE
      E(1) = 0.2099D+00
      E(2) = 0.0529D+00
      E(3) = 0.0209D+00
      E(4) = 0.2794D+00
      E(5) = 0.0376D+00
      E(6) = 0.0140D+00
      CS(1) = -0.2871128D+00
      CS(2) =  0.4206847D+00
      CS(3) =  0.7493584D+00
      CP(4) = -0.0508040D+00
      CP(5) =  0.5239690D+00
      CP(6) =  0.5575560D+00
      RETURN
C
C     CALCIUM
C
   20 CONTINUE
      E(1) = 0.2342D+00
      E(2) = 0.1447D+00
      E(3) = 0.0350D+00
      E(4) = 0.4119D+00
      E(5) = 0.0705D+00
      E(6) = 0.0263D+00
      CS(1) = -0.6975968D+00
      CS(2) =  0.6897188D+00
      CS(3) =  0.8833578D+00
      CP(4) = -0.0735430D+00
      CP(5) =  0.5797403D+00
      CP(6) =  0.5123463D+00
      RETURN
C
C     GALLIUM
C
   31 CONTINUE
      E(1) = 0.8306D+00
      E(2) = 0.3392D+00
      E(3) = 0.0918D+00
      E(4) = 1.6750D+00
      E(5) = 0.2030D+00
      E(6) = 0.0579D+00
      CS(1) = -0.4137939D+00
      CS(2) =  0.4907699D+00
      CS(3) =  0.8122499D+00
      CP(4) = -0.0408020D+00
      CP(5) =  0.4874108D+00
      CP(6) =  0.6264438D+00
      RETURN
C
C     GERMANIUM
C
   32 CONTINUE
      E(1) = 0.8935D+00
      E(2) = 0.4424D+00
      E(3) = 0.1162D+00
      E(4) = 1.8770D+00
      E(5) = 0.2623D+00
      E(6) = 0.0798D+00
      CS(1) = -0.5473100D+00
      CS(2) =  0.6161590D+00
      CS(3) =  0.8113429D+00
      CP(4) = -0.0518020D+00
      CP(5) =  0.5302898D+00
      CP(6) =  0.5800398D+00
      RETURN
C
C     ARSENIC
C
   33 CONTINUE
      E(1) = 0.9635D+00
      E(2) = 0.5427D+00
      E(3) = 0.1407D+00
      E(4) = 2.0840D+00
      E(5) = 0.3224D+00
      E(6) = 0.1020D+00
      CS(1) = -0.6857832D+00
      CS(2) =  0.7545512D+00
      CS(3) =  0.8069852D+00
      CP(4) = -0.0613810D+00
      CP(5) =  0.5603297D+00
      CP(6) =  0.5488037D+00
      RETURN
C
C     SELENIUM
C
   34 CONTINUE
      E(1) = 1.0330D+00
      E(2) = 0.6521D+00
      E(3) = 0.1660D+00
      E(4) = 2.3660D+00
      E(5) = 0.3833D+00
      E(6) = 0.1186D+00
      CS(1) = -0.9057420D+00
      CS(2) =  0.9815120D+00
      CS(3) =  0.7922750D+00
      CP(4) = -0.0655770D+00
      CP(5) =  0.5760670D+00
      CP(6) =  0.5382920D+00
      RETURN
C
C     BROMIME
C
   35 CONTINUE
      E(1) = 1.1590D+00
      E(2) = 0.7107D+00
      E(3) = 0.1905D+00
      E(4) = 2.6910D+00
      E(5) = 0.4446D+00
      E(6) = 0.1377D+00
      CS(1) = -0.8690699D+00
      CS(2) =  0.9641899D+00
      CS(3) =  0.7737520D+00
      CP(4) = -0.0673380D+00
      CP(5) =  0.5899843D+00
      CP(6) =  0.5251153D+00
      RETURN
C
C     KRYPTON
C
   36 CONTINUE
      E(1) = 1.2270D+00
      E(2) = 0.8457D+00
      E(3) = 0.2167D+00
      E(4) = 2.9200D+00
      E(5) = 0.5169D+00
      E(6) = 0.1614D+00
      CS(1) = -1.1859395D+00
      CS(2) =  1.2811545D+00
      CS(3) =  0.7678797D+00
      CP(4) = -0.0759660D+00
      CP(5) =  0.5995830D+00
      CP(6) =  0.5182310D+00
      RETURN
      END
C*MODULE BASECP  *DECK HWFIV
      SUBROUTINE HWFIV(E,CS,CP,CD,NUCZ)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION E(*),CS(*),CP(*),CD(*)
C
C     ----- HAY AND WADT FIFTH ROW BASIS SETS -----
C
      IF(NUCZ.GE.39  .AND.  NUCZ.LE.48) THEN
         CALL HWTM2(E,CS,CP,CD,NUCZ)
         RETURN
      END IF
C
      NN = 0
      IF(NUCZ.LE.38) NN = NUCZ-36
      IF(NUCZ.GE.49) NN = NUCZ-46
C
      GO TO (37,38,49,50,51,52,53,54), NN
C
C     RUBIDIUM
C
   37 CONTINUE
      E(1) = 0.17560D+00
      E(2) = 0.03660D+00
      E(3) = 0.01550D+00
      E(4) = 0.19470D+00
      E(5) = 0.03180D+00
      E(6) = 0.01240D+00
      CS(1) = -0.2912452D+00
      CS(2) =  0.6631344D+00
      CS(3) =  0.5088743D+00
      CP(4) = -0.0659050D+00
      CP(5) =  0.5501010D+00
      CP(6) =  0.5337630D+00
      RETURN
C
C     STRONTIUM
C
   38 CONTINUE
      E(1) = 0.1865D+00
      E(2) = 0.1099D+00
      E(3) = 0.0292D+00
      E(4) = 0.2735D+00
      E(5) = 0.0570D+00
      E(6) = 0.0222D+00
      CS(1) = -0.7123187D+00
      CS(2) =  0.7327107D+00
      CS(3) =  0.8470097D+00
      CP(4) = -0.0989490D+00
      CP(5) =  0.5959809D+00
      CP(6) =  0.5039349D+00
      RETURN
C
C     INDIUM
C
   49 CONTINUE
      E(1) = 0.4915D+00
      E(2) = 0.3404D+00
      E(3) = 0.0774D+00
      E(4) = 0.9755D+00
      E(5) = 0.1550D+00
      E(6) = 0.0474D+00
      CS(1) = -1.0815561D+00
      CS(2) =  1.1418861D+00
      CS(3) =  0.8134181D+00
      CP(4) = -0.0610500D+00
      CP(5) =  0.5185538D+00
      CP(6) =  0.5945877D+00
      RETURN
C
C     TIN
C
   50 CONTINUE
      E(1) = 0.5418D+00
      E(2) = 0.3784D+00
      E(3) = 0.0926D+00
      E(4) = 1.0470D+00
      E(5) = 0.1932D+00
      E(6) = 0.0630D+00
      CS(1) = -1.2116640D+00
      CS(2) =  1.3011570D+00
      CS(3) =  0.7758870D+00
      CP(4) = -0.0763140D+00
      CP(5) =  0.5681508D+00
      CP(6) =  0.5445228D+00
      RETURN
C
C     ANTIMONY
C
   51 CONTINUE
      E(1) = 0.5863D+00
      E(2) = 0.4293D+00
      E(3) = 0.1078D+00
      E(4) = 1.1110D+00
      E(5) = 0.2365D+00
      E(6) = 0.0800D+00
      CS(1) = -1.4596445D+00
      CS(2) =  1.5689216D+00
      CS(3) =  0.7529903D+00
      CP(4) = -0.0994670D+00
      CP(5) =  0.5924868D+00
      CP(6) =  0.5267898D+00
      RETURN
C
C     TELLURIUM
C
   52 CONTINUE
      E(1) = 0.6938D+00
      E(2) = 0.4038D+00
      E(3) = 0.1165D+00
      E(4) = 1.2310D+00
      E(5) = 0.2756D+00
      E(6) = 0.0911D+00
      CS(1) = -0.9544519D+00
      CS(2) =  1.1549188D+00
      CS(3) =  0.6537419D+00
      CP(4) = -0.1079069D+00
      CP(5) =  0.6102076D+00
      CP(6) =  0.5171696D+00
      RETURN
C
C     IODINE
C
   53 CONTINUE
      E(1) = 0.7242D+00
      E(2) = 0.4653D+00
      E(3) = 0.1336D+00
      E(4) = 1.2900D+00
      E(5) = 0.3180D+00
      E(6) = 0.1053D+00
      CS(1) = -1.1737608D+00
      CS(2) =  1.3749707D+00
      CS(3) =  0.6531029D+00
      CP(4) = -0.1189321D+00
      CP(5) =  0.6272564D+00
      CP(6) =  0.5082193D+00
      RETURN
C
C     XENON
C
   54 CONTINUE
      E(1) = 0.7646D+00
      E(2) = 0.5322D+00
      E(3) = 0.1491D+00
      E(4) = 1.2110D+00
      E(5) = 0.3808D+00
      E(6) = 0.1259D+00
      CS(1) = -1.5143658D+00
      CS(2) =  1.7270277D+00
      CS(3) =  0.6338089D+00
      CP(4) = -0.1405220D+00
      CP(5) =  0.6212978D+00
      CP(6) =  0.5366258D+00
      RETURN
      END
C*MODULE BASECP  *DECK HWSIX
      SUBROUTINE HWSIX(E,S,P,D,NUCZ)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION E(*),S(*),P(*),D(*)
C
C     ----- HAY AND WADT SIXTH ROW BASIS SETS -----
C
      IF(NUCZ.GE.57  .AND.  NUCZ.LE.80) THEN
         CALL HWTM3(E,S,P,D,NUCZ)
         RETURN
      END IF
C
      NN = 0
      IF(NUCZ.LE.56) NN = NUCZ-54
      IF(NUCZ.GE.81) NN = NUCZ-78
C
      GO TO (55,56,81,82,83,84,85,86), NN
C
C     CESIUM
C
   55 CONTINUE
      E(1)= 0.1206D+00
      E(2)= 0.03933D+00
      E(3)= 0.01513D+00
      S(1)=-0.403357D+00
      S(2)= 0.574369D+00
      S(3)= 0.693857D+00
      E(4)= 0.14580D+00
      E(5)= 0.02793D+00
      E(6)= 0.01126D+00
      P(4)=-0.086837D+00
      P(5)= 0.551116D+00
      P(6)= 0.537664D+00
      RETURN
C
C     BARIUM
C
   56 CONTINUE
      E(1)=0.1297D+00
      E(2)=0.08227D+00
      E(3)=0.02305D+00
      S(1)=-0.933093D+00
      S(2)= 1.000168D+00
      S(3)= 0.792833D+00
      E(4)=0.1804D+00
      E(5)=0.04763D+00
      E(6)=0.01925D+00
      P(4)=-0.140655D+00
      P(5)= 0.603981D+00
      P(6)= 0.517564D+00
      RETURN
C
C     THALLIUM
C
   81 CONTINUE
      E(1)=0.5169D+00
      E(2)=0.3025D+00
      E(3)=0.08117D+00
      S(1)=-0.896044D+00
      S(2)= 1.06738D+00
      S(3)= 0.694656D+00
      E(4)=0.7912D+00
      E(5)=0.1494D+00
      E(6)=0.0451D+00
      P(4)=-0.070986D+00
      P(5)= 0.532675D+00
      P(6)= 0.588788D+00
      RETURN
C
C     LEAD
C
   82 CONTINUE
      E(1)= 0.5135D+00
      E(2)= 0.3756D+00
      E(3)= 0.09443D+00
      S(1)=-1.646606D+00
      S(2)= 1.828689D+00
      S(3)= 0.675993D+00
      E(4)= 0.8748D+00
      E(5)= 0.1843D+00
      E(6)= 0.05982D+00
      P(4)=-0.09514D+00
      P(5)= 0.571781D+00
      P(6)= 0.551003D+00
      RETURN
C
C     BISMUTH
C
   83 CONTINUE
      E(1)=0.5744D+00
      E(2)=0.3851D+00
      E(3)=0.105D+00
      S(1)=-1.360422D+00
      S(2)= 1.586274D+00
      S(3)= 0.626609D+00
      E(4)=0.9105D+00
      E(5)=0.2194D+00
      E(6)=0.07455D+00
      P(4)=-0.118866D+00
      P(5)= 0.606464D+00
      P(6)= 0.524106D+00
      RETURN
C
C     POLONIUM, ASTATINE, RADON DO NOT EXIST
C
   84 CONTINUE
   85 CONTINUE
   86 CONTINUE
      CALL ABRT
      STOP
      END
C*MODULE BASECP  *DECK HWTM1
      SUBROUTINE HWTM1(E,S,P,D,NUCZ)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION E(20),S(20),P(20),D(20)
C
      NN = NUCZ - 20
      GO TO (21,22,23,24,25,26,27,28,29,30), NN
C
C     SCANDIUM
C
   21 CONTINUE
      E(1)= 3.717D+00
      E(2)= 1.097D+00
      E(3)= 4.164D-01
      E(4)= 7.61D-02
      E(5)= 2.84D-02
      S(1)=-3.91403D-01
      S(2)= 7.12502D-01
      S(3)= 5.4922D-01
      S(4)= 6.215D-03
      S(5)=-5.83D-04
      E(6)= 1.04D+01
      E(7)= 1.311D+00
      E(8)= 4.266D-01
      E(9)= 4.7D-02
      E(10)= 1.4D-02
      P(6)=-4.9073D-02
      P(7)= 6.04873D-01
      P(8)= 4.89877D-01
      P(9)= 2.1707D-02
      P(10)=-7.131D-03
      E(11)= 1.513D+01
      E(12)= 4.205D+00
      E(13)= 1.303D+00
      E(14)= 3.68D-01
      E(15)= 8.12D-02
      D(11)= 3.1312D-02
      D(12)= 1.43511D-01
      D(13)= 3.5235D-01
      D(14)= 5.15024D-01
      D(15)= 3.77768D-01
      E(16)=E(1)
      E(17)=E(2)
      E(18)=E(3)
      E(19)=E(4)
      E(20)=E(5)
      S(16)= 1.03996D-01
      S(17)=-2.15146D-01
      S(18)=-3.28354D-01
      S(19)= 5.32895D-01
      S(20)= 6.27441D-01
      RETURN
C
C     TITANIUM
C
   22 CONTINUE
      E(1)= 4.372D+00
      E(2)= 1.098D+00
      E(3)= 0.4178D+00
      E(4)= 0.0872D+00
      E(5)= 0.0314D+00
      S(1)=-0.363439D+00
      S(2)= 0.817844D+00
      S(3)= 0.418141D+00
      S(4)= 0.001067D+00
      S(5)= 0.000631D+00
      E(6)=12.52D+00
      E(7)= 1.491D+00
      E(8)= 0.4859D+00
      E(9)= 0.053D+00
      E(10)=0.016D+00
      P(6)=-0.045528D+00
      P(7)= 0.618127D+00
      P(8)= 0.47484D+00
      P(9)= 0.020802D+00
      P(10)=-0.007008D+00
      E(11)=20.21D+00
      E(12)= 5.495D+00
      E(13)= 1.699D+00
      E(14)= 0.484D+00
      E(15)= 0.1157D+00
      D(11)= 0.028726D+00
      D(12)= 0.143765D+00
      D(13)= 0.370411D+00
      D(14)= 0.51404D+00
      D(15)= 0.34266D+00
      E(16)=E(1)
      E(17)=E(2)
      E(18)=E(3)
      E(19)=E(4)
      E(20)=E(5)
      S(16)= 0.096235D+00
      S(17)=-0.261855D+00
      S(18)=-0.276801D+00
      S(19)= 0.537838D+00
      S(20)= 0.630882D+00
      RETURN
C
C     VANADIUM
C
   23 CONTINUE
      E(1)= 4.59D+00
      E(2)= 1.493D+00
      E(3)= 5.57D-01
      E(4)= 9.75D-02
      E(5)= 3.42D-02
      S(1)=-4.25305D-01
      S(2)= 7.02961D-01
      S(3)= 5.86238D-01
      S(4)= 1.1835D-02
      S(5)=-2.089D-03
      E(6)= 1.376D+01
      E(7)= 1.712D+00
      E(8)= 5.587D-01
      E(9)= 5.9D-02
      E(10)= 1.8D-02
      P(6)=-4.8055D-02
      P(7)= 6.11873D-01
      P(8)= 4.82067D-01
      P(9)= 2.1972D-02
      P(10)=-7.678D-03
      E(11)= 2.57D+01
      E(12)= 6.53D+00
      E(13)= 2.078D+00
      E(14)= 6.243D-01
      E(15)= 1.542D-01
      D(11)= 2.7837D-02
      D(12)= 1.51007D-01
      D(13)= 3.67736D-01
      D(14)= 5.03274D-01
      D(15)= 3.38129D-01
      E(16)=E(1)
      E(17)=E(2)
      E(18)=E(3)
      E(19)=E(4)
      E(20)=E(5)
      S(16)= 1.12237D-01
      S(17)=-2.10814D-01
      S(18)=-3.19935D-01
      S(19)= 4.93923D-01
      S(20)= 6.58213D-01
      RETURN
C
C     CHROMIUM
C
   24 CONTINUE
      E(1)= 5.361D+00
      E(2)= 1.449D+00
      E(3)= 5.496D-01
      E(4)= 1.052D-01
      E(5)= 3.64D-02
      S(1)=-3.79604D-01
      S(2)= 7.77586D-01
      S(3)= 4.71878D-01
      S(4)= 4.46D-03
      S(5)= 7.64D-04
      E(6)= 1.642D+01
      E(7)= 1.914D+00
      E(8)= 6.241D-01
      E(9)= 6.3D-02
      E(10)= 1.9D-02
      P(6)=-4.5978D-02
      P(7)= 6.08849D-01
      P(8)= 4.84255D-01
      P(9)= 2.2086D-02
      P(10)=-7.872D-03
      E(11)= 2.895D+01
      E(12)= 7.708D+00
      E(13)= 2.495D+00
      E(14)= 7.655D-01
      E(15)= 1.889D-01
      D(11)= 2.8629D-02
      D(12)= 1.5089D-01
      D(13)= 3.70373D-01
      D(14)= 5.03588D-01
      D(15)= 3.28014D-01
      E(16)=E(1)
      E(17)=E(2)
      E(18)=E(3)
      E(19)=E(4)
      E(20)=E(5)
      S(16)= 9.8966D-02
      S(17)=-2.43623D-01
      S(18)=-2.71669D-01
      S(19)= 5.00268D-01
      S(20)= 6.54647D-01
      RETURN
C
C     MANGANESE
C
   25 CONTINUE
      E(1)= 5.914D+00
      E(2)= 1.605D+00
      E(3)= 6.26D-01
      E(4)= 1.115D-01
      E(5)= 3.8D-02
      S(1)=-3.74541D-01
      S(2)= 7.6856D-01
      S(3)= 4.74515D-01
      S(4)= 1.0786D-02
      S(5)=-1.55D-03
      E(6)= 1.82D+01
      E(7)= 2.141D+00
      E(8)= 7.009D-01
      E(9)= 6.9D-02
      E(10)= 2.1D-02
      P(6)=-4.465D-02
      P(7)= 6.24073D-01
      P(8)= 4.68164D-01
      P(9)= 2.061D-02
      P(10)=-7.493D-03
      E(11)= 3.227D+01
      E(12)= 8.875D+00
      E(13)= 2.89D+00
      E(14)= 8.761D-01
      E(15)= 2.12D-01
      D(11)= 2.8955D-02
      D(12)= 1.49285D-01
      D(13)= 3.72495D-01
      D(14)= 5.03798D-01
      D(15)= 3.31244D-01
      E(16)=E(1)
      E(17)=E(2)
      E(18)=E(3)
      E(19)=E(4)
      E(20)=E(5)
      S(16)= 9.4344D-02
      S(17)=-2.3139D-01
      S(18)=-2.60682D-01
      S(19)= 4.89705D-01
      S(20)= 6.55995D-01
      RETURN
C
C     IRON
C     NOTE NORMALIZATION OF 3S=1.00046, 4S=0.99995
C
   26 CONTINUE
      E(1)= 6.422D+00
      E(2)= 1.826D+00
      E(3)= 7.135D-01
      E(4)= 1.021D-01
      E(5)= 3.63D-02
      S(1)=-3.90482D-01
      S(2)= 7.66736D-01
      S(3)= 4.89134D-01
      S(4)= 1.3944D-02
      S(5)=-3.851D-03
      E(6)= 1.948D+01
      E(7)= 2.389D+00
      E(8)= 7.795D-01
      E(9)= 7.4D-02
      E(10)= 2.2D-02
      P(6)=-4.6889D-02
      P(7)= 6.23034D-01
      P(8)= 4.70856D-01
      P(9)= 2.0132D-02
      P(10)=-7.269D-03
      E(11)= 3.708D+01
      E(12)= 1.01D+01
      E(13)= 3.22D+00
      E(14)= 9.628D-01
      E(15)= 2.262D-01
      D(11)= 2.8292D-02
      D(12)= 1.53707D-01
      D(13)= 3.85911D-01
      D(14)= 5.05331D-01
      D(15)= 3.17387D-01
      E(16)=E(1)
      E(17)=E(2)
      E(18)=E(3)
      E(19)=E(4)
      E(20)=E(5)
      S(16)= 9.5048D-02
      S(17)=-2.2309D-01
      S(18)=-2.42992D-01
      S(19)= 5.86979D-01
      S(20)= 5.42877D-01
      RETURN
C
C     COBALT
C
   27 CONTINUE
      E(1)= 7.176D+00
      E(2)= 2.009D+00
      E(3)= 8.055D-01
      E(4)= 1.07D-01
      E(5)= 3.75D-02
      S(1)=-3.82974D-01
      S(2)= 7.40095D-01
      S(3)= 5.05618D-01
      S(4)= 1.914D-02
      S(5)=-6.337D-03
      E(6)= 2.139D+01
      E(7)= 2.65D+00
      E(8)= 8.619D-01
      E(9)= 8.0D-02
      E(10)= 2.3D-02
      P(6)=-4.7901D-02
      P(7)= 6.20417D-01
      P(8)= 4.74415D-01
      P(9)= 2.0138D-02
      P(10)=-7.18D-03
      E(11)= 3.925D+01
      E(12)= 1.078D+01
      E(13)= 3.496D+00
      E(14)= 1.066D+00
      E(15)= 2.606D-01
      D(11)= 3.1169D-02
      D(12)= 1.63521D-01
      D(13)= 3.90105D-01
      D(14)= 4.92304D-01
      D(15)= 3.09935D-01
      E(16)=E(1)
      E(17)=E(2)
      E(18)=E(3)
      E(19)=E(4)
      E(20)=E(5)
      S(16)= 8.8983D-02
      S(17)=-2.03496D-01
      S(18)=-2.37332D-01
      S(19)= 5.58637D-01
      S(20)= 5.63581D-01
      RETURN
C
C     NICKEL
C
   28 CONTINUE
      E(1)=7.62D+00
      E(2)=2.294D+00
      E(3)=0.876D+00
      E(4)=0.1153D+00
      E(5)=0.0396D+00
      S(1)=-0.406097D+00
      S(2)= 0.74159D+00
      S(3)= 0.529757D+00
      S(4)= 0.014167D+00
      S(5)=-0.004167D+00
      E(6)=23.66D+00
      E(7)= 2.893D+00
      E(8)= 0.9435D+00
      E(9)= 0.086D+00
      E(10)=0.024D+00
      P(6)=-0.048019D+00
      P(7)= 0.624069D+00
      P(8)= 0.470176D+00
      P(9)= 0.019945D+00
      P(10)=-0.007026D+00
      E(11)=42.72D+00
      E(12)=11.76D+00
      E(13)= 3.817D+00
      E(14)= 1.169D+00
      E(15)= 0.2836D+00
      D(11)=0.03236D+00
      D(12)=0.169841D+00
      D(13)=0.396038D+00
      D(14)=0.488101D+00
      D(15)=0.302231D+00
      E(16)=E(1)
      E(17)=E(2)
      E(18)=E(3)
      E(19)=E(4)
      E(20)=E(5)
      S(16)= 0.090412D+00
      S(17)=-0.191532D+00
      S(18)=-0.239188D+00
      S(19)= 0.523584D+00
      S(20)= 0.596627D+00
      RETURN
C
C     COPPER
C
   29 CONTINUE
      E(1)= 8.176D+00
      E(2)= 2.568D+00
      E(3)= 9.587D-01
      E(4)= 1.153D-01
      E(5)= 3.96D-02
      S(1)=-4.19145D-01
      S(2)= 7.35292D-01
      S(3)= 5.501D-01
      S(4)= 1.2726D-02
      S(5)=-4.027D-03
      E(6)= 2.563D+01
      E(7)= 3.166D+00
      E(8)= 1.023D+00
      E(9)= 8.6D-02
      E(10)= 2.4D-02
      P(6)=-4.8802D-02
      P(7)= 6.25804D-01
      P(8)= 4.70505D-01
      P(9)= 1.8172D-02
      P(10)=-6.569D-03
      E(11)= 4.134D+01
      E(12)= 1.142D+01
      E(13)= 3.839D+00
      E(14)= 1.23D+00
      E(15)= 3.102D-01
      D(11)= 4.0754D-02
      D(12)= 1.95077D-01
      D(13)= 3.97458D-01
      D(14)= 4.65382D-01
      D(15)= 2.88097D-01
      E(16)=E(1)
      E(17)=E(2)
      E(18)=E(3)
      E(19)=E(4)
      E(20)=E(5)
      S(16)= 8.9201D-02
      S(17)=-1.79247D-01
      S(18)=-2.34761D-01
      S(19)= 5.39268D-01
      S(20)= 5.75271D-01
      RETURN
C
C     ZINC: SEMI-CORE DOES NOT EXIST
C
   30 CONTINUE
      E(1)=68.85D+00
      E(2)=18.32D+00
      E(3)=5.922D+00
      E(4)=1.927D+00
      E(5)=0.5528D+00
      D(1)=0.0214335D+00
      D(2)=0.1368916D+00
      D(3)=0.3704352D+00
      D(4)=0.4834232D+00
      D(5)=0.331515D+00
      E(6)=0.7997D+00
      E(7)=0.17520D+00
      E(8)=0.05560D+00
      S(6)=-0.2517637D+00
      S(7)=0.5099734D+00
      S(8)=0.6581327D+00
      E(9)=0.1202D+00
      E(10)=0.0351D+00
      P(9)=0.6130140D+00
      P(10)=0.4898007D+00
      RETURN
      END
C*MODULE BASECP  *DECK HWTM2
      SUBROUTINE HWTM2(E,S,P,D,NUCZ)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION E(19),S(19),P(19),D(19)
C
      NN = NUCZ - 38
      GO TO (39,40,41,42,43,44,45,46,47,48), NN
C
C     YTTRIUM
C     NOTE NORMALIZATION OF 4D=1.00014
C
   39 CONTINUE
      E(1)= 1.751D+00
      E(2)= 1.143D+00
      E(3)= 3.581D-01
      E(4)= 1.058D-01
      E(5)= 3.18D-02
      S(1)=-1.154182D+00
      S(2)= 1.287401D+00
      S(3)= 7.06853D-01
      S(4)= 8.583D-03
      S(5)= 9.64D-04
      E(6)= 3.884D+00
      E(7)= 7.66D-01
      E(8)= 2.89D-01
      E(9)= 6.29D-02
      E(10)= 2.23D-02
      P(6)=-8.1283D-02
      P(7)= 6.69244D-01
      P(8)= 4.15576D-01
      P(9)= 2.8387D-02
      P(10)=-6.126D-03
      E(11)= 1.523D+00
      E(12)= 5.634D-01
      E(13)= 1.834D-01
      E(14)= 5.69D-02
      D(11)= 8.88D-02
      D(12)= 3.77058D-01
      D(13)= 4.98991D-01
      D(14)= 3.17519D-01
      E(15)=E(1)
      E(16)=E(2)
      E(17)=E(3)
      E(18)=E(4)
      E(19)=E(5)
      S(15)= 3.93283D-01
      S(16)=-4.60836D-01
      S(17)=-4.83893D-01
      S(18)= 3.76953D-01
      S(19)= 8.5187D-01
      RETURN
C
C     ZIRCONIUM
C
   40 CONTINUE
      E(1)= 1.976D+00
      E(2)= 1.154D+00
      E(3)= 3.91D-01
      E(4)= 1.001D-01
      E(5)= 3.34D-02
      S(1)=-9.14787D-01
      S(2)= 1.085915D+00
      S(3)= 6.75217D-01
      S(4)= 1.0652D-02
      S(5)=-2.117D-03
      E(6)= 4.192D+00
      E(7)= 8.764D-01
      E(8)= 3.263D-01
      E(9)= 7.24D-02
      E(10)= 2.43D-02
      P(6)=-9.3509D-02
      P(7)= 6.72996D-01
      P(8)= 4.21299D-01
      P(9)= 2.2838D-02
      P(10)=-3.67D-03
      E(11)= 2.269D+00
      E(12)= 7.855D-01
      E(13)= 2.615D-01
      E(14)= 8.02D-02
      D(11)= 4.9755D-02
      D(12)= 3.92753D-01
      D(13)= 5.0749D-01
      D(14)= 3.11661D-01
      E(15)=E(1)
      E(16)=E(2)
      E(17)=E(3)
      E(18)=E(4)
      E(19)=E(5)
      S(15)= 3.31084D-01
      S(16)=-4.32744D-01
      S(17)=-4.51019D-01
      S(18)= 4.79024D-01
      S(19)= 7.44664D-01
      RETURN
C
C     NIOBIUM
C
   41 CONTINUE
      E(1)= 2.182D+00
      E(2)= 1.209D+00
      E(3)= 4.165D-01
      E(4)= 1.454D-01
      E(5)= 3.92D-02
      S(1)=-8.82042D-01
      S(2)= 1.100169D+00
      S(3)= 6.28046D-01
      S(4)= 4.057D-03
      S(5)=-4.35D-04
      E(6)= 4.519D+00
      E(7)= 9.406D-01
      E(8)= 3.492D-01
      E(9)= 7.52D-02
      E(10)= 2.47D-02
      P(6)=-8.1198D-02
      P(7)= 6.94956D-01
      P(8)= 3.95507D-01
      P(9)= 1.9904D-02
      P(10)=-3.453D-03
      E(11)= 3.466D+00
      E(12)= 9.938D-01
      E(13)= 3.35D-01
      E(14)= 1.024D-01
      D(11)= 2.6343D-02
      D(12)= 4.03029D-01
      D(13)= 5.13958D-01
      D(14)= 3.04523D-01
      E(15)=E(1)
      E(16)=E(2)
      E(17)=E(3)
      E(18)=E(4)
      E(19)=E(5)
      S(15)= 3.17444D-01
      S(16)=-4.38162D-01
      S(17)=-4.68855D-01
      S(18)= 4.0623D-01
      S(19)= 8.58701D-01
      RETURN
C
C     MOLYBDENUM
C
   42 CONTINUE
      E(1)= 2.361D+00
      E(2)= 1.309D+00
      E(3)= 4.5D-01
      E(4)= 1.681D-01
      E(5)= 4.23D-02
      S(1)=-9.10746D-01
      S(2)= 1.145946D+00
      S(3)= 6.08755D-01
      S(4)= 2.416D-03
      S(5)=-8.11D-04
      E(6)= 4.895D+00
      E(7)= 1.044D+00
      E(8)= 3.877D-01
      E(9)= 7.8D-02
      E(10)= 2.47D-02
      P(6)=-9.0293D-02
      P(7)= 7.00158D-01
      P(8)= 3.94987D-01
      P(9)= 1.915D-02
      P(10)=-3.774D-03
      E(11)= 2.993D+00
      E(12)= 1.063D+00
      E(13)= 3.721D-01
      E(14)= 1.178D-01
      D(11)= 4.5268D-02
      D(12)= 4.29772D-01
      D(13)= 4.97633D-01
      D(14)= 2.65849D-01
      E(15)=E(1)
      E(16)=E(2)
      E(17)=E(3)
      E(18)=E(4)
      E(19)=E(5)
      S(15)= 3.26369D-01
      S(16)=-4.55518D-01
      S(17)=-4.65603D-01
      S(18)= 4.03579D-01
      S(19)= 8.72535D-01
      RETURN
C
C     TECHNETIUM
C
   43 CONTINUE
      E(1)= 2.342D+00
      E(2)= 1.634D+00
      E(3)= 5.094D-01
      E(4)= 1.706D-01
      E(5)= 4.35D-02
      S(1)=-1.486551D+00
      S(2)= 1.669707D+00
      S(3)= 6.55261D-01
      S(4)= 3.732D-03
      S(5)= 8.64D-04
      E(6)= 5.278D+00
      E(7)= 1.156D+00
      E(8)= 4.302D-01
      E(9)= 8.95D-02
      E(10)= 2.46D-02
      P(6)=-9.8966D-02
      P(7)= 7.04057D-01
      P(8)= 3.95058D-01
      P(9)= 1.7923D-02
      P(10)=-2.773D-03
      E(11)= 4.632D+00
      E(12)= 1.279D+00
      E(13)= 4.425D-01
      E(14)= 1.364D-01
      D(11)= 2.2906D-02
      D(12)= 4.3243D-01
      D(13)= 5.03886D-01
      D(14)= 2.78048D-01
      E(15)=E(1)
      E(16)=E(2)
      E(17)=E(3)
      E(18)=E(4)
      E(19)=E(5)
      S(15)= 5.32058D-01
      S(16)=-6.37978D-01
      S(17)=-4.51005D-01
      S(18)= 3.87878D-01
      S(19)= 8.60266D-01
      RETURN
C
C     RUTHENIUM
C     NOTE NORMALIZATION OF 4S=0.99841, 5S=1.00007
C     THIS IS THE WORST MISPRINT IN THE PAPER.
C
   44 CONTINUE
      E(1)= 2.565D+00
      E(2)= 1.503D+00
      E(3)= 5.129D-01
      E(4)= 1.362D-01
      E(5)= 4.17D-02
      S(1)=-1.040371D+00
      S(2)= 1.327988D+00
      S(3)= 5.59835D-01
      S(4)= 3.955D-03
      S(5)= 7.4D-05
      E(6)= 4.859D+00
      E(7)= 1.219D+00
      E(8)= 4.413D-01
      E(9)= 8.3D-02
      E(10)= 2.5D-02
      P(6)=-9.4258D-02
      P(7)= 7.40984D-01
      P(8)= 3.65583D-01
      P(9)= 1.2057D-02
      P(10)=-2.093D-03
      E(11)= 4.195D+00
      E(12)= 1.377D+00
      E(13)= 4.828D-01
      E(14)= 1.501D-01
      D(11)= 4.2106D-02
      D(12)= 4.42552D-01
      D(13)= 4.96714D-01
      D(14)= 2.56877D-01
      E(15)=E(1)
      E(16)=E(2)
      E(17)=E(3)
      E(18)=E(4)
      E(19)=E(5)
      S(15)= 3.62973D-01
      S(16)=-5.22916D-01
      S(17)=-3.47033D-01
      S(18)= 4.4027D-01
      S(19)= 7.64466D-01
      RETURN
C
C     RHODIUM
C     NOTE NORMALIZATION OF 4S=0.99997
C
   45 CONTINUE
      E(1)= 2.646D+00
      E(2)= 1.751D+00
      E(3)= 5.713D-01
      E(4)= 1.438D-01
      E(5)= 4.28D-02
      S(1)=-1.348775D+00
      S(2)= 1.603338D+00
      S(3)= 5.86497D-01
      S(4)= 7.607D-03
      S(5)=-1.0D-05
      E(6)= 5.44D+00
      E(7)= 1.329D+00
      E(8)= 4.845D-01
      E(9)= 8.69D-02
      E(10)= 2.57D-02
      P(6)=-9.8443D-02
      P(7)= 7.40899D-01
      P(8)= 3.65632D-01
      P(9)= 1.2455D-02
      P(10)=-2.456D-03
      E(11)= 3.669D+00
      E(12)= 1.423D+00
      E(13)= 5.091D-01
      E(14)= 1.61D-01
      D(11)= 6.7048D-02
      D(12)= 4.55084D-01
      D(13)= 4.79584D-01
      D(14)= 2.33826D-01
      E(15)=E(1)
      E(16)=E(2)
      E(17)=E(3)
      E(18)=E(4)
      E(19)=E(5)
      S(15)= 4.56934D-01
      S(16)=-5.95199D-01
      S(17)=-3.42127D-01
      S(18)= 4.10138D-01
      S(19)= 7.80486D-01
      RETURN
C
C     PALLADIUM
C
   46 CONTINUE
      E(1)=2.787D+00
      E(2)=1.965D+00
      E(3)=0.6243D+00
      E(4)=0.1496D+00
      E(5)=0.0436D+00
      S(1)=-1.601728D+00
      S(2)= 1.839211D+00
      S(3)= 0.600558D+00
      S(4)= 0.008203D+00
      S(5)= 0.000234D+00
      E(6)=5.999D+00
      E(7)=1.443D+00
      E(8)=0.5264D+00
      E(9)=0.0899D+00
      E(10)=0.0262D+00
      P(6)=-0.103179D+00
      P(7)= 0.743447D+00
      P(8)= 0.364547D+00
      P(9)= 0.011932D+00
      P(10)=-0.002519D+00
      E(11)=6.091D+00
      E(12)=1.719D+00
      E(13)=0.6056D+00
      E(14)=0.1883D+00
      D(11)=0.03276D+00
      D(12)=0.45293D+00
      D(13)=0.496964D+00
      D(14)=0.250281D+00
      E(15)=E(1)
      E(16)=E(2)
      E(17)=E(3)
      E(18)=E(4)
      E(19)=E(5)
      S(15)= 0.527184D+00
      S(16)=-0.65333D+00
      S(17)=-0.333243D+00
      S(18)= 0.397129D+00
      S(19)= 0.783642D+00
      RETURN
C
C     SILVER
C     SIGN MISPRINT IN 4P CORRECTED HERE
C     NOTE NORMALIZATION OF 5S=1.00001
C
   47 CONTINUE
      E(1)= 2.95D+00
      E(2)= 2.149D+00
      E(3)= 6.684D-01
      E(4)= 9.97D-02
      E(5)= 3.47D-02
      S(1)=-1.784583D+00
      S(2)= 2.01714D+00
      S(3)= 6.05089D-01
      S(4)= 8.114D-03
      S(5)=-1.506D-03
      E(6)= 6.553D+00
      E(7)= 1.565D+00
      E(8)= 5.748D-01
      E(9)= 8.33D-02
      E(10)= 2.52D-02
      P(6)=-1.07627D-01
      P(7)= 7.38411D-01
      P(8)= 3.71119D-01
      P(9)= 1.2393D-02
      P(10)=-3.468D-03
      E(11)= 3.391D+00
      E(12)= 1.599D+00
      E(13)= 6.282D-01
      E(14)= 2.108D-01
      D(11)= 1.22831D-01
      D(12)= 4.17171D-01
      D(13)= 4.53388D-01
      D(14)= 2.32117D-01
      E(15)=E(1)
      E(16)=E(2)
      E(17)=E(3)
      E(18)=E(4)
      E(19)=E(5)
      S(15)= 5.74282D-01
      S(16)=-7.02991D-01
      S(17)=-2.77563D-01
      S(18)= 6.30189D-01
      S(19)= 5.12782D-01
      RETURN
C
C     CADMIUM
C
   48 CONTINUE
      E(1)=5.148D+00
      E(2)=1.966D+00
      E(3)=0.736D+00
      E(4)=0.2479D+00
      D(1)=0.0629604D+00
      D(2)=0.4601487D+00
      D(3)=0.4850734D+00
      D(4)=0.2015723D+00
      E(5)=0.5095D+00
      E(6)=0.1924D+00
      E(7)=0.0544D+00
      S(5)=-0.4140627D+00
      S(6)=0.5863291D+00
      S(7)=0.7244515D+00
      E(8)=0.827D+00
      E(9)=0.1287D+00
      E(10)=0.0405D+00
      P(8)=-0.0544015D+00
      P(9)=0.5207503D+00
      P(10)=0.5865668D+00
      RETURN
      END
C*MODULE BASECP  *DECK HWTM3
      SUBROUTINE HWTM3(E,S,P,D,NUCZ)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION E(18),S(18),P(18),D(18)
C
      IF(NUCZ.GE.58  .AND.  NUCZ.LE.71) THEN
         CALL ABRT
         STOP
      END IF
C
      IF(NUCZ.EQ.57) GO TO 57
      NN = NUCZ - 70
      GO TO (57,72,73,74,75,76,77,78,79,80), NN
C
C     LANTHANUM
C
   57 CONTINUE
      E(1)= 9.167D-01
      E(2)= 7.427D-01
      E(3)= 2.237D-01
      E(4)= 7.92D-02
      E(5)= 2.39D-02
      S(1)=-3.026804D+00
      S(2)= 3.300122D+00
      S(3)= 5.51852D-01
      S(4)=-8.15D-04
      S(5)=-7.53D-04
      E(6)= 1.554D+00
      E(7)= 5.622D-01
      E(8)= 2.239D-01
      E(9)= 4.83D-02
      E(10)= 1.79D-02
      P(6)=-1.79346D-01
      P(7)= 6.54571D-01
      P(8)= 4.96417D-01
      P(9)= 3.5915D-02
      P(10)=-8.431D-03
      E(11)= 4.524D-01
      E(12)= 1.602D-01
      E(13)= 5.31D-02
      D(11)= 3.65757D-01
      D(12)= 5.35653D-01
      D(13)= 3.05032D-01
      E(14)=E(1)
      E(15)=E(2)
      E(16)=E(3)
      E(17)=E(4)
      E(18)=E(5)
      S(14)= 1.212767D+00
      S(15)=-1.373391D+00
      S(16)=-4.971D-01
      S(17)= 5.38185D-01
      S(18)= 7.76973D-01
      RETURN
C
C     HAFNIUM
C
   72 CONTINUE
      E(1)= 1.95D+00
      E(2)= 1.183D+00
      E(3)= 3.897D-01
      E(4)= 1.656D-01
      E(5)= 4.24D-02
      S(1)=-1.233434D+00
      S(2)= 1.578915D+00
      S(3)= 4.96298D-01
      S(4)=-7.415D-03
      S(5)=-5.111D-04
      E(6)= 1.972D+00
      E(7)= 1.354D+00
      E(8)= 4.134D-01
      E(9)= 8.04D-02
      E(10)= 2.74D-02
      P(6)=-6.25984D-01
      P(7)= 1.035986D+00
      P(8)= 5.74329D-01
      P(9)= 2.8574D-02
      P(10)=-4.794D-03
      E(11)= 8.226D-01
      E(12)= 2.585D-01
      E(13)= 7.62D-02
      D(11)= 3.58383D-01
      D(12)= 5.3664D-01
      D(13)= 3.538D-01
      E(14)=E(1)
      E(15)=E(2)
      E(16)=E(3)
      E(17)=E(4)
      E(18)=E(5)
      S(14)= 4.54581D-01
      S(15)=-6.33087D-01
      S(16)=-5.05352D-01
      S(17)= 5.18347D-01
      S(18)= 8.36148D-01
      RETURN
C
C     TANTALUM
C
   73 CONTINUE
      E(1)= 2.044D+00
      E(2)= 1.267D+00
      E(3)= 4.157D-01
      E(4)= 1.671D-01
      E(5)= 4.82D-02
      S(1)=-1.319247D+00
      S(2)= 1.669087D+00
      S(3)= 4.88078D-01
      S(4)=-7.937D-03
      S(5)=-1.597D-03
      E(6)= 2.565D+00
      E(7)= 1.229D+00
      E(8)= 4.244D-01
      E(9)= 8.4D-02
      E(10)= 2.8D-02
      P(6)=-2.89312D-01
      P(7)= 7.50806D-01
      P(8)= 5.19676D-01
      P(9)= 2.3986D-02
      P(10)=-3.846D-03
      E(11)= 8.948D-01
      E(12)= 2.989D-01
      E(13)= 9.35D-02
      D(11)= 3.85913D-01
      D(12)= 5.21982D-01
      D(13)= 3.2264D-01
      E(14)=E(1)
      E(15)=E(2)
      E(16)=E(3)
      E(17)=E(4)
      E(18)=E(5)
      S(14)= 5.22607D-01
      S(15)=-7.31396D-01
      S(16)=-4.6401D-01
      S(17)= 5.20118D-01
      S(18)= 8.10963D-01
      RETURN
C
C     TUNGSTEN
C     SIGN MISPRINT IN 5S CORRECTED HERE
C
   74 CONTINUE
      E(1)= 2.137D+00
      E(2)= 1.347D+00
      E(3)= 4.366D-01
      E(4)= 1.883D-01
      E(5)= 5.18D-02
      S(1)=-1.404508D+00
      S(2)= 1.767249D+00
      S(3)= 4.73814D-01
      S(4)=-1.0374D-02
      S(5)=-2.967D-03
      E(6)= 3.005D+00
      E(7)= 1.228D+00
      E(8)= 4.415D-01
      E(9)= 9.0D-02
      E(10)= 2.8D-02
      P(6)=-2.38809D-01
      P(7)= 7.3106D-01
      P(8)= 4.84603D-01
      P(9)= 2.0879D-02
      P(10)=-2.906D-03
      E(11)= 9.519D-01
      E(12)= 3.27D-01
      E(13)= 1.054D-01
      D(11)= 4.19829D-01
      D(12)= 5.14641D-01
      D(13)= 2.83185D-01
      E(14)=E(1)
      E(15)=E(2)
      E(16)=E(3)
      E(17)=E(4)
      E(18)=E(5)
      S(14)= 5.76273D-01
      S(15)=-8.03378D-01
      S(16)=-4.81276D-01
      S(17)= 5.58322D-01
      S(18)= 8.06911D-01
      RETURN
C
C     RHENIUM
C     NOTE NORMALIZATION OF 5S=0.999999, 6S=0.999975
C
   75 CONTINUE
      E(1)= 2.185D+00
      E(2)= 1.451D+00
      E(3)= 4.585D-01
      E(4)= 2.314D-01
      E(5)= 5.66D-02
      S(1)=-1.637201D+00
      S(2)= 2.012088D+00
      S(3)= 4.57258D-01
      S(4)=-9.968D-03
      S(5)=-2.363D-03
      E(6)= 3.358D+00
      E(7)= 1.271D+00
      E(8)= 4.644D-01
      E(9)= 8.9D-02
      E(10)= 2.8D-02
      P(6)=-2.30482D-01
      P(7)= 7.41414D-01
      P(8)= 4.60502D-01
      P(9)= 1.8714D-02
      P(10)=-3.238D-03
      E(11)= 1.116D+00
      E(12)= 4.267D-01
      E(13)= 1.378D-01
      D(11)= 3.75354D-01
      D(12)= 4.96983D-01
      D(13)= 3.37017D-01
      E(14)=E(1)
      E(15)=E(2)
      E(16)=E(3)
      E(17)=E(4)
      E(18)=E(5)
      S(14)= 6.67058D-01
      S(15)=-8.95707D-01
      S(16)=-5.14428D-01
      S(17)= 5.2955D-01
      S(18)= 8.74599D-01
      RETURN
C
C     OSMIUM
C     NOTE NORMALIZATION OF 6S=1.00001
C
   76 CONTINUE
      E(1)= 2.222D+00
      E(2)= 1.496D+00
      E(3)= 4.774D-01
      E(4)= 2.437D-01
      E(5)= 5.83D-02
      S(1)=-1.664305D+00
      S(2)= 2.080155D+00
      S(3)= 4.25889D-01
      S(4)=-7.09D-03
      S(5)=-1.334D-03
      E(6)= 2.518D+00
      E(7)= 1.46D+00
      E(8)= 4.923D-01
      E(9)= 9.8D-02
      E(10)= 2.9D-02
      P(6)=-4.15408D-01
      P(7)= 9.3824D-01
      P(8)= 4.64695D-01
      P(9)= 1.6636D-02
      P(10)=-2.172D-03
      E(11)= 1.183D+00
      E(12)= 4.492D-01
      E(13)= 1.463D-01
      D(11)= 3.94332D-01
      D(12)= 4.95481D-01
      D(13)= 3.17556D-01
      E(14)=E(1)
      E(15)=E(2)
      E(16)=E(3)
      E(17)=E(4)
      E(18)=E(5)
      S(14)= 6.92868D-01
      S(15)=-9.55103D-01
      S(16)=-4.87958D-01
      S(17)= 5.46392D-01
      S(18)= 8.6047D-01
      RETURN
C
C     IRIDIUM
C
   77 CONTINUE
      E(1)= 2.35D+00
      E(2)= 1.582D+00
      E(3)= 5.018D-01
      E(4)= 2.5D-01
      E(5)= 5.98D-02
      S(1)=-1.689732D+00
      S(2)= 2.109321D+00
      S(3)= 4.19088D-01
      S(4)=-8.319D-03
      S(5)= 9.3D-05
      E(6)= 2.792D+00
      E(7)= 1.541D+00
      E(8)= 5.285D-01
      E(9)= 9.8D-02
      E(10)= 2.9D-02
      P(6)=-3.86885D-01
      P(7)= 9.02999D-01
      P(8)= 4.66688D-01
      P(9)= 1.6764D-02
      P(10)= -2.778D-3
      E(11)= 1.24D+00
      E(12)= 4.647D-01
      E(13)= 1.529D-01
      D(11)= 4.27804D-01
      D(12)= 4.92986D-01
      D(13)= 2.82354D-01
      E(14)=E(1)
      E(15)=E(2)
      E(16)=E(3)
      E(17)=E(4)
      E(18)=E(5)
      S(14)= 7.01059D-01
      S(15)=-9.68619D-01
      S(16)=-4.46851D-01
      S(17)= 5.18106D-01
      S(18)= 8.59661D-01
      RETURN
C
C     PLATINUM
C
   78 CONTINUE
      E(1)= 2.547D+00
      E(2)= 1.614D+00
      E(3)= 0.5167D+00
      E(4)= 0.2651D+00
      E(5)= 0.058D+00
      S(1)=-1.484838D+00
      S(2)= 1.925735D+00
      S(3)= 0.395138D+00
      S(4)=-0.00953D+00
      S(5)= 0.000889D+00
      E(6)= 2.911D+00
      E(7)= 1.836D+00
      E(8)= 0.5982D+00
      E(9)= 0.0996D+00
      E(10)=0.029D+00
      P(6)=-0.521248D+00
      P(7)= 0.960745D+00
      P(8)= 0.54024D+00
      P(9)= 0.022318D+00
      P(10)=-0.004667D+00
      E(11)=1.243D+00
      E(12)=0.4271D+00
      E(13)=0.137D+00
      D(11)=0.502262D+00
      D(12)=0.494451D+00
      D(13)=0.205456D+00
      E(14)=E(1)
      E(15)=E(2)
      E(16)=E(3)
      E(17)=E(4)
      E(18)=E(5)
      S(14)= 0.599635D+00
      S(15)=-0.871512D+00
      S(16)=-0.455152D+00
      S(17)= 0.559561D+00
      S(18)= 0.844844D+00
      RETURN
C
C     GOLD
C
   79 CONTINUE
      E(1)= 2.809D+00
      E(2)= 1.595D+00
      E(3)= 5.327D-01
      E(4)= 2.826D-01
      E(5)= 5.98D-02
      S(1)=-1.203037D+00
      S(2)= 1.675385D+00
      S(3)= 3.52918D-01
      S(4)=-1.158D-03
      S(5)= 5.77D-04
      E(6)= 3.684D+00
      E(7)= 1.666D+00
      E(8)= 5.989D-01
      E(9)= 9.77D-02
      E(10)= 2.79D-02
      P(6)=-2.78885D-01
      P(7)= 7.77981D-01
      P(8)= 4.78106D-01
      P(9)= 1.7737D-02
      P(10)= -3.781D-03
      E(11)= 1.287D+00
      E(12)= 4.335D-01
      E(13)= 1.396D-01
      D(11)= 5.35907D-01
      D(12)= 4.8583D-01
      D(13)= 1.73628D-01
      E(14)=E(1)
      E(15)=E(2)
      E(16)=E(3)
      E(17)=E(4)
      E(18)=E(5)
      S(14)= 4.73068D-01
      S(15)=-7.59732D-01
      S(16)=-4.22037D-01
      S(17)= 5.324D-01
      S(18)= 8.60865D-01
      RETURN
C
C     MERCURY: SEMICORE DOES NOT EXIST
C
   80 CONTINUE
      E(1)=1.484D+00
      E(2)=0.5605D+00
      E(3)=0.1923D+00
      D(1)=0.4899927D+00
      D(2)=0.4932711D+00
      D(3)=0.1913356D+00
      E(4)=0.5275D+00
      E(5)=0.2334D+00
      E(6)=0.06861D+00
      S(4)=-0.4911676D+00
      S(5)=0.604407D+00
      S(6)=0.769026D+00
      E(7)=0.6503D+00
      E(8)=0.1368D+00
      E(9)=0.04256D+00
      P(7)=-0.0672271D+00
      P(8)=0.4979023D+00
      P(9)=0.6187761D+00
      RETURN
      END
C*MODULE BASECP  *DECK SBKBAS
      SUBROUTINE SBKBAS(NUCZ,CSINP,CPINP,CDINP,CFINP,IERR1,IERR2,
     *                  INTYP,NANGM,NBFS,MINF,MAXF,LOC,NGAUSS,NS)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXSH=1000, MXATM=500, MXGTOT=5000)
      PARAMETER (ZERO=0.0D+00, PT5=0.5D+00, ONE=1.0D+00,
     *           PI32=5.56832799683170D+00, TOL=1.0D-6,
     *           PT75=0.75D+00, PT1875=1.875D+00)
C
      LOGICAL DONE,GOPARR,DSKWRK,MASWRK
C
      DIMENSION CSINP(*),CPINP(*),CDINP(*),CFINP(*),
     *          INTYP(*),NANGM(*),NBFS(*),MINF(*),MAXF(*),NS(*)
      DIMENSION EEX(16),CCS(16),CCP(16),CCD(16),CCF(16)
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
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA TYPS,TYPP,TYPD,TYPF
     *       /8HS       ,8HP       ,8HD       ,8HF       /
#else
      CHARACTER*8 :: TYPS_STR, TYPP_STR, TYPD_STR, TYPF_STR
      EQUIVALENCE (TYPS, TYPS_STR), (TYPP, TYPP_STR), (TYPD, TYPD_STR),
     *            (TYPF, TYPF_STR)
      DATA TYPS_STR,TYPP_STR,TYPD_STR,TYPF_STR
     *       /"S       ","P       ","D       ","F       "/
#endif
C
C     ----- DRIVER FOR THE "SBK" BASIS SETS -----
C     LI-AR
C     W.J.STEVENS, H.BASCH, M.KRAUSS, J.CHEM.PHYS. 81,6026(1984)
C     NA-RN, EXCLUDING LANTHANIDES.
C     W.J.STEVENS, M.KRAUSS, H.BASCH, P.G.JASIEN
C     CAN.J.CHEM. 70,612(1992)
C     THE LANTHANIDES
C     T.R.CUNDARI, W.J.STEVENS, J.CHEM.PHYS. 98,5555(1993)
C
      DO 100 I = 1,16
         EEX(I) = ZERO
         CCS(I) = ZERO
         CCP(I) = ZERO
         CCD(I) = ZERO
         CCF(I) = ZERO
  100 CONTINUE
C
C     ----- HYDROGEN AND HELIUM USE THE UNSCALED POPLE -31G BASIS -----
C
      IF (NUCZ.GT.2) GO TO 210
      CALL N31ONE(EEX,CCS,NUCZ)
      GO TO 300
C
C     ----- LITHIUM TO NEON -----
C
  210 CONTINUE
      IF (NUCZ .GT. 10) GO TO 220
      CALL SBKTWO(NUCZ,EEX,CCS,CCP)
      GO TO 300
C
C     ----- SODIUM TO ARGON -----
C
  220 CONTINUE
      IF (NUCZ.GT.18) GO TO 230
      CALL SBKTHR(NUCZ,EEX,CCS,CCP)
      GO TO 300
C
C     ----- POTASSIUM TO KRYPTON -----
C
  230 CONTINUE
      IF (NUCZ.GT.36) GO TO 240
      IF (NUCZ.GE.21  .AND.  NUCZ.LE.30) THEN
         CALL SBKTM1(NUCZ,EEX,CCS,CCP,CCD)
      ELSE
         CALL SBKFOU(NUCZ,EEX,CCS,CCP,CCD)
      END IF
      GO TO 300
C
C     ----- RUBIDIUM TO XENON -----
C
  240 CONTINUE
      IF (NUCZ.GT.54) GO TO 250
      IF (NUCZ.GE.39  .AND.  NUCZ.LE.48) THEN
         CALL SBKTM2(NUCZ,EEX,CCS,CCP,CCD)
      ELSE
         CALL SBKFIV(NUCZ,EEX,CCS,CCP,CCD)
      END IF
      GO TO 300
C
C     ----- CESIUM TO RADON -----
C
  250 CONTINUE
      IF (NUCZ.GT.86) GO TO 260
      IF (NUCZ.GE.58 .AND. NUCZ.LE.71) THEN
         CALL SBKLAN(NUCZ,EEX,CCS,CCP,CCD,CCF)
      ELSE IF (NUCZ.GE.57 .AND. NUCZ.LE.80) THEN
         CALL SBKTM3(NUCZ,EEX,CCS,CCP,CCD)
      ELSE
         CALL SBKSIX(NUCZ,EEX,CCS,CCP,CCD)
      END IF
      GO TO 300
C
C     PAST RADON IS A BOOBOO
C
  260 CONTINUE
      IF (MASWRK) WRITE(IW,*)
     *  'ERROR. SBK BASES DO NOT EXTEND PAST RN.'
      CALL ABRT
      STOP
C
C     ----- LOOP OVER EACH SHELL -----
C
  300 CONTINUE
      IERR3=0
      NG=0
      IPASS=0
      DONE = .FALSE.
  310 CONTINUE
      IPASS=IPASS+1
      CALL SBKSHL(NUCZ,IPASS,ITYP,IGAUSS,DONE)
      IF(DONE  .AND.  IERR3.NE.0) THEN
         IF (MASWRK) WRITE(IW,910)
         CALL ABRT
      END IF
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
      DO 440 I = 1,IGAUSS
         K = K1+I-1
         EX(K) = EEX(NG+I)
         CSINP(K) = CCS(NG+I)
         CPINP(K) = CCP(NG+I)
         CDINP(K) = CCD(NG+I)
         CFINP(K) = CCF(NG+I)
         CS(K) = CSINP(K)
         CP(K) = CPINP(K)
         CD(K) = CDINP(K)
         CF(K) = CFINP(K)
  440 CONTINUE
C
C     ----- ALWAYS UNNORMALIZE PRIMITIVES -----
C
      DO 460 K = K1,K2
         EE = EX(K)+EX(K)
         FACS = PI32/(EE*SQRT(EE))
         FACP = PT5*FACS/EE
         FACD = PT75*FACS/(EE*EE)
         FACF = PT1875*FACS/(EE*EE*EE)
         IF(ITYP.EQ.1  .OR.  ITYP.EQ.6) CS(K) = CS(K)/SQRT(FACS)
         IF(ITYP.EQ.2  .OR.  ITYP.EQ.6) CP(K) = CP(K)/SQRT(FACP)
         IF(ITYP.EQ.3                 ) CD(K) = CD(K)/SQRT(FACD)
         IF(ITYP.EQ.4                 ) CF(K) = CF(K)/SQRT(FACF)
  460 CONTINUE
C
C     ----- IF(NORMF.EQ.0) NORMALIZE BASIS FUNCTIONS -----
C
      IF (NORMF .EQ. 1) GO TO 600
      FACS = ZERO
      FACP = ZERO
      FACD = ZERO
      FACF = ZERO
      DO 510 IG = K1,K2
         DO 500 JG = K1,IG
            EE = EX(IG)+EX(JG)
            FAC = EE*SQRT(EE)
            DUMS = CS(IG)*CS(JG)/FAC
            DUMP = PT5*CP(IG)*CP(JG)/(EE*FAC)
            DUMD = PT75*CD(IG)*CD(JG)/(EE*EE*FAC)
            DUMF = PT1875*CF(IG)*CF(JG)/(EE*EE*EE*FAC)
            IF (IG .EQ. JG) GO TO 480
               DUMS = DUMS+DUMS
               DUMP = DUMP+DUMP
               DUMD = DUMD+DUMD
               DUMF = DUMF+DUMF
  480       CONTINUE
            FACS = FACS+DUMS
            FACP = FACP+DUMP
            FACD = FACD+DUMD
            FACF = FACF+DUMF
  500    CONTINUE
  510 CONTINUE
      IF(ITYP.EQ.1  .OR.  ITYP.EQ.6) FACS=ONE/SQRT(FACS*PI32)
      IF(ITYP.EQ.2  .OR.  ITYP.EQ.6) FACP=ONE/SQRT(FACP*PI32)
      IF(ITYP.EQ.3                 ) FACD=ONE/SQRT(FACD*PI32)
      IF(ITYP.EQ.4                 ) FACF=ONE/SQRT(FACF*PI32)
C
C        VERIFY NORMALIZATION ON THE INTERNALLY STORED BASES
C        THE DATA FOR LI-AR ARE NOT NORMALIZED.
C
      IF(NUCZ.LE.18) GO TO 540
      IF(ITYP.NE.1  .AND.  ITYP.NE.6) GO TO 520
         TESTS=ABS(ONE-FACS)
         IF(TESTS.LT.TOL) GO TO 520
         TYPE = TYPS
         IF (MASWRK) THEN
            WRITE(IW,900) NUCZ,IPASS,TYPE,FACS
            WRITE(IW,920) (EX(K),K=K1,K2)
            WRITE(IW,920) (CSINP(K),K=K1,K2)
         END IF
         IERR3=IERR3+1
  520 IF(ITYP.NE.2  .AND.  ITYP.NE.6) GO TO 530
         TESTP=ABS(ONE-FACP)
         IF(TESTP.LT.TOL) GO TO 540
         TYPE = TYPP
         IF (MASWRK) THEN
            WRITE(IW,900) NUCZ,IPASS,TYPE,FACP
            WRITE(IW,920) (EX(K),K=K1,K2)
            WRITE(IW,920) (CPINP(K),K=K1,K2)
         END IF
         IERR3=IERR3+1
  530 IF(ITYP.NE.3) GO TO 535
         TESTD=ABS(ONE-FACD)
         IF(TESTD.LT.TOL) GO TO 540
         TYPE = TYPD
         IF (MASWRK) THEN
            WRITE(IW,900) NUCZ,IPASS,TYPE,FACD
            WRITE(IW,920) (EX(K),K=K1,K2)
            WRITE(IW,920) (CDINP(K),K=K1,K2)
         END IF
         IERR3=IERR3+1
  535 IF(ITYP.NE.4) GO TO 540
         TESTF=ABS(ONE-FACF)
         IF(TESTF.LT.TOL) GO TO 540
         TYPE = TYPF
         IF (MASWRK) THEN
            WRITE(IW,900) NUCZ,IPASS,TYPE,FACF
            WRITE(IW,920) (EX(K),K=K1,K2)
            WRITE(IW,920) (CFINP(K),K=K1,K2)
         END IF
         IERR3=IERR3+1
C
  540 CONTINUE
      DO 590 IG = K1,K2
         IF(ITYP.EQ.1  .OR.  ITYP.EQ.6) CS(IG) = FACS*CS(IG)
         IF(ITYP.EQ.2  .OR.  ITYP.EQ.6) CP(IG) = FACP*CP(IG)
         IF(ITYP.EQ.3                 ) CD(IG) = FACD*CD(IG)
         IF(ITYP.EQ.4                 ) CF(IG) = FACF*CF(IG)
         IF(ITYP.EQ.1  .OR.  ITYP.EQ.6) CSINP(IG) = FACS*CSINP(IG)
         IF(ITYP.EQ.2  .OR.  ITYP.EQ.6) CPINP(IG) = FACP*CPINP(IG)
         IF(ITYP.EQ.3                 ) CDINP(IG) = FACD*CDINP(IG)
         IF(ITYP.EQ.4                 ) CFINP(IG) = FACF*CFINP(IG)
  590 CONTINUE
C
  600 CONTINUE
      NG=NG+IGAUSS
      GO TO 310
C
  900 FORMAT(1X,'ERROR WITH NORMALIZATION OF -SBK- BASIS FUNCTION'/
     * 1X,'NUCZ=',I4,' SHELL=',I4,' TYPE=',A1,' NORMALIZATION=',F15.8)
  910 FORMAT(1X,'NORMALIZATION ERROR --- CHECK INTERNAL BASIS SET')
  920 FORMAT(1X,6F16.7)
      END
C*MODULE BASECP  *DECK SBKSHL
      SUBROUTINE SBKSHL(NUCZ,IPASS,ITYP,IGAUSS,DONE)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL DONE
      DIMENSION MAXPS(8),ITYPE(8,8),NGAUSS(8,8)
      DATA MAXPS/2,2,2,7,7,7,7,8/
      DATA ITYPE/1,1, 6*0,
     *           6,6, 6*0,
     *           6,6, 6*0,
     *           6,3,3,6,6,6,3,0,
     *           6,3,3,6,6,6,3,0,
     *           6,3,3,6,6,6,3,0,
     *           6,3,6,6,6,3,3,0,
     *           6,6,6,6,3,3,4,4/
      DATA NGAUSS/3,1,   6*0,
     *            3,1,   6*0,
     *            4,1,   6*0,
     *            4,4,1,2,1,1,1,0,
     *            4,3,1,2,1,1,1,0,
     *            4,3,1,1,1,1,1,0,
     *            4,4,1,2,1,1,1,0,
     *            3,1,1,1,2,1,5,2/
C
C     ----- DEFINE CURRENT SHELL PARAMETERS FOR "SBK" BASES -----
C        NUCZ  =NUCLEAR CHARGE OF THIS ATOM
C        IPASS =NUMBER OF CURRENT SHELL
C        MXPASS=TOTAL NUMBER OF SHELLS ON THIS ATOM
C        ITYP  =1,2,3,4,6 FOR S,P,D,F,L SHELLS
C        IGAUSS=NUMBER OF GAUSSIANS IN CURRENT SHELL
C
C             H AND HE USE A -31G S ONLY SHELL
      KIND=1
C             ALKALIS, AND LIGHT RIGHT MAIN GROUP USE A -31G SPLIT
      IF(NUCZ.GE. 3  .AND.  NUCZ.LE.20) KIND=2
      IF(NUCZ.GE.37  .AND.  NUCZ.LE.38) KIND=2
      IF(NUCZ.GE.55  .AND.  NUCZ.LE.56) KIND=2
C             HEAVIER RIGHT MAIN GROUP USE A -41G SPLIT
      IF(NUCZ.GE.32  .AND.  NUCZ.LE.36) KIND=3
      IF(NUCZ.GE.50  .AND.  NUCZ.LE.54) KIND=3
      IF(NUCZ.GE.82  .AND.  NUCZ.LE.86) KIND=3
C             SEMI-CORE TRANSITION METALS
      IF(NUCZ.GE.21  .AND.  NUCZ.LE.30) KIND=4
      IF(NUCZ.GE.39  .AND.  NUCZ.LE.48) KIND=5
      IF(NUCZ.EQ.57                   ) KIND=6
      IF(NUCZ.GE.72  .AND.  NUCZ.LE.80) KIND=6
C             SEMI-CORE GA, IN, TL
      IF(NUCZ.EQ.31  .OR.  NUCZ.EQ.49  .OR.  NUCZ.EQ.81) KIND=7
C             LANTHANIDES (MEANING CE-LU)
      IF(NUCZ.GE.58  .AND.  NUCZ.LE.71) KIND=8
C
      MXPASS=MAXPS(KIND)
      IF(IPASS.GT.MXPASS) DONE=.TRUE.
      IF(DONE) RETURN
C
      ITYP = ITYPE(IPASS,KIND)
      IGAUSS = NGAUSS(IPASS,KIND)
C
C            LA, HG ARE NOT QUITE LIKE THE REST OF 3RD TM SERIES
      IF(NUCZ.EQ.57  .AND.  IPASS.EQ.1) IGAUSS=5
      IF(NUCZ.EQ.57  .AND.  IPASS.EQ.4) IGAUSS=2
      IF(NUCZ.EQ.80  .AND.  IPASS.EQ.1) IGAUSS=5
C            IN IS NOT QUITE LIKE GA
      IF(NUCZ.EQ.49  .AND.  IPASS.EQ.2) IGAUSS=3
C            TL IS NOT QUITE LIKE GA
      IF(NUCZ.EQ.81  .AND.  IPASS.EQ.1) IGAUSS=5
      IF(NUCZ.EQ.81  .AND.  IPASS.EQ.2) IGAUSS=3
      IF(NUCZ.EQ.81  .AND.  IPASS.EQ.3) IGAUSS=1
      IF(NUCZ.EQ.81  .AND.  IPASS.EQ.4) IGAUSS=1
      RETURN
      END
C*MODULE BASECP  *DECK SBKLAN
      SUBROUTINE SBKLAN(NUCZ,E,S,P,D,F)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION E(16), S(16), P(16), D(16), F(16)
C
C     ----- "SBK" TYPE BASIS FOR THE LANTHANIDES -----
C     T.R.CUNDARI, W.J.STEVENS, J.CHEM.PHYS, 98, 5555(1993).
C
      IJUMP = NUCZ-57
C
      GO TO (58,59,60,61,62,63,64,65,66,67,68,69,70,71), IJUMP
C
C   CERIUM
C
 58   CONTINUE
      E(1) =  3.45700D+00
      E(2) =  4.29100D+00
      E(3) =  2.29000D+00
      E(4) =  0.28570D+00
      E(5) =  0.66840D+00
      E(6) =  0.06957D+00
      S(1) =  3.1374906D+00
      S(2) = -1.4295166D+00
      S(3) = -2.6758482D+00
      S(4) =  1.0000000D+00
      S(5) =  1.0000000D+00
      S(6) =  1.0000000D+00
      P(1) =  1.9497168D+00
      P(2) = -0.9123114D+00
      P(3) = -2.0200371D+00
      P(4) =  1.0000000D+00
      P(5) =  1.0000000D+00
      P(6) =  1.0000000D+00
      D(7) =   0.3946619D+00
      D(8) =   0.6503444D+00
      D(9) =   1.0000000D+00
      E(7) =  0.59160D+00
      E(8) =  0.30020D+00
      E(9) =  0.12440D+00
      F(10) =   0.0073853D+00
      F(11) =   0.0618916D+00
      F(12) =   0.2011817D+00
      F(13) =   0.3887943D+00
      F(14) =   0.5522320D+00
      F(15) =   0.7217269D+00
      F(16) =   0.3878059D+00
      E(10)= 83.88000D+00
      E(11)= 29.97000D+00
      E(12)= 13.05000D+00
      E(13)=  5.72700D+00
      E(14)=  2.54900D+00
      E(15)=  1.07500D+00
      E(16)=  0.39860D+00
      RETURN
C
C  PRASEODYMIUM
C
 59   CONTINUE
      E(1) =  3.45800D+00
      E(2) =  4.92100D+00
      E(3) =  2.10000D+00
      E(4) =  0.72900D+00
      E(5) =  0.29940D+00
      E(6) =  0.07179D+00
      S(1) =  0.8806114D+00
      S(2) = -0.1917569D+00
      S(3) = -1.6579589D+00
      S(4) =  1.0000000D+00
      S(5) =  1.0000000D+00
      S(6) =  1.0000000D+00
      P(1) =  0.7403625D+00
      P(2) = -0.2657420D+00
      P(3) = -1.4636781D+00
      P(4) =  1.0000000D+00
      P(5) =  1.0000000D+00
      P(6) =  1.0000000D+00
      D(7) =   0.3849163D+00
      D(8) =   0.6634936D+00
      D(9) =   1.0000000D+00
      E(7) =  0.64190D+00
      E(8) =  0.31570D+00
      E(9) =  0.12900D+00
      F(10) =   0.0076926D+00
      F(11) =   0.0621079D+00
      F(12) =   0.2008683D+00
      F(13) =   0.3881868D+00
      F(14) =   0.5494929D+00
      F(15) =   0.7209543D+00
      F(16) =   0.3882895D+00
      E(10)= 88.46000D+00
      E(11)= 32.00000D+00
      E(12)= 14.09000D+00
      E(13)=  6.24600D+00
      E(14)=  2.80800D+00
      E(15)=  1.19800D+00
      E(16)=  0.44510D+00
      RETURN
C
C   NEODYMIUM
C
 60   CONTINUE
      E(1) =  5.63400D+00
      E(2) =  4.72400D+00
      E(3) =  2.06400D+00
      E(4) =  0.77240D+00
      E(5) =  0.31270D+00
      E(6) =  0.07372D+00
      S(1) = -0.7064226D+00
      S(2) =  1.1373480D+00
      S(3) = -1.4029331D+00
      S(4) =  1.0000000D+00
      S(5) =  1.0000000D+00
      S(6) =  1.0000000D+00
      P(1) = -0.5818411D+00
      P(2) =  0.8487167D+00
      P(3) = -1.2523796D+00
      P(4) =  1.0000000D+00
      P(5) =  1.0000000D+00
      P(6) =  1.0000000D+00
      D(7) =   0.3873425D+00 
      D(8) =   0.6642494D+00
      D(9) =   1.0000000D+00 
      E(7) =  0.67780D+00
      E(8) =  0.32550D+00
      E(9) =  0.13240D+00
      F(10) =   0.0077258D+00 
      F(11) =   0.0617211D+00 
      F(12) =   0.2004538D+00 
      F(13) =   0.3857174D+00 
      F(14) =   0.5490437D+00 
      F(15) =   0.7190505D+00 
      F(16) =   0.3912950D+00 
      E(10)= 94.38000D+00
      E(11)= 34.33000D+00
      E(12)= 15.21000D+00
      E(13)=  6.81600D+00
      E(14)=  3.09600D+00
      E(15)=  1.33000D+00
      E(16)=  0.49230D+00
      RETURN
C
C   PROMETHIUM
C
 61   CONTINUE
      E(1) =  8.28900D+00
      E(2) =  3.85600D+00
      E(3) =  2.12300D+00
      E(4) =  0.82050D+00
      E(5) =  0.32550D+00
      E(6) =  0.07554D+00
      S(1) = -0.0647755D+00
      S(2) =  0.5300920D+00
      S(3) = -1.4386781D+00
      S(4) =  1.0000000D+00
      S(5) =  1.0000000D+00
      S(6) =  1.0000000D+00
      P(1) = -0.0785342D+00
      P(2) =  0.3975542D+00
      P(3) = -1.3026512D+00
      P(4) =  1.0000000D+00
      P(5) =  1.0000000D+00
      P(6) =  1.0000000D+00
      D(7) =   0.3689878D+00 
      D(8) =   0.6815842D+00 
      D(9) =   1.0000000D+00 
      E(7) =  0.71890D+00
      E(8) =  0.34490D+00
      E(9) =  0.13700D+00
      F(10) =   0.0077411D+00 
      F(11) =   0.0614518D+00 
      F(12) =   0.1994959D+00 
      F(13) =   0.3823259D+00 
      F(14) =   0.5500773D+00 
      F(15) =   0.7143553D+00 
      F(16) =   0.3988111D+00 
      E(10)=101.10000D+00
      E(11)= 36.86000D+00
      E(12)= 16.43000D+00
      E(13)=  7.44500D+00
      E(14)=  3.41400D+00
      E(15)=  1.47200D+00
      E(16)=  0.53960D+00
      RETURN
C
C   SAMARIUM
C
 62   CONTINUE
      E(1) = 12.61000D+00
      E(2) =  3.27800D+00
      E(3) =  2.23800D+00
      E(4) =  0.86610D+00
      E(5) =  0.33740D+00
      E(6) =  0.07732D+00
      S(1) = -0.0149996D+00
      S(2) =  0.7187788D+00
      S(3) = -1.6781500D+00
      S(4) =  1.0000000D+00
      S(5) =  1.0000000D+00
      S(6) =  1.0000000D+00
      P(1) = -0.0315794D+00
      P(2) =  0.5090988D+00
      P(3) = -1.4640624D+00
      P(4) =  1.0000000D+00
      P(5) =  1.0000000D+00
      P(6) =  1.0000000D+00
      D(7) =   0.3794177D+00 
      D(8) =   0.6769150D+00 
      D(9) =   1.0000000D+00 
      E(7) =  0.78510D+00
      E(8) =  0.36220D+00
      E(9) =  0.15730D+00
      F(10) =   0.0124675D+00 
      F(11) =   0.0875616D+00 
      F(12) =   0.2405394D+00 
      F(13) =   0.4205135D+00 
      F(14) =   0.4734687D+00 
      F(15) =   0.7473807D+00 
      F(16) =   0.3463134D+00 
      E(10)= 83.76000D+00
      E(11)= 30.54000D+00
      E(12)= 13.16000D+00
      E(13)=  5.73000D+00
      E(14)=  2.58500D+00
      E(15)=  1.13400D+00
      E(16)=  0.44450D+00
      RETURN
C
C  EUROPIUM
C
 63   CONTINUE
      E(1) = 14.22000D+00
      E(2) =  3.23700D+00
      E(3) =  2.34400D+00
      E(4) =  0.90470D+00
      E(5) =  0.34860D+00
      E(6) =  0.07916D+00
      S(1) = -0.0090055D+00
      S(2) =  0.8423269D+00
      S(3) = -1.8080826D+00
      S(4) =  1.0000000D+00
      S(5) =  1.0000000D+00
      S(6) =  1.0000000D+00
      P(1) = -0.0261021D+00
      P(2) =  0.5780793D+00
      P(3) = -1.5403465D+00
      P(4) =  1.0000000D+00
      P(5) =  1.0000000D+00
      P(6) =  1.0000000D+00
      D(7) =   0.3280824D+00
      D(8) =   0.7171188D+00
      D(9) =   1.0000000D+00
      E(7) =  0.81540D+00
      E(8) =  0.39910D+00
      E(9) =  0.15110D+00
      F(10) =   0.0139652D+00
      F(11) =   0.0943945D+00
      F(12) =   0.2492058D+00
      F(13) =   0.4238233D+00
      F(14) =   0.4589102D+00
      F(15) =   0.7550047D+00
      F(16) =   0.3360615D+00
      E(10)= 83.90000D+00
      E(11)= 30.66000D+00
      E(12)= 13.17000D+00
      E(13)=  5.74500D+00
      E(14)=  2.58800D+00
      E(15)=  1.13400D+00
      E(16)=  0.44720D+00
      RETURN
C
C   GADOLINIUM
C
 64   CONTINUE
      E(1) = 17.24000D+00
      E(2) =  3.34600D+00
      E(3) =  2.42900D+00
      E(4) =  0.93490D+00
      E(5) =  0.35880D+00
      E(6) =  0.08105D+00
      S(1) =  0.0036070D+00
      S(2) =  0.7375706D+00
      S(3) = -1.7149278D+00
      S(4) =  1.0000000D+00
      S(5) =  1.0000000D+00
      S(6) =  1.0000000D+00
      P(1) = -0.0207287D+00
      P(2) =  0.5191487D+00
      P(3) = -1.4883822D+00
      P(4) =  1.0000000D+00
      P(5) =  1.0000000D+00
      P(6) =  1.0000000D+00
      D(7) =   0.3051932D+00
      D(8) =   0.7399451D+00
      D(9) =   1.0000000D+00
      E(7) =  0.85730D+00
      E(8) =  0.41280D+00
      E(9) =  0.15190D+00
      F(10) =   0.0180914D+00
      F(11) =   0.1102055D+00
      F(12) =   0.2694254D+00
      F(13) =   0.4344260D+00
      F(14) =   0.4265979D+00
      F(15) =   0.7856925D+00
      F(16) =   0.2938734D+00
      E(10)= 77.60000D+00
      E(11)= 28.61000D+00
      E(12)= 12.13000D+00
      E(13)=  5.23900D+00
      E(14)=  2.30500D+00
      E(15)=  0.99420D+00
      E(16)=  0.40310D+00
      RETURN
C
C   TERBIUM
C
 65   CONTINUE
      E(1) = 13.18000D+00
      E(2) =  3.84700D+00
      E(3) =  2.53700D+00
      E(4) =  0.96140D+00
      E(5) =  0.36970D+00
      E(6) =  0.08254D+00
      S(1) = -0.0398058D+00
      S(2) =  0.7806111D+00
      S(3) = -1.7158528D+00
      S(4) =  1.0000000D+00
      S(5) =  1.0000000D+00
      S(6) =  1.0000000D+00
      P(1) = -0.0326821D+00
      P(2) =  0.3979847D+00
      P(3) = -1.3560540D+00
      P(4) =  1.0000000D+00
      P(5) =  1.0000000D+00
      P(6) =  1.0000000D+00
      D(7) =   0.2846558D+00
      D(8) =   0.7609937D+00
      D(9) =   1.0000000D+00
      E(7) =  0.90250D+00
      E(8) =  0.42480D+00
      E(9) =  0.15210D+00
      F(10) =   0.0151330D+00
      F(11) =   0.0998279D+00
      F(12) =   0.2549994D+00
      F(13) =   0.4191934D+00
      F(14) =   0.4497551D+00
      F(15) =   0.7446962D+00
      F(16) =   0.3516849D+00
      E(10)= 90.72000D+00
      E(11)= 33.17000D+00
      E(12)= 14.33000D+00
      E(13)=  6.35700D+00
      E(14)=  2.91400D+00
      E(15)=  1.28600D+00
      E(16)=  0.49850D+00
      RETURN
C
C   DYSPROSIUM
C
 66   CONTINUE
      E(1) = 12.36000D+00
      E(2) =  4.15500D+00
      E(3) =  2.64700D+00
      E(4) =  0.99290D+00
      E(5) =  0.38080D+00
      E(6) =  0.08408D+00
      S(1) = -0.0469790D+00
      S(2) =  0.7231229D+00
      S(3) = -1.6509757D+00
      S(4) =  1.0000000D+00
      S(5) =  1.0000000D+00
      S(6) =  1.0000000D+00
      P(1) = -0.0393311D+00
      P(2) =  0.3666596D+00
      P(3) = -1.3190207D+00
      P(4) =  1.0000000D+00
      P(5) =  1.0000000D+00
      P(6) =  1.0000000D+00
      E(7) =  0.92440D+00
      E(8) =  0.42800D+00
      E(9) =  0.15230D+00
      D(7) =   0.2894139D+00
      D(8) =   0.7585502D+00
      D(9) =   1.0000000D+00
      E(10)= 97.47000D+00
      E(11)= 35.71000D+00
      E(12)= 15.58000D+00
      E(13)=  7.00600D+00
      E(14)=  3.25900D+00
      E(15)=  1.44000D+00
      E(16)=  0.54570D+00
      F(10) =   0.0147178D+00
      F(11) =   0.0977056D+00
      F(12) =   0.2512386D+00
      F(13) =   0.4123795D+00
      F(14) =   0.4564398D+00
      F(15) =   0.7338642D+00
      F(16) =   0.3690476D+00
      RETURN
C
C    HOLMIUM
C
 67   CONTINUE
      E(1) = 12.31000D+00
      E(2) =  4.30500D+00
      E(3) =  2.74900D+00
      E(4) =  1.02400D+00
      E(5) =  0.39140D+00
      E(6) =  0.08555D+00
      S(1) = -0.0454979D+00
      S(2) =  0.7105661D+00
      S(3) = -1.6401006D+00
      S(4) =  1.0000000D+00
      S(5) =  1.0000000D+00
      S(6) =  1.0000000D+00
      P(1) = -0.0383146D+00
      P(2) =  0.3479272D+00
      P(3) = -1.3021661D+00
      P(4) =  1.0000000D+00
      P(5) =  1.0000000D+00
      P(6) =  1.0000000D+00
      E(7) =  0.95070D+00
      E(8) =  0.43170D+00
      E(9) =  0.15310D+00
      D(7) =   0.2945199D+00 
      D(8) =   0.7561902D+00 
      D(9) =   1.0000000D+00 
      E(10)=104.90000D+00
      E(11)= 38.50000D+00
      E(12)= 16.95000D+00
      E(13)=  7.72500D+00
      E(14)=  3.63500D+00
      E(15)=  1.60200D+00
      E(16)=  0.59480D+00
      F(10) =   0.0141835D+00 
      F(11) =   0.0950565D+00 
      F(12) =   0.2466934D+00 
      F(13) =   0.4052217D+00 
      F(14) =   0.4651430D+00 
      F(15) =   0.7262095D+00 
      F(16) =   0.3822719D+00 
      RETURN
C
C    ERBIUM
C
 68   CONTINUE
      E(1) = 12.58000D+00
      E(2) =  4.44900D+00
      E(3) =  2.87300D+00
      E(4) =  1.05800D+00
      E(5) =  0.40250D+00
      E(6) =  0.08708D+00
      S(1) = -0.0425587D+00
      S(2) =  0.7179336D+00
      S(3) = -1.6502894D+00
      S(4) =  1.0000000D+00
      S(5) =  1.0000000D+00
      S(6) =  1.0000000D+00
      P(1) = -0.0402829D+00
      P(2) =  0.3577179D+00
      P(3) = -1.3110413D+00
      P(4) =  1.0000000D+00
      P(5) =  1.0000000D+00
      P(6) =  1.0000000D+00
      E(7) =  0.97910D+00
      E(8) =  0.43670D+00
      E(9) =  0.15440D+00
      D(7) =   0.2987899D+00 
      D(8) =   0.7544645D+00 
      D(9) =   1.0000000D+00 
      E(10)=105.10000D+00
      E(11)= 38.68000D+00
      E(12)= 17.00000D+00
      E(13)=  7.74500D+00
      E(14)=  3.64200D+00
      E(15)=  1.60800D+00
      E(16)=  0.59800D+00
      F(10) =   0.0156082D+00 
      F(11) =   0.1009470D+00 
      F(12) =   0.2538319D+00 
      F(13) =   0.4082474D+00 
      F(14) =   0.4524062D+00 
      F(15) =   0.7274973D+00 
      F(16) =   0.3804196D+00 
      RETURN
C
C    THULIUM
C
 69   CONTINUE
      E(1) = 11.04000D+00
      E(2) =  4.88100D+00
      E(3) =  2.92800D+00
      E(4) =  1.08000D+00
      E(5) =  0.41210D+00
      E(6) =  0.08851D+00
      S(1) = -0.0592729D+00
      S(2) =  0.6115702D+00
      S(3) = -1.5277045D+00
      S(4) =  1.0000000D+00
      S(5) =  1.0000000D+00
      S(6) =  1.0000000D+00
      P(1) = -0.0242818D+00
      P(2) =  0.2161241D+00
      P(3) = -1.1825576D+00
      P(4) =  1.0000000D+00
      P(5) =  1.0000000D+00
      P(6) =  1.0000000D+00
      E(7) =  1.00900D+00
      E(8) =  0.44270D+00
      E(9) =  0.15560D+00
      D(7) =   0.3005086D+00
      D(8) =   0.7548745D+00
      D(9) =   1.0000000D+00
      E(10)=113.40000D+00
      E(11)= 41.80000D+00
      E(12)= 18.53000D+00
      E(13)=  8.54700D+00
      E(14)=  4.05600D+00
      E(15)=  1.77900D+00
      E(16)=  0.64780D+00
      F(10) =   0.0148458D+00
      F(11) =   0.0973394D+00
      F(12) =   0.2481870D+00
      F(13) =   0.4010063D+00
      F(14) =   0.4634212D+00
      F(15) =   0.7219403D+00
      F(16) =   0.3913644D+00
      RETURN
C
C    YTTERBIUM
C
 70   CONTINUE
      E(1) = 10.08000D+00
      E(2) =  5.39500D+00
      E(3) =  3.03100D+00
      E(4) =  1.10800D+00
      E(5) =  0.42210D+00
      E(6) =  0.09008D+00
      S(1) = -0.0890681D+00
      S(2) =  0.5767409D+00
      S(3) = -1.4628924D+00
      S(4) =  1.0000000D+00
      S(5) =  1.0000000D+00
      S(6) =  1.0000000D+00
      P(1) = -0.0359281D+00
      P(2) =  0.1941357D+00
      P(3) = -1.1499825D+00
      P(4) =  1.0000000D+00
      P(5) =  1.0000000D+00
      P(6) =  1.0000000D+00
      E(7) =  1.05100D+00
      E(8) =  0.45190D+00
      E(9) =  0.15620D+00
      D(7) =   0.2939074D+00
      D(8) =   0.7630676D+00
      D(9) =   1.0000000D+00
      E(10)=122.50000D+00
      E(11)= 45.18000D+00
      E(12)= 20.20000D+00
      E(13)=  9.42800D+00
      E(14)=  4.50100D+00
      E(15)=  1.96000D+00
      E(16)=  0.70050D+00
      F(10) =   0.0140468D+00
      F(11) =   0.0934822D+00
      F(12) =   0.2418361D+00
      F(13) =   0.3940876D+00
      F(14) =   0.4754289D+00
      F(15) =   0.7171299D+00
      F(16) =   0.4009835D+00
      RETURN
C
C    LUTETIUM
C
 71   CONTINUE
      E(1) =  9.46900D+00
      E(2) =  5.56800D+00
      E(3) =  3.18200D+00
      E(4) =  1.13500D+00
      E(5) =  0.43200D+00
      E(6) =  0.09161D+00
      S(1) = -0.1119004D+00
      S(2) =  0.6321414D+00
      S(3) = -1.4951712D+00
      S(4) =  1.0000000D+00
      S(5) =  1.0000000D+00
      S(6) =  1.0000000D+00
      P(1) = -0.0463915D+00
      P(2) =  0.2179130D+00
      P(3) = -1.1634161D+00
      P(4) =  1.0000000D+00
      P(5) =  1.0000000D+00
      P(6) =  1.0000000D+00
      E(7) =  1.10100D+00
      E(8) =  0.46390D+00
      E(9) =  0.15640D+00
      D(7) =   0.2744246D+00
      D(8) =   0.7823096D+00
      D(9) =   1.0000000D+00
      E(10)=117.80000D+00
      E(11)= 43.40000D+00
      E(12)= 19.13000D+00
      E(13)=  8.79700D+00
      E(14)=  4.15300D+00
      E(15)=  1.81700D+00
      E(16)=  0.66170D+00
      F(10) =   0.0164643D+00
      F(11) =   0.1045249D+00
      F(12) =   0.2577225D+00
      F(13) =   0.4054234D+00
      F(14) =   0.4476287D+00
      F(15) =   0.7256251D+00
      F(16) =   0.3870116D+00
      RETURN
C
      END
C
C*MODULE BASECP  *DECK SBKTWO
      SUBROUTINE SBKTWO(N,E,S,P)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION E(4),S(4),P(4)
C
C     ----- STEVENS, BASCH, KRAUSS BASES FOR 2ND ROW -----
C
      NN = N-2
      GO TO (100,120,140,160,180,200,220,240), NN
C
C     ----- LI -----
C
  100 CONTINUE
      E(1)=0.6177D+00
      S(1)=-0.16287D+00
      P(1)=0.06205D+00
      E(2)=0.1434D+00
      S(2)=0.12643D+00
      P(2)=0.24719D+00
      E(3)=0.05048D+00
      S(3)=0.76179D+00
      P(3)=0.52140D+00
      E(4)=0.01923D+00
      S(4)=0.21800D+00
      P(4)=0.34290D+00
      RETURN
C
C     ----- BE -----
C
  120 CONTINUE
      E(1)=1.447D+00
      S(1)=-0.15647D+00
      P(1)=0.08924D+00
      E(2)=0.3522D+00
      S(2)=0.10919D+00
      P(2)=0.30999D+00
      E(3)=0.1219D+00
      S(3)=0.67538D+00
      P(3)=0.51842D+00
      E(4)=0.04395D+00
      S(4)=0.32987D+00
      P(4)=0.27911D+00
      RETURN
C
C     ----- B  -----
C
  140 CONTINUE
      E(1)=2.710D+00
      S(1)=-0.14987D+00
      P(1)=0.09474D+00
      E(2)=0.6552D+00
      S(2)=0.08442D+00
      P(2)=0.30807D+00
      E(3)=0.2248D+00
      S(3)=0.69751D+00
      P(3)=0.46876D+00
      E(4)=0.07584D+00
      S(4)=0.32842D+00
      P(4)=0.35025D+00
      RETURN
C
C     ----- C  -----
C
  160 CONTINUE
      E(1)=4.286D+00
      S(1)=-0.14722D+00
      P(1)=0.10257D+00
      E(2)=1.046D+00
      S(2)=0.08125D+00
      P(2)=0.32987D+00
      E(3)=0.3447D+00
      S(3)=0.71360D+00
      P(3)=0.48212D+00
      E(4)=0.1128D+00
      S(4)=0.31521D+00
      P(4)=0.31593D+00
      RETURN
C
C     ----- N -----
C
  180 CONTINUE
      E(1)=6.403D+00
      S(1)=-0.13955D+00
      P(1)=0.10336D+00
      E(2)=1.580D+00
      S(2)=0.05492D+00
      P(2)=0.33205D+00
      E(3)=0.5094D+00
      S(3)=0.71678D+00
      P(3)=0.48708D+00
      E(4)=0.1623D+00
      S(4)=0.33210D+00
      P(4)=0.31312D+00
      RETURN
C
C     ----- O  ------
C
  200 CONTINUE
      E(1)=8.519D+00
      S(1)=-0.14551D+00
      P(1)=0.11007D+00
      E(2)=2.073D+00
      S(2)=0.08286D+00
      P(2)=0.34969D+00
      E(3)=0.6471D+00
      S(3)=0.74325D+00
      P(3)=0.48093D+00
      E(4)=0.2000D+00
      S(4)=0.28472D+00
      P(4)=0.30727D+00
      RETURN
C
C     ----- F  -----
C
  220 CONTINUE
      E(1)=11.12D+00
      S(1)=-0.14451D+00
      P(1)=0.11300D+00
      E(2)=2.687D+00
      S(2)=0.08971D+00
      P(2)=0.35841D+00
      E(3)=0.8210D+00
      S(3)=0.75659D+00
      P(3)=0.48002D+00
      E(4)=0.2475D+00
      S(4)=0.26570D+00
      P(4)=0.30381D+00
      RETURN
C
C     ----- NE -----
C
  240 CONTINUE
      E(1)=14.07D+00
      S(1)=-0.14463D+00
      P(1)=0.11514D+00
      E(2)=3.389D+00
      S(2)=0.09331D+00
      P(2)=0.36479D+00
      E(3)=1.021D+00
      S(3)=0.76297D+00
      P(3)=0.48052D+00
      E(4)=0.3031D+00
      S(4)=0.25661D+00
      P(4)=0.29896D+00
      RETURN
      END
C*MODULE BASECP  *DECK SBKTHR
      SUBROUTINE SBKTHR(N,E,S,P)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION E(4),S(4),P(4)
C
C     ----- FROM STEVENS, BASCH, KRAUSS BASES FOR 3RD ROW -----
C
      NN = N - 10
      GO TO (100,120,140,160,180,200,220,240), NN
C
C     ----- NA -----
C
  100 CONTINUE
      E(1)=0.4299D+00
      S(1)=-0.20874D+00
      P(1)=-0.02571D+00
      E(2)=0.08897D+00
      S(2)=0.31206D+00
      P(2)=0.21608D+00
      E(3)=0.03550D+00
      S(3)=0.70300D+00
      P(3)=0.54196D+00
      E(4)=0.01455D+00
      S(4)=0.11648D+00
      P(4)=0.35484D+00
      RETURN
C
C     ----- MG -----
C
  120 CONTINUE
      E(1)=0.6606D+00
      S(1)=-0.24451D+00
      P(1)=-0.04421D+00
      E(2)=0.1845D+00
      S(2)=0.25323D+00
      P(2)=0.27323D+00
      E(3)=0.06983D+00
      S(3)=0.69720D+00
      P(3)=0.57626D+00
      E(4)=0.02740D+00
      S(4)=0.21655D+00
      P(4)=0.28152D+00
      RETURN
C
C     ----- AL -----
C
  140 CONTINUE
      E(1)=0.9011D+00
      S(1)=-0.30377D+00
      P(1)=-0.07929D+00
      E(2)=0.4495D+00
      S(2)=0.13382D+00
      P(2)=0.16540D+00
      E(3)=0.1405D+00
      S(3)=0.76037D+00
      P(3)=0.53015D+00
      E(4)=0.04874D+00
      S(4)=0.32232D+00
      P(4)=0.47724D+00
      RETURN
C
C     ----- SI -----
C
  160 CONTINUE
      E(1)=1.167D+00
      S(1)=-0.32403D+00
      P(1)=-0.08450D+00
      E(2)=0.5268D+00
      S(2)=0.18438D+00
      P(2)=0.23786D+00
      E(3)=0.1807D+00
      S(3)=0.77737D+00
      P(3)=0.56532D+00
      E(4)=0.06480D+00
      S(4)=0.26767D+00
      P(4)=0.37433D+00
      RETURN
C
C     ----- P  -----
C
  180 CONTINUE
      E(1)=1.459D+00
      S(1)=-0.34091D+00
      P(1)=-0.09378D+00
      E(2)=0.6549D+00
      S(2)=0.21535D+00
      P(2)=0.29205D+00
      E(3)=0.2256D+00
      S(3)=0.79578D+00
      P(3)=0.58688D+00
      E(4)=0.08115D+00
      S(4)=0.23092D+00
      P(4)=0.30631D+00
      RETURN
C
C     ----- S  -----
C
  200 CONTINUE
      E(1)=1.817D+00
      S(1)=-0.34015D+00
      P(1)=-0.10096D+00
      E(2)=0.8379D+00
      S(2)=0.19601D+00
      P(2)=0.31244D+00
      E(3)=0.2854D+00
      S(3)=0.82666D+00
      P(3)=0.57906D+00
      E(4)=0.09939D+00
      S(4)=0.21652D+00
      P(4)=0.30748D+00
      RETURN
C
C     ----- CL -----
C
  220 CONTINUE
      E(1)=2.225D+00
      S(1)=-0.33098D+00
      P(1)=-0.12604D+00
      E(2)=1.173D+00
      S(2)=0.11528D+00
      P(2)=0.29952D+00
      E(3)=0.3851D+00
      S(3)=0.84717D+00
      P(3)=0.58357D+00
      E(4)=0.1301D+00
      S(4)=0.26534D+00
      P(4)=0.34097D+00
      RETURN
C
C     ----- AR -----
C
  240 CONTINUE
      E(1)=2.706D+00
      S(1)=-0.31286D+00
      P(1)=-0.10927D+00
      E(2)=1.278D+00
      S(2)=0.11821D+00
      P(2)=0.32601D+00
      E(3)=0.4354D+00
      S(3)=0.86786D+00
      P(3)=0.57952D+00
      E(4)=0.1476D+00
      S(4)=0.22264D+00
      P(4)=0.30349D+00
      RETURN
      END
C*MODULE BASECP  *DECK SBKFOU
      SUBROUTINE SBKFOU(NUCZ,E,S,P,D)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION E(14),S(14),P(14),D(14)
      PARAMETER (ONE=1.0D+00)
C
C         STEVENS, KRAUSS, BASCH, JASIEN EXPONENTS AND CONTRACTION
C         COEFFICIENTS FOR THE MAIN GROUP, 4TH PERIOD.
C
      IJUMP = NUCZ-18
      IF(IJUMP.GT.2) IJUMP=NUCZ-28
      GO TO (100,200,300,400,500,600,700,800), IJUMP
C
C   POTASSIUM
C
  100 CONTINUE
      E(1)=  0.2201D+00
      E(2)=  0.4825D-01
      E(3)=  0.2242D-01
      S(1)= -0.286027D+00
      S(2)=  0.48205D+00
      S(3)=  0.675841D+00
      P(1)= -0.66245D-01
      P(2)=  0.356898D+00
      P(3)=  0.704426D+00
      E(4)=  0.102D-01
      S(4)=  ONE
      P(4)=  ONE
      RETURN
C
C   CALCIUM
C
  200 CONTINUE
      E(1)=  0.2604D+00
      E(2)=  0.1439D+00
      E(3)=  0.4859D-01
      S(1)= -0.676466D+00
      S(2)=  0.525693D+00
      S(3)=  0.960216D+00
      P(1)= -0.270374D+00
      P(2)=  0.407807D+00
      P(3)=  0.828129D+00
      E(4)=  0.2167D-01
      S(4)=  ONE
      P(4)=  ONE
      RETURN
C
C   GALLIUM  ... SEMI-CORE BASIS
C
  300 CONTINUE
      E(1)=  1.139D+02
      E(2)=  9.155D+00
      E(3)=  6.633D+00
      E(4)=  2.278D+00
      S(1)= -1.711D-03
      S(2)= -8.23036D-01
      S(3)=  4.58618D-01
      S(4)=  1.161817D+00
      P(1)= -8.046D-03
      P(2)= -3.57432D-01
      P(3)=  6.63794D-01
      P(4)=  7.13619D-01
      E(5)=  7.043D+01
      E(6)=  2.105D+01
      E(7)=  7.401D+00
      E(8)=  2.752D+00
      D(5)=  2.8877D-02
      D(6)=  1.66253D-01
      D(7)=  4.27776D-01
      D(8)=  5.7041D-01
      E(9)=  7.461D-02
      S(9)=  ONE
      P(9)=  ONE
      E(10)=  2.123D+00
      E(11)=  1.939D-01
      S(10)= -1.45506D-01
      S(11)=  1.051147D+00
      P(10)= -9.6261D-02
      P(11)=  1.017573D+00
      E(12)=  8.818D-01
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  1.026D+00
      D(13)=  ONE
      E(14)=  3.907D-01
      D(14)=  ONE
      RETURN
C
C   GERMANIUM
C
  400 CONTINUE
      E(1)=  0.1834D+01
      E(2)=  0.1529D+01
      E(3)=  0.3594D+00
      E(4)=  0.147D+00
      S(1)=  0.49386D+00
      S(2)= -0.857354D+00
      S(3)=  0.41083D+00
      S(4)=  0.800378D+00
      P(1)=  0.6414D-02
      P(2)= -0.86052D-01
      P(3)=  0.383232D+00
      P(4)=  0.698185D+00
      E(5)=  0.5598D-01
      S(5)=  ONE
      P(5)=  ONE
      RETURN
C
C   ARSENIC
C
  500 CONTINUE
      E(1)=  0.2709D+01
      E(2)=  0.1578D+01
      E(3)=  0.4358D+00
      E(4)=  0.1776D+00
      S(1)=  0.121479D+00
      S(2)= -0.518918D+00
      S(3)=  0.428791D+00
      S(4)=  0.808078D+00
      P(1)= -0.292D-02
      P(2)= -0.95054D-01
      P(3)=  0.424682D+00
      P(4)=  0.67129D+00
      E(5)=  0.6984D-01
      S(5)=  ONE
      P(5)=  ONE
      RETURN
C
C   SELENIUM
C
  600 CONTINUE
      E(1)=  0.3711D+01
      E(2)=  0.1586D+01
      E(3)=  0.5339D+00
      E(4)=  0.2085D+00
      S(1)=  5.5744D-02
      S(2)= -5.10520D-01
      S(3)=  4.80755D-01
      S(4)=  8.10292D-01
      P(1)= -6.014D-03
      P(2)= -1.21447D-01
      P(3)=  4.52607D-01
      P(4)=  6.69751D-01
      E(5)=  7.821D-02
      S(5)=  ONE
      P(5)=  ONE
      RETURN
C
C   BROMINE
C
  700 CONTINUE
      E(1)=  0.3276D+01
      E(2)=  0.2044D+01
      E(3)=  0.6398D+00
      E(4)=  0.2561D+00
      S(1)=  0.20057D+00
      S(2)= -0.649296D+00
      S(3)=  0.405401D+00
      S(4)=  0.872607D+00
      P(1)=  0.5411D-02
      P(2)= -0.132391D+00
      P(3)=  0.430027D+00
      P(4)=  0.686009D+00
      E(5)=  0.9567D-01
      S(5)=  ONE
      P(5)=  ONE
      RETURN
C
C   KRYPTON
C
  800 CONTINUE
      E(1)=  0.3081D+01
      E(2)=  0.2413D+01
      E(3)=  0.7386D+00
      E(4)=  0.2941D+00
      S(1)=  0.533789D+00
      S(2)= -0.1001465D+01
      S(3)=  0.41551D+00
      S(4)=  0.880103D+00
      P(1)=  0.35906D-01
      P(2)= -0.169695D+00
      P(3)=  0.43508D+00
      P(4)=  0.68576D+00
      E(5)=  0.1095D+00
      S(5)=  ONE
      P(5)=  ONE
      RETURN
      END
C*MODULE BASECP  *DECK SBKFIV
      SUBROUTINE SBKFIV(NUCZ,E,S,P,D)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION E(13),S(13),P(13),D(13)
      PARAMETER (ONE=1.0D+00)
C
C         STEVENS, KRAUSS, BASCH, JASIEN EXPONENTS AND CONTRACTION
C         COEFFICIENTS FOR THE MAIN GROUP, 5TH PERIOD.
C
      IJUMP = NUCZ-36
      IF(IJUMP.GT.2) IJUMP=NUCZ-46
      GO TO (100,200,300,400,500,600,700,800), IJUMP
C
C   RUBIDIUM
C
  100 CONTINUE
      E(1)=  0.1543D+00
      E(2)=  0.9892D-01
      E(3)=  0.3227D-01
      S(1)= -0.75339D+00
      S(2)=  0.40627D+00
      S(3)=  0.1112487D+01
      P(1)= -0.108684D+00
      P(2)=  0.55587D-01
      P(3)=  0.1014168D+01
      E(4)=  0.149D-01
      S(4)=  ONE
      P(4)=  ONE
      RETURN
C
C   STRONTIUM
C
  200 CONTINUE
      E(1)=  0.1836D+00
      E(2)=  0.1252D+00
      E(3)=  0.3968D-01
      S(1)= -0.1075139D+01
      S(2)=  0.947037D+00
      S(3)=  0.934226D+00
      P(1)= -0.955549D+00
      P(2)=  0.856447D+00
      P(3)=  0.871558D+00
      E(4)=  0.184D-01
      S(4)=  ONE
      P(4)=  ONE
      RETURN
C
C   INDIUM ... SEMI-CORE BASIS
C
  300 CONTINUE
      E(1)=  7.176D+01
      E(2)=  7.654D+00
      E(3)=  5.616D+00
      E(4)=  2.104D+00
      S(1)=  7.33D-04
      S(2)=  1.089781D+00
      S(3)= -2.731089D+00
      S(4)=  2.112844D+00
      P(1)= -4.513D-03
      P(2)=  3.1615D-02
      P(3)= -3.38006D-01
      P(4)=  1.213464D+00
      E(5)=  1.716D+01
      E(6)=  3.127D+00
      E(7)=  1.475D+00
      D(5)=  1.4893D-02
      D(6)=  3.88135D-01
      D(7)=  6.62639D-01
      E(8)=  8.267D-02
      S(8)=  ONE
      P(8)=  ONE
      E(9)=  2.61D+00
      E(10)=  1.901D-01
      S(9)= -9.969D-02
      S(10)=  1.03123D+00
      P(9)= -1.10317D-01
      P(10)=  1.0139D+00
      E(11)=  8.41D-01
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  6.452D-01
      D(12)=  ONE
      E(13)=  2.754D-01
      D(13)=  ONE
      RETURN
C
C   TIN
C
  400 CONTINUE
      E(1)=  0.2604D+01
      E(2)=  0.7532D+00
      E(3)=  0.3191D+00
      E(4)=  0.1239D+00
      S(1)=  0.31042D-01
      S(2)= -0.611859D+00
      S(3)=  0.548163D+00
      S(4)=  0.84915D+00
      P(1)= -0.5053D-02
      P(2)= -0.1801D+00
      P(3)=  0.370068D+00
      P(4)=  0.781334D+00
      E(5)=  0.4798D-01
      S(5)=  ONE
      P(5)=  ONE
      RETURN
C
C   ANTIMONY
C
  500 CONTINUE
      E(1)=  0.9922D+00
      E(2)=  0.8089D+00
      E(3)=  0.4312D+00
      E(4)=  0.1498D+00
      S(1)=  0.690076D+00
      S(2)= -0.1580495D+01
      S(3)=  0.815248D+00
      S(4)=  0.894994D+00
      P(1)=  0.180229D+00
      P(2)= -0.487093D+00
      P(3)=  0.473936D+00
      P(4)=  0.804414D+00
      E(5)=  0.5803D-01
      S(5)=  ONE
      P(5)=  ONE
      RETURN
C
C   TELLURIUM
C
  600 CONTINUE
      E(1)=  0.2364D+01
      E(2)=  0.9769D+00
      E(3)=  0.4647D+00
      E(4)=  0.1771D+00
      S(1)=  0.87179D-01
      S(2)= -0.776826D+00
      S(3)=  0.56325D+00
      S(4)=  0.926053D+00
      P(1)= -0.3982D-02
      P(2)= -0.2369D+00
      P(3)=  0.401467D+00
      P(4)=  0.793179D+00
      E(5)=  0.6737D-01
      S(5)=  ONE
      P(5)=  ONE
      RETURN
C
C   IODINE
C
  700 CONTINUE
      E(1)=  0.2625D+01
      E(2)=  0.1014D+01
      E(3)=  0.5009D+00
      E(4)=  0.2023D+00
      S(1)=  0.7366D-01
      S(2)= -0.83687D+00
      S(3)=  0.656247D+00
      S(4)=  0.900744D+00
      P(1)= -0.888D-02
      P(2)= -0.257351D+00
      P(3)=  0.455368D+00
      P(4)=  0.760107D+00
      E(5)=  0.78D-01
      S(5)=  ONE
      P(5)=  ONE
      RETURN
C
C   XENON
C
  800 CONTINUE
      E(1)=  0.1739D+01
      E(2)=  0.1169D+01
      E(3)=  0.5765D+00
      E(4)=  0.2218D+00
      S(1)=  0.349091D+00
      S(2)= -0.1197512D+01
      S(3)=  0.758409D+00
      S(4)=  0.888699D+00
      P(1)= -0.5429D-02
      P(2)= -0.277342D+00
      P(3)=  0.467189D+00
      P(4)=  0.763456D+00
      E(5)=  0.8486D-01
      S(5)=  ONE
      P(5)=  ONE
      RETURN
      END
C*MODULE BASECP  *DECK SBKSIX
      SUBROUTINE SBKSIX(NUCZ,E,S,P,D)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION E(13),S(13),P(13),D(13)
      PARAMETER (ONE=1.0D+00)
C
C         STEVENS, KRAUSS, BASCH, JASIEN EXPONENTS AND CONTRACTION
C         COEFFICIENTS FOR THE MAIN GROUP, 6TH PERIOD.
C
      IJUMP = NUCZ-54
      IF(IJUMP.GT.2) IJUMP=NUCZ-78
      GO TO (100,200,300,400,500,600,700,800), IJUMP
C
C   CESIUM
C
  100 CONTINUE
      E(1)=  0.1157D+00
      E(2)=  0.5317D-01
      E(3)=  0.1897D-01
      S(1)= -0.519994D+00
      S(2)=  0.493361D+00
      S(3)=  0.872312D+00
      P(1)= -0.15056D+00
      P(2)=  0.221602D+00
      P(3)=  0.896486D+00
      E(4)=  0.774D-02
      S(4)=  ONE
      P(4)=  ONE
      RETURN
C
C   BARIUM
C
  200 CONTINUE
      E(1)=  0.1266D+00
      E(2)=  0.9433D-01
      E(3)=  0.302D-01
      S(1)= -0.1560287D+01
      S(2)=  0.1482538D+01
      S(3)=  0.878917D+00
      P(1)= -0.686637D+00
      P(2)=  0.807346D+00
      P(3)=  0.822436D+00
      E(4)=  0.1531D-01
      S(4)=  ONE
      P(4)=  ONE
      RETURN
C
C   THALLIUM ... SEMI-CORE BASIS
C
  300 CONTINUE
      E(1)=  2.772D+01
      E(2)=  8.583D+00
      E(3)=  4.65D+00
      E(4)=  1.876D+00
      E(5)=  7.369D-01
      S(1)= -1.5939D-02
      S(2)=  3.07229D-01
      S(3)= -1.124512D+00
      S(4)=  1.114798D+00
      S(5)=  5.31758D-01
      P(1)= -5.281D-03
      P(2)=  7.2455D-02
      P(3)= -3.43413D-01
      P(4)=  7.5962D-01
      P(5)=  4.8955D-01
      E(6)=  4.284D+00
      E(7)=  2.136D+00
      E(8)=  9.832D-01
      D(6)= -7.7432D-02
      D(7)=  4.55402D-01
      D(8)=  6.52892D-01
      E(9)=  1.6D-01
      S(9)=  ONE
      P(9)=  ONE
      E(10)=  6.181D-02
      S(10)=  ONE
      P(10)=  ONE
      E(11)=  1.629D+00
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  4.5D-01
      D(12)=  ONE
      E(13)=  2.04D-01
      D(13)=  ONE
      RETURN
C
C   LEAD
C
  400 CONTINUE
      E(1)=  0.1534D+01
      E(2)=  0.9923D+00
      E(3)=  0.2241D+00
      E(4)=  0.9664D-01
      S(1)=  0.225652D+00
      S(2)= -0.658998D+00
      S(3)=  0.766214D+00
      S(4)=  0.500934D+00
      P(1)=  0.28257D-01
      P(2)= -0.140659D+00
      P(3)=  0.428132D+00
      P(4)=  0.663422D+00
      E(5)=  0.39D-01
      S(5)=  ONE
      P(5)=  ONE
      RETURN
C
C   BISMUTH
C
  500 CONTINUE
      E(1)=  0.1746D+01
      E(2)=  0.9925D+00
      E(3)=  0.2642D+00
      E(4)=  0.1135D+00
      S(1)=  0.162327D+00
      S(2)= -0.653854D+00
      S(3)=  0.80135D+00
      S(4)=  0.516861D+00
      P(1)=  0.16087D-01
      P(2)= -0.158284D+00
      P(3)=  0.460932D+00
      P(4)=  0.64977D+00
      E(5)=  0.4642D-01
      S(5)=  ONE
      P(5)=  ONE
      RETURN
C
C   POLONIUM
C
  600 CONTINUE
      E(1)=  0.1897D+01
      E(2)=  ONE
      E(3)=  0.3107D+00
      E(4)=  0.1273D+00
      S(1)=  0.13269D+00
      S(2)= -0.681546D+00
      S(3)=  0.840179D+00
      S(4)=  0.536942D+00
      P(1)=  0.6796D-02
      P(2)= -0.176142D+00
      P(3)=  0.490092D+00
      P(4)=  0.645759D+00
      E(5)=  0.51D-01
      S(5)=  ONE
      P(5)=  ONE
      RETURN
C
C   ASTATINE
C
  700 CONTINUE
      E(1)=  0.2676D+01
      E(2)=  0.9805D+00
      E(3)=  0.3598D+00
      E(4)=  0.1483D+00
      S(1)=  0.68973D-01
      S(2)= -0.718668D+00
      S(3)=  0.917123D+00
      S(4)=  0.546749D+00
      P(1)= -0.2652D-02
      P(2)= -0.194971D+00
      P(3)=  0.506172D+00
      P(4)=  0.653171D+00
      E(5)=  0.5887D-01
      S(5)=  ONE
      P(5)=  ONE
      RETURN
C
C   RADON
C
  800 CONTINUE
      E(1)=  0.2489D+01
      E(2)=  0.1013D+01
      E(3)=  0.4072D+00
      E(4)=  0.1646D+00
      S(1)=  0.8282D-01
      S(2)= -0.8032D+00
      S(3)=  0.983073D+00
      S(4)=  0.553051D+00
      P(1)=  0.4727D-02
      P(2)= -0.23118D+00
      P(3)=  0.541354D+00
      P(4)=  0.647745D+00
      E(5)=  0.65D-01
      S(5)=  ONE
      P(5)=  ONE
      RETURN
      END
C*MODULE BASECP  *DECK SBKTM1
      SUBROUTINE SBKTM1(NUCZ,E,S,P,D)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION E(14),S(14),P(14),D(14)
      PARAMETER (ONE=1.0D+00)
C
C         STEVENS, KRAUSS, BASCH, JASIEN EXPONENTS AND CONTRACTION
C         COEFFICIENTS FOR THE FIRST TRANSITION SERIES.
C
      IJUMP = NUCZ-20
      GO TO (100,200,300,400,500,600,700,800,900,1000), IJUMP
C
C     SCANDIUM
C
  100 CONTINUE
      E(1)=  4.007D+01
      E(2)=  3.665D+00
      E(3)=  3.047D+00
      E(4)=  9.588D-01
      S(1)= -3.296D-03
      S(2)= -6.55617D-01
      S(3)=  1.03253D-01
      S(4)=  1.320199D+00
      P(1)= -7.753D-03
      P(2)= -6.15592D-01
      P(3)=  7.80261D-01
      P(4)=  8.34079D-01
      E(5)=  2.324D+01
      E(6)=  6.143D+00
      E(7)=  2.007D+00
      E(8)=  6.652D-01
      D(5)=  2.6288D-02
      D(6)=  1.42881D-01
      D(7)=  3.98178D-01
      D(8)=  6.40201D-01
      E(9)=  2.021D-01
      D(9)=  ONE
      E(10)=  1.264D+00
      E(11)=  7.502D-02
      S(10)= -5.7596D-02
      S(11)=  1.016468D+00
      P(10)= -3.1779D-02
      P(11)=  1.004065D+00
      E(12)=  2.741D-02
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  3.557D-01
      S(13)=  ONE
      P(13)=  ONE
      E(14)=  5.454D-02
      D(14)=  ONE
      RETURN
C
C     TITANIUM
C
  200 CONTINUE
      E(1)=  5.108D+01
      E(2)=  4.18D+00
      E(3)=  2.997D+00
      E(4)=  1.019D+00
      S(1)= -2.004D-03
      S(2)= -6.19686D-01
      S(3)=  1.56029D-01
      S(4)=  1.248817D+00
      P(1)= -6.949D-03
      P(2)= -3.26613D-01
      P(3)=  5.44254D-01
      P(4)=  7.89631D-01
      E(5)=  2.811D+01
      E(6)=  7.63D+00
      E(7)=  2.528D+00
      E(8)=  8.543D-01
      D(5)=  2.5746D-02
      D(6)=  1.41260D-01
      D(7)=  4.00607D-01
      D(8)=  6.34532D-01
      E(9)=  2.673D-01
      D(9)=  ONE
      E(10)=  1.383D+00
      E(11)=  8.128D-02
      S(10)= -7.2891D-02
      S(11)=  1.020184D+00
      P(10)= -2.5949D-02
      P(11)=  1.003353D+00
      E(12)=  3.023D-02
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  3.745D-01
      S(13)=  ONE
      P(13)=  ONE
      E(14)=  7.43D-02
      D(14)=  ONE
      RETURN
C
C     VANADIUM
C
  300 CONTINUE
      E(1)=  4.816D+01
      E(2)=  4.685D+00
      E(3)=  3.115D+00
      E(4)=  1.098D+00
      S(1)= -4.277D-03
      S(2)= -5.45554D-01
      S(3)=  1.31799D-01
      S(4)=  1.218022D+00
      P(1)= -8.007D-03
      P(2)= -2.65673D-01
      P(3)=  5.22944D-01
      P(4)=  7.54434D-01
      E(5)=  3.336D+01
      E(6)=  9.331D+00
      E(7)=  3.158D+00
      E(8)=  1.113D+00
      D(5)=  2.553D-02
      D(6)=  1.40336D-01
      D(7)=  3.97933D-01
      D(8)=  6.29027D-01
      E(9)=  3.608D-01
      D(9)=  ONE
      E(10)=  1.565D+00
      E(11)=  9.409D-02
      S(10)= -8.5426D-02
      S(11)=  1.023584D+00
      P(10)= -2.2816D-02
      P(11)=  1.003066D+00
      E(12)=  3.37D-02
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  3.936D-01
      S(13)=  ONE
      P(13)=  ONE
      E(14)=  1.007D-01
      D(14)=  ONE
      RETURN
C
C     CHROMIUM
C
  400 CONTINUE
      E(1)=  2.312D+01
      E(2)=  5.036D+00
      E(3)=  2.867D+00
      E(4)=  1.144D+00
      S(1)= -5.918D-03
      S(2)= -5.9075D-01
      S(3)=  2.9612D-01
      S(4)=  1.107646D+00
      P(1)= -1.8018D-02
      P(2)= -1.44996D-01
      P(3)=  4.97416D-01
      P(4)=  6.72884D-01
      E(5)=  3.789D+01
      E(6)=  1.058D+01
      E(7)=  3.603D+00
      E(8)=  1.27D+00
      D(5)=  2.591D-02
      D(6)=  1.4413D-01
      D(7)=  4.03597D-01
      D(8)=  6.20846D-01
      E(9)=  4.118D-01
      D(9)=  ONE
      E(10)=  1.571D+00
      E(11)=  9.654D-02
      S(10)= -9.0329D-02
      S(11)=  1.025164D+00
      P(10)= -3.7281D-02
      P(11)=  1.004879D+00
      E(12)=  3.492D-02
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  4.529D-01
      S(13)=  ONE
      P(13)=  ONE
      E(14)=  1.126D-01
      D(14)=  ONE
      RETURN
C
C     MANGANESE
C
  500 CONTINUE
      E(1)=  7.217D+01
      E(2)=  5.728D+00
      E(3)=  3.729D+00
      E(4)=  1.321D+00
      S(1)= -2.635D-03
      S(2)= -5.93184D-01
      S(3)=  2.24656D-01
      S(4)=  1.175342D+00
      P(1)= -7.172D-03
      P(2)= -2.45649D-01
      P(3)=  5.32283D-01
      P(4)=  7.30536D-01
      E(5)=  4.263D+01
      E(6)=  1.197D+01
      E(7)=  4.091D+00
      E(8)=  1.45D+00
      D(5)=  2.6095D-02
      D(6)=  1.46772D-01
      D(7)=  4.07115D-01
      D(8)=  6.1459D-01
      E(9)=  4.7D-01
      D(9)=  ONE
      E(10)=  1.827D+00
      E(11)=  1.13D-01
      S(10)= -9.6146D-02
      S(11)=  1.026668D+00
      P(10)= -3.26D-02
      P(11)=  1.004376D+00
      E(12)=  3.89D-02
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  4.754D-01
      S(13)=  ONE
      P(13)=  ONE
      E(14)=  1.281D-01
      D(14)=  ONE
      RETURN
C
C     IRON
C
  600 CONTINUE
      E(1)=  7.028D+01
      E(2)=  6.061D+00
      E(3)=  4.134D+00
      E(4)=  1.421D+00
      S(1)= -2.611D-03
      S(2)= -6.92435D-01
      S(3)=  3.6253D-01
      S(4)=  1.140645D+00
      P(1)= -7.94D-03
      P(2)= -2.90151D-01
      P(3)=  5.91028D-01
      P(4)=  7.19448D-01
      E(5)=  4.71D+01
      E(6)=  1.312D+01
      E(7)=  4.478D+00
      E(8)=  1.581D+00
      D(5)=  2.6608D-02
      D(6)=  1.5201D-01
      D(7)=  4.13827D-01
      D(8)=  6.05542D-01
      E(9)=  5.1D-01
      D(9)=  ONE
      E(10)=  1.978D+00
      E(11)=  1.213D-01
      S(10)= -9.8172D-02
      S(11)=  1.026957D+00
      P(10)= -3.3731D-02
      P(11)=  1.004462D+00
      E(12)=  4.1D-02
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  5.121D-01
      S(13)=  ONE
      P(13)=  ONE
      E(14)=  1.382D-01
      D(14)=  ONE
      RETURN
C
C     COBALT
C
  700 CONTINUE
      E(1)=  7.568D+01
      E(2)=  6.496D+00
      E(3)=  4.791D+00
      E(4)=  1.594D+00
      S(1)= -5.271D-03
      S(2)= -7.05525D-01
      S(3)=  3.4788D-01
      S(4)=  1.175595D+00
      P(1)= -8.085D-03
      P(2)= -3.90988D-01
      P(3)=  6.80078D-01
      P(4)=  7.29594D-01
      E(5)=  5.169D+01
      E(6)=  1.47D+01
      E(7)=  4.851D+00
      E(8)=  1.643D+00
      D(5)=  2.5447D-02
      D(6)=  1.49529D-01
      D(7)=  4.25056D-01
      D(8)=  6.05465D-01
      E(9)=  5.075D-01
      D(9)=  ONE
      E(10)=  2.337D+00
      E(11)=  1.269D-01
      S(10)= -8.9267D-02
      S(11)=  1.022589D+00
      P(10)= -1.5935D-02
      P(11)=  1.001945D+00
      E(12)=  4.232D-02
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  5.572D-01
      S(13)=  ONE
      P(13)=  ONE
      E(14)=  1.433D-01
      D(14)=  ONE
      RETURN
C
C     NICKEL
C
  800 CONTINUE
      E(1)=  8.936D+01
      E(2)=  7.265D+00
      E(3)=  5.572D+00
      E(4)=  1.85D+00
      S(1)= -5.22D-03
      S(2)= -6.83993D-01
      S(3)=  2.5105D-01
      S(4)=  1.236451D+00
      P(1)= -7.529D-03
      P(2)= -4.45862D-01
      P(3)=  7.06364D-01
      P(4)=  7.53606D-01
      E(5)=  5.873D+01
      E(6)=  1.671D+01
      E(7)=  5.783D+00
      E(8)=  2.064D+00
      D(5)=  2.6246D-02
      D(6)=  1.50334D-01
      D(7)=  4.13385D-01
      D(8)=  6.04114D-01
      E(9)=  6.752D-01
      D(9)=  ONE
      E(10)=  3.235D+00
      E(11)=  1.295D-01
      S(10)= -6.3418D-02
      S(11)=  1.013237D+00
      P(10)=  1.136D-03
      P(11)=  9.99895D-01
      E(12)=  4.327D-02
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  6.594D-01
      S(13)=  ONE
      P(13)=  ONE
      E(14)=  1.825D-01
      D(14)=  ONE
      RETURN
C
C     COPPER
C
  900 CONTINUE
      E(1)=  8.342D+01
      E(2)=  7.97D+00
      E(3)=  5.6D+00
      E(4)=  1.932D+00
      S(1)= -4.829D-03
      S(2)= -6.44799D-01
      S(3)=  2.6524D-01
      S(4)=  1.189791D+00
      P(1)= -8.284D-03
      P(2)= -3.21895D-01
      P(3)=  6.18133D-01
      P(4)=  7.22184D-01
      E(5)=  6.58D+01
      E(6)=  1.882D+01
      E(7)=  6.538D+00
      E(8)=  2.348D+00
      D(5)=  2.5597D-02
      D(6)=  1.48609D-01
      D(7)=  4.11786D-01
      D(8)=  6.05507D-01
      E(9)=  7.691D-01
      D(9)=  ONE
      E(10)=  2.866D+00
      E(11)=  1.319D-01
      S(10)= -7.4774D-02
      S(11)=  1.017037D+00
      P(10)= -5.41D-04
      P(11)=  1.000058D+00
      E(12)=  4.4D-02
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  6.874D-01
      S(13)=  ONE
      P(13)=  ONE
      E(14)=  2.065D-01
      D(14)=  ONE
      RETURN
C
C     ZINC
C
 1000 CONTINUE
      E(1)=  1.135D+02
      E(2)=  8.308D+00
      E(3)=  6.332D+00
      E(4)=  2.146D+00
      S(1)= -4.28D-03
      S(2)= -8.20232D-01
      S(3)=  4.25006D-01
      S(4)=  1.198077D+00
      P(1)= -7.429D-03
      P(2)= -4.32605D-01
      P(3)=  7.23451D-01
      P(4)=  7.27217D-01
      E(5)=  6.599D+01
      E(6)=  1.981D+01
      E(7)=  6.945D+00
      E(8)=  2.543D+00
      D(5)=  2.7653D-02
      D(6)=  1.58794D-01
      D(7)=  4.20971D-01
      D(8)=  5.85277D-01
      E(9)=  9.165D-01
      D(9)=  ONE
      E(10)=  2.906D+00
      E(11)=  1.623D-01
      S(10)= -8.2356D-02
      S(11)=  1.021574D+00
      P(10)= -2.3001D-02
      P(11)=  1.002824D+00
      E(12)=  5.369D-02
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  8.116D-01
      S(13)=  ONE
      P(13)=  ONE
      E(14)=  3.264D-01
      D(14)=  ONE
      RETURN
      END
C*MODULE BASECP  *DECK SBKTM2
      SUBROUTINE SBKTM2(NUCZ,E,S,P,D)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION E(13),S(13),P(13),D(13)
      PARAMETER (ONE=1.0D+00)
C
C         STEVENS, KRAUSS, BASCH, JASIEN EXPONENTS AND CONTRACTION
C         COEFFICIENTS FOR THE SECOND TRANSITION SERIES.
C
      IJUMP = NUCZ-38
      GO TO (100,200,300,400,500,600,700,800,900,1000), IJUMP
C
C     YTTRIUM
C
  100 CONTINUE
      E(1)=  2.984D+01
      E(2)=  3.242D+00
      E(3)=  2.694D+00
      E(4)=  8.43D-01
      S(1)= -6.294D-03
      S(2)=  1.436231D+00
      S(3)= -2.550213D+00
      S(4)=  1.756065D+00
      P(1)= -3.457D-03
      P(2)= -9.7386D-02
      P(3)= -9.8933D-02
      P(4)=  1.11219D+00
      E(5)=  5.399D+00
      E(6)=  1.066D+00
      E(7)=  3.892D-01
      D(5)=  1.202D-02
      D(6)=  3.4295D-01
      D(7)=  7.39397D-01
      E(8)=  1.3D-01
      D(8)=  ONE
      E(9)=  1.324D+00
      E(10)=  6.806D-02
      S(9)= -5.7976D-02
      S(10)=  1.014874D+00
      P(9)= -2.5891D-02
      P(10)=  1.002833D+00
      E(11)=  2.729D-02
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  3.015D-01
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  4.121D-02
      D(13)=  ONE
      RETURN
C
C     ZIRCONIUM
C
  200 CONTINUE
      E(1)=  3.053D+01
      E(2)=  3.518D+00
      E(3)=  2.971D+00
      E(4)=  9.587D-01
      S(1)= -4.504D-03
      S(2)=  1.708899D+00
      S(3)= -2.918518D+00
      S(4)=  1.822958D+00
      P(1)= -4.092D-03
      P(2)= -7.8665D-02
      P(3)= -1.40588D-01
      P(4)=  1.130526D+00
      E(5)=  6.191D+00
      E(6)=  1.32D+00
      E(7)=  5.359D-01
      D(5)=  1.2D-02
      D(6)=  3.25005D-01
      D(7)=  7.40299D-01
      E(8)=  1.9D-01
      D(8)=  ONE
      E(9)=  9.293D-01
      E(10)=  6.91D-02
      S(9)= -6.1958D-02
      S(10)=  1.020739D+00
      P(9)= -6.0217D-02
      P(10)=  1.009302D+00
      E(11)=  2.759D-02
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  3.764D-01
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  5.9D-02
      D(13)=  ONE
      RETURN
C
C     NIOBIUM
C
  300 CONTINUE
      E(1)=  3.853D+01
      E(2)=  3.907D+00
      E(3)=  3.274D+00
      E(4)=  1.052D+00
      S(1)= -2.059D-03
      S(2)=  1.618867D+00
      S(3)= -2.83086D+00
      S(4)=  1.818465D+00
      P(1)= -3.576D-03
      P(2)= -3.3217D-02
      P(3)= -1.85568D-01
      P(4)=  1.133471D+00
      E(5)=  7.994D+00
      E(6)=  1.55D+00
      E(7)=  6.666D-01
      D(5)=  1.0433D-02
      D(6)=  3.18071D-01
      D(7)=  7.39654D-01
      E(8)=  2.462D-01
      D(8)=  ONE
      E(9)=  1.057D+00
      E(10)=  7.876D-02
      S(9)= -7.1919D-02
      S(10)=  1.023797D+00
      P(9)= -6.7815D-02
      P(10)=  1.010254D+00
      E(11)=  3.091D-02
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  4.165D-01
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  7.86D-02
      D(13)=  ONE
      RETURN
C
C     MOLYBDENUM
C
  400 CONTINUE
      E(1)=  3.967D+01
      E(2)=  4.633D+00
      E(3)=  3.208D+00
      E(4)=  1.224D+00
      S(1)= -9.716D-03
      S(2)=  7.70126D-01
      S(3)= -2.22953D+00
      S(4)=  2.046495D+00
      P(1)= -4.015D-03
      P(2)= -3.4146D-02
      P(3)= -2.51817D-01
      P(4)=  1.192347D+00
      E(5)=  9.051D+00
      E(6)=  1.777D+00
      E(7)=  7.848D-01
      D(5)=  1.0883D-02
      D(6)=  3.11829D-01
      D(7)=  7.41802D-01
      E(8)=  2.899D-01
      D(8)=  ONE
      E(9)=  1.181D+00
      E(10)=  8.489D-02
      S(9)= -7.5222D-02
      S(10)=  1.024137D+00
      P(9)= -7.1971D-02
      P(10)=  1.010227D+00
      E(11)=  3.276D-02
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  4.728D-01
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  9.107D-02
      D(13)=  ONE
      RETURN
C
C     TECHNETIUM
C
  500 CONTINUE
      E(1)=  4.887D+01
      E(2)=  4.77D+00
      E(3)=  3.74D+00
      E(4)=  1.305D+00
      S(1)= -6.485D-03
      S(2)=  1.33093D+00
      S(3)= -2.698832D+00
      S(4)=  1.956263D+00
      P(1)= -3.492D-03
      P(2)=  3.7514D-02
      P(3)= -2.97801D-01
      P(4)=  1.1749D+00
      E(5)=  9.451D+00
      E(6)=  1.887D+00
      E(7)=  8.506D-01
      D(5)=  1.2195D-02
      D(6)=  3.4695D-01
      D(7)=  7.07123D-01
      E(8)=  3.258D-01
      D(8)=  ONE
      E(9)=  1.744D+00
      E(10)=  1.206D-01
      S(9)= -9.4275D-02
      S(10)=  1.028604D+00
      P(9)= -8.5401D-02
      P(10)=  1.010946D+00
      E(11)=  4.31D-02
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  4.94D-01
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  1.031D-01
      D(13)=  ONE
      RETURN
C
C     RUTHENIUM
C
  600 CONTINUE
      E(1)=  5.6D+01
      E(2)=  4.625D+00
      E(3)=  3.793D+00
      E(4)=  1.367D+00
      S(1)= -2.837D-03
      S(2)=  1.891661D+00
      S(3)= -3.431011D+00
      S(4)=  2.074446D+00
      P(1)= -3.198D-03
      P(2)= -1.03766D-01
      P(3)= -1.27594D-01
      P(4)=  1.146533D+00
      E(5)=  1.03D+01
      E(6)=  2.044D+00
      E(7)=  8.988D-01
      D(5)=  1.1961D-02
      D(6)=  3.64575D-01
      D(7)=  6.94316D-01
      E(8)=  3.443D-01
      D(8)=  ONE
      E(9)=  1.528D+00
      E(10)=  9.673D-02
      S(9)= -6.9035D-02
      S(10)=  1.020343D+00
      P(9)= -7.299D-02
      P(10)=  1.008642D+00
      E(11)=  3.636D-02
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  5.183D-01
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  1.108D-01
      D(13)=  ONE
      RETURN
C
C     RHODIUM
C
  700 CONTINUE
      E(1)=  5.687D+01
      E(2)=  5.945D+00
      E(3)=  4.078D+00
      E(4)=  1.547D+00
      S(1)= -3.087D-03
      S(2)=  7.0051D-01
      S(3)= -2.160867D+00
      S(4)=  2.029434D+00
      P(1)= -3.748D-03
      P(2)= -1.4772D-02
      P(3)= -2.68975D-01
      P(4)=  1.193604D+00
      E(5)=  1.156D+01
      E(6)=  2.24D+00
      E(7)=  1.015D+00
      D(5)=  1.3519D-02
      D(6)=  3.70081D-01
      D(7)=  6.84936D-01
      E(8)=  3.96D-01
      D(8)=  ONE
      E(9)=  1.894D+00
      E(10)=  1.179D-01
      S(9)= -8.17D-02
      S(10)=  1.023308D+00
      P(9)= -7.1093D-02
      P(10)=  1.00828D+00
      E(11)=  4.219D-02
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  5.963D-01
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  1.262D-01
      D(13)=  ONE
      RETURN
C
C     PALLADIUM
C
  800 CONTINUE
      E(1)=  4.182D+01
      E(2)=  5.46D+00
      E(3)=  4.6D+00
      E(4)=  1.752D+00
      S(1)= -1.6522D-02
      S(2)=  2.749478D+00
      S(3)= -4.457343D+00
      S(4)=  2.253933D+00
      P(1)= -5.555D-03
      P(2)=  5.6773D-02
      P(3)= -3.88641D-01
      P(4)=  1.231997D+00
      E(5)=  1.24D+01
      E(6)=  2.404D+00
      E(7)=  1.143D+00
      D(5)=  1.3759D-02
      D(6)=  3.94121D-01
      D(7)=  6.55946D-01
      E(8)=  4.693D-01
      D(8)=  ONE
      E(9)=  2.177D+00
      E(10)=  1.149D-01
      S(9)= -7.5029D-02
      S(10)=  1.019049D+00
      P(9)= -5.6178D-02
      P(10)=  1.005515D+00
      E(11)=  4.219D-02
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  6.691D-01
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  1.513D-01
      D(13)=  ONE
      RETURN
C
C     SILVER
C
  900 CONTINUE
      E(1)=  6.356D+01
      E(2)=  6.39D+00
      E(3)=  5.022D+00
      E(4)=  1.789D+00
      S(1)= -7.31D-04
      S(2)=  1.392736D+00
      S(3)= -2.873102D+00
      S(4)=  2.017361D+00
      P(1)= -4.234D-03
      P(2)=  6.1276D-02
      P(3)= -3.38746D-01
      P(4)=  1.190832D+00
      E(5)=  1.464D+01
      E(6)=  2.693D+00
      E(7)=  1.233D+00
      D(5)=  1.3628D-02
      D(6)=  3.72695D-01
      D(7)=  6.81294D-01
      E(8)=  5.057D-01
      D(8)=  ONE
      E(9)=  2.451D+00
      E(10)=  1.561D-01
      S(9)= -7.2539D-02
      S(10)=  1.021358D+00
      P(9)= -4.983D-02
      P(10)=  1.006516D+00
      E(11)=  5.227D-02
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  6.871D-01
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  1.9D-01
      D(13)=  ONE
      RETURN
C
C     CADMIUM
C
 1000 CONTINUE
      E(1)=  6.338D+01
      E(2)=  6.714D+00
      E(3)=  5.602D+00
      E(4)=  1.971D+00
      S(1)= -4.64D-04
      S(2)=  2.292877D+00
      S(3)= -3.902884D+00
      S(4)=  2.084604D+00
      P(1)= -4.874D-03
      P(2)=  1.8963D-01
      P(3)= -4.91872D-01
      P(4)=  1.213501D+00
      E(5)=  1.551D+01
      E(6)=  2.941D+00
      E(7)=  1.379D+00
      D(5)=  1.2065D-02
      D(6)=  3.70168D-01
      D(7)=  6.8076D-01
      E(8)=  5.782D-01
      D(8)=  ONE
      E(9)=  2.696D+00
      E(10)=  1.758D-01
      S(9)= -6.4866D-02
      S(10)=  1.019661D+00
      P(9)= -5.3338D-02
      P(10)=  1.007102D+00
      E(11)=  5.647D-02
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  7.689D-01
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  2.3D-01
      D(13)=  ONE
      RETURN
      END
C*MODULE BASECP  *DECK SBKTM3
      SUBROUTINE SBKTM3(NUCZ,E,S,P,D)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION E(14),S(14),P(14),D(14)
C
C      LOGICAL GOPARR,DSKWRK,MASWRK
C
C      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (ONE=1.0D+00)
C
C         STEVENS, KRAUSS, BASCH, JASIEN EXPONENTS AND CONTRACTION
C         COEFFICIENTS FOR THE THIRD TRANSITION SERIES.
C         NOTE THAT THE LANTHANIDES, EXCEPT LANTHANUM, ARE SET IN
C         SBKLAN.
C
      IJUMP = NUCZ-70
      IF(NUCZ.EQ.57) IJUMP=1
      GO TO (100,200,300,400,500,600,700,800,900,1000), IJUMP
C
C     LANTHANUM
C
  100 CONTINUE
      E(1)=  9.173D+00
      E(2)=  3.12D+00
      E(3)=  2.104D+00
      E(4)=  1.32D+00
      E(5)=  4.96D-01
      S(1)= -5.4833D-02
      S(2)=  6.76604D-01
      S(3)= -1.034429D+00
      S(4)= -5.18907D-01
      S(5)=  1.631603D+00
      P(1)= -9.798D-03
      P(2)=  2.31262D-01
      P(3)= -6.01215D-01
      P(4)=  1.95189D-01
      P(5)=  1.076137D+00
      E(6)=  1.238D+00
      E(7)=  6.061D-01
      E(8)=  2.518D-01
      D(6)= -5.3797D-02
      D(7)=  3.80144D-01
      D(8)=  7.20349D-01
      E(9)=  9.787D-02
      D(9)=  ONE
      E(10)=  6.182D-01
      E(11)=  4.546D-02
      S(10)= -1.07095D-01
      S(11)=  1.033448D+00
      P(10)= -5.1869D-02
      P(11)=  1.008108D+00
      E(12)=  1.775D-02
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  2.004D-01
      S(13)=  ONE
      P(13)=  ONE
      E(14)=  3.536D-02
      D(14)=  ONE
      RETURN
C
C     HAFNIUM
C
  200 CONTINUE
      E(1)=  4.711D+00
      E(2)=  3.331D+00
      E(3)=  9.844D-01
      E(4)=  3.587D-01
      S(1)=  4.03273D-01
      S(2)= -9.9654D-01
      S(3)=  9.79338D-01
      S(4)=  4.45791D-01
      P(1)=  1.3791D-02
      P(2)= -1.79991D-01
      P(3)=  6.93601D-01
      P(4)=  4.61877D-01
      E(5)=  3.298D+00
      E(6)=  9.286D-01
      E(7)=  3.405D-01
      D(5)= -5.062D-03
      D(6)=  4.27636D-01
      D(7)=  6.67943D-01
      E(8)=  1.23D-01
      D(8)=  ONE
      E(9)=  8.568D-02
      S(9)=  ONE
      P(9)=  ONE
      E(10)=  3.5D-02
      S(10)=  ONE
      P(10)=  ONE
      E(11)=  6.448D-01
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  4.268D-02
      D(12)=  ONE
      RETURN
C
C     TANTALUM
C
  300 CONTINUE
      E(1)=  5.554D+00
      E(2)=  3.436D+00
      E(3)=  1.033D+00
      E(4)=  3.747D-01
      S(1)=  2.64429D-01
      S(2)= -8.65843D-01
      S(3)=  1.008583D+00
      S(4)=  4.20826D-01
      P(1)=  1.6092D-02
      P(2)= -1.82862D-01
      P(3)=  7.22442D-01
      P(4)=  4.36168D-01
      E(5)=  3.438D+00
      E(6)=  1.016D+00
      E(7)=  3.733D-01
      D(5)= -1.0797D-02
      D(6)=  4.39474D-01
      D(7)=  6.58929D-01
      E(8)=  1.351D-01
      D(8)=  ONE
      E(9)=  9.34D-02
      S(9)=  ONE
      P(9)=  ONE
      E(10)=  3.684D-02
      S(10)=  ONE
      P(10)=  ONE
      E(11)=  7.315D-01
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  4.807D-02
      D(12)=  ONE
      RETURN
C
C     TUNGSTEN
C
  400 CONTINUE
      E(1)=  4.328D+00
      E(2)=  3.898D+00
      E(3)=  1.108D+00
      E(4)=  4.047D-01
      S(1)=  1.97396D+00
      S(2)= -2.605377D+00
      S(3)=  1.05292D+00
      S(4)=  4.12613D-01
      P(1)=  1.72752D-01
      P(2)= -3.48372D-01
      P(3)=  7.27425D-01
      P(4)=  4.34635D-01
      E(5)=  3.653D+00
      E(6)=  1.112D+00
      E(7)=  4.148D-01
      D(5)= -1.6297D-02
      D(6)=  4.50108D-01
      D(7)=  6.48857D-01
      E(8)=  1.495D-01
      D(8)=  ONE
      E(9)=  9.776D-02
      S(9)=  ONE
      P(9)=  ONE
      E(10)=  3.75D-02
      S(10)=  ONE
      P(10)=  ONE
      E(11)=  8.219D-01
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  5.255D-02
      D(12)=  ONE
      RETURN
C
C     RHENIUM
C
  500 CONTINUE
      E(1)=  4.666D+00
      E(2)=  4.062D+00
      E(3)=  1.188D+00
      E(4)=  4.339D-01
      S(1)=  1.451709D+00
      S(2)= -2.101771D+00
      S(3)=  1.062295D+00
      S(4)=  4.16694D-01
      P(1)=  1.56457D-01
      P(2)= -3.429D-01
      P(3)=  7.38595D-01
      P(4)=  4.32092D-01
      E(5)=  3.846D+00
      E(6)=  1.216D+00
      E(7)=  4.585D-01
      D(5)= -2.136D-02
      D(6)=  4.56284D-01
      D(7)=  6.43606D-01
      E(8)=  1.659D-01
      D(8)=  ONE
      E(9)=  1.048D-01
      S(9)=  ONE
      P(9)=  ONE
      E(10)=  3.981D-02
      S(10)=  ONE
      P(10)=  ONE
      E(11)=  9.127D-01
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  5.836D-02
      D(12)=  ONE
      RETURN
C
C     OSMIUM
C
  600 CONTINUE
      E(1)=  5.019D+00
      E(2)=  4.444D+00
      E(3)=  1.23D+00
      E(4)=  4.501D-01
      S(1)=  1.726729D+00
      S(2)= -2.364185D+00
      S(3)=  1.070406D+00
      S(4)=  3.91956D-01
      P(1)=  2.83956D-01
      P(2)= -4.62163D-01
      P(3)=  7.63251D-01
      P(4)=  4.03944D-01
      E(5)=  3.829D+00
      E(6)=  1.339D+00
      E(7)=  5.165D-01
      D(5)= -2.8163D-02
      D(6)=  4.57633D-01
      D(7)=  6.42551D-01
      E(8)=  1.88D-01
      D(8)=  ONE
      E(9)=  1.104D-01
      S(9)=  ONE
      P(9)=  ONE
      E(10)=  4.101D-02
      S(10)=  ONE
      P(10)=  ONE
      E(11)=  1.002D+00
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  6.552D-02
      D(12)=  ONE
      RETURN
C
C     IRIDIUM
C
  700 CONTINUE
      E(1)=  5.522D+00
      E(2)=  4.579D+00
      E(3)=  1.289D+00
      E(4)=  4.734D-01
      S(1)=  1.052849D+00
      S(2)= -1.697001D+00
      S(3)=  1.087609D+00
      S(4)=  3.7723D-01
      P(1)=  1.43988D-01
      P(2)= -3.25563D-01
      P(3)=  7.76442D-01
      P(4)=  3.89328D-01
      E(5)=  4.393D+00
      E(6)=  1.403D+00
      E(7)=  5.593D-01
      D(5)= -2.5391D-02
      D(6)=  4.75718D-01
      D(7)=  6.18182D-01
      E(8)=  2.129D-01
      D(8)=  ONE
      E(9)=  1.159D-01
      S(9)=  ONE
      P(9)=  ONE
      E(10)=  4.276D-02
      S(10)=  ONE
      P(10)=  ONE
      E(11)=  1.116D+00
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  7.634D-02
      D(12)=  ONE
      RETURN
C
C     PLATINUM
C
  800 CONTINUE
      E(1)=  6.653D+00
      E(2)=  3.995D+00
      E(3)=  1.541D+00
      E(4)=  5.599D-01
      S(1)=  2.82256D-01
      S(2)= -1.075967D+00
      S(3)=  1.131255D+00
      S(4)=  4.78241D-01
      P(1)=  3.6575D-02
      P(2)= -2.84606D-01
      P(3)=  7.52346D-01
      P(4)=  4.76585D-01
      E(5)=  4.658D+00
      E(6)=  1.487D+00
      E(7)=  6.093D-01
      D(5)= -2.6327D-02
      D(6)=  4.95995D-01
      D(7)=  5.94877D-01
      E(8)=  2.527D-01
      D(8)=  ONE
      E(9)=  1.277D-01
      S(9)=  ONE
      P(9)=  ONE
      E(10)=  4.754D-02
      S(10)=  ONE
      P(10)=  ONE
      E(11)=  1.2D+00
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  9.508D-02
      D(12)=  ONE
      RETURN
C
C     GOLD
C
  900 CONTINUE
      E(1)=  7.419D+00
      E(2)=  4.023D+00
      E(3)=  1.698D+00
      E(4)=  6.271D-01
      S(1)=  2.22546D-01
      S(2)= -1.086045D+00
      S(3)=  1.156039D+00
      S(4)=  5.18061D-01
      P(1)=  1.9924D-02
      P(2)= -2.99997D-01
      P(3)=  7.48919D-01
      P(4)=  5.04023D-01
      E(5)=  3.63D+00
      E(6)=  1.912D+00
      E(7)=  8.423D-01
      D(5)= -8.7402D-02
      D(6)=  4.68634D-01
      D(7)=  6.54805D-01
      E(8)=  3.756D-01
      D(8)=  ONE
      E(9)=  1.515D-01
      S(9)=  ONE
      P(9)=  ONE
      E(10)=  4.925D-02
      S(10)=  ONE
      P(10)=  ONE
      E(11)=  1.502D+00
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  1.544D-01
      D(12)=  ONE
      RETURN
C
C     MERCURY
C
 1000 CONTINUE
      E(1)=  2.554D+01
      E(2)=  8.458D+00
      E(3)=  4.493D+00
      E(4)=  1.751D+00
      E(5)=  6.753D-01
      S(1)= -2.2041D-02
      S(2)=  3.09845D-01
      S(3)= -1.080984D+00
      S(4)=  1.0936D+00
      S(5)=  5.19202D-01
      P(1)= -6.068D-03
      P(2)=  6.3D-02
      P(3)= -3.14502D-01
      P(4)=  7.46398D-01
      P(5)=  4.87253D-01
      E(6)=  4.204D+00
      E(7)=  1.871D+00
      E(8)=  8.215D-01
      D(6)= -5.5849D-02
      D(7)=  4.78221D-01
      D(8)=  6.22006D-01
      E(9)=  3.7D-01
      D(9)=  ONE
      E(10)=  1.52D-01
      S(10)=  ONE
      P(10)=  ONE
      E(11)=  4.78D-02
      S(11)=  ONE
      P(11)=  ONE
      E(12)=  1.586D+00
      S(12)=  ONE
      P(12)=  ONE
      E(13)=  1.674D-01
      D(13)=  ONE
      RETURN
      END
