C 25 JUN 01 - JMS - S3SP: INSERT RETURN AFTER STO-1G PART
C 19 NOV 00 - MWS - STONG: RETURN NORMALIZED CXINP COEFS
C 29 SEP 96 - MWS - S5SP: CORRECT THE N=6 5SP EXPANSION
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C 12 MAR 92 - MWS - REDIMENSION TO 500 ATOMS
C  4 JAN 92 - TLW - MAKE WRITES PARALLEL;ADD COMMON PAR
C  5 OCT 90 - JAB - STOSF: 5TH ROW TRNSTN METLS USE TRM5, NOT TRM4
C                   STONG: LOOSEN NORMALIZATION TEST (5DP SHELLS)
C  7 AUG 90 - TLW - ADD CF AND CG TO COMMON NSHEL
C 10 MAY 90 - MWS - ADD STONG,STOSHL TO PROVIDE EASY ACCESS TO STO-NG
C  7 JUL 86 - MWS - SANITIZE FLOATING POINT CONSTANTS
C 13 OCT 85 - STE - STOSF: INITIALIZE ZERO
C 14 JUL 85 - MWS - FIX O,NE SCALE FACTORS
C 16 APR 85 - MWS - ADD ROUTINES STOSF,S3SPD,S4SPD,S5SP
C  3 APR 84 - STE - DELETE CORRESPONDING ORBITAL BASIS ROUTINES
C 23 JUN 83 - MWS - BREAK UP BASIS SET SECTION-- STO-NG BASIS SETS
C*MODULE BASSTO  *DECK STONG
      SUBROUTINE STONG(NUCZ,IGAUSS,CSINP,CPINP,CDINP,IERR1,IERR2,
     *                 INTYP,NANGM,NBFS,MINF,MAXF,LOC,NGAUSS,NS)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      DIMENSION CSINP(*),CPINP(*),CDINP(*),
     *          INTYP(*),NANGM(*),NBFS(*),MINF(*),MAXF(*),NS(*)
      DIMENSION EXX(6),CSS(6),CPP(6),CDD(6),TTABLE(7),SHTYPS(7)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      SAVE IBR
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
      DATA ZERO,ONE,PT5,PT75/0.0D+00,1.0D+00,0.5D+00,0.75D+00/
      DATA PI32,TOLNRM,TM10/5.56832799683170D+00,5.0D-06,1.0D-10/
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA TTABLE/8H1S      ,8H2SP     ,8H3SP     ,8H3D      ,
     *            8H4SP     ,8H4D      ,8H5SP     /
#else
      CHARACTER*8 :: TTABLE_STR(7)
      EQUIVALENCE (TTABLE, TTABLE_STR)
      DATA TTABLE_STR/"1S      ","2SP     ","3SP     ","3D      ",
     *            "4SP     ","4D      ","5SP     "/
#endif
C
C     ----- GET STO-NG EXPONENTS AND CONTRACTION COEFFICIENTS -----
C
C     H-NE  N=2,3,4,5,6  W.J.HEHRE, R.F.STEWART, J.A.POPLE
C                        J.CHEM.PHYS. 1969, 51, 2657-2664.
C     NA-AR N=2,3,4,5,6  W.J.HEHRE, R.F.STEWART, J.A.POPLE
C                        J.CHEM.PHYS. 1970, 52, 2769-2773.
C     (VALENCE SCALE FACTORS FOR NA-CL ARE TAKEN FROM THE INDO
C      MBS PARAMETERIZATION DUE TO M.S.GORDON, M.D.BJORKE,
C      F.J.MARSH, M.S.KORTH   J.AM.CHEM.SOC. 1978, 100, 2670-XXXX)
C     K,CA,GA-KR  N=2,3,4,5,6  W.J.PIETRO, B.A.LEVI, W.J.HEHRE,
C                        R.F.STEWART  INORG.CHEM. 1980, 19, 2225-2229.
C     RB,SR,IN-XE N=2,3,4,5,6  W.J.PIETRO, E.S.BLUROCK, W.J.HEHRE,
C                        D.J.DEFREES, R.F.STEWART
C                        INORG. CHEM. 1981, 20, 3650-3654.
C     SC-ZN, Y-CD   W.J.PIETRO, W.J.HEHRE, J.COMP.CHEM. 4,241-251(1983)
C
C     3SPD AND 4SPD SHELLS ARE IMPLEMENTED IN THE BASIS SECTION, BUT
C     NOT IN THE INTEGRAL SECTION, SO THESE ABORT.
C
      IF(IGAUSS.LT.1  .OR.IGAUSS.GT.6) THEN
         IF (MASWRK) WRITE(IW,*)
     *       'STO-NG EXISTS ONLY FOR N=1,2,3,4,5,6.'
         CALL ABRT
      END IF
      IF(NUCZ .GT. 54) THEN
         IF (MASWRK) WRITE(IW,*)
     *      'THERE ARE CANNED STO-NG BASES ONLY FOR Z.LE.54'
         CALL ABRT
      END IF
      CALL STOSHL(NUCZ,MXSHLS,SHTYPS)
C
C     ----- BEGIN BUILDING EACH SHELL ON THIS ATOM -----
C
      NSHLS = 0
  100 CONTINUE
      NSHLS = NSHLS + 1
      IF(NSHLS.GT.MXSHLS) RETURN
C                         ******
C
      DO 110 I = 1,IGAUSS
         EXX(I) = ZERO
         CSS(I) = ZERO
         CPP(I) = ZERO
         CDD(I) = ZERO
  110 CONTINUE
C
      SHTYP = SHTYPS(NSHLS)
      DO 120 I=1,7
         IBR = I
         IF(SHTYP.EQ.TTABLE(I)) GO TO 130
  120 CONTINUE
      CALL ABRT
      STOP
  130 CONTINUE
      GO TO (210,220,230,240,250,260,270), IBR
C
  210 CONTINUE
      ITYP = 1
      CALL S1S(EXX,CSS,IGAUSS)
      STOSC = STOSF(NUCZ,1,0)
      GO TO 300
C
  220 CONTINUE
      ITYP = 6
      CALL S2SP(EXX,CSS,CPP,IGAUSS)
      STOSC = STOSF(NUCZ,2,0)
      GO TO 300
C
  230 CONTINUE
      ITYP = 6
      CALL S3SP(EXX,CSS,CPP,IGAUSS)
      STOSC = STOSF(NUCZ,3,0)
      GO TO 300
C
  240 CONTINUE
      ITYP = 3
      CALL S3D(EXX,CDD,IGAUSS)
      STOSC = STOSF(NUCZ,3,1)
      GO TO 300
C
  250 CONTINUE
      ITYP = 6
      CALL S4SP(EXX,CSS,CPP,IGAUSS)
      STOSC = STOSF(NUCZ,4,0)
      GO TO 300
C
  260 CONTINUE
      ITYP = 3
      CALL S4D(EXX,CDD,IGAUSS)
      STOSC = STOSF(NUCZ,4,1)
      GO TO 300
C
  270 CONTINUE
      ITYP = 6
      CALL S5SP(EXX,CSS,CPP,IGAUSS)
      STOSC = STOSF(NUCZ,5,0)
C
C     ----- DEFINE THE CURRENT SHELL -----
C
  300 CONTINUE
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
C
      SCALE = STOSC*STOSC
      DO 320 I = 1,IGAUSS
         K = K1+I-1
         EX(K) = EXX(I)*SCALE
         CSINP(K) = CSS(I)
         CPINP(K) = CPP(I)
         CDINP(K) = CDD(I)
         CS(K) = CSINP(K)
         CP(K) = CPINP(K)
         CD(K) = CDINP(K)
  320 CONTINUE
C
C     ----- ALWAYS UNNORMALIZE PRIMITIVES -----
C
      DO 410 K = K1,K2
         EE = EX(K)+EX(K)
         FACS = PI32     /(EE*SQRT(EE))
         FACP =  PT5*FACS/ EE
         FACD = PT75*FACS/(EE*EE)
         CS(K) = CS(K)/SQRT(FACS)
         CP(K) = CP(K)/SQRT(FACP)
         CD(K) = CD(K)/SQRT(FACD)
  410 CONTINUE
C
C     ----- IF(NORMF.EQ.0) NORMALIZE BASIS FUNCTIONS -----
C
      IF (NORMF .EQ. 1) GO TO 100
      FACS = ZERO
      FACP = ZERO
      FACD = ZERO
      DO 450 IG = K1,K2
         DO 440 JG = K1,IG
            EE = EX(IG)+EX(JG)
            FAC = EE*SQRT(EE)
            DUMS =      CS(IG)*CS(JG)/       FAC
            DUMP =  PT5*CP(IG)*CP(JG)/(   EE*FAC)
            DUMD = PT75*CD(IG)*CD(JG)/(EE*EE*FAC)
            IF (IG .NE. JG) THEN
               DUMS = DUMS+DUMS
               DUMP = DUMP+DUMP
               DUMD = DUMD+DUMD
            END IF
            FACS = FACS+DUMS
            FACP = FACP+DUMP
            FACD = FACD+DUMD
  440    CONTINUE
  450 CONTINUE
C
      IF(FACS.GT.TM10) FACS=ONE/SQRT(FACS*PI32)
      IF(FACP.GT.TM10) FACP=ONE/SQRT(FACP*PI32)
      IF(FACD.GT.TM10) FACD=ONE/SQRT(FACD*PI32)
C
C                     VERIFY NORMALIZATION TO AT LEAST -TOLNRM-
C
      TNORM=ZERO
      IF(ITYP.EQ.1) TNORM = ABS(ONE-FACS)
      IF(ITYP.EQ.2) TNORM = ABS(ONE-FACP)
      IF(ITYP.EQ.3) TNORM = ABS(ONE-FACD)
      IF(ITYP.EQ.6) TNORM = MAX(ABS(ONE-FACS),ABS(ONE-FACP))
      IF(TNORM.GT.TOLNRM) THEN
         IF (MASWRK) THEN
            WRITE(IW,9000) NUCZ,SHTYP,ITYP,FACS,FACP,FACD
            WRITE(IW,9010) 'ZETA=',(EXX(I),I=1,IGAUSS)
            WRITE(IW,9010) 'C(S)=',(CSS(I),I=1,IGAUSS)
            WRITE(IW,9010) 'C(P)=',(CPP(I),I=1,IGAUSS)
            WRITE(IW,9010) 'C(D)=',(CDD(I),I=1,IGAUSS)
         ENDIF
         CALL ABRT
         STOP
      END IF
C
      DO 460 IG = K1,K2
         CS(IG) = FACS*CS(IG)
         CP(IG) = FACP*CP(IG)
         CD(IG) = FACD*CD(IG)
         CSINP(IG) = FACS*CSINP(IG)
         CPINP(IG) = FACP*CPINP(IG)
         CDINP(IG) = FACD*CDINP(IG)
  460 CONTINUE
      GO TO 100
C
 9000 FORMAT(1X,'**** ERROR, ATOM Z=',I3,' SHELL TYPE=',A4,' ITYP=',I4/
     *       1X,'HAS S,P,D NORMALIZATION=',1P,3F16.10/
     *       1X,'CHECK THESE BUILT IN EXPONENTS AND CONT.COEFS.')
 9010 FORMAT(1X,A5,3F16.10,(/6X,3F16.10))
      END
C*MODULE BASSTO  *DECK STOSF
      DOUBLE PRECISION FUNCTION STOSF(NUCZ,IROW,ISPORD)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION ROW1(2),ROW2(2,8),ROW3(3,8),ROW4(4,8),ROW5(5,8)
      DIMENSION TRM4(5,10),TRM5(6,10)
C
      PARAMETER (ZERO=0.0D+00)
C
C                J.PHYS.CHEM. 52, 2769(1970)
C                J.AM.CHEM.SOC. 100, 260(1978) FOR NA-CL VALENCE
      DATA ROW1/1.24D+00, 1.69D+00/
      DATA ROW2/2.69D+00, 0.80D+00, 3.68D+00, 1.15D+00,
     *          4.68D+00, 1.50D+00, 5.67D+00, 1.72D+00,
     *          6.67D+00, 1.95D+00, 7.66D+00, 2.25D+00,
     *          8.65D+00, 2.55D+00, 9.64D+00, 2.88D+00/
      DATA ROW3/
     *   10.61D+00, 3.48D+00, 1.21D+00, 11.59D+00, 3.90D+00, 1.28D+00,
     *   12.56D+00, 4.36D+00, 1.39D+00, 13.53D+00, 4.83D+00, 1.55D+00,
     *   14.50D+00, 5.31D+00, 1.74D+00, 15.47D+00, 5.79D+00, 1.93D+00,
     *   16.43D+00, 6.26D+00, 2.14D+00, 17.40D+00, 6.74D+00, 2.33D+00/
C                INORG.CHEM. 19, 2225(1980)
      DATA ROW4/18.61D+00,  7.26D+00, 2.75D+00, 1.43D+00,
     *          19.58D+00,  7.74D+00, 3.01D+00, 1.36D+00,
     *          30.42D+00, 12.99D+00, 5.26D+00, 1.80D+00,
     *          31.40D+00, 13.47D+00, 5.58D+00, 2.00D+00,
     *          32.39D+00, 13.94D+00, 5.90D+00, 2.12D+00,
     *          33.37D+00, 14.40D+00, 6.22D+00, 2.22D+00,
     *          34.36D+00, 14.87D+00, 6.54D+00, 2.38D+00,
     *          35.34D+00, 15.34D+00, 6.86D+00, 2.54D+00/
C                 J.COMP.CHEM. 4, 241(1983)
      DATA TRM4/20.56D+00,  8.22D+00, 3.21D+00, 2.33D+00, 1.16D+00,
     *          21.54D+00,  8.70D+00, 3.44D+00, 2.68D+00, 1.21D+00,
     *          22.53D+00,  9.18D+00, 3.67D+00, 2.70D+00, 1.25D+00,
     *          23.52D+00,  9.66D+00, 3.89D+00, 3.23D+00, 1.29D+00,
     *          24.50D+00, 10.13D+00, 4.11D+00, 3.49D+00, 1.32D+00,
     *          25.49D+00, 10.61D+00, 4.33D+00, 3.71D+00, 1.36D+00,
     *          26.47D+00, 11.09D+00, 4.55D+00, 3.94D+00, 1.40D+00,
     *          27.46D+00, 11.56D+00, 4.76D+00, 4.16D+00, 1.43D+00,
     *          28.44D+00, 12.04D+00, 4.98D+00, 4.38D+00, 1.46D+00,
     *          29.43D+00, 12.52D+00, 5.19D+00, 4.61D+00, 1.50D+00/
C                INORG.CHEM. 20, 3650(1981)
      DATA ROW5/36.32D+00, 15.81D+00,  7.18D+00, 3.02D+00, 1.90D+00,
     *          37.31D+00, 16.28D+00,  7.49D+00, 3.16D+00, 1.80D+00,
     *          48.12D+00, 21.33D+00, 11.08D+00, 4.65D+00, 2.05D+00,
     *          49.10D+00, 21.79D+00, 11.39D+00, 4.89D+00, 2.15D+00,
     *          50.08D+00, 22.25D+00, 11.71D+00, 5.12D+00, 2.20D+00,
     *          51.07D+00, 22.71D+00, 12.03D+00, 5.36D+00, 2.28D+00,
     *          52.05D+00, 23.17D+00, 12.35D+00, 5.59D+00, 2.42D+00,
     *          53.03D+00, 23.63D+00, 12.66D+00, 5.82D+00, 2.57D+00/
C                 J.COMP.CHEM. 4, 241(1983)
      DATA TRM5/
     *   38.29D+00, 16.72D+00,  7.97D+00, 3.29D+00, 3.46D+00, 1.24D+00,
     *   39.27D+00, 17.19D+00,  8.21D+00, 3.48D+00, 3.00D+00, 1.29D+00,
     *   40.26D+00, 17.66D+00,  8.51D+00, 3.67D+00, 2.92D+00, 1.35D+00,
     *   41.24D+00, 18.12D+00,  8.82D+00, 3.87D+00, 2.99D+00, 1.40D+00,
     *   42.22D+00, 18.59D+00,  9.14D+00, 4.05D+00, 3.12D+00, 1.45D+00,
     *   43.21D+00, 19.05D+00,  9.45D+00, 4.24D+00, 3.26D+00, 1.49D+00,
     *   44.19D+00, 19.51D+00,  9.77D+00, 4.41D+00, 3.42D+00, 1.53D+00,
     *   45.17D+00, 19.97D+00, 10.09D+00, 4.59D+00, 3.58D+00, 1.57D+00,
     *   46.15D+00, 20.43D+00, 10.41D+00, 4.76D+00, 3.74D+00, 1.60D+00,
     *   47.14D+00, 20.88D+00, 10.74D+00, 4.93D+00, 3.96D+00, 1.63D+00/
C
C        PROVIDE THE DEFAULT STO-NG SCALE FACTORS FOR ROWS 1-5
C        WRITTEN BY MIKE SCHMIDT, 3/31/85, NORTH DAKOTA STATE U.
C
      STOSF=ZERO
      IF(NUCZ.LE. 0) RETURN
      IF(NUCZ.GT.54) RETURN
C
                     ICOL=NUCZ
      IF(NUCZ.GT. 2) ICOL=NUCZ-2
      IF(NUCZ.GT.10) ICOL=NUCZ-10
      IF(NUCZ.GT.18) ICOL=NUCZ-18
      IF(NUCZ.GT.20) ICOL=NUCZ-20
      IF(NUCZ.GT.30) ICOL=NUCZ-28
      IF(NUCZ.GT.36) ICOL=NUCZ-36
      IF(NUCZ.GT.38) ICOL=NUCZ-38
      IF(NUCZ.GT.48) ICOL=NUCZ-46
      IF(NUCZ.GE.1   .AND.  NUCZ.LE. 2) STOSF=ROW1(ICOL)
      IF(NUCZ.GE.3   .AND.  NUCZ.LE.10) STOSF=ROW2(IROW,ICOL)
      IF(NUCZ.GE.11  .AND.  NUCZ.LE.18) STOSF=ROW3(IROW,ICOL)
      IF(NUCZ.GE.19  .AND.  NUCZ.LE.20) STOSF=ROW4(IROW,ICOL)
      IF(NUCZ.GE.21  .AND.  NUCZ.LE.30) STOSF=TRM4(IROW,ICOL)
      IF(NUCZ.LT.21  .OR.   NUCZ.GT.31) GO TO 100
         IRRR = IROW+ISPORD
         IF(IROW.EQ.4) IRRR=5
         STOSF = TRM4(IRRR,ICOL)
  100 CONTINUE
      IF(NUCZ.GE.31  .AND.  NUCZ.LE.36) STOSF=ROW4(IROW,ICOL)
      IF(NUCZ.GE.37  .AND.  NUCZ.LE.38) STOSF=ROW5(IROW,ICOL)
      IF(NUCZ.LT.39  .OR.   NUCZ.GT.48) GO TO 200
         IRRR = IROW+ISPORD
         IF(IROW.EQ.5) IRRR=6
         STOSF = TRM5(IRRR,ICOL)
  200 CONTINUE
      IF(NUCZ.GE.49  .AND.  NUCZ.LE.54) STOSF=ROW5(IROW,ICOL)
      RETURN
      END
C*MODULE BASSTO  *DECK STOSHL
      SUBROUTINE STOSHL(NUCZ,MXSHLS,SHTYPS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION SHTYPS(*)
      DIMENSION DEFALT(7)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DEFALT/8H1S      ,8H2SP     ,8H3SP     ,8H3D      ,
     *            8H4SP     ,8H4D      ,8H5SP     /
#else
      CHARACTER*8 :: DEFALT_STR(7)
      EQUIVALENCE (DEFALT, DEFALT_STR)
      DATA DEFALT_STR/"1S      ","2SP     ","3SP     ","3D      ",
     *            "4SP     ","4D      ","5SP     "/
#endif
C
C         DEFINE A STANDARD SET OF STO-NG SHELLS.
C
      MXSHLS = 0
      IF(NUCZ.LE.54) MXSHLS = 7
      IF(NUCZ.LE.48) MXSHLS = 7
      IF(NUCZ.LE.38) MXSHLS = 6
      IF(NUCZ.LE.36) MXSHLS = 5
      IF(NUCZ.LE.30) MXSHLS = 5
      IF(NUCZ.LE.20) MXSHLS = 4
      IF(NUCZ.LE.18) MXSHLS = 3
      IF(NUCZ.LE.10) MXSHLS = 2
      IF(NUCZ.LE. 2) MXSHLS = 1
      IF(NUCZ.LE. 0) MXSHLS = 0
      DO 100 I=1,MXSHLS
         SHTYPS(I) = DEFALT(I)
  100 CONTINUE
C
C         IN TRANSITION METAL, AND THE LEFT MAIN GROUP, THE INITIAL
C         GUESS CODE EXPECTS THE (N-1)D BELOW THE NSP SHELL.
C         BUT, FOR ALKALI AND ALKALI EARTHS, WE DON'T HAVE A (N-1)D.
C
      IF(NUCZ.EQ.19  .OR.  NUCZ.EQ.20) SHTYPS(4) = DEFALT(5)
      IF(NUCZ.EQ.37  .OR.  NUCZ.EQ.38) SHTYPS(6) = DEFALT(7)
      RETURN
      END
C*MODULE BASSTO  *DECK S1S
      SUBROUTINE S1S(EXX,CS,NGAUSS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C        *****  LEAST SQUARES FITS TO SLATER 1S FUNCTIONS       *****
C        *****  BY GAUSSIAN 1S FUNCTIONS                        *****
      DIMENSION EXX(6),CS(6)
      GO TO (100,120,140,160,180,200),NGAUSS
  100 CONTINUE
C        *****  STO(1S---1(1S))                                 *****
C        *****  ER1 4.3191D-02                                  *****
      EXX(1) = 2.709498091D-01
      CS(1) = 1.000000000D+00
      RETURN
  120 CONTINUE
C        *****  STO(1S---2(1S))                                 *****
C        *****  ER1 3.1606D-03                                  *****
      EXX(1) = 8.518186635D-01
      CS(1) = 4.301284983D-01
      EXX(2) = 1.516232927D-01
      CS(2) = 6.789135305D-01
      RETURN
  140 CONTINUE
C        *****  STO(1S---3(1S))                                 *****
C        *****  ER1 3.3053D-04                                  *****
      EXX(1) = 2.227660584D+00
      CS(1) = 1.543289673D-01
      EXX(2) = 4.057711562D-01
      CS(2) = 5.353281423D-01
      EXX(3) = 1.098175104D-01
      CS(3) = 4.446345422D-01
      RETURN
  160 CONTINUE
C        *****  STO(1S---4(1S))                                 *****
C        *****  ER1 4.3763D-05                                  *****
      EXX(1) = 5.216844534D+00
      CS(1) = 5.675242080D-02
      EXX(2) = 9.546182760D-01
      CS(2) = 2.601413550D-01
      EXX(3) = 2.652034102D-01
      CS(3) = 5.328461143D-01
      EXX(4) = 8.801862774D-02
      CS(4) = 2.916254405D-01
      RETURN
  180 CONTINUE
C        *****  STO(1S---5(1S))                                 *****
C        *****  ER1 6.8840D-06                                  *****
      EXX(1) = 1.130563696D+01
      CS(1) = 2.214055312D-02
      EXX(2) = 2.071728178D+00
      CS(2) = 1.135411520D-01
      EXX(3) = 5.786484833D-01
      CS(3) = 3.318161484D-01
      EXX(4) = 1.975724573D-01
      CS(4) = 4.825700713D-01
      EXX(5) = 7.445271746D-02
      CS(5) = 1.935721966D-01
      RETURN
  200 CONTINUE
C        *****  STO(1S---6(1S))                                 *****
C        *****  ER1 1.2372D-06                                  *****
      EXX(1) = 2.310303149D+01
      CS(1) = 9.163596280D-03
      EXX(2) = 4.235915534D+00
      CS(2) = 4.936149294D-02
      EXX(3) = 1.185056519D+00
      CS(3) = 1.685383049D-01
      EXX(4) = 4.070988982D-01
      CS(4) = 3.705627997D-01
      EXX(5) = 1.580884151D-01
      CS(5) = 4.164915298D-01
      EXX(6) = 6.510953954D-02
      CS(6) = 1.303340841D-01
      RETURN
      END
C*MODULE BASSTO  *DECK S2S
      SUBROUTINE S2S(EXX,CS,NGAUSS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C        *****  LEAST SQUARES FITS TO SLATER 2S FUNCTIONS       *****
C        *****  BY GUASSIAN 1S FUNCTIONS                        *****
      DIMENSION EXX(6),CS(6)
      GO TO (100,120,140,160,180,200),NGAUSS
  100 CONTINUE
C        *****  STO(2S---1(1S))                                 *****
C        *****  ER1 6.2923D-03                                  *****
      EXX(1) = 1.012151084D-01
      CS(1) = 1.000000000D+00
      RETURN
  120 CONTINUE
C        *****  STO(2S---2(1S))                                 *****
C        *****  ER1 2.6728D-03                                  *****
      EXX(1) = 1.292278611D-01
      CS(1) = 7.470867124D-01
      EXX(2) = 4.908584205D-02
      CS(2) = 2.855980556D-01
      RETURN
  140 CONTINUE
C        *****  STO(2S---3(1S))                                 *****
C        *****  ER1 6.8661D-05                                  *****
      EXX(1) = 2.581578398D+00
      CS(1) = -5.994474934D-02
      EXX(2) = 1.567622104D-01
      CS(2) = 5.960385398D-01
      EXX(3) = 6.018332272D-02
      CS(3) = 4.581786291D-01
      RETURN
  160 CONTINUE
C        *****  STO(2S---4(1S))                                 *****
C        *****  ER1 2.6974D-05                                  *****
      EXX(1) = 1.161525551D+01
      CS(1) = -1.198411747D-02
      EXX(2) = 2.000243111D+00
      CS(2) = -5.472052539D-02
      EXX(3) = 1.607280687D-01
      CS(3) = 5.805587176D-01
      EXX(4) = 6.125744532D-02
      CS(4) = 4.770079976D-01
      RETURN
  180 CONTINUE
C        *****  STO(2S---5(1S))                                 *****
C        *****  ER1 2.7009D-06                                  *****
      EXX(1) = 8.984956862D+00
      CS(1) = -1.596349096D-02
      EXX(2) = 1.673710636D+00
      CS(2) = -5.685884883D-02
      EXX(3) = 1.944726668D-01
      CS(3) = 3.698265599D-01
      EXX(4) = 8.806345634D-02
      CS(4) = 5.480512593D-01
      EXX(5) = 4.249068522D-02
      CS(5) = 1.472634893D-01
      RETURN
  200 CONTINUE
C        *****  STO(2S---6(1S))                                 *****
C        *****  ER1 3.2726D-07                                  *****
      EXX(1) = 2.768496241D+01
      CS(1) = -4.151277819D-03
      EXX(2) = 5.077140627D+00
      CS(2) = -2.067024148D-02
      EXX(3) = 1.426786050D+00
      CS(3) = -5.150303337D-02
      EXX(4) = 2.040335729D-01
      CS(4) = 3.346271174D-01
      EXX(5) = 9.260298399D-02
      CS(5) = 5.621061301D-01
      EXX(6) = 4.416183978D-02
      CS(6) = 1.712994697D-01
      RETURN
      END
C*MODULE BASSTO  *DECK S2P
      SUBROUTINE S2P(EXX,CP,NGAUSS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C        *****  LEAST SQUARES FITS TO SLATER 2P FUNCTIONS       *****
C        *****  BY GAUSSIAN 2P FUNCTIONS                        *****
      DIMENSION EXX(6),CP(6)
      GO TO (100,120,140,160,180,200),NGAUSS
  100 CONTINUE
C        *****  STO(2P---1(2P))                                 *****
C        *****  ER1 4.8232D-02                                  *****
      EXX(1) = 1.759666885D-01
      CP(1) = 1.000000000D+00
      RETURN
  120 CONTINUE
C        *****  STO(2P---2(2P))                                 *****
C        *****  ER1 3.0947D-03                                  *****
      EXX(1) = 4.323908358D-01
      CP(1) = 4.522627513D-01
      EXX(2) = 1.069439065D-01
      CP(2) = 6.713122642D-01
      RETURN
  140 CONTINUE
C        *****  STO(2P---3(2P))                                 *****
C        *****  ER1 2.6858D-04                                  *****
      EXX(1) = 9.192379002D-01
      CP(1) = 1.623948553D-01
      EXX(2) = 2.359194503D-01
      CP(2) = 5.661708862D-01
      EXX(3) = 8.009805746D-02
      CP(3) = 4.223071752D-01
      RETURN
  160 CONTINUE
C        *****  STO(2P---4(2P))                                 *****
C        *****  ER1 2.9037D-05                                  *****
      EXX(1) = 1.798260992D+00
      CP(1) = 5.713170255D-02
      EXX(2) = 4.662622228D-01
      CP(2) = 2.857455515D-01
      EXX(3) = 1.643718620D-01
      CP(3) = 5.517873105D-01
      EXX(4) = 6.543927065D-02
      CP(4) = 2.632314924D-01
      RETURN
  180 CONTINUE
C        *****  STO(2P---5(2P))                                 *****
C        *****  ER1 3.7171D-06                                  *****
      EXX(1) = 3.320386533D+00
      CP(1) = 2.079051117D-02
      EXX(2) = 8.643257633D-01
      CP(2) = 1.235472099D-01
      EXX(3) = 3.079819284D-01
      CP(3) = 3.667738886D-01
      EXX(4) = 1.273309895D-01
      CP(4) = 4.834930290D-01
      EXX(5) = 5.606243164D-02
      CP(5) = 1.653444074D-01
      RETURN
  200 CONTINUE
C        *****  STO(2P---6(2P))                                 *****
C        *****  ER1 5.4444D-07                                  *****
      EXX(1) = 5.868285913D+00
      CP(1) = 7.924233646D-03
      EXX(2) = 1.530329631D+00
      CP(2) = 5.144104825D-02
      EXX(3) = 5.475665231D-01
      CP(3) = 1.898400060D-01
      EXX(4) = 2.288932733D-01
      CP(4) = 4.049863191D-01
      EXX(5) = 1.046655969D-01
      CP(5) = 4.012362861D-01
      EXX(6) = 4.948220127D-02
      CP(6) = 1.051855189D-01
      RETURN
      END
C*MODULE BASSTO  *DECK S2SP
      SUBROUTINE S2SP(EXX,CS,CP,NGAUSS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C        *****  SIMULTANEOUS LEAST SQUARES FITS TO SLATER 2S    *****
C        *****  AND 2P FUNCTIONS BY GAUSSIAN 1S + 2P FUNCTIONS  *****
C        *****  WITH COMMON GAUSSIAN EXPONENTS                  *****
      DIMENSION EXX(6),CS(6),CP(6)
      GO TO (100,120,140,160,180,200),NGAUSS
  100 CALL BERROR(1)
  120 CONTINUE
C        *****  STO(2S---2(1S),2P---2(2P))                      *****
C        *****  ER1 5.5955D-03      ER2 3.4793D-03              *****
      EXX(1) = 3.842442531D-01
      CS(1) = 4.947176920D-02
      CP(1) = 5.115407076D-01
      EXX(2) = 9.745448900D-02
      CS(2) = 9.637824081D-01
      CP(2) = 6.128198961D-01
      RETURN
  140 CONTINUE
C        *****  STO(2S---3(1S),2P---3(2P))                      *****
C        *****  ER1 6.4229D-04      ER2 3.6036D-04              *****
      EXX(1) = 9.942027296D-01
      CS(1) = -9.996722919D-02
      CP(1) = 1.559162750D-01
      EXX(2) = 2.310313333D-01
      CS(2) = 3.995128261D-01
      CP(2) = 6.076837186D-01
      EXX(3) = 7.513856000D-02
      CS(3) = 7.001154689D-01
      CP(3) = 3.919573931D-01
      RETURN
  160 CONTINUE
C        *****  STO(2S---4(1S),2P---4(2P))                      *****
C        *****  ER1 7.9474D-05      ER2 4.8191D-05              *****
      EXX(1) = 2.323503675D+00
      CS(1) = -6.220714565D-02
      CP(1) = 4.368434884D-02
      EXX(2) = 5.029886906D-01
      CS(2) = 2.976804596D-05
      CP(2) = 2.863793984D-01
      EXX(3) = 1.635406719D-01
      CS(3) = 5.588549221D-01
      CP(3) = 5.835753141D-01
      EXX(4) = 6.281044213D-02
      CS(4) = 4.977673218D-01
      CP(4) = 2.463134378D-01
      RETURN
  180 CONTINUE
C        *****  STO(2S---5(1S),2P---5(2P))                      *****
C        *****  ER1 1.1708D-05      ER2 7.2753D-06              *****
      EXX(1) = 5.036294248D+00
      CS(1) = -2.940855088D-02
      CP(1) = 1.255609128D-02
      EXX(2) = 1.032503477D+00
      CS(2) = -6.532746883D-02
      CP(2) = 1.075576962D-01
      EXX(3) = 3.290598322D-01
      CS(3) = 1.289973181D-01
      CP(3) = 3.735975367D-01
      EXX(4) = 1.279200125D-01
      CS(4) = 6.122899938D-01
      CP(4) = 5.102395637D-01
      EXX(5) = 5.449486448D-02
      CS(5) = 3.461205655D-01
      CP(5) = 1.568281801D-01
      RETURN
  200 CONTINUE
C        *****  STO(2S---6(1S),2P---6(2P))                      *****
C        *****  ER1 2.0132D-06      ER2 1.2173D-06              *****
      EXX(1) = 1.030869372D+01
      CS(1) = -1.325278809D-02
      CP(1) = 3.759696623D-03
      EXX(2) = 2.040359519D+00
      CS(2) = -4.699171014D-02
      CP(2) = 3.767936984D-02
      EXX(3) = 6.341422177D-01
      CS(3) = -3.378537151D-02
      CP(3) = 1.738967435D-01
      EXX(4) = 2.439773685D-01
      CS(4) = 2.502417861D-01
      CP(4) = 4.180364347D-01
      EXX(5) = 1.059595374D-01
      CS(5) = 5.951172526D-01
      CP(5) = 4.258595477D-01
      EXX(6) = 4.856900860D-02
      CS(6) = 2.407061763D-01
      CP(6) = 1.017082955D-01
      RETURN
      END
C*MODULE BASSTO  *DECK S3S
      SUBROUTINE S3S(EXX,CS,NGAUSS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C        *****  LEAST SQUARES FITS TO SLATER 3S FUNCTIONS BY    *****
C        *****  GAUSSIAN 1S FUNCTIONS                           *****
      DIMENSION EXX(6),CS(6)
      GO TO (100,120,140,160,180,200),NGAUSS
  100 CONTINUE
C        *****  STO(3S---1(1S))                                 *****
C        *****  ER1 1.6764D-02                                  *****
      EXX(1) = 5.296881757D-02
      CS(1) = 1.000000000D+00
      RETURN
  120 CONTINUE
C        *****  STO(3S---2(1S))                                 *****
C        *****  ER1 7.6424D-04                                  *****
      EXX(1) = 6.694095822D-01
      CS(1) = -1.529645716D-01
      EXX(2) = 5.837135094D-02
      CS(2) = 1.051370110D+00
      RETURN
  140 CONTINUE
C        *****  STO(3S---3(1S))                                 *****
C        *****  ER1 8.0718D-05                                  *****
      EXX(1) = 5.641487709D-01
      CS(1) = -1.782577972D-01
      EXX(2) = 6.924421391D-02
      CS(2) = 8.612761663D-01
      EXX(3) = 3.269529097D-02
      CS(3) = 2.261841969D-01
      RETURN
  160 CONTINUE
C        *****  STO(3S---4(1S))                                 *****
C        *****  ER1 1.7254D-06                                  *****
      EXX(1) = 1.513265591D+00
      CS(1) = -3.295496352D-02
      EXX(2) = 4.262497508D-01
      CS(2) = -1.724516959D-01
      EXX(3) = 7.643320863D-02
      CS(3) = 7.518511194D-01
      EXX(4) = 3.760545063D-02
      CS(4) = 3.589627317D-01
      RETURN
  180 CONTINUE
C        *****  STO(3S---5(1S))                                 *****
C        *****  ER1 7.9816D-07                                  *****
      EXX(1) = 4.275877914D+00
      CS(1) = -3.920358850D-03
      EXX(2) = 1.132409433D+00
      CS(2) = -4.168430506D-02
      EXX(3) = 4.016256968D-01
      CS(3) = -1.637440990D-01
      EXX(4) = 7.732370620D-02
      CS(4) = 7.419373723D-01
      EXX(5) = 3.800708627D-02
      CS(5) = 3.724364929D-01
      RETURN
  200 CONTINUE
C        *****  STO(3S---6(1S))                                 *****
C        *****  ER1 4.0662D-08                                  *****
      EXX(1) = 3.273031938D+00
      CS(1) = -6.775596947D-03
      EXX(2) = 9.200611311D-01
      CS(2) = -5.639325779D-02
      EXX(3) = 3.593349765D-01
      CS(3) = -1.587856086D-01
      EXX(4) = 8.636686991D-02
      CS(4) = 5.534527651D-01
      EXX(5) = 4.797373812D-02
      CS(5) = 5.015351020D-01
      EXX(6) = 2.724741144D-02
      CS(6) = 7.223633674D-02
      RETURN
      END
C*MODULE BASSTO  *DECK S3P
      SUBROUTINE S3P(EXX,CP,NGAUSS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C        *****  LEAST SQUARES FITS TO SLATER 3P FUNCTIONS       *****
C        *****  WITH GAUSSIAN 2P FUNCTIONS                      *****
      DIMENSION EXX(6),CP(6)
      GO TO (100,120,140,160,180,200),NGAUSS
  100 CONTINUE
C        *****  STO(3P---1(2P))                                 *****
C        *****  ER1 1.2745D-02                                  *****
      EXX(1) = 9.113614253D-02
      CP(1) = 1.000000000D+00
      RETURN
  120 CONTINUE
C        *****  STO(3P---2(2P))                                 *****
C        *****  ER1 2.3771D-04                                  *****
      EXX(1) = 1.458620964D-01
      CP(1) = 5.349653144D-01
      EXX(2) = 5.664210742D-02
      CP(2) = 5.299607212D-01
      RETURN
  140 CONTINUE
C        *****  STO(3P---3(2P))                                 *****
C        *****  ER1 1.3487D-04                                  *****
      EXX(1) = 2.692880368D+00
      CP(1) = -1.061945788D-02
      EXX(2) = 1.489359592D-01
      CP(2) = 5.218564264D-01
      EXX(3) = 5.739585040D-02
      CP(3) = 5.450015143D-01
      RETURN
  160 CONTINUE
C        *****  STO(3P---4(2P))                                 *****
C        *****  ER1 2.9785D-06                                  *****
      EXX(1) = 1.853180239D+00
      CP(1) = -1.434249391D-02
      EXX(2) = 1.915075719D-01
      CP(2) = 2.755177589D-01
      EXX(3) = 8.655487938D-02
      CP(3) = 5.846750879D-01
      EXX(4) = 4.184253862D-02
      CP(4) = 2.144986514D-01
      RETURN
  180 CONTINUE
C        *****  STO(3P---5(2P))                                 *****
C        *****  ER1 1.3387D-06                                  *****
      EXX(1) = 6.466803859D+00
      CP(1) = -2.329023747D-03
      EXX(2) = 1.555914802D+00
      CP(2) = -1.357395221D-02
      EXX(3) = 1.955925255D-01
      CP(3) = 2.632185383D-01
      EXX(4) = 8.809647701D-02
      CP(4) = 5.880427024D-01
      EXX(5) = 4.234835707D-02
      CP(5) = 2.242794445D-01
      RETURN
  200 CONTINUE
C        *****  STO(3P---6(2P))                                 *****
C        *****  ER1 7.9285D-08                                  *****
      EXX(1) = 5.077973607D+00
      CP(1) = -3.329929840D-03
      EXX(2) = 1.340786940D+00
      CP(2) = -1.419488340D-02
      EXX(3) = 2.248434849D-01
      CP(3) = 1.639395770D-01
      EXX(4) = 1.131741848D-01
      CP(4) = 4.485358256D-01
      EXX(5) = 6.076408893D-02
      CP(5) = 3.908813050D-01
      EXX(6) = 3.315424265D-02
      CP(6) = 7.411456232D-02
      RETURN
      END
C*MODULE BASSTO  *DECK S3D
      SUBROUTINE S3D(EXX,CS,NGAUSS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION EXX(6),CS(6)
      GO TO (100,120,140,160,180,200),NGAUSS
  100 CONTINUE
C*****************      STO(3D---1(3D))
C*********           ER1 5.0728D-02
      EXX(1) = 1.302270363D-01
      CS(1) = 1.000000000D+00
      RETURN
  120 CONTINUE
C*****************      STO(3D---2(3D))
C*********           ER1 2.9834D-03
      EXX(1) = 2.777427345D-01
      CS(1) = 4.666137923D-01
      EXX(2) = 8.336507714D-02
      CS(2) = 6.644706516D-01
      RETURN
  140 CONTINUE
C*****************      STO(3D---3(3D))
C*********           ER1 2.2687D-04
      EXX(1) = 5.229112225D-01
      CS(1) = 1.686596060D-01
      EXX(2) = 1.639595876D-01
      CS(2) = 5.847984817D-01
      EXX(3) = 6.386630021D-02
      CS(3) = 4.056779523D-01
      RETURN
  160 CONTINUE
C*****************      STO(3D---4(3D))
C*********           ER1 2.1116D-05
      EXX(1) = 9.185846715D-01
      CS(1) = 5.799057705D-02
      EXX(2) = 2.920461109D-01
      CS(2) = 3.045581349D-01
      EXX(3) = 1.187568890D-01
      CS(3) = 5.601358038D-01
      EXX(4) = 5.286755896D-02
      CS(4) = 2.432423313D-01
      RETURN
  180 CONTINUE
C*****************      STO(3D---5(3D))
C*********           ER1 2.3115D-06
      EXX(1) = 1.539033958D+00
      CS(1) = 2.020869128D-02
      EXX(2) = 4.922090297D-01
      CS(2) = 1.321157923D-01
      EXX(3) = 2.029756928D-01
      CS(3) = 3.911240346D-01
      EXX(4) = 9.424112917D-02
      CS(4) = 4.779609701D-01
      EXX(5) = 4.569058269D-02
      CS(5) = 1.463662294D-01
      RETURN
  200 CONTINUE
C*****************      STO(3D---6(3D))
C*********           ER1 2.8899D-07
      EXX(1) = 2.488296923D+00
      CS(1) = 7.283828112D-03
      EXX(2) = 7.981487853D-01
      CS(2) = 5.386799363D-02
      EXX(3) = 3.311327490D-01
      CS(3) = 2.072139149D-01
      EXX(4) = 1.559114463D-01
      CS(4) = 4.266269092D-01
      EXX(5) = 7.877734732D-02
      CS(5) = 3.843100204D-01
      EXX(6) = 4.058484363D-02
      CS(6) = 8.902827546D-02
      RETURN
      END
C*MODULE BASSTO  *DECK S3SP
      SUBROUTINE S3SP(EXX,CS,CP,NGAUSS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C        *****  SIMULTANEOUS LEAST SQUARES FITS TO SLATER       *****
C        *****  3S AND 3P FUNCTIONS WITH GAUSSIAN 1S AND 2P     *****
C        *****  FUNCTIONS USING COMMON GAUSSIAN EXPONENTS       *****
      DIMENSION EXX(6),CS(6),CP(6)
      GO TO (100,120,140,160,180,200),NGAUSS
  100 CONTINUE
C        *****  STO(3S---1(1S),3P---1(2P))                      *****
C        *****  ER1 5.2955D-02      ER2 4.1793D-02              *****
      EXX(1) = 7.163507065D-02
      CS(1) = 1.000000000D+00
      CP(1) = 1.000000000D+00
      RETURN
  120 CONTINUE
C        *****  STO(3S---2(1S),3P---2(2P))                      *****
C        *****  ER1 7.3952D-03      ER2 1.0928D-03              *****
      EXX(1) = 1.939790354D-01
      CS(1) = -2.983986044D-01
      CP(1) = 3.480471912D-01
      EXX(2) = 6.655227113D-02
      CS(2) = 1.227982887D+00
      CP(2) = 7.222523221D-01
      RETURN
  140 CONTINUE
C        *****  STO(3S---3(1S),3P---3(2P))                      *****
C        *****  ER1 3.1253D-04      ER2 2.7910D-04              *****
      EXX(1) = 4.828540806D-01
      CS(1) = -2.196203690D-01
      CP(1) = 1.058760429D-02
      EXX(2) = 1.347150629D-01
      CS(2) = 2.255954336D-01
      CP(2) = 5.951670053D-01
      EXX(3) = 5.272656258D-02
      CS(3) = 9.003984260D-01
      CP(3) = 4.620010120D-01
      RETURN
  160 CONTINUE
C        *****  STO(3S---4(1S),3P---4(2P))                      *****
C        *****  ER1 2.2403D-05      ER2 2.8610D-05              *****
      EXX(1) = 9.217072439D-01
      CS(1) = -8.529019644D-02
      CP(1) = -2.504945181D-02
      EXX(2) = 2.534730123D-01
      CS(2) = -2.132074034D-01
      CP(2) = 1.686604461D-01
      EXX(3) = 9.976476472D-02
      CS(3) = 5.920843928D-01
      CP(3) = 6.409553151D-01
      EXX(4) = 4.439990833D-02
      CS(4) = 6.115584746D-01
      CP(4) = 2.779508957D-01
      RETURN
  180 CONTINUE
C        *****  STO(3S---5(1S),3P---5(2P))                      *****
C        *****  ER1 1.9857D-06      ER2 3.5565D-06              *****
      EXX(1) = 1.709113496D+00
      CS(1) = -2.662203391D-02
      CP(1) = -1.566883448D-02
      EXX(2) = 4.654245585D-01
      CS(2) = -1.603484072D-01
      CP(2) = 7.214920506D-03
      EXX(3) = 1.785129144D-01
      CS(3) = -4.779473307D-02
      CP(3) = 3.170854762D-01
      EXX(4) = 8.067420191D-02
      CS(4) = 7.275158441D-01
      CP(4) = 5.818821382D-01
      EXX(5) = 3.914669014D-02
      CS(5) = 4.123846408D-01
      CP(5) = 1.701799824D-01
      RETURN
  200 CONTINUE
C        *****  STO(3S---6(1S),3P---6(2P))                      *****
C        *****  ER1 2.2088D-07      ER2 4.9043D-07              *****
      EXX(1) = 3.080165240D+00
      CS(1) = -7.943126362D-03
      CP(1) = -7.139358907D-03
      EXX(2) = 8.248959202D-01
      CS(2) = -7.100264172D-02
      CP(2) = -1.829277070D-02
      EXX(3) = 3.093447349D-01
      CS(3) = -1.785026925D-01
      CP(3) = 7.621621428D-02
      EXX(4) = 1.384683897D-01
      CS(4) = 1.510635058D-01
      CP(4) = 4.145098597D-01
      EXX(5) = 6.852094951D-02
      CS(5) = 7.354914767D-01
      CP(5) = 4.889621471D-01
      EXX(6) = 3.531333690D-02
      CS(6) = 2.760593123D-01
      CP(6) = 1.058816521D-01
      RETURN
      END
C*MODULE BASSTO  *DECK S3SPD
      SUBROUTINE S3SPD(EXX,CS,CP,CD,NGAUSS)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION EXX(NGAUSS),CS(NGAUSS),CP(NGAUSS),CD(NGAUSS)
C
C        INORG. CHEM. 19,2225(1980)
C
      GO TO (100,200,300,400,500,600), NGAUSS
  100 CONTINUE
      CALL ABRT
C
  200 CONTINUE
      EXX(1)=2.23602D-01
      EXX(2)=6.91573D-02
      CS(1)=-3.12172D-01
      CS(2)= 1.22540D+00
      CP(1)= 2.94003D-01
      CP(2)= 7.80042D-01
      CD(1)= 6.00604D-01
      CD(2)= 5.29204D-01
      RETURN
C
  300 CONTINUE
      EXX(1)=4.55950D-01
      EXX(2)=1.39079D-01
      EXX(3)=5.36612D-02
      CS(1)=-2.27764D-01
      CS(2)= 2.17544D-01
      CS(3)= 9.16677D-01
      CP(1)= 4.95151D-03
      CP(2)= 5.77766D-01
      CP(3)= 4.84646D-01
      CD(1)= 2.19768D-01
      CD(2)= 6.55547D-01
      CD(3)= 2.86573D-01
      RETURN
C
  400 CONTINUE
      EXX(1)=8.64219D-01
      EXX(2)=2.58758D-01
      EXX(3)=1.02634D-01
      EXX(4)=4.56280D-02
      CS(1)=-8.88886D-02
      CS(2)=-2.02619D-01
      CS(3)= 5.45089D-01
      CS(4)= 6.51702D-01
      CP(1)=-2.58036D-02
      CP(2)= 1.59141D-01
      CP(3)= 6.28412D-01
      CP(4)= 3.00984D-01
      CD(1)= 6.92077D-02
      CD(2)= 3.72816D-01
      CD(3)= 5.66409D-01
      CD(4)= 1.58595D-01
      RETURN
  500 CONTINUE
      EXX(1)=1.56001D+00
      EXX(2)=4.58958D-01
      EXX(3)=1.81819D-01
      EXX(4)=8.29517D-02
      EXX(5)=4.00949D-02
      CS(1)=-2.99608D-02
      CS(2)=-1.54905D-01
      CS(3)=-5.89327D-02
      CS(4)= 7.04419D-01
      CS(5)= 4.44530D-01
      CP(1)=-1.66512D-02
      CP(2)= 9.09022D-03
      CP(3)= 2.98563D-01
      CP(4)= 5.83201D-01
      CP(5)= 1.86709D-01
      CD(1)= 2.12997D-02
      CD(2)= 1.57672D-01
      CD(3)= 4.48517D-01
      CD(4)= 4.49537D-01
      CD(5)= 9.12249D-02
      RETURN
  600 CONTINUE
      EXX(1)=2.73051D+00
      EXX(2)=7.86247D-01
      EXX(3)=3.09251D-01
      EXX(4)=1.41338D-01
      EXX(5)=7.03391D-02
      EXX(6)=3.61235D-02
      CS(1)=-9.73740D-03
      CS(2)=-7.26588D-02
      CS(3)=-1.71616D-01
      CS(4)= 1.28978D-01
      CS(5)= 7.28861D-01
      CS(6)= 3.01332D-01
      CP(1)=-8.10494D-03
      CP(2)=-1.71548D-02
      CP(3)= 7.36979D-02
      CP(4)= 3.96515D-01
      CP(5)= 4.97808D-01
      CP(6)= 1.17483D-01
      CD(1)= 6.63343D-03
      CD(2)= 5.95818D-02
      CD(3)= 2.40195D-01
      CD(4)= 4.64811D-01
      CD(5)= 3.43409D-01
      CD(6)= 5.38906D-02
      RETURN
      END
C*MODULE BASSTO  *DECK S4S
      SUBROUTINE S4S(EXX,CS,NGAUSS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION EXX(6),CS(6)
      GO TO (100,120,140,160,180,200),NGAUSS
  100 CONTINUE
C*****************      STO(4S---1(1S))
C*********           ER1 4.1760D-02
      EXX(1) = 3.264600274D-02
      CS(1) = 1.000000000D+00
      RETURN
  120 CONTINUE
C*****************      STO(4S---2(1S))
C*********           ER1 1.7480D-04
      EXX(1) = 2.441785453D-01
      CS(1) = -3.046656896D-01
      EXX(2) = 4.051097664D-02
      CS(2) = 1.146877294D+00
      RETURN
  140 CONTINUE
C*****************      STO(4S---3(1S))
C*********           ER1 1.6867D-05
      EXX(1) = 2.267938753D-01
      CS(1) = -3.349048323D-01
      EXX(2) = 4.448178019D-02
      CS(2) = 1.056744667D+00
      EXX(3) = 2.195294664D-02
      CS(3) = 1.256661680D-01
      RETURN
  160 CONTINUE
C*****************      STO(4S---4(1S))
C*********           ER1 8.4105D-07
      EXX(1) = 3.242212833D-01
      CS(1) = -1.120682822D-01
      EXX(2) = 1.663217177D-01
      CS(2) = -2.845426863D-01
      EXX(3) = 5.081097451D-02
      CS(3) = 8.909873788D-01
      EXX(4) = 2.829066600D-02
      CS(4) = 3.517811205D-01
      RETURN
  180 CONTINUE
C*****************      STO(4S---5(1S))
C*********           ER1 6.0273D-08
      EXX(1) = 2.980263783D+00
      CS(1) = 1.513948997D-03
      EXX(2) = 3.792228833D-01
      CS(2) = -7.316801548D-02
      EXX(3) = 1.789717224D-01
      CS(3) = -3.143703799D-01
      EXX(4) = 5.002110360D-02
      CS(4) = 9.032615169D-01
      EXX(5) = 2.789361681D-02
      CS(5) = 3.294210848D-01
      RETURN
  200 CONTINUE
C*****************      STO(4S---6(1S))
C*********           ER1 5.9700D-09
      EXX(1) = 3.232838646D+00
      CS(1) = 1.374817488D-03
      EXX(2) = 3.605788802D-01
      CS(2) = -8.666390043D-02
      EXX(3) = 1.717905487D-01
      CS(3) = -3.130627309D-01
      EXX(4) = 5.277666487D-02
      CS(4) = 7.812787397D-01
      EXX(5) = 3.163400284D-02
      CS(5) = 4.389247988D-01
      EXX(6) = 1.874093091D-02
      CS(6) = 2.487178756D-02
      RETURN
      END
C*MODULE BASSTO  *DECK S4P
      SUBROUTINE S4P(EXX,CP,NGAUSS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION EXX(6),CP(6)
      GO TO (100,120,140,160,180,200),NGAUSS
  100 CONTINUE
C*****************      STO(4P---1(2P))
C*********           ER1 3.9516D-03
      EXX(1) = 5.578350235D-02
      CP(1) = 1.000000000D+00
      RETURN
  120 CONTINUE
C*****************      STO(4P---2(2P))
C*********           ER1 2.1579D-03
      EXX(1) = 6.190052680D-02
      CP(1) = 8.743116767D-01
      EXX(2) = 2.648418407D-02
      CP(2) = 1.513640107D-01
      RETURN
  140 CONTINUE
C*****************      STO(4P---3(2P))
C*********           ER1 1.2621D-05
      EXX(1) = 4.859692220D-01
      CP(1) = -6.147823411D-02
      EXX(2) = 7.430216918D-02
      CP(2) = 6.604172234D-01
      EXX(3) = 3.653340923D-02
      CP(3) = 3.932639495D-01
      RETURN
  160 CONTINUE
C*****************      STO(4P---4(2P))
C*********           ER1 5.4740D-06
      EXX(1) = 1.492607880D+00
      CP(1) = -6.035216774D-03
      EXX(2) = 4.327619272D-01
      CP(2) = -6.013310874D-02
      EXX(3) = 7.553156064D-02
      CP(3) = 6.451518200D-01
      EXX(4) = 3.706272183D-02
      CP(4) = 4.117923820D-01
      RETURN
  180 CONTINUE
C*****************      STO(4P---5(2P))
C*********           ER1 1.3595D-07
      EXX(1) = 1.091977298D+00
      CP(1) = -1.143929558D-02
      EXX(2) = 3.719985051D-01
      CP(2) = -6.322651538D-02
      EXX(3) = 8.590019352D-02
      CP(3) = 4.398907721D-01
      EXX(4) = 4.786503860D-02
      CP(4) = 5.245859166D-01
      EXX(5) = 2.730479990D-02
      CP(5) = 1.017072253D-01
      RETURN
  200 CONTINUE
C*****************      STO(4P---6(2P))
C*********           ER1 1.3897D-08
      EXX(1) = 2.389722618D+00
      CP(1) = -1.665913575D-03
      EXX(2) = 7.960947826D-01
      CP(2) = -1.657464971D-02
      EXX(3) = 3.415541380D-01
      CP(3) = -5.958513378D-02
      EXX(4) = 8.847434525D-02
      CP(4) = 4.053115554D-01
      EXX(5) = 4.958248334D-02
      CP(5) = 5.433958189D-01
      EXX(6) = 2.816929784D-02
      CP(6) = 1.204970491D-01
      RETURN
      END
C*MODULE BASSTO  *DECK S4D
      SUBROUTINE S4D(EXX,CD,NGAUSS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION EXX(6),CD(6)
      GO TO (100,120,140,160,180,200),NGAUSS
  100 CONTINUE
C*****************      STO(4D---1(3D))
C*********           ER1 2.0360D-02
      EXX(1) = 7.941656339D-02
      CD(1) = 1.000000000D+00
      RETURN
  120 CONTINUE
C*****************      STO(4D---2(3D))
C*********           ER1 3.7488D-04
      EXX(1) = 1.330958892D-01
      CD(1) = 4.932764167D-01
      EXX(2) = 5.272119659D-02
      CD(2) = 5.918727866D-01
      RETURN
  140 CONTINUE
C*****************      STO(4D---3(3D))
C*********           ER1 1.2729D-05
      EXX(1) = 1.777717219D-01
      CD(1) = 2.308552718D-01
      EXX(2) = 8.040647350D-02
      CD(2) = 6.042409177D-01
      EXX(3) = 3.949855551D-02
      CD(3) = 2.595768926D-01
      RETURN
  160 CONTINUE
C*****************      STO(4D---4(3D))
C*********           ER1 5.7655D-06
      EXX(1) = 1.995825422D+00
      CD(1) = -2.816702620D-03
      EXX(2) = 1.823461280D-01
      CD(2) = 2.177095871D-01
      EXX(3) = 8.197240896D-02
      CD(3) = 6.058047348D-01
      EXX(4) = 4.000634951D-02
      CD(4) = 2.717811257D-01
      RETURN
  180 CONTINUE
C*****************      STO(4D---5(3D))
C*********           ER1 1.5122D-07
      EXX(1) = 1.522122079D+00
      CD(1) = -3.673711876D-03
      EXX(2) = 2.173041823D-01
      CD(2) = 1.167122499D-01
      EXX(3) = 1.084876577D-01
      CD(3) = 4.216476416D-01
      EXX(4) = 5.836797641D-02
      CD(4) = 4.547673415D-01
      EXX(5) = 3.206682246D-02
      CD(5) = 1.037803318D-01
      RETURN
  200 CONTINUE
C*****************      STO(4D---6(3D))
C*********           ER1 8.0888D-08
      EXX(1) = 4.634239420D+00
      CD(1) = -4.749842876D-04
      EXX(2) = 1.341648295D+00
      CD(2) = -3.566777891D-03
      EXX(3) = 2.209593028D-01
      CD(3) = 1.108670481D-01
      EXX(4) = 1.101467943D-01
      CD(4) = 4.159646930D-01
      EXX(5) = 5.904190370D-02
      CD(5) = 4.621672517D-01
      EXX(6) = 3.232628887D-02
      CD(6) = 1.081250196D-01
      RETURN
      END
C*MODULE BASSTO  *DECK S4SP
      SUBROUTINE S4SP(EXX,CS,CP,NGAUSS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION EXX(6),CS(6),CP(6)
C
C        THIS ROUTINE WAS AVAILABLE IN COPIES OF GAUSSIAN XX
C        LONG BEFORE THESE NUMBERS WERE FINALLY PUBLISHED, WITH
C        FEWER DIGITS, IN INORG.CHEM. 19, 2225(1980)
C
      GO TO (100,120,140,160,180,200),NGAUSS
  100 CALL ABRT
  120 CONTINUE
C*****************      STO(4S---2(1S),4P---2(2P))
C*********           ER1 5.1660D-03      ER2 4.1650D-03
      EXX(1) = 1.242349951D-01
      CS(1) = -6.615603018D-01
      CP(1) = 1.309890515D-01
      EXX(2) = 4.999746691D-02
      CS(2) = 1.510754134D+00
      CP(2) = 8.946431268D-01
      RETURN
  140 CONTINUE
C*****************      STO(4S---3(1S),4P---3(2P))
C*********           ER1 1.6710D-04      ER2 2.9851D-04
      EXX(1) = 2.464581400D-01
      CS(1) = -3.088441215D-01
      CP(1) = -1.215468600D-01
      EXX(2) = 9.095855374D-02
      CS(2) = 1.960641166D-02
      CP(2) = 5.715227604D-01
      EXX(3) = 4.016825636D-02
      CS(3) = 1.131034442D+00
      CP(3) = 5.498949471D-01
      RETURN
  160 CONTINUE
C*****************      STO(4S---4(1S),4P---4(2P))
C*********           ER1 1.5073D-05      ER2 1.6717D-05
      EXX(1) = 4.389527791D-01
      CS(1) = -5.893559813D-02
      CP(1) = -7.180522657D-02
      EXX(2) = 1.624449952D-01
      CS(2) = -4.289230261D-01
      CP(2) = 3.726863425D-02
      EXX(3) = 6.983976374D-02
      CS(3) = 5.472998468D-01
      CP(3) = 6.965842249D-01
      EXX(4) = 3.469204610D-02
      CS(4) = 7.853692030D-01
      CP(4) = 3.310236850D-01
      RETURN
  180 CONTINUE
C*****************      STO(4S---5(1S),4P---5(2P))
C*********           ER1 1.6981D-06      ER2 1.1140D-06
      EXX(1) = 7.857764224D-01
      CS(1) = -1.628249797D-03
      CP(1) = -2.399519595D-02
      EXX(2) = 2.685947984D-01
      CS(2) = -2.138931336D-01
      CP(2) = -8.118321551D-02
      EXX(3) = 1.155680976D-01
      CS(3) = -3.039583456D-01
      CP(3) = 2.238628944D-01
      EXX(4) = 5.839870334D-02
      CS(4) = 8.491175450D-01
      CP(4) = 6.726900408D-01
      EXX(5) = 3.082651925D-02
      CS(5) = 5.164147260D-01
      CP(5) = 2.003802746D-01
      RETURN
  200 CONTINUE
C*****************      STO(3S---6(1S),3P---6(2P))
      EXX(1) = 3.080165240D+00
      EXX(2) = 8.248959202D-01
      EXX(3) = 3.093447349D-01
      EXX(4) = 1.384683897D-01
      EXX(5) = 6.852094951D-02
      EXX(6) = 3.531333690D-02
      CS(1) = -7.943126362D-03
      CS(2) = -7.100264172D-02
      CS(3) = -1.785026925D-01
      CS(4) = 1.510635058D-01
      CS(5) = 7.354914767D-01
      CS(6) = 2.760593123D-01
      CP(1) = -7.139358907D-03
      CP(2) = -1.829277070D-02
      CP(3) = 7.621621428D-02
      CP(4) = 4.145098597D-01
      CP(5) = 4.889621471D-01
      CP(6) = 1.058816521D-01
      RETURN
      END
C*MODULE BASSTO  *DECK S4SPD
      SUBROUTINE S4SPD(EXX,CS,CP,CD,NGAUSS)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION EXX(NGAUSS),CS(NGAUSS),CP(NGAUSS),CD(NGAUSS)
C
C        INORG.CHEM. 20, 3652(1981)
C
      GO TO (100,200,300,400,500,600), NGAUSS
  100 CONTINUE
      CALL ABRT
C
  200 CONTINUE
      EXX(1)=1.26011D-01
      EXX(2)=5.00901D-02
      CS(1)=-6.56197D-01
      CS(2)= 1.50354D+00
      CP(1)= 1.27008D-01
      CP(2)= 8.98533D-01
      CD(1)= 5.43275D-01
      CD(2)= 5.42087D-01
      RETURN
C
  300 CONTINUE
      EXX(1)=2.33486D-01
      EXX(2)=9.09182D-02
      EXX(3)=4.00224D-02
      CS(1)=-3.30610D-01
      CS(2)= 5.76110D-02
      CS(3)= 1.11558D+00
      CP(1)=-1.28393D-01
      CP(2)= 5.85205D-01
      CP(3)= 5.43944D-01
      CD(1)= 1.25066D-01
      CD(2)= 6.68679D-01
      CD(3)= 3.05247D-01
      RETURN
C
  400 CONTINUE
      EXX(1)=4.50425D-01
      EXX(2)=1.60793D-01
      EXX(3)=7.16348D-02
      EXX(4)=3.51148D-02
      CS(1)=-5.84688D-02
      CS(2)=-4.39804D-01
      CS(3)= 5.31922D-01
      CS(4)= 8.10747D-01
      CP(1)=-6.74634D-02
      CP(2)= 2.07793D-02
      CP(3)= 6.88454D-01
      CP(4)= 3.51482D-01
      CD(1)= 3.86994D-03
      CD(2)= 2.97612D-01
      CD(3)= 6.20373D-01
      CD(4)= 1.73553D-01
      RETURN
C
  500 CONTINUE
      EXX(1)=8.14733D-01
      EXX(2)=2.73043D-01
      EXX(3)=1.19670D-01
      EXX(4)=5.98104D-02
      EXX(5)=3.14652D-02
      CS(1)=-1.62523D-03
      CS(2)=-2.02142D-01
      CS(3)=-3.15702D-01
      CS(4)= 8.08210D-01
      CS(5)= 5.57217D-01
      CP(1)=-2.25272D-02
      CP(2)=-8.33081D-02
      CP(3)= 2.02471D-01
      CP(4)= 6.76431D-01
      CP(5)= 2.18597D-01
      CD(1)=-7.36629D-03
      CD(2)= 6.95976D-02
      CD(3)= 4.23805D-01
      CD(4)= 5.07395D-01
      CD(5)= 1.00361D-01
      RETURN
C
  600 CONTINUE
      EXX(1)=1.37434D+00
      EXX(2)=4.46516D-01
      EXX(3)=1.92451D-01
      EXX(4)=9.58282D-02
      EXX(5)=5.16642D-02
      EXX(6)=2.85894D-02
      CS(1)= 3.17377D-03
      CS(2)=-5.21220D-02
      CS(3)=-3.07763D-01
      CS(4)=-6.45125D-02
      CS(5)= 8.98078D-01
      CS(6)= 3.69096D-01
      CP(1)=-6.87731D-03
      CP(2)=-5.08511D-02
      CP(3)=-4.20645D-02
      CP(4)= 3.65240D-01
      CP(5)= 5.91591D-01
      CP(6)= 1.34413D-01
      CD(1)=-4.34638D-03
      CD(2)= 4.89123D-03
      CD(3)= 1.63480D-01
      CD(4)= 4.80748D-01
      CD(5)= 3.90692D-01
      CD(6)= 5.78663D-02
      RETURN
      END
C*MODULE BASSTO  *DECK S5S
      SUBROUTINE S5S(EXX,CS,NGAUSS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION EXX(6),CS(6)
      GO TO (100,120,140,160,180,200),NGAUSS
  100 CONTINUE
C*****************      STO(5S---1(1S))
C*********           ER1 7.1317D-02
      EXX(1) = 2.216912938D-02
      CS(1) = 1.000000000D+00
      RETURN
  120 CONTINUE
C*****************      STO(5S---2(1S))
C*********           ER1 1.4289D-04
      EXX(1) = 1.213425654D-01
      CS(1) = -5.114756049D-01
      EXX(2) = 3.133152144D-02
      CS(2) = 1.307377277D+00
      RETURN
  140 CONTINUE
C*****************      STO(5S---3(1S))
C*********           ER1 4.1124D-05
      EXX(1) = 1.080198458D-01
      CS(1) = -6.617401158D-01
      EXX(2) = 4.408119382D-02
      CS(2) = 7.467595004D-01
      EXX(3) = 2.610811810D-02
      CS(3) = 7.146490945D-01
      RETURN
  160 CONTINUE
C*****************      STO(5S---4(1S))
C*********           ER1 5.4159D-07
      EXX(1) = 8.602284252D-01
      CS(1) = 1.103657561D-02
      EXX(2) = 1.189050200D-01
      CS(2) = -5.606519023D-01
      EXX(3) = 3.446076176D-02
      CS(3) = 1.179429987D+00
      EXX(4) = 1.974798796D-02
      CS(4) = 1.734974376D-01
      RETURN
  180 CONTINUE
C*****************      STO(5S---5(1S))
C*********           ER1 7.0816D-08
      EXX(1) = 7.403763257D-01
      CS(1) = 1.375523371D-02
      EXX(2) = 1.367990863D-01
      CS(2) = -3.097344179D-01
      EXX(3) = 9.135301779D-02
      CS(3) = -3.199192259D-01
      EXX(4) = 3.726907315D-02
      CS(4) = 1.084547038D+00
      EXX(5) = 2.241490836D-02
      CS(5) = 3.345288361D-01
      RETURN
  200 CONTINUE
C*****************      STO(5S---6(1S))
C*********           ER1 7.9988D-09
      EXX(1) = 1.410128298D+00
      CS(1) = 2.695439582D-03
      EXX(2) = 5.077878915D-01
      CS(2) = 1.850157487D-02
      EXX(3) = 1.847926858D-01
      CS(3) = -9.588628125D-02
      EXX(4) = 1.061070594D-01
      CS(4) = -5.200673560D-01
      EXX(5) = 3.669584901D-02
      CS(5) = 1.087619490D+00
      EXX(6) = 2.213558430D-02
      CS(6) = 3.103964343D-01
      RETURN
      END
C*MODULE BASSTO  *DECK S5P
      SUBROUTINE S5P(EXX,CP,NGAUSS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION EXX(6),CP(6)
      GO TO (100,120,140,160,180,200),NGAUSS
  100 CONTINUE
C*****************      STO(5P---1(2P))
C*********           ER1 8.0406D-03
      EXX(1) = 3.769845216D-02
      CP(1) = 1.000000000D+00
      RETURN
  120 CONTINUE
C*****************      STO(5P---2(2P))
C*********           ER1 6.8912D-04
      EXX(1) = 2.691294191D-01
      CP(1) = -1.034227010D-01
      EXX(2) = 3.980805011D-02
      CP(2) = 1.033376378D+00
      RETURN
  140 CONTINUE
C*****************      STO(5P---3(2P))
C*********           ER1 6.5766D-06
      EXX(1) = 2.127482317D-01
      CP(1) = -1.389529695D-01
      EXX(2) = 4.729648620D-02
      CP(2) = 8.076691064D-01
      EXX(3) = 2.604865324D-02
      CP(3) = 2.726029342D-01
      RETURN
  160 CONTINUE
C*****************      STO(5P---4(2P))
C*********           ER1 4.4493D-07
      EXX(1) = 3.962838833D-01
      CP(1) = -1.801459207D-02
      EXX(2) = 1.838858552D-01
      CP(2) = -1.360777372D-01
      EXX(3) = 4.943555157D-02
      CP(3) = 7.533973719D-01
      EXX(4) = 2.750222273D-02
      CP(4) = 3.409304859D-01
      RETURN
  180 CONTINUE
C*****************      STO(5P---5(2P))
C*********           ER1 1.3483D-08
      EXX(1) = 3.422168934D-01
      CP(1) = -3.113958289D-02
      EXX(2) = 1.665099900D-01
      CP(2) = -1.374007017D-01
      EXX(3) = 5.443732013D-02
      CP(3) = 5.573881018D-01
      EXX(4) = 3.367775277D-02
      CP(4) = 4.855428100D-01
      EXX(5) = 2.091949042D-02
      CP(5) = 6.605423564D-02
      RETURN
  200 CONTINUE
C*****************      STO(5P---6(2P))
C*********           ER1 3.9301D-09
      EXX(1) = 3.778623374D+00
      CP(1) = 1.163246387D-04
      EXX(2) = 3.499121109D-01
      CP(2) = -2.920771322D-02
      EXX(3) = 1.683175469D-01
      CP(3) = -1.381051233D-01
      EXX(4) = 5.404070736D-02
      CP(4) = 5.706134877D-01
      EXX(5) = 3.328911801D-02
      CP(5) = 4.768808140D-01
      EXX(6) = 2.063815019D-02
      CP(6) = 6.021665516D-02
      RETURN
      END
C*MODULE BASSTO  *DECK S5D
      SUBROUTINE S5D(EXX,CD,NGAUSS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION EXX(6),CD(6)
      GO TO (100,120,140,160,180,200),NGAUSS
  100 CONTINUE
C*****************      STO(5D---1(3D))
C*********           ER1 6.7308D-03
      EXX(1) = 5.352200793D-02
      CD(1) = 1.000000000D+00
      RETURN
  120 CONTINUE
C*****************      STO(5D---2(3D))
C*********           ER1 3.1257D-04
      EXX(1) = 6.906014388D-02
      CD(1) = 6.539405185D-01
      EXX(2) = 3.399457777D-02
      CD(2) = 3.948945302D-01
      RETURN
  140 CONTINUE
C*****************      STO(5D---3(3D))
C*********           ER1 2.4664D-05
      EXX(1) = 4.913352950D-01
      CD(1) = -2.010175008D-02
      EXX(2) = 7.329090601D-02
      CD(2) = 5.899370608D-01
      EXX(3) = 3.594209290D-02
      CD(3) = 4.658445960D-01
      RETURN
  160 CONTINUE
C*****************      STO(5D---4(3D))
C*********           ER1 1.2737D-06
      EXX(1) = 4.230617826D-01
      CD(1) = -2.421626009D-02
      EXX(2) = 8.293863702D-02
      CD(2) = 3.937644956D-01
      EXX(3) = 4.590326388D-02
      CD(3) = 5.489520286D-01
      EXX(4) = 2.628744797D-02
      CD(4) = 1.190436963D-01
      RETURN
  180 CONTINUE
C*****************      STO(5D---5(3D))
C*********           ER1 8.6929D-08
      EXX(1) = 9.702946470D-01
      CD(1) = -3.231527611D-03
      EXX(2) = 3.603270196D-01
      CD(2) = -2.434931372D-02
      EXX(3) = 8.668717752D-02
      CD(3) = 3.440817054D-01
      EXX(4) = 4.833708379D-02
      CD(4) = 5.693674376D-01
      EXX(5) = 2.751899341D-02
      CD(5) = 1.511340183D-01
      RETURN
  200 CONTINUE
C*****************      STO(5D---6(3D))
C*********           ER1 1.4086D-08
      EXX(1) = 8.820520428D-01
      CD(1) = -4.097377019D-03
      EXX(2) = 3.410838409D-01
      CD(2) = -2.508271857D-02
      EXX(3) = 9.204308840D-02
      CD(3) = 2.648458555D-01
      EXX(4) = 5.472831774D-02
      CD(4) = 5.097437054D-01
      EXX(5) = 3.391202830D-02
      CD(5) = 2.654483467D-01
      EXX(6) = 2.108227374D-02
      CD(6) = 2.623132212D-02
      RETURN
      END
C*MODULE BASSTO  *DECK S5SP
      SUBROUTINE S5SP(EXX,CS,CP,NGAUSS)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION EXX(NGAUSS),CS(NGAUSS),CP(NGAUSS)
C
C     INORG.CHEM. 20,3650(1981)
C
      GO TO (100,200,300,400,500,600),NGAUSS
  100 CONTINUE
      CALL ABRT
C
  200 CONTINUE
      EXX(1)=8.12432D-02
      EXX(2)=3.93036D-02
      CS(1)=-1.09369D+00
      CS(2)= 1.88137D+00
      CP(1)=-1.04740D-01
      CP(2)= 1.08763D+00
      RETURN
C
  300 CONTINUE
      EXX(1)=1.34901D-01
      EXX(2)=7.26361D-02
      EXX(3)=3.20846D-02
      CS(1)=-3.84264D-01
      CS(2)=-1.97257D-01
      CS(3)= 1.37550D+00
      CP(1)=-3.48169D-01
      CP(2)= 6.29032D-01
      CP(3)= 6.66283D-01
      RETURN
C
  400 CONTINUE
      EXX(1)=2.57771D-01
      EXX(2)=1.18964D-01
      EXX(3)=5.27078D-02
      EXX(4)=2.87036D-02
      CS(1)= 4.04511D-02
      CS(2)=-6.57669D-01
      CS(3)= 3.79252D-01
      CS(4)= 1.03859D+00
      CP(1)=-8.58615D-02
      CP(2)=-1.09015D-01
      CP(3)= 7.23404D-01
      CP(4)= 4.11743D-01
      RETURN
C
  500 CONTINUE
      EXX(1)=4.82262D-01
      EXX(2)=1.81352D-01
      EXX(3)=8.58891D-02
      EXX(4)=4.61741D-02
      EXX(5)=2.58255D-02
      CS(1)= 3.03766D-02
      CS(2)=-1.82506D-01
      CS(3)=-6.59149D-01
      CS(4)= 9.18693D-01
      CS(5)= 6.95274D-01
      CP(1)=-1.25767D-02
      CP(2)=-1.56394D-01
      CP(3)= 6.50075D-02
      CP(4)= 7.85982D-01
      CP(5)= 2.58228D-01
      RETURN
C
  600 CONTINUE
      EXX(1)=7.70142D-01
      EXX(2)=2.34839D-02
      EXX(3)=4.00255D-02
      EXX(4)=6.95344D-02
      EXX(5)=1.30185D-01
      EXX(6)=2.75627D-01
      CS(1)= 1.26745D-02
      CS(2)= 4.36850D-01
      CS(3)= 1.10432D+00
      CS(4)=-3.23200D-01
      CS(5)=-4.30756D-01
      CS(6)= 3.26670D-03
      CP(1)=-1.10568D-03
      CP(2)= 1.49315D-01
      CP(3)= 6.96458D-01
      CP(4)= 3.21033D-01
      CP(5)=-1.62848D-01
      CP(6)=-6.24314D-02
      RETURN
      END
