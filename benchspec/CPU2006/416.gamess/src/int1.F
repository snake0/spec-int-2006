C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C  4 NOV 03 - TN  - ONEEI: CHANGES FOR DK
C  3 SEP 03 - MWS - SYNCHRONIZE RELWFN COMMON
C  3 JUL 03 - JMM - SUPPRESS PRINTING FOR MONTE CARLO JOBS
C 15 MAY 03 - MWS - PROVIDE ADDITIONAL MEMORY TO MCP INTEGRALS
C 17 APR 02 - MWS - SYNCH UP FRGINF COMMON
C 24 JAN 02 - STE - CONTRAM: USE HOLLERITH IN CALL TO TMOINT
C 25 OCT 01 - MK  - HSANDT, ONEEI: MODEL CORE POTENTIAL INTERFACING
C  8 OCT 01 - MWS - ONEEI: MCSCF RUNS ALWAYS DO DIPOLES HERE
C 25 JUN 01 - MWS - CHANGES TO COMMON BLOCK WFNOPT
C 13 JUN 00 - DGF,TN - IMPLEMENT INTERNALLY UNCONTRACTED RESC METHOD
C 15 AUG 00 - DGF - HANDTR: FIX L-SHELL PVP INTEGRAL BUG
C  1 MAY 00 - MWS - DERCHK: RAMAN RUN TYPE INVOLVES ANALYTIC GRADIENT
C 10 APR 00 - MWS - REMOVE STATIC MEMORY FROM COSMO COMMONS
C 25 MAR 00 - DGF - SMALL CHANGES FOR RELATIVISTIC EFFECTS
C  8 MAR 00 - KKB,LNB - COSMO MODIFICATIONS
C 16 FEB 00 - VK  - ONEEI: WRITE HAMILTONIAN WITHOUT EFP CONTRIB. TO DAF
C 10 JAN 00 - DGF - FINISH ADDING NESC (RELATIVISTIC METHOD OF K. DYALL)
C 21 DEC 99 - TN,DGF - ADD RESC METHOD (RELATIVISTIC CORRECTIONS),
C                   HSANDT: LZ INTS FOR LINEAR MOLECULES AND SOC RUNS
C 13 MAR 99 - MWS - ONEEI: COMPUTE ECP INTEGER STORAGE CORRECTLY
C  1 DEC 98 - BMB - UPDATED ECPINT CALL FOR NEW ECP CODE
C 20 NOV 97 - JMT - HSANDT: ZERO ALL 1E- STORAGE DURING MOROKUMA
C 28 SEP 97 - MWS - CONVERT PARALLEL CALLS FROM TCGMSG TO DDI
C 20 DEC 96 - HPP - ONEEI: CHANGE DIPOLE INT TIMING OUTPUT
C 18 DEC 96 - JHJ - ONEEI: VCLR DAF 89.
C  3 JAN 96 - MWS - NEW DRIVER ROUTINE ONEEI TO MANAGE 1E- INTS
C 14 SEP 95 - XL  - STANDV: INSERT CALLS TO SCREENED NUCLEAR ATTRACTION
C 31 MAY 95 - MWS - ADD DERCHK ROUTINE
C  1 FEB 95 - WC  - CHANGES FOR MOROKUMA DECOMPOSITION
C 29 DEC 94 - TLW - STANDV: ADD CALL TO EFLD1
C 21 NOV 94 - MWS - REMOVE FTNCHEK WARNINGS
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C 27 MAY 94 - PND - STANDV: CORRECTION TO POLARIZABLE POINTS INTEGRALS
C  5 APR 94 - MWS - STANDV: MOVE CALL TO ZRFINT
C  3 NOV 93 - MH  - STANDV: CHARMM EXTERNAL CHARGE PERT. ADDED
C  2 AUG 93 - BMB - MODIFIED STANDV FOR F-FUNCTIONS IN ECPINT CALL
C  1 APR 93 - PND - COMPUTE FRAGMENT CENTER OF MASS
C  4 MAR 93 - JHJ - COMPUTE SCRF CONTRIBUTIONS
C  4 JUN 92 - TLW - STANDV: PARALLELIZE
C 20 MAR 92 - MWS - ALLOCATE DYNAMIC MEMORY FOR ECP LABEL PACKING
C 12 MAR 92 - MWS - REDIMENSION TO 500 ATOMS
C  5 MAR 92 - MWS - CHANGE KINETIC ENERGY DAF RECORD
C  7 FEB 92 - JHJ - STANDV: CALL TO MPCINT
C  7 JAN 92 - TLW - MAKE WRITES PARALLEL;ADD COMMON PAR
C 23 OCT 91 - JHJ - INTRODUCED ICALC IN COMMON /ZRFPAR/.
C 17 JUL 91 - JHJ - STANDV:IMPLEMENTED DRG'S CALL TO ZRFINT.
C  8 JUL 91 - JHJ - STANDV: MOVED CHARGE-CHARGE INTEGRALS TO EFINT.
C 12 SEP 90 - MWS - CHECK RUNS AVOID DOING ECP INTEGRALS
C 29 AUG 90 - TLW - ADDED CODE FOR F AND G FUNCTION CAPABILITIES
C 14 AUG 90 - TLW - CLEANED UP CODE TO MAKE IT READABLE
C  7 AUG 90 - TLW - ADD CF AND CG TO COMMON NSHEL
C  8 MAY 90 - DRG - CALLS TO OTHER EFPOT INTEGRAL ROUTINES ADDED AT THE
C                   END OF STANDV.  THE JUMP OVER THEM IN PROPERTY RUNS
C                   OF HONDO HAS BEEN DROPPED.
C  8 MAY 90 - DRG - EFFECTIVE POINT CHARGE INTEGRALS ADDED IN STANDV.
C                   L LIMIT SET BACK TO D IN CODING.
C 29 NOV 89 - KAN - ECP RUNS CALL ECPINT TO ADJUST 1 ELECTRON INTEGRALS
C 10 AUG 88 - MWS - MXSH,MXGSH,MXGTOT FROM 120,10,440 TO 1000,30,5000
C 22 MAY 88 - MWS - USE PARAMETERS TO DIMENSION COMMON
C 14 FEB 88 - MWS - INCREASE /ROOT/ TO 9 ROOTS
C  7 JUL 86 - MWS - SANITIZE FLOATING POINT CONSTANTS
C 16 OCT 85 - STE - USE GENERIC EXP,SQRT
C 15 MAR 85 - MWS - KILL BLOCKING LOGIC, ROUTINES SYMHS,PRTHS,PRSYML
C  2 APR 84 - STE - REPLACE WRT123 WITH RT123, /STVRT/ TO /ROOT/
C  4 NOV 83 - STE - REMOVE CALLS TO UNPACK IN SYMHS
C  4 MAY 83 - MWS - RECOMPILED
C 18 MAR 83 - MWS - WRITE K.E. INTEGRALS ON RECORD 10 OF DAF
C 23 NOV 82 - MWS - CONDITIONAL CALL TO TEXIT IN STANDV
C  2 OCT 82 - MWS,NDSU - CONVERT FOR THE IBM
C
C*MODULE INT1    *DECK DERCHK
      SUBROUTINE DERCHK(MAXDER)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL HSSEND
C
      COMMON /HSSCTL/ IHESSM,IHREP,HSSEND
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA GRAD,HESS          /8HGRADIENT,8HHESSIAN /
#else
      CHARACTER*8 :: HESS_STR
      EQUIVALENCE (HESS, HESS_STR)
      CHARACTER*8 :: GRAD_STR
      EQUIVALENCE (GRAD, GRAD_STR)
      DATA GRAD_STR,HESS_STR/"GRADIENT","HESSIAN "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA OPTMZE,SADPT,GRDXTR/8HOPTIMIZE,8HSADPOINT,8HGRADEXTR/
#else
      CHARACTER*8 :: GRDXTR_STR
      EQUIVALENCE (GRDXTR, GRDXTR_STR)
      CHARACTER*8 :: OPTMZE_STR
      EQUIVALENCE (OPTMZE, OPTMZE_STR)
      CHARACTER*8 :: SADPT_STR
      EQUIVALENCE (SADPT, SADPT_STR)
      DATA OPTMZE_STR,SADPT_STR,GRDXTR_STR/"OPTIMIZE","SADPOINT",
     * "GRADEXTR"/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA AIRC,DRC,RAMAN     /8HIRC     ,8HDRC     ,8HRAMAN   /
#else
      CHARACTER*8 :: RAMAN_STR
      EQUIVALENCE (RAMAN, RAMAN_STR)
      CHARACTER*8 :: AIRC_STR
      EQUIVALENCE (AIRC, AIRC_STR)
      CHARACTER*8 :: DRC_STR
      EQUIVALENCE (DRC, DRC_STR)
      DATA AIRC_STR,DRC_STR,RAMAN_STR/"IRC     ","DRC     ","RAMAN   "/
#endif
C
C        RETURN MAXIMUM DERIVATIVE TO BE COMPUTED BY THIS RUN
C
      MAXDER = 0
C
      IF(RUNTYP.EQ.GRAD)   MAXDER=1
      IF(RUNTYP.EQ.OPTMZE) MAXDER=1
      IF(RUNTYP.EQ.SADPT)  MAXDER=1
      IF(RUNTYP.EQ.AIRC)   MAXDER=1
      IF(RUNTYP.EQ.DRC)    MAXDER=1
      IF(RUNTYP.EQ.GRDXTR) MAXDER=1
      IF(RUNTYP.EQ.RAMAN)  MAXDER=1
C
C        ASSUME FULLY ANALYTIC, THEN CHECK FOR NUMERIC
C
      IF(RUNTYP.EQ.HESS)   MAXDER=2
C
C        CHECK TO SEE IF HESSIAN IS BEING DONE NUMERICALLY.
C        THIS ALSO PICKS UP HESS=CALC AND HSSEND=.TRUE. SUBOPTIONS.
C
      IF(IHESSM.NE.0) MAXDER=IHESSM
      RETURN
      END
C
C*MODULE INT1    *DECK DIPINT
      SUBROUTINE DIPINT(XC,YC,ZC,DBUG)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,DBUG
C
      PARAMETER (MXATM=500)
C
      COMMON /ELPROP/ ELDLOC,ELMLOC,ELPLOC,ELFLOC,
     *                IEDEN,IEMOM,IEPOT,IEFLD,MODENS,
     *                IEDOUT,IEMOUT,IEPOUT,IEFOUT,
     *                IEDINT,IEMINT,IEPINT,IEFINT
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
      COMMON /XYZPRP/ XP,YP,ZP,
     *                DIPMX,DIPMY,DIPMZ,
     *                QXX,QYY,QZZ,QXY,QXZ,QYZ,
     *                QMXX,QMYY,QMZZ,QMXY,QMXZ,QMYZ,
     *                OXXX,OXXY,OXXZ,OXYY,OYYY,OYYZ,
     *                OXZZ,OYZZ,OZZZ,OXYZ,
     *                OMXXX,OMXXY,OMXXZ,OMXYY,OMYYY,
     *                OMYYZ,OMXZZ,OMYZZ,OMZZZ,OMXYZ
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA ELMOM/8HELMOM   /
#else
      CHARACTER*8 :: ELMOM_STR
      EQUIVALENCE (ELMOM, ELMOM_STR)
      DATA ELMOM_STR/"ELMOM   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA NONE/4HNONE/
#else
      CHARACTER*4 :: NONE_STR
      EQUIVALENCE (NONE, NONE_STR)
      DATA NONE_STR/"NONE"/
#endif
C
C     ----- CALCULATE DIPOLE INTEGRALS -----
C     AT REQUESTED ORIGIN, AND SAVE ON THE DAF FILE.
C
      XP = XC
      YP = YC
      ZP = ZC
C
C         MOPAC INTEGRALS ARE NOT QUITE READY.
C         ACCORDING TO HENRY KURTZ, THERE IS A MOPAC DIPOLE
C         INTEGRAL ROUTINE PRESENT IN THE TDHF CODE.  IT SHOULD
C         BE IDENTIFIED AND CALLED AT THIS LOCATION.
C
      IF(MPCTYP.NE.NONE) THEN
         IF(MASWRK) WRITE(IW,9100)
         CALL ABRT
         STOP
      END IF
C
      L1 = NUM
      L2 = (L1*L1+L1)/2
C
      CALL VALFM (LOADFM)
      LX   = LOADFM + 1
      LY   = LX     + L2
      LZ   = LY     + L2
      LMW  = LZ     + L2
      LAST = LMW    + 225*3
      NEED = LAST - LOADFM - 1
      CALL GETFM(NEED)
C
      IEMSV = IEMOM
      IEMOM = 1
      CALL PRCALC(ELMOM,X(LX),X(LMW),3,L2)
      IEMOM = IEMSV
C
      CALL DAWRIT(IDAF,IODA,X(LX),L2,95,0)
      CALL DAWRIT(IDAF,IODA,X(LY),L2,96,0)
      CALL DAWRIT(IDAF,IODA,X(LZ),L2,97,0)
C
      IF(DBUG) THEN
         WRITE(IW,*) 'DIPOLE ORIGIN=',XC,YC,ZC
         WRITE(IW,*) 'X DIPOLE INTEGRALS'
         CALL PRTRIL(X(LX),L1)
         WRITE(IW,*) 'Y DIPOLE INTEGRALS'
         CALL PRTRIL(X(LY),L1)
         WRITE(IW,*) 'Z DIPOLE INTEGRALS'
         CALL PRTRIL(X(LZ),L1)
      END IF
C
      CALL RETFM(NEED)
      RETURN
C
 9100 FORMAT(1X,'SORRY, MOPAC DIPOLE INTEGRALS ARE NOT YET HOOKED UP')
      END
C*MODULE INT1    *DECK HSANDT
      SUBROUTINE HSANDT(H,S,T,Z,LL2,SOME,LZINT,DBUG)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION NINE,MOROKM
C
      LOGICAL SOME,DBUG,LZINT,UNCON
      LOGICAL IANDJ,NORM,DOUBLE,GOPARR,DSKWRK,MASWRK,SCREEN
C
C     COSMO CHANGES
C
      LOGICAL ISEPS, USEPS
C
      DIMENSION H(LL2),S(LL2),T(LL2),Z(LL2)
      DIMENSION SBLK(225),TBLK(225),VBLK(225),ZBLK(225),
     *          FT(225),DIJ(225),IJX(225),IJY(225),IJZ(225),
     *          XIN(125),YIN(125),ZIN(125),
     *          IX(35),IY(35),IZ(35),JX(35),JY(35),JZ(35)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
      PARAMETER (MXCHRM=1)
C
      COMMON /CHMGMS/ XCHM(MXCHRM),YCHM(MXCHRM),ZCHM(MXCHRM),
     *                DXELMM(MXCHRM),DYELMM(MXCHRM),DZELMM(MXCHRM),
     *                QCHM(MXCHRM),NCHMAT,KCHRMM
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
      COMMON /SCINP / VLAMB,SCREEN
      COMMON /STV   / XINT,YINT,ZINT,TAA,X0,Y0,Z0,
     *                XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
      COMMON /SYMIND/ TOL,II,JJ,LIT,LJT,MINI,MINJ,MAXI,MAXJ,IANDJ
      COMMON /RELWFN/ RMETHOD,QRQMT,CLIG,CLIG2,QRTOL,IQRORD,MODQR,NESOC,
     *                NRATOM,NUMU,NQMTR,NQRDAF,MORDA,NDARELB
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
C     COSMO CHANGES
C
      PARAMETER (MAXDEN=25*MXATM, LENABC=2000, NPPA=1082)
      COMMON /ISEPS / ISEPS,USEPS
      COMMON /COSMO3/ COSZAN(NPPA),CORZAN(3,NPPA)
      COMMON /SOLV  / FEPSI,RDS,DISEX2,COSURF(3,LENABC),SRAD(MXATM),
     *                QDEN(MAXDEN),AR(LENABC),
     *                NSPA,NPS,NPS2,NDEN,NPSPHER
      COMMON /CONV  / DENTOL,EN,ETOT,EHF,EHF0,DIFF,ITER,ICALP,ICBET
C
      PARAMETER (ZERO=0.0D+00, PT5=0.5D+00, ONE=1.0D+00, TWO=2.0D+00,
     *           THREE=3.0D+00, FIVE=5.0D+00, SEVEN=7.0D+00,
     *           NINE=9.0D+00, ELEVEN=11.0D+00,
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
      DATA MOROKM/8HMOROKUMA/
#else
      CHARACTER*8 :: MOROKM_STR
      EQUIVALENCE (MOROKM, MOROKM_STR)
      DATA MOROKM_STR/"MOROKUMA"/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA NONE,RNONE/4HNONE,8HNONE    /
#else
      CHARACTER*8 :: RNONE_STR
      EQUIVALENCE (RNONE, RNONE_STR)
      CHARACTER*4 :: NONE_STR
      EQUIVALENCE (NONE, NONE_STR)
      DATA NONE_STR,RNONE_STR/"NONE","NONE    "/
#endif
C
C     ----- COMPUTE CONVENTIONAL H, S, AND T INTEGRALS -----
C
      UNCON=RMETHOD.NE.RNONE.AND.MOD(MODQR,2).EQ.1
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
      IF(SOME  .AND.  SCREEN) WRITE(IW,9010) VLAMB
C
C     ----- MOPAC INTEGRALS ARE DONE ELSEWHERE -----
C
      IF(MPCTYP.NE.NONE) THEN
         CALL MPCINT
         RETURN
      END IF
C     COSMO CHANGES VOLKER FOR DIRECT SCF, MRZ 99
C
C     FOR DIRECT SCF, WE NEED THE DIFFERENCE OF H BETWEEN THE SCF
C     ITERATIONS IN RHFCL, SO HOLD IS COPIED INTO SECTION 87
C     AND HNEW IS IN SECTION 11 AS USUAL, BEGINNING IN SCF-CYCLE 2
C
      IF(ISEPS  .AND.  ITER.GT.0) THEN
         CALL DAREAD(IDAF,IODA,H,LL2,11,0)
         CALL DAWRIT(IDAF,IODA,H,LL2,87,0)
      END IF
C
C     ----- RESET SOME PARAMETERS FOR MOROKUMA DECOMPOSITIONS -----
C     ISAVE .EQ. 0 : SAVE S, H, AND T TO DAF 12, 11, AND 13
C     ISAVE .EQ. 1 : SAVE S, H, AND T TO DAF 12, 11, AND 13
C                    AND SAVE S AND H TO DAF 312 AND 311
C     NOTE THAT LL2 IS ALWAYS (NUM*NUM+NUM)/2,
C     L1,L2 MAY BE SMALLER THAN USUAL FOR A MONOMER IN A MOROKUMA RUN
C
      IF (RUNTYP.EQ.MOROKM) THEN
         CALL STINT1(ISTART,IEND,JSTART,LOCIJ,NATST,NATED,ISAVE,L1,L2)
      ELSE
         ISTART = 1
         IEND   = NSHELL
         JSTART = 1
         LOCIJ  = 0
         NATST  = 1
         NATED  = NAT+NCHMAT
         IF(ISEPS) NATED = NAT+NPS
         ISAVE  = 0
         L1 = NUM
         IF(UNCON) L1=NUMU
         L2 = (L1*(L1+1))/2
      END IF
C
      CALL VCLR(H,1,LL2)
      CALL VCLR(S,1,LL2)
      CALL VCLR(T,1,LL2)
      IF(LZINT) CALL VCLR(Z,1,LL2)
C
C     ----- INTIALIZE PARALLEL -----
C
      IPCOUNT = ME - 1
C
C     ----- I SHELL -----
C
      DO 720 II = ISTART,IEND
         I = KATOM(II)
         XI = C(1,I)
         YI = C(2,I)
         ZI = C(3,I)
         I1 = KSTART(II)
         I2 = I1+KNG(II)-1
         LIT = KTYPE(II)
         MINI = KMIN(II)
         MAXI = KMAX(II)
         LOCI = KLOC(II)-MINI-LOCIJ
C
C     ----- J SHELL -----
C
         DO 700 JJ = JSTART,II
C
C     ----- GO PARALLEL! -----
C
            IF (GOPARR) THEN
               IPCOUNT = IPCOUNT + 1
               IF (MOD(IPCOUNT,NPROC).NE.0) GO TO 700
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
            LOCJ = KLOC(JJ)-MINJ-LOCIJ
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
                  IF (J.LE.1) FT(IJ) = THREE
                  IF ((J.GT.1).AND.(J.LE.4)) FT(IJ) = FIVE
                  IF ((J.GT.4).AND.(J.LE.10)) FT(IJ) = SEVEN
                  IF ((J.GT.10).AND.(J.LE.20)) FT(IJ) = NINE
                  IF (J.GT.20) FT(IJ) = ELEVEN
  140          CONTINUE
  160       CONTINUE
C
            CALL VCLR(SBLK,1,IJ)
            CALL VCLR(TBLK,1,IJ)
            CALL VCLR(VBLK,1,IJ)
            IF(LZINT) CALL VCLR(ZBLK,1,IJ)
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
C     THE ONLY REASON WHY -ILZ WORKS WITH THIS DENSITY THAT ASSUMES
C     HERMITICITY IS BECAUSE <I|-ILZ|I>=0 (MOMENTUM QUENCHING).
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
                     IF ((I.EQ. 8).AND.NORM) DUM1=DUM1*SQRT3
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
C     ----- OVERLAP AND KINETIC ENERGY
C
                  TAA = SQRT(AA1)
                  T1 = -TWO*AJ*AJ*TAA
                  T2 = -PT5*TAA
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
                        XIN(JN) = XINT*TAA
                        YIN(JN) = YINT*TAA
                        ZIN(JN) = ZINT*TAA
                        NJ = J+2
                        CALL STVINT
                        XIN(JN+25) = XINT*T1
                        YIN(JN+25) = YINT*T1
                        ZIN(JN+25) = ZINT*T1
                        NJ = J-2
                        IF (NJ .GT. 0) THEN
                           CALL STVINT
                        ELSE
                           XINT = ZERO
                           YINT = ZERO
                           ZINT = ZERO
                        END IF
                        N = (J-1)*(J-2)
                        DUM = N * T2
                        XIN(JN+50) = XINT*DUM
                        YIN(JN+50) = YINT*DUM
                        ZIN(JN+50) = ZINT*DUM
                        IF(LZINT) THEN
                           NJ = J+1
                           CALL STVINT
                           XIN(JN+75) = XINT*TAA
                           YIN(JN+75) = YINT*TAA
C                          ZIN(JN+75) = ZINT*TAA
                           NJ = J-1
                           IF (NJ .GT. 0) THEN
                              CALL STVINT
                           ELSE
                              XINT = ZERO
                              YINT = ZERO
C                             ZINT = ZERO
                           END IF
                           XIN(JN+100) = XINT*TAA*NJ
                           YIN(JN+100) = YINT*TAA*NJ
C                          ZIN(JN+100) = ZINT*TAA*NJ
                        END IF
  300                CONTINUE
  320             CONTINUE
                  DO 340 I = 1,IJ
                     NX = IJX(I)
                     NY = IJY(I)
                     NZ = IJZ(I)
                     YZ = YIN(NY)*ZIN(NZ)
                     DUM = YZ*XIN(NX)
                     DUM1 = (XIN(NX+25)+XIN(NX+50))*YZ+
     *                      (YIN(NY+25)+YIN(NY+50))*XIN(NX)*ZIN(NZ)+
     *                      (ZIN(NZ+25)+ZIN(NZ+50))*XIN(NX)*YIN(NY)
                     SBLK(I) = SBLK(I) + DIJ(I)*DUM
                     TBLK(I) = TBLK(I) + DIJ(I)*(DUM*AJ*FT(I)+DUM1)
                     IF(LZINT) THEN
                        DUM2 = XIN(NX+ 75)*YIN(NY+100)
     *                       - XIN(NX+100)*YIN(NY+ 75)
                        ZBLK(I) = ZBLK(I) + DIJ(I)*DUM2*ZIN(NZ)
                     END IF
  340             CONTINUE
C
C     ----- NUCLEAR ATTRACTION
C     EL SIGUIENTE DO SOLO EN CASO NO SCREEN.
C
                  IF (.NOT.SCREEN) THEN
                     DUM = PI212*AA1
                     DO 400 I = 1,IJ
                        DIJ(I) = DIJ(I)*DUM
  400                CONTINUE
                  END IF
C
                  AAX = AA*AX
                  AAY = AA*AY
                  AAZ = AA*AZ
C
C     -NCHMAT- IS NONZERO IF THERE ARE EXTERNAL CHARGES WHICH
C     PERTURB THE SYSTEM, SUCH AS IF CHARMM IS IN USE.  NOTE
C     THAT THERE IS ALSO A NUCLEAR REPULSION TERM WHICH IS NOT
C     INCLUDED HERE, IT IS IN THE CHARMM INTERFACE CODE.
C
                  DO 460 IC = NATST,NATED
                     IF(IC.LE.NAT) THEN
                        ZNUC = -ZAN(IC)
                        CX = C(1,IC)
                        CY = C(2,IC)
                        CZ = C(3,IC)
                     ELSE
C
C     COSMO OR CHARMM POINT CHARGES
C
                        IF(ISEPS) THEN
                           ZNUC = -COSZAN(IC-NAT)
                           CX = CORZAN(1,IC-NAT)
                           CY = CORZAN(2,IC-NAT)
                           CZ = CORZAN(3,IC-NAT)
                         ELSE
                           ZNUC = -QCHM(IC-NAT)
                           CX = XCHM(IC-NAT)
                           CY = YCHM(IC-NAT)
                           CZ = ZCHM(IC-NAT)
                        END IF
                     END IF
C
C         CHECKING IF IT IS AN SCREENED CALCULATION, IF SO CALL SCR1,
C         OTHERWISE, FOLLOW THE GAMESS CODE.
C
                  IF (SCREEN) THEN
                      NN=0
                      DO 425 I=MINI,MAXI
                         L1A=JX(I)
                         M1A=JY(I)
                         N1A=JZ(I)
                         MAX=MAXJ
                         IF(IANDJ) MAX=I
                         DO 415 J=MINJ,MAX
                            NN=NN+1
                            L2B=JX(J)
                            M2B=JY(J)
                            N2B=JZ(J)
                            VAL=FDNAI(VLAMB,AI,AJ,
     *                                L1A,M1A,N1A,L2B,M2B,N2B,
     *                                XI,YI,ZI,XJ,YJ,ZJ,CX,CY,CZ)
                            VBLK(NN) = VBLK(NN) + DIJ(NN)*VAL*ZNUC
C
  415                    CONTINUE
  425                 CONTINUE
                  ELSE
                     XX = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
                     IF (NROOTS.LE.3) CALL RT123
                     IF (NROOTS.EQ.4) CALL ROOT4
                     IF (NROOTS.EQ.5) CALL ROOT5
                     MM = 0
                     DO 430 K = 1,NROOTS
                        UU = AA*U(K)
                        WW = W(K)*ZNUC
                        TT = ONE/(AA+UU)
                        TAA = SQRT(TT)
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
  410                      CONTINUE
  420                   CONTINUE
                        MM = MM+25
  430                CONTINUE
                     DO 450 I = 1,IJ
                        NX = IJX(I)
                        NY = IJY(I)
                        NZ = IJZ(I)
                        DUM = ZERO
                        MM = 0
                        DO 440 K = 1,NROOTS
                           DUM = DUM+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM)
                           MM = MM+25
  440                   CONTINUE
                        VBLK(I) = VBLK(I) + DUM*DIJ(I)
  450                CONTINUE
                   END IF
C
  460             CONTINUE
C
C     ----- END OF PRIMITIVE LOOPS -----
C
  500          CONTINUE
  520       CONTINUE
C
C     ----- COPY BLOCK INTO H-CORE, OVERLAP, AND KINETIC ENERGY MATRICES
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
                  H(JN) = TBLK(NN) + VBLK(NN)
                  S(JN) = SBLK(NN)
                  T(JN) = TBLK(NN)
                  IF(LZINT) Z(JN) = ZBLK(NN)
  600          CONTINUE
  620       CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
  700    CONTINUE
  720 CONTINUE
C
C     ----- SUM UP PARTIAL CONTRIBUTIONS IF PARALLEL -----
C
      IF (GOPARR) THEN
         CALL DDI_GSUMF(910,H,L2)
         CALL DDI_GSUMF(911,S,L2)
         CALL DDI_GSUMF(912,T,L2)
         IF(LZINT) CALL DDI_GSUMF(913,Z,L2)
      END IF
C
C     ----- SAVE H, S, AND T MATRICES ON THE DAF -----
C
      CALL DAWRIT(IDAF,IODA,H,LL2,11,0)
      CALL DAWRIT(IDAF,IODA,S,LL2,12,0)
      CALL DAWRIT(IDAF,IODA,T,LL2,13,0)
      IF(LZINT) CALL DAWRIT(IDAF,IODA,Z,LL2,379,0)
      IF (ISAVE.EQ.1) THEN
         CALL DAWRIT(IDAF,IODA,H,LL2,311,0)
         CALL DAWRIT(IDAF,IODA,S,LL2,312,0)
      END IF
C
C     ----- OPTIONAL DEBUG PRINTOUT -----
C
      IF(DBUG) THEN
         WRITE(IW,*) 'OVERLAP MATRIX'
         CALL PRTRIL(S,L1)
         WRITE(IW,*) 'BARE NUCLEUS HAMILTONIAN INTEGRALS (H=T+V)'
         CALL PRTRIL(H,L1)
         WRITE(IW,*) 'KINETIC ENERGY INTEGRALS'
         CALL PRTRIL(T,L1)
         IF(LZINT) THEN
            WRITE(IW,*) 'Z-ANGULAR MOMENTUM INTEGRALS'
            CALL PRTRIL(Z,L1)
         END IF
      END IF
      RETURN
C
 9010 FORMAT(1X,'SCREENING VALUE=',F20.5,' IS BEING USED')
      END
C
C*MODULE INT1    *DECK ONEEI
      SUBROUTINE ONEEI
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME,DBUG,GOPARR,DSKWRK,MASWRK,EFLDL,LINEAR,LZINT,UNCON,
     *        QUADRE
C
      CHARACTER*2 WORDS(3)
C
      PARAMETER (MXATM=500, MXFRG=50, MXAO=2047)
      PARAMETER (NDQ=2)
      PARAMETER (ONE=1.0D+00, ZERO=0.0D+00)
C
      COMMON /ECPDIM/ NCOEF1,NCOEF2,J1LEN,J2LEN,LLIM,NLIM,NTLIM,J4LEN
      COMMON /EFLDC / EVEC(3),EFLDL
      COMMON /EFPBUF/ POLCHG(10),NBUFMO,LBUFF(MXAO),LBF,NAPOL,IAPOL(10)
      COMMON /FMCOM / X(1)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /INTOPT/ ISCHWZ,IECP,NECP,IEFLD
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RELWFN/ RMETHOD,QRQMT,CLIG,CLIG2,QRTOL,IQRORD,MODQR,NESOC,
     *                NRATOM,NUMU,NQMTR,NQRDAF,MORDA,NDARELB
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /SIMDAT/ NACC,NREJ,IGOMIN,NRPA,IBWM,NACCT,NREJT,NRPAT,
     *                NPRTGO,IDPUNC,IGOFLG
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
      COMMON /ZRFPAR/ GZRF,FIND(3),GNUCF,EBORN,DIELEC,IZRF,ICALC
      COMMON /ZMAT  / NZMAT,NZVAR,NVAR,NSYMC,LINEAR
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DEBUG/8HDEBUG   /,DBUGME/8HINT1    /
#else
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA DEBUG_STR/"DEBUG   "/,DBUGME_STR/"INT1    "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA ANONE/8HNONE    /
#else
      CHARACTER*8 :: ANONE_STR
      EQUIVALENCE (ANONE, ANONE_STR)
      DATA ANONE_STR/"NONE    "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA ANESC/8HNESC    /,  RESC/8HRESC    /, DK/8HDK      /
#else
      CHARACTER*8 :: ANESC_STR
      EQUIVALENCE (ANESC, ANESC_STR)
      CHARACTER*8 :: RESC_STR
      EQUIVALENCE (RESC, RESC_STR)
      CHARACTER*8 :: DK_STR
      EQUIVALENCE (DK, DK_STR)
      DATA ANESC_STR/"NESC    "/,  RESC_STR/"RESC    "/, 
     * DK_STR/"DK      "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA   RMC/8HMCSCF   /
#else
      CHARACTER*8 :: RMC_STR
      EQUIVALENCE (RMC, RMC_STR)
      DATA   RMC_STR/"MCSCF   "/
#endif
      DATA WORDS/'ST','ND','RD'/
C
C     ----- DRIVER FOR ONE ELECTRON INTEGRAL COMPUTATION -----
C     AVOID PRINTING DURING DK NUMERICAL GRADIENT (-2183)
C
      SOME = MASWRK  .AND.  .NOT.(NPRINT.EQ.-5 .OR. NPRINT.EQ.-2183)
      DBUG = MASWRK  .AND.
     *       (NPRINT.EQ.3 .OR. EXETYP.EQ.DEBUG .OR. EXETYP.EQ.DBUGME)
C
      LZINT=LINEAR
      UNCON =RMETHOD.NE.ANONE.AND.MOD(MODQR  ,2).EQ.1
      QUADRE=RMETHOD.NE.ANONE.AND.MOD(MODQR/8,2).EQ.1
C
      IF(SOME) WRITE (IW,9000)
      IF(SOME) CALL TSECND(T0)
C
      L1=NUM
      IF(UNCON) L1=NUMU
      L2 = (L1*L1+L1)/2
      L3 = L1*L1
C
      CALL VALFM(LOADFM)
      LH   = LOADFM + 1
      LS   = LH     + L2
      LT   = LS     + L2
      LZ   = LT     + L2
      LWRK = LZ     + L2
      IF(.NOT.LZINT) LWRK = LZ
      LAST = LWRK   + L2
      LH0=LAST
      IF(UNCON) LAST=LH0+(NUM*NUM+NUM)/2
      LQQ=LAST
      IF(QUADRE) LAST=LQQ+L2*NDQ*2
      NEED = LAST-LOADFM-1
      CALL GETFM(NEED)
C
C     ----- ORDINARY H, S, AND T INTEGRALS -----
C
      IF(RMETHOD.EQ.ANESC) CALL FLIPBASIS(0)
      IF(UNCON) CALL FLIPBASIS(17)
C
C     QSANDT OVERWRITES THE OVERLAP AND KINETIC ENERGY INTEGRALS
C     (REPLACES THOSE WITH DOUBLE PRECISION BY TRUNCATED QUADRUPLE)
C     PRACTICALLY THERE IS ALMOST NO DIFFERENCE FOR NON-RELATIVISTIC
C     ENERGY BUT LARGE DIFFERENCE FOR RELATIVISTIC (LARGE IN INTEGRALS,
C     THESE ARE MULTIPLIED BY TINY DENSITY SO THE TOTAL ENERGY CHANGES
C     LITTLE, IN THE 12TH DIGIT(?) FOR U).
C
      CALL HSANDT(X(LH),X(LS),X(LT),X(LZ),L2,SOME,LZINT,DBUG)
      IF(QUADRE) CALL QSANDT(X(LS),X(LT),X(LQQ),X(LQQ+L2*NDQ),L2,SOME)
      IF(RMETHOD.EQ.ANESC) CALL FLIPBASIS(3)
C
      IEFF = IEFC+IEFD+IEFQ+IEFO+IEFP+IREP
      IF(SOME  .AND.
     *  (RMETHOD.NE.ANONE  .OR.  IECP.GT.0   .OR. EFLDL
     *     .OR. IZRF.GT.0  .OR.  IEFF.GT.0)) THEN
         CALL TSECND(T1)
         TCPU = T1 - T0
         T0 = T1
         WRITE(IW,9010) TCPU
      END IF
C
C     ----- RELATIVISTIC CORRECTIONS -----
C         FOR -RESC-, THESE ARE PVP AND QUASI-RELATIVISTIC INTEGRALS
C
      IF(RMETHOD.EQ.RESC) THEN
         IF(SOME) WRITE(IW,9200)
         IF(MOD(MODQR/2,2).NE.0.AND.SOME) WRITE(IW,9210)
         CALL PVPINT(X(LWRK),L2,DBUG,ISAVE)
         CALL RESCX(X(LH),X(LS),X(LT),X(LWRK),L1,L2,DBUG,ISAVE)
      END IF
      IF (RMETHOD.EQ.DK) THEN
         IF(SOME) WRITE(IW,9300) IQRORD,WORDS(IQRORD)
         IF(MOD(MODQR/2,2).NE.0.AND.SOME) WRITE(IW,9210)
         CALL PVPINT(X(LWRK),L2,DBUG,ISAVE)
C                QRELX CAN ALSO DO RESC(?)
         CALL QRELX(X(LH),X(LS),X(LT),X(LWRK),L1,L2,DBUG,ISAVE)
      END IF
      IF(UNCON) THEN
C        CALL PRTRIL(X(LH),L1)
         CALL CONTRAM(X(LH0),X(LH),L2,11,1)
         CALL SYMH(X(LH0),X(LWRK),IA)
         IF(ISAVE.EQ.1) CALL DAWRIT(IDAF,IODA,X(LH0),L2,311,0)
         CALL CONTRAM(X(LH0),X(LS),L2,12,1)
         IF(ISAVE.EQ.1) CALL DAWRIT(IDAF,IODA,X(LH0),L2,312,0)
         CALL CONTRAM(X(LH0),X(LT),L2,13,1)
         IF(LZINT) CALL CONTRAM(X(LH0),X(LZ),L2,379,-1)
C
         CALL FLIPBASIS(15)
         L1=NUM
         L2 = (L1*L1+L1)/2
      ENDIF
C
C            DYALL'S CORRECTIONS
C
      IF(RMETHOD.EQ.ANESC) THEN
         IF(MASWRK) WRITE(IW,9100)
C        KEEP REPULSION INTEGRALS INTACT IN X(LH)
         CALL DAXPY(L2,-ONE,X(LT),1,X(LH),1)
C
C        THE ORDER 1,0 IN HANDTR IS IMPORTANT. FIRST, GET CORRECT T,
C        THEN V (AND H) AND S
C
         CALL FLIPBASIS(2)
         CALL HANDTR(X(LS),X(LH),X(LT),L2,1,SOME,DBUG)
         CALL FLIPBASIS(1)
         CALL HANDTR(X(LS),X(LH),X(LT),L2,0,SOME,DBUG)
         CALL FLIPBASIS(3)
      END IF
C
      IF(RMETHOD.NE.ANONE  .AND.  SOME) THEN
         CALL TSECND(T1)
         TCPU = T1 - T0
         T0 = T1
         WRITE(IW,9050) TCPU
      END IF
C
C     ----- EFFECTIVE CORE POTENTIAL INTEGRALS -----
C     THE ORDER OF MEMORY IS IMPORTANT HERE!!!
C
      IF(IECP.GT.0 .AND. IECP.NE.5) THEN
         LHECP = LWRK
         CALL VALFM(LOADFM)
         LDCF1  = LOADFM + 1
         LJLN   = LDCF1  +  NCOEF1
         LLB1   = LJLN   + (J1LEN-1)/NWDVAR+1
         LDCF4  = LLB1   + (NCOEF1*9)/NWDVAR+1
         LDCF2  = LDCF4  +  J4LEN
         LJ2N   = LDCF2  +  NCOEF2
         LLB2   = LJ2N   + (J2LEN-1)/NWDVAR+1
         LFPQR  = LLB2   + (NCOEF2*6)/NWDVAR
         LZLM   = LFPQR  +  15625
         LLMF   = LZLM   + 581
         LLMX   = LLMF   + 122/NWDVAR
         LLMY   = LLMX   + 582/NWDVAR
         LLMZ   = LLMY   + 582/NWDVAR
         LAST   = LLMY   + 582/NWDVAR
         NEED2  = LAST - LOADFM - 1
         CALL GETFM(NEED2)
         IF(EXETYP.NE.CHECK) CALL ECPINT(X(LHECP),X(LH),X(LDCF1),
     *          X(LJLN),X(LLB1),X(LDCF4),X(LDCF2),X(LJ2N),X(LLB2),
     *          X(LFPQR),X(LZLM),X(LLMF),X(LLMX),X(LLMY),X(LLMZ),L2,0)
         IF(SOME) THEN
            CALL TSECND(T1)
            TCPU = T1 - T0
            T0 = T1
            WRITE(IW,9020) TCPU
         END IF
         CALL RETFM(NEED2)
      END IF
C
C     ---- CALCULATE THE MODEL-POTENTIAL CONTRIBUTION
C
      IF(IECP.EQ.5) THEN
         IF(UNCON) THEN
           LU2 = (NUMU*NUMU+NUMU)/2
         ELSE
           LU2=L2
         ENDIF
         CALL VCLR(X(LH),1,L2)
         CALL DAWRIT(IDAF,IODA,X(LH),L2,76,0)
         CALL MCPINT(X(LWRK),X(LH),LU2)
         CALL VCLR(X(LH),1,L2)
         CALL DAWRIT(IDAF,IODA,X(LH),L2,77,0)
         CALL VALFM(LOADFM)
         LSMP = LOADFM + 1
         LAST = LSMP   + L3
         NDMCP = LAST - LOADFM - 1
         CALL GETFM(NDMCP)
         CALL MCPPRO(X(LWRK),X(LH),X(LSMP),LU2,L1)
         CALL RETFM(NDMCP)
         IF(SOME) THEN
            CALL TSECND(T1)
            TCPU = T1 - T0
            T0 = T1
            WRITE(IW,9051) TCPU
         END IF
      END IF
C
C     ----- DIPOLE INTEGRALS, AT COORDINATE ORIGIN -----
C     THE REASONING BEHIND DOING DIPOLE INTEGRALS FOR MCSCF IS IN CASE
C     SOMEONE DOES STATE-AVERAGED MCSCF ON DIFFERENT ENERGIES SO THAT
C     THE PROPERTY CODE SKIPS OUT, BUT WHERE ONE MIGHT WANT TO DO A
C     BOYS LOCALIZATION THAT REQUIRES THESE INTEGRALS.  IT IS HARD TO
C     TEST FOR THIS SOMEWHAT ODD CONDITION, SO JUST DO THE INTEGRALS.
C
      IF(EFLDL  .OR.  IZRF.EQ.1  .OR.  SCFTYP.EQ.RMC) THEN
         CALL DIPINT(ZERO,ZERO,ZERO,DBUG)
         IF(SOME) THEN
            CALL TSECND(T1)
            TCPU = T1 - T0
            T0 = T1
            WRITE(IW,9030) TCPU
         END IF
      END IF
C
C     ----- ADD OPTIONAL ELECTRIC FIELD CONTRIBUTION TO H -----
C
      IF(EFLDL) THEN
         LMUX = LS
         LMUY = LT
         LMUZ = LWRK
         CALL EFLD1(X(LMUX),X(LMUY),X(LMUZ),X(LH),L2,SOME)
      END IF
C
C     ----- CALCULATE CENTER OF MASS WITH FRAGMENTS ----
C
      IF(NFRG.GT.0) CALL EFCM
C
C     ----- EFFECTIVE FRAGMENT INTEGRALS -----
C           CHARGE-CHARGE INTEGRALS
C           CHARGE-DIPOLE INTEGRALS
C           CHARGE-QUADRUPOLE INTEGRALS
C           CHARGE-OCTUPOLE INTEGRALS
C           CHARGE-POLARIZABLE POINTS INTEGRALS
C           CHARGE-REPULSIVE POTENTIAL INTEGRALS
C
      IF(EXETYP.EQ.CHECK) GO TO 800
      IF (NFRG.GT.0) THEN
         CALL VCLR(X(LH),1,L2)
         CALL DAWRIT(IDAF,IODA,X(LH),L2,89,0)
      END IF
      IF(NBUFMO.GT.0)THEN
        CALL DAREAD(IDAF,IODA,X(LH),L2,11,0)
        CALL DAWRIT(IDAF,IODA,X(LH),L2,319,0)
        CALL VCLR(X(LH),1,L2)
      END IF
      IF(IEFC.EQ.1) CALL EFCINT(X(LWRK),X(LH))
      IF(IEFD.EQ.1) CALL EFDINT(X(LWRK),X(LH))
      IF(IEFQ.EQ.1) CALL EFQINT(X(LWRK),X(LH))
      IF(IEFO.EQ.1) CALL EFOINT(X(LWRK),X(LH))
      IF(IEFP.EQ.1) CALL POLINT(X(LWRK),X(LS),X(LT),L2)
      IF(IREP.EQ.1) CALL REPINT(X(LWRK),X(LH))
      IF(IEFF.GT.0  .AND.  SOME) THEN
         CALL TSECND(T1)
         TCPU = T1 - T0
         T0 = T1
         WRITE(IW,9040) TCPU
      END IF
C
C     ----- DONE WITH INTEGRALS -----
C     AVOID PRINTING TIMING INFO DURING MONTE CARLO JOBS.
C
  800 CONTINUE
      CALL RETFM(NEED)
      IF(MASWRK .AND. NPRTGO.NE.2  .AND.  NPRINT.NE.-2183) THEN
         WRITE(IW,9090)
         CALL TIMIT(1)
      END IF
      RETURN
C
 9000 FORMAT(/10X,20("*")/10X,'1 ELECTRON INTEGRALS'/10X,20("*"))
 9010 FORMAT(1X,'TIME TO DO ORDINARY INTEGRALS=',F10.2)
 9020 FORMAT(1X,'TIME TO DO      ECP INTEGRALS=',F10.2)
 9030 FORMAT(1X,'TIME TO DO   DIPOLE INTEGRALS=',F10.2)
 9040 FORMAT(1X,'TIME TO DO FRAGMENT INTEGRALS=',F10.2)
 9050 FORMAT(1X,'TIME TO DO  RELATIVISTIC INTS=',F10.2)
 9051 FORMAT(1X,'TIME TO DO      MCP INTEGRALS=',F10.2)
 9090 FORMAT(1X,'...... END OF ONE-ELECTRON INTEGRALS ......')
 9100 FORMAT(/1X,56("-")/
     *      5X,'NORMALISED ELIMINATION OF SMALL COMPONENT (NESC)',/,
     *      5X,'      CODED BY DMITRI FEDOROV, BASED UPON:'/
     *      5X,'        K. G. DYALL JCP 100, 2118 (1994)',/,
     *      5X,'        K. G. DYALL JCP 106, 9618 (1997)',/,
     *      3X,'K. G. DYALL AND T. ENEVOLDSEN, JCP 111, 10000 (1999)',/,
     *      1X,56(1H-)/)
 9200 FORMAT(/1X,70("-")/
     *   5X,'    RELATIVISTIC ELIMINATION OF SMALL COMPONENT (RESC)'/
     *   5X,'       T.NAKAJIMA, K.HIRAO, CPL 302 (1999) 383-391'/
     *   5X,'D.G.FEDOROV, T.NAKAJIMA, K.HIRAO, CPL, 335 (2001) 183-187',
     *  /1X,70(1H-)/)
 9210 FORMAT(/1X,'HONDO STYLE RESOLUTION OF THE IDENTITY IS USED.',/)
 9300 FORMAT(/1X,66("-")/
     *    1X,I1,A2,' ORDER DOUGLAS-KROLL-HESS RELATIVISTIC ONE-',
     *       'ELECTRON HAMILTONIAN'/
     *    12X,'CODED BY TAKAHITO NAKAJIMA AND DMITRI FEDOROV'/
     *    1X,66(1H-)/)
      END
C*MODULE INT1    *DECK STVINT
      SUBROUTINE STVINT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION H(28),W(28),MIN(7),MAX(7)
C
      COMMON /HERMIT/ H1,H2(2),H3(3),H4(4),H5(5),H6(6),H7(7)
      COMMON /WERMIT/ W1,W2(2),W3(3),W4(4),W5(5),W6(6),W7(7)
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
C
      EQUIVALENCE (H(1),H1),(W(1),W1)
C
      PARAMETER (ZERO=0.0D+00)
C
      DATA MIN /1,2,4,7,11,16,22/
      DATA MAX /1,3,6,10,15,21,28/
C
C     ----- GAUSS-HERMITE QUADRATURE USING MINIMUM POINT FORMULA -----
C
      XINT = ZERO
      YINT = ZERO
      ZINT = ZERO
      NPTS = (NI+NJ-2)/2+1
      IMIN = MIN(NPTS)
      IMAX = MAX(NPTS)
      DO 300 I = IMIN,IMAX
         DUM = W(I)
         PX = DUM
         PY = DUM
         PZ = DUM
         DUM = H(I)*T
         PTX = DUM+X0
         PTY = DUM+Y0
         PTZ = DUM+Z0
         AX = PTX-XI
         AY = PTY-YI
         AZ = PTZ-ZI
         BX = PTX-XJ
         BY = PTY-YJ
         BZ = PTZ-ZJ
         GO TO (160,150,140,130,120,110,100),NI
  100       PX = PX*AX
            PY = PY*AY
            PZ = PZ*AZ
  110       PX = PX*AX
            PY = PY*AY
            PZ = PZ*AZ
  120       PX = PX*AX
            PY = PY*AY
            PZ = PZ*AZ
  130       PX = PX*AX
            PY = PY*AY
            PZ = PZ*AZ
  140       PX = PX*AX
            PY = PY*AY
            PZ = PZ*AZ
  150       PX = PX*AX
            PY = PY*AY
            PZ = PZ*AZ
  160       GO TO (270,260,250,240,230,220,210,200),NJ
  200          PX = PX*BX
               PY = PY*BY
               PZ = PZ*BZ
  210          PX = PX*BX
               PY = PY*BY
               PZ = PZ*BZ
  220          PX = PX*BX
               PY = PY*BY
               PZ = PZ*BZ
  230          PX = PX*BX
               PY = PY*BY
               PZ = PZ*BZ
  240          PX = PX*BX
               PY = PY*BY
               PZ = PZ*BZ
  250          PX = PX*BX
               PY = PY*BY
               PZ = PZ*BZ
  260          PX = PX*BX
               PY = PY*BY
               PZ = PZ*BZ
  270          XINT = XINT+PX
               YINT = YINT+PY
               ZINT = ZINT+PZ
  300 CONTINUE
      RETURN
      END
C*MODULE INT1    *DECK HANDTR
      SUBROUTINE HANDTR(H,S,T,LL2,MODUS,SOME,DBUG)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION NINE,MOROKM
C
      LOGICAL SOME,DBUG
      LOGICAL IANDJ,NORM,DOUBLE,GOPARR,DSKWRK,MASWRK,SCREEN
C
      DIMENSION H(LL2),S(LL2),T(LL2),IEXTRA(4),JEXTRA(4)
      DIMENSION TBLK(225),VBLK(225),FT(225),DIJ(225),
     *          IJX(225),IJY(225),IJZ(225),
     *          XIN(175),YIN(175),ZIN(175),XIND(175),YIND(175),ZIND(175)
     *         ,IX(35),IY(35),IZ(35),JX(35),JY(35),JZ(35)
C
      PARAMETER (ZERO=0.0D+00, PT5=0.5D+00, ONE=1.0D+00, TWO=2.0D+00,
     *           THREE=3.0D+00,FOUR=4.0D+00,FIVE=5.0D+00,SEVEN=7.0D+00,
     *           NINE=9.0D+00, ELEVEN=11.0D+00,
     *           PI212=1.1283791670955D+00, SQRT3=1.73205080756888D+00,
     *           SQRT5=2.23606797749979D+00, SQRT7=2.64575131106459D+00,
     *           RLN10=2.30258D+00)
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500, MXCHRM=1)
C
      COMMON /CHMGMS/ XCHM(MXCHRM),YCHM(MXCHRM),ZCHM(MXCHRM),
     *                DXELMM(MXCHRM),DYELMM(MXCHRM),DZELMM(MXCHRM),
     *                QCHM(MXCHRM),NCHMAT,KCHRMM
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
      COMMON /SCINP / VLAMB,SCREEN
      COMMON /STV   / XINT,YINT,ZINT,TAA,X0,Y0,Z0,
     *                XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
      COMMON /SYMIND/ TOL,II,JJ,LIT,LJT,MINI,MINJ,MAXI,MAXJ,IANDJ
      COMMON /RELWFN/ RMETHOD,QRQMT,CLIG,CLIG2,QRTOL,IQRORD,MODQR,NESOC,
     *                NRATOM,NUMU,NQMTR,NQRDAF,MORDA,NDARELB
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
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
      DATA MOROKM/8HMOROKUMA/,IEXTRA/-1,-1,1,1/,JEXTRA/-1,1,-1,1/
#else
      CHARACTER*8 :: MOROKM_STR
      EQUIVALENCE (MOROKM, MOROKM_STR)
      DATA MOROKM_STR/"MOROKUMA"/,IEXTRA/-1,-1,1,1/,JEXTRA/-1,1,-1,1/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA NONE/4HNONE/
#else
      CHARACTER*4 :: NONE_STR
      EQUIVALENCE (NONE, NONE_STR)
      DATA NONE_STR/"NONE"/
#endif
C
C     ----- COMPUTE RELATIVISTIC (DOUGLAS-KROUL) CORRECTIONS
C           TO ONE-ELECTRON INTEGRALS (H,S AND T)
C
C     MODUS= 0 CALCULATE CORRECTION TO 1E INTERALS AND OVERLAPS
C              (IE BWB = B(DELTA*V*DELTA)B AND BTB)
C            1 CALCULATE CORRECTION TO KINETIC INTEGRALS
C              (IE -(A-B)T(A-B))
C
      FSC=ONE/CLIG
      FSC2=FSC*FSC/TWO
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
      IF(SOME  .AND.  SCREEN) WRITE(IW,9010) VLAMB
C
C     ----- MOPAC INTEGRALS ARE DONE ELSEWHERE -----
C
      IF(MPCTYP.NE.NONE) THEN
         CALL MPCINT
         RETURN
      END IF
C
C     ----- RESET SOME PARAMETERS FOR MOROKUMA DECOMPOSITIONS -----
C     ISAVE .EQ. 0 : SAVE S, H, AND T TO DAF 12, 11, AND 13
C     ISAVE .EQ. 1 : SAVE S, H, AND T TO DAF 12, 11, AND 13
C                    AND SAVE S AND H TO DAF 312 AND 311
C     NOTE THAT LL2 IS ALWAYS (NUM*NUM+NUM)/2,
C     L1,L2 MAY BE SMALLER THAN USUAL FOR A MONOMER IN A MOROKUMA RUN
C
      IF (RUNTYP.EQ.MOROKM) THEN
         CALL STINT1(ISTART,IEND,JSTART,LOCIJ,NATST,NATED,ISAVE,L1,L2)
      ELSE
         ISTART = 1
         IEND   = NSHELL
         JSTART = 1
         LOCIJ  = 0
         NATST  = 1
         NATED  = NAT+NCHMAT
         ISAVE  = 0
         L1 = NUM
         L2 = (NUM*(NUM+1))/2
      END IF
C
      CALL VCLR(H,1,LL2)
C     CALL VCLR(S,1,LL2)
      CALL VCLR(T,1,LL2)
C
C     ----- INTIALIZE PARALLEL -----
C
      IPCOUNT = ME - 1
C
C     ----- I SHELL -----
C
      DO 720 II = ISTART,IEND
         I = KATOM(II)
         XI = C(1,I)
         YI = C(2,I)
         ZI = C(3,I)
         I1 = KSTART(II)
         I2 = I1+KNG(II)-1
         LIT = KTYPE(II)
         MINI = KMIN(II)
         MAXI = KMAX(II)
         LOCI = KLOC(II)-MINI-LOCIJ
C
C     ----- J SHELL -----
C
         DO 700 JJ = JSTART,II
C
C     ----- GO PARALLEL! -----
C
            IF (GOPARR) THEN
               IPCOUNT = IPCOUNT + 1
               IF (MOD(IPCOUNT,NPROC).NE.0) GO TO 700
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
            LOCJ = KLOC(JJ)-MINJ-LOCIJ
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
                  IF (J.LE.1) FT(IJ) = THREE
                  IF ((J.GT.1).AND.(J.LE.4)) FT(IJ) = FIVE
                  IF ((J.GT.4).AND.(J.LE.10)) FT(IJ) = SEVEN
                  IF ((J.GT.10).AND.(J.LE.20)) FT(IJ) = NINE
                  IF (J.GT.20) FT(IJ) = ELEVEN
  140          CONTINUE
  160       CONTINUE
C
            DO 180 I = 1,IJ
               TBLK(I) = ZERO
               VBLK(I) = ZERO
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
C     ----- J PRIMITIVE
C
C              IF (IANDJ) JGMAX = IG
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
C                 DOUBLE=IANDJ.AND.IG.NE.JG
                  DOUBLE=.FALSE.
                  MAX = MAXJ
                  NN = 0
                  DUM1 = ZERO
                  DUM2 = ZERO
                  DO 220 I = MINI,MAXI
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
C     ----- OVERLAP AND KINETIC ENERGY
C
                  TAA = SQRT(AA1)
                  T1 = -TWO*AJ*AJ*TAA
                  T2 = -PT5*TAA
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
                        XIN(JN) = XINT*TAA
                        YIN(JN) = YINT*TAA
                        ZIN(JN) = ZINT*TAA
                        NJ = J+2
                        CALL STVINT
                        XIN(JN+25) = XINT*T1
                        YIN(JN+25) = YINT*T1
                        ZIN(JN+25) = ZINT*T1
                        NJ = J-2
                        IF (NJ .GT. 0) THEN
                           CALL STVINT
                        ELSE
                           XINT = ZERO
                           YINT = ZERO
                           ZINT = ZERO
                        END IF
                        N = (J-1)*(J-2)
                        DUM = N * T2
                        XIN(JN+50) = XINT*DUM
                        YIN(JN+50) = YINT*DUM
                        ZIN(JN+50) = ZINT*DUM
  300                CONTINUE
  320             CONTINUE
                  DO 340 I = 1,IJ
                     NX = IJX(I)
                     NY = IJY(I)
                     NZ = IJZ(I)
                     YZ = YIN(NY)*ZIN(NZ)
                     DUM = YZ*XIN(NX)
                     DUM1 = (XIN(NX+25)+XIN(NX+50))*YZ+
     *                      (YIN(NY+25)+YIN(NY+50))*XIN(NX)*ZIN(NZ)+
     *                      (ZIN(NZ+25)+ZIN(NZ+50))*XIN(NX)*YIN(NY)
C                    SBLK(I) = SBLK(I) + DIJ(I)*DUM
                     TBLK(I) = TBLK(I) + DIJ(I)*(DUM*AJ*FT(I)+DUM1)
  340             CONTINUE
                  IF(MODUS.NE.0) GOTO 500
C
C     ----- NUCLEAR ATTRACTION
C     EL SIGUIENTE DO SOLO EN CASO NO SCREEN.
C
                  IF (.NOT.SCREEN) THEN
                     DUM = PI212*AA1
                     DO 400 I = 1,IJ
                        DIJ(I) = DIJ(I)*DUM
  400                CONTINUE
                  END IF
C
                  AAX = AA*AX
                  AAY = AA*AY
                  AAZ = AA*AZ
C
C     -NCHMAT- IS NONZERO IF THERE ARE EXTERNAL CHARGES WHICH
C     PERTURB THE SYSTEM, SUCH AS IF CHARMM IS IN USE.  NOTE
C     THAT THERE IS ALSO A NUCLEAR REPULSION TERM WHICH IS NOT
C     INCLUDED HERE, IT IS IN THE CHARMM INTERFACE CODE.
C
                  DO 460 IC = NATST,NATED
                     IF(IC.LE.NAT) THEN
                        ZNUC = -ZAN(IC)
                        CX = C(1,IC)
                        CY = C(2,IC)
                        CZ = C(3,IC)
                     ELSE
                        ZNUC = -QCHM(IC-NAT)
                        CX = XCHM(IC-NAT)
                        CY = YCHM(IC-NAT)
                        CZ = ZCHM(IC-NAT)
                     END IF
                     XX = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
                     DO 6666 KK=1,4
                     IEX=IEXTRA(KK)
                     JEX=JEXTRA(KK)
                     NROOTS = (LIT+LJT+IEX+JEX-2)/2+1
                     IF (NROOTS.LE.3) CALL RT123
                     IF (NROOTS.EQ.4) CALL ROOT4
                     IF (NROOTS.EQ.5) CALL ROOT5
                     IF (NROOTS.GE.6) CALL ROOT6
                     MM = 0
                     DO 430 K = 1,NROOTS
                        UU = AA*U(K)
                        WW = W(K)*ZNUC
                        TT = ONE/(AA+UU)
                        TAA = SQRT(TT)
                        X0 = (AAX+UU*CX)*TT
                        Y0 = (AAY+UU*CY)*TT
                        Z0 = (AAZ+UU*CZ)*TT
                        IN = -5+MM
                        DO 420 I = 1,LIT
                           IN = IN+5
                           DO 410 J = 1,LJT
                              JN = IN+J
                              NI = I
                              NJ = J
                              CALL STVINT
                              XIN(JN) = XINT
                              YIN(JN) = YINT
                              ZIN(JN) = ZINT*WW
                              NI = I + IEX
                              NJ = J + JEX
                              IF(NI.LE.0.OR.NJ.LE.0) THEN
                                 XIND(JN) = ZERO
                                 YIND(JN) = ZERO
                                 ZIND(JN) = ZERO
                              ELSE
                              CALL STVINT
                              IF(KK.EQ.1) DERFAC=NI*NJ
                              IF(KK.EQ.2) DERFAC=-TWO*NI*AJ
                              IF(KK.EQ.3) DERFAC=-TWO*NJ*AI
                              IF(KK.EQ.4) DERFAC=FOUR*AI*AJ
                              XIND(JN) = XINT*DERFAC
                              YIND(JN) = YINT*DERFAC
                              ZIND(JN) = ZINT*DERFAC*WW
                              END IF
  410                      CONTINUE
  420                   CONTINUE
                        MM = MM+25
  430                CONTINUE
                     DO 450 I = 1,IJ
                        NX = IJX(I)
                        NY = IJY(I)
                        NZ = IJZ(I)
                        DUM = ZERO
                        MM = 0
                        DO 440 K = 1,NROOTS
                           DUM = DUM+XIND(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM)
     *                              +XIN(NX+MM)*YIND(NY+MM)*ZIN(NZ+MM)
     *                              +XIN(NX+MM)*YIN(NY+MM)*ZIND(NZ+MM)
                           MM = MM+25
  440                   CONTINUE
                        VBLK(I) = VBLK(I) + DUM*DIJ(I)
  450                CONTINUE
 6666                CONTINUE
C
  460             CONTINUE
C
C     ----- END OF PRIMITIVE LOOPS -----
C
  500          CONTINUE
  520       CONTINUE
C
C     ----- COPY BLOCK INTO H-CORE, OVERLAP, AND KINETIC ENERGY MATRICES
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
                  H(JN) = VBLK(NN)
                  T(JN) = TBLK(NN)
  600          CONTINUE
  620       CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
  700    CONTINUE
  720 CONTINUE
C
C     ----- SUM UP PARTIAL CONTRIBUTIONS IF PARALLEL -----
C
      IF (GOPARR) THEN
         CALL DDI_GSUMF(910,H,L2)
C        CALL DDI_GSUMF(911,S,L2)
         CALL DDI_GSUMF(912,T,L2)
      END IF
C
C     ----- OPTIONAL DEBUG PRINTOUT -----
C
      IF(DBUG) THEN
         IF(MODUS.EQ.0) THEN
           WRITE(IW,*) 'UNSCALED RELATIVISTIC CORRECTION TO REP. INTS'
           CALL PRTRIL(H,L1)
           WRITE(IW,*) 'UNSCALED RELATIVISTIC CORRECTION TO OVERLAPS'
           CALL PRTRIL(T,L1)
         ELSE
           WRITE(IW,*) 'UNSCALED RELATIVISTIC CORRECTION TO KIN. INTS'
           CALL PRTRIL(T,L1)
         END IF
      END IF
C
C     ----- SAVE H, S, AND T MATRICES ON THE DAF -----
C
      IF(MODUS.EQ.0) THEN
C
C        S CONTAINS NONRELATIVISTIC V
C        ADD RELATIVISTIC CORRECTION TO IT, THEN ADD CORRECTED T
C
         CALL DAXPY(LL2,FSC2/TWO,H,1,S,1)
         CALL DAREAD(IDAF,IODA,H,LL2,13,0)
         CALL DAXPY(LL2,ONE,H,1,S,1)
         CALL DAWRIT(IDAF,IODA,S,LL2,11,0)
         IF (ISAVE.EQ.1) CALL DAWRIT(IDAF,IODA,S,LL2,311,0)
         IF(DBUG) THEN
           WRITE(IW,*) 'RELATIVISTICALLY CORRECTED BARE NUCLEUS (T+V)'
           CALL PRTRIL(S,L1)
         ENDIF
C
C        T CONTAINS CORRECTION TO S
C        READ IN S, ADD THE CORRECTION
C
         CALL DAREAD(IDAF,IODA,S,LL2,12,0)
         CALL DAXPY(LL2,FSC2,T,1,S,1)
         CALL DAWRIT(IDAF,IODA,S,LL2,12,0)
         IF (ISAVE.EQ.1) CALL DAWRIT(IDAF,IODA,S,LL2,312,0)
         IF(DBUG) THEN
           WRITE(IW,*) 'RELATIVISTICALLY CORRECTED OVERLAPS'
           CALL PRTRIL(S,L1)
         ENDIF
      ELSE
C
C        T CONTAINS CORRECTION TO T. READ IN T, ADD THE CORRECTION
C
         CALL DAREAD(IDAF,IODA,H,LL2,13,0)
         CALL DAXPY(LL2,-ONE,T,1,H,1)
C
C        EVALUATED OVER A-B, SO FSC IS NOT BE MULTIPLIED BY.
C
         CALL DAWRIT(IDAF,IODA,H,LL2,13,0)
         IF(DBUG) THEN
           WRITE(IW,*) 'RELATIVISTICALLY CORRECTED KINETIC INTEGRALS'
           CALL PRTRIL(H,L1)
         ENDIF
      END IF
      RETURN
C
 9010 FORMAT(1X,'SCREENING VALUE=',F20.5,' IS BEING USED')
      END
C*MODULE INT1    *DECK CONTRAM
      SUBROUTINE CONTRAM(A,AU,LU2,NREC,IHERM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MXATM=500)
C
      DIMENSION A(*),AU(*)
C
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /RELWFN/ RMETHOD,QRQMT,CLIG,CLIG2,QRTOL,IQRORD,MODQR,NESOC,
     *                NRATOM,NUMU,NQMTR,NQRDAF,MORDA,NDARELB
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA LZ/4H LZ /
#else
      CHARACTER*4 :: LZ_STR
      EQUIVALENCE (LZ, LZ_STR)
      DATA LZ_STR/" LZ "/
#endif
C
C     TRANSFORM A TRIANGULAR MATRIX AU (NUMU,NUMU) IN THE INTERNALLY
C     UNCONTRACTED BASIS SET INTO CONTRACTED BASIS SET, SAVE THE RESULT
C     ON DISK (FOR POSITIVE NREC) AND COPY THE RESULT BACK TO THE
C     UNCONTRACTED ARRAY
C     IHERM=1 FOR HERMITIAN AND -1 FOR ANTIHERMITIAN MATRICES
C
      L1=NUM
      LU1=NUMU
      L2=(L1*L1+L1)/2
      CALL VALFM(LOADFM)
      LUU=LOADFM + 1
      LWRK=LUU+LU1*L1
      LSQ=LWRK+L1
      LAST=LSQ
      IF(IHERM.LT.0) LAST=LSQ+L1*L1
      NEED=LAST-LOADFM-1
      CALL GETFM(NEED)
      CALL DAREAD(IDAF,IODA,X(LUU),LU1*L1,NDARELB+19,0)
      IF(IHERM.GT.0) THEN
        CALL TFTRI(A,AU,X(LUU),X(LWRK),L1,LU1,LU1)
        CALL DCOPY(L2,A,1,AU,1)
      ELSE
C       IFORM=IHERM
C       IF(IHERM.LT.0) IFORM=2
        CALL TMOINT(X(LSQ),X(LUU),X(LUU),AU,LZ,2,L1,LU1,LU2,.FALSE.)
        CALL CPYSQT(X(LSQ),AU,L1,1)
        CALL DCOPY(L2,AU,1,A,1)
      ENDIF
      IF(NREC.GT.0) CALL DAWRIT(IDAF,IODA,AU,LU2,NREC,0)
C     NOTE THAT WE MUST SAVE DATA WITH THE SAME SIZE (LU2) AS IN HSANDT
C     ONLY THE FIRST L2 ELEMENTS IN AU ARE MEANINGFUL CONTRACTED DATA.
      CALL RETFM(NEED)
      RETURN
      END
