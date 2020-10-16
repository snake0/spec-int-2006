C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C  4 NOV 03 - MWS - TRANDDI: FIX ORTHOGONALIZATION IF C1 AND SPHER.HARM.
C  3 SEP 03 - GDF - TRANDDI: FIX VARIOUS MEMORY BUGS
C 15 MAY 03 - MWS - CHANGES TO HAVE NO BETA ORBS IN DDI TRANSFORMATION
C 14 JAN 03 - JI  - ADD HOOKS FOR ORMAS, TRFIN: DIRTRF FORCES DIRSCF
C 12 DEC 02 - MWS - SYNC ARGS FOR DEBUT CALL
C 20 JUN 02 - GDF - FIX CHECK JOB MEMORY CALCULATION
C 22 MAY 02 - GDF - IMPLEMENT DISTRIBUTED MEMORY PARTIAL TRANSFORMATION
C 16 FEB 02 - MWS - UNDO CHANGE OF 8 OCT 01
C 16 NOV 01 - MWS - SOFTEN CLENMO PARAMETERS IN NON-ABELIAN GROUPS
C  8 OCT 01 - MWS - TRFSYM: SET DMITRI'S IRMON ARRAY FOR SOC JOBS
C 19 SEP 01 - MWS - CONVERT MXAOCI PARAMTER TO MXAO
C  6 SEP 01 - MWS - ADD DUMMY ARGUMENTS TO NAMEIO CALL
C  1 AUG 01 - JI  - TWEAKS FOR GENERAL CI
C 25 JUN 01 - MWS - ALTER COMMON BLOCK WFNOPT
C 13 JUN 01 - HL  - TRFMCX: PAD EDCMP COMMON BLOCK
C 11 OCT 00 - JI  - TRFMCX: FIX SYMMETRY ASSIGNMENT MEMORY COMPUTATION
C 15 AUG 00 - MWS - TRFMCX: FIX DROPC=T DET PARAMS, MCSCF CALLS TRFSYM
C 28 JUL 00 - MWS - TRFMCX: CHANGE PAR.MEM ADJUSTMENT BY 1, AND TOLZ/E
C 11 JUN 00 - MWS - TRFMCX: ALWAYS ALLOCATE HONDO INTEGRAL BUFFER
C                   TRFSYM: SOFTEN SYMMETRY ASSIGNMENT TESTING
C  1 MAY 00 - MWS - IMPLEMENT NO SYM FOR TRANSF, RESTORE ORIGINAL TOLZ
C 16 FEB 00 - MWS - CHANGE CLENMO TOLZ PARAMETER
C 21 DEC 99 - DGF - NON-ABELIAN INTEGRAL TRANSFORMATION IMPLEMENTED
C  9 APR 99 - RLB - ADD FRSTTR FOR ZAPT 1ST INDEX
C 13 MAR 99 - MWS - TRFMCX: ORTHOGONALIZE ONLY FIRST -L0- ORBITALS
C 24 FEB 99 - MWS - TRFMCX: FIX ORBITAL COUNTS FOR DET. MCSCF
C  9 JAN 99 - MWS - DIRTRN: SAVE STATEMENT TO WORK AROUND XLF V5 PROBLEM
C 31 DEC 98 - MWS - TRFMCX: CIS ORBITAL COUNTING ONLY IF NEXT.EQ.0
C 12 NOV 98 - GDF - CHANGE BIT PACKING TO ISHIFT
C 27 OCT 98 - MAF - TRFMCX: ALLOW FOR USE OF SPHERICAL HARMONICS
C  6 MAY 98 - MWS - CHANGE CALLS TO DEBUT ROUTINE
C  6 JAN 98 - MWS - CHANGE RAOPEN,RAWRIT,RAREAD,RACLOS CALLS
C 28 NOV 97 - MWS - TRFMCX: CORRECT THE CALL FOR GLOBAL SUM
C 28 SEP 97 - MWS - CONVERT PARALLEL CALLS FROM TCGMSG TO DDI
C 14 AUG 97 - VAG - TRFMCX: PARTIAL TRANFORMATION IF CIS RUN
C 24 JUN 97 - MWS - TRFMCX: MAKE SURE PROPERTY RUNS INITIALIZE LABSIZ
C 21 APR 97 - MWS - TRFMCX: SAVE MO LABELS, AVOID 32 BIT MEMORY OVERFLOW
C  3 FEB 97 - MWS - ONEPFF,ONEPRF,ONETFF,ONETRF: REMOVE CRAY WARNINGS
C  8 JAN 97 - GMC - TRFMCX,TR1E,MCXTR2,TR1MC: CHANGES TO DROP MC CORES,
C                   MCXTR2,TR3MC,TR6MC: CORE ORBITAL DGEMV BUGS FIXED
C 18 DEC 96 - MWS - DIRJK,FZCORE: ADJUST CALL TO TWOEI, PASS DIMENSIONS
C 26 NOV 96 - VAG - INCREASE CI AO'S TO 768
C 19 NOV 96 - SPW - TRFMCX: FOR FROZEN CORE CI GRADIENTS DO FULL TRANSF
C 29 SEP 96 - GMC - MCXTR1,TR1E,TR5MC,TRFMCX: CHANGES TO PARALLEL TRANS
C 19 SEP 96 - MWS - TRFIN: PAY ATTENTION TO DIRSCF VALUE
C 18 JUL 96 - MWS - TRFMCX: DEBUT IS CALLED BEFORE EXCHNGE INTS
C 13 JUN 96 - VAG - ADD VARIABLE FOR CI TYPE TO SCFOPT COMMON
C 24 MAY 96 - GMC - CLEAN UP DIRECT OPTION'S IMPLEMENTATION
C 18 MAY 96 - GMC - TRFMCX,TR1MC,TR5MC: REMOVE HNDTYP AND USE DIRTRF
C 12 APR 96 - GMC - CHANGES TO ALLOW FOCAS TRANSF, FIX CORE FOCK BUG
C  5 MAR 96 - MWS - CHANGE SHELL SYMMETRY PACKING COMMONS
C 30 OCT 95 - MWS - TRFBRD: EXIT IF NOT A DUPLICATED AO LIST ON DISK
C 14 SEP 95 - MWS - PAD OUT EDCMP COMMON BLOCK
C 24 MAY 95 - MWS - TRFMCX,FZCORE,TR1MC: USE DISTRIBUTED AO INTEGRALS
C 21 APR 95 - MWS - USE CORRECT NAME FOR EXCHANGE INTEGRAL SCREENING
C 19 APR 95 - JHJ - TRFMCX: IF ZDO SKIP ORTHONORMALIZATION
C 28 MAR 95 - MWS - DIRTRN: SAVE ADDED FOR AIX COMPILER
C  4 JAN 95 - MWS - FZCORE: FIX CHECK RUNS
C 13 DEC 94 - MWS - FIX DIRECT TRANSFORMATION BUG
C 11 NOV 94 - MWS - REMOVE FTNCHEK WARNINGS
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C  5 AUG 94 - MWS - USE DOUBLE LABEL PACKING, CHANGE TWOEI ARGS
C 10 JUN 94 - MWS - DGEMV USAGE BY FRANCIS MUGUET AND CHRISTIAN THOMAS
C  1 JUN 94 - MWS - TREAT CORE ORBITALS CORRECTLY, USE THROUGHOUT GAMESS
C 20 FEB 94 - MWS - USE MACHINE DEPENDENT 16 BIT PACKING CALLS
C 25 JAN 93 - MWS - TRFSYM: TRAP ATTEMPT TO USE ASYMMETRIC MO-S
C  7 JAN 94 - MWS - TRFMCX: FIX STORAGE PROBLEM FOR ORTHOG STEP
C 18 DEC 93 - TLW,NM - NEW TRANSFORMATION ADAPTED FROM MICHEL'S HONDO,
C                   MODIFIED FOR PARALLEL COMPUTATION.
C
C*MODULE TRANS   *DECK DIRJK
      SUBROUTINE DIRJK(GHONDO,XINTS,DDIJ,BUFP,IBUF,MAXG,NSH2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL PACK2E,DIRTRF
C
      DIMENSION GHONDO(MAXG),XINTS(NSH2),DDIJ(*),BUFP(*),IBUF(*)
C
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /TRFOPT/ CUTTRF,NWDTRF,MPTRAN,ITRFAO,NOSYMT,DIRTRF
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA ANYSCF/8HDUMMY   /
#else
      CHARACTER*8 :: ANYSCF_STR
      EQUIVALENCE (ANYSCF, ANYSCF_STR)
      DATA ANYSCF_STR/"DUMMY   "/
#endif
C
C     ----- DRIVER FOR DIRECT 2E- INTEGRALS TRANSFORMATION -----
C
      CALL DEBUT(INTG76,.FALSE.,DUMMY,DUMMY,DUMMY,NINTMX,0,DIRTRF)
C
      CALL TWOEI(ANYSCF,.FALSE.,.FALSE.,.TRUE.,.FALSE.,
     *           INTG76,.TRUE.,NINT,NSCHWZ,1,1,
     *           BUFP,DUMMY,IBUF,NINTMX,
     *           XINTS,NSH2,GHONDO,MAXG,DDIJ,
     *           IDUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,1)
      RETURN
      END
C
C*MODULE TRANS   *DECK DIRTRN
      SUBROUTINE DIRTRN(IEXCH,POPLE,BUFP,IX,NINTMX,GHONDO,ICONT,NINT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL IANDJ,KANDL,SAME,OUT,POPLE
C
      DIMENSION BUFP(*),IX(*),GHONDO(*)
C
      PARAMETER (MXSH=1000, MXGTOT=5000)
C
      COMMON /FLIPS / IB(4,3)
      COMMON /GOUT  / GPOPLE(768),NORG
      COMMON /INTDEX/ IJGT(225),IJX(225),IJY(225),IJZ(225),IK(225),
     *                KLGT(225),KLX(225),KLY(225),KLZ(225)
      COMMON /MISC  / IANDJ,KANDL,SAME
      COMMON /NSHEL / EXX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /PCKLAB/ LABSIZ
      COMMON /SHLG70/ ISH,JSH,KSH,LSH,IJKLXX(4)
      COMMON /SHLNOS/ QQ4,LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJ,KL,IJKL
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
      DIMENSION IBPOP(4,4)
C
      DATA IBPOP/0,0,0,0,64,16,4,1,128,32,8,2,192,48,12,3/
      DATA HALF /0.5D+00/
C
C-NEXT STATEMENT IS FOR VARIOUS IBM XLF 3.X AND 5.X COMPILERS-
C
      SAVE IJN,KLN,IBB,JBB,KBB,LBB
C
C     ----- PACK THE 4 INDICES OF INTEGRAL INTO ONE WORD
C           WRITE LABEL + INTEGRAL ON BUFP AND IX
C           BEFORE SENDING INTEGRALS TO ROUTINE ONEIDX
C
      IF(POPLE) THEN
         IANDJ = ISH.EQ.JSH
         KANDL = KSH.EQ.LSH
         SAME  = ISH.EQ.KSH  .AND.  JSH.EQ.LSH
         IBB  = IB(1,IEXCH)
         JBB  = IB(2,IEXCH)
         KBB  = IB(3,IEXCH)
         LBB  = IB(4,IEXCH)
         MINI = KMIN(ISH)
         MINJ = KMIN(JSH)
         MINK = KMIN(KSH)
         MINL = KMIN(LSH)
         MAXI = KMAX(ISH)
         MAXJ = KMAX(JSH)
         MAXK = KMAX(KSH)
         MAXL = KMAX(LSH)
         LOCI = KLOC(ISH)-MINI
         LOCJ = KLOC(JSH)-MINJ
         LOCK = KLOC(KSH)-MINK
         LOCL = KLOC(LSH)-MINL
      END IF
C
      IJN = 0
      JMAX = MAXJ
      DO 260 I = MINI,MAXI
         IF (IANDJ) JMAX = I
         DO 240 J = MINJ,JMAX
            IJN = IJN+1
            IF(POPLE) THEN
               N1 = IBPOP(IBB,I)+IBPOP(JBB,J)+1+NORG
            ELSE
               N1 = IJGT(IJN)
            END IF
            LMAX = MAXL
            KLN = 0
            DO 220 K =  MINK,MAXK
               IF (KANDL) LMAX = K
               DO 200 L = MINL,LMAX
                  KLN = KLN+1
                  IF (SAME .AND. KLN .GT. IJN) GO TO 240
                  IF(POPLE) THEN
                     NN = N1+IBPOP(KBB,K)+IBPOP(LBB,L)
                     VAL = GPOPLE(NN)
                  ELSE
                     NN = N1+KLGT(KLN)
                     VAL = GHONDO(NN)
                  END IF
C
                  IF (ABS(VAL) .LT. CUTOFF) GO TO 200
                  NINT = NINT+1
                  I1 = LOCI+I
                  I2 = LOCJ+J
                  I3 = LOCK+K
                  I4 = LOCL+L
                  IF (I1 .GE. I2) GO TO 100
                  N = I1
                  I1 = I2
                  I2 = N
  100             IF (I3 .GE. I4) GO TO 120
                  N = I3
                  I3 = I4
                  I4 = N
  120             IF (I1-I3) 140,160,180
  140             N = I1
                  I1 = I3
                  I3 = N
                  N = I2
                  I2 = I4
                  I4 = N
                  GO TO 180
  160             IF (I2 .LT. I4) GO TO 140
  180             CONTINUE
C
                  IF (OUT) CALL INTOUT(I1,I2,I3,I4,QQ4,NN,VAL)
                  IF (I1 .EQ. I2) VAL = VAL*HALF
                  IF (I3 .EQ. I4) VAL = VAL*HALF
                  IF (I1 .EQ. I3 .AND. I2 .EQ. I4) VAL = VAL*HALF
                  ICONT = ICONT + 1
                  BUFP(ICONT) = VAL
C
               NPACK = ICONT
               IPACK = I1
               JPACK = I2
               KPACK = I3
               LPACK = I4
               IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                 IX( 2*NPACK-1 ) = ISHFT( IPACK, 16 ) + JPACK
                 IX( 2*NPACK   ) = ISHFT( KPACK, 16 ) + LPACK
#endif
#if defined(SPEC_CPU_ILP64)
                 LABEL = ISHFT( IPACK, 48 ) + ISHFT( JPACK, 32 ) +
     *                   ISHFT( KPACK, 16 ) + LPACK
                 IX(NPACK) = LABEL
#endif
               ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                 LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                   ISHFT( KPACK,  8 ) + LPACK
                 IX(NPACK) = LABEL
#endif
#if defined(SPEC_CPU_ILP64)
                 IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                   LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                     ISHFT( KPACK,  8 ) + LPACK
                   IX( NPACK/2 ) = IX( NPACK/2 ) + LABEL
                 ELSE
                   LABEL = ISHFT( IPACK, 56 ) + ISHFT( JPACK, 48 ) +
     *                     ISHFT( KPACK, 40 ) + ISHFT( LPACK, 32 )
                   IX( (NPACK/2)+1 ) = LABEL
                 END IF
#endif
               END IF
C
                  IF(ICONT.GE.NINTMX) THEN
                     CALL ONEIDX(BUFP,IX,NINTMX)
                     ICONT = 0
                  END IF
  200          CONTINUE
  220       CONTINUE
  240    CONTINUE
  260 CONTINUE
      RETURN
      END
C*MODULE TRANS   *DECK FCKBRD
      SUBROUTINE FCKBRD(ITERM,F,C,D,WRK,NCORB,ECORE,DOFOCK,
     *                  NUM,NUM2,MYPASS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,DOFOCK
C
      PARAMETER (MXAO=2047)
C
      DIMENSION F(NUM2),C(NUM,*),D(NUM2),WRK(NUM2)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (ZERO=0.0D+00, PT5=0.5D+00, TWO=2.0D+00)
C
C     THIS ROUTINE IS USED IN PARALLEL ON NODES THAT DO NOT RECEIVE
C     ANY TRANSFORMATION PASSES.  ITS FUNCTION IS TO SERVE IN THE
C     PLACE OF ROUTINES TR1MC AND TR1E, WHICH HAVE MESSAGE PASSING
C     STEPS IN WHICH ALL THE NODES MUST PARTICIPATE.
C
      IF(ITERM.EQ.1) MYPASS = MYPASS + 1
C
      IF(MYPASS.GT.1) RETURN
      IF(.NOT.DOFOCK) RETURN
      IF(NCORB.EQ.0)  RETURN
C
C        1 ELECTRON TERM
C     PARTICIPATE IN THE DAREAD OF 1E- INTEGRALS, GET 1E- CORE ENERGY
C
      IF(ITERM.EQ.1) THEN
         ECORE = ZERO
         CALL GETDEN(D,C,NUM,NCORB)
         CALL DAREAD(IDAF,IODA,F,NUM2,11,0)
         ECORE = TRACEP(D,F,NUM)
      END IF
C
C        2 ELECTRON TERM
C     RECEIVE 2E- INTEGRAL CONTRIBUTION TO FOCK MATRIX FROM MASTER
C     EVALUATE THE 2E- CORE ENERGY CONTRIBUTION
C     PARTICIPATE IN THE ADJUSTMENT OF THE 1E- OPERATOR
C
      IF(ITERM.EQ.2) THEN
         CALL DDI_BCAST(1720,'F',F,NUM2,MASTER)
         CALL DSCAL(NUM2,PT5,F,1)
         II = 0
         DO 750 I=1,NUM
            II = II+I
            F(II) = F(II)+F(II)
  750    CONTINUE
         CALL SYMH(F,WRK,IA)
C
         ECORE = ECORE+TRACEP(D,F,NUM)
         ECORE = ECORE+ECORE
C
         CALL DAREAD(IDAF,IODA,WRK,NUM2,11,0)
         DO 780 I=1,NUM2
            WRK(I) = WRK(I) + TWO*F(I)
  780    CONTINUE
         CALL DAWRIT(IDAF,IODA,WRK,NUM2,35,0)
      END IF
C
      RETURN
      END
C*MODULE TRANS   *DECK FRSTTR
      SUBROUTINE FRSTTR(XPQRJ,CO,NDIM,NUM,NOC,XX,IX,NXX,LPASS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXAO=2047)
C
      DIMENSION XPQRJ(LPASS,NDIM,NUM),CO(NUM,*),XX(*),IX(*)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /PCKLAB/ LABSIZ
C
      NINT = ABS(NXX)
C
      IF(LABSIZ.GE.2) THEN
C
C     ----- TWO LABELS PER INTEGRAL -----
C
         DO 20 M=1,NINT
            VAL1 = XX(M)
            VAL3 = VAL1
C
                       NPACK = M
#if !defined(SPEC_CPU_ILP64)
                         LABEL = IX( 2*NPACK - 1 )
                         IPACK = ISHFT( LABEL, -16 )
                         JPACK = IAND(  LABEL, 65535 )
                         LABEL = IX( 2*NPACK     )
                         KPACK = ISHFT( LABEL, -16 )
                         LPACK = IAND(  LABEL, 65535 )
#endif
#if defined(SPEC_CPU_ILP64)
                         LABEL = IX(NPACK)
                         IPACK = ISHFT( LABEL, -48 )
                         JPACK = IAND( ISHFT( LABEL, -32 ), 65535 )
                         KPACK = IAND( ISHFT( LABEL, -16 ), 65535 )
                         LPACK = IAND( LABEL, 65535 )
#endif
                       MP = IPACK
                       MQ = JPACK
                       MR = KPACK
                       MS = LPACK
C
            MPQ= IA(MP)+MQ
            MRS= IA(MR)+MS
            IF(MP.EQ.MQ) VAL1 = VAL1+VAL1
            IF(MR.EQ.MS) VAL3 = VAL3+VAL3
            DO 10 MJ=1,NOC
               XPQRJ(MJ,MPQ,MR) = XPQRJ(MJ,MPQ,MR) + VAL1*CO(MS,MJ)
               XPQRJ(MJ,MPQ,MS) = XPQRJ(MJ,MPQ,MS) + VAL1*CO(MR,MJ)
               XPQRJ(MJ,MRS,MP) = XPQRJ(MJ,MRS,MP) + VAL3*CO(MQ,MJ)
               XPQRJ(MJ,MRS,MQ) = XPQRJ(MJ,MRS,MQ) + VAL3*CO(MP,MJ)
   10       CONTINUE
   20    CONTINUE
C
      ELSE
C
C     ----- ONE BYTE PER LABEL -----
C
*VDIR NODIVLOOP
         DO 40 M=1,NINT
            VAL1 = XX(M)
            VAL3 = VAL1
C
                       NPACK = M
#if !defined(SPEC_CPU_ILP64)
                         LABEL = IX(NPACK)
                         IPACK = ISHFT( LABEL, -24 )
                         JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
                         KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
                         LPACK = IAND( LABEL, 255 )
#endif
#if defined(SPEC_CPU_ILP64)
                         IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                           LABEL = IX( NPACK/2 )
                           IPACK = IAND( ISHFT( LABEL, -24 ), 255 )
                           JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
                           KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
                           LPACK = IAND( LABEL, 255 )
                         ELSE
                           LABEL = IX( (NPACK/2)+1 )
                           IPACK = ISHFT( LABEL, -56 )
                           JPACK = IAND( ISHFT( LABEL, -48 ), 255 )
                           KPACK = IAND( ISHFT( LABEL, -40 ), 255 )
                           LPACK = IAND( ISHFT( LABEL, -32 ), 255 )
                         END IF
#endif
                       MP = IPACK
                       MQ = JPACK
                       MR = KPACK
                       MS = LPACK
C
            MPQ= IA(MP)+MQ
            MRS= IA(MR)+MS
            IF(MP.EQ.MQ) VAL1 = VAL1+VAL1
            IF(MR.EQ.MS) VAL3 = VAL3+VAL3
            DO 30 MJ=1,NOC
               XPQRJ(MJ,MPQ,MR) = XPQRJ(MJ,MPQ,MR) + VAL1*CO(MS,MJ)
               XPQRJ(MJ,MPQ,MS) = XPQRJ(MJ,MPQ,MS) + VAL1*CO(MR,MJ)
               XPQRJ(MJ,MRS,MP) = XPQRJ(MJ,MRS,MP) + VAL3*CO(MQ,MJ)
               XPQRJ(MJ,MRS,MQ) = XPQRJ(MJ,MRS,MQ) + VAL3*CO(MP,MJ)
   30       CONTINUE
   40    CONTINUE
C
      END IF
C
      RETURN
      END
C*MODULE TRANS   *DECK FZCORE
      SUBROUTINE FZCORE(V,D,F,H,XX,IX,NINTMX,IA,L1,L2,
     *                  NCORBS,GHONDO,XINTS,DDIJ,DSH,WRK,NSH2,MAXG,
     *                  INTG76)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION V(L1,NCORBS),D(L2),F(L2),H(L2),XX(NINTMX),IX(NINTMX),
     *          IA(L1),GHONDO(MAXG),XINTS(NSH2),DDIJ(*),
     *          DSH(NSH2),WRK(L2)
C
      LOGICAL DIRTRF,DIRSCF,FDIFF,SVDIRT,SVDIRS,SVFDFF
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXRT=100)
C
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /ENRGYS/ ENUCR,EELCT,ETOT,SZ,SZZ,ECORE,ESCF,EERD,E1,E2,
     *                VEN,VEE,EPOT,EKIN,ESTATE(MXRT),STATN
      COMMON /OPTSCF/ DIRSCF,FDIFF
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCKLAB/ LABSIZ
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /TRFOPT/ CUTTRF,NWDTRF,MPTRAN,ITRFAO,NOSYMT,DIRTRF
C
      PARAMETER (ZERO=0.0D+00, HALF=0.5D+00, TWO=2.0D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RHF/8HRHF     /, CHECK/8HCHECK   /
#else
      CHARACTER*8 :: RHF_STR
      EQUIVALENCE (RHF, RHF_STR)
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA RHF_STR/"RHF     "/, CHECK_STR/"CHECK   "/
#endif
C
C     ----- CALCULATE FROZEN CORE ENERGY CONTRIBUTION -----
C     ON ENTRY, -H- MUST CONTAIN THE ONE ELECTRON INTEGRALS
C     ON EXIT, -H- IS MODIFIED TO ACCOUNT FOR FROZEN CORES
C
C        CALCULATE DENSITY MATRIX FOR CORE ORBITALS ONLY
C
      IJ = 0
      DO 130 I = 1,L1
         IA(I)=IJ
         DO 120 J = 1,I
            IJ = IJ + 1
            DUM = ZERO
            DO 110 K = 1,NCORBS
               DUM = DUM+V(I,K)*V(J,K)
  110       CONTINUE
            D(IJ) = DUM
  120    CONTINUE
  130 CONTINUE
C
C        TWO ELECTRON CONTRIBUTION TO CORE FOCK OPERATOR
C
      CALL VCLR(F,1,L2)
C
      IF(DIRTRF) THEN
         SVDIRT = DIRTRF
         SVDIRS = DIRSCF
         SVFDFF = FDIFF
         DIRTRF = .FALSE.
         DIRSCF = .TRUE.
         FDIFF  = .TRUE.
         IF(EXETYP.EQ.CHECK) THEN
            CALL VCLR(XINTS,1,NSH2)
         ELSE
            CALL DAREAD(IDAF,IODA,XINTS,NSH2,54,0)
         END IF
         DUMMY = ZERO
         CALL SHLDEN(RHF,D,DUMMY,DUMMY,DSH,IA,L1,L2,NSH2,1)
         CALL TWOEI(RHF,DIRSCF,.FALSE.,.FALSE.,.FALSE.,
     *              INTG76,.TRUE.,NINT,NSCHWZ,L1,L2,
     *              DUMMY,DUMMY,IDUMMY,1,
     *              XINTS,NSH2,GHONDO,MAXG,DDIJ,
     *              IA,D,F,DUMMY,DUMMY,DSH,DUMMY,DUMMY,1)
         IF (GOPARR) CALL DDI_GSUMF(1710,F,L2)
         DIRTRF = SVDIRT
         DIRSCF = SVDIRS
         FDIFF  = SVFDFF
         GO TO 300
      END IF
C
C        IF ALL NODES HAVE A DUPLICATE AO INTEGRAL LIST, ONLY
C        ONE NODE NEED BOTHER FORMING THE CORE FOCK OPERATOR.
C
      IF(ITRFAO.EQ.1  .AND.  .NOT.MASWRK) GO TO 350
C
      CALL SEQREW(IJKO)
  200 CONTINUE
C
C           READ DUPLICATED OR DISTRIBUTED AO INTEGRAL LIST
C
         CALL PREAD(IJKO,XX,IX,NXX,NINTMX)
C
         IF (NXX.EQ.0) GO TO 280
         NINT = ABS(NXX)
         IF (NINT.GT.NINTMX) CALL ABRT
C
C           PLACE INTEGRALS INTO CORE FOCK MATRIX
C
         DO 270 M = 1,NINT
C
                       NPACK = M
                       IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                         LABEL = IX( 2*NPACK - 1 )
                         IPACK = ISHFT( LABEL, -16 )
                         JPACK = IAND(  LABEL, 65535 )
                         LABEL = IX( 2*NPACK     )
                         KPACK = ISHFT( LABEL, -16 )
                         LPACK = IAND(  LABEL, 65535 )
#endif
#if defined(SPEC_CPU_ILP64)
                         LABEL = IX(NPACK)
                         IPACK = ISHFT( LABEL, -48 )
                         JPACK = IAND( ISHFT( LABEL, -32 ), 65535 )
                         KPACK = IAND( ISHFT( LABEL, -16 ), 65535 )
                         LPACK = IAND( LABEL, 65535 )
#endif
                       ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                         LABEL = IX(NPACK)
                         IPACK = ISHFT( LABEL, -24 )
                         JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
                         KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
                         LPACK = IAND( LABEL, 255 )
#endif
#if defined(SPEC_CPU_ILP64)
                         IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                           LABEL = IX( NPACK/2 )
                           IPACK = IAND( ISHFT( LABEL, -24 ), 255 )
                           JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
                           KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
                           LPACK = IAND( LABEL, 255 )
                         ELSE
                           LABEL = IX( (NPACK/2)+1 )
                           IPACK = ISHFT( LABEL, -56 )
                           JPACK = IAND( ISHFT( LABEL, -48 ), 255 )
                           KPACK = IAND( ISHFT( LABEL, -40 ), 255 )
                           LPACK = IAND( ISHFT( LABEL, -32 ), 255 )
                         END IF
#endif
                       END IF
                       I = IPACK
                       J = JPACK
                       K = KPACK
                       L = LPACK
C
            VAL = XX(M)
            VAL4= (VAL+VAL)+(VAL+VAL)
            NIJ = IA(I)+J
            NKL = IA(K)+L
            NIK = IA(I)+K
            NIL = IA(I)+L
            IF (J .LT. K) GO TO 220
            NJK = IA(J)+K
            NJL = IA(J)+L
            GO TO 260
C
  220       NJK = IA(K)+J
            IF (J .LT. L) GO TO 240
            NJL = IA(J)+L
            GO TO 260
C
  240       NJL = IA(L)+J
  260       F(NIJ) = F(NIJ)+VAL4*D(NKL)
            F(NKL) = F(NKL)+VAL4*D(NIJ)
            F(NIK) = F(NIK)-VAL*D(NJL)
            F(NIL) = F(NIL)-VAL*D(NJK)
            F(NJK) = F(NJK)-VAL*D(NIL)
            F(NJL) = F(NJL)-VAL*D(NIK)
  270    CONTINUE
      IF(NXX.GT.0) GO TO 200
C
C        FOR DISTRIBUTED AO INTEGRAL LISTS, SUM ALL TWO ELECTRON TERMS
C
  280 CONTINUE
      CALL SEQREW(IJKO)
      IF(GOPARR  .AND.  ITRFAO.EQ.2) CALL DDI_GSUMF(1705,F,L2)
C
C        COMPLETE FORMATION OF FOCK OPERATOR
C
  300 CONTINUE
      CALL DSCAL(L2,HALF,F,1)
      II = 0
      DO 340 I = 1, L1
         II = II + I
         F(II) = F(II) + F(II)
  340 CONTINUE
      CALL SYMH(F,WRK,IA)
C
C        FOR DUPLICATED AO INTEGRAL LISTS, WE NOW GIVE
C        THE COMPLETED 2E- FOCK MATRIX TO ALL OTHER NODES
C
  350 CONTINUE
      IF(GOPARR  .AND.  ITRFAO.EQ.1  .AND.  .NOT.DIRTRF)
     *   CALL DDI_BCAST(1715,'F',F,L2,MASTER)
C
C        COMPUTE THE FROZEN CORE ENERGY, MODIFY 1E- INTEGRALS
C
      EC1 = TRACEP(D,H,L1)
      EC2 = TRACEP(D,F,L1)
      ECORE = TWO*(EC1+EC2)
      CALL DAXPY(L2,TWO,F,1,H,1)
      RETURN
      END
C*MODULE TRANS   *DECK GETDEN
      SUBROUTINE GETDEN(D,C,NUM,NORB)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (ZERO=0.0D+00)
C
      DIMENSION D(*),C(NUM,*)
C
      MPQ=0
      DO 20 MP=1,NUM
      DO 20 MQ=1,MP
         MPQ=MPQ+1
         DUM=ZERO
         DO 10 K=1,NORB
            DUM=DUM+C(MP,K)*C(MQ,K)
   10    CONTINUE
         D(MPQ)=DUM
   20 CONTINUE
      RETURN
      END
C*MODULE TRANS   *DECK MCXTR1
      SUBROUTINE MCXTR1(XPQRJ,C,F,D,XX,IX,JFLG,NCPU,
     *                  MOSLAB,MOSIRP,WRK,IWRK,
     *                  IJK,NFT,TOL,NPFLG,NINT,ECORE,DOFOCK,XBUF,IXBUF,
     *                  GHONDO,XINTS,DDIJ,MAXG,NSH2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL DBUG,DOFOCK
C
      PARAMETER (MXATM=500)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),CR(3,MXATM)
      COMMON /MCPAR / NFZC,NCORB,NCI,NOC,NORB,NUM1
C
      DIMENSION XPQRJ(NOC,*),C(NUM,*),F(*),D(*),XX(*),IX(*),MOSLAB(*),
     *          MOSIRP(*),XBUF(*),IXBUF(*),
     *          GHONDO(MAXG),XINTS(NSH2),DDIJ(*),
     *          JFLG(0:NCPU-1),WRK(*),IWRK(*)
C
      NUM2 = (NUM*(NUM+1))/2
      CALL TR1MC(XPQRJ,F,WRK,C,C,D,XX,IX,JFLG,NCPU,
     *           NUM2,NUM,NCORB,NOC,IJK,
     *           ECORE,DOFOCK,GHONDO,XINTS,DDIJ,MAXG,NSH2)
      DBUG = NPFLG.GE.1
      IF(DOFOCK)
     *    CALL TR1E(D,F,C(1,NCORB+1),WRK,NUM,NCI,NCORB,NFT,DBUG,MOSLAB)
      DBUG = NPFLG.GE.2
      CALL TR2MC(XPQRJ,C,XX,NUM,NUM2,NORB,NOC)
      CALL TR3MC(XPQRJ,C,WRK,IWRK,D,F,NUM,MOSIRP,NFT,NOC,NUM2,DBUG,
     *           TOL,NINT,XBUF,IXBUF)
      RETURN
      END
C*MODULE TRANS   *DECK MCXTR2
      SUBROUTINE MCXTR2(XPQRJ,C,F,D,XX,IX,JFLG,NCPU,
     *                  X,Y,MOSLAB,MOSIRP,WRK,IWRK,
     *                  IJK,NFT,TOL,NPFLG,ECORE,DOFOCK,NDIM,NUM2,
     *                  MINJ,MAXJ,XBUF,IXBUF,NUX,NREC,
     *                  GHONDO,XINTS,DDIJ,MAXG,NSH2,MYPASS)
C
C        NOTE THAT -F- AND -X- ARE EQUIVALENT BY CALL
C        NOTE THAT -D- AND -Y- ARE EQUIVALENT BY CALL
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
C
      LOGICAL DBUG,DOFOCK,SYM2EI
C
      PARAMETER (MXATM=500, MXSH=1000, MXAO=2047)
C
      DIMENSION XPQRJ(NDIM,NUM2,*),C(NUM,*),F(*),D(*),XX(*),IX(*),
     *          X(NUM,*),Y(NUM,*),MOSLAB(*),MOSIRP(*),WRK(*),IWRK(*),
     *          XBUF(*), IXBUF(*),GHONDO(MAXG),XINTS(NSH2),DDIJ(*),
     *          JFLG(0:NCPU-1)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),CR(3,MXATM)
      COMMON /MCPAR / NFZC,NCORBS,NCI,NORBS,NORB,NUM1
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                TSYM(432),INVT(48),NT
C
C     ----- DRIVER FOR SEGMENTED INTEGRAL TRANSFORMATION -----
C
C     ----- TRANSFORM  (PQ|RS) -> (PQ|RJ)  JMIN <= J <= JMAX
C     ----- IF JMIN=1 AND NCORBS<>0 ALSO FOCK CORE OPERATOR IS COMPUTED
C
      MYPASS = MYPASS + 1
      IF(MYPASS.EQ.1) THEN
         NCORB = NCORBS
      ELSE
         NCORB = 0
      END IF
      CALL TR1MC(XPQRJ,F,WRK,C,C(1,MINJ),D,XX,IX,JFLG,NCPU,
     *           NUM2,NUM,NCORB,NDIM,IJK,
     *           ECORE,DOFOCK,GHONDO,XINTS,DDIJ,MAXG,NSH2)
      DBUG = NPFLG.GE.1
      IF(DOFOCK  .AND.  MYPASS.EQ.1)
     *    CALL TR1E(D,F,C(1,NCORB+1),WRK,NUM,NCI,NCORB,NFT,DBUG,MOSLAB)
      DBUG = NPFLG.GE.2
C
C     ----- TRANSFORM (PQ|RJ) TO (PQ|YJ) FOR ALL PQ,
C     ----- Y .LE. J IF Y .LE. NORBS
C     ----- Y NORBS+1 ... NORB
C
      DO 220 MPQ=1,NUM2
         DO 210 MJ=MINJ,MAXJ
            NJ=MJ-MINJ+1
            DO 200 MR=1,NUM
               X(MR,NJ) = XPQRJ(NJ,MPQ,MR)
  200       CONTINUE
            CALL DGEMV('T',NUM,MJ        ,ONE,C(1,1)      ,NUM,
     *                 X(1,NJ),1,ZERO,XPQRJ(NJ,MPQ,1)   ,NUM2*NDIM)
            CALL DGEMV('T',NUM,NORB-NORBS,ONE,C(1,NORBS+1),NUM,
     *                 X(1,NJ),1,ZERO,XPQRJ(NJ,MPQ,MJ+1),NUM2*NDIM)
  210    CONTINUE
  220 CONTINUE
C
      DO 600 MJ=MINJ,MAXJ
      NJ=MJ-MINJ+1
      NY=0
      DO 600 MY=1,NORB
      IF(MY.GT.MJ.AND.MY.LE.NORBS) GO TO 600
         NY=NY+1
         MPQ=0
         DO 310 MP=1,NUM
         DO 310 MQ=1,MP
            MPQ=MPQ+1
            X(MP,MQ) = XPQRJ(NJ,MPQ,NY)
            X(MQ,MP) = X(MP,MQ)
  310    CONTINUE
C
         IF(MY.LE.NCORBS.AND.MJ.LE.NCORBS) THEN
C
C     ----- (PQ|YJ) IS (PQ|{CORE}{CORE}); WILL BE TRANSFORMED TO
C     ----- ({VAL+VIRT}{VAL+VIRT}|{CORE}{CORE})
C
            CALL MRARBR(X,NUM,NUM,NUM,C(1,NCORBS+1),
     *                  NUM,NORB-NCORBS,Y,NUM)
            M1 = IA(NCORBS+1)+1
            MPQ = 0
            DO 340 MA=1,NORB-NCORBS
               DO 320 MX=1,NCORBS
                  WRK(MPQ+MX) = ZERO
  320          CONTINUE
               MPQ = MPQ+NCORBS
               IF(NT.EQ.1) THEN
                  CALL DGEMV('T',NUM,MA,ONE,C(1,1+NCORBS),NUM,
     *                       Y(1,MA),1,ZERO,WRK(MPQ+1),1)
                  MPQ = MPQ+MA
               ELSE
                  DO 330 MX=1,MA
                     MPQ=MPQ+1
                     IF(SYM2EI(MOSIRP(MA+NCORBS),MOSIRP(MX+NCORBS),
     *                         MOSIRP(MY),MOSIRP(MJ))) THEN
                        DUM = DDOT(NUM,Y(1,MA),1,C(1,MX+NCORBS),1)
                     ELSE
                        DUM = ZERO
                     END IF
                     WRK(MPQ) = DUM
  330             CONTINUE
               END IF
  340       CONTINUE
C
         ELSE IF(MY.LE.NORBS) THEN
C
C     ----- (PQ|YJ) IS (PQ|{VAL}{CORE+VAL}); WILL BE TRANSFORMED TO
C     ----- ({VAL+VIRT}{ALL}|{VAL}{CORE+VAL})
C
            CALL MRARBR(X,NUM,NUM,NUM,C(1,MJ),NUM,NORB-MJ+1,Y,NUM)
            M1 = IA(MJ)+1
            MPQ = 0
            DO 430 MA=MJ,NORB
               DO 420 MX=1,MA
                  MPQ = MPQ+1
                  IF(MA.EQ.MJ .AND. MX.LT.MY) THEN
                     DUM = ZERO
                  ELSE IF(SYM2EI(MOSIRP(MA),MOSIRP(MX),MOSIRP(MY),
     *                    MOSIRP(MJ))) THEN
                     DUM = DDOT(NUM,Y(1,MA-MJ+1),1,C(1,MX),1)
                  ELSE
                     DUM = ZERO
                  END IF
                  WRK(MPQ) = DUM
  420          CONTINUE
  430       CONTINUE
C
         ELSE
C
C     ----- (PQ|YJ) IS (PQ|{VIRT}{CORE+VAL}); WILL BE TRANSFORMED TO
C     ----- ({VIRT}{CORE+VAL}|{VIRT}{CORE+VAL})
C
            CALL MRARBR(X,NUM,NUM,NUM,C(1,MY),
     *                  NUM,NORB-MY+1,Y,NUM)
            M1 = IA(MY)+1
            MPQ = 0
            DO 540 MA=MY,NORB
               DO 520 MX=1,NORBS
                  MPQ = MPQ+1
                  IF(MA.EQ.MY .AND. MX.LT.MJ) THEN
                     DUM = ZERO
                  ELSE IF(SYM2EI(MOSIRP(MA),MOSIRP(MX),MOSIRP(MY),
     *                    MOSIRP(MJ))) THEN
                     DUM = DDOT(NUM,Y(1,MA-MY+1),1,C(1,MX),1)
                  ELSE
                     DUM = ZERO
                  END IF
                  WRK(MPQ) = DUM
  520          CONTINUE
               DO 530 MX=NORBS+1,MA
                  MPQ=MPQ+1
                  WRK(MPQ) = ZERO
  530          CONTINUE
  540       CONTINUE
C
         END IF
C
         MYTEMP = MY
         MJTEMP = MJ
         IF (MY.GE.MJ) THEN
            CALL TRFWT(NFT,WRK,NUX,IWRK,XBUF,MPQ,M1,TOL,NREC,DBUG,
     *                 MYTEMP,MJTEMP,IXBUF)
         ELSE
            CALL TRFWT(NFT,WRK,NUX,IWRK,XBUF,MPQ,M1,TOL,NREC,DBUG,
     *                 MJTEMP,MYTEMP,IXBUF)
         END IF
C
  600 CONTINUE
      RETURN
      END
C*MODULE TRANS   *DECK MCXTR3
      SUBROUTINE MCXTR3(XPQRJ,C,F,D,XX,IX,
     *                  X,MOSLAB,WRK,IJK,NFT,
     *                  NPFLG,ECORE,DOFOCK,NDIM,MINPQ,MAXPQ,
     *                  IDAF20,IODA20,GHONDO,XINTS,DDIJ,MAXG,NSH2)
C
C           NOTE THAT -F- AND -X- ARE EQUIVALENT BY CALL
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION XPQRJ(NORBS,NDIM,*),D(*),F(*),C(NUM,*),XX(*),IX(*),
     *          X(NUM,*),IODA20(*),GHONDO(MAXG),XINTS(NSH2),DDIJ(*),
     *          WRK(*)
C
      LOGICAL DBUG,DOFOCK
C
      PARAMETER (MXATM=500)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),CA(3,MXATM)
      COMMON /MCPAR / NFZC,NCORBS,NCI,NORBS,NORB,NUM1
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
C
      NUM2 = (NUM*(NUM+1))/2
      IF(MINPQ.EQ.1) THEN
         NCORB=NCORBS
      ELSE
         NCORB=0
      END IF
      CALL TR5MC(XPQRJ,F,WRK,C,C,D,XX,IX,
     *           NDIM,NUM,NCORB,NORBS,MINPQ,MAXPQ,
     *           IJK,DOFOCK,ECORE,GHONDO,XINTS,DDIJ,MAXG,NSH2)
      DBUG=NPFLG.GE.1
      IF(DOFOCK  .AND.  MINPQ.EQ.1)
     *    CALL TR1E(D,F,C(1,NCORB+1),WRK,NUM,NCI,NCORB,NFT,DBUG,MOSLAB)
C
C     ----- (PQ|YJ) = SUM OVER S OF (PQ|RJ)*C(R,Y)      -----
C     ----- FOR Y = 1...NORB; MINPQ .LE. PQ .LE. MAXPQ  -----
C
      DO 170 MPQ=1,NDIM
         DO 160 MJ=1,NORBS
         DO 160 MR=1,NUM
            X(MR,MJ) = XPQRJ(MJ,MPQ,MR)
  160    CONTINUE
C
         DO 170 MY=1,NORB
            CALL DGEMV('T',NUM,MIN(MY,NORBS),ONE,X(1,1),
     *                 NUM,C(1,MY),1,ZERO,XPQRJ(1,MPQ,MY),1)
  170    CONTINUE
C
C     ----- SAVE IN DAFILE -----
C
      MYJ=0
      DO 200 MY=1,NORB
      DO 200 MJ=1,MIN(MY,NORBS)
         MYJ=MYJ+1
         IF(MINPQ.EQ.1) THEN
            CALL VCLR(WRK,1,NUM2)
         ELSE
            CALL RAREAD(IDAF20,IODA20,WRK,NUM2,MYJ,0)
         END IF
         DO 190 MPQ=1,NDIM
            WRK(MPQ+MINPQ-1)=XPQRJ(MJ,MPQ,MY)
  190    CONTINUE
         CALL RAWRIT(IDAF20,IODA20,WRK,NUM2,MYJ,0)
  200 CONTINUE
      RETURN
      END
C*MODULE TRANS   *DECK ONEIDX
      SUBROUTINE ONEIDX(XX,IX,NXX)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /DIRCAS/ I2CASE
      COMMON /DIRMEM/ IPQRJ,IVEC,IDEN,IFCK,NUM,NOC,NDIM,MINPQ,MAXPQ
      COMMON /DIRZPT/ LPASS,NJUMP
      COMMON /FMCOM / X(1)
C
      DIMENSION XX(*),IX(*)
C
C     ----- DIRECT TRANSFORMATION FOR MCSCF, CPHF, AND MP2 -----
C     THE VALUES IN COMMON /DIRMEM/ ARE POINTERS TO THE MEMORY
C
C               IPQRJ - (PQ|KL) OR (PQ|RJ) INTEGRALS
C                IVEC - ORBITALS
C                IDEN - DENSITY MATRIX
C                IFCK - FOCK MATRIX
C
C     ----- SELECT WHAT TO DO -----
C                 I2CASE IN COMMON /DIRCAS/ DEFINES TYPES OF
C                 TRANSFORMATION:  1--> WITH FOCK OPERATOR
C                                  2--> TWO INDEX TRANSFORMATION
C                                  3--> FOR MP2
C                                  6--> FOR DIRECT MP2 ZAPT
C
      GO TO (100,200,300,400,500,600) I2CASE
      WRITE(IW,9999) I2CASE
      CALL ABRT
      RETURN
C
C     ----- 1-INDEX TRANSFORMATION WITH FOCK OPERATOR -----
C
  100 CONTINUE
      CALL ONETFF(X(IPQRJ),X(IVEC),X(IFCK),X(IDEN),
     *            NDIM,NUM,NOC,XX,IX,NXX)
      RETURN
C
C     ----- 2-INDEX TRANSFORMATION -----
C
  200 CONTINUE
      NUM2 = (NUM*(NUM+1))/2
      CALL TWOTFF(X(IPQRJ),X(IVEC),X(IFCK),X(IDEN),
     *            NUM2,NUM,NOC,XX,IX,NXX)
      RETURN
C
C     ----- 1-INDEX TRANSFORMATION FOR MP2 -----
C
  300 CONTINUE
      CALL ONETRF(X(IPQRJ),X(IVEC),NDIM,NUM,NOC,XX,IX,NXX)
      RETURN
C
C     ----- 1-INDEX PARTIAL TRANSFORMATION WITH FOCK OPERATOR -----
C
  400 CONTINUE
      CALL ONEPFF(X(IPQRJ),X(IVEC),X(IFCK),X(IDEN),
     *            NDIM,MINPQ,MAXPQ,NUM,NOC,XX,IX,NXX)
      RETURN
C
C     ----- 1-INDEX PARTIAL TRANSFORMATION -----
C
  500 CONTINUE
      CALL ONEPRF(X(IPQRJ),X(IVEC),NDIM,MINPQ,MAXPQ,
     *            NUM,NOC,XX,IX,NXX)
      RETURN
C
  600 CONTINUE
      CALL FRSTTR(X(IPQRJ),X(IVEC + NJUMP * NUM),NDIM,NUM,NOC,
     *            XX,IX,NXX,LPASS)
      RETURN
C
 9999 FORMAT('WRONG I2CASE PARAMETER (',I3,') IN ONEIDX')
      END
C*MODULE TRANS   *DECK ONEPFF
      SUBROUTINE ONEPFF(XPQRJ,CO,FC,DC,NDIM,MINPQ,MAXPQ,NUM,NOC,
     *                  XX,IX,NXX)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXAO=2047)
C
      DIMENSION XPQRJ(NOC,NDIM,*),CO(NUM,*),FC(*),DC(*),XX(*),IX(*)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /PCKLAB/ LABSIZ
C
      NINT = ABS(NXX)
C
      IF(LABSIZ.GE.2) THEN
C
C     ----- TWO BYTES PER LABEL -----
C
         DO 20 M=1,NINT
            VAL1 = XX(M)
            VAL3 = VAL1
            VAL4 = (VAL1+VAL1)+(VAL1+VAL1)
C
                       NPACK = M
#if !defined(SPEC_CPU_ILP64)
                         LABEL = IX( 2*NPACK - 1 )
                         IPACK = ISHFT( LABEL, -16 )
                         JPACK = IAND(  LABEL, 65535 )
                         LABEL = IX( 2*NPACK     )
                         KPACK = ISHFT( LABEL, -16 )
                         LPACK = IAND(  LABEL, 65535 )
#endif
#if defined(SPEC_CPU_ILP64)
                         LABEL = IX(NPACK)
                         IPACK = ISHFT( LABEL, -48 )
                         JPACK = IAND( ISHFT( LABEL, -32 ), 65535 )
                         KPACK = IAND( ISHFT( LABEL, -16 ), 65535 )
                         LPACK = IAND( LABEL, 65535 )
#endif
                       MP = IPACK
                       MQ = JPACK
                       MR = KPACK
                       MS = LPACK
C
            MPQ= IA(MP)+MQ
            MRS= IA(MR)+MS
            MPR= IA(MP)+MR
            MPS= IA(MP)+MS
            MQR= IA(MAX(MQ,MR))+MIN(MQ,MR)
            MQS= IA(MAX(MQ,MS))+MIN(MQ,MS)
            FC(MPQ) = FC(MPQ)+VAL4*DC(MRS)
            FC(MRS) = FC(MRS)+VAL4*DC(MPQ)
            FC(MPR) = FC(MPR)-VAL1*DC(MQS)
            FC(MPS) = FC(MPS)-VAL1*DC(MQR)
            FC(MQR) = FC(MQR)-VAL1*DC(MPS)
            FC(MQS) = FC(MQS)-VAL1*DC(MPR)
            IF(MP.EQ.MQ) VAL1 = VAL1+VAL1
            IF(MR.EQ.MS) VAL3 = VAL3+VAL3
            IF(MPQ.GE.MINPQ.AND.MPQ.LE.MAXPQ) THEN
            MPQ=MPQ-MINPQ+1
            DO 10 MJ=1,NOC
               XPQRJ(MJ,MPQ,MR) = XPQRJ(MJ,MPQ,MR) + VAL1*CO(MS,MJ)
               XPQRJ(MJ,MPQ,MS) = XPQRJ(MJ,MPQ,MS) + VAL1*CO(MR,MJ)
   10       CONTINUE
            END IF
            IF(MRS.GE.MINPQ.AND.MRS.LE.MAXPQ) THEN
            MRS=MRS-MINPQ+1
            DO 15 MJ=1,NOC
               XPQRJ(MJ,MRS,MP) = XPQRJ(MJ,MRS,MP) + VAL3*CO(MQ,MJ)
               XPQRJ(MJ,MRS,MQ) = XPQRJ(MJ,MRS,MQ) + VAL3*CO(MP,MJ)
   15       CONTINUE
            END IF
   20    CONTINUE
C
      ELSE
C
C     ----- ONE BYTE PER LABEL -----
C
         DO 40 M=1,NINT
            VAL1 = XX(M)
            VAL3 = VAL1
            VAL4 = (VAL1+VAL1)+(VAL1+VAL1)
C
                       NPACK = M
#if !defined(SPEC_CPU_ILP64)
                         LABEL = IX(NPACK)
                         IPACK = ISHFT( LABEL, -24 )
                         JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
                         KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
                         LPACK = IAND( LABEL, 255 )
#endif
#if defined(SPEC_CPU_ILP64)
                         IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                           LABEL = IX( NPACK/2 )
                           IPACK = IAND( ISHFT( LABEL, -24 ), 255 )
                           JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
                           KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
                           LPACK = IAND( LABEL, 255 )
                         ELSE
                           LABEL = IX( (NPACK/2)+1 )
                           IPACK = ISHFT( LABEL, -56 )
                           JPACK = IAND( ISHFT( LABEL, -48 ), 255 )
                           KPACK = IAND( ISHFT( LABEL, -40 ), 255 )
                           LPACK = IAND( ISHFT( LABEL, -32 ), 255 )
                         END IF
#endif
                       MP = IPACK
                       MQ = JPACK
                       MR = KPACK
                       MS = LPACK
C
            MPQ= IA(MP)+MQ
            MRS= IA(MR)+MS
            MPR= IA(MP)+MR
            MPS= IA(MP)+MS
            MQR= IA(MAX(MQ,MR))+MIN(MQ,MR)
            MQS= IA(MAX(MQ,MS))+MIN(MQ,MS)
            FC(MPQ) = FC(MPQ)+VAL4*DC(MRS)
            FC(MRS) = FC(MRS)+VAL4*DC(MPQ)
            FC(MPR) = FC(MPR)-VAL1*DC(MQS)
            FC(MPS) = FC(MPS)-VAL1*DC(MQR)
            FC(MQR) = FC(MQR)-VAL1*DC(MPS)
            FC(MQS) = FC(MQS)-VAL1*DC(MPR)
            IF(MP.EQ.MQ) VAL1 = VAL1+VAL1
            IF(MR.EQ.MS) VAL3 = VAL3+VAL3
            IF(MPQ.GE.MINPQ.AND.MPQ.LE.MAXPQ) THEN
            MPQ=MPQ-MINPQ+1
            DO 30 MJ=1,NOC
               XPQRJ(MJ,MPQ,MR) = XPQRJ(MJ,MPQ,MR) + VAL1*CO(MS,MJ)
               XPQRJ(MJ,MPQ,MS) = XPQRJ(MJ,MPQ,MS) + VAL1*CO(MR,MJ)
   30       CONTINUE
            END IF
            IF(MRS.GE.MINPQ.AND.MRS.LE.MAXPQ) THEN
            MRS=MRS-MINPQ+1
            DO 35 MJ=1,NOC
               XPQRJ(MJ,MRS,MP) = XPQRJ(MJ,MRS,MP) + VAL3*CO(MQ,MJ)
               XPQRJ(MJ,MRS,MQ) = XPQRJ(MJ,MRS,MQ) + VAL3*CO(MP,MJ)
   35       CONTINUE
            END IF
   40    CONTINUE
C
      END IF
C
      RETURN
      END
C*MODULE TRANS   *DECK ONEPRF
      SUBROUTINE ONEPRF(XPQRJ,CO,NDIM,MINPQ,MAXPQ,NUM,NOC,XX,IX,NXX)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXAO=2047)
C
      DIMENSION XPQRJ(NOC,NDIM,*),CO(NUM,*),XX(*),IX(*)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /PCKLAB/ LABSIZ
C
      NINT = ABS(NXX)
C
      IF(LABSIZ.GE.2) THEN
C
C     ----- TWO BYTES PER LABEL -----
C
         DO 20 M=1,NINT
            VAL1 = XX(M)
            VAL3 = VAL1
C
                       NPACK = M
#if !defined(SPEC_CPU_ILP64)
                         LABEL = IX( 2*NPACK - 1 )
                         IPACK = ISHFT( LABEL, -16 )
                         JPACK = IAND(  LABEL, 65535 )
                         LABEL = IX( 2*NPACK     )
                         KPACK = ISHFT( LABEL, -16 )
                         LPACK = IAND(  LABEL, 65535 )
#endif
#if defined(SPEC_CPU_ILP64)
                         LABEL = IX(NPACK)
                         IPACK = ISHFT( LABEL, -48 )
                         JPACK = IAND( ISHFT( LABEL, -32 ), 65535 )
                         KPACK = IAND( ISHFT( LABEL, -16 ), 65535 )
                         LPACK = IAND( LABEL, 65535 )
#endif
                       MP = IPACK
                       MQ = JPACK
                       MR = KPACK
                       MS = LPACK
C
            MPQ= IA(MP)+MQ
            MRS= IA(MR)+MS
            IF(MP.EQ.MQ) VAL1 = VAL1+VAL1
            IF(MR.EQ.MS) VAL3 = VAL3+VAL3
            IF(MPQ.GE.MINPQ.AND.MPQ.LE.MAXPQ) THEN
            MPQ=MPQ-MINPQ+1
            DO 10 MJ=1,NOC
               XPQRJ(MJ,MPQ,MR) = XPQRJ(MJ,MPQ,MR) + VAL1*CO(MS,MJ)
               XPQRJ(MJ,MPQ,MS) = XPQRJ(MJ,MPQ,MS) + VAL1*CO(MR,MJ)
   10       CONTINUE
            END IF
            IF(MRS.GE.MINPQ.AND.MRS.LE.MAXPQ) THEN
            MRS=MRS-MINPQ+1
            DO 15 MJ=1,NOC
               XPQRJ(MJ,MRS,MP) = XPQRJ(MJ,MRS,MP) + VAL3*CO(MQ,MJ)
               XPQRJ(MJ,MRS,MQ) = XPQRJ(MJ,MRS,MQ) + VAL3*CO(MP,MJ)
   15       CONTINUE
            END IF
   20    CONTINUE
C
      ELSE
C
C     ----- ONE BYTE PER LABEL -----
C
         DO 40 M=1,NINT
            VAL1 = XX(M)
            VAL3 = VAL1
C
                       NPACK = M
#if !defined(SPEC_CPU_ILP64)
                         LABEL = IX(NPACK)
                         IPACK = ISHFT( LABEL, -24 )
                         JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
                         KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
                         LPACK = IAND( LABEL, 255 )
#endif
#if defined(SPEC_CPU_ILP64)
                         IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                           LABEL = IX( NPACK/2 )
                           IPACK = IAND( ISHFT( LABEL, -24 ), 255 )
                           JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
                           KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
                           LPACK = IAND( LABEL, 255 )
                         ELSE
                           LABEL = IX( (NPACK/2)+1 )
                           IPACK = ISHFT( LABEL, -56 )
                           JPACK = IAND( ISHFT( LABEL, -48 ), 255 )
                           KPACK = IAND( ISHFT( LABEL, -40 ), 255 )
                           LPACK = IAND( ISHFT( LABEL, -32 ), 255 )
                         END IF
#endif
                       MP = IPACK
                       MQ = JPACK
                       MR = KPACK
                       MS = LPACK
C
            MPQ= IA(MP)+MQ
            MRS= IA(MR)+MS
            IF(MP.EQ.MQ) VAL1 = VAL1+VAL1
            IF(MR.EQ.MS) VAL3 = VAL3+VAL3
            IF(MPQ.GE.MINPQ.AND.MPQ.LE.MAXPQ) THEN
            MPQ=MPQ-MINPQ+1
            DO 30 MJ=1,NOC
               XPQRJ(MJ,MPQ,MR) = XPQRJ(MJ,MPQ,MR) + VAL1*CO(MS,MJ)
               XPQRJ(MJ,MPQ,MS) = XPQRJ(MJ,MPQ,MS) + VAL1*CO(MR,MJ)
   30       CONTINUE
            END IF
            IF(MRS.GE.MINPQ.AND.MRS.LE.MAXPQ) THEN
            MRS=MRS-MINPQ+1
            DO 35 MJ=1,NOC
               XPQRJ(MJ,MRS,MP) = XPQRJ(MJ,MRS,MP) + VAL3*CO(MQ,MJ)
               XPQRJ(MJ,MRS,MQ) = XPQRJ(MJ,MRS,MQ) + VAL3*CO(MP,MJ)
   35       CONTINUE
            END IF
   40    CONTINUE
C
      END IF
C
      RETURN
      END
C*MODULE TRANS   *DECK ONETFF
      SUBROUTINE ONETFF(XPQRJ,CO,FC,DC,NDIM,NUM,NOC,XX,IX,NXX)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXAO=2047)
C
      DIMENSION XPQRJ(NOC,NDIM,*),CO(NUM,*),FC(*),DC(*),XX(*),IX(*)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /PCKLAB/ LABSIZ
C
      NINT = ABS(NXX)
C
      IF(LABSIZ.GE.2) THEN
C
C     ----- TWO BYTES PER LABEL -----
C
         DO 20 M=1,NINT
            VAL1 = XX(M)
            VAL3 = VAL1
            VAL4 = (VAL1+VAL1)+(VAL1+VAL1)
C
                       NPACK = M
#if !defined(SPEC_CPU_ILP64)
                         LABEL = IX( 2*NPACK - 1 )
                         IPACK = ISHFT( LABEL, -16 )
                         JPACK = IAND(  LABEL, 65535 )
                         LABEL = IX( 2*NPACK     )
                         KPACK = ISHFT( LABEL, -16 )
                         LPACK = IAND(  LABEL, 65535 )
#endif
#if defined(SPEC_CPU_ILP64)
                         LABEL = IX(NPACK)
                         IPACK = ISHFT( LABEL, -48 )
                         JPACK = IAND( ISHFT( LABEL, -32 ), 65535 )
                         KPACK = IAND( ISHFT( LABEL, -16 ), 65535 )
                         LPACK = IAND( LABEL, 65535 )
#endif
                       MP = IPACK
                       MQ = JPACK
                       MR = KPACK
                       MS = LPACK
C
            MPQ= IA(MP)+MQ
            MRS= IA(MR)+MS
            MPR= IA(MP)+MR
            MPS= IA(MP)+MS
            MQR= IA(MAX(MQ,MR))+MIN(MQ,MR)
            MQS= IA(MAX(MQ,MS))+MIN(MQ,MS)
            FC(MPQ) = FC(MPQ)+VAL4*DC(MRS)
            FC(MRS) = FC(MRS)+VAL4*DC(MPQ)
            FC(MPR) = FC(MPR)-VAL1*DC(MQS)
            FC(MPS) = FC(MPS)-VAL1*DC(MQR)
            FC(MQR) = FC(MQR)-VAL1*DC(MPS)
            FC(MQS) = FC(MQS)-VAL1*DC(MPR)
            IF(MP.EQ.MQ) VAL1 = VAL1+VAL1
            IF(MR.EQ.MS) VAL3 = VAL3+VAL3
            DO 10 MJ=1,NOC
               XPQRJ(MJ,MPQ,MR) = XPQRJ(MJ,MPQ,MR) + VAL1*CO(MS,MJ)
               XPQRJ(MJ,MPQ,MS) = XPQRJ(MJ,MPQ,MS) + VAL1*CO(MR,MJ)
               XPQRJ(MJ,MRS,MP) = XPQRJ(MJ,MRS,MP) + VAL3*CO(MQ,MJ)
               XPQRJ(MJ,MRS,MQ) = XPQRJ(MJ,MRS,MQ) + VAL3*CO(MP,MJ)
   10       CONTINUE
   20    CONTINUE
C
      ELSE
C
C     ----- ONE BYTE PER LABEL -----
C
         DO 40 M=1,NINT
            VAL1 = XX(M)
            VAL3 = VAL1
            VAL4 = (VAL1+VAL1)+(VAL1+VAL1)
C
                       NPACK = M
#if !defined(SPEC_CPU_ILP64)
                         LABEL = IX(NPACK)
                         IPACK = ISHFT( LABEL, -24 )
                         JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
                         KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
                         LPACK = IAND( LABEL, 255 )
#endif
#if defined(SPEC_CPU_ILP64)
                         IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                           LABEL = IX( NPACK/2 )
                           IPACK = IAND( ISHFT( LABEL, -24 ), 255 )
                           JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
                           KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
                           LPACK = IAND( LABEL, 255 )
                         ELSE
                           LABEL = IX( (NPACK/2)+1 )
                           IPACK = ISHFT( LABEL, -56 )
                           JPACK = IAND( ISHFT( LABEL, -48 ), 255 )
                           KPACK = IAND( ISHFT( LABEL, -40 ), 255 )
                           LPACK = IAND( ISHFT( LABEL, -32 ), 255 )
                         END IF
#endif
                       MP = IPACK
                       MQ = JPACK
                       MR = KPACK
                       MS = LPACK
C
            MPQ= IA(MP)+MQ
            MRS= IA(MR)+MS
            MPR= IA(MP)+MR
            MPS= IA(MP)+MS
            MQR= IA(MAX(MQ,MR))+MIN(MQ,MR)
            MQS= IA(MAX(MQ,MS))+MIN(MQ,MS)
            FC(MPQ) = FC(MPQ)+VAL4*DC(MRS)
            FC(MRS) = FC(MRS)+VAL4*DC(MPQ)
            FC(MPR) = FC(MPR)-VAL1*DC(MQS)
            FC(MPS) = FC(MPS)-VAL1*DC(MQR)
            FC(MQR) = FC(MQR)-VAL1*DC(MPS)
            FC(MQS) = FC(MQS)-VAL1*DC(MPR)
            IF(MP.EQ.MQ) VAL1 = VAL1+VAL1
            IF(MR.EQ.MS) VAL3 = VAL3+VAL3
            DO 30 MJ=1,NOC
               XPQRJ(MJ,MPQ,MR) = XPQRJ(MJ,MPQ,MR) + VAL1*CO(MS,MJ)
               XPQRJ(MJ,MPQ,MS) = XPQRJ(MJ,MPQ,MS) + VAL1*CO(MR,MJ)
               XPQRJ(MJ,MRS,MP) = XPQRJ(MJ,MRS,MP) + VAL3*CO(MQ,MJ)
               XPQRJ(MJ,MRS,MQ) = XPQRJ(MJ,MRS,MQ) + VAL3*CO(MP,MJ)
   30       CONTINUE
   40    CONTINUE
C
      END IF
C
      RETURN
      END
C*MODULE TRANS   *DECK ONETRF
      SUBROUTINE ONETRF(XPQRJ,CO,NDIM,NUM,NOC,XX,IX,NXX)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXAO=2047)
C
      DIMENSION XPQRJ(NOC,NDIM,*),CO(NUM,*),XX(*),IX(*)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /PCKLAB/ LABSIZ
C
      NINT = ABS(NXX)
C
      IF(LABSIZ.GE.2) THEN
C
C     ----- TWO BYTES PER LABEL -----
C
         DO 20 M=1,NINT
            VAL1 = XX(M)
            VAL3 = VAL1
C
                       NPACK = M
#if !defined(SPEC_CPU_ILP64)
                         LABEL = IX( 2*NPACK - 1 )
                         IPACK = ISHFT( LABEL, -16 )
                         JPACK = IAND(  LABEL, 65535 )
                         LABEL = IX( 2*NPACK     )
                         KPACK = ISHFT( LABEL, -16 )
                         LPACK = IAND(  LABEL, 65535 )
#endif
#if defined(SPEC_CPU_ILP64)
                         LABEL = IX(NPACK)
                         IPACK = ISHFT( LABEL, -48 )
                         JPACK = IAND( ISHFT( LABEL, -32 ), 65535 )
                         KPACK = IAND( ISHFT( LABEL, -16 ), 65535 )
                         LPACK = IAND( LABEL, 65535 )
#endif
                       MP = IPACK
                       MQ = JPACK
                       MR = KPACK
                       MS = LPACK
C
            MPQ= IA(MP)+MQ
            MRS= IA(MR)+MS
            IF(MP.EQ.MQ) VAL1 = VAL1+VAL1
            IF(MR.EQ.MS) VAL3 = VAL3+VAL3
            DO 10 MJ=1,NOC
               XPQRJ(MJ,MPQ,MR) = XPQRJ(MJ,MPQ,MR) + VAL1*CO(MS,MJ)
               XPQRJ(MJ,MPQ,MS) = XPQRJ(MJ,MPQ,MS) + VAL1*CO(MR,MJ)
               XPQRJ(MJ,MRS,MP) = XPQRJ(MJ,MRS,MP) + VAL3*CO(MQ,MJ)
               XPQRJ(MJ,MRS,MQ) = XPQRJ(MJ,MRS,MQ) + VAL3*CO(MP,MJ)
   10       CONTINUE
   20    CONTINUE
C
      ELSE
C
C     ----- ONE BYTE PER LABEL -----
C
*VDIR NODIVLOOP
         DO 40 M=1,NINT
            VAL1 = XX(M)
            VAL3 = VAL1
C
                       NPACK = M
#if !defined(SPEC_CPU_ILP64)
                         LABEL = IX(NPACK)
                         IPACK = ISHFT( LABEL, -24 )
                         JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
                         KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
                         LPACK = IAND( LABEL, 255 )
#endif
#if defined(SPEC_CPU_ILP64)
                         IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                           LABEL = IX( NPACK/2 )
                           IPACK = IAND( ISHFT( LABEL, -24 ), 255 )
                           JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
                           KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
                           LPACK = IAND( LABEL, 255 )
                         ELSE
                           LABEL = IX( (NPACK/2)+1 )
                           IPACK = ISHFT( LABEL, -56 )
                           JPACK = IAND( ISHFT( LABEL, -48 ), 255 )
                           KPACK = IAND( ISHFT( LABEL, -40 ), 255 )
                           LPACK = IAND( ISHFT( LABEL, -32 ), 255 )
                         END IF
#endif
                       MP = IPACK
                       MQ = JPACK
                       MR = KPACK
                       MS = LPACK
C
            MPQ= IA(MP)+MQ
            MRS= IA(MR)+MS
            IF(MP.EQ.MQ) VAL1 = VAL1+VAL1
            IF(MR.EQ.MS) VAL3 = VAL3+VAL3
            DO 30 MJ=1,NOC
               XPQRJ(MJ,MPQ,MR) = XPQRJ(MJ,MPQ,MR) + VAL1*CO(MS,MJ)
               XPQRJ(MJ,MPQ,MS) = XPQRJ(MJ,MPQ,MS) + VAL1*CO(MR,MJ)
               XPQRJ(MJ,MRS,MP) = XPQRJ(MJ,MRS,MP) + VAL3*CO(MQ,MJ)
               XPQRJ(MJ,MRS,MQ) = XPQRJ(MJ,MRS,MQ) + VAL3*CO(MP,MJ)
   30       CONTINUE
   40    CONTINUE
C
      END IF
C
      RETURN
      END
C*MODULE TRANS   *DECK TR2OUT
      SUBROUTINE TR2OUT(I,J,K,L,N,VAL,NCALL)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TR2PRT/ V(3),JC,J1(3),J2(3),J3(3),J4(3),N1(3)
C
      IF (NCALL .NE. 0) GO TO 100
         JC = JC+1
         J1(JC) = I
         J2(JC) = J
         J3(JC) = K
         J4(JC) = L
         N1(JC) = N
         V(JC) = VAL
         IF (JC .LT. 3) RETURN
C
  100 CONTINUE
      IF (NCALL .LT. 0) GO TO 110
      IF ((JC .GT. 0) .AND. MASWRK)
     *    WRITE (IW,9008) (J1(M),J2(M),J3(M),J4(M),N1(M),V(M),M = 1,JC)
  110 CONTINUE
      JC = 0
      RETURN
 9008 FORMAT(3(4I3,I5,E20.12))
      END
C*MODULE TRANS   *DECK TR1E
      SUBROUTINE TR1E(H0,FC,C,T,NUM,NORB,NCORB,IJKT,DBUG,MOSLAB)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (ZERO=0.0D+00,TWO=2.0D+00)
C
      LOGICAL DBUG
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      DIMENSION H0(*),FC(*),C(NUM,*),T(*),MOSLAB(*)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      NUM2 = (NUM*(NUM+1))/2
C
      CALL DAREAD(IDAF,IODA,H0,NUM2,11,0)
C
C        MODIFY ONE ELECTRON OPERATOR BY ADDING CORE CONTRIBUTIONS
C
      IF(NCORB.NE.0) THEN
         DO 10 I=1,NUM2
            H0(I) = H0(I) + TWO*FC(I)
   10    CONTINUE
      END IF
      CALL DAWRIT(IDAF,IODA,H0,NUM2,35,0)
C
C       TRANSFORM THE ONE ELECTRON OPERATOR TO THE MO BASIS,
C       WRITE THIS AS THE FIRST RECORD OF THE TRANSFORMED INTEGRAL FILE
C       NOTE THAT ONLY NODE 0 OWNS THIS MATRIX
C
      CALL TFTRI(FC,H0,C,T,NORB,NUM,NUM)
      CALL SEQREW(IJKT)
      IF (MASWRK) CALL SQWRIT(IJKT,FC,NUM2)
C
      IF(DBUG) THEN
         DO 20 I=1,NORB
            T(I) = ZERO
   20    CONTINUE
         WRITE(IW,9999)
         CALL PREVS(C,T,MOSLAB,NORB,NUM,NUM)
         WRITE(IW,9998)
         CALL PRTRI(FC,NORB)
      END IF
      RETURN
C
 9999 FORMAT(/' TRANSFORMATION VECTORS')
 9998 FORMAT(/' TRANSFORMED 1E-INTEGRALS')
      END
C*MODULE TRANS   *DECK TR1MC
      SUBROUTINE TR1MC(XPQRJ,F,WRK,C,CO,D,XX,IX,JFLG,NCPU,
     *                 NDIM,NUM,NCORB,NOC,IJK,
     *                 ECORE,DOFOCK,GHONDO,XINTS,DDIJ,MAXG,NSH2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXAO=2047)
C
      LOGICAL PACK2E,GOPARR,DSKWRK,MASWRK,DOFOCK,DIRTRF
C
      DIMENSION XPQRJ(NOC,NDIM,*),F(*),C(NUM,*),CO(NUM,*),D(*),WRK(*),
     *          XX(*),IX(*),JFLG(0:NCPU-1),
     *          GHONDO(MAXG),XINTS(NSH2),DDIJ(*)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,IPOPLE
      COMMON /DIRCAS/ I2CASE
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TRFOPT/ CUTTRF,NWDTRF,MPTRAN,ITRFAO,NOSYMT,DIRTRF
C
      PARAMETER (ZERO=0.0D+00, PT5=0.5D+00)
C
C     ----- PARAMETERS:
C           XPQRJ       ON EXIT ONE-INDEX TRANSFORMED INTEGRALS
C           F           ON EXIT FOCK CORE OPERATOR
C           D           CORE DENSITY MATRIX
C           XX,IX       2E-INTEGRALS BUFFERS
C           C           ON ENTRY CORE ORBITALS
C           CO          ON ENTRY ORBITALS TO TRANSFORM THE 2E-INTEGRALS
C
      NUM2 = (NUM*NUM+NUM)/2
      CALL VCLR(XPQRJ,1,NDIM*NUM*NOC)
C
C     ----- GET 1E-CONTRIBUTION TO FROZEN CORE ENERGY -----
C
      IF(DOFOCK  .AND.  NCORB.GT.0) THEN
         ECORE = ZERO
         CALL GETDEN(D,C,NUM,NCORB)
         CALL DAREAD(IDAF,IODA,F,NUM2,11,0)
         ECORE = TRACEP(D,F,NUM)
         CALL VCLR(F,1,NUM2)
      END IF
C
C     ----- PROCESS 2E-INTEGRALS -----
C
      IF(.NOT.DIRTRF) THEN
C
         DO 100 KAP=0,NPROC-1
            JFLG(KAP)=1
  100    CONTINUE
         KAP = -1
         IFLG=  1
C
         CALL SEQREW(IJK)
  200    CONTINUE
C
C              READ NEXT INTEGRAL RECORD FROM A DUPLICATED OR
C              DISTRIBUTED AO INTEGRAL FILE
C
         IF(ITRFAO.EQ.1) THEN
            CALL PREAD(IJK,XX,IX,NXX,NINTMX)
         ELSE
            CALL PREADP(IJK,XX,IX,NXX,NINTMX,
     *                  KAP,IFLG,JFLG,NPROC)
            IF(IFLG.EQ.0) THEN
               CALL SEQREW(IJK)
               GO TO 700
            END IF
         END IF
C
         IF(NXX.EQ.0) GO TO 250
         NINT=ABS(NXX)
         IF(NINT.GT.NINTMX) CALL ABRT
C
C                PERFORM FIRST INDEX TRANSFORMATION
C
         IF(DOFOCK  .AND.  NCORB.GT.0  .AND.  MASWRK) THEN
            CALL ONETFF(XPQRJ,CO,F,D,NDIM,NUM,NOC,XX,IX,NXX)
         ELSE
            CALL ONETRF(XPQRJ,CO,    NDIM,NUM,NOC,XX,IX,NXX)
         END IF
C
  250    CONTINUE
         IF(ITRFAO.EQ.1  .AND.  NXX.LE.0) THEN
            CALL SEQREW(IJK)
            GO TO 700
         END IF
         GO TO 200
      END IF
C
C     ----- DIRECT CASSCF -----
C
      IF(DIRTRF) THEN
         IF(DOFOCK  .AND.  NCORB.GT.0  .AND.  MASWRK) THEN
            I2CASE=1
         ELSE
            I2CASE=3
         END IF
         CALL DIRJK(GHONDO,XINTS,DDIJ,XX,IX,MAXG,NSH2)
      END IF
C
C     ----- POST-PROCESSING OF FOCK MATRIX -----
C     ----- 2E-CONTRIBUTION TO FROZEN CORE ENERGY -----
C
  700 CONTINUE
      IF(DOFOCK  .AND.  NCORB.GT.0) THEN
         IF(GOPARR) CALL DDI_BCAST(1720,'F',F,NUM2,MASTER)
         CALL DSCAL(NUM2,PT5,F,1)
         II = 0
         DO 750 I=1,NUM
            II = II+I
            F(II) = F(II)+F(II)
  750    CONTINUE
         CALL SYMH(F,WRK,IA)
         ECORE = ECORE+TRACEP(D,F,NUM)
         ECORE = ECORE+ECORE
      END IF
      RETURN
C
      END
C*MODULE TRANS   *DECK TR2MC
      SUBROUTINE TR2MC(XPQRJ,C,X,NUM,NDIM,NORB,NORBS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION XPQRJ(NORBS,NDIM,NUM),C(NUM,*),X(NUM,*)
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
C
      DO 20 MPQ=1,NDIM
         DO 10 MJ=1,NORBS
         DO 10 MR=1,NUM
            X(MR,MJ) = XPQRJ(MJ,MPQ,MR)
   10    CONTINUE
         DO 20 MY=1,NORB
            CALL DGEMV('T',NUM,MIN(MY,NORBS),ONE,X(1,1),NUM,
     *                 C(1,MY),1,ZERO,XPQRJ(1,MPQ,MY),1)
   20 CONTINUE
      RETURN
      END
C*MODULE TRANS   *DECK TR3MC
      SUBROUTINE TR3MC(XPQRJ,C,X,Y,WRK,IWRK,NUM,MOSIRP,NFT,NOC,NUM2,
     *           DBUG,TOL,NO,XBUF,IXBUF)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION WRK(*),IWRK(*),X(NUM,*),Y(NUM,*),C(NUM,*),MOSIRP(*),
     *          XPQRJ(NOC,NUM2,*),XBUF(1),IXBUF(1)
C
      PARAMETER (MXSH=1000, MXATM=500, MXAO=2047)
C
      LOGICAL DBUG,GOPARR,DSKWRK,MASWRK,PACK2E,SYM2EI
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,IPOPLE
      COMMON /IOFILE/ IR,IW,IP,NFTI,NFTO,IDAF,NAV,IODA(400)
      COMMON /MCPAR / NFZC,NCORBS,NCI,NORBS,NORB,NUM1
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                TSYM(432),INVT(48),NT
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
C
C     ----- THREE INTEGRALS TYPES ARE NEEDED FOR A 2ND-ORDER MCSCF:
C           ( {VAL+VIRT} (VAL+VIRT} | {CORE} {CORE}     )
C           ( {VAL+VIRT} {ALL}      | {VAL}  {CORE+VAL} )
C           ( {VIRT}     {CORE+VAL} | {VIRT} {CORE+VAL} )
C
      IF (DBUG) CALL TR2OUT(0,0,0,0,0,ZERO,-1)
      NUMX = 0
      NREC = 0
C
      DO 600 MY=1,NORB
      DO 600 MJ=1,MIN(MY,NORBS)
         MPQ=0
         DO 10 MP=1,NUM
         DO 10 MQ=1,MP
            MPQ=MPQ+1
            X(MP,MQ)=XPQRJ(MJ,MPQ,MY)
            X(MQ,MP)=X(MP,MQ)
   10    CONTINUE
         IF(MY.LE.NCORBS) THEN
C
C     ----- (PQ|YJ) IS (PQ|{CORE}{CORE}); WILL BE TRANSFORMED TO
C     ----- ({VAL+VIRT}{VAL+VIRT}|{CORE}{CORE})
C
            CALL MRARBR(X,NUM,NUM,NUM,C(1,NCORBS+1),
     *                  NUM,NORB-NCORBS,Y,NUM)
            M1 = IA(NCORBS+1)+1
            MPQ = 0
            DO 230 MA=1,NORB-NCORBS
               DO 210 MX=1,NCORBS
                  WRK(MPQ+MX)=ZERO
  210          CONTINUE
               MPQ = MPQ+NCORBS
               IF(NT.EQ.1) THEN
                  CALL DGEMV('T',NUM,MA,ONE,C(1,1+NCORBS),NUM,
     *                       Y(1,MA),1,ZERO,WRK(MPQ+1),1)
                  MPQ = MPQ+MA
               ELSE
                  DO 220 MX=1,MA
                     MPQ = MPQ+1
                     IF(SYM2EI(MOSIRP(MA+NCORBS),MOSIRP(MX+NCORBS),
     *                         MOSIRP(MY),MOSIRP(MJ))) THEN
                        DUM = DDOT(NUM,Y(1,MA),1,C(1,MX+NCORBS),1)
                     ELSE
                        DUM = ZERO
                     END IF
                     WRK(MPQ)=DUM
  220             CONTINUE
               END IF
  230       CONTINUE
C
         ELSE IF(MY.LE.NORBS) THEN
C
C     ----- (PQ|YJ) IS (PQ|{VAL}{CORE+VAL}); WILL BE TRANSFORMED TO
C     ----- ({VAL+VIRT}{ALL}|{VAL}{CORE+VAL})
C
            CALL MRARBR(X,NUM,NUM,NUM,C(1,MY),
     *                  NUM,NORB-MY+1,Y,NUM)
            M1 = IA(MY)+1
            MPQ= 0
            DO 335 MA=MY,NORB
               DO 330 MX=1,MA
                  MPQ=MPQ+1
                  IF(MA.EQ.MY .AND. MX.LT.MJ) THEN
                     DUM = ZERO
                  ELSE IF(SYM2EI(MOSIRP(MA),MOSIRP(MX),MOSIRP(MY),
     *                           MOSIRP(MJ))) THEN
                     DUM = DDOT(NUM,Y(1,MA-MY+1),1,C(1,MX),1)
                  ELSE
                     DUM = ZERO
                  END IF
                  WRK(MPQ)=DUM
  330          CONTINUE
  335       CONTINUE
C
         ELSE
C
C     ----- (PQ|YJ) IS (PQ|{VIRT}{CORE+VAL)); WILL BE TRANSFORMED TO
C     ----- ({VIRT}{CORE+VAL}|{VIRT}{CORE+VAL})
C
            CALL MRARBR(X,NUM,NUM,NUM,C(1,MY),
     *                  NUM,NORB-MY+1,Y,NUM)
            M1 = IA(MY)+1
            MPQ= 0
            DO 440 MA=MY,NORB
            DO 435 MX=1,NORBS
               MPQ=MPQ+1
               IF(MA.EQ.MY .AND. MX.LT.MJ) THEN
                  DUM = ZERO
               ELSE IF(SYM2EI(MOSIRP(MA),MOSIRP(MX),MOSIRP(MY),
     *                        MOSIRP(MJ))) THEN
                  DUM = DDOT(NUM,Y(1,MA-MY+1),1,C(1,MX),1)
               ELSE
                  DUM = ZERO
               END IF
               WRK(MPQ)=DUM
  435       CONTINUE
            DO 440 MX=NORBS+1,MA
               MPQ=MPQ+1
               WRK(MPQ)=ZERO
  440       CONTINUE
C
         END IF
C
         MYTEMP = MY
         MJTEMP = MJ
         CALL TRFWT(NFT,WRK,NUMX,IWRK,XBUF,MPQ,M1,TOL,NREC,DBUG,
     *              MYTEMP,MJTEMP,IXBUF)
  600 CONTINUE
C
      IF (DBUG) CALL TR2OUT(0,0,0,0,0,ZERO,1)
      NINT = NREC*NINTMX+NUMX
      NREC = NREC+1
      NUMX = -NUMX
      CALL PWRIT(NFT,XBUF,IXBUF,NUMX,NINTMX)
      IF (MASWRK.AND.DBUG) WRITE (IW,9008) NINT,NREC,NINTMX
C
      NO = NINT
      RETURN
 9008 FORMAT(" TOTAL NUMBER OF TRANSFORMED 2E-INTEGRALS KEPT = ",I10,
     +     4H IN ,I10,14H RECORD(S) OF ,I5,11H INTEGRALS.)
      END
C*MODULE TRANS   *DECK TR5MC
      SUBROUTINE TR5MC(XPQRJ,F,WRK,C,CO,D,XX,IX,
     *                 NDIM,NUM,NCORB,NOC,
     *                 MINPQ,MAXPQ,IJK,DOFOCK,ECORE,
     *                 GHONDO,XINTS,DDIJ,MAXG,NSH2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXAO=2047)
C
      DIMENSION XPQRJ(NOC,NDIM,*),F(*),C(NUM,*),CO(NUM,*),D(*),
     *          XX(*),IX(*),GHONDO(MAXG),XINTS(NSH2),DDIJ(*),WRK(*)
C
      LOGICAL PACK2E,DOFOCK,DIRTRF
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IJ,IPK,IDAF,NAV,IODA(400)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,IPOPLE
      COMMON /DIRMEM/ IDUM(6),MDIM,MNPQ,MXPQ
      COMMON /DIRCAS/ I2CASE
      COMMON /TRFOPT/ CUTTRF,NWDTRF,MPTRAN,ITRFAO,NOSYMT,DIRTRF
C
      PARAMETER (ZERO=0.0D+00,PT5=0.5D+00)
C
      NUM2 = (NUM*NUM+NUM)/2
C
C     ----- GET 1E-CONTRIBUTION TO FROZEN CORE -----
C
      IF(DOFOCK  .AND.  NCORB.NE.0) THEN
         ECORE=ZERO
         CALL GETDEN(D,C,NUM,NCORB)
         CALL DAREAD(IDAF,IODA,F,NUM2,11,0)
         ECORE = TRACEP(D,F,NUM)
         CALL VCLR(F,1,NUM2)
      END IF
C
      CALL VCLR(XPQRJ,1,NUM*NOC*NDIM)
C
C     ----- PROCESS 2E-INTEGRALS -----
C
      IF(.NOT.DIRTRF) THEN
C
         CALL SEQREW(IJK)
  200    CONTINUE
C
C              READ AO INTEGRAL LIST
C
         CALL PREAD(IJK,XX,IX,NXX,NINTMX)
         IF(NXX.EQ.0) GO TO 250
         NINT=ABS(NXX)
         IF(NINT.GT.NINTMX) CALL ABRT
C
C              CARRY OUT FIRST INDEX TRANSFORMATION
C
         IF(DOFOCK  .AND.  NCORB.NE.0  .AND.  MINPQ.EQ.1) THEN
            CALL ONEPFF(XPQRJ,CO,F,D,NDIM,MINPQ,MAXPQ,
     *                  NUM,NOC,XX,IX,NXX)
         ELSE
            CALL ONEPRF(XPQRJ,CO,NDIM,MINPQ,MAXPQ,
     *                  NUM,NOC,XX,IX,NXX)
         END IF
C
  250    CONTINUE
         IF(NXX.LE.0) THEN
            CALL SEQREW(IJK)
            GO TO 700
         END IF
         GO TO 200
      END IF
C
C     ----- 2E- INTEGRALS FROM DIRECT COMPUTATION  -----
C
      IF(DIRTRF) THEN
         IF(NCORB.NE.0) THEN
            I2CASE=4
         ELSE
            I2CASE=5
         END IF
         MDIM=NDIM
         MNPQ=MINPQ
         MXPQ=MAXPQ
         CALL DIRJK(GHONDO,XINTS,DDIJ,XX,IX,MAXG,NSH2)
      END IF
C
C     ----- POST-PROCESSING OF FOCK MATRIX -----
C     ----- 2E-CONTRIBUTION TO FROZEN CORE ENERGY -----
C
  700 CONTINUE
      IF(DOFOCK  .AND.  NCORB.NE.0  .AND.  MINPQ.EQ.1) THEN
         CALL DSCAL(NUM2,PT5,F,1)
         II = 0
         DO 750 I=1,NUM
            II = II+I
            F(II) = F(II)+F(II)
  750    CONTINUE
         CALL SYMH(F,WRK,IA)
         ECORE = ECORE+TRACEP(D,F,NUM)
         ECORE = ECORE+ECORE
      END IF
      RETURN
C
      END
C*MODULE TRANS   *DECK TR6MC
      SUBROUTINE TR6MC(V,IDAF20,IODA20,X,Y,WRK,IWRK,NUM,MOSIRP,
     *           NFTO,NUM2,DBUG,TOL,NO,XBUF,IXBUF)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION WRK(*),IWRK(*),X(NUM,*),Y(NUM,*),IODA20(*),V(NUM,*),
     *          MOSIRP(*),XBUF(*),IXBUF(*)
C
      PARAMETER (MXSH=1000, MXATM=500, MXAO=2047)
C
      LOGICAL DBUG,PACK2E,GOPARR,DSKWRK,MASWRK,SYM2EI
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,IPOPLE
      COMMON /IOFILE/ IR,IW,IP,NFTI,NFTX,IDAF,NAV,IODA(400)
      COMMON /MCPAR / NFZC,NCORBS,NCI,NORBS,NORB,NUM1
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                TSYM(432),INVT(48),NT
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
C
C     ----- THREE INTEGRALS TYPES ARE NEEDED FOR A 2ND-ORDER MCSCF:
C           ( {VAL+VIRT} (VAL+VIRT} | {CORE} {CORE}     )
C           ( {VAL+VIRT} {ALL}      | {VAL}  {CORE+VAL} )
C           ( {VIRT}     {CORE+VAL} | {VIRT} {CORE+VAL} )
C
      IF (DBUG) CALL TR2OUT(0,0,0,0,0,ZERO,-1)
      NUMX = 0
      NREC = 0
      MYJ=0
      DO 60 MY=1,NORB
      DO 60 MJ=1,MIN(MY,NORBS)
         MYJ=MYJ+1
         CALL RAREAD(IDAF20,IODA20,WRK,NUM2,MYJ,0)
         MPQ=0
         DO 10 MP=1,NUM
         DO 10 MQ=1,MP
            MPQ=MPQ+1
            X(MP,MQ)=WRK(MPQ)
            X(MQ,MP)=WRK(MPQ)
   10    CONTINUE
C
         IF(MY.LE.NCORBS) THEN
C
C     ----- (PQ|YJ) IS (PQ|{CORE}{CORE}); WILL BE TRANSFORMED TO
C     ----- ({VAL+VIRT}{VAL+VIRT}|{CORE}{CORE})
C
            CALL MRARBR(X,NUM,NUM,NUM,V(1,NCORBS+1),
     *                  NUM,NORB-NCORBS,Y,NUM)
            M1 = IA(NCORBS+1)+1
            MPQ = 0
            DO 25 MA=1,NORB-NCORBS
               DO 15 MX=1,NCORBS
                  WRK(MPQ+MX)=ZERO
   15          CONTINUE
               MPQ = MPQ+NCORBS
               IF(NT.EQ.1) THEN
                  CALL DGEMV('T',NUM,MA,ONE,V(1,1+NCORBS),NUM,
     *                       Y(1,MA),1,ZERO,WRK(MPQ+1),1)
                  MPQ = MPQ+MA
               ELSE
                  DO 20 MX=1,MA
                     MPQ = MPQ+1
                     IF(SYM2EI(MOSIRP(MA+NCORBS),MOSIRP(MX+NCORBS),
     *                         MOSIRP(MY),MOSIRP(MJ))) THEN
                        DUM = DDOT(NUM,Y(1,MA),1,V(1,MX+NCORBS),1)
                     ELSE
                        DUM = ZERO
                     END IF
                     WRK(MPQ)=DUM
   20             CONTINUE
               END IF
   25       CONTINUE
C
         ELSE IF(MY.LE.NORBS) THEN
C
C     ----- (PQ|YJ) IS (PQ|{VAL}{CORE+VAL}); WILL BE TRANSFORMED TO
C     ----- ({VAL+VIRT}{ALL}|{VAL}{CORE+VAL})
C
            CALL MRARBR(X,NUM,NUM,NUM,V(1,MY),
     *                  NUM,NORB-MY+1,Y,NUM)
            M1 = IA(MY)+1
            MPQ= 0
            DO 30 MA=MY,NORB
            DO 30 MX=1,MA
               MPQ=MPQ+1
               IF(MA.EQ.MY .AND. MX.LT.MJ) THEN
                  DUM = ZERO
               ELSE IF(SYM2EI(MOSIRP(MA),MOSIRP(MX),MOSIRP(MY),
     *                        MOSIRP(MJ))) THEN
                  DUM = DDOT(NUM,Y(1,MA-MY+1),1,V(1,MX),1)
               ELSE
                  DUM = ZERO
               END IF
               WRK(MPQ)=DUM
   30       CONTINUE
C
         ELSE
C
C     ----- (PQ|YJ) IS (PQ|{VIRT}{CORE+VAL)); WILL BE TRANSFORMED TO
C     ----- ({VIRT}{CORE+VAL}|{VIRT}{CORE+VAL})
C
            CALL MRARBR(X,NUM,NUM,NUM,V(1,MY),
     *                  NUM,NORB-MY+1,Y,NUM)
            M1 = IA(MY)+1
            MPQ= 0
            DO 40 MA=MY,NORB
            DO 35 MX=1,NORBS
               MPQ=MPQ+1
               IF(MA.EQ.MY .AND. MX.LT.MJ) THEN
                  DUM = ZERO
               ELSE IF(SYM2EI(MOSIRP(MA),MOSIRP(MX),MOSIRP(MY),
     *                        MOSIRP(MJ))) THEN
                  DUM = DDOT(NUM,Y(1,MA-MY+1),1,V(1,MX),1)
               ELSE
                  DUM = ZERO
               END IF
               WRK(MPQ)=DUM
   35       CONTINUE
            DO 40 MX=NORBS+1,MA
               MPQ=MPQ+1
               WRK(MPQ)=ZERO
   40       CONTINUE
C
         END IF
C
         MYTEMP = MY
         MJTEMP = MJ
         CALL TRFWT(NFTO,WRK,NUMX,IWRK,XBUF,MPQ,M1,TOL,NREC,DBUG,
     *              MYTEMP,MJTEMP,IXBUF)
   60 CONTINUE
C
      IF (DBUG) CALL TR2OUT(0,0,0,0,0,ZERO,1)
      NINT = NREC*NINTMX+NUMX
      NREC = NREC+1
      NUMX = -NUMX
      CALL PWRIT(NFTO,XBUF,IXBUF,NUMX,NINTMX)
      IF (MASWRK.AND.DBUG) WRITE (IW,9008) NINT,NREC,NINTMX
C
      NO = NINT
      RETURN
 9008 FORMAT(" TOTAL NUMBER OF TRANSFORMED 2E-INTEGRALS KEPT = ",I10,
     +     4H IN ,I10,14H RECORD(S) OF ,I5,11H INTEGRALS.)
      END
C*MODULE TRANS   *DECK TRFBRD
      SUBROUTINE TRFBRD(NFT,XX,IX,NINTMX,JFLG,NCPU)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL GOPARR,DSKWRK,MASWRK,DIRSCF,FDIFF,DIRTRF
      DIMENSION XX(NINTMX),IX(NINTMX),JFLG(0:NCPU-1)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /OPTSCF/ DIRSCF,FDIFF
      COMMON /TRFOPT/ CUTTRF,NWDTRF,MPTRAN,ITRFAO,NOSYMT,DIRTRF
C
C     ----- INTEGRAL TRANSFORMATION BROADCAST ROUTINE -----
C     THIS ROUTINE IS USED WHEN A NODE IS NOT ASSIGNED AN ACTUAL
C     TRANSFORMATION PASS, BUT MUST CONTRIBUTE ITS SUBSET OF THE
C     AO INTEGRALS ON ITS DISK TO THE OTHER NODES, WHICH ARE STILL
C     DOING WORK.  IF THE INTEGRAL LIST IS DUPLICATED ON EACH NODE,
C     OR IF AO INTEGRALS ARE BEING RECOMPUTED IN DIRECT MODE, PUNT.
C
      IF(DIRSCF) RETURN
      IF(DIRTRF) RETURN
      IF(ITRFAO.EQ.1) RETURN
C
      DO 100 KAP=0,NPROC-1
         JFLG(KAP)=1
  100 CONTINUE
      KAP = -1
      IFLG=  1
C
      CALL SEQREW(NFT)
  200 CONTINUE
         CALL PREADP(NFT,XX,IX,NXX,NINTMX,
     *               KAP,IFLG,JFLG,NPROC)
         IF(IFLG.EQ.0) THEN
            CALL SEQREW(NFT)
            RETURN
         END IF
      GO TO 200
      END
C*MODULE TRANS   *DECK TRFIN
      SUBROUTINE TRFIN
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (NNAM=6)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,DIRTRF,DIRSCF,FDIFF
C
      DIMENSION QNAM(NNAM),KQNAM(NNAM)
C
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /OPTSCF/ DIRSCF,FDIFF
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TRFOPT/ CUTTRF,NWDTRF,MPTRAN,ITRFAO,NOSYMT,DIRTRF
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA TRANS/8HTRANS   /
#else
      CHARACTER*8 :: TRANS_STR
      EQUIVALENCE (TRANS, TRANS_STR)
      DATA TRANS_STR/"TRANS   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QNAM/8HNWORD   ,8HMPTRAN  ,8HCUTTRF  ,8HDIRTRF  ,8HAOINTS  ,
     *          8HNOSYM   /
#else
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"NWORD   ","MPTRAN  ","CUTTRF  ","DIRTRF  ",
     *          "AOINTS  ",
     *          "NOSYM   "/
#endif
      DATA KQNAM/1,1,3,0,5,1/
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DUP,DIST/8HDUP     ,8HDIST    /
#else
      CHARACTER*8 :: DIST_STR
      EQUIVALENCE (DIST, DIST_STR)
      CHARACTER*8 :: DUP_STR
      EQUIVALENCE (DUP, DUP_STR)
      DATA DUP_STR,DIST_STR/"DUP     ","DIST    "/
#endif
C
C     ----- READ $TRANS INPUT DATA -----
C
      NWDTRF = 0
      MPTRAN = 0
      CUTTRF = 1.0D-09
      DIRTRF = .FALSE.
      IF(ITRFAO.EQ.1) AOINTS=DUP
      IF(ITRFAO.EQ.2) AOINTS=DIST
      NOSYMT=0
C
      JRET = 0
      CALL NAMEIO(IR,JRET,TRANS,NNAM,QNAM,KQNAM,
     *            NWDTRF,MPTRAN,CUTTRF,DIRTRF,AOINTS,NOSYMT,
     *            0,0,0,   0,0,0,0,0,
     *   0,0,0,0,0,  0,0,0,0,0,
     *   0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,
     *   0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0)
      IF(JRET.EQ.2) THEN
         IF(MASWRK) WRITE(IW,9010)
         CALL ABRT
      END IF
      NERR = 0
C
      ITRFAO = 0
      IF(AOINTS.EQ.DUP)  ITRFAO=1
      IF(AOINTS.EQ.DIST) ITRFAO=2
      IF(ITRFAO.EQ.0) THEN
         IF(MASWRK) WRITE(IW,*) 'ILLEGAL VALUE FOR -AOINTS-'
         NERR = NERR+1
      END IF
C
C        -DIRSCF- FORCES -DIRTRF-
C        AND CONVERSELY, E.G. MP2 RUNS MAY READ $TRANS AND SEE A DIRTRF!
C
      IF(DIRSCF) DIRTRF=.TRUE.
      IF(DIRTRF) DIRSCF=.TRUE.
C
      IF(MASWRK) WRITE(IW,9000) NWDTRF,CUTTRF,MPTRAN,DIRTRF,AOINTS
C
      IF(NERR.GT.0) THEN
         IF(MASWRK) WRITE(IW,9010)
         CALL ABRT
         STOP
      END IF
      RETURN
C
 9000 FORMAT(/5X,31("-")/5X,'INTEGRAL TRANSFORMATION OPTIONS'/
     *        5X,31(1H-)/
     *       5X,'NWORD  =',I8,5X,'CUTOFF =',1P,E8.1/
     *       5X,'MPTRAN =',I8,5X,'DIRTRF =',L8/
     *       5X,'AOINTS =',A8)
 9010 FORMAT(1X,'*** PLEASE FIX THE ERROR IN YOUR $TRANS INPUT ***')
      END
C*MODULE TRANS   *DECK TRFMCX
      SUBROUTINE TRFMCX(NPRINT,ICORBS,IORBS,IORB,DOFOCK,DOEXCH,
     *                  DDITRF,DOOOOO,DOVOOO,DOVVOO,DOVOVO)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION METHOD
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXGSH=30, MXG2=MXGSH*MXGSH,
     *           MXATM=500, MXRT=100, NMO=500, MXAO=2047,
     *           MXNORO=250)
C
      LOGICAL DOFOCK,DOEXCH,SOME,DEBUG,DEBG1,PACK2E,
     *        MASWRK,DSKWRK,GOPARR,NXT,DIRTRF,CANONC,FCORE,FORS,
     *        NOCI,EKT,LINSER,ABEL,ABELPT,MOIDON,EDCOMP,DIPDCM,
     *        DEPRNT,QADDCM,ZDO,POLDCM,POLANG,POLAPP,KMIDPT,
     *        NOTRF,DDITRF,DOOOOO,DOVOOO,DOVVOO,DOVOVO
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,SDET,SZDET,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCORDT,NACTDT,
     *                NORBDT,NADET,NBDET,KDET,KSTDET,IRTDT,IPURES,
     *                MAXW1,NITDET,MAXP,NCIDET,IGPDET,KSTSYM,NFTGCI
      COMMON /DIRMEM/ IPQKL,IVEC,IDEN,IFCK,NBF,NOC,NDIM,MNPQ,MXPQ
      COMMON /EDCMP / MOIDNO(5,NMO),ZIJ(NMO),ZMO(5,NMO),IJMO(2,NMO),
     *                MOIJ(NMO),NMOIJ(NMO),NMOAT(NMO),OCCUP(NMO),MOIDON,
     *                EDCOMP,DIPDCM,IPROT(5),NPROT,DEPRNT,QADDCM,ZDO,
     *                POLDCM,POLANG,POLAPP,KMIDPT
      COMMON /ENRGYS/ EN,EELCT,ETOT,SZ,SZZ,ECORE,ESCF,EERD,E1,E2,
     *                VEN,VEE,EPOT,EKIN,ESTATE(MXRT),STATN
      COMMON /FMCOM / X(1)
      COMMON /GUGWFN/ NNFZC,NNMCC,NNDOC,NNAOS,NNBOS,NNALP,NNVAL,NNEXT,
     *                NNFZV,IFORS,IEXCIT,ICICI,NOIRR
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /MCINP / METHOD,CISTEP,ACURCY,ENGTOL,DAMP,MICIT,NWRDMC,
     *                NORBMC,NOROT(2,MXNORO),MOFRZ(15),NPFLG(10),
     *                NOFO,CANONC,FCORE,FORS,NOCI,EKT,LINSER
      COMMON /MCPAR / NFZC,NCORBS,NVAL,NORBS,NORB,NUM1
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /NTNOPT/ ENERGY,ENERG0,DEMAX,SQCDF,ITER,MICRO,NOTRF
      COMMON /ORBSET/ NORBMX,KORBS,KCORBS,NLEVS,NNA,NNB,NC,NSYM,MSYM,
     *                IDOCC,IIVAL,IMCC,ISYM(MXAO),ICODE(MXAO),
     *                NLCS(MXAO),LEVPT(MXAO),LEVNR(MXAO),IOUT(MXAO),
     *                NREFS,IEXCT,NFOCI,INTACT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCKLAB/ LABSIZ
      COMMON /PSILVL/ IPSI
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /SYMMOL/ GROUP,COMPLX,IGROUP,NAXIS,ILABMO,ABEL
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                TSYM(432),INVT(48),NT
      COMMON /TRFOPT/ CUTTRF,NWDTRF,MPTRAN,ITRFAO,NOSYMT,DIRTRF
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      PARAMETER (ZERO=0.0D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RMC/8HMCSCF   /, CHECK/8HCHECK   /
#else
      CHARACTER*8 :: RMC_STR
      EQUIVALENCE (RMC, RMC_STR)
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA RMC_STR/"MCSCF   "/, CHECK_STR/"CHECK   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DBUG/8HDEBUG   /, DBUGME/8HTRNSF   /
      DATA GUGA,ALDET,GENCI,ORMAS
     *       /8HGUGA    ,8HALDET   ,8HGENCI   ,8HORMAS   /
#else
      CHARACTER*8 :: DBUG_STR
      EQUIVALENCE (DBUG, DBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA DBUG_STR/"DEBUG   "/, DBUGME_STR/"TRNSF   "/
      CHARACTER*8 :: GUGA_STR
      EQUIVALENCE (GUGA, GUGA_STR)
      CHARACTER*8 :: ALDET_STR
      EQUIVALENCE (ALDET, ALDET_STR)
      CHARACTER*8 :: GENCI_STR
      EQUIVALENCE (GENCI, GENCI_STR)
      CHARACTER*8 :: ORMAS_STR
      EQUIVALENCE (ORMAS, ORMAS_STR)
      DATA GUGA_STR,ALDET_STR,GENCI_STR,ORMAS_STR
     *       /"GUGA    ","ALDET   ","GENCI   ","ORMAS   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA PROP/8HPROP    /
#else
      CHARACTER*8 :: PROP_STR
      EQUIVALENCE (PROP, PROP_STR)
      DATA PROP_STR/"PROP    "/
#endif
C
C           ----- PARTIAL INTEGRAL TRANSFORMATION PROGRAM -----
C     -- TRANSFORM INTEGRALS AND OPTIONALLY FORM CORE FOCK OPERATOR --
C     ALL INTEGRALS WITH 0,1,2 VIRTUAL ORBITAL INDEXES ARE PRODUCED.
C
C     -NPRINT- SHOULD BE 1,2 FOR DEBUG OUTPUT OF 1,2 E- TRANSFORM.
C     -ICORBS- IS NUMBER OF CORE ORBS.  IF NEGATIVE, SKIP FROZEN CORE E
C     -IORBS- IS NUMBER OF OCCUPIED ORBS INCLUDED IN THE TRANSFORM,
C        INCLUDING ANY CORE ORBITALS.
C     -IORB- IS THE TOTAL NUMBER OF ORBS TO INCLUDE IN THE TRANSFORM.
C     -DOFOCK- IS A FLAG TO CONTROL GENERATION OF CORE FOCK MATRICES:
C            SUPPOSE THERE ARE 3 CORES, 5 TOTAL OCCUPIED.
C        IF FALSE, NO FOCK MATRICES ARE MADE, AND THE INDEX LABELS
C            PACKED UP FOR THE OCCUPIED ORBS ARE 1 AND 2.  NO CORE
C            ORBITAL INTEGRALS ARE PRODUCED.
C        IF TRUE, THE FOCK MATRICES ARE MADE, AND THE INDEX LABELS
C            PACKED UP FOR THE OCCUPIED ORBS ARE 4 AND 5.  ALL
C            INTEGRALS INVOLVING THE CORE ORBITALS ARE PRODUCED
C            AND INCLUDED IN THE OUTPUT FILES, LABELED 1,2,3.  NOTE:
C            THIS OPTION IS NOT TESTED, AND THUS MAY NOT BE WORKING.
C     -DOEXCH- INDICATES EXCHANGE INTEGRALS ARE NOT AVAILABLE ON THE
C        DAF FILE AT THIS GEOMETRY, AND THEREFORE MUST BE COMPUTED.
C
C     -DDITRF- CONTROL SELECTION OF A DISTRIBUTED MEMORY PARALLEL
C              INTEGRAL TRANSFORMATION.  THE FLAGS -DOOOO-, -DOVOOO-,
C              -DOVOVO-, AND -DOVVOO- ARE MEANINGLESS UNLESS -DDITRF-
C              IS TURNED ON, IN WHICH CASE, THEY REQUEST FORMATION OF
C              MO INTEGRAL CLASSES (OO|OO),(VO|OO),(VV|OO),(VO|VO).
C
      L0 = NQMT
      L1 = NUM
      L2 = (L1*L1+L1)/2
      L3 =  L1*L1
C
C        LABSIZ IS NORMALLY SET BY -DEBUT- BUT THAT MIGHT NOT BE CALLED
C
      IF(RUNTYP.EQ.PROP) THEN
         MAXAO=255
         LABSIZ = 1
         IF(NUM.GT.MAXAO) LABSIZ = 2
      END IF
C
      SOME  = NPRINT.NE.-5 .AND. MASWRK
      DEBG1 = NPRINT.EQ. 1 .AND. MASWRK
      DEBUG = NPRINT.EQ. 2 .AND. MASWRK
      IF (EXETYP.EQ.DBUG   .OR.   EXETYP.EQ.DBUGME) THEN
         SOME = .TRUE. .AND. MASWRK
         DEBUG= .TRUE. .AND. MASWRK
      END IF
      IF(SOME) WRITE(IW,9000)
C
      NREC=0
      NINT=0
C
      CALL SEQOPN(IJKT,'MOINTS','UNKNOWN',.FALSE.,'UNFORMATTED')
C
C     ----- PROVIDE DEFAULT VALUES FOR NCORBS,NORB -----
C     THESE MAY HAVE BEEN PASSED IN THE CALLING ARGUMENTS.
C     NCORBS IS THE NUMBER OF -FZC- OR -MCC- GIVEN IN $DRTINP
C     NORBS  IS THE NUMBER OF OCCUPIED ORBITALS FOR MCSCF,
C            AND IS THE NUMBER OF OCCUPIED, NON-FZC FOR CI.
C     NORB IS THE TOTAL NUMBER TO BE INCLUDED (I.E. INCLUDING VIRTUALS)
C
      CALL DERCHK(NDER)
      JCORBS = IABS(ICORBS)
      IF(JCORBS+IORBS.EQ.0) THEN
         IF(SCFTYP.EQ.RMC  .OR.  CITYP.EQ.GUGA) THEN
C
C               TRANSFORMATION FOR A CI COMPUTATION...
C
            IF(ICICI.EQ.1) THEN
C
C               CI GRADIENT RUNS MUST INCLUDE CORE ORBITALS IN TRANSFORM
C
               IF(NDER.GT.0) THEN
                  NCORBS = 0
                  NORBS  = KORBS
                  NORB   = L0
C
C               CIS ENERGY NEEDS ONLY A PARTIAL TRANSFORMATION,
C               OTHER CI COMPUTATIONS NEED A FULL TRANSFORMATION
C
               ELSE
                  IF (IEXCIT.EQ.1  .AND.  NNEXT.EQ.0) THEN
                     NCORBS = KCORBS
                     NORBS  = KCORBS+NNDOC+NNALP+NNAOS+NNBOS
                     NORB   = KCORBS+NNDOC+NNALP+NNAOS+NNBOS+NNVAL
                  ELSE
                     NCORBS = KCORBS
                     NORBS  = KCORBS+KORBS
                     NORB   = KCORBS+KORBS
                  END IF
               END IF
            END IF
C
C               TRANSFORMATION FOR A MCSCF COMPUTATION...
C
            IF(ICICI.EQ.0) THEN
               IF(CISTEP.EQ.GUGA) THEN
                  NCORBS = 0
                  NORBS  = KORBS
                  NORB   = L0
               END IF
               IF(CISTEP.EQ.ALDET  .OR.  CISTEP.EQ.GENCI
     *               .OR.  CISTEP.EQ.ORMAS) THEN
                  NCORBS = 0
                  NORBS  = NCORSV+NACTDT
                  NORB   = L0
               END IF
            END IF
         END IF
C
C               TRANSFORMATION FOR HESSIAN OR ORBITAL LOCALIZATION...
C
      ELSE
         NCORBS = JCORBS
         NORBS  = IORBS
         NORB   = IORB
      END IF
      NVAL = NORBS - NCORBS
      IF (SOME) WRITE (IW,9010) NCORBS,NORBS,NORB,L1,CUTTRF
C
C     ----- DISTRIBUTED MEMORY PARALLEL INTEGRAL TRANSFORMATION -----
C              IMPLEMENTED BY GRAHAM FLETCHER, FEB 14, 2002
C
C     DO NOT SKIP CORE INDICES IN TRANSFORMATION FOR MCSCF
C
      IF (DDITRF) THEN
C
C  ENSURE THE PAIR-INDEX IS INITIALIZED (AGAIN!)
C
        II = 0
        DO I = 1, MXAO
          IA(I) = II
          II = II + I
        END DO
C
C  DISTINCT NAMES
C
        NBF  = NUM
        NMOS = NORB
        NOCC = NORBS
        NCOR = NCORBS
        NACT = NOCC - NCOR
C
C  DRIVER FOR PARALLEL DIRECT 4-INDEX TRANSFORMATION
C
        CALL TRANDDI(NMOS,NOCC,NCOR,SOME,DOOOOO,DOVOOO,DOVVOO,DOVOVO)
C
C  DEPENDENT PARAMETERS
C
        NBSQ = NBF*NBF
        NBTR = (NBF*NBF+NBF)/2
        NMTR = (NMOS*NMOS+NMOS)/2
        NATR = (NACT*NACT+NACT)/2
C
C  DATA STRUCTURE LENGTHS
C
        LVECT = NBSQ
        LONEL = NBTR
        LHAMO = NMTR
        LWORK = NBF
C
C  ALLOCATE MEMORY: COMPUTE ADDRESSES AND CHECK TOTAL
C
        CALL VALFM(LOADFM)
        IVECT = LOADFM + 1            !  MO COEFFICIENTS
        IONEL = IVECT  + LVECT        !  1-EL INTEGRALS OVER AOS
        IHAMO = IONEL  + LONEL        !  1-EL INTEGRALS OVER MOS
        IWORK = IHAMO  + LHAMO        !  WORKSPACE
        LAST  = IWORK  + LWORK
        NEED  = LAST   - LOADFM
        CALL GETFM(NEED)
        IF (EXETYP.NE.CHECK) THEN
C
C  READ MO COEFFICIENTS (FOCK EIGEN'VECT'ORS)
C
          CALL DAREAD(IDAF,IODA,X(IVECT),LVECT,15,0)
C
C  READ 1-EL (CORE-HAMILTONIAN) INTEGRALS
C
          CALL DAREAD(IDAF,IODA,X(IONEL),LONEL,11,0)
C
C  TRANSFORM 1-EL INTEGRALS TO THE MO BASIS
C
          CALL TFTRI(X(IHAMO),X(IONEL),X(IVECT),X(IWORK)
     *,              NMOS,NBF,NBF)
C
C  SAVE 1-EL INTEGRALS OVER MOS FOR USE IN NEWTON
C
          CALL DAWRIT(IDAF,IODA,X(IHAMO),LHAMO,355,0)
C
C  FORM 2-EL CORE FOCK OPERATOR IN PARALLEL
C
          IFCOR = IONEL   !  RE-USE 1-EL INT BUFFER FOR CORE FOCK
          IBUFF = IVECT   !  RE-USE VECTORS BUFFER FOR MESSAGES
C
          CALL FCODDI(NMOS,NOCC,NCOR,X(IFCOR),X(IBUFF))
C
C  SUM 1-EL AND 2-EL TERMS OF CORE FOCK MATRIX
C
          CALL DAXPY(LHAMO,1.0D+00,X(IHAMO),1,X(IFCOR),1)
C
C  SAVE CORE FOCK MATRIX
C
          LFCOR = LHAMO
          CALL DAWRIT(IDAF,IODA,X(IFCOR),LFCOR,35,0)
C
C  COMPUTE FROZEN CORE ENERGY, /ENRGYS/...ECORE...
C
          ECORE = ZERO
          IHII = IHAMO - 1
          IFII = IFCOR - 1
          DO I = 1, NCOR
            IHII = IHII + I
            IFII = IFII + I
            ECORE = ECORE + X(IHII) + X(IFII)
          END DO
          IF(DOFOCK.AND.NCORBS.GT.0.AND.SOME) WRITE(IW,9040) ECORE
C
C  COPY FOCK ELEMENTS OVER ACTIVE MOS TO SEPARATE BUFFER
C
          IJ = IHAMO
          DO I = 1, NACT
            IN = I + NCOR
            DO J = 1, I
              JN = J + NCOR
              IJN = IA(IN) + JN
              IJH = (IFCOR-1) + IJN
              X(IJ) = X(IJH)
              IJ = IJ + 1
            END DO
          END DO
C
C  SAVE ACTIVE MO CORE FOCK ELEMENTS FOR CI (RDCI12)
C
          CALL SEQREW(IJKT)
          IF (MASWRK) CALL SQWRIT(IJKT,X(IHAMO),NATR)
        END IF   ! CHECK RUN
        CALL RETFM(NEED)
        IF(SOME) WRITE(IW,*) '... END OF INTEGRAL TRANSFORMATION ...'
        IF(SOME) CALL TIMIT(1)
        RETURN
      END IF
C
C     THIS IS THE  END OF THE DISTRIBUTED MEMORY INTEGRAL TRANSFORMATION
C
C     ----- REPLICATED MEMORY PARALLEL INTEGRAL TRANSFORMATION -----
C
      IF (DIRTRF) THEN
        IF (SOME) WRITE(IW,9020)
      ELSE
        IF (SOME) WRITE(IW,9030)
      END IF
C
      NDIM=L2
C
      CALL GOTFM(NGOTMX)
      IF(NWDTRF.LE.0) NWDTRF=NGOTMX
      NWORD=MIN(NWDTRF,NGOTMX)
C
C     ----- SET POINTERS TO MEMORY FOR DIRECT RUNS -----
C     XBUF AND IXBUF ARE OUTPUT MO INTEGRAL BUFFERS
C
      CALL BASCHK(LMAX)
                    NANGM =  4
      IF(LMAX.EQ.2) NANGM =  6
      IF(LMAX.EQ.3) NANGM = 10
      IF(LMAX.EQ.4) NANGM = 15
      MAXG = NANGM**4
      NSH2   = (NSHELL*NSHELL+NSHELL)/2
      NCPU   = NPROC
C
      CALL VALFM(LOADFM)
      LXBUF  = LOADFM + 1
      LIXBUF = LXBUF  + NINTMX
      LJFLG  = LIXBUF + NINTMX
      LAST   = LJFLG  + NPROC
      IF (DIRTRF) THEN
         LGHND  = LAST
         LXINTS = LGHND  + MAXG
         LDDIJ  = LXINTS + NSH2
         LDSH   = LDDIJ  + 16*MXG2
         LAST   = LDSH   + NSH2
      ELSE
         LGHND  = LAST
         LXINTS = LAST
         LDDIJ  = LAST
         LDSH   = LAST
      END IF
      NEEDD = LAST - LOADFM - 1
      CALL GETFM(NEEDD)
C
C        ---- GENERATE EXCHANGE INTEGRALS FOR SCHWARZ SCREENING ----
C
      IF(DIRTRF) THEN
         IF(EXETYP.EQ.CHECK) THEN
            CALL VCLR(X(LXINTS),1,NSH2)
         ELSE
            IF(DOEXCH) THEN
               CALL DEBUT(INTG76,.FALSE.,DUM,DUM,IDUM,NINTMX,0,DIRTRF)
               CALL EXCHNG(X(LXINTS),X(LGHND),X(LDDIJ),NSH2,MAXG,INTG76)
            ELSE
               CALL DAREAD(IDAF,IODA,X(LXINTS),NSH2,54,0)
            END IF
         END IF
      END IF
C
C        ---- OPTION TO DEFEAT SYMMETRY USAGE ----
C
      ABEL=ABELPT()
      IF(NOSYMT.EQ.1) CALL SYMOFF
C
C        ---- MEMORY TO STORE VECTORS AND SYMMETRY DATA ----
C
      CALL VALFM(LOADFM)
      LMOLAB = LOADFM + 1
      LMOIRP = LMOLAB + L1
      LVEC   = LMOIRP + L1
      LAST   = LVEC   + L3
      NEEDV = LAST - LMOLAB
      CALL GETFM(NEEDV)
C
C        ---- CLEAN UP AND ORTHONORMALIZE ORBITALS ----
C
      CALL DAREAD(IDAF,IODA,X(LVEC),L3,15,0)
      IF (ZDO) GO TO 10
      LQ   = LAST  + 1
      LS   = LQ    + L3
      LTMP = LS    + L2
      LAST = LTMP  + L1
      NEEDO = LAST - LQ
      CALL GETFM(NEEDO)
C
      CALL DAREAD(IDAF,IODA,X(LS),L2,12,0)
      CALL DAREAD(IDAF,IODA,X(LQ),L3,45,0)
C          TRADITIONAL VALUES ARE 1D-8 AND 1D-6 RESPECTIVELY
      IF(NT.GT.1) THEN
         IF(ABEL) THEN
            TOLZ=1.0D-09
            TOLE=1.0D-07
         ELSE
            TOLZ=1.0D-11
            TOLE=1.0D-10
         END IF
         IF(SCFTYP.EQ.RMC  .AND.  IPSI.EQ.0) THEN
            IF(ITER.EQ.1  .OR.  SQCDF.GT.1.0D-04) THEN
               TOLZ=TOLZ*10.0D+00
               TOLE=TOLE*10.0D+00
            END IF
         END IF
         CALL CLENMO(X(LVEC),L1,L0,TOLZ,TOLE,IW,.FALSE.)
      END IF
C
      CALL ORTHO(X(LQ),X(LS),X(LVEC),X(LTMP),L0,L0,L1,L2,L1)
      CALL TFSQB(X(LVEC),X(LQ),X(LTMP),L0,L1,L1)
C
      CALL DAWRIT(IDAF,IODA,X(LVEC),L3,15,0)
      CALL RETFM(NEEDO)
   10 CONTINUE
C
      IF(DOFOCK) GO TO 50
C
C        ----- ONE ELECTRON TRANSFORMATION/FROZEN CORE ENERGY -----
C
      CALL VALFM(LOADFM)
      LD    = LOADFM + 1
      LF    = LD     + L2
      LH    = LF     + L2
      LXX   = LH     + L2
      LIX   = LXX    + NINTMX
      LWRK  = LIX    + NINTMX
      LAST  = LWRK   + L2
      NEED1 = LAST - LOADFM - 1
      CALL GETFM(NEED1)
C
      CALL DAREAD(IDAF,IODA,X(LH),L2,11,0)
C
C        ---- CALCULATE FROZEN CORE ENERGY ----
C        AND MODIFY THE ONE ELECTRON INTEGRALS
C
      ECORE = ZERO
      IF(NCORBS.GT.0   .AND.  ICORBS.GE.0  .AND.  EXETYP.NE.CHECK) THEN
         IF (SOME) WRITE(IW,*) 'EVALUATING THE FROZEN CORE ENERGY...'
         CALL FZCORE(X(LVEC),X(LD),X(LF),X(LH),X(LXX),X(LIX),NINTMX,
     *               IA,L1,L2,NCORBS,X(LGHND),X(LXINTS),
     *               X(LDDIJ),X(LDSH),X(LWRK),NSH2,MAXG,INTG76)
         IF (SOME) WRITE(IW,9040) ECORE
      END IF
      CALL DAWRIT(IDAF,IODA,EN,MXRT+15,2,0)
      CALL DAWRIT(IDAF,IODA,X(LH),L2,35,0)
C
C        ---- DROP ANY FROZEN CORES FROM THE TRANSFORMATION ----
C
      LVEC   = LVEC + L1*NCORBS
      NORB   = NORB   - NCORBS
      NORBS  = NORBS  - NCORBS
      NCORBS = NCORBS - NCORBS
C
      IF (DEBG1  .OR.  DEBUG) THEN
         WRITE (IW,*) 'TRANSFORMATION VECTORS'
         CALL PRSQ(X(LVEC),NORB,L1,L1)
      END IF
C
C        ---- TRANSFORM 1E- INTEGRALS ----
C
      CALL TFTRI(X(LF),X(LH),X(LVEC),X(LWRK),NORB,L1,L1)
      M2 = (NORB*NORB+NORB)/2
      CALL SEQREW(IJKT)
      IF(MASWRK) CALL SQWRIT(IJKT,X(LF),M2)
      IF(DEBG1  .OR.  DEBUG) THEN
         WRITE (IW,*) '1E- INTEGRALS IN MO BASIS'
         CALL PRTRI(X(LF),NORB)
      END IF
      CALL RETFM(NEED1)
C
C        ---- ASSIGN IRREP SYMMETRY TO THE TRANSFORMATION MO'S ----
C        LATER, WE USE SYMMETRY TYPE TO ELIMINATE FULLNR ROTATIONS
C
   50 CONTINUE
      CALL VALFM(LOADFM)
      LS     = LOADFM + 1
      LQ     = LS     + L2
      LWRK   = LQ     + L3
      LMODEG = LWRK   + L1
      LAST   = LMODEG + L1
      NEEDS = LAST - LOADFM - 1
      CALL GETFM(NEEDS)
C
C         MCSCF JOBS NEED TO KNOW ORBITAL SYMMETRY TO PROPERLY
C         ESTIMATE THE NUMBER OF NON-REDUNDANT ROTATIONS IN ORDER
C         TO PROPERLY SIZE FULLNR MEMORY NEEDS.  INPUT ORBITALS
C         SHOULD BE THE TRUE MOREAD, SO WE CAN TRUST THE SYMMETRY
C         LABELS THAT WE GENERATE IN THIS CASE.
C
      IF(EXETYP.NE.CHECK  .OR.  SCFTYP.EQ.RMC) THEN
         CALL DAREAD(IDAF,IODA,X(LS),L2,12,0)
         CALL DAREAD(IDAF,IODA,X(LQ),L3,45,0)
         CALL TRFSYM(X(LMOLAB),X(LMOIRP),X(LMODEG),X(LQ),X(LS),X(LVEC),
     *               X(LWRK),IA,L0,L1,NORB,L1)
         CALL DAWRIT(IDAF,IODA,X(LMOLAB),L1,324,1)
      END IF
C
      CALL RETFM(NEEDS)
C
      NTSAVE = NT
      IF(DIRTRF  .AND.  .NOT.ABEL) NT=1
C
C     -------- TRANSFORM TWO ELECTRON INTEGRALS ------
C     --- MEMORY FOR DIRECT OR CONVENTIONAL PARTIAL TRANSFORMATION ---
C     THIS DIVIDES INTO AN IN MEMORY, AND A SEGMENTED OPTION
C
      NMUL = L1*L2*NORBS
      FMUL = L1
      FMUL = FMUL*L2
      FMUL = FMUL*NORBS
C
      CALL VALFM(LOADFM)
      LFCK  = LOADFM + 1
      LDEN  = LFCK   + MAX(L2,L3)
      LXX   = LDEN   + MAX(L2,L3)
      LIX   = LXX    + NINTMX
      LWRK  = LIX    + NINTMX
      LIWRK = LWRK   + L3
      LPQRJ = LIWRK  + L3/NWDVAR + 1
      LAST  = LPQRJ  + NMUL
      NEED2 = LAST - LOADFM - 1
      NEEDX = LPQRJ - LOADFM - 1
      NDBASE = NEEDV + NEEDD + NEEDX
      NTOT = NDBASE + NMUL
      IF(SOME) WRITE(IW,9100) NWORD,NTOT
C
C        DIRECT TRANSFORMATIONS PASS SOME MEMORY POINTERS IN COMMON
C
      IVEC = LVEC
      IFCK = LFCK
      IDEN = LDEN
      IPQKL= LPQRJ
      NBF  = L1
      NOC  = NORBS
C
C        MAKE SURE IN MEMORY NEED DOESN'T EXCEED 32 BIT INTEGER
C
      IF(NWDVAR.EQ.2  .AND.  FMUL.GT.2.14D+09) GO TO 100
      IF(NTOT.GT.NWORD) GO TO 100
      IF(MPTRAN.GT.0) GO TO 100
      IF (GOPARR) THEN
         NWORD = NTOT
         IF (SOME) WRITE(IW,9110)
         GO TO 100
      END IF
C
C     ----- FULLY IN MEMORY TRANSFORMATION -----
C
      CALL GETFM(NEED2)
      IF(SOME) WRITE(IW,9120)
      IF(EXETYP.EQ.CHECK) GO TO 700
      CALL MCXTR1(X(LPQRJ),X(LVEC),X(LFCK),X(LDEN),X(LXX),X(LIX),
     *            X(LJFLG),NCPU,
     *            X(LMOLAB),X(LMOIRP),X(LWRK),X(LIWRK),
     *            IJKO,IJKT,CUTTRF,NPRINT,NINT,ECORE,DOFOCK,
     *            X(LXBUF),X(LIXBUF),X(LGHND),X(LXINTS),X(LDDIJ),
     *            MAXG,NSH2)
      GO TO 700
C
C     ----- SEGMENTED TRANSFORMATION, BY BATCHES OF J -----
C     ----- MEMORY REQUIRED IS (MIN)=L2*L1   (MAX)=L2*L1*NORBS
C
  100 CONTINUE
      NMIN = L2*L1
      NORBSMX = (NORBS-1)/NPROC+1
      NDMIN  = NDBASE + NMIN*1
      NDMAX  = NDBASE + NMIN*NORBSMX
      IF(SOME) WRITE(IW,9200) NDMIN,NDMAX,NMIN
C
      LEFT = NWORD-NDBASE
      IF(LEFT.LT.NMIN) THEN
        IF (GOPARR) THEN
          IF (SOME) WRITE(IW,9210) NMIN-LEFT+1
          CALL ABRT
          STOP
        ELSE
          GO TO 200
        END IF
      END IF
C
      LPASS = MIN(NORBSMX,LEFT/NMIN)
      NPASS = (NORBS-1)/LPASS+1
      LPASS = (NORBS-1)/NPASS+1
      LAST  = LPQRJ + NMIN*LPASS
      NEED2 = LAST - LOADFM - 1
      NTOT = NEED2+NEEDV+NEEDD
      IF(SOME) WRITE(IW,9220) NTOT,NPASS,LPASS,NPROC
      IF(MPTRAN.GT.1) GO TO 200
C
      IF(SOME) WRITE(IW,9230)
      CALL GETFM(NEED2)
      IF(EXETYP.EQ.CHECK) GO TO 700
C
      IF(DEBUG) CALL TR2OUT(0,0,0,0,0,ZERO,-1)
      NUMX = 0
      NREC = 0
C
      IF (GOPARR) THEN
        NXT = IBTYP.EQ.1  .AND.  ITRFAO.EQ.1  .AND.  NPASS.GE.NPROC
        IPCOUNT = ME - 1
        NEXT = -1
        MINE = -1
      END IF
C
C        ----- BEGIN LOOP OVER PASSES (SEGMENTS) -----
C        THERE ARE -NPASS- TRUE PASSES, AND -MXPASS- TOTAL PASSES,
C        WHERE -MXPASS- IS THE NEXT EVEN MULTIPLE OF -NPROC-.
C        THE PURPOSE OF THE EXTRA MXPASS-NPASS PASSES IS TO HAVE
C        THOSE NODES BROADCAST THEIR PORTION OF A DISTRIBUTED AO
C        INTEGRAL FILE TO THOSE NODES ASSIGNED A GENUINE PASS.
C
      MXPASS = ((NPASS-1+NPROC)/NPROC)*NPROC
C
      CALL TSECND(TIM1)
      MINJ = 1
      IPASS=0
      MYPASS=0
C
  110 CONTINUE
      IPASS = IPASS+1
      MAXJ = MIN(NORBS,MINJ-1+LPASS)
      NOC  = MAXJ-MINJ+1
      IF (GOPARR) THEN
        IF (NXT) THEN
          MINE = MINE + 1
          IF(MINJ.EQ.1) THEN
             IF(MASWRK) THEN
                NEXT = 0
             ELSE
                IF(MINE.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
                IF(NEXT.EQ.0) CALL DDI_DLBNEXT(NEXT)
             END IF
          ELSE
             IF (MINE.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
          END IF
          IF (NEXT.NE.MINE) THEN
             IF(IPASS.LE.NPASS  .AND.  SOME) WRITE(IW,9400) IPASS
             GO TO 115
          END IF
        ELSE
          IPCOUNT = IPCOUNT + 1
          IF (MOD(IPCOUNT,NPROC).NE.0) THEN
             IF(IPASS.LE.NPASS  .AND.  SOME) WRITE(IW,9400) IPASS
             GO TO 115
          END IF
        END IF
      END IF
C
      IF(IPASS.LE.NPASS) THEN
         CALL MCXTR2(X(LPQRJ),X(LVEC),X(LFCK),X(LDEN),X(LXX),X(LIX),
     *               X(LJFLG),NCPU,
     *               X(LFCK),X(LDEN),X(LMOLAB),X(LMOIRP),X(LWRK),
     *               X(LIWRK),IJKO,IJKT,CUTTRF,NPRINT,ECORE,DOFOCK,
     *               NOC,L2,MINJ,MAXJ,X(LXBUF),X(LIXBUF),NUMX,NREC,
     *               X(LGHND),X(LXINTS),X(LDDIJ),MAXG,NSH2,MYPASS)
         IF(SOME) THEN
            CALL TSECND(TIM2)
            TIME = TIM2 - TIM1
            TIM1 = TIM2
            WRITE(IW,9410) IPASS,TIME
            CALL FLSHBF(IW)
         END IF
      ELSE
         CALL FCKBRD(1,X(LFCK),X(LVEC),X(LDEN),X(LWRK),
     *               NCORBS,ECORE,DOFOCK,L1,L2,MYPASS)
         CALL TRFBRD(IJKO,X(LXX),X(LIX),NINTMX,X(LJFLG),NCPU)
         CALL FCKBRD(2,X(LFCK),X(LVEC),X(LDEN),X(LWRK),
     *               NCORBS,ECORE,DOFOCK,L1,L2,MYPASS)
      END IF
C
  115 CONTINUE
      IVEC = IVEC+NOC*L1
      MINJ=MAXJ+1
      IF(IPASS.LT.MXPASS) GO TO 110
C
      IF (DEBUG) CALL TR2OUT(0,0,0,0,0,ZERO,1)
      NINT = NREC*NINTMX+NUMX
      NREC = NREC+1
      NUMX = -NUMX
      CALL PWRIT(IJKT,X(LXBUF),X(LIXBUF),NUMX,NINTMX)
      IF (GOPARR) THEN
         CALL DDI_GSUMI(1700,NINT,1)
         IF (NXT) CALL DDI_DLBRESET
      END IF
      GO TO 700
C
C     ----- OUT-OF-MEMORY CONVENTIONAL OR DIRECT TRANSFORMATION -----
C     ----- MINUMUM MEMORY REQUIRED : L1*NORBS
C     ----- DISK REQUIRED           : L1*(L1+1)*NDAR20/2
C
  200 CONTINUE
      IF(GOPARR) THEN
         WRITE(IW,9320)
         CALL ABRT
      END IF
      NORBS2 = (NORBS*NORBS+NORBS)/2
      NDAR20 = NORBS2+NORBS*(L1-NORBS)
C
C        MOST MEMORY POINTERS ARE SAME AS FOR SEGMENTED,
C        BUT WE HAVE TO SQUEEZE IN A CHAINING ARRAY
C
      LIODA = LPQRJ
      LPQRJ = LIODA + NDAR20
C
      IPQKL = LPQRJ
C
      NMIN = L1*NORBS
      LEFT = NWORD - NEEDD - NEEDV - (LPQRJ-LOADFM-1)
      IF(LEFT.LT.NMIN) THEN
         IF (MASWRK) WRITE(IW,9310) NMIN-LEFT
         CALL ABRT
         STOP
      END IF
      LPASS = LEFT/NMIN
      NPASS = (L2-1)/LPASS+1
      LPASS = (L2-1)/NPASS+1
C
      LAST  = LPQRJ + NMIN*LPASS
      NEED2 = LAST - LOADFM - 1
      CALL GETFM(NEED2)
C
      NTOT = NEEDV+NEED2+NEEDD
      IF(SOME) WRITE(IW,9300) NPASS,NTOT
      IF (EXETYP.EQ.CHECK) GO TO 700
C
      CALL RAOPEN(IDAF20,X(LIODA),0,NDAR20,L2,NPRINT)
      MINPQ=1
  210 CONTINUE
      MAXPQ=MIN(L2,MINPQ+LPASS-1)
      NUMPQ=MAXPQ-MINPQ+1
      CALL MCXTR3(X(LPQRJ),X(LVEC),X(LFCK),X(LDEN),X(LXX),X(LIX),
     *            X(LFCK),X(LMOLAB),X(LWRK),
     *            IJKO,IJKT,NPRINT,ECORE,DOFOCK,
     *            NUMPQ,MINPQ,MAXPQ,IDAF20,X(LIODA),
     *            X(LGHND),X(LXINTS),X(LDDIJ),MAXG,NSH2)
      MINPQ=MAXPQ+1
      IF(MINPQ.LE.L2) GO TO 210
      DEBUG=NPRINT.GE.2
      CALL TR6MC(X(LVEC),IDAF20,X(LIODA),X(LFCK),X(LDEN),
     *           X(LWRK),X(LIWRK),L1,X(LMOIRP),IJKT,L2,DEBUG,
     *           CUTTRF,NINT,X(LXBUF),X(LIXBUF))
      CALL RACLOS(IDAF20,'DELETE')
C
C     ----- ALL IS DONE -----
C
  700 CONTINUE
      CALL RETFM(NEED2)
      CALL RETFM(NEEDV)
      CALL RETFM(NEEDD)
C
      IF(.NOT.DIRTRF) CALL SEQREW(IJKO)
      CALL SEQREW(IJKT)
C
      IF(SOME) THEN
         WRITE(IW,9050) NINT
         IF(DOFOCK  .AND.  NCORBS.GT.0) WRITE(IW,9040) ECORE
         WRITE(IW,*) '... END OF INTEGRAL TRANSFORMATION ...'
         CALL TEXIT(2,2)
      END IF
C
      NT = NTSAVE
      IF(NOSYMT.EQ.1) CALL SYMON
      RETURN
C
 9000 FORMAT(/5X,31('-')/
     *        5X,'PARTIAL INTEGRAL TRANSFORMATION'/
     *        5X,31('-'))
 9010 FORMAT(/1X,'NUMBER OF CORE MOLECULAR ORBITALS     =',I5/
     *        1X,'NUMBER OF OCCUPIED MOLECULAR ORBITALS =',I5/
     *        1X,'TOTAL NUMBER OF MOLECULAR ORBITALS    =',I5/
     *        1X,'TOTAL NUMBER OF ATOMIC ORBITALS       =',I5/
     * 1X,'THRESHOLD FOR KEEPING TRANSFORMED 2E- INTEGRALS =',1P,E10.3)
 9020 FORMAT(1X,'AO INTEGRALS WILL BE CALCULATED IN DIRECT MODE...')
 9030 FORMAT(1X,'AO INTEGRALS WILL BE READ IN FROM DISK...')
 9040 FORMAT(1X,'----- FROZEN CORE ENERGY = ',F20.10)
 9050 FORMAT(1X,'TOTAL NUMBER OF TRANSFORMED 2E- INTEGRALS KEPT =',I10)
C
 9100 FORMAT(1X,'# OF WORDS AVAILABLE = ',I10/
     *       1X,'# OF WORDS NEEDED    = ',I10,
     *          ' FOR IN MEMORY TRANSFORMATION')
 9110 FORMAT(/' PARALLEL ONLY WORKS WITH THE SEGMENTED ALGORITHM.',
     *       /' ADJUSTING MEMORY TO USE SEGMENTED ALGORITHM.'/)
 9120 FORMAT(/1X,'CHOOSING IN MEMORY PARTIAL TRANSFORMATION...')
C
 9200 FORMAT(1X,'FOR THE SEGMENTED TRANSFORMATION:'/
     *       1X,'MINIMUM=',I10,
     *           ' WORDS FOR 1 MOLECULAR ORBITAL PER PASS'/
     *       1X,'MAXIMUM=',I10,
     *           ' WORDS FOR ALL MOLECULAR ORBITALS IN 1 PASS'/
     *       8X,'(',I10,
     *           ' EXTRA WORDS WOULD INCLUDE AN EXTRA ORBITAL/PASS)')
 9210 FORMAT(/' THERE IS NOT ENOUGH MEMORY TO RUN THE TRANSFORMATION',
     *       /' IN PARALLEL.  INCREASE MEMORY BY AT LEAST',I10,' WORDS')
 9220 FORMAT(1X,'SEGMENTED PARTIAL TRANSFORMATION WOULD USE',
     *          I10,' WORDS,'/
     *       1X,'DISTRIBUTING',I4,' PASSES EACH CONTAINING',I4,
     *          ' ORBITALS OVER',I4,' PROCESSORS.')
 9230 FORMAT(/1X,'CHOOSING SEGMENTED PARTIAL TRANSFORMATION...')
C
 9300 FORMAT(/' CHOOSING OUT-OF-MEMORY BIN-SORT TRANSFORMATION...'
     *       /' # PASSES     =',I10
     *       /' # WORDS USED =',I10)
 9310 FORMAT(/' NOT ENOUGH MEMORY TO TRANSFORM THE 2E-INTEGRALS'
     *       /' INCREASE MEMORY BY AT LEAST',I10,' WORDS')
 9320 FORMAT(1X,'THE BIN SORT TRANSFORMATION DOES NOT RUN IN PARALLEL')
 9400 FORMAT(1X,'ASSIGNING PASS #',I4,' TO A DIFFERENT PROCESSOR.')
 9410 FORMAT(1X,'PASS #',I4,' TOOK',F12.2,' SECONDS.')
      END
C*MODULE TRANS   *DECK TRFSYM
      SUBROUTINE TRFSYM(MOSLAB,MOSIRP,MOSDEG,Q,S,V,T,IA,
     *                  NQO,NAO,NMO,LDQV)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      DIMENSION Q(LDQV,NQO),S(*),V(LDQV,NMO),T(NAO),IA(NAO),
     *          MOSLAB(NMO),MOSIRP(NMO),MOSDEG(NMO)
C
      PARAMETER (MXSH=1000, MXATM=500)
C
      COMMON /IOFILE/ IR,IW,IP,NFTI,NFTO,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /SYMBLK/ NIRRED,NSALC,NSALC2,NSALC3
      COMMON /SYMQMT/ IRPLAB(14),IRPNUM(14),IRPDIM(14),IRPDEG(14)
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                TSYM(432),INVT(48),NT
C
C        NOTE THAT ASSIGNMENT THRESHOLD IN SYMMOS IS TEN TIMES TIGHTER
C
      DATA ZERO,SMALL /0.0D+00,1.0D-09/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA IQMARK/4H??  /
#else
      CHARACTER*4 :: IQMARK_STR
      EQUIVALENCE (IQMARK, IQMARK_STR)
      DATA IQMARK_STR/"??  "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA LETA/4HA   /
#else
      CHARACTER*4 :: LETA_STR
      EQUIVALENCE (LETA, LETA_STR)
      DATA LETA_STR/"A   "/
#endif
C
C   THERE ARE -NAO- AO'S, -NMO- MO'S TO BE ASSIGNED, -NQO- TOTAL MO'S.
C
      IF(NMO.GT.NQO  .OR.  NMO.GT.NAO  .OR.  NQO.GT.NAO
     *               .OR.  LDQV.LT.NAO) THEN
         IF(MASWRK) WRITE(IW,9010) NMO,NQO,NAO,LDQV
         CALL ABRT
      END IF
C
      IF(NT.EQ.1) THEN
         DO 5 IV=1,NMO
            MOSLAB(IV)= LETA
            MOSIRP(IV)= 1
            MOSDEG(IV)= 1
    5    CONTINUE
         RETURN
      END IF
C
C     ----- CALCULATE (Q-DAGGER * S) * V -----
C        OVERWRITING -Q- IN THE PROCESS...
C
      DO 35 I=1,NQO
         DO 20 J=1,NAO
            DUM=ZERO
            DO 10 K=1,NAO
               KJ=IA(MAX(K,J))+MIN(K,J)
               DUM=DUM+Q(K,I)*S(KJ)
   10       CONTINUE
            T(J)=DUM
   20    CONTINUE
         DO 30 J=1,NAO
            Q(J,I)=T(J)
   30    CONTINUE
   35 CONTINUE
C
      DO 65 I=1,NQO
         DO 50 J=1,NMO
            DUM=ZERO
            DO 40 K=1,NAO
               DUM=DUM+Q(K,I)*V(K,J)
   40       CONTINUE
            T(J)=DUM
   50    CONTINUE
         DO 60 J=1,NMO
            Q(J,I)=T(J)
   60    CONTINUE
   65 CONTINUE
C
C     ----- ASSIGN SYMMETRY TO -V- ORBITALS ----
C
      DO 150 IV=1,NMO
         IQ=0
         MOSLAB(IV) = 0
         SMAX = ZERO
         DO 140 IRP=1,NIRRED
            DO 130 ID=1,IRPDIM(IRP)
               IF(IRPNUM(IRP).EQ.0) GO TO 130
               DO 120 IN=1,IRPNUM(IRP)
                  IQ=IQ+1
                  SVAL= ABS(Q(IV,IQ))
                  IF(SVAL.GT.SMALL) THEN
                     IF(SVAL.GT.SMAX) SMAX = SVAL
                     IF(MOSLAB(IV).EQ.0) THEN
                        MOSLAB(IV)=IRPLAB(IRP)
                        MOSIRP(IV)= IRP
                        MOSDEG(IV)= IRPDIM(IRP)
                     ELSE IF (MOSLAB(IV).NE.IRPLAB(IRP)) THEN
                        MOSLAB(IV)=IQMARK
                        MOSIRP(IV)= 0
                        MOSDEG(IV)= 0
                     END IF
                  END IF
  120          CONTINUE
  130       CONTINUE
  140    CONTINUE
  150 CONTINUE
C
C        TRANSF. CODE USING SYMMETRY MUST BE ABLE TO ASSIGN ALL MO-S.
C
      NERR = 0
      DO 200 IV=1,NAO
         IF(MOSLAB(IV).EQ.IQMARK) THEN
            NERR = NERR+1
            IF(MASWRK) WRITE(IW,9980) IV
         END IF
  200 CONTINUE
C
      IF(NERR.GT.0) THEN
         IF(MASWRK) WRITE(IW,9970)
         CALL ABRT
      END IF
      RETURN
C
 9010 FORMAT(1X,'CONFUSION IN TRFSYM: NMO,NQO,NAO,LDQV=',4I6)
 9980 FORMAT(1X,'INTEGRAL TRANSFORMATION CANNOT ASSIGN A DEFINITE ',
     *          'ORBITAL SYMMETRY TO MO',I5)
 9970 FORMAT(//1X,'** ERROR **  THE MO-S ARE NOT SYMMETRICAL ENOUGH.'//
     *       1X,'YOU MUST TAKE ACTION TO RECOVER SYMMETRY, PERHAPS TO'/
     *       1X,'PROVIDE A SYMMETRY ADAPTED MO SET BY GUESS=MOREAD,'/
     *       1X,'OR ENTER NOSYM=1 TO USE THESE SYMMETRY BROKEN MO-S.'/)
      END
C*MODULE TRANS   *DECK TRFWT
      SUBROUTINE TRFWT(NFT,X,NX,IX,XBUF,NN,I0,TOL,NREC,OUT,MI,MJ,IXBUF)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL PACK2E,OUT
C
      DIMENSION X(*),IX(*),XBUF(*),IXBUF(*)
C
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /MCPAR / NFZC,NCORBS,NCI,NORBS,NORB,NUM1
      COMMON /PCKLAB/ LABSIZ
C
C     ----- SIFT THE INTEGRALS FOR NON-SMALL ENTRIES -----
C
      ICOUNT = 0
      DO 10 I=1,NN
        IF( ABS(X(I)).LT.TOL) GO TO 10
        ICOUNT=ICOUNT+1
        X(ICOUNT) = X(I)
        IX(ICOUNT)=I+I0-1
   10 CONTINUE
C
      MO = 1
      KL = 0
      DO 30 K = 1,NORB
        DO 20 L = 1, K
          KL = KL + 1
          IF (MO.GT.ICOUNT) GO TO 40
          IF (KL.NE.IX(MO)) GO TO 20
          MK = K
          ML = L
          NX = NX + 1
C
               NPACK = NX
               IPACK = MI
               JPACK = MJ
               KPACK = MK
               LPACK = ML
               IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                 IXBUF( 2*NPACK-1 ) = ISHFT( IPACK, 16 ) + JPACK
                 IXBUF( 2*NPACK   ) = ISHFT( KPACK, 16 ) + LPACK
#endif
#if defined(SPEC_CPU_ILP64)
                 LABEL = ISHFT( IPACK, 48 ) + ISHFT( JPACK, 32 ) +
     *                   ISHFT( KPACK, 16 ) + LPACK
                 IXBUF(NPACK) = LABEL
#endif
               ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                 LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                   ISHFT( KPACK,  8 ) + LPACK
                 IXBUF(NPACK) = LABEL
#endif
#if defined(SPEC_CPU_ILP64)
                 IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                   LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                     ISHFT( KPACK,  8 ) + LPACK
                   IXBUF( NPACK/2 ) = IXBUF( NPACK/2 ) + LABEL
                 ELSE
                   LABEL = ISHFT( IPACK, 56 ) + ISHFT( JPACK, 48 ) +
     *                     ISHFT( KPACK, 40 ) + ISHFT( LPACK, 32 )
                   IXBUF( (NPACK/2)+1 ) = LABEL
                 END IF
#endif
               END IF
C
          XBUF(NX)  = X(MO)
          MO = MO + 1
          IF (OUT) CALL TR2OUT(MI,MJ,MK,ML,NX,XBUF(NX),0)
          IF (NX .EQ. NINTMX) THEN
             CALL PWRIT(NFT,XBUF,IXBUF,NINTMX,NINTMX)
             NREC = NREC+1
             NX = 0
          END IF
   20   CONTINUE
   30 CONTINUE
   40 CONTINUE
      RETURN
      END
C*MODULE GUGDRT  *DECK SYM2EI
      LOGICAL FUNCTION SYM2EI(IG1,IG2,IG3,IG4)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL ABEL
C
      PARAMETER (MXIRR=14)
C
      COMMON /SYMMOL/ GROUP,COMPLX,IGROUP,NAXIS,ILABMO,ABEL
      COMMON /SYMMUL/ NIJREP(MXIRR,MXIRR),IJREP(2,MXIRR,MXIRR,MXIRR)
C
C     DETERMINES IF DIRECT PRODUCT G1 X G2 X G3 X G4 CONTAINS
C     THE TOTALLY SYMMETRIC IRREP, G1,2,3,4 BEING IRREPS OF
C     ANY FOUR MO ORBITALS (E.G.).
C     RETURNS .TRUE. IF THE INTEGRAL BE NON-ZERO.
C
C     THE CODE IS GENERAL ALTHOUGH FOR GREATER EFFICIENCY THE WIMPY CASE
C     OF ABELIAN GROUPS IS TREATED SEPARATELY.
C
      IF(IG1.EQ.0.OR.IG2.EQ.0.OR.IG3.EQ.0.OR.IG4.EQ.0) THEN
         SYM2EI=.TRUE.
         RETURN
      ENDIF
C
      IF(ABEL) THEN
         SYM2EI=IJREP(2,1,IJREP(2,1,IJREP(2,1,IG1,IG2),IG3),IG4).EQ.1
         RETURN
      ELSE
         N12=NIJREP(IG1,IG2)
         DO 200 IGEL=1,N12
C           2 REFERS TO THE LABEL, (1 TO THE NUMBER OF OCCURANCES)
            IG12=IJREP(2,IGEL,IG1,IG2)
            N123=NIJREP(IG12,IG3)
            DO 100 JGEL=1,N123
               IF(IJREP(2,JGEL,IG12,IG3).EQ.IG4) THEN
                  SYM2EI=.TRUE.
                  RETURN
               ENDIF
  100       CONTINUE
  200    CONTINUE
         SYM2EI=.FALSE.
      ENDIF
      RETURN
      END
C
C GDF:  4/23/02  NEW PARALLEL SUBROUTINES
C
C*MODULE TRANS   *DECK TRANDDI
      SUBROUTINE TRANDDI(NORB,NORBS,NCORBS,LOGIT
     *,                  DOOOOO,DOVOOO,DOVVOO,DOVOVO)
C
C -----------------------------------------------------------------
C
C  DRIVER FOR PARALLEL DIRECT FOUR-INDEX TRANSFORMATION (PARTRAN).
C  CALLED FROM TRFMCX.
C
C  SYMBOLS:
C     NORB   = TOTAL NUMBER OF MOLECULAR ORBITALS
C     NORBS  = NUMBER OF OCCUPIED MOS
C     NCORBS = NUMBER OF CORE OCCUPIED MOS
C     LOGIT  = PRODUCE HELPFUL MESSAGES IN THE OUTPUT FILE
C     DOOOOO = DO THE (OO|OO) MO INTEGRAL CLASS
C     DOVOOO = DO THE (VO|OO) MO INTEGRAL CLASS
C     DOVVOO = DO THE (VV|OO) MO INTEGRAL CLASS
C     DOVOVO = DO THE (VO|VO) MO INTEGRAL CLASS
C
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER ( MXSH=1000, MXGTOT=5000, MXATM=500 )
      PARAMETER ( MXGSH=30, MXG2=MXGSH*MXGSH, MXAO=2047 )
      PARAMETER ( TEN=1.0D+01,    ONE=1.0D+00,  TWO=2.0D+00,
     *            TWOPT5=2.5D+00, FOUR=4.0D+00, E=2.30258D+00)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,ABEL,ABELPT
      LOGICAL PACK2E,OUT,OPUT,MP2ENG,UMP2GD,LOGIT,NOTRF
C
C  INTEGRAL CLASS SWITCHES
C
      LOGICAL DOOOOO,DOVOOO,DOVVOO,DOVOVO
C
      COMMON /B     / X(MXSH),Y(MXSH),Z(MXSH)
      COMMON /FMCOM / XX(1)
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /INTOPT/ ISCHWZ,IECP,NECP,IEFLD
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /MP2XTR/ NBF,NCORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /NTNOPT/ ENERGY,ENERG0,DEMAX,SQCDF,ITER,MICRO,NOTRF
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PICON / PITO52,PIDIV4
      COMMON /PSILVL/ IPSI
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                TSYM(432),INVT(48),NT
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
     *,               NDOOOO, NDVOOO, NDVVOO, NDVOVO
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RMC/8HMCSCF   /, CHECK/8HCHECK   /
#else
      CHARACTER*8 :: RMC_STR
      EQUIVALENCE (RMC, RMC_STR)
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA RMC_STR/"MCSCF   "/, CHECK_STR/"CHECK   "/
#endif
C
C  SWITCH OUPUT
C
      ICHANL = IW
      OPUT = LOGIT.AND.MASWRK   ! OPUT RENAMED IN THIS SUBROUTINE
      CALL DDI_OUTPUT(OPUT)     ! SWITCH MESSAGES FROM DDI
      IF (OPUT) WRITE(ICHANL,1)
C
C  PARAMETERS IN /MP2XTR/ AND /MP2DMS/
C
      NDVOVO = DOVOVO
      NDVOOO = DOVOOO
      NDVVOO = DOVVOO
      NDOOOO = DOOOOO
      NBF    = NUM
      NMOS   = NORB
      NOCC   = NORBS
      NCORE  = NCORBS
      NACT   = NOCC - NCORE
      NVIR   = NMOS - NOCC
      CALL BASCHK(LMAX)
                    MXSHL =  4
      IF(LMAX.EQ.2) MXSHL =  6
      IF(LMAX.EQ.3) MXSHL = 10
      IF(LMAX.EQ.4) MXSHL = 15
      MP2ENG = .FALSE.
C
C  DERIVED PARAMETERS IN /MP2XTR/
C
      NBSQ = NBF*NBF
      NBTR = (NBF*NBF+NBF)/2
      NSTR = (NSHELL*NSHELL+NSHELL)/2
      NOV  = NOCC*NVIR
      NOSQ = NOCC*NOCC
      NOP1 = NOCC+1
      NOTR = (NOCC*NOCC+NOCC)/2
      NVSQ = NVIR*NVIR
      NVTR = (NVIR*NVIR+NVIR)/2
      NCP1 = NCORE+1
      NATR = (NACT*NACT+NACT)/2
      IOST = 1
C
C  SWITCH OFF SYMMETRY FOR NON-ABELIAN POINT GROUPS
C
      ABEL = ABELPT()
      NTSAVE = NT
      IF (.NOT.ABEL) NT=1
C
C  ALLOCATE MEMORY FOR MOS AND LABELS
C
      LVECT = NBSQ
      LLABS = NBF
C
      CALL VALFM(LOADFM)
      IVECT = LOADFM + 1            !  MO COEFFICIENTS
      ILABS = IVECT  + LVECT        !  MO SYMMETRY LABELS
      LAST  = ILABS  + LLABS
      NEED  = LAST   - LOADFM
      CALL GETFM(NEED)
C
C  MEMORY FOR COMPUTING MOS AND LABELS
C
      LOVLP = NBTR
      LQSYM = NBSQ
      LWORK = NBSQ            ! WAS NBF PRIOR TO 7/23/03
C
      LMIRP = NBF
      LMDEG = NBF
C
      CALL VALFM(LOADFM)
      IOVLP = LOADFM + 1            !  OVERLAP INTEGRALS
      IQSYM = IOVLP  + LOVLP        !  Q-MATRIX
      IWORK = IQSYM  + LQSYM        !  WORKSPACE
      IMIRP = IWORK  + LWORK        !  WORKSPACE
      IMDEG = IMIRP  + LMIRP        !  WORKSPACE
      LAST  = IMDEG  + LMDEG
      NEED2 = LAST   - LOADFM
      CALL GETFM(NEED2)
C
      IF (EXETYP.NE.CHECK.OR.SCFTYP.EQ.RMC) THEN
C
C  READ MO COEFFICIENTS
C
        CALL DAREAD(IDAF,IODA,XX(IVECT),LVECT,15,0)
C
C  READ OVERLAP INTEGRALS
C
        CALL DAREAD(IDAF,IODA,XX(IOVLP),LOVLP,12,0)
C
C  READ Q MATRIX
C
        CALL DAREAD(IDAF,IODA,XX(IQSYM),LQSYM,45,0)
C
C  CLEAN UP ORBITALS
C  TRADITIONAL VALUES ARE 1D-8 AND 1D-6 RESPECTIVELY
C
        IF(NT.GT.1) THEN
          IF(ABEL) THEN
             TOLZ=1.0D-09
             TOLE=1.0D-07
          ELSE
             TOLZ=1.0D-11
             TOLE=1.0D-10
          END IF
          IF(SCFTYP.EQ.RMC  .AND.  IPSI.EQ.0) THEN
             IF(ITER.EQ.1  .OR.  SQCDF.GT.1.0D-04) THEN
                TOLZ=TOLZ*10.0D+00
                TOLE=TOLE*10.0D+00
             END IF
          END IF
          CALL CLENMO(XX(IVECT),NBF,NQMT,TOLZ,TOLE,IW,.FALSE.)
        END IF
C
C  ORTHONORMALIZE ORBITALS
C
        CALL ORTHO(XX(IQSYM),XX(IOVLP),XX(IVECT),XX(IWORK)
     *,            NQMT,NQMT,NBF,NBTR,NBF)
C
C GDF - 07/23/03 - REPLACE TFSQB BY FASTER EQUIVALENT DGEMM
C
C        CALL TFSQB(XX(IVECT),XX(IQSYM),XX(IWORK),NQMT,NBF,NBF)
C
        CALL DCOPY(NBF*NQMT,XX(IVECT),1,XX(IWORK),1)
        CALL DGEMM('N','N',NBF,NQMT,NQMT
     *,            1.0D+00,XX(IQSYM),NBF,XX(IWORK),NBF
     *,            0.0D+00,XX(IVECT),NBF)
C
C
C  OVERWRITE OLD MOS WITH THE ORTHOGONALIZED ONES.
C
        CALL DAWRIT(IDAF,IODA,XX(IVECT),NBSQ, 15,0)
C
C  GET MO SYMMETRY LABELS
C
        CALL TRFSYM(XX(IMIRP),XX(ILABS),XX(IMDEG)
     *,             XX(IQSYM),XX(IOVLP),XX(IVECT)
     *,             XX(IWORK),IA,NQMT,NBF,NMOS,NBF)
C
C  SAVE MO SYMMETRY LABELS
C
        CALL DAWRIT(IDAF,IODA,XX(ILABS),LLABS,324,1)
C
      END IF   !  EXETYP
      CALL RETFM(NEED2)
C
C  COMPUTE DISTRIBUTED MEMORY IN DOUBLE TO AVOID INTEGER OVERFLOWS,
C  AND ROUND UP TO NEAREST MEGAWORD
C
      TMP = 0.0D+00
      ANOTR=NOTR
      ANBTR=NBTR
      ANBSQ=NBSQ
      ANVIR=NVIR
      ANOCC=NOCC
      IF (NDVOVO) TMP = TMP + ANOTR*ANBSQ
      IF (NDVOOO) TMP = TMP + ANOTR*ANVIR*ANOCC
      IF (NDVVOO) TMP = TMP + ANOTR*ANBTR
      IF (NDOOOO) TMP = TMP + ANOTR*ANOTR
      IF ((NDOOOO.OR.NDVOOO) .AND. .NOT.NDVVOO) TMP=TMP+ANOTR*ANBTR
      MEMREQ = INT(TMP/1.0D+06 + 1.0D+00)
      IF(OPUT) WRITE(ICHANL,5) MEMREQ
C
C  CREATE DDI MATRICES FOR UP TO 2 VIRTUAL INDICES
C  NOTE: D_VOVO IS ITS OWN WORKSPACE
C
      IF (EXETYP.NE.CHECK) THEN
        IF (NDVOVO) THEN
          IF (OPUT) WRITE(ICHANL,9100)
          CALL DDI_CREATE(NBSQ,NOTR,      D_VOVO)
        END IF
        IF (NDVOOO) THEN
          IF (OPUT) WRITE(ICHANL,9110)
          CALL DDI_CREATE(NVIR,NOTR*NOCC, D_VOOO)
        END IF
        IF (NDOOOO) THEN
          IF (OPUT) WRITE(ICHANL,9120)
          CALL DDI_CREATE(NOTR,NOTR,      D_OOOO)
        END IF
C
C  VVOO IS THE WORKSPACE FOR ITSELF AND OOOO, VOOO
C
        IF (NDVVOO.OR.NDOOOO.OR.NDVOOO) THEN
          IF (OPUT) THEN
             IF(NDVVOO) THEN
                WRITE(ICHANL,9130)
             ELSE
                WRITE(ICHANL,9140)
             END IF
          END IF
          CALL DDI_CREATE(NBTR,NOTR,      D_VVOO)
        END IF
      END IF   !  EXETYP
C
C  WORKSPACE LENGTHS.  THE REPLICATED MEMORY NEED DECREASES AS THE NO.
C  OF NODES INCREASES, SO CHECK JOBS LOOP OVER LIKELY PROCESSOR COUNTS.
C
                          MXPWR=0
      IF(EXETYP.EQ.CHECK) MXPWR=5
      DO IPOWER=0,MXPWR
      NNODES=2**IPOWER
C
      LSCHW = NSTR
      LGOUT = MXSHL**4
      LDDIJ = 16*MXG2
      LTRAN = MXSHL*MXSHL*NBF*NOCC
      LTMP1 = MXSHL*MXSHL*NVIR
      LMSG1 = MXSHL*MXSHL*NOTR
      IF(NDVOVO  .AND.  EXETYP.NE.CHECK) THEN
         CALL DDI_DISTRIB( D_VOVO, 0, ILO, IHI, JLO, JHI )
      ELSE
         JHI = (NOCC*NOCC+NOCC)/2
      ENDIF
      LMSG2 = (JHI*MXSHL*NVIR)/NNODES
      LMSG3 = NBF*NBF              !  WAS NBF*NVIR PRIOR TO 7/20/03
C
      LMSG4 = NBTR
      LTRAN = MAX(LTRAN,LMSG3)
      IF (NDVOVO) THEN
        LTMP1 = MAX(LTMP1,LMSG1,LMSG2,LMSG3)
        LTMP2 = MAX(LMSG2,LMSG3)   !  WAS LMSG2 PRIOR TO 7/22/03
      ELSE
        LTMP1 = MAX(LMSG1,LMSG2,LMSG4)
        LTMP2 = NVSQ               !  WAS 0 PRIOR TO 7/22/03
      END IF
C
C  ALLOCATE WORKSPACES FOR PARALLEL TRANSFORMATION
C
      CALL VALFM(LOADFM)
      ISCHW = LOADFM + 1            !  SCHWARZ INTEGRALS
      IGOUT = ISCHW  + LSCHW        !  MAIN INTEGRAL BUFFER
      IDDIJ = IGOUT  + LGOUT        !  INTEGRAL ARRAY
      ITRAN = IDDIJ  + LDDIJ        !  TRANSFORMATION ARRAY
      ITMP1 = ITRAN  + LTRAN
      ITMP2 = ITMP1  + LTMP1
      LAST  = ITMP2  + LTMP2
      NEED3 = LAST   - LOADFM
C
C        THERE ARE NO BETA ORBITALS HERE!
C
      UMP2GD = .FALSE.
      IVECTB = LAST
      ITRANB = LAST
      ITMP1B = LAST
      ITMP2B = LAST
      ILABSB = LAST
C
      IF(EXETYP.EQ.CHECK  .AND.  OPUT)
     *     WRITE(ICHANL,9030) NNODES,NEED+NEED3
      ENDDO
C
      CALL GETFM(NEED3)
C
      IF (EXETYP.NE.CHECK) THEN
C
C  INITIALISE ERI COMPUTATION
C
        PIDIV4 = ATAN(ONE)
        PI     = FOUR*PIDIV4
        PITO52 = TWO*(PI**TWOPT5)
        TOL    = E*ITOL
        CUTOFF = ONE/(TEN**ICUT)
C
C  INITIALIZATIONS FOR POPLE INTEGRALS
C
        IF (INTG76.EQ.1) CALL GAMGEN(2)
        DO I = 1,NSHELL
          ICC  = KATOM(I)
          X(I) = C(1,ICC)
          Y(I) = C(2,ICC)
          Z(I) = C(3,ICC)
        END DO
C
C  GENERATE EXCHANGE INTEGRALS FOR SCHWARZ SCREENING
C
        IF (ISCHWZ.GT.0) THEN
          CALL EXCHNG(XX(ISCHW),XX(IGOUT),XX(IDDIJ)
     *,               NSTR,LGOUT,INTG76)
        ELSE
          CALL DCOPY(LSCHW,ONE,0,XX(ISCHW),1)
        END IF
C
C  PARALLEL TRANSFORMATION
C
        CALL PARTRAN(XX(ISCHW),
     P               XX(IVECT),XX(IVECTB),
     A               XX(IGOUT),
     R               XX(ITRAN),XX(ITRANB),
     T               XX(ITMP1),XX(ITMP1B),
     R               XX(ITMP2),XX(ITMP2B),
     A               XX(IDDIJ),
     N               XX(ILABS),XX(ILABSB))
C
      ELSE
C
C  CHECK DISTRIBUTED MEMORY REQUEST - READ $SYSTEM AGAIN
C  (THE ACTUAL MEMORY ALLOCATION ON ALL NODES IS NOT DONE,
C  BUT AT PRESENT THIS IS WORTHWHILE FOR THE SHEER CONVENIENCE
C  OF AVOIDING THE NEED TO QUEUE A PARALLEL JOB JUST FOR THE
C  CHECK RUN)
C
        CALL SYSINP( MEMINP, MEMDDI )
        IF (MEMDDI.LT.MEMREQ) THEN
          IF (OPUT) WRITE(ICHANL,3) MEMDDI, MEMREQ
          CALL ABRT
        ELSE
          IF (OPUT) WRITE(ICHANL,4)
        END IF
      END IF   !  EXETYP
C
C  FREE REPLICATED WORKSPACE
C
      CALL RETFM(NEED3)
      CALL RETFM(NEED)
C
C  FREE DISTRIBUTED WORKSPACE IF JUST NEED OOOO AND VOOO
C
      IF (.NOT.NDVVOO) CALL DDI_DESTROY(D_VVOO)
C
C  RESTORE SYMMETRY
C
      NT = NTSAVE
      RETURN
C
1     FORMAT(/1X,31('-'),5X,30('-')
     *,/,1X,      'DISTRIBUTED DATA TRANSFORMATION'
     *,5X,        'PROGRAM WRITTEN BY G. FLETCHER'
     *,/,1X,       31('-'),5X,30('-')/)
3     FORMAT(/,6X,'* INSUFFICIENT DISTRIBUTED MEMORY REQUESTED *'
     *,      /,6X,'  MEMDDI = ',1I10,' NEED = ',1I10,//)
4     FORMAT(/6X,'CHECK RUN SHOWS THE DISTRIBUTED MEMORY REQUEST',
     *           ' IS SUFFICIENT.'/)
    5 FORMAT(1X,'THE DISTRIBUTED MEMORY REQUIRED FOR THIS STEP IS',
     *           ' MEMDDI=',I6,' MWORDS')
 9030 FORMAT(1X,'FOR',I5,' NODES, THE REPLICATED MEMORY NEEDED IS',I10)
 9100 FORMAT(1X,'CREATING DISTRIBUTED STORAGE FOR [VIR OCC|VIR OCC]',
     *          ' INTEGRALS')
 9110 FORMAT(1X,'CREATING DISTRIBUTED STORAGE FOR [VIR OCC|OCC OCC]',
     *          ' INTEGRALS')
 9120 FORMAT(1X,'CREATING DISTRIBUTED STORAGE FOR [OCC OCC|OCC OCC]',
     *          ' INTEGRALS')
 9130 FORMAT(1X,'CREATING DISTRIBUTED STORAGE FOR [VIR VIR|OCC OCC]',
     *          ' INTEGRALS')
 9140 FORMAT(1X,'CREATING WORKSPACE OF SIZE VVOO')
      END
C*MODULE TRANS   *DECK FCODDI
      SUBROUTINE FCODDI(NMOS,NOCC,NCOR,FCOR,BUFF)
C
C ----------------------------------------------------------------------
C
C  MCSCF AND CI. CALLED FROM TRFMCX.
C  CONSTRUCT THE 2-EL TERMS OF THE CORE FOCK OPERATOR DIRECTLY IN THE
C  MO BASIS FROM TRANSFORMED INTEGRALS STORED IN THE DDI ARRAYS,
C  WHERE CORE AND ACTIVE INDICES ARE SUBSETS OF THE OCCUPIED MO RANGE.
C
C  SYMBOLS:
C     NMOS = NUMBER OF MOLECULAR ORBITALS
C  (NOTE NMOS MAY NOT EQUAL THE NUMBER OF BASIS FUNCTIONS)
C     NOCC = NUMBER OF OCCUPIED ORBITALS
C     NCOR = NUMBER OF CORE ORBITALS
C     FCOR = CORE FOCK OPERATOR IN THE MO BASIS
C     BUFF = A MESSAGE BUFFER FOR TWO-ELECTRON INTEGRALS
C
C  INTEGRALS NEED AT LEAST TWO CORE INDICES IN THE FORMULA
C  FCOR(IJ) = SUM_OVER_K{[2(IJ|KK)-(IK|JK)]}, K CORE, I,J GENERAL.
C  FCOR STORED IN TRIANGULAR FORM.
C
C ----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER ( TWO = 2.0D+00 )
      PARAMETER ( MXAO=2047 )
      DOUBLE PRECISION FCOR(*),BUFF(*)
      INTEGER DDI_NP, DDI_ME, A,B,AB
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
     *,               NDOOOO, NDVOOO, NDVVOO, NDVOVO
C
C  PAIR-INDEX INITIALIZED IN TRANDDI
C
      COMMON /IJPAIR/ IA(MXAO)
C
      NVIR = NMOS - NOCC
      NMTR = (NMOS*NMOS+NMOS)/2
      NOTR = (NOCC*NOCC+NOCC)/2
      NVTR = (NVIR*NVIR+NVIR)/2
      NVSQ = NVIR*NVIR
      CALL DCOPY(NMTR,0.0D+00,0,FCOR,1)
      CALL DDI_NPROC(DDI_NP, DDI_ME)
C
C  (OO|OO) CLASS
C
      CALL DDI_DISTRIB(D_OOOO,DDI_ME,ILO,IHI,JLO,JHI)
      DO I = 1, NOCC
        DO J = 1, I
          IJCOL = IA(I) + J
          IF ((IJCOL.GE.JLO).AND.(IJCOL.LE.JHI)) THEN
            CALL DDI_GET(D_OOOO,1,NOTR,IJCOL,IJCOL,BUFF)
            IJ = IJCOL
C
C  COULOMB TERMS
C
            DO K = 1, NCOR
              KK = IA(K) + K
              FCOR(IJ) = FCOR(IJ) + TWO*BUFF(KK)
            END DO
C
C  EXCHANGE TERMS
C
            IF (J.LE.NCOR) THEN
              DO K = 1, I
                IK = IA(I) + K
                JK = IA(MAX(J,K)) + MIN(J,K)
                FCOR(IK) = FCOR(IK) - BUFF(JK)
              END DO
            END IF
            IF (I.NE.J) THEN
              IF (I.LE.NCOR) THEN
                DO K = 1, J
                  IK = IA(I) + K
                  JK = IA(MAX(J,K)) + MIN(J,K)
                  FCOR(JK) = FCOR(JK) - BUFF(IK)
                END DO
              END IF
            END IF
          END IF  ! LOCAL
        END DO
      END DO
C
C  (VO|OO) CLASS
C
      CALL DDI_DISTRIB(D_VOOO,DDI_ME,ILO,IHI,JLO,JHI)
C
C  COULOMB TERMS
C
      DO I = 1, NCOR
        IICOL = IA(I) + I
        DO J = 1, NOCC
          IIJCOL = (IICOL-1)*NOCC + J
          IF ((IIJCOL.GE.JLO).AND.(IIJCOL.LE.JHI)) THEN
            CALL DDI_GET(D_VOOO,1,NVIR,IIJCOL,IIJCOL,BUFF)
            DO A = 1, NVIR
              NA = A + NOCC
              JA = IA(NA) + J
              FCOR(JA) = FCOR(JA) + TWO*BUFF(A)
            END DO
          END IF
        END DO
      END DO
C
C  EXCHANGE TERMS
C
      DO I = 1, NCOR
        DO J = 1, NOCC
          IJCOL  = IA(MAX(I,J)) + MIN(I,J)
          IJICOL = (IJCOL-1)*NOCC + I
          IF ((IJICOL.GE.JLO).AND.(IJICOL.LE.JHI)) THEN
            CALL DDI_GET(D_VOOO,1,NVIR,IJICOL,IJICOL,BUFF)
            DO A = 1, NVIR
              NA = A + NOCC
              JA = IA(NA) + J
              FCOR(JA) = FCOR(JA) - BUFF(A)
            END DO
          END IF
        END DO
      END DO
C
C  (VV|CC) CLASS - COULOMB TERMS
C
      CALL DDI_DISTRIB(D_VVOO,DDI_ME,ILO,IHI,JLO,JHI)
      DO I = 1, NCOR
        IICOL = IA(I) + I
        IF ((IICOL.GE.JLO).AND.(IICOL.LE.JHI)) THEN
          CALL DDI_GET(D_VVOO,1,NVTR,IICOL,IICOL,BUFF)
          DO A = 1, NVIR
            NA  = A + NOCC
            DO B = 1, A
              NB  = B + NOCC
              NAB = IA(NA) + NB
              AB  = IA(A) + B
              FCOR(NAB) = FCOR(NAB) + TWO*BUFF(AB)
            END DO
          END DO
        END IF
      END DO
C
C  (VC|VC) CLASS - EXCHANGE TERMS
C
      CALL DDI_DISTRIB(D_VOVO,DDI_ME,ILO,IHI,JLO,JHI)
      DO I = 1, NCOR
        IICOL = IA(I) + I
        IF ((IICOL.GE.JLO).AND.(IICOL.LE.JHI)) THEN
          CALL DDI_GET(D_VOVO,1,NVSQ,IICOL,IICOL,BUFF)
          DO A = 1, NVIR
            NA  = A + NOCC
            DO B = 1, A
              NB  = B + NOCC
              NAB = IA(NA) + NB
              AB  = (A-1)*NVIR + B
              FCOR(NAB) = FCOR(NAB) - BUFF(AB)
            END DO
          END DO
        END IF
      END DO
C
C  GLOBALLY SUM 2-EL CORE FOCK MATRIX (+ SYNC)
C
      CALL DDI_GSUMF( 2520, FCOR, NMTR )
      RETURN
      END
