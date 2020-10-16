C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C 26 OCT 00 - MWS - INTRODUCE MXAO PARAMETER
C 12 NOV 98 - GDF - CHANGE BIT PACKING TO ISHIFT
C 26 OCT 98 - MAF - CPROHF,CPSTRF,CPWAUB,ROCPHF,ROCPOM: ALLOW FOR USE OF
C                   SPHERICAL HARMONICS
C 28 SEP 97 - MWS - CONVERT PARALLEL CALLS FROM TCGMSG TO DDI
C  2 SEP 97 - MWS - CPROHF: WATCH OUT FOR IN MEMORY OVERFLOW IF 32 BITS
C  8 JAN 97 - MWS - ROCPOM: FIX GLOBAL SUM BUG
C 18 DEC 96 - TLW - PAD THE CHFINF COMMON BLOCK
C  9 FEB 96 - MWS - CHANGES FOR THE IMPROVED SOLVER
C 21 APR 95 - SPW - CPROHF,CPWAUB,ROCPHF,CPWAUB: ANALYTIC POLARIZABILITY
C 27 MAR 95 - SPW - CPROHF,CPSTRF,CPWAUB: CHANGES FOR IR INTENSITY
C 15 DEC 94 - MWS - AVOID LARGE GLOBAL SUM OF ORBITAL HESSIANS
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C  5 AUG 94 - MWS - CPROHF,ROCPHF,ROCPOM: USE DOUBLE LABEL PACKING
C  1 JUN 94 - MWS - IMPLEMENT PARALLEL FORMATION OF CPHF EQUATIONS
C  9 DEC 93 - MWS - ROCPHF: CHANGE H-A DAF RECORD NUMBER
C  2 NOV 93 - HS  - CPROHF: FIX MEMORY ALLOCATION GLITCH
C 12 MAR 92 - MWS - REDIMENSION TO 500 ATOMS
C  3 FEB 92 - MWS - ALLOCATE MEMORY FOR NEW OUT OF CORE SOLVER
C 11 JAN 92 - TLW - USE SEQOPN+SEQREW, PARALLEL I/O CHANGES
C  7 OCT 91 - MWS - CHANGE UNIX BYTE UNPACKING
C 25 OCT 90 - MWS - ROCPOM ADDED FOR OUT OF MEMORY CPHF
C  7 AUG 90 - TLW - ADD CF AND CG TO COMMON NSHEL
C 22 JUL 90 - JAM,MWS - OS-TCSCF CPHF IMPLEMENTED
C  3 JUN 90 - JAM - CHANGE VAX BYTE PACKING TO FULL 8 BITS
C 27 NOV 89 - MWS - OPEN SHELL CPHF IMPLEMENTED IN GAMESS
C*MODULE CPROHF  *DECK CPROHF
      SUBROUTINE CPROHF(TCSCF,SOME,OUT,NOCP,NXYZ,NUNIQ,
     *                  NFT16,NFT18)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL TCSCF,SOME,OUT,PACK2E,CHKMEM,INCORE,GOPARR,DSKWRK,MASWRK,
     *        SVGPAR,SVDSKW,POLAR
C
      DIMENSION NOCP(NXYZ+3)
C
      PARAMETER (MXATM=500, MXAO=2047)
C
      COMMON /CHFINF/ CHFSLV,CPTYPE,NWDCHF,POLAR
      COMMON /FMCOM / X(1)
      COMMON /GVBWFN/ CICOEF(2,12),F(25),ALPHA(325),BETA(325),NO(10),
     *                NCO,NSETO,NOPEN,NPAIR,NORB,NCONF(MXAO),NHAM
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
      DATA MAXEQU/128/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CONJG/8HCONJG   /, ONDISK/8HONDISK  /
#else
      CHARACTER*8 :: CONJG_STR
      EQUIVALENCE (CONJG, CONJG_STR)
      CHARACTER*8 :: ONDISK_STR
      EQUIVALENCE (ONDISK, ONDISK_STR)
      DATA CONJG_STR/"CONJG   "/, ONDISK_STR/"ONDISK  "/
#endif
C
C     ----- RESTRICTED OPEN SHELL CPHF CODE -----
C     WRITTEN BY MIKE SCHMIDT AT NORTH DAKOTA STATE UNIVERSITY,
C     IN FITS AND SPURTS, OVER THE PERIOD MAY-NOVEMBER, 1989.
C            THIS IS CODED FROM
C     Y.OSAMURA, Y.YAMAGUCHI, P.SAXE, D.J.FOX, M.A.VINCENT,
C     H.F.SCHAEFER,   J.MOL.STRUCT. 103,183-186(1983)
C            BUT SEE ALSO
C     P.SAXE, Y.YAMAGUCHI, H.F.SCHAEFER
C        J.CHEM.PHYS. 77, 5647-5654(1982)
C     Y.OSAMURA, Y.YAMAGUCHI, P.SAXE, M.A.VINCENT, J.F.GAW,
C     H.F.SCHAEFER    CHEM.PHYS. 72, 131-9(1982)
C            WITH FORMULAE SUMMARIZED IN
C     Y.YAMAGUCHI, M.FRISCH, J.GAW, H.F.SCHAEFER, J.S.BINKLEY
C        J.CHEM.PHYS. 84, 2262-2278(1986)
C
C     ----- OPEN SHELL TWO CONFIGURATION CODE -----
C     WRITTEN BY JOHN MONTGOMERY AND MIKE SCHMIDT BETWEEN
C     JUNE 22 AND JULY 22, 1990.
C            THIS IS CODED FROM
C     M.DURAN, Y.YAMAGUCHI, H.F.SCHAEFER III
C     J.PHYS.CHEM. 92, 3070-3075(1988).
C            BUT SEE ALSO
C     M.DURAN, Y.YAMAGUCHI, R.B.REMINGTON, H.F.SCHAEFER
C     CHEM.PHYS. 122, 201-231(1988).
C
      IF(SOME  .AND.  .NOT.TCSCF) WRITE(IW,9000)
      IF(SOME  .AND.       TCSCF) WRITE(IW,9010)
      TIM1 = 0.0D+00
      DO 100 I=1,NUM
         IA(I) = (I*I-I)/2
  100 CONTINUE
C
C     ----- INITIALIZE -NCONF- FOR A VIRTUAL SHELL -----
C
      DO 150 I=NORB+1,NUM
         NCONF(I) = NHAM+1
  150 CONTINUE
C
      IF(OUT) THEN
         WRITE(IW,8150) NCO,NSETO,NOPEN,NPAIR,NORB,NHAM
         WRITE(IW,8160) (NCONF(I),I=1,NORB)
         WRITE(IW,8170) '    F=',(F(I),I=1,NHAM)
         NTRI = (NHAM*NHAM+NHAM)/2
         WRITE(IW,8170) 'ALPHA=',(ALPHA(I),I=1,NTRI)
         WRITE(IW,8170) ' BETA=',( BETA(I),I=1,NTRI)
      END IF
C
      L0 = NQMT
      L1 = NUM
      L2 = (L1*L1+L1)/2
      L3 = L1*L1
C
C     ---- IF CALCULATING ALPHA POLARIZABILITIES ----
C     ---- 3 EXTRA SOLUTIONS REQUIRED            ----
C
      IF(POLAR)THEN
         NNXYZ=NXYZ+3
      ELSE
         NNXYZ=NXYZ
      END IF
C
C     ----- DETERMINE THE NUMBER OF INTER-SHELL ROTATIONS -----
C
      NROT=0
      DO 210 I=1,L0
         NCI = NCONF(I)
         DO 200 J=1,I
            IF(NCONF(J).NE.NCI) NROT=NROT+1
  200    CONTINUE
  210 CONTINUE
      IF(SOME) WRITE(IW,9020) NROT
      IF(TCSCF) THEN
         NROT = NROT+2
         IF (MASWRK) WRITE(IW,9030)
      END IF
C
C     FIRST, ESTABLISH MEMORY NEEDS.
C     THEN, COME BACK TO 300 AND BEGIN TO DO THE ACTUAL WORK.
C
      CHKMEM=.TRUE.
      CALL VALFM(LOADFM)
      CALL GOTFM(NGOTMX)
      IF(NWDCHF.GT.0) NGOTMX=MIN(NGOTMX,NWDCHF)
      IF (MASWRK) WRITE(IW,9040) NGOTMX
      NEED1 = 0
      NEED2 = 0
      GO TO 310
C
  300 CONTINUE
      CHKMEM=.FALSE.
      NEED = MAX(NEED1,NEED2)
      IF(NEED.GT.NGOTMX) THEN
         IF (MASWRK) THEN
            WRITE(IW,*)
     *         '**** INSUFFICIENT MEMORY FOR CP-ROHF *****'
            WRITE(IW,*) 'NEED',NEED,' WORDS.'
            WRITE(IW,*) ' GOT',NGOTMX,' WORDS.'
         END IF
         CALL ABRT
         STOP
      END IF
      IF(EXETYP.EQ.CHECK) GO TO 800
      TIM1=0.0D+00
      CALL TSECND(TIM1)
C
C     ----- TRANSFORM DS/DA TO MO BASIS -----
C
  310 CONTINUE
      LSA    = 1      + LOADFM
      LSAAO  = LSA    + L2*NXYZ
      LV     = LSAAO  + L2*NXYZ
      LDS    = LV     + L3
      LWRK   = LDS    + L3*3
      LSD    = LWRK   + L1
      LSDA   = LSD    + NAT*9
      LAST   = LSDA   + NAT*9
      NEED1  = LAST - LOADFM -1
      IF(CHKMEM) GO TO 400
      CALL GETFM(NEED1)
      CALL CPSTRF(X(LDS),X(LSAAO),X(LSA),X(LV),X(LWRK),
     *            X(LSD),X(LSDA),IA,L0,L1,L2,L3,NXYZ,NAT,OUT)
      CALL RETFM(NEED1)
      CALL TSECND(TIM2)
      THYME = TIM2-TIM1
      IF (MASWRK) WRITE(IW,9050)
     *   'TRANSFORMING DS/DA TO MO BASIS',THYME
      TIM1 = TIM2
C
C     ----- MEMORY FOR THE CPHF -----
C     NOTE THAT -SA- FROM THE PREVIOUS STEP MUST BE THE 1ST ARRAY
C
  400 CONTINUE
      LSA    = 1      + LOADFM
      LXX    = LSA    + L2*NXYZ
      LIXX   = LXX    + NINTMX
      LEPS   = LIXX   + NINTMX
      LZETA  = LEPS   + L3
      LALP   = LZETA  + L2*(NHAM+1)
      LBET   = LALP   + L2
      LIROT  = LBET   + L2
      LHMO   = LIROT  + L3/NWDVAR + 1
      LW     = LHMO   + L2
      LFCM   = LW     + L1*NORB*NXYZ
      LFCKDR = LFCM   + NXYZ*NXYZ
      LDDM   = LFCKDR + L3
      LDDMA  = LDDM   + NAT*9
      LSK    = LDDMA  + NAT*9
      LHF    = LSK    + L1
C
      IF(POLAR) THEN
         LAST  =  LHF + L2*3
      ELSE
         LAST  =  LHF
      END IF
C
      IF(TCSCF) THEN
         LDHAM  = LAST
         LEPSIJ = LDHAM  + 3*NXYZ
         LEGRAD = LEPSIJ + 3*L3
         LAST   = LEGRAD + NXYZ
      ELSE
         LDHAM  = LAST
         LEPSIJ = LAST
         LEGRAD = LAST
      END IF
C
      LB     = LAST
      LU     = LB     + NROT*NNXYZ
      LAII   = LU     + NROT*NNXYZ
      LAST   = LAII   + NROT
C
C        THE VECTOR VERSION NEEDS 4 EXTRA ARRAYS FOR TFTRI LATER.
C
      NEEDX  = LAST -LOADFM -1 + (3*L3+L1)
C
C        FOR DEBUGGING, WE MAY WANT TO FORCE THE CHOICE OF SOLVER
C
      IF(CHFSLV.EQ.CONJG) MAXEQU=1
      IF(CHFSLV.EQ.ONDISK) THEN
         MAXEQU=1
         NGOTMX = NEEDX +2*NNXYZ +4*NROT*NNXYZ + (NROT*NROT+10)/2
         IF(TCSCF) NGOTMX = NGOTMX + 2*NROT
      END IF
C
C        DECIDE UPON IN CORE OR OUT OF CORE METHOD,
C        WATCH OUT FOR 32 BIT SYSTEMS
C
      NSOLV = NNXYZ
      IF(NROT.LE.MAXEQU) NSOLV=1
C
      DMY = 2.0D+00**31 - 2*NSOLV - 4*NROT*NSOLV - 1000
      N32MAX = INT(SQRT(DMY))
      IF(NWDVAR.EQ.1  .OR.  NROT.LE.N32MAX) THEN
         NEEDIN = NEEDX + NROT*NROT + 2*NSOLV + 4*NROT*NSOLV
         INCORE = NEEDIN.LT.NGOTMX
      ELSE
         NEEDIN = 0
         INCORE = .FALSE.
      END IF
C
      IF(INCORE) THEN
         NWIDTH = NROT
         NSLICE = 1
         LACI   = LAST
         LWRK1  = LAST
         LWRK2  = LWRK1 + NROT*NSOLV
         LWRK3  = LWRK2 + NROT*NSOLV
         LWRK4  = LWRK3 + NROT*NSOLV
         LBNRM  = LWRK4 + NROT*NSOLV
         LRESD  = LBNRM + NSOLV
         LA     = LRESD + NSOLV
         LAST   = LA    + NROT*NROT
         IF(CHKMEM .AND. MASWRK) WRITE(IW,9060)
      ELSE
         LACI   = LAST
         IF(TCSCF) LAST = LACI  + 2*NROT
         LWRK1  = LAST
         LWRK2  = LWRK1 + NROT*NSOLV
         LWRK3  = LWRK2 + NROT*NSOLV
         LWRK4  = LWRK3 + NROT*NSOLV
         LBNRM  = LWRK4 + NROT*NSOLV
         LRESD  = LBNRM + NSOLV
         LAST   = LRESD + NSOLV
         NEEDX  = LAST -LOADFM -1 + (3*L3+L1)
         NFREE  = NGOTMX - NEEDX
         NWIDTH = NFREE/NROT
         IF(NWIDTH.GT.0) THEN
            NSLICE = (NROT-1)/NWIDTH + 1
         ELSE
            NSLICE = -1
         END IF
         LA     = LAST
         LAST   = LA    + NROT*NWIDTH
         IF(NWIDTH.LE.0  .OR.  (NSLICE.GT.NAT .AND. NAT.GT.5)) THEN
            IF (MASWRK) WRITE(IW,9070)
     *         NWIDTH,NSLICE,NAT,NFREE,NEEDX,NROT
            CALL ABRT
            STOP
         END IF
         IF(CHKMEM .AND. MASWRK) WRITE(IW,9080) NSLICE,NWIDTH
      END IF
C
      NEED2  = LAST - LOADFM -1
      IF(SOME  .AND.  CHKMEM) THEN
         WRITE(IW,9090) 'DS/DA TRANSFORM',NEED1
         WRITE(IW,9090) ' CPHF EQUATIONS',NEED2
         IF(.NOT.INCORE  .AND.  NEEDIN.GT.0) WRITE(IW,9100) NEEDIN
      END IF
      IF(CHKMEM) GO TO 300
      CALL GETFM(NEED2)
C
C     ----- READ BARE NUCLEUS HAMILTONIAN, PUT IN MO BASIS -----
C     TEMP ARRAYS: LALP=HAO, LEPS=V, LBET=WRK
C
      CALL DAREAD(IDAF,IODA,X(LALP),L2,11,0)
      CALL DAREAD(IDAF,IODA,X(LEPS),L3,15,0)
      CALL TFTRI(X(LHMO),X(LALP),X(LEPS),X(LBET),L0,L1,L1)
C
C     ----- SET UP THE OPEN SHELL/TCSCF CPHF EQUATIONS -----
C
      CALL ROCPHF(TCSCF,X(LXX),X(LIXX),NINTMX,IA,X(LALP),X(LBET),
     *            X(LEPS),X(LZETA),X(LHMO),X(LIROT),X(LA),X(LB),
     *            X(LSA),X(LW),X(LFCM),X(LFCKDR),X(LEPSIJ),X(LDHAM),
     *            X(LEGRAD),X(LACI),NAT,L0,L1,L2,L3,NROT,NXYZ,NHAM,NORB,
     *            NFT18,OUT,INCORE,X(LHF),NNXYZ)
      IF(.NOT.INCORE) THEN
         SVDSKW = DSKWRK
         DSKWRK = .FALSE.
         CALL SEQOPN(NFT16,'WORK16','UNKNOWN',.FALSE.,'UNFORMATTED')
         DSKWRK = SVDSKW
         CALL ROCPOM(TCSCF,X(LXX),X(LIXX),NINTMX,IA,X(LALP),X(LBET),
     *               X(LEPS),X(LZETA),X(LIROT),X(LA),X(LACI),X(LAII),
     *               L0,L1,L2,NROT,NHAM,NWIDTH,NFT16,OUT)
      END IF
C
      CALL TSECND(TIM2)
      THYME = TIM2-TIM1
      IF (MASWRK) WRITE(IW,9050)
     *   ' SETTING UP THE CPHF EQUATIONS',THYME
      CALL FLSHBF(IW)
      TIM1 = TIM2
C
C     ----- SOLVE THE OPEN SHELL/TCSCF CPHF EQUATIONS -----
C
      NITER=0
      NPASS=0
      SVGPAR = GOPARR
      SVDSKW = DSKWRK
      GOPARR = .FALSE.
      DSKWRK = .FALSE.
      IF(MASWRK) THEN
      CALL SOLVCP(X(LA),X(LB),X(LU),NROT,NNXYZ,NOCP,X(LAII),
     *            X(LWRK1),X(LWRK2),X(LWRK3),X(LWRK4),
     *            X(LBNRM),X(LRESD),NITER,NPASS,NWIDTH,NSOLV,
     *            INCORE,NFT16,MAXEQU)
      END IF
      GOPARR = SVGPAR
      DSKWRK = SVDSKW
      IF(OUT) THEN
         WRITE(IW,*) 'CPHF RESPONSE VECTORS -U-'
         CALL PRSQ(X(LU),NNXYZ,NROT,NROT)
      END IF
C
      CALL TSECND(TIM2)
      THYME = TIM2-TIM1
      IF (MASWRK) THEN
         WRITE(IW,9050) 'SOLVING FOR THE CPHF RESPONSES',THYME
         IF(NITER.NE.0) WRITE(IW,9110) NUNIQ,NITER
         IF(.NOT.INCORE) WRITE(IW,9120) NPASS
      END IF
      TIM1 = TIM2
C
C     ----- ADD THE CPHF TERMS TO THE NUCLEAR HESSIAN -----
C
      CALL CPWAUB(TCSCF,X(LFCM),X(LW),X(LU),X(LSA),X(LIROT),
     *            X(LEPSIJ),X(LDHAM),X(LDDM),X(LDDMA),X(LSK),
     *            IA,NROT,NAT,NXYZ,NNXYZ,L0,L1,L2,OUT)
C
      CALL RETFM(NEED2)
  800 CONTINUE
      RETURN
C
 8150 FORMAT(1X,'NCO,NSETO,NOPEN,NPAIR,NORB,NHAM=',6I4)
 8170 FORMAT(1X,A6,10F10.5/(7X,10F10.5))
 8160 FORMAT(1X,'NCONF=',10I5/(7X,10I5))
C
 9000 FORMAT(/5X,41("-")/
     *        5X,'COUPLED-PERTURBED OPEN SHELL HARTREE-FOCK'/5X,41(1H-))
 9010 FORMAT(/5X,48("-")/
     *        5X,'COUPLED-PERTURBED TWO CONFIGURATION HARTREE-FOCK'/
     *        5X,48(1H-))
 9020 FORMAT(1X,'THE CPHF HAS',I8,' INDEPENDENT ORBITAL ROTATIONS.')
 9030 FORMAT(1X,'THE CPHF HAS 2 ADDITIONAL CSF ROTATIONS.')
 9040 FORMAT(1X,'THE CPHF CODE HAS',I12,' WORDS OF MEMORY AVAILABLE.')
 9050 FORMAT(5X,A30,' TOOK',F12.1,' SECONDS')
 9060 FORMAT(1X,'CHOOSING IN MEMORY CPHF ALGORITHM')
 9070 FORMAT(/1X,'*** NOT ENOUGH MEMORY TO RUN THE OUT OF MEMORY',
     *          ' CPHF CODE ***'/
     *       1X,'NWIDTH=',I10,'  NSLICE=',I10,'   NAT=',I10/
     *       1X,' NFREE=',I10,'   NEEDX=',I10,'  NROT=',I10//
     *       1X,'INCREASE THE VALUE OF MEMORY IN $CONTRL SO THAT'/
     *       1X,'NFREE=MEMORY-NEEDX AT LEAST EXCEEDS NROT*NROT/NAT'//
     *       1X,'LARGER AMOUNTS OF MEMORY ARE EVEN BETTER!')
 9080 FORMAT(1X,'CHOOSING OUT OF MEMORY CPHF ALGORITHM'/
     *       1X,'THE -A- MATRIX WILL BE FORMED IN',I5,
     *          ' SLICES OF',I7,' COLUMNS.')
 9090 FORMAT(1X,A15,' STEP WILL REQUIRE',I12,' WORDS OF MEMORY.')
 9100 FORMAT(1X,'(THE IN CORE ALGORITHM WOULD NEED',I12,' WORDS)')
 9110 FORMAT(1X,'SOLVING FOR ALL',I5,' UNIQUE RESPONSES TOOK',
     *          I5,' ITERATIONS.')
 9120 FORMAT(1X,'CONJUGATE GRADIENT SOLVER MADE',I4,
     *          ' READS OF THE EXTERNALLY STORED -A- MATRIX')
      END
C*MODULE CPROHF  *DECK CPSTRF
      SUBROUTINE CPSTRF(DS,DSAO,DSMO,V,WRK,SDIDER,SDIDERA,IA,
     *                  L0,L1,L2,L3,NXYZ,NAT,OUT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL OUT
C
      DIMENSION DS(L1,L1,3),DSAO(L2,NXYZ),DSMO(L2,NXYZ),
     *          V(L3),WRK(L1),SDIDER(NAT*9),SDIDERA(NAT*9),IA(L1)
C
      PARAMETER (MXGTOT=5000, MXSH=1000, MXAO=2047)
C
      COMMON /GVBWFN/ CICOEF(2,12),F(25),ALPHA(325),BETA(325),NO(10),
     *                NCO,NSETO,NOPEN,NPAIR,NORB,NCONF(MXAO),NHAM
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
C
      PARAMETER (TWO=2.0D+00)
C
C     ----- TRANSFORM DS/DA FROM AO TO MO BASIS -----
C
C     READ 1ST DERIVATIVES OF OVERLAP MATRIX
C
      CALL DAREAD(IDAF,IODA,DS(1,1,1),L3,63,0)
      CALL DAREAD(IDAF,IODA,DS(1,1,2),L3,64,0)
      CALL DAREAD(IDAF,IODA,DS(1,1,3),L3,65,0)
C
C     READ MO VECTORS.
C
      CALL DAREAD(IDAF,IODA,V,L3,15,0)
C
C     EXPAND DS/DA FROM PACKED DAF FORMAT.
C
      CALL VCLR(DSAO,1,L2*NXYZ)
C
      DO 150 ISHELL = 1,NSHELL
         IAT  = KATOM(ISHELL)
         IXYZ = 3*(IAT-1)
         LOCI = KLOC(ISHELL) - KMIN(ISHELL)
         MINI = KMIN(ISHELL)
         MAXI = KMAX(ISHELL)
         DO 140 I = MINI,MAXI
            II   = LOCI + I
            DO 130 JJ = 1,L1
               IJ = IA(MAX(II,JJ)) + MIN(II,JJ)
               DSAO(IJ,IXYZ+1) = DSAO(IJ,IXYZ+1) + DS(II,JJ,1)
               DSAO(IJ,IXYZ+2) = DSAO(IJ,IXYZ+2) + DS(II,JJ,2)
               DSAO(IJ,IXYZ+3) = DSAO(IJ,IXYZ+3) + DS(II,JJ,3)
  130       CONTINUE
  140    CONTINUE
  150 CONTINUE
C
C     TRANSFORM EACH MATRIX TO MO BASIS.
C
      DO 400 IXYZ=1,NXYZ
         CALL TFTRI(DSMO(1,IXYZ),DSAO(1,IXYZ),V,WRK,L0,L1,L1)
  400 CONTINUE
C
C     ---- GET DIPOLE INTEGRALS AND TRANSFORM TO MO BASIS   ----
C     ---- WRITE TRANSFORMED INTEGRALS TO DAF FOR LATER USE ----
C
      CALL DAREAD(IDAF,IODA,DSAO(1,1),L2,95,0)
      CALL TFTRI(DSAO(1,2),DSAO(1,1),V,WRK,L0,L1,L1)
      CALL DAREAD(IDAF,IODA,DSAO(1,1),L2,96,0)
      CALL TFTRI(DSAO(1,3),DSAO(1,1),V,WRK,L0,L1,L1)
      CALL DAREAD(IDAF,IODA,DSAO(1,1),L2,97,0)
      CALL TFTRI(DSAO(1,4),DSAO(1,1),V,WRK,L0,L1,L1)
C
      CALL DAWRIT(IDAF,IODA,DSAO(1,2),L2,252,0)
      CALL DAWRIT(IDAF,IODA,DSAO(1,3),L2,253,0)
      CALL DAWRIT(IDAF,IODA,DSAO(1,4),L2,254,0)
C
C     ---- OVERLAP DERIVATIVE CONTRIBUTION TO DIPOLE DERIVATIVE ----
C
      CALL VCLR(SDIDER,1,9*NAT)
C
      DO 500 IXYZ=1,NXYZ
         NN=1 + (IXYZ-1)*3
         DO 510 IOCC=1,NORB
            NF=NCONF(IOCC)
            DO 520 JOCC=1,IOCC
               IJ=IA(IOCC)+JOCC
               VAL=DSMO(IJ,IXYZ)
               IF (IOCC .NE. JOCC) VAL = VAL + VAL
C
             SDIDER(NN  )=SDIDER(NN  ) + (TWO*F(NF)*VAL*DSAO(IJ,2))
             SDIDER(NN+1)=SDIDER(NN+1) + (TWO*F(NF)*VAL*DSAO(IJ,3))
             SDIDER(NN+2)=SDIDER(NN+2) + (TWO*F(NF)*VAL*DSAO(IJ,4))
C
  520       CONTINUE
  510    CONTINUE
  500 CONTINUE
C
C     --- ADD OVERLAP DERIVATIVE CONTRIBUTIONS TO DIPOLE DERIVATIVE ---
C
      IF(OUT) THEN
         WRITE(IW,9040)
         CALL DDMOUT(SDIDER,3*NAT)
      END IF
      CALL DAREAD (IDAF,IODA,SDIDERA,9*NAT,34,0)
      CALL VADD(SDIDERA,1,SDIDER,1,SDIDERA,1,9*NAT)
      CALL DAWRIT (IDAF,IODA,SDIDERA,9*NAT,34,0)
C
      RETURN
 9040 FORMAT(/10X,34("-")/
     *        10X,'OVERLAP DERIVATIVE CONTRIBUTION TO'/
     *        10X,34(1H-))
      END
C*MODULE CPROHF  *DECK CPWAUB
      SUBROUTINE CPWAUB(TCSCF,FCM,W,U,SA,IROT,EPSIJ,DHAM,DDM,DDMA,SK,IA,
     *                  NROT,NAT,NXYZ,NNXYZ,L0,L1,L2,OUT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXAO=2047)
C
      LOGICAL OUT,TCSCF,POLAR
C
      DIMENSION FCM(NXYZ,NXYZ),W(L1,NORB,NXYZ),U(NROT,NNXYZ),
     *          SA(L2,NXYZ),IROT(L1,L1),EPSIJ(L1,L1,3),
     *          DHAM(3,NXYZ),DDM(NAT*9),DDMA(NAT*9),IA(NORB)
      DIMENSION CIRESP(2),IADD(2,2),APOL(6),APOLA(6),
     *          CIUF(2,3),DUMA(2,3)
C
      COMMON /CHFINF/ CHFSLV,CPTYPE,NWDCHF,POLAR
      COMMON /GVBWFN/ CICOEF(2,12),F(25),ALPHA(325),BETA(325),NO(10),
     *                NCO,NSETO,NOPEN,NPAIR,NORB,NCONF(MXAO),NHAM
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
C
      PARAMETER (ZERO=0.0D+00, TWO=2.0D+00, FOUR=4.0D+00)
C
      DATA IADD/1,2,2,3/
C
C     ----- ADD CPHF CONTRIBUTIONS TO THE HESSIAN -----
C     FIRST, INITIALIZE HESSIAN WITH ALL OTHER TERMS.
C
      CALL DAREAD(IDAF,IODA,FCM,NXYZ*NXYZ,4,0)
C
C     ----- SET UP ROTATION INDEX ARRAY OVER MO'S -----
C
      MROT=0
      DO 220 I=1,L0
         ISH = NCONF(I)
         DO 210 J=1,I
            IF(NCONF(J).EQ.ISH) THEN
               IROT(I,J) = 0
               IROT(J,I) = 0
            ELSE
               MROT=MROT+1
               IROT(I,J) = MROT
               IROT(J,I) = MROT
            END IF
  210    CONTINUE
  220 CONTINUE
      IF(TCSCF) MROT = MROT+2
      IF(MROT.NE.NROT) CALL ABRT
C
C     FIRST, THE TRUE CPHF INTER-SHELL CONTRIBUTIONS.
C     WE KNOW UA(IJ) FOR I>J, AND UA(IJ) + UA(JI) + SA(IJ) = 0.
C
      DO 360 I=1,L0
         ISH = NCONF(I)
         DO 350 J=1,NORB
            JSH = NCONF(J)
            IF(ISH.EQ.JSH) GO TO 350
            IJROT = IROT(I,J)
            DO 330 IXYZ=1,NXYZ
               WA = TWO * W(I,J,IXYZ)
               IF(I.GT.J) THEN
                  DO 310 JXYZ=1,NXYZ
                     FCM(IXYZ,JXYZ) = FCM(IXYZ,JXYZ) + WA*U(IJROT,JXYZ)
  310             CONTINUE
               ELSE
                  IJ = (J*J-J)/2 + I
                  DO 320 JXYZ=1,NXYZ
                     FCM(IXYZ,JXYZ) = FCM(IXYZ,JXYZ)
     *                              - WA * (U(IJROT,JXYZ)+SA(IJ,JXYZ))
  320             CONTINUE
               END IF
  330       CONTINUE
  350    CONTINUE
  360 CONTINUE
C
      IF(OUT) THEN
         WRITE(IW,*) 'HESSIAN WITH CPHF WA*UB TERMS'
         CALL CPFCM(FCM,NAT)
      END IF
C
C     THEN, INTRA-SHELL CONTRIBUTIONS FOR REDUNDANT ROTATIONS.
C     W IS SYMMETRIC FOR INTRA-SHELL SUBBLOCKS, AND WE KNOW
C     THE FORMULA UA(IJ) + UA(JI) + SA(IJ) = 0.
C
      IJ=0
      DO 460 I=1,NORB
         ISH = NCONF(I)
         DO 450 J=1,I
            IJ = IJ+1
            JSH = NCONF(J)
            IF(ISH.NE.JSH) GO TO 450
            DO 430 IXYZ=1,NXYZ
               WA = -W(I,J,IXYZ)
               IF(I.NE.J) WA = WA+WA
               DO 420 JXYZ=1,NXYZ
                  FCM(IXYZ,JXYZ) = FCM(IXYZ,JXYZ) + WA*SA(IJ,JXYZ)
  420          CONTINUE
  430       CONTINUE
  450    CONTINUE
  460 CONTINUE
C
      IF(OUT) THEN
         WRITE(IW,*) 'HESSIAN WITH NON-CPHF WA*UB TERMS'
         CALL CPFCM(FCM,NAT)
      END IF
C
C     ----- ADD POSSIBLE CI RESPONSE CONTRIBUTIONS -----
C
      IF(.NOT.TCSCF) GO TO 700
      DO 590 IXYZ=1,NXYZ
         DO 580 JXYZ=1,NXYZ
            CIRESP(1) = U(NROT-1,JXYZ)
            CIRESP(2) = U(NROT  ,JXYZ)
            DAB = ZERO
            DO 560 ICI=1,2
               DO 550 JCI=1,2
                 IJCI = IADD(ICI,JCI)
                 DUM = ZERO
                 DO 530 I=1,NORB
                    DO 520 J=1,NORB
                       IJ = IA(I) + J
                       IF(J.GT.I) IJ = IA(J) + I
                       DUM = DUM + SA(IJ,IXYZ)*EPSIJ(I,J,IJCI)
  520               CONTINUE
  530            CONTINUE
                 DAB = DAB + CIRESP(ICI)*CICOEF(JCI,1)
     *                         * (DHAM(IJCI,IXYZ)-TWO*DUM)
  550          CONTINUE
  560       CONTINUE
            FCM(IXYZ,JXYZ) = FCM(IXYZ,JXYZ) + TWO*DAB
  580    CONTINUE
  590 CONTINUE
C
      IF(OUT) THEN
         WRITE(IW,*) 'HESSIAN WITH CI RESPONSE TERMS ADDED'
         CALL CPFCM(FCM,NAT)
      END IF
C
C     ----- SYMMETRIZE THE HESSIAN AND SAVE IT -----
C
  700 CONTINUE
      CALL CPSYM(FCM,NXYZ)
      CALL DAWRIT(IDAF,IODA,FCM,NXYZ*NXYZ,4,0)
C
C     ---- GET TRANSFORMED DIPOLE INTEGRALS AND CALCULATE RESPONSE ----
C     ---- FACTOR CONTRIBUTION TO DIPOLE DERIVATIVE                ----
C
      CALL DAREAD(IDAF,IODA,SA(1,1),L2,252,0)
      CALL DAREAD(IDAF,IODA,SA(1,2),L2,253,0)
      CALL DAREAD(IDAF,IODA,SA(1,3),L2,254,0)
C
      CALL VCLR(DDM,1,9*NAT)
C
      JMAX=NORB
C
      DO 800 I=1,L0
         ISH = NCONF(I)
         DO 810 J=1,I
            IF (J .GT. JMAX) GOTO 800
            JSH = NCONF(J)
            IF(ISH.EQ.JSH) GO TO 810
            FIJ  = F(ISH) - F(JSH)
            IJ = IA(I) + J
            IJROT = IROT(I,J)
            DO 820 IXYZ=1,NXYZ
            NN= 1 + (IXYZ-1)*3
              DDM(NN  )=DDM(NN  )+(FOUR*FIJ*U(IJROT,IXYZ)*SA(IJ,1))
              DDM(NN+1)=DDM(NN+1)+(FOUR*FIJ*U(IJROT,IXYZ)*SA(IJ,2))
              DDM(NN+2)=DDM(NN+2)+(FOUR*FIJ*U(IJROT,IXYZ)*SA(IJ,3))
  820       CONTINUE
  810    CONTINUE
  800 CONTINUE
C
C     ---- ADD RESPONSE CONTRIBUTIONS TO DIPOLE DERIVATIVE ----
C
      IF(OUT) THEN
         WRITE(IW,9998)
         CALL DDMOUT(DDM,3*NAT)
      END IF
      CALL DAREAD(IDAF,IODA,DDMA,NAT*9,34,0)
      CALL VADD(DDMA,1,DDM,1,DDMA,1,NAT*9)
      CALL DAWRIT(IDAF,IODA,DDMA,NAT*9,34,0)
C
C     ----- ADD POSSIBLE CI RESPONSE CONTRIBUTIONS TO DIPOLE DERIV.-----
C
      IF(.NOT.TCSCF) GO TO 900
C
      CALL VCLR(DDM,1,9*NAT)
C
      NCOPO=NORB-2*NPAIR
      DO 850 IXYZ=1,NXYZ
         NN= 1 + (IXYZ-1)*3
         CIRESP(1) = U(NROT-1,IXYZ)
         CIRESP(2) = U(NROT  ,IXYZ)
         DUMX = ZERO
         DUMY = ZERO
         DUMZ = ZERO
         DO 860 I=1,NCOPO
            II = IA(I) + I
            ISH = NCONF(I)
            DUMX = DUMX + F(ISH)*SA(II,1)
            DUMY = DUMY + F(ISH)*SA(II,2)
            DUMZ = DUMZ + F(ISH)*SA(II,3)
  860    CONTINUE
            DO 870 ICI=1,2
               NP=NCOPO+ICI
               II = IA(NP) + NP
               DUMA(ICI,1) = TWO*(DUMX + SA(II,1))
               DUMA(ICI,2) = TWO*(DUMY + SA(II,2))
               DUMA(ICI,3) = TWO*(DUMZ + SA(II,3))
               DDM(NN  )=DDM(NN  )-TWO*CIRESP(ICI)*
     *                                 CICOEF(ICI,1)*DUMA(ICI,1)
               DDM(NN+1)=DDM(NN+1)-TWO*CIRESP(ICI)*
     *                                 CICOEF(ICI,1)*DUMA(ICI,2)
               DDM(NN+2)=DDM(NN+2)-TWO*CIRESP(ICI)*
     *                                 CICOEF(ICI,1)*DUMA(ICI,3)
  870       CONTINUE
  850 CONTINUE
C
      IF(OUT) THEN
         WRITE(IW,9999)
         CALL DDMOUT(DDM,3*NAT)
      END IF
      CALL DAREAD(IDAF,IODA,DDMA,NAT*9,34,0)
      CALL VADD(DDMA,1,DDM,1,DDMA,1,NAT*9)
      CALL DAWRIT(IDAF,IODA,DDMA,NAT*9,34,0)
C
  900 CONTINUE
C
C     ---- SYMMETRIZE DIPOLE DERIVATIVE ----
C
      CALL CPSDDM(DDMA,SK,3*NAT)
C
C     ---- CALCULATE RESPONSE CONTRIBUTION TO ALPHA POLARIZABILITY ----
C
      IF(POLAR) THEN
         JMAX=NORB
C
         DO 910 K=1,3
            DO 915 L=1,K
               KL = IA(K) + L
               APOL(KL)=ZERO
               DO 920 I=1,L1
                  ISH = NCONF(I)
                  DO 930 J=1,I
                     IF (J .GT. JMAX) GOTO 920
                     JSH = NCONF(J)
                     IF(ISH.EQ.JSH) GO TO 930
                        FIJ  = F(ISH) - F(JSH)
                        IJ = IA(I) + J
                        IJROT = IROT(I,J)
                        APOL(KL)=APOL(KL)+(FOUR*FIJ*
     *                                   U(IJROT,NXYZ+K)*SA(IJ,L))
  930             CONTINUE
  920          CONTINUE
  915       CONTINUE
  910    CONTINUE
C
         CALL DAWRIT(IDAF,IODA,APOL,6,251,0)
         IF(OUT) THEN
            WRITE(IW,9998)
            CALL POLOUT(APOL)
         END IF
C
C     ----- POSSIBLE CI RESPONSE CONTRIBUTIONS TO POLARIZABILITY -----
C
      IF(.NOT.TCSCF) GO TO 990
C
         CALL VCLR(APOL,1,6)
         DO 940 K=1,3
            CIUF(1,K) = U(NROT-1,NXYZ+K)
            CIUF(2,K) = U(NROT  ,NXYZ+K)
  940    CONTINUE
C
         DO 950 ICI=1,2
            DO 955 K=1,3
               DO 960 L=1,K
                  KL=IA(K)+L
                  APOL(KL)=APOL(KL)-TWO*CIUF(ICI,K)*
     *                                  CICOEF(ICI,1)*DUMA(ICI,L)
  960          CONTINUE
  955       CONTINUE
  950    CONTINUE
C
         IF(OUT) THEN
            WRITE(IW,9999)
            CALL POLOUT(APOL)
         END IF
         CALL DAREAD(IDAF,IODA,APOLA,6,251,0)
         CALL VADD(APOLA,1,APOL,1,APOLA,1,6)
         CALL DAWRIT(IDAF,IODA,APOLA,6,251,0)
C
  990    CONTINUE
C
      END IF
      RETURN
C
 9998 FORMAT(/10X,31("-")/
     *        10X,'RESPONSE FACTOR CONTRIBUTION TO'/
     *        10X,31(1H-))
 9999 FORMAT(/10X,34("-")/
     *        10X,'CI RESPONSE FACTOR CONTRIBUTION TO'/
     *        10X,34(1H-))
      END
C*MODULE CPROHF  *DECK ROCPHF
      SUBROUTINE ROCPHF(TCSCF,XX,IX,NINTMX,IA,ALP,BET,EPS,ZETA,
     *                  HMO,IROT,A,B,SA,W,FCM,FCKDER,EPSIJ,DHAM,EGRAD,
     *                  ACI,NAT,L0,L1,L2,L3,NROT,NXYZ,NHAM,NORB,NFT18,
     *                  OUT,INCORE,HF,NNXYZ)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL INCORE,OUT,TCSCF,IOCC,JOCC,KOCC,LOCC,
     *        ICOP,JCOP,KCOP,LCOP,IPR1,JPR1,KPR1,LPR1,
     *        IPR2,JPR2,KPR2,LPR2,GOPARR,MASWRK,DSKWRK,SVDSKW,
     *        POLAR
C
      DIMENSION XX(NINTMX),IX(*),IA(L1),ALP(L2),BET(L2),
     *          EPS(L1,L1),ZETA(L2,NHAM),HMO(L2),IROT(L0,L0),
     *          A(NROT,NROT),B(NROT,NNXYZ),SA(L2,NXYZ),
     *          W(L1,NORB,NXYZ),FCM(NXYZ,NXYZ),FCKDER(L1,L1),
     *          EPSIJ(L1,L1,3),DHAM(3,NXYZ),EGRAD(NXYZ),
     *          ACI(NROT,2),HF(L2,3)
C
      PARAMETER (MXATM=500, MXAO=2047)
C
      COMMON /CHFINF/ CHFSLV,CPTYPE,NWDCHF,POLAR
      COMMON /FUNCT / ETOT,EG(3,MXATM)
      COMMON /GVBCI / CIHAM(91),CILOW(12),JPAIR,KONE,KTWO,KCORB(2,12)
      COMMON /GVBWFN/ CICOEF(2,12),F(25),ALPHA(325),BETA(325),NO(10),
     *                NCO,NSETO,NOPEN,NPAIR,NORBB,NCONF(MXAO),NHAMM
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCKLAB/ LABSIZ
C
      PARAMETER (ZERO=0.0D+00, HALF=0.5D+00, ONE=1.0D+00, TWO=2.0D+00,
     *           SMALL=1.0D-10)
C
C     ----- SET UP GENERAL OPEN SHELL CPHF PROBLEM -----
C     IF -INCORE- IS TRUE, SET UP THE ENTIRE ORBITAL HESSIAN -A-
C     IN MEMORY, -ACI- IS NOT USED IN THIS CASE.
C     OTHERWISE, -A- IS NOT COMPUTED, ALTHOUGH THE POSSIBLE TCSCF
C     CI TERMS ARE COMPUTED IN -ACI-.
C
C     ALL THE OTHER ARRAYS SUCH AS -EPS- OR -ZETA- AND -W- AND SO ON
C     ARE ALWAYS FORMED IN MEMORY BY THIS ROUTINE.
C
      VALHF = ZERO
      VAL4  = ZERO
      AFACT = ZERO
      BFACT = ZERO
      ICOP = .FALSE.
      JCOP = .FALSE.
      KCOP = .FALSE.
      LCOP = .FALSE.
      IPR1 = .FALSE.
      JPR1 = .FALSE.
      KPR1 = .FALSE.
      LPR1 = .FALSE.
      IPR2 = .FALSE.
      JPR2 = .FALSE.
      KPR2 = .FALSE.
      LPR2 = .FALSE.
      NCLOP = NCO+NOPEN
C
C     FIRST FILL ALPHA AND BETA COUPLING COEFFICIENT MATRICES
C
      CALL VCLR(ALP,1,L2)
      CALL VCLR(BET,1,L2)
      IJ=0
      DO 110 I=1,NORB
         DO 100 J=1,I
            IJ = IJ + 1
            ISH = NCONF(I)
            JSH = NCONF(J)
            IJSH = IA(ISH) + JSH
            ALP(IJ) = ALPHA(IJSH)
            BET(IJ) =  BETA(IJSH)
  100    CONTINUE
  110 CONTINUE
C
C     ----- SET UP ROTATION INDEX ARRAY -----
C
      MROT=0
      DO 220 I=1,L0
         ISH = NCONF(I)
         DO 210 J=1,I
            JSH = NCONF(J)
            IF(JSH.EQ.ISH) THEN
               IROT(I,J) = 0
               IROT(J,I) = 0
            ELSE
               MROT=MROT+1
               IROT(I,J) = MROT
               IROT(J,I) = MROT
            END IF
  210    CONTINUE
  220 CONTINUE
      IF(TCSCF) MROT = MROT+2
      IF(MROT.NE.NROT) CALL ABRT
C
C     ZERO OUT CPHF -A- AND -B- MATRICES, AND THE -W- MATRIX
C     ZERO OUT LAGRANGIAN -EPS- AND GENERALIZED LAGRANGIANS -ZETA-
C     FOR TCSCF, ZERO OUT BARE LAGRANGIANS -EPSIJ-
C
      IF(INCORE) THEN
         CALL VCLR(A,1,NROT*NROT)
      ELSE
         IF(TCSCF) CALL VCLR(ACI,1,NROT*2)
      END IF
      CALL VCLR(B,1,NROT*NNXYZ)
      CALL VCLR(W,1,L1*NORB*NXYZ)
      CALL VCLR(EPS,1,L3)
      CALL VCLR(ZETA,1,L2*NHAM)
      IF(TCSCF) CALL VCLR(EPSIJ,1,3*L3)
C
C     ----- READ IN 2 ELECTRON MO INTEGRALS -----
C
      CALL SEQREW(IJKT)
      IF (MASWRK) READ(IJKT)
C
  300 CONTINUE
      CALL PREAD(IJKT,XX,IX,NX,NINTMX)
      IF(NX.EQ.0) GO TO 900
      MX=ABS(NX)
      IF(MX.GT.NINTMX) CALL ABRT
C
C     ----- UNPACK LABEL AND PROCESS INTEGRAL -----
C     TRANSFORMED INTEGRALS ASSUMED IN REVERSE CANONICAL ORDER
C
      DO 800 M=1,MX
      VAL   =  XX(M)
C
                       NPACK = M
                       IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                         LABEL1 = IX( 2*NPACK - 1 )
                         LABEL2 = IX( 2*NPACK     )
                         IPACK = ISHFT( LABEL1, -16 )
                         JPACK = IAND( LABEL1, 65535 )
                         KPACK = ISHFT( LABEL2, -16 )
                         LPACK = IAND( LABEL2, 65535 )
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
                       K = IPACK
                       L = JPACK
                       I = KPACK
                       J = LPACK
C
      IF(J.EQ.I           ) VAL=VAL*HALF
      IF(L.EQ.K           ) VAL=VAL*HALF
      IF(K.EQ.I.AND.L.EQ.J) VAL=VAL*HALF
      VAL2 = VAL+VAL
C
      IJ = IA(I)+J
      IK = IA(I)+K
      IL = IA(I)+L
      KL = IA(K)+L
      JK = IA(MAX(J,K))+MIN(J,K)
      JL = IA(MAX(J,L))+MIN(J,L)
C
      ISH = NCONF(I)
      JSH = NCONF(J)
      KSH = NCONF(K)
      LSH = NCONF(L)
C
      IOCC = I.LE.NORB
      JOCC = J.LE.NORB
      KOCC = K.LE.NORB
      LOCC = L.LE.NORB
C
      IF(TCSCF) THEN
         VALHF = VAL*HALF
         VAL4 = VAL2 + VAL2
C
         ICOP = I.LE.NCLOP
         JCOP = J.LE.NCLOP
         KCOP = K.LE.NCLOP
         LCOP = L.LE.NCLOP
C
         IPR1 = I.EQ.NORB-1
         JPR1 = J.EQ.NORB-1
         KPR1 = K.EQ.NORB-1
         LPR1 = L.EQ.NORB-1
C
         IPR2 = I.EQ.NORB
         JPR2 = J.EQ.NORB
         KPR2 = K.EQ.NORB
         LPR2 = L.EQ.NORB
      END IF
C
C                  COULOMB CONTRIBUTIONS TO -EPS- AND -ZETA-
C                                             (I,I//K,L)
C
      IF(IOCC  .AND.  I.EQ.J) THEN
         IF(TCSCF) THEN
            IF(ICOP) THEN
               IF(KCOP) THEN
                  EPSIJ(K,L,1) = EPSIJ(K,L,1) + ALP(IK)*VAL2
                  EPSIJ(K,L,3) = EPSIJ(K,L,3) + ALP(IK)*VAL2
               ELSE IF(KPR1) THEN
                  EPSIJ(K,L,1) = EPSIJ(K,L,1) + F(NCONF(I))*VAL4
               ELSE IF(KPR2) THEN
                  EPSIJ(K,L,3) = EPSIJ(K,L,3) + F(NCONF(I))*VAL4
               END IF
               IF(LCOP) THEN
                  EPSIJ(L,K,1) = EPSIJ(L,K,1) + ALP(IL)*VAL2
                  EPSIJ(L,K,3) = EPSIJ(L,K,3) + ALP(IL)*VAL2
               ELSE IF(LPR1) THEN
                  EPSIJ(L,K,1) = EPSIJ(L,K,1) + F(NCONF(I))*VAL4
               ELSE IF(LPR2) THEN
                  EPSIJ(L,K,3) = EPSIJ(L,K,3) + F(NCONF(I))*VAL4
               END IF
            ELSE IF(IPR1) THEN
               IF(KCOP) THEN
                  EPSIJ(K,L,1) = EPSIJ(K,L,1) + F(NCONF(K))*VAL4
               ELSE IF(KPR1) THEN
                  EPSIJ(K,L,1) = EPSIJ(K,L,1) + VAL2
               END IF
               IF(LCOP) THEN
                  EPSIJ(L,K,1) = EPSIJ(L,K,1) + F(NCONF(L))*VAL4
               ELSE IF(LPR1) THEN
                  EPSIJ(L,K,1) = EPSIJ(L,K,1) + VAL2
               END IF
            ELSE IF(IPR2) THEN
               IF(KCOP) THEN
                  EPSIJ(K,L,3) = EPSIJ(K,L,3) + F(NCONF(K))*VAL4
               ELSE IF(KPR2) THEN
                  EPSIJ(K,L,3) = EPSIJ(K,L,3) + VAL2
               END IF
               IF(LCOP) THEN
                  EPSIJ(L,K,3) = EPSIJ(L,K,3) + F(NCONF(L))*VAL4
               ELSE IF(LPR2) THEN
                  EPSIJ(L,K,3) = EPSIJ(L,K,3) + VAL2
               END IF
            END IF
         ELSE
            EPS(K,L)=EPS(K,L) + ALP(IK)*VAL2
            EPS(L,K)=EPS(L,K) + ALP(IL)*VAL2
         END IF
         DO 410 MSH=1,NHAM
            IM = IA(MAX(ISH,MSH))+MIN(ISH,MSH)
            ZETA(KL,MSH)=ZETA(KL,MSH) + ALPHA(IM)*VAL2
  410    CONTINUE
      END IF
C                                             (I,J//K,K)
      IF(KOCC  .AND.  K.EQ.L) THEN
         IF(TCSCF) THEN
            IF(KCOP) THEN
               IF(ICOP) THEN
                  EPSIJ(I,J,1) = EPSIJ(I,J,1) + ALP(IK)*VAL2
                  EPSIJ(I,J,3) = EPSIJ(I,J,3) + ALP(IK)*VAL2
               ELSE IF(IPR1) THEN
                  EPSIJ(I,J,1) = EPSIJ(I,J,1) + F(NCONF(K))*VAL4
               ELSE IF(IPR2) THEN
                  EPSIJ(I,J,3) = EPSIJ(I,J,3) + F(NCONF(K))*VAL4
               END IF
               IF(JCOP) THEN
                  EPSIJ(J,I,1) = EPSIJ(J,I,1) + ALP(JK)*VAL2
                  EPSIJ(J,I,3) = EPSIJ(J,I,3) + ALP(JK)*VAL2
               ELSE IF(JPR1) THEN
                  EPSIJ(J,I,1) = EPSIJ(J,I,1) + F(NCONF(K))*VAL4
               ELSE IF(JPR2) THEN
                  EPSIJ(J,I,3) = EPSIJ(J,I,3) + F(NCONF(K))*VAL4
               END IF
            ELSE IF(KPR1) THEN
               IF(ICOP) THEN
                  EPSIJ(I,J,1) = EPSIJ(I,J,1) + F(NCONF(I))*VAL4
               ELSE IF(IPR1) THEN
                  EPSIJ(I,J,1) = EPSIJ(I,J,1) + VAL2
               END IF
               IF(JCOP) THEN
                  EPSIJ(J,I,1) = EPSIJ(J,I,1) + F(NCONF(J))*VAL4
               ELSE IF(JPR1) THEN
                  EPSIJ(J,I,1) = EPSIJ(J,I,1) + VAL2
               END IF
            ELSE IF(KPR2) THEN
               IF(ICOP) THEN
                  EPSIJ(I,J,3) = EPSIJ(I,J,3) + F(NCONF(I))*VAL4
               ELSE IF(IPR2) THEN
                  EPSIJ(I,J,3) = EPSIJ(I,J,3) + VAL2
               END IF
               IF(JCOP) THEN
                  EPSIJ(J,I,3) = EPSIJ(J,I,3) + F(NCONF(J))*VAL4
               ELSE IF(JPR2) THEN
                  EPSIJ(J,I,3) = EPSIJ(J,I,3) + VAL2
               END IF
            END IF
         ELSE
            EPS(I,J)=EPS(I,J) + ALP(IK)*VAL2
            EPS(J,I)=EPS(J,I) + ALP(JK)*VAL2
         END IF
         DO 420 MSH=1,NHAM
            KM = IA(MAX(MSH,KSH))+MIN(MSH,KSH)
            ZETA(IJ,MSH)=ZETA(IJ,MSH) + ALPHA(KM)*VAL2
  420    CONTINUE
      END IF
C
C                  EXCHANGE CONTRIBUTIONS TO -EPS- AND -ZETA-
C                                             (I,J//I,L)
      IF(IOCC  .AND.  I.EQ.K) THEN
         IF(TCSCF) THEN
            IF(ICOP) THEN
               IF(JCOP) THEN
                  EPSIJ(J,L,1) = EPSIJ(J,L,1) + BET(IJ)*VAL
                  EPSIJ(J,L,3) = EPSIJ(J,L,3) + BET(IJ)*VAL
               ELSE IF(JPR1) THEN
                  EPSIJ(J,L,1) = EPSIJ(J,L,1) - F(NCONF(I))*VAL
               ELSE IF(JPR2) THEN
                  EPSIJ(J,L,3) = EPSIJ(J,L,3) - F(NCONF(I))*VAL
               END IF
               IF(LCOP) THEN
                  EPSIJ(L,J,1) = EPSIJ(L,J,1) + BET(IL)*VAL
                  EPSIJ(L,J,3) = EPSIJ(L,J,3) + BET(IL)*VAL
               ELSE IF(LPR1) THEN
                  EPSIJ(L,J,1) = EPSIJ(L,J,1) - F(NCONF(I))*VAL
               ELSE IF(LPR2) THEN
                  EPSIJ(L,J,3) = EPSIJ(L,J,3) - F(NCONF(I))*VAL
               END IF
            ELSE IF(IPR1) THEN
               IF(JCOP) THEN
                  EPSIJ(J,L,1) = EPSIJ(J,L,1) - F(NCONF(J))*VAL
               ELSE IF(JPR2) THEN
                  EPSIJ(J,L,2) = EPSIJ(J,L,2) + VALHF
               END IF
               IF(LCOP) THEN
                  EPSIJ(L,J,1) = EPSIJ(L,J,1) - F(NCONF(L))*VAL
               ELSE IF(LPR2) THEN
                  EPSIJ(L,J,2) = EPSIJ(L,J,2) + VALHF
               END IF
            ELSE IF(IPR2) THEN
               IF(JCOP) THEN
                  EPSIJ(J,L,3) = EPSIJ(J,L,3) - F(NCONF(J))*VAL
               ELSE IF(JPR1) THEN
                  EPSIJ(J,L,2) = EPSIJ(J,L,2) + VALHF
               END IF
               IF(LCOP) THEN
                  EPSIJ(L,J,3) = EPSIJ(L,J,3) - F(NCONF(L))*VAL
               ELSE IF(LPR1) THEN
                  EPSIJ(L,J,2) = EPSIJ(L,J,2) + VALHF
               END IF
            END IF
         ELSE
            EPS(J,L)=EPS(J,L) + BET(IJ)*VAL
            EPS(L,J)=EPS(L,J) + BET(IL)*VAL
         END IF
         DO 510 MSH=1,NHAM
            IM = IA(MAX(ISH,MSH))+MIN(ISH,MSH)
            ZETA(JL,MSH)=ZETA(JL,MSH) + BETA(IM)*VAL
  510    CONTINUE
      END IF
C                                             (I,J//K,I)
      IF(IOCC  .AND.  I.EQ.L) THEN
         IF(TCSCF) THEN
            IF(ICOP) THEN
               IF(JCOP) THEN
                  EPSIJ(J,K,1) = EPSIJ(J,K,1) + BET(IJ)*VAL
                  EPSIJ(J,K,3) = EPSIJ(J,K,3) + BET(IJ)*VAL
               ELSE IF(JPR1) THEN
                  EPSIJ(J,K,1) = EPSIJ(J,K,1) - F(NCONF(I))*VAL
               ELSE IF(JPR2) THEN
                  EPSIJ(J,K,3) = EPSIJ(J,K,3) - F(NCONF(I))*VAL
               END IF
               IF(KCOP) THEN
                  EPSIJ(K,J,1) = EPSIJ(K,J,1) + BET(IK)*VAL
                  EPSIJ(K,J,3) = EPSIJ(K,J,3) + BET(IK)*VAL
               ELSE IF(KPR1) THEN
                  EPSIJ(K,J,1) = EPSIJ(K,J,1) - F(NCONF(I))*VAL
               ELSE IF(KPR2) THEN
                  EPSIJ(K,J,3) = EPSIJ(K,J,3) - F(NCONF(I))*VAL
               END IF
            ELSE IF(IPR1) THEN
               IF(JCOP) THEN
                  EPSIJ(J,K,1) = EPSIJ(J,K,1) - F(NCONF(J))*VAL
               ELSE IF(JPR2) THEN
                  EPSIJ(J,K,2) = EPSIJ(J,K,2) + VALHF
               END IF
               IF(KCOP) THEN
                  EPSIJ(K,J,1) = EPSIJ(K,J,1) - F(NCONF(K))*VAL
               ELSE IF(KPR2) THEN
                  EPSIJ(K,J,2) = EPSIJ(K,J,2) + VALHF
               END IF
            ELSE IF(IPR2) THEN
               IF(JCOP) THEN
                  EPSIJ(J,K,3) = EPSIJ(J,K,3) - F(NCONF(J))*VAL
               ELSE IF(JPR1) THEN
                  EPSIJ(J,K,2) = EPSIJ(J,K,2) + VALHF
               END IF
               IF(KCOP) THEN
                  EPSIJ(K,J,3) = EPSIJ(K,J,3) - F(NCONF(K))*VAL
               ELSE IF(KPR1) THEN
                  EPSIJ(K,J,2) = EPSIJ(K,J,2) + VALHF
               END IF
            END IF
         ELSE
            EPS(J,K)=EPS(J,K) + BET(IJ)*VAL
            EPS(K,J)=EPS(K,J) + BET(IK)*VAL
         END IF
         DO 520 MSH=1,NHAM
            IM = IA(MAX(ISH,MSH))+MIN(ISH,MSH)
            ZETA(JK,MSH)=ZETA(JK,MSH) + BETA(IM)*VAL
  520    CONTINUE
      END IF
C                                             (I,J//J,L)
      IF(JOCC  .AND.  J.EQ.K) THEN
         IF(TCSCF) THEN
            IF(JCOP) THEN
               IF(ICOP) THEN
                  EPSIJ(I,L,1) = EPSIJ(I,L,1) + BET(IJ)*VAL
                  EPSIJ(I,L,3) = EPSIJ(I,L,3) + BET(IJ)*VAL
               ELSE IF(IPR1) THEN
                  EPSIJ(I,L,1) = EPSIJ(I,L,1) - F(NCONF(J))*VAL
               ELSE IF(IPR2) THEN
                  EPSIJ(I,L,3) = EPSIJ(I,L,3) - F(NCONF(J))*VAL
               END IF
               IF(LCOP) THEN
                  EPSIJ(L,I,1) = EPSIJ(L,I,1) + BET(JL)*VAL
                  EPSIJ(L,I,3) = EPSIJ(L,I,3) + BET(JL)*VAL
               ELSE IF(LPR1) THEN
                  EPSIJ(L,I,1) = EPSIJ(L,I,1) - F(NCONF(J))*VAL
               ELSE IF(LPR2) THEN
                  EPSIJ(L,I,3) = EPSIJ(L,I,3) - F(NCONF(J))*VAL
               END IF
            ELSE IF(JPR1) THEN
               IF(ICOP) THEN
                  EPSIJ(I,L,1) = EPSIJ(I,L,1) - F(NCONF(I))*VAL
               ELSE IF(IPR2) THEN
                  EPSIJ(I,L,2) = EPSIJ(I,L,2) + VALHF
               END IF
               IF(LCOP) THEN
                  EPSIJ(L,I,1) = EPSIJ(L,I,1) - F(NCONF(L))*VAL
               ELSE IF(LPR2) THEN
                  EPSIJ(L,I,2) = EPSIJ(L,I,2) + VALHF
               END IF
            ELSE IF(JPR2) THEN
               IF(ICOP) THEN
                  EPSIJ(I,L,3) = EPSIJ(I,L,3) - F(NCONF(I))*VAL
               ELSE IF(IPR1) THEN
                  EPSIJ(I,L,2) = EPSIJ(I,L,2) + VALHF
               END IF
               IF(LCOP) THEN
                  EPSIJ(L,I,3) = EPSIJ(L,I,3) - F(NCONF(L))*VAL
               ELSE IF(LPR1) THEN
                  EPSIJ(L,I,2) = EPSIJ(L,I,2) + VALHF
               END IF
            END IF
         ELSE
            EPS(I,L)=EPS(I,L) + BET(IJ)*VAL
            EPS(L,I)=EPS(L,I) + BET(JL)*VAL
         END IF
         DO 530 MSH=1,NHAM
            JM = IA(MAX(JSH,MSH))+MIN(JSH,MSH)
            ZETA(IL,MSH)=ZETA(IL,MSH) + BETA(JM)*VAL
  530    CONTINUE
      END IF
C                                             (I,J//K,J)
      IF(JOCC  .AND.  J.EQ.L) THEN
         IF(TCSCF) THEN
            IF(JCOP) THEN
               IF(ICOP) THEN
                  EPSIJ(I,K,1) = EPSIJ(I,K,1) + BET(IJ)*VAL
                  EPSIJ(I,K,3) = EPSIJ(I,K,3) + BET(IJ)*VAL
               ELSE IF(IPR1) THEN
                  EPSIJ(I,K,1) = EPSIJ(I,K,1) - F(NCONF(J))*VAL
               ELSE IF(IPR2) THEN
                  EPSIJ(I,K,3) = EPSIJ(I,K,3) - F(NCONF(J))*VAL
               END IF
               IF(KCOP) THEN
                  EPSIJ(K,I,1) = EPSIJ(K,I,1) + BET(JK)*VAL
                  EPSIJ(K,I,3) = EPSIJ(K,I,3) + BET(JK)*VAL
               ELSE IF(KPR1) THEN
                  EPSIJ(K,I,1) = EPSIJ(K,I,1) - F(NCONF(J))*VAL
               ELSE IF(KPR2) THEN
                  EPSIJ(K,I,3) = EPSIJ(K,I,3) - F(NCONF(J))*VAL
               END IF
            ELSE IF(JPR1) THEN
               IF(ICOP) THEN
                  EPSIJ(I,K,1) = EPSIJ(I,K,1) - F(NCONF(I))*VAL
               ELSE IF(IPR2) THEN
                  EPSIJ(I,K,2) = EPSIJ(I,K,2) + VALHF
               END IF
               IF(KCOP) THEN
                  EPSIJ(K,I,1) = EPSIJ(K,I,1) - F(NCONF(K))*VAL
               ELSE IF(KPR2) THEN
                  EPSIJ(K,I,2) = EPSIJ(K,I,2) + VALHF
               END IF
            ELSE IF(JPR2) THEN
               IF(ICOP) THEN
                  EPSIJ(I,K,3) = EPSIJ(I,K,3) - F(NCONF(I))*VAL
               ELSE IF(IPR1) THEN
                  EPSIJ(I,K,2) = EPSIJ(I,K,2) + VALHF
               END IF
               IF(KCOP) THEN
                  EPSIJ(K,I,3) = EPSIJ(K,I,3) - F(NCONF(K))*VAL
               ELSE IF(KPR1) THEN
                  EPSIJ(K,I,2) = EPSIJ(K,I,2) + VALHF
               END IF
            END IF
         ELSE
            EPS(I,K)=EPS(I,K) + BET(IJ)*VAL
            EPS(K,I)=EPS(K,I) + BET(JK)*VAL
         END IF
         DO 540 MSH=1,NHAM
            JM = IA(MAX(JSH,MSH))+MIN(JSH,MSH)
            ZETA(IK,MSH)=ZETA(IK,MSH) + BETA(JM)*VAL
  540    CONTINUE
      END IF
C
C                  INTEGRAL CONTRIBUTIONS TO -B- MATRIX
C                                                     (I,J//K,L)
      IF(ISH.NE.JSH  .AND.  KOCC  .AND.  LOCC) THEN
         IJROT = IROT(I,J)
         AAVAL = (ALP(JK)-ALP(IK)) * VAL2
         IF(ABS(AAVAL).GT.SMALL) THEN
            DO 610 IXYZ=1,NXYZ
               B(IJROT,IXYZ) = B(IJROT,IXYZ) + SA(KL,IXYZ)*AAVAL
  610       CONTINUE
         END IF
      END IF
C                                                     (K,L//I,J)
      IF(KSH.NE.LSH  .AND.  IOCC  .AND.  JOCC) THEN
         KLROT = IROT(K,L)
         AAVAL = (ALP(IL)-ALP(IK)) * VAL2
         IF(ABS(AAVAL).GT.SMALL) THEN
            DO 620 IXYZ=1,NXYZ
                B(KLROT,IXYZ) = B(KLROT,IXYZ) + SA(IJ,IXYZ)*AAVAL
  620       CONTINUE
         END IF
      END IF
C                                           (I,K//J,L) AND (I,L//J,K)
      IF(ISH.NE.KSH  .AND.  JOCC  .AND.  LOCC) THEN
         IKROT = IROT(I,K)
         IF(J.GE.L) BBVAL = (BET(JK)-BET(IJ)) * VAL
         IF(J.LT.L) BBVAL = (BET(KL)-BET(IL)) * VAL
         IF(ABS(BBVAL).GT.SMALL) THEN
            DO 630 IXYZ=1,NXYZ
               B(IKROT,IXYZ) = B(IKROT,IXYZ) + SA(JL,IXYZ)*BBVAL
  630       CONTINUE
         END IF
      END IF
C                                           (I,K//L,J) AND (I,L//K,J)
      IF(ISH.NE.LSH  .AND.  JOCC  .AND.  KOCC) THEN
         ILROT = IROT(I,L)
         IF(J.GE.K) BBVAL = (BET(JL)-BET(IJ)) * VAL
         IF(J.LT.K) BBVAL = (BET(KL)-BET(IK)) * VAL
         IF(ABS(BBVAL).GT.SMALL) THEN
            DO 640 IXYZ=1,NXYZ
               B(ILROT,IXYZ) = B(ILROT,IXYZ) + SA(JK,IXYZ)*BBVAL
  640       CONTINUE
         END IF
      END IF
C                                           (K,I//J,L) AND (K,J//I,L)
      IF(JSH.NE.KSH  .AND.  IOCC  .AND.  LOCC) THEN
         JKROT = IROT(J,K)
         BBVAL = (BET(IK)-BET(IJ)) * VAL
         IF(J.LT.K) BBVAL = -BBVAL
         IF(ABS(BBVAL).GT.SMALL) THEN
            DO 650 IXYZ=1,NXYZ
               B(JKROT,IXYZ) = B(JKROT,IXYZ) + SA(IL,IXYZ)*BBVAL
  650       CONTINUE
         END IF
      END IF
C                                           (K,I//L,J) AND (K,J//L,I)
      IF(JSH.NE.LSH  .AND.  IOCC  .AND.  KOCC) THEN
         JLROT = IROT(J,L)
         BBVAL = (BET(IL)-BET(IJ)) * VAL
         IF(J.LT.L) BBVAL = -BBVAL
         IF(ABS(BBVAL).GT.SMALL) THEN
            DO 660 IXYZ=1,NXYZ
               B(JLROT,IXYZ) = B(JLROT,IXYZ) + SA(IK,IXYZ)*BBVAL
  660       CONTINUE
         END IF
      END IF
C
C                  INTEGRAL CONTRIBUTIONS TO -W- MATRIX
C                  ALL 8 INTEGRAL PERMUTATIONS CONTRIBUTE
C
      IF(JOCC  .AND.  KOCC  .AND.  LOCC) THEN
         AVAL = VAL2 * (ALP(JL) + ALP(JK))
         B1VAL = VAL * (BET(KL) + BET(JK))
         B2VAL = VAL * (BET(KL) + BET(JL))
         DO 710 IXYZ=1,NXYZ
            W(I,J,IXYZ) = W(I,J,IXYZ) +  AVAL * SA(KL,IXYZ)
            W(I,K,IXYZ) = W(I,K,IXYZ) + B1VAL * SA(JL,IXYZ)
            W(I,L,IXYZ) = W(I,L,IXYZ) + B2VAL * SA(JK,IXYZ)
  710    CONTINUE
      END IF
C
      IF(IOCC  .AND.  KOCC  .AND.  LOCC) THEN
         AVAL = VAL2 * (ALP(IL) + ALP(IK))
         B1VAL = VAL * (BET(KL) + BET(IK))
         B2VAL = VAL * (BET(KL) + BET(IL))
         DO 720 IXYZ=1,NXYZ
            W(J,I,IXYZ) = W(J,I,IXYZ) +  AVAL * SA(KL,IXYZ)
            W(J,K,IXYZ) = W(J,K,IXYZ) + B1VAL * SA(IL,IXYZ)
            W(J,L,IXYZ) = W(J,L,IXYZ) + B2VAL * SA(IK,IXYZ)
  720    CONTINUE
      END IF
C
      IF(IOCC  .AND.  JOCC  .AND.  LOCC) THEN
         AVAL = VAL2 * (ALP(JL) + ALP(IL))
         B1VAL = VAL * (BET(IJ) + BET(IL))
         B2VAL = VAL * (BET(IJ) + BET(JL))
         DO 730 IXYZ=1,NXYZ
            W(K,L,IXYZ) = W(K,L,IXYZ) +  AVAL * SA(IJ,IXYZ)
            W(K,I,IXYZ) = W(K,I,IXYZ) + B1VAL * SA(JL,IXYZ)
            W(K,J,IXYZ) = W(K,J,IXYZ) + B2VAL * SA(IL,IXYZ)
  730    CONTINUE
      END IF
C
      IF(IOCC  .AND.  JOCC .AND.  KOCC) THEN
         AVAL = VAL2 * (ALP(JK) + ALP(IK))
         B1VAL = VAL * (BET(IJ) + BET(IK))
         B2VAL = VAL * (BET(IJ) + BET(JK))
         DO 740 IXYZ=1,NXYZ
            W(L,K,IXYZ) = W(L,K,IXYZ) +  AVAL * SA(IJ,IXYZ)
            W(L,I,IXYZ) = W(L,I,IXYZ) + B1VAL * SA(JK,IXYZ)
            W(L,J,IXYZ) = W(L,J,IXYZ) + B2VAL * SA(IK,IXYZ)
  740    CONTINUE
      END IF
C
C                  INTEGRAL CONTRIBUTIONS TO -A- MATRIX
C                                              (I,J//K,L)
      IF(.NOT.INCORE) GO TO 800
      IF(ISH.NE.JSH  .AND.  KSH.NE.LSH) THEN
         IJROT = IROT(I,J)
         KLROT = IROT(K,L)
         IF(IJROT.LT.KLROT) THEN
            NN    = IJROT
            IJROT = KLROT
            KLROT = NN
         END IF
         AFACT = ALP(IK)-ALP(JK)-ALP(IL)+ALP(JL)
         A(IJROT,KLROT) = A(IJROT,KLROT) + AFACT*VAL2
      END IF
C                                         (I,K//J,L) AND (I,L//J,K)
      IF(ISH.NE.KSH  .AND.  JSH.NE.LSH) THEN
         IKROT=IROT(I,K)
         JLROT=IROT(J,L)
         IF(IKROT.LT.JLROT) THEN
            NN    = IKROT
            IKROT = JLROT
            JLROT = NN
         END IF
         IF(J.GE.L) BFACT =  BET(IJ) -BET(JK) -BET(IL) +BET(KL)
         IF(J.LT.L) BFACT =  BET(IL) -BET(KL) -BET(IJ) +BET(JK)
         A(IKROT,JLROT) = A(IKROT,JLROT) + BFACT*VAL
      END IF
C                                         (I,L//K,J) AND (I,K//L,J)
      IF(ISH.NE.LSH  .AND.  JSH.NE.KSH) THEN
         ILROT=IROT(I,L)
         JKROT=IROT(J,K)
         IF(ILROT.LT.JKROT) THEN
            NN    = ILROT
            ILROT = JKROT
            JKROT = NN
         END IF
         IF(K.GE.J) BFACT =  BET(IK) -BET(KL) -BET(IJ) +BET(JL)
         IF(K.LT.J) BFACT =  BET(IJ) -BET(JL) -BET(IK) +BET(KL)
         A(ILROT,JKROT) = A(ILROT,JKROT) + BFACT*VAL
      END IF
C
C                  AND GO BACK FOR ANOTHER INTEGRAL BUFFER.
C
  800 CONTINUE
      IF(NX.GT.0) GO TO 300
C
C                  DONE WITH 2 ELECTRON INTEGRALS
C
  900 CONTINUE
      CALL SEQREW(IJKT)
C
C     IF RUNNING IN PARALLEL, WE MUST SUM ALL INTEGRAL CONTRIBUTIONS
C
      IF(GOPARR) THEN
         IF(TCSCF)  CALL DDI_GSUMF(2111,EPSIJ,3*L3)
                    CALL DDI_GSUMF(2112,EPS  ,L3)
                    CALL DDI_GSUMF(2113,ZETA ,L2*NHAM)
                    CALL DDI_GSUMF(2114,B    ,NROT*NXYZ)
                    CALL DDI_GSUMF(2115,W    ,L1*NORB*NXYZ)
         IF(INCORE) CALL GSUMCP(2116,A,NROT*NROT)
      END IF
C
C     DOUBLE THE DIAGONALS, SYMMETRIZE, AND SIGN CHANGE.
C
      II = 0
      DO 920 I=1,L1
         II = II + I
         DO 910 MSH=1,NHAM
            ZETA(II,MSH) = ZETA(II,MSH)+ZETA(II,MSH)
  910    CONTINUE
  920 CONTINUE
C
      IF(.NOT.INCORE) GO TO 980
      DO 950 IJROT=1,NROT
         A(IJROT,IJROT) = A(IJROT,IJROT)+A(IJROT,IJROT)
         DO 940 KLROT=1,IJROT
            A(KLROT,IJROT) = A(IJROT,KLROT)
  940    CONTINUE
  950 CONTINUE
C
  980 CONTINUE
      CALL DSCAL(L1*NORB*NXYZ,-ONE,W,1)
C
C        FOR TCSCF, ADD THE ONE ELECTRON TERM TO -EPSIJ-,
C                   AND FORM THE TOTAL LAGRANGIAN -EPS-.
C
      IF(TCSCF) THEN
         M = NORB-1
         N = NORB
         DO 1010 I=1,NORB
            FI = F(NCONF(I))
            IF(I.EQ.M  .OR.  I.EQ.N) FI = ONE
            DO 1000 J=1,L1
               IJ = IA(MAX(I,J)) + MIN(I,J)
               IF(I.NE.N) EPSIJ(I,J,1) = FI*HMO(IJ) + EPSIJ(I,J,1)
               IF(I.NE.M) EPSIJ(I,J,3) = FI*HMO(IJ) + EPSIJ(I,J,3)
 1000       CONTINUE
 1010    CONTINUE
C
         DO 1030 I=1,L1
            DO 1020 J=1,L1
               EPS(I,J) = CICOEF(1,1)*CICOEF(1,1)*EPSIJ(I,J,1) +
     *              TWO * CICOEF(1,1)*CICOEF(2,1)*EPSIJ(I,J,2) +
     *                    CICOEF(2,1)*CICOEF(2,1)*EPSIJ(I,J,3)
 1020       CONTINUE
 1030    CONTINUE
C
         IF(OUT) THEN
            WRITE(IW,*) 'CICOEF=', CICOEF(1,1),CICOEF(2,1)
            WRITE(IW,*) 'EPS(1,1) MATRIX'
            CALL PRSQ(EPSIJ(1,1,1),L1,L1,L1)
            WRITE(IW,*) 'EPS(1,2) MATRIX'
            CALL PRSQ(EPSIJ(1,1,2),L1,L1,L1)
            WRITE(IW,*) 'EPS(2,2) MATRIX'
            CALL PRSQ(EPSIJ(1,1,3),L1,L1,L1)
         END IF
C
C        FOR NON-TCSCF, SIMPLY ADD THE ONE ELECTRON TERM TO -EPS-
C
      ELSE
         IJ=0
         DO 1060 I=1,L1
            FI = ZERO
            IF(I.LE.NORB) FI = F(NCONF(I))
            DO 1050 J=1,L1
               IJ = IA(MAX(I,J)) + MIN(I,J)
               EPS(I,J) = FI*HMO(IJ) + EPS(I,J)
 1050       CONTINUE
 1060    CONTINUE
      END IF
C
C        THE SCF SHOULD HAVE CONVERGED TO A SYMMETRIC LAGRANGIAN,
C        BUT ONLY TO ABOUT 8-9 DIGITS.  FORCE -EPS- EXACTLY SYMMETRIC,
C        TO ENSURE CONSTRUCTION OF A SYMMETRIC ORBITAL HESSIAN -A-.
C
      DO 1075 I=1,NORB
         DO 1070 J=1,I
            AVE = HALF * (EPS(I,J) + EPS(J,I))
            EPS(I,J) = AVE
            EPS(J,I) = AVE
 1070    CONTINUE
         DO 1072 J=NORB+1,L1
            EPS(I,J) = ZERO
 1072    CONTINUE
 1075 CONTINUE
C
C        ADD THE ONE ELECTRON CONTRIBUTIONS TO -ZETA-
C
      DO 1080 MSH=1,NHAM
         CALL DAXPY(L2,F(MSH),HMO,1,ZETA(1,MSH),1)
 1080 CONTINUE
C
C        THE VIRTUAL SHELL -ZETA- IS IDENTICALLY ZERO
C
      CALL VCLR(ZETA(1,NHAM+1),1,L2)
C
      IF(OUT) THEN
         WRITE(IW,*) 'LAGRANGIAN -EPS- IN MO BASIS IS'
         CALL PRSQ(EPS,L1,L1,L1)
         DO 1090 MSH=1,NHAM
            WRITE(IW,*) 'GEN.LAG. -ZETA- FOR SHELL',MSH
            CALL PRTRI(ZETA(1,MSH),L1)
 1090    CONTINUE
      END IF
C
C     ----- ADD FOCK DERIVATIVE TERMS TO -W- AND -B- -----
C     ONLY THE MASTER NODE HAS THE DERIVATIVE FOCK MATRIX DISK FILE
C     ----- ADD SA*EB CONTRIBUTION TO THE HESSIAN -----
C
      CALL DAREAD(IDAF,IODA,FCM,NXYZ*NXYZ,4,0)
      SVDSKW = DSKWRK
      DSKWRK = .FALSE.
      CALL SEQREW(NFT18)
C
      DO 1190 IXYZ=1,NXYZ
         CALL SQREAD(NFT18,FCKDER,L3)
         IF(OUT) THEN
            WRITE(IW,*) 'FCKDER',IXYZ
            CALL PRSQ(FCKDER,L1,L1,L1)
         END IF
         DO 1130 I=1,L1
            ISH = NCONF(I)
            DO 1110 J=1,I
               JSH = NCONF(J)
               IF(ISH.EQ.JSH) GO TO 1110
               IJROT = IROT(I,J)
               B(IJROT,IXYZ) = B(IJROT,IXYZ) + FCKDER(I,J)
     *                                       - FCKDER(J,I)
 1110       CONTINUE
            DO 1120 J=1,NORB
               W(I,J,IXYZ) = W(I,J,IXYZ) + TWO*FCKDER(J,I)
 1120       CONTINUE
 1130    CONTINUE
C
         DO 1160 JXYZ=1,NXYZ
            DUM=ZERO
            DO 1150 IO=1,NORB
               DO 1140 JO=1,NORB
                  IJ=IA(MAX(IO,JO))+MIN(IO,JO)
                  DUM=DUM+FCKDER(IO,JO)*SA(IJ,JXYZ)
 1140          CONTINUE
 1150       CONTINUE
            FCM(JXYZ,IXYZ) = FCM(JXYZ,IXYZ) - (DUM+DUM)
 1160    CONTINUE
 1190 CONTINUE
C
      CALL SEQREW(NFT18)
      DSKWRK = SVDSKW
      CALL DAWRIT(IDAF,IODA,FCM,NXYZ*NXYZ,4,0)
      IF(OUT) THEN
         WRITE(IW,*) 'NUCLEAR HESSIAN WITH SA*EB TERMS'
         CALL CPFCM(FCM,NAT)
      END IF
C
C     ----- LAGRANGIAN CONTRIBUTIONS TO -A- MATRIX -----
C
      IF(.NOT.INCORE) GO TO 1300
      DO 1250 I=1,L0
         ISH = NCONF(I)
         DO 1240 J=1,I
            JSH = NCONF(J)
            IF(ISH.EQ.JSH) GO TO 1240
            IJROT = IROT(I,J)
            DO 1230 K=1,L0
               KSH = NCONF(K)
               DO 1220 L=1,K
                  LSH = NCONF(L)
                  IF(KSH.EQ.LSH) GO TO 1220
                  KLROT = IROT(K,L)
C
                  IF(J.EQ.K) THEN
                     IL = IA(MAX(I,L)) + MIN(I,L)
                     A(IJROT,KLROT) = A(IJROT,KLROT) + EPS(I,L)
     *                                               - ZETA(IL,JSH)
                  END IF
C
                  IF(I.EQ.K) THEN
                     JL = IA(MAX(J,L)) + MIN(J,L)
                     A(IJROT,KLROT) = A(IJROT,KLROT) - EPS(J,L)
     *                                               + ZETA(JL,ISH)
                  END IF
C
                  IF(J.EQ.L) THEN
                     IK = IA(MAX(I,K)) + MIN(I,K)
                     A(IJROT,KLROT) = A(IJROT,KLROT) - EPS(I,K)
     *                                               + ZETA(IK,JSH)
                  END IF
C
                  IF(I.EQ.L) THEN
                     JK = IA(MAX(J,K)) + MIN(J,K)
                     A(IJROT,KLROT) = A(IJROT,KLROT) + EPS(J,K)
     *                                               - ZETA(JK,ISH)
                  END IF
 1220          CONTINUE
 1230       CONTINUE
 1240    CONTINUE
 1250 CONTINUE
C
C     ----- LAGRANGIAN CONTRIBUTIONS TO -B- MATRIX -----
C
 1300 CONTINUE
      DO 1380 I=1,L0
         ISH = NCONF(I)
         DO 1370 J=1,I
            JSH = NCONF(J)
            IF(ISH.EQ.JSH) GO TO 1370
            IJROT = IROT(I,J)
            DO 1360 L=1,NORB
                LP1 = L+1
                DO 1350 K=LP1,L1
                   KL = IA(K) + L
                   IF(K.EQ.J) THEN
                      IL = IA(MAX(I,L)) + MIN(I,L)
                      EZIL = EPS(I,L)-ZETA(IL,JSH)
                      DO 1310 IXYZ=1,NXYZ
                         B(IJROT,IXYZ) = B(IJROT,IXYZ) -
     *                                   SA(KL,IXYZ)*EZIL
 1310                 CONTINUE
                   END IF
                   IF(K.EQ.I) THEN
                      JL = IA(MAX(J,L)) + MIN(J,L)
                      EZJL = EPS(J,L)-ZETA(JL,ISH)
                      DO 1330 IXYZ=1,NXYZ
                         B(IJROT,IXYZ) = B(IJROT,IXYZ) +
     *                                   SA(KL,IXYZ)*EZJL
 1330                 CONTINUE
                   END IF
 1350           CONTINUE
 1360        CONTINUE
 1370     CONTINUE
 1380 CONTINUE
C
C     ----- LAGRANGIAN CONTRIBUTIONS TO -W- MATRIX -----
C
      DO 1440 I=1,L0
         DO 1430 J=1,NORB
            JSH = NCONF(J)
            DO 1420 K=1,NORB
               JK=IA(MAX(J,K)) + MIN(J,K)
               IK=IA(MAX(I,K)) + MIN(I,K)
               E2 = -(EPS(J,K)+EPS(J,K))
               EZ = -EPS(I,K)-ZETA(IK,JSH)
               DO 1410 IXYZ=1,NXYZ
                  W(I,J,IXYZ) = W(I,J,IXYZ)
     *                        + EZ*SA(JK,IXYZ) + E2*SA(IK,IXYZ)
 1410          CONTINUE
 1420       CONTINUE
 1430    CONTINUE
 1440 CONTINUE
C
      IF(OUT) THEN
         DO 1480 IXYZ=1,NXYZ
            WRITE(IW,*) '-W- FOR DERIVATIVE A=',IXYZ
            CALL PRSQ(W(1,1,IXYZ),NORB,L1,L1)
 1480    CONTINUE
      END IF
C
C        ----- ADD TCSCF CI DERIVATIVE TERMS -----
C        THE A11 AND B1-A BLOCKS ARE COMPLETE AT THIS POINT.
C
      IF(.NOT.TCSCF) GO TO 2000
      MROT = NROT-1
C
C                                 A22 BLOCK
C
      IF(INCORE) THEN
         A(MROT,MROT) = HALF*(CIHAM(1) -ETOT + CICOEF(1,1)*CICOEF(1,1))
         A(MROT,NROT) = HALF*(CIHAM(2)       + CICOEF(1,1)*CICOEF(2,1))
         A(NROT,MROT) = A(MROT,NROT)
         A(NROT,NROT) = HALF*(CIHAM(3) -ETOT + CICOEF(2,1)*CICOEF(2,1))
      ELSE
         ACI(MROT,1) = HALF*(CIHAM(1) -ETOT + CICOEF(1,1)*CICOEF(1,1))
         ACI(MROT,2) = HALF*(CIHAM(2)       + CICOEF(1,1)*CICOEF(2,1))
         ACI(NROT,1) = ACI(MROT,2)
         ACI(NROT,2) = HALF*(CIHAM(3) -ETOT + CICOEF(2,1)*CICOEF(2,1))
      END IF
C
C                                 A21 BLOCK
C
      DO 1520 I=1,L0
         DO 1510 J=1,I
            IJROT=IROT(I,J)
            IF(IJROT.EQ.0) GO TO 1510
C
            DUM = TWO*(CICOEF(1,1)*(EPSIJ(J,I,1)-EPSIJ(I,J,1))
     *               + CICOEF(2,1)*(EPSIJ(J,I,2)-EPSIJ(I,J,2)))
            IF(INCORE) THEN
               A(IJROT,MROT) = DUM
               A(MROT,IJROT) = DUM
            ELSE
               ACI(IJROT,1) = DUM
            END IF
C
            DUM = TWO*(CICOEF(1,1)*(EPSIJ(J,I,2)-EPSIJ(I,J,2))
     *               + CICOEF(2,1)*(EPSIJ(J,I,3)-EPSIJ(I,J,3)))
            IF(INCORE) THEN
               A(IJROT,NROT) = DUM
               A(NROT,IJROT) = DUM
            ELSE
               ACI(IJROT,2) = DUM
            END IF
 1510    CONTINUE
 1520 CONTINUE
C
C                                 B2-A BLOCK
C        RECOVER H-A ELEMENTS
C
      CALL DAREAD(IDAF,IODA,DHAM,3*NXYZ,66,0)
      IF(OUT) THEN
         WRITE(IW,*) 'DERIVATIVE HAMILTONIAN ELEMENTS ARE'
         CALL PRSQ(DHAM,NXYZ,3,3)
      END IF
C
C        COMPUTE ELECTRONIC GRADIENT
C
      DO 1690 IXYZ=1,NXYZ
         EEE=ZERO
         IJ=0
         DO 1640 I=1,NORB
            DO 1630 J=1,I
               IJ = IJ+1
               DUM = EPS(I,J)*SA(IJ,IXYZ)
               IF(I.NE.J) DUM = DUM+DUM
               EEE = EEE + DUM
 1630       CONTINUE
 1640    CONTINUE
C
         HHH = ZERO
         IJ=0
         DO 1670 I=1,2
            DO 1660 J=1,I
               IJ=IJ+1
               DUM = CICOEF(I,1)*CICOEF(J,1)*DHAM(IJ,IXYZ)
               IF(I.NE.J) DUM = DUM+DUM
               HHH = HHH + DUM
 1660       CONTINUE
 1670    CONTINUE
         EGRAD(IXYZ) = HHH - TWO*EEE
 1690 CONTINUE
C
      IF(OUT) THEN
         WRITE(IW,*) 'GRADIENT OF ELECTRONIC ENERGY IS'
         CALL EGOUT(EGRAD,NAT)
      END IF
C
C        AND USE THEM TO FORM B2-A
C
      DO 1790 IXYZ=1,NXYZ
         DUM1 = ZERO
         DUM2 = ZERO
         DUM3 = ZERO
         II = 0
         IJ = 0
         DO 1750 I=1,NORB
            DO 1740 J=1,I
               IJ = IJ+1
               DUM1 = DUM1 + TWO * SA(IJ,IXYZ) * EPSIJ(I,J,1)
               DUM2 = DUM2 + TWO * SA(IJ,IXYZ) * EPSIJ(I,J,2)
               DUM3 = DUM3 + TWO * SA(IJ,IXYZ) * EPSIJ(I,J,3)
 1740       CONTINUE
            II = II + I
            DUM1 = DUM1 - SA(II,IXYZ) * EPSIJ(I,I,1)
            DUM2 = DUM2 - SA(II,IXYZ) * EPSIJ(I,I,2)
            DUM3 = DUM3 - SA(II,IXYZ) * EPSIJ(I,I,3)
 1750    CONTINUE
C
         B(MROT,IXYZ) = -HALF*CICOEF(1,1)*DHAM(1,IXYZ)
     *                  -HALF*CICOEF(2,1)*DHAM(2,IXYZ)
     *                  +HALF*CICOEF(1,1)*EGRAD(IXYZ)
     *                  +     CICOEF(1,1)*DUM1
     *                  +     CICOEF(2,1)*DUM2
         B(NROT,IXYZ) = -HALF*CICOEF(1,1)*DHAM(2,IXYZ)
     *                  -HALF*CICOEF(2,1)*DHAM(3,IXYZ)
     *                  +HALF*CICOEF(2,1)*EGRAD(IXYZ)
     *                  +     CICOEF(1,1)*DUM2
     *                  +     CICOEF(2,1)*DUM3
 1790 CONTINUE
C
C      ---- IF CALCULATING ALPHA POLARIZABILITIES NEED TO SET UP ----
C      ---- 3 EXTRA COLUMNS OF -B- MATRIX                         ----
C
 2000 CONTINUE
      IF(POLAR) THEN
         CALL DAREAD(IDAF,IODA,HF(1,1),L2,252,0)
         CALL DAREAD(IDAF,IODA,HF(1,2),L2,253,0)
         CALL DAREAD(IDAF,IODA,HF(1,3),L2,254,0)
C
         DO 2100 I=1,L0
            ISH = NCONF(I)
            DO 2110 J=1,I
               JSH = NCONF(J)
               IJROT = IROT(I,J)
               IF(IJROT.EQ.0) GO TO 2100
               IF(ISH.EQ.JSH) GO TO 2110
               FIJ  = F(ISH) - F(JSH)
               IJ = IA(I) + J
                   B(IJROT,NXYZ+1)=FIJ*HF(IJ,1)
                   B(IJROT,NXYZ+2)=FIJ*HF(IJ,2)
                   B(IJROT,NXYZ+3)=FIJ*HF(IJ,3)
 2110       CONTINUE
 2100    CONTINUE
C
C     ---- ADD POSSIBLE CI CONTRIBUTIONS TO -B- MATRIX ----
C
         IF(TCSCF) THEN
            NCOPO=NORBB-2*NPAIR
            DUMX = ZERO
            DUMY = ZERO
            DUMZ = ZERO
            DO 2150 I=1,NCOPO
               II = IA(I) + I
               ISH = NCONF(I)
               DUMX = DUMX + F(ISH)*HF(II,1)
               DUMY = DUMY + F(ISH)*HF(II,2)
               DUMZ = DUMZ + F(ISH)*HF(II,3)
 2150       CONTINUE
C
            HMNX=ZERO
            HMNY=ZERO
            HMNZ=ZERO
            DO 2155 I=NORBB-1,NORBB
               ISH = NCONF(I)
               II = IA(I) + I
               HMNX=HMNX+F(ISH)*HF(II,1)
               HMNY=HMNY+F(ISH)*HF(II,2)
               HMNZ=HMNZ+F(ISH)*HF(II,3)
 2155       CONTINUE
C
            DO 2160 ICI=1,2
               IJROT=NROT-2+ICI
               NP=NCOPO+ICI
               ISH = NCONF(NP)
               II = IA(NP) + NP
               DUMAX = TWO*(DUMX + HF(II,1))
               DUMAY = TWO*(DUMY + HF(II,2))
               DUMAZ=  TWO*(DUMZ + HF(II,3))
               B(IJROT,NXYZ+1)=CICOEF(ICI,1)*(DUMX+HMNX)
     *                         -HALF*CICOEF(ICI,1)*DUMAX
               B(IJROT,NXYZ+2)=CICOEF(ICI,1)*(DUMY+HMNY)
     *                         -HALF*CICOEF(ICI,1)*DUMAY
               B(IJROT,NXYZ+3)=CICOEF(ICI,1)*(DUMZ+HMNZ)
     *                         -HALF*CICOEF(ICI,1)*DUMAZ
 2160       CONTINUE
         END IF
C
      END IF
C
C     ----- REVERSE THE SIGN OF -B- FROM THAT IN THE PAPERS -----
C
      CALL DSCAL(NROT*NNXYZ,-ONE,B,1)
C
      IF(OUT) THEN
         IF(INCORE) THEN
            WRITE(IW,*) 'THE CPHF -A- MATRIX IS'
            CALL PRSQ(A,NROT,NROT,NROT)
         END IF
         IF(.NOT.INCORE  .AND.  TCSCF) THEN
            WRITE(IW,*) 'THE CPHF -ACI- MATRIX IS'
            CALL PRSQ(ACI,2,NROT,NROT)
         END IF
         WRITE(IW,*) 'THE CPHF -B- MATRIX IS'
         CALL PRSQ(B,NNXYZ,NROT,NROT)
      END IF
      RETURN
      END
C*MODULE CPROHF  *DECK ROCPOM
      SUBROUTINE ROCPOM(TCSCF,XX,IX,NINTMX,IA,ALP,BET,EPS,ZETA,
     *                  IROT,A,ACI,AII,L0,L1,L2,NROT,NHAM,NWIDTH,
     *                  NFT16,OUT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXAO=2047)
C
      LOGICAL OUT,TCSCF,GOPARR,DSKWRK,MASWRK,SVDSKW
C
      DIMENSION XX(NINTMX),IX(*),IA(L1),ALP(L2),BET(L2),
     *          EPS(L1,L1),ZETA(L2,NHAM),IROT(L0,L0),
     *          A(NROT,NWIDTH),ACI(NROT,2),AII(NROT)
C
      COMMON /GVBWFN/ CICOEF(2,12),F(25),ALPHA(325),BETA(325),NO(10),
     *                NCO,NSETO,NOPEN,NPAIR,NORB,NCONF(MXAO),NHAMM
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCKLAB/ LABSIZ
C
      PARAMETER (ZERO=0.0D+00, HALF=0.5D+00)
C
C     ----- OUT OF CORE FORMATION OF ORBITAL HESSIAN -----
C     STRIPS OF -A- ARE FORMED AND WRITTEN TO UNIT -NFT16-.
C     ARRAYS SUCH AS -IROT-, -ALP-, -BET-, -EPS-, -ZETA- PREEXIST.
C     FOR TCSCF, THE CSF TERMS ALREADY EXIST IN -ACI-.
C
      BFACT = ZERO
      NROTO = NROT
      IF(TCSCF) NROTO = NROT-2
C
      IF (MASWRK) REWIND NFT16
C
      KOL2 = 0
  100 CONTINUE
      KOL0 = KOL2
      KOL1 = KOL2 + 1
      KOL2 = KOL2 + NWIDTH
      IF(KOL2.GT.NROT) KOL2=NROT
      NCOL = KOL2 - KOL0
C
      CALL VCLR(A,1,NROT*NCOL)
      IF(KOL1.GT.NROTO) GO TO 400
C
C           READ IN 2 ELECTRON MO INTEGRALS
C
      CALL SEQREW(IJKT)
      IF (MASWRK) READ(IJKT)
C
  150 CONTINUE
      CALL PREAD(IJKT,XX,IX,NX,NINTMX)
      IF(NX.EQ.0) GO TO 300
      MX=ABS(NX)
      IF(MX.GT.NINTMX) CALL ABRT
C
C     ----- UNPACK LABEL AND PROCESS INTEGRAL -----
C     TRANSFORMED INTEGRALS ASSUMED IN REVERSE CANONICAL ORDER
C
      DO 200 M=1,MX
      VAL   =  XX(M)
C
                       NPACK = M
                       IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                         LABEL1 = IX( 2*NPACK - 1 )
                         LABEL2 = IX( 2*NPACK     )
                         IPACK = ISHFT( LABEL1, -16 )
                         JPACK = IAND( LABEL1, 65535 )
                         KPACK = ISHFT( LABEL2, -16 )
                         LPACK = IAND( LABEL2, 65535 )
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
                       K = IPACK
                       L = JPACK
                       I = KPACK
                       J = LPACK
C
      IF(J.EQ.I           ) VAL=VAL*HALF
      IF(L.EQ.K           ) VAL=VAL*HALF
      IF(K.EQ.I.AND.L.EQ.J) VAL=VAL*HALF
      VAL2 = VAL+VAL
C
      IJ = IA(I)+J
      IK = IA(I)+K
      IL = IA(I)+L
      KL = IA(K)+L
      JK = IA(MAX(J,K))+MIN(J,K)
      JL = IA(MAX(J,L))+MIN(J,L)
C
      ISH = NCONF(I)
      JSH = NCONF(J)
      KSH = NCONF(K)
      LSH = NCONF(L)
C
C                  INTEGRAL CONTRIBUTIONS TO -A- MATRIX
C                                              (I,J//K,L)
      IF(ISH.NE.JSH  .AND.  KSH.NE.LSH) THEN
         IJROT = IROT(I,J)
         KLROT = IROT(K,L)
         AFACT = ALP(IK)-ALP(JK)-ALP(IL)+ALP(JL)
         IF(KLROT.GE.KOL1  .AND.  KLROT.LE.KOL2)
     *      A(IJROT,KLROT-KOL0) = A(IJROT,KLROT-KOL0) + AFACT*VAL2
         IF(IJROT.GE.KOL1  .AND.  IJROT.LE.KOL2)
     *      A(KLROT,IJROT-KOL0) = A(KLROT,IJROT-KOL0) + AFACT*VAL2
      END IF
C                                         (I,K//J,L) AND (I,L//J,K)
      IF(ISH.NE.KSH  .AND.  JSH.NE.LSH) THEN
         IKROT=IROT(I,K)
         JLROT=IROT(J,L)
         IF(J.GE.L) BFACT =  BET(IJ) -BET(JK) -BET(IL) +BET(KL)
         IF(J.LT.L) BFACT =  BET(IL) -BET(KL) -BET(IJ) +BET(JK)
         IF(JLROT.GE.KOL1  .AND.  JLROT.LE.KOL2)
     *      A(IKROT,JLROT-KOL0) = A(IKROT,JLROT-KOL0) + BFACT*VAL
         IF(IKROT.GE.KOL1  .AND.  IKROT.LE.KOL2)
     *      A(JLROT,IKROT-KOL0) = A(JLROT,IKROT-KOL0) + BFACT*VAL
      END IF
C                                         (I,L//K,J) AND (I,K//L,J)
      IF(ISH.NE.LSH  .AND.  JSH.NE.KSH) THEN
         ILROT=IROT(I,L)
         JKROT=IROT(J,K)
         IF(K.GE.J) BFACT =  BET(IK) -BET(KL) -BET(IJ) +BET(JL)
         IF(K.LT.J) BFACT =  BET(IJ) -BET(JL) -BET(IK) +BET(KL)
         IF(JKROT.GE.KOL1  .AND.  JKROT.LE.KOL2)
     *      A(ILROT,JKROT-KOL0) = A(ILROT,JKROT-KOL0) + BFACT*VAL
         IF(ILROT.GE.KOL1  .AND.  ILROT.LE.KOL2)
     *      A(JKROT,ILROT-KOL0) = A(JKROT,ILROT-KOL0) + BFACT*VAL
      END IF
C
C                  AND GO BACK FOR ANOTHER INTEGRAL BUFFER.
C
  200 CONTINUE
      IF(NX.GT.0) GO TO 150
C
  300 CONTINUE
      IF(GOPARR) CALL GSUMCP(2117,A,NROT*NCOL)
C
C     ----- LAGRANGIAN CONTRIBUTIONS TO -A- MATRIX -----
C
      DO 350 I=1,L0
         ISH = NCONF(I)
         DO 340 J=1,I
            JSH = NCONF(J)
            IF(ISH.EQ.JSH) GO TO 340
            IJROT = IROT(I,J)
            DO 330 K=1,L1
               KSH = NCONF(K)
               DO 320 L=1,K
                  LSH = NCONF(L)
                  IF(KSH.EQ.LSH) GO TO 320
                  KLROT = IROT(K,L)
                  IF(KLROT.LT.KOL1  .OR.  KLROT.GT.KOL2) GO TO 320
                  KLROT = KLROT-KOL0
C
                  IF(J.EQ.K) THEN
                     IL = IA(MAX(I,L)) + MIN(I,L)
                     A(IJROT,KLROT) = A(IJROT,KLROT)
     *                              + EPS(I,L) - ZETA(IL,JSH)
                  END IF
C
                  IF(I.EQ.K) THEN
                     JL = IA(MAX(J,L)) + MIN(J,L)
                     A(IJROT,KLROT) = A(IJROT,KLROT)
     *                              - EPS(J,L) + ZETA(JL,ISH)
                  END IF
C
                  IF(J.EQ.L) THEN
                     IK = IA(MAX(I,K)) + MIN(I,K)
                     A(IJROT,KLROT) = A(IJROT,KLROT)
     *                              - EPS(I,K) + ZETA(IK,JSH)
                  END IF
C
                  IF(I.EQ.L) THEN
                     JK = IA(MAX(J,K)) + MIN(J,K)
                     A(IJROT,KLROT) = A(IJROT,KLROT)
     *                              + EPS(J,K) - ZETA(JK,ISH)
                  END IF
  320          CONTINUE
  330       CONTINUE
  340    CONTINUE
  350 CONTINUE
C
C        ADD POSSIBLE CSF COUPLING TERMS
C
  400 CONTINUE
      IF(TCSCF) THEN
         MROT = NROT-1
         DO 420 I=1,NROT
            IF(MROT.GE.KOL1  .AND.  MROT.LE.KOL2)
     *         A(I,MROT-KOL0) = ACI(I,1)
            IF(NROT.GE.KOL1  .AND.  NROT.LE.KOL2)
     *         A(I,NROT-KOL0) = ACI(I,2)
            IF(   I.GE.KOL1  .AND.     I.LE.KOL2) THEN
               A(MROT,I-KOL0) = ACI(I,1)
               A(NROT,I-KOL0) = ACI(I,2)
            END IF
  420    CONTINUE
      END IF
C
      IF(OUT) THEN
         WRITE(IW,9030) KOL1,KOL2
         CALL PRSQ(A,NCOL,NROT,NROT)
      END IF
      SVDSKW = DSKWRK
      DSKWRK = .FALSE.
      CALL SQWRIT(NFT16,A,NROT*NCOL)
      DSKWRK = SVDSKW
C
C         STORE DIAGONAL ELEMENT FOR LATER USE BY SOLVER
C
      DO 410 JROT=KOL1,KOL2
         AII(JROT) = A(JROT,JROT-KOL1+1)
  410 CONTINUE
      IF(KOL2.LT.NROT) GO TO 100
C
      CALL SEQREW(IJKT)
      IF(MASWRK) REWIND NFT16
      RETURN
C
 9030 FORMAT(1X,'SLICE OF -A- MATRIX FOR ROTATIONS',I5,' TO',I5)
      END
