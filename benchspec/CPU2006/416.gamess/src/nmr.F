C  9 dec 03 - mws - regenerate c1 integral list if needed
C  4 nov 03 - MAF - include module for NMR shielding computations
C
C*MODULE NMR  *DECK NMRX
C
      SUBROUTINE NMRX
C
C     Nuclear Magnetic Resonance Chemical Shifts.  Mark A. Freitag 2002.
C     In the comments, equations are referenced as, for example, (3.38)
C     and refer to chapter 3, equation 38 of MAF dissertation.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DOUBLE PRECISION NMR
C
      LOGICAL POEINT,PEVEC,TEDBG,PTEINT,PRMAT,PITER,PDIA,PPARA,ANGINT
      LOGICAL PACK2E,INMEM,GOPARR,MASWRK,DSKWRK,AOSYM,DIRSCF,FDIFF
C
      PARAMETER (MXATM=500, MXSH=1000)
C
      PARAMETER (NNAM=10)
      DIMENSION QNAM(NNAM),KQNAM(NNAM)
C
      COMMON /FMCOM / X(1)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /OPTSCF/ DIRSCF,FDIFF
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                T(432),INVT(48),NT
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA NMR /8HNMR     /
#else
      CHARACTER*8 :: NMR_STR
      EQUIVALENCE (NMR, NMR_STR)
      DATA NMR_STR/"NMR     "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QNAM/8HPOEINT  ,8HPEVEC   ,8HTEDBG   ,8HPTEINT  ,8HPRMAT   ,
     *          8HPITER   ,8HPDIA    ,8HPPARA   ,8HANGINT  ,8HINMEM   /
#else
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"POEINT  ","PEVEC   ","TEDBG   ","PTEINT  ",
     *          "PRMAT   ",
     *          "PITER   ","PDIA    ","PPARA   ","ANGINT  ","INMEM   "/
#endif
      DATA KQNAM/0,0,0,0,0,
     *           0,0,0,0,0/
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA NONE,RNONE,RHF,CHECK/4HNONE,8HNONE    ,8HRHF     ,8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      CHARACTER*4 :: NONE_STR
      EQUIVALENCE (NONE, NONE_STR)
      CHARACTER*8 :: RNONE_STR
      EQUIVALENCE (RNONE, RNONE_STR)
      CHARACTER*8 :: RHF_STR
      EQUIVALENCE (RHF, RHF_STR)
      DATA NONE_STR,RNONE_STR,RHF_STR,CHECK_STR/"NONE","NONE    ",
     * "RHF     ", "CHECK   "/
#endif
C
C     ----- Are you ready to ROCK?  Print header in .log file -----
C
      IF(MASWRK) WRITE(IW,9999)
C
C     Default values for $NMR group
C
      POEINT=.FALSE.  ! PRINT ONE-ELECTRON INTEGRAL MATRICES
      PEVEC=.FALSE.   ! PRINT EIGENVECTORS OF SHIELDING TENSOR
      TEDBG=.FALSE.   ! PRINT TWO-ELECTRON DEBUGGING INFORMATION (LOTS!)
      PTEINT=.FALSE.  ! PRINT TWO-ELECTRON INTEGRALS
      PRMAT=.FALSE.   ! PRINT MAJOR COMPONENT MATRICES
      PITER=.FALSE.   ! PRINT ITERATIONS IN FORMING P(1,0)
      PDIA=.FALSE.    ! PRINT DIAMAGNETIC PART OF THE SHIELDING TENSOR
      PPARA=.FALSE.   ! PRINT PARAMAGNETIC PART OF THE SHIELDING TENSOR
      ANGINT=.TRUE.   ! INCREASE THE ANGULAR MOMENTUM TO DO TWO E- INTS.
      INMEM=.FALSE.   ! CARRY TWO E- INTS IN MEMORY. IF .F., ANGINT=.T.
C
      JRET = 0
      CALL NAMEIO(IR,JRET,NMR,NNAM,QNAM,KQNAM,
     *            POEINT,PEVEC,TEDBG,PTEINT,PRMAT,
     *            PITER,PDIA,PPARA,ANGINT,INMEM,
     *            0,0,0,0,
     *            0,0,0,0,0,   0,0,0,0,0,
     *            0,0,0,0,0,   0,0,0,0,0,
     *            0,0,0,0,0,   0,0,0,0,0,
     *            0,0,0,0,0,   0,0,0,0,0,
     *            0,0,0,0,0,   0,0,0,0,0)
      IF(JRET.EQ.2) THEN
         IF(MASWRK) WRITE (IW,9000)
         CALL ABRT
      END IF
      IF(MASWRK) WRITE(IW,9010) INMEM,ANGINT,
     *                          PDIA,PPARA,PEVEC,
     *                          POEINT,PTEINT,PRMAT,PITER
C
C         NMR IS PROGRAMMED ONLY FOR THE RHF CASE, BUT IT WILL ACCEPT
C         ANY SOLVENT MODEL UNDER THE APPROXIMATION THAT THE SOLVATED
C         UNPERTURBED DENSITY WILL BE PICKED UP AND USED.
C         NMR IS NOT ENABLED FOR PARALLEL CALCULATION.
C
      IF(SCFTYP.NE.RHF   .OR.  GOPARR  .OR.  DIRSCF  .OR.
     *   MPCTYP.NE.NONE  .OR.
     *  (MPLEVL.NE.0     .OR.  CITYP.NE.RNONE  .OR.
     *   CCTYP.NE.RNONE  .OR.  DFTYPE.NE.RNONE)) THEN
        IF(MASWRK) WRITE(IW,9100)
        CALL ABRT
      END IF
C
C     evaluate the closed shell wavefunction
C
      CALL ENERGX
C
C     Turn off symmetry, get c1 ao integral list if necessary
C
      AOSYM = NT.GT.1
      CALL SYMOFF
      IF(AOSYM) THEN
         IF(MASWRK) WRITE(IW,9030)
         CALL JANDK
      END IF
C
C     If you're going to use Cartesian integrals, you have to store
C     more to memory.  At this point, the use of Cartesian integrals
C     is primarily for debugging.
C
      IF(.NOT.ANGINT) INMEM=.TRUE.
C
C     Print debugging information if exetyp is NMR
C
      IF(EXETYP.EQ.NMR) PRMAT=.TRUE.
C
C     ----- PARTITION FAST MEMORY -----
C
      L2     = (NUM*NUM+NUM)/2    ! THE USUAL UPPER-TRIANGULAR SPACE
      L2S    = (NUM*NUM-NUM)/2    ! UPPER-TRIANGULAR W/O DIAGONAL
      L3     = NUM*NUM
      L4     = NUM**4
      NMRISM = L3*NAT*3*3
      NMRISMS= L2S*NAT*3*3
      NMRILG = L3*NAT*3*3*3
      NMRHI  = L3*NAT
      NMRHIS = L2S*NAT
C
      IF(ANGINT) THEN
C
      CALL VALFM(LOADFM)
      LSOESOI = 1 + LOADFM
      LDELIN  = LSOESOI + NMRISMS ! ONE-ELECTRON SPIN-ORBIT INTS (3.38)
      LDELINM = LDELIN  + NMRISM  ! ONE-ELECTRON DEL-TYPE INTS   (3.39)
      LTINT   = LDELINM + NMRISM  ! MATRIX OF DELIN
      LTINTM  = LTINT   + NMRILG  ! ONE-ELECTRON T-TYPE INTS     (3.39)
      LAST    = LTINTM  + NMRILG  ! MATRIX OF TINT
C
      LR      = LAST
      LBR     = LR      + 25*25*25*25
      LAST    = LBR     + 25*25*25*25
      LTINTBK = LAST
      LDELINBK= LTINTBK + 225*125*3*3*3 ! 1 MWORD FOR ONEINTS OVERLAPPED
      LAST1   = LDELINBK+ 225*125*3*3
      LAIN    = LAST
      LX      = LAIN    + 75*225*225    ! WITH THE 4 MWORDS FOR TWOINTSX
      LAST2   = LX      + 225*225
C
      LSH01X  = MAX(LAST1,LAST2)
      LSH01Y  = LSH01X  + NMRHIS  ! H(0,1) MATRIX X-DIRECTION    (3.38)
      LSH01Z  = LSH01Y  + NMRHIS  !   "      "    Y-    "
      LH11XX  = LSH01Z  + NMRHIS  !   "      "    Z-    "
      LH11XY  = LH11XX  + NMRHI   ! H(1,1) MATRIX XX-DIRECTION   (3.39)
      LH11XZ  = LH11XY  + NMRHI   !   "      "    XY-   "
      LH11YX  = LH11XZ  + NMRHI   !   "      "    XZ-   "
      LH11YY  = LH11YX  + NMRHI   !   "      "    YX-   "
      LH11YZ  = LH11YY  + NMRHI   !   "      "    YY-   "
      LH11ZX  = LH11YZ  + NMRHI   !   "      "    YZ-   "
      LH11ZY  = LH11ZX  + NMRHI   !   "      "    ZX-   "
      LH11ZZ  = LH11ZY  + NMRHI   !   "      "    ZY-   "
      LSQX    = LH11ZZ  + NMRHI   !   "      "    ZZ-   "
      LSQY    = LSQX    + L2S     ! Q MATRIX X-COMPONENT       (P. 52)
      LSQZ    = LSQY    + L2S     !   "      Y-    "
      LSXIJ   = LSQZ    + L2S     !   "      Z-    "
      LSYIJ   = LSXIJ   + L2S     ! R MATRIX X-COMP (PART OF T MATRIX)
      LSZIJ   = LSYIJ   + L2S     !   "      Y-    "
      LFTXX   = LSZIJ   + L2S     !   "      Z-    "
      LFTXY   = LFTXX   + NAT     ! DIAMAG TERM OF SHIELDING TEN. (3.37)
      LFTXZ   = LFTXY   + NAT
      LFTYX   = LFTXZ   + NAT
      LFTYY   = LFTYX   + NAT
      LFTYZ   = LFTYY   + NAT
      LFTZX   = LFTYZ   + NAT
      LFTZY   = LFTZX   + NAT
      LFTZZ   = LFTZY   + NAT
      LSTXX   = LFTZZ   + NAT
      LSTXY   = LSTXX   + NAT     ! PARAMAG TERM OF SHIELDING TEN (3.37)
      LSTXZ   = LSTXY   + NAT
      LSTYX   = LSTXZ   + NAT
      LSTYY   = LSTYX   + NAT
      LSTYZ   = LSTYY   + NAT
      LSTZX   = LSTYZ   + NAT
      LSTZY   = LSTZX   + NAT
      LSTZZ   = LSTZY   + NAT
      LSSX    = LSTZZ   + NAT
      LSSY    = LSSX    + L2S     ! PERTURBED OVERLAP INTS        (3.48)
      LSSZ    = LSSY    + L2S
      LSS10X  = LSSZ    + L2S
      LSS10Y  = LSS10X  + L2S  ! FULL PERTURBD OVERLAP MAT S(1,0) (3.48)
      LSS10Z  = LSS10Y  + L2S
      LS      = LSS10Z  + L2S
      LP      = LS      + L2      ! OVERLAP MATRIX
      LSP10X  = LP      + L2      ! DENSITY MATRIX
      LSP10Y  = LSP10X  + L2S     ! PERTURBED DENS MATRIX P(1,0) (3.65)
      LSP10Z  = LSP10Y  + L2S
       IF(INMEM) THEN
       LTEINT0 = LSP10Z  + L2S
       LTEINT1 = LTEINT0 + L4      ! TWO-ELECTRON INTEGRALS, UNPERTURBED
       LTEINT2 = LTEINT1 + L4      ! TWO-ELECTRON INTS, X-I SHELL (3.52)
       LTEINT3 = LTEINT2 + L4      ! TWO-ELECTRON INTS, Y-I SHELL (3.52)
       LTEINT4 = LTEINT3 + L4      ! TWO-ELECTRON INTS, Z-I SHELL (3.52)
       LTEINT5 = LTEINT4 + L4      ! TWO-ELECTRON INTS, X-K SHELL (3.52)
       LTEINT6 = LTEINT5 + L4      ! TWO-ELECTRON INTS, Y-K SHELL (3.52)
       LTEWRK  = LTEINT6 + L4      ! TWO-ELECTRON INTS, Z-K SHELL (3.52)
       LWRK    = LTEWRK  + L4      ! FULL PERTURBED TWO E- INT.   (3.52)
       ELSE
       LWRK    = LSP10Z  + L2S
       END IF
      LWRK2   = LWRK    + NUM     ! WORK ARRAY
      LWRK3   = LWRK2   + NUM     ! WORK ARRAY
      LWRK4   = LWRK3   + NUM     ! WORK ARRAY
      LSPS1PX = LWRK4   + NUM     ! WORK ARRAY
      LSPS1PY = LSPS1PX + L2S     ! P(0)*S(1,0)*P(0) - 1ST TERM (3.65)
      LSPS1PZ = LSPS1PY + L2S
      LSD     = LSPS1PZ + L2S
      LSTDX   = LSD     + L2S     ! KINETIC MATRIX                (3.51)
      LSTDY   = LSTDX   + L2S     ! T-TYPE KINETIC MATRIX         (3.50)
      LSTDZ   = LSTDY   + L2S
      LSV     = LSTDZ   + L2S
      LSTVX   = LSV     + L2S     ! NUCLEAR MATRIX                (3.51)
      LSTVY   = LSTVX   + L2S     ! T-TYPE NUCLEAR MATRIX         (3.50)
      LSTVZ   = LSTVY   + L2S
      LSELLX  = LSTVZ   + L2S
      LSELLY  = LSELLX  + L2S     ! ONE-ELECTRON L-LAMBDA INTS    (3.50)
      LSELLZ  = LSELLY  + L2S
      LSH10X  = LSELLZ  + L2S
      LSH10Y  = LSH10X  + L2S     ! H(1,0) MATRICES               (3.50)
      LSH10Z  = LSH10Y  + L2S
      LSF10X  = LSH10Z  + L2S
      LSF10Y  = LSF10X  + L2S     ! PERTURBED FOCK MATS F(1,0)    (3.49)
      LSF10Z  = LSF10Y  + L2S
      LDUM1L2S= LSF10Z  + L2S
      LDUM2L2S= LDUM1L2S+ L2S     ! WORK ARRAY
      LDUM3L2S= LDUM2L2S+ L2S     ! WORK ARRAY
      LDUM    = LDUM3L2S+ L2S     ! WORK ARRAY
      LSP0G10X= LDUM    + L3      ! WORK ARRAY
      LSP0G10Y= LSP0G10X+ L2S     ! P(0)*G(1,0) IN F(1,0)         (3.49)
      LSP0G10Z= LSP0G10Y+ L2S
      LBUFP   = LSP0G10Z+ L2S
      LIX     = LBUFP   + NINTMX
      LAST    = LIX     + NINTMX
C
      ELSE
C
      CALL VALFM(LOADFM)
      LSOESOI = 1 + LOADFM
      LDELIN  = LSOESOI + NMRISMS ! ONE-ELECTRON SPIN-ORBIT INTS (3.38)
      LDELINM = LDELIN  + NMRISM  ! ONE-ELECTRON DEL-TYPE INTS   (3.39)
      LTINT   = LDELINM + NMRISM  ! MATRIX OF DELIN
      LTINTM  = LTINT   + NMRILG  ! ONE-ELECTRON T-TYPE INTS     (3.39)
      LAST    = LTINTM  + NMRILG  ! MATRIX OF TINT
C
      LR      = LAST
      LBR     = LR      + 25*25*25*25
      LAST    = LBR     + 25*25*25*25
      LTINTBK = LAST
      LDELINBK= LTINTBK + 225*125*3*3*3 ! 1 MWORD FOR ONEINTS OVERLAPPED
      LAST1   = LDELINBK+ 225*125*3*3
      LAIN    = LAST
      LX      = LAIN    + 75*225*225    ! WITH THE 4 MWORDS FOR TWOINTSX
      LAST2   = LX      + 225*225
C
      LSH01X  = MAX(LAST1,LAST2)
      LSH01Y  = LSH01X  + NMRHIS  ! H(0,1) MATRIX X-DIRECTION    (3.38)
      LSH01Z  = LSH01Y  + NMRHIS  !   "      "    Y-    "
      LH11XX  = LSH01Z  + NMRHIS  !   "      "    Z-    "
      LH11XY  = LH11XX  + NMRHI   ! H(1,1) MATRIX XX-DIRECTION   (3.39)
      LH11XZ  = LH11XY  + NMRHI   !   "      "    XY-   "
      LH11YX  = LH11XZ  + NMRHI   !   "      "    XZ-   "
      LH11YY  = LH11YX  + NMRHI   !   "      "    YX-   "
      LH11YZ  = LH11YY  + NMRHI   !   "      "    YY-   "
      LH11ZX  = LH11YZ  + NMRHI   !   "      "    YZ-   "
      LH11ZY  = LH11ZX  + NMRHI   !   "      "    ZX-   "
      LH11ZZ  = LH11ZY  + NMRHI   !   "      "    ZY-   "
      LSQX    = LH11ZZ  + NMRHI   !   "      "    ZZ-   "
      LSQY    = LSQX    + L2S     ! Q MATRIX X-COMPONENT         (P. 52)
      LSQZ    = LSQY    + L2S     !   "      Y-    "
      LSXIJ   = LSQZ    + L2S     !   "      Z-    "
      LSYIJ   = LSXIJ   + L2S     ! R MATRIX X-COMPNT (PART OF T MATRIX)
      LSZIJ   = LSYIJ   + L2S     !   "      Y-    "
      LFTXX   = LSZIJ   + L2S     !   "      Z-    "
      LFTXY   = LFTXX   + NAT     ! DIAMAG TERM OF SHIELDING TEN. (3.37)
      LFTXZ   = LFTXY   + NAT
      LFTYX   = LFTXZ   + NAT
      LFTYY   = LFTYX   + NAT
      LFTYZ   = LFTYY   + NAT
      LFTZX   = LFTYZ   + NAT
      LFTZY   = LFTZX   + NAT
      LFTZZ   = LFTZY   + NAT
      LSTXX   = LFTZZ   + NAT
      LSTXY   = LSTXX   + NAT     ! PARAMAG TERM OF SHIELDING TEN (3.37)
      LSTXZ   = LSTXY   + NAT
      LSTYX   = LSTXZ   + NAT
      LSTYY   = LSTYX   + NAT
      LSTYZ   = LSTYY   + NAT
      LSTZX   = LSTYZ   + NAT
      LSTZY   = LSTZX   + NAT
      LSTZZ   = LSTZY   + NAT
      LSSX    = LSTZZ   + NAT
      LSSY    = LSSX    + L2S     ! PERTURBED OVERLAP INTS        (3.48)
      LSSZ    = LSSY    + L2S
      LSS10X  = LSSZ    + L2S
      LSS10Y  = LSS10X  + L2S     ! FULL PERTURBED S MAT. S(1,0)  (3.48)
      LSS10Z  = LSS10Y  + L2S
      LS      = LSS10Z  + L2S
      LP      = LS      + L2      ! OVERLAP MATRIX
      LSP10X  = LP      + L2      ! DENSITY MATRIX
      LSP10Y  = LSP10X  + L2S     ! PERTURBED DENSITY MAT P(1,0)  (3.65)
      LSP10Z  = LSP10Y  + L2S
      LTEINT0 = LSP10Z  + L2S
      LTEWRK  = LTEINT0 + L4      ! TWO-ELECTRON INTEGRALS, UNPERTURBED
      LWRK    = LTEWRK  + L4      ! FULL PERTURBED TWO E- INT.    (3.52)
      LWRK2   = LWRK    + NUM     ! WORK ARRAY
      LWRK3   = LWRK2   + NUM     ! WORK ARRAY
      LWRK4   = LWRK3   + NUM     ! WORK ARRAY
      LSPS1PX = LWRK4   + NUM     ! WORK ARRAY
      LSPS1PY = LSPS1PX + L2S     ! P(0)*S(1,0)*P(0) - FIRST TERM (3.65)
      LSPS1PZ = LSPS1PY + L2S
      LSD     = LSPS1PZ + L2S
      LSTDX   = LSD     + L2S     ! KINETIC MATRIX                (3.51)
      LSTDY   = LSTDX   + L2S     ! T-TYPE KINETIC MATRIX         (3.50)
      LSTDZ   = LSTDY   + L2S
      LSV     = LSTDZ   + L2S
      LSTVX   = LSV     + L2S     ! NUCLEAR MATRIX                (3.51)
      LSTVY   = LSTVX   + L2S     ! T-TYPE NUCLEAR MATRIX         (3.50)
      LSTVZ   = LSTVY   + L2S
      LSELLX  = LSTVZ   + L2S
      LSELLY  = LSELLX  + L2S     ! ONE-ELECTRON L-LAMBDA INTS    (3.50)
      LSELLZ  = LSELLY  + L2S
      LSH10X  = LSELLZ  + L2S
      LSH10Y  = LSH10X  + L2S     ! H(1,0) MATRICES               (3.50)
      LSH10Z  = LSH10Y  + L2S
      LSF10X  = LSH10Z  + L2S
      LSF10Y  = LSF10X  + L2S     ! PERTURBED FOCK MATS. F(1,0)   (3.49)
      LSF10Z  = LSF10Y  + L2S
      LDUM1L2S= LSF10Z  + L2S
      LDUM2L2S= LDUM1L2S+ L2S     ! WORK ARRAY
      LDUM3L2S= LDUM2L2S+ L2S     ! WORK ARRAY
      LDUM    = LDUM3L2S+ L2S     ! WORK ARRAY
      LSP0G10X= LDUM    + L3      ! WORK ARRAY
      LSP0G10Y= LSP0G10X+ L2S     ! P(0)*G(1,0) IN F(1,0)         (3.49)
      LSP0G10Z= LSP0G10Y+ L2S
      LBUFP   = LSP0G10Z+ L2S
      LIX     = LBUFP   + NINTMX
      LAST    = LIX     + NINTMX
C
      END IF
C
      NEED = LAST - LOADFM - 1
      IF(MASWRK) WRITE(IW,9060) NEED
      CALL GETFM(NEED)
C
      IF(EXETYP.EQ.CHECK) GO TO 100
C
C     ----- Do exotic one-electron integrals using McMurchie-Davidson
C
      IF(MASWRK) WRITE(IW,9050)
C
C     Not a lot of symmetry with these integrals:
C
      CALL ONEINTS(X(LTINT),X(LTINTM),X(LDELIN),X(LDELINM),
     *             X(LTINTBK),X(LDELINBK),X(LR),X(LBR),POEINT)
C
C     Symmetry can be exploited here - much anti-symmetry:
C
      CALL SONEINTS(X(LSOESOI),X(LSXIJ),X(LSYIJ),X(LSZIJ),X(LSQX),
     *              X(LSQY),X(LSQZ),X(LSTDX),X(LSTDY),X(LSTDZ),
     *              X(LSTVX),X(LSTVY),X(LSTVZ),X(LSD),X(LSV),
     *              X(LSELLX),X(LSELLY),X(LSELLZ),
     *              X(LSSX),X(LSSY),X(LSSZ),
     *              X(LDELINBK),X(LR),X(LBR),
     *              L2S,POEINT)
C
      CALL TIMIT(1)
C
C     ----- Do exotic two-electron integrals using McMurchie-Davidson
C
      IF(ANGINT) THEN
        IF(INMEM) THEN
          IF(MASWRK) WRITE(IW,9041)
          CALL TWOINTSA(X(LTEINT0),X(LTEINT1),X(LTEINT2),X(LTEINT3),
     *                  X(LTEINT4),X(LTEINT5),X(LTEINT6),PTEINT,TEDBG,
     *                  X(LP),L2,X(LBUFP),X(LIX),NINTMX,
     *                  X(LAIN),X(LX),X(LR))
        ELSE
          IF(MASWRK) WRITE(IW,9042)
          CALL TWOINTS(X(LSP0G10X),X(LSP0G10Y),X(LSP0G10Z),X(LP),
     *                 X(LSXIJ),X(LSYIJ),X(LSZIJ),X(LSQX),X(LSQY),
     *                 X(LSQZ),L2,L2S,PTEINT,TEDBG,
     *                 X(LBUFP),X(LIX),NINTMX,X(LAIN),X(LX),X(LR))
        END IF
      ELSE
          IF(MASWRK) WRITE(IW,9043)
          CALL TWOINTSM(X(LTEINT0),X(LTEWRK),X(LBUFP),X(LIX),PTEINT,
     *                  TEDBG,X(LAIN),X(LX),X(LR))
      END IF
C
      CALL TIMIT(1)
C
C     ----- Assemble the three H(0,1)a matrices for each nucleus (3.38)
C
      CALL H01(X(LSOESOI),X(LSH01X),X(LSH01Y),X(LSH01Z),L2S)
C
C     ----- Assemble the nine H(1,1)ab matrices for each nucleus (3.39)
C
      CALL H11(X(LDELINM),X(LTINTM),X(LSH01X),X(LSH01Y),X(LSH01Z),
     *         X(LSQX),X(LSQY),X(LSQZ),X(LSXIJ),X(LSYIJ),X(LSZIJ),
     *         X(LH11XX),X(LH11XY),X(LH11XZ),
     *         X(LH11YX),X(LH11YY),X(LH11YZ),
     *         X(LH11ZX),X(LH11ZY),X(LH11ZZ),L2S)
C
C     ----- Compute "diamagnetic" term:  P(0) times H(1,1) (3.37)
C
      CALL DIAMAG(X(LH11XX),X(LH11XY),X(LH11XZ),
     *            X(LH11YX),X(LH11YY),X(LH11YZ),
     *            X(LH11ZX),X(LH11ZY),X(LH11ZZ),
     *            X(LFTXX),X(LFTXY),X(LFTXZ),
     *            X(LFTYX),X(LFTYY),X(LFTYZ),
     *            X(LFTZX),X(LFTZY),X(LFTZZ),X(LP),PRMAT,PDIA)
C
C     ----- Assemble the three S(1,0) matrices (3.48)
C
      CALL S10(X(LSS10X),X(LSS10Y),X(LSS10Z),X(LSSX),X(LSSY),X(LSSZ),
     *            X(LSQX),X(LSQY),X(LSQZ),X(LSXIJ),X(LSYIJ),X(LSZIJ),
     *            X(LSPS1PX),X(LSPS1PY),X(LSPS1PZ),
     *            X(LS),X(LP),X(LDUM),L2S)
C
C     ----- Assemble the three H(1,0) matrices (3.50)
C
      CALL H10(X(LSQX),X(LSQY),X(LSQZ),X(LSXIJ),X(LSYIJ),X(LSZIJ),
     *         X(LSD),X(LSTDX),X(LSTDY),X(LSTDZ),
     *         X(LSV),X(LSTVX),X(LSTVY),X(LSTVZ),
     *         X(LSELLX),X(LSELLY),X(LSELLZ),
     *         X(LSH10X),X(LSH10Y),X(LSH10Z),L2S)
C
C     ----- Assemble three G(1,0), F(1,0), and P(1,0) matrices (3.49,65)
C
C     X direction
C
      IF(ANGINT) THEN
       IF(INMEM) THEN
         CALL G10(X(LTEINT0),X(LTEINT3),X(LTEINT2),X(LTEINT6),
     *             X(LTEINT5),X(LTEWRK),X(LSQX),
     *             X(LSYIJ),X(LSZIJ),X(LP),X(LSP0G10X),L2S,1)
         CALL P10ITER(X(LSP0G10X),X(LDUM1L2S),X(LSH10X),X(LSF10X),
     *             X(LTEINT0),X(LSS10X),X(LDUM3L2S),X(LDUM),X(LSPS1PX),
     *             X(LWRK),X(LWRK2),X(LWRK3),X(LWRK4),X(LSP10X),
     *             X(LDUM2L2S),1,PITER,L2S,X(LBUFP),X(LIX),NINTMX)
       ELSE
       CALL P10ITE(X(LSP0G10X),X(LDUM1L2S),X(LSH10X),X(LSF10X),
     *             X(LSS10X),X(LDUM3L2S),X(LDUM),X(LSPS1PX),
     *             X(LWRK),X(LWRK2),X(LWRK3),X(LWRK4),X(LSP10X),
     *             X(LDUM2L2S),1,PITER,L2S,X(LBUFP),X(LIX),NINTMX)
       END IF
      ELSE
       CALL G10DISC(X(LTEWRK),X(LTEINT0),X(LSQX),X(LSYIJ),X(LSZIJ),
     *             X(LP),X(LSP0G10X),L2S,1,X(LBUFP),X(LIX),NINTMX)
       CALL P10ITER(X(LSP0G10X),X(LDUM1L2S),X(LSH10X),X(LSF10X),
     *             X(LTEINT0),X(LSS10X),X(LDUM3L2S),X(LDUM),X(LSPS1PX),
     *             X(LWRK),X(LWRK2),X(LWRK3),X(LWRK4),X(LSP10X),
     *             X(LDUM2L2S),1,PITER,L2S,X(LBUFP),X(LIX),NINTMX)
      END IF
C
C     Y direction
C
      IF(ANGINT) THEN
       IF(INMEM) THEN
         CALL G10(X(LTEINT0),X(LTEINT1),X(LTEINT3),X(LTEINT4),
     *             X(LTEINT6),X(LTEWRK),X(LSQY),
     *             X(LSZIJ),X(LSXIJ),X(LP),X(LSP0G10Y),L2S,2)
         CALL P10ITER(X(LSP0G10Y),X(LDUM1L2S),X(LSH10Y),X(LSF10Y),
     *             X(LTEINT0),X(LSS10Y),X(LDUM3L2S),X(LDUM),X(LSPS1PY),
     *             X(LWRK),X(LWRK2),X(LWRK3),X(LWRK4),X(LSP10Y),
     *             X(LDUM2L2S),2,PITER,L2S,X(LBUFP),X(LIX),NINTMX)
       ELSE
         CALL P10ITE(X(LSP0G10Y),X(LDUM1L2S),X(LSH10Y),X(LSF10Y),
     *             X(LSS10Y),X(LDUM3L2S),X(LDUM),X(LSPS1PY),
     *             X(LWRK),X(LWRK2),X(LWRK3),X(LWRK4),X(LSP10Y),
     *             X(LDUM2L2S),2,PITER,L2S,X(LBUFP),X(LIX),NINTMX)
       END IF
      ELSE
       CALL G10DISC(X(LTEWRK),X(LTEINT0),X(LSQY),X(LSZIJ),X(LSXIJ),
     *             X(LP),X(LSP0G10Y),L2S,2,X(LBUFP),X(LIX),NINTMX)
       CALL P10ITER(X(LSP0G10Y),X(LDUM1L2S),X(LSH10Y),X(LSF10Y),
     *             X(LTEINT0),X(LSS10Y),X(LDUM3L2S),X(LDUM),X(LSPS1PY),
     *             X(LWRK),X(LWRK2),X(LWRK3),X(LWRK4),X(LSP10Y),
     *             X(LDUM2L2S),2,PITER,L2S,X(LBUFP),X(LIX),NINTMX)
      END IF
C
C     Z direction
C
      IF(ANGINT) THEN
       IF(INMEM) THEN
         CALL G10(X(LTEINT0),X(LTEINT2),X(LTEINT1),X(LTEINT5),
     *             X(LTEINT4),X(LTEWRK),X(LSQZ),
     *             X(LSXIJ),X(LSYIJ),X(LP),X(LSP0G10Z),L2S,3)
         CALL P10ITER(X(LSP0G10Z),X(LDUM1L2S),X(LSH10Z),X(LSF10Z),
     *             X(LTEINT0),X(LSS10Z),X(LDUM3L2S),X(LDUM),X(LSPS1PZ),
     *             X(LWRK),X(LWRK2),X(LWRK3),X(LWRK4),X(LSP10Z),
     *             X(LDUM2L2S),3,PITER,L2S,X(LBUFP),X(LIX),NINTMX)
       ELSE
         CALL P10ITE(X(LSP0G10Z),X(LDUM1L2S),X(LSH10Z),X(LSF10Z),
     *             X(LSS10Z),X(LDUM3L2S),X(LDUM),X(LSPS1PZ),
     *             X(LWRK),X(LWRK2),X(LWRK3),X(LWRK4),X(LSP10Z),
     *             X(LDUM2L2S),3,PITER,L2S,X(LBUFP),X(LIX),NINTMX)
       END IF
      ELSE
       CALL G10DISC(X(LTEWRK),X(LTEINT0),X(LSQZ),X(LSXIJ),X(LSYIJ),
     *             X(LP),X(LSP0G10Z),L2S,3,X(LBUFP),X(LIX),NINTMX)
       CALL P10ITER(X(LSP0G10Z),X(LDUM1L2S),X(LSH10Z),X(LSF10Z),
     *             X(LTEINT0),X(LSS10Z),X(LDUM3L2S),X(LDUM),X(LSPS1PZ),
     *             X(LWRK),X(LWRK2),X(LWRK3),X(LWRK4),X(LSP10Z),
     *             X(LDUM2L2S),3,PITER,L2S,X(LBUFP),X(LIX),NINTMX)
      END IF
C
C
C     ----- Compute "paramagnetic" term:  P(1,0) times H(0,1) (3.37)
C
      CALL PARAMAG(X(LSH01X),X(LSH01Y),X(LSH01Z),
     *             X(LSP10X),X(LSP10Y),X(LSP10Z),
     *             X(LSTXX),X(LSTXY),X(LSTXZ),
     *             X(LSTYX),X(LSTYY),X(LSTYZ),
     *             X(LSTZX),X(LSTZY),X(LSTZZ),PRMAT,PPARA,L2S)
C
C     ----- Add the two terms - Print tensor for each nucleus (3.37)
C
      CALL SHFTTEN(X(LFTXX),X(LFTXY),X(LFTXZ),
     *             X(LFTYX),X(LFTYY),X(LFTYZ),
     *             X(LFTZX),X(LFTZY),X(LFTZZ),
     *             X(LSTXX),X(LSTXY),X(LSTXZ),
     *             X(LSTYX),X(LSTYY),X(LSTYZ),
     *             X(LSTZX),X(LSTZY),X(LSTZZ),PEVEC)
C
C     Turn on symmetry, return memory, and exit
C
  100 CONTINUE
      CALL SYMON
      CALL RETFM(NEED)
      IF(MASWRK) WRITE(IW,9080)
      CALL TIMIT(1)
      RETURN
C
 9000 FORMAT('ERROR IN $NMR NAMELIST INPUT - STOP')
 9010 FORMAT(/5X,20("-")/5X,'NMR INPUT PARAMETERS'/5X,20("-")/
     *        5X,' INMEM=',L2,'  ANGINT=',L2/
     *        5X,'  PDIA=',L2,'   PPARA=',L2,'   PEVEC=',L2/
     *        5X,'POEINT=',L2,'  PTEINT=',L2,'   PRMAT=',L2,
     *          '   PITER=',L2)
 9030 FORMAT(/1X,'REPEATING AO INTEGRAL EVALUATION WITHOUT POINT',
     *           ' GROUP SYMMETRY')
 9041 FORMAT(/1X,'CALCULATING GIAO TWO-ELECTRON INTEGRALS USING ',
     *  'MCMURCHIE-DAVIDSON METHOD'/
     *  1X,'CHOOSING IN MEMORY OPTION USING TWO INTEGRAL PASSES...')
 9042 FORMAT(/1X,'CALCULATING GIAO TWO-ELECTRON INTEGRALS USING ',
     *  'MCMURCHIE-DAVIDSON METHOD'/
     *  1X,'CHOOSING OUT OF MEMORY OPTION USING TWO INTEGRAL PASSES...')
 9043 FORMAT(/1X,'CALCULATING GIAO TWO-ELECTRON INTEGRALS USING ',
     *  'MCMURCHIE-DAVIDSON METHOD'/
     *  1X,'CHOOSING SIX INTEGRAL PASSES...')
 9050 FORMAT(/1X,'CALCULATING GIAO ONE-ELECTRON INTEGRALS USING ',
     *  'MCMURCHIE-DAVIDSON METHOD...')
 9060 FORMAT(' NMR CHEMICAL SHIFT CALCULATION REQUIRES',I20,
     *       ' WORDS OF MEMORY.')
 9080 FORMAT(1X,'..... DONE WITH NMR SHIELDINGS .....')
 9100 FORMAT(/1X,'*** INCOMPATIBLE OPTION CHOSEN WITH RUNTYP=NMR ***'/
     *       1X,'NMR MAY BE COMPUTED ONLY FOR SCFTYP=RHF,'/
     *       1X,'NO CORRELATION OPTION (DFTTYP, CITYP, CCTYP, MPLEVL)',
     *          ' MAY BE CHOSEN'/
     *       1X,'NO SEMI-EMPIRICAL OPTION (GBASIS=AM1/PM3/MNDO)',
     *          ' MAY BE CHOSEN'/
     *       1X,'DIRECT AO INTEGRAL CALCULATION (DIRSCF) IS NOT',
     *          ' ENABLED,'/
     *       1X,'AND/OR PARALLEL EXECUTION IS NOT ENABLED.')
 9999 FORMAT(/7X,24('-'),3(' '),30('-'),
     *       /7X,'RHF GIAO CHEMICAL SHIFTS',
     *       '   PROGRAM WRITTEN BY M.A.FREITAG',
     *       /7X,24('-'),3(' '),30('-'),/)
      END
C
C*MODULE NMR  *DECK GETKLM
      SUBROUTINE GETKLM(IFUN,K,L,M)
C
C     This routine returns the exponents k, l, and m of
C     a primitive function (ifun) of the form:
C
C          k  l  m   -a r^2
C         x  y  z   e
C
      DIMENSION KX(35),LY(35),MZ(35)
C
      DATA KX /0,1,0,0,2,0,0,1,1,0,3,0,0,2,2,1,0,1,0,1,
     *         4,0,0,3,3,1,0,1,0,2,2,0,1,1,1/
      DATA LY /0,0,1,0,0,2,0,1,0,1,0,3,0,1,0,2,2,0,1,1,
     *         0,4,0,1,0,3,3,0,1,2,0,2,1,2,1/
      DATA MZ /0,0,0,1,0,0,2,0,1,1,0,0,3,0,1,0,1,2,2,1,
     *         0,0,4,0,1,0,1,3,3,0,2,2,1,1,2/
C
      K = KX(IFUN)
      L = LY(IFUN)
      M = MZ(IFUN)
C
      RETURN
      END
C*MODULE NMR  *DECK ONEINTS
      SUBROUTINE ONEINTS(TINT,TINTM,DELIN,DELINM,TINTBK,DELINBK,
     *                   R,BR,POEINT)
C
C     This does all one e- integrals that don't have much for symmetry.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL IANDJ, DOUBLE, NORM, POEINT
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      PARAMETER (ZERO=0.0D+00, TWO=2.0D+00,SQRT3=1.73205080756888D+00,
     *           SQRT5=2.23606797749979D+00, SQRT7=2.64575131106459D+00,
     *           PI=3.1415926535897932D+00)
C
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /SYMIND/ TOL,II,JJ,LIT,LJT,MINI,MINJ,MAXI,MAXJ,IANDJ
C
      DIMENSION TINT(NUM*NUM,NAT,3,3,3),TINTM(NUM,NUM,NAT,3,3,3),
     *          DELIN(NUM*NUM,NAT,3,3),DELINM(NUM,NUM,NAT,3,3),
     *          TINTBK(225,125,3,3,3),DELINBK(225,125,3,3),
     *          R(0:24,0:24,0:24,0:24),BR(0:24,0:24,0:24,0:24)
      DIMENSION ES(3),E(1:3,0:12,0:6,-2:6),FOFT(0:24),DIJ(225)
C
      NORM=NORMP.NE.1 .OR. NORMF.NE.1
C
C     Clean out the recursion array E
C
      DO 11 L=1,3
       DO 21 M=0,8
        DO 31 N=0,4
         DO 41 I=0,4
          E(N,L,M,I)=ZERO
 41      CONTINUE
 31     CONTINUE
 21    CONTINUE
 11   CONTINUE
C
C
C     ----- I SHELL
C
      DO 720 II = 1,NSHELL
C
         I = KATOM(II)
         XI = C(1,I)
         YI = C(2,I)
         ZI = C(3,I)
         I1 = KSTART(II)
         I2 = I1+KNG(II)-1
         MINI = KMIN(II)
         MAXI = KMAX(II)
         LIT = KTYPE(II)
         LOCI = KLOC(II)-MINI
C
C     ----- J SHELL
C
         DO 700 JJ = 1,NSHELL
C
            J = KATOM(JJ)
            XJ = C(1,J)
            YJ = C(2,J)
            ZJ = C(3,J)
            J1 = KSTART(JJ)
            J2 = J1+KNG(JJ)-1
            MINJ = KMIN(JJ)
            MAXJ = KMAX(JJ)
            LJT = KTYPE(JJ)
            LOCJ = KLOC(JJ)-MINJ
C
            IANDJ = II .EQ. JJ
C
            RX= XI-XJ
            RY= YI-YJ
            RZ= ZI-ZJ
            RX2= RX*RX
            RY2= RY*RY
            RZ2= RZ*RZ
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
            IJ = 0
            MAX = MAXJ
            DO 160 I = MINI,MAXI
               DO 140 J = MINJ,MAX
                  IJ = IJ+1
  140          CONTINUE
  160       CONTINUE
C
C           Clean out integral arrays for this set of primitives
C
            DO 12 ICLN=1,3
             DO 22 JCLN=1,3
              DO 32 KCLN=1,NAT
               DO 42 LCLN=1,IJ
                DELINBK(LCLN,KCLN,JCLN,ICLN)=ZERO
                 DO 52 MCLN=1,3
                  TINTBK(LCLN,KCLN,ICLN,JCLN,MCLN)=ZERO
 52              CONTINUE
 42            CONTINUE
 32           CONTINUE
 22          CONTINUE
 12         CONTINUE
C
C     ----- I PRIMITIVE
C
            JGMAX = J2
            DO 520 IG = I1,I2
C
               AI = EX(IG)
               CSI = CS(IG)
               CPI = CP(IG)
               CDI = CD(IG)
               CFI = CF(IG)
               CGI = CG(IG)
C
C     ----- J PRIMITIVE
C
               DO 500 JG = J1,JGMAX
C
                  AJ = EX(JG)
                  CSJ = CS(JG)
                  CPJ = CP(JG)
                  CDJ = CD(JG)
                  CFJ = CF(JG)
                  CGJ = CG(JG)
C
C         Coefficients depend on this particular alpha(i) and alpha(j):
C         Return e([x,y,z],ip,ia,ib) (The recurrance formulas.)
C
                  CALL EAB(ES,AI,AJ,RX2,RY2,RZ2)
C
                  CALL RECURS(ES,AI,AJ,RX,RY,RZ,LIT+1,LJT+1,E)
C
C         Compute primitive coefficients: (no doubling... no symmetry)
C
                  DOUBLE=.FALSE.
                  MAX = MAXJ
                  NN = 0
                  DUM1 = ZERO
                  DUM2 = ZERO
                  DO 220 III = MINI,MAXI
C
                     IF (III.EQ.1) DUM1=CSI
                     IF (III.EQ.2) DUM1=CPI
                     IF (III.EQ.5) DUM1=CDI
                     IF ((III.EQ. 8).AND.NORM) DUM1=DUM1*SQRT3
                     IF (III.EQ.11) DUM1=CFI
                     IF ((III.EQ.14).AND.NORM) DUM1=DUM1*SQRT5
                     IF ((III.EQ.20).AND.NORM) DUM1=DUM1*SQRT3
                     IF (III.EQ.21) DUM1=CGI
                     IF ((III.EQ.24).AND.NORM) DUM1=DUM1*SQRT7
                     IF ((III.EQ.30).AND.NORM) DUM1=DUM1*SQRT5/SQRT3
                     IF ((III.EQ.33).AND.NORM) DUM1=DUM1*SQRT3
C
                     DO 200 JJJ = MINJ,MAX
C
                        IF (JJJ.EQ.1) THEN
                           DUM2=DUM1*CSJ
                           IF (DOUBLE) THEN
                              IF (III.LE.1) THEN
                                 DUM2=DUM2+DUM2
                              ELSE
                                 DUM2=DUM2+CSI*CPJ
                              END IF
                           END IF
                        ELSE IF (JJJ.EQ.2) THEN
                           DUM2=DUM1*CPJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF (JJJ.EQ.5) THEN
                           DUM2=DUM1*CDJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((JJJ.EQ.8).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        ELSE IF (JJJ.EQ.11) THEN
                           DUM2=DUM1*CFJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((JJJ.EQ.14).AND.NORM) THEN
                           DUM2=DUM2*SQRT5
                        ELSE IF ((JJJ.EQ.20).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        ELSE IF (JJJ.EQ.21) THEN
                           DUM2=DUM1*CGJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((JJJ.EQ.24).AND.NORM) THEN
                           DUM2=DUM2*SQRT7
                        ELSE IF ((JJJ.EQ.30).AND.NORM) THEN
                           DUM2=DUM2*SQRT5/SQRT3
                        ELSE IF ((JJJ.EQ.33).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        END IF
                        NN = NN+1
                        DIJ(NN) = DUM2
C
  200                CONTINUE
  220             CONTINUE
C
C     ----- Some integrals involve the nuclear positions:
C
                  AP = AI + AJ
C
                  TPOAP = TWO*PI/AP
C
                  MXLIJT = LIT + LJT
C
                  DO 460 IC = 1,NAT
C
                    CX = C(1,IC)
                    CY = C(2,IC)
                    CZ = C(3,IC)
C
                    CAX = CX - XI
                    CAY = CY - YI
                    CAZ = CZ - ZI
C
                    CBX = CX - XJ
                    CBY = CY - YJ
                    CBZ = CZ - ZJ
C
                    CPX = -(AI*CAX + AJ*CBX)/AP
                    CPY = -(AI*CAY + AJ*CBY)/AP
                    CPZ = -(AI*CAZ + AJ*CBZ)/AP
C
C                   Compute McMurchie-Davidson stuff [c.f. (3.93,100)]
C
                    T = AP * (CPX**2 + CPY**2 + CPZ**2)
C
                    CALL FOFTJ(MXLIJT,T,FOFT)
C
                    CALL RNLM(MXLIJT,CPX,CPY,CPZ,AP,FOFT,R)
C
                    NN=0
                    MAX=MAXJ
                    DO 450 III=MINI,MAXI
                      CALL GETKLM(III,IA,JA,KA)
                      DO 440 JJJ=MINJ,MAX
                        NN=NN+1
                        CALL GETKLM(JJJ,IB,JB,KB)
C
                        FAC2 = -DIJ(NN)*TPOAP
C
C     ---- Do specific integral calculations ----
C
C
      DO IN=1,3
      DO ID=1,3
C
        CALL DELINTS(DELINBK(NN,IC,ID,IN),IA,JA,KA,IB,JB,KB,FAC2,
     *               ID,IN,E,R,BR)
        DO IAN=1,3
          IF(IN.NE.ID)
     *      CALL TINTS(TINTBK(NN,IC,IAN,IN,ID),IA,JA,KA,IB,JB,KB,AJ,
     *                 FAC2,ID,IN,IAN,E,R,BR)
        ENDDO
C
      ENDDO
      ENDDO
C
C     ---- End specific integral calculations ----
C
  440                 CONTINUE
  450               CONTINUE
C
  460             CONTINUE
C
C     ----- END OF PRIMITIVE LOOPS -----
C
  500          CONTINUE
  520       CONTINUE
C
C     ----- COPY BLOCK INTO OVERLAP MATRIX
C
            MAX = MAXJ
            NN = 0
            DO 620 I = MINI,MAXI
               LI = LOCI+I
               IN = NUM*(LI-1)
               DO 600 J = MINJ,MAX
                  JN = IN+LOCJ+J
                  NN = NN+1
                  DO ID=1,3
                  DO INU=1,3
                  DO IC=1,NAT
                  DELIN(JN,IC,ID,INU) = DELINBK(NN,IC,ID,INU)
                  DO IAN=1,3
                  IF(INU.NE.ID) THEN
                    TINT(JN,IC,IAN,INU,ID) = TINTBK(NN,IC,IAN,INU,ID)
                  END IF
                  ENDDO
                  ENDDO
                  ENDDO
                  ENDDO
  600          CONTINUE
  620       CONTINUE
  700    CONTINUE
  720 CONTINUE
C
      DO 621 I=1,NUM
       DO 622 J=1,NUM
        DO ID=1,3
        DO INU=1,3
        DO IC=1,NAT
        DELINM(J,I,IC,ID,INU) = DELIN((I-1)*NUM+J,IC,ID,INU)
        DO IAN=1,3
        IF(INU.NE.ID) THEN
           TINTM(J,I,IC,IAN,INU,ID) = TINT((I-1)*NUM+J,IC,IAN,INU,ID)
        END IF
        ENDDO
        ENDDO
        ENDDO
        ENDDO
  622  CONTINUE
  621 CONTINUE
C
      IF (POEINT) THEN
      DO IC=1,NAT
      DO INU=1,3
      DO ID=1,3
       WRITE(IW,9980) ID,INU,IC
       CALL OEPRSQL(DELINM,NUM,NUM,NUM,NAT,IC,ID,INU)
        DO IAN=1,3
        IF(INU.NE.ID) THEN
          WRITE(IW,9970) IAN,INU,ID,IC
          CALL TIPRSQL(TINTM,NUM,NUM,NUM,NAT,IC,IAN,INU,ID)
        END IF
        ENDDO
      ENDDO
      ENDDO
      ENDDO
      END IF
C
      RETURN
 9970 FORMAT(/5X,'TINT(',I2,I2,I2,' ) INTEGRAL MATRIX FOR NUCLEUS',
     *        I2,':')
 9980 FORMAT(/5X,'DELIN(',I2,I2,' ) INTEGRAL MATRIX FOR NUCLEUS',I2,':')
      END
C*MODULE NMR  *DECK SONEINTS
      SUBROUTINE SONEINTS(SOESOI,SXIJ,SYIJ,SZIJ,SQX,SQY,
     *                    SQZ,STDX,STDY,STDZ,STVX,STVY,STVZ,SD,
     *                    SV,SELLX,SELLY,SELLZ,SSX,SSY,SSZ,
     *                    SOESOIBK,R,BR,L2S,POEINT)
C
C     This does all one e- integrals that have some (anti)symmetry.
C     See "oneints" for more comments... this is pretty much the same.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL IANDJ, DOUBLE, NORM, POEINT
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      PARAMETER (ZERO=0.0D+00, TWO=2.0D+00,SQRT3=1.73205080756888D+00,
     *           SQRT5=2.23606797749979D+00, SQRT7=2.64575131106459D+00,
     *           PI=3.1415926535897932D+00)
C
      DIMENSION SOESOI(L2S,NAT,3,3),SXIJ(L2S),SYIJ(L2S),SZIJ(L2S),
     *          SQX(L2S),SQY(L2S),SQZ(L2S),
     *          STDX(L2S),STDY(L2S),STDZ(L2S),
     *          STVX(L2S),STVY(L2S),STVZ(L2S),SD(L2S),SV(L2S),
     *          SELLX(L2S),SELLY(L2S),SELLZ(L2S),
     *          SSX(L2S),SSY(L2S),SSZ(L2S),
     *          SOESOIBK(225,125,3,3),
     *          R(0:24,0:24,0:24,0:24),BR(0:24,0:24,0:24,0:24)
      DIMENSION ES(3),E(1:3,0:12,0:6,-2:6),
     *          FOFT(0:24),DIJ(225),SDBK(225),SVBK(225),
     *          SXIJBK(225),SYIJBK(225),SZIJBK(225),
     *          SQXBK(225),SQYBK(225),SQZBK(225),
     *          STDXBK(225),STDYBK(225),STDZBK(225),
     *          STVXBK(225),STVYBK(225),STVZBK(225),
     *          SELLXBK(225),SELLYBK(225),SELLZBK(225),
     *          SSXBK(225),SSYBK(225),SSZBK(225)
C
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /SYMIND/ TOL,II,JJ,LIT,LJT,MINI,MINJ,MAXI,MAXJ,IANDJ
C
      NORM=NORMP.NE.1 .OR. NORMF.NE.1
C
C     Clean out the recursion array E
C
      DO 11 L=1,3
       DO 21 M=0,8
        DO 31 N=0,4
         DO 41 I=0,4
          E(N,L,M,I)=ZERO
 41      CONTINUE
 31     CONTINUE
 21    CONTINUE
 11   CONTINUE
C
C     ----- I SHELL
C
      DO 720 II = 1,NSHELL
C
         I = KATOM(II)
         XI = C(1,I)
         YI = C(2,I)
         ZI = C(3,I)
         I1 = KSTART(II)
         I2 = I1+KNG(II)-1
         MINI = KMIN(II)
         MAXI = KMAX(II)
         LIT = KTYPE(II)
         LOCI = KLOC(II)-MINI
C
C     ----- J SHELL
C
         DO 700 JJ = 1,II
C
            J = KATOM(JJ)
            XJ = C(1,J)
            YJ = C(2,J)
            ZJ = C(3,J)
            J1 = KSTART(JJ)
            J2 = J1+KNG(JJ)-1
            MINJ = KMIN(JJ)
            MAXJ = KMAX(JJ)
            LJT = KTYPE(JJ)
            LOCJ = KLOC(JJ)-MINJ
C
            IANDJ = II .EQ. JJ
C
            RX= XI-XJ
            RY= YI-YJ
            RZ= ZI-ZJ
            RX2= RX*RX
            RY2= RY*RY
            RZ2= RZ*RZ
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
            IJ = 0
            MAX = MAXJ
            DO 160 I = MINI,MAXI
               IF(IANDJ) MAX=I
               DO 140 J = MINJ,MAX
                  IF(I.EQ.J.AND.LOCI.EQ.LOCJ) GO TO 140
                  IJ = IJ+1
C
C     The next few lines calculate the nuclear prefactor Q_nu,lambda
C     (x,y,z) and X_nu,lambda, Y_nu,lambda, and Z_nu,lambda, which is
C     part of the T matrix.  See the definition on p. 52, MAF thesis.
C
                  SQXBK(IJ) = YI*ZJ - ZI*YJ
                  SQYBK(IJ) = ZI*XJ - XI*ZJ
                  SQZBK(IJ) = XI*YJ - YI*XJ
C
                  SXIJBK(IJ)= XI - XJ
                  SYIJBK(IJ)= YI - YJ
                  SZIJBK(IJ)= ZI - ZJ
C
  140          CONTINUE
  160       CONTINUE
C
            CALL VCLR(STDXBK,1,IJ)
            CALL VCLR(STDYBK,1,IJ)
            CALL VCLR(STDZBK,1,IJ)
            CALL VCLR(STVXBK,1,IJ)
            CALL VCLR(STVYBK,1,IJ)
            CALL VCLR(STVZBK,1,IJ)
            CALL VCLR(SDBK,1,IJ)
            CALL VCLR(SVBK,1,IJ)
            CALL VCLR(SELLXBK,1,IJ)
            CALL VCLR(SELLYBK,1,IJ)
            CALL VCLR(SELLZBK,1,IJ)
            CALL VCLR(SSXBK,1,IJ)
            CALL VCLR(SSYBK,1,IJ)
            CALL VCLR(SSZBK,1,IJ)
C
            DO 12 ICLN=1,3
             DO 22 JCLN=1,3
              DO 32 KCLN=1,NAT
               DO 42 LCLN=1,IJ
                SOESOIBK(LCLN,KCLN,JCLN,ICLN)=ZERO
 42            CONTINUE
 32           CONTINUE
 22          CONTINUE
 12         CONTINUE
C
C     ----- I PRIMITIVE
C
            JGMAX = J2
            DO 520 IG = I1,I2
C
               AI = EX(IG)
               CSI = CS(IG)
               CPI = CP(IG)
               CDI = CD(IG)
               CFI = CF(IG)
               CGI = CG(IG)
C
C     ----- J PRIMITIVE
C
C           Integrals are exotic enough that you can't get away with
C           this short-cut.  Also means you have to turn off "double."
C              IF (IANDJ) JGMAX = IG
C
               DO 500 JG = J1,JGMAX
C
                  AJ = EX(JG)
                  CSJ = CS(JG)
                  CPJ = CP(JG)
                  CDJ = CD(JG)
                  CFJ = CF(JG)
                  CGJ = CG(JG)
C
C         Coefficients depend on this particular alpha(i) and alpha(j):
C         Return e([x,y,z],ip,ia,ib) (The recurrance formulas.)
C
                  FAC = SQRT((PI/(AI+AJ))*(PI/(AI+AJ))*(PI/(AI+AJ)))
C
                  CALL EAB(ES,AI,AJ,RX2,RY2,RZ2)
C
                  CALL RECURS(ES,AI,AJ,RX,RY,RZ,LIT+1,LJT+1,E)
C
C         Compute primitive coefficients:
C
                  DOUBLE=.FALSE.
                  MAX = MAXJ
                  NN = 0
                  DUM1 = ZERO
                  DUM2 = ZERO
                  DO 220 III = MINI,MAXI
C
                     IF (III.EQ.1) DUM1=CSI
                     IF (III.EQ.2) DUM1=CPI
                     IF (III.EQ.5) DUM1=CDI
                     IF ((III.EQ. 8).AND.NORM) DUM1=DUM1*SQRT3
                     IF (III.EQ.11) DUM1=CFI
                     IF ((III.EQ.14).AND.NORM) DUM1=DUM1*SQRT5
                     IF ((III.EQ.20).AND.NORM) DUM1=DUM1*SQRT3
                     IF (III.EQ.21) DUM1=CGI
                     IF ((III.EQ.24).AND.NORM) DUM1=DUM1*SQRT7
                     IF ((III.EQ.30).AND.NORM) DUM1=DUM1*SQRT5/SQRT3
                     IF ((III.EQ.33).AND.NORM) DUM1=DUM1*SQRT3
C
                     IF (IANDJ) MAX = III
C
                     CALL GETKLM(III,IA,JA,KA)
C
                     DO 200 JJJ = MINJ,MAX
C
                     IF(III.EQ.JJJ.AND.LOCI.EQ.LOCJ) GO TO 220
C
                        IF (JJJ.EQ.1) THEN
                           DUM2=DUM1*CSJ
                           IF (DOUBLE) THEN
                              IF (III.LE.1) THEN
                                 DUM2=DUM2+DUM2
                              ELSE
                                 DUM2=DUM2+CSI*CPJ
                              END IF
                           END IF
                        ELSE IF (JJJ.EQ.2) THEN
                           DUM2=DUM1*CPJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF (JJJ.EQ.5) THEN
                           DUM2=DUM1*CDJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((JJJ.EQ.8).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        ELSE IF (JJJ.EQ.11) THEN
                           DUM2=DUM1*CFJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((JJJ.EQ.14).AND.NORM) THEN
                           DUM2=DUM2*SQRT5
                        ELSE IF ((JJJ.EQ.20).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        ELSE IF (JJJ.EQ.21) THEN
                           DUM2=DUM1*CGJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((JJJ.EQ.24).AND.NORM) THEN
                           DUM2=DUM2*SQRT7
                        ELSE IF ((JJJ.EQ.30).AND.NORM) THEN
                           DUM2=DUM2*SQRT5/SQRT3
                        ELSE IF ((JJJ.EQ.33).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        END IF
                        NN = NN+1
                        DIJ(NN) = DUM2
C
                        CALL GETKLM(JJJ,IB,JB,KB)
C
C     Some integrals come straight from recurrance formulas:
C
      CALL DOEINT(SDBK(NN),STDXBK(NN),STDYBK(NN),STDZBK(NN),
     *  SELLXBK(NN),SELLYBK(NN),SELLZBK(NN),SSXBK(NN),SSYBK(NN),
     *  SSZBK(NN),DIJ(NN),FAC,E,AJ,IA,JA,KA,IB,JB,KB)
C
C
  200                CONTINUE
  220             CONTINUE
C
C     ----- Some integrals involve the nuclear positions:
C
                  AP = AI + AJ
C
                  TPOAP = TWO*PI/AP
C
                  MXLIJT = LIT + LJT
C
                  DO 460 IC = 1,NAT
C
                    ZNUC = -ZAN(IC)
                    CX = C(1,IC)
                    CY = C(2,IC)
                    CZ = C(3,IC)
C
                    CAX = CX - XI
                    CAY = CY - YI
                    CAZ = CZ - ZI
C
                    CBX = CX - XJ
                    CBY = CY - YJ
                    CBZ = CZ - ZJ
C
                    CPX = -(AI*CAX + AJ*CBX)/AP
                    CPY = -(AI*CAY + AJ*CBY)/AP
                    CPZ = -(AI*CAZ + AJ*CBZ)/AP
C
                    T = AP * (CPX**2 + CPY**2 + CPZ**2)
C
                    CALL FOFTJ(MXLIJT,T,FOFT)
C
                    CALL RNLM(MXLIJT,CPX,CPY,CPZ,AP,FOFT,R)
C
                    NN=0
                    MAX=MAXJ
                    DO 450 III=MINI,MAXI
                      IF(IANDJ) MAX=III
                      CALL GETKLM(III,IA,JA,KA)
                      DO 440 JJJ=MINJ,MAX
                     IF(III.EQ.JJJ.AND.LOCI.EQ.LOCJ) GO TO 440
                        NN=NN+1
                        CALL GETKLM(JJJ,IB,JB,KB)
C
                        FAC1 = DIJ(NN)*TPOAP*ZNUC
                        FAC2 = -DIJ(NN)*TPOAP
C
C     ---- Do specific integral calculations ----
C
      CALL NOEINT(SVBK(NN),STVXBK(NN),STVYBK(NN),STVZBK(NN),
     *            IA,JA,KA,IB,JB,KB,FAC1,E,R)
C
      DO ID=1,3
      DO IN=1,3
C
        IF(IN.NE.ID)
     *    CALL OESOINT(SOESOIBK(NN,IC,IN,ID),IA,JA,KA,IB,JB,KB,AJ,FAC2,
     *                 ID,IN,E,R,BR)
C
      ENDDO
      ENDDO
C
C     ---- End specific integral calculations ----
C
  440                 CONTINUE
  450               CONTINUE
C
  460             CONTINUE
C
C     ----- END OF PRIMITIVE LOOPS -----
C
  500          CONTINUE
  520       CONTINUE
C
C     ----- COPY BLOCK INTO OVERLAP MATRIX
C
            MAX = MAXJ
            NN = 0
            DO 620 I = MINI,MAXI
               LI = LOCI+I-1
               IN = (LI*(LI-1))/2
               IF (IANDJ) MAX = I
               DO 600 J = MINJ,MAX
                  IF(I.EQ.J.AND.LOCI.EQ.LOCJ) GO TO 600
                  JN = IN+LOCJ+J
                  NN = NN+1
                  SXIJ(JN)=SXIJBK(NN)
                  SYIJ(JN)=SYIJBK(NN)
                  SZIJ(JN)=SZIJBK(NN)
                  SQX(JN)=SQXBK(NN)
                  SQY(JN)=SQYBK(NN)
                  SQZ(JN)=SQZBK(NN)
                  STDX(JN)=STDXBK(NN)
                  STDY(JN)=STDYBK(NN)
                  STDZ(JN)=STDZBK(NN)
                  STVX(JN)=STVXBK(NN)
                  STVY(JN)=STVYBK(NN)
                  STVZ(JN)=STVZBK(NN)
                  SD(JN)=SDBK(NN)
                  SV(JN)=SVBK(NN)
                  SELLX(JN)=SELLXBK(NN)
                  SELLY(JN)=SELLYBK(NN)
                  SELLZ(JN)=SELLZBK(NN)
                  SSX(JN)=SSXBK(NN)
                  SSY(JN)=SSYBK(NN)
                  SSZ(JN)=SSZBK(NN)
                  DO ID=1,3
                  DO INU=1,3
                  DO IC=1,NAT
                  IF(INU.NE.ID) THEN
                    SOESOI(JN,IC,INU,ID) = SOESOIBK(NN,IC,INU,ID)
                  END IF
                  ENDDO
                  ENDDO
                  ENDDO
  600          CONTINUE
  620       CONTINUE
  700    CONTINUE
  720 CONTINUE
C
      IF(POEINT) THEN
      WRITE(IW,9990) 'X'
      CALL SPRTRIL(SXIJ,NUM)
      WRITE(IW,9990) 'Y'
      CALL SPRTRIL(SYIJ,NUM)
      WRITE(IW,9990) 'Z'
      CALL SPRTRIL(SZIJ,NUM)
      WRITE(IW,9999) 'X'
      CALL SPRTRIL(SQX,NUM)
      WRITE(IW,9999) 'Y'
      CALL SPRTRIL(SQY,NUM)
      WRITE(IW,9999) 'Z'
      CALL SPRTRIL(SQZ,NUM)
      DO IC=1,NAT
      DO INU=1,3
      DO ID=1,3
       WRITE(IW,9981) INU,ID,IC
       CALL SOEPRTRIL(SOESOI,NUM,NAT,IC,INU,ID)
      ENDDO
      ENDDO
      ENDDO
      END IF
C
      RETURN
 9981 FORMAT(/5X,'OESOI(',I2,I2,' ) INTEGRAL MATRIX FOR NUCLEUS',I2,':')
 9990 FORMAT(/5X,A1,'IJ MATRIX FOR ALL NUCLEI:')
 9999 FORMAT(/5X,'Q',A1,' MATRIX FOR ALL NUCLEI:')
      END
C
C*MODULE NMR  *DECK EAB
      SUBROUTINE EAB(ES,AI,AJ,RX2,RY2,RZ2)
C
C   Compute prefactor for use in RECURS.  MAF - idea from NWChem. (3.68)
C
C             /   a b     2 \
C   ESx = EXP| - -----  Rx   |
C             \  a + b      /
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION ES(3)
C
      AP=AI+AJ
      ALPHA=-(AI*AJ/AP)
C
      ES(1)=EXP(ALPHA*RX2)
      ES(2)=EXP(ALPHA*RY2)
      ES(3)=EXP(ALPHA*RZ2)
C
      RETURN
      END
C
C*MODULE NMR  *DECK RECURS
      SUBROUTINE RECURS(ES,AI,AJ,RX,RY,RZ,LA,LB,E)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION ES(3), PF(2), E(1:3,0:12,0:6,-2:6)
C
      PARAMETER (ZERO=0.0D+00)
C
C This routine was taken (and modified slightly) from NWChem.  No
C credit is taken for it in the NWChem code, but I think it's due
C to Ricky Kendall and an unnamed post-doc.  Modified by MAF. (3.76)
C
C Define the linear expansion coefficients for the product of two CGF in
C terms of HGF.
C
C   Recursion Formulas:
C
C      Ia+1,Ib              Ia,Ib              Ia,Ib             Ia,Ib
C     E           = (1/2p) E        - (b/p) R E        + (Ip+1) E
C      Ip                   Ip-1               Ip                Ip+1
C
C      Ia,Ib+1              Ia,Ib              Ia,Ib             Ia,Ib
C     E           = (1/2p) E        + (a/p) R E        + (Ip+1) E
C      Ip                   Ip-1               Ip                Ip+1
C
C   Initial Values:
C
C       0,0              0,0              0,0
C     Ex      = ESx,   Ey      = ESy,   Ez      = ESz,
C       0                0                0
C
C                                    /   a b     2 \
C      where typically   ESx =   EXP| - -----  Rx   |
C                                    \  a + b      /
C
C N.B. The prefactors for the overlap distribution of the two CGF
C are typically used to scale the expansion coefficients. Products of
C contraction coefficients may also be incorporated.
C
C   Indices for E(k,Ip,Ia,Ib):
C
C     1 [  k  ] 3         Cartesian Components {X,Y,Z}
C     0 [  Ip ] Ia+Ib     Angular Momentum Index for the HGF
C     0 [  Ia ] La        Angular Mom. Index for the CGF on Center "A"
C     0 [  Ib ] Lb        Angular Mom. Index for the CGF on Center "B"
C
C**********************************************************************
C
C
C Define the expansion coefficients for the products of two CGF.
C
C MWS: the J and K loops started at 1, causing floating point errors
C      on the first entry into TINTS on some machines.  The L loop
C      originally ran only -2 to -1, but was extended to the full
C      array of E since after many entries into TINTS, similar floating
C      point errors can occur.  No test was made to learn which part
C      of the range of L from 0 to 6 is truly necessary.
C
       DO I=1,3
         DO J=0,12
           DO K=0,6
             DO L=-2,6
               E(I,J,K,L)=ZERO
             ENDDO
           ENDDO
         ENDDO
       ENDDO
C
C Define E(Ip,Ia,Ib) for Ip=0, Ia=0, Ib=0.
C
       E(1,0,0,0) = ES(1)
       E(2,0,0,0) = ES(2)
       E(3,0,0,0) = ES(3)
C
C Compute the prefactor for the 1st term of the recursion formulas.
C
      IF( LA.GT.0 .OR. LB.GT.0 )THEN
        PF(1) = 0.5D+00/( AI + AJ )
      END IF
C
C Define E(Ip,Ia,Ib) for Ip = 0,Ia+Ib; Ia = 1,La; Ib = 0.
C
      IF( LA.GT.0 )THEN
C
C Compute the prefactor for the 2nd term of the recursion formula.
C
        PF(2) = -2.0D+00*AJ*PF(1)
C
C            ===>   Ip = 0,Ia; Ia = 1; Ib = 0   <===
C
C
        EX2 = RX*E(1,0,0,0)
        EY2 = RY*E(2,0,0,0)
        EZ2 = RZ*E(3,0,0,0)
C
        E(1,0,1,0) = PF(2)*EX2
        E(2,0,1,0) = PF(2)*EY2
        E(3,0,1,0) = PF(2)*EZ2
C
        EX1 = E(1,0,0,0)
        EY1 = E(2,0,0,0)
        EZ1 = E(3,0,0,0)
C
        E(1,1,1,0) = PF(1)*EX1
        E(2,1,1,0) = PF(1)*EY1
        E(3,1,1,0) = PF(1)*EZ1
C
C
       DO 260 IA = 2,LA
C
C            ===>   Ip = 0; Ia = 2,La; Ib = 0   <===
C
C
         EX2 = RX*E(1,0,IA-1,0)
         EX3 =    E(1,1,IA-1,0)
C
         EY2 = RY*E(2,0,IA-1,0)
         EY3 =    E(2,1,IA-1,0)
C
         EZ2 = RZ*E(3,0,IA-1,0)
         EZ3 =    E(3,1,IA-1,0)
C
         E(1,0,IA,0) = PF(2)*EX2 + EX3
         E(2,0,IA,0) = PF(2)*EY2 + EY3
         E(3,0,IA,0) = PF(2)*EZ2 + EZ3
C
C
C            ===>   Ip = 1,Ia-2; Ia = 2,La; Ib = 0   <===
C
         DO 240 IP = 1,IA-2
C
C
           EX1 =    E(1,IP-1,IA-1,0)
           EX2 = RX*E(1,IP  ,IA-1,0)
           EX3 =    E(1,IP+1,IA-1,0)
C
           EY1 =    E(2,IP-1,IA-1,0)
           EY2 = RY*E(2,IP  ,IA-1,0)
           EY3 =    E(2,IP+1,IA-1,0)
C
           EZ1 =    E(3,IP-1,IA-1,0)
           EZ2 = RZ*E(3,IP  ,IA-1,0)
           EZ3 =    E(3,IP+1,IA-1,0)
C
           E(1,IP,IA,0) = PF(1)*EX1 + PF(2)*EX2 + (IP+1)*EX3
           E(2,IP,IA,0) = PF(1)*EY1 + PF(2)*EY2 + (IP+1)*EY3
           E(3,IP,IA,0) = PF(1)*EZ1 + PF(2)*EZ2 + (IP+1)*EZ3
C
C
  240    CONTINUE
C
C            ===>   Ip = Ia-1,Ia; Ia = 2,La; Ib = 0   <===
C
        IP = IA-1
C
C
         EX1 =    E(1,IP-1,IA-1,0)
         EX2 = RX*E(1,IP  ,IA-1,0)
C
         EY1 =    E(2,IP-1,IA-1,0)
         EY2 = RY*E(2,IP  ,IA-1,0)
C
         EZ1 =    E(3,IP-1,IA-1,0)
         EZ2 = RZ*E(3,IP  ,IA-1,0)
C
         E(1,IP,IA,0) = PF(1)*EX1 + PF(2)*EX2
         E(2,IP,IA,0) = PF(1)*EY1 + PF(2)*EY2
         E(3,IP,IA,0) = PF(1)*EZ1 + PF(2)*EZ2
C
         EX1 = E(1,IP,IA-1,0)
         EY1 = E(2,IP,IA-1,0)
         EZ1 = E(3,IP,IA-1,0)
C
         E(1,IP+1,IA,0) = PF(1)*EX1
         E(2,IP+1,IA,0) = PF(1)*EY1
         E(3,IP+1,IA,0) = PF(1)*EZ1
C
C
  260   CONTINUE
C
      END IF
C
C Define E(Ip,Ia,Ib) for Ip=0,Ia+Ib, Ia=0,La, Ib=1,Lb.
C
      IF( LB.GT.0 )THEN
C
C Compute the prefactor for the 2nd term of the recursion formula.
C
        PF(2) = 2.0D+00*AI*PF(1)
C
C    ===>   Ip = 0,Ia+Ib; Ia = 0; Ib = 1   <===
C
C
        EX2 = RX*E(1,0,0,0)
        EY2 = RY*E(2,0,0,0)
        EZ2 = RZ*E(3,0,0,0)
C
        E(1,0,0,1) = PF(2)*EX2
        E(2,0,0,1) = PF(2)*EY2
        E(3,0,0,1) = PF(2)*EZ2
C
        EX1 = E(1,0,0,0)
        EY1 = E(2,0,0,0)
        EZ1 = E(3,0,0,0)
C
        E(1,1,0,1) = PF(1)*EX1
        E(2,1,0,1) = PF(1)*EY1
        E(3,1,0,1) = PF(1)*EZ1
C
C
       DO 370 IB = 1,LB
C
        IF( IB.EQ.1 )THEN
         IA1 = 1
        ELSE
         IA1 = 0
        END IF
C
        DO 360 IA = IA1,LA
C
C    ===>   Ip = 0; Ia = Ia1,La; Ib = 1,Lb   <===
C
C
          EX2 = RX*E(1,0,IA,IB-1)
          EX3 =    E(1,1,IA,IB-1)
C
          EY2 = RY*E(2,0,IA,IB-1)
          EY3 =    E(2,1,IA,IB-1)
C
          EZ2 = RZ*E(3,0,IA,IB-1)
          EZ3 =    E(3,1,IA,IB-1)
C
          E(1,0,IA,IB) = PF(2)*EX2 + EX3
          E(2,0,IA,IB) = PF(2)*EY2 + EY3
          E(3,0,IA,IB) = PF(2)*EZ2 + EZ3
C
C
C    ===>   Ip = 1,Ia+Ib-2; Ia = Ia1,La; Ib = 1,Lb   <===
C
         DO 340 IP = 1,IA+IB-2
C
C
           EX1 =    E(1,IP-1,IA,IB-1)
           EX2 = RX*E(1,IP  ,IA,IB-1)
           EX3 =    E(1,IP+1,IA,IB-1)
C
           EY1 =    E(2,IP-1,IA,IB-1)
           EY2 = RY*E(2,IP  ,IA,IB-1)
           EY3 =    E(2,IP+1,IA,IB-1)
C
           EZ1 =    E(3,IP-1,IA,IB-1)
           EZ2 = RZ*E(3,IP  ,IA,IB-1)
           EZ3 =    E(3,IP+1,IA,IB-1)
C
           E(1,IP,IA,IB) = PF(1)*EX1 + PF(2)*EX2 + (IP+1)*EX3
           E(2,IP,IA,IB) = PF(1)*EY1 + PF(2)*EY2 + (IP+1)*EY3
           E(3,IP,IA,IB) = PF(1)*EZ1 + PF(2)*EZ2 + (IP+1)*EZ3
C
C
  340     CONTINUE
C
C    ===>   Ip = Ia+Ib-1,Ia+Ib; Ia = Ia1,La; Ib = 1,Lb   <===
C
         IP = IA+IB-1
C
C
          EX1 =    E(1,IP-1,IA,IB-1)
          EX2 = RX*E(1,IP  ,IA,IB-1)
C
          EY1 =    E(2,IP-1,IA,IB-1)
          EY2 = RY*E(2,IP  ,IA,IB-1)
C
          EZ1 =    E(3,IP-1,IA,IB-1)
          EZ2 = RZ*E(3,IP  ,IA,IB-1)
C
          E(1,IP,IA,IB) = PF(1)*EX1 + PF(2)*EX2
          E(2,IP,IA,IB) = PF(1)*EY1 + PF(2)*EY2
          E(3,IP,IA,IB) = PF(1)*EZ1 + PF(2)*EZ2
C
          EX1 = E(1,IP,IA,IB-1)
          EY1 = E(2,IP,IA,IB-1)
          EZ1 = E(3,IP,IA,IB-1)
C
          E(1,IP+1,IA,IB) = PF(1)*EX1
          E(2,IP+1,IA,IB) = PF(1)*EY1
          E(3,IP+1,IA,IB) = PF(1)*EZ1
C
C
  360    CONTINUE
C
  370   CONTINUE
C
      END IF
C
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION GAMIN(A,X)
C
C     Calculate the incomplete Gamma function by (6.5.33) in
C     Abramowitz and Stegun.  This is called but once, then the
C     recurrance relations take over to calculate foft(j)s.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (ITER=100, ONE=1.0D+00)
C
      DIMENSION XGAMMA(101)
C
      DATA XGAMMA /1.00000000000000000D+00,
     * 1.00000000000000000D+00, 2.00000000000000000D+00,
     * 6.00000000000000000D+00, 24.0000000000000000D+00,
     * 120.000000000000000D+00, 720.000000000000000D+00,
     * 5040.00000000000000D+00, 40320.0000000000000D+00,
     * 362880.000000000000D+00, 3628800.00000000000D+00,
     * 39916800.0000000000D+00, 479001600.000000000D+00,
     * 6227020800.00000000D+00, 87178291200.0000000D+00,
     * 1307674368000.00000D+00, 20922789888000.0000D+00,
     * 355687428096000.000D+00, 6402373705728000.00D+00,
     * 0.121645100408832000D+18, 0.243290200817664000D+19,
     * 0.510909421717094400D+20, 0.112400072777760768D+22,
     * 0.258520167388849782D+23, 0.620448401733239410D+24,
     * 0.155112100433309861D+26, 0.403291461126605650D+27,
     * 0.108888694504183519D+29, 0.304888344611713872D+30,
     * 0.884176199373970190D+31, 0.265252859812191068D+33,
     * 0.822283865417792358D+34, 0.263130836933693555D+36,
     * 0.868331761881188712D+37, 0.295232799039604157D+39,
     * 0.103331479663861430D+41, 0.371993326789901255D+42,
     * 0.137637530912263456D+44, 0.523022617466601117D+45,
     * 0.203978820811974441D+47, 0.815915283247897684D+48,
     * 0.334525266131638080D+50, 0.140500611775287962D+52,
     * 0.604152630633738341D+53, 0.265827157478844887D+55,
     * 0.119622220865480210D+57, 0.550262215981208915D+58,
     * 0.258623241511168178D+60, 0.124139155925360711D+62,
     * 0.608281864034267522D+63, 0.304140932017133814D+65,
     * 0.155111875328738219D+67, 0.806581751709438649D+68,
     * 0.427488328406002472D+70, 0.230843697339241379D+72,
     * 0.126964033536582749D+74, 0.710998587804863481D+75,
     * 0.405269195048772077D+77, 0.235056133128287849D+79,
     * 0.138683118545689865D+81, 0.832098711274138990D+82,
     * 0.507580213877224836D+84, 0.314699732603879394D+86,
     * 0.198260831540444053D+88, 0.126886932185884165D+90,
     * 0.824765059208247152D+91, 0.544344939077443069D+93,
     * 0.364711109181886906D+95, 0.248003554243683007D+97,
     * 0.171122452428141297D+99, 0.119785716699698942D+101,
     * 0.850478588567862176D+102, 0.612344583768860926D+104,
     * 0.447011546151268489D+106, 0.330788544151938624D+108,
     * 0.248091408113954039D+110, 0.188549470166605011D+112,
     * 0.145183092028285872D+114, 0.113242811782062968D+116,
     * 0.894618213078297711D+117, 0.715694570462637968D+119,
     * 0.579712602074736669D+121, 0.475364333701284129D+123,
     * 0.394552396972065879D+125, 0.331424013456535319D+127,
     * 0.281710411438055052D+129, 0.242270953836727292D+131,
     * 0.210775729837952754D+133, 0.185482642257398436D+135,
     * 0.165079551609084602D+137, 0.148571596448176135D+139,
     * 0.135200152767840292D+141, 0.124384140546413055D+143,
     * 0.115677250708164157D+145, 0.108736615665674308D+147,
     * 0.103299784882390579D+149, 0.991677934870949648D+150,
     * 0.961927596824821088D+152, 0.942689044888324798D+154,
     * 0.933262154439441533D+156, 0.933262154439441510D+158/
C
      GAMIN = X**A/A
C
      DO 100 N=1,ITER
C
        B = (-ONE)**N
        C = X**(A+N)
        D = (A+N)*XGAMMA(N+1)
C
        GAMIN = GAMIN + B*C/D
C
 100  CONTINUE
C
      RETURN
      END
C*MODULE NMR  *DECK FOFTJ
C
      SUBROUTINE FOFTJ(NLM,T,FOFT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (HALF=0.5D+00,ONE=1.0D+00,TWO=2.0D+00,TWLV=12.0D+00,
     *           FIFTN=15.0D+00,EIGTN=18.0D+00,TWFR=24.0D+00,
     *           PI=3.1415926535897932D+00,SMALL=1.0D-09)
C
      DIMENSION FOFT(0:24), GAMINT(0:25)
C
C     nlm = 0, N+L+M  (ia+ib+ja+jb+ka+kb)
C     t = alpha_p * CP**2 (3.100)
C
C     Find the array of incomplete gamma functions for a given T,
C     using Ambramowitz and Stegun's recurrence relation (6.5.22)
C
C     When T=0, remember
C     Fj(T) = Integrate[u^2j E^(-t u^2), {u, 0, 1}] (3.92)
C
      IF(T.LT.SMALL) THEN
        DO 50 J=0,NLM
          FOFT(J) = ONE/(2*J + 1)
 50     CONTINUE
        RETURN
      END IF
C
      IF(T.LT.TWLV) THEN
C
        GAMINT(0) = GAMIN(HALF,T)
C
C       Get other incomplete gamma(j+1/2,T)
C
        DO 100 J=0,NLM
C
          HALFJ       = J + HALF
          GAMINT(J+1) = HALFJ * GAMINT(J) - T**HALFJ * EXP(-T)
C
C         Get F_j(T)'s as given by Shavitt, "Methods in Computational
C         Physics", vol. 2, p. 7. (1963)
C
          COEF    = ONE/(TWO*T**(J+HALF))
          FOFT(J) = COEF * GAMINT(J)
C
 100    CONTINUE
C
      ELSE
C
C       Use formalism given in the McMurchie and Davidson paper.
C
        G=0.4999489092D+00-0.2473631686D+00/T
     *        +0.321180909D+00/T**2-0.3811559346D+00/T**3
        IF(T.GT.FIFTN) G=0.4998436875D+00-0.24249438D+00/T
     *        +0.24642845D+00/T**2
        IF(T.GT.EIGTN) G=0.499093162D+00-0.2152832D+00/T
        IF(T.GT.TWFR) G=0.490D+00
C
        FOFT(0) = HALF*SQRT(PI/T) - EXP(-T)*G/T
C
        DO 200 J=0,NLM
C
          AA = (ONE/(TWO*T))
          AB = (TWO*J + 1)
          AD = EXP(-T)
C
          FOFT(J+1) = AA * (AB*FOFT(J) - AD)
C
 200    CONTINUE
C
      END IF
C
      RETURN
      END
C*MODULE NMR  *DECK RNLM
C
      SUBROUTINE RNLM(MXNLM,A,B,C,ALPHA,FOFT,R)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (TWO=2.0D+00)
C
      DIMENSION FOFT(0:24),R(0:24,0:24,0:24,0:24)
C
C     N=0,ia+ib   L=0,ja+jb   M=0,ka+kb
C     t = alpha_p * CP**2 (eq. 3.11 in MD paper)
C       = alpha_p * (a**2 + b**2 + c**2) (eq. 4.2 in MD paper)
C
C     Note:  The foft passed in should be for the correct value of T!
C
C     Rnlmj => (4.1) in McMurchie + Davidson; recurrence relations are:
C
C     R0,0,M+1,j = c*R0,0,M,j+1  +  M*R0,0,M-1,j+1
C     R0,L+1,M,j = b*R0,L,M,j+1  +  L*R0,L-1,M,j+1
C     RN+1,L,M,j = a*RN,L,M,j+1  +  N*RN-1,L,M,j+1     (3.95-97)
C
      DO 100 J=0,MXNLM
        R(0,0,0,J) = (-TWO*ALPHA)**J * FOFT(J)
 100  CONTINUE
C
      DO 10 N=0,MXNLM-1
        DO 20 L=0,MXNLM-1
          DO 30 M=0,MXNLM-1
            DO 40 J=0,MXNLM-1
C
              R(0,0,M+1,J) = C*R(0,0,M,J+1) + M*R(0,0,M-1,J+1)
              R(0,L+1,M,J) = B*R(0,L,M,J+1) + L*R(0,L-1,M,J+1)
              R(N+1,L,M,J) = A*R(N,L,M,J+1) + N*R(N-1,L,M,J+1)
C
 40         CONTINUE
 30       CONTINUE
 20     CONTINUE
 10   CONTINUE
C
      RETURN
      END
C*MODULE NMR  *DECK OEPRSQL
      SUBROUTINE OEPRSQL(V,M,N,NDIM,NAT,IC,IN,ID)
C
C     Modified printing routine (prsql)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500)
C
      DIMENSION V(NDIM,M,NAT,3,3)
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNLAB/ TITLE(10),ANAM(MXATM),BNAM(MXATM),BFLAB(2047)
C
C     ----- PRINT OUT A SQUARE MATRIX WITH BASIS SET LABELS -----
C     -V- IS -N- ROWS BY -M- COLUMNS, WITH LEADING DIMENSION -NDIM-
C
      IF (MASWRK) THEN
      MAX = 10
      IF (NPRINT .EQ. 6) MAX = 10
      IMAX = 0
  100 IMIN = IMAX+1
      IMAX = IMAX+MAX
      IF (IMAX .GT. M) IMAX = M
      WRITE (IW,9008)
      WRITE (IW,9028) (I,I = IMIN,IMAX)
      WRITE (IW,9008)
      DO 120 J = 1,N
  120 WRITE (IW,9048) J,BFLAB(J),(V(J,I,IC,IN,ID),I = IMIN,IMAX)
      IF (IMAX .LT. M) GO TO 100
      END IF
      RETURN
 9008 FORMAT(1X)
 9028 FORMAT(15X,10(4X,I4,3X))
 9048 FORMAT(I5,2X,A8,10F11.6)
      END
C*MODULE NMR  *DECK SNPRTRIL
      SUBROUTINE SNPRTRIL(D,N,NAT,IC)
C
C     Modified printing routine, to print upper-triangular w/o diagonal,
C     which is zero for anti-symmetric matrix.  Diagonal is NOT stored.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500, MXAO=2047)
C
      DIMENSION D((N*N-N)/2,NAT)
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNLAB/ TITLE(10),ANAM(MXATM),BNAM(MXATM),BFLAB(MXAO)
C
C     ----- PRINT SYMMETRIC MATRIX -D- OF DIMENSION -N- -----
C     THE ROWS WILL BE LABELED WITH BASIS FUNCTION TAGS.
C
      IF (MASWRK) THEN
      MAX = 10
      IF (NPRINT .EQ. 6) MAX = 10
      MM1 = MAX - 1
      DO 120 I0=1,N-1,MAX
         IL = MIN(N-1,I0+MM1)
         WRITE(IW,9008)
         WRITE(IW,9028) (I,I=I0,IL)
         WRITE(IW,9008)
         IL = -1
         DO 100 I=I0,N-1
            IL=IL+1
            J0=I0+(I*I-I)/2
            JL=J0+MIN(IL,MM1)
            WRITE(IW,9048) I+1,BFLAB(I+1),(D(J,IC),J=J0,JL)
  100    CONTINUE
  120 CONTINUE
      END IF
      RETURN
 9008 FORMAT(1X)
 9028 FORMAT(15X,10(4X,I4,3X))
 9048 FORMAT(I5,2X,A8,10F11.6)
      END
C*MODULE NMR  *DECK SPRTRIL
      SUBROUTINE SPRTRIL(D,N)
C
C     Modified printing routine, to print upper-triangular w/o diagonal,
C     which is zero for anti-symmetric matrix.  Diagonal is NOT stored.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500, MXAO=2047)
C
      DIMENSION D((N*N-N)/2)
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNLAB/ TITLE(10),ANAM(MXATM),BNAM(MXATM),BFLAB(MXAO)
C
C     ----- PRINT SYMMETRIC MATRIX -D- OF DIMENSION -N- -----
C     THE ROWS WILL BE LABELED WITH BASIS FUNCTION TAGS.
C
      IF (MASWRK) THEN
      MAX = 10
      IF (NPRINT .EQ. 6) MAX = 10
      MM1 = MAX - 1
      DO 120 I0=1,N-1,MAX
         IL = MIN(N-1,I0+MM1)
         WRITE(IW,9008)
         WRITE(IW,9028) (I,I=I0,IL)
         WRITE(IW,9008)
         IL = -1
         DO 100 I=I0,N-1
            IL=IL+1
            J0=I0+(I*I-I)/2
            JL=J0+MIN(IL,MM1)
            WRITE(IW,9048) I+1,BFLAB(I+1),(D(J),J=J0,JL)
  100    CONTINUE
  120 CONTINUE
      END IF
      RETURN
 9008 FORMAT(1X)
 9028 FORMAT(15X,10(4X,I4,3X))
 9048 FORMAT(I5,2X,A8,10F11.6)
      END
C*MODULE NMR  *DECK SOEPRTRIL
      SUBROUTINE SOEPRTRIL(D,N,NAT,IC,INU,ID)
C
C     Modified printing routine, to print upper-triangular w/o diagonal,
C     which is zero for anti-symmetric matrix.  Diagonal is NOT stored.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500, MXAO=2047)
C
      DIMENSION D((N*N-N)/2,NAT,3,3)
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNLAB/ TITLE(10),ANAM(MXATM),BNAM(MXATM),BFLAB(MXAO)
C
C     ----- PRINT SYMMETRIC MATRIX -D- OF DIMENSION -N- -----
C     THE ROWS WILL BE LABELED WITH BASIS FUNCTION TAGS.
C
      IF (MASWRK) THEN
      MAX = 10
      IF (NPRINT .EQ. 6) MAX = 10
      MM1 = MAX - 1
      DO 120 I0=1,N-1,MAX
         IL = MIN(N-1,I0+MM1)
         WRITE(IW,9008)
         WRITE(IW,9028) (I,I=I0,IL)
         WRITE(IW,9008)
         IL = -1
         DO 100 I=I0,N-1
            IL=IL+1
            J0=I0+(I*I-I)/2
            JL=J0+MIN(IL,MM1)
            WRITE(IW,9048) I+1,BFLAB(I+1),(D(J,IC,INU,ID),J=J0,JL)
  100    CONTINUE
  120 CONTINUE
      END IF
      RETURN
 9008 FORMAT(1X)
 9028 FORMAT(15X,10(4X,I4,3X))
 9048 FORMAT(I5,2X,A8,10F11.6)
      END
C*MODULE NMR  *DECK TIPRSQL
      SUBROUTINE TIPRSQL(V,M,N,NDIM,NAT,IC,IAN,IN,ID)
C
C     Modified printing routine.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500)
C
      DIMENSION V(NDIM,M,NAT,3,3,3)
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNLAB/ TITLE(10),ANAM(MXATM),BNAM(MXATM),BFLAB(2047)
C
C     ----- PRINT OUT A SQUARE MATRIX WITH BASIS SET LABELS -----
C     -V- IS -N- ROWS BY -M- COLUMNS, WITH LEADING DIMENSION -NDIM-
C
      IF (MASWRK) THEN
      MAX = 10
      IF (NPRINT .EQ. 6) MAX = 10
      IMAX = 0
  100 IMIN = IMAX+1
      IMAX = IMAX+MAX
      IF (IMAX .GT. M) IMAX = M
      WRITE (IW,9008)
      WRITE (IW,9028) (I,I = IMIN,IMAX)
      WRITE (IW,9008)
      DO 120 J = 1,N
  120 WRITE (IW,9048) J,BFLAB(J),(V(J,I,IC,IAN,IN,ID),I = IMIN,IMAX)
      IF (IMAX .LT. M) GO TO 100
      END IF
      RETURN
 9008 FORMAT(1X)
 9028 FORMAT(15X,10(4X,I4,3X))
 9048 FORMAT(I5,2X,A8,10F11.6)
      END
C*MODULE NMR  *DECK HPRSQL
      SUBROUTINE HPRSQL(V,M,N,NDIM,NAT,IC)
C
C     Modified printing routine.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500)
C
      DIMENSION V(NDIM,M,NAT)
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNLAB/ TITLE(10),ANAM(MXATM),BNAM(MXATM),BFLAB(2047)
C
C     ----- PRINT OUT A SQUARE MATRIX WITH BASIS SET LABELS -----
C     -V- IS -N- ROWS BY -M- COLUMNS, WITH LEADING DIMENSION -NDIM-
C
      IF (MASWRK) THEN
      MAX = 10
      IF (NPRINT .EQ. 6) MAX = 10
      IMAX = 0
  100 IMIN = IMAX+1
      IMAX = IMAX+MAX
      IF (IMAX .GT. M) IMAX = M
      WRITE (IW,9008)
      WRITE (IW,9028) (I,I = IMIN,IMAX)
      WRITE (IW,9008)
      DO 120 J = 1,N
  120 WRITE (IW,9048) J,BFLAB(J),(V(J,I,IC),I = IMIN,IMAX)
      IF (IMAX .LT. M) GO TO 100
      END IF
      RETURN
 9008 FORMAT(1X)
 9028 FORMAT(15X,10(4X,I4,3X))
 9048 FORMAT(I5,2X,A8,10F11.6)
      END
C*MODULE NMR  *DECK H01
      SUBROUTINE H01(OESOIM,H01X,H01Y,H01Z,L2S)
C
C     See Eq. (3.38) in MAF thesis
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL DBG
C
      DIMENSION OESOIM(L2S,NAT,3,3),
     *          H01X(L2S,NAT),
     *          H01Y(L2S,NAT),
     *          H01Z(L2S,NAT)
C
      PARAMETER (MXATM=500)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
C                           (0,1)
C     These are the three [H            ]
C                           B,nu,lambda  beta
C     integrals WITHOUT the 1/c factor which is added in PARAMAG
C
      DBG=.FALSE.
C
      DO 300 IC=1,NAT
C
        DO 600 NN=1,L2S
C
          H01X(NN,IC) = OESOIM(NN,IC,2,3) - OESOIM(NN,IC,3,2)
          H01Y(NN,IC) = OESOIM(NN,IC,3,1) - OESOIM(NN,IC,1,3)
          H01Z(NN,IC) = OESOIM(NN,IC,1,2) - OESOIM(NN,IC,2,1)
C
  600   CONTINUE
C
        IF(DBG) THEN
        WRITE(IW,*) 'MAF H01X MATRIX, NUCLEUS', IC
        CALL SNPRTRIL(H01X,NUM,NAT,IC)
C
        WRITE(IW,*) 'MAF H01Y MATRIX, NUCLEUS', IC
        CALL SNPRTRIL(H01Y,NUM,NAT,IC)
C
        WRITE(IW,*) 'MAF H01Z MATRIX, NUCLEUS', IC
        CALL SNPRTRIL(H01Z,NUM,NAT,IC)
        END IF
C
  300 CONTINUE
C
      RETURN
      END
C*MODULE NMR  *DECK H11
      SUBROUTINE H11(DELINM,TINTM,H01X,H01Y,H01Z,QXM,QYM,QZM,
     *               XIJM,YIJM,ZIJM,H11XX,H11XY,H11XZ,
     *               H11YX,H11YY,H11YZ,H11ZX,H11ZY,H11ZZ,L2S)
C
C     See Eq. (3.39) in MAF thesis.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL DBG
C
      DIMENSION TINTM(NUM,NUM,NAT,3,3,3),DELINM(NUM,NUM,NAT,3,3),
     *    H01X(L2S,NAT),H01Y(L2S,NAT),H01Z(L2S,NAT),
     *    QXM(L2S),QYM(L2S),QZM(L2S),XIJM(L2S),YIJM(L2S),ZIJM(L2S),
     *    H11XX(NUM,NUM,NAT),H11XY(NUM,NUM,NAT),H11XZ(NUM,NUM,NAT),
     *    H11YX(NUM,NUM,NAT),H11YY(NUM,NUM,NAT),H11YZ(NUM,NUM,NAT),
     *    H11ZX(NUM,NUM,NAT),H11ZY(NUM,NUM,NAT),H11ZZ(NUM,NUM,NAT)
C
      PARAMETER (MXATM=500)
      PARAMETER (ZERO=0.0D+00,ONE=1.0D+00)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
C                          (1,1)
C     These are the nine [H            ]
C                          B,nu,lambda  alpha,beta
C     integrals WITHOUT the 1/2c factor
C
      DBG=.FALSE.
C
      DO 300 IC=1,NAT
C
C       qxm, xijm are anti-symmetric, but integrals have no symmetry...
C       must be careful...
C
        DO 400 I=1,NUM
         II=((I-1)*(I-1)-(I-1))/2
        DO 500 J=1,NUM
         JJ=((J-1)*(J-1)-(J-1))/2
         IF(I.EQ.J) THEN
           NN=1
           FACI=ZERO
           FACN=ZERO
         ELSE IF(I.LT.J) THEN
           NN=JJ+I
           FACI=ONE
           FACN=ONE
         ELSE IF(I.GT.J) THEN
           NN=II+J
           FACI=-ONE
           FACN=ONE
         END IF
C
          H11XX(I,J,IC)=FACI*(YIJM(NN)*TINTM(I,J,IC,3,2,3)-
     *                  YIJM(NN)*TINTM(I,J,IC,3,3,2)-
     *                  ZIJM(NN)*TINTM(I,J,IC,2,2,3)+
     *                  ZIJM(NN)*TINTM(I,J,IC,2,3,2))+
     *                  FACN*QXM(NN)*H01X(NN,IC)
     *                  +DELINM(I,J,IC,2,2)+
     *                  DELINM(I,J,IC,3,3)
C
          H11XY(I,J,IC)=FACI*(YIJM(NN)*TINTM(I,J,IC,3,3,1)-
     *                  YIJM(NN)*TINTM(I,J,IC,3,1,3)-
     *                  ZIJM(NN)*TINTM(I,J,IC,2,3,1)+
     *                  ZIJM(NN)*TINTM(I,J,IC,2,1,3))+
     *                  FACN*QXM(NN)*H01Y(NN,IC)
     *                  -DELINM(I,J,IC,1,2)
C
          H11XZ(I,J,IC)=FACI*(YIJM(NN)*TINTM(I,J,IC,3,1,2)-
     *                  YIJM(NN)*TINTM(I,J,IC,3,2,1)-
     *                  ZIJM(NN)*TINTM(I,J,IC,2,1,2)+
     *                  ZIJM(NN)*TINTM(I,J,IC,2,2,1))+
     *                  FACN*QXM(NN)*H01Z(NN,IC)
     *                  -DELINM(I,J,IC,1,3)
C
          H11YX(I,J,IC)=FACI*(ZIJM(NN)*TINTM(I,J,IC,1,2,3)-
     *                  ZIJM(NN)*TINTM(I,J,IC,1,3,2)-
     *                  XIJM(NN)*TINTM(I,J,IC,3,2,3)+
     *                  XIJM(NN)*TINTM(I,J,IC,3,3,2))+
     *                  FACN*QYM(NN)*H01X(NN,IC)
     *                  -DELINM(I,J,IC,2,1)
C
          H11YY(I,J,IC)=FACI*(ZIJM(NN)*TINTM(I,J,IC,1,3,1)-
     *                  ZIJM(NN)*TINTM(I,J,IC,1,1,3)-
     *                  XIJM(NN)*TINTM(I,J,IC,3,3,1)+
     *                  XIJM(NN)*TINTM(I,J,IC,3,1,3))+
     *                  FACN*QYM(NN)*H01Y(NN,IC)
     *                  +DELINM(I,J,IC,1,1)+
     *                  DELINM(I,J,IC,3,3)
C
          H11YZ(I,J,IC)=FACI*(ZIJM(NN)*TINTM(I,J,IC,1,1,2)-
     *                  ZIJM(NN)*TINTM(I,J,IC,1,2,1)-
     *                  XIJM(NN)*TINTM(I,J,IC,3,1,2)+
     *                  XIJM(NN)*TINTM(I,J,IC,3,2,1))+
     *                  FACN*QYM(NN)*H01Z(NN,IC)
     *                  -DELINM(I,J,IC,2,3)
C
          H11ZX(I,J,IC)=FACI*(XIJM(NN)*TINTM(I,J,IC,2,2,3)-
     *                  XIJM(NN)*TINTM(I,J,IC,2,3,2)-
     *                  YIJM(NN)*TINTM(I,J,IC,1,2,3)+
     *                  YIJM(NN)*TINTM(I,J,IC,1,3,2))+
     *                  FACN*QZM(NN)*H01X(NN,IC)
     *                  -DELINM(I,J,IC,3,1)
C
          H11ZY(I,J,IC)=FACI*(XIJM(NN)*TINTM(I,J,IC,2,3,1)-
     *                  XIJM(NN)*TINTM(I,J,IC,2,1,3)-
     *                  YIJM(NN)*TINTM(I,J,IC,1,3,1)+
     *                  YIJM(NN)*TINTM(I,J,IC,1,1,3))+
     *                  FACN*QZM(NN)*H01Y(NN,IC)
     *                  -DELINM(I,J,IC,3,2)
C
          H11ZZ(I,J,IC)=FACI*(XIJM(NN)*TINTM(I,J,IC,2,1,2)-
     *                  XIJM(NN)*TINTM(I,J,IC,2,2,1)-
     *                  YIJM(NN)*TINTM(I,J,IC,1,1,2)+
     *                  YIJM(NN)*TINTM(I,J,IC,1,2,1))+
     *                  FACN*QZM(NN)*H01Z(NN,IC)
     *                  +DELINM(I,J,IC,1,1)+
     *                  DELINM(I,J,IC,2,2)
C
C
  500   CONTINUE
  400   CONTINUE
C
        IF(DBG) THEN
        WRITE(IW,9999) 'X','X',IC
        CALL HPRSQL(H11XX,NUM,NUM,NUM,NAT,IC)
C
        WRITE(IW,9999) 'X','Y',IC
        CALL HPRSQL(H11XY,NUM,NUM,NUM,NAT,IC)
C
        WRITE(IW,9999) 'X','Z',IC
        CALL HPRSQL(H11XZ,NUM,NUM,NUM,NAT,IC)
C
        WRITE(IW,9999) 'Y','X',IC
        CALL HPRSQL(H11YX,NUM,NUM,NUM,NAT,IC)
C
        WRITE(IW,9999) 'Y','Y',IC
        CALL HPRSQL(H11YY,NUM,NUM,NUM,NAT,IC)
C
        WRITE(IW,9999) 'Y','Z',IC
        CALL HPRSQL(H11YZ,NUM,NUM,NUM,NAT,IC)
C
        WRITE(IW,9999) 'Z','X',IC
        CALL HPRSQL(H11ZX,NUM,NUM,NUM,NAT,IC)
C
        WRITE(IW,9999) 'Z','Y',IC
        CALL HPRSQL(H11ZY,NUM,NUM,NUM,NAT,IC)
C
        WRITE(IW,9999) 'Z','Z',IC
        CALL HPRSQL(H11ZZ,NUM,NUM,NUM,NAT,IC)
        END IF
C
  300 CONTINUE
C
      RETURN
 9999 FORMAT(/5X,'H(1,1)',A1,A1,' MATRIX FOR NUCLEUS',I2,':')
      END
C*MODULE NMR  *DECK DIAMAG
      SUBROUTINE DIAMAG(H11XX,H11XY,H11XZ,
     *                  H11YX,H11YY,H11YZ,
     *                  H11ZX,H11ZY,H11ZZ,
     *                  FTXX,FTXY,FTXZ,
     *                  FTYX,FTYY,FTYZ,
     *                  FTZX,FTZY,FTZZ,P,PRMAT,PDIA)
C
C     This is actually the second term of Eq. (3.37), MAF thesis.
C     It is usually called the "diamagnetic" term.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL PRMAT,PDIA
C
      PARAMETER (MXATM=500)
C
      PARAMETER (ZERO=0.0D+00,C2PPM=2.66257D+01)
C
      DIMENSION FTXX(NAT),FTXY(NAT),FTXZ(NAT),
     *          FTYX(NAT),FTYY(NAT),FTYZ(NAT),
     *          FTZX(NAT),FTZY(NAT),FTZZ(NAT),
     *    H11XX(NUM,NUM,NAT),H11XY(NUM,NUM,NAT),H11XZ(NUM,NUM,NAT),
     *    H11YX(NUM,NUM,NAT),H11YY(NUM,NUM,NAT),H11YZ(NUM,NUM,NAT),
     *    H11ZX(NUM,NUM,NAT),H11ZY(NUM,NUM,NAT),H11ZZ(NUM,NUM,NAT),
     *    P((NUM*NUM+NUM)/2)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /RUNLAB/ TITLE(10),ANAM(MXATM),BNAM(MXATM),BFLAB(2047)
C
      IF(PRMAT) THEN
      DO IC=1,NAT
C
        WRITE(IW,9999) 'X','X',IC
        CALL HPRSQL(H11XX,NUM,NUM,NUM,NAT,IC)
C
        WRITE(IW,9999) 'X','Y',IC
        CALL HPRSQL(H11XY,NUM,NUM,NUM,NAT,IC)
C
        WRITE(IW,9999) 'X','Z',IC
        CALL HPRSQL(H11XZ,NUM,NUM,NUM,NAT,IC)
C
        WRITE(IW,9999) 'Y','X',IC
        CALL HPRSQL(H11YX,NUM,NUM,NUM,NAT,IC)
C
        WRITE(IW,9999) 'Y','Y',IC
        CALL HPRSQL(H11YY,NUM,NUM,NUM,NAT,IC)
C
        WRITE(IW,9999) 'Y','Z',IC
        CALL HPRSQL(H11YZ,NUM,NUM,NUM,NAT,IC)
C
        WRITE(IW,9999) 'Z','X',IC
        CALL HPRSQL(H11ZX,NUM,NUM,NUM,NAT,IC)
C
        WRITE(IW,9999) 'Z','Y',IC
        CALL HPRSQL(H11ZY,NUM,NUM,NUM,NAT,IC)
C
        WRITE(IW,9999) 'Z','Z',IC
        CALL HPRSQL(H11ZZ,NUM,NUM,NUM,NAT,IC)
C
      ENDDO
      END IF
C
C     Get density matrix from record 16
C
      L2=(NUM*NUM+NUM)/2
C
      CALL DAREAD(IDAF,IODA,P,L2,16,0)
C
      IF(PRMAT) WRITE(IW,9998)
      IF(PRMAT) CALL PRTRIL(P,NUM)
C
      IF(PDIA) WRITE(IW,9100)
C
C     Do the sum over all i,j (nu,lambda)
C
      DO 250 IC=1,NAT
C
      FTXX(IC)=ZERO
      FTXY(IC)=ZERO
      FTXZ(IC)=ZERO
      FTYX(IC)=ZERO
      FTYY(IC)=ZERO
      FTYZ(IC)=ZERO
      FTZX(IC)=ZERO
      FTZY(IC)=ZERO
      FTZZ(IC)=ZERO
C
C     P is symmetric, and stored that way.
C
      NN=0
      DO 300 J=1,NUM
       DO 400 I=1,J
        NN=NN+1
C
        FTXX(IC) = FTXX(IC) + P(NN)*H11XX(I,J,IC)
        FTXY(IC) = FTXY(IC) + P(NN)*H11XY(I,J,IC)
        FTXZ(IC) = FTXZ(IC) + P(NN)*H11XZ(I,J,IC)
        FTYX(IC) = FTYX(IC) + P(NN)*H11YX(I,J,IC)
        FTYY(IC) = FTYY(IC) + P(NN)*H11YY(I,J,IC)
        FTYZ(IC) = FTYZ(IC) + P(NN)*H11YZ(I,J,IC)
        FTZX(IC) = FTZX(IC) + P(NN)*H11ZX(I,J,IC)
        FTZY(IC) = FTZY(IC) + P(NN)*H11ZY(I,J,IC)
        FTZZ(IC) = FTZZ(IC) + P(NN)*H11ZZ(I,J,IC)
C
        IF(I.EQ.J) GO TO 400
C
        FTXX(IC) = FTXX(IC) + P(NN)*H11XX(J,I,IC)
        FTXY(IC) = FTXY(IC) + P(NN)*H11XY(J,I,IC)
        FTXZ(IC) = FTXZ(IC) + P(NN)*H11XZ(J,I,IC)
        FTYX(IC) = FTYX(IC) + P(NN)*H11YX(J,I,IC)
        FTYY(IC) = FTYY(IC) + P(NN)*H11YY(J,I,IC)
        FTYZ(IC) = FTYZ(IC) + P(NN)*H11YZ(J,I,IC)
        FTZX(IC) = FTZX(IC) + P(NN)*H11ZX(J,I,IC)
        FTZY(IC) = FTZY(IC) + P(NN)*H11ZY(J,I,IC)
        FTZZ(IC) = FTZZ(IC) + P(NN)*H11ZZ(J,I,IC)
C
  400  CONTINUE
  300 CONTINUE
C
C     The factor of 1/2c**2: (c in atomic units: 1/fine structure const)
C                                ppm - parts per million
C
      FTXX(IC) = FTXX(IC)*C2PPM
      FTXY(IC) = FTXY(IC)*C2PPM
      FTXZ(IC) = FTXZ(IC)*C2PPM
      FTYX(IC) = FTYX(IC)*C2PPM
      FTYY(IC) = FTYY(IC)*C2PPM
      FTYZ(IC) = FTYZ(IC)*C2PPM
      FTZX(IC) = FTZX(IC)*C2PPM
      FTZY(IC) = FTZY(IC)*C2PPM
      FTZZ(IC) = FTZZ(IC)*C2PPM
C
      SISO = (FTXX(IC) + FTYY(IC) + FTZZ(IC))/3
C
      IF(PDIA) THEN
      WRITE(IW,9160)  IC,ANAM(IC),BNAM(IC),
     *                FTXX(IC),FTXY(IC),FTXZ(IC),
     *                FTYX(IC),FTYY(IC),FTYZ(IC),SISO,
     *                FTZX(IC),FTZY(IC),FTZZ(IC)
      END IF
C
  250 CONTINUE
C
      RETURN
C
 9100 FORMAT(/13X,'"DIAMAGNETIC" TERM OF SHIELDING TENSOR (PPM):',//
     *        26X,'X',13X,'Y',13X,'Z',9X,'ISOTROPIC',/64X,'SHIELDING')
 9160 FORMAT(1X,I4,1X,A8,A2,'X',F12.4,2X,F12.4,2X,F12.4,/,
     *       16X,'Y',F12.4,2X,F12.4,2X,F12.4,4X,F12.4,/,
     *       16X,'Z',F12.4,2X,F12.4,2X,F12.4,/)
 9998 FORMAT(/5X,'P(0) MATRIX FOR ALL NUCLEI:')
 9999 FORMAT(/5X,'H(1,1)',A1,A1,' MATRIX FOR NUCLEUS',I2,':')
      END
C*MODULE NMR  *DECK PARAMAG
      SUBROUTINE PARAMAG(H01X,H01Y,H01Z,P10X,P10Y,P10Z,
     *                   STXX,STXY,STXZ,STYX,STYY,STYZ,STZX,STZY,STZZ,
     *                   PRMAT,PPARA,L2S)
C
C     This is usually called the "paramagnetic" term.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL PRMAT, PPARA
C
      PARAMETER (MXATM=500)
C
      PARAMETER (ZERO=0.0D+00,C2PPM=+2.66257D+01,TWO=2.0D+00)
C
      DIMENSION H01X(L2S,NAT),H01Y(L2S,NAT),H01Z(L2S,NAT),
     *          P10X(L2S),P10Y(L2S),P10Z(L2S),
     *          STXX(NAT),STXY(NAT),STXZ(NAT),
     *          STYX(NAT),STYY(NAT),STYZ(NAT),
     *          STZX(NAT),STZY(NAT),STZZ(NAT)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /RUNLAB/ TITLE(10),ANAM(MXATM),BNAM(MXATM),BFLAB(2047)
C
      IF(PRMAT) THEN
       WRITE(IW,8998) 'X'
       CALL SPRTRIL(P10X,NUM)
       WRITE(IW,8998) 'Y'
       CALL SPRTRIL(P10Y,NUM)
       WRITE(IW,8998) 'Z'
       CALL SPRTRIL(P10Z,NUM)
       DO IC=1,NAT
        WRITE(IW,8999) 'X',IC
        CALL SNPRTRIL(H01X,NUM,NAT,IC)
        WRITE(IW,8999) 'Y',IC
        CALL SNPRTRIL(H01Y,NUM,NAT,IC)
        WRITE(IW,8999) 'Z',IC
        CALL SNPRTRIL(H01Z,NUM,NAT,IC)
        ENDDO
      END IF
C
      IF(PPARA) WRITE(IW,9100)
C
C     Do the sum over all i,j (nu,lambda)
C
      DO 250 IC=1,NAT
C
      STXX(IC)=ZERO
      STXY(IC)=ZERO
      STXZ(IC)=ZERO
      STYX(IC)=ZERO
      STYY(IC)=ZERO
      STYZ(IC)=ZERO
      STZX(IC)=ZERO
      STZY(IC)=ZERO
      STZZ(IC)=ZERO
C
C     P(1,0) and H(0,1) are anti-symmetric, and stored that way.
C
      NN=0
      DO 300 J=2,NUM
       DO 400 I=1,J-1
        NN=NN+1
C
        STXX(IC) = STXX(IC) + TWO*P10X(NN)*H01X(NN,IC)
        STXY(IC) = STXY(IC) + TWO*P10X(NN)*H01Y(NN,IC)
        STXZ(IC) = STXZ(IC) + TWO*P10X(NN)*H01Z(NN,IC)
        STYX(IC) = STYX(IC) + TWO*P10Y(NN)*H01X(NN,IC)
        STYY(IC) = STYY(IC) + TWO*P10Y(NN)*H01Y(NN,IC)
        STYZ(IC) = STYZ(IC) + TWO*P10Y(NN)*H01Z(NN,IC)
        STZX(IC) = STZX(IC) + TWO*P10Z(NN)*H01X(NN,IC)
        STZY(IC) = STZY(IC) + TWO*P10Z(NN)*H01Y(NN,IC)
        STZZ(IC) = STZZ(IC) + TWO*P10Z(NN)*H01Z(NN,IC)
C
  400  CONTINUE
  300 CONTINUE
C
C     The factor of 1/c**2:  (c in atomic units: 1/fine structure const)
C                                  parts per million - ppm
C     The 1/c comes from Eq. (3.38),  and the other 1/2c from the terms
C     that go into P(1,0).
C     Where's the negative sign?  See notes in routine SP10.
C
      STXX(IC) = STXX(IC)*C2PPM
      STXY(IC) = STXY(IC)*C2PPM
      STXZ(IC) = STXZ(IC)*C2PPM
      STYX(IC) = STYX(IC)*C2PPM
      STYY(IC) = STYY(IC)*C2PPM
      STYZ(IC) = STYZ(IC)*C2PPM
      STZX(IC) = STZX(IC)*C2PPM
      STZY(IC) = STZY(IC)*C2PPM
      STZZ(IC) = STZZ(IC)*C2PPM
C
      SISO = (STXX(IC) + STYY(IC) + STZZ(IC))/3
C
      IF(PPARA) THEN
      WRITE(IW,9160)  IC,ANAM(IC),BNAM(IC),
     *                STXX(IC),STXY(IC),STXZ(IC),
     *                STYX(IC),STYY(IC),STYZ(IC),SISO,
     *                STZX(IC),STZY(IC),STZZ(IC)
      END IF
C
  250 CONTINUE
C
      RETURN
C
 8998 FORMAT(/5X,'P(1,0)',A1,' MATRIX FOR ALL NUCLEI:')
 8999 FORMAT(/5X,'H(0,1)',A1,' MATRIX FOR NUCLEUS',I2,':')
 9100 FORMAT(/13X,'"PARAMAGNETIC" TERM OF SHIELDING TENSOR (PPM):',//
     *        26X,'X',13X,'Y',13X,'Z',9X,'ISOTROPIC',/64X,'SHIELDING')
 9160 FORMAT(1X,I4,1X,A8,A2,'X',F12.4,2X,F12.4,2X,F12.4,/,
     *       16X,'Y',F12.4,2X,F12.4,2X,F12.4,4X,F12.4,/,
     *       16X,'Z',F12.4,2X,F12.4,2X,F12.4,/)
C
      END
C*MODULE NMR  *DECK S10
      SUBROUTINE S10(S10X,S10Y,S10Z,SXM,SYM,SZM,
     *                 QXM,QYM,QZM,XIJM,YIJM,ZIJM,
     *                 PS1PX,PS1PY,PS1PZ,S,P,WRK,L2S)
C
C     This routine calculates the first order perturbation to the
C     overlap matrix, as found in Eq. (3.48) of MAF dissertation.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DBG
C
      PARAMETER (MXATM=500)
C
      DIMENSION QXM(L2S),QYM(L2S),QZM(L2S),
     *          XIJM(L2S),YIJM(L2S),ZIJM(L2S),
     *          S10X(L2S),S10Y(L2S),S10Z(L2S),
     *          SXM(L2S),SYM(L2S),SZM(L2S),
     *          PS1PX(L2S),PS1PY(L2S),PS1PZ(L2S),
     *          S((NUM*NUM+NUM)/2),WRK(NUM,NUM),
     *          P((NUM*NUM+NUM)/2)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
C     If true, then print debugging data for THIS routine.
C
      DBG=.FALSE.
C
C     Get overlap matrix from record 12
C
      L2=(NUM*NUM+NUM)/2
C
      CALL DAREAD(IDAF,IODA,S,L2,12,0)
C
C     S(1,0) matrices:  Be sure to skip the diagonal in the S matrix!
C
      NN=0
      MM=0
      DO 300 J=1,NUM
       DO 400 I=1,J
        MM=MM+1
        IF(I.EQ.J) GO TO 400
        NN=NN+1
C
         S10X(NN)=YIJM(NN)*SZM(NN)
     *                 - ZIJM(NN)*SYM(NN)
     *                 +  QXM(NN)* S(MM)
C
         S10Y(NN)=ZIJM(NN)*SXM(NN)
     *                 - XIJM(NN)*SZM(NN)
     *                 +  QYM(NN)* S(MM)
C
         S10Z(NN)=XIJM(NN)*SYM(NN)
     *                 - YIJM(NN)*SXM(NN)
     *                 +  QZM(NN)* S(MM)
C
  400  CONTINUE
  300 CONTINUE
C
C     Now transform using unperturbed density matrix: P(0)*S(1,0)a*P(0)
C
      CALL TFTRA(PS1PX,S10X,P,WRK,NUM,L2S,L2)
      CALL TFTRA(PS1PY,S10Y,P,WRK,NUM,L2S,L2)
      CALL TFTRA(PS1PZ,S10Z,P,WRK,NUM,L2S,L2)
C
      IF(DBG) THEN
      WRITE(IW,9999) 'X'
      CALL SPRTRIL(S10X,NUM)
      WRITE(IW,9999) 'Y'
      CALL SPRTRIL(S10Y,NUM)
      WRITE(IW,9999) 'Z'
      CALL SPRTRIL(S10Z,NUM)
      WRITE(IW,9998) 'X'
      CALL SPRTRIL(PS1PX,NUM)
      WRITE(IW,9998) 'Y'
      CALL SPRTRIL(PS1PY,NUM)
      WRITE(IW,9998) 'Z'
      CALL SPRTRIL(PS1PZ,NUM)
      END IF
C
      RETURN
 9998 FORMAT(/5X,'P(0)*S(1,0)',A1,'*P(0) MATRIX FOR ALL NUCLEI:')
 9999 FORMAT(/5X,'S(1,0)',A1,' MATRIX FOR ALL NUCLEI:')
      END
C*MODULE NMR  *DECK TFTRA
      SUBROUTINE TFTRA(PSP,S1,P,WRK,NUM,L2S,L2)
C
C     This multiplies P(0)*S(1,0)*P(0), where P(0) is symmetric,
C     and stored that way (upper triangular w/diagonal) and S(1,0) is
C     antisymmetric and stored as such (upper triangular w/o diagonal).
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DBG
C
      PARAMETER (ZERO=0.0D+00)
C
      DIMENSION PSP(L2S),S1(L2S),P(L2),WRK(NUM,NUM)
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
      DBG=.FALSE.
C
      IF(DBG) THEN
      WRITE(IW,*) 'MAF DENSITY TRIANGLE'
      CALL PRTRIL(P,NUM)
      END IF
C
C     First multiply S(1,0) [where only the off-diagonal elements are
C     stored in the array s1] by P(0) [where the usual upper triangular
C     is stored] into a full matrix wrk.  Remember s1 is ANTIsymmetric.
C
      DO 100 I=1,NUM
       II=((I-1)*(I-1)-(I-1))/2
      DO 200 J=1,NUM
       JJ=(J*J-J)/2
       WRK(I,J)=ZERO
C
       DO 300 K=1,NUM
         KK=((K-1)*(K-1)-(K-1))/2
C
         IF(I.EQ.K) GO TO 300
         IF(K.LT.I) THEN
           NN=II+K
         ELSE
           NN=KK+I
         END IF
         IF(K.LT.J) THEN
           MM=JJ+K
         ELSE IF(K.GE.J) THEN
           MM=(K*K-K)/2+J
         END IF
C
         IF(K.LT.I) THEN
           WRK(I,J)=WRK(I,J) - S1(NN)*P(MM)
         ELSE
           WRK(I,J)=WRK(I,J) + S1(NN)*P(MM)
         END IF
C
 300   CONTINUE
 200  CONTINUE
 100  CONTINUE
C
      IF(DBG) THEN
      WRITE(IW,*) 'MAF S*P'
      CALL PRSQL(WRK,NUM,NUM,NUM)
      END IF
C
C     Now multiply P(0) by S(1,0)P(0), and calculate only off-diagonal
C     elements, since P(0)S(1,0)P(0) must be anti-symmetric.
C
      NN=0
      DO 150 J=2,NUM
         DO 140 I=1,J-1
            II=(I*I-I)/2
            NN=NN+1
            PSP(NN)=ZERO
            DO K=1,NUM
               IF(K.LT.I) THEN
                 MM=II+K
               ELSE IF(K.GE.I) THEN
                 MM=(K*K-K)/2+I
               END IF
               PSP(NN)=PSP(NN)+P(MM)*WRK(K,J)
            ENDDO
  140    CONTINUE
  150 CONTINUE
C
      IF(DBG) CALL SPRTRIL(PSP,NUM)
C
      RETURN
      END
C*MODULE NMR  *DECK G10
      SUBROUTINE G10(TEINT0,TEINTA,TEINTB,TEINTC,TEINTD,TEWRK,QAM,
     *                  BIJM,CIJM,P,P0G10,L2S,NCART)
C
C     This calculates the first order perturbed G matrix
C     (integrals only) c.f. (3.49)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DBG
C
      PARAMETER (MXATM=500)
C
      DIMENSION TEINT0(NUM,NUM,NUM,NUM),TEINTA(NUM,NUM,NUM,NUM),
     *          TEINTB(NUM,NUM,NUM,NUM),TEINTC(NUM,NUM,NUM,NUM),
     *          TEINTD(NUM,NUM,NUM,NUM),TEWRK(NUM,NUM,NUM,NUM),
     *          QAM(L2S),BIJM(L2S),CIJM(L2S),
     *          P((NUM*NUM+NUM)/2),P0G10(L2S)
C
      PARAMETER (HALF=0.5D+00,ZERO=0.0D+00,ONE=1.0D+00)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
C     If dbg, print debugging data for THIS routine.
C
      DBG=.FALSE.
C
      CALL VCLR(P0G10,1,L2S)
C
C     Construct first order G matrix (integrals only) ncart-direction
C
C     Integrals are in 4-index array, but other matrices are
C     anti-symmetric in l2s storage...
C
      DO 100 I=1,NUM
       II=((I-1)*(I-1)-(I-1))/2
       DO 200 J=1,NUM
        JJ=((J-1)*(J-1)-(J-1))/2
        IF(I.EQ.J) THEN
          NN=1
          FACI=ZERO
        ELSE IF(I.LT.J) THEN
          NN=JJ+I
          FACI=ONE
        ELSE IF(I.GT.J) THEN
          NN=II+J
          FACI=-ONE
        END IF
        DO 300 K=1,NUM
         KK=((K-1)*(K-1)-(K-1))/2
         DO 400 L=1,NUM
C
          TEWRK(I,J,K,L)=ZERO
          IF(I.EQ.J.AND.K.EQ.L) GO TO 400
C
          LL=((L-1)*(L-1)-(L-1))/2
          IF(K.EQ.L) THEN
            MM=1
            FACK=ZERO
          ELSE IF(K.LT.L) THEN
            MM=LL+K
            FACK=ONE
          ELSE IF(K.GT.L) THEN
            MM=KK+L
            FACK=-ONE
          END IF
C
          TEWRK(I,J,K,L)=  FACI*(QAM(NN)*TEINT0(I,J,K,L)
     *                    + BIJM(NN)*TEINTA(I,J,K,L)
     *                    - CIJM(NN)*TEINTB(I,J,K,L))
     *                    + FACK*(QAM(MM)*TEINT0(I,J,K,L)
     *                    + BIJM(MM)*TEINTC(I,J,K,L)
     *                    - CIJM(MM)*TEINTD(I,J,K,L))
C
 400     CONTINUE
 300    CONTINUE
 200   CONTINUE
 100  CONTINUE
C
C     P is symmetric, l2 storage; p0g10 is antisymmetric, l2s storage.
C
      NN=0
      DO J=2,NUM
       DO I=1,J-1
        NN=NN+1
        MM=0
        DO K=1,NUM
         DO 500 L=1,K
          MM=MM+1
C
          P0G10(NN)=P0G10(NN)+P(MM)*
     *                          (TEWRK(I,J,K,L)-HALF*TEWRK(I,L,K,J))
C
          IF(K.EQ.L) GO TO 500
C
          P0G10(NN)=P0G10(NN)+P(MM)*
     *                          (TEWRK(I,J,L,K)-HALF*TEWRK(I,K,L,J))
C
 500     CONTINUE
C
        ENDDO
       ENDDO
      ENDDO
C
      IF(DBG) THEN
       IF(NCART.EQ.1) WRITE(IW,9999) 'X'
       IF(NCART.EQ.2) WRITE(IW,9999) 'Y'
       IF(NCART.EQ.3) WRITE(IW,9999) 'Z'
       CALL SPRTRIL(P0G10,NUM)
      END IF
C
      RETURN
 9999 FORMAT(/5X,'P(0)*G(1,0) AS INITIALLY CALCULATED, ',A1,
     *    ' DIRECTION:')
      END
C*MODULE NMR  *DECK G10DISC
      SUBROUTINE G10DISC(TEWRK,TEINT,QAM,BIJM,CIJM,P,P0G10,L2S,NCART,
     *                   XX,IX,NINTMX)
C
C     This calculates the first order perturbed G matrix (integrals
C     only) c.f. (3.49) using two electron integrals that have been
C     stored to disk.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DBG
C
      PARAMETER (MXATM=500)
C
      DIMENSION TEWRK(NUM,NUM,NUM,NUM),TEINT(NUM,NUM,NUM,NUM),
     *          QAM(L2S),BIJM(L2S),CIJM(L2S),
     *          P((NUM*NUM+NUM)/2),P0G10(L2S),XX(NINTMX),IX(NINTMX)
C
      PARAMETER (HALF=0.5D+00,ZERO=0.0D+00,ONE=1.0D+00)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
C     If dbg, print debugging data for THIS routine.
C
      DBG=.FALSE.
C
      CALL VCLR(P0G10,1,L2S)
C
C     Construct first order G matrix (integrals only) ncart-direction
C
      CALL INTGET(TEINT,XX,IX,NINTMX)      ! GET UNPERTURBED 2 E- INTS
      NTWOEI = 0
C
      CALL VCLR(TEWRK,1,NUM*NUM*NUM*NUM)
C
 450  CONTINUE
C
C     Grab the right integrals for this perturbation of
C     G(1,0) (x, y, or z) and for this pass through the
C     construction of G(1,0)
C
      NFTNMR=60
      IF(NCART.EQ.1) THEN
         IF(NTWOEI.EQ.1) CALL RDNMR1(NFTNMR+3,TEINT,XX,IX,NINTMX)
         IF(NTWOEI.EQ.2) CALL RDNMR1(NFTNMR+2,TEINT,XX,IX,NINTMX)
         IF(NTWOEI.EQ.3) CALL RDNMR1(NFTNMR+6,TEINT,XX,IX,NINTMX)
         IF(NTWOEI.EQ.4) CALL RDNMR1(NFTNMR+5,TEINT,XX,IX,NINTMX)
      ELSE IF(NCART.EQ.2) THEN
         IF(NTWOEI.EQ.1) CALL RDNMR1(NFTNMR+1,TEINT,XX,IX,NINTMX)
         IF(NTWOEI.EQ.2) CALL RDNMR1(NFTNMR+3,TEINT,XX,IX,NINTMX)
         IF(NTWOEI.EQ.3) CALL RDNMR1(NFTNMR+4,TEINT,XX,IX,NINTMX)
         IF(NTWOEI.EQ.4) CALL RDNMR1(NFTNMR+6,TEINT,XX,IX,NINTMX)
      ELSE IF(NCART.EQ.3) THEN
         IF(NTWOEI.EQ.1) CALL RDNMR1(NFTNMR+2,TEINT,XX,IX,NINTMX)
         IF(NTWOEI.EQ.2) CALL RDNMR1(NFTNMR+1,TEINT,XX,IX,NINTMX)
         IF(NTWOEI.EQ.3) CALL RDNMR1(NFTNMR+5,TEINT,XX,IX,NINTMX)
         IF(NTWOEI.EQ.4) CALL RDNMR1(NFTNMR+4,TEINT,XX,IX,NINTMX)
      END IF
C
C     write(6,*) 'ntwoei=',ntwoei
C     call pint(teint)
C
C     Integrals are in 4-index array, but other matrices are
C     anti-symmetric in l2s storage...
C
      DO 100 I=1,NUM
       II=((I-1)*(I-1)-(I-1))/2
       DO 200 J=1,NUM
        JJ=((J-1)*(J-1)-(J-1))/2
        IF(I.EQ.J) THEN
          NN=1
          FACI=ZERO
        ELSE IF(I.LT.J) THEN
          NN=JJ+I
          FACI=ONE
        ELSE IF(I.GT.J) THEN
          NN=II+J
          FACI=-ONE
        END IF
        DO 300 K=1,NUM
         KK=((K-1)*(K-1)-(K-1))/2
         DO 400 L=1,NUM
C
          IF(I.EQ.J.AND.K.EQ.L) GO TO 400
C
          LL=((L-1)*(L-1)-(L-1))/2
          IF(K.EQ.L) THEN
            MM=1
            FACK=ZERO
          ELSE IF(K.LT.L) THEN
            MM=LL+K
            FACK=ONE
          ELSE IF(K.GT.L) THEN
            MM=KK+L
            FACK=-ONE
          END IF
C
          IF(NTWOEI.EQ.0) THEN
           TEWRK(I,J,K,L)=  FACI*QAM(NN)*TEINT(I,J,K,L)
     *                    + FACK*QAM(MM)*TEINT(I,J,K,L)
          ELSE IF(NTWOEI.EQ.1) THEN
           TEWRK(I,J,K,L)=  TEWRK(I,J,K,L)
     *                    + FACI*BIJM(NN)*TEINT(I,J,K,L)
          ELSE IF(NTWOEI.EQ.2) THEN
           TEWRK(I,J,K,L)=  TEWRK(I,J,K,L)
     *                    - FACI*CIJM(NN)*TEINT(I,J,K,L)
          ELSE IF(NTWOEI.EQ.3) THEN
           TEWRK(I,J,K,L)=  TEWRK(I,J,K,L)
     *                    + FACK*BIJM(MM)*TEINT(I,J,K,L)
          ELSE IF(NTWOEI.EQ.4) THEN
           TEWRK(I,J,K,L)=  TEWRK(I,J,K,L)
     *                    - FACK*CIJM(MM)*TEINT(I,J,K,L)
          END IF
C
 400     CONTINUE
 300    CONTINUE
 200   CONTINUE
 100  CONTINUE
C
      NTWOEI = NTWOEI + 1
      IF(NTWOEI.LE.4) GO TO 450
C
C     P is symmetric, l2 storage; p0g10 is antisymmetric, l2s storage.
C
      NN=0
      DO J=2,NUM
       DO I=1,J-1
        NN=NN+1
        MM=0
        DO K=1,NUM
         DO 500 L=1,K
          MM=MM+1
C
          P0G10(NN)=P0G10(NN)+P(MM)*
     *                          (TEWRK(I,J,K,L)-HALF*TEWRK(I,L,K,J))
C
          IF(K.EQ.L) GO TO 500
C
          P0G10(NN)=P0G10(NN)+P(MM)*
     *                          (TEWRK(I,J,L,K)-HALF*TEWRK(I,K,L,J))
C
 500     CONTINUE
        ENDDO
       ENDDO
      ENDDO
C
      IF(DBG) THEN
       IF(NCART.EQ.1) WRITE(IW,9999) 'X'
       IF(NCART.EQ.2) WRITE(IW,9999) 'Y'
       IF(NCART.EQ.3) WRITE(IW,9999) 'Z'
       CALL SPRTRIL(P0G10,NUM)
      END IF
C
      RETURN
 9999 FORMAT(/5X,'P(0)*G(1,0) AS INITIALLY CALCULATED (DISK), ',A1,
     *    ' DIRECTION:')
      END
C*MODULE NMR  *DECK TWOINTSA
      SUBROUTINE TWOINTSA(TEINT0,TEINT1,TEINT2,TEINT3,TEINT4,TEINT5,
     *                   TEINT6,PTEINT,TEDBG,P,L2,XX,IX,NINTMX,
     *                   AIN,X,R)
C
C     This is a driver for two-electron integral retrieval/calculation.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL PTEINT, TEDBG
C
      PARAMETER (MXATM=500)
C
      DIMENSION TEINT0(NUM,NUM,NUM,NUM),TEINT1(NUM,NUM,NUM,NUM),
     *          TEINT2(NUM,NUM,NUM,NUM),TEINT3(NUM,NUM,NUM,NUM),
     *          TEINT4(NUM,NUM,NUM,NUM),TEINT5(NUM,NUM,NUM,NUM),
     *          TEINT6(NUM,NUM,NUM,NUM),P(L2),XX(NINTMX),IX(NINTMX),
     *          AIN(75,225,225),X(225,225),R(0:24,0:24,0:24,0:24)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
      CALL DAREAD(IDAF,IODA,P,L2,16,0)
C
      WRITE(IW,9998) ' ',' '
      CALL FLSHBF(IW)
      CALL INTGET(TEINT0,XX,IX,NINTMX)
C
      WRITE(IW,9999) '^',' '
      CALL FLSHBF(IW)
      CALL MDTWOEIA(TEDBG,PTEINT,TEINT1,TEINT2,TEINT3,AIN,X,R,1)
C
      WRITE(IW,9999) ' ','^'
      CALL FLSHBF(IW)
      CALL MDTWOEIA(TEDBG,PTEINT,TEINT4,TEINT5,TEINT6,AIN,X,R,2)
C
      RETURN
 9998 FORMAT(5X,'READING (',A1,'I J|',A1,'K L)',
     *          ' INTEGRALS INTO MEMORY...')
 9999 FORMAT(5X,'WORKING ON (',A1,'I J|',A1,'K L)',
     *          ' INTEGRALS...')
      END
C*MODULE NMR  *DECK TWOINTS
      SUBROUTINE TWOINTS(P0G10X,P0G10Y,P0G10Z,P,XIJM,YIJM,ZIJM,
     *                   QX,QY,QZ,L2,L2S,PTEINT,TEDBG,XX,IX,NINTMX,
     *                   AIN,X,R)
C
C     This is a driver for two-electron integral retrieval/calculation,
C     used when we don't want to store any two e- integrals to disk.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL PTEINT, TEDBG, DBG
C
      PARAMETER (MXATM=500)
C
      DIMENSION P0G10X(L2S),P0G10Y(L2S),P0G10Z(L2S),P(L2),
     *          XIJM(L2S),YIJM(L2S),ZIJM(L2S),
     *          QX(L2S),QY(L2S),QZ(L2S),XX(NINTMX),IX(NINTMX),
     *          AIN(75,225,225),X(225,225),R(0:24,0:24,0:24,0:24)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
      DBG=.FALSE.
C
      CALL DAREAD(IDAF,IODA,P,L2,16,0)
C
      CALL P0QG0(P0G10X,P0G10Y,P0G10Z,QX,QY,QZ,P,L2,L2S,IJK,0,NUM,
     *           XX,IX,NINTMX)
C
      WRITE(IW,9999) '^',' '
      CALL FLSHBF(IW)
      CALL MDTWOEI(TEDBG,PTEINT,P0G10X,P0G10Y,P0G10Z,P,XIJM,YIJM,ZIJM,
     *             AIN,X,R,L2,L2S,1)
C
      WRITE(IW,9999) ' ','^'
      CALL FLSHBF(IW)
      CALL MDTWOEI(TEDBG,PTEINT,P0G10X,P0G10Y,P0G10Z,P,XIJM,YIJM,ZIJM,
     *             AIN,X,R,L2,L2S,2)
C
      IF(DBG) THEN
         WRITE(IW,9997) 'X'
         CALL SPRTRIL(P0G10X,NUM)
         WRITE(IW,9997) 'Y'
         CALL SPRTRIL(P0G10Y,NUM)
         WRITE(IW,9997) 'Z'
         CALL SPRTRIL(P0G10Z,NUM)
      END IF
      RETURN
C
 9997 FORMAT(/5X,'P(0)*G(1,0) AS CALC. IN MDTWOEIA & P0QG0, ',A1,
     *    ' DIRECTION:')
 9999 FORMAT(5X,'WORKING ON (',A1,'I J|',A1,'K L)',
     *          ' INTEGRALS...')
      END
C*MODULE NMR  *DECK TWOINTSM
      SUBROUTINE TWOINTSM(TEINT0,TEWRK,BUFP,IX,PTEINT,TEDBG,AIN,X,R)
C
C     This is a driver for two e- integral retrieval/calculation, used
C     when we want to use Cartesian integrals, and write some to disk.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL PTEINT, TEDBG, PACK2E
C
      PARAMETER (MXATM=500)
C
      DIMENSION TEINT0(NUM,NUM,NUM,NUM),TEWRK(NUM,NUM,NUM,NUM),
     *          BUFP(NINTMX),IX(NINTMX),
     *          AIN(75,225,225),X(225,225),R(0:24,0:24,0:24,0:24)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
      WRITE(IW,9998) ' ',' '
C
      CALL INTGET(TEINT0,BUFP,IX,NINTMX)
C
      WRITE(IW,9999) 'X',' '
      CALL FLSHBF(IW)
      CALL MDTWOEIM(TEDBG,PTEINT,TEWRK,BUFP,IX,AIN,X,R,1)
C
      WRITE(IW,9999) 'Y',' '
      CALL FLSHBF(IW)
      CALL MDTWOEIM(TEDBG,PTEINT,TEWRK,BUFP,IX,AIN,X,R,2)
C
      WRITE(IW,9999) 'Z',' '
      CALL FLSHBF(IW)
      CALL MDTWOEIM(TEDBG,PTEINT,TEWRK,BUFP,IX,AIN,X,R,3)
C
      WRITE(IW,9999) ' ','X'
      CALL FLSHBF(IW)
      CALL MDTWOEIM(TEDBG,PTEINT,TEWRK,BUFP,IX,AIN,X,R,4)
C
      WRITE(IW,9999) ' ','Y'
      CALL FLSHBF(IW)
      CALL MDTWOEIM(TEDBG,PTEINT,TEWRK,BUFP,IX,AIN,X,R,5)
C
      WRITE(IW,9999) ' ','Z'
      CALL FLSHBF(IW)
      CALL MDTWOEIM(TEDBG,PTEINT,TEWRK,BUFP,IX,AIN,X,R,6)
C
      RETURN
 9998 FORMAT(5X,'READING (',A1,'I J|',A1,'K L) TWO-ELECTRON',
     *          ' INTEGRALS INTO MEMORY...')
 9999 FORMAT(5X,'WORKING ON (',A1,'I J|',A1,'K L) TWO-ELECTRON',
     *          ' INTEGRALS...')
      END
C*MODULE NMR  *DECK MDTWOEIM
      SUBROUTINE MDTWOEIM(TEDBG,PTEINT,TEINT,XX,IX,AIN,X,R,NCART)
C
C     Calculate Cartesian two-electron intgrals.  There are six cases:
C     ncart can be 0,1,2,3,4,5 or 6:
C       0 = unperturbed integral
C       1 = perturbed integral in x-direction, I shell.
C       2 = perturbed integral in y-direction, I shell.
C       3 = perturbed integral in z-direction, I shell.
C       4 = perturbed integral in x-direction, K shell.
C       5 = perturbed integral in y-direction, K shell.
C       6 = perturbed integral in z-direction, K shell.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DOUBLE, NORM, TEDBG, PTEINT, PACK2E, OUT
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      PARAMETER (ZERO=0.0D+00, SQRT3=1.73205080756888D+00,
     *           SQRT5=2.23606797749979D+00, SQRT7=2.64575131106459D+00,
     *           TPIFH=34.9868366552497D+00, ONE=1.0D+00,
     *           CUT=1.0D-10)
C
      DIMENSION TEINT(NUM,NUM,NUM,NUM),XX(NINTMX),IX(NINTMX),
     *          AIN(75,225,225),X(225,225),R(0:24,0:24,0:24,0:24)
      DIMENSION ES1(3),E1(1:3,0:12,0:6,-2:6),
     *          ES2(3),E2(1:3,0:12,0:6,-2:6),
     *          FOFT(0:24),DIJ(225),DKL(225)
C
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
C     tedbg: print detailed two-electron integral debugging info.
C     turn off simple printing of ints (pteint) when this is selected.
C
      IF(TEDBG) PTEINT=.FALSE.
      IF(TEDBG) WRITE(IW,9999)
C
      NORM=NORMP.NE.1 .OR. NORMF.NE.1
      IF(PTEINT.OR.TEDBG) WRITE(IW,*)
     *      'FOR THIS SET OF INTEGRALS, NCART=',NCART
C
      ICOUNT = 1
      NFTNMR = 60 + NCART
      IF(NCART.EQ.1) THEN
       CALL SEQOPN(NFTNMR,'NMRINT1','UNKNOWN',.FALSE.,'UNFORMATTED')
      ELSE IF(NCART.EQ.2) THEN
       CALL SEQOPN(NFTNMR,'NMRINT2','UNKNOWN',.FALSE.,'UNFORMATTED')
      ELSE IF(NCART.EQ.3) THEN
       CALL SEQOPN(NFTNMR,'NMRINT3','UNKNOWN',.FALSE.,'UNFORMATTED')
      ELSE IF(NCART.EQ.4) THEN
       CALL SEQOPN(NFTNMR,'NMRINT4','UNKNOWN',.FALSE.,'UNFORMATTED')
      ELSE IF(NCART.EQ.5) THEN
       CALL SEQOPN(NFTNMR,'NMRINT5','UNKNOWN',.FALSE.,'UNFORMATTED')
      ELSE IF(NCART.EQ.6) THEN
       CALL SEQOPN(NFTNMR,'NMRINT6','UNKNOWN',.FALSE.,'UNFORMATTED')
      END IF
C
C     ----- I SHELL
C
      DO 680 II = 1,NSHELL
C
         I = KATOM(II)
         XI = C(1,I)
         YI = C(2,I)
         ZI = C(3,I)
         I1 = KSTART(II)
         I2 = I1+KNG(II)-1
         MINI = KMIN(II)
         MAXI = KMAX(II)
         LOCI = KLOC(II)-MINI
C        +1 for the extra ang. mom. for first-order integrals
         LIT = KTYPE(II)+1
C
C     ----- J SHELL
C
         DO 660 JJ = 1,NSHELL
C
            J = KATOM(JJ)
            XJ = C(1,J)
            YJ = C(2,J)
            ZJ = C(3,J)
            J1 = KSTART(JJ)
            J2 = J1+KNG(JJ)-1
            MINJ = KMIN(JJ)
            MAXJ = KMAX(JJ)
            LOCJ = KLOC(JJ)-MINJ
            LJT = KTYPE(JJ)
C
            RX1= XI-XJ
            RY1= YI-YJ
            RZ1= ZI-ZJ
            RX12= RX1*RX1
            RY12= RY1*RY1
            RZ12= RZ1*RZ1
C
C     ----- K SHELL
C
      DO 720 KK = 1,NSHELL
C
         K = KATOM(KK)
         XK = C(1,K)
         YK = C(2,K)
         ZK = C(3,K)
         K1 = KSTART(KK)
         K2 = K1+KNG(KK)-1
         MINK = KMIN(KK)
         MAXK = KMAX(KK)
         LOCK = KLOC(KK)-MINK
C        +1 for the extra ang. mom. for first-order integrals
         LKT = KTYPE(KK)+1
C
C     ----- L SHELL
C
         DO 700 LL = 1,NSHELL
C
            L = KATOM(LL)
            XL = C(1,L)
            YL = C(2,L)
            ZL = C(3,L)
            L1 = KSTART(LL)
            L2 = L1+KNG(LL)-1
            MINL = KMIN(LL)
            MAXL = KMAX(LL)
            LOCL = KLOC(LL)-MINL
            LLT = KTYPE(LL)
C
            RX2= XK-XL
            RY2= YK-YL
            RZ2= ZK-ZL
            RX22= RX2*RX2
            RY22= RY2*RY2
            RZ22= RZ2*RZ2
C
C     ----- Begin McMurchie-Davidson code ------
C
            MMCLR=(MAXI-MINI+2)*(MAXJ-MINJ+2)
            NNCLR=(MAXK-MINK+2)*(MAXL-MINL+2)
            IKCLR=2*(LIT+1)*(LJT+1)
C
            DO NNTMP=1,NNCLR
            DO MMTMP=1,MMCLR
              X(MMTMP,NNTMP)=ZERO
            ENDDO
            ENDDO
C
            IF(TEDBG.OR.PTEINT) WRITE(IW,9998) II,JJ,KK,LL
C
C           Sum over primitives in I shell contraction
C
            JGMAX = J2
            DO 640 IG = I1,I2
C
            AI  = EX(IG)
            CSI = CS(IG)
            CPI = CP(IG)
            CDI = CD(IG)
            CFI = CF(IG)
            CGI = CG(IG)
C
C           Sum over primitives in J shell contraction
C
            DO 620 JG = J1,JGMAX
C
              AJ  = EX(JG)
              CSJ = CS(JG)
              CPJ = CP(JG)
              CDJ = CD(JG)
              CFJ = CF(JG)
              CGJ = CG(JG)
C
C             Clean out the recursion array E
C
              DO  L=1,3
               DO  M=0,8
                DO  N=0,4
                 DO  I=0,4
                  E1(L,M,N,I)=ZERO
                  E2(L,M,N,I)=ZERO
                 ENDDO
                ENDDO
               ENDDO
              ENDDO
C
C       Coefficients depend on this particular alpha(i) and alpha(j):
C       Return e1([x,y,z],ip,ia,ib) (The recurrance formulas.)
C
              CALL EAB(ES1,AI,AJ,RX12,RY12,RZ12)
C
              CALL RECURS(ES1,AI,AJ,RX1,RY1,RZ1,LIT,LJT,E1)
C
C       "Zero I" Still may be a better way...
C
              DO IKTMP=0,IKCLR
              DO MMTMP=0,MMCLR
              DO NNTMP=0,NNCLR
C
                AIN(IKTMP,MMTMP,NNTMP)=ZERO
C
              ENDDO
              ENDDO
              ENDDO
C
C             Sum over primitives in K shell contraction
C
              LGMAX = L2
              DO 600 KG = K1,K2
C
              AK  = EX(KG)
              CSK = CS(KG)
              CPK = CP(KG)
              CDK = CD(KG)
              CFK = CF(KG)
              CGK = CG(KG)
C
C             Sum over primitives in L shell contraction
C
              DO 580 LG = L1,LGMAX
C
                AL  = EX(LG)
                CSL = CS(LG)
                CPL = CP(LG)
                CDL = CD(LG)
                CFL = CF(LG)
                CGL = CG(LG)
C
C       Coefficients depend on this particular alpha(k) and alpha(l):
C       Return e2([x,y,z],ip,ia,ib) (The recurrance formulas.)
C
                CALL EAB(ES2,AK,AL,RX22,RY22,RZ22)
C
                CALL RECURS(ES2,AK,AL,RX2,RY2,RZ2,LKT,LLT,E2)
C
C       Compute lambda, fofj(T)
C
                AMBDA=TPIFH/((AI+AJ)*(AK+AL)*SQRT(AI+AJ+AK+AL))
C
                MXLIJT = LIT + LJT + LKT + LLT
C
                PX = (AI*XI+AJ*XJ)/(AI+AJ)
                PY = (AI*YI+AJ*YJ)/(AI+AJ)
                PZ = (AI*ZI+AJ*ZJ)/(AI+AJ)
C
                QX = (AK*XK+AL*XL)/(AK+AL)
                QY = (AK*YK+AL*YL)/(AK+AL)
                QZ = (AK*ZK+AL*ZL)/(AK+AL)
C
                PQX = PX - QX
                PQY = PY - QY
                PQZ = PZ - QZ
C
                ALPHA = ((AI+AJ)*(AK+AL))/(AI+AJ+AK+AL)
C
                T = ALPHA * (PQX**2 + PQY**2 + PQZ**2)
C
                CALL FOFTJ(MXLIJT,T,FOFT)
C
C               Compute R_nlm
C
                CALL RNLM(MXLIJT,PQX,PQY,PQZ,ALPHA,FOFT,R)
C
C               Compute primitive coefficients (loop over g, h)
C                                              (counted by nn)
C
                 DOUBLE=.FALSE.
                 NN = 0
                 DUM1 = ZERO
                 DUM2 = ZERO
                 LLMAX=MAXL
                 DO 560 KKK = MINK,MAXK
C
                    K17=LOCK+KKK
                    IF (KKK.EQ.1) DUM1=CSK
                    IF (KKK.EQ.2) DUM1=CPK
                    IF (KKK.EQ.5) DUM1=CDK
                    IF ((KKK.EQ. 8).AND.NORM) DUM1=DUM1*SQRT3
                    IF (KKK.EQ.11) DUM1=CFK
                    IF ((KKK.EQ.14).AND.NORM) DUM1=DUM1*SQRT5
                    IF ((KKK.EQ.20).AND.NORM) DUM1=DUM1*SQRT3
                    IF (KKK.EQ.21) DUM1=CGK
                    IF ((KKK.EQ.24).AND.NORM) DUM1=DUM1*SQRT7
                    IF ((KKK.EQ.30).AND.NORM) DUM1=DUM1*SQRT5/SQRT3
                    IF ((KKK.EQ.33).AND.NORM) DUM1=DUM1*SQRT3
C
                    CALL GETKLM(KKK,IC,JC,KC)
C
                       IF(NCART.EQ.4) THEN
                         IC=IC+1
                       ELSE IF(NCART.EQ.5) THEN
                         JC=JC+1
                       ELSE IF(NCART.EQ.6) THEN
                         KC=KC+1
                       END IF
C
                    DO 540 LLL = MINL,LLMAX
C
                       L17=LOCL+LLL
                       IF (LLL.EQ.1) THEN
                          DUM2=DUM1*CSL
                          IF (DOUBLE) THEN
                             IF (KKK.LE.1) THEN
                                DUM2=DUM2+DUM2
                             ELSE
                                DUM2=DUM2+CSK*CPL
                             END IF
                          END IF
                       ELSE IF (LLL.EQ.2) THEN
                          DUM2=DUM1*CPL
                          IF (DOUBLE) DUM2=DUM2+DUM2
                       ELSE IF (LLL.EQ.5) THEN
                          DUM2=DUM1*CDL
                          IF (DOUBLE) DUM2=DUM2+DUM2
                       ELSE IF ((LLL.EQ.8).AND.NORM) THEN
                          DUM2=DUM2*SQRT3
                       ELSE IF (LLL.EQ.11) THEN
                          DUM2=DUM1*CFL
                          IF (DOUBLE) DUM2=DUM2+DUM2
                       ELSE IF ((LLL.EQ.14).AND.NORM) THEN
                          DUM2=DUM2*SQRT5
                       ELSE IF ((LLL.EQ.20).AND.NORM) THEN
                          DUM2=DUM2*SQRT3
                       ELSE IF (LLL.EQ.21) THEN
                          DUM2=DUM1*CGL
                          IF (DOUBLE) DUM2=DUM2+DUM2
                       ELSE IF ((LLL.EQ.24).AND.NORM) THEN
                          DUM2=DUM2*SQRT7
                       ELSE IF ((LLL.EQ.30).AND.NORM) THEN
                          DUM2=DUM2*SQRT5/SQRT3
                       ELSE IF ((LLL.EQ.33).AND.NORM) THEN
                          DUM2=DUM2*SQRT3
                       END IF
C
                       CALL GETKLM(LLL,ID,JD,KD)
C
                       NN = NN+1
C
                       DKL(NN) = DUM2
C
                       NPMAX=IC+ID
                       LPMAX=JC+JD
                       MPMAX=KC+KD
C
                       MM=0
C
                       JMAX=MAXJ
                       DO 535 III=MINI,MAXI
                         I17=LOCI+III
                         CALL GETKLM(III,IA,JA,KA)
C
                       IF(NCART.EQ.1) THEN
                         IA=IA+1
                       ELSE IF(NCART.EQ.2) THEN
                         JA=JA+1
                       ELSE IF(NCART.EQ.3) THEN
                         KA=KA+1
                       END IF
C
                       DO 525 JJJ=MINJ,JMAX
                         J17=LOCJ+JJJ
                         CALL GETKLM(JJJ,IB,JB,KB)
C
                       MM=MM+1
C
                       IF(TEDBG) WRITE(IW,9997) IG,JG,KG,LG,
     *                                CSI,CSJ,CSK,CSL,
     *                  IA,JA,KA,IB,JB,KB,IC,JC,KC,ID,JD,KD,
     *                                CPI,CPJ,CPK,CPL
C
C
                       NMAX=IA+IB
                       LMAX=JA+JB
                       MMAX=KA+KB
C
C                      Sum over (N',L',M')k'
C
                       DO 520 NP=0,NPMAX
                         DO 510 LP=0,LPMAX
                           DO 500 MP=0,MPMAX
C
                             SIGN=(-ONE)**(NP+LP+MP)
C
C                            Loop over (N,L,M)k (ik)
C
                             IK=0
                             DO 480 N=0,NMAX
                             DO 460 L=0,LMAX
                             DO 440 M=0,MMAX
                             IK=IK+1
C
                             IF(TEDBG) WRITE(IW,9995) IK,MM,NN,
     *                       AIN(IK,MM,NN),
     *                       SIGN,AMBDA,NN,DKL(NN),
     *                       N+NP,L+LP,M+MP,R(N+NP,L+LP,M+MP,0),
     *                       NP,IC,ID,E2(1,NP,IC,ID),
     *                       LP,JC,JD,E2(2,LP,JC,JD),
     *                       MP,KC,KD,E2(3,MP,KC,KD)
C
                             ADDON=  SIGN *
     *                               AMBDA *
     *                               DKL(NN) *
     *                               E2(1,NP,IC,ID) *
     *                               E2(2,LP,JC,JD) *
     *                               E2(3,MP,KC,KD) *
     *                               R(N+NP,L+LP,M+MP,0)
C
                             AIN(IK,MM,NN) = AIN(IK,MM,NN)
     *                                       + ADDON
C
                             IF(TEDBG) WRITE(IW,9994) IK,MM,NN,
     *                           AIN(IK,MM,NN),ADDON
C
 440                         CONTINUE
 460                         CONTINUE
 480                         CONTINUE
C
 500                       CONTINUE
 510                     CONTINUE
 520                   CONTINUE
C
 525                   CONTINUE
 535                   CONTINUE
C
 540                CONTINUE
 560              CONTINUE
C                 end of loop over g, h
C
 580  CONTINUE
 600  CONTINUE
C
C     end of sum over alpha_C, alpha_D (580 & 600)
C
C     loop over g, h for second time
C
      NN=0
C
      LLMAX=MAXL
      DO 420 KKK = MINK,MAXK
C
         K17=LOCK+KKK
C
         DO 400 LLL = MINL,LLMAX
C
            L17=LOCL+LLL
C
            NN=NN+1
C
C           loop over i, j
C
            DOUBLE=.FALSE.
            MM = 0
            DUM1 = ZERO
            DUM2 = ZERO
            JMAX=MAXJ
            DO 380 III = MINI,MAXI
C
               I17=LOCI+III
               IF (III.EQ. 1) DUM1=CSI
               IF (III.EQ. 2) DUM1=CPI
               IF (III.EQ. 5) DUM1=CDI
               IF((III.EQ. 8).AND.NORM) DUM1=DUM1*SQRT3
               IF (III.EQ.11) DUM1=CFI
               IF((III.EQ.14).AND.NORM) DUM1=DUM1*SQRT5
               IF((III.EQ.20).AND.NORM) DUM1=DUM1*SQRT3
               IF (III.EQ.21) DUM1=CGI
               IF((III.EQ.24).AND.NORM) DUM1=DUM1*SQRT7
               IF((III.EQ.30).AND.NORM) DUM1=DUM1*SQRT5/SQRT3
               IF((III.EQ.33).AND.NORM) DUM1=DUM1*SQRT3
C
               CALL GETKLM(III,IA,JA,KA)
C
                  IF(NCART.EQ.1) THEN
                    IA=IA+1
                  ELSE IF(NCART.EQ.2) THEN
                    JA=JA+1
                  ELSE IF(NCART.EQ.3) THEN
                    KA=KA+1
                  END IF
C
               DO 360 JJJ = MINJ,JMAX
C
                  J17=LOCJ+JJJ
C
                  IF (JJJ.EQ.1) THEN
                     DUM2=DUM1*CSJ
                     IF (DOUBLE) THEN
                        IF (III.LE.1) THEN
                           DUM2=DUM2+DUM2
                        ELSE
                           DUM2=DUM2+CSI*CPJ
                        END IF
                     END IF
                  ELSE IF (JJJ.EQ.2) THEN
                     DUM2=DUM1*CPJ
                     IF (DOUBLE) DUM2=DUM2+DUM2
                  ELSE IF (JJJ.EQ.5) THEN
                     DUM2=DUM1*CDJ
                     IF (DOUBLE) DUM2=DUM2+DUM2
                  ELSE IF ((JJJ.EQ.8).AND.NORM) THEN
                     DUM2=DUM2*SQRT3
                  ELSE IF (JJJ.EQ.11) THEN
                     DUM2=DUM1*CFJ
                     IF (DOUBLE) DUM2=DUM2+DUM2
                  ELSE IF ((JJJ.EQ.14).AND.NORM) THEN
                     DUM2=DUM2*SQRT5
                  ELSE IF ((JJJ.EQ.20).AND.NORM) THEN
                     DUM2=DUM2*SQRT3
                  ELSE IF (JJJ.EQ.21) THEN
                     DUM2=DUM1*CGJ
                     IF (DOUBLE) DUM2=DUM2+DUM2
                  ELSE IF ((JJJ.EQ.24).AND.NORM) THEN
                     DUM2=DUM2*SQRT7
                  ELSE IF ((JJJ.EQ.30).AND.NORM) THEN
                     DUM2=DUM2*SQRT5/SQRT3
                  ELSE IF ((JJJ.EQ.33).AND.NORM) THEN
                     DUM2=DUM2*SQRT3
                  END IF
C
                  CALL GETKLM(JJJ,IB,JB,KB)
C
                  IF(TEDBG) WRITE(IW,9993) IG,JG,I17,J17,K17,L17
C
                  MM = MM+1
C
                  DIJ(MM) = DUM2
C
                  NMAX=IA+IB
                  LMAX=JA+JB
                  MMAX=KA+KB
C
C                 Loop over (N,L,M)k
C
                  IK=0
                  DO 340 N=0,NMAX
                  DO 320 L=0,LMAX
                  DO 300 M=0,MMAX
                  IK=IK+1
C
                   IF(TEDBG) WRITE(IW,9992) MM,NN,X(MM,NN),
     *             IK,MM,NN,AIN(IK,MM,NN),MM,DIJ(MM),
     *             N,IA,IB,E1(1,N,IA,IB),
     *             L,JA,JB,E1(2,L,JA,JB),
     *             M,KA,KB,E1(3,M,KA,KB)
C
                   ADDON=DIJ(MM) *
     *                   E1(1,N,IA,IB) *
     *                   E1(2,L,JA,JB) *
     *                   E1(3,M,KA,KB) *
     *                   AIN(IK,MM,NN)
C
                   X(MM,NN)=X(MM,NN)+ADDON
C
                   IF(TEDBG) WRITE(IW,9991) MM,NN,X(MM,NN),ADDON
C
 300              CONTINUE
 320              CONTINUE
 340              CONTINUE
C
                  TEINT(I17,J17,K17,L17)=X(MM,NN)
C
                  CALL NMRQOUT(I17,J17,K17,L17,XX,IX,NINTMX,X(MM,NN),
     *                         NFTNMR)
C
 360          CONTINUE
 380       CONTINUE
C          end if loop over i, j
C
 400    CONTINUE
 420  CONTINUE
C     end of loop over g, h
C
 620  CONTINUE
 640  CONTINUE
C     end of sum over alpha_A, alpha_B (640 & 620)
C
      IF (PTEINT) THEN
      LLMAX=MAXL
      DO KKK = MINK,MAXK
       K17=LOCK+KKK
      DO LLL = MINL,LLMAX
       L17=LOCL+LLL
       JMAX=MAXJ
      DO III = MINI,MAXI
       I17=LOCI+III
      DO JJJ = MINJ,JMAX
       J17=LOCJ+JJJ
C
       IF(ABS(TEINT(I17,J17,K17,L17)).GT.CUT)
     *  WRITE(IW,9989) I17,J17,K17,L17,TEINT(I17,J17,K17,L17)
C
      ENDDO
      ENDDO
      ENDDO
      ENDDO
      END IF
C
 700  CONTINUE
 720  CONTINUE
 660  CONTINUE
 680  CONTINUE
C
C     Write the last few integrals to disk
C
      NXX = ICOUNT-1
      NXX = -NXX
      CALL PWRIT (NFTNMR,XX,IX,NXX,NINTMX)
C
      RETURN
 9989 FORMAT(7X,'(',I3,I3,'|',I3,I3,')',F25.15)
 9991 FORMAT(7X,'OUTGOING X(',I2,',',I2,')=',F10.5,5X,'THIS TERM:',
     *       F15.10/)
 9992 FORMAT(7X,'INCOMING X(',I2,',',I2,')=',F10.5,11X,'I(',
     *       I2,',',I2,',',I2,')=',F10.5,5X,'DIJ(',I2,')=',F10.5/
     *       12X,'EX(',I2,',',I2,',',I2,')=',F10.5,
     *       3X,'EY(',I2,',',I2,',',I2,')=',F10.5,
     *       3X,'EZ(',I2,',',I2,',',I2,')=',F10.5)
 9993 FORMAT(4X,'***AFTER TERMS INVOLVING (',I2,I2,'|:',5X,
     *       'AO INTEGRAL: (',I2,I2,'|',I2,I2,')')
 9994 FORMAT(10X,'OUTGOING I(',I2,',',I2,',',I2,')=',F10.5,6X,
     *       'THIS TERM=',F10.5/)
 9995 FORMAT(10X,'INCOMING I(',I2,',',I2,',',I2,')=',F10.5,11X,
     *       'SIGN=',F10.5,9X,'LAMBDA=',F10.5,8X,'DKL(',I2,')=',F10.5/
     *       16X,'R(',I2,',',I2,',',I2,',',' 0)=',F10.5,
     *       3X,'EX(',I2,',',I2,',',I2,')=',F10.5,
     *       3X,'EY(',I2,',',I2,',',I2,')=',F10.5,
     *       3X,'EZ(',I2,',',I2,',',I2,')=',F10.5)
 9997 FORMAT(/5X,'PRIMITIVE TERM:',1X,'(',I2,I2,'|',I2,I2,')',5X,
     *           'S COEFICIENTS:',1X,4F10.5,
     *       /6X,'(',I2,I2,I2,1X,I2,I2,I2,'|',I2,I2,I2,1X,I2,I2,I2,')',
     *        2X,'P COEFICIENTS:',1X,4F10.5)
 9998 FORMAT(/,2X,'SHELLS:',1X,I2,I2,I2,I2/)
 9999 FORMAT(/,3X,'*****DEBUG OUTPUT FOR MCM-DAV 2E INTEGRALS*****')
      END
C*MODULE NMR  *DECK SF10
      SUBROUTINE SF10(P0G10,P10I,H10,F10,TEINT0,ITER,NCART,DBG,L2S)
C
C     This routine calculates the first order Fock matrix in any
C     direction.  See Eq. (3.49), MAF thesis.  If you're not writing
C     anything to disk, use routine P10G0 instead.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DBG
C
      PARAMETER (MXATM=500)
C
      DIMENSION F10(L2S),P0G10(L2S),P10I(L2S),H10(L2S),
     *          TEINT0(NUM,NUM,NUM,NUM)
C
      PARAMETER (HALF=0.5D+00)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
      IF(DBG) THEN
       IF(NCART.EQ.1) WRITE(IW,9995) ITER, 'X'
       IF(NCART.EQ.2) WRITE(IW,9995) ITER, 'Y'
       IF(NCART.EQ.3) WRITE(IW,9995) ITER, 'Z'
      END IF
C
      CALL VCLR(F10,1,L2S)
C
      NN=0
      DO 100 I=2,NUM
       DO 200 J=1,I-1
        NN=NN+1
        DO 300 L=1,NUM
         LL=L-1
         DO 400 K=1,NUM
          KK=K-1
C
          IF(L.EQ.K) THEN
            GO TO 400
          ELSE IF(L.LT.K) THEN
            MM=(KK*KK-KK)/2+L
            F10(NN)=F10(NN)+P10I(MM)*(TEINT0(I,J,K,L)
     *                                   -HALF*TEINT0(I,L,K,J))
          ELSE IF(L.GE.K) THEN
            MM=(LL*LL-LL)/2+K
            F10(NN)=F10(NN)-P10I(MM)*(TEINT0(I,J,K,L)
     *                                   -HALF*TEINT0(I,L,K,J))
          END IF
C
 400     CONTINUE
 300    CONTINUE
C
        F10(NN)=H10(NN)+F10(NN)+P0G10(NN)
C
 200   CONTINUE
 100  CONTINUE
C
      IF(DBG) THEN
       WRITE(IW,9996)
       CALL SPRTRIL(H10,NUM)
       WRITE(IW,9998)
       CALL SPRTRIL(P10I,NUM)
       WRITE(IW,9997)
       CALL SPRTRIL(P0G10,NUM)
       WRITE(IW,9999)
       CALL SPRTRIL(F10,NUM)
      END IF
C
      RETURN
 9995 FORMAT(/1X,'----------DEBUG OUTPUT FOR ITERATION',I3,
     *  ' PERTURBATION IN ',A1,' DIRECTION----------')
 9996 FORMAT(/5X,'H(1,0) MATRIX USED TO CALCULATE NEW F(1,0):')
 9997 FORMAT(/5X,'P(0)*G(1,0) MATRIX USED TO CALCULATE NEW F(1,0):')
 9998 FORMAT(/5X,'P(1,0) MATRIX USED TO CALCULATE NEW F(1,0):')
 9999 FORMAT(/5X,'NEW F(1,0) MATRIX:')
      END
C*MODULE NMR  *DECK SP10
      SUBROUTINE SP10(F10M,S10,P10I,EVEC,DUM,PS1P,E,CK,CL,CDUM,
     *                  P10M,DBG,L2S)
C
C     This routine calculates the first order Density matrix in any
C     direction.  Eq. (3.65)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DBG
C
      PARAMETER (MXATM=500)
C
      DIMENSION F10M(L2S),EVEC(NUM,NUM),P10I(L2S),DUM(L2S),
     *          S10(L2S),E(NUM),CK(NUM),CL(NUM),PS1P(L2S),
     *          CDUM(NUM),P10M(L2S)
C
      PARAMETER (HALF=0.5D+00,TWO=2.0D+00,ZERO=0.0D+00)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
      L3 = NUM*NUM
C
      CALL VCLR(P10M,1,L2S)
C
C     Get unperturbed vectors and orbital energies
C
      CALL DAREAD(IDAF,IODA,EVEC,L3,15,0)
      CALL DAREAD(IDAF,IODA,E,NUM,17,0)
C
      NOCC=NE/2
      DO K=1,NOCC
        DO L=NOCC+1,NUM
C
           NN=0
           DO I=1,NUM
             CK(I)=EVEC(I,K)
             CL(I)=EVEC(I,L)
             IF(I.GT.1) THEN
               DO J=1,I-1
                 NN=NN+1
                 DUM(NN)=F10M(NN) - E(K)*S10(NN)
               ENDDO
             END IF
           ENDDO
C
C          Multiply F(1,0)-eS(1,0), which is antisymmetric, by cL.
C
           DO 100 I=1,NUM
             II=I-1
             CDUM(I)=ZERO
             DO 200 J=1,NUM
               JJ=J-1
               IF(I.EQ.J) THEN
                 GO TO 200
               ELSE IF(I.LT.J) THEN
                 MM=(JJ*JJ-JJ)/2+I
                 CDUM(I)=CDUM(I) + DUM(MM)*CL(J)
               ELSE IF(I.GE.J) THEN
                 MM=(II*II-II)/2+J
                 CDUM(I)=CDUM(I) - DUM(MM)*CL(J)
               END IF
 200         CONTINUE
 100       CONTINUE
C
           CALL MRTRBR(CK,NUM,NUM,1,CDUM,NUM,1,CFESC,1)
           CFESC=CFESC/(E(K)-E(L))
C
           NN=0
           DO J=2,NUM
            DO I=1,J-1
             NN=NN+1
             P10I(NN)=CK(I)*CL(J)-CL(I)*CK(J)
             P10M(NN)=P10M(NN)+CFESC*P10I(NN)
            ENDDO
           ENDDO
C
        ENDDO
      ENDDO
C
C     Following the usual convention, we multiply the negative
C     sign from the H(0,1) term into P(1,0) here.  This results in
C     a different result, since P(1,0)=f[P(1,0)].  In essence,
C     the "paramagnetic" term is iterated as a whole, not just the
C     first-order density matrix by itself.  Personally, I don't
C     think this is correct, but as the basis set gets large, it
C     doesn't matter.
C
      NN=0
      DO J=2,NUM
       DO I=1,J-1
         NN=NN+1
C
         P10M(NN) =   HALF*PS1P(NN) - TWO*P10M(NN)
C
       ENDDO
      ENDDO
C
      IF (DBG) THEN
       WRITE(IW,9999)
       CALL SPRTRIL(P10M,NUM)
      END IF
C
      RETURN
 9999 FORMAT(/5X,'NEW P(1,0) MATRIX:')
      END
C*MODULE NMR  *DECK P10ITER
      SUBROUTINE P10ITER(P0G10,P10I,H10,F10M,TEINT0,S10,DUM,DUM3,PS1P,
     *                  E,CK,CL,CDUM,P10M,P10F,NCART,PITER,L2S,
     *                  XX,IX,NINTMX)
C
C     This routine iterates on the first order density matrix.
C     ncart is 1, 2 or 3 for x, y, or z perturbation.  Used when keeping
C     large arrays in memory.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DBG,PITER,CVGING
C
      PARAMETER (MXATM=500,CUTOFF=1.0D-07,TWO=2.0D+00,ZERO=0.00D+00)
C
      DIMENSION F10M(L2S),P0G10(L2S),P10I(L2S),P10M(L2S),
     *          H10(L2S),S10(L2S),PS1P(L2S),P10F(L2S),
     *          E(NUM),CK(NUM),CL(NUM),CDUM(NUM),
     *          TEINT0(NUM,NUM,NUM,NUM),DUM3(NUM,NUM),DUM(L2S),
     *          XX(NINTMX),IX(NINTMX)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
C     If dbg, print debugging data for entire iterative process.
C
      DBG=.FALSE.
C
C     For use when integrals read from disk!
      CALL INTGET(TEINT0,XX,IX,NINTMX)
C
      ITER=0
      DIFFPR=ZERO
C
      IF(PITER) THEN
        IF(NCART.EQ.1) WRITE(IW,9994) 'X'
        IF(NCART.EQ.2) WRITE(IW,9994) 'Y'
        IF(NCART.EQ.3) WRITE(IW,9994) 'Z'
        WRITE(IW,9998)
      END IF
C
C     First time around, use null matrix as starting guess for P(1,0)
C
      CALL VCLR(P10I,1,L2S)
      CALL VCLR(P10F,1,L2S)
C
 100  CONTINUE
C
      CALL SF10(P0G10,P10I,H10,F10M,TEINT0,ITER,NCART,DBG,L2S)
      CALL SP10(F10M,S10,P10I,DUM3,DUM,PS1P,E,CK,CL,CDUM,P10M,DBG,L2S)
      CALL DDIFF(P10M,P10F,L2S,DIFF)
C
      CALL DCOPY(L2S,P10M,1,P10I,1)
      CALL DCOPY(L2S,P10M,1,P10F,1)
C
      IF(PITER) WRITE(IW,9999) ITER,DIFF
      ITER=ITER+1
C
      CVGING = DIFF.LT.(TWO*DIFFPR)
      DIFFPR = DIFF
      IF(.NOT.(ITER.LE.50.OR.(CVGING.AND.ITER.LE.200))) THEN
        WRITE(IW,9993)
        CALL ABRT
      END IF
C
      IF (DIFF.GT.CUTOFF) GO TO 100
C
      RETURN
C
 9993 FORMAT(/5X,'TOO MANY ITERATIONS IN P10ITER - STOP.',
     *       /5X,'RHF MAY NOT BE SUITABLE FOR THIS MOLECULE;',
     *       /5X,'USE PITER=.T. IN $NMR TO SEE ITERATIONS.'/)
 9994 FORMAT(/4X,'FOR THE P(1,0)',A1,' MATRIX:')
 9998 FORMAT(5X,'ITER',7X,'LARGEST ABSOLUTE CHANGE IN MATRIX')
 9999 FORMAT(5X,I4,17X,F10.6)
      END
C*MODULE NMR  *DECK P10ITE
      SUBROUTINE P10ITE(P0G10,P10I,H10,F10M,S10,DUM,DUM3,PS1P,
     *                  E,CK,CL,CDUM,P10M,P10F,NCART,PITER,L2S,
     *                  XX,IX,NINTMX)
C
C     This routine iterates on the first order density matrix.
C     ncart is 1, 2 or 3 for x, y, or z perturbation.  Used when
C     not storing large arrays.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DBG,PITER,CVGING
C
      PARAMETER (MXATM=500,CUTOFF=1.0D-07,TWO=2.0D+00,ZERO=0.00D+00)
C
      DIMENSION F10M(L2S),P0G10(L2S),P10I(L2S),P10M(L2S),
     *          H10(L2S),S10(L2S),PS1P(L2S),P10F(L2S),
     *          E(NUM),CK(NUM),CL(NUM),CDUM(NUM),
     *          DUM3(NUM,NUM),DUM(L2S),XX(NINTMX),IX(NINTMX)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
C     If dbg, print debugging data for entire iterative process.
C
      DBG=.FALSE.
C
      ITER=0
      DIFFPR=ZERO
C
      IF(PITER) THEN
        IF(NCART.EQ.1) WRITE(IW,9994) 'X'
        IF(NCART.EQ.2) WRITE(IW,9994) 'Y'
        IF(NCART.EQ.3) WRITE(IW,9994) 'Z'
        WRITE(IW,9998)
      END IF
C
C     First time around, use null matrix as starting guess for P(1,0)
C
      CALL VCLR(P10I,1,L2S)
      CALL VCLR(P10F,1,L2S)
C
 100  CONTINUE
C
      CALL P10G0(F10M,P10I,P0G10,H10,L2S,IJK,0,NUM,XX,IX,NINTMX)
      CALL SP10(F10M,S10,P10I,DUM3,DUM,PS1P,E,CK,CL,CDUM,P10M,DBG,L2S)
      CALL DDIFF(P10M,P10F,L2S,DIFF)
C
      CALL DCOPY(L2S,P10M,1,P10I,1)
      CALL DCOPY(L2S,P10M,1,P10F,1)
C
      IF(PITER) WRITE(IW,9999) ITER,DIFF
      ITER=ITER+1
C
      CVGING = DIFF.LT.(TWO*DIFFPR)
      DIFFPR = DIFF
      IF(.NOT.(ITER.LE.50.OR.(CVGING.AND.ITER.LE.200))) THEN
        WRITE(IW,9993)
        CALL ABRT
      END IF
C
      IF (DIFF.GT.CUTOFF) GO TO 100
C
      RETURN
C
 9993 FORMAT(/5X,'TOO MANY ITERATIONS IN P10ITE - STOP.',
     *       /5X,'RHF MAY NOT BE SUITABLE FOR THIS MOLECULE;',
     *       /5X,'USE PITER=.T. IN $NMR TO SEE ITERATIONS.'/)
 9994 FORMAT(/4X,'FOR THE P(1,0)',A1,' MATRIX:')
 9998 FORMAT(5X,'ITER',7X,'LARGEST ABSOLUTE CHANGE IN MATRIX')
 9999 FORMAT(5X,I4,17X,F10.6)
      END
C*MODULE NMR  *DECK CHKSYM
C---      subroutine chksym(teint,pm,dum)
C---
C---c     This is a debugging routine that checks the symmetry of the
C---c     two-electron integrals.  It can probably be deleted, but if
C---c     someone wants to teach the two-electron McM-D routines neat
C---c     symmetry tricks, it might be useful.
C---
C---      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C---
C---      PARAMETER (MXATM=500)
C---      parameter (one=1.00D+00)
C---
C---      dimension teint(num,num,num,num),pm(num,num),dum(num,num)
C---
C---      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
C---     *                ZAN(MXATM),C(3,MXATM)
C---      COMMON /IOFILE/ IR,IW,IP,ijk,ijkt,IDAF,NAV,IODA(400)
C---
C---      call vclr(dum,1,num*num)
C---
C---      do i=1,num
C---       do j=1,num
C---        do k=1,num
C---         do l=1,num
C---
C---          dum(i,j)=dum(i,j)+one*teint(i,j,k,l)
C---          dum(i,j)=dum(i,j)+pm(k,l)*teint(i,j,k,l)
C---
C---         enddo
C---        enddo
C---       enddo
C---      enddo
C---
C---      write(iw,*) ' '
C---      call prsql(dum,num,num,num)
C---
C---      return
C---      end
C*MODULE NMR  *DECK DBGOUT
C---      subroutine dbgout(f10x,f10y,f10z,p0g10x,p0g10y,p0g10z)
C---
C---c     Debugging routine - prints out important matrices.
C---c     Could be deleted.
C---
C---      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C---
C---      PARAMETER (MXATM=500)
C---
C---      dimension f10x(num,num),f10y(num,num),f10z(num,num),
C---     *          p0g10x(num,num),p0g10y(num,num),p0g10z(num,num)
C---
C---      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
C---     *                ZAN(MXATM),C(3,MXATM)
C---      COMMON /IOFILE/ IR,IW,IP,ijk,ijkt,IDAF,NAV,IODA(400)
C---
C---      write(iw,9999) 'x'
C---      call prsql(f10x,num,num,num)
C---
C---      write(iw,9999) 'y'
C---      call prsql(f10y,num,num,num)
C---
C---      write(iw,9999) 'z'
C---      call prsql(f10z,num,num,num)
C---
C---      write(iw,9998) 'x'
C---      call prsql(p0g10x,num,num,num)
C---
C---      write(iw,9998) 'y'
C---      call prsql(p0g10y,num,num,num)
C---
C---      write(iw,9998) 'z'
C---      call prsql(p0g10z,num,num,num)
C---
C---      return
C---
C--- 9998 format(/5x,'P(0)*G(1,0)',a1,' MATRIX FOR ALL NUCLEI:')
C--- 9999 format(/5x,'F(1,0)',a1,' MATRIX FOR ALL NUCLEI AFTER ALL',
C---     *  ' ITERATIONS:')
C---      end
C*MODULE NMR  *DECK SHFTTEN
      SUBROUTINE SHFTTEN(FTXX,FTXY,FTXZ,FTYX,FTYY,FTYZ,FTZX,FTZY,FTZZ,
     *                   STXX,STXY,STXZ,STYX,STYY,STYZ,STZX,STZY,STZZ,
     *                   PEVEC)
C
C     Print full shielding tensor from diamagnetic + paramagnetic terms
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL PEVEC
C
      PARAMETER (MXATM=500, THREE=3.0D+00, HALF=0.5D+00)
C
      DIMENSION FTXX(NAT),FTXY(NAT),FTXZ(NAT),
     *          FTYX(NAT),FTYY(NAT),FTYZ(NAT),
     *          FTZX(NAT),FTZY(NAT),FTZZ(NAT),
     *          STXX(NAT),STXY(NAT),STXZ(NAT),
     *          STYX(NAT),STYY(NAT),STYZ(NAT),
     *          STZX(NAT),STZY(NAT),STZZ(NAT),
     *          SHTEN(3,3),WR(3),WI(3),Z(3,3),IV1(3),FV1(3)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /RUNLAB/ TITLE(10),ANAM(MXATM),BNAM(MXATM),BFLAB(2047)
C
      WRITE(IW,9100)
C
C     Add the first and second terms for all nuclei
C
      DO 250 IC=1,NAT
C
      SHTEN(1,1)=FTXX(IC)+STXX(IC)
      SHTEN(1,2)=FTXY(IC)+STXY(IC)
      SHTEN(1,3)=FTXZ(IC)+STXZ(IC)
      SHTEN(2,1)=FTYX(IC)+STYX(IC)
      SHTEN(2,2)=FTYY(IC)+STYY(IC)
      SHTEN(2,3)=FTYZ(IC)+STYZ(IC)
      SHTEN(3,1)=FTZX(IC)+STZX(IC)
      SHTEN(3,2)=FTZY(IC)+STZY(IC)
      SHTEN(3,3)=FTZZ(IC)+STZZ(IC)
C
      SISO=(SHTEN(1,1)+SHTEN(2,2)+SHTEN(3,3))/THREE
C
      WRITE(IW,9160)  IC,ANAM(IC),BNAM(IC),
     *                SHTEN(1,1),SHTEN(1,2),SHTEN(1,3),
     *                SHTEN(2,1),SHTEN(2,2),SHTEN(2,3),
     *                SHTEN(3,1),SHTEN(3,2),SHTEN(3,3)
      WRITE(IW,9030)  SISO
C
C     Form Hessenberg matrix from shielding tensor and diagonalize.
C     Here I use the existing GAMESS routines for diagonalizing
C     NON-SYMMETRIC matrices.
C     added later:  In 2003, DGEEV was added to GAMESS, so it could
C     be used in an attempt to get more accuracy than RG, if needed.
C
      NVEC=0
      IF(PEVEC)NVEC=1
C
      CALL RG(3,3,SHTEN,WR,WI,NVEC,Z,IV1,FV1,IERR)
C
      WRITE(IW,9050) (WR(I),I=1,3)
C
C     Order eigenvalues from smallest to largest to calculate anisotropy
C
      CALL ORDER(WR)
      SANI=(WR(3)-SISO) - HALF*((WR(1)-SISO)+(WR(2)-SISO))
      WRITE(IW,9040) SANI
C
      IF(PEVEC) WRITE(IW,9150) Z(1,1),Z(1,2),Z(1,3),
     *                         Z(2,1),Z(2,2),Z(2,3),
     *                         Z(3,1),Z(3,2),Z(3,3)
C
C
  250 CONTINUE
C
      RETURN
C
 9030 FORMAT(61X,F12.4)
 9040 FORMAT(60X,'(',F12.4,' )')
 9050 FORMAT(5X,'EIGENVALS:  ',F12.4,2X,F12.4,2X,F12.4)
 9150 FORMAT(5X,'EIGENVECS:  ',F12.4,2X,F12.4,2X,F12.4,/,
     *       17X,F12.4,2X,F12.4,2X,F12.4,/,
     *       17X,F12.4,2X,F12.4,2X,F12.4,/)
 9100 FORMAT(/13X,'GIAO CHEMICAL SHIELDING TENSOR (PPM):',/
     *        64X,'ISOTROPIC',/,26X,'X',13X,'Y',13X,'Z',9X,'SHIELDING',
     *        /60X,'(  ANISOTROPY )')
 9160 FORMAT(/1X,I4,1X,A8,A2,'X',F12.4,2X,F12.4,2X,F12.4,/,
     *       16X,'Y',F12.4,2X,F12.4,2X,F12.4,/,
     *       16X,'Z',F12.4,2X,F12.4,2X,F12.4)
C
      END
C*MODULE NMR  *DECK H10
      SUBROUTINE H10(QXM,QYM,QZM,XIJM,YIJM,ZIJM,
     *               DM,TDXM,TDYM,TDZM,
     *               VM,TVXM,TVYM,TVZM,
     *               ELLXM,ELLYM,ELLZM,
     *               H10X,H10Y,H10Z,L2S)
C
C     Calculates H(1,0).  See (3.50).
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DBG
C
      PARAMETER (MXATM=500)
C
      DIMENSION H10X(L2S),H10Y(L2S),H10Z(L2S),
     *          QXM(L2S),QYM(L2S),QZM(L2S),
     *          XIJM(L2S),YIJM(L2S),ZIJM(L2S),
     *          DM(L2S),TDXM(L2S),TDYM(L2S),TDZM(L2S),
     *          VM(L2S),TVXM(L2S),TVYM(L2S),TVZM(L2S),
     *          ELLXM(L2S),ELLYM(L2S),ELLZM(L2S)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
      DBG=.FALSE.
C
C     Construct the three [H^(1,0)_nu,lambda]_alpha matrices Eq. (3.50)
C
      DO 730 NN=1,L2S
C
        H10X(NN)=YIJM(NN)*TDZM(NN)-ZIJM(NN)*TDYM(NN)
     *           +YIJM(NN)*TVZM(NN)-ZIJM(NN)*TVYM(NN)
     *           +QXM(NN)*(DM(NN)+VM(NN))-ELLXM(NN)
C
        H10Y(NN)=ZIJM(NN)*TDXM(NN)-XIJM(NN)*TDZM(NN)
     *           +ZIJM(NN)*TVXM(NN)-XIJM(NN)*TVZM(NN)
     *           +QYM(NN)*(DM(NN)+VM(NN))-ELLYM(NN)
C
        H10Z(NN)=XIJM(NN)*TDYM(NN)-YIJM(NN)*TDXM(NN)
     *           +XIJM(NN)*TVYM(NN)-YIJM(NN)*TVXM(NN)
     *           +QZM(NN)*(DM(NN)+VM(NN))-ELLZM(NN)
C
 730  CONTINUE
C
      IF (DBG) THEN
      WRITE(IW,9999) 'X'
      CALL SPRTRIL(H10X,NUM)
      WRITE(IW,9999) 'Y'
      CALL SPRTRIL(H10Y,NUM)
      WRITE(IW,9999) 'Z'
      CALL SPRTRIL(H10Z,NUM)
      END IF
C
      RETURN
 9999 FORMAT(/5X,'H(1,0)',A1,' MATRIX FOR ALL NUCLEI:')
      END
C*MODULE NMR  *DECK DOEINT
      SUBROUTINE DOEINT(DBLK,TDXBLK,TDYBLK,TDZBLK,
     *                 ELLXBK,ELLYBK,ELLZBK,SXBLK,SYBLK,SZBLK,DIJ,
     *                 FAC,E,AJ,IA,JA,KA,IB,JB,KB)
C
C     Direct One Electron integrals (right from recurrance formulas.)
C     There are three new types of one-electron integrals here, but
C     since they don't have a factor of 1/r^3, they are much simpler
C     than the other one-electron MD integrals.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(HALF=0.5D+00,FOUR=4.0D+00,TWO=2.0D+00)
C
      DIMENSION E(1:3,0:12,0:6,-2:6)
C
                        DBLK = DBLK -
     *                              DIJ*FAC*HALF*
     * (( IB*(IB-1)*E(1,0,IA,IB-2)-(FOUR*AJ*IB+TWO*AJ)*E(1,0,IA,IB)
     *    +FOUR*(AJ**2)*E(1,0,IA,IB+2) ) * E(2,0,JA,JB)*E(3,0,KA,KB)
     * +( JB*(JB-1)*E(2,0,JA,JB-2)-(FOUR*AJ*JB+TWO*AJ)*E(2,0,JA,JB)
     *    +FOUR*(AJ**2)*E(2,0,JA,JB+2) ) * E(1,0,IA,IB)*E(3,0,KA,KB)
     * +( KB*(KB-1)*E(3,0,KA,KB-2)-(FOUR*AJ*KB+TWO*AJ)*E(3,0,KA,KB)
     *    +FOUR*(AJ**2)*E(3,0,KA,KB+2) ) * E(1,0,IA,IB)*E(2,0,JA,JB))
C
C                  Kinetic Energy T integrals (higher ang. mom. on A)
C
                        IA=IA+1
C
                        TDXBLK = TDXBLK -
     *                              DIJ*FAC*HALF*
     * (( IB*(IB-1)*E(1,0,IA,IB-2)-(FOUR*AJ*IB+TWO*AJ)*E(1,0,IA,IB)
     *    +FOUR*(AJ**2)*E(1,0,IA,IB+2) ) * E(2,0,JA,JB)*E(3,0,KA,KB)
     * +( JB*(JB-1)*E(2,0,JA,JB-2)-(FOUR*AJ*JB+TWO*AJ)*E(2,0,JA,JB)
     *    +FOUR*(AJ**2)*E(2,0,JA,JB+2) ) * E(1,0,IA,IB)*E(3,0,KA,KB)
     * +( KB*(KB-1)*E(3,0,KA,KB-2)-(FOUR*AJ*KB+TWO*AJ)*E(3,0,KA,KB)
     *    +FOUR*(AJ**2)*E(3,0,KA,KB+2) ) * E(1,0,IA,IB)*E(2,0,JA,JB))
C
                        IA=IA-1
C
                        JA=JA+1
C
                        TDYBLK = TDYBLK -
     *                              DIJ*FAC*HALF*
     * (( IB*(IB-1)*E(1,0,IA,IB-2)-(FOUR*AJ*IB+TWO*AJ)*E(1,0,IA,IB)
     *    +FOUR*(AJ**2)*E(1,0,IA,IB+2) ) * E(2,0,JA,JB)*E(3,0,KA,KB)
     * +( JB*(JB-1)*E(2,0,JA,JB-2)-(FOUR*AJ*JB+TWO*AJ)*E(2,0,JA,JB)
     *    +FOUR*(AJ**2)*E(2,0,JA,JB+2) ) * E(1,0,IA,IB)*E(3,0,KA,KB)
     * +( KB*(KB-1)*E(3,0,KA,KB-2)-(FOUR*AJ*KB+TWO*AJ)*E(3,0,KA,KB)
     *    +FOUR*(AJ**2)*E(3,0,KA,KB+2) ) * E(1,0,IA,IB)*E(2,0,JA,JB))
C
                        JA=JA-1
C
                        KA=KA+1
C
                        TDZBLK = TDZBLK -
     *                              DIJ*FAC*HALF*
     * (( IB*(IB-1)*E(1,0,IA,IB-2)-(FOUR*AJ*IB+TWO*AJ)*E(1,0,IA,IB)
     *    +FOUR*(AJ**2)*E(1,0,IA,IB+2) ) * E(2,0,JA,JB)*E(3,0,KA,KB)
     * +( JB*(JB-1)*E(2,0,JA,JB-2)-(FOUR*AJ*JB+TWO*AJ)*E(2,0,JA,JB)
     *    +FOUR*(AJ**2)*E(2,0,JA,JB+2) ) * E(1,0,IA,IB)*E(3,0,KA,KB)
     * +( KB*(KB-1)*E(3,0,KA,KB-2)-(FOUR*AJ*KB+TWO*AJ)*E(3,0,KA,KB)
     *    +FOUR*(AJ**2)*E(3,0,KA,KB+2) ) * E(1,0,IA,IB)*E(2,0,JA,JB))
C
                        KA=KA-1
C
C                       L^lambda^_alpha integrals  (3.50)
C
                        ELLXBK = ELLXBK +
     *                              DIJ*FAC* E(1,0,IA,IB) *
     *  (E(2,0,JA,JB+1)*( KB*E(3,0,KA,KB-1)-TWO*AJ*E(3,0,KA,KB+1) )
     *  - ( JB*E(2,0,JA,JB-1)-TWO*AJ*E(2,0,JA,JB+1) )*E(3,0,KA,KB+1))
C
                        ELLYBK = ELLYBK +
     *                              DIJ*FAC* E(2,0,JA,JB) *
     *  (E(3,0,KA,KB+1)*( IB*E(1,0,IA,IB-1)-TWO*AJ*E(1,0,IA,IB+1) )
     *  - ( KB*E(3,0,KA,KB-1)-TWO*AJ*E(3,0,KA,KB+1) )*E(1,0,IA,IB+1))
C
                        ELLZBK = ELLZBK +
     *                              DIJ*FAC* E(3,0,KA,KB) *
     *  (E(1,0,IA,IB+1)*( JB*E(2,0,JA,JB-1)-TWO*AJ*E(2,0,JA,JB+1) )
     *  - ( IB*E(1,0,IA,IB-1)-TWO*AJ*E(1,0,IA,IB+1) )*E(2,0,JA,JB+1))
C
                        SXBLK = SXBLK + DIJ*FAC*
     *                            E(1,0,IA+1,IB) *
     *                            E(2,0,JA,JB)   *
     *                            E(3,0,KA,KB)
C
                        SYBLK = SYBLK + DIJ*FAC*
     *                            E(1,0,IA,IB) *
     *                            E(2,0,JA+1,JB)   *
     *                            E(3,0,KA,KB)
C
                        SZBLK = SZBLK + DIJ*FAC*
     *                            E(1,0,IA,IB) *
     *                            E(2,0,JA,JB)   *
     *                            E(3,0,KA+1,KB)
C
      RETURN
      END
C*MODULE NMR  *DECK NOEINT
      SUBROUTINE NOEINT(VBLK,TVXBLK,TVYBLK,TVZBLK,
     *            IA,JA,KA,IB,JB,KB,FAC1,E,R)
C
C     Nuclear One Electron integrals (involve the nuclear positions.)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION E(1:3,0:12,0:6,-2:6), R(0:24,0:24,0:24,0:24)
C
      DO 430 N=0,IA+IB
        DO 420 L=0,JA+JB
          DO 410 M=0,KA+KB
C
            VBLK = VBLK + FAC1*
     *          E(1,N,IA,IB)*
     *          E(2,L,JA,JB)*
     *          E(3,M,KA,KB)*
     *          R(N,L,M,0)
C
  410     CONTINUE
  420   CONTINUE
  430 CONTINUE
C
C     x-type T integral
C
      IA = IA + 1
C
      DO 431 N=0,IA+IB
        DO 432 L=0,JA+JB
          DO 433 M=0,KA+KB
C
            TVXBLK = TVXBLK + FAC1*
     *          E(1,N,IA,IB)*
     *          E(2,L,JA,JB)*
     *          E(3,M,KA,KB)*
     *          R(N,L,M,0)
C
  433     CONTINUE
  432   CONTINUE
  431  CONTINUE
C
      IA = IA - 1
C
C     y-type T integral
C
      JA = JA + 1
C
      DO 434 N=0,IA+IB
        DO 435 L=0,JA+JB
          DO 436 M=0,KA+KB
C
            TVYBLK = TVYBLK + FAC1*
     *          E(1,N,IA,IB)*
     *          E(2,L,JA,JB)*
     *          E(3,M,KA,KB)*
     *          R(N,L,M,0)
C
  436     CONTINUE
  435   CONTINUE
  434 CONTINUE
C
      JA = JA - 1
C
C     z-type T integral
C
      KA = KA + 1
C
      DO 437 N=0,IA+IB
        DO 438 L=0,JA+JB
          DO 439 M=0,KA+KB
C
            TVZBLK = TVZBLK + FAC1*
     *          E(1,N,IA,IB)*
     *          E(2,L,JA,JB)*
     *          E(3,M,KA,KB)*
     *          R(N,L,M,0)
C
  439     CONTINUE
  438   CONTINUE
  437 CONTINUE
C
      KA = KA - 1
C
      RETURN
      END
C*MODULE NMR  *DECK OESOINT
      SUBROUTINE OESOINT(OESOI,IA,JA,KA,IB,JB,KB,AJ,FAC2,ID,IN,E,R,BR)
C
C     One-Electron Spin Orbit INTegrals
C     The form of these integrals is the second term in Eq. (3.39),
C     MAF thesis.  (The integral multiplied by the Q matrix.)  Note that
C     the "L" in that equation is a cross-product, so there are six
C     types of OESO integrals.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DBG
C
      PARAMETER(TWO=2.0D+00)
C
      DIMENSION E(1:3,0:12,0:6,-2:6), R(0:24,0:24,0:24,0:24),
     *         BE(1:3,0:12,0:6,-2:6),BR(0:24,0:24,0:24,0:24)
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
      DBG=.FALSE.
C
      IF(DBG) WRITE(IW,9998)
C
      NMAX=IA+IB
      LMAX=JA+JB
      MMAX=KA+KB
C
      IF(ID.EQ.1) NMAX=IA+IB+1
      IF(ID.EQ.2) LMAX=JA+JB+1
      IF(ID.EQ.3) MMAX=KA+KB+1
C
      DO 430 N=0,NMAX
        DO 420 L=0,LMAX
          DO 410 M=0,MMAX
C
            IF(ID.EQ.1) THEN
              BE(1,N,IA,IB) = IB*E(1,N,IA,IB-1) -
     *           TWO*AJ*E(1,N,IA,IB+1)
              BE(2,L,JA,JB) = E(2,L,JA,JB)
              BE(3,M,KA,KB) = E(3,M,KA,KB)
            ELSE IF(ID.EQ.2) THEN
              BE(1,N,IA,IB) = E(1,N,IA,IB)
              BE(2,L,JA,JB) = JB*E(2,L,JA,JB-1) -
     *           TWO*AJ*E(2,L,JA,JB+1)
              BE(3,M,KA,KB) = E(3,M,KA,KB)
            ELSE IF(ID.EQ.3) THEN
              BE(1,N,IA,IB) = E(1,N,IA,IB)
              BE(2,L,JA,JB) = E(2,L,JA,JB)
              BE(3,M,KA,KB) = KB*E(3,M,KA,KB-1) -
     *           TWO*AJ*E(3,M,KA,KB+1)
            END IF
C
            IF(IN.EQ.1) THEN
              BR(N,L,M,0) = R(N+1,L,M,0)
            ELSE IF(IN.EQ.2) THEN
              BR(N,L,M,0) = R(N,L+1,M,0)
            ELSE IF(IN.EQ.3) THEN
              BR(N,L,M,0) = R(N,L,M+1,0)
            END IF
C
            OESOI = OESOI + FAC2 *
     *          BE(1,N,IA,IB) *
     *          BE(2,L,JA,JB) *
     *          BE(3,M,KA,KB) *
     *          BR(N,L,M,0)
C
      IF(DBG)WRITE(IW,9999) N,L,M,OESOI,FAC2,BE(1,N,IA,IB),
     *    BE(2,L,JA,JB),BE(3,M,KA,KB),BR(N,L,M,0)
C
  410     CONTINUE
  420   CONTINUE
  430 CONTINUE
C
      RETURN
 9998 FORMAT(/1X,'N ','L ','M ',4X,'OESOI     ','FAC2      ',
     * 'EX        ','EY        ','EZ        ','R         ')
 9999 FORMAT(I2,I2,I2,2X,6F10.6)
      END
C*MODULE NMR  *DECK TINTS
      SUBROUTINE TINTS(TINTBK,IA,JA,KA,IB,JB,KB,AJ,FAC2,
     *                 ID,IN,IAN,E,R,BR)
C
C     The integrals that include the "T" cross product.  These are
C     like the One-Electron Spin Orbit INTegrals, except the angular
C     momentum on basis function A is risen.  Since each of the six
C     OESOI can be raised by x, y, or z, this leads to 6 x 3 = 18
C     types of integrals.  See the 1st term Eq. (3.39) in MAF thesis.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(TWO=2.0D+00)
C
      DIMENSION E(1:3,0:12,0:6,-2:6), R(0:24,0:24,0:24,0:24),
     *         BE(1:3,0:12,0:6,-2:6),BR(0:24,0:24,0:24,0:24)
C
      NMAX=IA+IB
      LMAX=JA+JB
      MMAX=KA+KB
C
      IF(ID.EQ.1) NMAX=NMAX+1
      IF(ID.EQ.2) LMAX=LMAX+1
      IF(ID.EQ.3) MMAX=MMAX+1
C
      IF(IAN.EQ.1) NMAX=NMAX+1
      IF(IAN.EQ.2) LMAX=LMAX+1
      IF(IAN.EQ.3) MMAX=MMAX+1
C
      DO 430 N=0,NMAX
        DO 420 L=0,LMAX
          DO 410 M=0,MMAX
C
            BE(1,N,IA,IB) = E(1,N,IA,IB)
            BE(2,L,JA,JB) = E(2,L,JA,JB)
            BE(3,M,KA,KB) = E(3,M,KA,KB)
C
            IF(ID.EQ.1) THEN
              BE(1,N,IA,IB) = IB*E(1,N,IA,IB-1) -
     *           TWO*AJ*E(1,N,IA,IB+1)
            ELSE IF(ID.EQ.2) THEN
              BE(2,L,JA,JB) = JB*E(2,L,JA,JB-1) -
     *           TWO*AJ*E(2,L,JA,JB+1)
            ELSE IF(ID.EQ.3) THEN
              BE(3,M,KA,KB) = KB*E(3,M,KA,KB-1) -
     *           TWO*AJ*E(3,M,KA,KB+1)
            END IF
C
            IF(IAN.EQ.1) THEN
              BE(1,N,IA,IB) = E(1,N,IA+1,IB)
            ELSE IF(IAN.EQ.2) THEN
              BE(2,L,JA,JB) = E(2,L,JA+1,JB)
            ELSE IF(IAN.EQ.3) THEN
              BE(3,M,KA,KB) = E(3,M,KA+1,KB)
            END IF
C
            IF(ID.EQ.1.AND.IAN.EQ.1) THEN
              BE(1,N,IA,IB) = IB*E(1,N,IA+1,IB-1) -
     *           TWO*AJ*E(1,N,IA+1,IB+1)
            ELSE IF(ID.EQ.2.AND.IAN.EQ.2) THEN
              BE(2,L,JA,JB) = JB*E(2,L,JA+1,JB-1) -
     *           TWO*AJ*E(2,L,JA+1,JB+1)
            ELSE IF(ID.EQ.3.AND.IAN.EQ.3) THEN
              BE(3,M,KA,KB) = KB*E(3,M,KA+1,KB-1) -
     *           TWO*AJ*E(3,M,KA+1,KB+1)
            END IF
C
            IF(IN.EQ.1) THEN
              BR(N,L,M,0) = R(N+1,L,M,0)
            ELSE IF(IN.EQ.2) THEN
              BR(N,L,M,0) = R(N,L+1,M,0)
            ELSE IF(IN.EQ.3) THEN
              BR(N,L,M,0) = R(N,L,M+1,0)
            END IF
C
            TINTBK =  TINTBK + FAC2 *
     *          BE(1,N,IA,IB) *
     *          BE(2,L,JA,JB) *
     *          BE(3,M,KA,KB) *
     *          BR(N,L,M,0)
C
  410     CONTINUE
  420   CONTINUE
  430 CONTINUE
C
      RETURN
      END
C*MODULE NMR  *DECK DELINTS
      SUBROUTINE DELINTS(DELINBK,IA,JA,KA,IB,JB,KB,FAC2,
     *          IN,IBN,E,R,BR)
C
C     Integerals that contain the Kronecker delta - these are basically
C     electric field integrals with a higher angular momentum on the
C     second basis function.  See the last term Eq. (3.39), MAF thesis.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DBG
C
      DIMENSION E(1:3,0:12,0:6,-2:6), R(0:24,0:24,0:24,0:24),
     *         BE(1:3,0:12,0:6,-2:6),BR(0:24,0:24,0:24,0:24)
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
      DBG=.FALSE.
      IF(DBG)WRITE(IW,9999) IA,JA,KA,IB,JB,KB,IN,IBN
C
      NMAX=IA+IB
      LMAX=JA+JB
      MMAX=KA+KB
C
      IF(IBN.EQ.1) NMAX=IA+IB+1
      IF(IBN.EQ.2) LMAX=JA+JB+1
      IF(IBN.EQ.3) MMAX=KA+KB+1
C
      DO 430 N=0,NMAX
        DO 420 L=0,LMAX
          DO 410 M=0,MMAX
C
            BE(1,N,IA,IB) = E(1,N,IA,IB)
            BE(2,L,JA,JB) = E(2,L,JA,JB)
            BE(3,M,KA,KB) = E(3,M,KA,KB)
C
            IF(IBN.EQ.1) THEN
              BE(1,N,IA,IB) = E(1,N,IA,IB+1)
            ELSE IF(IBN.EQ.2) THEN
              BE(2,L,JA,JB) = E(2,L,JA,JB+1)
            ELSE IF(IBN.EQ.3) THEN
              BE(3,M,KA,KB) = E(3,M,KA,KB+1)
            END IF
C
            IF(IN.EQ.1) THEN
              BR(N,L,M,0) = R(N+1,L,M,0)
            ELSE IF(IN.EQ.2) THEN
              BR(N,L,M,0) = R(N,L+1,M,0)
            ELSE IF(IN.EQ.3) THEN
              BR(N,L,M,0) = R(N,L,M+1,0)
            END IF
C
            IF(DBG) THEN
            WRITE(IW,9990) IBN,IN,DELINBK
            WRITE(IW,9980) BE(1,N,IA,IB),BE(2,L,JA,JB),BE(3,M,KA,KB)
            WRITE(IW,9970) BR(N,L,M,0),FAC2
            END IF
C
            DELINBK = DELINBK + FAC2 *
     *                BE(1,N,IA,IB) *
     *                BE(2,L,JA,JB) *
     *                BE(3,M,KA,KB) *
     *                BR(N,L,M,0)
C
  410     CONTINUE
  420   CONTINUE
  430 CONTINUE
C
      IF(DBG)WRITE(IW,9960) IBN,IN,DELINBK
C
      RETURN
 9960 FORMAT(3X,'OUTGOING DELINBK(',I2,I2,' ):',F12.7)
 9970 FORMAT(5X,'RNLM, FAC2:      ',2F12.7)
 9980 FORMAT(5X,'X, Y AND Z COEF.:',3F12.7)
 9990 FORMAT(5X,'INCOMING DELINBK(',I2,I2,' ):',F12.7)
 9999 FORMAT(/1X,'-------',I2,I2,I2,I3,I2,I2,I3,I2,' -------')
      END
C*MODULE NMR  *DECK ORDER
      SUBROUTINE ORDER(A)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION A(3)
C
      DO 10 L=1,2
        BIG=A(L)
        DO 10 I=L,3
          IF(A(I).LT.BIG) THEN
            TEMP=A(I)
            A(I)=A(L)
            A(L)=TEMP
            BIG=A(L)
          END IF
   10 CONTINUE
C
      RETURN
      END
C*MODULE NMR  *DECK INTGET
      SUBROUTINE INTGET(X2,XX,IX,NINTMX)
C
C     Read two-electron integrals from disk.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      LOGICAL DBG
C
      DIMENSION X2(NUM,NUM,NUM,NUM),XX(NINTMX),IX(NINTMX)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,MA,MB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
      DBG=.FALSE.
C
      M1=NUM
C
      CALL RDNMR2(IJK,X2,0,M1,XX,IX,NINTMX)
C
      DO I=1,NUM
       DO J=1,NUM
        DO K=1,NUM
         DO L=1,NUM
C
          IF(I.EQ.J.AND.K.EQ.L.AND.J.EQ.K) THEN
           X2(I,J,K,L)=8.0D+00*X2(I,J,K,L)
           GO TO 100
          ELSE IF (I.EQ.J.AND.K.EQ.L) THEN
           X2(I,J,K,L)=4.0D+00*X2(I,J,K,L)
           GO TO 100
          ELSE IF((I.EQ.J.OR.K.EQ.L).OR.(I.EQ.K.AND.J.EQ.L)
     *                             .OR.(I.EQ.L.AND.J.EQ.K)) THEN
           X2(I,J,K,L)=2.0D+00*X2(I,J,K,L)
          END IF
C
 100      CONTINUE
          IF(DBG) WRITE(IW,*) '(',I,J,'|',K,L,')=',X2(I,J,K,L)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
      RETURN
      END
C*MODULE NMR  *DECK RDNMR2
      SUBROUTINE RDNMR2(NFT,X2,NCORE,M1,XX,IX,NINTMX)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500)
C
      DIMENSION X2(NUM,NUM,NUM,NUM),XX(NINTMX),IX(*)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,MA,MB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCKLAB/ LABSIZ
C
C     ---- Read transformed 1 and 2 e- integrals into memory ----
C     N.B. this routine is not correct for parallel processing,
C     messages are needed to make all nodes have all integrals
C
C         only the master has the 1e- integral record
C
      NX = 0
      CALL SEQREW(NFT)
C     IF(MASWRK) CALL SQREAD(NFT,X1,M2)
C
C         Read transformed 2e- integral file in reverse canonical order.
C         Take only integrals with all 4 indices in the active space.
C
      CALL VCLR(X2,1,NUM*NUM*NUM*NUM)
  200 CONTINUE
      CALL PREAD(NFT,XX,IX,NX,NINTMX)
      IF (NX.EQ.0) GO TO 240
      MX = ABS(NX)
      IF (MX.GT.NINTMX) THEN
         IF(MASWRK) WRITE(IW,*) 'CONFUSION WITH INTEGRALS IN -RDNMR2-'
         CALL ABRT
         STOP
      END IF
      DO 220 M = 1,MX
         VAL = XX(M)
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
            IPACK =       ISHFT( LABEL, -48 )
            JPACK = IAND( ISHFT( LABEL, -32 ), 65535 )
            KPACK = IAND( ISHFT( LABEL, -16 ), 65535 )
            LPACK = IAND(        LABEL,        65535 )
#endif
         ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
            LABEL = IX(NPACK)
            IPACK =       ISHFT( LABEL, -24 )
            JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
            KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
            LPACK = IAND(        LABEL,        255 )
#endif
#if defined(SPEC_CPU_ILP64)
            IF ( MOD(NPACK,2) .EQ. 0 ) THEN
               LABEL = IX( NPACK/2 )
               IPACK = IAND( ISHFT( LABEL, -24 ), 255 )
               JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
               KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
               LPACK = IAND(        LABEL,        255 )
            ELSE
               LABEL = IX( (NPACK/2)+1 )
               IPACK =       ISHFT( LABEL, -56 )
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
         I = I-NCORE
         J = J-NCORE
         K = K-NCORE
         L = L-NCORE
         IF(I.LE.0  .OR.  I.GT.M1) GO TO 220
         IF(J.LE.0  .OR.  J.GT.M1) GO TO 220
         IF(K.LE.0  .OR.  J.GT.M1) GO TO 220
         IF(L.LE.0  .OR.  L.GT.M1) GO TO 220
C
         X2(I,J,K,L) = VAL
         X2(I,J,L,K) = VAL
         X2(J,I,L,K) = VAL
         X2(J,I,K,L) = VAL
         X2(K,L,I,J) = VAL
         X2(K,L,J,I) = VAL
         X2(L,K,J,I) = VAL
         X2(L,K,I,J) = VAL
  220 CONTINUE
      IF (NX .GT. 0) GO TO 200
C
  240 CONTINUE
      CALL SEQREW(NFT)
      RETURN
      END
C*MODULE NMR  *DECK P0QG0
      SUBROUTINE P0QG0(P0G10X,P0G10Y,P0G10Z,QX,QY,QZ,P,L2,L2S,
     *                 NFT,NCORE,M1,XX,IX,NINTMX)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (TWO=2.0D+00,HALF=0.5D+00,ONE=1.0D+00)
C
      DIMENSION XX(NINTMX),IX(NINTMX),P(L2),
     *     P0G10X(L2S),P0G10Y(L2S),P0G10Z(L2S),
     *     QX(L2S),QY(L2S),QZ(L2S)
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCKLAB/ LABSIZ
C
      CALL VCLR(P0G10X,1,L2S)
      CALL VCLR(P0G10Y,1,L2S)
      CALL VCLR(P0G10Z,1,L2S)
C
      CALL DAREAD(IDAF,IODA,P,L2,16,0)
C
      NX = 0
      CALL SEQREW(NFT)
C
  200 CONTINUE
      CALL PREAD(NFT,XX,IX,NX,NINTMX)
      IF (NX.EQ.0) GO TO 240
      MX = ABS(NX)
      IF (MX.GT.NINTMX) THEN
         IF(MASWRK) WRITE(IW,*) 'CONFUSION WITH INTEGRALS IN -RDNMR2-'
         CALL ABRT
         STOP
      END IF
      DO 220 M = 1,MX
         VAL = XX(M)
C
         NPACK = M
         IF(LABSIZ .EQ. 2) THEN
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
            IPACK =       ISHFT( LABEL, -48 )
            JPACK = IAND( ISHFT( LABEL, -32 ), 65535 )
            KPACK = IAND( ISHFT( LABEL, -16 ), 65535 )
            LPACK = IAND(        LABEL,        65535 )
#endif
         ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
            LABEL = IX(NPACK)
            IPACK =       ISHFT( LABEL, -24 )
            JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
            KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
            LPACK = IAND(        LABEL,        255 )
#endif
#if defined(SPEC_CPU_ILP64)
            IF ( MOD(NPACK,2) .EQ. 0 ) THEN
               LABEL = IX( NPACK/2 )
               IPACK = IAND( ISHFT( LABEL, -24 ), 255 )
               JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
               KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
               LPACK = IAND(        LABEL,        255 )
            ELSE
               LABEL = IX( (NPACK/2)+1 )
               IPACK =       ISHFT( LABEL, -56 )
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
         I = I-NCORE
         J = J-NCORE
         K = K-NCORE
         L = L-NCORE
         IF(I.LE.0  .OR.  I.GT.M1) GO TO 220
         IF(J.LE.0  .OR.  J.GT.M1) GO TO 220
         IF(K.LE.0  .OR.  J.GT.M1) GO TO 220
         IF(L.LE.0  .OR.  L.GT.M1) GO TO 220
C
C        val is equal to (i j|k l) and its seven other permutations.
C
         IF(I.EQ.J.AND.K.EQ.L.AND.J.EQ.K) THEN
          VAL=8.0D+00*VAL
          GO TO 100
         ELSE IF (I.EQ.J.AND.K.EQ.L) THEN
          VAL=4.0D+00*VAL
          GO TO 100
         ELSE IF((I.EQ.J.OR.K.EQ.L).OR.(I.EQ.K.AND.J.EQ.L)
     *                            .OR.(I.EQ.L.AND.J.EQ.K)) THEN
          VAL=2.0D+00*VAL
         END IF
C
 100  CONTINUE
C
C     Since we are forming the P(0)*Q*G(0) matrix (see the first
C     term of Eq. (3.52), and the second term in the sum in (3.49))
C     as the integrals come off disk, we need to calculate the matrix
C     one element at a time, being sure to use each integral in all
C     the places it's needed.  Remember that G has the general
C     form G_ijkl = g_ijkl - half*g_ilkj, and in what follows,
C     elements kl and ij refer to the first term of G, and elements
C     kj, il, ki, and lj refer to the second term of G.
C
C     Calculate the kl element of P(0)*Q*G(0).  If i.ne.j, we need
C     to multiply by two since the density matrix is symmetric.
C
C     k is always ge l
      IF(K.EQ.L) GO TO 110
C
C     i is always ge j
C
      II  = (I*I-I)/2
      KKS = ((K-1)*(K-1)-(K-1))/2
C
      IF(I.EQ.J) THEN
        F=ONE
      ELSE
        F=TWO
      END IF
C
      IJNN = II + J
      KLNNS = KKS + L
C
      P0G10X(KLNNS) = P0G10X(KLNNS) + F*P(IJNN)*QX(KLNNS)*VAL
      P0G10Y(KLNNS) = P0G10Y(KLNNS) + F*P(IJNN)*QY(KLNNS)*VAL
      P0G10Z(KLNNS) = P0G10Z(KLNNS) + F*P(IJNN)*QZ(KLNNS)*VAL
C
 110  CONTINUE
C
C     Calculate the ij element.  This element arises when we make the
C     exchange (ij <-> kl) in element kl.
C
C     i is always ge j
C
      IF(I.EQ.J) GO TO 115
C
      IF(I.EQ.K.AND.J.EQ.L) GO TO 115
C
      IF(K.EQ.L) THEN
        F = ONE
      ELSE
        F = TWO
      END IF
C
      KK  = (K*K-K)/2
      IIS = ((I-1)*(I-1)-(I-1))/2
C
      IJNNS = IIS + J
      KLNN = KK + L
C
      P0G10X(IJNNS) = P0G10X(IJNNS) + F*P(KLNN)*QX(IJNNS)*VAL
      P0G10Y(IJNNS) = P0G10Y(IJNNS) + F*P(KLNN)*QY(IJNNS)*VAL
      P0G10Z(IJNNS) = P0G10Z(IJNNS) + F*P(KLNN)*QZ(IJNNS)*VAL
C
 115  CONTINUE
C
C     Calculate the kj element of P(0)*Q*G(0).  Since the Q are
C     antisymmetric, we must skip the diagonals, which aren't stored.
C
C     k is always ge j
C     k is always ge l
C     i is always ge j
C
      IF(K.EQ.J) GO TO 120
C
      II  = (I*I-I)/2
      LL  = (L*L-L)/2
      KKS = ((K-1)*(K-1)-(K-1))/2
      IIS = ((I-1)*(I-1)-(I-1))/2
C
      KJNNS = KKS + J
C
      IF(I.LE.L) THEN
        ILNN = LL + I
      ELSE
        ILNN = II + L
      END IF
C
      IF(I.EQ.J) GO TO 119
      IJNNS = IIS + J
      P0G10X(KJNNS) = P0G10X(KJNNS) - HALF*P(ILNN)*QX(IJNNS)*VAL
      P0G10Y(KJNNS) = P0G10Y(KJNNS) - HALF*P(ILNN)*QY(IJNNS)*VAL
      P0G10Z(KJNNS) = P0G10Z(KJNNS) - HALF*P(ILNN)*QZ(IJNNS)*VAL
C
 119  CONTINUE
      IF(K.EQ.L) GO TO 120
      KLNNS = KKS + L
      P0G10X(KJNNS) = P0G10X(KJNNS) - HALF*P(ILNN)*QX(KLNNS)*VAL
      P0G10Y(KJNNS) = P0G10Y(KJNNS) - HALF*P(ILNN)*QY(KLNNS)*VAL
      P0G10Z(KJNNS) = P0G10Z(KJNNS) - HALF*P(ILNN)*QZ(KLNNS)*VAL
C
 120  CONTINUE
C
C     Calculate the ki element.  This element arises when we make the
C     exchange (i <-> j) in element kj.
C
C     k is always ge i
C
      IF(K.EQ.I) GO TO 130
      IF(I.EQ.J) GO TO 130
C
      JJ  = (J*J-J)/2
      LL  = (L*L-L)/2
      IIS = ((I-1)*(I-1)-(I-1))/2
      KKS = ((K-1)*(K-1)-(K-1))/2
C
      IF(J.LE.L) THEN
        JLNN = LL + J
      ELSE
        JLNN = JJ + L
      END IF
C
      KINNS = KKS + I
C
      IF(I.EQ.J) GO TO 129
      IJNNS = IIS + J
      P0G10X(KINNS) = P0G10X(KINNS) + HALF*P(JLNN)*QX(IJNNS)*VAL
      P0G10Y(KINNS) = P0G10Y(KINNS) + HALF*P(JLNN)*QY(IJNNS)*VAL
      P0G10Z(KINNS) = P0G10Z(KINNS) + HALF*P(JLNN)*QZ(IJNNS)*VAL
C
 129  CONTINUE
      IF(K.EQ.L) GO TO 130
      KLNNS = KKS + L
      P0G10X(KINNS) = P0G10X(KINNS) - HALF*P(JLNN)*QX(KLNNS)*VAL
      P0G10Y(KINNS) = P0G10Y(KINNS) - HALF*P(JLNN)*QY(KLNNS)*VAL
      P0G10Z(KINNS) = P0G10Z(KINNS) - HALF*P(JLNN)*QZ(KLNNS)*VAL
C
  130 CONTINUE
C
C     Calculate the il element.  This element arises when we make the
C     exchange (ij <-> kl) in element kj.
C
C     take the case where i is ge l
C
      IF(I.LE.L) GO TO 140
C
      IF(I.EQ.K.AND.J.EQ.L) GO TO 140
C
      KK  = (K*K-K)/2
      LLS = ((L-1)*(L-1)-(L-1))/2
C
      ILNNS = IIS + L
      KJNN = KK + J
C
      IF(I.EQ.J) GO TO 139
      IJNNS = IIS + J
      P0G10X(ILNNS) = P0G10X(ILNNS) - HALF*P(KJNN)*QX(IJNNS)*VAL
      P0G10Y(ILNNS) = P0G10Y(ILNNS) - HALF*P(KJNN)*QY(IJNNS)*VAL
      P0G10Z(ILNNS) = P0G10Z(ILNNS) - HALF*P(KJNN)*QZ(IJNNS)*VAL
C
 139  CONTINUE
      IF(K.EQ.L) GO TO 140
      KLNNS = KKS + L
      P0G10X(ILNNS) = P0G10X(ILNNS) - HALF*P(KJNN)*QX(KLNNS)*VAL
      P0G10Y(ILNNS) = P0G10Y(ILNNS) - HALF*P(KJNN)*QY(KLNNS)*VAL
      P0G10Z(ILNNS) = P0G10Z(ILNNS) - HALF*P(KJNN)*QZ(KLNNS)*VAL
C
 140  CONTINUE
C
C     Calculate the lj element.  This element arises when we make the
C     exchange (k <-> l) in element kj.
C
C     take the case where l is ge j
C
      IF(L.LE.J) GO TO 150
      IF(K.EQ.L) GO TO 150
C
      KK  = (K*K-K)/2
      IIS = ((I-1)*(I-1)-(I-1))/2
      KKS = ((K-1)*(K-1)-(K-1))/2
      LLS = ((L-1)*(L-1)-(L-1))/2
C
      LJNNS = LLS + J
C
      IKNN = KK + I
C
      IF(I.EQ.J) GO TO 149
      IJNNS = IIS + J
      P0G10X(LJNNS) = P0G10X(LJNNS) - HALF*P(IKNN)*QX(IJNNS)*VAL
      P0G10Y(LJNNS) = P0G10Y(LJNNS) - HALF*P(IKNN)*QY(IJNNS)*VAL
      P0G10Z(LJNNS) = P0G10Z(LJNNS) - HALF*P(IKNN)*QZ(IJNNS)*VAL
C
 149  CONTINUE
      IF(K.EQ.L) GO TO 150
      KLNNS = KKS + L
      P0G10X(LJNNS) = P0G10X(LJNNS) + HALF*P(IKNN)*QX(KLNNS)*VAL
      P0G10Y(LJNNS) = P0G10Y(LJNNS) + HALF*P(IKNN)*QY(KLNNS)*VAL
      P0G10Z(LJNNS) = P0G10Z(LJNNS) + HALF*P(IKNN)*QZ(KLNNS)*VAL
C
 150  CONTINUE
C
C     take the case where j is ge l
C
      IF(J.LE.L) GO TO 160
      IF(I.EQ.J) GO TO 160
C
      KK  = (K*K-K)/2
      JJS = ((J-1)*(J-1)-(J-1))/2
C
      KINN = KK + I
      JLNNS = JJS + L
      IJNNS = IIS + J
C
      P0G10X(JLNNS) = P0G10X(JLNNS) + HALF*P(KINN)*QX(IJNNS)*VAL
      P0G10Y(JLNNS) = P0G10Y(JLNNS) + HALF*P(KINN)*QY(IJNNS)*VAL
      P0G10Z(JLNNS) = P0G10Z(JLNNS) + HALF*P(KINN)*QZ(IJNNS)*VAL
C
      IF(K.EQ.L) GO TO 160
      KLNNS = KKS + L
      P0G10X(JLNNS) = P0G10X(JLNNS) - HALF*P(KINN)*QX(KLNNS)*VAL
      P0G10Y(JLNNS) = P0G10Y(JLNNS) - HALF*P(KINN)*QY(KLNNS)*VAL
      P0G10Z(JLNNS) = P0G10Z(JLNNS) - HALF*P(KINN)*QZ(KLNNS)*VAL
C
 160  CONTINUE
C
C     take the case where l is ge i
C
      IF(L.LE.I) GO TO 170
      IF(I.EQ.J.OR.K.EQ.L) GO TO 170
C
      KK  = (K*K-K)/2
      IIS = ((I-1)*(I-1)-(I-1))/2
      KKS = ((K-1)*(K-1)-(K-1))/2
      LLS = ((L-1)*(L-1)-(L-1))/2
C
      JKNN = KK + J
      LINNS = LLS + I
      KLNNS = KKS + L
      IJNNS = IIS + J
C
      P0G10X(LINNS) = P0G10X(LINNS) + HALF*P(JKNN)*QX(IJNNS)*VAL
      P0G10Y(LINNS) = P0G10Y(LINNS) + HALF*P(JKNN)*QY(IJNNS)*VAL
      P0G10Z(LINNS) = P0G10Z(LINNS) + HALF*P(JKNN)*QZ(IJNNS)*VAL
C
      P0G10X(LINNS) = P0G10X(LINNS) + HALF*P(JKNN)*QX(KLNNS)*VAL
      P0G10Y(LINNS) = P0G10Y(LINNS) + HALF*P(JKNN)*QY(KLNNS)*VAL
      P0G10Z(LINNS) = P0G10Z(LINNS) + HALF*P(JKNN)*QZ(KLNNS)*VAL
C
 170  CONTINUE
C
 220  CONTINUE
      IF (NX .GT. 0) GO TO 200
C
  240 CONTINUE
      CALL SEQREW(NFT)
      RETURN
      END
C*MODULE NMR  *DECK P10G0
      SUBROUTINE P10G0(F10,P10,P0G10,H10,L2S,NFT,NCORE,M1,XX,IX,NINTMX)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (HALF=0.5D+00)
C
      DIMENSION XX(NINTMX),IX(NINTMX),P10(L2S),F10(L2S),
     *          P0G10(L2S),H10(L2S)
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCKLAB/ LABSIZ
C
      CALL VCLR(F10,1,L2S)
C
      NX = 0
      CALL SEQREW(NFT)
C
  200 CONTINUE
      CALL PREAD(NFT,XX,IX,NX,NINTMX)
      IF (NX.EQ.0) GO TO 240
      MX = ABS(NX)
      IF (MX.GT.NINTMX) THEN
         IF(MASWRK) WRITE(IW,*) 'CONFUSION WITH INTEGRALS IN -RDNMR2-'
         CALL ABRT
         STOP
      END IF
      DO 220 M = 1,MX
         VAL = XX(M)
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
            IPACK =       ISHFT( LABEL, -48 )
            JPACK = IAND( ISHFT( LABEL, -32 ), 65535 )
            KPACK = IAND( ISHFT( LABEL, -16 ), 65535 )
            LPACK = IAND(        LABEL,        65535 )
#endif
         ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
            LABEL = IX(NPACK)
            IPACK =       ISHFT( LABEL, -24 )
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
               LPACK = IAND(        LABEL,        255 )
            ELSE
               LABEL = IX( (NPACK/2)+1 )
               IPACK =       ISHFT( LABEL, -56 )
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
         I = I-NCORE
         J = J-NCORE
         K = K-NCORE
         L = L-NCORE
         IF(I.LE.0  .OR.  I.GT.M1) GO TO 220
         IF(J.LE.0  .OR.  J.GT.M1) GO TO 220
         IF(K.LE.0  .OR.  J.GT.M1) GO TO 220
         IF(L.LE.0  .OR.  L.GT.M1) GO TO 220
C
C        val is equal to (i j|k l) and its seven other permutations.
C
         IF(I.EQ.J.AND.K.EQ.L.AND.J.EQ.K) THEN
          VAL=8.0D+00*VAL
          GO TO 100
         ELSE IF (I.EQ.J.AND.K.EQ.L) THEN
          VAL=4.0D+00*VAL
          GO TO 100
         ELSE IF((I.EQ.J.OR.K.EQ.L).OR.(I.EQ.K.AND.J.EQ.L)
     *                            .OR.(I.EQ.L.AND.J.EQ.K)) THEN
          VAL=2.0D+00*VAL
         END IF
C
 100  CONTINUE
C
C     Calculate the kj element of P(1,0)*G(0).  Since the P(1,0) are
C     anti-symmetric, we must skip the diagonals, which aren't stored.
C
C     k is always ge i,j,l
C     i is always ge j
C
      IF(K.EQ.J) GO TO 110
      IF(I.EQ.L) GO TO 110
C
      KKS = ((K-1)*(K-1)-(K-1))/2
      KJNNS = KKS + J
C
      IF(I.LT.L) GO TO 109
      IIS = ((I-1)*(I-1)-(I-1))/2
      ILNNS = IIS + L
      F10(KJNNS) = F10(KJNNS) - HALF*P10(ILNNS)*VAL
C
 109  CONTINUE
C
      IF(I.GT.L) GO TO 110
      IF(I.EQ.K.OR.J.EQ.L) GO TO 110
      LLS = ((L-1)*(L-1)-(L-1))/2
      ILNNS = LLS + I
      F10(KJNNS) = F10(KJNNS) + HALF*P10(ILNNS)*VAL
C
 110  CONTINUE
C
C     Calculate the ki element of P(1,0)*G(0).
C
      IF(K.EQ.I) GO TO 120
      IF(I.EQ.J) GO TO 120
      IF(J.EQ.L) GO TO 120
C
      KKS = ((K-1)*(K-1)-(K-1))/2
      KINNS = KKS + I
C
      IF(J.LT.L) GO TO 119
      JJS = ((J-1)*(J-1)-(J-1))/2
      JLNNS = JJS + L
      F10(KINNS) = F10(KINNS) - HALF*P10(JLNNS)*VAL
C
 119  CONTINUE
C
      IF(L.LT.J) GO TO 120
      LLS = ((L-1)*(L-1)-(L-1))/2
      JLNNS = LLS + J
      F10(KINNS) = F10(KINNS) + HALF*P10(JLNNS)*VAL
C
 120  CONTINUE
C
C     Calculate the il/li elements of P(1,0)*G(0).
C     Take the case where i.gt.l
C
      IF(K.EQ.J) GO TO 130
      IF(I.EQ.L) GO TO 130
C
      KKS = ((K-1)*(K-1)-(K-1))/2
      KJNNS = KKS + J
C
      IF(I.LE.L) GO TO 129
C
      IF(I.EQ.K.AND.J.EQ.L) GO TO 129
C
      IIS = ((I-1)*(I-1)-(I-1))/2
      ILNNS = IIS + L
      F10(ILNNS) = F10(ILNNS) - HALF*P10(KJNNS)*VAL
C
 129  CONTINUE
C
C     Take the case where l.gt.i
C
      IF(L.LT.I) GO TO 130
      IF(L.EQ.K.OR.J.EQ.I) GO TO 130
C
      LLS = ((L-1)*(L-1)-(L-1))/2
      ILNNS = LLS + I
      F10(ILNNS) = F10(ILNNS) + HALF*P10(KJNNS)*VAL
C
 130  CONTINUE
C
C     Calculate the lj/jl element of P(1,0)*G(0).
C     Take the case where l.gt.j
C
      IF(I.EQ.K) GO TO 140
      IF(J.EQ.L) GO TO 140
C
      KKS = ((K-1)*(K-1)-(K-1))/2
      KINNS = KKS + I
C
      IF(L.LE.J) GO TO 139
C
      IF(L.EQ.K) GO TO 139
C
      LLS = ((L-1)*(L-1)-(L-1))/2
      LJNNS = LLS + J
      F10(LJNNS) = F10(LJNNS) + HALF*P10(KINNS)*VAL
C
 139  CONTINUE
C
C     Take the case where j.gt.l
C
      IF(J.LT.L) GO TO 140
      IF(I.EQ.J) GO TO 140
C
      JJS = ((J-1)*(J-1)-(J-1))/2
      LJNNS = JJS + L
      F10(LJNNS) = F10(LJNNS) - HALF*P10(KINNS)*VAL
C
 140  CONTINUE
C
 220  CONTINUE
      IF (NX .GT. 0) GO TO 200
C
  240 CONTINUE
C
      CALL SEQREW(NFT)
C
C     Construct the full F(1,0) matrix
C
      DO NN=1,L2S
        F10(NN)=H10(NN)+F10(NN)+P0G10(NN)
      ENDDO
C
      RETURN
      END
C*MODULE NMR  *DECK PINT
C---      subroutine pint(teint)
C---
C---c     Debugging routine - print two-electron integrals.
C---
C---      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C---
C---      PARAMETER (MXATM=500)
C---
C---      dimension teint(num,num,num,num)
C---
C---      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
C---     *                ZAN(MXATM),C(3,MXATM)
C---      COMMON /IOFILE/ IR,IW,IP,ijk,ijkt,IDAF,NAV,IODA(400)
C---
C---      do i=1,num
C---       do j=1,num
C---        do k=1,num
C---         do l=1,num
C---
C---          write(iw,9989) i,j,k,l,teint(i,j,k,l)
C---
C---         enddo
C---        enddo
C---       enddo
C---      enddo
C---
C---      return
C--- 9989 format(7x,'(',i3,i3,'|',i3,i3,')',f25.15)
C---      end
C*MODULE NMR  *DECK MDTWOEIA
      SUBROUTINE MDTWOEIA(TEDBG,PTEINT,TEINTX,TEINTY,TEINTZ,
     *                    AIN,X,R,NUPANG)
C
C     This calculates the perturbed two-electron integrals by tricking
C     the normal 2-electron code into thinking that the angular momentum
C     on the I or K shell is one unit larger than it is:
C
C     nupang can be 1 or 2:
C
C       1 = up angular momentum on I shell
C       2 = up angular momentum on K shell
C
C     To get away with this, you must reset some of the integral
C     parameters (sub. upshan); reset the coefficients, since they
C     shouldn't change even though the angular momentum does;
C     preserve the old normalization factors, and finally, filter
C     the resulting integrals into a form that won't result in the
C     overwriting of integrals. (disti and distk).  This last step
C     is not trivial, and it must be done at the end of each shell.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL UPANGI, UPANGK
      LOGICAL DOUBLE, NORM, TEDBG, PTEINT, LSHEL
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      PARAMETER (ZERO=0.0D+00, SQRT3=1.73205080756888D+00,
     *           SQRT5=2.23606797749979D+00, SQRT7=2.64575131106459D+00,
     *           TPIFH=34.9868366552497D+00, ONE=1.0D+00)
C
      DIMENSION TEINTX(NUM,NUM,NUM,NUM),TEINTY(NUM,NUM,NUM,NUM),
     *          TEINTZ(NUM,NUM,NUM,NUM),
     *          AIN(75,225,225),X(225,225),R(0:24,0:24,0:24,0:24)
      DIMENSION ES1(3),E1(1:3,0:12,0:6,-2:6),
     *          ES2(3),E2(1:3,0:12,0:6,-2:6),
     *          FOFT(0:24),DIJ(225),DKL(225)
C
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
C
C     tedbg: print detailed two-electron integral debugging info.
C     turn off simple printing of ints (pteint) when this is selected.
C
      IF(TEDBG) PTEINT=.FALSE.
      IF(TEDBG) WRITE(IW,9999)
C
      NORM=NORMP.NE.1 .OR. NORMF.NE.1
C
      IF(PTEINT.OR.TEDBG) WRITE(IW,*)
     *      'FOR THIS SET OF INTEGRALS, NUPANG=',NUPANG
C
      UPANGI=.FALSE.
      UPANGK=.FALSE.
      IF(NUPANG.EQ.1) UPANGI=.TRUE.
      IF(NUPANG.EQ.2) UPANGK=.TRUE.
C
C     ----- I SHELL
C
      DO 680 II = 1,NSHELL
C
         I = KATOM(II)
         XI = C(1,I)
         YI = C(2,I)
         ZI = C(3,I)
         I1 = KSTART(II)
         I2 = I1+KNG(II)-1
         MINI = KMIN(II)
         MAXI = KMAX(II)
         LOCI = KLOC(II)-MINI
         INDI=KLOC(II)
         LIT = KTYPE(II)
C
         IF(UPANGI) CALL UPSHAN(II,MINI,MAXI,LOCI,LSHEL)
C
C     ----- J SHELL
C
         DO 660 JJ = 1,NSHELL
C
            J = KATOM(JJ)
            XJ = C(1,J)
            YJ = C(2,J)
            ZJ = C(3,J)
            J1 = KSTART(JJ)
            J2 = J1+KNG(JJ)-1
            MINJ = KMIN(JJ)
            MAXJ = KMAX(JJ)
            LOCJ = KLOC(JJ)-MINJ
            LJT = KTYPE(JJ)
C
            RX1= XI-XJ
            RY1= YI-YJ
            RZ1= ZI-ZJ
            RX12= RX1*RX1
            RY12= RY1*RY1
            RZ12= RZ1*RZ1
C
C     ----- K SHELL
C
      DO 720 KK = 1,NSHELL
C
C
         K = KATOM(KK)
         XK = C(1,K)
         YK = C(2,K)
         ZK = C(3,K)
         K1 = KSTART(KK)
         K2 = K1+KNG(KK)-1
         MINK = KMIN(KK)
         MAXK = KMAX(KK)
         LOCK = KLOC(KK)-MINK
         INDK=KLOC(KK)
         LKT = KTYPE(KK)
C
         IF(UPANGK) CALL UPSHAN(KK,MINK,MAXK,LOCK,LSHEL)
C
C     ----- L SHELL
C
         DO 700 LL = 1,NSHELL
C
            L = KATOM(LL)
            XL = C(1,L)
            YL = C(2,L)
            ZL = C(3,L)
            L1 = KSTART(LL)
            L2 = L1+KNG(LL)-1
            MINL = KMIN(LL)
            MAXL = KMAX(LL)
            LOCL = KLOC(LL)-MINL
            LLT = KTYPE(LL)
C
            RX2= XK-XL
            RY2= YK-YL
            RZ2= ZK-ZL
            RX22= RX2*RX2
            RY22= RY2*RY2
            RZ22= RZ2*RZ2
C
C     ----- Begin McMurchie-Davidson code ------
C
            MMCLR=(MAXI-MINI+2)*(MAXJ-MINJ+2)
            NNCLR=(MAXK-MINK+2)*(MAXL-MINL+2)
            IKCLR=2*(LIT+1)*(LJT+1)
C
            DO NNTMP=1,NNCLR
            DO MMTMP=1,MMCLR
              X(MMTMP,NNTMP)=ZERO
            ENDDO
            ENDDO
C
            IF(TEDBG.OR.PTEINT) WRITE(IW,9998) II,JJ,KK,LL
C
C           Sum over primitives in I shell contraction
C
            JGMAX = J2
            DO 640 IG = I1,I2
C
            AI  = EX(IG)
            CSI = CS(IG)
            CPI = CP(IG)
            CDI = CD(IG)
            CFI = CF(IG)
            CGI = CG(IG)
C
            IF(UPANGI) THEN
            CPI = CS(IG)
            CDI = CP(IG)
            CFI = CD(IG)
            CGI = CF(IG)
            END IF
C
C           Sum over primitives in J shell contraction
C
            DO 620 JG = J1,JGMAX
C
              AJ  = EX(JG)
              CSJ = CS(JG)
              CPJ = CP(JG)
              CDJ = CD(JG)
              CFJ = CF(JG)
              CGJ = CG(JG)
C
C             Clean out the recursion array E
C
              DO  L=1,3
               DO  M=0,8
                DO  N=0,4
                 DO  I=0,4
                  E1(L,M,N,I)=ZERO
                  E2(L,M,N,I)=ZERO
                 ENDDO
                ENDDO
               ENDDO
              ENDDO
C
C       Coefficients depend on this particular alpha(i) and alpha(j):
C       Return e1([x,y,z],ip,ia,ib) (The recurrance formulas.)
C
              CALL EAB(ES1,AI,AJ,RX12,RY12,RZ12)
C
              CALL RECURS(ES1,AI,AJ,RX1,RY1,RZ1,LIT,LJT,E1)
C
C       "Zero I" Still may be a better way...
C
              DO IKTMP=0,IKCLR
              DO MMTMP=0,MMCLR
              DO NNTMP=0,NNCLR
C
                AIN(IKTMP,MMTMP,NNTMP)=ZERO
C
              ENDDO
              ENDDO
              ENDDO
C
C             Sum over primitives in K shell contraction
C
              LGMAX = L2
              DO 600 KG = K1,K2
C
              AK  = EX(KG)
              CSK = CS(KG)
              CPK = CP(KG)
              CDK = CD(KG)
              CFK = CF(KG)
              CGK = CG(KG)
C
              IF(UPANGK) THEN
              CPK = CS(KG)
              CDK = CP(KG)
              CFK = CD(KG)
              CGK = CF(KG)
              END IF
C
C             Sum over primitives in L shell contraction
C
              DO 580 LG = L1,LGMAX
C
                AL  = EX(LG)
                CSL = CS(LG)
                CPL = CP(LG)
                CDL = CD(LG)
                CFL = CF(LG)
                CGL = CG(LG)
C
C         Coefficients depend on this particular alpha(k) and alpha(l):
C         Return e2([x,y,z],ip,ia,ib) (The recurrance formulas.)
C
                CALL EAB(ES2,AK,AL,RX22,RY22,RZ22)
C
                CALL RECURS(ES2,AK,AL,RX2,RY2,RZ2,LKT,LLT,E2)
C
C         Compute lambda, fofj(T)
C
                AMBDA=TPIFH/((AI+AJ)*(AK+AL)*SQRT(AI+AJ+AK+AL))
C
                MXLIJT = LIT + LJT + LKT + LLT - 3
C
                PX = (AI*XI+AJ*XJ)/(AI+AJ)
                PY = (AI*YI+AJ*YJ)/(AI+AJ)
                PZ = (AI*ZI+AJ*ZJ)/(AI+AJ)
C
                QX = (AK*XK+AL*XL)/(AK+AL)
                QY = (AK*YK+AL*YL)/(AK+AL)
                QZ = (AK*ZK+AL*ZL)/(AK+AL)
C
                PQX = PX - QX
                PQY = PY - QY
                PQZ = PZ - QZ
C
                ALPHA = ((AI+AJ)*(AK+AL))/(AI+AJ+AK+AL)
C
                T = ALPHA * (PQX**2 + PQY**2 + PQZ**2)
C
                CALL FOFTJ(MXLIJT,T,FOFT)
C
C               Compute R_nlm
C
                CALL RNLM(MXLIJT,PQX,PQY,PQZ,ALPHA,FOFT,R)
C
C               Compute primitive coefficients (loop over g, h)
C                                              (counted by nn)
C
                 DOUBLE=.FALSE.
                 NN = 0
                 DUM1 = ZERO
                 DUM2 = ZERO
                 LLMAX=MAXL
                 DO 560 KKK = MINK,MAXK
C
                    IF (KKK.EQ.1) DUM1=CSK
                    IF (KKK.EQ.2) DUM1=CPK
                    IF (KKK.EQ.5) DUM1=CDK
C
C             The normalization for shell KK is done later (sub. distk)
C
                    IF ((KKK.EQ. 8).AND.NORM.AND.UPANGI) DUM1=DUM1*SQRT3
                    IF (KKK.EQ.11) DUM1=CFK
                    IF ((KKK.EQ.14).AND.NORM.AND.UPANGI) DUM1=DUM1*SQRT5
                    IF ((KKK.EQ.20).AND.NORM.AND.UPANGI) DUM1=DUM1*SQRT3
                    IF (KKK.EQ.21) DUM1=CGK
                    IF ((KKK.EQ.24).AND.NORM.AND.UPANGI) DUM1=DUM1*SQRT7
                    IF ((KKK.EQ.30).AND.NORM.AND.UPANGI)
     *                                             DUM1=DUM1*SQRT5/SQRT3
                    IF ((KKK.EQ.33).AND.NORM.AND.UPANGI) DUM1=DUM1*SQRT3
C
                    CALL GETKLM(KKK,IC,JC,KC)
C
                    DO 540 LLL = MINL,LLMAX
C
                       IF (LLL.EQ.1) THEN
                          DUM2=DUM1*CSL
                          IF (DOUBLE) THEN
                             IF (KKK.LE.1) THEN
                                DUM2=DUM2+DUM2
                             ELSE
                                DUM2=DUM2+CSK*CPL
                             END IF
                          END IF
                       ELSE IF (LLL.EQ.2) THEN
                          DUM2=DUM1*CPL
                          IF (DOUBLE) DUM2=DUM2+DUM2
                       ELSE IF (LLL.EQ.5) THEN
                          DUM2=DUM1*CDL
                          IF (DOUBLE) DUM2=DUM2+DUM2
                       ELSE IF ((LLL.EQ.8).AND.NORM) THEN
                          DUM2=DUM2*SQRT3
                       ELSE IF (LLL.EQ.11) THEN
                          DUM2=DUM1*CFL
                          IF (DOUBLE) DUM2=DUM2+DUM2
                       ELSE IF ((LLL.EQ.14).AND.NORM) THEN
                          DUM2=DUM2*SQRT5
                       ELSE IF ((LLL.EQ.20).AND.NORM) THEN
                          DUM2=DUM2*SQRT3
                       ELSE IF (LLL.EQ.21) THEN
                          DUM2=DUM1*CGL
                          IF (DOUBLE) DUM2=DUM2+DUM2
                       ELSE IF ((LLL.EQ.24).AND.NORM) THEN
                          DUM2=DUM2*SQRT7
                       ELSE IF ((LLL.EQ.30).AND.NORM) THEN
                          DUM2=DUM2*SQRT5/SQRT3
                       ELSE IF ((LLL.EQ.33).AND.NORM) THEN
                          DUM2=DUM2*SQRT3
                       END IF
C
                       CALL GETKLM(LLL,ID,JD,KD)
C
                       NN = NN+1
C
                       DKL(NN) = DUM2
C
                       NPMAX=IC+ID
                       LPMAX=JC+JD
                       MPMAX=KC+KD
C
                       MM=0
C
                       JMAX=MAXJ
                       DO 535 III=MINI,MAXI
                         CALL GETKLM(III,IA,JA,KA)
C
                       DO 525 JJJ=MINJ,JMAX
C
                         CALL GETKLM(JJJ,IB,JB,KB)
C
                       MM=MM+1
C
                       IF(TEDBG) WRITE(IW,9997) IG,JG,KG,LG,
     *                                CSI,CSJ,CSK,CSL,
     *                  IA,JA,KA,IB,JB,KB,IC,JC,KC,ID,JD,KD,
     *                                CPI,CPJ,CPK,CPL
C
C
                       NMAX=IA+IB
                       LMAX=JA+JB
                       MMAX=KA+KB
C
C                      Sum over (N',L',M')k'
C
                       DO 520 NP=0,NPMAX
                         DO 510 LP=0,LPMAX
                           DO 500 MP=0,MPMAX
C
                             SIGN=(-ONE)**(NP+LP+MP)
C
C                            Loop over (N,L,M)k (ik)
C
                             IK=0
                             DO 480 N=0,NMAX
                             DO 460 L=0,LMAX
                             DO 440 M=0,MMAX
                             IK=IK+1
C
                             IF(TEDBG) WRITE(IW,9995) IK,MM,NN,
     *                       AIN(IK,MM,NN),
     *                       SIGN,AMBDA,NN,DKL(NN),
     *                       N+NP,L+LP,M+MP,R(N+NP,L+LP,M+MP,0),
     *                       NP,IC,ID,E2(1,NP,IC,ID),
     *                       LP,JC,JD,E2(2,LP,JC,JD),
     *                       MP,KC,KD,E2(3,MP,KC,KD)
C
                             ADDON=  SIGN *
     *                               AMBDA *
     *                               DKL(NN) *
     *                               E2(1,NP,IC,ID) *
     *                               E2(2,LP,JC,JD) *
     *                               E2(3,MP,KC,KD) *
     *                               R(N+NP,L+LP,M+MP,0)
C
                             AIN(IK,MM,NN) = AIN(IK,MM,NN)
     *                                       + ADDON
C
                             IF(TEDBG) WRITE(IW,9994) IK,MM,NN,
     *                           AIN(IK,MM,NN),ADDON
C
 440                         CONTINUE
 460                         CONTINUE
 480                         CONTINUE
C
 500                       CONTINUE
 510                     CONTINUE
 520                   CONTINUE
C
 525                   CONTINUE
 535                   CONTINUE
C
 540                CONTINUE
 560              CONTINUE
C                 end of loop over g, h
C
 580  CONTINUE
 600  CONTINUE
C
C     end of sum over alpha_C, alpha_D (580 & 600)
C
C     loop over g, h for second time
C
      NN=0
C
      LLMAX=MAXL
      DO 420 KKK = MINK,MAXK
C
         K17=LOCK+KKK
C
         DO 400 LLL = MINL,LLMAX
C
            L17=LOCL+LLL
C
            NN=NN+1
C
C           loop over i, j
C
            DOUBLE=.FALSE.
            MM = 0
            DUM1 = ZERO
            DUM2 = ZERO
            JMAX=MAXJ
            DO 380 III = MINI,MAXI
C
               I17=LOCI+III
               IF (III.EQ. 1) DUM1=CSI
               IF (III.EQ. 2) DUM1=CPI
               IF (III.EQ. 5) DUM1=CDI
C
C              The normalization for shell II is done later (sub. disti)
C
               IF((III.EQ. 8).AND.NORM.AND.UPANGK) DUM1=DUM1*SQRT3
               IF (III.EQ.11) DUM1=CFI
               IF((III.EQ.14).AND.NORM.AND.UPANGK) DUM1=DUM1*SQRT5
               IF((III.EQ.20).AND.NORM.AND.UPANGK) DUM1=DUM1*SQRT3
               IF (III.EQ.21) DUM1=CGI
               IF((III.EQ.24).AND.NORM.AND.UPANGK) DUM1=DUM1*SQRT7
               IF((III.EQ.30).AND.NORM.AND.UPANGK) DUM1=DUM1*SQRT5/SQRT3
               IF((III.EQ.33).AND.NORM.AND.UPANGK) DUM1=DUM1*SQRT3
C
               CALL GETKLM(III,IA,JA,KA)
C
               DO 360 JJJ = MINJ,JMAX
C
                  J17=LOCJ+JJJ
C
                  IF (JJJ.EQ.1) THEN
                     DUM2=DUM1*CSJ
                     IF (DOUBLE) THEN
                        IF (III.LE.1) THEN
                           DUM2=DUM2+DUM2
                        ELSE
                           DUM2=DUM2+CSI*CPJ
                        END IF
                     END IF
                  ELSE IF (JJJ.EQ.2) THEN
                     DUM2=DUM1*CPJ
                     IF (DOUBLE) DUM2=DUM2+DUM2
                  ELSE IF (JJJ.EQ.5) THEN
                     DUM2=DUM1*CDJ
                     IF (DOUBLE) DUM2=DUM2+DUM2
                  ELSE IF ((JJJ.EQ.8).AND.NORM) THEN
                     DUM2=DUM2*SQRT3
                  ELSE IF (JJJ.EQ.11) THEN
                     DUM2=DUM1*CFJ
                     IF (DOUBLE) DUM2=DUM2+DUM2
                  ELSE IF ((JJJ.EQ.14).AND.NORM) THEN
                     DUM2=DUM2*SQRT5
                  ELSE IF ((JJJ.EQ.20).AND.NORM) THEN
                     DUM2=DUM2*SQRT3
                  ELSE IF (JJJ.EQ.21) THEN
                     DUM2=DUM1*CGJ
                     IF (DOUBLE) DUM2=DUM2+DUM2
                  ELSE IF ((JJJ.EQ.24).AND.NORM) THEN
                     DUM2=DUM2*SQRT7
                  ELSE IF ((JJJ.EQ.30).AND.NORM) THEN
                     DUM2=DUM2*SQRT5/SQRT3
                  ELSE IF ((JJJ.EQ.33).AND.NORM) THEN
                     DUM2=DUM2*SQRT3
                  END IF
C
                  CALL GETKLM(JJJ,IB,JB,KB)
C
                  IF(TEDBG) WRITE(IW,9993) IG,JG,I17,J17,K17,L17
C
                  MM = MM+1
C
                  DIJ(MM) = DUM2
C
                  NMAX=IA+IB
                  LMAX=JA+JB
                  MMAX=KA+KB
C
C                 Loop over (N,L,M)k
C
                  IK=0
                  DO 340 N=0,NMAX
                  DO 320 L=0,LMAX
                  DO 300 M=0,MMAX
                  IK=IK+1
C
                   IF(TEDBG) WRITE(IW,9992) MM,NN,X(MM,NN),
     *             IK,MM,NN,AIN(IK,MM,NN),MM,DIJ(MM),
     *             N,IA,IB,E1(1,N,IA,IB),
     *             L,JA,JB,E1(2,L,JA,JB),
     *             M,KA,KB,E1(3,M,KA,KB)
C
                   ADDON=DIJ(MM) *
     *                   E1(1,N,IA,IB) *
     *                   E1(2,L,JA,JB) *
     *                   E1(3,M,KA,KB) *
     *                   AIN(IK,MM,NN)
C
                   X(MM,NN)=X(MM,NN)+ADDON
C
                   IF(TEDBG) WRITE(IW,9991) MM,NN,X(MM,NN),ADDON
C
 300              CONTINUE
 320              CONTINUE
 340              CONTINUE
C
 360          CONTINUE
 380       CONTINUE
C          end if loop over i, j
C
 400    CONTINUE
 420  CONTINUE
C     end of loop over g, h
C
 620  CONTINUE
 640  CONTINUE
C     end of sum over alpha_A, alpha_B (640 & 620)
C
C     Now that the primitives are done, we can distibute the integrals
C     into their Cartesian families:
C
      IF(UPANGI) CALL DISTI(TEINTX,TEINTY,TEINTZ,INDI,X,LSHEL,
     *                      MINI,MAXI,MINJ,MAXJ,MINK,MAXK,MINL,MAXL,
     *                      LOCI,LOCJ,LOCK,LOCL)
      IF(UPANGK) CALL DISTK(TEINTX,TEINTY,TEINTZ,INDK,X,LSHEL,
     *                      MINI,MAXI,MINJ,MAXJ,MINK,MAXK,MINL,MAXL,
     *                      LOCI,LOCJ,LOCK,LOCL)
C
 700  CONTINUE
 720  CONTINUE
 660  CONTINUE
 680  CONTINUE
      RETURN
C
 9991 FORMAT(7X,'OUTGOING X(',I2,',',I2,')=',F10.5,5X,'THIS TERM:',
     *       F15.10/)
 9992 FORMAT(7X,'INCOMING X(',I2,',',I2,')=',F10.5,11X,'I(',
     *       I2,',',I2,',',I2,')=',F10.5,5X,'DIJ(',I2,')=',F10.5/
     *       12X,'EX(',I2,',',I2,',',I2,')=',F10.5,
     *       3X,'EY(',I2,',',I2,',',I2,')=',F10.5,
     *       3X,'EZ(',I2,',',I2,',',I2,')=',F10.5)
 9993 FORMAT(4X,'***AFTER TERMS INVOLVING (',I2,I2,'|:',5X,
     *       'AO INTEGRAL: (',I2,I2,'|',I2,I2,')')
 9994 FORMAT(10X,'OUTGOING I(',I2,',',I2,',',I2,')=',F10.5,6X,
     *       'THIS TERM=',F10.5/)
 9995 FORMAT(10X,'INCOMING I(',I2,',',I2,',',I2,')=',F10.5,11X,
     *       'SIGN=',F10.5,9X,'LAMBDA=',F10.5,8X,'DKL(',I2,')=',F10.5/
     *       16X,'R(',I2,',',I2,',',I2,',',' 0)=',F10.5,
     *       3X,'EX(',I2,',',I2,',',I2,')=',F10.5,
     *       3X,'EY(',I2,',',I2,',',I2,')=',F10.5,
     *       3X,'EZ(',I2,',',I2,',',I2,')=',F10.5)
 9997 FORMAT(/5X,'PRIMITIVE TERM:',1X,'(',I2,I2,'|',I2,I2,')',5X,
     *           'S COEFICIENTS:',1X,4F10.5,
     *       /6X,'(',I2,I2,I2,1X,I2,I2,I2,'|',I2,I2,I2,1X,I2,I2,I2,')',
     *        2X,'P COEFICIENTS:',1X,4F10.5)
 9998 FORMAT(/,2X,'SHELLS:',1X,I2,I2,I2,I2/)
 9999 FORMAT(/,3X,'*****DEBUG OUTPUT FOR MCM-DAV 2E INTEGRALS*****')
      END
C*MODULE NMR  *DECK MDTWOEI
      SUBROUTINE MDTWOEI(TEDBG,PTEINT,P0G10X,P0G10Y,P0G10Z,P,
     *                   XIJM,YIJM,ZIJM,AIN,X,R,L2,L2S,NUPANG)
C
C     This calculates the perturbed two-electron integrals by tricking
C     the normal 2-electron code into thinking that the angular momentum
C     on the I or K shell is one unit larger than it is:
C
C     nupang can be 1 or 2:
C
C       1 = up angular momentum on I shell
C       2 = up angular momentum on K shell
C
C     To get away with this, you must reset some of the integral
C     parameters (sub. upshan); reset the coefficients, since they
C     shouldn't change even though the angular momentum does;
C     preserve the old normalization factors, and finally, filter
C     the resulting integrals into a form that won't result in the
C     overwriting of integrals. (disti and distk).  This last step
C     is not trivial, and it must be done at the end of each shell.
C     This is much like mdtwoeia, but we don't assume that any
C     large arrays have been kept in memory.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL UPANGI, UPANGK
      LOGICAL DOUBLE, NORM, TEDBG, PTEINT, LSHEL
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      PARAMETER (ZERO=0.0D+00, SQRT3=1.73205080756888D+00,
     *           SQRT5=2.23606797749979D+00, SQRT7=2.64575131106459D+00,
     *           TPIFH=34.9868366552497D+00, ONE=1.0D+00)
C
      DIMENSION ES1(3),E1(1:3,0:12,0:6,-2:6),
     *          ES2(3),E2(1:3,0:12,0:6,-2:6),
     *          FOFT(0:24),DIJ(225),DKL(225)
      DIMENSION P0G10X(L2S),P0G10Y(L2S),P0G10Z(L2S),P(L2),
     *          XIJM(L2S),YIJM(L2S),ZIJM(L2S),
     *          AIN(75,225,225),X(225,225),R(0:24,0:24,0:24,0:24)
C
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
C
C     tedbg: print detailed two-electron integral debugging info.
C     turn off simple printing of ints (pteint) when this is selected.
C
      IF(TEDBG) PTEINT=.FALSE.
      IF(TEDBG) WRITE(IW,9999)
C
      NORM=NORMP.NE.1 .OR. NORMF.NE.1
C
      IF(PTEINT.OR.TEDBG) WRITE(IW,*)
     *      'FOR THIS SET OF INTEGRALS, NUPANG=',NUPANG
C
      UPANGI=.FALSE.
      UPANGK=.FALSE.
      IF(NUPANG.EQ.1) UPANGI=.TRUE.
      IF(NUPANG.EQ.2) UPANGK=.TRUE.
C
C     ----- I SHELL
C
      DO 680 II = 1,NSHELL
C
         I = KATOM(II)
         XI = C(1,I)
         YI = C(2,I)
         ZI = C(3,I)
         I1 = KSTART(II)
         I2 = I1+KNG(II)-1
         MINI = KMIN(II)
         MAXI = KMAX(II)
         LOCI = KLOC(II)-MINI
         INDI=KLOC(II)
         LIT = KTYPE(II)
C
         IF(UPANGI) CALL UPSHAN(II,MINI,MAXI,LOCI,LSHEL)
C
C     ----- J SHELL
C
         DO 660 JJ = 1,NSHELL
C
            J = KATOM(JJ)
            XJ = C(1,J)
            YJ = C(2,J)
            ZJ = C(3,J)
            J1 = KSTART(JJ)
            J2 = J1+KNG(JJ)-1
            MINJ = KMIN(JJ)
            MAXJ = KMAX(JJ)
            LOCJ = KLOC(JJ)-MINJ
            LJT = KTYPE(JJ)
C
            RX1= XI-XJ
            RY1= YI-YJ
            RZ1= ZI-ZJ
            RX12= RX1*RX1
            RY12= RY1*RY1
            RZ12= RZ1*RZ1
C
C     ----- K SHELL
C
      DO 720 KK = 1,NSHELL
C
C
         K = KATOM(KK)
         XK = C(1,K)
         YK = C(2,K)
         ZK = C(3,K)
         K1 = KSTART(KK)
         K2 = K1+KNG(KK)-1
         MINK = KMIN(KK)
         MAXK = KMAX(KK)
         LOCK = KLOC(KK)-MINK
         INDK=KLOC(KK)
         LKT = KTYPE(KK)
C
         IF(UPANGK) CALL UPSHAN(KK,MINK,MAXK,LOCK,LSHEL)
C
C     ----- L SHELL
C
         DO 700 LL = 1,NSHELL
C
            L = KATOM(LL)
            XL = C(1,L)
            YL = C(2,L)
            ZL = C(3,L)
            L1 = KSTART(LL)
            L2 = L1+KNG(LL)-1
            MINL = KMIN(LL)
            MAXL = KMAX(LL)
            LOCL = KLOC(LL)-MINL
            LLT = KTYPE(LL)
C
            RX2= XK-XL
            RY2= YK-YL
            RZ2= ZK-ZL
            RX22= RX2*RX2
            RY22= RY2*RY2
            RZ22= RZ2*RZ2
C
C     ----- Begin McMurchie-Davidson code ------
C
            MMCLR=(MAXI-MINI+2)*(MAXJ-MINJ+2)
            NNCLR=(MAXK-MINK+2)*(MAXL-MINL+2)
            IKCLR=2*(LIT+1)*(LJT+1)
C
            DO NNTMP=1,NNCLR
            DO MMTMP=1,MMCLR
              X(MMTMP,NNTMP)=ZERO
            ENDDO
            ENDDO
C
            IF(TEDBG.OR.PTEINT) WRITE(IW,9998) II,JJ,KK,LL
C
C           Sum over primitives in I shell contraction
C
            JGMAX = J2
            DO 640 IG = I1,I2
C
            AI  = EX(IG)
            CSI = CS(IG)
            CPI = CP(IG)
            CDI = CD(IG)
            CFI = CF(IG)
            CGI = CG(IG)
C
            IF(UPANGI) THEN
            CPI = CS(IG)
            CDI = CP(IG)
            CFI = CD(IG)
            CGI = CF(IG)
            END IF
C
C           Sum over primitives in J shell contraction
C
            DO 620 JG = J1,JGMAX
C
              AJ  = EX(JG)
              CSJ = CS(JG)
              CPJ = CP(JG)
              CDJ = CD(JG)
              CFJ = CF(JG)
              CGJ = CG(JG)
C
C             Clean out the recursion array E
C
              DO  L=1,3
               DO  M=0,8
                DO  N=0,4
                 DO  I=0,4
                  E1(L,M,N,I)=ZERO
                  E2(L,M,N,I)=ZERO
                 ENDDO
                ENDDO
               ENDDO
              ENDDO
C
C       Coefficients depend on this particular alpha(i) and alpha(j):
C       Return e1([x,y,z],ip,ia,ib) (The recurrance formulas.)
C
              CALL EAB(ES1,AI,AJ,RX12,RY12,RZ12)
C
              CALL RECURS(ES1,AI,AJ,RX1,RY1,RZ1,LIT,LJT,E1)
C
C       "Zero I" Still may be a better way...
C
              DO IKTMP=0,IKCLR
              DO MMTMP=0,MMCLR
              DO NNTMP=0,NNCLR
C
                AIN(IKTMP,MMTMP,NNTMP)=ZERO
C
              ENDDO
              ENDDO
              ENDDO
C
C             Sum over primitives in K shell contraction
C
              LGMAX = L2
              DO 600 KG = K1,K2
C
              AK  = EX(KG)
              CSK = CS(KG)
              CPK = CP(KG)
              CDK = CD(KG)
              CFK = CF(KG)
              CGK = CG(KG)
C
              IF(UPANGK) THEN
              CPK = CS(KG)
              CDK = CP(KG)
              CFK = CD(KG)
              CGK = CF(KG)
              END IF
C
C             Sum over primitives in L shell contraction
C
              DO 580 LG = L1,LGMAX
C
                AL  = EX(LG)
                CSL = CS(LG)
                CPL = CP(LG)
                CDL = CD(LG)
                CFL = CF(LG)
                CGL = CG(LG)
C
C         Coefficients depend on this particular alpha(k) and alpha(l):
C         Return e2([x,y,z],ip,ia,ib) (The recurrance formulas.)
C
                CALL EAB(ES2,AK,AL,RX22,RY22,RZ22)
C
                CALL RECURS(ES2,AK,AL,RX2,RY2,RZ2,LKT,LLT,E2)
C
C         Compute lambda, fofj(T)
C
                AMBDA=TPIFH/((AI+AJ)*(AK+AL)*SQRT(AI+AJ+AK+AL))
C
                MXLIJT = LIT + LJT + LKT + LLT - 3
C
                PX = (AI*XI+AJ*XJ)/(AI+AJ)
                PY = (AI*YI+AJ*YJ)/(AI+AJ)
                PZ = (AI*ZI+AJ*ZJ)/(AI+AJ)
C
                QX = (AK*XK+AL*XL)/(AK+AL)
                QY = (AK*YK+AL*YL)/(AK+AL)
                QZ = (AK*ZK+AL*ZL)/(AK+AL)
C
                PQX = PX - QX
                PQY = PY - QY
                PQZ = PZ - QZ
C
                ALPHA = ((AI+AJ)*(AK+AL))/(AI+AJ+AK+AL)
C
                T = ALPHA * (PQX**2 + PQY**2 + PQZ**2)
C
                CALL FOFTJ(MXLIJT,T,FOFT)
C
C               Compute R_nlm
C
                CALL RNLM(MXLIJT,PQX,PQY,PQZ,ALPHA,FOFT,R)
C
C               Compute primitive coefficients (loop over g, h)
C                                              (counted by nn)
C
                 DOUBLE=.FALSE.
                 NN = 0
                 DUM1 = ZERO
                 DUM2 = ZERO
                 LLMAX=MAXL
                 DO 560 KKK = MINK,MAXK
C
                    IF (KKK.EQ.1) DUM1=CSK
                    IF (KKK.EQ.2) DUM1=CPK
                    IF (KKK.EQ.5) DUM1=CDK
C
C             The normalization for shell KK is done later (sub. distk)
C
                    IF ((KKK.EQ. 8).AND.NORM.AND.UPANGI) DUM1=DUM1*SQRT3
                    IF (KKK.EQ.11) DUM1=CFK
                    IF ((KKK.EQ.14).AND.NORM.AND.UPANGI) DUM1=DUM1*SQRT5
                    IF ((KKK.EQ.20).AND.NORM.AND.UPANGI) DUM1=DUM1*SQRT3
                    IF (KKK.EQ.21) DUM1=CGK
                    IF ((KKK.EQ.24).AND.NORM.AND.UPANGI) DUM1=DUM1*SQRT7
                    IF ((KKK.EQ.30).AND.NORM.AND.UPANGI)
     *                                             DUM1=DUM1*SQRT5/SQRT3
                    IF ((KKK.EQ.33).AND.NORM.AND.UPANGI) DUM1=DUM1*SQRT3
C
                    CALL GETKLM(KKK,IC,JC,KC)
C
                    DO 540 LLL = MINL,LLMAX
C
                       IF (LLL.EQ.1) THEN
                          DUM2=DUM1*CSL
                          IF (DOUBLE) THEN
                             IF (KKK.LE.1) THEN
                                DUM2=DUM2+DUM2
                             ELSE
                                DUM2=DUM2+CSK*CPL
                             END IF
                          END IF
                       ELSE IF (LLL.EQ.2) THEN
                          DUM2=DUM1*CPL
                          IF (DOUBLE) DUM2=DUM2+DUM2
                       ELSE IF (LLL.EQ.5) THEN
                          DUM2=DUM1*CDL
                          IF (DOUBLE) DUM2=DUM2+DUM2
                       ELSE IF ((LLL.EQ.8).AND.NORM) THEN
                          DUM2=DUM2*SQRT3
                       ELSE IF (LLL.EQ.11) THEN
                          DUM2=DUM1*CFL
                          IF (DOUBLE) DUM2=DUM2+DUM2
                       ELSE IF ((LLL.EQ.14).AND.NORM) THEN
                          DUM2=DUM2*SQRT5
                       ELSE IF ((LLL.EQ.20).AND.NORM) THEN
                          DUM2=DUM2*SQRT3
                       ELSE IF (LLL.EQ.21) THEN
                          DUM2=DUM1*CGL
                          IF (DOUBLE) DUM2=DUM2+DUM2
                       ELSE IF ((LLL.EQ.24).AND.NORM) THEN
                          DUM2=DUM2*SQRT7
                       ELSE IF ((LLL.EQ.30).AND.NORM) THEN
                          DUM2=DUM2*SQRT5/SQRT3
                       ELSE IF ((LLL.EQ.33).AND.NORM) THEN
                          DUM2=DUM2*SQRT3
                       END IF
C
                       CALL GETKLM(LLL,ID,JD,KD)
C
                       NN = NN+1
C
                       DKL(NN) = DUM2
C
                       NPMAX=IC+ID
                       LPMAX=JC+JD
                       MPMAX=KC+KD
C
                       MM=0
C
                       JMAX=MAXJ
                       DO 535 III=MINI,MAXI
                         CALL GETKLM(III,IA,JA,KA)
C
                       DO 525 JJJ=MINJ,JMAX
C
                         CALL GETKLM(JJJ,IB,JB,KB)
C
                       MM=MM+1
C
                       IF(TEDBG) WRITE(IW,9997) IG,JG,KG,LG,
     *                                CSI,CSJ,CSK,CSL,
     *                  IA,JA,KA,IB,JB,KB,IC,JC,KC,ID,JD,KD,
     *                                CPI,CPJ,CPK,CPL
C
C
                       NMAX=IA+IB
                       LMAX=JA+JB
                       MMAX=KA+KB
C
C                      Sum over (N',L',M')k'
C
                       DO 520 NP=0,NPMAX
                         DO 510 LP=0,LPMAX
                           DO 500 MP=0,MPMAX
C
                             SIGN=(-ONE)**(NP+LP+MP)
C
C                            Loop over (N,L,M)k (ik)
C
                             IK=0
                             DO 480 N=0,NMAX
                             DO 460 L=0,LMAX
                             DO 440 M=0,MMAX
                             IK=IK+1
C
                             IF(TEDBG) WRITE(IW,9995) IK,MM,NN,
     *                       AIN(IK,MM,NN),
     *                       SIGN,AMBDA,NN,DKL(NN),
     *                       N+NP,L+LP,M+MP,R(N+NP,L+LP,M+MP,0),
     *                       NP,IC,ID,E2(1,NP,IC,ID),
     *                       LP,JC,JD,E2(2,LP,JC,JD),
     *                       MP,KC,KD,E2(3,MP,KC,KD)
C
                             ADDON=  SIGN *
     *                               AMBDA *
     *                               DKL(NN) *
     *                               E2(1,NP,IC,ID) *
     *                               E2(2,LP,JC,JD) *
     *                               E2(3,MP,KC,KD) *
     *                               R(N+NP,L+LP,M+MP,0)
C
                             AIN(IK,MM,NN) = AIN(IK,MM,NN)
     *                                       + ADDON
C
                             IF(TEDBG) WRITE(IW,9994) IK,MM,NN,
     *                           AIN(IK,MM,NN),ADDON
C
 440                         CONTINUE
 460                         CONTINUE
 480                         CONTINUE
C
 500                       CONTINUE
 510                     CONTINUE
 520                   CONTINUE
C
 525                   CONTINUE
 535                   CONTINUE
C
 540                CONTINUE
 560              CONTINUE
C                 end of loop over g, h
C
 580  CONTINUE
 600  CONTINUE
C
C     end of sum over alpha_C, alpha_D (580 & 600)
C
C     loop over g, h for second time
C
      NN=0
C
      LLMAX=MAXL
      DO 420 KKK = MINK,MAXK
C
         K17=LOCK+KKK
C
         DO 400 LLL = MINL,LLMAX
C
            L17=LOCL+LLL
C
            NN=NN+1
C
C           loop over i, j
C
            DOUBLE=.FALSE.
            MM = 0
            DUM1 = ZERO
            DUM2 = ZERO
            JMAX=MAXJ
            DO 380 III = MINI,MAXI
C
               I17=LOCI+III
               IF (III.EQ. 1) DUM1=CSI
               IF (III.EQ. 2) DUM1=CPI
               IF (III.EQ. 5) DUM1=CDI
C
C              The normalization for shell II is done later (sub. disti)
C
               IF((III.EQ. 8).AND.NORM.AND.UPANGK) DUM1=DUM1*SQRT3
               IF (III.EQ.11) DUM1=CFI
               IF((III.EQ.14).AND.NORM.AND.UPANGK) DUM1=DUM1*SQRT5
               IF((III.EQ.20).AND.NORM.AND.UPANGK) DUM1=DUM1*SQRT3
               IF (III.EQ.21) DUM1=CGI
               IF((III.EQ.24).AND.NORM.AND.UPANGK) DUM1=DUM1*SQRT7
               IF((III.EQ.30).AND.NORM.AND.UPANGK) DUM1=DUM1*SQRT5/SQRT3
               IF((III.EQ.33).AND.NORM.AND.UPANGK) DUM1=DUM1*SQRT3
C
               CALL GETKLM(III,IA,JA,KA)
C
               DO 360 JJJ = MINJ,JMAX
C
                  J17=LOCJ+JJJ
C
                  IF (JJJ.EQ.1) THEN
                     DUM2=DUM1*CSJ
                     IF (DOUBLE) THEN
                        IF (III.LE.1) THEN
                           DUM2=DUM2+DUM2
                        ELSE
                           DUM2=DUM2+CSI*CPJ
                        END IF
                     END IF
                  ELSE IF (JJJ.EQ.2) THEN
                     DUM2=DUM1*CPJ
                     IF (DOUBLE) DUM2=DUM2+DUM2
                  ELSE IF (JJJ.EQ.5) THEN
                     DUM2=DUM1*CDJ
                     IF (DOUBLE) DUM2=DUM2+DUM2
                  ELSE IF ((JJJ.EQ.8).AND.NORM) THEN
                     DUM2=DUM2*SQRT3
                  ELSE IF (JJJ.EQ.11) THEN
                     DUM2=DUM1*CFJ
                     IF (DOUBLE) DUM2=DUM2+DUM2
                  ELSE IF ((JJJ.EQ.14).AND.NORM) THEN
                     DUM2=DUM2*SQRT5
                  ELSE IF ((JJJ.EQ.20).AND.NORM) THEN
                     DUM2=DUM2*SQRT3
                  ELSE IF (JJJ.EQ.21) THEN
                     DUM2=DUM1*CGJ
                     IF (DOUBLE) DUM2=DUM2+DUM2
                  ELSE IF ((JJJ.EQ.24).AND.NORM) THEN
                     DUM2=DUM2*SQRT7
                  ELSE IF ((JJJ.EQ.30).AND.NORM) THEN
                     DUM2=DUM2*SQRT5/SQRT3
                  ELSE IF ((JJJ.EQ.33).AND.NORM) THEN
                     DUM2=DUM2*SQRT3
                  END IF
C
                  CALL GETKLM(JJJ,IB,JB,KB)
C
                  IF(TEDBG) WRITE(IW,9993) IG,JG,I17,J17,K17,L17
C
                  MM = MM+1
C
                  DIJ(MM) = DUM2
C
                  NMAX=IA+IB
                  LMAX=JA+JB
                  MMAX=KA+KB
C
C                 Loop over (N,L,M)k
C
                  IK=0
                  DO 340 N=0,NMAX
                  DO 320 L=0,LMAX
                  DO 300 M=0,MMAX
                  IK=IK+1
C
                   IF(TEDBG) WRITE(IW,9992) MM,NN,X(MM,NN),
     *             IK,MM,NN,AIN(IK,MM,NN),MM,DIJ(MM),
     *             N,IA,IB,E1(1,N,IA,IB),
     *             L,JA,JB,E1(2,L,JA,JB),
     *             M,KA,KB,E1(3,M,KA,KB)
C
                   ADDON=DIJ(MM) *
     *                   E1(1,N,IA,IB) *
     *                   E1(2,L,JA,JB) *
     *                   E1(3,M,KA,KB) *
     *                   AIN(IK,MM,NN)
C
                   X(MM,NN)=X(MM,NN)+ADDON
C
                   IF(TEDBG) WRITE(IW,9991) MM,NN,X(MM,NN),ADDON
C
 300              CONTINUE
 320              CONTINUE
 340              CONTINUE
C
 360          CONTINUE
 380       CONTINUE
C          end if loop over i, j
C
 400    CONTINUE
 420  CONTINUE
C     end of loop over g, h
C
 620  CONTINUE
 640  CONTINUE
C     end of sum over alpha_A, alpha_B (640 & 620)
C
C     Now that the primitives are done, we can distibute the integrals
C     into their Cartesian families:
C
      IF(UPANGI) CALL DISTIG10(P0G10X,P0G10Y,P0G10Z,P,XIJM,YIJM,ZIJM,
     *                        INDI,X,LSHEL,L2,L2S,MINI,MAXI,MINJ,MAXJ,
     *                        MINK,MAXK,MINL,MAXL,LOCI,LOCJ,LOCK,LOCL)
      IF(UPANGK) CALL DISTKG10(P0G10X,P0G10Y,P0G10Z,P,XIJM,YIJM,ZIJM,
     *                        INDK,X,LSHEL,L2,L2S,MINI,MAXI,MINJ,MAXJ,
     *                        MINK,MAXK,MINL,MAXL,LOCI,LOCJ,LOCK,LOCL)
C
 700  CONTINUE
 720  CONTINUE
 660  CONTINUE
 680  CONTINUE
      RETURN
C
 9991 FORMAT(7X,'OUTGOING X(',I2,',',I2,')=',F10.5,5X,'THIS TERM:',
     *       F15.10/)
 9992 FORMAT(7X,'INCOMING X(',I2,',',I2,')=',F10.5,11X,'I(',
     *       I2,',',I2,',',I2,')=',F10.5,5X,'DIJ(',I2,')=',F10.5/
     *       12X,'EX(',I2,',',I2,',',I2,')=',F10.5,
     *       3X,'EY(',I2,',',I2,',',I2,')=',F10.5,
     *       3X,'EZ(',I2,',',I2,',',I2,')=',F10.5)
 9993 FORMAT(4X,'***AFTER TERMS INVOLVING (',I2,I2,'|:',5X,
     *       'AO INTEGRAL: (',I2,I2,'|',I2,I2,')')
 9994 FORMAT(10X,'OUTGOING I(',I2,',',I2,',',I2,')=',F10.5,6X,
     *       'THIS TERM=',F10.5/)
 9995 FORMAT(10X,'INCOMING I(',I2,',',I2,',',I2,')=',F10.5,11X,
     *       'SIGN=',F10.5,9X,'LAMBDA=',F10.5,8X,'DKL(',I2,')=',F10.5/
     *       16X,'R(',I2,',',I2,',',I2,',',' 0)=',F10.5,
     *       3X,'EX(',I2,',',I2,',',I2,')=',F10.5,
     *       3X,'EY(',I2,',',I2,',',I2,')=',F10.5,
     *       3X,'EZ(',I2,',',I2,',',I2,')=',F10.5)
 9997 FORMAT(/5X,'PRIMITIVE TERM:',1X,'(',I2,I2,'|',I2,I2,')',5X,
     *           'S COEFICIENTS:',1X,4F10.5,
     *       /6X,'(',I2,I2,I2,1X,I2,I2,I2,'|',I2,I2,I2,1X,I2,I2,I2,')',
     *        2X,'P COEFICIENTS:',1X,4F10.5)
 9998 FORMAT(/,2X,'SHELLS:',1X,I2,I2,I2,I2/)
 9999 FORMAT(/,3X,'*****DEBUG OUTPUT FOR MCM-DAV 2E INTEGRALS*****')
      END
C*MODULE NMR  *DECK UPSHAN
      SUBROUTINE UPSHAN(I,MIN,MAX,LOC,LSHEL)
C
C     This redefines the integral parameters to up the angular momentum
C     on the ith shell by one.  L shells are a little tricky.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL LSHEL
C
      PARAMETER (MXSH=1000, MXGTOT=5000)
C
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
C
      LSHEL=.FALSE.
C
      IF(KTYPE(I).EQ.1) THEN
       MIN=2
       MAX=4
      ELSE IF(KTYPE(I).EQ.2) THEN
       MIN=5
       MAX=10
       IF(KMIN(I).EQ.1) THEN
         MIN=2
         LSHEL=.TRUE.
       END IF
      ELSE IF(KTYPE(I).EQ.3) THEN
       MIN=11
       MAX=20
      END IF
C
      LOC=0
C
      RETURN
      END
C*MODULE NMR  *DECK DISTI
      SUBROUTINE DISTI(TEINT1,TEINT2,TEINT3,IND,X,LSHEL,
     *                 MINI,MAXI,MINJ,MAXJ,MINK,MAXK,MINL,MAXL,
     *                 LOCI,LOCJ,LOCK,LOCL)
C
C     This DISTributes the integrals based on the I shell and
C     renormalizes them correctly.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL LSHEL,DBG
C
      PARAMETER (MXATM=500)
      PARAMETER (SQRT3=1.73205080756888D+00,
     *           SQRT5=2.23606797749979D+00)
C
      DIMENSION TEINT1(NUM,NUM,NUM,NUM)
      DIMENSION TEINT2(NUM,NUM,NUM,NUM)
      DIMENSION TEINT3(NUM,NUM,NUM,NUM)
      DIMENSION X(225,225)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
      DBG=.FALSE.
C
      NN = 0
      DO 421 K = MINK,MAXK
         K17 = LOCK + K
         DO 401 L = MINL,MAXL
            L17 = LOCL + L
            NN = NN  + 1
            MM = 0
            DO 381 I = MINI,MAXI
               I17 = LOCI + I
               DO 361 J = MINJ,MAXJ
                  J17 = LOCJ + J
                  MM = MM + 1
C
C     It's not possible to have i17=1, because the angular momentum
C     has been increased by one unit on the I shell.
C
C     P functions
C
      IF(I17.EQ.2) THEN
        TEINT1(IND  ,J17,K17,L17)=X(MM,NN)
        IF(DBG) THEN
        WRITE(IW,1111) 1,IND  ,J17,K17,L17,TEINT1(IND  ,J17,K17,L17)
        END IF
C
      ELSE IF(I17.EQ.3) THEN
        TEINT2(IND  ,J17,K17,L17)=X(MM,NN)
        IF(DBG) THEN
        WRITE(IW,1111) 2,IND  ,J17,K17,L17,TEINT2(IND  ,J17,K17,L17)
        END IF
C
      ELSE IF(I17.EQ.4) THEN
        TEINT3(IND  ,J17,K17,L17)=X(MM,NN)
        IF(DBG) THEN
        WRITE(IW,1111) 3,IND  ,J17,K17,L17,TEINT3(IND  ,J17,K17,L17)
        END IF
C
C     D functions:
C          (these may have been generated from an L or P shell...)
C  teint1 xx <- xx
C         xy    yy
C         xz    zz
C  teint2 yx    xy
C         yy    xz
C         yz    yz
C  teint3 zx
C         zy
C         zz
C
      ELSE IF(I17.EQ.5) THEN
        IF(LSHEL) THEN
          TEINT1(IND+1,J17,K17,L17)=X(MM,NN)
        ELSE
          TEINT1(IND  ,J17,K17,L17)=X(MM,NN)
        END IF
        IF(DBG) THEN
        IF(LSHEL) THEN
        WRITE(IW,1111) 1,IND+1,J17,K17,L17,TEINT1(IND+1,J17,K17,L17)
        ELSE
        WRITE(IW,1111) 1,IND  ,J17,K17,L17,TEINT1(IND  ,J17,K17,L17)
        END IF
        END IF
C
      ELSE IF(I17.EQ.6) THEN
        IF(LSHEL) THEN
          TEINT2(IND+2,J17,K17,L17)=X(MM,NN)
        ELSE
          TEINT2(IND+1,J17,K17,L17)=X(MM,NN)
        END IF
        IF(DBG) THEN
        IF(LSHEL) THEN
        WRITE(IW,1111) 2,IND+2,J17,K17,L17,TEINT2(IND+2,J17,K17,L17)
        ELSE
        WRITE(IW,1111) 2,IND+1,J17,K17,L17,TEINT2(IND+1,J17,K17,L17)
        END IF
        END IF
C
      ELSE IF(I17.EQ.7) THEN
        IF(LSHEL) THEN
          TEINT3(IND+3,J17,K17,L17)=X(MM,NN)
        ELSE
          TEINT3(IND+2,J17,K17,L17)=X(MM,NN)
        END IF
        IF(DBG) THEN
        IF(LSHEL) THEN
        WRITE(IW,1111) 3,IND+3,J17,K17,L17,TEINT3(IND+3,J17,K17,L17)
        ELSE
        WRITE(IW,1111) 3,IND+2,J17,K17,L17,TEINT3(IND+2,J17,K17,L17)
        END IF
        END IF
C
      ELSE IF(I17.EQ.8) THEN
        IF(LSHEL) THEN
          TEINT1(IND+2,J17,K17,L17)=X(MM,NN)
          TEINT2(IND+1,J17,K17,L17)=X(MM,NN)
        ELSE
          TEINT1(IND+1,J17,K17,L17)=X(MM,NN)
          TEINT2(IND  ,J17,K17,L17)=X(MM,NN)
        END IF
        IF(DBG) THEN
        IF(LSHEL) THEN
        WRITE(IW,1111) 1,IND+2,J17,K17,L17,TEINT1(IND+2,J17,K17,L17)
        WRITE(IW,1111) 2,IND+1,J17,K17,L17,TEINT2(IND+1,J17,K17,L17)
        ELSE
        WRITE(IW,1111) 1,IND+1,J17,K17,L17,TEINT1(IND+1,J17,K17,L17)
        WRITE(IW,1111) 2,IND  ,J17,K17,L17,TEINT2(IND  ,J17,K17,L17)
        END IF
        END IF
C
      ELSE IF(I17.EQ.9) THEN
        IF(LSHEL) THEN
          TEINT1(IND+3,J17,K17,L17)=X(MM,NN)
          TEINT3(IND+1,J17,K17,L17)=X(MM,NN)
        ELSE
          TEINT1(IND+2,J17,K17,L17)=X(MM,NN)
          TEINT3(IND  ,J17,K17,L17)=X(MM,NN)
        END IF
        IF(DBG) THEN
        IF(LSHEL) THEN
        WRITE(IW,1111) 1,IND+3,J17,K17,L17,TEINT1(IND+3,J17,K17,L17)
        WRITE(IW,1111) 3,IND+1,J17,K17,L17,TEINT3(IND+1,J17,K17,L17)
        ELSE
        WRITE(IW,1111) 1,IND+2,J17,K17,L17,TEINT1(IND+2,J17,K17,L17)
        WRITE(IW,1111) 3,IND  ,J17,K17,L17,TEINT3(IND  ,J17,K17,L17)
        END IF
        END IF
C
      ELSE IF(I17.EQ.10) THEN
        IF(LSHEL) THEN
          TEINT2(IND+3,J17,K17,L17)=X(MM,NN)
          TEINT3(IND+2,J17,K17,L17)=X(MM,NN)
        ELSE
          TEINT2(IND+2,J17,K17,L17)=X(MM,NN)
          TEINT3(IND+1,J17,K17,L17)=X(MM,NN)
        END IF
        IF(DBG) THEN
        IF(LSHEL) THEN
        WRITE(IW,1111) 2,IND+3,J17,K17,L17,TEINT2(IND+3,J17,K17,L17)
        WRITE(IW,1111) 3,IND+2,J17,K17,L17,TEINT3(IND+2,J17,K17,L17)
        ELSE
        WRITE(IW,1111) 2,IND+2,J17,K17,L17,TEINT2(IND+2,J17,K17,L17)
        WRITE(IW,1111) 3,IND+1,J17,K17,L17,TEINT3(IND+1,J17,K17,L17)
        END IF
        END IF
C
C     F functions
C
      ELSE IF(I17.EQ.11) THEN
        TEINT1(IND  ,J17,K17,L17)=X(MM,NN)
C
      ELSE IF(I17.EQ.12) THEN
        TEINT2(IND+1,J17,K17,L17)=X(MM,NN)
C
      ELSE IF(I17.EQ.13) THEN
        TEINT3(IND+2,J17,K17,L17)=X(MM,NN)
C
      ELSE IF(I17.EQ.14) THEN
        TEINT1(IND+3,J17,K17,L17)=X(MM,NN)*SQRT3
        TEINT2(IND  ,J17,K17,L17)=X(MM,NN)
C
      ELSE IF(I17.EQ.15) THEN
        TEINT1(IND+4,J17,K17,L17)=X(MM,NN)*SQRT3
        TEINT3(IND  ,J17,K17,L17)=X(MM,NN)
C
      ELSE IF(I17.EQ.16) THEN
        TEINT1(IND+1,J17,K17,L17)=X(MM,NN)
        TEINT2(IND+3,J17,K17,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.17) THEN
        TEINT2(IND+5,J17,K17,L17)=X(MM,NN)*SQRT3
        TEINT3(IND+1,J17,K17,L17)=X(MM,NN)
C
      ELSE IF(I17.EQ.18) THEN
        TEINT1(IND+2,J17,K17,L17)=X(MM,NN)
        TEINT3(IND+4,J17,K17,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.19) THEN
        TEINT2(IND+2,J17,K17,L17)=X(MM,NN)
        TEINT3(IND+5,J17,K17,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.20) THEN
        TEINT1(IND+5,J17,K17,L17)=X(MM,NN)*SQRT3
        TEINT2(IND+4,J17,K17,L17)=X(MM,NN)*SQRT3
        TEINT3(IND+3,J17,K17,L17)=X(MM,NN)*SQRT3
C
C     G functions
C
      ELSE IF(I17.EQ.21) THEN
        TEINT1(IND  ,J17,K17,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.22) THEN
        TEINT2(IND+1,J17,K17,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.23) THEN
        TEINT3(IND+2,J17,K17,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.24) THEN
        TEINT1(IND+3,J17,K17,L17)=X(MM,NN)*SQRT5
        TEINT2(IND  ,J17,K17,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.25) THEN
        TEINT1(IND+4,J17,K17,L17)=X(MM,NN)*SQRT5
        TEINT3(IND  ,J17,K17,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.26) THEN
        TEINT1(IND+1,J17,K17,L17)=X(MM,NN)*SQRT3
        TEINT2(IND+5,J17,K17,L17)=X(MM,NN)*SQRT5
C
      ELSE IF(I17.EQ.27) THEN
        TEINT2(IND+6,J17,K17,L17)=X(MM,NN)*SQRT5
        TEINT3(IND+1,J17,K17,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.28) THEN
        TEINT1(IND+2,J17,K17,L17)=X(MM,NN)*SQRT3
        TEINT3(IND+7,J17,K17,L17)=X(MM,NN)*SQRT5
C
      ELSE IF(I17.EQ.29) THEN
        TEINT2(IND+2,J17,K17,L17)=X(MM,NN)*SQRT3
        TEINT3(IND+8,J17,K17,L17)=X(MM,NN)*SQRT5
C
      ELSE IF(I17.EQ.30) THEN
        TEINT1(IND+5,J17,K17,L17)=X(MM,NN)*SQRT5
        TEINT2(IND+3,J17,K17,L17)=X(MM,NN)*SQRT5
C
      ELSE IF(I17.EQ.31) THEN
        TEINT1(IND+7,J17,K17,L17)=X(MM,NN)*SQRT5
        TEINT3(IND+4,J17,K17,L17)=X(MM,NN)*SQRT5
C
      ELSE IF(I17.EQ.32) THEN
        TEINT2(IND+8,J17,K17,L17)=X(MM,NN)*SQRT5
        TEINT3(IND+6,J17,K17,L17)=X(MM,NN)*SQRT5
C
      ELSE IF(I17.EQ.33) THEN
        TEINT1(IND+9,J17,K17,L17)=X(MM,NN)*SQRT3
        TEINT2(IND+4,J17,K17,L17)=X(MM,NN)*SQRT5
        TEINT3(IND+3,J17,K17,L17)=X(MM,NN)*SQRT5
C
      ELSE IF(I17.EQ.34) THEN
        TEINT1(IND+6,J17,K17,L17)=X(MM,NN)*SQRT5
        TEINT2(IND+9,J17,K17,L17)=X(MM,NN)*SQRT3
        TEINT3(IND+5,J17,K17,L17)=X(MM,NN)*SQRT5
C
      ELSE IF(I17.EQ.35) THEN
        TEINT1(IND+8,J17,K17,L17)=X(MM,NN)*SQRT5
        TEINT2(IND+7,J17,K17,L17)=X(MM,NN)*SQRT5
        TEINT3(IND+9,J17,K17,L17)=X(MM,NN)*SQRT3
C
      END IF
C
C     One could envision going from G -> H functions this way, too...
C
 361  CONTINUE
 381  CONTINUE
 401  CONTINUE
 421  CONTINUE
C
      RETURN
 1111 FORMAT(4X,I3,'(',I3,I3,'|',I3,I3,')',F25.15)
      END
C*MODULE NMR  *DECK DISTIG10
      SUBROUTINE DISTIG10(P0G10X,P0G10Y,P0G10Z,P,XIJM,YIJM,ZIJM,
     *                    IND,X,LSHEL,L2,L2S,MINI,MAXI,MINJ,MAXJ,
     *                    MINK,MAXK,MINL,MAXL,LOCI,LOCJ,LOCK,LOCL)
C
C     This DISTributes the integrals based on the I shell and
C     renormalizes them correctly.  It is basically the same as
C     DISTI, but this routine also does the same work as G10 (or
C     G10disk):  it forms the P(0)*G(1,0) matrix for each of the
C     three cartesian directions.  The advantage to constructing
C     this matrix here is that we never have to store the six
C     NUM**4 integral arrays.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL LSHEL
C
      PARAMETER (SQRT3=1.73205080756888D+00,
     *           SQRT5=2.23606797749979D+00,
     *           ZERO=0.0D+00, HALF=0.5D+00, ONE=1.0D+00)
C
      DIMENSION X(225,225),P0G10X(L2S),P0G10Y(L2S),P0G10Z(L2S),
     *          P(L2),XIJM(L2S),YIJM(L2S),ZIJM(L2S)
C
      NN = 0
      DO 421 K = MINK,MAXK
         K17 = LOCK + K
         DO 401 L = MINL,MAXL
            L17 = LOCL + L
            NN = NN  + 1
            MM = 0
            DO 381 I = MINI,MAXI
               I17 = LOCI + I
               DO 361 J = MINJ,MAXJ
                  J17 = LOCJ + J
                  MM = MM + 1
C
      I1 = 0
      I2 = 0
      I3 = 0
      X1 = ZERO
      X2 = ZERO
      X3 = ZERO
C
C     It's not possible to have i17=1, because the angular momentum
C     has been increased by one unit on the I shell.
C
C     P functions
C
      IF(I17.EQ.2) THEN
        I1 = IND
        X1 = X(MM,NN)
C
      ELSE IF(I17.EQ.3) THEN
        I2 = IND
        X2 = X(MM,NN)
C
      ELSE IF(I17.EQ.4) THEN
        I3 = IND
        X3 = X(MM,NN)
C
C     D functions:
C          (these might have been generated from an L or P shell...)
C  teint1 xx <- xx
C         xy    yy
C         xz    zz
C  teint2 yx    xy
C         yy    xz
C         yz    yz
C  teint3 zx
C         zy
C         zz
C
      ELSE IF(I17.EQ.5) THEN
        IF(LSHEL) THEN
          I1 = IND+1
          X1 = X(MM,NN)
        ELSE
          I1 = IND
          X1 = X(MM,NN)
        END IF
C
      ELSE IF(I17.EQ.6) THEN
        IF(LSHEL) THEN
          I2 = IND+2
          X2 = X(MM,NN)
        ELSE
          I2 = IND+1
          X2 = X(MM,NN)
        END IF
C
      ELSE IF(I17.EQ.7) THEN
        IF(LSHEL) THEN
          I3 = IND+3
          X3 = X(MM,NN)
        ELSE
          I3 = IND+2
          X3 = X(MM,NN)
        END IF
C
      ELSE IF(I17.EQ.8) THEN
        IF(LSHEL) THEN
          I1 = IND+2
          I2 = IND+1
          X1 = X(MM,NN)
          X2 = X(MM,NN)
        ELSE
          I1 = IND+1
          I2 = IND
          X1 = X(MM,NN)
          X2 = X(MM,NN)
        END IF
C
      ELSE IF(I17.EQ.9) THEN
        IF(LSHEL) THEN
          I1 = IND+3
          I3 = IND+1
          X1 = X(MM,NN)
          X3 = X(MM,NN)
        ELSE
          I1 = IND+2
          I3 = IND
          X1 = X(MM,NN)
          X3 = X(MM,NN)
        END IF
C
      ELSE IF(I17.EQ.10) THEN
        IF(LSHEL) THEN
          I2 = IND+3
          I3 = IND+2
          X2 = X(MM,NN)
          X3 = X(MM,NN)
        ELSE
          I2 = IND+2
          I3 = IND+1
          X2 = X(MM,NN)
          X3 = X(MM,NN)
        END IF
C
C     F functions
C
      ELSE IF(I17.EQ.11) THEN
        I1 = IND
        X1 = X(MM,NN)
C
      ELSE IF(I17.EQ.12) THEN
        I2 = IND+1
        X2 = X(MM,NN)
C
      ELSE IF(I17.EQ.13) THEN
        I3 = IND+2
        X3 = X(MM,NN)
C
      ELSE IF(I17.EQ.14) THEN
        I1 = IND+3
        I2 = IND
        X1 = X(MM,NN)*SQRT3
        X2 = X(MM,NN)
C
      ELSE IF(I17.EQ.15) THEN
        I1 = IND+4
        I3 = IND
        X1 = X(MM,NN)*SQRT3
        X3 = X(MM,NN)
C
      ELSE IF(I17.EQ.16) THEN
        I1 = IND+1
        I2 = IND+3
        X1 = X(MM,NN)
        X2 = X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.17) THEN
        I2 = IND+5
        I3 = IND+1
        X2 = X(MM,NN)*SQRT3
        X3 = X(MM,NN)
C
      ELSE IF(I17.EQ.18) THEN
        I1 = IND+2
        I3 = IND+4
        X1 = X(MM,NN)
        X3 = X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.19) THEN
        I2 = IND+2
        I3 = IND+5
        X2 = X(MM,NN)
        X3 = X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.20) THEN
        I1 = IND+5
        I2 = IND+4
        I3 = IND+3
        X1 = X(MM,NN)*SQRT3
        X2 = X(MM,NN)*SQRT3
        X3 = X(MM,NN)*SQRT3
C
C     G functions
C
      ELSE IF(I17.EQ.21) THEN
        I1 = IND
        X1 = X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.22) THEN
        I2 = IND+1
        X2 = X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.23) THEN
        I3 = IND+2
        X3 = X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.24) THEN
        I1 = IND+3
        I2 = IND
        X1 = X(MM,NN)*SQRT5
        X2 = X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.25) THEN
        I1 = IND+4
        I3 = IND
        X1 = X(MM,NN)*SQRT5
        X3 = X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.26) THEN
        I1 = IND+1
        I2 = IND+5
        X1 = X(MM,NN)*SQRT3
        X2 = X(MM,NN)*SQRT5
C
      ELSE IF(I17.EQ.27) THEN
        I2 = IND+6
        I3 = IND+1
        X2 = X(MM,NN)*SQRT5
        X3 = X(MM,NN)*SQRT3
C
      ELSE IF(I17.EQ.28) THEN
        I1 = IND+2
        I3 = IND+7
        X1 = X(MM,NN)*SQRT3
        X3 = X(MM,NN)*SQRT5
C
      ELSE IF(I17.EQ.29) THEN
        I2 = IND+2
        I3 = IND+8
        X2 = X(MM,NN)*SQRT3
        X3 = X(MM,NN)*SQRT5
C
      ELSE IF(I17.EQ.30) THEN
        I1 = IND+5
        I2 = IND+3
        X1 = X(MM,NN)*SQRT5
        X2 = X(MM,NN)*SQRT5
C
      ELSE IF(I17.EQ.31) THEN
        I1 = IND+7
        I3 = IND+4
        X1 = X(MM,NN)*SQRT5
        X3 = X(MM,NN)*SQRT5
C
      ELSE IF(I17.EQ.32) THEN
        I2 = IND+8
        I3 = IND+6
        X2 = X(MM,NN)*SQRT5
        X3 = X(MM,NN)*SQRT5
C
      ELSE IF(I17.EQ.33) THEN
        I1 = IND+9
        I2 = IND+4
        I3 = IND+3
        X1 = X(MM,NN)*SQRT3
        X2 = X(MM,NN)*SQRT5
        X3 = X(MM,NN)*SQRT5
C
      ELSE IF(I17.EQ.34) THEN
        I1 = IND+6
        I2 = IND+9
        I3 = IND+5
        X1 = X(MM,NN)*SQRT5
        X2 = X(MM,NN)*SQRT3
        X3 = X(MM,NN)*SQRT5
C
      ELSE IF(I17.EQ.35) THEN
        I1 = IND+8
        I2 = IND+7
        I3 = IND+9
        X1 = X(MM,NN)*SQRT5
        X2 = X(MM,NN)*SQRT5
        X3 = X(MM,NN)*SQRT3
C
      END IF
C
C     One could envision going from G -> H functions this way, too.
C
C     Begin construction of P(0)*G(1,0) matrix directly from distributed
C     integrals.  This is element [P(0)*G(1,0)]_k17,l17
C
C     jj is an index for the symmetric P matrix in l2 space.
C     jjs in an index for an antisymmetric matrix in l2s space.
C
      JJ   = (J17*J17-J17)/2
      JJS  = ((J17-1)*(J17-1)-(J17-1))/2
      LL   = (L17*L17-L17)/2
      LLS  = ((L17-1)*(L17-1)-(L17-1))/2
C
C     The diagonals of this antisymmetric matrix are zero by definition.
C
      IF(K17.GE.L17) GO TO 500
C
      KLNNS = LLS + K17
C
C     It may be that no integral corresponds to i1 in this pass.
C     Skip it and repeat the process for i2.
C
      IF(I1.EQ.0) GO TO 300
C
C     The matrices xijm, yijm, and zijm are antisymmetric.
C
      IF(I1.EQ.J17) GO TO 300
C
      II1  = (I1*I1-I1)/2
      II1S = ((I1-1)*(I1-1)-(I1-1))/2
C
      IF(I1.LT.J17) THEN
        IJNNX = JJ + I1
        IJNNXS = JJS + I1
        F = ONE
      ELSE
        IJNNX = II1 + J17
        IJNNXS = II1S + J17
        F = -ONE
      END IF
C
      P0G10Y(KLNNS) = P0G10Y(KLNNS) + P(IJNNX)*F*ZIJM(IJNNXS)*X1
      P0G10Z(KLNNS) = P0G10Z(KLNNS) - P(IJNNX)*F*YIJM(IJNNXS)*X1
C
 300  CONTINUE
C
      IF(I2.EQ.0) GO TO 400
C
      IF(I2.EQ.J17) GO TO 400
C
      II2  = (I2*I2-I2)/2
      II2S = ((I2-1)*(I2-1)-(I2-1))/2
C
      IF(I2.LT.J17) THEN
        IJNNY = JJ + I2
        IJNNYS = JJS + I2
        F = ONE
      ELSE
        IJNNY = II2 + J17
        IJNNYS = II2S + J17
        F = -ONE
      END IF
C
      P0G10X(KLNNS) = P0G10X(KLNNS) - P(IJNNY)*F*ZIJM(IJNNYS)*X2
      P0G10Z(KLNNS) = P0G10Z(KLNNS) + P(IJNNY)*F*XIJM(IJNNYS)*X2
C
 400  CONTINUE
C
      IF(I3.EQ.0) GO TO 500
C
      IF(I3.EQ.J17) GO TO 500
C
      II3  = (I3*I3-I3)/2
      II3S = ((I3-1)*(I3-1)-(I3-1))/2
C
      IF(I3.LT.J17) THEN
        IJNNZ = JJ + I3
        IJNNZS = JJS + I3
        F = ONE
      ELSE
        IJNNZ = II3 + J17
        IJNNZS = II3S + J17
        F = -ONE
      END IF
C
      P0G10X(KLNNS) = P0G10X(KLNNS) + P(IJNNZ)*F*YIJM(IJNNZS)*X3
      P0G10Y(KLNNS) = P0G10Y(KLNNS) - P(IJNNZ)*F*XIJM(IJNNZS)*X3
C
 500  CONTINUE
C
C     This is for the -1/2 g_ijkl term in G(1,0).  Here, we are
C     building the matrix element [P(0)*G(1,0)]_k17,j17.  See the
C     above section for more comments.
C
      IF(K17.GE.J17) GO TO 800
C
      KJNNS = JJS + K17
C
      IF(I1.EQ.0) GO TO 600
C
      IF(I1.EQ.J17) GO TO 600
C
      II1  = (I1*I1-I1)/2
      II1S = ((I1-1)*(I1-1)-(I1-1))/2
C
      IF(I1.LT.J17) THEN
        IJNNXS = JJS + I1
        F = ONE
      ELSE
        IJNNXS = II1S + J17
        F = -ONE
      END IF
C
      IF(I1.LE.L17) THEN
        ILNNX = LL + I1
      ELSE
        ILNNX = II1 + L17
      END IF
C
      P0G10Y(KJNNS) = P0G10Y(KJNNS) - HALF*P(ILNNX)*F*ZIJM(IJNNXS)*X1
      P0G10Z(KJNNS) = P0G10Z(KJNNS) + HALF*P(ILNNX)*F*YIJM(IJNNXS)*X1
C
 600  CONTINUE
C
      IF(I2.EQ.0) GO TO 700
C
      IF(I2.EQ.J17) GO TO 700
C
      II2  = (I2*I2-I2)/2
      II2S = ((I2-1)*(I2-1)-(I2-1))/2
C
      IF(I2.LT.J17) THEN
        IJNNYS = JJS + I2
        F = ONE
      ELSE
        IJNNYS = II2S + J17
        F = -ONE
      END IF
C
      IF(I2.LE.L17) THEN
        ILNNY = LL + I2
      ELSE
        ILNNY = II2 + L17
      END IF
C
      P0G10X(KJNNS) = P0G10X(KJNNS) + HALF*P(ILNNY)*F*ZIJM(IJNNYS)*X2
      P0G10Z(KJNNS) = P0G10Z(KJNNS) - HALF*P(ILNNY)*F*XIJM(IJNNYS)*X2
C
 700  CONTINUE
C
      IF(I3.EQ.0) GO TO 800
C
      IF(I3.EQ.J17) GO TO 800
C
      II3  = (I3*I3-I3)/2
      II3S = ((I3-1)*(I3-1)-(I3-1))/2
C
      IF(I3.LT.J17) THEN
        IJNNZS = JJS + I3
        F = ONE
      ELSE
        IJNNZS = II3S + J17
        F = -ONE
      END IF
C
      IF(I3.LE.L17) THEN
        ILNNZ = LL + I3
      ELSE
        ILNNZ = II3 + L17
      END IF
C
      P0G10X(KJNNS) = P0G10X(KJNNS) - HALF*P(ILNNZ)*F*YIJM(IJNNZS)*X3
      P0G10Y(KJNNS) = P0G10Y(KJNNS) + HALF*P(ILNNZ)*F*XIJM(IJNNZS)*X3
C
 800  CONTINUE
C
 361  CONTINUE
 381  CONTINUE
 401  CONTINUE
 421  CONTINUE
C
      RETURN
      END
C*MODULE NMR  *DECK DISTK
      SUBROUTINE DISTK(TEINT4,TEINT5,TEINT6,IND,X,LSHEL,
     *                 MINI,MAXI,MINJ,MAXJ,MINK,MAXK,MINL,MAXL,
     *                 LOCI,LOCJ,LOCK,LOCL)
C
C     This DISTributes the integrals based on the K shell and
C     renormalizes them correctly.  See DISTI for more comments.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL LSHEL,DBG
C
      PARAMETER (MXATM=500)
      PARAMETER (SQRT3=1.73205080756888D+00,SQRT5=2.23606797749979D+00)
C
      DIMENSION TEINT4(NUM,NUM,NUM,NUM)
      DIMENSION TEINT5(NUM,NUM,NUM,NUM)
      DIMENSION TEINT6(NUM,NUM,NUM,NUM)
      DIMENSION X(225,225)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
C
      DBG=.FALSE.
C
      NN = 0
      DO 421 K = MINK,MAXK
         K17 = LOCK + K
         DO 401 L = MINL,MAXL
            L17 = LOCL + L
            NN = NN  + 1
            MM = 0
            DO 381 I = MINI,MAXI
               I17 = LOCI + I
               DO 361 J = MINJ,MAXJ
                  J17 = LOCJ + J
                  MM = MM + 1
C
C     It's not possible to have k17=1.
C
      IF(K17.EQ.2) THEN
        TEINT4(I17,J17,IND  ,L17)=X(MM,NN)
        IF(DBG) THEN
        WRITE(IW,1111) 4,I17,J17,IND,L17,TEINT4(I17,J17,IND  ,L17)
        END IF
C
      ELSE IF(K17.EQ.3) THEN
        TEINT5(I17,J17,IND  ,L17)=X(MM,NN)
        IF(DBG) THEN
        WRITE(IW,1111) 5,I17,J17,IND,L17,TEINT5(I17,J17,IND  ,L17)
        END IF
C
      ELSE IF(K17.EQ.4) THEN
        TEINT6(I17,J17,IND  ,L17)=X(MM,NN)
        IF(DBG) THEN
        WRITE(IW,1111) 6,I17,J17,IND,L17,TEINT6(I17,J17,IND  ,L17)
        END IF
C
      ELSE IF(K17.EQ.5) THEN
        IF(LSHEL) THEN
          TEINT4(I17,J17,IND+1,L17)=X(MM,NN)
        ELSE
          TEINT4(I17,J17,IND  ,L17)=X(MM,NN)
        END IF
        IF(DBG) THEN
        IF(LSHEL) THEN
        WRITE(IW,1111) 4,I17,J17,IND+1,L17,TEINT4(I17,J17,IND+1,L17)
        ELSE
        WRITE(IW,1111) 4,I17,J17,IND  ,L17,TEINT4(I17,J17,IND  ,L17)
        END IF
        END IF
C
      ELSE IF(K17.EQ.6) THEN
        IF(LSHEL) THEN
          TEINT5(I17,J17,IND+2,L17)=X(MM,NN)
        ELSE
          TEINT5(I17,J17,IND+1,L17)=X(MM,NN)
        END IF
        IF(DBG) THEN
        IF(LSHEL) THEN
        WRITE(IW,1111) 5,I17,J17,IND+2,L17,TEINT5(I17,J17,IND+2,L17)
        ELSE
        WRITE(IW,1111) 5,I17,J17,IND+1,L17,TEINT5(I17,J17,IND+1,L17)
        END IF
        END IF
C
      ELSE IF(K17.EQ.7) THEN
        IF(LSHEL) THEN
          TEINT6(I17,J17,IND+3,L17)=X(MM,NN)
        ELSE
          TEINT6(I17,J17,IND+2,L17)=X(MM,NN)
        END IF
        IF(DBG) THEN
        IF(LSHEL) THEN
        WRITE(IW,1111) 6,I17,J17,IND+3,L17,TEINT6(I17,J17,IND+3,L17)
        ELSE
        WRITE(IW,1111) 6,I17,J17,IND+2,L17,TEINT6(I17,J17,IND+2,L17)
        END IF
        END IF
C
      ELSE IF(K17.EQ.8) THEN
        IF(LSHEL) THEN
          TEINT4(I17,J17,IND+2,L17)=X(MM,NN)
          TEINT5(I17,J17,IND+1,L17)=X(MM,NN)
        ELSE
          TEINT4(I17,J17,IND+1,L17)=X(MM,NN)
          TEINT5(I17,J17,IND  ,L17)=X(MM,NN)
        END IF
        IF(DBG) THEN
        IF(LSHEL) THEN
        WRITE(IW,1111) 4,I17,J17,IND+2,L17,TEINT4(I17,J17,IND+2,L17)
        WRITE(IW,1111) 5,I17,J17,IND+1,L17,TEINT5(I17,J17,IND+1,L17)
        ELSE
        WRITE(IW,1111) 4,I17,J17,IND+1,L17,TEINT4(I17,J17,IND+1,L17)
        WRITE(IW,1111) 5,I17,J17,IND  ,L17,TEINT5(I17,J17,IND  ,L17)
        END IF
        END IF
C
      ELSE IF(K17.EQ.9) THEN
        IF(LSHEL) THEN
          TEINT4(I17,J17,IND+3,L17)=X(MM,NN)
          TEINT6(I17,J17,IND+1,L17)=X(MM,NN)
        ELSE
          TEINT4(I17,J17,IND+2,L17)=X(MM,NN)
          TEINT6(I17,J17,IND  ,L17)=X(MM,NN)
        END IF
        IF(DBG) THEN
        IF(LSHEL) THEN
        WRITE(IW,1111) 4,I17,J17,IND+3,L17,TEINT4(I17,J17,IND+3,L17)
        WRITE(IW,1111) 6,I17,J17,IND+1,L17,TEINT6(I17,J17,IND+1,L17)
        ELSE
        WRITE(IW,1111) 4,I17,J17,IND+2,L17,TEINT4(I17,J17,IND+2,L17)
        WRITE(IW,1111) 6,I17,J17,IND  ,L17,TEINT6(I17,J17,IND  ,L17)
        END IF
        END IF
C
      ELSE IF(K17.EQ.10) THEN
        IF(LSHEL) THEN
          TEINT5(I17,J17,IND+3,L17)=X(MM,NN)
          TEINT6(I17,J17,IND+2,L17)=X(MM,NN)
        ELSE
          TEINT5(I17,J17,IND+2,L17)=X(MM,NN)
          TEINT6(I17,J17,IND+1,L17)=X(MM,NN)
        END IF
        IF(DBG) THEN
        IF(LSHEL) THEN
        WRITE(IW,1111) 5,I17,J17,IND+3,L17,TEINT5(I17,J17,IND+3,L17)
        WRITE(IW,1111) 6,I17,J17,IND+2,L17,TEINT6(I17,J17,IND+2,L17)
        ELSE
        WRITE(IW,1111) 5,I17,J17,IND+2,L17,TEINT5(I17,J17,IND+2,L17)
        WRITE(IW,1111) 6,I17,J17,IND+1,L17,TEINT6(I17,J17,IND+1,L17)
        END IF
        END IF
C
      ELSE IF(K17.EQ.11) THEN
        TEINT4(I17,J17,IND  ,L17)=X(MM,NN)
C
      ELSE IF(K17.EQ.12) THEN
        TEINT5(I17,J17,IND+1,L17)=X(MM,NN)
C
      ELSE IF(K17.EQ.13) THEN
        TEINT6(I17,J17,IND+2,L17)=X(MM,NN)
C
      ELSE IF(K17.EQ.14) THEN
        TEINT4(I17,J17,IND+3,L17)=X(MM,NN)*SQRT3
        TEINT5(I17,J17,IND  ,L17)=X(MM,NN)
C
      ELSE IF(K17.EQ.15) THEN
        TEINT4(I17,J17,IND+4,L17)=X(MM,NN)*SQRT3
        TEINT6(I17,J17,IND  ,L17)=X(MM,NN)
C
      ELSE IF(K17.EQ.16) THEN
        TEINT4(I17,J17,IND+1,L17)=X(MM,NN)
        TEINT5(I17,J17,IND+3,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.17) THEN
        TEINT5(I17,J17,IND+5,L17)=X(MM,NN)*SQRT3
        TEINT6(I17,J17,IND+1,L17)=X(MM,NN)
C
      ELSE IF(K17.EQ.18) THEN
        TEINT4(I17,J17,IND+2,L17)=X(MM,NN)
        TEINT6(I17,J17,IND+4,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.19) THEN
        TEINT5(I17,J17,IND+2,L17)=X(MM,NN)
        TEINT6(I17,J17,IND+5,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.20) THEN
        TEINT4(I17,J17,IND+5,L17)=X(MM,NN)*SQRT3
        TEINT5(I17,J17,IND+4,L17)=X(MM,NN)*SQRT3
        TEINT6(I17,J17,IND+3,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.21) THEN
        TEINT4(I17,J17,IND  ,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.22) THEN
        TEINT5(I17,J17,IND+1,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.23) THEN
        TEINT6(I17,J17,IND+2,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.24) THEN
        TEINT4(I17,J17,IND+3,L17)=X(MM,NN)*SQRT5
        TEINT5(I17,J17,IND  ,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.25) THEN
        TEINT4(I17,J17,IND+4,L17)=X(MM,NN)*SQRT5
        TEINT6(I17,J17,IND  ,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.26) THEN
        TEINT4(I17,J17,IND+1,L17)=X(MM,NN)*SQRT3
        TEINT5(I17,J17,IND+5,L17)=X(MM,NN)*SQRT5
C
      ELSE IF(K17.EQ.27) THEN
        TEINT5(I17,J17,IND+6,L17)=X(MM,NN)*SQRT5
        TEINT6(I17,J17,IND+1,L17)=X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.28) THEN
        TEINT4(I17,J17,IND+2,L17)=X(MM,NN)*SQRT3
        TEINT6(I17,J17,IND+7,L17)=X(MM,NN)*SQRT5
C
      ELSE IF(K17.EQ.29) THEN
        TEINT5(I17,J17,IND+2,L17)=X(MM,NN)*SQRT3
        TEINT6(I17,J17,IND+8,L17)=X(MM,NN)*SQRT5
C
      ELSE IF(K17.EQ.30) THEN
        TEINT4(I17,J17,IND+5,L17)=X(MM,NN)*SQRT5
        TEINT5(I17,J17,IND+3,L17)=X(MM,NN)*SQRT5
C
      ELSE IF(K17.EQ.31) THEN
        TEINT4(I17,J17,IND+7,L17)=X(MM,NN)*SQRT5
        TEINT6(I17,J17,IND+4,L17)=X(MM,NN)*SQRT5
C
      ELSE IF(K17.EQ.32) THEN
        TEINT5(I17,J17,IND+8,L17)=X(MM,NN)*SQRT5
        TEINT6(I17,J17,IND+6,L17)=X(MM,NN)*SQRT5
C
      ELSE IF(K17.EQ.33) THEN
        TEINT4(I17,J17,IND+9,L17)=X(MM,NN)*SQRT3
        TEINT5(I17,J17,IND+4,L17)=X(MM,NN)*SQRT5
        TEINT6(I17,J17,IND+3,L17)=X(MM,NN)*SQRT5
C
      ELSE IF(K17.EQ.34) THEN
        TEINT4(I17,J17,IND+6,L17)=X(MM,NN)*SQRT5
        TEINT5(I17,J17,IND+9,L17)=X(MM,NN)*SQRT3
        TEINT6(I17,J17,IND+5,L17)=X(MM,NN)*SQRT5
C
      ELSE IF(K17.EQ.35) THEN
        TEINT4(I17,J17,IND+8,L17)=X(MM,NN)*SQRT5
        TEINT5(I17,J17,IND+7,L17)=X(MM,NN)*SQRT5
        TEINT6(I17,J17,IND+9,L17)=X(MM,NN)*SQRT3
C
      END IF
C
 361  CONTINUE
 381  CONTINUE
 401  CONTINUE
 421  CONTINUE
C
      RETURN
 1111 FORMAT(4X,I3,'(',I3,I3,'|',I3,I3,')',F25.15)
      END
C*MODULE NMR  *DECK DISTKG10
      SUBROUTINE DISTKG10(P0G10X,P0G10Y,P0G10Z,P,XIJM,YIJM,ZIJM,
     *                    IND,X,LSHEL,L2,L2S,MINI,MAXI,MINJ,MAXJ,
     *                    MINK,MAXK,MINL,MAXL,LOCI,LOCJ,LOCK,LOCL)
C
C     This DISTributes the integrals based on the K shell and
C     renormalizes them correctly.
C     See DISTIG10 for more comments on the details.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL LSHEL
C
      PARAMETER (SQRT3=1.73205080756888D+00,
     *           SQRT5=2.23606797749979D+00,
     *           ZERO=0.0D+00, HALF=0.5D+00, ONE=1.0D+00)
C
      DIMENSION X(225,225),P0G10X(L2S),P0G10Y(L2S),P0G10Z(L2S),
     *          P(L2),XIJM(L2S),YIJM(L2S),ZIJM(L2S)
C
      NN = 0
      DO 421 K = MINK,MAXK
         K17 = LOCK + K
         DO 401 L = MINL,MAXL
            L17 = LOCL + L
            NN = NN  + 1
            MM = 0
            DO 381 I = MINI,MAXI
               I17 = LOCI + I
               DO 361 J = MINJ,MAXJ
                  J17 = LOCJ + J
                  MM = MM + 1
C
      K4 = 0
      K5 = 0
      K6 = 0
      X4 = ZERO
      X5 = ZERO
      X6 = ZERO
C
C     It's not possible to have k17=1.  (See disti for more comments.)
C
      IF(K17.EQ.2) THEN
        K4 = IND
        X4 = X(MM,NN)
C
      ELSE IF(K17.EQ.3) THEN
        K5 = IND
        X5 = X(MM,NN)
C
      ELSE IF(K17.EQ.4) THEN
        K6 = IND
        X6 = X(MM,NN)
C
      ELSE IF(K17.EQ.5) THEN
        IF(LSHEL) THEN
          K4 = IND+1
          X4 = X(MM,NN)
        ELSE
          K4 = IND
          X4 = X(MM,NN)
        END IF
C
      ELSE IF(K17.EQ.6) THEN
        IF(LSHEL) THEN
          K5 = IND+2
          X5 = X(MM,NN)
        ELSE
          K5 = IND+1
          X5 = X(MM,NN)
        END IF
C
      ELSE IF(K17.EQ.7) THEN
        IF(LSHEL) THEN
          K6 = IND+3
          X6 = X(MM,NN)
        ELSE
          K6 = IND+2
          X6 = X(MM,NN)
        END IF
C
      ELSE IF(K17.EQ.8) THEN
        IF(LSHEL) THEN
          K4 = IND+2
          K5 = IND+1
          X4 = X(MM,NN)
          X5 = X(MM,NN)
        ELSE
          K4 = IND+1
          K5 = IND
          X4 = X(MM,NN)
          X5 = X(MM,NN)
        END IF
C
      ELSE IF(K17.EQ.9) THEN
        IF(LSHEL) THEN
          K4 = IND+3
          K6 = IND+1
          X4 = X(MM,NN)
          X6 = X(MM,NN)
        ELSE
          K4 = IND+2
          K6 = IND
          X4 = X(MM,NN)
          X6 = X(MM,NN)
        END IF
C
      ELSE IF(K17.EQ.10) THEN
        IF(LSHEL) THEN
          K5 = IND+3
          K6 = IND+2
          X5 = X(MM,NN)
          X6 = X(MM,NN)
        ELSE
          K5 = IND+2
          K6 = IND+1
          X5 = X(MM,NN)
          X6 = X(MM,NN)
        END IF
C
      ELSE IF(K17.EQ.11) THEN
        K4 = IND
        X4 = X(MM,NN)
C
      ELSE IF(K17.EQ.12) THEN
        K5 = IND+1
        X5 = X(MM,NN)
C
      ELSE IF(K17.EQ.13) THEN
        K6 = IND+2
        X6 = X(MM,NN)
C
      ELSE IF(K17.EQ.14) THEN
        K4 = IND+3
        K5 = IND
        X4 = X(MM,NN)*SQRT3
        X5 = X(MM,NN)
C
      ELSE IF(K17.EQ.15) THEN
        K4 = IND+4
        K6 = IND
        X4 = X(MM,NN)*SQRT3
        X6 = X(MM,NN)
C
      ELSE IF(K17.EQ.16) THEN
        K4 = IND+1
        K5 = IND+3
        X4 = X(MM,NN)
        X5 = X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.17) THEN
        K5 = IND+5
        K6 = IND+1
        X5 = X(MM,NN)*SQRT3
        X6 = X(MM,NN)
C
      ELSE IF(K17.EQ.18) THEN
        K4 = IND+2
        K6 = IND+4
        X4 = X(MM,NN)
        X6 = X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.19) THEN
        K5 = IND+2
        K6 = IND+5
        X5 = X(MM,NN)
        X6 = X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.20) THEN
        K4 = IND+5
        K5 = IND+4
        K6 = IND+3
        X4 = X(MM,NN)*SQRT3
        X5 = X(MM,NN)*SQRT3
        X6 = X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.21) THEN
        K4 = IND
        X4 = X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.22) THEN
        K5 = IND+1
        X5 = X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.23) THEN
        K6 = IND+2
        X6 = X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.24) THEN
        K4 = IND+3
        K5 = IND
        X4 = X(MM,NN)*SQRT5
        X5 = X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.25) THEN
        K4 = IND+4
        K6 = IND
        X4 = X(MM,NN)*SQRT5
        X6 = X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.26) THEN
        K4 = IND+1
        K5 = IND+5
        X4 = X(MM,NN)*SQRT3
        X5 = X(MM,NN)*SQRT5
C
      ELSE IF(K17.EQ.27) THEN
        K5 = IND+6
        K6 = IND+1
        X5 = X(MM,NN)*SQRT5
        X6 = X(MM,NN)*SQRT3
C
      ELSE IF(K17.EQ.28) THEN
        K4 = IND+2
        K6 = IND+7
        X4 = X(MM,NN)*SQRT3
        X6 = X(MM,NN)*SQRT5
C
      ELSE IF(K17.EQ.29) THEN
        K5 = IND+2
        K6 = IND+8
        X5 = X(MM,NN)*SQRT3
        X6 = X(MM,NN)*SQRT5
C
      ELSE IF(K17.EQ.30) THEN
        K4 = IND+5
        K5 = IND+3
        X4 = X(MM,NN)*SQRT5
        X5 = X(MM,NN)*SQRT5
C
      ELSE IF(K17.EQ.31) THEN
        K4 = IND+7
        K6 = IND+4
        X4 = X(MM,NN)*SQRT5
        X6 = X(MM,NN)*SQRT5
C
      ELSE IF(K17.EQ.32) THEN
        K5 = IND+8
        K6 = IND+6
        X5 = X(MM,NN)*SQRT5
        X6 = X(MM,NN)*SQRT5
C
      ELSE IF(K17.EQ.33) THEN
        K4 = IND+9
        K5 = IND+4
        K6 = IND+3
        X4 = X(MM,NN)*SQRT3
        X5 = X(MM,NN)*SQRT5
        X6 = X(MM,NN)*SQRT5
C
      ELSE IF(K17.EQ.34) THEN
        K4 = IND+6
        K5 = IND+9
        K6 = IND+5
        X4 = X(MM,NN)*SQRT5
        X5 = X(MM,NN)*SQRT3
        X6 = X(MM,NN)*SQRT5
C
      ELSE IF(K17.EQ.35) THEN
        K4 = IND+8
        K5 = IND+7
        K6 = IND+9
        X4 = X(MM,NN)*SQRT5
        X5 = X(MM,NN)*SQRT5
        X6 = X(MM,NN)*SQRT3
C
      END IF
C
C     Begin construction of P(0)*G(1,0) matrix directly from distributed
C     integrals.  This is element [P(0)*G(1,0)]_k17,l17
C
C     jj is an index for the symmetric P matrix in l2 space.
C     jjs in an index for an antisymmetric matrix in l2s space.
C
      II   = (I17*I17-I17)/2
      JJ   = (J17*J17-J17)/2
      JJS  = ((J17-1)*(J17-1)-(J17-1))/2
      LL   = (L17*L17-L17)/2
      LLS  = ((L17-1)*(L17-1)-(L17-1))/2
C
C     ijnns is the label for the i,j matrix element in an l2s array.
C     ijnn is the label for the i,j matrix element in an l2 array.
C
      IF(I17.LE.J17) THEN
        IJNN = JJ + I17
      ELSE
        IJNN = II + J17
      END IF
C
C     It may be that no integral corresponds to k4 in this pass.
C     Skip it and repeat the process for k5.
C
      IF(K4.EQ.0) GO TO 300
C
C     The matrices xijm, yijm, and zijm are antisymmetric, and we
C     need the k4,l17 element.
C
      IF(K4.GE.L17) GO TO 300
C
      KK4S = ((K4-1)*(K4-1)-(K4-1))/2
C
      IF(K4.LT.L17) THEN
        KLNNXS = LLS + K4
        F = ONE
      ELSE IF(K4.GT.L17) THEN
        KLNNXS = KK4S + L17
        F = -ONE
      END IF
C
      P0G10Y(KLNNXS) = P0G10Y(KLNNXS) + P(IJNN)*F*ZIJM(KLNNXS)*X4
      P0G10Z(KLNNXS) = P0G10Z(KLNNXS) - P(IJNN)*F*YIJM(KLNNXS)*X4
C
 300  CONTINUE
C
      IF(K5.EQ.0) GO TO 400
C
      IF(K5.GE.L17) GO TO 400
C
      KK5S = ((K5-1)*(K5-1)-(K5-1))/2
C
      IF(K5.LT.L17) THEN
        KLNNYS = LLS + K5
        F = ONE
      ELSE IF(K5.GT.L17) THEN
        KLNNYS = KK5S + L17
        F = -ONE
      END IF
C
      P0G10X(KLNNYS) = P0G10X(KLNNYS) - P(IJNN)*F*ZIJM(KLNNYS)*X5
      P0G10Z(KLNNYS) = P0G10Z(KLNNYS) + P(IJNN)*F*XIJM(KLNNYS)*X5
C
 400  CONTINUE
C
      IF(K6.EQ.0) GO TO 500
C
      IF(K6.GE.L17) GO TO 500
C
      KK6S = ((K6-1)*(K6-1)-(K6-1))/2
C
      IF(K6.LT.L17) THEN
        KLNNZS = LLS + K6
        F = ONE
      ELSE IF(K6.GT.L17) THEN
        KLNNZS = KK6S + L17
        F = -ONE
      END IF
C
      P0G10X(KLNNZS) = P0G10X(KLNNZS) + P(IJNN)*F*YIJM(KLNNZS)*X6
      P0G10Y(KLNNZS) = P0G10Y(KLNNZS) - P(IJNN)*F*XIJM(KLNNZS)*X6
C
 500  CONTINUE
C
C     This is for the -1/2 g_ijkl term in G(1,0).  Here, we are
C     building the matrix element [P(0)*G(1,0)]_k17,j17.  See the
C     above section for more comments.
C
      IF(I17.LE.L17) THEN
        ILNN = LL + I17
      ELSE
        ILNN = II + L17
      END IF
C
      IF(K4.EQ.0) GO TO 600
C
      IF(K4.EQ.L17) GO TO 600
C
      IF(K4.GE.J17) GO TO 600
C
      KK4S = ((K4-1)*(K4-1)-(K4-1))/2
C
      IF(K4.LT.L17) THEN
        KLNNXS = LLS + K4
        F = ONE
      ELSE
        KLNNXS = KK4S + L17
        F = -ONE
      END IF
C
      IF(K4.LT.J17) THEN
        KJNNXS = JJS + K4
      ELSE
        KJNNXS = KK4S + J17
      END IF
C
      P0G10Y(KJNNXS) = P0G10Y(KJNNXS) - HALF*P(ILNN)*F*ZIJM(KLNNXS)*X4
      P0G10Z(KJNNXS) = P0G10Z(KJNNXS) + HALF*P(ILNN)*F*YIJM(KLNNXS)*X4
C
 600  CONTINUE
C
      IF(K5.EQ.0) GO TO 700
C
      IF(K5.EQ.L17) GO TO 700
C
      IF(K5.GE.J17) GO TO 700
C
      KK5S = ((K5-1)*(K5-1)-(K5-1))/2
C
      IF(K5.LT.L17) THEN
        KLNNYS = LLS + K5
        F = ONE
      ELSE
        KLNNYS = KK5S + L17
        F = -ONE
      END IF
C
      IF(K5.LT.J17) THEN
        KJNNYS = JJS + K5
      ELSE
        KJNNYS = KK5S + J17
      END IF
C
      P0G10X(KJNNYS) = P0G10X(KJNNYS) + HALF*P(ILNN)*F*ZIJM(KLNNYS)*X5
      P0G10Z(KJNNYS) = P0G10Z(KJNNYS) - HALF*P(ILNN)*F*XIJM(KLNNYS)*X5
C
 700  CONTINUE
C
      IF(K6.EQ.0) GO TO 800
C
      IF(K6.EQ.L17) GO TO 800
C
      IF(K6.GE.J17) GO TO 800
C
      KK6S = ((K6-1)*(K6-1)-(K6-1))/2
C
      IF(K6.LT.L17) THEN
        KLNNZS = LLS + K6
        F = ONE
      ELSE
        KLNNZS = KK6S + L17
        F = -ONE
      END IF
C
      IF(K6.LT.J17) THEN
        KJNNZS = JJS + K6
      ELSE
        KJNNZS = KK6S + J17
      END IF
C
      P0G10X(KJNNZS) = P0G10X(KJNNZS) - HALF*P(ILNN)*F*YIJM(KLNNZS)*X6
      P0G10Y(KJNNZS) = P0G10Y(KJNNZS) + HALF*P(ILNN)*F*XIJM(KLNNZS)*X6
C
 800  CONTINUE
C
 361  CONTINUE
 381  CONTINUE
 401  CONTINUE
 421  CONTINUE
C
      RETURN
      END
C*MODULE NMR  *DECK NMRQOUT
      SUBROUTINE NMRQOUT(I17,J17,K17,L17,BUFP,IX,NINTMX,VAL,NFTNMR)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL OUT
C
      DIMENSION BUFP(NINTMX),IX(NINTMX)
C
      COMMON /PCKLAB/ LABSIZ
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
      NPACK = ICOUNT
      IPACK = I17
      JPACK = J17
      KPACK = K17
      LPACK = L17
      IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
         LABEL1 = ISHFT( IPACK, 16 ) + JPACK
         LABEL2 = ISHFT( KPACK, 16 ) + LPACK
         IX( 2*NPACK-1 ) = LABEL1
         IX( 2*NPACK   ) = LABEL2
#endif
#if defined(SPEC_CPU_ILP64)
         LABEL = ISHFT( IPACK, 48 ) + ISHFT( JPACK, 32 ) +
     *           ISHFT( KPACK, 16 ) + LPACK
         IX(NPACK) = LABEL
#endif
      ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
         LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *           ISHFT( KPACK,  8 ) + LPACK
         IX(NPACK) = LABEL
#endif
#if defined(SPEC_CPU_ILP64)
         IF ( MOD(NPACK,2) .EQ. 0 ) THEN
            LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *              ISHFT( KPACK,  8 ) + LPACK
            IX( NPACK/2 ) = IX( NPACK/2 ) + LABEL
         ELSE
            LABEL = ISHFT( IPACK, 56 ) + ISHFT( JPACK, 48 ) +
     *              ISHFT( KPACK, 40 ) + ISHFT( LPACK, 32 )
            IX( (NPACK/2)+1 ) = LABEL
         END IF
#endif
      END IF
C
      BUFP(ICOUNT) = VAL
      ICOUNT = ICOUNT+1
      IF (ICOUNT .GT. NINTMX) THEN
         NXX = NINTMX
         CALL PWRIT(NFTNMR,BUFP,IX,NXX,NINTMX)
         ICOUNT = 1
      END IF
      RETURN
      END
C*MODULE NMR  *DECK RDNMR1
      SUBROUTINE RDNMR1(NFT,X2,XX,IX,NINTMX)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500)
C
      DIMENSION X2(NUM,NUM,NUM,NUM),XX(NINTMX),IX(NINTMX)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,MA,MB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCKLAB/ LABSIZ
C
      NX = 0
      CALL SEQREW(NFT)
C
      CALL VCLR(X2,1,NUM*NUM*NUM*NUM)
  200 CONTINUE
      CALL PREAD(NFT,XX,IX,NX,NINTMX)
      IF (NX.EQ.0) GO TO 240
      MX = ABS(NX)
      IF (MX.GT.NINTMX) THEN
         IF(MASWRK) WRITE(IW,*) 'CONFUSION WITH INTEGRALS IN -RDNMR1-'
         CALL ABRT
         STOP
      END IF
      DO 220 M = 1,MX
         VAL = XX(M)
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
            IPACK =       ISHFT( LABEL, -48 )
            JPACK = IAND( ISHFT( LABEL, -32 ), 65535 )
            KPACK = IAND( ISHFT( LABEL, -16 ), 65535 )
            LPACK = IAND(        LABEL,        65535 )
#endif
         ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
            LABEL = IX(NPACK)
            IPACK =       ISHFT( LABEL, -24 )
            JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
            KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
            LPACK = IAND(        LABEL,        255 )
#endif
#if defined(SPEC_CPU_ILP64)
            IF ( MOD(NPACK,2) .EQ. 0 ) THEN
               LABEL = IX( NPACK/2 )
               IPACK = IAND( ISHFT( LABEL, -24 ), 255 )
               JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
               KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
               LPACK = IAND(        LABEL,        255 )
            ELSE
               LABEL = IX( (NPACK/2)+1 )
               IPACK =       ISHFT( LABEL, -56 )
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
         IF(I.LE.0) GO TO 220
         IF(J.LE.0) GO TO 220
         IF(K.LE.0) GO TO 220
         IF(L.LE.0) GO TO 220
C
         X2(I,J,K,L) = VAL
  220 CONTINUE
      IF (NX .GT. 0) GO TO 200
C
  240 CONTINUE
      CALL SEQREW(NFT)
      RETURN
      END
