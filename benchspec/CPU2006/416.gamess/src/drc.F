C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C  4 NOV 03 - TT  - NMDANA: PERFORM NORMAL MODE ANALYSIS AT C.O.M
C 15 MAY 03 - MWS - DRCDRV: PRINT ISOTOPE MASSES USED
C  7 AUG 02 - MWS - DRCDRV: FIX VIBENG INPUT
C 16 FEB 02 - TT  - DRCDRV: IMPROVE THE QUADR FORMULA
C 24 JAN 02 - MWS - DRCDRV: ENSURE PARALLEL RUNS KEEP IDENTICAL COORDS
C 16 NOV 01 - TT  - CHANGE N.M. ANALYSIS OPTIONS, AND DRC INITIATION
C  6 SEP 01 - MWS - ADD DUMMY ARGUMENTS TO NAMEIO CALL
C 25 JUN 01 - MWS - DRCDRV: ALTER COMMON SCFOPT, PRINT ZNUC FOR ECP RUNS
C 26 OCT 00 - TT  - ADD FRAGAT TERMINATION OPTION, PRINT CHANGES
C  4 NOV 99 - MWS - DRCDRV: MOVE ETOT SO MP2 DRC WILL HAVE CORRECT EPOT
C 17 OCT 96 - SPW - DRCDRV: ADD CALL TO CIGRAD FOR CI RUNS
C 13 JUN 96 - VAG - ADD VARIABLE FOR CI TYPE TO SCFOPT COMMON
C  6 OCT 95 - GMC - IMPLEMENT VIBLVL INITIAL ENERGY DISTRIBUTION
C 14 SEP 95 - MWS - DRCDRV: FINER CONTROL OF OUTPUT VOLUME
C 26 JUL 95 - TT  - DRCDRV: ABORT JOB IF ENERGY CONSERVATION LOST
C 31 MAY 95 - MWS - DRCDRV: INITIALIZE LOOP INDEX
C 28 NOV 94 - TT  - ADD HESSTS INPUT OPTION
C 17 NOV 94 - TT  - DRC ANALYSIS WITH NORMAL MODE MAPPING
C
C*MODULE DRC     *DECK DRCX
      SUBROUTINE DRCX
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500)
C
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      NAT1 =   NAT
      NAT3 = 3*NAT
C
      CALL VALFM(LOADFM)
      LVELO0 = LOADFM + 1
      LC0    = LVELO0 + NAT3
      LC1    = LC0    + NAT3
      LL     = LC1    + NAT3
      LL2    = LL     + NAT3*NAT3
      LPED   = LL2    + NAT3*NAT3
      LFREQ  = LPED   + NAT3
      LVELO2 = LFREQ  + NAT3
      LVELO3 = LVELO2 + NAT3
      LACCEL = LVELO3 + NAT3
      LACCO1 = LACCEL + NAT3
      LACCO2 = LACCO1 + NAT3
      LP     = LACCO2 + NAT3
      LQ     = LP     + NAT3
      LFCM   = LQ     + NAT3
      LSCR   = LFCM   + NAT3*NAT3
      LVIBENG= LSCR   + NAT3*8
      LAST   = LVIBENG+ NAT3
      NEED   = LAST - LOADFM -1
      CALL GETFM(NEED)
C
      CALL DRCDRV(X(LVELO0),X(LC0),X(LC1),X(LL),X(LL2),X(LPED),X(LFREQ),
     *            X(LVELO2),X(LVELO3),X(LACCEL),X(LACCO1),X(LACCO2),
     *            X(LP),X(LQ),X(LFCM),X(LSCR),X(LVIBENG),NAT1,NAT3)
C
      CALL RETFM(NEED)
      IF(MASWRK) WRITE(IW,*) '.... DONE WITH DRC SEGMENT ....'
      CALL TIMIT(1)
      RETURN
      END
C*MODULE DRC     *DECK DRCDRV
      SUBROUTINE DRCDRV(VELO0,C0,C1,L,L2,PED,FREQ,VELO2,VELO3,ACCEL,
     *              ACCOLD,ACCOL2,P,Q,FCM,SCR,VIBENG,NAT1,NAT3)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, MXRT=100, MXAO=2047)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,NMANAL,GOTEH,NVEL,VIBLVL
C
      DOUBLE PRECISION L(NAT3,NAT3), L2(NAT3,NAT3)
      INTEGER FRAGAT(200),FRGINI(100),FGPAIR
      DIMENSION VELO0(NAT3),C0(3,NAT1),C1(3,NAT1),PED(NAT3),FREQ(NAT3),
     *          VELO2(NAT3),VELO3(NAT3),ACCEL(NAT3),ACCOLD(NAT3),
     *          ACCOL2(NAT3),P(NAT3),Q(NAT3),FCM(NAT3,NAT3),
     *          SCR(NAT3,8),VIBENG(NAT3),FRGCUT(100)
C
      COMMON /ECP2  / CLP(400),ZLP(400),NLP(400),KFIRST(MXATM,6),
     *                KLAST(MXATM,6),LMAX(MXATM),LPSKIP(MXATM),
     *                IZCORE(MXATM)
      COMMON /ENRGYS/ ENUCR,EELCT,EPOT,SZ,SZZ,ECORE,ESCF,EERD,E1,E2,
     *                VEN,VEE,EPOT00,EKIN00,ESTATE(MXRT),STATN
      COMMON /FUNCT / E,EG(3*MXATM)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MASSES/ ZMASS(MXATM)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNLAB/ TITLE(10),ANAM(MXATM),BNAM(MXATM),BFLAB(MXAO)
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /SCFOPT/ CONVHF,MAXIT,MCONV,NPUNCH
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      PARAMETER (TOANGS=0.52917724924D+00, TOMETR=TOANGS*1.0D-10,
     *           TOKCAL=627.5131018D+00, TOJOUL=4.35975D-18,
     *           AVOGAD=6.022045D+26, PLANCK=6.626176D-34,
     *           CLIGHT=2.99792458D+10, FEMTO=1.0D-15)
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, TWO=2.0D+00, HALF=0.5D+00)
C
      PARAMETER (NNAM=21)
      DIMENSION QNAM(NNAM),KQNAM(NNAM)
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QNAM/8HNSTEP   ,8HDELTAT  ,8HTOTIME  ,8HNPRTSM  ,8HNMANAL  ,
     *          8HC0      ,8HNNM     ,8HENM     ,8HNVEL    ,
     *          8HEKIN    ,8HVEL     ,8HNPRT    ,8HNPUN    ,
     *          8HVIBLVL  ,8HVIBENG  ,8HIFRGPR  ,8HFRGCUT  ,8HNFRGPR  ,
     *          8HRCENG   ,8HHESS    ,8HHESS2   /
#else
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"NSTEP   ","DELTAT  ","TOTIME  ","NPRTSM  ",
     * "NMANAL  ",  "C0      ","NNM     ","ENM     ", "NVEL    ",
     *          "EKIN    ","VEL     ","NPRT    ","NPUN    ",
     *          "VIBLVL  ","VIBENG  ","IFRGPR  ","FRGCUT  ","NFRGPR  ",
     *          "RCENG   ","HESS    ","HESS2   "/
#endif
      DATA KQNAM/1,3,3,1,0,  -3,1,3,0,  3,-3,1,1,  0,-3,-1,-1,1,  3,5,5/
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DRCWRD/8HDRC     /
#else
      CHARACTER*8 :: DRCWRD_STR
      EQUIVALENCE (DRCWRD, DRCWRD_STR)
      DATA DRCWRD_STR/"DRC     "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA GUGA/8HGUGA    /
#else
      CHARACTER*8 :: GUGA_STR
      EQUIVALENCE (GUGA, GUGA_STR)
      DATA GUGA_STR/"GUGA    "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RMIN,TS/8HMIN     ,8HTS      /
#else
      CHARACTER*8 :: RMIN_STR, TS_STR
      EQUIVALENCE (RMIN, RMIN_STR), (TS, TS_STR)
      DATA RMIN_STR,TS_STR/"MIN     ","TS      "/
#endif
C
C         ---- DRIVER FOR DYNAMICAL REACTION COORDINATE ----
C                PROGRAM WRITTEN BY TETSUYA TAKETSUGU
C
      NAT36 = NAT3 - 6
      IPIRC = 4
      CALL SEQOPN(IPIRC,'IRCDATA','NEW',.FALSE.,'FORMATTED')
C
C         PROVIDE DEFAULTS, AND READ $DRC GROUP
C
      NSTEP  = 1000
      DELTAT = 1.0D-01
      TOTIME = ZERO
      NPRTSM = 1
      NPRT   = 0
      NPUN   = 0
      NMANAL = .FALSE.
      NNM    = 0
      ENM    = ZERO
      EKIN   = ZERO
      NVEL   = .FALSE.
      VIBLVL = .FALSE.
      FGPAIR = 0
      RCENG  = ZERO
      DO 5 I=1,NAT36
         VIBENG(I)=HALF
    5 CONTINUE
      DO 8 I=1,100
         FRGINI(I) = 0
         FRGCUT(I) = ZERO
         FRAGAT(I*2-1)=0
         FRAGAT(I*2)=0
    8 CONTINUE
      HESS = RMIN
      HESS2= RMIN
C
      KQNAM( 6) = 10*NAT3 + 3
      KQNAM(11) = 10*NAT3 + 3
      KQNAM(15) = 10*NAT3 + 3
      KQNAM(16) = 10*200  + 1
      KQNAM(17) = 10*100  + 1
      CALL NAMEIO(IR,JRET,DRCWRD,NNAM,QNAM,KQNAM,
     *            NSTEP,DELTAT,TOTIME,NPRTSM,NMANAL,
     *            C0,NNM,ENM,NVEL,EKIN,VELO0,NPRT,NPUN,
     *            VIBLVL,VIBENG,FRAGAT,FRGCUT,FGPAIR,RCENG,
     *            HESS,HESS2,   0,0,0,
     *    0,0,0,0,0,    0,0,0,0,0,    0,0,0,0,0,    0,0,0,0,0,
     *    0,0,0,0,0,    0,0,0,0,0,    0,0,0,0,0,    0,0,0,0,0)
      IF(JRET.EQ.2) THEN
         IF(MASWRK) WRITE(IW,*) 'ILLEGAL $DRC GROUP READ'
         CALL ABRT
      END IF
C
      IF(NPRT.LT.-1) NPRT=-1
      IF(NPRT.GT. 1) NPRT= 1
      IF(NPUN.LT.-1) NPUN=-1
      IF(NPUN.GT. 2) NPUN= 2
C
C     SET NHESTS, WHICH MEANS:    INITIAL   REFERENCE  STRUCTURE(S)
C                                  $HESS      $HESS2
C         NHESTS =  0 (DEFAULT)   MINIMUM    MINIMUM
C                = -1             MINIMUM      TS
C                =  1               TS       MINIMUM
C                =  2               TS         TS
C
                                             NHESTS=  0
      IF(HESS.EQ.RMIN  .AND.  HESS2.EQ.TS)   NHESTS= -1
      IF(HESS.EQ.TS    .AND.  HESS2.EQ.RMIN) NHESTS=  1
      IF(HESS.EQ.TS    .AND.  HESS2.EQ.TS)   NHESTS=  2
C
C        CONVERT UNITS
C
      IF(NMANAL) THEN
         CALL DSCAL(NAT3,ONE/TOANGS,C0,1)
         SUMW=ZERO
         SUMX=ZERO
         SUMY=ZERO
         SUMZ=ZERO
         DO 10 I = 1, NAT
            AMS = ZMASS(I)
            SUMW= SUMW+ AMS
            SUMX= SUMX+ C0(1,I)*AMS
            SUMY= SUMY+ C0(2,I)*AMS
            SUMZ= SUMZ+ C0(3,I)*AMS
   10    CONTINUE
         DO 20 I = 1, NAT
            C0(1,I) = C0(1,I) - SUMX/SUMW
            C0(2,I) = C0(2,I) - SUMY/SUMW
            C0(3,I) = C0(3,I) - SUMZ/SUMW
   20    CONTINUE
         CALL FCMIN2(FCM,NAT3,GOTEH)
         IF(GOTEH) THEN
            IOPT2=1
            IF(NHESTS.EQ.-1.OR.NHESTS.EQ.2) IOPT2=-1
            CALL GENNM(FCM,FCM,L2,FREQ,SCR,NAT,NAT3,0,IOPT2)
         ELSE
            IF(MASWRK) WRITE(IW,*) 'THIS DRC RUN REQUIRES A',
     *      ' $HESS2 GROUP'
            CALL ABRT
            STOP
         END IF
      END IF
      EKIN = EKIN/TOKCAL
C
      IF(NNM.NE.0  .OR.  VIBLVL) THEN
         CALL FCMIN(FCM,NAT3,GOTEH)
         IF(GOTEH) THEN
            IOPT2=1
            IF(NHESTS.EQ.1.OR.NHESTS.EQ.2) IOPT2=-1
            CALL GENNM(FCM,FCM,L,FREQ,SCR,NAT,NAT3,0,IOPT2)
         ELSE
            IF(MASWRK) WRITE(IW,*) 'THIS DRC RUN REQUIRES A $HESS GROUP'
            CALL ABRT
            STOP
         END IF
      END IF
      IF(FGPAIR.NE.0) THEN
         DO 28 I = 1, FGPAIR
            I1 = FRAGAT(I*2-1)
            I2 = FRAGAT(I*2)
            DIS=0.D0
            DO 25 K = 1, 3
               DIS = DIS + (C(K,I1)-C(K,I2))**2
   25       CONTINUE
            DIS = SQRT(DIS)
            IF(FRGCUT(I).GT.0.D0) THEN
               IF(DIS.GT.FRGCUT(I)) FRGINI(I)=1
            ELSEIF(FRGCUT(I).LT.0.D0) THEN
               IF(DIS.LT.-FRGCUT(I)) FRGINI(I)=1
            ELSE
               IF(MASWRK) WRITE(IW,*) 'FRGCUT MUST NOT BE ZERO'
               CALL ABRT
               STOP
            END IF
   28    CONTINUE
      END IF
C
      FAC = TOJOUL * AVOGAD * FEMTO**2 / TOMETR**2
      IF(NVEL) THEN
         JJ = 0
         DO 140 I = 1, NAT
            AMS = ZMASS(I)
            DO 140 J = 1,3
               JJ = JJ + 1
               EKIN = EKIN + VELO0(JJ)**2 * AMS
  140    CONTINUE
         EKIN = 0.5D+00 * EKIN / FAC
      ELSE IF(.NOT.VIBLVL .AND. NNM.EQ.0) THEN
         SUMW=ZERO
         SUMX=ZERO
         SUMY=ZERO
         SUMZ=ZERO
         DO 150 I = 1, NAT
            AMS = ZMASS(I)
            SUMW= SUMW+ AMS
            SUMX= SUMX+ VELO0(I*3-2)    * AMS
            SUMY= SUMY+ VELO0(I*3-1)    * AMS
            SUMZ= SUMZ+ VELO0(I*3  )    * AMS
  150    CONTINUE
         DO 160 I = 1, NAT
            VELO0(I*3-2) = VELO0(I*3-2) - SUMX/SUMW
            VELO0(I*3-1) = VELO0(I*3-1) - SUMY/SUMW
            VELO0(I*3  ) = VELO0(I*3  ) - SUMZ/SUMW
  160    CONTINUE
         SUM =ZERO
         DO 170 I = 1, NAT
            AMS = ZMASS(I)
            SUM = SUM + VELO0(I*3-2)**2 * AMS
            SUM = SUM + VELO0(I*3-1)**2 * AMS
            SUM = SUM + VELO0(I*3  )**2 * AMS
  170    CONTINUE
         DO 180 I = 1, NAT3
            VELO0(I) = VELO0(I) / SQRT(SUM)
  180    CONTINUE
      ELSE IF(ENM.NE.ZERO) THEN
         EKIN = FREQ(IABS(NNM))*CLIGHT*PLANCK /TOJOUL*ENM
      ELSE IF(VIBLVL) THEN
         IF(NHESTS.LE.0) THEN
            VIBENG(1)=FREQ(1)*CLIGHT*PLANCK*VIBENG(1)/TOJOUL
         ELSE
            VIBENG(1)=RCENG/TOKCAL
         ENDIF
         EKIN=ABS(VIBENG(1))
         DO 182 I=2,NAT36
            VIBENG(I)=FREQ(I)*CLIGHT*PLANCK*VIBENG(I)/TOJOUL
            EKIN=EKIN+ABS(VIBENG(I))
  182    CONTINUE
      ELSE
         ENM = EKIN*TOJOUL / PLANCK / CLIGHT / FREQ(IABS(NNM))
      END IF
C
      IF (MASWRK) THEN
         WRITE(IW,9000) NSTEP,DELTAT,TOTIME,NPRT,NPUN,NPRTSM,
     *                  NMANAL,NHESTS,VIBLVL,FGPAIR
         IF(FGPAIR.NE.0) THEN
            DO 183 I=1,FGPAIR
               WRITE(IW,9005) FRAGAT(I*2-1),FRAGAT(I*2),FRGCUT(I)
  183       CONTINUE
         ENDIF
         IF(NNM.NE.0) THEN
            WRITE(IW,9010) NNM,ENM
         ELSE IF(VIBLVL) THEN
            WRITE(IW,9015) EKIN
         ELSE
            WRITE(IW,9020) NVEL,EKIN
            WRITE(IW,*) 'NORMALIZED INITIAL VELOCITY VECTOR'
            I0=0
            DO 185 I=1,NAT
               WRITE(IW,9030) I,(VELO0(II+I0),II=1,3)
               I0 = I0+3
  185       CONTINUE
         END IF
         IF(NMANAL) THEN
            WRITE(IW,*) 'EQUILIBRIUM GEOMETRY EXPANSION CENTER'
            DO 186 I=1,NAT
               WRITE(IW,9040) I,(C0(II,I),II=1,3)
  186       CONTINUE
            WRITE(IW,'('' '')')
         END IF
      END IF
C
      IF(VIBLVL) THEN
         DO 190 K = 1, NAT36
            PED(K) = ONE / (FREQ(K) * CLIGHT) / FEMTO
  190    CONTINUE
         IF (MASWRK) WRITE(IW,9050)
         KMIN = 1
         KMAX = 6
  200    IF(KMAX.GT.NAT36) KMAX = NAT36
         IF (MASWRK) THEN
            IF(KMAX-KMIN.NE.2) THEN
               WRITE(IW,9060) (K,K=KMIN,KMAX)
            ELSE
               WRITE(IW,9065) (K,K=KMIN,KMAX)
            ENDIF
            WRITE(IW,9070) (FREQ(K), K = KMIN, KMAX)
            WRITE(IW,9080) (PED(K),  K = KMIN, KMAX)
            WRITE(IW,9085) (ABS(VIBENG(K))*TOKCAL, K = KMIN, KMAX)
            WRITE(IW,'('' '')')
         END IF
         IF(KMAX.EQ.NAT36) GO TO 210
         KMIN = KMIN + 6
         KMAX = KMAX + 6
         GO TO 200
      END IF
C
C        PRINT ISOTOPES USED
C
      IF(MASWRK) THEN
         WRITE(IW,9086)
         WRITE(IW,9087) (III,ZMASS(III),III=1,NAT)
      END IF
C
  210 CONTINUE
      NPRINT= 7
      NPUNCH= 2
      IF(NPRT.EQ.-1) NPRINT=-5
      IF(NPUN.EQ.-1) NPUNCH= 0
      IF(NPUNCH.GT.0  .AND.  MASWRK) WRITE(IP,7000) TOTIME
C
C        WAVEFUNCTION AND GRADIENT AT INITIAL GEOMETRY
C
      CALL ENERGX
      IF(E.EQ.ZERO  .AND.  EXETYP.NE.CHECK) THEN
         IF (MASWRK) THEN
            WRITE(IW,*) 'SCF FAILED TO CONVERGE AT FIRST DRC POINT'
         END IF
         RETURN
      END IF
      IF (CITYP.EQ.GUGA) THEN
         CALL CIGRAD
      ELSE
         CALL HFGRAD
      END IF
      IF(EXETYP.EQ.CHECK) RETURN
C                         ******
C
C           INITIAL TOTAL ENERGY MUST BE CONSERVED ON ENTIRE DRC
C
      ETOT  = EPOT + EKIN
C
C   UNIT OF GRADIENTS  :    EG(I) HARTREE/BOHR
C   UNIT OF ACCELATION : ACCEL(I) BOHR/FS**2
C   UNIT OF VELOCITY   : VELO0(I) BOHR/FS
C
      JJ = 0
      DO 220 I = 1, NAT
         AMS = ZMASS(I)
         DO 220 J = 1, 3
            JJ = JJ + 1
            ACCEL(JJ)  = -EG(JJ) / AMS * FAC
            ACCOLD(JJ) = ACCEL(JJ)
            IF(NNM.GT.0) THEN
               VELO0(JJ)  = L(JJ,NNM) / SQRT(AMS) * SQRT(EKIN*FAC*TWO)
            ELSE IF(NNM.LT.0) THEN
               VELO0(JJ)  = -L(JJ,-NNM) / SQRT(AMS) * SQRT(EKIN*FAC*TWO)
            ELSE IF(VIBLVL) THEN
               TEMP=ZERO
               DO 215 IMODE=1,NAT36
                  IF (VIBENG(IMODE).GT.ZERO) THEN
                     TEMP = TEMP + L(JJ,IMODE) / SQRT(AMS)
     *                           *SQRT(ABS(VIBENG(IMODE))*FAC*TWO)
                  ELSE
                     TEMP = TEMP - L(JJ,IMODE) / SQRT(AMS)
     *                           *SQRT(ABS(VIBENG(IMODE))*FAC*TWO)
                  ENDIF
  215          CONTINUE
               VELO0(JJ) = TEMP
            ELSE IF(.NOT.NVEL) THEN
               VELO0(JJ)  = VELO0(JJ) * SQRT(EKIN*FAC*TWO)
            END IF
  220 CONTINUE
C
      IF (.NOT.MASWRK.AND.NMANAL) CALL NMDANA(IW,IPIRC,L2,VELO0,C0,C1,
     *                                        Q,P,NAT1,NAT3,EKIN,TOTIME)
      IF (MASWRK) THEN
         WRITE(IW,8000)
         IF(NMANAL) THEN
            WRITE(IPIRC,8010)
            WRITE(IW   ,8060)
            WRITE(IW   ,8010)
            CALL NMDANA(IW,IPIRC,L2,VELO0,C0,C1,
     *                  Q,P,NAT1,NAT3,EKIN,TOTIME)
         ELSE
            WRITE(IPIRC,8020)
            WRITE(IW   ,8060)
            WRITE(IW   ,8020)
            WRITE(IPIRC,8030) TOTIME,EKIN,EPOT,EKIN+EPOT
            WRITE(IW   ,8030) TOTIME,EKIN,EPOT,EKIN+EPOT
         END IF
         WRITE(IPIRC,8040)
         WRITE(IW   ,8060)
         WRITE(IW   ,8040)
         WRITE(IW   ,8060)
         DO 225 I = 1, NAT
            ZNUC = ZAN(I) + IZCORE(I)
            WRITE(IPIRC,8050) ZNUC,(C(K,I),K=1,3),
     *                        (VELO0(JJ),JJ=3*I-2,3*I)
            WRITE(IW   ,8050) ZNUC,(C(K,I),K=1,3),
     *                        (VELO0(JJ),JJ=3*I-2,3*I)
  225    CONTINUE
         WRITE(IPIRC,8060)
         WRITE(IW   ,8060)
         WRITE(IW   ,9130)
         CALL EGOUT(EG,NAT)
         WRITE(IW,'()')
      END IF
C
      QUADR = ONE
      NPCNT = 0
C
C        NOW LOOP OVER ALL REMAINING DRC POINTS
C
      DO 290 ILOOP = 1, NSTEP
         EKIN = ZERO
         JJ = 0
         DO 250 I = 1, NAT
            AMS = ZMASS(I)
            DO 250 J = 1,3
               JJ = JJ + 1
               IF(ILOOP.GE.3) THEN
                  VELO3(JJ) = (ACCEL(JJ) - ACCOLD(JJ) * TWO
     1                         + ACCOL2(JJ)) / DELTAT**2
                  VELO2(JJ) = (ACCEL(JJ) - ACCOLD(JJ)) / DELTAT
     1                         + VELO3(JJ) * DELTAT / TWO
               ELSE
                  VELO2(JJ) = (ACCEL(JJ) - ACCOLD(JJ)) / DELTAT
                  VELO3(JJ) = ZERO
               END IF
               C(J,I)    = C(J,I)
     1                   + DELTAT    * VELO0(JJ)
     2                   + DELTAT**2 * ACCEL(JJ) / TWO
     3                   + DELTAT**3 * VELO2(JJ) / 6.0D+00
     4                   + DELTAT**4 * VELO3(JJ) / 24.0D+00
               VELO0(JJ) = VELO0(JJ)
     1                   + DELTAT    * ACCEL(JJ)
     2                   + DELTAT**2 * VELO2(JJ) / TWO
     3                   + DELTAT**3 * VELO3(JJ) / 6.0D+00
C
C      VELOCITY IS SCALED FOR TOTAL ENERGY CONSERVATION
C
               VELO0(JJ) = VELO0(JJ) * QUADR
               EKIN = EKIN + VELO0(JJ)**2 * AMS
  250    CONTINUE
         EKIN = 0.5D+00 * EKIN / FAC
         DO 260 K = 1, NAT3
            ACCOL2(K) = ACCOLD(K)
            ACCOLD(K) = ACCEL(K)
  260    CONTINUE
C
         NPRINT = -5
         IF(NPRT.EQ.1) NPRINT=7
         IF(NPRT.EQ.0  .AND.  ILOOP.EQ.NSTEP) NPRINT=7
         NPUNCH = 0
         IF(NPUN.EQ.2) NPUNCH=2
         IF(NPUN.EQ.1) NPUNCH=1
         IF(NPUN.GE.0  .AND.  ILOOP.EQ.NSTEP) NPUNCH=2
C
         TOTIME = TOTIME + DELTAT
         IF(NPUNCH.GT.0  .AND.  MASWRK) WRITE(IP,7000) TOTIME
C
C           WAVEFUNCTION AND GRADIENT AT CURRENT POINT
C
         CALL ENERGX
         IF(E.EQ.ZERO  .AND.  EXETYP.NE.CHECK) THEN
            IF (MASWRK) THEN
               WRITE(IW,*) 'SCF FAILED TO CONVERGE AT DRC POINT',ILOOP
            END IF
            RETURN
         END IF
         IF (CITYP.EQ.GUGA) THEN
            CALL CIGRAD
         ELSE
            CALL HFGRAD
         END IF
C
         JJ = 0
         DO 270 I = 1, NAT
            AMS = ZMASS(I)
            DO 270 J = 1,3
               JJ = JJ + 1
               ACCEL(JJ) = -EG(JJ) / AMS * FAC
  270    CONTINUE
         ERROR  = ETOT - (EKIN + EPOT)
C-OLDER- QUADR  =      ONE + ERROR / (EKIN + 1.0D-10) / TWO
         QUADR  = SQRT(ONE + ERROR / (EKIN + 1.0D-10))
         QUADR  = MIN(1.3D+00, MAX(0.8D+00, QUADR))
C
         NPCNT = NPCNT + 1
         IF(NPCNT.NE.NPRTSM) GO TO 280
         IF (.NOT.MASWRK.AND.NMANAL) CALL NMDANA(IW,IPIRC,L2,VELO0,
     *                                           C0,C1,Q,P,NAT1,NAT3,
     *                                           EKIN,TOTIME)
         IF (MASWRK) THEN
            IF(NMANAL) THEN
               WRITE(IPIRC,8010)
               WRITE(IW   ,8060)
               WRITE(IW   ,8010)
               CALL NMDANA(IW,IPIRC,L2,VELO0,C0,C1,Q,P,NAT1,NAT3,EKIN,
     *                     TOTIME)
            ELSE
               WRITE(IPIRC,8020)
               WRITE(IW   ,8060)
               WRITE(IW   ,8020)
               WRITE(IPIRC,8030) TOTIME,EKIN,EPOT,EKIN+EPOT
               WRITE(IW   ,8030) TOTIME,EKIN,EPOT,EKIN+EPOT
            END IF
            WRITE(IPIRC,8040)
            WRITE(IW   ,8060)
            WRITE(IW   ,8040)
            WRITE(IW   ,8060)
            DO 230 I = 1, NAT
               ZNUC = ZAN(I) + IZCORE(I)
               WRITE(IPIRC,8050) ZNUC,(C(K,I),K=1,3),
     *                           (VELO0(JJ),JJ=3*I-2,3*I)
               WRITE(IW   ,8050) ZNUC,(C(K,I),K=1,3),
     *                           (VELO0(JJ),JJ=3*I-2,3*I)
  230       CONTINUE
            WRITE(IPIRC,8060)
            WRITE(IW   ,8060)
            WRITE(IW   ,9130)
            CALL EGOUT(EG,NAT)
            WRITE(IW,'()')
         END IF
         NPCNT = 0
C
C        ----- PRINT RESTART DRC INFORMATION -----
C
  280    CONTINUE
         IF(MASWRK) THEN
            CALL DSCAL(NAT3,TOANGS,C0,1)
            CALL DSCAL(NAT3,TOANGS,C ,1)
            WRITE(IW,*) '----- RESTART INFORMATION FOR NEXT DRC RUN:'
            WRITE(IW,*) 'COORDINATES (IN ANGSTROM) FOR $DATA GROUP ARE'
            WRITE(IW,9090) (ANAM(I),BNAM(I),ZAN(I)+IZCORE(I),
     *                      (C(J,I),J=1,3),I=1,NAT)
            WRITE(IW,9120)
            WRITE(IW,9100) DELTAT,TOTIME,NPRTSM,NMANAL,NPRT,NPUN
            WRITE(IW,9105) (VELO0(I),I=1,NAT3)
            IF(NMANAL) THEN
               WRITE(IW,9110) HESS2
               WRITE(IW,9115) ((C0(J,I),J=1,3),I=1,NAT)
            END IF
            WRITE(IW,9120)
            CALL DSCAL(NAT3,ONE/TOANGS,C0,1)
            CALL DSCAL(NAT3,ONE/TOANGS,C ,1)
         END IF
C
C            ENSURE ALL NODES WORK WITH THE SAME COORDINATES EXACTLY,
C            FOR EXAMPLE THIS DEALS WITH ROUNDOFF ERRORS IN THE
C            IF BLOCK JUST ABOVE, BUT ALSO ANY OTHER POSSIBLE ERRORS
C            IN THE REPLICATED COMPUTATION OF THE TRAJECTORIES.
C
         IF (GOPARR) CALL DDI_BCAST(426,'F',C,NAT3,MASTER)
C
C            THE QUADR FACTOR IS UNKNOWN THE FIRST TIME THROUGH THE
C            LOOP AND SO ENERGY CONSERVATION TEST SHOULD BE AVOIDED.
C
         IF(ILOOP.NE.1  .AND.  ABS(ERROR).GE.5.0D-05) THEN
            IF(MASWRK) THEN
               WRITE(IW,*) 'ENERGY CONSERVATION VIOLATED ON THIS STEP.'
               WRITE(IW,*) 'THIS REGION REQUIRES A SMALLER TIME STEP.'
            ENDIF
            RETURN
         END IF
C
         IF(FGPAIR.NE.0) THEN
            DO 288 I = 1, FGPAIR
               I1 = FRAGAT(I*2-1)
               I2 = FRAGAT(I*2)
               DIS=0.D0
               DO 285 K = 1, 3
                  DIS = DIS + (C(K,I1)-C(K,I2))**2
  285          CONTINUE
               DIS = SQRT(DIS)
               IF(FRGCUT(I).GT.0.D0) THEN
                  IF(DIS.LE.FRGCUT(I).AND.FRGINI(I).EQ.1) FRGINI(I)=0
                  IF(DIS.GE.FRGCUT(I).AND.FRGINI(I).EQ.0) THEN
                     IF(MASWRK) WRITE(IW,'(''DRC RUN TERMINATES SINCE'',
     *               '' ATOM PAIR '',2I3,'' SEPARATES SUFFICIENTLY'')')
     *               I1,I2
                     RETURN
                  END IF
               ELSEIF(FRGCUT(I).LT.0.D0) THEN
                  IF(DIS.GE.-FRGCUT(I).AND.FRGINI(I).EQ.1) FRGINI(I)=0
                  IF(DIS.LE.-FRGCUT(I).AND.FRGINI(I).EQ.0) THEN
                     IF(MASWRK) WRITE(IW,'(''DRC RUN TERMINATES SINCE'',
     *               '' ATOM PAIR '',2I3,'' APPROACHES SUFFICIENTLY'')')
     *               I1,I2
                     RETURN
                  END IF
               END IF
  288       CONTINUE
         END IF
C
  290 CONTINUE
      RETURN
C
 7000 FORMAT(/'------ DATA FOR DRC POINT AT T=',F10.2,' ------')
C
 8000 FORMAT(1X,24("*")/1X,'START OF DRC CALCULATION'/1X,24("*"))
 8010 FORMAT(1X,'  TIME     MODE     Q              P     ',
     *          'KINETIC      POTENTIAL          TOTAL'/
     *       1X,'   FS       BOHR*SQRT(AMU) BOHR*SQRT(AMU)/FS',
     *          '   E         ENERGY         ENERGY')
 8020 FORMAT(1X,'  TIME        KINETIC       POTENTIAL       TOTAL'/
     *       1X,'              ENERGY        ENERGY          ENERGY')
 8030 FORMAT(1X,F9.4,3X,3(F10.5,5X))
 8040 FORMAT(11X,'CARTESIAN COORDINATES (BOHR)',15X,
     *      'VELOCITY (BOHR/FS)')
 8050 FORMAT(1X,F4.1,1X,3F11.5,3X,3F11.5)
 8060 FORMAT(1X,75("-"))
C
 9000 FORMAT(/1X,30("-")/1X,'PARAMETERS FOR DRC CALCULATION'/1X,30("-")/
     *       5X,'NSTEP  = ',I5,5X,'DELTAT = ',F6.4,' FS',
     *       3X,'TOTIME = ',F9.4,' FS'/
     *       5X,'NPRT   = ',I5,5X,'NPUN   = ',I4,8X,'NPRTSM =',I5/
     *       5X,'NMANAL =',L4,7X,'NHESTS = ',I4,8X,'VIBLVL =',L4/
     *       5X,'FGPAIR = ',I5)
 9005 FORMAT(5X,'FRAGAT = ',I3,',',I3,3X,'FRGCUT =',F8.3)
 9010 FORMAT(5X,'NNM    = ',I5,5X,'ENM    = ',F4.2,' QUANTA'/)
 9015 FORMAT(5X,'EKIN   = ',F6.5,' HARTREE'/)
 9020 FORMAT(5X,'NVEL   = ',L5,5X,'EKIN   = ',F6.5,' HARTREE'/)
 9030 FORMAT(5X,'ATOM',I5,'  VEL=',3F15.8)
 9040 FORMAT(5X,'ATOM',I5,'   C0=',3F15.8,' BOHR')
 9050 FORMAT(5X,'FREQUENCY (CM**-1), PERIOD (FS), AND ENERGY (KCAL/MOL)'
     *      /5X,'OF EACH NORMAL MODE -L- AT STARTING GEOMETRY')
 9060 FORMAT(5X,6(3X,'L',I3,2X))
 9065 FORMAT(5X,3(3X,'L',I3,2X))
 9070 FORMAT(3X,6F9.1)
 9080 FORMAT(3X,6F9.2)
 9085 FORMAT(3X,6F9.3)
 9086 FORMAT(/1X,'ATOMIC ISOTOPES USED DURING THIS CALCULATION ARE')
 9087 FORMAT(4(I5,'=',F13.6))
 9090 FORMAT(1X,A8,A2,F5.1,3F20.10)
 9100 FORMAT(' $DRC NSTEP=??  DELTAT=',F6.4,'  TOTIME=',F9.4/
     *       '      NPRTSM=',I2,'  NMANAL=.',L1,'.',
     *       '  NPRT=',I2,'  NPUN=',I2,'  NVEL=.TRUE.')
 9105 FORMAT('  VEL(1)=',3(2X,F15.12)/(9X,3(2X,F15.12)))
 9110 FORMAT('  HESS2=',A8)
 9115 FORMAT('  C0(1) =',3(2X,F15.9)/(9X,3(2X,F15.9)))
 9120 FORMAT(' $END')
 9130 FORMAT(/25X,22("-")/25X,"GRADIENT OF THE ENERGY"/25X,22("-"))
      END
C*MODULE DRC     *DECK GENNM
      SUBROUTINE GENNM(FCM,A,CMODES,FREQ,SCR,NAT,NAT3,IOPT1,IOPT2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C       NOTE: SYMMETRIC STORAGE -A- MAY BE EQUIVALENT BY CALL TO -FCM-,
C             IN WHICH CASE THE ORIGINAL HESSIAN IS DESTROYED ON EXIT.
C
      DIMENSION FCM(NAT3,NAT3),A(*),CMODES(NAT3,NAT3),FREQ(NAT3),
     *          SCR(NAT3,8)
C
      PARAMETER (MXATM=500, MXAO=2047)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /MASSES/ ZMASS(MXATM)
C
      PARAMETER (ONE=1.0D+00)
      PARAMETER (TFACT=2.642461D+07)
C
C     ----- GENERATE NORMAL MODES OF VIBRATION -----
C     IOPT1=0/1 MEANS RETURN MODES IN MASSWEIGHTED/ORDINARY CARTESIANS
C     IOPT2=-1/0/1 MEANS TS, DROP 2-7/KEEP ALL/EQ, DROP 1-6 MODES
C
C        MASS WEIGHT THE HESSIAN
C
      J=0
      DO 150 JAT=1,NAT
         RMJ = ONE/SQRT(ZMASS(JAT))
         DO 140 JXYZ=1,3
            J = J+1
            I = 0
            DO 130 IAT=1,NAT
               RMI = ONE/SQRT(ZMASS(IAT))
               DO 120 IXYZ=1,3
                  I = I+1
                  CMODES(I,J) = RMI * FCM(I,J) * RMJ
  120          CONTINUE
  130       CONTINUE
  140    CONTINUE
  150 CONTINUE
C
C        COPY TO SYMMETRIC STORAGE, DIAGONALIZE TO OBTAIN NORMAL MODES
C
      IJ=0
      DO 180 I=1,NAT3
         DO 170 J=1,I
            IJ=IJ+1
            A(IJ) = CMODES(I,J)
  170    CONTINUE
  180 CONTINUE
C
      CALL GLDIAG(NAT3,NAT3,NAT3,A,SCR,FREQ,CMODES,IERR,IA)
C
C        CONVERT FREQUENCIES TO WAVE NUMBERS
C
      DO 210 I = 1,NAT3
         FREQ(I) = SQRT(ABS(TFACT*FREQ(I)))
  210 CONTINUE
C
C        IF DESIRED, CONVERT NORMAL MODES BACK TO CARTESIAN SPACE
C
      IF(IOPT1.EQ.1) THEN
         DO 340 JMODE=1,NAT3
            I=0
            DO 330 IAT=1,NAT
               AMASS = ONE/SQRT(ZMASS(IAT))
               DO 300 IXYZ=1,3
                  I=I+1
                  CMODES(I,JMODE) = AMASS * CMODES(I,JMODE)
  300          CONTINUE
  330       CONTINUE
  340    CONTINUE
      END IF
C
      CALL STFASE(CMODES,NAT3,NAT3,NAT3)
C
C        IF DESIRED, DROP SIX TRANSLATIONS AND VIBRATIONS
C        THE CALLER IS SUPPOSED TO KNOW IF THIS IS MODES 1-6 OR 2-7.
C
      IF(IOPT2.NE.0) THEN
         IF(IOPT2.GT.0) IBEG = 7
         IF(IOPT2.LT.0) IBEG = 8
         DO 420 IFROM=IBEG,NAT3
            ITO = IFROM-6
            FREQ(ITO) = FREQ(IFROM)
            CALL DCOPY(NAT3,CMODES(1,IFROM),1,CMODES(1,ITO),1)
  420    CONTINUE
      END IF
      RETURN
      END
C*MODULE DRC     *DECK NMDANA
      SUBROUTINE NMDANA(IW,IPIRC,L,VELO0,C0,C1,
     *                  Q,P,NAT1,NAT3,EKIN,TOTIME)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXATM=500,MXRT=100)
      LOGICAL GOPARR,DSKWRK,MASWRK
      DOUBLE PRECISION L(NAT3,NAT3)
      DIMENSION VELO0(NAT3),C0(3,NAT1),C1(3,NAT1),Q(NAT3),P(NAT3)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /MASSES/ ZMASS(MXATM)
      COMMON /ENRGYS/ ENUCR,EELCT,EPOT,SZ,SZZ,ECORE,ESCF,EERD,E1,E2,
     1                VEN,VEE,EPOT00,EKIN00,ESTATE(MXRT),STATN
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (ZERO=0.0D+00)
C
      NAT36 = NAT * 3 - 6
C
C        OBTAIN CENTER OF MASS COORDINATES
C
      SUMW=ZERO
      SUMX=ZERO
      SUMY=ZERO
      SUMZ=ZERO
      DO I = 1, NAT
         AMS = ZMASS(I)
         SUMW= SUMW+ AMS
         SUMX= SUMX+ C(1,I)*AMS
         SUMY= SUMY+ C(2,I)*AMS
         SUMZ= SUMZ+ C(3,I)*AMS
      ENDDO
      DO I = 1, NAT
         C1(1,I) = C(1,I) - SUMX/SUMW
         C1(2,I) = C(2,I) - SUMY/SUMW
         C1(3,I) = C(3,I) - SUMZ/SUMW
      ENDDO
C
      DO K = 1, NAT36
         Q(K) = ZERO
         P(K) = ZERO
         JJ = 0
         DO I = 1, NAT
            AMS = ZMASS(I)
            DO J = 1, 3
               JJ = JJ + 1
               Q(K) = Q(K) + (C1(J,I) - C0(J,I)) * SQRT(AMS) * L(JJ,K)
               P(K) = P(K) + VELO0(JJ) * SQRT(AMS) * L(JJ,K)
            ENDDO
         ENDDO
      ENDDO
C
      IF (MASWRK) THEN
         WRITE(IPIRC,8010) TOTIME, Q(1), P(1), EKIN, EPOT, EKIN + EPOT
         WRITE(IW   ,8010) TOTIME, Q(1), P(1), EKIN, EPOT, EKIN + EPOT
         DO K = 2, NAT36
            WRITE(IPIRC,8020) K, Q(K), P(K)
            WRITE(IW   ,8020) K, Q(K), P(K)
         ENDDO
      END IF
      RETURN
C
 8010 FORMAT(1X,F9.4,'  L 1    ',2(F10.6),3(F10.5,5X))
 8020 FORMAT(11X,' L',I2,4X,2(F10.6))
      END
C*MODULE DRC     *DECK FCMIN2
      SUBROUTINE FCMIN2(FCM,NC1,GOTEH)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOTEH,GOPARR,DSKWRK,MASWRK
C
      DIMENSION FCM(*)
      DIMENSION TEXT1(10)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C     ----- READ HESSIAN MATRIX FROM CARDS -----
C
      NC2=NC1
      NCT2=NC1*NC1
      ISTART=1
C
      CALL VCLR(FCM,1,NCT2)
C
      CALL SEQREW(IR)
      CALL FNDGRP(IR,' $HESS2  ',IEOF)
      IF (IEOF.EQ.1) THEN
         GOTEH=.FALSE.
         GO TO 800
      ELSE
         GOTEH=.TRUE.
      END IF
C
      IF (MASWRK) THEN
         READ (IR,9000) TEXT1
         WRITE(IW,9010) TEXT1
C
         II = 0
         ICC = 0
         DO 240 I=1,NC1
            IC=0
            DO 220 MINCOL = 1, NC1, 5
               MAXCOL=MINCOL+4
               IF(MAXCOL.GT.NC1) MAXCOL=NC1
               IC=IC+1
               READ(IR,9020) II,ICC,
     *           (FCM(ISTART+(J-1)*NC2),J=MINCOL,MAXCOL)
               IMD100 = MOD(I,100)
               IF (II .NE. IMD100 .AND. ICC .NE. IC) THEN
                  WRITE(IW,9030) I,IC,II,ICC
                  CALL ABRT
               END IF
  220       CONTINUE
            ISTART=ISTART+1
  240    CONTINUE
      END IF
C
C SEND FCM TO ALL SLAVES
C
      IF (GOPARR) CALL DDI_BCAST(425,'F',FCM,NCT2,MASTER)
C
  800 CONTINUE
C     CALL DAWRIT(IDAF,IODA,FCM,NCT2,4,0)
      RETURN
C
 9000 FORMAT(10A8)
 9010 FORMAT(/1X,'$HESS2 GROUP READ FROM CARDS'/1X,10A8)
 9020 FORMAT(I2,I3,5F15.8)
 9030 FORMAT(' *** ERROR READING FORCE CONSTANT MATRIX ELEMENT',2I4/
     *       '           INPUT WAS',2I4/
     *  'DO YOU NEED TO INCLUDE A TITLE CARD BELOW THE $HESS2 CARD?'/
     *  'A BLANK TITLING CARD IS PERFECTLY OK.')
      END
