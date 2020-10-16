C  9 dec 03 - TJD - detinp: REMOVE SYM. lowering FOR ANALYTIC HESSIANS
C  3 Jul 03 - MWS - ECOR,PRICI2: change to 100 active orbitals
C 16 Jun 03 - MWS - DAVCI: work around Cray read error
C 28 Jan 03 - JI  - DAVCI: changes for root tracking
C 14 Jan 03 - MWS - ALDECI: fix dir.trf., DETFCI: no spin array overflow
C  7 Aug 02 - MWS - DAVCI: always print message if CI states are uncnvgd
C 20 Jun 02 - MWS - DAVCI: clobbering of CI vector file, fix KST>NSTATE
C 22 May 02 - JI  - correlation energy analysis implemented
C 22 May 02 - GDF - allow parallel execution by replicated computation
C 16 Feb 02 - JI  - DAVCI: state tracking changes
C 24 Jan 02 - JI  - PRICI2: avoid printing zero coef
C  6 Sep 01 - MWS - DETINP: add backdoor to permit degenerate state opts
C  1 Aug 01 - JI  - DAVCI: patches for degenerate root solving
C 13 Jun 01 - JI  - DAVCI: correction to ms=0 CI zeroing when S=2,4,..
C 29 DEC 00 - JI  - DETINP: CORRECT MXXPAN DEFAULT FOR MANY STATES
C  7 Nov 00 - JI  - Basically redo to include symmetry.
C 21 DEC 99 - MWS - ALDECI: REMOVE C1 SYMMETRY FORCE FOR NON-ABELIAN
C 13 MAR 99 - MWS - MATRSA: WORK AROUND NO BETA ELECTRON BUG
C 12 NOV 98 - GDF - CHANGE BIT PACKING TO ISHIFT
C 27 OCT 98 - MAF - DETINP: ALLOW FOR USE OF SPHERICAL HARMONICS
C 27 SEP 98 - MWS - DETCI,DAVCI: KILL JOB IF ROOTS DON'T CONVERGE
C 16 MAY 98 - MWS - ALDECI: DOWNSHIFT NONABELIAN TRANSF TO C1 SYMMETRY
C  6 MAY 98 - JI  - ADD NEW FULL CI CODE TO GAMESS
C
C*MODULE ALDECI  *DECK ALDECI
C     ------------------------------
      SUBROUTINE ALDECI(NRNFG,NPFLG)
C     ------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION NRNFG(10),NPFLG(10)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,DOEXCH,
     *        DDITRF,DOOOOO,DOVOOO,DOVVOO,DOVOVO
C
      PARAMETER (MXRT=100)
C
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,S,SZ,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORB,
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NCI,
     *                IGPDET,KSTSYM,NFTGCI
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CIDET/8HCIDET   /, RNONE/8HNONE    /
#else
      CHARACTER*8 :: CIDET_STR
      EQUIVALENCE (CIDET, CIDET_STR)
      CHARACTER*8 :: RNONE_STR
      EQUIVALENCE (RNONE, RNONE_STR)
      DATA CIDET_STR/"CIDET   "/, RNONE_STR/"NONE    "/
#endif
C
C        driver for determinant based CI calculations...
C
C        ----- read input defining the full CI dimensions -----
C
      CALL DETINP(NPFLG(1),CIDET)
C
C        ----- integral transformation -----
C
      DDITRF=GOPARR
      DOOOOO=.TRUE.
      DOVOOO=.FALSE.
      DOVVOO=.FALSE.
      DOVOVO=.FALSE.
      DOEXCH=SCFTYP.EQ.RNONE
      CALL TRFMCX(NPFLG(2),NCOR,NORB,NORB,.FALSE.,DOEXCH,
     *            DDITRF,DOOOOO,DOVOOO,DOVVOO,DOVOVO)
C
C        ----- direct full CI calculation -----
C
      CALL DETFCI(NPFLG(3),.FALSE.,DDITRF)
C
C        ----- 1e- density matrix and natural orbitals -----
C
      IF(NRNFG(5).GT.0) CALL DETDM1(NPFLG(5))
C
C        ----- state averaged 1e- and 2e- density matrix -----
C
      IF(NRNFG(6).GT.0) CALL DETDM2(NPFLG(6))
      RETURN
      END
C*MODULE ALDECI  *DECK DETINP
      SUBROUTINE DETINP(NPRINT,GPNAME)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME,PURES,GOPARR,DSKWRK,MASWRK,ABEL,WTSOK,ANALYS,CLOBBR
C
      PARAMETER (MXATM=500, MXRT=100, MXAO=2047, MXSH=1000)
C
      COMMON /DETPAR/ ICLBBR,ANALYS
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,S,SZ,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORB,
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NCI,
     *                IGPDET,KSTSYM,NFTGCI
      COMMON /FMCOM / X(1)
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,MA,MB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IJKT,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /SYMMOL/ GROUP,COMPLEX,IGROUP,NAXIS,ILABMO,ABEL
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                T(432),INVT(48),NT
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, TWO=2.0D+00)
C
      DIMENSION FANT(8),LFANT(8),GANT(27),LGANT(8)
C
      PARAMETER (NNAM=20)
      DIMENSION QNAM(NNAM),KQNAM(NNAM)
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QNAM/8HGROUP   ,8HISTSYM  ,
     *          8HNCORE   ,8HNACT    ,8HNELS    ,8HSZ      ,
     *          8HNSTATE  ,8HNSTGSS  ,8HNHGSS   ,8HMXXPAN  ,
     *          8HITERMX  ,8HCVGTOL  ,8HPRTTOL  ,
     *          8HIROOT   ,8HNFLGDM  ,8HPURES   ,8HWSTATE  ,
     *          8HWTSOK   ,8HANALYS  ,8HCLOBBR  /
#else
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"GROUP   ","ISTSYM  ",
     *          "NCORE   ","NACT    ","NELS    ","SZ      ",
     *          "NSTATE  ","NSTGSS  ","NHGSS   ","MXXPAN  ",
     *          "ITERMX  ","CVGTOL  ","PRTTOL  ",
     *          "IROOT   ","NFLGDM  ","PURES   ","WSTATE  ",
     *          "WTSOK   ","ANALYS  ","CLOBBR  "/
#endif
      DATA KQNAM/5,1,   1,1,1,3,  1,1,1,1,   1,3,3,  1,-1,0,-3,   0,0,0/
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DET,CIDET/8HDET     ,8HCIDET   /
#else
      CHARACTER*8 :: CIDET_STR, DET_STR
      EQUIVALENCE (CIDET, CIDET_STR), (DET, DET_STR)
      DATA DET_STR,CIDET_STR/"DET     ","CIDET   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA HESS/8HHESSIAN /
#else
      CHARACTER*8 :: HESS_STR
      EQUIVALENCE (HESS, HESS_STR)
      DATA HESS_STR/"HESSIAN "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA FANT/8HC1      ,8HCI      ,8HCS      ,8HC2      ,
     *          8HD2      ,8HC2V     ,8HC2H     ,8HD2H     /
#else
      CHARACTER*8 :: FANT_STR(8)
      EQUIVALENCE (FANT, FANT_STR)
      DATA FANT_STR/"C1      ","CI      ","CS      ","C2      ",
     *          "D2      ","C2V     ","C2H     ","D2H     "/
#endif
      DATA LFANT/1,1,1,1,2,2,2,3/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA GANT/8HA       ,8HAG      ,8HAU      ,8HA'      ,
     *          8HA"      ,8HA       ,8HB       ,8HA       ,
     *          8HB1      ,8HB2      ,8HB3      ,8HA1      ,
     *          8HA2      ,8HB1      ,8HB2      ,8HAG      ,
     *          8HBG      ,8HBU      ,8HAU      ,8HAG      ,
     *          8HB1G     ,8HB2G     ,8HB3G     ,8HAU      ,
     *          8HB1U     ,8HB2U     ,8HB3U     /
#else
      CHARACTER*8 :: GANT_STR(27)
      EQUIVALENCE (GANT, GANT_STR)
      DATA GANT_STR/"A       ","AG      ","AU      ","A'      ",
     *          'A"      ','A       ','B       ','A       ',
     *          'B1      ','B2      ','B3      ','A1      ',
     *          'A2      ','B1      ','B2      ','AG      ',
     *          'BG      ','BU      ','AU      ','AG      ',
     *          'B1G     ','B2G     ','B3G     ','AU      ',
     *          'B1U     ','B2U     ','B3U     '/
#endif
      DATA LGANT/0,1,3,5,7,11,15,19/
C
      SOME = MASWRK  .AND.  NPRINT.NE.-5  .AND.  NPRINT.NE.-23
C
      IF(SOME) WRITE(IW,9000)
C
C          set up input to specify the CI space
C          select point group and electron/orbital counts
C
      GRPDET = FANT(1)
      IF(IGROUP.EQ.1)                GRPDET = FANT(1)
      IF(IGROUP.EQ.3)                GRPDET = FANT(2)
      IF(IGROUP.EQ.2)                GRPDET = FANT(3)
      IF(IGROUP.EQ.4.AND.NAXIS.EQ.2) GRPDET = FANT(4)
      IF(IGROUP.EQ.8.AND.NAXIS.EQ.2) GRPDET = FANT(5)
      IF(IGROUP.EQ.7.AND.NAXIS.EQ.2) GRPDET = FANT(6)
      IF(IGROUP.EQ.6.AND.NAXIS.EQ.2) GRPDET = FANT(7)
      IF(IGROUP.EQ.9.AND.NAXIS.EQ.2) GRPDET = FANT(8)
      IF(NT.EQ.1) GRPDET=FANT(1)
      IF(RUNTYP.EQ.HESS  .AND.  NHLEVL.GT.0) GRPDET=FANT(1)
      KSTSYM = 1
C
      NCORE  = 0
      NACT   = 0
      NELS   = 0
      SZ     = (MUL-1)/TWO
C
C          set up input to control the diagonalization
C
      NSTATE = 1
      NSTGSS = 1
      NHGSS  = 300
      MXXPAN = 10
      ITERMX = 100
      CVGTOL = 1.0D-05
      PRTTOL = 0.05D+00
C
C          set up input to control the first order density computation
C
      IROOT=1
      KQNAM(15)=MXRT*10 + 1
      DO 5 I=1,MXRT
         NFLGDM(I) = 0
    5 CONTINUE
      NFLGDM(1)=1
C
C          set up input to control the second order density computation
C
      PURES = .TRUE.
      KQNAM(17)=MXRT*10 + 3
      CALL VCLR(WSTATE,1,MXRT)
      WSTATE(1) = ONE
      WTSOK  = .FALSE.
      ANALYS = .FALSE.
      CLOBBR = .FALSE.
C
      CALL NAMEIO(IR,JRET,GPNAME,NNAM,QNAM,KQNAM,
     *            GRPDET,KSTSYM,NCORE,NACT,NELS,SZ,NSTATE,NSTGSS,
     *            NHGSS,MXXPAN,ITERMX,CVGTOL,PRTTOL,IROOT,NFLGDM,
     *            PURES,WSTATE,WTSOK,ANALYS,CLOBBR,
     *            0,0,0,0,
     *    0,0,0,0,0,    0,0,0,0,0,    0,0,0,0,0,    0,0,0,0,0,
     *    0,0,0,0,0,    0,0,0,0,0,    0,0,0,0,0,    0,0,0,0,0)
      IF(JRET.EQ.2) THEN
         IF(MASWRK) WRITE(IW,9010) GPNAME
         CALL ABRT
      END IF
C
      IGPDET = -1
C
C        numerical hessians for mcscf make atomic displacements into c1
C
      IF ((RUNTYP.EQ.HESS.AND.NHLEVL.GT.0)  .OR.  NT.EQ.1) THEN
         GRPDET=FANT(1)
         KSTSYM=1
      END IF
C
C        the input for C2h is supposed to be identical to the GUGA
C        order, namely 1,2,3,4=ag,bu,bg,au, but the CI code wants
C        the order of  1,2,3,4=ag,bg,bu,au.  See also GAJASW routine.
C
      IF (GRPDET.EQ.FANT(7)) THEN
         MODI = KSTSYM
         IF(KSTSYM.EQ.2) MODI=3
         IF(KSTSYM.EQ.3) MODI=2
         KSTSYM=MODI
      END IF
      DO I=1,8
         IF (GRPDET.EQ.FANT(I)) THEN
            IGPDET=LFANT(I)
            STSYM = GANT(LGANT(I)+KSTSYM)
         ENDIF
      END DO
      IF (IGPDET.EQ.-1) THEN
         IF(MASWRK) WRITE(IW,*) '$DET POINT GROUP IS UNRECOGNIZED!'
         CALL ABRT
      ENDIF
      IF (GRPDET.EQ.FANT(1).AND.KSTSYM.GT.1) THEN
         IF(MASWRK) WRITE(IW,*) '$DET STATE SYMM IS NOT CORRECT IRREP'
         CALL ABRT
      ENDIF
      IF (KSTSYM.GT.(2**IGPDET)) THEN
         IF(MASWRK) WRITE(IW,*)
     *       '$DET STATE SYMMETRY IS TOO LARGE FOR THIS GROUP'
         CALL ABRT
      ENDIF
C
C Read MO symmetries and write to direct access file,
C but don't do if this if we are peeking at the $det input
C at the most early stages of an initial MCSCF run.
C
      IF (NPRINT.EQ.-23) GOTO 1314
C
      L0 = NQMT
      L1 = NUM
      L2 = (L1*L1+L1)/2
      L3 =  L1*L1
C
      CALL VALFM(LOADFM)
      LMOLAB = LOADFM + 1
      LMOIRP = LMOLAB + L1
      LVEC   = LMOIRP + L1
      LS     = LVEC   + L3
      LQ     = LS     + L2
      LWRK   = LQ     + L3
      LMODEG = LWRK   + L1
      LAST   = LMODEG + L1
      NEEDD   = LAST - LOADFM - 1
      CALL GETFM(NEEDD)
C
      IF(GRPDET.EQ.FANT(1)) THEN
         CALL C1DET(X(LMOIRP),X(LMOLAB),L0)
      ELSE
         CALL DAREAD(IDAF,IODA,X(LVEC),L3,15,0)
         CALL DAREAD(IDAF,IODA,X(LS),L2,12,0)
         CALL DAREAD(IDAF,IODA,X(LQ),L3,45,0)
         CALL TRFSYM(X(LMOLAB),X(LMOIRP),X(LMODEG),X(LQ),X(LS),X(LVEC),
     *               X(LWRK),IA,L0,L1,L0,L1)
      END IF
C
C  change orbital symmetry labels from GAMESS to JAKAL values
C
      CALL GAJASW(X(LMOIRP),NUM,GRPDET)
C
      CALL DAWRIT(IDAF,IODA,X(LMOIRP),L1,262,1)
C
C     1.  set NCORSV,NCOR,NACT,NORB,NA,NB for determinant specification
C         Check input, and copy into internally used variable names.
C         NCOR will be set to zero to drop cores, so NCORSV saves this.
C
 1314 CONTINUE
      IF(NPRINT.NE.-23  .AND.
     *       (NCORE.LT.0  .OR.  NACT.LE.0  .OR.  NELS.LE.0)) THEN
         IF(MASWRK) WRITE(IW,9020) GPNAME,NCORE,NACT,NELS
         CALL ABRT
      END IF
      NCORSV = NCORE
      NCOR   = NCORE
      NORB   = NCORE + NACT
      NHIGH = INT(SZ+SZ+0.0001D+00)
      NB = (NELS-NHIGH)/2
      NA = NB+NHIGH
      MA = NA+NCORSV
      MB = NB+NCORSV
      NELTOT = 2*NCOR+NA+NB
      NERR=0
      IF(NELTOT.NE.NE)  NERR=1
      IF(NELS.NE.NA+NB) NERR=1
      IF(NA.LT.NB)      NERR=1
      IF(NA.LE.0)       NERR=1
      IF(NB.LT.0)       NERR=1
      IF(NPRINT.NE.-23  .AND.  NERR.GT.0) THEN
         IF(MASWRK) WRITE(IW,9030) NCORE,NELS,SZ,ICH,MUL
         CALL ABRT
      END IF
      S = (MUL-1)/TWO
C
C        2. set K,KST,MAXW1,NITER,MAXP,CRIT,PRTTOL for diagonalization
C
      K     = NSTATE
      KST   = MAX(NSTGSS,K)
      MAXP  = MAX(MXXPAN,2*KST)
      MAXW1 = NHGSS
      NITER = ITERMX
      CRIT  = CVGTOL
      IF(NPRINT.NE.-23  .AND.  K.GT.MXRT) THEN
         IF(MASWRK) WRITE(IW,9035) K,MXRT
         CALL ABRT
      END IF
                 ICLBBR=0
      IF(CLOBBR) ICLBBR=1
C
C        3. setup for 1st order density computation
C
      IF(IROOT.GT.NSTATE) THEN
         IF(MASWRK) WRITE(IW,9036) IROOT,NSTATE
         CALL ABRT
      END IF
      IF(IROOT.GT.MXRT) THEN
         IF(MASWRK) WRITE(IW,9037) IROOT,MXRT
         CALL ABRT
      END IF
      IF(NFLGDM(IROOT).EQ.0) NFLGDM(IROOT)=1
C
C        4. setup for state-averaging 1st and 2nd order densities.
C        -IWTS- indexes the non-zero elements of -WSTATE-
C
      IPURES=0
      IF(PURES) IPURES=1
      MXSTAT=0
      WSUM = ZERO
      DO 15 I=1,MXRT
         IF(WSTATE(I).GT.ZERO) THEN
            IF(I.LE.NSTATE) THEN
               WSUM = WSUM + WSTATE(I)
               MXSTAT = MXSTAT+1
               IWTS(MXSTAT) = I
            ELSE
               IF(MASWRK) WRITE(IW,9040) NSTATE
               CALL ABRT
            END IF
         END IF
         IF(WSTATE(I).LT.ZERO) THEN
            IF(MASWRK) WRITE(IW,9050)
            CALL ABRT
         END IF
   15 CONTINUE
      SCALE = ONE/WSUM
      CALL DSCAL(MXRT,SCALE,WSTATE,1)
C
C        if running silently, return without printing anything
C
      IF(NPRINT.EQ.-23) RETURN
C
C        Replicated CI computation proceeds in order to support MCSCF.
C        Presently the only parallelization of the determinant FCI is
C        attention to I/O statements so that it will run correctly,
C        repeating the FCI on every node.
C
      IF(GOPARR  .AND.  SOME) WRITE(IW,9070)
C
C     THE COMPUTATION OF THE ENERGY GRADIENT REQUIRES A SYMMETRIC
C     LAGRANGIAN (SINCE GAMESS DOES NOT AT PRESENT DO THE CPHF
C     PROBLEM TO OBTAIN ORBITAL PERTURBATIONS).  ALTHOUGH A STATE
C     AVERAGED MCSCF RUN RESULTS IN A SYMMETRIC "AVERAGE LAGRANGIAN",
C     THE LAGRANGIAN FOR ANY SINGLE STATE USING THE AVERAGED ORBITALS
C     WILL *NOT* BE SYMMETRIC.  HENCE ANY JOB THAT ATTEMPTS TO BOTH
C     STATE AVERAGE AND DO A GRADIENT SHOULD BE FLUSHED.
C
      CALL DERCHK(MAXDER)
      IF(MXSTAT.GT.1  .AND.  MAXDER.GT.0) THEN
         IF(WTSOK) THEN
            IF (SOME) WRITE(IW,9065) RUNTYP,MXSTAT
         ELSE
            IF (SOME) WRITE(IW,9060) RUNTYP,MXSTAT
            CALL ABRT
         END IF
      END IF
C
      IF(SOME) THEN
         WRITE(IW,9100) GRPDET,STSYM,NCOR,NACT,
     *                  NA+NCOR,NA,NB+NCOR,NB,NORB
C
         WRITE(IW,9110) K,KST,MAXP,MAXW1,NITER,CRIT
         IF(GPNAME.EQ.CIDET) THEN
            WRITE(IW,9120) IROOT
            WRITE(IW,9130) (NFLGDM(II),II=1,K)
         END IF
         IF(GPNAME.EQ.DET) THEN
            WRITE(IW,9140) PURES
            WRITE(IW,9150) (IWTS(II),WSTATE(IWTS(II)),II=1,MXSTAT)
         END IF
      END IF
C
      IF (SOME) THEN
         WRITE(IW,9155) ANALYS
      ENDIF
C
      IF(SOME) THEN
         WRITE(IW,9160) NCOR,NACT
         CALL MOSYPR(X(LMOLAB),NCOR,NACT)
      ENDIF
C
      CALL RETFM(NEEDD)
      RETURN
C
C
 9000 FORMAT(/5X,50("-")/
     *       5X,'     AMES LABORATORY DETERMINANTAL FULL CI'/
     *       5X,'PROGRAM WRITTEN BY JOE IVANIC AND KLAUS RUEDENBERG'/
     *       5X,50(1H-))
 9010 FORMAT(/1X,'**** ERROR, THIS RUN REQUIRES INPUT OF A $',A8,
     *          ' GROUP')
 9020 FORMAT(/1X,'**** ERROR, THIS RUN DOES NOT CORRECTLY SPECIFY',
     *          ' THE FULL CI SPACE'/
     *     1X,'CHECK $',A8,' INPUT: NCORE=',I4,' NACT=',I4,' NELS=',I4)
 9030 FORMAT(/1X,'**** ERROR, $ALDET INPUT NCORE=',I4,' NELS=',I4,
     *          ' SZ=',F6.3/
     *       1X,' IS INCONSISTENT WITH $CONTRL INPUT ICH=',I4,
     *          ' MULT=',I4)
 9035 FORMAT(/1X,'***** ERROR, REQUESTED NUMBER OF CI ROOTS=',I5/
     *        1X,'EXCEEDS THE DIMENSION LIMIT FOR NUMBER OF STATES',I5)
 9036 FORMAT(/1X,'**** ERROR, YOUR STATE SELECTED FOR PROPERTIES=',I5/
     *        1X,'EXCEEDS THE NUMBER OF ROOTS YOU REQUESTED=',I5)
 9037 FORMAT(/1X,'**** ERROR, YOUR STATE SELECTED FOR PROPERTIES=',I5/
     *        1X,'EXCEEDS THE DIMENSION LIMIT FOR NUMBER OF STATES',I5)
 9040 FORMAT(/1X,'**** ERROR, WEIGHTS ASSIGNED TO STATES HIGHER',
     *          ' THAN NSTATE=',I5)
 9050 FORMAT(/1X,'**** ERROR, NEGATIVE VALUE FOR -WSTATE- ???')
 9060 FORMAT(/1X,'**** ERROR, RUNTYP=',A8,' REQUIRES ENERGY GRADIENT.'/
     *       1X,'THIS IS IMPOSSIBLE WHILE STATE AVERAGING. NAVG=',I5)
 9065 FORMAT(/1X,'**** WARNING ****'/
     *        1X,'RUNTYP=',A8,' INVOLVING A NUCLEAR GRADIENT'/
     *        1X,'HAS BEEN REQUESTED FOR A RUN AVERAGING OVER',I4,
     *           ' STATES.'/
     *        1X,'THIS MAKES SENSE ONLY IF THE STATES ARE DEGENERATE.'/
     *        1X,'THE RUN IS ALLOWED TO PROCEED BECAUSE USER INPUT'/
     *        1X,'REQUESTS THIS.  PLEASE CHECK THAT THE STATES ARE IN'/
     *        1X,'FACT DEGENERATE AFTER THEY ARE COMPUTED BELOW.'/)
 9070 FORMAT(/1X,'**** CAUTION: DETERMINANT CI PROGRAM DOES NOT YET',
     *           ' RUN IN PARALLEL.'/
     *       1X,'A REDUNDANT DETERMINANT CI COMPUTATION WILL BE PERF',
     *          'ORMED BY ALL NODES.'/
     *       1X,'THIS MAY INHIBIT SCALABILITY.')
C
 9100 FORMAT(/1X,'THE POINT GROUP                  =',3X,A8/
     *       1X,'THE STATE SYMMETRY               =',3X,A8/
     *       1X,'NUMBER OF CORE ORBITALS          =',I5/
     *       1X,'NUMBER OF ACTIVE ORBITALS        =',I5/
     *       1X,'NUMBER OF ALPHA ELECTRONS        =',I5,
     *          ' (',I4,' ACTIVE)'/
     *       1X,'NUMBER OF BETA ELECTRONS         =',I5,
     *          ' (',I4,' ACTIVE)'/
     *       1X,'NUMBER OF OCCUPIED ORBITALS      =',I5)
 9110 FORMAT(1X,'NUMBER OF CI STATES REQUESTED    =',I5/
     *       1X,'NUMBER OF CI STARTING VECTORS    =',I5/
     *       1X,'MAX. NO. OF CI EXPANSION VECTORS =',I5/
     *       1X,'SIZE OF INITIAL CI GUESS MATRIX  =',I5/
     *       1X,'MAX. NO. OF CI ITERS/STATE       =',I5/
     *       1X,'CI DIAGONALIZATION CRITERION     =',1P,E9.2)
C
 9120 FORMAT(1X,'CI PROPERTIES WILL BE FOUND FOR ROOT NUMBER',I4)
 9130 FORMAT(1X,'1E- DENSITY MATRIX OPTIONS ARE',20I2)
 9140 FORMAT(1X,'PURE SPIN STATE AVERAGED 1E- AND 2E- DENSITY MATRIX',
     *          ' OPTION=.',L1,'.')
 9150 FORMAT(2(1X,'STATE=',I4,' DM2 WEIGHT=',F10.5,4X,:))
 9155 FORMAT(/1X,'CORRELATION ENERGY ANALYSIS      =',L5)
 9160 FORMAT(/1X,'SYMMETRIES FOR THE',I4,' CORE,',I4,' ACTIVE ARE')
      END
C*MODULE ALDECI  *DECK NUMCSF
      INTEGER FUNCTION NUMCSF(N,M,S)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C        Use Robinson-Weyl formula to evaluate the number of
C        CSFs with N electrons in M orbitals and total spin S.
C        The formula is correct only for the case of no symmetry.
C
      N2MS = INT(N/2.0D+00 - S + 0.0001D+00)
      N2PS = INT(N/2.0D+00 + S + 0.0001D+00)
      MULT = INT(S + S + 1.0D+00 + 0.0001D+01)
      IF((M-N2PS).LT.0  .OR.  N2MS.LT.0  .OR.  (S+S).GT.N) THEN
         NUMCSF=0
         RETURN
      END IF
      K1 = ICOMB(M+1,M-N2PS)
      K2 = ICOMB(M+1,  N2MS)
C
      NUMCSF= (MULT * K1 * K2)/(M+1)
      RETURN
      END
C*MODULE ALDECI  *DECK ICOMB
      INTEGER FUNCTION ICOMB(I,J)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C        evaluate the combinatorial i on j.
C
      COMB = 1.0D+00
      K = I-J
      DO 100 II=MAX(J,K)+1,I
         RI = II
         COMB = COMB*RI
  100 CONTINUE
      DO 200 KK=1,MIN(J,K)
         RK=KK
         COMB = COMB/RK
  200 CONTINUE
C
      ICOMB = INT(COMB + 0.01D+00)
      RETURN
      END
C*MODULE ALDECI  *DECK DETFCI
      SUBROUTINE DETFCI(NPRINT,CLABEL,DDITRF)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME,PACK2E,GOPARR,DSKWRK,MASWRK,CLABEL,JACOBI,DDITRF
      LOGICAL ANALYS
C
      PARAMETER (MXRT=100)
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /DETPAR/ ICLBBR,ANALYS
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,S,SZ,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORB,
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NCI,
     *                IGPDET,KSTSYM,NFTGCI
      COMMON /ENRGYS/ ENUCR,EELCT,ETOT,STOT,SSQUAR,ECORE,ESCF,EERD,
     *                E1,E2,VEN,VEE,EPOT,EKIN,ESTATE(MXRT),STATN
      COMMON /FMCOM / X(1)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /IOFILE/ IR,IW,IP,IS,IJKT,IDAF,NAV,IODA(400)
      COMMON /JACOBI/ JACOBI,NJAOR,ELAST,ISTAT
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA AMCSCF/8HMCSCF   /
#else
      CHARACTER*8 :: AMCSCF_STR
      EQUIVALENCE (AMCSCF, AMCSCF_STR)
      DATA AMCSCF_STR/"MCSCF   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA C1/8HC1      /
#else
      CHARACTER*8 :: C1_STR
      EQUIVALENCE (C1, C1_STR)
      DATA C1_STR/"C1      "/
#endif
C
C     ----- driver for Full CI computation -----
C
      SOME = MASWRK  .AND.  NPRINT.NE.-5
C
C        core contribution to the energy is obtained from -ecore-,
C        and from modifications to the transformed 1e- integrals.
C        this effectively removes core orbitals from the computation.
C
      ECONST = ECORE + ENUCR
      NTOT = NACT + NCORSV
      NTCO = NCORSV
      NORB = NACT
      NCOR = 0
      NSYM = 2**IGPDET
C
C
C        Compute the total number of determinants in this full CI.
C        Decide necessary double/integer working storage -IDS- and -IIS-
C
      CALL VALFM(LOADFM)
      LIFA  = LOADFM + 1
      LAST  = LIFA   + ((NACT+1)*(NACT+1))/NWDVAR + 1
      NEED1 = LAST - LOADFM - 1
      CALL GETFM(NEED1)
C
      CALL BINOM6(X(LIFA),NACT)
C
      CALL VALFM(LOADFM)
      IBO = LOADFM + 1
      ICON = IBO + NTOT
      ICA  = ICON + NA
      ICB  = ICA + NSYM
      KTAB = ICB + NSYM
      IWRK = KTAB + NSYM
      LAST = IWRK + 43
      NEEDT = LAST - LOADFM - 1
      CALL GETFM(NEEDT)
      CALL DAREAD(IDAF,IODA,X(IBO),NTOT,262,1)
      CALL CORTRA(X(IBO),NTOT,NTCO)
      CALL MEMCI(IW,NORB,NCOR,NA,NB,K,MAXP,MAXW1,X(LIFA),
     *           NCI,IDS,IIS,NALP,NBLP,IGPDET,KSTSYM,NSYM,
     *           X(IBO),ISST,X(ICON),X(ICA),X(ICB),X(KTAB),X(IWRK))
      CALL RETFM(NEEDT)
C
      IF(SOME) THEN
         WRITE(IW,9000)
         WRITE(IW,9110) STSYM,GRPDET,SZ,NCI
         IF(GRPDET.NE.C1) GO TO 6
         SVAL = SZ
    5    CONTINUE
            NOCSF = NUMCSF(NA+NB,NACT,SVAL)
            IF(NOCSF.LE.0) GO TO 6
            WRITE(IW,9120) NOCSF,SVAL
            SVAL = SVAL + 1.0D+00
         GO TO 5
      END IF
    6 CONTINUE
C
      M1 = NACT
      M2 = (M1*M1+M1)/2
      M4 = (M2*M2+M2)/2
C
C        integral buffers for distributed/disk file transformed ints
C
      IF(DDITRF) THEN
        NOCC  = NACT + NCORSV
        NOTR  = (NOCC*NOCC+NOCC)/2
        LENXX = NOTR
        LENIXX= 0
      ELSE
        LENXX = NINTMX
        LENIXX= NINTMX
      END IF
C
C        allocate memory for the CI step
C
      CALL VALFM(LOADFM)
      LDWRK  = LOADFM + 1
      LIWRK  = LDWRK  + IDS
      LSINT1 = LIWRK  + IIS/NWDVAR + 1
      LSINT2 = LSINT1 + M2
      LIA    = LSINT2 + M4
      LXX    = LIA    + M2/NWDVAR + 1
      LIXX   = LXX    + LENXX
      LEL    = LIXX   + LENIXX
      LSP    = LEL    + MAXW1
      IBO    = LSP    + MAXW1
      LAST   = IBO    + NTOT
      NEED2  = LAST - LOADFM - 1
      NEEDCI = NEED1 + NEED2
      IF(SOME) WRITE(IW,9130) NEEDCI
      CALL GETFM(NEED2)
C
      CALL DAREAD(IDAF,IODA,X(IBO),NTOT,262,1)
      CALL CORTRA(X(IBO),NTOT,NTCO)
C
      IF(EXETYP.EQ.CHECK) THEN
         DO IST=1,MIN(K,MXRT)
            SPINS(IST) = S
            ESTATE(IST) = ZERO
         ENDDO
         LCIVEC = LDWRK
         CALL VCLR(X(LCIVEC),1,K*NCI)
         GO TO 450
      END IF
C
C     -- obtain 1 and 2 e- transformed integrals over active orbitals --
C     calling argument -CLABEL- governs whether transformed integrals
C     on file -IJKT- include the core orbitals or not.  It is assumed
C     that no core integrals are in -IJKT-, so this variable tells if
C     the active orbitals start from 1,2,3... or NCORSV+1,NCORSV+2,...
C
      NCORE = 0
      IF(CLABEL) NCORE=NCORSV
      CALL RDCI12(DDITRF,IJKT,X(LSINT1),X(LSINT2),NCORE,M1,M2,M4,
     *            X(LIA),X(LXX),X(LIXX),NINTMX)
C
C        ----- compute full ci wavefunction -----
C
      CALL DETCI(IW,SOME,ECONST,
     *           X(LSINT1),X(LSINT2),M2,M4,NORB,NCOR,NCI,NA,NB,
     *           K,KST,MAXP,MAXW1,NITER,CRIT,
     *           X(LIFA),X(LSP),X(LEL),X(LDWRK),IDS,X(LIWRK),IIS,
     *           IGPDET,KSTSYM,NSYM,X(IBO),NALP,NBLP,ISTAT)
C
      DO I=1,MIN(K,MXRT)
         ESTATE(I) = X(LEL-1+I) + ECONST
         SPINS(I)  = X(LSP-1+I)
      ENDDO
C
C        save energy quantities
C
  450 CONTINUE
      ETOT  = ESTATE(IROOT)
      EELCT = ETOT - ENUCR
      STOT  = SPINS(IROOT)
      SSQUAR= STOT*(STOT+ONE)
      STATN = K
C
C        save eigenvectors to disk, on master node only
C
      IF (.NOT.ANALYS) THEN
         CALL SEQOPN(NFT12,'CIVECTR','UNKNOWN',.FALSE.,'UNFORMATTED')
         CALL SEQREW(NFT12)
         IF(MASWRK) WRITE(NFT12) K,NCI
         DO 420 IST=1,K
            LCIVEC = LDWRK + (IST-1)*NCI
            CALL STFASE(X(LCIVEC),NCI,NCI,1)
            CALL SQWRIT(NFT12,X(LCIVEC),NCI)
  420    CONTINUE
         CALL SEQREW(NFT12)
      ENDIF
C
      CALL RETFM(NEED2)
      CALL RETFM(NEED1)
C
C  print results of the CI calculation
C
      IF (.NOT.ANALYS) CALL DETPRT(IW,NFT12,SOME)
C
      IF(SOME) WRITE(IW,9140)
      IF(SOME) CALL TIMIT(1)
      IF(EXETYP.NE.CHECK  .AND.  ISTAT.NE.0 .AND. SCFTYP.NE.AMCSCF) THEN
         IF(MASWRK) WRITE(IW,9150)
         CALL ABRT
      END IF
      RETURN
C
 9000 FORMAT(/5X,50("-")/
     *       5X,'     AMES LABORATORY DETERMINANTAL FULL CI'/
     *       5X,'PROGRAM WRITTEN BY JOE IVANIC AND KLAUS RUEDENBERG'/
     *       5X,50(1H-))
 9110 FORMAT(/1X,'THE NUMBER OF DETERMINANTS HAVING SPACE SYMMETRY ',A3/
     *        1X,'IN POINT GROUP ',A4,' WITH SZ=',F5.1,' IS',I15)
 9120 FORMAT(1X,'WHICH INCLUDES',I15,' CSFS WITH S=',F5.1)
 9130 FORMAT(1X,'THE DETERMINANT FULL CI REQUIRES',I12,' WORDS')
 9140 FORMAT(1X,'..... DONE WITH DETERMINANT CI COMPUTATION .....')
 9150 FORMAT(1X,'CI COMPUTATION DID NOT CONVERGE, JOB CANNOT CONTINUE')
      END
C*MODULE ALDECI  *DECK RDCI12
      SUBROUTINE RDCI12(DDITRF,NFT,X1,X2,NCORE,M1,M2,M4,IA,XX,IX,NINTMX)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,SVDSKW,DDITRF
C          non-dditrf needx xx(nintmx), and ix(nintmx d.p.)
C              dditrf needs xx(m2), and no ix array
      DIMENSION X1(M2),X2(M4),IA(M2),XX(*),IX(*)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCKLAB/ LABSIZ
C
C  DDI ARRAY HANDLES
C
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U,
     *                NDOOOO, NDVOOO, NDVVOO, NDVOVO
C
C     -- read 1 and 2 e- transformed integrals into replicated memory --
C     Only integrals in the active space, between NCORE and NCORE+M1
C     are returned in X1 and X2 arrays.  The 2e- integrals might be
C     in distributed memory, depending on DDITRF flag.
C
      IROW = 0
      DO 110 I=1,M2
         IA(I) = IROW
         IROW = IROW+I
  110 CONTINUE
C
      CALL VCLR(X2,1,M4)
C
C         integrals are to be obtained from distributed memory
C
      IF (DDITRF) THEN
C
C         obtain the one electron integrals, always read from disk.
C
         SVDSKW=DSKWRK
         DSKWRK=.FALSE.
         CALL SEQREW(NFT)
         CALL SQREAD(NFT,X1,M2)
         CALL SEQREW(NFT)
C
C         obtain the two electron integrals from distributed memory.
C
         CALL DDI_DISTRIB(D_OOOO,ME,ILO,IHI,JLO,JHI)
         NACT = M1
         NOCC = NACT + NCORE
         NOTR = (NOCC*NOCC+NOCC)/2
         DO I = 1, NACT
           IN = I + NCORE
           DO J = 1, I
             JN = J + NCORE
             IJ = (I*I-I)/2 + J
             IJN = (IN*IN-IN)/2 + JN
             IF ((IJN.GE.JLO).AND.(IJN.LE.JHI)) THEN
               CALL DDI_GET(D_OOOO,1,NOTR,IJN,IJN,XX)
               DO K = 1, NACT
                 KN = K + NCORE
                 DO L = 1, K
                  LN = L + NCORE
                   KL = (K*K-K)/2 + L
                   IF (IJ.GE.KL) THEN
                     KLN = (KN*KN-KN)/2 + LN
                     IJKL = (IJ*IJ-IJ)/2 + KL
                     X2(IJKL) = XX(KLN)
                   END IF
                 END DO
               END DO
             END IF ! LOCAL STRIPE
           END DO ! J
         END DO ! I
C
      ELSE
C
C         obtain the one electron integrals, always read from disk.
C         only the master has the 1e- integrals, but if the 2e-
C         integrals are on disk, all nodes must process them.
C
         SVDSKW=DSKWRK
         DSKWRK=.TRUE.
         CALL SEQREW(NFT)
         IF(MASWRK) CALL SQREAD(NFT,X1,M2)
         IF(GOPARR) CALL DDI_BCAST(2511,'F',X1,M2,MASTER)
C
C         Read transformed 2e- integral file in reverse canonical order.
C
  200    CONTINUE
         CALL PREAD(NFT,XX,IX,NX,NINTMX)
         IF (NX.EQ.0) GO TO 240
         MX = ABS(NX)
         IF (MX.GT.NINTMX) THEN
            IF(MASWRK) WRITE(IW,*) 'INTEGRAL CONFUSION IN -RDCI12-'
            CALL ABRT
         END IF
         DO 220 M = 1,MX
            VAL = XX(M)
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
C
C        note index reversals to convert from reverse canonical order
C
            K = IPACK - NCORE
            L = JPACK - NCORE
            I = KPACK - NCORE
            J = LPACK - NCORE
C
            IF(I.LE.0  .OR.  I.GT.M1) GO TO 220
            IF(J.LE.0  .OR.  J.GT.M1) GO TO 220
            IF(K.LE.0  .OR.  J.GT.M1) GO TO 220
            IF(L.LE.0  .OR.  L.GT.M1) GO TO 220
C
            IJ = IA(I)+J
            KL = IA(K)+L
            IJKL = IA(IJ) + KL
            X2(IJKL) = VAL
  220    CONTINUE
         IF(NX.GT.0) GO TO 200
C
  240    CONTINUE
         CALL SEQREW(NFT)
         DSKWRK=SVDSKW
      END IF
C
C  GLOBAL SUM ALSO ACTS AS A SYNC
C
      CALL DDI_GSUMF(2500,X2,M4)
      RETURN
      END
C*MODULE ALDECI  *DECK DETPRT
      SUBROUTINE DETPRT(IW,NFT12,SOME)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME,GOPARR,DSKWRK,MASWRK,SVDSKW
C
      PARAMETER (MXRT=100)
C
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,S,SZ,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORB,
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NCI,
     *                IGPDET,KSTSYM,NFTGCI
      COMMON /ENRGYS/ ENUCR,EELCT,ETOT,STOT,SSQUAR,ECORE,ESCF,EERD,
     *                E1,E2,VEN,VEE,EPOT,EKIN,ESTATE(MXRT),STATN
      COMMON /FMCOM / X(1)
      COMMON /IOFILE/ IR,IZ,IP,IS,IPK,IDAF,NAV,IODA(400)
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
C
C     ----- print the determinant based CI eigenvector -----
C
      SVDSKW = DSKWRK
      DSKWRK=.FALSE.
C
      M1 = NACT
      NTOT = NACT + NCORSV
      NTCO = NCORSV
      NSYM = 2**IGPDET
C
      CALL VALFM(LOADFM)
      LCIVEC = LOADFM + 1
C
      LIFA   = LCIVEC + NCI
      LIBO   = LIFA + (M1+1)*(M1+1)
      LAST   = LIBO + NTOT
      NEED = LAST - LOADFM - 1
      CALL GETFM(NEED)
C
      IF(SOME) WRITE(IW,9140) GRPDET
C
C        ----- print ci energies and eigenvectors -----
C        note that PRTDET destroys the eigenvectors.
C
      IF(NCI.LE.20) THEN
         IOP=1
         NUMPRT=NCI
         IF(SOME) WRITE(IW,9150)
      ELSE
         IOP=2
         NUMPRT=0
         IF(SOME) WRITE(IW,9160) PRTTOL
      END IF
C
      CALL BINOM6(X(LIFA),M1)
      CALL DAREAD(IDAF,IODA,X(LIBO),NTOT,262,1)
      CALL CORTRA(X(LIBO),NTOT,NTCO)
      CALL MEMPRI(X(LIFA),NA,NB,NACT,NSYM,IPRMEM)
C
      CALL VALFM(LOADFM)
      LIWRK = LOADFM + 1
      LAST  = LIWRK  + IPRMEM
      NEED1 = LAST - LOADFM - 1
      CALL GETFM(NEED1)
C
      CALL SEQREW(NFT12)
      IF(MASWRK) READ(NFT12) NSTATS,NDETS
      IF (GOPARR) CALL DDI_BCAST(2501,'I',NSTATS,1,MASTER)
      IF (GOPARR) CALL DDI_BCAST(2502,'I',NDETS ,1,MASTER)
C
      IF(NSTATS.NE.K  .OR.  NDETS.NE.NCI) THEN
         IF(MASWRK) WRITE(IW,9180) NSTATS,NDETS,K,NCI
         CALL ABRT
      END IF
C
      DO 430 I=1,K
         CALL SQREAD(NFT12,X(LCIVEC),NCI)
         IF(SOME) THEN
            WRITE(IW,9170) I,ESTATE(I),SPINS(I),SZ,STSYM
            IF(EXETYP.NE.CHECK) CALL PRICI1(IW,X(LIFA),NA,NB,0,M1,
     *                      X(LCIVEC),NCI,X(LIBO),
     *        IOP,PRTTOL,NUMPRT,IGPDET,KSTSYM,NSYM,X(LIWRK),IPRMEM)
C
         END IF
  430 CONTINUE
C
      CALL SEQREW(NFT12)
      DSKWRK=SVDSKW
      CALL RETFM(NEED1)
      CALL RETFM(NEED)
      RETURN
C
 9140 FORMAT(/1X,'CI EIGENVECTORS WILL BE LABELED IN GROUP=',A8)
 9150 FORMAT(1X,'PRINTING ALL NON-ZERO CI COEFFICIENTS')
 9160 FORMAT(1X,'PRINTING CI COEFFICIENTS LARGER THAN',F10.6)
 9170 FORMAT(/1X,'STATE',I4,'  ENERGY= ',F20.10,'  S=',F6.2,
     *           '  SZ=',F6.2,:,'  SPACE SYM=',A4/)
 9180 FORMAT(/1X,'***** ERROR IN -DETPRT- ROUTINE *****'/
     *       1X,'CI EIGENVECTOR FILE HAS DATA FOR NSTATE,NDETS=',I3,I10/
     *       1X,'BUT THE PRESENT CALCULATION HAS NSTATE,NDETS=',I3,I10)
      END
C
C*MODULE ALDECI  *DECK C1DET
      SUBROUTINE C1DET(MOIRP,LMOLAB,L0)
      DIMENSION MOIRP(L0),LMOLAB(L0)
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA LETA/4HA   /
#else
      CHARACTER*4 :: LETA_STR
      EQUIVALENCE (LETA, LETA_STR)
      DATA LETA_STR/"A   "/
#endif
C
C         force orbital symmetry assignment to C1 point group
C
      DO I=1,L0
         MOIRP(I) = 1
         LMOLAB(I) = LETA
      END DO
      RETURN
      END
C*MODULE ALDECI  *DECK DETGRP
      SUBROUTINE DETGRP(GRPDET,LABMO,LBABEL,PTGRP,LBIRRP,SYIRRP,
     *                  NSYM,NIRRP,L1,NACT,NCORSV)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,ABEL
C
      PARAMETER (MXSH=1000, MXGRPS=13, MXATM=500)
C
      DIMENSION LABMO(L1),LBABEL(NACT),LBIRRP(12)
      INTEGER   SYIRRP(12)
      DIMENSION GROUPS(MXGRPS),NSYMS(MXGRPS),NIRRPS(MXGRPS),
     *          ISYMRP(12,MXGRPS)
      INTEGER   SYMREP(12,MXGRPS),SYMB
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /SYMBLK/ NIRRED,NSALC,NSALC2,NSALC3
      COMMON /SYMMOL/ GROUP,COMPLEX,IGROUP,NAXIS,ILABMO,ABEL
      COMMON /SYMQMT/ IRPLAB(14),IRPNUM(14),IRPDIM(14),IRPDEG(14)
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                T(432),INVT(48),NT
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA GROUPS/8HC1      ,8HCI      ,8HCS      ,8HC2      ,
     *            8HC2V     ,8HC2H     ,8HD2      ,8HD2H     ,
     *            8HCINFV   ,8HDINFH   ,8HD4H     ,8HD4      ,
     *            8HC4V     /
#else
      CHARACTER*8 :: GROUPS_STR(MXGRPS)
      EQUIVALENCE (GROUPS, GROUPS_STR)
      DATA GROUPS_STR/"C1      ","CI      ","CS      ","C2      ",
     *            "C2V     ","C2H     ","D2      ","D2H     ",
     *            "CINFV   ","DINFH   ","D4H     ","D4      ",
     *            "C4V     "/
#endif
      DATA NIRRPS/1,2,2,2,4,4,4,8,5,10,12,6,6/
      DATA NSYMS /1,2,2,2,4,4,4,8,4, 8, 8,4,4/
C                        C1
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA  SYMREP(1,1)       /4HA   /
      DATA  ISYMRP(1,1)       /1/
C                        CI
      DATA (SYMREP(I,2),I=1,2)/4HAG  ,4HAU  /
      DATA (ISYMRP(I,2),I=1,2)/1,2/
C                        CS
      DATA (SYMREP(I,3),I=1,2)/4HA'  ,4HA'' /
      DATA (ISYMRP(I,3),I=1,2)/1,2/
C                        C2
      DATA (SYMREP(I,4),I=1,2)/4HA   ,4HB   /
      DATA (ISYMRP(I,4),I=1,2)/1,2/
C                        C2V
      DATA (SYMREP(I,5),I=1,4)/4HA1  ,4HA2  ,4HB1  ,4HB2  /
      DATA (ISYMRP(I,5),I=1,4)/1,2,3,4/
C                        C2H
      DATA (SYMREP(I,6),I=1,4)/4HAG  ,4HBU  ,4HBG  ,4HAU  /
      DATA (ISYMRP(I,6),I=1,4)/1,2,3,4/
C                        D2
      DATA (SYMREP(I,7),I=1,4)/4HA   ,4HB1  ,4HB2  ,4HB3  /
      DATA (ISYMRP(I,7),I=1,4)/1,2,3,4/
C                        D2H
      DATA (SYMREP(I,8),I=1,8)/4HAG  ,4HB1G ,4HB2G ,4HB3G ,
     *                         4HAU  ,4HB1U ,4HB2U ,4HB3U /
#else
      CHARACTER*4 :: SYMREP_STR(12, MXGRPS)
      EQUIVALENCE (SYMREP, SYMREP_STR)
      DATA  SYMREP_STR(1,1)       /"A   "/
      DATA  ISYMRP(1,1)       /1/
C                        CI
      DATA (SYMREP_STR(I,2),I=1,2)/"AG  ","AU  "/
      DATA (ISYMRP(I,2),I=1,2)/1,2/
C                        CS
      DATA (SYMREP_STR(I,3),I=1,2)/"A'  ","A'' "/
      DATA (ISYMRP(I,3),I=1,2)/1,2/
C                        C2
      DATA (SYMREP_STR(I,4),I=1,2)/"A   ","B   "/
      DATA (ISYMRP(I,4),I=1,2)/1,2/
C                        C2V
      DATA (SYMREP_STR(I,5),I=1,4)/"A1  ","A2  ","B1  ","B2  "/
      DATA (ISYMRP(I,5),I=1,4)/1,2,3,4/
C                        C2H
      DATA (SYMREP_STR(I,6),I=1,4)/"AG  ","BU  ","BG  ","AU  "/
      DATA (ISYMRP(I,6),I=1,4)/1,2,3,4/
C                        D2
      DATA (SYMREP_STR(I,7),I=1,4)/"A   ","B1  ","B2  ","B3  "/
      DATA (ISYMRP(I,7),I=1,4)/1,2,3,4/
C                        D2H
      DATA (SYMREP_STR(I,8),I=1,8)/"AG  ","B1G ","B2G ","B3G ",
     *                         "AU  ","B1U ","B2U ","B3U "/
#endif
      DATA (ISYMRP(I,8),I=1,8)/1,2,3,4,5,6,7,8/
C
C           NOTE THAT CINFV AND DINFH DON'T WORK NOW
C
C                        CINFV
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA (SYMREP(I,9),I=1,5)/4HSIG ,4HPIX ,4HPIY ,4HDELX,4HDELY/
#else
      DATA (SYMREP_STR(I,9),I=1,5)/"SIG ","PIX ","PIY ","DELX","DELY"/
#endif
      DATA (ISYMRP(I,9),I=1,5)/1,3,4,1,2/
C                        DINFH
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA (SYMREP(I,10),I=1,10)/4HSIGG,4HSIGU,4HPIUX,4HPIUY,4HPIGX,
     *                           4HPIGY,4H DGX,4H DGY,4H DUX,4H DUY/
#else
      DATA (SYMREP_STR(I,10),I=1,10)/"SIGG","SIGU","PIUX","PIUY","PIGX",
     *                           "PIGY"," DGX"," DGY"," DUX"," DUY"/
#endif
      DATA (ISYMRP(I,10),I=1,10)/1,6,8,7,3,4,1,2,6,5/
C                        D4H
C         MICHEL'S SYMMETRY CODE GENERATE EGY BEFORE EGX
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA (SYMREP(I,11),I=1,12)/4HA1G ,4HA2G ,4HB1G ,4HB2G ,
     *                           4HA1U ,4HA2U ,4HB1U ,4HB2U ,
     *                           4HEG  ,4HEG  ,4HEU  ,4HEU  /
#else
      DATA (SYMREP_STR(I,11),I=1,12)/"A1G ","A2G ","B1G ","B2G ",
     *                           "A1U ","A2U ","B1U ","B2U ",
     *                           "EG  ","EG  ","EU  ","EU  "/
#endif
      DATA (ISYMRP(I,11),I=1,12)/1,2,1,2, 5,6,5,6, 4,3,8,7/
C                        D4
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA (SYMREP(I,12),I=1,6) /4HA1  ,4HA2  ,4HB1  ,4HB2  ,
     *                           4HE   ,4HE   /
#else
      DATA (SYMREP_STR(I,12),I=1,6) /"A1  ","A2  ","B1  ","B2  ",
     *                           "E   ","E   "/
#endif
      DATA (ISYMRP(I,12),I=1,6) /1,2,1,2, 4,3/
C                        C4V
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA (SYMREP(I,13),I=1,6) /4HA1  ,4HA2  ,4HB1  ,4HB2  ,
     *                           4HE   ,4HE   /
#else
      DATA (SYMREP_STR(I,13),I=1,6) /"A1  ","A2  ","B1  ","B2  ",
     *                           "E   ","E   "/
#endif
      DATA (ISYMRP(I,13),I=1,6) /1,2,1,2, 3,4/
C
C     Assign the orbitals a symmetry label -LBABEL- under
C     the highest possible Abelian subgroup.  Only a
C     handful of the non-Abelian groups will downshift.
C
C     -IGROUP- IS A POINTER INTO THE FOLLOWING TABLE, from $data:
C     DATA GRP /C1   ,CS   ,CI   ,CN   ,S2N  ,CNH  ,
C    *          CNV  ,DN   ,DNH  ,DND  ,CINFV,DINFH,T
C    *          TH   ,TD   ,O    ,OH   ,I    ,IH   /
C     -igrp- is a pointer into -GROUPS- table, local to this routine:
C
      IGRP=1
      IF(IGROUP.EQ.3) IGRP=2
      IF(IGROUP.EQ.2) IGRP=3
      IF(IGROUP.EQ.4  .AND.  NAXIS.EQ.2) IGRP=4
      IF(IGROUP.EQ.7  .AND.  NAXIS.EQ.2) IGRP=5
      IF(IGROUP.EQ.6  .AND.  NAXIS.EQ.2) IGRP=6
      IF(IGROUP.EQ.8  .AND.  NAXIS.EQ.2) IGRP=7
      IF(IGROUP.EQ.9  .AND.  NAXIS.EQ.2) IGRP=8
      IF(IGROUP.EQ.6  .AND.  NAXIS.EQ.4) IGRP=11
      IF(IGROUP.EQ.8  .AND.  NAXIS.EQ.4) IGRP=12
      IF(IGROUP.EQ.7  .AND.  NAXIS.EQ.4) IGRP=13
      IF(NT.EQ.1  .OR.  ILABMO.EQ.0) IGRP=1
      IF(GRPDET.EQ.GROUPS(1)) IGRP=1
C
      PTGRP= GROUPS(IGRP)
      NSYM = NSYMS(IGRP)
      NIRRP= NIRRPS(IGRP)
      DO 100 I=1,NIRRP
         LBIRRP(I) = ISYMRP(I,IGRP)
         SYIRRP(I) = SYMREP(I,IGRP)
  100 CONTINUE
C
C        all orbitals are the same symmetry in C1, or unsupported group.
C
      IF(IGRP.EQ.1) THEN
         DO 200 I=1,NACT
            LBABEL(I) = 1
  200    CONTINUE
         RETURN
      END IF
C
C        obtain active orbital symmetry labels in point group of $data
C
      CALL DAREAD(IDAF,IODA,LABMO,L1,255,1)
C
C     ----- map these labels onto the highest Abelian subgroup -----
C
      NERR=0
      NERR2=0
      IPART=0
      IRREP=0
C
      DO 360 K=1,NACT
         SYMB=LABMO(K+NCORSV)
         DO 310 IRP=1,NIRRED
            IF(SYMB.EQ.IRPLAB(IRP)) THEN
               IRREP = IRP
               GO TO 320
            END IF
  310    CONTINUE
C
         NERR=NERR+1
         LBABEL(K)=0
         IF(MASWRK) WRITE(IW,9050) NCORSV+K,SYMB
         GO TO 360
C
  320    CONTINUE
         IDIM = IRPDIM(IRREP)
         IF(IDIM.GT.1) THEN
            IPART=IPART+1
            IOFF =IPART-1
         ELSE
            IOFF=0
         END IF
         DO 340 I=1,NIRRP
            IF(SYMB.EQ.SYMREP(I,IGRP)) THEN
               LBABEL(K)=ISYMRP(I+IOFF,IGRP)
               IF(IPART.EQ.IDIM) IPART=0
               GO TO 360
            END IF
  340    CONTINUE
C
         NERR2=NERR2+1
         IF(MASWRK) WRITE(IW,9055) K,SYMB,GROUPS(IGRP)
         LBABEL(K)=0
C
  360 CONTINUE
C
      IF(NERR2.GT.0) THEN
         IF(MASWRK) WRITE(IW,*) 'CONFUSION WITH GROUPS IN -DETGRP-'
         CALL ABRT
      END IF
C
      RETURN
C
 9050 FORMAT(1X,'MO=',I5,' HAS ILLEGAL SYMMETRY LABEL ',A4)
 9055 FORMAT(1X,'MO=',I5,' HAS A SYMMETRY LABEL ',A4,
     *          ' UNKNOWN IN GROUP ',A8)
      END
C*MODULE ALDECI  *DECK DETVAL
      SUBROUTINE DETVAL(ICI,NA,NB,NACT,IACON,IBCON,IACONF,IBCONF,IFA)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION IACON(NA),IBCON(NA),IACONF(NACT),IBCONF(NACT),
     *          IFA(0:NACT,0:NACT)
C
C     Obtain the orbital occupation of determinant -ICI-, within a
C     full CI defined by -NA- plus -NB- electrons in -NACT- orbitals.
C     The configuration is returned in -IACONF- and -IBCONF-.
C     Before calling, set the binomial coefficient array -IFA-.
C     Working storage: IACON, IBCON
C
      NBLP = IFA(NACT,NB)
C
C   determine alpha and beta substring positions
C
      INDA = (ICI-1)/NBLP + 1
      INDB = MOD(ICI,NBLP)
      IF(INDB.EQ.0) INDB=NBLP
C
C   Now to obtain the determinant
C
      DO 320 II=1,NA
         IACON(II) = II
         IBCON(II) = 0
  320 CONTINUE
      DO 330 II=1,NB
         IBCON(II) = II
  330 CONTINUE
C
      DO 340 II=1,INDA-1
         CALL ADVANC(IACON,NA,NACT)
  340 CONTINUE
      DO 350 II=1,INDB-1
         CALL ADVANC(IBCON,NB,NACT)
  350 CONTINUE
C
      DO 410 I=1,NACT
         IACONF(I) = 0
         IBCONF(I) = 0
  410 CONTINUE
      DO 420 I=1,NA
         IACONF(IACON(I)) = 1
  420 CONTINUE
      DO 430 I=1,NB
         IBCONF(IBCON(I)) = 1
  430 CONTINUE
      RETURN
      END
C*MODULE ALDECI  *DECK DETSYM
      SUBROUTINE DETSYM(SYMB,PTGRP,LBIRRP,SYIRRP,NIRRP,LBABEL,
     *                  IACONF,IBCONF,NACT,IW,SOME)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION LBABEL(NACT),IACONF(NACT),IBCONF(NACT)
      INTEGER SYMB,BLANK,LBIRRP(NIRRP),SYIRRP(NIRRP)
      LOGICAL SOME
      DIMENSION MULT8(8),LKUPSM(64)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA BLANK/4H    /
#else
      CHARACTER*4 :: BLANK_STR
      EQUIVALENCE (BLANK, BLANK_STR)
      DATA BLANK_STR/"    "/
#endif
      DATA MULT8/0,8,16,24,32,40,48,56/
      DATA LKUPSM/1,2,3,4,5,6,7,8,
     *            2,1,4,3,6,5,8,7,
     *            3,4,1,2,7,8,5,6,
     *            4,3,2,1,8,7,6,5,
     *            5,6,7,8,1,2,3,4,
     *            6,5,8,7,2,1,4,3,
     *            7,8,5,6,3,4,1,2,
     *            8,7,6,5,4,3,2,1/
C
C        given the determinant strings -IACONF- and -IBCONF-,
C        return the determinants spatial symmetry -SYMB-.
C
      SYMB=BLANK
      ISYM=1
      DO 110 J=1,NACT
         IF(LBABEL(J).EQ.0) GO TO 300
         IF(IACONF(J).NE.IBCONF(J)) THEN
            IJMUL = MULT8(ISYM)+LBABEL(J)
            ISYM = LKUPSM(IJMUL)
         END IF
  110 CONTINUE
C
      NTIMES = 0
      DO  210 I=1,NIRRP
         IF(ISYM.EQ.LBIRRP(I)) THEN
            NTIMES=NTIMES+1
            IF(SOME  .AND.  NTIMES.EQ.2) WRITE(IW,9010) PTGRP,SYMB
            SYMB = SYIRRP(I)
            IF(SOME  .AND.  NTIMES.GT.1) WRITE(IW,9020) SYMB
         END IF
  210 CONTINUE
      IF(SOME  .AND.  NTIMES.GT.1) SYMB=BLANK
      RETURN
C
  300 CONTINUE
      SYMB=BLANK
      IF(SOME) WRITE(IW,9030)
C
 9010 FORMAT(/1X,'UNABLE TO UNAMBIGOUSLY ASSIGN STATE SYMMETRY',
     *          ' OF THE NEXT ROOT IN GROUP ',A8/
     *       1X,'A POSSIBLE SPACE SYMMETRY IS ',A4)
 9020 FORMAT(1X,'A POSSIBLE SPACE SYMMETRY IS ',A4)
 9030 FORMAT(/1X,'IT IS IMPOSSIBLE TO ASSIGN A SPACE SYMMETRY TO THE',
     *          ' NEXT STATE,'/
     *       1X,'SINCE ONE OR MORE OCCUPIED ORBITALS DO NOT HAVE',
     *          ' DEFINITE SYMMETRY.')
      END
C*MODULE ALDECI  *DECK DETDM1
      SUBROUTINE DETDM1(NPRINT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXRT=100, MXATM=500)
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,S,SZ,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORB,
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NCI,
     *                IGPDET,KSTSYM,NFTGCI
      COMMON /ENRGYS/ ENUCR,EELCT,ETOT,STOT,SSQUAR,ECORE,ESCF,EERD,
     *                E1,E2,VEN,VEE,EPOT,EKIN,ESTATE(MXRT),STATN
      COMMON /FMCOM / X(1)
      COMMON /FUNCT / E,EGRAD(3,MXATM)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,MA,MB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IJKT,IDAF,NAV,IODA(400)
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
C
      SOME = MASWRK  .AND.  NPRINT.NE.-5
      IF(SOME) WRITE(IW,9210) IROOT
C
C        allocate memory for one particle density matrix.
C
      M1 = NACT
      M2 = (M1*M1+M1)/2
      L1 = NUM
      L2 = (L1*L1+L1)/2
      L3 = L1*L1
      NSYM = 2**IGPDET
      NTOT = NACT + NCORSV
C
      CALL VALFM(LOADFM)
      LCI    = LOADFM + 1
      LDM1   = LCI    + NCI
C
      LIBO = LDM1 + M2
      LDAO = LIBO + NTOT
      LVAO   = LDAO   + L2
      LVNO   = LVAO   + L3
      LOCCNO = LVNO   + L3
      LIWRK  = LOCCNO + L1
      LWRK   = LIWRK  + M1
      LSCR   = LWRK   + 8*M1
      LIFA   = LSCR   + M1
      LAST   = LIFA   + (NACT+1)*(NACT+1)
      NEED = LAST - LOADFM - 1
      CALL GETFM(NEED)
C
      IF(EXETYP.EQ.CHECK) THEN
         CALL VCLR(X(LDAO)  ,1,L2)
         CALL VCLR(X(LVNO)  ,1,L3)
         CALL VCLR(X(LOCCNO),1,L1)
         CALL DAWRIT(IDAF,IODA,X(LDAO)  ,L2,16,0)
         CALL DAWRIT(IDAF,IODA,X(LVNO)  ,L3,19,0)
         CALL DAWRIT(IDAF,IODA,X(LOCCNO),L1,21,0)
      END IF
C
      CALL BINOM6(X(LIFA),NACT)
      CALL MATME1(NORB,NCOR,NA,NB,X(LIFA),NSYM,IIS)
      CALL DAREAD(IDAF,IODA,X(LIBO),NTOT,262,1)
      CALL CORTRA(X(LIBO),NTOT,NCORSV)
      CALL VALFM(LOADFM)
      LIWRK1 = LOADFM + 1
      LAST  = LIWRK1 + IIS
      NEED1 = LAST - LOADFM - 1
      CALL GETFM(NEED1)
      IF(EXETYP.EQ.CHECK) GO TO 580
C
C        set the energy to the root whose properties will be computed
C
      E = ESTATE(IROOT)
C
C        generate one particle density matrix for each state
C
      CALL SEQREW(NFT12)
      IF(MASWRK) READ(NFT12) NSTATS,NDETS
      IF (GOPARR) CALL DDI_BCAST(2503,'I',NSTATS,1,MASTER)
      IF (GOPARR) CALL DDI_BCAST(2504,'I',NDETS ,1,MASTER)
C
      IF(NSTATS.NE.K  .OR.  NDETS.NE.NCI) THEN
         IF(MASWRK) WRITE(IW,9250) NSTATS,NDETS,K,NCI
         CALL ABRT
      END IF
C
      DO 550 IST=1,K
         IF(NFLGDM(IST).EQ.0) THEN
            CALL SEQADV(NFT12)
            GO TO 550
         ELSE
            CALL SQREAD(NFT12,X(LCI),NCI)
         END IF
         IF(SOME) WRITE(IW,9220) IST,ESTATE(IST)
C
         CALL MATRD1(IW,X(LDM1),M2,NORB,NCOR,NA,NB,X(LCI),NCI,
     *               X(LIFA),X(LIWRK1),IIS,X(LIBO),IGPDET,KSTSYM,NSYM)
C
C
         IF(SOME  .AND.  NFLGDM(IST).EQ.2) THEN
            WRITE(IW,9230)
            CALL PRTRI(X(LDM1),NORB)
         END IF
         CALL DETNO(SOME,X(LDM1),X(LDAO),X(LVAO),X(LVNO),
     *              X(LOCCNO),X(LIWRK),X(LWRK),X(LSCR),ESTATE,
     *              IROOT,IST,NCORSV,NACT,M1,M2,L1,L2)
  550 CONTINUE
C
      CALL SEQREW(NFT12)
C
  580 CONTINUE
      CALL RETFM(NEED1)
      CALL RETFM(NEED)
      IF(SOME) WRITE(IW,9240)
      IF(SOME) CALL TIMIT(1)
      RETURN
C
 9210 FORMAT(/5X,27("-")/5X,'ONE PARTICLE DENSITY MATRIX'/5X,27("-")//
     *  1X,'DENSITY MATRIX WILL BE SAVED FOR PROPERTIES OF STATE',I4)
 9220 FORMAT(/1X,'CI EIGENSTATE',I4,' TOTAL ENERGY =',F20.10)
 9230 FORMAT(/1X,'1-PARTICLE DENSITY MATRIX IN MO BASIS')
 9240 FORMAT(1X,'..... DONE WITH ONE PARTICLE DENSITY MATRIX .....')
 9250 FORMAT(/1X,'***** ERROR IN -DETDM1- ROUTINE *****'/
     *       1X,'CI EIGENVECTOR FILE HAS DATA FOR NSTATE,NDETS=',I3,I10/
     *       1X,'BUT THE PRESENT CALCULATION HAS NSTATE,NDETS=',I3,I10)
      END
C*MODULE ALDECI  *DECK DETNO
C     ---------------------------------------------------------
      SUBROUTINE DETNO(SOME,DMO,DAO,VAO,VNO,OCCNO,IWRK,WRK,SCR,
     *                 ESTATE,IROOT,IST,NCORE,NACT,M1,M2,L1,L2)
C     ---------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME
C
      DIMENSION DMO(M2),DAO(L2),VAO(L1,L1),VNO(L1,L1),OCCNO(L1),
     *          IWRK(M1),WRK(M1,8),SCR(M1),ESTATE(*)
      DIMENSION TIMSTR(3)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IJKT,IDAF,NAV,IODA(400)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA OCCWRD,VECWRD,ENDWRD/8H $OCCNO ,8H $VEC   ,8H $END   /
#else
      CHARACTER*8 :: ENDWRD_STR
      CHARACTER*8 :: OCCWRD_STR
      CHARACTER*8 :: VECWRD_STR
      EQUIVALENCE (ENDWRD, ENDWRD_STR)
      EQUIVALENCE (OCCWRD, OCCWRD_STR)
      EQUIVALENCE (VECWRD, VECWRD_STR)
      DATA OCCWRD_STR,VECWRD_STR,ENDWRD_STR/" $OCCNO ",
     * " $VEC   "," $END   "/
#endif
      DATA ZERO,TWO/0.0D+00,2.0D+00/
C
C     ----- generate, print, and save natural orbitals -----
C
      NOCC = NCORE + NACT
      NVIRT = L1-NOCC
C
C        diagonalize MO density matrix
C
      CALL GLDIAG(L1,M1,M1,DMO,WRK,SCR,VNO(1,NCORE+1),IERR,IWRK)
      IF(IERR.NE.0) THEN
         WRITE(IW,*) 'DIAG FAILED IN DETNO'
         CALL ABRT
      END IF
C
C        reorder from biggest to lowest eigenvalues
C
      NDUM = M1/2
      DO 140 IDUM = 1,NDUM
         NFIRST = IDUM
         NLAST = M1-IDUM+1
         DUM = SCR(NLAST)
         SCR(NLAST ) = SCR(NFIRST)
         SCR(NFIRST) = DUM
         CALL DSWAP(M1,VNO(1,NCORE+NFIRST),1,VNO(1,NCORE+NLAST),1)
  140 CONTINUE
C
C        TRANSFORM active NO'S TO ATOMIC BASIS
C
      CALL DAREAD(IDAF,IODA,VAO,L1*L1,15,0)
      CALL TFSQB(VNO(1,NCORE+1),VAO(1,NCORE+1),OCCNO,NACT,L1,L1)
C
C        copy any core and virtual orbitals, adjust phase of NOs
C
      IF(NCORE.GT.0) CALL DCOPY(L1*NCORE,VAO,1,VNO,1)
      IF(NVIRT.GT.0) CALL DCOPY(L1*NVIRT,VAO(1,NOCC+1),1,
     *                                   VNO(1,NOCC+1),1)
      CALL STFASE(VNO,L1,L1,L1)
C
C        Set up occupation number array
C
      DO 210 I = 1,NCORE
         OCCNO(I) = TWO
  210 CONTINUE
      DO 220 I = 1,NACT
         OCCNO(I+NCORE) = SCR(I)
  220 CONTINUE
      DO 230 I=1,NVIRT
         OCCNO(I+NOCC) = ZERO
  230 CONTINUE
C
C        PRINT AND PUNCH NO'S IN AO BASIS
C
      IF(SOME) THEN
         WRITE(IW,9050)
         CALL PREV(VNO,OCCNO,NOCC,L1,L1)
         CALL TMDATE(TIMSTR)
         WRITE(IP,9100) IST,ESTATE(IST),TIMSTR
         WRITE(IP,9120) OCCWRD
         WRITE(IP,9110) (OCCNO(I),I=1,NOCC)
         WRITE(IP,9120) ENDWRD
         WRITE(IP,9120) VECWRD
         CALL PUSQL(VNO,NOCC,L1,L1)
         WRITE(IP,9120) ENDWRD
      END IF
C
      IF(IST.NE.IROOT) RETURN
C
C        generate ao basis density for future property calculation.
C        note: the approach here is correct only for full ci
C
      CALL DMTX(DAO,VNO,OCCNO,NOCC,L1,L1)
      CALL DAWRIT(IDAF,IODA,DAO  ,L2   ,16,0)
      CALL DAWRIT(IDAF,IODA,VNO  ,L1*L1,19,0)
      CALL DAWRIT(IDAF,IODA,OCCNO,L1   ,21,0)
      RETURN
C
 9050 FORMAT(/10X,'NATURAL ORBITALS IN ATOMIC ORBITAL BASIS'/
     *        10X,40(1H-))
 9100 FORMAT('- - - NO-S OF CI STATE',I3,' E=',F20.10,
     *       ' CREATED',3A8)
 9110 FORMAT(5F16.10)
 9120 FORMAT(A8)
      END
C*MODULE ALDECI  *DECK DETDM2
      SUBROUTINE DETDM2(NPRINT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME,PACK2E,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXRT=100, MXATM=500)
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,S,SZ,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORB,
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NCI,
     *                IGPDET,KSTSYM,NFTGCI
      COMMON /ENRGYS/ ENUCR,EELCT,ETOT,STOT,SSQUAR,ECORE,ESCF,EERD,
     *                E1,E2,VEN,VEE,EPOT,EKIN,ESTATE(MXRT),STATN
      COMMON /FMCOM / X(1)
      COMMON /FUNCT / E,EGRAD(3,MXATM)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,MA,MB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /IOFILE/ IR,IW,IP,IS,IJKT,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /OUTPUT/ NPRINTX,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCKLAB/ LABSIZ
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
      PARAMETER (ZERO=0.0D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
C
C        ----- state averaged 1e- and 2e- density matrix -----
C
      SOME = MASWRK  .AND.  NPRINT.NE.-5
      IF(SOME) WRITE(IW,9310)
C
      M1 = NACT
      M2 = (M1*M1+M1)/2
      M4 = (M2*M2+M2)/2
      L1 = NUM
      NSYM = 2**IGPDET
      NTOT = NORB + NCORSV
C
      MXSTAT=0
      MXNZW=0
      DO 100 I=MXRT,1,-1
         IF(WSTATE(I).GT.ZERO) THEN
            IF(MXSTAT.EQ.0) MXSTAT=I
            MXNZW=MXNZW+1
         END IF
  100 CONTINUE
      IF(MXSTAT.EQ.0) THEN
         WRITE(IW,*) 'OOPS, IN -DETDM2-, SOMETHING HAPPENED TO WSTATE'
         CALL ABRT
      END IF
C
C        ----- allocate memory for two particle density matrix -----
C        N.B. the +5 below is to pad storage a bit in the case of no
C        beta electrons.  This is a work around, instead we should
C        fix the -MATRSA- code for the case of no beta electrons,
C        rather than letting the code address the IBCON1(0) element.
C
      NOCC1 = NCORSV + NACT
      NOCC2 = (NOCC1*NOCC1+NOCC1)/2
C
      CALL VALFM(LOADFM)
      LIBO = LOADFM + 1
      LCI    = LIBO + NTOT
      LDM1   = LCI    + MXNZW*NCI
      LDM2   = LDM1   + M2
      LXX    = LDM2 + M4
      LIXX   = LXX    + NINTMX
      LWRK   = LIXX   + NINTMX
      LIFA   = LWRK   + NOCC2
      LWST   = LIFA   + ((M1+1)*(M1+1))/NWDVAR + 1
      LIWST  = LWST   + MXNZW
      LLABMO = LIWST  + MXNZW
      LLBABL = LLABMO + L1
      LLBIRP = LLBABL + M1
      LSYIRP = LLBIRP + 12
      LAST   = LSYIRP + 12
      NEED = LAST - LOADFM - 1
      CALL GETFM(NEED)
C
      IF(SOME) WRITE(IW,9320) NEED,MXNZW
C
C        set state averaged energy, print root information
C
      E = ZERO
      NXTR=0
      DO 310 IST=1,K
         IF(IPURES.EQ.1  .AND.  ABS(SPINS(IST)-S).GT.0.03D+00) GO TO 310
         NXTR=NXTR+1
         IF(WSTATE(NXTR).GT.ZERO) THEN
            E = E + WSTATE(NXTR) * ESTATE(IST)
            IF(SOME) WRITE(IW,9340) IST,ESTATE(IST),
     *                              WSTATE(NXTR),SPINS(IST)
         END IF
         IF(NXTR.GT.MXSTAT) GO TO 320
  310 CONTINUE
C
C        Croak the job if we didn't calculate enough roots with the
C        desired spin multiplicity during the CI diagonalization.
C        If this happens on the 1st MCSCF iter, we've already got
C        the CI expansions printed out, and should not repeat it.
C
      IF(NXTR.LT.MXSTAT) THEN
         IF(MASWRK) WRITE(IW,9350) NXTR,S,MXSTAT
         IF(MASWRK  .AND.  .NOT.SOME) CALL DETPRT(IW,NFT12,MASWRK)
         CALL ABRT
      END IF
C
  320 CONTINUE
      CALL BINOM6(X(LIFA),NACT)
      CALL MATME2(NORB,NCOR,NA,NB,X(LIFA),NSYM,IIS)
      CALL DAREAD(IDAF,IODA,X(LIBO),NTOT,262,1)
      CALL CORTRA(X(LIBO),NTOT,NCORSV)
      CALL VALFM(LOADFM)
      LIWRK1 = LOADFM + 1
      LAST  = LIWRK1 + IIS
      NEED1 = LAST - LOADFM - 1
      CALL GETFM(NEED1)
C
      CALL DETGRP(GRPDET,X(LLABMO),X(LLBABL),PTGRP,X(LLBIRP),
     *            X(LSYIRP),NSYM,NIRRP,L1,NACT,NCORSV)
C
      IF(EXETYP.EQ.CHECK) THEN
         CALL VCLR(X(LDM1),1,M2)
         CALL VCLR(X(LDM2),1,M4)
         GO TO 700
      END IF
C
C        obtain CI coeficients for all states with nonzero weights
C
      CALL SEQREW(NFT12)
      IF(MASWRK) READ(NFT12) NSTATS,NDETS
      IF (GOPARR) CALL DDI_BCAST(2505,'I',NSTATS,1,MASTER)
      IF (GOPARR) CALL DDI_BCAST(2506,'I',NDETS ,1,MASTER)
C
      IF(NSTATS.NE.K  .OR.  NDETS.NE.NCI) THEN
         IF(MASWRK) WRITE(IW,9360) NSTATS,NDETS,K,NCI
         CALL ABRT
      END IF
C
      LCIVEC = LCI
      NXTW=1
      NXTR=0
      DO 620 IST=1,K
         IF(IPURES.EQ.1) THEN
            IF(ABS(SPINS(IST)-S).GT.0.03D+00) THEN
               CALL SEQADV(NFT12)
               GO TO 620
            END IF
            NXTR=NXTR+1
         ELSE
            NXTR=IST
         END IF
C
         IF(NXTR.NE.IWTS(NXTW)) THEN
            CALL SEQADV(NFT12)
         ELSE
            CALL SQREAD(NFT12,X(LCIVEC),NCI)
            LCIVEC = LCIVEC + NCI
            NXTW=NXTW+1
         END IF
  620 CONTINUE
      CALL SEQREW(NFT12)
C
      CALL MATRDS(IW,X(LDM1),X(LDM2),NORB,NCOR,NA,NB,X(LCI),NCI,
     *            X(LIFA),X(LIWRK1),IIS,X(LIBO),IGPDET,KSTSYM,NSYM,
     *            MXNZW,IWTS,WSTATE,X(LIWST),X(LWST),K)
C
C        output of density matrices
C
  700 CONTINUE
      CUTOFF = MAX(1.0D-11,10.0D+00**(-ICUT))
      IF(SOME) WRITE(IW,9370) X(LSYIRP),PTGRP
C
      CALL SEQOPN(NFT15,'WORK15','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQREW(NFT15)
      CALL WTDM12(EXETYP,X(LDM1),X(LDM2),X(LLBABL),X(LXX),X(LIXX),
     *            NINTMX,LABSIZ,M1,M2,M4,
     *            X(LWRK),NOCC2,NCORSV,CUTOFF,NFT15,NRECO,NDM2O)
      CALL SEQREW(NFT15)
C
      IF(SOME) WRITE(IW,9380) NDM2O,NRECO,NFT15
      CALL RETFM(NEED1)
      CALL RETFM(NEED)
      IF(SOME) WRITE(IW,9390)
      IF(SOME) CALL TIMIT(1)
      RETURN
C
 9310 FORMAT(/5X,50("-")/
     *       5X,' ONE AND TWO PARTICLE DENSITY MATRIX COMPUTATION'/
     *       5X,'PROGRAM WRITTEN BY JOE IVANIC AND KLAUS RUEDENBERG'/
     *       5X,50(1H-))
 9320 FORMAT(/1X,I10,' WORDS WILL BE USED TO FORM THE DENSITIES'/
     *       1X,'THE DENSITIES ARE STATE AVERAGED OVER',I4,' ROOT(S)')
 9340 FORMAT(1X,'STATE=',I4,'   ENERGY=',F20.10,'   WEIGHT=',F8.5,
     *           '   S=',F6.2)
 9350 FORMAT(/1X,'***** ERROR *****'/
     *       1X,'THIS RUN FOUND',I5,' CI EIGENVECTORS WITH S=',F5.2,','/
     *       1X,'BUT YOU REQUESTED STATE AVERAGING OF',I5,' ROOTS.'/
     *       1X,'PLEASE EXAMINE YOUR CHOICE OF -NSTATE- INPUT DATA.'/)
 9360 FORMAT(/1X,'***** ERROR IN -DETDM2- ROUTINE *****'/
     *       1X,'CI EIGENVECTOR FILE HAS DATA FOR NSTATE,NDETS=',I3,I10/
     *       1X,'BUT THE PRESENT CALCULATION HAS NSTATE,NDETS=',I3,I10)
 9370 FORMAT(1X,'SIEVING THE ',A4,
     *          ' SYMMETRY NONZERO DENSITY ELEMENTS IN GROUP ',A8)
 9380 FORMAT(1X,I10,' NONZERO DM2 ELEMENTS WRITTEN IN',I8,
     *          ' RECORDS TO FILE',I3)
 9390 FORMAT(1X,'..... DONE WITH 1 AND 2 PARTICLE DENSITY MATRIX .....')
      END
C*MODULE ALDECI  *DECK WTDM12
      SUBROUTINE WTDM12(EXETYP,DM1,DM2,LBABEL,X,IX,NINTMX,LABSIZ,
     *                  M1,M2,M4,WRK,NOCC2,NCORE,CUTOFF,
     *                  NFTO,NRECO,NDM2O)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXAO=2047)
C
      LOGICAL IEQJ,KEQL,GOPARR,DSKWRK,MASWRK
C
      DIMENSION DM1(M2),DM2(M4),LBABEL(M1),
     *          X(NINTMX),IX(*),WRK(NOCC2)
      DIMENSION MULT8(8),LKUPSM(64)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IS,IJKT,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (HALF=0.5D+00, TWO=2.0D+00)
C
      DATA MULT8/0,8,16,24,32,40,48,56/
      DATA LKUPSM/1,2,3,4,5,6,7,8,
     *            2,1,4,3,6,5,8,7,
     *            3,4,1,2,7,8,5,6,
     *            4,3,2,1,8,7,6,5,
     *            5,6,7,8,1,2,3,4,
     *            6,5,8,7,2,1,4,3,
     *            7,8,5,6,3,4,1,2,
     *            8,7,6,5,4,3,2,1/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
C
C     ----- WRITE -dm1- and -DM2- -----
C
      THRSH = 1.0D+01*CUTOFF
C
      DO 50 I=1,M1
         IF(LBABEL(I).EQ.0) THEN
            IBAD = I+NCORE
            IF(MASWRK) WRITE(IW,9000) IBAD
            CALL ABRT
         END IF
   50 CONTINUE
C
      CALL DSCAL(M2,     HALF,DM1,1)
      CALL DSCAL(M4,HALF*HALF,DM2,1)
C
      SMALL = 1.0D-07
      IF(EXETYP.EQ.CHECK) SMALL = -HALF
      NERR = 0
      IJ = 0
      DO 110 I=1,M1
         ISYM = LBABEL(I)
         DO 100 J=1,I
            IJ = IJ+1
            IJMUL = MULT8(ISYM)+LBABEL(J)
            IJSYM = LKUPSM(IJMUL)
            IF(IJSYM.NE.1) THEN
               IF(ABS(DM1(IJ)).LT.THRSH) THEN
                  DM1(IJ) = 0.0D+00
               ELSE
                  IBAD = I+NCORE
                  JBAD = J+NCORE
                  IF(MASWRK) WRITE(IW,9010) IBAD,JBAD,DM1(IJ)
                  IF(MASWRK) WRITE(IW,9030)
                  CALL ABRT
               END IF
            END IF
  100    CONTINUE
         DM1(IJ) = DM1(IJ) + DM1(IJ)
         IF(DM1(IJ).LT.SMALL) THEN
            IF(MASWRK) WRITE(IW,9040) I,DM1(IJ)
            NERR=NERR+1
         END IF
  110 CONTINUE
C
      IF(NERR.GT.0) THEN
         IF(MASWRK) WRITE(IW,*) 'CHECK YOUR ACTIVE SPACE CAREFULLY.'
         IF(MASWRK) WRITE(IW,*) 'THE 1ST ORDER DENSITY MATRIX IS:'
         CALL PRTRI(DM1,M1)
         CALL ABRT
      END IF
C
C     ----- write nonzero elements of -dm2- on file -nfto- -----
C
      NRECO=0
      NDM2O=0
C
      N = 0
      NX = 1
      DO 280 I = 1,M1
         ISYM = LBABEL(I)
         DO 260 J = 1,I
            IEQJ = I.EQ.J
            IJMUL = MULT8(ISYM)+LBABEL(J)
            IJSYM = LKUPSM(IJMUL)
            DO 240 K = 1,I
               LMAX = K
               IF(K.EQ.I) LMAX = J
               IJKMUL = MULT8(IJSYM)+LBABEL(K)
               IJKSYM = LKUPSM(IJKMUL)
               DO 220 L = 1,LMAX
                  KEQL=K.EQ.L
                  N = N+1
                  VAL = DM2(N)
C
                  IF(IEQJ)                VAL=VAL+VAL
                  IF(KEQL)                VAL=VAL+VAL
                  IF(I.EQ.K .AND. J.EQ.L) VAL=VAL+VAL
                  IF(ABS(VAL).LT.CUTOFF) GO TO 220
C
C      only totally symmetric direct product should be nonzero elements
C
                  LSYM = LBABEL(L)
                  IF(LSYM.NE.IJKSYM) THEN
                     IF(ABS(VAL).LT.THRSH) GO TO 220
                     IBAD = I+NCORE
                     JBAD = J+NCORE
                     KBAD = K+NCORE
                     LBAD = L+NCORE
                     IF(MASWRK) WRITE(IW,9020) IBAD,JBAD,KBAD,LBAD,VAL
                     IF(MASWRK) WRITE(IW,9030)
                     CALL ABRT
                  END IF
C
              NPACK = NX
              IPACK = I
              JPACK = J
              KPACK = K
              LPACK = L
              IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                LABEL1 = ISHFT( IPACK, 16 ) + JPACK
                LABEL2 = ISHFT( KPACK, 16 ) + LPACK
                IX( 2*NPACK-1 ) = LABEL1
                IX( 2*NPACK   ) = LABEL2
#endif
#if defined(SPEC_CPU_ILP64)
                LABEL = ISHFT( IPACK, 48 ) + ISHFT( JPACK, 32 ) +
     *                  ISHFT( KPACK, 16 ) + LPACK
                IX(NPACK) = LABEL
#endif
              ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                  ISHFT( KPACK,  8 ) + LPACK
                IX(NPACK) = LABEL
#endif
#if defined(SPEC_CPU_ILP64)
                IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                  LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                    ISHFT( KPACK,  8 ) + LPACK
                  IX( NPACK/2 ) = IX( NPACK/2 ) + LABEL
                ELSE
                  LABEL = ISHFT( IPACK, 56 ) + ISHFT( JPACK, 48 ) +
     *                    ISHFT( KPACK, 40 ) + ISHFT( LPACK, 32 )
                  IX( (NPACK/2)+1 ) = LABEL
                END IF
#endif
              END IF
C
                  X(NX) = VAL
                  NX = NX+1
                  IF(NX.GT.NINTMX) THEN
                     CALL PWRIT(NFTO,X,IX,NINTMX,NINTMX)
                     NRECO=NRECO+1
                     NDM2O=NDM2O+NINTMX
                     NX = 1
                  END IF
  220          CONTINUE
  240       CONTINUE
  260    CONTINUE
  280 CONTINUE
C
C         OUTPUT THE FINAL, PARTIAL BUFFER OF DM2
C
      NX = -(NX-1)
      CALL PWRIT(NFTO,X,IX,NX,NINTMX)
      NDM2O=NDM2O-NX
      NRECO=NRECO+1
C
C     ----- output first order density in MO basis -----
C     generate mo density over all orbitals, including core
C
      CALL VCLR(WRK,1,NOCC2)
      II = 0
      DO 300 I=1,NCORE
         II = II+I
         WRK(II) = TWO
  300 CONTINUE
      DO 320 I=1,M1
         IV = IA(I)
         IC = IA(I+NCORE)
         DO 310 J=1,I
             IJV = IV + J
             IJC = IC + J + NCORE
             WRK(IJC) = DM1(IJV)
  310    CONTINUE
  320 CONTINUE
C
C     write -dm1- without core orbitals to daf record 320
C     write -dm1- with    core orbitals to daf record 68
C     write -dm1- with    core orbitals after -dm2- on file -nfto-
C
      CALL DAWRIT(IDAF,IODA,DM1,   M2,320,0)
      CALL DAWRIT(IDAF,IODA,WRK,NOCC2, 68,0)
      CALL SQWRIT(NFTO,WRK,NOCC2)
      RETURN
C
 9000 FORMAT(1X,'UNABLE TO SIFT DENSITY MATRIX, ORBITAL',I5,
     *          ' HAS UNKNOWN SYMMETRY.')
 9010 FORMAT(/1X,'INACCURATE 1ST ORDER DENSITY MATRIX ELEMENT FOUND,'/
     *       1X,'GAMMA(',2I5,')=',E20.10,' FOUND, IT SHOULD BE ZERO'/
     *       1X,'BY SYMMETRY.')
 9020 FORMAT(/1X,'INACCURATE 2ND ORDER DENSITY MATRIX ELEMENT FOUND,'/
     *       1X,'GAMMA(',4I5,')=',E20.10,' FOUND, IT SHOULD BE ZERO'/
     *       1X,'BY SYMMETRY.')
 9030 FORMAT(/1X,'LOSS OF SYMMETRY IN THE DENSITY MATRIX MAY BE DUE TO'/
     *       1X,'    UNSYMMETRICAL ORBITALS: CHECK $VEC GROUP,'/
     *       1X,'         ADJUST $GUESS TOLZ=1.0D-5 TOLE=1.0D-4'/
     *       1X,'    OR, UNSYMMETRICAL CI ROOT: $DET CVGTOL=5.0D-6')
 9040 FORMAT(1X,'***** ERROR: ACTIVE ORBITAL',I3,
     *          ' HAS VERY SMALL OCCUPATION NUMBER=',1P,E13.6)
      END
C*MODULE ALDECI  *DECK BINOM6
C     ------------------------
      SUBROUTINE BINOM6(IFA,N)
C     ------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER IFA(0:N,0:N)
C
C     Returns all binomial numbers (i,j) for i=0,n and j=0,i in fa .
C     The binomial number (i,j) is stored in ifa(i,j)
C
      DO 13 II=0,N
         IFA(II,0)  = 1
         IFA(II,II) = 1
   13 CONTINUE
C
      DO 113 IY = 1, N
         DO 114 IX = 1, (IY-1)
            IFA(IY,IX) = IFA(IY-1,IX-1) + IFA(IY-1,IX)
  114    CONTINUE
  113 CONTINUE
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK DAVCI
C     ----------------------------------------------------------
      SUBROUTINE DAVCI(IW,SOME,ECONST,SI1,SI2,NORB,NCOR,NCI,NA,NB,
     *    CI,SPIN,EL,K,KST,MAXP,MAXW1,NITER,CRIT,
     *    AB,Q,B,EF,F,IWRK2,EC,IACON1,IBCON1,IACON2,IBCON2,IPOSA,IPERA,
     *    IIND1,IWRK1,ISD,IDO,IFA,INDEX,IMMA,IMMC,ISYMA,ISYMB,ITAB,
     *    IMUL,ISPA,ISPB,ISAS,ISBS,ISAC,ISBC,NSYM,IOB,NALP,NBLP,
     *      IHMCON,GR,ISTRB,ISTRP,ISTAR,ISTAT)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL SOME,GOPARR,DSKWRK,MASWRK,ANALYS
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /DETPAR/ ICLBBR,ANALYS
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      DIMENSION SI1(*), SI2(*), CI(NCI,MAXP),AB(NCI,MAXP),Q(NCI)
      DIMENSION F((MAXW1*(MAXW1+1))/2),EF(MAXW1,MAXW1),EL(MAXW1)
      DIMENSION EC(MAXP,MAXP),IWRK2(MAXW1),IWRK1(2*MAXW1),B(8*MAXW1)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2)
      DIMENSION IACON1(NA+NCOR),IBCON1(NA+NCOR)
      DIMENSION IACON2(NA+NCOR),IBCON2(NB+NCOR)
      DIMENSION IPERA(NA*(NORB-NCOR-NA))
      DIMENSION IIND1(NA*(NORB-NCOR-NA))
      DIMENSION IPOSA(NA*(NORB-NCOR-NA))
      DIMENSION IMMA(NSYM*(NA*(NORB-NCOR-NA)))
      DIMENSION IMMC(NSYM)
      DIMENSION ISD(NA+NB),IDO(NA)
      DIMENSION SPIN(MAXP)
      DIMENSION ISYMA(NALP),ISYMB(NBLP)
      DIMENSION ITAB(NSYM),ISPA(NALP),ISPB(NBLP)
      DIMENSION IMUL(NSYM,NSYM)
      DIMENSION ISAS(NSYM+1),ISBS(NSYM+1),ISAC(NALP),ISBC(NBLP)
      DIMENSION IHMCON(K)
      DIMENSION GR(K)
      DIMENSION IOB(NORB-NCOR)
      DIMENSION ISTRB((NBLP*(NB*(NORB-NCOR-NB)))/2)
      DIMENSION ISTRP((NBLP*(NB*(NORB-NCOR-NB)))/2),ISTAR(NBLP)
C
C   Code to do a full CI almost completely directly.  Written by
C   J. Ivanic '99.  This code makes use of symmetry.
C
C   THIS SHOULD REALLY ONLY BE CALLED USING DETCI, UNLESS YOU REALLY
C   KNOW WHAT YOU ARE DOING.
C
C   si1, si2  : 1 and 2 electron integrals stored in reverse canonical
C       order, same as GAMESS
C   norb,ncor : total number of orbitals and number of core orbs
C   nci       : number of determinants, size of CI essentially
C   na, nb    : number of active alpha and beta electrons respectively
C   CI        : returned CI coefficients, CI(i,j) = coefficient of
C               determinant i for state j.
C   spin      : returned spin, spin(i) = spin of state i
C   EL        : EL(i) = total electronic energy of state i
C   k, kst    : No. of states required and number of minimum/starting
C               states in CI procedure.
C   maxp      : Maximum number of vectors before transforming and
C               starting at kst.
C   maxw1     : Size of diagonalization for initial guess vectors.
C   niter,crit: Maximum no. of total iters and convergence criterion.
C               I very strongly suggest crit = 5.0d-5, this gives
C               accuracy to at least 8 decimal places.  niter I would
C               make very large,2000, because I am sure that if you
C               have reasonable orbitals, states will have to converge
C               eventually.
C      gr     : gr(i) contains gradient for state i.
C
C    VARIABLES BELOW ARE ALL SCRATCH
C
C   Ab,q      : Scratch double precisions, Ab contains the matrix
C               H.c, and q contains diagonal elements of hamiltonian.
C   B,EF,F,EC : Double precisions for diagonalization routine, EVVRSP.
C               EC is used to check if states have flipped.
C   iwrk2     : Scratch integer array used in EVVRSP
C   iwrk1     : This is a scratch integer array used very effectively
C               in the inital guess.
C   isd,iso   : Used in retspin and other places, work integer arrays.
C   iacon1,ibcon1,iacon2,ibcon2,ipera,iposa,iind1 : scratch integer
C               arrays used in retAb and rinAb which are really the
C               guts of the program.
C   ifa, index: ifa contains binomial coefficients, by calling binom6,
C               and index contains a list of positions for indices for
C              the integral arrays.
C   Everything else is for symmetry and more info may be obtained from
C   subroutine symwrk.
C
      ISTAT=0
      NACT = NORB-NCOR
C
      DO 7 I=1,(NORB*(NORB+1))/2
         DO 8 J=1,I
            INDEX(I,J) = I*(I-1)/2 + J
            INDEX(J,I) = INDEX(I,J)
    8    CONTINUE
    7 CONTINUE
C
C      nalp = ifa(nact,na)
C      nblp = ifa(nact,nb)
C
      IF (NB+NCOR.EQ.0) NBLP = 0
C
C   Initial setup, first work out diagonal elements.
C
      CALL GETQ(SI1,SI2,NORB,NCOR,NCI,NA,NB,IACON1,IBCON1,
     *          INDEX,NALP,NBLP,Q,
     *       ISYMA,ISYMB,ISPA,ISPB,
     *       ISAS,ISBS,ISAC,ISBC,NSYM)
C
C     Keep option here to average diagonal elements for spin
C     consistency.......
C
C      call averq(iw,CI,nci,nalp,nblp,na,nb,ncor,norb,iacon1,
C     *           ibcon1,iacon2,ibcon2,ifa,isd,ido,iwrk1,maxw1,q)
C
      IF (NCI.LE.MAXW1) GOTO 2345
C
C   See if we have initial guess vectors on disk.
C   If the size of the initial guess space requested by the user is
C   bigger than the number of vectors on disk, we always go to the
C   initial guess code below.  the alternative is to reset KST=NSTATE
C   after reading the header record.
C
      NSTATE = 0
      NVECS  = 0
      IF(KST.GT.NSTATE) GO TO 2345
      IF(ICLBBR.EQ.1)   GO TO 2345
C
      CALL SEQOPN(NFT12,'CIVECTR','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQREW(NFT12)
      IF(MASWRK) READ(NFT12,ERR=2343,END=2343) NSTATE,NVECS
      GO TO 2344
C
 2343 CONTINUE
      NSTATE=0
      NVECS =0
C
C         let other nodes know if anything was read
C
 2344 CONTINUE
      IF (GOPARR) CALL DDI_BCAST(2507,'I',NSTATE,1,MASTER)
      IF (GOPARR) CALL DDI_BCAST(2508,'I',NVECS ,1,MASTER)
C
C         if nothing read, we must make an initial guess
C
      IF (NSTATE+NVECS.EQ.0) GO TO 2345
C
C         if inconsistency read, we must terminate
C
      IF (NVECS.NE.NCI) THEN
         IF (SOME) WRITE(IW,9005) NVECS,NCI
         CALL ABRT
      ENDIF
C
C  Yes, we have, read these in and use them.
C
      DO 100 ISTATE = 1,NSTATE
         CALL SQREAD(NFT12,CI(1,ISTATE),NVECS)
         IF(NVECS.EQ.0) THEN
            IF (SOME) WRITE(IW,*)
     *         'UNEXPECTED END OF FILE ON UNIT',NFT12
            CALL ABRT
         END IF
  100 CONTINUE
      IF (SOME) WRITE(IW,9007)
      GOTO 3333
C
 2345 CONTINUE
      NVECS = 0
C
C    Determine the initial guess vectors.
C
      DO 87 II=1,NCI
         DO 89 JJ=1,MAXP
            CI(II,JJ) = 0.0D+00
   89    CONTINUE
   87 CONTINUE
C
      DO 77 II=1,NCI
         CI(II,1) = Q(II)
   77 CONTINUE
C
      IF(SOME) CALL TSECND(E0)
C
      CALL INITI(IW,B,NCI,NALP,NBLP,NA,NB,NCOR,NORB,IACON1,IBCON1,
     *   IACON2,IBCON2,IFA,ISD,IDO,CI,IWRK1,MAXW1,KST,INDEX,F,EL,EF,
     *   SI1,SI2,IWRK2,IMARK,AB,NSIZE,
     *       ISYMA,ISPA,ISPB,
     *       ISBS,ISBC,NSYM,SOME)
C
C   Check if we have finished the CI by doing the first
C   diagonalization.
C
      IF (IMARK.EQ.1) THEN
         CALL  RETSPIN(NA,NB,NACT,IACON1,IACON2,IBCON1,IBCON2,
     *      ISD(1),ISD(NA+1),IDO,CI,AB,NALP,NBLP,IFA,K,NCI,SPIN,
     *       ISYMA,ISPA,ISPB,ISBS,ISBC,NSYM)
C
         IF (ANALYS.AND.SOME) THEN
            CALL ECORR(IW,NFT12,
     *                 EL,ECONST,Q(1),CI,NCI,K,SI1,SI2,NACT,NA,NB,
     *                 IFA,INDEX,IACON1,IACON2,IBCON1,IBCON2,
     *                 ISYMA,IMUL,ISPA,ISPB,
     *                 ISBS,ISBC,NSYM,IOB,NALP,NBLP)
         ENDIF
         RETURN
      ENDIF
C
 3333 CONTINUE
C
      IF (NA.EQ.NB) THEN
         CALL RETSPIN(NA,NB,NACT,IACON1,IACON2,IBCON1,IBCON2,
     *      ISD(1),ISD(NA+1),IDO,CI,AB,NALP,NBLP,IFA,KST,NCI,SPIN,
     *       ISYMA,ISPA,ISPB,ISBS,ISBC,NSYM)
      DO 856 I=1,KST
         IWRK1(I)=INT(SPIN(I) + 0.3D+00)
         IWRK2(I)=I
  856 CONTINUE
      ENDIF
C
      CALL SETUP(NORB,NCOR,NA,NB,IBCON1,IACON2,IFA,INDEX,
     *       ISPB,NBLP,ISTRB,ISTRP,ISTAR)
C
      IF(SOME) THEN
         CALL TSECND(E1)
         ELAP = E1 - E0
         E0 = E1
         IF (NVECS.EQ.0) WRITE(IW,9010) ELAP
      END IF
C
      IF (NA.EQ.NB) THEN
      IF (KST.GT.1) THEN
      CALL RINAB0(SI1,SI2,NORB,NCOR,NCI,NA,NB,CI,IACON1,IBCON1,
     *             IACON2,
     *       IFA,IPOSA,IPERA,IIND1,INDEX,AB,KST,Q,UAIA,UAIB,
     *       IMMA,IMMC,
     *       ISYMA,ISYMB,ITAB,
     *       IMUL,ISPA,ISPB,
     *       ISBS,ISBC,NSYM,IOB,NALP,NBLP,
     *       IWRK1,IWRK2,ISTRB,ISTRP,ISTAR)
      ELSE
      CALL RETAB0(SI1,SI2,NORB,NCOR,NCI,NA,NB,CI,IACON1,IBCON1,
     *             IACON2,
     *       IFA,IPOSA,IPERA,IIND1,INDEX,AB,Q,UAIA,UAIB,
     *       IMMA,IMMC,
     *       ISYMA,ISYMB,ITAB,
     *       IMUL,ISPA,ISPB,
     *       ISBS,ISBC,NSYM,IOB,NALP,NBLP,
     *       IWRK1,IWRK2,ISTRB,ISTRP,ISTAR)
      ENDIF
      ELSE
      CALL RINAB(SI1,SI2,NORB,NCOR,NCI,NA,NB,CI,IACON1,IBCON1,
     *             IACON2,
     *       IFA,IPOSA,IPERA,IIND1,INDEX,AB,KST,Q,UAIA,UAIB,
     *       IMMA,IMMC,
     *       ISYMA,ISYMB,ITAB,
     *       IMUL,ISPA,ISPB,
     *       ISAS,ISBS,ISAC,ISBC,NSYM,IOB,NALP,NBLP,
     *       ISTRB,ISTRP,ISTAR)
      ENDIF
C
      IF(SOME) THEN
         CALL TSECND(E1)
         ELAP = E1 - E0
         WRITE(IW,9020) ELAP
C         WRITE(IW,9030) UAIA,UAIB,SAIT,TAIT
      END IF
C
      DO 13 II=1,KST
         EL(II) = 0.0D+00
         DO 15 KK=1,NCI
            EL(II) = EL(II) + CI(KK,II)*AB(KK,II)
   15    CONTINUE
   13 CONTINUE
      DO 555 II=1,MAXP
         DO 677 JJ=1,II-1
            EC(II,JJ) = 0.0D+00
            EF(II,JJ) = 0.0D+00
            EC(JJ,II) = 0.0D+00
            EF(JJ,II) = 0.0D+00
  677    CONTINUE
         EC(II,II) = 1.0D+00
         EF(II,II) = 1.0D+00
  555 CONTINUE
C
C     Now to get into the loop, set some loop constants here
C     ip is the current number of CI vectors being dealt with.
C     il is the current root being optimized.
C
      IPXT = -1
      IP = KST
      IL = 1
      NTCON = K
      DO 4599 KL = 1,K
      IHMCON(KL) = KL
      IWRK2(KL) = KL
 4599 CONTINUE
C
C     Loop over number of roots, iterations for each root.
C
      IF(SOME) WRITE(IW,9040)
  333 CONTINUE
C
      DO 1315 ITER=0,NITER
C
      IPXT = IPXT + 1
C
C     Check to see if ip = maxp, if
C     so then transform the first kst vectors in CI and Ab
C     and start over with ip = k
C
      IF (IP+NTCON.GT.MAXP) THEN
         CALL TRAN(CI,NCI,MAXW1,EF,IP,EC,KST)
         CALL TRAN(AB,NCI,MAXW1,EF,IP,EC,KST)
         IP = KST
         DO 1396 II=1,MAXP
            EC(II,II) = 1.0D+00
            EF(II,II) = 1.0D+00
            DO 1398 JJ=1,II-1
               EC(II,JJ) = 0.0D+00
               EF(II,JJ) = 0.0D+00
               EC(JJ,II) = 0.0D+00
               EF(JJ,II) = 0.0D+00
 1398       CONTINUE
 1396    CONTINUE
      ENDIF
C
C   Make gradient vectors, put in CI(ip+1) -> CI(ip+ntcon)
C
      DO 4588 KK=1,NTCON
      IL = IHMCON(KK)
      GR(KK) = 0.0D+00
      DO 80 II=1,NCI
         CI(II,IP+KK) = 0.0D+00
         DO 70 JJ=1,IP
      CI(II,IP+KK) = CI(II,IP+KK) +
     *     EF(JJ,IL)*(AB(II,JJ)-EL(IL)*CI(II,JJ))
   70    CONTINUE
         GR(KK) = GR(KK) + (CI(II,IP+KK)*CI(II,IP+KK))
   80 CONTINUE
      GR(KK) = SQRT(GR(KK))
      IF(SOME) THEN
         WRITE(IW,9050) ITER,EL(IL)+ECONST,GR(KK)
         CALL FLSHBF(IW)
      END IF
 4588 CONTINUE
C
      IF (SOME.AND.NTCON.GT.1) WRITE(IW,*)
      IF (ITER.EQ.NITER) THEN
         CALL TRAN(CI,NCI,MAXW1,EF,IP,EC,KST)
         GOTO 9890
      ENDIF
C
C     Check for convergence of any state, if converged, transform
C     all ip vectors in CI and Ab, modify ihmcon and ntcon.
C     Start with  ip = kst again.
C
      NUMC = 0
      DO 4255 II=1,NTCON
         IWRK2(II) = IHMCON(II)
 4255 CONTINUE
      DO 4522 KK=1,NTCON
      IL = IHMCON(KK)
      IF (GR(KK).LE.CRIT) THEN
         IF(SOME) WRITE(IW,9060) IL,EL(IL)+ECONST,IPXT
         DO 3233 II=KK-NUMC,NTCON-NUMC
         IWRK2(II) = IWRK2(II+1)
 3233    CONTINUE
         NUMC = NUMC + 1
      ENDIF
 4522 CONTINUE
C
      IF (NUMC.GT.0) THEN
         CALL TRAN(CI,NCI,MAXW1,EF,IP,EC,KST)
         CALL TRAN(AB,NCI,MAXW1,EF,IP,EC,KST)
         NTCON = NTCON - NUMC
         DO II=1,NTCON
            IHMCON(II) = IWRK2(II)
         ENDDO
         DO 74 II=1,MAXP
            DO 75 JJ=1,MAXP
               EF(II,JJ) = 0.0D+00
               EC(II,JJ) = 0.0D+00
   75       CONTINUE
            EF(II,II) = 1.0D+00
            EC(II,II) = 1.0D+00
   74    CONTINUE
         IP = KST
         IF (NTCON.NE.0) GOTO 333
         IF(SOME) WRITE(IW,*) 'ALL STATES CONVERGED.'
C
         CALL RETSPIN(NA,NB,NACT,IACON1,IACON2,IBCON1,IBCON2,
     *      ISD(1),ISD(NA+1),IDO,CI,AB,NALP,NBLP,IFA,K,NCI,SPIN,
     *       ISYMA,ISPA,ISPB,ISBS,ISBC,NSYM)
C
         IF (ANALYS.AND.SOME) THEN
            CALL ECORR(IW,NFT12,
     *                 EL,ECONST,Q(1),CI,NCI,K,SI1,SI2,NACT,NA,NB,
     *                 IFA,INDEX,IACON1,IACON2,IBCON1,IBCON2,
     *                 ISYMA,IMUL,ISPA,ISPB,
     *                 ISBS,ISBC,NSYM,IOB,NALP,NBLP)
         ENDIF
         RETURN
      ENDIF
C
      DO 68 JJ=IP+1,IP+NTCON
         IL = IHMCON(JJ-IP)
         DO 63 II=1,NCI
            CI(II,JJ) = CI(II,JJ)/(EL(IL) - Q(II))
   63    CONTINUE
   68 CONTINUE
C
C  If Ms=0, impose restriction on the CI coefficients.
C
      IF (NA.EQ.NB) THEN
      DO 1111 II=1,NALP
         ISA1 = ISYMA(II)
         ICIT = ISPA(II)
         INB = ISPB(II)
         DO 2222 INB1=ISBS(ISA1),ISBS(ISA1+1)-1
            NEND = ISBC(INB1)
            IF (NEND.GT.II) GOTO 1111
            ICI1= ICIT + ISPB(NEND)
            IF (NEND.EQ.II) THEN
               DO 3232 KJ=1,NTCON
                  NV = IHMCON(KJ)
                  IPS = (IWRK1(NV)/2)
                  IF ((IPS+IPS).NE.IWRK1(NV)) CI(ICI1,KJ+IP) = 0.0D+00
 3232          CONTINUE
               GOTO 1111
            ENDIF
C
            ICI2 = ISPA(NEND) + INB
            DO 3331 KJ=1,NTCON
            NV = IHMCON(KJ)
            IS = (-1)**IWRK1(NV)
            CI(ICI2,KJ+IP) = IS*CI(ICI1,KJ+IP)
 3331       CONTINUE
 2222    CONTINUE
 1111 CONTINUE
      ENDIF
C
C   Make the new vectors (ip+1 -> ip+ntcon).
C
C    Assume the new vectors are Bi, have to orthogonalize
C    these vectors to all others and then renormalize.
C
      DO 97 KK=IP+1,IP+NTCON
         SPIN(KK) = SPIN(IHMCON(KK-IP))
         DO 86 II=1,KK-1
            ROV = 0.0D+00
            DO 81 JJ=1,NCI
               ROV = ROV + CI(JJ,KK)*CI(JJ,II)
   81       CONTINUE
            DO 90 JJ=1,NCI
              CI(JJ,KK) = CI(JJ,KK) - ROV*CI(JJ,II)
   90       CONTINUE
   86    CONTINUE
C
         RNOR = 0.0D+00
         DO 40 II=1,NCI
            RNOR = RNOR + CI(II,KK)*CI(II,KK)
   40    CONTINUE
         RNOR = SQRT(RNOR)
         DO 42 II=1,NCI
            CI(II,KK) = CI(II,KK)/RNOR
   42    CONTINUE
   97 CONTINUE
C
      IP = IP + 1
C
C     Now to return the new part of Ab
      IF (NA.EQ.NB) THEN
       IF (NTCON.GT.1) THEN
      CALL RINAB0(SI1,SI2,NORB,NCOR,NCI,NA,NB,CI(1,IP),IACON1,IBCON1,
     *             IACON2,
     *       IFA,IPOSA,IPERA,IIND1,INDEX,AB(1,IP),NTCON,Q,UAIA,UAIB,
     *       IMMA,IMMC,
     *       ISYMA,ISYMB,ITAB,
     *       IMUL,ISPA,ISPB,
     *       ISBS,ISBC,NSYM,IOB,NALP,NBLP,
     *       IWRK1,IHMCON,ISTRB,ISTRP,ISTAR)
       ELSE
      CALL RETAB0(SI1,SI2,NORB,NCOR,NCI,NA,NB,CI(1,IP),IACON1,IBCON1,
     *             IACON2,
     *       IFA,IPOSA,IPERA,IIND1,INDEX,AB(1,IP),Q,UAIA,UAIB,
     *       IMMA,IMMC,
     *       ISYMA,ISYMB,ITAB,
     *       IMUL,ISPA,ISPB,
     *       ISBS,ISBC,NSYM,IOB,NALP,NBLP,
     *       IWRK1,IHMCON,ISTRB,ISTRP,ISTAR)
       ENDIF
      ELSE
       IF (NTCON.EQ.1) THEN
C
      CALL RETAB(SI1,SI2,NORB,NCOR,NCI,NA,NB,CI(1,IP),IACON1,IBCON1,
     *              IACON2,IFA,IPOSA,IPERA,IIND1,INDEX,AB(1,IP),
     *              Q,IMMA,IMMC,
     *       ISYMA,ISYMB,ITAB,
     *       IMUL,ISPA,ISPB,
     *       ISAS,ISBS,ISAC,ISBC,NSYM,IOB,NALP,NBLP,
     *       ISTRB,ISTRP,ISTAR)
C
      ELSE
      CALL RINAB(SI1,SI2,NORB,NCOR,NCI,NA,NB,CI(1,IP),IACON1,IBCON1,
     *             IACON2,
     *       IFA,IPOSA,IPERA,IIND1,INDEX,AB(1,IP),NTCON,Q,UAIA,UAIB,
     *       IMMA,IMMC,
     *       ISYMA,ISYMB,ITAB,
     *       IMUL,ISPA,ISPB,
     *       ISAS,ISBS,ISAC,ISBC,NSYM,IOB,NALP,NBLP,
     *      ISTRB,ISTRP,ISTAR)
      ENDIF
      ENDIF
      IP = IP + NTCON - 1
C
C  Make the new matrix elements between the CI vectors.
C
      IX = 0
      DO 103 II=1,IP
          DO 102 JJ=1,II
          IX = IX + 1
            F(IX) = 0.0D+00
            DO 115 KK=1,NCI
               F(IX) = F(IX) + CI(KK,II)*AB(KK,JJ)
  115       CONTINUE
  102    CONTINUE
  103 CONTINUE
C
C  Diagonalize small matrix
C
      CALL EVVRSP(-1,IP,IP,(IP*(IP+1))/2,MAXW1
     *              ,F,B,IWRK2,EL,EF,0,IERR)
      IF (IERR.NE.0) THEN
         WRITE(IW,*) 'ERROR IN SMALL DIAGONALIZATION'
         WRITE(IW,*) IERR
         RETURN
      ENDIF
C
C   Check to see if any states have skipped in
C
      DO 700 IJK=1,KST
         DO 705 II=1,NTCON
            IF (IJK.EQ.IHMCON(II)) GOTO 700
  705    CONTINUE
         IDXC = 0
         POV = 0.0D+00
         DO 713 JJ=1,KST
            UIT = 0.0D+00
            DO 715 KK=1,IP
               UIT = UIT + EF(KK,JJ)*EC(KK,IJK)
  715       CONTINUE
            POV = ABS(UIT)
            IDXC = JJ
  713    CONTINUE
         DO 720 II=1,NTCON
            IF (JJ.EQ.IHMCON(II)) THEN
               IHMCON(II) = IJK
               GOTO 700
            ENDIF
  720    CONTINUE
  700 CONTINUE
C
C  Check to see where the spins occur now
C
      IF (NA.EQ.NB) THEN
C
      DO 800 II=1,IP
         POV = 0.0D+00
         DO 813 JJ=1,IP
            UIT = 0.0D+00
            DO 823 KK=1,IP
               UIT = UIT + EF(KK,JJ)*EC(KK,II)
  823       CONTINUE
            IF (ABS(UIT).GT.POV) THEN
               POV = ABS(UIT)
               IDXC = JJ
            ENDIF
  813    CONTINUE
         IF (IDXC.NE.II) THEN
            GR(II) = SPIN(IDXC)
         ELSE
            GR(II) = SPIN(II)
         ENDIF
  800 CONTINUE
C
      DO 786 KK=1,IP
         SPIN(KK) = GR(KK)
  786 CONTINUE
C
C  CHECK TO SEE WHERE THE CONVERGED STATES ARE NOW.
C
      NCON = 0
      DO 805 II=1,K
         DO 850 JJ=1,NTCON
            IF (II.EQ.IHMCON(JJ)) GOTO 805
  850    CONTINUE
         DO 852 JJ=1,IP
            UIT = 0.0D0
            DO 853 KK=1,IP
               UIT = UIT + EF(KK,JJ)*EC(KK,II)
  853       CONTINUE
            IF (ABS(UIT).GT.0.6D0) THEN
               NCON = NCON + 1
               IWRK1(NCON) = JJ
               GOTO 805
            ENDIF
  852    CONTINUE
C
  805 CONTINUE
C
      NTCON1 = 0
      DO 860 II=1,K
         DO 864 JJ=1,NCON
            IF (II.EQ.IWRK1(JJ)) GOTO 860
  864    CONTINUE
         NTCON1 = NTCON1 + 1
         IHMCON(NTCON1) = II
  860 CONTINUE
C
      IF (NTCON1.NE.NTCON) THEN
         WRITE(6,*)
         WRITE(6,*) 'CONVERGED STATES HAVE SKIPPED OUT'
         WRITE(6,*) 'NUMBER OF UNCONVERGED STATES NOW = ',NTCON1
         NTCON = NTCON1
      ENDIF
C
C   END OF CHECK
C
      DO 543 II=1,KST
         IWRK1(II) = INT(SPIN(II) + 0.3D+00)
  543 CONTINUE
C
      ENDIF
C
      DO 55 II=1,IP
         DO 66  JJ=1,IP
            EC(II,JJ) = EF(II,JJ)
   66    CONTINUE
   55 CONTINUE
C
 1315 CONTINUE
C
 9890 CONTINUE
C
      IF (MASWRK) WRITE(IW,9080) IL-1
C
      CALL RETSPIN(NA,NB,NACT,IACON1,IACON2,IBCON1,IBCON2,
     *      ISD(1),ISD(NA+1),IDO,CI,AB,NALP,NBLP,IFA,K,NCI,SPIN,
     *       ISYMA,ISPA,ISPB,ISBS,ISBC,NSYM)
      ISTAT = 1
      RETURN
C
 9005 FORMAT(/1X,'ERROR, NUMBER OF VECTORS STORED=',I5,
     *           ' NOT EQUAL TO NCI=',I5/
     *        1X,'(THIS MAY BE DUE TO A GARBAGE -CIVECTR- FILE',
     *           ' LEFT OVER FROM AN EARLIER RUN.)')
 9007 FORMAT(1X,'INITIAL CI VECTORS READ FROM DISK')
 9010 FORMAT(1X,'INITIAL CI VECTOR GUESS TIME  :',F13.1)
 9020 FORMAT(1X,'INITIAL FULL CI ITERATION TIME:',F13.1)
 9040 FORMAT(/1X,'ITERATION',6X,'ENERGY',11X,'GRADIENT')
 9050 FORMAT(1X,I5,F20.10,F15.8)
 9060 FORMAT(/1X,'CONVERGED STATE',I5,' ENERGY=',F20.10,' IN',
     *           I5,' ITERS'/)
 9080 FORMAT(1X,'DETERMINANT FULL CI CONVERGED ONLY',I4,' ROOTS.')
C
      END
C*MODULE ALDECI  *DECK TRAN
C     ------------------------------------------------------
      SUBROUTINE TRAN(CI,NCI,MAXP,EF,IP,EC,KST)
C     ------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION CI(NCI,IP),EF(MAXP,KST),EC(*)
C
      DO 14 II=1,NCI
         DO 16 JJ=1,KST
            EC(JJ) = 0.0D+00
            DO 18 KK=1,IP
               EC(JJ) = EC(JJ) + CI(II,KK)*EF(KK,JJ)
   18       CONTINUE
   16    CONTINUE
         DO 17 KI=1,KST
            CI(II,KI) = EC(KI)
   17    CONTINUE
   14 CONTINUE
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK GETQ
C     --------------------------------------------------------
      SUBROUTINE GETQ(SI1,SI2,NORB,NCOR,NCI,NA,NB,IACON1,IBCON1,
     *          INDEX,NALP,NBLP,Q,
     *       ISYMA,ISYMB,ISPA,ISPB,
     *       ISAS,ISBS,ISAC,ISBC,NSYM)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION SI1(*),SI2(*),IACON1(NA+NCOR),IBCON1(NB+NCOR)
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2),Q(NCI)
      DIMENSION ISYMA(NALP),ISYMB(NBLP)
      DIMENSION ISPA(NALP),ISPB(NBLP)
      DIMENSION ISAS(NSYM+1),ISBS(NSYM+1),ISAC(NALP),ISBC(NBLP)
C
      NAT = NA + NCOR
      NBT = NB + NCOR
C
      DO 13 II=1,NCI
         Q(II) = 0.0D+00
   13 CONTINUE
      DO 30 II=1,NAT
         IACON1(II) = II
   30 CONTINUE
C
C     Big loop over Alpha
C
Cc      icat = -nblp
      DO 9000 IJK = 1, NALP
         ICAT = ISPA(IJK)
         ISA = ISYMA(IJK)
Cc         icat = icat + nblp
         C = 0.0D+00
         DO 67 II=NCOR+1,NAT
            I1 = IACON1(II)
            IND1 = INDEX(I1,I1)
            C = C + SI1(IND1)
            DO 64 JJ=1,II-1
               I2 = IACON1(JJ)
               IND2 = INDEX(I2,I2)
               INDM = INDEX(I1,I2)
               J1 = INDEX(INDM,INDM)
               J2 = INDEX(IND2,IND1)
               C = C + SI2(J2) - SI2(J1)
   64        CONTINUE
   67     CONTINUE
C
          DO 47 I=1,NBT
             IBCON1(I) = I
   47     CONTINUE
C
Cc          do 56 inb1 = 1,nblp
            NST = 1
            DO 56 INB1 = ISBS(ISA),ISBS(ISA+1)-1
               NEND = ISBC(INB1)
               DO 5510 KK=NST,NEND-1
                  CALL ADVANC(IBCON1,NBT,NORB)
 5510          CONTINUE
C
             ICIT = ICAT + ISPB(NEND)
             D = 0.0D+00
             DO 73 JJ=1,NCOR
                I2 = IBCON1(JJ)
                IND2 = INDEX(I2,I2)
                DO 74 KK=NCOR+1,NAT
                   I1 = IACON1(KK)
                   IND1 = INDEX(I1,I1)
                   J2 = INDEX(IND1,IND2)
                   D = D + SI2(J2)
   74           CONTINUE
   73        CONTINUE
C
             DO 68 JJ=NCOR+1,NBT
                I2 = IBCON1(JJ)
                IND2 = INDEX(I2,I2)
                DO 77 KK=1,NAT
                   I1 = IACON1(KK)
                   IND1 = INDEX(I1,I1)
                   J2 = INDEX(IND1,IND2)
                   D = D + SI2(J2)
   77           CONTINUE
   68        CONTINUE
             T = C + D
             Q(ICIT) = Q(ICIT) + T
Cc             call advanc(ibcon1,nbt,norb)
              NST = NEND
   56     CONTINUE
C
          IF (NBT.EQ.0) Q(IJK) = Q(IJK) + C
          CALL ADVANC(IACON1,NAT,NORB)
 9000 CONTINUE
C
C  Now for the Beta part
C
      DO 876 JJI = 1,NBT
         IBCON1(JJI) = JJI
  876 CONTINUE
C
      DO 9999 IJK = 1, NBLP
         ISB = ISYMB(IJK)
         IPB = ISPB(IJK)
         C = 0.0D+00
         DO 45 II=NCOR+1,NBT
            I1 = IBCON1(II)
            IND1 = INDEX(I1,I1)
            C = C + SI1(IND1)
            DO 54 JJ = 1,II-1
               I2 = IBCON1(JJ)
               IND2 = INDEX(I2,I2)
               INDM = INDEX(I1,I2)
               J1 = INDEX(INDM,INDM)
               J2 = INDEX(IND2,IND1)
               C = C + SI2(J2) - SI2(J1)
   54       CONTINUE
   45    CONTINUE
C
Cc         do 93 ina1 = 0,(nalp-1)
Cc            icia = ina1*nblp
Cc            icit = icia + ijk
Cc            q(icit) = q(icit) + C
Cc   93    continue
C
           DO 93 INA1 = ISAS(ISB),ISAS(ISB+1)-1
              IDA = ISAC(INA1)
              ICIA = ISPA(IDA)
              ICIT = ICIA + IPB
              Q(ICIT) = Q(ICIT) + C
   93      CONTINUE
C
         CALL ADVANC(IBCON1,NBT,NORB)
C
 9999 CONTINUE
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK AVERQ
C     -------------------------------------------------------------
      SUBROUTINE AVERQ(IW,B,NCI,NALP,NBLP,NA,NB,NCOR,NORB,IACON1,
     *              IBCON1,IACON2,IBCON2,IFA,ISD,IDO,IWRK1,MAXW1,Q)
C     -------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IACON2(NA+NCOR),IBCON2(NB+NCOR),Q(NCI)
      DIMENSION B(NCI),IACON1(NA+NCOR),IBCON1(NB+NCOR),ISD(NA+NB)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR),IWRK1(2*MAXW1)
      DIMENSION IDO(NA)
      INTEGER POSDET
C
      NACT = NORB-NCOR
C
      DO 11 II=1,NCI
         B(II) = Q(II)
   11 CONTINUE
C
      DO 13 II=1,NA
         IACON2(II) = II
   13 CONTINUE
C
      ICI = 0
      DO 8000 IJK=1,NALP
         DO 15 II=1,NB
            IBCON2(II) = II
   15    CONTINUE
         DO 7000 KJI=1,NBLP
            ICI = ICI + 1
C
C   Check to see if we have found it before
C
            IF (B(ICI).EQ.0.0D+00) GOTO 3000
C
C
C     Find doubly occupied orbitals, put in beggining of isd
C
         NSS = 0
         NSD = 0
            DO 18 II=1,NA
               IA = IACON2(II)
               DO 17 JJ=1,NB
                  IF (IA.EQ.IBCON2(JJ)) THEN
                     NSD = NSD + 1
                     ISD(NSD) = IA
                  ENDIF
   17          CONTINUE
   18       CONTINUE
C
C      Check to see if all beta orbitals are paired, means
C      that the determinant has a unique space function.
C
            IF (NSD.EQ.NB) THEN
               GOTO 3000
            ENDIF
C
C      Find singly occupied orbs now, put in end of isd, beta first
C      then alpha.
C
            DO 20 II=1,NB
               IB = IBCON2(II)
               DO 24 JJ=1,NSD
                  IF (IB.EQ.ISD(JJ)) GOTO 20
   24          CONTINUE
               NSS = NSS + 1
               ISD(NSS+NSD) = IB
   20       CONTINUE
C
            DO 30 II=1,NA
               IA = IACON2(II)
               DO 34 JJ=1,NSD
                  IF (IA.EQ.ISD(JJ)) GOTO 30
   34          CONTINUE
               NSS = NSS + 1
               ISD(NSS+NSD) = IA
   30       CONTINUE
C
C       Reorder the things.
C
            DO 40 II=1,NSS-1
               DO 42 JJ=II+1,NSS
                  IF (ISD(JJ+NSD).LT.ISD(II+NSD)) THEN
                     KK=ISD(II+NSD)
                     ISD(II+NSD) = ISD(JJ+NSD)
                     ISD(JJ+NSD) = KK
                  ENDIF
   42          CONTINUE
   40       CONTINUE
C
C
C   Find its buddies
C
            NSPA = NA-NSD
            NODE = IFA(NSS,NSPA)
C Check for memory
            IF (NODE.GT.2*MAXW1) THEN
               WRITE(IW,*) 'NOT ENOUGH MEMORY: SPECIFIED ',2*MAXW1,
     *        'NEED ',NODE
               CALL ABRT
            ENDIF
C
            DO 88 II=1,NSPA
               IDO(II) = II
   88       CONTINUE
C
            DO 5000 III=1,NODE
               DO 90 II=1,NSD
                  IACON1(II) = ISD(II)
   90          CONTINUE
               DO 105 II=1,NSPA
                  IACON1(II+NSD) = ISD(NSD+IDO(II))
  105          CONTINUE
C
C   Must reorder here.
C
               DO 140 II=1,NA-1
                  DO 142 JJ=II+1,NA
                     IF (IACON1(JJ).LT.IACON1(II)) THEN
                        KK=IACON1(II)
                        IACON1(II) = IACON1(JJ)
                        IACON1(JJ) = KK
                     ENDIF
  142             CONTINUE
  140          CONTINUE
C
               IWRK1(III) = POSDET(NACT,NA,IACON1,IFA)
               CALL ADVANC(IDO,NSPA,NSS)
 5000       CONTINUE
C
C
      NSPB = NB - NSD
      IF (NA.NE.NB) THEN
         DO 76 II=1,NSPB
            IDO(II) = II
   76    CONTINUE
         DO 4000 III = 1,NODE
            DO 190 II=1,NSD
               IBCON1(II) = ISD(II)
  190       CONTINUE
            DO 205 II=1,NSPB
               IBCON1(II+NSD) = ISD(NSD+IDO(II))
  205       CONTINUE
C
C   Must reorder here.
C
            DO 240 II=1,NB-1
               DO 242 JJ=II+1,NB
                  IF (IBCON1(JJ).LT.IBCON1(II)) THEN
                     KK=IBCON1(II)
                     IBCON1(II) = IBCON1(JJ)
                     IBCON1(JJ) = KK
                  ENDIF
  242          CONTINUE
  240       CONTINUE
C
         IWRK1(NODE+NODE-III+1) = POSDET(NACT,NB,IBCON1,IFA)
         CALL ADVANC(IDO,NSPB,NSS)
 4000 CONTINUE
C
      ELSE
C
         DO 432 II=1,NODE
            IWRK1(NODE+NODE-II+1) = IWRK1(II)
  432    CONTINUE
C
      ENDIF
C
C    Now to average all the energies
C
      TOTE = 0.0D+00
      DO 897 II=1,NODE
         IPOS = (IWRK1(II)-1)*NBLP + IWRK1(II+NODE)
         TOTE = TOTE + B(IPOS)
         B(IPOS) = 0.0D+00
         IWRK1(II) = IPOS
  897 CONTINUE
      AVEE = TOTE/NODE
      DO 898 II=1,NODE
         Q(IWRK1(II)) = AVEE
  898 CONTINUE
C
C   Done, on to next set
C
 3000       CALL ADVANC(IBCON2,NB,NACT)
C
 7000    CONTINUE
         CALL ADVANC(IACON2,NA,NACT)
 8000 CONTINUE
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK INITI
C     --------------------------------------------------------
      SUBROUTINE INITI(IW,B,NCI,NALP,NBLP,NA,NB,NCOR,NORB,IACON1,
     *         IBCON1,IACON2,IBCON2,IFA,ISD,IDO,CI,IWRK1,MAXWX,
     *         KST,INDEX,F,EL,EF,SINT1,SINT2,IWRK2,IMARK,AB,
     *         NSIZE,ISYMA,ISPA,ISPB,ISBS,ISBC,NSYM,SOME)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL SOME
      DIMENSION SINT1(*),SINT2(*),IWRK2(MAXWX),AB(NCI,KST)
      DIMENSION IACON2(NA+NCOR),IBCON2(NB+NCOR),B(8*MAXWX)
      DIMENSION IACON1(NA+NCOR),IBCON1(NB+NCOR),ISD(NA+NB)
      DIMENSION CI(NCI,KST),IFA(0:NORB-NCOR,0:NORB-NCOR),IWRK1(2*MAXWX)
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2),IDO(NA)
      DIMENSION F((MAXWX*(MAXWX+1))/2),EF(MAXWX,MAXWX),EL(MAXWX)
      DIMENSION ISYMA(NALP)
      DIMENSION ISPA(NALP),ISPB(NBLP)
      DIMENSION ISBS(NSYM+1)
      DIMENSION ISBC(NBLP)
      INTEGER POSDET
C
      MAXW1 = MAXWX
      NACT = NORB-NCOR
      IBG = 1
      IMARK = 0
      IF (NCI.LE.MAXW1) THEN
         MAXW1 = NCI
         IMARK = 1
         IF(SOME) WRITE(IW,*)
     *        'SMALL CI MATRIX, JUST USING INCORE DIAGONALIZATION...'
      ELSE
         IF(NB.EQ.0) THEN
            WRITE(IW,9020) NCI,MAXW1,NCI
            CALL ABRT
         END IF
      ENDIF
      IF (KST.GT.NCI) THEN
         IF(SOME) WRITE(IW,9010) KST,NCI
         CALL ABRT
      ENDIF
C
C   Sort of a loop structure here, keep coming
C   back to 999 until all initial determinant space <=maxw1
C   are found.
C
  999 CONTINUE
      IF (IBG.GT.MAXW1) GOTO 9999
      PMIN = 100.0D+00
      IND = 0
      DO 29 II=1,NCI
         IF (CI(II,1).LT.PMIN) THEN
            IND = II
            PMIN = CI(IND,1)
         ENDIF
   29 CONTINUE
C
C JOENBLP
      IF (NB.EQ.0) THEN
         IWRK1(IBG) = IND
         IWRK1(IBG+MAXW1) = 1
         CI(IND,1) = 101.0D+00
         IBG = IBG + 1
         GOTO 999
      ENDIF
C
      DO 13 II=1,NA
         IACON1(II) = II
   13 CONTINUE
C
      ICI = 0
      DO 100 IJK=1,NALP
         INNA = IJK
         ISA = ISYMA(IJK)
         DO 15 II=1,NB
            IBCON1(II) = II
   15    CONTINUE
C
C         nst = 1
Cc         do 50 kji=1,nblp
         DO 50 KJI = ISBS(ISA),ISBS(ISA+1)-1
            NEND = ISBC(KJI)
            INNB = NEND
C               do 5510 kk=nst,nend-1
C                  call advanc(ibcon1,nb,nact)
C 5510          continue
C
         ICI = ICI + 1
         IF (ICI.EQ.IND) GOTO 200
Cc         call advanc(ibcon1,nb,nact)
C         nst = nend
   50    CONTINUE
C
C        call advanc(iacon1,na,nact)
  100 CONTINUE
C
C
  200 CONTINUE
C
C  Now to make the alpha and beta parts
C
      DO 223 II=1,NA
         IACON1(II) = II
  223 CONTINUE
      DO 323 II=1,NB
         IBCON1(II) = II
  323 CONTINUE
      DO 423 II=1,INNA-1
         CALL ADVANC(IACON1,NA,NACT)
  423 CONTINUE
      DO 523 II=1,INNB-1
         CALL ADVANC(IBCON1,NB,NACT)
  523 CONTINUE
C
C     Find doubly occupied orbitals, put in beggining of isd
C
         NSS = 0
         NSD = 0
            DO 18 II=1,NA
               IA = IACON1(II)
               DO 17 JJ=1,NB
                  IF (IA.EQ.IBCON1(JJ)) THEN
                     NSD = NSD + 1
                     ISD(NSD) = IA
                  ENDIF
   17          CONTINUE
   18       CONTINUE
C
C      Check to see if all beta orbitals are paired.
C
      IF (NSD.EQ.NB) THEN
         IWRK1(IBG) = INNA
         IWRK1(IBG+MAXW1) = INNB
         CI(IND,1) = 101.0D+00
C         write(6,*) 'single one',ibg
C         write(6,*) iwrk1(ibg),iwrk1(ibg+maxw1)
         IBG = IBG + 1
         IF (IBG.LE.MAXW1) GOTO 999
         GOTO 9999
      ENDIF
C
C      Find singly occupied orbs now, put in end of isd, beta first
C      then alpha.
C
            DO 20 II=1,NB
               IB = IBCON1(II)
               DO 24 JJ=1,NSD
                  IF (IB.EQ.ISD(JJ)) GOTO 20
   24          CONTINUE
               NSS = NSS + 1
               ISD(NSS+NSD) = IB
   20       CONTINUE
C
            DO 30 II=1,NA
               IA = IACON1(II)
               DO 34 JJ=1,NSD
                  IF (IA.EQ.ISD(JJ)) GOTO 30
   34          CONTINUE
               NSS = NSS + 1
               ISD(NSS+NSD) = IA
   30       CONTINUE
C
C       Reorder the things.
C
            DO 40 II=1,NSS-1
               DO 42 JJ=II+1,NSS
                  IF (ISD(JJ+NSD).LT.ISD(II+NSD)) THEN
                     KK=ISD(II+NSD)
                     ISD(II+NSD) = ISD(JJ+NSD)
                     ISD(JJ+NSD) = KK
                  ENDIF
   42          CONTINUE
   40       CONTINUE
C
C     Now to store positions of all possible determinants with
C     same space function.  Alpha first.
C
      NSPA = NA-NSD
C      write(6,*) 'HERE AGAIN'
C      write(6,*) nss,nspa
      NODE = IFA(NSS,NSPA)
C      write(6,*) 'node' ,node
      IF (NODE+IBG-1.GT.MAXW1) GOTO 9999
      DO 88 II=1,NSPA
         IDO(II) = II
   88 CONTINUE
C
C      write(6,*) 'alpha'
      DO 3000 IJK=1,NODE
         DO 90 II=1,NSD
            IACON1(II) = ISD(II)
   90    CONTINUE
         DO 105 II=1,NSPA
            IACON1(II+NSD) = ISD(NSD+IDO(II))
  105    CONTINUE
C
C   Must reorder here.
C
            DO 140 II=1,NA-1
               DO 142 JJ=II+1,NA
                  IF (IACON1(JJ).LT.IACON1(II)) THEN
                     KK=IACON1(II)
                     IACON1(II) = IACON1(JJ)
                     IACON1(JJ) = KK
                  ENDIF
  142          CONTINUE
  140       CONTINUE
C
C         write(6,*) (iacon1(i),i=1,na)
         IWRK1(IJK+IBG-1) = POSDET(NACT,NA,IACON1,IFA)
         CALL ADVANC(IDO,NSPA,NSS)
 3000 CONTINUE
C
C   If we have a singlet here then the corresponding beta positions
C   are just the reverse of the alpha positions.  However,
C   if ms > 0 then we must store the beta positions after the alpha.
C
C      write(6,*) 'Beta here'
      NSPB = NB - NSD
      IF (NA.NE.NB) THEN
         DO 76 II=1,NSPB
            IDO(II) = II
   76    CONTINUE
         DO 4000 IJK = 1,NODE
            DO 190 II=1,NSD
               IBCON1(II) = ISD(II)
  190       CONTINUE
            DO 205 II=1,NSPB
               IBCON1(II+NSD) = ISD(NSD+IDO(II))
  205       CONTINUE
C
C   Must reorder here.
C
            DO 240 II=1,NB-1
               DO 242 JJ=II+1,NB
                  IF (IBCON1(JJ).LT.IBCON1(II)) THEN
                     KK=IBCON1(II)
                     IBCON1(II) = IBCON1(JJ)
                     IBCON1(JJ) = KK
                  ENDIF
  242          CONTINUE
  240       CONTINUE
C
         IWRK1(NODE+IBG-IJK+MAXW1) = POSDET(NACT,NB,IBCON1,IFA)
         CALL ADVANC(IDO,NSPB,NSS)
 4000 CONTINUE
C
      ELSE
C
         DO 432 II=1,NODE
            IWRK1(II+IBG-1+MAXW1) = IWRK1(NODE+IBG-II)
  432    CONTINUE
C
      ENDIF
C
C      do 566 ii=1,node
C         write(6,*) iwrk1(ii+ibg-1),iwrk1(ii+ibg-1+maxw1)
C  566 continue
C
C
C    Zero all diagonal elements just found
C
      DO 39 II=1,NODE
         INA = IWRK1(II+IBG-1)
         INB = IWRK1(II+IBG-1+MAXW1)
Cc         ind = (ina-1)*nblp + inb
         IND = ISPA(INA) + ISPB(INB)
         CI(IND,1) = 101.0D+00
   39 CONTINUE
C
      IBG = IBG + NODE
      GOTO 999
C
 9999 CONTINUE
      NSIZE = IBG - 1
C
C     Now to form the Hamiltonian.
C
      IXI = 0
      JI = 1
      DO 6000 IJK=1,NSIZE
C
      DO 900 II=1,NA+NCOR
         IACON1(II) = II
  900 CONTINUE
      DO 700 JJ=1,NB+NCOR
         IBCON1(JJ) = JJ
  700 CONTINUE
         DO 344 II=1,IWRK1(IJK)-1
            CALL ADVANC(IACON1,NA+NCOR,NORB)
  344    CONTINUE
         DO 355 II=1,IWRK1(IJK+MAXW1)-1
            CALL ADVANC(IBCON1,NB+NCOR,NORB)
  355    CONTINUE
C
      DO 5000 KJI = 1,IJK
         IXI = IXI + 1
         DO 676 II=1,NA+NCOR
            IACON2(II) = II
  676    CONTINUE
         DO 675 II=1,NB+NCOR
            IBCON2(II) = II
  675    CONTINUE
         DO 455 JJ=1,IWRK1(KJI)-1
            CALL ADVANC(IACON2,NA+NCOR,NORB)
  455    CONTINUE
         DO 735 II=1,IWRK1(KJI+MAXW1)-1
            CALL ADVANC(IBCON2,NB+NCOR,NORB)
  735    CONTINUE
      IJ = 0
      IF (KJI.EQ.IJK) IJ=1
C
      CALL HELEM(SINT1,SINT2,NORB,NA+NCOR,NB+NCOR,IACON1,
     *             IBCON1,IACON2,IBCON2,NCOR,IJ,JI,INDEX,ELEM)
      F(IXI) = ELEM
C
 5000 CONTINUE
 6000 CONTINUE
C
      CALL EVVRSP(-1,NSIZE,NSIZE,(NSIZE*(NSIZE+1))/2,MAXWX
     *              ,F,B,IWRK2,EL,EF,0,IERR)
      IF (IERR.NE.0) THEN
         IF(SOME) WRITE(IW,*) 'ERROR IN SMALL DIAGONALIZATION'
         IF(SOME) WRITE(IW,*) 'IERR = ',IERR
         RETURN
      ENDIF
C
C
      DO 347 II=1,NCI
         DO 450 JJ=1,KST
         CI(II,JJ) = 0.0D+00
         AB(II,JJ) = 0.0D+00
  450    CONTINUE
  347 CONTINUE
C
C *** For debuggin
C      write(6,*) (EL(i),i=1,nsize)
C      do 222 ii=1,nsize
C      write(6,'(3f20.15)') (EF(i,ii),i=1,nsize)
C  222 continue
C ***
C
      DO 799 IJK=1,KST
         DO 899 II=1,NSIZE
            KI = ISPA(IWRK1(II)) + ISPB(IWRK1(II+MAXW1))
            IF (NB.EQ.0) KI = IWRK1(II)
            CI(KI,IJK) = EF(II,IJK)
  899    CONTINUE
  799 CONTINUE
C
      RETURN
C
 9010 FORMAT(/1X,'***** ERROR *****'/
     *       1X,'INPUT NSTATE=',I4,' EXCEEDS HAMILTONIAN DIMENSION',I5)
 9020 FORMAT(/1X,'***** ERROR *****'/
     *   1X,'THIS JOB HAS NO BETA ELECTRONS, AND MORE DETERMINANTS=',I8/
     *   1X,'THAN THE INITIAL HAMILTONIAN MATRIX GUESS SIZE=',I8,'.'/
     *   1X,'PLEASE INCREASE -NHGSS- IN $DET TO ',I8,' AND RERUN.')
      END
C
C*MODULE ALDECI  *DECK HELEM
C     --------------------------------------------------------
      SUBROUTINE HELEM(SINT1,SINT2,NORB,NAELE,NBELE,IACON1,IBCON1,
     *               IACON2,IBCON2,NCOR,IJ,JI,INDEX,ELEM)
C     --------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IACON1(NAELE),IBCON1(NBELE)
      DIMENSION IACON2(NAELE),IBCON2(NBELE)
      DIMENSION SINT1(*),SINT2(*)
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2)
      INTEGER DIFF1(2),DIFF2(2),IPOS1(2),IPOS2(2)
C
C     Returns the matrix element < K | H | L > where K, L are
C     determinants.  Alpha and Beta occupations are stored in
C     iacon1, ibcon1 for K and iacon2, ibcon2 for L.
C
      ELEM = 0.0D+00
C
C
C    If Determinants are same
C
      IF (IJ.EQ.JI) THEN
         DO 200 I=1,NAELE
            IA = IACON1(I)
            I1 = INDEX(IA,IA)
            ELEM = ELEM + SINT1(I1)
            DO 197 J=I+1,NAELE
               IA1 = IACON1(J)
               I2 = INDEX(IA1,IA1)
               IT = INDEX(I1,I2)
               ELEM = ELEM + SINT2(IT)
               IC = INDEX(IA,IA1)
               IT = INDEX(IC,IC)
               ELEM = ELEM - SINT2(IT)
C
  197       CONTINUE
            DO 198 J=1,NBELE
               IB1 = IBCON1(J)
               I2 = INDEX(IB1,IB1)
               IT = INDEX(I1,I2)
               ELEM = ELEM + SINT2(IT)
  198       CONTINUE
  200    CONTINUE
C
         DO 210 I=1,NBELE
            IB = IBCON1(I)
            I1 = INDEX(IB,IB)
            ELEM = ELEM + SINT1(I1)
            DO 204 J=I+1,NBELE
               IB1 = IBCON1(J)
               I2 = INDEX(IB1,IB1)
               IT = INDEX(I1,I2)
               ELEM = ELEM + SINT2(IT)
               IC = INDEX(IB,IB1)
               IT = INDEX(IC,IC)
               ELEM = ELEM - SINT2(IT)
  204       CONTINUE
  210    CONTINUE
      RETURN
      ENDIF
C
      IDEA=0
      IDEB=0
C
C     Different orbitals in first deterinant
C
      DO 20 I=NCOR+1,NAELE
         DO 15 J=NCOR+1,NAELE
            IF (IACON1(I).EQ.IACON2(J)) GOTO 20
   15    CONTINUE
         IDEA = IDEA + 1
         IF (IDEA.GT.2) RETURN
         DIFF1(IDEA) = IACON1(I)
         IPOS1(IDEA) = I
   20 CONTINUE
C
      DO 30 I=NCOR+1,NBELE
         DO 25 J=NCOR+1,NBELE
            IF (IBCON1(I).EQ.IBCON2(J)) GOTO 30
   25    CONTINUE
         IDEB = IDEB + 1
         IF (IDEA+IDEB.GT.2) RETURN
         DIFF1(IDEA+IDEB) = IBCON1(I)
         IPOS1(IDEA+IDEB) = I
   30 CONTINUE
C
C
C    To find the different orbitals in second determinant
C
      IST = NCOR+1
      DO 63 II=1,IDEA
            DO 50 I=IST,NAELE
               DO 45 J=NCOR+1,NAELE
                  IF (IACON2(I).EQ.IACON1(J)) GOTO 50
   45          CONTINUE
               GOTO 60
   50       CONTINUE
C
   60       DIFF2(II) = IACON2(I)
            IPOS2(II) = I
            IST = I+1
   63 CONTINUE
C
      IST = NCOR+1
      DO 163 II=1,IDEB
            DO 150 I=IST,NBELE
               DO 145 J=NCOR+1,NBELE
                  IF (IBCON2(I).EQ.IBCON1(J)) GOTO 150
  145          CONTINUE
               GOTO 160
  150       CONTINUE
  160       DIFF2(II+IDEA) = IBCON2(I)
            IPOS2(II+IDEA) = I
            IST = I+1
  163 CONTINUE
C
C    If determinants differ by one orbital
C
       IF (IDEA+IDEB.EQ.1) THEN
C
C   One particle density contribution
C
          IND1 = INDEX(DIFF1(1),DIFF2(1))
          IPERM = (-1)**(IPOS1(1)-IPOS2(1))
          ELEM = ELEM + IPERM*SINT1(IND1)
C
C    Two particle density contribution
C
C    If different orbitals are alpha spin orbs
C
         IF (IDEA.EQ.1) THEN
           DO 673 K=1,NAELE
              NK = IACON1(K)
              IF (NK.EQ.DIFF1(1)) GOTO 673
              IND2 = INDEX(NK,NK)
              INDX = INDEX(IND1,IND2)
              ELEM = ELEM + SINT2(INDX)*IPERM
              I1 = INDEX(DIFF1(1),NK)
              I2 = INDEX(DIFF2(1),NK)
C              i1 = (max(d1,nk)*(max(d1,nk)-1)/2) + min(d1,nk)
C              i2 = (max(d2,nk)*(max(d2,nk)-1)/2) + min(d2,nk)
              INX = INDEX(I1,I2)
              ELEM = ELEM - IPERM*SINT2(INX)
  673     CONTINUE
C
           DO 678 K=1,NBELE
              NK = IBCON1(K)
              IND2 = INDEX(NK,NK)
              INDX = INDEX(IND1,IND2)
              ELEM = ELEM + IPERM*SINT2(INDX)
  678     CONTINUE
C
        ELSE
C
C     Different orbitals are beta spin orbs
C
           DO 732 K=1,NAELE
              NK = IACON1(K)
              IND2 = INDEX(NK,NK)
              INDX = INDEX(IND1,IND2)
              ELEM = ELEM + IPERM*SINT2(INDX)
  732      CONTINUE
C
            DO 752 K=1,NBELE
               NK = IBCON1(K)
               IF (NK.EQ.DIFF1(1)) GOTO 752
               IND2 = INDEX(NK,NK)
               INDX = INDEX(IND1,IND2)
               ELEM = ELEM + IPERM*SINT2(INDX)
              I1 = INDEX(DIFF1(1),NK)
              I2 = INDEX(DIFF2(1),NK)
              INX = INDEX(I1,I2)
              ELEM = ELEM - IPERM*SINT2(INX)
  752      CONTINUE
C
          ENDIF
          RETURN
C
      ELSE
C      return
C
C     Two orbitals are different
C     Contribution only to 2-particle density matrix.
C     Differing orbitals in diff1(1),diff1(2) for con1 and
C     diff2(1),diff2(2) for con2.  Position stored in
C     ipos1(1),ipos1(2) and ipos2(1),ipos2(2).
C
         IPERM = (-1)**(IPOS1(1)-IPOS2(1)+IPOS1(2)-IPOS2(2))
         I11 = DIFF1(1)
         I12 = DIFF2(1)
         I21 = DIFF1(2)
         I22 = DIFF2(2)
         I1 = INDEX(I11,I12)
         I2 = INDEX(I21,I22)
         INX = INDEX(I1,I2)
         ELEM = ELEM + IPERM*SINT2(INX)
C
C     If all differing orbitals are or same spin then
C     have extra matrix elements.
C
         IF (IDEA.EQ.2.OR.IDEB.EQ.2) THEN
         I1 = INDEX(I11,I22)
         I2 = INDEX(I12,I21)
         INX = INDEX(I1,I2)
         ELEM = ELEM - IPERM*SINT2(INX)
         ENDIF
      ENDIF
C
      RETURN
      END
C
C*MODULE ALDECI *DECK RETAB
C     --------------------------------------------------------
      SUBROUTINE RETAB(SI1,SI2,NORB,NCOR,NCI,NA,NB,CI,IACON1,IBCON1,
     *               IACON2,IFA,IPOSA,IPERA,IIND1,INDEX,AB,
     *               Q,IMMA,IMMC,
     *       ISYMA,ISYMB,ITAB,
     *       IMUL,ISPA,ISPB,
     *       ISAS,ISBS,ISAC,ISBC,NSYM,IOX,NALP,NBLP,
     *     ISTRB,ISTRP,ISTART)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
      DIMENSION AB(NCI)
      DIMENSION SI1(*), SI2(*), CI(NCI), IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION IACON1(NA+NCOR),IBCON1(NA+NCOR)
      DIMENSION IACON2(NA+NCOR),IPERA(NA*(NORB-NCOR-NA))
      DIMENSION IIND1(NA*(NORB-NCOR-NA))
      DIMENSION IPOSA(NA*(NORB-NCOR-NA))
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2)
      DIMENSION Q(NCI)
      DIMENSION ISYMA(NALP),ISYMB(NBLP)
      DIMENSION ITAB(NSYM),ISPA(NALP),ISPB(NBLP)
      DIMENSION IMUL(NSYM,NSYM)
      DIMENSION ISAS(NSYM+1),ISBS(NSYM+1)
      DIMENSION ISAC(NALP),ISBC(NBLP)
      DIMENSION IOX(NORB)
      DIMENSION IMMA(NSYM,(NA*(NORB-NCOR-NA)))
      DIMENSION IMMC(NSYM)
      DIMENSION ISTRB((NBLP*(NB*(NORB-NCOR-NB)))/2)
      DIMENSION ISTRP((NBLP*(NB*(NORB-NCOR-NB)))/2)
      DIMENSION ISTART(NBLP)
C
      NACT = NORB - NCOR
      NAT = NA + NCOR
      NBT = NB + NCOR
C      siac = (1.0d-07)/uaia
C      sibc = (1.0d-07)/uaib
C *********
C  Assume that we have ifa and index already calculated
C
C      call binom6(ifa,nact)
C
C      do 7 i=1,(norb*(norb+1))/2
C         do 8 j=1,i
C            index(i,j) = i*(i-1)/2 + j
C            index(j,i) = index(i,j)
C    8    continue
C    7 continue
C
C ************
C
      DO 13 II=1,NCI
          AB(II) = 0.0D+00
   13 CONTINUE
C
      DO 30 I=1,NAT
         IACON1(I) = I
   30 CONTINUE
C
C
C   Big Loop over all alpha determinants
C
Cd      icat = -nblp
      DO 9000 IJK = 1,NALP
C
C  Alpha excitations here
C   Single first
C
         IAC = 0
         DO 45 II=1,NSYM
            IMMC(II) = 0
   45    CONTINUE
Cd icat = icat + nblp
Cc Position of alpha
         ICAT = ISPA(IJK)
Cc
Cc Symmetry of alpha determinant
         ISA1 = ISYMA(IJK)
         ITAS = ITAB(ISA1)
Cc
         DO 7030 IA=NCOR+1,NAT
             IO1 = IACON1(IA)
Cc Symmetry of orbital being deoccupied
             IS1 = IOX(IO1)
Cc
             IST = IO1 + 1
             IEN = IACON1(IA+1)-1
             IF (IA.EQ.NAT) IEN=NORB
             DO 7025 KKJ=IA-NCOR+1,NA+1
                DO 7020 JJ=IST,IEN
Cc Symmetry of orbital being occupied
             IS2 = IOX(JJ)
Cc is1xis2 = ip1
             IP1 = IMUL(IS2,IS1)
C
             IAC = IAC + 1
             CALL RET1DET(IACON1,IACON2,NA,IA,JJ,NCOR,KKJ,IPER1)
C
C   Storage here for later use, well worth it timewise
C
             IPET = POSDET(NACT,NA,IACON2,IFA)
             IMMC(ISYMA(IPET)) = IMMC(ISYMA(IPET))+1
             IPOSA(IAC) = ISPA(IPET)
             IMMA(ISYMA(IPET),IMMC(ISYMA(IPET))) = IAC
             IPERA(IAC) = ((-1)**IPER1)
             IND = INDEX(JJ,IO1)
             IIND1(IAC) = IND
Cc If deoccupied and newly occupied orbitals are of different sym,
C  skip to doubles
             IF (IS1.NE.IS2) GOTO 417
C
Cc Symmetry of excited alpha is same as original, isa1
C
             C = SI1(IND)
C
             DO 412 IK=1,NAT
                IF (IK.EQ.IA) GOTO 412
                ION = IACON1(IK)
                J1 = INDEX(ION,ION)
                JJ1 = INDEX(IND,J1)
                J1 = INDEX(ION,JJ)
                J2 = INDEX(ION,IO1)
                INX = INDEX(J1,J2)
                C = C + SI2(JJ1) - SI2(INX)
  412        CONTINUE
C
             DO 49 I=1,NBT
                IBCON1(I) = I
   49        CONTINUE
C
C
Cd             do 415 inb1=1,nblp
Cc   Loop over beta dets of the right symmetry, ie itab(isa1) = itas
                NST = 1
                DO 415 INB1 = ISBS(ISA1),ISBS(ISA1+1)-1
                   NEND = ISBC(INB1)
                   DO 5510 KK=NST,NEND-1
                      CALL ADVANC(IBCON1,NBT,NORB)
 5510              CONTINUE
Cc Modified position here
                ICIT = ICAT+ISPB(NEND)
                ICI2 = IPOSA(IAC)+ISPB(NEND)
C  JOE
C         if (abs(CI(icit)).lt.siac.and.abs(CI(ici2)).lt.siac) goto 333
C
                D = 0.0D+00
                DO 790 IK=1,NBT
                   ION = IBCON1(IK)
                   J1 = INDEX(ION,ION)
                   JJ1 = INDEX(IND,J1)
                   D = D + SI2(JJ1)
  790           CONTINUE
C
                T = (C+D)*IPERA(IAC)
                AB(ICIT) = AB(ICIT) + T*CI(ICI2)
                AB(ICI2) = AB(ICI2) + T*CI(ICIT)
C
Cd                call advanc(ibcon1,nbt,norb)
Cc Added here for symmetry
                NST = NEND
  415        CONTINUE
C
  417      CONTINUE
C
C      Double excitations
C
          DO 4015 IAA = IA+1,NAT
             IPA = IAA-NCOR
             IIA = IACON1(IAA)
Cc Symmetry of orbital being deoccupied
             IS3 = IOX(IIA)
             IF (JJ.GT.IIA) IPA = IPA - 1
             ISTAA = JJ+1
             IENAA = IEN
             DO 4010 KKJAA=KKJ,NA+1
                DO 4005 JJAA=ISTAA,IENAA
Cc Symmetry of orbital being occupied
             IS4 = IOX(JJAA)
Cc ip2 = is3xis4
             IP2 = IMUL(IS4,IS3)
Cc If symmetry of alpha '' is not right, skip it.
             IF (IP1.NE.IP2) GOTO 4005
C
             CALL RET1DET(IACON2,IBCON1,NA,IPA,JJAA-NCOR,0,KKJAA,IPER2)
             IPET = POSDET(NACT,NA,IBCON1,IFA)
             ICA1 = ISPA(IPET)
             IPERT = IPER1+IPER2
             IPERT = ((-1)**IPERT)
                   I2 = INDEX(IIA,JJAA)
                   INX = INDEX(I2,IND)
                   II1 = INDEX(JJAA,IO1)
                   II2 = INDEX(IIA,JJ)
                   INX2 = INDEX(II1,II2)
                   C = SI2(INX) - SI2(INX2)
C JOE
C                 if (abs(C).lt.1.0d-07) goto 4005
                   T = C*IPERT
C
Cd    do 786 inb1 = 1,nblp
Cc  Loop over beta dets of the right symmetry, itab(isa1)
               DO 786 INB1 = ISBS(ISA1),ISBS(ISA1+1)-1
                   NEND = ISBC(INB1)
                   ICI2 = ICA1 + ISPB(NEND)
                   ICIT = ICAT + ISPB(NEND)
                   AB(ICIT) = AB(ICIT) + T*CI(ICI2)
                   AB(ICI2) = AB(ICI2) + T*CI(ICIT)
  786           CONTINUE
C
 4005           CONTINUE
                ISTAA = IACON1(KKJAA+NCOR)+1
                IENAA = IACON1(NCOR+KKJAA+1)-1
                IF (KKJAA.EQ.NA) IENAA=NORB
 4010        CONTINUE
 4015 CONTINUE
C
 7020           CONTINUE
                IST = IACON1(KKJ+NCOR)+1
                IEN = IACON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NA) IEN=NORB
 7025        CONTINUE
 7030     CONTINUE
C
C   End of pure alpha excitations
C
C   Loop over Beta dets now
C
         DO 40 I=1,NBT
            IBCON1(I) = I
   40    CONTINUE
C
         DO 8000 KJI = 1,NBLP
            ISTAR = ISTART(KJI)-1
            IPB1 = ISPB(KJI)
            ISB1 = ISYMB(KJI)
            ITBS = ITAB(ISB1)
Cc Number of matching alpha'
            IMZZ = IMMC(ITBS)
Cc Check to see if beta is of the right symmetry, ie itab(isa1) = itas
            M1 = 0
            M2 = 0
            IF (ISB1.EQ.ITAS) M1 = 1
            IF (IMZZ.NE.0) M2 = 1
            IF (M1.EQ.0.AND.M2.EQ.0) GOTO 7998
            IC1 = ICAT + IPB1
C
C   Beta first *********************** Single
C
          DO 900 IB=NCOR+1,NBT
             IBB = IBCON1(IB)
Cc Symmetry of deoccupied orbital
             IB1 = IOX(IBB)
Cc Symmetry of remaining orbital product
             IR1 = IMUL(IB1,ISB1)
             IST = IBB+1
             IEN = IBCON1(IB+1)-1
             IF (IB.EQ.NBT) IEN = NORB
             DO 895 KKJ=IB-NCOR+1,NB+1
                DO 890 JJ=IST,IEN
Cc Symmetry of occupied orbital
             IB2 = IOX(JJ)
Cc Symmetry of excited beta determinant
             ISB2 = IMUL(IR1,IB2)
             ITB2 = ITAB(ISB2)
             IMZ1 = IMMC(ITB2)
             ISTAR = ISTAR + 1
Cc Check to see if isb2 ne itas
Cc and whether m1 = 0
             IF (ISB2.NE.ITAS.AND.M1.EQ.0) GOTO 890
             IF (M2.EQ.0.AND.IMZ1.EQ.0) GOTO 890
             IF (ISB2.NE.ITAS.AND.IMZ1.EQ.0) GOTO 890
C
C               call ret1det(ibcon1,iacon2,nb,ib,jj,ncor,kkj,iper1)
C                   iposb = posdet(nact,nb,iacon2,ifa)
C                   ipb2 = ispb(iposb)
                   IPB2 = ISTRB(ISTAR)
                   IC2 = ICAT + IPB2
C   iperb = ((-1)**iper1)
                   IPERB = ISTRP(ISTAR)
                   IOB = INDEX(IBB,JJ)
C
Cd                do 1013 iat=1,iac
Cd                   ic3 = iposa(iat) + kji
Cd                   ic4 = iposa(iat) + iposb
Cd                   ind = iind1(iat)
Cd                   ix = index(ind,iob)
Cd                   C = si2(ix)*iperb*ipera(iat)
Cd                   Ab(ic1) = Ab(ic1) + C*CI(ic4)
Cd                   Ab(ic4) = Ab(ic4) + C*CI(ic1)
Cd                   Ab(ic2) = Ab(ic2) + C*CI(ic3)
Cd                   Ab(ic3) = Ab(ic3) + C*CI(ic2)
Cd 1013           continue
C
                IF (M2.EQ.0.AND.IMZ1.NE.0) THEN
                   DO 1013 IAT = 1,IMZ1
                      IJU = IMMA(ITB2,IAT)
                      IC4 = IPOSA(IJU) + IPB2
C      ind = iind1(iju)
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*IPERB*IPERA(IJU)
                      AB(IC1) = AB(IC1) + C*CI(IC4)
                      AB(IC4) = AB(IC4) + C*CI(IC1)
 1013             CONTINUE
                  GOTO 890
                ENDIF
C
                IF (M1.EQ.0.AND.ISB2.EQ.ITAS) THEN
                   DO 2013 IAT = 1,IMZZ
                      IJU = IMMA(ITBS,IAT)
                      IC3 = IPOSA(IJU) + IPB1
C      ind = iind1(iju)
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*IPERB*IPERA(IJU)
                      AB(IC2) = AB(IC2) + C*CI(IC3)
                      AB(IC3) = AB(IC3) + C*CI(IC2)
 2013              CONTINUE
                   GOTO 890
                ENDIF
C
                IF (ISB2.NE.ITAS.AND.IMZ1.NE.0) THEN
                   DO 3013 IAT = 1,IMZ1
                      IJU = IMMA(ITB2,IAT)
                      IC4 = IPOSA(IJU) + IPB2
C      ind = iind1(iju)
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*IPERB*IPERA(IJU)
                      AB(IC1) = AB(IC1) + C*CI(IC4)
                      AB(IC4) = AB(IC4) + C*CI(IC1)
 3013             CONTINUE
                  GOTO 890
                ENDIF
C
               IF (IMZ1.EQ.0.AND.ISB2.EQ.ITAS) THEN
                   DO 4013 IAT = 1,IMZZ
                      IJU = IMMA(ITBS,IAT)
                      IC3 = IPOSA(IJU) + IPB1
Cc      ind = iind1(iju)
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*IPERB*IPERA(IJU)
                      AB(IC2) = AB(IC2) + C*CI(IC3)
                      AB(IC3) = AB(IC3) + C*CI(IC2)
 4013              CONTINUE
                   GOTO 890
                ENDIF
C
                IF (IMZ1.NE.0.AND.ISB2.EQ.ITAS) THEN
                   DO 5013 IAT = 1,IMZZ
                      IJU = IMMA(ITBS,IAT)
                      IC3 = IPOSA(IJU) + IPB1
                      IC4 = IPOSA(IJU) + IPB2
C      ind = iind1(iju)
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*IPERB*IPERA(IJU)
                      AB(IC1) = AB(IC1) + C*CI(IC4)
                      AB(IC4) = AB(IC4) + C*CI(IC1)
                      AB(IC2) = AB(IC2) + C*CI(IC3)
                      AB(IC3) = AB(IC3) + C*CI(IC2)
 5013             CONTINUE
                  GOTO 890
               ENDIF
C
  890           CONTINUE
                IST = IBCON1(KKJ+NCOR)+1
                IEN = IBCON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NB) IEN=NORB
  895        CONTINUE
  900     CONTINUE
Cc Added for symmetry
 7998 CONTINUE
C
           CALL ADVANC(IBCON1,NBT,NORB)
 8000    CONTINUE
C
C  The diagonal elements are assumed to be in q.  If this
C  is not the case then you must uncomment the following
C  statements.
C
C      Diagonal contributions here
C
Cc            C = ehc
C            do 67 ii=ncor+1,nat
C               i1 = iacon1(ii)
C               ind1 = index(i1,i1)
C               C = C + si1(ind1)
C               do 64 jj=1,ii-1
C                  i2 = iacon1(jj)
C                  ind2 = index(i2,i2)
C                  indm = index(i1,i2)
C                  j1 = index(indm,indm)
C                  j2 = index(ind2,ind1)
C                  C = C + si2(j2) - si2(j1)
C   64          continue
C   67       continue
Cc
C         do 47 i=1,nbt
C            ibcon1(i) = i
C   47    continue
Cc
C             do 56 inb1 = 1,nblp
C                icit = icat+inb1
C                D = 0.0d+00
C                do 73 jj=1,ncor
C                   i2 = ibcon1(jj)
C                   ind2 = index(i2,i2)
C                   do 74 kk=ncor+1,nat
C                      i1 = iacon1(kk)
C                      ind1 = index(i1,i1)
C                      j2 = index(ind1,ind2)
C                      D = D + si2(j2)
C   74              continue
C   73           continue
Cc
C                do 68 jj=ncor+1,nbt
C                  i2 = ibcon1(jj)
C                  ind2 = index(i2,i2)
C                  do 77 kk=1,nat
C                     i1 = iacon1(kk)
C                     ind1 = index(i1,i1)
C                     j2 = index(ind1,ind2)
C                     D = D + si2(j2)
C   77             continue
C   68          continue
C               T = C+D
C               Ab(icit) = Ab(icit) + T*CI(icit)
C            call advanc(ibcon1,nbt,norb)
C   56       continue
C
         CALL ADVANC(IACON1,NAT,NORB)
 9000 CONTINUE
C
C   Now for the Beta part
C
      DO 876 JJI=1,NBT
         IBCON1(JJI) = JJI
  876 CONTINUE
C
      DO 9999 IJK = 1,NBLP
Cc Position of beta det
      ICAB = ISPB(IJK)
Cc Symmetry of beta det
      ISB1 = ISYMB(IJK)
C      itb1 = itab(isb1)
Cc
C
C   Single Beta excitations
C
      DO 6030 IB=NCOR+1,NBT
         IO1 = IBCON1(IB)
Cc Symmetry of orbital being deoccupied
         IS1 = IOX(IO1)
         IST = IO1+1
         IEN = IBCON1(IB+1)-1
         IF (IB.EQ.NBT) IEN=NORB
         DO 6025 KKJ=IB-NCOR+1,NB+1
            DO 6020 JJ=IST,IEN
Cc Symmetry of orbital being occupied
            IS2 = IOX(JJ)
Cc is1xis2 = ip1
            IP1 = IMUL(IS2,IS1)
Cc
            CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
            IPER = ((-1)**IPER1)
            IND = INDEX(JJ,IO1)
Cc   If deoccupied and newly occupied are of different symmetry skip
C    to doubles
            IF (IS1.NE.IS2) GOTO 517
            IPB1 = POSDET(NACT,NB,IACON2,IFA)
Cc New position of beta det
            IPB1 = ISPB(IPB1)
C
            C = SI1(IND)
C
            DO 912 IK=1,NBT
               IF (IK.EQ.IB) GOTO 912
               ION = IBCON1(IK)
               J1 = INDEX(ION,ION)
               JJ1 = INDEX(IND,J1)
               J1 = INDEX(ION,JJ)
               J2 = INDEX(ION,IO1)
               INX = INDEX(J1,J2)
               C = C + SI2(JJ1) - SI2(INX)
  912       CONTINUE
C
       DO 89 I=1,NAT
          IACON1(I) = I
   89 CONTINUE
C
Cd   do 920 ina1 = 0,(nalp-1)
Cc Loop over alpha dets of the right symmetry, ie itab(isb1)
            NST = 1
            DO 920 INA1 = ISAS(ISB1),ISAS(ISB1+1)-1
               NEND = ISAC(INA1)
               DO 6610 KK=NST,NEND-1
                  CALL ADVANC(IACON1,NAT,NORB)
 6610          CONTINUE
Cc Modified position here
            ICIA = ISPA(NEND)
            ICIT = ICIA + ICAB
            ICI2 = ICIA  + IPB1
C JOE
C          if (abs(CI(icit)).lt.sibc.and.abs(CI(ici2)).lt.sibc) goto 444
C
            D = 0.0D+00
             DO 690 IK=1,NAT
                ION = IACON1(IK)
                J1 = INDEX(ION,ION)
                JJ1 = INDEX(IND,J1)
                D = D + SI2(JJ1)
  690        CONTINUE
C
             T = (C+D)*IPER
             AB(ICIT) = AB(ICIT) + T*CI(ICI2)
             AB(ICI2) = AB(ICI2) + T*CI(ICIT)
C
Cd call advanc(iacon1,nat,norb)
Cc Added here for symmetry
         NST = NEND
  920    CONTINUE
C
  517 CONTINUE
C
C   Now for Beta double excitations
C
       DO 6015 IBB = IB+1,NBT
               ISTBB = JJ+1
               IENBB = IEN
               JB = IBCON1(IBB)
Cc Symmetry of orbital being deoccupied
               IS3 = IOX(JB)
               IPB = IBB-NCOR
               IF (JJ.GT.JB) IPB = IPB - 1
               DO 6010 KKJBB = KKJ,NB+1
                  DO 6005 JJBB = ISTBB,IENBB
Cc Symmetry of orbital being occupied
            IS4 = IOX(JJBB)
Cc ip2 = is3xis4
            IP2 = IMUL(IS4,IS3)
Cc If symmetry of beta '' is not right, skip it
          IF (IP1.NE.IP2) GOTO 6005
C
          CALL RET1DET(IACON2,IACON1,NB,IPB,JJBB-NCOR,0,KKJBB,IPER2)
          IBP2 = POSDET(NACT,NB,IACON1,IFA)
          IBP2 = ISPB(IBP2)
          IPER = IPER1+IPER2
          IPER = ((-1)**IPER)
               I2 = INDEX(JB,JJBB)
               INX = INDEX(I2,IND)
               II1 = INDEX(JJBB,IO1)
               II2 = INDEX(JB,JJ)
               INX2 = INDEX(II1,II2)
               C = SI2(INX) - SI2(INX2)
C JOE
C           if (abs(C).lt.1.0d-07) goto 6005
               T = C*IPER
Cd             do 686 ina1 = 0,(nalp-1)
Cc Loop over alpha dets of the right symmetry
             DO 686 INA1 = ISAS(ISB1),ISAS(ISB1+1)-1
                NEND = ISAC(INA1)
             ICIA = ISPA(NEND)
             ICIT = ICIA + ICAB
             ICI2 = ICIA + IBP2
             AB(ICIT) = AB(ICIT) + T*CI(ICI2)
             AB(ICI2) = AB(ICI2) + T*CI(ICIT)
  686       CONTINUE
C
C
 6005          CONTINUE
               ISTBB = IBCON1(KKJBB+NCOR)+1
               IENBB = IBCON1(NCOR+KKJBB+1)-1
               IF (KKJBB.EQ.NB) IENBB=NORB
 6010      CONTINUE
 6015 CONTINUE
C
 6020       CONTINUE
            IST = IBCON1(KKJ+NCOR)+1
            IEN=IBCON1(NCOR+KKJ+1)-1
            IF (KKJ.EQ.NB) IEN=NORB
 6025     CONTINUE
 6030 CONTINUE
C
C  Again, the diagonal elements are assumed to be in q.  If
C  this is not the case then you must uncomment the following
C  statements.
C
C    Remaining part of diagonal contributions
C
Cc            C = 0.0d+00
C            do 45 ii=ncor+1,nbt
C               i1 = ibcon1(ii)
C               ind1 = index(i1,i1)
C               C = C + si1(ind1)
C               do 54 jj=1,ii-1
C                  i2 = ibcon1(jj)
C                  ind2 = index(i2,i2)
C                  indm = index(i1,i2)
C                  j1 = index(indm,indm)
C                  j2 = index(ind2,ind1)
C                  C = C + si2(j2) - si2(j1)
C   54          continue
C   45       continue
C
C            do 93 ina1 = 0,(nalp-1)
C              icia = ina1*nblp
C              icit = icia + ijk
C              Ab(icit) = Ab(icit) + C*CI(icit)
C   93       continue
C
           CALL ADVANC(IBCON1,NBT,NORB)
C
 9999 CONTINUE
C
C   Now for the diagonal contributions
C
C     If q does not contain the diagaonal elements
C     then various statements above must be uncommented.
C     They should be easy to find.
C
      DO 119 IJK=1,NCI
         AB(IJK) = AB(IJK) + Q(IJK)*CI(IJK)
  119 CONTINUE
C
      RETURN
      END
C*MODULE ALDECI  *DECK RET1DET
C     --------------------------------------------------------
      SUBROUTINE RET1DET(IBCON1,IBCON2,NB,IB,JJ,NCOR,KKJ,IPER)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IBCON1(*),IBCON2(*)
C
      IF (JJ.LT.IBCON1(IB)) THEN
         DO 870 KI=1,KKJ-1
               IBCON2(KI) = IBCON1(KI+NCOR)-NCOR
  870    CONTINUE
         IBCON2(KKJ) = JJ-NCOR
         IPR = KKJ
         DO 875 KI=KKJ,NB
              IF (KI+NCOR.EQ.IB) GOTO 875
            IPR = IPR+1
            IBCON2(IPR) = IBCON1(KI+NCOR)-NCOR
  875    CONTINUE
         IPER = IB-KKJ-NCOR
      ELSE
         IPR = 0
         DO 880 KI=1,KKJ-1
            IF (KI+NCOR.EQ.IB) GOTO 880
              IPR = IPR + 1
              IBCON2(IPR) = IBCON1(KI+NCOR)-NCOR
  880    CONTINUE
            IBCON2(KKJ-1) = JJ-NCOR
            DO 885 KI=KKJ,NB
                 IBCON2(KI) = IBCON1(KI+NCOR)-NCOR
  885       CONTINUE
            IPER = KKJ-1-IB+NCOR
      ENDIF
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK POSDET
C     -------------------------------------------
      INTEGER FUNCTION POSDET(NACT,NOEL,CON,IFA)
C     -------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER CON(NOEL),POS1,I,J
      DIMENSION IFA(0:NACT,0:NACT)
C
      POS1 = 0
      POSDET = 1
      DO 33 I=1,NOEL
         DO 55 J=POS1+1,CON(I)-1
            POSDET = POSDET + IFA(NACT-J,NOEL-I)
   55    CONTINUE
         POS1 = CON(I)
   33 CONTINUE
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK ADVANC
C     ---------------------------------------------
      SUBROUTINE ADVANC(CON,NELE,NORB)
C     ---------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER CON(*)
C
      IF (CON(NELE).EQ.NORB) THEN
         DO 50 I=NELE-1,1,-1
            IF (CON(I+1)-CON(I).GT.1) THEN
               CON(I) = CON(I) + 1
               DO 40 J=I+1,NELE
                  CON(J) = CON(J-1) + 1
   40          CONTINUE
               RETURN
            ENDIF
   50    CONTINUE
      ENDIF
C
         CON(NELE) = CON(NELE)+1
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK RETSPIN
C     ----------------------------------------------------------
      SUBROUTINE RETSPIN(NA,NB,NACT,IACON1,IACON2,IBCON1,IBCON2,
     *                   ISA,ISB,ISD,CI,SVEC,NALP,NBLP,IFA,NV,NCI,
     *                   SPIN,ISYMA,ISPA,ISPB,ISBS,ISBC,NSYM)
C     ----------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
      DIMENSION CI(NCI,NV),SVEC(NCI,NV)
      DIMENSION IACON1(NA),IBCON1(NB)
      DIMENSION IACON2(NA),IBCON2(NB)
      DIMENSION ISA(NA),ISB(NB),ISD(NB)
      DIMENSION IFA(0:NACT,0:NACT)
      DIMENSION SPIN(NV)
C Symmetry arrays
      DIMENSION ISYMA(NALP)
      DIMENSION ISPA(NALP),ISPB(NBLP)
      DIMENSION ISBS(NSYM+1)
      DIMENSION ISBC(NBLP)
C
C     Returns spin of CI vector i in spin(i) for full space
C     determinantal wavefunction.  Must compile with subroutine
C     posdet which is located in rinAb.f
C
C     na,nb   Number of active alpha and beta electrons
C     nact    Number of active orbitals
C     iacon1,iacon2,ibcon1,ibcon2,isa,isb,isd scratch integer
C             arrays
C     CI      CI vectors
C     svec    Scratch matrix
C     ifa     Contains binomial coefficients, must have been
C             worked out previously using binom6.f
C     nv,nci  Number of vectors and size of CI respectively
C     spin(i) will contain the spin of vector i
C     Remaining data is just symmetry information.  This should
C     be used in conjunction with subroutine davci.  If you
C     want to use it outside, look at subroutine symwrk, the
C     names of data above match those in symwrk, and that is
C     where they are initially worked out.
C
      DO 43 II=1,NV
         SPIN(II) =((NA-NB)/2.0D+00)**2.0D+00 + (NA + NB)/2.0D+00
   43 CONTINUE
C
      DO 13 II=1,NA
         IACON1(II) = II
   13 CONTINUE
      ICI1 = 0
      ICT = NCI
      DO 450 II=1,ICT
         DO 550 JJ=1,NV
            SVEC(II,JJ) = 0.0D+00
  550    CONTINUE
  450 CONTINUE
C
      DO 6000 IJK=1,NALP
         ICAT = ISPA(IJK)
         ISA1 = ISYMA(IJK)
C itas = itab(isa1)
         DO 15 JJ=1,NB
            IBCON1(JJ) = JJ
   15    CONTINUE
         NST = 1
         DO 5800 KJI=ISBS(ISA1),ISBS(ISA1+1)-1
            NEND = ISBC(KJI)
            DO 5510 KK=NST,NEND-1
               CALL ADVANC(IBCON1,NB,NACT)
 5510       CONTINUE
C
            ICI1 = ICAT + ISPB(NEND)
C
            INMN = 0
            DO 877 NMN=1,NV
               IF (ABS(CI(ICI1,NMN)).LT.1.0D-10) INMN=INMN+1
  877       CONTINUE
            NST = NEND
            IF (INMN.EQ.NV) GOTO 5800
C
C
            IXA = 0
            NSD = 0
            DO 30 II=1,NA
               IAO = IACON1(II)
               DO 20 JJ=1,NB
                  IF (IAO.EQ.IBCON1(JJ)) GOTO 60
   20          CONTINUE
               IXA = IXA + 1
               ISA(IXA) = II
               GOTO 30
   60          NSD = NSD + 1
               ISD(NSD) = IAO
   30       CONTINUE
C
            IDB = 0
            IXB = NB-NSD
            DO 40 II=1,NB
               IBO = IBCON1(II)
               DO 50 JJ=1,NSD
                  IF (IBO.EQ.ISD(JJ)) GOTO 40
   50          CONTINUE
               IDB = IDB + 1
               ISB(IDB) = II
               IF (IDB.EQ.IXB) GOTO 90
   40       CONTINUE
   90       CONTINUE
C
            DO 53 II=1,NV
            SPIN(II) = SPIN(II) - (CI(ICI1,II)**2.0D+00)*NSD
   53       CONTINUE
C
            DO 100 III=1,IXA
               KA = ISA(III)
               IA1 = IACON1(KA)
               DO 200 JJJ=1,IXB
                  KB = ISB(JJJ)
                  IB1 = IBCON1(KB)
C
                  DO 110 IJ=1,NA
                     IACON2(IJ) = IACON1(IJ)
  110             CONTINUE
                  DO 120 IJ=1,NB
                     IBCON2(IJ) = IBCON1(IJ)
  120             CONTINUE
                  IACON2(KA) = IB1
                  IBCON2(KB) = IA1
C
                  DO 130 I=1,NA
                     IF (IACON2(I).GT.IB1) GOTO 135
  130             CONTINUE
  135             CONTINUE
                  DO 140 J=1,NB
                     IF (IBCON2(J).GT.IA1) GOTO 145
  140             CONTINUE
  145             CONTINUE
                  CALL REORD(I,KA,IACON2,IB1,IP1)
                  CALL REORD(J,KB,IBCON2,IA1,IP2)
                  IPT = IP1+IP2+1
                  IPER = (-1)**IPT
                  ICA2 = POSDET(NACT,NA,IACON2,IFA)
                  ICB2 = POSDET(NACT,NB,IBCON2,IFA)
                  ICI2 = ISPA(ICA2) + ISPB(ICB2)
                  DO 253 KKK=1,NV
                  SVEC(ICI1,KKK) = SVEC(ICI1,KKK) + CI(ICI2,KKK)*IPER
  253             CONTINUE
  200          CONTINUE
  100       CONTINUE
C
         NST = NEND
 5800    CONTINUE
         CALL ADVANC(IACON1,NA,NACT)
 6000 CONTINUE
C
      DO 340 II=1,ICT
         DO 440 JJ=1,NV
            SPIN(JJ) = SPIN(JJ) + CI(II,JJ)*SVEC(II,JJ)
  440    CONTINUE
  340 CONTINUE
C
      DO 670 II=1,NV
         SRT = SQRT(4.0D+00*ABS(SPIN(II)) + 1.0D+00)
         SPIN(II) = (SRT-1.0D+00)/2.0D+00
  670 CONTINUE
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK REORD
C     -----------------------------------------
      SUBROUTINE REORD(I,K,ICON,IORB,IPER)
C     -----------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION ICON(*)
      IF (I-1.EQ.K) THEN
         IPER = 0
         RETURN
      ELSEIF (I.LT.K) THEN
         DO 13 II=K,I+1,-1
            ICON(II) = ICON(II-1)
   13    CONTINUE
         ICON(I) = IORB
         IPER = K-I
         RETURN
      ELSEIF (I-1.GT.K) THEN
         DO 113 II=K,I-2
            ICON(II) = ICON(II+1)
  113    CONTINUE
         ICON(I-1) = IORB
         IPER = I-K-1
      ENDIF
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK RINAB
C     --------------------------------------------------------
      SUBROUTINE RINAB(SI1,SI2,NORB,NCOR,NCI,NA,NB,CI,IACON1,IBCON1,
     *               IACON2,IFA,IPOSA,IPERA,IIND1,INDEX,AB,NV,Q,
     *               UAIA,UAIB,IMMA,IMMC,
     *       ISYMA,ISYMB,ITAB,
     *       IMUL,ISPA,ISPB,
     *       ISAS,ISBS,ISAC,ISBC,NSYM,IOX,NALP,NBLP,
     *   ISTRB,ISTRP,ISTART)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
      DIMENSION AB(NCI,NV)
      DIMENSION SI1(*), SI2(*), CI(NCI,NV), IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION IACON1(NA+NCOR),IBCON1(NA+NCOR)
      DIMENSION IACON2(NA+NCOR),IPERA(NA*(NORB-NCOR-NA))
      DIMENSION IIND1(NA*(NORB-NCOR-NA))
      DIMENSION IPOSA(NA*(NORB-NCOR-NA))
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2)
      DIMENSION Q(NCI)
      DIMENSION ISYMA(NALP),ISYMB(NBLP)
      DIMENSION ITAB(NSYM),ISPA(NALP),ISPB(NBLP)
      DIMENSION IMUL(NSYM,NSYM)
      DIMENSION ISAS(NSYM+1),ISBS(NSYM+1)
      DIMENSION ISAC(NALP),ISBC(NBLP)
      DIMENSION IOX(NORB)
      DIMENSION IMMA(NSYM,(NA*(NORB-NCOR-NA)))
      DIMENSION IMMC(NSYM)
      DIMENSION ISTRB((NBLP*(NB*(NORB-NCOR-NB)))/2)
      DIMENSION ISTRP((NBLP*(NB*(NORB-NCOR-NB)))/2)
      DIMENSION ISTART(NBLP)
C
      UAIA = 0.0D+00
      UAIB = 0.0D+00
      NACT = NORB - NCOR
      NAT = NA + NCOR
      NBT = NB + NCOR
C
C *********
C  Assume that we have ifa and index already calculated
C
C      call binom6(ifa,nact)
C
C      do 7 i=1,(norb*(norb+1))/2
C         do 8 j=1,i
C            index(i,j) = i*(i-1)/2 + j
C            index(j,i) = index(i,j)
C    8    continue
C    7 continue
C
C ************
C
      DO 13 II=1,NCI
          DO 12 JJ=1,NV
             AB(II,JJ) = 0.0D+00
   12     CONTINUE
   13 CONTINUE
C
      DO 30 I=1,NAT
         IACON1(I) = I
   30 CONTINUE
C
C   Big Loop over all alpha determinants
C
Cd      icat = -nblp
      DO 9000 IJK = 1,NALP
C
C  Alpha excitations here
C   Single first
C
         DO 45 II=1,NSYM
            IMMC(II) = 0
   45    CONTINUE
         IAC = 0
Cd icat = icat + nblp
Cc Postion of alpha
         ICAT = ISPA(IJK)
Cc
Cc Symetry of alpha determinant
         ISA1 = ISYMA(IJK)
         ITAS = ITAB(ISA1)
Cc
         DO 7030 IA=NCOR+1,NAT
             IO1 = IACON1(IA)
Cc Symmetry of orbital being deoccupied
             IS1 = IOX(IO1)
Cc
             IST = IO1 + 1
             IEN = IACON1(IA+1)-1
             IF (IA.EQ.NAT) IEN=NORB
             DO 7025 KKJ=IA-NCOR+1,NA+1
                DO 7020 JJ=IST,IEN
Cc Symmetry of orbital being occupied
             IS2 = IOX(JJ)
Cc is1xis2 = ip1
             IP1 = IMUL(IS2,IS1)
Cc
             IAC = IAC + 1
             CALL RET1DET(IACON1,IACON2,NA,IA,JJ,NCOR,KKJ,IPER1)
C
C   Storage here for later use, well worth it timewise
C
             IPET = POSDET(NACT,NA,IACON2,IFA)
Cd    iposa(iac) = (ipet-1)*nblp
Cc New starting position of alpha determinant
             IMMC(ISYMA(IPET)) = IMMC(ISYMA(IPET)) + 1
             IPOSA(IAC) = ISPA(IPET)
             IMMA(ISYMA(IPET),IMMC(ISYMA(IPET))) = IAC
Cc
             IPERA(IAC) = ((-1)**IPER1)
             IND = INDEX(JJ,IO1)
             IIND1(IAC) = IND
Cc  If deoccupied and newly occupied orbs are of different symmetry,
C   skip to doubles
             IF (IS1.NE.IS2) GOTO 417
C
Cc  Symmetry of excited alpha is same as original, Isa1
C
             C = SI1(IND)
C
             DO 412 IK=1,NAT
                IF (IK.EQ.IA) GOTO 412
                ION = IACON1(IK)
                J1 = INDEX(ION,ION)
                JJ1 = INDEX(IND,J1)
                J1 = INDEX(ION,JJ)
                J2 = INDEX(ION,IO1)
                INX = INDEX(J1,J2)
                C = C + SI2(JJ1) - SI2(INX)
  412        CONTINUE
C
             DO 49 I=1,NBT
                IBCON1(I) = I
   49        CONTINUE
C
C
Cd             do 415 inb1=1,nblp
Cc   Loop over beta dets, of the right symmetry, ie itab(isa1)
               NST = 1
               DO 415 INB1=ISBS(ISA1),ISBS(ISA1+1)-1
                  NEND = ISBC(INB1)
                  DO 5510 KK=NST,NEND-1
                     CALL ADVANC(IBCON1,NBT,NORB)
 5510             CONTINUE
C
Cc  Modified position here
                ICIT = ICAT+ISPB(NEND)
                ICI2 = IPOSA(IAC)+ISPB(NEND)
Cc
                D = 0.0D+00
                DO 790 IK=1,NBT
                   ION = IBCON1(IK)
                   J1 = INDEX(ION,ION)
                   JJ1 = INDEX(IND,J1)
                   D = D + SI2(JJ1)
  790           CONTINUE
C
                T = (C+D)*IPERA(IAC)
C
                IF (ABS(T).GT.UAIA) UAIA = ABS(T)
                DO 44 KJ = 1,NV
                   AB(ICIT,KJ) = AB(ICIT,KJ) + T*CI(ICI2,KJ)
                   AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICIT,KJ)
   44           CONTINUE
C
Cd                call advanc(ibcon1,nbt,norb)
Cc Added here for symmetry
                NST = NEND
  415        CONTINUE
C
  417     CONTINUE
C
C      Double excitations
C
          DO 4015 IAA = IA+1,NAT
             IPA = IAA-NCOR
             IIA = IACON1(IAA)
Cc Symmetry of orbital being deoccupied
             IS3 = IOX(IIA)
             IF (JJ.GT.IIA) IPA = IPA - 1
             ISTAA = JJ+1
             IENAA = IEN
             DO 4010 KKJAA=KKJ,NA+1
                DO 4005 JJAA=ISTAA,IENAA
Cc Symmetry of orbital being occupied
             IS4 = IOX(JJAA)
Cc ip2 = is3xis4
             IP2 = IMUL(IS4,IS3)
Cc If symmetry of alpha '' is not right, skip it.
             IF (IP1.NE.IP2) GOTO 4005
Cc
C
             CALL RET1DET(IACON2,IBCON1,NA,IPA,JJAA-NCOR,0,KKJAA,IPER2)
             IPET = POSDET(NACT,NA,IBCON1,IFA)
             ICA1 = ISPA(IPET)
             IPERT = IPER1+IPER2
             IPERT = ((-1)**IPERT)
C
                   I2 = INDEX(IIA,JJAA)
                   INX = INDEX(I2,IND)
                   II1 = INDEX(JJAA,IO1)
                   II2 = INDEX(IIA,JJ)
                   INX2 = INDEX(II1,II2)
                   C = SI2(INX) - SI2(INX2)
                   T = C*IPERT
C
Cd   do 786 inb1 = 1,nblp
Cc  Loop over beta dets of the right symmetry, itab(isa1)
               DO 786 INB1 = ISBS(ISA1),ISBS(ISA1+1)-1
                   NEND = ISBC(INB1)
                   ICI2 = ICA1 + ISPB(NEND)
                   ICIT = ICAT+ISPB(NEND)
                   DO 55 KJ = 1,NV
                      AB(ICIT,KJ) = AB(ICIT,KJ) + T*CI(ICI2,KJ)
                      AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICIT,KJ)
   55              CONTINUE
  786           CONTINUE
C
 4005           CONTINUE
                ISTAA = IACON1(KKJAA+NCOR)+1
                IENAA = IACON1(NCOR+KKJAA+1)-1
                IF (KKJAA.EQ.NA) IENAA=NORB
 4010        CONTINUE
 4015 CONTINUE
C
 7020           CONTINUE
                IST = IACON1(KKJ+NCOR)+1
                IEN = IACON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NA) IEN=NORB
 7025        CONTINUE
 7030     CONTINUE
C
C   End of pure alpha excitations
C
C   Loop over Beta dets now
C
         DO 40 I=1,NBT
            IBCON1(I) = I
   40    CONTINUE
C
C ***
         DO 8000 KJI = 1,NBLP
          ISTAR = ISTART(KJI)-1
          IPB1 = ISPB(KJI)
          ISB1 = ISYMB(KJI)
          ITBS = ITAB(ISB1)
Cc Number of matching alpha'
          IMZZ = IMMC(ITBS)
Cc Check to see if beta is of right sym, ie itab(isa1) = itas
          M1 = 0
          M2 = 0
          IF (ISB1.EQ.ITAS) M1 = 1
          IF (IMZZ.NE.0) M2 = 1
          IF (M1.EQ.0.AND.M2.EQ.0) GOTO 7998
          IC1 = ICAT + IPB1
C
C   Beta first *********************** Single
C
          DO 900 IB=NCOR+1,NBT
             IBB = IBCON1(IB)
Cc Symmetry of deoccupied orbital
             IB1 = IOX(IBB)
Cc Symmetry of remaining orbital product
             IR1 = IMUL(IB1,ISB1)
             IST = IBB+1
             IEN = IBCON1(IB+1)-1
             IF (IB.EQ.NBT) IEN = NORB
             DO 895 KKJ=IB-NCOR+1,NB+1
                DO 890 JJ=IST,IEN
Cc Symmetry of occupied orbital
            IB2 = IOX(JJ)
Cc Symmetry of excited beta determinant
            ISB2 = IMUL(IR1,IB2)
            ITB2 = ITAB(ISB2)
Cc Check to see if isb2 ne itas
Cc and whether m1 = 0
            IMZ1 = IMMC(ITB2)
            ISTAR = ISTAR + 1
            IF (ISB2.NE.ITAS.AND.M1.EQ.0) GOTO 890
            IF (M2.EQ.0.AND.IMZ1.EQ.0) GOTO 890
            IF (ISB2.NE.ITAS.AND.IMZ1.EQ.0) GOTO 890
Cc
C               call ret1det(ibcon1,iacon2,nb,ib,jj,ncor,kkj,iper1)
C                   iposb = posdet(nact,nb,iacon2,ifa)
C                   ipb2 = ispb(iposb)
                   IPB2 = ISTRB(ISTAR)
                   IC2 = ICAT + IPB2
C           iperb = ((-1)**iper1)
                   IPERB = ISTRP(ISTAR)
                   IOB = INDEX(IBB,JJ)
C
Cd                do 1013 iat=1,iac
Cd                   ic3 = iposa(iat) + kji
Cd                   ic4 = iposa(iat) + iposb
Cd                   ind = iind1(iat)
Cd                   ix = index(ind,iob)
Cd                   C = si2(ix)*iperb*ipera(iat)
Cd                   do 66 kj = 1,nv
Cd                      Ab(ic1,kj) = Ab(ic1,kj) + C*CI(ic4,kj)
Cd                      Ab(ic4,kj) = Ab(ic4,kj) + C*CI(ic1,kj)
Cd                      Ab(ic2,kj) = Ab(ic2,kj) + C*CI(ic3,kj)
Cd                      Ab(ic3,kj) = Ab(ic3,kj) + C*CI(ic2,kj)
Cd   66              continue
Cd 1013           continue
C
Cc
                IF (M2.EQ.0.AND.IMZ1.NE.0) THEN
                   DO 1013 IAT = 1,IMZ1
                      IJU = IMMA(ITB2,IAT)
                      IC4 = IPOSA(IJU) + IPB2
C      ind = iind1(iju)
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*IPERB*IPERA(IJU)
                      DO 66 KJ = 1,NV
                         AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                         AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
   66                 CONTINUE
 1013              CONTINUE
                   GOTO 890
                ENDIF
C
                IF (M1.EQ.0.AND.ISB2.EQ.ITAS) THEN
                   DO 2013 IAT=1,IMZZ
                      IJU = IMMA(ITBS,IAT)
                      IC3 = IPOSA(IJU) + IPB1
C      ind = iind1(iju)
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*IPERB*IPERA(IJU)
                      DO 76 KJ=1,NV
                         AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                         AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
   76                 CONTINUE
 2013              CONTINUE
                   GOTO 890
                   ENDIF
C
                IF (ISB2.NE.ITAS.AND.IMZ1.NE.0) THEN
                   DO 3013 IAT = 1,IMZ1
                      IJU = IMMA(ITB2,IAT)
                      IC4 = IPOSA(IJU) + IPB2
C      ind = iind1(iju)
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*IPERB*IPERA(IJU)
                      DO 86 KJ = 1,NV
                         AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                         AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
   86                 CONTINUE
 3013              CONTINUE
                   GOTO 890
                ENDIF
C
                IF (IMZ1.EQ.0.AND.ISB2.EQ.ITAS) THEN
                      DO 4013 IAT=1,IMZZ
                      IJU = IMMA(ITBS,IAT)
                      IC3 = IPOSA(IJU) + IPB1
Cc      ind = iind1(iju)
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*IPERB*IPERA(IJU)
                      DO 96 KJ=1,NV
                         AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                         AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
   96                 CONTINUE
 4013              CONTINUE
                   GOTO 890
                ENDIF
C
                IF (IMZ1.NE.0.AND.ISB2.EQ.ITAS) THEN
                   DO 5013 IAT=1,IMZZ
                      IJU = IMMA(ITBS,IAT)
                      IC3 = IPOSA(IJU) + IPB1
                      IC4 = IPOSA(IJU) + IPB2
C      ind = iind1(iju)
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*IPERB*IPERA(IJU)
                      DO 106 KJ = 1,NV
                         AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                         AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                         AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                         AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
  106                 CONTINUE
 5013              CONTINUE
                   GOTO 890
                ENDIF
C
C
  890           CONTINUE
                IST = IBCON1(KKJ+NCOR)+1
                IEN = IBCON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NB) IEN=NORB
  895        CONTINUE
  900     CONTINUE
C
Cc Added for symmetry
 7998 CONTINUE
           CALL ADVANC(IBCON1,NBT,NORB)
 8000    CONTINUE
C
C   The diagonal elements are assumed to be in q, if this is
C   not the case then the following statements must be
C   uncommented.
C
C      Diagonal contributions here
C
C            C = ehc
C
C            do 67 ii=ncor+1,nat
C               i1 = iacon1(ii)
C               ind1 = index(i1,i1)
C               C = C + si1(ind1)
C               do 64 jj=1,ii-1
C                  i2 = iacon1(jj)
C                  ind2 = index(i2,i2)
C                  indm = index(i1,i2)
C                  j1 = index(indm,indm)
C                  j2 = index(ind2,ind1)
C                  C = C + si2(j2) - si2(j1)
C   64          continue
C   67       continue
C
C         do 47 i=1,nbt
C            ibcon1(i) = i
C   47    continue
C
C             do 56 inb1 = 1,nblp
C                icit = icat+inb1
C                D = 0.0d+00
C                do 113 jj=1,ncor
C                  i2 = ibcon1(jj)
C                  ind2 = index(i2,i2)
C                  do 117 kk=ncor+1,nat
C                     i1 = iacon1(kk)
C                     ind1 = index(i1,i1)
C                     j2 = index(ind1,ind2)
C                     D = D + si2(j2)
C  117             continue
C  113           continue
C                do 68 jj=ncor+1,nbt
C                  i2 = ibcon1(jj)
C                  ind2 = index(i2,i2)
C                  do 77 kk=1,nat
C                     i1 = iacon1(kk)
C                     ind1 = index(i1,i1)
C                     j2 = index(ind1,ind2)
C                     D = D + si2(j2)
C   77             continue
C   68          continue
C               T = C+D
C               do 88 kj = 1,nv
C                  Ab(icit,kj) = Ab(icit,kj) + T*CI(icit,kj)
C   88          continue
C            call advanc(ibcon1,nbt,norb)
C   56       continue
C
         CALL ADVANC(IACON1,NAT,NORB)
 9000 CONTINUE
C
C   Now for the Beta part
C
      DO 876 JJI=1,NBT
         IBCON1(JJI) = JJI
  876 CONTINUE
C
      DO 9999 IJK = 1,NBLP
Cc Position of beta det
      ICAB = ISPB(IJK)
Cc Symmetry of beta det
      ISB1 = ISYMB(IJK)
C      itb1 = itab(isb1)
Cc
C
C   Single Beta excitations
C
      DO 6030 IB=NCOR+1,NBT
         IO1 = IBCON1(IB)
Cc Symmetry of orbital being deoccupied
         IS1 = IOX(IO1)
Cc
         IST = IO1+1
         IEN = IBCON1(IB+1)-1
         IF (IB.EQ.NBT) IEN=NORB
         DO 6025 KKJ=IB-NCOR+1,NB+1
            DO 6020 JJ=IST,IEN
Cc Symmetry of orbital being occupied
         IS2 = IOX(JJ)
Cc is1xis2 = ip1
         IP1 = IMUL(IS2,IS1)
Cc
            CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
            IPER = ((-1)**IPER1)
            IND = INDEX(JJ,IO1)
Cc If deoccupied and newly occupied are of different symmetry,
C  skip to doubles
         IF (IS1.NE.IS2) GOTO 517
            IPB1 = POSDET(NACT,NB,IACON2,IFA)
Cc New position of beta det
            IPB1 = ISPB(IPB1)
Cc
C
            C = SI1(IND)
C
            DO 912 IK=1,NBT
               IF (IK.EQ.IB) GOTO 912
               ION = IBCON1(IK)
               J1 = INDEX(ION,ION)
               JJ1 = INDEX(IND,J1)
               J1 = INDEX(ION,JJ)
               J2 = INDEX(ION,IO1)
               INX = INDEX(J1,J2)
               C = C + SI2(JJ1) - SI2(INX)
  912       CONTINUE
C
       DO 89 I=1,NAT
          IACON1(I) = I
   89 CONTINUE
C
C
Cd          do 920 ina1 = 0,(nalp-1)
Cc  Loop over alpha dets of the right symmetry, ie itab(isb1)
          NST = 1
          DO 920 INA1 = ISAS(ISB1),ISAS(ISB1+1)-1
           NEND = ISAC(INA1)
          DO 6610 KK=NST,NEND-1
             CALL ADVANC(IACON1,NAT,NORB)
 6610     CONTINUE
C
Cc Modified position here
            ICIA = ISPA(NEND)
            ICIT = ICIA + ICAB
            ICI2 = ICIA  + IPB1
            D = 0.0D+00
             DO 690 IK=1,NAT
                ION = IACON1(IK)
                J1 = INDEX(ION,ION)
                JJ1 = INDEX(IND,J1)
                D = D + SI2(JJ1)
  690        CONTINUE
C
             T = (C+D)*IPER
             IF (ABS(T).GT.UAIB) UAIB = ABS(T)
             DO 87 KJ = 1,NV
                AB(ICIT,KJ) = AB(ICIT,KJ) + T*CI(ICI2,KJ)
                AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICIT,KJ)
   87        CONTINUE
C
Cd            call advanc(iacon1,nat,norb)
Cc Added here for the symmetry
             NST = NEND
  920    CONTINUE
C
  517   CONTINUE
C
C   Now for Beta double excitations
C
       DO 6015 IBB = IB+1,NBT
               ISTBB = JJ+1
               IENBB = IEN
               JB = IBCON1(IBB)
Cc Symmetry of orbital being deoccupied
               IS3 = IOX(JB)
               IPB = IBB-NCOR
               IF (JJ.GT.JB) IPB = IPB - 1
               DO 6010 KKJBB = KKJ,NB+1
                  DO 6005 JJBB = ISTBB,IENBB
Cc Symmetry of orbital being occupied
             IS4 = IOX(JJBB)
Cc ip2 = is3xis4
             IP2 = IMUL(IS4,IS3)
Cc If symmetry of beta '' is not right, skip it
             IF (IP1.NE.IP2) GOTO 6005
Cc
C
          CALL RET1DET(IACON2,IACON1,NB,IPB,JJBB-NCOR,0,KKJBB,IPER2)
          IBP2 = POSDET(NACT,NB,IACON1,IFA)
          IBP2 = ISPB(IBP2)
          IPER = IPER1+IPER2
          IPER = ((-1)**IPER)
               I2 = INDEX(JB,JJBB)
               INX = INDEX(I2,IND)
               II1 = INDEX(JJBB,IO1)
               II2 = INDEX(JB,JJ)
               INX2 = INDEX(II1,II2)
               C = SI2(INX) - SI2(INX2)
               T = C*IPER
C
Cd             do 686 ina1 = 0,(nalp-1)
Cc Loop over alpha dets of the right symmetry ie itab(isb1)
               DO 686 INA1 = ISAS(ISB1),ISAS(ISB1+1)-1
                  NEND = ISAC(INA1)
             ICIA = ISPA(NEND)
             ICIT = ICIA + ICAB
             ICI2 = ICIA + IBP2
             DO 85 KJ = 1,NV
                AB(ICIT,KJ) = AB(ICIT,KJ) + T*CI(ICI2,KJ)
                AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICIT,KJ)
   85        CONTINUE
  686       CONTINUE
C
C
 6005          CONTINUE
               ISTBB = IBCON1(KKJBB+NCOR)+1
               IENBB = IBCON1(NCOR+KKJBB+1)-1
               IF (KKJBB.EQ.NB) IENBB=NORB
 6010      CONTINUE
 6015 CONTINUE
C
 6020       CONTINUE
            IST = IBCON1(KKJ+NCOR)+1
            IEN=IBCON1(NCOR+KKJ+1)-1
            IF (KKJ.EQ.NB) IEN=NORB
 6025     CONTINUE
 6030 CONTINUE
C
C   Again, the diagonal elements are assumed to be in q, if this
C   is not the case then you must uncomment the following statements.
C
C    Remaining part of diagonal contributions
C
C            C = 0.0d+00
C            do 45 ii=ncor+1,nbt
C               i1 = ibcon1(ii)
C               ind1 = index(i1,i1)
C               C = C + si1(ind1)
C               do 54 jj=1,ii-1
C                  i2 = ibcon1(jj)
C                  ind2 = index(i2,i2)
C                  indm = index(i1,i2)
C                  j1 = index(indm,indm)
C                  j2 = index(ind2,ind1)
C                  C = C + si2(j2) - si2(j1)
C   54          continue
C   45       continue
C
C            do 93 ina1 = 0,(nalp-1)
C              icia = ina1*nblp
C              icit = icia + ijk
C              do 83 kj = 1,nv
C                 Ab(icit,kj) = Ab(icit,kj) + C*CI(icit,kj)
C   83         continue
C   93       continue
C
           CALL ADVANC(IBCON1,NBT,NORB)
C
 9999 CONTINUE
C
C  The following assumes that the diagonal elements are stored in q.
C  If this is not the case then you must comment the last few lines
C  and uncomment some statements above, should be easy to find.
C
C   Now for the diagonal contributions
C
      DO 119 IJK = 1,NCI
         DO 118 KJ = 1,NV
            AB(IJK,KJ) = AB(IJK,KJ) + Q(IJK)*CI(IJK,KJ)
  118    CONTINUE
  119 CONTINUE
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK RINAB0
C     --------------------------------------------------------
      SUBROUTINE RINAB0(SI1,SI2,NORB,NCOR,NCI,NA,NB,CI,IACON1,IBCON1,
     *               IACON2,IFA,IPOSA,IPERA,IIND1,INDEX,AB,NV,Q,
     *               UAIA,UAIB,IMMA,IMMC,
     *       ISYMA,ISYMB,ITAB,
     *       IMUL,ISPA,ISPB,
     *       ISBS,ISBC,NSYM,IOX,NALP,NBLP,
     *       ISPIN,IHMCON,ISTRB,ISTRP,ISTART)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
      DIMENSION AB(NCI,NV)
      DIMENSION SI1(*), SI2(*), CI(NCI,NV), IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION IACON1(NA+NCOR),IBCON1(NA+NCOR)
      DIMENSION IACON2(NA+NCOR),IPERA(NA*(NORB-NCOR-NA))
      DIMENSION IIND1(NA*(NORB-NCOR-NA))
      DIMENSION IPOSA(NA*(NORB-NCOR-NA))
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2)
      DIMENSION Q(NCI)
      DIMENSION ISYMA(NALP),ISYMB(NBLP)
      DIMENSION ITAB(NSYM),ISPA(NALP),ISPB(NBLP)
      DIMENSION IMUL(NSYM,NSYM)
      DIMENSION ISBS(NSYM+1)
      DIMENSION ISBC(NBLP)
      DIMENSION IOX(NORB)
      DIMENSION IMMA(NSYM,(NA*(NORB-NCOR-NA)))
      DIMENSION IMMC(NSYM)
      DIMENSION ISPIN(*),IHMCON(NV)
      DIMENSION ISTRB((NBLP*(NB*(NORB-NCOR-NB)))/2)
      DIMENSION ISTRP((NBLP*(NB*(NORB-NCOR-NB)))/2)
      DIMENSION ISTART(NBLP)
C
      UAIA = 0.0D+00
      UAIB = 0.0D+00
      NACT = NORB - NCOR
      NAT = NA + NCOR
      NBT = NB + NCOR
C
C *********
C  Assume that we have ifa and index already calculated
C
C      call binom6(ifa,nact)
C
C      do 7 i=1,(norb*(norb+1))/2
C         do 8 j=1,i
C            index(i,j) = i*(i-1)/2 + j
C            index(j,i) = index(i,j)
C    8    continue
C    7 continue
C
C ************
C
      DO 13 II=1,NCI
          DO 12 JJ=1,NV
             AB(II,JJ) = 0.0D+00
   12     CONTINUE
   13 CONTINUE
C
      DO 30 I=1,NAT
         IACON1(I) = I
   30 CONTINUE
C
C   Big Loop over all alpha determinants
C
      DO 9000 IJK = 1,NALP
C
C  Alpha excitations here
C   Single first
C
         DO 45 II=1,NSYM
            IMMC(II) = 0
   45    CONTINUE
         IAC = 0
         ICAT = ISPA(IJK)
Cc
Cc Symetry of alpha determinant
         ISA1 = ISYMA(IJK)
         ITAS = ITAB(ISA1)
Cc
         DO 7030 IA=NCOR+1,NAT
             IO1 = IACON1(IA)
Cc Symmetry of orbital being deoccupied
             IS1 = IOX(IO1)
Cc
             IST = IO1 + 1
             IEN = IACON1(IA+1)-1
             IF (IA.EQ.NAT) IEN=NORB
             DO 7025 KKJ=IA-NCOR+1,NA+1
                DO 7020 JJ=IST,IEN
Cc Symmetry of orbital being occupied
             IS2 = IOX(JJ)
Cc is1xis2 = ip1
             IP1 = IMUL(IS2,IS1)
Cc
             IAC = IAC + 1
             CALL RET1DET(IACON1,IACON2,NA,IA,JJ,NCOR,KKJ,IPER1)
C
C   Storage here for later use, well worth it timewise
C
             IPET = POSDET(NACT,NA,IACON2,IFA)
Cd    iposa(iac) = (ipet-1)*nblp
Cc New starting position of alpha determinant
             IMMC(ISYMA(IPET)) = IMMC(ISYMA(IPET)) + 1
             IPOSA(IAC) = ISPA(IPET)
C
             IMMA(ISYMA(IPET),IMMC(ISYMA(IPET))) = IAC
Cc
             IPERA(IAC) = ((-1)**IPER1)
             IND = INDEX(JJ,IO1)
             IIND1(IAC) = IND
Cc  If deoccupied and newly occupied orbs are of different symmetry,
C   skip to doubles
             IF (IS1.NE.IS2) GOTO 417
C
Cc  Symmetry of excited alpha is same as original, Isa1
C
             C = SI1(IND)
C
             DO 412 IK=1,NAT
                IF (IK.EQ.IA) GOTO 412
                ION = IACON1(IK)
                J1 = INDEX(ION,ION)
                JJ1 = INDEX(IND,J1)
                J1 = INDEX(ION,JJ)
                J2 = INDEX(ION,IO1)
                INX = INDEX(J1,J2)
                C = C + SI2(JJ1) - SI2(INX)
  412        CONTINUE
C
             DO 49 I=1,NBT
                IBCON1(I) = I
   49        CONTINUE
C
C
Cd             do 415 inb1=1,nblp
Cc   Loop over beta dets, of the right symmetry, ie itab(isa1)
               NST = 1
               DO 415 INB1=ISBS(ISA1),ISBS(ISA1+1)-1
                  NEND = ISBC(INB1)
                  DO 5510 KK=NST,NEND-1
                     CALL ADVANC(IBCON1,NBT,NORB)
 5510             CONTINUE
C
Cc  Modified position here
                ICIT = ICAT+ISPB(NEND)
                ICI2 = IPOSA(IAC)+ISPB(NEND)
Cc
                D = 0.0D+00
                DO 790 IK=1,NBT
                   ION = IBCON1(IK)
                   J1 = INDEX(ION,ION)
                   JJ1 = INDEX(IND,J1)
                   D = D + SI2(JJ1)
  790           CONTINUE
C
                T = (C+D)*IPERA(IAC)
                IF (ABS(T).GT.UAIA) UAIA = ABS(T)
                DO 44 KJ = 1,NV
                   AB(ICIT,KJ) = AB(ICIT,KJ) + T*CI(ICI2,KJ)
                   AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICIT,KJ)
   44           CONTINUE
C
Cd                call advanc(ibcon1,nbt,norb)
Cc Added here for symmetry
                NST = NEND
  415        CONTINUE
C
  417     CONTINUE
C
C      Double excitations
C
          DO 4015 IAA = IA+1,NAT
             IPA = IAA-NCOR
             IIA = IACON1(IAA)
Cc Symmetry of orbital being deoccupied
             IS3 = IOX(IIA)
             IF (JJ.GT.IIA) IPA = IPA - 1
             ISTAA = JJ+1
             IENAA = IEN
             DO 4010 KKJAA=KKJ,NA+1
                DO 4005 JJAA=ISTAA,IENAA
Cc Symmetry of orbital being occupied
             IS4 = IOX(JJAA)
Cc ip2 = is3xis4
             IP2 = IMUL(IS4,IS3)
Cc If symmetry of alpha '' is not right, skip it.
             IF (IP1.NE.IP2) GOTO 4005
Cc
C
             CALL RET1DET(IACON2,IBCON1,NA,IPA,JJAA-NCOR,0,KKJAA,IPER2)
             IPET = POSDET(NACT,NA,IBCON1,IFA)
             ICA1 = ISPA(IPET)
C     icbb = ispb(ipet)
             IPERT = IPER1+IPER2
             IPERT = ((-1)**IPERT)
C
                   I2 = INDEX(IIA,JJAA)
                   INX = INDEX(I2,IND)
                   II1 = INDEX(JJAA,IO1)
                   II2 = INDEX(IIA,JJ)
                   INX2 = INDEX(II1,II2)
                   C = SI2(INX) - SI2(INX2)
                   T = C*IPERT
C
Cd   do 786 inb1 = 1,nblp
Cc  Loop over beta dets of the right symmetry, itab(isa1)
               DO 786 INB1 = ISBS(ISA1),ISBS(ISA1+1)-1
                   NEND = ISBC(INB1)
                   ICI2 = ICA1 + ISPB(NEND)
                   ICIT = ICAT+ISPB(NEND)
                   DO 55 KJ = 1,NV
                      AB(ICIT,KJ) = AB(ICIT,KJ) + T*CI(ICI2,KJ)
                      AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICIT,KJ)
   55              CONTINUE
  786           CONTINUE
C
 4005           CONTINUE
                ISTAA = IACON1(KKJAA+NCOR)+1
                IENAA = IACON1(NCOR+KKJAA+1)-1
                IF (KKJAA.EQ.NA) IENAA=NORB
 4010        CONTINUE
 4015 CONTINUE
C
 7020           CONTINUE
                IST = IACON1(KKJ+NCOR)+1
                IEN = IACON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NA) IEN=NORB
 7025        CONTINUE
 7030     CONTINUE
C
C
C   End of pure alpha excitations
C
C   Loop over Beta dets now
C
         DO 40 I=1,NBT
            IBCON1(I) = IACON1(I)
   40    CONTINUE
C
C ***
         DO 8000 KJI =IJK,NBLP
          ISTAR = ISTART(KJI) - 1
          IPB1 = ISPB(KJI)
          ISB1 = ISYMB(KJI)
          ITBS = ITAB(ISB1)
Cc Number of matching alpha'
          IMZZ = IMMC(ITBS)
Cc Check to see if beta is of right sym, ie itab(isa1) = itas
          M1 = 0
          M2 = 0
          IF (ISB1.EQ.ITAS) M1 = 1
          IF (IMZZ.NE.0) M2 = 1
          IF (M1.EQ.0.AND.M2.EQ.0) GOTO 7998
          IC1 = ICAT + IPB1
          QNUM = 1.0D+00
          IF (IJK.EQ.KJI) QNUM = 2.0D+00
C
C   Beta first *********************** Single
C
             DO 900 IB=1,NBT
             IBB = IBCON1(IB)
Cc Symmetry of deoccupied orbital
             IB1 = IOX(IBB)
Cc Symmetry of remaining orbital product
              IR1 = IMUL(IB1,ISB1)
              IST = IBB+1
              IEN = IBCON1(IB+1)-1
              IF (IB.EQ.NBT) IEN=NORB
              DO 895 KKJ=IB+1,NB+1
              DO 890 JJ=IST,IEN
Cc Symmetry of occupied orbital
            IB2 = IOX(JJ)
C
Cc Symmetry of excited beta determinant
            ISB2 = IMUL(IR1,IB2)
            ITB2 = ITAB(ISB2)
Cc Check to see if isb2 ne itas
Cc and whether m1 = 0
            IMZ1 = IMMC(ITB2)
            ISTAR = ISTAR + 1
            IF (ISB2.NE.ITAS.AND.M1.EQ.0) GOTO 890
            IF (M2.EQ.0.AND.IMZ1.EQ.0) GOTO 890
            IF (ISB2.NE.ITAS.AND.IMZ1.EQ.0) GOTO 890
Cc
C               call ret1det(ibcon1,iacon2,nb,ib,jj,0,kkj,iper1)
C                   iposb = posdet(nact,nb,iacon2,ifa)
C                   ipb2 = ispb(iposb)
                   IPB2 = ISTRB(ISTAR)
                   IC2 = ICAT + IPB2
C           zperb = ((-1)**iper1)
C                   zperb = zperb/qnum
                   ZPERB = ISTRP(ISTAR)/QNUM
                   IOB = INDEX(IBB,JJ)
C
C
               IF (M2.EQ.0.AND.IMZ1.NE.0) THEN
                   DO 1013 IAT = 1,IMZ1
                      IJU = IMMA(ITB2,IAT)
                      IC4 = IPOSA(IJU) + IPB2
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*ZPERB*IPERA(IJU)
                      DO 66 KJ = 1,NV
                         AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                         AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
   66                 CONTINUE
 1013              CONTINUE
                   GOTO 890
C
C
                ELSEIF (M1.EQ.0.AND.ISB2.EQ.ITAS) THEN
                   DO 2013 IAT=1,IMZZ
                      IJU = IMMA(ITBS,IAT)
                      IC3 = IPOSA(IJU) + IPB1
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*ZPERB*IPERA(IJU)
                      DO 76 KJ=1,NV
                         AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                         AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
   76                 CONTINUE
 2013              CONTINUE
                   GOTO 890
C
                ELSEIF (ISB2.NE.ITAS.AND.IMZ1.NE.0) THEN
                   DO 3013 IAT = 1,IMZ1
                      IJU = IMMA(ITB2,IAT)
                      IC4 = IPOSA(IJU) + IPB2
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*ZPERB*IPERA(IJU)
                      DO 86 KJ = 1,NV
                         AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                         AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
C
   86                 CONTINUE
 3013              CONTINUE
                   GOTO 890
C
                ELSEIF (IMZ1.EQ.0.AND.ISB2.EQ.ITAS) THEN
                      DO 4013 IAT=1,IMZZ
                      IJU = IMMA(ITBS,IAT)
                      IC3 = IPOSA(IJU) + IPB1
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*ZPERB*IPERA(IJU)
                      DO 96 KJ=1,NV
                         AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                         AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
C
   96                 CONTINUE
 4013              CONTINUE
                   GOTO 890
C
                ELSEIF (IMZ1.NE.0.AND.ISB2.EQ.ITAS) THEN
                   DO 5013 IAT=1,IMZZ
                      IJU = IMMA(ITBS,IAT)
                      IC3 = IPOSA(IJU) + IPB1
                      IC4 = IPOSA(IJU) + IPB2
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*ZPERB*IPERA(IJU)
                      DO 106 KJ = 1,NV
                         AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                         AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                         AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                         AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
C
  106                 CONTINUE
 5013              CONTINUE
                   GOTO 890
C
                ENDIF
C
C
C
  890           CONTINUE
                IST = IBCON1(KKJ)+1
                IEN = IBCON1(KKJ+1)-1
                IF (KKJ.EQ.NB) IEN=NORB
  895        CONTINUE
  900     CONTINUE
C
Cc Added for symmetry
 7998 CONTINUE
           CALL ADVANC(IBCON1,NBT,NORB)
 8000    CONTINUE
C
C   The diagonal elements are assumed to be in q, if this is
C   not the case then the following statements must be
C   uncommented.
C
C
         CALL ADVANC(IACON1,NAT,NORB)
 9000 CONTINUE
C
C   Now for the Beta part
C
C      do 876 jji=1,nbt
C         ibcon1(jji) = jji
C  876 continue
Cc
C      do 9999 ijk = 1,nblp
Ccc Position of beta det
C      icab = ispb(ijk)
Ccc Symmetry of beta det
C      isb1 = isymb(ijk)
C      itb1 = itab(isb1)
Cc
C
C   Single Beta excitations
C
C      do 6030 ib=ncor+1,nbt
C         io1 = ibcon1(ib)
Cc Symmetry of orbital being deoccupied
C         is1 = iox(io1)
Ccc
C         ist = io1+1
C         ien = ibcon1(ib+1)-1
C         if (ib.eq.nbt) ien=norb
C         do 6025 kkj=ib-ncor+1,nb+1
C            do 6020 jj=ist,ien
Ccc Symmetry of orbital being occupied
C         is2 = iox(jj)
Ccc is1xis2 = ip1
C         ip1 = imul(is2,is1)
Ccc
C            call ret1det(ibcon1,iacon2,nb,ib,jj,ncor,kkj,iper1)
C            iper = ((-1)**iper1)
C            ind = index(jj,io1)
Ccc If deoccupied and newly occupied are of different symmetry,
C   skip to doubles
C         if (is1.ne.is2) goto 517
C            ipb1 = posdet(nact,nb,iacon2,ifa)
Ccc New position of beta det
C            ipb1 = ispb(ipb1)
Ccc
Cc
C            C = si1(ind)
Cc
C            do 912 ik=1,nbt
C               if (ik.eq.ib) goto 912
C               ion = ibcon1(ik)
C               j1 = index(ion,ion)
C               jj1 = index(ind,j1)
C               j1 = index(ion,jj)
C               j2 = index(ion,io1)
C               inx = index(j1,j2)
C               C = C + si2(jj1) - si2(inx)
C  912       continue
Cc
C       do 89 i=1,nat
C          iacon1(i) = i
C   89 continue
Cc
Cc
Ccd          do 920 ina1 = 0,(nalp-1)
Ccc  Loop over alpha dets of the right symmetry, ie itab(isb1)
C          nst = 1
C          do 920 ina1 = isas(isb1),isas(isb1+1)-1
C           nend = isac(ina1)
C          do 6610 kk=nst,nend-1
C             call advanc(iacon1,nat,norb)
C 6610     continue
Cc
Ccc Modified position here
C            icia = ispa(nend)
C            icit = icia + icab
C            ici2 = icia  + ipb1
C            D = 0.0d+00
C             do 690 ik=1,nat
C                ion = iacon1(ik)
C                j1 = index(ion,ion)
C                jj1 = index(ind,j1)
C                D = D + si2(jj1)
C  690        continue
Cc
C             T = (C+D)*iper
C             if (abs(T).gt.uaib) uaib = abs(T)
Cc     do 87 kj = 1,nv
Cc                Ab(icit,kj) = Ab(icit,kj) + T*CI(ici2,kj)
Cc                Ab(ici2,kj) = Ab(ici2,kj) + T*CI(icit,kj)
Cc   87        continue
Cc
Ccd            call advanc(iacon1,nat,norb)
Ccc Added here for the symmetry
C             nst = nend
C  920    continue
Cc
C  517   continue
Cc
Cc   Now for Beta double excitations
Cc
C       do 6015 ibb = ib+1,nbt
C               istbb = jj+1
C               ienbb = ien
C               jb = ibcon1(ibb)
Ccc Symmetry of orbital being deoccupied
C               is3 = iox(jb)
C               ipb = ibb-ncor
C               if (jj.gt.jb) ipb = ipb - 1
C               do 6010 kkjbb = kkj,nb+1
C                  do 6005 jjbb = istbb,ienbb
Ccc Symmetry of orbital being occupied
C             is4 = iox(jjbb)
Ccc ip2 = is3xis4
C             ip2 = imul(is4,is3)
Ccc If symmetry of beta '' is not right, skip it
C             if (ip1.ne.ip2) goto 6005
Ccc
Cc
C          call ret1det(iacon2,iacon1,nb,ipb,jjbb-ncor,0,kkjbb,iper2)
C          ibp2 = posdet(nact,nb,iacon1,ifa)
C          ibp2 = ispb(ibp2)
C          iper = iper1+iper2
C          iper = ((-1)**iper)
C               i2 = index(jb,jjbb)
C               inx = index(i2,ind)
C               ii1 = index(jjbb,io1)
C               ii2 = index(jb,jj)
C               inx2 = index(ii1,ii2)
C               C = si2(inx) - si2(inx2)
C               T = C*iper
Cc
Ccd             do 686 ina1 = 0,(nalp-1)
Ccc Loop over alpha dets of the right symmetry ie itab(isb1)
C               do 686 ina1 = isas(isb1),isas(isb1+1)-1
C                  nend = isac(ina1)
C             icia = ispa(nend)
C             icit = icia + icab
C             ici2 = icia + ibp2
C             do 85 kj = 1,nv
C                Ab(icit,kj) = Ab(icit,kj) + T*CI(ici2,kj)
C                Ab(ici2,kj) = Ab(ici2,kj) + T*CI(icit,kj)
C   85        continue
C  686       continue
C
C
C 6005          continue
C               istbb = ibcon1(kkjbb+ncor)+1
C               ienbb = ibcon1(ncor+kkjbb+1)-1
C               if (kkjbb.eq.nb) ienbb=norb
C 6010      continue
C 6015 continue
C
C 6020       continue
C            ist = ibcon1(kkj+ncor)+1
C            ien=ibcon1(ncor+kkj+1)-1
C            if (kkj.eq.nb) ien=norb
C 6025     continue
C 6030 continue
Cc
Cc   Again, the diagonal elements are assumed to be in q, if this
Cc   is not the case then you must uncomment the following statements.
Cc
C    Remaining part of diagonal contributions
C
C            C = 0.0d+00
C            do 45 ii=ncor+1,nbt
C               i1 = ibcon1(ii)
C               ind1 = index(i1,i1)
C               C = C + si1(ind1)
C               do 54 jj=1,ii-1
C                  i2 = ibcon1(jj)
C                  ind2 = index(i2,i2)
C                  indm = index(i1,i2)
C                  j1 = index(indm,indm)
C                  j2 = index(ind2,ind1)
C                  C = C + si2(j2) - si2(j1)
C   54          continue
C   45       continue
C
C            do 93 ina1 = 0,(nalp-1)
C              icia = ina1*nblp
C              icit = icia + ijk
C              do 83 kj = 1,nv
C                 Ab(icit,kj) = Ab(icit,kj) + C*CI(icit,kj)
C   83         continue
C   93       continue
C
C   call advanc(ibcon1,nbt,norb)
C
C 9999 continue
C
C  The following assumes that the diagonal elements are stored in q.
C  If this is not the case then you must comment the last few lines
C  and uncomment some statements above, should be easy to find.
C
      DO 1111 II=1,NALP
         ISA1 = ISYMA(II)
         ICIT = ISPA(II)
         INB = ISPB(II)
         DO 2222 INB1=ISBS(ISA1),ISBS(ISA1+1)-1
            NEND = ISBC(INB1)
            IF (NEND.GT.II) GOTO 1111
            ICI1= ICIT + ISPB(NEND)
            ICI2 = ISPA(NEND) + INB
            IF (NEND.EQ.II) THEN
            DO 4444 KJ=1,NV
            IS = (-1)**ISPIN(IHMCON(KJ))
            AB(ICI2,KJ) = AB(ICI2,KJ) + IS*AB(ICI2,KJ)
 4444       CONTINUE
            GOTO 1111
            ENDIF
C
            DO 3333 KJ=1,NV
            IS = (-1)**ISPIN(IHMCON(KJ))
            QT = AB(ICI1,KJ)
            AB(ICI1,KJ) = AB(ICI1,KJ) + IS*AB(ICI2,KJ)
            AB(ICI2,KJ) = AB(ICI2,KJ) + IS*QT
 3333       CONTINUE
 2222    CONTINUE
 1111 CONTINUE
C
C   Now for the diagonal contributions
C
      DO 119 IJK = 1,NCI
         DO 118 KJ = 1,NV
            AB(IJK,KJ) = AB(IJK,KJ) + Q(IJK)*CI(IJK,KJ)
  118    CONTINUE
  119 CONTINUE
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK RETAB0
C     --------------------------------------------------------
      SUBROUTINE RETAB0(SI1,SI2,NORB,NCOR,NCI,NA,NB,CI,IACON1,IBCON1,
     *               IACON2,IFA,IPOSA,IPERA,IIND1,INDEX,AB,Q,
     *               UAIA,UAIB,IMMA,IMMC,
     *       ISYMA,ISYMB,ITAB,
     *       IMUL,ISPA,ISPB,
     *       ISBS,ISBC,NSYM,IOX,NALP,NBLP,
     *       ISPIN,IHMCON,ISTRB,ISTRP,ISTART)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
      DIMENSION AB(NCI)
      DIMENSION SI1(*), SI2(*), CI(NCI), IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION IACON1(NA+NCOR),IBCON1(NA+NCOR)
      DIMENSION IACON2(NA+NCOR),IPERA(NA*(NORB-NCOR-NA))
      DIMENSION IIND1(NA*(NORB-NCOR-NA))
      DIMENSION IPOSA(NA*(NORB-NCOR-NA))
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2)
      DIMENSION Q(NCI)
      DIMENSION ISYMA(NALP),ISYMB(NBLP)
      DIMENSION ITAB(NSYM),ISPA(NALP),ISPB(NBLP)
      DIMENSION IMUL(NSYM,NSYM)
      DIMENSION ISBS(NSYM+1)
      DIMENSION ISBC(NBLP)
      DIMENSION IOX(NORB)
      DIMENSION IMMA(NSYM,(NA*(NORB-NCOR-NA)))
      DIMENSION IMMC(NSYM)
      DIMENSION ISPIN(*),IHMCON(1)
      DIMENSION ISTRB((NBLP*(NB*(NORB-NCOR-NB)))/2)
      DIMENSION ISTRP((NBLP*(NB*(NORB-NCOR-NB)))/2)
      DIMENSION ISTART(NBLP)
C
      UAIA = 0.0D+00
      UAIB = 0.0D+00
      NACT = NORB - NCOR
      NAT = NA + NCOR
      NBT = NB + NCOR
C
C *********
C  Assume that we have ifa and index already calculated
C
C      call binom6(ifa,nact)
C
C      do 7 i=1,(norb*(norb+1))/2
C         do 8 j=1,i
C            index(i,j) = i*(i-1)/2 + j
C            index(j,i) = index(i,j)
C    8    continue
C    7 continue
C
C ************
C
      DO 13 II=1,NCI
             AB(II) = 0.0D+00
   13 CONTINUE
C
      DO 30 I=1,NAT
         IACON1(I) = I
   30 CONTINUE
C
C   Big Loop over all alpha determinants
C
      DO 9000 IJK = 1,NALP
C
C  Alpha excitations here
C   Single first
C
         DO 45 II=1,NSYM
            IMMC(II) = 0
   45    CONTINUE
         IAC = 0
         ICAT = ISPA(IJK)
Cc
Cc Symetry of alpha determinant
         ISA1 = ISYMA(IJK)
         ITAS = ITAB(ISA1)
Cc
         DO 7030 IA=NCOR+1,NAT
             IO1 = IACON1(IA)
Cc Symmetry of orbital being deoccupied
             IS1 = IOX(IO1)
Cc
             IST = IO1 + 1
             IEN = IACON1(IA+1)-1
             IF (IA.EQ.NAT) IEN=NORB
             DO 7025 KKJ=IA-NCOR+1,NA+1
                DO 7020 JJ=IST,IEN
Cc Symmetry of orbital being occupied
             IS2 = IOX(JJ)
Cc is1xis2 = ip1
             IP1 = IMUL(IS2,IS1)
Cc
             IAC = IAC + 1
             CALL RET1DET(IACON1,IACON2,NA,IA,JJ,NCOR,KKJ,IPER1)
C
C   Storage here for later use, well worth it timewise
C
             IPET = POSDET(NACT,NA,IACON2,IFA)
Cd    iposa(iac) = (ipet-1)*nblp
Cc New starting position of alpha determinant
             IMMC(ISYMA(IPET)) = IMMC(ISYMA(IPET)) + 1
             IPOSA(IAC) = ISPA(IPET)
C
             IMMA(ISYMA(IPET),IMMC(ISYMA(IPET))) = IAC
Cc
             IPERA(IAC) = ((-1)**IPER1)
             IND = INDEX(JJ,IO1)
             IIND1(IAC) = IND
Cc  If deoccupied and newly occupied orbs are of different symmetry,
C   skip to doubles
             IF (IS1.NE.IS2) GOTO 417
C
Cc  Symmetry of excited alpha is same as original, Isa1
C
             C = SI1(IND)
C
             DO 412 IK=1,NAT
                IF (IK.EQ.IA) GOTO 412
                ION = IACON1(IK)
                J1 = INDEX(ION,ION)
                JJ1 = INDEX(IND,J1)
                J1 = INDEX(ION,JJ)
                J2 = INDEX(ION,IO1)
                INX = INDEX(J1,J2)
                 C = C + SI2(JJ1) - SI2(INX)
  412        CONTINUE
C
             DO 49 I=1,NBT
                IBCON1(I) = I
   49        CONTINUE
C
C
Cd             do 415 inb1=1,nblp
Cc   Loop over beta dets, of the right symmetry, ie itab(isa1)
               NST = 1
               DO 415 INB1=ISBS(ISA1),ISBS(ISA1+1)-1
                  NEND = ISBC(INB1)
                  DO 5510 KK=NST,NEND-1
                     CALL ADVANC(IBCON1,NBT,NORB)
 5510             CONTINUE
C
Cc  Modified position here
                ICIT = ICAT+ISPB(NEND)
                ICI2 = IPOSA(IAC)+ISPB(NEND)
Cc
                D = 0.0D+00
                DO 790 IK=1,NBT
                   ION = IBCON1(IK)
                   J1 = INDEX(ION,ION)
                   JJ1 = INDEX(IND,J1)
                   D = D + SI2(JJ1)
  790           CONTINUE
C
                T = (C+D)*IPERA(IAC)
C               if (abs(T).gt.uaia) uaia = abs(T)
                   AB(ICIT) = AB(ICIT) + T*CI(ICI2)
                   AB(ICI2) = AB(ICI2) + T*CI(ICIT)
C
Cd                call advanc(ibcon1,nbt,norb)
Cc Added here for symmetry
                NST = NEND
  415        CONTINUE
C
  417     CONTINUE
C
C      Double excitations
C
          DO 4015 IAA = IA+1,NAT
             IPA = IAA-NCOR
             IIA = IACON1(IAA)
Cc Symmetry of orbital being deoccupied
             IS3 = IOX(IIA)
             IF (JJ.GT.IIA) IPA = IPA - 1
             ISTAA = JJ+1
             IENAA = IEN
             DO 4010 KKJAA=KKJ,NA+1
                DO 4005 JJAA=ISTAA,IENAA
Cc Symmetry of orbital being occupied
             IS4 = IOX(JJAA)
Cc ip2 = is3xis4
             IP2 = IMUL(IS4,IS3)
Cc If symmetry of alpha '' is not right, skip it.
             IF (IP1.NE.IP2) GOTO 4005
Cc
C
             CALL RET1DET(IACON2,IBCON1,NA,IPA,JJAA-NCOR,0,KKJAA,IPER2)
             IPET = POSDET(NACT,NA,IBCON1,IFA)
             ICA1 = ISPA(IPET)
C     icbb = ispb(ipet)
             IPERT = IPER1+IPER2
             IPERT = ((-1)**IPERT)
C
                   I2 = INDEX(IIA,JJAA)
                   INX = INDEX(I2,IND)
                   II1 = INDEX(JJAA,IO1)
                   II2 = INDEX(IIA,JJ)
                   INX2 = INDEX(II1,II2)
                   C = SI2(INX) - SI2(INX2)
                   T = C*IPERT
C
Cd   do 786 inb1 = 1,nblp
Cc  Loop over beta dets of the right symmetry, itab(isa1)
               DO 786 INB1 = ISBS(ISA1),ISBS(ISA1+1)-1
                   NEND = ISBC(INB1)
                   ICI2 = ICA1 + ISPB(NEND)
                   ICIT = ICAT+ISPB(NEND)
                      AB(ICIT) = AB(ICIT) + T*CI(ICI2)
                      AB(ICI2) = AB(ICI2) + T*CI(ICIT)
  786           CONTINUE
C
 4005           CONTINUE
                ISTAA = IACON1(KKJAA+NCOR)+1
                IENAA = IACON1(NCOR+KKJAA+1)-1
                IF (KKJAA.EQ.NA) IENAA=NORB
 4010        CONTINUE
 4015 CONTINUE
C
 7020           CONTINUE
                IST = IACON1(KKJ+NCOR)+1
                IEN = IACON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NA) IEN=NORB
 7025        CONTINUE
 7030     CONTINUE
C
C
C   End of pure alpha excitations
C
C   Loop over Beta dets now
C
         DO 40 I=1,NBT
            IBCON1(I) = IACON1(I)
   40    CONTINUE
C
C ***
         DO 8000 KJI =IJK,NBLP
          ISTAR = ISTART(KJI) - 1
          IPB1 = ISPB(KJI)
          ISB1 = ISYMB(KJI)
          ITBS = ITAB(ISB1)
Cc Number of matching alpha'
          IMZZ = IMMC(ITBS)
Cc Check to see if beta is of right sym, ie itab(isa1) = itas
          M1 = 0
          M2 = 0
          IF (ISB1.EQ.ITAS) M1 = 1
          IF (IMZZ.NE.0) M2 = 1
          IF (M1.EQ.0.AND.M2.EQ.0) GOTO 7998
          IC1 = ICAT + IPB1
          QNUM = 1.0D+00
          IF (IJK.EQ.KJI) QNUM = 2.0D+00
C
C   Beta first *********************** Single
C
             DO 900 IB=1,NBT
             IBB = IBCON1(IB)
Cc Symmetry of deoccupied orbital
             IB1 = IOX(IBB)
Cc Symmetry of remaining orbital product
              IR1 = IMUL(IB1,ISB1)
              IST = IBB+1
              IEN = IBCON1(IB+1)-1
              IF (IB.EQ.NBT) IEN=NORB
              DO 895 KKJ=IB+1,NB+1
              DO 890 JJ=IST,IEN
Cc Symmetry of occupied orbital
            IB2 = IOX(JJ)
C
Cc Symmetry of excited beta determinant
            ISB2 = IMUL(IR1,IB2)
            ITB2 = ITAB(ISB2)
Cc Check to see if isb2 ne itas
Cc and whether m1 = 0
            IMZ1 = IMMC(ITB2)
            ISTAR = ISTAR + 1
            IF (ISB2.NE.ITAS.AND.M1.EQ.0) GOTO 890
            IF (M2.EQ.0.AND.IMZ1.EQ.0) GOTO 890
            IF (ISB2.NE.ITAS.AND.IMZ1.EQ.0) GOTO 890
Cc
C              call ret1det(ibcon1,iacon2,nb,ib,jj,0,kkj,iper1)
C              iposb = posdet(nact,nb,iacon2,ifa)
C               ipb2 = ispb(iposb)
                   IPB2 = ISTRB(ISTAR)
                   IC2 = ICAT + IPB2
C           zperb = ((-1)**iper1)
C           zperb = zperb/qnum
                   ZPERB = ISTRP(ISTAR)/QNUM
                   IOB = INDEX(IBB,JJ)
C
C
               IF (M2.EQ.0.AND.IMZ1.NE.0) THEN
                   DO 1013 IAT = 1,IMZ1
                      IJU = IMMA(ITB2,IAT)
                      IC4 = IPOSA(IJU) + IPB2
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*ZPERB*(DBLE(IPERA(IJU)))
                         AB(IC1) = AB(IC1) + C*CI(IC4)
                         AB(IC4) = AB(IC4) + C*CI(IC1)
 1013              CONTINUE
                   GOTO 890
C
C
                ELSEIF (M1.EQ.0.AND.ISB2.EQ.ITAS) THEN
                   DO 2013 IAT=1,IMZZ
                      IJU = IMMA(ITBS,IAT)
                      IC3 = IPOSA(IJU) + IPB1
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*ZPERB*(DBLE(IPERA(IJU)))
                         AB(IC2) = AB(IC2) + C*CI(IC3)
                         AB(IC3) = AB(IC3) + C*CI(IC2)
 2013              CONTINUE
                   GOTO 890
C
                ELSEIF (ISB2.NE.ITAS.AND.IMZ1.NE.0) THEN
                   DO 3013 IAT = 1,IMZ1
                     IJU = IMMA(ITB2,IAT)
                      IC4 = IPOSA(IJU) + IPB2
                    IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*ZPERB*(DBLE(IPERA(IJU)))
                         AB(IC1) = AB(IC1) + C*CI(IC4)
                         AB(IC4) = AB(IC4) + C*CI(IC1)
 3013              CONTINUE
                   GOTO 890
C
                ELSEIF (IMZ1.EQ.0.AND.ISB2.EQ.ITAS) THEN
                      DO 4013 IAT=1,IMZZ
                      IJU = IMMA(ITBS,IAT)
                      IC3 = IPOSA(IJU) + IPB1
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*ZPERB*(DBLE(IPERA(IJU)))
                         AB(IC2) = AB(IC2) + C*CI(IC3)
                         AB(IC3) = AB(IC3) + C*CI(IC2)
 4013              CONTINUE
                   GOTO 890
C
                ELSEIF (IMZ1.NE.0.AND.ISB2.EQ.ITAS) THEN
                   DO 5013 IAT=1,IMZZ
                      IJU = IMMA(ITBS,IAT)
                      IC3 = IPOSA(IJU) + IPB1
                      IC4 = IPOSA(IJU) + IPB2
                      IX = INDEX(IIND1(IJU),IOB)
                      C = SI2(IX)*ZPERB*(DBLE(IPERA(IJU)))
                         AB(IC1) = AB(IC1) + C*CI(IC4)
                         AB(IC4) = AB(IC4) + C*CI(IC1)
                         AB(IC2) = AB(IC2) + C*CI(IC3)
                         AB(IC3) = AB(IC3) + C*CI(IC2)
 5013              CONTINUE
                   GOTO 890
C
                ENDIF
C
C
C
  890           CONTINUE
                IST = IBCON1(KKJ)+1
                IEN = IBCON1(KKJ+1)-1
                IF (KKJ.EQ.NB) IEN=NORB
  895        CONTINUE
  900     CONTINUE
C
Cc Added for symmetry
 7998 CONTINUE
           CALL ADVANC(IBCON1,NBT,NORB)
 8000    CONTINUE
C
C   The diagonal elements are assumed to be in q, if this is
C   not the case then the following statements must be
C   uncommented.
C
C
         CALL ADVANC(IACON1,NAT,NORB)
 9000 CONTINUE
C
C   Now for the Beta part
C
C      do 876 jji=1,nbt
C         ibcon1(jji) = jji
C  876 continue
Cc
C      do 9999 ijk = 1,nblp
Ccc Position of beta det
C      icab = ispb(ijk)
Ccc Symmetry of beta det
C      isb1 = isymb(ijk)
C      itb1 = itab(isb1)
Cc
C
C   Single Beta excitations
C
C      do 6030 ib=ncor+1,nbt
C         io1 = ibcon1(ib)
Cc Symmetry of orbital being deoccupied
C         is1 = iox(io1)
Ccc
C         ist = io1+1
C         ien = ibcon1(ib+1)-1
C         if (ib.eq.nbt) ien=norb
C         do 6025 kkj=ib-ncor+1,nb+1
C            do 6020 jj=ist,ien
Ccc Symmetry of orbital being occupied
C         is2 = iox(jj)
Ccc is1xis2 = ip1
C         ip1 = imul(is2,is1)
Ccc
C            call ret1det(ibcon1,iacon2,nb,ib,jj,ncor,kkj,iper1)
C            iper = ((-1)**iper1)
C            ind = index(jj,io1)
Ccc If deoccupied and newly occupied are of different symmetry,
C   skip to doubles
C         if (is1.ne.is2) goto 517
C            ipb1 = posdet(nact,nb,iacon2,ifa)
Ccc New position of beta det
C            ipb1 = ispb(ipb1)
Ccc
Cc
C            C = si1(ind)
Cc
C            do 912 ik=1,nbt
C               if (ik.eq.ib) goto 912
C               ion = ibcon1(ik)
C               j1 = index(ion,ion)
C               jj1 = index(ind,j1)
C               j1 = index(ion,jj)
C               j2 = index(ion,io1)
C               inx = index(j1,j2)
C               C = C + si2(jj1) - si2(inx)
C  912       continue
Cc
C       do 89 i=1,nat
C          iacon1(i) = i
C   89 continue
Cc
Cc
Ccd          do 920 ina1 = 0,(nalp-1)
Ccc  Loop over alpha dets of the right symmetry, ie itab(isb1)
C          nst = 1
C          do 920 ina1 = isas(isb1),isas(isb1+1)-1
C           nend = isac(ina1)
C          do 6610 kk=nst,nend-1
C             call advanc(iacon1,nat,norb)
C 6610     continue
Cc
Ccc Modified position here
C            icia = ispa(nend)
C            icit = icia + icab
C            ici2 = icia  + ipb1
C            D = 0.0d+00
C             do 690 ik=1,nat
C               ion = iacon1(ik)
C               j1 = index(ion,ion)
C               jj1 = index(ind,j1)
C               D = D + si2(jj1)
C  690        continue
Cc
C             T = (C+D)*iper
C             if (abs(T).gt.uaib) uaib = abs(T)
Cc     do 87 kj = 1,nv
Cc               Ab(icit,kj) = Ab(icit,kj) + T*CI(ici2,kj)
Cc               Ab(ici2,kj) = Ab(ici2,kj) + T*CI(icit,kj)
Cc   87        continue
Cc
Ccd            call advanc(iacon1,nat,norb)
Ccc Added here for the symmetry
C             nst = nend
C  920    continue
Cc
C  517   continue
Cc
Cc   Now for Beta double excitations
Cc
C       do 6015 ibb = ib+1,nbt
C               istbb = jj+1
C               ienbb = ien
C               jb = ibcon1(ibb)
Ccc Symmetry of orbital being deoccupied
C               is3 = iox(jb)
C               ipb = ibb-ncor
C               if (jj.gt.jb) ipb = ipb - 1
C               do 6010 kkjbb = kkj,nb+1
C                  do 6005 jjbb = istbb,ienbb
Ccc Symmetry of orbital being occupied
C             is4 = iox(jjbb)
Ccc ip2 = is3xis4
C             ip2 = imul(is4,is3)
Ccc If symmetry of beta '' is not right, skip it
C             if (ip1.ne.ip2) goto 6005
Ccc
Cc
C          call ret1det(iacon2,iacon1,nb,ipb,jjbb-ncor,0,kkjbb,iper2)
C          ibp2 = posdet(nact,nb,iacon1,ifa)
C          ibp2 = ispb(ibp2)
C          iper = iper1+iper2
C          iper = ((-1)**iper)
C               i2 = index(jb,jjbb)
C               inx = index(i2,ind)
C               ii1 = index(jjbb,io1)
C               ii2 = index(jb,jj)
C               inx2 = index(ii1,ii2)
C               C = si2(inx) - si2(inx2)
C               T = C*iper
Cc
Ccd             do 686 ina1 = 0,(nalp-1)
Ccc Loop over alpha dets of the right symmetry ie itab(isb1)
C               do 686 ina1 = isas(isb1),isas(isb1+1)-1
C                  nend = isac(ina1)
C             icia = ispa(nend)
C             icit = icia + icab
C             ici2 = icia + ibp2
C             do 85 kj = 1,nv
C                Ab(icit,kj) = Ab(icit,kj) + T*CI(ici2,kj)
C                Ab(ici2,kj) = Ab(ici2,kj) + T*CI(icit,kj)
C   85        continue
C  686       continue
C
C
C 6005          continue
C               istbb = ibcon1(kkjbb+ncor)+1
C               ienbb = ibcon1(ncor+kkjbb+1)-1
C               if (kkjbb.eq.nb) ienbb=norb
C 6010      continue
C 6015 continue
C
C 6020       continue
C            ist = ibcon1(kkj+ncor)+1
C            ien=ibcon1(ncor+kkj+1)-1
C            if (kkj.eq.nb) ien=norb
C 6025     continue
C 6030 continue
Cc
Cc   Again, the diagonal elements are assumed to be in q, if this
Cc   is not the case then you must uncomment the following statements.
Cc
C    Remaining part of diagonal contributions
C
C            C = 0.0d+00
C            do 45 ii=ncor+1,nbt
C               i1 = ibcon1(ii)
C               ind1 = index(i1,i1)
C               C = C + si1(ind1)
C               do 54 jj=1,ii-1
C                  i2 = ibcon1(jj)
C                  ind2 = index(i2,i2)
C                  indm = index(i1,i2)
C                  j1 = index(indm,indm)
C                  j2 = index(ind2,ind1)
C                  C = C + si2(j2) - si2(j1)
C   54          continue
C   45       continue
C
C            do 93 ina1 = 0,(nalp-1)
C              icia = ina1*nblp
C              icit = icia + ijk
C              do 83 kj = 1,nv
C                 Ab(icit,kj) = Ab(icit,kj) + C*CI(icit,kj)
C   83         continue
C   93       continue
C
C   call advanc(ibcon1,nbt,norb)
C
C 9999 continue
C
C  The following assumes that the diagonal elements are stored in q.
C  If this is not the case then you must comment the last few lines
C  and uncomment some statements above, should be easy to find.
C
      DO 1111 II=1,NALP
         ISA1 = ISYMA(II)
         ICIT = ISPA(II)
         INB = ISPB(II)
         DO 2222 INB1=ISBS(ISA1),ISBS(ISA1+1)-1
            NEND = ISBC(INB1)
            IF (NEND.GT.II) GOTO 1111
            ICI1= ICIT + ISPB(NEND)
            ICI2 = ISPA(NEND) + INB
            IF (NEND.EQ.II) THEN
            IS = (-1)**ISPIN(IHMCON(1))
            AB(ICI2) = AB(ICI2) + IS*AB(ICI2)
            GOTO 1111
            ENDIF
C
            IS = (-1)**ISPIN(IHMCON(1))
            QT = AB(ICI1)
            AB(ICI1) = AB(ICI1) + IS*AB(ICI2)
            AB(ICI2) = AB(ICI2) + IS*QT
 2222    CONTINUE
 1111 CONTINUE
C
C   Now for the diagonal contributions
C
      DO 119 IJK = 1,NCI
            AB(IJK) = AB(IJK) + Q(IJK)*CI(IJK)
  119 CONTINUE
C
      RETURN
      END
C
C
C*MODULE ALDECI  *DECK MOSYPR
C     -----------------------------------
      SUBROUTINE MOSYPR(LMOLAB,NCOR,NACT)
C     -----------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION LMOLAB(NCOR+NACT)
      COMMON /IOFILE/ IR,IW,IP,IS,IJKT,IDAF,NAV,IODA(400)
C
      IF(NCOR.GT.0) WRITE(IW,9070) (LMOLAB(I),I=1,NCOR)
      IF(NACT.GT.0) WRITE(IW,9080) (LMOLAB(I+NCOR),I=1,NACT)
C
 9070 FORMAT(/1X,'    CORE=',10(1X,A4,1X)/(10X,10(1X,A4,1X)))
 9080 FORMAT(/1X,'  ACTIVE=',10(1X,A4,1X)/(10X,10(1X,A4,1X)))
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK GAJASW
C     ------------------------------------
      SUBROUTINE GAJASW(LMOIRP,NUM,GRPDET)
C     ------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION LMOIRP(NUM)
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA C2H,D2,D2H/8HC2H     ,8HD2      ,8HD2H     /
#else
      CHARACTER*8 :: D2H_STR, C2H_STR, D2_STR
      EQUIVALENCE (D2H, D2H_STR), (D2, D2_STR), (C2H, C2H_STR)
      DATA C2H_STR,D2_STR,D2H_STR/"C2H     ","D2  "    ,"D2H "    /
#endif
C
C        note that the DAF record that saves the orbital labels
C        for the CI code is private to the CI code, so this
C        translation does not affect anything else.
C
C        see routine SYMBOL for GAMESS' assignment of integers
C        see routine GTAB for Jakal's desired labels
C
C        the orbital symmetry code in GAMESS assigns C2h orbitals
C        by the following: 1,2,3,4 = ag,au,bu,bg.  However, the
C        the determinant code wants the order 1,2,3,4=ag,bg,bu,au
C
C
      IF(GRPDET.EQ.C2H) THEN
         DO I=1,NUM
            MODI=LMOIRP(I)
            IF (LMOIRP(I).EQ.2) MODI=4
            IF (LMOIRP(I).EQ.4) MODI=2
            LMOIRP(I)=MODI
         END DO
      END IF
C        the orbital symmetry code in GAMESS assigns D2 orbitals
C        by the following: 1,2,3,4 = a,b1,b3,b2.  However, the
C        the determinant code wants the order 1,2,3,4=a,b1,b2,b3
C
      IF(GRPDET.EQ.D2) THEN
         DO I=1,NUM
            MODI=LMOIRP(I)
            IF (LMOIRP(I).EQ.3) MODI=4
            IF (LMOIRP(I).EQ.4) MODI=3
            LMOIRP(I)=MODI
         END DO
      END IF
C
C        the orbital symmetry code in GAMESS assigns D2H orbitals
C        by: 1,2,3,4,5,6,7,8 = ag, au,b3u,b3g,b1g,b1u,b2u,b2g
C        we need 1,2,3,...,8 = ag,b1g,b2g,b3g,au ,b1u,b2u,b3u
C
      IF(GRPDET.EQ.D2H) THEN
         DO I=1,NUM
            MODI=LMOIRP(I)
            IF (LMOIRP(I).EQ.2) MODI=5
            IF (LMOIRP(I).EQ.3) MODI=8
            IF (LMOIRP(I).EQ.5) MODI=2
            IF (LMOIRP(I).EQ.8) MODI=3
            LMOIRP(I)=MODI
         END DO
      END IF
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK CORTRA
C     -----------------------------------
      SUBROUTINE CORTRA(IBO,NTOT,NCOR)
C     -----------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IBO(NTOT)
C
      DO I=NCOR+1,NTOT
         IBO(I-NCOR)=IBO(I)
      END DO
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK MEMCI
C     --------------------------------------------------------
      SUBROUTINE MEMCI(IW,NORB,NCOR,NA,NB,
     *                 K,MAXP,MAXW1,IFA,NCI,IDS,IIS,
     *                 NALP,NBLP,
     *                 IDSYM,ISYM1,NSYM,
     *                 IBO,ISST,ICON,ICA,ICB,KTAB,IWRK)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION ICON(NA),ICA(NSYM),ICB(NSYM),KTAB(NSYM)
      DIMENSION IBO(NORB-NCOR),IWRK(43)
C
C     Code to return memory requirement for CI calculation.
C
C   norb,ncor : total no. of orbitals and no. of core orbs respectively
C   na, nb    : number of active alpha and beta electrons respectively
C   k         : Number of states required.
C   maxp      : Maximum number of vectors before transforming and
C               starting at kst.
C   maxw1     : Size of diagonalization for initial guess vectors.
C   idsym     : Which point group, see gtab, gmul or getsym1 in symwrk.f
C               for convention.
C   isym1     : Which irreducible representation, see gtab etc for info.
C   nsym      : nsym = 2**(idsym)
C   ibo       : ibo(i) contains symmetry of active orbital i, see gtab
C
C  RETURNED
C
C   ifa       : ifa will be a list of binomial coefficients which
C               are required for the CI calculation and also to
C               decide how much memory is required.
C   nci       : Returned with the no. of determinants for required CI.
C   ids       : Required space for double precision arrays, not
C               including for arrays spin and EL.
C   iis       : Required space for integer arrays, not including ifa.
C   nalp      : Number of alpha space functions.
C   nblp      : Number of beta space functions.
C   isst      : This is where the symmetry stuff starts in the huge
C               integer work array of size iis.
C
C  SCRATCH    : iwrk,icon,ica,icb,ktab, these are so small that they
C               don't need to be prepared for, ie be a part of the
C               calling statement.
C
      NACT = NORB-NCOR
      NCI = 0
      CALL BINOM6(IFA,NACT)
C
      NALP = IFA(NACT,NA)
      NBLP = IFA(NACT,NB)
C
      DO 45 II=1,NSYM
         ICA(II) = 0
         ICB(II) = 0
   45 CONTINUE
C
      IF (IDSYM.GT.0) THEN
      CALL GTAB(IDSYM,ISYM1,KTAB,
     *      IWRK(1),IWRK(4),IWRK(7),IWRK(10))
      ELSE
      CALL GTAB(1,1,KTAB,
     *      IWRK(1),IWRK(4),IWRK(7),IWRK(10))
      ENDIF
C
      DO 23 II=1,NA
         ICON(II) = II
   23 CONTINUE
C
      DO 53 IA=1,NALP
         IF (IDSYM.GT.0) THEN
         CALL GETSYM1(IW,ICON,NACT,NA,IBO,IDSYM,ISYM,
     *   IWRK(1),IWRK(4),IWRK(7),IWRK(10))
         ELSE
         CALL GETSYM1(IW,ICON,NACT,NA,IBO,1,ISYM,
     *   IWRK(1),IWRK(4),IWRK(7),IWRK(10))
         ENDIF
         ICA(ISYM) = ICA(ISYM) + 1
         CALL ADVANC(ICON,NA,NACT)
   53 CONTINUE
C
      DO 33 II=1,NB
         ICON(II) = II
   33 CONTINUE
C
      DO 43 IB=1,NBLP
         IF (IDSYM.GT.0) THEN
         CALL GETSYM1(IW,ICON,NACT,NB,IBO,IDSYM,ISYM,
     *   IWRK(1),IWRK(4),IWRK(7),IWRK(10))
         ELSE
         CALL GETSYM1(IW,ICON,NACT,NB,IBO,1,ISYM,
     *   IWRK(1),IWRK(4),IWRK(7),IWRK(10))
         ENDIF
         ICB(ISYM) = ICB(ISYM) + 1
         CALL ADVANC(ICON,NB,NACT)
   43 CONTINUE
C
      DO 67 II=1,NSYM
         NCI = NCI + ICA(II)*ICB(KTAB(II))
   67 CONTINUE
C
C  Now for the extra memory requirements.
C
      IDS = (2*MAXP+1)*NCI+MAXW1*(8+MAXW1)+(MAXW1*(MAXW1+1))/2+
     * (MAXP*MAXP) + K
C
      IIS = 5*NA + 2*NB + 4*NCOR + 3*(NA*(NORB-NCOR-NA)) +
     *      ((NORB*(NORB+1))/2)**2 + 3*MAXW1 +
     *       (NSYM*(NA*(NORB-NCOR-NA))) + NSYM +
     *  (NB*(NORB-NCOR-NB))*NBLP + NBLP +
     *      NALP*3 + NBLP*3 + NSYM*NSYM + 3*NSYM + 2*(NSYM+1) + K
C
      ISST = 5*NA + 2*NB + 4*NCOR + 3*(NA*(NORB-NCOR-NA)) +
     *      ((NORB*(NORB+1))/2)**2 + 3*MAXW1 +
     *      (NSYM*(NA*(NORB-NCOR-NA))) + NSYM + 1 +
     *    (NB*(NORB-NCOR-NB))*NBLP + NBLP
      RETURN
      END
C
C*MODULE ALDECI  *DECK SYMWRK
C     --------------------------------------------------------
      SUBROUTINE SYMWRK(IW,IBO,NACT,NA,NB,IDSYM,ISYM1,NSYM,
     *     NALP,NBLP,ICON,ISYMA,ISYMB,ICOA,ICOB,ITAB,
     *     IMUL,ISPA,ISPB,ISAS,ISBS,ISAC,ISBC)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION ICON(*)
      DIMENSION ISPA(NALP),ISPB(NBLP),ICOA(NSYM),ICOB(NSYM)
      DIMENSION ISYMA(NALP),ISYMB(NBLP),ITAB(NSYM)
      DIMENSION ISAS(NSYM+1),ISBS(NSYM+1)
      DIMENSION ISAC(NALP),ISBC(NBLP)
      DIMENSION IMUL(NSYM,NSYM)
      DIMENSION IBO(NACT)
C
C     Code to return symmetry data for CI calculation.
C
C     ibo   : ibo(i) is symmetry of orbital i, see gtab for info
C     nact  : No. of active orbitals
C     na    : No. of active alpha electrons
C     nb    : No. of active beta electrons
C     idsym : Which point group, see gtab for convention
C     isym1 : Which irreducible representation, see gtab for conv.
C     nsym  : nsym = 2**(idsym)
C     nalp  : Number of alpha space functions
C     nblp  : Number of beta space functions
C     ALL REMAINING ARRAYS ARE USED FOR CI,DENSITY,AND MCSCF ROUTINES.
C
      CALL GTAB(IDSYM,ISYM1,ITAB,ICON(1),ICON(4),ICON(7),ICON(10))
      CALL GMUL(IDSYM,IMUL,ICON(1),ICON(4),ICON(7),ICON(10))
C
      DO 13 II=1,NSYM
         ISAS(II) = 0
         ISBS(II) = 0
         ICOA(II) = 0
         ICOB(II) = 0
   13 CONTINUE
C
      DO 23 II=1,NB
         ICON(II) = II
   23 CONTINUE
C
      DO 43 IB=1,NBLP
         CALL GETSYM1(IW,ICON(1),NACT,NB,IBO,IDSYM,ISYM,
     *    ICON(NA+1),ICON(NA+4),ICON(NA+7),ICON(NA+10))
         ISYMB(IB) = ISYM
         ICOB(ISYM) = ICOB(ISYM) + 1
         ISPB(IB) = ICOB(ISYM)
         CALL ADVANC(ICON,NB,NACT)
   43 CONTINUE
C
      DO 33 II=1,NA
         ICON(II) = II
   33 CONTINUE
C
      NCI = 0
      DO 53 IA=1,NALP
         CALL GETSYM1(IW,ICON(1),NACT,NA,IBO,IDSYM,ISYM,
     *   ICON(NA+1),ICON(NA+4),ICON(NA+7),ICON(NA+10))
         ISYMA(IA) = ISYM
         ICOA(ISYM) = ICOA(ISYM) + 1
         ISPA(IA) = NCI
         NCI = NCI + ICOB(ITAB(ISYM))
         CALL ADVANC(ICON,NA,NACT)
   53 CONTINUE
C
      ISAS(1) = 1
      ISBS(1) = 1
      ISAS(NSYM+1) = NALP + 1
      ISBS(NSYM+1) = NBLP + 1
C
      DO 63 II=2,NSYM
         ISAS(II) = ISAS(II-1) + ICOA(ITAB(II-1))
         ISBS(II) = ISBS(II-1) + ICOB(ITAB(II-1))
   63 CONTINUE
C
      DO 73 II=1,NSYM
         ICOA(II) = 0
         ICOB(II) = 0
   73 CONTINUE
C
      DO 83 IA=1,NALP
         NSA = ISYMA(IA)
         ICOA(NSA) = ICOA(NSA) + 1
         ISAC(ISAS(ITAB(NSA))+ICOA(NSA)-1) = IA
   83 CONTINUE
C
      DO 93 IB=1,NBLP
         NSA = ISYMB(IB)
         ICOB(NSA) = ICOB(NSA) + 1
         ISBC(ISBS(ITAB(NSA))+ICOB(NSA)-1) = IB
   93 CONTINUE
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK GTAB
C     -----------------------------------------------------
      SUBROUTINE GTAB(IDSYM,ISYM1,ITAB,IELE,ISTA,ISCR,ICHA)
C     -----------------------------------------------------
C
C     Routine to return table such that i x itab(i) = isym1
C     where i is an irreducible representation.
C     idsym  specifies the point group.
C     isym1  desired symmetry
C
C     CONVENTION FOR idsym,isym1, and itab
C
C     Point group  idsym  Irred rep isym1  Sym operations used
C   -----------------------------------------------------------
C        Ci          1       Ag       1        i
C                            Au       2
C
C        Cs          1       A'       1      (sigma)h
C                            A''      2
C
C        C2          1       A        1       C2
C                            B        2
C
C        D2          2       A        1       C2(z)
C                            B1       2       C2(y)
C                            B2       3
C                            B3       4
C
C        C2v         2       A1       1       C2
C                            A2       2       (sigma)v(xz)
C                            B1       3
C                            B2       4
C
C        C2h         2       Ag       1       i
C                            Bg       2       (sigma)h
C                            Bu       3
C                            Au       4
C
C        D2h         3       Ag       1       (sigma)(xy)
C                            B1g      2       (sigma)(xz)
C                            B2g      3       (sigma)(yz)
C                            B3g      4
C                            Au       5
C                            B1u      6
C                            B2u      7
C                            B3u      8
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IELE(3),ISTA(3),ICHA(34)
      DIMENSION ISCR(3)
      DIMENSION ITAB(*)
      CALL GETDATA(IELE,ISTA,ICHA)
C      data (iele(i),i=1,3) /2,4,8/
C      data (ista(i),i=1,3) /1,3,11/
C      data (icha(i),i=1,34) /1,-1,1,1,1,-1,-1,1,-1,-1,
C     *   1,1,1,1,-1,-1,-1,1,-1,-1,-1,1,-1,-1,-1,-1,1,1,
C     *   1,-1,1,1,1,-1/
C
      IST = ISTA(IDSYM)
      IEL = IELE(IDSYM)
      CALL GTAB1(ICHA(IST),IEL,IDSYM,ISYM1,ITAB,ISCR)
      RETURN
      END
C
C*MODULE ALDECI  *DECK GTAB1
C     ----------------------------------------------------
      SUBROUTINE GTAB1(ICHA,IEL,IDI,ISYM1,ITAB,ISCR)
C     ----------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION ISCR(3)
      DIMENSION ICHA(IDI,IEL)
      DIMENSION ITAB(IEL)
C
      DO 34 II=1,IEL
         DO 45 JJ=1,IEL
            DO 77 KK=1,IDI
               ISCR(KK) = ICHA(KK,II)*ICHA(KK,JJ)
               IF (ISCR(KK).NE.ICHA(KK,ISYM1)) GOTO 45
   77       CONTINUE
            ITAB(II) = JJ
            GOTO 34
   45    CONTINUE
   34 CONTINUE
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK GMUL
C     -------------------------------------------------
      SUBROUTINE GMUL(IDSYM,IMUL,IELE,ISTA,ISCR,ICHA)
C     -------------------------------------------------
C
C     Routine to return multiplication table ixj = imul(i,j)
C     where i,j are irreducible representations.
C     idsym  specifies the point group.
C
C     CONVENTION FOR idsym,i,j and imul
C
C     Point group  idsym  Irred rep  i,j  Sym operation used
C   -----------------------------------------------------------
C        Ci          1       Ag       1        i
C                            Au       2
C
C        Cs          1       A'       1      (sigma)h
C                            A''      2
C
C        C2          1       A        1       C2
C                            B        2
C
C        D2          2       A        1       C2(z)
C                            B1       2       C2(y)
C                            B2       3
C                            B3       4
C
C        C2v         2       A1       1       C2
C                            A2       2       (sigma)v(xz)
C                            B1       3
C                            B2       4
C
C        C2h         2       Ag       1       i
C                            Bg       2       (sigma)h
C                            Bu       3
C                            Au       4
C
C        D2h         3       Ag       1       (sigma)(xy)
C                            B1g      2       (sigma)(xz)
C                            B2g      3       (sigma)(yz)
C                            B3g      4
C                            Au       5
C                            B1u      6
C                            B2u      7
C                            B3u      8
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IELE(3),ISTA(3),ICHA(34)
      DIMENSION ISCR(3)
      DIMENSION IMUL(*)
      CALL GETDATA(IELE,ISTA,ICHA)
C      data (iele(i),i=1,3) /2,4,8/
C      data (ista(i),i=1,3) /1,3,11/
C      data (icha(i),i=1,34) /1,-1,1,1,1,-1,-1,1,-1,-1,
C     *   1,1,1,1,-1,-1,-1,1,-1,-1,-1,1,-1,-1,-1,-1,1,1,
C     *   1,-1,1,1,1,-1/
C
      IST = ISTA(IDSYM)
      IEL = IELE(IDSYM)
      CALL GMUL1(ICHA(IST),IEL,IDSYM,IMUL,ISCR)
      RETURN
      END
C
C*MODULE ALDECI  *DECK MUL1
C     ----------------------------------------------------
      SUBROUTINE GMUL1(ICHA,IEL,IDI,IMUL,ISCR)
C     ----------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION ISCR(IDI)
      DIMENSION ICHA(IDI,IEL)
      DIMENSION IMUL(IEL,IEL)
C
      DO 34 II=1,IEL
         DO 45 JJ=1,IEL
            DO 77 KK=1,IDI
               ISCR(KK) = ICHA(KK,II)*ICHA(KK,JJ)
   77       CONTINUE
            DO 88 KL=1,IEL
               DO 99 LK=1,IDI
                  IF (ISCR(LK).NE.ICHA(LK,KL)) GOTO 88
   99          CONTINUE
               IMUL(II,JJ) = KL
               GOTO 45
   88       CONTINUE
   45    CONTINUE
   34 CONTINUE
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK GETSYM1
C     -------------------------------------------------
      SUBROUTINE GETSYM1(IW,ICON,NACT,NELE,IBO,IDSYM,ISYM,
     *    IELE,ISTA,ISCR,ICHA)
C     -------------------------------------------------
C
C     Routine to return symmetry for a single spin space function.
C     icon(i) contains orbital occupied by electron i.
C     nact    No. of orbitals.
C     nele    No. of electrons.
C     ibo(i) contains symmetry of orbital i.
C     idsym  specifies the point group.
C     isym   returns the symmetry(irreducible rep) of the icon.
C
C     CONVENTION FOR IBO, IDSYM AND ISYM.
C
C     Point group  idsym  Irred rep  isym  Sym operation used
C   -----------------------------------------------------------
C        Ci          1       Ag       1        i
C                            Au       2
C
C        Cs          1       A'       1      (sigma)h
C                            A''      2
C
C        C2          1       A        1       C2
C                            B        2
C
C        D2          2       A        1       C2(z)
C                            B1       2       C2(y)
C                            B2       3
C                            B3       4
C
C        C2v         2       A1       1       C2
C                            A2       2       (sigma)v(xz)
C                            B1       3
C                            B2       4
C
C        C2h         2       Ag       1       i
C                            Bg       2       (sigma)h
C                            Bu       3
C                            Au       4
C
C        D2h         3       Ag       1       (sigma)(xy)
C                            B1g      2       (sigma)(xz)
C                            B2g      3       (sigma)(yz)
C                            B3g      4
C                            Au       5
C                            B1u      6
C                            B2u      7
C                            B3u      8
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IELE(3),ISTA(3),ICHA(34),ISCR(3)
      DIMENSION ICON(NELE),IBO(NACT)
      CALL GETDATA(IELE,ISTA,ICHA)
C
      IST = ISTA(IDSYM)
      IEL = IELE(IDSYM)
      CALL SYM(IW,ICON,NACT,NELE,IBO,ICHA(IST),IDSYM,IEL,ISYM,ISCR)
      RETURN
      END
C
C*MODULE ALDECI  *DECK SYM
C     ---------------------------------------------------------
      SUBROUTINE SYM(IW,ICON,NACT,NELE,IBO,ICHA,IDI,IEL,ISYM,ISCR)
C     ---------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IBO(NACT),ICON(NELE)
      DIMENSION ISCR(3)
      DIMENSION ICHA(IDI,IEL)
C
      DO 7 KK=1,IDI
         ISCR(KK) = 1
    7 CONTINUE
      DO 13 II=1,NELE
         IA = ICON(II)
         DO 20 JJ=1,IDI
            ISCR(JJ) = ISCR(JJ)*ICHA(JJ,IBO(IA))
   20    CONTINUE
   13 CONTINUE
C
      DO 56 II=1,IEL
         DO 89 JJ=1,IDI
            IF (ISCR(JJ).NE.ICHA(JJ,II)) GOTO 56
   89    CONTINUE
         ISYM = II
         RETURN
   56 CONTINUE
C
      WRITE(IW,*) 'ELEMENT NOT IDENTIFIED'
      RETURN
      END
C
C*MODULE ALDECI  *DECK GETDATA
C     ----------------------------------
      SUBROUTINE GETDATA(IELE,ISTA,ICHA)
C     ----------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IELE(3),ISTA(3),ICHA(34)
      IELE(1) = 2
      IELE(2) = 4
      IELE(3) = 8
C
      ISTA(1) = 1
      ISTA(2) = 3
      ISTA(3) = 11
C
      ICHA(1) = 1
      ICHA(2) = -1
      ICHA(3) = 1
      ICHA(4) = 1
      ICHA(5) = 1
      ICHA(6) = -1
      ICHA(7) = -1
      ICHA(8) = 1
      ICHA(9) = -1
      ICHA(10) = -1
      ICHA(11) = 1
      ICHA(12) = 1
      ICHA(13) = 1
      ICHA(14) = 1
      ICHA(15) = -1
      ICHA(16) = -1
      ICHA(17) = -1
      ICHA(18) = 1
      ICHA(19) = -1
      ICHA(20) = -1
      ICHA(21) = -1
      ICHA(22) = 1
      ICHA(23) = -1
      ICHA(24) = -1
      ICHA(25) = -1
      ICHA(26) = -1
      ICHA(27) = 1
      ICHA(28) = 1
      ICHA(29) = 1
      ICHA(30) = -1
      ICHA(31) = 1
      ICHA(32) = 1
      ICHA(33) = 1
      ICHA(34) = -1
      RETURN
      END
C
C*MODULE ALDECI  *DECK PRISYM
C     ---------------------------
      SUBROUTINE PRISYM(IW,IDSYM)
C     ---------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      IF (IDSYM.EQ.0) THEN
         WRITE(IW,*) 'HAVE SPECIFIED C1 SYMMETRY'
      ENDIF
      IF (IDSYM.EQ.1) THEN
      WRITE(IW,*) ' DEFINITIONS OF IRREDUCIBLE REPRESENTATIONS: ',
     *'        1=(1),        2=(-1)'
      ENDIF
      IF (IDSYM.EQ.2) THEN
       WRITE(IW,*) ' DEFINITIONS OF IRREDUCIBLE REPRESENTATIONS: ',
     *'        1=(11),       2=(1-1)'
         WRITE(IW,*) '                                            ',
     *'        3=(-11),      4=(-11)'
      ENDIF
      IF (IDSYM.EQ.3) THEN
      WRITE(IW,*) ' DEFINITIONS OF IRREDUCIBLE REPRESENTATIONS: ',
     *'        1=(111),      2=(1-1-1)'
         WRITE(IW,*) '                                            ',
     *'        3=(-11-1),    4=(-1-11)'
         WRITE(IW,*) '                                            ',
     *'        5=(-1-1-1),   6=(-111)'
         WRITE(IW,*) '                                            ',
     *'        7=(1-11),     8=(11-1)'
      ENDIF
      RETURN
      END
C
C*MODULE ALDECI  *DECK DETCI
C     --------------------------------------------------------
      SUBROUTINE DETCI(IW,SOME,ECONST,SI1,SI2,ISI1,ISI2,
     *         NORB,NCOR,NCI,NA,NB,K,KST,MAXP,MAXW1,NITER,CRIT,
     *         IFA,SPIN,EL,CI,IDS,IWRK,IIS,
     *         IDSYM,ISYM1,NSYM,IOB,NALP,NBLP,ISTAT)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME
C
      DIMENSION SI1(ISI1)
      DIMENSION SI2(ISI2)
      DIMENSION SPIN(KST),EL(MAXW1)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION CI(IDS),IWRK(IIS)
      DIMENSION IOB(NORB-NCOR)
C
C   Code to do determinantal CI, here we partition the memory up
C   and call subroutine davci which is the real guts of the CI.
C   Written by J. Ivanic '97
C
C   si1, si2  : 1 and 2 electron active integrals stored in reverse
C               canonical order, same as GAMESS, if have core orbitals,
C               must have been modified by routine cormod.
C   norb,ncor : total number of orbitals and number of core orbs
C   nci       : number of determinants, size of CI essentially
C   na, nb    : number of active alpha and beta electrons respectively
C   k, kst    : Number of states required and no. of minimum/starting
C               states in CI procedure.
C   maxp      : Maximum number of vectors before transforming and
C               starting at kst.
C   maxw1     : Size of initial diag. for initial guess vectors.
C   niter,crit: Maximum no. of total iterations, convergence criterion.
C               I very strongly suggest crit = 5.0d-5, this gives
C               accuracy to at least 8 decimal places.  niter I would
C               make very large,2000, because I am sure that if you
C               have reasonable orbitals, states will have to converge
C               eventually.
C   ifa       : Contains binomial coefficients, obtained by calling
C               routine memci.
C   spin      : returned spin, spin(i) = spin of state i
C   EL        : EL(i) = total electronic energy of state i
C   CI        : First part of CI contains the returned CI coefficients,
C               ie CI((i-1)*nci + j) contains coefficient for det. j
C               and state i.  The remainder of CI is used for scratch.
C   ids       : Total space for CI in order to do the CI, should be
C               obtained from memci
C   iwrk      : Scratch integer space
C   iis       : Space for iwrk in order to do the CI, should be obtained
C               from memci
C   ehc       : Frozen core energy determined from cormod
C   idsym     : Which point group, see subs gtab, gmul or getsym1
C               in symwrk.f for convention.
C   isym1     : Which irreducible representation, see gtab etc for info.
C   nsym      : nsym = 2**(idsym)
C   iob       : iob(i) contains symmetry of active orbital i, see gtab
C               etc for info.
C   nalp      : Number of alpha space functions, from memci.
C   nblp      : Number of beta space functions, from memci.
C
      NACT = NORB - NCOR
      KCOEF = 1
      KAB = KCOEF + MAXP*NCI
      KQ = KAB + MAXP*NCI
      KB = KQ + NCI
      KEF = KB + 8*MAXW1
      KF = KEF + MAXW1*MAXW1
      KEC = KF + (MAXW1*(MAXW1+1))/2
      KGR = KEC + MAXP*MAXP
      KCITOT = KGR + K
C
C    Now for integer iwrk array
C
      IWRK2 = 1
      IACON1 = IWRK2 + MAXW1
      IBCON1 = IACON1 + NA
      IACON2 = IBCON1 + NA
      IBCON2 = IACON2 + NA
      IPOSA = IBCON2 + NB
      IPERA = IPOSA + (NA*(NACT-NA))
      IIND1 = IPERA + (NA*(NACT-NA))
      IWRK1 = IIND1 + (NA*(NACT-NA))
      ISD = IWRK1 + 2*MAXW1
      ISO = ISD + NA+NB
      INDEX = ISO + NA
      IMMA = INDEX + ((NACT*(NACT+1))/2)**2
      IMMC = IMMA + NSYM*(NA*(NACT-NA))
C
      ISTRB = IMMC + NSYM
      ISTRP = ISTRB + ((NB*(NORB-NCOR-NB))*NBLP)/2
      ISTAR = ISTRP + ((NB*(NORB-NCOR-NB))*NBLP)/2
C
      ISYMA = ISTAR + NBLP
      ISYMB = ISYMA + NALP
      ICOA = ISYMB + NBLP
      ICOB = ICOA + NSYM
      ITAB = ICOB + NSYM
      IMUL = ITAB + NSYM
      ISPA = IMUL + NSYM*NSYM
      ISPB = ISPA + NALP
      ISAS = ISPB + NBLP
      ISBS = ISAS + NSYM+1
      ISAC = ISBS + NSYM+1
      ISBC = ISAC + NALP
      IHMCON = ISBC + NBLP
      ITOT = IHMCON + K
C
      IF (KCITOT.GT.IDS+1.OR.ITOT.GT.IIS+1) THEN
         IF (SOME) THEN
            WRITE(IW,*) 'NOT ENOUGH MEMORY SPECIFIED FOR CI'
            WRITE(IW,*) 'ASKED FOR ',IDS,' AND ',IIS,' DOUBLE '
            WRITE(IW,*) 'PRECISION AND INTEGER, ACTUALLY NEED'
            WRITE(IW,*) KCITOT-1,' AND ',ITOT-1
         ENDIF
         CALL ABRT
      ENDIF
C
C  Set up the symmetry data here
C
C
      CALL SYMWRK(IW,IOB,NACT,NA,NB,IDSYM,ISYM1,NSYM,NALP,NBLP,IWRK,
     *     IWRK(ISYMA),IWRK(ISYMB),IWRK(ICOA),IWRK(ICOB),IWRK(ITAB),
     *     IWRK(IMUL),IWRK(ISPA),IWRK(ISPB),IWRK(ISAS),IWRK(ISBS),
     *     IWRK(ISAC),IWRK(ISBC))
C
      CALL DAVCI(IW,SOME,ECONST,
     *           SI1,SI2,NACT,0,NCI,NA,NB,
     *           CI(KCOEF),SPIN,EL,
     *           K,KST,MAXP,MAXW1,NITER,CRIT,
     *    CI(KAB),CI(KQ),CI(KB),CI(KEF),CI(KF),IWRK(IWRK2),CI(KEC),
     *    IWRK(IACON1),IWRK(IBCON1),IWRK(IACON2),IWRK(IBCON2),
     *    IWRK(IPOSA),IWRK(IPERA),IWRK(IIND1),IWRK(IWRK1),
     *    IWRK(ISD),IWRK(ISO),IFA,IWRK(INDEX),IWRK(IMMA),
     *    IWRK(IMMC),
     *       IWRK(ISYMA),IWRK(ISYMB),
     *       IWRK(ITAB),
     *       IWRK(IMUL),IWRK(ISPA),IWRK(ISPB),
     *       IWRK(ISAS),IWRK(ISBS),IWRK(ISAC),
     *       IWRK(ISBC),NSYM,IOB,NALP,NBLP,IWRK(IHMCON),
     *       CI(KGR),IWRK(ISTRB),IWRK(ISTRP),IWRK(ISTAR),ISTAT)
C
      RETURN
      END
C*MODULE ALDECI  *DECK SETUP
C     --------------------------------------------------------
      SUBROUTINE SETUP(NORB,NCOR,NA,NB,IBCON1,IACON2,IFA,INDEX,
     *       ISPB,NBLP,ISTRB,ISTRP,ISTART)
C     --------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION IBCON1(NA+NCOR)
      DIMENSION IACON2(NA+NCOR)
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2)
      DIMENSION ISPB(NBLP)
      DIMENSION ISTRB((NBLP*(NB*(NORB-NCOR-NB)))/2)
      DIMENSION ISTRP((NBLP*(NB*(NORB-NCOR-NB)))/2),ISTART(NBLP)
C
      NACT = NORB - NCOR
C      nat = na + ncor
      NBT = NB + NCOR
C
C  Assume that we have ifa already calculated.
C
      DO 7 I=1,(NORB*(NORB+1))/2
         DO 8 J=1,I
            INDEX(I,J) = I*(I-1)/2 + J
            INDEX(J,I) = INDEX(I,J)
    8    CONTINUE
    7 CONTINUE
C
C ************
C HERE WE SET UP STORAGE OF EXCITED BETA STRINGS.  SO WE SKIP
C THE ALPHA PART COMPLETELY.
C
      ICOUB = 0
C
      DO 876 JJI=1,NBT
         IBCON1(JJI) = JJI
  876 CONTINUE
C
      DO 9999 IJK = 1,NBLP
      ISTART(IJK) = ICOUB + 1
Cc Position of beta det
C      icab = ispb(ijk)
Cc Symmetry of beta det
C      isb1 = isymb(ijk)
C      itb1 = itab(isb1)
Cc
C
C   Single Beta excitations
C
      DO 6030 IB=NCOR+1,NBT
         IO1 = IBCON1(IB)
Cc Symmetry of orbital being deoccupied
C is1 = iox(io1)
Cc
         IST = IO1+1
         IEN = IBCON1(IB+1)-1
         IF (IB.EQ.NBT) IEN=NORB
         DO 6025 KKJ=IB-NCOR+1,NB+1
            DO 6020 JJ=IST,IEN
Cc Symmetry of orbital being occupied
C is2 = iox(jj)
Cc is1xis2 = ip1
C ip1 = imul(is2,is1)
Cc
            CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
            IPER = ((-1)**IPER1)
C    ind = index(jj,io1)
            IPB1 = POSDET(NACT,NB,IACON2,IFA)
            ICOUB = ICOUB + 1
            ISTRB(ICOUB) = ISPB(IPB1)
            ISTRP(ICOUB) =IPER
C
 6020       CONTINUE
            IST = IBCON1(KKJ+NCOR)+1
            IEN=IBCON1(NCOR+KKJ+1)-1
            IF (KKJ.EQ.NB) IEN=NORB
 6025     CONTINUE
 6030 CONTINUE
C
           CALL ADVANC(IBCON1,NBT,NORB)
C
 9999 CONTINUE
      RETURN
      END
C
C*MODULE ALDECI  *DECK MEMPRI
C    ---------------------------------------------------------------
      SUBROUTINE MEMPRI(IFA,NA,NB,NACT,NSYM,IPRMEM)
C    ---------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IFA(0:NACT,0:NACT)
      NALP = IFA(NACT,NA)
      NBLP = IFA(NACT,NB)
      IPRMEM = 3*NA + 43 + NALP*3 + NBLP*3 + (NSYM+1)*2 +
     *       NSYM*3 + NSYM*NSYM
      RETURN
      END
C
C*MODULE ALDECI  *DECK PRICI1
C    ---------------------------------------------------------------
      SUBROUTINE PRICI1(IW,IFA,NA,NB,NCOR,NORB,CI,NCI,IOB,
     *                  IOP,CRIT,NUM,IDSYM,ISYM1,NSYM,IWRK,IMEM)
C    ---------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR),CI(NCI)
      DIMENSION IWRK(IMEM)
      DIMENSION IOB(NORB-NCOR)
      CHARACTER*102 CONA,CONB
C
      CALL PRICI2(IW,IFA,NA,NB,NCOR,NORB,CI,NCI,IWRK(1),IWRK(NA+1),
     *        CONA,CONB,IOP,CRIT,NUM,IOB,IWRK(2*NA+1),IMEM-2*NA,
     *       IDSYM,ISYM1,NSYM)
      RETURN
      END
C
C*MODULE ALDECI  *DECK PRICI2
C     -----------------------------------------------------------
      SUBROUTINE PRICI2(IW,IFA,NA,NB,NCOR,NORB,CI,NCI,IACON,IBCON,
     *                 CONA,CONB,IOP,CRIT,NUM,IOB,ISYD,MSYD,
     *            IDSYM,ISYM1,NSYM)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL GOPARR,DSKWRK,MASWRK
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR),CI(*)
      DIMENSION IACON(NA),IBCON(NA)
      DIMENSION ISYD(MSYD),IOB(NORB-NCOR)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
#if defined(SPEC_CPU)
      COMMON /IGFILE/ IGV
#endif
      CHARACTER*102 CONA,CONB
C
C    This subroutine prints out required part of the determinantal
C    wavefunction specified by the coefficients in CI.
C
C   Should be compiled with the ALDeC CI code as it needs the subroutine
C   advanc to run properly.
C
C    ifa contains binomial coefficients, should be returned intact
C    from subroutine detci
C    na, nb are numbers of active alpha and beta electrons respectively
C    ncor is number of core orbitals
C    norb is total number of orbitals
C    CI contains the CI coefficients, THIS IS SOMEWHAT DESTROYED, so
C    your best bet is to copy the vectors.  If you can do the CI
C    in the first place then you have enough spare space to copy all
C    vectors, trust me.  Note that you need the space to store A.b in
C    a CI.
C    iacon, ibcon are scratch integer arrays
C    cona, conb are characters.  At the moment it is dimensioned
C    for a maximum of 100 active orbitals.  If anytime soon someone
C    does a bigger CI then you have to do character*(nact+2) where
C    nact is number of active orbitals.
C    iop is a choice paramter
C      iop=1 prints out the largest (num) CI coefs with determinants.
C      iop=2 prints out all dets with CI coeff larger than crit
C    crit and num are explained above.
C    isyd = integer array with the symmetry information.  This is
C    obtained with subroutine symwrk where isyd contains all the
C    arrays consecutively in one big one.  One can also use the
C    integer array returned from detci but starting at 'isst' where
C    isst is returned from memci, see memci for more details.
C    nsym = 2**(idsym) where idsym determines the point group, see
C    symwrk.f, subroutine gtab for convention.  nsym = total number
C    of irreducible representations.
C    nalp,nblp are numbers of alpha and beta space functions
C
      NACT = NORB - NCOR
      IF(NACT.GT.100) THEN
         IF(MASWRK) WRITE(IW,*)
     *      'PRICI2: TOO MANY ACTIVE ORBITALS TO PRINT CI VECTOR'
         RETURN
      END IF
      NALP = IFA(NACT,NA)
      NBLP = IFA(NACT,NB)
C
      IZ0 = 0
      IZ1 = IZ0 + (NA+43)
      IZ2 = IZ1 + NALP
      IZ3 = IZ2 + NBLP
      IZ4 = IZ3 + NSYM
      IZ5 = IZ4 + NSYM
      IZ6 = IZ5 + NSYM
      IZ7 = IZ6 + NSYM*NSYM
      IZ8 = IZ7 + NALP
      IZ9 = IZ8 + NBLP
      IZ10 = IZ9 + NSYM+1
      IZ11 = IZ10 + NSYM+1
      IZ12 = IZ11 + NALP
      IZTOT = IZ12 + NBLP
C
      IF (MSYD.LT.IZTOT) THEN
         IF(MASWRK) WRITE(IW,*) 'NOT ENOUGH MEMORY FOR PRINTING'
         CALL ABRT
      ENDIF
C
      CALL SYMWRK(IW,IOB,NACT,NA,NB,IDSYM,ISYM1,NSYM,NALP,NBLP,
     *      ISYD(IZ0+1),
     *     ISYD(IZ1+1),ISYD(IZ2+1),ISYD(IZ3+1),ISYD(IZ4+1),
     *     ISYD(IZ5+1),
     *     ISYD(IZ6+1),ISYD(IZ7+1),ISYD(IZ8+1),ISYD(IZ9+1),
     *     ISYD(IZ10+1),
     *     ISYD(IZ11+1),ISYD(IZ12+1))
C
      NACT = NORB-NCOR
      DO II=1,NACT+2
         CONA(II:II) = ' '
         CONB(II:II) = ' '
      ENDDO
C
      IF (IOP.EQ.1) THEN
C
C  Set up the table
C
         IA = (NACT+2)/2 - 2
         IF (IA.LE.0) IA = 1
         CONA(IA:IA+4) = 'ALPHA'
         CONB(IA:IA+4) = 'BETA '
         write (*, *) 'jray: i am here 1'
         IF(MASWRK) WRITE(IW,'(4A)') CONA(1:NACT+2),'|',
     *                               CONB(1:NACT+2),'| COEFFICIENT'
         DO 45 II=1,NACT+2
            CONA(II:II) = '-'
   45    CONTINUE
         IF(MASWRK) WRITE(IW,'(4A)') CONA(1:NACT+2),'|',
     *                               CONA(1:NACT+2),'|------------'
C
      DO 3000 KJK=1,NUM
C
         ICI = 0
         IPOS = -1
         PMAX = 0.0D+00
C
         DO 413 IJK=1,NALP
C   do 313 kji=1,nblp
         ISA1 = ISYD(IZ1 + IJK)
            DO 313 KJI = ISYD(IZ10 + ISA1),ISYD(IZ10 + ISA1 + 1)-1
               NEND = ISYD(IZ12 + KJI)
C
               ICI = ICI + 1
               IF (ABS(CI(ICI)).GT.PMAX) THEN
                  INDA = IJK
                  INDB = NEND
                  IPOS = ICI
                  PMAX = ABS(CI(ICI))
               ENDIF
  313       CONTINUE
  413    CONTINUE
         IF (IPOS.EQ.-1) GOTO 3000
C
C   Now to print out the determinant
C
      DO 50 II=1,NA
         IACON(II) = II
   50 CONTINUE
      DO 40 II=1,NB
         IBCON(II) = II
   40 CONTINUE
      DO 67 II=1,INDA-1
         CALL ADVANC(IACON,NA,NACT)
   67 CONTINUE
      DO 77 II=1,INDB-1
         CALL ADVANC(IBCON,NB,NACT)
   77 CONTINUE
C
      CONA(1:1) = ' '
      CONB(1:1) = ' '
      DO II=2,NACT+1
         CONA(II:II) = '0'
         CONB(II:II) = '0'
      ENDDO
C
      DO 82 II=1,NA
         CONA(IACON(II)+1:IACON(II)+1) = '1'
   82 CONTINUE
      DO 92 II=1,NB
         CONB(IBCON(II)+1:IBCON(II)+1) = '1'
   92 CONTINUE
C
      CONA(NACT+2:NACT+2) = ' '
      CONB(NACT+2:NACT+2) = ' '
      IF(MASWRK) WRITE(IW,'(4A,F10.7)') CONA(1:NACT+2),'|',
     *                                  CONB(1:NACT+2),'|  ',CI(IPOS)
      CI(IPOS) = 0.0D+00
C
 3000 CONTINUE
C
      ELSE
C
C  Set up the table
C
         IA = (NACT+2)/2 - 2
         IF (IA.LE.0) IA = 1
         CONA(IA:IA+4) = 'ALPHA'
         CONB(IA:IA+4) = 'BETA '
#if defined(SPEC_CPU)
         write(IW, '(A)') 'Only the coefficients are validated.'
         write(IW, '(A)') 'The full determinant is printed in *.eigenv.t
     *xt.'
         IF(MASWRK) WRITE(IW,'(4A)')'COEFFICIENT'
         IF(MASWRK) WRITE(IGV,'(4A)') CONA(1:NACT+2),'|',
     *                               CONB(1:NACT+2),'| COEFFICIENT'
#else
         IF(MASWRK) WRITE(IW,'(4A)') CONA(1:NACT+2),'|',
     *                               CONB(1:NACT+2),'| COEFFICIENT'
#endif
         DO 47 II=1,NACT+2
            CONA(II:II) = '-'
   47    CONTINUE
#if defined(SPEC_CPU)
         IF(MASWRK) WRITE(IW,'(4A)') '------------'
         IF(MASWRK) WRITE(IGV,'(4A)') CONA(1:NACT+2),'|',
     *                               CONA(1:NACT+2),'|------------'
#else
         IF(MASWRK) WRITE(IW,'(4A)') CONA(1:NACT+2),'|',
     *                               CONA(1:NACT+2),'|------------'
#endif
C
      DO 4000 KJK=1,NCI
C
         ICI = 0
         DO 113 II=1,NA
            IACON(II) = II
  113    CONTINUE
         PMAX = 0.0D+00
C
         DO 415 IJK=1,NALP
            ISA1 = ISYD(IZ1 + IJK)
C
            DO 18 II=1,NB
               IBCON(II) = II
   18       CONTINUE
C
            DO 315 KJI = ISYD(IZ10 + ISA1),ISYD(IZ10+ISA1+1)-1
               NEND = ISYD(IZ12 + KJI)
C
               ICI = ICI + 1
               IF (ABS(CI(ICI)).GT.PMAX) THEN
                  INDA = IJK
                  INDB = NEND
                  IPOS = ICI
                  PMAX = ABS(CI(ICI))
               ENDIF
C
  315       CONTINUE
  415    CONTINUE
C
C  Check if is bigger than crit
C
      IF (ABS(CI(IPOS)).GE.CRIT) THEN
C
C   Now to print out the determinant
C
      DO 150 II=1,NA
         IACON(II) = II
  150 CONTINUE
      DO 140 II=1,NB
         IBCON(II) = II
  140 CONTINUE
      DO 167 II=1,INDA-1
         CALL ADVANC(IACON,NA,NACT)
  167 CONTINUE
      DO 177 II=1,INDB-1
         CALL ADVANC(IBCON,NB,NACT)
  177 CONTINUE
C
      CONA(1:1) = ' '
      CONB(1:1) = ' '
      DO II=2,NACT+1
         CONA(II:II) = '0'
         CONB(II:II) = '0'
      ENDDO
C
      DO 182 II=1,NA
         CONA(IACON(II)+1:IACON(II)+1) = '1'
  182 CONTINUE
      DO 192 II=1,NB
         CONB(IBCON(II)+1:IBCON(II)+1) = '1'
  192 CONTINUE
C
      CONA(NACT+2:NACT+2) = ' '
      CONB(NACT+2:NACT+2) = ' '
#if defined(SPEC_CPU)
C SPEC: Only print CI(IPOS) to IW (stdout) (which will be validated).
C  The full determinant C is printed to '*.eigenv.txt'
C Why? Because the CI(*) elements are sorted in descending
C  order of their absolute values, and for one input, the
C last two elements are very very close. With different levels
C of optimizations, the ordering becomes slightly different,
C so the CONA(*) and CONB(*) do not match!
C
      IF(MASWRK) WRITE(IW,'(F10.7)') CI(IPOS)
      IF(MASWRK) WRITE(IGV,'(4A,F10.7)') CONA(1:NACT+2),'|',
     *                                  CONB(1:NACT+2),'|  ',CI(IPOS)
#else
      IF(MASWRK) WRITE(IW,'(4A,F10.7)') CONA(1:NACT+2),'|',
     *                                  CONB(1:NACT+2),'|  ',CI(IPOS)
#endif
      CI(IPOS) = 0.0D+00
C
      GOTO 4000
      ENDIF
      RETURN
C
 4000 CONTINUE
C
      ENDIF
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK MATME1
C     -----------------------------------------------
      SUBROUTINE MATME1(NORB,NCOR,NA,NB,IFA,NSYM,IIS)
C     -----------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR)
C
      NACT = NORB-NCOR
      NALP = IFA(NACT,NA)
      NBLP = IFA(NACT,NB)
C
      IIS = 3*NA + NB + 3*(NA*(NACT-NA)) + NACT*NACT +
     *      3*NALP + 3*NBLP + 3*NSYM + 2*(NSYM+1) + NSYM*NSYM +43
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK MATRD1
C     --------------------------------------------------------
      SUBROUTINE MATRD1(IW,DEN,M2,NORB,NCOR,NA,NB,CI,NCI,
     *                  IFA,IWRK,IIS,IOX,IDSYM,ISYM1,NSYM)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION CI(NCI),DEN(M2)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION IOX(NORB-NCOR)
      DIMENSION IWRK(IIS)
C
C     Must be compiled with the subroutines ret1det,advanc and posdet.
C     These are contained in the file retAb.f.  Also symwrk.f
C     We assume here that we have a full space and the determinants are
C     ordered according to molpro convention, a reference for
C     this ordering is
C         Knowles and Handy, Chem. Phys. Lett. 315,vol 111, '84.
C
C     Returns the 1 particle density matrix in den
C     Assumes full symmetry in terms
C     of indices, ie i.ge.j, k.ge.l ij.ge.kl for <i|j> and [ij|kl].
C     norb = total number of orbitals
C     ncor = number of core orbitals
C     na = number of active alpha electrons
C     nb = number of active beta electrons
C     CI is an array containing the coefficients of the determinants.
C     nci is number of determinants.
C     ifa is a scratch array of dimension above.  It should contain
C     a list of binomial coeffs.
C
C     iwrk is scratch and contains some important information,
C     it is regenerated.
C     iis is the size of iwrk, see memci and detci for more.
C     idsym specifies the point group, see subroutine gtab in symwrk.f
C     isym1 specifies the irrep, see gtab in symwrk.f
C     nsym = 2**(idsym)
C     nalp, nblp are no. of alpha and beta space functions respecively
C
      NACT = NORB - NCOR
      NALP = IFA(NACT,NA)
      NBLP = IFA(NACT,NB)
C
      IACON1 = 1
      IBCON1 = IACON1 + NA + 43
      IACON2 = IBCON1 + NA
      IBCON2 = IACON2 + NA
      IPOSA = IBCON2 + NB
      IPERA = IPOSA + (NA*(NACT-NA))
      IIND1 = IPERA + (NA*(NACT-NA))
      INDEX = IIND1 + (NA*(NACT-NA))
      ISYMA = INDEX + NACT*NACT
      ISYMB = ISYMA + NALP
      ICOA = ISYMB + NBLP
      ICOB = ICOA + NSYM
      ITAB = ICOB + NSYM
      IMUL = ITAB + NSYM
      ISPA = IMUL + NSYM*NSYM
      ISPB = ISPA + NALP
      ISAS = ISPB + NBLP
      ISBS = ISAS + NSYM+1
      ISAC = ISBS + NSYM+1
      ISBC = ISAC + NALP
      ITOT = ISBC + NBLP - 1
C
      IF (ITOT.GT.IIS) THEN
         WRITE(IW,*) 'NOT ENOUGH MEMORY SPECIFIED FOR IWRK'
         WRITE(IW,*) 'ASKED FOR ',IIS,' NEED ',ITOT
         CALL ABRT
      ENDIF
C
      CALL SYMWRK(IW,IOX,NACT,NA,NB,IDSYM,ISYM1,NSYM,NALP,NBLP,IWRK,
     *     IWRK(ISYMA),IWRK(ISYMB),IWRK(ICOA),IWRK(ICOB),IWRK(ITAB),
     *     IWRK(IMUL),IWRK(ISPA),IWRK(ISPB),IWRK(ISAS),IWRK(ISBS),
     *     IWRK(ISAC),IWRK(ISBC))
C
C
      CALL MATR82(DEN,M2,NACT,0,NA,NB,CI,NCI,NALP,NBLP,
     *           IFA,IOX,NSYM,
     *         IWRK(IACON1),IWRK(IBCON1),IWRK(IACON2),
     *         IWRK(IPOSA),IWRK(IPERA),IWRK(IIND1),
     *         IWRK(INDEX),
     *        IWRK(ISYMA),IWRK(ISYMB),
     *        IWRK(ISPA),IWRK(ISPB),
     *        IWRK(ISAS),IWRK(ISBS),IWRK(ISAC),IWRK(ISBC))
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK MATR82
C     --------------------------------------------------------
      SUBROUTINE MATR82(DEN,M2,NORB,NCOR,NA,NB,CI,NCI,NALP,NBLP,
     *              IFA,IOX,NSYM,
     *            IACON1,IBCON1,IACON2,IPOSA,IPERA,IIND1,
     *            INDEX,ISYMA,ISYMB,
     *            ISPA,ISPB,ISAS,ISBS,ISAC,ISBC)
C     --------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
      DIMENSION IOX(NORB-NCOR)
      DIMENSION DEN(M2), CI(NCI)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION IACON1(NA+NCOR),IBCON1(NA+NCOR)
      DIMENSION IACON2(NA+NCOR),IPERA(NA*(NORB-NCOR-NA))
      DIMENSION IIND1(NA*(NORB-NCOR-NA))
      DIMENSION IPOSA(NA*(NORB-NCOR-NA))
      DIMENSION INDEX(NORB,NORB)
      DIMENSION ISYMA(NALP),ISYMB(NBLP)
      DIMENSION ISPA(NALP),ISPB(NBLP)
      DIMENSION ISAS(NSYM+1),ISBS(NSYM+1),ISAC(NALP),ISBC(NBLP)
C
      DO 7 I=1,NORB
         DO 8 J=1,I
            INDEX(I,J) = I*(I-1)/2 + J
            INDEX(J,I) = INDEX(I,J)
    8    CONTINUE
    7 CONTINUE
C
      NACT = NORB - NCOR
      NAT = NA + NCOR
      NBT = NB + NCOR
C
      NTOT = NORB*(NORB+1)/2
      DO 13 I=1,NTOT
         DEN(I) = 0.0D+00
   13 CONTINUE
C
      DO 30 I=1,NAT
         IACON1(I) = I
   30 CONTINUE
C
C   Big Loop over all alpha determinants
C
      DO 9000 IJK = 1,NALP
C
C  Alpha excitations here
C   Single first
C
         IAC = 0
         ICAT = ISPA(IJK)
         ISA1 = ISYMA(IJK)
C itas = itab(isa1)
         DO 7030 IA=NCOR+1,NAT
             IO1 = IACON1(IA)
             IS1 = IOX(IO1)
             IST = IO1 + 1
             IEN = IACON1(IA+1)-1
             IF (IA.EQ.NAT) IEN=NORB
             DO 7025 KKJ=IA-NCOR+1,NA+1
                DO 7020 JJ=IST,IEN
                IS2 = IOX(JJ)
C        ip1 = imul(is2,is1)
C
             IAC = IAC + 1
             CALL RET1DET(IACON1,IACON2,NA,IA,JJ,NCOR,KKJ,IPER1)
C
C   Storage here for later use, well worth it
C
             IPET = POSDET(NACT,NA,IACON2,IFA)
             IPOSA(IAC) = ISPA(IPET)
             IPERA(IAC) = ((-1)**IPER1)
             IND = INDEX(JJ,IO1)
             IIND1(IAC) = IND
Cc If deoccupied and newly occupied orbitals are of diff symm,
C   skip to doubles
             IF (IS1.NE.IS2) GOTO 417
C
         DO 49 I=1,NBT
            IBCON1(I) = I
   49    CONTINUE
C
C
C                  do 407 inb1 = 1,nblp
C    Loop over beta dets of the right symmetry
                   DO 407 INBB = ISBS(ISA1),ISBS(ISA1+1)-1
                   INB1 = ISBC(INBB)
                   ICIT = ICAT+ISPB(INB1)
                     ICI2 = IPOSA(IAC) + ISPB(INB1)
CState average here
            FC = CI(ICIT)*CI(ICI2)
C
           FC = FC*IPERA(IAC)
C
C
                   DEN(IND) = DEN(IND) + FC
  407          CONTINUE
C
C
  417     CONTINUE
C
 7020           CONTINUE
                IST = IACON1(KKJ+NCOR)+1
                IEN = IACON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NA) IEN=NORB
 7025        CONTINUE
 7030     CONTINUE
C
C  Diagonal
C
            DO 67 II=1,NAT
               I1 = IACON1(II)
               IND1 = INDEX(I1,I1)
C
              DO 53 INB1 = ISBS(ISA1),ISBS(ISA1+1)-1
              NEND = ISBC(INB1)
              ICIT = ICAT+ISPB(NEND)
C State average here
              FC = CI(ICIT)*CI(ICIT)
C
               DEN(IND1) = DEN(IND1) + FC
   53         CONTINUE
C
   67       CONTINUE
C
C   Loop over Beta dets now
C
      CALL ADVANC(IACON1,NAT,NORB)
 9000 CONTINUE
C
C   Now for the Beta part
C
C
      DO 876 JJI=1,NBT
         IBCON1(JJI) = JJI
  876 CONTINUE
C
      DO 9999 IJK = 1,NBLP
         ICAB = ISPB(IJK)
         ISB1 = ISYMB(IJK)
C itb1 = itab(isb1)
C
      DO 6030 IB=NCOR+1,NBT
         IST = IBCON1(IB)+1
         IEN = IBCON1(IB+1)-1
         IF (IB.EQ.NBT) IEN=NORB
         IO1 = IBCON1(IB)
         IS1 = IOX(IO1)
         DO 6025 KKJ=IB-NCOR+1,NB+1
            DO 6020 JJ=IST,IEN
            IS2 = IOX(JJ)
C    ip1 = imul(is2,is1)
C
            CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
            IPER = ((-1)**IPER1)
            IND = INDEX(JJ,IO1)
Cc If deoccupied and newly occupied are of diff sym, then
C   skip to doubles
            IF (IS1.NE.IS2) GOTO 517
            IPB1 = POSDET(NACT,NB,IACON2,IFA)
            IPB1 = ISPB(IPB1)
C
       DO 89 I=1,NAT
          IACON1(I) = I
   89 CONTINUE
C
C  Loop over alpha
C
          DO 907 INA1 = ISAS(ISB1),ISAS(ISB1+1)-1
             ICIA = ISAC(INA1)
             ICIT = ISPA(ICIA) + ICAB
             ICI2 = ISPA(ICIA) + IPB1
CState average
            FC = CI(ICIT)*CI(ICI2)
             FC = FC*IPER
C
             DEN(IND) = DEN(IND) + FC
  907     CONTINUE
C
  517 CONTINUE
 6020      CONTINUE
            IST = IBCON1(KKJ+NCOR)+1
            IEN=IBCON1(NCOR+KKJ+1)-1
            IF (KKJ.EQ.NB) IEN=NORB
 6025     CONTINUE
 6030 CONTINUE
C
C    Remaining part of diagonal contributions
C
            DO 69 II=1,NBT
               I1 = IBCON1(II)
               IND1 = INDEX(I1,I1)
            DO 93 INA1 = ISAS(ISB1),ISAS(ISB1+1)-1
              NEND = ISAC(INA1)
              ICIA = ISPA(NEND)
              ICIT = ICIA + ICAB
CState average
          FC =  CI(ICIT)*CI(ICIT)
C
              DEN(IND1) = DEN(IND1) + FC
   93       CONTINUE
C
C
   69       CONTINUE
           CALL ADVANC(IBCON1,NBT,NORB)
C
 9999 CONTINUE
      RETURN
      END
C
C*MODULE ALDECI  *DECK MATME2
C     -----------------------------------------------
      SUBROUTINE MATME2(NORB,NCOR,NA,NB,IFA,NSYM,IIS)
C     -----------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR)
C
      NACT = NORB - NCOR
      NALP = IFA(NACT,NA)
      NBLP = IFA(NACT,NB)
C
      IIS = 3*NA + NB + 3*(NA*(NACT-NA)) +
     *  ((NACT*(NACT+1))/2)**2 + 3*(NALP+NBLP) + 4*NSYM +
     *   2*(NSYM+1) + NSYM*NSYM + NSYM*(NA*(NACT-NA)) +
     *  ((NB*(NORB-NCOR-NB))*NBLP) + NBLP + 43
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK MATRDS
C     --------------------------------------------------------
      SUBROUTINE MATRDS(IW,DEN,DEN2,NORB,NCOR,NA,NB,CI,NCI,
     *               IFA,IWRK,IIS,IOX,IDSYM,ISYM1,NSYM,
     *               NSTATE,IWTS,WSTATE,IWH,W,MXSTAT)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION CI(NCI,MXSTAT),DEN(*),DEN2(*)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION IOX(NORB-NCOR)
      DIMENSION IWRK(IIS)
      DIMENSION IWH(NSTATE),W(NSTATE),IWTS(*),WSTATE(*)
C
C     Must be compiled with the routines ret1det,advanc and posdet.
C     These are contained in the file retAb.f.  Also symwrk.f
C     We assume here that we have a full space and the determinants are
C     ordered according to molpro convention, a reference for
C     this ordering is
C        Knowles and Handy, Chem. Phys. Lett. 315,vol 111, '84.
C
C     Returns the state averaged 1 and 2 particle density matrices in
C     den and den2 respectively.  Assumes full symmetry in terms
C     of indices, ie i.ge.j, k.ge.l ij.ge.kl for <i|j> and [ij|kl].
C     norb = total number of orbitals
C     ncor = number of core orbitals
C     na = number of active alpha electrons
C     nb = number of active beta electrons
C     CI is an array containing the coefficients of the determinants.
C     nci is number of determinants.
C     ifa is a scratch array of dimension above.  It should contain
C     a list of binomial coeffs.
C
C     iwrk is scratch and contains some important information, it
C     is regenerated.
C     iis is the size of iwrk, see memci and detci for more.
C     idsym specifies the point group, see subroutine gtab in symwrk.f
C     isym1 specifies the irrep, see gtab in symwrk.f
C     nsym = 2**(idsym)
C
C     nstate is the number of states to be averaged
C     iwh(i) contains which state the ith one is. ie, which CI vector
C     W(i) contains the weight of vector iwh(i)
C     nxstate = iwh(nstate) and is only given for dimensioning purposes.
C
C  Now the density matrices are returned such that
C  SUM(den(i)*sint1(i)) + SUM(den2(i)*sint2(i))
C  is the total electronic energy
C
      NACT = NORB - NCOR
      NALP = IFA(NACT,NA)
      NBLP = IFA(NACT,NB)
C
      IACON1 = 1
      IBCON1 = IACON1 + NA + 43
      IACON2 = IBCON1 + NA
      IBCON2 = IACON2 + NA
      IPOSA = IBCON2 + NB
      IPERA = IPOSA + (NA*(NACT-NA))
      IIND1 = IPERA + (NA*(NACT-NA))
      INDEX = IIND1 + (NA*(NACT-NA))
      IMMA = INDEX + ((NACT*(NACT+1))/2)**2
      IMMC = IMMA + NSYM*(NA*(NACT-NA))
      ISYMA = IMMC + NSYM
      ISYMB = ISYMA + NALP
      ICOA = ISYMB + NBLP
      ICOB = ICOA + NSYM
      ITAB = ICOB + NSYM
      IMUL = ITAB + NSYM
      ISPA = IMUL + NSYM*NSYM
      ISPB = ISPA + NALP
      ISAS = ISPB + NBLP
      ISBS = ISAS + NSYM+1
      ISAC = ISBS + NSYM+1
      ISBC = ISAC + NALP
      ISTRB = ISBC + NBLP
      ISTRP = ISTRB + ((NB*(NORB-NCOR-NB))*NBLP)/2
      ISTAR = ISTRP + ((NB*(NORB-NCOR-NB))*NBLP)/2
      ITOT = ISTAR + NBLP - 1
C
      CALL SYMWRK(IW,IOX,NACT,NA,NB,IDSYM,ISYM1,NSYM,NALP,NBLP,IWRK,
     *     IWRK(ISYMA),IWRK(ISYMB),IWRK(ICOA),IWRK(ICOB),IWRK(ITAB),
     *     IWRK(IMUL),IWRK(ISPA),IWRK(ISPB),IWRK(ISAS),IWRK(ISBS),
     *     IWRK(ISAC),IWRK(ISBC))
C
      IF (ITOT.GT.IIS) THEN
         WRITE(IW,*) 'NOT ENOUGH MEMORY SPECIFIED FOR IWRK'
         WRITE(IW,*) 'ASKED FOR ',IIS,' NEED ',ITOT
         CALL ABRT
      ENDIF
C
      CALL SETUP(NORB,NCOR,NA,NB,IWRK(IBCON1),
     *   IWRK(IACON2),IFA,IWRK(INDEX),IWRK(ISPB),NBLP,
     *    IWRK(ISTRB),IWRK(ISTRP),IWRK(ISTAR))
C
      CALL MATRD2(DEN,DEN2,NACT,0,NA,NB,CI,NCI,NALP,NBLP,
     *           IFA,NSTATE,IWTS,WSTATE,IWH,W,MXSTAT,
     *         IOX,NSYM,
     *         IWRK(IACON1),IWRK(IBCON1),IWRK(IACON2),
     *         IWRK(IPOSA),IWRK(IPERA),IWRK(IIND1),
     *         IWRK(INDEX),IWRK(IMMA),IWRK(IMMC),
     *        IWRK(ISYMA),IWRK(ISYMB),
     *        IWRK(ITAB),IWRK(IMUL),IWRK(ISPA),IWRK(ISPB),
     *        IWRK(ISAS),IWRK(ISBS),IWRK(ISAC),IWRK(ISBC),
     *       IWRK(ISTRB),IWRK(ISTRP),IWRK(ISTAR))
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK MATRD2
C     --------------------------------------------------------
      SUBROUTINE MATRD2(DEN,DEN2,NORB,NCOR,NA,NB,CI,NCI,NALP,NBLP,
     *              IFA,NSTATE,IWTS,WSTATE,IWH,W,MXSTAT,
     *            IOX,NSYM,
     *            IACON1,IBCON1,IACON2,IPOSA,IPERA,IIND1,
     *            INDEX,IMMA,IMMC,ISYMA,ISYMB,
     *            ITAB,IMUL,ISPA,ISPB,ISAS,ISBS,ISAC,ISBC,
     *            ISTRB,ISTRP,ISTART)
C     --------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
      DIMENSION IWH(NSTATE),W(NSTATE),IWTS(*),WSTATE(*)
      DIMENSION IOX(NORB-NCOR)
      DIMENSION DEN(*), DEN2(*), CI(NCI,MXSTAT)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION IACON1(NA+NCOR),IBCON1(NA+NCOR)
      DIMENSION IACON2(NA+NCOR),IPERA(NA*(NORB-NCOR-NA))
      DIMENSION IIND1(NA*(NORB-NCOR-NA))
      DIMENSION IPOSA(NA*(NORB-NCOR-NA))
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2)
      DIMENSION IMMA(NSYM,(NA*(NORB-NCOR-NA))),IMMC(NSYM)
      DIMENSION ISYMA(NALP),ISYMB(NBLP)
      DIMENSION ITAB(NSYM),IMUL(NSYM,NSYM)
      DIMENSION ISPA(NALP),ISPB(NBLP)
      DIMENSION ISAS(NSYM+1),ISBS(NSYM+1),ISAC(NALP),ISBC(NBLP)
      DIMENSION ISTRB((NBLP*(NB*(NORB-NCOR-NB)))/2)
      DIMENSION ISTRP((NBLP*(NB*(NORB-NCOR-NB)))/2)
      DIMENSION ISTART(NBLP)
C
      ICONST = 2
      IF (NA.EQ.NB) ICONST = 4
      DO 1 IST=1,NSTATE
          W(IST) = WSTATE(IWTS(IST))
         IWH(IST) = IST
    1 CONTINUE
C
      DO 7 I=1,(NORB*(NORB+1))/2
         DO 8 J=1,I
            INDEX(I,J) = I*(I-1)/2 + J
            INDEX(J,I) = INDEX(I,J)
    8    CONTINUE
    7 CONTINUE
C
      NACT = NORB - NCOR
      NAT = NA + NCOR
      NBT = NB + NCOR
C
      NTOT = NORB*(NORB+1)/2
      NTOT2 = NTOT*(NTOT+1)/2
      DO 13 I=1,NTOT
         DEN(I) = 0.0D+00
   13 CONTINUE
      DO 15 I=1,NTOT2
         DEN2(I) = 0.0D+00
   15 CONTINUE
C
      DO 30 I=1,NAT
         IACON1(I) = I
   30 CONTINUE
C
C   Big Loop over all alpha determinants
C
      DO 9000 IJK = 1,NALP
C
C  Alpha excitations here
C   Single first
C
         IAC = 0
         DO 45 II=1,NSYM
            IMMC(II) = 0
   45    CONTINUE
         ICAT = ISPA(IJK)
         ISA1 = ISYMA(IJK)
         ITAS = ITAB(ISA1)
         DO 7030 IA=NCOR+1,NAT
             IO1 = IACON1(IA)
             IS1 = IOX(IO1)
             IST = IO1 + 1
             IEN = IACON1(IA+1)-1
             IF (IA.EQ.NAT) IEN=NORB
             DO 7025 KKJ=IA-NCOR+1,NA+1
                DO 7020 JJ=IST,IEN
                IS2 = IOX(JJ)
                IP1 = IMUL(IS2,IS1)
C
             IAC = IAC + 1
             CALL RET1DET(IACON1,IACON2,NA,IA,JJ,NCOR,KKJ,IPER1)
C
C   Storage here for later use, well worth it
C
             IPET = POSDET(NACT,NA,IACON2,IFA)
             IPOSA(IAC) = ISPA(IPET)
             IMMC(ISYMA(IPET)) = IMMC(ISYMA(IPET)) + 1
             IMMA(ISYMA(IPET),IMMC(ISYMA(IPET))) = IAC
             IPERA(IAC) = ((-1)**IPER1)*ICONST
             IND = INDEX(JJ,IO1)
             IIND1(IAC) = IND
Cc If deoccupied and newly occupied orbitals are of diff symm,
C  skip to doubles
             IF (IS1.NE.IS2) GOTO 417
C
         DO 49 I=1,NBT
            IBCON1(I) = I
   49    CONTINUE
C
C
C                  do 407 inb1 = 1,nblp
C    Loop over beta dets of the right symmetry
                   DO 407 INBB = ISBS(ISA1),ISBS(ISA1+1)-1
                   INB1 = ISBC(INBB)
                   ICIT = ICAT+ISPB(INB1)
                   ICI2 = IPOSA(IAC) + ISPB(INB1)
CState average here
           FC = 0.0D+00
           DO 409 KKI=1,NSTATE
            FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICI2,IWH(KKI))
  409      CONTINUE
C
           FC = FC*IPERA(IAC)
C
C
                   DEN(IND) = DEN(IND) + FC
  407          CONTINUE
C
                   DO 487 IK=1,NAT
                      IF (IK.EQ.IA) GOTO 487
                      ION = IACON1(IK)
                      J1 = INDEX(ION,ION)
                      JJ1 = INDEX(IND,J1)
                      J1 = INDEX(ION,JJ)
                      J2 = INDEX(ION,IO1)
                      INX = INDEX(J1,J2)
                   DO 413 INBB=ISBS(ISA1),ISBS(ISA1+1)-1
                      INB1 = ISBC(INBB)
                      ICIT = ICAT+ISPB(INB1)
                      ICI2 = IPOSA(IAC)+ISPB(INB1)
CState average here
               FC = 0.0D+00
               DO 419 KKI=1,NSTATE
           FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICI2,IWH(KKI))
  419         CONTINUE
            FC = FC*IPERA(IAC)
C
                      DEN2(JJ1) = DEN2(JJ1) + FC
                      DEN2(INX) = DEN2(INX) -  FC
  413              CONTINUE
C
  487              CONTINUE
C
C                  do 415 inb1=1,nblp
                  NST = 1
                  DO 415 INBB = ISBS(ISA1),ISBS(ISA1+1)-1
                  NEND = ISBC(INBB)
                  DO 5510 KK=NST,NEND-1
                     CALL ADVANC(IBCON1,NBT,NORB)
 5510             CONTINUE
                  ICIT = ICAT+ISPB(NEND)
                  ICI2 = IPOSA(IAC)+ISPB(NEND)
C State average here
             FC = 0.0D+00
             DO 719 KKI=1,NSTATE
              FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICI2,IWH(KKI))
  719         CONTINUE
              FC = FC*IPERA(IAC)
C
                     DO 790 IK=1,NBT
                      ION = IBCON1(IK)
                      J1 = INDEX(ION,ION)
                      JJ1 = INDEX(IND,J1)
                      DEN2(JJ1) = DEN2(JJ1) + FC
  790              CONTINUE
C
Cd           call advanc(ibcon1,nbt,norb)
          NST = NEND
  415     CONTINUE
C
  417     CONTINUE
C
C      Double excitations
C
          DO 4015 IAA = IA+1,NAT
             IPA = IAA-NCOR
             IIA = IACON1(IAA)
             IS3 = IOX(IIA)
             IF (JJ.GT.IIA) IPA = IPA - 1
             ISTAA = JJ+1
             IENAA = IEN
             DO 4010 KKJAA=KKJ,NA+1
                DO 4005 JJAA=ISTAA,IENAA
                IS4 = IOX(JJAA)
                IP2 = IMUL(IS4,IS3)
                IF (IP1.NE.IP2) GOTO 4005
C
             CALL RET1DET(IACON2,IBCON1,NA,IPA,JJAA-NCOR,0,KKJAA,IPER2)
             IPET = POSDET(NACT,NA,IBCON1,IFA)
             ICA1 = ISPA(IPET)
             IPERT = IPER1+IPER2
             IPERT = ((-1)**IPERT)*ICONST
                   I2 = INDEX(IACON1(IAA),JJAA)
                   INX = INDEX(I2,IND)
                   II1 = INDEX(JJAA,IO1)
                   II2 = INDEX(IACON1(IAA),JJ)
                   INX2 = INDEX(II1,II2)
C
                DO 786 INB1 = ISBS(ISA1),ISBS(ISA1+1)-1
                   NEND = ISBC(INB1)
                   ICI2 = ICA1 + ISPB(NEND)
                   ICIT = ICAT+ISPB(NEND)
CState average here
              FC = 0.0D+00
              DO 819 KKI=1,NSTATE
              FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICI2,IWH(KKI))
  819         CONTINUE
              FC = FC*IPERT
C
                   DEN2(INX) = DEN2(INX) + FC
                   DEN2(INX2) = DEN2(INX2) - FC
  786       CONTINUE
C
 4005           CONTINUE
                ISTAA = IACON1(KKJAA+NCOR)+1
                IENAA = IACON1(NCOR+KKJAA+1)-1
                IF (KKJAA.EQ.NA) IENAA=NORB
 4010        CONTINUE
 4015 CONTINUE
C
C
 7020           CONTINUE
                IST = IACON1(KKJ+NCOR)+1
                IEN = IACON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NA) IEN=NORB
 7025        CONTINUE
 7030     CONTINUE
C
C  Diagonal
C
            DO 67 II=1,NAT
               I1 = IACON1(II)
               IND1 = INDEX(I1,I1)
C
              DO 53 INB1 = ISBS(ISA1),ISBS(ISA1+1)-1
              NEND = ISBC(INB1)
              ICIT = ICAT+ISPB(NEND)
C State average here
              FC = 0.0D+00
              DO 919 KKI=1,NSTATE
              FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICIT,IWH(KKI))
  919         CONTINUE
C
               DEN(IND1) = DEN(IND1) + FC
   53         CONTINUE
C
               DO 64 JJ=II+1,NAT
                  I2 = IACON1(JJ)
                  IND2 = INDEX(I2,I2)
                  INDM = IND2 - I2 + I1
                  J1 = INDEX(INDM,INDM)
                  J2 = INDEX(IND2,IND1)
               DO 54 INB1 = ISBS(ISA1),ISBS(ISA1+1)-1
                  NEND = ISBC(INB1)
                  ICIT = ICAT+ISPB(NEND)
CState average here
               FC = 0.0D+00
               DO 619 KKI=1,NSTATE
               FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICIT,IWH(KKI))
  619          CONTINUE
C
                  DEN2(J1) = DEN2(J1) - FC
                  DEN2(J2) = DEN2(J2) + FC
   54          CONTINUE
C
   64          CONTINUE
C
         DO 47 I=1,NBT
            IBCON1(I) = I
   47    CONTINUE
C
             NST = 1
             DO 56 INB1 = ISBS(ISA1),ISBS(ISA1+1)-1
             NEND = ISBC(INB1)
             DO 7710 KK=NST,NEND-1
                CALL ADVANC(IBCON1,NBT,NORB)
 7710        CONTINUE
             ICIT = ICAT+ISPB(NEND)
CState average here
            FC = 0.0D+00
            DO 519 KKI=1,NSTATE
            FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICIT,IWH(KKI))
  519       CONTINUE
C
               DO 68 JJ=1,NBT
                  I2 = IBCON1(JJ)
                  IND2 = INDEX(I2,I2)
                  J2 = INDEX(IND1,IND2)
                  DEN2(J2) = DEN2(J2) + FC
   68          CONTINUE
C
Cc           call advanc(ibcon1,nbt,norb)
            NST = NEND
   56       CONTINUE
C
   67       CONTINUE
C
C   Loop over Beta dets now
C
         IF (NA.NE.NB) THEN
         DO 40 II=1,NBT
            IBCON1(II) = II
   40    CONTINUE
         JJZ = 1
         ELSE
         DO 41 II=1,NBT
            IBCON1(II) = IACON1(II)
   41    CONTINUE
         JJZ = IJK
         ENDIF
C
         DO 8000 KJI = JJZ,NBLP
         ISTAR = ISTART(KJI)-1
         IPB1 = ISPB(KJI)
         ISB1 = ISYMB(KJI)
         ITBS = ITAB(ISB1)
         IMZZ = IMMC(ITBS)
         M1 = 0
         M2 = 0
         IF (ISB1.EQ.ITAS) M1 = 1
         IF (IMZZ.NE.0) M2 = 1
         IF (M1.EQ.0.AND.M2.EQ.0) GOTO 7998
         QNUM = 1.0D+00
         IF (NA.EQ.NB.AND.IJK.EQ.KJI) QNUM=2.0D+00
         IC1 = ICAT + IPB1
C
C   Beta first *********************** Single
C
          DO 900 IB=NCOR+1,NBT
             IBB = IBCON1(IB)
             IB1 = IOX(IBB)
             IR1 = IMUL(IB1,ISB1)
             IST = IBB+1
             IEN = IBCON1(IB+1)-1
             IF (IB.EQ.NBT) IEN = NORB
             DO 895 KKJ=IB-NCOR+1,NB+1
                DO 890 JJ=IST,IEN
                IB2 = IOX(JJ)
                ISB2 = IMUL(IR1,IB2)
                ITB2 = ITAB(ISB2)
                ISTAR = ISTAR + 1
                IF (ISB2.NE.ITAS.AND.M1.EQ.0) GOTO 890
                IF (M2.EQ.0.AND.IMMC(ITB2).EQ.0) GOTO 890
                IF (ISB2.NE.ITAS.AND.IMMC(ITB2).EQ.0) GOTO 890
C
C               call ret1det(ibcon1,iacon2,nb,ib,jj,ncor,kkj,iper1)
C                   iposb = posdet(nact,nb,iacon2,ifa)
C                  ipb2 = ispb(iposb)
                   IPB2 = ISTRB(ISTAR)
                   IC2 = ICAT + IPB2
C           iperb = ((-1)**iper1)
C   iperb1 = ((-1)**iper1)
                   ZPERB = ISTRP(ISTAR)/QNUM
                   IOB = INDEX(IBB,JJ)
C
C           do 1013 iat=1,iac
C                  ici2 = iposa(iat) + iposb
C                  ici3 = iposa(iat) + kji
CcState average here
C               fc = 0.0d+00
C               fc1 = 0.0d+00
C             do 319 kki=1,nstate
C             fc = fc + W(kki)*CI(ici1,iwh(kki))*CI(ici2,iwh(kki))
C             fc1 = fc1+W(kki)*CI(ici3,iwh(kki))*CI(icit,iwh(kki))
C  319          continue
C             fc = fc * ipera(iat)*zperb
C             fc1=fc1 * ipera(iat)*zperb1
C
C                   ind = iind1(iat)
C                    ix = index(ind,iob)
C                   den2(ix) = den2(ix)+fc+fc1
C                   den2(ix) = den2(ix) + fc
C 1013      continue
C
            IF (M2.EQ.0.AND.IMMC(ITB2).NE.0) THEN
               DO 1013 IAT = 1,IMMC(ITB2)
                  IJU = IMMA(ITB2,IAT)
                  IC4 = IPOSA(IJU) + IPB2
                  IND = IIND1(IJU)
                  IX = INDEX(IND,IOB)
                  FC = 0.0D+00
                  DO 319 KKI=1,NSTATE
                  FC = FC + W(KKI)*CI(IC1,IWH(KKI))*CI(IC4,IWH(KKI))
  319             CONTINUE
                  FC = FC * IPERA(IJU)*ZPERB
                  DEN2(IX) = DEN2(IX) + FC
 1013          CONTINUE
               GOTO 890
          ENDIF
C
            IF (M1.EQ.0.AND.ISB2.EQ.ITAS) THEN
               DO 2013 IAT =1,IMMC(ITBS)
                  IJU = IMMA(ITBS,IAT)
                  IC3 = IPOSA(IJU) + IPB1
                  IND = IIND1(IJU)
                  IX = INDEX(IND,IOB)
                  FC = 0.0D+00
                  DO 321 KKI=1,NSTATE
                  FC = FC + W(KKI)*CI(IC2,IWH(KKI))*CI(IC3,IWH(KKI))
  321             CONTINUE
                  FC = FC * IPERA(IJU)*ZPERB
                  DEN2(IX) = DEN2(IX) + FC
 2013          CONTINUE
               GOTO 890
            ENDIF
C
            IF (ISB2.NE.ITAS.AND.IMMC(ITB2).NE.0) THEN
               DO 3013 IAT = 1,IMMC(ITB2)
                  IJU = IMMA(ITB2,IAT)
                  IC4 = IPOSA(IJU) + IPB2
                  IND = IIND1(IJU)
                  IX = INDEX(IND,IOB)
                  FC = 0.0D+00
                  DO 323 KKI=1,NSTATE
                  FC = FC + W(KKI)*CI(IC1,IWH(KKI))*CI(IC4,IWH(KKI))
  323             CONTINUE
                  FC = FC * IPERA(IJU)*ZPERB
                  DEN2(IX) = DEN2(IX) + FC
 3013          CONTINUE
               GOTO 890
            ENDIF
C
            IF (IMMC(ITB2).EQ.0.AND.ISB2.EQ.ITAS) THEN
               DO 4013 IAT =1,IMMC(ITBS)
                  IJU = IMMA(ITBS,IAT)
                  IC3 = IPOSA(IJU) + IPB1
                  IND = IIND1(IJU)
                  IX = INDEX(IND,IOB)
                  FC = 0.0D+00
                  DO 325 KKI=1,NSTATE
                  FC = FC + W(KKI)*CI(IC2,IWH(KKI))*CI(IC3,IWH(KKI))
  325             CONTINUE
                  FC = FC * IPERA(IJU)*ZPERB
                  DEN2(IX) = DEN2(IX) + FC
 4013          CONTINUE
               GOTO 890
            ENDIF
C
            IF (IMMC(ITB2).NE.0.AND.ISB2.EQ.ITAS) THEN
               DO 5013 IAT=1,IMMC(ITB2)
                  IJU = IMMA(ITB2,IAT)
                  IC3 = IPOSA(IJU) + IPB1
                  IC4 = IPOSA(IJU) + IPB2
                  IND = IIND1(IJU)
                  IX = INDEX(IND,IOB)
                  FC = 0.0D+00
                  FC1 = 0.0D+00
                  DO 327 KKI=1,NSTATE
                  FC = FC + W(KKI)*CI(IC1,IWH(KKI))*CI(IC4,IWH(KKI))
                  FC1 = FC1 + W(KKI)*CI(IC2,IWH(KKI))*CI(IC3,IWH(KKI))
  327             CONTINUE
                  FC = FC * IPERA(IJU)*ZPERB
                  FC1 = FC1 * IPERA(IJU)*ZPERB
                  DEN2(IX) = DEN2(IX) + FC + FC1
 5013          CONTINUE
               GOTO 890
            ENDIF
C
  890           CONTINUE
                IST = IBCON1(KKJ+NCOR)+1
                IEN = IBCON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NB) IEN=NORB
  895        CONTINUE
  900     CONTINUE
C
 7998     CONTINUE
          CALL ADVANC(IBCON1,NBT,NORB)
 8000    CONTINUE
         CALL ADVANC(IACON1,NAT,NORB)
 9000 CONTINUE
C
C   Now for the Beta part
C
C
      DO 876 JJI=1,NBT
         IBCON1(JJI) = JJI
  876 CONTINUE
C
      DO 9999 IJK = 1,NBLP
         ICAB = ISPB(IJK)
         ISB1 = ISYMB(IJK)
C itb1 = itab(isb1)
         IF (NA.EQ.NB) GOTO 7999
C
      DO 6030 IB=NCOR+1,NBT
         IST = IBCON1(IB)+1
         IEN = IBCON1(IB+1)-1
         IF (IB.EQ.NBT) IEN=NORB
         IO1 = IBCON1(IB)
         IS1 = IOX(IO1)
         DO 6025 KKJ=IB-NCOR+1,NB+1
            DO 6020 JJ=IST,IEN
            IS2 = IOX(JJ)
            IP1 = IMUL(IS2,IS1)
C
            CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
            IPER = ((-1)**IPER1)*2
            IND = INDEX(JJ,IO1)
Cc If deoccupied and newly occupied are of diff sym,
C   then skip to doubles
            IF (IS1.NE.IS2) GOTO 517
            IPB1 = POSDET(NACT,NB,IACON2,IFA)
            IPB1 = ISPB(IPB1)
C
       DO 89 I=1,NAT
          IACON1(I) = I
   89 CONTINUE
C
C  Loop over alpha
C
          DO 907 INA1 = ISAS(ISB1),ISAS(ISB1+1)-1
             ICIA = ISAC(INA1)
             ICIT = ISPA(ICIA) + ICAB
             ICI2 = ISPA(ICIA) + IPB1
CState average
            FC = 0.0D+00
            DO 219 KKI=1,NSTATE
            FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICI2,IWH(KKI))
  219       CONTINUE
             FC = FC*IPER
C
             DEN(IND) = DEN(IND) + FC
  907     CONTINUE
C
             DO 687 IK=1,NBT
                IF (IK.EQ.IB) GOTO 687
                ION = IBCON1(IK)
                J1 = INDEX(ION,ION)
                JJ1 = INDEX(IND,J1)
                J1 = INDEX(ION,JJ)
                J2 = INDEX(ION,IO1)
                INX = INDEX(J1,J2)
           DO 918 INA1 = ISAS(ISB1),ISAS(ISB1+1)-1
               ICIA = ISAC(INA1)
               ICIT = ISPA(ICIA) + ICAB
               ICI2 = ISPA(ICIA) + IPB1
CState average
           FC = 0.0D+00
           DO 119 KKI=1,NSTATE
              FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICI2,IWH(KKI))
  119      CONTINUE
             FC = FC*IPER
C
                DEN2(JJ1) = DEN2(JJ1) + FC
                DEN2(INX) = DEN2(INX) - FC
  918      CONTINUE
C
  687        CONTINUE
C
            NST = 1
            DO 920 INA1 = ISAS(ISB1),ISAS(ISB1+1)-1
            NEND = ISAC(INA1)
            DO 8810 KK=NST,NEND-1
               CALL ADVANC(IACON1,NAT,NORB)
 8810       CONTINUE
            ICIA = ISPA(NEND)
            ICIT = ICIA + ICAB
            ICI2 = ICIA  + IPB1
CState average
           FC = 0.0D+00
           DO 1019 KKI=1,NSTATE
           FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICI2,IWH(KKI))
 1019      CONTINUE
             FC = FC*IPER
C
             DO 690 IK=1,NAT
                ION = IACON1(IK)
                J1 = INDEX(ION,ION)
                JJ1 = INDEX(IND,J1)
                DEN2(JJ1) = DEN2(JJ1) + FC
  690        CONTINUE
Cdo           call advanc(iacon1,nat,norb)
             NST = NEND
  920    CONTINUE
C
  517   CONTINUE
C
C  Now for beta doubles
C
       DO 6015 IBB = IB+1,NBT
               ISTBB = JJ+1
               IENBB = IEN
               JB = IBCON1(IBB)
               IS3 = IOX(JB)
               IPB = IBB-NCOR
               IF (JJ.GT.JB) IPB = IPB - 1
               DO 6010 KKJBB = KKJ,NB+1
                  DO 6005 JJBB = ISTBB,IENBB
                    IS4 = IOX(JJBB)
                    IP2 = IMUL(IS4,IS3)
                    IF (IP1.NE.IP2) GOTO 6005
C
          CALL RET1DET(IACON2,IACON1,NB,IPB,JJBB-NCOR,0,KKJBB,IPER2)
          IBP2 = POSDET(NACT,NB,IACON1,IFA)
          IBP2 = ISPB(IBP2)
          IPER = IPER1+IPER2
          IPER = ((-1)**IPER)*2
               I2 = INDEX(JB,JJBB)
               INX = INDEX(I2,IND)
               II1 = INDEX(JJBB,IO1)
               II2 = INDEX(JB,JJ)
               INX2 = INDEX(II1,II2)
C
             DO 686 INA1 = ISAS(ISB1),ISAS(ISB1+1)-1
             NEND = ISAC(INA1)
             ICIA = ISPA(NEND)
             ICIT = ICIA + ICAB
             ICI2 = ICIA + IBP2
CState average
          FC = 0.0D+00
          DO 1119 KKI=1,NSTATE
          FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICI2,IWH(KKI))
 1119     CONTINUE
             FC =FC*IPER
C
             DEN2(INX) = DEN2(INX) + FC
             DEN2(INX2) = DEN2(INX2) - FC
  686       CONTINUE
C
 6005          CONTINUE
               ISTBB = IBCON1(KKJBB+NCOR)+1
               IENBB = IBCON1(NCOR+KKJBB+1)-1
               IF (KKJBB.EQ.NB) IENBB=NORB
 6010      CONTINUE
 6015 CONTINUE
C
 6020       CONTINUE
            IST = IBCON1(KKJ+NCOR)+1
            IEN=IBCON1(NCOR+KKJ+1)-1
            IF (KKJ.EQ.NB) IEN=NORB
 6025     CONTINUE
 6030 CONTINUE
C
C    Remaining part of diagonal contributions
C
 7999 CONTINUE
            DO 69 II=1,NBT
               I1 = IBCON1(II)
               IND1 = INDEX(I1,I1)
            DO 93 INA1 = ISAS(ISB1),ISAS(ISB1+1)-1
              NEND = ISAC(INA1)
              ICIA = ISPA(NEND)
              ICIT = ICIA + ICAB
CState average
          FC = 0.0D+00
          DO 1219 KKI=1,NSTATE
          FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICIT,IWH(KKI))
 1219     CONTINUE
C
              DEN(IND1) = DEN(IND1) + FC
   93       CONTINUE
C
               DO 74 JJ=II+1,NBT
                  I2 = IBCON1(JJ)
                  IND2 = INDEX(I2,I2)
                  INDM = IND2-I2+I1
                  J1 = INDEX(INDM,INDM)
                  J2 = INDEX(IND2,IND1)
            DO 97 INA1 = ISAS(ISB1),ISAS(ISB1+1)-1
               NEND = ISAC(INA1)
               ICIA = ISPA(NEND)
               ICIT = ICIA + ICAB
CState average
            FC = 0.0D+00
            DO 1319 KKI=1,NSTATE
            FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICIT,IWH(KKI))
 1319       CONTINUE
C
                 DEN2(J1) = DEN2(J1) - FC
                 DEN2(J2) = DEN2(J2) + FC
   97       CONTINUE
C
   74          CONTINUE
C
   69       CONTINUE
           CALL ADVANC(IBCON1,NBT,NORB)
 9999 CONTINUE
C
      RETURN
      END
C
C*MODULE ALDECI  *DECK ECORR
C     -----------------------------------------------------------
      SUBROUTINE ECORR(IW,NFT12,
     *                 EL,ECONST,EHF,CI,NCI,K,SI1,SI2,NACT,NA,NB,
     *                 IFA,INDEX,IACON1,IACON2,IBCON1,IBCON2,
     *                 ISYMA,IMUL,ISPA,ISPB,
     *                 ISBS,ISBC,NSYM,IOX,NALP,NBLP)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXRT=100)
      LOGICAL MASWRK,DSKWRK,GOPARR
      COMMON /ENRGYS/ ENUCR,EELCT,ETOT,STOT,SSQUAR,ECORE,ESCF,EERD,
     *                E1,E2,VEN,VEE,EPOT,EKIN,ESTATE(MXRT),STATN
      COMMON /FMCOM / X(1)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      DIMENSION EL(K),CI(NCI,K),SI1(*),SI2(*)
      DIMENSION IFA(0:NACT,0:NACT)
      DIMENSION INDEX((NACT*(NACT+1))/2,(NACT*(NACT+1))/2)
      DIMENSION IACON1(NA),IACON2(NA)
      DIMENSION IBCON1(NA),IBCON2(NB)
      DIMENSION ISYMA(NALP)
      DIMENSION ISPA(NALP),ISPB(NBLP)
      DIMENSION IMUL(NSYM,NSYM)
      DIMENSION ISBS(NSYM+1),ISBC(NBLP)
      DIMENSION IOX(NACT)
      INTEGER POSDET
C
      CHARACTER*102 CONA,CONB
C
C     SUBROUTINE TO ANALYSE CORRELATION ENERGY BASED UPON
C     EQUATION (4.10) IN 'MODERN QUANTUM CHEMISTRY' BY
C     SZABO AND OSTLUND.
C
C     CORRELATION ENERGY IS WRITTEN IN TERMS OF ORBITAL PAIR
C     CONTRIBUTIONS.
C     METHOD BY J. IVANIC, K. RUEDENBERG.
C     REFERENCE IS FORTHCOMING.
C
C     the dimension check is against the length of two character values
C
      IF(NA.GT.100) THEN
         IF(MASWRK) WRITE(IW,*)
     *      'ECOR: TOO MANY ACTIVE ORBITALS TO ANALYZE CI ENERGIES'
         RETURN
      ENDIF
C
C  FIRST PRINT OUT THE CI VECTORS.
C
      DO 10 I=1,K
         ESTATE(I) = EL(I)+ECONST
   10 CONTINUE
      EHFT = EHF + ECONST
C
      CALL SEQOPN(NFT12,'CIVECTR','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQREW(NFT12)
      IF(MASWRK) WRITE(NFT12) K,NCI
      DO 20 IST=1,K
         CALL STFASE(CI(1,IST),NCI,NCI,1)
         CALL SQWRIT(NFT12,CI(1,IST),NCI)
   20 CONTINUE
      CALL SEQREW(NFT12)
      CALL DETPRT(IW,NFT12,.TRUE.)
C
C  NOW TO PERFORM THE ANALYSIS
C
C  CALL UP MEMORY FOR ALL MATRICES THAT WILL BE EVALUATED.
C
      CALL VALFM(LOADFM)
      LE1A  = LOADFM + 1
      LE1B  = LE1A + NACT*NA
      LE12A = LE1B + NACT*NB
      LE12B = LE12A + NACT*NA
      LE2A  = LE12B + NACT*NB
      LE2B  = LE2A  + (NA*(NA+1))/2
      LE2AB = LE2B  + (NB*(NB+1))/2
      LAST  = LE2AB + (NB*NA)
      NEED = LAST - LOADFM - 1
      CALL GETFM(NEED)
      DO II=LE1A,LAST-1
         X(II) = 0.0D0
      ENDDO
C
      IF(MASWRK) THEN
         WRITE(IW,7000)
         WRITE(IW,7020) ESTATE(1),EHFT,ESTATE(1)-EHFT
      END IF
C
C  MAKE THE 1ST DETERMINANT IN THE LIST.
C
      DO II=1,NA
         IACON1(II)=II
      ENDDO
      DO II=1,NB
         IBCON1(II)=II
      ENDDO
C
      DO 30 IJK = 1, NALP
         ICAT = ISPA(IJK)
         IF (ICAT.GT.0) GOTO 111
         ISA = ISYMA(IJK)
         NST = 1
         DO 40 INB1 = ISBS(ISA),ISBS(ISA+1)-1
            NEND = ISBC(INB1)
            DO 50 KK=NST,NEND-1
               CALL ADVANC(IBCON1,NB,NACT)
   50       CONTINUE
             ICIT = ICAT + ISPB(NEND)
             IF (ICIT.EQ.1) GOTO 222
             IF (ICIT.GT.1) GOTO 111
   40    CONTINUE
         CALL ADVANC(IACON1,NA,NACT)
   30 CONTINUE
C
C   IF THERE IS AN ERROR ABOVE WE END UP HERE!!!!!!
  111 CONTINUE
      IF(MASWRK) WRITE(IW,7040)
      RETURN
C
C   IF FOUND 1ST DETERMINANT WE END UP HERE, PRINT IT.
C
  222 CONTINUE
      CONA(1:1) = ' '
      CONB(1:1) = ' '
      DO II=2,NA+1
         CONA(II:II) = '0'
         CONB(II:II) = '0'
      ENDDO
      CONA(NA+2:NA+2) = ' '
      CONB(NA+2:NA+2) = ' '
C
      DO II=1,NA
         CONA(IACON1(II)+1:IACON1(II)+1) = '1'
      ENDDO
      DO II=1,NB
         CONB(IBCON1(II)+1:IBCON1(II)+1) = '1'
      ENDDO
C
      CONA(NACT+2:NACT+2) = ' '
      CONB(NACT+2:NACT+2) = ' '
      IF(MASWRK) THEN
         WRITE(IW,7025)
         WRITE(IW,'(4A,F10.7)') CONA(1:NACT+2),'|',CONB(1:NACT+2),
     *                          '|  ',CI(1,1)
      END IF
      DO II=1,NA
         IF (IACON1(II).NE.II) THEN
            IF(MASWRK) WRITE(IW,7050)
            GOTO 444
         ENDIF
      ENDDO
  444 CONTINUE
      DO II=1,NB
         IF (IBCON1(II).NE.II) THEN
            IF(MASWRK) WRITE(IW,7060)
            GOTO 555
         ENDIF
      ENDDO
C
C  NOW FOR ANALYSIS
C
  555 CONTINUE
C
      E1COA1 = 0.0D0
      E1COB1 = 0.0D0
      E1COA2 = 0.0D0
      E1COB2 = 0.0D0
      E2COA  = 0.0D0
      E2COB  = 0.0D0
      E2COAB = 0.0D0
      E2COD  = 0.0D0
      CI0 = CI(1,1)
C
C  ****    HERE IS WHERE WE LOOP OVER NON-ZERO    ****
C  **** MATRIX ELEMENTS INVOLVING 1ST DETERMINANT ****
C
C SINGLE ALPHA EXCITES IOC -> IVI
C
      DO 200 IOC=1,NA
         IS1 = IOX(IOC)
         DO 190 IVI=NA+1,NACT
            IS2 = IOX(IVI)
            IP1 = IMUL(IS2,IS1)
            CALL RET1DET(IACON1,IACON2,NA,IOC,IVI,0,NA+1,IPER1)
            IPET = POSDET(NACT,NA,IACON2,IFA)
            IPOSA = ISPA(IPET)
            ICI2 = IPOSA+1
            IPERA = ((-1)**IPER1)
            IND = INDEX(IOC,IVI)
            RC = IPERA*CI(ICI2,1)
C
            IF (IS1.NE.IS2) GOTO 180
C
C  WORK OUT PARTS OF H(0,S) AND USE.
C
            C = SI1(IND)*RC
            INZ = LE1A-1+(IOC-1)*NACT + IVI
            X(INZ) =  C
C
            D = 0.0D0
            DO 170 IK=1,NA
               IF (IK.EQ.IOC) GOTO 170
               J1 = INDEX(IK,IK)
               JJ1 = INDEX(IND,J1)
               J1 = INDEX(IK,IVI)
               J2 = INDEX(IK,IOC)
               INX = INDEX(J1,J2)
               D = D + SI2(JJ1) - SI2(INX)
  170       CONTINUE
C
            DO 165 IK=1,NB
               J1 = INDEX(IK,IK)
               JJ1 = INDEX(IND,J1)
               D = D + SI2(JJ1)
  165       CONTINUE
C
            D = D*RC
            INZ = LE12A-1+(IOC-1)*NACT + IVI
            X(INZ) =  D
C
  180       CONTINUE
C
C DOUBLE ALPHA EXCITES IOC,IOC2 -> IVI,IVI2
C
            DO 160 IOC2=IOC+1,NA
               IS3 = IOX(IOC2)
               DO 150 IVI2=IVI+1,NACT
                  IS4 = IOX(IVI2)
                  IP2 = IMUL(IS3,IS4)
C
                  IF (IP1.NE.IP2) GOTO 150
C
             CALL RET1DET(IACON2,IBCON1,NA,IOC2-1,IVI2,0,NA+1,IPER2)
             IPET = POSDET(NACT,NA,IBCON1,IFA)
             IPOSA2 = ISPA(IPET)
             ICI2 = IPOSA2 + 1
             IPERT = IPER1+IPER2
             IPERT = ((-1)**IPERT)
C
                  I2 = INDEX(IOC2,IVI2)
                  INX = INDEX(I2,IND)
                  II1 = INDEX(IVI2,IOC)
                  II2 = INDEX(IOC2,IVI)
                  INX2 = INDEX(II1,II2)
                  C = SI2(INX) - SI2(INX2)
C
                  INZ = LE2A-1+INDEX(IOC,IOC2)
                  X(INZ) = X(INZ) + C*IPERT*CI(ICI2,1)
C
  150          CONTINUE
  160       CONTINUE
C
C  SIMULTANEOUS ALPHA + BETA EXCITATIONS
C
            DO II=1,NB
               IBCON1(II)=II
            ENDDO
C
            DO 140 IOC2=1,NB
               IS3 = IOX(IOC2)
               DO 130 IVI2 = NB+1,NACT
                  IS4 = IOX(IVI2)
                  IP2 = IMUL(IS3,IS4)
C
                  IF (IP1.NE.IP2) GOTO 130
C
            CALL RET1DET(IBCON1,IBCON2,NB,IOC2,IVI2,0,NB+1,IPER2)
            IPET = POSDET(NACT,NB,IBCON2,IFA)
            IPOSB = ISPB(IPET)
            IPERT = (-1)**(IPER1+IPER2)
            ICI2 = IPOSA + IPOSB
C
                  I2 = INDEX(IOC2,IVI2)
                  INX = INDEX(I2,IND)
                  C = SI2(INX)
C
                  INZ = LE2AB-1+(IOC-1)*NB + IOC2
                  X(INZ) = X(INZ) + C*IPERT*CI(ICI2,1)
C
  130          CONTINUE
  140       CONTINUE
C
  190    CONTINUE
  200 CONTINUE
C
C  NOW FOR EXCITED BETA LOOP
C
      DO II=1,NB
         IBCON1(II)=II
      ENDDO
C
C SINGLE BETA EXCITES IOC -> IVI
C
      DO 500 IOC=1,NB
         IS1 = IOX(IOC)
         DO 490 IVI=NB+1,NACT
            IS2 = IOX(IVI)
            IP1 = IMUL(IS2,IS1)
            CALL RET1DET(IBCON1,IBCON2,NB,IOC,IVI,0,NB+1,IPER1)
            IPET = POSDET(NACT,NB,IBCON2,IFA)
            IPOSB = ISPB(IPET)
            IPERB = ((-1)**IPER1)
            IND = INDEX(IOC,IVI)
            RC = IPERB*CI(IPOSB,1)
C
            IF (IS1.NE.IS2) GOTO 480
C
C  WORK OUT H(0,S)*C(S)
C
            C = SI1(IND)*RC
            INZ = LE1B-1+(IOC-1)*NACT + IVI
            X(INZ) =  C
C
            D = 0.0D0
            DO 470 IK=1,NB
               IF (IK.EQ.IOC) GOTO 470
               J1 = INDEX(IK,IK)
               JJ1 = INDEX(IND,J1)
               J1 = INDEX(IK,IVI)
               J2 = INDEX(IK,IOC)
               INX = INDEX(J1,J2)
               D = D + SI2(JJ1) - SI2(INX)
  470       CONTINUE
C
            DO 465 IK=1,NA
               J1 = INDEX(IK,IK)
               JJ1 = INDEX(IND,J1)
               D = D + SI2(JJ1)
  465       CONTINUE
C
            D = D*RC
            INZ = LE12B-1+(IOC-1)*NACT + IVI
            X(INZ) =  D
C
  480       CONTINUE
C
C DOUBLE BETA EXCITES IOC,IOC2 -> IVI,IVI2
C
            DO 460 IOC2=IOC+1,NB
               IS3 = IOX(IOC2)
               DO 450 IVI2=IVI+1,NACT
                  IS4 = IOX(IVI2)
                  IP2 = IMUL(IS3,IS4)
C
                  IF (IP1.NE.IP2) GOTO 450
C
             CALL RET1DET(IBCON2,IACON1,NB,IOC2-1,IVI2,0,NB+1,IPER2)
             IPET = POSDET(NACT,NB,IACON1,IFA)
             IPOSB2 = ISPB(IPET)
             IPERT = IPER1+IPER2
             IPERT = ((-1)**IPERT)
C
                  I2 = INDEX(IOC2,IVI2)
                  INX = INDEX(I2,IND)
                  II1 = INDEX(IVI2,IOC)
                  II2 = INDEX(IOC2,IVI)
                  INX2 = INDEX(II1,II2)
                  C = SI2(INX) - SI2(INX2)
C
                  INZ = LE2B-1+INDEX(IOC,IOC2)
                  X(INZ) = X(INZ) + C*IPERT*CI(IPOSB2,1)
C
  450          CONTINUE
  460       CONTINUE
C
  490     CONTINUE
  500 CONTINUE
C
      DO II=LE1A,LE1B-1
         X(II) = X(II)/CI0
         E1COA1 = E1COA1 + X(II)
      ENDDO
      DO II=LE1B,LE12A-1
         X(II) = X(II)/CI0
         E1COB1 = E1COB1 + X(II)
      ENDDO
      DO II=LE12A,LE12B-1
         X(II) = X(II)/CI0
         E1COA2 = E1COA2 + X(II)
      ENDDO
      DO II=LE12B,LE12B+NACT*NB-1
         X(II) = X(II)/CI0
         E1COB2 = E1COB2 + X(II)
      ENDDO
C
C SINGLE EXCITATION ANALYSIS
C
      E1COAT = E1COA1 + E1COA2
      E1COBT = E1COB1 + E1COB2
      E1COT   = E1COAT + E1COBT
      IF(MASWRK) THEN
         WRITE(IW,7030)
         WRITE(IW,7065)
         WRITE(IW,7070) E1COA1,E1COA2,E1COAT
         WRITE(IW,7080) E1COB1,E1COB2,E1COBT
         WRITE(IW,7090) E1COT
         WRITE(IW,7100)
         CALL PRSQ(X(LE1A),NA,NACT,NACT)
         WRITE(IW,7110)
         CALL PRSQ(X(LE12A),NA,NACT,NACT)
      END IF
      DO II=1,NA*NACT
         X(II+LE1A-1) = X(II+LE12A-1) + X(II+LE1A-1)
      ENDDO
      IF(MASWRK) THEN
         WRITE(IW,7140)
         CALL PRSQ(X(LE1A),NA,NACT,NACT)
      END IF
C
      IF(MASWRK) THEN
         WRITE(IW,7120)
         CALL PRSQ(X(LE1B),NB,NACT,NACT)
         WRITE(IW,7130)
         CALL PRSQ(X(LE12B),NB,NACT,NACT)
      END IF
      DO II=1,NB*NACT
         X(II+LE1B-1) = X(II+LE12B-1) + X(II+LE1B-1)
      ENDDO
      IF(MASWRK) THEN
         WRITE(IW,7150)
         CALL PRSQ(X(LE1B),NB,NACT,NACT)
      END IF
C
      DO II=1,NB*NACT
         X(II+LE1A-1) = X(II+LE1B-1) + X(II+LE1A-1)
      ENDDO
      IF(MASWRK) THEN
         WRITE(IW,7160)
         CALL PRSQ(X(LE1A),NA,NACT,NACT)
      END IF
C
C  END OF SINGLE EXCITATION ANALYSIS
C  NOW FOR DOUBLE EXCITATION ANALYSIS
C
      IF(MASWRK) WRITE(IW,7170)
      DO II=LE2A,LE2A + (NA*(NA+1))/2 - 1
         X(II) = X(II)/CI0
         E2COA = E2COA + X(II)
      ENDDO
      DO II=LE2B,LE2B + (NB*(NB+1))/2 - 1
         X(II) = X(II)/CI0
         E2COB = E2COB + X(II)
      ENDDO
      DO II=LE2AB,LE2AB + (NB*NA) - 1
         X(II) = X(II)/CI0
         E2COAB = E2COAB + X(II)
      ENDDO
      DO II=1,NB
         INZ = LE2AB-1+(II-1)*NB + II
         E2COD = E2COD + X(INZ)
      ENDDO
      E2COO = E2COAB - E2COD
C
      E2COT = E2COA + E2COB + E2COAB
      IF(MASWRK) THEN
         WRITE(IW,7180)
         WRITE(IW,7190) E2COA,E2COB,E2COO,E2COD,E2COT
         WRITE(IW,7200)
         CALL PRTRI(X(LE2A),NA)
         WRITE(IW,7210)
         CALL PRTRI(X(LE2B),NB)
         WRITE(IW,7220)
         CALL PRSQ(X(LE2AB),NA,NB,NB)
      END IF
C
      DO II=1,(NB*(NB+1))/2
         X(LE2A+II-1) = X(LE2A+II-1)+X(LE2B+II-1)
      ENDDO
      DO II=1,NB
         DO JJ=1,NA
            X(LE2A-1+INDEX(II,JJ))=X(LE2A-1+INDEX(II,JJ)) +
     *      X(LE2AB-1+(JJ-1)*NB + II)
         ENDDO
      ENDDO
      IF(MASWRK) THEN
         WRITE(IW,7230)
         CALL PRTRI(X(LE2A),NA)
      END IF
C
      ECOT = E1COT + E2COT
C
      IF(MASWRK) THEN
         WRITE(IW,7240) E1COT,E2COT,ECOT,ESTATE(1)-EHFT,ECOT,
     *                  ESTATE(1)-EHFT-ECOT
         WRITE(IW,7250)
      END IF
C
C   SUM UP SINGLE CONTRIBUTIONS INTO SINGLE OCCUPIED ORBITAL ONES.
C
      DO II=1,NA
         X(LE1B+II-1) = 0.0D0
         DO JJ=1,NACT
            X(LE1B+II-1) = X(LE1B+II-1) +
     *       X(LE1A+(II-1)*NACT + JJ - 1)
         ENDDO
      ENDDO
C
      DO M=1,NA
         IND1 = INDEX(M,M)
         DO N=1,M
            IND2 = INDEX(N,N)
            INDJ = INDEX(IND1,IND2)
            IND3 = INDEX(M,N)
            INDK = INDEX(IND3,IND3)
            IF (M.NE.N) THEN
               IF(MASWRK) WRITE(IW,7260) M,N,SI2(INDJ),SI2(INDK),
     *                                   X(LE2A+IND3-1)
            ELSE
               IF(MASWRK) WRITE(IW,7260) M,N,SI2(INDJ),SI2(INDK),
     *                                   X(LE2A+IND3-1),X(LE1B+M-1)
            ENDIF
         ENDDO
      ENDDO
      IF(MASWRK) WRITE(IW,*)
C
      CALL RETFM(NEED)
C
      RETURN
C
 7000 FORMAT(/1X,50("*")/
     *        11X,'CORRELATION ENERGY ANALYSIS'/
     *        1X,50(1H*)//
     *        1X,'CORRELATION ENERGY WILL BE DECOMPOSED IN TERMS OF '/
     *        1X,'CONTRIBUTIONS FROM ORBITALS AND ORBITAL PAIRS.'/
     *        1X,'METHOD BY J. IVANIC, K. RUEDENBERG.'/
     *        1X,'REFERENCE TO BE INCLUDED.')
 7020 FORMAT(/1X,'E(FCI) = ',F20.10/
     *        1X,'E(SCF) = ',F20.10,2X,'-'/
     *        1X,30(1H-)/
     *        1X,'E(COR) = ',F20.10)
 7025 FORMAT(/1X,'1ST DETERMINANT (0) AND ITS COEFFICIENT (C0) : - '/)
 7030 FORMAT(/1X,'SINGLE EXCITATION ANALYSIS AND ENERGY CONTRIBUTIONS'/
     *        1X,'---------------------------------------------------')
 7040 FORMAT(/1X,'ERROR IN EVALUATING 1ST DETERMINANT IN LIST!!')
 7050 FORMAT(/1X,'WARNING!!!',
     *       ' ALPHA OCCUPIED ORBITALS NOT FIRST NA ORBITALS !!!')
 7060 FORMAT(/1X,'WARNING!!!',
     *       ' BETA  OCCUPIED ORBITALS NOT FIRST NB ORBITALS !!!')
 7065 FORMAT(/1X,'E[ALPHA 1-E] = SUM(M=ALPHA OCC,V=ALPHA VIR) ',
     *           '[M|H|V]*C(M->V)/C0'/
     *        1X,'E[BETA  1-E] = SUM(M=BETA  OCC,V=BETA  VIR) ',
     *           '[M|H|V]*C(M->V)/C0'/
     *       /1X,'WHERE H = CORE MODIFIED ONE-ELECTRON HAMILTONIAN.'
     *      //1X,'E[ALPHA 2-E] = SUM(M=A OCC, V=A VIR) ',
     *           '<0|1/R|S(M->V)>*C(M->V)/C0'
     *       /1X,'E[BETA  2-E] = SUM(M=B OCC, V=B VIR) ',
     *           '<0|1/R|S(M->V)>*C(M->V)/C0')
 7070 FORMAT(/1X,'E[ALPHA 1-E] = ',F20.10/
     *        1X,'E[ALPHA 2-E] = ',F20.10,2X,'+'/
     *        1X,40(1H-)/
     *        1X,'E[ALPHA TOT] = ',F20.10)
 7080 FORMAT(/1X,'E[BETA  1-E] = ',F20.10/
     *        1X,'E[BETA  2-E] = ',F20.10,2X,'+'/
     *        1X,40(1H-)/
     *        1X,'E[BETA  TOT] = ',F20.10)
 7090 FORMAT(/1X,'E[SINGLE-EX] = ',F20.10)
 7100 FORMAT(/1X,'1-E ALPHA MATRIX [V,M] = [V|H|M]*C(M->V)/C0 : -')
 7110 FORMAT(/1X,'2-E ALPHA MATRIX [V,M] = ',
     *           '<0|1/R|S(M->V)>*C(M->V)/C0 : -')
 7120 FORMAT(/1X,'1-E BETA MATRIX [V,M] = [V|H|M]*C(M->V)/C0 : -')
 7130 FORMAT(/1X,'2-E BETA MATRIX [V,M] = ',
     *           '<0|1/R|S(M->V)>*C(M->V)/C0 : -')
 7140 FORMAT(/1X,'TOTAL ALPHA MATRIX [V,M] : -')
 7150 FORMAT(/1X,'TOTAL BETA MATRIX [V,M] : -')
 7160 FORMAT(/1X,'TOTAL SINGLE EXCITATION MATRIX [V,M] : -')
 7170 FORMAT(/1X,'DOUBLE EXCITATION ANALYSIS AND ENERGY CONTRIBUTIONS'/
     *        1X,'---------------------------------------------------')
 7180 FORMAT(/1X,'E[AA]  = SUM(M<N M,N=A OCC; V<W V,W=A VIR)',
     *           ' <0|1/R|D(MN->VW)>*C(MN->VW)/C0'/
     *        1X,'E[BB]  = SUM(M<N M,N=B OCC; V<W V,W=B VIR)',
     *           ' <0|1/R|D(MN->VW)>*C(MN->VW)/C0'//
     *        1X,'E[AB1] = SUM(M<>N M=BO,N=AO; V=BV,W=AV)',
     *           '    <0|1/R|D(MN->VW)>*C(MN->VW)/C0'//
     *        1X,'E[AB2] = SUM(M=B+A OCC; V=B VIR,W=A VIR)',
     *           '   <0|1/R|D(MM->VW)>*C(MM->VW)/C0')
 7190 FORMAT(/1X,'E[AA]        = ',F20.10/
     *        1X,'E[BB]        = ',F20.10/
     *        1X,'E[AB1]       = ',F20.10/
     *        1X,'E[AB2]       = ',F20.10,2X,'+'/
     *        1X,50(1H-)/
     *        1X,'E[DOUBLE-EX] = ',F20.10)
 7200 FORMAT(/1X,'2-E AA MATRIX [M,N] : -')
 7210 FORMAT(/1X,'2-E BB MATRIX [M,N] : -')
 7220 FORMAT(/1X,'2-E AB MATRIX [M,N] M=BETA OCC, N=ALPHA OCC : -')
 7230 FORMAT(/1X,'ORBITAL PAIR CONTRIBUTIONS [M,N] M<N, M,N = OCC : -')
 7240 FORMAT(/1X,'E[SINGLE-EX] = ',F20.10/
     *        1X,'E[DOUBLE-EX] = ',F20.10,2X,'+'/
     *        1X,40(1H-)/
     *        1X,'E[TOTAL-EX]  = ',F20.10//
     *        1X,'A GOOD MEASURE OF HOW WELL CI COEFFICIENTS ARE ',
     *           'CONVERGED IS TO COMPARE THE '/
     *        1X,'MINIMIZED CI ENERGY WITH THAT OBTAINED BY ABOVE ',
     *           'DECOMPOSITION.'//
     *        1X,'E[CORR]      = ',F20.10/
     *        1X,'E[TOTAL-EX]  = ',F20.10,2X,'-'/
     *        1X,40(1H-)/
     *        1X,'E[DIFF]      = ',F20.10)
 7250 FORMAT(/1X,'TABLE OF ORBITAL PAIRS, THEIR COULOMB, EXCHANGE ',
     *           'AND CORRELATION CONTRIBUTIONS'//
     *        1X,'PAIR (M,N)',3X,'J [MM|NN]',8X,'K [MN|MN]',
     *        4X,'2-E CORR CONTR',4X,'1-E CORR CONTR'/
     *        1X,75(1H-)/)
 7260 FORMAT(1X,2I3,F16.10,F17.10,F18.10,F18.10)
      END
