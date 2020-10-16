C  9 dec 03 - mws - synch common block runopt
C 16 Jun 03 - MWS - GCIDAV: work around Cray read errors
C 14 Jan 03 - MWS - ALGNCI: fix dir.trf., DETGCI: no spin array overflow
C  7 Aug 02 - MWS - GCIDAV: always print message if CI states uncnvgd
C 20 Jun 02 - MWS - GCIDAV: allow user to clobber CI eigenvector file
C 22 May 02 - JI  - correlation energy analysis implemented
C 22 May 02 - MWS - allow parallel execution by replicated computation
C 26 Mar 02 - KRG - use ABRT calls before STOP
C 25 Sep 01 - DGF - add GLIST=SACAS option
C  6 Sep 01 - MWS - add dummy arguments to NAMEIO call
C  1 Aug 01 - JI  - add general CI code to GAMESS
C
C*MODULE ALGNCI  *DECK ALGNCI
C     ------------------------------
      SUBROUTINE ALGNCI(NRNFG,NPFLG)
C     ------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,DOEXCH,
     *        DDITRF,DOOOOO,DOVOOO,DOVVOO,DOVOVO
C
      DIMENSION NRNFG(10),NPFLG(10)
C
      PARAMETER (MXRT=100)
C
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,S,SZ,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORB,
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NGCI,
     *                IGPDET,KSTSYM,NFTGCI
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CIGEN/8HCIGEN   /, RNONE/8HNONE    /
#else
      CHARACTER*8 :: CIGEN_STR
      EQUIVALENCE (CIGEN, CIGEN_STR)
      CHARACTER*8 :: RNONE_STR
      EQUIVALENCE (RNONE, RNONE_STR)
      DATA CIGEN_STR/"CIGEN   "/, RNONE_STR/"NONE    "/
#endif
C
C        driver for determinant based General CI calculations...
C
C        ----- read input defining the general CI dimensions -----
C
      CALL GCIINP(NPFLG(1),CIGEN)
C
C        ----- integral transformation -----
C
      DDITRF=GOPARR
      DOOOOO=.TRUE.
      DOVOOO=.FALSE.
      DOVVOO=.FALSE.
      DOVOVO=.FALSE.
      DOEXCH=SCFTYP.NE.RNONE
      CALL TRFMCX(NPFLG(2),NCOR,NORB,NORB,.FALSE.,DOEXCH,
     *            DDITRF,DOOOOO,DOVOOO,DOVVOO,DOVOVO)
C
C        ----- direct General CI calculation -----
C
      CALL DETGCI(NPFLG(3),.FALSE.,DDITRF)
C
C        ----- 1e- density matrix and natural orbitals -----
C
      IF(NRNFG(5).GT.0) CALL GCIDM1(NPFLG(5))
C
C        ----- state averaged 1e- and 2e- density matrix -----
C
      IF(NRNFG(6).GT.0) CALL GCIDM2(NPFLG(6))
      RETURN
      END
C*MODULE ALGNCI  *DECK GCIINP
      SUBROUTINE GCIINP(NPRINT,GPNAME)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME,PURES,GOPARR,DSKWRK,MASWRK,ABEL,ANALYS
C
      PARAMETER (MXATM=500, MXRT=100, MXAO=2047, MXSH=1000)
C
      COMMON /DETPAR/ ICLBBR,ANALYS
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,S,SZ,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORB,
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NGCI,
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
      COMMON /DONASK/ NEXTRA
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, TWO=2.0D+00)
C
      PARAMETER (NNAM=20,NLISTOPT=3)
C
      DIMENSION QNAM(NNAM),KQNAM(NNAM)
      DIMENSION FANT(8),LFANT(8),GANT(27),LGANT(8),
     *          GLSTYP(NLISTOPT),IRREPS(8)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QNAM/8HGROUP   ,8HISTSYM  ,
     *          8HNCORE   ,8HNACT    ,8HNELS    ,8HSZ      ,
     *          8HNSTATE  ,8HNSTGSS  ,8HNHGSS   ,8HMXXPAN  ,
     *          8HITERMX  ,8HCVGTOL  ,8HPRTTOL  ,
     *          8HIROOT   ,8HNFLGDM  ,8HPURES   ,8HWSTATE  ,
     *          8HGLIST   ,8HIRREPS  ,8HANALYS  /
#else
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"GROUP   ","ISTSYM  ",
     *          "NCORE   ","NACT    ","NELS    ","SZ      ",
     *          "NSTATE  ","NSTGSS  ","NHGSS   ","MXXPAN  ",
     *          "ITERMX  ","CVGTOL  ","PRTTOL  ",
     *          "IROOT   ","NFLGDM  ","PURES   ","WSTATE  ",
     *          "GLIST   ","IRREPS  ","ANALYS  "/
#endif
      DATA KQNAM/5,1,1,1,1,3,  1,1,1,1,1,3,3,  1,-1,0,-3,5,81,0/
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA GENWRD,GENCI/8HGEN     ,8HCIGEN   /
#else
      CHARACTER*8 :: GENCI_STR, GENWRD_STR
      EQUIVALENCE (GENCI, GENCI_STR), (GENWRD, GENWRD_STR)
      DATA GENWRD_STR,GENCI_STR/"GEN     ","CIGEN   "/
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
     *          'A"      ',"A       ","B       ","A       ",
     *          "B1      ","B2      ","B3      ","A1      ",
     *          "A2      ","B1      ","B2      ","AG      ",
     *          "BG      ","BU      ","AU      ","AG      ",
     *          "B1G     ","B2G     ","B3G     ","AU      ",
     *          "B1U     ","B2U     ","B3U     "/
#endif
      DATA LGANT/0,1,3,5,7,11,15,19/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA GLSTYP/8HEXTRNL  ,8HINPUT   ,8HSACAS   /
#else
      CHARACTER*8 :: GLSTYP_STR(NLISTOPT)
      EQUIVALENCE (GLSTYP, GLSTYP_STR)
      DATA GLSTYP_STR/"EXTRNL  ","INPUT   ","SACAS   "/
#endif
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
      IF (RUNTYP.EQ.HESS  .OR.  NT.EQ.1) GRPDET=FANT(1)
      KSTSYM = 1
C
      NCORE  = 0
      NACT   = 0
      NELS   = 0
      SZ     = (MUL-1)/TWO
C
C     Unit number for $JOB.GCI file, NEXTRA
C
      NFTGCI = 37
C
C     SET NEXTRA.  Don't ask, you'll know when you need to
C     play with this and it shouldn't be anytime soon, if ever.
C
      NEXTRA = 0
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
      CALL VICLR(IRREPS,1,8)
C
C          Assume that the list is read from the input stream
C
      GLIST   = GLSTYP(2)
      ANALYS = .FALSE.
C
C-----------Get input parameters------------------
C
      CALL NAMEIO(IR,JRET,GPNAME,NNAM,QNAM,KQNAM,
     *            GRPDET,KSTSYM,
     *            NCORE,NACT,NELS,SZ,NSTATE,NSTGSS,NHGSS,MXXPAN,ITERMX,
     *            CVGTOL,PRTTOL,IROOT,NFLGDM,PURES,WSTATE,GLIST,IRREPS,
     *            ANALYS,
     *            0,0,0,0,
     *            0,0,0,0,0,    0,0,0,0,0,    0,0,0,0,0,    0,0,0,0,0,
     *            0,0,0,0,0,    0,0,0,0,0,    0,0,0,0,0,    0,0,0,0,0)
      IF(JRET.EQ.2) THEN
         IF(MASWRK) WRITE(IW,9010) GPNAME
         CALL ABRT
      END IF
C
      IGPDET = -1
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
      MAXP  = MAX(MXXPAN,KST+1)
      MAXW1 = NHGSS
      NITER = ITERMX
      CRIT  = CVGTOL
      IF(NPRINT.NE.-23  .AND.  K.GT.MXRT) THEN
         IF(MASWRK) WRITE(IW,9035) K,MXRT
         CALL ABRT
      END IF
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
C        Presently the only parallelization of the general CI is
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
         IF (SOME) WRITE(IW,9060) RUNTYP,MXSTAT
         CALL ABRT
      END IF
C
C  5.  And finally we check to see if the GLIST option is valid.
C
      DO I=1,NLISTOPT
         IF (GLIST.EQ.GLSTYP(I)) GOTO 1313
      END DO
C
      IF(MASWRK) WRITE(IW,9075) (GLSTYP(I),I=1,NLISTOPT)
      CALL ABRT
C
 1313 CONTINUE
C
C Print details of the GCI calculation
C
      IF(SOME) THEN
         WRITE(IW,9100) GRPDET,STSYM,NCOR,NACT,
     *                  NA+NCOR,NA,NB+NCOR,NB,NORB
C
         WRITE(IW,9110) K,KST,MAXP,MAXW1,NITER,CRIT
         WRITE(IW,9115) GLIST
         IF(GPNAME.EQ.GENCI) THEN
            WRITE(IW,9120) IROOT
            WRITE(IW,9130) (NFLGDM(II),II=1,K)
         END IF
         IF(GPNAME.EQ.GENWRD) THEN
            WRITE(IW,9140) PURES
            WRITE(IW,9150) (IWTS(II),WSTATE(IWTS(II)),II=1,MXSTAT)
         END IF
         WRITE(IW,9155) ANALYS
         WRITE(IW,9160) NCOR,NACT
         CALL MOSYPR(X(LMOLAB),NCOR,NACT)
      ENDIF
C
      CALL RETFM(NEEDD)
C
C  Determine if an external list needs to be generated
C
      IF (GLIST.EQ.GLSTYP(2)) THEN
         CALL GCIGEN(IW,SOME)
      ENDIF
      IF (GLIST.EQ.GLSTYP(3)) THEN
         CALL GCICASG(IW,SOME,IRREPS)
      ENDIF
C
      RETURN
C
C
 9000 FORMAT(/5X,50("-")/
     *       5X,'    AMES LABORATORY DETERMINANTAL GENERAL CI'/
     *       5X,'PROGRAM WRITTEN BY JOE IVANIC AND KLAUS RUEDENBERG'/
     *       5X,50(1H-))
 9010 FORMAT(/1X,'**** ERROR, THIS RUN REQUIRES INPUT OF A $',A8,
     *          ' GROUP')
 9020 FORMAT(/1X,'**** ERROR, THIS RUN DOES NOT CORRECTLY SPECIFY',
     *          ' THE CI SPACE'/
     *     1X,'CHECK $',A8,' INPUT: NCORE=',I4,' NACT=',I4,' NELS=',I4)
 9030 FORMAT(/1X,'**** ERROR, GCI INPUT NCORE=',I4,' NELS=',I4,
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
 9155 FORMAT(/1X,'CORRELATION ENERGY ANALYSIS      =',L5)
 9060 FORMAT(/1X,'**** ERROR, RUNTYP=',A8,' REQUIRES ENERGY GRADIENT.'/
     *       1X,'THIS IS IMPOSSIBLE WHILE STATE AVERAGING. NAVG=',I5)
 9070 FORMAT(/1X,'**** CAUTION: GENERAL CI PROGRAM DOES NOT YET',
     *           ' RUN IN PARALLEL.'/
     *       1X,'A REDUNDANT GENERAL CI COMPUTATION WILL BE PERF',
     *          'ORMED BY ALL NODES.'/
     *       1X,'THIS MAY INHIBIT SCALABILITY.')
 9075 FORMAT(/1X,'**** ERROR, GLIST SPECIFICATION NOT RECOGNIZED.'/
     *       1X,'GLIST MUST BE ONE OF'/
     *       1X,A8,3X,A8,3X,A8,3X,A8)
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
 9115 FORMAT(/1X,'GLIST SPECIFICATION              =',3X,A8/)
 9120 FORMAT(1X,'CI PROPERTIES WILL BE FOUND FOR ROOT NUMBER',I4)
 9130 FORMAT(1X,'1E- DENSITY MATRIX OPTIONS ARE',20I2)
 9140 FORMAT(1X,'PURE SPIN STATE AVERAGED 1E- AND 2E- DENSITY MATRIX',
     *          ' OPTION=.',L1,'.')
 9150 FORMAT(2(1X,'STATE=',I4,' DM2 WEIGHT=',F10.5,4X,:))
 9160 FORMAT(/1X,'SYMMETRIES FOR THE',I4,' CORE,',I4,' ACTIVE ARE')
C
      END
C*MODULE ALGNCI  *DECK DETGCI
      SUBROUTINE DETGCI(NPRINT,CLABEL,DDITRF)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME,PACK2E,GOPARR,DSKWRK,MASWRK,CLABEL,JACOBI,DDITRF,
     *        ANALYS
C
      PARAMETER (MXRT=100)
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /DETPAR/ ICLBBR,ANALYS
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,S,SZ,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORB,
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NGCI,
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
      COMMON /DONASK/ NEXTRA
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
C
C     ----- driver for General CI computation -----
C
      SOME = MASWRK  .AND.  NPRINT.NE.-5
C
      IF(SOME) WRITE(IW,9000)
C
C        core contribution to the energy is obtained from -ecore-,
C        and from modifications to the transformed 1e- integrals.
C        this effectively removes core orbitals from the computation.
C
C
      ECONST = ECORE + ENUCR
      NTOT = NACT + NCORSV
      NTCO = NCORSV
      NORB = NACT
      NCOR = 0
      NSYM = 2**IGPDET
C
C     Determine binomial coefficients, start storage at X(LIFA).
C
      CALL VALFM(LOADFM)
      LIFA  = LOADFM + 1
      LAST  = LIFA   + ((NACT+1)*(NACT+1))/NWDVAR + 1
      NEED1 = LAST - LOADFM - 1
      CALL GETFM(NEED1)
C
      CALL BINOM6(X(LIFA),NACT)
C
C     Now to read the $JOB.GCI file to see how many dets to be read in.
C
      CALL SEQOPN(NFTGCI,'GCILIST','UNKNOWN',.FALSE.,'UNFORMATTED')
      LEN = 1
      CALL SQRINT(NFTGCI,NGCI,LEN)
      IF(SOME) WRITE(IW,9010) NGCI
C
C     Make memory for two integer lists, each of NGCI length.
C
      CALL VALFM(LOADFM)
      IPICA  = LOADFM + 1
      IPICB  = IPICA + NGCI/NWDVAR + 1
      LAST   = IPICB + NGCI/NWDVAR + 1
      NEED2 = LAST - LOADFM - 1
      IF(SOME) WRITE(IW,9020) NEED2+NEED1
      CALL GETFM(NEED2)
C
C     Read list.
C
      NGCI1 = NGCI
      CALL SQRINT(NFTGCI,X(IPICA),NGCI)
      CALL SQRINT(NFTGCI,X(IPICB),NGCI1)
      IF (NGCI1.EQ.0.OR.NGCI.EQ.0) THEN
         IF(SOME) WRITE(IW,9025)
         CALL ABRT
      ENDIF
C
C  ------  Check for spin consistency  -------
C
C     1/ Calculate memory requirements for sorting.
C
      CALL VALFM(LOADFM)
      JPICA  = LOADFM + 1
      JPICB  = JPICA + NGCI/NWDVAR + 1
      KPOSJ  = JPICB + (NGCI+NEXTRA)/NWDVAR + 1
      LAST   = KPOSJ + (NGCI+NEXTRA)/NWDVAR + 1
      NEED3  = LAST - LOADFM - 1
      MEMSP  = NEED3+NEED2+NEED1 + NGCI
      IF(SOME) WRITE(IW,9030) MEMSP
      CALL GETFM(NEED3)
C
      CALL VALFM(LOADFM)
      IPOS     = LOADFM + 1
      LAST    = IPOS + NGCI
      NEED4   = LAST - LOADFM - 1
      CALL GETFM(NEED4)
C
C     2/ Sort and make extra lists.
C
      IF(SOME) CALL TSECND(E0)
C
      CALL GCISRT(X(IPICA),X(IPICB),X(JPICA),X(JPICB),X(KPOSJ),
     *            NGCI,X(LIFA),NACT,NA,NB,X(IPOS),NALP,NBLP)
C
      IF(SOME) THEN
         CALL TSECND(E1)
         ELAP = E1 - E0
         E0 = E1
         WRITE(IW,9040) ELAP
      END IF
      CALL RETFM(NEED4)
C
C     3/  Calculate memory requirements for spin checking.
C
      CALL VALFM(LOADFM)
      IAC1  = LOADFM + 1
      IBC1  = IAC1 + NA
      IAC2  = IBC1 + NB
      IBC2  = IAC2 + NA
      ISD   = IBC2 + NB
      IDO   = ISD  + NA + NB
      LAST  = IDO + NA
      NEED4 = LAST - LOADFM - 1
      MEMSP = NEED3 + NEED2 + NEED1 + NEED4
      IF(SOME) WRITE(IW,9050) MEMSP
      CALL GETFM(NEED4)
C
C     4/ Now to check for spin consistency. We will overwrite
C        the old list with the sorted one and will add to the
C        end of this list any extra dets required for spin
C        consistency.
C
      CALL SEQREW(NFTGCI)
      CALL SQWINT(NFTGCI,NGCI,1)
      CALL SQWINT(NFTGCI,X(IPICA),NGCI)
      CALL SQWINT(NFTGCI,X(IPICB),NGCI)
C
      IF(SOME) CALL TSECND(E0)
C
      CALL SPCHEK(NFTGCI,X(IPICA),X(IPICB),X(JPICA),X(JPICB),
     *            X(KPOSJ),X(IAC1),X(IBC1),X(IAC2),X(IBC2),
     *            X(ISD),X(IDO),NGCI,NA,NB,NACT,X(LIFA),NREQ,NEXTRA)
C
      IF(SOME) THEN
         CALL TSECND(E1)
         ELAP = E1 - E0
         E0 = E1
         WRITE(IW,9060) ELAP
      END IF
      CALL RETFM(NEED4)
C
C     5/ If not spin consistent, then have to read extra list.
C        Overwrite old one to contain sorted new expanded list.
C
      IF (NREQ.GT.0) THEN
         IF(SOME) WRITE(IW,9070) NREQ,NREQ+NGCI
         CALL RETFM(NEED3)
         CALL RETFM(NEED2)
         NGCINEW = NGCI + NREQ
C
         CALL VALFM(LOADFM)
         IPICA = LOADFM + 1
         IPICB = IPICA + NGCINEW/NWDVAR + 1
         LAST  = IPICB + NGCINEW/NWDVAR + 1
         NEED2 = LAST - LOADFM - 1
         IF(SOME) WRITE(IW,*)
         IF(SOME) WRITE(IW,9020) NEED2+NEED1
         CALL GETFM(NEED2)
C
         CALL SEQREW(NFTGCI)
         LEN = 1
         CALL SQRINT(NFTGCI,NGAR,LEN)
         CALL SQRINT(NFTGCI,X(IPICA),NGCI)
         CALL SQRINT(NFTGCI,X(IPICB),NGCI)
         CALL READEX(NFTGCI,X(IPICA),X(IPICB),NGCI,NREQ)
         IF (NREQ.EQ.0) THEN
            IF(SOME) WRITE(IW,*) 'SOMETHING UNUSUAL HAS HAPPENED!!'
            CALL ABRT
         ENDIF
         NGCI = NGCINEW
C
C        Now to sort amended list and create extra lists.
C
C        a/  Memory for sorting.
C
         CALL VALFM(LOADFM)
         JPICA  = LOADFM + 1
         JPICB  = JPICA + NGCI/NWDVAR + 1
         KPOSJ  = JPICB + NGCI/NWDVAR + 1
         LAST   = KPOSJ + NGCI/NWDVAR + 1
         NEED3  = LAST - LOADFM - 1
         MEMSP  = NEED3+NEED2+NEED1 + NGCI
         IF(SOME) WRITE(IW,9030) MEMSP
         CALL GETFM(NEED3)
C
         CALL VALFM(LOADFM)
         IPOS     = LOADFM + 1
         LAST    = IPOS + NGCI
         NEED4   = LAST - LOADFM - 1
         CALL GETFM(NEED4)
C
C        b/ Sort and make extra lists.
C
         IF(SOME) CALL TSECND(E0)
C
         CALL GCISRT(X(IPICA),X(IPICB),X(JPICA),X(JPICB),X(KPOSJ),
     *            NGCI,X(LIFA),NACT,NA,NB,X(IPOS),NALP,NBLP)
C
         IF(SOME) THEN
            CALL TSECND(E1)
            ELAP = E1 - E0
            E0 = E1
            WRITE(IW,9040) ELAP
         END IF
         CALL RETFM(NEED4)
C
C       Finally spit the new sorted list over the old one.
C
         CALL SEQREW(NFTGCI)
         CALL SQWINT(NFTGCI,NGCI,1)
         CALL SQWINT(NFTGCI,X(IPICA),NGCI)
         CALL SQWINT(NFTGCI,X(IPICB),NGCI)
         IF(SOME) WRITE(IW,9080)
C
      ELSE
C
C     Make sorted lists again as spin checking ruins jpica -> kposj
C
         CALL VALFM(LOADFM)
         IPOS     = LOADFM + 1
         LAST    = IPOS + NGCI
         NEED4   = LAST - LOADFM - 1
         CALL GETFM(NEED4)
C
         IF(SOME) CALL TSECND(E0)
C
         CALL GCISRT(X(IPICA),X(IPICB),X(JPICA),X(JPICB),X(KPOSJ),
     *            NGCI,X(LIFA),NACT,NA,NB,X(IPOS),NALP,NBLP)
C
         IF(SOME) THEN
            CALL TSECND(E1)
            ELAP = E1 - E0
            E0 = E1
            WRITE(IW,9040) ELAP
         END IF
         CALL RETFM(NEED4)
      ENDIF
C
      CALL SEQCLO(NFTGCI,'KEEP')
C
C     6/ We have the list, which is spin consistent.
C        Now to determine memory for general CI.
C
      IDS = (2*MAXP + 1)*NGCI + MAXW1*(8 + MAXW1)+(MAXW1*(MAXW1+1))/2+
     *  (MAXP*MAXP) + K
      IIS = 5*NA + 2*NB + 5*(NA*(NACT-NA)) +
     *      ((NORB*(NORB+1))/2)**2 + 3*MAXW1 +
     *      3*NALP + K + NSYM*NSYM + 5*NGCI
      NTOTCI = NALP*NBLP
      IF(SOME) WRITE(IW,9110) NTOTCI
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
C        Allocate memory for the CI step
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
      NEED4  = LAST - LOADFM - 1
      NEEDCI = NEED1+NEED2+NEED3+NEED4
      IF(SOME) WRITE(IW,9130) NEEDCI
      CALL GETFM(NEED4)
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
         CALL VCLR(X(LCIVEC),1,K*NGCI)
         GO TO 450
      END IF
CC
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
C        ----- compute general ci wavefunction -----
C
      CALL GCIDET(IW,SOME,ECONST,
     *            0,X(LSINT1),M2,X(LSINT2),M4,NORB,NCOR,NGCI,NA,NB,
     *            K,KST,MAXP,MAXW1,NITER,CRIT,
     *            X(LIFA),X(LSP),X(LEL),X(LDWRK),IDS,X(LIWRK),IIS,
     *            NALP,NBLP,X(IPICA),X(IPICB),X(JPICA),X(JPICB),
     *            X(KPOSJ),IGPDET,NSYM,X(IBO),ISTAT)
C
      DO I=1,MIN(K,MXRT)
         ESTATE(I) = X(LEL-1+I) + ECONST
         SPINS(I)  = X(LSP-1+I)
      ENDDO
C
C        save energy quantities
C
  450 CONTINUE
      ETOT = ESTATE(IROOT)
      EELCT = ETOT - ENUCR
      STOT = SPINS(IROOT)
      SSQUAR = STOT*(STOT+ONE)
      STATN = K
C
C        save eigenvectors to disk
C
      IF (.NOT.ANALYS) THEN
         CALL SEQOPN(NFT12,'CIVECTR','UNKNOWN',.FALSE.,'UNFORMATTED')
         CALL SEQREW(NFT12)
         IF(MASWRK) WRITE(NFT12) K,NGCI
C
         DO 420 IST=1,K
            LCIVEC = LDWRK + (IST-1)*NGCI
            CALL STFASE(X(LCIVEC),NGCI,NGCI,1)
            CALL SQWRIT(NFT12,X(LCIVEC),NGCI)
  420    CONTINUE
         CALL SEQREW(NFT12)
      ENDIF
      CALL RETFM(NEED4)
      CALL RETFM(NEED3)
      CALL RETFM(NEED2)
      CALL RETFM(NEED1)
C
C  print results of the CI calculation
C
      IF (.NOT.ANALYS) CALL GCIPRT(IW,NFT12,SOME)
      IF(SOME) WRITE(IW,9140)
      IF(SOME) CALL TIMIT(1)
      IF(EXETYP.NE.CHECK  .AND.  ISTAT.NE.0 .AND. SCFTYP.NE.AMCSCF) THEN
         IF(MASWRK) WRITE(IW,9150)
         CALL ABRT
      END IF
      RETURN
C
 9000 FORMAT(/5X,50("-")/
     *       5X,'    AMES LABORATORY DETERMINANTAL GENERAL CI'/
     *       5X,'PROGRAM WRITTEN BY JOE IVANIC AND KLAUS RUEDENBERG'/
     *       5X,50(1H-))
 9010 FORMAT(/1X,'NUMBER OF DETERMINANTS TO BE READ IN   = ',I15)
 9020 FORMAT(1X,'MEMORY FOR DETERMINANT LIST STORAGE    = ',I12,
     *          ' WORDS')
 9025 FORMAT(/1X,'NUMBER OF DETERMINANTS IN LIST IS LESS THAN ',
     *           'NUMBER SPECIFIED.'/1X,'STOPPING RUN NOW !!!!')
 9030 FORMAT(1X,'MEMORY FOR SORTING AND FULL LISTS      = ',I12,
     *          ' WORDS')
 9040 FORMAT(1X,'TIME TAKEN FOR SORTING:        ',F13.1)
 9050 FORMAT(1X,'MEMORY FOR SPIN CONSISTENCY CHECK      = ',I12,
     *          ' WORDS')
 9060 FORMAT(1X,'TIME TAKEN FOR SPIN CHECK:     ',F13.1)
 9070 FORMAT(/1X,'**************************************************'/
     *        1X,'DETERMINANT LIST IS NOT SPIN CONSISTENT'/
     *        1X,'NUMBER OF EXTRA DETERIMINANTS NEEDED     = ',I7/
     *        1X,'DETERMINANT LIST WILL BE AMENDED TO INCLUDE EXTRAS'/
     *        1X,'TOTAL NUMBER OF DETERMINANTS IN LIST NOW = ',I7/
     *        1X,'**************************************************')
 9080 FORMAT(1X,'OLD LIST HAS BEEN OVERWRITTEN WITH NEW EXPANDED LIST')
 9110 FORMAT(1X,'NUMBER OF FULL SPACE DETERMINANTS FOR ACTIVE SPACE = ',
     *       I10)
 9130 FORMAT(1X,'THE DETERMINANT GENERAL CI REQUIRES',I12,' WORDS')
 9140 FORMAT(1X,'..... DONE WITH GENERAL CI COMPUTATION .....')
 9150 FORMAT(1X,'CI COMPUTATION DID NOT CONVERGE, JOB CANNOT CONTINUE')
      END
C
C*MODULE ALGNCI  *DECK GCIPRT
      SUBROUTINE GCIPRT(IW,NFT12,SOME)
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
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NGCI,
     *                IGPDET,KSTSYM,NFTGCI
      COMMON /ENRGYS/ ENUCR,EELCT,ETOT,STOT,SSQUAR,ECORE,ESCF,EERD,
     *                E1,E2,VEN,VEE,EPOT,EKIN,ESTATE(MXRT),STATN
      COMMON /FMCOM / X(1)
      COMMON /IOFILE/ IR,IZ,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
      DIMENSION FANT(8),GANT(27),LGANT(8)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA FANT/8HC1      ,8HCI      ,8HCS      ,8HC2      ,
     *          8HD2      ,8HC2V     ,8HC2H     ,8HD2H     /
#else
      CHARACTER*8 :: FANT_STR(8)
      EQUIVALENCE (FANT, FANT_STR)
      DATA FANT_STR/"C1      ","CI      ","CS      ","C2      ",
     *          "D2      ","C2V     ","C2H     ","D2H     "/
#endif
C
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
     *          'A"      ',"A       ","B       ","A       ",
     *          "B1      ","B2      ","B3      ","A1      ",
     *          "A2      ","B1      ","B2      ","AG      ",
     *          "BG      ","BU      ","AU      ","AG      ",
     *          "B1G     ","B2G     ","B3G     ","AU      ",
     *          "B1U     ","B2U     ","B3U     "/
#endif
      DATA LGANT/0,1,3,5,7,11,15,19/
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
      DSKWRK = .FALSE.
      M1 = NACT
      NTOT = NACT + NCORSV
      NTCO = NCORSV
      NSYM = 2**IGPDET
C
      CALL VALFM(LOADFM)
      LCIVEC = LOADFM + 1
      LIFA   = LCIVEC + NGCI
      IPICA  = LIFA + (M1+1)*(M1+1)
      IPICB  = IPICA + NGCI/NWDVAR + 1
      IACON  = IPICB + NGCI/NWDVAR + 1
      IBCON  = IACON + NA
      IGMUL  = IBCON + NA
      IWRK   = IGMUL + NSYM*NSYM
      IBO    = IWRK + 43
      LAST   = IBO + NTOT
      NEED = LAST - LOADFM - 1
      CALL GETFM(NEED)
C
      IF(SOME) WRITE(IW,9140) GRPDET
C
C        ----- print ci energies and eigenvectors -----
C        note that GPRICI destroys the eigenvectors.
C
      IF(NGCI.LE.20) THEN
         IOP=1
         NUMPRT=NGCI
         IF(SOME) WRITE(IW,9150)
      ELSE
         IOP=2
         NUMPRT=0
         IF(SOME) WRITE(IW,9160) PRTTOL
      END IF
C
C GET DATA
C
      CALL BINOM6(X(LIFA),M1)
      CALL DAREAD(IDAF,IODA,X(IBO),NTOT,262,1)
      CALL CORTRA(X(IBO),NTOT,NTCO)
      CALL GMUL(IGPDET,X(IGMUL),X(IWRK),X(IWRK+3),X(IWRK+6),X(IWRK+9))
      IZZ = 1
      DO I=1,8
         IF (GRPDET.EQ.FANT(I)) IZZ=I
      END DO
C
C   Read list
C
      CALL SEQOPN(NFTGCI,'GCILIST','UNKNOWN',.FALSE.,'UNFORMATTED')
      LEN = 1
      CALL SQRINT(NFTGCI,NGCI1,LEN)
      IF(NGCI1.NE.NGCI) THEN
         WRITE(IW,9175) NGCI1,NGCI
         CALL ABRT
      ENDIF
      CALL SQRINT(NFTGCI,X(IPICA),NGCI)
      CALL SQRINT(NFTGCI,X(IPICB),NGCI1)
      IF (NGCI1.EQ.0.OR.NGCI.EQ.0) THEN
         IF(SOME) WRITE(IW,9025)
         CALL ABRT
      ENDIF
      CALL SEQCLO(NFTGCI,'KEEP')
C
C   Read no. of coefficients.
C
      CALL SEQREW(NFT12)
      IF(MASWRK) READ(NFT12) NSTATS,NDETS
      IF (GOPARR) CALL DDI_BCAST(2511,'I',NSTATS,1,MASTER)
      IF (GOPARR) CALL DDI_BCAST(2512,'I',NDETS ,1,MASTER)
C
      IF(NSTATS.NE.K  .OR.  NDETS.NE.NGCI) THEN
         WRITE(IW,9180) NSTATS,NDETS,K,NGCI
         CALL ABRT
      END IF
C
C   Print out coefficients one vector at a time.
C
      DO 430 I=1,K
         CALL SQREAD(NFT12,X(LCIVEC),NGCI)
         CALL GCISYM(IW,X(LCIVEC),X(IPICA),X(IPICB),X(IACON),
     *         X(IBCON),X(IGMUL),NSYM,X(IWRK),X(IBO),JSTSYM)
            ZSTSYM = GANT(LGANT(IZZ)+JSTSYM)
         IF(SOME) THEN
            WRITE(IW,9170) I,ESTATE(I),SPINS(I),SZ,ZSTSYM
            IF(EXETYP.NE.CHECK) CALL GPRICI(IW,NA,NB,0,M1,
     *               X(LCIVEC),X(IACON),X(IBCON),
     *               IOP,PRTTOL,NUMPRT,X(IPICA),X(IPICB),NGCI)
C
         END IF
  430 CONTINUE
C
      CALL SEQREW(NFT12)
      DSKWRK = SVDSKW
      CALL RETFM(NEED)
      RETURN
C
 9025 FORMAT(/1X,'NUMBER OF DETERMINANTS IN LIST IS LESS THAN ',
     *           'NUMBER SPECIFIED.'/1X,'STOPPING RUN NOW !!!!')
 9140 FORMAT(/1X,'CI EIGENVECTORS WILL BE LABELED IN GROUP=',A8)
 9150 FORMAT(1X,'PRINTING ALL NON-ZERO CI COEFFICIENTS')
 9160 FORMAT(1X,'PRINTING CI COEFFICIENTS LARGER THAN',F10.6)
 9170 FORMAT(/1X,'STATE',I4,'  ENERGY= ',F20.10,'  S=',F6.2,
     *           '  SZ=',F6.2,:,'  SPACE SYM=',A4/)
 9175 FORMAT(/1X,'***** ERROR IN -GCIPRT- ROUTINE *****'/
     *       1X,'SIZE OF LIST TO BE READ IN              = ',I10/
     *       1X,'SIZE OF LIST IN CALCULATED WAVEFUNCTION = ',I10)
 9180 FORMAT(/1X,'***** ERROR IN -GCIPRT- ROUTINE *****'/
     *       1X,'CI EIGENVECTOR FILE HAS DATA FOR NSTATE,NDETS=',I3,I10/
     *       1X,'BUT THE PRESENT CALCULATION HAS NSTATE,NDETS=',I3,I10)
      END
C
C*MODULE ALGNCI   *DECK SQRINT
      SUBROUTINE SQRINT (LFILE,IREGION,LENGTH)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IREGION(LENGTH)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /IOFILE/ IR,IW,IP,IIS,IPK,IIDAF,NAV,IIODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C     READ -LENGTH- INTEGERS INTO -IREGION- FROM -LFILE-
C     NOTE THAT DUE TO ITS POSSIBLE RESET TO 0 BELOW, -LENGTH-
C     SHOULD BE A INTEGER VARIABLE, RATHER THAN AN INTEGER CONSTANT.
C
      IF (DSKWRK.OR.MASWRK) READ(LFILE, END=200, ERR=300) IREGION
C
C         IF RUNNING IN PARALLEL, AND THE FILE EXISTS ONLY
C         ON THE MASTER NODE (DSKWRK=.FALSE.), THEN THE DATA
C         SHOULD BE BROADCAST FROM THE MASTER TO ALL OTHER NODES.
C
      IF (GOPARR.AND.(.NOT.DSKWRK)) THEN
         CALL DDI_BCAST(2519,'I',IREGION,LENGTH,MASTER)
      END IF
      RETURN
C
C                  END OF FILE
C        THIS IS HANDLED BY RETURNING ZERO LENGTH READ, SO THE CALLER
C        CAN DETERMINE IF THIS IS REALLY AN ERROR, OR WAS EXPECTED.
C
  200 CONTINUE
      LENGTH=0
      RETURN
C
C                  ERROR READING FILE, PULL THE PLUG ON THE JOB
C
  300 CONTINUE
      WRITE(IW,9000) LFILE,ME,LENGTH
      CALL ABRT
      RETURN
C
 9000 FORMAT(1X,'SQRINT: ERROR READING FILE',I4,' ON NODE',I5,
     *          ' LENGTH=',I10)
      END
C*MODULE ALGNCI   *DECK SQWINT
      SUBROUTINE SQWINT (LFILE,IREGION,LENGTH)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL GOPARR,DSKWRK,MASWRK
      DIMENSION IREGION(LENGTH)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C     WRITE AN ARRAY IREGION OF LENGTH LENGTH TO UNIT LFILE
C
      IF (DSKWRK.OR.MASWRK) WRITE(LFILE,ERR=300) IREGION
C
      RETURN
C
  300 CONTINUE
      WRITE(6,9020) ME,LFILE
 9020 FORMAT(1X,'SQWINT: NODE',I4,
     *          ' ENCOUNTERED I/O ERROR WRITING UNIT',I4)
      CALL ABRT
      RETURN
      END
C
C*MODULE ALGNCI   *DECK GCISRT
C     ----------------------------------------------------
      SUBROUTINE GCISRT(IPICA,IPICB,JPICA,JPICB,KPOSJ,
     *                  NPIC,IFA,NACT,NA,NB,CI2,NALP,NBLP)
C     ----------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IFA(0:NACT,0:NACT)
      DIMENSION IPICA(NPIC),IPICB(NPIC)
      DIMENSION JPICA(NPIC),JPICB(NPIC),KPOSJ(NPIC)
      DIMENSION CI2(NPIC)
C
C    Code to resort determinants.
C
C    ipica,ipicb contain a list of determinants, alpha and
C    beta strings desired for a General CI.
C
C    ipica,ipicb will contain the list of determinants ordered
C    first according to alpha and then beta.
C    jpica,jpicb will contain the list of determinants ordered
C    first according to beta and then alpha.
C    kposj will contain the position that the determinant
C    jpica,jpicb is in ipica,ipicb.
C    CI2 is purely scratch.
C
      NALP = IFA(NACT,NA)
      NBLP = IFA(NACT,NB)
C
      IF (NPIC.EQ.1) THEN
         JPICA(1) = IPICA(1)
         JPICB(1) = IPICB(1)
         KPOSJ(1) = 1
         RETURN
      ENDIF
C
      DO 7789 II=1,NPIC
         DP1 = (IPICA(II)-1)
         DP2 = NBLP
         DP3 = IPICB(II)
         CI2(II) = DP1*DP2 + DP3
         JPICA(II) = IPICA(II)
         JPICB(II) = IPICB(II)
 7789 CONTINUE
C
      N = NPIC
      L=N/2+1
      IR=N
C
   10 CONTINUE
         IF (L.GT.1) THEN
            L=L-1
            RRA=CI2(L)
            IRRB=IPICA(L)
            IRRC=IPICB(L)
         ELSE
            RRA=CI2(IR)
            IRRB=IPICA(IR)
            IRRC=IPICB(IR)
            CI2(IR)=CI2(1)
            IPICA(IR)=IPICA(1)
            IPICB(IR)=IPICB(1)
C
            IR=IR-1
            IF (IR.EQ.1) THEN
               CI2(1)=RRA
               IPICA(1)=IRRB
               IPICB(1)=IRRC
C
               GOTO 122
            ENDIF
         ENDIF
         I=L
         J=L+L
   20    IF (J.LE.IR) THEN
            IF (J.LT.IR) THEN
                   IF (CI2(J).LT.CI2(J+1)) J=J+1
            ENDIF
            IF (RRA.LT.CI2(J)) THEN
               CI2(I)=CI2(J)
               IPICA(I)=IPICA(J)
               IPICB(I)=IPICB(J)
               I=J
               J=J+J
            ELSE
               J=IR+1
            ENDIF
         GOTO 20
         ENDIF
         CI2(I)=RRA
         IPICA(I)=IRRB
         IPICB(I)=IRRC
      GOTO 10
C
  122 CONTINUE
C
       DO 213 II=1,NPIC
         DP1 = (IPICB(II)-1)
         DP2 = NALP
         DP3 = IPICA(II)
         CI2(II) = DP1*DP2 + DP3
         KPOSJ(II) = II
         JPICA(II) = IPICA(II)
         JPICB(II) = IPICB(II)
  213 CONTINUE
C
      N = NPIC
      L=N/2+1
      IR=N
C
   30 CONTINUE
         IF (L.GT.1) THEN
            L=L-1
            RRA=CI2(L)
            JRRB=JPICA(L)
            JRRC=JPICB(L)
            KRRD=KPOSJ(L)
         ELSE
            RRA=CI2(IR)
            JRRB=JPICA(IR)
            JRRC=JPICB(IR)
            KRRD=KPOSJ(IR)
            CI2(IR)=CI2(1)
            JPICA(IR)=JPICA(1)
            JPICB(IR)=JPICB(1)
            KPOSJ(IR)=KPOSJ(1)
            IR=IR-1
            IF (IR.EQ.1) THEN
               CI2(1)=RRA
               JPICA(1)=JRRB
               JPICB(1)=JRRC
               KPOSJ(1)=KRRD
               GOTO 222
            ENDIF
         ENDIF
         I=L
         J=L+L
   40    IF (J.LE.IR) THEN
            IF (J.LT.IR) THEN
                   IF (CI2(J).LT.CI2(J+1)) J=J+1
            ENDIF
            IF (RRA.LT.CI2(J)) THEN
               CI2(I)=CI2(J)
               JPICA(I)=JPICA(J)
               JPICB(I)=JPICB(J)
               KPOSJ(I)=KPOSJ(J)
               I=J
               J=J+J
            ELSE
               J=IR+1
            ENDIF
         GOTO 40
         ENDIF
         CI2(I)=RRA
         JPICA(I)=JRRB
         JPICB(I)=JRRC
         KPOSJ(I)=KRRD
      GOTO 30
C
  222 CONTINUE
C
Cc
      RETURN
      END
C
C*MODULE ALGNCI   *DECK SPCHEK
C     -------------------------------------------------------------
      SUBROUTINE SPCHEK(IW,IPICA,IPICB,JPICA,KPICA,KPICB,IACON1,
     *                  IBCON1,IACON2,IBCON2,ISD,IDO,
     *                  NPIC,NA,NB,NACT,IFA,NEXT,NEED)
C     -------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IPICA(NPIC),IPICB(NPIC),JPICA(NPIC)
      DIMENSION KPICA(NPIC+NEED),KPICB(NPIC+NEED)
      DIMENSION IFA(0:NACT,0:NACT)
      DIMENSION IACON1(NA),IBCON1(NB),IACON2(NA),IBCON2(NB)
      DIMENSION ISD(NA+NB),IDO(NA)
      INTEGER POSDET
      LOGICAL GOPARR,MASWRK,DSKWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      NEXT = 0
      DO 599 II=1,NPIC
         JPICA(II) = 1
  599 CONTINUE
C
      DO 13 II=1,NA
         IACON1(II) = II
   13 CONTINUE
      INDA = 1
      IND = 1
  833 CONTINUE
      DO 799 IKK=INDA,IPICA(IND)-1
         CALL ADVANC(IACON1,NA,NACT)
  799 CONTINUE
      INDA = IPICA(IND)
C
      DO 113 II=1,NB
         IBCON1(II) = II
  113 CONTINUE
      INDB = 1
  733 CONTINUE
C
      DO 899 JKK=INDB,IPICB(IND)-1
         CALL ADVANC(IBCON1,NB,NACT)
  899 CONTINUE
      INDB = IPICB(IND)
C
C   I know now what the determinant in ipica(ind),ipicb(ind)
C   look like.
C
C   Check to see if I have found this one before.
C
      IF (JPICA(IND).EQ.0) GOTO 722
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
         JPICA(IND) = 0
         GOTO 722
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
      NODE = IFA(NSS,NSPA)
      IF (NODE.GT.NPIC+NEED) THEN
         WRITE(6,*) 'I NEVER THOUGHT THIS ERROR WOULD OCCUR!!!'
         WRITE(6,*) 'GET IN TOUCH WITH JOE IVANIC FOR INFO!!!'
         CALL ABRT
      ENDIF
C
      DO 88 II=1,NSPA
         IDO(II) = II
   88 CONTINUE
C
      DO 3000 IJK=1,NODE
         DO 90 II=1,NSD
            IACON2(II) = ISD(II)
   90    CONTINUE
         DO 105 II=1,NSPA
            IACON2(II+NSD) = ISD(NSD+IDO(II))
  105    CONTINUE
C
C   Must reorder here.
C
            DO 140 II=1,NA-1
               DO 142 JJ=II+1,NA
                  IF (IACON2(JJ).LT.IACON2(II)) THEN
                     KK=IACON2(II)
                     IACON2(II) = IACON2(JJ)
                     IACON2(JJ) = KK
                  ENDIF
  142          CONTINUE
  140       CONTINUE
C
         KPICA(IJK) = POSDET(NACT,NA,IACON2,IFA)
         CALL ADVANC(IDO,NSPA,NSS)
 3000 CONTINUE
C
C  Have worked out the alpha determinants, need to work out
C  the beta determinants now.
C
      NSPB = NB - NSD
         DO 76 II=1,NSPB
            IDO(II) = II
   76    CONTINUE
         DO 4000 IJK = 1,NODE
            DO 190 II=1,NSD
               IBCON2(II) = ISD(II)
  190       CONTINUE
            DO 205 II=1,NSPB
               IBCON2(II+NSD) = ISD(NSD+IDO(II))
  205       CONTINUE
C
C   Must reorder here.
C
            DO 240 II=1,NB-1
               DO 242 JJ=II+1,NB
                  IF (IBCON2(JJ).LT.IBCON2(II)) THEN
                     KK=IBCON2(II)
                     IBCON2(II) = IBCON2(JJ)
                     IBCON2(JJ) = KK
                  ENDIF
  242          CONTINUE
  240       CONTINUE
C
         KPICB(NODE-IJK+1) = POSDET(NACT,NB,IBCON2,IFA)
         CALL ADVANC(IDO,NSPB,NSS)
 4000 CONTINUE
C
C  Now have all the determinants with the same space function
C  in kpica,kpicb.  Now to check if they all appear in the
C  list.
C
      DO 8922 JJJ=1,NODE
         IDA = KPICA(JJJ)
         IDB = KPICB(JJJ)
         DO 9277 III=IND,NPIC
            IF (IPICA(III).LT.IDA) GOTO 9277
            IF (IPICA(III).GT.IDA) THEN
               IF(MASWRK) WRITE(IW) IDA,IDB
               NEXT = NEXT + 1
               GOTO 8922
            ENDIF
            IF (IDB.NE.IPICB(III)) GOTO 9277
            JPICA(III) = 0
            GOTO 8922
 9277    CONTINUE
         IF(MASWRK) WRITE(IW) IDA,IDB
         NEXT = NEXT + 1
 8922 CONTINUE
C
  722 CONTINUE
      IND = IND + 1
      IF (IND.GT.NPIC) GOTO 933
      IF (IPICA(IND).NE.IPICA(IND-1)) GOTO 833
      GOTO 733
C
  933 CONTINUE
      RETURN
      END
C
C*MODULE ALGNCI   *DECK READEX
      SUBROUTINE READEX (LFILE,IPICA,IPICB,LEN,LEXT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IPICA(LEN+LEXT),IPICB(LEN+LEXT)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /IOFILE/ IR,IW,IP,IIS,IPK,IIDAF,NAV,IIODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C     READ EXTRA DETERMINANTS INTO IPICA, IPICB.
C     LEXT = NUMBER OF EXTRA.
C     LEN = ORIGINAL NUMBER READ IN BEFORE SPIN CHECK.
C
      IF (DSKWRK.OR.MASWRK) THEN
         DO II=LEN+1,LEN+LEXT
            READ(LFILE, END=200, ERR=300) IPICA(II),IPICB(II)
         ENDDO
      ENDIF
      IF (GOPARR.AND.(.NOT.DSKWRK)) THEN
         CALL DDI_BCAST(2520,'I',IPICA,LEN+LEXT,MASTER)
         CALL DDI_BCAST(2521,'I',IPICB,LEN+LEXT,MASTER)
      END IF
C
      RETURN
C
C                  END OF FILE
C        THIS IS HANDLED BY RETURNING ZERO LENGTH READ, SO THE CALLER
C        CAN DETERMINE IF THIS IS REALLY AN ERROR, OR WAS EXPECTED.
C
  200 CONTINUE
      LEXT=0
      RETURN
C
C                  ERROR READING FILE, PULL THE PLUG ON THE JOB
C
  300 CONTINUE
      WRITE(IW,9000) LFILE,ME,LEXT
      CALL ABRT
      RETURN
 9000 FORMAT(1X,'ERROR READING FILE',I4,' ON NODE',I5,' LENGTH=',I10)
      END
C
C*MODULE ALGNCI   *DECK GCIDET
C     --------------------------------------------------------
      SUBROUTINE GCIDET(IW,SOME,ECONST,
     *                  ISARA,SI1,ISI1,SI2,ISI2,NORB,
     *                  NCOR,NCI,NA,NB,
     *                  K,KST,MAXP,MAXW1,NITER,CRIT,IFA,
     *                  SPIN,EL,CI,IDS,IWRK,IIS,NALP,NBLP,
     *                  IPICA,IPICB,JPICA,JPICB,KPOSJ,
     *                  IDSYM,NSYM,IOB,ISTAT)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL SOME,GOPARR,MASWRK,DSKWRK
      DIMENSION SI1(ISI1),SI2(ISI2)
      DIMENSION SPIN(KST),EL(MAXW1)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION CI(IDS),IWRK(IIS)
      DIMENSION IPICA(NCI),IPICB(NCI),JPICA(NCI),JPICB(NCI)
      DIMENSION KPOSJ(NCI)
      DIMENSION IOB(NORB-NCOR)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C   Code to do Det Gen CI, here we partition the memory up
C   and call routine gdavci which is the real guts of the CI.
C   Written by J. Ivanic '97
C
C   **************************************
C   Should have called routines cormod(cormod.f) and gmemci(gmemci.f)
C   and sort(sort.f) before this !!!!!!!!!!!!!
C   **************************************
C
C   isara     : = 0, then there is no initial CI coefficients specified
C                    in CI.
C             : = 1, then an initial guess at CI coefficients has been
C                    given in CI.
C   si1, si2  : 1 and 2 electron integrals in reverse canonical order.
C
C   norb,ncor : total no. of orbitals and no. of core orbs respectively
C             : ncor should equal 0, and norb = active no. of orbs for
C               real efficiency.  ie, Integrals should be adjusted such
C               that there are no core orbitals using routine cormod
C   nci       : number of specified determinants, size of CI essentially
C   na, nb    : number of active alpha and beta electrons respectively
C   k, kst    : no. of states required and no. of minimum/starting
C               states in CI procedure.
C   maxp      : Maximum number of vectors before transforming and
C               starting at kst.
C   maxw1     : Size of diagonalization for initial guess vectors.
C   niter,crit: Maximum number of total iters and convergence criterion.
C               I very strongly suggest crit=5.0d-5, this gives accuracy
C               to at least 8 decimal places.  niter I would make very
C               large,2000, because if you have reasonable orbitals,
C               states will have to converge eventually, unless you want
C               expectation values if isara=1
C   ifa       : Contains binomial coeffs, obtained by calling gmemci.
C   spin      : returned spin, spin(i) = spin of state i
C   EL        : EL(i) = total electronic energy of state i
C   CI        : First part of CI contains the returned CI coefficients,
C               CI((i-1)*nci + j) contains coefficient for determinant
C               j and state i.  The remainder of CI is used for scratch.
C   ids       : Total space for CI in order to do the CI, should be
C               obtained from gmemci
C   iwrk      : Scratch integer space
C   iis       : Space for iwrk in order to do the CI, should be obtained
C               from gmemci
C   nalp, nblp: No. of alpha and beta space products respectively,
C               got from gmemci
C   ipica,ipicb contain the list of determinants, alpha and beta string,
C               these are to be ordered first according to alpha and
C               then in each alpha subgroup, according to beta.
C   jpica,jpicb contain the list of determinants but ordered first
C               according to beta and then alpha
C   kposj       contains the position of determinant jpica(i),jpicb(i)
C               in the ipica,ipicb list.
C ********
C  ipica,ipicb,jpica,jpicb and kposj may be obtained from routine gcisrt
C  if you  have a list (in any order) of determinants.
C ********
C  idsym,nsym  : See symwrk.f, it depends on the point group...
C                C1,C2,Cs,Ci   idsym=1,nsym=2
C                C2v,D2,C2h    idsym=2,nsym=4
C                D2h           idsym=3,nsym=8
C   iob        : iob(i) contains symmetry of active orbital i, see gtab
C                in symwrk.f for information.
C
      NACT = NORB - NCOR
C
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
      ISYMX = IIND1 + (NA*(NACT-NA))
      IWRK1 = ISYMX + (NA*(NACT-NA))
      ISD = IWRK1 + 2*MAXW1
      ISO = ISD + NA+NB
      INDEX = ISO + NA
      LPICB1 = INDEX + ((NACT*(NACT+1))/2)**2
      LPICB2 = LPICB1 + NALP
      IPOSA1 = LPICB2 + NALP
      ITEST = IPOSA1 + (NA*(NACT-NA))
      IHMCON = ITEST + NALP
      IGMUL = IHMCON+K
      ITOT = IGMUL+ (NSYM*NSYM)
C
      IF (KCITOT.GT.IDS+1.OR.ITOT.GT.IIS+1) THEN
         IF(MASWRK) THEN
            WRITE(IW,*) 'NOT ENOUGH MEMORY SPECIFIED FOR CI'
            WRITE(IW,*) 'ASKED FOR ',IDS,' AND ',IIS,' DOUBLE '
            WRITE(IW,*) 'PRECISION AND INTEGER, ACTUALLY NEED'
            WRITE(IW,*) KCITOT-1,' AND ',ITOT-1
         END IF
         CALL ABRT
      ENDIF
C
      CALL GMUL(IDSYM,IWRK(IGMUL),IWRK(1),IWRK(4),IWRK(7),IWRK(10))
C
C  Check to see if we have an initial guess and if we do, make
C  sure all are normalized.
C
      IF (ISARA.EQ.1) THEN
         IF(MASWRK) WRITE(IW,*)
     *'INITIAL GUESS SUPPLIED, CHECKING FOR ORTHONORMALIZATION NOW.'
         DO 799 KK=1,KST
            DO 766 II=1,KK-1
               ROV = 0.0D+00
                  DO 811 JJ=1,NCI
                     ROV = ROV + CI(JJ+((KK-1)*NCI))*CI(JJ+((II-1)*NCI))
  811             CONTINUE
                  DO 911 JJ=1,NCI
                     CI(JJ+((KK-1)*NCI)) = CI(JJ+((KK-1)*NCI)) -
     *               ROV*CI(JJ+((II-1)*NCI))
  911             CONTINUE
  766      CONTINUE
C
           RNOR = 0.0D+00
           DO 411 II=1,NCI
              RNOR = RNOR + CI(II+((KK-1)*NCI))*CI(II+((KK-1)*NCI))
  411      CONTINUE
           RNOR = SQRT(RNOR)
           DO 422 II=1,NCI
              CI(II+((KK-1)*NCI)) = CI(II+((KK-1)*NCI))/RNOR
  422      CONTINUE
  799   CONTINUE
      ELSE
C
C       write(iw,*) ' No initial guess vectors supplied'
      ENDIF
C
      CALL GCIDAV(IW,SOME,ECONST,SI1,SI2,NACT,0,NCI,NA,NB,
     *           CI(KCOEF),SPIN,EL,
     *           K,KST,MAXP,MAXW1,NITER,CRIT,
     *    CI(KAB),CI(KQ),CI(KB),CI(KEF),CI(KF),IWRK(IWRK2),CI(KEC),
     *    IWRK(IACON1),IWRK(IBCON1),IWRK(IACON2),IWRK(IBCON2),
     *    IWRK(IPOSA),IWRK(IPERA),IWRK(IIND1),IWRK(ISYMX),
     *    IWRK(IWRK1),
     *    IWRK(ISD),IWRK(ISO),IFA,IWRK(INDEX),NALP,NBLP,
     *    IWRK(LPICB1),IWRK(LPICB2),IWRK(IPOSA1),IPICA,
     *    IPICB,JPICA,JPICB,KPOSJ,IWRK(ITEST),IWRK(IHMCON),
     *    CI(KGR),IWRK(IGMUL),NSYM,IOB,ISTAT)
C
      END
C
C*MODULE ALGNCI  *DECK GCIDAV
C     --------------------------------------------------------
      SUBROUTINE GCIDAV(IW,SOME,ECONST,
     *                  SI1,SI2,NORB,NCOR,NPIC,NA,NB,
     *                  CI,SPIN,EL,
     *                  K,KST,MAXP,MAXW1,NITER,CRIT,
     *                  AB,Q,B,EF,F,IWRK2,EC,
     *                  IACON1,IBCON1,IACON2,IBCON2,IPOSA,IPERA,
     *                  IIND1,ISYMX,IWRK1,ISD,IDO,
     *                  IFA,INDEX,NALP,NBLP,LPICB1,LPICB2,IPOSA1,
     *                  IPICA,IPICB,JPICA,JPICB,KPOSJ,ITEST,IHMCON,
     *                  GR,IGMUL,NSYM,IOB,ISTAT)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /DETPAR/ ICLBBR,ANALYS
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      LOGICAL SOME,ANALYS,GOPARR,DSKWRK,MASWRK
C
      DIMENSION CI(NPIC,MAXP),AB(NPIC,MAXP),Q(NPIC)
      DIMENSION SI1(*),SI2(*)
      DIMENSION F((MAXW1*(MAXW1+1))/2),EF(MAXW1,MAXW1),EL(MAXW1)
      DIMENSION EC(MAXP,MAXP),IWRK2(MAXW1),IWRK1(2*MAXW1),B(8*MAXW1)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2)
      DIMENSION IACON1(NA+NCOR),IBCON1(NA+NCOR)
      DIMENSION IACON2(NA+NCOR),IBCON2(NB+NCOR)
      DIMENSION IPERA(NA*(NORB-NCOR-NA))
      DIMENSION IIND1(NA*(NORB-NCOR-NA))
      DIMENSION ISYMX(NA*(NORB-NCOR-NA))
      DIMENSION IPOSA(NA*(NORB-NCOR-NA))
      DIMENSION ISD(NA+NB),IDO(NA)
      DIMENSION SPIN(KST)
      DIMENSION IPICA(NPIC),IPICB(NPIC)
      DIMENSION LPICB1(NALP),LPICB2(NALP)
      DIMENSION JPICA(NPIC),JPICB(NPIC),KPOSJ(NPIC)
      DIMENSION IPOSA1(NA*(NORB-NCOR-NA))
      DIMENSION ITEST(NALP)
      DIMENSION IHMCON(K)
      DIMENSION GR(K)
      DIMENSION IGMUL(NSYM*NSYM)
      DIMENSION IOB(NORB-NCOR)
C
C   Code to do a General CI almost completely direct
C   Written by J. Ivanic '98
C
C   si1, si2  : 1 and 2 electron integrals stored in reverse canonical
C               order, same as GAMESS
C   norb,ncor : total no. of orbitals and no. of core orbs respectively
C   npic      : number of determinants, size of CI essentially
C   na, nb    : number of active alpha and beta electrons respectively
C   CI        : returned CI coefs, CI(i,j) = coefficient of determinant
C               i for state j.
C   spin      : returned spin, spin(i) = spin of state i
C   EL        : EL(i) = total electronic energy of state i
C   k, kst    : Number of states required and number of minimum/starting
C               states in CI procedure.
C   maxp      : Maximum number of vectors before transforming and
C               starting at kst.
C   maxw1     : Size of diagonalization for initial guess vectors.
C   niter,crit: Max number of total iterations, convergence criterion.
C               I very strongly suggest crit=5.0d-5, this gives accuracy
C               to at least 8 decimal places.  niter I would make very
C               large, say 2000, because I am sure that if you have
C               reasonable orbitals, states will have to converge
C               eventually.
C   ipica,ipicb contain the list of determinants, alpha and beta string,
C               these are to be ordered first according to alpha and
C               then in each alpha subgroup, according to beta.
C   jpica,jpicb contain the list of determinants but ordered first
C               according to beta and then alpha
C   kposj       contains the position of the det jpica(i),jpicb(i) in
C               the ipica,ipicb list.
C    VARIABLES BELOW ARE ALL SCRATCH
C
C   Ab,q      : Scratch double precisions, Ab contains the matrix H.c,
C               and q contains diagonal elements of hamiltonian.
C   B,EF,F,EC : Double precisions for diagonalization routine, EVVRSP.
C               EC is used to check if states have flipped.
C   iwrk2     : Scratch integer array used in EVVRSP
C   iwrk1     : This is a scratch integer array used very effectively
C               in the inital guess.
C   isd,iso   : Used in retspin and other places, scratch int. arrays.
C   iacon1,ibcon1,iacon2,ibcon2,ipera,iposa,iind1 : scratch integer
C               arrays used in gretAb and grinAb which are really the
C               guts of the program.
C   ifa, index: ifa contains binomial coefficients, by calling binom6,
C               and index contains a list of positions for indices for
C               the integral arrays.
C  lpicb1,lpicb2,iposa1,itest are purely scratch
C
      ISTAT = 0
      NACT = NORB-NCOR
      NOSP = MIN(200,NPIC)
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
      IF (NB+NCOR.EQ.0) NBLP = 0
C
C   Initial setup, first work out diagonal elements.
C
      CALL QGET(SI1,SI2,NORB,NCOR,NA,NB,IACON1,IBCON1,
     *          INDEX,Q,NPIC,IPICA,IPICB,JPICB,KPOSJ)
C
C   Now to work out constants for zero testing in
C   routines rinAb and retAb
C
C      sait = 0.0d+00
C      do 312 ip=ncor+1,nact-3
C         do 313 iq=ip+1,nact-2
C            do 314 im=iq+1,nact-1
C               do 315 in=im+1,nact
C                  i1=index(ip,im)
C                  i2=index(iq,in)
C                  it=index(i1,i2)
C                  j1=index(ip,in)
C                  j2=index(iq,im)
C                  jt = index(j1,j2)
C                  siy=abs(si2(it) - si2(jt))
C                  if (siy.gt.sait) sait = siy
C  315          continue
C  314       continue
C  313    continue
C  312 continue
Cc
C      tait = 0.0d+00
C      do 412 ip=ncor+1,nact-1
C         do 413 im=ip+1,nact
C            do 414 iq=ncor+1,nact-1
C               do 415 in=iq+1,nact
C                  i1=index(ip,im)
C                  i2=index(iq,in)
C                  it=index(i1,i2)
C                  siy=abs(si2(it))
C                  if (siy.gt.tait) tait = siy
C  415          continue
C  414       continue
C  413    continue
C  412 continue
C
C ***
C     Keep option here to average diagonal elements .......
C
C      call averq(CI,nalp,nblp,na,nb,ncor,norb,iacon1,
C     *              ibcon1,iacon2,ibcon2,ifa,isd,ido,iwrk1,maxw1,
C     *              kst,q)
C ***
C
      IF (NPIC.LE.MAXW1) GOTO 2345
C
C        See if we have initial guess vectors on disk.
C
      NSTATE = 0
      NVECS  = 0
      IF (ICLBBR.EQ.1) GO TO 2345
C
      CALL SEQOPN(NFT12,'CIVECTR','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQREW(NFT12)
      IF(MASWRK) READ(NFT12,ERR=2343,END=2343) NSTATE,NVECS
      GO TO 2344
C
 2343 CONTINUE
      NSTATE=0
      NVECS=0
C
C        let other nodes know if anything was read
C
 2344 CONTINUE
      IF (GOPARR) CALL DDI_BCAST(2513,'I',NSTATE,1,MASTER)
      IF (GOPARR) CALL DDI_BCAST(2514,'I',NVECS ,1,MASTER)
C
C         if nothing read, we must make an initial guess
C
      IF (NSTATE+NVECS.EQ.0) GOTO 2345
C
C         if inconsistency read, we must terminate
C
      IF (NVECS.NE.NPIC) THEN
         IF (SOME) WRITE(IW,9005) NVECS,NPIC
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
C  Determine the initial guess vectors.
C
      DO 87 II=1,NPIC
         DO 89 JJ=1,MAXP
            CI(II,JJ) = 0.0D+00
   89    CONTINUE
   87 CONTINUE
C
C    Now to determine the initial guess vectors.
C
      DO 77 II=1,NPIC
         CI(II,1) = Q(II)
   77 CONTINUE
C
      IF(SOME) CALL TSECND(E0)
C
      CALL GCINITI(IW,B,NA,NB,NCOR,NORB,IACON1,IBCON1,
     *   IACON2,IBCON2,IFA,ISD,IDO,CI,IWRK1,MAXW1,KST,INDEX,F,EL,EF,
     *   SI1,SI2,IWRK2,IMARK,AB,NSIZE,NPIC,IPICA,IPICB,SOME)
C
C   Check if we have finished the CI by doing the first
C   diagonalization.
C
      IF (IMARK.EQ.1) THEN
         CALL GSPINDET(IW,NA,NB,NACT,IACON1,IACON2,IBCON1,IBCON2,
     *      ISD,IDO,CI,AB,IFA,K,SPIN,
     *      NPIC,IPICA,IPICB,NOSP,LPICB1,LPICB2,ITEST)
C
         IF (ANALYS.AND.SOME) THEN
            CALL ECORRG(IW,NFT12,
     *                  EL,ECONST,Q(1),CI,NPIC,K,SI1,SI2,NACT,NA,NB,
     *                  IFA,INDEX,IACON1,IACON2,IBCON1,IBCON2,
     *                  IGMUL,NSYM,IOB,
     *                  IPICA,IPICB,JPICA,JPICB,KPOSJ)
         ENDIF
         RETURN
      ENDIF
C
 3333 CONTINUE
C
      IF (NA.EQ.NB) THEN
         CALL GSPINDET(IW,NA,NB,NACT,IACON1,IACON2,IBCON1,IBCON2,
     *      ISD,IDO,CI,AB,IFA,KST,SPIN,
     *      NPIC,IPICA,IPICB,NOSP,LPICB1,LPICB2,ITEST)
      DO 856 I=1,KST
         IWRK1(I) = INT(SPIN(I) + 0.3D+00)
         IWRK2(I) = I
  856 CONTINUE
      ENDIF
C
      IF(SOME) THEN
         CALL TSECND(E1)
         ELAP = E1 - E0
         E0 = E1
         IF (NVECS.EQ.0) WRITE(IW,9010) ELAP
      END IF
C
      IF (NA.EQ.NB) THEN
C
      IF (KST.GT.1) THEN
      CALL GRINAB0(SI1,SI2,NORB,NCOR,NA,NB,CI,IACON1,IBCON1,
     *             IACON2,
     *       IFA,IPOSA,IPERA,IIND1,ISYMX,
     *    INDEX,AB,KST,Q,UAIA,UAIB,
     *       NALP,NBLP,
     *    NPIC,IPICA,IPICB,LPICB1,LPICB2,
     *    KPOSJ,IPOSA1,ITEST,IOB,IGMUL,NSYM,
     *    IWRK1,IWRK2)
      ELSE
      CALL GRETAB0(SI1,SI2,NORB,NCOR,NA,NB,CI,IACON1,IBCON1,
     *             IACON2,
     *       IFA,IPOSA,IPERA,IIND1,ISYMX,
     *    INDEX,AB,Q,UAIA,UAIB,
     *       NALP,NBLP,
     *    NPIC,IPICA,IPICB,LPICB1,LPICB2,
     *    KPOSJ,IPOSA1,ITEST,IOB,IGMUL,NSYM,
     *    IWRK1,IWRK2)
      ENDIF
C
      ELSE
      IF (KST.GT.1) THEN
      CALL GRINAB(SI1,SI2,NORB,NCOR,NA,NB,CI,IACON1,IBCON1,
     *             IACON2,
     *       IFA,IPOSA,IPERA,IIND1,ISYMX,
     *    INDEX,AB,KST,Q,UAIA,UAIB,
     *       NALP,NBLP,
     *    NPIC,IPICA,IPICB,LPICB1,LPICB2,
     *    JPICA,JPICB,KPOSJ,IPOSA1,ITEST,IOB,IGMUL,NSYM)
      ELSE
      CALL GRETAB(SI1,SI2,NORB,NCOR,NA,NB,CI,IACON1,IBCON1,
     *             IACON2,
     *       IFA,IPOSA,IPERA,IIND1,ISYMX,
     *    INDEX,AB,Q,UAIA,UAIB,
     *       NALP,NBLP,
     *    NPIC,IPICA,IPICB,LPICB1,LPICB2,
     *    JPICA,JPICB,KPOSJ,IPOSA1,ITEST,IOB,IGMUL,NSYM)
      ENDIF
C
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
         DO 14 JJ=1,II
            EF(II,JJ) = 0.0D+00
            EF(JJ,II) = 0.0D+00
            IF (II.EQ.JJ) EF(II,II) = 1.0D+00
   14    CONTINUE
         DO 15 KK=1,NPIC
            EL(II) = EL(II) + CI(KK,II)*AB(KK,II)
   15    CONTINUE
   13 CONTINUE
      DO 555 II=1,KST
         DO 677 JJ=1,KST
            EC(II,JJ) = EF(II,JJ)
  677 CONTINUE
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
C     and start over with ip = kst.
C
      IF (IP+NTCON.GT.MAXP) THEN
         CALL TRAN(CI,NPIC,MAXW1,EF,IP,EC,KST)
         CALL TRAN(AB,NPIC,MAXW1,EF,IP,EC,KST)
         IP = KST
         DO 1396 II=1,IP
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
C   Make gradient vector, put in CI(ip+1)
C
      DO 4588 KK=1,NTCON
      IL = IHMCON(KK)
      GR(KK) = 0.0D+00
      DO 80 II=1,NPIC
         CI(II,IP+KK) = 0.0D+00
         DO 70 JJ=1,IP
       CI(II,IP+KK) = CI(II,IP+KK) +
     *       EF(JJ,IL)*(AB(II,JJ) - EL(IL)*CI(II,JJ))
   70    CONTINUE
         GR(KK) = GR(KK) + (CI(II,IP+KK)*CI(II,IP+KK))
   80 CONTINUE
      GR(KK) = SQRT(GR(KK))
      IF(SOME) THEN
         WRITE(IW,9050) ITER,EL(IL)+ECONST,GR(KK)
         CALL FLSHBF(IW)
      END IF
 4588 CONTINUE
      IF (SOME.AND.NTCON.GT.1) WRITE(IW,*)
      IF (ITER.EQ.NITER) THEN
         CALL TRAN(CI,NPIC,MAXW1,EF,IP,EC,KST)
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
      IF (NUMC.GT.0) THEN
         CALL TRAN(CI,NPIC,MAXW1,EF,IP,EC,KST)
         CALL TRAN(AB,NPIC,MAXW1,EF,IP,EC,KST)
         NTCON = NTCON-NUMC
         DO 4233 II=1,NTCON
            IHMCON(II) = IWRK2(II)
 4233    CONTINUE
            DO 74 II=1,IP
               DO 75 JJ=1,IP
                  EF(II,JJ) = 0.0D+00
                  EC(II,JJ) = 0.0D+00
   75          CONTINUE
               EF(II,II) = 1.0D+00
               EC(II,II) = 1.0D+00
   74       CONTINUE
            IP = KST
            IF (NTCON.EQ.0) GOTO 444
            GOTO 333
  444 CONTINUE
         IF(SOME) WRITE(IW,*) 'ALL STATES CONVERGED.'
C
         CALL GSPINDET(IW,NA,NB,NACT,IACON1,IACON2,IBCON1,IBCON2,
     *      ISD,IDO,CI,AB,IFA,K,SPIN,
     *      NPIC,IPICA,IPICB,NOSP,LPICB1,LPICB2,ITEST)
C
         IF (ANALYS.AND.SOME) THEN
            CALL ECORRG(IW,NFT12,
     *                  EL,ECONST,Q(1),CI,NPIC,K,SI1,SI2,NACT,NA,NB,
     *                  IFA,INDEX,IACON1,IACON2,IBCON1,IBCON2,
     *                  IGMUL,NSYM,IOB,
     *                  IPICA,IPICB,JPICA,JPICB,KPOSJ)
         ENDIF
         RETURN
      ENDIF
C
      DO 68 JJ=IP+1,IP+NTCON
         IL = IHMCON(JJ-IP)
         DO 63 II=1,NPIC
            CI(II,JJ) = CI(II,JJ)/(EL(IL) - Q(II))
   63    CONTINUE
   68 CONTINUE
C
C If Ms=0, impose restriction on the CI coefficients.
C
      IF (NA.EQ.NB) THEN
C
      DO 977 II=1,NPIC
C
         IF (IPICA(II).EQ.IPICB(II)) THEN
            DO 9017 KJ=1,NTCON
            NV = IHMCON(KJ)
            IPS = IWRK1(NV)/2
            IF (IPS*2.NE.IWRK1(NV)) CI(II,KJ+IP) = 0.0D+00
 9017       CONTINUE
            GOTO 977
         ENDIF
C
         JJ = KPOSJ(II)
         IF (JJ.LT.II) GOTO 977
         DO 9018 KJ = 1,NTCON
            NV = IHMCON(KJ)
            IS = (-1)**IWRK1(NV)
            CI(JJ,KJ+IP) = IS*CI(II,KJ+IP)
 9018    CONTINUE
C
  977 CONTINUE
C
      ENDIF
C
C    Assume the new vector is Bi, have to orthogonalize
C    these vectors to all others and then renormalize.
C
      DO 97 KK=IP+1,IP+NTCON
      DO 86 II=1,KK-1
          ROV = 0.0D+00
         DO 81 JJ=1,NPIC
            ROV = ROV + CI(JJ,KK)*CI(JJ,II)
   81    CONTINUE
         DO 90 JJ=1,NPIC
            CI(JJ,KK) = CI(JJ,KK) - ROV*CI(JJ,II)
   90    CONTINUE
   86 CONTINUE
C
      RNOR = 0.0D+00
      DO 40 II=1,NPIC
         RNOR = RNOR + CI(II,KK)*CI(II,KK)
   40 CONTINUE
      RNOR = SQRT(RNOR)
      DO 42 II=1,NPIC
         CI(II,KK) = CI(II,KK)/RNOR
   42 CONTINUE
   97 CONTINUE
C
      IP = IP + 1
C
C     Now to return the new part of Ab
C
      IF (NA.EQ.NB) THEN
C
      IF (NTCON.GT.1) THEN
      CALL GRINAB0(SI1,SI2,NORB,NCOR,NA,NB,CI(1,IP),IACON1,IBCON1,
     *             IACON2,
     *       IFA,IPOSA,IPERA,IIND1,ISYMX,
     *      INDEX,AB(1,IP),NTCON,Q,UAIA,UAIB,
     *       NALP,NBLP,
     *    NPIC,IPICA,IPICB,LPICB1,LPICB2,
     *    KPOSJ,IPOSA1,ITEST,IOB,IGMUL,NSYM,
     *    IWRK1,IHMCON)
      ELSE
      CALL GRETAB0(SI1,SI2,NORB,NCOR,NA,NB,CI(1,IP),IACON1,IBCON1,
     *             IACON2,
     *       IFA,IPOSA,IPERA,IIND1,ISYMX,
     *      INDEX,AB(1,IP),Q,UAIA,UAIB,
     *       NALP,NBLP,
     *    NPIC,IPICA,IPICB,LPICB1,LPICB2,
     *    KPOSJ,IPOSA1,ITEST,IOB,IGMUL,NSYM,
     *    IWRK1,IHMCON)
      ENDIF
      ELSE
      IF (NTCON.GT.1) THEN
      CALL GRINAB(SI1,SI2,NORB,NCOR,NA,NB,CI(1,IP),IACON1,IBCON1,
     *             IACON2,
     *       IFA,IPOSA,IPERA,IIND1,ISYMX,
     *      INDEX,AB(1,IP),NTCON,Q,UAIA,UAIB,
     *       NALP,NBLP,
     *    NPIC,IPICA,IPICB,LPICB1,LPICB2,
     *    JPICA,JPICB,KPOSJ,IPOSA1,ITEST,IOB,IGMUL,NSYM)
      ELSE
      CALL GRETAB(SI1,SI2,NORB,NCOR,NA,NB,CI(1,IP),IACON1,IBCON1,
     *             IACON2,
     *       IFA,IPOSA,IPERA,IIND1,ISYMX,
     *      INDEX,AB(1,IP),Q,UAIA,UAIB,
     *       NALP,NBLP,
     *    NPIC,IPICA,IPICB,LPICB1,LPICB2,
     *    JPICA,JPICB,KPOSJ,IPOSA1,ITEST,IOB,IGMUL,NSYM)
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
            DO 115 KK=1,NPIC
               F(IX) = F(IX) + CI(KK,II)*AB(KK,JJ)
  115       CONTINUE
  102    CONTINUE
  103 CONTINUE
C
C  Diagonalize small matrix
C
C
      CALL EVVRSP(-1,IP,IP,(IP*(IP+1))/2,MAXW1
     *              ,F,B,IWRK2,EL,EF,0,IERR)
      IF (IERR.NE.0) THEN
         IF(MASWRK) WRITE(IW,*) 'ERROR IN SMALL DIAGONALIZATION'
         IF(MASWRK) WRITE(IW,*) IERR
         RETURN
      ENDIF
C
C   Check to see if any states have skipped in
C
      DO 700 IJK=1,NTCON
         IK = IHMCON(IJK)
         IDXC = 0
         POV = 0.0D+00
          DO 713 JJ=1,KST
            UIT = 0.0D+00
            DO 715 KK=1,IP-1
               UIT = UIT + EF(KK,JJ)*EC(KK,IK)
  715       CONTINUE
            IF (ABS(UIT).GT.POV) THEN
                DO 733 IH=1,IJK-1
                  IF (IHMCON(IH).EQ.JJ) GOTO 713
  733          CONTINUE
               POV = ABS(UIT)
               IDXC = JJ
               ENDIF
  713    CONTINUE
         IF (IDXC.NE.IK) THEN
            IHMCON(IJK) = IDXC
            IF(MASWRK) WRITE(IW,'(A,I3,A,I3)')
     *                      'STATE ',IK,' IS NOW ',IDXC
         ENDIF
  700 CONTINUE
C
C  Check to see where the spins occur now
C
      IF (NA.EQ.NB) THEN
C
      DO 800 II=1,KST
         POV = 0.0D+00
         DO 813 JJ=1,KST
            UIT = 0.0D+00
            DO 823 KK=1,IP-1
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
      DO 786 KK=1,KST
         SPIN(KK) = GR(KK)
  786 CONTINUE
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
      CALL GSPINDET(IW,NA,NB,NACT,IACON1,IACON2,IBCON1,IBCON2,
     *      ISD,IDO,CI,AB,IFA,K,SPIN,
     *      NPIC,IPICA,IPICB,NOSP,LPICB1,LPICB2,ITEST)
      ISTAT = 1
C
      RETURN
 9005 FORMAT(/1X,'ERROR, NUMBER OF VECTORS STORED=',I5,
     *           ' NOT EQUAL TO NCI=',I5/
     *        1X,'(THIS MAY BE DUE TO A GARBAGE -CIVECTR- FILE',
     *           ' LEFT OVER FROM AN EARLIER RUN.)')
 9007 FORMAT(1X,'INITIAL CI VECTORS READ FROM DISK')
 9010 FORMAT(1X,'INITIAL CI VECTOR GUESS TIME  :',F13.1)
 9020 FORMAT(1X,'INITIAL GENERAL CI ITERATION TIME:',F13.1)
 9040 FORMAT(/1X,'ITERATION',6X,'ENERGY',11X,'GRADIENT')
 9050 FORMAT(1X,I5,F20.10,F15.8)
 9060 FORMAT(/1X,'CONVERGED STATE',I5,' ENERGY=',F20.10,' IN',
     *           I5,' ITERS'/)
 9080 FORMAT(1X,'DETERMINANT GENERAL CI CONVERGED ONLY',I4,' ROOTS.')
C
      END
C
C*MODULE ALGNCI  *DECK QGET
C     --------------------------------------------------------
      SUBROUTINE QGET(SI1,SI2,NORB,NCOR,NA,NB,IACON1,IBCON1,
     *                INDEX,Q,NPIC,IPICA,IPICB,JPICB,KPOSJ)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION SI1(*),SI2(*),IACON1(NA+NCOR),IBCON1(NB+NCOR)
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2),Q(NPIC)
      DIMENSION IPICA(NPIC),IPICB(NPIC),JPICB(NPIC)
      DIMENSION KPOSJ(NPIC)
C
      NAT = NA + NCOR
      NBT = NB + NCOR
C
      DO 13 II=1,NPIC
         Q(II) = 0.0D+00
   13 CONTINUE
      DO 30 II=1,NAT
         IACON1(II) = II
   30 CONTINUE
C
C     Big loop over Alpha determinants in ipica
C
      IZX = 1
      IZY = 1
 9013 CONTINUE
      DO 7010 KK=IZX,IPICA(IZY)-1
         CALL ADVANC(IACON1,NAT,NORB)
 7010 CONTINUE
      DO 7013 IZW=IZY,NPIC
         IF (IPICA(IZY).NE.IPICA(IZW)) GOTO 7015
 7013 CONTINUE
C
 7015 IZW = IZW - 1
C
C  Now we have the indices in ipica from izy -> izw
C  and the beta determinants are in increasing
C  order ipicb(izy) -> ipicb(izw) *****
C
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
C  Loop over beta dets in ipicb(izy -> izw)
C
          NZX = 1
          NZY = IZY
 6613     CONTINUE
          DO 5510 KK=NZX,IPICB(NZY)-1
             CALL ADVANC(IBCON1,NBT,NORB)
 5510     CONTINUE
C
             ICIT = NZY
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
C
          NZX = IPICB(NZY)
          NZY = NZY + 1
          IF (NZY.NE.IZW+1) GOTO 6613
C
C  End of Beta loop
C
      IZX = IPICA(IZW)
      IZY = IZW + 1
      IF (IZW.NE.NPIC) GOTO 9013
C
C  End of the original alpha loop
C
C  Now for the Beta part
C
      DO 876 JJI = 1,NBT
         IBCON1(JJI) = JJI
  876 CONTINUE
C
C  Big loop over betas in jpicb
C
      IZX = 1
      IZY = 1
 5013 CONTINUE
      DO 3010 KK=IZX,JPICB(IZY)-1
         CALL ADVANC(IBCON1,NBT,NORB)
 3010 CONTINUE
      DO 3013 IZW = IZY,NPIC
         IF (JPICB(IZY).NE.JPICB(IZW)) GOTO 3015
 3013 CONTINUE
 3015 IZW = IZW - 1
C
C  Now we have indices in jpicb from izy -> izw
C  and the alpha determinants are in increasing
C  order jpica(izy) -> jpica(izw) *****
C
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
C  Loop over alpha dets in jpica(izy -> izw)
C
         DO 93 INA1 = IZY,IZW
            ICIT = KPOSJ(INA1)
            Q(ICIT) = Q(ICIT) + C
   93    CONTINUE
C
      IZX = JPICB(IZW)
      IZY = IZW + 1
      IF (IZW.NE.NPIC) GOTO 5013
C
      RETURN
      END
C
C*MODULE ALGNCI  *DECK GCINITI
C     --------------------------------------------------------
      SUBROUTINE GCINITI(IW,B,NA,NB,NCOR,NORB,IACON1,
     *              IBCON1,IACON2,IBCON2,IFA,ISD,IDO,CI,IWRK1,MAXWX,
     *              KST,INDEX,F,EL,EF,SINT1,SINT2,IWRK2,IMARK,AB,
     *              NSIZE,NPIC,IPICA,IPICB,SOME)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL SOME
      DIMENSION SINT1(*),SINT2(*),IWRK2(MAXWX),AB(NPIC,KST)
      DIMENSION IACON2(NA+NCOR),IBCON2(NB+NCOR),B(8*MAXWX)
      DIMENSION IACON1(NA+NCOR),IBCON1(NB+NCOR),ISD(NA+NB)
      DIMENSION CI(NPIC,KST),IFA(0:NORB-NCOR,0:NORB-NCOR),IWRK1(2*MAXWX)
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2),IDO(NA)
      DIMENSION F((MAXWX*(MAXWX+1))/2),EF(MAXWX,MAXWX),EL(MAXWX)
      DIMENSION IPICA(NPIC),IPICB(NPIC)
      INTEGER POSDET
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      MAXW1 = MAXWX
      NACT = NORB-NCOR
      IBG = 1
      IMARK = 0
      IF (NPIC.LE.MAXW1) THEN
         MAXW1 = NPIC
         IMARK = 1
         IF(SOME)WRITE(IW,*)
     *      'SMALL CI MATRIX, JUST USING INCORE DIAGONALIZATION'
      ELSE
         IF(NB.EQ.0) THEN
            WRITE(IW,9020) NPIC,MAXW1,NPIC
            CALL ABRT
         END IF
      ENDIF
      IF (KST.GT.NPIC) THEN
         IF(SOME) WRITE(IW,9010) KST,NPIC
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
      DO 29 II=1,NPIC
         IF (CI(II,1).LT.PMIN) THEN
            IND = II
            PMIN = CI(IND,1)
         ENDIF
   29 CONTINUE
C
      DO 13 II=1,NA
         IACON1(II) = II
   13 CONTINUE
      DO 113 II=1,NB
         IBCON1(II) = II
  113 CONTINUE
C
C  I know now that the deteminant is in ipica(ind),ipicb(ind)
C  Now to generate them.
C
      DO 15 IJK = 1,IPICA(IND)-1
         CALL ADVANC(IACON1,NA,NACT)
   15 CONTINUE
      DO 50 IJK = 1,IPICB(IND)-1
         CALL ADVANC(IBCON1,NB,NACT)
   50 CONTINUE
      INNB = IPICB(IND)
C
Cc      inna = 1
C      ici = 0
C      do 100 ijk=1,nalp
C         do 15 ii=1,nb
C            ibcon1(ii) = ii
C   15    continue
C         innb = 1
C         do 50 kji=1,nblp
C         ici = ici + 1
C         if (ici.eq.ind) goto 200
C         call advanc(ibcon1,nb,nact)
C         innb = innb + 1
C   50    continue
C         call advanc(iacon1,na,nact)
C         inna = inna + 1
C  100 continue
C
C  200 continue
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
         IWRK1(IBG) = IND
         IWRK1(IBG+MAXW1) = INNB
         CI(IND,1) = 0.0D+00
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
      NODE = IFA(NSS,NSPA)
      IF (NODE+IBG-1.GT.MAXW1) GOTO 9999
      DO 88 II=1,NSPA
         IDO(II) = II
   88 CONTINUE
C
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
         IWRK1(IJK+IBG-1) = POSDET(NACT,NA,IACON1,IFA)
         CALL ADVANC(IDO,NSPA,NSS)
 3000 CONTINUE
C
C  Have worked out the alpha determinants, need to work out
C  the beta determinants now.
C
      NSPB = NB - NSD
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
C  Now have the alpha determinants stored in iwrk1(ibg -> ibg+node-1)
C  and the betas in iwrk1(ibg+maxw1 -> ibg+node-1+maxw1).
C  Now to find these determinants in the list so we know where
C  they occur.
C
      DO 339 III=1,NODE
C
         DO 349 JJJ=1,NPIC
            IF (IWRK1(IBG+III-1).EQ.IPICA(JJJ)) GOTO 449
  349    CONTINUE
         IF(MASWRK) WRITE(IW,*) 'SOMETHING IS WRONG WITH THE LIST ALPHA'
         CALL ABRT
  449    CONTINUE
         DO 529 KKK=JJJ,NPIC
            IF (IPICA(JJJ).NE.IPICA(KKK)) GOTO 539
  529    CONTINUE
  539    KKK = KKK - 1
C
C  Now I have found the alpha and it occurs from jjj -> kkk
C  Now to look for the beta in there.
C
         DO 549 IJK=JJJ,KKK
            IF (IWRK1(III+IBG+MAXW1-1).EQ.IPICB(IJK)) GOTO 569
  549    CONTINUE
         IF(MASWRK) WRITE(IW,*) 'SOMETHING IS WRONG WITH THE LIST BETA'
         CALL ABRT
  569    CONTINUE
C
C   Okay, so we have the determinant in ipica(ijk),ipicb(ijk)
C  Now to put ijk in iwrk1(ibg+iii-1)
C
         IWRK1(IBG+III-1) = IJK
C
C  Zero the diagonal element just found
C
         CI(IJK,1) = 0.0D+00
C
  339 CONTINUE
C
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
         DO 344 II=1,IPICA(IWRK1(IJK))-1
            CALL ADVANC(IACON1,NA+NCOR,NORB)
  344    CONTINUE
         DO 355 IJ=1,IPICB(IWRK1(IJK))-1
            CALL ADVANC(IBCON1,NB+NCOR,NORB)
  355    CONTINUE
C
C
      DO 5000 KJI = 1,IJK
         IXI = IXI + 1
         DO 676 II=1,NA+NCOR
            IACON2(II) = II
  676    CONTINUE
         DO 675 II=1,NB+NCOR
            IBCON2(II) = II
  675    CONTINUE
         DO 455 JJ=1,IPICA(IWRK1(KJI))-1
            CALL ADVANC(IACON2,NA+NCOR,NORB)
  455    CONTINUE
         DO 735 II=1,IPICB(IWRK1(KJI))-1
            CALL ADVANC(IBCON2,NB+NCOR,NORB)
  735    CONTINUE
C
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
      DO 347 II=1,NPIC
         DO 450 JJ=1,KST
         CI(II,JJ) = 0.0D+00
         AB(II,JJ) = 0.0D+00
  450    CONTINUE
  347 CONTINUE
C
C *** For debuggin
C      write(iw,*) (EL(i),i=1,10)
C      write(iw,*) nsize
C      write(iw,*) maxwx
C      do 222 ii=1,nsize
C      write(iw,'(3f20.15)') (EF(i,ii),i=1,nsize)
C  222 continue
C ***
C
      DO 799 IJK=1,KST
         DO 899 II=1,NSIZE
            KI = IWRK1(II)
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
C
      END
C
C*MODULE ALGNCI  *DECK GRINAB0
C     --------------------------------------------------------
      SUBROUTINE GRINAB0(SI1,SI2,NORB,NCOR,NA,NB,CI,IACON1,IBCON1,
     *               IACON2,IFA,IPOSA,IPERA,IIND1,ISYMX,
     *     INDEX,AB,NV,Q,
     *               UAIA,UAIB,NALP,NBLP,
     *     NPIC,IPICA,IPICB,LPICB1,LPICB2,
     *     KPOSJ,IPOSA1,ITEST,IOX,IGMUL,NSYM,
     *    ISPIN,IHMCON)
C     --------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
      DIMENSION AB(NPIC,NV)
      DIMENSION SI1(*),SI2(*)
      DIMENSION CI(NPIC,NV),IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION IACON1(NA+NCOR),IBCON1(NA+NCOR)
      DIMENSION IACON2(NA+NCOR),IPERA(NA*(NORB-NCOR-NA))
      DIMENSION IIND1(NA*(NORB-NCOR-NA))
      DIMENSION IPOSA(NA*(NORB-NCOR-NA))
      DIMENSION ISYMX(NA*(NORB-NCOR-NA))
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2)
      DIMENSION Q(NPIC)
      DIMENSION IPICA(NPIC),IPICB(NPIC)
      DIMENSION LPICB1(NALP),LPICB2(NALP)
      DIMENSION KPOSJ(NPIC)
      DIMENSION IPOSA1(NA*(NORB-NCOR-NA))
      DIMENSION ITEST(NA*(NORB-NCOR-NA))
      DIMENSION IOX(NORB),IGMUL(NSYM,NSYM)
      DIMENSION ISPIN(*),IHMCON(NV)
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
      NALP = IFA(NACT,NA)
      NBLP = IFA(NACT,NB)
      DO 13 II=1,NPIC
          DO 12 JJ=1,NV
             AB(II,JJ) = 0.0D+00
   12     CONTINUE
   13 CONTINUE
C
      DO 30 I=1,NAT
         IACON1(I) = I
   30 CONTINUE
C
C   Big Loop over all alpha determinants in ipica
C
      IZX = 1
      IZY = 1
 9013 CONTINUE
      DO 7010 KK=IZX,IPICA(IZY)-1
         CALL ADVANC(IACON1,NAT,NORB)
 7010 CONTINUE
      DO 7013 IZW=IZY,NPIC
         IF (IPICA(IZY).NE.IPICA(IZW)) GOTO 7015
 7013 CONTINUE
C
 7015 IZW = IZW - 1
C
C  Now, we have indices in ipicaa from izy -> izw
C  and the beta determinants are in increasing
C   order ipicb(izy) -> ipicb(izw) *****
C
C  Alpha excitations here
C   Single first
C
         IAC = 0
         DO 7030 IA=NCOR+1,NAT
             IO1 = IACON1(IA)
             IST = IO1 + 1
             IEN = IACON1(IA+1)-1
CSymmetry of orbital being deoccupied
             IS1 = IOX(IO1)
C
             IF (IA.EQ.NAT) IEN=NORB
             DO 7025 KKJ=IA-NCOR+1,NA+1
                DO 7020 JJ=IST,IEN
CSymmetry of orbitals being occupied
             IS2 = IOX(JJ)
Cis1xis2 = ip1
             IP1 = IGMUL(IS2,IS1)
C
             CALL RET1DET(IACON1,IACON2,NA,IA,JJ,NCOR,KKJ,IPER1)
             IPET = POSDET(NACT,NA,IACON2,IFA)
C
C***
C  Check to see if this excited alpha determinant is in ipica
C
             DO 7018 KK=IZW+1,NPIC
                IF (IPICA(KK).EQ.IPET) GOTO 7021
 7018        CONTINUE
             GOTO 413
 7021        JZY = KK
C
C   Check to see how many of the ipet alphas there are
C
             DO 7040 KK=JZY,NPIC
                IF (IPICA(KK).NE.IPET) GOTO 7034
 7040        CONTINUE
 7034        JZW = KK - 1
C
C   Storage here for later use, well worth it timewise
C
             IAC = IAC + 1
C            iposa(iac) = (ipet-1)*nblp
             IPOSA(IAC) = JZY
             IPOSA1(IAC) = JZW
             IPERA(IAC) = ((-1)**IPER1)
             IND = INDEX(JJ,IO1)
             IIND1(IAC) = IND
             ISYMX(IAC) = IP1
C
C If deoccupied and newly occupied are of different sym skip to doubles
             IF (IS1.NE.IS2) GOTO 413
C
C   Now the excited alphas lie in ipica(jzy - > jzw)
C
C   Now have to see if any of the beta determinants match up.
C
             NPCA = 0
             ISY = JZY
             DO 7051 KK1=IZY,IZW
                DO 7053 KK2 = ISY,JZW
                   IF (IPICB(KK1).EQ.IPICB(KK2)) THEN
                      NPCA = NPCA + 1
                      LPICB1(NPCA) = KK1
                      LPICB2(NPCA) = KK2
                      ISY = KK2+1
                      GOTO 7051
                   ENDIF
 7053           CONTINUE
 7051        CONTINUE
             IF (NPCA.EQ.0) GOTO 413
C
C   Now I know that there are npca determinants which
C   match up and that they are the determinants given
C   by ipica(lpicb1(i)), ipicb(lpicb1(i))
C  and ipica(lpicb2(i)), ipicb(lpicb2(i)),i=1,npca
C  lpicb1(i),lpicb2(i) are actally the positions
C  of the CI coefficients.
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
C   Loop over beta dets, check to see if they equal any
C   of ipicb(lpicb1(i)),i=1,npca
C
             IXYY = 1
C
             DO 415 INB1=1,NBLP
C
                 IF (IXYY.EQ.NPCA+1) GOTO 413
                 IF (INB1.EQ.IPICB(LPICB1(IXYY))) GOTO 7058
                 GOTO 414
 7058            CONTINUE
C
                 ICIT = LPICB1(IXYY)
                 ICI2 = LPICB2(IXYY)
                 IXYY = IXYY + 1
C
C  Now I know the determinant ibcon1 and the positions
C  of the CI coefficients are lpicb1(kk) and
C  lpicb2(kk)
C
C              ici2 = iposa(iac)+inb1
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
  414           CALL ADVANC(IBCON1,NBT,NORB)
  415        CONTINUE
C
  413        CONTINUE
C
C      Double alpha excitations
C
          DO 4015 IAA = IA+1,NAT
             IPA = IAA-NCOR
             IIA = IACON1(IAA)
CSymmetry of orbital being deoccupied
             IS3 = IOX(IIA)
             IF (JJ.GT.IIA) IPA = IPA - 1
             ISTAA = JJ+1
             IENAA = IEN
             DO 4010 KKJAA=KKJ,NA+1
                DO 4005 JJAA=ISTAA,IENAA
CSymmetry of orbital being occupied
             IS4 = IOX(JJAA)
Cip2 = is3xis4
             IP2 = IGMUL(IS3,IS4)
CIf symmetry of alpha '' is not right, skip it.
             IF(IP1.NE.IP2) GOTO 4005
C
             CALL RET1DET(IACON2,IBCON1,NA,IPA,JJAA-NCOR,0,KKJAA,IPER2)
             IPET = POSDET(NACT,NA,IBCON1,IFA)
C
C ****
C Check to see if this doubly excited alpha determinant
C is in ipica
C
             DO 8018 KK=IZW+1,NPIC
                IF (IPICA(KK).EQ.IPET) GOTO 8021
 8018        CONTINUE
             GOTO 4005
 8021        KZY = KK
C
C  Check to see how many of them there are
C
             DO 8040 KK=KZY,NPIC
                IF (IPICA(KK).NE.IPET) GOTO 8034
 8040        CONTINUE
 8034        KZW = KK - 1
C
C   Now the doubly excited alphas lie in ipica(kzy -> kzw)
C
C  Now to see if any of the beta determinants match up
C
             NPCA = 0
             ISY = KZY
             DO 8051 KK1 = IZY,IZW
                     DO 8053 KK2 = ISY,KZW
                  IF (IPICB(KK1).EQ.IPICB(KK2)) THEN
                     NPCA = NPCA + 1
                     LPICB1(NPCA) = KK1
                     LPICB2(NPCA) = KK2
                     ISY = KK2+1
                     GOTO 8051
                  ENDIF
 8053           CONTINUE
 8051       CONTINUE
            IF (NPCA.EQ.0) GOTO 4005
C
C  Now I know that there are npca determinants which
C  match up and that they are the determinants given
C  by ipica(lpicb1(i)), ipicb(lpicb1(i))
C and ipica(lpicb2(i)), ipicb(lpicb2(i)),i=1,npca
C lpicb1(i), lpicb2(i) are actually the positions
C of the CI coefficients.
C
             IPERT = IPER1+IPER2
             IPERT = ((-1)**IPERT)
C
                   IND = INDEX(JJ,IO1)
                   I2 = INDEX(IIA,JJAA)
                   INX = INDEX(I2,IND)
                   II1 = INDEX(JJAA,IO1)
                   II2 = INDEX(IIA,JJ)
                   INX2 = INDEX(II1,II2)
                   C = SI2(INX) - SI2(INX2)
                   T = C*IPERT
C
                DO 786 INB1 = 1,NPCA
                   ICI2 = LPICB1(INB1)
                   ICIT = LPICB2(INB1)
C
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
C  Skip over the rest of the exciteds here.
C
C   Loop over Beta dets now, we have to form the
C   simultaneous alpha and beta single excitations.
C
         DO 40 I=1,NBT
            IBCON1(I) = I
   40    CONTINUE
C
C ***
C  Loop over Beta dets in ipicb(izy -> izw)
C
      NZX = 1
      NZY = IZY
 6613 CONTINUE
      DO 5510 KK=NZX,IPICB(NZY)-1
         CALL ADVANC(IBCON1,NBT,NORB)
 5510 CONTINUE
C
      IF (IPICB(NZY).LT.IPICA(IZY)) GOTO 5555
      QNUM = 1.0D+00
      IF (IPICB(NZY).EQ.IPICA(IZY)) QNUM=2.0D+00
C
          IC1 = NZY
C
C  Find where to start looking for beta excited
C
          IKT = 0
          DO 665 II=1,IAC
             DO 778 KK=IPOSA(II),IPOSA1(II)
                IF (IPICB(KK).GT.IPICB(NZY)) GOTO 449
  778        CONTINUE
             ITEST(II) = 0
             IKT = IKT + 1
             GOTO 665
  449        ITEST(II) = KK
  665     CONTINUE
          IF (IKT.EQ.IAC) GOTO 913
C
C   Beta first *********************** Single excits
C
          DO 900 IB=NCOR+1,NBT
             IBB = IBCON1(IB)
CSymmetry of deoccupied orbital
             IB1 = IOX(IBB)
             IST = IBB+1
             IEN = IBCON1(IB+1)-1
C
          IF (IB.EQ.NBT) IEN = NORB
          DO 895 KKJ=IB-NCOR+1,NB+1
                DO 890 JJ=IST,IEN
CSymmetry of occupied orbital
             IB2 = IOX(JJ)
             IV1 = IGMUL(IB1,IB2)
C
               CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
                   IPOSB = POSDET(NACT,NB,IACON2,IFA)
                   IPERB = ((-1)**IPER1)
                   ZPERB = IPERB/QNUM
                   IOB = INDEX(IBB,JJ)
C
C   Loop over all single alpha excitations stored
C
                DO 1013 IAT=1,IAC
                   IF (ITEST(IAT).EQ.0) GOTO 1013
                   IF (ISYMX(IAT).NE.IV1) GOTO 1013
C
C  See if the alpha excit matches with beta single excit.
C
                   DO 1011 KK=ITEST(IAT),IPOSA1(IAT)
                      IF (IPICB(KK).EQ.IPOSB) GOTO 1007
 1011              CONTINUE
                   GOTO 1013
C
 1007              CONTINUE
                   IND = IIND1(IAT)
                   IX = INDEX(IND,IOB)
                      C = SI2(IX)*ZPERB*IPERA(IAT)
C
                   DO 66 KJ = 1,NV
                        AB(IC1,KJ) = AB(IC1,KJ) + C*CI(KK,KJ)
                      AB(KK,KJ) = AB(KK,KJ) + C*CI(IC1,KJ)
   66              CONTINUE
C
 1013           CONTINUE
C
  890           CONTINUE
                IST = IBCON1(KKJ+NCOR)+1
                IEN = IBCON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NB) IEN=NORB
  895       CONTINUE
  900     CONTINUE
C
  913 CONTINUE
C
C   end of <ab|H|a'b'> elements
C
C  Now for the <a'b|H|ab'>
C
C
          IKT = 0
          DO 765 II=1,IAC
             DO 878 KK=IPOSA1(II),IPOSA(II),-1
                IF (IPICB(KK).LT.IPICB(NZY)) GOTO 749
  878        CONTINUE
             ITEST(II) = 0
             IKT = IKT + 1
             GOTO 765
  749        ITEST(II) = KK
  765     CONTINUE
          IF (IKT.EQ.IAC) GOTO 613
C          do 765 ii=1,iac
C             itest(ii) = iposa(ii)
C  765 continue
C
          DO 7900 IB=NCOR+1,NBT
             IBB = IBCON1(IB)
CSymmetry of deoccupied orbital
             IB1 = IOX(IBB)
             IST = NCOR+1
             IEN = IBCON1(1+NCOR)-1
C
             DO 7895 KKJ=1,IB
                DO 7890 JJ=IST,IEN
CSymmetry of occupied orbital
             IB2 = IOX(JJ)
             IV1 = IGMUL(IB1,IB2)
C
               CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
                   IPOSB = POSDET(NACT,NB,IACON2,IFA)
                   IF (IPOSB.LT.IPICA(IZY)) GOTO 7890
                   IPERB= (-1)**IPER1
                   QNUM=1.0D+00
                   IF (IPOSB.EQ.IPICA(IZY)) QNUM=2.0D+00
                   ZPERB = IPERB/QNUM
                   IOB = INDEX(IBB,JJ)
C
C   Loop over all single alpha excitations stored
C
                DO 6013 IAT=1,IAC
                   IF (ITEST(IAT).EQ.0) GOTO 6013
                   IF (ISYMX(IAT).NE.IV1) GOTO 6013
C
                   IND = IIND1(IAT)
                   IX = INDEX(IND,IOB)
                      C = SI2(IX)*ZPERB*IPERA(IAT)
C
C  See if the alpha excit matches with beta single excit.
C
                   DO 7011 KK=ITEST(IAT),IPOSA(IAT),-1
                      IF (IPICB(KK).EQ.IPOSB) GOTO 7007
 7011              CONTINUE
                   GOTO 6013
C
 7007              CONTINUE
                   DO 766 KJ = 1,NV
                        AB(IC1,KJ) = AB(IC1,KJ) + C*CI(KK,KJ)
                      AB(KK,KJ) = AB(KK,KJ) + C*CI(IC1,KJ)
  766              CONTINUE
C
 6013           CONTINUE
C
 7890           CONTINUE
                IST = IBCON1(KKJ+NCOR)+1
                IEN = IBCON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NB) IEN=NORB
 7895       CONTINUE
 7900     CONTINUE
C
  613 CONTINUE
C
 5555 CONTINUE
      NZX = IPICB(NZY)
      NZY = NZY + 1
      IF (NZY.NE.IZW+1) GOTO 6613
C
C
C  End of the Beta loop for <ab|H|a'b'>, <ab'|H|a'b> elements  ^
C
      IZX = IPICA(IZW)
      IZY = IZW + 1
      IF (IZW.NE.NPIC) GOTO 9013
C
C---      goto 9914
C
C  End of the original alpha loop ^
C---c
C---c   Now for the Beta part
C---c
C---      do 876 jji=1,nbt
C---         ibcon1(jji) = jji
C---  876 continue
C---c
C---c  Big loop over all beta determinants
C---c
C---      izx = 1
C---      izy = 1
C--- 5013 continue
C---      do 3010 kk=izx,jpicb(izy)-1
C---         call advanc(ibcon1,nbt,norb)
C--- 3010 continue
C---      do 3013 izw=izy,npic
C---         if (jpicb(izy).ne.jpicb(izw)) goto 3015
C--- 3013 continue
C--- 3015 izw = izw - 1
C---      ijk = jpicb(izy)
C---c
C---c  Now we have indices in jpicb from izy -> izw
C---c  and the alpha determinants are in increasing
C---c  order jpica(izy) -> jpica(izw) *****
C---c
C---c   Single Beta excitations
C---c
C---      do 6030 ib=ncor+1,nbt
C---         io1 = ibcon1(ib)
C---cSymmetry of orbital being deoccupied
C---         is1 = iox(io1)
C---         ist = io1+1
C---         ien = ibcon1(ib+1)-1
C---         if (ib.eq.nbt) ien=norb
C---         do 6025 kkj=ib-ncor+1,nb+1
C---            do 6020 jj=ist,ien
C---cSymmetry of orbital being occupied
C---         is2 = iox(jj)
C---cis1xis2 = ip1
C---         ip1 = igmul(is2,is1)
C---c
C---            call ret1det(ibcon1,iacon2,nb,ib,jj,ncor,kkj,iper1)
C---            iper = ((-1)**iper1)
C---            ind = index(jj,io1)
C---c   If deoccupied and newly occupied are of different symmetry,
C---c   skip to doubles
C---            if (is1.ne.is2) goto 313
C---            ipb1 = posdet(nact,nb,iacon2,ifa)
C---c
C---c  ***
C---c Check to see if this excited beta determinant is in jpicb
C---c
C---            do 3018 kk=izw+1,npic
C---               if (jpicb(kk).eq.ipb1) goto 3021
C--- 3018       continue
C---            goto 313
C--- 3021       jzy = kk
C---c
C---c  Check to see how many of the ipb1 betas there are
C---c
C---            do 3040 kk=jzy,npic
C---               if (jpicb(kk).ne.ipb1) goto 3034
C--- 3040       continue
C--- 3034       jzw = kk - 1
C---c
C---c  Now the excited betas lie in jpicb(jzy -> jzw)
C---c
C---c  Now to see if any of the alpha determinants match up.
C---c
C---            npca = 0
C---            isy = jzy
C---            do 3051 kk1=izy,izw
C---               do 3053 kk2=isy,jzw
C---                  if (jpica(kk1).eq.jpica(kk2)) then
C---                     npca = npca + 1
C---                     lpicb1(npca) = kk1
C---                     lpicb2(npca) = kk2
C---                     isy = kk2 + 1
C---                     goto 3051
C---                  endif
C--- 3053          continue
C--- 3051       continue
C---            if (npca.eq.0) goto 313
C---c
C---c   Now I know that there are npca determinants that match up
C---c   and that they are the determinants given by
C---c      jpica(lpicb1(i)), jpicb(lpicb1(i))
C---c and  jpica(lpicb2(i)), jpicb(lpicb2(i)),i=1,npca
C---c  kposj(lpicb1(i)) and kposj(lpicb2(i)) are actually the
C---c  position of the CI coefficients.
C---c
C---c
C---            C = si1(ind)
C---c
C---            do 912 ik=1,nbt
C---               if (ik.eq.ib) goto 912
C---               ion = ibcon1(ik)
C---               j1 = index(ion,ion)
C---               jj1 = index(ind,j1)
C---               j1 = index(ion,jj)
C---               j2 = index(ion,io1)
C---               inx = index(j1,j2)
C---               C = C + si2(jj1) - si2(inx)
C---  912       continue
C---c
C---       do 89 i=1,nat
C---          iacon1(i) = i
C---   89 continue
C---c
C---c  Loop over alpha dets, check to see if they equal any
C---c  of jpica(lpicb1(i)),i=1,npca
C---c
C---          ixyy = 1
C---            do 920 ina1 = 1,nalp
C---               if (ixyy.eq.npca+1) goto 313
C---               if (ina1.eq.jpica(lpicb1(ixyy))) goto 3058
C---               goto 914
C--- 3058          continue
C---c
C---               icit = kposj(lpicb1(ixyy))
C---               ici2 = kposj(lpicb2(ixyy))
C---               ixyy = ixyy + 1
C---c
C---            D = 0.0d+00
C---             do 690 ik=1,nat
C---                ion = iacon1(ik)
C---                j1 = index(ion,ion)
C---                jj1 = index(ind,j1)
C---                D = D + si2(jj1)
C---  690        continue
C---c
C---             T = (C+D)*iper
C---             if (abs(T).gt.uaib) uaib = abs(T)
C---             do 87 kj = 1,nv
C---                Ab(icit,kj) = Ab(icit,kj) + T*CI(ici2,kj)
C---                Ab(ici2,kj) = Ab(ici2,kj) + T*CI(icit,kj)
C---   87        continue
C---c
C---  914        call advanc(iacon1,nat,norb)
C---  920    continue
C---c
C---  313 continue
C---c
C---c   Now for Beta double excitations
C---c
C---       do 6015 ibb = ib+1,nbt
C---               istbb = jj+1
C---               ienbb = ien
C---               jb = ibcon1(ibb)
C---cSymmetry of orbital being deoccupied
C---               is3 = iox(jb)
C---               ipb = ibb-ncor
C---               if (jj.gt.jb) ipb = ipb - 1
C---               do 6010 kkjbb = kkj,nb+1
C---                  do 6005 jjbb = istbb,ienbb
C---cSymmetry of orbital being occupied
C---               is4 = iox(jjbb)
C---cip2=is4xis4
C---               ip2 = igmul(is4,is3)
C---cIf symmetry of beta'' is not right, skip it
C---               if (ip1.ne.ip2) goto 6005
C---c
C---          call ret1det(iacon2,iacon1,nb,ipb,jjbb-ncor,0,kkjbb,iper2)
C---          ibp2 = posdet(nact,nb,iacon1,ifa)
C---c
C---c***
C---c Check to see if this double excited beta determinant
C---c is in jpicb
C---c
C---              do 2018 kk=izw+1,npic
C---                 if (jpicb(kk).eq.ibp2) goto 2021
C--- 2018         continue
C---              goto 6005
C--- 2021         kzy = kk
C---c
C---c Check to see how many of them there are
C---c
C---              do 2040 kk=kzy,npic
C---                 if (jpicb(kk).ne.ibp2) goto 2034
C--- 2040         continue
C--- 2034         kzw = kk - 1
C---c
C---c Now the doubly excited betas lie in jpicb(kzy -> kzw)
C---c
C---c   Now to see if any of the alpha determinants match up
C---c
C---              npca = 0
C---              isy = kzy
C---              do 2051 kk1=izy,izw
C---                 do 2053 kk2 = isy,kzw
C---                    if (jpica(kk1).eq.jpica(kk2)) then
C---                       npca = npca + 1
C---                       lpicb1(npca) = kk1
C---                       lpicb2(npca) = kk2
C---                       isy = kk2 + 1
C---                       goto 2051
C---                    endif
C--- 2053            continue
C--- 2051         continue
C---              if (npca.eq.0) goto 6005
C---c
C---c  Now I know that there are npca determinants which
C---c  match up and that they are the determinants given
C---c  by jpica(lpicb1(i)), jpicb(lpicb1(i))
C---c and jpica(lpicb2(i)), jpicb(lpicb2(i)),i=1,npca
C---c kposj(lpicb1(i)), kposj(lpicb2(i)) are actually the
C---c positions of the CI coefficients.
C---c
C---          iper = iper1+iper2
C---          iper = ((-1)**iper)
C---               ind = index(jj,io1)
C---               i2 = index(jb,jjbb)
C---               inx = index(i2,ind)
C---               ii1 = index(jjbb,io1)
C---               ii2 = index(jb,jj)
C---               inx2 = index(ii1,ii2)
C---               C = si2(inx) - si2(inx2)
C---               T = C*iper
C---c
C---             do 686 ina1 = 1,npca
C---             icit = kposj(lpicb1(ina1))
C---             ici2 = kposj(lpicb2(ina1))
C---             do 85 kj = 1,nv
C---                Ab(icit,kj) = Ab(icit,kj) + T*CI(ici2,kj)
C---                Ab(ici2,kj) = Ab(ici2,kj) + T*CI(icit,kj)
C---   85        continue
C---  686       continue
C---
C---
C--- 6005          continue
C---               istbb = ibcon1(kkjbb+ncor)+1
C---               ienbb = ibcon1(ncor+kkjbb+1)-1
C---               if (kkjbb.eq.nb) ienbb=norb
C--- 6010      continue
C--- 6015 continue
C---
C--- 6020       continue
C---            ist = ibcon1(kkj+ncor)+1
C---            ien=ibcon1(ncor+kkj+1)-1
C---            if (kkj.eq.nb) ien=norb
C--- 6025     continue
C--- 6030 continue
C---c
C---      izx = jpicb(izw)
C---      izy = izw + 1
C---      if (izw.ne.npic) goto 5013
C
C  End of the Beta loop ^
C
C--- 9914 continue
      DO 977 II=1,NPIC
C
         IF (IPICA(II).EQ.IPICB(II)) THEN
            DO 9017 KJ=1,NV
            IS = (-1)**ISPIN(IHMCON(KJ))
            AB(II,KJ) = AB(II,KJ) + IS*AB(II,KJ)
 9017       CONTINUE
            GOTO 977
         ENDIF
C
         JJ = KPOSJ(II)
         IF (JJ.LT.II) GOTO 977
         DO 9018 KJ = 1,NV
            IS = (-1)**ISPIN(IHMCON(KJ))
            QT = AB(II,KJ)
            AB(II,KJ) = AB(II,KJ) + IS*AB(JJ,KJ)
            AB(JJ,KJ) = AB(JJ,KJ) + IS*QT
 9018    CONTINUE
C
  977 CONTINUE
C
Cc   Now for the diagonal contributions
C
      DO 119 IJK = 1,NPIC
         DO 118 KJ = 1,NV
            AB(IJK,KJ) = AB(IJK,KJ) + Q(IJK)*CI(IJK,KJ)
  118    CONTINUE
  119 CONTINUE
C
      RETURN
      END
C
C*MODULE ALGNCI  *DECK GRINAB
C     --------------------------------------------------------
      SUBROUTINE GRINAB(SI1,SI2,NORB,NCOR,NA,NB,CI,IACON1,IBCON1,
     *           IACON2,IFA,IPOSA,IPERA,IIND1,ISYMX,
     *     INDEX,AB,NV,Q,
     *           UAIA,UAIB,NALP,NBLP,
     *     NPIC,IPICA,IPICB,LPICB1,LPICB2,
     *     JPICA,JPICB,KPOSJ,IPOSA1,ITEST,IOX,IGMUL,NSYM)
C     --------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
      DIMENSION AB(NPIC,NV)
      DIMENSION SI1(*),SI2(*)
      DIMENSION CI(NPIC,NV),IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION IACON1(NA+NCOR),IBCON1(NA+NCOR)
      DIMENSION IACON2(NA+NCOR),IPERA(NA*(NORB-NCOR-NA))
      DIMENSION IIND1(NA*(NORB-NCOR-NA))
      DIMENSION IPOSA(NA*(NORB-NCOR-NA))
      DIMENSION ISYMX(NA*(NORB-NCOR-NA))
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2)
      DIMENSION Q(NPIC)
      DIMENSION IPICA(NPIC),IPICB(NPIC)
      DIMENSION LPICB1(NALP),LPICB2(NALP)
      DIMENSION JPICA(NPIC),JPICB(NPIC),KPOSJ(NPIC)
      DIMENSION IPOSA1(NA*(NORB-NCOR-NA))
      DIMENSION ITEST(NA*(NORB-NCOR-NA))
      DIMENSION IOX(NORB),IGMUL(NSYM,NSYM)
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
      NALP = IFA(NACT,NA)
      NBLP = IFA(NACT,NB)
      DO 13 II=1,NPIC
          DO 12 JJ=1,NV
             AB(II,JJ) = 0.0D+00
   12     CONTINUE
   13 CONTINUE
C
      DO 30 I=1,NAT
         IACON1(I) = I
   30 CONTINUE
C
C   Big Loop over all alpha determinants in ipica
C
      IZX = 1
      IZY = 1
 9013 CONTINUE
      DO 7010 KK=IZX,IPICA(IZY)-1
         CALL ADVANC(IACON1,NAT,NORB)
 7010 CONTINUE
      DO 7013 IZW=IZY,NPIC
         IF (IPICA(IZY).NE.IPICA(IZW)) GOTO 7015
 7013 CONTINUE
C
 7015 IZW = IZW - 1
C
C  Now, we have indices in ipicaa from izy -> izw
C  and the beta determinants are in increasing
C   order ipicb(izy) -> ipicb(izw) *****
C
C  Alpha excitations here
C   Single first
C
         IAC = 0
         DO 7030 IA=NCOR+1,NAT
             IO1 = IACON1(IA)
             IST = IO1 + 1
             IEN = IACON1(IA+1)-1
CSymmetry of orbital being deoccupied
             IS1 = IOX(IO1)
C
             IF (IA.EQ.NAT) IEN=NORB
             DO 7025 KKJ=IA-NCOR+1,NA+1
                DO 7020 JJ=IST,IEN
CSymmetry of orbitals being occupied
             IS2 = IOX(JJ)
Cis1xis2 = ip1
             IP1 = IGMUL(IS2,IS1)
C
             CALL RET1DET(IACON1,IACON2,NA,IA,JJ,NCOR,KKJ,IPER1)
             IPET = POSDET(NACT,NA,IACON2,IFA)
C
C***
C  Check to see if this excited alpha determinant is in ipica
C
             DO 7018 KK=IZW+1,NPIC
                IF (IPICA(KK).EQ.IPET) GOTO 7021
 7018        CONTINUE
             GOTO 413
 7021        JZY = KK
C
C   Check to see how many of the ipet alphas there are
C
             DO 7040 KK=JZY,NPIC
                IF (IPICA(KK).NE.IPET) GOTO 7034
 7040        CONTINUE
 7034        JZW = KK - 1
C
C   Storage here for later use, well worth it timewise
C
             IAC = IAC + 1
C            iposa(iac) = (ipet-1)*nblp
             IPOSA(IAC) = JZY
             IPOSA1(IAC) = JZW
             IPERA(IAC) = ((-1)**IPER1)
             IND = INDEX(JJ,IO1)
             IIND1(IAC) = IND
             ISYMX(IAC) = IP1
C
C If deoccupied and newly occupied are of different sym skip to doubles
             IF (IS1.NE.IS2) GOTO 413
C
C   Now the excited alphas lie in ipica(jzy - > jzw)
C
C   Now have to see if any of the beta determinants match up.
C
             NPCA = 0
             ISY = JZY
             DO 7051 KK1=IZY,IZW
                DO 7053 KK2 = ISY,JZW
                   IF (IPICB(KK1).EQ.IPICB(KK2)) THEN
                      NPCA = NPCA + 1
                      LPICB1(NPCA) = KK1
                      LPICB2(NPCA) = KK2
                      ISY = KK2+1
                      GOTO 7051
                   ENDIF
 7053           CONTINUE
 7051        CONTINUE
             IF (NPCA.EQ.0) GOTO 413
C
C   Now I know that there are npca determinants which
C   match up and that they are the determinants given
C   by ipica(lpicb1(i)), ipicb(lpicb1(i))
C  and ipica(lpicb2(i)), ipicb(lpicb2(i)),i=1,npca
C  lpicb1(i),lpicb2(i) are actally the positions
C  of the CI coefficients.
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
C   Loop over beta dets, check to see if they equal any
C   of ipicb(lpicb1(i)),i=1,npca
C
             IXYY = 1
C
             DO 415 INB1=1,NBLP
C
                 IF (IXYY.EQ.NPCA+1) GOTO 413
                 IF (INB1.EQ.IPICB(LPICB1(IXYY))) GOTO 7058
                 GOTO 414
 7058            CONTINUE
C
                 ICIT = LPICB1(IXYY)
                 ICI2 = LPICB2(IXYY)
                 IXYY = IXYY + 1
C
C  Now I know the determinant ibcon1 and the positions
C  of the CI coefficients are lpicb1(kk) and
C  lpicb2(kk)
C
C              ici2 = iposa(iac)+inb1
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
  414           CALL ADVANC(IBCON1,NBT,NORB)
  415        CONTINUE
C
  413        CONTINUE
C
C      Double alpha excitations
C
          DO 4015 IAA = IA+1,NAT
             IPA = IAA-NCOR
             IIA = IACON1(IAA)
CSymmetry of orbital being deoccupied
             IS3 = IOX(IIA)
             IF (JJ.GT.IIA) IPA = IPA - 1
             ISTAA = JJ+1
             IENAA = IEN
             DO 4010 KKJAA=KKJ,NA+1
                DO 4005 JJAA=ISTAA,IENAA
CSymmetry of orbital being occupied
             IS4 = IOX(JJAA)
Cip2 = is3xis4
             IP2 = IGMUL(IS3,IS4)
CIf symmetry of alpha '' is not right, skip it.
             IF(IP1.NE.IP2) GOTO 4005
C
             CALL RET1DET(IACON2,IBCON1,NA,IPA,JJAA-NCOR,0,KKJAA,IPER2)
             IPET = POSDET(NACT,NA,IBCON1,IFA)
C
C ****
C Check to see if this doubly excited alpha determinant
C is in ipica
C
             DO 8018 KK=IZW+1,NPIC
                IF (IPICA(KK).EQ.IPET) GOTO 8021
 8018        CONTINUE
             GOTO 4005
 8021        KZY = KK
C
C  Check to see how many of them there are
C
             DO 8040 KK=KZY,NPIC
                IF (IPICA(KK).NE.IPET) GOTO 8034
 8040        CONTINUE
 8034        KZW = KK - 1
C
C   Now the doubly excited alphas lie in ipica(kzy -> kzw)
C
C  Now to see if any of the beta determinants match up
C
             NPCA = 0
             ISY = KZY
             DO 8051 KK1 = IZY,IZW
                     DO 8053 KK2 = ISY,KZW
                  IF (IPICB(KK1).EQ.IPICB(KK2)) THEN
                     NPCA = NPCA + 1
                     LPICB1(NPCA) = KK1
                     LPICB2(NPCA) = KK2
                     ISY = KK2+1
                     GOTO 8051
                  ENDIF
 8053           CONTINUE
 8051       CONTINUE
            IF (NPCA.EQ.0) GOTO 4005
C
C  Now I know that there are npca determinants which
C  match up and that they are the determinants given
C  by ipica(lpicb1(i)), ipicb(lpicb1(i))
C and ipica(lpicb2(i)), ipicb(lpicb2(i)),i=1,npca
C lpicb1(i), lpicb2(i) are actually the positions
C of the CI coefficients.
C
             IPERT = IPER1+IPER2
             IPERT = ((-1)**IPERT)
C
                   IND = INDEX(JJ,IO1)
                   I2 = INDEX(IIA,JJAA)
                   INX = INDEX(I2,IND)
                   II1 = INDEX(JJAA,IO1)
                   II2 = INDEX(IIA,JJ)
                   INX2 = INDEX(II1,II2)
                   C = SI2(INX) - SI2(INX2)
                   T = C*IPERT
C
                DO 786 INB1 = 1,NPCA
                   ICI2 = LPICB1(INB1)
                   ICIT = LPICB2(INB1)
C
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
C   Loop over Beta dets now, we have to form the
C   simultaneous alpha and beta single excitations.
C
         DO 40 I=1,NBT
            IBCON1(I) = I
   40    CONTINUE
C
C ***
C  Loop over Beta dets in ipicb(izy -> izw)
C
      NZX = 1
      NZY = IZY
 6613 CONTINUE
      DO 5510 KK=NZX,IPICB(NZY)-1
         CALL ADVANC(IBCON1,NBT,NORB)
 5510 CONTINUE
          IC1 = NZY
C
C  Find where to start looking for beta excited
C
          IKT = 0
          DO 665 II=1,IAC
             DO 778 KK=IPOSA(II),IPOSA1(II)
                IF (IPICB(KK).GT.IPICB(NZY)) GOTO 449
  778        CONTINUE
             ITEST(II) = 0
             IKT = IKT + 1
             GOTO 665
  449        ITEST(II) = KK
  665     CONTINUE
          IF (IKT.EQ.IAC) GOTO 913
C
C   Beta first *********************** Single excits
C
          DO 900 IB=NCOR+1,NBT
             IBB = IBCON1(IB)
CSymmetry of deoccupied orbital
             IB1 = IOX(IBB)
             IST = IBB+1
             IEN = IBCON1(IB+1)-1
C
          IF (IB.EQ.NBT) IEN = NORB
          DO 895 KKJ=IB-NCOR+1,NB+1
                DO 890 JJ=IST,IEN
CSymmetry of occupied orbital
             IB2 = IOX(JJ)
             IV1 = IGMUL(IB1,IB2)
C
               CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
                   IPOSB = POSDET(NACT,NB,IACON2,IFA)
                   IPERB = ((-1)**IPER1)
                   IOB = INDEX(IBB,JJ)
C
C   Loop over all single alpha excitations stored
C
                DO 1013 IAT=1,IAC
                   IF (ITEST(IAT).EQ.0) GOTO 1013
                   IF (ISYMX(IAT).NE.IV1) GOTO 1013
C
C  See if the alpha excit matches with beta single excit.
C
                   DO 1011 KK=ITEST(IAT),IPOSA1(IAT)
                      IF (IPICB(KK).EQ.IPOSB) GOTO 1007
 1011              CONTINUE
                   GOTO 1013
C
 1007              CONTINUE
C
                   IND = IIND1(IAT)
                   IX = INDEX(IND,IOB)
                      C = SI2(IX)*IPERB*IPERA(IAT)
                   DO 66 KJ = 1,NV
                        AB(IC1,KJ) = AB(IC1,KJ) + C*CI(KK,KJ)
                      AB(KK,KJ) = AB(KK,KJ) + C*CI(IC1,KJ)
   66              CONTINUE
C
 1013           CONTINUE
C
  890           CONTINUE
                IST = IBCON1(KKJ+NCOR)+1
                IEN = IBCON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NB) IEN=NORB
  895       CONTINUE
  900     CONTINUE
C
  913 CONTINUE
C
C   end of <ab|H|a'b'> elements
C
C  Now for the <a'b|H|ab'>
C
          IKT = 0
          DO 765 II=1,IAC
             DO 878 KK=IPOSA1(II),IPOSA(II),-1
                IF (IPICB(KK).LT.IPICB(NZY)) GOTO 749
  878        CONTINUE
             ITEST(II) = 0
             IKT = IKT + 1
             GOTO 765
  749        ITEST(II) = KK
  765     CONTINUE
          IF (IKT.EQ.IAC) GOTO 613
C          do 765 ii=1,iac
C             itest(ii) = iposa(ii)
C  765 continue
C
          DO 7900 IB=NCOR+1,NBT
             IBB = IBCON1(IB)
CSymmetry of deoccupied orbital
             IB1 = IOX(IBB)
             IST = NCOR+1
             IEN = IBCON1(NCOR+1)-1
C
             DO 7895 KKJ=1,IB
                DO 7890 JJ=IST,IEN
CSymmetry of occupied orbital
             IB2 = IOX(JJ)
             IV1 = IGMUL(IB1,IB2)
C
               CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
                   IPOSB = POSDET(NACT,NB,IACON2,IFA)
                   IPERB = ((-1)**IPER1)
                   IOB = INDEX(IBB,JJ)
C
C   Loop over all single alpha excitations stored
C
                DO 6013 IAT=1,IAC
                   IF (ITEST(IAT).EQ.0) GOTO 6013
                   IF (ISYMX(IAT).NE.IV1) GOTO 6013
C
                   IND = IIND1(IAT)
                   IX = INDEX(IND,IOB)
                      C = SI2(IX)*IPERB*IPERA(IAT)
C
C  See if the alpha excit matches with beta single excit.
C
                   DO 7011 KK=ITEST(IAT),IPOSA(IAT),-1
                      IF (IPICB(KK).EQ.IPOSB) GOTO 7007
 7011              CONTINUE
                   GOTO 6013
C
 7007              CONTINUE
                   DO 766 KJ = 1,NV
                        AB(IC1,KJ) = AB(IC1,KJ) + C*CI(KK,KJ)
                      AB(KK,KJ) = AB(KK,KJ) + C*CI(IC1,KJ)
  766              CONTINUE
C
 6013           CONTINUE
C
 7890           CONTINUE
                IST = IBCON1(KKJ+NCOR)+1
                IEN = IBCON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NB) IEN=NORB
 7895       CONTINUE
 7900     CONTINUE
C
  613 CONTINUE
C
      NZX = IPICB(NZY)
      NZY = NZY + 1
      IF (NZY.NE.IZW+1) GOTO 6613
C
C
C  End of the Beta loop for <ab|H|a'b'>, <ab'|H|a'b> elements  ^
C
      IZX = IPICA(IZW)
      IZY = IZW + 1
      IF (IZW.NE.NPIC) GOTO 9013
C
C  End of the original alpha loop ^
C
C   Now for the Beta part
C
      DO 876 JJI=1,NBT
         IBCON1(JJI) = JJI
  876 CONTINUE
C
C  Big loop over all beta determinants
C
      IZX = 1
      IZY = 1
 5013 CONTINUE
      DO 3010 KK=IZX,JPICB(IZY)-1
         CALL ADVANC(IBCON1,NBT,NORB)
 3010 CONTINUE
      DO 3013 IZW=IZY,NPIC
         IF (JPICB(IZY).NE.JPICB(IZW)) GOTO 3015
 3013 CONTINUE
 3015 IZW = IZW - 1
      IJK = JPICB(IZY)
C
C  Now we have indices in jpicb from izy -> izw
C  and the alpha determinants are in increasing
C  order jpica(izy) -> jpica(izw) *****
C
C   Single Beta excitations
C
      DO 6030 IB=NCOR+1,NBT
         IO1 = IBCON1(IB)
CSymmetry of orbital being deoccupied
         IS1 = IOX(IO1)
         IST = IO1+1
         IEN = IBCON1(IB+1)-1
         IF (IB.EQ.NBT) IEN=NORB
         DO 6025 KKJ=IB-NCOR+1,NB+1
            DO 6020 JJ=IST,IEN
CSymmetry of orbital being occupied
         IS2 = IOX(JJ)
Cis1xis2 = ip1
         IP1 = IGMUL(IS2,IS1)
C
            CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
            IPER = ((-1)**IPER1)
            IND = INDEX(JJ,IO1)
C    If deoccupied and newly occupied are of different symmetry,
C    skip to doubles
            IF (IS1.NE.IS2) GOTO 313
            IPB1 = POSDET(NACT,NB,IACON2,IFA)
C
C  ***
C Check to see if this excited beta determinant is in jpicb
C
            DO 3018 KK=IZW+1,NPIC
               IF (JPICB(KK).EQ.IPB1) GOTO 3021
 3018       CONTINUE
            GOTO 313
 3021       JZY = KK
C
C  Check to see how many of the ipb1 betas there are
C
            DO 3040 KK=JZY,NPIC
               IF (JPICB(KK).NE.IPB1) GOTO 3034
 3040       CONTINUE
 3034       JZW = KK - 1
C
C  Now the excited betas lie in jpicb(jzy -> jzw)
C
C  Now to see if any of the alpha determinants match up.
C
            NPCA = 0
            ISY = JZY
            DO 3051 KK1=IZY,IZW
               DO 3053 KK2=ISY,JZW
                  IF (JPICA(KK1).EQ.JPICA(KK2)) THEN
                     NPCA = NPCA + 1
                     LPICB1(NPCA) = KK1
                     LPICB2(NPCA) = KK2
                     ISY = KK2 + 1
                     GOTO 3051
                  ENDIF
 3053          CONTINUE
 3051       CONTINUE
            IF (NPCA.EQ.0) GOTO 313
C
C   Now I know that there are npca determinants that match up
C   and that they are the determinants given by
C      jpica(lpicb1(i)), jpicb(lpicb1(i))
C and  jpica(lpicb2(i)), jpicb(lpicb2(i)),i=1,npca
C  kposj(lpicb1(i)) and kposj(lpicb2(i)) are actually the
C  position of the CI coefficients.
C
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
C  Loop over alpha dets, check to see if they equal any
C  of jpica(lpicb1(i)),i=1,npca
C
          IXYY = 1
            DO 920 INA1 = 1,NALP
               IF (IXYY.EQ.NPCA+1) GOTO 313
               IF (INA1.EQ.JPICA(LPICB1(IXYY))) GOTO 3058
               GOTO 914
 3058          CONTINUE
C
               ICIT = KPOSJ(LPICB1(IXYY))
               ICI2 = KPOSJ(LPICB2(IXYY))
               IXYY = IXYY + 1
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
             IF (ABS(T).GT.UAIB) UAIB = ABS(T)
             DO 87 KJ = 1,NV
                AB(ICIT,KJ) = AB(ICIT,KJ) + T*CI(ICI2,KJ)
                AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICIT,KJ)
   87        CONTINUE
C
  914        CALL ADVANC(IACON1,NAT,NORB)
  920    CONTINUE
C
  313 CONTINUE
C
C   Now for Beta double excitations
C
       DO 6015 IBB = IB+1,NBT
               ISTBB = JJ+1
               IENBB = IEN
               JB = IBCON1(IBB)
CSymmetry of orbital being deoccupied
               IS3 = IOX(JB)
               IPB = IBB-NCOR
               IF (JJ.GT.JB) IPB = IPB - 1
               DO 6010 KKJBB = KKJ,NB+1
                  DO 6005 JJBB = ISTBB,IENBB
CSymmetry of orbital being occupied
               IS4 = IOX(JJBB)
Cip2=is4xis4
               IP2 = IGMUL(IS4,IS3)
CIf symmetry of beta'' is not right, skip it
               IF (IP1.NE.IP2) GOTO 6005
C
          CALL RET1DET(IACON2,IACON1,NB,IPB,JJBB-NCOR,0,KKJBB,IPER2)
          IBP2 = POSDET(NACT,NB,IACON1,IFA)
C
C***
C Check to see if this double excited beta determinant
C is in jpicb
C
              DO 2018 KK=IZW+1,NPIC
                 IF (JPICB(KK).EQ.IBP2) GOTO 2021
 2018         CONTINUE
              GOTO 6005
 2021         KZY = KK
C
C Check to see how many of them there are
C
              DO 2040 KK=KZY,NPIC
                 IF (JPICB(KK).NE.IBP2) GOTO 2034
 2040         CONTINUE
 2034         KZW = KK - 1
C
C Now the doubly excited betas lie in jpicb(kzy -> kzw)
C
C   Now to see if any of the alpha determinants match up
C
              NPCA = 0
              ISY = KZY
              DO 2051 KK1=IZY,IZW
                 DO 2053 KK2 = ISY,KZW
                    IF (JPICA(KK1).EQ.JPICA(KK2)) THEN
                       NPCA = NPCA + 1
                       LPICB1(NPCA) = KK1
                       LPICB2(NPCA) = KK2
                       ISY = KK2 + 1
                       GOTO 2051
                    ENDIF
 2053            CONTINUE
 2051         CONTINUE
              IF (NPCA.EQ.0) GOTO 6005
C
C  Now I know that there are npca determinants which
C  match up and that they are the determinants given
C  by jpica(lpicb1(i)), jpicb(lpicb1(i))
C and jpica(lpicb2(i)), jpicb(lpicb2(i)),i=1,npca
C kposj(lpicb1(i)), kposj(lpicb2(i)) are actually the
C positions of the CI coefficients.
C
          IPER = IPER1+IPER2
          IPER = ((-1)**IPER)
               IND = INDEX(JJ,IO1)
               I2 = INDEX(JB,JJBB)
               INX = INDEX(I2,IND)
               II1 = INDEX(JJBB,IO1)
               II2 = INDEX(JB,JJ)
               INX2 = INDEX(II1,II2)
               C = SI2(INX) - SI2(INX2)
               T = C*IPER
C
             DO 686 INA1 = 1,NPCA
             ICIT = KPOSJ(LPICB1(INA1))
             ICI2 = KPOSJ(LPICB2(INA1))
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
      IZX = JPICB(IZW)
      IZY = IZW + 1
      IF (IZW.NE.NPIC) GOTO 5013
C
C  End of the Beta loop ^
C
C   Now for the diagonal contributions
C
      DO 119 IJK = 1,NPIC
         DO 118 KJ = 1,NV
            AB(IJK,KJ) = AB(IJK,KJ) + Q(IJK)*CI(IJK,KJ)
  118    CONTINUE
  119 CONTINUE
C
      RETURN
      END
C
C*MODULE ALGNCI  *DECK GRETAB0
C     --------------------------------------------------------
      SUBROUTINE GRETAB0(SI1,SI2,NORB,NCOR,NA,NB,CI,IACON1,IBCON1,
     *               IACON2,IFA,IPOSA,IPERA,IIND1,ISYMX,
     *     INDEX,AB,Q,
     *               UAIA,UAIB,NALP,NBLP,
     *     NPIC,IPICA,IPICB,LPICB1,LPICB2,
     *     KPOSJ,IPOSA1,ITEST,IOX,IGMUL,NSYM,
     *    ISPIN,IHMCON)
C     --------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
      DIMENSION AB(NPIC)
      DIMENSION SI1(*),SI2(*)
      DIMENSION CI(NPIC),IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION IACON1(NA+NCOR),IBCON1(NA+NCOR)
      DIMENSION IACON2(NA+NCOR),IPERA(NA*(NORB-NCOR-NA))
      DIMENSION IIND1(NA*(NORB-NCOR-NA))
      DIMENSION IPOSA(NA*(NORB-NCOR-NA))
      DIMENSION ISYMX(NA*(NORB-NCOR-NA))
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2)
      DIMENSION Q(NPIC)
      DIMENSION IPICA(NPIC),IPICB(NPIC)
      DIMENSION LPICB1(NALP),LPICB2(NALP)
      DIMENSION KPOSJ(NPIC)
      DIMENSION IPOSA1(NA*(NORB-NCOR-NA))
      DIMENSION ITEST(NA*(NORB-NCOR-NA))
      DIMENSION IOX(NORB),IGMUL(NSYM,NSYM)
      DIMENSION ISPIN(*),IHMCON(1)
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
      NALP = IFA(NACT,NA)
      NBLP = IFA(NACT,NB)
      DO 13 II=1,NPIC
             AB(II) = 0.0D+00
   13 CONTINUE
C
      DO 30 I=1,NAT
         IACON1(I) = I
   30 CONTINUE
C
C   Big Loop over all alpha determinants in ipica
C
      IZX = 1
      IZY = 1
 9013 CONTINUE
      DO 7010 KK=IZX,IPICA(IZY)-1
         CALL ADVANC(IACON1,NAT,NORB)
 7010 CONTINUE
      DO 7013 IZW=IZY,NPIC
         IF (IPICA(IZY).NE.IPICA(IZW)) GOTO 7015
 7013 CONTINUE
C
 7015 IZW = IZW - 1
C
C  Now, we have indices in ipicaa from izy -> izw
C  and the beta determinants are in increasing
C   order ipicb(izy) -> ipicb(izw) *****
C
C  Alpha excitations here
C   Single first
C
         IVY = IZW+1
         IAC = 0
         DO 7030 IA=NAT,NCOR+1,-1
             IO1 = IACON1(IA)
             IST = IO1 + 1
             IEN = IACON1(IA+1)-1
CSymmetry of orbital being deoccupied
             IS1 = IOX(IO1)
C
             IF (IA.EQ.NAT) IEN=NORB
             DO 7025 KKJ=IA-NCOR+1,NA+1
                DO 7020 JJ=IST,IEN
CSymmetry of orbitals being occupied
             IS2 = IOX(JJ)
Cis1xis2 = ip1
             IP1 = IGMUL(IS2,IS1)
C
             CALL RET1DET(IACON1,IACON2,NA,IA,JJ,NCOR,KKJ,IPER1)
             IPET = POSDET(NACT,NA,IACON2,IFA)
C***
C  Check to see if this excited alpha determinant is in ipica
C
             DO 7018 KK=IVY,NPIC
                IF (IPICA(KK).GE.IPET) GOTO 7021
 7018        CONTINUE
             GOTO 413
C
 7021        JZY = KK
             IVY = KK
             IF (IPICA(KK).NE.IPET) GOTO 413
C
C   Check to see how many of the ipet alphas there are
C
             DO 7040 KK=JZY,NPIC
                IF (IPICA(KK).NE.IPET) GOTO 7034
 7040        CONTINUE
 7034        JZW = KK - 1
             IVY = KK
C
C   Storage here for later use, well worth it timewise
C
             IAC = IAC + 1
C            iposa(iac) = (ipet-1)*nblp
             IPOSA(IAC) = JZY
             IPOSA1(IAC) = JZW
             IPERA(IAC) = ((-1)**IPER1)
             IND = INDEX(JJ,IO1)
             IIND1(IAC) = IND
             ISYMX(IAC) = IP1
C
C If deoccupied and newly occupied are of different sym skip to doubles
             IF (IS1.NE.IS2) GOTO 413
C
C   Now the excited alphas lie in ipica(jzy - > jzw)
C
C   Now have to see if any of the beta determinants match up.
C
             NPCA = 0
             ISY = JZY
             DO 7051 KK1=IZY,IZW
                DO 7053 KK2 = ISY,JZW
                   IF (IPICB(KK2).GE.IPICB(KK1)) THEN
                      ISY = KK2
                      IF (IPICB(KK1).NE.IPICB(KK2)) GOTO 7051
                      NPCA = NPCA + 1
                      ISY = ISY + 1
                      LPICB1(NPCA) = KK1
                      LPICB2(NPCA) = KK2
                      GOTO 7051
                   ENDIF
 7053           CONTINUE
                GOTO 7054
 7051        CONTINUE
 7054        IF (NPCA.EQ.0) GOTO 413
C
C   Now I know that there are npca determinants which
C   match up and that they are the determinants given
C   by ipica(lpicb1(i)), ipicb(lpicb1(i))
C  and ipica(lpicb2(i)), ipicb(lpicb2(i)),i=1,npca
C  lpicb1(i),lpicb2(i) are actally the positions
C  of the CI coefficients.
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
C   Loop over beta dets, check to see if they equal any
C   of ipicb(lpicb1(i)),i=1,npca
C
             IXYY = 1
C
             DO 415 INB1=1,NBLP
C
                 IF (IXYY.EQ.NPCA+1) GOTO 413
                 IF (INB1.EQ.IPICB(LPICB1(IXYY))) GOTO 7058
                 GOTO 414
 7058            CONTINUE
C
                 ICIT = LPICB1(IXYY)
                 ICI2 = LPICB2(IXYY)
                 IXYY = IXYY + 1
C
C  Now I know the determinant ibcon1 and the positions
C  of the CI coefficients are lpicb1(kk) and
C  lpicb2(kk)
C
C              ici2 = iposa(iac)+inb1
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
                   AB(ICIT) = AB(ICIT) + T*CI(ICI2)
                    AB(ICI2) = AB(ICI2) + T*CI(ICIT)
C
  414           CALL ADVANC(IBCON1,NBT,NORB)
  415        CONTINUE
C
  413        CONTINUE
C
C      Double alpha excitations
C
          DO 4015 IAA = NAT,IA+1,-1
             IPA = IAA-NCOR
             IIA = IACON1(IAA)
CSymmetry of orbital being deoccupied
             IS3 = IOX(IIA)
             IF (JJ.GT.IIA) IPA = IPA - 1
             ISTAA = JJ+1
             IENAA = IEN
C
             IVZ = IZW + 1
C
             DO 4010 KKJAA=KKJ,NA+1
                DO 4005 JJAA=ISTAA,IENAA
CSymmetry of orbital being occupied
             IS4 = IOX(JJAA)
Cip2 = is3xis4
             IP2 = IGMUL(IS3,IS4)
CIf symmetry of alpha '' is not right, skip it.
             IF(IP1.NE.IP2) GOTO 4005
C
             CALL RET1DET(IACON2,IBCON1,NA,IPA,JJAA-NCOR,0,KKJAA,IPER2)
             IPET = POSDET(NACT,NA,IBCON1,IFA)
C
C ****
C Check to see if this doubly excited alpha determinant
C is in ipica
C
C             do 8018 kk=izw+1,npic
C
             DO 8018 KK=IVZ,NPIC
                IF (IPICA(KK).GE.IPET) GOTO 8021
 8018        CONTINUE
             GOTO 4015
C
 8021        KZY = KK
             IVZ = KK
             IF (IPICA(KK).NE.IPET) GOTO 4005
C
C  Check to see how many of them there are
C
             DO 8040 KK=KZY,NPIC
                IF (IPICA(KK).NE.IPET) GOTO 8034
 8040        CONTINUE
 8034        KZW = KK - 1
C
C   Now the doubly excited alphas lie in ipica(kzy -> kzw)
C
C  Now to see if any of the beta determinants match up
C
             NPCA = 0
             ISY = KZY
             DO 8051 KK1 = IZY,IZW
                     DO 8053 KK2 = ISY,KZW
                     IF (IPICB(KK2).GE.IPICB(KK1)) THEN
                     ISY = KK2
                     IF (IPICB(KK2).NE.IPICB(KK1)) GOTO 8051
                     NPCA = NPCA + 1
                     LPICB1(NPCA) = KK1
                     LPICB2(NPCA) = KK2
                     ISY = KK2+1
                     GOTO 8051
                  ENDIF
 8053           CONTINUE
                GOTO 8054
 8051       CONTINUE
 8054       IF (NPCA.EQ.0) GOTO 4005
C
C  Now I know that there are npca determinants which
C  match up and that they are the determinants given
C  by ipica(lpicb1(i)), ipicb(lpicb1(i))
C and ipica(lpicb2(i)), ipicb(lpicb2(i)),i=1,npca
C lpicb1(i), lpicb2(i) are actually the positions
C of the CI coefficients.
C
             IPERT = IPER1+IPER2
             IPERT = ((-1)**IPERT)
C
                   IND = INDEX(JJ,IO1)
                   I2 = INDEX(IIA,JJAA)
                   INX = INDEX(I2,IND)
                   II1 = INDEX(JJAA,IO1)
                   II2 = INDEX(IIA,JJ)
                   INX2 = INDEX(II1,II2)
                   C = SI2(INX) - SI2(INX2)
                   T = C*IPERT
C
                DO 786 INB1 = 1,NPCA
                   ICI2 = LPICB1(INB1)
                   ICIT = LPICB2(INB1)
C
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
C  Skip over the rest of the exciteds here.
C
C   Loop over Beta dets now, we have to form the
C   simultaneous alpha and beta single excitations.
C
         DO 40 I=1,NBT
            IBCON1(I) = IACON1(I)
   40    CONTINUE
         DO 42 I=IZY,IZW
            IF (IPICB(I).GE.IPICA(I)) GOTO 43
  42     CONTINUE
         GOTO 9313
  43     NZY = I
C
C ***
C  Loop over Beta dets in ipicb(izy -> izw)
C
      NZX = IPICA(IZY)
 6613 CONTINUE
      DO 5510 KK=NZX,IPICB(NZY)-1
         CALL ADVANC(IBCON1,NBT,NORB)
 5510 CONTINUE
C
C      if (ipicb(nzy).lt.ipica(izy)) goto 5555
      QNUM = 1.0D+00
      IF (IPICB(NZY).EQ.IPICA(IZY)) QNUM=2.0D+00
C
          IC1 = NZY
C
C  Find where to start looking for beta excited
C
          IKT = 0
          DO 665 II=1,IAC
             DO 778 KK=IPOSA(II),IPOSA1(II)
                IF (IPICB(KK).GT.IPICB(NZY)) GOTO 449
  778        CONTINUE
             ITEST(II) = IPOSA1(II)+1
             IKT = IKT + 1
             GOTO 665
  449        ITEST(II) = KK
  665     CONTINUE
          IF (IKT.EQ.IAC) GOTO 913
C
C   Beta first *********************** Single excits
C
          DO 900 IB=NBT,NCOR+1,-1
             IBB = IBCON1(IB)
CSymmetry of deoccupied orbital
             IB1 = IOX(IBB)
             IST = IBB+1
             IEN = IBCON1(IB+1)-1
C
          IF (IB.EQ.NBT) IEN = NORB
          DO 895 KKJ=IB-NCOR+1,NB+1
                DO 890 JJ=IST,IEN
CSymmetry of occupied orbital
             IB2 = IOX(JJ)
             IV1 = IGMUL(IB1,IB2)
C
               CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
                   IPOSB = POSDET(NACT,NB,IACON2,IFA)
                   IPERB = ((-1)**IPER1)
                   ZPERB = IPERB/QNUM
                   IOB = INDEX(IBB,JJ)
                   IKT = 0
C
C   Loop over all single alpha excitations stored
C
                DO 1013 IAT=1,IAC
                   IF (ISYMX(IAT).NE.IV1) GOTO 1013
                     IF (ITEST(IAT).GT.IPOSA1(IAT)) GOTO 1008
C
C  See if the alpha excit matches with beta single excit.
C
                   DO 1011 KK=ITEST(IAT),IPOSA1(IAT)
                      IF (IPICB(KK).GE.IPOSB) GOTO 1007
 1011              CONTINUE
                   ITEST(IAT) = KK
 1008              IKT = IKT + 1
                   GOTO 1013
C
 1007              IF (IPICB(KK).NE.IPOSB) GOTO 1012
C
                   IND = IIND1(IAT)
                   IX = INDEX(IND,IOB)
                      C = SI2(IX)*ZPERB*IPERA(IAT)
C
                        AB(IC1) = AB(IC1) + C*CI(KK)
                      AB(KK) = AB(KK) + C*CI(IC1)
                      KK = KK + 1
C
 1012              ITEST(IAT) = KK
 1013           CONTINUE
                IF (IKT.EQ.IAC) GOTO 913
C
  890           CONTINUE
                IST = IBCON1(KKJ+NCOR)+1
                IEN = IBCON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NB) IEN=NORB
  895       CONTINUE
  900     CONTINUE
C
  913 CONTINUE
C
C   end of <ab|H|a'b'> elements
C
C  Now for the <a'b|H|ab'>
C
C
          IKT = 0
          DO 765 II=1,IAC
             DO 878 KK=IPOSA1(II),IPOSA(II),-1
                IF (IPICB(KK).LT.IPICB(NZY)) GOTO 749
  878        CONTINUE
             ITEST(II) = IPOSA(II)-1
             IKT = IKT + 1
             GOTO 765
  749        ITEST(II) = KK
  765     CONTINUE
          IF (IKT.EQ.IAC) GOTO 5555
C
          DO 7900 KKJ=NBT,NCOR+1,-1
             IST = IBCON1(KKJ-1)+1
             IF (KKJ.EQ.1) IST = 1
             IEN = IBCON1(KKJ)-1
C
             DO 7890 JJ=IEN,IST,-1
                   IB2 = IOX(JJ)
C
                DO 7895 IB=KKJ,NBT
C
CSymmetry of deoccupied orbital
C
                IBB = IBCON1(IB)
                IB1 = IOX(IBB)
C
CSymmetry of occupied orbital
                  IV1 = IGMUL(IB1,IB2)
C
                CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
                   IPOSB = POSDET(NACT,NB,IACON2,IFA)
C
                   IF (IPOSB.LT.IPICA(IZY)) GOTO 5555
                   IPERB= (-1)**IPER1
                   QNUM=1.0D+00
                   IF (IPOSB.EQ.IPICA(IZY)) QNUM=2.0D+00
                   ZPERB = IPERB/QNUM
                   IOB = INDEX(IBB,JJ)
                   IKT = 0
C
C   Loop over all single alpha excitations stored
C
                DO 6013 IAT=1,IAC
                   IF (ISYMX(IAT).NE.IV1) GOTO 6013
                   IF (ITEST(IAT).LT.IPOSA(IAT)) GOTO 7008
C
C  See if the alpha excit matches with beta single excit.
C
C
                   DO 7011 KK=ITEST(IAT),IPOSA(IAT),-1
                      IF (IPICB(KK).LE.IPOSB) GOTO 7007
 7011              CONTINUE
                   ITEST(IAT) = KK
 7008              IKT = IKT + 1
                   GOTO 6013
C
 7007              IF (IPICB(KK).NE.IPOSB) GOTO 7012
C
                   IND = IIND1(IAT)
                   IX = INDEX(IND,IOB)
                      C = SI2(IX)*ZPERB*IPERA(IAT)
C
                        AB(IC1) = AB(IC1) + C*CI(KK)
                      AB(KK) = AB(KK) + C*CI(IC1)
                      KK = KK - 1
C
 7012           ITEST(IAT) = KK
 6013           CONTINUE
                IF (IKT.EQ.IAC) GOTO 5555
C
 7895          CONTINUE
 7890       CONTINUE
 7900     CONTINUE
C
C
 5555 CONTINUE
      NZX = IPICB(NZY)
      NZY = NZY + 1
      IF (NZY.NE.IZW+1) GOTO 6613
C
C
C  End of the Beta loop for <ab|H|a'b'>, <ab'|H|a'b> elements  ^
C
 9313 CONTINUE
C
      IZX = IPICA(IZW)
      IZY = IZW + 1
      IF (IZW.NE.NPIC) GOTO 9013
C
C---      goto 9914
C---c
C---c  End of the original alpha loop ^
C---c
C---c   Now for the Beta part
C---c
C---      do 876 jji=1,nbt
C---         ibcon1(jji) = jji
C---  876 continue
C---c
C---c  Big loop over all beta determinants
C---c
C---      izx = 1
C---      izy = 1
C--- 5013 continue
C---      do 3010 kk=izx,jpicb(izy)-1
C---         call advanc(ibcon1,nbt,norb)
C--- 3010 continue
C---      do 3013 izw=izy,npic
C---         if (jpicb(izy).ne.jpicb(izw)) goto 3015
C--- 3013 continue
C--- 3015 izw = izw - 1
C---      ijk = jpicb(izy)
C---c
C---c  Now we have indices in jpicb from izy -> izw
C---c  and the alpha determinants are in increasing
C---c  order jpica(izy) -> jpica(izw) *****
C---c
C---c   Single Beta excitations
C---c
C---      do 6030 ib=ncor+1,nbt
C---         io1 = ibcon1(ib)
C---cSymmetry of orbital being deoccupied
C---         is1 = iox(io1)
C---         ist = io1+1
C---         ien = ibcon1(ib+1)-1
C---         if (ib.eq.nbt) ien=norb
C---         do 6025 kkj=ib-ncor+1,nb+1
C---            do 6020 jj=ist,ien
C---cSymmetry of orbital being occupied
C---         is2 = iox(jj)
C---cis1xis2 = ip1
C---         ip1 = igmul(is2,is1)
C---c
C---            call ret1det(ibcon1,iacon2,nb,ib,jj,ncor,kkj,iper1)
C---            iper = ((-1)**iper1)
C---            ind = index(jj,io1)
C---c  If deoccupied and newly occupied are of different symmetry,
C---c  skip to doubles
C---            if (is1.ne.is2) goto 313
C---            ipb1 = posdet(nact,nb,iacon2,ifa)
C---c
C---c  ***
C---c Check to see if this excited beta determinant is in jpicb
C---c
C---            do 3018 kk=izw+1,npic
C---               if (jpicb(kk).eq.ipb1) goto 3021
C--- 3018       continue
C---            goto 313
C--- 3021       jzy = kk
C---c
C---c  Check to see how many of the ipb1 betas there are
C---c
C---            do 3040 kk=jzy,npic
C---               if (jpicb(kk).ne.ipb1) goto 3034
C--- 3040       continue
C--- 3034       jzw = kk - 1
C---c
C---c  Now the excited betas lie in jpicb(jzy -> jzw)
C---c
C---c  Now to see if any of the alpha determinants match up.
C---c
C---            npca = 0
C---            isy = jzy
C---            do 3051 kk1=izy,izw
C---               do 3053 kk2=isy,jzw
C---                  if (jpica(kk1).eq.jpica(kk2)) then
C---                     npca = npca + 1
C---                     lpicb1(npca) = kk1
C---                     lpicb2(npca) = kk2
C---                     isy = kk2 + 1
C---                     goto 3051
C---                  endif
C--- 3053          continue
C--- 3051       continue
C---            if (npca.eq.0) goto 313
C---c
C---c   Now I know that there are npca determinants that match up
C---c   and that they are the determinants given by
C---c      jpica(lpicb1(i)), jpicb(lpicb1(i))
C---c and  jpica(lpicb2(i)), jpicb(lpicb2(i)),i=1,npca
C---c  kposj(lpicb1(i)) and kposj(lpicb2(i)) are actually the
C---c  position of the CI coefficients.
C---c
C---c
C---            C = si1(ind)
C---c
C---            do 912 ik=1,nbt
C---               if (ik.eq.ib) goto 912
C---               ion = ibcon1(ik)
C---               j1 = index(ion,ion)
C---               jj1 = index(ind,j1)
C---               j1 = index(ion,jj)
C---               j2 = index(ion,io1)
C---               inx = index(j1,j2)
C---               C = C + si2(jj1) - si2(inx)
C---  912       continue
C---c
C---       do 89 i=1,nat
C---          iacon1(i) = i
C---   89 continue
C---c
C---c  Loop over alpha dets, check to see if they equal any
C---c  of jpica(lpicb1(i)),i=1,npca
C---c
C---          ixyy = 1
C---            do 920 ina1 = 1,nalp
C---               if (ixyy.eq.npca+1) goto 313
C---               if (ina1.eq.jpica(lpicb1(ixyy))) goto 3058
C---               goto 914
C--- 3058          continue
C---c
C---               icit = kposj(lpicb1(ixyy))
C---               ici2 = kposj(lpicb2(ixyy))
C---               ixyy = ixyy + 1
C---c
C---            D = 0.0d+00
C---             do 690 ik=1,nat
C---                ion = iacon1(ik)
C---                j1 = index(ion,ion)
C---                jj1 = index(ind,j1)
C---                D = D + si2(jj1)
C---  690        continue
C---c
C---             T = (C+D)*iper
C---             if (abs(T).gt.uaib) uaib = abs(T)
C---                Ab(icit) = Ab(icit) + T*CI(ici2)
C---                Ab(ici2) = Ab(ici2) + T*CI(icit)
C---c
C---  914        call advanc(iacon1,nat,norb)
C---  920    continue
C---c
C---  313 continue
C---c
C---c   Now for Beta double excitations
C---c
C---       do 6015 ibb = ib+1,nbt
C---               istbb = jj+1
C---               ienbb = ien
C---               jb = ibcon1(ibb)
C---cSymmetry of orbital being deoccupied
C---               is3 = iox(jb)
C---               ipb = ibb-ncor
C---               if (jj.gt.jb) ipb = ipb - 1
C---               do 6010 kkjbb = kkj,nb+1
C---                  do 6005 jjbb = istbb,ienbb
C---cSymmetry of orbital being occupied
C---               is4 = iox(jjbb)
C---cip2=is4xis4
C---               ip2 = igmul(is4,is3)
C---cIf symmetry of beta'' is not right, skip it
C---               if (ip1.ne.ip2) goto 6005
C---c
C---          call ret1det(iacon2,iacon1,nb,ipb,jjbb-ncor,0,kkjbb,iper2)
C---          ibp2 = posdet(nact,nb,iacon1,ifa)
C---c
C---c***
C---c Check to see if this double excited beta determinant
C---c is in jpicb
C---c
C---              do 2018 kk=izw+1,npic
C---                 if (jpicb(kk).eq.ibp2) goto 2021
C--- 2018         continue
C---              goto 6005
C--- 2021         kzy = kk
C---c
C---c Check to see how many of them there are
C---c
C---              do 2040 kk=kzy,npic
C---                 if (jpicb(kk).ne.ibp2) goto 2034
C--- 2040         continue
C--- 2034         kzw = kk - 1
C---c
C---c Now the doubly excited betas lie in jpicb(kzy -> kzw)
C---c
C---c   Now to see if any of the alpha determinants match up
C---c
C---              npca = 0
C---              isy = kzy
C---              do 2051 kk1=izy,izw
C---                 do 2053 kk2 = isy,kzw
C---                    if (jpica(kk1).eq.jpica(kk2)) then
C---                       npca = npca + 1
C---                       lpicb1(npca) = kk1
C---                       lpicb2(npca) = kk2
C---                       isy = kk2 + 1
C---                       goto 2051
C---                    endif
C--- 2053            continue
C--- 2051         continue
C---              if (npca.eq.0) goto 6005
C---c
C---c  Now I know that there are npca determinants which
C---c  match up and that they are the determinants given
C---c  by jpica(lpicb1(i)), jpicb(lpicb1(i))
C---c and jpica(lpicb2(i)), jpicb(lpicb2(i)),i=1,npca
C---c kposj(lpicb1(i)), kposj(lpicb2(i)) are actually the
C---c positions of the CI coefficients.
C---c
C---          iper = iper1+iper2
C---          iper = ((-1)**iper)
C---               ind = index(jj,io1)
C---               i2 = index(jb,jjbb)
C---               inx = index(i2,ind)
C---               ii1 = index(jjbb,io1)
C---               ii2 = index(jb,jj)
C---               inx2 = index(ii1,ii2)
C---               C = si2(inx) - si2(inx2)
C---               T = C*iper
C---c
C---             do 686 ina1 = 1,npca
C---             icit = kposj(lpicb1(ina1))
C---             ici2 = kposj(lpicb2(ina1))
C---                Ab(icit) = Ab(icit) + T*CI(ici2)
C---                Ab(ici2) = Ab(ici2) + T*CI(icit)
C---  686       continue
C---
C---
C--- 6005          continue
C---               istbb = ibcon1(kkjbb+ncor)+1
C---               ienbb = ibcon1(ncor+kkjbb+1)-1
C---               if (kkjbb.eq.nb) ienbb=norb
C--- 6010      continue
C--- 6015 continue
C---
C--- 6020       continue
C---            ist = ibcon1(kkj+ncor)+1
C---            ien=ibcon1(ncor+kkj+1)-1
C---            if (kkj.eq.nb) ien=norb
C--- 6025     continue
C--- 6030 continue
C---c
C---      izx = jpicb(izw)
C---      izy = izw + 1
C---      if (izw.ne.npic) goto 5013
C---c
C---c  End of the Beta loop ^
C---c
C--- 9914 continue
      DO 977 II=1,NPIC
C
         IF (IPICA(II).EQ.IPICB(II)) THEN
            IS = (-1)**ISPIN(IHMCON(1))
            AB(II) = AB(II) + IS*AB(II)
            GOTO 977
         ENDIF
C
         JJ = KPOSJ(II)
         IF (JJ.LT.II) GOTO 977
            IS = (-1)**ISPIN(IHMCON(1))
            QT = AB(II)
            AB(II) = AB(II) + IS*AB(JJ)
            AB(JJ) = AB(JJ) + IS*QT
C
  977 CONTINUE
C
Cc   Now for the diagonal contributions
C
      DO 119 IJK = 1,NPIC
            AB(IJK) = AB(IJK) + Q(IJK)*CI(IJK)
  119 CONTINUE
C
      RETURN
      END
C
C*MODULE ALGNCI  *DECK GRETAB
C     --------------------------------------------------------
      SUBROUTINE GRETAB(SI1,SI2,NORB,NCOR,NA,NB,CI,IACON1,IBCON1,
     *               IACON2,IFA,IPOSA,IPERA,IIND1,ISYMX,
     *     INDEX,AB,Q,
     *               UAIA,UAIB,NALP,NBLP,
     *     NPIC,IPICA,IPICB,LPICB1,LPICB2,
     *     JPICA,JPICB,KPOSJ,IPOSA1,ITEST,IOX,IGMUL,NSYM)
C     --------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
      DIMENSION AB(NPIC)
      DIMENSION SI1(*),SI2(*)
      DIMENSION CI(NPIC),IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION IACON1(NA+NCOR),IBCON1(NA+NCOR)
      DIMENSION IACON2(NA+NCOR),IPERA(NA*(NORB-NCOR-NA))
      DIMENSION IIND1(NA*(NORB-NCOR-NA))
      DIMENSION IPOSA(NA*(NORB-NCOR-NA))
      DIMENSION ISYMX(NA*(NORB-NCOR-NA))
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2)
      DIMENSION Q(NPIC)
      DIMENSION IPICA(NPIC),IPICB(NPIC)
      DIMENSION LPICB1(NALP),LPICB2(NALP)
      DIMENSION JPICA(NPIC),JPICB(NPIC),KPOSJ(NPIC)
      DIMENSION IPOSA1(NA*(NORB-NCOR-NA))
      DIMENSION ITEST(NA*(NORB-NCOR-NA))
      DIMENSION IOX(NORB),IGMUL(NSYM,NSYM)
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
      NALP = IFA(NACT,NA)
      NBLP = IFA(NACT,NB)
      DO 13 II=1,NPIC
             AB(II) = 0.0D+00
   13 CONTINUE
C
      DO 30 I=1,NAT
         IACON1(I) = I
   30 CONTINUE
C
C   Big Loop over all alpha determinants in ipica
C
      IZX = 1
      IZY = 1
 9013 CONTINUE
      DO 7010 KK=IZX,IPICA(IZY)-1
         CALL ADVANC(IACON1,NAT,NORB)
 7010 CONTINUE
      DO 7013 IZW=IZY,NPIC
         IF (IPICA(IZY).NE.IPICA(IZW)) GOTO 7015
 7013 CONTINUE
C
 7015 IZW = IZW - 1
C
C  Now, we have indices in ipicaa from izy -> izw
C  and the beta determinants are in increasing
C   order ipicb(izy) -> ipicb(izw) *****
C
C  Alpha excitations here
C   Single first
C
         IAC = 0
         DO 7030 IA=NCOR+1,NAT
             IO1 = IACON1(IA)
             IST = IO1 + 1
             IEN = IACON1(IA+1)-1
CSymmetry of orbital being deoccupied
             IS1 = IOX(IO1)
C
             IF (IA.EQ.NAT) IEN=NORB
             DO 7025 KKJ=IA-NCOR+1,NA+1
                DO 7020 JJ=IST,IEN
CSymmetry of orbitals being occupied
             IS2 = IOX(JJ)
Cis1xis2 = ip1
             IP1 = IGMUL(IS2,IS1)
C
             CALL RET1DET(IACON1,IACON2,NA,IA,JJ,NCOR,KKJ,IPER1)
             IPET = POSDET(NACT,NA,IACON2,IFA)
C
C***
C  Check to see if this excited alpha determinant is in ipica
C
             DO 7018 KK=IZW+1,NPIC
                IF (IPICA(KK).EQ.IPET) GOTO 7021
 7018        CONTINUE
             GOTO 413
 7021        JZY = KK
C
C   Check to see how many of the ipet alphas there are
C
             DO 7040 KK=JZY,NPIC
                IF (IPICA(KK).NE.IPET) GOTO 7034
 7040        CONTINUE
 7034        JZW = KK - 1
C
C   Storage here for later use, well worth it timewise
C
             IAC = IAC + 1
C            iposa(iac) = (ipet-1)*nblp
             IPOSA(IAC) = JZY
             IPOSA1(IAC) = JZW
             IPERA(IAC) = ((-1)**IPER1)
             IND = INDEX(JJ,IO1)
             IIND1(IAC) = IND
             ISYMX(IAC) = IP1
C
C If deoccupied and newly occupied are of different sym skip to doubles
             IF (IS1.NE.IS2) GOTO 413
C
C   Now the excited alphas lie in ipica(jzy - > jzw)
C
C   Now have to see if any of the beta determinants match up.
C
             NPCA = 0
             ISY = JZY
             DO 7051 KK1=IZY,IZW
                DO 7053 KK2 = ISY,JZW
                   IF (IPICB(KK1).EQ.IPICB(KK2)) THEN
                      NPCA = NPCA + 1
                      LPICB1(NPCA) = KK1
                      LPICB2(NPCA) = KK2
                      ISY = KK2+1
                      GOTO 7051
                   ENDIF
 7053           CONTINUE
 7051        CONTINUE
             IF (NPCA.EQ.0) GOTO 413
C
C   Now I know that there are npca determinants which
C   match up and that they are the determinants given
C   by ipica(lpicb1(i)), ipicb(lpicb1(i))
C  and ipica(lpicb2(i)), ipicb(lpicb2(i)),i=1,npca
C  lpicb1(i),lpicb2(i) are actally the positions
C  of the CI coefficients.
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
C   Loop over beta dets, check to see if they equal any
C   of ipicb(lpicb1(i)),i=1,npca
C
             IXYY = 1
C
             DO 415 INB1=1,NBLP
C
                 IF (IXYY.EQ.NPCA+1) GOTO 413
                 IF (INB1.EQ.IPICB(LPICB1(IXYY))) GOTO 7058
                 GOTO 414
 7058            CONTINUE
C
                 ICIT = LPICB1(IXYY)
                 ICI2 = LPICB2(IXYY)
                 IXYY = IXYY + 1
C
C  Now I know the determinant ibcon1 and the positions
C  of the CI coefficients are lpicb1(kk) and
C  lpicb2(kk)
C
C              ici2 = iposa(iac)+inb1
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
                   AB(ICIT) = AB(ICIT) + T*CI(ICI2)
                    AB(ICI2) = AB(ICI2) + T*CI(ICIT)
C
  414           CALL ADVANC(IBCON1,NBT,NORB)
  415        CONTINUE
C
  413        CONTINUE
C
C      Double alpha excitations
C
          DO 4015 IAA = IA+1,NAT
             IPA = IAA-NCOR
             IIA = IACON1(IAA)
CSymmetry of orbital being deoccupied
             IS3 = IOX(IIA)
             IF (JJ.GT.IIA) IPA = IPA - 1
             ISTAA = JJ+1
             IENAA = IEN
             DO 4010 KKJAA=KKJ,NA+1
                DO 4005 JJAA=ISTAA,IENAA
CSymmetry of orbital being occupied
             IS4 = IOX(JJAA)
Cip2 = is3xis4
             IP2 = IGMUL(IS3,IS4)
CIf symmetry of alpha '' is not right, skip it.
             IF(IP1.NE.IP2) GOTO 4005
C
             CALL RET1DET(IACON2,IBCON1,NA,IPA,JJAA-NCOR,0,KKJAA,IPER2)
             IPET = POSDET(NACT,NA,IBCON1,IFA)
C
C ****
C Check to see if this doubly excited alpha determinant
C is in ipica
C
             DO 8018 KK=IZW+1,NPIC
                IF (IPICA(KK).EQ.IPET) GOTO 8021
 8018        CONTINUE
             GOTO 4005
 8021        KZY = KK
C
C  Check to see how many of them there are
C
             DO 8040 KK=KZY,NPIC
                IF (IPICA(KK).NE.IPET) GOTO 8034
 8040        CONTINUE
 8034        KZW = KK - 1
C
C   Now the doubly excited alphas lie in ipica(kzy -> kzw)
C
C  Now to see if any of the beta determinants match up
C
             NPCA = 0
             ISY = KZY
             DO 8051 KK1 = IZY,IZW
                     DO 8053 KK2 = ISY,KZW
                  IF (IPICB(KK1).EQ.IPICB(KK2)) THEN
                     NPCA = NPCA + 1
                     LPICB1(NPCA) = KK1
                     LPICB2(NPCA) = KK2
                     ISY = KK2+1
                     GOTO 8051
                  ENDIF
 8053           CONTINUE
 8051       CONTINUE
            IF (NPCA.EQ.0) GOTO 4005
C
C  Now I know that there are npca determinants which
C  match up and that they are the determinants given
C  by ipica(lpicb1(i)), ipicb(lpicb1(i))
C and ipica(lpicb2(i)), ipicb(lpicb2(i)),i=1,npca
C lpicb1(i), lpicb2(i) are actually the positions
C of the CI coefficients.
C
             IPERT = IPER1+IPER2
             IPERT = ((-1)**IPERT)
C
                   IND = INDEX(JJ,IO1)
                   I2 = INDEX(IIA,JJAA)
                   INX = INDEX(I2,IND)
                   II1 = INDEX(JJAA,IO1)
                   II2 = INDEX(IIA,JJ)
                   INX2 = INDEX(II1,II2)
                   C = SI2(INX) - SI2(INX2)
                   T = C*IPERT
C
                DO 786 INB1 = 1,NPCA
                   ICI2 = LPICB1(INB1)
                   ICIT = LPICB2(INB1)
C
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
C   Loop over Beta dets now, we have to form the
C   simultaneous alpha and beta single excitations.
C
         DO 40 I=1,NBT
            IBCON1(I) = I
   40    CONTINUE
C
C ***
C  Loop over Beta dets in ipicb(izy -> izw)
C
      NZX = 1
      NZY = IZY
 6613 CONTINUE
      DO 5510 KK=NZX,IPICB(NZY)-1
         CALL ADVANC(IBCON1,NBT,NORB)
 5510 CONTINUE
          IC1 = NZY
C
C  Find where to start looking for beta excited
C
          IKT = 0
          DO 665 II=1,IAC
             DO 778 KK=IPOSA(II),IPOSA1(II)
                IF (IPICB(KK).GT.IPICB(NZY)) GOTO 449
  778        CONTINUE
             ITEST(II) = 0
             IKT = IKT + 1
             GOTO 665
  449        ITEST(II) = KK
  665     CONTINUE
          IF (IKT.EQ.IAC) GOTO 913
C
C   Beta first *********************** Single excits
C
          DO 900 IB=NCOR+1,NBT
             IBB = IBCON1(IB)
CSymmetry of deoccupied orbital
             IB1 = IOX(IBB)
             IST = IBB+1
             IEN = IBCON1(IB+1)-1
C
          IF (IB.EQ.NBT) IEN = NORB
          DO 895 KKJ=IB-NCOR+1,NB+1
                DO 890 JJ=IST,IEN
CSymmetry of occupied orbital
             IB2 = IOX(JJ)
             IV1 = IGMUL(IB1,IB2)
C
               CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
                   IPOSB = POSDET(NACT,NB,IACON2,IFA)
                   IPERB = ((-1)**IPER1)
                   IOB = INDEX(IBB,JJ)
C
C   Loop over all single alpha excitations stored
C
                DO 1013 IAT=1,IAC
                   IF (ITEST(IAT).EQ.0) GOTO 1013
                   IF (ISYMX(IAT).NE.IV1) GOTO 1013
C
C  See if the alpha excit matches with beta single excit.
C
                   DO 1011 KK=ITEST(IAT),IPOSA1(IAT)
                      IF (IPICB(KK).EQ.IPOSB) GOTO 1007
 1011              CONTINUE
                   GOTO 1013
C
 1007              CONTINUE
C
                   IND = IIND1(IAT)
                   IX = INDEX(IND,IOB)
                      C = SI2(IX)*IPERB*IPERA(IAT)
                        AB(IC1) = AB(IC1) + C*CI(KK)
                      AB(KK) = AB(KK) + C*CI(IC1)
C
 1013           CONTINUE
C
  890           CONTINUE
                IST = IBCON1(KKJ+NCOR)+1
                IEN = IBCON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NB) IEN=NORB
  895       CONTINUE
  900     CONTINUE
C
  913 CONTINUE
C
C   end of <ab|H|a'b'> elements
C
C  Now for the <a'b|H|ab'>
C
          IKT = 0
          DO 765 II=1,IAC
             DO 878 KK=IPOSA1(II),IPOSA(II),-1
                IF (IPICB(KK).LT.IPICB(NZY)) GOTO 749
  878        CONTINUE
             ITEST(II) = 0
             IKT = IKT + 1
             GOTO 765
  749        ITEST(II) = KK
  765     CONTINUE
          IF (IKT.EQ.IAC) GOTO 613
C          do 765 ii=1,iac
C             itest(ii) = iposa(ii)
C  765 continue
C
          DO 7900 IB=NCOR+1,NBT
             IBB = IBCON1(IB)
CSymmetry of deoccupied orbital
             IB1 = IOX(IBB)
             IST = NCOR+1
             IEN = IBCON1(NCOR+1)-1
C
             DO 7895 KKJ=1,IB
                DO 7890 JJ=IST,IEN
CSymmetry of occupied orbital
             IB2 = IOX(JJ)
             IV1 = IGMUL(IB1,IB2)
C
               CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
                   IPOSB = POSDET(NACT,NB,IACON2,IFA)
                   IPERB = ((-1)**IPER1)
                   IOB = INDEX(IBB,JJ)
C
C   Loop over all single alpha excitations stored
C
                DO 6013 IAT=1,IAC
                   IF (ITEST(IAT).EQ.0) GOTO 6013
                   IF (ISYMX(IAT).NE.IV1) GOTO 6013
C
                   IND = IIND1(IAT)
                   IX = INDEX(IND,IOB)
                      C = SI2(IX)*IPERB*IPERA(IAT)
C
C  See if the alpha excit matches with beta single excit.
C
                   DO 7011 KK=ITEST(IAT),IPOSA(IAT),-1
                      IF (IPICB(KK).EQ.IPOSB) GOTO 7007
 7011              CONTINUE
                   GOTO 6013
C
 7007              CONTINUE
                        AB(IC1) = AB(IC1) + C*CI(KK)
                      AB(KK) = AB(KK) + C*CI(IC1)
C
 6013           CONTINUE
C
 7890           CONTINUE
                IST = IBCON1(KKJ+NCOR)+1
                IEN = IBCON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NB) IEN=NORB
 7895       CONTINUE
 7900     CONTINUE
C
  613 CONTINUE
C
      NZX = IPICB(NZY)
      NZY = NZY + 1
      IF (NZY.NE.IZW+1) GOTO 6613
C
C
C  End of the Beta loop for <ab|H|a'b'>, <ab'|H|a'b> elements  ^
C
      IZX = IPICA(IZW)
      IZY = IZW + 1
      IF (IZW.NE.NPIC) GOTO 9013
C
C  End of the original alpha loop ^
C
C   Now for the Beta part
C
      DO 876 JJI=1,NBT
         IBCON1(JJI) = JJI
  876 CONTINUE
C
C  Big loop over all beta determinants
C
      IZX = 1
      IZY = 1
 5013 CONTINUE
      DO 3010 KK=IZX,JPICB(IZY)-1
         CALL ADVANC(IBCON1,NBT,NORB)
 3010 CONTINUE
      DO 3013 IZW=IZY,NPIC
         IF (JPICB(IZY).NE.JPICB(IZW)) GOTO 3015
 3013 CONTINUE
 3015 IZW = IZW - 1
      IJK = JPICB(IZY)
C
C  Now we have indices in jpicb from izy -> izw
C  and the alpha determinants are in increasing
C  order jpica(izy) -> jpica(izw) *****
C
C   Single Beta excitations
C
      DO 6030 IB=NCOR+1,NBT
         IO1 = IBCON1(IB)
CSymmetry of orbital being deoccupied
         IS1 = IOX(IO1)
         IST = IO1+1
         IEN = IBCON1(IB+1)-1
         IF (IB.EQ.NBT) IEN=NORB
         DO 6025 KKJ=IB-NCOR+1,NB+1
            DO 6020 JJ=IST,IEN
CSymmetry of orbital being occupied
         IS2 = IOX(JJ)
Cis1xis2 = ip1
         IP1 = IGMUL(IS2,IS1)
C
            CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
            IPER = ((-1)**IPER1)
            IND = INDEX(JJ,IO1)
C    If deoccupied and newly occupied are of different symmetry,
C    skip to doubles
            IF (IS1.NE.IS2) GOTO 313
            IPB1 = POSDET(NACT,NB,IACON2,IFA)
C
C  ***
C Check to see if this excited beta determinant is in jpicb
C
            DO 3018 KK=IZW+1,NPIC
               IF (JPICB(KK).EQ.IPB1) GOTO 3021
 3018       CONTINUE
            GOTO 313
 3021       JZY = KK
C
C  Check to see how many of the ipb1 betas there are
C
            DO 3040 KK=JZY,NPIC
               IF (JPICB(KK).NE.IPB1) GOTO 3034
 3040       CONTINUE
 3034       JZW = KK - 1
C
C  Now the excited betas lie in jpicb(jzy -> jzw)
C
C  Now to see if any of the alpha determinants match up.
C
            NPCA = 0
            ISY = JZY
            DO 3051 KK1=IZY,IZW
               DO 3053 KK2=ISY,JZW
                  IF (JPICA(KK1).EQ.JPICA(KK2)) THEN
                     NPCA = NPCA + 1
                     LPICB1(NPCA) = KK1
                     LPICB2(NPCA) = KK2
                     ISY = KK2 + 1
                     GOTO 3051
                  ENDIF
 3053          CONTINUE
 3051       CONTINUE
            IF (NPCA.EQ.0) GOTO 313
C
C   Now I know that there are npca determinants that match up
C   and that they are the determinants given by
C      jpica(lpicb1(i)), jpicb(lpicb1(i))
C and  jpica(lpicb2(i)), jpicb(lpicb2(i)),i=1,npca
C  kposj(lpicb1(i)) and kposj(lpicb2(i)) are actually the
C  position of the CI coefficients.
C
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
C  Loop over alpha dets, check to see if they equal any
C  of jpica(lpicb1(i)),i=1,npca
C
          IXYY = 1
            DO 920 INA1 = 1,NALP
               IF (IXYY.EQ.NPCA+1) GOTO 313
               IF (INA1.EQ.JPICA(LPICB1(IXYY))) GOTO 3058
               GOTO 914
 3058          CONTINUE
C
               ICIT = KPOSJ(LPICB1(IXYY))
               ICI2 = KPOSJ(LPICB2(IXYY))
               IXYY = IXYY + 1
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
             IF (ABS(T).GT.UAIB) UAIB = ABS(T)
                AB(ICIT) = AB(ICIT) + T*CI(ICI2)
                AB(ICI2) = AB(ICI2) + T*CI(ICIT)
C
  914        CALL ADVANC(IACON1,NAT,NORB)
  920    CONTINUE
C
  313 CONTINUE
C
C   Now for Beta double excitations
C
       DO 6015 IBB = IB+1,NBT
               ISTBB = JJ+1
               IENBB = IEN
               JB = IBCON1(IBB)
CSymmetry of orbital being deoccupied
               IS3 = IOX(JB)
               IPB = IBB-NCOR
               IF (JJ.GT.JB) IPB = IPB - 1
               DO 6010 KKJBB = KKJ,NB+1
                  DO 6005 JJBB = ISTBB,IENBB
CSymmetry of orbital being occupied
               IS4 = IOX(JJBB)
Cip2=is4xis4
               IP2 = IGMUL(IS4,IS3)
CIf symmetry of beta'' is not right, skip it
               IF (IP1.NE.IP2) GOTO 6005
C
          CALL RET1DET(IACON2,IACON1,NB,IPB,JJBB-NCOR,0,KKJBB,IPER2)
          IBP2 = POSDET(NACT,NB,IACON1,IFA)
C
C***
C Check to see if this double excited beta determinant
C is in jpicb
C
              DO 2018 KK=IZW+1,NPIC
                 IF (JPICB(KK).EQ.IBP2) GOTO 2021
 2018         CONTINUE
              GOTO 6005
 2021         KZY = KK
C
C Check to see how many of them there are
C
              DO 2040 KK=KZY,NPIC
                 IF (JPICB(KK).NE.IBP2) GOTO 2034
 2040         CONTINUE
 2034         KZW = KK - 1
C
C Now the doubly excited betas lie in jpicb(kzy -> kzw)
C
C   Now to see if any of the alpha determinants match up
C
              NPCA = 0
              ISY = KZY
              DO 2051 KK1=IZY,IZW
                 DO 2053 KK2 = ISY,KZW
                    IF (JPICA(KK1).EQ.JPICA(KK2)) THEN
                       NPCA = NPCA + 1
                       LPICB1(NPCA) = KK1
                       LPICB2(NPCA) = KK2
                       ISY = KK2 + 1
                       GOTO 2051
                    ENDIF
 2053            CONTINUE
 2051         CONTINUE
              IF (NPCA.EQ.0) GOTO 6005
C
C  Now I know that there are npca determinants which
C  match up and that they are the determinants given
C  by jpica(lpicb1(i)), jpicb(lpicb1(i))
C and jpica(lpicb2(i)), jpicb(lpicb2(i)),i=1,npca
C kposj(lpicb1(i)), kposj(lpicb2(i)) are actually the
C positions of the CI coefficients.
C
          IPER = IPER1+IPER2
          IPER = ((-1)**IPER)
               IND = INDEX(JJ,IO1)
               I2 = INDEX(JB,JJBB)
               INX = INDEX(I2,IND)
               II1 = INDEX(JJBB,IO1)
               II2 = INDEX(JB,JJ)
               INX2 = INDEX(II1,II2)
               C = SI2(INX) - SI2(INX2)
               T = C*IPER
C
             DO 686 INA1 = 1,NPCA
             ICIT = KPOSJ(LPICB1(INA1))
             ICI2 = KPOSJ(LPICB2(INA1))
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
      IZX = JPICB(IZW)
      IZY = IZW + 1
      IF (IZW.NE.NPIC) GOTO 5013
C
C  End of the Beta loop ^
C
C   Now for the diagonal contributions
C
      DO 119 IJK = 1,NPIC
            AB(IJK) = AB(IJK) + Q(IJK)*CI(IJK)
  119 CONTINUE
C
      RETURN
      END
C
C*MODULE ALGNCI  *DECK GSPINDET
C     ----------------------------------------------------------
      SUBROUTINE GSPINDET(IW,NA,NB,NACT,IACON1,IACON2,IBCON1,IBCON2,
     *                   ISD,IDO,CI,AB,IFA,NV,
     *                   SPIN,
     *      NPIC,IPICA,IPICB,NBI,JPICA,JPICB,KPOSJ)
C     ----------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
      DIMENSION CI(NPIC,NV),AB(NPIC,NV)
      DIMENSION IACON1(NA),IBCON1(NB)
      DIMENSION IACON2(NA),IBCON2(NB)
      DIMENSION ISD(NA+NB),IDO(NA)
      DIMENSION IFA(0:NACT,0:NACT)
      DIMENSION SPIN(NV)
      DIMENSION IPICA(NPIC),IPICB(NPIC)
      DIMENSION JPICA(*),JPICB(*),KPOSJ(*)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C     Returns spin of CI vector i in spin(i) for General
C     determinantal wavefunction.  Must compile with routine
C     posdet which is located in grinAb.f
C
C     na,nb   Number of active alpha and beta electrons
C     nact    Number of active orbitals
C     iacon1,iacon2,ibcon1,ibcon2,isa,isb,isd scratch integer
C             arrays
C     CI      CI vectors
C     Ab    Scratch matrix
C     ifa     Contains binomial coefficients, must have been
C             worked out previously using binom6.f
C     nv,npic  Number of vectors and size of CI respectively
C     spin(i) will contain the spin of vector i
C     ipica,ipicb contains the list of determinants in the
C       general wavefunction, see documentation.
C     nbi is the number of highest CI coefficients to take.
C     Note that the nbi determinants with the highest CI
C     coefficients are taken along with all their "buddies".
C     jpica,jpicb,kposj are scratch arrays.  They can be
C     set intially to be of size nblp(number of Beta determinants
C     in the full space)
C
      DO 888 II=1,NV
         SPIN(II) = 0.0D+00
  888 CONTINUE
C
      DO 43 II=1,NV
         DO 49 JJ=1,NPIC
            AB(JJ,II) = CI(JJ,II)
   49    CONTINUE
   43 CONTINUE
C
C  Loop over all states
C
      DO 9000 JJJ=1,NV
         ZNORM = 0.0D+00
C
C  Loop over nbi
C
         DO 8000 III=1,NBI
C
C  Find largest CI coefficient
C
            INDX = 1
            PMAX = 0.0D+00
            DO 123 II=1,NPIC
               IF (ABS(AB(II,JJJ)).GT.PMAX) THEN
                  PMAX = ABS(AB(II,JJJ))
                  INDX = II
               ENDIF
  123       CONTINUE
            AB(INDX,JJJ) = 0.0D+00
C
C   Make the determinant
C
         DO 44 II=1,NA
            IACON1(II) = II
   44    CONTINUE
         DO 46 II=1,NB
            IBCON1(II) = II
   46    CONTINUE
         DO 48 II=1,IPICA(INDX)-1
            CALL ADVANC(IACON1,NA,NACT)
   48    CONTINUE
         DO 51 II=1,IPICB(INDX)-1
            CALL ADVANC(IBCON1,NB,NACT)
   51    CONTINUE
C
C  Have to find ALL determinants having same space function.
C  Find doubly occupied orbitals.
C
            NSS = 0
            NSD = 0
            DO 30 II=1,NA
               IAO = IACON1(II)
               DO 20 JJ=1,NB
                  IF (IAO.EQ.IBCON1(JJ)) GOTO 60
   20          CONTINUE
               GOTO 30
   60          NSD = NSD + 1
               ISD(NSD) = IAO
   30       CONTINUE
C
C  nsd is how many doubly occupied orbitals there are.
C  isd contains a list of the doubly occuppied orbitals
C
C      Find singly occupied orbs now, put in end of isd, beta first
C      then alpha.
C
            DO 320 II=1,NB
                  IB = IBCON1(II)
               DO 324 JJ=1,NSD
                  IF (IB.EQ.ISD(JJ)) GOTO 320
  324          CONTINUE
               NSS = NSS + 1
               ISD(NSS+NSD) = IB
  320       CONTINUE
            NSPB = NSS
C
             DO 330 II=1,NA
                 IA = IACON1(II)
                 DO 334 JJ=1,NSD
                  IF (IA.EQ.ISD(JJ)) GOTO 330
  334          CONTINUE
                NSS = NSS + 1
                 ISD(NSS+NSD) = IA
  330       CONTINUE
            NSPA = NSS - NSPB
C
C       Reorder the things.
C
            DO 340 II=1,NSS-1
                 DO 342 JJ=II+1,NSS
               IF (ISD(JJ+NSD).LT.ISD(II+NSD)) THEN
                  KK=ISD(II+NSD)
                  ISD(II+NSD) = ISD(JJ+NSD)
                  ISD(JJ+NSD) = KK
               ENDIF
  342          CONTINUE
  340       CONTINUE
C
C
C     Find out how many there are
C
           NODE = IFA(NSS,NSPA)
C
C     Now to store positions of all possible determinants with
C     same space function.  Alpha first.
C
      DO 88 II=1,NSPA
         IDO(II) = II
   88 CONTINUE
C
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
         JPICA(IJK) = POSDET(NACT,NA,IACON1,IFA)
         CALL ADVANC(IDO,NSPA,NSS)
 3000 CONTINUE
C
      DO 76 II=1,NSPB
         IDO(II) = II
   76 CONTINUE
      DO 4000 IJK = 1,NODE
         DO 390 II=1,NSD
              IBCON1(II) = ISD(II)
  390    CONTINUE
         DO 205 II=1,NSPB
            IBCON1(II+NSD) = ISD(NSD+IDO(II))
  205    CONTINUE
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
  242       CONTINUE
  240    CONTINUE
C
         JPICB(NODE-IJK+1) = POSDET(NACT,NB,IBCON1,IFA)
         CALL ADVANC(IDO,NSPB,NSS)
 4000 CONTINUE
C
C  Now have the alpha and beta parts stored in jpica and jpicb
C  of all the friends, now to find where in the list they occur
C
      DO 566 KKL=1,NODE
         DO 456 II=1,NPIC
            IF (IPICA(II).EQ.JPICA(KKL)) GOTO 466
  456    CONTINUE
         IF(MASWRK) WRITE(IW,*) 'SOMETHING IS WRONG WITH THE LIST'
         CALL ABRT
  466    CONTINUE
         DO 577 JJ=II,NPIC
            IF (IPICB(JJ).EQ.JPICB(KKL)) GOTO 588
  577    CONTINUE
         IF(MASWRK) WRITE(IW,*) 'SOMETHING IS WRONG WITH THE LIST'
  588    CONTINUE
         KPOSJ(KKL) = JJ
         AB(JJ,JJJ) = 0.0D+00
  566 CONTINUE
C
C  Now to generate the determinants and work out
C  each contribution to the spin
C
         DO 788 IJK=1,NODE
            ZNORM = ZNORM + CI(KPOSJ(IJK),JJJ)**2.0D+00
            DO 67 II=1,NA
               IACON1(II) = II
   67       CONTINUE
            DO 77 II=1,NB
               IBCON1(II) = II
   77       CONTINUE
            DO 79 II=1,IPICA(KPOSJ(IJK))-1
               CALL ADVANC(IACON1,NA,NACT)
   79       CONTINUE
            DO 81 II=1,IPICB(KPOSJ(IJK))-1
               CALL ADVANC(IBCON1,NB,NACT)
   81       CONTINUE
C
C  Have to find its friends and their positions.
C  First find how many doubly occupied orbitals there are.
C
            IXA = 0
            NSD = 0
            DO 139 II=1,NA
               IAO = IACON1(II)
               DO 129 JJ=1,NB
                  IF (IAO.EQ.IBCON1(JJ)) GOTO 160
  129          CONTINUE
               IXA = IXA + 1
               ISD(IXA) = II
               GOTO 139
  160          NSD = NSD + 1
               IDO(NSD) = IAO
  139       CONTINUE
C
C  ixa is how many single alpha orbitals there are.
C  nsd is how many doubly occupied orbitals there are.
C  ido contains a list of the doubly occuppied orbitals
C  isd(i),i=1,ixa contains a list of the single alpha orbs.
C
            IDB = 0
            IXB = NB-NSD
            DO 149 II=1,NB
               IBO = IBCON1(II)
               DO 150 JJ=1,NSD
                  IF (IBO.EQ.IDO(JJ)) GOTO 149
  150          CONTINUE
               IDB = IDB + 1
               ISD(IXA+IDB) = II
               IF (IDB.EQ.IXB) GOTO 190
  149       CONTINUE
  190       CONTINUE
C
C  ixb is how many single beta orbitals there are.
C  isd(i),i=ixa+1,ixa+ixb contains a list of the single beta orbs.
C
            DO 1100 ILK=1,IXA
               KA = ISD(ILK)
               IA1 = IACON1(KA)
               DO 1200 KLI=1,IXB
                  KB = ISD(KLI+IXA)
                  IB1 = IBCON1(KB)
                    DO 1110 IJ=1,NA
                     IACON2(IJ) = IACON1(IJ)
 1110             CONTINUE
                  DO 1120 IJ=1,NB
                     IBCON2(IJ) = IBCON1(IJ)
 1120             CONTINUE
                  IACON2(KA) = IB1
                  IBCON2(KB) = IA1
C
                  DO 1130 I=1,NA
                     IF (IACON2(I).GT.IB1) GOTO 1135
 1130             CONTINUE
 1135             CONTINUE
                  DO 1140 J=1,NB
                        IF (IBCON2(J).GT.IA1) GOTO 1145
 1140             CONTINUE
 1145             CONTINUE
C
                  CALL REORD(I,KA,IACON2,IB1,IP1)
                   CALL REORD(J,KB,IBCON2,IA1,IP2)
                  IPT = IP1 + IP2 + 1
                  IPER = (-1)**IPT
                  ICA2 = POSDET(NACT,NA,IACON2,IFA)
                  ICB2 = POSDET(NACT,NB,IBCON2,IFA)
C
C  Now to find where this determinant is in the list
C
                  DO 567 II=1,NODE
           IF (JPICA(II).EQ.ICA2.AND.JPICB(II).EQ.ICB2) GOTO 589
  567             CONTINUE
                  IF(MASWRK) WRITE(IW,*) 'SOMETHING IS WRONG WITH LIST'
  589             CONTINUE
                  ICI2 = KPOSJ(II)
C
      SPIN(JJJ) = SPIN(JJJ) + CI(KPOSJ(IJK),JJJ)*CI(ICI2,JJJ)*IPER
C
 1200          CONTINUE
 1100       CONTINUE
C
         SPIN(JJJ) = SPIN(JJJ) - (CI(KPOSJ(IJK),JJJ)**2.0D+00)*NSD
  788    CONTINUE
Cc
 8000    CONTINUE
         SPIN(JJJ) = SPIN(JJJ)/ZNORM
 9000 CONTINUE
      DO 943 II=1,NV
         SPIN(II)=SPIN(II)+((NA-NB)/2.0D+00)**2.0D+00 + (NA+NB)/2.0D+00
  943 CONTINUE
      DO 670 II=1,NV
         SRT = SQRT(4.0D+00*ABS(SPIN(II)) + 1.0D+00)
         SPIN(II) = (SRT-1.0D+00)/2.0D+00
  670 CONTINUE
      RETURN
      END
C
C*MODULE ALGNCI  *DECK GPRICI
C     -----------------------------------------------------------
      SUBROUTINE GPRICI(IW,NA,NB,NCOR,NORB,CI,IACON,IBCON,
     *                 IOP,CRIT,NUM,IPICA,IPICB,NPIC)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL GOPARR,DSKWRK,MASWRK
      DIMENSION CI(NPIC)
      DIMENSION IACON(NA),IBCON(NA)
      DIMENSION IPICA(NPIC),IPICB(NPIC)
      CHARACTER*30 CONA,CONB
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C    This routine prints out required part of the determinantal
C    wavefunction specified by the coefficients in CI and the
C    determinants in ipica,ipicb.
C
C   Should be compiled with the ALDeC GenCI code as it needs the routine
C   advanc to run properly.
C
C    ifa contains binomial coefficients, should be returned intact
C    from routine detci
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
C    for a maximum of 28 active orbitals.  If anytime soon someone
C    does a bigger CI then you have to do character*(nact+2) where
C    nact is number of active orbitals.
C    iop is a choice paramter
C      iop=1 prints out the largest (num) CI coefs with determinants.
C      iop=2 prints out all dets with CI coeff larger than crit
C    crit and num are explained above.
C    ipica,ipicb contain the list of determinants.
C    npic is the number of determinants.
C
      CONA(1:30) = '                              '
      CONB(1:30) = '                              '
      NACT = NORB-NCOR
C
      IF (IOP.EQ.1) THEN
C
C  Set up the table
C
         IA = (NACT+2)/2 - 2
         IF (IA.LE.0) IA = 1
         CONA(IA:IA+4) = 'ALPHA'
         CONB(IA:IA+4) = 'BETA '
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
         PMAX = 0.0D+00
C
         DO 1300 JJJ=1,NPIC
C
                 IF (ABS(CI(JJJ)).GT.PMAX) THEN
                  INDA = IPICA(JJJ)
                  INDB = IPICB(JJJ)
                  IPOS = JJJ
                  PMAX = ABS(CI(JJJ))
               ENDIF
 1300    CONTINUE
         IF (PMAX.LT.1.0D-10) GOTO 3000
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
      CONA(1:30) = ' 00000000000000000000000000000'
      CONB(1:30) = ' 00000000000000000000000000000'
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
      CI(IPOS) = 0.0D-100
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
         IF(MASWRK) WRITE(IW,'(4A)') CONA(1:NACT+2),'|',
     *                               CONB(1:NACT+2),'| COEFFICIENT'
         DO 47 II=1,NACT+2
            CONA(II:II) = '-'
   47    CONTINUE
         IF(MASWRK) WRITE(IW,'(4A)') CONA(1:NACT+2),'|',
     *                               CONA(1:NACT+2),'|------------'
C
      DO 4000 KJK=1,NPIC
C
         ICI = 0
         PMAX = 0.0D+00
C
            DO 315 JJJ=1,NPIC
C
               ICI = ICI + 1
                 IF (ABS(CI(ICI)).GT.PMAX) THEN
                  INDA = IPICA(JJJ)
                  INDB = IPICB(JJJ)
                  IPOS = JJJ
                  PMAX = ABS(CI(ICI))
               ENDIF
C
               CALL ADVANC(IBCON,NB,NACT)
  315       CONTINUE
            CALL ADVANC(IACON,NA,NACT)
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
      CONA(1:30) = ' 00000000000000000000000000000'
      CONB(1:30) = ' 00000000000000000000000000000'
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
      IF(MASWRK) WRITE(IW,'(4A,F10.7)') CONA(1:NACT+2),'|',
     *                                  CONB(1:NACT+2),'|  ',CI(IPOS)
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
C*MODULE ALGNCI  *DECK GCIDM1
      SUBROUTINE GCIDM1(NPRINT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXRT=100, MXATM=500)
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
C
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,S,SZ,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORB,
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NGCI,
     *                IGPDET,KSTSYM,NFTGCI
      COMMON /ENRGYS/ ENUCR,EELCT,ETOT,STOT,SSQUAR,ECORE,ESCF,EERD,
     *                E1,E2,VEN,VEE,EPOT,EKIN,ESTATE(MXRT),STATN
      COMMON /FMCOM / X(1)
      COMMON /FUNCT / E,EGRAD(3,MXATM)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,MA,MB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
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
      LDM1   = LCI    + NGCI
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
      NEED1 = LAST - LOADFM - 1
      CALL GETFM(NEED1)
C
      IF(EXETYP.EQ.CHECK) THEN
         CALL VCLR(X(LDAO)  ,1,L2)
         CALL VCLR(X(LVNO)  ,1,L3)
         CALL VCLR(X(LOCCNO),1,L1)
         CALL DAWRIT(IDAF,IODA,X(LDAO)  ,L2,16,0)
         CALL DAWRIT(IDAF,IODA,X(LVNO)  ,L3,19,0)
         CALL DAWRIT(IDAF,IODA,X(LOCCNO),L1,21,0)
         GO TO 580
      END IF
C
      CALL BINOM6(X(LIFA),NACT)
C
C     Now to read the $JOB.GCI file to see how many dets to be read in.
C
      CALL SEQOPN(NFTGCI,'GCILIST','UNKNOWN',.FALSE.,'UNFORMATTED')
      LEN = 1
      CALL SQRINT (NFTGCI,NGCI1,LEN)
C
      IF(NGCI1.NE.NGCI) THEN
         IF(MASWRK) WRITE(IW,9215) NGCI1,NGCI
         CALL ABRT
      ENDIF
C
C    Now to read in the list, sort and make larger lists.
C
      CALL VALFM(LOADFM)
      IPICA = LOADFM + 1
      IPICB = IPICA + NGCI/NWDVAR + 1
      JPICA = IPICB + NGCI/NWDVAR + 1
      JPICB = JPICA + NGCI/NWDVAR + 1
      KPOSJ = JPICB + NGCI/NWDVAR + 1
      LAST  = KPOSJ + NGCI/NWDVAR + 1
      NEED2 = LAST - LOADFM - 1
      CALL GETFM(NEED2)
C
      CALL SQRINT(NFTGCI,X(IPICA),NGCI)
      CALL SQRINT(NFTGCI,X(IPICB),NGCI1)
      IF (NGCI1.EQ.0.OR.NGCI.EQ.0) THEN
         IF(SOME) WRITE(IW,9217)
         CALL ABRT
      ENDIF
      CALL SEQCLO(NFTGCI,'KEEP')
C
      CALL GCISRT(X(IPICA),X(IPICB),X(JPICA),X(JPICB),X(KPOSJ),
     *            NGCI,X(LIFA),NACT,NA,NB,X(LCI),NALP,NBLP)
C
C  List read, now to read orbital symmetries
C
      CALL DAREAD(IDAF,IODA,X(LIBO),NTOT,262,1)
      CALL CORTRA(X(LIBO),NTOT,NCORSV)
C
C  Calculate extra memory for density matrix evaluation
C
      CALL GCIME1(NORB,NCOR,NA,NB,X(LIFA),NALP,NBLP,NSYM,IIS)
C
      CALL VALFM(LOADFM)
      LIWRK1 = LOADFM + 1
      LAST  = LIWRK1 + IIS
      NEED3 = LAST - LOADFM - 1
      CALL GETFM(NEED3)
C
C        set the energy to the root whose properties will be computed
C
      E = ESTATE(IROOT)
C
C        generate one particle density matrix for each state
C
      CALL SEQREW(NFT12)
      IF(MASWRK) READ(NFT12) NSTATS,NDETS
      IF (GOPARR) CALL DDI_BCAST(2515,'I',NSTATS,1,MASTER)
      IF (GOPARR) CALL DDI_BCAST(2516,'I',NDETS ,1,MASTER)
C
      IF(NSTATS.NE.K  .OR.  NDETS.NE.NGCI) THEN
         IF(MASWRK) WRITE(IW,9250) NSTATS,NDETS,K,NGCI
         CALL ABRT
      END IF
C
      DO 550 IST=1,K
         IF(NFLGDM(IST).EQ.0) THEN
            CALL SEQADV(NFT12)
            GO TO 550
         ELSE
            CALL SQREAD(NFT12,X(LCI),NGCI)
         END IF
         IF(SOME) WRITE(IW,9220) IST,ESTATE(IST)
C
         CALL GCIMA1(IW,X(LDM1),M2,NORB,NCOR,NGCI,NA,NB,
     *               X(LIFA),X(LCI),X(LIWRK1),IIS,NALP,
     *               X(IPICA),X(IPICB),X(JPICA),X(JPICB),X(KPOSJ),
     *               IGPDET,NSYM,X(LIBO))
C
         IF(SOME) THEN
            WRITE(IW,9230)
            CALL PRTRI(X(LDM1),NORB)
         END IF
         CALL DETNO(SOME,X(LDM1),X(LDAO),X(LVAO),X(LVNO),
     *              X(LOCCNO),X(LIWRK),X(LWRK),X(LSCR),ESTATE,
     *              IROOT,IST,NCORSV,NACT,M1,M2,L1,L2)
  550 CONTINUE
C
      CALL SEQREW(NFT12)
CC
  580 CONTINUE
      CALL RETFM(NEED1)
      CALL RETFM(NEED2)
      CALL RETFM(NEED3)
      IF(SOME) WRITE(IW,9240)
      IF(SOME) CALL TIMIT(1)
      RETURN
C
 9210 FORMAT(/5X,27("-")/5X,'ONE PARTICLE DENSITY MATRIX'/5X,27("-")//
     *  1X,'DENSITY MATRIX WILL BE SAVED FOR PROPERTIES OF STATE',I4)
 9215 FORMAT(/1X,'***** ERROR IN -GCIDM1- ROUTINE *****'/
     *       1X,'SIZE OF LIST TO BE READ IN              = ',I10/
     *       1X,'SIZE OF LIST IN CALCULATED WAVEFUNCTION = ',I10)
 9217 FORMAT(/1X,'NUMBER OF DETERMINANTS IN LIST IS LESS THAN ',
     *           'NUMBER SPECIFIED.'/1X,'STOPPING RUN NOW !!!!')
C
 9220 FORMAT(/1X,'CI EIGENSTATE',I4,' TOTAL ENERGY =',F20.10)
 9230 FORMAT(/1X,'1-PARTICLE DENSITY MATRIX IN MO BASIS')
 9240 FORMAT(1X,'..... DONE WITH ONE PARTICLE DENSITY MATRIX .....')
 9250 FORMAT(/1X,'***** ERROR IN -GCIDM1- ROUTINE *****'/
     *       1X,'CI EIGENVECTOR FILE HAS DATA FOR NSTATE,NDETS=',I3,I10/
     *       1X,'BUT THE PRESENT CALCULATION HAS NSTATE,NDETS=',I3,I10)
      END
C
C*MODULE ALGNCI  *DECK GCIME1
C     --------------------------------------------------------
      SUBROUTINE GCIME1(NORB,NCOR,NA,NB,IFA,NALP,NBLP,NSYM,IIS)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR)
C
      NACT = NORB - NCOR
      NALP = IFA(NACT,NA)
      NBLP = IFA(NACT,NB)
C
      IIS = 3*NA + ((NACT*(NACT+1))/2)**2 + 2*NALP + NSYM*NSYM +43
C
      RETURN
      END
C
C*MODULE ALGNCI  *DECK GCIMA1
C     --------------------------------------------------------
      SUBROUTINE GCIMA1(IW,D1,M2,NORB,NCOR,NCI,NA,NB,
     *           IFA,CI,IWRK,IIS,NALP,
     *           IPICA,IPICB,JPICA,JPICB,KPOSJ,
     *           IDSYM,NSYM,IOB)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION D1(M2)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION CI(NCI)
      DIMENSION IWRK(IIS)
      DIMENSION IPICA(NCI),IPICB(NCI),JPICA(NCI),JPICB(NCI)
      DIMENSION KPOSJ(NCI)
      DIMENSION IOB(NORB-NCOR)
C
C   Code to return density matrices, here we partition the memory up.
C   Written by J. Ivanic '99
C
C   **************************************
C   Should have called routines cormod(cormod.f) and gmemci(gmemci.f)
C   and sort(sort.f) before this !!!!!!!!!!!!!
C   **************************************
C
C   d1        : 1 electron density matrix stored in reverse canonical
C       order.
C
C   norb,ncor : total no. of orbitals and no. of core orbs respectively
C             : ncor should equal 0, and norb = active no. of orbs for
C               real efficiency.  ie, Integrals should be adjusted such
C               that there are no core orbitals using routine cormod in
C               cormod.f
C   nci       : number of specified determinants, size of CI essentially
C   na, nb    : number of active alpha and beta electrons respectively
C   ifa       : Contains binomial coefficients, obtained by calling
C               routine gmemci.
C   CI        : Contains CI coefficients.
C   iwrk      : Scratch integer space
C   iis       : Space for iwrk in order to do the CI, should be obtained
C               from gcime1
C   nalp, nblp: No. of alpha and beta space products respectively, got
C               from gmemci
C   ipica,ipicb contain the list of determinants, alpha and beta string,
C               these are
C               to be ordered first according to alpha and then in each
C               alpha subgroup, according to beta.
C   jpica,jpicb contain the list of determinants but ordered first
C               according to beta and then alpha
C   kposj       contains the position of determinant jpica(i),jpicb(i)
C               in the ipica,ipicb list.
C ********
C  ipica,ipicb,jpica,jpicb and kposj may be obtained from sort(sort.f)
C  if you have a list (in any order) of determinants in ipica,ipicb.
C ********
C  idsym,nsym  : See symwrk.f, but quickly,
C                it depends on the point group...
C                C1,C2,Cs,Ci   idsym=1,nsym=2
C                C2v,D2,C2h    idsym=2,nsym=4
C                D2h           idsym=3,nsym=8
C   iob        : iob(i) contains symmetry of active orbital i,
C                see gtab in symwrk.f
C                for information.
C    Now for integer iwrk array
C
      NACT = NORB - NCOR
C
      LIACON1 = 1+43
      LIBCON1 = LIACON1 + NA
      LIACON2 = LIBCON1 + NA
      LINDEX = LIACON2 + NA
      LLPICB1 = LINDEX + ((NACT*(NACT+1))/2)**2
      LLPICB2 = LLPICB1 + NALP
      LIGMUL = LLPICB2 + NALP
      LITOT = LIGMUL+ (NSYM*NSYM)
C
      IF (LITOT.GT.IIS+1) THEN
         WRITE(IW,*) 'ASKED FOR ',IIS,' INTEGER'
         WRITE(IW,*) ' ACTUALLY NEED'
         WRITE(IW,*) LITOT-1
         CALL ABRT
      ENDIF
C
      CALL GMUL(IDSYM,IWRK(LIGMUL),IWRK(1),
     *    IWRK(4),IWRK(7),IWRK(10))
C
      IT1 = (NACT*(NACT+1))/2
C
      CALL GCIMA2(D1,IT1,
     *    NACT,0,NA,NB,CI,
     *    IWRK(LIACON1),IWRK(LIBCON1),IWRK(LIACON2),IFA,
     *    IWRK(LINDEX),NALP,NCI,IPICA,IPICB,
     *    IWRK(LLPICB1),IWRK(LLPICB2),JPICA,JPICB,KPOSJ,IOB)
C
      RETURN
      END
C*MODULE ALGNCI  *DECK GCIMA2
C     --------------------------------------------------------
      SUBROUTINE GCIMA2(DEN,IT1,NORB,NCOR,NA,NB,CI,
     *       IACON1,IBCON1,IACON2,IFA,INDEX,
     *       NALP,NPIC,IPICA,IPICB,LPICB1,LPICB2,
     *     JPICA,JPICB,KPOSJ,IOX)
C     --------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
      DIMENSION DEN(IT1)
      DIMENSION CI(NPIC)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION IACON1(NA+NCOR),IBCON1(NA+NCOR)
      DIMENSION IACON2(NA+NCOR)
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2)
      DIMENSION IPICA(NPIC),IPICB(NPIC)
      DIMENSION LPICB1(NALP),LPICB2(NALP)
      DIMENSION JPICA(NPIC),JPICB(NPIC),KPOSJ(NPIC)
      DIMENSION IOX(NORB-NCOR)
C
      NACT = NORB - NCOR
      NAT = NA + NCOR
      NBT = NB + NCOR
C
      DO 7 I=1,(NORB*(NORB+1))/2
         DO 8 J=1,I
            INDEX(I,J) = I*(I-1)/2 + J
            INDEX(J,I) = INDEX(I,J)
    8    CONTINUE
    7 CONTINUE
C
C
      NTOT = NORB*(NORB+1)/2
C
      DO 13 I=1,NTOT
         DEN(I) = 0.0D+00
   13 CONTINUE
C
      DO 30 I=1,NAT
         IACON1(I) = I
   30 CONTINUE
C
C   Big Loop over all alpha determinants in ipica
C
      IZX = 1
      IZY = 1
 9013 CONTINUE
      DO 7010 KK=IZX,IPICA(IZY)-1
         CALL ADVANC(IACON1,NAT,NORB)
 7010 CONTINUE
      DO 7013 IZW=IZY,NPIC
         IF (IPICA(IZY).NE.IPICA(IZW)) GOTO 7015
 7013 CONTINUE
C
 7015 IZW = IZW - 1
C
C  Now, we have indices in ipicaa from izy -> izw
C  and the beta determinants are in increasing
C   order ipicb(izy) -> ipicb(izw) *****
C
C  Alpha excitations here
C   Single first
C
         IAC = 0
         DO 7030 IA=NCOR+1,NAT
             IO1 = IACON1(IA)
             IST = IO1 + 1
             IEN = IACON1(IA+1)-1
CSymmetry of orbital being deoccupied
             IS1 = IOX(IO1)
C
             IF (IA.EQ.NAT) IEN=NORB
             DO 7025 KKJ=IA-NCOR+1,NA+1
                DO 7020 JJ=IST,IEN
CSymmetry of orbitals being occupied
             IS2 = IOX(JJ)
Cis1xis2 = ip1
C
             CALL RET1DET(IACON1,IACON2,NA,IA,JJ,NCOR,KKJ,IPER1)
             IPET = POSDET(NACT,NA,IACON2,IFA)
C
C***
C  Check to see if this excited alpha determinant is in ipica
C
             DO 7018 KK=IZW+1,NPIC
                IF (IPICA(KK).EQ.IPET) GOTO 7021
 7018        CONTINUE
             GOTO 413
 7021        JZY = KK
C
C   Check to see how many of the ipet alphas there are
C
             DO 7040 KK=JZY,NPIC
                IF (IPICA(KK).NE.IPET) GOTO 7034
 7040        CONTINUE
 7034        JZW = KK - 1
C
C   Storage here for later use, well worth it timewise
C
             IAC = IAC + 1
             IPERZ = ((-1)**IPER1)
             IND = INDEX(JJ,IO1)
C
C If deoccupied and newly occupied are of different sym skip to doubles
             IF (IS1.NE.IS2) GOTO 413
C
C   Now the excited alphas lie in ipica(jzy - > jzw)
C
C   Now have to see if any of the beta determinants match up.
C
             NPCA = 0
             ISY = JZY
             DO 7051 KK1=IZY,IZW
                DO 7053 KK2 = ISY,JZW
                   IF (IPICB(KK1).EQ.IPICB(KK2)) THEN
                      NPCA = NPCA + 1
                      LPICB1(NPCA) = KK1
                      LPICB2(NPCA) = KK2
                      ISY = KK2+1
                      GOTO 7051
                   ENDIF
 7053           CONTINUE
 7051        CONTINUE
             IF (NPCA.EQ.0) GOTO 413
C
C   Now I know that there are npca determinants which
C   match up and that they are the determinants given
C   by ipica(lpicb1(i)), ipicb(lpicb1(i))
C  and ipica(lpicb2(i)), ipicb(lpicb2(i)),i=1,npca
C  lpicb1(i),lpicb2(i) are actally the positions
C  of the CI coefficients.
C
             DO 407 INB1 = 1,NPCA
                ICIT = LPICB1(INB1)
                ICI2 = LPICB2(INB1)
                FC = CI(ICIT)*CI(ICI2)
                FC = FC*IPERZ
                DEN(IND) = DEN(IND) + FC
  407       CONTINUE
C
  413        CONTINUE
C
C
 7020           CONTINUE
                IST = IACON1(KKJ+NCOR)+1
                IEN = IACON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NA) IEN=NORB
 7025        CONTINUE
 7030     CONTINUE
C
C Diagonal contributions here
C
            DO 67 II=1,NAT
               I1 = IACON1(II)
               IND1 = INDEX(I1,I1)
C
              DO 53 INB1 = IZY,IZW
              ICIT = INB1
              FC = CI(ICIT)*CI(ICIT)
              DEN(IND1) = DEN(IND1) + FC
   53         CONTINUE
C
   67       CONTINUE
C
      IZX = IPICA(IZW)
      IZY = IZW + 1
      IF (IZW.NE.NPIC) GOTO 9013
C
C  End of the original alpha loop ^
C
C   Now for the Beta part
C
      DO 876 JJI=1,NBT
         IBCON1(JJI) = JJI
  876 CONTINUE
C
C  Big loop over all beta determinants
C
      IZX = 1
      IZY = 1
 5013 CONTINUE
      DO 3010 KK=IZX,JPICB(IZY)-1
         CALL ADVANC(IBCON1,NBT,NORB)
 3010 CONTINUE
      DO 3013 IZW=IZY,NPIC
         IF (JPICB(IZY).NE.JPICB(IZW)) GOTO 3015
 3013 CONTINUE
 3015 IZW = IZW - 1
C
C  Now we have indices in jpicb from izy -> izw
C  and the alpha determinants are in increasing
C  order jpica(izy) -> jpica(izw) *****
C
C   Single Beta excitations
C
      DO 6030 IB=NCOR+1,NBT
         IO1 = IBCON1(IB)
CSymmetry of orbital being deoccupied
         IS1 = IOX(IO1)
         IST = IO1+1
         IEN = IBCON1(IB+1)-1
         IF (IB.EQ.NBT) IEN=NORB
         DO 6025 KKJ=IB-NCOR+1,NB+1
            DO 6020 JJ=IST,IEN
CSymmetry of orbital being occupied
         IS2 = IOX(JJ)
Cis1xis2 = ip1
C
            CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
            IPER = ((-1)**IPER1)
            IND = INDEX(JJ,IO1)
C    If deoccupied and newly occupied are of different symmetry,
C    skip to doubles
            IF (IS1.NE.IS2) GOTO 313
            IPB1 = POSDET(NACT,NB,IACON2,IFA)
C
C  ***
C Check to see if this excited beta determinant is in jpicb
C
            DO 3018 KK=IZW+1,NPIC
               IF (JPICB(KK).EQ.IPB1) GOTO 3021
 3018       CONTINUE
            GOTO 313
 3021       JZY = KK
C
C  Check to see how many of the ipb1 betas there are
C
            DO 3040 KK=JZY,NPIC
               IF (JPICB(KK).NE.IPB1) GOTO 3034
 3040       CONTINUE
 3034       JZW = KK - 1
C
C  Now the excited betas lie in jpicb(jzy -> jzw)
C
C  Now to see if any of the alpha determinants match up.
C
            NPCA = 0
            ISY = JZY
            DO 3051 KK1=IZY,IZW
               DO 3053 KK2=ISY,JZW
                  IF (JPICA(KK1).EQ.JPICA(KK2)) THEN
                     NPCA = NPCA + 1
                     LPICB1(NPCA) = KK1
                     LPICB2(NPCA) = KK2
                     ISY = KK2 + 1
                     GOTO 3051
                  ENDIF
 3053          CONTINUE
 3051       CONTINUE
            IF (NPCA.EQ.0) GOTO 313
C
C   Now I know that there are npca determinants that match up
C   and that they are the determinants given by
C      jpica(lpicb1(i)), jpicb(lpicb1(i))
C and  jpica(lpicb2(i)), jpicb(lpicb2(i)),i=1,npca
C  kposj(lpicb1(i)) and kposj(lpicb2(i)) are actually the
C  position of the CI coefficients.
C
C
C    C = si1(ind)
          DO 507 INA1=1,NPCA
             ICIT = KPOSJ(LPICB1(INA1))
             ICI2 = KPOSJ(LPICB2(INA1))
             FC = CI(ICIT)*CI(ICI2)
             FC = FC * IPER
             DEN(IND) = DEN(IND)+FC
  507 CONTINUE
C
  313 CONTINUE
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
            DO 69 II=1,NBT
               I1 = IBCON1(II)
               IND1 = INDEX(I1,I1)
            DO 93 INA1 = IZY,IZW
              ICIT = KPOSJ(INA1)
              FC = CI(ICIT)*CI(ICIT)
              DEN(IND1) = DEN(IND1) + FC
   93       CONTINUE
C
   69       CONTINUE
C
      IZX = JPICB(IZW)
      IZY = IZW + 1
      IF (IZW.NE.NPIC) GOTO 5013
C
C  End of the Beta loop ^
C
C
      RETURN
      END
C
C*MODULE ALGNCI  *DECK GCIDM2
      SUBROUTINE GCIDM2(NPRINT)
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
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NGCI,
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
         IF(MASWRK) WRITE(IW,*)
     *       'OOPS, IN -GCIDM2-, SOMETHING HAPPENED TO WSTATE'
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
      LDM1   = LCI    + MXNZW*NGCI
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
      NEED1 = LAST - LOADFM - 1
      CALL GETFM(NEED1)
C
      IF(SOME) WRITE(IW,9320) NEED1,MXNZW
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
C
C     Now to read the $JOB.GCI file to see how many dets to be read in.
C
      CALL SEQOPN(NFTGCI,'GCILIST','UNKNOWN',.FALSE.,'UNFORMATTED')
      LEN = 1
      CALL SQRINT (NFTGCI,NGCI1,LEN)
C
      IF(NGCI1.NE.NGCI) THEN
         IF(MASWRK) WRITE(IW,9355) NGCI1,NGCI
         CALL ABRT
      ENDIF
C
C    Now to read in the list, sort and make larger lists.
C
      CALL VALFM(LOADFM)
      IPICA = LOADFM + 1
      IPICB = IPICA + NGCI/NWDVAR + 1
      JPICA = IPICB + NGCI/NWDVAR + 1
      JPICB = JPICA + NGCI/NWDVAR + 1
      KPOSJ = JPICB + NGCI/NWDVAR + 1
      LAST  = KPOSJ + NGCI/NWDVAR + 1
      NEED2 = LAST - LOADFM - 1
      CALL GETFM(NEED2)
C
      CALL SQRINT(NFTGCI,X(IPICA),NGCI)
      CALL SQRINT(NFTGCI,X(IPICB),NGCI1)
      IF (NGCI1.EQ.0.OR.NGCI.EQ.0) THEN
         IF(MASWRK) WRITE(IW,9357)
         CALL ABRT
      ENDIF
      CALL SEQCLO(NFTGCI,'KEEP')
C
      CALL GCISRT(X(IPICA),X(IPICB),X(JPICA),X(JPICB),X(KPOSJ),
     *            NGCI,X(LIFA),NACT,NA,NB,X(LCI),NALP,NBLP)
C
C  List read, now to read orbital symmetries
C
      CALL DAREAD(IDAF,IODA,X(LIBO),NTOT,262,1)
      CALL CORTRA(X(LIBO),NTOT,NCORSV)
C
C  Now for extra memory for density matrices determination
C
      CALL GCIME2(NORB,NCOR,NA,NB,X(LIFA),NSYM,NALP,NBLP,IIS)
C
      CALL VALFM(LOADFM)
      LIWRK1 = LOADFM + 1
      LAST  = LIWRK1 + IIS
      NEED3 = LAST - LOADFM - 1
      CALL GETFM(NEED3)
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
      IF (GOPARR) CALL DDI_BCAST(2517,'I',NSTATS,1,MASTER)
      IF (GOPARR) CALL DDI_BCAST(2518,'I',NDETS ,1,MASTER)
C
      IF(NSTATS.NE.K  .OR.  NDETS.NE.NGCI) THEN
         IF(MASWRK) WRITE(IW,9360) NSTATS,NDETS,K,NGCI
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
            CALL SQREAD(NFT12,X(LCIVEC),NGCI)
            LCIVEC = LCIVEC + NGCI
            NXTW=NXTW+1
         END IF
  620 CONTINUE
      CALL SEQREW(NFT12)
C
      CALL GCISA1(IW,X(LDM1),M2,X(LDM2),M4,NORB,NCOR,NGCI,NA,NB,
     *             X(LIFA),X(LCI),
     *              X(LIWRK1),IIS,NALP,NBLP,
     *       X(IPICA),X(IPICB),X(JPICA),X(JPICB),X(KPOSJ),
     *     IGPDET,NSYM,X(LIBO),
     *               MXNZW,IWTS,WSTATE,X(LIWST),X(LWST),K)
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
      CALL RETFM(NEED3)
      CALL RETFM(NEED2)
      CALL RETFM(NEED1)
      IF(SOME) WRITE(IW,9390)
      IF(SOME) CALL TIMIT(1)
      RETURN
C
 9310 FORMAT(/5X,50("-")/
     *       5X,'       COMPLETELY ARBITRARY SELECTED CI '/
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
 9355 FORMAT(/1X,'***** ERROR IN -GCIDM2- ROUTINE *****'/
     *       1X,'SIZE OF LIST TO BE READ IN              = ',I10/
     *       1X,'SIZE OF LIST IN CALCULATED WAVEFUNCTION = ',I10)
 9357 FORMAT(/1X,'NUMBER OF DETERMINANTS IN LIST IS LESS THAN ',
     *           'NUMBER SPECIFIED.'/1X,'STOPPING RUN NOW !!!!')
 9360 FORMAT(/1X,'***** ERROR IN -DETDM2- ROUTINE *****'/
     *       1X,'CI EIGENVECTOR FILE HAS DATA FOR NSTATE,NDETS=',I3,I10/
     *       1X,'BUT THE PRESENT CALCULATION HAS NSTATE,NDETS=',I3,I10)
 9370 FORMAT(1X,'SIEVING THE ',A4,
     *          ' SYMMETRY NONZERO DENSITY ELEMENTS IN GROUP ',A8)
 9380 FORMAT(1X,I10,' NONZERO DM2 ELEMENTS WRITTEN IN',I8,
     *          ' RECORDS TO FILE',I3)
 9390 FORMAT(1X,'..... DONE WITH 1 AND 2 PARTICLE DENSITY MATRIX .....')
      END
C
C*MODULE ALGNCI  *DECK GCIME2
C     ---------------------------------------------------------
      SUBROUTINE GCIME2(NORB,NCOR,NA,NB,IFA,NSYM,NALP,NBLP,IIS)
C     ---------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR)
C
      NACT = NORB - NCOR
      NALP = IFA(NACT,NA)
      NBLP = IFA(NACT,NB)
C
      IIS = 3*NA + 5*(NA*(NACT-NA)) + 3*NALP + NSYM*NSYM +
     *      ((NACT*(NACT+1))/2)**2 + 43
C
      RETURN
      END
C*MODULE ALGNCI  *DECK GCISA1
C     --------------------------------------------------------
      SUBROUTINE GCISA1(IW,D1,M2,D2,M4,NORB,NCOR,NCI,NA,NB,
     *                 IFA,CI,
     *                 IWRK,IIS,NALP,NBLP,
     *           IPICA,IPICB,JPICA,JPICB,KPOSJ,
     *     IDSYM,NSYM,IOB,
     *               NSTATE,IWTS,WSTATE,IWH,W,MXSTATE)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION D1(M2),D2(M4)
      DIMENSION IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION CI(NCI,MXSTATE),IWRK(IIS)
      DIMENSION IPICA(NCI),IPICB(NCI),JPICA(NCI),JPICB(NCI)
      DIMENSION KPOSJ(NCI)
      DIMENSION IOB(NORB-NCOR)
      DIMENSION IWH(NSTATE),W(NSTATE)
      DIMENSION IWTS(*),WSTATE(*)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C   Code to return density matrices, here we partition the memory up.
C   Written by J. Ivanic '99
C
C   d1, d2  : 1 and 2 e- density matrices in reverse canonical order.
C
C   norb,ncor : total no. of orbitals and no. of core orbs respectively
C             : ncor should equal 0, and norb = active no. of orbs for
C               real efficiency.  ie, Integrals should be adjusted such
C               that there are no core orbitals using routine cormod
C   nci       : number of specified determinants, size of CI essentially
C   na, nb    : number of active alpha and beta electrons respectively
C   ifa       : Contains binomial coefficients.
C   CI        : Contains CI coefficients.
C   iwrk      : Scratch integer space
C   iis       : Space for iwrk in order to do the CI, should be obtained
C               from gcime2
C   nalp, nblp: No. of alpha and beta space products respectively, got
C               from gmemci
C   ipica,ipicb contain the list of determinants, alpha and beta string,
C               these are to be ordered first according to alpha and
C               then in each alpha subgroup, according to beta.
C   jpica,jpicb contain the list of determinants but ordered first
C               according to beta and then alpha
C   kposj       contains the position of determinant jpica(i),jpicb(i)
C               in the ipica,ipicb list.
C ********
C  ipica,ipicb,jpica,jpicb and kposj may be obtained from sort(sort.f)
C  if you have a list (in any order) of determinants in jpica,jpicb.
C ********
C  idsym,nsym  : See symwrk.f, but quickly,
C                it depends on the point group...
C                C1,C2,Cs,Ci   idsym=1,nsym=2
C                C2v,D2,C2h    idsym=2,nsym=4
C                D2h           idsym=3,nsym=8
C   iob        : iob(i) contains symmetry of active orbital i,
C                see gtab in symwrk.f for information.
C     nstate is the number of states to be averaged
C     iwh(i) contains which state the ith one is. ie, which CI vector
C     W(i) contains the weight of vector iwh(i)
C     mxstate = iwh(nstate) and is only given for dimensioning purposes.
C
C    Now for integer iwrk array
C
      NACT = NORB - NCOR
C
      IACON1 = 1
      IBCON1 = IACON1 + NA + 43
      IACON2 = IBCON1 + NA
      IPOSA = IACON2 + NA
      IPERA = IPOSA + (NA*(NACT-NA))
      IIND1 = IPERA + (NA*(NACT-NA))
      ISYMX = IIND1 + (NA*(NACT-NA))
      INDEX = ISYMX + (NA*(NACT-NA))
      LPICB1 = INDEX + ((NACT*(NACT+1))/2)**2
      LPICB2 = LPICB1 + NALP
      IPOSA1 = LPICB2 + NALP
      ITEST = IPOSA1 + (NA*(NACT-NA))
      IGMUL = ITEST + NALP
      ITOT = IGMUL+ (NSYM*NSYM)
C
      IF (ITOT.GT.IIS+1) THEN
         IF(MASWRK) THEN
            WRITE(IW,*) 'ASKED FOR ',IIS,' INTEGER'
            WRITE(IW,*) ' ACTUALLY NEED'
            WRITE(IW,*) ITOT-1
         END IF
         CALL ABRT
      ENDIF
C
      CALL GMUL(IDSYM,IWRK(IGMUL),IWRK(1),IWRK(4),IWRK(7),IWRK(10))
C
      IT1 = (NORB*(NORB+1))/2
      IT2 = (IT1*(IT1+1))/2
      CALL GCISA2(D1,IT1,D2,IT2,NACT,0,NA,NB,CI,
     *    NSTATE,IWTS,WSTATE,IWH,W,MXSTATE,
     *    IWRK(IACON1),IWRK(IBCON1),IWRK(IACON2),IFA,
     *    IWRK(IPOSA),IWRK(IPERA),IWRK(IIND1),IWRK(ISYMX),
     *    IWRK(INDEX),NALP,NBLP,NCI,IPICA,IPICB,
     *    IWRK(LPICB1),IWRK(LPICB2),JPICA,JPICB,KPOSJ,IWRK(IPOSA1),
     *    IWRK(ITEST),IOB,
     *    IWRK(IGMUL),NSYM)
C
      RETURN
      END
C*MODULE ALGNCI  *DECK GCISA2
C     --------------------------------------------------------
      SUBROUTINE GCISA2(DEN,IT1,DEN2,IT2,NORB,NCOR,NA,NB,CI,
     *    NSTATE,IWTS,WSTATE,IWH,W,MXSTATE,
     *       IACON1,IBCON1,IACON2,IFA,IPOSA,IPERA,IIND1,ISYMX,
     *     INDEX,
     *          NALP,NBLP,
     *     NPIC,IPICA,IPICB,LPICB1,LPICB2,
     *     JPICA,JPICB,KPOSJ,IPOSA1,ITEST,IOX,IGMUL,NSYM)
C     --------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
      DIMENSION DEN(IT1),DEN2(IT2)
      DIMENSION IWH(NSTATE),W(NSTATE),IWTS(*),WSTATE(*)
      DIMENSION CI(NPIC,MXSTATE),IFA(0:NORB-NCOR,0:NORB-NCOR)
      DIMENSION IACON1(NA+NCOR),IBCON1(NA+NCOR)
      DIMENSION IACON2(NA+NCOR),IPERA(NA*(NORB-NCOR-NA))
      DIMENSION IIND1(NA*(NORB-NCOR-NA))
      DIMENSION IPOSA(NA*(NORB-NCOR-NA))
      DIMENSION ISYMX(NA*(NORB-NCOR-NA))
      DIMENSION INDEX((NORB*(NORB+1))/2,(NORB*(NORB+1))/2)
      DIMENSION IPICA(NPIC),IPICB(NPIC)
      DIMENSION LPICB1(NALP),LPICB2(NALP)
      DIMENSION JPICA(NPIC),JPICB(NPIC),KPOSJ(NPIC)
      DIMENSION IPOSA1(NA*(NORB-NCOR-NA))
      DIMENSION ITEST(NA*(NORB-NCOR-NA))
      DIMENSION IOX(NORB),IGMUL(NSYM,NSYM)
C
      DO 1 IST=1,NSTATE
          W(IST) = WSTATE(IWTS(IST))
         IWH(IST) = IST
    1 CONTINUE
C
C      call binom6(ifa,norb-ncor)
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
C   Big Loop over all alpha determinants in ipica
C
      IZX = 1
      IZY = 1
 9013 CONTINUE
      DO 7010 KK=IZX,IPICA(IZY)-1
         CALL ADVANC(IACON1,NAT,NORB)
 7010 CONTINUE
      DO 7013 IZW=IZY,NPIC
         IF (IPICA(IZY).NE.IPICA(IZW)) GOTO 7015
 7013 CONTINUE
C
 7015 IZW = IZW - 1
C
C  Now, we have indices in ipicaa from izy -> izw
C  and the beta determinants are in increasing
C   order ipicb(izy) -> ipicb(izw) *****
C
C  Alpha excitations here
C   Single first
C
         IAC = 0
         DO 7030 IA=NCOR+1,NAT
             IO1 = IACON1(IA)
             IST = IO1 + 1
             IEN = IACON1(IA+1)-1
CSymmetry of orbital being deoccupied
             IS1 = IOX(IO1)
C
             IF (IA.EQ.NAT) IEN=NORB
             DO 7025 KKJ=IA-NCOR+1,NA+1
                DO 7020 JJ=IST,IEN
CSymmetry of orbitals being occupied
             IS2 = IOX(JJ)
Cis1xis2 = ip1
             IP1 = IGMUL(IS2,IS1)
C
             CALL RET1DET(IACON1,IACON2,NA,IA,JJ,NCOR,KKJ,IPER1)
             IPET = POSDET(NACT,NA,IACON2,IFA)
C
C***
C  Check to see if this excited alpha determinant is in ipica
C
             DO 7018 KK=IZW+1,NPIC
                IF (IPICA(KK).EQ.IPET) GOTO 7021
 7018        CONTINUE
             GOTO 413
 7021        JZY = KK
C
C   Check to see how many of the ipet alphas there are
C
             DO 7040 KK=JZY,NPIC
                IF (IPICA(KK).NE.IPET) GOTO 7034
 7040        CONTINUE
 7034        JZW = KK - 1
C
C   Storage here for later use, well worth it timewise
C
             IAC = IAC + 1
C            iposa(iac) = (ipet-1)*nblp
             IPOSA(IAC) = JZY
             IPOSA1(IAC) = JZW
             IPERA(IAC) = ((-1)**IPER1)*2
             IND = INDEX(JJ,IO1)
             IIND1(IAC) = IND
             ISYMX(IAC) = IP1
C
C If deoccupied and newly occupied are of different sym skip to doubles
             IF (IS1.NE.IS2) GOTO 413
C
C   Now the excited alphas lie in ipica(jzy - > jzw)
C
C   Now have to see if any of the beta determinants match up.
C
             NPCA = 0
             ISY = JZY
             DO 7051 KK1=IZY,IZW
                DO 7053 KK2 = ISY,JZW
                   IF (IPICB(KK1).EQ.IPICB(KK2)) THEN
                      NPCA = NPCA + 1
                      LPICB1(NPCA) = KK1
                      LPICB2(NPCA) = KK2
                      ISY = KK2+1
                      GOTO 7051
                   ENDIF
 7053           CONTINUE
 7051        CONTINUE
             IF (NPCA.EQ.0) GOTO 413
C
C   Now I know that there are npca determinants which
C   match up and that they are the determinants given
C   by ipica(lpicb1(i)), ipicb(lpicb1(i))
C  and ipica(lpicb2(i)), ipicb(lpicb2(i)),i=1,npca
C  lpicb1(i),lpicb2(i) are actally the positions
C  of the CI coefficients.
C
             DO 407 INB1 = 1,NPCA
                ICIT = LPICB1(INB1)
                ICI2 = LPICB2(INB1)
                FC = 0.0D+00
                DO 409 KKI=1,NSTATE
         FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICI2,IWH(KKI))
  409       CONTINUE
            FC = FC*IPERA(IAC)
            DEN(IND) = DEN(IND) + FC
  407       CONTINUE
C
             DO 412 IK=1,NAT
                IF (IK.EQ.IA) GOTO 412
                ION = IACON1(IK)
                J1 = INDEX(ION,ION)
                   JJ1 = INDEX(IND,J1)
                J1 = INDEX(ION,JJ)
                J2 = INDEX(ION,IO1)
                INX = INDEX(J1,J2)
                DO 491 INBB=1,NPCA
                   ICIT = LPICB1(INBB)
                   ICI2 = LPICB2(INBB)
                   FC = 0.0D+00
            DO 419 KKI=1,NSTATE
               FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICI2,IWH(KKI))
  419       CONTINUE
            FC = FC * IPERA(IAC)
            DEN2(JJ1) = DEN2(JJ1)+FC
            DEN2(INX) = DEN2(INX)-FC
  491           CONTINUE
  412        CONTINUE
C
               DO 49 I=1,NBT
                IBCON1(I) = I
   49        CONTINUE
C
C   Loop over beta dets, check to see if they equal any
C   of ipicb(lpicb1(i)),i=1,npca
C
             IXYY = 1
C
             DO 415 INB1=1,NBLP
C
                 IF (IXYY.EQ.NPCA+1) GOTO 413
                 IF (INB1.EQ.IPICB(LPICB1(IXYY))) GOTO 7058
                 GOTO 414
 7058            CONTINUE
C
                 ICIT = LPICB1(IXYY)
                 ICI2 = LPICB2(IXYY)
                 IXYY = IXYY + 1
C
C  Now I know the determinant ibcon1 and the positions
C  of the CI coefficients are lpicb1(kk) and
C  lpicb2(kk)
C
C              ici2 = iposa(iac)+inb1
                FC = 0.0D+00
                DO 719 KKI=1,NSTATE
           FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICI2,IWH(KKI))
  719       CONTINUE
            FC = FC * IPERA(IAC)
                DO 790 IK=1,NBT
                   ION = IBCON1(IK)
                   J1 = INDEX(ION,ION)
                    JJ1 = INDEX(IND,J1)
                   DEN2(JJ1) = DEN2(JJ1) + FC
  790           CONTINUE
C
  414           CALL ADVANC(IBCON1,NBT,NORB)
  415        CONTINUE
C
  413        CONTINUE
C
C      Double alpha excitations
C
          DO 4015 IAA = IA+1,NAT
             IPA = IAA-NCOR
             IIA = IACON1(IAA)
CSymmetry of orbital being deoccupied
             IS3 = IOX(IIA)
             IF (JJ.GT.IIA) IPA = IPA - 1
             ISTAA = JJ+1
             IENAA = IEN
             DO 4010 KKJAA=KKJ,NA+1
                DO 4005 JJAA=ISTAA,IENAA
CSymmetry of orbital being occupied
             IS4 = IOX(JJAA)
Cip2 = is3xis4
             IP2 = IGMUL(IS3,IS4)
CIf symmetry of alpha '' is not right, skip it.
             IF(IP1.NE.IP2) GOTO 4005
C
             CALL RET1DET(IACON2,IBCON1,NA,IPA,JJAA-NCOR,0,KKJAA,IPER2)
             IPET = POSDET(NACT,NA,IBCON1,IFA)
C
C ****
C Check to see if this doubly excited alpha determinant
C is in ipica
C
             DO 8018 KK=IZW+1,NPIC
                IF (IPICA(KK).EQ.IPET) GOTO 8021
 8018        CONTINUE
             GOTO 4005
 8021        KZY = KK
C
C  Check to see how many of them there are
C
             DO 8040 KK=KZY,NPIC
                IF (IPICA(KK).NE.IPET) GOTO 8034
 8040        CONTINUE
 8034        KZW = KK - 1
C
C   Now the doubly excited alphas lie in ipica(kzy -> kzw)
C
C  Now to see if any of the beta determinants match up
C
             NPCA = 0
             ISY = KZY
             DO 8051 KK1 = IZY,IZW
                     DO 8053 KK2 = ISY,KZW
                  IF (IPICB(KK1).EQ.IPICB(KK2)) THEN
                     NPCA = NPCA + 1
                     LPICB1(NPCA) = KK1
                     LPICB2(NPCA) = KK2
                     ISY = KK2+1
                     GOTO 8051
                  ENDIF
 8053           CONTINUE
 8051       CONTINUE
            IF (NPCA.EQ.0) GOTO 4005
C
C  Now I know that there are npca determinants which
C  match up and that they are the determinants given
C  by ipica(lpicb1(i)), ipicb(lpicb1(i))
C and ipica(lpicb2(i)), ipicb(lpicb2(i)),i=1,npca
C lpicb1(i), lpicb2(i) are actually the positions
C of the CI coefficients.
C
             IPERT = IPER1+IPER2
             IPERT = ((-1)**IPERT)*2
C
                   IND = INDEX(JJ,IO1)
                   I2 = INDEX(IIA,JJAA)
                   INX = INDEX(I2,IND)
                   II1 = INDEX(JJAA,IO1)
                   II2 = INDEX(IIA,JJ)
                   INX2 = INDEX(II1,II2)
C
                DO 786 INB1 = 1,NPCA
                   ICI2 = LPICB1(INB1)
                   ICIT = LPICB2(INB1)
                   FC = 0.0D+00
                DO 819 KKI=1,NSTATE
                   FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICI2,IWH(KKI))
  819           CONTINUE
                FC = FC * IPERT
                DEN2(INX) = DEN2(INX) + FC
                DEN2(INX2) = DEN2(INX2) - FC
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
C Diagonal contributions here
C
            DO 67 II=1,NAT
               I1 = IACON1(II)
               IND1 = INDEX(I1,I1)
C
              DO 53 INB1 = IZY,IZW
              ICIT = INB1
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
               DO 54 INB1 = IZY,IZW
                  ICIT = INB1
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
          IXYY =IZY
C
             DO 56 INB1 = 1,NBLP
C
             IF (IXYY.EQ.IZW+1) GOTO 67
             IF (INB1.EQ.IPICB(IXYY)) GOTO 9058
             GOTO 55
 9058        CONTINUE
             ICIT = IXYY
             IXYY = IXYY + 1
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
   55       CALL ADVANC(IBCON1,NBT,NORB)
   56       CONTINUE
C
   67       CONTINUE
C
C
C
C
C
C
C   Loop over Beta dets now, we have to form the
C   simultaneous alpha and beta single excitations.
C
         DO 40 I=1,NBT
            IBCON1(I) = I
   40    CONTINUE
C
C ***
C  Loop over Beta dets in ipicb(izy -> izw)
C
      NZX = 1
      NZY = IZY
 6613 CONTINUE
      DO 5510 KK=NZX,IPICB(NZY)-1
         CALL ADVANC(IBCON1,NBT,NORB)
 5510 CONTINUE
          IC1 = NZY
C
C  Find where to start looking for beta excited
C
          IKT = 0
          DO 665 II=1,IAC
             DO 778 KK=IPOSA(II),IPOSA1(II)
                IF (IPICB(KK).GT.IPICB(NZY)) GOTO 449
  778        CONTINUE
             ITEST(II) = 0
             IKT = IKT + 1
             GOTO 665
  449        ITEST(II) = KK
  665     CONTINUE
          IF (IKT.EQ.IAC) GOTO 913
C
C   Beta first *********************** Single excits
C
          DO 900 IB=NCOR+1,NBT
             IBB = IBCON1(IB)
CSymmetry of deoccupied orbital
             IB1 = IOX(IBB)
             IST = IBB+1
             IEN = IBCON1(IB+1)-1
C
          IF (IB.EQ.NBT) IEN = NORB
          DO 895 KKJ=IB-NCOR+1,NB+1
                DO 890 JJ=IST,IEN
CSymmetry of occupied orbital
             IB2 = IOX(JJ)
             IV1 = IGMUL(IB1,IB2)
C
               CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
                   IPOSB = POSDET(NACT,NB,IACON2,IFA)
                   IPERB = ((-1)**IPER1)
                   IOB = INDEX(IBB,JJ)
C
C   Loop over all single alpha excitations stored
C
                DO 1013 IAT=1,IAC
                   IF (ITEST(IAT).EQ.0) GOTO 1013
                   IF (ISYMX(IAT).NE.IV1) GOTO 1013
C
                   IND = IIND1(IAT)
                   IX = INDEX(IND,IOB)
C                      C = si2(ix)*iperb*ipera(iat)
C
C  See if the alpha excit matches with beta single excit.
C
                   DO 1011 KK=ITEST(IAT),IPOSA1(IAT)
                      IF (IPICB(KK).EQ.IPOSB) GOTO 1007
 1011              CONTINUE
                   GOTO 1013
C
 1007              CONTINUE
              FC = 0.0D+00
              DO 319 KKI=1,NSTATE
             FC = FC + W(KKI)*CI(IC1,IWH(KKI))*CI(KK,IWH(KKI))
  319        CONTINUE
            FC = FC * IPERB*IPERA(IAT)
            DEN2(IX) = DEN2(IX) + FC
C
 1013           CONTINUE
C
  890           CONTINUE
                IST = IBCON1(KKJ+NCOR)+1
                IEN = IBCON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NB) IEN=NORB
  895       CONTINUE
  900     CONTINUE
C
  913 CONTINUE
C
C   end of <ab|H|a'b'> elements
C
C  Now for the <a'b|H|ab'>
C
          IKT = 0
          DO 765 II=1,IAC
             DO 878 KK=IPOSA1(II),IPOSA(II),-1
                IF (IPICB(KK).LT.IPICB(NZY)) GOTO 749
  878        CONTINUE
             ITEST(II) = 0
             IKT = IKT + 1
             GOTO 765
  749        ITEST(II) = KK
  765     CONTINUE
          IF (IKT.EQ.IAC) GOTO 613
C          do 765 ii=1,iac
C             itest(ii) = iposa(ii)
C  765 continue
C
          DO 7900 IB=NCOR+1,NBT
             IBB = IBCON1(IB)
CSymmetry of deoccupied orbital
             IB1 = IOX(IBB)
             IST = NCOR+1
             IEN = IBCON1(NCOR+1)-1
C
             DO 7895 KKJ=1,IB
                DO 7890 JJ=IST,IEN
CSymmetry of occupied orbital
             IB2 = IOX(JJ)
             IV1 = IGMUL(IB1,IB2)
C
               CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
                   IPOSB = POSDET(NACT,NB,IACON2,IFA)
                   IPERB = ((-1)**IPER1)
                   IOB = INDEX(IBB,JJ)
C
C   Loop over all single alpha excitations stored
C
                DO 6013 IAT=1,IAC
                   IF (ITEST(IAT).EQ.0) GOTO 6013
                   IF (ISYMX(IAT).NE.IV1) GOTO 6013
C
                   IND = IIND1(IAT)
                   IX = INDEX(IND,IOB)
C                      C = si2(ix)*iperb*ipera(iat)
C
C  See if the alpha excit matches with beta single excit.
C
                   DO 7011 KK=ITEST(IAT),IPOSA(IAT),-1
                      IF (IPICB(KK).EQ.IPOSB) GOTO 7007
 7011              CONTINUE
                   GOTO 6013
C
 7007              CONTINUE
                 FC = 0.0D+00
                 DO 321 KKI=1,NSTATE
                 FC = FC + W(KKI)*CI(IC1,IWH(KKI))*CI(KK,IWH(KKI))
  321            CONTINUE
                FC = FC *IPERB*IPERA(IAT)
                DEN2(IX) = DEN2(IX)+FC
 6013           CONTINUE
C
 7890           CONTINUE
                IST = IBCON1(KKJ+NCOR)+1
                IEN = IBCON1(NCOR+KKJ+1)-1
                IF (KKJ.EQ.NB) IEN=NORB
 7895       CONTINUE
 7900     CONTINUE
C
  613 CONTINUE
C
      NZX = IPICB(NZY)
      NZY = NZY + 1
      IF (NZY.NE.IZW+1) GOTO 6613
C
C  End of the Beta loop for <ab|H|a'b'>, <ab'|H|a'b> elements  ^
C
      IZX = IPICA(IZW)
      IZY = IZW + 1
      IF (IZW.NE.NPIC) GOTO 9013
C
C  End of the original alpha loop ^
C
C   Now for the Beta part
C
      DO 876 JJI=1,NBT
         IBCON1(JJI) = JJI
  876 CONTINUE
C
C  Big loop over all beta determinants
C
      IZX = 1
      IZY = 1
 5013 CONTINUE
      DO 3010 KK=IZX,JPICB(IZY)-1
         CALL ADVANC(IBCON1,NBT,NORB)
 3010 CONTINUE
      DO 3013 IZW=IZY,NPIC
         IF (JPICB(IZY).NE.JPICB(IZW)) GOTO 3015
 3013 CONTINUE
 3015 IZW = IZW - 1
C
C  Now we have indices in jpicb from izy -> izw
C  and the alpha determinants are in increasing
C  order jpica(izy) -> jpica(izw) *****
C
C   Single Beta excitations
C
      DO 6030 IB=NCOR+1,NBT
         IO1 = IBCON1(IB)
CSymmetry of orbital being deoccupied
         IS1 = IOX(IO1)
         IST = IO1+1
         IEN = IBCON1(IB+1)-1
         IF (IB.EQ.NBT) IEN=NORB
         DO 6025 KKJ=IB-NCOR+1,NB+1
            DO 6020 JJ=IST,IEN
CSymmetry of orbital being occupied
         IS2 = IOX(JJ)
Cis1xis2 = ip1
         IP1 = IGMUL(IS2,IS1)
C
            CALL RET1DET(IBCON1,IACON2,NB,IB,JJ,NCOR,KKJ,IPER1)
            IPER = ((-1)**IPER1)*2
            IND = INDEX(JJ,IO1)
C      If deoccupied and newly occupied are of different symmetry,
C      skip to doubles
            IF (IS1.NE.IS2) GOTO 313
            IPB1 = POSDET(NACT,NB,IACON2,IFA)
C
C  ***
C Check to see if this excited beta determinant is in jpicb
C
            DO 3018 KK=IZW+1,NPIC
               IF (JPICB(KK).EQ.IPB1) GOTO 3021
 3018       CONTINUE
            GOTO 313
 3021       JZY = KK
C
C  Check to see how many of the ipb1 betas there are
C
            DO 3040 KK=JZY,NPIC
               IF (JPICB(KK).NE.IPB1) GOTO 3034
 3040       CONTINUE
 3034       JZW = KK - 1
C
C  Now the excited betas lie in jpicb(jzy -> jzw)
C
C  Now to see if any of the alpha determinants match up.
C
            NPCA = 0
            ISY = JZY
            DO 3051 KK1=IZY,IZW
               DO 3053 KK2=ISY,JZW
                  IF (JPICA(KK1).EQ.JPICA(KK2)) THEN
                     NPCA = NPCA + 1
                     LPICB1(NPCA) = KK1
                     LPICB2(NPCA) = KK2
                     ISY = KK2 + 1
                     GOTO 3051
                  ENDIF
 3053          CONTINUE
 3051       CONTINUE
            IF (NPCA.EQ.0) GOTO 313
C
C   Now I know that there are npca determinants that match up
C   and that they are the determinants given by
C      jpica(lpicb1(i)), jpicb(lpicb1(i))
C and  jpica(lpicb2(i)), jpicb(lpicb2(i)),i=1,npca
C  kposj(lpicb1(i)) and kposj(lpicb2(i)) are actually the
C  position of the CI coefficients.
C
C
C    C = si1(ind)
          DO 507 INA1=1,NPCA
             ICIT = KPOSJ(LPICB1(INA1))
             ICI2 = KPOSJ(LPICB2(INA1))
             FC = 0.0D+00
             DO 509 KKI=1,NSTATE
        FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICI2,IWH(KKI))
  509      CONTINUE
          FC = FC * IPER
          DEN(IND) = DEN(IND)+FC
  507 CONTINUE
C
            DO 912 IK=1,NBT
               IF (IK.EQ.IB) GOTO 912
               ION = IBCON1(IK)
               J1 = INDEX(ION,ION)
               JJ1 = INDEX(IND,J1)
               J1 = INDEX(ION,JJ)
               J2 = INDEX(ION,IO1)
               INX = INDEX(J1,J2)
              DO 591 INAA=1,NPCA
                 ICIT = KPOSJ(LPICB1(INAA))
                 ICI2 = KPOSJ(LPICB2(INAA))
                 FC = 0.0D+00
             DO 691 KKI=1,NSTATE
               FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICI2,IWH(KKI))
  691        CONTINUE
           FC = FC * IPER
           DEN2(JJ1) = DEN2(JJ1) + FC
           DEN2(INX) = DEN2(INX) - FC
  591    CONTINUE
  912       CONTINUE
C
       DO 89 I=1,NAT
          IACON1(I) = I
   89 CONTINUE
C
C  Loop over alpha dets, check to see if they equal any
C  of jpica(lpicb1(i)),i=1,npca
C
          IXYY = 1
            DO 920 INA1 = 1,NALP
               IF (IXYY.EQ.NPCA+1) GOTO 313
               IF (INA1.EQ.JPICA(LPICB1(IXYY))) GOTO 3058
               GOTO 914
 3058          CONTINUE
C
               ICIT = KPOSJ(LPICB1(IXYY))
               ICI2 = KPOSJ(LPICB2(IXYY))
               IXYY = IXYY + 1
C
             FC = 0.0D+00
             DO 991 KKI=1,NSTATE
              FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICI2,IWH(KKI))
  991       CONTINUE
         FC = FC * IPER
             DO 690 IK=1,NAT
                ION = IACON1(IK)
                J1 = INDEX(ION,ION)
                JJ1 = INDEX(IND,J1)
               DEN2(JJ1) = DEN2(JJ1) + FC
  690        CONTINUE
C
  914        CALL ADVANC(IACON1,NAT,NORB)
  920    CONTINUE
C
  313 CONTINUE
C
C   Now for Beta double excitations
C
       DO 6015 IBB = IB+1,NBT
               ISTBB = JJ+1
               IENBB = IEN
               JB = IBCON1(IBB)
CSymmetry of orbital being deoccupied
               IS3 = IOX(JB)
               IPB = IBB-NCOR
               IF (JJ.GT.JB) IPB = IPB - 1
               DO 6010 KKJBB = KKJ,NB+1
                  DO 6005 JJBB = ISTBB,IENBB
CSymmetry of orbital being occupied
               IS4 = IOX(JJBB)
Cip2=is4xis4
               IP2 = IGMUL(IS4,IS3)
CIf symmetry of beta'' is not right, skip it
               IF (IP1.NE.IP2) GOTO 6005
C
          CALL RET1DET(IACON2,IACON1,NB,IPB,JJBB-NCOR,0,KKJBB,IPER2)
          IBP2 = POSDET(NACT,NB,IACON1,IFA)
C
C***
C Check to see if this double excited beta determinant
C is in jpicb
C
              DO 2018 KK=IZW+1,NPIC
                 IF (JPICB(KK).EQ.IBP2) GOTO 2021
 2018         CONTINUE
              GOTO 6005
 2021         KZY = KK
C
C Check to see how many of them there are
C
              DO 2040 KK=KZY,NPIC
                 IF (JPICB(KK).NE.IBP2) GOTO 2034
 2040         CONTINUE
 2034         KZW = KK - 1
C
C Now the doubly excited betas lie in jpicb(kzy -> kzw)
C
C   Now to see if any of the alpha determinants match up
C
              NPCA = 0
              ISY = KZY
              DO 2051 KK1=IZY,IZW
                 DO 2053 KK2 = ISY,KZW
                    IF (JPICA(KK1).EQ.JPICA(KK2)) THEN
                       NPCA = NPCA + 1
                       LPICB1(NPCA) = KK1
                       LPICB2(NPCA) = KK2
                       ISY = KK2 + 1
                       GOTO 2051
                    ENDIF
 2053            CONTINUE
 2051         CONTINUE
              IF (NPCA.EQ.0) GOTO 6005
C
C  Now I know that there are npca determinants which
C  match up and that they are the determinants given
C  by jpica(lpicb1(i)), jpicb(lpicb1(i))
C and jpica(lpicb2(i)), jpicb(lpicb2(i)),i=1,npca
C kposj(lpicb1(i)), kposj(lpicb2(i)) are actually the
C positions of the CI coefficients.
C
          IPER = IPER1+IPER2
          IPER = ((-1)**IPER)*2
               IND = INDEX(JJ,IO1)
               I2 = INDEX(JB,JJBB)
               INX = INDEX(I2,IND)
               II1 = INDEX(JJBB,IO1)
               II2 = INDEX(JB,JJ)
               INX2 = INDEX(II1,II2)
C
             DO 686 INA1 = 1,NPCA
             ICIT = KPOSJ(LPICB1(INA1))
             ICI2 = KPOSJ(LPICB2(INA1))
             FC = 0.0D+00
            DO 219 KKI=1,NSTATE
              FC = FC + W(KKI)*CI(ICIT,IWH(KKI))*CI(ICI2,IWH(KKI))
  219       CONTINUE
          FC = FC * IPER
          DEN2(INX) = DEN2(INX) + FC
           DEN2(INX2) = DEN2(INX2) - FC
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
C    Remaining part of diagonal contributions
C
            DO 69 II=1,NBT
               I1 = IBCON1(II)
               IND1 = INDEX(I1,I1)
            DO 93 INA1 = IZY,IZW
              ICIT = KPOSJ(INA1)
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
            DO 97 INA1 = IZY,IZW
               ICIT = KPOSJ(INA1)
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
C
      IZX = JPICB(IZW)
      IZY = IZW + 1
      IF (IZW.NE.NPIC) GOTO 5013
C
C  End of the Beta loop ^
C
C   Now for the diagonal contributions
C
C
      RETURN
      END
C
C*MODULE ALGNCI  *DECK GCIGEN
      SUBROUTINE GCIGEN(IW,SOME)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME
C
      PARAMETER (MXRT=100)
C
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,S,SZ,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORB,
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NGCI,
     *                IGPDET,KSTSYM,NFTGCI
      COMMON /IOFILE/ IR,IZ,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /FMCOM / X(1)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
C
      NSYM = 2**IGPDET
C
      IF (SOME) WRITE(IW,9000)
C
      CALL SEQREW(IR)
C
      IEOF = 0
      CALL FNDGRP(IR,' $GCILST',IEOF)
      IF (IEOF.EQ.1) THEN
         IF (SOME) WRITE(IW,9010)
         CALL ABRT
      ENDIF
C
C  Read in how many Space functions to be read in
C
      CALL RDCARD('$GCILST ',IEOF)
      IF (IEOF.EQ.1) THEN
         IF (SOME) WRITE(IW,9030)
         CALL ABRT
      ENDIF
C
      LSTSZE = IFIND('LSTSZE  ',IEOF)
      IF (IEOF.EQ.1) THEN
         IF (SOME) WRITE(IW,9025)
         CALL ABRT
      ENDIF
      IF (SOME) WRITE(IW,9020) LSTSZE
      ISYMO = IFIND('ISYMO   ',IEOF)
      IF (ISYMO.EQ.0) THEN
         IF (SOME) WRITE(IW,9026)
      ELSEIF(ISYMO.EQ.1) THEN
         IF (SOME) WRITE(IW,9027)
      ELSEIF(ISYMO.EQ.2) THEN
         IF (SOME) WRITE(IW,9027)
         IF (SOME) WRITE(IW,9028)
      ENDIF
      IF (SOME) WRITE(IW,9029)
C
C
C  Determine memory and addresses for scratch storage
C
      CALL VALFM(LOADFM)
      IPICA  = LOADFM + 1
      IPICB  = IPICA + LSTSZE/NWDVAR + 1
      ICONA  = IPICB + LSTSZE/NWDVAR + 1
      ICONB  = ICONA + NA
      LIFA   = ICONB + NB
      ISPAC  = LIFA   + ((NACT+1)*(NACT+1))/NWDVAR + 1
      IBO    = ISPAC + NACT
      IGMUL  = IBO + NORB
      IWRK   = IGMUL + NSYM*NSYM
      NPSY   = IWRK + 43
      LAST   = NPSY + NSYM
      NEED1 = LAST - LOADFM - 1
      CALL GETFM(NEED1)
C
C  Binomial coefficients, orbital symmetries, multiplication table.
C
      CALL BINOM6(X(LIFA),NACT)
      CALL DAREAD(IDAF,IODA,X(IBO),NORB,262,1)
      CALL CORTRA(X(IBO),NORB,NCOR)
      CALL GMUL(IGPDET,X(IGMUL),X(IWRK),X(IWRK+3),X(IWRK+6),X(IWRK+9))
C
C  Now to read the space products.
C
      NGCI = 0
      CALL RSPACE(SOME,IW,LSTSZE,X(IPICA),X(IPICB),X(ICONA),
     *            X(ICONB),X(LIFA),X(ISPAC),X(IBO),X(IGMUL),X(IWRK),
     *            X(NPSY),NACT,NA,NB,IGPDET,NSYM,KSTSYM,NGCI,ISYMO)
C
C  Write the determinant list to GCILIST
C
      CALL SEQOPN(NFTGCI,'GCILIST','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQREW(NFTGCI)
      CALL SQWINT(NFTGCI,NGCI,1)
      CALL SQWINT(NFTGCI,X(IPICA),NGCI)
      CALL SQWINT(NFTGCI,X(IPICB),NGCI)
      CALL SEQCLO(NFTGCI,'KEEP')
C
      IF (SOME) WRITE(IW,9040) NGCI
      IF (SOME) WRITE(IW,9050)
C
      CALL RETFM(NEED1)
C
      RETURN
 9000 FORMAT(/5X,50("-")/
     *       5X,' READING GENERAL CI LIST FROM $GCILST GROUP '/
     *       5X,50(1H-))
 9010 FORMAT(/1X,'***** ERROR IN -GCIGEN- ROUTINE *****'/
     *       1X,'INPUT GCI LIST SPECIFIED BUT NO $GCILST GROUP FOUND.')
 9020 FORMAT(/1X,'NUMBER OF SPACE PRODUCTS TO BE READ IN = ',I6)
 9025 FORMAT(/1X,'***** ERROR IN -GCIGEN- ROUTINE *****'/
     *       1X,'CANNOT DECIPHER NUMBER OF SPACE PRODUCTS TO BE READ.')
 9026 FORMAT(/1X,'WILL CHECK THAT SPACE PRODUCTS HAVE CORRECT IRREP.')
 9027 FORMAT(/1X,'WILL KEEP ALL SPACE PRODUCTS REGARDLESS OF IRREP.')
 9028 FORMAT(/1X,'WILL PRINT OUT IRREPS OF ALL SPACE PRODUCTS')
 9029 FORMAT(/1X,'*** NOTE THAT IRREP NUMBERS BELOW CORRESPOND'/
     *       1X,'WITH DETERMINANT CODE LABELLING')
 9030 FORMAT(/1X,'***** ERROR IN -GCIGEN- ROUTINE *****'/
     *       1X,'END OF FILE PREMATURELY HIT IN $GCILIST GROUP.')
 9040 FORMAT(/1X,'NUMBER OF DETERMINANTS WRITTEN TO DISK = ',I6)
 9050 FORMAT(/1X,'THESE WILL BE SPIN COMPLEMENTED LATER.')
      END
C
C*MODULE ALGNCI  *DECK RSPACE
      SUBROUTINE RSPACE(SOME,IW,LSTSZE,IPICA,IPICB,ICONA,ICONB,
     *                  IFA,ISPAC,IBO,IGMUL,IWRK,NPSY,NACT,NA,NB,
     *                  IGPDET,NSYM,KSTSYM,NGCI,ISYMO)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,SOME
C
      INTEGER POSDET
C
      DIMENSION IPICA(LSTSZE),IPICB(LSTSZE)
      DIMENSION ICONA(NA),ICONB(NB)
      DIMENSION IFA(0:NACT,0:NACT)
      DIMENSION ISPAC(NACT),IBO(NACT),IGMUL(NSYM,NSYM)
      DIMENSION IWRK(43),NPSY(NSYM)
C
      COMMON /DONASK/ NEXTRA
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      DO 13 II=1,NSYM
         NPSY(II)=0
   13 CONTINUE
C
      IF (ISYMO.EQ.2  .AND.  SOME) WRITE(IW,9002)
      IEOF = 0
C
      DO 9999 ISFTZ=1,LSTSZE
C
      CALL RDCARD('SPACEF  ',IEOF)
      IF (IEOF.EQ.1) THEN
         IF(MASWRK) WRITE(IW,9005)
         CALL ABRT
      ENDIF
C
C Read space product
C
      DO 113 II=1,NACT
         ISPAC(II) = IFIND('ISPAC   ',IEOF)
         IF (IEOF.EQ.1) THEN
            IF(MASWRK) WRITE(IW,9010) ISFTZ
            CALL ABRT
         ENDIF
  113 CONTINUE
C
C  Check no occupation is more than 2 or less than 0
C  and that no. of electrons is okay.
C
      NOEL = 0
      NODB = 0
      DO 120 II=1,NACT
         IF (ISPAC(II).GT.2.OR.ISPAC(II).LT.0) THEN
            IF(MASWRK) WRITE(IW,9020) ISFTZ,(ISPAC(I),I=1,NACT)
            CALL ABRT
         ENDIF
         NOEL = NOEL + ISPAC(II)
         IF (ISPAC(II).EQ.2) NODB = NODB + 1
  120 CONTINUE
C
      IF (NOEL.NE.(NA+NB)) THEN
         IF(MASWRK) WRITE(IW,9030) ISFTZ,(ISPAC(I),I=1,NACT)
         CALL ABRT
      ENDIF
C
C  Make the first logical determinant, checking to see if one
C  of the proper ms may be constructed.
C
      IF (NODB.GT.NB) THEN
         IF(MASWRK) WRITE(IW,9040) ISFTZ,(ISPAC(I),I=1,NACT)
         CALL ABRT
      ENDIF
C
      NASI = NA - NODB
      NPSI = NASI
      NBSI = NB - NODB
C
      NAOC = 1
      NBOC = 1
      DO 130 II=1,NACT
         IF (ISPAC(II).EQ.0) GOTO 130
         IF (ISPAC(II).EQ.2) THEN
            ICONA(NAOC) = II
            ICONB(NBOC) = II
            NAOC = NAOC + 1
            NBOC = NBOC + 1
            GOTO 130
         ENDIF
         IF (NASI.GT.0) THEN
            ICONA(NAOC) = II
            NAOC = NAOC + 1
            NASI = NASI - 1
            GOTO 130
         ENDIF
         ICONB(NBOC) = II
         NBOC = NBOC + 1
  130 CONTINUE
C
C  Check to see if it is of the right irrep, ie same as specified kstsym
C
      CALL GETSYM1(IW,ICONA,NACT,NA,IBO,IGPDET,ISYMA,
     *   IWRK(1),IWRK(4),IWRK(7),IWRK(10))
C
      CALL GETSYM1(IW,ICONB,NACT,NB,IBO,IGPDET,ISYMB,
     *   IWRK(1),IWRK(4),IWRK(7),IWRK(10))
      ISYMT = IGMUL(ISYMA,ISYMB)
C
      IF (ISYMO.EQ.0.AND.ISYMT.NE.KSTSYM) THEN
         IF(MASWRK) WRITE(IW,9050) ISFTZ,(ISPAC(I),I=1,NACT)
         IF(MASWRK) WRITE(IW,9055) ISYMT,KSTSYM
         CALL ABRT
      ENDIF
C
C  Space function has passed all the tests, now to see
C  if it has occurred before.
C
      IAST = POSDET(NACT,NA,ICONA,IFA)
      IBST = POSDET(NACT,NB,ICONB,IFA)
C
      DO 213 II=1,NGCI
         IF (IAST.EQ.IPICA(II).AND.IBST.EQ.IPICB(II)) THEN
            IF(SOME) WRITE(IW,9060) ISFTZ,(ISPAC(I),I=1,NACT)
            IF(SOME) WRITE(IW,9065)
            GOTO 9999
         ENDIF
  213 CONTINUE
C
C  Space function is unique, add it to the list.
C
      NGCI = NGCI + 1
      IPICA(NGCI) = IAST
      IPICB(NGCI) = IBST
      NPSY(ISYMT) = NPSY(ISYMT) + 1
C
C  Update variable NEXTRA, don't even ask.
C
      NODE = IFA(NPSI+NBSI,NPSI)
      IF (NODE.GT.NEXTRA) NEXTRA=NODE
C
      IF (ISYMO.EQ.2  .AND.  SOME) WRITE(IW,'(I4,I15)') ISFTZ,ISYMT
C
 9999 CONTINUE
C
C  Print out how many unique space products there were.
C
      IF(SOME) WRITE(IW,9070) NGCI
      IF (ISYMO.NE.0  .AND.  SOME) WRITE(IW,9080) (NPSY(I),I=1,NSYM)
C
      RETURN
C
 9002 FORMAT(/1X,'SPACE PRODUCT      IRREP             ')
 9005 FORMAT(/1X,'***** ERROR IN -GCIGEN- ROUTINE *****'/
     *       1X,'END OF FILE PREMATURELY HIT IN $GCILIST GROUP.')
 9010 FORMAT(/1X,'***** ERROR IN -RSPACE- ROUTINE *****'/
     *       1X,'ERROR READING SPACE FUNCTION',I6/)
 9020 FORMAT(/1X,'***** ERROR IN -RSPACE- ROUTINE *****'/
     *       1X,'ORBITAL OCCUPATION NOT CORRECT IN PRODUCT',I6/
     *       1X,30I2)
 9030 FORMAT(/1X,'***** ERROR IN -RSPACE- ROUTINE *****'/
     *       1X,'NO OF ELECTRONS NOT CORRECT IN PRODUCT',I6/
     *       1X,30I2)
 9040 FORMAT(/1X,'***** ERROR IN -RSPACE- ROUTINE *****'/
     *       1X,'MS IS NOT CORRECT IN PRODUCT',I6/
     *       1X,30I2)
 9050 FORMAT(/1X,'***** ERROR IN -RSPACE- ROUTINE *****'/
     *       1X,'INCORRECT IRREP FOR SPACE PRODUCT',I6/
     *       1X,30I2)
 9055 FORMAT(1X,'IRREP OF SPACE PRODUCT = ',I2/
     *       1X,'KSTSYM                 = ',I2)
 9060 FORMAT(/1X,'IGNORING PRODUCT  ',I2/
     *       1X,30I2)
 9065 FORMAT(1X,'THIS PRODUCT HAS BEEN FOUND EARLIER IN LIST.')
 9070 FORMAT(/1X,'NO OF UNIQUE SPACE PRODUCTS READ = ',I6)
 9080 FORMAT(/1X,'NO OF SPACE PRODUCTS IN EACH IRREP.'/
     *       1X,8I5)
C
      END
C
C*MODULE ALGNCI  *DECK GCISYM
      SUBROUTINE GCISYM(IW,CI,IPICA,IPICB,IACON,IBCON,IGMUL,NSYM,
     *                  IWRK,IBO,JSTSYM)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXRT=100)
C
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,S,SZ,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORB,
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NGCI,
     *                IGPDET,KSTSYM,NFTGCI
C
      DIMENSION CI(NGCI)
      DIMENSION IPICA(NGCI),IPICB(NGCI),IACON(NA),IBCON(NB)
      DIMENSION IGMUL(NSYM,NSYM),IWRK(43)
      DIMENSION IBO(NACT)
C
      PMAX = 0.0D-100
      DO 100 II=1,NGCI
         IF (ABS(CI(II)).GE.PMAX) THEN
            IND = II
            PMAX = ABS(CI(II))
         ENDIF
  100 CONTINUE
C
      INDA = IPICA(IND)
      INDB = IPICB(IND)
      DO 120 II=1,NA
         IACON(II) = II
  120 CONTINUE
      DO 130 II=1,NB
         IBCON(II) = II
  130 CONTINUE
C
      DO 200 II=1,INDA-1
         CALL ADVANC(IACON,NA,NACT)
  200 CONTINUE
      DO 220 II=1,INDB-1
         CALL ADVANC(IBCON,NB,NACT)
  220 CONTINUE
C
      CALL GETSYM1(IW,IACON,NACT,NA,IBO,IGPDET,ISYMA,
     *   IWRK(1),IWRK(4),IWRK(7),IWRK(10))
      CALL GETSYM1(IW,IBCON,NACT,NB,IBO,IGPDET,ISYMB,
     *   IWRK(1),IWRK(4),IWRK(7),IWRK(10))
      JSTSYM = IGMUL(ISYMA,ISYMB)
C
      RETURN
      END
C
C*MODULE ALGNCI  *DECK GCICASG
      SUBROUTINE GCICASG(IW,SOME,IRREPS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME
C
      PARAMETER (MXRT=100)
      DIMENSION IRREPS(8),ISTAT(8)
C
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,S,SZ,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORB,
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NGCI,
     *                IGPDET,KSTSYM,NFTGCI
      COMMON /FMCOM / X(1)
      COMMON /IOFILE/ IR,IZ,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
C
C     generate one CAS determinant list for each irreducible
C     representation in IRREPS, then write out the total list.
C
      NSYM = 2**IGPDET
C
      IF (SOME) WRITE(IW,9000)
C
      NIRREPS=0
      DO I=1,8
         IF(IRREPS(I).NE.0) NIRREPS=NIRREPS+1
      ENDDO
      CALL VICLR(ISTAT,1,8)
C
C  Determine memory and addresses for scratch storage
C
      CALL VALFM(LOADFM)
      LIFA   = LOADFM + 1
      LAST  = LIFA   + ((NACT+1)*(NACT+1)-1)/NWDVAR + 1
      NEED1 = LAST - LOADFM - 1
      CALL GETFM(NEED1)
C
C  Binomial coefficients, orbital symmetries, multiplication table.
C
      CALL BINOM6(X(LIFA),NACT)
      NALP = IXFTCH(X(LIFA),NACT+1+NA*(NACT+1))
      NBLP = IXFTCH(X(LIFA),NACT+1+NB*(NACT+1))
C
C  Determine memory and addresses for scratch storage
C
      CALL VALFM(LOADFM)
      LSYMA  = LOADFM + 1
      LSYMB  = LSYMA + (NALP-1)/NWDVAR + 1
      IPICA  = LSYMB + (NBLP-1)/NWDVAR + 1
      IPICB  = IPICA + (NALP*NBLP-1)/NWDVAR + 1
      IBO    = IPICB + (NALP*NBLP-1)/NWDVAR + 1
      IGMUL  = IBO + NORB
      IWRK   = IGMUL + NSYM*NSYM
      ICON   = IWRK + 43
      LAST   = ICON + NA
      NEED2  = LAST - LOADFM - 1
      CALL GETFM(NEED2)
C
      CALL DAREAD(IDAF,IODA,X(IBO),NORB,262,1)
      CALL CORTRA(X(IBO),NORB,NCOR)
      CALL GMUL(IGPDET,X(IGMUL),X(IWRK),X(IWRK+3),X(IWRK+6),X(IWRK+9))
C
      DO 23 II=1,NA
         CALL IXSTOR(X(ICON),II,II)
   23 CONTINUE
C
      DO 53 IA=1,NALP
         CALL GETSYM1(IW,X(ICON),NACT,NA,X(IBO),IGPDET,ISYM,
     *                X(IWRK),X(IWRK+3),X(IWRK+6),X(IWRK+9))
         CALL IXSTOR(X(LSYMA),IA,ISYM)
         CALL ADVANC(X(ICON),NA,NACT)
   53 CONTINUE
      DO 33 II=1,NB
         CALL IXSTOR(X(ICON),II,II)
   33 CONTINUE
      DO 43 IB=1,NBLP
         CALL GETSYM1(IW,X(ICON),NACT,NB,X(IBO),IGPDET,ISYM,
     *                X(IWRK),X(IWRK+3),X(IWRK+6),X(IWRK+9))
         CALL IXSTOR(X(LSYMB),IB,ISYM)
         CALL ADVANC(X(ICON),NB,NACT)
   43 CONTINUE
      NGCI=0
      DO IA=1,NALP
         ISYMA=IXFTCH(X(LSYMA),IA)
         DO IB=1,NBLP
            ISYMB=IXFTCH(X(LSYMB),IB)
            ISYMAB=IXFTCH(X(IGMUL),ISYMA+(ISYMB-1)*NSYM)
            DO I=1,NIRREPS
               IRREP=IRREPS(I)
               IF(ISYMAB.EQ.IRREP) THEN
                  NGCI=NGCI+1
                  CALL IXSTOR(X(IPICA),NGCI,IA)
                  CALL IXSTOR(X(IPICB),NGCI,IB)
                  ISTAT(IRREP)=ISTAT(IRREP)+1
               ENDIF
            ENDDO
         ENDDO
      ENDDO
C
C  Write the determinant list to GCILIST
C
      CALL SEQOPN(NFTGCI,'GCILIST','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQREW(NFTGCI)
      CALL SQWINT(NFTGCI,NGCI,1)
      CALL SQWINT(NFTGCI,X(IPICA),NGCI)
      CALL SQWINT(NFTGCI,X(IPICB),NGCI)
      CALL SEQCLO(NFTGCI,'KEEP')
C
      DO I=1,NIRREPS
         IF(SOME) WRITE(IW,9040) IRREPS(I),ISTAT(IRREPS(I))
      ENDDO
      IF (SOME) WRITE(IW,9045) NGCI
      IF (SOME) WRITE(IW,9050)
C
      CALL RETFM(NEED2)
      CALL RETFM(NEED1)
C
      RETURN
 9000 FORMAT(/5X,45("-")/
     *       5X,' CAS MULTI-IRREP DETERMINANT LIST GENERATION.'/
     *       5X,45(1H-))
 9040 FORMAT( 1X,'NUMBER OF DETERMINANTS OF SYMMETRY',I2,' IS',I9)
 9045 FORMAT(/1X,'TOTAL NUMBER OF DETERMINANTS = ',I6)
 9050 FORMAT(/1X,'THEY WILL BE SPIN COMPLEMENTED LATER.')
      END
C*MODULE ALDECI  *DECK ECORRG
C     -----------------------------------------------------------
      SUBROUTINE ECORRG(IW,NFT12,
     *                  EL,ECONST,EHF,CI,NCI,K,SI1,SI2,NACT,NA,NB,
     *                  IFA,INDEX,IACON1,IACON2,IBCON1,IBCON2,
     *                  IMUL,NSYM,IOX,
     *                  IPICA,IPICB,JPICA,JPICB,KPOSJ)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXRT=100)
      LOGICAL GOPARR,DSKWRK,MASWRK
C
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
      DIMENSION IMUL(NSYM,NSYM)
      DIMENSION IOX(NACT)
      DIMENSION IPICA(NCI),IPICB(NCI),JPICA(NCI),JPICB(NCI)
      DIMENSION KPOSJ(NCI)
      INTEGER POSDET
C
      CHARACTER*30 CONA,CONB
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
      CALL GCIPRT(IW,NFT12,.TRUE.)
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
      IF (IPICA(1).NE.1.AND.IPICB(1).NE.1) GOTO 111
      GOTO 222
C
C   IF THERE IS AN ERROR ABOVE WE END UP HERE!!!!!!
  111 CONTINUE
      IF(MASWRK) WRITE(IW,7040)
      RETURN
C
C   IF FOUND 1ST DETERMINANT WE END UP HERE, PRINT IT.
  222 CONTINUE
C
      CONA(1:30) = ' 00000000000000000000000000000'
      CONB(1:30) = ' 00000000000000000000000000000'
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
         WRITE(IW,'(4A,F10.7)') CONA(1:NACT+2),'|',
     *                          CONB(1:NACT+2),'|  ',CI(1,1)
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
C     SET UP FOR STRING SEARCHING DATA
C
      IEN = 1
      DO II=1,NCI
         IF (JPICB(II).NE.1) GOTO 810
      ENDDO
  810 CONTINUE
      IEN = II-1
C
C SINGLE ALPHA EXCITES IOC -> IVI
C
      DO 200 IOC=1,NA
         IS1 = IOX(IOC)
         DO 190 IVI=NA+1,NACT
            IS2 = IOX(IVI)
            IP1 = IMUL(IS2,IS1)
            CALL RET1DET(IACON1,IACON2,NA,IOC,IVI,0,NA+1,IPER1)
            IPETA1 = POSDET(NACT,NA,IACON2,IFA)
            IPERA = ((-1)**IPER1)
            IND = INDEX(IOC,IVI)
C
            IF (IS1.NE.IS2) GOTO 180
C
C   SEE IF EXCITED ALPHA OCCURS WITH SCF BETA.
C
            DO II=1,IEN
               IF (JPICA(II).GE.IPETA1) GOTO 710
            ENDDO
            GOTO 180
  710       CONTINUE
            IF (JPICA(II).GT.IPETA1) GOTO 180
            ICI2 = KPOSJ(II)
C
C  WORK OUT PARTS OF H(0,S) AND USE.
C
            RC = IPERA*CI(ICI2,1)
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
             IPETA2 = POSDET(NACT,NA,IBCON1,IFA)
             IPERT = IPER1+IPER2
             IPERT = ((-1)**IPERT)
C
C   SEE IF EXCITED ALPHA OCCURS WITH SCF BETA.
C
            DO II=1,IEN
               IF (JPICA(II).GE.IPETA2) GOTO 720
            ENDDO
            GOTO 150
  720       CONTINUE
            IF (JPICA(II).GT.IPETA2) GOTO 150
            ICI2 = KPOSJ(II)
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
C  FIRST SEE WHERE/IF ALPHA STRING IPETA1 STARTS/OCCURS
C  AND ENDS IN IPICA.
C
            DO II=1,NCI
               IF (IPICA(II).GE.IPETA1) GOTO 830
            ENDDO
            GOTO 190
  830       CONTINUE
            IF (IPICA(II).GT.IPETA1) GOTO 190
C
            ISTA1 = II
            DO II=ISTA1+1,NCI
               IF (IPICA(II).GT.IPETA1) GOTO 840
            ENDDO
  840       CONTINUE
            IEND1 = II-1
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
            IPETB = POSDET(NACT,NB,IBCON2,IFA)
            IPERT = (-1)**(IPER1+IPER2)
C
C  LOOK FROM ISTA1,IEND1 IN IPICB FOR IPETB
C
            DO II=ISTA1,IEND1
               IF (IPICB(II).GE.IPETB) GOTO 850
            ENDDO
            GOTO 130
  850       CONTINUE
            IF (IPICB(II).GT.IPETB) GOTO 130
            ICI2 = II
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
C     SET UP FOR STRING SEARCHING DATA
C
      IEN = 1
      DO II=1,NCI
         IF (IPICA(II).NE.1) GOTO 860
      ENDDO
  860 CONTINUE
      IEN = II-1
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
            IPETB1 = POSDET(NACT,NB,IBCON2,IFA)
            IPERB = ((-1)**IPER1)
            IND = INDEX(IOC,IVI)
C
            IF (IS1.NE.IS2) GOTO 480
C
C   SEE IF EXCITED BETA OCCURS WITH SCF ALPHA.
C
            DO II=1,IEN
               IF (IPICB(II).GE.IPETB1) GOTO 740
            ENDDO
            GOTO 480
  740       CONTINUE
            IF (IPICB(II).GT.IPETB1) GOTO 480
            ICI2 = II
C
C  WORK OUT H(0,S)*C(S)
C
            RC = IPERB*CI(ICI2,1)
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
             IPETB2 = POSDET(NACT,NB,IACON1,IFA)
             IPERT = IPER1+IPER2
             IPERT = ((-1)**IPERT)
C
C   SEE IF EXCITED BETA OCCURS WITH SCF ALPHA.
C
            DO II=1,IEN
               IF (IPICB(II).GE.IPETB2) GOTO 760
            ENDDO
            GOTO 450
  760       CONTINUE
            IF (IPICB(II).GT.IPETB2) GOTO 450
            ICI2 = II
C
                  I2 = INDEX(IOC2,IVI2)
                  INX = INDEX(I2,IND)
                  II1 = INDEX(IVI2,IOC)
                  II2 = INDEX(IOC2,IVI)
                  INX2 = INDEX(II1,II2)
                  C = SI2(INX) - SI2(INX2)
C
                  INZ = LE2B-1+INDEX(IOC,IOC2)
                  X(INZ) = X(INZ) + C*IPERT*CI(ICI2,1)
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
      IF(MASWRK) THEN
         WRITE(IW,7180)
         E2COT = E2COA + E2COB + E2COAB
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
      IF(MASWRK) WRITE(IW,7230)
      CALL PRTRI(X(LE2A),NA)
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
 7040 FORMAT(/1X,'ERROR, 1ST DETERMINANT IN LIST IS NOT SCF-LIKE')
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
     *        1X,'E[DIFF]      = ',F20.10/)
 7250 FORMAT(/1X,'TABLE OF ORBITAL PAIRS, THEIR COULOMB, EXCHANGE ',
     *           'AND CORRELATION CONTRIBUTIONS'//
     *        1X,'PAIR (M,N)',3X,'J [MM|NN]',8X,'K [MN|MN]',
     *        4X,'2-E CORR CONTR',4X,'1-E CORR CONTR'/
     *        1X,75(1H-)/)
 7260 FORMAT(1X,2I3,F16.10,F17.10,F18.10,F18.10)
      END
