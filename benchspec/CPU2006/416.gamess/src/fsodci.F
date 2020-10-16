C  9 dec 03 - mws - synch common block runopt
C 22 May 02 - MWS - FSODCI: additional args for trfmcx
C 26 Mar 02 - MWS,JI - MESOCI: fix bug for small numbers of e-
C 16 feb 02 - JI  - ADD NEW SECOND ORDER CI CODE TO GAMESS
C
C*MODULE FSODCI  *DECK FSODCI
C     ------------------------------
      SUBROUTINE FSODCI(NRNFG,NPFLG)
C     ------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,DDITRF,DOOOOO,DOVOOO,DOVVOO,DOVOVO
C
      DIMENSION NRNFG(10),NPFLG(10)
C
      PARAMETER (MXRT=100)
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,S,SZ,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORB,
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NCI,
     *                IGPDET,KSTSYM,NFTGCI
      COMMON /DESOCI/ OSPIN(MXRT),NEXT,MOSET,MAXPSO,KSO,NSOCI
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CIDET,SODET/8HCIDET   ,8HSODET   /
#else
      CHARACTER*8 :: SODET_STR
      CHARACTER*8 :: CIDET_STR
      EQUIVALENCE (CIDET, CIDET_STR)
      EQUIVALENCE (SODET, SODET_STR)
      DATA CIDET_STR,SODET_STR/"CIDET   ","SODET   "/
#endif
C
C        driver for determinant based Full SOCI calculations...
C
C     This program was written by Joe Ivanic in Jan-Feb, 2002
C     at the Advanced Biomedical Computing Centre, National Cancer
C     Institute-Frederick, Fort Detrick
C
C        ----- read input defining the Full SOCI dimensions -----
C
      NPRINT = -5
      CALL DETINP(NPRINT,CIDET)
      CALL SOCINP(NPFLG(1),SODET)
C
C        ----- integral transformation -----
C
      DDITRF=GOPARR
      DOOOOO=.TRUE.
      DOVOOO=.FALSE.
      DOVVOO=.FALSE.
      DOVOVO=.FALSE.
      NSOTOT = NEXT + NORB
      CALL TRFMCX(NPFLG(2),NCOR,NSOTOT,NSOTOT,.FALSE.,.FALSE.,
     *            DDITRF,DOOOOO,DOVOOO,DOVVOO,DOVOVO)
C
C        ----- direct full CI calculation within NACT orbitals -----
C           make sure any CI vector file from before MCSCF orbital
C           canonicalization will not be used as a starting guess.
C
      CALL SEQOPN(NFT12,'CIVECTR','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQCLO(NFT12,'DELETE')
      CALL DETFCI(NPFLG(3),.FALSE.,DDITRF)
C
C        ----- direct Second Order CI calculation ---------
C        ---------within NACT -> NEXT orbitals -----
C
      CALL FSOCIW(NPFLG(3),.FALSE.,NRNFG,NPFLG(5),DDITRF)
C
C        ----- 1e- density matrix and natural orbitals -----
C
      IF(NRNFG(5).GT.0) CALL DETSODM(NPFLG(5))
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK SOCINP
      SUBROUTINE SOCINP(NPRINT,GPNAME)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500, MXRT=100, MXAO=2047)
C
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,S,SZ,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORB,
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NCI,
     *                IGPDET,KSTSYM,NFTGCI
      COMMON /DESOCI/ OSPIN(MXRT),NEXT,MOSET,MAXPSO,KSO,NSOCI
      COMMON /FMCOM / X(1)
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,MA,MB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IJKT,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      PARAMETER (NNAM=4)
C
      DIMENSION QNAM(NNAM),KQNAM(NNAM)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QNAM/8HNEXT    ,8HMAXPSO  ,8HNSOST   ,8HORBS    /
#else
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"NEXT    ","MAXPSO  ","NSOST   ","ORBS    "/
#endif
      DATA KQNAM/1,1,1,5/
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA C1/8HC1      /
#else
      CHARACTER*8 :: C1_STR
      EQUIVALENCE (C1, C1_STR)
      DATA C1_STR/"C1      "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RNOS,RMOS/8HNOS     ,8HMOS     /, RINPUT/8H$VEC    /
#else
      CHARACTER*8 :: RMOS_STR
      EQUIVALENCE (RMOS, RMOS_STR)
      CHARACTER*8 :: RNOS_STR
      EQUIVALENCE (RNOS, RNOS_STR)
      CHARACTER*8 :: RINPUT_STR
      EQUIVALENCE (RINPUT, RINPUT_STR)
      DATA RNOS_STR,RMOS_STR/"NOS     ","MOS     "/, 
     * RINPUT_STR/"$VEC    "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RNONE/8HNONE    /
#else
      CHARACTER*8 :: RNONE_STR
      EQUIVALENCE (RNONE, RNONE_STR)
      DATA RNONE_STR/"NONE    "/
#endif
C
      SOME = MASWRK  .AND.  NPRINT.NE.-5  .AND.  NPRINT.NE.-23
C
      IF(SOME) WRITE(IW,9000)
C
C  Set up defaults to include all the virtual orbitals as external.
C
      NEXT   = NQMT-NACT-NCOR
      ORBS   = RMOS
      MAXPSO = MAXP
      KSO    = K
C
      CALL NAMEIO(IR,JRET,GPNAME,NNAM,QNAM,KQNAM,
     *            NEXT,MAXPSO,KSO,ORBS,
     *    0,0,0,0,0,   0,0,0,0,0,    0,0,0,0,0,    0,0,0,0,0,
     *    0,0,0,0,0,   0,0,0,0,0,    0,0,0,0,0,    0,0,0,0,0,
     *    0,0,0,0,0,   0,0,0,0,0,    0,0,0,0,0,    0,0,0,0,0)
      IF(JRET.EQ.2) THEN
         IF(MASWRK) WRITE(IW,9010) GPNAME
         CALL ABRT
         STOP
      END IF
C
C  Check to see if the number of external orbitals is not ridiculous.
C
      IF (NEXT.GT.NQMT-NCOR-NACT) THEN
         WRITE(IW,9020) GPNAME,NEXT
         CALL ABRT
         STOP
      ENDIF
C
      IF (NEXT.LE.0) THEN
         WRITE(IW,9030) GPNAME,NEXT
         CALL ABRT
         STOP
      ENDIF
C
C  select orbital set to be used,
C  moset=0 means (probably) canonicalized MOs, while 1 means the NOs
C  NOs are not available unless this run is doing the mcscf.
C
                       MOSET=-1
      IF(ORBS.EQ.RMOS) MOSET=0
      IF(ORBS.EQ.RNOS) MOSET=1
      IF(SCFTYP.EQ.RNONE) THEN
         MOSET=0
         ORBS=RINPUT
      END IF
      IF(MOSET.EQ.-1) THEN
         WRITE(IW,*) 'ERROR: -ORBS- CAN ONLY BE -MOS- OR -NOS-'
         CALL ABRT
      END IF
C
C  See if the size of the SOCI spaces makes sense.
C
      IF (KSO.GT.K) THEN
         WRITE(IW,9040) GPNAME,KSO
         CALL ABRT
         STOP
      ENDIF
      MAXZ   = MAX(MAXPSO,2*KSO)
      MAXPSO = MAXZ
C
C  Now to write all the details of the calculation.
C
      IF(SOME) THEN
         WRITE(IW,9100) GRPDET,STSYM,ORBS,NCOR,NACT,NEXT,
     *                  NA+NCOR,NA,NB+NCOR,NB,NORB+NEXT
C
         WRITE(IW,9110) K,KST,MAXP,MAXW1,KSO,KSO,MAXPSO,NITER,CRIT
         WRITE(IW,9120) IROOT
         WRITE(IW,9130) (NFLGDM(II),II=1,K)
C
      END IF
C
C        full soci is to be done over the mcscf natural orbitals.
C        note we must exchange the orbitals before symmetry assign.
C
      IF(MOSET.EQ.1) THEN
         L1 = NUM
         L3 = L1*L1
         CALL VALFM(LOADFM)
         LNOS = LOADFM + 1
         LMOS = LNOS   + L3
         LAST = LMOS   + L3
         NEED = LAST - LOADFM - 1
         CALL GETFM(NEED)
         CALL DAREAD(IDAF,IODA,X(LMOS),L3,15,0)
         CALL DAREAD(IDAF,IODA,X(LNOS),L3,19,0)
         CALL DAWRIT(IDAF,IODA,X(LMOS),L3,19,0)
         CALL DAWRIT(IDAF,IODA,X(LNOS),L3,15,0)
         CALL RETFM(NEED)
      END IF
C
C  This is the only way I know to print out all symmetries
C  without modifying the original Determinant FCI code.
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
C        generate daf orbital symmetry record
C
      IF(GRPDET.EQ.C1) THEN
         CALL C1DET(X(LMOIRP),X(LMOLAB),L0)
      ELSE
         CALL DAREAD(IDAF,IODA,X(LVEC),L3,15,0)
         CALL DAREAD(IDAF,IODA,X(LS),L2,12,0)
         CALL DAREAD(IDAF,IODA,X(LQ),L3,45,0)
         CALL TRFSYM(X(LMOLAB),X(LMOIRP),X(LMODEG),X(LQ),X(LS),X(LVEC),
     *               X(LWRK),IA,L0,L1,L0,L1)
      END IF
C
C        change orbital symmetry labels from GAMESS to JAKAL values
C
      CALL GAJASW(X(LMOIRP),NUM,GRPDET)
      CALL DAWRIT(IDAF,IODA,X(LMOIRP),L1,262,1)
C
      IF(SOME) THEN
         WRITE(IW,9160) NCOR,NACT,NEXT
         CALL SOSYPR(X(LMOLAB),NCOR,NACT,NEXT)
      ENDIF
C
      CALL RETFM(NEEDD)
      RETURN
C
C
 9000 FORMAT(/5X,34("-")/
     *       5X,'DETERMINANTAL FULL SECOND ORDER CI'/
     *       5X,'  PROGRAM WRITTEN BY JOE IVANIC   '/
     *       5X,34(1H-))
 9010 FORMAT(/1X,'**** ERROR, THIS RUN REQUIRES INPUT OF A $',A8,
     *          ' GROUP')
 9020 FORMAT(/1X,'**** ERROR, THIS RUN SPECIFIES MORE EXTERNAL',
     *          ' ORBITALS THAN IS POSSIBLE'/
     *     1X,'CHECK $',A8,' INPUT: NEXT=',I4)
 9030 FORMAT(/1X,'**** ERROR, THIS RUN SPECIFIES A SILLY NUMBER OF',
     *          ' EXTERNAL ORBITALS'/
     *     1X,'CHECK $',A8,' INPUT: NEXT=',I4)
C
 9040 FORMAT(/1X,'**** ERROR, NO. OF SOCI STATES WANTED IS MORE ',
     *          ' THAN NO. OF FCI STATES REQUESTED'/
     *     1X,'CHECK $',A8,' INPUT: NSOST=',I4)
C
 9100 FORMAT(/1X,'THE POINT GROUP                  =',3X,A8/
     *       1X,'THE STATE SYMMETRY               =',3X,A8/
     *       1X,'ORBITAL SET BEING USED           =',A8/
     *       1X,'NUMBER OF CORE ORBITALS          =',I5/
     *       1X,'NUMBER OF ACTIVE ORBITALS        =',I5/
     *       1X,'NUMBER OF EXTERNAL ORBITALS      =',I5/
     *       1X,'NUMBER OF ALPHA ELECTRONS        =',I5,
     *          ' (',I4,' ACTIVE)'/
     *       1X,'NUMBER OF BETA ELECTRONS         =',I5,
     *          ' (',I4,' ACTIVE)'/
     *       1X,'NUMBER OF OCCUPIED ORBITALS      =',I5)
 9110 FORMAT(/1X,'NUMBER OF FCI STATES REQUESTED   =',I5/
     *        1X,'NUMBER OF FCI STARTING VECTORS   =',I5/
     *        1X,'MAX. NO. OF  FCI EXPANSION VECS  =',I5/
     *        1X,'SIZE OF INITIAL FCI GUESS MATRIX =',I5/
     *       /1X,'NUMBER OF SOCI STATES REQUESTED  =',I5/
     *        1X,'NUMBER OF SOCI STARTING VECTORS  =',I5/
     *        1X,'MAX. NO. OF SOCI EXPANSION VECS  =',I5/
     *       /1X,'MAX. NO. OF CI ITERS/STATE       =',I5/
     *        1X,'CI DIAGONALIZATION CRITERION     =',1P,E9.2)
 9120 FORMAT(1X,'CI PROPERTIES WILL BE FOUND FOR ROOT NUMBER',I4)
 9130 FORMAT(1X,'1E- DENSITY MATRIX OPTIONS ARE',20I2)
 9160 FORMAT(/1X,'SYMMETRIES FOR THE',I4,' CORE,',I4,' ACTIVE,',
     *      I4,' EXTERNAL ARE')
C
      END
C
C*MODULE FSODCI  *DECK SOSYPR
C     ----------------------------------------
      SUBROUTINE SOSYPR(LMOLAB,NCOR,NACT,NEXT)
C     ----------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION LMOLAB(NCOR+NACT+NEXT)
      COMMON /IOFILE/ IR,IW,IP,IS,IJKT,IDAF,NAV,IODA(400)
C
      IF(NCOR.GT.0) WRITE(IW,9070) (LMOLAB(I),I=1,NCOR)
      IF(NACT.GT.0) WRITE(IW,9080) (LMOLAB(I+NCOR),I=1,NACT)
      IF(NEXT.GT.0) WRITE(IW,9090) (LMOLAB(I+NCOR+NACT),I=1,NEXT)
C
 9070 FORMAT(/1X,'    CORE=',10(1X,A4,1X)/(10X,10(1X,A4,1X)))
 9080 FORMAT(/1X,'  ACTIVE=',10(1X,A4,1X)/(10X,10(1X,A4,1X)))
 9090 FORMAT(/1X,'EXTERNAL=',10(1X,A4,1X)/(10X,10(1X,A4,1X)))
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK FSOCIW
      SUBROUTINE FSOCIW(NPRINT,CLABEL,NRNFG,NPRINT2,DDITRF)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME,SOME2,PACK2E,GOPARR,DSKWRK,MASWRK,CLABEL,JACOBI,
     *        DDITRF
C
      DIMENSION NRNFG(10)
C
      PARAMETER (MXRT=100)
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,S,SZ,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORB,
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NCI,
     *                IGPDET,KSTSYM,NFTGCI
      COMMON /DESOCI/ OSPIN(MXRT),NEXT,MOSET,MAXPSO,KSO,NSOCI
C
      COMMON /ENRGYS/ ENUCR,EELCT,ETOT,STOT,SSQUAR,ECORE,ESCF,EERD,
     *                E1,E2,VEN,VEE,EPOT,EKIN,ESTATE(MXRT),STATN
      COMMON /FMCOM / X(1)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /IOFILE/ IR,IW,IP,IS,IJKT,IDAF,NAV,IODA(400)
      COMMON /JACOBI/ JACOBI,NJAOR,ELAST,ISTAT
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
C
C     ----- driver for Full SOCI computation -----
C
      SOME = MASWRK  .AND.  NPRINT.NE.-5
      SOME2 = MASWRK  .AND.  NPRINT2.NE.-5
C
C        core contribution to the energy is obtained from -ecore-,
C        and from modifications to the transformed 1e- integrals.
C        this effectively removes core orbitals from the computation.
C
      DO 10 II=1,KSO
         OSPIN(II) = SPINS(II)
   10 CONTINUE
C
      ECONST = ECORE + ENUCR
      NTOT = NEXT + NACT + NCORSV
      NTCO = NCORSV
      NORB = NACT
      NCOR = 0
      NSYM = 2**IGPDET
C
C        Compute the total number of determinants in this SOCI.
C        Decide necessary double/integer working storage -IDS- and -IIS-
C
      CALL VALFM(LOADFM)
      LIFA  = LOADFM + 1
      LIFE  = LIFA   + ((NACT+1)*(NACT+1))/NWDVAR + 1
      LAST  = LIFE   + ((NEXT+1)*3)/NWDVAR + 1
      NEED1 = LAST - LOADFM - 1
      CALL GETFM(NEED1)
C
      CALL BINOM6(X(LIFA),NACT)
      CALL BINOM7(X(LIFE),NEXT)
C
      CALL VALFM(LOADFM)
      IBO = LOADFM + 1
      ICON = IBO + NTOT
      ICA  = ICON + NA
      ICB  = ICA + 3*NSYM
      KTAB = ICB + 3*NSYM
      IWRK = KTAB + NSYM
      LAST = IWRK + 43
C
      NEEDT = LAST - LOADFM - 1
      CALL GETFM(NEEDT)
      CALL DAREAD(IDAF,IODA,X(IBO),NTOT,262,1)
      CALL CORTRA(X(IBO),NTOT,NTCO)
C
      CALL MESOCI(IW,NACT,NEXT,NA,NB,KSO,MAXPSO,MAXW1,
     *           X(LIFA),X(LIFE),
     *           NSOCI,IDS,IIS,IGPDET,KSTSYM,NSYM,
     *           X(IBO),X(ICON),X(ICA),X(ICB),X(KTAB),X(IWRK),
     *           N0,N1A,N1B,N2A,N2B,N1AB)
C
      CALL RETFM(NEEDT)
C
      IF(SOME) THEN
         WRITE(IW,9000)
         WRITE(IW,9110) STSYM,GRPDET,SZ,NSOCI
         WRITE(IW,9115) N0,N1A,N1B,N2A,N2B,N1AB
      END IF
C
      M1 = NACT+NEXT
      M2 = (M1*M1+M1)/2
      M4 = (M2*M2+M2)/2
C
C        allocate memory for the SOCI step
C
      CALL VALFM(LOADFM)
      LDWRK  = LOADFM + 1
      LIWRK  = LDWRK  + IDS
      LSINT1 = LIWRK  + IIS/NWDVAR + 1
      LSINT2 = LSINT1 + M2
      LIA    = LSINT2 + M4
      LXX    = LIA    + M2/NWDVAR + 1
      LIXX   = LXX    + NINTMX
      LEL    = LIXX   + NINTMX
      IBO    = LEL + MAXW1
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
         DO 330 IST=1,K
            SPINS(IST) = S
            ESTATE(IST) = ZERO
  330    CONTINUE
         LCIVEC = LDWRK
         CALL VCLR(X(LCIVEC),1,K*NCI)
         GO TO 450
      END IF
C
C     --- obtain 1 and 2 e- transformed integrals over occupied orbs ---
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
C        ----- compute full second order ci wavefunction -----
C
      CALL DETSO(IW,SOME,ECONST,
     *     X(LSINT1),X(LSINT2),M2,M4,NACT,NEXT,NCI,NSOCI,NA,NB,
     *     KSO,KSO,MAXPSO,MAXW1,NITER,CRIT,
     *     X(LIFA),X(LIFE),SPINS,X(LEL),X(LDWRK),IDS,X(LIWRK),IIS,
     *     IGPDET,KSTSYM,NSYM,X(IBO),ISTAT,
     *     NRNFG,NFLGDM,SOME2)
C
      DO 400 I=1,KSO
         ESTATE(I) = X(LEL-1+I)+ECONST
  400 CONTINUE
C
C        save energy quantities
C
  450 CONTINUE
      ETOT = ESTATE(IROOT)
      EELCT = ETOT - ENUCR
      STOT = SPINS(IROOT)
      SSQUAR = STOT*(STOT+ONE)
      STATN = KSO
C
C        save eigenvectors to disk
C
      CALL SEQOPN(NFT12,'CIVECTR','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQREW(NFT12)
      WRITE(NFT12) KSO,NSOCI
      DO 420 IST=1,KSO
         LCIVEC = LDWRK + (IST-1)*NSOCI
         CALL STFASE(X(LCIVEC),NSOCI,NSOCI,1)
         CALL SQWRIT(NFT12,X(LCIVEC),NSOCI)
  420 CONTINUE
      CALL SEQREW(NFT12)
C
      CALL RETFM(NEED2)
      CALL RETFM(NEED1)
C
C      RETURN
C
 9000 FORMAT(/5X,34("-")/
     *       5X,'DETERMINANTAL FULL SECOND ORDER CI'/
     *       5X,'  PROGRAM WRITTEN BY JOE IVANIC   '/
     *       5X,34(1H-))
 9110 FORMAT(/1X,'THE NUMBER OF DETERMINANTS HAVING SPACE SYMMETRY ',A3/
     *        1X,'IN POINT GROUP ',A4,' WITH SZ=',F5.1,' IS',I15)
 9115 FORMAT(/1X,'NO. OF ZEROTH ORDER DETS              =',I15/
     *        1X,'NO. OF SINGLE ALPHA EXCITED DETS      =',I15/
     *        1X,'NO. OF SINGLE  BETA EXCITED DETS      =',I15/
     *        1X,'NO. OF DOUBLE ALPHA EXCITED DETS      =',I15/
     *        1X,'NO. OF DOUBLE  BETA EXCITED DETS      =',I15/
     *        1X,'NO. OF SINGLE ALPHA+BETA EXCITED DETS =',I15)
 9130 FORMAT(/1X,'THE DETERMINANT FULL SOCI REQUIRES',I12,' WORDS')
C
      END
C
C*MODULE FSODCI  *DECK BINOM7
C     ------------------------
      SUBROUTINE BINOM7(IFA,N)
C     ------------------------
      IMPLICIT INTEGER(A-Z)
      INTEGER IFA(0:N,0:2)
C
C     Returns all binomial numbers (i,j) for i=0,n and j=0,2 in ifa .
C     The binomial number (i,j) is stored in ifa(i,j)
C
      DO 13 II=0,N
         IFA(II,0)  = 1
   13 CONTINUE
      IFA(0,1) = 0
      IFA(0,2) = 0
      IFA(1,1) = 1
      IFA(1,2) = 0
      IFA(2,1) = 2
      IFA(2,2) = 1
C
      DO 113 IY = 3, N
         DO 114 IX = 1, 2
            IFA(IY,IX) = IFA(IY-1,IX-1) + IFA(IY-1,IX)
  114    CONTINUE
  113 CONTINUE
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK MESOCI
C     --------------------------------------------------------
      SUBROUTINE MESOCI(IW,NACT,NEXT,NA,NB,
     *                 KSO,MAXPSO,MAXW1,IFA,IFE,NSOCI,IDS,IIS,
     *                 IDSYM,ISYM1,NSYM,
     *                 IBO,ICON,ICA,ICB,KTAB,IWRK,
     *                 N0,N1A,N1B,N2A,N2B,N1AB)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION IFA(0:NACT,0:NACT)
      DIMENSION IFE(0:NEXT,0:2)
      DIMENSION ICON(NA),ICA(3*NSYM),ICB(3*NSYM),KTAB(NSYM)
      DIMENSION IBO(NACT+NEXT),IWRK(43)
C
C     Code to return memory requirement for CI calculation.
C
C   nact      : no. of active orbitals.
C   next      : no. of external orbitals.
C   na, nb    : number of active alpha and beta electrons respectively
C   kso       : Number of states required.
C   maxpso    : Maximum number of vectors before transforming and
C               starting at kst.
C   maxw1     : Needed for scratch arrays, same as initial guess size
C               for FCI.
C   ifa       : Contains list of binomial coefficients for active orbs.
C   ife       : Contains list of binomial coefs for external orbs.
C   idsym     : Which point group, see gtab, gmul or getsym1 in symwrk.f
C               for convention.
C   isym1     : Which irreducible representation, see gtab etc for info.
C   nsym      : nsym = 2**(idsym)
C   ibo       : ibo(i) contains symmetry of orbital i, see gtab
C
C  RETURNED
C
C   nsoci     : Returns with the no. of determinants for required FSOCI.
C   ids       : Required space for double precision arrays.
C   iis       : Required space for integer arrays.
C
C  SCRATCH    : iwrk,icon,ica,icb,ktab, these are so small that they
C               don't need to be prepared for, ie be a part of the
C               calling statement.
C
      NOCC = NACT + NEXT
      IDSYMT = IDSYM
      ISYM1T = ISYM1
      IF (IDSYM.EQ.0) IDSYMT = 1
      IF (ISYM1T.EQ.0) ISYM1T = 1
C
      NSOCI = 0
      NA0F = IFA(NACT,NA)
      NB0F = IFA(NACT,NB)
      NA1F = 0
      NB1F = 0
      NA2F = 0
      NB2F = 0
      IF(NA.GE.1) NA1F = IFA(NACT,NA-1)
      IF(NB.GE.1) NB1F = IFA(NACT,NB-1)
      IF(NA.GE.2) NA2F = IFA(NACT,NA-2)
      IF(NB.GE.2) NB2F = IFA(NACT,NB-2)
      NA1E = IFE(NEXT,1)
      NB1E = IFE(NEXT,1)
      NA2E = IFE(NEXT,2)
      NB2E = IFE(NEXT,2)
      NA1T = NA1F*NA1E
      NB1T = NB1F*NB1E
      NA2T = NA2F*NA2E
      NB2T = NB2F*NB2E
      NA2S = NA0F + NA1T
      NB2S = NB0F + NB1T
      NATT = NA2S + NA2T
      NBTT = NB2S + NB2T
C
      DO 45 II=1,3*NSYM
         ICA(II) = 0
         ICB(II) = 0
   45 CONTINUE
C
      CALL GTAB(IDSYMT,ISYM1T,KTAB,
     *      IWRK(1),IWRK(4),IWRK(7),IWRK(10))
C
C  Symmetry of zero excitated strings.
C
      DO 23 II=1,NA
         ICON(II) = II
   23 CONTINUE
C
      DO 53 IA=1,NA0F
         CALL GETSYM1(IW,ICON,NACT,NA,IBO,IDSYMT,ISYM,
     *   IWRK(1),IWRK(4),IWRK(7),IWRK(10))
         ICA(ISYM) = ICA(ISYM) + 1
         CALL ADVANC(ICON,NA,NACT)
   53 CONTINUE
C
      DO 33 II=1,NB
         ICON(II) = II
   33 CONTINUE
C
      DO 43 IB=1,NB0F
         CALL GETSYM1(IW,ICON,NACT,NB,IBO,IDSYMT,ISYM,
     *   IWRK(1),IWRK(4),IWRK(7),IWRK(10))
         ICB(ISYM) = ICB(ISYM) + 1
         CALL ADVANC(ICON,NB,NACT)
   43 CONTINUE
C
C   Symmetry of singly excited strings.
C
      DO 123 II=1,NA-1
         ICON(II) = II
  123 CONTINUE
C
      DO 153 IA=1,NA1F
         ICON(NA) = NACT+1
         DO 155 IE=1,NA1E
            CALL GETSYM1(IW,ICON,NOCC,NA,IBO,IDSYMT,ISYM,
     *      IWRK(1),IWRK(4),IWRK(7),IWRK(10))
            ICA(NSYM+ISYM) = ICA(NSYM+ISYM) + 1
            CALL ADVANC(ICON(NA),1,NOCC)
  155    CONTINUE
         CALL ADVANC(ICON,NA-1,NACT)
  153 CONTINUE
C
      DO 133 II=1,NB-1
         ICON(II) = II
  133 CONTINUE
C
      DO 143 IB=1,NB1F
         ICON(NB) = NACT+1
         DO 145 IE=1,NB1E
            CALL GETSYM1(IW,ICON,NOCC,NB,IBO,IDSYMT,ISYM,
     *      IWRK(1),IWRK(4),IWRK(7),IWRK(10))
            ICB(NSYM+ISYM) = ICB(NSYM+ISYM) + 1
            CALL ADVANC(ICON(NB),1,NOCC)
  145    CONTINUE
         CALL ADVANC(ICON,NB-1,NACT)
  143 CONTINUE
C
C  Symmetry of doubly excited strings.
C
      DO 223 II=1,NA-2
         ICON(II) = II
  223 CONTINUE
C
      DO 253 IA=1,NA2F
         ICON(NA-1) = NACT+1
         ICON(NA) = NACT+2
         DO 255 IE=1,NA2E
            CALL GETSYM1(IW,ICON,NOCC,NA,IBO,IDSYMT,ISYM,
     *      IWRK(1),IWRK(4),IWRK(7),IWRK(10))
            ICA(2*NSYM+ISYM) = ICA(2*NSYM+ISYM) + 1
            CALL ADVANC(ICON(NA-1),2,NOCC)
  255    CONTINUE
         CALL ADVANC(ICON,NA-2,NACT)
  253 CONTINUE
C
      DO 233 II=1,NB-2
         ICON(II) = II
  233 CONTINUE
C
      DO 243 IA=1,NB2F
         ICON(NB-1) = NACT+1
         ICON(NB) = NACT+2
         DO 245 IE=1,NB2E
            CALL GETSYM1(IW,ICON,NOCC,NB,IBO,IDSYMT,ISYM,
     *      IWRK(1),IWRK(4),IWRK(7),IWRK(10))
            ICB(2*NSYM+ISYM) = ICB(2*NSYM+ISYM) + 1
            CALL ADVANC(ICON(NB-1),2,NOCC)
  245    CONTINUE
         CALL ADVANC(ICON,NB-2,NACT)
  243 CONTINUE
C
C  Now to combine alpha and beta strings to see how many give
C  the right symmetry group.
C
      N0   = 0
      N1A  = 0
      N1B  = 0
      N2A  = 0
      N2B  = 0
      N1AB = 0
      DO 67 II=1,NSYM
         N0   = N0 + ICA(II)*ICB(KTAB(II))
         N1A  = N1A + ICA(II+NSYM)*ICB(KTAB(II))
         N1B  = N1B + ICA(II)*ICB(NSYM+KTAB(II))
         N2A  = N2A + ICA(II+2*NSYM)*ICB(KTAB(II))
         N2B  = N2B + ICA(II)*ICB(NSYM*2+KTAB(II))
         N1AB = N1AB + ICA(NSYM+II)*ICB(NSYM+KTAB(II))
   67 CONTINUE
C
      NSOCI = N0 + N1A + N1B + N2A + N2B + N1AB
C
C  Now for the extra memory requirements.
C
      IDS = (2*MAXPSO+1)*NSOCI+MAXW1*(8+MAXW1)+(MAXW1*(MAXW1+1))/2+
     * (MAXPSO*MAXPSO) + MAXPSO
C
      IIS = MAXW1 + 43 + NA + NATT*3 + NBTT*3 + NSYM*NSYM + NSYM +
     *      2*(NSYM+1) + 10*NSYM +
     *      (NOCC*NOCC+NOCC)/2 + 1 +
     *      4*NA + KSO + 3*(NA*(NACT-NA))*NSYM + 5*NSYM +
     *      3*(NA*NEXT*NSYM)+
     *      3*((NA-1)*(NACT-NA+1)+(NEXT-1))*NSYM +
     *      3*(NA-1)*(NEXT-1)*NSYM +
     *      3*((NA-2)*(NACT-NA+2)+2*(NEXT-2))*NSYM +
     *      KSO
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK DETSO
C     --------------------------------------------------------
      SUBROUTINE DETSO(IW,SOME,ECONST,SI1,SI2,ISI1,ISI2,
     *      NACT,NEXT,NCI,NSOCI,NA,NB,K,KST,MAXP,MAXW1,NITER,CRIT,
     *      IFA,IFE,SPIN,EL,CI,IDS,IWRK,IIS,
     *         IDSYM,ISYM1,NSYM,IOB,ISTAT,
     *         NRNFG,NFLGDM,SOME2)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
      LOGICAL SOME,SOME2
C
      DIMENSION NRNFG(10),NFLGDM(K)
      DIMENSION SI1(ISI1)
      DIMENSION SI2(ISI2)
      DIMENSION SPIN(MAXP),EL(MAXW1)
      DIMENSION IFA(0:NACT,0:NACT)
      DIMENSION IFE(0:NEXT,0:2)
      DIMENSION CI(IDS),IWRK(IIS)
      DIMENSION IOB(NACT+NEXT)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
C
C   Code to do determinantal SOCI, here we partition the memory up
C   and call subroutine DAVSO which is the real guts of the CI.
C   Written by J. Ivanic 2001
C
C   econst    : Constant nuclear + frozen energy.
C   si1, si2  : 1 and 2 electron active integrals stored in reverse
C               canonical order, same as GAMESS, if have core orbitals,
C               must have been modified by routine cormod.
C   nact,next : number of active and external orbitals resp.
C   nci       : number of 0th order determinants, FCI size essentially
C   nsoci     : number of SOCI determinants.
C   na, nb    : number of active alpha and beta electrons respectively
C   k, kst    : Number of states required and no. of minimum/starting
C               states in CI procedure.
C   maxp      : Maximum number of vectors before transforming and
C               starting at kst.
C   maxw1     : Needed for sizes of various scratch arrays.
C   niter,crit: Maximum no. of total iterations, convergence criterion.
C               I very strongly suggest crit = 5.0d-5, this gives
C               accuracy to at least 8 decimal places.  niter I would
C               make very large,2000, because I am sure that if you
C               have reasonable orbitals, states will have to converge
C               eventually.
C   ifa       : Contains binomial coefficients, obtained by calling
C               routine binom6.
C   ife       : Contains binomial coefficients, obtained by calling
C               routine binom7.
C   spin      : returned spin, spin(i) = spin of state i
C   EL        : EL(i) = total electronic energy of state i
C   CI        : First part of CI contains the returned CI coefficients,
C               ie CI((i-1)*nsoci + j) contains coefficient for det. j
C               and state i.  The remainder of CI is used for scratch.
C   ids       : Total space for CI in order to do the CI, should be
C               obtained from memci
C   iwrk      : Scratch integer space
C   iis       : Space for iwrk in order to do the CI, should be obtained
C               from mesoci.
C   idsym     : Which point group, see subs gtab, gmul or getsym1
C               in symwrk.f for convention.
C   isym1     : Which irreducible representation, see gtab etc for info.
C   nsym      : nsym = 2**(idsym)
C   iob       : iob(i) contains symmetry of occupied orbital i, see gtab
C               etc for info.
C
      NOCC = NACT + NEXT
C
C  Parition double precision array.
C
      KCOEF = 1
      KAB = KCOEF + MAXP*NSOCI
      KQ = KAB + MAXP*NSOCI
      KB = KQ + NSOCI
      KEF = KB + 8*MAXW1
      KF = KEF + MAXW1*MAXW1
      KEC = KF + (MAXW1*(MAXW1+1))/2
      KGR = KEC + MAXP*MAXP
      KCITOT = KGR + MAXP
C
C    Now for integer iwrk array
C
      IWRK2 = 1
      ISYMA = IWRK2 + 43 + NA + MAXW1
      ISYMB = ISYMA + NATT
      ISPA  = ISYMB + NBTT
      ISPB  = ISPA  + NATT
      IMUL  = ISPB  + NBTT
      ITAB  = IMUL  + NSYM*NSYM
      ICOA  = ITAB  + NSYM
      ICOB  = ICOA  + 3*NSYM
      ISAS0 = ICOB  + 3*NSYM
      ISBS0 = ISAS0 + NSYM+1
      ISAS1 = ISBS0 + NSYM+1
      ISBS1 = ISAS1 + NSYM
      ISAS2 = ISBS1 + NSYM
      ISBS2 = ISAS2 + NSYM
      ISAC  = ISBS2 + NSYM
      ISBC  = ISAC  + NATT
C
      INDEX = ISBC  + NBTT
C
      IACON1 = INDEX + (NOCC*(NOCC+1))/2 + 1
      IBCON1 = IACON1 + NA
      IWRK1  = IBCON1 + NA
      IACON2 = IWRK1 + KST
      IBCON2 = IACON2 + NA
      IPOSA00  = IBCON2 + NA
      IPERA00  = IPOSA00 + (NA*(NACT-NA))*NSYM
      IIND100  = IPERA00 + (NA*(NACT-NA))*NSYM
      IMMC00   = IIND100 + (NA*(NACT-NA))*NSYM
      IPOSA01  = IMMC00 + NSYM
      IPERA01  = IPOSA01 + (NA*NEXT*NSYM)
      IIND101  = IPERA01 + (NA*NEXT*NSYM)
      IMMC01   = IIND101 + (NA*NEXT*NSYM)
      IPOSA11  = IMMC01 + NSYM
      IPERA11  = IPOSA11 + ((NA-1)*(NACT-NA+1)+(NEXT-1))*NSYM
      IIND111  = IPERA11 + ((NA-1)*(NACT-NA+1)+(NEXT-1))*NSYM
      IMMC11   = IIND111 + ((NA-1)*(NACT-NA+1)+(NEXT-1))*NSYM
      IPOSA12  = IMMC11 + NSYM
      IPERA12  = IPOSA12 + ((NA-1)*(NEXT-1))*NSYM
      IIND112  = IPERA12 + ((NA-1)*(NEXT-1))*NSYM
      IMMC12   = IIND112 + ((NA-1)*(NEXT-1))*NSYM
      IPOSA22  = IMMC12 + NSYM
      IPERA22  = IPOSA22 + ((NA-2)*(NACT-NA+2)+2*(NEXT-2))*NSYM
      IIND122  = IPERA22 + ((NA-2)*(NACT-NA+2)+2*(NEXT-2))*NSYM
      IMMC22   = IIND122 + ((NA-2)*(NACT-NA+2)+2*(NEXT-2))*NSYM
      IHMCON   = IMMC22 + NSYM
      ITOT     = IHMCON + K
C
      IF (KCITOT.GT.IDS+1.OR.ITOT.GT.IIS+1) THEN
         IF (SOME) THEN
            WRITE(IW,*) 'NOT ENOUGH MEMORY SPECIFIED FOR CI'
            WRITE(IW,*) 'ASKED FOR ',IDS,' AND ',IIS,' DOUBLE '
            WRITE(IW,*) 'PRECISION AND INTEGER, ACTUALLY NEED'
            WRITE(IW,*) KCITOT-1,' AND ',ITOT-1
         ENDIF
         CALL ABRT
         STOP
      ENDIF
C
C    Set up symmetry data here.
C
      CALL SYMWRK2(IW,IOB,NACT,NEXT,NA,NB,IDSYM,ISYM1,NSYM,
     * IWRK,IWRK(ISYMA),IWRK(ISYMB),IWRK(ICOA),IWRK(ICOB),IWRK(ITAB),
     * IWRK(IMUL),IWRK(ISPA),IWRK(ISPB),IWRK(ISAS0),IWRK(ISBS0),
     * IWRK(ISAS1),IWRK(ISBS1),IWRK(ISAS2),IWRK(ISBS2),
     * IWRK(ISAC),IWRK(ISBC))
C
C   nsym to iwrk(isbc) is SYMMETRY AND POSITION STUFF.
C   iwrk(iacon1) to end is SCRATCH ARRAYS.
C
      CALL DAVSO(IW,SOME,ECONST,ISTAT,
     *    SI1,SI2,NACT,NEXT,NOCC,NCI,NSOCI,NA,NB,CI(KCOEF),SPIN,EL,
     *    K,KST,MAXP,MAXW1,NITER,CRIT,CI(KAB),CI(KQ),CI(KB),
     *    CI(KEF),CI(KF),CI(KEC),IFA,IFE,IOB,IWRK(IWRK2),
     *    IWRK(INDEX),
     *    NSYM,IWRK(ITAB),IWRK(IMUL),IWRK(ISYMA),IWRK(ISYMB),
     *    IWRK(ISPA),IWRK(ISPB),IWRK(ISAS0),IWRK(ISBS0),
     *    IWRK(ISAS1),IWRK(ISBS1),IWRK(ISAS2),IWRK(ISBS2),
     *    IWRK(ISAC),IWRK(ISBC),
     *    IWRK(IACON1),IWRK(IBCON1),IWRK(IWRK1),IWRK(IACON2),
     *    IWRK(IBCON2),IWRK(IPOSA00),IWRK(IPERA00),
     *    IWRK(IIND100),IWRK(IMMC00),IWRK(IPOSA01),
     *    IWRK(IPERA01),IWRK(IIND101),IWRK(IMMC01),
     *    IWRK(IPOSA11),IWRK(IPERA11),IWRK(IIND111),
     *    IWRK(IMMC11),IWRK(IPOSA12),IWRK(IPERA12),
     *    IWRK(IIND112),IWRK(IMMC12),IWRK(IPOSA22),
     *    IWRK(IPERA22),IWRK(IIND122),IWRK(IMMC22),
     *    IWRK(IHMCON),CI(KGR))
C
C  Now to print out the results
C
      IF (SOME) THEN
C
      CALL PRISO(IW,CI(KCOEF),CI(KAB),EL,IWRK(IACON1),IWRK(IBCON1),
     *    NSYM,IWRK(ISYMA),IWRK(ISPA),IWRK(ISPB),
     *    IWRK(ISBS0),IWRK(ISBS1),IWRK(ISBS2),IWRK(ISBC))
C
      ENDIF
C
      IF(SOME) WRITE(IW,9140)
      IF(SOME) CALL TIMIT(1)
      IF(EXETYP.NE.CHECK  .AND.  ISTAT.NE.0) THEN
         IF(MASWRK) WRITE(IW,9150)
         CALL ABRT
         STOP
      END IF
C
C  Now for 1e- density matrices.
C
      IF (NRNFG(5).EQ.0) RETURN
C
      CALL SEQOPN(NFT15,'WORK15', 'UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQREW(NFT15)
C
      IF (SOME2) WRITE(IW,9210)
C
      DO 550 IST=1,K
         IF(NFLGDM(IST).EQ.0) GOTO 550
         IF (SOME2) WRITE(IW,9220) IST,EL(IST)+ECONST
C
C   nsym through iwrk(isbc) is SYMMETRY AND POSITION STUFF.
C   iwrk(iacon1) to iwrk(ibcon2) is SCRATCH ARRAYS.
C
      CALL DENSO1(SI1,NACT,NEXT,NOCC,NSOCI,NA,NB,
     *    CI(KCOEF+(IST-1)*NSOCI),IFA,IFE,IOB,
     *    IWRK(INDEX),
     *    NSYM,IWRK(ISYMA),IWRK(ISYMB),
     *    IWRK(ISPA),IWRK(ISPB),IWRK(ISAS0),IWRK(ISBS0),
     *    IWRK(ISAS1),IWRK(ISBS1),IWRK(ISAS2),IWRK(ISBS2),
     *    IWRK(ISAC),
     *    IWRK(IACON1),IWRK(IBCON1),IWRK(IACON2),IWRK(IBCON2))
C
         CALL SQWRIT(NFT15,SI1,(NOCC*(NOCC+1))/2)
C
  550 CONTINUE
      IF(SOME) WRITE(IW,9230)
      IF(SOME) CALL TIMIT(1)
C
      RETURN
 9140 FORMAT(1X,'..... DONE WITH DETERMINANT SOCI COMPUTATION .....')
 9150 FORMAT(1X,'CI COMPUTATION DID NOT CONVERGE, JOB CANNOT CONTINUE')
 9210 FORMAT(/5X,27("-")/5X,'ONE PARTICLE DENSITY MATRIX'/5X,27("-"))
 9220 FORMAT(/1X,'CI EIGENSTATE',I4,' TOTAL ENERGY =',F20.10)
 9230 FORMAT(1X,'..... DONE WITH ONE PARTICLE DENSITY MATRICES .....')
C
      END
C
C*MODULE FSODCI  *DECK SYMWRK2
C     --------------------------------------------------------
      SUBROUTINE SYMWRK2(IW,IBO,NACT,NEXT,NA,NB,IDSYM,ISYM1,NSYM,
     *     ICON,ISYMA,ISYMB,ICOA,ICOB,ITAB,
     *     IMUL,ISPA,ISPB,ISAS0,ISBS0,ISAS1,ISBS1,ISAS2,ISBS2,
     *     ISAC,ISBC)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION ICON(*)
      DIMENSION ISPA(NATT),ISPB(NBTT),ICOA(3*NSYM),ICOB(3*NSYM)
      DIMENSION ISYMA(NATT),ISYMB(NBTT),ITAB(NSYM)
      DIMENSION ISAS0(NSYM+1),ISBS0(NSYM+1)
      DIMENSION ISAS1(NSYM),ISBS1(NSYM)
      DIMENSION ISAS2(NSYM),ISBS2(NSYM)
      DIMENSION ISAC(NATT),ISBC(NBTT)
      DIMENSION IMUL(NSYM,NSYM)
      DIMENSION IBO(NACT+NEXT)
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
      NOCC = NACT + NEXT
      CALL GTAB(IDSYM,ISYM1,ITAB,ICON(1),ICON(4),ICON(7),ICON(10))
      CALL GMUL(IDSYM,IMUL,ICON(1),ICON(4),ICON(7),ICON(10))
C
      DO 13 II=1,NSYM
         ISAS0(II) = 0
         ISBS0(II) = 0
         ISAS1(II) = 0
         ISBS1(II) = 0
         ISAS2(II) = 0
         ISBS2(II) = 0
   13 CONTINUE
      DO 15 II=1,3*NSYM
         ICOA(II) = 0
         ICOB(II) = 0
   15 CONTINUE
C
      DO 23 II=1,NB
         ICON(II) = II
   23 CONTINUE
C
      DO 43 IB=1,NB0F
         CALL GETSYM1(IW,ICON(1),NACT,NB,IBO,IDSYM,ISYM,
     *    ICON(NA+1),ICON(NA+4),ICON(NA+7),ICON(NA+10))
         ISYMB(IB) = ISYM
         ICOB(ISYM) = ICOB(ISYM) + 1
         ISPB(IB) = ICOB(ISYM)
         CALL ADVANC(ICON,NB,NACT)
   43 CONTINUE
C
      ICOUNT = NB0F
      DO 53 II=1,NB-1
         ICON(II) = II
  53  CONTINUE
C
      DO 63 IZ=1,NB1F
         ICON(NB) = NACT+1
         DO 65 IE=1,NB1E
            CALL GETSYM1(IW,ICON(1),NOCC,NB,IBO,IDSYM,ISYM,
     *      ICON(NA+1),ICON(NA+4),ICON(NA+7),ICON(NA+10))
            ICOUNT = ICOUNT + 1
            ISYMB(ICOUNT) = ISYM
            ICOB(NSYM+ISYM) = ICOB(NSYM+ISYM) + 1
            ISPB(ICOUNT) = ICOB(ISYM)+ICOB(ISYM+NSYM)
            CALL ADVANC(ICON(NB),1,NOCC)
   65    CONTINUE
         CALL ADVANC(ICON,NB-1,NACT)
   63 CONTINUE
C
      DO 73 II=1,NB-2
         ICON(II) = II
   73 CONTINUE
C
      DO 83 IZ=1,NB2F
         ICON(NB-1) = NACT+1
         ICON(NB) = NACT+2
         DO 85 IE=1,NB2E
            CALL GETSYM1(IW,ICON(1),NOCC,NB,IBO,IDSYM,ISYM,
     *      ICON(NA+1),ICON(NA+4),ICON(NA+7),ICON(NA+10))
            ICOUNT = ICOUNT + 1
            ISYMB(ICOUNT) = ISYM
            ICOB(2*NSYM+ISYM) = ICOB(2*NSYM+ISYM) + 1
            ISPB(ICOUNT) = ICOB(ISYM)+ICOB(ISYM+NSYM)+
     *                     ICOB(ISYM+2*NSYM)
            CALL ADVANC(ICON(NB-1),2,NOCC)
   85    CONTINUE
         CALL ADVANC(ICON,NB-2,NACT)
   83 CONTINUE
C
      DO 33 II=1,NA
         ICON(II) = II
   33 CONTINUE
C
      NCI = 0
      DO 153 IA=1,NA0F
         CALL GETSYM1(IW,ICON(1),NACT,NA,IBO,IDSYM,ISYM,
     *   ICON(NA+1),ICON(NA+4),ICON(NA+7),ICON(NA+10))
         ISYMA(IA) = ISYM
         ICOA(ISYM) = ICOA(ISYM) + 1
         ISPA(IA) = NCI
         NCI = NCI + ICOB(ITAB(ISYM))+ICOB(NSYM+ITAB(ISYM))+
     *               ICOB(2*NSYM+ITAB(ISYM))
         CALL ADVANC(ICON,NA,NACT)
  153 CONTINUE
C
      ICOUNT = NA0F
      DO 163 II=1,NA-1
         ICON(II) = II
  163 CONTINUE
C
      DO 173 IA=1,NA1F
         ICON(NA) = NACT+1
         DO 175 IE=1,NA1E
            CALL GETSYM1(IW,ICON(1),NOCC,NA,IBO,IDSYM,ISYM,
     *      ICON(NA+1),ICON(NA+4),ICON(NA+7),ICON(NA+10))
            ICOUNT = ICOUNT + 1
            ISYMA(ICOUNT) = ISYM
            ICOA(NSYM+ISYM) = ICOA(NSYM+ISYM) + 1
            ISPA(ICOUNT) = NCI
            NCI = NCI + ICOB(ITAB(ISYM))+ICOB(NSYM+ITAB(ISYM))
            CALL ADVANC(ICON(NA),1,NOCC)
  175    CONTINUE
         CALL ADVANC(ICON,NA-1,NACT)
  173 CONTINUE
C
      DO 183 II=1,NA-2
         ICON(II) = II
  183 CONTINUE
C
      DO 193 IA=1,NA2F
         ICON(NA-1) = NACT+1
         ICON(NA) = NACT+2
         DO 195 IE=1,NA2E
            CALL GETSYM1(IW,ICON(1),NOCC,NA,IBO,IDSYM,ISYM,
     *      ICON(NA+1),ICON(NA+4),ICON(NA+7),ICON(NA+10))
            ICOUNT = ICOUNT + 1
            ISYMA(ICOUNT) = ISYM
            ICOA(2*NSYM+ISYM) = ICOA(2*NSYM+ISYM) + 1
            ISPA(ICOUNT) = NCI
            NCI = NCI + ICOB(ITAB(ISYM))
            CALL ADVANC(ICON(NA-1),2,NOCC)
  195    CONTINUE
         CALL ADVANC(ICON,NA-2,NACT)
  193 CONTINUE
C
      ISYMCA = 1
      ISYMCB = 1
      DO 210 II=1,NSYM
         ISAS0(II) = ISYMCA
         ISBS0(II) = ISYMCB
         ISYMCA = ISYMCA + ICOA(ITAB(II))
         ISYMCB = ISYMCB + ICOB(ITAB(II))
         ISAS1(II) = ISYMCA
         ISBS1(II) = ISYMCB
         ISYMCA = ISYMCA + ICOA(ITAB(II)+NSYM)
         ISYMCB = ISYMCB + ICOB(ITAB(II)+NSYM)
         ISAS2(II) = ISYMCA
         ISBS2(II) = ISYMCB
         ISYMCA = ISYMCA + ICOA(ITAB(II)+2*NSYM)
         ISYMCB = ISYMCB + ICOB(ITAB(II)+2*NSYM)
  210 CONTINUE
      ISAS0(NSYM+1) = ISYMCA
      ISBS0(NSYM+1) = ISYMCB
C
      DO 373 II=1,NSYM
         ICOA(II) = 0
         ICOB(II) = 0
  373 CONTINUE
C
      DO 383 IA=1,NATT
         NSA = ISYMA(IA)
         ICOA(NSA) = ICOA(NSA) + 1
         ISAC(ISAS0(ITAB(NSA))+ICOA(NSA)-1) = IA
  383 CONTINUE
C
      DO 393 IB=1,NBTT
         NSA = ISYMB(IB)
         ICOB(NSA) = ICOB(NSA) + 1
         ISBC(ISBS0(ITAB(NSA))+ICOB(NSA)-1) = IB
  393 CONTINUE
C
      RETURN
      END
C
C
C*MODULE FSODCI  *DECK DAVSO
C     ----------------------------------------------------------
C    nsym to isbc is SYMMETRY AND POSITION STUFF
C    iacon1 to gr is SCRATCH ARRAYS
C
      SUBROUTINE DAVSO(IW,SOME,ECONST,ISTAT,
     * SI1,SI2,NACT,NEXT,NOCC,NCI,NSOCI,NA,NB,CI,SPIN,EL,K,KST,
     * MAXP,MAXW1,NITER,CRIT,AB,Q,B,EF,F,EC,IFA,IFE,IOB,IWRK2,
     * INDEX,
     * NSYM,ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISAS0,ISBS0,
     * ISAS1,ISBS1,ISAS2,ISBS2,ISAC,ISBC,
     * IACON1,IBCON1,IWRK1,IACON2,IBCON2,IPOSA00,IPERA00,
     * IIND100,IMMC00,IPOSA01,IPERA01,IIND101,IMMC01,
     * IPOSA11,IPERA11,IIND111,IMMC11,
     * IPOSA12,IPERA12,IIND112,IMMC12,
     * IPOSA22,IPERA22,IIND122,IMMC22,
     * IHMCON,GR)
C     --------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      LOGICAL SOME
      DIMENSION SI1(*), SI2(*), CI(NSOCI,MAXP),AB(NSOCI,MAXP)
      DIMENSION Q(NSOCI)
      DIMENSION F((MAXW1*(MAXW1+1))/2),EF(MAXW1,MAXW1),EL(MAXW1)
      DIMENSION EC(MAXP,MAXP),IWRK2(MAXW1+NA+43),B(8*MAXW1)
      DIMENSION SPIN(MAXP)
      DIMENSION IFA(0:NACT,0:NACT)
      DIMENSION IFE(0:NEXT,0:2)
      DIMENSION IOB(NOCC)
C
      DIMENSION INDEX((NOCC*(NOCC+1))/2 + 1)
C
      DIMENSION ITAB(NSYM),IMUL(NSYM,NSYM)
      DIMENSION ISYMA(NATT),ISYMB(NBTT)
      DIMENSION ISPA(NATT),ISPB(NBTT)
      DIMENSION ISAS0(NSYM+1),ISBS0(NSYM+1)
      DIMENSION ISAS1(NSYM),ISBS1(NSYM)
      DIMENSION ISAS2(NSYM),ISBS2(NSYM)
      DIMENSION ISAC(NATT),ISBC(NBTT)
C
      DIMENSION IACON1(NA),IBCON1(NA)
      DIMENSION IWRK1(KST)
      DIMENSION IACON2(NA),IBCON2(NA)
      DIMENSION IPOSA00(NSYM*(NA*(NACT-NA)))
      DIMENSION IPERA00(NSYM*(NA*(NACT-NA)))
      DIMENSION IIND100(NSYM*(NA*(NACT-NA)))
      DIMENSION IMMC00(NSYM)
      DIMENSION IPOSA01(NSYM*NA*NEXT)
      DIMENSION IPERA01(NSYM*NA*NEXT)
      DIMENSION IIND101(NSYM*NA*NEXT)
      DIMENSION IMMC01(NSYM)
      DIMENSION IPOSA11(((NA-1)*(NACT-NA+1)+(NEXT-1))*NSYM)
      DIMENSION IPERA11(((NA-1)*(NACT-NA+1)+(NEXT-1))*NSYM)
      DIMENSION IIND111(((NA-1)*(NACT-NA+1)+(NEXT-1))*NSYM)
      DIMENSION IMMC11(NSYM)
      DIMENSION IPOSA12((NA-1)*(NEXT-1)*NSYM)
      DIMENSION IPERA12((NA-1)*(NEXT-1)*NSYM)
      DIMENSION IIND112((NA-1)*(NEXT-1)*NSYM)
      DIMENSION IMMC12(NSYM)
      DIMENSION IPOSA22(((NA-2)*(NACT-NA+2)+2*(NEXT-2))*NSYM)
      DIMENSION IPERA22(((NA-2)*(NACT-NA+2)+2*(NEXT-2))*NSYM)
      DIMENSION IIND122(((NA-2)*(NACT-NA+2)+2*(NEXT-2))*NSYM)
      DIMENSION IMMC22(NSYM)
      DIMENSION IHMCON(K)
      DIMENSION GR(MAXP)
C
C   Code to do a full SO CI almost completely directly.  Written by
C   J. Ivanic '01.  This code makes use of symmetry.
C
C   THIS SHOULD REALLY ONLY BE CALLED USING DETSO, UNLESS YOU REALLY
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
C
      IF (SOME) CALL TSECND(E0)
      ISTAT=0
C
      DO 7 I=1,(NOCC*(NOCC+1))/2 + 1
         INDEX(I) = (I*(I-1))/2
    7 CONTINUE
C
      DO 20 II=1,KST
         DO 30 JJ=1,NSOCI
            CI(JJ,II) = 0.0D+00
            AB(JJ,II) = 0.0D+00
   30    CONTINUE
   20 CONTINUE
C
C
      IF (NB.EQ.0) NB0F = 0
C
C   Initial setup, first work out diagonal elements.
C
      CALL GETQSO(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,IACON1,IBCON1,
     *     INDEX,Q,ISYMA,ISYMB,ISPA,ISPB,ISAS0,ISBS0,ISAS1,ISBS1,
     *     ISAS2,ISBS2,ISAC,ISBC,NSYM)
C
C   Read initial guess vectors for 0th order space from disk.
C
      CALL SEQOPN(NFT12,'CIVECTR','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQREW(NFT12)
      READ(NFT12,ERR=1111,END=1111) NSTATE,NVECS
      IF (NSTATE+NVECS.EQ.0) GOTO 1111
      IF (NVECS.NE.NCI) THEN
         IF (SOME) WRITE(IW,9005)
         CALL ABRT
         STOP
      ENDIF
      IF (NSTATE.LT.KST) THEN
         IF (SOME)WRITE(IW,9004)
         CALL ABRT
         STOP
      ENDIF
      GOTO 1122
C
 1111 IF(SOME) WRITE(IW,9003)
      CALL ABRT
      STOP
C
 1122 CONTINUE
C
      DO 100 ISTATE = 1,KST
         CALL SQREAD(NFT12,AB(1,ISTATE),NVECS)
         IF(NVECS.EQ.0) THEN
            IF (SOME) WRITE(IW,*)
     *         'UNEXPECTED END OF FILE ON UNIT',NFT12
            CALL ABRT
            STOP
         END IF
  100 CONTINUE
C
      IF (SOME) WRITE(IW,9007)
C
C   Now to reorder the read 0th order CI coefficients to correspond
C   with the ordering scheme for FSOCI wavefunction.
C
      CALL REZOSO(CI,AB,KST,NSOCI,ISYMA,ISPA,ISBS0,ISBS1,NSYM)
C
      IF (NA.EQ.NB) THEN
      DO 856 I=1,KST
         IWRK1(I)=INT(SPIN(I) + 0.3D+00)
         IWRK2(I)=I
  856 CONTINUE
      ENDIF
C
      IF(SOME) THEN
         CALL TSECND(E1)
         ELAP = E1 - E0
         E0 = E1
         WRITE(IW,9010) ELAP
      END IF
C
      IF (NA.NE.NB) THEN
      IF (KST.GT.1) THEN
      CALL RINSO0(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,KST,IOB,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA00,IPERA00,IIND100,IMMC00,
     *    IPOSA01,IPERA01,IIND101,IMMC01)
      CALL RINSO1(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,KST,IOB,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA11,IPERA11,IIND111,IMMC11,
     *    IPOSA12,IPERA12,IIND112,IMMC12)
      CALL RINSO2(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,KST,IOB,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBC,NSYM,
     *    IPOSA22,IPERA22,IIND122,IMMC22)
      CALL RINSOB(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IBCON2,IFA,IFE,INDEX,AB,KST,Q,IOB,
     *    IMUL,ISYMB,ISPA,ISPB,ISAS0,ISAS1,ISAS2,ISAC,NSYM)
      ELSE
      CALL RINSO01(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,IOB,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA00,IPERA00,IIND100,IMMC00,
     *    IPOSA01,IPERA01,IIND101,IMMC01)
      CALL RINSO11(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,IOB,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA11,IPERA11,IIND111,IMMC11,
     *    IPOSA12,IPERA12,IIND112,IMMC12)
      CALL RINSO21(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,IOB,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBC,NSYM,
     *    IPOSA22,IPERA22,IIND122,IMMC22)
      CALL RINSOB1(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IBCON2,IFA,IFE,INDEX,AB,Q,IOB,
     *    IMUL,ISYMB,ISPA,ISPB,ISAS0,ISAS1,ISAS2,ISAC,NSYM)
      ENDIF
      ELSE
      IF (KST.GT.1) THEN
      CALL SINSO0(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,KST,IOB,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA00,IPERA00,IIND100,IMMC00,
     *    IPOSA01,IPERA01,IIND101,IMMC01)
      CALL SINSO1(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,KST,IOB,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA11,IPERA11,IIND111,IMMC11,
     *    IPOSA12,IPERA12,IIND112,IMMC12)
      CALL SINSO2(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IFA,IFE,INDEX,AB,KST,Q,IOB,
     *    IMUL,ISYMA,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA22,IPERA22,IIND122,IMMC22,IWRK1,IWRK2)
      ELSE
      CALL SINSO01(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,IOB,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA00,IPERA00,IIND100,IMMC00,
     *    IPOSA01,IPERA01,IIND101,IMMC01)
      CALL SINSO11(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,IOB,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA11,IPERA11,IIND111,IMMC11,
     *    IPOSA12,IPERA12,IIND112,IMMC12)
      CALL SINSO21(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IFA,IFE,INDEX,AB,Q,IOB,
     *    IMUL,ISYMA,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA22,IPERA22,IIND122,IMMC22,IWRK1,IWRK2)
      ENDIF
      ENDIF
C
      IF(SOME) THEN
         CALL TSECND(E1)
         ELAP = E1 - E0
         WRITE(IW,9020) ELAP
      END IF
C
      DO 13 II=1,KST
         EL(II) = 0.0D+00
         DO 15 KK=1,NSOCI
            EL(II) = EL(II) + CI(KK,II)*AB(KK,II)
   15    CONTINUE
   13 CONTINUE
      DO 555 II=1,MAXP
         DO 677 JJ=1,II-1
            EC(II,JJ) = 0.0D+00
            EF(II,JJ) = 0.0D+00
            EC(JJ,II) = 0.0D+00
            EF(JJ,II) = 0.0D+00
  677 CONTINUE
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
      INIT = 1
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
         CALL TRAN(CI,NSOCI,MAXW1,EF,IP,EC,KST)
         CALL TRAN(AB,NSOCI,MAXW1,EF,IP,EC,KST)
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
      DO 80 II=1,NSOCI
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
         CALL TRAN(CI,NSOCI,MAXW1,EF,IP,EC,KST)
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
         CALL TRAN(CI,NSOCI,MAXW1,EF,IP,EC,KST)
         CALL TRAN(AB,NSOCI,MAXW1,EF,IP,EC,KST)
         NTCON = NTCON - NUMC
      DO 4233 II=1,NTCON
         IHMCON(II) = IWRK2(II)
 4233 CONTINUE
            DO 74 II=1,MAXP
               DO 75 JJ=1,MAXP
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
         RETURN
      ENDIF
C
      IF (INIT.EQ.1) THEN
         INIT=0
         GOTO 9068
      ENDIF
      DO 68 JJ=IP+1,IP+NTCON
         IL = IHMCON(JJ-IP)
         DO 63 II=1,NSOCI
            CI(II,JJ) = CI(II,JJ)/(EL(IL) - Q(II))
   63    CONTINUE
   68 CONTINUE
 9068 CONTINUE
C
C  If Ms=0, impose restriction on the CI coefficients.
C
      IF (NA.EQ.NB) THEN
      DO 1110 II=1,NATT
         ISA1 = ISYMA(II)
         ICIT = ISPA(II)
         INB = ISPB(II)
         IENDO = ISBS1(ISA1)-1
         IF (II.GT.NA0F.AND.II.LE.NA2S) IENDO = ISBS2(ISA1)-1
         DO 2222 INB1=ISBS0(ISA1),IENDO
            NEND = ISBC(INB1)
            IF (NEND.GT.II) GOTO 1110
            ICI1= ICIT + ISPB(NEND)
            IF (NEND.EQ.II) THEN
               DO 3232 KJ=1,NTCON
                  NV = IHMCON(KJ)
                  IPS = (IWRK1(NV)/2)
                  IF ((IPS+IPS).NE.IWRK1(NV)) CI(ICI1,KJ+IP) = 0.0D+00
 3232          CONTINUE
               GOTO 1110
            ENDIF
C
            ICI2 = ISPA(NEND) + INB
            DO 3331 KJ=1,NTCON
            NV = IHMCON(KJ)
            IS = (-1)**IWRK1(NV)
            CI(ICI2,KJ+IP) = IS*CI(ICI1,KJ+IP)
 3331       CONTINUE
 2222    CONTINUE
 1110 CONTINUE
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
         DO 81 JJ=1,NSOCI
            ROV = ROV + CI(JJ,KK)*CI(JJ,II)
   81    CONTINUE
         DO 90 JJ=1,NSOCI
           CI(JJ,KK) = CI(JJ,KK) - ROV*CI(JJ,II)
   90    CONTINUE
   86 CONTINUE
C
      RNOR = 0.0D+00
      DO 40 II=1,NSOCI
         RNOR = RNOR + CI(II,KK)*CI(II,KK)
   40 CONTINUE
      RNOR = SQRT(RNOR)
      DO 42 II=1,NSOCI
         CI(II,KK) = CI(II,KK)/RNOR
   42 CONTINUE
   97 CONTINUE
C
      IP = IP + 1
C
C     Now to return the new part of Ab
C
      IF (NA.NE.NB) THEN
      IF (NTCON.GT.1) THEN
      CALL RINSO0(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI(1,IP),
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB(1,IP),NTCON,
     *    IOB,ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA00,IPERA00,IIND100,IMMC00,
     *    IPOSA01,IPERA01,IIND101,IMMC01)
      CALL RINSO1(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI(1,IP),
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB(1,IP),NTCON,
     *    IOB,ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA11,IPERA11,IIND111,IMMC11,
     *    IPOSA12,IPERA12,IIND112,IMMC12)
      CALL RINSO2(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI(1,IP),
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB(1,IP),NTCON,
     *    IOB,ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBC,NSYM,
     *    IPOSA22,IPERA22,IIND122,IMMC22)
      CALL RINSOB(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI(1,IP),
     *    IACON1,IBCON1,IBCON2,IFA,IFE,INDEX,AB(1,IP),NTCON,Q,
     *    IOB,IMUL,ISYMB,ISPA,ISPB,ISAS0,ISAS1,
     *    ISAS2,ISAC,NSYM)
      ELSE
      CALL RINSO01(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI(1,IP),
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB(1,IP),IOB,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA00,IPERA00,IIND100,IMMC00,
     *    IPOSA01,IPERA01,IIND101,IMMC01)
      CALL RINSO11(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI(1,IP),
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB(1,IP),IOB,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA11,IPERA11,IIND111,IMMC11,
     *    IPOSA12,IPERA12,IIND112,IMMC12)
      CALL RINSO21(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI(1,IP),
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB(1,IP),IOB,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBC,NSYM,
     *    IPOSA22,IPERA22,IIND122,IMMC22)
      CALL RINSOB1(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI(1,IP),
     *    IACON1,IBCON1,IBCON2,IFA,IFE,INDEX,AB(1,IP),Q,
     *    IOB,IMUL,ISYMB,ISPA,ISPB,ISAS0,ISAS1,
     *    ISAS2,ISAC,NSYM)
      ENDIF
      ELSE
      IF (NTCON.GT.1) THEN
      CALL SINSO0(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI(1,IP),
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB(1,IP),NTCON,
     *    IOB,ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA00,IPERA00,IIND100,IMMC00,
     *    IPOSA01,IPERA01,IIND101,IMMC01)
      CALL SINSO1(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI(1,IP),
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB(1,IP),NTCON,
     *    IOB,ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA11,IPERA11,IIND111,IMMC11,
     *    IPOSA12,IPERA12,IIND112,IMMC12)
      CALL SINSO2(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI(1,IP),
     *    IACON1,IBCON1,IACON2,IFA,IFE,INDEX,AB(1,IP),NTCON,Q,
     *    IOB,IMUL,ISYMA,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA22,IPERA22,IIND122,IMMC22,IWRK1,IHMCON)
      ELSE
      CALL SINSO01(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI(1,IP),
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB(1,IP),IOB,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA00,IPERA00,IIND100,IMMC00,
     *    IPOSA01,IPERA01,IIND101,IMMC01)
      CALL SINSO11(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI(1,IP),
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB(1,IP),IOB,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA11,IPERA11,IIND111,IMMC11,
     *    IPOSA12,IPERA12,IIND112,IMMC12)
      CALL SINSO21(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI(1,IP),
     *    IACON1,IBCON1,IACON2,IFA,IFE,INDEX,AB(1,IP),Q,IOB,
     *    IMUL,ISYMA,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA22,IPERA22,IIND122,IMMC22,IWRK1,IHMCON)
      ENDIF
      ENDIF
C
C
      IP = IP + NTCON - 1
C
C  Make the new matrix elements between the CI vectors.
C
      IX = 0
      DO 103 II=1,IP
          DO 102 JJ=1,II
          IX = IX + 1
            F(IX) = 0.0D+00
            DO 115 KK=1,NSOCI
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
C   Check to see order of converging states.
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
      DO 800 II=1,IP
         POV = 0.0D+00
         DO 813 JJ=1,IP
            UIT = 0.0D+00
            DO 823 KK=1,IP
               UIT = UIT + EF(KK,II)*EC(KK,JJ)
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
      DO 543 II=1,KST
         IWRK1(II) = INT(SPIN(II) + 0.3D+00)
  543 CONTINUE
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
      IF (SOME) THEN
      WRITE(IW,*) 'RAN OUT OF ITERATIONS'
      WRITE(IW,*) 'NO. OF STATES CONVERGED :',IL-1
      WRITE(IW,*)
      ENDIF
C
      ISTAT = 1
      RETURN
C
 9003 FORMAT(1X,'ERROR READING INITIAL ZEROTH SPACE VECTORS')
 9004 FORMAT(1X,'NOT ENOUGH INITIAL ZEROTH SPACE VECTORS')
 9005 FORMAT(1X,'ERROR, NO OF VECTORS STORED NOT EQUAL TO NCI')
 9007 FORMAT(/1X,'ZEROTH ORDER CI VECTORS READ FROM DISK')
 9010 FORMAT(1X,'TIME TAKEN FOR SETTING UP:',F13.1)
 9020 FORMAT(1X,'INITIAL MR-CI ITERATION TIME:',F13.1)
 9040 FORMAT(/1X,'ITERATION',6X,'ENERGY',11X,'GRADIENT')
 9050 FORMAT(1X,I5,F20.10,F15.8)
 9060 FORMAT(/1X,'CONVERGED STATE',I5,' ENERGY=',F20.10,' IN',
     *           I5,' ITERS'/)
      END
C
C*MODULE FSODCI  *DECK GETQSO
C     -----------------------------------------------------
      SUBROUTINE GETQSO(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,
     *   IACON1,IBCON1,INDEX,Q,ISYMA,ISYMB,ISPA,ISPB,ISAS0,
     *   ISBS0,ISAS1,ISBS1,ISAS2,ISBS2,ISAC,ISBC,NSYM)
C     -----------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION SI1(*),SI2(*)
      DIMENSION IACON1(NA),IBCON1(NA)
      DIMENSION INDEX((NOCC*(NOCC+1))/2+1),Q(NSOCI)
      DIMENSION ISYMA(NATT),ISYMB(NBTT)
      DIMENSION ISPA(NATT),ISPB(NBTT)
      DIMENSION ISAS0(NSYM+1),ISBS0(NSYM+1)
      DIMENSION ISAS1(NSYM),ISBS1(NSYM)
      DIMENSION ISAS2(NSYM),ISBS2(NSYM)
      DIMENSION ISAC(NATT),ISBC(NBTT)
C
      DO 13 II=1,NSOCI
         Q(II) = 0.0D+00
   13 CONTINUE
C
      DO 30 II=1,NA
         IACON1(II) = II
   30 CONTINUE
C
C   Big loop over 0th order alpha strings.
C
      DO 2000 IJK=1,NA0F
         ICAT = ISPA(IJK)
         ISA = ISYMA(IJK)
         C = 0.0D+00
         DO 67 II=1,NA
            I1 = IACON1(II)
            IND1 = INDEX(I1+1)
            C = C + SI1(IND1)
            DO 64 JJ=1,II-1
               I2 = IACON1(JJ)
               IND2 = INDEX(I2+1)
               INDM = INDEX(I1)+I2
               J1 = INDEX(IND1)+IND2
               J2 = INDEX(INDM+1)
               C = C + SI2(J1) - SI2(J2)
   64       CONTINUE
   67    CONTINUE
C
C   0th order beta strings
C
         DO 47 I=1,NB
            IBCON1(I) = I
   47    CONTINUE
C
         NST = 1
         DO 56 INB1 = ISBS0(ISA),ISBS1(ISA)-1
            NEND = ISBC(INB1)
            DO 58 KK=NST,NEND-1
               CALL ADVANC(IBCON1,NB,NACT)
   58       CONTINUE
            ICAT = ICAT + 1
            D = 0.0D+00
            DO 73 JJ=1,NB
               I2 = IBCON1(JJ)
               IND2 = INDEX(I2+1)
               DO 77 KK=1,NA
                  I1 = IACON1(KK)
                  IND1 = INDEX(I1+1)
                  IMA = MAX(IND1,IND2)
                  IMI = MIN(IND1,IND2)
                  J2 = INDEX(IMA)+IMI
                  D = D + SI2(J2)
   77          CONTINUE
   73       CONTINUE
            T = C + D
            Q(ICAT) = Q(ICAT) + T
            NST = NEND
   56    CONTINUE
C
C   1st order beta strings
C
         DO 147 I=1,NB-1
            IBCON1(I) = I
  147    CONTINUE
         NSTF = 1
C
         DO 156 INB1 = ISBS1(ISA),ISBS2(ISA)-1
            NIBP = ISBC(INB1)  - NB0F
            NX = (NIBP-1)/NB1E + 1
            NY = NIBP - (NX-1)*NB1E
            DO 158 KK=NSTF,NX-1
               CALL ADVANC(IBCON1,NB-1,NACT)
  158       CONTINUE
            IBCON1(NB) = NY+NACT
            ICAT = ICAT + 1
            D = 0.0D+00
            DO 173 JJ=1,NB
               I2 = IBCON1(JJ)
               IND2 = INDEX(I2+1)
               DO 177 KK=1,NA
                  I1 = IACON1(KK)
                  IND1 = INDEX(I1+1)
                  IMA = MAX(IND1,IND2)
                  IMI = MIN(IND1,IND2)
                  J2 = INDEX(IMA)+IMI
                  D = D + SI2(J2)
  177          CONTINUE
  173       CONTINUE
            T = C + D
            Q(ICAT) = Q(ICAT) + T
            NSTF = NX
  156    CONTINUE
C
C   2nd order beta strings
C
         IF (NB.LT.2) GOTO 253
         DO 247 I=1,NB-2
            IBCON1(I) = I
  247    CONTINUE
         IBCON1(NB-1) = 1
         IBCON1(NB) = 2
         NSTF = 1
         NSTE = 1
C
         DO 256 INB1 = ISBS2(ISA),ISBS0(ISA+1)-1
            NIBP = ISBC(INB1) - NB2S
            NX = (NIBP-1)/NB2E + 1
            NY = NIBP - (NX-1)*NB2E
            DO 258 KK=NSTF,NX-1
               CALL ADVANC(IBCON1,NB-2,NACT)
  258       CONTINUE
            IF (NY.LT.NSTE) THEN
               IBCON1(NB-1) = 1
               IBCON1(NB)   = 2
               NSTE = 1
            ENDIF
            DO 260 KK=NSTE,NY-1
               CALL ADVANC(IBCON1(NB-1),2,NEXT)
  260       CONTINUE
            ICAT = ICAT + 1
            D = 0.0D+00
            DO 273 JJ=1,NB-2
               I2 = IBCON1(JJ)
               IND2 = INDEX(I2+1)
               DO 277 KK=1,NA
                  I1 = IACON1(KK)
                  IND1 = INDEX(I1+1)
                  IMA = MAX(IND1,IND2)
                  IMI = MIN(IND1,IND2)
                  J2 = INDEX(IMA)+IMI
                  D = D + SI2(J2)
  277          CONTINUE
  273       CONTINUE
C
            DO 275 JJ=NB-1,NB
               I2 = IBCON1(JJ) + NACT
               IND2 = INDEX(I2+1)
               DO 279 KK=1,NA
                  I1 = IACON1(KK)
                  IND1 = INDEX(I1+1)
                  J2 = INDEX(IND2)+IND1
                  D = D + SI2(J2)
  279          CONTINUE
  275       CONTINUE
            T = C + D
            Q(ICAT) = Q(ICAT) + T
            NSTF = NX
            NSTE = NY
  256    CONTINUE
C
  253    CONTINUE
         CALL ADVANC(IACON1,NA,NACT)
 2000 CONTINUE
C
C   Big loop over 1st order alpha strings.
C
      DO 330 II=1,NA-1
         IACON1(II) = II
  330 CONTINUE
      ICOUNT = NA0F
C
      DO 3000 IJK=1,NA1F
         DO 2900 KJI=NACT+1,NOCC
            IACON1(NA) = KJI
            ICOUNT = ICOUNT + 1
            ICAT = ISPA(ICOUNT)
            ISA = ISYMA(ICOUNT)
            C = 0.0D+00
C
            DO 367 II=1,NA
               I1 = IACON1(II)
               IND1 = INDEX(I1+1)
               C = C + SI1(IND1)
               DO 364 JJ=1,II-1
                  I2 = IACON1(JJ)
                  IND2 = INDEX(I2+1)
                  INDM = INDEX(I1)+I2
                  J1 = INDEX(IND1)+IND2
                  J2 = INDEX(INDM+1)
                  C = C + SI2(J1) - SI2(J2)
  364          CONTINUE
  367       CONTINUE
C
C   0th order beta strings
C
            DO 347 I=1,NB
            IBCON1(I) = I
  347       CONTINUE
C
            NST = 1
            DO 356 INB1 = ISBS0(ISA),ISBS1(ISA)-1
               NEND = ISBC(INB1)
               DO 358 KK=NST,NEND-1
                  CALL ADVANC(IBCON1,NB,NACT)
  358          CONTINUE
               ICAT = ICAT + 1
               D = 0.0D+00
               DO 373 JJ=1,NB
                  I2 = IBCON1(JJ)
                  IND2 = INDEX(I2+1)
                  DO 377 KK=1,NA
                     I1 = IACON1(KK)
                     IND1 = INDEX(I1+1)
                     IMA = MAX(IND1,IND2)
                     IMI = MIN(IND1,IND2)
                     J2 = INDEX(IMA)+IMI
                     D = D + SI2(J2)
  377             CONTINUE
  373          CONTINUE
               T = C + D
               Q(ICAT) = Q(ICAT) + T
               NST = NEND
  356       CONTINUE
C
C   1st order beta strings
C
            DO 447 I=1,NB-1
               IBCON1(I) = I
  447       CONTINUE
            NSTF = 1
C
            DO 456 INB1 = ISBS1(ISA),ISBS2(ISA)-1
               NIBP = ISBC(INB1)  - NB0F
               NX = (NIBP-1)/NB1E + 1
               NY = NIBP - (NX-1)*NB1E
               DO 458 KK=NSTF,NX-1
                  CALL ADVANC(IBCON1,NB-1,NACT)
  458          CONTINUE
               IBCON1(NB) = NY+NACT
               ICAT = ICAT + 1
               D = 0.0D+00
               DO 473 JJ=1,NB
                  I2 = IBCON1(JJ)
                  IND2 = INDEX(I2+1)
                  DO 477 KK=1,NA
                     I1 = IACON1(KK)
                     IND1 = INDEX(I1+1)
                     IMA = MAX(IND1,IND2)
                     IMI = MIN(IND1,IND2)
                     J2 = INDEX(IMA)+IMI
                     D = D + SI2(J2)
  477             CONTINUE
  473          CONTINUE
               T = C + D
               Q(ICAT) = Q(ICAT) + T
               NSTF = NX
  456       CONTINUE
C
 2900    CONTINUE
         CALL ADVANC(IACON1,NA-1,NACT)
 3000 CONTINUE
C
C   Big loop over 2nd order alpha strings.
C
      DO 530 II=1,NA-2
         IACON1(II) = II
  530 CONTINUE
      ICOUNT = NA2S
C
      DO 4000 IJK = 1,NA2F
         IACON1(NA-1) = 1 + NACT
         IACON1(NA) = 2 + NACT
         DO 3900 KJI=1,NA2E
            ICOUNT = ICOUNT + 1
            ICAT = ISPA(ICOUNT)
            ISA = ISYMA(ICOUNT)
            C = 0.0D+00
C
            DO 567 II=1,NA
               I1 = IACON1(II)
               IND1 = INDEX(I1+1)
               C = C + SI1(IND1)
               DO 564 JJ=1,II-1
                  I2 = IACON1(JJ)
                  IND2 = INDEX(I2+1)
                  INDM = INDEX(I1)+I2
                  J1 = INDEX(IND1)+IND2
                  J2 = INDEX(INDM+1)
                  C = C + SI2(J1) - SI2(J2)
  564          CONTINUE
  567       CONTINUE
C
C   0th order beta strings
C
            DO 547 I=1,NB
               IBCON1(I) = I
  547       CONTINUE
C
            NST = 1
            DO 856 INB1 = ISBS0(ISA),ISBS1(ISA)-1
               NEND = ISBC(INB1)
               DO 858 KK=NST,NEND-1
                  CALL ADVANC(IBCON1,NB,NACT)
  858          CONTINUE
               ICAT = ICAT + 1
               D = 0.0D+00
               DO 873 JJ=1,NB
                  I2 = IBCON1(JJ)
                  IND2 = INDEX(I2+1)
                  DO 877 KK=1,NA
                     I1 = IACON1(KK)
                     IND1 = INDEX(I1+1)
                     IMA = MAX(IND1,IND2)
                     IMI = MIN(IND1,IND2)
                     J2 = INDEX(IMA)+IMI
                     D = D + SI2(J2)
  877             CONTINUE
  873          CONTINUE
               T = C + D
               Q(ICAT) = Q(ICAT) + T
               NST = NEND
  856       CONTINUE
C
            CALL ADVANC(IACON1(NA-1),2,NOCC)
 3900    CONTINUE
         CALL ADVANC(IACON1,NA-2,NACT)
 4000 CONTINUE
C
C  Now for the beta part.
C
      DO 600 II=1,NB
         IBCON1(II) = II
  600 CONTINUE
C
C   Big loop over 0th order beta strings.
C
      DO 5000 IJK=1,NB0F
         IPB = ISPB(IJK)
         ISB = ISYMB(IJK)
         C = 0.0D+00
         DO 667 II=1,NB
            I1 = IBCON1(II)
            IND1 = INDEX(I1+1)
            C = C + SI1(IND1)
            DO 664 JJ=1,II-1
               I2 = IBCON1(JJ)
               IND2 = INDEX(I2+1)
               INDM = INDEX(I1)+I2
               J1 = INDEX(IND1)+IND2
               J2 = INDEX(INDM+1)
               C = C + SI2(J1) - SI2(J2)
  664       CONTINUE
  667    CONTINUE
C
C  All alpha strings
C
         DO 693 INA1 = ISAS0(ISB),ISAS0(ISB+1)-1
            IDA = ISAC(INA1)
            ICIA = ISPA(IDA)
            ICIT = ICIA + IPB
            Q(ICIT) = Q(ICIT) + C
  693    CONTINUE
C
         CALL ADVANC(IBCON1,NB,NACT)
 5000 CONTINUE
C
C   Big loop over 1st order beta strings.
C
      DO 730 II=1,NB-1
         IBCON1(II) = II
  730 CONTINUE
      ICOUNT = NB0F
C
      DO 6000 IJK=1,NB1F
         DO 5900 KJI=NACT+1,NOCC
            IBCON1(NB) = KJI
            ICOUNT = ICOUNT + 1
            IPB = ISPB(ICOUNT)
            ISB = ISYMB(ICOUNT)
            C = 0.0D+00
C
            DO 767 II=1,NB
               I1 = IBCON1(II)
               IND1 = INDEX(I1+1)
               C = C + SI1(IND1)
               DO 764 JJ=1,II-1
                  I2 = IBCON1(JJ)
                  IND2 = INDEX(I2+1)
                  INDM = INDEX(I1)+I2
                  J1 = INDEX(IND1)+IND2
                  J2 = INDEX(INDM+1)
                  C = C + SI2(J1) - SI2(J2)
  764          CONTINUE
  767       CONTINUE
C
C  0th and 1st order alpha strings
C
         DO 793 INA1 = ISAS0(ISB),ISAS2(ISB)-1
            IDA = ISAC(INA1)
            ICIA = ISPA(IDA)
            ICIT = ICIA + IPB
            Q(ICIT) = Q(ICIT) + C
  793    CONTINUE
C
 5900    CONTINUE
         IF (NB.LE.1) GOTO 6000
         CALL ADVANC(IBCON1,NB-1,NACT)
 6000 CONTINUE
C
C   Big loop over 2nd order beta strings.
C
      IF (NB.LT.2) GOTO 7200
      DO 830 II=1,NB-2
         IBCON1(II) = II
  830 CONTINUE
      ICOUNT = NB2S
C
      DO 7000 IJK = 1,NB2F
         IBCON1(NB-1) = 1 + NACT
         IBCON1(NB) = 2 + NACT
         DO 6900 KJI=1,NB2E
            ICOUNT = ICOUNT + 1
            IPB = ISPB(ICOUNT)
            ISB = ISYMB(ICOUNT)
            C = 0.0D+00
C
            DO 867 II=1,NB
               I1 = IBCON1(II)
               IND1 = INDEX(I1+1)
               C = C + SI1(IND1)
               DO 864 JJ=1,II-1
                  I2 = IBCON1(JJ)
                  IND2 = INDEX(I2+1)
                  INDM = INDEX(I1)+I2
                  J1 = INDEX(IND1)+IND2
                  J2 = INDEX(INDM+1)
                  C = C + SI2(J1) - SI2(J2)
  864          CONTINUE
  867       CONTINUE
C
C  0th order alpha strings
C
         DO 893 INA1 = ISAS0(ISB),ISAS1(ISB)-1
            IDA = ISAC(INA1)
            ICIA = ISPA(IDA)
            ICIT = ICIA + IPB
            Q(ICIT) = Q(ICIT) + C
  893    CONTINUE
C
            CALL ADVANC(IBCON1(NB-1),2,NOCC)
 6900    CONTINUE
         IF (NB.LE.2) GOTO 7000
         CALL ADVANC(IBCON1,NB-2,NACT)
 7000 CONTINUE
C
 7200 CONTINUE
      RETURN
      END
C
C*MODULE FSODCI  *DECK REZOSO
C     --------------------------------------------------------------
      SUBROUTINE REZOSO(CI,AB,KST,NSOCI,ISYMA,ISPA,ISBS0,ISBS1,NSYM)
C     --------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION CI(NSOCI,KST),AB(NSOCI,KST)
      DIMENSION ISYMA(NATT),ISPA(NATT)
      DIMENSION ISBS0(NSYM+1),ISBS1(NSYM)
C
      ICOUNT = 0
      DO 1000 II=1,NA0F
         ICAT = ISPA(II)
         ISA = ISYMA(II)
         DO 500 JJ=ISBS0(ISA),ISBS1(ISA)-1
            ICOUNT = ICOUNT + 1
            ICAT = ICAT + 1
            DO 400 IJK=1,KST
               CI(ICAT,IJK) = AB(ICOUNT,IJK)
  400       CONTINUE
  500    CONTINUE
 1000 CONTINUE
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK RINSO0
C     -----------------------------------------------------------
      SUBROUTINE RINSO0(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,NV,IOX,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA00,IPERA00,IIND100,IMMC00,
     *    IPOSA01,IPERA01,IIND101,IMMC01)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION SI1(*),SI2(*),AB(NSOCI,NV),CI(NSOCI,NV)
      DIMENSION IFA(0:NACT,0:NACT),IFE(0:NEXT,0:2)
      DIMENSION IACON1(NA),IBCON1(NA),IACON2(NA),IBCON2(NA)
      DIMENSION INDEX((NOCC*(NOCC+1))/2 + 1)
      DIMENSION IOX(NOCC)
      DIMENSION ITAB(NSYM),IMUL(NSYM,NSYM)
      DIMENSION ISYMA(NATT),ISYMB(NATT),ISPA(NATT),ISPB(NATT)
      DIMENSION ISBS0(NSYM+1),ISBS1(NSYM),ISBS2(NSYM)
      DIMENSION ISBC(NATT)
      DIMENSION IPOSA00((NA*(NACT-NA)),NSYM)
      DIMENSION IPERA00((NA*(NACT-NA)),NSYM)
      DIMENSION IIND100((NA*(NACT-NA)),NSYM)
      DIMENSION IMMC00(NSYM)
      DIMENSION IPOSA01(NA*NEXT,NSYM)
      DIMENSION IPERA01(NA*NEXT,NSYM)
      DIMENSION IIND101(NA*NEXT,NSYM)
      DIMENSION IMMC01(NSYM)
C
      DO 13 II=1,NSOCI
         DO 12 JJ=1,NV
            AB(II,JJ) = 0.0D+00
   12    CONTINUE
   13 CONTINUE
C
C    --------------------------------------------
C    Loop over all 0th order alpha determinants.
C    --------------------------------------------
C
      DO 20 II=1,NA
         IACON1(II)=II
   20 CONTINUE
C
      DO 2000 IJK=1,NA0F
         ICAT = ISPA(IJK)
         ISA1 = ISYMA(IJK)
         ITAS = ITAB(ISA1)
C
         DO 45 II=1,NSYM
            IMMC00(II) = 0
            IMMC01(II) = 0
   45    CONTINUE
C
C   Loop single excitations from 0th space.
C
         DO 990 IA=1,NA
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C   Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 980 IGEL=IA,NA
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA) IEN=NACT
               DO 970 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
C
C  Storage here for later.
C
                  IPET = POSDET(NACT,NA,IACON2,IFA)
                  IMMC00(ISYMA(IPET)) = IMMC00(ISYMA(IPET)) + 1
                  IPOSA00(IMMC00(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
                  IPER11 = (-1)**IPER1
                  IPERA00(IMMC00(ISYMA(IPET)),ISYMA(IPET)) = IPER11
                  IND = INDEX(JJ)+IO1
                  IIND100(IMMC00(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 900
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 905 IK=1,IA-1
                     J1  = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + IACON1(IK)
                     J2  = INDEX(IO1) + IACON1(IK)
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  905             CONTINUE
C
                  DO 907 IK=IA+1,IGEL
                     J1 = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + IACON1(IK)
                     J2 = INDEX(IACON1(IK)) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  907             CONTINUE
C
                  DO 908 IK=IGEL+1,NA
                     J1 = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(IACON1(IK)) + JJ
                     J2 = INDEX(IACON1(IK)) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  908             CONTINUE
C
C  Loop over all beta strings now. 0th order first.
C
                  DO 910 II=1,NB
                     IBCON1(II) = II
  910             CONTINUE
C
                  NST = 1
                  DO 915 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 913 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
  913                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 918 IK=1,NB
                        J1 = INDEX(IBCON1(IK)+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
  918                CONTINUE
C
                     T = (C+D)*IPER11
                     DO 920 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
  920                CONTINUE
C
                     NST = NEND
  915             CONTINUE
C
C  Loop over 1st order beta strings.
C
                  DO 922 II=1,NB-1
                     IBCON1(II) = II
  922             CONTINUE
                  NSTF = 1
C
                  DO 930 INB1 = ISBS1(ISA1),ISBS2(ISA1)-1
                     NIBP = ISBC(INB1) - NB0F
                     NX = (NIBP-1)/NB1E + 1
                     NY = NIBP - (NX-1)*NB1E
                     DO 924 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-1,NACT)
  924                CONTINUE
                     IBCON1(NB) = NY+NACT
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 926 IK=1,NB-1
                        J1 = INDEX(IBCON1(IK)+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
  926                CONTINUE
C
                     J1 = INDEX(IBCON1(NB)+1)
                     JJ1 = INDEX(J1) + IND
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER11
                     DO 928 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
  928                CONTINUE
C
                     NSTF = NX
  930             CONTINUE
C
C
C  Loop over 2nd order beta strings.
C
                  IF (NB.LT.2) GOTO 900
                  DO 932 II=1,NB-2
                     IBCON1(II) = II
  932             CONTINUE
                  IBCON1(NB-1) = 1 + NACT
                  IBCON1(NB) = 2 + NACT
                  NSTF = 1
                  NSTE = 1
C
                  DO 940 INB1 = ISBS2(ISA1),ISBS0(ISA1+1)-1
                     NIBP = ISBC(INB1) - NB2S
                     NX = (NIBP-1)/NB2E + 1
                     NY = NIBP - (NX-1)*NB2E
                     DO 934 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-2,NACT)
  934                CONTINUE
                     IF (NY.LT.NSTE) THEN
                        IBCON1(NB-1) = 1 + NACT
                        IBCON1(NB)   = 2 + NACT
                        NSTE = 1
                     ENDIF
                     DO 936 KK=NSTE,NY-1
                        CALL ADVANC(IBCON1(NB-1),2,NOCC)
  936                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 937 IK=1,NB-2
                        J1 = INDEX(IBCON1(IK)+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
  937                CONTINUE
C
                     DO 938 IK=NB-1,NB
                        J1 = INDEX(IBCON1(IK)+1)
                        JJ1 = INDEX(J1) + IND
                        D = D + SI2(JJ1)
  938                CONTINUE
C
                     T = (C+D)*IPER11
                     DO 939 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
  939                CONTINUE
C
                     NSTF = NX
                     NSTE = NY
C
  940             CONTINUE
C
  900             CONTINUE
C
C   Loop double excitations from 0th space.
C
                  DO 967 IAA=IA+1,NA
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
                     IF (JJ.GT.IIA) IPA=IPA-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA
C
                     DO 965 IGEL2=IGEL,NA
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA) IENAA=NACT
                        DO 963 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 963
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                           IPET = POSDET(NACT,NA,IBCON1,IFA)
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = (-1)**(IPER1 + IPER2)
C
C   Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           IF (IIA.GT.JJAA) THEN
                              I2 = INDEX(IIA) + JJAA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIA.LT.JJ) THEN
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIA
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                           DO 961 INB1 = ISBS0(ISA1),ISBS0(ISA1+1)-1
                              ICI1 = ICI1 + 1
                              ICI2 = ICI2 + 1
                              DO 960 KJ=1,NV
                            AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                            AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
  960                         CONTINUE
  961                      CONTINUE
C
  963                   CONTINUE
  965                CONTINUE
C
C  Loop over orbitals in external space, ie AA -> AE
C
                     DO 950 JJAA=NACT+1,NOCC
C
                        IS4 = IOX(JJAA)
                        IP2 = IMUL(IS3,IS4)
                        IF (IP1.NE.IP2) GOTO 950
C
                        CALL REDE01(IACON2,IBCON1,NA,IPA,JJAA,IPER2)
C
                        IPET = POSDET(NACT,NA-1,IBCON1,IFA)
                        IPET = (IPET-1)*NEXT + JJAA-NACT + NA0F
                        ICI1 = ICAT
                        ICI2 = ISPA(IPET)
                        IPERT = (-1)**(IPER1 + IPER2)
C
C  Make matrix element.
C
                        II1 = INDEX(JJAA) + IO1
                        I2 = INDEX(JJAA) + IIA
                        INX = INDEX(I2) + IND
                        IF (IIA.LT.JJ) THEN
                           II2 = INDEX(JJ) + IIA
                           INX2 = INDEX(II1) + II2
                        ELSE
                           II2 = INDEX(IIA) + JJ
                           INX2 = INDEX(II1) + II2
                        ENDIF
                        C = SI2(INX) - SI2(INX2)
                        T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 954 INB1 = ISBS0(ISA1),ISBS2(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                           DO 952 KJ=1,NV
                         AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                         AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
  952                      CONTINUE
  954                   CONTINUE
C
  950                CONTINUE
C
  967             CONTINUE
C
  970          CONTINUE
  980       CONTINUE
C
C  Loop over orbitals in external gap, ie  A -> E.
C
            DO 880 JJ=NACT+1,NOCC
C
               IS2 = IOX(JJ)
               IP1 = IMUL(IS2,IS1)
C
               CALL REDE01(IACON1,IACON2,NA,IA,JJ,IPER1)
C
C  Storage here for later.
C
C
               IPET = (POSDET(NACT,NA-1,IACON2,IFA)-1)*NEXT
               IPET = IPET + NA0F + (JJ-NACT)
               IMMC01(ISYMA(IPET)) = IMMC01(ISYMA(IPET)) + 1
               IPOSA01(IMMC01(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
               IPER11 = (-1)**IPER1
               IPERA01(IMMC01(ISYMA(IPET)),ISYMA(IPET)) = IPER11
               IND = INDEX(JJ)+IO1
               IIND101(IMMC01(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
               IF (IS1.NE.IS2) GOTO 800
               ICI1 = ICAT
               ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 805 IK=1,IA-1
                     J1  = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + IACON1(IK)
                     J2  = INDEX(IO1) + IACON1(IK)
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  805             CONTINUE
C
                  DO 807 IK=IA+1,NA
                     J1 = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + IACON1(IK)
                     J2 = INDEX(IACON1(IK)) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  807             CONTINUE
C
C  Loop over all beta dets now. 0th order first.
C
                  DO 810 II=1,NB
                     IBCON1(II) = II
  810             CONTINUE
C
                  NST = 1
                  DO 815 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 813 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
  813                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 818 IK=1,NB
                        J1 = INDEX(IBCON1(IK)+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
  818                CONTINUE
C
                     T = (C+D)*IPER11
                     DO 820 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
  820                CONTINUE
C
                     NST = NEND
  815             CONTINUE
C
C  Loop over 1st order beta strings.
C
                  DO 822 II=1,NB-1
                     IBCON1(II) = II
  822             CONTINUE
                  NSTF = 1
C
                  DO 830 INB1 = ISBS1(ISA1),ISBS2(ISA1)-1
                     NIBP = ISBC(INB1) - NB0F
                     NX = (NIBP-1)/NB1E + 1
                     NY = NIBP - (NX-1)*NB1E
                     DO 824 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-1,NACT)
  824                CONTINUE
                     IBCON1(NB) = NY+NACT
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 826 IK=1,NB-1
                        J1 = INDEX(IBCON1(IK)+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
  826                CONTINUE
C
                     J1 = INDEX(IBCON1(NB)+1)
                     IMA = MAX(IND,J1)
                     IMI = MIN(IND,J1)
                     JJ1 = INDEX(IMA) + IMI
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER11
                     DO 828 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
  828                CONTINUE
C
                     NSTF = NX
  830             CONTINUE
C
  800          CONTINUE
C
C   Loop double excitations from 0th space.
C
               DO 867 IAA=IA+1,NA
                  IIA = IACON1(IAA)
                  IS3 = IOX(IIA)
                  IPA = IAA - 1
C
C  Loop over external orbitals > JJ, ie AA -> EE
C
                  DO 865 JJAA=JJ+1,NOCC
C
                     IS4 = IOX(JJAA)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 865
C
                     CALL REDE01(IACON2,IBCON1,NA,IPA,JJAA,IPER2)
                     IBCON1(NA-1) = JJ-NACT
                     IBCON1(NA)   = JJAA-NACT
C
                     IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                     IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                     IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                     ICI1 = ICAT
                     ICI2 = ISPA(IPET)
                     IPERT = IPER1 + IPER2
                     IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                     II1 = INDEX(JJAA) + IO1
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(JJ) + IIA
                     INX2 = INDEX(II1) + II2
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 863 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                        DO 861 KJ=1,NV
                      AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                      AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
  861                   CONTINUE
  863                CONTINUE
C
  865             CONTINUE
C
  867          CONTINUE
C
  880       CONTINUE
C
  990    CONTINUE
C
C    Loop over all betas and their single excites now.
C
C   First loop over all 0th order betas
C
         DO 1010 II=1,NB
            IBCON1(II) = II
 1010    CONTINUE
C
         DO 1995 KJI=1,NB0F
            IPB1 = ISPB(KJI)
            ISB1 = ISYMB(KJI)
            ITBS = ITAB(ISB1)
            IMZZ0 = IMMC00(ITBS)
            IMZZ1 = IMMC01(ITBS)
            IMZZ = IMZZ0 + IMZZ1
            IF (IMZZ.EQ.0.AND.ISB1.NE.ITAS) GOTO 1994
            IC1 = ICAT + IPB1
C
C   Loop single excitations from 0th space.
C
         DO 1990 IB=1,NB
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C   Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 1980 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN=NACT
               DO 1970 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ10 = IMMC00(ITB2)
                  IMZ11 = IMMC01(ITB2)
                  IMZ1 = IMZ10 + IMZ11
C
                  IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 1975
                  IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1975
                  GOTO 1970
C
 1975             CONTINUE
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
                  IPOSB = POSDET(NACT,NB,IBCON2,IFA)
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1960 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                        DO 1962 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1962                   CONTINUE
 1960                CONTINUE
                     DO 1964 IAT=1,IMZ11
                        IC4 = IPOSA01(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITB2))
                        I2 = MIN(IOB,IIND101(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITB2)
                        DO 1963 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1963                   CONTINUE
 1964                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1955 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1957 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1957                   CONTINUE
 1955                CONTINUE
                     DO 1953 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                        DO 1954 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1954                   CONTINUE
 1953                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1950 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1952 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1952                   CONTINUE
 1950                CONTINUE
                     DO 1958 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        IC4 = IPOSA01(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                        DO 1951 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1951                   CONTINUE
 1958                CONTINUE
C
                  ENDIF
C
 1970          CONTINUE
 1980       CONTINUE
C
C  Loop over orbitals in external gap, ie  A -> E.
C
            DO 1880 JJ=NACT+1,NOCC
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ10 = IMMC00(ITB2)
               IMZ11 = IMMC01(ITB2)
               IMZ1 = IMZ10 + IMZ11
C
               IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 1875
               IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1875
               GOTO 1880
Cc
 1875          CONTINUE
               CALL REDE01(IBCON1,IBCON2,NB,IB,JJ,IPER1)
               IPOSB = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
               IPOSB = IPOSB + NB0F + (JJ-NACT)
               IPB2 = ISPB(IPOSB)
               IPERB = (-1)**IPER1
               IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1860 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                        DO 1862 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1862                   CONTINUE
 1860                CONTINUE
                     DO 1865 IAT=1,IMZ11
                        IC4 = IPOSA01(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITB2))
                        I2 = MIN(IOB,IIND101(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITB2)
                        DO 1864 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1864                   CONTINUE
 1865                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1855 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1857 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1857                   CONTINUE
 1855                CONTINUE
                     DO 1858 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                        DO 1856 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1856                   CONTINUE
 1858                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1850 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1852 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1852                   CONTINUE
 1850                CONTINUE
                     DO 1853 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        IC4 = IPOSA01(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                        DO 1851 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1851                   CONTINUE
 1853                CONTINUE
C
                  ENDIF
C
 1880          CONTINUE
C
 1990       CONTINUE
Cc
 1994       CONTINUE
            CALL ADVANC(IBCON1,NB,NACT)
 1995    CONTINUE
C
C  Now loop over 1st order beta strings.
C
         DO 1840 II=1,NB-1
            IBCON1(II) = II
 1840    CONTINUE
         ICOUNT = NB0F
C
         DO 1920 KJI=1,NB1F
            IPOS1 = (KJI-1)*NEXT + NB0F
            DO 1910 KJJ=NACT+1,NOCC
               IBCON1(NB) = KJJ
               ICOUNT = ICOUNT + 1
C
               IPB1 = ISPB(ICOUNT)
               ISB1 = ISYMB(ICOUNT)
               ITBS = ITAB(ISB1)
               IMZZ0 = IMMC00(ITBS)
               IMZZ1 = IMMC01(ITBS)
               IMZZ = IMZZ0 + IMZZ1
               IF (IMZZ.EQ.0.AND.ISB1.NE.ITAS) GOTO 1910
               IC1 = ICAT + IPB1
C
C   Loop single excitations from 0th space.
C
         DO 1905 IB=1,NB-1
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C   Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 1790 IGEL=IB,NB-1
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-1) IEN=NACT
               DO 1780 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ10 = IMMC00(ITB2)
                  IMZ11 = IMMC01(ITB2)
                  IMZ1 = IMZ10 + IMZ11
C
                  IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 1785
                  IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1785
                  GOTO 1780
C
 1785             CONTINUE
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
                  IPOSB = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
                  IPOSB = IPOSB + NB0F + (KJJ-NACT)
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1760 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                        DO 1762 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1762                   CONTINUE
 1760                CONTINUE
                     DO 1764 IAT=1,IMZ11
                        IC4 = IPOSA01(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITB2))
                        I2 = MIN(IOB,IIND101(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITB2)
                        DO 1763 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1763                   CONTINUE
 1764                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1755 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1757 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1757                   CONTINUE
 1755                CONTINUE
                     DO 1758 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                        DO 1759 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1759                   CONTINUE
 1758                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1750 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1752 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1752                   CONTINUE
 1750                CONTINUE
                     DO 1753 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        IC4 = IPOSA01(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                        DO 1754 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1754                   CONTINUE
 1753                CONTINUE
C
                  ENDIF
C
 1780          CONTINUE
 1790       CONTINUE
C
C   Loop over orbitals in gaps in external space, ie A -> E.
C
            IGEL = NB-1
            DO 1690 JJ=NACT+1,NOCC
               IF (JJ.EQ.IBCON1(NB)) THEN
                  IGEL=NB
                  GOTO 1690
               ENDIF
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ10 = IMMC00(ITB2)
               IMZ11 = IMMC01(ITB2)
C
               IF (ISB1.EQ.ITAS.AND.IMZ10.NE.0) GOTO 1685
               IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1685
               GOTO 1690
C
 1685          CONTINUE
               CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
               IBCON2(NB-1) = IBCON2(NB-1)-NACT
               IBCON2(NB)   = IBCON2(NB)-NACT
C
               IPET1 = POSDET(NACT,NB-2,IBCON2,IFA)
               IPET2 = POSDET(NEXT,2,IBCON2(NB-1),IFE)
               IPOSB = (IPET1-1)*NB2E + IPET2 + NB2S
               IPB2 = ISPB(IPOSB)
               IPERB = (-1)**IPER1
               IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1660 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                        DO 1662 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1662                   CONTINUE
 1660                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ10.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1655 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1657 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1657                   CONTINUE
 1655                CONTINUE
                     DO 1658 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                        DO 1659 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1659                   CONTINUE
 1658                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1650 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1652 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1652                   CONTINUE
 1650                CONTINUE
                     DO 1653 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                        DO 1654 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1654                   CONTINUE
 1653                CONTINUE
C
                  ENDIF
C
 1690       CONTINUE
C
 1905    CONTINUE
C
C   Loop single excitations from External space.
C
         IBB = IBCON1(NB)
         IB1 = IOX(IBB)
         IR1 = IMUL(IB1,ISB1)
         IPERB = 1
C
C  Loop over orbitals in gaps in external space, ie E -> E.
C
         DO 1590 JJ=IBB+1,NOCC
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ10 = IMMC00(ITB2)
               IMZ11 = IMMC01(ITB2)
               IMZ1 = IMZ10 + IMZ11
C
               IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 1585
               IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1585
               GOTO 1590
C
 1585          CONTINUE
               IPOSB = IPOS1 + (JJ-NACT)
               IPB2 = ISPB(IPOSB)
               IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1560 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                        DO 1562 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1562                   CONTINUE
 1560                CONTINUE
                     DO 1563 IAT=1,IMZ11
                        IC4 = IPOSA01(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITB2))
                        I2 = MIN(IOB,IIND101(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITB2)
                        DO 1564 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1564                   CONTINUE
 1563                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1555 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1557 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1557                   CONTINUE
 1555                CONTINUE
                     DO 1554 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                        DO 1553 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1553                   CONTINUE
 1554                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1550 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1552 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1552                   CONTINUE
 1550                CONTINUE
                     DO 1558 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        IC4 = IPOSA01(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                        DO 1559 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1559                   CONTINUE
 1558                CONTINUE
C
                  ENDIF
C
 1590    CONTINUE
C
 1910       CONTINUE
            IF (NB.LE.1) GOTO 1920
            CALL ADVANC(IBCON1,NB-1,NACT)
 1920    CONTINUE
C
C  Now loop over 2nd order beta strings.
C
         IF (NB.LT.2) GOTO 1497
         DO 1495 II=1,NB-2
            IBCON1(II) = II
 1495    CONTINUE
         ICOUNT = NB2S
C
         DO 1490 KJI=1,NB2F
            IBCON1(NB-1) = 1 + NACT
            IBCON1(NB) = 2 + NACT
            DO 1485 KJJ=1,NB2E
               ICOUNT = ICOUNT + 1
C
               IPB1 = ISPB(ICOUNT)
               ISB1 = ISYMB(ICOUNT)
               ITBS = ITAB(ISB1)
               IMZZ0 = IMMC00(ITBS)
               IF (IMZZ0.EQ.0.AND.ISB1.NE.ITAS) GOTO 1487
               IC1 = ICAT + IPB1
C
C  Loop single excitations from 0th space.
C
         DO 1480 IB=1,NB-2
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 1475 IGEL=IB,NB-2
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-2) IEN = NACT
               DO 1470 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ10 = IMMC00(ITB2)
C
                  IF (ISB1.EQ.ITAS.AND.IMZ10.NE.0) GOTO 1465
                  IF (ISB2.EQ.ITAS.AND.IMZZ0.NE.0) GOTO 1465
                  GOTO 1470
C
 1465             CONTINUE
                  CALL REDE00(IBCON1,IBCON2,NB-2,IB,IGEL,JJ,IPER1)
C
                  IPET1 = POSDET(NACT,NB-2,IBCON2,IFA)
                  IPOSB = (IPET1-1)*NB2E + KJJ + NB2S
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ0.EQ.0) THEN
                     DO 1460 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                        DO 1462 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1462                   CONTINUE
 1460                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ10.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1455 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1457 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1457                   CONTINUE
 1455                CONTINUE
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1450 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1452 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1452                   CONTINUE
 1450                CONTINUE
                  ENDIF
C
 1470          CONTINUE
 1475       CONTINUE
C
 1480    CONTINUE
C
C  Loop single excitations from Excited space.
C
         DO 1400 IB=NB-1,NB
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C  Loop over orbitals in gaps in Excited space, E -> E.
C
            DO 1395 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN = NOCC
               DO 1390 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ10 = IMMC00(ITB2)
C
                  IF (ISB1.EQ.ITAS.AND.IMZ10.NE.0) GOTO 1385
                  IF (ISB2.EQ.ITAS.AND.IMZZ0.NE.0) GOTO 1385
                  GOTO 1390
C
 1385             CONTINUE
                  CALL REDE00(IBCON1(NB-1),IBCON2(NB-1),
     *            2,IB-NB+2,IGEL-NB+2,JJ,IPER1)
C
                  IBCON2(NB-1) = IBCON2(NB-1)-NACT
                  IBCON2(NB)   = IBCON2(NB)-NACT
                  IPET2 = POSDET(NEXT,2,IBCON2(NB-1),IFE)
                  IPOSB = (KJI-1)*NB2E + IPET2 + NB2S
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ0.EQ.0) THEN
                     DO 1360 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                        DO 1362 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1362                   CONTINUE
 1360                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ10.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1355 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1357 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1357                   CONTINUE
 1355                CONTINUE
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1350 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1352 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1352                   CONTINUE
 1350                CONTINUE
                  ENDIF
C
 1390          CONTINUE
 1395       CONTINUE
C
 1400   CONTINUE
C
 1487       CONTINUE
            CALL ADVANC(IBCON1(NB-1),2,NOCC)
 1485    CONTINUE
C
         IF (NB.LE.2) GOTO 1490
         CALL ADVANC(IBCON1,NB-2,NACT)
 1490    CONTINUE
 1497    CONTINUE
C
         CALL ADVANC(IACON1,NA,NACT)
 2000 CONTINUE
C
C    --------------------------------------------
C    End of loop over all 0th order alpha strings.
C    --------------------------------------------
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK RINSO1
C     -----------------------------------------------------------
      SUBROUTINE RINSO1(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,NV,IOX,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA11,IPERA11,IIND111,IMMC11,
     *    IPOSA12,IPERA12,IIND112,IMMC12)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION SI1(*),SI2(*),AB(NSOCI,NV),CI(NSOCI,NV)
      DIMENSION IFA(0:NACT,0:NACT),IFE(0:NEXT,0:2)
      DIMENSION IACON1(NA),IBCON1(NA),IACON2(NA),IBCON2(NA)
      DIMENSION INDEX((NOCC*(NOCC+1))/2 + 1)
      DIMENSION IOX(NOCC)
      DIMENSION ITAB(NSYM),IMUL(NSYM,NSYM)
      DIMENSION ISYMA(NATT),ISYMB(NATT),ISPA(NATT),ISPB(NATT)
      DIMENSION ISBS0(NSYM+1),ISBS1(NSYM),ISBS2(NSYM)
      DIMENSION ISBC(NATT)
      DIMENSION IPOSA11(((NA-1)*(NACT-NA+1)+(NEXT-1)),NSYM)
      DIMENSION IPERA11(((NA-1)*(NACT-NA+1)+(NEXT-1)),NSYM)
      DIMENSION IIND111(((NA-1)*(NACT-NA+1)+(NEXT-1)),NSYM)
      DIMENSION IMMC11(NSYM)
      DIMENSION IPOSA12((NA-1)*(NEXT-1),NSYM)
      DIMENSION IPERA12((NA-1)*(NEXT-1),NSYM)
      DIMENSION IIND112((NA-1)*(NEXT-1),NSYM)
      DIMENSION IMMC12(NSYM)
C
C    --------------------------------------------
C    Loop over all 1st order alpha strings.
C    --------------------------------------------
C
      DO 3995 II=1,NA-1
         IACON1(II)=II
 3995 CONTINUE
      ICOUNT = NA0F
C
      DO 3993 IJK=1,NA1F
         IPOS1 = (IJK-1)*NEXT + NA0F
         DO 3990 KJI=NACT+1,NOCC
            IACON1(NA) = KJI
            ICOUNT = ICOUNT + 1
            ICAT = ISPA(ICOUNT)
            ISA1 = ISYMA(ICOUNT)
            ITAS = ITAB(ISA1)
C
            DO 3005 II=1,NSYM
               IMMC11(II) = 0
               IMMC12(II) = 0
 3005       CONTINUE
C
C   Loop single excitations from 0th space.
C
         DO 3985 IA=1,NA-1
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 3980 IGEL=IA,NA-1
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA-1) IEN = NACT
               DO 3975 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
C
C  Storage here for later.
C
                  IPET = (POSDET(NACT,NA-1,IACON2,IFA)-1)*NEXT
                  IPET = IPET + NA0F + (KJI-NACT)
                  IMMC11(ISYMA(IPET)) = IMMC11(ISYMA(IPET)) + 1
                  IPOSA11(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
                  IPER11 = (-1)**IPER1
                  IPERA11(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = IPER11
                  IND = INDEX(JJ)+IO1
                  IIND111(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 3970
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 3905 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3905             CONTINUE
C
                  DO 3907 IK=IA+1,IGEL
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3907             CONTINUE
C
                  DO 3908 IK=IGEL+1,NA
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3908             CONTINUE
C
C  Loop over 0th and 1st beta strings now. 0th order first.
C
                  DO 3910 II=1,NB
                     IBCON1(II) = II
 3910             CONTINUE
C
                  NST = 1
                  DO 3915 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 3913 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 3913                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3918 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 3918                CONTINUE
C
                     T = (C+D)*IPER11
                     DO 3920 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3920                CONTINUE
C
                     NST = NEND
 3915             CONTINUE
C
C  Loop over 1st order beta strings.
C
                  DO 3922 II=1,NB-1
                     IBCON1(II) = II
 3922             CONTINUE
                  NSTF = 1
C
                  DO 3930 INB1 = ISBS1(ISA1),ISBS2(ISA1)-1
                     NIBP = ISBC(INB1) - NB0F
                     NX = (NIBP-1)/NB1E + 1
                     NY = NIBP - (NX-1)*NB1E
                     DO 3924 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-1,NACT)
 3924                CONTINUE
                     IBCON1(NB) = NY+NACT
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3926 IK=1,NB-1
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 3926                CONTINUE
C
                     ION = IBCON1(NB)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER11
                     DO 3928 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3928                CONTINUE
C
                     NSTF = NX
 3930             CONTINUE
C
 3970             CONTINUE
C
C   Loop double excitations from 0th space.
C
                  DO 3967 IAA=IA+1,NA-1
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
                     IF (JJ.GT.IIA) IPA=IPA-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA
C
                     DO 3965 IGEL2=IGEL,NA-1
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA-1) IENAA=NACT
                        DO 3963 JJAA=ISTAA,IENAA
C
                        IS4 = IOX(JJAA)
                        IP2 = IMUL(IS3,IS4)
                        IF (IP1.NE.IP2) GOTO 3963
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                        IPET = POSDET(NACT,NA-1,IBCON1,IFA)
                        IPET = (IPET-1)*NEXT + KJI-NACT + NA0F
                        ICI1 = ICAT
                        ICI2 = ISPA(IPET)
                        IPERT = IPER1 + IPER2
                        IPERT = (-1)**IPERT
C
C   Make matrix element.
C
                        II1 = INDEX(JJAA) + IO1
                        IF (IIA.GT.JJAA) THEN
                           I2 = INDEX(IIA) + JJAA
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIA) + JJ
                           INX2 = INDEX(II2) + II1
                        ELSEIF (IIA.LT.JJ) THEN
                           I2 = INDEX(JJAA) + IIA
                           INX = INDEX(I2) + IND
                           II2 = INDEX(JJ) + IIA
                           INX2 = INDEX(II1) + II2
                        ELSE
                           I2 = INDEX(JJAA) + IIA
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIA) + JJ
                           INX2 = INDEX(II1) + II2
                        ENDIF
                        C = SI2(INX) - SI2(INX2)
                        T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 3954 INB1 = ISBS0(ISA1),ISBS2(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                           DO 3952 KJ=1,NV
                         AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                         AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3952                      CONTINUE
 3954                   CONTINUE
C
 3963                   CONTINUE
 3965                CONTINUE
C
C  Loop over orbitals in gaps in external space, ie AA -> AE
C
                     IGEL2 = NA-1
                     DO 3959 JJAA=NACT+1,NOCC
                     IF (JJAA.EQ.IACON1(NA)) THEN
                        IGEL2=NA
                        GOTO 3959
                     ENDIF
C
                     IS4 = IOX(JJAA)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 3959
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
                     IBCON1(NA-1) = IBCON1(NA-1)-NACT
                     IBCON1(NA)   = IBCON1(NA)-NACT
C
                     IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                     IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                     IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                     ICI1 = ICAT
                     ICI2 = ISPA(IPET)
                     IPERT = IPER1 + IPER2
                     IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                     II1 = INDEX(JJAA) + IO1
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     IF (IIA.LT.JJ) THEN
                        II2 = INDEX(JJ) + IIA
                        INX2 = INDEX(II1) + II2
                     ELSE
                        II2 = INDEX(IIA) + JJ
                        INX2 = INDEX(II1) + II2
                     ENDIF
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 3944 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                        DO 3942 KJ=1,NV
                     AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                     AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3942                   CONTINUE
 3944                CONTINUE
C
 3959                CONTINUE
C
 3967             CONTINUE
C
C   Loop double excitations, 2nd excitation from external space.
C
               IAA = NA
               IIA = IACON1(IAA)
               IS3 = IOX(IIA)
               IPA = IAA
C
C Loop over orbitals in gaps in 0th space, ie AE -> AA
C
               DO 3955 IGEL2=IGEL,NA-1
                  ISTAA = IACON1(IGEL2)+1
                  IENAA = IACON1(IGEL2+1)-1
                  IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                  IF (IGEL2.EQ.NA-1) IENAA=NACT
                  DO 3957 JJAA=ISTAA,IENAA
C
                     IS4 = IOX(JJAA)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 3957
C
                  CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                     IPET = POSDET(NACT,NA,IBCON1,IFA)
                     ICI1 = ICAT
                     ICI2 = ISPA(IPET)
                     IPERT = IPER1 + IPER2
                     IPERT = (-1)**IPERT
C
                     II1 = INDEX(JJAA) + IO1
                     I2 = INDEX(IIA) + JJAA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIA) + JJ
                     INX2 = INDEX(II2) + II1
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 3934 INB1 = ISBS0(ISA1),ISBS2(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                        DO 3932 KJ=1,NV
                     AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                     AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3932                   CONTINUE
 3934                CONTINUE
C
 3957             CONTINUE
 3955          CONTINUE
C
C  Loop over orbitals in gaps in external space, ie AE -> AE
C
               IGEL2 = NA-1
               DO 3953 JJAA=NACT+1,NOCC
                  IF (JJAA.EQ.IIA) THEN
                     IGEL2 = NA
                     GOTO 3953
                  ENDIF
C
                  IS4 = IOX(JJAA)
                  IP2 = IMUL(IS3,IS4)
                  IF (IP1.NE.IP2) GOTO 3953
C
              CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                  IPET = POSDET(NACT,NA-1,IBCON1,IFA)
                  IPET = (IPET-1)*NEXT + JJAA-NACT + NA0F
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
                  IPERT = IPER1 + IPER2
                  IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                  II1 = INDEX(JJAA) + IO1
                  II2 = INDEX(IIA) + JJ
                  IF (IIA.GT.JJAA) THEN
                     I2 = INDEX(IIA) + JJAA
                     INX = INDEX(I2) + IND
                     INX2 = INDEX(II2) + II1
                  ELSE
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     INX2 = INDEX(II1) + II2
                  ENDIF
                  C = SI2(INX) - SI2(INX2)
                  T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 3951 INB1 = ISBS0(ISA1),ISBS2(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                        DO 3929 KJ=1,NV
                     AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                     AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3929                   CONTINUE
 3951                CONTINUE
C
 3953          CONTINUE
C
 3975          CONTINUE
 3980       CONTINUE
C
C  Loop over orbitals in gaps in external space, A -> E.
C
            IGEL = NA-1
            DO 3895 JJ=NACT+1,NOCC
               IF (JJ.EQ.IACON1(NA)) THEN
                  IGEL = NA
                  GOTO 3895
               ENDIF
               IS2 = IOX(JJ)
               IP1 = IMUL(IS2,IS1)
C
               CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
               IACON2(NA-1) = IACON2(NA-1)-NACT
               IACON2(NA) = IACON2(NA)-NACT
C
C  Storage here for later.
C
               IPET1 = POSDET(NACT,NA-2,IACON2,IFA)
               IPET2 = POSDET(NEXT,2,IACON2(NA-1),IFE)
               IPET = (IPET1-1)*NA2E + IPET2 + NA2S
               IMMC12(ISYMA(IPET)) = IMMC12(ISYMA(IPET)) + 1
               IPOSA12(IMMC12(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
               IPER11 = (-1)**IPER1
               IPERA12(IMMC12(ISYMA(IPET)),ISYMA(IPET)) = IPER11
               IND = INDEX(JJ)+IO1
               IIND112(IMMC12(ISYMA(IPET)),ISYMA(IPET)) = IND
C
               IACON2(NA-1) = IACON2(NA-1)+NACT
               IACON2(NA) = IACON2(NA)+NACT
C
C  Check if excited string is correct symmetry.
C
               IF (IS1.NE.IS2) GOTO 3870
               ICI1 = ICAT
               ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 3805 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3805             CONTINUE
C
                  DO 3807 IK=IA+1,IGEL
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3807             CONTINUE
C
                  DO 3808 IK=IGEL+1,NA
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3808             CONTINUE
C
C  Loop over all 0th beta dets now.
C
                  DO 3810 II=1,NB
                     IBCON1(II) = II
 3810             CONTINUE
C
                  NST = 1
                  DO 3815 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 3813 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 3813                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3818 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 3818                CONTINUE
C
                     T = (C+D)*IPER11
                     DO 3820 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3820                CONTINUE
C
                     NST = NEND
 3815             CONTINUE
C
 3870          CONTINUE
C
C   Loop double excitations, AE -> EE
C
               IAA = NA
               IIA = IACON1(IAA)
               IS3 = IOX(IIA)
               IPA = IAA
               IF (JJ.GT.IIA) IPA=IPA-1
C
C Loop over orbitals in gaps in external space > JJ, AE -> EE
C
               IGEL2 = IGEL
               DO 3865 JJAA=JJ+1,NOCC
                  IF (JJAA.EQ.IIA) THEN
                     IGEL2=NA
                     GOTO 3865
                  ENDIF
C
                  IS4 = IOX(JJAA)
                  IP2 = IMUL(IS3,IS4)
                  IF (IP1.NE.IP2) GOTO 3865
C
             CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
                  IBCON1(NA-1) = IBCON1(NA-1)-NACT
                  IBCON1(NA)   = IBCON1(NA)-NACT
C
                  IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                  IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                  IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
                  IPERT = IPER1 + IPER2
                  IPERT = (-1)**IPERT
C
C Make matrix element.
C
                  II1 = INDEX(JJAA) + IO1
                  IF (IIA.GT.JJAA) THEN
                     I2 = INDEX(IIA) + JJAA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIA) + JJ
                     INX2 = INDEX(II2) + II1
                  ELSEIF (IIA.LT.JJ) THEN
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(JJ) + IIA
                     INX2 = INDEX(II1) + II2
                  ELSE
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIA) + JJ
                     INX2 = INDEX(II1) + II2
                  ENDIF
                  C = SI2(INX) - SI2(INX2)
                  T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 3851 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                        DO 3853 KJ=1,NV
                     AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                     AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3853                   CONTINUE
 3851                CONTINUE
C
 3865          CONTINUE
C
 3895       CONTINUE
C
 3985    CONTINUE
C
C  Loop single excitation from external space.
C
         IA = NA
         IO1 = IACON1(NA)
         IS1 = IOX(IO1)
         IPER11 = 1
C
C  Loop over orbitals in external space, E -> E.
C
         DO 3795 JJ=IO1+1,NOCC
            IS2 = IOX(JJ)
            IP1 = IMUL(IS2,IS1)
C
            CALL REDE01(IACON1,IACON2,NA,IA,JJ,IPER1)
C
C  Storage here for later.
C
            IPET = IPOS1 + (JJ-NACT)
            IMMC11(ISYMA(IPET)) = IMMC11(ISYMA(IPET)) + 1
            IPOSA11(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
            IPERA11(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = IPER11
            IND = INDEX(JJ)+IO1
            IIND111(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
            IF (IS1.NE.IS2) GOTO 3770
            ICI1 = ICAT
            ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 3705 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3705             CONTINUE
C
C  Loop over all 0th beta dets now.
C
                  DO 3710 II=1,NB
                     IBCON1(II) = II
 3710             CONTINUE
C
                  NST = 1
                  DO 3715 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 3713 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 3713                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3718 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 3718                CONTINUE
C
                     T = (C+D)*IPER11
                     DO 3720 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3720                CONTINUE
C
                     NST = NEND
 3715             CONTINUE
C
C  Loop over 1st order beta strings.
C
                  DO 3722 II=1,NB-1
                     IBCON1(II) = II
 3722             CONTINUE
                  NSTF = 1
C
                  DO 3730 INB1 = ISBS1(ISA1),ISBS2(ISA1)-1
                     NIBP = ISBC(INB1) - NB0F
                     NX = (NIBP-1)/NB1E + 1
                     NY = NIBP - (NX-1)*NB1E
                     DO 3724 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-1,NACT)
 3724                CONTINUE
                     IBCON1(NB) = NY+NACT
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3726 IK=1,NB-1
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 3726                CONTINUE
C
                     ION = IBCON1(NB)
                     J1 = INDEX(ION+1)
                     IMA = MAX(IND,J1)
                     IMI = MIN(IND,J1)
                     JJ1 = INDEX(IMA) + IMI
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER11
                     DO 3728 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3728                CONTINUE
C
                     NSTF = NX
 3730             CONTINUE
C
 3770       CONTINUE
C
 3795    CONTINUE
C
C    Loop over all relevant betas and their relevant single excites now.
C
C   First loop over all 0th order betas
C
         DO 3010 II=1,NB
            IBCON1(II) = II
 3010    CONTINUE
C
         DO 3600 KJIH=1,NB0F
            IPB1 = ISPB(KJIH)
            ISB1 = ISYMB(KJIH)
            ITBS = ITAB(ISB1)
            IMZZ1 = IMMC11(ITBS)
            IMZZ2 = IMMC12(ITBS)
            IMZZ = IMZZ1 + IMZZ2
            IF (IMZZ.EQ.0.AND.ISB1.NE.ITAS) GOTO 3605
            IC1 = ICAT + IPB1
C
C   Loop single excitations from 0th space.
C
         DO 3610 IB=1,NB
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C  Loop over orbitals in gaps in 0th space, ie A -> A (B0 -> B0)
C
            DO 3615 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN=NACT
               DO 3620 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ11 = IMMC11(ITB2)
                  IMZ12 = IMMC12(ITB2)
                  IMZ1 = IMZ11 + IMZ12
C
                  IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 3625
                  IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 3625
                  GOTO 3620
C
 3625             CONTINUE
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
                  IPOSB = POSDET(NACT,NB,IBCON2,IFA)
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 3630 IAT=1,IMZ11
                        IC4 = IPOSA11(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND111(IAT,ITB2))
                        I2 = MIN(IOB,IIND111(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA11(IAT,ITB2)
                        DO 3635 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 3635                   CONTINUE
 3630                CONTINUE
                     DO 3640 IAT=1,IMZ12
                        IC4 = IPOSA12(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND112(IAT,ITB2))
                        I2 = MIN(IOB,IIND112(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA12(IAT,ITB2)
                        DO 3645 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 3645                   CONTINUE
 3640                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 3650 IAT=1,IMZZ1
                        IC3 = IPOSA11(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND111(IAT,ITBS))
                        I2 = MIN(IOB,IIND111(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA11(IAT,ITBS)
                        DO 3655 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 3655                   CONTINUE
 3650                CONTINUE
                     DO 3652 IAT=1,IMZZ2
                        IC3 = IPOSA12(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND112(IAT,ITBS))
                        I2 = MIN(IOB,IIND112(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA12(IAT,ITBS)
                        DO 3654 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 3654                   CONTINUE
 3652                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 3662 IAT=1,IMZZ1
                        IC3 = IPOSA11(IAT,ITBS) + IPB1
                        IC4 = IPOSA11(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND111(IAT,ITBS))
                        I2 = MIN(IOB,IIND111(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA11(IAT,ITBS)
                        DO 3664 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 3664                   CONTINUE
 3662                CONTINUE
                     DO 3666 IAT=1,IMZZ2
                        IC3 = IPOSA12(IAT,ITBS) + IPB1
                        IC4 = IPOSA12(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND112(IAT,ITBS))
                        I2 = MIN(IOB,IIND112(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA12(IAT,ITBS)
                        DO 3668 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 3668                   CONTINUE
 3666                CONTINUE
C
                  ENDIF
C
 3620          CONTINUE
C
 3615       CONTINUE
C
C  Loop over orbitals in external gap, ie  A -> E, (B0 -> B1)
C
            DO 3690 JJ=NACT+1,NOCC
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ11 = IMMC11(ITB2)
               IMZ12 = IMMC12(ITB2)
C
               IF (ISB1.EQ.ITAS.AND.IMZ11.NE.0) GOTO 3672
               IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 3672
               GOTO 3690
C
 3672          CONTINUE
               CALL REDE01(IBCON1,IBCON2,NB,IB,JJ,IPER1)
               IPOSB = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
               IPOSB = IPOSB + NB0F + (JJ-NACT)
               IPB2 = ISPB(IPOSB)
               IPERB = (-1)**IPER1
               IOB = INDEX(JJ)+IBB
C
               IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                  DO 3674 IAT=1,IMZ11
                     IC4 = IPOSA11(IAT,ITB2) + IPB2
                     I1 = MAX(IOB,IIND111(IAT,ITB2))
                     I2 = MIN(IOB,IIND111(IAT,ITB2))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*IPERB*IPERA11(IAT,ITB2)
                     DO 3676 KJ=1,NV
                        AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                        AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 3676                CONTINUE
 3674             CONTINUE
C
               ELSE IF(ISB1.NE.ITAS.OR.IMZ11.EQ.0) THEN
                  IC2 = ICAT + IPB2
                  DO 3680 IAT=1,IMZZ1
                     IC3 = IPOSA11(IAT,ITBS) + IPB1
                     I1 = MAX(IOB,IIND111(IAT,ITBS))
                     I2 = MIN(IOB,IIND111(IAT,ITBS))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*IPERB*IPERA11(IAT,ITBS)
                     DO 3682 KJ=1,NV
                        AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                        AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 3682                CONTINUE
 3680             CONTINUE
                  DO 3684 IAT=1,IMZZ2
                     IC3 = IPOSA12(IAT,ITBS) + IPB1
                     I1 = MAX(IOB,IIND112(IAT,ITBS))
                     I2 = MIN(IOB,IIND112(IAT,ITBS))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*IPERB*IPERA12(IAT,ITBS)
                     DO 3686 KJ=1,NV
                        AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                        AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 3686                CONTINUE
 3684             CONTINUE
C
               ELSE
                  IC2 = ICAT + IPB2
                  DO 3688 IAT=1,IMZZ1
                     IC3 = IPOSA11(IAT,ITBS) + IPB1
                     IC4 = IPOSA11(IAT,ITBS) + IPB2
                     I1 = MAX(IOB,IIND111(IAT,ITBS))
                     I2 = MIN(IOB,IIND111(IAT,ITBS))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*IPERB*IPERA11(IAT,ITBS)
                     DO 3692 KJ=1,NV
                        AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                        AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                        AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                        AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 3692                CONTINUE
 3688             CONTINUE
                  DO 3696 IAT=1,IMZZ2
                     IC3 = IPOSA12(IAT,ITBS) + IPB1
                     I1 = MAX(IOB,IIND112(IAT,ITBS))
                     I2 = MIN(IOB,IIND112(IAT,ITBS))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*IPERB*IPERA12(IAT,ITBS)
                     DO 3698 KJ=1,NV
                        AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                        AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 3698                CONTINUE
 3696             CONTINUE
C
               ENDIF
C
 3690       CONTINUE
C
 3610    CONTINUE
C
 3605       CONTINUE
C
            CALL ADVANC(IBCON1,NB,NACT)
 3600    CONTINUE
C
C  Now loop over 1st order beta strings.
C
         DO 3020 II=1,NB-1
            IBCON1(II) = II
 3020    CONTINUE
         ICOUNTB = NB0F
C
         DO 3500 KJIH=1,NB1F
            IPOSB1 = (KJIH-1)*NEXT + NB0F
            DO 3505 KJJ=NACT+1,NOCC
               IBCON1(NB) = KJJ
               ICOUNTB = ICOUNTB + 1
C
               IPB1 = ISPB(ICOUNTB)
               ISB1 = ISYMB(ICOUNTB)
               ITBS = ITAB(ISB1)
               IMZZ1 = IMMC11(ITBS)
               IF (IMZZ1.EQ.0.AND.ISB1.NE.ITAS) GOTO 3505
               IC1 = ICAT + IPB1
C
C   Loop single excitations from 0th space.
C
         DO 3510 IB=1,NB-1
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C  Loop over orbitals in gaps in 0th space, ie A -> A (B1 -> B1)
C
            DO 3515 IGEL=IB,NB-1
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-1) IEN=NACT
               DO 3520 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ11 = IMMC11(ITB2)
C
                  IF (ISB1.EQ.ITAS.AND.IMZ11.NE.0) GOTO 3523
                  IF (ISB2.EQ.ITAS.AND.IMZZ1.NE.0) GOTO 3523
                  GOTO 3520
C
 3523             CONTINUE
C
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
                  IPOSB = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
                  IPOSB = IPOSB + NB0F + (KJJ-NACT)
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ1.EQ.0) THEN
                     DO 3526 IAT=1,IMZ11
                        IC4 = IPOSA11(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND111(IAT,ITB2))
                        I2 = MIN(IOB,IIND111(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA11(IAT,ITB2)
                        DO 3529 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 3529                   CONTINUE
 3526                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ11.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 3530 IAT=1,IMZZ1
                        IC3 = IPOSA11(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND111(IAT,ITBS))
                        I2 = MIN(IOB,IIND111(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA11(IAT,ITBS)
                        DO 3533 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 3533                   CONTINUE
 3530                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 3532 IAT=1,IMZZ1
                        IC3 = IPOSA11(IAT,ITBS) + IPB1
                        IC4 = IPOSA11(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND111(IAT,ITBS))
                        I2 = MIN(IOB,IIND111(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA11(IAT,ITBS)
                        DO 3534 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 3534                   CONTINUE
 3532                CONTINUE
C
                  ENDIF
C
 3520          CONTINUE
 3515       CONTINUE
C
 3510    CONTINUE
C
C   Loop single excitations from External space.
C
         IBB = IBCON1(NB)
         IB1 = IOX(IBB)
         IR1 = IMUL(IB1,ISB1)
         IPERB = 1
C
C  Loop over orbitals in gaps in external space, ie E -> E, (B1 -> B1)
C
         DO 3550 JJ=IBB+1,NOCC
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ11 = IMMC11(ITB2)
C
               IF (ISB1.EQ.ITAS.AND.IMZ11.NE.0) GOTO 3555
               IF (ISB2.EQ.ITAS.AND.IMZZ1.NE.0) GOTO 3555
               GOTO 3550
C
 3555          CONTINUE
C
               IPOSB = IPOSB1 + (JJ-NACT)
               IPB2 = ISPB(IPOSB)
               IOB = INDEX(JJ)+IBB
C
               IF (ISB2.NE.ITAS.OR.IMZZ1.EQ.0) THEN
                  DO 3560 IAT=1,IMZ11
                     IC4 = IPOSA11(IAT,ITB2) + IPB2
                     I1 = MAX(IOB,IIND111(IAT,ITB2))
                     I2 = MIN(IOB,IIND111(IAT,ITB2))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*IPERB*IPERA11(IAT,ITB2)
                     DO 3562 KJ=1,NV
                        AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                        AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 3562                CONTINUE
 3560             CONTINUE
C
               ELSE IF(ISB1.NE.ITAS.OR.IMZ11.EQ.0) THEN
                  IC2 = ICAT + IPB2
                  DO 3565 IAT=1,IMZZ1
                     IC3 = IPOSA11(IAT,ITBS) + IPB1
                     I1 = MAX(IOB,IIND111(IAT,ITBS))
                     I2 = MIN(IOB,IIND111(IAT,ITBS))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*IPERB*IPERA11(IAT,ITBS)
                     DO 3570 KJ=1,NV
                        AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                        AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 3570                CONTINUE
 3565             CONTINUE
C
               ELSE
                  IC2 = ICAT + IPB2
                  DO 3573 IAT=1,IMZZ1
                     IC3 = IPOSA11(IAT,ITBS) + IPB1
                     IC4 = IPOSA11(IAT,ITBS) + IPB2
                     I1 = MAX(IOB,IIND111(IAT,ITBS))
                     I2 = MIN(IOB,IIND111(IAT,ITBS))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*IPERB*IPERA11(IAT,ITBS)
                     DO 3576 KJ=1,NV
                        AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                        AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                        AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                        AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 3576                CONTINUE
 3573             CONTINUE
C
               ENDIF
C
 3550    CONTINUE
C
 3505       CONTINUE
            IF (NB.LE.1) GOTO 3500
            CALL ADVANC(IBCON1,NB-1,NACT)
 3500    CONTINUE
C
 3990    CONTINUE
         CALL ADVANC(IACON1,NA-1,NACT)
 3993 CONTINUE
C
C    --------------------------------------------
C    End of loop over all 1st order alpha strings.
C    --------------------------------------------
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK RINSO2
C     -----------------------------------------------------------
      SUBROUTINE RINSO2(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,NV,IOX,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBC,NSYM,
     *    IPOSA22,IPERA22,IIND122,IMMC22)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION SI1(*),SI2(*),AB(NSOCI,NV),CI(NSOCI,NV)
      DIMENSION IFA(0:NACT,0:NACT),IFE(0:NEXT,0:2)
      DIMENSION IACON1(NA),IBCON1(NA),IACON2(NA),IBCON2(NA)
      DIMENSION INDEX((NOCC*(NOCC+1))/2 + 1)
      DIMENSION IOX(NOCC)
      DIMENSION ITAB(NSYM),IMUL(NSYM,NSYM)
      DIMENSION ISYMA(NATT),ISYMB(NATT),ISPA(NATT),ISPB(NATT)
      DIMENSION ISBS0(NSYM+1),ISBS1(NSYM)
      DIMENSION ISBC(NATT)
      DIMENSION IPOSA22(((NA-2)*(NACT-NA+2)+2*(NEXT-2)),NSYM)
      DIMENSION IPERA22(((NA-2)*(NACT-NA+2)+2*(NEXT-2)),NSYM)
      DIMENSION IIND122(((NA-2)*(NACT-NA+2)+2*(NEXT-2)),NSYM)
      DIMENSION IMMC22(NSYM)
C
C    --------------------------------------------
C    Loop over all 2nd order alpha strings.
C    --------------------------------------------
C
      DO 4995 II=1,NA-2
         IACON1(II) = II
 4995 CONTINUE
      ICOUNT = NA2S
C
      DO 4993 IJK = 1,NA2F
         IACON1(NA-1) = 1 + NACT
         IACON1(NA) = 2 + NACT
         DO 4990 KJI=1,NA2E
            ICOUNT = ICOUNT + 1
            ICAT = ISPA(ICOUNT)
            ISA1 = ISYMA(ICOUNT)
            ITAS = ITAB(ISA1)
C
            DO 4005 II=1,NSYM
               IMMC22(II) = 0
 4005       CONTINUE
C
C   Loop single excitations from 0th space.
C
         DO 4985 IA=1,NA-2
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in zeroth order space, A -> A.
C
            DO 4980 IGEL=IA,NA-2
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA-2) IEN = NACT
               DO 4975 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
C
C  Storage here for later.
C
                  IPET1 = POSDET(NACT,NA-2,IACON2,IFA)
                  IPET2 = KJI
                  IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                  IMMC22(ISYMA(IPET)) = IMMC22(ISYMA(IPET)) + 1
                  IPOSA22(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
                  IPER11 = (-1)**IPER1
                  IPERA22(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = IPER11
                  IND = INDEX(JJ)+IO1
                  IIND122(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 4970
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 4905 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4905             CONTINUE
C
                  DO 4907 IK=IA+1,IGEL
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4907             CONTINUE
C
                  DO 4908 IK=IGEL+1,NA
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4908             CONTINUE
C
C  Loop over all 0th beta dets now.
C
                  DO 4910 II=1,NB
                     IBCON1(II) = II
 4910             CONTINUE
C
                  NST = 1
                  DO 4915 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 4913 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 4913                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 4918 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 4918                CONTINUE
C
                     T = (C+D)*IPER11
                     DO 4920 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 4920                CONTINUE
C
                     NST = NEND
 4915             CONTINUE
C
 4970             CONTINUE
C
C  Loop double excitations from 0th space.
C
                  DO 4967 IAA=IA+1,NA-2
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
                     IF (JJ.GT.IIA) IPA=IPA-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA.
C
                     DO 4965 IGEL2=IGEL,NA-2
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA-2) IENAA=NACT
                        DO 4963 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 4963
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                           IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                           IPET2 = KJI
                           IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = IPER1 + IPER2
                           IPERT = (-1)**IPERT
C
C   Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           IF (IIA.GT.JJAA) THEN
                              I2 = INDEX(IIA) + JJAA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIA.LT.JJ) THEN
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIA
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 4966 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                           DO 4961 KJ=1,NV
                      AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                      AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 4961                      CONTINUE
 4966                   CONTINUE
C
 4963                   CONTINUE
 4965                CONTINUE
C
 4967             CONTINUE
C
C  Loop double excitations, 2nd excitation from external space.
C
                  DO 4957 IAA=NA-1,NA
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
C
C  Loop over orbitals in gaps in 0th space, ie AE -> AA
C
                     DO 4955 IGEL2=IGEL,NA-2
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA-2) IENAA=NACT
                        DO 4953 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 4953
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                           IORB = IBCON1(NA)-NACT
                           IPET = POSDET(NACT,NA-1,IBCON1,IFA)
                           IPET = (IPET-1)*NEXT + IORB + NA0F
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = IPER1 + IPER2
                           IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           I2 = INDEX(IIA) + JJAA
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIA) + JJ
                           INX2 = INDEX(II2) + II1
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 4956 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                           DO 4931 KJ=1,NV
                      AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                      AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 4931                      CONTINUE
 4956                   CONTINUE
C
 4953                   CONTINUE
 4955                CONTINUE
C
C  Loop over orbitals in gaps in excited space, ie AE -> AE.
C
                     DO 4951 IGEL2=NA-2,NA
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.NA-2) ISTAA=NACT+1
                        IF (IGEL2.EQ.NA) IENAA=NOCC
                        DO 4949 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 4949
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
                           IBCON1(NA-1) = IBCON1(NA-1)-NACT
                           IBCON1(NA)   = IBCON1(NA)-NACT
C
                           IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                           IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                           IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = IPER1 + IPER2
                           IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           IF (IIA.GT.JJAA) THEN
                              I2 = INDEX(IIA) + JJAA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSE
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 4946 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                           DO 4941 KJ=1,NV
                      AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                      AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 4941                      CONTINUE
 4946                   CONTINUE
C
 4949                   CONTINUE
 4951                CONTINUE
C
 4957             CONTINUE
C
 4975          CONTINUE
 4980       CONTINUE
C
 4985    CONTINUE
C
C  Loop single excitations from excited space.
C
         DO 4785 IA=NA-1,NA
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in excited space, E -> E.
C
            DO 4780 IGEL=IA,NA
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA) IEN=NOCC
               DO 4775 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
                  IACON2(NA-1) = IACON2(NA-1)-NACT
                  IACON2(NA) = IACON2(NA)-NACT
C
C  Storage here for later.
C
                  IPET1 = IJK
                  IPET2 = POSDET(NEXT,2,IACON2(NA-1),IFE)
                  IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                  IMMC22(ISYMA(IPET)) = IMMC22(ISYMA(IPET)) + 1
                  IPOSA22(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
                  IPER11 = (-1)**IPER1
                  IPERA22(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = IPER11
                  IND = INDEX(JJ)+IO1
                  IIND122(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = IND
C
                  IACON2(NA-1) = IACON2(NA-1)+NACT
                  IACON2(NA) = IACON2(NA)+NACT
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 4770
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 4705 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4705             CONTINUE
C
                  DO 4707 IK=IA+1,IGEL
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4707             CONTINUE
C
                  DO 4708 IK=IGEL+1,NA
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4708             CONTINUE
C
C  Loop over all 0th beta dets now.
C
                  DO 4710 II=1,NB
                     IBCON1(II) = II
 4710             CONTINUE
C
                  NST = 1
                  DO 4715 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 4713 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 4713                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 4718 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 4718                CONTINUE
C
                     T = (C+D)*IPER11
                     DO 4720 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 4720                CONTINUE
C
                     NST = NEND
 4715             CONTINUE
C
 4770             CONTINUE
C
C   Loop double excitations from excited space, ie EE -> EE.
C
                  DO 4767 IAA=IA+1,NA
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
                     IF (JJ.GT.IIA) IPA=IPA-1
C
                     DO 4765 IGEL2=IGEL,NA
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA) IENAA=NOCC
                        DO 4763 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 4763
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
                           IBCON1(NA-1) = IBCON1(NA-1)-NACT
                           IBCON1(NA)   = IBCON1(NA)-NACT
C
                           IPET1 = IJK
                           IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                           IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = IPER1 + IPER2
                           IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           IF (IIA.GT.JJAA) THEN
                              I2 = INDEX(IIA) + JJAA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIA.LT.JJ) THEN
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIA
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 4746 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                           DO 4741 KJ=1,NV
                      AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                      AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 4741                      CONTINUE
 4746                   CONTINUE
C
 4763                   CONTINUE
 4765                CONTINUE
C
 4767             CONTINUE
C
 4775          CONTINUE
 4780       CONTINUE
C
 4785    CONTINUE
C
C    Loop over all relevant betas and their relevant single excites now.
C
C   Loop over all 0th order betas
C
         DO 5010 II=1,NB
            IBCON1(II) = II
 5010    CONTINUE
C
         DO 5600 KJIH=1,NB0F
            IPB1 = ISPB(KJIH)
            ISB1 = ISYMB(KJIH)
            ITBS = ITAB(ISB1)
            IMZZ = IMMC22(ITBS)
            IF (IMZZ.EQ.0.AND.ISB1.NE.ITAS) GOTO 5605
            IC1 = ICAT + IPB1
C
C   Loop single excitations from 0th space.
C
         DO 5610 IB=1,NB
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C  Loop over orbitals in gaps in 0th space, ie A -> A (B0 -> B0)
C
            DO 5615 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN=NACT
               DO 5620 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ2 = IMMC22(ITB2)
C
                  IF (ISB1.EQ.ITAS.AND.IMZ2.NE.0) GOTO 5625
                  IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 5625
                  GOTO 5620
C
 5625             CONTINUE
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
                  IPOSB = POSDET(NACT,NB,IBCON2,IFA)
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 5630 IAT=1,IMZ2
                        IC4 = IPOSA22(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND122(IAT,ITB2))
                        I2 = MIN(IOB,IIND122(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA22(IAT,ITB2)
                        DO 5635 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 5635                   CONTINUE
 5630                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ2.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 5650 IAT=1,IMZZ
                        IC3 = IPOSA22(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND122(IAT,ITBS))
                        I2 = MIN(IOB,IIND122(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA22(IAT,ITBS)
                        DO 5655 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 5655                   CONTINUE
 5650                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 5662 IAT=1,IMZZ
                        IC3 = IPOSA22(IAT,ITBS) + IPB1
                        IC4 = IPOSA22(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND122(IAT,ITBS))
                        I2 = MIN(IOB,IIND122(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA22(IAT,ITBS)
                        DO 5664 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 5664                   CONTINUE
 5662                CONTINUE
C
                  ENDIF
C
 5620          CONTINUE
 5615       CONTINUE
C
 5610    CONTINUE
C
 5605       CONTINUE
C
            CALL ADVANC(IBCON1,NB,NACT)
 5600    CONTINUE
C
            CALL ADVANC(IACON1(NA-1),2,NOCC)
 4990    CONTINUE
         CALL ADVANC(IACON1,NA-2,NACT)
 4993 CONTINUE
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK RINSOB
C     -----------------------------------------------------------
      SUBROUTINE RINSOB(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IBCON2,IFA,IFE,INDEX,AB,NV,Q,IOX,
     *    IMUL,ISYMB,ISPA,ISPB,ISAS0,ISAS1,ISAS2,ISAC,NSYM)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION SI1(*),SI2(*),AB(NSOCI,NV),CI(NSOCI,NV)
      DIMENSION IFA(0:NACT,0:NACT),IFE(0:NEXT,0:2)
      DIMENSION IACON1(NA),IBCON1(NA),IBCON2(NA)
      DIMENSION INDEX((NOCC*(NOCC+1))/2 + 1)
      DIMENSION Q(NSOCI)
      DIMENSION IOX(NOCC)
      DIMENSION IMUL(NSYM,NSYM)
      DIMENSION ISYMB(NATT),ISPA(NATT),ISPB(NATT)
      DIMENSION ISAS0(NSYM+1),ISAS1(NSYM),ISAS2(NSYM)
      DIMENSION ISAC(NATT)
C
C    --------------------------------------------
C    Loop over all 0th order beta strings.
C    --------------------------------------------
C
      DO 6020 II=1,NB
         IBCON1(II)=II
 6020 CONTINUE
C
      DO 6000 IJK=1,NB0F
         ICAB = ISPB(IJK)
         ISB1 = ISYMB(IJK)
C
C   Loop single excitations from 0th space.
C
         DO 6990 IB=1,NB
            IO1 = IBCON1(IB)
            IS1 = IOX(IO1)
C
C   Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 6980 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN=NACT
               DO 6970 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
C
                  IPER = ((-1)**IPER1)
                  IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 6900
                  IPET = POSDET(NACT,NB,IBCON2,IFA)
                  IPB1 = ISPB(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 6905 IK=1,IB-1
                     ION = IBCON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 6905             CONTINUE
C
                  DO 6907 IK=IB+1,IGEL
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 6907             CONTINUE
C
                  DO 6908 IK=IGEL+1,NB
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 6908             CONTINUE
C
C  Loop over compatible alpha strings. 0th order first.
C
                  DO 6910 II=1,NA
                     IACON1(II) = II
 6910             CONTINUE
C
                  NST = 1
                  DO 6915 INA1=ISAS0(ISB1),ISAS1(ISB1)-1
                     NEND = ISAC(INA1)
                     DO 6913 KK=NST,NEND-1
                        CALL ADVANC(IACON1,NA,NACT)
 6913                CONTINUE
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 6918 IK=1,NA
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 6918                CONTINUE
C
                     T = (C+D)*IPER
                     DO 6920 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 6920                CONTINUE
C
                     NST = NEND
 6915             CONTINUE
C
C  Loop over 1st order alpha strings.
C
                  DO 6922 II=1,NA-1
                     IACON1(II) = II
 6922             CONTINUE
                  NSTF = 1
C
                  DO 6930 INA1 = ISAS1(ISB1),ISAS2(ISB1)-1
                     NIAP = ISAC(INA1) - NA0F
                     NX = (NIAP-1)/NA1E + 1
                     NY = NIAP - (NX-1)*NA1E
                     DO 6924 KK=NSTF,NX-1
                        CALL ADVANC(IACON1,NA-1,NACT)
 6924                CONTINUE
                     IACON1(NA) = NY+NACT
C
                     ICIA = ISPA(NIAP+NA0F)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 6926 IK=1,NA-1
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 6926                CONTINUE
C
                     ION = IACON1(NA)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER
                     DO 6928 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 6928                CONTINUE
C
                     NSTF = NX
 6930             CONTINUE
C
C  Loop over 2nd order alpha strings.
C
                  DO 6932 II=1,NA-2
                     IACON1(II) = II
 6932             CONTINUE
                  IACON1(NA-1) = 1 + NACT
                  IACON1(NA) = 2 + NACT
                  NSTF = 1
                  NSTE = 1
C
                  DO 6940 INA1 = ISAS2(ISB1),ISAS0(ISB1+1)-1
                     NIAP = ISAC(INA1) - NA2S
                     NX = (NIAP-1)/NA2E + 1
                     NY = NIAP - (NX-1)*NA2E
                     DO 6934 KK=NSTF,NX-1
                        CALL ADVANC(IACON1,NA-2,NACT)
 6934                CONTINUE
                     IF (NY.LT.NSTE) THEN
                        IACON1(NA-1) = 1 + NACT
                        IACON1(NA)   = 2 + NACT
                        NSTE = 1
                     ENDIF
                     DO 6936 KK=NSTE,NY-1
                        CALL ADVANC(IACON1(NA-1),2,NOCC)
 6936                CONTINUE
C
                     ICIA = ISPA(NIAP+NA2S)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 6937 IK=1,NA-2
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 6937                CONTINUE
C
                     DO 6938 IK=NA-1,NA
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(J1) + IND
                        D = D + SI2(JJ1)
 6938                CONTINUE
C
                     T = (C+D)*IPER
                     DO 6939 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 6939                CONTINUE
C
                     NSTF = NX
                     NSTE = NY
C
 6940             CONTINUE
C
 6900             CONTINUE
C
C   Loop double excitations from 0th space.
C
                  DO 6967 IBB=IB+1,NB
                     IIB = IBCON1(IBB)
                     IS3 = IOX(IIB)
                     IPB = IBB
                     IF (JJ.GT.IIB) IPB=IPB-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA
C
                     DO 6965 IGEL2=IGEL,NB
                        ISTBB = IBCON1(IGEL2)+1
                        IENBB = IBCON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTBB=JJ+1
                        IF (IGEL2.EQ.NB) IENBB=NACT
                        DO 6963 JJBB=ISTBB,IENBB
C
                           IS4 = IOX(JJBB)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 6963
C
                     CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
C
                           IPET = POSDET(NACT,NB,IACON1,IFA)
                           IBP2 = ISPB(IPET)
                           IPER = IPER1 + IPER2
                           IPER = (-1)**IPER
C
C   Make matrix element.
C
                           II1 = INDEX(JJBB) + IO1
                           IF (IIB.GT.JJBB) THEN
                              I2 = INDEX(IIB) + JJBB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIB) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIB.LT.JJ) THEN
                              I2 = INDEX(JJBB) + IIB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIB
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJBB) + IIB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIB) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                           DO 6961 INA1 = ISAS0(ISB1),ISAS0(ISB1+1)-1
                              NEND = ISAC(INA1)
                              ICIA = ISPA(NEND)
                              ICI1 = ICIA + ICAB
                              ICI2 = ICIA + IBP2
                              DO 6960 KJ=1,NV
                            AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                            AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 6960                         CONTINUE
 6961                      CONTINUE
C
 6963                   CONTINUE
 6965                CONTINUE
C
C  Loop over orbitals in external space, ie AA -> AE
C
                     DO 6950 JJBB=NACT+1,NOCC
C
                        IS4 = IOX(JJBB)
                        IP2 = IMUL(IS3,IS4)
                        IF (IP1.NE.IP2) GOTO 6950
C
                        CALL REDE01(IBCON2,IACON1,NB,IPB,JJBB,IPER2)
C
                        IPET = POSDET(NACT,NB-1,IACON1,IFA)
                        IPET = (IPET-1)*NEXT + JJBB-NACT + NB0F
                        IBP2 = ISPB(IPET)
                        IPER = IPER1 + IPER2
                        IPER = (-1)**IPER
C
C  Make matrix element.
C
                        II1 = INDEX(JJBB) + IO1
                        I2 = INDEX(JJBB) + IIB
                        INX = INDEX(I2) + IND
                        IF (IIB.LT.JJ) THEN
                           II2 = INDEX(JJ) + IIB
                           INX2 = INDEX(II1) + II2
                        ELSE
                           II2 = INDEX(IIB) + JJ
                           INX2 = INDEX(II1) + II2
                        ENDIF
                        C = SI2(INX) - SI2(INX2)
                        T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                        DO 6954 INA1 = ISAS0(ISB1),ISAS2(ISB1)-1
                           NEND = ISAC(INA1)
                           ICIA = ISPA(NEND)
                           ICI1 = ICIA + ICAB
                           ICI2 = ICIA + IBP2
                           DO 6952 KJ=1,NV
                         AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                         AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 6952                      CONTINUE
 6954                   CONTINUE
C
 6950                CONTINUE
C
 6967             CONTINUE
C
 6970          CONTINUE
 6980       CONTINUE
C
C  Loop over orbitals in external gap, ie  A -> E.
C
            DO 6880 JJ=NACT+1,NOCC
C
               IS2 = IOX(JJ)
               IP1 = IMUL(IS2,IS1)
C
               CALL REDE01(IBCON1,IBCON2,NB,IB,JJ,IPER1)
C
               IPER = ((-1)**IPER1)
               IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry.
C
               IF (IS1.NE.IS2) GOTO 6800
               IPET = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
               IPET = IPET + NB0F + (JJ-NACT)
               IPB1 = ISPB(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 6805 IK=1,IB-1
                     ION = IBCON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 6805             CONTINUE
C
                  DO 6807 IK=IB+1,NB
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 6807             CONTINUE
C
C  Loop over all compatible alpha strings now. 0th order first.
C
                  DO 6810 II=1,NA
                     IACON1(II) = II
 6810             CONTINUE
C
                  NST = 1
                  DO 6815 INA1=ISAS0(ISB1),ISAS1(ISB1)-1
                     NEND = ISAC(INA1)
                     DO 6813 KK=NST,NEND-1
                        CALL ADVANC(IACON1,NA,NACT)
 6813                CONTINUE
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 6818 IK=1,NA
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 6818                CONTINUE
C
                     T = (C+D)*IPER
                     DO 6820 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 6820                CONTINUE
C
                     NST = NEND
 6815             CONTINUE
C
C  Loop over 1st order alpha strings.
C
                  DO 6822 II=1,NA-1
                     IACON1(II) = II
 6822             CONTINUE
                  NSTF = 1
C
                  DO 6830 INA1 = ISAS1(ISB1),ISAS2(ISB1)-1
                     NIAP = ISAC(INA1) - NA0F
                     NX = (NIAP-1)/NA1E + 1
                     NY = NIAP - (NX-1)*NA1E
                     DO 6824 KK=NSTF,NX-1
                        CALL ADVANC(IACON1,NA-1,NACT)
 6824                CONTINUE
                     IACON1(NA) = NY+NACT
C
                     ICIA = ISPA(NIAP+NA0F)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 6826 IK=1,NA-1
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 6826                CONTINUE
C
                     ION = IACON1(NA)
                     J1 = INDEX(ION+1)
                     IMA = MAX(IND,J1)
                     IMI = MIN(IND,J1)
                     JJ1 = INDEX(IMA) + IMI
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER
                     DO 6828 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 6828                CONTINUE
C
                     NSTF = NX
 6830             CONTINUE
C
 6800          CONTINUE
C
C   Loop double excitations from 0th space.
C
               DO 6867 IBB=IB+1,NB
                  IIB = IBCON1(IBB)
                  IS3 = IOX(IIB)
                  IPB = IBB - 1
C
C  Loop over external orbitals > JJ, ie AA -> EE
C
                  DO 6865 JJBB=JJ+1,NOCC
C
                     IS4 = IOX(JJBB)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 6865
C
                     CALL REDE01(IBCON2,IACON1,NB,IPB,JJBB,IPER2)
                     IACON1(NB-1) = JJ-NACT
                     IACON1(NB)   = JJBB-NACT
C
                     IPET1 = POSDET(NACT,NB-2,IACON1,IFA)
                     IPET2 = POSDET(NEXT,2,IACON1(NB-1),IFE)
                     IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                     IBP2 = ISPB(IPET)
                     IPER = IPER1 + IPER2
                     IPER = (-1)**IPER
C
C  Make matrix element.
C
                     II1 = INDEX(JJBB) + IO1
                     I2 = INDEX(JJBB) + IIB
                     INX = INDEX(I2) + IND
                     II2 = INDEX(JJ) + IIB
                     INX2 = INDEX(II1) + II2
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPER
C
C
C  Loop over alpha strings of the right symmetry.
C
                     DO 6863 INA1 = ISAS0(ISB1),ISAS1(ISB1)-1
                        NEND = ISAC(INA1)
                        ICIA = ISPA(NEND)
                        ICI1 = ICIA + ICAB
                        ICI2 = ICIA + IBP2
                        DO 6861 KJ=1,NV
                      AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                      AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 6861                   CONTINUE
 6863                CONTINUE
C
 6865             CONTINUE
C
 6867          CONTINUE
C
 6880       CONTINUE
C
 6990    CONTINUE
C
         CALL ADVANC(IBCON1,NB,NACT)
 6000 CONTINUE
C
C    --------------------------------------------
C    Loop over all 1st order beta strings.
C    --------------------------------------------
C
      DO 7995 II=1,NB-1
         IBCON1(II)=II
 7995 CONTINUE
      ICOUNT = NB0F
C
      DO 7993 IJK=1,NB1F
         IPOS1 = (IJK-1)*NEXT + NB0F
         DO 7990 KJI=NACT+1,NOCC
            IBCON1(NB) = KJI
            ICOUNT = ICOUNT + 1
            ICAB = ISPB(ICOUNT)
            ISB1 = ISYMB(ICOUNT)
C
C   Loop single excitations from 0th space.
C
         DO 7985 IB=1,NB-1
            IO1 = IBCON1(IB)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 7980 IGEL=IB,NB-1
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-1) IEN = NACT
               DO 7975 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
C
                  IPER = ((-1)**IPER1)
                  IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 7970
                  IPET = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
                  IPET = IPET + NB0F + (KJI-NACT)
                  IPB1 = ISPB(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 7905 IK=1,IB-1
                     ION = IBCON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 7905             CONTINUE
C
                  DO 7907 IK=IB+1,IGEL
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 7907             CONTINUE
C
                  DO 7908 IK=IGEL+1,NB
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 7908             CONTINUE
C
C  Loop over compatible alpha strings. 0th order first.
C
                  DO 7910 II=1,NA
                     IACON1(II) = II
 7910             CONTINUE
C
                  NST = 1
                  DO 7915 INA1=ISAS0(ISB1),ISAS1(ISB1)-1
                     NEND = ISAC(INA1)
                     DO 7913 KK=NST,NEND-1
                        CALL ADVANC(IACON1,NA,NACT)
 7913                CONTINUE
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 7918 IK=1,NA
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 7918                CONTINUE
C
                     T = (C+D)*IPER
                     DO 7920 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 7920                CONTINUE
C
                     NST = NEND
 7915             CONTINUE
C
C  Loop over 1st order alpha strings.
C
                  DO 7922 II=1,NA-1
                     IACON1(II) = II
 7922             CONTINUE
                  NSTF = 1
C
                  DO 7930 INA1 = ISAS1(ISB1),ISAS2(ISB1)-1
                     NIAP = ISAC(INA1) - NA0F
                     NX = (NIAP-1)/NA1E + 1
                     NY = NIAP - (NX-1)*NA1E
                     DO 7924 KK=NSTF,NX-1
                        CALL ADVANC(IACON1,NA-1,NACT)
 7924                CONTINUE
                     IACON1(NA) = NY+NACT
C
                     ICIA = ISPA(NIAP+NA0F)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 7926 IK=1,NA-1
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 7926                CONTINUE
C
                     ION = IACON1(NA)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER
                     DO 7928 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 7928                CONTINUE
C
                     NSTF = NX
 7930             CONTINUE
C
 7970             CONTINUE
C
C   Loop double excitations from 0th space.
C
                  DO 7967 IBB=IB+1,NB-1
                     IIB = IBCON1(IBB)
                     IS3 = IOX(IIB)
                     IPB = IBB
                     IF (JJ.GT.IIB) IPB=IPB-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA
C
                     DO 7965 IGEL2=IGEL,NB-1
                        ISTBB = IBCON1(IGEL2)+1
                        IENBB = IBCON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTBB=JJ+1
                        IF (IGEL2.EQ.NB-1) IENBB=NACT
                        DO 7963 JJBB=ISTBB,IENBB
C
                        IS4 = IOX(JJBB)
                        IP2 = IMUL(IS3,IS4)
                        IF (IP1.NE.IP2) GOTO 7963
C
                     CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
C
                        IPET = POSDET(NACT,NB-1,IACON1,IFA)
                        IPET = (IPET-1)*NEXT + KJI-NACT + NB0F
                        IBP2 = ISPB(IPET)
                        IPER = IPER1 + IPER2
                        IPER = (-1)**IPER
C
C   Make matrix element.
C
                        II1 = INDEX(JJBB) + IO1
                        IF (IIB.GT.JJBB) THEN
                           I2 = INDEX(IIB) + JJBB
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIB) + JJ
                           INX2 = INDEX(II2) + II1
                        ELSEIF (IIB.LT.JJ) THEN
                           I2 = INDEX(JJBB) + IIB
                           INX = INDEX(I2) + IND
                           II2 = INDEX(JJ) + IIB
                           INX2 = INDEX(II1) + II2
                        ELSE
                           I2 = INDEX(JJBB) + IIB
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIB) + JJ
                           INX2 = INDEX(II1) + II2
                        ENDIF
                        C = SI2(INX) - SI2(INX2)
                        T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                        DO 7954 INA1 = ISAS0(ISB1),ISAS2(ISB1)-1
                           NEND = ISAC(INA1)
                           ICIA = ISPA(NEND)
                           ICI1 = ICIA + ICAB
                           ICI2 = ICIA + IBP2
                           DO 7952 KJ=1,NV
                         AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                         AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 7952                      CONTINUE
 7954                   CONTINUE
C
 7963                   CONTINUE
 7965                CONTINUE
C
C  Loop over orbitals in gaps in external space, ie AA -> AE
C
                     IGEL2 = NB-1
                     DO 7959 JJBB=NACT+1,NOCC
                     IF (JJBB.EQ.IBCON1(NB)) THEN
                        IGEL2=NB
                        GOTO 7959
                     ENDIF
C
                     IS4 = IOX(JJBB)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 7959
C
                     CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
                     IACON1(NB-1) = IACON1(NB-1)-NACT
                     IACON1(NB)   = IACON1(NB)-NACT
C
                     IPET1 = POSDET(NACT,NB-2,IACON1,IFA)
                     IPET2 = POSDET(NEXT,2,IACON1(NB-1),IFE)
                     IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                     IBP2 = ISPB(IPET)
                     IPER = IPER1 + IPER2
                     IPER = (-1)**IPER
C
C  Make matrix element.
C
                     II1 = INDEX(JJBB) + IO1
                     I2 = INDEX(JJBB) + IIB
                     INX = INDEX(I2) + IND
                     IF (IIB.LT.JJ) THEN
                        II2 = INDEX(JJ) + IIB
                        INX2 = INDEX(II1) + II2
                     ELSE
                        II2 = INDEX(IIB) + JJ
                        INX2 = INDEX(II1) + II2
                     ENDIF
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                     DO 7944 INA1 = ISAS0(ISB1),ISAS1(ISB1)-1
                        NEND = ISAC(INA1)
                        ICIA = ISPA(NEND)
                        ICI1 = ICIA + ICAB
                        ICI2 = ICIA + IBP2
                        DO 7942 KJ=1,NV
                      AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                      AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 7942                   CONTINUE
 7944                CONTINUE
C
 7959                CONTINUE
C
 7967             CONTINUE
C
C   Loop double excitations, 2nd excitation from external space.
C
               IBB = NB
               IIB = IBCON1(IBB)
               IS3 = IOX(IIB)
               IPB = IBB
C
C Loop over orbitals in gaps in 0th space, ie AE -> AA
C
               DO 7955 IGEL2=IGEL,NB-1
                  ISTBB = IBCON1(IGEL2)+1
                  IENBB = IBCON1(IGEL2+1)-1
                  IF (IGEL2.EQ.IGEL) ISTBB=JJ+1
                  IF (IGEL2.EQ.NB-1) IENBB=NACT
                  DO 7957 JJBB=ISTBB,IENBB
C
                     IS4 = IOX(JJBB)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 7957
C
                  CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
C
                     IPET = POSDET(NACT,NB,IACON1,IFA)
                     IBP2 = ISPB(IPET)
                     IPER = IPER1 + IPER2
                     IPER = (-1)**IPER
C
                     II1 = INDEX(JJBB) + IO1
                     I2 = INDEX(IIB) + JJBB
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIB) + JJ
                     INX2 = INDEX(II2) + II1
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                     DO 7934 INA1 = ISAS0(ISB1),ISAS2(ISB1)-1
                        NEND = ISAC(INA1)
                        ICIA = ISPA(NEND)
                        ICI1 = ICIA + ICAB
                        ICI2 = ICIA + IBP2
                        DO 7932 KJ=1,NV
                      AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                      AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 7932                   CONTINUE
 7934                CONTINUE
C
 7957             CONTINUE
 7955          CONTINUE
C
C  Loop over orbitals in gaps in external space, ie AE -> AE
C
               IGEL2 = NB-1
               DO 7953 JJBB=NACT+1,NOCC
                  IF (JJBB.EQ.IIB) THEN
                     IGEL2 = NB
                     GOTO 7953
                  ENDIF
C
                  IS4 = IOX(JJBB)
                  IP2 = IMUL(IS3,IS4)
                  IF (IP1.NE.IP2) GOTO 7953
C
              CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
C
                  IPET = POSDET(NACT,NB-1,IACON1,IFA)
                  IPET = (IPET-1)*NEXT + JJBB-NACT + NB0F
                  IBP2 = ISPB(IPET)
                  IPER = IPER1 + IPER2
                  IPER = (-1)**IPER
C
C  Make matrix element.
C
                  II1 = INDEX(JJBB) + IO1
                  II2 = INDEX(IIB) + JJ
                  IF (IIB.GT.JJBB) THEN
                     I2 = INDEX(IIB) + JJBB
                     INX = INDEX(I2) + IND
                     INX2 = INDEX(II2) + II1
                  ELSE
                     I2 = INDEX(JJBB) + IIB
                     INX = INDEX(I2) + IND
                     INX2 = INDEX(II1) + II2
                  ENDIF
                  C = SI2(INX) - SI2(INX2)
                  T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                  DO 7951 INA1 = ISAS0(ISB1),ISAS2(ISB1)-1
                     NEND = ISAC(INA1)
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IBP2
                     DO 7929 KJ=1,NV
                   AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                   AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 7929                CONTINUE
 7951             CONTINUE
C
 7953          CONTINUE
C
 7975          CONTINUE
 7980       CONTINUE
C
C  Loop over orbitals in gaps in external space, A -> E.
C
            IGEL = NB-1
            DO 7895 JJ=NACT+1,NOCC
               IF (JJ.EQ.IBCON1(NB)) THEN
                  IGEL = NB
                  GOTO 7895
               ENDIF
               IS2 = IOX(JJ)
               IP1 = IMUL(IS2,IS1)
C
               CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
C
               IPER = ((-1)**IPER1)
               IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry
C
               IF (IS1.NE.IS2) GOTO 7870
C
               IBCON2(NB-1) = IBCON2(NB-1)-NACT
               IBCON2(NB) = IBCON2(NB)-NACT
C
               IPET1 = POSDET(NACT,NB-2,IBCON2,IFA)
               IPET2 = POSDET(NEXT,2,IBCON2(NB-1),IFE)
               IPET = (IPET1-1)*NB2E + IPET2 + NB2S
               IPB1 = ISPB(IPET)
C
               IBCON2(NB-1) = IBCON2(NB-1)+NACT
               IBCON2(NB) = IBCON2(NB)+NACT
C
C  Make singly excited matrix elements.
C
               C = SI1(IND)
C
               DO 7805 IK=1,IB-1
                  ION = IBCON1(IK)
                  J1  = INDEX(ION+1)
                  JJ1 = INDEX(IND) + J1
                  J1  = INDEX(JJ) + ION
                  J2  = INDEX(IO1) + ION
                  INX = INDEX(J1) + J2
                  C = C + SI2(JJ1) - SI2(INX)
 7805          CONTINUE
C
               DO 7807 IK=IB+1,IGEL
                  ION = IBCON1(IK)
                  J1 = INDEX(ION+1)
                  JJ1 = INDEX(IND) + J1
                  J1 = INDEX(JJ) + ION
                  J2 = INDEX(ION) + IO1
                  INX = INDEX(J1) + J2
                  C = C + SI2(JJ1) - SI2(INX)
 7807          CONTINUE
C
               DO 7808 IK=IGEL+1,NB
                  ION = IBCON1(IK)
                  J1 = INDEX(ION+1)
                  JJ1 = INDEX(J1) + IND
                  J1 = INDEX(ION) + JJ
                  J2 = INDEX(ION) + IO1
                  INX = INDEX(J1) + J2
                  C = C + SI2(JJ1) - SI2(INX)
 7808          CONTINUE
C
C  Loop over all 0th alpha dets now.
C
                  DO 7810 II=1,NA
                     IACON1(II) = II
 7810             CONTINUE
C
                  NST = 1
                  DO 7815 INA1=ISAS0(ISB1),ISAS1(ISB1)-1
                     NEND = ISAC(INA1)
                     DO 7813 KK=NST,NEND-1
                        CALL ADVANC(IACON1,NA,NACT)
 7813                CONTINUE
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 7818 IK=1,NA
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 7818                CONTINUE
C
                     T = (C+D)*IPER
                     DO 7820 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 7820                CONTINUE
C
                     NST = NEND
 7815             CONTINUE
C
 7870          CONTINUE
C
C   Loop double excitations, AE -> EE
C
               IBB = NB
               IIB = IBCON1(IBB)
               IS3 = IOX(IIB)
               IPB = IBB
               IF (JJ.GT.IIB) IPB=IPB-1
C
C Loop over orbitals in gaps in external space > JJ, AE -> EE
C
               IGEL2 = IGEL
               DO 7865 JJBB=JJ+1,NOCC
                  IF (JJBB.EQ.IIB) THEN
                     IGEL2=NB
                     GOTO 7865
                  ENDIF
C
                  IS4 = IOX(JJBB)
                  IP2 = IMUL(IS3,IS4)
                  IF (IP1.NE.IP2) GOTO 7865
C
              CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
                  IACON1(NB-1) = IACON1(NB-1)-NACT
                  IACON1(NB)   = IACON1(NB)-NACT
C
                  IPET1 = POSDET(NACT,NB-2,IACON1,IFA)
                  IPET2 = POSDET(NEXT,2,IACON1(NB-1),IFE)
                  IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                  IBP2 = ISPB(IPET)
                  IPER = IPER1 + IPER2
                  IPER = (-1)**IPER
C
C Make matrix element.
C
                  II1 = INDEX(JJBB) + IO1
                  IF (IIB.GT.JJBB) THEN
                     I2 = INDEX(IIB) + JJBB
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIB) + JJ
                     INX2 = INDEX(II2) + II1
                  ELSEIF (IIB.LT.JJ) THEN
                     I2 = INDEX(JJBB) + IIB
                     INX = INDEX(I2) + IND
                     II2 = INDEX(JJ) + IIB
                     INX2 = INDEX(II1) + II2
                  ELSE
                     I2 = INDEX(JJBB) + IIB
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIB) + JJ
                     INX2 = INDEX(II1) + II2
                  ENDIF
                  C = SI2(INX) - SI2(INX2)
                  T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                  DO 7851 INA1 = ISAS0(ISB1),ISAS1(ISB1)-1
                     NEND = ISAC(INA1)
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IBP2
                     DO 7853 KJ=1,NV
                   AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                   AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 7853                CONTINUE
 7851             CONTINUE
C
C
 7865         CONTINUE
C
 7895       CONTINUE
C
 7985    CONTINUE
C
C  Loop single excitation from external space.
C
         IB = NB
         IO1 = IBCON1(NB)
         IS1 = IOX(IO1)
         IPER1 = 1
C
C  Loop over orbitals in external space, E -> E.
C
         DO 7795 JJ=IO1+1,NOCC
            IS2 = IOX(JJ)
            IP1 = IMUL(IS2,IS1)
C
            IF (IS1.NE.IS2) GOTO 7795
C
            CALL REDE01(IBCON1,IBCON2,NB,IB,JJ,IPER1)
            IPET = IPOS1 + (JJ-NACT)
            IPB1 = ISPB(IPET)
            IPER = ((-1)**IPER1)
            IND = INDEX(JJ) + IO1
C
C  Make singly excited matrix elements.
C
            C = SI1(IND)
C
            DO 7705 IK=1,IB-1
               ION = IBCON1(IK)
               J1  = INDEX(ION+1)
               JJ1 = INDEX(IND) + J1
               J1  = INDEX(JJ) + ION
               J2  = INDEX(IO1) + ION
               INX = INDEX(J1) + J2
               C = C + SI2(JJ1) - SI2(INX)
 7705       CONTINUE
C
C  Loop over all 0th alpha dets now.
C
            DO 7710 II=1,NA
               IACON1(II) = II
 7710       CONTINUE
C
            NST = 1
            DO 7715 INA1=ISAS0(ISB1),ISAS1(ISB1)-1
               NEND = ISAC(INA1)
               DO 7713 KK=NST,NEND-1
                  CALL ADVANC(IACON1,NA,NACT)
 7713          CONTINUE
C
               ICIA = ISPA(NEND)
               ICI1 = ICIA + ICAB
               ICI2 = ICIA + IPB1
C
               D = 0.0D+00
               DO 7718 IK=1,NA
                  ION = IACON1(IK)
                  J1 = INDEX(ION+1)
                  JJ1 = INDEX(IND) + J1
                  D = D + SI2(JJ1)
 7718          CONTINUE
C
               T = (C+D)*IPER
               DO 7720 KJ=1,NV
                  AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                  AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 7720          CONTINUE
C
               NST = NEND
 7715       CONTINUE
C
C  Loop over 1st order alpha strings.
C
            DO 7722 II=1,NA-1
               IACON1(II) = II
 7722       CONTINUE
            NSTF = 1
C
            DO 7730 INA1 = ISAS1(ISB1),ISAS2(ISB1)-1
               NIAP = ISAC(INA1) - NA0F
               NX = (NIAP-1)/NA1E + 1
               NY = NIAP - (NX-1)*NA1E
               DO 7724 KK=NSTF,NX-1
                  CALL ADVANC(IACON1,NA-1,NACT)
 7724          CONTINUE
               IACON1(NA) = NY+NACT
C
               ICIA = ISPA(NIAP+NA0F)
               ICI1 = ICIA + ICAB
               ICI2 = ICIA + IPB1
C
               D = 0.0D+00
               DO 7726 IK=1,NA-1
                  ION = IACON1(IK)
                  J1 = INDEX(ION+1)
                  JJ1 = INDEX(IND) + J1
                  D = D + SI2(JJ1)
 7726          CONTINUE
C
               ION = IACON1(NA)
               J1 = INDEX(ION+1)
               IMA = MAX(IND,J1)
               IMI = MIN(IND,J1)
               JJ1 = INDEX(IMA) + IMI
               D = D + SI2(JJ1)
C
               T = (C+D)*IPER
               DO 7728 KJ=1,NV
                  AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                  AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 7728          CONTINUE
C
               NSTF = NX
 7730       CONTINUE
C
 7795    CONTINUE
C
 7990    CONTINUE
         IF (NB.LE.1) GOTO 7993
         CALL ADVANC(IBCON1,NB-1,NACT)
 7993 CONTINUE
C
C    --------------------------------------------
C    Loop over all 2nd order beta strings.
C    --------------------------------------------
C
      IF (NB.LT.2) GOTO 8997
      DO 8995 II=1,NB-2
         IBCON1(II) = II
 8995 CONTINUE
      ICOUNT = NB2S
C
      DO 8993 IJK = 1,NB2F
         IBCON1(NB-1) = 1 + NACT
         IBCON1(NB) = 2 + NACT
         DO 8990 KJI=1,NB2E
            ICOUNT = ICOUNT + 1
            ICAB = ISPB(ICOUNT)
            ISB1 = ISYMB(ICOUNT)
C
C   Loop single excitations from 0th space.
C
         DO 8985 IB=1,NB-2
            IO1 = IBCON1(IB)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in zeroth order space, A -> A.
C
            DO 8980 IGEL=IB,NB-2
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-2) IEN = NACT
               DO 8975 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
C
                  IPER = ((-1)**IPER1)
                  IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 8970
C
                  IPET1 = POSDET(NACT,NB-2,IBCON2,IFA)
                  IPET2 = KJI
                  IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                  IPB1 = ISPB(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 8905 IK=1,IB-1
                     ION = IBCON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 8905             CONTINUE
C
                  DO 8907 IK=IB+1,IGEL
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 8907             CONTINUE
C
                  DO 8908 IK=IGEL+1,NB
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 8908             CONTINUE
C
C  Loop over all 0th alpha dets now.
C
                  DO 8910 II=1,NA
                     IACON1(II) = II
 8910             CONTINUE
C
                  NST = 1
                  DO 8915 INA1=ISAS0(ISB1),ISAS1(ISB1)-1
                     NEND = ISAC(INA1)
                     DO 8913 KK=NST,NEND-1
                        CALL ADVANC(IACON1,NA,NACT)
 8913                CONTINUE
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 8918 IK=1,NA
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 8918                CONTINUE
C
                     T = (C+D)*IPER
                     DO 8920 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 8920                CONTINUE
C
                     NST = NEND
 8915             CONTINUE
C
 8970             CONTINUE
C
C
C  Loop double excitations from 0th space.
C
                  DO 8967 IBB=IB+1,NB-2
                     IIB = IBCON1(IBB)
                     IS3 = IOX(IIB)
                     IPB = IBB
                     IF (JJ.GT.IIB) IPB=IPB-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA.
C
                     DO 8965 IGEL2=IGEL,NB-2
                        ISTBB = IBCON1(IGEL2)+1
                        IENBB = IBCON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTBB=JJ+1
                        IF (IGEL2.EQ.NB-2) IENBB=NACT
                        DO 8963 JJBB=ISTBB,IENBB
C
                           IS4 = IOX(JJBB)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 8963
C
                  CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
                           IPET1 = POSDET(NACT,NB-2,IACON1,IFA)
                           IPET2 = KJI
                           IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                           IBP2 = ISPB(IPET)
                           IPER = IPER1 + IPER2
                           IPER = (-1)**IPER
C
C   Make matrix element.
C
                           II1 = INDEX(JJBB) + IO1
                           IF (IIB.GT.JJBB) THEN
                              I2 = INDEX(IIB) + JJBB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIB) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIB.LT.JJ) THEN
                              I2 = INDEX(JJBB) + IIB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIB
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJBB) + IIB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIB) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                        DO 8966 INA1 = ISAS0(ISB1),ISAS1(ISB1)-1
                           NEND = ISAC(INA1)
                           ICIA = ISPA(NEND)
                           ICI1 = ICIA + ICAB
                           ICI2 = ICIA + IBP2
                           DO 8961 KJ=1,NV
                         AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                         AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 8961                      CONTINUE
 8966                   CONTINUE
C
 8963                   CONTINUE
 8965                CONTINUE
C
 8967             CONTINUE
C
C  Loop double excitations, 2nd excitation from external space.
C
                  DO 8957 IBB=NB-1,NB
                     IIB = IBCON1(IBB)
                     IS3 = IOX(IIB)
                     IPB = IBB
C
C  Loop over orbitals in gaps in 0th space, ie AE -> AA
C
                     DO 8955 IGEL2=IGEL,NB-2
                        ISTBB = IBCON1(IGEL2)+1
                        IENBB = IBCON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTBB=JJ+1
                        IF (IGEL2.EQ.NB-2) IENBB=NACT
                        DO 8953 JJBB=ISTBB,IENBB
C
                           IS4 = IOX(JJBB)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 8953
C
                     CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
C
                           IORB = IACON1(NB)-NACT
                           IPET = POSDET(NACT,NB-1,IACON1,IFA)
                           IPET = (IPET-1)*NEXT + IORB + NB0F
                           IBP2 = ISPB(IPET)
                           IPER = IPER1 + IPER2
                           IPER = (-1)**IPER
C
C  Make matrix element.
C
                           II1 = INDEX(JJBB) + IO1
                           I2 = INDEX(IIB) + JJBB
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIB) + JJ
                           INX2 = INDEX(II2) + II1
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                        DO 8956 INA1 = ISAS0(ISB1),ISAS1(ISB1)-1
                           NEND = ISAC(INA1)
                           ICIA = ISPA(NEND)
                           ICI1 = ICIA + ICAB
                           ICI2 = ICIA + IBP2
                           DO 8931 KJ=1,NV
                         AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                         AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 8931                      CONTINUE
 8956                   CONTINUE
C
 8953                   CONTINUE
 8955                CONTINUE
C
C  Loop over orbitals in gaps in excited space, ie AE -> AE.
C
                     DO 8951 IGEL2=NB-2,NB
                        ISTBB = IBCON1(IGEL2)+1
                        IENBB = IBCON1(IGEL2+1)-1
                        IF (IGEL2.EQ.NB-2) ISTBB=NACT+1
                        IF (IGEL2.EQ.NB) IENBB=NOCC
                        DO 8949 JJBB=ISTBB,IENBB
C
                           IS4 = IOX(JJBB)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 8949
C
                     CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
                           IACON1(NB-1) = IACON1(NB-1)-NACT
                           IACON1(NB)   = IACON1(NB)-NACT
C
                           IPET1 = POSDET(NACT,NB-2,IACON1,IFA)
                           IPET2 = POSDET(NEXT,2,IACON1(NB-1),IFE)
                           IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                           IBP2 = ISPB(IPET)
                           IPER = IPER1 + IPER2
                           IPER = (-1)**IPER
C
C  Make matrix element.
C
                           II1 = INDEX(JJBB) + IO1
                           IF (IIB.GT.JJBB) THEN
                              I2 = INDEX(IIB) + JJBB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIB) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSE
                              I2 = INDEX(JJBB) + IIB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIB) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                        DO 8946 INA1 = ISAS0(ISB1),ISAS1(ISB1)-1
                           NEND = ISAC(INA1)
                           ICIA = ISPA(NEND)
                           ICI1 = ICIA + ICAB
                           ICI2 = ICIA + IBP2
                           DO 8941 KJ=1,NV
                         AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                         AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 8941                      CONTINUE
 8946                   CONTINUE
C
 8949                   CONTINUE
 8951                CONTINUE
C
 8957             CONTINUE
C
 8975          CONTINUE
 8980       CONTINUE
C
 8985   CONTINUE
C
C  Loop single excitations from excited space.
C
         DO 8785 IB=NB-1,NB
            IO1 = IBCON1(IB)
            IS1 = IOX(IO1)
C
C
C  Loop over orbitals in gaps in excited space, E -> E.
C
            DO 8780 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN=NOCC
               DO 8775 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
C
                  IPER = ((-1)**IPER1)
                  IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 8770
C
                  IBCON2(NB-1) = IBCON2(NB-1)-NACT
                  IBCON2(NB)   = IBCON2(NB)-NACT
C
                  IPET1 = IJK
                  IPET2 = POSDET(NEXT,2,IBCON2(NB-1),IFE)
                  IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                  IPB1 = ISPB(IPET)
C
                  IBCON2(NB-1) = IBCON2(NB-1)+NACT
                  IBCON2(NB) = IBCON2(NB)+NACT
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 8705 IK=1,IB-1
                     ION = IBCON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 8705             CONTINUE
C
                  DO 8707 IK=IB+1,IGEL
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 8707             CONTINUE
C
                  DO 8708 IK=IGEL+1,NB
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 8708             CONTINUE
C
C  Loop over all 0th alpha dets now.
C
                  DO 8710 II=1,NA
                     IACON1(II) = II
 8710             CONTINUE
C
                  NST = 1
                  DO 8715 INA1=ISAS0(ISB1),ISAS1(ISB1)-1
                     NEND = ISAC(INA1)
                     DO 8713 KK=NST,NEND-1
                        CALL ADVANC(IACON1,NA,NACT)
 8713                CONTINUE
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 8718 IK=1,NA
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 8718                CONTINUE
C
                     T = (C+D)*IPER
                     DO 8720 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 8720                CONTINUE
C
                     NST = NEND
 8715             CONTINUE
C
 8770             CONTINUE
C
C   Loop double excitations from excited space, ie EE -> EE.
C
                  DO 8767 IBB=IB+1,NB
                     IIB = IBCON1(IBB)
                     IS3 = IOX(IIB)
                     IPB = IBB
                     IF (JJ.GT.IIB) IPB=IPB-1
C
                     DO 8765 IGEL2=IGEL,NB
                        ISTBB = IBCON1(IGEL2)+1
                        IENBB = IBCON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTBB=JJ+1
                        IF (IGEL2.EQ.NB) IENBB=NOCC
                        DO 8763 JJBB=ISTBB,IENBB
C
                           IS4 = IOX(JJBB)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 8763
C
                     CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
                           IACON1(NB-1) = IACON1(NB-1)-NACT
                           IACON1(NB)   = IACON1(NB)-NACT
C
                           IPET1 = IJK
                           IPET2 = POSDET(NEXT,2,IACON1(NB-1),IFE)
                           IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                           IBP2 = ISPB(IPET)
                           IPER = IPER1 + IPER2
                           IPER = (-1)**IPER
C
C  Make matrix element.
C
                           II1 = INDEX(JJBB) + IO1
                           IF (IIB.GT.JJBB) THEN
                              I2 = INDEX(IIB) + JJBB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIB) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIB.LT.JJ) THEN
                              I2 = INDEX(JJBB) + IIB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIB
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJBB) + IIB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIB) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                        DO 8746 INA1 = ISAS0(ISB1),ISAS1(ISB1)-1
                           NEND = ISAC(INA1)
                           ICIA = ISPA(NEND)
                           ICI1 = ICIA + ICAB
                           ICI2 = ICIA + IBP2
                           DO 8741 KJ=1,NV
                         AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                         AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 8741                      CONTINUE
 8746                   CONTINUE
C
 8763                   CONTINUE
 8765                CONTINUE
C
 8767             CONTINUE
C
 8775          CONTINUE
 8780       CONTINUE
C
 8785    CONTINUE
C
            CALL ADVANC(IBCON1(NB-1),2,NOCC)
 8990    CONTINUE
         IF (NB.LE.2) GOTO 8993
         CALL ADVANC(IBCON1,NB-2,NACT)
 8993 CONTINUE
 8997 CONTINUE
C
C   Now for the diagonal contributions
C
      DO 9119 IJK = 1,NSOCI
         DO 9118 KJ = 1,NV
            AB(IJK,KJ) = AB(IJK,KJ) + Q(IJK)*CI(IJK,KJ)
 9118    CONTINUE
 9119 CONTINUE
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK RINSO01
C     -----------------------------------------------------------
      SUBROUTINE RINSO01(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,IOX,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA00,IPERA00,IIND100,IMMC00,
     *    IPOSA01,IPERA01,IIND101,IMMC01)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION SI1(*),SI2(*),AB(NSOCI),CI(NSOCI)
      DIMENSION IFA(0:NACT,0:NACT),IFE(0:NEXT,0:2)
      DIMENSION IACON1(NA),IBCON1(NA),IACON2(NA),IBCON2(NA)
      DIMENSION INDEX((NOCC*(NOCC+1))/2 + 1)
      DIMENSION IOX(NOCC)
      DIMENSION ITAB(NSYM),IMUL(NSYM,NSYM)
      DIMENSION ISYMA(NATT),ISYMB(NATT),ISPA(NATT),ISPB(NATT)
      DIMENSION ISBS0(NSYM+1),ISBS1(NSYM),ISBS2(NSYM)
      DIMENSION ISBC(NATT)
      DIMENSION IPOSA00((NA*(NACT-NA)),NSYM)
      DIMENSION IPERA00((NA*(NACT-NA)),NSYM)
      DIMENSION IIND100((NA*(NACT-NA)),NSYM)
      DIMENSION IMMC00(NSYM)
      DIMENSION IPOSA01(NA*NEXT,NSYM)
      DIMENSION IPERA01(NA*NEXT,NSYM)
      DIMENSION IIND101(NA*NEXT,NSYM)
      DIMENSION IMMC01(NSYM)
C
      DO 13 II=1,NSOCI
         AB(II) = 0.0D+00
   13 CONTINUE
C
C    --------------------------------------------
C    Loop over all 0th order alpha determinants.
C    --------------------------------------------
C
      DO 20 II=1,NA
         IACON1(II)=II
   20 CONTINUE
C
      DO 2000 IJK=1,NA0F
         ICAT = ISPA(IJK)
         ISA1 = ISYMA(IJK)
         ITAS = ITAB(ISA1)
C
         DO 45 II=1,NSYM
            IMMC00(II) = 0
            IMMC01(II) = 0
   45    CONTINUE
C
C   Loop single excitations from 0th space.
C
         DO 990 IA=1,NA
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C   Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 980 IGEL=IA,NA
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA) IEN=NACT
               DO 970 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
C
C  Storage here for later.
C
                  IPET = POSDET(NACT,NA,IACON2,IFA)
                  IMMC00(ISYMA(IPET)) = IMMC00(ISYMA(IPET)) + 1
                  IPOSA00(IMMC00(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
                  IPER11 = (-1)**IPER1
                  IPERA00(IMMC00(ISYMA(IPET)),ISYMA(IPET)) = IPER11
                  IND = INDEX(JJ)+IO1
                  IIND100(IMMC00(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 900
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 905 IK=1,IA-1
                     J1  = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + IACON1(IK)
                     J2  = INDEX(IO1) + IACON1(IK)
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  905             CONTINUE
C
                  DO 907 IK=IA+1,IGEL
                     J1 = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + IACON1(IK)
                     J2 = INDEX(IACON1(IK)) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  907             CONTINUE
C
                  DO 908 IK=IGEL+1,NA
                     J1 = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(IACON1(IK)) + JJ
                     J2 = INDEX(IACON1(IK)) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  908             CONTINUE
C
C  Loop over all beta strings now. 0th order first.
C
                  DO 910 II=1,NB
                     IBCON1(II) = II
  910             CONTINUE
C
                  NST = 1
                  DO 915 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 913 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
  913                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 918 IK=1,NB
                        J1 = INDEX(IBCON1(IK)+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
  918                CONTINUE
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
  915             CONTINUE
C
C  Loop over 1st order beta strings.
C
                  DO 922 II=1,NB-1
                     IBCON1(II) = II
  922             CONTINUE
                  NSTF = 1
C
                  DO 930 INB1 = ISBS1(ISA1),ISBS2(ISA1)-1
                     NIBP = ISBC(INB1) - NB0F
                     NX = (NIBP-1)/NB1E + 1
                     NY = NIBP - (NX-1)*NB1E
                     DO 924 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-1,NACT)
  924                CONTINUE
                     IBCON1(NB) = NY+NACT
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 926 IK=1,NB-1
                        J1 = INDEX(IBCON1(IK)+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
  926                CONTINUE
C
                     J1 = INDEX(IBCON1(NB)+1)
                     JJ1 = INDEX(J1) + IND
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NSTF = NX
  930             CONTINUE
C
C
C  Loop over 2nd order beta strings.
C
                  IF (NB.LT.2) GOTO 900
                  DO 932 II=1,NB-2
                     IBCON1(II) = II
  932             CONTINUE
                  IBCON1(NB-1) = 1 + NACT
                  IBCON1(NB) = 2 + NACT
                  NSTF = 1
                  NSTE = 1
C
                  DO 940 INB1 = ISBS2(ISA1),ISBS0(ISA1+1)-1
                     NIBP = ISBC(INB1) - NB2S
                     NX = (NIBP-1)/NB2E + 1
                     NY = NIBP - (NX-1)*NB2E
                     DO 934 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-2,NACT)
  934                CONTINUE
                     IF (NY.LT.NSTE) THEN
                        IBCON1(NB-1) = 1 + NACT
                        IBCON1(NB)   = 2 + NACT
                        NSTE = 1
                     ENDIF
                     DO 936 KK=NSTE,NY-1
                        CALL ADVANC(IBCON1(NB-1),2,NOCC)
  936                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 937 IK=1,NB-2
                        J1 = INDEX(IBCON1(IK)+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
  937                CONTINUE
C
                     DO 938 IK=NB-1,NB
                        J1 = INDEX(IBCON1(IK)+1)
                        JJ1 = INDEX(J1) + IND
                        D = D + SI2(JJ1)
  938                CONTINUE
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NSTF = NX
                     NSTE = NY
C
  940             CONTINUE
C
  900             CONTINUE
C
C   Loop double excitations from 0th space.
C
                  DO 967 IAA=IA+1,NA
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
                     IF (JJ.GT.IIA) IPA=IPA-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA
C
                     DO 965 IGEL2=IGEL,NA
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA) IENAA=NACT
                        DO 963 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 963
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                           IPET = POSDET(NACT,NA,IBCON1,IFA)
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = (-1)**(IPER1 + IPER2)
C
C   Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           IF (IIA.GT.JJAA) THEN
                              I2 = INDEX(IIA) + JJAA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIA.LT.JJ) THEN
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIA
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                           DO 961 INB1 = ISBS0(ISA1),ISBS0(ISA1+1)-1
                              ICI1 = ICI1 + 1
                              ICI2 = ICI2 + 1
                            AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                            AB(ICI2) = AB(ICI2) + T*CI(ICI1)
  961                      CONTINUE
C
  963                   CONTINUE
  965                CONTINUE
C
C  Loop over orbitals in external space, ie AA -> AE
C
                     DO 950 JJAA=NACT+1,NOCC
C
                        IS4 = IOX(JJAA)
                        IP2 = IMUL(IS3,IS4)
                        IF (IP1.NE.IP2) GOTO 950
C
                        CALL REDE01(IACON2,IBCON1,NA,IPA,JJAA,IPER2)
C
                        IPET = POSDET(NACT,NA-1,IBCON1,IFA)
                        IPET = (IPET-1)*NEXT + JJAA-NACT + NA0F
                        ICI1 = ICAT
                        ICI2 = ISPA(IPET)
                        IPERT = (-1)**(IPER1 + IPER2)
C
C  Make matrix element.
C
                        II1 = INDEX(JJAA) + IO1
                        I2 = INDEX(JJAA) + IIA
                        INX = INDEX(I2) + IND
                        IF (IIA.LT.JJ) THEN
                           II2 = INDEX(JJ) + IIA
                           INX2 = INDEX(II1) + II2
                        ELSE
                           II2 = INDEX(IIA) + JJ
                           INX2 = INDEX(II1) + II2
                        ENDIF
                        C = SI2(INX) - SI2(INX2)
                        T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 954 INB1 = ISBS0(ISA1),ISBS2(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                         AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                         AB(ICI2) = AB(ICI2) + T*CI(ICI1)
  954                   CONTINUE
C
  950                CONTINUE
C
  967             CONTINUE
C
  970          CONTINUE
  980       CONTINUE
C
C  Loop over orbitals in external gap, ie  A -> E.
C
            DO 880 JJ=NACT+1,NOCC
C
               IS2 = IOX(JJ)
               IP1 = IMUL(IS2,IS1)
C
               CALL REDE01(IACON1,IACON2,NA,IA,JJ,IPER1)
C
C  Storage here for later.
C
C
               IPET = (POSDET(NACT,NA-1,IACON2,IFA)-1)*NEXT
               IPET = IPET + NA0F + (JJ-NACT)
               IMMC01(ISYMA(IPET)) = IMMC01(ISYMA(IPET)) + 1
               IPOSA01(IMMC01(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
               IPER11 = (-1)**IPER1
               IPERA01(IMMC01(ISYMA(IPET)),ISYMA(IPET)) = IPER11
               IND = INDEX(JJ)+IO1
               IIND101(IMMC01(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
               IF (IS1.NE.IS2) GOTO 800
               ICI1 = ICAT
               ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 805 IK=1,IA-1
                     J1  = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + IACON1(IK)
                     J2  = INDEX(IO1) + IACON1(IK)
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  805             CONTINUE
C
                  DO 807 IK=IA+1,NA
                     J1 = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + IACON1(IK)
                     J2 = INDEX(IACON1(IK)) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  807             CONTINUE
C
C  Loop over all beta dets now. 0th order first.
C
                  DO 810 II=1,NB
                     IBCON1(II) = II
  810             CONTINUE
C
                  NST = 1
                  DO 815 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 813 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
  813                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 818 IK=1,NB
                        J1 = INDEX(IBCON1(IK)+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
  818                CONTINUE
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
  815             CONTINUE
C
C  Loop over 1st order beta strings.
C
                  DO 822 II=1,NB-1
                     IBCON1(II) = II
  822             CONTINUE
                  NSTF = 1
C
                  DO 830 INB1 = ISBS1(ISA1),ISBS2(ISA1)-1
                     NIBP = ISBC(INB1) - NB0F
                     NX = (NIBP-1)/NB1E + 1
                     NY = NIBP - (NX-1)*NB1E
                     DO 824 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-1,NACT)
  824                CONTINUE
                     IBCON1(NB) = NY+NACT
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 826 IK=1,NB-1
                        J1 = INDEX(IBCON1(IK)+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
  826                CONTINUE
C
                     J1 = INDEX(IBCON1(NB)+1)
                     IMA = MAX(IND,J1)
                     IMI = MIN(IND,J1)
                     JJ1 = INDEX(IMA) + IMI
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NSTF = NX
  830             CONTINUE
C
  800          CONTINUE
C
C   Loop double excitations from 0th space.
C
               DO 867 IAA=IA+1,NA
                  IIA = IACON1(IAA)
                  IS3 = IOX(IIA)
                  IPA = IAA - 1
C
C  Loop over external orbitals > JJ, ie AA -> EE
C
                  DO 865 JJAA=JJ+1,NOCC
C
                     IS4 = IOX(JJAA)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 865
C
                     CALL REDE01(IACON2,IBCON1,NA,IPA,JJAA,IPER2)
                     IBCON1(NA-1) = JJ-NACT
                     IBCON1(NA)   = JJAA-NACT
C
                     IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                     IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                     IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                     ICI1 = ICAT
                     ICI2 = ISPA(IPET)
                     IPERT = IPER1 + IPER2
                     IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                     II1 = INDEX(JJAA) + IO1
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(JJ) + IIA
                     INX2 = INDEX(II1) + II2
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 863 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                      AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                      AB(ICI2) = AB(ICI2) + T*CI(ICI1)
  863                CONTINUE
C
  865             CONTINUE
C
  867          CONTINUE
C
  880       CONTINUE
C
  990    CONTINUE
C
C    Loop over all betas and their single excites now.
C
C   First loop over all 0th order betas
C
         DO 1010 II=1,NB
            IBCON1(II) = II
 1010    CONTINUE
C
         DO 1995 KJI=1,NB0F
            IPB1 = ISPB(KJI)
            ISB1 = ISYMB(KJI)
            ITBS = ITAB(ISB1)
            IMZZ0 = IMMC00(ITBS)
            IMZZ1 = IMMC01(ITBS)
            IMZZ = IMZZ0 + IMZZ1
            IF (IMZZ.EQ.0.AND.ISB1.NE.ITAS) GOTO 1994
            IC1 = ICAT + IPB1
C
C   Loop single excitations from 0th space.
C
         DO 1990 IB=1,NB
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C   Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 1980 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN=NACT
               DO 1970 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ10 = IMMC00(ITB2)
                  IMZ11 = IMMC01(ITB2)
                  IMZ1 = IMZ10 + IMZ11
C
                  IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 1975
                  IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1975
                  GOTO 1970
C
 1975             CONTINUE
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
                  IPOSB = POSDET(NACT,NB,IBCON2,IFA)
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1960 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1960                CONTINUE
                     DO 1964 IAT=1,IMZ11
                        IC4 = IPOSA01(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITB2))
                        I2 = MIN(IOB,IIND101(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1964                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1955 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1955                CONTINUE
                     DO 1953 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1953                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1950 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1950                CONTINUE
                     DO 1958 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        IC4 = IPOSA01(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1958                CONTINUE
C
                  ENDIF
C
 1970          CONTINUE
 1980       CONTINUE
C
C  Loop over orbitals in external gap, ie  A -> E.
C
            DO 1880 JJ=NACT+1,NOCC
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ10 = IMMC00(ITB2)
               IMZ11 = IMMC01(ITB2)
               IMZ1 = IMZ10 + IMZ11
C
               IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 1875
               IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1875
               GOTO 1880
Cc
 1875          CONTINUE
               CALL REDE01(IBCON1,IBCON2,NB,IB,JJ,IPER1)
               IPOSB = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
               IPOSB = IPOSB + NB0F + (JJ-NACT)
               IPB2 = ISPB(IPOSB)
               IPERB = (-1)**IPER1
               IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1860 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1860                CONTINUE
                     DO 1865 IAT=1,IMZ11
                        IC4 = IPOSA01(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITB2))
                        I2 = MIN(IOB,IIND101(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1865                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1855 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1855                CONTINUE
                     DO 1858 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1858                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1850 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1850                CONTINUE
                     DO 1853 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        IC4 = IPOSA01(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1853                CONTINUE
C
                  ENDIF
C
 1880          CONTINUE
C
 1990       CONTINUE
Cc
 1994       CONTINUE
            CALL ADVANC(IBCON1,NB,NACT)
 1995    CONTINUE
C
C  Now loop over 1st order beta strings.
C
         DO 1840 II=1,NB-1
            IBCON1(II) = II
 1840    CONTINUE
         ICOUNT = NB0F
C
         DO 1920 KJI=1,NB1F
            IPOS1 = (KJI-1)*NEXT + NB0F
            DO 1910 KJJ=NACT+1,NOCC
               IBCON1(NB) = KJJ
               ICOUNT = ICOUNT + 1
C
               IPB1 = ISPB(ICOUNT)
               ISB1 = ISYMB(ICOUNT)
               ITBS = ITAB(ISB1)
               IMZZ0 = IMMC00(ITBS)
               IMZZ1 = IMMC01(ITBS)
               IMZZ = IMZZ0 + IMZZ1
               IF (IMZZ.EQ.0.AND.ISB1.NE.ITAS) GOTO 1910
               IC1 = ICAT + IPB1
C
C   Loop single excitations from 0th space.
C
         DO 1905 IB=1,NB-1
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C   Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 1790 IGEL=IB,NB-1
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-1) IEN=NACT
               DO 1780 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ10 = IMMC00(ITB2)
                  IMZ11 = IMMC01(ITB2)
                  IMZ1 = IMZ10 + IMZ11
C
                  IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 1785
                  IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1785
                  GOTO 1780
C
 1785             CONTINUE
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
                  IPOSB = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
                  IPOSB = IPOSB + NB0F + (KJJ-NACT)
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1760 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1760                CONTINUE
                     DO 1764 IAT=1,IMZ11
                        IC4 = IPOSA01(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITB2))
                        I2 = MIN(IOB,IIND101(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1764                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1755 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1755                CONTINUE
                     DO 1758 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1758                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1750 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1750                CONTINUE
                     DO 1753 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        IC4 = IPOSA01(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1753                CONTINUE
C
                  ENDIF
C
 1780          CONTINUE
 1790       CONTINUE
C
C   Loop over orbitals in gaps in external space, ie A -> E.
C
            IGEL = NB-1
            DO 1690 JJ=NACT+1,NOCC
               IF (JJ.EQ.IBCON1(NB)) THEN
                  IGEL=NB
                  GOTO 1690
               ENDIF
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ10 = IMMC00(ITB2)
               IMZ11 = IMMC01(ITB2)
C
               IF (ISB1.EQ.ITAS.AND.IMZ10.NE.0) GOTO 1685
               IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1685
               GOTO 1690
C
 1685          CONTINUE
               CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
               IBCON2(NB-1) = IBCON2(NB-1)-NACT
               IBCON2(NB)   = IBCON2(NB)-NACT
C
               IPET1 = POSDET(NACT,NB-2,IBCON2,IFA)
               IPET2 = POSDET(NEXT,2,IBCON2(NB-1),IFE)
               IPOSB = (IPET1-1)*NB2E + IPET2 + NB2S
               IPB2 = ISPB(IPOSB)
               IPERB = (-1)**IPER1
               IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1660 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1660                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ10.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1655 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1655                CONTINUE
                     DO 1658 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1658                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1650 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1650                CONTINUE
                     DO 1653 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1653                CONTINUE
C
                  ENDIF
C
 1690       CONTINUE
C
 1905    CONTINUE
C
C   Loop single excitations from External space.
C
         IBB = IBCON1(NB)
         IB1 = IOX(IBB)
         IR1 = IMUL(IB1,ISB1)
         IPERB = 1
C
C  Loop over orbitals in gaps in external space, ie E -> E.
C
         DO 1590 JJ=IBB+1,NOCC
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ10 = IMMC00(ITB2)
               IMZ11 = IMMC01(ITB2)
               IMZ1 = IMZ10 + IMZ11
C
               IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 1585
               IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1585
               GOTO 1590
C
 1585          CONTINUE
               IPOSB = IPOS1 + (JJ-NACT)
               IPB2 = ISPB(IPOSB)
               IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1560 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1560                CONTINUE
                     DO 1563 IAT=1,IMZ11
                        IC4 = IPOSA01(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITB2))
                        I2 = MIN(IOB,IIND101(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1563                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1555 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1555                CONTINUE
                     DO 1554 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1554                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1550 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1550                CONTINUE
                     DO 1558 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        IC4 = IPOSA01(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1558                CONTINUE
C
                  ENDIF
C
 1590    CONTINUE
C
 1910       CONTINUE
            IF (NB.LE.1) GOTO 1920
            CALL ADVANC(IBCON1,NB-1,NACT)
 1920    CONTINUE
C
C  Now loop over 2nd order beta strings.
C
         IF (NB.LT.2) GOTO 1497
         DO 1495 II=1,NB-2
            IBCON1(II) = II
 1495    CONTINUE
         ICOUNT = NB2S
C
         DO 1490 KJI=1,NB2F
            IBCON1(NB-1) = 1 + NACT
            IBCON1(NB) = 2 + NACT
            DO 1485 KJJ=1,NB2E
               ICOUNT = ICOUNT + 1
C
               IPB1 = ISPB(ICOUNT)
               ISB1 = ISYMB(ICOUNT)
               ITBS = ITAB(ISB1)
               IMZZ0 = IMMC00(ITBS)
               IF (IMZZ0.EQ.0.AND.ISB1.NE.ITAS) GOTO 1487
               IC1 = ICAT + IPB1
C
C  Loop single excitations from 0th space.
C
         DO 1480 IB=1,NB-2
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 1475 IGEL=IB,NB-2
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-2) IEN = NACT
               DO 1470 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ10 = IMMC00(ITB2)
C
                  IF (ISB1.EQ.ITAS.AND.IMZ10.NE.0) GOTO 1465
                  IF (ISB2.EQ.ITAS.AND.IMZZ0.NE.0) GOTO 1465
                  GOTO 1470
C
 1465             CONTINUE
                  CALL REDE00(IBCON1,IBCON2,NB-2,IB,IGEL,JJ,IPER1)
C
                  IPET1 = POSDET(NACT,NB-2,IBCON2,IFA)
                  IPOSB = (IPET1-1)*NB2E + KJJ + NB2S
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ0.EQ.0) THEN
                     DO 1460 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1460                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ10.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1455 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1455                CONTINUE
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1450 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1450                CONTINUE
                  ENDIF
C
 1470          CONTINUE
 1475       CONTINUE
C
 1480    CONTINUE
C
C  Loop single excitations from Excited space.
C
         DO 1400 IB=NB-1,NB
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C  Loop over orbitals in gaps in Excited space, E -> E.
C
            DO 1395 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN = NOCC
               DO 1390 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ10 = IMMC00(ITB2)
C
                  IF (ISB1.EQ.ITAS.AND.IMZ10.NE.0) GOTO 1385
                  IF (ISB2.EQ.ITAS.AND.IMZZ0.NE.0) GOTO 1385
                  GOTO 1390
C
 1385             CONTINUE
                  CALL REDE00(IBCON1(NB-1),IBCON2(NB-1),
     *            2,IB-NB+2,IGEL-NB+2,JJ,IPER1)
C
                  IBCON2(NB-1) = IBCON2(NB-1)-NACT
                  IBCON2(NB)   = IBCON2(NB)-NACT
                  IPET2 = POSDET(NEXT,2,IBCON2(NB-1),IFE)
                  IPOSB = (KJI-1)*NB2E + IPET2 + NB2S
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ0.EQ.0) THEN
                     DO 1360 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1360                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ10.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1355 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1355                CONTINUE
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1350 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1350                CONTINUE
                  ENDIF
C
 1390          CONTINUE
 1395       CONTINUE
C
 1400   CONTINUE
C
 1487       CONTINUE
            CALL ADVANC(IBCON1(NB-1),2,NOCC)
 1485    CONTINUE
C
         IF (NB.LE.2) GOTO 1490
         CALL ADVANC(IBCON1,NB-2,NACT)
 1490    CONTINUE
 1497    CONTINUE
C
         CALL ADVANC(IACON1,NA,NACT)
 2000 CONTINUE
C
C    --------------------------------------------
C    End of loop over all 0th order alpha strings.
C    --------------------------------------------
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK RINSO11
C     -----------------------------------------------------------
      SUBROUTINE RINSO11(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,IOX,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA11,IPERA11,IIND111,IMMC11,
     *    IPOSA12,IPERA12,IIND112,IMMC12)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION SI1(*),SI2(*),AB(NSOCI),CI(NSOCI)
      DIMENSION IFA(0:NACT,0:NACT),IFE(0:NEXT,0:2)
      DIMENSION IACON1(NA),IBCON1(NA),IACON2(NA),IBCON2(NA)
      DIMENSION INDEX((NOCC*(NOCC+1))/2 + 1)
      DIMENSION IOX(NOCC)
      DIMENSION ITAB(NSYM),IMUL(NSYM,NSYM)
      DIMENSION ISYMA(NATT),ISYMB(NATT),ISPA(NATT),ISPB(NATT)
      DIMENSION ISBS0(NSYM+1),ISBS1(NSYM),ISBS2(NSYM)
      DIMENSION ISBC(NATT)
      DIMENSION IPOSA11(((NA-1)*(NACT-NA+1)+(NEXT-1)),NSYM)
      DIMENSION IPERA11(((NA-1)*(NACT-NA+1)+(NEXT-1)),NSYM)
      DIMENSION IIND111(((NA-1)*(NACT-NA+1)+(NEXT-1)),NSYM)
      DIMENSION IMMC11(NSYM)
      DIMENSION IPOSA12((NA-1)*(NEXT-1),NSYM)
      DIMENSION IPERA12((NA-1)*(NEXT-1),NSYM)
      DIMENSION IIND112((NA-1)*(NEXT-1),NSYM)
      DIMENSION IMMC12(NSYM)
C
C    --------------------------------------------
C    Loop over all 1st order alpha strings.
C    --------------------------------------------
C
      DO 3995 II=1,NA-1
         IACON1(II)=II
 3995 CONTINUE
      ICOUNT = NA0F
C
      DO 3993 IJK=1,NA1F
         IPOS1 = (IJK-1)*NEXT + NA0F
         DO 3990 KJI=NACT+1,NOCC
            IACON1(NA) = KJI
            ICOUNT = ICOUNT + 1
            ICAT = ISPA(ICOUNT)
            ISA1 = ISYMA(ICOUNT)
            ITAS = ITAB(ISA1)
C
            DO 3005 II=1,NSYM
               IMMC11(II) = 0
               IMMC12(II) = 0
 3005       CONTINUE
C
C   Loop single excitations from 0th space.
C
         DO 3985 IA=1,NA-1
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 3980 IGEL=IA,NA-1
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA-1) IEN = NACT
               DO 3975 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
C
C  Storage here for later.
C
                  IPET = (POSDET(NACT,NA-1,IACON2,IFA)-1)*NEXT
                  IPET = IPET + NA0F + (KJI-NACT)
                  IMMC11(ISYMA(IPET)) = IMMC11(ISYMA(IPET)) + 1
                  IPOSA11(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
                  IPER11 = (-1)**IPER1
                  IPERA11(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = IPER11
                  IND = INDEX(JJ)+IO1
                  IIND111(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 3970
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 3905 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3905             CONTINUE
C
                  DO 3907 IK=IA+1,IGEL
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3907             CONTINUE
C
                  DO 3908 IK=IGEL+1,NA
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3908             CONTINUE
C
C  Loop over 0th and 1st beta strings now. 0th order first.
C
                  DO 3910 II=1,NB
                     IBCON1(II) = II
 3910             CONTINUE
C
                  NST = 1
                  DO 3915 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 3913 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 3913                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3918 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 3918                CONTINUE
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
 3915             CONTINUE
C
C  Loop over 1st order beta strings.
C
                  DO 3922 II=1,NB-1
                     IBCON1(II) = II
 3922             CONTINUE
                  NSTF = 1
C
                  DO 3930 INB1 = ISBS1(ISA1),ISBS2(ISA1)-1
                     NIBP = ISBC(INB1) - NB0F
                     NX = (NIBP-1)/NB1E + 1
                     NY = NIBP - (NX-1)*NB1E
                     DO 3924 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-1,NACT)
 3924                CONTINUE
                     IBCON1(NB) = NY+NACT
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3926 IK=1,NB-1
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 3926                CONTINUE
C
                     ION = IBCON1(NB)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NSTF = NX
 3930             CONTINUE
C
 3970             CONTINUE
C
C   Loop double excitations from 0th space.
C
                  DO 3967 IAA=IA+1,NA-1
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
                     IF (JJ.GT.IIA) IPA=IPA-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA
C
                     DO 3965 IGEL2=IGEL,NA-1
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA-1) IENAA=NACT
                        DO 3963 JJAA=ISTAA,IENAA
C
                        IS4 = IOX(JJAA)
                        IP2 = IMUL(IS3,IS4)
                        IF (IP1.NE.IP2) GOTO 3963
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                        IPET = POSDET(NACT,NA-1,IBCON1,IFA)
                        IPET = (IPET-1)*NEXT + KJI-NACT + NA0F
                        ICI1 = ICAT
                        ICI2 = ISPA(IPET)
                        IPERT = IPER1 + IPER2
                        IPERT = (-1)**IPERT
C
C   Make matrix element.
C
                        II1 = INDEX(JJAA) + IO1
                        IF (IIA.GT.JJAA) THEN
                           I2 = INDEX(IIA) + JJAA
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIA) + JJ
                           INX2 = INDEX(II2) + II1
                        ELSEIF (IIA.LT.JJ) THEN
                           I2 = INDEX(JJAA) + IIA
                           INX = INDEX(I2) + IND
                           II2 = INDEX(JJ) + IIA
                           INX2 = INDEX(II1) + II2
                        ELSE
                           I2 = INDEX(JJAA) + IIA
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIA) + JJ
                           INX2 = INDEX(II1) + II2
                        ENDIF
                        C = SI2(INX) - SI2(INX2)
                        T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 3954 INB1 = ISBS0(ISA1),ISBS2(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                         AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                         AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 3954                   CONTINUE
C
 3963                   CONTINUE
 3965                CONTINUE
C
C  Loop over orbitals in gaps in external space, ie AA -> AE
C
                     IGEL2 = NA-1
                     DO 3959 JJAA=NACT+1,NOCC
                     IF (JJAA.EQ.IACON1(NA)) THEN
                        IGEL2=NA
                        GOTO 3959
                     ENDIF
C
                     IS4 = IOX(JJAA)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 3959
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
                     IBCON1(NA-1) = IBCON1(NA-1)-NACT
                     IBCON1(NA)   = IBCON1(NA)-NACT
C
                     IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                     IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                     IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                     ICI1 = ICAT
                     ICI2 = ISPA(IPET)
                     IPERT = IPER1 + IPER2
                     IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                     II1 = INDEX(JJAA) + IO1
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     IF (IIA.LT.JJ) THEN
                        II2 = INDEX(JJ) + IIA
                        INX2 = INDEX(II1) + II2
                     ELSE
                        II2 = INDEX(IIA) + JJ
                        INX2 = INDEX(II1) + II2
                     ENDIF
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 3944 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                     AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                     AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 3944                CONTINUE
C
 3959                CONTINUE
C
 3967             CONTINUE
C
C   Loop double excitations, 2nd excitation from external space.
C
               IAA = NA
               IIA = IACON1(IAA)
               IS3 = IOX(IIA)
               IPA = IAA
C
C Loop over orbitals in gaps in 0th space, ie AE -> AA
C
               DO 3955 IGEL2=IGEL,NA-1
                  ISTAA = IACON1(IGEL2)+1
                  IENAA = IACON1(IGEL2+1)-1
                  IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                  IF (IGEL2.EQ.NA-1) IENAA=NACT
                  DO 3957 JJAA=ISTAA,IENAA
C
                     IS4 = IOX(JJAA)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 3957
C
                  CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                     IPET = POSDET(NACT,NA,IBCON1,IFA)
                     ICI1 = ICAT
                     ICI2 = ISPA(IPET)
                     IPERT = IPER1 + IPER2
                     IPERT = (-1)**IPERT
C
                     II1 = INDEX(JJAA) + IO1
                     I2 = INDEX(IIA) + JJAA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIA) + JJ
                     INX2 = INDEX(II2) + II1
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 3934 INB1 = ISBS0(ISA1),ISBS2(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                     AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                     AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 3934                CONTINUE
C
 3957             CONTINUE
 3955          CONTINUE
C
C  Loop over orbitals in gaps in external space, ie AE -> AE
C
               IGEL2 = NA-1
               DO 3953 JJAA=NACT+1,NOCC
                  IF (JJAA.EQ.IIA) THEN
                     IGEL2 = NA
                     GOTO 3953
                  ENDIF
C
                  IS4 = IOX(JJAA)
                  IP2 = IMUL(IS3,IS4)
                  IF (IP1.NE.IP2) GOTO 3953
C
              CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                  IPET = POSDET(NACT,NA-1,IBCON1,IFA)
                  IPET = (IPET-1)*NEXT + JJAA-NACT + NA0F
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
                  IPERT = IPER1 + IPER2
                  IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                  II1 = INDEX(JJAA) + IO1
                  II2 = INDEX(IIA) + JJ
                  IF (IIA.GT.JJAA) THEN
                     I2 = INDEX(IIA) + JJAA
                     INX = INDEX(I2) + IND
                     INX2 = INDEX(II2) + II1
                  ELSE
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     INX2 = INDEX(II1) + II2
                  ENDIF
                  C = SI2(INX) - SI2(INX2)
                  T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 3951 INB1 = ISBS0(ISA1),ISBS2(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                     AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                     AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 3951                CONTINUE
C
 3953          CONTINUE
C
 3975          CONTINUE
 3980       CONTINUE
C
C  Loop over orbitals in gaps in external space, A -> E.
C
            IGEL = NA-1
            DO 3895 JJ=NACT+1,NOCC
               IF (JJ.EQ.IACON1(NA)) THEN
                  IGEL = NA
                  GOTO 3895
               ENDIF
               IS2 = IOX(JJ)
               IP1 = IMUL(IS2,IS1)
C
               CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
               IACON2(NA-1) = IACON2(NA-1)-NACT
               IACON2(NA) = IACON2(NA)-NACT
C
C  Storage here for later.
C
               IPET1 = POSDET(NACT,NA-2,IACON2,IFA)
               IPET2 = POSDET(NEXT,2,IACON2(NA-1),IFE)
               IPET = (IPET1-1)*NA2E + IPET2 + NA2S
               IMMC12(ISYMA(IPET)) = IMMC12(ISYMA(IPET)) + 1
               IPOSA12(IMMC12(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
               IPER11 = (-1)**IPER1
               IPERA12(IMMC12(ISYMA(IPET)),ISYMA(IPET)) = IPER11
               IND = INDEX(JJ)+IO1
               IIND112(IMMC12(ISYMA(IPET)),ISYMA(IPET)) = IND
C
               IACON2(NA-1) = IACON2(NA-1)+NACT
               IACON2(NA) = IACON2(NA)+NACT
C
C  Check if excited string is correct symmetry.
C
               IF (IS1.NE.IS2) GOTO 3870
               ICI1 = ICAT
               ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 3805 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3805             CONTINUE
C
                  DO 3807 IK=IA+1,IGEL
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3807             CONTINUE
C
                  DO 3808 IK=IGEL+1,NA
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3808             CONTINUE
C
C  Loop over all 0th beta dets now.
C
                  DO 3810 II=1,NB
                     IBCON1(II) = II
 3810             CONTINUE
C
                  NST = 1
                  DO 3815 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 3813 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 3813                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3818 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 3818                CONTINUE
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
 3815             CONTINUE
C
 3870          CONTINUE
C
C   Loop double excitations, AE -> EE
C
               IAA = NA
               IIA = IACON1(IAA)
               IS3 = IOX(IIA)
               IPA = IAA
               IF (JJ.GT.IIA) IPA=IPA-1
C
C Loop over orbitals in gaps in external space > JJ, AE -> EE
C
               IGEL2 = IGEL
               DO 3865 JJAA=JJ+1,NOCC
                  IF (JJAA.EQ.IIA) THEN
                     IGEL2=NA
                     GOTO 3865
                  ENDIF
C
                  IS4 = IOX(JJAA)
                  IP2 = IMUL(IS3,IS4)
                  IF (IP1.NE.IP2) GOTO 3865
C
             CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
                  IBCON1(NA-1) = IBCON1(NA-1)-NACT
                  IBCON1(NA)   = IBCON1(NA)-NACT
C
                  IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                  IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                  IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
                  IPERT = IPER1 + IPER2
                  IPERT = (-1)**IPERT
C
C Make matrix element.
C
                  II1 = INDEX(JJAA) + IO1
                  IF (IIA.GT.JJAA) THEN
                     I2 = INDEX(IIA) + JJAA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIA) + JJ
                     INX2 = INDEX(II2) + II1
                  ELSEIF (IIA.LT.JJ) THEN
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(JJ) + IIA
                     INX2 = INDEX(II1) + II2
                  ELSE
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIA) + JJ
                     INX2 = INDEX(II1) + II2
                  ENDIF
                  C = SI2(INX) - SI2(INX2)
                  T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 3851 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                     AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                     AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 3851                CONTINUE
C
 3865          CONTINUE
C
 3895       CONTINUE
C
 3985    CONTINUE
C
C  Loop single excitation from external space.
C
         IA = NA
         IO1 = IACON1(NA)
         IS1 = IOX(IO1)
         IPER11 = 1
C
C  Loop over orbitals in external space, E -> E.
C
         DO 3795 JJ=IO1+1,NOCC
            IS2 = IOX(JJ)
            IP1 = IMUL(IS2,IS1)
C
            CALL REDE01(IACON1,IACON2,NA,IA,JJ,IPER1)
C
C  Storage here for later.
C
            IPET = IPOS1 + (JJ-NACT)
            IMMC11(ISYMA(IPET)) = IMMC11(ISYMA(IPET)) + 1
            IPOSA11(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
            IPERA11(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = IPER11
            IND = INDEX(JJ)+IO1
            IIND111(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
            IF (IS1.NE.IS2) GOTO 3770
            ICI1 = ICAT
            ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 3705 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3705             CONTINUE
C
C  Loop over all 0th beta dets now.
C
                  DO 3710 II=1,NB
                     IBCON1(II) = II
 3710             CONTINUE
C
                  NST = 1
                  DO 3715 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 3713 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 3713                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3718 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 3718                CONTINUE
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
 3715             CONTINUE
C
C  Loop over 1st order beta strings.
C
                  DO 3722 II=1,NB-1
                     IBCON1(II) = II
 3722             CONTINUE
                  NSTF = 1
C
                  DO 3730 INB1 = ISBS1(ISA1),ISBS2(ISA1)-1
                     NIBP = ISBC(INB1) - NB0F
                     NX = (NIBP-1)/NB1E + 1
                     NY = NIBP - (NX-1)*NB1E
                     DO 3724 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-1,NACT)
 3724                CONTINUE
                     IBCON1(NB) = NY+NACT
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3726 IK=1,NB-1
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 3726                CONTINUE
C
                     ION = IBCON1(NB)
                     J1 = INDEX(ION+1)
                     IMA = MAX(IND,J1)
                     IMI = MIN(IND,J1)
                     JJ1 = INDEX(IMA) + IMI
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NSTF = NX
 3730             CONTINUE
C
 3770       CONTINUE
C
 3795    CONTINUE
C
C    Loop over all relevant betas and their relevant single excites now.
C
C   First loop over all 0th order betas
C
         DO 3010 II=1,NB
            IBCON1(II) = II
 3010    CONTINUE
C
         DO 3600 KJIH=1,NB0F
            IPB1 = ISPB(KJIH)
            ISB1 = ISYMB(KJIH)
            ITBS = ITAB(ISB1)
            IMZZ1 = IMMC11(ITBS)
            IMZZ2 = IMMC12(ITBS)
            IMZZ = IMZZ1 + IMZZ2
            IF (IMZZ.EQ.0.AND.ISB1.NE.ITAS) GOTO 3605
            IC1 = ICAT + IPB1
C
C   Loop single excitations from 0th space.
C
         DO 3610 IB=1,NB
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C  Loop over orbitals in gaps in 0th space, ie A -> A (B0 -> B0)
C
            DO 3615 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN=NACT
               DO 3620 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ11 = IMMC11(ITB2)
                  IMZ12 = IMMC12(ITB2)
                  IMZ1 = IMZ11 + IMZ12
C
                  IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 3625
                  IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 3625
                  GOTO 3620
C
 3625             CONTINUE
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
                  IPOSB = POSDET(NACT,NB,IBCON2,IFA)
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 3630 IAT=1,IMZ11
                        IC4 = IPOSA11(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND111(IAT,ITB2))
                        I2 = MIN(IOB,IIND111(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA11(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 3630                CONTINUE
                     DO 3640 IAT=1,IMZ12
                        IC4 = IPOSA12(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND112(IAT,ITB2))
                        I2 = MIN(IOB,IIND112(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA12(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 3640                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 3650 IAT=1,IMZZ1
                        IC3 = IPOSA11(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND111(IAT,ITBS))
                        I2 = MIN(IOB,IIND111(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA11(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 3650                CONTINUE
                     DO 3652 IAT=1,IMZZ2
                        IC3 = IPOSA12(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND112(IAT,ITBS))
                        I2 = MIN(IOB,IIND112(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA12(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 3652                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 3662 IAT=1,IMZZ1
                        IC3 = IPOSA11(IAT,ITBS) + IPB1
                        IC4 = IPOSA11(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND111(IAT,ITBS))
                        I2 = MIN(IOB,IIND111(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA11(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 3662                CONTINUE
                     DO 3666 IAT=1,IMZZ2
                        IC3 = IPOSA12(IAT,ITBS) + IPB1
                        IC4 = IPOSA12(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND112(IAT,ITBS))
                        I2 = MIN(IOB,IIND112(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA12(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 3666                CONTINUE
C
                  ENDIF
C
 3620          CONTINUE
C
 3615       CONTINUE
C
C  Loop over orbitals in external gap, ie  A -> E, (B0 -> B1)
C
            DO 3690 JJ=NACT+1,NOCC
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ11 = IMMC11(ITB2)
               IMZ12 = IMMC12(ITB2)
C
               IF (ISB1.EQ.ITAS.AND.IMZ11.NE.0) GOTO 3672
               IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 3672
               GOTO 3690
C
 3672          CONTINUE
               CALL REDE01(IBCON1,IBCON2,NB,IB,JJ,IPER1)
               IPOSB = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
               IPOSB = IPOSB + NB0F + (JJ-NACT)
               IPB2 = ISPB(IPOSB)
               IPERB = (-1)**IPER1
               IOB = INDEX(JJ)+IBB
C
               IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                  DO 3674 IAT=1,IMZ11
                     IC4 = IPOSA11(IAT,ITB2) + IPB2
                     I1 = MAX(IOB,IIND111(IAT,ITB2))
                     I2 = MIN(IOB,IIND111(IAT,ITB2))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*IPERB*IPERA11(IAT,ITB2)
                        AB(IC1) = AB(IC1) + C*CI(IC4)
                        AB(IC4) = AB(IC4) + C*CI(IC1)
 3674             CONTINUE
C
               ELSE IF(ISB1.NE.ITAS.OR.IMZ11.EQ.0) THEN
                  IC2 = ICAT + IPB2
                  DO 3680 IAT=1,IMZZ1
                     IC3 = IPOSA11(IAT,ITBS) + IPB1
                     I1 = MAX(IOB,IIND111(IAT,ITBS))
                     I2 = MIN(IOB,IIND111(IAT,ITBS))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*IPERB*IPERA11(IAT,ITBS)
                        AB(IC2) = AB(IC2) + C*CI(IC3)
                        AB(IC3) = AB(IC3) + C*CI(IC2)
 3680             CONTINUE
                  DO 3684 IAT=1,IMZZ2
                     IC3 = IPOSA12(IAT,ITBS) + IPB1
                     I1 = MAX(IOB,IIND112(IAT,ITBS))
                     I2 = MIN(IOB,IIND112(IAT,ITBS))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*IPERB*IPERA12(IAT,ITBS)
                        AB(IC2) = AB(IC2) + C*CI(IC3)
                        AB(IC3) = AB(IC3) + C*CI(IC2)
 3684             CONTINUE
C
               ELSE
                  IC2 = ICAT + IPB2
                  DO 3688 IAT=1,IMZZ1
                     IC3 = IPOSA11(IAT,ITBS) + IPB1
                     IC4 = IPOSA11(IAT,ITBS) + IPB2
                     I1 = MAX(IOB,IIND111(IAT,ITBS))
                     I2 = MIN(IOB,IIND111(IAT,ITBS))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*IPERB*IPERA11(IAT,ITBS)
                        AB(IC1) = AB(IC1) + C*CI(IC4)
                        AB(IC4) = AB(IC4) + C*CI(IC1)
                        AB(IC2) = AB(IC2) + C*CI(IC3)
                        AB(IC3) = AB(IC3) + C*CI(IC2)
 3688             CONTINUE
                  DO 3696 IAT=1,IMZZ2
                     IC3 = IPOSA12(IAT,ITBS) + IPB1
                     I1 = MAX(IOB,IIND112(IAT,ITBS))
                     I2 = MIN(IOB,IIND112(IAT,ITBS))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*IPERB*IPERA12(IAT,ITBS)
                        AB(IC2) = AB(IC2) + C*CI(IC3)
                        AB(IC3) = AB(IC3) + C*CI(IC2)
 3696             CONTINUE
C
               ENDIF
C
 3690       CONTINUE
C
 3610    CONTINUE
C
 3605       CONTINUE
C
            CALL ADVANC(IBCON1,NB,NACT)
 3600    CONTINUE
C
C  Now loop over 1st order beta strings.
C
         DO 3020 II=1,NB-1
            IBCON1(II) = II
 3020    CONTINUE
         ICOUNTB = NB0F
C
         DO 3500 KJIH=1,NB1F
            IPOSB1 = (KJIH-1)*NEXT + NB0F
            DO 3505 KJJ=NACT+1,NOCC
               IBCON1(NB) = KJJ
               ICOUNTB = ICOUNTB + 1
C
               IPB1 = ISPB(ICOUNTB)
               ISB1 = ISYMB(ICOUNTB)
               ITBS = ITAB(ISB1)
               IMZZ1 = IMMC11(ITBS)
               IF (IMZZ1.EQ.0.AND.ISB1.NE.ITAS) GOTO 3505
               IC1 = ICAT + IPB1
C
C   Loop single excitations from 0th space.
C
         DO 3510 IB=1,NB-1
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C  Loop over orbitals in gaps in 0th space, ie A -> A (B1 -> B1)
C
            DO 3515 IGEL=IB,NB-1
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-1) IEN=NACT
               DO 3520 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ11 = IMMC11(ITB2)
C
                  IF (ISB1.EQ.ITAS.AND.IMZ11.NE.0) GOTO 3523
                  IF (ISB2.EQ.ITAS.AND.IMZZ1.NE.0) GOTO 3523
                  GOTO 3520
C
 3523             CONTINUE
C
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
                  IPOSB = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
                  IPOSB = IPOSB + NB0F + (KJJ-NACT)
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ1.EQ.0) THEN
                     DO 3526 IAT=1,IMZ11
                        IC4 = IPOSA11(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND111(IAT,ITB2))
                        I2 = MIN(IOB,IIND111(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA11(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 3526                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ11.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 3530 IAT=1,IMZZ1
                        IC3 = IPOSA11(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND111(IAT,ITBS))
                        I2 = MIN(IOB,IIND111(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA11(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 3530                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 3532 IAT=1,IMZZ1
                        IC3 = IPOSA11(IAT,ITBS) + IPB1
                        IC4 = IPOSA11(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND111(IAT,ITBS))
                        I2 = MIN(IOB,IIND111(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA11(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 3532                CONTINUE
C
                  ENDIF
C
 3520          CONTINUE
 3515       CONTINUE
C
 3510    CONTINUE
C
C   Loop single excitations from External space.
C
         IBB = IBCON1(NB)
         IB1 = IOX(IBB)
         IR1 = IMUL(IB1,ISB1)
         IPERB = 1
C
C  Loop over orbitals in gaps in external space, ie E -> E, (B1 -> B1)
C
         DO 3550 JJ=IBB+1,NOCC
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ11 = IMMC11(ITB2)
C
               IF (ISB1.EQ.ITAS.AND.IMZ11.NE.0) GOTO 3555
               IF (ISB2.EQ.ITAS.AND.IMZZ1.NE.0) GOTO 3555
               GOTO 3550
C
 3555          CONTINUE
C
               IPOSB = IPOSB1 + (JJ-NACT)
               IPB2 = ISPB(IPOSB)
               IOB = INDEX(JJ)+IBB
C
               IF (ISB2.NE.ITAS.OR.IMZZ1.EQ.0) THEN
                  DO 3560 IAT=1,IMZ11
                     IC4 = IPOSA11(IAT,ITB2) + IPB2
                     I1 = MAX(IOB,IIND111(IAT,ITB2))
                     I2 = MIN(IOB,IIND111(IAT,ITB2))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*IPERB*IPERA11(IAT,ITB2)
                        AB(IC1) = AB(IC1) + C*CI(IC4)
                        AB(IC4) = AB(IC4) + C*CI(IC1)
 3560             CONTINUE
C
               ELSE IF(ISB1.NE.ITAS.OR.IMZ11.EQ.0) THEN
                  IC2 = ICAT + IPB2
                  DO 3565 IAT=1,IMZZ1
                     IC3 = IPOSA11(IAT,ITBS) + IPB1
                     I1 = MAX(IOB,IIND111(IAT,ITBS))
                     I2 = MIN(IOB,IIND111(IAT,ITBS))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*IPERB*IPERA11(IAT,ITBS)
                        AB(IC2) = AB(IC2) + C*CI(IC3)
                        AB(IC3) = AB(IC3) + C*CI(IC2)
 3565             CONTINUE
C
               ELSE
                  IC2 = ICAT + IPB2
                  DO 3573 IAT=1,IMZZ1
                     IC3 = IPOSA11(IAT,ITBS) + IPB1
                     IC4 = IPOSA11(IAT,ITBS) + IPB2
                     I1 = MAX(IOB,IIND111(IAT,ITBS))
                     I2 = MIN(IOB,IIND111(IAT,ITBS))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*IPERB*IPERA11(IAT,ITBS)
                        AB(IC1) = AB(IC1) + C*CI(IC4)
                        AB(IC4) = AB(IC4) + C*CI(IC1)
                        AB(IC2) = AB(IC2) + C*CI(IC3)
                        AB(IC3) = AB(IC3) + C*CI(IC2)
 3573             CONTINUE
C
               ENDIF
C
 3550    CONTINUE
C
 3505       CONTINUE
            IF (NB.LE.1) GOTO 3500
            CALL ADVANC(IBCON1,NB-1,NACT)
 3500    CONTINUE
C
 3990    CONTINUE
         CALL ADVANC(IACON1,NA-1,NACT)
 3993 CONTINUE
C
C    --------------------------------------------
C    End of loop over all 1st order alpha strings.
C    --------------------------------------------
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK RINSO21
C     -----------------------------------------------------------
      SUBROUTINE RINSO21(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,IOX,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBC,NSYM,
     *    IPOSA22,IPERA22,IIND122,IMMC22)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION SI1(*),SI2(*),AB(NSOCI),CI(NSOCI)
      DIMENSION IFA(0:NACT,0:NACT),IFE(0:NEXT,0:2)
      DIMENSION IACON1(NA),IBCON1(NA),IACON2(NA),IBCON2(NA)
      DIMENSION INDEX((NOCC*(NOCC+1))/2 + 1)
      DIMENSION IOX(NOCC)
      DIMENSION ITAB(NSYM),IMUL(NSYM,NSYM)
      DIMENSION ISYMA(NATT),ISYMB(NATT),ISPA(NATT),ISPB(NATT)
      DIMENSION ISBS0(NSYM+1),ISBS1(NSYM)
      DIMENSION ISBC(NATT)
      DIMENSION IPOSA22(((NA-2)*(NACT-NA+2)+2*(NEXT-2)),NSYM)
      DIMENSION IPERA22(((NA-2)*(NACT-NA+2)+2*(NEXT-2)),NSYM)
      DIMENSION IIND122(((NA-2)*(NACT-NA+2)+2*(NEXT-2)),NSYM)
      DIMENSION IMMC22(NSYM)
C
C    --------------------------------------------
C    Loop over all 2nd order alpha strings.
C    --------------------------------------------
C
      DO 4995 II=1,NA-2
         IACON1(II) = II
 4995 CONTINUE
      ICOUNT = NA2S
C
      DO 4993 IJK = 1,NA2F
         IACON1(NA-1) = 1 + NACT
         IACON1(NA) = 2 + NACT
         DO 4990 KJI=1,NA2E
            ICOUNT = ICOUNT + 1
            ICAT = ISPA(ICOUNT)
            ISA1 = ISYMA(ICOUNT)
            ITAS = ITAB(ISA1)
C
            DO 4005 II=1,NSYM
               IMMC22(II) = 0
 4005       CONTINUE
C
C   Loop single excitations from 0th space.
C
         DO 4985 IA=1,NA-2
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in zeroth order space, A -> A.
C
            DO 4980 IGEL=IA,NA-2
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA-2) IEN = NACT
               DO 4975 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
C
C  Storage here for later.
C
                  IPET1 = POSDET(NACT,NA-2,IACON2,IFA)
                  IPET2 = KJI
                  IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                  IMMC22(ISYMA(IPET)) = IMMC22(ISYMA(IPET)) + 1
                  IPOSA22(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
                  IPER11 = (-1)**IPER1
                  IPERA22(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = IPER11
                  IND = INDEX(JJ)+IO1
                  IIND122(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 4970
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 4905 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4905             CONTINUE
C
                  DO 4907 IK=IA+1,IGEL
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4907             CONTINUE
C
                  DO 4908 IK=IGEL+1,NA
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4908             CONTINUE
C
C  Loop over all 0th beta dets now.
C
                  DO 4910 II=1,NB
                     IBCON1(II) = II
 4910             CONTINUE
C
                  NST = 1
                  DO 4915 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 4913 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 4913                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 4918 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 4918                CONTINUE
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
 4915             CONTINUE
C
 4970             CONTINUE
C
C  Loop double excitations from 0th space.
C
                  DO 4967 IAA=IA+1,NA-2
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
                     IF (JJ.GT.IIA) IPA=IPA-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA.
C
                     DO 4965 IGEL2=IGEL,NA-2
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA-2) IENAA=NACT
                        DO 4963 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 4963
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                           IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                           IPET2 = KJI
                           IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = IPER1 + IPER2
                           IPERT = (-1)**IPERT
C
C   Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           IF (IIA.GT.JJAA) THEN
                              I2 = INDEX(IIA) + JJAA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIA.LT.JJ) THEN
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIA
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 4966 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                      AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                      AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 4966                   CONTINUE
C
 4963                   CONTINUE
 4965                CONTINUE
C
 4967             CONTINUE
C
C  Loop double excitations, 2nd excitation from external space.
C
                  DO 4957 IAA=NA-1,NA
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
C
C  Loop over orbitals in gaps in 0th space, ie AE -> AA
C
                     DO 4955 IGEL2=IGEL,NA-2
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA-2) IENAA=NACT
                        DO 4953 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 4953
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                           IORB = IBCON1(NA)-NACT
                           IPET = POSDET(NACT,NA-1,IBCON1,IFA)
                           IPET = (IPET-1)*NEXT + IORB + NA0F
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = IPER1 + IPER2
                           IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           I2 = INDEX(IIA) + JJAA
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIA) + JJ
                           INX2 = INDEX(II2) + II1
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 4956 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                      AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                      AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 4956                   CONTINUE
C
 4953                   CONTINUE
 4955                CONTINUE
C
C  Loop over orbitals in gaps in excited space, ie AE -> AE.
C
                     DO 4951 IGEL2=NA-2,NA
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.NA-2) ISTAA=NACT+1
                        IF (IGEL2.EQ.NA) IENAA=NOCC
                        DO 4949 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 4949
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
                           IBCON1(NA-1) = IBCON1(NA-1)-NACT
                           IBCON1(NA)   = IBCON1(NA)-NACT
C
                           IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                           IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                           IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = IPER1 + IPER2
                           IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           IF (IIA.GT.JJAA) THEN
                              I2 = INDEX(IIA) + JJAA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSE
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 4946 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                      AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                      AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 4946                   CONTINUE
C
 4949                   CONTINUE
 4951                CONTINUE
C
 4957             CONTINUE
C
 4975          CONTINUE
 4980       CONTINUE
C
 4985    CONTINUE
C
C  Loop single excitations from excited space.
C
         DO 4785 IA=NA-1,NA
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in excited space, E -> E.
C
            DO 4780 IGEL=IA,NA
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA) IEN=NOCC
               DO 4775 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
                  IACON2(NA-1) = IACON2(NA-1)-NACT
                  IACON2(NA) = IACON2(NA)-NACT
C
C  Storage here for later.
C
                  IPET1 = IJK
                  IPET2 = POSDET(NEXT,2,IACON2(NA-1),IFE)
                  IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                  IMMC22(ISYMA(IPET)) = IMMC22(ISYMA(IPET)) + 1
                  IPOSA22(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
                  IPER11 = (-1)**IPER1
                  IPERA22(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = IPER11
                  IND = INDEX(JJ)+IO1
                  IIND122(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = IND
C
                  IACON2(NA-1) = IACON2(NA-1)+NACT
                  IACON2(NA) = IACON2(NA)+NACT
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 4770
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 4705 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4705             CONTINUE
C
                  DO 4707 IK=IA+1,IGEL
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4707             CONTINUE
C
                  DO 4708 IK=IGEL+1,NA
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4708             CONTINUE
C
C  Loop over all 0th beta dets now.
C
                  DO 4710 II=1,NB
                     IBCON1(II) = II
 4710             CONTINUE
C
                  NST = 1
                  DO 4715 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 4713 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 4713                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 4718 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 4718                CONTINUE
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
 4715             CONTINUE
C
 4770             CONTINUE
C
C   Loop double excitations from excited space, ie EE -> EE.
C
                  DO 4767 IAA=IA+1,NA
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
                     IF (JJ.GT.IIA) IPA=IPA-1
C
                     DO 4765 IGEL2=IGEL,NA
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA) IENAA=NOCC
                        DO 4763 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 4763
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
                           IBCON1(NA-1) = IBCON1(NA-1)-NACT
                           IBCON1(NA)   = IBCON1(NA)-NACT
C
                           IPET1 = IJK
                           IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                           IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = IPER1 + IPER2
                           IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           IF (IIA.GT.JJAA) THEN
                              I2 = INDEX(IIA) + JJAA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIA.LT.JJ) THEN
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIA
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 4746 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                      AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                      AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 4746                   CONTINUE
C
 4763                   CONTINUE
 4765                CONTINUE
C
 4767             CONTINUE
C
 4775          CONTINUE
 4780       CONTINUE
C
 4785    CONTINUE
C
C    Loop over all relevant betas and their relevant single excites now.
C
C   Loop over all 0th order betas
C
         DO 5010 II=1,NB
            IBCON1(II) = II
 5010    CONTINUE
C
         DO 5600 KJIH=1,NB0F
            IPB1 = ISPB(KJIH)
            ISB1 = ISYMB(KJIH)
            ITBS = ITAB(ISB1)
            IMZZ = IMMC22(ITBS)
            IF (IMZZ.EQ.0.AND.ISB1.NE.ITAS) GOTO 5605
            IC1 = ICAT + IPB1
C
C   Loop single excitations from 0th space.
C
         DO 5610 IB=1,NB
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C  Loop over orbitals in gaps in 0th space, ie A -> A (B0 -> B0)
C
            DO 5615 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN=NACT
               DO 5620 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ2 = IMMC22(ITB2)
C
                  IF (ISB1.EQ.ITAS.AND.IMZ2.NE.0) GOTO 5625
                  IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 5625
                  GOTO 5620
C
 5625             CONTINUE
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
                  IPOSB = POSDET(NACT,NB,IBCON2,IFA)
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 5630 IAT=1,IMZ2
                        IC4 = IPOSA22(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND122(IAT,ITB2))
                        I2 = MIN(IOB,IIND122(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA22(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 5630                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ2.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 5650 IAT=1,IMZZ
                        IC3 = IPOSA22(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND122(IAT,ITBS))
                        I2 = MIN(IOB,IIND122(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA22(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 5650                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 5662 IAT=1,IMZZ
                        IC3 = IPOSA22(IAT,ITBS) + IPB1
                        IC4 = IPOSA22(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND122(IAT,ITBS))
                        I2 = MIN(IOB,IIND122(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA22(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 5662                CONTINUE
C
                  ENDIF
C
 5620          CONTINUE
 5615       CONTINUE
C
 5610    CONTINUE
C
 5605       CONTINUE
C
            CALL ADVANC(IBCON1,NB,NACT)
 5600    CONTINUE
C
            CALL ADVANC(IACON1(NA-1),2,NOCC)
 4990    CONTINUE
         CALL ADVANC(IACON1,NA-2,NACT)
 4993 CONTINUE
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK RINSOB1
C     -----------------------------------------------------------
      SUBROUTINE RINSOB1(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IBCON2,IFA,IFE,INDEX,AB,Q,IOX,
     *    IMUL,ISYMB,ISPA,ISPB,ISAS0,ISAS1,ISAS2,ISAC,NSYM)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION SI1(*),SI2(*),AB(NSOCI),CI(NSOCI)
      DIMENSION IFA(0:NACT,0:NACT),IFE(0:NEXT,0:2)
      DIMENSION IACON1(NA),IBCON1(NA),IBCON2(NA)
      DIMENSION INDEX((NOCC*(NOCC+1))/2 + 1)
      DIMENSION Q(NSOCI)
      DIMENSION IOX(NOCC)
      DIMENSION IMUL(NSYM,NSYM)
      DIMENSION ISYMB(NATT),ISPA(NATT),ISPB(NATT)
      DIMENSION ISAS0(NSYM+1),ISAS1(NSYM),ISAS2(NSYM)
      DIMENSION ISAC(NATT)
C
C    --------------------------------------------
C    Loop over all 0th order beta strings.
C    --------------------------------------------
C
      DO 6020 II=1,NB
         IBCON1(II)=II
 6020 CONTINUE
C
      DO 6000 IJK=1,NB0F
         ICAB = ISPB(IJK)
         ISB1 = ISYMB(IJK)
C
C   Loop single excitations from 0th space.
C
         DO 6990 IB=1,NB
            IO1 = IBCON1(IB)
            IS1 = IOX(IO1)
C
C   Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 6980 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN=NACT
               DO 6970 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
C
                  IPER = ((-1)**IPER1)
                  IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 6900
                  IPET = POSDET(NACT,NB,IBCON2,IFA)
                  IPB1 = ISPB(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 6905 IK=1,IB-1
                     ION = IBCON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 6905             CONTINUE
C
                  DO 6907 IK=IB+1,IGEL
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 6907             CONTINUE
C
                  DO 6908 IK=IGEL+1,NB
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 6908             CONTINUE
C
C  Loop over compatible alpha strings. 0th order first.
C
                  DO 6910 II=1,NA
                     IACON1(II) = II
 6910             CONTINUE
C
                  NST = 1
                  DO 6915 INA1=ISAS0(ISB1),ISAS1(ISB1)-1
                     NEND = ISAC(INA1)
                     DO 6913 KK=NST,NEND-1
                        CALL ADVANC(IACON1,NA,NACT)
 6913                CONTINUE
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 6918 IK=1,NA
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 6918                CONTINUE
C
                     T = (C+D)*IPER
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
 6915             CONTINUE
C
C  Loop over 1st order alpha strings.
C
                  DO 6922 II=1,NA-1
                     IACON1(II) = II
 6922             CONTINUE
                  NSTF = 1
C
                  DO 6930 INA1 = ISAS1(ISB1),ISAS2(ISB1)-1
                     NIAP = ISAC(INA1) - NA0F
                     NX = (NIAP-1)/NA1E + 1
                     NY = NIAP - (NX-1)*NA1E
                     DO 6924 KK=NSTF,NX-1
                        CALL ADVANC(IACON1,NA-1,NACT)
 6924                CONTINUE
                     IACON1(NA) = NY+NACT
C
                     ICIA = ISPA(NIAP+NA0F)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 6926 IK=1,NA-1
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 6926                CONTINUE
C
                     ION = IACON1(NA)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NSTF = NX
 6930             CONTINUE
C
C  Loop over 2nd order alpha strings.
C
                  DO 6932 II=1,NA-2
                     IACON1(II) = II
 6932             CONTINUE
                  IACON1(NA-1) = 1 + NACT
                  IACON1(NA) = 2 + NACT
                  NSTF = 1
                  NSTE = 1
C
                  DO 6940 INA1 = ISAS2(ISB1),ISAS0(ISB1+1)-1
                     NIAP = ISAC(INA1) - NA2S
                     NX = (NIAP-1)/NA2E + 1
                     NY = NIAP - (NX-1)*NA2E
                     DO 6934 KK=NSTF,NX-1
                        CALL ADVANC(IACON1,NA-2,NACT)
 6934                CONTINUE
                     IF (NY.LT.NSTE) THEN
                        IACON1(NA-1) = 1 + NACT
                        IACON1(NA)   = 2 + NACT
                        NSTE = 1
                     ENDIF
                     DO 6936 KK=NSTE,NY-1
                        CALL ADVANC(IACON1(NA-1),2,NOCC)
 6936                CONTINUE
C
                     ICIA = ISPA(NIAP+NA2S)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 6937 IK=1,NA-2
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 6937                CONTINUE
C
                     DO 6938 IK=NA-1,NA
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(J1) + IND
                        D = D + SI2(JJ1)
 6938                CONTINUE
C
                     T = (C+D)*IPER
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NSTF = NX
                     NSTE = NY
C
 6940             CONTINUE
C
 6900             CONTINUE
C
C   Loop double excitations from 0th space.
C
                  DO 6967 IBB=IB+1,NB
                     IIB = IBCON1(IBB)
                     IS3 = IOX(IIB)
                     IPB = IBB
                     IF (JJ.GT.IIB) IPB=IPB-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA
C
                     DO 6965 IGEL2=IGEL,NB
                        ISTBB = IBCON1(IGEL2)+1
                        IENBB = IBCON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTBB=JJ+1
                        IF (IGEL2.EQ.NB) IENBB=NACT
                        DO 6963 JJBB=ISTBB,IENBB
C
                           IS4 = IOX(JJBB)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 6963
C
                     CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
C
                           IPET = POSDET(NACT,NB,IACON1,IFA)
                           IBP2 = ISPB(IPET)
                           IPER = IPER1 + IPER2
                           IPER = (-1)**IPER
C
C   Make matrix element.
C
                           II1 = INDEX(JJBB) + IO1
                           IF (IIB.GT.JJBB) THEN
                              I2 = INDEX(IIB) + JJBB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIB) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIB.LT.JJ) THEN
                              I2 = INDEX(JJBB) + IIB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIB
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJBB) + IIB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIB) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                           DO 6961 INA1 = ISAS0(ISB1),ISAS0(ISB1+1)-1
                              NEND = ISAC(INA1)
                              ICIA = ISPA(NEND)
                              ICI1 = ICIA + ICAB
                              ICI2 = ICIA + IBP2
                            AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                            AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 6961                      CONTINUE
C
 6963                   CONTINUE
 6965                CONTINUE
C
C  Loop over orbitals in external space, ie AA -> AE
C
                     DO 6950 JJBB=NACT+1,NOCC
C
                        IS4 = IOX(JJBB)
                        IP2 = IMUL(IS3,IS4)
                        IF (IP1.NE.IP2) GOTO 6950
C
                        CALL REDE01(IBCON2,IACON1,NB,IPB,JJBB,IPER2)
C
                        IPET = POSDET(NACT,NB-1,IACON1,IFA)
                        IPET = (IPET-1)*NEXT + JJBB-NACT + NB0F
                        IBP2 = ISPB(IPET)
                        IPER = IPER1 + IPER2
                        IPER = (-1)**IPER
C
C  Make matrix element.
C
                        II1 = INDEX(JJBB) + IO1
                        I2 = INDEX(JJBB) + IIB
                        INX = INDEX(I2) + IND
                        IF (IIB.LT.JJ) THEN
                           II2 = INDEX(JJ) + IIB
                           INX2 = INDEX(II1) + II2
                        ELSE
                           II2 = INDEX(IIB) + JJ
                           INX2 = INDEX(II1) + II2
                        ENDIF
                        C = SI2(INX) - SI2(INX2)
                        T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                        DO 6954 INA1 = ISAS0(ISB1),ISAS2(ISB1)-1
                           NEND = ISAC(INA1)
                           ICIA = ISPA(NEND)
                           ICI1 = ICIA + ICAB
                           ICI2 = ICIA + IBP2
                         AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                         AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 6954                   CONTINUE
C
 6950                CONTINUE
C
 6967             CONTINUE
C
 6970          CONTINUE
 6980       CONTINUE
C
C  Loop over orbitals in external gap, ie  A -> E.
C
            DO 6880 JJ=NACT+1,NOCC
C
               IS2 = IOX(JJ)
               IP1 = IMUL(IS2,IS1)
C
               CALL REDE01(IBCON1,IBCON2,NB,IB,JJ,IPER1)
C
               IPER = ((-1)**IPER1)
               IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry.
C
               IF (IS1.NE.IS2) GOTO 6800
               IPET = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
               IPET = IPET + NB0F + (JJ-NACT)
               IPB1 = ISPB(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 6805 IK=1,IB-1
                     ION = IBCON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 6805             CONTINUE
C
                  DO 6807 IK=IB+1,NB
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 6807             CONTINUE
C
C  Loop over all compatible alpha strings now. 0th order first.
C
                  DO 6810 II=1,NA
                     IACON1(II) = II
 6810             CONTINUE
C
                  NST = 1
                  DO 6815 INA1=ISAS0(ISB1),ISAS1(ISB1)-1
                     NEND = ISAC(INA1)
                     DO 6813 KK=NST,NEND-1
                        CALL ADVANC(IACON1,NA,NACT)
 6813                CONTINUE
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 6818 IK=1,NA
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 6818                CONTINUE
C
                     T = (C+D)*IPER
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
 6815             CONTINUE
C
C  Loop over 1st order alpha strings.
C
                  DO 6822 II=1,NA-1
                     IACON1(II) = II
 6822             CONTINUE
                  NSTF = 1
C
                  DO 6830 INA1 = ISAS1(ISB1),ISAS2(ISB1)-1
                     NIAP = ISAC(INA1) - NA0F
                     NX = (NIAP-1)/NA1E + 1
                     NY = NIAP - (NX-1)*NA1E
                     DO 6824 KK=NSTF,NX-1
                        CALL ADVANC(IACON1,NA-1,NACT)
 6824                CONTINUE
                     IACON1(NA) = NY+NACT
C
                     ICIA = ISPA(NIAP+NA0F)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 6826 IK=1,NA-1
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 6826                CONTINUE
C
                     ION = IACON1(NA)
                     J1 = INDEX(ION+1)
                     IMA = MAX(IND,J1)
                     IMI = MIN(IND,J1)
                     JJ1 = INDEX(IMA) + IMI
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NSTF = NX
 6830             CONTINUE
C
 6800          CONTINUE
C
C   Loop double excitations from 0th space.
C
               DO 6867 IBB=IB+1,NB
                  IIB = IBCON1(IBB)
                  IS3 = IOX(IIB)
                  IPB = IBB - 1
C
C  Loop over external orbitals > JJ, ie AA -> EE
C
                  DO 6865 JJBB=JJ+1,NOCC
C
                     IS4 = IOX(JJBB)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 6865
C
                     CALL REDE01(IBCON2,IACON1,NB,IPB,JJBB,IPER2)
                     IACON1(NB-1) = JJ-NACT
                     IACON1(NB)   = JJBB-NACT
C
                     IPET1 = POSDET(NACT,NB-2,IACON1,IFA)
                     IPET2 = POSDET(NEXT,2,IACON1(NB-1),IFE)
                     IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                     IBP2 = ISPB(IPET)
                     IPER = IPER1 + IPER2
                     IPER = (-1)**IPER
C
C  Make matrix element.
C
                     II1 = INDEX(JJBB) + IO1
                     I2 = INDEX(JJBB) + IIB
                     INX = INDEX(I2) + IND
                     II2 = INDEX(JJ) + IIB
                     INX2 = INDEX(II1) + II2
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPER
C
C
C  Loop over alpha strings of the right symmetry.
C
                     DO 6863 INA1 = ISAS0(ISB1),ISAS1(ISB1)-1
                        NEND = ISAC(INA1)
                        ICIA = ISPA(NEND)
                        ICI1 = ICIA + ICAB
                        ICI2 = ICIA + IBP2
                      AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                      AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 6863                CONTINUE
C
 6865             CONTINUE
C
 6867          CONTINUE
C
 6880       CONTINUE
C
 6990    CONTINUE
C
         CALL ADVANC(IBCON1,NB,NACT)
 6000 CONTINUE
C
C    --------------------------------------------
C    Loop over all 1st order beta strings.
C    --------------------------------------------
C
      DO 7995 II=1,NB-1
         IBCON1(II)=II
 7995 CONTINUE
      ICOUNT = NB0F
C
      DO 7993 IJK=1,NB1F
         IPOS1 = (IJK-1)*NEXT + NB0F
         DO 7990 KJI=NACT+1,NOCC
            IBCON1(NB) = KJI
            ICOUNT = ICOUNT + 1
            ICAB = ISPB(ICOUNT)
            ISB1 = ISYMB(ICOUNT)
C
C   Loop single excitations from 0th space.
C
         DO 7985 IB=1,NB-1
            IO1 = IBCON1(IB)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 7980 IGEL=IB,NB-1
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-1) IEN = NACT
               DO 7975 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
C
                  IPER = ((-1)**IPER1)
                  IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 7970
                  IPET = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
                  IPET = IPET + NB0F + (KJI-NACT)
                  IPB1 = ISPB(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 7905 IK=1,IB-1
                     ION = IBCON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 7905             CONTINUE
C
                  DO 7907 IK=IB+1,IGEL
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 7907             CONTINUE
C
                  DO 7908 IK=IGEL+1,NB
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 7908             CONTINUE
C
C  Loop over compatible alpha strings. 0th order first.
C
                  DO 7910 II=1,NA
                     IACON1(II) = II
 7910             CONTINUE
C
                  NST = 1
                  DO 7915 INA1=ISAS0(ISB1),ISAS1(ISB1)-1
                     NEND = ISAC(INA1)
                     DO 7913 KK=NST,NEND-1
                        CALL ADVANC(IACON1,NA,NACT)
 7913                CONTINUE
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 7918 IK=1,NA
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 7918                CONTINUE
C
                     T = (C+D)*IPER
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
 7915             CONTINUE
C
C  Loop over 1st order alpha strings.
C
                  DO 7922 II=1,NA-1
                     IACON1(II) = II
 7922             CONTINUE
                  NSTF = 1
C
                  DO 7930 INA1 = ISAS1(ISB1),ISAS2(ISB1)-1
                     NIAP = ISAC(INA1) - NA0F
                     NX = (NIAP-1)/NA1E + 1
                     NY = NIAP - (NX-1)*NA1E
                     DO 7924 KK=NSTF,NX-1
                        CALL ADVANC(IACON1,NA-1,NACT)
 7924                CONTINUE
                     IACON1(NA) = NY+NACT
C
                     ICIA = ISPA(NIAP+NA0F)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 7926 IK=1,NA-1
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 7926                CONTINUE
C
                     ION = IACON1(NA)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NSTF = NX
 7930             CONTINUE
C
 7970             CONTINUE
C
C   Loop double excitations from 0th space.
C
                  DO 7967 IBB=IB+1,NB-1
                     IIB = IBCON1(IBB)
                     IS3 = IOX(IIB)
                     IPB = IBB
                     IF (JJ.GT.IIB) IPB=IPB-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA
C
                     DO 7965 IGEL2=IGEL,NB-1
                        ISTBB = IBCON1(IGEL2)+1
                        IENBB = IBCON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTBB=JJ+1
                        IF (IGEL2.EQ.NB-1) IENBB=NACT
                        DO 7963 JJBB=ISTBB,IENBB
C
                        IS4 = IOX(JJBB)
                        IP2 = IMUL(IS3,IS4)
                        IF (IP1.NE.IP2) GOTO 7963
C
                     CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
C
                        IPET = POSDET(NACT,NB-1,IACON1,IFA)
                        IPET = (IPET-1)*NEXT + KJI-NACT + NB0F
                        IBP2 = ISPB(IPET)
                        IPER = IPER1 + IPER2
                        IPER = (-1)**IPER
C
C   Make matrix element.
C
                        II1 = INDEX(JJBB) + IO1
                        IF (IIB.GT.JJBB) THEN
                           I2 = INDEX(IIB) + JJBB
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIB) + JJ
                           INX2 = INDEX(II2) + II1
                        ELSEIF (IIB.LT.JJ) THEN
                           I2 = INDEX(JJBB) + IIB
                           INX = INDEX(I2) + IND
                           II2 = INDEX(JJ) + IIB
                           INX2 = INDEX(II1) + II2
                        ELSE
                           I2 = INDEX(JJBB) + IIB
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIB) + JJ
                           INX2 = INDEX(II1) + II2
                        ENDIF
                        C = SI2(INX) - SI2(INX2)
                        T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                        DO 7954 INA1 = ISAS0(ISB1),ISAS2(ISB1)-1
                           NEND = ISAC(INA1)
                           ICIA = ISPA(NEND)
                           ICI1 = ICIA + ICAB
                           ICI2 = ICIA + IBP2
                         AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                         AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 7954                   CONTINUE
C
 7963                   CONTINUE
 7965                CONTINUE
C
C  Loop over orbitals in gaps in external space, ie AA -> AE
C
                     IGEL2 = NB-1
                     DO 7959 JJBB=NACT+1,NOCC
                     IF (JJBB.EQ.IBCON1(NB)) THEN
                        IGEL2=NB
                        GOTO 7959
                     ENDIF
C
                     IS4 = IOX(JJBB)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 7959
C
                     CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
                     IACON1(NB-1) = IACON1(NB-1)-NACT
                     IACON1(NB)   = IACON1(NB)-NACT
C
                     IPET1 = POSDET(NACT,NB-2,IACON1,IFA)
                     IPET2 = POSDET(NEXT,2,IACON1(NB-1),IFE)
                     IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                     IBP2 = ISPB(IPET)
                     IPER = IPER1 + IPER2
                     IPER = (-1)**IPER
C
C  Make matrix element.
C
                     II1 = INDEX(JJBB) + IO1
                     I2 = INDEX(JJBB) + IIB
                     INX = INDEX(I2) + IND
                     IF (IIB.LT.JJ) THEN
                        II2 = INDEX(JJ) + IIB
                        INX2 = INDEX(II1) + II2
                     ELSE
                        II2 = INDEX(IIB) + JJ
                        INX2 = INDEX(II1) + II2
                     ENDIF
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                     DO 7944 INA1 = ISAS0(ISB1),ISAS1(ISB1)-1
                        NEND = ISAC(INA1)
                        ICIA = ISPA(NEND)
                        ICI1 = ICIA + ICAB
                        ICI2 = ICIA + IBP2
                      AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                      AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 7944                CONTINUE
C
 7959                CONTINUE
C
 7967             CONTINUE
C
C   Loop double excitations, 2nd excitation from external space.
C
               IBB = NB
               IIB = IBCON1(IBB)
               IS3 = IOX(IIB)
               IPB = IBB
C
C Loop over orbitals in gaps in 0th space, ie AE -> AA
C
               DO 7955 IGEL2=IGEL,NB-1
                  ISTBB = IBCON1(IGEL2)+1
                  IENBB = IBCON1(IGEL2+1)-1
                  IF (IGEL2.EQ.IGEL) ISTBB=JJ+1
                  IF (IGEL2.EQ.NB-1) IENBB=NACT
                  DO 7957 JJBB=ISTBB,IENBB
C
                     IS4 = IOX(JJBB)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 7957
C
                  CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
C
                     IPET = POSDET(NACT,NB,IACON1,IFA)
                     IBP2 = ISPB(IPET)
                     IPER = IPER1 + IPER2
                     IPER = (-1)**IPER
C
                     II1 = INDEX(JJBB) + IO1
                     I2 = INDEX(IIB) + JJBB
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIB) + JJ
                     INX2 = INDEX(II2) + II1
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                     DO 7934 INA1 = ISAS0(ISB1),ISAS2(ISB1)-1
                        NEND = ISAC(INA1)
                        ICIA = ISPA(NEND)
                        ICI1 = ICIA + ICAB
                        ICI2 = ICIA + IBP2
                      AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                      AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 7934                CONTINUE
C
 7957             CONTINUE
 7955          CONTINUE
C
C  Loop over orbitals in gaps in external space, ie AE -> AE
C
               IGEL2 = NB-1
               DO 7953 JJBB=NACT+1,NOCC
                  IF (JJBB.EQ.IIB) THEN
                     IGEL2 = NB
                     GOTO 7953
                  ENDIF
C
                  IS4 = IOX(JJBB)
                  IP2 = IMUL(IS3,IS4)
                  IF (IP1.NE.IP2) GOTO 7953
C
              CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
C
                  IPET = POSDET(NACT,NB-1,IACON1,IFA)
                  IPET = (IPET-1)*NEXT + JJBB-NACT + NB0F
                  IBP2 = ISPB(IPET)
                  IPER = IPER1 + IPER2
                  IPER = (-1)**IPER
C
C  Make matrix element.
C
                  II1 = INDEX(JJBB) + IO1
                  II2 = INDEX(IIB) + JJ
                  IF (IIB.GT.JJBB) THEN
                     I2 = INDEX(IIB) + JJBB
                     INX = INDEX(I2) + IND
                     INX2 = INDEX(II2) + II1
                  ELSE
                     I2 = INDEX(JJBB) + IIB
                     INX = INDEX(I2) + IND
                     INX2 = INDEX(II1) + II2
                  ENDIF
                  C = SI2(INX) - SI2(INX2)
                  T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                  DO 7951 INA1 = ISAS0(ISB1),ISAS2(ISB1)-1
                     NEND = ISAC(INA1)
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IBP2
                   AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                   AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 7951             CONTINUE
C
 7953          CONTINUE
C
 7975          CONTINUE
 7980       CONTINUE
C
C  Loop over orbitals in gaps in external space, A -> E.
C
            IGEL = NB-1
            DO 7895 JJ=NACT+1,NOCC
               IF (JJ.EQ.IBCON1(NB)) THEN
                  IGEL = NB
                  GOTO 7895
               ENDIF
               IS2 = IOX(JJ)
               IP1 = IMUL(IS2,IS1)
C
               CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
C
               IPER = ((-1)**IPER1)
               IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry
C
               IF (IS1.NE.IS2) GOTO 7870
C
               IBCON2(NB-1) = IBCON2(NB-1)-NACT
               IBCON2(NB) = IBCON2(NB)-NACT
C
               IPET1 = POSDET(NACT,NB-2,IBCON2,IFA)
               IPET2 = POSDET(NEXT,2,IBCON2(NB-1),IFE)
               IPET = (IPET1-1)*NB2E + IPET2 + NB2S
               IPB1 = ISPB(IPET)
C
               IBCON2(NB-1) = IBCON2(NB-1)+NACT
               IBCON2(NB) = IBCON2(NB)+NACT
C
C  Make singly excited matrix elements.
C
               C = SI1(IND)
C
               DO 7805 IK=1,IB-1
                  ION = IBCON1(IK)
                  J1  = INDEX(ION+1)
                  JJ1 = INDEX(IND) + J1
                  J1  = INDEX(JJ) + ION
                  J2  = INDEX(IO1) + ION
                  INX = INDEX(J1) + J2
                  C = C + SI2(JJ1) - SI2(INX)
 7805          CONTINUE
C
               DO 7807 IK=IB+1,IGEL
                  ION = IBCON1(IK)
                  J1 = INDEX(ION+1)
                  JJ1 = INDEX(IND) + J1
                  J1 = INDEX(JJ) + ION
                  J2 = INDEX(ION) + IO1
                  INX = INDEX(J1) + J2
                  C = C + SI2(JJ1) - SI2(INX)
 7807          CONTINUE
C
               DO 7808 IK=IGEL+1,NB
                  ION = IBCON1(IK)
                  J1 = INDEX(ION+1)
                  JJ1 = INDEX(J1) + IND
                  J1 = INDEX(ION) + JJ
                  J2 = INDEX(ION) + IO1
                  INX = INDEX(J1) + J2
                  C = C + SI2(JJ1) - SI2(INX)
 7808          CONTINUE
C
C  Loop over all 0th alpha dets now.
C
                  DO 7810 II=1,NA
                     IACON1(II) = II
 7810             CONTINUE
C
                  NST = 1
                  DO 7815 INA1=ISAS0(ISB1),ISAS1(ISB1)-1
                     NEND = ISAC(INA1)
                     DO 7813 KK=NST,NEND-1
                        CALL ADVANC(IACON1,NA,NACT)
 7813                CONTINUE
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 7818 IK=1,NA
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 7818                CONTINUE
C
                     T = (C+D)*IPER
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
 7815             CONTINUE
C
 7870          CONTINUE
C
C   Loop double excitations, AE -> EE
C
               IBB = NB
               IIB = IBCON1(IBB)
               IS3 = IOX(IIB)
               IPB = IBB
               IF (JJ.GT.IIB) IPB=IPB-1
C
C Loop over orbitals in gaps in external space > JJ, AE -> EE
C
               IGEL2 = IGEL
               DO 7865 JJBB=JJ+1,NOCC
                  IF (JJBB.EQ.IIB) THEN
                     IGEL2=NB
                     GOTO 7865
                  ENDIF
C
                  IS4 = IOX(JJBB)
                  IP2 = IMUL(IS3,IS4)
                  IF (IP1.NE.IP2) GOTO 7865
C
              CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
                  IACON1(NB-1) = IACON1(NB-1)-NACT
                  IACON1(NB)   = IACON1(NB)-NACT
C
                  IPET1 = POSDET(NACT,NB-2,IACON1,IFA)
                  IPET2 = POSDET(NEXT,2,IACON1(NB-1),IFE)
                  IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                  IBP2 = ISPB(IPET)
                  IPER = IPER1 + IPER2
                  IPER = (-1)**IPER
C
C Make matrix element.
C
                  II1 = INDEX(JJBB) + IO1
                  IF (IIB.GT.JJBB) THEN
                     I2 = INDEX(IIB) + JJBB
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIB) + JJ
                     INX2 = INDEX(II2) + II1
                  ELSEIF (IIB.LT.JJ) THEN
                     I2 = INDEX(JJBB) + IIB
                     INX = INDEX(I2) + IND
                     II2 = INDEX(JJ) + IIB
                     INX2 = INDEX(II1) + II2
                  ELSE
                     I2 = INDEX(JJBB) + IIB
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIB) + JJ
                     INX2 = INDEX(II1) + II2
                  ENDIF
                  C = SI2(INX) - SI2(INX2)
                  T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                  DO 7851 INA1 = ISAS0(ISB1),ISAS1(ISB1)-1
                     NEND = ISAC(INA1)
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IBP2
                   AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                   AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 7851             CONTINUE
C
C
 7865         CONTINUE
C
 7895       CONTINUE
C
 7985    CONTINUE
C
C  Loop single excitation from external space.
C
         IB = NB
         IO1 = IBCON1(NB)
         IS1 = IOX(IO1)
         IPER1 = 1
C
C  Loop over orbitals in external space, E -> E.
C
         DO 7795 JJ=IO1+1,NOCC
            IS2 = IOX(JJ)
            IP1 = IMUL(IS2,IS1)
C
            IF (IS1.NE.IS2) GOTO 7795
C
            CALL REDE01(IBCON1,IBCON2,NB,IB,JJ,IPER1)
            IPET = IPOS1 + (JJ-NACT)
            IPB1 = ISPB(IPET)
            IPER = ((-1)**IPER1)
            IND = INDEX(JJ) + IO1
C
C  Make singly excited matrix elements.
C
            C = SI1(IND)
C
            DO 7705 IK=1,IB-1
               ION = IBCON1(IK)
               J1  = INDEX(ION+1)
               JJ1 = INDEX(IND) + J1
               J1  = INDEX(JJ) + ION
               J2  = INDEX(IO1) + ION
               INX = INDEX(J1) + J2
               C = C + SI2(JJ1) - SI2(INX)
 7705       CONTINUE
C
C  Loop over all 0th alpha dets now.
C
            DO 7710 II=1,NA
               IACON1(II) = II
 7710       CONTINUE
C
            NST = 1
            DO 7715 INA1=ISAS0(ISB1),ISAS1(ISB1)-1
               NEND = ISAC(INA1)
               DO 7713 KK=NST,NEND-1
                  CALL ADVANC(IACON1,NA,NACT)
 7713          CONTINUE
C
               ICIA = ISPA(NEND)
               ICI1 = ICIA + ICAB
               ICI2 = ICIA + IPB1
C
               D = 0.0D+00
               DO 7718 IK=1,NA
                  ION = IACON1(IK)
                  J1 = INDEX(ION+1)
                  JJ1 = INDEX(IND) + J1
                  D = D + SI2(JJ1)
 7718          CONTINUE
C
               T = (C+D)*IPER
                  AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                  AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
               NST = NEND
 7715       CONTINUE
C
C  Loop over 1st order alpha strings.
C
            DO 7722 II=1,NA-1
               IACON1(II) = II
 7722       CONTINUE
            NSTF = 1
C
            DO 7730 INA1 = ISAS1(ISB1),ISAS2(ISB1)-1
               NIAP = ISAC(INA1) - NA0F
               NX = (NIAP-1)/NA1E + 1
               NY = NIAP - (NX-1)*NA1E
               DO 7724 KK=NSTF,NX-1
                  CALL ADVANC(IACON1,NA-1,NACT)
 7724          CONTINUE
               IACON1(NA) = NY+NACT
C
               ICIA = ISPA(NIAP+NA0F)
               ICI1 = ICIA + ICAB
               ICI2 = ICIA + IPB1
C
               D = 0.0D+00
               DO 7726 IK=1,NA-1
                  ION = IACON1(IK)
                  J1 = INDEX(ION+1)
                  JJ1 = INDEX(IND) + J1
                  D = D + SI2(JJ1)
 7726          CONTINUE
C
               ION = IACON1(NA)
               J1 = INDEX(ION+1)
               IMA = MAX(IND,J1)
               IMI = MIN(IND,J1)
               JJ1 = INDEX(IMA) + IMI
               D = D + SI2(JJ1)
C
               T = (C+D)*IPER
                  AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                  AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
               NSTF = NX
 7730       CONTINUE
C
 7795    CONTINUE
C
 7990    CONTINUE
         IF (NB.LE.1) GOTO 7993
         CALL ADVANC(IBCON1,NB-1,NACT)
 7993 CONTINUE
C
C    --------------------------------------------
C    Loop over all 2nd order beta strings.
C    --------------------------------------------
C
      IF (NB.LT.2) GOTO 8997
      DO 8995 II=1,NB-2
         IBCON1(II) = II
 8995 CONTINUE
      ICOUNT = NB2S
C
      DO 8993 IJK = 1,NB2F
         IBCON1(NB-1) = 1 + NACT
         IBCON1(NB) = 2 + NACT
         DO 8990 KJI=1,NB2E
            ICOUNT = ICOUNT + 1
            ICAB = ISPB(ICOUNT)
            ISB1 = ISYMB(ICOUNT)
C
C   Loop single excitations from 0th space.
C
         DO 8985 IB=1,NB-2
            IO1 = IBCON1(IB)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in zeroth order space, A -> A.
C
            DO 8980 IGEL=IB,NB-2
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-2) IEN = NACT
               DO 8975 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
C
                  IPER = ((-1)**IPER1)
                  IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 8970
C
                  IPET1 = POSDET(NACT,NB-2,IBCON2,IFA)
                  IPET2 = KJI
                  IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                  IPB1 = ISPB(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 8905 IK=1,IB-1
                     ION = IBCON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 8905             CONTINUE
C
                  DO 8907 IK=IB+1,IGEL
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 8907             CONTINUE
C
                  DO 8908 IK=IGEL+1,NB
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 8908             CONTINUE
C
C  Loop over all 0th alpha dets now.
C
                  DO 8910 II=1,NA
                     IACON1(II) = II
 8910             CONTINUE
C
                  NST = 1
                  DO 8915 INA1=ISAS0(ISB1),ISAS1(ISB1)-1
                     NEND = ISAC(INA1)
                     DO 8913 KK=NST,NEND-1
                        CALL ADVANC(IACON1,NA,NACT)
 8913                CONTINUE
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 8918 IK=1,NA
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 8918                CONTINUE
C
                     T = (C+D)*IPER
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
 8915             CONTINUE
C
 8970             CONTINUE
C
C
C  Loop double excitations from 0th space.
C
                  DO 8967 IBB=IB+1,NB-2
                     IIB = IBCON1(IBB)
                     IS3 = IOX(IIB)
                     IPB = IBB
                     IF (JJ.GT.IIB) IPB=IPB-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA.
C
                     DO 8965 IGEL2=IGEL,NB-2
                        ISTBB = IBCON1(IGEL2)+1
                        IENBB = IBCON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTBB=JJ+1
                        IF (IGEL2.EQ.NB-2) IENBB=NACT
                        DO 8963 JJBB=ISTBB,IENBB
C
                           IS4 = IOX(JJBB)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 8963
C
                  CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
                           IPET1 = POSDET(NACT,NB-2,IACON1,IFA)
                           IPET2 = KJI
                           IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                           IBP2 = ISPB(IPET)
                           IPER = IPER1 + IPER2
                           IPER = (-1)**IPER
C
C   Make matrix element.
C
                           II1 = INDEX(JJBB) + IO1
                           IF (IIB.GT.JJBB) THEN
                              I2 = INDEX(IIB) + JJBB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIB) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIB.LT.JJ) THEN
                              I2 = INDEX(JJBB) + IIB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIB
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJBB) + IIB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIB) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                        DO 8966 INA1 = ISAS0(ISB1),ISAS1(ISB1)-1
                           NEND = ISAC(INA1)
                           ICIA = ISPA(NEND)
                           ICI1 = ICIA + ICAB
                           ICI2 = ICIA + IBP2
                         AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                         AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 8966                   CONTINUE
C
 8963                   CONTINUE
 8965                CONTINUE
C
 8967             CONTINUE
C
C  Loop double excitations, 2nd excitation from external space.
C
                  DO 8957 IBB=NB-1,NB
                     IIB = IBCON1(IBB)
                     IS3 = IOX(IIB)
                     IPB = IBB
C
C  Loop over orbitals in gaps in 0th space, ie AE -> AA
C
                     DO 8955 IGEL2=IGEL,NB-2
                        ISTBB = IBCON1(IGEL2)+1
                        IENBB = IBCON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTBB=JJ+1
                        IF (IGEL2.EQ.NB-2) IENBB=NACT
                        DO 8953 JJBB=ISTBB,IENBB
C
                           IS4 = IOX(JJBB)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 8953
C
                     CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
C
                           IORB = IACON1(NB)-NACT
                           IPET = POSDET(NACT,NB-1,IACON1,IFA)
                           IPET = (IPET-1)*NEXT + IORB + NB0F
                           IBP2 = ISPB(IPET)
                           IPER = IPER1 + IPER2
                           IPER = (-1)**IPER
C
C  Make matrix element.
C
                           II1 = INDEX(JJBB) + IO1
                           I2 = INDEX(IIB) + JJBB
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIB) + JJ
                           INX2 = INDEX(II2) + II1
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                        DO 8956 INA1 = ISAS0(ISB1),ISAS1(ISB1)-1
                           NEND = ISAC(INA1)
                           ICIA = ISPA(NEND)
                           ICI1 = ICIA + ICAB
                           ICI2 = ICIA + IBP2
                         AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                         AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 8956                   CONTINUE
C
 8953                   CONTINUE
 8955                CONTINUE
C
C  Loop over orbitals in gaps in excited space, ie AE -> AE.
C
                     DO 8951 IGEL2=NB-2,NB
                        ISTBB = IBCON1(IGEL2)+1
                        IENBB = IBCON1(IGEL2+1)-1
                        IF (IGEL2.EQ.NB-2) ISTBB=NACT+1
                        IF (IGEL2.EQ.NB) IENBB=NOCC
                        DO 8949 JJBB=ISTBB,IENBB
C
                           IS4 = IOX(JJBB)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 8949
C
                     CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
                           IACON1(NB-1) = IACON1(NB-1)-NACT
                           IACON1(NB)   = IACON1(NB)-NACT
C
                           IPET1 = POSDET(NACT,NB-2,IACON1,IFA)
                           IPET2 = POSDET(NEXT,2,IACON1(NB-1),IFE)
                           IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                           IBP2 = ISPB(IPET)
                           IPER = IPER1 + IPER2
                           IPER = (-1)**IPER
C
C  Make matrix element.
C
                           II1 = INDEX(JJBB) + IO1
                           IF (IIB.GT.JJBB) THEN
                              I2 = INDEX(IIB) + JJBB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIB) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSE
                              I2 = INDEX(JJBB) + IIB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIB) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                        DO 8946 INA1 = ISAS0(ISB1),ISAS1(ISB1)-1
                           NEND = ISAC(INA1)
                           ICIA = ISPA(NEND)
                           ICI1 = ICIA + ICAB
                           ICI2 = ICIA + IBP2
                         AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                         AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 8946                   CONTINUE
C
 8949                   CONTINUE
 8951                CONTINUE
C
 8957             CONTINUE
C
 8975          CONTINUE
 8980       CONTINUE
C
 8985   CONTINUE
C
C  Loop single excitations from excited space.
C
         DO 8785 IB=NB-1,NB
            IO1 = IBCON1(IB)
            IS1 = IOX(IO1)
C
C
C  Loop over orbitals in gaps in excited space, E -> E.
C
            DO 8780 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN=NOCC
               DO 8775 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
C
                  IPER = ((-1)**IPER1)
                  IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 8770
C
                  IBCON2(NB-1) = IBCON2(NB-1)-NACT
                  IBCON2(NB)   = IBCON2(NB)-NACT
C
                  IPET1 = IJK
                  IPET2 = POSDET(NEXT,2,IBCON2(NB-1),IFE)
                  IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                  IPB1 = ISPB(IPET)
C
                  IBCON2(NB-1) = IBCON2(NB-1)+NACT
                  IBCON2(NB) = IBCON2(NB)+NACT
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 8705 IK=1,IB-1
                     ION = IBCON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 8705             CONTINUE
C
                  DO 8707 IK=IB+1,IGEL
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 8707             CONTINUE
C
                  DO 8708 IK=IGEL+1,NB
                     ION = IBCON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 8708             CONTINUE
C
C  Loop over all 0th alpha dets now.
C
                  DO 8710 II=1,NA
                     IACON1(II) = II
 8710             CONTINUE
C
                  NST = 1
                  DO 8715 INA1=ISAS0(ISB1),ISAS1(ISB1)-1
                     NEND = ISAC(INA1)
                     DO 8713 KK=NST,NEND-1
                        CALL ADVANC(IACON1,NA,NACT)
 8713                CONTINUE
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     D = 0.0D+00
                     DO 8718 IK=1,NA
                        ION = IACON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 8718                CONTINUE
C
                     T = (C+D)*IPER
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
 8715             CONTINUE
C
 8770             CONTINUE
C
C   Loop double excitations from excited space, ie EE -> EE.
C
                  DO 8767 IBB=IB+1,NB
                     IIB = IBCON1(IBB)
                     IS3 = IOX(IIB)
                     IPB = IBB
                     IF (JJ.GT.IIB) IPB=IPB-1
C
                     DO 8765 IGEL2=IGEL,NB
                        ISTBB = IBCON1(IGEL2)+1
                        IENBB = IBCON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTBB=JJ+1
                        IF (IGEL2.EQ.NB) IENBB=NOCC
                        DO 8763 JJBB=ISTBB,IENBB
C
                           IS4 = IOX(JJBB)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 8763
C
                     CALL REDE00(IBCON2,IACON1,NB,IPB,IGEL2,JJBB,IPER2)
                           IACON1(NB-1) = IACON1(NB-1)-NACT
                           IACON1(NB)   = IACON1(NB)-NACT
C
                           IPET1 = IJK
                           IPET2 = POSDET(NEXT,2,IACON1(NB-1),IFE)
                           IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                           IBP2 = ISPB(IPET)
                           IPER = IPER1 + IPER2
                           IPER = (-1)**IPER
C
C  Make matrix element.
C
                           II1 = INDEX(JJBB) + IO1
                           IF (IIB.GT.JJBB) THEN
                              I2 = INDEX(IIB) + JJBB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIB) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIB.LT.JJ) THEN
                              I2 = INDEX(JJBB) + IIB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIB
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJBB) + IIB
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIB) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPER
C
C  Loop over alpha strings of the right symmetry.
C
                        DO 8746 INA1 = ISAS0(ISB1),ISAS1(ISB1)-1
                           NEND = ISAC(INA1)
                           ICIA = ISPA(NEND)
                           ICI1 = ICIA + ICAB
                           ICI2 = ICIA + IBP2
                         AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                         AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 8746                   CONTINUE
C
 8763                   CONTINUE
 8765                CONTINUE
C
 8767             CONTINUE
C
 8775          CONTINUE
 8780       CONTINUE
C
 8785    CONTINUE
C
            CALL ADVANC(IBCON1(NB-1),2,NOCC)
 8990    CONTINUE
         IF (NB.LE.2) GOTO 8993
         CALL ADVANC(IBCON1,NB-2,NACT)
 8993 CONTINUE
 8997 CONTINUE
C
C   Now for the diagonal contributions
C
      DO 9119 IJK = 1,NSOCI
         AB(IJK) = AB(IJK) + Q(IJK)*CI(IJK)
 9119 CONTINUE
C
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK SINSO0
C     -----------------------------------------------------------
      SUBROUTINE SINSO0(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,NV,IOX,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA00,IPERA00,IIND100,IMMC00,
     *    IPOSA01,IPERA01,IIND101,IMMC01)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION SI1(*),SI2(*),AB(NSOCI,NV),CI(NSOCI,NV)
      DIMENSION IFA(0:NACT,0:NACT),IFE(0:NEXT,0:2)
      DIMENSION IACON1(NA),IBCON1(NA),IACON2(NA),IBCON2(NA)
      DIMENSION INDEX((NOCC*(NOCC+1))/2 + 1)
      DIMENSION IOX(NOCC)
      DIMENSION ITAB(NSYM),IMUL(NSYM,NSYM)
      DIMENSION ISYMA(NATT),ISYMB(NATT),ISPA(NATT),ISPB(NATT)
      DIMENSION ISBS0(NSYM+1),ISBS1(NSYM),ISBS2(NSYM)
      DIMENSION ISBC(NATT)
      DIMENSION IPOSA00((NA*(NACT-NA)),NSYM)
      DIMENSION IPERA00((NA*(NACT-NA)),NSYM)
      DIMENSION IIND100((NA*(NACT-NA)),NSYM)
      DIMENSION IMMC00(NSYM)
      DIMENSION IPOSA01(NA*NEXT,NSYM)
      DIMENSION IPERA01(NA*NEXT,NSYM)
      DIMENSION IIND101(NA*NEXT,NSYM)
      DIMENSION IMMC01(NSYM)
C
      DO 13 II=1,NSOCI
         DO 12 JJ=1,NV
            AB(II,JJ) = 0.0D+00
   12    CONTINUE
   13 CONTINUE
C
C    --------------------------------------------
C    Loop over all 0th order alpha determinants.
C    --------------------------------------------
C
      DO 20 II=1,NA
         IACON1(II)=II
   20 CONTINUE
C
      DO 2000 IJK=1,NA0F
         ICAT = ISPA(IJK)
         ISA1 = ISYMA(IJK)
         ITAS = ITAB(ISA1)
C
         DO 45 II=1,NSYM
            IMMC00(II) = 0
            IMMC01(II) = 0
   45    CONTINUE
C
C   Loop single excitations from 0th space.
C
         DO 990 IA=1,NA
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C   Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 980 IGEL=IA,NA
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA) IEN=NACT
               DO 970 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
C
C  Storage here for later.
C
                  IPET = POSDET(NACT,NA,IACON2,IFA)
                  IMMC00(ISYMA(IPET)) = IMMC00(ISYMA(IPET)) + 1
                  IPOSA00(IMMC00(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
                  IPER11 = (-1)**IPER1
                  IPERA00(IMMC00(ISYMA(IPET)),ISYMA(IPET)) = IPER11
                  IND = INDEX(JJ)+IO1
                  IIND100(IMMC00(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 900
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 905 IK=1,IA-1
                     J1  = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + IACON1(IK)
                     J2  = INDEX(IO1) + IACON1(IK)
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  905             CONTINUE
C
                  DO 907 IK=IA+1,IGEL
                     J1 = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + IACON1(IK)
                     J2 = INDEX(IACON1(IK)) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  907             CONTINUE
C
                  DO 908 IK=IGEL+1,NA
                     J1 = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(IACON1(IK)) + JJ
                     J2 = INDEX(IACON1(IK)) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  908             CONTINUE
C
C  Loop over all beta strings now. 0th order first.
C
                  DO 910 II=1,NB
                     IBCON1(II) = II
  910             CONTINUE
C
                  NST = 1
                  DO 915 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 913 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
  913                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 918 IK=1,NB
                        J1 = INDEX(IBCON1(IK)+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
  918                CONTINUE
C
                     T = (C+D)*IPER11
                     DO 920 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
  920                CONTINUE
C
                     NST = NEND
  915             CONTINUE
C
C  Loop over 1st order beta strings.
C
                  DO 922 II=1,NB-1
                     IBCON1(II) = II
  922             CONTINUE
                  NSTF = 1
C
                  DO 930 INB1 = ISBS1(ISA1),ISBS2(ISA1)-1
                     NIBP = ISBC(INB1) - NB0F
                     NX = (NIBP-1)/NB1E + 1
                     NY = NIBP - (NX-1)*NB1E
                     DO 924 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-1,NACT)
  924                CONTINUE
                     IBCON1(NB) = NY+NACT
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 926 IK=1,NB-1
                        J1 = INDEX(IBCON1(IK)+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
  926                CONTINUE
C
                     J1 = INDEX(IBCON1(NB)+1)
                     JJ1 = INDEX(J1) + IND
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER11
                     DO 928 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
  928                CONTINUE
C
                     NSTF = NX
  930             CONTINUE
C
C
C  Loop over 2nd order beta strings.
C
                  IF (NB.LT.2) GOTO 900
                  DO 932 II=1,NB-2
                     IBCON1(II) = II
  932             CONTINUE
                  IBCON1(NB-1) = 1 + NACT
                  IBCON1(NB) = 2 + NACT
                  NSTF = 1
                  NSTE = 1
C
                  DO 940 INB1 = ISBS2(ISA1),ISBS0(ISA1+1)-1
                     NIBP = ISBC(INB1) - NB2S
                     NX = (NIBP-1)/NB2E + 1
                     NY = NIBP - (NX-1)*NB2E
                     DO 934 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-2,NACT)
  934                CONTINUE
                     IF (NY.LT.NSTE) THEN
                        IBCON1(NB-1) = 1 + NACT
                        IBCON1(NB)   = 2 + NACT
                        NSTE = 1
                     ENDIF
                     DO 936 KK=NSTE,NY-1
                        CALL ADVANC(IBCON1(NB-1),2,NOCC)
  936                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 937 IK=1,NB-2
                        J1 = INDEX(IBCON1(IK)+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
  937                CONTINUE
C
                     DO 938 IK=NB-1,NB
                        J1 = INDEX(IBCON1(IK)+1)
                        JJ1 = INDEX(J1) + IND
                        D = D + SI2(JJ1)
  938                CONTINUE
C
                     T = (C+D)*IPER11
                     DO 939 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
  939                CONTINUE
C
                     NSTF = NX
                     NSTE = NY
C
  940             CONTINUE
C
  900             CONTINUE
C
C   Loop double excitations from 0th space.
C
                  DO 967 IAA=IA+1,NA
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
                     IF (JJ.GT.IIA) IPA=IPA-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA
C
                     DO 965 IGEL2=IGEL,NA
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA) IENAA=NACT
                        DO 963 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 963
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                           IPET = POSDET(NACT,NA,IBCON1,IFA)
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = (-1)**(IPER1 + IPER2)
C
C   Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           IF (IIA.GT.JJAA) THEN
                              I2 = INDEX(IIA) + JJAA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIA.LT.JJ) THEN
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIA
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                           DO 961 INB1 = ISBS0(ISA1),ISBS0(ISA1+1)-1
                              ICI1 = ICI1 + 1
                              ICI2 = ICI2 + 1
                              DO 960 KJ=1,NV
                            AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                            AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
  960                         CONTINUE
  961                      CONTINUE
C
  963                   CONTINUE
  965                CONTINUE
C
C  Loop over orbitals in external space, ie AA -> AE
C
                     DO 950 JJAA=NACT+1,NOCC
C
                        IS4 = IOX(JJAA)
                        IP2 = IMUL(IS3,IS4)
                        IF (IP1.NE.IP2) GOTO 950
C
                        CALL REDE01(IACON2,IBCON1,NA,IPA,JJAA,IPER2)
C
                        IPET = POSDET(NACT,NA-1,IBCON1,IFA)
                        IPET = (IPET-1)*NEXT + JJAA-NACT + NA0F
                        ICI1 = ICAT
                        ICI2 = ISPA(IPET)
                        IPERT = (-1)**(IPER1 + IPER2)
C
C  Make matrix element.
C
                        II1 = INDEX(JJAA) + IO1
                        I2 = INDEX(JJAA) + IIA
                        INX = INDEX(I2) + IND
                        IF (IIA.LT.JJ) THEN
                           II2 = INDEX(JJ) + IIA
                           INX2 = INDEX(II1) + II2
                        ELSE
                           II2 = INDEX(IIA) + JJ
                           INX2 = INDEX(II1) + II2
                        ENDIF
                        C = SI2(INX) - SI2(INX2)
                        T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 954 INB1 = ISBS0(ISA1),ISBS2(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                           DO 952 KJ=1,NV
                         AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                         AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
  952                      CONTINUE
  954                   CONTINUE
C
  950                CONTINUE
C
  967             CONTINUE
C
  970          CONTINUE
  980       CONTINUE
C
C  Loop over orbitals in external gap, ie  A -> E.
C
            DO 880 JJ=NACT+1,NOCC
C
               IS2 = IOX(JJ)
               IP1 = IMUL(IS2,IS1)
C
               CALL REDE01(IACON1,IACON2,NA,IA,JJ,IPER1)
C
C  Storage here for later.
C
C
               IPET = (POSDET(NACT,NA-1,IACON2,IFA)-1)*NEXT
               IPET = IPET + NA0F + (JJ-NACT)
               IMMC01(ISYMA(IPET)) = IMMC01(ISYMA(IPET)) + 1
               IPOSA01(IMMC01(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
               IPER11 = (-1)**IPER1
               IPERA01(IMMC01(ISYMA(IPET)),ISYMA(IPET)) = IPER11
               IND = INDEX(JJ)+IO1
               IIND101(IMMC01(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
               IF (IS1.NE.IS2) GOTO 800
               ICI1 = ICAT
               ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 805 IK=1,IA-1
                     J1  = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + IACON1(IK)
                     J2  = INDEX(IO1) + IACON1(IK)
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  805             CONTINUE
C
                  DO 807 IK=IA+1,NA
                     J1 = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + IACON1(IK)
                     J2 = INDEX(IACON1(IK)) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  807             CONTINUE
C
C  Loop over all beta dets now. 0th order first.
C
                  DO 810 II=1,NB
                     IBCON1(II) = II
  810             CONTINUE
C
                  NST = 1
                  DO 815 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 813 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
  813                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 818 IK=1,NB
                        J1 = INDEX(IBCON1(IK)+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
  818                CONTINUE
C
                     T = (C+D)*IPER11
                     DO 820 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
  820                CONTINUE
C
                     NST = NEND
  815             CONTINUE
C
C  Loop over 1st order beta strings.
C
                  DO 822 II=1,NB-1
                     IBCON1(II) = II
  822             CONTINUE
                  NSTF = 1
C
                  DO 830 INB1 = ISBS1(ISA1),ISBS2(ISA1)-1
                     NIBP = ISBC(INB1) - NB0F
                     NX = (NIBP-1)/NB1E + 1
                     NY = NIBP - (NX-1)*NB1E
                     DO 824 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-1,NACT)
  824                CONTINUE
                     IBCON1(NB) = NY+NACT
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 826 IK=1,NB-1
                        J1 = INDEX(IBCON1(IK)+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
  826                CONTINUE
C
                     J1 = INDEX(IBCON1(NB)+1)
                     IMA = MAX(IND,J1)
                     IMI = MIN(IND,J1)
                     JJ1 = INDEX(IMA) + IMI
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER11
                     DO 828 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
  828                CONTINUE
C
                     NSTF = NX
  830             CONTINUE
C
  800          CONTINUE
C
C   Loop double excitations from 0th space.
C
               DO 867 IAA=IA+1,NA
                  IIA = IACON1(IAA)
                  IS3 = IOX(IIA)
                  IPA = IAA - 1
C
C  Loop over external orbitals > JJ, ie AA -> EE
C
                  DO 865 JJAA=JJ+1,NOCC
C
                     IS4 = IOX(JJAA)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 865
C
                     CALL REDE01(IACON2,IBCON1,NA,IPA,JJAA,IPER2)
                     IBCON1(NA-1) = JJ-NACT
                     IBCON1(NA)   = JJAA-NACT
C
                     IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                     IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                     IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                     ICI1 = ICAT
                     ICI2 = ISPA(IPET)
                     IPERT = IPER1 + IPER2
                     IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                     II1 = INDEX(JJAA) + IO1
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(JJ) + IIA
                     INX2 = INDEX(II1) + II2
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 863 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                        DO 861 KJ=1,NV
                      AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                      AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
  861                   CONTINUE
  863                CONTINUE
C
  865             CONTINUE
C
  867          CONTINUE
C
  880       CONTINUE
C
  990    CONTINUE
C
C    Loop over all betas and their single excites now.
C
C   First loop over all 0th order betas
C
         DO 1010 II=1,NB
            IBCON1(II) = IACON1(II)
 1010    CONTINUE
C
         DO 1995 KJI=IJK,NB0F
            IPB1 = ISPB(KJI)
            ISB1 = ISYMB(KJI)
            ITBS = ITAB(ISB1)
            IMZZ0 = IMMC00(ITBS)
            IMZZ1 = IMMC01(ITBS)
            IMZZ = IMZZ0 + IMZZ1
            IF (IMZZ.EQ.0.AND.ISB1.NE.ITAS) GOTO 1994
            IC1 = ICAT + IPB1
            QNUM = 1.0D+00
            IF (IJK.EQ.KJI) QNUM=2.0D+00
C
C   Loop single excitations from 0th space.
C
         DO 1990 IB=1,NB
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C   Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 1980 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN=NACT
               DO 1970 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ10 = IMMC00(ITB2)
                  IMZ11 = IMMC01(ITB2)
                  IMZ1 = IMZ10 + IMZ11
C
                  IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 1975
                  IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1975
                  GOTO 1970
C
 1975             CONTINUE
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
                  IPOSB = POSDET(NACT,NB,IBCON2,IFA)
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  ZPERB = IPERB/QNUM
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1960 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA00(IAT,ITB2)
                        DO 1962 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1962                   CONTINUE
 1960                CONTINUE
                     DO 1964 IAT=1,IMZ11
                        IC4 = IPOSA01(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITB2))
                        I2 = MIN(IOB,IIND101(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA01(IAT,ITB2)
                        DO 1963 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1963                   CONTINUE
 1964                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1955 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA00(IAT,ITBS)
                        DO 1957 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1957                   CONTINUE
 1955                CONTINUE
                     DO 1953 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA01(IAT,ITBS)
                        DO 1954 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1954                   CONTINUE
 1953                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1950 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA00(IAT,ITBS)
                        DO 1952 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1952                   CONTINUE
 1950                CONTINUE
                     DO 1958 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        IC4 = IPOSA01(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA01(IAT,ITBS)
                        DO 1951 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1951                   CONTINUE
 1958                CONTINUE
C
                  ENDIF
C
 1970          CONTINUE
 1980       CONTINUE
C
C  Loop over orbitals in external gap, ie  A -> E.
C
            DO 1880 JJ=NACT+1,NOCC
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ10 = IMMC00(ITB2)
               IMZ11 = IMMC01(ITB2)
               IMZ1 = IMZ10 + IMZ11
C
               IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 1875
               IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1875
               GOTO 1880
Cc
 1875          CONTINUE
               CALL REDE01(IBCON1,IBCON2,NB,IB,JJ,IPER1)
               IPOSB = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
               IPOSB = IPOSB + NB0F + (JJ-NACT)
               IPB2 = ISPB(IPOSB)
               IPERB = (-1)**IPER1
               ZPERB = IPERB/QNUM
               IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1860 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA00(IAT,ITB2)
                        DO 1862 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1862                   CONTINUE
 1860                CONTINUE
                     DO 1865 IAT=1,IMZ11
                        IC4 = IPOSA01(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITB2))
                        I2 = MIN(IOB,IIND101(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA01(IAT,ITB2)
                        DO 1864 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1864                   CONTINUE
 1865                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1855 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA00(IAT,ITBS)
                        DO 1857 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1857                   CONTINUE
 1855                CONTINUE
                     DO 1858 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA01(IAT,ITBS)
                        DO 1856 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1856                   CONTINUE
 1858                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1850 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA00(IAT,ITBS)
                        DO 1852 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1852                   CONTINUE
 1850                CONTINUE
                     DO 1853 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        IC4 = IPOSA01(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA01(IAT,ITBS)
                        DO 1851 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1851                   CONTINUE
 1853                CONTINUE
C
                  ENDIF
C
 1880          CONTINUE
C
 1990       CONTINUE
Cc
 1994       CONTINUE
            CALL ADVANC(IBCON1,NB,NACT)
 1995    CONTINUE
C
C  Now loop over 1st order beta strings.
C
         DO 1840 II=1,NB-1
            IBCON1(II) = II
 1840    CONTINUE
         ICOUNT = NB0F
C
         DO 1920 KJI=1,NB1F
            IPOS1 = (KJI-1)*NEXT + NB0F
            DO 1910 KJJ=NACT+1,NOCC
               IBCON1(NB) = KJJ
               ICOUNT = ICOUNT + 1
C
               IPB1 = ISPB(ICOUNT)
               ISB1 = ISYMB(ICOUNT)
               ITBS = ITAB(ISB1)
               IMZZ0 = IMMC00(ITBS)
               IMZZ1 = IMMC01(ITBS)
               IMZZ = IMZZ0 + IMZZ1
               IF (IMZZ.EQ.0.AND.ISB1.NE.ITAS) GOTO 1910
               IC1 = ICAT + IPB1
C
C   Loop single excitations from 0th space.
C
         DO 1905 IB=1,NB-1
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C   Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 1790 IGEL=IB,NB-1
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-1) IEN=NACT
               DO 1780 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ10 = IMMC00(ITB2)
                  IMZ11 = IMMC01(ITB2)
                  IMZ1 = IMZ10 + IMZ11
C
                  IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 1785
                  IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1785
                  GOTO 1780
C
 1785             CONTINUE
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
                  IPOSB = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
                  IPOSB = IPOSB + NB0F + (KJJ-NACT)
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1760 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                        DO 1762 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1762                   CONTINUE
 1760                CONTINUE
                     DO 1764 IAT=1,IMZ11
                        IC4 = IPOSA01(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITB2))
                        I2 = MIN(IOB,IIND101(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITB2)
                        DO 1763 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1763                   CONTINUE
 1764                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1755 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1757 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1757                   CONTINUE
 1755                CONTINUE
                     DO 1758 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                        DO 1759 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1759                   CONTINUE
 1758                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1750 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1752 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1752                   CONTINUE
 1750                CONTINUE
                     DO 1753 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        IC4 = IPOSA01(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                        DO 1754 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1754                   CONTINUE
 1753                CONTINUE
C
                  ENDIF
C
 1780          CONTINUE
 1790       CONTINUE
C
C   Loop over orbitals in gaps in external space, ie A -> E.
C
            IGEL = NB-1
            DO 1690 JJ=NACT+1,NOCC
               IF (JJ.EQ.IBCON1(NB)) THEN
                  IGEL=NB
                  GOTO 1690
               ENDIF
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ10 = IMMC00(ITB2)
               IMZ11 = IMMC01(ITB2)
C
               IF (ISB1.EQ.ITAS.AND.IMZ10.NE.0) GOTO 1685
               IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1685
               GOTO 1690
C
 1685          CONTINUE
               CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
               IBCON2(NB-1) = IBCON2(NB-1)-NACT
               IBCON2(NB)   = IBCON2(NB)-NACT
C
               IPET1 = POSDET(NACT,NB-2,IBCON2,IFA)
               IPET2 = POSDET(NEXT,2,IBCON2(NB-1),IFE)
               IPOSB = (IPET1-1)*NB2E + IPET2 + NB2S
               IPB2 = ISPB(IPOSB)
               IPERB = (-1)**IPER1
               IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1660 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                        DO 1662 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1662                   CONTINUE
 1660                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ10.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1655 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1657 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1657                   CONTINUE
 1655                CONTINUE
                     DO 1658 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                        DO 1659 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1659                   CONTINUE
 1658                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1650 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1652 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1652                   CONTINUE
 1650                CONTINUE
                     DO 1653 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                        DO 1654 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1654                   CONTINUE
 1653                CONTINUE
C
                  ENDIF
C
 1690       CONTINUE
C
 1905    CONTINUE
C
C   Loop single excitations from External space.
C
         IBB = IBCON1(NB)
         IB1 = IOX(IBB)
         IR1 = IMUL(IB1,ISB1)
         IPERB = 1
C
C  Loop over orbitals in gaps in external space, ie E -> E.
C
         DO 1590 JJ=IBB+1,NOCC
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ10 = IMMC00(ITB2)
               IMZ11 = IMMC01(ITB2)
               IMZ1 = IMZ10 + IMZ11
C
               IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 1585
               IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1585
               GOTO 1590
C
 1585          CONTINUE
               IPOSB = IPOS1 + (JJ-NACT)
               IPB2 = ISPB(IPOSB)
               IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1560 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                        DO 1562 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1562                   CONTINUE
 1560                CONTINUE
                     DO 1563 IAT=1,IMZ11
                        IC4 = IPOSA01(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITB2))
                        I2 = MIN(IOB,IIND101(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITB2)
                        DO 1564 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1564                   CONTINUE
 1563                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1555 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1557 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1557                   CONTINUE
 1555                CONTINUE
                     DO 1554 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                        DO 1553 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1553                   CONTINUE
 1554                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1550 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1552 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1552                   CONTINUE
 1550                CONTINUE
                     DO 1558 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        IC4 = IPOSA01(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                        DO 1559 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1559                   CONTINUE
 1558                CONTINUE
C
                  ENDIF
C
 1590    CONTINUE
C
 1910       CONTINUE
            IF (NB.LE.1) GOTO 1920
            CALL ADVANC(IBCON1,NB-1,NACT)
 1920    CONTINUE
C
C  Now loop over 2nd order beta strings.
C
         IF (NB.LT.2) GOTO 1497
         DO 1495 II=1,NB-2
            IBCON1(II) = II
 1495    CONTINUE
         ICOUNT = NB2S
C
         DO 1490 KJI=1,NB2F
            IBCON1(NB-1) = 1 + NACT
            IBCON1(NB) = 2 + NACT
            DO 1485 KJJ=1,NB2E
               ICOUNT = ICOUNT + 1
C
               IPB1 = ISPB(ICOUNT)
               ISB1 = ISYMB(ICOUNT)
               ITBS = ITAB(ISB1)
               IMZZ0 = IMMC00(ITBS)
               IF (IMZZ0.EQ.0.AND.ISB1.NE.ITAS) GOTO 1487
               IC1 = ICAT + IPB1
C
C  Loop single excitations from 0th space.
C
         DO 1480 IB=1,NB-2
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 1475 IGEL=IB,NB-2
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-2) IEN = NACT
               DO 1470 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ10 = IMMC00(ITB2)
C
                  IF (ISB1.EQ.ITAS.AND.IMZ10.NE.0) GOTO 1465
                  IF (ISB2.EQ.ITAS.AND.IMZZ0.NE.0) GOTO 1465
                  GOTO 1470
C
 1465             CONTINUE
                  CALL REDE00(IBCON1,IBCON2,NB-2,IB,IGEL,JJ,IPER1)
C
                  IPET1 = POSDET(NACT,NB-2,IBCON2,IFA)
                  IPOSB = (IPET1-1)*NB2E + KJJ + NB2S
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ0.EQ.0) THEN
                     DO 1460 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                        DO 1462 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1462                   CONTINUE
 1460                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ10.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1455 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1457 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1457                   CONTINUE
 1455                CONTINUE
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1450 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1452 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1452                   CONTINUE
 1450                CONTINUE
                  ENDIF
C
 1470          CONTINUE
 1475       CONTINUE
C
 1480    CONTINUE
C
C  Loop single excitations from Excited space.
C
         DO 1400 IB=NB-1,NB
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C  Loop over orbitals in gaps in Excited space, E -> E.
C
            DO 1395 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN = NOCC
               DO 1390 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ10 = IMMC00(ITB2)
C
                  IF (ISB1.EQ.ITAS.AND.IMZ10.NE.0) GOTO 1385
                  IF (ISB2.EQ.ITAS.AND.IMZZ0.NE.0) GOTO 1385
                  GOTO 1390
C
 1385             CONTINUE
                  CALL REDE00(IBCON1(NB-1),IBCON2(NB-1),
     *            2,IB-NB+2,IGEL-NB+2,JJ,IPER1)
C
                  IBCON2(NB-1) = IBCON2(NB-1)-NACT
                  IBCON2(NB)   = IBCON2(NB)-NACT
                  IPET2 = POSDET(NEXT,2,IBCON2(NB-1),IFE)
                  IPOSB = (KJI-1)*NB2E + IPET2 + NB2S
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ0.EQ.0) THEN
                     DO 1360 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                        DO 1362 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 1362                   CONTINUE
 1360                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ10.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1355 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1357 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1357                   CONTINUE
 1355                CONTINUE
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1350 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                        DO 1352 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 1352                   CONTINUE
 1350                CONTINUE
                  ENDIF
C
 1390          CONTINUE
 1395       CONTINUE
C
 1400   CONTINUE
C
 1487       CONTINUE
            CALL ADVANC(IBCON1(NB-1),2,NOCC)
 1485    CONTINUE
C
         IF (NB.LE.2) GOTO 1490
         CALL ADVANC(IBCON1,NB-2,NACT)
 1490    CONTINUE
 1497    CONTINUE
C
         CALL ADVANC(IACON1,NA,NACT)
 2000 CONTINUE
C
C    --------------------------------------------
C    End of loop over all 0th order alpha strings.
C    --------------------------------------------
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK SINSO1
C     -----------------------------------------------------------
      SUBROUTINE SINSO1(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,NV,IOX,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA11,IPERA11,IIND111,IMMC11,
     *    IPOSA12,IPERA12,IIND112,IMMC12)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION SI1(*),SI2(*),AB(NSOCI,NV),CI(NSOCI,NV)
      DIMENSION IFA(0:NACT,0:NACT),IFE(0:NEXT,0:2)
      DIMENSION IACON1(NA),IBCON1(NA),IACON2(NA),IBCON2(NA)
      DIMENSION INDEX((NOCC*(NOCC+1))/2 + 1)
      DIMENSION IOX(NOCC)
      DIMENSION ITAB(NSYM),IMUL(NSYM,NSYM)
      DIMENSION ISYMA(NATT),ISYMB(NATT),ISPA(NATT),ISPB(NATT)
      DIMENSION ISBS0(NSYM+1),ISBS1(NSYM),ISBS2(NSYM)
      DIMENSION ISBC(NATT)
      DIMENSION IPOSA11(((NA-1)*(NACT-NA+1)+(NEXT-1)),NSYM)
      DIMENSION IPERA11(((NA-1)*(NACT-NA+1)+(NEXT-1)),NSYM)
      DIMENSION IIND111(((NA-1)*(NACT-NA+1)+(NEXT-1)),NSYM)
      DIMENSION IMMC11(NSYM)
      DIMENSION IPOSA12((NA-1)*(NEXT-1),NSYM)
      DIMENSION IPERA12((NA-1)*(NEXT-1),NSYM)
      DIMENSION IIND112((NA-1)*(NEXT-1),NSYM)
      DIMENSION IMMC12(NSYM)
C
C    --------------------------------------------
C    Loop over all 1st order alpha strings.
C    --------------------------------------------
C
      DO 3995 II=1,NA-1
         IACON1(II)=II
 3995 CONTINUE
      ICOUNT = NA0F
C
      DO 3993 IJK=1,NA1F
         IPOS1 = (IJK-1)*NEXT + NA0F
         DO 3990 KJI=NACT+1,NOCC
            IACON1(NA) = KJI
            ICOUNT = ICOUNT + 1
            ICAT = ISPA(ICOUNT)
            ISA1 = ISYMA(ICOUNT)
            ITAS = ITAB(ISA1)
C
            DO 3005 II=1,NSYM
               IMMC11(II) = 0
               IMMC12(II) = 0
 3005       CONTINUE
C
C   Loop single excitations from 0th space.
C
         DO 3985 IA=1,NA-1
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 3980 IGEL=IA,NA-1
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA-1) IEN = NACT
               DO 3975 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
C
C  Storage here for later.
C
                  IPET = (POSDET(NACT,NA-1,IACON2,IFA)-1)*NEXT
                  IPET = IPET + NA0F + (KJI-NACT)
                  IMMC11(ISYMA(IPET)) = IMMC11(ISYMA(IPET)) + 1
                  IPOSA11(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
                  IPER11 = (-1)**IPER1
                  IPERA11(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = IPER11
                  IND = INDEX(JJ)+IO1
                  IIND111(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 3970
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 3905 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3905             CONTINUE
C
                  DO 3907 IK=IA+1,IGEL
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3907             CONTINUE
C
                  DO 3908 IK=IGEL+1,NA
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3908             CONTINUE
C
C  Loop over 0th and 1st beta strings now. 0th order first.
C
                  DO 3910 II=1,NB
                     IBCON1(II) = II
 3910             CONTINUE
C
                  NST = 1
                  DO 3915 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 3913 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 3913                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3918 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 3918                CONTINUE
C
                     T = (C+D)*IPER11
                     DO 3920 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3920                CONTINUE
C
                     NST = NEND
 3915             CONTINUE
C
C  Loop over 1st order beta strings.
C
                  DO 3922 II=1,NB-1
                     IBCON1(II) = II
 3922             CONTINUE
                  NSTF = 1
C
                  DO 3930 INB1 = ISBS1(ISA1),ISBS2(ISA1)-1
                     NIBP = ISBC(INB1) - NB0F
                     NX = (NIBP-1)/NB1E + 1
                     NY = NIBP - (NX-1)*NB1E
                     DO 3924 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-1,NACT)
 3924                CONTINUE
                     IBCON1(NB) = NY+NACT
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3926 IK=1,NB-1
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 3926                CONTINUE
C
                     ION = IBCON1(NB)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER11
                     DO 3928 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3928                CONTINUE
C
                     NSTF = NX
 3930             CONTINUE
C
 3970             CONTINUE
C
C   Loop double excitations from 0th space.
C
                  DO 3967 IAA=IA+1,NA-1
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
                     IF (JJ.GT.IIA) IPA=IPA-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA
C
                     DO 3965 IGEL2=IGEL,NA-1
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA-1) IENAA=NACT
                        DO 3963 JJAA=ISTAA,IENAA
C
                        IS4 = IOX(JJAA)
                        IP2 = IMUL(IS3,IS4)
                        IF (IP1.NE.IP2) GOTO 3963
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                        IPET = POSDET(NACT,NA-1,IBCON1,IFA)
                        IPET = (IPET-1)*NEXT + KJI-NACT + NA0F
                        ICI1 = ICAT
                        ICI2 = ISPA(IPET)
                        IPERT = IPER1 + IPER2
                        IPERT = (-1)**IPERT
C
C   Make matrix element.
C
                        II1 = INDEX(JJAA) + IO1
                        IF (IIA.GT.JJAA) THEN
                           I2 = INDEX(IIA) + JJAA
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIA) + JJ
                           INX2 = INDEX(II2) + II1
                        ELSEIF (IIA.LT.JJ) THEN
                           I2 = INDEX(JJAA) + IIA
                           INX = INDEX(I2) + IND
                           II2 = INDEX(JJ) + IIA
                           INX2 = INDEX(II1) + II2
                        ELSE
                           I2 = INDEX(JJAA) + IIA
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIA) + JJ
                           INX2 = INDEX(II1) + II2
                        ENDIF
                        C = SI2(INX) - SI2(INX2)
                        T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 3954 INB1 = ISBS0(ISA1),ISBS2(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                           DO 3952 KJ=1,NV
                         AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                         AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3952                      CONTINUE
 3954                   CONTINUE
C
 3963                   CONTINUE
 3965                CONTINUE
C
C  Loop over orbitals in gaps in external space, ie AA -> AE
C
                     IGEL2 = NA-1
                     DO 3959 JJAA=NACT+1,NOCC
                     IF (JJAA.EQ.IACON1(NA)) THEN
                        IGEL2=NA
                        GOTO 3959
                     ENDIF
C
                     IS4 = IOX(JJAA)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 3959
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
                     IBCON1(NA-1) = IBCON1(NA-1)-NACT
                     IBCON1(NA)   = IBCON1(NA)-NACT
C
                     IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                     IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                     IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                     ICI1 = ICAT
                     ICI2 = ISPA(IPET)
                     IPERT = IPER1 + IPER2
                     IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                     II1 = INDEX(JJAA) + IO1
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     IF (IIA.LT.JJ) THEN
                        II2 = INDEX(JJ) + IIA
                        INX2 = INDEX(II1) + II2
                     ELSE
                        II2 = INDEX(IIA) + JJ
                        INX2 = INDEX(II1) + II2
                     ENDIF
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 3944 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                        DO 3942 KJ=1,NV
                     AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                     AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3942                   CONTINUE
 3944                CONTINUE
C
 3959                CONTINUE
C
 3967             CONTINUE
C
C   Loop double excitations, 2nd excitation from external space.
C
               IAA = NA
               IIA = IACON1(IAA)
               IS3 = IOX(IIA)
               IPA = IAA
C
C Loop over orbitals in gaps in 0th space, ie AE -> AA
C
               DO 3955 IGEL2=IGEL,NA-1
                  ISTAA = IACON1(IGEL2)+1
                  IENAA = IACON1(IGEL2+1)-1
                  IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                  IF (IGEL2.EQ.NA-1) IENAA=NACT
                  DO 3957 JJAA=ISTAA,IENAA
C
                     IS4 = IOX(JJAA)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 3957
C
                  CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                     IPET = POSDET(NACT,NA,IBCON1,IFA)
                     ICI1 = ICAT
                     ICI2 = ISPA(IPET)
                     IPERT = IPER1 + IPER2
                     IPERT = (-1)**IPERT
C
                     II1 = INDEX(JJAA) + IO1
                     I2 = INDEX(IIA) + JJAA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIA) + JJ
                     INX2 = INDEX(II2) + II1
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 3934 INB1 = ISBS0(ISA1),ISBS2(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                        DO 3932 KJ=1,NV
                     AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                     AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3932                   CONTINUE
 3934                CONTINUE
C
 3957             CONTINUE
 3955          CONTINUE
C
C  Loop over orbitals in gaps in external space, ie AE -> AE
C
               IGEL2 = NA-1
               DO 3953 JJAA=NACT+1,NOCC
                  IF (JJAA.EQ.IIA) THEN
                     IGEL2 = NA
                     GOTO 3953
                  ENDIF
C
                  IS4 = IOX(JJAA)
                  IP2 = IMUL(IS3,IS4)
                  IF (IP1.NE.IP2) GOTO 3953
C
              CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                  IPET = POSDET(NACT,NA-1,IBCON1,IFA)
                  IPET = (IPET-1)*NEXT + JJAA-NACT + NA0F
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
                  IPERT = IPER1 + IPER2
                  IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                  II1 = INDEX(JJAA) + IO1
                  II2 = INDEX(IIA) + JJ
                  IF (IIA.GT.JJAA) THEN
                     I2 = INDEX(IIA) + JJAA
                     INX = INDEX(I2) + IND
                     INX2 = INDEX(II2) + II1
                  ELSE
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     INX2 = INDEX(II1) + II2
                  ENDIF
                  C = SI2(INX) - SI2(INX2)
                  T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 3951 INB1 = ISBS0(ISA1),ISBS2(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                        DO 3929 KJ=1,NV
                     AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                     AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3929                   CONTINUE
 3951                CONTINUE
C
 3953          CONTINUE
C
 3975          CONTINUE
 3980       CONTINUE
C
C  Loop over orbitals in gaps in external space, A -> E.
C
            IGEL = NA-1
            DO 3895 JJ=NACT+1,NOCC
               IF (JJ.EQ.IACON1(NA)) THEN
                  IGEL = NA
                  GOTO 3895
               ENDIF
               IS2 = IOX(JJ)
               IP1 = IMUL(IS2,IS1)
C
               CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
               IACON2(NA-1) = IACON2(NA-1)-NACT
               IACON2(NA) = IACON2(NA)-NACT
C
C  Storage here for later.
C
               IPET1 = POSDET(NACT,NA-2,IACON2,IFA)
               IPET2 = POSDET(NEXT,2,IACON2(NA-1),IFE)
               IPET = (IPET1-1)*NA2E + IPET2 + NA2S
               IMMC12(ISYMA(IPET)) = IMMC12(ISYMA(IPET)) + 1
               IPOSA12(IMMC12(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
               IPER11 = (-1)**IPER1
               IPERA12(IMMC12(ISYMA(IPET)),ISYMA(IPET)) = IPER11
               IND = INDEX(JJ)+IO1
               IIND112(IMMC12(ISYMA(IPET)),ISYMA(IPET)) = IND
C
               IACON2(NA-1) = IACON2(NA-1)+NACT
               IACON2(NA) = IACON2(NA)+NACT
C
C  Check if excited string is correct symmetry.
C
               IF (IS1.NE.IS2) GOTO 3870
               ICI1 = ICAT
               ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 3805 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3805             CONTINUE
C
                  DO 3807 IK=IA+1,IGEL
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3807             CONTINUE
C
                  DO 3808 IK=IGEL+1,NA
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3808             CONTINUE
C
C  Loop over all 0th beta dets now.
C
                  DO 3810 II=1,NB
                     IBCON1(II) = II
 3810             CONTINUE
C
                  NST = 1
                  DO 3815 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 3813 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 3813                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3818 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 3818                CONTINUE
C
                     T = (C+D)*IPER11
                     DO 3820 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3820                CONTINUE
C
                     NST = NEND
 3815             CONTINUE
C
 3870          CONTINUE
C
C   Loop double excitations, AE -> EE
C
               IAA = NA
               IIA = IACON1(IAA)
               IS3 = IOX(IIA)
               IPA = IAA
               IF (JJ.GT.IIA) IPA=IPA-1
C
C Loop over orbitals in gaps in external space > JJ, AE -> EE
C
               IGEL2 = IGEL
               DO 3865 JJAA=JJ+1,NOCC
                  IF (JJAA.EQ.IIA) THEN
                     IGEL2=NA
                     GOTO 3865
                  ENDIF
C
                  IS4 = IOX(JJAA)
                  IP2 = IMUL(IS3,IS4)
                  IF (IP1.NE.IP2) GOTO 3865
C
             CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
                  IBCON1(NA-1) = IBCON1(NA-1)-NACT
                  IBCON1(NA)   = IBCON1(NA)-NACT
C
                  IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                  IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                  IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
                  IPERT = IPER1 + IPER2
                  IPERT = (-1)**IPERT
C
C Make matrix element.
C
                  II1 = INDEX(JJAA) + IO1
                  IF (IIA.GT.JJAA) THEN
                     I2 = INDEX(IIA) + JJAA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIA) + JJ
                     INX2 = INDEX(II2) + II1
                  ELSEIF (IIA.LT.JJ) THEN
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(JJ) + IIA
                     INX2 = INDEX(II1) + II2
                  ELSE
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIA) + JJ
                     INX2 = INDEX(II1) + II2
                  ENDIF
                  C = SI2(INX) - SI2(INX2)
                  T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 3851 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                        DO 3853 KJ=1,NV
                     AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                     AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3853                   CONTINUE
 3851                CONTINUE
C
 3865          CONTINUE
C
 3895       CONTINUE
C
 3985    CONTINUE
C
C  Loop single excitation from external space.
C
         IA = NA
         IO1 = IACON1(NA)
         IS1 = IOX(IO1)
         IPER11 = 1
C
C  Loop over orbitals in external space, E -> E.
C
         DO 3795 JJ=IO1+1,NOCC
            IS2 = IOX(JJ)
            IP1 = IMUL(IS2,IS1)
C
            CALL REDE01(IACON1,IACON2,NA,IA,JJ,IPER1)
C
C  Storage here for later.
C
            IPET = IPOS1 + (JJ-NACT)
            IMMC11(ISYMA(IPET)) = IMMC11(ISYMA(IPET)) + 1
            IPOSA11(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
            IPERA11(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = IPER11
            IND = INDEX(JJ)+IO1
            IIND111(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
            IF (IS1.NE.IS2) GOTO 3770
            ICI1 = ICAT
            ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 3705 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3705             CONTINUE
C
C  Loop over all 0th beta dets now.
C
                  DO 3710 II=1,NB
                     IBCON1(II) = II
 3710             CONTINUE
C
                  NST = 1
                  DO 3715 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 3713 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 3713                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3718 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 3718                CONTINUE
C
                     T = (C+D)*IPER11
                     DO 3720 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3720                CONTINUE
C
                     NST = NEND
 3715             CONTINUE
C
C  Loop over 1st order beta strings.
C
                  DO 3722 II=1,NB-1
                     IBCON1(II) = II
 3722             CONTINUE
                  NSTF = 1
C
                  DO 3730 INB1 = ISBS1(ISA1),ISBS2(ISA1)-1
                     NIBP = ISBC(INB1) - NB0F
                     NX = (NIBP-1)/NB1E + 1
                     NY = NIBP - (NX-1)*NB1E
                     DO 3724 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-1,NACT)
 3724                CONTINUE
                     IBCON1(NB) = NY+NACT
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3726 IK=1,NB-1
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 3726                CONTINUE
C
                     ION = IBCON1(NB)
                     J1 = INDEX(ION+1)
                     IMA = MAX(IND,J1)
                     IMI = MIN(IND,J1)
                     JJ1 = INDEX(IMA) + IMI
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER11
                     DO 3728 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 3728                CONTINUE
C
                     NSTF = NX
 3730             CONTINUE
C
 3770       CONTINUE
C
 3795    CONTINUE
C
C    Loop over all relevant betas and their relevant single excites now.
C
C  Now loop over 1st order beta strings.
C
         DO 3020 II=1,NB-1
            IBCON1(II) = IACON1(II)
 3020    CONTINUE
         ICOUNTB = ICOUNT - 1
C
         DO 3500 KJIH=IJK,NB1F
            IPOSB1 = (KJIH-1)*NEXT + NB0F
            KJS = NACT+1
            IF (KJIH.EQ.IJK) KJS = KJI
            DO 3505 KJJ=KJS,NOCC
               IBCON1(NB) = KJJ
               ICOUNTB = ICOUNTB + 1
C
               IPB1 = ISPB(ICOUNTB)
               ISB1 = ISYMB(ICOUNTB)
               ITBS = ITAB(ISB1)
               IMZZ1 = IMMC11(ITBS)
               IF (IMZZ1.EQ.0.AND.ISB1.NE.ITAS) GOTO 3505
               IC1 = ICAT + IPB1
               QNUM=1.0D+00
               IF (ICOUNT.EQ.ICOUNTB) QNUM=2.0D+00
C
C   Loop single excitations from 0th space.
C
         DO 3510 IB=1,NB-1
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C  Loop over orbitals in gaps in 0th space, ie A -> A (B1 -> B1)
C
            DO 3515 IGEL=IB,NB-1
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-1) IEN=NACT
               DO 3520 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ11 = IMMC11(ITB2)
C
                  IF (ISB1.EQ.ITAS.AND.IMZ11.NE.0) GOTO 3523
                  IF (ISB2.EQ.ITAS.AND.IMZZ1.NE.0) GOTO 3523
                  GOTO 3520
C
 3523             CONTINUE
C
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
                  IPOSB = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
                  IPOSB = IPOSB + NB0F + (KJJ-NACT)
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  ZPERB = IPERB/QNUM
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ1.EQ.0) THEN
                     DO 3526 IAT=1,IMZ11
                        IC4 = IPOSA11(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND111(IAT,ITB2))
                        I2 = MIN(IOB,IIND111(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA11(IAT,ITB2)
                        DO 3529 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 3529                   CONTINUE
 3526                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ11.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 3530 IAT=1,IMZZ1
                        IC3 = IPOSA11(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND111(IAT,ITBS))
                        I2 = MIN(IOB,IIND111(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA11(IAT,ITBS)
                        DO 3533 KJ=1,NV
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 3533                   CONTINUE
 3530                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 3532 IAT=1,IMZZ1
                        IC3 = IPOSA11(IAT,ITBS) + IPB1
                        IC4 = IPOSA11(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND111(IAT,ITBS))
                        I2 = MIN(IOB,IIND111(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA11(IAT,ITBS)
                        DO 3534 KJ=1,NV
                           AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                           AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                           AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                           AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 3534                   CONTINUE
 3532                CONTINUE
C
                  ENDIF
C
 3520          CONTINUE
 3515       CONTINUE
C
 3510    CONTINUE
C
C   Loop single excitations from External space.
C
         IBB = IBCON1(NB)
         IB1 = IOX(IBB)
         IR1 = IMUL(IB1,ISB1)
         ZPERB = 1.0D+00/QNUM
C
C  Loop over orbitals in gaps in external space, ie E -> E, (B1 -> B1)
C
         DO 3550 JJ=IBB+1,NOCC
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ11 = IMMC11(ITB2)
C
               IF (ISB1.EQ.ITAS.AND.IMZ11.NE.0) GOTO 3555
               IF (ISB2.EQ.ITAS.AND.IMZZ1.NE.0) GOTO 3555
               GOTO 3550
C
 3555          CONTINUE
C
               IPOSB = IPOSB1 + (JJ-NACT)
               IPB2 = ISPB(IPOSB)
               IOB = INDEX(JJ)+IBB
C
               IF (ISB2.NE.ITAS.OR.IMZZ1.EQ.0) THEN
                  DO 3560 IAT=1,IMZ11
                     IC4 = IPOSA11(IAT,ITB2) + IPB2
                     I1 = MAX(IOB,IIND111(IAT,ITB2))
                     I2 = MIN(IOB,IIND111(IAT,ITB2))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*ZPERB*IPERA11(IAT,ITB2)
                     DO 3562 KJ=1,NV
                        AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                        AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
 3562                CONTINUE
 3560             CONTINUE
C
               ELSE IF(ISB1.NE.ITAS.OR.IMZ11.EQ.0) THEN
                  IC2 = ICAT + IPB2
                  DO 3565 IAT=1,IMZZ1
                     IC3 = IPOSA11(IAT,ITBS) + IPB1
                     I1 = MAX(IOB,IIND111(IAT,ITBS))
                     I2 = MIN(IOB,IIND111(IAT,ITBS))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*ZPERB*IPERA11(IAT,ITBS)
                     DO 3570 KJ=1,NV
                        AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                        AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 3570                CONTINUE
 3565             CONTINUE
C
               ELSE
                  IC2 = ICAT + IPB2
                  DO 3573 IAT=1,IMZZ1
                     IC3 = IPOSA11(IAT,ITBS) + IPB1
                     IC4 = IPOSA11(IAT,ITBS) + IPB2
                     I1 = MAX(IOB,IIND111(IAT,ITBS))
                     I2 = MIN(IOB,IIND111(IAT,ITBS))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*ZPERB*IPERA11(IAT,ITBS)
                     DO 3576 KJ=1,NV
                        AB(IC1,KJ) = AB(IC1,KJ) + C*CI(IC4,KJ)
                        AB(IC4,KJ) = AB(IC4,KJ) + C*CI(IC1,KJ)
                        AB(IC2,KJ) = AB(IC2,KJ) + C*CI(IC3,KJ)
                        AB(IC3,KJ) = AB(IC3,KJ) + C*CI(IC2,KJ)
 3576                CONTINUE
 3573             CONTINUE
C
               ENDIF
C
 3550    CONTINUE
C
 3505       CONTINUE
            IF (NB.LE.1) GOTO 3500
            CALL ADVANC(IBCON1,NB-1,NACT)
 3500    CONTINUE
C
 3990    CONTINUE
         CALL ADVANC(IACON1,NA-1,NACT)
 3993 CONTINUE
C
C    --------------------------------------------
C    End of loop over all 1st order alpha strings.
C    --------------------------------------------
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK SINSO2
C     -----------------------------------------------------------
      SUBROUTINE SINSO2(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IFA,IFE,INDEX,AB,NV,Q,IOX,
     *    IMUL,ISYMA,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA22,IPERA22,IIND122,IMMC22,ISPIN,IHMCON)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION SI1(*),SI2(*),AB(NSOCI,NV),CI(NSOCI,NV)
      DIMENSION Q(NSOCI)
      DIMENSION IFA(0:NACT,0:NACT),IFE(0:NEXT,0:2)
      DIMENSION IACON1(NA),IBCON1(NA),IACON2(NA)
      DIMENSION INDEX((NOCC*(NOCC+1))/2 + 1)
      DIMENSION IOX(NOCC)
      DIMENSION IMUL(NSYM,NSYM)
      DIMENSION ISYMA(NATT),ISPA(NATT),ISPB(NATT)
      DIMENSION ISBS0(NSYM+1),ISBS1(NSYM),ISBS2(NSYM)
      DIMENSION ISBC(NATT)
      DIMENSION IPOSA22(((NA-2)*(NACT-NA+2)+2*(NEXT-2)),NSYM)
      DIMENSION IPERA22(((NA-2)*(NACT-NA+2)+2*(NEXT-2)),NSYM)
      DIMENSION IIND122(((NA-2)*(NACT-NA+2)+2*(NEXT-2)),NSYM)
      DIMENSION IMMC22(NSYM)
      DIMENSION ISPIN(*),IHMCON(NV)
C
C    --------------------------------------------
C    Loop over all 2nd order alpha strings.
C    --------------------------------------------
C
      DO 4995 II=1,NA-2
         IACON1(II) = II
 4995 CONTINUE
      ICOUNT = NA2S
C
      DO 4993 IJK = 1,NA2F
         IACON1(NA-1) = 1 + NACT
         IACON1(NA) = 2 + NACT
         DO 4990 KJI=1,NA2E
            ICOUNT = ICOUNT + 1
            ICAT = ISPA(ICOUNT)
            ISA1 = ISYMA(ICOUNT)
C
            DO 4005 II=1,NSYM
               IMMC22(II) = 0
 4005       CONTINUE
C
C   Loop single excitations from 0th space.
C
         DO 4985 IA=1,NA-2
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in zeroth order space, A -> A.
C
            DO 4980 IGEL=IA,NA-2
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA-2) IEN = NACT
               DO 4975 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
C
C  Storage here for later.
C
                  IPET1 = POSDET(NACT,NA-2,IACON2,IFA)
                  IPET2 = KJI
                  IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                  IMMC22(ISYMA(IPET)) = IMMC22(ISYMA(IPET)) + 1
                  IPOSA22(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
                  IPER11 = (-1)**IPER1
                  IPERA22(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = IPER11
                  IND = INDEX(JJ)+IO1
                  IIND122(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 4970
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 4905 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4905             CONTINUE
C
                  DO 4907 IK=IA+1,IGEL
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4907             CONTINUE
C
                  DO 4908 IK=IGEL+1,NA
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4908             CONTINUE
C
C  Loop over all 0th beta dets now.
C
                  DO 4910 II=1,NB
                     IBCON1(II) = II
 4910             CONTINUE
C
                  NST = 1
                  DO 4915 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 4913 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 4913                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 4918 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 4918                CONTINUE
C
                     T = (C+D)*IPER11
                     DO 4920 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 4920                CONTINUE
C
                     NST = NEND
 4915             CONTINUE
C
 4970             CONTINUE
C
C  Loop double excitations from 0th space.
C
                  DO 4967 IAA=IA+1,NA-2
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
                     IF (JJ.GT.IIA) IPA=IPA-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA.
C
                     DO 4965 IGEL2=IGEL,NA-2
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA-2) IENAA=NACT
                        DO 4963 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 4963
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                           IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                           IPET2 = KJI
                           IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = IPER1 + IPER2
                           IPERT = (-1)**IPERT
C
C   Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           IF (IIA.GT.JJAA) THEN
                              I2 = INDEX(IIA) + JJAA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIA.LT.JJ) THEN
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIA
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 4966 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                           DO 4961 KJ=1,NV
                      AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                      AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 4961                      CONTINUE
 4966                   CONTINUE
C
 4963                   CONTINUE
 4965                CONTINUE
C
 4967             CONTINUE
C
C  Loop double excitations, 2nd excitation from external space.
C
                  DO 4957 IAA=NA-1,NA
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
C
C  Loop over orbitals in gaps in 0th space, ie AE -> AA
C
                     DO 4955 IGEL2=IGEL,NA-2
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA-2) IENAA=NACT
                        DO 4953 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 4953
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                           IORB = IBCON1(NA)-NACT
                           IPET = POSDET(NACT,NA-1,IBCON1,IFA)
                           IPET = (IPET-1)*NEXT + IORB + NA0F
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = IPER1 + IPER2
                           IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           I2 = INDEX(IIA) + JJAA
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIA) + JJ
                           INX2 = INDEX(II2) + II1
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 4956 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                           DO 4931 KJ=1,NV
                      AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                      AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 4931                      CONTINUE
 4956                   CONTINUE
C
 4953                   CONTINUE
 4955                CONTINUE
C
C  Loop over orbitals in gaps in excited space, ie AE -> AE.
C
                     DO 4951 IGEL2=NA-2,NA
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.NA-2) ISTAA=NACT+1
                        IF (IGEL2.EQ.NA) IENAA=NOCC
                        DO 4949 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 4949
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
                           IBCON1(NA-1) = IBCON1(NA-1)-NACT
                           IBCON1(NA)   = IBCON1(NA)-NACT
C
                           IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                           IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                           IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = IPER1 + IPER2
                           IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           IF (IIA.GT.JJAA) THEN
                              I2 = INDEX(IIA) + JJAA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSE
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 4946 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                           DO 4941 KJ=1,NV
                      AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                      AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 4941                      CONTINUE
 4946                   CONTINUE
C
 4949                   CONTINUE
 4951                CONTINUE
C
 4957             CONTINUE
C
 4975          CONTINUE
 4980       CONTINUE
C
 4985    CONTINUE
C
C  Loop single excitations from excited space.
C
         DO 4785 IA=NA-1,NA
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in excited space, E -> E.
C
            DO 4780 IGEL=IA,NA
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA) IEN=NOCC
               DO 4775 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
                  IACON2(NA-1) = IACON2(NA-1)-NACT
                  IACON2(NA) = IACON2(NA)-NACT
C
C  Storage here for later.
C
                  IPET1 = IJK
                  IPET2 = POSDET(NEXT,2,IACON2(NA-1),IFE)
                  IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                  IMMC22(ISYMA(IPET)) = IMMC22(ISYMA(IPET)) + 1
                  IPOSA22(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
                  IPER11 = (-1)**IPER1
                  IPERA22(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = IPER11
                  IND = INDEX(JJ)+IO1
                  IIND122(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = IND
C
                  IACON2(NA-1) = IACON2(NA-1)+NACT
                  IACON2(NA) = IACON2(NA)+NACT
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 4770
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 4705 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4705             CONTINUE
C
                  DO 4707 IK=IA+1,IGEL
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4707             CONTINUE
C
                  DO 4708 IK=IGEL+1,NA
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4708             CONTINUE
C
C  Loop over all 0th beta dets now.
C
                  DO 4710 II=1,NB
                     IBCON1(II) = II
 4710             CONTINUE
C
                  NST = 1
                  DO 4715 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 4713 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 4713                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 4718 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 4718                CONTINUE
C
                     T = (C+D)*IPER11
                     DO 4720 KJ=1,NV
                        AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                        AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 4720                CONTINUE
C
                     NST = NEND
 4715             CONTINUE
C
 4770             CONTINUE
C
C   Loop double excitations from excited space, ie EE -> EE.
C
                  DO 4767 IAA=IA+1,NA
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
                     IF (JJ.GT.IIA) IPA=IPA-1
C
                     DO 4765 IGEL2=IGEL,NA
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA) IENAA=NOCC
                        DO 4763 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 4763
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
                           IBCON1(NA-1) = IBCON1(NA-1)-NACT
                           IBCON1(NA)   = IBCON1(NA)-NACT
C
                           IPET1 = IJK
                           IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                           IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = IPER1 + IPER2
                           IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           IF (IIA.GT.JJAA) THEN
                              I2 = INDEX(IIA) + JJAA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIA.LT.JJ) THEN
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIA
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 4746 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                           DO 4741 KJ=1,NV
                      AB(ICI1,KJ) = AB(ICI1,KJ) + T*CI(ICI2,KJ)
                      AB(ICI2,KJ) = AB(ICI2,KJ) + T*CI(ICI1,KJ)
 4741                      CONTINUE
 4746                   CONTINUE
C
 4763                   CONTINUE
 4765                CONTINUE
C
 4767             CONTINUE
C
 4775          CONTINUE
 4780       CONTINUE
C
 4785    CONTINUE
C
            CALL ADVANC(IACON1(NA-1),2,NOCC)
 4990    CONTINUE
         CALL ADVANC(IACON1,NA-2,NACT)
 4993 CONTINUE
C
      DO 1111 II=1,NATT
         ISA1 = ISYMA(II)
         ICIT = ISPA(II)
         INB = ISPB(II)
         IENDO = ISBS1(ISA1)-1
         IF (II.GT.NA0F.AND.II.LE.NA2S) IENDO = ISBS2(ISA1)-1
         DO 2222 INB1=ISBS0(ISA1),IENDO
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
      DO 9119 IJK = 1,NSOCI
         DO 9118 KJ = 1,NV
            AB(IJK,KJ) = AB(IJK,KJ) + Q(IJK)*CI(IJK,KJ)
 9118    CONTINUE
 9119 CONTINUE
C
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK SINSO01
C     -----------------------------------------------------------
      SUBROUTINE SINSO01(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,IOX,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA00,IPERA00,IIND100,IMMC00,
     *    IPOSA01,IPERA01,IIND101,IMMC01)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION SI1(*),SI2(*),AB(NSOCI),CI(NSOCI)
      DIMENSION IFA(0:NACT,0:NACT),IFE(0:NEXT,0:2)
      DIMENSION IACON1(NA),IBCON1(NA),IACON2(NA),IBCON2(NA)
      DIMENSION INDEX((NOCC*(NOCC+1))/2 + 1)
      DIMENSION IOX(NOCC)
      DIMENSION ITAB(NSYM),IMUL(NSYM,NSYM)
      DIMENSION ISYMA(NATT),ISYMB(NATT),ISPA(NATT),ISPB(NATT)
      DIMENSION ISBS0(NSYM+1),ISBS1(NSYM),ISBS2(NSYM)
      DIMENSION ISBC(NATT)
      DIMENSION IPOSA00((NA*(NACT-NA)),NSYM)
      DIMENSION IPERA00((NA*(NACT-NA)),NSYM)
      DIMENSION IIND100((NA*(NACT-NA)),NSYM)
      DIMENSION IMMC00(NSYM)
      DIMENSION IPOSA01(NA*NEXT,NSYM)
      DIMENSION IPERA01(NA*NEXT,NSYM)
      DIMENSION IIND101(NA*NEXT,NSYM)
      DIMENSION IMMC01(NSYM)
C
      DO 13 II=1,NSOCI
         AB(II) = 0.0D+00
   13 CONTINUE
C
C    --------------------------------------------
C    Loop over all 0th order alpha determinants.
C    --------------------------------------------
C
      DO 20 II=1,NA
         IACON1(II)=II
   20 CONTINUE
C
      DO 2000 IJK=1,NA0F
         ICAT = ISPA(IJK)
         ISA1 = ISYMA(IJK)
         ITAS = ITAB(ISA1)
C
         DO 45 II=1,NSYM
            IMMC00(II) = 0
            IMMC01(II) = 0
   45    CONTINUE
C
C   Loop single excitations from 0th space.
C
         DO 990 IA=1,NA
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C   Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 980 IGEL=IA,NA
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA) IEN=NACT
               DO 970 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
C
C  Storage here for later.
C
                  IPET = POSDET(NACT,NA,IACON2,IFA)
                  IMMC00(ISYMA(IPET)) = IMMC00(ISYMA(IPET)) + 1
                  IPOSA00(IMMC00(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
                  IPER11 = (-1)**IPER1
                  IPERA00(IMMC00(ISYMA(IPET)),ISYMA(IPET)) = IPER11
                  IND = INDEX(JJ)+IO1
                  IIND100(IMMC00(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 900
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 905 IK=1,IA-1
                     J1  = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + IACON1(IK)
                     J2  = INDEX(IO1) + IACON1(IK)
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  905             CONTINUE
C
                  DO 907 IK=IA+1,IGEL
                     J1 = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + IACON1(IK)
                     J2 = INDEX(IACON1(IK)) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  907             CONTINUE
C
                  DO 908 IK=IGEL+1,NA
                     J1 = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(IACON1(IK)) + JJ
                     J2 = INDEX(IACON1(IK)) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  908             CONTINUE
C
C  Loop over all beta strings now. 0th order first.
C
                  DO 910 II=1,NB
                     IBCON1(II) = II
  910             CONTINUE
C
                  NST = 1
                  DO 915 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 913 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
  913                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 918 IK=1,NB
                        J1 = INDEX(IBCON1(IK)+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
  918                CONTINUE
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
  915             CONTINUE
C
C  Loop over 1st order beta strings.
C
                  DO 922 II=1,NB-1
                     IBCON1(II) = II
  922             CONTINUE
                  NSTF = 1
C
                  DO 930 INB1 = ISBS1(ISA1),ISBS2(ISA1)-1
                     NIBP = ISBC(INB1) - NB0F
                     NX = (NIBP-1)/NB1E + 1
                     NY = NIBP - (NX-1)*NB1E
                     DO 924 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-1,NACT)
  924                CONTINUE
                     IBCON1(NB) = NY+NACT
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 926 IK=1,NB-1
                        J1 = INDEX(IBCON1(IK)+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
  926                CONTINUE
C
                     J1 = INDEX(IBCON1(NB)+1)
                     JJ1 = INDEX(J1) + IND
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NSTF = NX
  930             CONTINUE
C
C
C  Loop over 2nd order beta strings.
C
                  IF (NB.LT.2) GOTO 900
                  DO 932 II=1,NB-2
                     IBCON1(II) = II
  932             CONTINUE
                  IBCON1(NB-1) = 1 + NACT
                  IBCON1(NB) = 2 + NACT
                  NSTF = 1
                  NSTE = 1
C
                  DO 940 INB1 = ISBS2(ISA1),ISBS0(ISA1+1)-1
                     NIBP = ISBC(INB1) - NB2S
                     NX = (NIBP-1)/NB2E + 1
                     NY = NIBP - (NX-1)*NB2E
                     DO 934 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-2,NACT)
  934                CONTINUE
                     IF (NY.LT.NSTE) THEN
                        IBCON1(NB-1) = 1 + NACT
                        IBCON1(NB)   = 2 + NACT
                        NSTE = 1
                     ENDIF
                     DO 936 KK=NSTE,NY-1
                        CALL ADVANC(IBCON1(NB-1),2,NOCC)
  936                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 937 IK=1,NB-2
                        J1 = INDEX(IBCON1(IK)+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
  937                CONTINUE
C
                     DO 938 IK=NB-1,NB
                        J1 = INDEX(IBCON1(IK)+1)
                        JJ1 = INDEX(J1) + IND
                        D = D + SI2(JJ1)
  938                CONTINUE
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NSTF = NX
                     NSTE = NY
C
  940             CONTINUE
C
  900             CONTINUE
C
C   Loop double excitations from 0th space.
C
                  DO 967 IAA=IA+1,NA
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
                     IF (JJ.GT.IIA) IPA=IPA-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA
C
                     DO 965 IGEL2=IGEL,NA
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA) IENAA=NACT
                        DO 963 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 963
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                           IPET = POSDET(NACT,NA,IBCON1,IFA)
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = (-1)**(IPER1 + IPER2)
C
C   Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           IF (IIA.GT.JJAA) THEN
                              I2 = INDEX(IIA) + JJAA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIA.LT.JJ) THEN
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIA
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                           DO 961 INB1 = ISBS0(ISA1),ISBS0(ISA1+1)-1
                              ICI1 = ICI1 + 1
                              ICI2 = ICI2 + 1
                            AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                            AB(ICI2) = AB(ICI2) + T*CI(ICI1)
  961                      CONTINUE
C
  963                   CONTINUE
  965                CONTINUE
C
C  Loop over orbitals in external space, ie AA -> AE
C
                     DO 950 JJAA=NACT+1,NOCC
C
                        IS4 = IOX(JJAA)
                        IP2 = IMUL(IS3,IS4)
                        IF (IP1.NE.IP2) GOTO 950
C
                        CALL REDE01(IACON2,IBCON1,NA,IPA,JJAA,IPER2)
C
                        IPET = POSDET(NACT,NA-1,IBCON1,IFA)
                        IPET = (IPET-1)*NEXT + JJAA-NACT + NA0F
                        ICI1 = ICAT
                        ICI2 = ISPA(IPET)
                        IPERT = (-1)**(IPER1 + IPER2)
C
C  Make matrix element.
C
                        II1 = INDEX(JJAA) + IO1
                        I2 = INDEX(JJAA) + IIA
                        INX = INDEX(I2) + IND
                        IF (IIA.LT.JJ) THEN
                           II2 = INDEX(JJ) + IIA
                           INX2 = INDEX(II1) + II2
                        ELSE
                           II2 = INDEX(IIA) + JJ
                           INX2 = INDEX(II1) + II2
                        ENDIF
                        C = SI2(INX) - SI2(INX2)
                        T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 954 INB1 = ISBS0(ISA1),ISBS2(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                         AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                         AB(ICI2) = AB(ICI2) + T*CI(ICI1)
  954                   CONTINUE
C
  950                CONTINUE
C
  967             CONTINUE
C
  970          CONTINUE
  980       CONTINUE
C
C  Loop over orbitals in external gap, ie  A -> E.
C
            DO 880 JJ=NACT+1,NOCC
C
               IS2 = IOX(JJ)
               IP1 = IMUL(IS2,IS1)
C
               CALL REDE01(IACON1,IACON2,NA,IA,JJ,IPER1)
C
C  Storage here for later.
C
C
               IPET = (POSDET(NACT,NA-1,IACON2,IFA)-1)*NEXT
               IPET = IPET + NA0F + (JJ-NACT)
               IMMC01(ISYMA(IPET)) = IMMC01(ISYMA(IPET)) + 1
               IPOSA01(IMMC01(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
               IPER11 = (-1)**IPER1
               IPERA01(IMMC01(ISYMA(IPET)),ISYMA(IPET)) = IPER11
               IND = INDEX(JJ)+IO1
               IIND101(IMMC01(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
               IF (IS1.NE.IS2) GOTO 800
               ICI1 = ICAT
               ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 805 IK=1,IA-1
                     J1  = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + IACON1(IK)
                     J2  = INDEX(IO1) + IACON1(IK)
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  805             CONTINUE
C
                  DO 807 IK=IA+1,NA
                     J1 = INDEX(IACON1(IK)+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + IACON1(IK)
                     J2 = INDEX(IACON1(IK)) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
  807             CONTINUE
C
C  Loop over all beta dets now. 0th order first.
C
                  DO 810 II=1,NB
                     IBCON1(II) = II
  810             CONTINUE
C
                  NST = 1
                  DO 815 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 813 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
  813                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 818 IK=1,NB
                        J1 = INDEX(IBCON1(IK)+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
  818                CONTINUE
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
  815             CONTINUE
C
C  Loop over 1st order beta strings.
C
                  DO 822 II=1,NB-1
                     IBCON1(II) = II
  822             CONTINUE
                  NSTF = 1
C
                  DO 830 INB1 = ISBS1(ISA1),ISBS2(ISA1)-1
                     NIBP = ISBC(INB1) - NB0F
                     NX = (NIBP-1)/NB1E + 1
                     NY = NIBP - (NX-1)*NB1E
                     DO 824 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-1,NACT)
  824                CONTINUE
                     IBCON1(NB) = NY+NACT
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 826 IK=1,NB-1
                        J1 = INDEX(IBCON1(IK)+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
  826                CONTINUE
C
                     J1 = INDEX(IBCON1(NB)+1)
                     IMA = MAX(IND,J1)
                     IMI = MIN(IND,J1)
                     JJ1 = INDEX(IMA) + IMI
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NSTF = NX
  830             CONTINUE
C
  800          CONTINUE
C
C   Loop double excitations from 0th space.
C
               DO 867 IAA=IA+1,NA
                  IIA = IACON1(IAA)
                  IS3 = IOX(IIA)
                  IPA = IAA - 1
C
C  Loop over external orbitals > JJ, ie AA -> EE
C
                  DO 865 JJAA=JJ+1,NOCC
C
                     IS4 = IOX(JJAA)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 865
C
                     CALL REDE01(IACON2,IBCON1,NA,IPA,JJAA,IPER2)
                     IBCON1(NA-1) = JJ-NACT
                     IBCON1(NA)   = JJAA-NACT
C
                     IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                     IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                     IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                     ICI1 = ICAT
                     ICI2 = ISPA(IPET)
                     IPERT = IPER1 + IPER2
                     IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                     II1 = INDEX(JJAA) + IO1
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(JJ) + IIA
                     INX2 = INDEX(II1) + II2
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 863 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                      AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                      AB(ICI2) = AB(ICI2) + T*CI(ICI1)
  863                CONTINUE
C
  865             CONTINUE
C
  867          CONTINUE
C
  880       CONTINUE
C
  990    CONTINUE
C
C    Loop over all betas and their single excites now.
C
C   First loop over all 0th order betas
C
         DO 1010 II=1,NB
            IBCON1(II) = IACON1(II)
 1010    CONTINUE
C
         DO 1995 KJI=IJK,NB0F
            IPB1 = ISPB(KJI)
            ISB1 = ISYMB(KJI)
            ITBS = ITAB(ISB1)
            IMZZ0 = IMMC00(ITBS)
            IMZZ1 = IMMC01(ITBS)
            IMZZ = IMZZ0 + IMZZ1
            IF (IMZZ.EQ.0.AND.ISB1.NE.ITAS) GOTO 1994
            IC1 = ICAT + IPB1
            QNUM = 1.0D+00
            IF (IJK.EQ.KJI) QNUM=2.0D+00
C
C   Loop single excitations from 0th space.
C
         DO 1990 IB=1,NB
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C   Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 1980 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN=NACT
               DO 1970 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ10 = IMMC00(ITB2)
                  IMZ11 = IMMC01(ITB2)
                  IMZ1 = IMZ10 + IMZ11
C
                  IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 1975
                  IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1975
                  GOTO 1970
C
 1975             CONTINUE
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
                  IPOSB = POSDET(NACT,NB,IBCON2,IFA)
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  ZPERB = IPERB/QNUM
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1960 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA00(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1960                CONTINUE
                     DO 1964 IAT=1,IMZ11
                        IC4 = IPOSA01(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITB2))
                        I2 = MIN(IOB,IIND101(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA01(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1964                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1955 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA00(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1955                CONTINUE
                     DO 1953 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA01(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1953                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1950 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA00(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1950                CONTINUE
                     DO 1958 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        IC4 = IPOSA01(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA01(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1958                CONTINUE
C
                  ENDIF
C
 1970          CONTINUE
 1980       CONTINUE
C
C  Loop over orbitals in external gap, ie  A -> E.
C
            DO 1880 JJ=NACT+1,NOCC
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ10 = IMMC00(ITB2)
               IMZ11 = IMMC01(ITB2)
               IMZ1 = IMZ10 + IMZ11
C
               IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 1875
               IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1875
               GOTO 1880
Cc
 1875          CONTINUE
               CALL REDE01(IBCON1,IBCON2,NB,IB,JJ,IPER1)
               IPOSB = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
               IPOSB = IPOSB + NB0F + (JJ-NACT)
               IPB2 = ISPB(IPOSB)
               IPERB = (-1)**IPER1
               ZPERB = IPERB/QNUM
               IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1860 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA00(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1860                CONTINUE
                     DO 1865 IAT=1,IMZ11
                        IC4 = IPOSA01(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITB2))
                        I2 = MIN(IOB,IIND101(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA01(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1865                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1855 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA00(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1855                CONTINUE
                     DO 1858 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA01(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1858                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1850 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA00(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1850                CONTINUE
                     DO 1853 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        IC4 = IPOSA01(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA01(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1853                CONTINUE
C
                  ENDIF
C
 1880          CONTINUE
C
 1990       CONTINUE
Cc
 1994       CONTINUE
            CALL ADVANC(IBCON1,NB,NACT)
 1995    CONTINUE
C
C  Now loop over 1st order beta strings.
C
         DO 1840 II=1,NB-1
            IBCON1(II) = II
 1840    CONTINUE
         ICOUNT = NB0F
C
         DO 1920 KJI=1,NB1F
            IPOS1 = (KJI-1)*NEXT + NB0F
            DO 1910 KJJ=NACT+1,NOCC
               IBCON1(NB) = KJJ
               ICOUNT = ICOUNT + 1
C
               IPB1 = ISPB(ICOUNT)
               ISB1 = ISYMB(ICOUNT)
               ITBS = ITAB(ISB1)
               IMZZ0 = IMMC00(ITBS)
               IMZZ1 = IMMC01(ITBS)
               IMZZ = IMZZ0 + IMZZ1
               IF (IMZZ.EQ.0.AND.ISB1.NE.ITAS) GOTO 1910
               IC1 = ICAT + IPB1
C
C   Loop single excitations from 0th space.
C
         DO 1905 IB=1,NB-1
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C   Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 1790 IGEL=IB,NB-1
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-1) IEN=NACT
               DO 1780 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ10 = IMMC00(ITB2)
                  IMZ11 = IMMC01(ITB2)
                  IMZ1 = IMZ10 + IMZ11
C
                  IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 1785
                  IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1785
                  GOTO 1780
C
 1785             CONTINUE
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
                  IPOSB = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
                  IPOSB = IPOSB + NB0F + (KJJ-NACT)
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1760 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1760                CONTINUE
                     DO 1764 IAT=1,IMZ11
                        IC4 = IPOSA01(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITB2))
                        I2 = MIN(IOB,IIND101(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1764                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1755 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1755                CONTINUE
                     DO 1758 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1758                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1750 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1750                CONTINUE
                     DO 1753 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        IC4 = IPOSA01(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1753                CONTINUE
C
                  ENDIF
C
 1780          CONTINUE
 1790       CONTINUE
C
C   Loop over orbitals in gaps in external space, ie A -> E.
C
            IGEL = NB-1
            DO 1690 JJ=NACT+1,NOCC
               IF (JJ.EQ.IBCON1(NB)) THEN
                  IGEL=NB
                  GOTO 1690
               ENDIF
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ10 = IMMC00(ITB2)
               IMZ11 = IMMC01(ITB2)
C
               IF (ISB1.EQ.ITAS.AND.IMZ10.NE.0) GOTO 1685
               IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1685
               GOTO 1690
C
 1685          CONTINUE
               CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
               IBCON2(NB-1) = IBCON2(NB-1)-NACT
               IBCON2(NB)   = IBCON2(NB)-NACT
C
               IPET1 = POSDET(NACT,NB-2,IBCON2,IFA)
               IPET2 = POSDET(NEXT,2,IBCON2(NB-1),IFE)
               IPOSB = (IPET1-1)*NB2E + IPET2 + NB2S
               IPB2 = ISPB(IPOSB)
               IPERB = (-1)**IPER1
               IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1660 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1660                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ10.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1655 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1655                CONTINUE
                     DO 1658 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1658                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1650 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1650                CONTINUE
                     DO 1653 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1653                CONTINUE
C
                  ENDIF
C
 1690       CONTINUE
C
 1905    CONTINUE
C
C   Loop single excitations from External space.
C
         IBB = IBCON1(NB)
         IB1 = IOX(IBB)
         IR1 = IMUL(IB1,ISB1)
         IPERB = 1
C
C  Loop over orbitals in gaps in external space, ie E -> E.
C
         DO 1590 JJ=IBB+1,NOCC
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ10 = IMMC00(ITB2)
               IMZ11 = IMMC01(ITB2)
               IMZ1 = IMZ10 + IMZ11
C
               IF (ISB1.EQ.ITAS.AND.IMZ1.NE.0) GOTO 1585
               IF (ISB2.EQ.ITAS.AND.IMZZ.NE.0) GOTO 1585
               GOTO 1590
C
 1585          CONTINUE
               IPOSB = IPOS1 + (JJ-NACT)
               IPB2 = ISPB(IPOSB)
               IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ.EQ.0) THEN
                     DO 1560 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1560                CONTINUE
                     DO 1563 IAT=1,IMZ11
                        IC4 = IPOSA01(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITB2))
                        I2 = MIN(IOB,IIND101(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1563                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ1.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1555 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1555                CONTINUE
                     DO 1554 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1554                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1550 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1550                CONTINUE
                     DO 1558 IAT=1,IMZZ1
                        IC3 = IPOSA01(IAT,ITBS) + IPB1
                        IC4 = IPOSA01(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND101(IAT,ITBS))
                        I2 = MIN(IOB,IIND101(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA01(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1558                CONTINUE
C
                  ENDIF
C
 1590    CONTINUE
C
 1910       CONTINUE
            IF (NB.LE.1) GOTO 1920
            CALL ADVANC(IBCON1,NB-1,NACT)
 1920    CONTINUE
C
C  Now loop over 2nd order beta strings.
C
         IF (NB.LT.2) GOTO 1497
         DO 1495 II=1,NB-2
            IBCON1(II) = II
 1495    CONTINUE
         ICOUNT = NB2S
C
         DO 1490 KJI=1,NB2F
            IBCON1(NB-1) = 1 + NACT
            IBCON1(NB) = 2 + NACT
            DO 1485 KJJ=1,NB2E
               ICOUNT = ICOUNT + 1
C
               IPB1 = ISPB(ICOUNT)
               ISB1 = ISYMB(ICOUNT)
               ITBS = ITAB(ISB1)
               IMZZ0 = IMMC00(ITBS)
               IF (IMZZ0.EQ.0.AND.ISB1.NE.ITAS) GOTO 1487
               IC1 = ICAT + IPB1
C
C  Loop single excitations from 0th space.
C
         DO 1480 IB=1,NB-2
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 1475 IGEL=IB,NB-2
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-2) IEN = NACT
               DO 1470 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ10 = IMMC00(ITB2)
C
                  IF (ISB1.EQ.ITAS.AND.IMZ10.NE.0) GOTO 1465
                  IF (ISB2.EQ.ITAS.AND.IMZZ0.NE.0) GOTO 1465
                  GOTO 1470
C
 1465             CONTINUE
                  CALL REDE00(IBCON1,IBCON2,NB-2,IB,IGEL,JJ,IPER1)
C
                  IPET1 = POSDET(NACT,NB-2,IBCON2,IFA)
                  IPOSB = (IPET1-1)*NB2E + KJJ + NB2S
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ0.EQ.0) THEN
                     DO 1460 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1460                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ10.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1455 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1455                CONTINUE
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1450 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1450                CONTINUE
                  ENDIF
C
 1470          CONTINUE
 1475       CONTINUE
C
 1480    CONTINUE
C
C  Loop single excitations from Excited space.
C
         DO 1400 IB=NB-1,NB
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C  Loop over orbitals in gaps in Excited space, E -> E.
C
            DO 1395 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN = NOCC
               DO 1390 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ10 = IMMC00(ITB2)
C
                  IF (ISB1.EQ.ITAS.AND.IMZ10.NE.0) GOTO 1385
                  IF (ISB2.EQ.ITAS.AND.IMZZ0.NE.0) GOTO 1385
                  GOTO 1390
C
 1385             CONTINUE
                  CALL REDE00(IBCON1(NB-1),IBCON2(NB-1),
     *            2,IB-NB+2,IGEL-NB+2,JJ,IPER1)
C
                  IBCON2(NB-1) = IBCON2(NB-1)-NACT
                  IBCON2(NB)   = IBCON2(NB)-NACT
                  IPET2 = POSDET(NEXT,2,IBCON2(NB-1),IFE)
                  IPOSB = (KJI-1)*NB2E + IPET2 + NB2S
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ0.EQ.0) THEN
                     DO 1360 IAT=1,IMZ10
                        IC4 = IPOSA00(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITB2))
                        I2 = MIN(IOB,IIND100(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 1360                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ10.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 1355 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1355                CONTINUE
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 1350 IAT=1,IMZZ0
                        IC3 = IPOSA00(IAT,ITBS) + IPB1
                        IC4 = IPOSA00(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND100(IAT,ITBS))
                        I2 = MIN(IOB,IIND100(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*IPERB*IPERA00(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 1350                CONTINUE
                  ENDIF
C
 1390          CONTINUE
 1395       CONTINUE
C
 1400   CONTINUE
C
 1487       CONTINUE
            CALL ADVANC(IBCON1(NB-1),2,NOCC)
 1485    CONTINUE
C
         IF (NB.LE.2) GOTO 1490
         CALL ADVANC(IBCON1,NB-2,NACT)
 1490    CONTINUE
 1497    CONTINUE
C
         CALL ADVANC(IACON1,NA,NACT)
 2000 CONTINUE
C
C    --------------------------------------------
C    End of loop over all 0th order alpha strings.
C    --------------------------------------------
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK SINSO11
C     -----------------------------------------------------------
      SUBROUTINE SINSO11(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IBCON2,IFA,IFE,INDEX,AB,IOX,
     *    ITAB,IMUL,ISYMA,ISYMB,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA11,IPERA11,IIND111,IMMC11,
     *    IPOSA12,IPERA12,IIND112,IMMC12)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION SI1(*),SI2(*),AB(NSOCI),CI(NSOCI)
      DIMENSION IFA(0:NACT,0:NACT),IFE(0:NEXT,0:2)
      DIMENSION IACON1(NA),IBCON1(NA),IACON2(NA),IBCON2(NA)
      DIMENSION INDEX((NOCC*(NOCC+1))/2 + 1)
      DIMENSION IOX(NOCC)
      DIMENSION ITAB(NSYM),IMUL(NSYM,NSYM)
      DIMENSION ISYMA(NATT),ISYMB(NATT),ISPA(NATT),ISPB(NATT)
      DIMENSION ISBS0(NSYM+1),ISBS1(NSYM),ISBS2(NSYM)
      DIMENSION ISBC(NATT)
      DIMENSION IPOSA11(((NA-1)*(NACT-NA+1)+(NEXT-1)),NSYM)
      DIMENSION IPERA11(((NA-1)*(NACT-NA+1)+(NEXT-1)),NSYM)
      DIMENSION IIND111(((NA-1)*(NACT-NA+1)+(NEXT-1)),NSYM)
      DIMENSION IMMC11(NSYM)
      DIMENSION IPOSA12((NA-1)*(NEXT-1),NSYM)
      DIMENSION IPERA12((NA-1)*(NEXT-1),NSYM)
      DIMENSION IIND112((NA-1)*(NEXT-1),NSYM)
      DIMENSION IMMC12(NSYM)
C
C    --------------------------------------------
C    Loop over all 1st order alpha strings.
C    --------------------------------------------
C
      DO 3995 II=1,NA-1
         IACON1(II)=II
 3995 CONTINUE
      ICOUNT = NA0F
C
      DO 3993 IJK=1,NA1F
         IPOS1 = (IJK-1)*NEXT + NA0F
         DO 3990 KJI=NACT+1,NOCC
            IACON1(NA) = KJI
            ICOUNT = ICOUNT + 1
            ICAT = ISPA(ICOUNT)
            ISA1 = ISYMA(ICOUNT)
            ITAS = ITAB(ISA1)
C
            DO 3005 II=1,NSYM
               IMMC11(II) = 0
               IMMC12(II) = 0
 3005       CONTINUE
C
C   Loop single excitations from 0th space.
C
         DO 3985 IA=1,NA-1
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 3980 IGEL=IA,NA-1
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA-1) IEN = NACT
               DO 3975 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
C
C  Storage here for later.
C
                  IPET = (POSDET(NACT,NA-1,IACON2,IFA)-1)*NEXT
                  IPET = IPET + NA0F + (KJI-NACT)
                  IMMC11(ISYMA(IPET)) = IMMC11(ISYMA(IPET)) + 1
                  IPOSA11(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
                  IPER11 = (-1)**IPER1
                  IPERA11(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = IPER11
                  IND = INDEX(JJ)+IO1
                  IIND111(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 3970
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 3905 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3905             CONTINUE
C
                  DO 3907 IK=IA+1,IGEL
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3907             CONTINUE
C
                  DO 3908 IK=IGEL+1,NA
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3908             CONTINUE
C
C  Loop over 0th and 1st beta strings now. 0th order first.
C
                  DO 3910 II=1,NB
                     IBCON1(II) = II
 3910             CONTINUE
C
                  NST = 1
                  DO 3915 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 3913 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 3913                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3918 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 3918                CONTINUE
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
 3915             CONTINUE
C
C  Loop over 1st order beta strings.
C
                  DO 3922 II=1,NB-1
                     IBCON1(II) = II
 3922             CONTINUE
                  NSTF = 1
C
                  DO 3930 INB1 = ISBS1(ISA1),ISBS2(ISA1)-1
                     NIBP = ISBC(INB1) - NB0F
                     NX = (NIBP-1)/NB1E + 1
                     NY = NIBP - (NX-1)*NB1E
                     DO 3924 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-1,NACT)
 3924                CONTINUE
                     IBCON1(NB) = NY+NACT
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3926 IK=1,NB-1
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 3926                CONTINUE
C
                     ION = IBCON1(NB)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NSTF = NX
 3930             CONTINUE
C
 3970             CONTINUE
C
C   Loop double excitations from 0th space.
C
                  DO 3967 IAA=IA+1,NA-1
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
                     IF (JJ.GT.IIA) IPA=IPA-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA
C
                     DO 3965 IGEL2=IGEL,NA-1
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA-1) IENAA=NACT
                        DO 3963 JJAA=ISTAA,IENAA
C
                        IS4 = IOX(JJAA)
                        IP2 = IMUL(IS3,IS4)
                        IF (IP1.NE.IP2) GOTO 3963
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                        IPET = POSDET(NACT,NA-1,IBCON1,IFA)
                        IPET = (IPET-1)*NEXT + KJI-NACT + NA0F
                        ICI1 = ICAT
                        ICI2 = ISPA(IPET)
                        IPERT = IPER1 + IPER2
                        IPERT = (-1)**IPERT
C
C   Make matrix element.
C
                        II1 = INDEX(JJAA) + IO1
                        IF (IIA.GT.JJAA) THEN
                           I2 = INDEX(IIA) + JJAA
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIA) + JJ
                           INX2 = INDEX(II2) + II1
                        ELSEIF (IIA.LT.JJ) THEN
                           I2 = INDEX(JJAA) + IIA
                           INX = INDEX(I2) + IND
                           II2 = INDEX(JJ) + IIA
                           INX2 = INDEX(II1) + II2
                        ELSE
                           I2 = INDEX(JJAA) + IIA
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIA) + JJ
                           INX2 = INDEX(II1) + II2
                        ENDIF
                        C = SI2(INX) - SI2(INX2)
                        T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 3954 INB1 = ISBS0(ISA1),ISBS2(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                         AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                         AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 3954                   CONTINUE
C
 3963                   CONTINUE
 3965                CONTINUE
C
C  Loop over orbitals in gaps in external space, ie AA -> AE
C
                     IGEL2 = NA-1
                     DO 3959 JJAA=NACT+1,NOCC
                     IF (JJAA.EQ.IACON1(NA)) THEN
                        IGEL2=NA
                        GOTO 3959
                     ENDIF
C
                     IS4 = IOX(JJAA)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 3959
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
                     IBCON1(NA-1) = IBCON1(NA-1)-NACT
                     IBCON1(NA)   = IBCON1(NA)-NACT
C
                     IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                     IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                     IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                     ICI1 = ICAT
                     ICI2 = ISPA(IPET)
                     IPERT = IPER1 + IPER2
                     IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                     II1 = INDEX(JJAA) + IO1
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     IF (IIA.LT.JJ) THEN
                        II2 = INDEX(JJ) + IIA
                        INX2 = INDEX(II1) + II2
                     ELSE
                        II2 = INDEX(IIA) + JJ
                        INX2 = INDEX(II1) + II2
                     ENDIF
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 3944 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                     AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                     AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 3944                CONTINUE
C
 3959                CONTINUE
C
 3967             CONTINUE
C
C   Loop double excitations, 2nd excitation from external space.
C
               IAA = NA
               IIA = IACON1(IAA)
               IS3 = IOX(IIA)
               IPA = IAA
C
C Loop over orbitals in gaps in 0th space, ie AE -> AA
C
               DO 3955 IGEL2=IGEL,NA-1
                  ISTAA = IACON1(IGEL2)+1
                  IENAA = IACON1(IGEL2+1)-1
                  IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                  IF (IGEL2.EQ.NA-1) IENAA=NACT
                  DO 3957 JJAA=ISTAA,IENAA
C
                     IS4 = IOX(JJAA)
                     IP2 = IMUL(IS3,IS4)
                     IF (IP1.NE.IP2) GOTO 3957
C
                  CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                     IPET = POSDET(NACT,NA,IBCON1,IFA)
                     ICI1 = ICAT
                     ICI2 = ISPA(IPET)
                     IPERT = IPER1 + IPER2
                     IPERT = (-1)**IPERT
C
                     II1 = INDEX(JJAA) + IO1
                     I2 = INDEX(IIA) + JJAA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIA) + JJ
                     INX2 = INDEX(II2) + II1
                     C = SI2(INX) - SI2(INX2)
                     T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 3934 INB1 = ISBS0(ISA1),ISBS2(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                     AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                     AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 3934                CONTINUE
C
 3957             CONTINUE
 3955          CONTINUE
C
C  Loop over orbitals in gaps in external space, ie AE -> AE
C
               IGEL2 = NA-1
               DO 3953 JJAA=NACT+1,NOCC
                  IF (JJAA.EQ.IIA) THEN
                     IGEL2 = NA
                     GOTO 3953
                  ENDIF
C
                  IS4 = IOX(JJAA)
                  IP2 = IMUL(IS3,IS4)
                  IF (IP1.NE.IP2) GOTO 3953
C
              CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                  IPET = POSDET(NACT,NA-1,IBCON1,IFA)
                  IPET = (IPET-1)*NEXT + JJAA-NACT + NA0F
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
                  IPERT = IPER1 + IPER2
                  IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                  II1 = INDEX(JJAA) + IO1
                  II2 = INDEX(IIA) + JJ
                  IF (IIA.GT.JJAA) THEN
                     I2 = INDEX(IIA) + JJAA
                     INX = INDEX(I2) + IND
                     INX2 = INDEX(II2) + II1
                  ELSE
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     INX2 = INDEX(II1) + II2
                  ENDIF
                  C = SI2(INX) - SI2(INX2)
                  T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 3951 INB1 = ISBS0(ISA1),ISBS2(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                     AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                     AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 3951                CONTINUE
C
 3953          CONTINUE
C
 3975          CONTINUE
 3980       CONTINUE
C
C  Loop over orbitals in gaps in external space, A -> E.
C
            IGEL = NA-1
            DO 3895 JJ=NACT+1,NOCC
               IF (JJ.EQ.IACON1(NA)) THEN
                  IGEL = NA
                  GOTO 3895
               ENDIF
               IS2 = IOX(JJ)
               IP1 = IMUL(IS2,IS1)
C
               CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
               IACON2(NA-1) = IACON2(NA-1)-NACT
               IACON2(NA) = IACON2(NA)-NACT
C
C  Storage here for later.
C
               IPET1 = POSDET(NACT,NA-2,IACON2,IFA)
               IPET2 = POSDET(NEXT,2,IACON2(NA-1),IFE)
               IPET = (IPET1-1)*NA2E + IPET2 + NA2S
               IMMC12(ISYMA(IPET)) = IMMC12(ISYMA(IPET)) + 1
               IPOSA12(IMMC12(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
               IPER11 = (-1)**IPER1
               IPERA12(IMMC12(ISYMA(IPET)),ISYMA(IPET)) = IPER11
               IND = INDEX(JJ)+IO1
               IIND112(IMMC12(ISYMA(IPET)),ISYMA(IPET)) = IND
C
               IACON2(NA-1) = IACON2(NA-1)+NACT
               IACON2(NA) = IACON2(NA)+NACT
C
C  Check if excited string is correct symmetry.
C
               IF (IS1.NE.IS2) GOTO 3870
               ICI1 = ICAT
               ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 3805 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3805             CONTINUE
C
                  DO 3807 IK=IA+1,IGEL
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3807             CONTINUE
C
                  DO 3808 IK=IGEL+1,NA
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3808             CONTINUE
C
C  Loop over all 0th beta dets now.
C
                  DO 3810 II=1,NB
                     IBCON1(II) = II
 3810             CONTINUE
C
                  NST = 1
                  DO 3815 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 3813 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 3813                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3818 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 3818                CONTINUE
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
 3815             CONTINUE
C
 3870          CONTINUE
C
C   Loop double excitations, AE -> EE
C
               IAA = NA
               IIA = IACON1(IAA)
               IS3 = IOX(IIA)
               IPA = IAA
               IF (JJ.GT.IIA) IPA=IPA-1
C
C Loop over orbitals in gaps in external space > JJ, AE -> EE
C
               IGEL2 = IGEL
               DO 3865 JJAA=JJ+1,NOCC
                  IF (JJAA.EQ.IIA) THEN
                     IGEL2=NA
                     GOTO 3865
                  ENDIF
C
                  IS4 = IOX(JJAA)
                  IP2 = IMUL(IS3,IS4)
                  IF (IP1.NE.IP2) GOTO 3865
C
             CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
                  IBCON1(NA-1) = IBCON1(NA-1)-NACT
                  IBCON1(NA)   = IBCON1(NA)-NACT
C
                  IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                  IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                  IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
                  IPERT = IPER1 + IPER2
                  IPERT = (-1)**IPERT
C
C Make matrix element.
C
                  II1 = INDEX(JJAA) + IO1
                  IF (IIA.GT.JJAA) THEN
                     I2 = INDEX(IIA) + JJAA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIA) + JJ
                     INX2 = INDEX(II2) + II1
                  ELSEIF (IIA.LT.JJ) THEN
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(JJ) + IIA
                     INX2 = INDEX(II1) + II2
                  ELSE
                     I2 = INDEX(JJAA) + IIA
                     INX = INDEX(I2) + IND
                     II2 = INDEX(IIA) + JJ
                     INX2 = INDEX(II1) + II2
                  ENDIF
                  C = SI2(INX) - SI2(INX2)
                  T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                     DO 3851 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                        ICI1 = ICI1 + 1
                        ICI2 = ICI2 + 1
                     AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                     AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 3851                CONTINUE
C
 3865          CONTINUE
C
 3895       CONTINUE
C
 3985    CONTINUE
C
C  Loop single excitation from external space.
C
         IA = NA
         IO1 = IACON1(NA)
         IS1 = IOX(IO1)
         IPER11 = 1
C
C  Loop over orbitals in external space, E -> E.
C
         DO 3795 JJ=IO1+1,NOCC
            IS2 = IOX(JJ)
            IP1 = IMUL(IS2,IS1)
C
            CALL REDE01(IACON1,IACON2,NA,IA,JJ,IPER1)
C
C  Storage here for later.
C
            IPET = IPOS1 + (JJ-NACT)
            IMMC11(ISYMA(IPET)) = IMMC11(ISYMA(IPET)) + 1
            IPOSA11(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
            IPERA11(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = IPER11
            IND = INDEX(JJ)+IO1
            IIND111(IMMC11(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
            IF (IS1.NE.IS2) GOTO 3770
            ICI1 = ICAT
            ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 3705 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 3705             CONTINUE
C
C  Loop over all 0th beta dets now.
C
                  DO 3710 II=1,NB
                     IBCON1(II) = II
 3710             CONTINUE
C
                  NST = 1
                  DO 3715 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 3713 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 3713                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3718 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 3718                CONTINUE
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
 3715             CONTINUE
C
C  Loop over 1st order beta strings.
C
                  DO 3722 II=1,NB-1
                     IBCON1(II) = II
 3722             CONTINUE
                  NSTF = 1
C
                  DO 3730 INB1 = ISBS1(ISA1),ISBS2(ISA1)-1
                     NIBP = ISBC(INB1) - NB0F
                     NX = (NIBP-1)/NB1E + 1
                     NY = NIBP - (NX-1)*NB1E
                     DO 3724 KK=NSTF,NX-1
                        CALL ADVANC(IBCON1,NB-1,NACT)
 3724                CONTINUE
                     IBCON1(NB) = NY+NACT
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 3726 IK=1,NB-1
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 3726                CONTINUE
C
                     ION = IBCON1(NB)
                     J1 = INDEX(ION+1)
                     IMA = MAX(IND,J1)
                     IMI = MIN(IND,J1)
                     JJ1 = INDEX(IMA) + IMI
                     D = D + SI2(JJ1)
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NSTF = NX
 3730             CONTINUE
C
 3770       CONTINUE
C
 3795    CONTINUE
C
C    Loop over all relevant betas and their relevant single excites now.
C
C  Now loop over 1st order beta strings.
C
         DO 3020 II=1,NB-1
            IBCON1(II) = IACON1(II)
 3020    CONTINUE
         ICOUNTB = ICOUNT - 1
C
         DO 3500 KJIH=IJK,NB1F
            IPOSB1 = (KJIH-1)*NEXT + NB0F
            KJS = NACT+1
            IF (KJIH.EQ.IJK) KJS = KJI
            DO 3505 KJJ=KJS,NOCC
               IBCON1(NB) = KJJ
               ICOUNTB = ICOUNTB + 1
C
               IPB1 = ISPB(ICOUNTB)
               ISB1 = ISYMB(ICOUNTB)
               ITBS = ITAB(ISB1)
               IMZZ1 = IMMC11(ITBS)
               IF (IMZZ1.EQ.0.AND.ISB1.NE.ITAS) GOTO 3505
               IC1 = ICAT + IPB1
               QNUM=1.0D+00
               IF (ICOUNT.EQ.ICOUNTB) QNUM=2.0D+00
C
C   Loop single excitations from 0th space.
C
         DO 3510 IB=1,NB-1
            IBB = IBCON1(IB)
            IB1 = IOX(IBB)
            IR1 = IMUL(IB1,ISB1)
C
C  Loop over orbitals in gaps in 0th space, ie A -> A (B1 -> B1)
C
            DO 3515 IGEL=IB,NB-1
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-1) IEN=NACT
               DO 3520 JJ=IST,IEN
                  IB2 = IOX(JJ)
                  ISB2 = IMUL(IR1,IB2)
                  ITB2 = ITAB(ISB2)
                  IMZ11 = IMMC11(ITB2)
C
                  IF (ISB1.EQ.ITAS.AND.IMZ11.NE.0) GOTO 3523
                  IF (ISB2.EQ.ITAS.AND.IMZZ1.NE.0) GOTO 3523
                  GOTO 3520
C
 3523             CONTINUE
C
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
                  IPOSB = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
                  IPOSB = IPOSB + NB0F + (KJJ-NACT)
                  IPB2 = ISPB(IPOSB)
                  IPERB = (-1)**IPER1
                  ZPERB = IPERB/QNUM
                  IOB = INDEX(JJ)+IBB
C
                  IF (ISB2.NE.ITAS.OR.IMZZ1.EQ.0) THEN
                     DO 3526 IAT=1,IMZ11
                        IC4 = IPOSA11(IAT,ITB2) + IPB2
                        I1 = MAX(IOB,IIND111(IAT,ITB2))
                        I2 = MIN(IOB,IIND111(IAT,ITB2))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA11(IAT,ITB2)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
 3526                CONTINUE
C
                  ELSE IF(ISB1.NE.ITAS.OR.IMZ11.EQ.0) THEN
                     IC2 = ICAT + IPB2
                     DO 3530 IAT=1,IMZZ1
                        IC3 = IPOSA11(IAT,ITBS) + IPB1
                        I1 = MAX(IOB,IIND111(IAT,ITBS))
                        I2 = MIN(IOB,IIND111(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA11(IAT,ITBS)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 3530                CONTINUE
C
                  ELSE
                     IC2 = ICAT + IPB2
                     DO 3532 IAT=1,IMZZ1
                        IC3 = IPOSA11(IAT,ITBS) + IPB1
                        IC4 = IPOSA11(IAT,ITBS) + IPB2
                        I1 = MAX(IOB,IIND111(IAT,ITBS))
                        I2 = MIN(IOB,IIND111(IAT,ITBS))
                        IX = INDEX(I1) + I2
                        C = SI2(IX)*ZPERB*IPERA11(IAT,ITBS)
                           AB(IC1) = AB(IC1) + C*CI(IC4)
                           AB(IC4) = AB(IC4) + C*CI(IC1)
                           AB(IC2) = AB(IC2) + C*CI(IC3)
                           AB(IC3) = AB(IC3) + C*CI(IC2)
 3532                CONTINUE
C
                  ENDIF
C
 3520          CONTINUE
 3515       CONTINUE
C
 3510    CONTINUE
C
C   Loop single excitations from External space.
C
         IBB = IBCON1(NB)
         IB1 = IOX(IBB)
         IR1 = IMUL(IB1,ISB1)
         ZPERB = 1.0D+00/QNUM
C
C  Loop over orbitals in gaps in external space, ie E -> E, (B1 -> B1)
C
         DO 3550 JJ=IBB+1,NOCC
               IB2 = IOX(JJ)
               ISB2 = IMUL(IR1,IB2)
               ITB2 = ITAB(ISB2)
               IMZ11 = IMMC11(ITB2)
C
               IF (ISB1.EQ.ITAS.AND.IMZ11.NE.0) GOTO 3555
               IF (ISB2.EQ.ITAS.AND.IMZZ1.NE.0) GOTO 3555
               GOTO 3550
C
 3555          CONTINUE
C
               IPOSB = IPOSB1 + (JJ-NACT)
               IPB2 = ISPB(IPOSB)
               IOB = INDEX(JJ)+IBB
C
               IF (ISB2.NE.ITAS.OR.IMZZ1.EQ.0) THEN
                  DO 3560 IAT=1,IMZ11
                     IC4 = IPOSA11(IAT,ITB2) + IPB2
                     I1 = MAX(IOB,IIND111(IAT,ITB2))
                     I2 = MIN(IOB,IIND111(IAT,ITB2))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*ZPERB*IPERA11(IAT,ITB2)
                        AB(IC1) = AB(IC1) + C*CI(IC4)
                        AB(IC4) = AB(IC4) + C*CI(IC1)
 3560             CONTINUE
C
               ELSE IF(ISB1.NE.ITAS.OR.IMZ11.EQ.0) THEN
                  IC2 = ICAT + IPB2
                  DO 3565 IAT=1,IMZZ1
                     IC3 = IPOSA11(IAT,ITBS) + IPB1
                     I1 = MAX(IOB,IIND111(IAT,ITBS))
                     I2 = MIN(IOB,IIND111(IAT,ITBS))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*ZPERB*IPERA11(IAT,ITBS)
                        AB(IC2) = AB(IC2) + C*CI(IC3)
                        AB(IC3) = AB(IC3) + C*CI(IC2)
 3565             CONTINUE
C
               ELSE
                  IC2 = ICAT + IPB2
                  DO 3573 IAT=1,IMZZ1
                     IC3 = IPOSA11(IAT,ITBS) + IPB1
                     IC4 = IPOSA11(IAT,ITBS) + IPB2
                     I1 = MAX(IOB,IIND111(IAT,ITBS))
                     I2 = MIN(IOB,IIND111(IAT,ITBS))
                     IX = INDEX(I1) + I2
                     C = SI2(IX)*ZPERB*IPERA11(IAT,ITBS)
                        AB(IC1) = AB(IC1) + C*CI(IC4)
                        AB(IC4) = AB(IC4) + C*CI(IC1)
                        AB(IC2) = AB(IC2) + C*CI(IC3)
                        AB(IC3) = AB(IC3) + C*CI(IC2)
 3573             CONTINUE
C
               ENDIF
C
 3550    CONTINUE
C
 3505       CONTINUE
            IF (NB.LE.1) GOTO 3500
            CALL ADVANC(IBCON1,NB-1,NACT)
 3500    CONTINUE
C
 3990    CONTINUE
         CALL ADVANC(IACON1,NA-1,NACT)
 3993 CONTINUE
C
C    --------------------------------------------
C    End of loop over all 1st order alpha strings.
C    --------------------------------------------
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK SINSO21
C     -----------------------------------------------------------
      SUBROUTINE SINSO21(SI1,SI2,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     *    IACON1,IBCON1,IACON2,IFA,IFE,INDEX,AB,Q,IOX,
     *    IMUL,ISYMA,ISPA,ISPB,ISBS0,
     *    ISBS1,ISBS2,ISBC,NSYM,
     *    IPOSA22,IPERA22,IIND122,IMMC22,ISPIN,IHMCON)
C     -----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION SI1(*),SI2(*),AB(NSOCI),CI(NSOCI)
      DIMENSION Q(NSOCI)
      DIMENSION IFA(0:NACT,0:NACT),IFE(0:NEXT,0:2)
      DIMENSION IACON1(NA),IBCON1(NA),IACON2(NA)
      DIMENSION INDEX((NOCC*(NOCC+1))/2 + 1)
      DIMENSION IOX(NOCC)
      DIMENSION IMUL(NSYM,NSYM)
      DIMENSION ISYMA(NATT),ISPA(NATT),ISPB(NATT)
      DIMENSION ISBS0(NSYM+1),ISBS1(NSYM),ISBS2(NSYM)
      DIMENSION ISBC(NATT)
      DIMENSION IPOSA22(((NA-2)*(NACT-NA+2)+2*(NEXT-2)),NSYM)
      DIMENSION IPERA22(((NA-2)*(NACT-NA+2)+2*(NEXT-2)),NSYM)
      DIMENSION IIND122(((NA-2)*(NACT-NA+2)+2*(NEXT-2)),NSYM)
      DIMENSION IMMC22(NSYM)
      DIMENSION ISPIN(*),IHMCON(1)
C
C    --------------------------------------------
C    Loop over all 2nd order alpha strings.
C    --------------------------------------------
C
      DO 4995 II=1,NA-2
         IACON1(II) = II
 4995 CONTINUE
      ICOUNT = NA2S
C
      DO 4993 IJK = 1,NA2F
         IACON1(NA-1) = 1 + NACT
         IACON1(NA) = 2 + NACT
         DO 4990 KJI=1,NA2E
            ICOUNT = ICOUNT + 1
            ICAT = ISPA(ICOUNT)
            ISA1 = ISYMA(ICOUNT)
C
            DO 4005 II=1,NSYM
               IMMC22(II) = 0
 4005       CONTINUE
C
C   Loop single excitations from 0th space.
C
         DO 4985 IA=1,NA-2
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in zeroth order space, A -> A.
C
            DO 4980 IGEL=IA,NA-2
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA-2) IEN = NACT
               DO 4975 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
C
C  Storage here for later.
C
                  IPET1 = POSDET(NACT,NA-2,IACON2,IFA)
                  IPET2 = KJI
                  IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                  IMMC22(ISYMA(IPET)) = IMMC22(ISYMA(IPET)) + 1
                  IPOSA22(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
                  IPER11 = (-1)**IPER1
                  IPERA22(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = IPER11
                  IND = INDEX(JJ)+IO1
                  IIND122(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = IND
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 4970
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 4905 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4905             CONTINUE
C
                  DO 4907 IK=IA+1,IGEL
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4907             CONTINUE
C
                  DO 4908 IK=IGEL+1,NA
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4908             CONTINUE
C
C  Loop over all 0th beta dets now.
C
                  DO 4910 II=1,NB
                     IBCON1(II) = II
 4910             CONTINUE
C
                  NST = 1
                  DO 4915 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 4913 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 4913                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 4918 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        IMA = MAX(J1,IND)
                        IMI = MIN(J1,IND)
                        JJ1 = INDEX(IMA) + IMI
                        D = D + SI2(JJ1)
 4918                CONTINUE
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
 4915             CONTINUE
C
 4970             CONTINUE
C
C  Loop double excitations from 0th space.
C
                  DO 4967 IAA=IA+1,NA-2
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
                     IF (JJ.GT.IIA) IPA=IPA-1
C
C  Loop over orbitals in gaps in 0th space > JJ, ie AA -> AA.
C
                     DO 4965 IGEL2=IGEL,NA-2
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA-2) IENAA=NACT
                        DO 4963 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 4963
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                           IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                           IPET2 = KJI
                           IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = IPER1 + IPER2
                           IPERT = (-1)**IPERT
C
C   Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           IF (IIA.GT.JJAA) THEN
                              I2 = INDEX(IIA) + JJAA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIA.LT.JJ) THEN
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIA
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 4966 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                      AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                      AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 4966                   CONTINUE
C
 4963                   CONTINUE
 4965                CONTINUE
C
 4967             CONTINUE
C
C  Loop double excitations, 2nd excitation from external space.
C
                  DO 4957 IAA=NA-1,NA
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
C
C  Loop over orbitals in gaps in 0th space, ie AE -> AA
C
                     DO 4955 IGEL2=IGEL,NA-2
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA-2) IENAA=NACT
                        DO 4953 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 4953
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
C
                           IORB = IBCON1(NA)-NACT
                           IPET = POSDET(NACT,NA-1,IBCON1,IFA)
                           IPET = (IPET-1)*NEXT + IORB + NA0F
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = IPER1 + IPER2
                           IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           I2 = INDEX(IIA) + JJAA
                           INX = INDEX(I2) + IND
                           II2 = INDEX(IIA) + JJ
                           INX2 = INDEX(II2) + II1
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 4956 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                      AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                      AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 4956                   CONTINUE
C
 4953                   CONTINUE
 4955                CONTINUE
C
C  Loop over orbitals in gaps in excited space, ie AE -> AE.
C
                     DO 4951 IGEL2=NA-2,NA
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.NA-2) ISTAA=NACT+1
                        IF (IGEL2.EQ.NA) IENAA=NOCC
                        DO 4949 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 4949
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
                           IBCON1(NA-1) = IBCON1(NA-1)-NACT
                           IBCON1(NA)   = IBCON1(NA)-NACT
C
                           IPET1 = POSDET(NACT,NA-2,IBCON1,IFA)
                           IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                           IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = IPER1 + IPER2
                           IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           IF (IIA.GT.JJAA) THEN
                              I2 = INDEX(IIA) + JJAA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSE
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 4946 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                      AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                      AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 4946                   CONTINUE
C
 4949                   CONTINUE
 4951                CONTINUE
C
 4957             CONTINUE
C
 4975          CONTINUE
 4980       CONTINUE
C
 4985    CONTINUE
C
C  Loop single excitations from excited space.
C
         DO 4785 IA=NA-1,NA
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in excited space, E -> E.
C
            DO 4780 IGEL=IA,NA
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA) IEN=NOCC
               DO 4775 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
                  IP1 = IMUL(IS2,IS1)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
                  IACON2(NA-1) = IACON2(NA-1)-NACT
                  IACON2(NA) = IACON2(NA)-NACT
C
C  Storage here for later.
C
                  IPET1 = IJK
                  IPET2 = POSDET(NEXT,2,IACON2(NA-1),IFE)
                  IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                  IMMC22(ISYMA(IPET)) = IMMC22(ISYMA(IPET)) + 1
                  IPOSA22(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = ISPA(IPET)
                  IPER11 = (-1)**IPER1
                  IPERA22(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = IPER11
                  IND = INDEX(JJ)+IO1
                  IIND122(IMMC22(ISYMA(IPET)),ISYMA(IPET)) = IND
C
                  IACON2(NA-1) = IACON2(NA-1)+NACT
                  IACON2(NA) = IACON2(NA)+NACT
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 4770
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C  Make singly excited matrix elements.
C
                  C = SI1(IND)
C
                  DO 4705 IK=1,IA-1
                     ION = IACON1(IK)
                     J1  = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1  = INDEX(JJ) + ION
                     J2  = INDEX(IO1) + ION
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4705             CONTINUE
C
                  DO 4707 IK=IA+1,IGEL
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(IND) + J1
                     J1 = INDEX(JJ) + ION
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4707             CONTINUE
C
                  DO 4708 IK=IGEL+1,NA
                     ION = IACON1(IK)
                     J1 = INDEX(ION+1)
                     JJ1 = INDEX(J1) + IND
                     J1 = INDEX(ION) + JJ
                     J2 = INDEX(ION) + IO1
                     INX = INDEX(J1) + J2
                     C = C + SI2(JJ1) - SI2(INX)
 4708             CONTINUE
C
C  Loop over all 0th beta dets now.
C
                  DO 4710 II=1,NB
                     IBCON1(II) = II
 4710             CONTINUE
C
                  NST = 1
                  DO 4715 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
                     NEND = ISBC(INB1)
                     DO 4713 KK=NST,NEND-1
                        CALL ADVANC(IBCON1,NB,NACT)
 4713                CONTINUE
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     D = 0.0D+00
                     DO 4718 IK=1,NB
                        ION = IBCON1(IK)
                        J1 = INDEX(ION+1)
                        JJ1 = INDEX(IND) + J1
                        D = D + SI2(JJ1)
 4718                CONTINUE
C
                     T = (C+D)*IPER11
                        AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                        AB(ICI2) = AB(ICI2) + T*CI(ICI1)
C
                     NST = NEND
 4715             CONTINUE
C
 4770             CONTINUE
C
C   Loop double excitations from excited space, ie EE -> EE.
C
                  DO 4767 IAA=IA+1,NA
                     IIA = IACON1(IAA)
                     IS3 = IOX(IIA)
                     IPA = IAA
                     IF (JJ.GT.IIA) IPA=IPA-1
C
                     DO 4765 IGEL2=IGEL,NA
                        ISTAA = IACON1(IGEL2)+1
                        IENAA = IACON1(IGEL2+1)-1
                        IF (IGEL2.EQ.IGEL) ISTAA=JJ+1
                        IF (IGEL2.EQ.NA) IENAA=NOCC
                        DO 4763 JJAA=ISTAA,IENAA
C
                           IS4 = IOX(JJAA)
                           IP2 = IMUL(IS3,IS4)
                           IF (IP1.NE.IP2) GOTO 4763
C
                     CALL REDE00(IACON2,IBCON1,NA,IPA,IGEL2,JJAA,IPER2)
                           IBCON1(NA-1) = IBCON1(NA-1)-NACT
                           IBCON1(NA)   = IBCON1(NA)-NACT
C
                           IPET1 = IJK
                           IPET2 = POSDET(NEXT,2,IBCON1(NA-1),IFE)
                           IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                           ICI1 = ICAT
                           ICI2 = ISPA(IPET)
                           IPERT = IPER1 + IPER2
                           IPERT = (-1)**IPERT
C
C  Make matrix element.
C
                           II1 = INDEX(JJAA) + IO1
                           IF (IIA.GT.JJAA) THEN
                              I2 = INDEX(IIA) + JJAA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II2) + II1
                           ELSEIF (IIA.LT.JJ) THEN
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(JJ) + IIA
                              INX2 = INDEX(II1) + II2
                           ELSE
                              I2 = INDEX(JJAA) + IIA
                              INX = INDEX(I2) + IND
                              II2 = INDEX(IIA) + JJ
                              INX2 = INDEX(II1) + II2
                           ENDIF
                           C = SI2(INX) - SI2(INX2)
                           T = C*IPERT
C
C  Loop over beta strings of the right symmetry.
C
                        DO 4746 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                           ICI1 = ICI1 + 1
                           ICI2 = ICI2 + 1
                      AB(ICI1) = AB(ICI1) + T*CI(ICI2)
                      AB(ICI2) = AB(ICI2) + T*CI(ICI1)
 4746                   CONTINUE
C
 4763                   CONTINUE
 4765                CONTINUE
C
 4767             CONTINUE
C
 4775          CONTINUE
 4780       CONTINUE
C
 4785    CONTINUE
C
            CALL ADVANC(IACON1(NA-1),2,NOCC)
 4990    CONTINUE
         CALL ADVANC(IACON1,NA-2,NACT)
 4993 CONTINUE
C
      DO 1111 II=1,NATT
         ISA1 = ISYMA(II)
         ICIT = ISPA(II)
         INB = ISPB(II)
         IENDO = ISBS1(ISA1)-1
         IF (II.GT.NA0F.AND.II.LE.NA2S) IENDO = ISBS2(ISA1)-1
         DO 2222 INB1=ISBS0(ISA1),IENDO
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
C
C   Now for the diagonal contributions
C
      DO 9119 IJK = 1,NSOCI
         AB(IJK) = AB(IJK) + Q(IJK)*CI(IJK)
 9119 CONTINUE
C
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK REDE00
C     ---------------------------------------------------
      SUBROUTINE REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER)
C     ---------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IACON1(NA),IACON2(NA)
C
      IF (IGEL.GE.IA) THEN
C
         DO 100 II=1,(IA-1)
            IACON2(II) = IACON1(II)
  100    CONTINUE
         DO 200 II=IA,(IGEL-1)
            IACON2(II) = IACON1(II+1)
  200    CONTINUE
         IACON2(IGEL) = JJ
         DO 300 II=IGEL+1,NA
            IACON2(II) = IACON1(II)
  300    CONTINUE
         IPER = IGEL-IA
C
      ELSE
C
         DO 500 II=1,IGEL
            IACON2(II) = IACON1(II)
  500    CONTINUE
         IACON2(IGEL+1) = JJ
         DO 600 II=IGEL+2,IA
            IACON2(II) = IACON1(II-1)
  600    CONTINUE
         DO 700 II=IA+1,NA
            IACON2(II) = IACON1(II)
  700    CONTINUE
         IPER = (IA-IGEL-1)
C
      ENDIF
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK REDE01
C     ---------------------------------------------------
      SUBROUTINE REDE01(IACON1,IACON2,NA,IA,JJ,IPER)
C     ---------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IACON1(NA),IACON2(NA)
C
      DO 100 II=1,(IA-1)
         IACON2(II) = IACON1(II)
  100 CONTINUE
      DO 200 II=IA,NA-1
         IACON2(II) = IACON1(II+1)
  200 CONTINUE
      IACON2(NA) = JJ
      IPER = NA - IA
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK PRISO
C     ----------------------------------------------------------
      SUBROUTINE PRISO(IW,CI,AB,EL,IACON,IBCON,
     *      NSYM,ISYMA,ISPA,ISPB,ISBS0,ISBS1,ISBS2,ISBC)
C     ----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXRT=100)
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
      COMMON /DESOCI/ OSPIN(MXRT),NEXT,MOSET,MAXPSO,KSO,NSOCI
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,S,SZ,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORB,
     *                NA,NB,K,KST,IROOT,IPURES,MAXW1,NITER,MAXP,NCI,
     *                IGPDET,KSTSYM,NFTGCI
      COMMON /ENRGYS/ ENUCR,EELCT,ETOT,STOT,SSQUAR,ECORE,ESCF,EERD,
     *                E1,E2,VEN,VEE,EPOT,EKIN,ESTATE(MXRT),STATN
C
      DIMENSION CI(NSOCI,KSO),AB(NSOCI,KSO)
      DIMENSION EL(KSO)
      DIMENSION ISYMA(NATT)
      DIMENSION ISPA(NATT),ISPB(NBTT)
      DIMENSION ISBS0(NSYM+1),ISBS1(NSYM),ISBS2(NSYM)
      DIMENSION ISBC(NBTT)
      DIMENSION IACON(NA),IBCON(NA)
C
      CHARACTER*200 CONA,CONB
C
C     ----------- print the determinant based SOCI eigenvectors -------
C
      NOCC = NACT + NEXT
      WRITE(IW,9140) GRPDET
C
      ECONST = ECORE + ENUCR
C
      DO 100 II=1,KSO
         DO 200 JJ=1,NSOCI
            AB(JJ,II) = CI(JJ,II)
  200    CONTINUE
  100 CONTINUE
C
C   ----- print ci energies and eigenvectors and Davidson corre. -----
C
      WRITE(IW,9160) PRTTOL
C
C
      DO 9000 IJK=1,KSO
         WRITE(IW,9180)
         WRITE(IW,9170) IJK,EL(IJK)+ECONST,SPINS(IJK),SZ,STSYM
         WRITE(IW,9180)
C
C     -- CALCULATE DAVIDSON'S CORRECTION FOR QUADRUPLE EXCITATIONS --
C     THIS IS ACTUALLY A "RENORMALIZED DAVIDSON CORRECTION" FROM
C     PER SIEGBAHN CHEM.PHYS.LETT 55, 386-394(1978)
C     FOR SINGLE REFERENCE, AND FOR MULTIREFERENCE FROM
C     M.R.A.BLOMBERG, P.E.M.SIEGBAHN J.CHEM.PHYS. 78, 5682-5692(1983)
C     SOME OTHER INTERESTING PAPERS IN THIS AREA ARE
C     LANGHOFF AND DAVIDSON, IJQC 8,61(1974)
C     DAVIDSON AND SILVER, CPL, 52, 403(1977)
C     POPLE ET AL, IJQC 11,165(1977) AND IJQC 12,543(1978)
C
         NO = 1
         DO 110 II=1,IJK-1
            IF (ABS(SPINS(II)-SPINS(IJK)).LT.1.0D-05) NO = NO + 1
  110    CONTINUE
         NP = 0
         DO 120 JJ=1,K
            IF (ABS(OSPIN(JJ)-SPINS(IJK)).LT.1.0D-05) THEN
               NP = NP + 1
               IF (NP.EQ.NO) GOTO 125
            ENDIF
  120    CONTINUE
         WRITE(IW,9162)
         GOTO 135
C
  125    WRITE(IW,9164) JJ
C
C  NOW TO EVALUATE SIZE OF ZEROTH ORDER SPACE IN SOCI WAVEFUNCTION
C  AND THE CORRECTION ITSELF.
C
         C0SQ = 0
         ICI = 0
         DO 130 IIA = 1,NA0F
            ISA1 = ISYMA(IIA)
            ICAE = ISPA(IIA)
            DO 132 IIB = ISBS0(ISA1),ISBS1(ISA1)-1
               NEND = ISBC(IIB)
               ICI = ICAE + ISPB(NEND)
               C0SQ = C0SQ + AB(ICI,IJK)*AB(ICI,IJK)
  132       CONTINUE
  130    CONTINUE
C
         ELOWER = ESTATE(JJ) - (EL(IJK)+ECONST)
         EQ = ELOWER*(1.0D+00-C0SQ)/C0SQ
         ESDQ = (EL(IJK)+ECONST)-EQ
         WRITE (IW,9250) NCI,C0SQ,ESTATE(JJ),ELOWER,EQ,ESDQ
C
  135    CONTINUE
C
C
C  Set up the table
C
         DO 150 II=1,200
            CONA(II:II) = ' '
            CONB(II:II) = ' '
  150    CONTINUE
         IA = (NACT+10)/2 - 2
         IF (IA.LE.0) IA = 1
         CONA(IA:IA+4) = 'ALPHA'
         CONB(IA:IA+4) = 'BETA '
         WRITE(6,*)
         WRITE(IW,'(4A)') CONA(1:NACT+10),'|',CONB(1:NACT+10),
     *             '| COEFFICIENT'
         DO 45 II=1,NACT+10
            CONA(II:II) = '-'
   45    CONTINUE
      WRITE(IW,'(4A)') CONA(1:NACT+10),'|',CONA(1:NACT+10),
     *  '|------------'
C
      DO 4000 KJK=1,NSOCI
C
         ICI = 0
         PMAX = 0.0D+00
C
         DO 215 IIA = 1,NA0F
            ISA1 = ISYMA(IIA)
            DO 315 IIB = ISBS0(ISA1),ISBS0(ISA1+1)-1
               NEND = ISBC(IIB)
C
               ICI = ICI + 1
               IF (ABS(AB(ICI,IJK)).GT.PMAX) THEN
                  INDA = IIA
                  INDB = NEND
                  IPOS = ICI
                  PMAX = ABS(AB(ICI,IJK))
               ENDIF
C
  315       CONTINUE
  215    CONTINUE
C
         DO 230 IIA = NA0F+1,NA2S
            ISA1 = ISYMA(IIA)
            DO 330 IIB = ISBS0(ISA1),ISBS2(ISA1)-1
               NEND = ISBC(IIB)
C
               ICI = ICI + 1
               IF (ABS(AB(ICI,IJK)).GT.PMAX) THEN
                  INDA = IIA
                  INDB = NEND
                  IPOS = ICI
                  PMAX = ABS(AB(ICI,IJK))
               ENDIF
C
  330       CONTINUE
  230    CONTINUE
C
         DO 240 IIA = NA2S+1,NATT
            ISA1 = ISYMA(IIA)
            DO 340 IIB = ISBS0(ISA1),ISBS1(ISA1)-1
               NEND = ISBC(IIB)
C
               ICI = ICI + 1
               IF (ABS(AB(ICI,IJK)).GT.PMAX) THEN
                  INDA = IIA
                  INDB = NEND
                  IPOS = ICI
                  PMAX = ABS(AB(ICI,IJK))
               ENDIF
C
  340       CONTINUE
  240    CONTINUE
C
      IF (ABS(AB(IPOS,IJK)).GE.PRTTOL) THEN
C
C   Now to print out the determinant.
C
      IF (INDA.LE.NA0F) THEN
C
         DO 350 II=1,NA
            IACON(II) = II
  350    CONTINUE
         DO 369 II=1,INDA-1
            CALL ADVANC(IACON,NA,NACT)
  369    CONTINUE
C
      ELSEIF(INDA.GT.NA0F.AND.INDA.LE.NA2S) THEN
C
         DO 360 II=1,NA-1
            IACON(II) = II
  360    CONTINUE
C
         NIAP = INDA - NA0F
         NX = (NIAP-1)/NA1E + 1
         NY = NIAP - (NX-1)*NA1E
         DO 367 KK=1,NX-1
            CALL ADVANC(IACON,NA-1,NACT)
  367    CONTINUE
         IACON(NA) = NY+NACT
C
      ELSE
C
         DO 380 II=1,NA-2
            IACON(II) = II
  380    CONTINUE
         IACON(NA-1) = 1 + NACT
         IACON(NA) = 2 + NACT
C
         NIAP = INDA - NA2S
         NX = (NIAP-1)/NA2E + 1
         NY = NIAP - (NX-1)*NA2E
         DO 385 KK=1,NX-1
            CALL ADVANC(IACON,NA-2,NACT)
  385    CONTINUE
         DO 390 KK=1,NY-1
            CALL ADVANC(IACON(NA-1),2,NOCC)
  390    CONTINUE
C
      ENDIF
C
      IF (INDB.LE.NB0F) THEN
C
         DO 353 II=1,NB
            IBCON(II) = II
  353    CONTINUE
         DO 364 II=1,INDB-1
            CALL ADVANC(IBCON,NB,NACT)
  364    CONTINUE
C
      ELSEIF(INDB.GT.NB0F.AND.INDB.LE.NB2S) THEN
C
         DO 362 II=1,NB-1
            IBCON(II) = II
  362    CONTINUE
C
         NIBP = INDB - NB0F
         NX = (NIBP-1)/NB1E + 1
         NY = NIBP - (NX-1)*NB1E
         DO 397 KK=1,NX-1
            CALL ADVANC(IBCON,NB-1,NACT)
  397    CONTINUE
         IBCON(NB) = NY+NACT
C
      ELSE
C
         DO 480 II=1,NB-2
            IBCON(II) = II
  480    CONTINUE
         IBCON(NB-1) = 1 + NACT
         IBCON(NB) = 2 + NACT
C
         NIBP = INDB - NB2S
         NX = (NIBP-1)/NB2E + 1
         NY = NIBP - (NX-1)*NB2E
         DO 485 KK=1,NX-1
            CALL ADVANC(IBCON,NB-2,NACT)
  485    CONTINUE
         DO 490 KK=1,NY-1
            CALL ADVANC(IBCON(NB-1),2,NOCC)
  490    CONTINUE
C
      ENDIF
C
      CONA(1:1) = ' '
      CONB(1:1) = ' '
      DO 400 II=1,NACT
         CONA(II+1:II+1) = '0'
         CONB(II+1:II+1) = '0'
  400 CONTINUE
C
      NX = NA
      IF (INDA.GT.NA0F.AND.INDA.LE.NA2S) NX=NA-1
      IF (INDA.GT.NA2S) NX=NA-2
      NY = NB
      IF (INDB.GT.NB0F.AND.INDB.LE.NB2S) NY=NB-1
      IF (INDB.GT.NB2S) NY=NB-2
C
      DO 182 II=1,NX
         CONA(IACON(II)+1:IACON(II)+1) = '1'
  182 CONTINUE
      DO 192 II=1,NY
         CONB(IBCON(II)+1:IBCON(II)+1) = '1'
  192 CONTINUE
C
      CONA(NACT+2:NACT+10) = ' '
      CONB(NACT+2:NACT+10) = ' '
      IF (INDA.GT.NA0F.AND.INDA.LE.NA2S) THEN
         WRITE(CONA(NACT+3:NACT+5),'(I3)') IACON(NA)
      ELSEIF (INDA.GT.NA2S) THEN
         WRITE(CONA(NACT+3:NACT+5),'(I3)') IACON(NA-1)
         WRITE(CONA(NACT+7:NACT+9),'(I3)') IACON(NA)
      ENDIF
      IF (INDB.GT.NB0F.AND.INDB.LE.NB2S) THEN
         WRITE(CONB(NACT+3:NACT+5),'(I3)') IBCON(NB)
      ELSEIF (INDB.GT.NB2S) THEN
         WRITE(CONA(NACT+3:NACT+5),'(I3)') IBCON(NB-1)
         WRITE(CONA(NACT+7:NACT+9),'(I3)') IBCON(NB)
      ENDIF
C
      WRITE(IW,'(4A,F10.7)') CONA(1:NACT+10),'|',CONB(1:NACT+10),'|  ',
     *         AB(IPOS,IJK)
      AB(IPOS,IJK) = 0.0D+00
C
      GOTO 4000
      ENDIF
      WRITE(6,*)
      GOTO 9000
C
 4000 CONTINUE
C
 9000 CONTINUE
C
      RETURN
 9140 FORMAT(/1X,'CI EIGENVECTORS WILL BE LABELED IN GROUP=',A8)
 9160 FORMAT(1X,'PRINTING CI COEFFICIENTS LARGER THAN',F10.6//
     *       1X,'HAVE TRIED BUT CANNOT GUARANTEE TO HAVE MATCHED ',
     *          'SOCI STATES WITH '/1X,'ZEROTH ORDER STATES.'/
     *       1X,'YOU SHOULD CHECK THIS BEFORE TAKING THE DAVIDSON ',
     *          'CORRECTIONS SERIOUSLY.'/)
 9162 FORMAT(/1X,'COULD NOT MATCH UP STATE WITH ANY ZEROTH ORDER ONE')
 9164 FORMAT(/1X,'STATE MATCHED TO ZEROTH ORDER STATE',I3)
 9170 FORMAT(1X,'STATE',I4,'  ENERGY= ',F20.10,'  S=',F6.2,
     *           '  SZ=',F6.2,:,'  SPACE SYM=',A4)
 9180 FORMAT(1X,74("-"))
 9250 FORMAT(/1X,'RENORMALIZED DAVIDSON CORRECTION FOR',I6,
     *          '-REFERENCE CI.'/
     *       1X,'C0SQ=',F9.6,' EREF=',F16.6,' E-E(REF)=',F11.6,
     *          ' E(Q)=',F11.6/
     *       1X,'GIVES A E(SD+Q) ESTIMATE OF',F20.10)
      END
C
C*MODULE FSODCI  *DECK DENSO1
C     ----------------------------------------------------------
C  nsym through isbc is SYMMETRY AND POSITION STUFF
C  iacon1 to ibcon2 are SCRATCH ARRAYS
C
      SUBROUTINE DENSO1(DE1,NACT,NEXT,NOCC,NSOCI,NA,NB,CI,
     * IFA,IFE,IOX,
     * INDEX,
     * NSYM,ISYMA,ISYMB,ISPA,ISPB,ISAS0,ISBS0,
     * ISAS1,ISBS1,ISAS2,ISBS2,ISAC,
     * IACON1,IBCON1,IACON2,IBCON2)
C     ----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER POSDET
C
      COMMON /DESOC1/ NA0F,NB0F,NA1F,NB1F,NA2F,NB2F,NA1E,NB1E,
     *                NA2E,NB2E,NA1T,NB1T,NA2T,NB2T,NA2S,NB2S,
     *                NATT,NBTT
C
      DIMENSION DE1(*),CI(NSOCI)
      DIMENSION IFA(0:NACT,0:NACT)
      DIMENSION IFE(0:NEXT,0:2)
      DIMENSION IOX(NOCC)
C
      DIMENSION INDEX((NOCC*(NOCC+1))/2 + 1)
C
      DIMENSION ISYMA(NATT),ISYMB(NBTT)
      DIMENSION ISPA(NATT),ISPB(NBTT)
      DIMENSION ISAS0(NSYM+1),ISBS0(NSYM+1)
      DIMENSION ISAS1(NSYM),ISBS1(NSYM)
      DIMENSION ISAS2(NSYM),ISBS2(NSYM)
      DIMENSION ISAC(NATT)
C
      DIMENSION IACON1(NA),IBCON1(NA)
      DIMENSION IACON2(NA),IBCON2(NA)
C
      IDI1 = (NOCC*(NOCC+1))/2
      DO 100 II=1,IDI1
         DE1(II) = 0.0D+00
  100 CONTINUE
C
C    --------------------------------------------
C    Loop over all 0th order alpha determinants.
C    --------------------------------------------
C
      DO 20 II=1,NA
         IACON1(II)=II
   20 CONTINUE
C
      DO 2000 IJK=1,NA0F
         ICAT = ISPA(IJK)
         ISA1 = ISYMA(IJK)
C
C   Loop single excitations from 0th space.
C
         DO 990 IA=1,NA
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C   Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 980 IGEL=IA,NA
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA) IEN=NACT
               DO 970 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
C
C  Storage here for later.
C
                  IPET = POSDET(NACT,NA,IACON2,IFA)
                  IPER11 = (-1)**IPER1
                  IND = INDEX(JJ)+IO1
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 900
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C  Loop over all beta strings now.
C
                  DO 915 INB1=ISBS0(ISA1),ISBS0(ISA1+1)-1
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     FC = CI(ICI1)*CI(ICI2)
                     FC = FC*IPER11
                     DE1(IND) = DE1(IND) + FC
  915             CONTINUE
C
  900             CONTINUE
C
  970          CONTINUE
  980       CONTINUE
C
C  Loop over orbitals in external gap, ie  A -> E.
C
            DO 880 JJ=NACT+1,NOCC
C
               IS2 = IOX(JJ)
C
               CALL REDE01(IACON1,IACON2,NA,IA,JJ,IPER1)
C
C  Storage here for later.
C
               IPET = (POSDET(NACT,NA-1,IACON2,IFA)-1)*NEXT
               IPET = IPET + NA0F + (JJ-NACT)
               IPER11 = (-1)**IPER1
               IND = INDEX(JJ)+IO1
C
C  Check if excited string is correct symmetry.
C
               IF (IS1.NE.IS2) GOTO 800
               ICI1 = ICAT
               ICI2 = ISPA(IPET)
C
C  Loop over all beta dets now. 0th order first.
C
                  DO 815 INB1=ISBS0(ISA1),ISBS2(ISA1)-1
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     FC = CI(ICI1)*CI(ICI2)
                     FC = FC*IPER11
                     DE1(IND) = DE1(IND) + FC
  815             CONTINUE
C
  800          CONTINUE
C
  880       CONTINUE
C
  990    CONTINUE
C
C      DIAGONAL CONTRIBUTIONS NOW
C
            DO 67 II=1,NA
               I1 = IACON1(II)
               IND1 = INDEX(I1) + I1
               ICI1 = ICAT
C
              DO 53 INB1 = ISBS0(ISA1),ISBS0(ISA1+1)-1
                 ICI1 = ICI1 + 1
                 FC = CI(ICI1)*CI(ICI1)
                 DE1(IND1) = DE1(IND1) + FC
   53         CONTINUE
C
   67       CONTINUE
C
         CALL ADVANC(IACON1,NA,NACT)
 2000 CONTINUE
C
C    --------------------------------------------
C    End of loop over all 0th order alpha strings.
C    --------------------------------------------
C
C    --------------------------------------------
C    Loop over all 1st order alpha strings.
C    --------------------------------------------
C
      DO 3995 II=1,NA-1
         IACON1(II)=II
 3995 CONTINUE
      ICOUNT = NA0F
C
      DO 3993 IJK=1,NA1F
         IPOS1 = (IJK-1)*NEXT + NA0F
         DO 3990 KJI=NACT+1,NOCC
            IACON1(NA) = KJI
            ICOUNT = ICOUNT + 1
            ICAT = ISPA(ICOUNT)
            ISA1 = ISYMA(ICOUNT)
C
C   Loop single excitations from 0th space.
C
         DO 3985 IA=1,NA-1
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 3980 IGEL=IA,NA-1
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA-1) IEN = NACT
               DO 3975 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
C
C  Storage here for later.
C
                  IPET = (POSDET(NACT,NA-1,IACON2,IFA)-1)*NEXT
                  IPET = IPET + NA0F + (KJI-NACT)
                  IPER11 = (-1)**IPER1
                  IND = INDEX(JJ)+IO1
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 3970
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
C  Loop over 0th and 1st beta strings now. 0th order first.
C
                  DO 3915 INB1=ISBS0(ISA1),ISBS2(ISA1)-1
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     FC = CI(ICI1)*CI(ICI2)
                     FC = FC*IPER11
                     DE1(IND) = DE1(IND) + FC
C
 3915             CONTINUE
C
 3970             CONTINUE
C
 3975          CONTINUE
 3980       CONTINUE
C
C  Loop over orbitals in gaps in external space, A -> E.
C
            IGEL = NA-1
            DO 3895 JJ=NACT+1,NOCC
               IF (JJ.EQ.IACON1(NA)) THEN
                  IGEL = NA
                  GOTO 3895
               ENDIF
               IS2 = IOX(JJ)
C
               CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
               IACON2(NA-1) = IACON2(NA-1)-NACT
               IACON2(NA) = IACON2(NA)-NACT
C
C  Storage here for later.
C
               IPET1 = POSDET(NACT,NA-2,IACON2,IFA)
               IPET2 = POSDET(NEXT,2,IACON2(NA-1),IFE)
               IPET = (IPET1-1)*NA2E + IPET2 + NA2S
               IPER11 = (-1)**IPER1
               IND = INDEX(JJ)+IO1
C
               IACON2(NA-1) = IACON2(NA-1)+NACT
               IACON2(NA) = IACON2(NA)+NACT
C
C  Check if excited string is correct symmetry.
C
               IF (IS1.NE.IS2) GOTO 3870
               ICI1 = ICAT
               ICI2 = ISPA(IPET)
C
C  Loop over all 0th beta dets now.
C
                  DO 3815 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     FC = CI(ICI1)*CI(ICI2)
                     FC = FC*IPER11
                     DE1(IND) = DE1(IND) + FC
C
 3815             CONTINUE
C
 3870          CONTINUE
C
 3895       CONTINUE
C
 3985    CONTINUE
C
C  Loop single excitation from external space.
C
         IA = NA
         IO1 = IACON1(NA)
         IS1 = IOX(IO1)
         IPER11 = 1
C
C  Loop over orbitals in external space, E -> E.
C
         DO 3795 JJ=IO1+1,NOCC
            IS2 = IOX(JJ)
C
            CALL REDE01(IACON1,IACON2,NA,IA,JJ,IPER1)
C
C  Storage here for later.
C
            IPET = IPOS1 + (JJ-NACT)
            IND = INDEX(JJ)+IO1
C
C  Check if excited string is correct symmetry.
C
            IF (IS1.NE.IS2) GOTO 3770
            ICI1 = ICAT
            ICI2 = ISPA(IPET)
C
C  Loop over all 0th beta dets now.
C
                  DO 3715 INB1=ISBS0(ISA1),ISBS2(ISA1)-1
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     FC = CI(ICI1)*CI(ICI2)
                     FC = FC*IPER11
                     DE1(IND) = DE1(IND) + FC
C
 3715             CONTINUE
C
 3770       CONTINUE
C
 3795    CONTINUE
C
C      DIAGONAL CONTRIBUTIONS NOW
C
            DO 167 II=1,NA
               I1 = IACON1(II)
               IND1 = INDEX(I1) + I1
               ICI1 = ICAT
C
              DO 153 INB1 = ISBS0(ISA1),ISBS2(ISA1)-1
                 ICI1 = ICI1 + 1
                 FC = CI(ICI1)*CI(ICI1)
                 DE1(IND1) = DE1(IND1) + FC
  153         CONTINUE
C
  167       CONTINUE
C
 3990    CONTINUE
         CALL ADVANC(IACON1,NA-1,NACT)
 3993 CONTINUE
C
C    --------------------------------------------
C    End of loop over all 1st order alpha strings.
C    --------------------------------------------
C
C    --------------------------------------------
C    Loop over all 2nd order alpha strings.
C    --------------------------------------------
C
      DO 4995 II=1,NA-2
         IACON1(II) = II
 4995 CONTINUE
      ICOUNT = NA2S
C
      DO 4993 IJK = 1,NA2F
         IACON1(NA-1) = 1 + NACT
         IACON1(NA) = 2 + NACT
         DO 4990 KJI=1,NA2E
            ICOUNT = ICOUNT + 1
            ICAT = ISPA(ICOUNT)
            ISA1 = ISYMA(ICOUNT)
C
C   Loop single excitations from 0th space.
C
         DO 4985 IA=1,NA-2
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in zeroth order space, A -> A.
C
            DO 4980 IGEL=IA,NA-2
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA-2) IEN = NACT
               DO 4975 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
C
C  Storage here for later.
C
                  IPET1 = POSDET(NACT,NA-2,IACON2,IFA)
                  IPET2 = KJI
                  IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                  IPER11 = (-1)**IPER1
                  IND = INDEX(JJ)+IO1
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 4970
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
                  DO 4915 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     FC = CI(ICI1)*CI(ICI2)
                     FC = FC*IPER11
                     DE1(IND) = DE1(IND) + FC
C
 4915             CONTINUE
C
 4970             CONTINUE
C
 4975          CONTINUE
 4980       CONTINUE
C
 4985    CONTINUE
C
C  Loop single excitations from excited space.
C
         DO 4785 IA=NA-1,NA
            IO1 = IACON1(IA)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in excited space, E -> E.
C
            DO 4780 IGEL=IA,NA
               IST = IACON1(IGEL)+1
               IEN = IACON1(IGEL+1)-1
               IF (IGEL.EQ.NA) IEN=NOCC
               DO 4775 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
C
                  CALL REDE00(IACON1,IACON2,NA,IA,IGEL,JJ,IPER1)
                  IACON2(NA-1) = IACON2(NA-1)-NACT
                  IACON2(NA) = IACON2(NA)-NACT
C
C  Storage here for later.
C
                  IPET1 = IJK
                  IPET2 = POSDET(NEXT,2,IACON2(NA-1),IFE)
                  IPET = (IPET1-1)*NA2E + IPET2 + NA2S
                  IPER11 = (-1)**IPER1
                  IND = INDEX(JJ)+IO1
C
                  IACON2(NA-1) = IACON2(NA-1)+NACT
                  IACON2(NA) = IACON2(NA)+NACT
C
C  Check if excited string is correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 4770
                  ICI1 = ICAT
                  ICI2 = ISPA(IPET)
C
                  DO 4715 INB1=ISBS0(ISA1),ISBS1(ISA1)-1
C
                     ICI1 = ICI1 + 1
                     ICI2 = ICI2 + 1
C
                     FC = CI(ICI1)*CI(ICI2)
                     FC = FC*IPER11
                     DE1(IND) = DE1(IND) + FC
C
 4715             CONTINUE
C
 4770             CONTINUE
C
 4775          CONTINUE
 4780       CONTINUE
C
 4785    CONTINUE
C
C      DIAGONAL CONTRIBUTIONS NOW
C
            DO 267 II=1,NA
               I1 = IACON1(II)
               IND1 = INDEX(I1) + I1
               ICI1 = ICAT
C
              DO 253 INB1 = ISBS0(ISA1),ISBS1(ISA1)-1
                 ICI1 = ICI1 + 1
                 FC = CI(ICI1)*CI(ICI1)
                 DE1(IND1) = DE1(IND1) + FC
  253         CONTINUE
C
  267       CONTINUE
C
            CALL ADVANC(IACON1(NA-1),2,NOCC)
 4990    CONTINUE
         CALL ADVANC(IACON1,NA-2,NACT)
 4993 CONTINUE
C
C    --------------------------------------------
C    End loop over all 2nd order alpha strings.
C    --------------------------------------------
C
C    --------------------------------------------
C    Loop over all 0th order beta strings.
C    --------------------------------------------
C
      DO 6020 II=1,NB
         IBCON1(II)=II
 6020 CONTINUE
C
      DO 6000 IJK=1,NB0F
         ICAB = ISPB(IJK)
         ISB1 = ISYMB(IJK)
C
C   Loop single excitations from 0th space.
C
         DO 6990 IB=1,NB
            IO1 = IBCON1(IB)
            IS1 = IOX(IO1)
C
C   Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 6980 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN=NACT
               DO 6970 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
C
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
C
                  IPER = ((-1)**IPER1)
                  IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 6900
                  IPET = POSDET(NACT,NB,IBCON2,IFA)
                  IPB1 = ISPB(IPET)
C
C  Loop over compatible alpha strings.
C
                  DO 6915 INA1=ISAS0(ISB1),ISAS0(ISB1+1)-1
                     NEND = ISAC(INA1)
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     FC = CI(ICI1)*CI(ICI2)
                     FC = FC*IPER
                     DE1(IND) = DE1(IND) + FC
C
 6915             CONTINUE
C
 6900             CONTINUE
C
 6970          CONTINUE
 6980       CONTINUE
C
C  Loop over orbitals in external gap, ie  A -> E.
C
            DO 6880 JJ=NACT+1,NOCC
C
               IS2 = IOX(JJ)
C
               CALL REDE01(IBCON1,IBCON2,NB,IB,JJ,IPER1)
C
               IPER = ((-1)**IPER1)
               IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry.
C
               IF (IS1.NE.IS2) GOTO 6800
               IPET = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
               IPET = IPET + NB0F + (JJ-NACT)
               IPB1 = ISPB(IPET)
C
C  Loop over all compatible alpha strings now.
C
                  DO 6815 INA1=ISAS0(ISB1),ISAS2(ISB1)-1
                     NEND = ISAC(INA1)
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     FC = CI(ICI1)*CI(ICI2)
                     FC = FC*IPER
                     DE1(IND) = DE1(IND) + FC
C
 6815             CONTINUE
C
 6800          CONTINUE
C
 6880       CONTINUE
C
 6990    CONTINUE
C
C   DIAGONAL CONTRIBUTIONS.
C
            DO 69 II=1,NB
               I1 = IBCON1(II)
               IND1 = INDEX(I1) + I1
               DO 93 INA1 = ISAS0(ISB1),ISAS0(ISB1+1)-1
                  NEND = ISAC(INA1)
                  ICIA = ISPA(NEND)
                  ICI1 = ICIA + ICAB
C
                  FC =  CI(ICI1)*CI(ICI1)
                  DE1(IND1) = DE1(IND1) + FC
   93          CONTINUE
   69       CONTINUE
C
         CALL ADVANC(IBCON1,NB,NACT)
 6000 CONTINUE
C
C    --------------------------------------------
C    Loop over all 1st order beta strings.
C    --------------------------------------------
C
      DO 7995 II=1,NB-1
         IBCON1(II)=II
 7995 CONTINUE
      ICOUNT = NB0F
C
      DO 7993 IJK=1,NB1F
         IPOS1 = (IJK-1)*NEXT + NB0F
         DO 7990 KJI=NACT+1,NOCC
            IBCON1(NB) = KJI
            ICOUNT = ICOUNT + 1
            ICAB = ISPB(ICOUNT)
            ISB1 = ISYMB(ICOUNT)
C
C   Loop single excitations from 0th space.
C
         DO 7985 IB=1,NB-1
            IO1 = IBCON1(IB)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in 0th space, ie A -> A.
C
            DO 7980 IGEL=IB,NB-1
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-1) IEN = NACT
               DO 7975 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
C
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
C
                  IPER = ((-1)**IPER1)
                  IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 7970
                  IPET = (POSDET(NACT,NB-1,IBCON2,IFA)-1)*NEXT
                  IPET = IPET + NB0F + (KJI-NACT)
                  IPB1 = ISPB(IPET)
C
C  Loop over compatible alpha strings.
C
                  DO 7915 INA1=ISAS0(ISB1),ISAS2(ISB1)-1
                     NEND = ISAC(INA1)
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     FC = CI(ICI1)*CI(ICI2)
                     FC = FC*IPER
                     DE1(IND) = DE1(IND) + FC
C
 7915             CONTINUE
C
 7970             CONTINUE
C
 7975          CONTINUE
 7980       CONTINUE
C
C  Loop over orbitals in gaps in external space, A -> E.
C
            IGEL = NB-1
            DO 7895 JJ=NACT+1,NOCC
               IF (JJ.EQ.IBCON1(NB)) THEN
                  IGEL = NB
                  GOTO 7895
               ENDIF
               IS2 = IOX(JJ)
C
               CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
C
               IPER = ((-1)**IPER1)
               IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry
C
               IF (IS1.NE.IS2) GOTO 7870
C
               IBCON2(NB-1) = IBCON2(NB-1)-NACT
               IBCON2(NB) = IBCON2(NB)-NACT
C
               IPET1 = POSDET(NACT,NB-2,IBCON2,IFA)
               IPET2 = POSDET(NEXT,2,IBCON2(NB-1),IFE)
               IPET = (IPET1-1)*NB2E + IPET2 + NB2S
               IPB1 = ISPB(IPET)
C
               IBCON2(NB-1) = IBCON2(NB-1)+NACT
               IBCON2(NB) = IBCON2(NB)+NACT
C
C  Loop over all 0th alpha dets now.
C
                  DO 7815 INA1=ISAS0(ISB1),ISAS1(ISB1)-1
                     NEND = ISAC(INA1)
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
                     FC = CI(ICI1)*CI(ICI2)
                     FC = FC*IPER
                     DE1(IND) = DE1(IND) + FC
C
 7815             CONTINUE
C
 7870          CONTINUE
C
 7895       CONTINUE
C
 7985    CONTINUE
C
C  Loop single excitation from external space.
C
         IB = NB
         IO1 = IBCON1(NB)
         IS1 = IOX(IO1)
         IPER1 = 1
C
C  Loop over orbitals in external space, E -> E.
C
         DO 7795 JJ=IO1+1,NOCC
            IS2 = IOX(JJ)
C
            IF (IS1.NE.IS2) GOTO 7795
C
            CALL REDE01(IBCON1,IBCON2,NB,IB,JJ,IPER1)
            IPET = IPOS1 + (JJ-NACT)
            IPB1 = ISPB(IPET)
            IPER = ((-1)**IPER1)
            IND = INDEX(JJ) + IO1
C
C  Loop over all zero and 1st order alpha strings.
C
            DO 7730 INA1 = ISAS0(ISB1),ISAS2(ISB1)-1
               NEND = ISAC(INA1)
C
               ICIA = ISPA(NEND)
               ICI1 = ICIA + ICAB
               ICI2 = ICIA + IPB1
C
               FC = CI(ICI1)*CI(ICI2)
               FC = FC*IPER
               DE1(IND) = DE1(IND) + FC
C
 7730       CONTINUE
C
 7795    CONTINUE
C
C   DIAGONAL CONTRIBUTIONS.
C
            DO 169 II=1,NB
               I1 = IBCON1(II)
               IND1 = INDEX(I1) + I1
               DO 193 INA1 = ISAS0(ISB1),ISAS2(ISB1)-1
                  NEND = ISAC(INA1)
                  ICIA = ISPA(NEND)
                  ICI1 = ICIA + ICAB
C
                  FC =  CI(ICI1)*CI(ICI1)
                  DE1(IND1) = DE1(IND1) + FC
  193          CONTINUE
  169       CONTINUE
C
 7990    CONTINUE
         IF (NB.LE.1) GOTO 7993
         CALL ADVANC(IBCON1,NB-1,NACT)
 7993 CONTINUE
C
C    --------------------------------------------
C    Loop over all 2nd order beta strings.
C    --------------------------------------------
C
      IF (NB.LT.2) GOTO 8997
      DO 8995 II=1,NB-2
         IBCON1(II) = II
 8995 CONTINUE
      ICOUNT = NB2S
C
      DO 8993 IJK = 1,NB2F
         IBCON1(NB-1) = 1 + NACT
         IBCON1(NB) = 2 + NACT
         DO 8990 KJI=1,NB2E
            ICOUNT = ICOUNT + 1
            ICAB = ISPB(ICOUNT)
            ISB1 = ISYMB(ICOUNT)
C
C   Loop single excitations from 0th space.
C
         DO 8985 IB=1,NB-2
            IO1 = IBCON1(IB)
            IS1 = IOX(IO1)
C
C  Loop over orbitals in gaps in zeroth order space, A -> A.
C
            DO 8980 IGEL=IB,NB-2
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB-2) IEN = NACT
               DO 8975 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
C
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
C
                  IPER = ((-1)**IPER1)
                  IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 8970
C
                  IPET1 = POSDET(NACT,NB-2,IBCON2,IFA)
                  IPET2 = KJI
                  IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                  IPB1 = ISPB(IPET)
C
C  Loop over all 0th alpha dets now.
C
                  DO 8915 INA1=ISAS0(ISB1),ISAS1(ISB1)-1
                     NEND = ISAC(INA1)
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
               FC = CI(ICI1)*CI(ICI2)
               FC = FC*IPER
               DE1(IND) = DE1(IND) + FC
C
 8915             CONTINUE
C
 8970             CONTINUE
C
 8975          CONTINUE
 8980       CONTINUE
C
 8985   CONTINUE
C
C  Loop single excitations from excited space.
C
         DO 8785 IB=NB-1,NB
            IO1 = IBCON1(IB)
            IS1 = IOX(IO1)
C
C
C  Loop over orbitals in gaps in excited space, E -> E.
C
            DO 8780 IGEL=IB,NB
               IST = IBCON1(IGEL)+1
               IEN = IBCON1(IGEL+1)-1
               IF (IGEL.EQ.NB) IEN=NOCC
               DO 8775 JJ=IST,IEN
C
                  IS2 = IOX(JJ)
C
                  CALL REDE00(IBCON1,IBCON2,NB,IB,IGEL,JJ,IPER1)
C
                  IPER = ((-1)**IPER1)
                  IND = INDEX(JJ) + IO1
C
C  Check if string is of correct symmetry.
C
                  IF (IS1.NE.IS2) GOTO 8770
C
                  IBCON2(NB-1) = IBCON2(NB-1)-NACT
                  IBCON2(NB)   = IBCON2(NB)-NACT
C
                  IPET1 = IJK
                  IPET2 = POSDET(NEXT,2,IBCON2(NB-1),IFE)
                  IPET = (IPET1-1)*NB2E + IPET2 + NB2S
                  IPB1 = ISPB(IPET)
C
                  IBCON2(NB-1) = IBCON2(NB-1)+NACT
                  IBCON2(NB) = IBCON2(NB)+NACT
C
C  Loop over all 0th alpha dets now.
C
                  DO 8715 INA1=ISAS0(ISB1),ISAS1(ISB1)-1
                     NEND = ISAC(INA1)
C
                     ICIA = ISPA(NEND)
                     ICI1 = ICIA + ICAB
                     ICI2 = ICIA + IPB1
C
               FC = CI(ICI1)*CI(ICI2)
               FC = FC*IPER
               DE1(IND) = DE1(IND) + FC
C
 8715             CONTINUE
C
 8770             CONTINUE
C
 8775          CONTINUE
 8780       CONTINUE
C
 8785    CONTINUE
C
C   DIAGONAL CONTRIBUTIONS.
C
            DO 269 II=1,NB
               I1 = IBCON1(II)
               IND1 = INDEX(I1) + I1
               DO 293 INA1 = ISAS0(ISB1),ISAS1(ISB1)-1
                  NEND = ISAC(INA1)
                  ICIA = ISPA(NEND)
                  ICI1 = ICIA + ICAB
C
                  FC =  CI(ICI1)*CI(ICI1)
                  DE1(IND1) = DE1(IND1) + FC
  293          CONTINUE
  269       CONTINUE
C
            CALL ADVANC(IBCON1(NB-1),2,NOCC)
 8990    CONTINUE
         IF (NB.LE.2) GOTO 8993
         CALL ADVANC(IBCON1,NB-2,NACT)
 8993 CONTINUE
 8997 CONTINUE
C
      RETURN
      END
C
C*MODULE FSODCI  *DECK DETSODM
C     --------------------------
      SUBROUTINE DETSODM(NPRINT)
C     --------------------------
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
      COMMON /DESOCI/ OSPIN(MXRT),NEXT,MOSET,MAXPSO,KSO,NSOCI
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
      M1 = NACT+NEXT
      M2 = (M1*M1+M1)/2
      L1 = NUM
      L2 = (L1*L1+L1)/2
      L3 = L1*L1
C
      CALL VALFM(LOADFM)
      LDM1    = LOADFM + 1
C
      LDAO = LDM1 + M2
      LVAO   = LDAO   + L2
      LVNO   = LVAO   + L3
      LOCCNO = LVNO   + L3
      LIWRK  = LOCCNO + L1
      LWRK   = LIWRK  + M1
      LSCR   = LWRK   + 8*M1
      LAST   = LSCR   + M1
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
         GO TO 580
      END IF
C
C        set the energy to the root whose properties will be computed
C
      E = ESTATE(IROOT)
C
C        Read one particle density matrix for each state
C
      CALL SEQREW(NFT15)
C
      DO 550 IST=1,K
         IF(NFLGDM(IST).EQ.0) GOTO 550
         CALL SQREAD(NFT15,X(LDM1),M2)
C
         IF(SOME  .AND.  NFLGDM(IST).EQ.2) THEN
            WRITE(IW,9230)
            CALL PRTRI(X(LDM1),M1)
         END IF
         CALL DETNO(SOME,X(LDM1),X(LDAO),X(LVAO),X(LVNO),
     *              X(LOCCNO),X(LIWRK),X(LWRK),X(LSCR),ESTATE,
     *              IROOT,IST,NCORSV,M1,M1,M2,L1,L2)
  550 CONTINUE
C
  580 CONTINUE
      CALL RETFM(NEED)
      IF(SOME) WRITE(IW,9240)
      IF(SOME) CALL TIMIT(1)
      RETURN
C
 9210 FORMAT(/5X,27("-")/5X,'NATURAL ORBITAL GENERATION'/5X,27("-")//
     *  1X,'DENSITY MATRIX WILL BE SAVED FOR PROPERTIES OF STATE',I4)
 9230 FORMAT(/1X,'1-PARTICLE DENSITY MATRIX IN MO BASIS')
 9240 FORMAT(1X,'..... DONE WITH ONE PARTICLE DENSITY MATRIX .....')
      END
