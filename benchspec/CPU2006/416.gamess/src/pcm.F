C  9 dec 03 - mws - synch common block runopt
C 16 Jun 03 - HL  - add c-pcm and new gradients for pcm
C                   SOLPRT: print gas phase to solvent energy change
C  7 Aug 02 - HL,CP - iterative isotropic IEF-PCM/EFP
C                   MAKCAV: new keyword radii and optional suaefp,vdwefp
C                   DISREP: provide default dka(*) and rwa(*)
C                   SOLPRT: print details of matrx inversion IEF-PCM/EFP
C 17 Apr 02 - MWS - synch up efmult and frginf common
C  8 Oct 01 - HL  - parallelize PCM, re-keyword ikrep for PCM
C  6 Sep 01 - MWS - add dummy arguments to name io call
C 25 Jun 01 - MWS - alter common block wfnopt
C 13 Jun 01 - HL  - solvnt,makcav,disrep: buffer/EFP+PCM, dynamic mem
C 29 Dec 00 - MWS - pcmfld: correct fix for direct scf computation
C 26 Oct 00 - MWS - KEYWORD SPELLING CHANGED BACK TO IREP
C 11 Oct 00 - PB,BM - interfaced EFP+PCM
C 25 AUG 00 - BM  - added IEF solvation model
C 21 Dec 99 - MWS - MAKE SYMMOL COMMON CONSISTENT
C 12 Nov 98 - GDF - make symtry common consistent
C 27 FEB 98 - MWS - PCMINP: TRAP ILLEGAL SCFTYP, CI, MP2, SEMIEMPIRICAL
C 28 SEP 97 - BM  - SOLPRT: PRINT REPULSIVE ENERGI IF IDISK=1
C 14 AUG 97 - MWS - PCMFLD: FIX DIRECT SCF COMPUTATIONS
C 27 MAR 97 - MWS - PCMINP,SOLPRT: ADD CHECKING, EXPLANATORY PRINTOUT
C 18 MAR 97 - PISA - NEW MODULE TO DRIVE PCM COMPUTATIONS
C
C*MODULE PCM     *DECK PCMINP
      SUBROUTINE PCMINP
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,ABEL
C
      CHARACTER*8 ZSOL
C
      PARAMETER (MXTS=2500, MXSP=250, MXATM=500, MXSH=1000, MXFRG=50)
C
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMABC/ AXYZ(3),BXYZ(3),CXYZ(3),NAB,NAC
      COMMON /PCMCAV/ OMEGA,RET,FRO,ALPHA(MXSP),RIN(MXSP),ICENT,
     *                IPRINT,IRETCAV
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMDIS/ WB,WA,ETA2,GD,EVAC,IDP
      COMMON /PCMITR/ IPCMIT,LIST(MXSP+1),IMUL,MXDIIS,RCUT(2),
     *                THRES,NREG,MXITR1,MXITR2
      COMMON /PCMOPT/ PEL(MXTS),V_ELE(MXTS),V_NUC(MXTS),V_MON1(MXTS),
     *                V_MON2(MXTS),RABI,RASC,REFPOL,THRSLS,DENSLS,
     *                IDIRCT,IEFPOL,
     *                IMGABI,IMGASC
      COMMON /PCMPAR/ IPCM,NFT26,NFT27,IKREP,IEF,IP_F
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /SYMMOL/ GROUP,COMPLEX,IGROUP,NAXIS,ILABMO,ABEL
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                T(432),INVT(48),NT
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      PARAMETER (ZERO=0.0D+00)
C
C     ----- set up the NAMELIST $PCM -----
C
      PARAMETER (NNAM=32)
      DIMENSION QNAM(NNAM),KQNAM(NNAM)
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA PCMWD/8HPCM     /
#else
      CHARACTER*8 :: PCMWD_STR
      EQUIVALENCE (PCMWD, PCMWD_STR)
      DATA PCMWD_STR/"PCM     "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QNAM/8HIEF     ,8HSOLVNT  ,8HEPSINF  ,8HEPS     ,8HRSOLV   ,
     *          8HOMEGA   ,8HRET     ,8HFRO     ,8HTABS    ,
     *          8HVMOL    ,8HTCE     ,8HDSTEN   ,8HSTEN    ,8HCMF     ,
     *          8HNESFP   ,8HICENT   ,8HICOMP   ,8HIFIELD  ,
     *          8HICAV    ,8HIDISP   ,8HIREP    ,8HAXYZ    ,8HBXYZ    ,
     *          8HCXYZ    ,8HNAB     ,8HNAC     ,8HIPRINT  ,
     *          8HIDP     ,8HWB      ,8HWA      ,8HETA2    ,8HNESFF   /
#else
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"IEF     ","SOLVNT  ","EPSINF  ","EPS     ",
     *          "RSOLV   ",
     *          "OMEGA   ","RET     ","FRO     ","TABS    ",
     *          "VMOL    ","TCE     ","DSTEN   ","STEN    ","CMF     ",
     *          "NESFP   ","ICENT   ","ICOMP   ","IFIELD  ",
     *          "ICAV    ","IDISP   ","IREP    ","AXYZ    ","BXYZ    ",
     *          "CXYZ    ","NAB     ","NAC     ","IPRINT  ",
     *          "IDP     ","WB      ","WA      ","ETA2    ","NESFF   "/
#endif
      DATA KQNAM/1,5,3,3,3,3,3,3,3,3,3,3,3,3,
     *           1,1,1,1,1,1,1,33,33,33,1,1,1,
     *           1,3,3,3,1/
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA C1/8HC1      /
#else
      CHARACTER*8 :: C1_STR
      EQUIVALENCE (C1, C1_STR)
      DATA C1_STR/"C1      "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RINPUT/8HINPUT   /, RNONE/8HNONE    /
#else
      CHARACTER*8 :: RINPUT_STR
      EQUIVALENCE (RINPUT, RINPUT_STR)
      CHARACTER*8 :: RNONE_STR
      EQUIVALENCE (RNONE, RNONE_STR)
      DATA RINPUT_STR/"INPUT   "/, RNONE_STR/"NONE    "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RHF,UHF,ROHF,GVB/8HRHF     ,8HUHF     ,8HROHF    ,8HGVB     /
#else
      CHARACTER*8 :: RHF_STR
      EQUIVALENCE (RHF, RHF_STR)
      CHARACTER*8 :: UHF_STR
      EQUIVALENCE (UHF, UHF_STR)
      CHARACTER*8 :: ROHF_STR
      EQUIVALENCE (ROHF, ROHF_STR)
      CHARACTER*8 :: GVB_STR
      EQUIVALENCE (GVB, GVB_STR)
      DATA RHF_STR,UHF_STR,ROHF_STR,GVB_STR/"RHF     ","UHF     ", 
     * "ROHF    ", "GVB     "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA TDHF/8HTDHF    /
#else
      CHARACTER*8 :: TDHF_STR
      EQUIVALENCE (TDHF, TDHF_STR)
      DATA TDHF_STR/"TDHF    "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA NONE/4HNONE/
#else
      CHARACTER*4 :: NONE_STR
      EQUIVALENCE (NONE, NONE_STR)
      DATA NONE_STR/"NONE"/
#endif
C
C     --- READ IN PARAMETERS TO PERFORM A PCM SOLVATION CALCULATION ---
C     If the $PCM group is not found, PCM computation is not performed.
C
      IPCM = 0
C
C     The default solvent is assumed to be WATER at 25 C, and the
C     cavity is built as usual, with a sphere of scaled atomic radius
C     around each solute atom.
C
C       this group defines the solvent (no default)
C
      RZSOL = RINPUT
      EPSI = ZERO
      EPSINI = ZERO
      RSOLVI = ZERO
      VMOLI = ZERO
      TCEI = ZERO
      STENI = ZERO
      DSTENI = ZERO
      CMFI = ZERO
C
C       this group defines the cavity
C
      ICENT = 0
      NESFP = NAT
      NESFF = NFRG*3
      OMEGA = 40.0D+00
      FRO = 0.7D+00
      RET = 100.0D+00
C
C       this group defines the type of PCM run done
C
C*********************************************
C **  NEW INTEGRAL EQUATION FORMALISM (IEF) **
C **       E. CANCES AND B. MENNUCCI        **
C*********************************************
C Option IEF:0  isotropic dielectrics with standard PCM
C            1  anisotropic dielectrics
C            2  ionic solutions
C            3  isotropic dielectrics with the IEF method
C               by Cances & Mennucci
C               (new version: more accurate and faster, default),
C            4,5-7,8 absorption/emission processes
C                4-5 nonequilibrium absorption
C                7-8 nonequilibrium emission
C        IEF:-3 ITERATIVE isotropic IEF-PCM, as IEF=3
C
      CALL DERCHK(NDER)
      IF(NDER.GT.0) THEN
         IEF = -3
      ELSE
         IEF =  3
      END IF
      ICOMP = 0
      IFIELD = 0
      IDISP = 0
      IKREP = 0
      IDP = 0
      ICAV = 0
C
      IPRINT=0
C
C        data associated with ICAV=1
C
      TABS = 298.0D+00
C
C        data associated with IDP=1
C
      WB = 0.451D+00
      WA = 1.10D+00
      ETA2 = 1.75D+00
C
C        data associated with IFIELD=2
C        AXYZ(3),BXYZ(3),CXYZ(3) contain the coordinates of the vertices
C        of the planar grid where the reaction field has to be computed:
C        A ===> higher left corner of the grid
C        B ===> lower left corner of the grid
C        C ===> higher right corner of the grid
C           A ----------- C
C           |             |
C           |             |
C           |             |
C           B ------------
C        while NAB and NAC define the vertical (A--B edge) and the
C        orizontal (A--C edge) subdivision of the grid.
C
      CALL VCLR(AXYZ,1,3)
      CALL VCLR(BXYZ,1,3)
      CALL VCLR(CXYZ,1,3)
      NAB = 0
      NAC = 0
C
      JRET=0
      CALL NAMEIO(IR,JRET,PCMWD,NNAM,QNAM,KQNAM,
     *            IEF, RZSOL, EPSINI, EPSI, RSOLVI, OMEGA, RET,
     *            FRO, TABS, VMOLI, TCEI, DSTENI, STENI, CMFI,
     *            NESFP, ICENT, ICOMP, IFIELD, ICAV, IDISP, IKREP,
     *            AXYZ, BXYZ, CXYZ, NAB, NAC, IPRINT,
     *            IDP, WB, WA, ETA2, NESFF,
     *            0,0,
     *    0,0,0,0,0,  0,0,0,0,0,
     *    0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0)
C
      IF(JRET.EQ.1) RETURN
C
      IF(JRET.EQ.2) THEN
         IF(MASWRK) WRITE(IW,9000) PCMWD
         CALL ABRT
      END IF
C
C        Detection of the $PCM group enables PCM computation.
C
      IPCM=1
C
C     crisbeg
C
C     Detection of iterative calculation
C
      IPCMIT=0
      IF (IEF.LT.0) THEN
         IPCMIT=1
         IEF=-IEF
         IF (IEF.NE.3.AND.IEF.NE.10) THEN
            IF(MASWRK)WRITE(IW,*)
     *            'IEF ITERATIVE PROCEDURE ONLY FOR ISOTROPIC IEF'
            CALL ABRT
         ENDIF
C
C     READ ITERATIVE-RELATED INPUT
C
         CALL ITIEFIN
      ENDIF
C     crisend
C
C        its too early in the run to test on analytic hessian...
C
      NERR=0
      IF(SCFTYP.EQ.UHF)  NERR=NERR+1
      IF(SCFTYP.EQ.ROHF) NERR=NERR+1
      IF(SCFTYP.EQ.GVB)  NERR=NERR+1
      IF(CITYP.NE.RNONE) NERR=NERR+1
      IF(MPLEVL.GT.0)    NERR=NERR+1
      IF(MPCTYP.NE.NONE) NERR=NERR+1
C
      IF(NERR.GT.0) THEN
         IF(MASWRK) WRITE(IW,9070)
         CALL ABRT
      END IF
C
      NFT26 = 26
      NFT27 = 27
      CALL SEQOPN(NFT26,'PCMDATA','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQOPN(NFT27,'PCMINTS','UNKNOWN',.FALSE.,'UNFORMATTED')
C
C        Point group symmetry must not be used, the cavity
C        is not tesselated sufficiently symmetrically.
C
      NT=1
      GROUP=C1
      IGROUP=1
      NAXIS=1
C
C        No longer an input value, DR is the distance away from the
C        representative point of the tessera along the surface normal
C
      DR = 1.0D-04
C
C        look up numerical parameters for the solvent, default is water
C        any values which were input should override stored values
C
      WRITE(UNIT=ZSOL,FMT='(A8)') RZSOL
      IF(ZSOL.NE.'INPUT   ') THEN
         CALL DATSOL(ZSOL,EPS,EPSINF,RSOLV,VMOL,TCE,STEN,DSTEN,CMF)
      END IF
      IF(EPSI  .NE.ZERO) EPS    = EPSI
      IF(EPSINI.NE.ZERO) EPSINF = EPSINI
      IF(RSOLVI.NE.ZERO) RSOLV  = RSOLVI
      IF(VMOLI .NE.ZERO) VMOL   = VMOLI
      IF(TCEI  .NE.ZERO) TCE    = TCEI
      IF(STENI .NE.ZERO) STEN   = STENI
      IF(DSTENI.NE.ZERO) DSTEN  = DSTENI
      IF(CMFI  .NE.ZERO) CMF    = CMFI
C
C        print what we've got for input
C
      IF(MASWRK) THEN
         WRITE(IW,9020) IEF,ICOMP,IFIELD,ICAV,IDISP,IKREP,IDP,
     *     ZSOL,EPS,EPSINF,VMOL,TCE,STEN,DSTEN,CMF,RSOLV,
     *     ICENT,NESFP,NESFF,OMEGA,RET,FRO,
     *     TABS,IPRINT
         IF(IDP.EQ.1) WRITE(IW,9030) WA,WB,ETA2
      END IF
C        print iterative IEF-PCM input
      IF(MASWRK.AND.IPCMIT.NE.0)WRITE(IW,9025)
     *    IPCMIT,MXDIIS,MXITR1,MXITR2,THRES,IMUL,
     *    RCUT(1),RCUT(2),IDIRCT,IMGABI,RABI,IMGASC,RASC,
     *    THRSLS,DENSLS,IEFPOL,REFPOL
C
C
C        Check that input for booboos
C
      NERR=0
      IF(ZSOL.EQ.'INPUT   ' .AND. (EPS.EQ.ZERO.OR.RSOLV.EQ.ZERO)) THEN
         IF(MASWRK) WRITE(IW,9010)
         IF(MASWRK) WRITE(IW,*) 'PICK SOLVNT OR RSOLV/EPS/EPSINF...'
         NERR = NERR+1
      END IF
      IF(NESFP.GT.MXSP) THEN
         IF(MASWRK) WRITE(IW,9010)
         IF(MASWRK) WRITE(IW,*) 'EXCESSIVE NUMBER OF RADII (>MXSP)'
         NERR=NERR+1
      END IF
      IF(ICENT.GT.3) THEN
         IF(MASWRK) WRITE(IW,9010)
         IF(MASWRK) WRITE(IW,*) 'WRONG VALUE FOR -ICENT- (>3)'
         NERR=NERR+1
      END IF
      IF(ICOMP.GT.3) THEN
         IF(MASWRK) WRITE(IW,9010)
         IF(MASWRK) WRITE(IW,*) 'WRONG VALUE FOR -ICOMP- (>3)'
         NERR=NERR+1
      END IF
      CALL DERCHK(NDER)
      IF(NDER.GT.0  .AND.  (ICOMP.NE.0  .AND.  ICOMP.NE.2)) THEN
         IF(MASWRK) WRITE(IW,9010)
         IF(MASWRK) WRITE(IW,*) 'A GRADIENT RUN REQUIRES ICOMP=0 OR 2'
         NERR=NERR+1
      END IF
C     IF(NDER.GT.0.AND.(IKREP+IDP+IDISP+ICAV.NE.0)) THEN
      IF(NDER.GT.0.AND.(IKREP+IDP.NE.0)) THEN
         IF(MASWRK) WRITE(IW,9010)
         IF(MASWRK) WRITE(IW,*) 'A GRADIENT RUN REQUIRES IKREP=IDP=0'
         NERR=NERR+1
      END IF
      IF(IFIELD.GT.2) THEN
         IF(MASWRK) WRITE(IW,9010)
         IF(MASWRK) WRITE(IW,*) 'WRONG VALUE FOR -IFIELD- (>2)'
         NERR=NERR+1
      END IF
      IF(ICAV.GT.1) THEN
         IF(MASWRK) WRITE(IW,9010)
         IF(MASWRK) WRITE(IW,*) 'WRONG VALUE FOR -ICAV- (>1)'
         NERR=NERR+1
      END IF
      IF(IDISP.GT.1) THEN
         IF(MASWRK) WRITE(IW,9010)
         IF(MASWRK) WRITE(IW,*) 'WRONG VALUE FOR -IDISP- (>1)'
         NERR=NERR+1
      END IF
      IF(IKREP.GT.1) THEN
         IF(MASWRK) WRITE(IW,9010)
         IF(MASWRK) WRITE(IW,*) 'WRONG VALUE FOR -IKREP- (>1)'
         NERR=NERR+1
      END IF
      IF(IDP.GT.1) THEN
         IF(MASWRK) WRITE(IW,9010)
         IF(MASWRK) WRITE(IW,*) 'WRONG VALUE FOR -IDP- (>1)'
         NERR=NERR+1
      END IF
      IF(IDISP.GT.0) THEN
         IF((IKREP+IDP).NE.0) THEN
          IF(MASWRK) WRITE(IW,9010)
          IF(MASWRK) WRITE(IW,*) 'IDISP IS INCOMPATIBLE WITH IKREP/IDP'
          NERR = NERR+1
         END IF
      END IF
C
      IF(NERR.GT.0) THEN
         IF(MASWRK) WRITE(IW,*) 'PLEASE FIX THE ABOVE ERROR(S) IN $PCM'
         CALL ABRT
         STOP
      END IF
C
C        read options for the IEF solver
C
      IF(IEF.NE.0) CALL IEFDAT
C
C        only RHF IEF=3 non-TDHF runs can go in parallel,
C        parallelism is OK for EFP+PCM and buffer links+PCM
C
C     IF(SCFTYP.EQ.RHF  .AND.  IEF.EQ.3  .AND.  RUNTYP.NE.TDHF) THEN
      IF(SCFTYP.EQ.RHF.AND.(IEF.EQ.3.OR.IEF.EQ.10)
     *               .AND.  RUNTYP.NE.TDHF) THEN
         CONTINUE
      ELSE
         IF(GOPARR) THEN
            IF(MASWRK) WRITE(IW,9060)
            CALL ABRT
            STOP
         END IF
      END IF
      RETURN
C
 9000 FORMAT(1X,'**** ERROR IN $',A8,' INPUT')
 9010 FORMAT(1X,'*** INCONSISTENCY FOUND IN $PCM INPUT GROUP ***')
 9020 FORMAT(/5X,35("-")/
     *   5X,'INPUT FOR PCM SOLVATION CALCULATION '/5X,35(1H-)/
     *   5X,'IEF   =',I8/
     *   5X,'ICOMP =',I8,  5X,'IFIELD=',I8,  5X,'ICAV  =',I8/
     *   5X,'IDISP =',I8,  5X,'IKREP =',I8,  5X,'IDP   =',I8//
     *   5X,'SOLVNT=',A8,  5X,'EPS   =',F8.4,5X,'EPSINF=',F8.4/
     *   5X,'VMOL  =',F8.4,5X,'TCE   =',F12.8,1X,'STEN  =',F8.4/
     *   5X,'DSTEN =',F8.4,5X,'CMF   =',F8.4,5X,'RSOLV =',F8.4//
     *   5X,'ICENT =',I8,  5X,'NESFP =',I8  ,5X,'NESFF =',I8/
     *   5X,'OMEGA =',F8.4,5X,'RET   =',F8.4,5X,'FRO   =',F8.4//
     *   5X,'TABS  =',F8.4,5X,'IPRINT=',I8)
C  hui li
 9025 FORMAT(/5X,34("-")/
     *   5X,'INPUT FOR ITERATIVE IEF-PCM METHOD '/5X,34(1H-)/
     *   5X,'IPCMIT  =',I10,    5X,'MXDIIS  =',I10/
     *   5X,'MXITR1  =',I10,    5X,'MXITR2  =',I10/
     *   5X,'THRES   =',E10.3,  5X,'IMUL    =',I10/
     *   5X,'RCUT(1) =',F10.4,  5X,'RCUT(2) =',F10.4//
     *   5X,'IDIRCT  =',I10/
     *   5X,'IMGABI  =',I10,    5X,'RABI    =',F10.4/
     *   5X,'IMGASC  =',I10,    5X,'RASC    =',F10.4/
     *   5X,'THRSLS  =',E10.3,  5X,'DENSLS  =',F10.4/
     *   5X,'IEFPOL  =',I10,    5X,'REFPOL  =',F10.4/)
 9030 FORMAT(/5X,'INPUT FOR PCM DISPERSION CALCULATION '/5X,36("-")/
     *   5X,'WA    =',F8.4,5X,'WB    =',F8.4,' ETA2  =',F8.4/)
 9060 FORMAT(1X,'*** ERROR *** THIS JOB CANNOT RUN IN PARALLEL'/
     *       1X,'ONLY SCFTYP=RHF WITH IEF=3 CAN RUN IN PARALLEL, AND'/
     *       1X,'THEN ONLY IF THE JOB IS NOT RUNTYP=TDHF.')
 9070 FORMAT(1X,'*** ERROR *** PCM COMPUTATIONS ARE NOT POSSIBLE FOR'/
     *       6X,'SCFTYP=UHF, ROHF, OR GVB RUNS'/
     *       6X,'MP2 OR CI RUNS'/6X,'SEMI-EMPIRICAL RUNS, OR'/
     *       6X,'ANALYTIC HESSIAN RUNS.')
      END
C*MODULE PCM     *DECK SOLVNT
      SUBROUTINE SOLVNT
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME
C
C   hui li
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS)
      PARAMETER (MXSP=250)
C
      COMMON /FMCOM / XX(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMDIS/ WB,WA,ETA2,GD,EVAC,IDP
      COMMON /PCMF  / IPCDER,IFAST,Q2(MXTS),CHG2N(MXTS)
      COMMON /PCMITR/ IPCMIT,LIST(MXSP+1),IMUL,MXDIIS,RCUT(2),
     *                THRES,NREG,MXITR1,MXITR2
      COMMON /PCMOPT/ PEL(MXTS),V_ELE(MXTS),V_NUC(MXTS),V_MON1(MXTS),
     *                V_MON2(MXTS),RABI,RASC,REFPOL,THRSLS,DENSLS,
     *                IDIRCT,IEFPOL,
     *                IMGABI,IMGASC
      COMMON /PCMPAR/ IPCM,NFT26,NFT27,IKREP,IEF,IP_F
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RMC/8HMCSCF   /
#else
      CHARACTER*8 :: RMC_STR
      EQUIVALENCE (RMC, RMC_STR)
      DATA RMC_STR/"MCSCF   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
C
C     ----- set up a PCM-BEM calculation -----
C
C     The common blocks used by the PCM package all begin with PCM...
C       /PCMABC/ reaction field grid
C       /PCMCAV/ cavity sphere info       size=          2*mxsp
C       /PCMCHG/ surface charge info      size= 2*mxts
C       /PCMDBS/ dispersion basis set
C       /PCMDAT/ input control params
C       /PCMDIS/ dispersion info
C       /PCMPLY/ polyhedron definitions   size= 1*mxts + 5*mxsp
C       /PCMPAR/ input control params
C       /PCMPRT/ final print results
C       /PCMREP/ repulsion info
C       /PCMTES/ tessera data             size= 8*mxts
C       /PCMITR/ PCM iterative procedure related data size = mxsp**2
C       /PCMTHF/ time dependent HF data
C       /PCMUGG/ surface energy data
C     Those listed without a size are very small.
C     Total fixed memory demand is roughly 7*MXSP + 11*MXTS.
C
C     The disk files used by the PCM package are
C       PCMDATA - NFT26 - All runs have the D inverse matrix, while
C                         gradient runs also have the DERPUNT, DERTES,
C                         DERCENTR, and DERRAD arrays present.
C       PCMINTS - NFT27 - elec field ints projected on tesserae normals
C     The file 26 data cannot be stored on DAF, unfortunately, as the
C     number of tesserae on the cavity surface changes during the run.
C
C     The DAF records used by the PCM package are
C       330 - CEL matrix, AO components of electric fields on normals.
C       331 - VEF matrix, for auxiliary dispersion basis set.
C       332 - QEFF matrix
C       333 - ELD matrix
C
C     The memory usage by the PCM package is controlled by 2 parameters,
C       MXSP = maximum number of spheres in the cavity.  This should be
C              similar to the number of atoms being treated, although a
C              methyl group, for example, might be in a single sphere.
C       MXTS = maximum number of cavity tessera.  This should be roughly
C              25 times MXSP, more or less.
C     There are two other less well set up memory parameters,
C       MXTSEC in routine SOLVNT for the escaped charge cavity.
C       MXDBS in routine SOLVNT for dispersion basis set size.
C     The 'mung' script can be used to resize MXSP or MXTS, but the
C     other two require hand editing of the source routines.
C
      L2 = (NUM*NUM+NUM)/2
      L3 = NUM*NUM
      SOME = NPRINT.NE.-5
      IF(SOME.AND.(IP_F.EQ.1)) THEN
        IF(MASWRK) WRITE(IW,800)
      ELSE
        IF(SOME .AND. MASWRK) WRITE(IW,900)
      END IF
C
C     1) Solute's cavity formation (GEPOL-GAUSS-BONNET method)
C
      CALL TESIN
      CALL PEDRAM
      IF(SOME .AND. MASWRK) THEN
         WRITE(IW,*)
         WRITE(IW,*) '..... DONE GENERATING CAVITY .....'
         CALL TIMIT(1)
      END IF
C
      IF(IPCMIT.EQ.0) THEN
C     ----  hui li  print PCM memory message ---
      IF(MASWRK) WRITE(IW,*) ' '
      IF(MASWRK) WRITE(IW,*) ' '
      IF(MASWRK) WRITE(IW,*) 'GAMESS NEEDS AT LEAST ',NTS*NTS,
     *  ' WORDS ON EACH NODE TO RUN PCM.'
      IF(MASWRK.AND.IEF.EQ.3) WRITE(IW,*)
     *             'IF THE MEMORY IS MORE THAN ',INT(3.2*NTS*NTS),
     *             '(3.2 TIMES ',NTS*NTS,')'
      IF(MASWRK.AND.IEF.EQ.3)
     *        WRITE(IW,*)'GAMESS WILL USE 3 MATRICES THUS',
     *                     ' AROUND 2.5 TIMES FASTER.'
      IF(MASWRK.AND.IEF.EQ.3) WRITE(IW,*)
     *         'PLEASE SELECT OPTIMAL MEMORY AND NODES',
     *         ' TO ACHIEVE THE HIGHEST EFFICIENCY.'
      IF(MASWRK) WRITE(IW,*) ' '
      IF(MASWRK) WRITE(IW,*) ' '
C
C     ----  if memory is enough, use 3 matrice (~2.5 times faster)
C           if not enough, use 1 matrix (slow but affordable)
C
      CALL GOTFM(NGOTMX)
      NMAT = 1
      IF(NGOTMX.GT.(3.2*NTS*NTS)) NMAT = 3
C
      END IF
C
C
C     1b) New cavity with a finer mesh to be used in the calculation of
C         escaped electronic charge.
C
      IF(ICOMP.EQ.3  .OR.  IKREP.EQ.1) THEN
C
C     Maximum number of tesserae in escaped charge cavity is -mxtsec-,
C     Note that quadrupling the number of tessera is just a guess.
C
         MXTSEC = 4*MXTS
         MXVERT = MXTSEC/2 + 2
C
         CALL VALFM(LOADFM)
         LCV    = LOADFM + 1
         LJTR   = LCV    + MXVERT*3
         LOLDTR = LJTR   + (MXTSEC*3)/NWDVAR + 1
         LEDNEW = LOLDTR + (100*100)/NWDVAR
         LINTSP = LEDNEW + (90*100)/NWDVAR
         LX     = LINTSP + (MXTSEC*10)/NWDVAR
         LY     = LX     + MXTSEC
         LZ     = LY     + MXTSEC
         LAS    = LZ     + MXTSEC
         LX1    = LAS    + MXTSEC
         LY1    = LX1    + MXTSEC
         LZ1    = LY1    + MXTSEC
         LSCR   = LZ1    + MXTSEC
         LSCR2  = LSCR   + L2*3
         LCEL   = LSCR2  + 3*225
         LAST   = LCEL   + L2
         NEED   = LAST - LOADFM - 1
         CALL GETFM(NEED)
         IF(EXETYP.NE.CHECK) THEN
            CALL MORETS(XX(LCV),XX(LJTR),XX(LOLDTR),XX(LEDNEW),
     *                  XX(LINTSP),XX(LX),XX(LY),XX(LZ),XX(LX1),
     *                  XX(LY1),XX(LZ1),XX(LAS),XX(LSCR),XX(LSCR2),
     *                  XX(LCEL),L2,MXTSEC,MXVERT)
         END IF
         CALL RETFM(NEED)
         IF(SOME .AND. MASWRK) THEN
            WRITE(IW,*)
            WRITE(IW,*) '..... DONE FORMING ESCAPED CHARGE CAVITY .....'
            CALL TIMIT(1)
         END IF
      END IF
C
C     Memory pointers for both DDMM and CCVEBF, note the overlaps.
C     NTS= number of tesserae in original cavity
C
C     in the iterative calculation no need to call CCVEBF
      IF (IPCMIT.EQ.0) THEN
         NTS3 = NTS*NTS
      ELSE
         NTS3=0
      END IF
C
C     NTS3 = NTS*NTS
      NTV  = NTS*3*10
      CALL VALFM(LOADFM)
      LDMATM = LOADFM + 1
      LAST   = LDMATM + NTS3
C
      IF(IEF.NE.0) THEN
         LSE   = LAST
         LDE   = LSE   + NTS3
         LAST  = LDE   + NTS3
         IF(IEF.LT.3) THEN
            LSI   = LAST
            LDI   = LSI   + NTS3
            LVERT = LDI   + NTS3
            LCNT  = LVERT + NTV
            LAST  = LCNT  + NTV
         ELSE
C   hui li  only 1 nts3 is needed if nmat=1 .
            IF(NMAT.EQ.1)LAST  = LDMATM + NTS3
            LSI   = LAST
            LDI   = LAST
            LVERT = LAST
            LCNT  = LAST
         END IF
      END IF
C
      LWORK  = LAST
      LIPVT  = LWORK  + NTS
      LDIMT1 = LIPVT  + NTS
      LDIMT2 = LDIMT1 + NTS
      LAST1  = LDIMT2 + NTS
C
      IF(IEF.NE.0) THEN
        LMEP=LAST
        LCAM=LMEP+NTS
        LAST=LCAM+NTS
      END IF
      LBMCHG = LAST
      LVEC   = LBMCHG + L2
      LFLD   = LVEC   + NTS
      LFLW   = LFLD   + L2*3
      LAST2  = LFLW   + 225*3
      LAST   = MAX(LAST1,LAST2)
      NEED   = LAST - LOADFM - 1
      CALL GETFM(NEED)
C
C     2) Calculation of D matrix and its inverse.
C     Save the inverse D matrix on file nft26, after derivative
C     information from -PEDRA-, if these exist.
C     Note memory is not returned, to pass D inverse to -CCVEBF-
C
      CALL SEQREW(NFT26)
      CALL DERCHK(NDER)
      IF(NDER.EQ.1.AND.IPCDER.EQ.1) THEN
         CALL SEQADV(NFT26)
         CALL SEQADV(NFT26)
         CALL SEQADV(NFT26)
         CALL SEQADV(NFT26)
      END IF
C
      IF(EXETYP.EQ.CHECK) THEN
         CALL VCLR(XX(LDMATM),1,NTS3)
         IF(IEF.NE.0) THEN
          CALL VCLR(XX(LSE),1,NTS3)
          CALL VCLR(XX(LDE),1,NTS3)
          IF(IEF.LT.3) THEN
           CALL VCLR(XX(LSI),1,NTS3)
           CALL VCLR(XX(LDI),1,NTS3)
           CALL VCLR(XX(LVERT),1,NTV)
           CALL VCLR(XX(LCNT),1,NTV)
          END IF
         END IF
      ELSE
         IF(IEF.EQ.0) THEN
           CALL DDMM(XX(LDMATM),XX(LWORK),XX(LIPVT),NTS)
         ELSE
           IF(IEF.LT.3) THEN
              CALL SQREAD(NFT26,XX(LVERT),NTV)
              CALL SQREAD(NFT26,XX(LCNT),NTV)
              CALL IEFCMM(XX(LDMATM),XX(LSE),XX(LDE),XX(LSI),XX(LDI),
     *                    XX(LVERT),XX(LCNT),XX(LWORK),XX(LIPVT),NTS)
           ELSE
C     in iterative procedure this section is skipped
             IF (IPCMIT.EQ.0) THEN
             IF (IEF.EQ.10) THEN
                 CALL CPCMMV(XX(LDMATM),XX(LWORK),XX(LIPVT),NTS)
             ELSE
              IF(NMAT.EQ.1)
     *           CALL IEFCMV1(XX(LDMATM),
     *                  XX(LWORK),XX(LIPVT),XX(LDIMT1),XX(LDIMT2),NTS)
              IF(NMAT.EQ.3)
     *           CALL IEFCMMV(XX(LDMATM),XX(LSE),XX(LDE),
     *                  XX(LWORK),XX(LIPVT),XX(LDIMT1),XX(LDIMT2),NTS)
             END IF
             END IF
           END IF
         END IF
      END IF
C
      IF(IEF.GT.0.AND.IEF.LT.3) THEN
        CALL SQWRIT(NFT26,XX(LSE),NTS3)
        CALL SQWRIT(NFT26,XX(LDE),NTS3)
      END IF
      CALL SQWRIT(NFT26,XX(LDMATM),NTS3)
      CALL SEQREW(NFT26)
C cris
      IF (IPCMIT.EQ.0) THEN
C
        IF(SOME .AND. MASWRK) THEN
         WRITE(IW,*)
         IF(IEF.EQ.0) THEN
          WRITE(IW,*) '..... DONE GENERATING -D- INVERSE MATRIX .....'
         ELSE
          WRITE(IW,*) '..... DONE GENERATING -C- INVERSE MATRIX .....'
         END IF
         CALL TIMIT(1)
        END IF
      END IF
C----------------------------------
C
C     C. Amovilli and B. Mennucci
C     2b) Preparation of data for dispersion calculation
C     The maximum size of auxiliary dispersion basis set is -mxdbs-
C
      IF(IDP.EQ.1) THEN
         MXDBS = 200
         CALL VALFM(LOADFM)
         LNKTYP = LOADFM + 1
         LXYZE  = LNKTYP + MXDBS
         LAST   = LXYZE  + MXDBS*4
         NEEDDP = LAST - LOADFM - 1
         CALL GETFM(NEEDDP)
         CALL ENLBS(XX(LNKTYP),XX(LXYZE),MXDBS,NUMB)
         CALL RETFM(NEEDDP)
C
         NUMB2 = (NUMB*NUMB+NUMB)/2
         CALL VALFM(LOADFM)
         LSINV  = LOADFM + 1
         LSSQR  = LSINV  + NUMB2
         LIPVT  = LSSQR  + NUMB*NUMB
         LWORK  = LIPVT  + NUMB
         LVX    = LWORK  + NUMB
         LEX    = LVX    + NUMB
         LA     = LEX    + NUMB
         LPOT   = LA     + L3
         LFLD   = LPOT   + NUMB2
         LFLW   = LFLD   + 3*NUMB2
         LAST   = LFLW   + 3*225
         NEEDDP = LAST - LOADFM - 1
         CALL GETFM(NEEDDP)
         IF(EXETYP.EQ.CHECK) THEN
            CALL VCLR(XX(LSINV),1,L2)
         ELSE
            CALL DBSINV(XX(LSINV),XX(LSSQR),XX(LIPVT),XX(LWORK),
     *                  NUMB,NUMB2)
            CALL INTVE(XX(LSINV),XX(LVX),XX(LEX),XX(LA),XX(LPOT),
     *                 XX(LFLD),XX(LFLW),NUMB,NUMB2,L3)
         END IF
         CALL DAWRIT(IDAF,IODA,XX(LSINV),L2,331,0)
         CALL RETFM(NEEDDP)
         IF(SOME .AND. MASWRK) THEN
            WRITE(IW,*)
            WRITE(IW,*) '..... DONE WITH DISPERSION SETUP .....'
            CALL TIMIT(1)
         END IF
      END IF
C
C     3) Calculation of induced charges on the cavity.
C     CCVEBF writes the electric field integrals to disk file -NFT27-.
C
      IF(EXETYP.NE.CHECK) THEN
       IF(IEF.EQ.0) THEN
         CALL CCVEBF(XX(LDMATM),XX(LBMCHG),XX(LVEC),XX(LFLD),XX(LFLW),
     *               NTS,L2,NFT27,SOME)
       ELSE
C
         IF(IEF.LT.3) THEN
          CALL ICVEBF(XX(LDMATM),XX(LSE),XX(LDE),XX(LBMCHG),XX(LVEC),
     *        XX(LMEP),XX(LCAM),XX(LFLD),XX(LFLW),NTS,L2,NFT27,SOME)
         ELSE
C
C     cris: the electronic potential is now calculated in ELEPOT,
C     in the iterative procedure there is no need to call ICVEV
C
               IF (IPCMIT.EQ.0) THEN
                 CALL ICVEV(XX(LDMATM),XX(LBMCHG),XX(LMEP),XX(LCAM),
     *                      NTS,L2,NFT27,SOME)
               ELSE
                 IF(IDIRCT.NE.1) CALL ELEPOT(XX(LBMCHG),L2,NFT27)
                 CALL VCLR(QSN,1,NTS)
               END IF
         END IF
C
       END IF
      END IF
      IF(IEF.LT.3.AND.SOME .AND. MASWRK) THEN
         WRITE(IW,*)
         WRITE(IW,*) '..... DONE WITH ELECTRIC FIELD INTEGRALS .....'
         CALL TIMIT(1)
      END IF
C
C     4) Calculation of the interaction free energy between
C        solute's nuclei-nuclear induced charges.
C
C     in iterative qsn are all 0 thus vnn is not useful
C
      IF(EXETYP.NE.CHECK) THEN
         IF (IPCMIT.EQ.0) THEN
            CALL VNN
            IF(SOME .AND. MASWRK) THEN
               WRITE(IW,*)
               WRITE(IW,*)
     *              '..... DONE WITH INDUCED NUCLEAR CHARGES .....'
               CALL TIMIT(1)
            END IF
         ELSE
            UNZ=0.0D+00
         ENDIF
      END IF
C
C-----PRADIPTA---CHANGES-FOR-THE-EFP+PCM-model---------
C     4a) Additive terms like Vf,n-Vf,f-and-Vn,f-are-evaluated in the
C         next routine.
      IF(IP_F.EQ.1) THEN
         CALL VALFM(LOADFM)
         LVEC=LOADFM+1
         LVECI=LVEC+L2
         LVECTEST=LVECI+L2
         LVECEFP=LVECTEST+L2
         LVECMUL=LVECEFP+NTS
         LVECQD=LVECMUL+NTS
         LQFS  = LVECQD + NTS
         LAST = LQFS + NTS
         NEEDEF = LAST - LOADFM -1
         CALL GETFM(NEEDEF)
C crisbeg
         IF (IPCMIT.EQ.0) THEN
           CALL ENUADD(XX(LDMATM),XX(LVEC),XX(LVECI),XX(LVECTEST),
     *          XX(LVECEFP),XX(LVECQD),NTS,L2)
           CALL DAWRIT(IDAF,IODA,XX(LVECI),L2,263,0)
         ELSE
           CALL VCLR(Q_FS,1,NTS)
         END IF
C crisend
         CALL RETFM(NEEDEF)
      END IF
C
C        this is paired with a memory request quite a ways above here.
C
      CALL RETFM(NEED)
C
C     5) Calculation of the interaction matrix between nuclear induced
C        charges and solute's electrons. The matrix has to be added to
C        the monoelectronic terms of the hamiltonian H.
C
      IF (IPCMIT.EQ.0) THEN
        IF(IDP.EQ.0) THEN
         CALL VALFM(LOADFM)
         LVEC  = LOADFM + 1
         LVEC2 = LVEC   + L2
         LS    = LVEC2  + L2
         LREP  = LS     + L2
         LCEL  = LREP   + L2
         LAST  = LCEL   + L2
         NEED  = LAST - LOADFM - 1
         CALL GETFM(NEED)
         IF(EXETYP.NE.CHECK.AND.SCFTYP.NE.RMC) THEN
            CALL JMAT(XX(LVEC),XX(LVEC2),XX(LS),XX(LREP),XX(LCEL),L2)
            CALL DAWRIT(IDAF,IODA,XX(LVEC2),L2,264,0)
            IF(SOME .AND. MASWRK) THEN
               WRITE(IW,*)
               WRITE(IW,*) '..... DONE GENERATING -J- MATRIX .....'
               CALL TIMIT(1)
            END IF
         END IF
         CALL RETFM(NEED)
        END IF
      END IF
C
      IF((.NOT.SOME).AND.MASWRK) THEN
         WRITE(IW,*)
         WRITE(IW,*) '..... DONE SETTING UP PCM CALCULATION .....'
         CALL TIMIT(1)
      END IF
      RETURN
C
  900 FORMAT(/10X,'-------------------------------------'/
     *        10X,'---- POLARISABLE CONTINUUM MODEL ----'/
     *        10X,'----      UNIVERSITY OF PISA     ----'/
     *        10X,'-------------------------------------')
  800 FORMAT(/10X,'********************************************'/
     *        10X,'*    EFFECTIVE FRAGMENT POTENTIAL(EFP)     *'/
     *        10X,'*                 +                        *'/
     *        10X,'*    POLARISABLE CONTINUUM MODEL(PCM)      *'/
     *        10X,'*                                          *'/
     *        10X,'*       PRADIPTA BANDYOPADHYAY             *'/
     *        10X,'*               AND                        *'/
     *        10X,'*         BENEDETTA MENNUCCI               *'/
     *        10X,'*             JULY-2000                    *'/
     *        10X,'********************************************')
      END
C*MODULE PCM     *DECK MAKCVM
      SUBROUTINE MAKCVM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXFRG=50)
      COMMON /FMCOM / X(1)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
C         get dynamic memory for -makcav-
C
      NMTPTS = NMTTPT
      CALL VALFM(LOADFM)
      LAAF  = LOADFM + 1
      LIFR  = LAAF   + 3*NMTPTS
      LAST  = LIFR   + NMTPTS
      NEED = LAST - LOADFM - 1
      CALL GETFM(NEED)
C
      CALL MAKCAV(X(LAAF),X(LIFR),NMTPTS)
C
      CALL RETFM(NEED)
      RETURN
      END
C*MODULE PCM     *DECK MAKCAV
      SUBROUTINE MAKCAV(AAF,IFR,NMTPTS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*8 FRGNME
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      DIMENSION AAF(3,NMTPTS),IFR(NMTPTS)
      DIMENSION RVDW(86)
C
      PARAMETER (MXATM=500, MXTS=2500, MXSP=250)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /ECP2  / CLP(400),ZLP(400),NLP(400),KFIRST(MXATM,6),
     *                KLAST(MXATM,6),LMAX(MXATM),LPSKIP(MXATM),
     *                IZCORE(MXATM)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /FRGMSS/ FPMASS(MXPT),FMASS(MXPT,MXFRG),
     *                FPNUC(MXPT),FGNUC(MXFGPT)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMAG / I_NESF,L_AST
      COMMON /PCMCAV/ OMEGA,RET,FRO,ALPHA(MXSP),RIN(MXSP),ICENT,
     *                IPRINT,IRETCAV
      COMMON /PCMPAR/ IPCM,NFT26,NFT27,IKREP,IEF,IP_F
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMSPH/ INA(MXSP),INF(MXSP)
C
      PARAMETER (NNAM=8)
      DIMENSION QNAM(NNAM),KQNAM(NNAM)
C
      PARAMETER (TOANGS=0.52917724924D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CAVWD/8HPCMCAV  /
#else
      CHARACTER*8 :: CAVWD_STR
      EQUIVALENCE (CAVWD, CAVWD_STR)
      DATA CAVWD_STR/"PCMCAV  "/
#endif
C  hui li  new keyword RADII
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QNAM/8HXE      ,8HYE      ,8HZE      ,8HRIN     ,
     *          8HALPHA   ,8HINA     ,8HINF     ,8HRADII   /
#else
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"XE      ","YE      ","ZE      ","RIN     ",
     *          "ALPHA   ","INA     ","INF     ","RADII   "/
#endif
      DATA KQNAM/-3,-3,-3,-3,-3,-1,-1,5/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA VANDW/8HVANDW   /
#else
      CHARACTER*8 :: VANDW_STR
      EQUIVALENCE (VANDW, VANDW_STR)
      DATA VANDW_STR/"VANDW   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA SUAEFP/8HSUAEFP  /
#else
      CHARACTER*8 :: SUAEFP_STR
      EQUIVALENCE (SUAEFP, SUAEFP_STR)
      DATA SUAEFP_STR/"SUAEFP  "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA VDWEFP/8HVDWEFP  /
#else
      CHARACTER*8 :: VDWEFP_STR
      EQUIVALENCE (VDWEFP, VDWEFP_STR)
      DATA VDWEFP_STR/"VDWEFP  "/
#endif
C
C       van der Waals radii taken from "The Elements", 2nd edition,
C       John Emsley, Clarendon Press, Oxford, 1991.  Unknown values
C       are simply set to zero, rather than trying to guess them.
C
C       A.Bondi, J.Chem.Phys. 68: 441-451(1964) gives alternate
C       values, and a few transition metals.
C
      DATA (RVDW(II),II=1,18)/1.20D+00,1.22D+00,
     *      0.00D+00,0.00D+00,2.08D+00,1.85D+00,
     *      1.54D+00,1.40D+00,1.35D+00,1.60D+00,
     *      2.31D+00,0.00D+00,2.05D+00,2.00D+00,
     *      1.90D+00,1.85D+00,1.81D+00,1.91D+00/
      DATA (RVDW(II),II=19,36)/2.31D+00,13*0.0D+00,
     *      2.00D+00,2.00D+00,1.95D+00,1.98D+00/
      DATA (RVDW(II),II=37,54)/2.44D+00,13*0.0D+00,
     *      2.20D+00,2.20D+00,2.15D+00,0.00D+00/
      DATA (RVDW(II),II=55,86)/2.62D+00,27*0.0D+00,2.40D+00,3*0.0D+00/
C
C       --- read possible $PCMCAV input group ---
C
C     override the above table with U. Pisa's experience
C     as to what works best for singly bonded C,N,O
C
      RVDW(6) = 1.70D+00
      RVDW(7) = 1.60D+00
      RVDW(8) = 1.50D+00
C
      KQNAM(1) = 10*MXSP + 3
      KQNAM(2) = 10*MXSP + 3
      KQNAM(3) = 10*MXSP + 3
      KQNAM(4) = 10*MXSP + 3
      KQNAM(5) = 10*MXSP + 3
      KQNAM(6) = 10*MXSP + 1
      KQNAM(7) = 10*MXSP + 1
C
C        sphere centers XE,YE,ZE and radii RIN must be input in Angstrom
C
      CALL VCLR(XE,1,MXSP)
      CALL VCLR(YE,1,MXSP)
      CALL VCLR(ZE,1,MXSP)
      CALL VCLR(RIN,1,MXSP)
      CALL VCLR(ALPHA,1,MXSP)
      DO I=1,MXSP
        INA(I)=0
        INF(I)=0
      ENDDO
      IBT=0
C
      RADII=VANDW
      JRET=0
      CALL NAMEIO(IR,JRET,CAVWD,NNAM,QNAM,KQNAM,
     *            XE,YE,ZE,RIN,ALPHA,INA,INF,RADII,
     *            0,   0,0,0,0,0,
     *    0,0,0,0,0,  0,0,0,0,0,
     *    0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,
     *    0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0)
      IF(JRET.EQ.2) THEN
         IF(MASWRK) WRITE(IW,9000) CAVWD
         CALL ABRT
      END IF
C
C
C--------ICENT=0---------------
C
C
      IF(ICENT.LE.0) THEN
      IF(RADII.EQ.VANDW)THEN
        DO J=1,NESFP
           XE(J)=C(1,J)*TOANGS
           YE(J)=C(2,J)*TOANGS
           ZE(J)=C(3,J)*TOANGS
           NUCZ = INT(ZAN(J)+IZCORE(J)+0.001D+00)
           RIN(J) = RVDW(NUCZ)
        ENDDO
C
        IAT=0
        IF(IP_F.EQ.1) THEN
          DO 5120 IFRG=1,NFRG
             DO 5110 III=1,NMPTS(IFRG)
                IAT=IAT+1
                IF(FMASS(III,IFRG).GT.0.0) THEN
C                WRITE(IW,*)'ERROR_PRA',IBT
                IBT=IBT+1
                  AAF(1,IBT) = EFC(1,IAT)*TOANGS
                  AAF(2,IBT) = EFC(2,IAT)*TOANGS
                  AAF(3,IBT) = EFC(3,IAT)*TOANGS
                  IFR(IBT)=1
                END IF
5110      ENDDO
5120      ENDDO
          JN=0
          DO J=NESFP+1,NESFP+IBT
C             WRITE(IW,*)'ERR-PRA',NESFP,J,JN
             IF((J-NESFP).EQ.(1+3*JN))RIN(J)=RVDW(8)
             IF((J-NESFP).EQ.(2+3*JN))RIN(J)=RVDW(1)
             IF((J-NESFP).EQ.(3+3*JN))RIN(J)=RVDW(1)
             IF(MOD((J-NESFP),3).EQ.0)JN=JN+1
C            JN=JN+1
             KN=J-NESFP
             XE(J)=AAF(1,KN)
             YE(J)=AAF(2,KN)
             ZE(J)=AAF(3,KN)
          ENDDO
        END IF
          I_NESFP=NESFP+IBT
          I_NESF=I_NESFP
C
      END IF
      IF(RADII.EQ.SUAEFP)THEN
C     apply SUAEFP
C     SUAEFP: Simplified United Atomic radii for EFP
C
C       -- for ab initio and buffer atoms --
        DO J=1,NESFP
           XE(J)=C(1,J)*TOANGS
           YE(J)=C(2,J)*TOANGS
           ZE(J)=C(3,J)*TOANGS
           NUCZ = INT(ZAN(J)+IZCORE(J)+0.001D+00)
           RIN(J)=1.500D+00
           IF(NUCZ.EQ.1)RIN(J)=0.010D+00
           IF(NUCZ.EQ.6)RIN(J)=1.770D+00
           IF(NUCZ.EQ.7)RIN(J)=1.680D+00
           IF(NUCZ.EQ.8)RIN(J)=1.590D+00
           IF(NUCZ.EQ.15)RIN(J)=2.100D+00
           IF(NUCZ.EQ.16)RIN(J)=2.100D+00
           IF(NUCZ.GE.17)RIN(J)=2.300D+00
        ENDDO
C
C       -- for EFP points with non-zero nuclear charges --
        IF(IP_F.EQ.1) THEN
             DO 5500 III=1,MXFGPT
                IF(FGNUC(III).GT.0.001D+00) THEN
                  IBT=IBT+1
                  AAF(1,IBT) = EFC(1,III)*TOANGS
                  AAF(2,IBT) = EFC(2,III)*TOANGS
                  AAF(3,IBT) = EFC(3,III)*TOANGS
                  IFR(IBT)=1
C
                  J=NESFP+IBT
                  NUCZ = INT(FGNUC(III)+0.001D+00)
                  RIN(J)=1.500D+00
                  IF(NUCZ.EQ.1)RIN(J)=0.010D+00
                  IF(NUCZ.EQ.6)RIN(J)=1.770D+00
                  IF(NUCZ.EQ.7)RIN(J)=1.680D+00
                  IF(NUCZ.EQ.8)RIN(J)=1.590D+00
                  IF(NUCZ.EQ.15)RIN(J)=2.100D+00
                  IF(NUCZ.EQ.16)RIN(J)=2.100D+00
                  IF(NUCZ.GE.17)RIN(J)=2.300D+00
C
                  XE(J)=AAF(1,IBT)
                  YE(J)=AAF(2,IBT)
                  ZE(J)=AAF(3,IBT)
                END IF
5500        ENDDO
        END IF
          I_NESFP=NESFP+IBT
          I_NESF=I_NESFP
      END IF
C
      IF(RADII.EQ.VDWEFP)THEN
C     apply VDWEFP
C     VDWEFP:  van der Waals radii for EFP
C              Note the difference from VANDW
C
C       -- for ab initio and buffer atoms --
        DO J=1,NESFP
           XE(J)=C(1,J)*TOANGS
           YE(J)=C(2,J)*TOANGS
           ZE(J)=C(3,J)*TOANGS
           NUCZ = INT(ZAN(J)+IZCORE(J)+0.001D+00)
           RIN(J)=RVDW(NUCZ)
           IF(RIN(J).LT.0.0010D+00)RIN(J)=1.600D+00
        ENDDO
C
C       -- for EFP points with non-zero nuclear charges --
        IF(IP_F.EQ.1) THEN
             DO 5600 III=1,MXFGPT
                IF(FGNUC(III).GT.0.001D+00) THEN
                  IBT=IBT+1
                  AAF(1,IBT) = EFC(1,III)*TOANGS
                  AAF(2,IBT) = EFC(2,III)*TOANGS
                  AAF(3,IBT) = EFC(3,III)*TOANGS
                  IFR(IBT)=1
C
                  J=NESFP+IBT
                  NUCZ = INT(FGNUC(III)+0.001D+00)
                  RIN(J)=RVDW(NUCZ)
                  IF(RIN(J).LT.0.0010D+00)RIN(J)=1.60D+00
C
                  XE(J)=AAF(1,IBT)
                  YE(J)=AAF(2,IBT)
                  ZE(J)=AAF(3,IBT)
                END IF
5600        ENDDO
        END IF
          I_NESFP=NESFP+IBT
          I_NESF=I_NESFP
      END IF
C
      END IF
C
C
C        this is the end of icent=0 code
C
      NESF=NESFP
C
      JRET=0
      CALL NAMEIO(IR,JRET,CAVWD,NNAM,QNAM,KQNAM,
     *            XE,YE,ZE,RIN,ALPHA,INA,INF,RADII,
     *            0,   0,0,0,0,0,
     *    0,0,0,0,0,  0,0,0,0,0,
     *    0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,
     *    0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0)
C
      IF(JRET.EQ.2) THEN
         IF(MASWRK) WRITE(IW,9000) CAVWD
         CALL ABRT
      END IF
C
C--------ICENT=2---------
C
      IF(ICENT.EQ.2) THEN
        DO J=1,NESF
          XE(J)=C(1,INA(J))*TOANGS
          YE(J)=C(2,INA(J))*TOANGS
          ZE(J)=C(3,INA(J))*TOANGS
        ENDDO
C
        IF(IP_F.EQ.1) THEN
          IAT=0
          IBT=0
          DO 5121 IFRG=1,NFRG
             DO 5111 III=1,NMPTS(IFRG)
                IAT=IAT+1
                IF(FMASS(III,IFRG).GT.0.0) THEN
                  IBT=IBT+1
                  IFR(IBT)=0
                  AAF(1,IBT) = EFC(1,IAT)*TOANGS
                  AAF(2,IBT) = EFC(2,IAT)*TOANGS
                  AAF(3,IBT) = EFC(3,IAT)*TOANGS
                END IF
 5111      CONTINUE
 5121     CONTINUE
          DO J=1,NESFF
             IFR(INF(J))=1
             XE(NESF+J)=AAF(1,INF(J))
             YE(NESF+J)=AAF(2,INF(J))
             ZE(NESF+J)=AAF(3,INF(J))
          ENDDO
        END IF
        I_NESFP=NESFP+NESFF
        I_NESF=I_NESFP
      END IF
C
C         end of icent=2 code
C
      IF(IP_F.EQ.1) CALL DAWRIT(IDAF,IODA,IFR,NMTPTS,266,1)
C
      IF(MASWRK) WRITE(IW,9010)
      NERR = 0
      DO J=1,I_NESF
         IF(MASWRK) WRITE(IW,9020) J,XE(J),YE(J),ZE(J),RIN(J)
         IF(RIN(J).LE.0) NERR=NERR+1
      ENDDO
      IF(NERR.GT.0) THEN
         IF(MASWRK) WRITE(IW,9030)
         CALL ABRT
         STOP
      END IF
C
C        on exit, XE,YE,ZE should be converted to Bohr, but RIN stays A.
C
      DO J=1,I_NESFP
         XE(J)=XE(J)/TOANGS
         YE(J)=YE(J)/TOANGS
         ZE(J)=ZE(J)/TOANGS
      ENDDO
C
C     If parameter ALPHA(I) is greater than 0 the I-th radius is
C     multiplied by ALPHA(I).
C     This allows to use radii=R(van der Waals)*ALPHA in the calculation
C     of electrostatic (and eventually SCF disp-rep) contribution, and
C     radii = RvdW for the cavitation.
C     It is possible to give a different value of ALPHA to each atom:
C     if the first one is less than zero, it is fixed equal to 1,
C     otherwise it keeps its value. Each following value less than zero
C     is changed to 1.
C
      IF(ALPHA(1).LE.0.0D+00) ALPHA(1) = 1.2D+00
      DO I = 2, MXSP
        IF(ALPHA(I).LE.0.0D+00) ALPHA(I) = ALPHA(1)
      ENDDO
      RETURN
C
 9000 FORMAT(1X,'**** ERROR IN $',A8,' INPUT')
 9010 FORMAT(/5X,'INPUT FOR CAVITY DEFINITION '/5X,27("-")/
     *   5X,'ATOM         COORDINATES           RADIUS ')
 9020 FORMAT(2X,I4,4(2X,F8.4))
 9030 FORMAT(1X,'**** ERROR **** PCM SPHERE(S) MUST HAVE',
     *          ' A POSITIVE RADIUS')
      END
C*MODULE PCM     *DECK CCVEBF
      SUBROUTINE CCVEBF(DMATM1,BEMCHG,VEC,FLD,FLW,NTSX,L2,NFT27,SOME)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION DMATM1(NTSX,NTSX),BEMCHG(L2),VEC(NTSX),
     *          FLD(L2,3),FLW(225,3)
C
      LOGICAL SOME
C
C   hui li
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
      COMMON /ELPROP/ ELDLOC,ELMLOC,ELPLOC,ELFLOC,
     *                IEDEN,IEMOM,IEPOT,IEFLD,MODENS,
     *                IEDOUT,IEMOUT,IEPOUT,IEFOUT,
     *                IEDINT,IEMINT,IEPINT,IEFINT
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
C   hui li
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
      COMMON /XYZPRP/ XP,YP,ZP,DMY(35)
C
      PARAMETER (TOANGS=0.52917724924D+00, ANTOAU=1.0D+00/TOANGS)
C
      DATA ZERO, ONE  /0.0D+00, 1.0D+00/
      DATA FPI/12.56637061D+00/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA ELFLD  /8HELFLD   /
#else
      CHARACTER*8 :: ELFLD_STR
      EQUIVALENCE (ELFLD, ELFLD_STR)
      DATA ELFLD_STR/"ELFLD   "/
#endif
C
C     After projection onto the normal to the tessera,
C     the electric field integrals are written to disk file -nft27-.
C
C  1) Calculation of < chi(mu) | alpha/r | chi(nu) > integrals
C     (elect. field integrals) on representative points of tesserae.
C
      IEFLDOLD=IEFLD
      IEFLD=1
      CALL SEQREW(NFT27)
      DO ITS = 1, NTS
         ITS2=ITS+NTS
         XP=XCTS(ITS)
         YP=YCTS(ITS)
         ZP=ZCTS(ITS)
         CNX=(XP-XCTS(ITS2))/DR
         CNY=(YP-YCTS(ITS2))/DR
         CNZ=(ZP-ZCTS(ITS2))/DR
         CALL PRCALC(ELFLD,FLD,FLW,3,L2)
         DO IBAS = 1, L2
            BEMCHG(IBAS) = FLD(IBAS,1)*CNX
     *                   + FLD(IBAS,2)*CNY
     *                   + FLD(IBAS,3)*CNZ
         ENDDO
         CALL SQWRIT(NFT27,BEMCHG,L2)
      ENDDO
      CALL SEQREW(NFT27)
      IEFLD=IEFLDOLD
C
C  2) Calculation of apparent charges generated by the solute's nuclei.
C
      DO  ITS = 1, NTS
         XI=XCTS(ITS)
         YI=YCTS(ITS)
         ZI=ZCTS(ITS)
         ITS2=ITS+NTS
         CX=(XI-XCTS(ITS2))/DR
         CY=(YI-YCTS(ITS2))/DR
         CZ=(ZI-ZCTS(ITS2))/DR
         VEC(ITS) = ZERO
         DO  JATOM = 1, NAT
            XL=C(1,JATOM)
            YL=C(2,JATOM)
            ZL=C(3,JATOM)
            RIL= SQRT((XI-XL)**2+(YI-YL)**2+(ZI-ZL)**2)
            XM = ((XI-XL)*CX+(YI-YL)*CY+(ZI-ZL)*CZ) / RIL**3
            VEC(ITS) = VEC(ITS) + XM*ZAN(JATOM)
         ENDDO
      ENDDO
C         write(iw,*)'raja3'
C
      QNTOT = ZERO
      QNUC2 = ZERO
      DO ITS = 1, NTS
         QSN(ITS) = ZERO
         DO JTS = 1, NTS
            QSN(ITS) = QSN(ITS)+DMATM1(ITS,JTS)*VEC(JTS)
         ENDDO
         QSN(ITS) = QSN(ITS)*AS(ITS)
         QNUC2 = QNUC2 + VEC(ITS)*AS(ITS)
         QNTOT = QNTOT + QSN(ITS)
      ENDDO
      QNUC2 = QNUC2/FPI
      IF(SOME .AND. MASWRK) WRITE(IW,1200) QNUC2
C
C  4) Renormalization of `nuclear' apparent charges.
C     If ICOMP=1 the correction is proportional to the tessera's area.
C     If ICOMP=2 or ICOMP=3 the correction is equal for each tessera.
C
      CHG = ZERO
      DO JATOM = 1, NAT
         CHG = CHG + ZAN(JATOM)
      ENDDO
      TCH = - CHG * (EPS - ONE) / EPS
      QNTOTN = ZERO
C
C     Option 1:
C
      FN = ONE
      IF(ICOMP.EQ.1) THEN
         SUPTOT = STOT * ANTOAU * ANTOAU
         DIFF = TCH - QNTOT
         DO ITS = 1, NTS
            QSN(ITS) = QSN(ITS) + DIFF * AS(ITS) / SUPTOT
            QNTOTN = QNTOTN + QSN(ITS)
         ENDDO
C
C     Option 2:
C
      ELSE IF(ICOMP.EQ.2.OR.ICOMP.EQ.3) THEN
         FN = TCH / QNTOT
         DO ITS = 1, NTS
            QSN(ITS) = QSN(ITS) * FN
            QNTOTN = QNTOTN + QSN(ITS)
         ENDDO
      END IF
C
C     Total apparent charge before and after renormalization.
C
      IF(SOME .AND. MASWRK) THEN
         IF(ICOMP.NE.0) WRITE(IW,1000) QNTOT, TCH, QNTOTN
         IF(ICOMP.EQ.0) WRITE(IW,1100) QNTOT, TCH
      END IF
      RETURN
C
 1000 FORMAT(1X,'NUCLEAR APPARENT CHARGE',F10.5,' THEORETICAL',
     *          F10.5,' RENORMALIZED',F10.5)
 1100 FORMAT(1X,'NUCLEAR APPARENT CHARGE',F10.5,' THEORETICAL',
     *          F10.5,' NOT RENORMALIZED',F10.5)
 1200 FORMAT(/1X,'ESTIMATE OF NUCLEAR CHARGE',F15.5)
      END
C*MODULE PCM     *DECK XMATBF
      SUBROUTINE XMATBF(D,XX,Q,SCR,DMATM1,QET,QETN,TCH,NTSX,NUM2,NFT27)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
      DIMENSION D(NUM2),XX(NUM2),SCR(NUM2),Q(NTSX),DMATM1(NTSX,NTSX)
C
C   hui li
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
C   hui li
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
C
      PARAMETER (TOANGS=0.52917724924D+00, ANTOAU=1.0D+00/TOANGS)
C
      DATA ZERO, ONE/0.0D+00, 1.0D+00/
C
C     Routine for the calculation of matrix XX (interaction between
C     `electronic' apparent charges and electronic potential of the
C     solute):
C         XX(m,n) = sum_i[ V(m,n;i)QSE(i) ]
C         QSE(i) = a(i)sum_j[ DMATM1(i,j){sum_m,n[ D(m,n)dV(m,n;i)dn ]}
C     which is used in the SCF routine in order to correct two-electron
C     integrals.
C
C     Disk file NFT27 stores the normal components of the electric
C     field -dV(m,n;i)/dn.
C
      CALL SEQREW(NFT27)
C
      DO ITS = 1, NTSX
         CALL SQREAD(NFT27,SCR,NUM2)
         Q(ITS) = TRACEP(D,SCR,NUM)
      ENDDO
C
      CALL SEQREW(NFT27)
C
      QET = ZERO
      DO ITS = 1, NTSX
         QSE(ITS) = ZERO
         DO ITSJ = 1, NTSX
            QSE(ITS) = QSE(ITS)+DMATM1(ITS,ITSJ)*Q(ITSJ)
         ENDDO
         QSE(ITS) = QSE(ITS)*AS(ITS)
         QET = QET + QSE(ITS)
      ENDDO
C
C  4) Normalizzazione delle cariche virtuali elementari elettroniche.
C     Per ICOMP=1 la correzione e' distribuita in proporzione
C     all'area della tessera.
C     Per ICOMP=2 la correzione e' effettuata con fattori costanti.
C
      NELEC = NE
      TCH = NELEC * (EPS - ONE) / EPS
      QETN = ZERO
C
C     Option 1
C
      FE = ONE
      IF(ICOMP.EQ.1) THEN
         SUPTOT = STOT * ANTOAU * ANTOAU
         DIFF = TCH - QET
         DO ITS = 1, NTSX
            QSE(ITS) = QSE(ITS) + DIFF * AS(ITS) / SUPTOT
            QETN = QETN + QSE(ITS)
        ENDDO
C
C     Option 2
C
      ELSE IF(ICOMP.EQ.2) THEN
         FE = TCH / QET
         DO ITS = 1, NTSX
            QSE(ITS) = QSE(ITS) * FE
            QETN = QETN + QSE(ITS)
         ENDDO
      END IF
C
C     Print total induced charge before and after renormalization
C
      IF(NPRINT.EQ.5 .AND. MASWRK) THEN
         IF(ICOMP.NE.0) WRITE(IW,1000) QET, TCH, QETN
         IF(ICOMP.EQ.0) WRITE(IW,1100) QET, TCH
      END IF
C
C   Calculation of interaction matrices to be added to the Fock matrix
C
      CALL VCLR(XX,1,NUM2)
      PB = ZERO
      PC = ZERO
      PX = ZERO
C
      DO ITS = 1, NTSX
        CALL INTMEP(SCR,XCTS(ITS),YCTS(ITS),ZCTS(ITS))
        DO IBAS = 1, NUM2
          XX(IBAS) = XX(IBAS) - SCR(IBAS) * QSE(ITS)
C          WRITE(IW,*)'MAGNITUDE',XX(IBAS),SCR(IBAS),QSE(ITS)
        ENDDO
C
C     PB = interaction electrons-nuclear induced charges
C     PX = interaction electrons-electronic induced charges
C
        VEL =  - TRACEP(D,SCR,NUM)
        PB = PB + VEL * QSN(ITS)
        PX = PX + VEL * QSE(ITS)
C
C     PC = interaction nuclei-electronic induced charges
C
        DO JATOM = 1, NAT
          R2 = (C(1,JATOM)-XCTS(ITS))**2 + (C(2,JATOM)-
     *         YCTS(ITS))**2 + (C(3,JATOM)-ZCTS(ITS))**2
          R = SQRT(R2)
          PC = PC + QSE(ITS) * ZAN(JATOM) / R
        ENDDO
      ENDDO
      RETURN
C
 1000 FORMAT(1X,'ELECTRONIC APPARENT CHARGE',F10.5,' THEORETICAL ',
     * F10.5,' RENORMALIZED',F10.5)
 1100 FORMAT(1X,'ELECTRONIC APPARENT CHARGE',F10.5,' THEORETICAL ',
     * F10.5,' NOT RENORMALIZED',F10.5)
      END
C*MODULE PCM     *DECK INTMEP
      SUBROUTINE INTMEP(VALUE,XPP,YPP,ZPP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL IANDJ,NORM,DOUBLE
C
      DIMENSION VALUE(*)
      DIMENSION IX(35),IY(35),IZ(35),JX(35),JY(35),JZ(35),
     *          XIN(125),YIN(125),ZIN(125),WINT(225),WORK(225),
     *          DIJ(225),IJX(225),IJY(225),IJZ(225),
     *          HP(28),WP(28),MINP(7),MAXP(7)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      COMMON /HERMIT/ H1,H2(2),H3(3),H4(4),H5(5),H6(6),H7(7)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /WERMIT/ W1,W2(2),W3(3),W4(4),W5(5),W6(6),W7(7)
C
      EQUIVALENCE (HP(1),H1),(WP(1),W1)
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
      PARAMETER (PI212=1.1283791670955D+00)
      PARAMETER (SQRT3=1.73205080756888D+00)
      PARAMETER (SQRT5=2.23606797749979D+00)
      PARAMETER (SQRT7=2.64575131106459D+00)
      PARAMETER (RLN10=2.30258D+00)
      DATA MINP /1,2,4,7,11,16,22/
      DATA MAXP /1,3,6,10,15,21,28/
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
C
C       EVALUATE THE -MEP- VALUE FOR ALL POINTS GIVEN IN -XYZ-
C
      TOL  = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C                    LOOP OVER SHELLS II
C
      DO 510 II=1,NSHELL
      I    = KATOM(II)
      XI   = C(1,I)
      YI   = C(2,I)
      ZI   = C(3,I)
      I1   = KSTART(II)
      I2   = I1 + KNG(II) - 1
      LIT  = KTYPE(II)
      MINI = KMIN(II)
      MAXI = KMAX(II)
      LOCI = KLOC(II) - MINI
C
C                    LOOP OVER SHELLS JJ
C
      DO 500  JJ=1,II
C
      J    = KATOM(JJ)
      XJ   = C(1,J)
      YJ   = C(2,J)
      ZJ   = C(3,J)
      J1   = KSTART(JJ)
      J2   = J1 + KNG(JJ) - 1
      LJT  = KTYPE(JJ)
      MINJ = KMIN(JJ)
      MAXJ = KMAX(JJ)
      LOCJ = KLOC(JJ) - MINJ
C
      RR     = (XI-XJ)**2 + (YI-YJ)**2 + (ZI-ZJ)**2
      NROOTS = (LIT + LJT - 2)/2 + 1
      IANDJ  = II.EQ.JJ
C
C             PREPARE INDICES FOR PAIRS OF (I,J) ORBITALS
C
      IJ = 0
      MAX = MAXJ
      DO 100  I=MINI,MAXI
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
C
      IF (IANDJ) MAX = I
      DO 100  J=MINJ,MAX
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
  100 CONTINUE
C
      CALL VCLR(WORK,1,225)
C
C                     LOOP OVER PRIMITIVES IG
C
      JGMAX = J2
      DO 410  IG=I1,I2
      AI  = EX(IG)
      CSI = CS(IG)
      CPI = CP(IG)
      CDI = CD(IG)
      CFI = CF(IG)
      CGI = CG(IG)
C
C                     LOOP OVER PRIMITIVES JG
C
      IF (IANDJ) JGMAX = IG
      DO 400 JG=J1,JGMAX
      AJ  = EX(JG)
      CSJ = CS(JG)
      CPJ = CP(JG)
      CDJ = CD(JG)
      CFJ = CF(JG)
      CGJ = CG(JG)
C
      AA  = AI + AJ
      AA1 = ONE/AA
      FI  = PI212*AA1
C
      AAX = (AI*XI + AJ*XJ)
      AAY = (AI*YI + AJ*YJ)
      AAZ = (AI*ZI + AJ*ZJ)
C
      AX  = AAX*AA1
      AY  = AAY*AA1
      AZ  = AAZ*AA1
C
      DUM = AI*AJ*RR*AA1
      IF(DUM .GT. TOL) GO TO 400
      FAC = FI*EXP(-DUM)
C
C                       CALCULATE DENSITY FACTORS
C
      DOUBLE = IANDJ.AND.IG.NE.JG
      MAX = MAXJ
      NN  = 0
C
      DUM1 = ZERO
      DUM2 = ZERO
      DO 200 I = MINI,MAXI
         IF(I.EQ.1) DUM1=CSI*FAC
         IF(I.EQ.2) DUM1=CPI*FAC
         IF(I.EQ.5) DUM1=CDI*FAC
         IF(I.EQ.8.AND.NORM) DUM1=DUM1*SQRT3
         IF(I.EQ.11) DUM1 = CFI*FAC
         IF((I.EQ.14).AND.NORM) DUM1 = DUM1*SQRT5
         IF((I.EQ.20).AND.NORM) DUM1 = DUM1*SQRT3
         IF(I.EQ.21) DUM1 = CGI*FAC
         IF((I.EQ.24).AND.NORM) DUM1 = DUM1*SQRT7
         IF((I.EQ.30).AND.NORM) DUM1 = DUM1*SQRT5/SQRT3
         IF((I.EQ.33).AND.NORM) DUM1 = DUM1*SQRT3
         IF(IANDJ) MAX = I
         DO 180 J = MINJ,MAX
            NN = NN+1
            IF(J.EQ.1) THEN
              DUM2 = DUM1*CSJ
              IF(DOUBLE .AND. I.EQ.1) DUM2 = DUM2 + DUM2
              IF(DOUBLE .AND. I.GT.1) DUM2 = DUM2 + CSI*CPJ*FAC
C
            ELSE IF(J.EQ.2) THEN
              DUM2 = DUM1*CPJ
              IF(DOUBLE) DUM2 = DUM2 + DUM2
C
            ELSE IF(J.EQ.5) THEN
              DUM2 = DUM1*CDJ
              IF(DOUBLE) DUM2 = DUM2 + DUM2
C
            ELSE IF((J.EQ.8).AND.NORM) THEN
              DUM2 = DUM2*SQRT3
C
            ELSE IF (J.EQ.11) THEN
              DUM2 = DUM1*CFJ
              IF (DOUBLE) DUM2 = DUM2+DUM2
C
            ELSE IF ((J.EQ.14).AND.NORM) THEN
              DUM2 = DUM2*SQRT5
C
            ELSE IF ((J.EQ.20).AND.NORM) THEN
              DUM2 = DUM2*SQRT3
C
            ELSE IF (J.EQ.21) THEN
              DUM2 = DUM1*CGJ
              IF (DOUBLE) DUM2 = DUM2+DUM2
C
            ELSE IF ((J.EQ.24).AND.NORM) THEN
              DUM2 = DUM2*SQRT7
C
            ELSE IF ((J.EQ.30).AND.NORM) THEN
              DUM2 = DUM2*SQRT5/SQRT3
C
            ELSE IF ((J.EQ.33).AND.NORM) THEN
              DUM2 = DUM2*SQRT3
C
            END IF
C
            DIJ(NN) = DUM2
  180    CONTINUE
  200 CONTINUE
C
C       CALCULATE POINTS AND WEIGHTS FOR RYS POLYNOMIAL
C
      XX  = AA * ((AX-XPP)**2 + (AY-YPP)**2 + (AZ-ZPP)**2)
      IF (NROOTS.LE.3) CALL RT123
      IF (NROOTS.EQ.4) CALL ROOT4
      IF (NROOTS.EQ.5) CALL ROOT5
C
C       LOOP OVER ROOTS OF RYS POLYNOMIAL TO CALCULATE INTEGRALS
C
      MM = 0
      DO 340  K=1,NROOTS
C
      UU = AA*U(K)
      WW = W(K)
      TT = ONE/(AA+UU)
      T  = SQRT(TT)
C
      X0 = (AAX + UU*XPP)*TT
      Y0 = (AAY + UU*YPP)*TT
      Z0 = (AAZ + UU*ZPP)*TT
C
C      CALCULATE 1-DIMENSIONAL INTEGRALS OVER ALL ANGULAR MOMENTA
C
      IN = -5+MM
      DO 320  I=1,LIT
      IN = IN+5
      NI = I
C
      DO 320  J=1,LJT
      JN = IN+J
      NJ = J
C
C       EVALUATE MOMENT INTEGRALS USING GAUSS-HERMITE QUADRATURE:
C
      XINT0 = ZERO
      YINT0 = ZERO
      ZINT0 = ZERO
C
      NPTS = (NI + NJ - 2)/2 + 1
      IMIN = MINP(NPTS)
      IMAX = MAXP(NPTS)
C
      DO 310  IROOT=IMIN,IMAX
C
      DUM = WP(IROOT)
      PX = DUM
      PY = DUM
      PZ = DUM
C
      DUM = HP(IROOT)*T
      PTX = DUM + X0
      PTY = DUM + Y0
      PTZ = DUM + Z0
C
      AXI = PTX - XI
      AYI = PTY - YI
      AZI = PTZ - ZI
C
      BXI = PTX - XJ
      BYI = PTY - YJ
      BZI = PTZ - ZJ
C
      GO TO (250,240,230,220,210),NI
C
  210 PX = PX*AXI
      PY = PY*AYI
      PZ = PZ*AZI
C
  220 PX = PX*AXI
      PY = PY*AYI
      PZ = PZ*AZI
C
  230 PX = PX*AXI
      PY = PY*AYI
      PZ = PZ*AZI
C
  240 PX = PX*AXI
      PY = PY*AYI
      PZ = PZ*AZI
C
  250 CONTINUE
C
      GO TO (300,290,280,270,260),NJ
C
  260 PX = PX*BXI
      PY = PY*BYI
      PZ = PZ*BZI
C
  270 PX = PX*BXI
      PY = PY*BYI
      PZ = PZ*BZI
C
  280 PX = PX*BXI
      PY = PY*BYI
      PZ = PZ*BZI
C
  290 PX = PX*BXI
      PY = PY*BYI
      PZ = PZ*BZI
C
  300 CONTINUE
C
      XINT0 = XINT0 + PX
      YINT0 = YINT0 + PY
      ZINT0 = ZINT0 + PZ
C
  310 CONTINUE
C
      XIN(JN) = XINT0
      YIN(JN) = YINT0
      ZIN(JN) = ZINT0*WW
C
  320 CONTINUE
C
      MM = MM+25
  340 CONTINUE
C
C                      LOOP OVER ORBITAL PRODUCTS
C
      DO 360 I=1,IJ
         NX = IJX(I)
         NY = IJY(I)
         NZ = IJZ(I)
         SUM = ZERO
         MM = 0
         DO 350 K=1,NROOTS
            SUM = SUM + XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM)
            MM = MM+25
  350    CONTINUE
         WINT(I) = SUM*DIJ(I)
  360 CONTINUE
C
C              SET UP EXPECTATION VALUE MATRICES
C
      INDEX = 1
      MAX = MAXJ
      NN2 = 0
      DO 380 I=MINI,MAXI
         IF (IANDJ) MAX = I
         DO 370 J=MINJ,MAX
            INDEX = INDEX + 1
            NN2 = NN2 + 1
            WORK(INDEX) = WORK(INDEX) + WINT(NN2)
  370    CONTINUE
  380 CONTINUE
C
  400 CONTINUE
  410 CONTINUE
C
C        END OF LOOPS OVER PRIMITIVES
C
      INDEX = 1
      MAX = MAXJ
      DO 420  I=MINI,MAXI
         LI = LOCI + I
         IN = LI*(LI-1)/2
         IF (IANDJ) MAX = I
         DO 420  J=MINJ,MAX
            LJ = LOCJ + J
            JN = LJ + IN
            INDEX = INDEX + 1
            DW  = WORK(INDEX)
            VALUE(JN) = DW
  420 CONTINUE
C
  500 CONTINUE
  510 CONTINUE
C
C        END OF LOOPS OVER SHELLS
C
      RETURN
      END
C*MODULE PCM     *DECK VNN
      SUBROUTINE VNN
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /NONEQ / VDD(MXTS),QOR(MXTS),UNZOR,PORT
      COMMON /PCMPAR/ IPCM,NFT26,NFT27,IKREP,IEF,IP_F
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
C
      DATA ZERO, TWO  /0.0D+00, 2.0D+00/
C
C     Routine for calculation of free energy of interaction between
C     solute nuclei and the polarization charges induced by the
C     nuclei themselves.
C
      UNZ = ZERO
      UNZOR = ZERO
      DO ITS = 1, NTS
        DO JATOM = 1, NAT
          R2 = (C(1,JATOM)-XCTS(ITS))**2 + (C(2,JATOM)-
     *         YCTS(ITS))**2 + (C(3,JATOM)-ZCTS(ITS))**2
          R = SQRT(R2)
          UNZ = UNZ + QSN(ITS) * ZAN(JATOM) / R
          IF(IEF.EQ.5.OR.IEF.EQ.8)
     *       UNZOR = UNZOR + QOR(ITS) * ZAN(JATOM) / R
        ENDDO
      ENDDO
      UNZ = UNZ / TWO
      RETURN
      END
C*MODULE PCM     *DECK JMAT
      SUBROUTINE JMAT(VEC,VEC2,S,REP,CEL,NUM2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION VEC(NUM2),VEC2(NUM2),S(NUM2),REP(NUM2),CEL(NUM2)
C
      PARAMETER (MXTS=2500, MXTSPT=2*MXTS)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NONEQ / VAD(MXTS),QOR(MXTS),UNZOR,PORT
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMPAR/ IPCM,NFT26,NFT27,IKREP,IEF,IP_F
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RMC/8HMCSCF   /
#else
      CHARACTER*8 :: RMC_STR
      EQUIVALENCE (RMC, RMC_STR)
      DATA RMC_STR/"MCSCF   "/
#endif
C
C     This routine computes J matrix  (interaction between induced
C     nuclear charges and electronic potential due to the solute):
C         J(m,n) = sum[ V(m,n;i)qsn(i) ]
C     and if IKREP=1 h_(rep) matrix (repulsive interaction between
C     solute and solvent):
C          h_(rep)(m,n) = gamma*(S(m,n)+sum_i[E_i(m,n)*a(i)]/4PI)
C     (S=overlap matrix, E_i=matrix of normal components of the field)
C     These matrices are used to correct 1-e integrals obtained without
C     solvent (they are stored in file 11, or for MCSCF in file 87).
C
      CALL VCLR(VEC2,1,NUM2)
C
      DCOEF=1.0D+0
      IF (SCFTYP.EQ.RMC)DCOEF=0.50D+0
         DO ITS = 1, NTS
            QT=DCOEF*QSN(ITS)
            IF(IEF.EQ.5.OR.IEF.EQ.8)QT=DCOEF*QSN(ITS)+QOR(ITS)
            CALL INTMEP(VEC,XCTS(ITS),YCTS(ITS),ZCTS(ITS))
            DO IBAS = 1, NUM2
               VEC2(IBAS) = VEC2(IBAS) - VEC(IBAS)*QT
            ENDDO
         ENDDO
C
C     Add repulsion potential
C
      IF(IKREP.EQ.1) THEN
         CALL DAREAD(IDAF,IODA,S,NUM2,12,0)
         CALL POTREP(REP,S,CEL,NUM2)
      END IF
C
C     Read original 1-e integrals
C
      NH1=11
      IF (SCFTYP .EQ. RMC) NH1=87
      CALL DAREAD(IDAF,IODA,VEC,NUM2,NH1,0)
      DO IBAS = 1, NUM2
        IF(IKREP.EQ.1) VEC2(IBAS)=VEC2(IBAS)+REP(IBAS)
        VEC(IBAS) = VEC(IBAS) + VEC2(IBAS)
      ENDDO
C
C     Write modified 1-e integrals
C
      CALL DAWRIT(IDAF,IODA,VEC,NUM2,NH1,0)
      RETURN
      END
C*MODULE PCM     *DECK DDMM
      SUBROUTINE DDMM(DMATM1,WORK,IPVT,NTSX)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION DMATM1(NTSX,NTSX),WORK(NTSX),IPVT(NTSX)
      DIMENSION DET(2)
C
      PARAMETER (MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
C   hui li
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
C   hui li
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      COMMON /PCMCAV/ OMEGA,RET,FRO,ALPHA(MXSP),RIN(MXSP),ICENT,
     *                IPRINT,IRETCAV
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
C
      DATA ZERO,ONE/0.0D+00,1.0D+00/
      DATA TPI,FPI/6.28318531D+00,12.56637061D+00/
C
C        compute the d and then d-inverse matrix
C
      IF(IPRINT.EQ.1 .AND. MASWRK) THEN
         WRITE(IW,*) ' -------------------'
         WRITE(IW,*) ' -- MATRIX D^(-1) --'
         WRITE(IW,*) ' -------------------'
      END IF
C
C         compute the d matrix, stored at -dmatm1-
C
      DO 140 I=1,NTS
        L=ISPHE(I)
        ETA=SQRT(AS(I)/(FPI*RE(L)**2))
        DMATM1(I,I) = TPI*(ONE-ETA)-FPI*EPS/(EPS-ONE)
        K=I+NTS
        XI=XCTS(I)
        YI=YCTS(I)
        ZI=ZCTS(I)
        CX=(XCTS(K)-XCTS(I))/DR
        CY=(YCTS(K)-YCTS(I))/DR
        CZ=(ZCTS(K)-ZCTS(I))/DR
        DO 150 J=1,NTS
          IF(J.EQ.I) GO TO 150
          XJ=XCTS(J)
          YJ=YCTS(J)
          ZJ=ZCTS(J)
          RIJ=SQRT((XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2)
          IF(RIJ.GE.0.026D+00) THEN
             DIJ = AS(J) * ((XI-XJ)*CX+(YI-YJ)*CY+(ZI-ZJ)*CZ) / RIJ**3
          ELSE
             DIJ = ZERO
          END IF
          DMATM1(I,J) = DIJ
  150   CONTINUE
  140 CONTINUE
C
      IF(IPRINT.EQ.1 .AND. MASWRK) THEN
         WRITE(IW,*) ' DDMM: THE D MATRIX IS'
         CALL PRSQ(DMATM1,NTS,NTS,NTS)
      END IF
C
      INFO=0
      CALL DGEFA(DMATM1,NTS,NTS,IPVT,INFO)
      IF(INFO.NE.0) THEN
         IF(MASWRK) WRITE(IW,*) ' THE D MATRIX IS SINGULAR'
         CALL ABRT
         STOP
      END IF
C
      CALL DGEDI(DMATM1,NTS,NTS,IPVT,DET,WORK,01)
C
      IF(IPRINT.EQ.1 .AND. MASWRK) THEN
         WRITE(IW,*) ' DDMM: THE D INVERSE MATRIX IS'
         CALL PRSQ(DMATM1,NTS,NTS,NTS)
      END IF
      RETURN
      END
C*MODULE PCM     *DECK DISRPM
      SUBROUTINE DISRPM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXATM=500)
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
C
C          original values from Pisa were nta=50, ntb=30
C          ntb=3000 was selected at U.Iowa "for proteins"
C          the input description makes this sound like a overestimate,
C          but it is a bit hard to be sure what NTB should be.
C
C  hui li  change ntb back to 30.
C     ntb is the number of chemical elements in the solvent molecule
C     and never exeed 100. usually less than 30.
C
      NTA=NAT
      NTB=30
      CALL VALFM(LOADFM)
      LDKT   = LOADFM + 1
      LRWT   = LDKT   + NTB
      LNT    = LRWT   + NTB
      LRDIFF = LNT    + NTB
      LEPSI  = LRDIFF + NTB
      LSIGMA = LEPSI  + NTA
      LDKA   = LSIGMA + NTA
      LRWA   = LDKA   + NTA
      LAST   = LRWA   + NTA
      NEED = LAST - LOADFM - 1
C
C   hui li  call getfm(need)
      CALL GETFM(NEED)
      CALL DISREP(X(LDKT),X(LRWT),X(LNT),X(LRDIFF),X(LEPSI),X(LSIGMA),
     *            X(LDKA),X(LRWA),NTA,NTB)
C
      CALL RETFM(NEED)
      RETURN
      END
C*MODULE PCM     *DECK DISREP
      SUBROUTINE DISREP(DKT,RWT,NT,RDIFF,EPSI,SIGMA,DKA,RWA,NTA,NTB)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
      DIMENSION EPSI(NTA),SIGMA(NTA),DKA(NTA),RWA(NTA),
     *          DKT(NTB),RWT(NTB),NT(NTB),RDIFF(NTB)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMPAR/ IPCM,NFT26,NFT27,IKREP,IEF,IP_F
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMPRT/ GCAVP,GCAVS,GDISP,GREP,EHFGAS
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
C
      PARAMETER (TOANGS=0.52917724924D+00, ANTOAU=1.0D+00/TOANGS)
      PARAMETER (ZERO=0.0D+00)
C
      PARAMETER (NNAM=12)
      DIMENSION QNAM(NNAM),KQNAM(NNAM)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DISWD/8HDISREP  /
#else
      CHARACTER*8 :: DISWD_STR
      EQUIVALENCE (DISWD, DISWD_STR)
      DATA DISWD_STR/"DISREP  "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QNAM/8HICLAV   ,8HN       ,8HRHO     ,8HNT      ,8HRDIFF   ,
     *          8HDKT     ,8HRWT     ,8HDKA     ,8HRWA     ,8HILJ     ,
     *          8HEPSI    ,8HSIGMA   /
#else
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"ICLAV   ","N       ","RHO     ","NT      ",
     *          "RDIFF   ",
     *          "DKT     ","RWT     ","DKA     ","RWA     ","ILJ     ",
     *          "EPSI    ","SIGMA   "/
#endif
      DATA KQNAM/1,1,3,-1,-3,-3,-3,-3,-3,1,-3,-3/
C
C     Routine per il calcolo dell'energia di interazione dispersiva
C     e repulsiva tra il soluto e il solvente rappresentato con
C     il modello continuo.
C
C     Se ICLAV = 1:
C     l'interazione tra molecole e' rappresentata
C     come somma di interazioni atomo-atomo;
C     le interazioni somo rappresentate con una formula di Buckingham
C     (R^-6 per la dispersione, exp per la repulsione).
C
C     Cfr. : Pertsin-Kitaigorodsky "The atom-atom potential
C     method", pag. 146.
C
C     Per eseguire questo calcolo, si costruisce una nuova cavita'
C     per ogni atomo del solvente, tenendo conto dell'effettiva
C     distanza dell'atomo in questione dal soluto. In pratica le
C     nuove cavita' hanno i centri delle sfere iniziali invariati,
C     ma i raggi aumentati di RDIF. Per ogni atomo di solvente
C     viene letto RDIF, che dipende dal suo raggio di van der Waals
C     e dalla geometria della molecola di solvente.
C
C     Se ILJ = 1:
C     si considerano interazioni atomo di soluto - molecola di solvente,
C     e il potenziale usato e' di tipo Lennard-Jones (R^-6 per la
C     dispersione, R^-12 per la repulsione).
C
C
C       Se ICLAV = 1:
C       legge i dati riferiti al solvente (T) e al soluto (A):
C            N = numero di atomi diversi in una molecola di solvente
C            RHO = densita' numerale del solvente
C            NT(I) = numero di atomi del tipo i in 1 molec. di solvente
C            RDIFF(I) = distanza dei primi atomi di tipo I dalla cavita'
C                       originaria
C            DK, RW = parametri dell'interazione atomo-atomo
C
C       Se ILJ = 1
C       legge i dati relativi al potenziale di Lennard-Jones:
C       EPSI(I) = costante relativa all'energia, riferita all'i-esimo
C                 atomo del soluto
C       SIGMA(I)= distanza tipica, riferita all'i-esimo atomo del soluto
C       RHO     = densita' numerale del solvente
C
C       I dati relativi agli atomi del soluto devono corrispondere
C       all'ordine con cui PEDRA elenca i centri.
C
C     SPECIFY DEFAULT PARAMETERS
C     SOLVENT: WATER
C
      ICLAV=1
      N=2
      RHO=3.348D-02
      ILJ = 0
      CALL VICLR(NT,1,NTB)
      NT(1)=2
      NT(2)=1
      CALL VCLR(RDIFF,1,NTB)
      RDIFF(1)=1.20D+00
      RDIFF(2)=1.50D+00
      CALL VCLR(DKT,1,NTB)
      DKT(1)=1.0D+00
      DKT(2)=1.36D+00
      CALL VCLR(RWT,1,NTB)
      RWT(1)=1.2D+00
      RWT(2)=1.5D+00
      KQNAM(4) = 10*NTB + 3
      KQNAM(5) = 10*NTB + 3
      KQNAM(6) = 10*NTB + 3
      KQNAM(7) = 10*NTB + 3
      CALL VCLR(DKA,1,NTA)
      CALL VCLR(RWA,1,NTA)
C hui li  define default DKA and RWA
      DO I = 1, NAT
        IF(ZAN(I).EQ.1) DKA(I)=1.00D+00
        IF(ZAN(I).EQ.1) RWA(I)=1.20D+00
C     NOTE:Be and B have the same value as C
        IF(ZAN(I).EQ.4) DKA(I)=1.00D+00
        IF(ZAN(I).EQ.4) RWA(I)=1.72D+00
        IF(ZAN(I).EQ.5) DKA(I)=1.00D+00
        IF(ZAN(I).EQ.5) RWA(I)=1.72D+00
C
        IF(ZAN(I).EQ.6) DKA(I)=1.00D+00
        IF(ZAN(I).EQ.6) RWA(I)=1.72D+00
        IF(ZAN(I).EQ.7) DKA(I)=1.10D+00
        IF(ZAN(I).EQ.7) RWA(I)=1.60D+00
        IF(ZAN(I).EQ.8) DKA(I)=1.36D+00
        IF(ZAN(I).EQ.8) RWA(I)=1.50D+00
        IF(ZAN(I).EQ.15) DKA(I)=2.10D+00
        IF(ZAN(I).EQ.15) RWA(I)=1.85D+00
        IF(ZAN(I).EQ.16) DKA(I)=1.40D+00
        IF(ZAN(I).EQ.16) RWA(I)=1.80D+00
      ENDDO
C
      CALL VCLR(EPSI,1,NTA)
      CALL VCLR(SIGMA,1,NTA)
      KQNAM( 8) = 10*NTA + 3
      KQNAM( 9) = 10*NTA + 3
      KQNAM(11) = 10*NTA + 3
      KQNAM(12) = 10*NTA + 3
C
      JRET=0
      CALL NAMEIO(IR,JRET,DISWD,NNAM,QNAM,KQNAM,
     *            ICLAV, N, RHO, NT, RDIFF, DKT, RWT,
     *            DKA, RWA, ILJ, EPSI, SIGMA,
     *            0,0,
     *    0,0,0,0,0,  0,0,0,0,0,
     *    0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,
     *    0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0)
C
      IF(JRET.EQ.2) THEN
         IF(MASWRK.AND.NPRINT.NE.817) WRITE(IW,930) DISWD
         CALL ABRT
      END IF
C
      IF(JRET.EQ.0.AND.MASWRK.AND.NPRINT.NE.817) THEN
       WRITE(IW,950) ICLAV,N,RHO,ILJ
        WRITE(IW,*)'      ATOM      DK        RW '
        WRITE(IW,*)'------------------------------'
       WRITE(IW,*)'1)        SOLVENT'
       DO I=1,N
        WRITE(IW,960)I,DKT(I),RWT(I)
       ENDDO
        WRITE(IW,*)'------------------------------'
       WRITE(IW,*)'2)        SOLUTE '
       DO I=1,NAT
        WRITE(IW,960) I,DKA(I),RWA(I)
       ENDDO
      END IF
C
  930 FORMAT(1X,'**** ERROR IN $',A8,' INPUT')
  950 FORMAT(/5X,'INPUT FOR PCM DISP-REP CALCULATION '/5X,35("-")/
     *5X,'ICLAV=',I4,5X,'N  =',I4,5X,'RHO=',F8.4,2X,'ILJ=',I4,5X/)
  960 FORMAT(5X,I4,5X,F6.3,5X,F6.3)
C
C       Calcola l'energia libera di dispersione e di repulsione.
C       I dati geometrici relativi al soluto sono convertiti in
C       ANGSTROM prima di essere usati.
C
      IF (ICLAV.NE.1) GO TO 1000
      IF(IKREP.NE.1) THEN
       IF(MASWRK.AND.NPRINT.NE.817) WRITE (IW, 400)
      ELSE
       IF(MASWRK.AND.NPRINT.NE.817) WRITE (IW, 401)
      END IF
C
C       Singola cavita'.
C       Loop sugli atomi (diversi) del solvente.
C
        SUMTD = ZERO
        SUMTR = ZERO
        DO 100 M = 1, N
C
C       Costruisce una nuova cavita' per ogni tipo di atomo del solvente
C
        IF(MASWRK.AND.NPRINT.NE.817) WRITE (IW, 50) M
        RDIF = RDIFF(M)
        IDISP = 2
C       CALL PEDRAM
        CALL PEDFSM
        FACT = NT(M) * RHO
C
C       Loop sugli atomi di soluto.
C
        SUMAD = ZERO
        SUMAR = ZERO
        DO 200 I = 1, NAT
          RWAT = 2.0D+00 * SQRT(RWA(I)*RWT(M))
          XA = C(1,I) / ANTOAU
          YA = C(2,I) / ANTOAU
          ZA = C(3,I) / ANTOAU
C       Loop sulle tessere.
          SUMKD = ZERO
          SUMKR = ZERO
          DO 300 K = 1, NTS
            L = ISPHE(K)
            X = XCTS(K) / ANTOAU
            Y = YCTS(K) / ANTOAU
            Z = ZCTS(K) / ANTOAU
            AREA = AS(K) / (ANTOAU*ANTOAU)
            R2 = (XA-X)**2 + (YA-Y)**2 + (ZA-Z)**2
            R = SQRT(R2)
            XC = XE(L) / ANTOAU
            YC = YE(L) / ANTOAU
            ZC = ZE(L) / ANTOAU
            RL = RE(L) / ANTOAU
            RL2 = RL*RL
            DR2 = (XA-XC)**2 + (YA-YC)**2 + (ZA-ZC)**2
            RAPP = (RL2 + R2 - DR2) / (2.0D+00 * RL)
            DAT = 0.214D+00 * DKA(I) * DKT(M) * RWAT**6
            BAT = 47000.0D+00 * DKA(I) * DKT(M)
            ALPHAT = 12.35D+00 / RWAT
C
C       Parte dispersiva.
C
            R6 = R2*R2*R2
            AD = - DAT / (3.0D+00 * R6)
            S  =  AREA * RAPP * AD
            SUMKD = SUMKD + S
C
C       Parte repulsiva.
C
            IF(IKREP.NE.1) THEN
            S1  = ALPHAT * R
            S   = 1.0D+00/S1 + 2.0D+00/(S1*S1) + 2.0D+00/(S1*S1*S1)
            ESP = EXP(- ALPHAT * R)
            AR  = BAT * ESP * S
            SUMKR = SUMKR + (AREA * AR * RAPP)
            END IF
C
  300     CONTINUE
          SUMAD = SUMAD + SUMKD
          IF(IKREP.NE.1)SUMAR = SUMAR + SUMKR
  200   CONTINUE
        SUMTD = SUMTD + FACT * SUMAD
        IF(IKREP.NE.1)SUMTR = SUMTR + FACT * SUMAR
  100   CONTINUE
        GDISP = SUMTD
        IF(IKREP.NE.1) THEN
         GREP = SUMTR
         IF(MASWRK.AND.NPRINT.NE.817) WRITE (IW, 500)GDISP, GREP
        ELSE
         IF(MASWRK.AND.NPRINT.NE.817) WRITE (IW, 501)GDISP
        END IF
      GO TO 1100
 1000 CONTINUE
C
C       Potenziale Lennard-Jones.
C
        IF (ILJ.NE.1) GO TO 1100
C
C       Calcola l'energia libera di dispersione e di repulsione.
C       I dati geometrici relativi al soluto sono convertiti in
C       ANGSTROM prima di essere usati.
C
C       Singola cavita'.
C
      IF(MASWRK.AND.NPRINT.NE.817) WRITE (IW, 450)
C
C       Essendo questo potenziale molecolare, viene costruita una
C       sola cavita', con raggio aumentato di RSOLV
C
        RDIF = RSOLV
        IDISP = 2
C       CALL PEDRAM
        CALL PEDFSM
C
C       Loop sugli atomi di soluto.
C
        SUMAD = 0.0D+00
        SUMAR = 0.0D+00
        DO 110 I = 1, NAT
        XA = C(1,I) / ANTOAU
        YA = C(2,I) / ANTOAU
        ZA = C(3,I) / ANTOAU
C
C       Loop sulle tessere.
C
        SUMKD = 0.0D+00
        SUMKR = 0.0D+00
        DO 310 K = 1, NTS
        L = ISPHE(K)
        X = XCTS(K) / ANTOAU
        Y = YCTS(K) / ANTOAU
        Z = ZCTS(K) / ANTOAU
        AREA = AS(K) / (ANTOAU*ANTOAU)
        R2 = (XA-X)**2 + (YA-Y)**2 + (ZA-Z)**2
        R = SQRT(R2)
        R2 = R*R
        XC = XE(L) / ANTOAU
        YC = YE(L) / ANTOAU
        ZC = ZE(L) / ANTOAU
        RL = RE(L) / ANTOAU
        RL2  = RL*RL
        DR2  = (XA-XC)**2 + (YA-YC)**2 + (ZA-ZC)**2
        RAPP = (RL2 + R2 - DR2) / (2.0D+00 * RL)
        DAT = 4.0D+00 * EPSI(I) * SIGMA(I)**6
        CAT = 4.0D+00 * EPSI(I) * SIGMA(I)**12
C
C       Parte dispersiva.
C
        R6 = R2*R2*R2
        S  = - AREA * RAPP * DAT / (3.0D+00 * R6)
        SUMKD = SUMKD + S
C
C       Parte repulsiva.
C
        R12 = R6 * R6
        S   = AREA * RAPP * CAT / (9.0D+00 * R12)
        SUMKR = SUMKR + S
  310   CONTINUE
        SUMAD = SUMAD + SUMKD
        SUMAR = SUMAR + SUMKR
  110   CONTINUE
        GDISP = RHO * SUMAD
        GREP  = RHO * SUMAR
      IF(MASWRK.AND.NPRINT.NE.817) WRITE (IW, 500)GDISP, GREP
C
 1100 CONTINUE
      IF(MASWRK.AND.NPRINT.NE.817) WRITE(IW,*)
     *      '..... DONE COMPUTING DISPERSION-REPULSION .....'
      IF(NPRINT.NE.817)CALL TIMIT(1)
      RETURN
   50 FORMAT(/, 'CAVITY REFERRED TO SOLVENT ATOM OF TYPE',I3,/)
  400 FORMAT(/,'DISPERSION-REPULSION FREE ENERGY: ',
     *         ' ATOM-ATOM INTERACTION.',/)
  401 FORMAT(/,'DISPERSION FREE ENERGY: ',
     *         ' ATOM-ATOM INTERACTION.',/)
  450 FORMAT(/,'DISPERSION-REPULSION FREE ENERGY: ',
     *         ' LENNARD-JONES POTENTIAL.',/)
  500 FORMAT(//,'   GDISP =', 3X,F15.8,3X,'KCAL/MOL',/,
     *  '   GREP  =', 3X,F15.8,3X,'KCAL/MOL')
  501 FORMAT(//,'   GDISP =', 3X,F15.8,3X,'KCAL/MOL')
      END
C*MODULE PCM     *DECK SOLPRT
      SUBROUTINE SOLPRT
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXTS=2500, MXFRG=50, MXSP=250)
C
C   hui li
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /CONV  / DENTOL,EN,ET,EHF,EHF0,DIFF,ITER,ICALP,ICBET
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
C   hui li
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMDIS/ WB,WA,ETA2,GD,EVAC,IDP
      COMMON /PCMEFP/ VEC_MUL(MXTS),AIND_PROJ(MXTS),FIND_PROJ(MXTS),
     *                PB_MUL, PC_MUL,PB_POL,PC_POL,
     *                P_FF,P_NF,P_FN,P_I,P_IBIS,P_J,P_NUCC,P_NUCCBIS,
     *                ENPCM
      COMMON /PCMITR/ IPCMIT,LIST(MXSP+1),IMUL,MXDIIS,RCUT(2),
     *                THRES,NREG,MXITR1,MXITR2
      COMMON /PCMPAR/ IPCM,NFT26,NFT27,IKREP,IEF,IP_F
      COMMON /PCMPRT/ GCAVP,GCAVS,GDISP,GRP,EHFGAS
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
      DATA AUTOKAL /627.509541D+00/
      DATA ZERO /0.0D+00/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA ENERGY  /8HENERGY  /
#else
      CHARACTER*8 :: ENERGY_STR
      EQUIVALENCE (ENERGY, ENERGY_STR)
      DATA ENERGY_STR/"ENERGY  "/
#endif
C
C  ------------------------------------------------------------
C
C    ETOT= total energy of the system solute-solvent
C    EINT= internal energy of the solute
C    EDELTA=EINT-EVAC= variation of internal energy
C          = <PSI| H(0) |PSI> - <PSI(0)| H(0) |PSI(0)>
C
C    Electrostatic contribution:
C    GES = 1/2 (PB + PC) + 1/2 PX + UNZ
C
C    PB = interaction solute electrons - polarization nuclear charges
C             PB = sum_i [Vel(i) * QSN(i)]
C
C    PC = interaction solute nuclei - polarization electronic charges
C             PC = sum_i [Vnuc(i) * QSE(i)]
C
C    PX = interaction solute electrons - polarization electronic charges
C             PX = sum_i [Vel(i) * QSE(i)]
C
C    UNZ = 1/2 interaction nuclei - polarization nuclear charges
C
C    GCAVP = cavitation energy (Pierotti's theory)
C    GCAVS = cavitation energy (Sinanoglu's theory)
C
C    GDISP = dispersion free energy
C    GREP =  repulsion free energy
C  --------------------------------------------------------------
C
      ETOT = ET
C
C     hui li 04/28/2003  remove the cav+dis+rep energies.
      IF(ICAV.EQ.1)ETOT=ETOT-GCAVP/AUTOKAL
      IF(IDISP.EQ.1)ETOT=ETOT-(GDISP+GRP)/AUTOKAL
C
C     Electrostatic contribution
C
      GES = 0.5D+00 * (PB + PC) + 0.5D+00 * PX + UNZ
     *    + 0.5D+00 * (P_I+P_IBIS+P_J+P_NUCC+P_NUCCBIS)
      IF(IPCMIT.NE.0) GES = 0.50D+00 * ENPCM
      EINT = ETOT - GES
C
C     Cavitation contribution: 1-Pierotti; 2-Sinanoglu
C
      GCAV = 0.0D+00
      IF (ICAV.EQ.1) GCAV = GCAVP
      IF (ICAV.EQ.2) GCAV = GCAVS
C
C     Dispersive and repulsive contributions
C
      IF(IDISP.EQ.0) THEN
         GDISP=ZERO
         GREP=ZERO
      ELSE
         GREP=GRP
      END IF
C
C     SCF method:
C
      IF(IKREP.EQ.1) THEN
         GREP = GRP*AUTOKAL
         EINT=EINT-GRP
      END IF
      IF(IDP.EQ.1) THEN
         EINT=EINT-GD
         GDISP = GD*AUTOKAL
         EDELTA=EINT-EVAC
      ELSE
         EDELTA=ZERO
      END IF
C
      GINT = GES + GCAV/AUTOKAL + GDISP/AUTOKAL + GREP/AUTOKAL
      GTOT = ETOT + GCAV/AUTOKAL
      IF(IDISP.NE.0)
     * GTOT = GTOT + GDISP/AUTOKAL + GREP/AUTOKAL
C
      IF(MASWRK)THEN
        WRITE(IW,150)
        WRITE(IW,100) ETOT,EINT,EDELTA,GES,
     *                GCAV/AUTOKAL,GDISP/AUTOKAL,GREP/AUTOKAL,
     *                GINT,GTOT
C    hui li  print details of electrostatic interaction
        IPOT=IEFC+IEFD+IEFQ+IEFO+IEFP+IREP
        IF(IPOT.GT.0) THEN
          IF(IPCMIT.EQ.0)THEN
          E_EE=0.50D+00*PX*AUTOKAL
          E_EN=0.50D+00*(PB-PB_MUL-PB_POL)*AUTOKAL
          E_EM=0.50D+00*PB_MUL*AUTOKAL
          E_EP=0.50D+00*PB_POL*AUTOKAL
          E_NE=0.50D+00*(PC-PC_MUL-PC_POL)*AUTOKAL
          E_NN=(UNZ-0.50D+00*(P_FF+P_NF+P_FN))*AUTOKAL
          E_NM=0.50D+00*P_NF*AUTOKAL
          E_NP=0.50D+00*P_I*AUTOKAL
          E_ME=0.50D+00*PC_MUL*AUTOKAL
          E_MN=0.50D+00*P_FN*AUTOKAL
          E_MM=0.50D+00*P_FF*AUTOKAL
          E_MP=0.50D+00*P_NUCC*AUTOKAL
          E_PE=0.50D+00*PC_POL*AUTOKAL
          E_PN=0.50D+00*P_IBIS*AUTOKAL
          E_PM=0.50D+00*P_NUCCBIS*AUTOKAL
          E_PP=0.50D+00*P_J*AUTOKAL
          E_TT=E_EE+E_EN+E_EM+E_EP+E_NE+E_NN+E_NM+E_NP+
     *           E_ME+E_MN+E_MM+E_MP+E_PE+E_PN+E_PM+E_PP
          WRITE(IW,101)E_EE,E_EN,E_EM,E_EP,E_NE,E_NN,E_NM,E_NP,
     *           E_ME,E_MN,E_MM,E_MP,E_PE,E_PN,E_PM,E_PP,E_TT
          ELSE
          E_TT=0.50D+00*ENPCM*AUTOKAL
          WRITE(IW,103)E_TT
          END IF
        ELSE
        WRITE(IW,102) PB,PC,PX,UNZ
        END IF
        WRITE(IW,105) ETOT*AUTOKAL,EINT*AUTOKAL,
     *                EDELTA*AUTOKAL,GES*AUTOKAL,
     *                GCAV,GDISP,GREP,
     *                GINT*AUTOKAL,GTOT*AUTOKAL
        WRITE(IW,110)
C
C     calculate and print the correct solvation energy,
C     which are the difference between pcm and gas
C
      IF(RUNTYP.EQ.ENERGY)THEN
      WRITE(IW,120)  (ETOT-EHFGAS)*AUTOKAL,
     *               (ETOT-EHFGAS)*AUTOKAL+GCAV,
     *               (ETOT-EHFGAS)*AUTOKAL+GCAV+GDISP+GREP
      END IF
C
        WRITE(IW,*) '.... DONE PRINTING PCM SOLVENT SUMMARY ....'
        CALL TIMIT(1)
      END IF
C
      RETURN
C
  150 FORMAT(/13X,'----------------------------------------------'/
     *        13X,'-------   RESULTS OF PCM CALCULATION   -------'/
     *        13X,'----------------------------------------------'/)
  100 FORMAT(1X,'FREE ENERGY IN SOLVENT = <PSI| H(0)+V/2 |PSI>       =',
     *          F20.10,' A.U.'/
     *       1X,'INTERNAL ENERGY IN SOLVENT = <PSI| H(0) |PSI>       =',
     *          F20.10,' A.U.'/
     *       1X,'DELTA INTERNAL ENERGY =  <D-PSI| H(0) |D-PSI>       =',
     *          F20.10,' A.U.'/
     *       1X,'ELECTROSTATIC INTERACTION = 1/2(PB+PC) + 1/2PX + UNZ=',
     *          F20.10,' A.U.'/
     *       1X,'PIEROTTI CAVITATION ENERGY                          =',
     *          F20.10,' A.U.'/
     *       1X,'DISPERSION FREE ENERGY                              =',
     *          F20.10,' A.U.'/
     *       1X,'REPULSION FREE ENERGY                               =',
     *          F20.10,' A.U.'/
     *       1X,'TOTAL INTERACTION (DELTA + ES + CAV + DISP + REP)   =',
     *          F20.10,' A.U.'/
     *       1X,'TOTAL FREE ENERGY IN SOLVENT                        =',
     *          F20.10,' A.U.')
  101 FORMAT(
     * /11X,'-------   EFP/PCM ELECTROSTATIC INTERATIONS   -------'/
     *  11X,'--------------------  KCAL/MOL  ---------------------'//
     *  23X,'      ELECTRON        NUCLEI     MULTIPOLE   POLARIZABLE'/
     *  23X,'       INDUCED       INDUCED       INDUCED       INDUCED'/
     *  23X,'        CHARGE        CHARGE        CHARGE        CHARGE'/
     *  1X,'ELECTRONIC  POTENTIAL ',4F14.2/
     *  1X,'NUCLEAR     POTENTIAL ',4F14.2/
     *  1X,'MULTIPOLE   POTENTIAL ',4F14.2/
     *  1X,'POLARIZABLE POTENTIAL ',4F14.2/
     *  11X,'----------------- TOTAL: ',F10.2,' -----------------'//)
  102 FORMAT(5X,'PB =',F15.8,' A.U.   PC =',F15.8,' A.U.'/
     *       5X,'PX =',F15.8,' A.U.  UNZ =',F15.8,' A.U.')
  103 FORMAT(
     * /11X,'-------   EFP/PCM ELECTROSTATIC INTERATIONS   -------'/
     *  11X,'----------------  ITERATIVE METHOD  -----------------'//
     *  11X,'------------- TOTAL: ',F10.2,' KCAL/MOL-------------'//)
  105 FORMAT(/1X,'FREE ENERGY IN SOLVENT       =',F15.2,' KCAL/MOL'/
     *        1X,'INTERNAL ENERGY IN SOLVENT   =',F15.2,' KCAL/MOL'/
     *        1X,'DELTA INTERNAL ENERGY        =',F15.2,' KCAL/MOL'/
     *        1X,'ELECTROSTATIC INTERACTION    =',F15.2,' KCAL/MOL'/
     *        1X,'PIEROTTI CAVITATION ENERGY   =',F15.2,' KCAL/MOL'/
     *        1X,'DISPERSION FREE ENERGY       =',F15.2,' KCAL/MOL'/
     *        1X,'REPULSION FREE ENERGY        =',F15.2,' KCAL/MOL'/
     *        1X,'TOTAL INTERACTION            =',F15.2,' KCAL/MOL'/
     *        1X,'TOTAL FREE ENERGY IN SOLVENT =',F15.2,' KCAL/MOL')
  110 FORMAT(//1X,'-INTERNAL ENERGY IN SOLVENT- OMITS ES,CAV,DISP,REP'/
     *         1X,'-FREE ENERGY IN SOLVENT- INCLUDES ES,DISP,REP,'/
     *    1X,'WHICH ARE INCLUDED IN THE SELF-CONSISTENT WAVEFUNCTION.'/
     *         1X,'-TOTAL FREE ENERGY IN SOLVENT- ALSO INCLUDES CAV,'/
     *    1X,'AND IS THUS THE CORRECT TOTAL ENERGY FOR MOST PURPOSES.')
  120 FORMAT(
     * /11X,'-----  ENERGY CHANGE FROM GAS PHASE TO SOLVENT  -----'/
     *  11X,'--------------------  KCAL/MOL  ---------------------'//
     *  11X,'           ELEC          ELEC+CAV    ELEC+CAV+DIS+REP'/
     *  1X,'DELTAG =',F17.3,F18.3,F20.3//)
      END
C*MODULE PCM     *DECK DATSOL
      SUBROUTINE DATSOL(ZSOL,EPS,EPSINF,RSOLV,VMOL,TCE,STEN,DSTEN,CMF)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C   hui li
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
C
C   hui li
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      CHARACTER*8 ZSOL
C
C     Database of optical and physical data for various solvents
C
      IF(ZSOL.EQ.'WATER   '  .OR.  ZSOL.EQ.'H2O     ') THEN
        EPS = 78.39D+00
        EPSINF = 1.776D+00
        RSOLV = 1.385D+00
        VMOL = 18.07D+00
        TCE = 2.57D-04
        STEN = 71.81D+00
        DSTEN = 0.650D+00
        CMF = 1.277D+00
      ELSE IF(ZSOL.EQ.'CH3OH   ') THEN
        EPS = 32.63D+00
        EPSINF = 1.758D+00
        RSOLV = 1.855D+00
        VMOL = 40.7D+00
        TCE = 1.182D-03
        STEN = 22.12D+00
        DSTEN = 1.154D+00
        CMF = 1.776D+00
      ELSE IF(ZSOL.EQ.'C2H5OH  ') THEN
        EPS = 24.55D+00
        EPSINF = 1.847D+00
        RSOLV = 2.180D+00
        VMOL = 58.7D+00
        TCE = 1.103D-03
        STEN = 21.89D+00
        DSTEN = 1.146D+00
        CMF = 1.543D+00
      ELSE IF(ZSOL.EQ.'CLFORM  '  .OR.  ZSOL.EQ.'CHCL3   ') THEN
        EPS = 4.90D+00
        EPSINF = 2.085D+00
        RSOLV = 2.48D+00
        VMOL = 80.7D+00
        TCE = 1.255D-03
        STEN = 26.53D+00
        DSTEN = 0.0D+00
        CMF = 0.0D+00
      ELSE IF(ZSOL.EQ.'METHYCL '  .OR.  ZSOL.EQ.'CH2CL2  ') THEN
        EPS = 8.93D+00
        EPSINF = 2.020D+00
        RSOLV = 2.27D+00
        VMOL = 64.5D+00
        TCE = 1.367D-03
        STEN = 27.33D+00
        DSTEN = 0.0D+00
        CMF = 0.0D+00
      ELSE IF(ZSOL.EQ.'12DCLET '  .OR.  ZSOL.EQ.'C2H4CL2 ') THEN
        EPS = 10.36D+00
        EPSINF = 2.085D+00
        RSOLV = 2.505D+00
        VMOL = 79.4D+00
        TCE = 1.156D-03
        STEN = 31.54D+00
        DSTEN = 0.0D+00
        CMF = 0.0D+00
      ELSE IF(ZSOL.EQ.'CTCL    '  .OR.  ZSOL.EQ.'CCL4    ') THEN
        EPS = 2.228D+00
        EPSINF = 2.129D+00
        RSOLV = 2.685D+00
        VMOL = 96.5D+00
        TCE = 1.270D-03
        STEN = 26.15D+00
        DSTEN = 1.436D+00
        CMF = 0.629D+00
      ELSE IF(ZSOL.EQ.'BENZENE '  .OR.  ZSOL.EQ.'C6H6    ') THEN
        EPS = 2.247D+00
        EPSINF = 2.244D+00
        RSOLV = 2.63D+00
        VMOL = 88.91D+00
        TCE = 1.380D-03
        STEN = 28.18D+00
        DSTEN = 1.469D+00
        CMF = 0.629D+00
      ELSE IF(ZSOL.EQ.'TOLUENE '  .OR.  ZSOL.EQ.'C6H5CH3 ') THEN
        EPS = 2.379D+00
        EPSINF = 2.232D+00
        RSOLV = 2.82D+00
        VMOL = 106.3D+00
        TCE = 1.08D-03
        STEN = 27.92D+00
        DSTEN = 1.391D+00
        CMF = 0.679D+00
      ELSE IF(ZSOL.EQ.'CLBENZ  '  .OR.  ZSOL.EQ.'C6H5CL  ') THEN
        EPS = 5.621D+00
        EPSINF = 2.320D+00
        RSOLV = 2.805D+00
        VMOL = 101.79D+00
        TCE = 0.981D-03
        STEN = 32.69D+00
        DSTEN = 0.0D+00
        CMF = 0.0D+00
      ELSE IF(ZSOL.EQ.'NITMET  '  .OR.  ZSOL.EQ.'CH3NO2  ') THEN
        EPS = 38.20D+00
        EPSINF = 1.904D+00
        RSOLV = 2.155D+00
        VMOL = 53.68D+00
        TCE = 1.192D-03
        STEN = 36.47D+00
        DSTEN = 1.373D+00
        CMF = 0.808D+00
      ELSE IF(ZSOL.EQ.'NEPTANE '  .OR.  ZSOL.EQ.'C7H16   ') THEN
        EPS = 1.92D+00
        EPSINF = 1.918D+00
        RSOLV = 3.125D+00
        VMOL = 146.56D+00
        TCE = 1.25D-03
        STEN = 19.80D+00
        DSTEN = 1.505D+00
        CMF = 0.687D+00
      ELSE IF(ZSOL.EQ.'CYCHEX  '  .OR.  ZSOL.EQ.'C6H12   ') THEN
        EPS = 2.023D+00
        EPSINF = 2.028D+00
        RSOLV = 2.815D+00
        VMOL = 108.10D+00
        TCE = 1.20D-03
        STEN = 24.38D+00
        DSTEN = 1.467D+00
        CMF = 0.621D+00
      ELSE IF(ZSOL.EQ.'ANILINE '  .OR.  ZSOL.EQ.'C6H5NH2 ') THEN
        EPS = 6.89D+00
        EPSINF = 2.506D+00
        RSOLV = 2.80D+00
        VMOL = 91.15D+00
        TCE = 0.85D-03
        STEN = 42.79D+00
        DSTEN = 0.731D+00
        CMF = 0.972D+00
      ELSE IF(ZSOL.EQ.'ACETONE '  .OR.  ZSOL.EQ.'CH3COCH3') THEN
        EPS = 20.7D+00
        EPSINF = 1.841D+00
        RSOLV = 2.38D+00
        VMOL = 73.52D+00
        TCE = 1.42D-03
        STEN = 22.67D+00
        DSTEN = 0.0D+00
        CMF = 0.0D+00
      ELSE IF(ZSOL.EQ.'THF     ') THEN
        EPS = 7.58D+00
        EPSINF = 1.971D+00
        RSOLV = 2.9D+00
        VMOL = 81.11D+00
        TCE = 1.142D-03
        STEN = 26.40D+00
        DSTEN = 0.0D+00
        CMF = 0.0D+00
      ELSE IF(ZSOL.EQ.'DMETSOX '  .OR.  ZSOL.EQ.'DMSO    ') THEN
        EPS = 46.7D+00
        EPSINF = 2.179D+00
        RSOLV = 2.455D+00
        VMOL = 70.94D+00
        TCE = 9.82D-02
        STEN = 42.86D+00
        DSTEN = 0.0D+00
        CMF = 0.0D+00
      ELSE
        IF(MASWRK) WRITE(IW,9010) ZSOL
        CALL ABRT
        STOP
      END IF
      IF(MASWRK) WRITE(IW,9020)
     *         ZSOL,EPS,EPSINF,RSOLV,VMOL,TCE,STEN,DSTEN,CMF
C
 9010 FORMAT(/,'*** ERROR: NO DATA TABULATED FOR SOLVENT=',A8//
     * 'ALLOWED SOLVENTS ARE: WATER (OR H2O); METHANOL (OR CH3OH); '/
     * 'ETHANOL (OR C2H5OH); CHLOROFORM (OR CHCL3); ',
     * 'METHYLENE CHLORIDE (OR CH2CL2);'/
     * '1,2-DICHLOROETHANE (OR CH2CLCH2CL); CARBON TETRACHLORIDE ',
     * '(OR CCL4);'/
     * 'BENZENE (OR C6H6); TOLUENE (OR C6H5CH3); CHLOROBENZENE ',
     * '(OR C6H5CL);'/
     * 'NITROMETHANE (OR CH3NO2); N-EPTANE (OR C7H16); CYCLOHEXANE ',
     * '(OR C6H12);'/
     * 'ANILINE (OR C6H5NH2); ACETONE (OR CH3COCH3); TETRAHYDROFURAN ',
     * '(OR THF);'/
     * 'DIMETHYLSULFOXIDE (OR DMSO)'//
     * 'DATA FOR OTHER SOLVENTS CAN BE ADDED TO THE ',
     * 'DATABASE IN ROUTINE "DATSOL"'/
     * 'OR DIRECTLY INPUT IN THE NAMELIST $PCM (SEE INSTRUCTIONS).')
C
 9020 FORMAT(/1X,'** LOOKING UP INTERNALLY STORED DATA FOR SOLVENT=',
     *            A8,' **'/
     *        1X,'OPTICAL AND PHYSICAL CONSTANTS:'/
     *        1X,'EPS= ',F6.3,';',1X,'EPSINF= ',F6.3,';',
     *           ' RSOLV= ',F6.3,' A;',1X,'VMOL= ',F7.3,' ML/MOL;'/
     *        1X,'TCE= ',E10.5,' 1/K;',1X,'STEN= ',F6.3,
     *           ' DYN/CM;',1X,' DSTEN= ',F7.4,';',1X,'CMF= ',F7.4/)
      RETURN
      END
C*MODULE PCM     *DECK PCMFLD
      SUBROUTINE PCMFLD(XH1,XD,XSOL,XQ,XSCR,XDISV,XDIS1,XDIS2,
     *                  XBK,XCOL1,XDM,XCQEF,XQEFF,XELD,XQPOT,XQFLD,
     *                  XVPOT,XSE,XDE,
     *                  EFPUX,EFPUY,EFPUZ,EMPUX,EMPUY,EMPUZ,
     *                  DENTOT,XEFI,YEFI,ZEFI,EFLD,EFADD,ABFLD,
     *                  Q0,Q1,Q2,Q3,D0,QA,DIMAT,QREP,TMP,TMP1,TMP2,
     *                  IPVT,POTTMP,RMUL,LNEAR,LMID,LLONG,
     *                  ECOR,TCH,QET,QETN,QESC,ISD,NFT27,IPCFP,
     *                  L1,L2,NNTS,MXDII1,MAXDII,MXSPX,NPTTPTX,NNREG)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DIRSCF,FDIFF
C
      DIMENSION XH1(L2),XD(L2),XSOL(L2),XQ(NNTS),XSCR(L2),XDISV(*),
     *          XDIS1(L2),XDIS2(L2),XBK(L1,L1),XCOL1(L1),XDM(NNTS,NNTS),
     *          XCQEF(NNTS),XQEFF(NNTS),XELD(NNTS),XQPOT(NNTS),
     *          XQFLD(NNTS),XVPOT(L2),XSE(NNTS,NNTS),XDE(NNTS,NNTS),
     *          EFPUX(NPTTPTX),EFPUY(NPTTPTX),EFPUZ(NPTTPTX),
     *          EMPUX(NPTTPTX),EMPUY(NPTTPTX),EMPUZ(NPTTPTX),
     *          DENTOT(L2),XEFI(L2),YEFI(L2),ZEFI(L2),
     *          EFLD(3,NPTTPTX),EFADD(3,NPTTPTX),ABFLD(3,NPTTPTX),
     *          Q0(NNTS),Q1(NNTS),Q2(NNTS),Q3(NNTS),D0(NNTS),QA(NNTS),
     *          DIMAT(MXDII1,MXDII1),QREP(2,NNTS,MAXDII),
     *          TMP(NNTS,3),TMP1(MXDII1),TMP2(MXDII1,MXDII1),
     *          IPVT(MXDII1),POTTMP(NNTS),RMUL(NNREG,10),
     *          LNEAR(MXSPX,MXSPX),LMID(MXSPX,MXSPX),LLONG(MXSPX,MXSPX)
C
C          the following are used only by the iterative solver:
C        q0,q1,q2,q3,d0,qa,dimat,qrep,tmp,tmp1,tmp2,ipvt,pottmp,rmul,
C        lnear,lmid,llong
C
      PARAMETER (MXTS=2500)
      PARAMETER (MXATM=500)
C
      COMMON /CONV  / DENTOL,EN,ETOT,EHF,EHF0,DIFF,ITER,ICALP,ICBET
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /OPTGRD/ X(3*MXATM),ES,FE(20),
     *                CONVF,FMAXT,DXMAXT,RMAX,RMIN,RLIM,
     *                EIGMAX,EIGMIN,GRDERR,FRMS,FMAX,TRMAX,TRMIN,
     *                IC(20),NSTEP,NSERCH,NPMAX,NP,IFOLOW,
     *                NNEG,IUPHSS,IEXIT,ITRUPD,IPAD
      COMMON /OPTSCF/ DIRSCF,FDIFF
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FEX,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMDIS/ WB,WA,ETA2,GD,EVAC,IDP
      COMMON /PCMF  / IPCDER,IFAST,CHG2(MXTS),CHG2N(MXTS)
      COMMON /PCMOPT/ PEL(MXTS),V_ELE(MXTS),V_NUC(MXTS),V_MON1(MXTS),
     *                V_MON2(MXTS),RABI,RASC,REFPOL,THRSLS,DENSLS,
     *                IDIRCT,IEFPOL,
     *                IMGABI,IMGASC
      COMMON /PCMPAR/ IPCM,N26,N27,IKREP,IEF,IP_F
      COMMON /PCMPRT/ GCAVP,GCAVS,GDISP,GREP,EHFGAS
      COMMON /PCMREP/ RHOW,PM,NEVAL
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      PARAMETER (HALF=0.5D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RHF,RMC/8HRHF     ,8HMCSCF   /
#else
      CHARACTER*8 :: RHF_STR
      EQUIVALENCE (RHF, RHF_STR)
      CHARACTER*8 :: RMC_STR
      EQUIVALENCE (RMC, RMC_STR)
      DATA RHF_STR,RMC_STR/"RHF     ","MCSCF   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA OPT /8HOPTIMIZE/
#else
      CHARACTER*8 :: OPT_STR
      EQUIVALENCE (OPT, OPT_STR)
      DATA OPT_STR/"OPTIMIZE"/
#endif
C
C         --- Induce the PCM surface charges and energy correction ---
C         This routine is programmed only for RHF and MCSCF cases.
C         For RHF, -XH1- is the Fock operator,
C         For MCSCF, -XH1- is the one electron hamiltonian.
C
      IF(IKREP.EQ.1.OR.ICOMP.EQ.3) CALL SFUG(XD,XSCR,CSF,QNUC,L2)
C
      IF(IDP.EQ.0. OR. (IDP.EQ.1.AND.ISD.EQ.1)) THEN
C
C hui li skip PCM for runtyp=opt to save time
C     use the last pcm XSOL
C
      IF(RUNTYP.EQ.OPT .AND. IFAST.EQ.1)THRSLS=0.0D+00
      IF(RUNTYP.EQ.OPT .AND. IFAST.EQ.1.AND.
     *     ITER.GT.1.AND.NSERCH.GT.0)THEN
        IF(DIFF.GT.DENTOL)THEN
          GOTO 100
        ELSE
          IF(DENSLS.EQ.DBLE(NSERCH)*1.0D+04)GOTO 100
          DENSLS=DBLE(NSERCH)*1.0D+04
        END IF
      END IF
C
         IF(ICOMP.LE.2) THEN
            IF(IEF.EQ.0) THEN
              CALL XMATBF(XD,XSOL,XQ,XSCR,XDM,
     *                    QET,QETN,TCH,NNTS,L2,NFT27)
            ELSE
             IF(IEF.LT.3) THEN
              CALL IXMATBF(XD,XSOL,XQ,XSCR,XDM,XSE,XDE,
     *                     XQPOT,XQFLD,XVPOT,QET,QETN,TCH,NNTS,L2,NFT27)
             ELSE
              CALL IXMATV(XD,XSOL,XSCR,XDM,XQPOT,XVPOT,
     *                    EFPUX,EFPUY,EFPUZ,EMPUX,EMPUY,EMPUZ,DENTOT,
     *                    XEFI,YEFI,ZEFI,EFLD,EFADD,ABFLD,
     *                    Q0,Q1,Q2,Q3,D0,QA,DIMAT,QREP,TMP,TMP1,TMP2,
     *                    IPVT,POTTMP,RMUL,LNEAR,LMID,LLONG,
     *                    QET,QETN,TCH,NFT27,IPCFP,
     *                    L1,L2,NNTS,MXDII1,MAXDII,NNREG,MXSPX,NPTTPTX)
             END IF
            END IF
         ELSE
            CALL XMATEF(XD,XSOL,XQ,XSCR,XDM,XCQEF,XQEFF,XELD,
     *                  QET,TCH,QESC,QETN,L2,NNTS,CSF,NFT27)
            CALL DAWRIT(IDAF,IODA,XQEFF,NNTS,332,0)
            CALL DAWRIT(IDAF,IODA,XELD,NNTS,333,0)
         END IF
C
         IF(SCFTYP.EQ.RMC) CALL DSCAL(L2,HALF,XSOL,1)
C
C     ---- add dispersion interaction (if idp=1) ----
C
         IF(IDP.EQ.1) THEN
            CALL DISP(XD,XSCR,XDISV,XDIS1,XDIS2,XBK,XCOL1,L1,L2)
            IF(SCFTYP.EQ.RMC) THEN
               CALL VADD(XH1,1,XDIS1,1,XH1,1,L2)
               CALL DSCAL(L2,HALF,XDIS2,1)
            END IF
            IF(ISD.EQ.1) CALL VADD(XSOL,1,XDIS1,1,XSOL,1,L2)
            CALL VADD(XH1,1,XDIS2,1,XH1,1,L2)
         END IF
C
C       --- at this point add total correction -XSOL- to -XH1- ---
C       for differenced Fock formation, add only the change to -XSOL-
C
C
 100    CONTINUE
C
         IF(SCFTYP.EQ.RHF  .AND.  DIRSCF  .AND.  FDIFF) THEN
            IF(ISD.EQ.1  .AND.  ITER.EQ.1) THEN
               CALL VCLR(XSCR,1,L2)
            ELSE
               CALL DAREAD(IDAF,IODA,XSCR,L2,87,0)
            END IF
            CALL DAWRIT(IDAF,IODA,XSOL,L2,87,0)
            CALL VSUB(XSCR,1,XSOL,1,XSOL,1,L2)
         END IF
C
         CALL VADD(XH1,1,XSOL,1,XH1,1,L2)
      END IF
C
C     ----- CALCULATE THE ENERGY -----
C
      ECOR = HALF*PC
C
C     ----- STORE THE pcm model REPULSION FREE ENERGY -----
C
      IF(IKREP.EQ.1) GREP=CSF*0.063D+00*RHOW*NEVAL/PM
C
C     --- FOR MCSCF, H1+PERTURBATION MUST BE WRITTEN TO DAF ---
C
      IF(SCFTYP.EQ.RMC) CALL DAWRIT(IDAF,IODA,XH1,L2,11,0)
C
      RETURN
      END
C*MODULE PCM     *DECK IEFDAT
      SUBROUTINE IEFDAT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      COMMON /ANIDAT/ EPSM,EPSXX,EPSXY,EPSXZ,EPSYY,EPSYZ,EPSZZ,
     *                EPSM1XX,EPSM1XY,EPSM1XZ,EPSM1YY,EPSM1YZ,EPSM1ZZ,
     *                EPS1,EPS2,EPS3,ROT(3,3)
      COMMON /CAVANIS/ DLMOL,ANGSS,ALP,DLAM
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /IONDAT/ EPSI,DK2,DALP
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMPAR/ IPCM,NFT26,NFT27,IKREP,IEF,IP_F
C
      PARAMETER (NNAM=12)
      DIMENSION QNAM(NNAM),KQNAM(NNAM)
C
      DATA FPI/12.56637061D+00/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA ANIWD/8HIEFPCM  /
#else
      CHARACTER*8 :: ANIWD_STR
      EQUIVALENCE (ANIWD, ANIWD_STR)
      DATA ANIWD_STR/"IEFPCM  "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QNAM/8HEPSI    ,8HDISM     ,
     *          8HEPS1    ,8HEPS2    ,8HEPS3    ,
     *          8HEUPHI   ,8HEUTHE   ,8HEUPSI   ,
     *          8HDLMOL   ,8HANGSS   ,8HALP     ,8HDLAM    /
#else
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"EPSI    ","DISM    " ,
     *          "EPS1    ","EPS2    ","EPS3    ",
     *          "EUPHI   ","EUTHE   ","EUPSI   ",
     *          "DLMOL   ","ANGSS   ","ALP     ","DLAM    "/
#endif
      DATA KQNAM/3,3,3,3,3,3,3,3,3,3,3,3/
C
      EPSI=EPS
      DISM=0.0D+00
      EPS1=EPS
      EPS2=EPS
      EPS3=EPS
C   EULERIAN ANGLES IN DEGREE
      EUPHI=0.0D+00
      EUTHE=0.0D+00
      EUPSI=0.0D+00
C
      JRET=0
      CALL NAMEIO(IR,JRET,ANIWD,NNAM,QNAM,KQNAM,
     *            EPSI,DISM,EPS1,EPS2,EPS3,EUPHI,EUTHE,EUPSI,
     *            DLMOL,ANGSS,ALP,DLAM,
     *            0,0,
     *    0,0,0,0,0,  0,0,0,0,0,
     *    0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,
     *    0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0)
C
      IF(JRET.EQ.2) THEN
         IF(MASWRK) WRITE(IW,930) ANIWD
         CALL ABRT
      END IF
C
      IF(IEF.EQ.1) THEN
C
C-----------------------
C ANISOTROPIC DIELECTRIC
C-----------------------
C Required data:
C
C EPS1, EPS2, EPS3 = diagonal values of the dielectric permittivity
C                    tensor with respect of the laboratory frame
C                    (ie. the frame respect to which the solute position
C                     is given).
C EUPHI EUTHE EUPSI= eulerian angles which say the rotation of the
C                    solvent orientation with respect to the laboratory
C                    frame.
C
C read three diel const. values and euler angles in degrees
C
      CSPHI=COS(EUPHI)
      SNPHI=SIN(EUPHI)
      CSTHE=COS(EUTHE)
      SNTHE=SIN(EUTHE)
      CSPSI=COS(EUPSI)
      SNPSI=SIN(EUPSI)
C
      ROT(1,1)=CSPHI*CSPSI-SNPHI*CSTHE*SNPSI
      ROT(1,2)=-SNPHI*CSPSI-CSPHI*CSTHE*SNPSI
      ROT(1,3)=SNTHE*SNPSI
      ROT(2,1)=CSPHI*SNPSI+SNPHI*CSTHE*CSPSI
      ROT(2,2)=-SNPHI*SNPSI+CSPHI*CSTHE*CSPSI
      ROT(2,3)=-SNTHE*CSPSI
      ROT(3,1)=SNPHI*SNTHE
      ROT(3,2)=CSPHI*SNTHE
      ROT(3,3)=CSTHE
C
      EPSM=(EPS1*EPS2*EPS3)**(1.0D+00/3.0D+00)
C
      EPSXX=(EPS1*ROT(1,1)*ROT(1,1)+EPS2*ROT(2,1)*ROT(2,1)+
     *      EPS3*ROT(3,1)*ROT(3,1))
      EPSXY=(EPS1*ROT(1,1)*ROT(1,2)+EPS2*ROT(2,1)*ROT(2,2)+
     *      EPS3*ROT(3,1)*ROT(3,2))
      EPSXZ=(EPS1*ROT(1,1)*ROT(1,3)+EPS2*ROT(2,1)*ROT(2,3)+
     *      EPS3*ROT(3,1)*ROT(3,3))
      EPSYY=(EPS1*ROT(1,2)*ROT(1,2)+EPS2*ROT(2,2)*ROT(2,2)+
     *      EPS3*ROT(3,2)*ROT(3,2))
      EPSYZ=(EPS1*ROT(1,2)*ROT(1,3)+EPS2*ROT(2,2)*ROT(2,3)+
     *      EPS3*ROT(3,2)*ROT(3,3))
      EPSZZ=(EPS1*ROT(1,3)*ROT(1,3)+EPS2*ROT(2,3)*ROT(2,3)+
     *      EPS3*ROT(3,3)*ROT(3,3))
C
      EPSM1XX=(EPS1**(-1)*ROT(1,1)*ROT(1,1)+EPS2**(-1)*ROT(2,1)
     *      *ROT(2,1)+EPS3**(-1)*ROT(3,1)*ROT(3,1))
      EPSM1XY=(EPS1**(-1)*ROT(1,1)*ROT(1,2)+EPS2**(-1)*ROT(2,1)
     *      *ROT(2,2)+EPS3**(-1)*ROT(3,1)*ROT(3,2))
      EPSM1XZ=(EPS1**(-1)*ROT(1,1)*ROT(1,3)+EPS2**(-1)*ROT(2,1)
     *      *ROT(2,3)+EPS3**(-1)*ROT(3,1)*ROT(3,3))
      EPSM1YY=(EPS1**(-1)*ROT(1,2)*ROT(1,2)+EPS2**(-1)*ROT(2,2)
     *      *ROT(2,2)+EPS3**(-1)*ROT(3,2)*ROT(3,2))
      EPSM1YZ=(EPS1**(-1)*ROT(1,2)*ROT(1,3)+EPS2**(-1)*ROT(2,2)
     *      *ROT(2,3)+EPS3**(-1)*ROT(3,2)*ROT(3,3))
      EPSM1ZZ=(EPS1**(-1)*ROT(1,3)*ROT(1,3)+EPS2**(-1)*ROT(2,3)
     *      *ROT(2,3)+EPS3**(-1)*ROT(3,3)*ROT(3,3))
C
       EPS=EPSM
       IF(MASWRK) WRITE(IW,950)
       IF(MASWRK) WRITE(IW,*)'     EPS1,  EPS2,  EPS3,  EPSM'
       IF(MASWRK) WRITE(IW,55)EPS1,EPS2,EPS3,EPSM
      ELSE IF(IEF.EQ.2) THEN
C
C----------------
C IONIC SOLUTIONS
C----------------
C
C Required data:
C
C EPSI= dielectric constant (is equal to the permittivity
C                            of the chosen solvent, EPS)
C In atomic units:
C DK2= 29.8633 I/(4*PI*EPSI)
C      where I=ionic strength in mol/dm^3 (M)
C      dimensional units: [DK2] = (bohr)^(-2)
C      The square root of DK2 (DALP) is the inverse of the
C      physical quantity known as Debye length (LD),
C      DALP is here expressed in bohr^-1.
C
C read the ionic strength in mol/dm^3 (M): DISM
C
       EPS=EPSI
       DK2=29.8633D+00*DISM/(FPI*EPS)
       DALP=SQRT(DK2)
       IF(MASWRK) WRITE(IW,951)
       IF(MASWRK) WRITE(IW,*)'     EPSI, IONIC-STRENGTH (M)'
       IF(MASWRK) WRITE(IW,56) EPSI,DISM
      ELSE IF(IEF.EQ.5.OR.IEF.EQ.8) THEN
C
C----------------------
C NONEQULIBRIUM SOLVENT
C----------------------
C
C Required data:
C EPSINF= dielectric constant at infinite frequency
C         it determines the fast contribution (due to electronic
C         motions) to the polarization of the solvent
C
       EPSM=1.0D+00
       EPS=EPSINF
       IF(MASWRK) WRITE(IW,953)
       IF(MASWRK) WRITE(IW,*)'     EPS'
       IF(MASWRK) WRITE(IW,57)EPS
      ELSE
       EPSM=1.0D+00
       IF(MASWRK) WRITE(IW,952)
       IF(MASWRK) WRITE(IW,*)'     EPS'
       IF(MASWRK) WRITE(IW,57)EPS
      END IF
C
  930 FORMAT(1X,'**** ERROR IN $',A8,' INPUT')
  950 FORMAT(/5X,'INPUT FOR ANISOTROPIC DIELECTRICS '/5X,30("-"))
  951 FORMAT(/5X,'INPUT FOR IONIC SOLUTIONS '/5X,22("-"))
  952 FORMAT(/5X,'INPUT FOR ISOTROPIC DIELECTRICS (IEF)'/5X,32("-"))
  953 FORMAT(/5X,'INPUT FOR SOLVENTS AT NONEQUILIBRIUM'/5X,32("-"))
  55  FORMAT(3X,4(2X,F8.4))
  56  FORMAT(3X,2(2X,F8.4))
  57  FORMAT(3X,3(2X,F8.4))
      RETURN
      END
