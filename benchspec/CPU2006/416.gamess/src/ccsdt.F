C  9 dec 03 - mws - synch common block runopt
C  3 Sep 03 - MWS - move I/O routines to CCAUX source file
C 16 Jun 03 - KK  - save info for possible EOM to follow
C 13 Jan 03 - KK  - eliminate disk file -CCT3AMP- in most cases
C 22 May 02 - PP,KK - new module for CC, R-CC, CR-CC computations
C
C*MODULE CCSDT   *DECK CCINP
      SUBROUTINE CCINP
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      DOUBLE PRECISION LCCD
      LOGICAL CCDISK,GOPARR,DSKWRK,MASWRK
C
      COMMON /CCPAR / AMPTSH,METHCC,NCCTOT,NCCOCC,NCCFZC,NCCFZV,
     *                MXCCIT,MXRLEIT,NWRDCC,ICCCNV,ICCRST,IDSKCC
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      PARAMETER (NNAM=9)
      DIMENSION QNAM(NNAM),KQNAM(NNAM)
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA GROUP/8HCCINP   /
#else
      CHARACTER*8 :: GROUP_STR
      EQUIVALENCE (GROUP, GROUP_STR)
      DATA GROUP_STR/"CCINP   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QNAM/8HNCORE   ,8HNFZV    ,8HMAXCC   ,8HNWORD   ,
     *          8HICONV   ,8HIREST   ,8HAMPTSH  ,8HMXDIIS  ,
     *          8HCCDISK  /
#else
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"NCORE   ","NFZV    ","MAXCC   ","NWORD   ",
     *          "ICONV   ","IREST   ","AMPTSH  ","MXDIIS  ",
     *          "CCDISK  "/
#endif
      DATA KQNAM/1,1,1,1,  1,1,3,1,  0/
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA LCCD,CCD,CCSD,CCSDT,RCC,CRCC,EOMSD,EOMSDT
     *     /8HLCCD    ,8HCCD     ,8HCCSD    ,8HCCSD(T) ,
     *      8HR-CC    ,8HCR-CC   ,8HEOM-CCSD,8HCR-EOM  /
#else
      CHARACTER*8 :: LCCD_STR, CCD_STR, CCSD_STR, CCSDT_STR, 
     * RCC_STR, CRCC_STR, EOMSD_STR, EOMSDT_STR
      EQUIVALENCE (LCCD, LCCD_STR), (CCD, CCD_STR), (CCSD, CCSD_STR),
     * (CCSDT, CCSDT_STR), (RCC, RCC_STR), (CRCC, CRCC_STR), 
     * (EOMSD, EOMSD_STR), (EOMSDT, EOMSDT_STR)
      DATA LCCD_STR,CCD_STR,CCSD_STR,CCSDT_STR,RCC_STR,CRCC_STR,
     * EOMSD_STR,EOMSDT_STR
     *     /"LCCD    ","CCD     ","CCSD    ","CCSD(T) ",
     *      "R-CC    ","CR-CC   ","EOM-CCSD","CR-EOM  "/
#endif

C
C     ---- set coupled cluster calculation parameters -----
C
      METHCC=-1
      IF(CCTYP.EQ.LCCD)   METHCC = 0
      IF(CCTYP.EQ.CCD)    METHCC = 1
      IF(CCTYP.EQ.CCSD)   METHCC = 2
      IF(CCTYP.EQ.CCSDT)  METHCC = 3
      IF(CCTYP.EQ.RCC)    METHCC = 4
      IF(CCTYP.EQ.CRCC)   METHCC = 5
      IF(CCTYP.EQ.EOMSD)  METHCC = 2
      IF(CCTYP.EQ.EOMSDT) METHCC = 2
      IF(METHCC.EQ.-1) THEN
         IF(MASWRK) WRITE(IW,9005) CCTYP
         CALL ABRT
      END IF
C
C        obtain total number of MOs and how many are occupied
C        note that it is too early in the run to know -nqmt-, and
C        anyway this is possibly different at different geometries,
C        so we must look up -ncctot- again later.
C
      NCCTOT = NQMT
      NCCOCC = NA
C
C        the rest is input variables...
C
      NCCFZC = NUMCOR()
      NCCFZV = 0
      MXCCIT = 30
      MXRLEIT= -1
      NWRDCC = 0
      ICCCNV = 7
      ICCRST = 0
      AMPTSH = 0.0D+00
      CCDISK = .FALSE.
C
      CALL NAMEIO(IR,JRET,GROUP,NNAM,QNAM,KQNAM,
     *            NCCFZC,NCCFZV,MXCCIT,NWRDCC,ICCCNV,ICCRST,AMPTSH,
     *            MXRLEIT,CCDISK,  0,0,0,0,0,
     *    0,0,0,0,0,  0,0,0,0,0,
     *    0,0,0,0,0,  0,0,0,0,0,   0,0,0,0,0,  0,0,0,0,0,
     *    0,0,0,0,0,  0,0,0,0,0,   0,0,0,0,0,  0,0,0,0,0)
C
      IF(JRET.EQ.2) THEN
        WRITE(IW,*) 'BAD $CCINP READ'
        CALL ABRT
      END IF
      NO = NCCOCC-NCCFZC
      NU = NCCTOT-NCCFZV-NCCOCC
C
C        small problems shouldn't allow too big rle expansion space
C
      IF(MXRLEIT.LT.0) THEN
                          MXRLEIT=5
         IF(NO*NU .LT. 6) MXRLEIT=3
         IF(NO*NU .LT. 3) MXRLEIT=0
      END IF
C
      IF(ICCRST.GT.0  .AND.  ICCRST.LT.3) THEN
         WRITE(IW,*) 'IREST BEING FORCED TO 3...'
         ICCRST=3
      END IF
C
      IDSKCC=1
      IF(CCDISK) IDSKCC=0
C
C        the cc program's memory allocations, etc. assume nu>no,
C        so kill any run with too few virtuals, and test to
C        be sure any freezing has left us with some electrons.
C
      IF(NO*NU.EQ.0  .OR.  NU.LT.NO) THEN
         WRITE(IW,9020) NO,NU
         CALL ABRT
      END IF
C
      RETURN
 9005 FORMAT(1X,'CCINP: UNRECOGNIZED CCTYP=',A8)
 9020 FORMAT(/5X,'*** ERROR IN CC CALCULATION ***'/
     *        1X,'THE NUMBER OF CORRELATED OCCUPIED ORBITALS=',I6/
     *        1X,'THE NUMBER OF EMPTY, CORRELATING ORBITALS =',I6/
     *        1X,'NEITHER OF THESE SHOULD BE ZERO, AND YOU MUST RUN'/
     *        1X,'WITH MORE EMPTY THAN OCCUPIED ORBITALS.')
      END
C
C         this was originally cc.f
C
C*MODULE CCSDT   *DECK CCDRVR
      SUBROUTINE CCDRVR(BESTCC,EOM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL EOM
      LOGICAL CNVR
C
      PARAMETER (MXRT=100)
C
      COMMON /CCENGY/ ENRG,EREF,EMP2,ETOT(6),ECORR(6),
     *                DIAGS(3),AMPMX(5,2),IAMPMX(5,4,2),XO1,XO2,
     *                DIFMAX,DIFFENG,ITER,CNVR
      COMMON /CCINFO/ TSH,NH,NP,MET,MEM,ICONV,MAXIT,IREST,IDISC
      COMMON /CCPAR / AMPTSH,METHCC,NCCTOT,NCCOCC,NCCFZC,NCCFZV,
     *                MXCCIT,MXRLEIT,NWRDCC,ICCCNV,ICCRST,IDSKCC
      COMMON /CCRLE / MXRLE,NRLE0,NRLE,IRLE,ITRLE
      COMMON /FMCOM / X(1)
      COMMON /ENRGYS/ VNN,EELCT,ETOTX,SZ,SZZ,ECORE,ESCF,EERD,E1,E2,
     *                VEN,VEE,EPOT,EKIN,ESTATE(MXRT),STATN
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
C
C             ---- main driver for CC calculations ----
C            Piotr Piecuch (a), Stanislaw A. Kucharski (b),
C              Karol Kowalski (a) and Monika Musial (b)
C       (a) Department of Chemistry, Michigan State University
C           (b) Institute of Chemistry, University of Silesia
C
C     Interfaced to GAMESS in March 2002.  For information on the
C     unique features of this code, the renormalized and completely
C     renormalized coupled-cluster theory, see the following:
C     K.Kowalski, P.Piecuch  J.Chem.Phys. 113, 18-35(2000)
C     K.Kowalski, P.Piecuch  J.Chem.Phys. 113, 5644-5652(2000)
C
C     The biggest (quartic) memory allocations are as follows
C     integral preparation
C        SRTING:  somewhere between NONU3 and NU4
C     CCSD iterations
C          CCSD:   NO4   NO3NU  4NO2U2   NONU3
C     non-iterative triples (only 1 of INTRIPL/INTRIP will be run)
C         T3WT2:         NO3NU  2NO2U2   NONU3
C        INTQUA:         NO3NU   NO2U2   NONU3
C       INTRIPL:         NO3NU   NO2U2  2NONU3
C          or
C        INTRIP:        2NO3NU   NO2U2   NONU3
C        INTRIH:        3NO3NU   NO2U2   NONU3
C        T3WT2N:         NO3NU  2NO2U2   NONU3
C       T3SQTOT:                2NO2U2
C     Since T3WT2 has two NU3 arrays, it may be the memory bottleneck
C     rather than CCSD, depending on how big NU is relative to NO2.
C
      WRITE(6,9010)
C
C        etotx is the SCF energy, including nuclear repulsion
C
      EREF = ETOTX
      ENRG = ZERO
      EMP2 = ZERO
      DO I=1,6
        ECORR(I)= ZERO
        ETOT(I) = ZERO
      ENDDO
      DIAGS(1) = ZERO
      DIAGS(2) = ZERO
      DIAGS(3) = ZERO
      DO J=1,2
         DO I=1,5
            AMPMX(I,J) = ZERO
            IAMPMX(I,1,J) = 0
            IAMPMX(I,2,J) = 0
            IAMPMX(I,3,J) = 0
            IAMPMX(I,4,J) = 0
         ENDDO
      ENDDO
      XO1 = ONE
      XO2 = ONE
C
C        copy input parameters
C
      NH    = NCCOCC-NCCFZC
      NP    = NCCTOT-NCCFZV-NCCOCC
      MET   = METHCC
      ICONV = ICCCNV
      MAXIT = MXCCIT
      MXRLE = MXRLEIT
      IREST = ICCRST
      MEM   = NWRDCC
      IF(MEM.EQ.0) CALL GOTFM(MEM)
      TSH   = AMPTSH
      IFC   = NCCFZC
      IDISC = IDSKCC
C
      ITER=IREST
C
      CALL DRPRINT(1)
C
C        the cc program's memory allocations, etc. assume np>nh,
C        so kill any run with too few virtuals
C
      IF(NP.LT.NH) THEN
         WRITE(6,9020) NH,NP
         CALL ABRT
      END IF
C
C        orbital energies are held throughout the entire run
C
      CALL VALFM(LOADFM)
      LEH  = LOADFM + 1
      LEP  = LEH    + NH
      LAST = LEP    + NP
      NEED = LAST - LOADFM - 1
      CALL GETFM(NEED)
C
C        prepare integrals and Fock matrix elements for the run
C
      CALL DRSRTING(NH,NP,X(LEH),X(LEP),IFC,EOM)
      WRITE(6,9060)
      CALL TIMIT(1)
C
C        carry out the coupled-cluster amplitude iterations
C
      CALL DRCCSD(NH,NP,X(LEH),X(LEP),IFC,EOM)
      CALL DRPRINT(3)
      WRITE(6,9070)
      CALL TIMIT(1)
      IF(.NOT.CNVR) THEN
         WRITE(6,*) ' CCSD DID NOT CONVERGED'
         CALL ABRT
      END IF
C
C        compute non-iterative triples corrections
C
      IF(MET.GE.3) THEN
         IF(MET.EQ.3.OR.MET.EQ.4.OR.IDISC.EQ.0) THEN
         CALL DRT3WT2(NH,NP,X(LEH),X(LEP))
         END IF
         IF(MET.GE.5) THEN
            CALL DRINTRI(NH,NP,0)
            CALL DRT3WT2N(NH,NP,X(LEH),X(LEP),IDISC)
         END IF
         CALL DRSUMA(NH,NP,X(LEH),X(LEP))
         WRITE(6,9080)
         CALL TIMIT(1)
      END IF
      CALL DRPRINT(4)
      BESTCC = ENRG + EREF
C
      CALL RETFM(NEED)
      RETURN
 9010 FORMAT(/3X,23("-"),5X,46("-")/
     *        3X,'COUPLED-CLUSTER PROGRAM',5X,
     *           'P.PIECUCH, S.A.KUCHARSKI, K.KOWALSKI, M.MUSIAL'/
     *        3X,23(1H-),5X,46(1H-))
 9020 FORMAT(/5X,'*** ERROR READING CC INPUT ***'/
     *        1X,'THE NUMBER OF OCCUPIED CORRELATED ORBITALS=',I6/
     *        1X,'THE NUMBER OF EMPTY, CORRELATING ORBITALS =',I6/
     *        1X,'PLEASE RUN WITH MORE EMPTY THAN FILLED ORBITALS.')
 9060 FORMAT(1X,'....... DONE WITH CC INTEGRAL PREPARATION .......')
 9070 FORMAT(1X,'....... DONE WITH CC AMPLITUDE ITERATIONS .......')
 9080 FORMAT(1X,'..... DONE WITH CC NON-ITERATIVE TRIPLES CORRECTIONS',
     *          ' .....')
      END
C
C*MODULE CCSDT   *DECK DRPRINT
      SUBROUTINE DRPRINT(I)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*10 CMET(9)
      CHARACTER*4 CMES(3)
      LOGICAL CNVR
      COMMON /CCENGY/ ENRG,EREF,EMP2,ETOT(6),ECORR(6),
     *                DIAGS(3),AMPMX(5,2),IAMPMX(5,4,2),XO1,XO2,
     *                DIFMAX,DIFFENG,ITER,CNVR
      COMMON /CCINFO/ TSH,NH,NP,MET,MEM,ICONV,MAXIT,IREST,IDISC
C
      DATA CMET/'      LCCD','       CCD','    CCSD  ','   CCSD[T]',
     &'   CCSD(T)',' R-CCSD[T]',' R-CCSD(T)','CR-CCSD[T]','CR-CCSD(T)'/
      DATA CMES/'LCCD',' CCD','CCSD'/
C
      IMET=MET+1
      IF(MET.GT.2)IMET=3
      GOTO (1,2,3,4)I
 1    CONTINUE
      WRITE(6,91)
      IF(MET.LE.2)WRITE(6,92)CMET(MET+1)
      IF(MET.GE.3)WRITE(6,92)CMET(3),CMET(4),CMET(5)
      IF(MET.GE.4)WRITE(6,92)CMET(6),CMET(7)
      IF(MET.GE.5)WRITE(6,92)CMET(8),CMET(9)
      IGR=MET+1
      IF(MET.GE.3)IGR=2*MET-1
      WRITE(6,93)CMET(IGR)
      WRITE(6,94)MEM
      CONV=10.0D+00**(-ICONV)
      WRITE(6,95)CONV
      WRITE(6,96)MAXIT
      IF(IREST.GT.0) WRITE(6,97) IREST
      RETURN
 91   FORMAT(/1X,'THE FOLLOWING CALCULATIONS WILL BE PERFORMED:')
 92   FORMAT(50X,A10)
 93   FORMAT(/1X,'THE FOLLOWING ENERGY WILL BE CONSIDERED',
     *           ' THE HIGHEST LEVEL: ',A10)
 94   FORMAT(1X,'THE AVAILABLE MEMORY IS',I12,' WORDS.')
 95   FORMAT(1X,'CONVERGENCE THRESHOLD:',1P,E10.1)
 96   FORMAT(1X,'MAXIMUM NUMBER OF ITERATIONS:',I5)
 97   FORMAT(1X,'CALCULATIONS RESTARTED FROM ITERATION:',I5)
C
 2    CONTINUE
      IMET=MET+1
      IF(MET.GT.2)IMET=3
      WRITE(6,191) ITER,CMET(IMET),ENRG,DIFMAX
      CALL FLSHBF(6)
191   FORMAT(1X,'ITER:',I3,A10,' CORR. ENERGY:',F15.10,'   CONV.:',
     *           1P,E12.4)
      RETURN
C
C        printing after ccsd iterations
C
 3    CONTINUE
      IF(CNVR) THEN
         WRITE(6,291) CMET(IMET)
      ELSE
         WRITE(6,1291) CMET(IMET)
      END IF
      WRITE(6,293) EMP2
      WRITE(6,294) CMET(IMET),ENRG
      IF(MET.GE.2) THEN
         WRITE(6,1300) (DIAGS(III),III=1,3)
         WRITE(6,1302)
         DO K=1,5
C jray: For SPEC, only the absolute values are printed for 
C       T1 and T2 amplitudes
            IF(ABS(AMPMX(K,1)).GT.1.0D-06)
#if defined(SPEC_CPU)
     *         WRITE(6,1305) ABS(AMPMX(K,1)),(IAMPMX(K,III,1),III=1,2)
#else
     *         WRITE(6,1305) AMPMX(K,1),(IAMPMX(K,III,1),III=1,2)
#endif
         ENDDO
         WRITE(6,1308)
         DO K=1,5
            IF(ABS(AMPMX(K,2)).GT.1.0D-06)
C jray: For SPEC, only the absolute values are printed for 
C       T1 and T2 amplitudes
#if defined(SPEC_CPU)
     *         WRITE(6,1310) ABS(AMPMX(K,2)),(IAMPMX(K,III,2),III=1,4)
#else
     *         WRITE(6,1310) AMPMX(K,2),(IAMPMX(K,III,2),III=1,4)
#endif
         ENDDO
         WRITE(6,1312)
      END IF
      RETURN
C
C        printing of final results
C
 4    CONTINUE
      IF(MET.GT.2) THEN
      WRITE(6,1294) ECORR(1)
      WRITE(6,2294) ECORR(2)
      IF(MET.GT.3) THEN
      WRITE(6,3294) ECORR(3)
      WRITE(6,4294) ECORR(4)
      IF(MET.EQ.4) THEN
      WRITE(6,302) XO1
      WRITE(6,303) XO2
      END IF
      IF(MET.GT.4) THEN
      WRITE(6,5294) ECORR(5)
      WRITE(6,6294) ECORR(6)
      WRITE(6,302) XO1
      WRITE(6,303) XO2
      END IF
      END IF
      END IF
C
      WRITE(6,295)
      WRITE(6,296)EREF
      WRITE(6,297)(EREF+EMP2)
      WRITE(6,298)CMET(IMET),(ENRG+EREF)
C
C ---- interface to MM23 program ------
C
C     nft566 = 64
C     open(nft566,file='ECCSD',status='unknown')
C     write(nft566,*) ENRG+EREF
C     write(nft566,*) ENRG
C     close(nft566, status='keep')
C
C ---- end of interface to MM23 program ------
C
      IF(MET.GT.2) THEN
      WRITE(6,1298) ETOT(1)
      WRITE(6,2298) ETOT(2)
      IF(MET.GT.3) THEN
      WRITE(6,3298) ETOT(3)
      WRITE(6,4298) ETOT(4)
      IF(MET.GT.4) THEN
      WRITE(6,5298) ETOT(5)
      WRITE(6,6298) ETOT(6)
      END IF
      END IF
      END IF
      IMET=MET+1
      IF(MET.GT.2)IMET=MET+2
      WRITE(6,300)
      IF(MET.LE.2) THEN
         WRITE(6,301) CMES(IMET),ENRG+EREF
      ELSE
         IMET=2*MET-4
         ENRG = ECORR(IMET)
         WRITE(6,1301) CMET(IMET+3),ENRG+EREF
      END IF
      RETURN
C
1291  FORMAT(/15X,A10,'  ITERATIONS DID NOT CONVERGE'/)
 291  FORMAT(/5X,'THE ',A10,'  ITERATIONS HAVE CONVERGED')
 293  FORMAT(/1X,'   MBPT(2) CORRELATION ENERGY:',F15.10)
 294  FORMAT( 1X,A10,      ' CORRELATION ENERGY:',F15.10)
 1294 FORMAT(/1X,'   CCSD[T] CORRELATION ENERGY:',F15.10)
 2294 FORMAT( 1X,'   CCSD(T) CORRELATION ENERGY:',F15.10)
 3294 FORMAT( 1X,' R-CCSD[T] CORRELATION ENERGY:',F15.10)
 4294 FORMAT( 1X,' R-CCSD(T) CORRELATION ENERGY:',F15.10)
 5294 FORMAT( 1X,'CR-CCSD[T] CORRELATION ENERGY:',F15.10)
 6294 FORMAT( 1X,'CR-CCSD(T) CORRELATION ENERGY:',F15.10)
 295  FORMAT(/22X,'SUMMARY OF RESULTS')
 296  FORMAT(/11X,' REFERENCE ENERGY:',F20.10)
 297  FORMAT( 11X,'   MBPT(2) ENERGY:',F20.10)
 298  FORMAT( 11X,A10,      ' ENERGY:',F20.10)
 1298 FORMAT( 11X,'   CCSD[T] ENERGY:',F20.10)
 2298 FORMAT( 11X,'   CCSD(T) ENERGY:',F20.10)
 3298 FORMAT( 11X,' R-CCSD[T] ENERGY:',F20.10)
 4298 FORMAT( 11X,' R-CCSD(T) ENERGY:',F20.10)
 5298 FORMAT( 11X,'CR-CCSD[T] ENERGY:',F20.10)
 6298 FORMAT( 11X,'CR-CCSD(T) ENERGY:',F20.10)
  300 FORMAT(/1X,'THE FOLLOWING METHOD AND ENERGY WILL BE CONSIDERED',
     *           ' THE HIGHEST LEVEL RESULT:')
 301  FORMAT(1X,'COUPLED-CLUSTER ENERGY E(',A4,') =',F20.10/)
1301  FORMAT(1X,'COUPLED-CLUSTER ENERGY E(',A10,') =',F20.10/)
 302  FORMAT(/1X,'R-CCSD[T] DENOMINATOR',F17.12)
 303  FORMAT( 1X,'R-CCSD(T) DENOMINATOR',F17.12)
 1300 FORMAT(/1X,'T1 DIAGNOSTIC        =',F15.8/
     *        1X,'NORM OF THE T1 VECTOR=',F15.8/
     *        1X,'NORM OF THE T2 VECTOR=',F15.8)
 1302 FORMAT(/1X,'THE FIVE LARGEST T1 AMPLITUDES ARE:')
 1305 FORMAT(1X,'T1 AMPLITUDE IS',F10.6,' FOR I=',I4,
     *          ' -> A=',I4)
 1308 FORMAT(/1X,'THE FIVE LARGEST SPIN-UNIQUE T2 AMPLITUDES ARE:')
 1310 FORMAT(1X,'T2 AMPLITUDE IS',F10.6,' FOR I,J=',2I4,
     *          ' -> A,B=',2I4)
 1312 FORMAT(1X,'PRINTED T2(I-ALPHA,J-BETA -> A-ALPHA,B-BETA) VALUES'/
     *       1X,'EQUAL   T2(J-ALPHA,I-BETA -> B-ALPHA,A-BETA)',
     *          ' AMPLITUDES.'/)
      END
C
C            this was originally srting.f
C
C*MODULE CCSDT   *DECK DRSRTING
      SUBROUTINE DRSRTING(NO,NU,EH,EP,IFC,EOM)
      IMPLICIT DOUBLE PRECISION  (A-H,O-Z)
      LOGICAL EOM
      LOGICAL PACK2E
      PARAMETER (MXATM=500)
      DIMENSION EH(NO),EP(NU)
C
      COMMON /CCFILE/ INTG,NT1,NT2,NT3,NVM,NVE,NFRLE,NRESF,NRESL
      COMMON /CCINFO/ TSH,NH,NP,MET,MEM,ICONV,MAXIT,IREST,IDISC
      COMMON /CCPAR / AMPTSH,METHCC,NCCTOT,NCCOCC,NCCFZC,NCCFZV,
     *                MXCCIT,MXRLEIT,NWRDCC,ICCCNV,ICCRST,IDSKCC
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
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
C     memory allocation for transformed integral manipulations
C         h=occupied, h means "holes" which are occupied
C                       states in the Fermi vacuum
C         p= virtual, p means "particles" which are unoccupied
C                       states in the Fermi vacuum
C
      NO2   = NO*NO
      NO3   = NO*NO2
      NO4   = NO*NO3
      NU2   = NU*NU
      NU3   = NU*NU2
      NOU   = NO*NU
      NO3U  = NO3*NU
      NOU2  = NO*NU2
      NOU3  = NO*NU3
      NO2U2 = NO2*NU2
C
C     open all disk files associated with CC calculations
C
      NRESF=70
      NFRLE=71
      INTG=72
      NT1=73
      NT2=74
      NT3=75
      NVM=76
      NVE=77
C
C        file -nresf- has records consisting of one floating point
C        and one integer array, both of the same length.  The number
C        below is the number of bytes in one f.p. and one integer.
C        be careful to ensure that lword*nresl is a multiple of 8.
C
      LWORD = 8 + 8/NWDVAR
      NRESL = 8096
C
C        these are the direct access file lengths
C
      LRESF = (LWORD*NRESL)/8
      LRLE  = (NOU+NO2U2)
      LT3   = NU*NU*NU
      LNOU  = NOU
      LNO2U2= NO2U2
      LNU3  = NU3
      LVM   = NO3U
      LVE   = NU3
C
C        all of these files are random access type files
C
      CALL CCOPEN(NRESF,LRESF ,'CCREST')
      CALL CCOPEN(NFRLE,LRLE  ,'CCDIIS')
      CALL CCOPEN(INTG ,LT3   ,'CCINTS')
      CALL CCOPEN(NT1  ,LNOU  ,'CCT1AMP')
      CALL CCOPEN(NT2  ,LNO2U2,'CCT2AMP')
      CALL CCOPEN(NT3  ,LNU3  ,'CCT3AMP')
      CALL CCOPEN(NVM  ,LVM   ,'CCVM')
      CALL CCOPEN(NVE  ,LVE   ,'CCVE')
      IF(EOM) THEN
         NHHHH = 85
         LHHHH = MAX(NU2,NO4)
         CALL CCOPEN(NHHHH,LHHHH,'EOMHHHH')
      END IF
C
      CALL GOTFM(NGOTMX)
      IF(MEM.LT.NGOTMX) NGOTMX=MEM
C
      L1 = NUM
      L2 = (L1*L1+L1)/2
      L3 = L1*L1
      M1 = NCCTOT - NCCFZC - NCCFZV
      M2 = (M1*M1+M1)/2
C
C         one electron contributions to the energy are made
C         using Fock matrix elements, in the MO basis.
C
      CALL VALFM(LOADFM)
      LFAO  = LOADFM+1
      LFMO  = LFAO  + L2
      LVEC  = LFMO  + M2
      LFHH  = LVEC  + L3
      LFPP  = LFHH  + NO*NO
      LFHP  = LFPP  + NU*NU
      LWRK  = LFHP  + NO*NU
      LAST  = LWRK  + L1
      NEED = LAST - LOADFM - 1
C
      CALL GETFM(NEED)
      IF(EXETYP.EQ.CHECK) GO TO 700
C
      CALL CCFOCK(INTG,X(LFAO),X(LFMO),X(LVEC),X(LFHH),X(LFPP),X(LFHP),
     *            X(LWRK),EH,EP,L1,L2,M1,M2,NO,NU,IFC)
C
  700 CONTINUE
      CALL RETFM(NEED)
C
C         prepare 2e- integrals, all passes need the disk buffers
C
      CALL VALFM(LOADFM)
      LXX    = LOADFM + 1
      LIX    = LXX    + NINTMX
      LAST   = LIX    + NINTMX
C                          storage for ipass=1
      LTI1   = LAST
      LTI3   = LTI1   + NOU2
      LVHHHH = LTI3   + NO3
      LVPHHH = LVHHHH + NO4
      LVPPHH = LVPHHH + NO3U
      LVHPPH = LVPPHH + NO2U2
      LAST1  = LVHPPH + NO2U2
      NEED1 = LAST1 - LOADFM - 1
C                          storage for ipass=2
      LTI0   = LAST
      LVPPPH = LTI0   + NU3
      LAST2  = LVPPPH + NOU3
      NEED2 = LAST2 - LOADFM - 1
C                          storage for ipass=3
C                All later steps need at least NONU3 memory, so
C                the mininum number of orbitals -NSLICE- for each
C                subpass within pass 3 can be assumed to be -NO-.
C                In other words, we can dedicate at least as much
C                memory to pass 3 subpasses as we need for pass 2.
      NSLICE = (NGOTMX-2*NINTMX-NU3)/NU3
      IF(NSLICE.LE.NO) NSLICE=NO
      IF(NSLICE.GT.NU) NSLICE=NU
      NU3NS = NU3*NSLICE
C
      LTI0   = LAST
      LVPPPP = LTI0   + NU3
      LAST3  = LVPPPP + NU3NS
      NEED3 = LAST3 - LOADFM - 1
      MNND3 = 2*NINTMX + NU3 + NU3*NO
C
      NEED = MAX(NEED1,NEED2,NEED3)
      WRITE(6,90) NEED,MAX(NEED1,NEED2,MNND3)
C
      IF(NEED.GT.NGOTMX) THEN
         WRITE(6,91) NEED,NGOTMX
         CALL ABRT
      END IF
C
      CALL GETFM(NEED)
      IF(EXETYP.EQ.CHECK) GO TO 800
C
      CALL SRTING(IJKT,X(LXX),X(LIX),NINTMX,NO,NU,NSLICE,
     *            X(LVHHHH),X(LVPHHH),X(LVHPPH),X(LVPPHH),X(LVPPPH),
     *            X(LVPPPP),X(LTI0),X(LTI1),X(LTI3),EH,EP)
C
  800 CONTINUE
      CALL RETFM(NEED)
      RETURN
C
 90   FORMAT(/1X,'MEMORY TO BE USED IN CC INTEGRAL SORTING IS',I12,
     *          ' WORDS.'/
     *        1X,'THE MINIMUM MEMORY TO ACCOMPLISH SORTING IS',I12,
     *          ' WORDS.')
 91   FORMAT(1X,'INSUFFICIENT MEMORY FOR CC INTEGRAL SORTING'/
     *       1X,'REQUIRED:',I12,'     AVAILABLE:',I12)
      END
C*MODULE CCSDT   *DECK CCFOCK
      SUBROUTINE CCFOCK(INTG,FAO,FMO,VEC,FHH,FPP,FHP,
     *                  WRK,EH,EP,L1,L2,M1,M2,NO,NU,IFC)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION FAO(L2),FMO(M2),VEC(L1,L1),FHH(NO,NO),FPP(NU,NU),
     *          FHP(NO,NU),EH(NO),EP(NU),WRK(L1)
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      PARAMETER (ZERO=0.0D+00)
C
C        read RHF fock matrix and orbitals
C
      CALL DAREAD(IDAF,IODA,FAO,L2,14,0)
      CALL DAREAD(IDAF,IODA,VEC,L1*L1,15,0)
C
C        perform a similarity transformation over only the
C        orbitals which are to be included in the correlation.
C
      CALL TFTRI(FMO,FAO,VEC(1,IFC+1),WRK,M1,L1,L1)
C
C        fill hole and particle orbital energy array
C
      II = 0
      DO 101 I=1,NO+NU
         II = II+I
         IF(I.LE.NO) THEN
            EH(I) = FMO(II)
         ELSE
            EP(I-NO) = FMO(II)
         END IF
 101  CONTINUE
C
C        fill hole/hole, particle/particle, and hole/particle
C        blocks of the Fock matrix.  diagonals are set to zero
C
      IJ = 0
      DO 103 I=1,NO
         DO 104 J=1,I
            IJ = IJ+1
            FHH(I,J)=FMO(IJ)
            FHH(J,I)=FMO(IJ)
 104     CONTINUE
         FHH(I,I)=ZERO
 103  CONTINUE
C
      DO 105 I=1,NU
         IROW = I+NO
         II = (IROW*IROW-IROW)/2
         DO 106 J=1,I
            JROW = J+NO
            IJ = II + JROW
            FPP(I,J)=FMO(IJ)
            FPP(J,I)=FMO(IJ)
 106     CONTINUE
         FPP(I,I)=ZERO
 105  CONTINUE
C
      DO 108 I=1,NU
         IROW = I+NO
         II = (IROW*IROW-IROW)/2
         DO 107 J=1,NO
            IJ = II + J
            FHP(J,I)=FMO(IJ)
 107     CONTINUE
 108  CONTINUE
C
C        These Fock blocks form the final records of the integral file
C
      NLAST=5*NO+2*NU
      WRITE(INTG,REC=NLAST+1) FHH
      WRITE(INTG,REC=NLAST+2) FPP
      WRITE(INTG,REC=NLAST+3) FHP
C
      RETURN
      END
C*MODULE CCSDT   *DECK SRTING
      SUBROUTINE SRTING(IJKT,XX,IX,NINTMX,NO,NU,NSLICE,
     *                  VHHHH,VPHHH,VHPPH,VPPHH,VPPPH,VPPPP,
     *                  TI0,TI1,TI3,EH,EP)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      INTEGER A,B,C,D
C
C        caution, the following storage is "equivalent by call"
C          pass 1 uses: ti1, ti3, vpphh, vhpph, vphhh, vhhhh
C          pass 2 uses: ti0, vppph
C          pass 3 uses: ti0, vpppp
C
      DIMENSION VHHHH(NO,NO,NO,NO),VPHHH(NU,NO,NO,NO),
     *          VHPPH(NO,NU,NU,NO),VPPHH(NU,NU,NO,NO),
     *          VPPPH(NU,NU,NU,NO),VPPPP(NSLICE,NU,NU,NU),
     *          TI0(NU,NU,NU),TI1(NU,NU,NO),TI3(NO,NO,NO),
     *          EH(NO),EP(NU),XX(NINTMX),IX(*)
C
      COMMON /CCFILE/ INTG,NT1,NT2,NT3,NVM,NVE,NFRLE,NRESF,NRESL
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCKLAB/ LABSIZ
C
C     ----- sort integrals into classes for cc calculations -----
C     within each class, direct access records are written for
C     easy recovery of all integrals with a single fixed label.
C
      NPPPP = 0
      NPPPH = 0
      NPPHH = 0
      NHPPH = 0
      NPHHH = 0
      NHHHH = 0
C
C        pass 1 sorts vhhhh, vphhh, vpphh, and vhpph
C        pass 2 sorts vppph
C        pass 3 sorts vpppp, in mxpass-2 subpasses.
C        Since pass 3 has the biggest memory need, it may be divided
C        into 'subpasses', each containing at least -NSLICE- orbitals.
C        The value of -NSLICE- is at least -NO- and no more than -NU-.
C
      IPASS  = 0
      MXPASS = 2 + (NU-1)/NSLICE + 1
      IMXVIR = 0
  100 CONTINUE
      IPASS=IPASS+1
      IF(IPASS.GE.3) THEN
         IMNVIR = IMXVIR+1
         IMXVIR = IMXVIR+NSLICE
         IF(IMXVIR.GT.NU) IMXVIR=NU
      END IF
C
      IF(IPASS.EQ.1) CALL VCLR(VHHHH,1,    NO*NO*NO*NO)
      IF(IPASS.EQ.1) CALL VCLR(VPHHH,1,    NU*NO*NO*NO)
      IF(IPASS.EQ.1) CALL VCLR(VHPPH,1,    NO*NU*NU*NO)
      IF(IPASS.EQ.1) CALL VCLR(VPPHH,1,    NO*NO*NU*NU)
      IF(IPASS.EQ.2) CALL VCLR(VPPPH,1,    NU*NU*NU*NO)
      IF(IPASS.GE.3) CALL VCLR(VPPPP,1,NSLICE*NU*NU*NU)
C
C        begin reading the transformed integral file.
C        only the master has the 1e- integral record, skip over it.
C
      CALL SEQREW(IJKT)
      IF(MASWRK) CALL SEQADV(IJKT)
C
C        Read transformed 2e- integral file in reverse canonical order.
C
  150 CONTINUE
      CALL PREAD(IJKT,XX,IX,NX,NINTMX)
      IF (NX.EQ.0) GO TO 300
      MX = ABS(NX)
      IF (MX.GT.NINTMX) THEN
         IF(MASWRK) WRITE(6,*) 'CONFUSION WITH INTEGRALS IN -SRTING-'
         CALL ABRT
      END IF
C
      DO 220 M = 1,MX
      VALL = XX(M)
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
C        switch indices to usual triangular canonical order
C        which is of course, i>=j, k>=l, ij>=kl
C
      K = IPACK
      L = JPACK
      I = KPACK
      J = LPACK
C
C        determine integral's type
C
      IF(I.LE.NO)             GO TO 2
C
      IF(K.GT.NO) GO TO 467
      IF(J.LE.NO.AND.L.LE.NO) GO TO 3
                              GO TO 5
C
  467 IF(J.GT.NO.AND.L.GT.NO) GO TO 7
      IF(J.LE.NO.AND.L.LE.NO) GO TO 4
                              GO TO 6
C
C        sort [ij|kl] integrals
C
 2    CONTINUE
      IF(IPASS.NE.1) GO TO 220
      NHHHH = NHHHH+1
      VHHHH(I,J,K,L)=VALL
      VHHHH(I,J,L,K)=VALL
      VHHHH(J,I,K,L)=VALL
      VHHHH(J,I,L,K)=VALL
      VHHHH(K,L,I,J)=VALL
      VHHHH(K,L,J,I)=VALL
      VHHHH(L,K,I,J)=VALL
      VHHHH(L,K,J,I)=VALL
      GO TO 220
C
C        sort [ai|jk] integrals
C
 3    CONTINUE
      IF(IPASS.NE.1) GO TO 220
      NPHHH = NPHHH+1
      INO=I-NO
      VPHHH(INO,K,L,J)=VALL
      VPHHH(INO,L,K,J)=VALL
      GO TO 220
C
C        sort [ia|bj] integrals
C
 4    CONTINUE
      IF(IPASS.NE.1) GO TO 220
      NHPPH = NHPPH+1
      INO=I-NO
      KNO=K-NO
      VHPPH(J,INO,KNO,L)=VALL
      VHPPH(L,KNO,INO,J)=VALL
      GO TO 220
C
C        sort [ab|ij] integrals
C
 5    CONTINUE
      IF(IPASS.NE.1) GO TO 220
      NPPHH = NPPHH+1
      INO=I-NO
      JNO=J-NO
      VPPHH(INO,JNO,K,L)=VALL
      VPPHH(JNO,INO,K,L)=VALL
      VPPHH(INO,JNO,L,K)=VALL
      VPPHH(JNO,INO,L,K)=VALL
      GO TO 220
C
C        sort [ab|ci] integrals
C
  6   CONTINUE
      IF(IPASS.NE.2) GO TO 220
      NPPPH = NPPPH+1
      IF(J.GT.NO) THEN
         INO=I-NO
         JNO=J-NO
         KNO=K-NO
         VPPPH(INO,JNO,KNO,L)=VALL
         VPPPH(JNO,INO,KNO,L)=VALL
      ELSE
         INO=I-NO
         KNO=K-NO
         LNO=L-NO
         VPPPH(KNO,LNO,INO,J)=VALL
         VPPPH(LNO,KNO,INO,J)=VALL
      END IF
      GO TO 220
C
C        sort [ab|cd] integrals
C
 7    CONTINUE
      IF(IPASS.LT.3) GO TO 220
      IF(IPASS.EQ.3) NPPPP = NPPPP+1
      INO=I-NO
      JNO=J-NO
      KNO=K-NO
      LNO=L-NO
      IF(IMNVIR.LE.INO  .AND.  INO.LE.IMXVIR) THEN
         ISL = INO - IMNVIR + 1
         VPPPP(ISL,JNO,KNO,LNO)=VALL
         VPPPP(ISL,JNO,LNO,KNO)=VALL
      END IF
      IF(IMNVIR.LE.JNO  .AND.  JNO.LE.IMXVIR) THEN
         JSL = JNO - IMNVIR + 1
         VPPPP(JSL,INO,KNO,LNO)=VALL
         VPPPP(JSL,INO,LNO,KNO)=VALL
      END IF
      IF(IMNVIR.LE.KNO  .AND.  KNO.LE.IMXVIR) THEN
         KSL = KNO - IMNVIR + 1
         VPPPP(KSL,LNO,INO,JNO)=VALL
         VPPPP(KSL,LNO,JNO,INO)=VALL
      END IF
      IF(IMNVIR.LE.LNO  .AND.  LNO.LE.IMXVIR) THEN
         LSL = LNO - IMNVIR + 1
         VPPPP(LSL,KNO,INO,JNO)=VALL
         VPPPP(LSL,KNO,JNO,INO)=VALL
      END IF
C               end of loop over integrals in the current bufferload
  220 CONTINUE
C
C         we are now done processing this integral buffer, and must
C         go back for next buffer if not at the end of integral file.
C
      IF(NX.GT.0) GO TO 150
C
C      We are now done reading entire 2e- integral file, and are now
C      ready to output the 2e- integrals to a direct access file.
C      The structure of the data in file -INTG- is to be as follows:
C         Vhpph [j,b|a,fixed i]/denom - records         1 to        NO
C         Vhpph [j,b|a,fixed i]       - records      NO+1 to      2*NO
C         Vpphh [a,b|j,fixed i]       - records    2*NO+1 to      3*NO
C       * Vppph [a,b|c,fixed i]       - records    3*NO+1 to      4*NO
C         Vphhh [fixed a,i|j,k]       - records    4*NO+1 to   NU+4*NO
C         Vhhhh [fixed i,j|k,l]       - records NU+4*NO+1 to   NU+5*NO
C       * Vpppp [fixed a,b|c,d]       - records NU+5*NO+1 to 2*NU+5*NO
C      The records marked with * have sizes NU3, and this determines
C      the length of all the records, specified during the file OPEN.
C
  300 CONTINUE
      IF(IPASS.GT.1) GO TO 400
C
C        this loop outputs vpphh and vhpph.
C
      DO 71 I=1,NO
C
      DO 74 J=1,NO
      DO 74 A=1,NU
      DO 74 B=1,NU
      TI1(A,B,J)=VPPHH(A,B,J,I)
  74  CONTINUE
      IRC=2*NO+I
      WRITE(INTG,REC=IRC) TI1
C
      DO 72 J=1,NO
      DO 72 A=1,NU
      DO 72 B=1,NU
      TI1(A,B,J)=VHPPH(J,B,A,I)
  72  CONTINUE
      IRC=NO+I
      WRITE(INTG,REC=IRC) TI1
C           note this record is the same but with an energy denominator
      DO 73 J=1,NO
      DO 73 A=1,NU
      DO 73 B=1,NU
      DEN=EH(I)+EH(J)-EP(A)-EP(B)
      TI1(A,B,J)=VHPPH(J,B,A,I)/DEN
  73  CONTINUE
      IRC=I
      WRITE(INTG,REC=IRC) TI1
C
  71  CONTINUE
C
      DO 81 A=1,NU
      DO 82 I=1,NO
      DO 82 J=1,NO
      DO 82 K=1,NO
      TI3(I,J,K)=VPHHH(A,I,J,K)
  82  CONTINUE
      IRC=4*NO+A
      WRITE(INTG,REC=IRC) TI3
  81  CONTINUE
C
      NLAST =4*NO+NU
      CALL MTRANS(VHHHH,NO,7)
      DO 75 I=1,NO
      DO 76 J=1,NO
      DO 76 K=1,NO
      DO 76 L=1,NO
      TI3(J,K,L)=VHHHH(I,J,K,L)
  76  CONTINUE
      IRC=NLAST +I
      WRITE(INTG,REC=IRC) TI3
  75  CONTINUE
      GO TO 600
C
  400 CONTINUE
      IF(IPASS.GT.2) GO TO 500
      DO 79 I=1,NO
      DO 80 A=1,NU
      DO 80 B=1,NU
      DO 80 C=1,NU
      TI0(A,B,C)=VPPPH(A,B,C,I)
  80  CONTINUE
      IRC=3*NO+I
      WRITE(INTG,REC=IRC) TI0
  79  CONTINUE
      GO TO 600
C
  500 CONTINUE
      NLAST=5*NO+NU+IMNVIR-1
      MAXA = NSLICE
      IF(IPASS.EQ.MXPASS) MAXA = NU - NSLICE*(MXPASS-3)
      DO 77 A=1,MAXA
      DO 78 B=1,NU
      DO 78 C=1,NU
      DO 78 D=1,NU
      TI0(B,C,D)=VPPPP(A,B,C,D)
  78  CONTINUE
      IRC=NLAST+A
      WRITE(INTG,REC=IRC) TI0
  77  CONTINUE
C
  600 CONTINUE
      IF(IPASS.LT.MXPASS) GO TO 100
C
      CALL SEQREW(IJKT)
      NINT = NHHHH + NPHHH + NPPHH + NHPPH + NPPPH + NPPPP
      WRITE(6,9010) NINT,INTG,
     *              NHHHH,NPHHH,NPPHH,NHPPH,NPPPH,NPPPP,
     *              IJKT,MXPASS
      RETURN
 9010 FORMAT(1X,I12,' NON-ZERO TRANSFORMED 2E- INTEGRALS WERE',
     *              ' SORTED INTO FILE',I3,':'/
     *       1X,I12,' [IJ|KL] TYPE,',I12,' [AJ|KL] TYPE,'/
     *       1X,I12,' [AB|IJ] TYPE,',I12,' [IA|BJ] TYPE,'/
     *       1X,I12,' [AB|CI] TYPE,',I12,' [AB|CD] TYPE.'/
     *       1X,'TRANSFORMED INTEGRAL FILE',I3,' WAS READ',I5,' TIMES.')
      END
C
C         this was originally ccsd.f
C
C*MODULE CCSDT   *DECK DRCCSD
      SUBROUTINE DRCCSD(NO,NU,OEH,OEP,IFC,EOM)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL EOM
      LOGICAL CNVR
      DIMENSION OEH(NO),OEP(NU)
C
      COMMON /CCENGY/ ENRG,EREF,EMP2,ETOT(6),ECORR(6),
     *                DIAGS(3),AMPMX(5,2),IAMPMX(5,4,2),XO1,XO2,
     *                DIFMAX,DIFFENG,ITER,CNVR
      COMMON /CCINFO/ TSH,NH,NP,MET,MEM,ICONV,MAXIT,IREST,IDISC
      COMMON /FMCOM / X(1)
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
C        ----- allocate memory for the CCSD iterations -----
C
      CALL GOTFM(NGOTMX)
      IF(MEM.LT.NGOTMX) NGOTMX=MEM
C
      NO2   = NO*NO
      NO3   = NO*NO2
      NO4   = NO*NO3
      NU2   = NU*NU
      NU3   = NU*NU2
      NOU   = NO*NU
      NO3U  = NO3*NU
      NOU3  = NO*NU3
      NO2U2 = NO2*NU2
C
C        caution.  The first array allocated here, at address -LO1-
C        will be passed to routines drcmp and drrlen, which will
C        re-allocate several arrays beginning at the same address.
C
      CALL VALFM(LOADFM)
      LO1  = LOADFM + 1
      LT1  = LO1    + NOU
      LFH  = LT1    + NOU
      LFPH = LFH    + NO2
      LFP  = LFPH   + NOU
      LVHH = LFP    + NU2
      LVM  = LVHH   + NO4
      LTI  = LVM    + NO3U
      LO2  = LTI    + NU3
      LT2  = LO2    + NO2U2
      LVL  = LT2    + NO2U2
      LVR  = LVL    + NO2U2
      LVPP = LVR    + NO2U2
      LAST = LVPP   + NOU3
      NEED = LAST - LOADFM - 1
      IF(MET.EQ.0) WRITE(6,190) NEED
      IF(MET.EQ.1) WRITE(6,290) NEED
      IF(MET.GE.2) WRITE(6,390) NEED
C
      IF(NEED.GT.NGOTMX) THEN
         IF(MEM.EQ.0) WRITE(6,191)
         IF(MEM.EQ.1) WRITE(6,291)
         IF(MEM.EQ.2) WRITE(6,391)
         WRITE(6,92) NEED,NGOTMX
         CALL ABRT
      END IF
C
      CALL GETFM(NEED)
      IF(EXETYP.EQ.CHECK) CNVR=.TRUE.
      IF(EXETYP.EQ.CHECK) GO TO 800
C
C        restart amplitudes provided by user, or from previous geometry
C
      IF(IREST.GT.0  .OR.  NEVALS.GT.0) THEN
         IF(NEVALS.GT.0) IREST=3
         IF(NEVALS.GT.0) ITER=IREST
         WRITE(6,404)
         CALL DRREST(0,NO,NU,X(LVL),X(LVR))
      END IF
C
C        perform CCSD iterations for T1 and T2 amplitudes
C
      CALL CCSD(NO,NU,X(LO1),X(LT1),X(LFH),X(LFPH),X(LFP),
     *          X(LVHH),X(LVM),X(LTI),X(LO2),X(LT2),X(LVL),X(LVR),
     *          X(LVPP),OEH,OEP,IFC,EOM)
C
  800 CONTINUE
      CALL RETFM(NEED)
      RETURN
C
190   FORMAT(/1X,'MEMORY REQUIRED FOR THE LCCD ITERATIONS IS',
     *            I12,' WORDS.')
290   FORMAT(/1X,'MEMORY REQUIRED FOR THE CCD ITERATIONS IS',
     *            I12,' WORDS.')
390   FORMAT(/1X,'MEMORY REQUIRED FOR THE CCSD ITERATIONS IS',
     *            I12,' WORDS.')
191   FORMAT(1X,'INSUFFICIENT MEMORY FOR LCCD STEP')
291   FORMAT(1X,'INSUFFICIENT MEMORY FOR CCD STEP')
391   FORMAT(1X,'INSUFFICIENT MEMORY FOR CCSD STEP')
 92   FORMAT(1X,'REQUIRED:',I12,'     AVAILABLE:',I12)
  404 FORMAT(1X,'RESTARTING USING PREVIOUS AMPLITUDES READ FROM A',
     *          ' DISK FILE.')
      END
C*MODULE CCSDT   *DECK CCSD
      SUBROUTINE CCSD(NO,NU,O1,T1,FH,FPH,FP,VHH,VM,TI,O2,T2,VL,VR,
     *                VPP,OEH,OEP,IFC,EOM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL EOM
      LOGICAL LCCD,CCD,CNVR
      INTEGER A,B
C
      COMMON /CCENGY/ ENRG,EREF,EMP2,ETOT(6),ECORR(6),
     *                DIAGS(3),AMPMX(5,2),IAMPMX(5,4,2),XO1,XO2,
     *                DIFMAX,DIFFENG,ITER,CNVR
      COMMON /CCFILE/ INTG,NT1,NT2,NT3,NVM,NVE,NFRLE,NRESF,NRESL
      COMMON /CCINFO/ TSH,NH,NP,MET,MEM,ICONV,MAXIT,IREST,IDISC
      COMMON /CCRENO/ OSS,ODS,ODD,OTS,OTD,OTT,ODS_S,ODS_D,ODS_T,
     *                OQS,OQDS,OQDD,OQTS,ESD,ETD,ETS,ETTM,ESD_TM
      COMMON /CCRLE / MXRLE,NRLE0,NRLE,IRLE,ITRLE
C
      DIMENSION VHH(1),TI(1),OEH(NO),OEP(NU),T2(NO,NO,NU,NU),FP(NU,NU),
     *          FH(NO,NO),O2(1),VPP(1),VR(1),VL(1),O1(1),T1(1),VM(1),
     *          FPH(NO,NU)
C
      DATA ZERO/0.0D+00/,TWO/2.0D+00/,HALF/0.5D+00/,TEN/10.0D+00/
     *     ONE/1.0D+00/,ONEM/-1.0D+00/,LCCD,CCD/2*.FALSE./
C
      NO2   = NO*NO
      NO3   = NO*NO2
      NO4   = NO*NO3
      NU2   = NU*NU
      NOU   = NO*NU
      NO2U  = NO2*NU
      NOU2  = NO*NU2
      NO2U2 = NO2*NU2
C
C ---- interface to MM23 program ------
C
C      nft131 = 67
C      nft132 = 68
C      open(nft131,file='EH',status='unknown')
C      open(nft132,file='EP',status='unknown')
C      do 781,i=1,no
C       write(nft131,790) oeh(i)
C781   continue
C      do 782,i=1,nu
C       write(nft132,790) oep(i)
C782   continue
C      close(nft131)
C      close(nft132)
C790   format(d25.16)
C
C ---- end of interface to MM23 program ------
C
      TRESH=TEN**(-ICONV)
      CNVR=.FALSE.
      LCCD=MET.EQ.0
      CCD =MET.EQ.1
      CALL RO2HPP(1,NO,NU,TI,O2)
      CALL ADDDEN(NO,NU,O2,OEH,OEP)
      CALL ENERGYMM(NO,NU,TI,O2,T2,EMP2)
      IF(IREST.EQ.0.OR.MET.LT.2) THEN
         CALL ZEROMA(O1,1,NOU)
         CALL WO1(1,NO,NU,O1)
         CALL WO1(2,NO,NU,O1)
      END IF
      IF(IREST.EQ.0) THEN
      CALL WO2(1,NO,NU,O2)
      ENRGOLD=EMP2
      ITER=0
      END IF
      ITRLE=0
 1000 CONTINUE
      CALL RF(NO,NU,FH,FP,FPH)
      CALL VECCOP(NOU,T1,FPH)
      CALL ZEROMA(T2,1,NO2U2)
      ITER=ITER+1
      ITRLE=ITRLE+1
      CALL RO2(1,NO,NU,O2)
C      call chksum('o2      ',o2,no2u2)
      CALL RDOV4(1,NU,NO,TI,VHH)
      CALL RO2HPP(2,NO,NU,TI,VL)
      CALL INSITU(NO,NU,NU,NO,TI,VL,12)
      CALL RO1(1,NO,NU,O1)
C      call chksum('o1      ',o1,nou)
      CALL RDVEM4(1,NU,NO,TI,VM)
      CALL RO2HPP(1,NO,NU,TI,VR)
      IF(LCCD)GOTO 100
      IF(CCD)GOTO 90
      CALL TRT1(NO,NU,TI,O1)
      CALL SYMT21(VR,NO,NU,NU,NO,23)
      CALL DGEMM('N','N',NOU,1,NOU,ONE,VR,NOU,O1,NOU,ONE,FPH,NOU)
      CALL DESM21(VR,NO,NU,NU,NO,23)
      CALL TRT1(NO,NU,TI,FPH)
      CALL INSITU(NO,NU,NU,NO,TI,VR,12)
      CALL RDVEM4(0,NO,NU,TI,VPP)
      CALL SYMT21(VPP,NU,NU,NU,NO,13)
      CALL DGEMM('N','N',NU2,1,NOU,ONE,VPP,NU2,O1,NU2,ONE,FP,NU2)
      CALL DGEMM('N','T',NU,NU,NO,ONEM,FPH,NU,O1,NU,ONE,FP,NU)
C      CALL CCMATMUL(fph,o1,fp,nu,nu,no,0,1)
      CALL DESM21(VPP,NU,NU,NU,NO,13)
      CALL TRANMD(VR,NU,NO,NU,NO,13)
      CALL TRMD(O2,TI,NU,NO,2)
 90   CONTINUE
      OL=ONE
      IF(CCD) THEN
      OL=ZERO
      CALL TRMD(O2,TI,NU,NO,2)
      CALL INSITU(NO,NU,NU,NO,TI,VR,12)
      CALL TRANMD(VR,NU,NO,NU,NO,13)
      END IF
      CALL SYMT21(O2,NO,NU,NO,NU,13)
      CALL DGEMM('N','N',NU,NU,NO2U,ONEM,VR,NU,O2,NO2U,OL,FP,NU)
      IF(EOM) THEN
         NHHHH = 85
         CALL WCCFL(NHHHH,2,NU2,FP)
      END IF
C
      CALL DESM21(O2,NO,NU,NO,NU,13)
      IF(CCD)GOTO 91
C      call ADT12(2,NO,NU,o1,o2,1)
      CALL TRANMD(VR,NU,NO,NU,NO,13)
      CALL TRMD(O2,TI,NU,NO,4)
      CALL TRT1(NU,NO,TI,O1)
      CALL SYMT21(VM,NO,NO,NO,NU,13)
      CALL DGEMM('N','N',NO2,1,NOU,ONE,VM,NO2,O1,NOU,ONE,FH,NO2)
      CALL DESM21(VM,NO,NO,NO,NU,13)
      CALL TRANSQ(FH,NO)
 91   CONTINUE
      IF(CCD) THEN
      CALL TRANMD(VR,NU,NO,NU,NO,13)
      CALL TRMD(O2,TI,NU,NO,4)
      END IF
      CALL TRMD(VR,TI,NU,NO,7)
      CALL SYMT21(O2,NO,NU,NU,NO,23)
      CALL DGEMM('N','N',NO,NO,NOU2,ONE,O2,NO,VR,NOU2,OL,FH,NO)
      CALL DESM21(O2,NO,NU,NU,NO,23)
      IF(CCD)GOTO 92
      CALL TRMD(VR,TI,NU,NO,8)
      CALL DGEMM('N','N',NO,NU,NU,ONE,O1,NO,FP,NU,ONE,T1,NO)
      CALL DGEMM('N','N',NO,NU,NO,ONEM,FH,NO,O1,NO,ONE,T1,NO)
      CALL SYMT21(O2,NO,NU,NU,NO,14)
      CALL DGEMM('N','N',NOU,1,NOU,ONE,O2,NOU,FPH,NOU,ONE,T1,NOU)
      CALL DESM21(O2,NO,NU,NU,NO,14)
      CALL TRT1(NO,NU,TI,T1)
      CALL TRT1(NO,NU,TI,O1)
      CALL VECMUL(VR,NO2U2,TWO)
      CALL VECSUB(VR,VL,NO2U2)
      CALL DGEMM('N','N',1,NOU,NOU,ONE,O1,1,VR,NOU,ONE,T1,1)
      CALL VECADD(VR,VL,NO2U2)
      CALL VECMUL(VR,NO2U2,HALF)
      CALL TRMD(O2,TI,NU,NO,10)
      CALL SYMT21(O2,NU,NU,NO,NO,12)
      CALL DGEMM('N','N',NU,NO,NOU2,ONE,VPP,NU,O2,NOU2,ONE,T1,NU)
      CALL TRT1(NU,NO,TI,T1)
      CALL TRMD(O2,TI,NU,NO,11)
      CALL DGEMM('N','N',NO,NU,NO2U,ONEM,VM,NO,O2,NO2U,ONE,T1,NO)
      CALL DESM21(O2,NO,NO,NU,NU,12)
      CALL TRMD(O2,TI,NU,NO,12)
      CALL TRT1(NU,NO,TI,O1)
      CALL ADDDEN1(NO,NU,T1,OEH,OEP)
      IF(ITER.LE.3)CALL ZEROMA(T1,1,NOU)
      CALL WO1(2,NO,NU,T1)
      CALL TRT1(NU,NO,TI,FPH)
      CALL DGEMM('N','T',NO,NO,NU,ONE,O1,NO,FPH,NO,ONE,FH,NO)
C
      IF(EOM) THEN
         CALL TRANSQ(FH,NO)
         CALL WCCFL(NHHHH,1,NO2,FH)
         CALL TRANSQ(FH,NO)
      END IF
C
      CALL WO1(3,NO,NU,FPH)
 92   CONTINUE
      IF(CCD) THEN
      CALL TRMD(VR,TI,NU,NO,8)
      END IF
      CALL DGEMM('N','T',NOU2,NO,NO,ONEM,O2,NOU2,FH,NO,ZERO,T2,NOU2)
C      call chksum('t2 1    ',t2,no2u2)
      CALL TRMD(T2,TI,NU,NO,2)
      CALL TRMD(O2,TI,NU,NO,2)
      CALL DGEMM('N','N',NO2U,NU,NU,ONE,O2,NO2U,FP,NU,ONE,T2,NO2U)
C      call chksum('t2 1    ',t2,no2u2)
      CALL INSITU(NU,NO,NU,NO,TI,VR,23)
      CALL TRMD(O2,TI,NU,NO,14)
      CALL ADT12(1,NO,NU,O1,O2,3)
      CALL DGEMM('N','N',NO2,NO2,NU2,ONE,O2,NO2,VR,NU2,ONE,VHH,NO2)
      IF(CCD)GOTO 93
      CALL MTRANS(VHH,NO,11)
      CALL VECMUL(VHH,NO4,HALF)
      CALL DGEMM('N','T',NO3,NO,NU,ONE,VM,NO3,O1,NO,ONE,VHH,NO3)
      CALL SYMV1(VHH,NO2)
      CALL TRMD(VHH,TI,NU,NO,16)
 93   CONTINUE
      IF(CCD) THEN
      CALL MTRANS(VHH,NO,19)
      END IF
      CALL TRANMD(O2,NO,NO,NU,NU,12)
      CALL TRMD(T2,TI,NU,NO,18)
 100  CONTINUE
      CALL VECMUL(O2,NO2U2,HALF)
      IF(LCCD) THEN
      CALL TRMD(O2,TI,NU,NO,39)
      END IF
      OL=ONE
      IF(LCCD)OL=ZERO
C
      IF(EOM) CALL WCCFL(NHHHH,3,NO4,VHH)
C
      CALL DGEMM('N','N',NO2,NU2,NO2,ONE,VHH,NO2,O2,NO2,OL,T2,NO2)
      DO I=1,NU
      CALL RDVPP(I,NO,NU,TI)
      CALL DGEMM('N','N',NO2,NU,NU2,ONE,O2,NO2,TI,NU2,ONE,
     &T2(1,1,1,I),NO2)
      ENDDO
      CALL VECMUL(O2,NO2U2,TWO)
      IF(LCCD)GOTO 101
      CALL INSITU(NU,NU,NO,NO,TI,VR,23)
      CALL TRMD(VR,TI,NU,NO,13)
      IF(.NOT.CCD)CALL ADT12(2,NO,NU,O1,O2,2)
      CALL TRMD(O2,TI,NU,NO,20)
      CALL TRMD(VR,TI,NU,NO,21)
      CALL VECMUL(O2,NO2U2,HALF)
      IF(.NOT.CCD)CALL ADT12(1,NO,NU,O1,O2,4)
      CALL DGEMM('N','N',NOU,NOU,NOU,ONEM,VR,NOU,O2,NOU,ONE,VL,NOU)
      IF(.NOT.CCD)CALL ADT12(2,NO,NU,O1,O2,4)
      CALL VECMUL(O2,NO2U2,TWO)
      IF(CCD)GOTO 94
      CALL TRMD(VM,TI,NU,NO,22)
      CALL TRMD(VL,TI,NU,NO,23)
      CALL DGEMM('T','N',NU,NO2U,NO,ONEM,O1,NO,VM,NO,ONE,VL,NU)
      CALL TRMD(VM,TI,NU,NO,22)
      CALL TRMD(VL,TI,NU,NO,24)
      CALL TRMD(VPP,TI,NU,NO,25)
      CALL DGEMM('N','N',NO,NOU2,NU,ONE,O1,NO,VPP,NU,ONE,VL,NO)
      CALL TRMD(VL,TI,NU,NO,26)
 94   CONTINUE
      CALL TRMD(O2,TI,NU,NO,27)
      CALL TRMD(T2,TI,NU,NO,28)
 101  CONTINUE
      IF(LCCD) THEN
      CALL TRMD(O2,TI,NU,NO,28)
      CALL TRMD(T2,TI,NU,NO,28)
      END IF
      CALL DGEMM('N','N',NOU,NOU,NOU,ONEM,O2,NOU,VL,NOU,ONE,T2,NOU)
C      call chksum('t2 1    ',t2,no2u2)
      CALL TRANMD(O2,NO,NU,NU,NO,23)
      CALL TRANMD(T2,NO,NU,NU,NO,23)
      CALL DGEMM('N','N',NOU,NOU,NOU,ONEM,O2,NOU,VL,NOU,ONE,T2,NOU)
C      call chksum('t2 1    ',t2,no2u2)
      IF(LCCD)GOTO 102
      CALL TRMD(VR,TI,NU,NO,30)
      CALL TRMD(O2,TI,NU,NO,31)
      CALL VECMUL(O2,NO2U2,HALF)
      CALL SYMT21(VR,NU,NO,NU,NO,13)
      CALL DGEMM('N','N',NOU,NOU,NOU,ONE,VR,NOU,O2,NOU,ZERO,VL,NOU)
      CALL DESM21(VR,NU,NO,NU,NO,13)
      CALL TRANMD(O2,NU,NO,NU,NO,13)
      IF(.NOT.CCD)CALL ADT12(1,NO,NU,O1,O2,5)
      CALL DGEMM('N','N',NOU,NOU,NOU,ONEM,VR,NOU,O2,NOU,ONE,VL,NOU)
      IF(CCD)GOTO 95
      CALL ADT12(2,NO,NU,O1,O2,5)
      CALL VECMUL(O2,NO2U2,TWO)
      CALL TRMD(VPP,TI,NU,NO,25)
      CALL TRMD(VL,TI,NU,NO,33)
      CALL DGEMM('N','N',NO,NOU2,NU,ONE,O1,NO,VPP,NU,ONE,VL,NO)
      CALL TRMD(VL,TI,NU,NO,34)
      CALL DGEMM('T','N',NU,NO2U,NO,ONEM,O1,NO,VM,NO,ONE,VL,NU)
 95   CONTINUE
      IF(CCD) THEN
      CALL VECMUL(O2,NO2U2,TWO)
      CALL TRMD(VL,TI,NU,NO,33)
      CALL TRMD(VL,TI,NU,NO,34)
      END IF
      CALL TRMD(VL,TI,NU,NO,35)
      CALL VECADD(VL,VR,NO2U2)
      CALL TRMD(O2,TI,NU,NO,36)
 102  CONTINUE
      IF(LCCD) THEN
      CALL RO2HPP(1,NO,NU,TI,VL)
      CALL INSITU(NO,NU,NU,NO,TI,VL,12)
      END IF
      CALL SYMT21(O2,NO,NU,NU,NO,23)
      CALL DGEMM('N','N',NOU,NOU,NOU,ONE,O2,NOU,VL,NOU,ONE,T2,NOU)
C      call chksum('t2 1    ',t2,no2u2)
      IF(LCCD.OR.CCD)GOTO 103
      CALL DESM21(O2,NO,NU,NU,NO,23)
      CALL RO2HPP(2,NO,NU,TI,VL)
      CALL INSITU(NO,NU,NU,NO,TI,VL,12)
      CALL TRMD(VL,TI,NU,NO,37)
      CALL TRMD(VPP,TI,NU,NO,25)
      CALL DGEMM('T','N',NU,NOU2,NO,ONEM,O1,NO,VL,NO,ONE,VPP,NU)
      CALL TRMD(VPP,TI,NU,NO,25)
      CALL TRMD(VR,TI,NU,NO,37)
      CALL DGEMM('T','N',NU,NOU2,NO,ONEM,O1,NO,VR,NO,ONE,VPP,NU)
      CALL TRMD(VPP,TI,NU,NO,38)
      CALL DGEMM('N','N',NO,NOU2,NU,ONE,O1,NO,VPP,NU,ONE,T2,NO)
C      call chksum('t2 1    ',t2,no2u2)
      CALL TRMD(T2,TI,NU,NO,24)
      CALL INSITU(NO,NU,NU,NO,TI,O2,12)
      CALL INSITU(NU,NO,NU,NO,TI,O2,23)
      CALL RDVEM4(0,NO,NU,TI,VPP)
      CALL TRANMD(VPP,NU,NU,NU,NO,13)
      CALL TRANMD(VM,NO,NO,NO,NU,312)
      CALL DGEMM('T','N',NO2,NOU,NU2,ONE,O2,NU2,VPP,NU2,ZERO,VR,NO2)
      CALL VMCP(NO,NU,VM,VR)
      CALL TRANMD(VM,NO,NO,NO,NU,23)
      CALL TRANMD(VPP,NU,NU,NU,NO,13)
      CALL DGEMM('N','N',NO,NOU2,NU,ONE,O1,NO,VPP,NU,ZERO,VL,NO)
      CALL TRANSQ(VL,NOU)
      CALL DGEMM('N','N',NO,NO2U,NU,ONE,O1,NO,VL,NU,ONE,VM,NO)
      CALL TRANMD(VM,NO,NO,NO,NU,12)
      CALL DGEMM('T','N',NU,NO2U,NO,ONEM,O1,NO,VM,NO,ONE,T2,NU)
C      call chksum('t2 1    ',t2,no2u2)
      CALL TRANSQ(T2,NOU)
 103  CONTINUE
      CALL SYMETR(T2,NO,NU)
C      call chksum('t2symetr ',t2,no2u2)
      CALL RO2HPP(1,NO,NU,TI,O2)
      CALL VECADD(T2,O2,NO2U2)
      CALL ADDDEN(NO,NU,T2,OEH,OEP)
      CALL WO2(2,NO,NU,T2)
C      call chksum('t2 1    ',t2,no2u2)
      CALL VECCOP(NO2U2,O2,T2)
      IF(.NOT.LCCD.AND..NOT.CCD)CALL ADT12(1,NO,NU,T1,T2,6)
      NLAST=5*NO+2*NU
      READ(INTG,REC=NLAST+3)FPH
      ZZ=TWO*DDOT(NOU,T1,1,FPH,1)
      CALL ENERGYMM(NO,NU,TI,T2,VR,ENRG)
      ENRG=ENRG+ZZ
      DIFFENG=ENRG-ENRGOLD
C
C       *** use a Jacobi procedure to update amplitudes ***
C
      IF(MXRLE.GT.0) GOTO 717
      CALL DRCMP(NO,NU,O1)
      IF(ABS(DIFMAX).GT.1.0D+04) GOTO 718
      CNVR=ABS(DIFMAX).LT.TRESH
      IF(.NOT.LCCD.AND..NOT.CCD) CALL RO1(2,NO,NU,T1)
      IF(.NOT.LCCD.AND..NOT.CCD) CALL WO1(1,NO,NU,T1)
      CALL RO2(2,NO,NU,T2)
      CALL WO2(1,NO,NU,T2)
C
      CALL DRPRINT(2)
      ENRGOLD=ENRG
C
      CALL DRREST(1,NO,NU,VL,VR)
      IF(ITER.LT.MAXIT.AND..NOT.CNVR) GOTO 1000
      GOTO 718
C
C       *** use reduced linear equation solver to update amplitudes ***
C
 717  CONTINUE
C
C           caution: drcmp will set several amplitude quantities
C           in the -o1- storage vector, to pass to drrlen.
C
      IF(IREST.GT.0.AND.ITRLE.EQ.1)CALL DRCMP(NO,NU,O1)
      CNVR=ABS(DIFMAX).LT.TRESH
      IF(CNVR.AND.ITER.GT.1)CALL DRPRINT(2)
C
      IF(.NOT.CNVR.OR.ITER.LT.2) THEN
      IF(.NOT.LCCD.AND..NOT.CCD)CALL RO1(2,NO,NU,T1)
C      call ro2(2,no,nu,t2)
      IF(MXRLE.GT.0) CALL DRRLEN(NO,NU,O1)
C
      CALL DRPRINT(2)
C
      IF(ITRLE.EQ.1) THEN
         IF(.NOT.LCCD.AND..NOT.CCD) THEN
            CALL RO1(2,NO,NU,T1)
            CALL WO1(1,NO,NU,T1)
         END IF
         CALL RO2(2,NO,NU,T2)
         CALL WO2(1,NO,NU,T2)
      END IF
      ENRGOLD=ENRG
C
C        next call uses -vl- and -vr- as scratch storage
C
      CALL DRREST(1,NO,NU,VL,VR)
      IF(ITER.LT.MAXIT) GOTO 1000
      END IF
C
C        *** convergence of cc has been obtained ***
C
  718 CONTINUE
C
C        ----- prepare some amplitude diagnostics -----
C
      IF(MET.GE.2) THEN
C
C               Read converged amplitudes from disk.
C        These have the shape t1(no,nu) and t2(no,nu,nu,no).
C        Note that the shape of T2 at this point does not match
C        the dimension above, so we use -VR- as storage of T2.
C
        CALL RO1(1,NO,NU,T1)
        CALL RO2(2,NO,NU,VR)
C
C        compute the T1 diagnostic of
C        T.J.Lee, P.R.Taylor  Int.J.Quantum Chem. S23, 199-207(1989)
C
        T1NORM=DDOT(NOU  ,T1,1,T1,1)
        T2NORM=DDOT(NO2U2,VR,1,VR,1)
        DIAGS(1) = SQRT(T1NORM/(TWO*NO))
        DIAGS(2) = SQRT(T1NORM)
        DIAGS(3) = SQRT(T2NORM)
C
C        Find largest amplitudes in t1(no,nu) and t2(no,nu,nu,no)
C        Note that the process destroys the biggest values.
C
        DO III=1,5
           IMAX = IDAMAX(NOU,T1,1)
           A = (IMAX-1)/NO + 1
           I = IMAX - (A-1)*NO
            AMPMX(III,  1) = T1(IMAX)
           IAMPMX(III,1,1) = I + IFC
           IAMPMX(III,2,1) = A + IFC + NO
           T1(IMAX) = ZERO
C
           IMAX = IDAMAX(NO2U2,VR,1)
           J = (IMAX-1)/NOU2 + 1
           IDMY = IMAX - (J-1)*NOU2
           B = (IDMY-1)/NOU + 1
           IDMY = IDMY - (B-1)*NOU
           A = (IDMY-1)/NO + 1
           I = IDMY - (A-1)*NO
            AMPMX(III,  2) = VR(IMAX)
           VR(IMAX) = ZERO
C               t2(i,a,b,j) equals t2(j,b,a,i)
           IMAX = J + (B-1)*NO + (A-1)*NOU + (I-1)*NOU2
           VR(IMAX) = ZERO
           IF(I.EQ.J  .AND.  A.GT.B) THEN
              K = B
              B = A
              A = K
           END IF
#if defined(SPEC_CPU)
C  jray: This will select the indices such that I < J.
C  As t2(i,a,b,j) is theoritically same as t2(j,b,a,i),
C  IDAMAX(...) can choose one or the other depending
C  on optimization etc. This way, the output will
C  always validate independent of which element is
C  chosen (This should fix the VE with ifort 8.1 + WinXP
C  with high optimizations).
           IF(I.GT.J) THEN
#else
           IF(I.NE.J) THEN
#endif
              K = J
              J = I
              I = K
              K = B
              B = A
              A = K
           END IF
           IAMPMX(III,1,2) = I + IFC
           IAMPMX(III,2,2) = J + IFC
           IAMPMX(III,3,2) = A + IFC + NO
           IAMPMX(III,4,2) = B + IFC + NO
        ENDDO
      END IF
C
      IF(MET.GE.4) THEN
      CALL RO1(1,NO,NU,T1)
      CALL RO2(1,NO,NU,T2)
      CALL VECCOP(NO2U2,O2,T2)
      CALL ADT12(1,NO,NU,T1,T2,6)
      OSS=TWO*DDOT(NOU,T1,1,T1,1)
      CALL SYMT21(O2,NO,NU,NU,NO,23)
      ODS=ZERO
      ODD=DDOT(NO2U2,O2,1,T2,1)
      END IF
      ESD=ENRG
      RETURN
      END
C*MODULE CCSDT   *DECK DRCMP
      SUBROUTINE DRCMP(NO,NU,O1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION O1(*)
C
      NOU  = NO*NU
      NO2U2= NOU*NOU
C
      I1=1
      I2=I1+NOU+NO2U2
      CALL CMPAMP(NO,NU,O1,O1(I2))
      RETURN
      END
C*MODULE CCSDT   *DECK CMPAMP
      SUBROUTINE CMPAMP(NO,NU,O1,T1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL CNVR
      DIMENSION O1(1),T1(1)
      COMMON /CCENGY/ ENRG,EREF,EMP2,ETOT(6),ECORR(6),
     *                DIAGS(3),AMPMX(5,2),IAMPMX(5,4,2),XO1,XO2,
     *                DIFMAX,DIFFENG,ITER,CNVR
C
      NOU  = NO*NU
      NO2U2= NOU*NOU
C
      CALL RO1(1,NO,NU,O1)
      CALL RO1(2,NO,NU,T1)
      CALL RO2(1,NO,NU,O1(1+NOU))
      CALL RO2(2,NO,NU,T1(1+NOU))
      NOUP=NOU+NO2U2
      CALL VECSUB(O1,T1,NOUP)
      IM=IDAMAX(NOUP,O1,1)
      DIFMAX=O1(IM)
      RETURN
      END
C*MODULE CCSDT   *DECK RF
      SUBROUTINE RF(NO,NU,FH,FP,FHP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION FH(NO,NO),FP(NU,NU),FHP(NO,NU)
      COMMON /CCFILE/ INTG,NT1,NT2,NT3,NVM,NVE,NFRLE,NRESF,NRESL
C
      NLAST=5*NO+2*NU
      READ(INTG,REC=NLAST+1)FH
      READ(INTG,REC=NLAST+2)FP
      READ(INTG,REC=NLAST+3)FHP
      RETURN
      END
C*MODULE CCSDT   *DECK ADDDEN
      SUBROUTINE ADDDEN(NO,NU,T2,EH,EP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER A,B
      DIMENSION T2(NO,NU,NU,NO),EH(NO),EP(NU)
C
      DO 1 J=1,NO
      DO 1 B=1,NU
      DO 1 A=1,NU
      DO 1 I=1,NO
         DEN=EH(I)+EH(J)-EP(A)-EP(B)
         T2(I,A,B,J)=T2(I,A,B,J)/DEN
 1    CONTINUE
      RETURN
      END
C*MODULE CCSDT   *DECK ENERGYMM
      SUBROUTINE ENERGYMM(NO,NU,TI,O2,V,Y)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION V(1),TI(1),O2(1)
      DATA TWO/2.0D+00/,HALF/0.5D+00/
C
      NO2U2 = NO*NO*NU*NU
      CALL RO2HPP(1,NO,NU,TI,V)
      CALL VECMUL(V,NO2U2,TWO)
      Y=DDOT(NO2U2,O2,1,V,1)
      CALL TRANMD(V,NO,NU,NU,NO,23)
      CALL VECMUL(V,NO2U2,HALF)
      Y=Y-DDOT(NO2U2,O2,1,V,1)
      RETURN
      END
C*MODULE CCSDT   *DECK ADDDEN1
      SUBROUTINE ADDDEN1(NO,NU,T1,EH,EP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER A
      DIMENSION T1(NO,NU),EH(NO),EP(NU)
C
      DO 2 I=1,NO
      DO 2 A=1,NU
         DEN=EH(I)-EP(A)
         T1(I,A)=T1(I,A)/DEN
 2    CONTINUE
      RETURN
      END
C*MODULE CCSDT   *DECK ADT12
      SUBROUTINE ADT12(IGOTO,NO,NU,T1,T2,ITYP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER A,B
      DIMENSION T1(1),T2(1)
C
      NOU  = NO*NU
      NO2  = NO*NO
      NO2U = NO2*NU
      NOU2 = NOU*NU
C
      DO 10 I=1,NO
      DO 10 J=1,NO
      DO 10 A=1,NU
      DO 10 B=1,NU
         IF(ITYP.EQ.1)IT2=(A-1)*NO2U+(I-1)*NOU+(B-1)*NO+J    !HPHP
         IF(ITYP.EQ.2)IT2=(A-1)*NO2U+(B-1)*NO2+(J-1)*NO+I    !HHPP
         IF(ITYP.EQ.3)IT2=(B-1)*NO2U+(A-1)*NO2+(J-1)*NO+I    !HHPPN
         IF(ITYP.EQ.4)IT2=(I-1)*NOU2+(B-1)*NOU+(J-1)*NU+A    !PHPH
         IF(ITYP.EQ.5)IT2=(J-1)*NOU2+(A-1)*NOU+(I-1)*NU+B    !PHPHN
         IF(ITYP.EQ.6)IT2=(J-1)*NOU2+(B-1)*NOU+(A-1)*NO+I    !HPPH
         IF(ITYP.EQ.1) THEN
         I1A=(I-1)*NU+A
         I1B=(J-1)*NU+B
         ELSE
         I1A=(A-1)*NO+I
         I1B=(B-1)*NO+J
         END IF
         GOTO(1,2)IGOTO
 1       CONTINUE
         T2(IT2)=T2(IT2)+T1(I1A)*T1(I1B)
         GOTO 10
 2       CONTINUE
         T2(IT2)=T2(IT2)-T1(I1A)*T1(I1B)
 10   CONTINUE
      RETURN
      END
C*MODULE CCSDT   *DECK SYMV1
      SUBROUTINE SYMV1(A,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(N,N)
C
      DO 10 I=1,N
      DO 10 J=1,I
      Z=A(I,J)+A(J,I)
      A(I,J)=Z
      A(J,I)=Z
 10   CONTINUE
      RETURN
      END
C*MODULE CCSDT   *DECK VMCP
      SUBROUTINE VMCP(NO,NU,VM,VM1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER A
      DIMENSION VM(NO,NO,NO,NU),VM1(NO,NO,NU,NO)
C
      DO 10 I=1,NO
      DO 10 J=1,NO
      DO 10 K=1,NO
      DO 10 A=1,NU
      VM(I,J,K,A)=VM(I,J,K,A)+VM1(I,J,A,K)
 10   CONTINUE
      RETURN
      END
C*MODULE CCSDT   *DECK VMCP1
      SUBROUTINE VMCP1(NO,NU,VM,VM1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER A
      DIMENSION VM(NO,NU,NO,NO),VM1(NO,NO,NO,NU)
C
      DO 10 I=1,NO
      DO 10 J=1,NO
      DO 10 K=1,NO
      DO 10 A=1,NU
      VM(K,A,I,J)=VM1(I,J,K,A)
 10   CONTINUE
      RETURN
      END
C*MODULE CCSDT   *DECK VMCP2
      SUBROUTINE VMCP2(NO,NU,VM,VM1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER A
      DIMENSION VM(NO,NO,NU,NO),VM1(NO,NO,NO,NU)
C
      DO 10 I=1,NO
      DO 10 J=1,NO
      DO 10 K=1,NO
      DO 10 A=1,NU
      VM(I,J,A,K)=VM1(I,J,K,A)
 10   CONTINUE
      RETURN
C---      end
C---c
C---      SUBROUTINE CHKSUM(A,T,N)
C---      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C---      DIMENSION T(N)
C---      DATA ZERO/0.0d+00/
C---c
C---      X=ZERO
C---      Y=ZERO
C---      ZY=ZERO
C---      XT=ZERO
C---      DO 10 I=1,N
C---      X=X+ABS(T(I))
C---      XT=XT+T(I)
C--- 10   CONTINUE
C---      DO 20 I=2,N,2
C---      Z=T(I-1)+T(I)
C---      Y=Y+Z*Z
C--- 20   CONTINUE
C---      DO 30 I=5,N,5
C---      Z=T(I-4)+T(I-3)+T(I-2)+T(I-1)+T(I)
C---      ZY=ZY+Z*Z
C--- 30   CONTINUE
C--- 100  FORMAT(1X,A8,'ABS:',D15.10)
C---C      WRITE(6,100)A,X
C--- 200  FORMAT(A8,' ',D16.11,' ',D16.11,' ',D16.11,' ',D16.10)
C---      WRITE(6,200)A,X,Y,ZY,XT
C---      RETURN
      END
C
C         this was originally t3wt2.f
C
C*MODULE CCSDT   *DECK DRT3WT2
      SUBROUTINE DRT3WT2(NO,NU,EH,EP)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION EH(NO),EP(NU)
      COMMON /CCINFO/ TSH,NH,NP,MET,MEM,ICONV,MAXIT,IREST,IDISC
      COMMON /FMCOM/ X(1)
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
C
      NO2   = NO*NO
      NO3   = NO*NO2
      NU2   = NU*NU
      NU3   = NU*NU2
      NOU   = NO*NU
      NO3U  = NO3*NU
      NOU3  = NO*NU3
      NO2U2 = NO2*NU2
C
      CALL GOTFM(NGOTMX)
      IF(MEM.LT.NGOTMX) NGOTMX=MEM
C
      CALL VALFM(LOADFM)
      I1   = LOADFM + 1
      I2   = I1     + NOU
      I3   = I2     + NO2U2
      I4   = I3     + NO3U
      I5   = I4     + NOU3
      I6   = I5     + NU3
      I7   = I6     + NU3
      I8   = I7     + NO2U2
      LAST = I8     + NOU
      NEED = LAST - LOADFM - 1
      WRITE(6,90) NEED
      CALL FLSHBF(6)
C
      IF(NEED.GT.NGOTMX) THEN
         WRITE(6,91) NEED,NGOTMX
         CALL ABRT
      END IF
C
      CALL GETFM(NEED)
      IF(EXETYP.EQ.CHECK) GO TO 800
C
      CALL T3WT2(NH,NP,X(I1),X(I2),X(I3),X(I4),X(I5),X(I6),X(I7),X(I8),
     *           EH,EP)
C
  800 CONTINUE
      CALL RETFM(NEED)
      RETURN
C
 90   FORMAT(/1X,'MEMORY REQUIRED FOR NONITERATIVE TRIPLES (T3WT2  )',
     *           ' IS ',I12,' WORDS.')
 91   FORMAT(1X,'INSUFFICIENT MEMORY FOR NONITERATIVE TRIPLES'/
     *       1X,'REQUIRED:',I12,'     AVAILABLE:',I12)
      END
C*MODULE CCSDT   *DECK T3WT2
      SUBROUTINE T3WT2(NO,NU,T1,T2,VM,VE,V3,T3,VOE,O1,EH,EP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION T1(1),T2(NU,NU,NO,NO),VM(NO,NU,NO,NO),VOE(1),
     *          V3(1),T3(1),VE(NU,NU,NU,NO),EH(NO),EP(NU),O1(1)
      COMMON /CCRENO/ OSS,ODS,ODD,OTS,OTD,OTT,ODS_S,ODS_D,ODS_T,
     *                OQS,OQDS,OQDD,OQTS,ESD,ETD,ETS,ETTM,ESD_TM
      COMMON /CCINFO/ TSH,NH,NP,MET,MEM,ICONV,MAXIT,IREST,IDISC
      DATA ZERO/0.0D+00/, OM/-1.0D+00/, ONE/1.0D+00/
C
      NU2   = NU*NU
      NU3   = NU*NU2
      NOU   = NO*NU
C
      CALL ZEROMA(V3,1,NU3)
      OTS=ZERO
      OTD=ZERO
      ETD=ZERO
      ETTM=ZERO
      ESD_TM=ZERO
      CALL ZEROMA(T1,1,NOU)
      CALL RO2HPP(1,NO,NU,V3,VOE)
      CALL INSITU(NO,NU,NU,NO,V3,VOE,13)
      CALL TRANMD(VOE,NU,NU,NO,NO,12)
      CALL RDVEM4(0,NO,NU,V3,VE)
      CALL TRANMD(VE,NU,NU,NU,NO,23)
      CALL RDVEM4(1,NU,NO,V3,T2)
      CALL VMCP1(NO,NU,VM,T2)
      CALL TRANMD(VM,NO,NU,NO,NO,13)
      CALL RO2(1,NO,NU,T2)
      CALL INSITU(NO,NU,NU,NO,V3,T2,13)
      CALL RO1(1,NO,NU,O1)
C
      KK=0
      DO 351 I=1,NO
         I1=I-1
      DO 351 J=1,I1
         J1=J-1
      DO 351 K=1,J1
         KK=IT3(I,J,K)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,VM(1,1,K,J),NO,
     *ZERO,V3,NU2)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,VM(1,1,J,K),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,4)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,VM(1,1,K,I),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,VM(1,1,I,K),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,5)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,K),NU2,VM(1,1,J,I),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,K),NU2,VM(1,1,I,J),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,4)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,I),NU,VE(1,1,1,K),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,K,I),NU,VE(1,1,1,J),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,5)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,J),NU,VE(1,1,1,K),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,K,J),NU,VE(1,1,1,I),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,4)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,K),NU,VE(1,1,1,J),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,K),NU,VE(1,1,1,I),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,5)
      CALL ZEROT3(V3,NU)
      IF(IDISC.EQ.0.AND.MET.GT.4) THEN
      CALL WRT3(KK,NU,V3)
      END IF
      CALL T3SQUA(I,J,K,NO,NU,O1,T2,V3,EH,EP)
      DEH=EH(I)+EH(J)+EH(K)
      CALL ADT3DEN(NU,DEH,V3,EP)
      ITMP=I
      JTMP=J
      KTMP=K
      CALL DRT1WT3IJK(ITMP,JTMP,KTMP,NO,NU,T1,VOE,V3,T3)
 351  CONTINUE
C
      DO 352 I=1,NO
         I1=I-1
      DO 352 J=1,I1
      KK=IT3(I,J,J)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,VM(1,1,J,J),NO,
     *ZERO,V3,NU2)
      CALL TRANT3(V3,NU,2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,VM(1,1,J,I),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,VM(1,1,I,J),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,4)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,I),NU,VE(1,1,1,J),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,3)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,J),NU,VE(1,1,1,J),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,J),NU,VE(1,1,1,I),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,2)
      CALL SYMT311(V3,NU,23)
      CALL ZEROT3(V3,NU)
      IF(IDISC.EQ.0.AND.MET.GT.4) THEN
      CALL WRT3(KK,NU,V3)
      END IF
      CALL T3SQUA(I,J,J,NO,NU,O1,T2,V3,EH,EP)
      DEH=EH(I)+EH(J)+EH(J)
      CALL ADT3DEN(NU,DEH,V3,EP)
      ITMP=I
      JTMP=J
      CALL DRT1WT3IJ(ITMP,JTMP,NO,NU,T1,VOE,V3,T3)
 352  CONTINUE
C
      DO 353 I=1,NO
         I1=I-1
      DO 353 J=1,I1
      KK=IT3(I,I,J)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,VM(1,1,J,I),NO,
     *ZERO,V3,NU2)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,VM(1,1,I,J),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,VM(1,1,I,I),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,2)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,I),NU,VE(1,1,1,J),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,I),NU,VE(1,1,1,I),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,3)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,J),NU,VE(1,1,1,I),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,3)
      CALL SYMT311(V3,NU,12)
      CALL ZEROT3(V3,NU)
      IF(IDISC.EQ.0.AND.MET.GT.4) THEN
      CALL WRT3(KK,NU,V3)
      END IF
      CALL T3SQUA(I,I,J,NO,NU,O1,T2,V3,EH,EP)
      DEH=EH(I)+EH(I)+EH(J)
      CALL ADT3DEN(NU,DEH,V3,EP)
      ITMP=I
      JTMP=J
      CALL DRT1WT3JK(ITMP,JTMP,NO,NU,T1,VOE,V3,T3)
 353  CONTINUE
      CALL TRT1(NU,NO,T3,T1)
      CALL T1SQ(NO,NU,T3,T1)
      CALL ADDDEN1(NO,NU,T1,EH,EP)
      CALL WO1(4,NO,NU,T1)
      RETURN
      END
C*MODULE CCSDT   *DECK DRT3WT2N
      SUBROUTINE DRT3WT2N(NH,NP,EH,EP,IDISC)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION EH(NH),EP(NP)
      COMMON /FMCOM/ X(1)
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
C
      NO    = NH
      NU    = NP
      NO2   = NO*NO
      NO3   = NO*NO2
      NU2   = NU*NU
      NU3   = NU*NU2
      NOU   = NO*NU
      NO3U  = NO3*NU
      NOU3  = NO*NU3
      NO2U2 = NO2*NU2
C
      CALL GOTFM(NGOTMX)
      IF(IDISC.EQ.0) THEN
C
      CALL VALFM(LOADFM)
      I1   = LOADFM + 1
      I2   = I1     + NOU
      I3   = I2     + NO2U2
      I4   = I3     + NO3U
      I5   = I4     + NOU3
      I6   = I5     + NU3
      I7   = I6     + NU3
      LAST = I7     + NO2U2
      NEED = LAST - LOADFM - 1
      WRITE(6,90) NEED
      CALL FLSHBF(6)
C
      IF(NEED.GT.NGOTMX) THEN
         WRITE(6,91) NEED,NGOTMX
         CALL ABRT
      END IF
C
      CALL GETFM(NEED)
      IF(EXETYP.EQ.CHECK) GO TO 800
C
      CALL T3WT2N(NH,NP,X(I1),X(I2),X(I3),X(I4),X(I5),X(I6),X(I7),
     *            EH,EP,IDISC)
C
      ELSE
C
      CALL VALFM(LOADFM)
      I1   = LOADFM + 1
      I2   = I1     + NOU
      I3   = I2     + NO2U2
      I4   = I3     + NO3U
      I5   = I4     + NOU3
      I6   = I5     + NU3
      I7   = I6     + NU3
      I8   = I7     + NO2U2
      I9   = I8     + NO3U
      LAST = I9     + NOU3
      NEED = LAST - LOADFM - 1
      WRITE(6,90) NEED
      CALL FLSHBF(6)
C
      IF(NEED.GT.NGOTMX) THEN
         WRITE(6,91) NEED,NGOTMX
         CALL ABRT
      END IF
C
      CALL GETFM(NEED)
      IF(EXETYP.EQ.CHECK) GO TO 800
C
      CALL T3WT2NN(NH,NP,X(I1),X(I2),X(I3),X(I4),X(I5),X(I6),X(I7),
     *             X(I8),X(I9),EH,EP)
C
      END IF
C
  800 CONTINUE
      CALL RETFM(NEED)
      RETURN
C
 90   FORMAT(1X,'MEMORY REQUIRED FOR NONITERATIVE TRIPLES (T3WT2N )',
     *           ' IS ',I12,' WORDS.')
 91   FORMAT(1X,'INSUFFICIENT MEMORY FOR NONITERATIVE TRIPLES'/
     *       1X,'REQUIRED:',I12,'     AVAILABLE:',I12)
      END
C*MODULE CCSDT   *DECK T3WT2N
      SUBROUTINE T3WT2N(NO,NU,O1,T2,VM,VE,V3,T3,VOE,EH,EP,IDISC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION O1(1),T2(NU,NU,NO,NO),VM(NO,NU,NO,NO),VOE(1),V3(1),
     *          T3(1),VE(NU,NU,NU,NO),EH(NO),EP(NU)
      DATA ZERO/0.0D+00/, ONE/1.0D+00/, OM/-1.0D+00/
C
      NU2   = NU*NU
      NU3   = NU*NU2
C
      CALL ZEROMA(V3,1,NU3)
      CALL RO2HPP(1,NO,NU,V3,VOE)
      CALL INSITU(NO,NU,NU,NO,V3,VOE,13)
      CALL TRANMD(VOE,NU,NU,NO,NO,12)
      CALL RDVE(3,NO,NU,VE)
      CALL TRANMD(VE,NU,NU,NU,NO,23)
      CALL RDVM(3,NO,NU,T2)
      CALL VMCP1(NO,NU,VM,T2)
      CALL TRANMD(VM,NO,NU,NO,NO,13)
      CALL RO2(1,NO,NU,T2)
      CALL INSITU(NO,NU,NU,NO,V3,T2,13)
      CALL RO1(1,NO,NU,O1)
C
      DO 351 I=1,NO
         I1=I-1
      DO 351 J=1,I1
         J1=J-1
      DO 351 K=1,J1
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,VM(1,1,K,J),NO,
     *ZERO,V3,NU2)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,VM(1,1,J,K),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,4)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,VM(1,1,K,I),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,VM(1,1,I,K),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,5)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,K),NU2,VM(1,1,J,I),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,K),NU2,VM(1,1,I,J),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,4)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,I),NU,VE(1,1,1,K),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,K,I),NU,VE(1,1,1,J),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,5)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,J),NU,VE(1,1,1,K),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,K,J),NU,VE(1,1,1,I),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,4)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,K),NU,VE(1,1,1,J),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,K),NU,VE(1,1,1,I),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,5)
      CALL ZEROT3(V3,NU)
      CALL T3SQUAN(I,J,K,NO,NU,O1,VOE,V3,T3,EH,EP,IDISC)
 351  CONTINUE
      DO 352 I=1,NO
         I1=I-1
      DO 352 J=1,I1
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,VM(1,1,J,J),NO,
     *ZERO,V3,NU2)
      CALL TRANT3(V3,NU,2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,VM(1,1,J,I),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,VM(1,1,I,J),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,4)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,I),NU,VE(1,1,1,J),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,3)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,J),NU,VE(1,1,1,J),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,J),NU,VE(1,1,1,I),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,2)
      CALL SYMT311(V3,NU,23)
      CALL ZEROT3(V3,NU)
      CALL T3SQUAN(I,J,J,NO,NU,O1,VOE,V3,T3,EH,EP,IDISC)
 352  CONTINUE
      DO 353 I=1,NO
         I1=I-1
      DO 353 J=1,I1
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,VM(1,1,J,I),NO,
     *ZERO,V3,NU2)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,VM(1,1,I,J),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,VM(1,1,I,I),NO,
     *ONE,V3,NU2)
      CALL TRANT3(V3,NU,2)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,I),NU,VE(1,1,1,J),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,1)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,I),NU,VE(1,1,1,I),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,3)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,J),NU,VE(1,1,1,I),NU,
     *ONE,V3,NU)
      CALL TRANT3(V3,NU,3)
      CALL SYMT311(V3,NU,12)
      CALL ZEROT3(V3,NU)
      CALL T3SQUAN(I,I,J,NO,NU,O1,VOE,V3,T3,EH,EP,IDISC)
 353  CONTINUE
      RETURN
      END
C*MODULE CCSDT   *DECK T3WT2NN
      SUBROUTINE T3WT2NN(NO,NU,O1,T2,VM,VE,V3,T3,VOE,AVM,AVE,EH,EP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION O1(1),T2(NU,NU,NO,NO),VM(NO,NU,NO,NO),VOE(1),V3(1),
     *          T3(1),VE(NU,NU,NU,NO),EH(NO),EP(NU),
     *          AVM(NO,NU,NO,NO),AVE(NU,NU,NU,NO)
      COMMON /CCRENO/ OSS,ODS,ODD,OTS,OTD,OTT,ODS_S,ODS_D,ODS_T,
     *                OQS,OQDS,OQDD,OQTS,ESD,ETD,ETS,ETTM,ESD_TM
      DATA ZERO/0.0D+00/, ONE/1.0D+00/, OM/-1.0D+00/
C
      NU2   = NU*NU
      NU3   = NU*NU2
      ETD=ZERO
      ETS=ZERO
      OTS=ZERO
      OTD=ZERO
      ETTM=ZERO
      ESD_TM=ZERO
C
      CALL ZEROMA(V3,1,NU3)
      CALL RO2HPP(1,NO,NU,V3,VOE)
      CALL INSITU(NO,NU,NU,NO,V3,VOE,13)
      CALL TRANMD(VOE,NU,NU,NO,NO,12)
C
      CALL RDVE(3,NO,NU,VE)
      CALL TRANMD(VE,NU,NU,NU,NO,23)
      CALL RDVM(3,NO,NU,T2)
      CALL VMCP1(NO,NU,VM,T2)
      CALL TRANMD(VM,NO,NU,NO,NO,13)
C
      CALL RDVEM4(0,NO,NU,V3,AVE)
      CALL TRANMD(AVE,NU,NU,NU,NO,23)
      CALL RDVEM4(1,NU,NO,V3,T2)
      CALL VMCP1(NO,NU,AVM,T2)
      CALL TRANMD(AVM,NO,NU,NO,NO,13)
C
      CALL RO2(1,NO,NU,T2)
      CALL INSITU(NO,NU,NU,NO,V3,T2,13)
      CALL RO1(1,NO,NU,O1)
C
      DO 351 I=1,NO
         I1=I-1
      DO 351 J=1,I1
         J1=J-1
      DO 351 K=1,J1
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,VM(1,1,K,J),NO,
     *ZERO,V3,NU2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,AVM(1,1,K,J),NO,
     *ZERO,T3,NU2)
      CALL TRANT3(V3,NU,1)
      CALL TRANT3(T3,NU,1)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,VM(1,1,J,K),NO,
     *ONE,V3,NU2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,AVM(1,1,J,K),NO,
     *ONE,T3,NU2)
      CALL TRANT3(V3,NU,4)
      CALL TRANT3(T3,NU,4)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,VM(1,1,K,I),NO,
     *ONE,V3,NU2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,AVM(1,1,K,I),NO,
     *ONE,T3,NU2)
      CALL TRANT3(V3,NU,1)
      CALL TRANT3(T3,NU,1)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,VM(1,1,I,K),NO,
     *ONE,V3,NU2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,AVM(1,1,I,K),NO,
     *ONE,T3,NU2)
      CALL TRANT3(V3,NU,5)
      CALL TRANT3(T3,NU,5)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,K),NU2,VM(1,1,J,I),NO,
     *ONE,V3,NU2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,K),NU2,AVM(1,1,J,I),NO,
     *ONE,T3,NU2)
      CALL TRANT3(V3,NU,1)
      CALL TRANT3(T3,NU,1)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,K),NU2,VM(1,1,I,J),NO,
     *ONE,V3,NU2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,K),NU2,AVM(1,1,I,J),NO,
     *ONE,T3,NU2)
      CALL TRANT3(V3,NU,4)
      CALL TRANT3(T3,NU,4)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,I),NU,VE(1,1,1,K),NU,
     *ONE,V3,NU)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,I),NU,AVE(1,1,1,K),NU,
     *ONE,T3,NU)
      CALL TRANT3(V3,NU,1)
      CALL TRANT3(T3,NU,1)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,K,I),NU,VE(1,1,1,J),NU,
     *ONE,V3,NU)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,K,I),NU,AVE(1,1,1,J),NU,
     *ONE,T3,NU)
      CALL TRANT3(V3,NU,5)
      CALL TRANT3(T3,NU,5)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,J),NU,VE(1,1,1,K),NU,
     *ONE,V3,NU)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,J),NU,AVE(1,1,1,K),NU,
     *ONE,T3,NU)
      CALL TRANT3(V3,NU,1)
      CALL TRANT3(T3,NU,1)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,K,J),NU,VE(1,1,1,I),NU,
     *ONE,V3,NU)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,K,J),NU,AVE(1,1,1,I),NU,
     *ONE,T3,NU)
      CALL TRANT3(V3,NU,4)
      CALL TRANT3(T3,NU,4)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,K),NU,VE(1,1,1,J),NU,
     *ONE,V3,NU)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,K),NU,AVE(1,1,1,J),NU,
     *ONE,T3,NU)
      CALL TRANT3(V3,NU,1)
      CALL TRANT3(T3,NU,1)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,K),NU,VE(1,1,1,I),NU,
     *ONE,V3,NU)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,K),NU,AVE(1,1,1,I),NU,
     *ONE,T3,NU)
      CALL TRANT3(V3,NU,5)
      CALL TRANT3(T3,NU,5)
      CALL ZEROT3(V3,NU)
      CALL ZEROT3(T3,NU)
      CALL T3SQUANN(I,J,K,NO,NU,O1,VOE,V3,T3,T2,EH,EP)
 351  CONTINUE
      DO 352 I=1,NO
         I1=I-1
      DO 352 J=1,I1
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,VM(1,1,J,J),NO,
     *ZERO,V3,NU2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,AVM(1,1,J,J),NO,
     *ZERO,T3,NU2)
      CALL TRANT3(V3,NU,2)
      CALL TRANT3(T3,NU,2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,VM(1,1,J,I),NO,
     *ONE,V3,NU2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,AVM(1,1,J,I),NO,
     *ONE,T3,NU2)
      CALL TRANT3(V3,NU,1)
      CALL TRANT3(T3,NU,1)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,VM(1,1,I,J),NO,
     *ONE,V3,NU2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,AVM(1,1,I,J),NO,
     *ONE,T3,NU2)
      CALL TRANT3(V3,NU,4)
      CALL TRANT3(T3,NU,4)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,I),NU,VE(1,1,1,J),NU,
     *ONE,V3,NU)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,I),NU,AVE(1,1,1,J),NU,
     *ONE,T3,NU)
      CALL TRANT3(V3,NU,3)
      CALL TRANT3(T3,NU,3)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,J),NU,VE(1,1,1,J),NU,
     *ONE,V3,NU)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,J),NU,AVE(1,1,1,J),NU,
     *ONE,T3,NU)
      CALL TRANT3(V3,NU,1)
      CALL TRANT3(T3,NU,1)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,J),NU,VE(1,1,1,I),NU,
     *ONE,V3,NU)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,J),NU,AVE(1,1,1,I),NU,
     *ONE,T3,NU)
      CALL TRANT3(V3,NU,2)
      CALL TRANT3(T3,NU,2)
      CALL SYMT311(V3,NU,23)
      CALL SYMT311(T3,NU,23)
      CALL ZEROT3(V3,NU)
      CALL ZEROT3(T3,NU)
      CALL T3SQUANN(I,J,J,NO,NU,O1,VOE,V3,T3,T2,EH,EP)
 352  CONTINUE
      DO 353 I=1,NO
         I1=I-1
      DO 353 J=1,I1
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,VM(1,1,J,I),NO,
     *ZERO,V3,NU2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,AVM(1,1,J,I),NO,
     *ZERO,T3,NU2)
      CALL TRANT3(V3,NU,1)
      CALL TRANT3(T3,NU,1)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,VM(1,1,I,J),NO,
     *ONE,V3,NU2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,I),NU2,AVM(1,1,I,J),NO,
     *ONE,T3,NU2)
      CALL TRANT3(V3,NU,2)
      CALL TRANT3(T3,NU,2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,VM(1,1,I,I),NO,
     *ONE,V3,NU2)
      CALL DGEMM('N','N',NU2,NU,NO,OM,T2(1,1,1,J),NU2,AVM(1,1,I,I),NO,
     *ONE,T3,NU2)
      CALL TRANT3(V3,NU,2)
      CALL TRANT3(T3,NU,2)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,I),NU,VE(1,1,1,J),NU,
     *ONE,V3,NU)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,I),NU,AVE(1,1,1,J),NU,
     *ONE,T3,NU)
      CALL TRANT3(V3,NU,1)
      CALL TRANT3(T3,NU,1)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,I),NU,VE(1,1,1,I),NU,
     *ONE,V3,NU)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,J,I),NU,AVE(1,1,1,I),NU,
     *ONE,T3,NU)
      CALL TRANT3(V3,NU,3)
      CALL TRANT3(T3,NU,3)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,J),NU,VE(1,1,1,I),NU,
     *ONE,V3,NU)
      CALL DGEMM('N','N',NU,NU2,NU,ONE,T2(1,1,I,J),NU,AVE(1,1,1,I),NU,
     *ONE,T3,NU)
      CALL TRANT3(V3,NU,3)
      CALL TRANT3(T3,NU,3)
      CALL SYMT311(V3,NU,12)
      CALL SYMT311(T3,NU,12)
      CALL ZEROT3(V3,NU)
      CALL ZEROT3(T3,NU)
      CALL T3SQUANN(I,I,J,NO,NU,O1,VOE,V3,T3,T2,EH,EP)
 353  CONTINUE
      RETURN
      END
C*MODULE CCSDT   *DECK ADT3DEN
      SUBROUTINE ADT3DEN(NU,DEH,T3,EP)
      IMPLICIT DOUBLE PRECISION  (A-H,O-Z)
      INTEGER A,B,C
      DIMENSION T3(NU,NU,NU),EP(NU)
C
      DO 10 A=1,NU
      DO 10 B=1,NU
      DO 10 C=1,NU
      DEN=DEH-EP(A)-EP(B)-EP(C)
      T3(A,B,C)=T3(A,B,C)/DEN
 10   CONTINUE
      RETURN
      END
C*MODULE CCSDT   *DECK TRANT3
      SUBROUTINE TRANT3(V,NU,ID)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER A,B,C,D
      DIMENSION V(NU,NU,NU)
C
      GO TO (1,2,3,4,5),ID
    1 CONTINUE
      DO 100 B=1,NU
      DO 100 C=1,B
      DO 100 A=1,NU
      X=V(A,B,C)
      V(A,B,C)=V(A,C,B)
      V(A,C,B)=X
  100 CONTINUE
      GO TO 1000
    2 CONTINUE
      DO 200 C=1,NU
      DO 200 A=1,NU
      DO 200 B=1,A
      X=V(A,B,C)
      V(A,B,C)=V(B,A,C)
      V(B,A,C)=X
  200 CONTINUE
      GO TO 1000
    3 CONTINUE
      DO 300 B=1,NU
      DO 300 A=1,NU
      DO 300 C=1,A
      X=V(A,B,C)
      V(A,B,C)=V(C,B,A)
      V(C,B,A)=X
  300 CONTINUE
      GO TO 1000
    4 CONTINUE
      DO 400 B=1,NU
      DO 400 C=1,B
      DO 400 A=1,C
      X=V(A,B,C)
      V(A,B,C)=V(B,C,A)
      V(B,C,A)=V(C,A,B)
      V(C,A,B)=X
      IF(B.EQ.C.OR.C.EQ.A)GO TO 400
      X=V(B,A,C)
      V(B,A,C)=V(A,C,B)
      V(A,C,B)=V(C,B,A)
      V(C,B,A)=X
  400 CONTINUE
      GO TO 1000
    5 CONTINUE
      DO 500 A=1,NU
      DO 500 C=1,A
      DO 500 D=1,C
      X=V(C,D,A)
      V(C,D,A)=V(A,C,D)
      V(A,C,D)=V(D,A,C)
      V(D,A,C)=X
      IF (A.EQ.C.OR.C.EQ.D)GO TO 500
      X=V(A,D,C)
      V(A,D,C)=V(C,A,D)
      V(C,A,D)=V(D,C,A)
      V(D,C,A)=X
  500 CONTINUE
      GO TO 1000
 1000 CONTINUE
      RETURN
      END
C*MODULE CCSDT   *DECK T3SQUA
      SUBROUTINE T3SQUA(I,J,K,NO,NU,T1,T2,T3,EH,EP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL IEJ,JEK
      INTEGER A,B,C
      DIMENSION T1(NO,NU),T2(NU,NU,NO,NO),T3(NU,NU,NU),EH(NO),EP(NU)
      COMMON /CCRENO/ OSS,ODS,ODD,OTS,OTD,OTT,ODS_S,ODS_D,ODS_T,
     *                OQS,OQDS,OQDD,OQTS,ESD,ETD,ETS,ETTM,ESD_TM
      DATA TWO/2.0D+00/,FOUR/4.0D+00/,EIGHT/8.0D+00/,ZERO/0.0D+00/,
     *     HALF/0.5D+00/,ONE/1.0D+00/
C
      DIJK=EH(I)+EH(J)+EH(K)
      X1=ZERO
      X2=ZERO
      X3=ZERO
      DO 150 A=1,NU
      DO 150 B=1,NU
      DO 150 C=1,NU
      IF (A.EQ.B.AND.B.EQ.C)GOTO 150
      DABC=EP(A)+EP(B)+EP(C)
      DENOM=DIJK-DABC
      XT111=T1(I,A)*T1(J,B)*T1(K,C)
      XT21=T1(I,A)*T2(B,C,K,J)+T1(J,B)*T2(A,C,K,I)+T1(K,C)*T2(A,B,J,I)
      D1=  T3(A,B,C)
      D2=  T3(A,C,B)+T3(C,B,A)+T3(B,A,C)
      D3=  T3(B,C,A)+T3(C,A,B)
      F=D1*EIGHT-FOUR*D2+D3*TWO
      X1=X1+F*XT111/DENOM
      X2=X2+F*XT21/DENOM
      X3=X3+F*D1/DENOM
 150  CONTINUE
      CF=ONE
      IEJ=I.EQ.J
      JEK=J.EQ.K
      IF(IEJ.OR.JEK) CF=HALF
      OTS=OTS+CF*X1
      OTD=OTD+CF*X2
      ETD=ETD+CF*X3
      RETURN
      END
C*MODULE CCSDT   *DECK DRT3SQTOT
      SUBROUTINE DRT3SQTOT(NO,NU,EH,EP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /FMCOM/ X(1)
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
C
      NO2   = NO*NO
      NU2   = NU*NU
      NU3   = NU*NU2
      NOU   = NO*NU
      NO2U2 = NO2*NU2
C
      CALL GOTFM(NGOTMX)
C
      CALL VALFM(LOADFM)
      I1   = LOADFM + 1
      I2   = I1     + NOU
      I3   = I2     + NO2U2
      I4   = I3     + NO2U2
      LAST = I4     + NU3
      NEED = LAST - LOADFM - 1
      WRITE(6,90) NEED
      CALL FLSHBF(6)
C
      IF(NEED.GT.NGOTMX) THEN
         WRITE(6,91) NEED,NGOTMX
         CALL ABRT
      END IF
C
      CALL GETFM(NEED)
      IF(EXETYP.EQ.CHECK) GO TO 800
C
      CALL T3SQUATOT(NO,NU,X(I1),X(I2),X(I3),X(I4),EH,EP)
C
  800 CONTINUE
      CALL RETFM(NEED)
      RETURN
C
 90   FORMAT(1X,'MEMORY REQUIRED FOR NONITERATIVE TRIPLES (T3SQTOT)',
     *           ' IS ',I12,' WORDS.')
 91   FORMAT(1X,'INSUFFICIENT MEMORY FOR NONITERATIVE TRIPLES'/
     *       1X,'REQUIRED:',I12,'     AVAILABLE:',I12)
      END
C*MODULE CCSDT   *DECK T3SQUATOT
      SUBROUTINE T3SQUATOT(NO,NU,T1,T2,VO,T3,EH,EP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL IEJ,JEK
      INTEGER A,B,C
      DIMENSION T1(NO,NU),T2(NO,NU,NU,NO),T3(NU,NU,NU),EH(NO),EP(NU),
     *          VO(NO,NU,NU,NO)
      COMMON /CCFILE/ INTG,NT1,NT2,NT3,NVM,NVE,NFRLE,NRESF,NRESL
      COMMON /CCRENO/ OSS,ODS,ODD,OTS,OTD,OTT,ODS_S,ODS_D,ODS_T,
     *                OQS,OQDS,OQDD,OQTS,ESD,ETD,ETS,ETTM,ESD_TM
      DATA TWO/2.0D+00/,FOUR/4.0D+00/,EIGHT/8.0D+00/,ZERO/0.0D+00/,
     *     HALF/0.5D+00/,ONE/1.0D+00/
C
      READ(NT1,REC=1)T1
      ODS_S=ZERO
      ODS_D=ZERO
      CALL RO2HPP(1,NO,NU,T3,VO)
      CALL RO2(1,NO,NU,T2)
C      call ro2hpp(0,no,nu,t3,t2)
      DO 1 I=1,NO
      DO 1 J=1,I
      DO 1 K=1,J
      DIJK=EH(I)+EH(J)+EH(K)
      IF(I.EQ.K)GOTO 1
      DO 2 A=1,NU
      DO 2 B=1,NU
      DO 2 C=1,NU
      IF(A.EQ.B.AND.B.EQ.C)GOTO 2
      T3(A,B,C)=T1(I,A)*VO(J,B,C,K)+T1(J,B)*VO(I,A,C,K)
     *         +T1(K,C)*VO(I,A,B,J)
 2    CONTINUE
      X1=ZERO
      X2=ZERO
      DO 150 A=1,NU
      DO 150 B=1,NU
      DO 150 C=1,NU
      IF (A.EQ.B.AND.B.EQ.C)GOTO 150
      DABC=EP(A)+EP(B)+EP(C)
      DENOM=DIJK-DABC
      XT111=T1(I,A)*T1(J,B)*T1(K,C)
      XT21=T1(I,A)*T2(J,B,C,K)+T1(J,B)*T2(I,A,C,K)+T1(K,C)*T2(I,A,B,J)
      D1=  T3(A,B,C)
      D2=  T3(A,C,B)+T3(C,B,A)+T3(B,A,C)
      D3=  T3(B,C,A)+T3(C,A,B)
      F=D1*EIGHT-FOUR*D2+D3*TWO
      X1=X1+F*XT111/DENOM
      X2=X2+F*XT21/DENOM
 150  CONTINUE
      CF=ONE
      IEJ=I.EQ.J
      JEK=J.EQ.K
      IF(IEJ.OR.JEK) CF=HALF
      ODS_S=ODS_S+CF*X1
      ODS_D=ODS_D+CF*X2
 1    CONTINUE
      RETURN
      END
C*MODULE CCSDT   *DECK IT3
      INTEGER FUNCTION IT3(I,J,K)
      IMPLICIT DOUBLE PRECISION  (A-H,O-Z)
C
      I2=(I-1)*(I-2)
C      IT3=I2+I2*(I-3)/6+J*(J-1)/2+K+i-1
      IT3=I2+I2*(I-3)/6+J*(J-1)/2+K
      RETURN
      END
C*MODULE CCSDT   *DECK SYMT311
      SUBROUTINE SYMT311(V,NU,ID)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER A,B,C
      DIMENSION V(NU,NU,NU)
C
      IF (ID.EQ.23)GOTO 23
      IF (ID.EQ.12)GOTO 12
      IF (ID.EQ.13)GOTO 13
 23   CONTINUE
      DO 100 A=1,NU
      DO 100 B=1,NU
      DO 100 C=1,B
      X=V(A,B,C)+V(A,C,B)
      V(A,B,C)=X
      V(A,C,B)=X
  100 CONTINUE
      GO TO 1000
 12   CONTINUE
      DO 101 A=1,NU
      DO 101 B=1,A
      DO 101 C=1,NU
      X=V(A,B,C)+V(B,A,C)
      V(A,B,C)=X
      V(B,A,C)=X
 101  CONTINUE
      GO TO 1000
 13   CONTINUE
      DO 102 A=1,NU
      DO 102 B=1,NU
      DO 102 C=1,A
      X=V(A,B,C)+V(C,B,A)
      V(A,B,C)=X
      V(C,B,A)=X
 102  CONTINUE
      GO TO 1000
 1000 CONTINUE
      END
C*MODULE CCSDT   *DECK T1WT3IJK
      SUBROUTINE T1WT3IJK(I,J,K,NO,NU,T1,VOE,TI,T3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION T1(NU,NO),VOE(NU,NU,NO,NO),T3(1),TI(1)
      DATA ONE/1.0D+00/
C
      NU2 = NU*NU
      CALL SMT3FOUR(NU,T3,TI)
      CALL DGEMM('N','N',NU,1,NU2,ONE,T3,NU,VOE(1,1,J,K),NU2,ONE,
     *           T1(1,I),NU)
      RETURN
      END
C*MODULE CCSDT   *DECK DRT1WT3IJ
      SUBROUTINE DRT1WT3IJ(I,J,NO,NU,T1,VOE,TI,T3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION T1(1),VOE(1),T3(1),TI(1)
C
      CALL T1WT3IJK(I,J,J,NO,NU,T1,VOE,TI,T3)
      CALL TRANT3(TI,NU,2)
      CALL T1WT3IJK(J,I,J,NO,NU,T1,VOE,TI,T3)
      CALL TRANT3(TI,NU,1)
      CALL T1WT3IJK(J,J,I,NO,NU,T1,VOE,TI,T3)
      RETURN
      END
C*MODULE CCSDT   *DECK DRT1WT3JK
      SUBROUTINE DRT1WT3JK(J,K,NO,NU,T1,VOE,TI,T3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION T1(1),VOE(1),T3(1),TI(1)
C
      CALL T1WT3IJK(J,J,K,NO,NU,T1,VOE,TI,T3)
      CALL TRANT3(TI,NU,1)
      CALL T1WT3IJK(J,K,J,NO,NU,T1,VOE,TI,T3)
      CALL TRANT3(TI,NU,2)
      CALL T1WT3IJK(K,J,J,NO,NU,T1,VOE,TI,T3)
      RETURN
      END
C*MODULE CCSDT   *DECK DRT1WT3IJK
      SUBROUTINE DRT1WT3IJK(I,J,K,NO,NU,T1,VOE,TI,T3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION T1(1),VOE(1),T3(1),TI(1)
C
      CALL T1WT3IJK(I,J,K,NO,NU,T1,VOE,TI,T3)
      CALL TRANT3(TI,NU,1)
      CALL T1WT3IJK(I,K,J,NO,NU,T1,VOE,TI,T3)
      CALL TRANT3(TI,NU,4)
      CALL T1WT3IJK(J,I,K,NO,NU,T1,VOE,TI,T3)
      CALL TRANT3(TI,NU,1)
      CALL T1WT3IJK(J,K,I,NO,NU,T1,VOE,TI,T3)
      CALL TRANT3(TI,NU,5)
      CALL T1WT3IJK(K,I,J,NO,NU,T1,VOE,TI,T3)
      CALL TRANT3(TI,NU,1)
      CALL T1WT3IJK(K,J,I,NO,NU,T1,VOE,TI,T3)
      RETURN
      END
C*MODULE CCSDT   *DECK SMT3FOUR
      SUBROUTINE SMT3FOUR(NU,T3,V3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER A,B,C
      DIMENSION T3(NU,NU,NU),V3(NU,NU,NU)
      DATA TWO/2.0D+00/
C
      DO 1 A=1,NU
      DO 1 B=1,NU
      DO 1 C=1,NU
         T3(A,B,C)=(V3(A,B,C)-V3(B,A,C))*TWO-V3(A,C,B)+V3(B,C,A)
 1    CONTINUE
      RETURN
      END
C*MODULE CCSDT   *DECK T3SQUAN
      SUBROUTINE T3SQUAN(I,J,K,NO,NU,T1,T2,T3,O3,EH,EP,IDISC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL IEJ,JEK
      INTEGER A,B,C
      DIMENSION T1(NO,NU),T2(NU,NU,NO,NO),T3(NU,NU,NU),EH(NO),EP(NU),
     *          O3(NU,NU,NU)
      COMMON /CCRENO/ OSS,ODS,ODD,OTS,OTD,OTT,ODS_S,ODS_D,ODS_T,
     *                OQS,OQDS,OQDD,OQTS,ESD,ETD,ETS,ETTM,ESD_TM
      DATA TWO/2.0D+00/,FOUR/4.0D+00/,EIGHT/8.0D+00/,ZERO/0.0D+00/,
     *     HALF/0.5D+00/,ONE/1.0D+00/
C
      DIJK=EH(I)+EH(J)+EH(K)
      KK=IT3(I,J,K)
      IF(IDISC.EQ.0) THEN
      CALL RDT3(KK,NU,O3)
      END IF
      X1=ZERO
      X2=ZERO
      DO 150 A=1,NU
      DO 150 B=1,NU
      DO 150 C=1,NU
      IF (A.EQ.B.AND.B.EQ.C)GOTO 150
      DABC=EP(A)+EP(B)+EP(C)
      DENOM=DIJK-DABC
      XT21=T1(I,A)*T2(B,C,J,K)+T1(J,B)*T2(A,C,I,K)+T1(K,C)*T2(A,B,I,J)
      D1=  T3(A,B,C)
      D2=  T3(A,C,B)+T3(C,B,A)+T3(B,A,C)
      D3=  T3(B,C,A)+T3(C,A,B)
      F=D1*EIGHT-FOUR*D2+D3*TWO
      X1=X1+F*XT21/DENOM
      X2=X2+F*O3(A,B,C)/DENOM
 150  CONTINUE
      CF=ONE
      IEJ=I.EQ.J
      JEK=J.EQ.K
      IF(IEJ.OR.JEK) CF=HALF
      ESD_TM=ESD_TM+CF*X1
      ETTM=ETTM+CF*X2
      RETURN
      END
C*MODULE CCSDT   *DECK T3SQUANN
      SUBROUTINE T3SQUANN(I,J,K,NO,NU,T1,T2,T3,O3,O2,EH,EP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL IEJ,JEK
      INTEGER A,B,C
      DIMENSION T1(NO,NU),T2(NU,NU,NO,NO),T3(NU,NU,NU),EH(NO),EP(NU),
     *          O3(NU,NU,NU),O2(NU,NU,NO,NO)
      COMMON /CCRENO/ OSS,ODS,ODD,OTS,OTD,OTT,ODS_S,ODS_D,ODS_T,
     *                OQS,OQDS,OQDD,OQTS,ESD,ETD,ETS,ETTM,ESD_TM
      DATA TWO/2.0D+00/,FOUR/4.0D+00/,EIGHT/8.0D+00/,ZERO/0.0D+00/,
     *     HALF/0.5D+00/,ONE/1.0D+00/
C
      DIJK=EH(I)+EH(J)+EH(K)
      X1=ZERO
      X2=ZERO
      Y1=ZERO
      Y2=ZERO
      Y3=ZERO
      Y4=ZERO
      DO 150 A=1,NU
      DO 150 B=1,NU
      DO 150 C=1,NU
      IF (A.EQ.B.AND.B.EQ.C)GOTO 150
      DABC=EP(A)+EP(B)+EP(C)
      DENOM=DIJK-DABC
      XT21=T1(I,A)*T2(B,C,J,K)+T1(J,B)*T2(A,C,I,K)+T1(K,C)*T2(A,B,I,J)
      XT111=T1(I,A)*T1(J,B)*T1(K,C)
      XTO=T1(I,A)*O2(C,B,J,K)+T1(J,B)*O2(C,A,I,K)+T1(K,C)*O2(B,A,I,J)
      D1=  T3(A,B,C)
      D2=  T3(A,C,B)+T3(C,B,A)+T3(B,A,C)
      D3=  T3(B,C,A)+T3(C,A,B)
      G1=  O3(A,B,C)
      G2=  O3(A,C,B)+O3(C,B,A)+O3(B,A,C)
      G3=  O3(B,C,A)+O3(C,A,B)
      F=D1*EIGHT-FOUR*D2+D3*TWO
      G=G1*EIGHT-FOUR*G2+G3*TWO
      X1=X1+F*XT21/DENOM
      X2=X2+F*O3(A,B,C)/DENOM
      Y1=Y1+G*O3(A,B,C)/DENOM
      Y2=Y2+G*XT21/DENOM
      Y3=Y3+G*XT111/DENOM
      Y4=Y4+G*XTO/DENOM
 150  CONTINUE
      CF=ONE
      IEJ=I.EQ.J
      JEK=J.EQ.K
      IF(IEJ.OR.JEK) CF=HALF
      ESD_TM=ESD_TM+CF*X1
      ETTM=ETTM+CF*X2
      ETD=ETD+CF*Y1
      ETS=ETS+CF*Y2
      OTS=OTS+CF*Y3
      OTD=OTD+CF*Y4
      RETURN
      END
C*MODULE CCSDT   *DECK ZEROT3
      SUBROUTINE ZEROT3(T3,NU)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER A
      DIMENSION T3(NU,NU,NU)
      DATA ZERO/0.0D+00/
C
      DO  A=1,NU
      T3(A,A,A)=ZERO
      ENDDO
      RETURN
      END
C
C         this was originally intt3.f
C
C*MODULE CCSDT   *DECK DRINTRI
      SUBROUTINE DRINTRI(NO,NU,MODE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*21 STRINGS(0:1)
      COMMON /CCINFO/ TSH,NH,NP,MET,MEM,ICONV,MAXIT,IREST,IDISC
      COMMON /FMCOM / X(1)
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
      DATA STRINGS/'NONITERATIVE TRIPLES ', 'EOMCCSD INTERMEDIATES'/
C
      NO2   = NO*NO
      NO3   = NO*NO2
      NU2   = NU*NU
      NU3   = NU*NU2
      NOU   = NO*NU
      NO3U  = NO3*NU
      NOU3  = NO*NU3
      NO2U2 = NO2*NU2
C
      LNU3=NU3
      IF(NO3U.GT.NU3) LNU3=NO3U
C
      CALL GOTFM(NGOTMX)
      IF(MEM.LT.NGOTMX) NGOTMX=MEM
C
      CALL VALFM(LOADFM)
      I1   = LOADFM + 1
      I2   = I1     + NOU
      I3   = I2     + LNU3
      I4   = I3     + NO2U2
      LAST = I4     + NOU3
      NEED = LAST - LOADFM - 1
      WRITE(6,991) STRINGS(MODE),NEED
      CALL FLSHBF(6)
C
      CALL GETFM(NEED)
      IF(EXETYP.EQ.CHECK) GO TO 800
C
      CALL INTQUA(NO,NU,X(I1),X(I2),X(I3),X(I4))
C
  800 CONTINUE
      CALL RETFM(NEED)
C
C        there are two algorithms depending on memory for -vm- or -ve-
C
      CALL VALFM(LOADFM)
      LTI   = LOADFM + 1
      LFPH  = LTI    + LNU3
      LT2   = LFPH   + NOU
      LVE1  = LT2    + NO2U2
      LVM   = LVE1   + NOU3
      LVE   = LVM
      LAST1 = LVM    + NOU3
      LAST2 = LVE    + NO3U
      NEED1 = LAST1 - LOADFM - 1
      NEED2 = LAST2 - LOADFM - 1
      WRITE(6,992) STRINGS(MODE),NEED1
      WRITE(6,993) STRINGS(MODE),NEED2
C
      IF(NEED1.LE.NGOTMX) THEN
         CALL GETFM(NEED1)
         WRITE(6,997)
         CALL FLSHBF(6)
         IF(EXETYP.NE.CHECK)
     *   CALL INTRIPL(NO,NU,X(LTI),X(LFPH),X(LT2),X(LVE),X(LVE1))
         CALL RETFM(NEED1)
      ELSE IF(NEED2.LE.NGOTMX) THEN
         CALL GETFM(NEED2)
         WRITE(6,998)
         CALL FLSHBF(6)
         IF(EXETYP.NE.CHECK)
     *   CALL INTRIP(NO,NU,X(LTI),X(LFPH),X(LT2),X(LVM),X(LVE1))
         CALL RETFM(NEED2)
      ELSE
         WRITE(6,91) STRINGS(MODE),NEED2,NGOTMX
         CALL ABRT
      END IF
C
      CALL VALFM(LOADFM)
      I1   = LOADFM + 1
      I2   = I1     + LNU3
      I3   = I2     + NOU
      I4   = I3     + NO2U2
      I5   = I4     + NO3U
      I6   = I5     + NO3U
      LAST = I6     + NOU3
      NEED = LAST - LOADFM - 1
      WRITE(6,994) STRINGS(MODE),NEED
      CALL FLSHBF(6)
C
      CALL GETFM(NEED)
      IF(EXETYP.EQ.CHECK) GO TO 850
C
      CALL INTRIH(NO,NU,X(I1),X(I2),X(I3),X(I4),X(I5),X(I6))
C
  850 CONTINUE
      CALL RETFM(NEED)
      RETURN
C
  991 FORMAT(1X,'MEMORY REQUIRED FOR ',A21,' (INTQUA )',
     *          ' IS ',I12,' WORDS.')
  992 FORMAT(1X,'MEMORY REQUIRED FOR ',A21,' (INTRIPL)',
     *          ' IS ',I12,' WORDS.')
  993 FORMAT(1X,'MEMORY REQUIRED FOR ',A21,' (INTRIP )',
     *          ' IS ',I12,' WORDS.')
  994 FORMAT(1X,'MEMORY REQUIRED FOR ',A21,' (INTRIH )',
     *          ' IS ',I12,' WORDS.')
  997 FORMAT(1X,'THERE IS ENOUGH MEMORY TO RUN THE MORE EFFICIENT',
     *          ' INTRIPL INSTEAD OF INTRIP.')
  998 FORMAT(1X,'THERE IS NOT ENOUGH MEMORY TO RUN INTRIPL, CHOOSING',
     *          ' INTRIP INSTEAD.')
   91 FORMAT(1X,'INSUFFICIENT MEMORY FOR ',A21/
     *       1X,'REQUIRED:',I12,'     AVAILABLE:',I12)
      END
C*MODULE CCSDT   *DECK INTRIH
      SUBROUTINE INTRIH(NO,NU,TI,FHP,T2,VM,VM1,VE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TI(1),T2(1),FHP(1),VE(1),VM(1),VM1(1)
      DATA ZERO/0.0D+00/, ONE/1.0D+00/,OM/-1.0D+00/
C
      NO2   = NO*NO
      NO3   = NO*NO2
      NU2   = NU*NU
      NOU   = NO*NU
      NO2U  = NO2*NU
C
      CALL RO2(1,NO,NU,T2)
      CALL TRANSQ(T2,NOU)
      CALL INSITU(NU,NO,NO,NU,TI,T2,12)
      CALL RDVM(2,NO,NU,VM)
      CALL RDVEM4(1,NU,NO,TI,VM1)
      CALL SYMT21(T2,NO,NU,NO,NU,13)
      CALL DGEMM('N','N',NO2,NOU,NOU,ONE,VM,NO2,T2,NOU,ONE,VM1,NO2)
      CALL DESM21(T2,NO,NU,NO,NU,13)
      CALL TRANMD(VM,NO,NO,NO,NU,23)
      CALL DGEMM('N','N',NO2,NOU,NOU,OM,VM,NO2,T2,NOU,ONE,VM1,NO2)
      CALL TRANMD(T2,NO,NU,NO,NU,13)
      CALL TRANMD(VM1,NO,NO,NO,NU,13)
      CALL DGEMM('N','N',NO2,NOU,NOU,OM,VM,NO2,T2,NOU,ONE,VM1,NO2)
      CALL TRANMD(T2,NO,NU,NO,NU,13)
      CALL TRANMD(VM,NO,NO,NO,NU,23)
      CALL TRANMD(VM1,NO,NO,NO,NU,231)
      CALL INSITU(NO,NU,NO,NU,TI,T2,23)
      CALL RDVE(2,NO,NU,VE)
      CALL TRANMD(VE,NU,NU,NU,NO,13)
      CALL DGEMM('N','N',NO2,NOU,NU2,ONE,T2,NO2,VE,NU2,ZERO,VM,NO2)
      CALL VMCP(NO,NU,VM1,VM)
      CALL TRANMD(VM1,NO,NO,NO,NU,231)
      CALL RO1(1,NO,NU,FHP)
      CALL RO2(3,NO,NU,T2)
      CALL TRANSQ(T2,NOU)
      CALL TRANMD(VM1,NO,NO,NO,NU,12)
      CALL DGEMM('N','N',NO,NO2U,NU,ONE,FHP,NO,T2,NU,ONE,VM1,NO)
      CALL TRANMD(VM1,NO,NO,NO,NU,231)
      CALL RO2(4,NO,NU,T2)
      CALL TRANSQ(T2,NOU)
      CALL DGEMM('N','N',NO,NO2U,NU,ONE,FHP,NO,T2,NU,ONE,VM1,NO)
      CALL TRANMD(VM1,NO,NO,NO,NU,312)
      CALL RDOV4(1,NU,NO,TI,VM)
      CALL TRANMD(VM1,NO,NO,NO,NU,12)
      CALL MTRANS(VM,NO,7)
      CALL DGEMM('N','N',NO3,NU,NO,OM,VM,NO3,FHP,NO,ONE,VM1,NO3)
      CALL WRVM(3,NO,NU,VM1)
      RETURN
      END
C*MODULE CCSDT   *DECK INTRIP
      SUBROUTINE INTRIP(NO,NU,TI,FPH,T2,VM,VE1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TI(1),FPH(NO,NU),T2(NU,NU,NO,NO),VM(1),VE1(1)
      DATA ON/1.0D+00/,OM/-1.0D+00/
C
      NO2   = NO*NO
      NU2   = NU*NU
      NU3   = NU*NU2
      NOU   = NO*NU
      NOU2  = NO*NU2
C
      CALL RO2(1,NO,NU,T2)
      CALL INSITU(NO,NU,NU,NO,TI,T2,13)
      CALL RDVEM4(0,NO,NU,TI,VE1)
      DO I=1,NO
      CALL RDVVE(I,2,NO,NU,TI)
      CALL SYMT21(T2(1,1,1,I),NU,NU,NO,1,12)
      CALL DGEMM('N','N',NU2,NOU,NU,ON,TI,NU2,T2(1,1,1,I),NU,ON,VE1,NU2)
      CALL DESM21(T2(1,1,1,I),NU,NU,NO,1,12)
      CALL TRANT3(TI,NU,1)
      CALL DGEMM('N','N',NU2,NOU,NU,OM,TI,NU2,T2(1,1,1,I),NU,ON,VE1,NU2)
      ENDDO
      CALL TRANMD(T2,NU,NU,NO,NO,12)
      CALL TRANMD(VE1,NU,NU,NU,NO,13)
      DO I=1,NO
      CALL RDVVE(I,2,NO,NU,TI)
      CALL TRANT3(TI,NU,1)
      CALL DGEMM('N','N',NU2,NOU,NU,OM,TI,NU2,T2(1,1,1,I),NU,ON,VE1,NU2)
      ENDDO
      CALL TRANMD(VE1,NU,NU,NU,NO,231)
      CALL RDVM(2,NO,NU,TI)
      CALL VMCP2(NO,NU,VM,TI)
      CALL TRANMD(VM,NO,NO,NU,NO,14)
      CALL DGEMM('N','N',NU2,NOU,NO2,ON,T2,NU2,VM,NO2,ON,VE1,NU2)
      CALL TRANMD(VE1,NU,NU,NU,NO,231)
      CALL RO2(1,NO,NU,T2)
      CALL RO1(3,NO,NU,FPH)
      CALL DGEMM('T','N',NU,NOU2,NO,OM,FPH,NO,T2,NO,ON,VE1,NU)
      CALL RO1(1,NO,NU,FPH)
      CALL RO2(3,NO,NU,T2)
      CALL TRANMD(T2,NO,NU,NU,NO,1234)
      CALL TRANMD(VE1,NU,NU,NU,NO,12)
      CALL DGEMM('T','N',NU,NOU2,NO,OM,FPH,NO,T2,NO,ON,VE1,NU)
      CALL TRANMD(VE1,NU,NU,NU,NO,231)
      CALL RO2(5,NO,NU,T2)
      CALL TRANMD(T2,NO,NU,NU,NO,1234)
      CALL DGEMM('T','N',NU,NOU2,NO,OM,FPH,NO,T2,NO,ON,VE1,NU)
      DO I=1,NU
      CALL RDVPPT(I,NO,NU,TI)
      CALL DGEMM('N','N',NU3,NO,1,ON,TI,NU3,FPH(1,I),1,ON,VE1,NU3)
      ENDDO
      CALL TRANMD(VE1,NU,NU,NU,NO,13)
      CALL WRVE(3,NO,NU,VE1)
      RETURN
      END
C*MODULE CCSDT   *DECK INTQUA
      SUBROUTINE INTQUA(NO,NU,O1,TI,VOE,VE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TI(1),VOE(1),VE(1),O1(1)
      DATA ONE/1.0D+00/,OM/-1.0D+00/
C
      NO2   = NO*NO
      NU2   = NU*NU
      NOU   = NO*NU
      NO2U  = NO2*NU
      NOU2  = NO*NU2
C
      CALL RO1(1,NO,NU,O1)
      CALL RO2HPP(1,NO,NU,TI,VOE)
      CALL TRANSQ(VOE,NOU)
      CALL RDVEM4(1,NU,NO,TI,VE)
      CALL DGEMM('N','N',NO,NO2U,NU,ONE,O1,NO,VOE,NU,ONE,VE,NO)
      CALL WRVM(2,NO,NU,VE)
      CALL TRANSQ(VOE,NOU)
      CALL RDVEM4(0,NO,NU,TI,VE)
      CALL DGEMM('T','N',NU,NOU2,NO,OM,O1,NO,VOE,NO,ONE,VE,NU)
      CALL WRVE(2,NO,NU,VE)
      CALL TRANMD(VE,NU,NU,NU,NO,12)
      CALL DGEMM('N','N',NO,NOU2,NU,ONE,O1,NO,VE,NU,ONE,VOE,NO)
      CALL TRANMD(VOE,NO,NU,NU,NO,1234)
      CALL TRANSQ(VOE,NOU)
      CALL RDVEM4(1,NU,NO,TI,VE)
      CALL DGEMM('T','N',NU,NO2U,NO,OM,O1,NO,VE,NO,ONE,VOE,NU)
C      CALL CCTMATMUL(O1,VE,voe,NU,NO2U,NO,0,1)
      CALL TRANSQ(VOE,NOU)
      CALL TRANMD(VOE,NO,NU,NU,NO,1234)
      CALL WO2(3,NO,NU,VOE)
      CALL RO2HPP(2,NO,NU,TI,VOE)
      CALL TRANSQ(VOE,NOU)
      CALL TRANMD(VE,NO,NO,NO,NU,13)
      CALL DGEMM('T','N',NU,NO2U,NO,OM,O1,NO,VE,NO,ONE,VOE,NU)
C      CALL CCTMATMUL(O1,VE,voe,NU,NO2U,NO,0,1)
      CALL TRANMD(VE,NO,NO,NO,NU,13)
      CALL TRANSQ(VOE,NOU)
      CALL TRANMD(VOE,NO,NU,NU,NO,1234)
      CALL WO2(4,NO,NU,VOE)
      CALL RO2HPP(2,NO,NU,TI,VOE)
      CALL RDVEM4(0,NO,NU,TI,VE)
      CALL TRANMD(VE,NU,NU,NU,NO,13)
      CALL DGEMM('N','N',NO,NOU2,NU,ONE,O1,NO,VE,NU,ONE,VOE,NO)
C      CALL CCMATMUL(O1,VE,voe,NO,NOU2,NU,0,0)
      CALL WO2(5,NO,NU,VOE)
      RETURN
      END
C*MODULE CCSDT   *DECK INTRIPL
      SUBROUTINE INTRIPL(NO,NU,TI,FPH,T2,VE,VE1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TI(1),FPH(NO,NU),T2(1),VE(NU,NU,NU,NU),VE1(1)
      DATA ON/1.0D+00/,OM/-1.0D+00/
C
      NO2   = NO*NO
      NU2   = NU*NU
      NU3   = NU*NU2
      NOU   = NO*NU
      NOU2  = NO*NU2
C
C uwaga ti moze zastepowac vm
C
      CALL RO2(1,NO,NU,T2)
      CALL INSITU(NO,NU,NU,NO,TI,T2,12)
      CALL RDVE(2,NO,NU,VE)
      CALL RDVEM4(0,NO,NU,TI,VE1)
      CALL SYMT21(T2,NU,NO,NU,NO,13)
      CALL DGEMM('N','N',NU2,NOU,NOU,ON,VE,NU2,T2,NOU,ON,VE1,NU2)
      CALL DESM21(T2,NU,NO,NU,NO,13)
      CALL TRANMD(VE,NU,NU,NU,NO,23)
      CALL DGEMM('N','N',NU2,NOU,NOU,OM,VE,NU2,T2,NOU,ON,VE1,NU2)
      CALL TRANMD(T2,NU,NO,NU,NO,13)
      CALL TRANMD(VE1,NU,NU,NU,NO,13)
      CALL DGEMM('N','N',NU2,NOU,NOU,OM,VE,NU2,T2,NOU,ON,VE1,NU2)
      CALL TRANMD(VE,NU,NU,NU,NO,23)
      CALL TRANMD(T2,NU,NO,NU,NO,13)
      CALL TRANMD(VE1,NU,NU,NU,NO,231)
      CALL INSITU(NU,NO,NU,NO,TI,T2,23)
      CALL RDVM(2,NO,NU,TI)
      CALL VMCP2(NO,NU,VE,TI)
      CALL TRANMD(VE,NO,NO,NU,NO,14)
      CALL DGEMM('N','N',NU2,NOU,NO2,ON,T2,NU2,VE,NO2,ON,VE1,NU2)
      CALL TRANMD(VE1,NU,NU,NU,NO,231)
      CALL RO2(1,NO,NU,T2)
      CALL RO1(3,NO,NU,FPH)
      CALL DGEMM('T','N',NU,NOU2,NO,OM,FPH,NO,T2,NO,ON,VE1,NU)
      CALL RO1(1,NO,NU,FPH)
      CALL RO2(3,NO,NU,T2)
      CALL TRANMD(T2,NO,NU,NU,NO,1234)
      CALL TRANMD(VE1,NU,NU,NU,NO,12)
      CALL DGEMM('T','N',NU,NOU2,NO,OM,FPH,NO,T2,NO,ON,VE1,NU)
      CALL TRANMD(VE1,NU,NU,NU,NO,231)
      CALL RO2(5,NO,NU,T2)
      CALL TRANMD(T2,NO,NU,NU,NO,1234)
      CALL DGEMM('T','N',NU,NOU2,NO,OM,FPH,NO,T2,NO,ON,VE1,NU)
        DO I=1,NU
      CALL RDVPPT(I,NO,NU,TI)
      CALL DGEMM('N','N',NU3,NO,1,ON,TI,NU3,FPH(1,I),1,ON,VE1,NU3)
      ENDDO
      CALL TRANMD(VE1,NU,NU,NU,NO,13)
      CALL WRVE(3,NO,NU,VE1)
      RETURN
      END
C
C         this was originally suma.f
C
C*MODULE CCSDT   *DECK DRSUMA
      SUBROUTINE DRSUMA(NO,NU,OEH,OEP)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL CNVR
      DIMENSION OEH(NO),OEP(NU)
      COMMON /CCENGY/ ENRG,EREF,EMP2,ETOT(6),ECORR(6),
     *                DIAGS(3),AMPMX(5,2),IAMPMX(5,4,2),XO1,XO2,
     *                DIFMAX,DIFFENG,ITER,CNVR
      COMMON /CCRENO/ OSS,ODS,ODD,OTS,OTD,OTT,ODS_S,ODS_D,ODS_T,
     *                OQS,OQDS,OQDD,OQTS,ESD,ETD,ETS,ETTM,ESD_TM
      DATA ONE/1.0D+00/
C
      CALL DRT3SQTOT(NO,NU,OEH,OEP)
      XO1=ONE+OSS+ODS+ODD+OTS+OTD
      XO2=ONE+OSS+ODS+ODD+OTS+OTD+ODS_S+ODS_D
      ECORR(1)=ESD+ ETD
      ECORR(2)=ESD+ ETD+ETS
      ECORR(3)=ESD+ ETD/XO1
      ECORR(4)=ESD+(ETD+ETS)/XO2
      ECORR(5)=ESD+ ETTM/XO1
      ECORR(6)=ESD+(ETTM+ESD_TM)/XO2
      DO I=1,6
         ETOT(I)=EREF+ECORR(I)
      ENDDO
      RETURN
      END
