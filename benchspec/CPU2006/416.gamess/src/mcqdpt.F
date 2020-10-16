C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C 26 MAR 03 - MWS - ABORT PARALLEL REFWGT, FIX REFWGT CI STATE PRINTING
C 28 JAN 03 - DGF - MQDRV: PASS VALID ARGUMENT IN ONE VALFM CALL
C 12 DEC 02 - HN,MWS - MQDRV: AVOID POSSIBLE 32 BIT OVERFLOWS ON NREQ2
C 17 APR 02 - MWS - MQREAD: REVERSE DETERMINANT SYMMETRY IN C2H GROUP
C 24 JAN 02 - DGF - MQCACI: CHANGES TO ORTHOGONALIZATION RETENTION TEST
C  5 JAN 02 - MWS - MQREAD: USE NEW STANDARD CORE COUNTING ROUTINE
C 16 NOV 01 - MWS - MQREAD: REQUIRE ISTSYM INPUT IN SPIN-ORBIT JOBS
C 29 OCT 01 - DGF - IMPROVE EFFICIENCY OF MCQDPT DIAGRAM LOOPS
C 25 OCT 01 - HW,DGF - INTRODUCE SPIN-FREE ISF (AKA ISA) TECHNIQUE
C 25 OCT 01 - MWS - MQMOSY: FIX C2H LABS, MQREAD: ISTSYM FROM $DET,$DRT
C  8 OCT 01 - HU  - FIX BUG WITH AO INTEGRAL SORTING
C 19 SEP 01 - MWS - FIX BUGS, RESTORE ZERO DENOMINATOR SHIFTS
C  7 SEP 01 - HU  - PARALLELIZE MCQDPT: PARAIO, DOORD0, DELSCR OPTIONS.
C                   MQREMO: READ $VEC AT MASTER NODE, AND THEN DDI_BCAST
C                   MQREMO,MQDRV,MQREAD,MQSORD: IPURIFY -> IPURFY
C                   MQCONF,MQI1EX,MQHASH: IMPLEMENT HASHING SEARCH
C  1 AUG 01 - JI  - TWEAKS FOR GENERAL CI
C 25 JUN 01 - DGF - ELIMINATE MQTRF CALLS FOR GLOBAL CANONICALISATION,
C                   MULTIPLE $VEC GROUPS FOR SO-MCQDPT, ORB. PURIFY
C 12 JUN 01 - HN  - PRINT CI VECTORS WITH NEW MQPRWF ROUTINE
C 12 JUN 01 - MWS - TRAP DAVIDSON DIAG DIFFS BETWEEN CANON AND P.T. CI'S
C 11 OCT 00 - MWS - UPDATE THE DETWFN COMMON
C 28 JUL 00 - MWS - MQREAD: READ WSTATE INSTEAD OF AVECOE
C 11 JUN 00 - MWS - MQREMO: READ ORBS FROM DAF INSTEAD OF FROM CARDS
C  1 MAY 00 - MWS - PLACE BETTER LABELING OF COMPUTATION ON OUTPUT
C 25 MAR 00 - MWS - RAISE NUMBER OF ROOTS FROM 20 TO MATCH OTHER CODE
C 16 FEB 00 - MWS - ADD A NOSYM OPTION (WHICH MAY NOT WORK...)
C 29 DEC 99 - SV  - ALLOW OPTION TO CALL THE MCQDPT WEIGHT CODE
C 21 DEC 99 - MWS - MAKE SYMMOL AND GUGWFN COMMON CONSISTENT
C 12 NOV 98 - GDF - CHANGE BIT PACKING TO ISHIFT
C  3 OCT 98 - MWS - MQREAD: INCLUDE -NBOS- IN ACTIVE SPACE
C 27 SEP 98 - MWS - MQREAD: ENFORCE MULTIPLICITY MATCH
C 12 SEP 98 - MWS - MQCIG1: FIX A ERROR FORMAT STATEMENT
C 26 OCT 98 - MAF - MQDRV: ALLOW FOR USE OF SPHERICAL HARMONICS
C 13 APR 98 - MWS - MQREAD: CHANGES FOR DETERMINANT BASED MCSCF
C 27 FEB 98 - MWS - MQREAD: CHECKS ON NUMBERS OF ELECTRONS ADDED
C  6 JAN 98 - MWS - MQREMO: ADD EXPLANATION FOR ORTHOGONALITY LOSS
C 28 SEP 97 - MWS - USE CORRECT MXAOCI PARAMETER DIMENSIONING
C 14 AUG 97 - KRG - MQREMO: ADD ERROR MESSAGE BEFORE STOPPING
C 24 JUN 97 - KRG - MQREAD: ADD SEMI-LOGICAL GUESS FOR $MCQDPT,
C                   SET INORB=0 FOR TRUDGE RUNS IN MQREAD,
C                   BETTER ERROR HANDLING IN MQREAD FOR $MCQDPT,
C                   NO LONGER USES NSTATE AND NSTCI (ONLY KSTATE)
C                   MQCORE: ADDED TO COUNT CHEMICAL CORES FOR DEFAULT
C  7 APR 97 - JF  - MQLMB2: ADD DIRECTIVES DEFEATING SX4 VECTORIZATION
C  2 APR 97 - MWS - USE -SEQREW- INSTEAD OF REWINDS THROUGHOUT
C 19 FEB 97 - MWS - MQGETE: SET MP2 ENERGY IN COMMON FOR TRUDGE RUNS
C 17 JAN 97 - KRG - MQDRV: CORRECT CSF NUMBER DURING CHECK RUNS
C 20 DEC 96 - HPP - MQCONF,MQPCNF,MQINIT,MQADDR: CORRECTIONS FOR CRAY
C 26 NOV 96 - MWS - MQMOSY: ASSIGN C2H,D2,D2H CORRECTLY,
C                   DELETE UNUSED ROUTINES MQGN2R,MQGN2W,MQWMAT
C 19 NOV 96 - FPR - ADD CARRIAGE CONTROL TO FORMAT STRINGS
C  6 NOV 96 - MWS - MQOPDA,MQDARE,MQDAWR TO IOLIB, IMPLEMENT CHECK RUNS
C 30 OCT 96 - HN  - NEW MODULE FOR MULTIREFERENCE PERTURBATION THEORY
C
C*MODULE MCQDPT  *DECK MCQDPT
      SUBROUTINE MCQDPT(GROUP,GRPV)
C**** INSTRUCTIONS *****************************************************
C
C     NAME-LIST $MCQDPT
C
C     *** GENERAL ***
C
C     NEL    : # OF ELECTRONS
C     MULT   : SPIN MULTIPLICITY
C     NMOFZC : # OF FROZEN CORE ORBITALS (NOT CORRELATED IN PT)
C     NMODOC : # OF DOUBLY OCCUPIED ORBITALS (CORRELATED IN PT)
C     NMOACT : # OF ACTIVE ORBITALS
C     NMOFZV : # OF FROZEN VIRTUAL ORBITALS (NOT CORRELATED IN PT)
C     NSTATE : # OF TARGET STATE(S)
C              ( = DIMENSION OF EFFECTIVE HAMILTONIAN )
C     ISTSYM : STATE SYMMETRY OF TARGET STATE(S)
C     INORB  : READ INPUT ORBITALS FROM CARDS (1) OR FROM THE DIRECT
C              ACCESS FILE (0)
C     LPOUT  : PRINT OPTION IN MC-QDPT
C              0:NORMAL PRINT, <0:DEBUG PRINT (-1,-5,-10,-100)
C
C     *** CAS-CI ***
C
C     NSTCI  : # OF STATES TO BE OBTAINED IN THE DAVIDSON'S METHOD
C     KSTATE : STATE IS USED (1) OR NOT (0) IN MC-QDPT2 (1-D ARRAY)
C
C     EXAMPLE. IF YOU WANT THE SECOND AND THE FOURTH ROOTS, SET
C              NSTATE=2, NSTCI=4, KSTATE=0,1,0,1, .
C
C     THRENE : THRESHOLD FOR THE ENERGY CONVERGENCE IN THE DAVIDSON'S
C              METHOD
C     THRCON : THRESHOLD FOR THE VECTOR CONVERGENCE IN THE DAVIDSON'S
C              METHOD
C
C     *** CANONICAL FOCK ORBITALS ***
C
C     IFORB  : REDEFINE CANONICAL FOCK ORBITALS (1) OR NOT (0)
C     AVECOE : WEIGHT OF THE EACH STATE IN COMPUTING (AVERAGED) DENSITY
C              MATRIX (1D ARRAY)
C
C     *** MC-QDPT2 ***
C
C     THRGEN : THRESHOLD FOR ONE-, TWO-, THREE-PARTICLE COUPLING
C              CONSTANTS IN THE PERTURBATION CALCULATION
C              NOTE: IF YOU WANT, FOR EXAMPLE, TO OBTAIN ENERGIES
C                    TO 6 FIGURES AFTER POINT, SET
C                    THRGEN=1.0D-08 OR 1.0D-09.
C
C     *** DEFAULT VALUES ***
C
C     IFORB  = 1
C     INORB  = 0
C     ISELCT = 0
C     ISTSYM = 1
C     LPOUT  = 0
C     MULT   = 1
C     NEL    = NE-ICH
C     NMOACT = 0
C     NMODOC = 0
C     NMOFZC = 0
C     NMOFZV = 0
C     NSTATE = 1
C     NSTCI  = 1
C
C     THRCON =  1.0D-06
C     THRENE = -1.0D+00
C     THRGEN =  1.0D-08
C     THRWGT =  1.0D-06
C
C     DO I=1,20
C       AVECOE(I) = 1.0D+00
C     END DO
C     DO I=1,20
C       KSTATE(I) = 1
C     END DO
C
C     *** REFERENCES ***
C
C     MC-QDPT
C       H.NAKANO, J.CHEM.PHYS.,99,7983(1993)
C       H.NAKANO, CHEM.PHYS.LETT.,207,372(1993).
C
C     THIS PROGRAM IS BASED ON
C       H.NAKANO, J.CHEM.PHYS.,99,7983(1993).
C
C     IN A SINGLE-STATE CASE (NAMELY, NSTATE=1 CASE) MC-QDPT REDUCES TO
C     MRMP PT.  THE REFERENCES OF MRMP PT ARE
C       K.HIRAO, CHEM.PHYS.LETT.,190,374(1992)
C                                196,397(1992)
C                                201,59(1993)
C                INTERN.J.QUANTUM CHEM.,S26,517(1992).
C
C     *** NOTES ***
C
C     SEE ROUTINE MQREAD FOR FURTHER INFORMATION.
C
C**** END OF INSTRUCTIONS **********************************************
C
C***********************************************************************
C
C       MAIN PROGRAM FOR MC-QDPT2
C
C***********************************************************************
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/IOFILE/IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      LOGICAL GOPARR,DSKWRK,MASWRK
      LOGICAL TDSKWK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C**** WRITE TITLE ******************************************************
      IF (MASWRK) WRITE(IW,9100)
C**** CALL DRIVER ROUTINE **********************************************
      TDSKWK=DSKWRK
      DSKWRK=.TRUE.
      CALL MQDRV(GROUP,GRPV)
      DSKWRK=TDSKWK
      IF (MASWRK) WRITE(IW,9200)
      RETURN
C.... MODIFIED ON 9/9/01
 9100 FORMAT(/10X,17("-"),10X,27("-")/
     1        10X,'MC-QDPT2 (MR-MP2)',
     2        10X,'PROGRAM WRITTEN BY H.NAKANO'/
     3        10X,17(1H-),10X,27(1H-)/
     5        37X,'    PARALLELIZED BY H.UMEDA'/
     6        37X,27(1H-)/)
 9200 FORMAT(' ...... END OF MC-QDPT2 (MR-MP2) CALCULATION ......',/)
      END
C*MODULE MCQDPT  *DECK MQCACI
      SUBROUTINE MQCACI(LUNOUT,LPOUT ,MAXOVL,LUNIII,LUNFT0,
     *                  NMO   ,NMODOC,NMOACT,INIMO ,LASMO ,
     *                  NMOEA ,NMOAA ,
     *                  NCI   ,
     *                  MAXDIA,NSTCI ,NSTATE,MDI   ,MAXERI,MAXCSF,
     *                  MXBASE,NSOLUT,NSTOP ,
     *                  ENUCLE,THRCON,THRENE,
     *                  MAINCS,
     *                  EIGVAL,EIGVEC,LWCD  ,WORKER,
     *                  LIJMO ,HCORE ,VONEEL,VTWOEL,LABEL1,WORKGE,
     *                  DIAHMA,DIAVAL,DIAVEC,DIAWRK,HMAT  ,LORDER,
     *                  HMATDI,LNWOLD,LODNEW,
     *                  BASE  ,HXBASE,WRKVEC,
     *                  EBEFOR,ENEWRK,DIFVEC,LKCONV,
     *                  KREF  ,KSTATE,EIGVOD,AVECOE,AVGENG,
     *                  CACIOP,KFT0M ,LFT0M ,WFT0M )
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***********************************************************************
C****                     **********************************************
C**** ARRAYS AND CONSTANT **********************************************
C****                     **********************************************
      DIMENSION EIGVAL(NSTATE,0:2), EIGVEC(NCI,NSTATE)
      INTEGER   LWCD(2,MAXERI)
      DIMENSION WORKER(MAXERI)
      DIMENSION LIJMO(INIMO:LASMO,INIMO:LASMO)
      DIMENSION HCORE(NMO,NMO), VONEEL(INIMO:LASMO,INIMO:LASMO)
      DIMENSION VTWOEL(NMOEA,NMOAA)
      INTEGER   LABEL1(4,MAXCSF)
      DIMENSION WORKGE(MAXCSF)
      DIMENSION DIAHMA(MAXDIA*(MAXDIA+1)/2)
      DIMENSION DIAVAL(MAXDIA), DIAVEC(MAXDIA,MAXDIA), DIAWRK(MAXDIA*8)
      DIMENSION HMAT (MAXDIA,MAXDIA), LORDER(MAXDIA)
      DIMENSION HMATDI(NCI)
      DIMENSION LNWOLD(NCI),LODNEW(NCI)
      DIMENSION BASE(NCI,MXBASE),HXBASE(NCI,MXBASE),WRKVEC(NCI)
      DIMENSION EBEFOR(NSOLUT)       ,ENEWRK(NSOLUT),DIFVEC(NSOLUT)
      DIMENSION LKCONV(NSOLUT)
      DIMENSION MAINCS(3)
      LOGICAL   KREF(NCI), KSTATE(NSTCI)
      DIMENSION EIGVOD(NCI,NSTATE),AVECOE(NSTCI)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
C
      COMMON /MQFT02/NWFT0,NWFT0P
      INTEGER CACIOP
      INTEGER   KFT0M(2,1),LFT0M(4,1)
      DIMENSION WFT0M(1)
C
C**** THRESHOLD ZERO ***************************************************
      THRZRO=1.0D-12
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQCACI ***'')')
        CALL MQDEBG(LUNOUT,
     *  'MAXOVL','LUNIII','LUNFT0','   NMO','NMODOC','NMOACT',' INIMO',
     *   MAXOVL , LUNIII , LUNFT0 ,    NMO , NMODOC , NMOACT ,  INIMO )
        CALL MQDEBG(LUNOUT,
     *  ' LASMO',' NMOEA',' NMOAA','   NCI','MAXDIA',' NSTCI','NSTATE',
     *    LASMO ,  NMOEA ,  NMOAA ,    NCI , MAXDIA ,  NSTCI , NSTATE )
        CALL MQDEBG(LUNOUT,
     *  '   MDI','MAXERI','MAXCSF','MXBASE','NSOLUT',' NSTOP','MAINC1',
     *      MDI , MAXERI , MAXCSF , MXBASE , NSOLUT ,  NSTOP ,
     *   MAINCS(1))
        CALL MQDEBG(LUNOUT,
     *  'MAINC2','MAINC3','     -','     -',' NWFT0','NWFT0P','CACIOP',
     *   MAINCS(2),MAINCS(3),   0 ,      0 ,  NWFT0 , NWFT0P , CACIOP )
      END IF
C                                READ FILE LUNFT0 ON MEMORY IF POSSIBLE
C ----------------------------------------------------------------------
C
C CACIOP:
C   0 => SAME AS PREVIOUS VERSION.
C   1 => READ LUNFT0 FILE TO ARRAYS KFT0M, LFT0M, WFT0M BEFORE LOOPS.
      IF (CACIOP.EQ.1) THEN
C --- READ LUNFTA TO ARRAYS KFT0M, LFT0M, WFT0M ---
C KFT0M(2,NWFT0)
C LFT0M(4,NWFT0P)
C WFT0M(NWFT0P)
         CALL SEQREW(LUNFT0)
         NWFT0M=1
         DO J=1,NWFT0
C            READ(LUNFT0) KCONT,NWORD
            READ(LUNFT0) KFT0M(1,J),KFT0M(2,J)
            NWORD=KFT0M(2,J)
C            CALL MQGENR(LUNFT0,4*NWORD,NWORD,LABEL1,WORKGE)
            CALL MQGENR(LUNFT0,4*NWORD,NWORD,
     *           LFT0M(1,NWFT0M),WFT0M(NWFT0M))
            NWFT0M=NWFT0M+NWORD
         END DO
      END IF
C
C***********************************************************************
C****                ***************************************************
C**** PREPARE ARRAYS ***************************************************
C****                ***************************************************
C**** CLEAR ARRAYS *****************************************************
C
      IF (MASWRK) WRITE(LUNOUT,9000)
 9000 FORMAT(1X,'##########################'/
     *       1X,'###   CAS-CI RESULTS   ###'/
     *       1X,'##########################')
      AVGENG = 0.0D+00
      CALL FLSHBF(LUNOUT)
C
      DO J=INIMO,LASMO
        DO I=INIMO,LASMO
          VONEEL(I,J)=0.0D+00
        END DO
      END DO
      DO J=1,NMOAA
        DO I=1,NMOEA
          VTWOEL(I,J)=0.0D+00
        END DO
      END DO
      DO J=1,MAXDIA
        DO I=1,MAXDIA
          HMAT(I,J)=0.0D+00
        END DO
      END DO
      DO I=1,NCI
        HMATDI(I)=0.0D+00
      END DO
      DO J=1,MXBASE
        DO I=1,NCI
          BASE  (I,J)=0.0D+00
          HXBASE(I,J)=0.0D+00
        END DO
      END DO
      DO I=1,NSOLUT
        EBEFOR(I)=0.0D+00
        ENEWRK(I)=0.0D+00
        DIFVEC(I)=0.0D+00
      END DO
C**** PREPARE LIJMO ****************************************************
      N=0
      DO I=INIMO,LASMO
        DO J=INIMO,I
          N=N+1
          LIJMO(I,J)=N
          LIJMO(J,I)=N
        END DO
      END DO
C**** PREPARE PERTURBATION MATRICES ************************************
C
C     VTWOEL AND VONEEL
C
      LUNIN=LUNIII
      CALL SEQREW(LUNIN)
      ECORE=0.0D+00
  100 CONTINUE
        READ(LUNIN) KCONT,IMOAB,JMOAB,NWORD
        IF(KCONT.EQ.0.AND.NWORD.EQ.0) GO TO 110
        CALL MQGENR(LUNIN,2*NWORD,NWORD,LWCD,WORKER)
        DO I=1,NWORD
          LP=IMOAB
          LQ=JMOAB
          LR=LWCD(1,I)
          LS=LWCD(2,I)
          V =WORKER(I)
          IF(LP.LT.INIMO .AND. LQ.LT.INIMO .AND.
     *      LR.LT.INIMO .AND. LS.LT.INIMO) THEN
            IF(LP.EQ.LQ .AND. LR.EQ.LS) THEN
              ECORE=ECORE+2.0D+00*V
              IF(LP.EQ.LR) ECORE=ECORE-V
            ELSE IF(LP.EQ.LR .AND. LQ.EQ.LS) THEN
              ECORE=ECORE-V*2.0D+00
            END IF
          ELSE IF(LP.GE.INIMO .AND. LQ.GE.INIMO .AND.
     *        LR.LT.INIMO .AND. LS.LT.INIMO .AND. LR.EQ.LS) THEN
            IF(LP.NE.LQ) THEN
              VONEEL(LP,LQ)=VONEEL(LP,LQ)+2.0D+00*V
              VONEEL(LQ,LP)=VONEEL(LQ,LP)+2.0D+00*V
            ELSE
              VONEEL(LP,LQ)=VONEEL(LP,LQ)+2.0D+00*V
            END IF
          ELSE IF(LP.GE.INIMO .AND. LQ.LT.INIMO .AND.
     *        LR.GE.INIMO .AND. LS.LT.INIMO .AND. LQ.EQ.LS) THEN
            VONEEL(LP,LR)=VONEEL(LP,LR)-V
          ELSE IF(LP.GE.INIMO .AND. LQ.GE.INIMO .AND.
     *        LR.GE.INIMO .AND. LS.GE.INIMO) THEN
            VTWOEL(LIJMO(LP,LQ),LIJMO(LR,LS))=V
          END IF
        END DO
      IF(KCONT.NE.0) GO TO 100
 110  CONTINUE
      IF (GOPARR) THEN
         CALL DDI_GSUMF(2508,ECORE,1)
         CALL DDI_GSUMF(2509,VTWOEL,NMOEA*NMOAA)
      END IF
C
      DO J=INIMO,LASMO
         IF (.NOT.GOPARR.OR.MOD(J,NPROC).EQ.ME) THEN
            DO I=INIMO,LASMO
               DO K=INIMO,LASMO
                  VONEEL(I,J)=VONEEL(I,J)
     *                 -0.5D+00*VTWOEL(LIJMO(I,K),LIJMO(K,J))
               END DO
            END DO
         END IF
      END DO
      IF (GOPARR) THEN
         CALL DDI_GSUMF(2510,VONEEL,NMOACT*NMOACT)
      END IF
      DO I=1,NMODOC
        ECORE=ECORE+HCORE(I,I)*2.0D+00
      END DO
      ESUM=ENUCLE+ECORE
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LE.-10 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** MATRIX LIJMO ***'')')
        CALL MQWMAI(LUNOUT,LIJMO,NMOACT,NMOACT,NMOACT,'    ')
        WRITE(LUNOUT,'('' *** MATRIX HCORE ***'')')
        CALL MQWMAG(LUNOUT,HCORE,NMO,NMO,NMO,'TRIG')
        WRITE(LUNOUT,'('' *** MATRIX VONEEL ***'')')
        CALL MQWMAG(LUNOUT,VONEEL,NMOACT,NMOACT,NMOACT,'    ')
        IF(LPOUT.LE.-100) THEN
          WRITE(LUNOUT,'('' *** MATRIX VTWOEL ***'')')
          CALL MQWMAG(LUNOUT,VTWOEL,NMOAA,NMOAA,NMOAA,'    ')
        END IF
      END IF
C***********************************************************************
C****                                              *********************
C**** BRANCH (CORE OR OUT-OF-CORE DIAGONALIZATION) *********************
C****                                              *********************
      NREAD=0
      IF(NCI.GT.MDI) GO TO 300
C***********************************************************************
C****                      *********************************************
C**** CORE DIAGONALIZATION *********************************************
C****                      *********************************************
      IF (CACIOP.EQ.0) CALL SEQREW(LUNFT0)
      NKFT0M=1
      NWFT0M=1
C**** GET HAMILTONIAN **************************************************
      DO INWFT0=1,NWFT0
C 200  CONTINUE
        IF (CACIOP.EQ.0) THEN
           READ(LUNFT0) KCONT,NWORD
           CALL MQGENR(LUNFT0,4*NWORD,NWORD,LABEL1,WORKGE)
        ELSE
           KCONT=KFT0M(1,NKFT0M)
           NWORD=KFT0M(2,NKFT0M)
        END IF
C        IF (.NOT.GOPARR.OR.MOD(NKFT0M,NPROC).EQ.ME) THEN
        IF (CACIOP.EQ.1) THEN
           CALL DCOPY(4*NWORD/NWDVAR,LFT0M(1,NWFT0M),1,LABEL1,1)
           CALL DCOPY(NWORD,WFT0M(NWFT0M),1,WORKGE,1)
        END IF
C
        DO I=1,NWORD
          M1 =LABEL1(1,I)
          M2 =LABEL1(2,I)
          LP =LABEL1(3,I)
          LQ =LABEL1(4,I)
          VALI=WORKGE(I)
          IF(KREF(M1).AND.KREF(M2)) THEN
            HMAT(M1,M2)=HMAT(M1,M2)+(HCORE(LP,LQ)+VONEEL(LP,LQ))*VALI
          END IF
          DO J=1,NWORD
C           M3 =LABEL1(1,J)
            M4 =LABEL1(2,J)
            LR =LABEL1(3,J)
            LS =LABEL1(4,J)
            VALJ=WORKGE(J)
            IF(KREF(M2) .AND. KREF(M4)) THEN
              HMAT(M2,M4)=HMAT(M2,M4)
     *          +0.5D+00*VTWOEL(LIJMO(LQ,LP),LIJMO(LR,LS))*VALI*VALJ
            END IF
          END DO
        END DO
C        END IF
        NKFT0M=NKFT0M+1
        NWFT0M=NWFT0M+NWORD
C      IF(KCONT.NE.0) GO TO 200
      END DO
      IF (GOPARR) CALL DDI_GSUMF(2525,HMAT,MAXDIA*MAXDIA)
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** HAMILTONIAN MATRIX ***'')')
        NLINE=MIN(10,NCI)
        CALL MQWMAG(LUNOUT,HMAT,MAXDIA,NLINE,NLINE,'    ')
      END IF
C**** DIAGONALIZATION OF HAMILTONIAN ***********************************
      N=0
      DO I=1,NCI
        DO J=1,I
          N=N+1
          DIAHMA(N)=HMAT(I,J)
        END DO
      END DO
      NCITRI=NCI*(NCI+1)/2
C     CALL MQDIAG(NCI   ,NCITRI,MAXDIA,ICON  ,
C    *            DIAHMA,DIAVAL,DIAVEC,DIAWRK)
      CALL EVVRSP(LUNOUT,NCI   ,NCI   ,NCITRI,MAXDIA,DIAHMA,
     *            DIAWRK,LORDER,DIAVAL,DIAVEC,0     ,ICON  )
C**** CHECK DIAGONALIZATION ********************************************
      IF(ICON.NE.0) THEN
        IF (MASWRK)
     *   WRITE(LUNOUT,'('' *** ERROR STOP IN SUB.MQCACI ***''/
     *    '' THE DIAGONALIZATION HAS FAILED.''/
     *    '' ICON ='',I3)') ICON
        CALL ABRT
      END IF
C**** REORDER SOLUTIONS ************************************************
      DO I=1,NCI
        LORDER(I)=I
      END DO
C     NM1=NCI-1
C     DO I=1,NM1
C       IP1=I+1
C       DO J=IP1,NCI
C         IF(DIAVAL(LORDER(I)).GT.DIAVAL(LORDER(J))) THEN
C           K        =LORDER(I)
C           LORDER(I)=LORDER(J)
C           LORDER(J)=K
C         END IF
C       END DO
C     END FO
C@@@@ PICK UP MAXIMUM OVERLAP SOLUTIONS OR NOT @@@@@@@@@@@@@@@@@@@@@@@@@
      IF(MAXOVL.EQ.0) THEN
C**** GET SOLUTIONS FOR REQUIRED SYMMETRY ******************************
        IF(MAINCS(1).NE.0) THEN
          DO I=1,NSTCI
            DO J=I,NCI
              IF(ABS(DIAVEC(MAINCS(1),LORDER(J))).GE.1.0D-08 .OR.
     *          ABS(DIAVEC(MAINCS(2),LORDER(J))).GE.1.0D-08 .OR.
     *          ABS(DIAVEC(MAINCS(3),LORDER(J))).GE.1.0D-08) THEN
                K        =LORDER(I)
                LORDER(I)=LORDER(J)
                LORDER(J)=K
                GO TO 240
              END IF
            END DO
            IF (MASWRK)
     *        WRITE(LUNOUT,'('' *** ATTENTION IN SUB.MQCACI ***''/
     *        '' SOLUTION FOR REQUIRED SYMMETRY WAS NOT FOUND.''/
     *        '' STATE ='',I3)') I
 240        CONTINUE
          END DO
        END IF
C**** SAVE THE SOLUTIONS ***********************************************
        N=0
        DO I=1,NSTCI
          IF(KSTATE(I)) THEN
            N=N+1
            IF(N.GT.NSTATE) THEN
              GO TO 260
            END IF
            J=LORDER(I)
            EIGVAL(N,0)=DIAVAL(J)+ESUM
            EIGVAL(N,1)=0.0D+00
            EIGVAL(N,2)=0.0D+00
            DO K=1,NCI
              EIGVEC(K,N)=DIAVEC(K,J)
            END DO
          END IF
        END DO
 260    CONTINUE
        IF(N.NE.NSTATE) THEN
          IF (MASWRK)
     *     WRITE(LUNOUT,'('' *** ERROR STOP IN SUB.MQCACI ***''/
     *      '' INPUT KSTATE(*) IS INVALID.''/
     *      '' N ='',I3,''  NSTATE ='',I3)') N,NSTATE
          CALL ABRT
        END IF
C**** DEBUG OUTPUT *****************************************************
C     IF(LPOUT.LT.0) THEN
C       NWRITE=MIN(NSTATE,20)
C       WRITE(LUNOUT,100)
C       WRITE(LUNOUT,172)
C 172   FORMAT(' *** ENERGY INFORMATION ***')
C       CALL MQWMAG(LUNOUT,EIGVAL,1,1,NWRITE,'    ')
C        IF(LPOUT.LE.-5) THEN
C       WRITE(LUNOUT,172)
C172     FORMAT(' *** EIGENVECTOR INFORMATION ***')
C108   FORMAT(' *** AVERAGE COEFFICIENT ***'/
C  1         ' AVECOE = ',10(F7.3))
C     NLINE=MIN(10,MAXDIA)
C     CALL MQMXWR(LUNOUT,MAXDIA,NLINE,NLINE,
C    *            LABEL1,
C    *            EIGVEC,WORK )
C     END IF
C@@@@ PICK UP MAXIMUM OVERLAP SOLUTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      ELSE
        IF (MASWRK) WRITE(LUNOUT,'('' *** MAXIMUM OVERLAP SORT ***''/
     *    ''      STATE     ORDER   OVERLAP'')')
        DO ISTATE=1,NSTATE
          SMAX=0.0D+00
          DO ISOLUT=1,NSOLUT
            S=0.0D+00
            DO I=1,NCI
              S=S+EIGVOD(I,ISTATE)*DIAVEC(I,ISOLUT)
            END DO
            S=ABS(S)
            IF(S.GE.SMAX) THEN
              SMAX=S
              LORDER(ISTATE)=ISOLUT
            END IF
          END DO
          IF (MASWRK) WRITE(LUNOUT,
     *                '(1X,2I10,1P,D10.3)') ISTATE,LORDER(ISTATE),SMAX
        END DO
        DO I=1,NSTATE
          J=LORDER(I)
          EIGVAL(I,0)=DIAVAL(J)+ESUM
          EIGVAL(I,1)=0.0D+00
          EIGVAL(I,2)=0.0D+00
          DO K=1,NCI
            EIGVEC(K,I)=DIAVEC(K,J)
          END DO
        END DO
        DO I=1,NSTATE
          DO J=I+1,NSTATE
            IF(LORDER(I).EQ.LORDER(J)) THEN
              IF (MASWRK)THEN
              WRITE(LUNOUT,'('' *** ERROR STOP IN SUB.MQCACI ***''/
     *          '' MAXIMUM OVERLAP SORTING WAS FAILED.''/
     *          '' *** MATRIX LORDER ***'')')
              CALL MQWMAI(LUNOUT,LORDER,1,1,NSTATE,'    ')
              CALL ABRT
              END IF
            END IF
          END DO
        END DO
      END IF
C**** GO TO WRITE-RESULTS **********************************************
      GO TO 990
C***********************************************************************
C****                             **************************************
C**** OUT OF CORE DIAGONALIZATION **************************************
C****                             **************************************
C**** GET DIAGONAL ELEMENTS ********************************************
 300  CONTINUE
      IF (CACIOP.EQ.0) CALL SEQREW(LUNFT0)
      NKFT0M=1
      NWFT0M=1
      DO INWFT0=1,NWFT0
C 310  CONTINUE
        IF (CACIOP.EQ.0) THEN
           READ(LUNFT0) KCONT,NWORD
           CALL MQGENR(LUNFT0,4*NWORD,NWORD,LABEL1,WORKGE)
        ELSE
           KCONT=KFT0M(1,NKFT0M)
           NWORD=KFT0M(2,NKFT0M)
        END IF
C        IF (.NOT.GOPARR.OR.MOD(NKFT0M,NPROC).EQ.ME) THEN
        IF (CACIOP.EQ.1) THEN
           CALL DCOPY(4*NWORD/NWDVAR,LFT0M(1,NWFT0M),1,LABEL1,1)
           CALL DCOPY(NWORD,WFT0M(NWFT0M),1,WORKGE,1)
        END IF
        DO I=1,NWORD
          M1 =LABEL1(1,I)
          M2 =LABEL1(2,I)
          LP =LABEL1(3,I)
          LQ =LABEL1(4,I)
          VALI=WORKGE(I)
C@@@@ MOD START @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
          IF(M1.EQ.M2 .AND. KREF(M1)) THEN
            HMATDI(M1)=HMATDI(M1)+(HCORE(LP,LQ)+VONEEL(LP,LQ))*VALI
          END IF
C@@@@ END @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
          DO J=1,NWORD
C           M3 =LABEL1(1,J)
            M4 =LABEL1(2,J)
            LR =LABEL1(3,J)
            LS =LABEL1(4,J)
            VALJ=WORKGE(J)
            IF(M2.EQ.M4 .AND. KREF(M2)) THEN
              HMATDI(M2)=HMATDI(M2)
     *          +0.5D+00*VTWOEL(LIJMO(LQ,LP),LIJMO(LR,LS))*VALI*VALJ
            END IF
          END DO
        END DO
C        END IF
        NKFT0M=NKFT0M+1
        NWFT0M=NWFT0M+NWORD
C      IF(KCONT.NE.0) GO TO 310
      END DO
      IF (GOPARR) CALL DDI_GSUMF(2526,HMATDI,NCI)
C
      NREAD=NREAD+1
C**** REORDER DIAGONAL ELEMENTS ****************************************
      DO I=1,NCI
        LNWOLD(I)=I
      END DO
      DO I=1,MDI
        IP1=I+1
        DO J=IP1,NCI
          IF(HMATDI(LNWOLD(I)).GT.HMATDI(LNWOLD(J))) THEN
            K        =LNWOLD(I)
            LNWOLD(I)=LNWOLD(J)
            LNWOLD(J)=K
          END IF
        END DO
      END DO
      DO I=1,NCI
        LODNEW(LNWOLD(I))=I
      END DO
C**** GET SMALL HAMILTONIAN ********************************************
      IF (CACIOP.EQ.0) CALL SEQREW(LUNFT0)
      NKFT0M=1
      NWFT0M=1
      DO INWFT0=1,NWFT0
C 350  CONTINUE
        IF (CACIOP.EQ.0) THEN
           READ(LUNFT0) KCONT,NWORD
           CALL MQGENR(LUNFT0,4*NWORD,NWORD,LABEL1,WORKGE)
        ELSE
           KCONT=KFT0M(1,NKFT0M)
           NWORD=KFT0M(2,NKFT0M)
        END IF
C        IF (.NOT.GOPARR.OR.MOD(NKFT0M,NPROC).EQ.ME) THEN
        IF (CACIOP.EQ.1) THEN
           CALL DCOPY(4*NWORD/NWDVAR,LFT0M(1,NWFT0M),1,LABEL1,1)
           CALL DCOPY(NWORD,WFT0M(NWFT0M),1,WORKGE,1)
        END IF
        DO I=1,NWORD
          M1 =LABEL1(1,I)
          M2 =LABEL1(2,I)
          LP =LABEL1(3,I)
          LQ =LABEL1(4,I)
          VALI=WORKGE(I)
          M1NEW=LODNEW(M1)
          M2NEW=LODNEW(M2)
          IF(M1NEW.LE.MDI .AND. M2NEW.LE.MDI .AND.
     *      KREF(M1) .AND. KREF(M2)) THEN
            HMAT(M1NEW,M2NEW)=HMAT(M1NEW,M2NEW)
     *        +(HCORE(LP,LQ)+VONEEL(LP,LQ))*VALI
          END IF
          IF(M2NEW.LE.MDI .AND. KREF(M2)) THEN
            DO J=1,NWORD
C             M3 =LABEL1(1,J)
              M4 =LABEL1(2,J)
              LR =LABEL1(3,J)
              LS =LABEL1(4,J)
              VALJ=WORKGE(J)
              M4NEW=LODNEW(M4)
              IF(M4NEW.LE.MDI .AND. KREF(M4)) THEN
                HMAT(M2NEW,M4NEW)=HMAT(M2NEW,M4NEW)
     *            +0.5D+00*VTWOEL(LIJMO(LQ,LP),LIJMO(LR,LS))*VALI*VALJ
              END IF
            END DO
          END IF
        END DO
C        END IF
        NKFT0M=NKFT0M+1
        NWFT0M=NWFT0M+NWORD
C      IF(KCONT.NE.0) GO TO 350
      END DO
      IF (GOPARR) CALL DDI_GSUMF(2527,HMAT,MAXDIA*MDI)
      NREAD=NREAD+1
C**** STORE HMAT INTO DIAHMA *******************************************
      N=0
      DO I=1,MDI
        DO J=1,I
          N=N+1
          DIAHMA(N)=HMAT(I,J)
        END DO
      END DO
C**** DIAGONALIZATION OF DIAHMA ****************************************
      MDITRI=MDI*(MDI+1)/2
C     CALL MQDIAG(MDI,MDITRI,MAXDIA,ICON,DIAHMA,DIAVAL,DIAVEC,DIAWRK)
      CALL EVVRSP(LUNOUT,MDI   ,MDI   ,MDITRI,MAXDIA,DIAHMA,
     *            DIAWRK,LORDER,DIAVAL,DIAVEC,0     ,ICON  )
C**** ERROR STOP *******************************************************
      IF(ICON.NE.0) THEN
        IF(MASWRK) WRITE(LUNOUT,'('' *** ERROR STOP IN SUB.MQCACI ***''/
     *    '' DIAGONALIZATION FAILED.''/
     *    '' ICON ='',I3)') ICON
        CALL ABRT
      END IF
C**** REORDER SOLUTIONS ************************************************
      DO I=1,MDI
        LORDER(I)=I
      END DO
C     MM1=MDI-1
C     DO I=1,MM1
C       IP1=I+1
C       DO J=IP1,MDI
C         IF(DIAVAL(LORDER(I)).GT.DIAVAL(LORDER(J))) THEN
C           K        =LORDER(I)
C           LORDER(I)=LORDER(J)
C           LORDER(J)=K
C         END IF
C       END DO
C     END DO
C**** DEBUG OUTPUT *****************************************************
C     IF(LPOUT.LE.-10 .AND. MASWRK) THEN
C       WRITE(LUNOUT,1000)
C 194    FORMAT(' *** DEBUG OUTPUT IN SUB.MQCACI ***')
C       DO 144 I=1,NSOLUT
C 144   VOLWRK(I)=EGNVAL(I)+ESUM
C       WRITE(LUNOUT,1010) MDI,(I,VOLWRK(I),I=1,NSOLUT)
C1010   FORMAT(' *** EIGENVALUES OF INITIAL VECTORS (MDI =',I5,")"
C    1         (1X,5(I3,F12.6)))
C     END IF
C**** SAVE THE VECTORS INTO BASE ***************************************
      DO J=1,NSOLUT
        J1=LORDER(J)
        DO I=1,MDI
          I1=LNWOLD(I)
          BASE(I1,J)=DIAVEC(I,J1)
        END DO
      END DO
      MDI1=MDI+1
      DO J=1,NSOLUT
        DO I=MDI1,NCI
          I1=LNWOLD(I)
          BASE(I1,J)=0.0D+00
        END DO
      END DO
C**** V -> HV **********************************************************
      DO ISOLUT=1,NSOLUT
        DO I=1,NCI
          HXBASE(I,ISOLUT)=0.0D+00
        END DO
      END DO
      IF (CACIOP.EQ.0) CALL SEQREW(LUNFT0)
      NKFT0M=1
      NWFT0M=1
      DO INWFT0=1,NWFT0
C 400  CONTINUE
        IF (CACIOP.EQ.0) THEN
           READ(LUNFT0) KCONT,NWORD
           CALL MQGENR(LUNFT0,4*NWORD,NWORD,LABEL1,WORKGE)
        ELSE
           KCONT=KFT0M(1,NKFT0M)
           NWORD=KFT0M(2,NKFT0M)
        END IF
C        IF (.NOT.GOPARR.OR.MOD(NKFT0M,NPROC).EQ.ME) THEN
        IF (CACIOP.EQ.1) THEN
           CALL DCOPY(4*NWORD/NWDVAR,LFT0M(1,NWFT0M),1,LABEL1,1)
           CALL DCOPY(NWORD,WFT0M(NWFT0M),1,WORKGE,1)
        END IF
        DO I=1,NWORD
          M1 =LABEL1(1,I)
          M2 =LABEL1(2,I)
          LP =LABEL1(3,I)
          LQ =LABEL1(4,I)
          VALI=WORKGE(I)
          V=(HCORE(LP,LQ)+VONEEL(LP,LQ))*VALI
          IF(KREF(M1) .AND. KREF(M2)) THEN
            DO ISOLUT=1,NSOLUT
              HXBASE(M1,ISOLUT)=HXBASE(M1,ISOLUT)+V*BASE(M2,ISOLUT)
C             HMAT(M1,M2)=HMAT(M1,M2)+(HCORE(LP,LQ)+VONEEL(LP,LQ))*VAL
            END DO
          END IF
          DO J=1,NWORD
C           M3 =LABEL1(1,J)
            M4 =LABEL1(2,J)
            LR =LABEL1(3,J)
            LS =LABEL1(4,J)
            VALJ=WORKGE(J)
            V=0.5D+00*VTWOEL(LIJMO(LQ,LP),LIJMO(LR,LS))*VALI*VALJ
            IF(KREF(M2) .AND. KREF(M4)) THEN
              DO ISOLUT=1,NSOLUT
                HXBASE(M2,ISOLUT)=HXBASE(M2,ISOLUT)+V*BASE(M4,ISOLUT)
              END DO
C               HMAT(M2,M4)=HMAT(M2,M4)
C    *         +0.5D+00*VTWOEL(LIJMO(LQ,LP),LIJMO(LR,LS))*VALI*VALJ
            END IF
          END DO
        END DO
C        END IF
        NKFT0M=NKFT0M+1
        NWFT0M=NWFT0M+NWORD
C      IF(KCONT.NE.0) GO TO 400
      END DO
      IF (GOPARR) CALL DDI_GSUMF(2528,HXBASE,NCI*NSOLUT)
C     DO 166 K=1,NWORD
C     I=LABEL(1,K)
C     J=LABEL(2,K)
C     S=WORK   (K)
C     DO 166 L=1,NSOLUT
C     HXBASE(I,L)=HXBASE(I,L)+S*BASE(J,L)
C     166 HXBASE(J,L)=HXBASE(J,L)+S*BASE(I,L)
C     IF(KCONT.NE.0) GO TO 164
      NREAD=NREAD+1
C**** SET PARAMETERS ***************************************************
      NOWBAS=NSOLUT
      NREMAI=NSOLUT
      NRECON=0
C**** SET LKCONV AND HMAT **********************************************
      DO I=1,NSOLUT
        LKCONV(I)=0
      END DO
      DO I=1,NSOLUT
        I1=LORDER(I)
        DO J=1,I
          IF(J.LT.I) THEN
            S=0.0D+00
          ELSE
            S=DIAVAL(I1)
          END IF
          HMAT(I,J)=S
          HMAT(J,I)=S
        END DO
        EBEFOR(I)=DIAVAL(I1)
        DO J=1,NSOLUT
          IF(J.NE.I) THEN
            DIAVEC(J,I1)=0.0D+00
          ELSE
            DIAVEC(J,I1)=1.0D+00
          END IF
        END DO
      END DO
C***********************************************************************
C**** CHECK SPACE FOR NEW BASES ****************************************
C****                           ****************************************
C**** RE-CONSTRUCTION OF BASES IF THERE IS NO STORAGE SPACE ************
 500  CONTINUE
      IF((NOWBAS+NREMAI).GT.MXBASE) THEN
        NRECON=NRECON+1
C.... ERROR STOP .......................................................
        IF(NRECON.GE.NSTOP) THEN
          IF (MASWRK)
     *     WRITE(LUNOUT,'('' *** ERROR STOP IN SUB.MQCACI ***''/
     *      '' NSTOP  ='',I3,'' NOWBAS ='',I3)') NSTOP,NOWBAS
          DO ISOLUT=1,NSOLUT
            I1=LORDER(ISOLUT)
            ENERGY=DIAVAL(I1)+ESUM
            IF (MASWRK) WRITE(LUNOUT,'('' ENERGY('',I3,'') ='',F18.10)')
     *        ISOLUT,ENERGY
          END DO
          CALL ABRT
        END IF
        IF((NSOLUT+NREMAI).GT.MXBASE) THEN
          IF (MASWRK)
     *     WRITE(LUNOUT,'('' *** ERROR STOP IN SUB.MQCACI ***''/
     *      '' MXBASE DEFINED AT SUB.MQCACI MUST NOT BE LESS'',
     *      '' THAN TWICE OF NSOLUT.''/
     *      '' NSOLUT  ='',I10,'' MXBASE ='',I10)') NSOLUT,MXBASE
          CALL ABRT
        END IF
C.... SAVE NEWEST VECTOR ...............................................
        DO I=1,NCI
          DO J=1,NSOLUT
            J1=LORDER(J)
            S=0.0D+00
            DO K=1,NOWBAS
              S=S+BASE(I,K)*DIAVEC(K,J1)
            END DO
            WRKVEC(J)=S
          END DO
          DO J=1,NSOLUT
            BASE(I,J)=WRKVEC(J)
          END DO
        END DO
C.... SAVE NEWEST VECTOR MULTIPLIED BY H-MATRIX ........................
        DO I=1,NCI
          DO J=1,NSOLUT
            J1=LORDER(J)
            S=0.0D+00
            DO K=1,NOWBAS
              S=S+HXBASE(I,K)*DIAVEC(K,J1)
            END DO
            WRKVEC(J)=S
          END DO
          DO J=1,NSOLUT
            HXBASE(I,J)=WRKVEC(J)
          END DO
        END DO
C.... SAVE NEWEST HMAT .................................................
        DO I=1,NSOLUT
          I1=LORDER(I)
          DO J=1,I
            IF(J.LT.I) THEN
              S=0.0D+00
            ELSE
              S=DIAVAL(I1)
            END IF
            HMAT(I,J)=S
            HMAT(J,I)=S
          END DO
          DO J=1,NSOLUT
            IF(J.NE.I) THEN
              DIAVEC(J,I1)=0.0D+00
            ELSE
              DIAVEC(J,I1)=1.0D+00
            END IF
          END DO
        END DO
C.... RESET PARAMETERS .................................................
        NOWBAS=NSOLUT
C.... PRINT OUT ATTENTION ..............................................
        IF(LPOUT.LE.-10 .AND. MASWRK) THEN
          WRITE(LUNOUT,'('' *** ATTENTION IN SUB.MQCACI ***''/
     *      '' ***IINITIAL BASES ARE RE-CONSTRUCTED. ***'')')
        END IF
      END IF
C***********************************************************************
C**** CONSTRUCTION OF NEW BASES ****************************************
C****                           ****************************************
      NEWBAS=NOWBAS
      NEWBAI=NOWBAS+1
      DO ISOLUT=1,NSOLUT
        IF(LKCONV(ISOLUT).LE.0) THEN
C**** CALCULATION OF DIFFERENCE BETWEEN HXBASE AND BASE ****************
          I1=LORDER(ISOLUT)
          EIGENV=DIAVAL(I1)
          VNORM=0.0D+00
          DO I=1,NCI
            X=0.0D+00
            DO J=1,NOWBAS
              X=X+(HXBASE(I,J)-EIGENV*BASE(I,J))*DIAVEC(J,I1)
            END DO
            VNORM=VNORM+X*X
            WRKVEC(I)=X
          END DO
          VNORM=SQRT(VNORM)
C**** DEBUG OUTPUT *****************************************************
          IF(LPOUT.LE.-5 .AND. MASWRK) THEN
            ENERGY=EIGENV+ESUM
            WRITE(LUNOUT,'('' ENERGY('',I3,'') ='',F16.10)')
     *        ISOLUT,ENERGY
C           WRITE(LUNOUT,'('' NOWBAS  ='',I5/
C    *        ''  ENERGY('',I3,'') ='',F16.10/
C    *        ''  VNORM ('',I3,'') ='',F16.10)')
C    *        NOWBAS,ISOLUT,ENERGY,ISOLUT,VNORM
          END IF
C**** CHECK THE CONVERGENCE ********************************************
          IF(VNORM.LT.THRCON) THEN
            LKCONV(ISOLUT)=1
            NREMAI=NREMAI-1
C.... GOTO WRITE RESULT ................................................
            IF(NREMAI.LE.0) GO TO 700
          ELSE
C***********************************************************************
C**** ADD NEW BASE *****************************************************
C****              *****************************************************
C**** DIVIDE BY THE ENERGY DIFFERENCE **********************************
            DO I=1,NCI
              X=EIGENV-HMATDI(I)
              IF(ABS(X).GE.THRZRO) THEN
                WRKVEC(I)=WRKVEC(I)/X
              ELSE
                WRKVEC(I)=0.0D+00
              END IF
            END DO
C**** SCHMIDT ORTHOGONALIZATION ****************************************
            DO I=NEWBAS,1,-1
              S=0.0D+00
              DO J=1,NCI
                S=S+BASE(J,I)*WRKVEC(J)
              END DO
              DO K=1,NCI
                WRKVEC(K)=WRKVEC(K)-BASE(K,I)*S
              END DO
            END DO
C**** NORMALIZATION ****************************************************
            VNORM=0.0D+00
            DO I=1,NCI
              VNORM=VNORM+WRKVEC(I)*WRKVEC(I)
            END DO
            VNORM=SQRT(VNORM)
            IF(VNORM.GE.1.0D+02*THRZRO) THEN
              NEWBAS=NEWBAS+1
              VNORM=1.0D+00/VNORM
              DO I=1,NCI
                BASE(I,NEWBAS)=WRKVEC(I)*VNORM
              END DO
            END IF
          END IF
        END IF
      END DO
      IF(NEWBAS.LE.NOWBAS) GO TO 700
      NOWBAS=NEWBAS
C***********************************************************************
C**** MULTIPLICATION OF H-MATRIX BY NEW BASES **************************
C****                                         **************************
C**** V -> HV **********************************************************
      DO ISOLUT=NEWBAI,NEWBAS
        DO I=1,NCI
          HXBASE(I,ISOLUT)=0.0D+00
        END DO
      END DO
      IF (CACIOP.EQ.0) CALL SEQREW(LUNFT0)
      NKFT0M=1
      NWFT0M=1
      DO INWFT0=1,NWFT0
C 600  CONTINUE
        IF (CACIOP.EQ.0) THEN
           READ(LUNFT0) KCONT,NWORD
           CALL MQGENR(LUNFT0,4*NWORD,NWORD,LABEL1,WORKGE)
        ELSE
           KCONT=KFT0M(1,NKFT0M)
           NWORD=KFT0M(2,NKFT0M)
        END IF
C        IF (.NOT.GOPARR.OR.MOD(NKFT0M,NPROC).EQ.ME) THEN
        IF (CACIOP.EQ.1) THEN
           CALL DCOPY(4*NWORD/NWDVAR,LFT0M(1,NWFT0M),1,LABEL1,1)
           CALL DCOPY(NWORD,WFT0M(NWFT0M),1,WORKGE,1)
        END IF
        DO I=1,NWORD
          M1 =LABEL1(1,I)
          M2 =LABEL1(2,I)
          LP =LABEL1(3,I)
          LQ =LABEL1(4,I)
          VALI=WORKGE(I)
          V=(HCORE(LP,LQ)+VONEEL(LP,LQ))*VALI
          IF(KREF(M1) .AND. KREF(M2)) THEN
            DO ISOLUT=NEWBAI,NEWBAS
              HXBASE(M1,ISOLUT)=HXBASE(M1,ISOLUT)+V*BASE(M2,ISOLUT)
            END DO
          END IF
          DO J=1,NWORD
C           M3 =LABEL1(1,J)
            M4 =LABEL1(2,J)
            LR =LABEL1(3,J)
            LS =LABEL1(4,J)
            VALJ=WORKGE(J)
            V=0.5D+00*VTWOEL(LIJMO(LQ,LP),LIJMO(LR,LS))*VALI*VALJ
            IF(KREF(M2) .AND. KREF(M4)) THEN
              DO ISOLUT=NEWBAI,NEWBAS
                HXBASE(M2,ISOLUT)=HXBASE(M2,ISOLUT)+V*BASE(M4,ISOLUT)
              END DO
            END IF
          END DO
        END DO
C        END IF
        NKFT0M=NKFT0M+1
        NWFT0M=NWFT0M+NWORD
C      IF(KCONT.NE.0) GO TO 600
      END DO
      IF (GOPARR)
     *     CALL DDI_GSUMF(2524,HXBASE(1,NEWBAI),NCI*(NEWBAS-NEWBAI+1))
      NREAD=NREAD+1
C
C**** CALCULATION OF H-MATRIX WITH THE PRESENT BASES *******************
      DO I=NEWBAI,NEWBAS
        DO J=1,I
          X=0.0D+00
          DO K=1,NCI
            X=X+BASE(K,I)*HXBASE(K,J)
          END DO
          HMAT(I,J)=X
          HMAT(J,I)=X
        END DO
      END DO
C**** DIAGONALIZATION **************************************************
      DO I=1,NSOLUT
        I1=LORDER(I)
        EBEFOR(I)=DIAVAL(I1)
      END DO
      N=0
      DO I=1,NOWBAS
        DO J=1,I
          N=N+1
          DIAHMA(N)=HMAT(I,J)
        END DO
      END DO
      NOWTRI=NOWBAS*(NOWBAS+1)/2
C     CALL MQDIAG(NOWBAS,NOWTRI,MAXDIA,ICON,
C    *            DIAHMA,DIAVAL,DIAVEC,DIAWRK)
      CALL EVVRSP(LUNOUT,NOWBAS,NOWBAS,NOWTRI,MAXDIA,DIAHMA,
     *            DIAWRK,LORDER,DIAVAL,DIAVEC,0     ,ICON  )
C**** ERROR STOP *******************************************************
      IF(ICON.NE.0) THEN
        IF(MASWRK) WRITE(LUNOUT,'('' *** ERROR STOP IN SUB.MQCACI ***''/
     *    '' DIAGONALIZATION FAILED.''/
     *    '' ICON ='',I3)') ICON
        CALL ABRT
      END IF
C**** REORDER SOLUTIONS ************************************************
      DO I=1,NOWBAS
        LORDER(I)=I
      END DO
C     NM1=NOWBAS-1
C     DO I=1,NM1
C       IP1=I+1
C       DO J=IP1,NOWBAS
C         IF(DIAVAL(LORDER(I)).GT.DIAVAL(LORDER(J))) THEN
C           K        =LORDER(I)
C           LORDER(I)=LORDER(J)
C           LORDER(J)=K
C         END IF
C       END DO
C     END DO
C**** CHECK ENERGY LOWERING ********************************************
      DO I=1,NSOLUT
        IF(LKCONV(I).LE.0) THEN
          I1=LORDER(I)
          IF(ABS(DIAVAL(I1)-EBEFOR(I)).LT.THRENE) THEN
            LKCONV(I)=1
            NREMAI=NREMAI-1
            IF(NREMAI.LE.0) GO TO 700
          END IF
        END IF
      END DO
C**** GO TO DIAGONALIZATION ********************************************
      GO TO 500
C**** EIGENVECTORS *****************************************************
 700  CONTINUE
      DO I=1,NCI
        DO J=1,NSOLUT
          J1=LORDER(J)
          ENEWRK(J)=DIAVAL(J1)+ESUM
C
C     ENEWRK: EIGENVALUES
C
          S=0.0D+00
          DO K=1,NOWBAS
            S=S+BASE(I,K)*DIAVEC(K,J1)
          END DO
          WRKVEC(J)=S
        END DO
        DO J=1,NSOLUT
          BASE(I,J)=WRKVEC(J)
        END DO
      END DO
C**** EIGENVECTORS MULTIPLIED BY H-MATRIX ******************************
      DO I=1,NCI
        DO J=1,NSOLUT
          J1=LORDER(J)
          S=0.0D+00
          DO K=1,NOWBAS
            S=S+HXBASE(I,K)*DIAVEC(K,J1)
          END DO
          WRKVEC(J)=S
        END DO
        DO J=1,NSOLUT
          HXBASE(I,J)=WRKVEC(J)
        END DO
      END DO
C**** DIFFERENCE OF ENERGY*BASE-H*BASE *********************************
      DO I=1,NSOLUT
        I1=LORDER(I)
        E=DIAVAL(I1)
        S=0.0D+00
        DO J=1,NCI
          S=S+(HXBASE(J,I)-E*BASE(J,I))**2
        END DO
        S=SQRT(S)
        DIFVEC(I  )=S
      END DO
C**** EIGENVECTOR(S) INFORMATION ***************************************
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        NWRITE=MIN(NSOLUT,10)
        WRITE(LUNOUT,'('' *** CAS-CI ENERGIES ***'')')
        CALL MQWMAG(LUNOUT,ENEWRK,1,1,NWRITE,'    ')
        WRITE(LUNOUT,'('' *** EIGENVECTORS ***'')')
        NMAX=MIN(10,MAXDIA)
        CALL MQMXWR(LUNOUT,NCI,NSOLUT,NMAX,LABEL1,BASE,WORKER)
      END IF
C**** GET SOLUTIONS FOR REQUIRED SYMMETRY ******************************
      DO I=1,NSOLUT
        ENEWRK(I)=DIAVAL(LORDER(I))
        LORDER(I)=I
      END DO
C
C@@@@ PICK UP MAXIMUM OVERLAP SOLUTIONS OR NOT @@@@@@@@@@@@@@@@@@@@@@@@@
C
      IF(MAXOVL.EQ.0) THEN
C
      IF(MAINCS(1).NE.0) THEN
        DO I=1,NSTCI
          DO J=I,NSOLUT
            IF(ABS(BASE(MAINCS(1),LORDER(J))).GE.1.0D-08 .OR.
     *         ABS(BASE(MAINCS(2),LORDER(J))).GE.1.0D-08 .OR.
     *         ABS(BASE(MAINCS(3),LORDER(J))).GE.1.0D-08) THEN
              K        =LORDER(I)
              LORDER(I)=LORDER(J)
              LORDER(J)=K
              GO TO 800
            END IF
          END DO
          IF(MASWRK)
     *     WRITE(LUNOUT,'('' *** ATTENTION IN SUB.MQCACI ***''/
     *      '' SOLUTION FOR REQUIRED SYMMETRY WAS NOT FOUND.''/
     *      '' STATE ='',I3)') I
 800      CONTINUE
        END DO
      END IF
C
C**** EIGENVALUES AND EIGENVECTORS *************************************
C
      N=0
      DO I=1,NSTCI
        IF(KSTATE(I)) THEN
          N=N+1
          IF(N.GT.NSTATE) THEN
            GO TO 820
C           WRITE(LUNOUT,174)
C           STOP
          END IF
          I1=LORDER(I)
          EIGVAL(N,0)=ENEWRK(I1)+ESUM
          EIGVAL(N,1)=ENEWRK(I1)-EBEFOR(I1)
          EIGVAL(N,2)=DIFVEC(I1)
          DO ICSF=1,NCI
            EIGVEC(ICSF,N)=BASE(ICSF,I1)
          END DO
        END IF
      END DO
C
 820  CONTINUE
      IF(N.NE.NSTATE) THEN
        IF(MASWRK) WRITE(LUNOUT,'('' *** ERROR STOP IN SUB.MQCACI ***''/
     *    '' INPUT KSTATE(*) IS INVALID.''/
     *    '' N ='',I3,''  NSTATE ='',I3)') N,NSTATE
        CALL ABRT
      END IF
C
C@@@@ PICK UP MAXIMUM OVERLAP SOLUTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
      ELSE
        IF (MASWRK) WRITE(LUNOUT,'('' *** MAXIMUM OVERLAP SORT ***''/
     *                 ''      STATE     ORDER   OVERLAP'')')
        DO ISTATE=1,NSTATE
          SMAX=0.0D+00
          DO ISOLUT=1,NSOLUT
            S=0.0D+00
            DO I=1,NCI
              S=S+EIGVOD(I,ISTATE)*BASE(I,ISOLUT)
            END DO
            S=ABS(S)
            IF(S.GE.SMAX) THEN
              SMAX=S
              LORDER(ISTATE)=ISOLUT
            END IF
          END DO
          IF (MASWRK)
     *     WRITE(LUNOUT,'(1X,2I10,1P,D10.3)') ISTATE,LORDER(ISTATE),SMAX
        END DO
        DO I=1,NSTATE
          I1=LORDER(I)
          EIGVAL(I,0)=ENEWRK(I1)+ESUM
          EIGVAL(I,1)=ENEWRK(I1)-EBEFOR(I1)
          EIGVAL(I,2)=DIFVEC(I1)
          DO ICSF=1,NCI
            EIGVEC(ICSF,I)=BASE(ICSF,I1)
          END DO
        END DO
        DO I=1,NSTATE
          DO J=I+1,NSTATE
            IF(LORDER(I).EQ.LORDER(J)) THEN
              IF (MASWRK)
     *         WRITE(LUNOUT,'('' *** ERROR STOP IN SUB.MQCACI ***''/
     *          '' MAXIMUM OVERLAP SORTING WAS FAILED.''/
     *          '' *** MATRIX LORDER ***'')')
              CALL MQWMAI(LUNOUT,LORDER,1,1,NSTATE,'    ')
              CALL ABRT
            END IF
          END DO
        END DO
      END IF
C
C****               ****************************************************
C**** WRITE RESULTS ****************************************************
C****               ****************************************************
C
 990  CONTINUE
      DO I=1,NSTATE
         AVGENG = AVGENG + AVECOE(I)*EIGVAL(I,0)
      END DO
C
C
      IF (MASWRK) WRITE(LUNOUT,9010) LUNFT0,NREAD
 9010 FORMAT(1X,'CAS-CI STATES CONVERGED, COUPLING COEFFICIENT FILE',I3,
     *          ' WAS READ',I4,' TIMES.')
C
C        ENERGIES AND STATES ARE NOW PRINTED AFTER CALL TO MQCACI
C
C---      WRITE(LUNOUT,9020)
C---      DO I=1,NSTATE
C---        WRITE(LUNOUT,9030) I,(EIGVAL(I,J),J=0,2)
C---      END DO
C--- 9020 FORMAT(6X,'**** CAS-CI ENERGIES ***'/
C---     *      19X,' ENERGY',7X,'DIF-ENE',7X,'DIF-VEC')
C--- 9030 FORMAT(1X,I5,F20.12,1P,D14.4,D14.4')
C
C---      NWRITE=MIN(NSTATE,10)
C---      WRITE(LUNOUT,9040)
C--- 9040 FORMAT(1X,'*** CAS-CI EIGENVECTORS ***')
C---      NMAX=MIN(10,NCI)
C---      CALL MQMXWR(LUNOUT,NCI,NSTATE,NMAX,LABEL1,EIGVEC,WORKER)
C
      RETURN
      END
C*MODULE MCQDPT  *DECK MQCHAI
      SUBROUTINE MQCHAI(LUNOUT,LPOUT,NORB,MAXROW,
     1                  NJ,NJSKP,K,L,Y,XBAR,YP,XP,CTYPE)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT INTEGER (A-Z)
      DIMENSION NJ(NORB+1), NJSKP(NORB+1), K(3,MAXROW), L(3,MAXROW)
      DIMENSION Y(3,MAXROW), XBAR(MAXROW)
      DIMENSION YP(3,MAXROW), XP(MAXROW)
      CHARACTER*4 CTYPE
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND .MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQCHAI ***'')')
        CALL MQDEBG(LUNOUT,
     *  '  NORB','MAXROW','     -','     -','     -','     -','     -',
     *     NORB , MAXROW ,      0 ,      0 ,      0 ,      0 ,      0 )
      END IF
C***********************************************************************
C**** CALCULATE THE YP(*,*) CHAINING INDICES ***************************
C**** FOR THE FORWARD WEIGHTS                ***************************
C****                                        ***************************
C**** ORBITAL CONFIGURATION OR SPIN FUNCTION ***************************
      IF(CTYPE.NE.'SPIN') THEN
        MAXCAS=3
      ELSE
        MAXCAS=2
      END IF
C**** INITIALIZE THE BOTTOM ROW ****************************************
      XP(1)=1
      YP(1,1)=0
      YP(2,1)=0
      YP(3,1)=0
C**** LOOP OVER HIGHER WEIGHT ******************************************
      DO 200 LEV=1,NORB
      LEVP1=LEV+1
      IROW=NJSKP(LEVP1)
      NUMROW=NJ(LEVP1)
      DO 200 I=1,NUMROW
      IROW=IROW+1
C**** YP(*,*)=0 FOR ALL VERTEICES **************************************
      YP(1,IROW)=0
C**** LOOP OVER THE OTHER CASE NUMBERS *********************************
      DO 100 ICASE=2,MAXCAS
      YP(ICASE,IROW)=YP(ICASE-1,IROW)
      KT=K(ICASE-1,IROW)
      IF(KT.NE.0) YP(ICASE,IROW)=YP(ICASE,IROW)+XP(KT)
  100 CONTINUE
      XP(IROW)=YP(MAXCAS,IROW)
      KT=K(MAXCAS,IROW)
      IF(KT.NE.0) XP(IROW)=XP(IROW)+XP(KT)
  200 CONTINUE
C***********************************************************************
C**** CALCULATE THE REVERSE WEIGHTS Y(*,*) *****************************
C****                                      *****************************
C**** INITIALIZE THE TOP ROW *******************************************
      NROW=NJSKP(NORB+1)+1
      XBAR(NROW)=1
      Y(3,NROW)=0
      Y(2,NROW)=0
      Y(1,NROW)=0
C**** LOOP OVER ALL THE LOWER ROWS *************************************
      DO 400 LEV=NORB,1,-1
      IROW=NJSKP(LEV)
      NUMROW=NJ(LEV)
      DO 400 I=1,NUMROW
      IROW=IROW+1
C**** Y(MAXCAS,*) ARE ALWAYS ZERO **************************************
      Y(MAXCAS,IROW)=0
C**** LOOP OVER THE REMAINING CASE NUMBER ******************************
      DO 300 ICASE=MAXCAS-1,1,-1
      Y(ICASE,IROW)=Y(ICASE+1,IROW)
      LB=L(ICASE+1,IROW)
      IF(LB.NE.0) Y(ICASE,IROW)=Y(ICASE,IROW)+XBAR(LB)
  300 CONTINUE
      XBAR(IROW)=Y(1,IROW)
      LB=L(1,IROW)
      IF(LB.NE.0) XBAR(IROW)=XBAR(IROW)+XBAR(LB)
  400 CONTINUE
C**** RETURN AND END ***************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQCIG1
      SUBROUTINE MQCIG1(LUNOUT,LPOUT ,LUNFTP,MAXCSF,NDOUB ,
     *                  NORB  ,NOCF  ,MAXSOC,MAXSPF,NCSF  ,
     *                  NOCFI ,NELM  ,
     *                  IOMAP ,ISMAP ,IOCSF ,LAB1  ,WORK  ,
     *                  I1EX1 ,I1EX2 ,NSNSF )
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
C
C     NOTE:
C
C       CLASSIFICATION IN WRITING ON FTP-FILE
C
C                       I
C         NTYPE=M : <M/E /M'>
C                       J
C
C       THE VALUE OF KCONT
C
C         KCONT= 0 : THE END OF RECORDS
C         KCONT= 1 : ELSE
C
C**** DECLARATION ******************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER   IOMAP(NDOUB+1:NORB+1,NOCF), ISMAP(MAXSOC+1,MAXSPF)
      INTEGER   IOCSF(2,NCSF)
      INTEGER   LAB1(4,MAXCSF)
      DIMENSION WORK(MAXCSF)
      INTEGER   I1EX1(NDOUB+1:NORB,0:NOCF)
      INTEGER   I1EX2(NDOUB+1:NORB,0:NOCFI)
      INTEGER   NSNSF(NOCF+1)
      DIMENSION IDELB(4)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /MQFT02/NWFT0,NWFT0P
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DATA IDELB/0,1,-1,0/
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQCIG1 ***'')')
        CALL MQDEBG(LUNOUT,
     *  'LUNFTP','MAXCSF',' NDOUB','  NORB','  NOCF','MAXSOC','MAXSPF',
     *   LUNFTP , MAXCSF ,  NDOUB ,   NORB ,   NOCF , MAXSOC , MAXSPF )
        CALL MQDEBG(LUNOUT,
     *  '  NCSF',' NOCFI','     -','     -','     -','     -','     -',
     *     NCSF ,  NOCFI ,      0 ,      0 ,      0 ,      0 ,      0 )
      END IF
C     DO 1 I=1,NOCF
C       WRITE(LUNOUT,*) I,(IOMAP(J,I),J=NDOUB+1,NORB)
C   1 CONTINUE
C     DO 2 I=1,MAXSPF
C       WRITE(LUNOUT,*) I,(ISMAP(J,I),J=1,MAXSOC)
C   2 CONTINUE
C     DO 3 I=1,NCSF
C       WRITE(LUNOUT,*) I,IOCSF(1,I),IOCSF(2,I)
C   3 CONTINUE
C
      NWFT0=0
      NWFT0P=0
      INWFT0=0
C
C***********************************************************************
C****           ********************************************************
C****   START   ********************************************************
C****           ********************************************************
      NELM=0
      NMODP1=NDOUB+1
C**** SET M' AND M *****************************************************
      DO 118 MDUM=1,NCSF
        IOCFD=IOCSF(1,MDUM)
        ISFD =IOCSF(2,MDUM)
        KCONT=10
        NWRITE=0
        NTYPE=MDUM
C***********************************************************************
C****                         ******************************************
C****   CASE M'=M (NTYPE=1)   ******************************************
C****                         ******************************************
      M=MDUM
      IOCF=IOCFD
      DO 102 I=NMODP1,NORB
        IF(IOMAP(I,IOCF).EQ.2) THEN
          NELM=NELM+1
          NWRITE=NWRITE+1
          IF(NWRITE.GT.MAXCSF) THEN
            IF(MASWRK) WRITE(LUNOUT,200) NWRITE,MAXCSF
  200       FORMAT(' *** ERROR STOP IN SUB.MQCIG1 ***'/
     *          1X,'NWRITE=',I15,' IS GREATER THAN MAXCSF=',I15)
            CALL ABRT
          END IF
          LAB1(1,NWRITE)=M
          LAB1(2,NWRITE)=M
          LAB1(3,NWRITE)=I
          LAB1(4,NWRITE)=I
          WORK(  NWRITE)=1.0D+00
C.... DEBUG OUTPUT .....................................................
          IF(LPOUT.LE.-100 .AND. MASWRK) THEN
            WRITE(LUNOUT,100) NTYPE,M,I,I,M,WORK(NWRITE)
  100       FORMAT(' NTYPE=',I5,'  <',I3,'/',I2,'+,',
     1                       I2,'-/',I3,'> =',F12.8)
          END IF
C.... END ..............................................................
        ELSE IF(IOMAP(I,IOCF).EQ.3) THEN
          NELM=NELM+1
          NWRITE=NWRITE+1
          IF(NWRITE.GT.MAXCSF) THEN
            IF(MASWRK) WRITE(LUNOUT,200) NWRITE,MAXCSF
            CALL ABRT
          END IF
          LAB1(1,NWRITE)=M
          LAB1(2,NWRITE)=M
          LAB1(3,NWRITE)=I
          LAB1(4,NWRITE)=I
          WORK(  NWRITE)=2.0D+00
C.... DEBUG OUTPUT .....................................................
          IF(LPOUT.LE.-100 .AND. MASWRK) THEN
            WRITE(LUNOUT,100) NTYPE,M,I,I,M,WORK(NWRITE)
          END IF
C.... END ..............................................................
        END IF
  102 CONTINUE
C***********************************************************************
C****                          *****************************************
C****   CASE M'/=M (NTYPE=2)   *****************************************
C****                          *****************************************
C@@@@ MODFICATION 1
      DO 900 ILAB=NMODP1,NORB
      DO 902 JLAB=NMODP1,NORB
      IOCFPD=I1EX2(ILAB,I1EX1(JLAB,IOCFD))
      IF(IOCFPD.EQ.0) GO TO 902
      NSFPD=NSNSF(IOCFPD+1)-NSNSF(IOCFPD)
      DO 116 ISFPD=1,NSFPD
      MPDUM=NSNSF(IOCFPD)+ISFPD
C     DO 116 MPDUM=1,NCSF
C       IOCFPD=IOCSF(1,MPDUM)
C       ISFPD =IOCSF(2,MPDUM)
C@@@@ END 1
        IF(MDUM.EQ.MPDUM) THEN
          GO TO 116
        ELSE IF(MDUM.GT.MPDUM) THEN
          M =MDUM
          IOCF=IOCFD
          ISF =ISFD
          MP=MPDUM
          IOCFP=IOCFPD
          ISFP =ISFPD
        ELSE
          M =MPDUM
          IOCF=IOCFPD
          ISF =ISFPD
          MP=MDUM
          IOCFP=IOCFD
          ISFP =ISFD
        END IF
C**** FIND BRANCH LEVEL ************************************************
      IB   =0
      NSOCP=MAXSOC-IOMAP(NORB+1,IOCFP)
      NSOC =MAXSOC-IOMAP(NORB+1,IOCF )
      DO 104 I=NMODP1,NORB
        LEV=I
        IOP=IOMAP(I,IOCFP)
        IF(IOP.EQ.1) THEN
          ICA1=1
        ELSE IF(IOP.EQ.2) THEN
          NSOCP=NSOCP+1
          IF(ISMAP(NSOCP,ISFP).EQ.2) THEN
            ICA1=2
          ELSE
            ICA1=3
          END IF
        ELSE
          ICA1=4
        END IF
        IO =IOMAP(I,IOCF )
        IF(IO.EQ.1) THEN
          ICA2=1
        ELSE IF(IO.EQ.2) THEN
          NSOC=NSOC+1
          IF(ISMAP(NSOC,ISF).EQ.2) THEN
            ICA2=2
          ELSE
            ICA2=3
          END IF
        ELSE
          ICA2=4
        END IF
        IF(ICA1.NE.ICA2) THEN
          LA1=LEV
          GO TO 106
        END IF
        IB=IB+IDELB(ICA2)
  104 CONTINUE
C.... ERROR STOP .......................................................
C***********************************************************************
C**** IN THE LOOP ******************************************************
C****             ******************************************************
  106 IB1=IB+IDELB(ICA1)
      IB2=IB+IDELB(ICA2)
      IDIF12=IB1-IB2
C%%%%
      B=IB2
      IF((ICA1.EQ.3).AND.(ICA2.EQ.1)) THEN
        F=1.0D+00
        GO TO 108
      ELSE IF((ICA1.EQ.4).AND.(ICA2.EQ.2)) THEN
        F=SQRT((B+1.0D+00)/B)
        GO TO 108
      ELSE IF((ICA1.EQ.2).AND.(ICA2.EQ.1)) THEN
        F=1.0D+00
        GO TO 108
      ELSE IF((ICA1.EQ.4).AND.(ICA2.EQ.3)) THEN
        F=SQRT((B+1.0D+00)/(B+2.0D+00))
        GO TO 108
      ELSE
        GO TO 116
      END IF
C  =    <<EQUAL ZERO>>
  108 LEVP1=LEV+1
      DO 110 I=LEVP1,NORB
        LEV=I
        IOP=IOMAP(I,IOCFP)
        IF(IOP.EQ.1) THEN
          ICA1=1
        ELSE IF(IOP.EQ.2) THEN
          NSOCP=NSOCP+1
          IF(ISMAP(NSOCP,ISFP).EQ.2) THEN
            ICA1=2
          ELSE
            ICA1=3
          END IF
        ELSE
          ICA1=4
        END IF
        IO =IOMAP(I,IOCF )
        IF(IO.EQ.1) THEN
          ICA2=1
        ELSE IF(IO.EQ.2) THEN
          NSOC=NSOC+1
          IF(ISMAP(NSOC,ISF).EQ.2) THEN
            ICA2=2
          ELSE
            ICA2=3
          END IF
        ELSE
          ICA2=4
        END IF
        IB1=IB1+IDELB(ICA1)
        IB2=IB2+IDELB(ICA2)
        IDIF12=IB1-IB2
C%%%%
        B=IB2
C**** IF LOOP CLOSES OR NOT ********************************************
        IF(IDIF12.EQ.0) THEN
          IF((ICA1.EQ.1).AND.(ICA2.EQ.3)) THEN
            GO TO 112
          ELSE IF((ICA1.EQ.2).AND.(ICA2.EQ.4)) THEN
            F=F*SQRT(B/(B+1.0D+00))
            GO TO 112
          ELSE IF((ICA1.EQ.1).AND.(ICA2.EQ.2)) THEN
            GO TO 112
          ELSE IF((ICA1.EQ.3).AND.(ICA2.EQ.4)) THEN
            F=F*SQRT((B+2.0D+00)/(B+1.0D+00))
            GO TO 112
          END IF
        END IF
C**** MIDDLE LOOP ******************************************************
        IF(ICA1.EQ.ICA2) THEN
          IF(IDIF12.EQ.-1) THEN
            IF(ICA1.EQ.1) THEN
              GO TO 110
            ELSE IF(ICA1.EQ.2) THEN
              F=F*SQRT(B*B-1.0D+00)/B
              GO TO 110
            ELSE
              F=-F
              GO TO 110
            END IF
          ELSE IF(IDIF12.EQ.1) THEN
            IF(ICA1.EQ.1) THEN
              GO TO 110
            ELSE IF(ICA1.EQ.3) THEN
              BP2=B+2.0D+00
              F=F*SQRT(BP2*BP2-1.0D+00)/BP2
              GO TO 110
            ELSE
              F=-F
              GO TO 110
            END IF
          END IF
        END IF
        IF((IDIF12.EQ.-1).AND.(ICA1.EQ.3).AND.(ICA2.EQ.2)) THEN
          F=F/B
          GO TO 110
        ELSE IF((IDIF12.EQ.1).AND.(ICA1.EQ.2).AND.(ICA2.EQ.3)) THEN
          F=-F/(B+2.0D+00)
          GO TO 110
        END IF
C
C     EQUAL ZERO
C
        GO TO 116
  110 CONTINUE
C
C     EQUAL ZERO
C
      GO TO 116
C***********************************************************************
C**** AFTER LOOP *******************************************************
C****            *******************************************************
  112 LA2=LEV
      LEVP1=LEV+1
      DO 114 I=LEVP1,NORB
        IOP=IOMAP(I,IOCFP)
        IF(IOP.EQ.1) THEN
          ICA1=1
        ELSE IF(IOP.EQ.2) THEN
          NSOCP=NSOCP+1
          IF(ISMAP(NSOCP,ISFP).EQ.2) THEN
            ICA1=2
          ELSE
            ICA1=3
          END IF
        ELSE
          ICA1=4
        END IF
        IO =IOMAP(I,IOCF )
        IF(IO.EQ.1) THEN
          ICA2=1
        ELSE IF(IO.EQ.2) THEN
          NSOC=NSOC+1
          IF(ISMAP(NSOC,ISF).EQ.2) THEN
            ICA2=2
          ELSE
            ICA2=3
          END IF
        ELSE
          ICA2=4
        END IF
        IF(ICA1.NE.ICA2) THEN
          GO TO 116
        END IF
  114 CONTINUE
C**** SET F INTO VALUE *************************************************
      NELM=NELM+1
      NWRITE=NWRITE+1
      IF(NWRITE.GT.MAXCSF) THEN
        IF(MASWRK) WRITE(LUNOUT,200) NWRITE,MAXCSF
        CALL ABRT
      END IF
      IF(MDUM.GT.MPDUM) THEN
        LAB1(1,NWRITE)=M
        LAB1(2,NWRITE)=MP
        LAB1(3,NWRITE)=LA2
        LAB1(4,NWRITE)=LA1
        WORK(  NWRITE)=F
      ELSE
        LAB1(1,NWRITE)=MP
        LAB1(2,NWRITE)=M
        LAB1(3,NWRITE)=LA1
        LAB1(4,NWRITE)=LA2
        WORK(  NWRITE)=F
      END IF
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LE.-100 .AND. MASWRK) THEN
        IF(MDUM.GT.MPDUM) THEN
          WRITE(LUNOUT,100) NTYPE,M,LA2,LA1,MP,F
        ELSE
          WRITE(LUNOUT,100) NTYPE,MP,LA1,LA2,M,F
        END IF
      END IF
C***********************************************************************
C**** CONTINUE *********************************************************
C****          *********************************************************
  116 CONTINUE
  902 CONTINUE
  900 CONTINUE
      IF(MDUM.NE.NCSF) THEN
        KCONT=1
      ELSE
        KCONT=0
      END IF
      IF (MOD(INWFT0,NPROC).EQ.ME) THEN
         WRITE(LUNFTP) KCONT,NWRITE
         CALL MQGENW(LUNFTP,4*NWRITE,NWRITE,LAB1,WORK)
         NWFT0=NWFT0+1
         NWFT0P=NWFT0P+NWRITE
      END IF
      INWFT0=INWFT0+1
  118 CONTINUE
C
      IF (MASWRK .AND. LPOUT.LE.-1) THEN
         WRITE(LUNOUT,*) 'INWFT0=',INWFT0
         WRITE(LUNOUT,*) 'NWFT0 =',NWFT0
         WRITE(LUNOUT,*) 'NWFT0P=',NWFT0P
      END IF
C
C***********************************************************************
C****            *******************************************************
C****   RETURN   *******************************************************
C****            *******************************************************
C     DO 3 I=1,NOCF
C       WRITE(LUNOUT,*) I,(IOMAP(J,I),J=NDOUB+1,NORB)
C   3 CONTINUE
C     DO 4 I=1,MAXSPF
C       WRITE(LUNOUT,*) I,(ISMAP(J,I),J=1,MAXSOC)
C   4 CONTINUE
      RETURN
      END
C*MODULE MCQDPT  *DECK MQCNDR
      SUBROUTINE MQCNDR(LUNOUT,LPOUT ,
     *                  NEL   ,MULT  ,NORB  ,NDOUB ,MAXROW,
     *                  OCC   ,K     ,L     ,NJ    ,NJSKP ,OCCMIN,
     *                  OCCMAX,CTYPE )
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
C**** INSTRUCTION ******************************************************
C
C     ROUTINE MQCNDR CONSTRUCTS THE CHAINING INDICES FOR THE
C  DICTINCT ROW TABLE SUBJECT TO THE OCCMIN AND OCCMAX.
C
C**** DECLARATION ******************************************************
      IMPLICIT INTEGER (A-Z)
      DIMENSION OCC(MAXROW) ,K(3,MAXROW)  ,L(3,MAXROW)
      DIMENSION NJ(NORB+1)  ,NJSKP(NORB+1)
      DIMENSION OCCMIN(NORB),OCCMAX(NORB)
      CHARACTER*4 CTYPE
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQCNDR ***'')')
        CALL MQDEBG(LUNOUT,
     *  '   NEL','  MULT','  NORB',' NDOUB','MAXROW','     -','     -',
     *      NEL ,   MULT ,   NORB ,  NDOUB , MAXROW ,      0 ,      0 )
      END IF
C***********************************************************************
C**** START ************************************************************
C****       ************************************************************
C**** ORBITAL CONFIGURATION OR SPIN FUNCTION ***************************
      IF(CTYPE.NE.'SPIN') THEN
        MAXCAS=3
      ELSE
        MAXCAS=2
        GO TO 140
      END IF
C**** MAKE OCCMIN AND OCCMAX *******************************************
      DO 104 I=1,NDOUB
        OCCMIN(I)=I*2
        OCCMAX(I)=I*2
  104 CONTINUE
      NDOUB2=NDOUB*2
      MINEL=NEL
      MAXEL=NDOUB2+2
      DO 106 I=NDOUB+1,NORB
        IF(MINEL.LT.NDOUB2) THEN
          OCCMIN(NORB+NDOUB+1-I)=NDOUB2
        ELSE
          OCCMIN(NORB+NDOUB+1-I)=MINEL
        END IF
        IF(MAXEL.GT.NEL) THEN
          OCCMAX(I)=NEL
        ELSE
          OCCMAX(I)=MAXEL
        END IF
        MINEL=MINEL-2
        MAXEL=MAXEL+2
  106 CONTINUE
C**** INITIALIZE K AND L ***********************************************
  140 DO 150 J=1,MAXROW
      DO 150 I=1,3
        K(I,J)=0
        L(I,J)=0
  150 CONTINUE
C**** INITIALIZE THE FIRST VERTEX AT THE ZEROTH LEVEL ******************
      OCC(1)=0
      NJ(1)=1
      NJSKP(1)=0
C**** LOOP OVER ALL THE REMAINING LEVELS *******************************
      DO 600 LEV=1,NORB
        LEVP1=LEV+1
        NJSKP(LEVP1)=NJSKP(LEV)+NJ(LEV)
        NJ(LEVP1)=0
C**** LOOP OVER THE ROWS OF THE CURRENT LEVEL **************************
        DO 500 NROW=1,NJ(LEV)
          IROW=NJSKP(LEV)+NROW
          OCCC=OCC(IROW)
C**** LOOP OVER THE FOUR CASE FROM THE CURRENT *************************
C****   DISTINCT ROW. IROW                     *************************
          DO 400 ICASE=MAXCAS,1,-1
            IF(CTYPE.NE.'SPIN') THEN
              OCCNXT=OCCC+ICASE-1
              IF(OCCNXT.LT.OCCMIN(LEV)) GO TO 400
              IF(OCCNXT.GT.OCCMAX(LEV)) GO TO 400
            ELSE
              IF(ICASE.EQ.2) THEN
                OCCNXT=OCCC+1
              ELSE
                OCCNXT=OCCC-1
              END IF
              IF(OCCNXT.LT.0) GO TO 400
              IF(NORB-LEV.LT.IABS(MULT-OCCNXT-1)) GO TO 400
            END IF
C**** CHECK TO SEE IF THIS NEW ROW ALREADY EXISTS IN THE DRT ***********
            N=NJSKP(LEVP1)
            IF(NJ(LEVP1).EQ.0) GO TO 200
            DO 100 I=1,NJ(LEVP1)
              N=N+1
              IF(OCCNXT.NE.OCC(N)) GO TO 100
C**** THIS ROW HAS ALREADY BEEN ADDED. ADJUST THE CHAINING INDICES *****
              K(ICASE,N)   =IROW
              L(ICASE,IROW)=N
              GO TO 300
  100       CONTINUE
C**** THIS IS A NEW ROW. APPEND THIS NEW ROW TO THE DRT ****************
  200       N=N+1
            IF(N.GT.MAXROW) THEN
              IF (MASWRK) WRITE(LUNOUT,1500) MAXROW
 1500         FORMAT(' *** ERROR STOP IN SUB.MQCNDR ***'/
     1               ' MAXROW =',I5)
              CALL ABRT
            END IF
C
            NJ(LEVP1)=NJ(LEVP1)+1
            OCC(N)=OCCNXT
            K(ICASE,N)   =IROW
            L(ICASE,IROW)=N
  300     CONTINUE
  400     CONTINUE
  500   CONTINUE
  600 CONTINUE
C**** RETURN AND END ***************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQCOND
      SUBROUTINE MQCOND(LUNOUT,LPOUT ,INIACT,LASACT,NOCF  ,NCSF  ,
     *                  NOCFI ,NCSFNW,ISELCT,
     *                  NSNSF ,I1EX1 ,I1EX2 ,KREF  ,KREFWK,LOD2NW)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER   NSNSF(NOCF+1)
      INTEGER   I1EX1(INIACT:LASACT,0:NOCF)
      INTEGER   I1EX2(INIACT:LASACT,0:NOCFI)
      LOGICAL   KREF(NCSF), KREFWK(NCSF)
      DIMENSION LOD2NW(NCSF)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQCOND ***'')')
        CALL MQDEBG(LUNOUT,
     *  'INIACT','LASACT','  NOCF','  NCSF',' NOCFI','     -','     -',
     *   INIACT , LASACT ,   NOCF ,   NCSF ,  NOCFI ,      0 ,      0 )
      END IF
C**** DETERMINE NECESSARY CSF'S ****************************************
      DO ICSF=1,NCSF
        KREFWK(ICSF)=.FALSE.
      END DO
      DO IOCF=1,NOCF
        DO ICSF=NSNSF(IOCF)+1,NSNSF(IOCF+1)
          IF(KREF(ICSF)) THEN
            DO I=INIACT,LASACT
              DO J=INIACT,LASACT
                K=I1EX2(I,I1EX1(J,IOCF))
                IF(K.NE.0) THEN
                  DO KCSF=NSNSF(K)+1,NSNSF(K+1)
                    KREFWK(KCSF)=.TRUE.
                  END DO
                END IF
              END DO
            END DO
C           GO TO 100
          END IF
        END DO
      END DO
C**** MAKE LABELS ******************************************************
      NCSFNW=0
      DO ICSF=1,NCSF
        IF(KREFWK(ICSF)) THEN
          NCSFNW=NCSFNW+1
          LOD2NW(ICSF)=NCSFNW
        ELSE
          LOD2NW(ICSF)=0
        END IF
      END DO
      IF (MASWRK) THEN
         IF(ISELCT.EQ.0) THEN
            WRITE(LUNOUT,9100) NCSF
         ELSE
            WRITE(LUNOUT,9110) NCSFNW,NCSF
         END IF
      END IF
      RETURN
 9100 FORMAT(1X,'CONDENSED LABELS MADE FOR ',I13,' CSFS')
 9110 FORMAT(1X,'CONDENSED LABELS MADE, SELECTING',I12,
     *          ' CSFS FROM A TOTAL OF',I13)
      END
C*MODULE MCQDPT  *DECK MQCONF
      SUBROUTINE MQCONF(LUNOUT,LPOUT ,
     *                  MULT  ,NEL   ,NORB  ,MAXROW,NDOUB,
     *                  OCCMIN,OCCMAX,
     *                  OCC   ,K     ,L     ,NJ    ,NJSKP,XBAR  ,
     *                  YP    ,XP    ,CASE  ,BROW  ,YTOT ,Y     ,
     *                  NSPINF,
     *                  NREF  ,MAXSOC,MAXSPF,NCSF  ,NOCFI)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT INTEGER (A-Z)
      DOUBLE PRECISION DNMOAC,DISOC,DNDOC,DUM,DI,DWORK
      DOUBLE PRECISION RUNTYP,EXETYP,CHECK
C
      PARAMETER (MXAO=2047)
      CHARACTER*4 LBL(MXAO)
C
      DIMENSION OCCMIN(NORB) ,OCCMAX(NORB)
      DIMENSION OCC(MAXROW)     ,K(3,MAXROW)  ,L(3,MAXROW)
      DIMENSION NJ(NORB+1)      ,NJSKP(NORB+1)
      DIMENSION XBAR(MAXROW)    ,YP(3,MAXROW) ,XP(MAXROW)
      DIMENSION CASE(NORB)      ,BROW(NORB+1) ,YTOT(MAXROW)
      DIMENSION Y(3,MAXROW)
      DIMENSION NSPINF(0:20,22)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /FMCOM / DWORK(1)
      COMMON /MQIOFI/ IDAF50,NAV50,IODA50(400)
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
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQCONF ***'')')
        CALL MQDEBG(LUNOUT,
     *  '  MULT','   NEL','  NORB','MAXROW',' NDOUB','     -','     -',
     *     MULT ,    NEL ,   NORB , MAXROW ,  NDOUB ,      0 ,      0 )
      END IF
C**** NUMBER OF SPIN FUNCTIONS *****************************************
      DO J=1,22
        DO I=0,20
          NSPINF(I,J)=0
        END DO
      END DO
      NSPINF(0,1)=1
      DO I=1,19,2
        DO J=2,I+1,2
          NSPINF(I,J)=NSPINF(I-1,J-1)+NSPINF(I-1,J+1)
        END DO
        NSPINF(I+1,1)=NSPINF(I,2)
        DO J=3,I+2,2
          NSPINF(I+1,J)=NSPINF(I,J-1)+NSPINF(I,J+1)
        END DO
      END DO
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQCONF ***''/
     *                 '' *** MATRIX NSPINF ***'')')
       CALL MQWMAI(LUNOUT,NSPINF(1,1),21,20,20,'    ')
      END IF
C***********************************************************************
C**** ORBITAL CONFIGURATION ********************************************
C****                       ********************************************
C**** CALL SUB.MQCNDR **************************************************
      CALL MQCNDR(LUNOUT,LPOUT,NEL,MULT,NORB,NDOUB,MAXROW,
     *            OCC,K,L,NJ,NJSKP,OCCMIN,OCCMAX,'    ')
C**** CALL SUB.MQCHAI **************************************************
      CALL MQCHAI(LUNOUT,LPOUT,NORB,MAXROW,
     *            NJ,NJSKP,K,L,Y,XBAR,YP,XP,'    ')
C**** WRITE DISTINCT ROW TABLE *****************************************
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
      WRITE(LUNOUT,108)
  108 FORMAT(' *** DISTINCT ROW TABLE ***'/
     *       1X,53(1H=)/
     *       '   LEV  ROW',T16,'OCC',T24,'K0',T29,'K1',
     *       T34,'K2',T41,'L0',T46,'L1',T51,'L2')
      DO LEV=NORB,0,-1
        LEVP1=LEV+1
        ROW=NJSKP(LEVP1)
        WRITE(LUNOUT,'(1X)')
        DO IROW=1,NJ(LEVP1)
          ROW=ROW+1
          WRITE(LUNOUT,110) LEV,ROW,OCC(ROW),
     *      (K(I,ROW),I=1,3),(L(I,ROW),I=1,3)
  110     FORMAT(1X,2I5,I7,2X,3I5,2X,3I5)
        END DO
      END DO
      WRITE(LUNOUT,'(1X,53(1H-))')
      WRITE(LUNOUT,114)
  114 FORMAT('   LEV  ROW',
     *       T24,'Y0',T29,'Y1',T32,'XBAR',
     *       T40,'YP1',T45,'YP2',T51,'XP')
      DO LEV=NORB,0,-1
        LEVP1=LEV+1
        ROW=NJSKP(LEVP1)
        WRITE(LUNOUT,'(1X)')
        DO IROW=1,NJ(LEVP1)
          ROW=ROW+1
          WRITE(LUNOUT,116) LEV,ROW,
     *      (Y(I,ROW),I=1,2),XBAR(ROW),
     *      (YP(I,ROW),I=2,3),XP(ROW)
  116     FORMAT(1X,2I5,9X,3I5,2X,3I5)
        END DO
      END DO
      WRITE(LUNOUT,'(1X,53(1H=))')
      END IF
C**** CALL SUB.MQREF ***************************************************
      NREFTM=XBAR(1)
C
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... OMAP .............................................................
      N001=MQADDR((NORB-NDOUB+1)*NREFTM,'I')
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-N001
      CALL GETFM(NEED1)
C**** ERROR STOP *******************************************************
C     IDUM=MQMCHK()
      CALL MQREF(LUNOUT,LPOUT,MULT,NORB,MAXROW,
     *           NDOUB,NREFTM,NREF,
     *           CASE,BROW,YTOT,Y,L,DWORK(N001),LBL,'    ')
C**** WRITE OMAP *******************************************************
      NDSIZE=MQDSIZ((NORB-NDOUB+1)*NREF,'I')
      CALL MQDAWR(IDAF50,IODA50,DWORK(N001),NDSIZE,1,1)
C***********************************************************************
C**** SPIN FUNCTION ****************************************************
C****               ****************************************************
C**** CALL SUB.MQCNDR **************************************************
      NELACT=NEL-2*NDOUB
      MAXSOC=MIN(NELACT,2*(NORB-NDOUB)-NELACT)
      CALL MQCNDR(LUNOUT,LPOUT,0,MULT,MAXSOC,0,MAXROW,
     *            OCC,K,L,NJ,NJSKP,OCCMIN,OCCMAX,'SPIN')
C**** CALL SUB.MQCHAI **************************************************
      CALL MQCHAI(LUNOUT,LPOUT,MAXSOC,MAXROW,
     *            NJ,NJSKP,K,L,Y,XBAR,YP,XP,'SPIN')
C**** WRITE DISTINCT ROW TABLE *****************************************
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
      WRITE(LUNOUT,122)
  122 FORMAT(' *** DISTINCT ROW TABLE FOR ORBITAL CONFIGURATION ***'/
     *       1X,53(1H=)/
     *       '   LEV  ROW',T16,'OCC',T24,'K0',T29,'K1',
     *       T34,'K2',T41,'L0',T46,'L1',T51,'L2')
      DO LEV=MAXSOC,0,-1
        LEVP1=LEV+1
        ROW=NJSKP(LEVP1)
        WRITE(LUNOUT,'(1X)')
        DO IROW=1,NJ(LEVP1)
          ROW=ROW+1
          WRITE(LUNOUT,124) LEV,ROW,OCC(ROW),
     *      (K(I,ROW),I=1,3),(L(I,ROW),I=1,3)
  124     FORMAT(1X,2I5,I7,2X,3I5,2X,3I5)
        END DO
      END DO
      WRITE(LUNOUT,'(1X,53(1H-))')
      WRITE(LUNOUT,128)
  128 FORMAT('   LEV  ROW',
     *       T24,'Y0',T29,'Y1',T32,'XBAR',
     *       T40,'YP1',T45,'YP2',T51,'XP')
      DO LEV=MAXSOC,0,-1
        LEVP1=LEV+1
        ROW=NJSKP(LEVP1)
        WRITE(LUNOUT,'(1X)')
        DO IROW=1,NJ(LEVP1)
          ROW=ROW+1
          WRITE(LUNOUT,130) LEV,ROW,
     *      (Y(I,ROW),I=1,2),XBAR(ROW),
     *      (YP(I,ROW),I=2,3),XP(ROW)
  130     FORMAT(1X,2I5,9X,3I5,2X,3I5)
        END DO
      END DO
      WRITE(LUNOUT,'(1X,53(1H=))')
      END IF
C**** CALL SUB.MQREF ***************************************************
      MAXSPF=XBAR(1)
      IF(MAXSPF.NE.NSPINF(MAXSOC,MULT)) THEN
        IF (MASWRK) WRITE(LUNOUT,133) MAXSPF,NSPINF(MAXSOC,MULT)
  133   FORMAT(' *** ERROR STOP IN SUB.MQCONF ***'/
     *         ' MAXSPF IS NOT EQUAL TO THE VALUE OF TABLE OF NUMBER',
     *         ' OF SPIN FUNCTION.'/
     *         ' MAXSPF =',I10,'  # FROM SPIN TABLE =',I10)
        CALL ABRT
      END IF
C.... PROGRAM IS KEEPING NEED1 (DW) NOW.
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... SMAP .............................................................
      N002=MQADDR((MAXSOC+1)*MAXSPF,'I')
C
      LASTP1=MQMNXT()
      NEED2=LASTP1-N002
      CALL GETFM(NEED2)
C**** ERROR STOP *******************************************************
C     IDUM=MQMCHK()
      CALL MQREF(LUNOUT,LPOUT,MULT,MAXSOC,MAXROW,
     *           0,MAXSPF,MAXSPF,
     *           CASE,BROW,YTOT,Y,L,DWORK(N002),LBL,'SPIN')
C**** WRITE SMAP *******************************************************
      NDSIZE=MQDSIZ((MAXSOC+1)*MAXSPF,'I')
      CALL MQDAWR(IDAF50,IODA50,DWORK(N002),NDSIZE,2,1)
C***********************************************************************
C**** MAKE LABEL *******************************************************
C****            *******************************************************
C**** CALL MQLBL1 ******************************************************
C.... PROGRAM IS KEEPING NEED1+NEED2 (DW) NOW.
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... NSNSF ............................................................
      N003=MQADDR(NREF+1,'I')
C
      LASTP1=MQMNXT()
      NEED3=LASTP1-N003
      CALL GETFM(NEED3)
C**** ERROR STOP *******************************************************
C     IDUM=MQMCHK()
      CALL MQLBL1(LUNOUT,LPOUT ,NDOUB+1,NORB,NREF  ,
     *            NCSF  ,
     *            NSPINF(0,MULT),DWORK(N001),DWORK(N003))
C     CALL MQLBL1(LUNOUT,LPOUT ,INIACT,LASACT,NOCF  ,
C    *            NCSF  ,
C    *            NSPINF,OMAP  ,NSNSF )
C**** WRITE NSNSF ******************************************************
      NDSIZE=MQDSIZ(NREF+1,'I')
      CALL MQDAWR(IDAF50,IODA50,DWORK(N003),NDSIZE,3,1)
C**** CALL MQLBL2 ******************************************************
C.... PROGRAM IS KEEPING NEED1+NEED2+NEED3 (DW) NOW.
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... IOCSF ............................................................
      N004=MQADDR(2*NCSF,'I')
C
      LASTP1=MQMNXT()
      NEED4=LASTP1-N004
      CALL GETFM(NEED4)
C**** ERROR STOP *******************************************************
C     IDUM=MQMCHK()
      IF(EXETYP.NE.CHECK) THEN
         CALL MQLBL2(LUNOUT,LPOUT ,NREF  ,NCSF  ,
     *               DWORK(N003),DWORK(N004))
      ELSE
         NDSIZE = MQDSIZ(2*NCSF,'I')
         CALL VCLR(DWORK(N004),1,NDSIZE)
      END IF
C     CALL MQLBL2(LUNOUT,LPOUT ,NOCF  ,NCSF  ,
C    *            NSNSF ,IOCSF )
C**** WRITE IOCSF ******************************************************
      NDSIZE=MQDSIZ(2*NCSF,'I')
      CALL MQDAWR(IDAF50,IODA50,DWORK(N004),NDSIZE,4,1)
C**** CALL MQI1EX ******************************************************
      NELACI=NEL-2*NDOUB-1
      NMOACT=NORB-NDOUB
      IF(MULT.EQ.1) THEN
        IF(NELACI.GT.0) THEN
          MNSOC=1
        ELSE
          MNSOC=0
        END IF
      ELSE
        MNSOC=MULT-2
      END IF
      MXSOC=MIN(NELACI,2*NMOACT-NELACI)
      NOCFI=0
      DNMOAC=NMOACT
      DO ISOC=MNSOC,MXSOC,2
        DUM=1.0D+00
        NDOC=(NELACI-ISOC)/2
        DISOC=ISOC
        DNDOC=NDOC
        DO I=0,ISOC-1
          DI=I
          DUM=DUM*(DNMOAC-DI)/(DISOC-DI)
        END DO
        DO I=0,NDOC-1
          DI=I
          DUM=DUM*(DNMOAC-DISOC-DI)/(DNDOC-DI)
        END DO
        NOCFI=NOCFI+INT(DUM+0.5D+00)
      END DO
C.... PROGRAM IS KEEPING NEED1+NEED2+NEED3+NEED4 (DW) NOW.
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... OMAPI ............................................................
      N005=MQADDR((NMOACT+1)*NOCFI,'I')
C.... I1EX1 ............................................................
      N006=MQADDR(NMOACT*(NREF+1),'I')
C.... I1EX2 ............................................................
      N007=MQADDR(NMOACT*(NOCFI+1),'I')
C.... OMAPC ............................................................
      N008=MQADDR(NMOACT,'I')
C.... IHMAPI ...........................................................
      N009=MQADDR(MAX(NOCFI,NREF),'I')
C.... IHSMAP ...........................................................
      NHASH=MAX(NOCFI,NREF)*4-1
      N010=MQADDR(NHASH,'I')
C
      LASTP1=MQMNXT()
      NEED5=LASTP1-N005
      CALL GETFM(NEED5)
C     IDUM=MQMCHK()
      IF(EXETYP.EQ.CHECK) THEN
         NDSIZE=MQDSIZ(NMOACT*(NREF+1),'I')
         CALL VCLR(DWORK(N006),1,NDSIZE)
         NDSIZE=MQDSIZ(NMOACT*(NOCFI+1),'I')
         CALL VCLR(DWORK(N007),1,NDSIZE)
      ELSE
         CALL MQI1EX(LUNOUT,LPOUT,NDOUB+1,NORB,NREF,NOCFI,
     *               DWORK(N001),DWORK(N005),DWORK(N006),DWORK(N007),
     *               DWORK(N008),DWORK(N009),DWORK(N010),NHASH)
      END IF
C     CALL MQI1EX(LUNOUT,LPOUT ,INIACT,LASACT,NOCF  ,NOCFI ,
C    *            OMAP  ,OMAPI ,I1EX1 ,I1EX2 ,OMAPC ,
C    *            IHMAPI,IHSMAP,NHASH)
C**** WRITE I1EX *******************************************************
      NDSIZE=MQDSIZ(NMOACT*(NREF+1),'I')
      CALL MQDAWR(IDAF50,IODA50,DWORK(N006),NDSIZE,5,1)
      NDSIZE=MQDSIZ(NMOACT*(NOCFI+1),'I')
      CALL MQDAWR(IDAF50,IODA50,DWORK(N007),NDSIZE,6,1)
C
      CALL RETFM(NEED5)
      CALL RETFM(NEED4)
      CALL RETFM(NEED3)
      CALL RETFM(NEED2)
      CALL RETFM(NEED1)
C
C     WRITE RESULTS
C
      IF (MASWRK) WRITE(LUNOUT,134) NREF,MAXSPF,NCSF
  134 FORMAT(1X,'NUMBER OF SPACE ORBITAL PRODUCTS      =',I10/
     *       1X,'MAXIMUM NUMBER OF SPIN FUNCTIONS      =',I10/
     *       1X,'TOTAL NUMBER OF CSFS (ALL SYMMETRIES) =',I10)
C
      IF(NREF.EQ.0) THEN
        IF(MASWRK) WRITE(LUNOUT,'('' *** ERROR STOP IN SUB.MQCONF ***''/
     *    '' NUMBER OF ORBITAL CONFIGURATIONS MUST BE GREATER THAN 0.'')
     *    ')
        CALL ABRT
      END IF
      RETURN
      END
C*MODULE MCQDPT  *DECK MQDEBG
      SUBROUTINE MQDEBG(LUNOUT,
     *                  C0,C1,C2,C3,C4,C5,C6,
     *                  X0,X1,X2,X3,X4,X5,X6)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT INTEGER (A-Z)
      CHARACTER*6 C0,C1,C2,C3,C4,C5,C6
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      IF (MASWRK) WRITE(LUNOUT,'(1X,7(4X,A6)/1X,7I10)')
     *  C0,C1,C2,C3,C4,C5,C6,X0,X1,X2,X3,X4,X5,X6
      RETURN
      END
C*MODULE MCQDPT  *DECK MQDRV
      SUBROUTINE MQDRV(GROUP,GRPV)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MXATM=500, MXRT=100)
      LOGICAL TRACE
      COMMON /FMCOM / D(1)
C**** COMMON FOR GAMESS ROUTINE ****************************************
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NVA,IODA(400)
      COMMON /INFOA / NAT,IDUM1,IDUM2,NGO,NQMT,IDUM4,IDUM5,IDUM6,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /MQIOFI/ IDAF50,NAV50,IODA50(400)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,NXRT,NSTAT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /SOMCQD/ MAXSPF1,MAXSOC1,NCSF1,NOCF1
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
C**** ARRAYS FOR INPUT AND STORE ***************************************
      LOGICAL   KSTATE,REFWGT,CANAVE,CANSPE,GOPARR,DSKWRK,MASWRK
      LOGICAL   DELSCR,PARAIO,LMQPAR
      COMMON /MQ2PAR/DMQPAR(200),AVECOE(MXRT),
     *               IMQPAR(400),MAINCS( 3),KSTATE(MXRT),LMQPAR(10)
      INTEGER CACIOP
      INTEGER DTWOEL
      LOGICAL OERFLG,DOORD0
      COMMON /MQMEM/ MEMINP,MEMDDI
      COMMON /MQFT02/NWFT0,NWFT0P
      COMMON /MQFTB2/NWFTB,NWFTBP,NWFTBG
      COMMON /MQFTC2/NWFTC,NWFTCP,NWFTCG
      COMMON /MQFT2 /NWFT2,NWFT2P,NWFT2G
      COMMON /TIMING/ CPU,WALL
      CHARACTER*6 STRSUB
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /,TRANST/8HTRANSITN/
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      CHARACTER*8 :: TRANST_STR
      EQUIVALENCE (TRANST, TRANST_STR)
      DATA CHECK_STR/"CHECK   "/,TRANST_STR/"TRANSITN"/
#endif
      DATA LASTE/0/
      DATA OERFLG/.FALSE./
      SAVE OERFLG,LASTE
C
C     STRUCTURE OF MC-QDPT DRIVER ROUTINE, MQDRV
C
C     IF(INORB.NE.0) THEN ! IF INORB=1, THEN READ MOS FROM CARDS
C       CALL MQTRSQ       ! CHANGE LOWER TRIANGULAR S-MATRIX TO SQUARE
C                         ! S-MATRIX
C       CALL MQREMO       ! READ MOS
C     END IF
C     CALL MQMOSY ! DETERMINE MO SYMMETRIES
C     CALL MQSORD ! ORDER MOS WITH THE SAME SYMMETRY REPEATED
C     CALL MQCONF ! GENERATE CONFIGURATIONS
C     CALL MQPCNF ! PRINT SPIN FUNCTIONS AND OCFS (ORBITAL CONFIGURATION
C                 ! FUNCTIONS) TO SOME FILE
C     CALL MQCIG1 ! EVALUATE ONE-PARTICLE COUPLING CONSTANTS FOR CAS-CI
C     CALL MQPTG1 ! EVALUATE ONE-PARTICLE COUPLING CONSTANTS FOR PT
C     CALL MQSYM  ! DETERMINE CSF SYMMETRIES
C     IF(IFORB.EQ.0) THEN ! IF FLAG TO CALCULATE CANONICAL FOCK ORBITALS
C                         ! IS OFF (IFORB=0), THEN
C                         ! SELECT AO -> MO INTEGRAL TRANSFORMATION
C                         ! ROUTINE FOR PT (AND ALSO CAS-CI)
C       CALL MQTRF        ! GET MO INTEGRALS FOR PT AND CAS-CI
C     ELSE                ! IF ON (IFORB=1), THEN SELECT AO -> MO
C                         ! INTEGRAL TRANSFORMATION ROUTINE
C                         ! FOR CAS-CI ONLY
C       CALL MQTRMC       ! GET MO INTEGRALS FOR CAS-CI
C     END IF
C     IF(IFORB.NE.0) THEN ! IF FLAG TO CALCULATE CANONICAL FOCK ORBITALS
C                         ! IS ON (IFORB=0), THEN
C                         ! GET CANONICAL FOCK ORBITALS
C       CALL MQCACI       ! DO CAS-CI USING INPUT MOS
C       CALL MQFORB       ! GET FOCK ORBITALS
C       CALL MQTRSQ       ! CHANGE LOWER TRIANGULAR S-MATRIX TO SQUARE
C                         ! S-MATRIX
C       CALL MQMXOL       ! ORDER FOCK MOS SO AS TO HAVE MAXIMUM
C                         ! OVERLAPS WITH INPUT MOS
C       CALL MQMOSY       ! RE-DETERMINE MO SYMMETRIES FOR SAFETY
C       CALL MQTRF        ! GET MO INTEGRALS FOR PT
C     END IF
C     IF(ISELCT.NE.0) THEN ! IF FLAG TO SELECT CSF IN CAS FOR SPEED UP
C                            IS ON, THEN DO CAS-CI AND
C                          ! CSF SELECTION
C       CALL MQCACI        ! DO CAS-CI
C       CALL MQSLT2        ! SELECT IMPORTANT CSFS IN CAS
C     END IF
C     CALL MQCACI ! DO CAS-CI
C     CALL MQORB2 ! CALCULATE ORBITAL ENERGIES
C     CALL MQOCF1 ! REMOVE UNNECESSARY COUPLING CONSTANTS FOR SPEED UP
C     CALL MQOCF2 ! SIMILAR TO THE ABOVE BUT REMOVE ANOTHER COUPLING
C                 ! CONSTANTS
C     CALL MQCOND ! MAKE CONDENSED LABEL:CSFS -> SELECTED CSFS
C     CALL MQLPR1 ! PREPARE MATRICES FOR PT
C     CALL MQLPR2 ! PREPARE OTHER MATRICES, PARTICLULARLY (IJ/KL), FOR
C                 ! PT
C     DO IALPHA=1,NSTATE ! LOOP OVER STATES
C       CALL MQLMB1      ! CALCULATE ZERO- AND ONE-BODY TERMS OF
C                        ! EFFECTIVE HAMILTONIAN
C       CALL MQLMB2      ! CALCULATE TWO-BODY TERMS
C       CALL MQLMB3      ! CALCULATE THREE-BODY TERMS
C 110   CONTINUE         ! DO LOOP ACCORDING TO THE AVAILABLE MEMORY
C         CALL MQLMBR    ! CALCULATE ZERO-, ONE-, AND TWO-BODY TERMS
C                        ! RELATED TO TWO ELECTRON INTEGRALS WITH TWO
C                        ! EXTERNAL ORBITALS
C       IF(ISTART.LE.LASEXT) GO TO 110
C     END DO
C     IF(ISELCT.NE.0) THEN ! IF FLAG TO SELECT CSF IN CAS IS ON, THEN
C                          ! CALCULATE THE CONTRIBUTION FROM UN-SELECTED
C                          ! CSF SPACE OF CAS
C       CALL MQVSML        ! GET CONTRIBUTION FROM UN-SELECTED CSF SPACE
C     END IF
C     CALL MQGETE ! DIAGONALIZE EFFECTIVE HAMILTONIAN AND GET ENERGIES
C
C     NOTE: MQCONF, MQTRMC, MQTRF ARE SUB-DRIVER ROUTINES.
C     ONLY THESE ROUTINES AND MQDRV GET DYNAMIC MEMORY.
C
C**** CPU TIME *********************************************************
      CALL TSECND(TIM0)
C
C        SET AO INTEGRAL ORDERING FLAG.
C        IN CASE OF TRUDGE RUNS, WE NEED TO SORT THE NEW AO INTEGRALS.
C        IN CASE OF SPIN-ORBIT, WE ARE AT THE SAME GEOMETRY, AND THE
C        NUMBER OF ENERGY EVALUATIONS SHOULD NOT BE BEING INCREMENTED.
C
      IF(NEVALS.NE.LASTE) OERFLG=.FALSE.
CNB
      OERFLG=.FALSE.
      LASTE = NEVALS
C
C**** FILES ************************************************************
      LUNIN =IR
      LUNOUT=IW
C**** FILES ************************************************************
      DELSCR=.FALSE.
      LUNERI= 8
      IDAF  =10
C
      IDAF50=50
C
      LUNFT0=51
      LUNFTA=52
      LUNFTB=53
      LUNFTC=54
C
      LUNOER=55
      LUNJMA=56
      LUNJM1=57
      LUNJM2=58
      LUNJM3=59
      LUNTWO=60
C
      LUNFT1=61
      LUNFT2=62
C
      LUNPUN=63
      LUNCNF=64
C
C         MARK PERTURBATION DISTRIBUTED MATRIX AS UNUSED
C         SO THAT SPIN-ORBIT RUNS DON'T MISTAKENLY TRY TO
C         DESTROY THIS D.M. AT THE END OF THIS ROUTINE.
C
      IDMWTH=0
C
C       SET TO MASWRK+TRUE TO SEE ROUTINE LEVEL TRACING AND TIMING
C
      TRACE=MASWRK  .AND.  .FALSE.
C**** WRITE TITLE PAGE *************************************************
C     WRITE(LUNOUT,'(72(1H*)/72(1H*)//)')
C     WRITE(LUNOUT,'(A43/A43//A39//A50/A50/A44/A55/A52/)')
C    *  '                          MR2D  VERSION 2.0',
C    *  '                          (MC-QDPT PROGRAM)',
C    *  '                              H. NAKANO',
C    *  '                   DEPARTMENT OF APPLIED CHEMISTRY',
C    *  '                    GRADUATE SCHOOL OF ENGINEERING',
C    *  '                         UNIVERSITY OF TOKYO',
C    *  '               HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN',
C    *  '                  E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP'
C     WRITE(LUNOUT,'(72(1H*)/72(1H*))')
C     WRITE(LUNOUT,'(A38/A41/A45)')
C    *  ' (C) 1992 KATO LAB.,  KYOTO UNIVERSITY',
C    *  ' (C) 1996 HIRAO LAB., UNIVERSITY OF TOKYO',
C    *  ' LAST MODIFICATION : THU JUL 28 18:50:22 1996'
C
C     READ $MCQDPT INPUT PARAMETERS
C
      STRSUB='MQREAD'
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      CALL MQREAD(GROUP,GRPV,MASWRK)
      IFORB =IMQPAR( 1)
      INOFMT=IMQPAR( 2)
      INORB =IMQPAR( 3)
      IROT  =IMQPAR( 4)
      ISELCT=IMQPAR( 5)
      ISTSYM=IMQPAR( 6)
      LENGTH=IMQPAR( 7)
      LPOUT =IMQPAR( 8)
      MAXCSF=IMQPAR( 9)
      MAXERI=IMQPAR(10)
      MAXROW=IMQPAR(11)
      MDI   =IMQPAR(12)
      MULT  =IMQPAR(13)
      MXBASE=IMQPAR(14)
      MXTRFR=IMQPAR(15)
      NEL   =IMQPAR(16)
      NMOACT=IMQPAR(17)
      NMODOC=IMQPAR(18)
      NMOEXT=IMQPAR(19)
      NMOFZC=IMQPAR(20)
C     NMOFZV=IMQPAR(21)
      NSOLUT=IMQPAR(22)
      NSTATE=IMQPAR(23)
      NSTCI =IMQPAR(24)
      NSTOP =IMQPAR(25)
      NOSYM =IMQPAR(26)
      IPURFY=IMQPAR(27)
C
      NINTMX=IMQPAR(101)
      NGO   =IMQPAR(102)
C
      GENZRO=DMQPAR( 1)
      THRCON=DMQPAR( 2)
      THRENE=DMQPAR( 3)
      THRERI=DMQPAR( 4)
      THRGEN=DMQPAR( 5)
      THRWGT=DMQPAR( 6)
      EDSHFT=DMQPAR( 7)
C
      REFWGT=LMQPAR( 1)
      PARAIO=LMQPAR( 2)
      DOORD0=LMQPAR( 3)
      DELSCR=LMQPAR( 4)
C
      NMOEXT=NMOEXT+NQMT-NGO
      NMOINT=NMOFZC+NMODOC+NMOACT
      NDOUB =NMOFZC+NMODOC
      NMO   =NMOFZC+NMODOC+NMOACT+NMOEXT
C
      AVGEN1 = 0.0D+00
      AVGEN2 = 0.0D+00
      AVGEN3 = 0.0D+00
C
      IF(IFORB.EQ.0.AND.NOCC.EQ.0.AND.ICI.NE.ABS(NUMVEC)) RETURN
C     WAIT FOR THE ORBITAL ENERGY GENERATING RUN.
      CANAVE=RUNTYP.EQ.TRANST.AND.IFORB.GT.2
      IF(CANAVE.AND.NOCC.NE.0) INORB=0
      IF(CANAVE.AND.NOCC.NE.0) IFORB=0
C     WRITE(6,*) 'WWW',CANAVE,NOCC,INORB,IFORB
      ITYPE=2-MOD(IFORB,2)
      IF(ITYPE.EQ.2.AND.IFORB.GT.2) THEN
         IF (MASWRK) WRITE(LUNOUT,*) 'IFORB=4 IS NOT SUPPORTED'
         CALL ABRT
      ENDIF
C
C**** OPEN FILES *******************************************************
C
C.... DIRECT ACCESS FILE, FILE 50.
C
C      TYPE  RECORD  CONTENT
C      ----  ------  -------
C      I      1      CASE NUMBERS FOR GENERATING ORBITAL
C                    CONFIGURATION FUNCTIONS (OCFS)
C      I      2      CASE NOS. FOR GENERATING SPIN FUNCS (SFS)
C      I      3      LABELS FOR OCF+SP -> CSF
C      I      4      LABELS FOR CSF -> OCF+SP
C      I      5      LABEL1
C      I      6      LABEL2; LABEL1 AND LABEL2: OCF -> SX-OCF
C      D      7      INPUT MOS
C      D      8      CANONICAL FOCK MOS
C      D      9      ORBITAL ENERGIES
C      I     10      MOLECULAR SYMMETRIES
C      L     11      SELECTED CSFS
C      L     12      SELECTED 1-E EXCITED OCFS
C      I     13      CONDENSED CSF LABELS
C            14      (RESERVED)
C            15      (RESERVED)
C      D     16      ONE-ELECTRON INTEGRALS (T+V) IN MO BASIS
C      D     17      CAS-CI ENERGIES
C      D     18      CAS-CI EIGENVECTORS
C      I     19      LABEL: SQUARE MAT -> TRIANGLE MAT
C      D     20      ONE-ELECTRON PERTURBATION
C      D     21      ZERO-TH ORDER ENERGIES OF REFERENCE CSFS
C      D     22      ZERO=TH ORDER ENERGIES OF REFERENCE STATES
C      D     23      2ND ORDER EFFECTIVE HAMILTONIAN
C      D     24      EIGENVALUES OF EFFECTIVE HAMILTONIAN = 2ND
C                    ORDER ENERGIES OF MC-QDPT
C      D     25      EIGENVECTORS OF EFFECTIVE HAMILTONIAN
C
C      HERE D=DOUBLE PRECISION, I=INTEGER, L=LOGICAL DATA
C
C     OPEN DISK FILES.  NOTE THAT LUNJMA/MCQD56 IS OPENED ELSEWHERE...
C
      CALL MQOPDA(0)
      CALL SEQOPN(LUNFT0,'MCQD51','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQOPN(LUNFTA,'MCQD52','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQOPN(LUNFTB,'MCQD53','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQOPN(LUNFTC,'MCQD54','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQOPN(LUNOER,'MCQD55','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQOPN(LUNJM1,'MCQD57','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQOPN(LUNJM2,'MCQD58','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQOPN(LUNJM3,'MCQD59','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQOPN(LUNTWO,'MCQD60','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQOPN(LUNFT1,'MCQD61','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQOPN(LUNFT2,'MCQD62','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQOPN(LUNPUN,'MCQD63','UNKNOWN',.FALSE.,'FORMATTED')
      CALL SEQOPN(LUNCNF,'MCQD64','UNKNOWN',.FALSE.,'FORMATTED')
C
      IF(NOSYM.NE.0) THEN
         ISTSYM=1
         CALL SYMOFF
      END IF
C
C**** READ NUCLEAR REPULSION ENERGY ************************************
      ENUCLE=ENUC(NAT,ZAN,C)
C***********************************************************************
C**** MQREMO ***********************************************************
C****        ***********************************************************
      IF(INORB.NE.0) THEN
        STRSUB='MQREMO'
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
C**** ADDRESSING OF WORK AREA ******************************************
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C.... CMO    ...........................................................
        N001=MQADDR(NGO*NMO,'D')
C.... SINTEG FOR READ ..................................................
        N002=MQADDR(NGO*(NGO+1)/2,'D')
C.... SINTEG ...........................................................
        N003=MQADDR(NGO*NGO,'D')
        LQ=MQADDR(NGO*NGO,'D')
        LABMO=MQADDR(NGO,'I')
        LWRK=MQADDR(NGO,'D')
C
        LASTP1=MQMNXT()
        NEED1=LASTP1-N001
        CALL GETFM(NEED1)
C**** ERROR STOP *******************************************************
C       IDUM=MQMCHK()
C**** READ SINTEG ******************************************************
        NDSIZE=NGO*(NGO+1)/2
        CALL DAREAD(IDAF,IODA,D(N002),NDSIZE,12,0)
C**** TRIG TO SQU ******************************************************
        CALL MQTRSQ(NGO,D(N002),D(N003))
C     READ Q MATRIX
      NDSIZE=NGO*NGO
      CALL DAREAD(IDAF,IODA,D(LQ),NDSIZE,45,0)
      CALL MQREMO(LUNOUT,LPOUT,LUNIN,GRPV,NGO,NMO,INOFMT,D(N001),
     *            D(N003),IPURFY,NQMT,D(LQ),D(N002),D(LABMO),D(LWRK))
C**** WRITE INPUT MO ***************************************************
        NDSIZE=NGO*NMO
        CALL MQDAWR(IDAF50,IODA50,D(N001),NDSIZE,7,0)
C
        CALL RETFM(NEED1)
C**** CPU TIME *********************************************************
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (MASWRK) WRITE(LUNOUT,9100) STRSUB,TIME
        IF (MASWRK .AND. LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        CALL FLSHBF(LUNOUT)
      ELSE
C
C       PURIFY THE ORBITALS. AVOID DOING IT IF THE ORBITALS HAVE
C       ALREADY BEEN PURIFIED (IN THE FIRST PASS).
C
        IF(IPURFY.NE.0.AND..NOT.(CANAVE.AND.NOCC.NE.0)) THEN
C
          STRSUB='MQCLMO'
          IF(TRACE) WRITE(LUNOUT,9000) STRSUB
          CALL TSECND(STIME)
          WALL1=WALL
C
          CALL VALFM(LOADFM)
          IDUM=MQINIT(LOADFM+1)
          LS=MQADDR((NGO*NGO+NGO)/2,'D')
          LQ=MQADDR(NGO*NGO,'D')
          LV=MQADDR(NGO*NGO,'D')
          LABMO=MQADDR(NGO,'I')
          LWRK=MQADDR(NGO,'D')
C
          LASTP1=MQMNXT()
          NEED1=LASTP1-LS
          CALL GETFM(NEED1)
C         READ Q MATRIX
          NDSIZE=NGO*NGO
          CALL DAREAD(IDAF,IODA,D(LQ),NDSIZE,45,0)
C         READ OVERLAP S MATRIX
          NDSIZE=NGO*(NGO+1)/2
          CALL DAREAD(IDAF,IODA,D(LS),NDSIZE,12,0)
C         READ THE ORBITALS
          NDSIZE=NGO*NGO
          CALL DAREAD(IDAF,IODA,D(LV),NDSIZE,15,0)
          CALL MQCLMO(NGO,NMO,NQMT,D(LQ),D(LS),D(LV),D(LABMO),D(LWRK))
C         WRITE THE ORBITALS
          NDSIZE=NGO*NGO
          CALL DAWRIT(IDAF,IODA,D(LV),NDSIZE,15,0)
          CALL RETFM(NEED1)
C
          CALL TSECND(ETIME)
          TIME=ETIME-STIME
          WALL1=WALL-WALL1
          IF (MASWRK) WRITE(LUNOUT,9100) STRSUB,TIME
          IF (MASWRK .AND. LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
          CALL FLSHBF(LUNOUT)
        ENDIF
      END IF
C***********************************************************************
C**** MQMOSY (1) *******************************************************
C****        ***********************************************************
      STRSUB='MQMOSY'
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      CALL TSECND(STIME)
      WALL1=WALL
C**** ADDRESSING OF WORK AREA ******************************************
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... MOSYM  ...........................................................
      N001=MQADDR(NMO,'I')
C.... Q      ...........................................................
      N002=MQADDR(NGO*NGO,'D')
C.... S      ...........................................................
      N003=MQADDR(NGO*(NGO+1)/2,'D')
C.... V (INPUT MO) .....................................................
      N004=MQADDR(NGO*NGO,'D')
C.... T      ...........................................................
      N005=MQADDR(NGO,'D')
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-N001
      CALL GETFM(NEED1)
C**** ERROR STOP *******************************************************
C     IDUM=MQMCHK()
C**** READ S, Q, V *****************************************************
      NDSIZE=NGO*(NGO+1)/2
      CALL DAREAD(IDAF,IODA,D(N003),NDSIZE,12,0)
      NDSIZE=NGO*NGO
      CALL DAREAD(IDAF,IODA,D(N002),NDSIZE,45,0)
      IF(INORB.NE.0) THEN
        NDSIZE=NGO*NMO
        CALL MQDARE(IDAF50,IODA50,D(N004),NDSIZE,7,0)
      ELSE
        NDSIZE=NGO*NGO
        CALL DAREAD(IDAF,IODA,D(N004),NDSIZE,15,0)
      END IF
      CALL MQMOSY(LUNOUT,LPOUT,1,NGO,NMO,
     *            D(N001),D(N002),D(N003),D(N004),D(N005))
      IF (MASWRK) WRITE(LUNOUT,9310) NMOFZC+1,NMOFZC+NMODOC+NMOACT,
     *                        NMOFZC+NMODOC+1,NMOFZC+NMODOC+NMOACT
 9310 FORMAT(1X,'THE CORRELATED ORBITALS RUN FROM',I6,' TO',I6/
     *       1X,'THE     ACTIVE ORBITALS RUN FROM',I6,' TO',I6)
C**** WRITE MOSYM ******************************************************
      NDSIZE=MQDSIZ(NMO,'I')
      CALL MQDAWR(IDAF50,IODA50,D(N001),NDSIZE,10,1)
C
      CALL RETFM(NEED1)
C**** CPU TIME *********************************************************
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF (TRACE) WRITE(LUNOUT,9100) STRSUB,TIME
      IF (TRACE .AND. LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
      CALL FLSHBF(LUNOUT)
C***********************************************************************
C**** MQSORD ***********************************************************
C****        ***********************************************************
      STRSUB='MQSORD'
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      CALL TSECND(STIME)
      WALL1=WALL
C**** ADDRESSING OF WORK AREA ******************************************
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... CFMONW ...........................................................
      N001=MQADDR(NGO*NMO,'D')
C.... CFMOOD ...........................................................
      N002=MQADDR(NGO*NGO,'D')
C.... MOSYMN ...........................................................
      N003=MQADDR(NMO,'I')
C.... MOSYMO ...........................................................
      N004=MQADDR(NMO,'I')
      LQ=MQADDR(NGO*NGO,'D')
      LS=MQADDR(NGO*NGO,'D')
      LWORK=MQADDR(NGO,'D')
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-N001
      CALL GETFM(NEED1)
C**** ERROR STOP *******************************************************
C     IDUM=MQMCHK()
C**** READ CFMOOD, MOSYMO **********************************************
C.... CFMOOD ...........................................................
      IF(INORB.NE.0) THEN
        NDSIZE=NGO*NMO
        CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,7,0)
      ELSE
        NDSIZE=NGO*NGO
        CALL DAREAD(IDAF,IODA,D(N002),NDSIZE,15,0)
      END IF
C.... MOSYMO ...........................................................
      NDSIZE=MQDSIZ(NMO,'I')
      CALL MQDARE(IDAF50,IODA50,D(N004),NDSIZE,10,1)
C     READ OVERLAP INTEGRALS
      NDSIZE=(NGO*(NGO+1))/2
      CALL DAREAD(IDAF,IODA,D(LS),NDSIZE,12,0)
      CALL MQSORD(LUNOUT,LPOUT,
     *            NGO,NMO,NMOFZC,NMODOC,NMOACT,NMOEXT,IPURFY,
     *            D(N001),D(N002),D(N003),D(N004),D(LS),D(LWORK))
C**** WRITE CFMONW, MOSYMN *********************************************
C.... CFMONW ...........................................................
      NDSIZE=NGO*NMO
      CALL MQDAWR(IDAF50,IODA50,D(N001),NDSIZE,7,0)
C.... MOSYMN ...........................................................
      NDSIZE=MQDSIZ(NMO,'I')
      CALL MQDAWR(IDAF50,IODA50,D(N003),NDSIZE,10,1)
C
      CALL RETFM(NEED1)
C**** CPU TIME *********************************************************
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF (TRACE) WRITE(LUNOUT,9100) STRSUB,TIME
      IF (TRACE .AND. LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
      CALL FLSHBF(LUNOUT)
C***********************************************************************
C**** MQCONF ***********************************************************
C****        ***********************************************************
      STRSUB='MQCONF'
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      CALL TSECND(STIME)
      WALL1=WALL
C     NDOUB=NMOFZC+NMODOC
      NORB =NMOFZC+NMODOC+NMOACT
C**** ADDRESSING OF WORK AREA ******************************************
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... OCCMIN ...........................................................
      N001=MQADDR(NORB,'I')
C.... OCCMAX ...........................................................
      N002=MQADDR(NORB,'I')
C.... OCC    ...........................................................
      N003=MQADDR(MAXROW,'I')
C.... K      ...........................................................
      N004=MQADDR(3*MAXROW,'I')
C.... L      ...........................................................
      N005=MQADDR(3*MAXROW,'I')
C.... NJ     ...........................................................
      N006=MQADDR(NORB+1,'I')
C.... NJSKP  ...........................................................
      N007=MQADDR(NORB+1,'I')
C.... XBAR   ...........................................................
      N008=MQADDR(MAXROW,'I')
C.... YP     ...........................................................
      N009=MQADDR(3*MAXROW,'I')
C.... XP     ...........................................................
      N010=MQADDR(MAXROW,'I')
C.... CASE   ...........................................................
      N011=MQADDR(NORB,'I')
C.... BROW   ...........................................................
      N012=MQADDR(NORB+1,'I')
C.... YTOT   ...........................................................
      N013=MQADDR(MAXROW,'I')
C.... Y      ...........................................................
      N014=MQADDR(3*MAXROW,'I')
C.... NSPINF ...........................................................
      N015=MQADDR(21*22,'I')
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-N001
      CALL GETFM(NEED1)
      CALL MQCONF(LUNOUT,LPOUT ,
     *            MULT  ,NEL   ,NORB  ,MAXROW,NDOUB,
     *            D(N001),D(N002),
     *            D(N003),D(N004),D(N005),D(N006),D(N007),D(N008),
     *            D(N009),D(N010),D(N011),D(N012),D(N013),D(N014),
     *            D(N015),
     *            NOCF  ,MAXSOC,MAXSPF,NCSF  ,NOCFI)
C
      CALL RETFM(NEED1)
C.... WRITE CONFIGURATION ..............................................
      INIACT=NDOUB+1
      LASACT=NDOUB+NMOACT
C
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C
C.... SMAP   ...........................................................
      N001=MQADDR((MAXSOC+1)*MAXSPF,'I')
C.... OMAP   ...........................................................
      N002=MQADDR((NMOACT+1)*NOCF,'I')
C.... NSNSF  ...........................................................
      N003=MQADDR(NOCF+1,'I')
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-N001
      CALL GETFM(NEED1)
C
C     IDUM=MQMCHK()
      NDSIZE=MQDSIZ((MAXSOC+1)*MAXSPF,'I')
      CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,2,1)
      NDSIZE=MQDSIZ((NMOACT+1)*NOCF,'I')
      CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,1,1)
      NDSIZE=MQDSIZ(NOCF+1,'I')
      CALL MQDARE(IDAF50,IODA50,D(N003),NDSIZE,3,1)
C
      CALL MQPCNF(LUNCNF,MAXSOC,MAXSPF,INIACT,LASACT,NOCF,
     *            D(N001),D(N002),D(N003))
C
      CALL RETFM(NEED1)
C**** CPU TIME *********************************************************
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF (TRACE) WRITE(LUNOUT,9100) STRSUB,TIME
      IF (TRACE .AND. LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
      CALL FLSHBF(LUNOUT)
C***********************************************************************
C**** MQCIG1 ***********************************************************
C****        ***********************************************************
      STRSUB='MQCIG1'
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      IF(MASWRK) WRITE(LUNOUT,9320)
 9320 FORMAT(1X,'EVALUATING ONE-PARTICLE COUPLING CONSTANTS FOR',
     *          ' CAS-CI...')
      CALL TSECND(STIME)
      WALL1=WALL
C**** ADDRESSING OF WORK AREA ******************************************
C     NDOUB=NMOFZC+NMODOC
C     NORB =NMOFZC+NMODOC+NMOACT
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... IOMAP  ...........................................................
      N001=MQADDR((NMOACT+1)*NOCF,'I')
C.... ISMAP  ...........................................................
      N002=MQADDR((MAXSOC+1)*MAXSPF,'I')
C.... IOCSF  ...........................................................
      N003=MQADDR(2*NCSF,'I')
C.... LAB1   ...........................................................
      N004=MQADDR(4*MAXCSF,'I')
C.... WORK   ...........................................................
      N005=MQADDR(MAXCSF,'D')
C.... I1EX1  ...........................................................
      N006=MQADDR(NMOACT*(NOCF+1),'I')
C.... I1EX2  ...........................................................
      N007=MQADDR(NMOACT*(NOCFI+1),'I')
C.... NSNSF  ...........................................................
      N008=MQADDR(NOCF+1,'I')
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-N001
      CALL GETFM(NEED1)
C**** ERROR STOP *******************************************************
C     IDUM=MQMCHK()
C**** READ IOMAP, ISMAP, IOCSF, I1EX1, I1EX2, NSNSF ********************
      NDSIZE=MQDSIZ((NMOACT+1)*NOCF,'I')
      CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,1,1)
      NDSIZE=MQDSIZ((MAXSOC+1)*MAXSPF,'I')
      CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,2,1)
      NDSIZE=MQDSIZ(2*NCSF,'I')
      CALL MQDARE(IDAF50,IODA50,D(N003),NDSIZE,4,1)
      NDSIZE=MQDSIZ(NMOACT*(NOCF+1),'I')
      CALL MQDARE(IDAF50,IODA50,D(N006),NDSIZE,5,1)
      NDSIZE=MQDSIZ(NMOACT*(NOCFI+1),'I')
      CALL MQDARE(IDAF50,IODA50,D(N007),NDSIZE,6,1)
      NDSIZE=MQDSIZ(NOCF+1,'I')
      CALL MQDARE(IDAF50,IODA50,D(N008),NDSIZE,3,1)
C
      IF(EXETYP.NE.CHECK) THEN
         CALL MQCIG1(LUNOUT,LPOUT ,LUNFT0,MAXCSF,NDOUB ,
     *               NORB  ,NOCF  ,MAXSOC,MAXSPF,NCSF  ,
     *               NOCFI ,NELM  ,
     *               D(N001),D(N002),D(N003),D(N004),D(N005),
     *               D(N006),D(N007),D(N008))
      END IF
C     CALL MQCIG1(LUNOUT,LPOUT ,LUNFTP,MAXCSF,NDOUB ,
C    *            NORB  ,NOCF  ,MAXSOC,MAXSPF,NCSF  ,
C    *            NOCFI ,NELM  ,
C    *            IOMAP ,ISMAP ,IOCSF ,LAB1  ,WORK  ,
C    *            I1EX1 ,I1EX2 ,NSNSF )
C
      CALL RETFM(NEED1)
C**** CPU TIME *********************************************************
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF (TRACE) THEN
         WRITE(LUNOUT,9100) STRSUB,TIME
         IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
      ELSE
         IF(MASWRK) WRITE(LUNOUT,9330) TIME
      END IF
 9330 FORMAT(1X,'CAS-CI COUPLING CONSTANTS TOOK',F8.1,' SECONDS.')
      CALL FLSHBF(LUNOUT)
C***********************************************************************
C**** MQPTG1 ***********************************************************
C****        ***********************************************************
      STRSUB='MQPTG1'
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      IF(MASWRK) WRITE(LUNOUT,9340)
 9340 FORMAT(1X,'EVALUATING ONE-PARTICLE COUPLING CONSTANTS FOR PT...')
      CALL TSECND(STIME)
      WALL1=WALL
C**** ADDRESSING OF WORK AREA ******************************************
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... IOMAP  ...........................................................
      N001=MQADDR((NMOACT+1)*NOCF,'I')
C.... ISMAP  ...........................................................
      N002=MQADDR((MAXSOC+1)*MAXSPF,'I')
C.... NSNSF  ...........................................................
      N003=MQADDR(NOCF+1,'I')
C.... I1EX1  ...........................................................
      N004=MQADDR(NMOACT*(NOCF+1),'I')
C.... I1EX2  ...........................................................
      N005=MQADDR(NMOACT*(NOCFI+1),'I')
C.... LAB1   ...........................................................
      N006=MQADDR(3*LENGTH,'I')
C.... WORK   ...........................................................
      N007=MQADDR(LENGTH,'D')
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-N001
      CALL GETFM(NEED1)
C**** ERROR STOP *******************************************************
C     IDUM=MQMCHK()
C**** READ IOMAP, ISMAP, NSNSF, I1EX1, I1EX2 ***************************
      NDSIZE=MQDSIZ((NMOACT+1)*NOCF,'I')
      CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,1,1)
      NDSIZE=MQDSIZ((MAXSOC+1)*MAXSPF,'I')
      CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,2,1)
      NDSIZE=MQDSIZ(NOCF+1,'I')
      CALL MQDARE(IDAF50,IODA50,D(N003),NDSIZE,3,1)
      NDSIZE=MQDSIZ(NMOACT*(NOCF+1),'I')
      CALL MQDARE(IDAF50,IODA50,D(N004),NDSIZE,5,1)
      NDSIZE=MQDSIZ(NMOACT*(NOCFI+1),'I')
      CALL MQDARE(IDAF50,IODA50,D(N005),NDSIZE,6,1)
C**** CALL MQPTG1 ******************************************************
      IF(EXETYP.NE.CHECK) THEN
         CALL MQPTG1(LUNOUT,LPOUT ,LUNFTA,
     *               NDOUB ,NORB  ,NCSF  ,NOCF  ,MAXSOC,MAXSPF,
     *               LENGTH,NOCFI ,
     *               D(N001),D(N002),D(N003),D(N004),D(N005),
     *               D(N006),D(N007))
      END IF
C     CALL MQPTG1(LUNOUT,LPOUT ,LUNFTP,
C    *            NDOUB ,NORB  ,NCSF  ,NOCF  ,MAXSOC,MAXSPF,
C    *            LENGTH,NOCFI ,
C    *            IOMAP ,ISMAP ,NSNSF ,I1EX1 ,I1EX2 ,
C    *            LAB1  ,WORK  )
C
      CALL RETFM(NEED1)
C**** CPU TIME *********************************************************
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF (TRACE) THEN
         WRITE(LUNOUT,9100) STRSUB,TIME
         IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
      ELSE
         IF(MASWRK) WRITE(LUNOUT,9350) TIME
      END IF
 9350 FORMAT(1X,'PT COUPLING CONSTANTS TOOK',F8.1,' SECONDS.')
      CALL FLSHBF(LUNOUT)
C***********************************************************************
C**** MQSYM ************************************************************
C****       ************************************************************
      STRSUB='MQSYM '
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      CALL TSECND(STIME)
      WALL1=WALL
      INIACT=NDOUB+1
      LASACT=NDOUB+NMOACT
C**** ADDRESSING OF WORK AREA ******************************************
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... IOMAP  ...........................................................
      N001=MQADDR((NMOACT+1)*NOCF,'I')
C.... NSNSF  ...........................................................
      N002=MQADDR(NOCF+1,'I')
C.... KREF   ...........................................................
      N003=MQADDR(NCSF,'L')
C.... MOSYM  ...........................................................
      N004=MQADDR(NMO,'I')
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-N001
      CALL GETFM(NEED1)
C**** ERROR STOP *******************************************************
C     IDUM=MQMCHK()
C**** READ IOMAP, NSNSF, MOSYM *****************************************
      NDSIZE=MQDSIZ((NMOACT+1)*NOCF,'I')
      CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,1,1)
      NDSIZE=MQDSIZ(NOCF+1,'I')
      CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,3,1)
      NDSIZE=MQDSIZ(NMO,'I')
      CALL MQDARE(IDAF50,IODA50,D(N004),NDSIZE,10,1)
C**** CALL MQSYM *******************************************************
      CALL MQSYM(LUNOUT,LPOUT ,INIACT,LASACT,NOCF  ,NCSF  ,
     *           ISTSYM,NMO   ,
     *           D(N001),D(N002),D(N004),D(N003))
C**** WRITE KREF *******************************************************
      NDSIZE=MQDSIZ(NCSF,'L')
      CALL MQDAWR(IDAF50,IODA50,D(N003),NDSIZE,11,1)
C
      CALL RETFM(NEED1)
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF (TRACE) WRITE(LUNOUT,9100) STRSUB,TIME
      IF (TRACE .AND. LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
C
      CALL TSECND(TIM1)
      TIME = TIM1 - TIM0
      TIM0 = TIM1
      IF(MASWRK) WRITE(LUNOUT,9360) TIME
 9360 FORMAT(1X,'CPU TIME TO SET UP SYMMETRY, GENERATE CSFS, AND',
     "          ' COUPLING CONSTANTS=',F10.2)
      CALL FLSHBF(LUNOUT)
C
C**** MQTRF (1) ********************************************************
C
      STRSUB='MQTRF '
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      CALL TSECND(STIME)
      WALL1=WALL
      IF(IFORB.EQ.0) THEN
         NMOTRF=NMO
      ELSE
         NMOTRF=NMOINT
      END IF
C**** ADDRESSING OF WORK AREA ******************************************
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... LIJGO  ...........................................................
      N001=MQADDR(NGO*NGO,'I')
C.... LIJMO  ...........................................................
C      N002=MQADDR(NMO*NMO,'I')
      N002=MQADDR(NMOTRF*NMOTRF,'I')
C.... LMOIJI ...........................................................
C      N003=MQADDR(NMO*(NMO+1)/2,'I')
      N003=MQADDR(NMOTRF*(NMOTRF+1)/2,'I')
C.... LMOIJJ ...........................................................
C      N004=MQADDR(NMO*(NMO+1)/2,'I')
      N004=MQADDR(NMOTRF*(NMOTRF+1)/2,'I')
C.... CFMO   ...........................................................
      N005=MQADDR(NGO*NGO,'D')
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-N001
      CALL GETFM(NEED1)
C**** ERROR STOP *******************************************************
C     IDUM=MQMCHK()
C**** READ MOLECULAR ORBITAL *******************************************
      NDSIZE=NGO*NMO
      CALL MQDARE(IDAF50,IODA50,D(N005),NDSIZE,7,0)
C**** CALL MQTRF *******************************************************
      ISWTCH=-1
      IF(IFORB.EQ.0) THEN
        IF(CANAVE.AND.NOCC.NE.0.AND.ICI.NE.1) THEN
C
C       SKIPPING MCQDPT TRANSFORMATION AS THE SAME SET OF CANONICAL
C       ORBITALS IS USED THROGHOUT AND THE INTEGRALS HAVE ALREADY BEEN
C       TRANSFORMED.  (DURING CANAVE.AND.NOCC.NE.0.AND.ICI.EQ.1 RUN)
C
           IF (MASWRK)
     *          WRITE(LUNOUT,*) 'SKIPPING THE TRANSFORMATION STEP.'
           NDSIZE=NMO*NMO
           CALL DAREAD(IDAF,IODA,D(N002),NMO*NMO,392,0)
           CALL MQDAWR(IDAF50,IODA50,D(N002),NDSIZE,16,0)
        ELSE
           IF (MASWRK)
     *          WRITE(LUNOUT,'(/1X,''TRANSFORMING INTEGRALS OVER ALL'',
     *          '' CANONICAL MCSCF ORBITALS.''/)')
        CALL MQTRF(LUNOUT,LPOUT ,ISWTCH,LUNERI,LUNOER,LUNJMA,
     *             LUNJM1,LUNJM2,LUNJM3,NGO   ,NMOINT,TRACE,
     *             NMO   ,MAXERI,MXTRFR,THRERI,PARAIO,DELSCR,
     *             D(N001),D(N002),D(N003),D(N004),D(N005),NMOTRF,
     *             OERFLG,DOORD0)
C
C         WE NEED TO SAVE 1E MO INTEGRALS SINCE MQOPDA DESTROYS ALL
C         RECORDS, THUS INFO IN IDAF50 CANNOT BE PASSED BETWEEN
C         DIFFERENT MCQD GROUPS (BUT 2E INTEGRALS COULD BE AS THEY
C         ARE STORED IN SEQ FILES)
C
          IF(CANAVE.AND.NOCC.NE.0.AND.ICI.EQ.1) THEN
            NDSIZE=NMO*NMO
            CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,16,0)
            CALL DAWRIT(IDAF,IODA,D(N002),NMO*NMO,392,0)
          ENDIF
        ENDIF
      ELSE
        IF (MASWRK) WRITE(LUNOUT,'(/1X,''TRANSFORMING INTEGRALS OVER'',
     *         '' OCCUPIED, UNCANONICAL MCSCF ORBITALS.''/)')
        CALL MQTRF(LUNOUT,LPOUT ,ISWTCH,LUNERI,LUNOER,LUNJMA,
     *             LUNJM1,LUNJM2,LUNJM3,NGO   ,NMOINT,TRACE,
     *             NMO   ,MAXERI,MXTRFR,THRERI,PARAIO,DELSCR,
     *             D(N001),D(N002),D(N003),D(N004),D(N005),NMOTRF,
     *             OERFLG,DOORD0)
      END IF
C
      CALL RETFM(NEED1)
C**** CPU TIME *********************************************************
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF (TRACE) THEN
         WRITE(LUNOUT,9100) STRSUB,TIME
         IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
      END IF
C
      CALL TSECND(TIM1)
      TIME = TIM1 - TIM0
      TIM0 = TIM1
      IF(MASWRK) WRITE(LUNOUT,9370) TIME
 9370 FORMAT(1X,'CPU TIME TO TRANSFORM INTEGRALS=',F10.2)
      CALL FLSHBF(LUNOUT)
C
C     MAKE CANONICAL FOCK ORBITAL OR NOT
C
      IF(IFORB.NE.0) THEN
        IF (MASWRK) WRITE(LUNOUT,9380)
 9380 FORMAT(/1X,'SOLVING FOR CAS-CI STATE FUNCTIONS.'/)
        STRSUB='MQCACI'
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
        MAXDIA=MAX(MDI,MXBASE)
        INIMO=NDOUB+1
        LASMO=NDOUB+NMOACT
        NMOAA=NMOACT*(NMOACT+1)/2
        NMOEA=NMOAA
C**** ADDRESSING OF WORK AREA ******************************************
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C.... EIGVAL ...........................................................
        N001=MQADDR(NSTATE*3,'D')
C.... EIGVEC ...........................................................
        N002=MQADDR(NCSF*NSTATE,'D')
C.... LWCD   ...........................................................
        N003=MQADDR(2*MAXERI,'I')
C.... WORKER ...........................................................
        N004=MQADDR(MAXERI,'D')
C.... LIJMO  ...........................................................
        N005=MQADDR(NMOACT*NMOACT,'I')
C.... HCORE  ...........................................................
        N006=MQADDR(NMO*NMO,'D')
C.... VONEEL ...........................................................
        N007=MQADDR(NMOACT*NMOACT,'D')
C.... VTWOEL ...........................................................
        N008=MQADDR(NMOEA*NMOAA,'D')
C.... LABEL1 ...........................................................
        N009=MQADDR(4*MAXCSF,'I')
C.... WORKGE ...........................................................
        N010=MQADDR(MAXCSF,'D')
C.... DIAHMA ...........................................................
        N011=MQADDR(MAXDIA*(MAXDIA+1)/2,'D')
C.... DIAVAL ...........................................................
        N012=MQADDR(MAXDIA,'D')
C.... DIAVEC ...........................................................
        N013=MQADDR(MAXDIA*MAXDIA,'D')
C.... DIAWRK ...........................................................
        N014=MQADDR(MAXDIA*8,'D')
C.... HMAT   ...........................................................
        N015=MQADDR(MAXDIA*MAXDIA,'D')
C.... LORDER ...........................................................
        N016=MQADDR(MAXDIA,'I')
C.... HMATDI ...........................................................
        N017=MQADDR(NCSF,'D')
C.... LNWOLD ...........................................................
        N018=MQADDR(NCSF,'I')
C.... LODNEW ...........................................................
        N019=MQADDR(NCSF,'I')
C.... BASE   ...........................................................
        N020=MQADDR(NCSF*MXBASE,'D')
C.... HXBASE ...........................................................
        N021=MQADDR(NCSF*MXBASE,'D')
C.... WRKVEC ...........................................................
        N022=MQADDR(NCSF,'D')
C.... EBEFOR ...........................................................
        N023=MQADDR(NSOLUT,'D')
C.... DIFENE ...........................................................
        N024=MQADDR(NSOLUT,'D')
C.... DIFVEC ...........................................................
        N025=MQADDR(NSOLUT,'D')
C.... LKCONV ...........................................................
        N026=MQADDR(NSOLUT,'I')
C.... KREF   ...........................................................
        N027=MQADDR(NCSF,'L')
C.... EIGVOD ...........................................................
        N028=MQADDR(NCSF*NSTATE,'D')
C
        LASTP1=MQMNXT()
        NEED1=LASTP1-N001
        CALL GETFM(NEED1)
C --- ADDITIONAL WORK AREA ---
        CALL GOTFM(NGOTMX)
        CACIOP=0
        NREQ1=NWFT0*2/NWDVAR+1+NWFT0P*4/NWDVAR+1+NWFT0P
        IF (LPOUT.LT.0 .AND. MASWRK) THEN
           WRITE(LUNOUT,*) 'NWFT0  = ',NWFT0
           WRITE(LUNOUT,*) 'NWFT0P = ',NWFT0P
           WRITE(LUNOUT,*) 'NGOTMX = ',NGOTMX
           WRITE(LUNOUT,*) 'NREQ1  = ',NREQ1
        END IF
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
        IF (NREQ1.GT.0 .AND. NGOTMX.GT.NREQ1) THEN
           CACIOP=1
           N101=MQADDR(NWFT0*2,'I')
           N102=MQADDR(NWFT0P*4,'I')
           N103=MQADDR(NWFT0P,'D')
        ELSE
           CACIOP=0
           N101=MQADDR(2,'I')
           N102=MQADDR(4,'I')
           N103=MQADDR(1,'D')
        END IF
        LASTP1=MQMNXT()
        NEED2=LASTP1-N101
        CALL GETFM(NEED2)
C**** ERROR STOP *******************************************************
C       IDUM=MQMCHK()
C**** READ ONE-ELECTRON INTEGRALS ON MO BASE ***************************
C.... HCORE  ...........................................................
        NDSIZE=NMO*NMO
        CALL MQDARE(IDAF50,IODA50,D(N006),NDSIZE,16,0)
C.... KREF   ...........................................................
        NDSIZE=MQDSIZ(NCSF,'L')
        CALL MQDARE(IDAF50,IODA50,D(N027),NDSIZE,11,1)
C**** CALL MQCACI ******************************************************
        IF(EXETYP.NE.CHECK) THEN
           CALL MQCACI(LUNOUT,LPOUT ,0     ,LUNJM1,LUNFT0,
     *                 NMO   ,NDOUB ,NMOACT,INIMO ,LASMO ,
     *                 NMOEA ,NMOAA ,
     *                 NCSF  ,
     *                 MAXDIA,NSTCI ,NSTATE,MDI   ,MAXERI,MAXCSF,
     *                 MXBASE,NSOLUT,NSTOP ,
     *                 ENUCLE,THRCON,THRENE,
     *                 MAINCS,
     *                 D(N001),D(N002),D(N003),D(N004),
     *                 D(N005),D(N006),D(N007),D(N008),D(N009),D(N010),
     *                 D(N011),D(N012),D(N013),D(N014),D(N015),D(N016),
     *                 D(N017),D(N018),D(N019),
     *                 D(N020),D(N021),D(N022),
     *                 D(N023),D(N024),D(N025),D(N026),
     *                 D(N027),KSTATE ,D(N028),AVECOE,AVGEN1,
     *                 CACIOP ,D(N101),D(N102),D(N103))
        END IF
C
C           SAVE CAS-CI ENERGIES AND EIGENVECTORS
C
        NDSIZE=NSTATE
        CALL MQDAWR(IDAF50,IODA50,D(N001),NDSIZE,17,0)
        NDSIZE=NCSF*NSTATE
        CALL MQDAWR(IDAF50,IODA50,D(N002),NDSIZE,18,0)
C
C           PRINT CI ENERGIES AND EIGENVECTORS
C
        M001=MQADDR((NMOACT+1)*NOCF,'I')
        M002=MQADDR((MAXSOC+1)*MAXSPF,'I')
        M003=MQADDR(2*NCSF,'I')
        M004=MQADDR(NMOACT,'I')
        LASTP1=MQMNXT()
        NEEDM=LASTP1-M001
        CALL GETFM(NEEDM)
        IF(EXETYP.NE.CHECK) THEN
           NDSIZE=MQDSIZ((NMOACT+1)*NOCF,'I')
           CALL MQDARE(IDAF50,IODA50,D(M001),NDSIZE,1,1)
           NDSIZE=MQDSIZ((MAXSOC+1)*MAXSPF,'I')
           CALL MQDARE(IDAF50,IODA50,D(M002),NDSIZE,2,1)
           NDSIZE=MQDSIZ(2*NCSF,'I')
           CALL MQDARE(IDAF50,IODA50,D(M003),NDSIZE,4,1)
           CALL MQPRWF(LUNOUT,NCSF,NSTATE,NOCF,MAXSOC,MAXSPF,NMOACT,
     *                 D(N001),D(N002),D(M001),D(M002),D(M003),D(M004))
        END IF
        CALL RETFM(NEEDM)
        CALL RETFM(NEED2)
        CALL RETFM(NEED1)
C
C          PRINT CPU USED
C
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (TRACE) THEN
           WRITE(LUNOUT,9100) STRSUB,TIME
           IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        END IF
C
        CALL TSECND(TIM1)
        TIME = TIM1 - TIM0
        TIM0 = TIM1
        IF(MASWRK) WRITE(LUNOUT,9390) TIME
        CALL FLSHBF(LUNOUT)
 9390 FORMAT(1X,'CPU TIME TO GENERATE REFERENCE CI STATES =',F10.2)
C
C***********************************************************************
C**** MQFORB ***********************************************************
C****        ***********************************************************
        STRSUB='MQFORB'
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
        N=MAX(NDOUB,NMOACT)
        MAXDIA=MAX(N,NMOEXT)
        NMOINT=NDOUB+NMOACT
C**** ADDRESSING OF WORK AREA ******************************************
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C.... EIGVEC ...........................................................
        N001=MQADDR(NCSF*NSTATE,'D')
C.... AVECOE ...........................................................
C       N002
C.... CMOOLD ...........................................................
C       N003=N002+NSTATE
        N003=MQADDR(NGO*NGO,'D')
C.... CMONEW ...........................................................
        N004=MQADDR(NGO*NMO,'D')
C.... EORB   ...........................................................
        N005=MQADDR(NMO,'D')
C.... HCORE  ...........................................................
        N006=MQADDR(NMO*NMO,'D')
C.... GEN1AO ...........................................................
        N007=MQADDR(NGO*NGO,'D')
C.... GEN1   ...........................................................
        N008=MQADDR(NMOACT*NMOACT,'D')
C.... GFCKAO ...........................................................
        N009=MQADDR(NGO*NGO,'D')
C.... GFOCK  ...........................................................
        N010=MQADDR(NMO*NMO,'D')
C.... XWORK  ...........................................................
        N011=MQADDR(NINTMX,'D')
C.... IXWORK ...........................................................
C   ALLOCATE THIS INTEGER BUFFER AS REAL IN CASE OF 2 BYTE LABELS
        N012=MQADDR(NINTMX,'D')
C.... LABEL1 ...........................................................
        N013=MQADDR(4*MAXCSF,'I')
C.... WORKGE ...........................................................
        N014=MQADDR(MAXCSF,'D')
C.... DIAHMA ...........................................................
        N015=MQADDR(MAXDIA*(MAXDIA+1)/2,'D')
C.... DIAVAL ...........................................................
        N016=MQADDR(MAXDIA,'D')
C.... DIAVEC  ..........................................................
        N017=MQADDR(MAXDIA*MAXDIA,'D')
C.... DIAWRK ...........................................................
        N018=MQADDR(MAXDIA*8,'D')
C.... LORDER ...........................................................
        N019=MQADDR(MAXDIA,'I')
C.... MOSYM  ...........................................................
        N020=MQADDR(NMO,'I')
C.... IDIAWK ...........................................................
        N021=MQADDR(MAXDIA,'I')
C
        LASTP1=MQMNXT()
        NEED1=LASTP1-N001
        CALL GETFM(NEED1)
C**** ERROR STOP *******************************************************
C       IDUM=MQMCHK()
C**** READ CAS-CI EIGENVECTORS *****************************************
        NDSIZE=NCSF*NSTATE
        CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,18,0)
C**** READ MOLECULAR ORBITAL *******************************************
        NDSIZE=NGO*NMO
        CALL MQDARE(IDAF50,IODA50,D(N003),NDSIZE,7,0)
C**** READ ONE-ELECTRON INTEGRALS ON MO BASE ***************************
        NDSIZE=NMO*NMO
        CALL MQDARE(IDAF50,IODA50,D(N006),NDSIZE,16,0)
C**** READ MOLECULAR SYMMETRY ******************************************
        NDSIZE=MQDSIZ(NMO,'I')
        CALL MQDARE(IDAF50,IODA50,D(N020),NDSIZE,10,1)
C**** CALL MQFORB ******************************************************
        IF(EXETYP.NE.CHECK) THEN
           CALL MQFORB(LUNOUT,LPOUT ,LUNERI,LUNFT0,LUNPUN,NCSF  ,
     *                 NSTATE,NMO   ,NDOUB ,NMOACT,NGO   ,MAXERI,
     *                 MAXCSF,MAXDIA,ITYPE,INOFMT,
     *                 D(N001),AVECOE ,D(N003),D(N004),D(N005),D(N006),
     *                 D(N007),D(N008),D(N009),D(N010),D(N011),D(N012),
     *                 D(N013),D(N014),D(N015),D(N016),D(N017),D(N018),
     *                 D(N021),
     *                 D(N019),D(N020),CANAVE)
        END IF
C**** WRITE FOCK ORBITAL AND ENERGY ************************************
        NDSIZE=NGO*NMO
        CALL MQDAWR(IDAF50,IODA50,D(N004),NDSIZE,8,0)
        NDSIZE=NMO
        CALL MQDAWR(IDAF50,IODA50,D(N005),NDSIZE,9,0)
C       WRITE FAKE HAMILTONIAN AND ORBITAL ENERGIES
C       IF(CANAVE.AND.NOCC.EQ.0) THEN
C          CALL MQDAWR(IDAF50,IODA50,D(N001),NSTATE*NSTATE*4,23,0)
C          CALL MQDAWR(IDAF50,IODA50,D(N001),NSTATE,24,0)
C       ENDIF
C
        CALL RETFM(NEED1)
C**** CPU TIME *********************************************************
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (TRACE) THEN
           WRITE(LUNOUT,9100) STRSUB,TIME
           IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        END IF
        CALL FLSHBF(LUNOUT)
C
C**** MQMXOL ***********************************************************
C
        IF(CANAVE.AND.NOCC.EQ.0) GOTO 1000
        STRSUB='MQMXOL'
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
        INIACT=NDOUB+1
        LASACT=NDOUB+NMOACT
C**** ADDRESSING OF WORK AREA ******************************************
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C.... CMOINP ...........................................................
        N001=MQADDR(NGO*NGO,'D')
C.... CMOOLD ...........................................................
        N002=MQADDR(NGO*NMO,'D')
C.... CMONEW ...........................................................
        N003=MQADDR(NGO*NMO,'D')
C.... EORBOD ...........................................................
        N004=MQADDR(NMO,'D')
C.... EORBNW ...........................................................
        N005=MQADDR(NMO,'D')
C.... SINTEG FOR READ ..................................................
        N006=MQADDR(NGO*(NGO+1)/2,'D')
C.... SINTEG ...........................................................
        N007=MQADDR(NGO*NGO,'D')
C.... OVRLAP ...........................................................
        N008=MQADDR(NMO,'D')
C.... LORDER ...........................................................
        N009=MQADDR(NMO,'I')
C
        LASTP1=MQMNXT()
        NEED1=LASTP1-N001
        CALL GETFM(NEED1)
C**** ERROR STOP *******************************************************
C       IDUM=MQMCHK()
C**** READ SINTEG ******************************************************
        NDSIZE=NGO*(NGO+1)/2
        CALL DAREAD(IDAF,IODA,D(N006),NDSIZE,12,0)
C**** TRIG TO SQU ******************************************************
        CALL MQTRSQ(NGO,D(N006),D(N007))
C**** READ INPUT MOLECULAR AND FOCK ORBITALS ***************************
        NDSIZE=NGO*NMO
        CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,7,0)
        NDSIZE=NGO*NMO
        CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,8,0)
        NDSIZE=NMO
        CALL MQDARE(IDAF50,IODA50,D(N004),NDSIZE,9,0)
C
        IF(EXETYP.EQ.CHECK) THEN
           CALL VCLR(D(N003),1,NGO*NMO)
           CALL VCLR(D(N005),1,NMO)
        ELSE
           CALL MQMXOL(LUNOUT,LPOUT ,LUNPUN,
     *                 NGO   ,NMO   ,INIACT,LASACT,INOFMT,
     *                 D(N001),D(N002),D(N003),D(N004),D(N005),D(N007),
     *                 D(N008),D(N009))
        END IF
C
C**** WRITE ORBITALS AND THEIR ENERGIES ********************************
        NDSIZE=NGO*NMO
        CALL MQDAWR(IDAF50,IODA50,D(N003),NDSIZE,8,0)
        NDSIZE=NMO
        CALL MQDAWR(IDAF50,IODA50,D(N005),NDSIZE,9,0)
C
        CALL RETFM(NEED1)
C**** CPU TIME *********************************************************
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (TRACE) THEN
           WRITE(LUNOUT,9100) STRSUB,TIME
           IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        END IF
        CALL FLSHBF(LUNOUT)
C***********************************************************************
C**** MQMOSY (2) *******************************************************
C****            *******************************************************
        STRSUB='MQMOSY'
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
C**** ADDRESSING OF WORK AREA ******************************************
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C.... MOSYM  ...........................................................
        N001=MQADDR(NMO,'I')
C.... Q      ...........................................................
        N002=MQADDR(NGO*NGO,'D')
C.... S      ...........................................................
        N003=MQADDR(NGO*(NGO+1)/2,'D')
C.... V (INPUT MO) .....................................................
        N004=MQADDR(NGO*NGO,'D')
C.... T      ...........................................................
        N005=MQADDR(NGO,'D')
C
        LASTP1=MQMNXT()
        NEED1=LASTP1-N001
        CALL GETFM(NEED1)
C**** ERROR STOP *******************************************************
C       IDUM=MQMCHK()
C**** READ S, Q, V *****************************************************
        NDSIZE=NGO*(NGO+1)/2
        CALL DAREAD(IDAF,IODA,D(N003),NDSIZE,12,0)
        NDSIZE=NGO*NGO
        CALL DAREAD(IDAF,IODA,D(N002),NDSIZE,45,0)
        NDSIZE=NGO*NMO
        CALL MQDARE(IDAF50,IODA50,D(N004),NDSIZE,7,0)
        CALL MQMOSY(LUNOUT,LPOUT,0,NGO,NMO,
     *              D(N001),D(N002),D(N003),D(N004),D(N005))
C
        CALL RETFM(NEED1)
C**** CPU TIME *********************************************************
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (TRACE) THEN
           WRITE(LUNOUT,9100) STRSUB,TIME
           IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        END IF
C
        CALL TSECND(TIM1)
        TIME = TIM1 - TIM0
        TIM0 = TIM1
        IF(MASWRK) WRITE(LUNOUT,9400) TIME
        CALL FLSHBF(LUNOUT)
 9400 FORMAT(1X,'CPU TIME TO CANONICALIZE THE ORBITALS =',F10.2)
C
C       TRANSFORM INTEGRALS OVER THE CANONICAL ORBITALS
C
        IF (MASWRK) WRITE(LUNOUT,9410)
 9410 FORMAT(/1X,'TRANSFORMING INTEGRALS OVER ALL',
     *           ' CANONICAL MCSCF ORBITALS.'/)
        STRSUB='MQTRF '
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
C**** ADDRESSING OF WORK AREA ******************************************
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C.... LIJGO  ...........................................................
        N001=MQADDR(NGO*NGO,'I')
C.... LIJMO  ...........................................................
        N002=MQADDR(NMO*NMO,'I')
C.... LMOIJI ...........................................................
        N003=MQADDR(NMO*(NMO+1)/2,'I')
C.... LMOIJJ ...........................................................
        N004=MQADDR(NMO*(NMO+1)/2,'I')
C.... CFMO   ...........................................................
        N005=MQADDR(NGO*NGO,'D')
C
        LASTP1=MQMNXT()
        NEED1=LASTP1-N001
        CALL GETFM(NEED1)
C**** ERROR STOP *******************************************************
C       IDUM=MQMCHK()
C**** READ MOLECULAR ORBITAL *******************************************
C.... MODIFIED ON 9/9/01
        NDSIZE=NGO*NMO
C       NDSIZE=NGO*NMO
C       NDSIZE=NGO*NGO
        CALL MQDARE(IDAF50,IODA50,D(N005),NDSIZE,8,0)
C**** CALL MQTRF *******************************************************
        ISWTCH=-1
        NMOTRF=NMO
        CALL MQTRF(LUNOUT,LPOUT ,ISWTCH,LUNERI,LUNOER,LUNJMA,
     *             LUNJM1,LUNJM2,LUNJM3,NGO   ,NMOINT,TRACE,
     *             NMO   ,MAXERI,MXTRFR,THRERI,PARAIO,DELSCR,
     *             D(N001),D(N002),D(N003),D(N004),D(N005),NMOTRF,
     *             OERFLG,DOORD0)
C
        CALL RETFM(NEED1)
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (TRACE) THEN
           WRITE(LUNOUT,9100) STRSUB,TIME
           IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        END IF
C
        CALL TSECND(TIM1)
        TIME = TIM1 - TIM0
        TIM0 = TIM1
        IF(MASWRK) WRITE(LUNOUT,9370) TIME
        CALL FLSHBF(LUNOUT)
      END IF
C==== END IF FOR IFORB =================================================
C
C     SELECT CSF OR NOT
C
      IF(ISELCT.NE.0) THEN
        IF (MASWRK) WRITE(LUNOUT,9420)
        STRSUB='MQCACI'
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
        MAXDIA=MAX(MDI,MXBASE)
        INIMO=NDOUB+1
        LASMO=NDOUB+NMOACT
        NMOAA=NMOACT*(NMOACT+1)/2
        NMOEA=NMOAA
C**** ADDRESSING OF WORK AREA ******************************************
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C.... EIGVAL ...........................................................
        N001=MQADDR(NSTATE*3,'D')
C.... EIGVEC ...........................................................
        N002=MQADDR(NCSF*NSTATE,'D')
C.... LWCD   ...........................................................
        N003=MQADDR(2*MAXERI,'I')
C.... WORKER ...........................................................
        N004=MQADDR(MAXERI,'D')
C.... LIJMO  ...........................................................
        N005=MQADDR(NMOACT*NMOACT,'I')
C.... HCORE  ...........................................................
        N006=MQADDR(NMO*NMO,'D')
C.... VONEEL ...........................................................
        N007=MQADDR(NMOACT*NMOACT,'D')
C.... VTWOEL ...........................................................
        N008=MQADDR(NMOEA*NMOAA,'D')
C.... LABEL1 ...........................................................
        N009=MQADDR(4*MAXCSF,'I')
C.... WORKGE ...........................................................
        N010=MQADDR(MAXCSF,'D')
C.... DIAHMA ...........................................................
        N011=MQADDR(MAXDIA*(MAXDIA+1)/2,'D')
C.... DIAVAL ...........................................................
        N012=MQADDR(MAXDIA,'D')
C.... DIAVEC ...........................................................
        N013=MQADDR(MAXDIA*MAXDIA,'D')
C.... DIAWRK ...........................................................
        N014=MQADDR(MAXDIA*8,'D')
C.... HMAT   ...........................................................
        N015=MQADDR(MAXDIA*MAXDIA,'D')
C.... LORDER ...........................................................
        N016=MQADDR(MAXDIA,'I')
C.... HMATDI ...........................................................
        N017=MQADDR(NCSF,'D')
C.... LNWOLD ...........................................................
        N018=MQADDR(NCSF,'I')
C.... LODNEW ...........................................................
        N019=MQADDR(NCSF,'I')
C.... BASE   ...........................................................
        N020=MQADDR(NCSF*MXBASE,'D')
C.... HXBASE ...........................................................
        N021=MQADDR(NCSF*MXBASE,'D')
C.... WRKVEC ...........................................................
        N022=MQADDR(NCSF,'D')
C.... EBEFOR ...........................................................
        N023=MQADDR(NSOLUT,'D')
C.... DIFENE ...........................................................
        N024=MQADDR(NSOLUT,'D')
C.... DIFVEC ...........................................................
        N025=MQADDR(NSOLUT,'D')
C.... LKCONV ...........................................................
        N026=MQADDR(NSOLUT,'I')
C.... KREF   ...........................................................
        N027=MQADDR(NCSF,'L')
C.... EIGVOD ...........................................................
        N028=MQADDR(NCSF*NSTATE,'D')
C
        LASTP1=MQMNXT()
        NEED1=LASTP1-N001
        CALL GETFM(NEED1)
C --- ADDITIONAL WORK AREA ---
        CALL GOTFM(NGOTMX)
        CACIOP=0
        NREQ1=NWFT0*2/NWDVAR+1+NWFT0P*4/NWDVAR+1+NWFT0P
        IF (LPOUT.LT.0 .AND. MASWRK) THEN
           WRITE(LUNOUT,*)
     *          'NWFT0=',NWFT0,', NWFT0P=',NWFT0P
           WRITE(LUNOUT,*) 'NGOTMX=',NGOTMX,', NREQ1=',NREQ1
           CALL FLSHBF(LUNOUT)
        END IF
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
        IF (NREQ1.GT.0 .AND. NGOTMX.GT.NREQ1) THEN
           CACIOP=1
           N101=MQADDR(NWFT0*2,'I')
           N102=MQADDR(NWFT0P*4,'I')
           N103=MQADDR(NWFT0P,'D')
        ELSE
           CACIOP=0
           N101=MQADDR(2,'I')
           N102=MQADDR(4,'I')
           N103=MQADDR(1,'D')
        END IF
        LASTP1=MQMNXT()
        NEED2=LASTP1-N101
        CALL GETFM(NEED2)
C**** ERROR STOP *******************************************************
C       IDUM=MQMCHK()
C**** READ ONE-ELECTRON INTEGRALS ON MO BASE ***************************
C.... HCORE  ...........................................................
        NDSIZE=NMO*NMO
        CALL MQDARE(IDAF50,IODA50,D(N006),NDSIZE,16,0)
C.... KREF   ...........................................................
        NDSIZE=MQDSIZ(NCSF,'L')
        CALL MQDARE(IDAF50,IODA50,D(N027),NDSIZE,11,1)
C
C          SOLVE CI PROBLEM
C
        IF(EXETYP.NE.CHECK) THEN
           CALL MQCACI(LUNOUT,LPOUT ,0     ,LUNJM1,LUNFT0,
     *                 NMO   ,NDOUB ,NMOACT,INIMO ,LASMO ,
     *                 NMOEA ,NMOAA ,
     *                 NCSF  ,
     *                 MAXDIA,NSTCI ,NSTATE,MDI   ,MAXERI,MAXCSF,
     *                 MXBASE,NSOLUT,NSTOP ,
     *                 ENUCLE,THRCON,THRENE,
     *                 MAINCS,
     *                 D(N001),D(N002),D(N003),D(N004),
     *                 D(N005),D(N006),D(N007),D(N008),D(N009),D(N010),
     *                 D(N011),D(N012),D(N013),D(N014),D(N015),D(N016),
     *                 D(N017),D(N018),D(N019),
     *                 D(N020),D(N021),D(N022),
     *                 D(N023),D(N024),D(N025),D(N026),
     *                 D(N027),KSTATE ,D(N028),AVECOE,AVGEN2,
     *                 CACIOP ,D(N101),D(N102),D(N103))
        END IF
C
C           SAVE CAS-CI ENERGIES AND EIGENVECTORS
C
        NDSIZE=NSTATE
        CALL MQDAWR(IDAF50,IODA50,D(N001),NDSIZE,17,0)
        NDSIZE=NCSF*NSTATE
        CALL MQDAWR(IDAF50,IODA50,D(N002),NDSIZE,18,0)
C
C           PRINT CI ENERGIES AND EIGENVECTORS
C
        M001=MQADDR((NMOACT+1)*NOCF,'I')
        M002=MQADDR((MAXSOC+1)*MAXSPF,'I')
        M003=MQADDR(2*NCSF,'I')
        M004=MQADDR(NMOACT,'I')
        LASTP1=MQMNXT()
        NEEDM=LASTP1-M001
        CALL GETFM(NEEDM)
        IF(EXETYP.NE.CHECK) THEN
           NDSIZE=MQDSIZ((NMOACT+1)*NOCF,'I')
           CALL MQDARE(IDAF50,IODA50,D(M001),NDSIZE,1,1)
           NDSIZE=MQDSIZ((MAXSOC+1)*MAXSPF,'I')
           CALL MQDARE(IDAF50,IODA50,D(M002),NDSIZE,2,1)
           NDSIZE=MQDSIZ(2*NCSF,'I')
           CALL MQDARE(IDAF50,IODA50,D(M003),NDSIZE,4,1)
           CALL MQPRWF(LUNOUT,NCSF,NSTATE,NOCF,MAXSOC,MAXSPF,NMOACT,
     *                 D(N001),D(N002),D(M001),D(M002),D(M003),D(M004))
        END IF
        CALL RETFM(NEEDM)
        CALL RETFM(NEED2)
        CALL RETFM(NEED1)
C
C           PRINT CPU USED
C
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (TRACE) THEN
           WRITE(LUNOUT,9100) STRSUB,TIME
           IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        END IF
C
        CALL TSECND(TIM1)
        TIME = TIM1 - TIM0
        TIM0 = TIM1
        IF(MASWRK) WRITE(LUNOUT,9390) TIME
        CALL FLSHBF(LUNOUT)
C
C**** MQSLT2 ***********************************************************
C
        STRSUB='MQSLT2'
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
C**** ADDRESSING OF WORK AREA ******************************************
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C.... CASVEC ...........................................................
        N001=MQADDR(NCSF*NSTATE,'D')
C.... KREFOD ...........................................................
        N002=MQADDR(NCSF,'L')
C.... KREF   ...........................................................
        N003=MQADDR(NCSF,'L')
C
        LASTP1=MQMNXT()
        NEED1=LASTP1-N001
        CALL GETFM(NEED1)
C**** ERROR STOP *******************************************************
C       IDUM=MQMCHK()
C**** READ CASVEC, KREFOD **********************************************
        NDSIZE=NCSF*NSTATE
        CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,18,0)
        NDSIZE=MQDSIZ(NCSF,'L')
        CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,11,1)
C
        CALL MQSLT2(LUNOUT,LPOUT ,NCSF  ,NSTATE,THRWGT,
     *              D(N001),D(N002),D(N003))
C**** WRITE KREF *******************************************************
        NDSIZE=MQDSIZ(NCSF,'L')
        CALL MQDAWR(IDAF50,IODA50,D(N003),NDSIZE,11,1)
C
        CALL RETFM(NEED1)
C**** CPU TIME *********************************************************
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (TRACE) THEN
           WRITE(LUNOUT,9100) STRSUB,TIME
           IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        END IF
      END IF
C==== END IF FOR ISELCT ================================================
C
C     GENERATE FINAL CAS-CI REFERENCE STATES
C
      IF (MASWRK) WRITE(LUNOUT,9420)
 9420 FORMAT(/1X,'SOLVING FOR CAS-CI STATE FUNCTIONS',
     *           ' IN CANONICAL ORBITAL BASIS'/)
      STRSUB='MQCACI'
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      CALL TSECND(STIME)
      WALL1=WALL
      MAXDIA=MAX(MDI,MXBASE)
      INIMO=NDOUB+1
      LASMO=NDOUB+NMOACT
      NMOAA=NMOACT*(NMOACT+1)/2
      NMOEA=NMOAA
C**** ADDRESSING OF WORK AREA ******************************************
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... EIGVAL ...........................................................
      N001=MQADDR(NSTATE*3,'D')
C.... EIGVEC ...........................................................
      N002=MQADDR(NCSF*NSTATE,'D')
C.... LWCD   ...........................................................
      N003=MQADDR(2*MAXERI,'I')
C.... WORKER ...........................................................
      N004=MQADDR(MAXERI,'D')
C.... LIJMO  ...........................................................
      N005=MQADDR(NMOACT*NMOACT,'I')
C.... HCORE  ...........................................................
      N006=MQADDR(NMO*NMO,'D')
C.... VONEEL ...........................................................
      N007=MQADDR(NMOACT*NMOACT,'D')
C.... VTWOEL ...........................................................
      N008=MQADDR(NMOEA*NMOAA,'D')
C.... LABEL1 ...........................................................
      N009=MQADDR(4*MAXCSF,'I')
C.... WORKGE ...........................................................
      N010=MQADDR(MAXCSF,'D')
C.... DIAHMA ...........................................................
      N011=MQADDR(MAXDIA*(MAXDIA+1)/2,'D')
C.... DIAVAL ...........................................................
      N012=MQADDR(MAXDIA,'D')
C.... DIAVEC ...........................................................
      N013=MQADDR(MAXDIA*MAXDIA,'D')
C.... DIAWRK ...........................................................
      N014=MQADDR(MAXDIA*8,'D')
C.... HMAT   ...........................................................
      N015=MQADDR(MAXDIA*MAXDIA,'D')
C.... LORDER ...........................................................
      N016=MQADDR(MAXDIA,'I')
C.... HMATDI ...........................................................
      N017=MQADDR(NCSF,'D')
C.... LNWOLD ...........................................................
      N018=MQADDR(NCSF,'I')
C.... LODNEW ...........................................................
      N019=MQADDR(NCSF,'I')
C.... BASE   ...........................................................
      N020=MQADDR(NCSF*MXBASE,'D')
C.... HXBASE ...........................................................
      N021=MQADDR(NCSF*MXBASE,'D')
C.... WRKVEC ...........................................................
      N022=MQADDR(NCSF,'D')
C.... EBEFOR ...........................................................
      N023=MQADDR(NSOLUT,'D')
C.... DIFENE ...........................................................
      N024=MQADDR(NSOLUT,'D')
C.... DIFVEC ...........................................................
      N025=MQADDR(NSOLUT,'D')
C.... LKCONV ...........................................................
      N026=MQADDR(NSOLUT,'I')
C.... KREF   ...........................................................
      N027=MQADDR(NCSF,'L')
C.... EIGVOD ...........................................................
      N028=MQADDR(NCSF*NSTATE,'D')
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-N001
      CALL GETFM(NEED1)
C --- ADDITIONAL WORK AREA ---
        CALL GOTFM(NGOTMX)
        CACIOP=0
        NREQ1=NWFT0*2/NWDVAR+1+NWFT0P*4/NWDVAR+1+NWFT0P
        IF (LPOUT.LT.0 .AND. MASWRK) THEN
           WRITE(LUNOUT,*)
     *          'NWFT0=',NWFT0,', NWFT0P=',NWFT0P
           WRITE(LUNOUT,*) 'NGOTMX=',NGOTMX,', NREQ1=',NREQ1
           CALL FLSHBF(LUNOUT)
        END IF
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
        IF (NREQ1.GT.0 .AND. NGOTMX.GT.NREQ1) THEN
           CACIOP=1
           N101=MQADDR(NWFT0*2,'I')
           N102=MQADDR(NWFT0P*4,'I')
           N103=MQADDR(NWFT0P,'D')
        ELSE
           CACIOP=0
           N101=MQADDR(2,'I')
           N102=MQADDR(4,'I')
           N103=MQADDR(1,'D')
        END IF
        LASTP1=MQMNXT()
        NEED2=LASTP1-N101
        CALL GETFM(NEED2)
C**** ERROR STOP *******************************************************
C     IDUM=MQMCHK()
C**** READ ONE-ELECTRON INTEGRALS ON MO BASE ***************************
C.... HCORE  ...........................................................
      NDSIZE=NMO*NMO
      CALL MQDARE(IDAF50,IODA50,D(N006),NDSIZE,16,0)
C.... KREF   ...........................................................
      NDSIZE=MQDSIZ(NCSF,'L')
      CALL MQDARE(IDAF50,IODA50,D(N027),NDSIZE,11,1)
C
C     MAXIMUM OVERLAP SORT
C
      IF(ISELCT.NE.0) THEN
        MAXOVL=1
        NDSIZE=NCSF*NSTATE
        CALL MQDARE(IDAF50,IODA50,D(N028),NDSIZE,18,0)
      ELSE
        MAXOVL=0
      END IF
C
C        SOLVE CI PROBLEM FOR 0-TH ORDER STATES
C
      IF(EXETYP.NE.CHECK) THEN
         CALL MQCACI(LUNOUT,LPOUT ,MAXOVL,LUNJM1,LUNFT0,
     *               NMO   ,NDOUB ,NMOACT,INIMO ,LASMO ,
     *               NMOEA ,NMOAA ,
     *               NCSF  ,
     *               MAXDIA,NSTCI ,NSTATE,MDI   ,MAXERI,MAXCSF,
     *               MXBASE,NSOLUT,NSTOP ,
     *               ENUCLE,THRCON,THRENE,
     *               MAINCS,
     *               D(N001),D(N002),D(N003),D(N004),
     *               D(N005),D(N006),D(N007),D(N008),D(N009),D(N010),
     *               D(N011),D(N012),D(N013),D(N014),D(N015),D(N016),
     *               D(N017),D(N018),D(N019),
     *               D(N020),D(N021),D(N022),
     *               D(N023),D(N024),D(N025),D(N026),
     *               D(N027),KSTATE ,D(N028),AVECOE,AVGEN3,
     *               CACIOP ,D(N101),D(N102),D(N103))
      END IF
C
C        SAVE CAS-CI ENERGIES AND EIGENVECTORS
C
      NDSIZE=NSTATE
      CALL MQDAWR(IDAF50,IODA50,D(N001),NDSIZE,17,0)
      NDSIZE=NCSF*NSTATE
      CALL MQDAWR(IDAF50,IODA50,D(N002),NDSIZE,18,0)
C
C        PRINT CI ENERGIES AND EIGENVECTORS
C
      M001=MQADDR((NMOACT+1)*NOCF,'I')
      M002=MQADDR((MAXSOC+1)*MAXSPF,'I')
      M003=MQADDR(2*NCSF,'I')
      M004=MQADDR(NMOACT,'I')
      LASTP1=MQMNXT()
      NEEDM=LASTP1-M001
      CALL GETFM(NEEDM)
      IF(EXETYP.NE.CHECK) THEN
         NDSIZE=MQDSIZ((NMOACT+1)*NOCF,'I')
         CALL MQDARE(IDAF50,IODA50,D(M001),NDSIZE,1,1)
         NDSIZE=MQDSIZ((MAXSOC+1)*MAXSPF,'I')
         CALL MQDARE(IDAF50,IODA50,D(M002),NDSIZE,2,1)
         NDSIZE=MQDSIZ(2*NCSF,'I')
         CALL MQDARE(IDAF50,IODA50,D(M003),NDSIZE,4,1)
         CALL MQPRWF(LUNOUT,NCSF,NSTATE,NOCF,MAXSOC,MAXSPF,NMOACT,
     *               D(N001),D(N002),D(M001),D(M002),D(M003),D(M004))
      END IF
      CALL RETFM(NEEDM)
      CALL RETFM(NEED2)
      CALL RETFM(NEED1)
C
C        PRINT CPU USED
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF (TRACE) THEN
         WRITE(LUNOUT,9100) STRSUB,TIME
         IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
      END IF
C
      CALL TSECND(TIM1)
      TIME = TIM1 - TIM0
      TIM0 = TIM1
      IF(MASWRK) WRITE(LUNOUT,9390) TIME
      CALL FLSHBF(LUNOUT)
C
C         ---- CHECK TO BE SURE WE GOT THE SAME ENERGIES ----
C              AS WHEN CANONICALIZING.  IF NOT, THERE IS
C              AN ERROR IN ONE OF THE DAVIDSON DIAGONALIZATIONS.
C
C         THE FOLLOWING TEST SHOULD NOT BE APPLIED TO SPIN-ORBIT,
C         WHERE WE WILL JUST ASSUME THINGS ARE OK.  THE WEIGHTS
C         ARE TREATED IN A COMPLEX MANNER IN THIS KIND OF RUN,
C         SO WE'LL JUST SKIP THE TEST.  THE ISELCT OPTION DELETES
C         SMALL CSF'S FROM THE CALCULATIONS, SO THE ENERGIES CANNOT
C         BE REQUIRED TO MATCH IN THAT CASE EITHER.
C
      IF(ISELCT.EQ.0  .AND.  IFORB.EQ.1  .AND.  RUNTYP.NE.TRANST) THEN
         EDIFF = ABS(AVGEN1 - AVGEN3)
         IF(EDIFF.GT.1.0D-06) THEN
            IF(MASWRK) WRITE(LUNOUT,9430) AVGEN1,AVGEN3
            CALL ABRT
         END IF
      END IF
 9430 FORMAT(//1X,'***** ERROR IN STATE ENERGIES DETECTED *****'/
     *     1X,'STATE AVERAGED ENERGY DURING ORBITAL CANONICALIZATION=',
     *        F20.10/
     *     1X,'STATE AVERAGED ENERGY USING CANONICAL ORBITALS       =',
     *        F20.10/
     *     1X,'JOB STOPS SINCE THESE DO NOT MATCH!  PLEASE COMPARE TO'/
     *     1X,'YOUR CALCULATED MCSCF ENERGY TO DETERMINE WHICH OF THE'/
     *     1X,'DIAGONALIZATIONS HERE IS INCORRECT.  YOU MAY BE ABLE TO'/
     *     1X,'ADJUST DAVIDSON DIAGONALIZATION $MCQDPT INPUT TO FIX'/
     *     1X,'THE PROBLEM OF FINDING DIFFERENT ROOTS:'/
     *     1X,'   NSOLUT = NO. OF STATES TO BE SOLVED FOR, YOU MIGHT'/
     *     1X,'            MAKE THIS BIGGER THAN THE NUMBER OF STATES'/
     *     1X,'            IN THE PERTURBATION RUN'/
     *     1X,'   MXBASE = MAXIMUM SIZE OF EXPANSION SPACE IN DAV. CI'/
     *     1X,'   MDI    = DIMENSION OF SMALL HAMILTONIAN IN DAV. CI'/
     *     1X,'            INITIAL GUESS PROCEDURE'/
     *     1X,'A GOOD THING TO TRY IS TO LET THE MCSCF PROGRAM DO THE'/
     *     1X,'CANONCALIZATION ITSELF, BY NOT USING CANONC=.FALSE.,'/
     *     1X,'AND BY PROVIDING THE MCSCF MO-S RATHER THAN MCSCF NO-S')
C
C***********************************************************************
C**** MQORB2 ***********************************************************
C****        ***********************************************************
      STRSUB='MQORB2'
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      CALL TSECND(STIME)
      WALL1=WALL
      NMOINT=NDOUB+NMOACT
C     NMOEI=NMOINT*(NMOINT+1)/2+(NMO-NMOINT)*NMOINT
C**** ADDRESSING OF WORK AREA ******************************************
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... EIGVEC ...........................................................
      N001=MQADDR(NCSF*NSTATE,'D')
C.... AVECOE ...........................................................
C     N002
C.... CMO    ...........................................................
      N003=MQADDR(NGO*NGO,'D')
C.... EORB   ...........................................................
      N004=MQADDR(NMO,'D')
C.... HCORE  ...........................................................
      N005=MQADDR(NMO*NMO,'D')
C.... GEN1AO ...........................................................
      N006=MQADDR(NGO*NGO,'D')
C.... GEN1   ...........................................................
      N007=MQADDR(NMOACT*NMOACT,'D')
C.... GFCKAO ...........................................................
      N008=MQADDR(NGO*NGO,'D')
C.... XWORK  ...........................................................
      N009=MQADDR(NINTMX,'D')
C.... IXWORK ...........................................................
C   ALLOCATE THIS INTEGER BUFFER AS REAL IN CASE OF 2 BYTE LABELS
      N010=MQADDR(NINTMX,'D')
C.... LABEL1 ...........................................................
      N011=MQADDR(4*MAXCSF,'I')
C.... WORKGE ...........................................................
      N012=MQADDR(MAXCSF,'D')
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-N001
      CALL GETFM(NEED1)
C**** ERROR STOP *******************************************************
C     IDUM=MQMCHK()
C**** READ MOLECULAR ORBITAL *******************************************
      IF(IFORB.NE.0) THEN
        NDSIZE=NGO*NMO
        CALL MQDARE(IDAF50,IODA50,D(N003),NDSIZE,8,0)
      ELSE
C     ELSE IF(INORB.NE.0) THEN
        NDSIZE=NGO*NMO
        CALL MQDARE(IDAF50,IODA50,D(N003),NDSIZE,7,0)
C     ELSE
C       NDSIZE=NGO*NGO
C       CALL DAREAD(IDAF,IODA,D(N003),NDSIZE,15,0)
      END IF
C**** READ CAS-CI EIGENVECTORS *****************************************
      NDSIZE=NCSF*NSTATE
      CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,18,0)
C**** READ ONE-ELECTRON INTEGRALS ON MO BASE ***************************
      NDSIZE=NMO*NMO
      CALL MQDARE(IDAF50,IODA50,D(N005),NDSIZE,16,0)
C
      CANSPE=RUNTYP.EQ.TRANST.AND.IFORB.EQ.0
      IF(EXETYP.NE.CHECK) THEN
         CALL MQORB2(LUNOUT,LPOUT ,LUNERI,LUNFT0,NCSF  ,NSTATE,
     *               NMO   ,NDOUB ,NMOACT,NGO   ,MAXCSF,
     *               D(N001),AVECOE ,D(N003),D(N004),D(N005),D(N006),
     *               D(N007),D(N008),D(N009),D(N010),D(N011),D(N012),
     *               CANAVE,CANSPE)
C        WRITE(6,*) 'FINAL ORBITALS USED IN PT'
C        CALL PRSQ(D(N003),NMO,NGO,NGO)
      END IF
C**** WRITE ORBITAL ENERGIES *******************************************
      NDSIZE=NMO
      CALL MQDAWR(IDAF50,IODA50,D(N004),NDSIZE,9,0)
C
      CALL RETFM(NEED1)
      IF(CANSPE.AND.NOCC.EQ.0) GOTO 1000
C**** CPU TIME *********************************************************
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF (TRACE) THEN
         WRITE(LUNOUT,9100) STRSUB,TIME
         IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
      END IF
C
      CALL TSECND(TIM1)
      TIME = TIM1 - TIM0
      TIM0 = TIM1
      IF(MASWRK) WRITE(LUNOUT,9440) TIME
      CALL FLSHBF(LUNOUT)
 9440 FORMAT(1X,'CPU TIME TO GENERATE ORBITAL ENERGIES =',F10.2)
C
C***********************************************************************
C****                                       ****************************
C**** DISCARD UNNECESSARY 1-BODY GENERATORS ****************************
C****                                       ****************************
C***********************************************************************
C**** MQOCF1 ***********************************************************
C
      IF (MASWRK) WRITE(LUNOUT,9450)
 9450 FORMAT(/1X,'DISCARDING NEGLIGIBLE GENERATORS')
      STRSUB='MQOCF1'
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      CALL TSECND(STIME)
      WALL1=WALL
      INIACT=NDOUB+1
      LASACT=NDOUB+NMOACT
C**** ADDRESSING OF WORK AREA ******************************************
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... NSNSF  ...........................................................
      N001=MQADDR(NOCF+1,'I')
C.... I1EX1  ...........................................................
      N002=MQADDR(NMOACT*(NOCF+1),'I')
C.... I1EX2  ...........................................................
      N003=MQADDR(NMOACT*(NOCFI+1),'I')
C.... LAB1   ...........................................................
      N004=MQADDR(3*LENGTH,'I')
C.... WORK   ...........................................................
      N005=MQADDR(LENGTH,'D')
C.... KREF   ...........................................................
      N006=MQADDR(NCSF,'L')
C.... KOCF1E (AND KOCF) ................................................
      N007=MQADDR(NOCF,'L')
C.... KREFWK ...........................................................
      N008=MQADDR(NCSF,'L')
C.... LOD2NW ...........................................................
      N009=MQADDR(NCSF,'I')
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-N001
      CALL GETFM(NEED1)
C**** ERROR STOP *******************************************************
C     IDUM=MQMCHK()
C**** READ NSNSF, I1EX1, I1EX2, KREF ***********************************
      NDSIZE=MQDSIZ(NOCF+1,'I')
      CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,3,1)
      NDSIZE=MQDSIZ(NMOACT*(NOCF+1),'I')
      CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,5,1)
      NDSIZE=MQDSIZ(NMOACT*(NOCFI+1),'I')
      CALL MQDARE(IDAF50,IODA50,D(N003),NDSIZE,6,1)
      NDSIZE=MQDSIZ(NCSF,'L')
      CALL MQDARE(IDAF50,IODA50,D(N006),NDSIZE,11,1)
C**** CALL MQOCF1 ******************************************************
      IF(EXETYP.NE.CHECK) THEN
         CALL MQOCF1(LUNOUT,LPOUT ,LUNFTA,LUNFTB,INIACT,LASACT,
     *               NOCF  ,NCSF  ,LENGTH,NOCFI ,
     *               D(N001),D(N002),D(N003),D(N004),D(N005),D(N006),
     *               D(N007))
      END IF
      IF (DELSCR) THEN
         CALL SEQCLO(LUNFTA,'DELETE')
      ELSE
         CALL SEQCLO(LUNFTA,'KEEP')
      END IF
      LUNFTA=LUNFTB
C**** WRITE KOCF1E *****************************************************
      NDSIZE=MQDSIZ(NOCF,'L')
      CALL MQDAWR(IDAF50,IODA50,D(N007),NDSIZE,12,1)
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF (TRACE) THEN
         WRITE(LUNOUT,9100) STRSUB,TIME
         IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
      END IF
      CALL FLSHBF(LUNOUT)
C***********************************************************************
C**** MQOCF2 ***********************************************************
C****        ***********************************************************
      STRSUB='MQOCF2'
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      CALL TSECND(STIME)
      WALL1=WALL
C**** CALL MQOCF2 ******************************************************
      IF(EXETYP.NE.CHECK) THEN
         CALL MQOCF2(LUNOUT,LPOUT ,LUNFTA,LUNFTC,INIACT,LASACT,
     *               NOCF  ,NCSF  ,LENGTH,
     *               D(N001),D(N004),D(N005),D(N006),D(N007))
      END IF
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF (TRACE) THEN
         WRITE(LUNOUT,9100) STRSUB,TIME
         IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
      END IF
      CALL FLSHBF(LUNOUT)
C***********************************************************************
C**** MQCOND ***********************************************************
C****        ***********************************************************
      STRSUB='MQCOND'
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      CALL TSECND(STIME)
      WALL1=WALL
C
      IF(EXETYP.EQ.CHECK) THEN
         NCSFNW = NCSF
         NDSIZE=MQDSIZ(NCSF,'I')
         CALL VCLR(D(N009),1,NDSIZE)
      ELSE
         CALL MQCOND(LUNOUT,LPOUT ,INIACT,LASACT,NOCF  ,NCSF  ,
     *               NOCFI ,NCSFNW,ISELCT,
     *               D(N001),D(N002),D(N003),D(N006),D(N008),D(N009))
      END IF
C**** WRITE LOD2NW *****************************************************
      NDSIZE=MQDSIZ(NCSF,'I')
      CALL MQDAWR(IDAF50,IODA50,D(N009),NDSIZE,13,1)
C
      CALL RETFM(NEED1)
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF (TRACE) THEN
         WRITE(LUNOUT,9100) STRSUB,TIME
         IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
      END IF
C
      CALL TSECND(TIM1)
      TIME = TIM1 - TIM0
      TIM0 = TIM1
      IF(MASWRK) WRITE(LUNOUT,9460) TIME
      CALL FLSHBF(LUNOUT)
 9460 FORMAT(1X,'CPU TIME TO REDUCE GENERATORS TO MINIMUM =',F10.2)
C
C     MQLPR1 AND MQLPR2
C
      IF(MASWRK) WRITE(LUNOUT,9470)
 9470 FORMAT(/1X,'FINISHING SETUP FOR MRMP/MCQDPT CALCULATION.'/)
      STRSUB='MQLPR1'
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      CALL TSECND(STIME)
      WALL1=WALL
      INIACT=NDOUB+1
      LASACT=NDOUB+NMOACT
      NMO=NMOFZC+NMODOC+NMOACT+NMOEXT
      NMOINT=NDOUB+NMOACT
      NMODA=NMODOC+NMOACT
      NMOEI=NMODA*(NMODA+1)/2+NMOEXT*NMODA
      NMOII=NMODA*(NMODA+1)/2
C**** ADDRESSING OF WORK AREA ******************************************
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... CASVEC ...........................................................
      N001=MQADDR(NCSF*NSTATE,'D')
C.... EORB   ...........................................................
      N002=MQADDR(NMO,'D')
C.... LIJMO  ...........................................................
      N003=MQADDR(NMO*NMO,'I')
C.... VONEEL ...........................................................
      N004=MQADDR(NMO*NMO,'D')
C.... VTWOEL ...........................................................
      N005=MQADDR(NMOEI*NMOII,'D')
C.... ECONF  ...........................................................
      N006=MQADDR(NCSF,'D')
C.... EREF0  ...........................................................
      N007=MQADDR(NSTATE,'D')
C.... HCORE  ...........................................................
      N008=MQADDR(NMO*NMO,'D')
C.... IOMAP  ...........................................................
      N009=MQADDR((NMOACT+1)*NOCF,'I')
C.... IOCSF  ...........................................................
      N010=MQADDR(2*NCSF,'I')
C.... LWCD   ...........................................................
      N011=MQADDR(2*MAXERI,'I')
C.... WORK   ...........................................................
      N012=MQADDR(MAXERI,'D')
C.... HCORE2 ...........................................................
      N013=MQADDR(NMO*NMO,'D')
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-N001
      CALL GETFM(NEED1)
C**** ERROR STOP *******************************************************
C     IDUM=MQMCHK()
C**** READ CASVEC, EORB, HCORE, IOMAP, IOCSF ***************************
      NDSIZE=NCSF*NSTATE
      CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,18,0)
      NDSIZE=NMO
      CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,9,0)
      NDSIZE=NMO*NMO
      CALL MQDARE(IDAF50,IODA50,D(N008),NDSIZE,16,0)
      NDSIZE=MQDSIZ((NMOACT+1)*NOCF,'I')
      CALL MQDARE(IDAF50,IODA50,D(N009),NDSIZE,1,1)
      NDSIZE=MQDSIZ(2*NCSF,'I')
      CALL MQDARE(IDAF50,IODA50,D(N010),NDSIZE,4,1)
      IF(EXETYP.NE.CHECK) THEN
         CALL MQLPR1(LUNOUT,LPOUT ,LUNJM1,LUNJM2,LUNJM3,LUNTWO,
     *               NMOFZC,NMODOC,NMOACT,NMOEXT,NMO   ,NMOEI ,
     *               NMOII ,
     *               INIACT,LASACT,
     *               NSTATE,NCSF  ,NOCF  ,
     *               MAXERI,
     *               D(N001),D(N002),D(N003),D(N004),D(N005),
     *               D(N006),D(N007),
     *               D(N008),D(N009),D(N010),D(N011),D(N012),D(N013))
      END IF
C**** WRITE LIJMO, VONEEL, ECONF, EREF0 ********************************
      NDSIZE=MQDSIZ(NMO*NMO,'I')
      CALL MQDAWR(IDAF50,IODA50,D(N003),NDSIZE,19,1)
      NDSIZE=NMO*NMO
      CALL MQDAWR(IDAF50,IODA50,D(N004),NDSIZE,20,0)
      NDSIZE=NCSF
      CALL MQDAWR(IDAF50,IODA50,D(N006),NDSIZE,21,0)
      NDSIZE=NSTATE
      CALL MQDAWR(IDAF50,IODA50,D(N007),NDSIZE,22,0)
C
      CALL RETFM(NEED1)
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF (TRACE) THEN
         WRITE(LUNOUT,9100) STRSUB,TIME
         IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
      END IF
      CALL FLSHBF(LUNOUT)
C
C**** MQLPR2 ***********************************************************
C
      STRSUB='MQLPR2'
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      CALL TSECND(STIME)
      WALL1=WALL
      INIDOC=NMOFZC+1
      LASDOC=NMOFZC+NMODOC
      INIACT=NMOFZC+NMODOC+1
      LASACT=NMOFZC+NMODOC+NMOACT
      INIEXT=NMOFZC+NMODOC+NMOACT+1
      LASEXT=NMOFZC+NMODOC+NMOACT+NMOEXT
C**** ADDRESSING OF WORK AREA ******************************************
      CALL VALFM(LOADFM)
      CALL GOTFM(NGOTMX)
      IDUM=MQINIT(LOADFM+1)
C.... LWCD   ...........................................................
      N001=MQADDR(2*MAXERI,'I')
C.... WORK   ...........................................................
      N002=MQADDR(MAXERI,'D')
C.... VTWOEL ...........................................................
      LASTP1=MQMNXT()
      NR=N001+NGOTMX-LASTP1
C     NR=MQMLFT()
C
C     --- DISTRIBUTED MEMORY ALLOCATION ---
C BOTH THE MAXIMUM MEMORY THAT CAN BE USED, AND THE POSSIBLY SMALLER
C AMOUNT TO BE USED BASED ON THE USER'S INPUT ALLOCATE AN ADDITIONAL
C MEGAWORD PER PROCESSOR FOR MESSAGE BUFFERS.
C
      NDSIZE=(NMODOC+NMOACT)**2*NMOEXT
      MXWTH = NPROC*   ((NMOEXT-1)/NPROC+1)
      MNWTH = NPROC*INT((MEMDDI*1.0D+06/(NDSIZE*NPROC))-1)
      IF(MNWTH.GT.MXWTH) MNWTH=MXWTH
      IDMWTH= MIN(MXWTH,MNWTH)
      IDMWTH= MAX(IDMWTH,0)
      MNWTH = IDMWTH
      MXDDI = INT((MXWTH/1.0D+06)*NDSIZE+1)
      MNDDI = INT((MNWTH/1.0D+06)*NDSIZE+1)
      IF(IDMWTH.EQ.0) MNDDI=0
      IF (MASWRK) THEN
         WRITE(LUNOUT,9480) MXDDI,MNDDI
         IF(MNWTH.LT.MXWTH) WRITE(LUNOUT,9485)
      END IF
 9480 FORMAT(1X,'MAXIMUM MEMDDI THAT CAN BE USED IN THE',
     *          ' PERTURBATION CALCULATION IS',I6,' MWORDS'/
     *       1X,'   THE MEMDDI THAT WILL BE USED IN THE',
     *          ' PERTURBATION CALCULATION IS',I6,' MWORDS')
 9485 FORMAT(1X,'SMALLER AMOUNTS OF DDI MEMORY RUN WITH INCREASED I/O')
C
      IF (IDMWTH.GT.0) THEN
         NREAD=(NMOEXT-1)/IDMWTH+1
         IF (NREAD.GT.80) THEN
            IDMWTH=0
         ELSE
            IF (EXETYP.NE.CHECK )
     *           CALL DDI_CREATE((NMODOC+NMOACT)*NMOEXT,
     *                           (NMODOC+NMOACT)*IDMWTH,DTWOEL)
            CALL DDI_SYNC(2530)
         END IF
      END IF
C
      IF (IDMWTH.EQ.0) THEN
         IWIDTH=MIN(NR/NDSIZE,NMOEXT)
         N003=MQADDR(NDSIZE*IWIDTH,'D')
         NREAD=(NMOEXT-1)/IWIDTH+1
         IF(NREAD.GT.80) THEN
            IF(MASWRK) WRITE(LUNOUT,
     *           '('' *** ERROR STOP IN SUB.MQDRV ***''/
     *           '' NREAD FOR SUB.MQLPR2 IS GREATER THAN 80.''/
     *           '' NREAD ='',I10)') NREAD
            CALL ABRT
         END IF
      ELSE
         IWIDTH=1
         N003=MQADDR(NDSIZE*2,'D')
      END IF
C
      IF (TRACE)
     *  WRITE(LUNOUT,'('' NO. DISK READS FOR SUB.MQLPR2 ='',I10)') NREAD
      IF(MASWRK) WRITE(LUNOUT,9490)
 9490 FORMAT(1X,'PREPARING TRANSFORMED INTEGRALS FOR PT...')
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-N001
      CALL GETFM(NEED1)
C
      IF(EXETYP.NE.CHECK) THEN
         CALL MQLPR2(LUNOUT,LPOUT ,LUNJM3,LUNTWO,
     *               INIDOC,LASDOC,INIACT,LASACT,INIEXT,LASEXT,
     *               MAXERI,IWIDTH,IDMWTH,DTWOEL,REFWGT,
     *               D(N001),D(N002),D(N003))
      END IF
C
      CALL RETFM(NEED1)
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF (TRACE) THEN
         WRITE(LUNOUT,9100) STRSUB,TIME
         IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
      END IF
C
      CALL TSECND(TIM1)
      TIME = TIM1 - TIM0
      TIM0 = TIM1
      IF(MASWRK) WRITE(LUNOUT,9500) TIME
      CALL FLSHBF(LUNOUT)
 9500 FORMAT(1X,'TIME FOR FINISHING SETUPS =',F10.2)
C
C     SAVE SOME INFO FOR SO-MCQDPT
C
      MAXSPF1=MAXSPF
      MAXSOC1=MAXSOC
      NCSF1=NCSF
      NOCF1=NOCF
C
      IF (MASWRK) WRITE(LUNOUT,9510)
 9510 FORMAT(/1X,'EVALUATING PERTURBATION THEORY CORRECTIONS',
     *           ' TO HAMILTONIAN ELEMENTS.'/)
      IF(REFWGT) GO TO 106
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%             NOT REFERENCE WEIGHT CALCULATION                  %%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... HEFF   ...........................................................
      M000=MQADDR(NSTATE*NSTATE*4,'D')
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-M000
      CALL GETFM(NEED1)
C
C     LOOP OVER STATES -IALPHA-, EVALUATING PERTURBATION CORRECTIONS
C     FOR ALL STATES -IBETA- IN THIS -IALPHA- ROW OF THE EFFECTIVE HAM.
C
      DO IALPHA=1,NSTATE
        ISTATE=IALPHA
C***********************************************************************
C**** MQLMB1 ***********************************************************
C****        ***********************************************************
        IF(NSTATE.GT.1 .AND. MASWRK) WRITE(LUNOUT,9520) IALPHA
 9520 FORMAT(/1X,'EVALUATING MATRIX ELEMENTS BETWEEN STATE',I4,
     *           ' AND ALL OTHER STATES.'/)
        STRSUB='MQLMB1'
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
        INIACT=NDOUB+1
        LASACT=NDOUB+NMOACT
        NMO=NMOFZC+NMODOC+NMOACT+NMOEXT
        NMODA=NMODOC+NMOACT
        NMOEI=NMODA*(NMODA+1)/2+NMOEXT*NMODA
        NMOII=NMODA*(NMODA+1)/2
C**** ADDRESSING OF WORK AREA ******************************************
C.... PROGRAM IS KEEPING NEED1 (DW) NOW.
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C.... CASVEC ...........................................................
        M001=MQADDR(NCSF*NSTATE,'D')
C.... EORB   ...........................................................
        M002=MQADDR(NMO,'D')
C.... LIJMO  ...........................................................
        M003=MQADDR(NMO*NMO,'I')
C.... VONEEL ...........................................................
        M004=MQADDR(NMO*NMO,'D')
C.... VTWOEL ...........................................................
        M005=MQADDR(NMOEI*NMOII,'D')
C.... ECONF  ...........................................................
        M006=MQADDR(NCSF,'D')
C.... EREF0  ...........................................................
        M007=MQADDR(NSTATE,'D')
C.... CASENE ...........................................................
        M008=MQADDR(NSTATE,'D')
C.... KREF   ...........................................................
        M009=MQADDR(NCSF,'L')
C.... MOSYM  ...........................................................
        M010=MQADDR(NMO,'I')
C.... LOD2NW ...........................................................
        M011=MQADDR(NCSF,'I')
C
        LASTP1=MQMNXT()
        NEED2=LASTP1-M001
        CALL GETFM(NEED2)
C
C.... NSNSF  ...........................................................
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C
        N001=MQADDR(NOCF+1,'I')
C.... I1EX1  ...........................................................
        N002=MQADDR(NMOACT*(NOCF+1),'I')
C.... I1EX2  ...........................................................
        N003=MQADDR(NMOACT*(NOCFI+1),'I')
C.... GEN1WK ...........................................................
        N004=MQADDR(NMOACT*MAXSPF*MAXSPF,'D')
C.... GEN1   ...........................................................
        N005=MQADDR(NMOACT*(NCSFNW+1),'D')
C.... LAB1R  ...........................................................
        N006=MQADDR(3*LENGTH,'I')
C.... WORKR  ...........................................................
        N007=MQADDR(LENGTH,'D')
C.... LAB1W  ...........................................................
        N008=MQADDR(2*LENGTH,'I')
C.... WORKW  ...........................................................
        N009=MQADDR(LENGTH,'D')
C.... VAL2   ...........................................................
        N010=MQADDR(NSTATE,'D')
C.... HEFF2  ...........................................................
        N011=MQADDR(NSTATE,'D')
C
        LASTP1=MQMNXT()
        NEED3=LASTP1-N001
        CALL GETFM(NEED3)
C**** ERROR STOP *******************************************************
C       IDUM=MQMCHK()
C**** READ CASVEC, EORB, LIJMO, VONEEL, ECONF, EREF0 *******************
C****      EIGVEC, NSNSF, I1EX1, I1EX2, CASENE, KREF *******************
C.... CASVEC ...........................................................
        NDSIZE=NCSF*NSTATE
        CALL MQDARE(IDAF50,IODA50,D(M001),NDSIZE,18,0)
C.... EORB   ...........................................................
        NDSIZE=NMO
        CALL MQDARE(IDAF50,IODA50,D(M002),NDSIZE,9,0)
C.... LIJMO  ...........................................................
        NDSIZE=MQDSIZ(NMO*NMO,'I')
        CALL MQDARE(IDAF50,IODA50,D(M003),NDSIZE,19,1)
C.... VONEEL ...........................................................
        NDSIZE=NMO*NMO
        CALL MQDARE(IDAF50,IODA50,D(M004),NDSIZE,20,0)
C.... ECONF  ...........................................................
        NDSIZE=NCSF
        CALL MQDARE(IDAF50,IODA50,D(M006),NDSIZE,21,0)
C.... EREF0  ...........................................................
        NDSIZE=NSTATE
        CALL MQDARE(IDAF50,IODA50,D(M007),NDSIZE,22,0)
C.... CASENE ...........................................................
        NDSIZE=NSTATE
        CALL MQDARE(IDAF50,IODA50,D(M008),NDSIZE,17,0)
C.... KREF   ...........................................................
        NDSIZE=MQDSIZ(NCSF,'L')
        CALL MQDARE(IDAF50,IODA50,D(M009),NDSIZE,11,1)
C.... MOSYM  ...........................................................
        NDSIZE=MQDSIZ(NMO,'I')
        CALL MQDARE(IDAF50,IODA50,D(M010),NDSIZE,10,1)
C.... LOD2NW ...........................................................
        NDSIZE=MQDSIZ(NCSF,'I')
        CALL MQDARE(IDAF50,IODA50,D(M011),NDSIZE,13,1)
C.... NSNSF  ...........................................................
        NDSIZE=MQDSIZ(NOCF+1,'I')
        CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,3,1)
C.... I1EX1  ...........................................................
        NDSIZE=MQDSIZ(NMOACT*(NOCF+1),'I')
        CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,5,1)
C.... I1EX2  ...........................................................
        NDSIZE=MQDSIZ(NMOACT*(NOCFI+1),'I')
        CALL MQDARE(IDAF50,IODA50,D(N003),NDSIZE,6,1)
        IF(EXETYP.NE.CHECK) THEN
           IALPX = IALPHA
           CALL MQLMB1(LUNOUT,LPOUT ,LUNFTA,LUNFT1,
     *                 IALPX,INIACT,LASACT,
     *                 NSTATE,NCSF  ,NOCF  ,MAXSPF,LENGTH,NOCFI ,
     *                 GENZRO,EDSHFT,
     *                 D(N001),D(N002),D(N003),
     *                 D(N004),D(N005),
     *                 D(N006),D(N007),D(N008),D(N009),D(N010),D(N011),
     *                 LUNTWO,NMOFZC,NMODOC,NMOACT,NMOEXT,
     *                 NMO   ,NMOEI ,NMOII ,THRGEN,
     *                 D(M000),D(M001),D(M002),D(M003),D(M004),D(M005),
     *                 D(M006),D(M007),D(M008),D(M009),D(M010),
     *                 NCSFNW ,D(M011))
        END IF
      CALL RETFM(NEED3)
C
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (TRACE) THEN
           WRITE(LUNOUT,9100) STRSUB,TIME
           IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        END IF
        CALL FLSHBF(LUNOUT)
C***********************************************************************
C**** MQLMB2 ***********************************************************
C****        ***********************************************************
        STRSUB='MQLMB2'
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
        INIACT=NDOUB+1
        LASACT=NDOUB+NMOACT
C**** ADDRESSING OF WORK AREA ******************************************
C.... PROGRAM IS KEEPING NEED1+NEED2 (DW) NOW.
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C.... NSNSF  ...........................................................
        N001=MQADDR(NOCF+1,'I')
C.... I1EX1  ...........................................................
        N002=MQADDR(NMOACT*(NOCF+1),'I')
C.... I1EX2  ...........................................................
        N003=MQADDR(NMOACT*(NOCFI+1),'I')
C.... GEN1WK ...........................................................
        N004=MQADDR(NMOACT*MAXSPF*MAXSPF,'D')
C.... GEN1   ...........................................................
        N005=MQADDR(NMOACT*(NCSFNW+1),'D')
C.... GEN2   ...........................................................
        N006=MQADDR(NMOACT*NMOACT*(NCSFNW+1),'D')
C.... LABA2  ...........................................................
        N007=MQADDR(3*LENGTH,'I')
C.... LAB1   ...........................................................
        N008=MQADDR(2*LENGTH,'I')
C.... WORK   ...........................................................
        N009=MQADDR(LENGTH,'D')
C.... VAL2   ...........................................................
        N010=MQADDR(NSTATE,'D')
C.... HEFF2  ...........................................................
        N011=MQADDR(NSTATE,'D')
C
        LASTP1=MQMNXT()
        NEED3=LASTP1-N001
        CALL GETFM(NEED3)
C --- ADDITIONAL WORK AREA ---
        CALL GOTFM(NGOTMX)
        LMB2OP=0
        NREQ1=(NWDVAR+3)*NWFTBP/NWDVAR+NWFTB*4/NWDVAR
C   CAUTION: NREQ2 CAN OVERFLOW IN A 32 BIT MACHINE, SO USE FLOATING PT.
C       NREQ2=NMOACT*MAXSPF*MAXSPF*NWFTCG+NWFTCG*4/NWDVAR + 3
        FREQ2=NMOACT
        FREQ2= (FREQ2*MAXSPF*MAXSPF)*NWFTCG
     *       + (NWFTCG*4.0D+00)/NWDVAR + 3.0D+00
        IF (LPOUT.LT.0 .AND. MASWRK) THEN
           WRITE(LUNOUT,*)
     *          'NWFTB=',NWFTB,', NWFTBP=',NWFTBP,', NWFTBG=',NWFTBG
           WRITE(LUNOUT,*) 'NGOTMX=',NGOTMX
           WRITE(LUNOUT,*)
     *          'NREQ1=',NREQ1,', NREQ2=',FREQ2
           CALL FLSHBF(LUNOUT)
        END IF
C
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
        IF (NGOTMX.GT.FREQ2) THEN
           LMB2OP=2
           N101=MQADDR(NWFTBG*4,'I')
           N102=MQADDR(3,'I')
           N103=MQADDR(NMOACT*MAXSPF*MAXSPF*NWFTBG,'D')
           NGOTMX=NGOTMX - NWFTBG*4/NWDVAR
     *                   - NMOACT*MAXSPF*MAXSPF*NWFTBG - 3
        ELSE IF (NREQ1.GT.0 .AND. NGOTMX.GT.NREQ1) THEN
           LMB2OP=1
           N101=MQADDR(NWFTB*4,'I')
           N102=MQADDR(NWFTBP*3,'I')
           N103=MQADDR(NWFTBP,'D')
           NGOTMX=NGOTMX-NREQ1-2
        ELSE
           LMB2OP=0
           N101=MQADDR(4,'I')
           N102=MQADDR(3,'I')
           N103=MQADDR(1,'D')
           NGOTMX=NGOTMX-8
        END IF
        LASTP1=MQMNXT()
        NEED4=LASTP1-N101
        CALL GETFM(NEED4)
C**** ERROR STOP *******************************************************
C       IDUM=MQMCHK()
C**** READ NSNSF, I1EX1, I1EX2 *****************************************
        NDSIZE=MQDSIZ(NOCF+1,'I')
        CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,3,1)
        NDSIZE=MQDSIZ(NMOACT*(NOCF+1),'I')
        CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,5,1)
        NDSIZE=MQDSIZ(NMOACT*(NOCFI+1),'I')
        CALL MQDARE(IDAF50,IODA50,D(N003),NDSIZE,6,1)
C
        IF(EXETYP.NE.CHECK) THEN
           CALL MQLMB2(LUNOUT,LPOUT ,LUNFTA,LUNFT1,LUNFT2,
     *                 INIACT,LASACT,NCSF  ,NOCF  ,MAXSPF,LENGTH,
     *                 NOCFI ,
     *                 GENZRO,EDSHFT,
     *                 D(N001),D(N002),D(N003),
     *                 D(N004),D(N005),D(N006),
     *                 D(N007),D(N008),D(N009),D(N010),D(N011),
     *                 NMOFZC,NMODOC,NMOACT,NMOEXT,
     *                 ISTATE,NSTATE,NMO   ,NMOEI ,NMOII ,THRGEN,
     *                 D(M000),D(M001),D(M002),D(M003),D(M004),D(M005),
     *                 D(M006),D(M007),D(M009),D(M010),
     *                 NCSFNW ,D(M011),
     *                 LMB2OP ,D(N101),D(N102),D(N103))
        END IF
        CALL RETFM(NEED4)
        CALL RETFM(NEED3)
C
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (TRACE) THEN
           WRITE(LUNOUT,9100) STRSUB,TIME
           IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        END IF
        CALL FLSHBF(LUNOUT)
C***********************************************************************
C**** MQLMB3 ***********************************************************
C****        ***********************************************************
        STRSUB='MQLMB3'
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
        INIACT=NDOUB+1
        LASACT=NDOUB+NMOACT
C**** ADDRESSING OF WORK AREA ******************************************
C.... PROGRAM IS KEEPING NEED1+NEED2 (DW) NOW.
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C.... NSNSF  ...........................................................
        N001=MQADDR(NOCF+1,'I')
C.... I1EX1  ...........................................................
        N002=MQADDR(NMOACT*(NOCF+1),'I')
C.... I1EX2  ...........................................................
        N003=MQADDR(NMOACT*(NOCFI+1),'I')
C.... IOCSF  ...........................................................
        N004=MQADDR(2*NCSF,'I')
C.... GEN1WK ...........................................................
        N005=MQADDR(NMOACT*MAXSPF*MAXSPF,'D')
C.... GEN2   ...........................................................
        N006=MQADDR(NMOACT*NMOACT*(NCSFNW+1),'D')
C.... GEN3   ...........................................................
        N007=MQADDR(NMOACT*NMOACT*NMOACT*MAXSPF,'D')
C.... LABA2  ...........................................................
        N008=MQADDR(3*LENGTH,'I')
C.... WORKR  ...........................................................
        N009=MQADDR(LENGTH,'D')
C.... VAL2   ...........................................................
        N010=MQADDR(NSTATE,'D')
C.... HEFF2  ...........................................................
        N011=MQADDR(NSTATE,'D')
C
        LASTP1=MQMNXT()
        NEED3=LASTP1-N001
        CALL GETFM(NEED3)
C --- ADDITIONAL WORK AREA ---
        CALL GOTFM(NGOTMX)
        LMB3OP=0
        NREQ1=(NWDVAR+3)*NWFTCP/NWDVAR+NWFTC*4/NWDVAR
C   CAUTION: NREQ2 CAN OVERFLOW IN A 32 BIT MACHINE, SO USE FLOATING PT.
C       NREQ2=NMOACT*MAXSPF*MAXSPF*NWFTCG+NWFTCG*4/NWDVAR + 3
        FREQ2 = NMOACT
        FREQ2 = (FREQ2*MAXSPF*MAXSPF)*NWFTCG
     *        + (NWFTCG*4.0D+00)/NWDVAR + 3.0D+00
        NREQ4=(NWDVAR+3)*NWFT2P/NWDVAR+NWFT2*4/NWDVAR
        IF (LPOUT.LT.0 .AND. MASWRK) THEN
           WRITE(LUNOUT,*)
     *          'NWFT2=',NWFT2,', NWFT2P=',NWFT2P,', NWFT2G=',NWFT2G
           WRITE(LUNOUT,*)
     *          'NWFTC=',NWFTC,', NWFTCP=',NWFTCP,', NWFTCG=',NWFTCG
           WRITE(LUNOUT,*) 'NGOTMX=',NGOTMX
           WRITE(LUNOUT,*)
     *          'NREQ1=',NREQ1,', NREQ2=',FREQ2, ', NREQ4=',NREQ4
           CALL FLSHBF(LUNOUT)
        END IF
C
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
        IF (FREQ2.GT.0 .AND. NGOTMX.GT.FREQ2) THEN
           LMB3OP=2
           N101=MQADDR(NWFTCG*4,'I')
           N102=MQADDR(3,'I')
           N103=MQADDR(NMOACT*MAXSPF*MAXSPF*NWFTCG,'D')
           NGOTMX=NGOTMX - NWFTCG*4/NWDVAR
     *                   - NMOACT*MAXSPF*MAXSPF*NWFTCG - 3
        ELSE IF (NREQ1.GT.0 .AND. NGOTMX.GT.NREQ1) THEN
           LMB3OP=1
           N101=MQADDR(NWFTC*4,'I')
           N102=MQADDR(NWFTCP*3,'I')
           N103=MQADDR(NWFTCP,'D')
           NGOTMX=NGOTMX-NREQ1-2
        ELSE
           LMB3OP=0
           N101=MQADDR(4,'I')
           N102=MQADDR(3,'I')
           N103=MQADDR(1,'D')
           NGOTMX=NGOTMX-8
        END IF
        IF (NGOTMX.GT.NREQ4) THEN
           LMB3OP=LMB3OP+4
           N201=MQADDR(NWFT2*4,'I')
           N202=MQADDR(NWFT2P*3,'I')
           N203=MQADDR(NWFT2P,'D')
        ELSE
           N201=MQADDR(4,'I')
           N202=MQADDR(3,'I')
           N203=MQADDR(1,'D')
        END IF
        LASTP1=MQMNXT()
        NEED4=LASTP1-N101
        CALL GETFM(NEED4)
C
C**** ERROR STOP *******************************************************
C       IDUM=MQMCHK()
C**** READ NSNSF, I1EX1, I1EX2, IOCSF **********************************
        NDSIZE=MQDSIZ(NOCF+1,'I')
        CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,3,1)
        NDSIZE=MQDSIZ(NMOACT*(NOCF+1),'I')
        CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,5,1)
        NDSIZE=MQDSIZ(NMOACT*(NOCFI+1),'I')
        CALL MQDARE(IDAF50,IODA50,D(N003),NDSIZE,6,1)
        NDSIZE=MQDSIZ(2*NCSF,'I')
        CALL MQDARE(IDAF50,IODA50,D(N004),NDSIZE,4,1)
C
        IF(EXETYP.NE.CHECK) THEN
           CALL MQLMB3(LUNOUT,LPOUT ,LUNFTC,LUNFT2,
     *                 INIACT,LASACT,NCSF  ,NOCF  ,MAXSPF,LENGTH,
     *                 NOCFI ,EDSHFT,
     *                 D(N001),D(N002),D(N003),D(N004),
     *                 D(N005),D(N006),D(N007),
     *                 D(N008),D(N009),D(N010),D(N011),
     *                 NMOFZC,NMODOC,NMOACT,NMOEXT,
     *                 ISTATE,NSTATE,NMO   ,NMOEI ,NMOII ,THRGEN,
     *                 D(M000),D(M001),D(M002),D(M003),D(M005),
     *                 D(M006),D(M007),D(M009),D(M010),
     *                 NCSFNW ,D(M011),
     *                 LMB3OP,D(N101),D(N102),D(N103),
     *                 D(N201),D(N202),D(N203))
        END IF
C
        CALL RETFM(NEED4)
        CALL RETFM(NEED3)
        CALL RETFM(NEED2)
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (TRACE) THEN
           WRITE(LUNOUT,9100) STRSUB,TIME
           IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        END IF
        CALL FLSHBF(LUNOUT)
C
C**** MQLMBR ***********************************************************
C
        STRSUB='MQLMBR'
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
        NMO=NMOFZC+NMODOC+NMOACT+NMOEXT
        INIDOC=NMOFZC+1
        LASDOC=NMOFZC+NMODOC
        INIACT=NMOFZC+NMODOC+1
        LASACT=NMOFZC+NMODOC+NMOACT
        INIEXT=NMOFZC+NMODOC+NMOACT+1
        LASEXT=NMOFZC+NMODOC+NMOACT+NMOEXT
C**** ADDRESSING OF WORK AREA ******************************************
C.... PROGRAM IS KEEPING NEED1 (DW) NOW.
        CALL VALFM(LOADFM)
        CALL GOTFM(NGOTMX)
        IDUM=MQINIT(LOADFM+1)
C.... CASVEC ...........................................................
        N001=MQADDR(NCSF*NSTATE,'D')
C.... EORB   ...........................................................
        N002=MQADDR(NMO,'D')
C.... ECONF  ...........................................................
        N004=MQADDR(NCSF,'D')
C.... EREF0  ...........................................................
        N005=MQADDR(NSTATE,'D')
C.... LABEL1 ...........................................................
        N006=MQADDR(3*LENGTH,'I')
C.... LABEL2 ...........................................................
        N007=N006
C.... WORK   ...........................................................
        N008=MQADDR(LENGTH,'D')
C.... KREF   ...........................................................
        N009=MQADDR(NCSF,'L')
C.... IOCSF  ...........................................................
        N010=MQADDR(2*NCSF,'I')
C.... MOSYM  ...........................................................
        N011=MQADDR(NMO,'I')
C.... VAL2   ...........................................................
        N012=MQADDR(NSTATE,'D')
C.... HEFF2  ...........................................................
        N013=MQADDR(NSTATE*2,'D')
C.... V2READ ...........................................................
        N014=MQADDR((NMODOC+NMOACT)**2*NMOEXT,'D')
C.... VTWOEL ...........................................................
        LASTP1=MQMNXT()
        NR=N001+NGOTMX-LASTP1
C       NR=MQMLFT()
        NDSIZE=(NMODOC+NMOACT)**2*NMOEXT
C
C        IF (IDMWTH.LT.NMOEXT) THEN
C           INIEX1=INIEXT
C           LASEX1=LASEXT
C           NMOEXP=NMOEXT
C        ELSE
           INIEX1=INIEXT
           NMOEXP=0
           DO IPROC=0,ME
              INIEX1=INIEX1+NMOEXP
              NMOEXP=NMOEXT/NPROC
              IF (MOD(NMOEXT,NPROC).GT.IPROC) NMOEXP=NMOEXP+1
           END DO
           LASEX1=INIEX1+NMOEXP-1
C        END IF
C
      IWIDTH=MIN(NR/NDSIZE,NMOEXP)
C
C
      IF (MASWRK.AND.LPOUT.LT.0) THEN
         WRITE(LUNOUT,*) 'NR    =',NR
         WRITE(LUNOUT,*) 'NDSIZE=',NDSIZE
         WRITE(LUNOUT,*) 'NMOEXT=',NMOEXT
         WRITE(LUNOUT,*) 'NMOEXP=',NMOEXP
         WRITE(LUNOUT,*) 'NPROC =',NPROC
         WRITE(LUNOUT,*) 'NDSIZE*NMOEXT=',NDSIZE*NMOEXT
         WRITE(LUNOUT,*) 'NDSIZE*NMOEXP=',NDSIZE*NMOEXP
         WRITE(LUNOUT,*) 'IWIDTH=',IWIDTH
      END IF
C
        N015=MQADDR(NDSIZE*IWIDTH,'D')
        IF (MASWRK) THEN
           NREAD=(NMOEXP-1)/IWIDTH+1
           IF (NREAD.GT.80.AND.IDMWTH.LT.NMOEXT) THEN
              WRITE(LUNOUT,
     *           '('' *** ERROR STOP IN SUB.MQDRV ***''/
     *             '' NREAD FOR SUB.MQLMBR IS GREATER THAN 80.''/
     *             '' NREAD ='',I10)') NREAD
              CALL ABRT
           END IF
        END IF
        IF(TRACE) WRITE(LUNOUT,
     *       '('' NO. DISK READS FOR SUB.MQLMBR ='',I10)') NREAD
C
        LASTP1=MQMNXT()
        NEED2=LASTP1-N001
        CALL GETFM(NEED2)
C**** READ CASVEC, EORB, VONEEL, ECONF, EREF0, KREF, MOSYM *************
C.... CASVEC ...........................................................
        NDSIZE=NCSF*NSTATE
        CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,18,0)
C.... EORB   ...........................................................
        NDSIZE=NMO
        CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,9,0)
C.... ECONF  ...........................................................
        NDSIZE=NCSF
        CALL MQDARE(IDAF50,IODA50,D(N004),NDSIZE,21,0)
C.... EREF0  ...........................................................
        NDSIZE=NSTATE
        CALL MQDARE(IDAF50,IODA50,D(N005),NDSIZE,22,0)
C.... KREF   ...........................................................
        NDSIZE=MQDSIZ(NCSF,'L')
        CALL MQDARE(IDAF50,IODA50,D(N009),NDSIZE,11,1)
C.... IOCSF  ...........................................................
        NDSIZE=MQDSIZ(2*NCSF,'I')
        CALL MQDARE(IDAF50,IODA50,D(N010),NDSIZE,4,1)
C.... MOSYM  ...........................................................
        NDSIZE=MQDSIZ(NMO,'I')
        CALL MQDARE(IDAF50,IODA50,D(N011),NDSIZE,10,1)
C
C        ISTART=INIEXT
        ISTART=INIEX1
  110   CONTINUE
          ISTAM1=ISTART-1
          IEND=ISTAM1+IWIDTH
C          IF(IEND.GT.LASEXT) IEND=LASEXT
          IF(IEND.GT.LASEX1) IEND=LASEX1
          IF(EXETYP.NE.CHECK) THEN
             CALL MQLMBR(LUNOUT,LPOUT ,LUNTWO,LUNFT1,LUNFT2,
     *                   NMOFZC,NMODOC,NMOACT,NMOEXT,NMO   ,
     *                   INIDOC,LASDOC,INIACT,LASACT,INIEXT,LASEXT,
     *                   NCSF  ,NSTATE,LENGTH,ISTATE,THRGEN,
     *                   ISTART,IEND  ,
     *                   IROT  ,EDSHFT,DTWOEL,IDMWTH,
     *                   D(M000),D(N001),D(N002),D(N004),
     *                   D(N005),D(N006),D(N007),D(N008),D(N009),
     *                   D(N010),D(N011),D(N012),D(N013),D(N014),
     *                   D(N015))
          END IF
          ISTART=ISTART+IWIDTH
C        IF(ISTART.LE.LASEXT) GO TO 110
        IF(ISTART.LE.LASEX1) GO TO 110
C
        CALL RETFM(NEED2)
C
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (TRACE) THEN
           WRITE(LUNOUT,9100) STRSUB,TIME
           IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        END IF
C
        CALL TSECND(TIM1)
        TIME = TIM1 - TIM0
        TIM0 = TIM1
        IF(MASWRK) THEN
           IF(NSTATE.GT.1) WRITE(LUNOUT,9530) IALPHA
           WRITE(LUNOUT,9540) TIME
        END IF
        CALL FLSHBF(LUNOUT)
        CALL FLSHBF(LUNOUT)
 9530 FORMAT(1X,'DONE EVALUATING PERTURBATIONS TO STATE',I4)
 9540 FORMAT(1X,'TIME FOR PERTURBATION TREATMENT =',F10.2)
      END DO
C
C     THIS IS THE END OF THE -IALPA- LOOP OVER STATES
C
C***********************************************************************
C**** MQVSML ***********************************************************
C****        ***********************************************************
C     IF(NPCONF.GT.0 .OR. ISELCT.NE.0 .OR.
C    *   (IQDCHK.NE.0 .AND. IQDFLG.EQ.1) ) THEN
      IF(ISELCT.NE.0) THEN
        STRSUB='MQVSML'
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
        INIMO=NDOUB+1
        LASMO=NDOUB+NMOACT
        NMOAA=NMOACT*(NMOACT+1)/2
C**** ADDRESSING OF WORK AREA ******************************************
C.... PROGRAM IS KEEPING NEED1 (DW) NOW.
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C.... CASVEC ...........................................................
        N001=MQADDR(NCSF*NSTATE,'D')
C.... ECONF  ...........................................................
        N002=MQADDR(NCSF,'D')
C.... EREF0  ...........................................................
        N003=MQADDR(NSTATE,'D')
C.... LWCD   ...........................................................
        N004=MQADDR(2*MAXERI,'I')
C.... LABEL1 ...........................................................
        N005=MQADDR(4*MAXCSF,'I')
C.... WORKER ...........................................................
        N006=MQADDR(MAXERI,'D')
C.... WORKGE ...........................................................
        N007=MQADDR(MAXCSF,'D')
C.... LIJMO  ...........................................................
        N008=MQADDR(NMOACT*NMOACT,'I')
C.... HCORE  ...........................................................
        N009=MQADDR(NMO*NMO,'D')
C.... VONEEL ...........................................................
        N010=MQADDR(NMOACT*NMOACT,'D')
C.... VTWOEL ...........................................................
        N011=MQADDR(NMOAA*NMOAA,'D')
C.... VSML   ...........................................................
        N012=MQADDR(NCSF*NSTATE,'D')
C.... KREF   ...........................................................
        N013=MQADDR(NCSF,'L')
C
        LASTP1=MQMNXT()
        NEED2=LASTP1-N001
        CALL GETFM(NEED2)
C**** ERROR STOP *******************************************************
C       IDUM=MQMCHK()
C**** READ CASVEC, ECONF, EREF0, HCORE, KREF ***************************
        NDSIZE=NCSF*NSTATE
        CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,18,0)
        NDSIZE=NCSF
        CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,21,0)
        NDSIZE=NSTATE
        CALL MQDARE(IDAF50,IODA50,D(N003),NDSIZE,22,0)
        NDSIZE=NMO*NMO
        CALL MQDARE(IDAF50,IODA50,D(N009),NDSIZE,16,0)
        NDSIZE=MQDSIZ(NCSF,'L')
        CALL MQDARE(IDAF50,IODA50,D(N013),NDSIZE,11,1)
C
        CALL MQVSML(LUNOUT,LPOUT ,LUNJM1,LUNFT0,
     *              NMO   ,NMOACT,INIMO ,LASMO ,NMOAA ,NCSF  ,
     *              NSTATE,MAXERI,MAXCSF,
     *              D(M000),D(N001),D(N002),D(N003),D(N004),D(N005),
     *              D(N006),D(N007),D(N008),D(N009),D(N010),D(N011),
     *              D(N012),D(N013),EDSHFT)
        CALL RETFM(NEED2)
C
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (TRACE) THEN
           WRITE(LUNOUT,9100) STRSUB,TIME
           IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        END IF
        CALL FLSHBF(LUNOUT)
      END IF
C
C     DIAGONALIZE THE EFFECTIVE HAMILTONIAN TO PRODUCE FINAL RESULTS
C
      IF (MASWRK) THEN
         IF(NSTATE.EQ.1) THEN
            WRITE(LUNOUT,9550)
         ELSE
            WRITE(LUNOUT,9560) NSTATE
         END IF
      END IF
 9550 FORMAT(/1X,'SINGLE STATE MULTICONFIGURATIONAL PERTURBATION',
     *           ' ENERGY (MRMP)'/)
 9560 FORMAT(/1X,'MULTICONFIGURATION QUASIDEGENERATE PERTURBATION',
     *           ' (MCQDPT) ENERGY FOR',I4,' STATES'/)
C
      STRSUB='MQGETE'
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      CALL TSECND(STIME)
      WALL1=WALL
C**** ADDRESSING OF WORK AREA ******************************************
C.... PROGRAM IS KEEPING NEED1 (DW) NOW.
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... CASVEC ...........................................................
      N001=MQADDR(NCSF*NSTATE,'D')
C.... CASENE ...........................................................
      N002=MQADDR(NSTATE,'D')
C.... EIGVAL ...........................................................
      N003=MQADDR(NSTATE,'D')
C.... EIGVEC ...........................................................
      N004=MQADDR(NCSF*NSTATE,'D')
C.... DIAHMA ...........................................................
      N005=MQADDR(NSTATE*(NSTATE+1)/2,'D')
C.... DIAVAL ...........................................................
      N006=MQADDR(NSTATE,'D')
C.... DIAVEC ...........................................................
      N007=MQADDR(NSTATE*NSTATE,'D')
C.... DIAWRK ...........................................................
      N008=MQADDR(NSTATE*8,'D')
C.... LORDER ...........................................................
      N009=MQADDR(NSTATE,'I')
C.... HEFFVC ...........................................................
      N012=MQADDR(NSTATE*NSTATE,'D')
C
      LASTP1=MQMNXT()
      NEED2=LASTP1-N001
      CALL GETFM(NEED2)
C**** ERROR STOP *******************************************************
C     IDUM=MQMCHK()
C**** READ CASENE, CASVEC **********************************************
      NDSIZE=NSTATE
      CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,17,0)
      NDSIZE=NCSF*NSTATE
      CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,18,0)
C
      CALL MQGETE(LUNOUT,LPOUT ,
     *            NCSF  ,NSTATE,1000  ,1000  ,IROT  ,
     *            D(M000),D(N001),D(N002),D(N003),D(N004),
     *            D(N005),D(N006),D(N007),D(N008),D(N009),
     *            D(N012))
C**** WRITE HEFF, EIGVAL, HEFFVC ***************************************
      NDSIZE=NSTATE*NSTATE*4
      CALL MQDAWR(IDAF50,IODA50,D(M000),NDSIZE,23,0)
      NDSIZE=NSTATE
      CALL MQDAWR(IDAF50,IODA50,D(N003),NDSIZE,24,0)
      NDSIZE=NSTATE*NSTATE
      CALL MQDAWR(IDAF50,IODA50,D(N012),NDSIZE,25,0)
C
C        PRINT PERTURBED STATE ENERGIES AND EIGENVECTORS
C
      IF(MASWRK) WRITE(LUNOUT,9570)
 9570 FORMAT(/1X,'THE UNPERTURBED CAS-CI STATE(S) WERE PRINTED ABOVE')
 9580 FORMAT(/1X,'THE PERTURBED STATE IS THE SAME AS THE UNPERTURBED',
     *           ' STATE,'/
     *        1X,'SINCE THE EFFECTIVE HAMILTONIAN IS OF DIMENSION ONE.')
 9590 FORMAT(/1X,'*** PERTURBED MCQDPT STATES ***'/
     *           1X,'(FROM DIAGONALIZATION OF 2ND ORDER',
     *              ' EFFECTIVE HAMILTONIAN)')
      IF(NSTATE.EQ.1) THEN
         IF(MASWRK) WRITE(LUNOUT,9580)
      ELSE
         IF(MASWRK) WRITE(LUNOUT,9590)
         L001=MQADDR((NMOACT+1)*NOCF,'I')
         L002=MQADDR((MAXSOC+1)*MAXSPF,'I')
         L003=MQADDR(2*NCSF,'I')
         L004=MQADDR(NMOACT,'I')
         LASTP1=MQMNXT()
         NEEDL=LASTP1-L001
         CALL GETFM(NEEDL)
         IF(EXETYP.NE.CHECK) THEN
            NDSIZE=MQDSIZ((NMOACT+1)*NOCF,'I')
            CALL MQDARE(IDAF50,IODA50,D(L001),NDSIZE,1,1)
            NDSIZE=MQDSIZ((MAXSOC+1)*MAXSPF,'I')
            CALL MQDARE(IDAF50,IODA50,D(L002),NDSIZE,2,1)
            NDSIZE=MQDSIZ(2*NCSF,'I')
            CALL MQDARE(IDAF50,IODA50,D(L003),NDSIZE,4,1)
            CALL MQPRWF(LUNOUT,NCSF,NSTATE,NOCF,MAXSOC,MAXSPF,NMOACT,
     *                  D(N003),D(N004),D(L001),D(L002),D(L003),D(L004))
         END IF
         CALL RETFM(NEEDL)
      END IF
C
      CALL RETFM(NEED2)
      CALL RETFM(NEED1)
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF (TRACE) THEN
         WRITE(LUNOUT,9100) STRSUB,TIME
         IF(LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
      END IF
C
C**** CLOSE FILES ******************************************************
C
 1000 CONTINUE
      CALL FLSHBF(LUNOUT)
      CLOSE(UNIT=IDAF50,STATUS='KEEP')
      IF (DELSCR) THEN
         CALL SEQCLO(LUNFT0,'DELETE')
         CALL SEQCLO(LUNFTA,'DELETE')
         CALL SEQCLO(LUNFTC,'DELETE')
      ELSE
         CALL SEQCLO(LUNFT0,'KEEP')
         CALL SEQCLO(LUNFTA,'KEEP')
         CALL SEQCLO(LUNFTC,'KEEP')
      END IF
      CALL SEQCLO(LUNTWO,'KEEP')
      IF (DELSCR) THEN
         CALL SEQCLO(LUNFT1,'DELETE')
         CALL SEQCLO(LUNFT2,'DELETE')
         CALL SEQCLO(LUNPUN,'DELETE')
      ELSE
         CALL SEQCLO(LUNFT1,'KEEP')
         CALL SEQCLO(LUNFT2,'KEEP')
         CALL SEQCLO(LUNPUN,'KEEP')
      END IF
      CALL SEQCLO(LUNCNF,'KEEP')
      CALL SEQCLO(LUNOER,'KEEP')
      CALL SEQCLO(LUNJM1,'KEEP')
      CALL SEQCLO(LUNJM2,'KEEP')
      CALL SEQCLO(LUNJM3,'KEEP')
C
      IF(NOSYM.NE.0) THEN
         CALL SYMON
      END IF
C
      GO TO 999
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%                 REFERENCE WEIGHT CALCULATION                 %%%%%
C%%%%                                                              %%%%%
  106 CONTINUE
C***********************************************************************
C**** LOOP FOR STATES **************************************************
C****                 **************************************************
C**** ADDRESSING OF WORK AREA ******************************************
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... EPART0 ...........................................................
      L001=MQADDR(NSTATE*NSTATE,'D')
C.... EPART1 ...........................................................
      L002=MQADDR((NMOACT+NMOEXT)*NSTATE*NSTATE,'D')
C.... EPART2 ...........................................................
      L003=MQADDR((NMOACT+NMOEXT)**2*NSTATE*NSTATE*2,'D')
C.... OVRLP0 ...........................................................
      L004=MQADDR(NSTATE*NSTATE,'D')
C.... OVRLP1 ...........................................................
      L005=MQADDR((NMOACT+NMOEXT)*NSTATE*NSTATE,'D')
C.... OVRLP2 ...........................................................
      L006=MQADDR((NMOACT+NMOEXT)**2*NSTATE*NSTATE*2,'D')
C
      LASTP1=MQMNXT()
      NEED0=LASTP1-L001
      CALL GETFM(NEED0)
C
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... HEFF   ...........................................................
      M000=MQADDR(NSTATE*NSTATE*4,'D')
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-M000
      CALL GETFM(NEED1)
C
CC    NHEFF=1
CC   &  +NSTATE*NSTATE*6
CC   &  +(NMOACT+NMOEXT)*NSTATE*NSTATE*2
CC   &  +(NMOACT+NMOEXT)**2*NSTATE*NSTATE*4
C     NHEFF=1
C    &  +NSTATE*NSTATE
C    &  +(NMOACT+NMOEXT)*NSTATE*NSTATE
C    &  +(NMOACT+NMOEXT)**2*NSTATE*NSTATE*2
C    &  +NSTATE*NSTATE
C    &  +(NMOACT+NMOEXT)*NSTATE*NSTATE
C    &  +(NMOACT+NMOEXT)**2*NSTATE*NSTATE*2
C    &  +NSTATE*NSTATE*4
C
      DO IALPHA=1,NSTATE
        ISTATE=IALPHA
C***********************************************************************
C**** MQWGT1 ***********************************************************
C****        ***********************************************************
        IF(NSTATE.GT.1 .AND. MASWRK) WRITE(LUNOUT,'(/1X,
     *         ''EVALUATING MATRIX ELEMENTS BETWEEN STATE'',I4,
     *         '' AND ALL OTHER STATES.''/)') IALPHA
        STRSUB='MQWGT1'
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
        INIACT=NDOUB+1
        LASACT=NDOUB+NMOACT
        NMO=NMOFZC+NMODOC+NMOACT+NMOEXT
        NMODA=NMODOC+NMOACT
        NMOEI=NMODA*(NMODA+1)/2+NMOEXT*NMODA
        NMOII=NMODA*(NMODA+1)/2
C**** ADDRESSING OF WORK AREA ******************************************
C.... PROGRAM IS KEEPING NEED1 NOW
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C.... CASVEC ...........................................................
        M001=MQADDR(NCSF*NSTATE,'D')
C.... EORB   ...........................................................
        M002=MQADDR(NMO,'D')
C.... LIJMO  ...........................................................
        M003=MQADDR(NMO*NMO,'I')
C.... VONEEL ...........................................................
        M004=MQADDR(NMO*NMO,'D')
C.... VTWOEL ...........................................................
        M005=MQADDR(NMOEI*NMOII,'D')
C.... ECONF  ...........................................................
        M006=MQADDR(NCSF,'D')
C.... EREF0  ...........................................................
        M007=MQADDR(NSTATE,'D')
C.... CASENE ...........................................................
        M008=MQADDR(NSTATE,'D')
C.... KREF   ...........................................................
        M009=MQADDR(NCSF,'L')
C.... MOSYM  ...........................................................
        M010=MQADDR(NMO,'I')
C.... LOD2NW ...........................................................
        M011=MQADDR(NCSF,'I')
C
        LASTP1=MQMNXT()
        NEED2=LASTP1-M001
        CALL GETFM(NEED2)
C
C.... NSNSF  ...........................................................
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C
        N001=MQADDR(NOCF+1,'I')
C.... I1EX1  ...........................................................
        N002=MQADDR(NMOACT*(NOCF+1),'I')
C.... I1EX2  ...........................................................
        N003=MQADDR(NMOACT*(NOCFI+1),'I')
C.... GEN1WK ...........................................................
        N004=MQADDR(NMOACT*MAXSPF*MAXSPF,'D')
C.... GEN1   ...........................................................
        N005=MQADDR(NMOACT*(NCSFNW+1),'D')
C.... LAB1R  ...........................................................
        N006=MQADDR(3*LENGTH,'I')
C.... WORKR  ...........................................................
        N007=MQADDR(LENGTH,'D')
C.... LAB1W  ...........................................................
        N008=MQADDR(2*LENGTH,'I')
C.... WORKW  ...........................................................
        N009=MQADDR(LENGTH,'D')
C.... VAL2   ...........................................................
        N010=MQADDR(NSTATE,'D')
C
        LASTP1=MQMNXT()
        NEED3=LASTP1-N001
        CALL GETFM(NEED3)
C**** ERROR STOP *******************************************************
C        IDUM=MQMCHK()
C**** READ CASVEC, EORB, LIJMO, VONEEL, ECONF, EREF0 *******************
C****      EIGVEC, NSNSF, I1EX1, I1EX2, CASENE, KREF *******************
C.... CASVEC ...........................................................
        NDSIZE=NCSF*NSTATE
        CALL MQDARE(IDAF50,IODA50,D(M001),NDSIZE,18,0)
C.... EORB   ...........................................................
        NDSIZE=NMO
        CALL MQDARE(IDAF50,IODA50,D(M002),NDSIZE,9,0)
C.... LIJMO  ...........................................................
        NDSIZE=MQDSIZ(NMO*NMO,'I')
        CALL MQDARE(IDAF50,IODA50,D(M003),NDSIZE,19,1)
C.... VONEEL ...........................................................
        NDSIZE=NMO*NMO
        CALL MQDARE(IDAF50,IODA50,D(M004),NDSIZE,20,0)
C.... ECONF  ...........................................................
        NDSIZE=NCSF
        CALL MQDARE(IDAF50,IODA50,D(M006),NDSIZE,21,0)
C.... EREF0  ...........................................................
        NDSIZE=NSTATE
        CALL MQDARE(IDAF50,IODA50,D(M007),NDSIZE,22,0)
C.... CASENE ...........................................................
        NDSIZE=NSTATE
        CALL MQDARE(IDAF50,IODA50,D(M008),NDSIZE,17,0)
C.... KREF   ...........................................................
        NDSIZE=MQDSIZ(NCSF,'L')
        CALL MQDARE(IDAF50,IODA50,D(M009),NDSIZE,11,1)
C.... MOSYM  ...........................................................
        NDSIZE=MQDSIZ(NMO,'I')
        CALL MQDARE(IDAF50,IODA50,D(M010),NDSIZE,10,1)
C.... LOD2NW ...........................................................
        NDSIZE=MQDSIZ(NCSF,'I')
        CALL MQDARE(IDAF50,IODA50,D(M011),NDSIZE,13,1)
C.... NSNSF  ...........................................................
        NDSIZE=MQDSIZ(NOCF+1,'I')
        CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,3,1)
C.... I1EX1  ...........................................................
        NDSIZE=MQDSIZ(NMOACT*(NOCF+1),'I')
        CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,5,1)
C.... I1EX2  ...........................................................
        NDSIZE=MQDSIZ(NMOACT*(NOCFI+1),'I')
        CALL MQDARE(IDAF50,IODA50,D(N003),NDSIZE,6,1)
        IF (EXETYP.NE.CHECK) THEN
C**** CALL MQWGT1 ******************************************************
        IALPHAX = IALPHA
        CALL MQWGT1(LUNOUT,LPOUT ,LUNFTA,LUNFT1,
     $              IALPHAX,INIACT,LASACT,
     $              NSTATE,NCSF  ,NOCF  ,MAXSPF,LENGTH,NOCFI ,
     $              GENZRO,
     $              D(N001),D(N002),D(N003),
     $              D(N004),D(N005),
     $              D(N006),D(N007),D(N008),D(N009),D(N010),
     $              LUNTWO,NMOFZC,NMODOC,NMOACT,NMOEXT,
     $              NMO   ,NMOEI ,NMOII ,THRGEN,
     $              D(M000),D(M001),D(M002),D(M003),D(M004),D(M005),
     $              D(M006),D(M007),D(M008),D(M009),D(M010),
     $              NCSFNW ,D(M011),
     $              3      ,
     $              D(L001),D(L002),D(L003),D(L004),D(L005),D(L006))
        END IF
C       CALL MQWGT1(LUNOUT,LPOUT ,LUNFTA,LUNFT1,
C    &              IALPHA,INIACT,LASACT,
C    &              NSTATE,NCSF  ,NOCF  ,MAXSPF,LENGTH,NOCFI ,
C    &              GENZRO,
C    &              NSNSF ,I1EX1 ,I1EX2 ,
C    &              GEN1WK,GEN1  ,
C    &              LAB1R ,WORKR ,LAB1W ,WORKW ,VAL2  ,
C    &              LUNTWO,NMOFZC,NMODOC,NMOACT,NMOEXT,
C    &              NMO   ,NMOEI ,NMOII ,THRGEN,
C    &              HEFF  ,CASVEC,EORB  ,LIJMO ,VONEEL,VTWOEL,
C    &              ECONF ,EREF0 ,CASENE,KREF  ,MOSYM ,
C    &              NCSFNW,LOD2NW,
C    &              ISWTCH,
C    &              EPART0,EPART1,EPART2,OVRLP0,OVRLP1,OVRLP2)
        CALL RETFM(NEED3)
C**** CPU TIME *********************************************************
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (MASWRK) WRITE(LUNOUT,9100) STRSUB,TIME
        IF (MASWRK .AND. LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        CALL FLSHBF(LUNOUT)
C***********************************************************************
C**** MQWGT2 ***********************************************************
C****        ***********************************************************
        STRSUB='MQWGT2'
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
C**** ADDRESSING OF WORK AREA ******************************************
C.... PROGRAM IS KEEPING NEED1+NEED2 (DW) NOW.
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C.... NSNSF  ...........................................................
        N001=MQADDR(NOCF+1,'I')
C.... I1EX1  ...........................................................
        N002=MQADDR(NMOACT*(NOCF+1),'I')
C.... I1EX2  ...........................................................
        N003=MQADDR(NMOACT*(NOCFI+1),'I')
C.... GEN1WK ...........................................................
        N004=MQADDR(NMOACT*MAXSPF*MAXSPF,'D')
C.... GEN1   ...........................................................
        N005=MQADDR(NMOACT*(NCSFNW+1),'D')
C.... GEN2   ...........................................................
        N006=MQADDR(NMOACT*NMOACT*(NCSFNW+1),'D')
C.... LABA2  ...........................................................
        N007=MQADDR(3*LENGTH,'I')
C.... LAB1   ...........................................................
        N008=MQADDR(2*LENGTH,'I')
C.... WORK   ...........................................................
        N009=MQADDR(LENGTH,'D')
C.... VAL2   ...........................................................
        N010=MQADDR(NSTATE,'D')
C
        LASTP1=MQMNXT()
        NEED3=LASTP1-N001
        CALL GETFM(NEED3)
C**** ERROR STOP *******************************************************
C        IDUM=MQMCHK()
C**** READ NSNSF, I1EX1, I1EX2 *****************************************
        NDSIZE=MQDSIZ(NOCF+1,'I')
        CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,3,1)
        NDSIZE=MQDSIZ(NMOACT*(NOCF+1),'I')
        CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,5,1)
        NDSIZE=MQDSIZ(NMOACT*(NOCFI+1),'I')
        CALL MQDARE(IDAF50,IODA50,D(N003),NDSIZE,6,1)
C
        IF(EXETYP.NE.CHECK) THEN
C**** CALL MQWGT2 ******************************************************
        CALL MQWGT2(LUNOUT,LPOUT ,LUNFTA,LUNFT1,LUNFT2,
     $              INIACT,LASACT,NCSF  ,NOCF  ,MAXSPF,LENGTH,
     $              NOCFI ,
     $              GENZRO,
     $              D(N001),D(N002),D(N003),
     $              D(N004),D(N005),D(N006),
     $              D(N007),D(N008),D(N009),D(N010),
     $              NMOFZC,NMODOC,NMOACT,NMOEXT,
     $              ISTATE,NSTATE,NMO   ,NMOEI ,NMOII ,THRGEN,
     $              D(M000),D(M001),D(M002),D(M003),D(M004),D(M005),
     $              D(M006),D(M007),D(M009),D(M010),
     $              NCSFNW ,D(M011),
     $              3      ,
     $              D(L001),D(L002),D(L003),D(L004),D(L005),D(L006))
        END IF
C       CALL MQWGT2(LUNOUT,LPOUT ,LUNFTA,LUNFT1,LUNFT2,
C    &              INIACT,LASACT,NCSF  ,NOCF  ,MAXSPF,LENGTH,
C    &              NOCFI ,
C    &              GENZRO,
C    &              NSNSF ,I1EX1 ,I1EX2 ,
C    &              GEN1WK,GEN1  ,GEN2  ,
C    &              LABA2 ,LAB1  ,WORK  ,VAL2  ,
C    &              NMOFZC,NMODOC,NMOACT,NMOEXT,
C    &              ISTATE,NSTATE,NMO   ,NMOEI ,NMOII ,THRGEN,
C    &              HEFF  ,CASVEC,EORB  ,LIJMO ,VONEEL,VTWOEL,
C    &              ECONF ,EREF0 ,KREF  ,MOSYM ,
C    &              NCSFNW,LOD2NW,
C    &              ISWTCH,
C    &              EPART0,EPART1,EPART2,OVRLP0,OVRLP1,OVRLP2)
        CALL RETFM(NEED3)
C**** CPU TIME *********************************************************
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (MASWRK) WRITE(LUNOUT,9100) STRSUB,TIME
        IF (MASWRK .AND. LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        CALL FLSHBF(LUNOUT)
C***********************************************************************
C**** MQWGT3 ***********************************************************
C****        ***********************************************************
        STRSUB='MQWGT3'
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
        INIACT=NDOUB+1
        LASACT=NDOUB+NMOACT
C**** ADDRESSING OF WORK AREA ******************************************
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C.... NSNSF  ...........................................................
        N001=MQADDR(NOCF+1,'I')
C.... I1EX1  ...........................................................
        N002=MQADDR(NMOACT*(NOCF+1),'I')
C.... I1EX2  ...........................................................
        N003=MQADDR(NMOACT*(NOCFI+1),'I')
C.... IOCSF  ...........................................................
        N004=MQADDR(2*NCSF,'I')
C.... GEN1WK ...........................................................
        N005=MQADDR(NMOACT*MAXSPF*MAXSPF,'D')
C.... GEN2   ...........................................................
        N006=MQADDR(NMOACT*NMOACT*(NCSFNW+1),'D')
C.... GEN3   ...........................................................
        N007=MQADDR(NMOACT*NMOACT*NMOACT*MAXSPF,'D')
C.... LABA2  ...........................................................
        N008=MQADDR(3*LENGTH,'I')
C.... WORKR  ...........................................................
        N009=MQADDR(LENGTH,'D')
C.... VAL2   ...........................................................
        N010=MQADDR(NSTATE,'D')
C
        LASTP1=MQMNXT()
        NEED3=LASTP1-N001
        CALL GETFM(NEED3)
C**** ERROR STOP *******************************************************
C        IDUM=MQMCHK()
C**** READ NSNSF, I1EX1, I1EX2, IOCSF **********************************
        NDSIZE=MQDSIZ(NOCF+1,'I')
        CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,3,1)
        NDSIZE=MQDSIZ(NMOACT*(NOCF+1),'I')
        CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,5,1)
        NDSIZE=MQDSIZ(NMOACT*(NOCFI+1),'I')
        CALL MQDARE(IDAF50,IODA50,D(N003),NDSIZE,6,1)
        NDSIZE=MQDSIZ(2*NCSF,'I')
        CALL MQDARE(IDAF50,IODA50,D(N004),NDSIZE,4,1)
C
        IF(EXETYP.NE.CHECK) THEN
C**** CALL MQWGT3 ******************************************************
        CALL MQWGT3(LUNOUT,LPOUT ,LUNFTC,LUNFT2,
     $              INIACT,LASACT,NCSF  ,NOCF  ,MAXSPF,LENGTH,
     $              NOCFI ,
     $              D(N001),D(N002),D(N003),D(N004),
     $              D(N005),D(N006),D(N007),
     $              D(N008),D(N009),D(N010),
     $              NMOFZC,NMODOC,NMOACT,NMOEXT,
     $              ISTATE,NSTATE,NMO   ,NMOEI ,NMOII ,THRGEN,
     $              D(M000),D(M001),D(M002),D(M003),D(M005),
     $              D(M006),D(M007),D(M009),D(M010),
     $              NCSFNW ,D(M011),
     $              3      ,
     $              D(L001),D(L002),D(L004),D(L005))
        END IF
C       CALL MQWGT3(LUNOUT,LPOUT ,LUNFTA,LUNFT2,
C    &              INIACT,LASACT,NCSF  ,NOCF  ,MAXSPF,LENGTH,
C    &              NOCFI ,
C    &              NSNSF ,I1EX1 ,I1EX2 ,IOCSF ,
C    &              GEN1WK,GEN2  ,GEN3  ,
C    &              LABA2 ,WORKR ,VAL2  ,
C    &              NMOFZC,NMODOC,NMOACT,NMOEXT,
C    &              ISTATE,NSTATE,NMO   ,NMOEI ,NMOII ,THRGEN,
C    &              HEFF  ,CASVEC,EORB  ,LIJMO ,VTWOEL,
C    &              ECONF ,EREF0 ,KREF  ,MOSYM ,
C    &              NCSFNW,LOD2NW,
C    &              ISWTCH,
C    &              EPART0,EPART1,OVRLP0,OVRLP1)
C
        CALL RETFM(NEED3)
        CALL RETFM(NEED2)
C**** CPU TIME *********************************************************
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (MASWRK) WRITE(LUNOUT,9100) STRSUB,TIME
        IF (MASWRK .AND. LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        CALL FLSHBF(LUNOUT)
C***********************************************************************
C**** MQWGTR ***********************************************************
C****        ***********************************************************
        STRSUB='MQWGTR'
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
        NMO=NMOFZC+NMODOC+NMOACT+NMOEXT
        INIDOC=NMOFZC+1
        LASDOC=NMOFZC+NMODOC
        INIACT=NMOFZC+NMODOC+1
        LASACT=NMOFZC+NMODOC+NMOACT
        INIEXT=NMOFZC+NMODOC+NMOACT+1
        LASEXT=NMOFZC+NMODOC+NMOACT+NMOEXT
C**** ADDRESSING OF WORK AREA ******************************************
C.... PROGRAM IS KEEPING NEED1 NOW
        CALL VALFM(LOADFM)
        CALL GOTFM(NGOTMX)
        IDUM=MQINIT(LOADFM+1)
C.... CASVEC ...........................................................
        N001=MQADDR(NCSF*NSTATE,'D')
C.... EORB   ...........................................................
        N002=MQADDR(NMO,'D')
C.... ECONF  ...........................................................
        N004=MQADDR(NCSF,'D')
C.... EREF0  ...........................................................
        N005=MQADDR(NSTATE,'D')
C.... LABEL1 ...........................................................
        N006=MQADDR(2*LENGTH,'I')
C.... LABEL2 ...........................................................
        N007=MQADDR(3*LENGTH,'I')
C.... WORK   ...........................................................
        N008=MQADDR(LENGTH,'D')
C.... KREF   ...........................................................
        N009=MQADDR(NCSF,'L')
C.... IOCSF  ...........................................................
        N010=MQADDR(2*NCSF,'I')
C.... MOSYM  ...........................................................
        N011=MQADDR(NMO,'I')
C.... VAL2   ...........................................................
        N012=MQADDR(NSTATE,'D')
C.... VTWOEL ...........................................................
        LASTP1=MQMNXT()
        NR=N001+NGOTMX-LASTP1
C       NR=MQMLFT()
        NDSIZE=(NMODOC+NMOACT)**2*NMOEXT
        IWIDTH=MIN(NR/NDSIZE,NMOEXT)
        N013=MQADDR(NDSIZE*IWIDTH,'D')
        NREAD=(NMOEXT-1)/IWIDTH+1
        IF(NREAD.GT.80) THEN
          IF (MASWRK)
     *      WRITE(LUNOUT,'('' *** ERROR STOP IN SUB.MQDRV ***''/
     *      '' NREAD FOR SUB.MQLMBR IS GREATER THAN 80.''/
     *      '' NREAD ='',I10)') NREAD
          CALL ABRT
        END IF
        IF (TRACE)
     *  WRITE(LUNOUT,'('' NO. DISK READS FOR SUB.MQLMBR ='',I10)') NREAD
C
        LASTP1=MQMNXT()
        NEED2=LASTP1-N001
        CALL GETFM(NEED2)
C**** READ CASVEC, EORB, VONEEL, ECONF, EREF0, KREF, MOSYM *************
C.... CASVEC ...........................................................
        NDSIZE=NCSF*NSTATE
        CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,18,0)
C.... EORB   ...........................................................
        NDSIZE=NMO
        CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,9,0)
C.... ECONF  ...........................................................
        NDSIZE=NCSF
        CALL MQDARE(IDAF50,IODA50,D(N004),NDSIZE,21,0)
C.... EREF0  ...........................................................
        NDSIZE=NSTATE
        CALL MQDARE(IDAF50,IODA50,D(N005),NDSIZE,22,0)
C.... KREF   ...........................................................
        NDSIZE=MQDSIZ(NCSF,'L')
        CALL MQDARE(IDAF50,IODA50,D(N009),NDSIZE,11,1)
C.... IOCSF  ...........................................................
        NDSIZE=MQDSIZ(2*NCSF,'I')
        CALL MQDARE(IDAF50,IODA50,D(N010),NDSIZE,4,1)
C.... MOSYM  ...........................................................
        NDSIZE=MQDSIZ(NMO,'I')
        CALL MQDARE(IDAF50,IODA50,D(N011),NDSIZE,10,1)
C**** CALL MQWGTR ******************************************************
        ISTART=INIEXT
  108   CONTINUE
          ISTAM1=ISTART-1
          IEND=ISTAM1+IWIDTH
          IF(IEND.GT.LASEXT) IEND=LASEXT
          IF(EXETYP.NE.CHECK) THEN
          CALL MQWGTR(LUNOUT,LPOUT ,LUNTWO,LUNFT1,LUNFT2,
     $                NMOFZC,NMODOC,NMOACT,NMOEXT,NMO   ,
     $                INIDOC,LASDOC,INIACT,LASACT,INIEXT,LASEXT,
     $                NCSF  ,NSTATE,LENGTH,ISTATE,THRGEN,
     $                ISTART,IEND  ,
     $                IROT  ,
     $                D(M000),D(N001),D(N002),D(N004),
     $                D(N005),D(N006),D(N007),D(N008),D(N009),D(N010),
     $                D(N011),D(N012),D(N013),
     $                3      ,
     $                D(L003),D(L006))
          END IF
          ISTART=ISTART+IWIDTH
        IF(ISTART.LE.LASEXT) GO TO 108
C
        CALL RETFM(NEED2)
C
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (MASWRK) WRITE(LUNOUT,9100) STRSUB,TIME
        IF (MASWRK .AND. LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        CALL FLSHBF(LUNOUT)
C***********************************************************************
C**** LOOP-END *********************************************************
C****          *********************************************************
      END DO
C***********************************************************************
C**** MQWVSM ***********************************************************
C****        ***********************************************************
C     IF(NPCONF.GT.0 .OR. ISELCT.NE.0 .OR.
C    &   (IQDCHK.NE.0 .AND. IQDFLG.EQ.1) ) THEN
      IF(ISELCT.NE.0) THEN
        STRSUB='MQWVSM'
        IF(TRACE) WRITE(LUNOUT,9000) STRSUB
        CALL TSECND(STIME)
        WALL1=WALL
        INIMO=NDOUB+1
        LASMO=NDOUB+NMOACT
        NMOAA=NMOACT*(NMOACT+1)/2
C**** ADDRESSING OF WORK AREA ******************************************
        CALL VALFM(LOADFM)
        IDUM=MQINIT(LOADFM+1)
C.... CASVEC ...........................................................
        N001=MQADDR(NCSF*NSTATE,'D')
C.... ECONF  ...........................................................
        N002=MQADDR(NCSF,'D')
C.... EREF0  ...........................................................
        N003=MQADDR(NSTATE,'D')
C.... LWCD   ...........................................................
        N004=MQADDR(2*MAXERI,'I')
C.... LABEL1 ...........................................................
        N005=MQADDR(4*MAXCSF,'I')
C.... WORKER ...........................................................
        N006=MQADDR(MAXERI,'D')
C.... WORKGE ...........................................................
        N007=MQADDR(MAXCSF,'D')
C.... LIJMO  ...........................................................
        N008=MQADDR(NMOACT*NMOACT,'I')
C.... HCORE  ...........................................................
        N009=MQADDR(NMO*NMO,'D')
C.... VONEEL ...........................................................
        N010=MQADDR(NMOACT*NMOACT,'D')
C.... VTWOEL ...........................................................
        N011=MQADDR(NMOAA*NMOAA,'D')
C.... VSML   ...........................................................
        N012=MQADDR(NCSF*NSTATE,'D')
C.... KREF   ...........................................................
        N013=MQADDR(NCSF,'L')
C
        LASTP1=MQMNXT()
        NEED2=LASTP1-N001
        CALL GETFM(NEED2)
C**** ERROR STOP *******************************************************
C        IDUM=MQMCHK()
C**** READ CASVEC, ECONF, EREF0, HCORE, KREF ***************************
        NDSIZE=NCSF*NSTATE
        CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,18,0)
        NDSIZE=NCSF
        CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,21,0)
        NDSIZE=NSTATE
        CALL MQDARE(IDAF50,IODA50,D(N003),NDSIZE,22,0)
        NDSIZE=NMO*NMO
        CALL MQDARE(IDAF50,IODA50,D(N009),NDSIZE,16,0)
        NDSIZE=MQDSIZ(NCSF,'L')
        CALL MQDARE(IDAF50,IODA50,D(N013),NDSIZE,11,1)
C
        CALL MQWVSM(LUNOUT,LPOUT ,LUNJM1,LUNFT0,
     $              NMO   ,NMOACT,INIMO ,LASMO ,NMOAA ,NCSF  ,
     $              NSTATE,MAXERI,MAXCSF,
     $              D(M000),D(N001),D(N002),D(N003),D(N004),D(N005),
     $              D(N006),D(N007),D(N008),D(N009),D(N010),D(N011),
     $              D(N012),D(N013),
     $              3      ,D(L001),D(L004))
        CALL RETFM(NEED2)
C
        CALL TSECND(ETIME)
        TIME=ETIME-STIME
        WALL1=WALL-WALL1
        IF (MASWRK) WRITE(LUNOUT,9100) STRSUB,TIME
        IF (MASWRK .AND. LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
        CALL FLSHBF(LUNOUT)
      END IF
C***********************************************************************
C**** MQGETE ***********************************************************
C****        ***********************************************************
      IF (MASWRK) THEN
         IF(NSTATE.EQ.1) THEN
            WRITE(LUNOUT,9550)
         ELSE
            WRITE(LUNOUT,9560) NSTATE
         END IF
      END IF
      STRSUB='MQGETE'
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      CALL TSECND(STIME)
      WALL1=WALL
C**** ADDRESSING OF WORK AREA ******************************************
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... CASVEC ...........................................................
      N001=MQADDR(NCSF*NSTATE,'D')
C.... CASENE ...........................................................
      N002=MQADDR(NSTATE,'D')
C.... EIGVAL ...........................................................
      N003=MQADDR(NSTATE,'D')
C.... EIGVEC ...........................................................
      N004=MQADDR(NCSF*NSTATE,'D')
C.... DIAHMA ...........................................................
      N005=MQADDR(NSTATE*(NSTATE+1)/2,'D')
C.... DIAVAL ...........................................................
      N006=MQADDR(NSTATE,'D')
C.... DIAVEC ...........................................................
      N007=MQADDR(NSTATE*NSTATE,'D')
C.... DIAWRK ...........................................................
      N008=MQADDR(NSTATE*8,'D')
C.... LORDER ...........................................................
      N009=MQADDR(NSTATE,'I')
C.... HEFFVC ...........................................................
      N012=MQADDR(NSTATE*NSTATE,'D')
C
      LASTP1=MQMNXT()
      NEED2=LASTP1-N001
      CALL GETFM(NEED2)
C**** ERROR STOP *******************************************************
C      IDUM=MQMCHK()
C**** READ CASENE, CASVEC **********************************************
      NDSIZE=NSTATE
      CALL MQDARE(IDAF50,IODA50,D(N002),NDSIZE,17,0)
      NDSIZE=NCSF*NSTATE
      CALL MQDARE(IDAF50,IODA50,D(N001),NDSIZE,18,0)
C
      CALL MQGETE(LUNOUT,LPOUT ,
     $            NCSF  ,NSTATE,1000  ,1000  ,IROT  ,
     $            D(M000),D(N001),D(N002),D(N003),D(N004),
     $            D(N005),D(N006),D(N007),D(N008),D(N009),
     $            D(N012))
C**** WRITE HEFF, EIGVAL, HEFFVC ***************************************
      NDSIZE=NSTATE*NSTATE*4
      CALL MQDAWR(IDAF50,IODA50,D(M000),NDSIZE,23,0)
      NDSIZE=NSTATE
      CALL MQDAWR(IDAF50,IODA50,D(N003),NDSIZE,24,0)
      NDSIZE=NSTATE*NSTATE
      CALL MQDAWR(IDAF50,IODA50,D(N012),NDSIZE,25,0)
C
C        PRINT PERTURBED STATE ENERGIES AND EIGENVECTORS
C
      IF(MASWRK) WRITE(LUNOUT,9570)
      IF(NSTATE.EQ.1) THEN
         IF(MASWRK) WRITE(LUNOUT,9580)
      ELSE
         IF(MASWRK) WRITE(LUNOUT,9590)
         LL01=MQADDR((NMOACT+1)*NOCF,'I')
         LL02=MQADDR((MAXSOC+1)*MAXSPF,'I')
         LL03=MQADDR(2*NCSF,'I')
         LL04=MQADDR(NMOACT,'I')
         LASTP1=MQMNXT()
         NEEDL=LASTP1-LL01
         CALL GETFM(NEEDL)
         IF(EXETYP.NE.CHECK) THEN
            NDSIZE=MQDSIZ((NMOACT+1)*NOCF,'I')
            CALL MQDARE(IDAF50,IODA50,D(LL01),NDSIZE,1,1)
            NDSIZE=MQDSIZ((MAXSOC+1)*MAXSPF,'I')
            CALL MQDARE(IDAF50,IODA50,D(LL02),NDSIZE,2,1)
            NDSIZE=MQDSIZ(2*NCSF,'I')
            CALL MQDARE(IDAF50,IODA50,D(LL03),NDSIZE,4,1)
            CALL MQPRWF(LUNOUT,NCSF,NSTATE,NOCF,MAXSOC,MAXSPF,NMOACT,
     *                  D(N003),D(N004),D(LL01),D(LL02),D(LL03),D(LL04))
         END IF
         CALL RETFM(NEEDL)
      END IF
C
      CALL RETFM(NEED2)
C**** CPU TIME *********************************************************
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF (MASWRK) WRITE(LUNOUT,9100) STRSUB,TIME
      IF (MASWRK .AND. LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
      CALL FLSHBF(LUNOUT)
C***********************************************************************
C**** MQGETW ***********************************************************
C****        ***********************************************************
      STRSUB='MQGETW'
      IF(TRACE) WRITE(LUNOUT,9000) STRSUB
      CALL TSECND(STIME)
      WALL1=WALL
      INIACT=NMOFZC+NMODOC+1
      LASACT=NMOFZC+NMODOC+NMOACT
      INIEXT=NMOFZC+NMODOC+NMOACT+1
      LASEXT=NMOFZC+NMODOC+NMOACT+NMOEXT
C**** ADDRESSING OF WORK AREA ******************************************
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... EPARTN ...........................................................
      N001=MQADDR(NSTATE*NSTATE*6,'D')
C.... OVRLAP ...........................................................
      N002=MQADDR(NSTATE*NSTATE*6,'D')
C.... OVRPER ...........................................................
      N003=MQADDR(NSTATE*2,'D')
C.... EPAR21 ...........................................................
      N004=MQADDR((NMOACT+NMOEXT)*NSTATE*2,'D')
C.... OVRL21 ...........................................................
      N005=MQADDR((NMOACT+NMOEXT)*NSTATE*2,'D')
C
      LASTP1=MQMNXT()
      NEED2=LASTP1-N001
      CALL GETFM(NEED2)
C
      CALL MQGETW(LUNOUT,LPOUT ,
     $            NSTATE,IROT  ,
     $            INIACT,INIEXT,NMO   ,
     $            3     ,
     $            D(L001),D(L002),D(L003),D(L004),D(L005),D(L006),
     $            D(N001),D(N002),D(N003),D(N004),D(N005))
      CALL RETFM(NEED2)
      CALL RETFM(NEED1)
      CALL RETFM(NEED0)
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF (MASWRK) WRITE(LUNOUT,9100) STRSUB,TIME
      IF (MASWRK .AND. LPOUT.LT.0) WRITE(LUNOUT,9110) STRSUB,WALL1
      CALL FLSHBF(LUNOUT)
C***********************************************************************
C**** END **************************************************************
C****     **************************************************************
C**** CLOSE FILES ******************************************************
      CLOSE(UNIT=IDAF50,STATUS='KEEP')
      IF (DELSCR) THEN
         CALL SEQCLO(LUNFT0,'DELETE')
         CALL SEQCLO(LUNFTA,'DELETE')
         CALL SEQCLO(LUNFTC,'DELETE')
         CALL SEQCLO(LUNTWO,'DELETE')
         CALL SEQCLO(LUNFT1,'DELETE')
         CALL SEQCLO(LUNFT2,'DELETE')
         CALL SEQCLO(LUNPUN,'DELETE')
         CALL SEQCLO(LUNCNF,'DELETE')
         CALL SEQCLO(LUNOER,'KEEP')
         CALL SEQCLO(LUNJM1,'KEEP')
         CALL SEQCLO(LUNJM2,'KEEP')
         CALL SEQCLO(LUNJM3,'KEEP')
      ELSE
         CALL SEQCLO(LUNFT0,'KEEP')
         CALL SEQCLO(LUNFTA,'KEEP')
         CALL SEQCLO(LUNFTC,'KEEP')
         CALL SEQCLO(LUNTWO,'KEEP')
         CALL SEQCLO(LUNFT1,'KEEP')
         CALL SEQCLO(LUNFT2,'KEEP')
         CALL SEQCLO(LUNPUN,'KEEP')
         CALL SEQCLO(LUNCNF,'KEEP')
         CALL SEQCLO(LUNOER,'KEEP')
         CALL SEQCLO(LUNJM1,'KEEP')
         CALL SEQCLO(LUNJM2,'KEEP')
         CALL SEQCLO(LUNJM3,'KEEP')
      END IF
C**** CPU TIME *********************************************************
C     CALL TSECND(TIMLAS)
C     TIME=TIMLAS-TIMINI
C     WRITE(LUNOUT,'('' ***** TOTAL CPU TIME FOR MR2D ='',
C    *  F15.3,'' SEC.''/72(1H*)/72(1H*))') TIME
C**** RETURN ***********************************************************
C      RETURN
C
C END OF REFERENCE WEIGHT CALCULATION
C
 999  CONTINUE
      IF (IDMWTH.GT.0.AND.EXETYP.NE.CHECK) THEN
         CALL DDI_SYNC(2536)
         CALL DDI_DESTROY(DTWOEL)
      END IF
      RETURN
C
 9000 FORMAT(1X,10("*"),' ROUTINE ',A6,' ',46("*"))
 9100 FORMAT(' ***** CPU TIME FOR ',A6,' =',F15.3,' SEC.')
 9110 FORMAT(' **** WALL TIME FOR ',A6,' =',F15.3,' SEC.')
      END
C*MODULE MCQDPT  *DECK MQDSIZ
      INTEGER FUNCTION MQDSIZ(NSIZE,CTYPE)
      CHARACTER*1 CTYPE
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NVA,IODA(400)
C
C       CALCULATE HOW MANY 64 BIT FLOATING POINT WORDS ARE
C       NEEDED TO STORE THE REQUESTED DATA TYPE.
C
      IF(CTYPE.EQ.'D') THEN
         MQDSIZ=NSIZE
         RETURN
      END IF
      IF(CTYPE.EQ.'I') THEN
         MQDSIZ=(NSIZE-1)/NWDVAR+1
         RETURN
      END IF
      IF(CTYPE.EQ.'L') THEN
         MQDSIZ=(NSIZE-1)/NWDVAR+1
         RETURN
      END IF
      IF(CTYPE.EQ.'Q') THEN
         MQDSIZ=2*NSIZE
         RETURN
      END IF
C
      IF (MASWRK) WRITE(IW,9000) CTYPE
      CALL ABRT
      STOP
 9000 FORMAT(1X,'-MQDSIZ- CALLED WITH ILLEGAL DATA TYPE ',A)
C
C     SHOULD NOT USE CHARACTER*1 OR REAL*4 WITHIN GAMESS DYNAMIC MEMORY
C-MWS-      IF(CTYPE.EQ.'C') THEN
C-MWS-        MQDSIZ=(NSIZE+7)/8
C-MWS-      IF(CTYPE.EQ.'2') THEN
C-MWS-        MQDSIZ=(NSIZE+3)/4
      END
C*MODULE MCQDPT  *DECK MQFORB
      SUBROUTINE MQFORB(LUNOUT,LPOUT ,LUNERI,LUNFT0,LUNPUN,NCI   ,
     *                  NSTATE,NMO   ,NDOUB ,NMOACT,NGO   ,MAXERI,
     *                  MAXCSF,MAXDIA,ITYPE ,INOFMT,
     *                  EIGVEC,AVECOE,CMOOLD,CMONEW,EORB  ,HCORE ,
     *                  GEN1AO,GEN1  ,GFCKAO,GFOCK ,XWORK ,IX    ,
     *                  LABEL1,WORKGE,DIAHMA,DIAVAL,DIAVEC,DIAWRK,
     *                  IDIAWK,
     *                  MONWOD,MOSYM,CANAVE )
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL PACK2E,CANAVE,DIRTRF
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,IPOPLE
      COMMON /PCKLAB/ LABSIZ
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      COMMON /IOFILE/ IR,IW,IP,IS1,IPK,IDAF,NAV,IODA(400)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TRFOPT/ CUTTRF,NWDTRF,MPTRAN,ITRFAO,NOSYMT,DIRTRF
      DIMENSION EIGVEC(NCI,NSTATE), AVECOE(NSTATE)
      DIMENSION CMOOLD(NGO,NMO), CMONEW(NGO,NMO), EORB(NMO)
      DIMENSION HCORE(NMO,NMO)
      DIMENSION GEN1AO(NGO,NGO)
      DIMENSION GEN1  (NDOUB+1:NDOUB+NMOACT,NDOUB+1:NDOUB+NMOACT)
      DIMENSION GFCKAO(NGO,NGO), GFOCK(NMO,NMO)
      DIMENSION XWORK(NINTMX), IX(NINTMX)
      INTEGER   LABEL1(4,MAXCSF)
      DIMENSION WORKGE(MAXCSF)
      DIMENSION DIAHMA(MAXDIA*(MAXDIA+1)/2)
      DIMENSION DIAVAL(MAXDIA), DIAVEC(MAXDIA,MAXDIA), DIAWRK(MAXDIA*8)
      DIMENSION IDIAWK(MAXDIA)
      DIMENSION MONWOD(NMO), MOSYM(NMO)
      DIMENSION ISYM(8,8)
      COMMON /MQFT02/NWFT0,NWFT0P
      COMMON/MQSYLB/ISYLAB(2,8,4)
      DATA ISYM /1,2,3,4,5,6,7,8,
     *           2,1,4,3,6,5,8,7,
     *           3,4,1,2,7,8,5,6,
     *           4,3,2,1,8,7,6,5,
     *           5,6,7,8,1,2,3,4,
     *           6,5,8,7,2,1,4,3,
     *           7,8,5,6,3,4,1,2,
     *           8,7,6,5,4,3,2,1/
C
C     DEFINITION:
C       CLOSED SHELL FOCK MATRIX
C         F(P,Q)=H(P,Q)+SUM(OVER R,S)D(R,S)[(PQ/RS)-1/2(PR/SQ)]
C
C     ITYPE=1: MAKE CANONICAL FOCK ORBITALS
C              (F'=D(F)D+A(F)A+E(F)E IS DIAGONAL.)
C     ITYPE=2: MAKE NATURAL ORBITALS
C              (F'=D(F)D+E(F)E IS DIAGONAL.)
C
C**** THRESHOLD ZERO ***************************************************
      THRZRO=1.0D-15
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQFORB ***'')')
        CALL MQDEBG(LUNOUT,
     *  'LUNERI','LUNFT0','LUNPUN','   NCI','NSTATE','   NMO',' NDOUB',
     *   LUNERI , LUNFT0 , LUNPUN ,    NCI , NSTATE ,    NMO ,  NDOUB )
        CALL MQDEBG(LUNOUT,
     *  'NMOACT','   NGO','MAXERI','MAXCSF','MAXDIA',' ITYPE','INOFMT',
     *   NMOACT ,    NGO , MAXERI , MAXCSF , MAXDIA ,  ITYPE , INOFMT )
        WRITE(LUNOUT,*) NWFT0,NWFT0P
      END IF
C**** NORMALIZE AVECOE *************************************************
      S=0.0D+00
      TWEIN=2.0D+00
      DO I=1,NSTATE
        S=S+AVECOE(I)
      END DO
      IF(CANAVE) THEN
C       TOTAL WEIGHT IS GOT BY NORMALISING TO SUM(I) {IROOTS(I)}
        S=MXRT
C       FOR THE CORE, DENSITY IS 2, SUMMED OVER ALL NUMCI RUNS.
        TWEIN=TWEIN/NUMCI
      ENDIF
      IF(ABS(S).GT.THRZRO) THEN
        S=1.0D+00/S
        DO I=1,NSTATE
          AVECOE(I)=AVECOE(I)*S
        END DO
      END IF
C**** SET CONSTANTS ****************************************************
      INIMO=1
      LASMO=NMO
      INIDOC=1
      LASDOC=NDOUB
      INIACT=NDOUB       +1
      LASACT=NDOUB+NMOACT
C     INIEXT=NDOUB+NMOACT+1
C     LASEXT=NMO
C     NMOINT=NDOUB+NMOACT
C     NMOEXT=NMO-NDOUB-NMOACT
C***********************************************************************
C****                  *************************************************
C**** PREPARE MATRICES *************************************************
C****                  *************************************************
C**** CLEAR ARRAYS *****************************************************
      DO J=INIACT,LASACT
        DO I=INIACT,LASACT
          GEN1(I,J)=0.0D+00
        END DO
      END DO
      DO J=1,NGO
        DO I=1,NGO
          GEN1AO(I,J)=0.0D+00
          GFCKAO(I,J)=0.0D+00
        END DO
      END DO
      DO J=INIMO,LASMO
        DO I=1,NGO
          CMONEW(I,J)=0.0D+00
        END DO
      END DO
C**** GET AVERAGED ONE-PARTICLE DENSITY MATRIX *************************
      CALL SEQREW(LUNFT0)
      DO INWFT0=1,NWFT0
C  100 CONTINUE
        READ(LUNFT0) KCONT,NWORD
        CALL MQGENR(LUNFT0,4*NWORD,NWORD,LABEL1,WORKGE)
        DO M=1,NWORD
          M1 =LABEL1(1,M)
          M2 =LABEL1(2,M)
          LP =LABEL1(3,M)
          LQ =LABEL1(4,M)
          VAL=WORKGE(M)
          DO I=1,NSTATE
            GEN1(LP,LQ)=GEN1(LP,LQ)
     *      +AVECOE(I)*EIGVEC(M1,I)*VAL*EIGVEC(M2,I)
          END DO
        END DO
C      IF(KCONT.NE.0) GO TO 100
      END DO
      IF (GOPARR) CALL DDI_GSUMF(2537,GEN1,NMOACT*NMOACT)
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LE.-10 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQFORB ***''/
     *    '' *** ONE PARTICLE DENSITY MATRIX ***'')')
        WRITE(LUNOUT,*) 'NWFT0,KCONT,NWORD=',NWFT0,KCONT,NWORD
        CALL MQWMAG(LUNOUT,GEN1,NMOACT,NMOACT,NMOACT,'SYMM')
      END IF
C**** GET AVERAGED ONE-PARTICLE DENSITY MATRIX ON AO BASE **************
      DO K=INIDOC,LASDOC
        DO J=1,NGO
C         S=CMOOLD(J,K)*2.0D+00
          S=CMOOLD(J,K)*TWEIN
          DO I=1,NGO
            GEN1AO(I,J)=GEN1AO(I,J)+CMOOLD(I,K)*S
          END DO
        END DO
      END DO
      DO L=INIACT,LASACT
        DO K=INIACT,LASACT
          DO J=1,NGO
            S=CMOOLD(J,L)*GEN1(K,L)
            DO I=1,NGO
              GEN1AO(I,J)=GEN1AO(I,J)+CMOOLD(I,K)*S
            END DO
          END DO
        END DO
      END DO
      IF(CANAVE.AND.NOCC.EQ.0) THEN
C       THAT IS, READ DENSITY FROM PREV MULTIPLICITIES IF
C       NOCC=0 (DENSITY GENERATING RUN)
C       WRITE(6,*) 'WWW: NORMALISING BY',MXRT
C       CALL DSCAL(NGO*NGO,1.0D+00/MXRT,GEN1AO,1)
C       THAT IS, EACH MULTIPLICITY GETS FIXED EQUAL WEIGHT.
C       ADD DENSITY FROM PREVIOUS MULTPLICITIES
        IF(ICI.NE.1) THEN
          CALL DAREAD(IDAF,IODA,GFCKAO,NGO*NGO,393,0)
          CALL VADD(GEN1AO,1,GFCKAO,1,GEN1AO,1,NGO*NGO)
          CALL VCLR(GFCKAO,1,NGO*NGO)
        ENDIF
        CALL DAWRIT(IDAF,IODA,GEN1AO,NGO*NGO,393,0)
      ENDIF
      IF(CANAVE.AND.NOCC.NE.0) THEN
C       (REAL MCQDPT), READ DENSITY AVERAGED OVER ALL MULTIPLICITIES
          CALL DAREAD(IDAF,IODA,GEN1AO,NGO*NGO,393,0)
      ENDIF
C
C**** GET CLOSED SHELL FOCK MATRIX ON AO BASE **************************
C
      NIX = 0
      CALL SEQREW(LUNERI)
 200  CONTINUE
        CALL PREAD(LUNERI,XWORK,IX,NX,NINTMX)
        NIX=NIX+1
        IF(NX.EQ.0) GO TO 220
        MX=IABS(NX)
        IF(MX.GT.NINTMX) CALL ABRT
C
        IF(GOPARR.AND.ITRFAO.EQ.1.AND.MOD(NIX-1,NPROC).NE.ME) GO TO 210
        DO IERI=1,MX
          VERI=XWORK(IERI)
C
                       NPACK = IERI
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
                       IA = IPACK
                       IB = JPACK
                       IC = KPACK
                       ID = LPACK
C
          CDP=GEN1AO(IC,ID)*VERI*2.0D+00
          ABP=GEN1AO(IA,IB)*VERI*2.0D+00
          BDM=-0.5D+00*GEN1AO(IB,ID)*VERI
          BCM=-0.5D+00*GEN1AO(IB,IC)*VERI
          ADM=-0.5D+00*GEN1AO(IA,ID)*VERI
          ACM=-0.5D+00*GEN1AO(IA,IC)*VERI
C
          GFCKAO(IA,IB)=GFCKAO(IA,IB)+CDP
          GFCKAO(IB,IA)=GFCKAO(IB,IA)+CDP
          GFCKAO(IC,ID)=GFCKAO(IC,ID)+ABP
          GFCKAO(ID,IC)=GFCKAO(ID,IC)+ABP
C
          GFCKAO(IA,IC)=GFCKAO(IA,IC)+BDM
          GFCKAO(IA,ID)=GFCKAO(IA,ID)+BCM
          GFCKAO(IB,IC)=GFCKAO(IB,IC)+ADM
          GFCKAO(IB,ID)=GFCKAO(IB,ID)+ACM
          GFCKAO(IC,IA)=GFCKAO(IC,IA)+BDM
          GFCKAO(IC,IB)=GFCKAO(IC,IB)+ADM
          GFCKAO(ID,IA)=GFCKAO(ID,IA)+BCM
          GFCKAO(ID,IB)=GFCKAO(ID,IB)+ACM
        END DO
 210    CONTINUE
C
      IF(NX.GT.0) GO TO 200
 220  CONTINUE
      IF (GOPARR) CALL DDI_GSUMF(2505,GFCKAO,NGO*NGO)
C**** GET CLOSED SHELL FOCK MATRIX ON MO BASE **************************
      CALL MQHTRN(LUNOUT,LPOUT ,NGO   ,NMO,
     *            CMOOLD,GFCKAO,GFOCK )
      DO J=1,NMO
        DO I=1,NMO
          K=ISYM(MOSYM(I),MOSYM(J))
          IF(K.NE.1) GFOCK(I,J)=0.0D+00
        END DO
      END DO
      DO J=INIMO,LASMO
        DO I=INIMO,LASMO
          GFOCK(I,J)=GFOCK(I,J)+HCORE(I,J)
        END DO
      END DO
C.... DEBUG OUTPUT .....................................................
      IF(LPOUT.LE.-10 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** FOCK MATRIX ***'')')
        CALL MQWMAG(LUNOUT,GFOCK,NMO,NMO,NMO,'TRIG')
      END IF
C***********************************************************************
C****                             **************************************
C**** GET CANONICAL FOCK ORBITALS **************************************
C****                             **************************************
C**** DIAGONALIZE FOCK MATRIX (DOC-DOC BLOCK) **************************
      DO IS=1,8
        MOCINI=ISYLAB(1,IS,1)
        MOCLAS=ISYLAB(2,IS,1)
        MODINI=ISYLAB(1,IS,2)
        MODLAS=ISYLAB(2,IS,2)
        N=0
        DO I=MOCINI,MOCLAS
          N=N+1
          MONWOD(N)=I
        END DO
        DO I=MODINI,MODLAS
          N=N+1
          MONWOD(N)=I
        END DO
        NDIA=N
        IF(NDIA.GT.0) THEN
          N=0
          DO I=1,NDIA
            DO J=1,I
              N=N+1
              DIAHMA(N)=GFOCK(MONWOD(I),MONWOD(J))
            END DO
          END DO
          NTRI=NDIA*(NDIA+1)/2
C         CALL MQDIAG(NDIA  ,NTRI,MAXDIA,ICON,
C    *                DIAHMA,DIAVAL,DIAVEC,DIAWRK)
          CALL EVVRSP(LUNOUT,NDIA  ,NDIA  ,NTRI  ,MAXDIA,DIAHMA,
     *                DIAWRK,IDIAWK,DIAVAL,DIAVEC,0     ,ICON  )
          DO K=1,NDIA
            KOLD=MONWOD(K)
            EORB(KOLD)=DIAVAL(K)
            DO J=1,NDIA
              JOLD=MONWOD(J)
              DO I=1     ,NGO
                CMONEW(I,JOLD)=CMONEW(I,JOLD)+DIAVEC(K,J)*CMOOLD(I,KOLD)
              END DO
            END DO
          END DO
        END IF
      END DO
C**** DIAGONALIZE FOCK OR DENSITY MATRIX (ACT-ACT BLOCK) ***************
C.... FOCK MATRIX ......................................................
      IF(ITYPE.EQ.1) THEN
        DO IS=1,8
          MOAINI=ISYLAB(1,IS,3)
          MOALAS=ISYLAB(2,IS,3)
          N=0
          DO I=MOAINI,MOALAS
            N=N+1
            MONWOD(N)=I
          END DO
          NDIA=N
          IF(NDIA.GT.0) THEN
            N=0
            DO I=1,NDIA
              DO J=1,I
                N=N+1
                DIAHMA(N)=GFOCK(MONWOD(I),MONWOD(J))
              END DO
            END DO
            NTRI=NDIA*(NDIA+1)/2
C           CALL MQDIAG(NDIA  ,NTRI,MAXDIA,ICON,
C    *                  DIAHMA,DIAVAL,DIAVEC,DIAWRK)
            CALL EVVRSP(LUNOUT,NDIA  ,NDIA  ,NTRI  ,MAXDIA,DIAHMA,
     *                  DIAWRK,IDIAWK,DIAVAL,DIAVEC,0     ,ICON  )
            DO K=1,NDIA
              KOLD=MONWOD(K)
              EORB(KOLD)=DIAVAL(K)
              DO J=1,NDIA
                JOLD=MONWOD(J)
                DO I=1,NGO
                  CMONEW(I,JOLD)=CMONEW(I,JOLD)
     *              +DIAVEC(K,J)*CMOOLD(I,KOLD)
                END DO
              END DO
            END DO
          END IF
        END DO
C.... DENSITY MATRIX ...................................................
      ELSE IF(ITYPE.EQ.2) THEN
        DO IS=1,8
          MOAINI=ISYLAB(1,IS,3)
          MOALAS=ISYLAB(2,IS,3)
          N=0
          DO I=MOAINI,MOALAS
            N=N+1
            MONWOD(N)=I
          END DO
          NDIA=N
          IF(NDIA.GT.0) THEN
            N=0
            DO I=1,NDIA
              DO J=1,I
                N=N+1
                DIAHMA(N)=GEN1(MONWOD(I),MONWOD(J))
              END DO
            END DO
            NTRI=NDIA*(NDIA+1)/2
C           CALL MQDIAG(NDIA  ,NTRI,MAXDIA,ICON,
C    *                  DIAHMA,DIAVAL,DIAVEC,DIAWRK)
            CALL EVVRSP(LUNOUT,NDIA  ,NDIA  ,NTRI  ,MAXDIA,DIAHMA,
     *                  DIAWRK,IDIAWK,DIAVAL,DIAVEC,0     ,ICON  )
            DO K=1,NDIA
              KOLD=MONWOD(K)
              EORB(KOLD)=0.0D+00
              DO J=1,NDIA
                JOLD=MONWOD(J)
                DO I=1,NGO
                  CMONEW(I,JOLD)=CMONEW(I,JOLD)
     *              +DIAVEC(K,J)*CMOOLD(I,KOLD)
                END DO
              END DO
            END DO
          END IF
        END DO
      END IF
C**** DIAGONALIZE FOCK MATRIX (EXT-EXT BLOCK) **************************
      DO IS=1,8
        MOEINI=ISYLAB(1,IS,4)
        MOELAS=ISYLAB(2,IS,4)
        N=0
        DO I=MOEINI,MOELAS
          N=N+1
          MONWOD(N)=I
        END DO
        NDIA=N
        IF(NDIA.GT.0) THEN
          N=0
          DO I=1,NDIA
            DO J=1,I
              N=N+1
              DIAHMA(N)=GFOCK(MONWOD(I),MONWOD(J))
            END DO
          END DO
          NTRI=NDIA*(NDIA+1)/2
C         CALL MQDIAG(NDIA  ,NTRI,MAXDIA,ICON,
C    *                DIAHMA,DIAVAL,DIAVEC,DIAWRK)
          CALL EVVRSP(LUNOUT,NDIA  ,NDIA  ,NTRI  ,MAXDIA,DIAHMA,
     *                DIAWRK,IDIAWK,DIAVAL,DIAVEC,0     ,ICON  )
          DO K=1,NDIA
            KOLD=MONWOD(K)
            EORB(KOLD)=DIAVAL(K)
            DO J=1,NDIA
              JOLD=MONWOD(J)
              DO I=1,NGO
                CMONEW(I,JOLD)=CMONEW(I,JOLD)+DIAVEC(K,J)*CMOOLD(I,KOLD)
              END DO
            END DO
          END DO
        END IF
      END DO
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** CANONICAL FOCK ORBITAL ***'')')
        CALL MQMOWR(LUNOUT,NGO,NMO,EORB,CMONEW)
        WRITE(LUNOUT,'('' *** ORBITAL ENERGIES ***'')')
        CALL MQWMAG(LUNOUT,EORB,1,1,NMO,'    ')
      END IF
C**** WRITE RESULTS ****************************************************
      IF(.NOT.CANAVE.OR.NOCC.NE.0) THEN
        IF (MASWRK) THEN
           WRITE(LUNOUT,9110)
           CALL MQWMAG(LUNOUT,GEN1,NMOACT,NMOACT,NMOACT,'SYMM')
        END IF
 9110 FORMAT(/1X,'*** ONE PARTICLE DENSITY MATRIX OVER ACTIVE MO-S ***')
C**** PUNCH OUT ORBITALS ***********************************************
      CALL SEQREW(LUNPUN)
      WRITE(LUNPUN,'('' *** CANONICAL FOCK ORBITAL ***''/'' $VEC'')')
      CALL MQPUSQ(LUNPUN,NGO,NMO,INOFMT,CMONEW)
      WRITE(LUNPUN,'('' $END'')')
      ENDIF
      IF(CANAVE.AND.ICI.EQ.NUMCI.AND.NOCC.EQ.0) THEN
         IF (MASWRK) THEN
         WRITE(IP,*) 'CANONICAL MOS AVERAGED OVER ALL MULTIPLICITIES'
         WRITE(IP,*) '$VEC1'
         CALL PUSQL(CMONEW,NMO,NGO,NGO)
         WRITE(IP,*) '$END'
         END IF
      ENDIF
C**** RETURN ***********************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQGENR
      SUBROUTINE MQGENR(LUNI,LENLAB,LENWOR,LABEL,WORK)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER   LABEL(LENLAB)
      DIMENSION WORK (LENWOR)
#if defined(SPEC_CPU_ILP64)
      PARAMETER (MASK32=2**32 - 1)
#endif
C
C LPAKBT : NUMBER OF BITS FOR PACKING AO INTEGRAL LABEL
C            =0  NO PACK
C            =8   8 BITS PACK
C            =16 16 BITS PACK
C            =32 32 BITS PACK
      READ(LUNI) LPAKBT
C THIS READ AFFECT NOT ONLY MQGENW BUT ALSO MQOCF1 AND MQOCF2.
C SHOULD BE TREATED CAREFULLY.
C      LPAKBT=16
C      IF (LENLAB.LT.4) LPAKBT=0
C
C
      IF (LPAKBT.EQ.8) THEN
C
#if !defined(SPEC_CPU_ILP64)
         NLPACK=(LENLAB-1)/4+1
#endif
#if defined(SPEC_CPU_ILP64)
         NLPACK=(LENLAB-1)/8+1
#endif
         CALL MQGNR2(LUNI,NLPACK,LENWOR,LABEL(LENLAB-NLPACK+1),WORK)
C
         DO I=1,NLPACK
            NPACK = LENLAB-NLPACK+I
            LABPK = LABEL(NPACK)
C
C 255 = 2**8 - 1
#if !defined(SPEC_CPU_ILP64)
            I1PACK =       ISHFT( LABPK, -24 )
            I2PACK = IAND( ISHFT( LABPK, -16 ), 255 )
            I3PACK = IAND( ISHFT( LABPK, - 8 ), 255 )
            I4PACK = IAND(        LABPK       , 255 )
#endif
#if defined(SPEC_CPU_ILP64)
            I1PACK =       ISHFT( LABPK, -56 )
            I2PACK = IAND( ISHFT( LABPK, -48 ), 255 )
            I3PACK = IAND( ISHFT( LABPK, -40 ), 255 )
            I4PACK = IAND( ISHFT( LABPK, -32 ), 255 )
            I5PACK = IAND( ISHFT( LABPK, -24 ), 255 )
            I6PACK = IAND( ISHFT( LABPK, -16 ), 255 )
            I7PACK = IAND( ISHFT( LABPK, - 8 ), 255 )
            I8PACK = IAND(        LABPK       , 255 )
#endif
C
#if !defined(SPEC_CPU_ILP64)
            LABEL(    (I-1)*4+1        ) = I1PACK
            LABEL(MIN((I-1)*4+2,LENLAB)) = I2PACK
            LABEL(MIN((I-1)*4+3,LENLAB)) = I3PACK
            LABEL(MIN((I-1)*4+4,LENLAB)) = I4PACK
#endif
#if defined(SPEC_CPU_ILP64)
            LABEL(    (I-1)*8+1        ) = I1PACK
            LABEL(MIN((I-1)*8+2,LENLAB)) = I2PACK
            LABEL(MIN((I-1)*8+3,LENLAB)) = I3PACK
            LABEL(MIN((I-1)*8+4,LENLAB)) = I4PACK
            LABEL(MIN((I-1)*8+5,LENLAB)) = I5PACK
            LABEL(MIN((I-1)*8+6,LENLAB)) = I6PACK
            LABEL(MIN((I-1)*8+7,LENLAB)) = I7PACK
            LABEL(MIN((I-1)*8+8,LENLAB)) = I8PACK
#endif
         END DO
C
      ELSE IF (LPAKBT.EQ.16) THEN
C
#if !defined(SPEC_CPU_ILP64)
         NLPACK=(LENLAB-1)/2+1
#endif
#if defined(SPEC_CPU_ILP64)
         NLPACK=(LENLAB-1)/4+1
#endif
         CALL MQGNR2(LUNI,NLPACK,LENWOR,LABEL(LENLAB-NLPACK+1),WORK)
C
         DO I=1,NLPACK
            NPACK = LENLAB-NLPACK+I
            LABPK = LABEL(NPACK)
C
C 65535 = 2**16 - 1
#if !defined(SPEC_CPU_ILP64)
            IPACK = ISHFT( LABPK, -16 )
            JPACK = IAND(  LABPK, 65535 )
#endif
#if defined(SPEC_CPU_ILP64)
            IPACK = ISHFT( LABPK, -48 )
            JPACK = IAND( ISHFT( LABPK, -32 ), 65535 )
            KPACK = IAND( ISHFT( LABPK, -16 ), 65535 )
            LPACK = IAND( LABPK, 65535 )
#endif
C
#if !defined(SPEC_CPU_ILP64)
            LABEL(    (I-1)*2+1        ) = IPACK
            LABEL(MIN((I-1)*2+2,LENLAB)) = JPACK
#endif
#if defined(SPEC_CPU_ILP64)
            LABEL(    (I-1)*4+1        ) = IPACK
            LABEL(MIN((I-1)*4+2,LENLAB)) = JPACK
            LABEL(MIN((I-1)*4+3,LENLAB)) = KPACK
            LABEL(MIN((I-1)*4+4,LENLAB)) = LPACK
#endif
         END DO
C
#if defined(SPEC_CPU_ILP64)
      ELSE IF (LPAKBT.EQ.32) THEN
#endif
C
C 4294967295 = 2**32 - 1
#if defined(SPEC_CPU_ILP64)
         NLPACK=(LENLAB-1)/2+1
         CALL MQGNR2(LUNI,NLPACK,LENWOR,LABEL(LENLAB-NLPACK+1),WORK)
#endif
C
#if defined(SPEC_CPU_ILP64)
         DO I=1,NLPACK
            NPACK = LENLAB-NLPACK+I
            LABPK = LABEL(NPACK)
#endif
C
#if defined(SPEC_CPU_ILP64)
            IPACK = ISHFT( LABPK, -32 )
            JPACK = IAND(  LABPK, MASK32 )
#endif
C
#if defined(SPEC_CPU_ILP64)
            LABEL(    (I-1)*2+1        ) = IPACK
            LABEL(MIN((I-1)*2+2,LENLAB)) = JPACK
          END DO
#endif
C
      ELSE
C NO PACK
C
         CALL MQGNR2(LUNI,LENLAB,LENWOR,LABEL,WORK)
C
      END IF
C
      RETURN
      END
C*MODULE MCQDPT  *DECK MQGENW
      SUBROUTINE MQGENW(LUNO,LENLAB,LENWOR,LABEL,WORK)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER   LABEL(LENLAB)
      DIMENSION WORK (LENWOR)
C
C LPAKBT : NUMBER OF BITS FOR PACKING AO INTEGRAL LABEL
C            =0  NO PACK
C            =8   8 BITS PACK
C            =16 16 BITS PACK
C            =32 32 BITS PACK
C
      MAXLAB=1
      DO I=1,LENLAB
         MAXLAB=MAX(LABEL(I),MAXLAB)
      END DO
C
#if !defined(SPEC_CPU_ILP64)
      IF (LENLAB.GE.4.AND.MAXLAB.LT.2**8) THEN
#endif
#if defined(SPEC_CPU_ILP64)
      IF (LENLAB.GE.8.AND.MAXLAB.LT.2**8) THEN
#endif
         LPAKBT= 8
#if !defined(SPEC_CPU_ILP64)
      ELSE IF (LENLAB.GE.2.AND.MAXLAB.LT.2**16) THEN
#endif
#if defined(SPEC_CPU_ILP64)
      ELSE IF (LENLAB.GE.4.AND.MAXLAB.LT.2**16) THEN
#endif
         LPAKBT=16
#if defined(SPEC_CPU_ILP64)
      ELSE IF (LENLAB.GE.2.AND.MAXLAB.LT.2**32) THEN
         LPAKBT=32
#endif
      ELSE
         LPAKBT=0
      END IF
C
      WRITE(LUNO) LPAKBT
C THIS WRITE AFFECT NOT ONLY MQGENR BUT ALSO MQOCF1 AND MQOCF2.
C SHOULD BE TREATED CAREFULLY.
C
C
      IF (LPAKBT.EQ.8) THEN
C
#if !defined(SPEC_CPU_ILP64)
         NLPACK=(LENLAB-1)/4+1
#endif
#if defined(SPEC_CPU_ILP64)
         NLPACK=(LENLAB-1)/8+1
#endif
C
         DO I=1,NLPACK
#if !defined(SPEC_CPU_ILP64)
            I1PACK=LABEL(    (I-1)*4+1)
            I2PACK=LABEL(MIN((I-1)*4+2,LENLAB))
            I3PACK=LABEL(MIN((I-1)*4+3,LENLAB))
            I4PACK=LABEL(MIN((I-1)*4+4,LENLAB))
#endif
#if defined(SPEC_CPU_ILP64)
            I1PACK=LABEL(    (I-1)*8+1)
            I2PACK=LABEL(MIN((I-1)*8+2,LENLAB))
            I3PACK=LABEL(MIN((I-1)*8+3,LENLAB))
            I4PACK=LABEL(MIN((I-1)*8+4,LENLAB))
            I5PACK=LABEL(MIN((I-1)*8+5,LENLAB))
            I6PACK=LABEL(MIN((I-1)*8+6,LENLAB))
            I7PACK=LABEL(MIN((I-1)*8+7,LENLAB))
            I8PACK=LABEL(MIN((I-1)*8+8,LENLAB))
#endif
C
#if !defined(SPEC_CPU_ILP64)
            LABEL(I) = ISHFT( I1PACK, 24 ) + ISHFT( I2PACK, 16 ) +
     *                 ISHFT( I3PACK,  8 ) +        I4PACK
#endif
#if defined(SPEC_CPU_ILP64)
            LABEL(I) = ISHFT( I1PACK, 56 ) + ISHFT( I2PACK, 48 ) +
     *                 ISHFT( I3PACK, 40 ) + ISHFT( I4PACK, 32 ) +
     *                 ISHFT( I5PACK, 24 ) + ISHFT( I6PACK, 16 ) +
     *                 ISHFT( I7PACK,  8 ) +        I8PACK
#endif
         END DO
C
      ELSE IF (LPAKBT.EQ.16) THEN
C
#if !defined(SPEC_CPU_ILP64)
         NLPACK=(LENLAB-1)/2+1
#endif
#if defined(SPEC_CPU_ILP64)
         NLPACK=(LENLAB-1)/4+1
#endif
C
         DO I=1,NLPACK
#if !defined(SPEC_CPU_ILP64)
            IPACK=LABEL(    (I-1)*2+1)
            JPACK=LABEL(MIN((I-1)*2+2,LENLAB))
#endif
#if defined(SPEC_CPU_ILP64)
            IPACK=LABEL(    (I-1)*4+1)
            JPACK=LABEL(MIN((I-1)*4+2,LENLAB))
            KPACK=LABEL(MIN((I-1)*4+3,LENLAB))
            LPACK=LABEL(MIN((I-1)*4+4,LENLAB))
#endif
C
#if !defined(SPEC_CPU_ILP64)
            LABEL(I) = ISHFT( IPACK, 16 ) + JPACK
#endif
#if defined(SPEC_CPU_ILP64)
            LABEL(I) = ISHFT( IPACK, 48 ) + ISHFT( JPACK, 32 ) +
     *                 ISHFT( KPACK, 16 ) + LPACK
#endif
         END DO
C
#if defined(SPEC_CPU_ILP64)
       ELSE IF (LPAKBT.EQ.32) THEN
#endif
C
#if defined(SPEC_CPU_ILP64)
         NLPACK=(LENLAB-1)/2+1
#endif
C
#if defined(SPEC_CPU_ILP64)
          DO I=1,NLPACK
            IPACK=LABEL(    (I-1)*2+1)
            JPACK=LABEL(MIN((I-1)*2+2,LENLAB))
#endif
C
#if defined(SPEC_CPU_ILP64)
            LABEL(I) = ISHFT( IPACK, 32 ) + JPACK
          END DO
#endif
C
      ELSE
C NO PACK
C
         NLPACK=LENLAB
C
      END IF
C
      CALL MQGNW2(LUNO,NLPACK,LENWOR,LABEL,WORK)
C
C
      RETURN
      END
C*MODULE MCQDPT  *DECK MQGETE
      SUBROUTINE MQGETE(LUNOUT,LPOUT ,
     *                  NCSF  ,NSTATE,NWRKM1,NWRKM2,IROT  ,
     *                  HEFF  ,CASVEC,CASENE,EIGVAL,EIGVEC,
     *                  DIAHMA,DIAVAL,DIAVEC,DIAWRK,LORDER,
     *                  HEFFVC)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***********************************************************************
C****                         ******************************************
C****   ARRAYS AND CONSTANT   ******************************************
C****                         ******************************************
      DIMENSION HEFF (NSTATE,NSTATE,0:3)
      DIMENSION CASVEC(NCSF,NSTATE), CASENE(NSTATE)
      DIMENSION EIGVAL(NSTATE), EIGVEC(NCSF,NSTATE)
      DIMENSION DIAHMA(NSTATE*(NSTATE+1)/2), DIAVAL(NSTATE)
      DIMENSION DIAVEC(NSTATE,NSTATE), DIAWRK(NSTATE*8), LORDER(NSTATE)
      DIMENSION HEFFVC(NSTATE,NSTATE)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      COMMON /ENRGMP/ EMP2,EMP3,EMP4
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
      IF (LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQGETE ***'')')
        CALL MQDEBG(LUNOUT,
     *  '  NCSF','NSTATE','NWRKM1','NWRKM2','  IROT','     -','     -',
     *     NCSF , NSTATE , NWRKM1 , NWRKM2 ,   IROT ,      0 ,      0 )
      END IF
C
      DO J=1,NSTATE
        DO I=1,NCSF
          EIGVEC(I,J)=0.0D+00
        END DO
      END DO
      IF(EXETYP.EQ.CHECK) THEN
         CALL VCLR(HEFF(1,1,0),1,NSTATE*NSTATE*4)
         CALL VCLR(CASENE,1,NSTATE)
         CALL VCLR(CASVEC,1,NSTATE*NCSF)
      END IF
C***********************************************************************
C****                                      *****************************
C****   GET EIGENVALUES AND EIGENVECTORS   *****************************
C****                                      *****************************
C
C     HEFF (INPUT)
C       HEFF(*,*,0) 0TH      ORDER
C       HEFF(*,*,1) (0+1)-TH ORDER = CASSCF ENERGIES
C       HEFF(*,*,2) 2ND      ORDER IN RS-PT
C
      DO J=1,NSTATE
        DO I=J,NSTATE
          S=0.5D+00*(HEFF(I,J,2)+HEFF(J,I,2))+HEFF(I,J,1)
          T=0.5D+00*(HEFF(I,J,2)+HEFF(J,I,2)+HEFF(I,J,3)+HEFF(J,I,3))
     *      +HEFF(I,J,1)
          HEFF(I,J,2)=S
          HEFF(J,I,2)=S
          HEFF(I,J,3)=T
          HEFF(J,I,3)=T
        END DO
      END DO
C
C     HEFF (OUTPUT)
C       HEFF(*,*,0) 0TH        ORDER
C       HEFF(*,*,1) (0+1)-TH   ORDER = CASSCF ENERGIES
C       HEFF(*,*,2) (0+1+2)-TH ORDER IN KCH-PT
C
C.... DEBUG OUTPUT .....................................................
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,9100)
        CALL MQWMAG(LUNOUT,HEFF(1,1,1),NSTATE,NSTATE,NSTATE,'TRIG')
        WRITE(LUNOUT,'('' *** EFFECTIVE HAMILTONIAN (0-2) ***'')')
        CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'TRIG')
        IF(IROT.EQ.2) THEN
          WRITE(LUNOUT,
     *      '('' *** EFFECTIVE HAMILTONIAN (0-2) (ROT) ***'')')
          CALL MQWMAG(LUNOUT,HEFF(1,1,3),NSTATE,NSTATE,NSTATE,'TRIG')
        END IF
      END IF
 9100 FORMAT(/1X,'*** EFFECTIVE HAMILTONIAN (FIRST ORDER) ***')
C
C**** DIAGONALIZATION OF EFFECTIVE HAMILTONIAN *************************
      N=0
      DO I=1,NSTATE
        DO J=1,I
          N=N+1
          DIAHMA(N)=HEFF(I,J,2)
        END DO
      END DO
      NSTTRI=(NSTATE*NSTATE+NSTATE)/2
      CALL EVVRSP(LUNOUT,NSTATE,NSTATE,NSTTRI,NSTATE,DIAHMA,
     *            DIAWRK,LORDER,DIAVAL,DIAVEC,0     ,ICON  )
C.... CHECK DIAGONALIZATION ............................................
      IF(ICON.NE.0) THEN
        IF (MASWRK)
     *   WRITE(LUNOUT,'('' *** ERROR STOP IN SUB.MQGETE ***''/
     *    '' THE DIAGONALIZATION HAS FAILED.''/
     *    '' ICON ='',I3)') ICON
        CALL ABRT
      END IF
C.... REORDER THE SOLUTIONS ............................................
      DO I=1,NSTATE
        LORDER(I)=I
      END DO
      NM1=NSTATE-1
      DO I=1,NM1
        IP1=I+1
        DO J=IP1,NSTATE
          IF(DIAVAL(LORDER(I)).GT.DIAVAL(LORDER(J))) THEN
            K        =LORDER(I)
            LORDER(I)=LORDER(J)
            LORDER(J)=K
          END IF
        END DO
      END DO
C.... SAVE THE SOLUTIONS ...............................................
      DO I=1,NSTATE
        J=LORDER(I)
        EIGVAL(I)=DIAVAL(J)
        DO L=1,NSTATE
          DO K=1,NCSF
            EIGVEC(K,I)=EIGVEC(K,I)+CASVEC(K,L)*DIAVEC(L,J)
          END DO
        END DO
      END DO
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C**** DIAGONALIZATION OF EFFECTIVE HAMILTONIAN (ROT) *******************
      IF(IROT.EQ.2) THEN
        N=0
        DO I=1,NSTATE
          DO J=1,I
            N=N+1
            DIAHMA(N)=HEFF(I,J,3)
          END DO
        END DO
        NSTTRI=NSTATE*(NSTATE+1)/2
C       CALL MQDIAG(NSTATE,NSTTRI,NSTATE,ICON,
C    *              DIAHMA,DIAVAL,DIAVEC,DIAWRK)
        CALL EVVRSP(LUNOUT,NSTATE,NSTATE,NSTTRI,NSTATE,DIAHMA,
     *              DIAWRK,LORDER,DIAVAL,DIAVEC,0     ,ICON  )
C.... CHECK DIAGONALIZATION ............................................
        IF(ICON.NE.0) THEN
          IF (MASWRK)
     *     WRITE(LUNOUT,'('' *** ERROR STOP IN SUB.MQGETE ***''/
     *      '' THE DIAGONALIZATION (FOR ROT) HAS FAILED.''/
     *      '' ICON ='',I3)') ICON
          CALL ABRT
        END IF
C.... REORDER THE SOLUTIONS ............................................
        DO I=1,NSTATE
          LORDER(I)=I
        END DO
        NM1=NSTATE-1
        DO I=1,NM1
          IP1=I+1
          DO J=IP1,NSTATE
            IF(DIAVAL(LORDER(I)).GT.DIAVAL(LORDER(J))) THEN
              K        =LORDER(I)
              LORDER(I)=LORDER(J)
              LORDER(J)=K
            END IF
          END DO
        END DO
      END IF
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C**** WRITE RESULTS ****************************************************
C
      IF (MASWRK) THEN
         IF(NSTATE.EQ.1) THEN
            WRITE(LUNOUT,9200)
         ELSE
            WRITE(LUNOUT,9210)
         END IF
         WRITE(LUNOUT,9220)
         CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'TRIG')
         WRITE(LUNOUT,9230)
         DO I=1,NSTATE
           E=HEFF(I,I,0)
           WRITE(LUNOUT,9240) I,E
         END DO
         WRITE(LUNOUT,9260)
         DO I=1,NSTATE
           WRITE(LUNOUT,9263) I,HEFF(I,I,2)
         END DO
         IF(NSTATE.GT.1) WRITE(LUNOUT,9265)
C
         IF(NSTATE.EQ.1) THEN
            WRITE(LUNOUT,9270)
         ELSE
            WRITE(LUNOUT,9280)
         END IF
         WRITE(LUNOUT,9290)
         DO I=1,NSTATE
           WRITE(LUNOUT,9300) I,CASENE(I),EIGVAL(I)
         END DO
 9200 FORMAT(1X,'#########################'/
     *       1X,'###   MRMP2 RESULTS   ###'/
     *       1X,'#########################')
 9210 FORMAT(1X,'###########################'/
     *       1X,'###   MCQDPT2 RESULTS   ###'/
     *       1X,'###########################')
 9220 FORMAT(/1X,'*** EFFECTIVE HAMILTONIAN (AT SECOND ORDER) ***')
 9230 FORMAT(/1X,'*** ZEROTH ORDER ENERGIES ***'/
     *       3X,'STATE',8X,'0TH ORDER')
 9240 FORMAT(I6,F20.10)
 9260 FORMAT(/1X,'*** DIAGONAL ELEMENTS OF EFFECTIVE HAMILTONIAN ***'/
     *       3X,'STATE',18X,'2ND ORDER')
 9263 FORMAT(I6,'   E(MRMP2)=',F20.10)
 9265 FORMAT(1X,'THE DIAGONAL ELEMENTS ARE ALMOST THE STATE-SPECIFIC',
     *          ' E(MRMP2) CORRECTIONS.'/
     *       1X,'HOWEVER, THEY DIFFER SLIGHTLY BECAUSE OF THE USE OF',
     *          ' STATE-AVERAGED,'/
     *       1X,'RATHER THAN STATE-SPECIFIC CANONICAL ORBITALS.')
 9270 FORMAT(/1X,'*** MRMP2 ENERGY ***')
 9280 FORMAT(/1X,'*** MCQDPT2 ENERGIES ***')
 9290 FORMAT(3X,'STATE',23X,'1ST ORDER',23X,'2ND ORDER')
 9300 FORMAT(I6,5X,'E(MCSCF)=',F20.10,5X,'E(MP2)=',F20.10)
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      IF(IROT.EQ.2) THEN
         WRITE(LUNOUT,9310)
         CALL MQWMAG(LUNOUT,HEFF(1,1,3),NSTATE,NSTATE,NSTATE,'TRIG')
         WRITE(LUNOUT,'('' *** MC-QDPT2 ENERGIES (ROT) ***''/
     *    1X,71(1H-)/3X,''STATE'',9X,''1ST'',27X,''2ND'')')
         DO I=1,NSTATE
          WRITE(LUNOUT,'(I6,2F30.20)') I,CASENE(I),DIAVAL(I)
         END DO
         WRITE(LUNOUT,'(1X,71(1H-))')
      END IF
 9310 FORMAT(/1X,'*** EFFECTIVE HAMILTONIAN (0-2) (ROT) ***')
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C          THIS ENDS A -MASWRK- IF BLOCK
      END IF
C
C**** ORDER DIAVEC FOR WRITING *****************************************
C
      DO I=1,NSTATE
        IOLD=LORDER(I)
        DO J=1,NSTATE
          HEFFVC(J,I)=DIAVEC(J,IOLD)
        END DO
      END DO
C
      EMP2=EIGVAL(NSTATE)
      RETURN
      END
C*MODULE MCQDPT  *DECK MQHTRN
      SUBROUTINE MQHTRN(LUNOUT,LPOUT ,NGO   ,NMO,
     *                  CFMO  ,HINTGO,HINTMO)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION CFMO(NGO,NMO),HINTGO(NGO,NGO),HINTMO(NMO,NMO)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQHTRN ***'')')
        CALL MQDEBG(LUNOUT,
     *  '   NGO','   NMO','     -','     -','     -','     -','     -',
     *      NGO ,    NMO ,      0 ,      0 ,      0 ,      0 ,      0 )
      END IF
      IF(LPOUT.LE.-10 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' CORE HAMILTONIAN ON GO BASE'')')
        CALL MQWMAG(LUNOUT,HINTGO,NGO,NGO,NGO,'TRIG')
      END IF
C***********************************************************************
C***                          ******************************************
C***   START TRANSFORMATION   ******************************************
C***                          ******************************************
C**** CLEAR HINTMO *****************************************************
      DO J=1,NMO
        DO I=1,NMO
          HINTMO(I,J)=0.0D+00
        END DO
      END DO
C**** START ************************************************************
      DO I=1,NGO
        HINTGO(I,I)=0.5D+00*HINTGO(I,I)
      END DO
C      DO L=1,NGO
C        DO K=L,NGO
C          H=HINTGO(K,L)
C          DO J=1,NMO
C            DO I=J,NMO
      DO J=1,NMO
        DO I=J,NMO
          DO L=1,NGO
            HIMCKI=0D0
            HIMCKJ=0D0
            DO K=L,NGO
              H=HINTGO(K,L)
C              HINTMO(I,J)=HINTMO(I,J)
C     *          + CFMO(K,I)*CFMO(L,J)*H
C     *          + CFMO(L,I)*CFMO(K,J)*H
C              HINTMO(J,I)=HINTMO(J,I)
C     *          + CFMO(K,J)*CFMO(L,I)*H
C     *          + CFMO(L,J)*CFMO(K,I)*H
              HIMCKI=HIMCKI+CFMO(K,I)*H
              HIMCKJ=HIMCKJ+CFMO(K,J)*H
            END DO
            HIMC=HIMCKI*CFMO(L,J)+HIMCKJ*CFMO(L,I)
            HINTMO(I,J)=HINTMO(I,J)+HIMC
            HINTMO(J,I)=HINTMO(J,I)+HIMC
          END DO
        END DO
      END DO
      DO I=1,NMO
        HINTMO(I,I)=0.5D+00*HINTMO(I,I)
      END DO
C
C     THE ABOVE STATEMENTS ARE EQUIVALENT TO THE FOLLOWING.
C
C     DO L=1,NGO
C       DO K=1,NGO
C         S=HINTGO(K,L)
C         DO J=1,NMO
C           DO I=1,NMO
C             HINTMO(I,J)=HINTMO(I,J)+CFMO(K,I)*CFMO(L,J)*S
C           END DO
C         END DO
C       END DO
C     END DO
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LE.-10 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' CORE HAMILTONIAN ON MO BASE'')')
        CALL MQWMAG(LUNOUT,HINTMO,NMO,NMO,NMO,'TRIG')
      END IF
C***********************************************************************
C***            ********************************************************
C***   RETURN   ********************************************************
C***            ********************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQI1EX
      SUBROUTINE MQI1EX(LUNOUT,LPOUT ,INIACT,LASACT,NOCF  ,NOCFI ,
     *                  OMAP  ,OMAPI ,I1EX1 ,I1EX2 ,OMAPC ,
     *                  IHMAPI,IHSMAP,NHASH )
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER   OMAP (INIACT:LASACT+1,NOCF )
      INTEGER   OMAPI(INIACT:LASACT+1,NOCFI)
      INTEGER   I1EX1(INIACT:LASACT,0:NOCF )
      INTEGER   I1EX2(INIACT:LASACT,0:NOCFI)
      INTEGER   OMAPC(INIACT:LASACT)
      INTEGER IHMAPI(NOCFI),IHSMAP(NHASH)
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQI1EX ***'')')
        CALL MQDEBG(LUNOUT,
     *  'INIACT','LASACT','  NOCF',' NOCFI','     -','     -','     -',
     *   INIACT , LASACT ,   NOCF ,  NOCFI ,      0 ,      0 ,      0 )
      END IF
C**** INITIALIZE I1EX1 AND I1EX2 ***************************************
      NMOACT=LASACT-INIACT+1
      DO IOCF=0,NOCF
        DO I=INIACT,LASACT
          I1EX1(I,IOCF)=0
        END DO
      END DO
      DO IOCFI=0,NOCFI
        DO I=INIACT,LASACT
          I1EX2(I,IOCFI)=0
        END DO
      END DO
C**** MAKE I1EX1 *******************************************************
      CALL VCLR(IHSMAP,1,NHASH)
      CALL VCLR(IHMAPI,1,NOCFI)
      N=0
      DO IOCF=1,NOCF
         DO IORB=INIACT,LASACT
            IF (OMAP(IORB,IOCF).NE.1) THEN
               DO I=INIACT,LASACT
                  OMAPC(I)=OMAP(I,IOCF)
               END DO
               OMAPC(IORB)=OMAPC(IORB)-1
               IHMAPC=MQHASH(OMAPC,NMOACT,NHASH)
               IF (IHSMAP(IHMAPC).EQ.0) THEN
                  N=N+1
                  IHSMAP(IHMAPC)=N
                  DO I=INIACT,LASACT
                     OMAPI(I,N)=OMAPC(I)
                  END DO
                  I1EX1(IORB,IOCF)=N
               ELSE
                  IOCFI=IHSMAP(IHMAPC)
 100              CONTINUE
                  IORBI=IHMAPI(IOCFI)
                  DO I=INIACT,LASACT
                     IF (OMAPC(I).NE.OMAPI(I,IOCFI)) GO TO 120
                  END DO
                  I1EX1(IORB,IOCF)=IOCFI
                  GO TO 140
 120              CONTINUE
                  IF (IORBI.GT.0) THEN
                     IOCFI=IORBI
                     GO TO 100
                  END IF
                  N=N+1
                  IHMAPI(IOCFI)=N
                  DO I=INIACT,LASACT
                     OMAPI(I,N)=OMAPC(I)
                  END DO
                  I1EX1(IORB,IOCF)=N
 140              CONTINUE
               END IF
            END IF
         END DO
      END DO
C**** ERROR STOP *******************************************************
      IF(N.NE.NOCFI) THEN
        IF(MASWRK) WRITE(LUNOUT,
     *        '('' *** ERROR STOP IN SUB.MQI1EX ***'',/,
     *        '' N /= NOCFI'',/,
     *        '' N ='',I10,''  NOCFI ='',I10)') N,NOCFI
        CALL ABRT
      END IF
C**** MAKE I1EX2 *******************************************************
      CALL VCLR(IHSMAP,1,NHASH)
      CALL VCLR(IHMAPI,1,NOCF)
      DO IOCF=1,NOCF
         IHASH=MQHASH(OMAP(INIACT,IOCF),NMOACT,NHASH)
         IF (IHSMAP(IHASH).EQ.0) THEN
            IHSMAP(IHASH)=IOCF
         ELSE
            IPOCF=IHSMAP(IHASH)
 200        CONTINUE
            IPOCFN=IHMAPI(IPOCF)
            IF (IPOCFN.GT.0) THEN
               IPOCF=IPOCFN
               GO TO 200
            END IF
            IHMAPI(IPOCF)=IOCF
         END IF
      END DO
C
      DO IOCFI=1,NOCFI
         DO IORB=INIACT,LASACT
            IF(OMAPI(IORB,IOCFI).NE.3) THEN
               DO I=INIACT,LASACT
                  OMAPC(I)=OMAPI(I,IOCFI)
               END DO
               OMAPC(IORB)=OMAPC(IORB)+1
               IHMAPC=MQHASH(OMAPC,NMOACT,NHASH)
               IF (IHSMAP(IHMAPC).NE.0) THEN
                  IOCF=IHSMAP(IHMAPC)
 300              CONTINUE
                  IOCFN=IHMAPI(IOCF)
                  DO I=INIACT,LASACT
                     IF (OMAPC(I).NE.OMAP(I,IOCF)) GO TO 320
                  END DO
                  I1EX2(IORB,IOCFI)=IOCF
                  GO TO 340
 320              CONTINUE
                  IF (IOCFN.GT.0) THEN
                     IOCF=IOCFN
                     GO TO 300
                  END IF
 340              CONTINUE
               END IF
            END IF
         END DO
      END DO
C
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LE.-100 .AND. MASWRK) THEN
        NMOACT=LASACT-INIACT+1
        WRITE(LUNOUT,'('' *** MATRIX I1EX1 ***'')')
        CALL MQWMAI(LUNOUT,I1EX1(1,1),NMOACT,NMOACT,NOCF ,'    ')
        WRITE(LUNOUT,'('' *** MATRIX I1EX2 ***'')')
        CALL MQWMAI(LUNOUT,I1EX2(1,1),NMOACT,NMOACT,NOCFI,'    ')
      END IF
C**** RETURN ***********************************************************
      RETURN
      END
C
C*MODULE MCQDPT  *DECK MQINIT
      INTEGER FUNCTION MQINIT(INIT)
      CHARACTER*1 CTYPE
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      SAVE N
C
C     INITIALIZE PARAMETERS
C
      N=INIT
      MQINIT=N
      RETURN
C
C     -------------------------
      ENTRY MQADDR(NSIZE,CTYPE)
C     -------------------------
C        RETURN VALUE IS THE STARTING ADDRESS OF THE WORK ARRAY.
C        WE THEN CALCULATE HOW FAR INTO THE DYNAMIC MEMORY POOL
C        THE END OF THE CURRENTLY REQUESTED ARRAY WILL EXTEND,
C        IN UNITS OF 64 BIT FLOATING POINT WORDS.
C
      MQADDR=N
C
      IF(CTYPE.EQ.'D') THEN
         N=N+NSIZE
         RETURN
      END IF
      IF(CTYPE.EQ.'I') THEN
         N=N+(NSIZE-1)/NWDVAR+1
         RETURN
      END IF
      IF(CTYPE.EQ.'L') THEN
         N=N+(NSIZE-1)/NWDVAR+1
         RETURN
      END IF
      IF(CTYPE.EQ.'Q') THEN
         N=N+2*NSIZE
         RETURN
      END IF
C
      WRITE(6,9000) CTYPE
      CALL ABRT
      STOP
 9000 FORMAT(1X,'-MQADDR- CALLED WITH ILLEGAL DATA TYPE ',A)
C
C     SHOULD NOT USE CHARACTER*1 OR REAL*4 WITHIN GAMESS DYNAMIC MEMORY
C-MWS-      IF(CTYPE.EQ.'C') THEN
C-MWS-         N=N+(NSIZE-1)/8+1
C-MWS-      IF(CTYPE.EQ.'2') THEN
C-MWS-         N=N+(NSIZE-1)/4+1
C
C     --------------
      ENTRY MQMNXT()
C     --------------
C     REPORT CURRENT TOP ADDRESS
C
      MQMNXT=N
      RETURN
      END
C*MODULE MCQDPT  *DECK MQLBL1
      SUBROUTINE MQLBL1(LUNOUT,LPOUT ,INIACT,LASACT,NOCF  ,
     *                  NCSF  ,
     *                  NSPINF,OMAP  ,NSNSF )
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT INTEGER (A-Z)
      DIMENSION NSPINF(0:20)
      INTEGER   OMAP(INIACT:LASACT+1,NOCF)
      INTEGER   NSNSF(NOCF+1)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQLBL1 ***'')')
        CALL MQDEBG(LUNOUT,
     *  'INIACT','LASACT','  NOCF','     -','     -','     -','     -',
     *   INIACT , LASACT ,   NOCF ,      0 ,      0 ,      0 ,      0 )
        WRITE(LUNOUT,'('' *** MATRIX NSPINF ***'')')
        CALL MQWMAI(LUNOUT,NSPINF(1),1,1,20,'    ')
      END IF
C**** MAKE LABELS ******************************************************
      NSNSF(1)=0
      DO 102 IOCF=1,NOCF
        N=0
        DO 100 I=INIACT,LASACT
          NOCC=OMAP(I,IOCF)
          IF(NOCC.EQ.2) N=N+1
  100   CONTINUE
        NSF=NSPINF(N)
C.... A AND LAMBDA => A LAMBDA .........................................
        NSNSF(IOCF+1)=NSNSF(IOCF)+NSF
C....                          .........................................
  102 CONTINUE
C     N=0
C     DO 104 I=INIACT,LASACT
C       NOCC=OMAP(I,NOCF)
C       IF(NOCC.EQ.2) N=N+1
C 104 CONTINUE
C     NSF=NSPINF(N)
C     NCSF=NSNSF(NOCF)+NSF
      NCSF=NSNSF(NOCF+1)
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** MATRIX NSNSF ***'')')
        CALL MQWMAI(LUNOUT,NSNSF,1,1,NOCF+1,'    ')
      END IF
C**** RETURN ***********************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQLBL2
      SUBROUTINE MQLBL2(LUNOUT,LPOUT ,NOCF  ,NCSF  ,
     *                  NSNSF ,IOCSF )
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT INTEGER (A-Z)
      INTEGER   NSNSF(NOCF+1), IOCSF(2,NCSF)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQLBL2 ***'')')
        CALL MQDEBG(LUNOUT,
     *  '  NOCF','  NCSF','     -','     -','     -','     -','     -',
     *     NOCF ,   NCSF ,      0 ,      0 ,      0 ,      0 ,      0 )
      END IF
C**** MAKE LABELS ******************************************************
      DO 102 IOCF=1,NOCF
C       IF(IOCF.NE.NOCF) THEN
          NSF=NSNSF(IOCF+1)-NSNSF(IOCF)
C       ELSE
C         NSF=NCSF-NSNSF(NOCF)
C       END IF
C.... A LAMBDA => A AND LAMBDA .........................................
        DO 100 I=1,NSF
          J=NSNSF(IOCF)+I
          IOCSF(1,J)=IOCF
          IOCSF(2,J)=I
  100   CONTINUE
C....                          .........................................
  102 CONTINUE
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LE.-100 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** MATRIX IOCSF ***'')')
        CALL MQWMAI(LUNOUT,IOCSF,2,2,NCSF,'    ')
      END IF
C**** RETURN ***********************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQLMB1
      SUBROUTINE MQLMB1(LUNOUT,LPOUT ,LUNFTA,LUNFT1,
     *                  IALPHA,INIACT,LASACT,
     *                  NSTATE,NCSF  ,NOCF  ,MAXSPF,LENGTH,NOCFI ,
     *                  GENZRO,EDSHFT,
     *                  NSNSF ,I1EX1 ,I1EX2 ,
     *                  GEN1WK,GEN1  ,
     *                  LAB1R ,WORKR ,LAB1W ,WORKW ,VAL2  ,HEFF2 ,
     *                  LUNTWO,NMOFZC,NMODOC,NMOACT,NMOEXT,
     *                  NMO   ,NMOEI ,NMOII ,THRGEN,
     *                  HEFF  ,CASVEC,EORB  ,LIJMO ,VONEEL,VTWOEL,
     *                  ECONF ,EREF0 ,CASENE,KREF  ,MOSYM ,
     *                  NCSFNW,LOD2NW)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL   KLOOP
      INTEGER   NSNSF(NOCF+1)
      INTEGER   I1EX1(INIACT:LASACT,0:NOCF)
      INTEGER   I1EX2(INIACT:LASACT,0:NOCFI)
C      DIMENSION GEN1WK(INIACT:LASACT,MAXSPF,MAXSPF)
      DIMENSION GEN1WK(MAXSPF,MAXSPF,INIACT:LASACT)
C      DIMENSION GEN1(INIACT:LASACT,0:NCSFNW)
      DIMENSION GEN1(0:NCSFNW,INIACT:LASACT)
      INTEGER   LAB1R(3,LENGTH), LAB1W(2,LENGTH)
      DIMENSION WORKR(LENGTH), WORKW(LENGTH)
      DIMENSION VAL2(NSTATE), HEFF2(NSTATE)
C
      DIMENSION HEFF  (NSTATE,NSTATE,0:3)
      DIMENSION CASVEC(NCSF,NSTATE), EORB(NMO)
      DIMENSION LIJMO (NMO,NMO)
      DIMENSION VONEEL(NMO,NMO) , VTWOEL(NMOEI,NMOII)
      DIMENSION ECONF (NCSF)    , EREF0 (NSTATE)     , CASENE(NSTATE)
      LOGICAL   KREF(NCSF)
      DIMENSION MOSYM(NMO)
      DIMENSION LOD2NW(NCSF)
      DIMENSION ISYM(8,8)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON/MQSYLB/ISYLAB(2,8,4)
      DATA ISYM /1,2,3,4,5,6,7,8,
     *           2,1,4,3,6,5,8,7,
     *           3,4,1,2,7,8,5,6,
     *           4,3,2,1,8,7,6,5,
     *           5,6,7,8,1,2,3,4,
     *           6,5,8,7,2,1,4,3,
     *           7,8,5,6,3,4,1,2,
     *           8,7,6,5,4,3,2,1/
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQLMB1 ***'')')
        CALL MQDEBG(LUNOUT,
     *  'LUNFTA','LUNFT1','IALPHA','INIACT','LASACT','NSTATE','  NCSF',
     *   LUNFTA , LUNFT1 , IALPHA,  INIACT , LASACT , NSTATE ,   NCSF )
        CALL MQDEBG(LUNOUT,
     *  '  NOCF','MAXSPF','LENGTH',' NOCFI','LUNTWO','NMOFZC','NMODOC',
     *     NOCF , MAXSPF , LENGTH ,  NOCFI , LUNTWO , NMOFZC , NMODOC )
        CALL MQDEBG(LUNOUT,
     *  'NMOACT','NMOEXT','   NMO',' NMOEI',' NMOII','NCSFNW','     -',
     *   NMOACT , NMOEXT ,    NMO ,  NMOEI ,  NMOII , NCSFNW ,      0 )
      END IF
C==== SET CONSTANTS ====================================================
      INIDOC=NMOFZC+1
      LASDOC=NMOFZC+NMODOC
      INIACT=NMOFZC+NMODOC+1
      LASACT=NMOFZC+NMODOC+NMOACT
      INIEXT=NMOFZC+NMODOC+NMOACT+1
      LASEXT=NMOFZC+NMODOC+NMOACT+NMOEXT
C==== CLEAR HEFF =======================================================
      ISTATE=IALPHA
      IF(ISTATE.EQ.1) THEN
        DO J=1,NSTATE
          DO I=1,NSTATE
            HEFF(I,J,0)=0.0D+00
            HEFF(I,J,1)=0.0D+00
            HEFF(I,J,2)=0.0D+00
            HEFF(I,J,3)=0.0D+00
          END DO
        END DO
      END IF
      DO I=1,NSTATE
         HEFF2(I)=0.0D+00
      END DO
C==== READ TWO-ELECTRON INTEGERALS =====================================
      CALL SEQREW(LUNTWO)
      DO I=1,NMOII
        CALL MQMATR(LUNTWO,NMOEI,VTWOEL(1,I))
      END DO
C=======================================================================
C====              =====================================================
C==== ZEROTH ORDER =====================================================
C====              =====================================================
      HEFF(ISTATE,ISTATE,0)=EREF0(ISTATE)
C=======================================================================
C====             ======================================================
C==== FIRST ORDER ======================================================
C====             ======================================================
      HEFF(ISTATE,ISTATE,1)=CASENE(ISTATE)
C==== DEBUG OUTPUT =====================================================
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** HEFF(0-1) ***'')')
        CALL MQWMAG(LUNOUT,HEFF(1,1,1),NSTATE,NSTATE,NSTATE,'SYMM')
      END IF
C=======================================================================
C====              =====================================================
C==== SECOND ORDER =====================================================
C====              =====================================================
C=======================================================================
C==== ZERO-BODY GENERATOR (GET ECORE) ==================================
C====                                 ==================================
      CALL TSECND(STIME)
C
C      DO I=1,NCSF
C        IF (GOPARR .AND. MOD(I,NPROC).NE.ME) GO TO 199
      DO I=1+ME,NCSF,NPROC
        IF(KREF(I)) THEN
          KLOOP=.FALSE.
          DO JSTATE=1,NSTATE
            VAL2(JSTATE)=CASVEC(I,ISTATE)*CASVEC(I,JSTATE)
            IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
          END DO
          IF(KLOOP) THEN
            ESI=ECONF(I)-EREF0(ISTATE)
            S=0.0D+00
            DO KAE=INIACT,LASEXT
              DO KI =INIDOC,LASDOC
                DELTA=EORB(KAE)-EORB(KI)+ESI
                S=S-2.0D+00*VONEEL(KI,KAE)**2/(DELTA+(EDSHFT/DELTA))
              END DO
            END DO
C.... MODIFIED ON 9/9/01
            DO KBF=INIACT,LASACT
              DO KJ =INIDOC,LASDOC
                LJBF=LIJMO(KJ,KBF)
                LBFJ=LIJMO(KBF,KJ)
                DO KI =INIDOC,LASDOC
                  DELTA0=-EORB(KI)+EORB(KBF)-EORB(KJ)+ESI
                  LBFI=LIJMO(KBF,KI)
                  DO KAE=INIACT,LASEXT
                    DELTA=EORB(KAE)+DELTA0
C                   DELTA=EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)+ESI
                    S=S-VTWOEL(LIJMO(KI,KAE),LJBF)
     *                 *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LBFJ)
     *                  -VTWOEL(LIJMO(KAE,KJ),LBFI)
     *                  )/(DELTA+(EDSHFT/DELTA))
                  END DO
                END DO
              END DO
            END DO
C.... MODIFIED ON 9/9/01
            DO KAE=INIACT,LASACT
              DO KJ =INIDOC,LASDOC
                LAEJ=LIJMO(KAE,KJ)
                DO KI =INIDOC,LASDOC
                  DELTA0=EORB(KAE)-EORB(KI)-EORB(KJ)+ESI
                  LIAE=LIJMO(KI,KAE)
                  LAEI=LIJMO(KAE,KI)
                  IS=ISYM(MOSYM(KAE),ISYM(MOSYM(KI),MOSYM(KJ)))
C                 DO KBF=INIEXT,LASEXT
                  DO KBF=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DELTA=EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)+ESI
                    DELTA=DELTA0+EORB(KBF)
                    S=S-VTWOEL(LIJMO(KJ,KBF),LIAE)
     *                 *(2.0D+00*VTWOEL(LIJMO(KBF,KJ),LAEI)
     *                   -VTWOEL(LIJMO(KBF,KI),LAEJ)
     *                  )/(DELTA+(EDSHFT/DELTA))
                  END DO
                END DO
              END DO
            END DO
            DO JSTATE=1,NSTATE
              IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
C                HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
C     *            +S*VAL2(JSTATE)
                 HEFF2(JSTATE)=HEFF2(JSTATE)+S*VAL2(JSTATE)
              END IF
            END DO
          END IF
        END IF
      END DO
C==== DEBUG OUTPUT =====================================================
      IF(LPOUT.LE.-5) THEN
         IF (GOPARR) CALL DDI_GSUMF(2504,HEFF2,NSTATE)
         DO JSTATE=1,NSTATE
            HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)+HEFF2(JSTATE)
            HEFF2(JSTATE)=0.0D+00
         END DO
         IF (MASWRK) THEN
           WRITE(LUNOUT,'('' *** HEFF(2) <- ZERO-BODY GENERATOR ***'')')
           CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
         END IF
      END IF
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      IF(MASWRK) WRITE(LUNOUT,9100) TIME
 9100 FORMAT(1X,'TIME FOR 0-BODY FORMULAE =',F10.2)
C***********************************************************************
C****       ************************************************************
C**** START ************************************************************
C****       ************************************************************
      CALL TSECND(STIME)
C**** CLEAR ARRAYS *****************************************************
C      DO I=1,MAXSPF
C        DO J=1,MAXSPF
C          DO K=INIACT,LASACT
      DO K=INIACT,LASACT
        DO J=1,MAXSPF
           DO I=1,MAXSPF
            GEN1WK(I,J,K)=0.0D+00
          END DO
        END DO
      END DO
      DO I=INIACT,LASACT
        DO J=0,NCSFNW
C          GEN1(I,J)=0.0D+00
          GEN1(J,I)=0.0D+00
        END DO
      END DO
      CALL SEQREW(LUNFT1)
      CALL SEQREW(LUNFTA)
C***********************************************************************
C**** START ************************************************************
C****       ************************************************************
C**** READ COUPLING CONSTANT ON CSF BASE *******************************
      MAXMU=0
      MAXNU=0
  200 CONTINUE
      DO I=INIACT,LASACT
         DO J=0,NCSFNW
C            GEN1(I,J)=0.0D+00
            GEN1(J,I)=0.0D+00
         END DO
      END DO
 210  CONTINUE
C      DO K=1,MAXNU
C         DO J=1,MAXMU
C            DO I=INIACT,LASACT
      DO K=INIACT,LASACT
         DO J=1,MAXMU
            DO I=1,MAXNU
               GEN1WK(I,J,K)=0.0D+00
            END DO
         END DO
      END DO
      MAXMU=0
 220  CONTINUE
        READ(LUNFTA) KCONTR,L1,IOCF2,NWORD
        CALL MQGENR(LUNFTA,3*NWORD,NWORD,LAB1R,WORKR)
        DO I=1,NWORD
          L2=LAB1R(1,I)
          MU=LAB1R(2,I)
          IF(MAXMU.LT.MU) MAXMU=MU
          NU=LAB1R(3,I)
C          GEN1WK(L2,MU,NU)=WORKR(I)
          GEN1WK(NU,MU,L2)=WORKR(I)
        END DO
      IF(KCONTR.EQ.100) GO TO 220
      MAXNU=NSNSF(IOCF2+1)-NSNSF(IOCF2)
C**** GET COUPLING CONSTANT <STATE/EIJ/CSF> ****************************
      NSPF2=NSNSF(IOCF2+1)-NSNSF(IOCF2)
      DO L2=INIACT,LASACT
        IOCF1=I1EX2(L1,I1EX1(L2,IOCF2))
        IF(IOCF1.NE.0) THEN
          NSPF1=NSNSF(IOCF1+1)-NSNSF(IOCF1)
          DO MU=1,NSPF1
            ICSF1=NSNSF(IOCF1)+MU
            DO NU=1,NSPF2
              ICSF2=NSNSF(IOCF2)+NU
              ICSF2N=LOD2NW(ICSF2)
C              GEN1(L2,ICSF2N)=GEN1(L2,ICSF2N)
C     *          +CASVEC(ICSF1,IALPHA)*GEN1WK(L2,MU,NU)
              GEN1(ICSF2N,L2)=GEN1(ICSF2N,L2)
     *          +CASVEC(ICSF1,IALPHA)*GEN1WK(NU,MU,L2)
            END DO
          END DO
        END IF
      END DO
C**** IF END OF OCF2, REFRESH GEN1WK AND GO TO 'READ' ******************
      IF(KCONTR.EQ.10) GO TO 210
C**** IF END OF ORB1, GET <STATE/EIJ/OCF> AND GO TO 'READ' *************
C      ELSE IF(KCONTR.EQ.1) THEN
C      ELSE IF(KCONTR.EQ.0) THEN
C=======================================================================
C==== ONE-BODY GENERATOR ===============================================
C====                    ===============================================
        DO II=INIACT,LASACT
C          IF (GOPARR .AND. MOD(II,NPROC).NE.ME) GO TO 299
          LP=L1
          LQ=II
          LPQ=LIJMO(LP,LQ)
          ISP=MOSYM(LP)
          ISQ=MOSYM(LQ)
          IF(ISP.EQ.ISQ) THEN
C            DO JJ=1,NOCF
            DO JJ=1+ME,NOCF,NPROC
              NSPINF=NSNSF(JJ+1)-NSNSF(JJ)
              EQ=EORB(LQ)
              EP=EORB(LP)
              DO JSTATE=1,NSTATE
                VAL2(JSTATE)=0.0D+00
              END DO
              DO JSTATE=1,NSTATE
                DO KK=1,NSPINF
                  M2=NSNSF(JJ)+KK
                  M2N=LOD2NW(M2)
                  IF(KREF(M2)) THEN
                    VAL2(JSTATE)=VAL2(JSTATE)
     *                +GEN1(M2N,II)*CASVEC(M2,JSTATE)
C     *                +GEN1(II,M2N)*CASVEC(M2,JSTATE)
                  END IF
                END DO
              END DO
              KLOOP=.FALSE.
              DO JSTATE=1,NSTATE
                IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
              END DO
              IF(KLOOP) THEN
                ESI2=ECONF(M2)-EREF0(ISTATE)
                S=0.0D+00
                DO KI=INIDOC,LASDOC
                  EI2=EORB(KI)-ESI2
                  DELTA=EP-EI2
                  S=S+VONEEL(KI,LQ)*VONEEL(LP,KI)/(DELTA+EDSHFT/DELTA)
                END DO
                DO KE=INIEXT,LASEXT
                  EE2=EORB(KE)+ESI2
                  DELTA=EE2-EQ
                  S=S-VONEEL(LP,KE)*VONEEL(KE,LQ)/(DELTA+EDSHFT/DELTA)
                END DO
                DO KAE=INIACT,LASEXT
                  DO KI =INIDOC,LASDOC
                    LPI=LIJMO(LP,KI)
                    LIQ=LIJMO(KI,LQ)
                    EAEI2=EORB(KAE)-EORB(KI)+ESI2
                    DELTA=EAEI2+EP-EQ
                    DELTA1=EAEI2
                    S=S-VONEEL(KI,KAE)
     *                *( ( 2.0D+00*VTWOEL(LIJMO(KAE,KI),LPQ)
     *                -      VTWOEL(LIJMO(KAE,LQ),LPI) )
     *                /(DELTA+EDSHFT/DELTA)
     *                +( 2.0D+00*VTWOEL(LIJMO(KI,KAE),LPQ)
     *                -      VTWOEL(LIJMO(LP,KAE),LIQ) )
     *                /(DELTA1+EDSHFT/DELTA1) 
     *                )
                  END DO
                END DO
                DO KI =INIDOC,LASDOC
                  LPI=LIJMO(LP,KI)
                  LIQ=LIJMO(KI,LQ)
                  DO KJ =INIDOC,LASDOC
                    LPJ=LIJMO(LP,KJ)
                    DELTA0=EP-EORB(KI)-EORB(KJ)+ESI2
                    IS=ISYM(MOSYM(LP),ISYM(MOSYM(KI),MOSYM(KJ)))
C                   DO KAE=INIACT,LASACT
                    DO KAE=ISYLAB(1,IS,3),ISYLAB(2,IS,3)
C                     DELTA=EORB(KAE)+EP-EORB(KI)-EORB(KJ)+ESI2
                      DELTA=EORB(KAE)+DELTA0
                      S=S+VTWOEL(LIJMO(KJ,KAE),LIQ)
     *                  *( 2.0D+00*VTWOEL(LIJMO(KAE,KJ),LPI)
     *                  -      VTWOEL(LIJMO(KAE,KI),LPJ) )
     *                  /(DELTA+EDSHFT/DELTA)
                    END DO
                  END DO
                END DO
                DO KI =INIDOC,LASDOC
                  LPI=LIJMO(LP,KI)
                  LIQ=LIJMO(KI,LQ)
                  DO KJ =INIDOC,LASDOC
                    LPJ=LIJMO(LP,KJ)
                    IS=ISYM(MOSYM(LP),ISYM(MOSYM(KI),MOSYM(KJ)))
                    DELTA0=EP-EORB(KI)-EORB(KJ)+ESI2
                    DO KAE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KAE=INIEXT,LASEXT
C                     DELTA=EORB(KAE)+EP-EORB(KI)-EORB(KJ)+ESI2
                      DELTA=EORB(KAE)+DELTA0
                      S=S+VTWOEL(LIJMO(KJ,KAE),LIQ)
     *                  *( 2.0D+00*VTWOEL(LIJMO(KAE,KJ),LPI)
     *                  -      VTWOEL(LIJMO(KAE,KI),LPJ) )
     *                  /(DELTA+EDSHFT/DELTA)
                    END DO
                  END DO
                END DO
C.... MODIFIED ON 9/9/01
                DO KBF=INIACT,LASACT
                  LPBF=LIJMO(LP,KBF)
                  LBFQ=LIJMO(KBF,LQ)
                  DO KI =INIDOC,LASDOC
                    DELTA0=EORB(KBF)-EORB(KI)-EQ+ESI2
                    LBFI=LIJMO(KBF,KI)
                    IS=ISYM(MOSYM(LP),ISYM(MOSYM(KBF),MOSYM(KI)))
                    DO KAE=ISYLAB(1,IS,3),ISYLAB(2,IS,3)
C                   DO KAE=INIACT,LASACT
C                     DELTA=EORB(KAE)+EORB(KBF)-EORB(KI)-EQ+ESI2
                      DELTA=EORB(KAE)+DELTA0
                      S=S-VTWOEL(LIJMO(KI,KAE),LPBF)
     *                  *( 2.0D+00*VTWOEL(LIJMO(KAE,KI),LBFQ)
     *                  -      VTWOEL(LIJMO(KAE,LQ),LBFI) )
     *                  /(DELTA+EDSHFT/DELTA)
                    END DO
                  END DO
                END DO
C.... MODIFIED ON 9/9/01
                DO KBF=INIACT,LASACT
                  LPBF=LIJMO(LP,KBF)
                  LBFQ=LIJMO(KBF,LQ)
                  DO KI =INIDOC,LASDOC
                    LBFI=LIJMO(KBF,KI)
                    IS=ISYM(MOSYM(LP),ISYM(MOSYM(KI),MOSYM(KBF)))
                    DELTA0=EORB(KBF)-EORB(KI)-EQ+ESI2
                    DO KAE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KAE=INIEXT,LASEXT
C                     DELTA=EORB(KAE)+EORB(KBF)-EORB(KI)-EQ+ESI2
                      DELTA=EORB(KAE)+DELTA0
                      S=S-VTWOEL(LIJMO(KI,KAE),LPBF)
     *                  *( 2.0D+00*VTWOEL(LIJMO(KAE,KI),LBFQ)
     *                  -      VTWOEL(LIJMO(KAE,LQ),LBFI) )
     *                  /(DELTA+EDSHFT/DELTA)
                    END DO
                  END DO
                END DO
C.... MODIFIED ON 9/9/01
                DO KAE=INIACT,LASACT
                  LAEQ=LIJMO(KAE,LQ) 
                  DO KI =INIDOC,LASDOC
                    LIAE=LIJMO(KI,KAE)
                    LAEI=LIJMO(KAE,KI)
                    IS=ISYM(MOSYM(LP),ISYM(MOSYM(KI),MOSYM(KAE)))
                    DELTA0=EORB(KAE)-EORB(KI)-EQ+ESI2
                    DO KBF=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KBF=INIEXT,LASEXT
C                     DELTA=EORB(KAE)+EORB(KBF)-EORB(KI)-EQ+ESI2
                      DELTA=EORB(KBF)+DELTA0
                      S=S-VTWOEL(LIJMO(LP,KBF),LIAE)
     *                  *( 2.0D+00*VTWOEL(LIJMO(KBF,LQ),LAEI)
     *                  -      VTWOEL(LIJMO(KBF,KI),LAEQ) )
     *                  /(DELTA+EDSHFT/DELTA)
                    END DO
                  END DO
                END DO
                DO JSTATE=1,NSTATE
                  IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
C                    HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
C     *                +S*VAL2(JSTATE)
                    HEFF2(JSTATE)=HEFF2(JSTATE)+S*VAL2(JSTATE)
                  END IF
                END DO
              END IF
            END DO
          END IF
        END DO
C.... WRITE GEN1 .......................................................
        NWRITE=0
        DO I=INIACT,LASACT
          DO J=1,NCSF
            JN=LOD2NW(J)
            IF(JN.NE.0) THEN
C              S=ABS(GEN1(I,JN))
              S=ABS(GEN1(JN,I))
              IF(S.GT.GENZRO) THEN
                NWRITE=NWRITE+1
                IF(NWRITE.GT.LENGTH) THEN
                  KCONTW=10
                  WRITE(LUNFT1) KCONTW,L1,LENGTH
                  CALL MQGENW(LUNFT1,2*LENGTH,LENGTH,LAB1W,WORKW)
                  NWRITE=1
                END IF
                LAB1W(1,NWRITE)=I
                LAB1W(2,NWRITE)=J
C                WORKW(  NWRITE)=GEN1(I,JN)
                WORKW(  NWRITE)=GEN1(JN,I)
              END IF
            END IF
          END DO
        END DO
C        KCONTW=1
C        IF(KCONTR.EQ.0) KCONTW=0
        KCONTW=KCONTR
        WRITE(LUNFT1) KCONTW,L1,NWRITE
        CALL MQGENW(LUNFT1,2*NWRITE,NWRITE,LAB1W,WORKW)
C.... CLEAR ARRAYS .....................................................
        IF(KCONTR.NE.0) GO TO 200
C**** IF END OF STATE, GET <STATE/EIJ/OCS> *****************************
C**** ERROR STOP *******************************************************
C      ELSE
C       STOP ' *** ERROR STOP IN SUB.MQLMB1.'
C      END IF
C------------------------------------------------------------- DDI_GSUMF
      IF (GOPARR) CALL DDI_GSUMF(2504,HEFF2,NSTATE)
      DO JSTATE=1,NSTATE
         HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)+HEFF2(JSTATE)
      END DO
C==== DEBUG OUTPUT =====================================================
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** HEFF(2) <- ONE-BODY GENERATOR ***'')')
        CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
      END IF
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      IF(MASWRK) WRITE(LUNOUT,9110) TIME
 9110 FORMAT(1X,'TIME FOR 1-BODY FORMULAE =',F10.2)
C***********************************************************************
C****        ***********************************************************
C**** RETURN ***********************************************************
C****        ***********************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQLMB2
      SUBROUTINE MQLMB2(LUNOUT,LPOUT ,LUNFTA,LUNFT1,LUNFT2,
     *                  INIACT,LASACT,NCSF  ,NOCF  ,MAXSPF,LENGTH,
     *                  NOCFI ,
     *                  GENZRO, EDSHFT,
     *                  NSNSF ,I1EX1 ,I1EX2 ,
     *                  GEN1WK,GEN1  ,GEN2  ,
     *                  LABA2 ,LAB1  ,WORK  ,VAL2  ,HEFF2 ,
     *                  NMOFZC,NMODOC,NMOACT,NMOEXT,
     *                  ISTATE,NSTATE,NMO   ,NMOEI ,NMOII ,THRGEN,
     *                  HEFF  ,CASVEC,EORB  ,LIJMO ,VONEEL,VTWOEL,
     *                  ECONF ,EREF0 ,KREF  ,MOSYM ,
     *                  NCSFNW,LOD2NW,
     *                  LMB2OP,KFTAM,LFTAM,WFTAM)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (ZERO=0.0D+00)
      LOGICAL   KLOOP
      INTEGER   NSNSF(NOCF+1)
      INTEGER   I1EX1(INIACT:LASACT,0:NOCF)
      INTEGER   I1EX2(INIACT:LASACT,0:NOCFI)
C      DIMENSION GEN1WK(INIACT:LASACT,MAXSPF,MAXSPF)
      DIMENSION GEN1WK(MAXSPF,MAXSPF,INIACT:LASACT)
      DIMENSION GEN1(INIACT:LASACT,0:NCSFNW)
C      DIMENSION GEN2(INIACT:LASACT,INIACT:LASACT,0:NCSFNW)
      DIMENSION GEN2(0:NCSFNW,INIACT:LASACT,INIACT:LASACT)
      INTEGER   LABA2(3,LENGTH), LAB1(2,LENGTH)
      DIMENSION WORK(LENGTH)
      DIMENSION VAL2(NSTATE),HEFF2(NSTATE)
C
      INTEGER   KFTAM(4,1),LFTAM(3,1)
      DIMENSION WFTAM(1)
C
      DIMENSION HEFF  (NSTATE,NSTATE,0:3)
      DIMENSION CASVEC(NCSF,NSTATE), EORB(NMO)
      DIMENSION LIJMO (NMO,NMO)
      DIMENSION VONEEL(NMO,NMO) , VTWOEL(NMOEI,NMOII)
      DIMENSION ECONF (NCSF)    , EREF0 (NSTATE)
      LOGICAL   KREF(NCSF)
      DIMENSION MOSYM(NMO)
      DIMENSION LOD2NW(NCSF)
      DIMENSION ISYM(8,8)
      LOGICAL GOPARR,DSKWRK,MASWRK
      LOGICAL GEN2PR
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /MQFT2 /NWFT2,NWFT2P,NWFT2G
      COMMON /MQFTB2/NWFTA,NWFTAP,NWFTAG
      COMMON/MQSYLB/ISYLAB(2,8,4)
      DATA ISYM /1,2,3,4,5,6,7,8,
     *           2,1,4,3,6,5,8,7,
     *           3,4,1,2,7,8,5,6,
     *           4,3,2,1,8,7,6,5,
     *           5,6,7,8,1,2,3,4,
     *           6,5,8,7,2,1,4,3,
     *           7,8,5,6,3,4,1,2,
     *           8,7,6,5,4,3,2,1/
C
      CALL TSECND(STIME)
C
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQLMB2 ***'')')
        CALL MQDEBG(LUNOUT,
     *  'LUNFTA','LUNFT1','LUNFT2','INIACT','LASACT','  NCSF','  NOCF',
     *   LUNFTA , LUNFT1 , LUNFT2 , INIACT , LASACT ,   NCSF ,   NOCF )
        CALL MQDEBG(LUNOUT,
     *  'MAXSPF','LENGTH',' NOCFI','NMOFZC','NMODOC','NMOACT','NMOEXT',
     *   MAXSPF , LENGTH ,  NOCFI , NMOFZC , NMODOC , NMOACT , NMOEXT )
        CALL MQDEBG(LUNOUT,
     *  'ISTATE','NSTATE','   NMO',' NMOEI',' NMOII','NCSFNW','LMB2OP',
     *   ISTATE , NSTATE ,    NMO ,  NMOEI ,  NMOII , NCSFNW , LMB2OP )
      END IF
C==== SET CONSTANTS ====================================================
      INIDOC=NMOFZC+1
      LASDOC=NMOFZC+NMODOC
      INIACT=NMOFZC+NMODOC+1
      LASACT=NMOFZC+NMODOC+NMOACT
      INIEXT=NMOFZC+NMODOC+NMOACT+1
      LASEXT=NMOFZC+NMODOC+NMOACT+NMOEXT
C                                                            INITIALIZE
C ----------------------------------------------------------------------
      LM2OP1=LMB2OP
      GEN2PR=.FALSE.
C GEN2PR:
C   .TRUE.  => PARALLEL CALCULATION FOR GEN2 (MAYBE SLOW)
C   .FALSE. => CALCULATE GEN2 AT EACH NODE
      NWFT2=0
      NWFT2P=0
      NWFT2G=0
      DO I=1,NSTATE
         HEFF2(I)=0.0D+00
      END DO
C                                READ FILE LUNFTA ON MEMORY IF POSSIBLE
C ----------------------------------------------------------------------
C
C LM2OP1:
C   0 => SAME AS PREVIOUS VERSION.
C   1 => READ LUNFTA FILE TO ARRAYS KFTAM, LFTAM, WFTAM BEFORE LOOPS.
C   2 => READ LUNFTA, AND PREPARE GEN1WK BEFORE LOOPS.
C
      IF (LM2OP1.EQ.2) THEN
C --- PREPARE GEN1WK ITSELF ---
C KFTAM(4,NWFTAG)
C LFTAM(3,1)
CC WFTAM(INIACT:LASACT,MAXSPF,MAXSPF,NWFTAG)
C WFTAM(MAXSPF,MAXSPF,INIACT:LASACT,NWFTAG)
         NDGEN1=NMOACT*MAXSPF*MAXSPF
         CALL SEQREW(LUNFTA)
         DO K=1,NDGEN1*NWFTAG
            WFTAM(K)=0.0D+00
         END DO
         DO J=1,NWFTAG
            MAXNU=0
 100        READ(LUNFTA) KCNTRA,L3,IOCF2,NWORD
            CALL MQGENR(LUNFTA,3*NWORD,NWORD,LABA2,WORK)
            DO I=1,NWORD
               L4=LABA2(1,I)
               NU=LABA2(2,I)
               MU=LABA2(3,I)
               IF(MAXNU.LT.NU) MAXNU=NU
C               GEN1WK(L4,NU,MU)=WORK(I)
C               WFTAM((J-1)*NDGEN1
C     *              +(MU-1)*NMOACT*MAXSPF
C     *              +(NU-1)*NMOACT
C     *              + L4-INIACT+1)=WORK(I)
               WFTAM((J-1)*NDGEN1
     *              +(L4-INIACT)*MAXSPF*MAXSPF
     *              +(MU-1)*MAXSPF
     *              + NU )=WORK(I)
            END DO
            IF(KCNTRA.EQ.100) GO TO 100
            KFTAM(1,J)=KCNTRA
            KFTAM(2,J)=L3
            KFTAM(3,J)=IOCF2
            KFTAM(4,J)=MAXNU
C            IF(MASWRK) WRITE(LUNOUT,*) 'KCNTRA',KCNTRA
         END DO
      ELSE IF (LM2OP1.EQ.1) THEN
C --- READ LUNFTA TO ARRAYS KFTAM, LFTAM, WFTAM ---
C KFTAM(4,NWFTA)
C LFTAM(3,NWFTAP)
C WFTAM(NWFTAP)
         CALL SEQREW(LUNFTA)
         NWFTAM=1
         DO J=1,NWFTA
C            READ(LUNFTA) KCNTRA,L3,IOCF2,NWORD
            READ(LUNFTA) (KFTAM(I,J),I=1,4)
            NWORD=KFTAM(4,J)
C            CALL MQGENR(LUNFTA,3*NWORD,NWORD,LABA2,WORK)
            CALL MQGENR(LUNFTA,3*NWORD,NWORD,
     *           LFTAM(1,NWFTAM),WFTAM(NWFTAM))
C            KFTAM(1,J)=KCNTRA
C            KFTAM(2,J)=L3
C            KFTAM(3,J)=IOCF2
C            KFTAM(4,J)=NWORD
            NWFTAM=NWFTAM+NWORD
         END DO
      END IF
C
C***********************************************************************
C****                 **************************************************
C**** LOOP FOR STATES **************************************************
C****                 **************************************************
      CALL SEQREW(LUNFT2)
      CALL SEQREW(LUNFT1)
C**** CLEAR ARRAYS *****************************************************
      MAXMU=MAXSPF
      MAXNU=MAXSPF
C***********************************************************************
C**** START ************************************************************
C****       ************************************************************
C**** READ COUPLING CONSTANT ON CSF BASE *******************************
  102 CONTINUE
      DO I=INIACT,LASACT
        DO J=0,NCSFNW
          GEN1(I,J)=0.0D+00
        END DO
      END DO
  120 CONTINUE
        READ(LUNFT1) KCNTR1,L1,NWORD
        CALL MQGENR(LUNFT1,2*NWORD,NWORD,LAB1,WORK)
        DO I=1,NWORD
          L2=LAB1(1,I)
          ICSF3=LAB1(2,I)
          ICSF3N=LOD2NW(ICSF3)
          GEN1(L2,ICSF3N)=WORK(I)
        END DO
      IF(KCNTR1.EQ.10) GO TO 120
C --- REWIND DATA POINTER ---
      IF (LM2OP1.EQ.2) THEN
         JGEN1W=0
      ELSE IF (LM2OP1.EQ.1) THEN
         NKFTAM=1
         NWFTAM=1
      ELSE IF (LM2OP1.EQ.0) THEN
         CALL SEQREW(LUNFTA)
      ENDIF
C --- CLEAR ARRAYS GEN2 AND GEN1WK ---
 122  CONTINUE
C      DO K=0,NCSFNW
C         DO J=INIACT,LASACT
C            DO I=INIACT,LASACT
C               GEN2(I,J,K)=0.0D+00
C            END DO
C         END DO
C      END DO
      DO K=INIACT,LASACT
         DO J=INIACT,LASACT
            DO I=0,NCSFNW
               GEN2(I,J,K)=0.0D+00
            END DO
         END DO
      END DO
  124 CONTINUE
      IF (LM2OP1.NE.2) THEN
C         DO K=1,MAXMU
C            DO J=1,MAXNU
C               DO I=INIACT,LASACT
C                  GEN1WK(I,J,K)=0.0D+00
C               END DO
C            END DO
C         END DO
         DO K=INIACT,LASACT
            DO J=1,MAXMU
               DO I=1,MAXNU
                  GEN1WK(I,J,K)=0.0D+00
               END DO
            END DO
         END DO
      END IF
C --- PREPARE GEN1WK ---
      IF (LM2OP1.EQ.2) THEN
         JGEN1W=JGEN1W+1
         KCNTRA=KFTAM(1,JGEN1W)
         L3    =KFTAM(2,JGEN1W)
         IOCF2 =KFTAM(3,JGEN1W)
         MAXNU =KFTAM(4,JGEN1W)
C         CALL DCOPY(NDGEN1,WFTAM(NDGEN1*(JGEN1W-1)+1),1,GEN1WK,1)
      ELSE
         MAXNU=0
 126     CONTINUE
         IF (LM2OP1.EQ.1) THEN
            KCNTRA=KFTAM(1,NKFTAM)
            L3    =KFTAM(2,NKFTAM)
            IOCF2 =KFTAM(3,NKFTAM)
            NWORD =KFTAM(4,NKFTAM)
            DO I=1,NWORD
               L4=LFTAM(1,NWFTAM+I-1)
               NU=LFTAM(2,NWFTAM+I-1)
               IF(MAXNU.LT.NU) MAXNU=NU
               MU=LFTAM(3,NWFTAM+I-1)
C               GEN1WK(L4,NU,MU)=WFTAM(NWFTAM+I-1)
               GEN1WK(NU,MU,L4)=WFTAM(NWFTAM+I-1)
            END DO
            NKFTAM=NKFTAM+1
            NWFTAM=NWFTAM+NWORD
         ELSE IF (LM2OP1.EQ.0) THEN
            READ(LUNFTA) KCNTRA,L3,IOCF2,NWORD
            CALL MQGENR(LUNFTA,3*NWORD,NWORD,LABA2,WORK)
            DO I=1,NWORD
               L4=LABA2(1,I)
               NU=LABA2(2,I)
               IF(MAXNU.LT.NU) MAXNU=NU
               MU=LABA2(3,I)
C               GEN1WK(L4,NU,MU)=WORK(I)
               GEN1WK(NU,MU,L4)=WORK(I)
            END DO
         END IF
         IF(KCNTRA.EQ.100) GO TO 126
      END IF
      MAXMU=NSNSF(IOCF2+1)-NSNSF(IOCF2)
      NSPF2=MAXMU
      L2=L3
      DO L4=INIACT,LASACT
        IF (GOPARR .AND. GEN2PR
     *             .AND. MOD(L4,NPROC).NE.ME) GO TO 499
        IF(I1EX2(L1,I1EX1(L4,IOCF2)).NE.0) THEN
          DO MU=1,NSPF2
            ICSF2=NSNSF(IOCF2)+MU
            ICSF2N=LOD2NW(ICSF2)
C            GEN2(L2,L4,ICSF2N)=GEN2(L2,L4,ICSF2N)
C            GEN2(L3,L4,ICSF2N)=GEN2(L3,L4,ICSF2N)
            GEN2(ICSF2N,L3,L4)=GEN2(ICSF2N,L3,L4)
     *        -GEN1(L4,ICSF2N)
          END DO
        END IF
C      END DO
C      DO L4=INIACT,LASACT
        JL4G1W=(JGEN1W-1)*NDGEN1+(L4-INIACT)*MAXSPF*MAXSPF
        IOCF3=I1EX2(L3,I1EX1(L4,IOCF2))
        IF(IOCF3.NE.0) THEN
          NSPF3=NSNSF(IOCF3+1)-NSNSF(IOCF3)
          DO L2=INIACT,LASACT
            IF(I1EX2(L1,I1EX1(L2,IOCF3)).NE.0) THEN
              DO MU=1,NSPF2
                ICSF2=NSNSF(IOCF2)+MU
                ICSF2N=LOD2NW(ICSF2)
                DO NU=1,NSPF3
                  ICSF3=NSNSF(IOCF3)+NU
                  ICSF3N=LOD2NW(ICSF3)
                  IF (LM2OP1.NE.2) THEN
C                     GEN2(L2,L4,ICSF2N)=GEN2(L2,L4,ICSF2N)
C     *                    +GEN1(L2,ICSF3N)*GEN1WK(L4,NU,MU)
C                     GEN2(L2,L4,ICSF2N)=GEN2(L2,L4,ICSF2N)
                     GEN2(ICSF2N,L2,L4)=GEN2(ICSF2N,L2,L4)
     *                    +GEN1(L2,ICSF3N)*GEN1WK(NU,MU,L4)
                  ELSE
C                     GEN2(L2,L4,ICSF2N)=GEN2(L2,L4,ICSF2N)
C     *                    +GEN1(L2,ICSF3N)
C     *                    *WFTAM((JGEN1W-1)*NMOACT*MAXSPF*MAXSPF
C     *                          +(MU    -1)*NMOACT*MAXSPF
C     *                          +(NU    -1)*NMOACT
C     *                          + L4-INIACT+1)
C                     GEN2(L2,L4,ICSF2N)=GEN2(L2,L4,ICSF2N)
C     *                    +GEN1(L2,ICSF3N)
C     *                    *WFTAM((JGEN1W-1)*NMOACT*MAXSPF*MAXSPF
C     *                          +(L4-INIACT)*MAXSPF*MAXSPF
C     *                          +(MU    -1)*MAXSPF
C     *                          + NU)
C                     GEN2(L2,L4,ICSF2N)=GEN2(L2,L4,ICSF2N)
                     GEN2(ICSF2N,L2,L4)=GEN2(ICSF2N,L2,L4)
     *                    +GEN1(L2,ICSF3N)
     *                    *WFTAM(JL4G1W+(MU-1)*MAXSPF+NU)
                 END IF
                END DO
              END DO
            END IF
          END DO
        END IF
 499  END DO
C**** IF END OF OCF2, REFRESH GEN1WK AND GO TO 'READ' ******************
      IF(KCNTRA.EQ.10) GO TO 124
C**** IF END OF ORB1, GET <STATE/EIJ/OCF> AND GO TO 'READ' *************
C**** IF END OF STATE, GET <STATE/EIJ/OCS> *****************************
C      ELSE IF(KCNTRA.EQ.1 .OR. KCNTRA.EQ.0) THEN
C --- SYNC GEN2 ---
        IF (GOPARR .AND. GEN2PR) THEN
C           CALL DDI_GSUMF(2502,GEN2,NMOACT*NMOACT*(NCSFNW+1))
           DO I=INIACT,LASACT
              IPROC=MOD(I,NPROC)
              CALL DDI_BCAST(2502,'F',
     *             GEN2(0,INIACT,I),NMOACT*(NCSFNW+1),IPROC)
           END DO
        END IF
C=======================================================================
C==== TWO-BODY GENERATOR ===============================================
C====                    ===============================================
C --- FOR PARALLEL ---
        IPBASE=ME
        IPDIFF=MOD(NOCF,NPROC)
C                                                             MAIN LOOP
        DO JJ=INIACT,LASACT
C          IF (GOPARR .AND. MOD(JJ-INIACT,NPROC).NE.ME) GO TO 599
C        DO JJ=INIACT+ME,LASACT,NPROC
          DO II=INIACT,LASACT
            LP=L1
            LR=L3
            LQ=II
            LS=JJ
C
            LRS=LIJMO(LR,LS)
            LPQ=LIJMO(LP,LQ)
            ISPQRS=ISYM(ISYM(MOSYM(LP),MOSYM(LQ)),
     *                  ISYM(MOSYM(LR),MOSYM(LS)))
            IF(ISPQRS.EQ.1) THEN
C
C              DO KK=1,NOCF
C              DO KK=1+ME,NOCF,NPROC
              DO KK=1+IPBASE,NOCF,NPROC
                NSPINF=NSNSF(KK+1)-NSNSF(KK)
                KLOOP=.FALSE.
                DO JSTATE=1,NSTATE
                  VAL2(JSTATE)=0.0D+00
C                END DO
C                DO JSTATE=1,NSTATE
                  DO LL=1,NSPINF
                    M2=NSNSF(KK)+LL
                    M2N=LOD2NW(M2)
                    IF(KREF(M2)) THEN
                      VAL2(JSTATE)=VAL2(JSTATE)
     *                  +GEN2(M2N,II,JJ)*CASVEC(M2,JSTATE)
C     *                  +GEN2(II,JJ,M2N)*CASVEC(M2,JSTATE)
                    END IF
                  END DO
                  IF(LP.EQ.LR) VAL2(JSTATE)=VAL2(JSTATE)*0.5D+00
C                END DO
C                KLOOP=.FALSE.
C                DO JSTATE=1,NSTATE
                  IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
                END DO
                IF(KLOOP) THEN
                  M2=NSNSF(KK)+NSPINF
                  ESI2=ECONF(M2)-EREF0(ISTATE)
                  S=0.0D+00
                  EP=EORB(LP)
                  EQ=EORB(LQ)
                  ER=EORB(LR)
                  ES=EORB(LS)
C     PQRS
                  EPR=EP+ER
                  EQS=EQ+ES
                  ERS=ER-ES
                  EQR=EQ-ER
                  EPSR=EP-ES+ER
                  EQRS=EQ-ER+ES
C     RSPQ
                  ERP=ER+EP
                  ESQ=ES+EQ
                  EPQ=EP-EQ
                  ESP=ES-EP
                  ERQP=ER-EQ+EP
                  ESPQ=ES-EP+EQ
*VDIR NOVECTOR
                  DO KI=INIDOC,LASDOC
                    EKI2=EORB(KI)-ESI2
                    DELTA=EPSR-EKI2
                    DELTA1=EP-EKI2
                    DELTA2=ERQP-EKI2
                    DELTA3=ER-EKI2
                    S=S
     *                +VONEEL(KI,LQ)*VTWOEL(LIJMO(LP,KI),LRS)
     *                /(DELTA+EDSHFT/DELTA)
     *                +VTWOEL(LIJMO(KI,LQ),LRS)*VONEEL(LP,KI)
     *                /(DELTA1+EDSHFT/DELTA1)
     *                +VONEEL(KI,LS)*VTWOEL(LIJMO(LR,KI),LPQ)
     *                /(DELTA2+EDSHFT/DELTA2)
     *                +VTWOEL(LIJMO(KI,LS),LPQ)*VONEEL(LR,KI)
     *                /(DELTA3+EDSHFT/DELTA3)
                  END DO
*VDIR NOVECTOR
                  DO KE=INIEXT,LASEXT
                    EKE2=EORB(KE)+ESI2
                    DELTA=EKE2-EQRS
                    DELTA1=EKE2-EQ
                    DELTA2=EKE2-ESPQ
                    DELTA3=EKE2-ES
                    S=S
     *                -VONEEL(LP,KE)*VTWOEL(LIJMO(KE,LQ),LRS)
     *                /(DELTA+EDSHFT/DELTA)
     *                -VTWOEL(LIJMO(LP,KE),LRS)*VONEEL(KE,LQ)
     *                /(DELTA1+EDSHFT/DELTA1)
     *                -VONEEL(LR,KE)*VTWOEL(LIJMO(KE,LS),LPQ)
     *                /(DELTA2+EDSHFT/DELTA2)
     *                -VTWOEL(LIJMO(LR,KE),LPQ)*VONEEL(KE,LS)
     *                /(DELTA3+EDSHFT/DELTA3)
                  END DO
                  S1=ZERO
                  DO KJ=INIDOC,LASDOC
                    LJS=LIJMO(KJ,LS)
                    LRJ=LIJMO(LR,KJ)
                    LJQ=LIJMO(KJ,LQ)
                    LPJ=LIJMO(LP,KJ)
                    IS=ISYM(MOSYM(KJ),ISYM(MOSYM(LP),MOSYM(LR)))
*VDIR NOVECTOR
                    DO KI=ISYLAB(1,IS,2),ISYLAB(2,IS,2)
C                   DO KI=INIDOC,LASDOC
                      EKIJ2=EORB(KI)+EORB(KJ)-ESI2
                      DELTA=EPR-EKIJ2
                      DELTA1=ERP-EKIJ2
                      S1=S1- VTWOEL(LIJMO(KI,LQ),LJS)
     *                  *VTWOEL(LIJMO(LP,KI),LRJ)/(DELTA+EDSHFT/DELTA)
     *                  - VTWOEL(LIJMO(KI,LS),LJQ)
     *                  *VTWOEL(LIJMO(LR,KI),LPJ)/(DELTA1+EDSHFT/DELTA1)
                    END DO
                  END DO
                  S=S+S1*0.5D+00
                  S1=ZERO
                  DO KAE=INIACT,LASACT
                    IS=ISYM(MOSYM(KAE),ISYM(MOSYM(LP),MOSYM(LR)))
                    LPAE=LIJMO(LP,KAE)
                    LAEQ=LIJMO(KAE,LQ)
                    LRAE=LIJMO(LR,KAE)
                    LAES=LIJMO(KAE,LS)
*VDIR NOVECTOR
                    DO KF =ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KF =INIEXT,LASEXT
                      EKAEF2=EORB(KAE)+EORB(KF)+ESI2
                      DELTA=EKAEF2-EQS
                      DELTA1=EKAEF2-ESQ
                      S1=S1- VTWOEL(LIJMO(LR,KF),LPAE)
     *                 *VTWOEL(LIJMO(KF,LS),LAEQ)/(DELTA+EDSHFT/DELTA)
     *                 - VTWOEL(LIJMO(LP,KF),LRAE)
     *                 *VTWOEL(LIJMO(KF,LQ),LAES)/(DELTA1+EDSHFT/DELTA1)
                    END DO
                  END DO
                  S=S+S1*0.5D+00
                  S1=ZERO
                  DO KA=INIACT,LASACT
                    LRA=LIJMO(LR,KA)
                    LAS=LIJMO(KA,LS)
                    LPA=LIJMO(LP,KA)
                    LAQ=LIJMO(KA,LQ) 
                    IS=ISYM(MOSYM(KA),ISYM(MOSYM(LP),MOSYM(LR)))
*VDIR NOVECTOR
                    DO KE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KE=INIEXT,LASEXT
                      EKEA2=EORB(KE)+EORB(KA)+ESI2
                      DELTA=EKEA2-EQS
                      DELTA1=EKEA2-ESQ
                      S1=S1- VTWOEL(LIJMO(LP,KE),LRA)
     *                  *VTWOEL(LIJMO(KE,LQ),LAS)/(DELTA+EDSHFT/DELTA)
     *                  - VTWOEL(LIJMO(LR,KE),LPA)
     *                  *VTWOEL(LIJMO(KE,LS),LAQ)/(DELTA1+EDSHFT/DELTA1)
                    END DO
                  END DO
                  S=S+S1*0.5D+00
                  DO KI =INIDOC,LASDOC
                    LRI=LIJMO(LR,KI)
                    LIQ=LIJMO(KI,LQ)
                    LPI=LIJMO(LP,KI)
                    LIS=LIJMO(KI,LS)
                    IS=ISYM(MOSYM(KI),ISYM(MOSYM(LP),MOSYM(LQ)))
*VDIR NOVECTOR
                    DO KAE=ISYLAB(1,IS,3),ISYLAB(2,IS,3)
C                   DO KAE=INIACT,LASACT
                      EKAEI2=EORB(KAE)-EORB(KI)+ESI2
                      DELTA=EKAEI2+ERS
                      DELTA1=EKAEI2+EPQ
                      LAEI=LIJMO(KAE,KI)
                      LIAE=LIJMO(KI,KAE)
                      S=S
     *                  -( VTWOEL(LIAE,LPQ)
     *                  *(2.0D+00*VTWOEL(LAEI,LRS)
     *                  -VTWOEL(LIJMO(KAE,LS),LRI))
     *                  -VTWOEL(LIJMO(LP,KAE),LIQ)*VTWOEL(LAEI,LRS)
     *                  )/(DELTA+EDSHFT/DELTA)
     *                  -( VTWOEL(LIAE,LRS)
     *                  *(2.0D+00*VTWOEL(LAEI,LPQ)
     *                  -VTWOEL(LIJMO(KAE,LQ),LPI))
     *                  -VTWOEL(LIJMO(LR,KAE),LIS)*VTWOEL(LAEI,LPQ)
     *                  )/(DELTA1+EDSHFT/DELTA1)
                     END DO
                  END DO
                  DO KI =INIDOC,LASDOC
                    LRI=LIJMO(LR,KI)
                    LIQ=LIJMO(KI,LQ)
                    LPI=LIJMO(LP,KI)
                    LIS=LIJMO(KI,LS)
                    IS=ISYM(MOSYM(KI),ISYM(MOSYM(LP),MOSYM(LQ)))
                    DO KAE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KAE=INIEXT,LASEXT
                      EKAEI2=EORB(KAE)-EORB(KI)+ESI2
                      DELTA=EKAEI2+ERS
                      DELTA1=EKAEI2+EPQ
                      LAEI=LIJMO(KAE,KI)
                      LIAE=LIJMO(KI,KAE)
                      S=S
     *                  -( VTWOEL(LIAE,LPQ)
     *                  *(2.0D+00*VTWOEL(LAEI,LRS)
     *                  -VTWOEL(LIJMO(KAE,LS),LRI))
     *                  -VTWOEL(LIJMO(LP,KAE),LIQ)*VTWOEL(LAEI,LRS)
     *                  )/(DELTA+EDSHFT/DELTA)
     *                  -( VTWOEL(LIAE,LRS)
     *                  *(2.0D+00*VTWOEL(LAEI,LPQ)
     *                  -VTWOEL(LIJMO(KAE,LQ),LPI))
     *                  -VTWOEL(LIJMO(LR,KAE),LIS)*VTWOEL(LAEI,LPQ)
     *                  )/(DELTA1+EDSHFT/DELTA1)
                    END DO
                  END DO
                  DO KI =INIDOC,LASDOC
                    LRI=LIJMO(LR,KI)
                    LIQ=LIJMO(KI,LQ)
                    LPI=LIJMO(LP,KI)
                    LIS=LIJMO(KI,LS)
                    IS=ISYM(MOSYM(KI),ISYM(MOSYM(LP),MOSYM(LS)))
                    DO KAE=ISYLAB(1,IS,3),ISYLAB(2,IS,3)
C                   DO KAE=INIACT,LASACT
                      EKAEI2=EORB(KAE)-EORB(KI)+ESI2
                      DELTA=EKAEI2-EQR
                      DELTA1=EKAEI2-ESP
                      S=S
     *                 + VTWOEL(LIJMO(LP,KAE),LIS)
     *                 *VTWOEL(LIJMO(KAE,LQ),LRI)/(DELTA+EDSHFT/DELTA)
     *                 + VTWOEL(LIJMO(LR,KAE),LIQ)
     *                 *VTWOEL(LIJMO(KAE,LS),LPI)/(DELTA1+EDSHFT/DELTA1)
                    END DO
                  END DO
                  DO KI =INIDOC,LASDOC
                    LRI=LIJMO(LR,KI)
                    LIQ=LIJMO(KI,LQ)
                    LPI=LIJMO(LP,KI)
                    LIS=LIJMO(KI,LS)
                    IS=ISYM(MOSYM(KI),ISYM(MOSYM(LP),MOSYM(LS)))
                    DO KAE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KAE=INIEXT,LASEXT
                      EKAEI2=EORB(KAE)-EORB(KI)+ESI2
                      DELTA=EKAEI2-EQR
                      DELTA1=EKAEI2-ESP
                      S=S
     *                 + VTWOEL(LIJMO(LP,KAE),LIS)
     *                 *VTWOEL(LIJMO(KAE,LQ),LRI)/(DELTA+EDSHFT/DELTA)
     *                 + VTWOEL(LIJMO(LR,KAE),LIQ)
     *                 *VTWOEL(LIJMO(KAE,LS),LPI)/(DELTA1+EDSHFT/DELTA1)
                    END DO
                  END DO
                  DO JSTATE=1,NSTATE
                    IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
C                      HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
C     *                  +S*VAL2(JSTATE)
                       HEFF2(JSTATE)=HEFF2(JSTATE)+S*VAL2(JSTATE)
                    END IF
                  END DO
                END IF
              END DO
              IPBASE=MOD(IPBASE+IPDIFF,NPROC)
            END IF
         END DO
        END DO
C
C.... WRITE GEN2 .......................................................
        NWRITE=0
        DO I=INIACT,LASACT
          DO J=INIACT,LASACT
            DO K=1,NCSF
              KN=LOD2NW(K)
              IF(KN.NE.0) THEN
C                S=ABS(GEN2(I,J,KN))
                S=ABS(GEN2(KN,I,J))
                IF(S.GT.GENZRO) THEN
                  NWRITE=NWRITE+1
                  IF(NWRITE.GT.LENGTH) THEN
                    KCONTW=10
                    WRITE(LUNFT2) KCONTW,L1,L3,LENGTH
                    CALL MQGENW(LUNFT2,3*LENGTH,LENGTH,LABA2,WORK)
                    NWFT2=NWFT2+1
                    NWFT2P=NWFT2P+LENGTH
                    NWRITE=1
                  END IF
                  LABA2(1,NWRITE)=I
                  LABA2(2,NWRITE)=J
                  LABA2(3,NWRITE)=K
C                  WORK (  NWRITE)=GEN2(I,J,KN)
                  WORK (  NWRITE)=GEN2(KN,I,J)
                END IF
              END IF
            END DO
          END DO
        END DO
C        IF (KCNTRA.EQ.1) THEN
C           KCONTW=1
C        ELSE
C           KCONTW=0
C        END IF
        KCONTW=KCNTRA
        WRITE(LUNFT2) KCONTW,L1,L3,NWRITE
        CALL MQGENW(LUNFT2,3*NWRITE,NWRITE,LABA2,WORK)
        NWFT2=NWFT2+1
        NWFT2G=NWFT2G+1
        NWFT2P=NWFT2P+NWRITE
        IF (KCNTRA.EQ.1) THEN
           IF(L1.EQ.L3) GO TO 102
           GO TO 122
C        ELSE IF(KCNTRA.EQ.0) THEN
        ELSE
           IF(KCNTR1.NE.0) THEN
              IF(MASWRK) WRITE(LUNOUT,
     *          '('' *** ERROR STOP IN SUB.MQLMB2 ***''/
     *          '' GEN2 FILE AND <A/E_IJ/B> FILE ARE INCONSISTENT.''/
     *          '' *** CHECK PROGRAM!'')')
              CALL ABRT
           END IF
        END IF
C**** ERROR STOP *******************************************************
C      ELSE
C       STOP ' *** ERROR STOP IN SUB.MQLMB2.'
C      END IF
      IF(GOPARR) CALL DDI_GSUMF(2501,HEFF2,NSTATE)
      DO JSTATE=1,NSTATE
         HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)+HEFF2(JSTATE)
      END DO
C==== DEBUG OUTPUT =====================================================
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** HEFF(2) <- TWO-BODY GENERATOR ***'')')
        CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
      END IF
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      IF(MASWRK) WRITE(LUNOUT,9100) TIME
 9100 FORMAT(1X,'TIME FOR 2-BODY FORMULAE =',F10.2)
C
      RETURN
      END
C*MODULE MCQDPT  *DECK MQLMB3
      SUBROUTINE MQLMB3(LUNOUT,LPOUT ,LUNFTA,LUNFT2,
     *                  INIACT,LASACT,NCSF  ,NOCF  ,MAXSPF,LENGTH,
     *                  NOCFI , EDSHFT,
     *                  NSNSF ,I1EX1 ,I1EX2 ,IOCSF ,
     *                  GEN1WK,GEN2  ,GEN3  ,
     *                  LABA2 ,WORKR ,VAL2  ,HEFF2 ,
     *                  NMOFZC,NMODOC,NMOACT,NMOEXT,
     *                  ISTATE,NSTATE,NMO   ,NMOEI ,NMOII ,THRGEN,
     *                  HEFF  ,CASVEC,EORB  ,LIJMO ,VTWOEL,
     *                  ECONF ,EREF0 ,KREF  ,MOSYM ,
     *                  NCSFNW,LOD2NW,
     *                  LMB3OP,KFTAM,LFTAM,WFTAM,
     *                  KFT2M,LFT2M,WFT2M)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL   KLOOP
      INTEGER   NSNSF(NOCF+1)
      INTEGER   I1EX1(INIACT:LASACT,0:NOCF)
      INTEGER   I1EX2(INIACT:LASACT,0:NOCFI)
      INTEGER   IOCSF(2,NCSF)
      DIMENSION GEN1WK(MAXSPF,MAXSPF,INIACT:LASACT)
      DIMENSION GEN2(INIACT:LASACT,INIACT:LASACT,0:NCSFNW)
      DIMENSION GEN3(MAXSPF,INIACT:LASACT,INIACT:LASACT,INIACT:LASACT)
      INTEGER   LABA2(3,LENGTH)
      DIMENSION WORKR(LENGTH)
      DIMENSION VAL2(NSTATE),HEFF2(NSTATE)
C
      INTEGER   KFTAM(4,1),LFTAM(3,1)
      DIMENSION WFTAM(1)
      INTEGER   KFT2M(4,1),LFT2M(3,1)
      DIMENSION WFT2M(1)
C
      DIMENSION HEFF  (NSTATE,NSTATE,0:3)
      DIMENSION CASVEC(NCSF,NSTATE), EORB(NMO)
      DIMENSION LIJMO (NMO,NMO)
      DIMENSION VTWOEL(NMOEI,NMOII)
      DIMENSION ECONF (NCSF)    , EREF0 (NSTATE)
      LOGICAL   KREF(NCSF)
      DIMENSION MOSYM(NMO)
      DIMENSION LOD2NW(NCSF)
      DIMENSION ISYM(8,8)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /MQFTC2/NWFTA,NWFTAP,NWFTAG
      COMMON /MQFT2 /NWFT2,NWFT2P,NWFT2G
      COMMON/MQSYLB/ISYLAB(2,8,4)
      DATA ISYM /1,2,3,4,5,6,7,8,
     *           2,1,4,3,6,5,8,7,
     *           3,4,1,2,7,8,5,6,
     *           4,3,2,1,8,7,6,5,
     *           5,6,7,8,1,2,3,4,
     *           6,5,8,7,2,1,4,3,
     *           7,8,5,6,3,4,1,2,
     *           8,7,6,5,4,3,2,1/
C
      CALL TSECND(STIME)
C
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQLMB3 ***'')')
        CALL MQDEBG(LUNOUT,
     *  'LUNFTA','LUNFT2','INIACT','LASACT','  NCSF','  NOCF','MAXSPF',
     *   LUNFTA , LUNFT2 , INIACT , LASACT ,   NCSF ,   NOCF , MAXSPF )
        CALL MQDEBG(LUNOUT,
     *  'LENGTH','NMOFZC','NMODOC','NMOACT','NMOEXT','ISTATE','NSTATE',
     *   LENGTH , NMOFZC , NMODOC , NMOACT , NMOEXT , ISTATE , NSTATE )
        CALL MQDEBG(LUNOUT,
     *  '   NMO',' NMOEI',' NMOII','NCSFNW','     -','     -','LMB3OP',
     *      NMO ,  NMOEI ,  NMOII , NCSFNW ,      0 ,      0 , LMB3OP )
      END IF
C==== SET CONSTANS =====================================================
      INIDOC=NMOFZC+1
      LASDOC=NMOFZC+NMODOC
      INIACT=NMOFZC+NMODOC+1
      LASACT=NMOFZC+NMODOC+NMOACT
C     INIEXT=NMOFZC+NMODOC+NMOACT+1
C     LASEXT=NMOFZC+NMODOC+NMOACT+NMOEXT
C***********************************************************************
C****       ************************************************************
C**** START ************************************************************
C****       ************************************************************
C**** ***
C                                                            INITIALIZE
C ----------------------------------------------------------------------
      LM3OP1=MOD(LMB3OP,4)
      LM3OP2=LMB3OP/4
C
      DO I=1,NSTATE
         HEFF2(I)=0.0D+00
      END DO
C
C      DLB=IBTYP.EQ.1
C      IF (GOPARR) THEN
C         IF (DLB) THEN
C            CALL DDI_DLBRESET()
C            CALL DDI_DLBNEXT( MYTASK )
C         END IF
C
C                                READ FILE LUNFT2 ON MEMORY IF POSSIBLE
C ----------------------------------------------------------------------
C
C LM3OP2:
C   0 => SAME AS PREVIOUS VERSION.
C   1 => READ LUNFT2 FILE TO ARRAYS KFT2M, LFT2M, WFT2M BEFORE LOOPS.
C
      IF (LM3OP2.EQ.1) THEN
C --- READ LUNFTA TO ARRAYS KFTAM, LFTAM, WFTAM ---
C KFTAM(4,NWFT2)
C LFTAM(3,NWFT2P)
C WFTAM(NWFT2P)
         CALL SEQREW(LUNFT2)
         NWFT2M=1
         DO J=1,NWFT2
C            READ(LUNFT2) KCNTR2,L1,L3,NWORD
            READ(LUNFT2) (KFT2M(I,J),I=1,4)
            NWORD=KFT2M(4,J)
C            CALL MQGENR(LUNFT2,3*NWORD,NWORD,LABA2,WORKR)
            CALL MQGENR(LUNFT2,3*NWORD,NWORD,
     *           LFT2M(1,NWFT2M),WFT2M(NWFT2M))
            NWFT2M=NWFT2M+NWORD
         END DO
      END IF
C
C                                READ FILE LUNFTA ON MEMORY IF POSSIBLE
C ----------------------------------------------------------------------
C
C LM3OP1:
C   0 => SAME AS PREVIOUS VERSION.
C   1 => READ LUNFTA FILE TO ARRAYS KFTAM, LFTAM, WFTAM BEFORE LOOPS.
C   2 => READ LUNFTA, AND PREPARE GEN1WK BEFORE LOOPS.
C
      IF (LM3OP1.EQ.2) THEN
C --- PREPARE GEN1WK ITSELF ---
C KFTAM(4,NWFTAG)
C LFTAM(3,1)
CC WFTAM(INIACT:LASACT,MAXSPF,MAXSPF,NWFTAG)
C WFTAM(MAXSPF,MAXSPF,INIACT:LASACT,NWFTAG)
         NDGEN1=NMOACT*MAXSPF*MAXSPF
         CALL SEQREW(LUNFTA)
         DO K=1,NDGEN1*NWFTAG
            WFTAM(K)=0.0D+00
         END DO
         DO J=1,NWFTAG
            MAXNU=0
 100        READ(LUNFTA) KCNTRA,L5,IOCF2,NWORD
            CALL MQGENR(LUNFTA,3*NWORD,NWORD,LABA2,WORKR)
            DO I=1,NWORD
               L6=LABA2(1,I)
               NU=LABA2(2,I)
               MU=LABA2(3,I)
               IF(MAXNU.LT.NU) MAXNU=NU
               WFTAM((J-1)*NDGEN1
     *              +(L6-INIACT)*MAXSPF*MAXSPF
     *              +(NU-1)*MAXSPF
     *              + MU )=WORKR(I)
            END DO
            IF(KCNTRA.EQ.100) GO TO 100
            KFTAM(1,J)=KCNTRA
            KFTAM(2,J)=L5
            KFTAM(3,J)=IOCF2
            KFTAM(4,J)=MAXNU
C            IF(MASWRK) WRITE(LUNOUT,*) 'KCNTRA',KCNTRA
         END DO
      ELSE IF (LM3OP1.EQ.1) THEN
C --- READ LUNFTA TO ARRAYS KFTAM, LFTAM, WFTAM ---
C KFTAM(4,NWFTA)
C LFTAM(3,NWFTAP)
C WFTAM(NWFTAP)
         CALL SEQREW(LUNFTA)
         NWFTAM=1
         DO J=1,NWFTA
C            READ(LUNFTA) KCNTRA,L5,IOCF2,NWORD
            READ(LUNFTA) (KFTAM(I,J),I=1,4)
            NWORD=KFTAM(4,J)
C            CALL MQGENR(LUNFTA,3*NWORD,NWORD,LABA2,WORKR)
            CALL MQGENR(LUNFTA,3*NWORD,NWORD,
     *           LFTAM(1,NWFTAM),WFTAM(NWFTAM))
C            KFTAM(1,J)=KCNTRA
C            KFTAM(2,J)=L5
C            KFTAM(3,J)=IOCF2
C            KFTAM(4,J)=NWORD
            NWFTAM=NWFTAM+NWORD
         END DO
      END IF
C
      IF(LM3OP2.EQ.1) THEN
         NKFT2M=1
         NWFT2M=1
      ELSE IF(LM3OP2.EQ.0) THEN
         CALL SEQREW(LUNFT2)
      END IF
C      IF(MASWRK) WRITE(LUNOUT,*) 'KCNTR2=',KCNTR2
C
C**** CLEAR ARRAYS *****************************************************
      MAXMU=MAXSPF
      MAXNU=MAXSPF
      IF(LM3OP1.NE.2) CALL VCLR(GEN1WK,1,MAXSPF**2*NMOACT)
      CALL VCLR(GEN3,1,MAXSPF*NMOACT**3)
C --- FOR PARALLEL ---
      IF (GOPARR) THEN
         IPBASE=0
         IPDIFF=MOD(NMOACT,NPROC)
      END IF
C                                                             MAIN LOOP
C ----------------------------------------------------------------------
C**** READ 2-BODY COUPLING CONSTANT ************************************
 200  CONTINUE
C --- CLEAR GEN2 BEFORE PREPARING GEN2 ---
      CALL VCLR(GEN2,1,NMOACT**2*(NCSFNW+1))
 210    CONTINUE
C --- PREPARE GEN2 ---
        IF (LM3OP2.EQ.1) THEN
            KCNTR2=KFT2M(1,NKFT2M)
            L1    =KFT2M(2,NKFT2M)
            L3    =KFT2M(3,NKFT2M)
            NWORD =KFT2M(4,NKFT2M)
            DO I=1,NWORD
               L2   =LFT2M(1,NWFT2M+I-1)
               L4   =LFT2M(2,NWFT2M+I-1)
               ICSF3=LFT2M(3,NWFT2M+I-1)
               IOCF3=IOCSF(1,ICSF3)
               ICSF3N=LOD2NW(ICSF3)
               GEN2(L2,L4,ICSF3N)=WFT2M(NWFT2M+I-1)
            END DO
            NKFT2M=NKFT2M+1
            NWFT2M=NWFT2M+NWORD
        ELSE IF (LM3OP2.EQ.0) THEN
           READ(LUNFT2) KCNTR2,L1,L3,NWORD
           CALL MQGENR(LUNFT2,3*NWORD,NWORD,LABA2,WORKR)
           DO I=1,NWORD
              L2=LABA2(1,I)
              L4=LABA2(2,I)
              ICSF3=LABA2(3,I)
              IOCF3=IOCSF(1,ICSF3)
              ICSF3N=LOD2NW(ICSF3)
              GEN2(L2,L4,ICSF3N)=WORKR(I)
           END DO
        END IF
      IF(KCNTR2.EQ.10) GO TO 210
C      IF(MASWRK) WRITE(LUNOUT,*) 'KCNTR2=',KCNTR2
C**** READ 1-BODY COUPLING CONSTANT ************************************
C --- REWIND DATA POINTER ---
      IF(LM3OP1.EQ.2) THEN
         JGEN1W=0
      ELSE IF(LM3OP1.EQ.1) THEN
         NKFTAM=1
         NWFTAM=1
      ELSE IF(LM3OP1.EQ.0) THEN
         CALL SEQREW(LUNFTA)
      END IF
  300 CONTINUE
C --- CLEAR GEN1WK AND GEN3 BEFORE PREPARING GEN1WK ---
      IF(LM3OP1.NE.2)THEN
         DO K=INIACT,LASACT
            DO J=1,MAXNU
               DO I=1,MAXMU
                  GEN1WK(I,J,K)=0.0D+00
               END DO
            END DO
         END DO
      ENDIF
      DO K=INIACT,LASACT
         IF (.NOT.GOPARR .OR. MOD(K+IPBASE,NPROC).EQ.ME) THEN
            DO J=INIACT,LASACT
               DO I=INIACT,LASACT
                  DO MU=1,MAXMU
                     GEN3(MU,I,J,K)=0.0D+00
                  END DO
               END DO
            END DO
         END IF
      END DO
      IF(GOPARR) IPBASE=IPBASE+IPDIFF
C --- PREPARE GEN1WK ---
      IF (LM3OP1.EQ.2) THEN
         JGEN1W=JGEN1W+1
         KCNTRA=KFTAM(1,JGEN1W)
         L5    =KFTAM(2,JGEN1W)
         IOCF2 =KFTAM(3,JGEN1W)
         MAXNU =KFTAM(4,JGEN1W)
C         CALL DCOPY(NDGEN1,WFTAM(NDGEN1*(JGEN1W-1)+1),1,GEN1WK,1)
      ELSE
         MAXNU=0
 350     CONTINUE
         IF (LM3OP1.EQ.1) THEN
            KCNTRA=KFTAM(1,NKFTAM)
            L5    =KFTAM(2,NKFTAM)
            IOCF2 =KFTAM(3,NKFTAM)
            NWORD =KFTAM(4,NKFTAM)
            DO I=1,NWORD
               L6=LFTAM(1,NWFTAM+I-1)
               NU=LFTAM(2,NWFTAM+I-1)
               IF(MAXNU.LT.NU) MAXNU=NU
               MU=LFTAM(3,NWFTAM+I-1)
               GEN1WK(MU,NU,L6)=WFTAM(NWFTAM+I-1)
            END DO
            NKFTAM=NKFTAM+1
            NWFTAM=NWFTAM+NWORD
         ELSE IF (LM3OP1.EQ.0) THEN
            READ(LUNFTA) KCNTRA,L5,IOCF2,NWORD
            CALL MQGENR(LUNFTA,3*NWORD,NWORD,LABA2,WORKR)
            DO I=1,NWORD
               L6=LABA2(1,I)
               NU=LABA2(2,I)
               IF(MAXNU.LT.NU) MAXNU=NU
               MU=LABA2(3,I)
               GEN1WK(MU,NU,L6)=WORKR(I)
            END DO
         ENDIF
         IF(KCNTRA.EQ.100) GO TO 350
      END IF
      MAXMU=NSNSF(IOCF2+1)-NSNSF(IOCF2)
C
      IF(L1.NE.L5) THEN
C--------------------------------------------------------- IF (L1.NE.L5)
C
C**** CALCULATE 3-BODY COUPLING CONSTANT *******************************
C
      NSPF2=MAXMU
C
      IS13=ISYM(MOSYM(L1),MOSYM(L3))
      IS135=ISYM(IS13,MOSYM(L5))
C
      DO L6=INIACT,LASACT
        IF (.NOT.GOPARR .OR. MOD(L6+IPBASE,NPROC).EQ.ME) THEN
C-------------------------------------------------- IF (IT IS MY JOB) --
C
        JL6G1W=(JGEN1W-1)*NDGEN1+(L6-INIACT)*MAXSPF*MAXSPF
C
        IS6=ISYM(IS135,MOSYM(L6))
C
        IOCF3=I1EX2(L5,I1EX1(L6,IOCF2))
        IF(IOCF3.NE.0) THEN
          NSPF3=NSNSF(IOCF3+1)-NSNSF(IOCF3)
          DO L4=INIACT,LASACT
C
            IS64=ISYM(IS6,MOSYM(L4))
C
            DO L2=INIACT,LASACT
C
              IS642=ISYM(IS64,MOSYM(L2))
              IF(IS642.EQ.1) THEN
C
                DO NU=1,NSPF3
                  ICSF3=NSNSF(IOCF3)+NU
                  ICSF3N=LOD2NW(ICSF3)
                  GEN2L24=GEN2(L2,L4,ICSF3N)
                  IF(GEN2L24.NE.0.0D+00) THEN
                    DO MU=1,NSPF2
                      IF(KREF(NSNSF(IOCF2)+MU)) THEN
                        IF(LM3OP1.NE.2) THEN
                          GEN3(MU,L2,L4,L6)=GEN3(MU,L2,L4,L6)
     *                      +GEN2L24*GEN1WK(MU,NU,L6)
C    *                      +GEN2(L2,L4,ICSF3N)*GEN1WK(MU,NU,L6)
                        ELSE
                          GEN3(MU,L2,L4,L6)=GEN3(MU,L2,L4,L6)
     *                      +GEN2L24*WFTAM(JL6G1W+(NU-1)*MAXSPF+MU)
C    *                      +GEN2(L2,L4,ICSF3N)
C    *                      *WFTAM(JL6G1W+(NU-1)*MAXSPF+MU)
                        END IF
                      END IF
                    END DO
                  END IF
                END DO
C
              END IF
C
            END DO
          END DO
        END IF
C      END DO
C      DO L6=INIACT,LASACT
C
        IS6=ISYM(IS13,MOSYM(L6))
C
        DO L24=INIACT,LASACT
C
          IS642=ISYM(IS6,MOSYM(L24))
          IF(IS642.EQ.1) THEN
C
            DO MU=1,NSPF2
              ICSF2=NSNSF(IOCF2)+MU
              IF(KREF(ICSF2)) THEN
                GEN3(MU,L5 ,L24,L6)=GEN3(MU,L5 ,L24,L6)
     *            -GEN2(L6 ,L24,LOD2NW(ICSF2))
              END IF
            END DO
            DO MU=1,NSPF2
              ICSF2=NSNSF(IOCF2)+MU
              IF(KREF(ICSF2)) THEN
                GEN3(MU,L24,L5 ,L6)=GEN3(MU,L24,L5 ,L6)
     *            -GEN2(L24,L6 ,LOD2NW(ICSF2))
              END IF
            END DO
C
          END IF
C
        END DO
C      END DO
C=======================================================================
C==== THREE-BODY GENERATOR =============================================
C====                      =============================================
C      DO KK=INIACT,LASACT
        KK=L6
        DO JJ=INIACT,LASACT
          DO II=INIACT,LASACT
            LP=L1
            LR=L3
            LT=L5
            LQ=II
            LS=JJ
            LU=KK
            LRS=LIJMO(LR,LS)
            LTU=LIJMO(LT,LU) 
            LPQ=LIJMO(LP,LQ)
C            IF(LP.NE.LT) THEN
C
              IS=ISYM(MOSYM(LP),MOSYM(LQ))
              IS=ISYM(IS,ISYM(MOSYM(LR),MOSYM(LS)))
              IS=ISYM(IS,ISYM(MOSYM(LT),MOSYM(LU)))
              IF(IS.EQ.1) THEN
C
                KLOOP=.FALSE.
                DO JSTATE=1,NSTATE
                  VAL2(JSTATE)=0.0D+00
C                END DO
C                DO JSTATE=1,NSTATE
                  DO LL=1,NSPF2
                    M2=NSNSF(IOCF2)+LL
                    IF(KREF(M2)) THEN
                      VAL2(JSTATE)=VAL2(JSTATE)
     *                  +GEN3(LL,II,JJ,KK)*CASVEC(M2,JSTATE)
                    END IF
                  END DO
                  IF(LP.EQ.LR .OR. LR.EQ.LT) THEN
                    VAL2(JSTATE)=VAL2(JSTATE)*0.5D+00
                  END IF
C                END DO
C                KLOOP=.FALSE.
C                DO JSTATE=1,NSTATE
                  IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
                END DO
                IF(KLOOP) THEN
                  M2=NSNSF(IOCF2)+NSPF2
                  ESI2=ECONF(M2)-EREF0(ISTATE)
                  S=0.0D+00
                  EP  =EORB(LP)
                  EQ  =EORB(LQ)
                  ER  =EORB(LR)
                  ES  =EORB(LS)
                  ET  =EORB(LT)
                  EU  =EORB(LU)
C1    PQRSTU
                  EPTU=EP+ET-EU
                  EQTU=EQ-ET+EU
C2    PQTURS
                  EPRS=EP+ER-ES
                  EQRS=EQ-ER+ES
C3    RSPQTU
                  ERTU=ER+ET-EU
                  ESTU=ES-ET+EU
C4    RSTUPQ
                  ERPQ=ER+EP-EQ
                  ESPQ=ES-EP+EQ
C5    TUPQRS
                  ETRS=ET+ER-ES
                  EURS=EU-ER+ES
C6    TURSPQ
                  ETPQ=ET+EP-EQ
                  EUPQ=EU-EP+EQ
                  DO KI=INIDOC,LASDOC
                    EKI2=EORB(KI)-ESI2
                    DELTA=EPTU-EKI2
                    DELTA1=EPRS-EKI2
                    DELTA2=ERTU-EKI2
                    DELTA3=ERPQ-EKI2
                    DELTA4=ETRS-EKI2
                    DELTA5=ETPQ-EKI2
                    LIQ=LIJMO(KI,LQ)
                    LIS=LIJMO(KI,LS)
                    LIU=LIJMO(KI,LU)
                    LPI=LIJMO(LP,KI)
                    LRI=LIJMO(LR,KI)
                    LTI=LIJMO(LT,KI) 
                    S=S+ VTWOEL(LIQ,LRS)*VTWOEL(LPI,LTU)
     *                  /(DELTA+EDSHFT/DELTA)
     *                 + VTWOEL(LIQ,LTU)*VTWOEL(LPI,LRS)
     *                  /(DELTA1+EDSHFT/DELTA1)
     *                 + VTWOEL(LIS,LPQ)*VTWOEL(LRI,LTU)
     *                  /(DELTA2+EDSHFT/DELTA2)
     *                 + VTWOEL(LIS,LTU)*VTWOEL(LRI,LPQ)
     *                  /(DELTA3+EDSHFT/DELTA3)
     *                 + VTWOEL(LIU,LPQ)*VTWOEL(LTI,LRS)
     *                  /(DELTA4+EDSHFT/DELTA4)
     *                 + VTWOEL(LIU,LRS)*VTWOEL(LTI,LPQ)
     *                  /(DELTA5+EDSHFT/DELTA5)
                  END DO
C
                  IS=ISYM(MOSYM(LP),ISYM(MOSYM(LR),MOSYM(LS)))
                  DO KE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                 DO KE=INIEXT,LASEXT
                    EKE2=EORB(KE)+ESI2
                    DELTA=EKE2-EQTU
                    S=S-VTWOEL(LIJMO(LP,KE),LRS)
     *                 *VTWOEL(LIJMO(KE,LQ),LTU)
     *                 /(DELTA+EDSHFT/DELTA)
                  END DO
                  IS=ISYM(MOSYM(LP),ISYM(MOSYM(LT),MOSYM(LU)))
                  DO KE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                  DO KE=INIEXT,LASEXT
                    EKE2=EORB(KE)+ESI2
                    DELTA=EKE2-EQRS
                    S=S-VTWOEL(LIJMO(LP,KE),LTU)
     *                 *VTWOEL(LIJMO(KE,LQ),LRS)
     *                 /(DELTA+EDSHFT/DELTA)
                  END DO
                  IS=ISYM(MOSYM(LP),ISYM(MOSYM(LQ),MOSYM(LR)))
                  DO KE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                 DO KE=INIEXT,LASEXT
                    EKE2=EORB(KE)+ESI2
                    DELTA=EKE2-ESTU
                    S=S-VTWOEL(LIJMO(LR,KE),LPQ)
     *                 *VTWOEL(LIJMO(KE,LS),LTU)
     *                 /(DELTA+EDSHFT/DELTA)
                  END DO
                  IS=ISYM(MOSYM(LP),ISYM(MOSYM(LQ),MOSYM(LS)))
                  DO KE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                 DO KE=INIEXT,LASEXT
                    EKE2=EORB(KE)+ESI2
                    DELTA=EKE2-ESPQ
                    S=S-VTWOEL(LIJMO(LR,KE),LTU)
     *                 *VTWOEL(LIJMO(KE,LS),LPQ)
     *                 /(DELTA+EDSHFT/DELTA)
                  END DO
                  IS=ISYM(MOSYM(LP),ISYM(MOSYM(LQ),MOSYM(LT)))
                  DO KE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                 DO KE=INIEXT,LASEXT
                    EKE2=EORB(KE)+ESI2
                    DELTA=EKE2-EURS
                    S=S-VTWOEL(LIJMO(LT,KE),LPQ)
     *                 *VTWOEL(LIJMO(KE,LU),LRS)
     *                 /(DELTA+EDSHFT/DELTA)
                  END DO
                  IS=ISYM(MOSYM(LP),ISYM(MOSYM(LQ),MOSYM(LU)))
                  DO KE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                 DO KE=INIEXT,LASEXT
                    EKE2=EORB(KE)+ESI2
                    DELTA=EKE2-EUPQ
                    S=S-VTWOEL(LIJMO(LT,KE),LRS)
     *                 *VTWOEL(LIJMO(KE,LU),LPQ)
     *                 /(DELTA+EDSHFT/DELTA)
                  END DO
                  DO JSTATE=1,NSTATE
                    IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
C                      HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
C     *                  +S*VAL2(JSTATE)
                       HEFF2(JSTATE)=HEFF2(JSTATE)+S*VAL2(JSTATE)
                    END IF
                  END DO
                END IF
              END IF
C            END IF
          END DO
        END DO
C---------------------------------------------- END IF (IT IS MY JOB) --
        END IF
      END DO
C**** IF END OF OCF2-GROUP, ... ****************************************
C----------------------------------------------------- END IF (L1.NE.L5)
      END IF
      IF(KCNTRA.EQ.10) THEN
        GO TO 300
C**** IF END OF L5-GROUP, ... ******************************************
      ELSE IF(KCNTRA.EQ.1) THEN
        IF(L3.EQ.L5) THEN
          GO TO 200
        END IF
        GO TO 300
C**** IF END OF 1-BODY COUPLING CONSTANT FILE **************************
      ELSE
        IF(KCNTR2.EQ.0) THEN
          GO TO 699
        ELSE
          GO TO 200
        END IF
      END IF
 699  CONTINUE
C------------------------------------------------------------- DDI_GSUMF
      IF(GOPARR) CALL DDI_GSUMF(2500,HEFF2,NSTATE)
      DO JSTATE=1,NSTATE
         HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)+HEFF2(JSTATE)
      END DO
C==== DEBUG OUTPUT =====================================================
C
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** HEFF(2) <- THREE-BODY GENERATOR ***'')')
        CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
      END IF
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      IF(MASWRK) WRITE(LUNOUT,9100) TIME
 9100 FORMAT(1X,'TIME FOR 3-BODY FORMULAE =',F10.2)
C
      RETURN
      END
C*MODULE MCQDPT  *DECK MQLMBR
      SUBROUTINE MQLMBR(LUNOUT,LPOUT ,LUNTWO,LUNFT1,LUNFT2,
     *                  NMOFZC,NMODOC,NMOACT,NMOEXT,NMO   ,
     *                  INIDOC,LASDOC,INIACT,LASACT,INIEXT,LASEXT,
     *                  NCSF  ,NSTATE,LENGTH,ISTATE,THRGEN,
     *                  ISTART,IEND  ,
     *                  IROT  ,EDSHFT,DTWOEL,IDMWTH,
     *                  HEFF  ,CASVEC,EORB  ,ECONF ,
     *                  EREF0 ,LABEL1,LABEL2,WORK  ,KREF  ,IOCSF ,
     *                  MOSYM ,VAL2  ,HEFF2 ,V2READ,VTWOEL)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***********************************************************************
C****                         ******************************************
C****   ARRAYS AND CONSTANT   ******************************************
C****                         ******************************************
      LOGICAL   KLOOP
      DIMENSION HEFF  (NSTATE,NSTATE,0:3)
      DIMENSION CASVEC(NCSF,NSTATE), EORB(NMO)
      DIMENSION ECONF (NCSF)    , EREF0 (NSTATE)
      INTEGER   LABEL1(2,LENGTH), LABEL2(3,LENGTH)
      DIMENSION WORK  (  LENGTH)
      LOGICAL   KREF(NCSF)
      INTEGER   IOCSF(2,NCSF)
      DIMENSION MOSYM(NMO)
      DIMENSION VAL2(NSTATE),HEFF2(NSTATE,2:3)
C.... MODIFIED ON 9/9/01
C
C (IJ|KL) -> (JL|IK)
C
      DIMENSION V2READ(INIDOC:LASACT,INIEXT:LASEXT,
     *                 INIDOC:LASACT)
      DIMENSION VTWOEL(INIEXT:LASEXT,ISTART:IEND  ,
     *                 INIDOC:LASACT,INIDOC:LASACT)
C     DIMENSION VTWOEL(INIDOC:LASACT,INIEXT:LASEXT,
C    *                 INIDOC:LASACT,ISTART:IEND  )
      INTEGER   DTWOEL
      DIMENSION ISYM(8,8)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON/MQSYLB/ISYLAB(2,8,4)
      DATA ISYM /1,2,3,4,5,6,7,8,
     *           2,1,4,3,6,5,8,7,
     *           3,4,1,2,7,8,5,6,
     *           4,3,2,1,8,7,6,5,
     *           5,6,7,8,1,2,3,4,
     *           6,5,8,7,2,1,4,3,
     *           7,8,5,6,3,4,1,2,
     *           8,7,6,5,4,3,2,1/
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQLMBR ***'')')
        CALL MQDEBG(LUNOUT,
     *  'LUNTWO','LUNFT1','LUNFT2','NMOFZC','NMODOC','NMOACT','NMOEXT',
     *   LUNTWO , LUNFT1 , LUNFT2 , NMOFZC , NMODOC , NMOACT , NMOEXT )
        CALL MQDEBG(LUNOUT,
     *  '   NMO','INIDOC','LASDOC','INIACT','LASACT','INIEXT','LASEXT',
     *      NMO , INIDOC , LASDOC , INIACT , LASACT , INIEXT , LASEXT )
        CALL MQDEBG(LUNOUT,
     *  '  NCSF','NSTATE','LENGTH','ISTATE','ISTART','  IEND','IDMWTH',
     *     NCSF , NSTATE , LENGTH , ISTATE , ISTART ,   IEND , IDMWTH )
      END IF
C                                                            INITIALIZE
C ----------------------------------------------------------------------
C
C      IF (IDMWTH.LT.NMOEXT) THEN
C         NPPEXT=NPROC
C         IME=ME
C      ELSE
         NPPEXT=1
         IME=0
C      END IF
C
      DO I=1,NSTATE
         HEFF2(I,2)=0.0D+00
         HEFF2(I,3)=0.0D+00
      END DO
      IPBASE=-1
C
C**** READ TWO-ELECTRON INTEGRALS **************************************
      NMODA =LASACT-INIDOC+1
      NDSIZE=NMODA*NMOEXT
      IF (IDMWTH.LT.NMOEXT) THEN
C.... MODIFIED ON 9/9/01
         DO J=ISTART,IEND
            DO I=INIDOC,LASACT
               CALL MQMATR(LUNTWO,NDSIZE,V2READ(INIDOC,INIEXT,I))
C              CALL MQMATR(LUNTWO,NDSIZE,VTWOEL(INIDOC,INIEXT,I,J))
            END DO
            DO I=INIDOC,LASACT
               DO II=INIDOC,LASACT
                  DO JJ=INIEXT,LASEXT
                     VTWOEL(JJ,J,II,I)=V2READ(II,JJ,I)
                  END DO
               END DO
            END DO
         END DO
      ELSE
C THIS CODE REQUIRES MUCH DDI MEMORIES.
C         JMIS=ISTART-INIEXT+1
C         JMIE=IEND  -INIEXT+1
C         CALL DDI_GET(DTWOEL,1,NDSIZE,(JMIS-1)*NMODA+1,JMIE*NMODA,
C     *        VTWOEL(INIDOC,INIEXT,INIDOC,ISTART))
C ******************************************************************
C MODIFIED ON 9/9/01
C MODIFIED CODE BELOW IS FOR AVOIDING CACHE MISSING.  THE ABOVE CODE
C COMMENTED IS NOT CORRECT ANY MORE DUE TO THIS MODIFICATION.
C ******************************************************************
C THIS CODE REQUIRES SMALLER DDI MEMORIES THAN ABOVE, BUT CAUSES
C MANY DDI_GET CALLS.
         DO J=ISTART,IEND
            JMI=J-INIEXT+1
            CALL DDI_GET(DTWOEL,1,NDSIZE,
     *        (JMI-1)*NMODA+1,JMI*NMODA,V2READ(INIDOC,INIEXT,INIDOC)  )
C           CALL DDI_GET(DTWOEL,1,NDSIZE,
C    *        (JMI-1)*NMODA+1,JMI*NMODA,VTWOEL(INIDOC,INIEXT,INIDOC,J))
            DO KK=INIDOC,LASACT
               DO II=INIDOC,LASACT
                  DO JJ=INIEXT,LASEXT
                     VTWOEL(JJ,J,II,KK)=V2READ(II,JJ,KK)
                  END DO
               END DO
            END DO
         END DO
      END IF
C
C
C***********************************************************************
C****              *****************************************************
C**** SECOND ORDER *****************************************************
C****              *****************************************************
      CALL SEQREW(LUNFT1)
      CALL SEQREW(LUNFT2)
C***********************************************************************
C**** ZERO-BODY GENERATOR (GET ECORE) **********************************
C****                                 **********************************
C
      CALL TSECND(STIME)
C
C.... MODIFIED ON 9/9/01
      IF(IROT.EQ.0) THEN
        DO I=1+IME,NCSF,NPPEXT
C          IF (GOPARR .AND. MOD(I,NPPEXT).NE.IME) GO TO 149
          IF(KREF(I)) THEN
            KLOOP=.FALSE.
            DO JSTATE=1,NSTATE
              VAL2(JSTATE)=CASVEC(I,ISTATE)*CASVEC(I,JSTATE)
              IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
            END DO
            IF(KLOOP) THEN
              ESI=ECONF(I)-EREF0(ISTATE)
              S=0.0D+00
              DO KJ =INIDOC,LASDOC
                DO KI =INIDOC,LASDOC
                  DO KBF=ISTART,IEND
                    DELTA0=-EORB(KI)+EORB(KBF)-EORB(KJ)+ESI
                    IS=ISYM(MOSYM(KBF),ISYM(MOSYM(KI),MOSYM(KJ)))
                    DO KAE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KAE=INIEXT,LASEXT
                      DELTA=EORB(KAE)+DELTA0
C                     DELTA=EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)+ESI
                      S=S-VTWOEL(KAE,KBF,KI,KJ)
     *                   *(2.0D+00*VTWOEL(KAE,KBF,KI,KJ)
     *                   -VTWOEL(KAE,KBF,KJ,KI)
     *                    )/(DELTA+EDSHFT/DELTA)
                    END DO
                  END DO
                END DO
              END DO
C             DO KBF=ISTART,IEND
C             DO KAE=INIEXT,LASEXT
C             DO KJ =INIDOC,LASDOC
C             DO KI =INIDOC,LASDOC
C               S=S-VTWOEL(KI,KAE,KJ,KBF)
C    *            *(2.0D+00*VTWOEL(KI,KAE,KJ,KBF)
C    *            -VTWOEL(KJ,KAE,KI,KBF)
C    *            )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)
C    *            +ESI)
              DO JSTATE=1,NSTATE
                IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
C                  HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
C     *              +S*VAL2(JSTATE)
                  HEFF2(JSTATE,2)=HEFF2(JSTATE,2)+S*VAL2(JSTATE)
                END IF
              END DO
            END IF
          END IF
        END DO
      ELSE IF(IROT.EQ.1) THEN
        DO I=1+IME,NCSF,NPPEXT
C          IF (GOPARR .AND. MOD(I,NPPEXT).NE.IME) GO TO 199
          IF(KREF(I)) THEN
            KLOOP=.FALSE.
            DO JSTATE=1,NSTATE
              VAL2(JSTATE)=CASVEC(I,ISTATE)*CASVEC(I,JSTATE)
              IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
            END DO
            IF(KLOOP) THEN
              ESI=0.0D+00 
              S=0.0D+00
              DO KJ =INIDOC,LASDOC
                DO KI =INIDOC,LASDOC
                  DO KBF=ISTART,IEND
                    IS=ISYM(MOSYM(KBF),ISYM(MOSYM(KI),MOSYM(KJ)))
                    DELTA0=-EORB(KI)+EORB(KBF)-EORB(KJ) + ESI
                    DO KAE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KAE=INIEXT,LASEXT
C                     DELTA=EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ) + ESI
                      DELTA=EORB(KAE)+DELTA0
                      S=S-VTWOEL(KAE,KBF,KI,KJ)
     *                   *(2.0D+00*VTWOEL(KAE,KBF,KI,KJ)
     *                    -VTWOEL(KAE,KBF,KJ,KI)
     *                    )/(DELTA+EDSHFT/DELTA)
                    END DO
                  END DO
                END DO
              END DO
C             DO KBF=ISTART,IEND
C             DO KAE=INIEXT,LASEXT
C             DO KJ =INIDOC,LASDOC
C             DO KI =INIDOC,LASDOC
C               S=S-VTWOEL(KI,KAE,KJ,KBF)
C    *            *(2.0D+00*VTWOEL(KI,KAE,KJ,KBF)
C    *            -VTWOEL(KJ,KAE,KI,KBF)
C    *            )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ) + ESI)
              DO JSTATE=1,NSTATE
                IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
C                  HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
C     *              +S*VAL2(JSTATE)
                  HEFF2(JSTATE,2)=HEFF2(JSTATE,2)+S*VAL2(JSTATE)
                END IF
              END DO
            END IF
          END IF
        END DO
      ELSE
        DO I=1+IME,NCSF,NPPEXT
C          IF (GOPARR .AND. MOD(I,NPPEXT).NE.IME) GO TO 999
          IF(KREF(I)) THEN
            KLOOP=.FALSE.
            DO JSTATE=1,NSTATE
              VAL2(JSTATE)=CASVEC(I,ISTATE)*CASVEC(I,JSTATE)
              IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
            END DO
            IF(KLOOP) THEN
              S=0.0D+00
              T=0.0D+00
              ESIS=ECONF(I)-EREF0(ISTATE)
              ESIT=0.0D+00
              DO KJ =INIDOC,LASDOC
                DO KI =INIDOC,LASDOC
                  DO KBF=ISTART,IEND
                    DELTA01=-EORB(KI)+EORB(KBF)-EORB(KJ) + ESIS
                    DELTA02=-EORB(KI)+EORB(KBF)-EORB(KJ) + ESIT
                    IS=ISYM(MOSYM(KBF),ISYM(MOSYM(KI),MOSYM(KJ)))
                    DO KAE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KAE=INIEXT,LASEXT
                      DELTA1=EORB(KAE)+DELTA01
                      DELTA2=EORB(KAE)+DELTA02
                      VAEBFIJ=VTWOEL(KAE,KBF,KI,KJ)
                      ST=VAEBFIJ*(2.0D+00*VAEBFIJ-VTWOEL(KAE,KBF,KJ,KI))
                      S=S-ST/(DELTA1+EDSHFT/DELTA1)
                      T=T-ST/(DELTA2+EDSHFT/DELTA2)
                    END DO
                  END DO
                END DO
              END DO
C             DO KBF=ISTART,IEND
C             DO KAE=INIEXT,LASEXT
C             DO KJ =INIDOC,LASDOC
C             DO KI =INIDOC,LASDOC
C               S=S-VTWOEL(KI,KAE,KJ,KBF)
C    *            *(2.0D+00*VTWOEL(KI,KAE,KJ,KBF)
C    *            -VTWOEL(KJ,KAE,KI,KBF)
C    *            )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ) + ESIS)
C               T=T-VTWOEL(KI,KAE,KJ,KBF)
C    *            *(2.0D+00*VTWOEL(KI,KAE,KJ,KBF)
C    *            -VTWOEL(KJ,KAE,KI,KBF)
C    *            )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ) + ESIT)
              DO JSTATE=1,NSTATE
                IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
C                  HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
C     *              +S*VAL2(JSTATE)
                  HEFF2(JSTATE,2)=HEFF2(JSTATE,2)+S*VAL2(JSTATE)
C                  HEFF(ISTATE,JSTATE,3)=HEFF(ISTATE,JSTATE,3)
C     *              +(T-S)*VAL2(JSTATE)
                  HEFF2(JSTATE,3)=HEFF2(JSTATE,3)+(T-S)*VAL2(JSTATE)
                END IF
              END DO
            END IF
          END IF
        END DO
      END IF
C**** DEBUG OUTPUT *****************************************************
      IF (LPOUT.LE.-5) THEN
         IF(GOPARR) CALL DDI_GSUMF(2503,HEFF2,NSTATE*2)
         DO JSTATE=1,NSTATE
            HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)+HEFF2(JSTATE,2)
            HEFF(ISTATE,JSTATE,3)=HEFF(ISTATE,JSTATE,3)+HEFF2(JSTATE,3)
            HEFF2(JSTATE,2)=0.0D+00
            HEFF2(JSTATE,3)=0.0D+00
         END DO
         IF (MASWRK) THEN
           WRITE(LUNOUT,'('' *** HEFF(2) <- ZERO-BODY GENERATOR ***'')')
           CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
           WRITE(LUNOUT,
     *          '('' *** HEFF(2) (ROT-2) <- ZERO-BODY GENERATOR ***'')')
           CALL MQWMAG(LUNOUT,HEFF(1,1,3),NSTATE,NSTATE,NSTATE,'    ')
         END IF
      END IF
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      IF(MASWRK) WRITE(LUNOUT,9100) TIME
 9100 FORMAT(1X,'TIME FOR 0-BODY FORMULAE INVOLVING 2 EXTERNAL',
     *          ' ORBITALS=',F10.2)
      STIME = ETIME
C
C**** ONE-BODY GENERATOR ***********************************************
C
  200 CONTINUE
        READ(LUNFT1) KCONT,LP,NWORD
        CALL MQGENR(LUNFT1,2*NWORD,NWORD,LABEL1,WORK)
        IPBASE=IPBASE+1
        IF (GOPARR .AND. MOD(IPBASE,NPPEXT).NE.IME) GO TO 299
        DO JSTATE=1,NSTATE
          VAL2(JSTATE)=0.0D+00
        END DO
C.... MODIFIED ON 9/9/01
        DO M=1,NWORD
C       DO M=1,NWORD-1
          LQ =LABEL1(1,M)
          M2 =LABEL1(2,M)
          IO =IOCSF (1,M2)
C.... MODIFIED ON 9/9/01
          IF(M.NE.NWORD) THEN
            MP1=M+1
            LQNEXT=LABEL1(1,MP1)
            IONEXT=IOCSF (1,LABEL1(2,MP1))
          ELSE
            LQNEXT=-1
            IONEXT=-1
          END IF
          IF(LQ.EQ.LQNEXT .AND. IO.EQ.IONEXT) THEN
            IF(KREF(M2)) THEN
              DO JSTATE=1,NSTATE
                VAL2(JSTATE)=VAL2(JSTATE)+WORK(M)*CASVEC(M2,JSTATE)
              END DO
            END IF
          ELSE
            IF(KREF(M2)) THEN
              DO JSTATE=1,NSTATE
                VAL2(JSTATE)=VAL2(JSTATE)+WORK(M)*CASVEC(M2,JSTATE)
              END DO
            END IF
            KLOOP=.FALSE.
            DO JSTATE=1,NSTATE
              IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
            END DO
            IF(KLOOP) THEN
              ESI2=ECONF(M2)-EREF0(ISTATE)
              EQ=EORB(LQ)
C             EP=EORB(LP)
              S=0.0D+00
              DO KBF=ISTART,IEND
                ISPB=ISYM(MOSYM(LP),MOSYM(KBF))
                ISQB=ISYM(MOSYM(LQ),MOSYM(KBF))
                IF(ISPB.EQ.ISQB) THEN
                  DO KI =INIDOC,LASDOC
                    ISPBI=ISYM(ISPB,MOSYM(KI))
                    DELTA0=EORB(KBF)-EORB(KI)-EQ+ESI2
                    DO KAE=ISYLAB(1,ISPBI,4),ISYLAB(2,ISPBI,4)
C                   DO KAE=INIEXT,LASEXT
                      DELTA=EORB(KAE)+DELTA0
C                     DELTA=EORB(KAE)+EORB(KBF)-EORB(KI)-EQ+ESI2
C.... MODIFIED ON 9/9/01
                      S=S-VTWOEL(KAE,KBF,KI,LP)
     *                  *( 2.0D+00*VTWOEL(KAE,KBF,KI,LQ)
     *                  -      VTWOEL(KAE,KBF,LQ,KI) )
     *                  /(DELTA+EDSHFT/DELTA)
C                     S=S-VTWOEL(KI,KAE,LP,KBF)
C    *                  *( 2.0D+00*VTWOEL(KI,KAE,LQ,KBF)
C    *                  -      VTWOEL(LQ,KAE,KI,KBF) )
C    *                  /(EAEBF2-EQ)
                    END DO
                  END DO
                END IF
              END DO
              DO JSTATE=1,NSTATE
                IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
C                  HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
C     *              +S*VAL2(JSTATE)
                  HEFF2(JSTATE,2)=HEFF2(JSTATE,2)+S*VAL2(JSTATE)
                END IF
              END DO
            END IF
            DO JSTATE=1,NSTATE
              VAL2(JSTATE)=0.0D+00
            END DO
          END IF
        END DO
C
C.... GO TO READ .......................................................
 299  CONTINUE
      IF(KCONT.NE.0) GO TO 200
C**** DEBUG OUTPUT *****************************************************
      IF (LPOUT.LE.-5) THEN
         IF(GOPARR) CALL DDI_GSUMF(2503,HEFF2,NSTATE*2)
         DO JSTATE=1,NSTATE
            HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)+HEFF2(JSTATE,2)
            HEFF(ISTATE,JSTATE,3)=HEFF(ISTATE,JSTATE,3)+HEFF2(JSTATE,3)
            HEFF2(JSTATE,2)=0.0D+00
            HEFF2(JSTATE,3)=0.0D+00
         END DO
         IF (MASWRK) THEN
            WRITE(LUNOUT,'('' *** HEFF(2) <- ONE-BODY GENERATOR ***'')')
            CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
         END IF
      END IF
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      IF(MASWRK) WRITE(LUNOUT,9110) TIME
      STIME = ETIME
 9110 FORMAT(1X,'TIME FOR 1-BODY FORMULAE INVOLVING 2 EXTERNAL',
     *          ' ORBITALS=',F10.2)
C
C**** TWO-BODY GENERATOR ***********************************************
C
 300  CONTINUE
        READ(LUNFT2) KCONT,LP,LR,NWORD
        CALL MQGENR(LUNFT2,3*NWORD,NWORD,LABEL2,WORK)
        IPBASE=IPBASE+1
        IF (GOPARR .AND. MOD(IPBASE,NPPEXT).NE.IME) GO TO 399
        DO JSTATE=1,NSTATE
          VAL2(JSTATE)=0.0D+00
        END DO
C.... MODIFIED ON 9/9/01
        DO M=1,NWORD
C       DO M=1,NWORD-1
          LQ =LABEL2(1,M)
          LS =LABEL2(2,M)
          M2 =LABEL2(3,M)
          IO =IOCSF (1,M2)
C.... MODIFIED ON 9/9/01
          IF(M.NE.NWORD) THEN
            MP1=M+1
            LQNEXT=LABEL2(1,MP1)
            LSNEXT=LABEL2(2,MP1)
            IONEXT=IOCSF (1,LABEL2(3,MP1))
          ELSE
            LQNEXT=-1
            LSNEXT=-1
            IONEXT=-1
          END IF
            IF(KREF(M2)) THEN
              IF(LP.EQ.LR) THEN
                DO JSTATE=1,NSTATE
                  VAL2(JSTATE)=VAL2(JSTATE)
     *              +WORK(M)*0.5D+00*CASVEC(M2,JSTATE)
                END DO
              ELSE
                DO JSTATE=1,NSTATE
                  VAL2(JSTATE)=VAL2(JSTATE)
     *              +WORK(M)*CASVEC(M2,JSTATE)
                END DO
              END IF
            END IF
          IF(LQ.NE.LQNEXT .OR. LS.NE.LSNEXT .OR. IO.NE.IONEXT) THEN
            KLOOP=.FALSE.
            DO JSTATE=1,NSTATE
              IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
            END DO
            IF(KLOOP) THEN
              ESI2=ECONF(M2)-EREF0(ISTATE)
              S=0.0D+00
C             EP=EORB(LP)
              EQ=EORB(LQ)
C             ER=EORB(LR)
              ES=EORB(LS)
C     PQRS
C             EPR=EP+ER
              EQS=EQ+ES
C             ERS=ER-ES
C             EQR=EQ-ER
C             EPSR=EP-ES+ER
C             EQRS=EQ-ER+ES
C     RSPQ
C             ERP=ER+EP
C             ESQ=ES+EQ
C             EPQ=EP-EQ
C             ESP=ES-EP
C             ERQP=ER-EQ+EP
C             ESPQ=ES-EP+EQ
C
              ISPR=ISYM(MOSYM(LP),MOSYM(LR))
              ISQS=ISYM(MOSYM(LQ),MOSYM(LS))
              IF(ISPR.EQ.ISQS) THEN
                DO KF =ISTART,IEND
                  ISPRF=ISYM(ISPR,MOSYM(KF))
                  DELTA0=EORB(KF)-EQS+ESI2
                  DO KAE=ISYLAB(1,ISPRF,4),ISYLAB(2,ISPRF,4)
C                 DO KAE=INIEXT,LASEXT
                    DELTA=EORB(KAE)+DELTA0
C                   DELTA=EORB(KAE)+EORB(KF)-EQS+ESI2
C.... MODIFIED ON 9/9/01
                    S=S- VTWOEL(KAE,KF,LP,LR)*VTWOEL(KAE,KF,LQ,LS)
     *                /(DELTA+EDSHFT/DELTA)
C    *                 - VTWOEL(LR,KAE,LP,KF)
C    *                *VTWOEL(LS,KAE,LQ,KF)
C    *                /(EKAEF2-ESQ)*0.5D+00
C                   S=S- VTWOEL(LP,KAE,LR,KF)
C    *                *VTWOEL(LQ,KAE,LS,KF)
C    *                /(EKAEF2-EQS)
CC   *                 - VTWOEL(LR,KAE,LP,KF)
CC   *                *VTWOEL(LS,KAE,LQ,KF)
CC   *                /(EKAEF2-ESQ)*0.5D+00
                  END DO
                END DO
              END IF
              DO JSTATE=1,NSTATE
                IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
C                  HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
C     *              +S*VAL2(JSTATE)
                  HEFF2(JSTATE,2)=HEFF2(JSTATE,2)+S*VAL2(JSTATE)
                END IF
              END DO
            END IF
            DO JSTATE=1,NSTATE
              VAL2(JSTATE)=0.0D+00
            END DO
          END IF
        END DO
C
C.... GO TO READ .......................................................
 399  CONTINUE
      IF(KCONT.NE.0) GO TO 300
C------------------------------------------------------------- DDI_GSUMF
      IF(GOPARR) CALL DDI_GSUMF(2503,HEFF2,NSTATE*2)
      DO JSTATE=1,NSTATE
         HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)+HEFF2(JSTATE,2)
         HEFF(ISTATE,JSTATE,3)=HEFF(ISTATE,JSTATE,3)+HEFF2(JSTATE,3)
      END DO
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** HEFF(2) <- TWO-BODY GENERATOR ***'')')
        CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
      END IF
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      IF(MASWRK) WRITE(LUNOUT,9120) TIME
 9120 FORMAT(1X,'TIME FOR 2-BODY FORMULAE INVOLVING 2 EXTERNAL',
     *          ' ORBITALS=',F10.2)
C
      RETURN
      END
C*MODULE MCQDPT  *DECK MQLPR1
      SUBROUTINE MQLPR1(LUNOUT,LPOUT ,LUNJM1,LUNJM2,LUNJM3,LUNTWO,
     *                  NMOFZC,NMODOC,NMOACT,NMOEXT,NMO   ,NMOEI ,
     *                  NMOII ,
     *                  INIACT,LASACT,
     *                  NSTATE,NCSF  ,NOCF  ,
     *                  MAXERI,
     *                  CASVEC,EORB  ,LIJMO ,VONEEL,VTWOEL,
     *                  ECONF ,EREF0 ,
     *                  HCORE ,IOMAP ,IOCSF ,LWCD  ,WORK  ,HCORE2)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***********************************************************************
C****                         ******************************************
C****   ARRAYS AND CONSTANT   ******************************************
C****                         ******************************************
      DIMENSION CASVEC(NCSF,NSTATE), EORB (NMO), LIJMO(NMO,NMO)
      DIMENSION VONEEL(NMO,NMO), VTWOEL(NMOEI,NMOII)
      DIMENSION ECONF (NCSF), EREF0(NSTATE)
      DIMENSION HCORE (NMO,NMO),HCORE2(NMO,NMO)
      INTEGER   IOMAP (INIACT:LASACT+1,NOCF), IOCSF(2,NCSF)
      INTEGER   LWCD  (2,MAXERI)
      DIMENSION WORK  (MAXERI)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQLPR1 ***'')')
        CALL MQDEBG(LUNOUT,
     *  '     -','LUNTWO','NMOFZC','NMODOC','NMOACT','NMOEXT','   NMO',
     *        0 , LUNTWO , NMOFZC , NMODOC , NMOACT , NMOEXT ,    NMO )
        CALL MQDEBG(LUNOUT,
     *  ' NMOEI',' NMOII','INIACT','LASACT','NSTATE','  NCSF','  NOCF',
     *    NMOEI ,  NMOII , INIACT , LASACT , NSTATE ,   NCSF ,   NOCF )
        CALL MQDEBG(LUNOUT,
     *  'MAXERI','LUNJM1','LUNJM2','LUNJM3','     -','     -','     -',
     *   MAXERI , LUNJM1 , LUNJM2 , LUNJM3 ,      0 ,      0 ,      0 )
      END IF
C**** SET CONSTANTS ****************************************************
      NMOINT=NMOFZC+NMODOC+NMOACT
      INIDOC=NMOFZC+1
      LASDOC=NMOFZC+NMODOC
C     INIACT=NMOFZC+NMODOC+1
C     LASACT=NMOFZC+NMODOC+NMOACT
C     INIEXT=NMOFZC+NMODOC+NMOACT+1
C     LASEXT=NMOFZC+NMODOC+NMOACT+NMOEXT
C***********************************************************************
C****                    ***********************************************
C****   PREPARE ARRAYS   ***********************************************
C****                    ***********************************************
      ECORE0=0.0D+00
      DO 500 I=1,NMOFZC
        ECORE0=ECORE0+HCORE(I,I)*2.0D+00
  500 CONTINUE
C**** CLEAR ARRAYS *****************************************************
      DO 107 J=1,NMOFZC
      DO 107 I=1,NMO
        HCORE(I,J)=0.0D+00
        HCORE(J,I)=0.0D+00
  107 CONTINUE
      IF (GOPARR) THEN
C         DCOPY(NMO*NMO,HCORE,1,HCORE2,1)
         DO J=1,NMO
            DO I=1,NMO
               HCORE2(I,J)=HCORE(I,J)
               HCORE(I,J)=0D0
            END DO
         END DO
      END IF
      DO 108 J=1,NMO
      DO 108 I=1,NMO
        VONEEL(I,J)=0.0D+00
  108 CONTINUE
      DO 110 J=1,NMOII
      DO 110 I=1,NMOEI
        VTWOEL(I,J)=0.0D+00
  110 CONTINUE
      DO 116 I=1,NCSF
        ECONF(I)=0.0D+00
  116 CONTINUE
C**** PREPARE LIJMO ****************************************************
      DO 117 J=1,NMO
      DO 117 I=1,NMO
        LIJMO(I,J)=0
  117 CONTINUE
      N=0
      DO 118 I=INIDOC,NMOINT
      DO 118 J=INIDOC,I
        N=N+1
        LIJMO(I,J)=N
        LIJMO(J,I)=N
  118 CONTINUE
      NMOIP1=NMOINT+1
      DO 120 I=NMOIP1,NMO
      DO 120 J=INIDOC,NMOINT
        N=N+1
        LIJMO(I,J)=N
        LIJMO(J,I)=N
  120 CONTINUE
C**** PREPARE PERTURBATION MATRICES ************************************
C
C     VTWOEL
C
      ECORE=0.0D+00
      CALL SEQREW(LUNJM1)
      CALL SEQREW(LUNJM2)
      CALL SEQREW(LUNJM3)
C
      DO 130 LOOP=1,3
        IF(LOOP.EQ.1) THEN
          LUNIN=LUNJM1
        ELSE IF(LOOP.EQ.2) THEN
          LUNIN=LUNJM2
        ELSE
          LUNIN=LUNJM3
        END IF
  126   READ(LUNIN) KCONT,IMOAB,JMOAB,NWORD
        IF(KCONT.EQ.0.AND.NWORD.EQ.0) GO TO 130
        CALL MQGENR(LUNIN,2*NWORD,NWORD,LWCD,WORK)
        DO 128 I=1,NWORD
          LP=IMOAB
          LQ=JMOAB
          LR=LWCD(1,I)
          LS=LWCD(2,I)
          V=WORK(I)
          IF(LP.EQ.LQ .AND. LP.LE.NMOFZC .AND.
     *       LR.EQ.LS .AND. LR.LE.NMOFZC ) ECORE=ECORE+2.0D+00*V
          IF(LP.EQ.LR .AND. LP.LE.NMOFZC .AND.
     *       LQ.EQ.LS .AND. LR.LE.NMOFZC ) THEN
            ECORE=ECORE-V*2.0D+00
            IF(LP.EQ.LQ) ECORE=ECORE+V
          END IF
          IF(LOOP.EQ.1 .AND. LQ.GT.NMOFZC .AND. LS.GT.NMOFZC) THEN
            MOI = LIJMO(LP,LQ)
            MOJ = LIJMO(LR,LS)
            VTWOEL(MOI,MOJ)=V
          ELSE IF(LOOP.EQ.2 .AND. LQ.GT.NMOFZC .AND. LS.GT.NMOFZC) THEN
            MOI = LIJMO(LP,LQ)
            MOJ = LIJMO(LR,LS)
            VTWOEL(MOI,MOJ)=V
          END IF
          IF(LR.EQ.LS .AND. LQ.GT.NMOFZC .AND. LR.LE.NMOFZC) THEN
            IF(LP.EQ.LQ) THEN
              IF(LP.LE.LASDOC) THEN
                HCORE(LP,LQ)=HCORE(LP,LQ)+      V
              ELSE
                HCORE(LP,LQ)=HCORE(LP,LQ)+2.0D+00*V
              END IF
            ELSE
              HCORE(LP,LQ)=HCORE(LP,LQ)+2.0D+00*V
              HCORE(LQ,LP)=HCORE(LQ,LP)+2.0D+00*V
            END IF
          ELSE IF(LQ.EQ.LS .AND. LP.GT.NMOFZC .AND. LQ.LE.NMOFZC .AND.
     *                           LR.GT.NMOFZC .AND. LS.LE.NMOFZC ) THEN
            IF(LOOP.NE.2) THEN
              IF(LP.EQ.LR .AND. LP.LE.LASDOC) THEN
                HCORE(LP,LR)=HCORE(LP,LR)-0.5D+00*V
              ELSE
                HCORE(LP,LR)=HCORE(LP,LR)-V
              END IF
            ELSE
              HCORE(LP,LR)=HCORE(LP,LR)-V
              HCORE(LR,LP)=HCORE(LR,LP)-V
            END IF
          END IF
          IF(LOOP.EQ.3) THEN
            IF(LQ.EQ.LS .AND.
     *         LQ.GE.INIDOC .AND. LQ.LE.LASDOC) THEN
              HCORE(LP,LR)=HCORE(LP,LR)-V
            END IF
          END IF
  128   CONTINUE
        IF(KCONT.NE.0) GO TO 126
  130 CONTINUE
      IF (GOPARR) THEN
         CALL DDI_GSUMF(2511,ECORE,1)
         CALL DDI_GSUMF(2512,HCORE,NMO*NMO)
         CALL DDI_GSUMF(2513,VTWOEL,NMOEI*NMOII)
C         CALL DAXPY(NMO*NMO,1D0,HCORE2,1,HCORE,1)
         DO J=1,NMO
            DO I=1,NMO
               HCORE(I,J)=HCORE(I,J)+HCORE2(I,J)
            END DO
         END DO
      END IF
      ECORE=ECORE+ECORE0
C
C
C     HCORE+V(ONE-BODY) AND VONEEL+V(ONE-BODY)
C
C.... (IJ/BB) AND (IB/JB) FROM VTWOEL ..................................
      DO 132 K=INIDOC,LASDOC
        MOJ1=LIJMO(K,K)
      DO 132 J=INIDOC,NMOINT
      DO 132 I=J,NMO
        MOI1=LIJMO(I,J)
        MOI2=LIJMO(I,K)
        MOJ2=LIJMO(J,K)
        S   = 2.0D+00*VTWOEL(MOI1,MOJ1)-VTWOEL(MOI2,MOJ2)
        IF(I.NE.J) THEN
          HCORE(I,J) = HCORE(I,J) + S
          HCORE(J,I) = HCORE(J,I) + S
        ELSE
          IF(I.LE.LASDOC) THEN
            HCORE(I,J) = HCORE(I,J) + S*0.5D+00
          ELSE
            HCORE(I,J) = HCORE(I,J) + S
          END IF
        END IF
  132 CONTINUE
      DO 140 J=INIDOC,NMO
      DO 140 I=J,NMO
        VONEEL(I,J)=HCORE(I,J)
        VONEEL(J,I)=HCORE(J,I)
  140 CONTINUE
      DO 142 I=INIDOC,NMO
        VONEEL(I,I)=VONEEL(I,I)-EORB(I)
  142 CONTINUE
C**** PREPARE ECONF ****************************************************
      EC=0.0D+00
      DO 200 I=1,LASDOC
        EC=EC+2*EORB(I)
  200 CONTINUE
      DO 202 I=1,NCSF
        ECONF(I)=EC
  202 CONTINUE
      DO 144 K=INIACT,LASACT
        E=EORB(K)
      DO 144 I=1,NCSF
        IOCF=IOCSF(1,I)
        ICASE=IOMAP(K,IOCF)
        IF(ICASE.EQ.2) THEN
          ECONF(I)=ECONF(I)+E
        ELSE IF(ICASE.EQ.3) THEN
          ECONF(I)=ECONF(I)+2.0D+00*E
        END IF
  144 CONTINUE
C**** ZEROTH ORDER ENERGIES OF TARGET STATES ***************************
      DO 194 ISTATE=1,NSTATE
        E0=0.0D+00
        DO 192 I=1,NCSF
          E0=E0+ECONF(I)*CASVEC(I,ISTATE)**2
  192   CONTINUE
        EREF0(ISTATE)=E0
  194 CONTINUE
C**** DEBUG OUTPUT *****************************************************
      IF(MASWRK) WRITE(LUNOUT,158) ECORE
  158 FORMAT(1X,'THE FROZEN CORE ENERGY IS',F20.10)
      IF(LPOUT.LE.-10 .AND. MASWRK) THEN
        WRITE(LUNOUT,146)
  146   FORMAT(' *** ORBITAL ENERGIES ***')
        CALL MQWMAG(LUNOUT,EORB,1,1,NMO,'    ')
        WRITE(LUNOUT,148)
  148   FORMAT(' *** MATRIX HCORE ***'/
     *         ' ONLY PARTS NOT INCLUDING FROZEN CORE ORBITALS ARE',
     *         ' CORRECT.')
        CALL MQWMAG(LUNOUT,HCORE,NMO,NMO,NMO,'    ')
        WRITE(LUNOUT,150)
  150   FORMAT(' *** MATRIX VONEEL ***'/
     *         ' ONLY PARTS NOT INCLUDING FROZEN CORE ORBITALS ARE',
     *         ' CORRECT.')
        CALL MQWMAG(LUNOUT,VONEEL,NMO,NMO,NMO,'    ')
        IF(LPOUT.LE.-100) THEN
          WRITE(LUNOUT,152)
  152     FORMAT(' *** MATRIX VTWOEL ***')
          CALL MQWMAG(LUNOUT,VTWOEL,NMOEI,NMOEI,NMOII,'    ')
        END IF
        WRITE(LUNOUT,154)
  154   FORMAT(' *** ZEROTH ENERGY OF CONFIGURATION ***')
        CALL MQWMAG(LUNOUT,ECONF,1,1,NCSF,'    ')
        WRITE(LUNOUT,156)
  156   FORMAT(' *** ZEROTH ENERGY OF TARGET STATES ***')
        CALL MQWMAG(LUNOUT,EREF0,1,1,NSTATE,'    ')
      END IF
C**** WRITE TWO-ELECTRON INTEGRALS *************************************
      CALL SEQREW(LUNTWO)
      DO 160 I=1,NMOII
        CALL MQMATW(LUNTWO,NMOEI,VTWOEL(1,I))
  160 CONTINUE
C***********************************************************************
C****            *******************************************************
C****   RETURN   *******************************************************
C****            *******************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQLPR2
      SUBROUTINE MQLPR2(LUNOUT,LPOUT ,LUNJM3,LUNTWO,
     *                  INIDOC,LASDOC,INIACT,LASACT,INIEXT,LASEXT,
     *                  MAXERI,IWIDTH,IDMWTH,DTWOEL,REFWGT,
     *                  LWCD  ,WORK  ,VTWOEL)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***********************************************************************
C****                         ******************************************
C****   ARRAYS AND CONSTANT   ******************************************
C****                         ******************************************
      INTEGER   LWCD  (2,MAXERI)
      DIMENSION WORK  (MAXERI)
      DIMENSION VTWOEL(INIDOC:LASACT,INIEXT:LASEXT,
     *                 INIDOC:LASACT,IWIDTH       )
      LOGICAL GOPARR,DSKWRK,MASWRK
      LOGICAL REFWGT
      INTEGER DTWOEL
      DIMENSION KIJN(4)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQLPR2 ***'')')
        CALL MQDEBG(LUNOUT,
     *  'LUNJM3','LUNTWO','MAXERI','IWIDTH','IDMWTH','     -','     -',
     *   LUNJM3 , LUNTWO , MAXERI , IWIDTH , IDMWTH ,      0 ,      0 )
        CALL MQDEBG(LUNOUT,
     *  'INIDOC','LASDOC','INIACT','LASACT','INIEXT','LASEXT','     -',
     *   INIDOC , LASDOC , INIACT , LASACT , INIEXT , LASEXT ,      0)
      END IF
C**** PREPARE PERTURBATION MATRICES ************************************
      NMOEXT=LASEXT-INIEXT+1
      NMODA =LASACT-INIDOC+1
      NDSIZE=NMODA*NMOEXT
C
      INIEX1=INIEXT
      NMOEXP=0
      DO IPROC=0,ME
         INIEX1=INIEX1+NMOEXP
         NMOEXP=NMOEXT/NPROC
         IF (MOD(NMOEXT,NPROC).GT.IPROC) NMOEXP=NMOEXP+1
      END DO
      LASEX1=INIEX1+NMOEXP-1
C
C
C     VTWOEL
C
      IF (IDMWTH.EQ.0) THEN
C
      INIT=INIEXT
  100 INITM1=INIT-1
      LAST=INITM1+IWIDTH
      IF(LAST.GT.LASEXT) LAST=LASEXT
      CALL SEQREW(LUNJM3)
      CALL VCLR(VTWOEL,1,(LAST-INITM1)*NMOEXT*NMODA*NMODA)
      DO IPROC=0,NPROC-1
 114     CONTINUE
         IF (IPROC.EQ.ME) THEN
C 114        READ(LUNJM3) KCONT,IMOAB,JMOAB,NWORD
C            READ(LUNJM3) KCONT,IMOAB,JMOAB,NWORD
            READ(LUNJM3) KIJN
            IF (KIJN(1).NE.0.OR.KIJN(4).NE.0) THEN
C               CALL MQGENR(LUNJM3,2*NWORD,NWORD,LWCD,WORK)
               CALL MQGENR(LUNJM3,2*KIJN(4),KIJN(4),LWCD,WORK)
            END IF
         END IF
         IF (GOPARR) THEN
            IPROCTMP = IPROC
            CALL DDI_BCAST(2514,'I',KIJN,4,IPROCTMP)
         END IF
         IF (KIJN(1).NE.0.OR.KIJN(4).NE.0) THEN
            KCONT=KIJN(1)
            IMOAB=KIJN(2)
            JMOAB=KIJN(3)
            NWORD=KIJN(4)
            IF (GOPARR) THEN
               IPROCTMP = IPROC
               CALL DDI_BCAST(2515,'I',LWCD,NWORD*2,IPROCTMP)
               CALL DDI_BCAST(2516,'F',WORK,NWORD  ,IPROCTMP)
            END IF
            LP=IMOAB
            LQ=JMOAB
            IF (LQ.GE.INIDOC) THEN
               DO I=1,NWORD
                  LR=LWCD(1,I)
                  LS=LWCD(2,I)
                  V=WORK(I)
                  LRDUM=LR-INITM1
                  IF (LS.GE.INIDOC.AND.LR.GE.INIT.AND.LR.LE.LAST)
     *                 VTWOEL(LQ,LP,LS,LRDUM)=V
               END DO
            END IF
         END IF
         IF(KIJN(1).NE.0) GO TO 114
      END DO
C
C**** WRITE TWO-ELECTRON INTEGRALS *************************************
      IF (REFWGT) THEN
         DO J=INIT,LAST
            JMI=J-INITM1
            DO I=INIDOC,LASACT
               CALL MQMATW(LUNTWO,NDSIZE,VTWOEL(INIDOC,INIEXT,I,JMI))
            END DO
         END DO
      ELSE
         DO J=MAX(INIT,INIEX1),MIN(LAST,LASEX1)
            JMI=J-INITM1
            DO I=INIDOC,LASACT
               CALL MQMATW(LUNTWO,NDSIZE,VTWOEL(INIDOC,INIEXT,I,JMI))
            END DO
         END DO
      END IF
C
C**** GO TO 100 ********************************************************
      INIT=INIT+IWIDTH
      IF(INIT.LE.LASEXT) GO TO 100
C
      ELSE
C
C                                                     DDI SHARED MEMORY
C-----------------------------------------------------------------------
         INIT=INIEXT
 500     INITM1=INIT-1
         LAST=INITM1+IDMWTH
         IF(LAST.GT.LASEXT) LAST=LASEXT
         CALL SEQREW(LUNJM3)
C         CALL DDI_DISTRIB(DTWOEL,ME,ILOC,IHIC,JLOC,JHIC)
C         CALL VCLR(VTWOEL,1,(IHIC-ILOC+1))
C         DO J=JLOC,JHIC
C            CALL DDI_PUT(DTWOEL,ILOC,IHIC,J,J,VTWOEL)
C         END DO
         CALL VCLR(VTWOEL,1,NMOEXT*NMODA)
         CALL DDI_SYNC(2523)
C
 510     CONTINUE
         READ(LUNJM3) KCONT,IMOAB,JMOAB,NWORD
         IF (KCONT.EQ.0.AND.NWORD.EQ.0) GO TO 590
         CALL MQGENR(LUNJM3,2*NWORD,NWORD,LWCD,WORK)
         LP=IMOAB
         LQ=JMOAB
         IF (LQ.GE.INIDOC) THEN
            DO I=1,NWORD
               LR=LWCD(1,I)
               LS=LWCD(2,I)
               IF (LS.GE.INIDOC.AND.LR.GE.INIT.AND.LR.LE.LAST) THEN
                  LRDUM=LR-INITM1+INIEXT-1
                  VTWOEL(LS,LRDUM,INIDOC,1)=WORK(I)
               END IF
            END DO
            LP=IMOAB-INIEXT+1
            LQ=JMOAB-INIDOC+1
C
            IF (KCONT.EQ.0 .OR. KCONT.EQ.1) THEN
               CALL DDI_PUT(DTWOEL,
     *              LQ+(LP-1)*NMODA,LQ+(LP-1)*NMODA,
     *              1,(LAST-INIT+1)*NMODA,VTWOEL)
               CALL VCLR(VTWOEL,1,NMOEXT*NMODA)
            END IF
         END IF
C
         IF (KCONT.NE.0) GO TO 510
 590     CONTINUE
         CALL DDI_SYNC(2521)
C
         IF (REFWGT) THEN
            DO J=INIT,LAST
               JMI=J-INITM1
               CALL DDI_GET(DTWOEL,1,NDSIZE,
     *              (JMI-1)*NMODA+1,JMI*NMODA,VTWOEL)
               DO I=INIDOC,LASACT
                  CALL MQMATW(LUNTWO,NDSIZE,VTWOEL(INIDOC,INIEXT,I,1))
               END DO
            END DO
         ELSE IF (IDMWTH.LT.NMOEXT) THEN
C THIS CODE REQUIRES MUCH DDI MEMORIES.
C            JMIS=MAX(INIT,INIEX1)-INITM1
C            JMIE=MIN(LAST,LASEX1)-INITM1
C            CALL DDI_GET(DTWOEL,1,NDSIZE,
C     *                   (JMIS-1)*NMODA+1,JMIE*NMODA,
C     *                  VTWOEL(INIDOC,INIEXT,INIDOC,JMIS))
C            DO J=MAX(INIT,INIEX1),MIN(LAST,LASEX1)
C               JMI=J-INITM1
C               DO I=INIDOC,LASACT
C                 CALL MQMATW(LUNTWO,NDSIZE,VTWOEL(INIDOC,INIEXT,I,JMI))
C               END DO
C            END DO
C THIS CODE REQUIRES SMALLER DDI MEMORIES THAN ABOVE, BUT CAUSES
C MANY DDI_GET CALLS.
            DO J=MAX(INIT,INIEX1),MIN(LAST,LASEX1)
               JMI=J-INITM1
               CALL DDI_GET(DTWOEL,1,NDSIZE,
     *              (JMI-1)*NMODA+1,JMI*NMODA,VTWOEL)
               DO I=INIDOC,LASACT
                  CALL MQMATW(LUNTWO,NDSIZE,VTWOEL(INIDOC,INIEXT,I,1))
               END DO
            END DO
         END IF
         CALL DDI_SYNC(2522)
C
         INIT=INIT+IDMWTH
         IF(INIT.LE.LASEXT) GO TO 500
C
      END IF
C***********************************************************************
C****            *******************************************************
C****   RETURN   *******************************************************
C****            *******************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQMATR
      SUBROUTINE MQMATR(LUNI,LENWOR,WORK)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION WORK (LENWOR)
      READ(LUNI) WORK
      RETURN
      END
C*MODULE MCQDPT  *DECK MQMATW
      SUBROUTINE MQMATW(LUNO,LENWOR,WORK)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION WORK (LENWOR)
      WRITE(LUNO) WORK
      RETURN
      END
C*MODULE MCQDPT  *DECK MQMOSY
      SUBROUTINE MQMOSY(LUNOUT,LPOUT,ISWTCH,NGO,NMO,MOSYM,Q,S,V,T)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION Q(NGO,*),S(NGO*(NGO+1)/2),V(NGO,NMO),T(NGO),
     *          MOSYM(NMO)
      DIMENSION IRTABL(8,9),ISYMTB(8,9)
C
      LOGICAL ABEL
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXAO=2047)
      PARAMETER (ZERO=0.0D+00, SMALL=1.0D-03)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /SYMBLK/ NIRRED,NSALC,NSALC2,NSALC3
      COMMON /SYMMOL/ GROUP,COMPLX,IGROUP,NAXIS,ILABMO,ABEL
      COMMON /SYMQMT/ IRPLAB(14),IRPNUM(14),IRPDIM(14),IRPDEG(14)
C
C     IRREP NUMBERS AND NAMES, THE ROW INDEX 1-9 MEANS
C     1=C1, 2=CS, 3=CI, 4=C2, 6=C2H, 7=C2V, 8=D2, 9=D2H
C
      DATA IRTABL /1,0,0,0,0,0,0,0,
     *             1,2,0,0,0,0,0,0,
     *             1,2,0,0,0,0,0,0,
     *             1,2,0,0,0,0,0,0,
     *             0,0,0,0,0,0,0,0,
     *             1,4,2,3,0,0,0,0,
     *             1,2,3,4,0,0,0,0,
     *             1,2,4,3,0,0,0,0,
     *             1,5,8,4,2,6,7,3/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA ISYMTB /4HA   ,7*4H    ,
     *             4HA'  ,4HA'' ,6*4H    ,
     *             4HAG  ,4HAU  ,6*4H    ,
     *             4HA   ,4HB   ,6*4H    ,
     *           8*4H    ,
     *             4HAG  ,4HBU  ,4HBG  ,4HAU  ,4*4H    ,
     *             4HA1  ,4HA2  ,4HB1  ,4HB2  ,4*4H    ,
     *             4HA   ,4HB1  ,4HB2  ,4HB3  ,4*4H    ,
     *      4HAG  ,4HB1G ,4HB2G ,4HB3G ,4HAU  ,4HB1U ,4HB2U ,4HB3U /
#else
      CHARACTER*4 :: ISYMTB_STR(8,9)
      EQUIVALENCE (ISYMTB, ISYMTB_STR)
      DATA ISYMTB_STR/"A   ",7*"    ",
     *             "A'  ","A'' ",6*"    ",
     *             "AG  ","AU  ",6*"    ",
     *             "A   ","B   ",6*"    ",
     *           8*"    ",
     *             "AG  ","BU  ","BG  ","AU  ",4*"    ",
     *             "A1  ","A2  ","B1  ","B2  ",4*"    ",
     *             "A   ","B1  ","B2  ","B3  ",4*"    ",
     *      "AG  ","B1G ","B2G ","B3G ","AU  ","B1U ","B2U ","B3U "/
#endif
C
C     ----- ASSIGN SYMMETRY LABEL TO EACH ORBITAL -----
C
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQMOSY ***'')')
        CALL MQDEBG(LUNOUT,
     *  'ISWTCH','   NGO','   NMO','     -','     -','     -','     -',
     *   ISWTCH ,    NGO ,    NMO ,      0 ,      0 ,      0 ,      0 )
      END IF
C
C     -IGROUP- IS A POINTER INTO THE FOLLOWING TABLE,
C     DATA GRP /C1   ,CS   ,CI   ,CN   ,S2N  ,CNH  ,
C    *          CNV  ,DN   ,DNH  ,DND  ,CINFV,DINFH,T
C    *          TH   ,TD   ,O    ,OH   ,I    ,IH   /
C     THIS ROUTINE SUPPORTS ONLY ABELIAN GROUPS.
C
      IF( (IGROUP.EQ. 4 .AND. NAXIS.NE.2) .OR.
     *    (IGROUP.EQ. 5                 ) .OR.
     *    (IGROUP.EQ. 6 .AND. NAXIS.NE.2) .OR.
     *    (IGROUP.EQ. 7 .AND. NAXIS.NE.2) .OR.
     *    (IGROUP.EQ. 8 .AND. NAXIS.NE.2) .OR.
     *    (IGROUP.EQ. 9 .AND. NAXIS.NE.2) .OR.
     *    (IGROUP.GE.10                 ) ) THEN
        WRITE(LUNOUT,'('' *** ERROR STOP IN SUB.MOMQSY ***''/
     *    '' YOU MUST USE AN ABELIAN POINT GROUP IN $DATA '')')
        CALL ABRT
      END IF
C
C     ----- CALCULATE Q * S * V -----
C
      DO 108 I=1,NSALC
        DO 104 J=1,NGO
          DUM=ZERO
          DO 102 K=1,NGO
            KJ=IA(MAX(K,J))+MIN(K,J)
            DUM=DUM+Q(K,I)*S(KJ)
  102     CONTINUE
          T(J)=DUM
  104   CONTINUE
        DO 106 J=1,NGO
          Q(J,I)=T(J)
  106   CONTINUE
  108 CONTINUE
C
      DO 116 I=1,NSALC
        DO 112 J=1,NMO
          DUM=ZERO
          DO 110 K=1,NGO
            DUM=DUM+Q(K,I)*V(K,J)
  110     CONTINUE
          T(J)=DUM
  112   CONTINUE
        DO 114 J=1,NMO
          Q(J,I)=T(J)
  114   CONTINUE
  116 CONTINUE
C
C**** ASSIGN SYMMETRY TO -V- ORBITALS **********************************
C
      DO 124 I=1,NMO
        SMAX=ZERO
        IQ=0
        DO 122 IRP=1,NIRRED
          DO 120 ID =1,IRPDIM(IRP)
            IF(IRPNUM(IRP).EQ.0) GO TO 120
            DO 118 IN =1,IRPNUM(IRP)
              IQ=IQ+1
              DUM= ABS(Q(I,IQ))
              IF(DUM.LE.SMAX) GO TO 118
              IRPMAX=IRP
              SMAX=DUM
  118       CONTINUE
  120     CONTINUE
  122   CONTINUE
        MOSYM(I)=IRTABL(IRPMAX,IGROUP)
  124 CONTINUE
C
C**** SYMMETRY CONTAMINATION CHECK *************************************
C
      DO 142 I=1,NMO
        SMAX = ZERO
        IQ = 0
        DO 130 IRP=1,NIRRED
          DO 128 ID=1,IRPDIM(IRP)
            IF(IRPNUM(IRP).EQ.0) GO TO 128
            DO 126 IN=1,IRPNUM(IRP)
              IQ=IQ+1
              DUM = ABS(Q(I,IQ))
              IF(DUM.GT.SMAX) THEN
                SMAX = DUM
                IRPMAX=IRP
              END IF
  126       CONTINUE
  128     CONTINUE
  130   CONTINUE
C--     IFLG=0
        IQ  =0
        DO 140 IRP=1,NIRRED
          DO 138 ID=1,IRPDIM(IRP)
            IF(IRPNUM(IRP).EQ.0) GO TO 138
            IF(IRP.NE.IRPMAX) THEN
              DO 136 IN=1,IRPNUM(IRP)
                IQ=IQ+1
                DUM=ABS(Q(I,IQ))
C               IF(DUM.GT.BIG) THEN
                IF(DUM.GT.SMALL) THEN
                  IF (MASWRK) WRITE(LUNOUT,132) I,DUM
  132             FORMAT(' *** ERROR STOP IN SUB.MQMOSY ***'/
     *            ' SYMMETRY CONTAMINATION TOO BIG ON MO',I6,D10.2)
                  CALL ABRT
C               ELSE IF(DUM.GT.SMALL.AND.IFLG.EQ.0) THEN
C                 IF (MASWRK) WRITE(LUNOUT,134) I,DUM
C 134             FORMAT(
C    *            ' -WARNING- SMALL SYMMETRY CONTAMINATION ON INPUT MO',
C    *            I6,D10.2
C    *            )
C                 IFLG=1
                END IF
                Q(IQ,I) = ZERO
  136         CONTINUE
            ELSE
              IQ=IQ+IRPNUM(IRP)
            END IF
  138     CONTINUE
  140   CONTINUE
  142 CONTINUE
C
C         PRINT ORBITAL SYMMETRY ASSIGNMENTS
C
      IF(ISWTCH.EQ.0) RETURN
      IF (MASWRK) THEN
      WRITE(LUNOUT,9021)
      WRITE(LUNOUT,9022) (I,MOSYM(I),ISYMTB(MOSYM(I),IGROUP),I=1,NMO)
      END IF
      RETURN
C
 9021 FORMAT(1X,'THE SYMMETRY OF EACH ORBITAL IS')
 9022 FORMAT(6(I5,'=',I2,',',A4))
      END
C*MODULE MCQDPT  *DECK MQMOWR
      SUBROUTINE MQMOWR(LUNOUT,NGO,NMO,EORB,CMO)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION EORB(NMO), CMO(NGO,NMO)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      IF (MASWRK) THEN
C**** WRITE MO *********************************************************
      INI                =1
  100 LAS                =INI+4
      IF(LAS.GT.NMO) LAS   =NMO
      WRITE(LUNOUT,'(1X,71(1H-)/'' ORB  /'',5I13)') (J,J=INI,LAS)
      WRITE(LUNOUT,'('' EORB /'',5F13.3)') (EORB(J),J=INI,LAS)
      WRITE(LUNOUT,'(1X,71(1H.))')
      DO 102 I           =1,NGO
        WRITE(LUNOUT,'(I7,5F13.8)') I,(CMO(I,J),J=INI,LAS)
  102 CONTINUE
      INI                =LAS+1
      IF(INI.LE.NMO) GO TO 100
      WRITE(LUNOUT,'(1X,71(1H-))')
C**** RETURN ***********************************************************
      END IF
      RETURN
      END
C*MODULE MCQDPT  *DECK MQMXOL
      SUBROUTINE MQMXOL(LUNOUT,LPOUT ,LUNPUN,
     *                  NGO   ,NMO   ,INIACT,LASACT,INOFMT,
     *                  CMOINP,CMOOLD,CMONEW,EORBOD,EORBNW,SINTEG,
     *                  OVRLAP,LORDER)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DIMENSION CMOINP(NGO,NMO)
      DIMENSION CMOOLD(NGO,NMO), CMONEW(NGO,NMO)
      DIMENSION EORBOD(    NMO), EORBNW(    NMO)
      DIMENSION SINTEG(NGO,NGO), OVRLAP(NMO)
      DIMENSION LORDER(NMO)
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQMXOL ***'')')
        CALL MQDEBG(LUNOUT,
     *  '   NGO','   NMO','INIACT','LASACT','INOFMT','     -','     -',
     *      NGO ,    NMO , INIACT , LASACT , INOFMT ,      0 ,      0 )
      END IF
C**** DETERMINE MAXIMUM OVERLAP ORBITAL ********************************
      DO I=1,NMO
        LORDER(I)=I
      END DO
      DO IOLD=1,INIACT-1
        SMAX=0.0D+00
        DO JNEW=IOLD,INIACT-1
          S=0.0D+00
          DO K=1,NGO
            DO L=1,NGO
              S=S+CMOINP(K,IOLD)*SINTEG(K,L)*CMOOLD(L,LORDER(JNEW))
            END DO
          END DO
          S=ABS(S)
          IF(S.GT.SMAX) THEN
            SMAX=S
            K           =LORDER(JNEW)
            LORDER(JNEW)=LORDER(IOLD)
            LORDER(IOLD)=K
          END IF
        END DO
        OVRLAP(IOLD)=SMAX
      END DO
      DO IOLD=INIACT,LASACT
        SMAX=0.0D+00
        DO JNEW=IOLD,LASACT
          S=0.0D+00
          DO K=1,NGO
            DO L=1,NGO
              S=S+CMOINP(K,IOLD)*SINTEG(K,L)*CMOOLD(L,LORDER(JNEW))
            END DO
          END DO
          S=ABS(S)
          IF(S.GT.SMAX) THEN
            SMAX=S
            K           =LORDER(JNEW)
            LORDER(JNEW)=LORDER(IOLD)
            LORDER(IOLD)=K
          END IF
        END DO
        OVRLAP(IOLD)=SMAX
      END DO
      DO IOLD=LASACT+1,NMO
        SMAX=0.0D+00
        DO JNEW=IOLD,NMO
          S=0.0D+00
          DO K=1,NGO
            DO L=1,NGO
              S=S+CMOINP(K,IOLD)*SINTEG(K,L)*CMOOLD(L,LORDER(JNEW))
            END DO
          END DO
          S=ABS(S)
          IF(S.GT.SMAX) THEN
            SMAX=S
            K           =LORDER(JNEW)
            LORDER(JNEW)=LORDER(IOLD)
            LORDER(IOLD)=K
          END IF
        END DO
        OVRLAP(IOLD)=SMAX
      END DO
C**** WRITE LORDER *****************************************************
C**** WRITE OVRLAP *****************************************************
      IF (MASWRK) THEN
         WRITE(LUNOUT,9110)
         WRITE(LUNOUT,9120) (LORDER(III),OVRLAP(III),III=1,NMO)
      END IF
 9110 FORMAT(/1X,'OVERLAP OF THE CANONICAL ORBITALS WITH PREVIOUS MO',
     *           ' INDEX:')
 9120 FORMAT(5(1X,I4,1P,E11.4,0P))
C**** REORDER ORBITALS *************************************************
      DO J=1,NMO
        EORBNW(J)=EORBOD(LORDER(J))
        DO I=1,NGO
          CMONEW(I,J)=CMOOLD(I,LORDER(J))
        END DO
      END DO
C**** PUNCH OUT ORBITALS ***********************************************
      CALL SEQREW(LUNPUN)
      WRITE(LUNPUN,'('' *** REORDERED FOCK ORBITAL ***''/'' $VEC'')')
      CALL MQPUSQ(LUNPUN,NGO,NMO,INOFMT,CMONEW)
      WRITE(LUNPUN,'('' $END'')')
C**** RETURN **********************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQMXWR
      SUBROUTINE MQMXWR(LUNOUT,ILENG,JLENG,NMAX,
     1                  LABMAX,
     2                  A     ,AMAX )
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
C**** DIMENSION ********************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DIMENSION LABMAX(NMAX,JLENG)
      DIMENSION A     (ILENG,JLENG) ,AMAX (NMAX,JLENG)
C**** GET LABMAX *******************************************************
      DO J=1,JLENG
        S=-1.0D+00
        DO I=1,ILENG
          AG=ABS(A(I,J))
          IF(AG.GT.S) THEN
            S   =AG
            IMAX=I
          END IF
        END DO
        AMAX  (1,J)=A(IMAX,J)
        LABMAX(1,J)=IMAX
        DO K=2,NMAX
          S=-1.0D+00
          AGMBEF=ABS(AMAX(K-1,J))
          DO I=1,ILENG
            AG=ABS(A(I,J))
            IF((AGMBEF.GT.AG).AND.(AG.GT.S)) THEN
              S   =AG
              IMAX=I
            END IF
          END DO
          AMAX  (K,J)=A(IMAX,J)
          LABMAX(K,J)=IMAX
        END DO
      END DO
C
C     WRITE STATES
C
      IF (MASWRK) THEN
         INILAB=1
         DO IWFN=1,JLENG
            WRITE(LUNOUT,9100) IWFN
            LASLAB=INILAB+4
            IF(LASLAB.GT.NMAX) LASLAB=NMAX
            DO J=1,JLENG
              WRITE(LUNOUT,9110) (LABMAX(I,J),I=INILAB,LASLAB)
              WRITE(LUNOUT,9120) (AMAX  (I,J),I=INILAB,LASLAB)
            END DO
            INILAB=LASLAB+1
         ENDDO
      END IF
      RETURN
C
 9100 FORMAT(/1X,'WAVEFUNCTION FOR STATE',I4)
 9110 FORMAT(1X,'CSF ',   5(2X,I10))
 9120 FORMAT(1X,'COEF',1P,5(2X,E10.3))
      END
C*MODULE MCQDPT  *DECK MQOCF1
      SUBROUTINE MQOCF1(LUNOUT,LPOUT ,LUNFIN,LUNFOU,INIACT,LASACT,
     *                  NOCF  ,NCSF  ,LENGTH,NOCFI ,
     *                  NSNSF ,I1EX1 ,I1EX2 ,LAB1  ,WORK  ,KREF  ,
     *                  KOCF1E)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER   NSNSF(NOCF+1)
      INTEGER   I1EX1(INIACT:LASACT,0:NOCF)
      INTEGER   I1EX2(INIACT:LASACT,0:NOCFI)
      INTEGER   LAB1(3,LENGTH)
      DIMENSION WORK(LENGTH)
      LOGICAL   KREF(NCSF), KOCF1E(NOCF)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /MQFTB2/NWFOU,NWFOUP,NWFOUG
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQOCF1 ***'')')
        CALL MQDEBG(LUNOUT,
     *  'LUNFIN','LUNFOU','INIACT','LASACT','  NOCF','  NCSF','LENGTH',
     *   LUNFIN , LUNFOU , INIACT , LASACT ,   NOCF ,   NCSF , LENGTH )
        CALL MQDEBG(LUNOUT,
     *  ' NOCFI','     -','     -','     -','     -','     -','     -',
     *    NOCFI ,      0 ,      0 ,      0 ,      0 ,      0 ,      0 )
      END IF
C**** DETERMINE NECESSARY OCF'S ****************************************
      DO IOCF=1,NOCF
        KOCF1E(IOCF)=.FALSE.
      END DO
      DO IOCF=1,NOCF
        DO ICSF=NSNSF(IOCF)+1,NSNSF(IOCF+1)
          IF(KREF(ICSF)) THEN
            DO I=INIACT,LASACT
              DO J=INIACT,LASACT
                K=I1EX2(I,I1EX1(J,IOCF))
                IF(K.NE.0) KOCF1E(K)=.TRUE.
              END DO
            END DO
            GO TO 100
          END IF
        END DO
 100    CONTINUE
      END DO
      N=0
      DO IOCF=1,NOCF
        IF(KOCF1E(IOCF)) N=N+1
      END DO
      IF (MASWRK) WRITE(LUNOUT,9100) NOCF,N
 9100 FORMAT(1X,'STEP ONE:  ORIGINAL SPACE PRODUCTS=',I13,
     *          ' REDUCED TO',I13)
C**** DISCARD UNNECESSARY 1-BODY GENERATORS ****************************
      CALL SEQREW(LUNFIN)
      CALL SEQREW(LUNFOU)
      NWFOU=0
      NWFOUP=0
      NWFOUG=0
 200  CONTINUE
      READ(LUNFIN) KCONTP,L1P,M2P,NWORDP
      CALL MQGENR(LUNFIN,3*NWORDP,NWORDP,LAB1,WORK)
      IF(.NOT.KOCF1E(M2P)) THEN
C        READ(LUNFIN)
        IF(KCONTP.EQ.0) RETURN
        GO TO 200
      END IF
      IF(KCONTP.EQ.0) THEN
        NWFOU=NWFOU+1
        NWFOUG=NWFOUG+1
        NWFOUP=NWFOUP+NWORDP
        WRITE(LUNFOU) KCONTP,L1P,M2P,NWORDP
        CALL MQGENW(LUNFOU,3*NWORDP,NWORDP,LAB1,WORK)
        RETURN
      END IF
  220 CONTINUE
      READ(LUNFIN) KCONT,L1,M2,NWORD
      IF(KOCF1E(M2)) THEN
        IF(KCONTP.EQ.100) THEN
          KCONTP=100
          NWFOUG=NWFOUG-1
        ELSE IF(KCONTP.EQ.10) THEN
          IF(L1P.NE.L1) KCONTP=1
        END IF
        NWFOU=NWFOU+1
        NWFOUG=NWFOUG+1
        NWFOUP=NWFOUP+NWORDP
        WRITE(LUNFOU) KCONTP,L1P,M2P,NWORDP
        CALL MQGENW(LUNFOU,3*NWORDP,NWORDP,LAB1,WORK)
        KCONTP=KCONT
        L1P   =L1
        M2P   =M2
        NWORDP=NWORD
        CALL MQGENR(LUNFIN,3*NWORDP,NWORDP,LAB1,WORK)
        IF(KCONTP.NE.0) GO TO 220
      END IF
      IF(KCONT.NE.0) THEN
C FOR PACKING BITS OF NEW MQGENR
        READ(LUNFIN)
C FOR ORIGINAL MQGENR
        READ(LUNFIN)
        GO TO 220
      END IF
      KCONTP=0
      NWFOU=NWFOU+1
      NWFOUG=NWFOUG+1
      NWFOUP=NWFOUP+NWORDP
      WRITE(LUNFOU) KCONTP,L1P,M2P,NWORDP
      CALL MQGENW(LUNFOU,3*NWORDP,NWORDP,LAB1,WORK)
C**** RETURN ***********************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQOCF2
      SUBROUTINE MQOCF2(LUNOUT,LPOUT ,LUNFIN,LUNFOU,INIACT,LASACT,
     *                  NOCF  ,NCSF  ,LENGTH,
     *                  NSNSF ,LAB1  ,WORK  ,KREF  ,KOCF  )
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER   NSNSF(NOCF+1)
      INTEGER   LAB1(3,LENGTH)
      DIMENSION WORK(LENGTH)
      LOGICAL   KREF(NCSF), KOCF(NOCF)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /MQFTC2/NWFOU,NWFOUP,NWFOUG
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQOCF2 ***'')')
        CALL MQDEBG(LUNOUT,
     *  'LUNFIN','LUNFOU','INIACT','LASACT','  NOCF','  NCSF','     -',
     *   LUNFIN , LUNFOU , INIACT , LASACT ,   NOCF ,   NCSF ,      0 )
      END IF
C**** DETERMINE NECESSARY OCF'S ****************************************
      DO IOCF=1,NOCF
        KOCF(IOCF)=.FALSE.
      END DO
      DO IOCF=1,NOCF
        DO ICSF=NSNSF(IOCF)+1,NSNSF(IOCF+1)
C          IF(KREF(ICSF)) THEN
C            KOCF(IOCF)=.TRUE.
C            GO TO 100
C          END IF
           KOCF(IOCF)=KOCF(IOCF).OR.KREF(ICSF)
        END DO
      END DO
      N=0
      DO IOCF=1,NOCF
        IF(KOCF(IOCF)) N=N+1
      END DO
      IF (MASWRK) WRITE(LUNOUT,9100) NOCF,N
 9100 FORMAT(1X,'STEP TWO: NUMBER OF SPACE PRODUCTS=',I13,
     *          ' REDUCED TO',I13)
C
C**** DISCARD UNNECESSARY 1-BODY GENERATORS ****************************
      CALL SEQREW(LUNFIN)
      CALL SEQREW(LUNFOU)
      NWFOU=0
      NWFOUP=0
      NWFOUG=0
 200  READ(LUNFIN) KCONTP,L1P,M2P,NWORDP
      CALL MQGENR(LUNFIN,3*NWORDP,NWORDP,LAB1,WORK)
      IF(.NOT.KOCF(M2P)) THEN
C        READ(LUNFIN)
        IF(KCONTP.EQ.0) RETURN
        GO TO 200
      END IF
      IF(KCONTP.EQ.0) THEN
        NWFOU=NWFOU+1
        NWFOUG=NWFOUG+1
        NWFOUP=NWFOUP+NWORDP
        WRITE(LUNFOU) KCONTP,L1P,M2P,NWORDP
        CALL MQGENW(LUNFOU,3*NWORDP,NWORDP,LAB1,WORK)
        RETURN
      END IF
 220  CONTINUE
      READ(LUNFIN) KCONT,L1,M2,NWORD
      IF(KOCF(M2)) THEN
        IF(KCONTP.EQ.100) THEN
          KCONTP=100
          NWFOUG=NWFOUG-1
        ELSE IF(KCONTP.EQ.10) THEN
          IF(L1P.NE.L1) KCONTP=1
        END IF
        NWFOU=NWFOU+1
        NWFOUG=NWFOUG+1
        NWFOUP=NWFOUP+NWORDP
        WRITE(LUNFOU) KCONTP,L1P,M2P,NWORDP
        CALL MQGENW(LUNFOU,3*NWORDP,NWORDP,LAB1,WORK)
        KCONTP=KCONT
        L1P   =L1
        M2P   =M2
        NWORDP=NWORD
        CALL MQGENR(LUNFIN,3*NWORDP,NWORDP,LAB1,WORK)
        IF(KCONTP.NE.0) GO TO 220
      END IF
      IF(KCONT.NE.0) THEN
C FOR PACKING BITS OF NEW MQGENR
        READ(LUNFIN)
C FOR ORIGINAL MQGENR
        READ(LUNFIN)
        GO TO 220
      END IF
      KCONTP=0
      NWFOU=NWFOU+1
      NWFOUG=NWFOUG+1
      NWFOUP=NWFOUP+NWORDP
      WRITE(LUNFOU) KCONTP,L1P,M2P,NWORDP
      CALL MQGENW(LUNFOU,3*NWORDP,NWORDP,LAB1,WORK)
C**** RETURN ***********************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQORB2
      SUBROUTINE MQORB2(LUNOUT,LPOUT ,LUNERI,LUNFT0,NCI   ,NSTATE,
     *                  NMO   ,NDOUB ,NMOACT,NGO   ,MAXCSF,
     *                  EIGVEC,AVECOE,CMO   ,EORB  ,HCORE ,GEN1AO,
     *                  GEN1  ,GFCKAO,XWORK ,IX    ,LABEL1,WORKGE,
     *                  CANAVE,CANSPE)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL PACK2E,CANAVE,CANSPE,DIRTRF
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,IPOPLE
      COMMON /PCKLAB/ LABSIZ
      COMMON /IOFILE/ IR,IW,IP,IS1,IPK,IDAF,NAV,IODA(400)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      DIMENSION EIGVEC(NCI,NSTATE), AVECOE(NSTATE)
      DIMENSION CMO(NGO,NMO), EORB(NMO)
      DIMENSION HCORE(NMO,NMO)
      DIMENSION GEN1AO(NGO,NGO)
      DIMENSION GEN1  (NDOUB+1:NDOUB+NMOACT,NDOUB+1:NDOUB+NMOACT)
      DIMENSION GFCKAO(NGO,NGO)
      DIMENSION XWORK(NINTMX), IX(NINTMX)
      INTEGER   LABEL1(4,MAXCSF)
      DIMENSION WORKGE(MAXCSF)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TRFOPT/ CUTTRF,NWDTRF,MPTRAN,ITRFAO,NOSYMT,DIRTRF
      COMMON /MQFT02/NWFT0,NWFT0P
C**** THRESHOLD ZERO ***************************************************
      THRZRO=1.0D-15
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQORB2 ***'')')
        CALL MQDEBG(LUNOUT,
     *  'LUNERI','LUNFT0','   NCI','NSTATE','   NMO',' NDOUB','NMOACT',
     *   LUNERI , LUNFT0 ,    NCI , NSTATE ,    NMO ,  NDOUB , NMOACT )
        CALL MQDEBG(LUNOUT,
     *  '   NGO','MAXCSF','     -','     -','     -','     -','     -',
     *      NGO , MAXCSF ,      0 ,      0 ,      0 ,      0 ,      0 )
        IF(LPOUT.LE.-5) THEN
          DO I=1,NMO
            EORB(I)=0.0D+00
          END DO
          WRITE(LUNOUT,'('' *** ORBITAL ***'')')
          CALL MQMOWR(LUNOUT,NGO,NMO,EORB,CMO)
        END IF
      END IF
C**** NORMALIZE AVECOE *************************************************
      S=0.0D+00
      DO I=1,NSTATE
        S=S+AVECOE(I)
      END DO
      IF(ABS(S).GT.THRZRO.AND..NOT.CANAVE) THEN
        S=1.0D+00/S
C       IF(CANAVE) S=S/NUMCI
C       THAT IS, EACH MULTIPLICITY GETS FIXED EQUAL WEIGHT.
        DO I=1,NSTATE
          AVECOE(I)=AVECOE(I)*S
        END DO
      END IF
C**** SET CONSTANTS ****************************************************
      INIDOC=1
      LASDOC=NDOUB
      INIACT=NDOUB+1
      LASACT=NDOUB+NMOACT
C***********************************************************************
C****                  *************************************************
C**** PREPARE MATRICES *************************************************
C****                  *************************************************
C**** CLEAR ARRAYS *****************************************************
      DO J=INIACT,LASACT
        DO I=INIACT,LASACT
          GEN1(I,J)=0.0D+00
        END DO
      END DO
      DO J=1,NGO
        DO I=1,NGO
          GEN1AO(I,J)=0.0D+00
          GFCKAO(I,J)=0.0D+00
        END DO
      END DO
      DO I=1,NMO
        EORB(I)=0.0D+00
      END DO
C
C**** GET AVERAGED ONE-PARTICLE DENSITY MATRIX *************************
C
      CALL SEQREW(LUNFT0)
      DO INWFT0=1,NWFT0
C  100 CONTINUE
        READ(LUNFT0) KCONT,NWORD
        CALL MQGENR(LUNFT0,4*NWORD,NWORD,LABEL1,WORKGE)
        DO M=1,NWORD
          M1 =LABEL1(1,M)
          M2 =LABEL1(2,M)
          LP =LABEL1(3,M)
          LQ =LABEL1(4,M)
          VAL=WORKGE(M)
          DO I=1,NSTATE
            GEN1(LP,LQ)=GEN1(LP,LQ)
     *      +AVECOE(I)*EIGVEC(M1,I)*VAL*EIGVEC(M2,I)
          END DO
        END DO
C      IF(KCONT.NE.0) GO TO 100
      END DO
      IF (GOPARR) CALL DDI_GSUMF(2538,GEN1,NMOACT*NMOACT)
C
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LE.-10 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQORB2 ***''/
     *    '' *** ONE PARTICLE DENSITY MATRIX ***'')')
        WRITE(LUNOUT,*) 'NWFT0,KCONT,NWORD=',NWFT0,KCONT,NWORD
        CALL MQWMAG(LUNOUT,GEN1,NMOACT,NMOACT,NMOACT,'SYMM')
      END IF
C**** GET AVERAGED ONE-PARTICLE DENSITY MATRIX ON AO BASE **************
      DO K=INIDOC,LASDOC
        DO J=1,NGO
          S=CMO(J,K)*2.0D+00
          DO I=1,NGO
            GEN1AO(I,J)=GEN1AO(I,J)+CMO(I,K)*S
          END DO
        END DO
      END DO
      DO L=INIACT,LASACT
        DO K=INIACT,LASACT
          DO J=1,NGO
            S=CMO(J,L)*GEN1(K,L)
            DO I=1,NGO
              GEN1AO(I,J)=GEN1AO(I,J)+CMO(I,K)*S
            END DO
          END DO
        END DO
      END DO
C
C     READ MULTI-STATE AVERAGED DENSITY
C     THIS DENSITY IS OBTAINED USING CI COEFF FROM A CI WITH
C     NON-CANONICAL ORBITALS. THIS IS A BIT DIFFERENT FROM USUAL
C     MCQDPT, WHERE THE ORBITAL ENERGIES ARE OBTAINED FROM THE CI
C     WITH CANOCICAL ORBITALS.  IF SEVERAL MULTIPLICITIES ARE
C     PRESENT, HOWEVER, THEN TO ACHIEVE SIMILAR EFFECT WE MUST DO:
C     1. CI WITH NON-CANONICAL (=INPUT) ORBITALS TO GET AV. DENSITY
C        AND CANONICAL ORBITALS
C     2. CI WITH CANONICAL ORBITALS TO GET AV. DENSITY WITH CAN. ORB
C     3. GET ORB ENERGIES.
C
C     AT PRESENT, WE DO STEP 1 FOR ALL MULTIPLICITIES, BUT COMBINE
C     STEP 2 AND 3, FOR EACH MUPLIPLICITY.
C     WE ACHIEVE THE GOAL OF HAVING SAME ORBITAL ENERGIES FOR ALL
C     MULTIPLICITIES, WHICH IS VERY IMPORTANT TO GET DENOMINATORS IN
C     THE PT. WE ALSO GET THE SAME SET OF CANONICAL ORBITALS.
C
      IF(CANAVE) CALL DAREAD(IDAF,IODA,GEN1AO,NGO*NGO,393,0)
      IF(CANSPE) THEN
        IF(NOCC.EQ.0) THEN
          CALL DAWRIT(IDAF,IODA,GEN1AO,NGO*NGO,393,0)
C
C         WE MUST BE IN THE ORBITAL ENERGY GENERATING RUN, SO SAVE THE
C         DENSITY FOR FUTURE REAL MCQDPT RUNS (SO THAT ALL RUNS HAVE
C         THE SAME ORBITAL ENERGIES AS WELL AS ORBITALS THEMSELVES).
C
        ELSE
          CALL DAREAD(IDAF,IODA,GEN1AO,NGO*NGO,393,0)
        ENDIF
      ENDIF
C**** GET FOCK MATRIX ON AO BASE ***************************************
      CALL SEQREW(LUNERI)
      NIX=0
 200  CONTINUE
        CALL PREAD(LUNERI,XWORK,IX,NX,NINTMX)
        NIX=NIX+1
        IF(NX.EQ.0) GO TO 220
        MX=IABS(NX)
        IF(MX.GT.NINTMX) CALL ABRT
        IF(GOPARR.AND.ITRFAO.EQ.1.AND.MOD(NIX-1,NPROC).NE.ME) GO TO 210
        DO IERI=1,MX
          VERI=XWORK(IERI)
C
                       NPACK = IERI
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
                       IA = IPACK
                       IB = JPACK
                       IC = KPACK
                       ID = LPACK
C
          CDP=GEN1AO(IC,ID)*VERI*2.0D+00
          ABP=GEN1AO(IA,IB)*VERI*2.0D+00
          BDM=-0.5D+00*GEN1AO(IB,ID)*VERI
          BCM=-0.5D+00*GEN1AO(IB,IC)*VERI
          ADM=-0.5D+00*GEN1AO(IA,ID)*VERI
          ACM=-0.5D+00*GEN1AO(IA,IC)*VERI
C
          GFCKAO(IA,IB)=GFCKAO(IA,IB)+CDP
          GFCKAO(IB,IA)=GFCKAO(IB,IA)+CDP
          GFCKAO(IC,ID)=GFCKAO(IC,ID)+ABP
          GFCKAO(ID,IC)=GFCKAO(ID,IC)+ABP
C
          GFCKAO(IA,IC)=GFCKAO(IA,IC)+BDM
          GFCKAO(IA,ID)=GFCKAO(IA,ID)+BCM
          GFCKAO(IB,IC)=GFCKAO(IB,IC)+ADM
          GFCKAO(IB,ID)=GFCKAO(IB,ID)+ACM
          GFCKAO(IC,IA)=GFCKAO(IC,IA)+BDM
          GFCKAO(IC,IB)=GFCKAO(IC,IB)+ADM
          GFCKAO(ID,IA)=GFCKAO(ID,IA)+BCM
          GFCKAO(ID,IB)=GFCKAO(ID,IB)+ACM
        END DO
 210  CONTINUE
      IF(NX.GT.0) GO TO 200
 220  CONTINUE
C**** GET FOCK MATRIX ON AO BASE ***************************************
C**** GET FOCK MATRIX ON MO BASE ***************************************
      DO I=1,NMO
        DO L=1,NGO
          DO K=1,NGO
            EORB(I)=EORB(I)+CMO(K,I)*CMO(L,I)*GFCKAO(K,L)
          END DO
        END DO
      END DO
      IF(GOPARR) CALL DDI_GSUMF(2506,EORB,NMO)
      DO I=1,NMO
        EORB(I)=EORB(I)+HCORE(I,I)
      END DO
C
      IF (MASWRK) THEN
         WRITE(LUNOUT,9100)
         CALL MQWMAG(LUNOUT,GEN1,NMOACT,NMOACT,NMOACT,'SYMM')
         WRITE(LUNOUT,9110)
         WRITE(LUNOUT,9111)
         WRITE(LUNOUT,9120) (EORB(III),III=1,NDOUB)
         WRITE(LUNOUT,9112)
         WRITE(LUNOUT,9120) (EORB(III),III=NDOUB+1,NDOUB+NMOACT)
         WRITE(LUNOUT,9113)
         WRITE(LUNOUT,9120) (EORB(III),III=NDOUB+NMOACT+1,NMO)
      END IF
      RETURN
 9100 FORMAT(/1X,'*** ONE PARTICLE DENSITY MATRIX OVER CANONICAL',
     *           ' ORBITALS ***')
 9110 FORMAT(/1X,'*** ORBITAL ENERGIES OF THE CANONICAL ORBITALS ***')
 9111 FORMAT(1X,'DOUBLY OCCUPIED ORBITALS:')
 9112 FORMAT(1X,'ACTIVE ORBITALS:')
 9113 FORMAT(1X,'VIRTUAL ORBITALS:')
 9120 FORMAT(1X,1P,5E15.6)
      END
C*MODULE MCQDPT  *DECK MQORD1
      SUBROUTINE MQORD1(LUNOUT,LPOUT ,LUNERI,LUNOER,LUNJMA,IDMWTH,DMOAO,
     *                  NGO   ,NGOIJ ,IWIDTH,THRERI,
     *                  LIJGO ,XWORK ,IX    ,LABEL ,WORK  ,VAOAO ,JFLG,
     *                  NMO   ,LIJMO ,IJLAST,JLAST ,
     *                  CFMO  ,VMOAO ,S1,S2,OERFLG)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL PACK2E
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,IPOPLE
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /PCKLAB/ LABSIZ
      DIMENSION LIJGO (NGO   ,NGO   )
      DIMENSION XWORK(NINTMX), IX(NINTMX)
      INTEGER   LABEL(NGOIJ)
      DIMENSION WORK(NGOIJ)
      DIMENSION VAOAO(NGOIJ,IWIDTH)
      DIMENSION JFLG(0:NPROC-1)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      DIMENSION LIJMO(NMO,NMO),CFMO(NGO,NGO)
      DIMENSION VMOAO(IJLAST),S1(NGO,NGO),S2(NGO,NGO)
C
      INTEGER DMOAO
      LOGICAL OERFLG
C
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQORD1 ***'')')
        CALL MQDEBG(LUNOUT,
     *  'LUNERI','LUNOER','   NGO',' NGOIJ','IWIDTH','NINTMX','IDMWTH',
     *   LUNERI , LUNOER ,    NGO ,  NGOIJ , IWIDTH , NINTMX , IDMWTH )
      END IF
C**** START ************************************************************
C
      NREAD=0
      IF (IDMWTH.GT.0) THEN
         NREAD=(IJLAST-1)/IDMWTH+1
         IF (NREAD.EQ.1) THEN
            CALL DDI_DISTRIB(DMOAO,MASTER,ILOC,IHIC,JLOC,JHIC)
            IDMWHP=(JHIC-JLOC)+1
            CALL DDI_DISTRIB(DMOAO,ME,ILOC,IHIC,JLOC,JHIC)
         END IF
      END IF
C
      IF (.NOT.OERFLG) THEN
C----------------------------------------------------------- NO MQORD0
      CALL SEQREW(LUNJMA)
      KCONT=1
      INIT=1
  100 CONTINUE
        INITM1=INIT-1
C        LAST=INITM1+IWIDTH
        LAST=INITM1+IWIDTH*NPROC
        IF(LAST.GT.NGOIJ) LAST=NGOIJ
C**** READ ERI VALUES FROM HONDO FILE **********************************
C        DO J=1,LAST-INIT+1
        CALL VCLR(VAOAO,1,NGOIJ*IWIDTH)
        DO KAP=0,NPROC-1
            JFLG(KAP)=1
        END DO
        IFLG=1
        KAP=-1
C
        NX=0
C
        CALL SEQREW(LUNERI)
  200   CONTINUE
          KAP = MOD(KAP+1,NPROC)
          IF(KAP.EQ.MASTER) THEN
            IF(IFLG.EQ.0) GOTO 290
            IFLG= 0
          END IF
          LFSEND=0
          IF (JFLG(KAP).GT.0) THEN
C
          IF (KAP.EQ.ME) THEN
             CALL PREAD(LUNERI,XWORK,IX,NX,NINTMX)
             IF(NX.NE.0) THEN
                MX=IABS(NX)
                IF(MX.GT.NINTMX) CALL ABRT
                DO IERI=1,MX
                   VERI=XWORK(IERI)
C
                       NPACK = IERI
                       IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                         LABLE = IX( 2*NPACK - 1 )
                         IPACK = ISHFT( LABLE, -16 )
                         JPACK = IAND(  LABLE, 65535 )
                         LABLE = IX( 2*NPACK     )
                         KPACK = ISHFT( LABLE, -16 )
                         LPACK = IAND(  LABLE, 65535 )
#endif
#if defined(SPEC_CPU_ILP64)
                         LABLE = IX(NPACK)
                         IPACK = ISHFT( LABLE, -48 )
                         JPACK = IAND( ISHFT( LABLE, -32 ), 65535 )
                         KPACK = IAND( ISHFT( LABLE, -16 ), 65535 )
                         LPACK = IAND( LABLE, 65535 )
#endif
                       ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                         LABLE = IX(NPACK)
                         IPACK = ISHFT( LABLE, -24 )
                         JPACK = IAND( ISHFT( LABLE, -16 ), 255 )
                         KPACK = IAND( ISHFT( LABLE,  -8 ), 255 )
                         LPACK = IAND( LABLE, 255 )
#endif
#if defined(SPEC_CPU_ILP64)
                         IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                           LABLE = IX( NPACK/2 )
                           IPACK = IAND( ISHFT( LABLE, -24 ), 255 )
                           JPACK = IAND( ISHFT( LABLE, -16 ), 255 )
                           KPACK = IAND( ISHFT( LABLE,  -8 ), 255 )
                           LPACK = IAND( LABLE, 255 )
                         ELSE
                           LABLE = IX( (NPACK/2)+1 )
                           IPACK = ISHFT( LABLE, -56 )
                           JPACK = IAND( ISHFT( LABLE, -48 ), 255 )
                           KPACK = IAND( ISHFT( LABLE, -40 ), 255 )
                           LPACK = IAND( ISHFT( LABLE, -32 ), 255 )
                         END IF
#endif
                       END IF
                       IA = IPACK
                       IB = JPACK
                       IC = KPACK
                       ID = LPACK
C
                   IAB              =LIJGO(IB,IA)
                   ICD              =LIJGO(ID,IC)
                   IF (IAB.GE.INIT .AND. IAB.LE.LAST) LFSEND=1
                   IF (ICD.GE.INIT .AND. ICD.LE.LAST) LFSEND=1
                END DO
             END IF
C
          ENDIF
C
          CALL DDI_BCAST(2531,'I',LFSEND,1,KAP)
          CALL DDI_BCAST(2532,'I',NX,1,KAP)
          JFLG(KAP)=NX
          IF (NX.GT.0) IFLG=1
C
C          IF(MASWRK) WRITE(LUNOUT,*) IFLG,KAP,NX
C
          IF (NX.NE.0) THEN
             IF (LFSEND.EQ.1) THEN
                MX=IABS(NX)
                IF (LABSIZ.EQ.1) THEN
                   IF(NWDVAR.EQ.2) MX2 = MX
                   IF(NWDVAR.EQ.1) MX2 = (MX+1)/2
                ELSE IF (LABSIZ.EQ.2) THEN
                   IF(NWDVAR.EQ.2) MX2 = MX*2
                   IF(NWDVAR.EQ.1) MX2 = MX
                END IF
C
                CALL DDI_BCAST(2535,'F',XWORK,MX,KAP)
                CALL DDI_BCAST(2534,'I',IX,MX2,KAP)
C                CALL DDI_BCAST(2534,'F',IX,MX,KAP)
C
                DO IERI=1,MX
                   VERI=XWORK(IERI)
C
                       NPACK = IERI
                       IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                         LABLE = IX( 2*NPACK - 1 )
                         IPACK = ISHFT( LABLE, -16 )
                         JPACK = IAND(  LABLE, 65535 )
                         LABLE = IX( 2*NPACK     )
                         KPACK = ISHFT( LABLE, -16 )
                         LPACK = IAND(  LABLE, 65535 )
#endif
#if defined(SPEC_CPU_ILP64)
                         LABLE = IX(NPACK)
                         IPACK = ISHFT( LABLE, -48 )
                         JPACK = IAND( ISHFT( LABLE, -32 ), 65535 )
                         KPACK = IAND( ISHFT( LABLE, -16 ), 65535 )
                         LPACK = IAND( LABLE, 65535 )
#endif
                       ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                         LABLE = IX(NPACK)
                         IPACK = ISHFT( LABLE, -24 )
                         JPACK = IAND( ISHFT( LABLE, -16 ), 255 )
                         KPACK = IAND( ISHFT( LABLE,  -8 ), 255 )
                         LPACK = IAND( LABLE, 255 )
#endif
#if defined(SPEC_CPU_ILP64)
                         IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                           LABLE = IX( NPACK/2 )
                           IPACK = IAND( ISHFT( LABLE, -24 ), 255 )
                           JPACK = IAND( ISHFT( LABLE, -16 ), 255 )
                           KPACK = IAND( ISHFT( LABLE,  -8 ), 255 )
                           LPACK = IAND( LABLE, 255 )
                         ELSE
                           LABLE = IX( (NPACK/2)+1 )
                           IPACK = ISHFT( LABLE, -56 )
                           JPACK = IAND( ISHFT( LABLE, -48 ), 255 )
                           KPACK = IAND( ISHFT( LABLE, -40 ), 255 )
                           LPACK = IAND( ISHFT( LABLE, -32 ), 255 )
                         END IF
#endif
                       END IF
                       IA = IPACK
                       IB = JPACK
                       IC = KPACK
                       ID = LPACK
C
                   IAB              =LIJGO(IB,IA)
                   ICD              =LIJGO(ID,IC)
                   IF(IA.EQ.IB)   VERI=2.0D+00*VERI
                   IF(IC.EQ.ID)   VERI=2.0D+00*VERI
                   IF(IAB.EQ.ICD) VERI=2.0D+00*VERI
                   IABDUM=IAB-INITM1
                   ICDDUM=ICD-INITM1
                   IF(IAB.GE.INIT .AND. IAB.LE.LAST
     *                  .AND. MOD(IAB-1,NPROC).EQ.ME)
     *                  VAOAO(ICD,(IABDUM-1)/NPROC+1)=VERI
                   IF(ICD.GE.INIT .AND. ICD.LE.LAST
     *                  .AND. MOD(ICD-1,NPROC).EQ.ME)
     *                  VAOAO(IAB,(ICDDUM-1)/NPROC+1)=VERI
                END DO
C
             END IF
          END IF
C
          END IF
C
          GO TO 200
C          IF (NX.GT.0) GO TO 200
  290   CONTINUE
C
CC**** WRITE VAOAO *************************************************
C
        DO IAB=INIT,LAST
          IABDUM=IAB-INITM1
          IF (MOD(IAB-1,NPROC).EQ.ME) THEN
C
             CALL MQTRF1(LUNOUT,LPOUT ,
     *            NGO   ,NGOIJ ,NMO   ,IJLAST,JLAST ,THRERI,
     *            LIJGO ,LIJMO ,CFMO  ,
     *            VAOAO(1,(IABDUM-1)/NPROC+1),VMOAO ,S1    ,S2)
C
C
             IF (IDMWTH.GT.0.AND.NREAD.EQ.1) THEN
                CALL VCLR(VAOAO(1,(IABDUM-1)/NPROC+1),1,IDMWTH)
                DO IJ=1,IJLAST
                   IJDUM=(IJ-1)/NPROC+MOD(IJ-1,NPROC)*IDMWHP+1
                   VAOAO(IJDUM,(IABDUM-1)/NPROC+1)=VMOAO(IJ)
                END DO
                IABTMP = IAB
                CALL DDI_PUT(DMOAO,IABTMP,IABTMP,1,IDMWTH,
     *                       VAOAO(1,(IABDUM-1)/NPROC+1))
             ELSE
                IF(IAB.EQ.NGOIJ) KCONT=0
                N=0
                DO IJ=1,IJLAST
                   V=VMOAO(IJ)
                   IF(ABS(V).GE.THRERI) THEN
                      N=N+1
                      LABEL(N)=IJ
                      WORK (N)=V
                   END IF
                END DO
                WRITE(LUNJMA) KCONT,IAB,N
                CALL MQGENW(LUNJMA,N,N,LABEL,WORK)
             END IF
          END IF
        END DO
C        INIT=INIT+IWIDTH
        INIT=INIT+IWIDTH*NPROC
      IF(INIT.LE.NGOIJ) GO TO 100
C***************************************************************
      ELSE
C---------------------------------------------------------- WITH MQORD0
         CALL SEQREW(LUNJMA)
         CALL SEQREW(LUNOER)
         KCONT=1
         DO IAB=1,NGOIJ
            IF (MOD(IAB-1,NPROC).EQ.ME) THEN
               READ(LUNOER) K,ICD,N
               IF (IAB.NE.ICD) THEN
                  IF(MASWRK) WRITE(LUNOUT,*) 'IAB,K,ICD,N=',IAB,K,ICD,N
                  CALL ABRT
               END IF
               CALL MQGENR(LUNOER,N,N,LABEL,WORK)
               CALL VCLR(VAOAO,1,NGOIJ)
               DO I=1,N
                  VAOAO(LABEL(I),1)=WORK(I)
               END DO
C
               CALL MQTRF1(LUNOUT,LPOUT ,
     *            NGO   ,NGOIJ ,NMO   ,IJLAST,JLAST ,THRERI,
     *            LIJGO ,LIJMO ,CFMO  ,
     *            VAOAO,VMOAO ,S1    ,S2)
C
               IF (IDMWTH.GT.0.AND.NREAD.EQ.1) THEN
                  CALL VCLR(VAOAO,1,IDMWTH)
                  DO IJ=1,IJLAST
                     IJDUM=(IJ-1)/NPROC+MOD(IJ-1,NPROC)*IDMWHP+1
                     VAOAO(IJDUM,1)=VMOAO(IJ)
                  END DO
                  IABTMP = IAB
                  CALL DDI_PUT(DMOAO,IABTMP,IABTMP,1,IDMWTH,VAOAO)
               ELSE
                  IF(IAB.EQ.NGOIJ) KCONT=0
                  N=0
                  DO IJ=1,IJLAST
                     V=VMOAO(IJ)
                     IF(ABS(V).GE.THRERI) THEN
                        N=N+1
                        LABEL(N)=IJ
                        WORK (N)=V
                     END IF
                  END DO
                  WRITE(LUNJMA) KCONT,IAB,N
                  CALL MQGENW(LUNJMA,N,N,LABEL,WORK)
               END IF
            END IF
         END DO
C
      END IF
C-----------------------------------------------------------------------
C
      RETURN
      END
C*MODULE MCQDPT  *DECK MQORD2
      SUBROUTINE MQORD2(LUNOUT,LPOUT ,LUNJMA,LUNJM1,LUNJM2,LUNJM3,
     *                  IDMWTH,DMOAO,
     *                  INI1  ,LAS1  ,INI2  ,LAS2  ,INI3  ,LAS3  ,
     *                  NGOIJ ,IJLAST,IWIDTH,ISWTCH,THRERI,
     *                  LABEL ,WORK  ,VMOAOI,VMOAOO,
     *                  NGO,NMO,NMOINT,MAXERI,S3,MOSYM,VMOAO,
     *                  LIJGO,LIJMO,LMOIJI,LMOIJJ,CFMO,S2)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER   LABEL(NGOIJ)
      DIMENSION WORK (NGOIJ)
      DIMENSION VMOAOI(IJLAST), VMOAOO(NGOIJ,IWIDTH)
C      DIMENSION VMOAOI(IJLAST), VMOAOO(IWIDTH,NGOIJ)
      DIMENSION LIJGO (NGO,NGO), LIJMO (NMO,NMO)
      DIMENSION LMOIJI(IJLAST), LMOIJJ(IJLAST)
      DIMENSION CFMO  (NGO,NGO),S3(NGO,NMO),VMOAO(NGOIJ)
      DIMENSION MOSYM(NMO),S2(NGO,NGO)
      INTEGER DMOAO
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQORD2 ***'')')
        CALL MQDEBG(LUNOUT,
     *  'LUNJMA','LUNJM1','LUNJM2','LUNJM3','  INI1','  LAS1','  INI2',
     *   LUNJMA , LUNJM1 , LUNJM2 , LUNJM3 ,   INI1 ,   LAS1 ,   INI2 )
        CALL MQDEBG(LUNOUT,
     *  '  LAS2','  INI3','  LAS3',' NGOIJ','IJLAST','IWIDTH','ISWTCH',
     *     LAS2 ,   INI3 ,   LAS3 ,  NGOIJ , IJLAST , IWIDTH , ISWTCH )
      END IF
C**** START ************************************************************
C
      CALL SEQREW(LUNJM1)
      CALL SEQREW(LUNJM2)
      CALL SEQREW(LUNJM3)
C
      IF (IDMWTH.LE.0) THEN
C
      INIT=1
  100 CONTINUE
        INITM1=INIT-1
        LAST=INITM1+IWIDTH
        IF(LAST.GT.IJLAST) LAST=IJLAST
C**** READ ERI VALUES FROM HONDO FILE **********************************
        CALL SEQREW(LUNJMA)
        DO IAB=1,NGOIJ
           IF (MOD(IAB-1,NPROC).EQ.ME) THEN
              READ(LUNJMA) KDUM,IRS,NWORD
              CALL MQGENR(LUNJMA,NWORD,NWORD,LABEL,WORK)
C              CALL VCLR(VMOAOI,1,IJLAST)
              DO IMO=1,IJLAST
                 VMOAOI(IMO)=0.0D+00
              END DO
              DO IERI=1,NWORD
                 VMOAOI(LABEL(IERI))=WORK(IERI)
              END DO
C**** ORDER VMOAO ******************************************************
              DO IJ=INIT,LAST
                 IJDUM=IJ-INITM1
                 VMOAOO(IRS,IJDUM)=VMOAOI(IJ)
C                    VMOAOO(IJDUM,IRS)=VMOAOI(IJ)
              END DO
           END IF
        END DO
C
        IF (GOPARR) THEN
          DO IAB=1,NGOIJ
C            CALL DDI_BCAST(2507,'F',
C     *            VMOAOO(1,IAB),LAST-INIT+1,MOD(IAB-1,NPROC))
             IF( MOD(IAB-1,NPROC).EQ.ME) THEN
                DO IJ=1,LAST-INIT+1
                   VMOAO(IJ)=VMOAOO(IAB,IJ)
                END DO
             END IF
              CALL DDI_BCAST(2507,'F',
     *            VMOAO,LAST-INIT+1,MOD(IAB-1,NPROC))
             IF( MOD(IAB-1,NPROC).NE.ME) THEN
                DO IJ=1,LAST-INIT+1
                   VMOAOO(IAB,IJ)=VMOAO(IJ)
                END DO
             END IF
          END DO
        END IF
C
        DO IJ=INIT,LAST
           IF(MOD(IJ-1,NPROC).EQ.ME) THEN
              IJDUM=IJ-INITM1
C                 DO IAB=1,NGOIJ
C                    VMOAO(IAB)=VMOAOO(IJDUM,IAB)
C                 END DO
C
              IJX = IJ
              CALL MQTRF2(LUNOUT,LPOUT ,LUNJM1,LUNJM2,LUNJM3,
     *              INI1  ,LAS1  ,INI2  ,LAS2  ,
     *              NGO   ,NGOIJ ,NMO   ,IJLAST,NMOINT,MAXERI,
     *              ISWTCH,THRERI,
     *              LIJGO ,LIJMO ,LMOIJI,LMOIJJ,CFMO  ,
     *              VMOAOO(1,IJDUM),
     *              VMOAOI,S3    ,LABEL,WORK,
     *              MOSYM ,IJX,S2)
C     *             VMOAO,
           END IF
        END DO
C
        INIT=INIT+IWIDTH
      IF(INIT.LE.IJLAST) GO TO 100
C
C
C --------------------------------------------------------------------
C
      ELSE
C
      CALL DDI_DISTRIB(DMOAO,MASTER,ILOC,IHIC,JLOC,JHIC)
      IDMWHP=(JHIC-JLOC)+1
      CALL DDI_DISTRIB(DMOAO,ME,ILOC,IHIC,JLOC,JHIC)
      NREAD=(IJLAST-1)/IDMWTH+1
C
      INIT=1
  500 CONTINUE
        INITM1=INIT-1
        LAST=INITM1+IWIDTH
        IF(LAST.GT.IJLAST) LAST=IJLAST
        IF (NREAD.NE.1) THEN
C**** READ ERI VALUES FROM HONDO FILE **********************************
           CALL SEQREW(LUNJMA)
           DO IAB=1,NGOIJ
              IF (MOD(IAB-1,NPROC).EQ.ME) THEN
                 READ(LUNJMA) KDUM,IRS,NWORD
                 CALL MQGENR(LUNJMA,NWORD,NWORD,LABEL,WORK)
                 CALL VCLR(VMOAOI,1,IJLAST)
                 DO IERI=1,NWORD
                    VMOAOI(LABEL(IERI))=WORK(IERI)
                 END DO
C**** ORDER VMOAO ******************************************************
                 CALL VCLR(VMOAO,1,IDMWTH)
                 DO IJ=INIT,LAST
                    IJDUM=IJ-INITM1
                    IJDUM=(IJDUM-1)/NPROC+MOD(IJDUM-1,NPROC)*IDMWHP+1
                    VMOAO(IJDUM)=VMOAOI(IJ)
                 END DO
                 IABTMP = IAB
                 CALL DDI_PUT(DMOAO,IABTMP,IABTMP,1,IDMWTH,VMOAO)
              END IF
           END DO
        END IF
C
        CALL DDI_SYNC(2519)
        DO IJ=INIT,LAST
           IJDUM=IJ-INITM1
           IJDUM=(IJDUM-1)/NPROC+MOD(IJDUM-1,NPROC)*IDMWHP+1
           IF (IJDUM.GE.JLOC.AND.IJDUM.LE.JHIC) THEN
              CALL DDI_GET(DMOAO,1,NGOIJ,IJDUM,IJDUM,VMOAO)
              IJX = IJ
              CALL MQTRF2(LUNOUT,LPOUT ,LUNJM1,LUNJM2,LUNJM3,
     *             INI1  ,LAS1  ,INI2  ,LAS2  ,
     *             NGO   ,NGOIJ ,NMO   ,IJLAST,NMOINT,MAXERI,
     *             ISWTCH,THRERI,
     *             LIJGO ,LIJMO ,LMOIJI,LMOIJJ,CFMO  ,
     *             VMOAO,
     *             VMOAOI,S3    ,LABEL,WORK,
     *             MOSYM ,IJX,S2)
C
           END IF
        END DO
        CALL DDI_SYNC(2520)
C
        INIT=INIT+IWIDTH
      IF(INIT.LE.IJLAST) GO TO 500
C
      END IF
C
      IZERO = 0
      WRITE(LUNJM1) IZERO,IZERO,IZERO,IZERO
      WRITE(LUNJM2) IZERO,IZERO,IZERO,IZERO
      WRITE(LUNJM3) IZERO,IZERO,IZERO,IZERO
C
      RETURN
      END
C*MODULE MCQDPT  *DECK MQPCNF
      SUBROUTINE MQPCNF(LUNCNF,MAXSOC,MAXSPF,INIACT,LASACT,NOCF  ,
     *                  SMAP  ,OMAP  ,NSNSF )
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
C**** DECLARATION ******************************************************
      IMPLICIT INTEGER (A-Z)
      INTEGER   SMAP(MAXSOC+1,MAXSPF), OMAP(INIACT:LASACT+1,NOCF)
      INTEGER   NSNSF(NOCF+1)
      PARAMETER (MXAO=2047)
      CHARACTER*4 LBL(MXAO), LBLTPS(2), LBLTPO(3)
      DATA LBLTPO/'    ',' +  ',' ++ '/
      DATA LBLTPS/'  - ','  + '/
C**** START ************************************************************
      CALL SEQREW(LUNCNF)
C**** WRITE SPIN FUNCTIONS *********************************************
      WRITE(LUNCNF,100) (I, I=1,MAXSOC)
  100 FORMAT(' *** SPIN FUNCTION ***'/
     *       '   LEVEL ---------',20I5 / ( 12X, 20I5 ) )
      DO 106 ISPF=1,MAXSPF
        DO 102 I=1,MAXSOC
          LBL(I)=LBLTPS(SMAP(I,ISPF))
  102   CONTINUE
        WRITE(LUNCNF,104) ISPF,ISPF,ISPF,(LBL(IJF),IJF=1,MAXSOC)
  104   FORMAT (1X,I5,':',I5,'-',I5,2X,20(A4,1X)/ ( 14X, 20(A4,1X) ) )
  106 CONTINUE
C**** WRITE ORBITAL CONFIGURATIONS *************************************
      WRITE(LUNCNF,108) ( I, I= INIACT, LASACT )
  108 FORMAT(' *** ORBITAL CONFIGURATION ***'/
     *       '   LEVEL ---------',20I5 / ( 12X, 20I5 ) )
      DO 112 IOCF=1,NOCF
        DO 110 I=INIACT,LASACT
          LBL(I)=LBLTPO(OMAP(I,IOCF))
  110   CONTINUE
        CSF1=NSNSF(IOCF)+1
        CSF2=NSNSF(IOCF+1)
        WRITE(LUNCNF,104) IOCF,CSF1,CSF2,(LBL(IJF),IJF=INIACT,LASACT)
  112 CONTINUE
C**** RETURN AND END ***************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQPTG1
      SUBROUTINE MQPTG1(LUNOUT,LPOUT ,LUNFTP,
     *                  NDOUB ,NORB  ,NCSF  ,NOCF  ,MAXSOC,MAXSPF,
     *                  LENGTH,NOCFI ,
     *                  IOMAP ,ISMAP ,NSNSF ,I1EX1 ,I1EX2 ,
     *                  LAB1  ,WORK  )
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
C
C     NOTE:
C
C               L1
C       <M1,MU/E  /M2,NU>
C               L2
C
C       I1, M2, MAT(L2,MU,NU)
C
C       THE VALUE OF KCONT
C
C         KCONT= 0  : THE END OF RECORDS
C         KCONT= 1  : THE END OF I1
C         KCONT=10  : THE END OF M2
C         KCONT=100 : ELSE
C
C**** DECLARATION ******************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER   IOMAP(NDOUB+1:NORB+1,NOCF),ISMAP(MAXSOC+1,MAXSPF)
      INTEGER   I1EX1(NDOUB+1:NORB,0:NOCF)
      INTEGER   I1EX2(NDOUB+1:NORB,0:NOCFI)
      INTEGER   NSNSF(NOCF+1)
      INTEGER   LAB1(3,LENGTH)
      DIMENSION WORK(LENGTH)
      DIMENSION IDELB(4)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DATA IDELB/0,1,-1,0/
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
       WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQPTG1 ***'')')
       CALL MQDEBG(LUNOUT,
     * 'LUNFTP',' NDOUB','  NORB','  NCSF','  NOCF','MAXSOC','MAXSPF',
     *  LUNFTP ,  NDOUB ,   NORB ,   NCSF ,   NOCF , MAXSOC , MAXSPF )
       CALL MQDEBG(LUNOUT,
     * 'LENGTH',' NOCFI','     -','     -','     -','     -','     -',
     *  LENGTH ,  NOCFI ,      0 ,      0 ,      0 ,      0 ,      0 )
      END IF
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LE.-100 .AND. MASWRK) THEN
        NMOACT=NORB-NDOUB
        WRITE(LUNOUT,'('' *** MATRIX I1EX1 ***'')')
        CALL MQWMAI(LUNOUT,I1EX1(1,1),NMOACT,NMOACT,NOCF,'    ')
        WRITE(LUNOUT,'('' *** MATRIX I1EX2 ***'')')
        CALL MQWMAI(LUNOUT,I1EX2(1,1),NMOACT,NMOACT,NOCFI,'    ')
      END IF
C***********************************************************************
C****       ************************************************************
C**** START ************************************************************
C****       ************************************************************
      NMODP1=NDOUB+1
      DO 128 L1=NMODP1,NORB
       DO 126 M2=1,NOCF
        NWRITE=0
C       IF(M2.NE.NOCF) THEN
         NSF2=NSNSF(M2+1)-NSNSF(M2)
C       ELSE
C        NSF2=NCSF-NSNSF(M2)
C       END IF
C***********************************************************************
C****           ********************************************************
C**** CASE M'=M ********************************************************
C****           ********************************************************
        IF(IOMAP(L1,M2).EQ.2) THEN
         DO 104 MU=1,NSF2
          NWRITE=NWRITE+1
          IF(NWRITE.GT.LENGTH) THEN
           KCONT=100
           WRITE(LUNFTP) KCONT,L1,M2,LENGTH
           CALL MQGENW(LUNFTP,3*LENGTH,LENGTH,LAB1,WORK)
           NWRITE=1
C          WRITE(LUNOUT,100) NWRITE,LENGTH
C 100      FORMAT(' *** ERROR STOP IN SUB.MQPTG1 ***'/
C    *       ' NWRITE GOT GREATER THAN LENGTH.'/
C    *       ' M1 =',I10,'  L1 =',I10,
C    *       '  L2 =',I10,'  M2 =',I10)
C          STOP
          END IF
          LAB1(1,NWRITE)=L1
          LAB1(2,NWRITE)=MU
          LAB1(3,NWRITE)=MU
          WORK(NWRITE)=1.0D+00
C.... DEBUG OUTPUT .....................................................
           IF(LPOUT.LE.-100 .AND. MASWRK) THEN
            WRITE(LUNOUT,102) M2,MU,L1,L1,M2,MU,WORK(NWRITE)
  102       FORMAT(' <',I5,',',I3,'/',I2,'+,',
     1        I2,'-/',I5,',',I3,'> =',F12.8)
           END IF
C.... END ..............................................................
  104    CONTINUE
        ELSE IF(IOMAP(L1,M2).EQ.3) THEN
         DO 106 MU=1,NSF2
          NWRITE=NWRITE+1
          IF(NWRITE.GT.LENGTH) THEN
           KCONT=100
           WRITE(LUNFTP) KCONT,L1,M2,LENGTH
           CALL MQGENW(LUNFTP,3*LENGTH,LENGTH,LAB1,WORK)
           NWRITE=1
C          WRITE(LUNOUT,100) NWRITE,LENGTH
C          STOP
          END IF
          LAB1(1,NWRITE)=L1
          LAB1(2,NWRITE)=MU
          LAB1(3,NWRITE)=MU
          WORK(NWRITE)=2.0D+00
C.... DEBUG OUTPUT .....................................................
           IF(LPOUT.LE.-100 .AND. MASWRK) THEN
            WRITE(LUNOUT,102) M2,MU,L1,L1,M2,MU,WORK(NWRITE)
           END IF
C.... END ..............................................................
  106    CONTINUE
        END IF
C***********************************************************************
C****            *******************************************************
C**** CASE M'/=M *******************************************************
C****            *******************************************************
        DO 124 L2=NMODP1,NORB
         IF(L1.EQ.L2) GO TO 124
         I1EX=I1EX2(L1,I1EX1(L2,M2))
         IF(I1EX.EQ.0) GO TO 124
         M1=I1EX
C        IF(M1.NE.NOCF) THEN
          NSF1=NSNSF(M1+1)-NSNSF(M1)
C        ELSE
C         NSF1=NCSF-NSNSF(M1)
C        END IF
         DO 122 MU=1,NSF1
          DO 120 NU=1,NSF2
           IF(M1.GT.M2) THEN
            IOCF=M1
            ISF =MU
            IOCFP=M2
            ISFP =NU
           ELSE
            IOCF=M2
            ISF =NU
            IOCFP=M1
            ISFP =MU
           END IF
C**** FIND BRANCH LEVEL ************************************************
           IB   =0
           NSOCP=MAXSOC-IOMAP(NORB+1,IOCFP)
           NSOC =MAXSOC-IOMAP(NORB+1,IOCF )
           DO 108 I=NMODP1,NORB
            LEV=I
            IOP=IOMAP(I,IOCFP)
            IF(IOP.EQ.1) THEN
             ICA1=1
            ELSE IF(IOP.EQ.2) THEN
             NSOCP=NSOCP+1
             IF(ISMAP(NSOCP,ISFP).EQ.2) THEN
              ICA1=2
             ELSE
              ICA1=3
             END IF
            ELSE
             ICA1=4
            END IF
            IO =IOMAP(I,IOCF )
            IF(IO.EQ.1) THEN
             ICA2=1
            ELSE IF(IO.EQ.2) THEN
             NSOC=NSOC+1
             IF(ISMAP(NSOC,ISF).EQ.2) THEN
              ICA2=2
             ELSE
              ICA2=3
             END IF
            ELSE
             ICA2=4
            END IF
            IF(ICA1.NE.ICA2) THEN
             GO TO 110
            END IF
            IB=IB+IDELB(ICA2)
  108      CONTINUE
C.... ERROR STOP .......................................................
C***********************************************************************
C**** IN THE LOOP ******************************************************
C****             ******************************************************
  110      IB1=IB+IDELB(ICA1)
           IB2=IB+IDELB(ICA2)
           IDIF12=IB1-IB2
C%%%%
           B=IB2
           IF((ICA1.EQ.3).AND.(ICA2.EQ.1)) THEN
            F=1.0D+00
            GO TO 112
           ELSE IF((ICA1.EQ.4).AND.(ICA2.EQ.2)) THEN
            F=SQRT((B+1.0D+00)/B)
            GO TO 112
           ELSE IF((ICA1.EQ.2).AND.(ICA2.EQ.1)) THEN
            F=1.0D+00
            GO TO 112
           ELSE IF((ICA1.EQ.4).AND.(ICA2.EQ.3)) THEN
            F=SQRT((B+1.0D+00)/(B+2.0D+00))
            GO TO 112
           ELSE
            GO TO 120
           END IF
C     =    <<EQUAL ZERO>>
  112      LEVP1=LEV+1
           DO 114 I=LEVP1,NORB
            LEV=I
            IOP=IOMAP(I,IOCFP)
            IF(IOP.EQ.1) THEN
             ICA1=1
            ELSE IF(IOP.EQ.2) THEN
             NSOCP=NSOCP+1
             IF(ISMAP(NSOCP,ISFP).EQ.2) THEN
              ICA1=2
             ELSE
              ICA1=3
             END IF
            ELSE
             ICA1=4
            END IF
            IO =IOMAP(I,IOCF )
            IF(IO.EQ.1) THEN
             ICA2=1
            ELSE IF(IO.EQ.2) THEN
             NSOC=NSOC+1
             IF(ISMAP(NSOC,ISF).EQ.2) THEN
              ICA2=2
             ELSE
              ICA2=3
             END IF
            ELSE
             ICA2=4
            END IF
            IB1=IB1+IDELB(ICA1)
            IB2=IB2+IDELB(ICA2)
            IDIF12=IB1-IB2
C%%%%
            B=IB2
C**** IF LOOP CLOSES OR NOT ********************************************
            IF(IDIF12.EQ.0) THEN
             IF((ICA1.EQ.1).AND.(ICA2.EQ.3)) THEN
              GO TO 116
             ELSE IF((ICA1.EQ.2).AND.(ICA2.EQ.4)) THEN
              F=F*SQRT(B/(B+1.0D+00))
              GO TO 116
             ELSE IF((ICA1.EQ.1).AND.(ICA2.EQ.2)) THEN
              GO TO 116
             ELSE IF((ICA1.EQ.3).AND.(ICA2.EQ.4)) THEN
              F=F*SQRT((B+2.0D+00)/(B+1.0D+00))
              GO TO 116
             END IF
            END IF
C**** MIDDLE LOOP ******************************************************
            IF(ICA1.EQ.ICA2) THEN
             IF(IDIF12.EQ.-1) THEN
              IF(ICA1.EQ.1) THEN
               GO TO 114
              ELSE IF(ICA1.EQ.2) THEN
               F=F*SQRT(B*B-1.0D+00)/B
               GO TO 114
              ELSE
               F=-F
               GO TO 114
              END IF
             ELSE IF(IDIF12.EQ.1) THEN
              IF(ICA1.EQ.1) THEN
               GO TO 114
              ELSE IF(ICA1.EQ.3) THEN
               BP2=B+2.0D+00
               F=F*SQRT(BP2*BP2-1.0D+00)/BP2
               GO TO 114
              ELSE
               F=-F
               GO TO 114
              END IF
             END IF
            END IF
            IF((IDIF12.EQ.-1).AND.(ICA1.EQ.3).AND.(ICA2.EQ.2)) THEN
             F=F/B
             GO TO 114
            ELSE IF((IDIF12.EQ.1).AND.(ICA1.EQ.2).AND.(ICA2.EQ.3)) THEN
             F=-F/(B+2.0D+00)
             GO TO 114
            END IF
C
C     EQUAL ZERO
C
            GO TO 120
  114      CONTINUE
C
C     EQUAL ZERO
C
           GO TO 120
C***********************************************************************
C**** AFTER LOOP *******************************************************
C****            *******************************************************
  116      LEVP1=LEV+1
           DO 118 I=LEVP1,NORB
            IOP=IOMAP(I,IOCFP)
            IF(IOP.EQ.1) THEN
             ICA1=1
            ELSE IF(IOP.EQ.2) THEN
             NSOCP=NSOCP+1
             IF(ISMAP(NSOCP,ISFP).EQ.2) THEN
              ICA1=2
             ELSE
              ICA1=3
             END IF
            ELSE
             ICA1=4
            END IF
            IO =IOMAP(I,IOCF )
            IF(IO.EQ.1) THEN
             ICA2=1
            ELSE IF(IO.EQ.2) THEN
             NSOC=NSOC+1
             IF(ISMAP(NSOC,ISF).EQ.2) THEN
              ICA2=2
             ELSE
              ICA2=3
             END IF
            ELSE
             ICA2=4
            END IF
            IF(ICA1.NE.ICA2) THEN
             GO TO 120
            END IF
  118      CONTINUE
C**** SET F INTO VALUE *************************************************
           NWRITE=NWRITE+1
           IF(NWRITE.GT.LENGTH) THEN
            KCONT=100
            WRITE(LUNFTP) KCONT,L1,M2,LENGTH
            CALL MQGENW(LUNFTP,3*LENGTH,LENGTH,LAB1,WORK)
            NWRITE=1
C           WRITE(LUNOUT,100) NWRITE,LENGTH
C           STOP
           END IF
           LAB1(1,NWRITE)=L2
           LAB1(2,NWRITE)=MU
           LAB1(3,NWRITE)=NU
           WORK(  NWRITE)=F
C**** DEBUG OUTPUT *****************************************************
           IF(LPOUT.LE.-100 .AND. MASWRK) THEN
            WRITE(LUNOUT,102) M1,MU,L1,L2,M2,NU,F
           END IF
C***********************************************************************
C**** CONTINUE *********************************************************
C****          *********************************************************
  120     CONTINUE
  122    CONTINUE
  124   CONTINUE
        IF(L1.EQ.NORB .AND. M2.EQ.NOCF) THEN
         KCONT=0
        ELSE
         IF(M2.LT.NOCF) THEN
          KCONT=10
         ELSE
          KCONT=1
         END IF
        END IF
        WRITE(LUNFTP) KCONT,L1,M2,NWRITE
        CALL MQGENW(LUNFTP,3*NWRITE,NWRITE,LAB1,WORK)
  126  CONTINUE
  128 CONTINUE
C***********************************************************************
C****        ***********************************************************
C**** RETURN ***********************************************************
C****        ***********************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQPUSQ
      SUBROUTINE MQPUSQ(LUNPUN,NGO,NMO,INOFMT,CMO)
C     FROM       PUSQL(V,M,N,NDIM)
C=================================================
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      DIMENSION CMO(NGO,NMO)
C
C     COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C     ----- PUNCH A RECTANGULAR MATRIX WITH ORDERING LABELS -----
C     -V- IS -N- ROWS BY -M- COLUMNS, WITH TRUE LEAD DIMENSION -NDIM-
C
      IF (MASWRK) THEN
        IF(INOFMT.EQ.0) THEN
          DO 120 J = 1,NMO
            IC = 0
            MAX = 0
  100       MIN = MAX+1
            MAX = MAX+5
              IC = IC+1
              IF (MAX .GT. NGO) MAX = NGO
              MODJ=MOD(J,100)
              WRITE (LUNPUN,9008) MODJ,IC,(CMO(I,J),I = MIN,MAX)
            IF (MAX .LT. NGO) GO TO 100
  120     CONTINUE
        ELSE
          DO 160 J = 1,NMO
            IC = 0
            MAX = 0
  140       MIN = MAX+1
            MAX = MAX+3
              IC = IC+1
              IF (MAX .GT. NGO) MAX = NGO
              MODJ=MOD(J,100)
              WRITE (LUNPUN,9008) MODJ,IC,(CMO(I,J),I = MIN,MAX)
            IF (MAX .LT. NGO) GO TO 140
  160     CONTINUE
        END IF
      ENDIF
      RETURN
C
 9008 FORMAT(I2,I3,1P,5E15.8)
      END
C*MODULE MCQDPT  *DECK MQREAD
      SUBROUTINE MQREAD(GROUP,GRPV,OUTPAR)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DOUBLE PRECISION METHOD
C
      LOGICAL CANONC,FCORE,FORS,NOCI,EKT,LINSER,OUTPAR,KST,
     *        PACK2E,DIRTRF,REFWGT,GOPARR,DSKWRK,MASWRK
      LOGICAL PARAIO,DELSCR,LMQPAR,DOORD0
C
      CHARACTER*8 GRPC
C
      PARAMETER (MXATM=500, MXRT=100, MXNORO=250, MXAO=2047)
C
      DIMENSION KSTATE(MXRT),WSTATE(MXRT)
C
      COMMON /MQ2PAR/DMQPAR(200),AVECOE(MXRT),
     *               IMQPAR(400),MAINCS(3),KST(MXRT),LMQPAR(10)
C
C**** COMMON IN GAMESS FOR SETTING DEFAULT VALUES **********************
C
      COMMON /DETWFN/ WSTDET(MXRT),SPINS(MXRT),CRIT,PRTTOL,SDET,SZDET,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACTDT,NORBDT,
     *                NADET,NBDET,KDET,KSTDET,IROOT,IPURES,MAXW1,NITDET,
     *                MAXP,NCIDET,IGPDET,KSTSYM,NFTGCI
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,IPOPLE
      COMMON /GUGWFN/ NFZC,NMCC,NDOC,NAOS,NBOS,NALP,NVAL,NEXT,NFZV,
     *                IFORS,IEXCIT,ICICI,NOIRR
      COMMON /MCINP / METHOD,CISTEP,ACURCY,ENGTOL,DAMP,MICIT,NWORD,NORB,
     *                NOROT(2,MXNORO),MOFRZ(15),NPFLG(10),
     *                NOFO,CANONC,FCORE,FORS,NOCI,EKT,LINSER
      COMMON /ORBSET/ NORBMX,NORBS,NCORBS,NLEVS,NAX,NBX,NCX,NSYM,MSYM,
     *                IDOCC,IVAL,IMCC,ISYM(MXAO),ICODE(MXAO),
     *                NLCS(MXAO),LEVPT(MXAO),LEVNR(MXAO),
     *                IOUT(MXAO),NREFS,IEXCT,NFOCI,INTACT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /TRFOPT/ CUTTRF,NWDTRF,MPTRAN,ITRFAO,NOSYMT,DIRTRF
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,MFZC,NUMVEC,ICI,NXRT,NSTAT
      COMMON /MQMEM/  MEMINP,MEMDDI
C
C         SETUP NAMELIST SIMULATION
C
      PARAMETER (NNAM=37)
      DIMENSION QNAM(NNAM),KQNAM(NNAM),EDSHFT(2)
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QNAM/
     *       8HIFORB   ,8HINORB   ,8HIROT    ,8HISELCT  ,8HISTSYM  ,
     *       8HLENGTH  ,8HLPOUT   ,8HMAXCSF  ,8HMAXERI  ,8HMAXROW  ,
     *       8HMDI     ,8HMULT    ,8HMXBASE  ,8HMXTRFR  ,8HNEL     ,
     *       8HNMOACT  ,8HNMODOC  ,8HNMOFZC  ,8HNMOFZV  ,8HNSOLUT  ,
     *       8HNSTATE  ,8HNOSYM   ,8HNSTOP   ,8HREFWGT  ,8HTHRCON  ,
     *       8HTHRENE  ,8HTHRERI  ,8HTHRGEN  ,8HTHRWGT  ,8HMAINCS  ,
     *       8HWSTATE  ,8HKSTATE  ,8HPARAIO  ,8HDOORD0  ,8HDELSCR  ,
     *       8HGENZRO  ,8HEDSHFT  /
#else
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/
     *       "IFORB   ","INORB   ","IROT    ","ISELCT  ","ISTSYM  ",
     *       "LENGTH  ","LPOUT   ","MAXCSF  ","MAXERI  ","MAXROW  ",
     *       "MDI     ","MULT    ","MXBASE  ","MXTRFR  ","NEL     ",
     *       "NMOACT  ","NMODOC  ","NMOFZC  ","NMOFZV  ","NSOLUT  ",
     *       "NSTATE  ","NOSYM   ","NSTOP   ","REFWGT  ","THRCON  ",
     *       "THRENE  ","THRERI  ","THRGEN  ","THRWGT  ","MAINCS  ",
     *       "WSTATE  ","KSTATE  ","PARAIO  ","DOORD0  ","DELSCR  ",
     *       "GENZRO  ","EDSHFT  "/
#endif
      DATA KQNAM/23*1,0,5*3,31,-3,-1,0,0,0,3,23/
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA TRUDGE/8HTRUDGE  /,TRANST/8HTRANSITN/,SACAS/8HSACAS   /
#else
      CHARACTER*8 :: TRUDGE_STR
      EQUIVALENCE (TRUDGE, TRUDGE_STR)
      CHARACTER*8 :: TRANST_STR
      EQUIVALENCE (TRANST, TRANST_STR)
      CHARACTER*8 :: SACAS_STR
      EQUIVALENCE (SACAS, SACAS_STR)
      DATA TRUDGE_STR/"TRUDGE  "/,TRANST_STR/"TRANSITN"/,
     * SACAS_STR/"SACAS   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DETWRD,DRTWRD,GENWRD/8HDET     ,8HDRT     ,8HGEN     /
#else
      CHARACTER*8 :: GENWRD_STR
      EQUIVALENCE (GENWRD, GENWRD_STR)
      CHARACTER*8 :: DETWRD_STR
      EQUIVALENCE (DETWRD, DETWRD_STR)
      CHARACTER*8 :: DRTWRD_STR
      EQUIVALENCE (DRTWRD, DRTWRD_STR)
      DATA DETWRD_STR,DRTWRD_STR,GENWRD_STR/"DET     ","DRT     ",
     * "GEN     "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA ALDET,GUGA,GENCI/8HALDET   ,8HGUGA    ,8HGENCI   /
#else
      CHARACTER*8 :: GENCI_STR
      EQUIVALENCE (GENCI, GENCI_STR)
      CHARACTER*8 :: ALDET_STR
      EQUIVALENCE (ALDET, ALDET_STR)
      CHARACTER*8 :: GUGA_STR
      EQUIVALENCE (GUGA, GUGA_STR)
      DATA ALDET_STR,GUGA_STR,GENCI_STR/"ALDET   ","GUGA    ",
     * "GENCI   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA C2H/8HC2H     /
#else
      CHARACTER*8 :: C2H_STR
      EQUIVALENCE (C2H, C2H_STR)
      DATA C2H_STR/"C2H     "/
#endif
C
C**** INSTRUCTIONS *****************************************************
C
C     NAME-LIST $MCQDPT
C
C     *** GENERAL ***
C
C     NEL    : # OF ELECTRONS
C     MULT   : SPIN MULTIPLICITY
C     NMOFZC : # OF FROZEN CORE ORBITALS (NOT CORRELATED IN PT)
C     NMODOC : # OF DOUBLY OCCUPIED ORBITALS (CORRELATED IN PT)
C     NMOACT : # OF ACTIVE ORBITALS
C    (NMOEXT : # OF EXTERNAL ORBITALS)
C     NMOFZV : # OF FROZEN VIRTUAL ORBITALS
C     NSTATE : # OF TARGET STATE(S)
C              ( = DIMENSION OF EFFECTIVE HAMILTONIAN )
C              NSTATE IS NOW BASED ON KSTATE
C     ISTSYM : STATE SYMMETRY OF TARGET STATE(S)
C     INORB  : READ INPUT ORBITALS FROM CARDS (1) OR FROM THE DIRECT
C              ACCESS FILE OF HONDO (0)
C    (INOFMT : FORMAT OF INPUT ORBITALS 5D15.8 (0) OR 3D25.15 (1))
C     LPOUT  : PRINT OPTION
C              0:NORMAL PRINT, <0:DEBUG PRINT (-1,-5,-10,-100)
C
C     *** DRT AND OTHERS ***
C
C     USUALLY ALL USERS DO NOT NEED CHANGE THE FOLLOWING PARAMETERS.
C
C     MAXROW : MAXIMUM # OF ROW IN THE DISTINCT ROW TABLE (DRT)
C     MAXCSF : MAXIMUM LENGTH FOR ONE-PARTICLE COUPLING CONSTANT USED
C              IN CAS-CI CALCULATION (MAXCSF<NCSF IS PERMISSIBLE.)
C     LENGTH : BUFFER LENGTH FOR THE COUPLING CONSTANT IO
C
C     *** INTEGRAL TRANSFORMATION ***
C
C     THRERI : THRESHOLD TO KEEP ERIS ON MO BASE
C
C     USUALLY ALL USERS DO NOT NEED CHANGE THE FOLLOWING PARAMETERS.
C
C     MXTRFR : MAXIMUM NUMBER OF READS IN THE INTEGRAL TRANSFORMATION
C              STEP
C     MAXERI : BUFFER LENGTH FOR 2-ELECTRON INTEGRAL IO
C
C     *** CAS-CI ***
C
C     NSTCI  : # OF STATES TO BE OBTAINED IN THE DAVIDSON'S METHOD
C                NSTCI IS NOW BASED ON KSTATE
C     KSTATE : STATE IS USED (1) OR NOT (0) IN MC-QDPT2 (1-D ARRAY)
C
C     EXAMPLE. IF YOU WANT THE SECOND AND THE FOURTH ROOTS, SET
C              KSTATE=0,1,0,1 .
C
C     USUALLY ALL USERS DO NOT NEED CHANGE THE FOLLOWING PARAMETERS.
C
C     NSOLUT : # OF STATES TO BE SOLVED IN THE DAVIDSON'S METHOD
C     NSTOP  : MAXIMUM # OF ITERATION IN THE DAVIDSON'S METHOD
C     MXBASE : MAXIMUM # OF BASIS VECTOR IN THE DAVIDSON'S METHOD
C     MDI    : DIMENSION OF SMALL HAMILTONIAN USED IN SETTING
C              INITIAL VECTOR(S) IN THE DAVIDSON'S METHOD
C     THRENE : THRESHOLD FOR THE ENERGY CONVERGENCE IN THE DAVIDSON'S
C              METHOD
C     THRCON : THRESHOLD FOR THE VECTOR CONVERGENCE IN THE DAVIDSON'S
C              METHOD
C     MAINCS : CSFS BELONGING TO THE SYMMETRY OF THE TARGET STATE(S)
C
C     *** CANONICAL FOCK ORBITALS ***
C
C     IFORB  : REDEFINE CANONICAL FOCK ORBITALS (1) OR NOT (0)
C              (3) SAME AS ONE, BUT AVERAGE OVER ALL MULTIPLICITIES
C              THIS SET WILL ALSO BE PUNCHED.
C
C     USUALLY ALL USERS DO NOT NEED CHANGE THE FOLLOWING PARAMETERS.
C
C     WSTATE : WEIGHT OF THE EACH STATE (1D ARRAY)
C        THIS IS MAPPED ONTO A SECOND ARRAY, -AVECOE-, WITH WEIGHTS
C        GIVEN ONLY FOR THOSE STATES INCLUDED IN THE PERTURBATION.
C
C     *** CSF SELECTION IN REFERENCE STATE(S) ***
C
C     ISELCT : SELECT CSF (1) OR NOT (0)
C     THRWGT : THRESHOLD WEIGHT (=/CI/**2) FOR CSF SELECTION
C
C     *** MC-QDPT2 ***
C
C     IROT   : USE MP2 FORMULA FOR THE (DD)->(EE) EXCITATIONS (1) OR NOT
C              (0)
C     USUALLY ALL USERS DO NOT NEED CHANGE THE FOLLOWING PARAMETERS.
C
C     THRGEN : THRESHOLD FOR ONE-, TWO-, THREE-PARTICLE COUPLING
C              CONSTANTS IN THE PERTURBATION CALCULATION
C              NOTE: IF YOU WANT, FOR EXAMPLE, TO OBTAIN ENERGIES
C                    TO 6 FIGURES AFTER POINT, SET
C                    THRGEN=1.0D-08 OR 1.0D-09.
C     GENZRO : ZERO THRESHOLD FOR ONE-, TWO-, AND THREE-PARTICLE
C              COUPLING CONSTANT CALCULATIONS
C
C     *** INTRUDER STATES ***
C     EDSHFT : ENERGY DENOMINATOR SHIFTS (DEFAULT IS ZERO SHIFT)
C     EDSHFT(1) SHIFT FOR SPIN-FREE MCQDPT 
C     EDSHFT(2) SHIFT FOR SPIN-ORBIT MCQDPT 
C
C        ---- READ INFORMATION ABOUT THE MCSCF WAVEFUNCTION ----
C
      LUNIN =IR
      LUNOUT=IW
C
      IF(CISTEP.EQ.GUGA)  CALL DRTGEN(-23,DRTWRD)
      IF(CISTEP.EQ.ALDET) CALL DETINP(-23,DETWRD)
      IF(CISTEP.EQ.GENCI) CALL GCIINP(-23,GENWRD) 
      IF(CISTEP.EQ.GENCI.AND.GLIST.NE.SACAS) THEN
         IF(MASWRK) WRITE(LUNOUT,*) 'GENERAL CI DOES NOT PERMIT MCQDPT'
         CALL ABRT
      END IF
C
C        THE DETERMINANT CODE LIKES TO REVERSE BU <--> BG FOR C2H
C        THIS MUST BE UNDONE FOR THE MCQDPT PROGRAM.
C
      IF(CISTEP.EQ.ALDET  .AND.  GRPDET.EQ.C2H) THEN
         MODI = KSTSYM
         IF(KSTSYM.EQ.2) MODI=3
         IF(KSTSYM.EQ.3) MODI=2
         KSTSYM=MODI
      END IF
C
C**** SET DEFAULT VALUES ***********************************************
C
      KQNAM(31) = 10*MXRT + 3
      KQNAM(32) = 10*MXRT + 1
C
      IFORB  = 1
      IPURFY= 0
C.... INOFMT CANNOT BE READ.
      INOFMT = 0
      INORB  = 0
      IROT   = 0
      ISELCT = 0
      LENGTH = 4096
      LPOUT  = 0
      MAXCSF = 2048
      MAXERI = 4096
      MAXROW = 200
      MDI    = 50
      MULT   = MUL
      MXBASE = MDI
      MXTRFR = 80
      NEL    = NE
      NOSYM  = 0
C
C        PROVIDE REASONABLE DEFAULTS FOR CHEMICAL CORE,
C        VALENCE FILLED, AND VALENCE ACTIVE ORBITALS.
C
      NMOFZC = NUMCOR()
      ISTSYM=0
      IF(CISTEP.EQ.ALDET) THEN
         ISTSYM = KSTSYM
         NMOACT = NACTDT
         NMODOC = NCORSV - NMOFZC
         IF(NMODOC.LT.0) THEN
            NMOFZC=NCORSV
            NMODOC=0
         END IF
      END IF
      IF(CISTEP.EQ.GUGA) THEN
         ISTSYM = MSYM
         NMOACT = NDOC+NAOS+NBOS+NALP+NVAL
         NMODOC = NMCC-NMOFZC
         IF(NMODOC .LT. 0)THEN
            NMOFZC=NMCC
            NMODOC=0
         END IF
      END IF
C
C.... NMOEXT CANNOT BE READ.
      NMOEXT = 0
      NMOFZV = 0
      NSOLUT = 1
      NSTATE = -1
      NSTCI  = -1
      NSTOP  = 100
C
      NGO    = NUM
C     NINTMX = NINTMX ; BECAUSE OF THE SAME VARIABLE NAME!
C
      GENZRO =  1.0D-12
      THRCON =  1.0D-06
      THRENE = -1.0D+00
      THRERI =  1.0D-12
      THRGEN =  1.0D-08
      THRWGT =  1.0D-06
      EDSHFT(1) =  0.0D+00
      EDSHFT(2) =  0.0D+00
      REFWGT =  .FALSE.
C
      DO I=1,3
        MAINCS(I) = 0
      END DO
      DO I=1,MXRT
        WSTATE(I) = 1.0D+00
      END DO
      DO I=1,MXRT
        KSTATE(I) = -1
      END DO
C
      PARAIO=.TRUE.
      DOORD0=.TRUE.
      DELSCR=.FALSE.
C
C**** READ NAMELIST ****************************************************
      CALL NAMEIO(LUNIN,JRET,GROUP,NNAM,QNAM,KQNAM,
     *            IFORB ,INORB ,IROT  ,ISELCT,ISTSYM,LENGTH,LPOUT ,
     *            MAXCSF,MAXERI,MAXROW,MDI   ,MULT  ,MXBASE,MXTRFR,
     *            NEL   ,NMOACT,NMODOC,NMOFZC,NMOFZV,NSOLUT,NSTATE,
     *            NOSYM ,NSTOP ,REFWGT,THRCON,THRENE,THRERI,THRGEN,
     *            THRWGT,MAINCS,WSTATE,KSTATE,PARAIO,DOORD0,DELSCR,
     *            GENZRO,EDSHFT,
     *            0,0,       0,0,0,0,0,
     *            0,0,0,0,0,   0,0,0,0,0,   0,0,0,0,0,   0,0,0,0,0)
C
      IF(JRET.EQ.1) THEN
         IF(RUNTYP.EQ.TRANST) THEN
            WRITE(IW,*) 'PLEASE DEFINE ALL $MCQD GROUPS AND RESUBMIT.'
            CALL ABRT
         ENDIF
         WRITE(IW,9010) GROUP
      ELSE IF(JRET .EQ. 2) THEN
         CALL ABRT
      END IF
C
C       JUST USING NSTCI TO GET 1,1,1,1,1, ETC.
C       THIS IS THE ORIGINAL METHOD OF INPUT
      IF(KSTATE(1).EQ.-1) THEN
        DO I=1,MXRT
          KSTATE(I)=1
        END DO
        IF(NSTCI.EQ.-1)  NSTCI=1
        IF(NSTATE.EQ.-1) NSTATE=1
      ELSE
C       USING KSTATE TO DEFINE OTHER PARAMETERS
C       THIS IS THE BEST WAY TO INPUT STATES
C
        NSTATE=0
        DO I=1,MXRT
          IF(KSTATE(I) .NE. -1) NSTCI=I
        END DO
        DO I=1,NSTCI
          IF(KSTATE(I) .NE.  0) NSTATE=NSTATE+1
        END DO
      END IF
C
      IF(INORB .EQ. 1 .AND. RUNTYP .EQ. TRUDGE) THEN
        IF(MASWRK) WRITE(IW,*) 'SETTING INORB=0 FOR RUNTYP=TRUDGE'
        INORB=0
      END IF
C
      IF(RUNTYP.EQ.TRANST.AND.NSTAT.GT.NSTATE) NSTATE=NSTAT
      IF(NSTCI .LT.NSTATE) NSTCI =NSTATE
      IF(NSOLUT.LT.NSTCI ) NSOLUT=NSTCI
      IF(NOSYM.LT.0) THEN
         NOSYM=0
         IPURFY=1
      ENDIF
C%%%%
      IF(NMOEXT.LE.0) NMOEXT=NUM-(NMOFZC+NMODOC+NMOACT+NMOFZV)
C**** WRITE PARAMETERS  ************************************************
      IF(OUTPAR) THEN
      WRITE(LUNOUT,*)
      WRITE(LUNOUT,'(''    ----------------------------'')')
      WRITE(LUNOUT,'(''    MRMP/MCQDPT INPUT PARAMETERS'')')
      WRITE(LUNOUT,'(''    ----------------------------'')')
      WRITE(LUNOUT,'('' # OF ELECTRONS                ='',I10)') NEL
      WRITE(LUNOUT,'('' SPIN MULTIPLICITY             ='',I10)') MULT
      WRITE(LUNOUT,'('' SPATIAL STATE SYMMETRY        ='',I10)') ISTSYM
      WRITE(LUNOUT,9110) NSTATE
 9110 FORMAT(1X,'# OF STATES                   =',I10,
     *          ' (DIMENSION OF EFFECTIVE HAMILTONIAN)')
      WRITE(LUNOUT,'('' # OF FROZEN CORE ORBITALS     ='',I10)') NMOFZC
      WRITE(LUNOUT,'('' # OF DOUBLY OCCUPIED ORBITALS ='',I10)') NMODOC
      WRITE(LUNOUT,'('' # OF ACTIVE ORBITALS          ='',I10)') NMOACT
      WRITE(LUNOUT,'('' # OF EXTERNAL ORBITALS        ='',I10)') NMOEXT
      WRITE(LUNOUT,'('' # OF FROZEN VIRTUAL ORBITALS  ='',I10)') NMOFZV
      WRITE(LUNOUT,102)
     *  '     IFORB     INORB      IROT    ISELCT    LENGTH    MAXCSF',
     *  '    MAXERI',
     *        IFORB,    INORB,     IROT,   ISELCT,   LENGTH,   MAXCSF,
     *       MAXERI,
     *  '    MAXROW       MDI    MXBASE    MXTRFR    NSOLUT     NSTCI',
     *  '     NSTOP',
     *       MAXROW,      MDI,   MXBASE,   MXTRFR,   NSOLUT,    NSTCI,
     *        NSTOP
  102 FORMAT(/1X,A60,A10/1X,7I10/1X,A60,A10/1X,7I10)
      WRITE(LUNOUT,104)
     *  '    GENZRO    THRCON    THRENE    THRERI    THRGEN    THRWGT',
     *  '    REFWGT',
     *       GENZRO,   THRCON,   THRENE,   THRERI,   THRGEN,   THRWGT ,
     *       REFWGT
  104 FORMAT(1X,A60,A10/1X,1P,6D10.3,L8)
      WRITE(LUNOUT,106)
     *  '     NOSYM    PARAIO    DOORD0    DELSCR',
     *       NOSYM,    PARAIO,   DOORD0,   DELSCR
  106 FORMAT(1X,A40/1X,I10,3L10)
      IF(ABS(EDSHFT(1)).GT.1.0D-06.OR.ABS(EDSHFT(2)).GT.1.0D-06)
     *   WRITE(LUNOUT,110) EDSHFT
  110 FORMAT(/1X,'INTRUDER STATE FREE TECHNIQUE WILL BE USED,'/
     *        1X,'WITH PARAMETERS: ',F10.6,' (SPIN-FREE) AND',F10.6,
     *           ' (SPIN-ORBIT) (A.U.)'/)
      ENDIF
C
      NERR=0
C
      IF(INORB.EQ.1) THEN
        CALL SEQREW(IR)
        WRITE(UNIT=GRPC,FMT='(A8)') GRPV
        CALL FNDGRP(IR,GRPC,IEOF)
        IF(IEOF.EQ.1) THEN
           IF(MASWRK) WRITE(IW,9000) GRPV
           NERR=NERR+1
        END IF
      END IF
C
      IF(ISTSYM.EQ.0) THEN
         IF(MASWRK) WRITE(IW,9020) 
         NERR=NERR+1
      END IF
C
      IF(NEL.EQ.2*NMOFZC+2*NMODOC) THEN
         IF(MASWRK) WRITE(IW,*) 'ERROR, MCQDPT HAS NO ACTIVE ELECTRONS.'
         NERR=NERR+1
      END IF
C
      IF(NEL.NE.NE) THEN
         IF(MASWRK) WRITE(IW,*)
     *        'ERROR, MCQDPT MOLECULAR CHARGE IS INCONSISTENT'
         NERR=NERR+1
      END IF
C
      IF(MULT.NE.MUL.AND.RUNTYP.NE.TRANST) THEN
         IF(MASWRK) WRITE(IW,*)
     *        'ERROR, $MCQDPT -MULT- DOES NOT MATCH $CONTRL'
         NERR=NERR+1
      END IF
C
      IF(IFORB.GT.2..AND.RUNTYP.EQ.TRANST.AND.NUMVEC.NE.-NUMCI) THEN
         NUMVEC=-NUMCI
         IF(MASWRK) WRITE(IW,*)
     *        'WARNING: FOR IFORB>2 NUMVEC IS RESET TO ',NUMVEC
      ENDIF
C
C           SELECTION OF DIRTRF IN $TRANS IS A NO-NO, PRINT WARNING
C
      IF(DIRTRF  .AND.  MASWRK) WRITE(IW,9050)
C
C         DDI MEMORY AVAILABLE - READ $SYSTEM
C            ( SYSINP BREAKS ITRFAO VALUE )
C
      ITRFA2=ITRFAO
      CALL SYSINP( MEMINP, MEMDDI )
      ITRFAO=ITRFA2
C
C         -WSTATE- IS FOR USERS, -AVECOE- FOR INTERNAL USE, AS IT
C         HOLDS ONLY VALUES FOR STATES IN THE PERTURBATION TREATMENT.
C         -AVECOE- WILL GET NORMALIZED AGAIN IN -MQFORB- ROUTINE.
C
      WSUM = 0.0D+00
      IST=0
      DO I=1,NSTCI
         IF(KSTATE(I).LE.0) THEN
            WSTATE(I) = 0.0D+00
         ELSE
            IST = IST+1
            AVECOE(IST) = WSTATE(I)
         END IF
         WSUM = WSUM + WSTATE(I)
      ENDDO
      IF(WSUM.LE.0.0D+00) THEN
         IF(MASWRK) WRITE(IW,*) '$MCQDPT WSTATE CONTAINS NO WEIGHTS'
         NERR=NERR+1
      END IF
      IF(RUNTYP.NE.TRANST) THEN
         CALL DSCAL(MXRT,1.0D+00/WSUM,WSTATE,1)
         CALL DSCAL(MXRT,1.0D+00/WSUM,AVECOE,1)
      END IF
C
      IF(OUTPAR) THEN
         WRITE(LUNOUT,9030)
         WRITE(LUNOUT,9040) (III,WSTATE(III),III=1,NSTCI)
      END IF
C
      IF(REFWGT  .AND.  GOPARR) THEN
         IF(MASWRK) WRITE(IW,*)
     *         'REFWGT=.TRUE. IS NOT PROGRAMMED FOR PARALLEL EXECUTION.'
         NERR=NERR+1
      END IF
C
      IF(NERR.GT.0) CALL ABRT
C
C**** STORE PARAMETERS IN IPARM AND DPARM ******************************
C
      IMQPAR( 1)=IFORB
      IMQPAR( 2)=INOFMT
      IMQPAR( 3)=INORB
      IMQPAR( 4)=IROT
      IMQPAR( 5)=ISELCT
      IMQPAR( 6)=ISTSYM
      IMQPAR( 7)=LENGTH
      IMQPAR( 8)=LPOUT
      IMQPAR( 9)=MAXCSF
      IMQPAR(10)=MAXERI
      IMQPAR(11)=MAXROW
      IMQPAR(12)=MDI
      IMQPAR(13)=MULT
      IMQPAR(14)=MXBASE
      IMQPAR(15)=MXTRFR
      IMQPAR(16)=NEL
      IMQPAR(17)=NMOACT
      IMQPAR(18)=NMODOC
      IMQPAR(19)=NMOEXT
      IMQPAR(20)=NMOFZC
      IMQPAR(21)=NMOFZV
      IMQPAR(22)=NSOLUT
      IMQPAR(23)=NSTATE
      IMQPAR(24)=NSTCI
      IMQPAR(25)=NSTOP
      IMQPAR(26)=NOSYM
      IMQPAR(27)=IPURFY
C
      IMQPAR(101)=NINTMX
      IMQPAR(102)=NGO
C
      DMQPAR( 1)=GENZRO
      DMQPAR( 2)=THRCON
      DMQPAR( 3)=THRENE
      DMQPAR( 4)=THRERI
      DMQPAR( 5)=THRGEN
      DMQPAR( 6)=THRWGT
      DMQPAR( 7)=EDSHFT(1)
      DMQPAR( 8)=EDSHFT(2)
C
      LMQPAR( 1)=REFWGT
      LMQPAR( 2)=PARAIO
      LMQPAR( 3)=DOORD0
      LMQPAR( 4)=DELSCR
C
      DO I=1,MXRT
        IF(KSTATE(I).NE.0) THEN
          KST(I)=.TRUE.
        ELSE
          KST(I)=.FALSE.
        END IF
      END DO
      RETURN
C
 9000 FORMAT (/1X,'YOU HAVE INORB=1, BUT NO ',A8,'!!',/)
 9010 FORMAT(1X,'WARNING! DEFAULT VALUES ARE USED FOR ',A8,' INPUT.'/
     *       1X,'YOU MIGHT THINK ABOUT KSTATE, WSTATE, AND ISTSYM.')
 9020 FORMAT(1X,'ERROR: SPATIAL STATE SYMMETRY WAS NOT INPUT,'/
     *       1X,'AND WAS NOT INHERITED FROM $DET OR $DRT.'/
     *       1X,'PLEASE PROVIDE -ISTSYM- IN YOUR $MCQDPT INPUT.')
 9030 FORMAT(/1X,'THE WEIGHTS OF THE CAS STATES IN THE DENSITY MATRIX',
     *           ' AVERAGING ARE')
 9040 FORMAT(5(1X,I3,'=',F10.6))
 9050 FORMAT(/1X,'NOTE: MCQDPT RUNS CANNOT BE AO INTEGRAL DIRECT.'/
     *       1X,'AN AO INTEGRAL FILE WILL STILL BE GENERATED,'/
     *       1X,'BEFORE THE MCQDPT CALCULATION IS STARTED.')
      END
C*MODULE MCQDPT  *DECK MQREF
      SUBROUTINE MQREF(LUNOUT,LPOUT,MULT,NORB,MAXROW,
     *                 NDOUB,NREFTM,NREF,
     *                 CASE,BROW,YTOT,Y,L,MAP,LBL,CTYPE )
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
C**** DECLARATION ******************************************************
      IMPLICIT INTEGER (A-Z)
      DIMENSION CASE(NORB) ,BROW(NORB+1),YTOT(MAXROW)
      DIMENSION Y(3,MAXROW),L(3,MAXROW)
      INTEGER   MAP(NDOUB+1:NORB+1,NREFTM)
      CHARACTER*4 LBL(NORB), CTYPE
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQREF ***'')')
        CALL MQDEBG(LUNOUT,
     *  '  MULT','  NORB','MAXROW',' NDOUB','NREFTM','     -','     -',
     *     MULT ,   NORB , MAXROW ,  NDOUB , NREFTM ,      0 ,      0 )
      END IF
C**** START ************************************************************
      INIACT=NDOUB+1
      LASACT=NORB
      YTOT(1)=0
      BROW(1)=1
      LEV=0
C      CASE(1)=0
      DO 150 I=1,NORB
        CASE(I)=0
  150 CONTINUE
C
   15 LEVP1=LEV+1
      ICASE=CASE(LEVP1)+1
      IF(ICASE.GT.3) GO TO 100
      CASE(LEVP1)=ICASE
      BROWT=BROW(LEVP1)
      LB=L(ICASE,BROWT)
      IF(LB.EQ.0) GO TO 15
      LEVP2=LEVP1+1
      BROW(LEVP2)=LB
      YTOT(LEVP2)=YTOT(LEVP1)+Y(ICASE,BROWT)
      IF(LEVP1.EQ.NORB) GO TO 200
      LEV=LEV+1
      GO TO 15
C
  100 IF(LEV.EQ.0) GO TO 400
      CASE(LEVP1)=0
      LEV=LEV-1
      GO TO 15
C**** SAVE CASE NUMBERS IN MAP(*,*) ************************************
  200 ICONF=YTOT(LEV+1)+1
      DO 300 I=INIACT,LASACT
        MAP(I,ICONF)=CASE(I)
  300 CONTINUE
      GO TO 15
C**** ARRANGE MAP ******************************************************
  400 CONTINUE
      IF(CTYPE.NE.'SPIN') THEN
        NREF=0
        DO 504 ICONF=1,NREFTM
          NSOC=0
          DO 500 I=INIACT,LASACT
            IF(MAP(I,ICONF).EQ.2) NSOC=NSOC+1
  500     CONTINUE
          LASAP1=LASACT+1
          MAP(LASAP1,ICONF)=NSOC
          MULTM1=MULT-1
          IF(NSOC.GE.MULTM1) THEN
            NREF=NREF+1
            IF(NREF.NE.ICONF) THEN
              DO 502 I=INIACT,LASAP1
                MAP(I,NREF)=MAP(I,ICONF)
  502         CONTINUE
            END IF
          END IF
  504   CONTINUE
      ELSE
        NREF=NREFTM
        NREFM1=NREF-1
        DO 512 ISP=1,NREFM1
          ISPP1=ISP+1
          DO 510 JSP=ISPP1,NREF
            DO 508 I=1,NORB
              IF(MAP(I,ISP).NE.MAP(I,JSP)) THEN
                IF(MAP(I,ISP).LT.MAP(I,JSP)) THEN
                  GO TO 510
                ELSE
                  DO 506 J=1,NORB
                    K         =MAP(J,ISP)
                    MAP(J,ISP)=MAP(J,JSP)
                    MAP(J,JSP)=K
  506             CONTINUE
                  GO TO 510
                END IF
              END IF
  508       CONTINUE
C           STOP ' ERROR IN SUB.MQREF'
  510     CONTINUE
  512   CONTINUE
      END IF
C**** WRITE REFERENCE CONFIGURATIONS ***********************************
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
C 400   WRITE(LUNOUT,1500) NREF
C1500   FORMAT(' TOTAL REFERENCE CONFIGURATIONS: ',I10)
        IF(CTYPE.NE.'SPIN') THEN
          CALL MQREFP(LUNOUT,NORB,NDOUB,NREF,MAP,LBL)
        ELSE
          CALL MQREFS(LUNOUT,NORB,NREF,MAP,LBL)
        END IF
      END IF
C**** RETURN AND END ***************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQREFP
      SUBROUTINE MQREFP(LUNOUT,NORB,NDOUB,NREF,
     1                  MAP   ,LBL )
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
C**** DECLARATION ******************************************************
      IMPLICIT INTEGER (A-Z)
      INTEGER   MAP(NDOUB+1:NORB+1,NREF)
      CHARACTER*4 LBL(NORB), LBLTP(3)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DATA LBLTP/'    ',' +  ',' ++ '/
      IF (MASWRK) THEN
C**** START ************************************************************
      WRITE(LUNOUT,1500) NREF
 1500 FORMAT(' *** REFERENCE ORBITAL CONFIGURATION ***'/
     1       ' TOTAL REFERENCE ORBITAL CONFIGURATIONS: ',I10)
C**** DOUBLY OCCPIED ORBITALS ******************************************
      IF(NDOUB.NE.0) THEN
        WRITE(LUNOUT,1600)
 1600   FORMAT(' ORBITALS DOUBLY OCCUPIED IN ALL REFERENCES')
        WRITE(LUNOUT,1700) ( I, I= 1, NDOUB )
 1700   FORMAT( '   LEVEL    ',20I5 / ( 12X, 20I5 ) )
      END IF
C**** PASS IF NDOUB = NORB *********************************************
      IF ( NDOUB .EQ. NORB ) GO TO 140
      NDP1 = NDOUB + 1
C**** WRITE REFERENCE CONFIGURATIONS ***********************************
      WRITE(LUNOUT,1900)
 1900 FORMAT (  ' ORBITALS PARTIALLY OCCUPIED IN ANY REFERENCE' )
      WRITE(LUNOUT,2000) ( I, I= NDP1, NORB )
 2000 FORMAT( '   LEVEL    ',20I5 / ( 12X, 20I5 ) )
C
      WRITE(LUNOUT,2200)
 2200 FORMAT (   ' CONF', '   WALK'
     +         / '   # ', '     # ' )
C
  140 NDP1=NDOUB+1
      DO 200 ICONF=NREF,1,-1
        DO 150 I=NDP1,NORB
          LBL(I)=LBLTP(MAP(I,ICONF))
  150   CONTINUE
C
        IF ( NDOUB .NE. NORB .AND. MASWRK) THEN
          WRITE(LUNOUT,2300) ICONF,ICONF,(LBL(IJF),IJF=NDP1,NORB)
 2300     FORMAT ( I5,I7,2X,20(A4,1X)/ ( 14X, 20(A4,1X) ) )
        END IF
C
 200  CONTINUE
C**** RETURN AND END ***************************************************
      END IF
      RETURN
      END
C*MODULE MCQDPT  *DECK MQREFS
      SUBROUTINE MQREFS(LUNOUT,NORB,NREF,MAP,LBL )
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
C**** DECLARATION ******************************************************
      IMPLICIT INTEGER (A-Z)
      INTEGER   MAP(NORB+1,NREF)
      CHARACTER*4 LBL(NORB), LBLTP(2)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DATA LBLTP/'  - ','  + '/
      IF (MASWRK) THEN
C**** START ************************************************************
      WRITE(LUNOUT,1500) NREF
 1500 FORMAT(' *** SPIN FUNCTIONS ***'/
     1       ' TOTAL SPIN FUNCTIONS: ',I10)
C**** WRITE SPIN FUNCTIONS *********************************************
      WRITE(LUNOUT,2000) (I, I=1,NORB)
 2000 FORMAT( '   LEVEL    ',20I5 / ( 12X, 20I5 ) )
      WRITE(LUNOUT,2200)
 2200 FORMAT (   '  SPF', '   WALK'
     +         / '   # ', '     # ' )
      DO 200 ICONF=NREF,1,-1
        DO 150 I=1,NORB
          LBL(I)=LBLTP(MAP(I,ICONF))
  150   CONTINUE
        WRITE(LUNOUT,2300) ICONF,ICONF,(LBL(IJF),IJF=1,NORB)
 2300   FORMAT ( I5,I7,2X,20(A4,1X)/ ( 14X, 20(A4,1X) ) )
  200 CONTINUE
C**** RETURN AND END ***************************************************
      END IF
      RETURN
      END
C*MODULE MCQDPT  *DECK MQREMO
      SUBROUTINE MQREMO(LUNOUT,LPOUT,LUNIN,VECWRD,NGO,NMO,INOFMT,
     *                  CMO,SINTEG,IPURFY,NQMT,Q,SS,LABMO,WRK)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION CMO(NGO,NMO),SINTEG(NGO,NGO),Q(NGO,*),SS(*),LABMO(NGO),
     *          WRK(NGO)
C
      CHARACTER*8 VECWRD, VEC1, VEC2, CHAR8
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA TRNSTN,CHECK/8HTRANSITN,8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      CHARACTER*8 :: TRNSTN_STR
      EQUIVALENCE (TRNSTN, TRNSTN_STR)
      DATA TRNSTN_STR,CHECK_STR/"TRANSITN","CHECK   "/
#endif
C
      VEC1=' $VEC   '
      VEC2=' &VEC   '
      THRERR=1.0D-05
C
C**** DEBUG OUTPUT *****************************************************
C
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQREMO ***'')')
        CALL MQDEBG(LUNOUT,
     *  ' LUNIN','   NGO','   NMO','INOFMT','IPURFY','     -','     -',
     *    LUNIN ,    NGO ,    NMO , INOFMT , IPURFY ,      0 ,      0 )
      END IF
C
C**** READ MO **********************************************************
C        ANY $VEC SHOULD HAVE BEEN PROCESSED BY GUESS=MOREAD,
C        AND ANY MISSING VIRTUALS GENERATED BY ORTHOGONALITY.
C        SO GAMESS WILL READ FROM DAF, AND THEN SKIP THE CODE
C        THAT READS CARD INPUT.  CARD INPUT IS USED FOR SPIN-ORBIT
C        RUNS, ONLY
C
      IF(VECWRD.EQ.VEC1  .AND.  RUNTYP.NE.TRNSTN) THEN
         CALL DAREAD(IDAF,IODA,CMO,NGO*NMO,15,0)
         IF(LUNIN.GT.0) RETURN
      END IF
C
C        FIRST LOCATE $VEC GROUP
C
      IF (MASWRK) THEN
C
      CALL SEQREW(LUNIN)
  100 CONTINUE
      READ(LUNIN,'(A8)',END=128,ERR=128) CHAR8
      IF(CHAR8.NE.VECWRD .AND. CHAR8.NE.VEC2) GO TO 100
      IF(INOFMT.EQ.0) THEN
        DO J=1,NMO
          IMAX=0
          IC=0
  104     CONTINUE
            IMIN=IMAX+1
            IMAX=IMAX+5
            IC=IC+1
            IF(IMAX.GT.NGO) IMAX=NGO
            READ(LUNIN,'(5X,5E15.8)',ERR=160,END=160)
     *             (CMO(I,J),I=IMIN,IMAX)
          IF(IMAX.LT.NGO) GO TO 104
        END DO
      ELSE
        DO J=1,NMO
          IMAX=0
          IC=0
  204     CONTINUE
            IMIN=IMAX+1
            IMAX=IMAX+3
            IC=IC+1
            IF(IMAX.GT.NGO) IMAX=NGO
            READ(LUNIN,'(5X,3E25.18)',ERR=160,END=160)
     *           (CMO(I,J),I=IMIN,IMAX)
          IF(IMAX.LT.NGO) GO TO 204
        END DO
      END IF
C
      IF(EXETYP.NE.CHECK) THEN
C
C        PURIFY THE ORBITALS.
C        EVEN THOUGH MCQDPT FAILED TO USE CONSISTENT LABELS IN C2H,
C        THE ORBITALS ARE PURIFIED IN THE "REST OF GAMESS" AND THEN
C        RELABELLED IN MQMOSY, SO THERE IS NO PROBLEM.
C
      IF(IPURFY.NE.0) THEN
C---     WRITE(6,*) 'ORBITAL PURIFICATION STARTED'
C---     CALL PRSQ(CMO,NMO,NGO,NGO)
         CALL SYMMOS(LABMO,Q,SS,CMO,WRK,NQMT,NGO,NMO,NGO)
         CALL ORBPUR(SS,CMO,NGO,NMO,.TRUE.)
C---     WRITE(6,*) 'ORBITAL PURIFICATION FINISHED'
C---     CALL PRSQ(CMO,NMO,NGO,NGO)
      ENDIF
C
C**** CHECK ORTHONORMALITY *********************************************
C
      IFLAG=0
      IF (MASWRK) WRITE(LUNOUT,'('' *** ORTHONORMALITY CHECK ***''/
     *  '' ... START'')')
      DO I=1,NMO
        S=0.0D+00
        DO K=1,NGO
          DO L=1,NGO
            S=S+CMO(K,I)*SINTEG(K,L)*CMO(L,I)
          END DO
        END DO
        DIFF=ABS(1.0D+00-S)
        IF(DIFF.GE.THRERR) THEN
          IFLAG=1
          IF (MASWRK)
     *     WRITE(LUNOUT,'(I4,'' IS NOT NORMALIZED.  DIFF = '',D10.3)')
     *      I,DIFF
        END IF
      END DO
      DO I=1,NMO
        DO J=1,I-1
          S=0.0D+00
          DO K=1,NGO
            DO L=1,NGO
              S=S+CMO(K,I)*SINTEG(K,L)*CMO(L,J)
            END DO
          END DO
          DIFF=ABS(S)
          IF(DIFF.GE.THRERR) THEN
            IFLAG=1
            IF (MASWRK) WRITE(LUNOUT,'(I4,'' AND'',I4,
     *        '' IS NOT ORTHOGONAL.  DIFF = '',D10.3)') J,I,DIFF
          END IF
        END DO
      END DO
      IF (MASWRK) WRITE(LUNOUT,'('' ... END'')')
C
      IF(IFLAG.EQ.1) THEN
         IF(IPURFY.NE.0) THEN
           IF (MASWRK) WRITE(LUNOUT,9020)
         ELSE
           IF (MASWRK) WRITE(LUNOUT,9010)
           CALL ABRT
         ENDIF
      END IF
C
C END OF CHECK
      END IF
C END OF MASWRK
      END IF
C
      IF (GOPARR) THEN
         CALL DDI_BCAST(2540,'F',CMO,NGO*NMO,MASTER)
      END IF
C
      RETURN
C
  128 CONTINUE
      IF (MASWRK) WRITE(LUNOUT,103) VECWRD
      CALL ABRT
      STOP
C
  160 CONTINUE
      IF (MASWRK) WRITE(LUNOUT,9030) VECWRD,J,NMO
      CALL ABRT
      STOP
C
  103 FORMAT(1X,'*** ERROR STOP IN SUB.MQREMO ***'/
     *       1X,'INPUT VECTORS IN GROUP ',A8,' WERE NOT FOUND.')
 9010 FORMAT(1X,'*** ERROR *** PROBLEM WITH MO ORTHOGONALITY'/
     *       1X,'THE REMEDY FOR THIS IS TO REOPTIMIZE THE MCSCF',
     *          ' ORBITALS, BY'/
     *       8X,'$GUESS GUESS=MOREAD NORB=XX'/
     *       8X,'$MCQDPT INORB=0'/
     *       1X,'WHICH WILL ENFORCE ORBITAL ORTHOGONALITY.')
 9020 FORMAT(1X,'THE ORBITALS WILL BE ORTHONORMALISED WITHIN EACH ',
     *          'SYMMETRY AND WITHIN EACH GROUP LATER.',/)
 9030 FORMAT(1X,'*** ERROR *** READING ',A8,'GROUP'/
     *       1X,'ERROR OCCURRED READING MO',I6/
     *       1X,'EXPECTING TO READ',I6,' ORBITALS TOTAL.')
      END
C*MODULE MCQDPT  *DECK MQSLT2
      SUBROUTINE MQSLT2(LUNOUT,LPOUT ,NCSF  ,NSTATE,THRWGT,
     *                  CASVEC,KREFOD,KREF  )
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION CASVEC(NCSF,NSTATE)
      LOGICAL   KREFOD(NCSF), KREF(NCSF)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQSLT2 ***'')')
        CALL MQDEBG(LUNOUT,
     *  '  NCSF','NSTATE','     -','     -','     -','     -','     -',
     *     NCSF , NSTATE ,      0 ,      0 ,      0 ,      0 ,      0 )
      END IF
C**** CALCULATE THRCOE *************************************************
C
      THRCOE=SQRT(THRWGT)
      IF (MASWRK) WRITE(LUNOUT,9100) THRCOE,THRWGT
C
C     SELECT CSF-S
C
      DO ICSF=1,NCSF
        KREF(ICSF)=.FALSE.
      END DO
      DO ISTATE=1,NSTATE
        DO ICSF=1,NCSF
          S=ABS(CASVEC(ICSF,ISTATE))
          IF(S.GT.THRCOE) KREF(ICSF)=.TRUE.
        END DO
      END DO
      DO ICSF=1,NCSF
        KREF(ICSF)=KREF(ICSF) .AND. KREFOD(ICSF)
      END DO
      N1=0
      N2=0
      DO ICSF=1,NCSF
        IF(KREFOD(ICSF)) N1=N1+1
        IF(KREF  (ICSF)) N2=N2+1
      END DO
      IF(MASWRK) WRITE(LUNOUT,9110) N2,N1
      RETURN
C
 9100 FORMAT(/1X,'SELECTING THE IMPORTANT CSFS FOR INCLUSION IN THE',
     *           ' PERTURBATION CALCULATION.'/
     *        1X,'CSF SELECTION THRESHOLD =',F15.8,' (BY CI COEF),',
     *           F11.8,' (BY WEIGHT)')
 9110 FORMAT(1X,I13,' CSFS SELECTED, OUT OF A TOTAL OF',I13,' CSFS.'/
     *       1X,'NOW, THE CAS-CI MUST BE REPEATED FOR THE SELECTED CSF',
     *          ' LIST.')
      END
C*MODULE MCQDPT  *DECK MQSORD
      SUBROUTINE MQSORD(LUNOUT,LPOUT,
     *                  NGO,NMO,NMOFZC,NMODOC,NMOACT,NMOEXT,IPURFY,
     *                  CFMONW,CFMOOD,MOSYMN,MOSYMO,S,WORK)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION CFMONW(NGO,NMO),CFMOOD(NGO,NMO)
      DIMENSION MOSYMN(NMO),MOSYMO(NMO),WORK(NGO),S(*)
      COMMON/MQSYLB/ISYLAB(2,8,4)
      DIMENSION ISYPRT(2,8,4)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQSORD ***'')')
        CALL MQDEBG(LUNOUT,
     *  '   NGO','   NMO','NMOFZC','NMODOC','NMOACT','NMOEXT','     -',
     *      NGO ,    NMO , NMOFZC , NMODOC , NMOACT , NMOEXT ,      0 )
      END IF
C**** SET PARAMETERS ***************************************************
      INIFZC=1
      LASFZC=NMOFZC
      INIDOC=LASFZC+1
      LASDOC=LASFZC+NMODOC
      INIACT=LASDOC+1
      LASACT=LASDOC+NMOACT
      INIEXT=LASACT+1
      LASEXT=LASACT+NMOEXT
C**** ORDER FROZEN CORE ORBITALS ***************************************
      N=0
C
      DO IS=1,8
        N0=N+1
        ISYLAB(1,IS,1)=N+1
        DO IMO=INIFZC,LASFZC
          IF(MOSYMO(IMO).EQ.IS) THEN
            N=N+1
            MOSYMN(N)=IS
            DO I=1,NGO
              CFMONW(I,N)=CFMOOD(I,IMO)
            END DO
          END IF
        END DO
        ISYLAB(2,IS,1)=N
        M=N-N0+1
        IF(M.GT.0.AND.IPURFY.NE.0) THEN
C         WRITE(6,*) 'ORTHOGONALISING FZC'
C         CALL ORTHO(Q,S,CFMONW(1,N0),WORK,M,M,NGO,L2,NGO)
C         CALL TFSQB(VEC,Q,SCR,L1,L1,L1)
C         WRITE(6,*) 'FZC, BEFORE ORTHO',N
C         CALL PRSQ(CFMONW(1,N0),M,NGO,NGO)
          CALL SCHMIDT(CFMONW(1,N0),S,WORK,NGO,M,NGO)
C         WRITE(6,*) 'FZC, AFTER ORTHO',N
C         CALL PRSQ(CFMONW(1,N0),M,NGO,NGO)
        ENDIF
      END DO
C**** ORDER DOUBLY OCCUPIED ORBITALS ***********************************
      DO IS=1,8
        N0=N+1
        ISYLAB(1,IS,2)=N+1
        DO IMO=INIDOC,LASDOC
          IF(MOSYMO(IMO).EQ.IS) THEN
            N=N+1
            MOSYMN(N)=IS
            DO I=1,NGO
              CFMONW(I,N)=CFMOOD(I,IMO)
            END DO
          END IF
        END DO
        ISYLAB(2,IS,2)=N
        M=N-N0+1
        IF(M.GT.0.AND.IPURFY.NE.0) THEN
C         WRITE(6,*) 'ORTHOGONALISING DOC'
          CALL SCHMIDT(CFMONW(1,N0),S,WORK,NGO,M,NGO)
        ENDIF
      END DO
C**** ORDER ACTIVE ORBITALS ********************************************
      DO IS=1,8
        N0=N+1
        ISYLAB(1,IS,3)=N+1
        DO IMO=INIACT,LASACT
          IF(MOSYMO(IMO).EQ.IS) THEN
            N=N+1
            MOSYMN(N)=IS
            DO I=1,NGO
              CFMONW(I,N)=CFMOOD(I,IMO)
            END DO
          END IF
        END DO
        ISYLAB(2,IS,3)=N
        M=N-N0+1
        IF(M.GT.0.AND.IPURFY.NE.0) THEN
C         WRITE(6,*) 'ORTHOGONALISING ACT'
          CALL SCHMIDT(CFMONW(1,N0),S,WORK,NGO,M,NGO)
        ENDIF
      END DO
C**** ORDER EXTERNAL ORBITALS ******************************************
      DO IS=1,8
        N0=N+1
        ISYLAB(1,IS,4)=N+1
        DO IMO=INIEXT,LASEXT
          IF(MOSYMO(IMO).EQ.IS) THEN
            N=N+1
            MOSYMN(N)=IS
            DO I=1,NGO
              CFMONW(I,N)=CFMOOD(I,IMO)
            END DO
          END IF
        END DO
        ISYLAB(2,IS,4)=N
        M=N-N0+1
        IF(M.GT.0.AND.IPURFY.NE.0) THEN
C         WRITE(6,*) 'ORTHOGONALISING EXT'
          CALL SCHMIDT(CFMONW(1,N0),S,WORK,NGO,M,NGO)
        ENDIF
      END DO
C
      IF(IPURFY.NE.0) THEN
C     DOUBLE CHECK THE ORBITAL ORTHOGONALITY.
        IFLAG=0
        DO I=1,NMO
          DO J=1,I-1
            W=0.0D+00
            DO K=1,NGO
              DO L=1,NGO
                IND=MAX(K,L)
                IND1=(IND*IND-IND)/2+MIN(K,L)
C               W=W+CMO(K,I)*S(K,L)*CMO(L,J)
                W=W+CFMONW(K,I)*S(IND1)*CFMONW(L,J)
              END DO
            END DO
            DIFF=ABS(W)
            IF(DIFF.GE.1.0D-05) THEN
              IFLAG=1
              WRITE(6,'(I4,'' AND'',I4,
     *          '' IS NOT ORTHOGONAL.  DIFF = '',D10.3)') J,I,DIFF
            END IF
          END DO
        END DO
        IF(IFLAG.EQ.1) CALL ABRT
        IF (MASWRK) WRITE(LUNOUT,*) 'PURIFIED ORBITALS ARE ORTHOGONAL.'
C       WRITE(6,*) 'FINAL ORBITALS'
C       CALL PRSQ(CFMONW,NMO,NGO,NGO)
      ENDIF
C**** WRITE RESULTS ****************************************************
      DO K=1,4
        DO J=1,8
          IF(ISYLAB(1,J,K).LE.ISYLAB(2,J,K)) THEN
            ISYPRT(1,J,K)=ISYLAB(1,J,K)
            ISYPRT(2,J,K)=ISYLAB(2,J,K)
          ELSE
            ISYPRT(1,J,K)=0
            ISYPRT(2,J,K)=0
          END IF
        END DO
      END DO
      IF (MASWRK) THEN
      WRITE(LUNOUT,9120)
 9120 FORMAT(/1X,'THE RESULTS OF REARRANGING THE ORBITALS BY SYMMETRY',
     *           ' TYPE ARE')
      WRITE(LUNOUT,'('' SYM.'',8(7X,I1))') (I,I=1,8)
      WRITE(LUNOUT,'(1X,71(1H-))')
      WRITE(LUNOUT,'('' FZC /'',8(I3,''-'',I3,''/''))')
     *  ((ISYPRT(I,J,1),I=1,2),J=1,8)
      WRITE(LUNOUT,'('' DOC /'',8(I3,''-'',I3,''/''))')
     *  ((ISYPRT(I,J,2),I=1,2),J=1,8)
      WRITE(LUNOUT,'('' ACT /'',8(I3,''-'',I3,''/''))')
     *  ((ISYPRT(I,J,3),I=1,2),J=1,8)
      WRITE(LUNOUT,'('' EXT /'',8(I3,''-'',I3,''/''))')
     *  ((ISYPRT(I,J,4),I=1,2),J=1,8)
      WRITE(LUNOUT,'(1X,71(1H-))')
      END IF
C**** RETURN ***********************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQSYM
      SUBROUTINE MQSYM(LUNOUT,LPOUT ,INIACT,LASACT,NOCF  ,NCSF  ,
     *                 ISTSYM,NMO   ,
     *                 IOMAP ,NSNSF ,MOSYM ,KREF  )
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER   IOMAP(INIACT:LASACT+1,NOCF), NSNSF(NOCF+1)
      DIMENSION MOSYM(NMO)
      LOGICAL   KREF(NCSF)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DIMENSION ISYM(8,8)
      DATA ISYM /1,2,3,4,5,6,7,8,
     *           2,1,4,3,6,5,8,7,
     *           3,4,1,2,7,8,5,6,
     *           4,3,2,1,8,7,6,5,
     *           5,6,7,8,1,2,3,4,
     *           6,5,8,7,2,1,4,3,
     *           7,8,5,6,3,4,1,2,
     *           8,7,6,5,4,3,2,1/
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQSYM ***'')')
        CALL MQDEBG(LUNOUT,
     *  'INIACT','LASACT','  NOCF','  NCSF','ISTSYM','   NMO','     -',
     *   INIACT , LASACT ,   NOCF ,   NCSF , ISTSYM ,    NMO ,      0 )
      END IF
C**** DETERMINE CSF SYMMETRY *******************************************
      DO ICSF=1,NCSF
        KREF(ICSF)=.FALSE.
      END DO
      DO IOCF=1,NOCF
        K=1
        DO I=INIACT,LASACT
          IF(IOMAP(I,IOCF).EQ.2) K=ISYM(K,MOSYM(I))
        END DO
C       WRITE(6,*) IOCF,'WIOMAP',(IOMAP(I,IOCF),I=INIACT,LASACT)
        IF(K.EQ.ISTSYM) THEN
          DO ICSF=NSNSF(IOCF)+1,NSNSF(IOCF+1)
            KREF(ICSF)=.TRUE.
          END DO
        END IF
      END DO
      N=0
      DO ICSF=1,NCSF
        IF(KREF(ICSF)) N=N+1
      END DO
      IF (MASWRK) THEN
      WRITE(LUNOUT,9100) N
 9100 FORMAT(1X,'TOTAL NUMBER OF SYMMETRY ADAPTED CSFS =',I10)
      END IF
      IF(N.EQ.0) THEN
        IF (MASWRK) WRITE(LUNOUT,'('' *** ERROR STOP IN SUB.MQSYM ***'',
     *    '' # OF SYMMETRY ADAPTED CSFS MUST BE GREATER THAN 0.'')')
        CALL ABRT
      END IF
C**** RETURN ***********************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQTRF
      SUBROUTINE MQTRF(LUNOUT,LPOUT ,ISWTCH,LUNERI,LUNOER,LUNJMA,
     *                 LUNJM1,LUNJM2,LUNJM3,NGO   ,NMOINT,TRACE,
     *                 NMO   ,MAXERI,MXTRFR,THRERI,PARAIO,DELSCR,
     *                 LIJGO ,LIJMO ,LMOIJI,LMOIJJ,CFMO  ,NMOTRF,
     *                 OERFLG,DOORD0)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION LIJGO (NGO,NGO), LIJMO(NMOTRF,NMOTRF)
      DIMENSION LMOIJI(NMOTRF*(NMOTRF+1)/2), LMOIJJ(NMOTRF*(NMOTRF+1)/2)
      DIMENSION CFMO  (NGO,NGO)
C
      LOGICAL PACK2E,DIRTRF,TRACE
      LOGICAL GOPARR,DSKWRK,MASWRK
      LOGICAL ISTRF
      LOGICAL DELSCR
C
      COMMON /FMCOM / D(1)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,IPOPLE
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /MQIOFI/ IDAF50,NAV50,IODA50(400)
      COMMON /TIMING/ CPU,WALL
      COMMON /TRFOPT/ CUTTRF,NWDTRF,MPTRAN,ITRFAO,NOSYMT,DIRTRF
C
      INTEGER DMOAO,DAOAO
      LOGICAL OERFLG,PARAIO,DOORD0
      COMMON /MQMEM/  MEMINP,MEMDDI
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
C
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQTRF ***'')')
        CALL MQDEBG(LUNOUT,
     *  'ISWTCH','LUNERI','LUNOER','LUNJMA','LUNJM1','LUNJM2','LUNJM3',
     *   ISWTCH , LUNERI , LUNOER , LUNJMA , LUNJM1 , LUNJM2 , LUNJM3 )
        CALL MQDEBG(LUNOUT,
     *  'NMOTRF','   NGO','NMOINT','   NMO','MAXERI','MXTRFR','  IDAF',
     *   NMOTRF ,    NGO , NMOINT ,    NMO , MAXERI , MXTRFR ,   IDAF )
        CALL MQDEBG(LUNOUT,
     *  '   NAV','IDAF50',' NAV50','NINTMX','     -','     -','     -',
     *      NAV , IDAF50 ,  NAV50 , NINTMX ,      0 ,      0 ,      0 )
      END IF
C
C        IF THE NUMBER OF ORBITALS IN THE TRANSFORMATION IS ALL MO-S,
C        THIS IS THE TRANNY FOR THE PERTURBATION RUN (ISTRF=.TRUE.).
C        OTHERWISE IT IS THE TRANNY FOR THE PREPARATION OF CANONICAL
C        ORBITALS, AND INVOLVES ONLY THE OCCUPIED ORBITALS.
C
      ISTRF=NMOTRF.EQ.NMO
C
      IF (ISTRF) THEN
         NGOIJ=NGO*(NGO+1)/2
         NMOIJ=NMO*(NMO+1)/2
         INI1=1
         LAS1=NMOINT*(NMOINT+1)/2
         INI2=LAS1+1
         LAS2=LAS1+(NMO-NMOINT)*NMOINT
         INI3=LAS2+1
         LAS3=LAS2+(NMO-NMOINT)*(NMO-NMOINT+1)/2
C
         IF(ISWTCH.EQ.-1) THEN
            IJLAST=LAS2
            JLAST =NMOINT
         ELSE
            IJLAST=NMOIJ
            JLAST =NMO
         END IF
      ELSE
         NGOIJ=NGO*(NGO+1)/2
         NMOIJ=NMOINT*(NMOINT+1)/2
         INI1=1
         LAS1=NMOINT*(NMOINT+1)/2
         INI2=LAS1+1
         LAS2=LAS1
         INI3=LAS2+1
         LAS3=LAS2
C
         IJLAST=NMOIJ
         JLAST =NMOINT
      END IF
C
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
      M001=MQADDR(NGO*NGO,'D')
      LASTP1=MQMNXT()
      NEED2=LASTP1-M001
      CALL GETFM(NEED2)
      CALL TRPOSE(CFMO,D(M001),NGO,NGO,0)
C
C     --- TRANSFORM ONE ELECTRON INTEGRALS ---
C
      CALL TSECND(STIME)
      WALL1=WALL
C**** ADDRESSING FOR SUB.MQHTRN ****************************************
      CALL VALFM(LOADFM)
      IDUM=MQINIT(LOADFM+1)
C.... HINTGO ...........................................................
      I1 = MQADDR(NGO*NGO,'D')
C.... HINTMO ...........................................................
      I2 = MQADDR(NMO*NMO,'D')
C.... TRIG   ...........................................................
      I3 = MQADDR(NGO*(NGO+1)/2,'D')
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-I1
      CALL GETFM(NEED1)
C
C     READ ONE-ELECTRON AO INTEGRALS, CONVERT TO SQUARE STORAGE
C
      NDSIZE=NGO*(NGO+1)/2
      CALL DAREAD(IDAF,IODA,D(I3),NDSIZE,11,0)
      CALL MQTRSQ(NGO,D(I3),D(I1))
C
C     TRANSFORM
C
      IF(EXETYP.EQ.CHECK) THEN
         CALL VCLR(D(I2),1,NMO*NMO)
      ELSE
         CALL MQHTRN(LUNOUT,LPOUT,NGO,NMO,CFMO,D(I1),D(I2))
      END IF
C
C     WRITE ONE-ELECTRON MO INTEGRALS ON MO BASE
C
      NDSIZE=NMO*NMO
      CALL MQDAWR(IDAF50,IODA50,D(I2),NDSIZE,16,0)
C
      CALL RETFM(NEED1)
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF(TRACE) WRITE(LUNOUT,
     *     '('' CPU TIME FOR MQHTRN ='',F15.3,'' SEC.'')') TIME
      IF(TRACE.AND.LPOUT.LT.0) WRITE(LUNOUT,
     *     '(''WALL TIME FOR MQHTRN ='',F15.3,'' SEC.'')') WALL1
      CALL FLSHBF(LUNOUT)
C
C      GO TO 999
C***********************************************************************
C**** MAKE TABLE *******************************************************
C****            *******************************************************
      IJ=0
      DO I=1,NMOINT
        DO J=1,I
          IJ=IJ+1
C          LIJMO(I,J)=IJ
          LIJMO(J,I)=IJ
          LMOIJI(IJ)=I
          LMOIJJ(IJ)=J
        END DO
      END DO
      IF (ISTRF) THEN
         DO I=NMOINT+1,NMO
            DO J=1,NMOINT
               IJ=IJ+1
C               LIJMO(I,J)=IJ
               LIJMO(J,I)=IJ
               LMOIJI(IJ)=I
               LMOIJJ(IJ)=J
            END DO
         END DO
         DO I=NMOINT+1,NMO
            DO J=NMOINT+1,I
               IJ=IJ+1
C               LIJMO(I,J)=IJ
               LIJMO(J,I)=IJ
               LMOIJI(IJ)=I
               LMOIJJ(IJ)=J
            END DO
         END DO
      END IF
      IJ=0
      DO I=1,NGO
        DO J=1,I
          IJ=IJ+1
C          LIJGO(I,J)=IJ
          LIJGO(J,I)=IJ
        END DO
      END DO
C
C**** CALL MQORD0 ******************************************************
C     THE PURPOSE OF -OERFLG- IS TO ENSURE THAT THE ORDERING DONE
C     BY MQORD0 IS DONE ONLY ONCE.  THIS CODE MAY BE CALLED MULTIPLE
C     TIMES BY SPIN-ORBIT RUN, BUT ALWAYS AT THE SAME NUCLEAR GEOMETRY.
C
      IF (.NOT.OERFLG .AND. DOORD0) THEN
      CALL TSECND(STIME)
      WALL1=WALL
C**** ADDRESSING FOR SUB.MQORD0 ****************************************
      CALL VALFM(LOADFM)
      CALL GOTFM(NGOTMX)
      IDUM=MQINIT(LOADFM+1)
C.... XWORK  ...........................................................
      N001=MQADDR(NINTMX,'D')
C.... IXWORK ...........................................................
C   ALLOCATE THIS INTEGER BUFFER AS REAL IN CASE OF 2 BYTE LABELS
      N002=MQADDR(NINTMX,'D')
C.... LABEL  ...........................................................
      N003=MQADDR(NGOIJ,'I')
C.... WORK   ...........................................................
      N004=MQADDR(NGOIJ,'D')
C.... JFLG   ...........................................................
      N006=MQADDR(NPROC,'I')
C.... VAOAO  ...........................................................
      LASTP1=MQMNXT()
      NR=N001+NGOTMX-LASTP1
C -----------------------------------------------------------------
C     DETERMINE MEMORY ALLOCATIONS FOR THE SORTING PROCEDURES
C
C     ITRFAO.EQ.1 MEANS AOINTS=DUP, WHICH IS THE MOST COMMON SITUATION.
C     EACH NODE JUST READS A FULL AO INTEGRAL FILE AND SORTS THEM.
C
C     ITRFAO.EQ.2 MEANS AOINTS=DIST, AND CAN RUN WITH OR WITHOUT
C     USING DISTRIBUTED MEMORY.  WITH DDI, THE ALGORITHM IS TO READ
C     AO INTEGRALS FROM DISK INTO AN ARRAY, PUT THEM TO DDI MEMORY
C     VIA ACCUMULATE CALLS, AND THEN STORE EACH NODE'S PART OF THE
C     DISTRIBUTED ARRAY TO DISK.  IF DDI MEMORY IS LESS, THIS MAY
C     BE REPEATED UNTIL ALL AO INTEGRALS ARE SORTED.  WITHOUT DDI
C     MEMORY, AO INTEGRALS ARE BROADCAST TO OTHER NODES, SO IT IS
C     PROBABLY FASTER TO USE DISTRIBUTED MEMORY, ESPECIALLY AS THE
C     LATER STEPS WILL RECYCLE THIS MEMORY.
C     NOTE THAT IN ADDITION TO -ITRFAO-, THE -PARAIO- OPTION MAY BE
C     SET TO FALSE IN ORDER TO EXECUTE EITHER OF THE DISTRIBUTED
C     AO INTEGRAL FILE SORTING OPTIONS.  DOES THE USER HAVE TO INPUT
C     AOINTS=DIST IN THIS CASE ???
C
C     UMEDA-SAN SAYS THE VALUE OF -IWIDTH- MUST BE A MULTIPLE OF THE
C     NUMBER OF PROCESSORS USED, AND THAT THE SUBTRACTION IN -MNWTH-
C     ENSURES BUFFER SPACE FOR DDI_ACC CALLS WILL EXIST.
C
      IF (ITRFAO.EQ.1 .OR. (.NOT.PARAIO)) THEN
         IF(MASWRK) WRITE(IW,9310)
         IDMWTH=0
      ELSE
         IWIDTH= NPROC * MIN((NR/NGOIJ)/NPROC,(NGOIJ-1)/NPROC+1)
         MXWTH = ((NGOIJ-1)/NPROC+1)*NPROC
         MNWTH = INT((MEMDDI*1000000.0-NGOIJ*IWIDTH)/NGOIJ)
         IF(MNWTH.GT.MXWTH) MNWTH=MXWTH
         IDMWTH= MIN(MNWTH,MXWTH)
         IDMWTH= MAX(IDMWTH,0)
         MXDDI = INT((MXWTH/1.0D+06)*NGOIJ+1)
         MNDDI = INT((MNWTH/1.0D+06)*NGOIJ+1)
         IF(IDMWTH.EQ.0) MNDDI=0
C
C          THE NEXT LINE FORCES A CHOICE OF NON-DDI SORTING.
C---     IDMWTH = 0
C
         IF (IDMWTH.GE.IWIDTH) THEN
            IDMWTH=IWIDTH
            IF(MASWRK) THEN
               WRITE(LUNOUT,9320) MXDDI,MNDDI
               IF(MNWTH.LT.MXWTH) WRITE(LUNOUT,9485)
            END IF
         ELSE
            IF(MASWRK) WRITE(IW,9330)
            IDMWTH=0
         END IF
      END IF
C
 9310 FORMAT(1X,'SORTING DUPLICATED AO INTEGRAL LIST')
 9320 FORMAT(1X,'SORTING DISTRIBUTED AO INTEGRAL LIST USING',
     *          ' DISTRIBUTED MEMORY'/
     *       1X,'MAXIMUM MEMDDI THAT CAN BE USED IN THE',
     *          ' AO INTEGRAL SORTING IS',I6,' MWORDS'/
     *       1X,'   THE MEMDDI THAT WILL BE USED IN THE',
     *          ' AO INTEGRAL SORTING IS',I6,' MWORDS')
 9330 FORMAT(1X,'SORTING DISTRIBUTED AO INTEGRAL LIST USING',
     *          ' REPLICATED MEMORY')
C
      IF (IDMWTH.EQ.0) IWIDTH=MIN(NR/NGOIJ,(NGOIJ-1)/NPROC+1)
      N005=MQADDR(NGOIJ*IWIDTH,'D')
C
      IF (IDMWTH.GT.0) THEN
         IF (EXETYP.NE.CHECK) CALL DDI_CREATE(NGOIJ,IDMWTH,DAOAO)
      END IF
C
C ------------------------------------------
      IF (IDMWTH.GT.0) THEN
         NREAD=(NGOIJ-1)/IWIDTH+1
      ELSE
         NREAD=(NGOIJ-1)/(IWIDTH*NPROC)+1
      END IF
      IF (MASWRK.AND.LPOUT.LT.0) THEN
         WRITE(LUNOUT,*) 'NR=',NR, ', NGOIJ=',NGOIJ, ', NPROC=',NPROC,
     *        ', IWIDTH(1)=',MIN(NR/NGOIJ,NGOIJ),
     *        ', IWIDTH=',IWIDTH,
     *        ', NREAD=', NREAD
      END IF
      IF (NREAD.GT.MXTRFR.AND.IDMWTH.LE.0) THEN
         MORE = ((NGOIJ-1)/(MXTRFR-1))*NGOIJ - NR
         IF(MASWRK) WRITE(LUNOUT,9000) NREAD,MXTRFR,NR,MORE
         CALL ABRT
      END IF
 9000 FORMAT(//1X,'*** ERROR IN SUB.MQTRF *** EXCESSIVE DISK I/O'/
     *       1X,'NREAD =',I10,' IS GREATER THAN MXTRFR =',I10/
     *       1X,'     AVAILABLE MEMORY IS',I12,' WORDS.'/
     *       1X,'TRY INCREASING MEMORY BY',I12,' WORDS, OR'/
     *       1X,'IF YOU DON''T MIND ABUSING YOUR DISK, ',
     *          'INCREASE -MXTRFR- IN $MCQDPT.'//)
C
      IF(TRACE) WRITE(LUNOUT,
     *     '('' NO. DISK READS FOR SUB.MQORD0 ='',I10)') NREAD
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-N001
      CALL GETFM(NEED1)
      IF(EXETYP.NE.CHECK) THEN
         CALL MQORD0(LUNOUT,LPOUT ,LUNERI,LUNOER,
     *               NGO   ,NGOIJ ,IWIDTH,THRERI,
     *               LIJGO ,D(N001),D(N002),D(N003),D(N004),
     *               D(N005),D(N006),DAOAO,IDMWTH,NREAD,OERFLG)
      ELSE
         NREAD=0
         OERFLG=.TRUE.
      END IF
C
      IF (IDMWTH.GT.0) THEN
         IF (EXETYP.NE.CHECK) CALL DDI_DESTROY(DAOAO)
      END IF
C
C
      CALL RETFM(NEED1)
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF(TRACE) THEN
         WRITE(LUNOUT,
     *     '('' CPU TIME FOR MQORD0 ='',F15.3,'' SEC.'')') TIME
         IF(LPOUT.LT.0) WRITE(LUNOUT,
     *     '(''WALL TIME FOR MQORD0 ='',F15.3,'' SEC.'')') WALL1
      ELSE
         IF(MASWRK) WRITE(LUNOUT,9100) LUNERI,NREAD,TIME
      END IF
      CALL FLSHBF(LUNOUT)
 9100 FORMAT(1X,'AO INTEGRAL FILE',I3,' READ',I6,
     *          ' TIMES, SORTING TIME=',F10.2)
C
C            END OF THE OERFLG/DOORD0 IF BLOCK
C
      END IF
C
C     CHECK AVAILABLE DDI MEMORY
C
      MXWTH = NPROC*   ((IJLAST-1)/NPROC+1)
      MNWTH = NPROC*INT((MEMDDI*1.0D+06)/(NGOIJ*NPROC)-1)
      IF(MNWTH.GT.MXWTH) MNWTH=MXWTH
      IDMWTH= MIN(MNWTH,MXWTH)
      IDMWTH= MAX(IDMWTH,0)
      MXDDI = INT((MXWTH/1.0D+06)*NGOIJ+1)
      MNDDI = INT((MNWTH/1.0D+06)*NGOIJ+1)
      IF(IDMWTH.EQ.0) MNDDI=0
      IF(MASWRK) THEN
         WRITE(LUNOUT,9480) MXDDI,MNDDI
         IF(MNWTH.LT.MXWTH) WRITE(LUNOUT,9485)
      END IF
 9480 FORMAT(/1X,'MAXIMUM MEMDDI THAT CAN BE USED IN THE',
     *          ' INTEGRAL TRANSFORMATION IS',I6,' MWORDS'/
     *       1X,'   THE MEMDDI THAT WILL BE USED IN THE',
     *          ' INTEGRAL TRANSFORMATION IS',I6,' MWORDS')
 9485 FORMAT(1X,'SMALLER AMOUNTS OF DDI MEMORY RUN WITH INCREASED I/O')
C
      IF (IDMWTH.GT.0) THEN
         NWRITE=(IJLAST-1)/IDMWTH+1
         IF (NWRITE.GT.MXTRFR) THEN
            IDMWTH=0
         ELSE
            IF (EXETYP.NE.CHECK) CALL DDI_CREATE(NGOIJ,IDMWTH,DMOAO)
         END IF
         IF(TRACE) WRITE(LUNOUT,
     *        '('' NO. DISK WRITES FOR SUB.MQORD1 ='',I10)') NWRITE
      END IF
C
C**** CALL MQORD1 ******************************************************
C
      CALL TSECND(STIME)
      WALL1=WALL
C**** OPEN FILE(S) *****************************************************
      CALL SEQOPN(LUNJMA,'MCQD56','UNKNOWN',.FALSE.,'UNFORMATTED')
C**** ADDRESSING FOR SUB.MQORD1 ****************************************
      CALL VALFM(LOADFM)
      CALL GOTFM(NGOTMX)
      IDUM=MQINIT(LOADFM+1)
C.... XWORK  ...........................................................
      N001=MQADDR(NINTMX,'D')
C.... IXWORK ...........................................................
C   ALLOCATE THIS INTEGER BUFFER AS REAL IN CASE OF 2 BYTE LABELS
      N002=MQADDR(NINTMX,'D')
C.... LABEL  ...........................................................
      N003=MQADDR(NGOIJ,'I')
C.... WORK   ...........................................................
      N004=MQADDR(NGOIJ,'D')
C.... JFLG   ...........................................................
      N006=MQADDR(NPROC,'I')
C.... VMOAO  ...........................................................
      N011=MQADDR(IJLAST,'D')
C.... S1     ...........................................................
      N012=MQADDR(NGO*NGO,'D')
C.... S2     ...........................................................
      N013=MQADDR(NGO*NGO,'D')
C.... VAOAO  ...........................................................
      IF (.NOT.OERFLG) THEN
         LASTP1=MQMNXT()
         NR=N001+NGOTMX-LASTP1
C         NR=MQMLFT()
C         IWIDTH=MIN(NR/NGOIJ,NGOIJ)
         IWIDTH=MIN(NR/NGOIJ,(NGOIJ-1)/NPROC+1)
         N005=MQADDR(NGOIJ*IWIDTH,'D')
C         NREAD=(NGOIJ-1)/IWIDTH+1
         NREAD=(NGOIJ-1)/(IWIDTH*NPROC)+1
         IF(NREAD.GT.MXTRFR) THEN
            MORE = ((NGOIJ-1)/(MXTRFR-1))*NGOIJ - NR
            IF(MASWRK) WRITE(LUNOUT,9010) NREAD,MXTRFR,NR,MORE
            CALL ABRT
         END IF
         IF(MASWRK) WRITE(LUNOUT,
     *        '('' NO. DISK READS FOR SUB.MQORD1 ='',I10)') NREAD
 9010 FORMAT(//1X,'*** ERROR IN SUB.MQTRF *** EXCESSIVE DISK I/O'/
     *       1X,'NREAD =',I10,' IS GREATER THAN MXTRFR =',I10/
     *       1X,'     AVAILABLE MEMORY IS',I12,' WORDS.'/
     *       1X,'TRY INCREASING MEMORY BY',I12,' WORDS, OR'/
     *       1X,'IF YOU DON''T MIND ABUSING YOUR DISK, ',
     *          'INCREASE -MXTRFR- IN $MCQDPT.'//)
C
      ELSE
         IWIDTH=1
         N005=MQADDR(NGOIJ,'D')
      END IF
C
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-N001
      CALL GETFM(NEED1)
      IF(EXETYP.NE.CHECK) THEN
         CALL MQORD1(LUNOUT ,LPOUT  ,LUNERI,LUNOER,LUNJMA,IDMWTH,
     *               DMOAO  ,NGO    ,NGOIJ  ,IWIDTH,THRERI,
     *               LIJGO  ,D(N001),D(N002),D(N003),D(N004),D(N005),
     *               D(N006),NMOTRF ,LIJMO,IJLAST,JLAST,
     *               D(M001),D(N011),D(N012),D(N013),OERFLG)
      END IF
C
      CALL RETFM(NEED1)
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF(TRACE) THEN
         WRITE(LUNOUT,
     *     '('' CPU TIME FOR MQORD1 ='',F15.3,'' SEC.'')') TIME
         IF(LPOUT.LT.0) WRITE(LUNOUT,
     *     '(''WALL TIME FOR MQORD1 ='',F15.3,'' SEC.'')') WALL1
      ELSE
         IF(MASWRK) WRITE(LUNOUT,9110) TIME
      END IF
      CALL FLSHBF(LUNOUT)
 9110 FORMAT(1X,'TIME FOR FIRST TWO INDEX TRANSFORM=',F10.2)
C***********************************************************************
C**** CALL MQORD2 ******************************************************
C****             ******************************************************
      CALL TSECND(STIME)
      WALL1=WALL
C
C.... (EE/AA)'S ARE NOT NECESSARY NOW ..................................
C
C**** ADDRESSING FOR SUB.MQORD2 ****************************************
      CALL VALFM(LOADFM)
      CALL GOTFM(NGOTMX)
      IDUM=MQINIT(LOADFM+1)
C.... LABEL  ...........................................................
      N001=MQADDR(MAX(NGOIJ,2*MAXERI),'I')
C.... WORK   ...........................................................
      N002=MQADDR(MAX(NGOIJ,2*MAXERI),'D')
C.... VMOAOI ...........................................................
      N003=MQADDR(IJLAST,'D')
C.... S2     ...........................................................
      N010=MQADDR(NGO*NGO,'D')
C.... S3     ...........................................................
      N011=MQADDR(NGO*NMOTRF,'D')
C.... MOSYM  ...........................................................
      N012=MQADDR(NMO,'I')
C.... VMOAO  ...........................................................
      N013=MQADDR(NGOIJ,'D')
C.... VMOAOO ...........................................................
      IF (IDMWTH.GT.0) THEN
         IWIDTH=IDMWTH
         N004=MQADDR(NGOIJ,'D')
      ELSE
         LASTP1=MQMNXT()
         NR=N001+NGOTMX-LASTP1
         IWIDTH=MIN(NR/NGOIJ,IJLAST)
         N004=MQADDR(NGOIJ*IWIDTH,'D')
         NREAD=(IJLAST-1)/IWIDTH+1
         IF(NREAD.GT.MXTRFR) THEN
            MORE = ((IJLAST-1)/(MXTRFR-1))*NGOIJ - NR
            IF(MASWRK) WRITE(LUNOUT,9000) NREAD,MXTRFR,NR,MORE
            CALL ABRT
         END IF
         IF(MASWRK) WRITE(LUNOUT,
     *        '('' NO. DISK READS FOR SUB.MQORD2 ='',I10)') NREAD
C
      END IF
C
      LASTP1=MQMNXT()
      NEED1=LASTP1-N001
      CALL GETFM(NEED1)
C
      NDSIZE=MQDSIZ(NMO,'I')
      CALL MQDARE(IDAF50,IODA50,D(N012),NDSIZE,10,1)
C
      IF(EXETYP.NE.CHECK) THEN
         CALL MQORD2(LUNOUT,LPOUT ,LUNJMA,LUNJM1,LUNJM2,LUNJM3,
     *               IDMWTH,DMOAO,
     *               INI1  ,LAS1  ,INI2  ,LAS2  ,INI3  ,LAS3  ,
     *               NGOIJ ,IJLAST,IWIDTH,ISWTCH,THRERI,
     *               D(N001),D(N002),D(N003),D(N004),
     *               NGO,NMOTRF,NMOINT,MAXERI,D(N011),D(N012),D(N013),
     *               LIJGO ,LIJMO ,LMOIJI,LMOIJJ,D(M001),D(N010))
      END IF
      IF (DELSCR) THEN
         CALL SEQCLO(LUNJMA,'DELETE')
      ELSE
         CALL SEQCLO(LUNJMA,'KEEP')
      END IF
C
      CALL RETFM(NEED1)
C
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      WALL1=WALL-WALL1
      IF(TRACE) THEN
         WRITE(LUNOUT,
     *     '('' CPU TIME FOR MQORD2 ='',F15.3,'' SEC.'')') TIME
         IF(LPOUT.LT.0) WRITE(LUNOUT,
     *     '(''WALL TIME FOR MQORD2 ='',F15.3,'' SEC.'')') WALL1
      ELSE
         IF(MASWRK) WRITE(LUNOUT,9120) TIME
      END IF
 9120 FORMAT(1X,'TIME FOR  LAST TWO INDEX TRANSFORM=',F10.2)
      CALL FLSHBF(LUNOUT)
C
      CALL RETFM(NEED2)
      IF (IDMWTH.GT.0.AND.EXETYP.NE.CHECK) CALL DDI_DESTROY(DMOAO)
      RETURN
      END
C*MODULE MCQDPT  *DECK MQTRF1
      SUBROUTINE MQTRF1(LUNOUT,LPOUT ,
     *                  NGO   ,NGOIJ ,NMO   ,IJLAST,JLAST ,THRERI,
     *                  LIJGO ,LIJMO ,CFMO  ,VAOAO ,VMOAO ,S1,S2)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C==== DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION LIJGO (NGO,NGO), LIJMO (NMO,NMO)
C      DIMENSION CFMO   (NGO,NMO)
      DIMENSION CFMO   (NGO,NGO)
      DIMENSION VAOAO(NGOIJ), VMOAO(IJLAST), S1(NMO,NGO),S2(NGO,NGO)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C      LIJGO(I,J)=J+I*(I-1)/2
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.-10 .AND. MASWRK) THEN
         WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQTRF1 ***'')')
        CALL MQDEBG(LUNOUT,
     *  '     -','     -','   NGO',' NGOIJ','   NMO','IJLAST',' JLAST',
     *        0 ,      0 ,    NGO ,  NGOIJ ,    NMO , IJLAST ,  JLAST )
      END IF
C**** THRESHOLD *******************************************************
      THRESH=THRERI*0.1D+00
      IF(THRESH.LT.1.0D-15) THRESH=1.0D-15
C**** READ ORDERED ERI ON GO BASE *************************************
C       VAOAO = ORDERED ERI ON GO BASE
C**** (PQ/RS) => (IJ/RS) **********************************************
      GO TO 100
C-----------------------------------------------------------------------
C---        DO IP=1,NGO
C---          IPP=LIJGO(IP,IP)
C---          IF(ABS(VAOAO(IPP)).LE.THRESH) THEN
C---            DO I=1,NMO
C---              S1(I,IP)=0.0D+00
C---            END DO
C---          ELSE
C---            GPP=VAOAO(IPP)
C---            DO I=1,NMO
C---              S1(I,IP)=CFMO(I,IP)*GPP
C---            END DO
C---          END IF
C---          DO IQ=1,IP-1
C---            IPQ=LIJGO(IQ,IP)
C---            IF(ABS(VAOAO(IPQ)).GT.THRESH) THEN
C---              GPQ=VAOAO(IPQ)
C---              DO I=1,NMO
C---                S1(I,IQ)=S1(I,IQ)+CFMO(I,IP)*GPQ
C---                S1(I,IP)=S1(I,IP)+CFMO(I,IQ)*GPQ
C---              END DO
C---            END IF
C---          END DO
C---        END DO
C---        DO I=1,IJLAST
C---          VMOAO(I)=0.0D+00
C---        END DO
C---        DO IQ=1,NGO
C---          DO I=1,NMO
C---            IF(ABS(S1(I,IQ)).GT.THRESH) THEN
C---              SQI=S1(I,IQ)
C---              DO J=1,MIN(JLAST,I)
C---                IJ=LIJMO(J,I)
C---                VMOAO(IJ)=VMOAO(IJ)+CFMO(J,IQ)*SQI
C---              END DO
C---            END IF
C---          END DO
C---        END DO
C-----------------------------------------------------------------------
 100    CONTINUE
C
C        CALL VCLR(S2,1,NGO*NGO)
C
        DO IP=1,NGO
           DO IQ=1,IP
              S2(IP,IQ)=VAOAO(LIJGO(IQ,IP))
              S2(IQ,IP)=VAOAO(LIJGO(IQ,IP))
           END DO
        END DO
C
C        CALL VCLR(S1,1,NGO*NGO)
C        DO IP=1,NGO
C           DO IQ=1,NGO
C              DO I=1,NMO
C                 S1(IQ,I)=S1(IQ,I)+CFMO(I,IP)*S2(IQ,IP)
C              END DO
C           END DO
C        END DO
C
C        CALL VCLR(S1,1,NGO*NMO)
        CALL DGEMM('N','T',NGO,NMO,NGO,
     *       1D0,S2,NGO,CFMO,NGO,0D0,S1,NGO)
C
C        CALL VCLR(S2,1,NGO*NGO)
C        DO IQ=1,NGO
C           DO I=1,NMO
C              DO J=1,MIN(JLAST,I)
C                 S2(I,J)=S2(I,J)+CFMO(J,IQ)*S1(IQ,I)
C              END DO
C           END DO
C        END DO
C
C        CALL VCLR(S2,1,NGO*NGO)
        CALL DGEMM('T','T',NMO,NMO,NGO,
     *       1D0,S1,NGO,CFMO,NGO,0D0,S2,NGO)
C
        CALL VCLR(VMOAO,1,IJLAST)
        DO I=1,NMO
           DO J=1,MIN(JLAST,I)
              IJ=LIJMO(J,I)
              VMOAO(IJ)=S2(I,J)
           END DO
        END DO
C
C
C-----------------------------------------------------------------------
C
C**** WRITE OUT (IJ/RS) ************************************************
C       VMOAO = (IJ/RS)
C**** RETURN ***********************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQTRF2
      SUBROUTINE MQTRF2(LUNOUT,LPOUT ,LUNJM1,LUNJM2,LUNJM3,
     *                  INI1  ,LAS1  ,INI2  ,LAS2  ,
     *                  NGO   ,NGOIJ ,NMO   ,IJLAST,NMOINT,MAXERI,
     *                  ISWTCH,THRERI,
     *                  LIJGO ,LIJMO ,LMOIJI,LMOIJJ,CFMO  ,VMOAO ,
     *                  VMOMO ,S3    ,LABELW,WORKW,
     *                  MOSYM ,IJ,S2)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION LIJGO (NGO,NGO), LIJMO (NMO,NMO)
      DIMENSION LMOIJI(IJLAST), LMOIJJ(IJLAST)
C      DIMENSION CFMO  (NGO,NMO)
      DIMENSION CFMO  (NGO,NGO)
      DIMENSION VMOAO(NGOIJ), VMOMO(IJLAST), S3(NGO,NMO),S2(NGO,NGO)
      INTEGER   LABELW(2,MAXERI)
      DIMENSION WORKW (  MAXERI)
      DIMENSION ISYM(8,8), MOSYM(NMO)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DATA ISYM /1,2,3,4,5,6,7,8,
     *           2,1,4,3,6,5,8,7,
     *           3,4,1,2,7,8,5,6,
     *           4,3,2,1,8,7,6,5,
     *           5,6,7,8,1,2,3,4,
     *           6,5,8,7,2,1,4,3,
     *           7,8,5,6,3,4,1,2,
     *           8,7,6,5,4,3,2,1/
C      LIJGO(J,I)=J+I*(I-1)/2
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.-10 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQTRF2 ***'')')
        CALL MQDEBG(LUNOUT,
     *  '    IJ','LUNJM1','LUNJM2','LUNJM3','  INI1','  LAS1','  INI2',
     *       IJ , LUNJM1 , LUNJM2 , LUNJM3 ,   INI1 ,   LAS1 ,   INI2 )
        CALL MQDEBG(LUNOUT,
     *  '  LAS2','   NGO',' NGOIJ','   NMO','IJLAST','NMOINT','MAXERI',
     *     LAS2 ,    NGO ,  NGOIJ ,    NMO , IJLAST , NMOINT , MAXERI )
        CALL MQDEBG(LUNOUT,
     *  'ISWTCH','     -','     -','     -','     -','     -','     -',
     *   ISWTCH ,      0 ,      0 ,      0 ,      0 ,      0 ,      0 )
      END IF
C**** THRESHOLD *******************************************************
      THRESH=THRERI*0.1D+00
      IF(THRESH.LT.1.0D-15) THRESH=1.0D-15
C
C      CALL VCLR(S2,1,NGO*NGO)
C
      DO IR=1,NGO
         DO IS=1,IR
            S2(IR,IS)=VMOAO(LIJGO(IS,IR))
            S2(IS,IR)=VMOAO(LIJGO(IS,IR))
         END DO
      END DO
C
C**** (IJ/RS) => (IJ/KL) ... TYPE: IIII *******************************
C.... READ (IJ/RS) ....................................................
      IF (IJ.GE.INI1 .AND. IJ.LE.LAS1) THEN
      KCONTW=10
        I=LMOIJI(IJ)
        J=LMOIJJ(IJ)
        IJSYM=ISYM(MOSYM(I),MOSYM(J))
C.... GET (IJ/KL) ......................................................
C        DO IR=1,NGO
C          IRR=LIJGO(IR,IR)
C          IF(ABS(VMOAO(IRR)).LE.THRESH) THEN
C            DO K=1,NMOINT
C              S3(K,IR)=0.0D+00
C            END DO
C          ELSE
C            SRR=VMOAO(IRR)
C            DO K=1,NMOINT
C              S3(K,IR)=CFMO(K,IR)*SRR
C            END DO
C          END IF
C          DO IS=1,IR-1
C            IRS=LIJGO(IS,IR)
C            IF(ABS(VMOAO(IRS)).GT.THRESH) THEN
C              SRS=VMOAO(IRS)
C              DO K=1,NMOINT
C                S3(K,IS)=S3(K,IS)+CFMO(K,IR)*SRS
C                S3(K,IR)=S3(K,IR)+CFMO(K,IS)*SRS
C              END DO
C            END IF
C          END DO
C        END DO
C
C        CALL VCLR(S3,1,NGO*NMO)
        CALL DGEMM('N','T',NGO,NMOINT,NGO,
     *       1D0,S2,NGO,CFMO,NGO,0D0,S3,NGO)
C
        DO K=INI1,LAS1
          VMOMO(K)=0.0D+00
        END DO
C
C        DO IS=1,NGO
C          DO K=1,NMOINT
C             SSK=S3(IS,K)
C            IF(ABS(SSK).GT.THRESH) THEN
C              DO L=1,K
C                KL=LIJMO(L,K)
C                VMOMO(KL)=VMOMO(KL)+CFMO(L,IS)*SSK
C              END DO
C            END IF
C          END DO
C        END DO
C
C        CALL VCLR(S2,1,NGO*NGO)
        CALL DGEMM('T','T',NMOINT,NMOINT,NGO,
     *       1D0,S3,NGO,CFMO,NGO,0D0,S2,NGO)
C
        DO K=1,NMOINT
           DO L=1,K
              VMOMO(LIJMO(L,K))=S2(K,L)
           END DO
        END DO
C
C.... WRITE (IJ/KL) ....................................................
        N=0
        DO K=1,NMOINT
          DO L=1,K
            KL=LIJMO(L,K)
            V=ABS(VMOMO(KL))
            IF(V.GE.THRERI) THEN
              IJKLSY=ISYM(IJSYM,ISYM(MOSYM(K),MOSYM(L)))
              IF(IJKLSY.NE.1) GO TO 100
C
              N=N+1
              IF(N.GT.MAXERI) THEN
                WRITE(LUNJM1) KCONTW,I,J,MAXERI
                CALL MQGENW(LUNJM1,2*MAXERI,MAXERI,LABELW,WORKW)
                N=1
              END IF
              LABELW(1,N)=K
              LABELW(2,N)=L
              WORKW(N)=VMOMO(KL)
            END IF
 100        CONTINUE
          END DO
        END DO
        IF(IJ.NE.LAS1) THEN
          KCONTW=1
        ELSE
          KCONTW=0
        END IF
        WRITE(LUNJM1) KCONTW,I,J,N
        CALL MQGENW(LUNJM1,2*N,N,LABELW,WORKW)
      END IF
C**** (IJ/RS) => (IJ/KL) ... TYPE: EIII *******************************
C**** (IJ/RS) => (IJ/KL) ... TYPE: EIEI *******************************
C.... READ (IJ/RS) ....................................................
      IF (IJ.GE.INI2 .AND. IJ.LE.LAS2) THEN
        I=LMOIJI(IJ)
        J=LMOIJJ(IJ)
        IJSYM=ISYM(MOSYM(I),MOSYM(J))
C.... GET (IJ/KL) ......................................................
C        DO IR=1,NGO
C          IRR=LIJGO(IR,IR)
C          IF(ABS(VMOAO(IRR)).LE.THRESH) THEN
C            DO K=1,NMO
C              S3(K,IR)=0.0D+00
C            END DO
C          ELSE
C            SRR=VMOAO(IRR)
C            DO K=1,NMO
C              S3(K,IR)=CFMO(K,IR)*SRR
C            END DO
C          END IF
C          DO IS=1,IR-1
C            IRS=LIJGO(IS,IR)
C            IF(ABS(VMOAO(IRS)).GT.THRESH) THEN
C              SRS=VMOAO(IRS)
C              DO K=1,NMO
C                S3(K,IS)=S3(K,IS)+CFMO(K,IR)*SRS
C                S3(K,IR)=S3(K,IR)+CFMO(K,IS)*SRS
C              END DO
C            END IF
C          END DO
C        END DO
C
C        CALL VCLR(S3,1,NGO*NMO)
        CALL DGEMM('N','T',NGO,NMO,NGO,
     *       1D0,S2,NGO,CFMO,NGO,0D0,S3,NGO)
C
        DO K=INI1,LAS2
          VMOMO(K)=0.0D+00
        END DO
C
C        DO IS=1,NGO
C           DO K=1,NMOINT
C              SSK=S3(IS,K)
C            IF(ABS(SSK).GT.THRESH) THEN
C              DO L=1,K
C                KL=LIJMO(L,K)
C                VMOMO(KL)=VMOMO(KL)+CFMO(L,IS)*SSK
C              END DO
C            END IF
C          END DO
C          DO K=NMOINT+1,NMO
C              SSK=S3(IS,K)
C            IF(ABS(SSK).GT.THRESH) THEN
C              DO L=1,NMOINT
C                KL=LIJMO(L,K)
C                VMOMO(KL)=VMOMO(KL)+CFMO(L,IS)*SSK
C              END DO
C            END IF
C          END DO
C        END DO
C
C
C        CALL VCLR(S2,1,NGO*NGO)
        CALL DGEMM('T','T',NMO,NMOINT,NGO,
     *       1D0,S3,NGO,CFMO,NGO,0D0,S2,NGO)
C
        DO K=1,NMOINT
           DO L=1,K
              VMOMO(LIJMO(L,K))=S2(K,L)
           END DO
        END DO
C
        DO K=NMOINT+1,NMO
           DO L=1,NMOINT
              VMOMO(LIJMO(L,K))=S2(K,L)
           END DO
        END DO
C
C.... WRITE (IJ/KL) .........TYPE: EIII ...............................
        KCONTW=10
        N=0
        DO K=1,NMOINT
          DO L=1,K
            KL=LIJMO(L,K)
            V=ABS(VMOMO(KL))
            IF(V.GE.THRERI) THEN
              IJKLSY=ISYM(IJSYM,ISYM(MOSYM(K),MOSYM(L)))
              IF(IJKLSY.NE.1) GO TO 200
C
              N=N+1
              IF(N.GT.MAXERI) THEN
                WRITE(LUNJM2) KCONTW,I,J,MAXERI
                CALL MQGENW(LUNJM2,2*MAXERI,MAXERI,LABELW,WORKW)
                N=1
              END IF
              LABELW(1,N)=K
              LABELW(2,N)=L
              WORKW(N)=VMOMO(KL)
            END IF
 200        CONTINUE
          END DO
        END DO
        IF(IJ.NE.LAS2) THEN
          KCONTW=1
        ELSE
          KCONTW=0
        END IF
        WRITE(LUNJM2) KCONTW,I,J,N
        CALL MQGENW(LUNJM2,2*N,N,LABELW,WORKW)
C
C.... WRITE (IJ/KL) .........TYPE: EIEI ...............................
        KCONTW=10
        N=0
        DO K=NMOINT+1,NMO
          DO L=1,NMOINT
            KL=LIJMO(L,K)
            V=ABS(VMOMO(KL))
            IF(V.GE.THRERI) THEN
              IJKLSY=ISYM(IJSYM,ISYM(MOSYM(K),MOSYM(L)))
              IF(IJKLSY.NE.1) GO TO 210
C
              N=N+1
              IF(N.GT.MAXERI) THEN
                WRITE(LUNJM3) KCONTW,I,J,MAXERI
                CALL MQGENW(LUNJM3,2*MAXERI,MAXERI,LABELW,WORKW)
                N=1
              END IF
              LABELW(1,N)=K
              LABELW(2,N)=L
              WORKW(N)=VMOMO(KL)
            END IF
 210        CONTINUE
          END DO
        END DO
        IF(IJ.NE.LAS2) THEN
          KCONTW=1
        ELSE
          KCONTW=0
        END IF
        WRITE(LUNJM3) KCONTW,I,J,N
        CALL MQGENW(LUNJM3,2*N,N,LABELW,WORKW)
      END IF
C**** (IJ/RS) => (IJ/KL) ... TYPE: EEII *******************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQTRSQ
      SUBROUTINE MQTRSQ(NSIZE,TRIG,SQUA)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TRIG(NSIZE*(NSIZE+1)/2), SQUA(NSIZE,NSIZE)
      N=0
      DO I=1,NSIZE
        DO J=1,I
          N=N+1
          SQUA(I,J)=TRIG(N)
          SQUA(J,I)=TRIG(N)
        END DO
      END DO
      RETURN
      END
C*MODULE MCQDPT  *DECK MQVSML
      SUBROUTINE MQVSML(LUNOUT,LPOUT ,LUNIII,LUNFT0,
     *                  NMO   ,NMOACT,INIMO ,LASMO ,NMOAA ,NCSF  ,
     *                  NSTATE,MAXERI,MAXCSF,
     *                  HEFF  ,CASVEC,ECONF ,EREF0 ,LWCD  ,LABEL1,
     *                  WORKER,WORKGE,LIJMO ,HCORE ,VONEEL,VTWOEL,
     *                  VSML  ,KREF  ,EDSHFT)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***********************************************************************
C****                     **********************************************
C**** ARRAYS AND CONSTANT **********************************************
C****                     **********************************************
      DIMENSION HEFF  (NSTATE,NSTATE,0:3)
      DIMENSION CASVEC(NCSF,NSTATE)
      DIMENSION ECONF (NCSF)    , EREF0 (NSTATE)
      INTEGER   LWCD(2,MAXERI)
      INTEGER   LABEL1(4,MAXCSF)
      DIMENSION WORKER(MAXERI), WORKGE(MAXCSF)
      DIMENSION LIJMO(INIMO:LASMO,INIMO:LASMO)
      DIMENSION HCORE(NMO,NMO), VONEEL(INIMO:LASMO,INIMO:LASMO)
      DIMENSION VTWOEL(NMOAA,NMOAA)
      DIMENSION VSML(NCSF,NSTATE)
      LOGICAL   KREF(NCSF)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /MQFT02/NWFT0,NWFT0P
C
C**** THRESHOLD ZERO ***************************************************
      THRZRO=1.0D-15
C
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQCACI ***'')')
        CALL MQDEBG(LUNOUT,
     *  'LUNIII','LUNFT0','   NMO','NMOACT',' INIMO',' LASMO',' NMOAA',
     *   LUNIII , LUNFT0 ,    NMO , NMOACT ,  INIMO ,  LASMO ,  NMOAA )
        CALL MQDEBG(LUNOUT,
     *  '  NCSF','NSTATE','MAXERI','MAXCSF','     -','     -','     -',
     *     NCSF , NSTATE , MAXERI , MAXCSF ,      0 ,      0 ,      0 )
      END IF
C**** CLEAR ARRAYS *****************************************************
      CALL VCLR(VONEEL,1,NMOACT*NMOACT)
      CALL VCLR(VTWOEL,1,NMOAA*NMOAA)
C      DO J=INIMO,LASMO
C        DO I=INIMO,LASMO
C          VONEEL(I,J)=0.0D+00
C        END DO
C      END DO
C      DO J=1,NMOAA
C        DO I=1,NMOAA
C          VTWOEL(I,J)=0.0D+00
C        END DO
C      END DO
C**** PREPARE LIJMO ****************************************************
      N=0
      DO I=INIMO,LASMO
        DO J=INIMO,I
          N=N+1
          LIJMO(I,J)=N
          LIJMO(J,I)=N
        END DO
      END DO
C**** PREPARE PERTURBATION MATRICES ************************************
C
C     VTWOEL AND VONEEL
C
      CALL SEQREW(LUNIII)
      LUNIN=LUNIII
  114 READ(LUNIN) KCONT,IMOAB,JMOAB,NWORD
      IF(KCONT.EQ.0.AND.NWORD.EQ.0) GO TO 117
      CALL MQGENR(LUNIN,2*NWORD,NWORD,LWCD,WORKER)
      DO 116 I=1,NWORD
        LP=IMOAB
        LQ=JMOAB
        LR=LWCD(1,I)
        LS=LWCD(2,I)
        V =WORKER(I)
        IF(LP.GE.INIMO .AND. LQ.GE.INIMO .AND.
     *     LR.LT.INIMO .AND. LS.LT.INIMO .AND. LR.EQ.LS) THEN
          IF(LP.NE.LQ) THEN
            VONEEL(LP,LQ)=VONEEL(LP,LQ)+2.0D+00*V
            VONEEL(LQ,LP)=VONEEL(LQ,LP)+2.0D+00*V
          ELSE
            VONEEL(LP,LQ)=VONEEL(LP,LQ)+2.0D+00*V
          END IF
        ELSE IF(LP.GE.INIMO .AND. LQ.LT.INIMO .AND.
     *          LR.GE.INIMO .AND. LS.LT.INIMO .AND. LQ.EQ.LS) THEN
          VONEEL(LP,LR)=VONEEL(LP,LR)-V
        ELSE IF(LP.GE.INIMO .AND. LQ.GE.INIMO .AND.
     *          LR.GE.INIMO .AND. LS.GE.INIMO) THEN
          VTWOEL(LIJMO(LP,LQ),LIJMO(LR,LS))=V
        END IF
  116 CONTINUE
      IF(KCONT.NE.0) GO TO 114
 117  CONTINUE
      IF(GOPARR) CALL DDI_GSUMF(2517,VTWOEL,NMOAA*NMOAA)
      DO J=INIMO,LASMO
         IF (MOD(J,NPROC).EQ.ME) THEN
            DO I=INIMO,LASMO
               DO K=INIMO,LASMO
                  VONEEL(I,J)=VONEEL(I,J)
     *                 -0.5D+00*VTWOEL(LIJMO(I,K),LIJMO(K,J))
               END DO
            END DO
         END IF
      END DO
      IF(GOPARR) CALL DDI_GSUMF(2518,VONEEL,NMOACT*NMOACT)
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LE.-10 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** MATRIX LIJMO ***'')')
        CALL MQWMAI(LUNOUT,LIJMO,NMOACT,NMOACT,NMOACT,'    ')
        WRITE(LUNOUT,'('' *** MATRIX HCORE ***'')')
        CALL MQWMAG(LUNOUT,HCORE,NMO,NMO,NMO,'TRIG')
        WRITE(LUNOUT,'('' *** MATRIX VONEEL ***'')')
        CALL MQWMAG(LUNOUT,VONEEL,NMOACT,NMOACT,NMOACT,'    ')
        IF(LPOUT.LE.-100) THEN
          WRITE(LUNOUT,'('' *** MATRIX VTWOEL ***'')')
          CALL MQWMAG(LUNOUT,VTWOEL,NMOAA,NMOAA,NMOAA,'    ')
        END IF
      END IF
C**** WRITE 2ND-ORDER EFFECTIVE HAMILTONIAN ****************************
      IF (MASWRK) THEN
         WRITE(LUNOUT,'('' ###   RESULTS   ###'')')
         WRITE(LUNOUT,'('' *** 2ND-ORDER EFFECTIVE HAMILTONIAN'',
     *                  '' (BEFORE SYMMETRIZATION) ***'')')
         CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
         WRITE(LUNOUT,'('' ===>'')')
      END IF
C**** CASVEC -> H*CASVEC ***********************************************
      CALL VCLR(VSML,1,NSTATE*NCSF)
C      DO ISTATE=1,NSTATE
C        DO I=1,NCSF
C          VSML(I,ISTATE)=0.0D+00
C        END DO
C      END DO
      CALL SEQREW(LUNFT0)
      DO INWFT0=1,NWFT0
      READ(LUNFT0) KCONT,NWORD
      CALL MQGENR(LUNFT0,4*NWORD,NWORD,LABEL1,WORKGE)
      DO I=1,NWORD
        M1 =LABEL1(1,I)
        M2 =LABEL1(2,I)
        LP =LABEL1(3,I)
        LQ =LABEL1(4,I)
        VALI=WORKGE(I)
        V=(HCORE(LP,LQ)+VONEEL(LP,LQ))*VALI
        IF(.NOT.KREF(M1) .AND. KREF(M2)) THEN
          DO ISTATE=1,NSTATE
            VSML(M1,ISTATE)=VSML(M1,ISTATE)+V*CASVEC(M2,ISTATE)
          END DO
        END IF
        DO J=1,NWORD
C         M3 =LABEL1(1,J)
          M4 =LABEL1(2,J)
          LR =LABEL1(3,J)
          LS =LABEL1(4,J)
          VALJ=WORKGE(J)
          V=0.5D+00*VTWOEL(LIJMO(LQ,LP),LIJMO(LR,LS))*VALI*VALJ
          IF(.NOT.KREF(M2) .AND. KREF(M4)) THEN
            DO ISTATE=1,NSTATE
              VSML(M2,ISTATE)=VSML(M2,ISTATE)+V*CASVEC(M4,ISTATE)
            END DO
          END IF
        END DO
      END DO
C      IF(KCONT.NE.0) GO TO 136
      END DO
      IF (GOPARR) CALL DDI_GSUMF(2539,VSML,NSTATE*NCSF)
C**** CALCULATE SECOND ORDER CONTRIBUTION TO HEFF **********************
      DO ISTATE=1,NSTATE
        DO JSTATE=1,NSTATE
          DO ICSF=1,NCSF
            IF(.NOT.KREF(ICSF)) THEN
              VI=VSML(ICSF,ISTATE)
              VJ=VSML(ICSF,JSTATE)
              DELTA=EREF0(JSTATE)-ECONF(ICSF)
              IF(ABS(VI).GE.THRZRO .AND. ABS(VJ).GE.THRZRO) THEN
                HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)+
     *          VI*VJ/(DELTA-EDSHFT/DELTA)
C    *          VI*VJ/(EREF0(JSTATE)-ECONF(ICSF)-EDSHFT)
              END IF
            END IF
          END DO
        END DO
      END DO
C**** WRITE 2ND-ORDER EFFECTIVE HAMILTONIAN ****************************
      IF(MASWRK)
     *     CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
C**** RETURN ***********************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQWMAG
      SUBROUTINE MQWMAG(LUNOUT,A,NPHISI,NROW,NCOL,KFORM)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C**** DIMENSION REFERRED ***********************************************
      DIMENSION A(NPHISI,NCOL)
      CHARACTER*4 KFORM
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      IF (MASWRK) THEN
C
C**** NON-TRIGONAL CASE ************************************************
C
      IF((KFORM.NE.'TRIG').AND.(KFORM.NE.'SYMM') .AND.
     *   (KFORM.NE.'TRIG').AND.(KFORM.NE.'SYMM')) THEN
        INICOL=1
  100   LASCOL=INICOL+4
        IF(LASCOL.GT.NCOL) LASCOL=NCOL
        WRITE(LUNOUT,9100) (J,J=INICOL,LASCOL)
        DO 200 I=1,NROW
          WRITE(LUNOUT,9110) I,(A(I,J),J=INICOL,LASCOL)
  200   CONTINUE
        INICOL=LASCOL+1
        IF(INICOL.LE.NCOL) GO TO 100
C
C**** TRIGONAL CASE ****************************************************
C
      ELSE
        INICOL=1
  300   LASCOL=INICOL+4
        IF(LASCOL.GT.NCOL) LASCOL=NCOL
        WRITE(LUNOUT,9100) (J,J=INICOL,LASCOL)
        DO 400 I=INICOL,NROW
          K=LASCOL
          IF(I.LT.LASCOL) K=I
          WRITE(LUNOUT,9110) I,(A(I,J),J=INICOL,K)
  400   CONTINUE
        INICOL=LASCOL+1
        IF(INICOL.LE.NCOL) GO TO 300
      END IF
      END IF
      RETURN
C
 9100 FORMAT(7X,5I13)
 9110 FORMAT(I6,1X,1P,5E13.6)
      END
C*MODULE MCQDPT  *DECK MQWMAI
      SUBROUTINE MQWMAI(LUNOUT,L,NPHISI,NROW,NCOL,KFORM)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
C**** DIMENSION REFERRED ***********************************************
      INTEGER    L(NPHISI,NCOL)
      CHARACTER*4 KFORM
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C***********************************************************************
C****       ************************************************************
C**** START ************************************************************
C****       ************************************************************
C**** NON-TRIGONAL CASE ************************************************
      IF (MASWRK) THEN
      IF((KFORM.NE.'TRIG').AND.(KFORM.NE.'SYMM') .AND.
     *   (KFORM.NE.'TRIG').AND.(KFORM.NE.'SYMM')) THEN
        INICOL=1
  100   LASCOL=INICOL+9
        IF(LASCOL.GT.NCOL) LASCOL=NCOL
        WRITE(LUNOUT,'(1X,71(1H-)/9X,10(I5,''.''))') (J,J=INICOL,LASCOL)
        DO 200 I=1,NROW
          WRITE(LUNOUT,'(I7,''.'',1X,10I6)') I,(L(I,J),J=INICOL,LASCOL)
  200   CONTINUE
        INICOL=LASCOL+1
        IF(INICOL.LE.NCOL) GO TO 100
        WRITE(LUNOUT,'(1X,71(1H-))')
C**** TRIGONAL CASE ****************************************************
      ELSE
        INICOL=1
  300   LASCOL=INICOL+9
        IF(LASCOL.GT.NCOL) LASCOL=NCOL
        WRITE(LUNOUT,'(1X,71(1H-)/9X,10(I5,''.''))') (J,J=INICOL,LASCOL)
        DO 400 I=INICOL,NROW
          K=LASCOL
          IF(I.LT.LASCOL) K=I
          WRITE(LUNOUT,'(I7,''.'',1X,10I6)') I,(L(I,J),J=INICOL,K)
  400   CONTINUE
        INICOL=LASCOL+1
        IF(INICOL.LE.NCOL) GO TO 300
        WRITE(LUNOUT,'(1X,71(1H-))')
      END IF
      END IF
C**** RETURN ***********************************************************
      RETURN
      END
C*MODULE MCQDPT  *DECK MQCLMO
      SUBROUTINE MQCLMO(NGO,NMO,NQMT,Q,S,CMO,LABMO,WRK)
     *
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION CMO(NGO,NMO),S(NGO,NGO),Q(NGO,*),LABMO(NGO),WRK(NGO)
C
C        WRITE(6,*) 'ORBITAL PURIFICATION FINISHED'
C        CALL PRSQ(CMO,NMO,NGO,NGO)
         CALL SYMMOS(LABMO,Q,S,CMO,WRK,NQMT,NGO,NMO,NGO)
         CALL ORBPUR(S,CMO,NGO,NMO,.TRUE.)
C        WRITE(6,*) 'ORBITAL PURIFICATION FINISHED'
C        CALL PRSQ(CMO,NMO,NGO,NGO)
      RETURN
      END
C*MODULE MCQDPT  *DECK MQPRWF
      SUBROUTINE MQPRWF
     *  (LUNOUT,NCSF  ,NSTATE,NOCF  ,MAXSOC,MAXSPF,NMOACT,
     *   EIGVAL,CIVEC ,IOMAP ,ISMAP ,IOCSF ,ICASE        )
C
C     THIS ROUTINE PRINTS CI VECTOR(S) COMPUTED BY THE CAS-CI ROUTINE
C     IN A SIMILAR FORMAT TO ROUTINE PRCIVC (MTHLIB.SRC).
C
C     CODED BY H. NAKANO (FRI MAY 05 14:24:14 JST 2001)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     INPUT ARRAYS
C
C     EIGVAL: CI EIGENVALUE(S)
C     CIVEC : CI EIGENVECTOR(S)
C     IOMAP : ORBITAL FUNCTIONS
C             (1:UNOCC, 2:SINGLY OCC, 3:DOUBLY OCC.)
C     ISMAP : SPIN FUNCTIONS (S**2 ADAPTED)
C             (1:UP, 2:DOWN)
C     IOCSF(1,*): CSF -> ORBITAL FUNCTION
C          (2,*): CSF -> SPIN FUNCTION
C
      DIMENSION EIGVAL(NSTATE), CIVEC(NCSF,NSTATE)
      DIMENSION IOMAP(NMOACT+1,NOCF), ISMAP(MAXSOC+1,MAXSPF)
      DIMENSION IOCSF(2,NCSF)
C
C     WORKING ARRAYS
C
      DIMENSION ICASE(NMOACT)
      CHARACTER COCC(4)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DATA COCC /'0','+','-','2'/
C
C     THRESHOLD VALUE IS A CONTANT FOR NOW
C
      PRTTOL=5.0D-02
C
C     LOOP FOR STATES
C
      DO ISTATE=1,NSTATE
        IF (MASWRK) WRITE (LUNOUT,100) ISTATE,EIGVAL(ISTATE)
C
C       LOOP FOR CSFS
C
        DO ICSF=1,NCSF
C
C         IF |CI| > PRTTOL, THEN PRINT CI
C
          IF(ABS(CIVEC(ICSF,ISTATE)).GT.PRTTOL) THEN
            IOCF=IOCSF(1,ICSF)
            ISF =IOCSF(2,ICSF)
            NSOC=MAXSOC-IOMAP(NMOACT+1,IOCF)
C
C           LOOP FOR ACTIVE ORBITALS
C
            DO IMO=1,NMOACT
C
C             FIND THE CASE NUMBER FOR THE ORBITAL
C
C             IOMAP ISMAP -> CASE
C               1              1   (UNOCCUPIED)
C               2     2        2   (ALPHA COUPLING)
C               2     1        3   (BETA COUPLING)
C               3              4   (DOUBLY OCCUPIED)
C             (ISMAP IS ASSIGNED ONLY TO SINGLY OCCUPIED ORBITALS)
C
              IO=IOMAP(IMO,IOCF)
              IF(IO.EQ.1) THEN
                ICASE(IMO)=1
              ELSE IF(IO.EQ.2) THEN
                NSOC=NSOC+1
                IF(ISMAP(NSOC,ISF).EQ.2) THEN
                  ICASE(IMO)=2
                ELSE
                  ICASE(IMO)=3
                END IF
              ELSE
                ICASE(IMO)=4
              END IF
C
            END DO
C
C           PRINT CSF LABEL, CI COEFFICENT, AND CONFIGURATION
C
            IF (MASWRK) WRITE(LUNOUT,102) ICSF,CIVEC(ICSF,ISTATE),
     $        (COCC(ICASE(IMO)),IMO=1,NMOACT)
C
          END IF
        END DO
      END DO
      RETURN
  100 FORMAT(/1X,'STATE #',I5,'  ENERGY =',F18.9//
     *  6X,'CSF',6X,'COEF',4X,'OCCUPANCY (IGNORING CORE)'/
     *  6X,'---',6X,'----',4X,'--------- --------- -----')
  102 FORMAT(1X,I8,F12.6,2X,50A1,(/23X,50A1))
      END
C
C*MODULE MCQDPT  *DECK MQGNR2
      SUBROUTINE MQGNR2(LUNI,LENLAB,LENWOR,LABEL,WORK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER   LABEL(LENLAB)
      DIMENSION WORK (LENWOR)
C
      READ(LUNI) LABEL,WORK
C
      RETURN
      END
C
C*MODULE MCQDPT  *DECK MQGNW2
      SUBROUTINE MQGNW2(LUNO,LENLAB,LENWOR,LABEL,WORK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER   LABEL(LENLAB)
      DIMENSION WORK (LENWOR)
C
      WRITE(LUNO) LABEL,WORK
C
      RETURN
      END
C*MODULE MCQDPT  *DECK MQHASH
      INTEGER FUNCTION MQHASH(OMAP,NMOACT,NHASH)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER OMAP(NMOACT)
C
C      HASH=0D0
      MQHASH=0
      DO I=1,NMOACT
C         HASH=HASH*4+OMAP(I)
         MQHASH=MOD((MQHASH*4+OMAP(I)),NHASH)
      END DO
C      HASH=ABS(HASH)*(SQRT(5D0)-1)*0.5D0
C      MQHASH=INT((HASH-INT(HASH))*NHASH)+1
      MQHASH=MQHASH+1
C
      RETURN
      END
C*MODULE MCQDPT  *DECK MQORD0
      SUBROUTINE MQORD0(LUNOUT,LPOUT ,LUNERI,LUNOER,
     *                  NGO   ,NGOIJ ,IWIDTH,THRERI,
     *                  LIJGO ,XWORK ,IX    ,LABEL ,WORK  ,
     *                  VAOAO ,JFLG  ,DAOAO ,IDMWTH,NREAD,OERFLG)
C=======================================================================
C====                                                              =====
C====              THIS ROUTINE WAS CODED BY H.NAKANO              =====
C====                                                              =====
C====    DEPARTMENT OF APPLIED CHEMISTRY, UNIVERSITY OF TOKYO      =====
C====          HONGO 7-3-1, BUNKYO-KU, TOKYO 113, JAPAN            =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL PACK2E
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,IPOPLE
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /PCKLAB/ LABSIZ
      DIMENSION LIJGO (NGO   ,NGO   )
      DIMENSION XWORK(NINTMX), IX(NINTMX)
      INTEGER   LABEL(NGOIJ)
      DIMENSION WORK(NGOIJ)
      DIMENSION VAOAO(NGOIJ,IWIDTH)
      DIMENSION JFLG(0:NPROC-1)
      LOGICAL GOPARR,DSKWRK,MASWRK,DIRTRF
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TRFOPT/ CUTTRF,NWDTRF,MPTRAN,ITRFAO,NOSYMT,DIRTRF
C
      LOGICAL OERFLG
      INTEGER DAOAO
C
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQORD0 ***'')')
        CALL MQDEBG(LUNOUT,
     *  'LUNERI','LUNOER','   NGO',' NGOIJ','IWIDTH','NINTMX','IDMWTH',
     *   LUNERI , LUNOER ,    NGO ,  NGOIJ , IWIDTH , NINTMX , IDMWTH )
      END IF
C
      NREAD=0
      IF (OERFLG) RETURN
      IZERO=0
C
      IF(ITRFAO.EQ.1) THEN
C -------------------------------------------------------  ITRFAO == 1
C
      KCONT=1
      INIT=1
  100 CONTINUE
        INITM1=INIT-1
        LAST=INITM1+IWIDTH*NPROC
        IF(LAST.GT.NGOIJ) LAST=NGOIJ
        CALL VCLR(VAOAO,1,NGOIJ*IWIDTH)
C
C**** READ ERI VALUES FROM HONDO FILE **********************************
C
        CALL SEQREW(LUNERI)
        NREAD=NREAD+1
  200   CONTINUE
          CALL PREAD(LUNERI,XWORK,IX,NX,NINTMX)
          IF(NX.NE.0) THEN
             MX=IABS(NX)
             IF(MX.GT.NINTMX) CALL ABRT
             DO IERI=1,MX
                VERI=XWORK(IERI)
C
                       NPACK = IERI
                       IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                         LABLE = IX( 2*NPACK - 1 )
                         IPACK = ISHFT( LABLE, -16 )
                         JPACK = IAND(  LABLE, 65535 )
                         LABLE = IX( 2*NPACK     )
                         KPACK = ISHFT( LABLE, -16 )
                         LPACK = IAND(  LABLE, 65535 )
#endif
#if defined(SPEC_CPU_ILP64)
                         LABLE = IX(NPACK)
                         IPACK = ISHFT( LABLE, -48 )
                         JPACK = IAND( ISHFT( LABLE, -32 ), 65535 )
                         KPACK = IAND( ISHFT( LABLE, -16 ), 65535 )
                         LPACK = IAND( LABLE, 65535 )
#endif
                       ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                         LABLE = IX(NPACK)
                         IPACK = ISHFT( LABLE, -24 )
                         JPACK = IAND( ISHFT( LABLE, -16 ), 255 )
                         KPACK = IAND( ISHFT( LABLE,  -8 ), 255 )
                         LPACK = IAND( LABLE, 255 )
#endif
#if defined(SPEC_CPU_ILP64)
                         IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                           LABLE = IX( NPACK/2 )
                           IPACK = IAND( ISHFT( LABLE, -24 ), 255 )
                           JPACK = IAND( ISHFT( LABLE, -16 ), 255 )
                           KPACK = IAND( ISHFT( LABLE,  -8 ), 255 )
                           LPACK = IAND( LABLE, 255 )
                         ELSE
                           LABLE = IX( (NPACK/2)+1 )
                           IPACK = ISHFT( LABLE, -56 )
                           JPACK = IAND( ISHFT( LABLE, -48 ), 255 )
                           KPACK = IAND( ISHFT( LABLE, -40 ), 255 )
                           LPACK = IAND( ISHFT( LABLE, -32 ), 255 )
                         END IF
#endif
                       END IF
                       IA = IPACK
                       IB = JPACK
                       IC = KPACK
                       ID = LPACK
C
C                IAB              =LIJGO(IA,IB)
                IAB              =LIJGO(IB,IA)
C                ICD              =LIJGO(IC,ID)
                ICD              =LIJGO(ID,IC)
                IF(IA.EQ.IB)   VERI=2.0D+00*VERI
                IF(IC.EQ.ID)   VERI=2.0D+00*VERI
                IF(IAB.EQ.ICD) VERI=2.0D+00*VERI
                IABDUM=IAB-INITM1
                ICDDUM=ICD-INITM1
C                IF(IAB.GE.INIT .AND. IAB.LE.LAST)
C     *               VAOAO(ICD,IABDUM)=VERI
                IF(IAB.GE.INIT .AND. IAB.LE.LAST
     *               .AND. MOD(IAB-1,NPROC).EQ.ME)
     *               VAOAO(ICD,(IABDUM-1)/NPROC+1)=VERI
C                IF(ICD.GE.INIT .AND. ICD.LE.LAST)
C     *               VAOAO(IAB,ICDDUM)=VERI
                IF(ICD.GE.INIT .AND. ICD.LE.LAST
     *               .AND. MOD(ICD-1,NPROC).EQ.ME)
     *               VAOAO(IAB,(ICDDUM-1)/NPROC+1)=VERI
             END DO
          END IF
          IF(NX.GT.0) GO TO 200
C
C**** WRITE VAOAO *************************************************
        DO IAB=INIT,LAST
           IABDUM=IAB-INITM1
           IF (MOD(IAB-1,NPROC).EQ.ME) THEN
              IABDUM=(IABDUM-1)/NPROC+1
              N=0
              DO ICD=1,NGOIJ
                 V=VAOAO(ICD,IABDUM)
                 IF(ABS(V).GE.THRERI) THEN
                    N=N+1
                    LABEL(N)=ICD
                    WORK (N)=V
                 END IF
              END DO
              IF(IAB.EQ.NGOIJ) KCONT=0
              WRITE(LUNOER) KCONT,IAB,N
              CALL MQGENW(LUNOER,N,N,LABEL,WORK)
           END IF
        END DO
C
C        INIT=INIT+IWIDTH
        INIT=INIT+IWIDTH*NPROC
      IF(INIT.LE.NGOIJ) GO TO 100
C**** RETURN ***********************************************************
      WRITE(LUNOER) IZERO,IZERO,IZERO
      OERFLG=.TRUE.
C
C
      ELSE
C -------------------------------------------------------  ITRFAO != 1
C
      IF (IDMWTH.GT.0) THEN
C --------------------------------------------------  IDMWTH > 0 -----
         IF (IDMWTH.NE.IWIDTH) THEN
            IF (MASWRK) WRITE(LUNOUT,*)
     *           'MQORD0: ERROR, IDMWTH SHOULD BE EQUAL IWIDTH'
            CALL ABRT
         END IF
         CALL DDI_DISTRIB(DAOAO,MASTER,ILOC,IHIC,JLOC,JHIC)
         IDMWHP=(JHIC-JLOC)+1
         CALL DDI_DISTRIB(DAOAO,ME,ILOC,IHIC,JLOC,JHIC)
C
         KCONT=1
         INIT=1
 500     CONTINUE
           INITM1=INIT-1
           LAST=INITM1+IWIDTH
           IF(LAST.GT.NGOIJ) LAST=NGOIJ
C**** READ ERI VALUES FROM HONDO FILE **********************************
           CALL VCLR(VAOAO,1,NGOIJ*IWIDTH)
           CALL DDI_PUT(DAOAO,1,NGOIJ,JLOC,JHIC,VAOAO)
           CALL DDI_SYNC(2531)
C
           CALL SEQREW(LUNERI)
           NREAD=NREAD+1
C
 600       CONTINUE
             CALL PREAD(LUNERI,XWORK,IX,NX,NINTMX)
             IF (NX.EQ.0) GO TO 620
             MX=IABS(NX)
             IF(MX.GT.NINTMX) CALL ABRT
C
             DO IERI=1,MX
               VERI=XWORK(IERI)
C
               NPACK = IERI
               IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                 LABLE = IX( 2*NPACK - 1 )
                 IPACK = ISHFT( LABLE, -16 )
                 JPACK = IAND(  LABLE, 65535 )
                 LABLE = IX( 2*NPACK     )
                 KPACK = ISHFT( LABLE, -16 )
                 LPACK = IAND(  LABLE, 65535 )
#endif
#if defined(SPEC_CPU_ILP64)
                 LABLE = IX(NPACK)
                 IPACK = ISHFT( LABLE, -48 )
                 JPACK = IAND( ISHFT( LABLE, -32 ), 65535 )
                 KPACK = IAND( ISHFT( LABLE, -16 ), 65535 )
                 LPACK = IAND( LABLE, 65535 )
#endif
               ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                 LABLE = IX(NPACK)
                 IPACK = ISHFT( LABLE, -24 )
                 JPACK = IAND( ISHFT( LABLE, -16 ), 255 )
                 KPACK = IAND( ISHFT( LABLE,  -8 ), 255 )
                 LPACK = IAND( LABLE, 255 )
#endif
#if defined(SPEC_CPU_ILP64)
                 IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                   LABLE = IX( NPACK/2 )
                   IPACK = IAND( ISHFT( LABLE, -24 ), 255 )
                   JPACK = IAND( ISHFT( LABLE, -16 ), 255 )
                   KPACK = IAND( ISHFT( LABLE,  -8 ), 255 )
                   LPACK = IAND( LABLE, 255 )
                 ELSE
                   LABLE = IX( (NPACK/2)+1 )
                   IPACK = ISHFT( LABLE, -56 )
                   JPACK = IAND( ISHFT( LABLE, -48 ), 255 )
                   KPACK = IAND( ISHFT( LABLE, -40 ), 255 )
                   LPACK = IAND( ISHFT( LABLE, -32 ), 255 )
                 END IF
#endif
               END IF
               IA = IPACK
               IB = JPACK
               IC = KPACK
               ID = LPACK
C
               IAB              =LIJGO(IB,IA)
               ICD              =LIJGO(ID,IC)
               IF(IA.EQ.IB)   VERI=2.0D+00*VERI
               IF(IC.EQ.ID)   VERI=2.0D+00*VERI
               IF(IAB.EQ.ICD) VERI=2.0D+00*VERI
               IF (IAB.GE.INIT .AND. IAB.LE.LAST) THEN
                  IABDUM=IAB-INITM1
                  IABDUM=(IABDUM-1)/NPROC+MOD(IABDUM-1,NPROC)*IDMWHP+1
                  VAOAO(ICD,IABDUM)=VERI
               END IF
               IF (ICD.GE.INIT .AND. ICD.LE.LAST) THEN
                  ICDDUM=ICD-INITM1
                  ICDDUM=(ICDDUM-1)/NPROC+MOD(ICDDUM-1,NPROC)*IDMWHP+1
                  VAOAO(IAB,ICDDUM)=VERI
               END IF
             END DO
C
           IF (NX.GT.0) GO TO 600
C
  620      CONTINUE
C
           CALL DDI_ACC(DAOAO,1,NGOIJ,1,IWIDTH,VAOAO)
C
C**** WRITE VAOAO *************************************************
           CALL DDI_SYNC(2532)
           CALL DDI_GET(DAOAO,1,NGOIJ,JLOC,JHIC,VAOAO)
           IABDUM=0
           DO IAB=INIT,LAST
              IF (MOD(IAB-1,NPROC).EQ.ME) THEN
C                 IABDUM=IAB-INIT+1
C                 IABDUM=(IABDUM-1)/NPROC+MOD(IABDUM-1,NPROC)*IDMWHP+1
C                 CALL DDI_GET(DAOAO,1,NGOIJ,IABDUM,IABDUM,VAOAO)
                 IABDUM=IABDUM+1
                 N=0
                 DO ICD=1,NGOIJ
C                    V=VAOAO(ICD,1)
                    V=VAOAO(ICD,IABDUM)
                    IF(ABS(V).GE.THRERI) THEN
                       N=N+1
                       LABEL(N)=ICD
                       WORK (N)=V
                    END IF
                 END DO
                 IF(IAB.EQ.NGOIJ) KCONT=0
                 WRITE(LUNOER) KCONT,IAB,N
                 CALL MQGENW(LUNOER,N,N,LABEL,WORK)
              END IF
           END DO
           CALL DDI_SYNC(2533)
C
           INIT=INIT+IWIDTH
         IF (INIT.LE.NGOIJ) GO TO 500
C**** RETURN ***********************************************************
      WRITE(LUNOER) IZERO,IZERO,IZERO
      OERFLG=.TRUE.
C
C
      ELSE
C --------------------------------------------------  IDMWTH <= 0 ----
C**** START ************************************************************
      KCONT=1
      INIT=1
  700 CONTINUE
        INITM1=INIT-1
C        LAST=INITM1+IWIDTH
        LAST=INITM1+IWIDTH*NPROC
        IF(LAST.GT.NGOIJ) LAST=NGOIJ
C**** READ ERI VALUES FROM HONDO FILE **********************************
C        DO J=1,LAST-INIT+1
        CALL VCLR(VAOAO,1,NGOIJ*IWIDTH)
        DO KAP=0,NPROC-1
            JFLG(KAP)=1
        END DO
        IFLG=1
        KAP=-1
C
        NX=0
C
        CALL SEQREW(LUNERI)
        NREAD=NREAD+1
  800   CONTINUE
          KAP = MOD(KAP+1,NPROC)
          IF(KAP.EQ.MASTER) THEN
            IF(IFLG.EQ.0) GOTO 890
            IFLG= 0
          END IF
          LFSEND=0
          IF (JFLG(KAP).GT.0) THEN
C
          IF (KAP.EQ.ME) THEN
             CALL PREAD(LUNERI,XWORK,IX,NX,NINTMX)
             IF(NX.NE.0) THEN
                MX=IABS(NX)
                IF(MX.GT.NINTMX) CALL ABRT
                DO IERI=1,MX
                   VERI=XWORK(IERI)
C
                       NPACK = IERI
                       IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                         LABLE = IX( 2*NPACK - 1 )
                         IPACK = ISHFT( LABLE, -16 )
                         JPACK = IAND(  LABLE, 65535 )
                         LABLE = IX( 2*NPACK     )
                         KPACK = ISHFT( LABLE, -16 )
                         LPACK = IAND(  LABLE, 65535 )
#endif
#if defined(SPEC_CPU_ILP64)
                         LABLE = IX(NPACK)
                         IPACK = ISHFT( LABLE, -48 )
                         JPACK = IAND( ISHFT( LABLE, -32 ), 65535 )
                         KPACK = IAND( ISHFT( LABLE, -16 ), 65535 )
                         LPACK = IAND( LABLE, 65535 )
#endif
                       ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                         LABLE = IX(NPACK)
                         IPACK = ISHFT( LABLE, -24 )
                         JPACK = IAND( ISHFT( LABLE, -16 ), 255 )
                         KPACK = IAND( ISHFT( LABLE,  -8 ), 255 )
                         LPACK = IAND( LABLE, 255 )
#endif
#if defined(SPEC_CPU_ILP64)
                         IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                           LABLE = IX( NPACK/2 )
                           IPACK = IAND( ISHFT( LABLE, -24 ), 255 )
                           JPACK = IAND( ISHFT( LABLE, -16 ), 255 )
                           KPACK = IAND( ISHFT( LABLE,  -8 ), 255 )
                           LPACK = IAND( LABLE, 255 )
                         ELSE
                           LABLE = IX( (NPACK/2)+1 )
                           IPACK = ISHFT( LABLE, -56 )
                           JPACK = IAND( ISHFT( LABLE, -48 ), 255 )
                           KPACK = IAND( ISHFT( LABLE, -40 ), 255 )
                           LPACK = IAND( ISHFT( LABLE, -32 ), 255 )
                         END IF
#endif
                       END IF
                       IA = IPACK
                       IB = JPACK
                       IC = KPACK
                       ID = LPACK
C
                   IAB              =LIJGO(IB,IA)
                   ICD              =LIJGO(ID,IC)
                   IF (IAB.GE.INIT .AND. IAB.LE.LAST) LFSEND=1
                   IF (ICD.GE.INIT .AND. ICD.LE.LAST) LFSEND=1
                END DO
             END IF
C
          ENDIF
C
          CALL DDI_BCAST(2531,'I',LFSEND,1,KAP)
          CALL DDI_BCAST(2532,'I',NX,1,KAP)
          JFLG(KAP)=NX
          IF (NX.GT.0) IFLG=1
C
C          IF(MASWRK) WRITE(LUNOUT,*) IFLG,KAP,NX
C
          IF (NX.NE.0) THEN
             IF (LFSEND.EQ.1) THEN
                MX=IABS(NX)
                IF (LABSIZ.EQ.1) THEN
                   IF(NWDVAR.EQ.2) MX2 = MX
                   IF(NWDVAR.EQ.1) MX2 = (MX+1)/2
                ELSE IF (LABSIZ.EQ.2) THEN
                   IF(NWDVAR.EQ.2) MX2 = MX*2
                   IF(NWDVAR.EQ.1) MX2 = MX
                END IF
C
                CALL DDI_BCAST(2535,'F',XWORK,MX,KAP)
                CALL DDI_BCAST(2534,'I',IX,MX2,KAP)
C                CALL DDI_BCAST(2534,'F',IX,MX,KAP)
C
                DO IERI=1,MX
                   VERI=XWORK(IERI)
C
                       NPACK = IERI
                       IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                         LABLE = IX( 2*NPACK - 1 )
                         IPACK = ISHFT( LABLE, -16 )
                         JPACK = IAND(  LABLE, 65535 )
                         LABLE = IX( 2*NPACK     )
                         KPACK = ISHFT( LABLE, -16 )
                         LPACK = IAND(  LABLE, 65535 )
#endif
#if defined(SPEC_CPU_ILP64)
                         LABLE = IX(NPACK)
                         IPACK = ISHFT( LABLE, -48 )
                         JPACK = IAND( ISHFT( LABLE, -32 ), 65535 )
                         KPACK = IAND( ISHFT( LABLE, -16 ), 65535 )
                         LPACK = IAND( LABLE, 65535 )
#endif
                       ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                         LABLE = IX(NPACK)
                         IPACK = ISHFT( LABLE, -24 )
                         JPACK = IAND( ISHFT( LABLE, -16 ), 255 )
                         KPACK = IAND( ISHFT( LABLE,  -8 ), 255 )
                         LPACK = IAND( LABLE, 255 )
#endif
#if defined(SPEC_CPU_ILP64)
                         IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                           LABLE = IX( NPACK/2 )
                           IPACK = IAND( ISHFT( LABLE, -24 ), 255 )
                           JPACK = IAND( ISHFT( LABLE, -16 ), 255 )
                           KPACK = IAND( ISHFT( LABLE,  -8 ), 255 )
                           LPACK = IAND( LABLE, 255 )
                         ELSE
                           LABLE = IX( (NPACK/2)+1 )
                           IPACK = ISHFT( LABLE, -56 )
                           JPACK = IAND( ISHFT( LABLE, -48 ), 255 )
                           KPACK = IAND( ISHFT( LABLE, -40 ), 255 )
                           LPACK = IAND( ISHFT( LABLE, -32 ), 255 )
                         END IF
#endif
                       END IF
                       IA = IPACK
                       IB = JPACK
                       IC = KPACK
                       ID = LPACK
C
                   IAB              =LIJGO(IB,IA)
                   ICD              =LIJGO(ID,IC)
                   IF(IA.EQ.IB)   VERI=2.0D+00*VERI
                   IF(IC.EQ.ID)   VERI=2.0D+00*VERI
                   IF(IAB.EQ.ICD) VERI=2.0D+00*VERI
                   IABDUM=IAB-INITM1
                   ICDDUM=ICD-INITM1
                   IF(IAB.GE.INIT .AND. IAB.LE.LAST
     *                  .AND. MOD(IAB-1,NPROC).EQ.ME)
     *                  VAOAO(ICD,(IABDUM-1)/NPROC+1)=VERI
                   IF(ICD.GE.INIT .AND. ICD.LE.LAST
     *                  .AND. MOD(ICD-1,NPROC).EQ.ME)
     *                  VAOAO(IAB,(ICDDUM-1)/NPROC+1)=VERI
                END DO
C
             END IF
          END IF
C
          END IF
C
          GO TO 800
C          IF (NX.GT.0) GO TO 800
  890   CONTINUE
C
C**** WRITE VAOAO *************************************************
        DO IAB=INIT,LAST
           IABDUM=IAB-INITM1
           IF (MOD(IAB-1,NPROC).EQ.ME) THEN
              IABDUM=(IABDUM-1)/NPROC+1
              N=0
              DO ICD=1,NGOIJ
                 V=VAOAO(ICD,IABDUM)
                 IF(ABS(V).GE.THRERI) THEN
                    N=N+1
                    LABEL(N)=ICD
                    WORK (N)=V
                 END IF
              END DO
              IF(IAB.EQ.NGOIJ) KCONT=0
              WRITE(LUNOER) KCONT,IAB,N
              CALL MQGENW(LUNOER,N,N,LABEL,WORK)
           END IF
        END DO
C
C        INIT=INIT+IWIDTH
        INIT=INIT+IWIDTH*NPROC
      IF(INIT.LE.NGOIJ) GO TO 700
C**** RETURN ***********************************************************
      WRITE(LUNOER) IZERO,IZERO,IZERO
      OERFLG=.TRUE.
C
      END IF
C --------------------------------------------------  END OF IDMWTH ----
      END IF
C -------------------------------------------------------  END OF ITRFAO
C
      RETURN
      END
