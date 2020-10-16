C  9 dec 03 - mws - synch common block runopt
C  3 Sep 03 - MWS - include changes from 10-Nov-01 to 23-Jul-02 below
C 23 Jul 02 - DGF - take full advantage of Lz values.
C  6 Jun 02 - DGF - implement form factor method for SO-MCQDPT
C 20 May 02 - DGF - add user-guided Lz symmetry for linear molecules
C 25 Feb 02 - DGF - use energy state (pseudo)degeneracy in diagram loops
C 14 Feb 02 - DGF - use point group symmetry in some SO-MCQDPT diagrams
C 02 Feb 02 - DGF - introduce user-guided L**2 symmetry for atoms
C 09 Jan 02 - DGF - allow several SO ISF parameters in one batch
C 23 Nov 01 - TN  - changes for DK
C 10 Nov 01 - DGF - implement MCP 2e core-active SOC integrals
C 25 Oct 01 - HW,DGF - include spin-orbit ISF (aka ISA) MCQDPT technique
C 19 Sep 01 - MWS - convert mxaoci paramter to mxao
C  1 Aug 01 - DGF - SOZEFX: don't reuse ints, SPNORY: energy zero
C 25 Jun 01 - MWS - SOZEFX: mplevl taken from common block wfnopt
C 13 Jun 01 - DGF - use second quantization to avoid calculating ms=+1
C 11 Oct 00 - DGF - SOZEFX: patch for label size on 64 bit machines
C 15 Aug 00 - DGF - SPNDIA: save energies for transition moments
C 11 Jun 00 - DGF - SPNORY: adjust mxspin for single reference CI
C 25 Mar 00 - DGF - synchronize relwfn common
C 16 FEB 00 - JAB - REWIND AFTER OPEN 51/52/53, AVOID ZERO LENGTH READS
C 10 Jan 00 - MWS - synchronize relwfn common
C 21 Dec 99 - DGF - include changes 7/jan/98 to 1/nov/99 in online code.
C  1 Nov 99 - DGF,tn - add 1 and 2e RESC SOC corrections
C 14 May 99 - DGF - let the user tell if the symmetry in $DRT=$DATA
C  9 May 99 - DGF - divide large 2e MO array into drags
C  7 May 99 - DGF - distribute CSF arrays over nodes
C  2 May 99 - DGF - allow to duplicate/distribute SOC 2e integrals
C 29 Apr 99 - DGF - use index perm. for 2e ints, out of core 4-ind trans
C 25 Apr 99 - DGF - allow for same multiplicities in different $CIDRTs
C 22 Apr 99 - DGF - fix GUGA irrep for two $VECs, allow reading SOC ints
C  9 Feb 99 - DGF - add Jz eigenvalues calculation for linear molecules
C 20 Dec 98 - DGF - systematise the tolerance for SOC
C 18 Dec 98 - DGF - sum the coupling constants over degenerate states
C 14 Dec 98 - DGF - defracture the 2e AO array into blocks
C  1 Dec 98 - DGF - move all SO int needs to BRTHSO (remove old driver)
C 24 Nov 98 - DGF - introduce partial Pauli-Breit method ZEFF3
C 27 Oct 98 - DGF - implement direct Pauli-Breit method ZEFF2
C 20 Jul 98 - DGF - optimise CSFREO
C 12 Jul 98 - DGF - stop calculating matrix elements ms=-1
C  7 Jul 98 - DGF - add noirr to gugwfn and detwfn
C 24 Jun 98 - DGF - fix various TMOMNT bugs (SPNTMX)
C 16 May 98 - DGF - parallelise Zeff, trash redundant code
C 14 May 98 - DGF - remarry the transition moment codes
C 29 Apr 98 - DGF - add dipole momnt/remove double group selection rules
C 17 Jan 98 - DGF - remove mxspin and mxprm, some cosmetic changes
C                   use spin-rotational symmetry to get matrix elements
C  7 Jan 98 - DGF - rid spntrn of the bothersome arrays
C  6 Jan 98 - DGF - merge SK's 95-97 changes into the current gamess
C 28 SEP 97 - MWS - CONVERT PARALLEL CALLS FROM TCGMSG TO DDI
C 18 Jul 96 - SK  - Minor changes.
C 16 May 96 - SK  - Set energy tolerance for Hso matrix.
C 17 Feb 96 - SK  - Found bugs.  Need to find them.
C 12 Feb 96 - SK  - Clean up comments and formats.
C  3 Feb 96 - SK  - Use one file to save CI information.
C 31 Jan 96 - SK  - Expand spin-multiplicity capability.
C                   Change PARAMETER to (MXSPIN=20), delete MXOPEN.
C                   Use SAVCIx, instead of SAVCIM.
C 13 Sep 95 - SK  - Calculate weight of spin-multiplicity
C 17 Mar 95 - SK  - Reconstruct routines of transition-moment calcs
C  8 Mar 95 - SK  - Add transition-moment routines for spin-mixed states
C  7 Mar 95 - SK  - Introduce SPNORX, instead of TRNSTX
C 22 JAN 95 - SK  - REMOVE BUGS
C 29 DEC 94 - SK  - ADD NEW ROUTINES (SPNCHK)
C 17 OCT 94 - SK  - ADD PARAMETER (MXSPIN=10, MXSPN2=MXSPIN*MXSPIN)
C 10 SEP 94 - SK  - ADD F FUNCTIONS.  START WORKING IN VERSION 04.
C 30 JUN 94 - SK  - NEW ROUTINE TO DETERMINE THE SPIN OF FSS (SOC)
C                   EMPLOY THE COPY OF GUGDRT.
C 12 NOV 94 - MWS - REMOVE FTNCHEK WARNINGS
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C  1 JUN 94 - MWS - SOLOOP: ALL NODES DO CSF FILE READS
C 27 APR 92 - SK  - SOLOOP: PRINT REAL AND IMAGINARY HSO ELEMENTS
C 12 MAR 92 - MWS - REDIMENSION TO 500 ATOMS
C 11 JAN 92 - TLW - SOLOOP: MAKE READS PARALLEL
C 10 JAN 92 - TLW - CHANGE REWIND TO CALL SEQREW
C  7 JAN 92 - TLW - MAKE WRITES PARALLEL; ADD COMMON PAR
C  7 AUG 90 - TLW - ADD CF AND CG TO COMMON NSHEL
C 23 JAN 90 - MWS - ADAPT SHIRO KOSEKI'S S.O.C INTO CURRENT GAMESS
C
C*MODULE SOZEFF  *DECK SOZEFX
      SUBROUTINE SOZEFX(NFT17,PRTPRM,ADD2E,ZEFF,TMOMNT,SKIPDM,NZSPIN,
     *                  MAXL,NFOCI,ETOL,DEIG,ENGYST,EREF0,EREFCAS,HMP2,
     *                  NWKSST,MULST,IROOTS,IVCORB,IVEX,ISTSYM,IRCIOR,
     *                  IRRL,IRRR,GSYLYES,NOSYM,MAXNCO,IPRHSO,HSOTOL,
     *                  JZOPT,MODPAR,LAMECSF,MINMO,NOSYMTRZE,MP2SO,
     *                  NMOFZC,NMODOC,NMOACT,NMOEXT,MCP2E,IONECNT,NISFSO
     *                 ,PISFSO,L2VAL,EEQTOL,NFFBUF)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500,MXSH=1000)
C
      DIMENSION ZEFF(MXATM),DEIG(*),ENGYST(MXRT,NUMCI),EREF0(MXRT,NUMCI)
     *         ,EREFCAS(MXRT,NUMCI),HMP2(MXRT,MXRT,NUMCI),NWKSST(NUMCI),
     *          MULST(NUMCI),IVCORB(NUM,NUMCI),IROOTS(NUMCI),IVEX(NUMCI)
     *         ,ISTSYM(MXRT,NUMCI),IRCIOR(MXRT,NUMCI),NFT2SO(3),IRRL(3),
     *          IRRR(3),N2INT(3),PISFSO(*),L2VAL(NUMCI)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,PRTPRM,ADD2E,TMOMNT,SKIPDM,GSYLYES(3)
     *       ,DISTINT
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCKLAB/ LABSIZ
      COMMON /RELWFN/ RMETHOD,QRQMT,CLIG,CLIG2,QRTOL,IQRORD,MODQR,NESOC,
     *                NRATOM,NUMU,NQMTR,NQRDAF,MORDA,NDARELB
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                T(432),INVT(48),NT
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RESC/8HRESC    /,N2INT/0,0,0/
#else
      CHARACTER*8 :: RESC_STR
      EQUIVALENCE (RESC, RESC_STR)
      DATA RESC_STR/"RESC    "/,N2INT/0,0,0/
#endif
C
C     ----- MAIN DRIVER FOR SPIN-ORBIT COUPLING -----
C
C     THIS MODULE WAS WRITTEN BY
C        DR. SHIRO KOSEKI
C        DEPARTMENT OF CHEMISTRY
C        NORTH DAKOTA STATE UNIVERSITY
C        FARGO N.D. 58105
C     S.O. COUPLING ADDED BY SHIRO MAY-JULY 1988.
C     modified by Dmitri Fedorov (10 BC - 1999 AD)
C
C        THE S.O. COUPLING MAY BE OBTAINED BETWEEN ANY TWO
C        CI STATES GAMESS CAN COMPUTE, USING A COMMON SET OF MO-S.
C
C        THE S.O. COUPLING MAY ALSO BE OBTAINED FOR TWO
C        CI STATES, COMPUTED IN DIFFERENT ORBITALS, IF 1) THE
C        TWO STATES SHARE A COMMON SET OF FROZEN CORE ORBITALS
C        AND 2) IF A COMPLETE ACTIVE SPACE CI IS BEING USED.
C        A REFERENCE FOR THE LATTER OPTION IS
C           B.H.LENGSFIELD III, J.J.JAFRI, D.H.PHILLIPS,
C           C.W.BAUSCHLICHER, JR.  J.CHEM.PHYS. 74,6849-6856(1981)
C
C     DEBUG FOR PARTICULAR ROUTINES IS AVAILABLE BY SETTING
C          EXETYP=TRNCMO  (CORRESPONDING ORBITALS)
C                =SAVCIV  (PRESERVING CSF DATA)
C                =DIPVEL  (DIPOLE VELOCITY INTEGRALS)
C                =SOINT   (S.O. INTegralS)
C                =sozeff  (COMPUTATION OF THE S.O. COUPLING)
C
C     ----- Check the energy tolerance -----
C     find the lowest state:
      ELOW = 1.0D+99
      DO 600 I=1,NUMCI
         DO 600 J=1,IROOTS(I)
            IF(ENGYST(J,I).LT.ELOW)  ELOW = ENGYST(J,I)
  600       CONTINUE
      EMAX = ELOW + ETOL
      DO 610 I=1,NUMCI
         DO 620 J=1,IROOTS(I)
            IF(ENGYST(J,I).GT.EMAX) GO TO 622
  620       CONTINUE
         J = IROOTS(I) +1
  622    CONTINUE
C        nstates(i) = j -1
         IF(J-1.NE.IROOTS(I)) WRITE(IW,9000) I,IROOTS(I),J-1
         IROOTS(I) = J -1
  610    CONTINUE
      NOSYM1=NOSYM
      IF(NT.EQ.1.AND.NOSYM.EQ.0) NOSYM1=1
C
C     At a place of critical importance it will not be fiddled with
C     C1 group, nor with allocating huge arrays for symmetry
C
C     labsiz is used in a misunderstood sense in SOC code.
C     It is taken to mean the number of integers to store 4 indices
C     whereas elsewhere it means the number of bytes to store 1 index.
C     The two definitions overlap but the disaster occurs on 64 bit with
C     >256 basis functions. The fix is below, but the memory is not
C     used most efficiently in case of 64 bit machines with <=256 basis
C     functions (in fact, 8+8 bytes used per integral instead of 8+4).
C
      LABSAV=LABSIZ
      IF(NWDVAR.EQ.1) LABSIZ=1
C
C     ----- COMPUTE 1 E- SPIN ORBIT INTEGRALS ----
C
      NCSF  = NWKSST(1)
      DO 20 ICI=2,NUMCI
   20   NCSF = MAX(NCSF,NWKSST(ICI))
      L1=NUM
      L2=(NUM*NUM+NUM)/2
      N1 = NOCC - NFZC
      N3 = N1*N1
      NEC=0
      IF(GSYLYES(1)) NEC=1
      IF(GSYLYES(2)) NEC=NEC+1
      IF(GSYLYES(3)) NEC=NEC+1
      IF(NEC.LE.1) THEN
         NPASS=3
      ELSE IF(.NOT.GSYLYES(3)) THEN
         NPASS=2
      ELSE
         NPASS=1
      ENDIF
      IF(MAXNCO.NE.2) NPASS=1
C
C     a quite fine point about npass with some symmetry present:
C     for skipdm=.f. runs it is difficult to predict whether npass=1
C     or npass=2 will be faster. Both generate corect numbers,
C     but npass=1 works on all three Ms,Ms' states/matrix elements
C     in a single run, whereas
C     npass=2 spends one pass for dipole moment and one pass for SOC
C     Currently, npass=2 is being used which appears to be faster.
C
      IF(NEC.EQ.0.AND.NOSYM.EQ.0) THEN
         IF(MASWRK) WRITE(IW,9300)
         LABSIZ = LABSAV
         RETURN
      ENDIF
      INTREST=0
      IF(MAXNCO.GT.1.AND.MCP2E.NE.1) THEN
C
C        this also prepares the backdoor for ECP cheating by
C        precalculating the integrals in the all-electron basis set
C        and feeding these into ECPSOC
C
         NFT2SO(1)=31
         NFT2SO(2)=32
         NFT2SO(3)=33
         NFTRESC=34
         CALL SEQOPN(NFT2SO(1),'SOINTX','UNKNOWN',.FALSE.,'UNFORMATTED')
         CALL SEQOPN(NFT2SO(2),'SOINTY','UNKNOWN',.FALSE.,'UNFORMATTED')
         CALL SEQOPN(NFT2SO(3),'SOINTZ','UNKNOWN',.FALSE.,'UNFORMATTED')
         IF(RMETHOD.EQ.RESC.AND.NESOC.GT.1)
     *   CALL SEQOPN(NFTRESC,'SORESC','UNKNOWN',.FALSE.,'UNFORMATTED')
C
C        read info for restart check
C
         ENUCR  = ENUC(NAT,ZAN,C)
         NEEDY=0
         DO 606 I=1,3
            IF(.NOT.GSYLYES(I)) GOTO 606
            NEEDY=NEEDY+1
            READ(NFT2SO(I),END=604,ERR=604)
     *           NWDVAR1,LABSIZ1,L21,N2INT(I),IONECNT1,ENUCR1
C           we relax the requirement that the geometry be the same
C           if one-centre approximation is used.
C           We also relax mixing one-centre approximation consistency.
C             .AND.ionecnt.eq.ionecnt1.and.
            IF(NWDVAR.EQ.NWDVAR1.AND.LABSIZ.EQ.LABSIZ1.AND.L2.EQ.L21
     *         .AND.(ABS(ENUCR-ENUCR1).LT.1.0D-10.OR.IONECNT.GT.0))
     *           INTREST=INTREST+1
            IF(IONECNT.NE.IONECNT1.AND.MASWRK) WRITE(IW,*)
     *         'DIFFERENT ONE-CENTRE APPROXIMATION:',IONECNT,IONECNT1
C           GO TO 606
  604       CONTINUE
            CALL SEQREW(NFT2SO(I))
  606    CONTINUE
         IF(INTREST.EQ.NEEDY.AND.MASWRK) WRITE(IW,9100)
         IF(INTREST.NE.NEEDY) INTREST=0
      ENDIF
      NRES=MAX(L1*L1*L1*N1+N3*N3+L1*L2,2*N1*NCSF)
      IF(MAXNCO.EQ.3) NRES=MAX(L1*L2,2*N1*NCSF)
      MAXIA=MAX(MXSH,NUM)
      DISTINT=MOD(MODPAR,4)/2.EQ.1
      MCPREC=396
      MAXLI=MAXL
      IF(MCP2E.NE.0) THEN
         CALL MPBASCHK(MAXLC)
         MAXLI=MAX(MAXLI,MAXLC)
C        write(6,*) 'debug: maxl',maxl,' maxlc',maxlc,' maxli',maxli
      ENDIF
      CALL SOINTS(MAXLI,L1,L2,ZEFF,83,MCPREC,N2INT,NSO2BUF,MAXNCO,NRES,
     *            NFT2SO,MAXIA,INTREST,ENUCR,GSYLYES,DISTINT,MCP2E,
     *            IONECNT,.FALSE.)
      IF (MASWRK) WRITE(IW,9200)
      CALL TIMIT(1)
C
C     ----- COMPUTE THE SPIN-ORBIT COUPLING -----
C
      CALL SPNORY(NFT17,EXETYP,PRTPRM,ADD2E,TMOMNT,SKIPDM,NZSPIN,MAXL,
     *            NFOCI,ENGYST,EREF0,EREFCAS,HMP2,NWKSST,MULST,IROOTS,
     *            IVCORB,DEIG,IVEX,ISTSYM,IRCIOR,IRRL,IRRR,NOSYM,NOSYM1,
     *            MAXNCO,NCSF,N2INT,NSO2BUF,NFT2SO,NFTRESC,IPRHSO,HSOTOL
     *           ,JZOPT,NPASS,DISTINT,LAMECSF,MINMO,MPLEVL,NMOACT,NMODOC
     *           ,NMOEXT,NMOFZC,NOSYMTRZE,MP2SO,MCP2E,MCPREC,NISFSO,
     *            PISFSO,L2VAL,EEQTOL,NFFBUF)
Cdg  *           ,ndim,NIJDR,ijdrep)
      IF(MAXNCO.GT.1.AND.MCP2E.NE.1) THEN
         CALL SEQCLO(NFT2SO(3),'KEEP')
         CALL SEQCLO(NFT2SO(2),'KEEP')
         CALL SEQCLO(NFT2SO(1),'KEEP')
         IF(RMETHOD.EQ.RESC.AND.NESOC.GT.1) CALL SEQCLO(NFTRESC,'KEEP')
      ENDIF
C
      LABSIZ=LABSAV
      RETURN
C
 9000 FORMAT(/1X,'ETOL CAUSES IROOTS(',I2,') TO BE CHANGED FROM ',I2,
     *       ' TO ',I2)
 9100 FORMAT(/1X,'2E INTEGRALS WILL BE READ FROM THE DISK',/)
 9200 FORMAT(6X,'...... DONE WITH SPIN-ORBIT INTEGRALS ......')
 9300 FORMAT(/1X,'SPIN-ORBIT COUPLING IS FORBIDDEN BY SYMMETRY.',/,
     *       'IF YOU PERSIST, SET NOSYM=1 OR 2.',/)
      END
C*MODULE SOZEFF  *DECK SPNORY
      SUBROUTINE SPNORY(NFT17,EXETYP,PRTPRM,ADD2E,TMOMNT,SKIPDM,NZSPIN,
     *                  MAXL,NFOCI,ENGYST,EREF0,EREFCAS,HMP2,NWKSST,
     *                  MULST,IROOTS,IVCORB,DEIG,IVEX,ISTSYM,IRCIOR,IRRL
     *                 ,IRRR,NOSYM,NOSYM1,MAXNCO,NCSF,N2INT,NSO2BUF,
     *                  NFT2SO,NFTRESC,IPRHSO,HSOTOL,JZOPT,NPASS,DISTINT
     *                 ,LAMECSF,MINMO,MPLEVL,NMOACT,NMODOC,NMOEXT,NMOFZC
     *                 ,NOSYMTRZE,MP2SO,MCP2E,MCPREC,NISFSO,PISFSO,L2VAL
     *                 ,EEQTOL,NFFBUF2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL DBGINT,DBGSO,DBGSOMP,GOPARR,DSKWRK,MASWRK,PRTPRM,TMOMNT,
     *        SKIPDM,ADD2E,SYLYES(3),SYRYES(3),DODM,DOSOL,SAMEMUL,
     *        DISTINT,TMO2,LINEAR,PRTLZ
Cdg   logical symyes
      CHARACTER*9 ANGL(3)
      CHARACTER*6 GLOLAB,LLET
C
      DIMENSION ITAG(7),ENGYST(MXRT,*),EREF0(MXRT,*),EREFCAS(MXRT,*),
     *         HMP2(MXRT,MXRT,*),NWKSST(*),MULST(*),IROOTS(*),DMCORE(3),
     *          IVCORB(NUM,*),DEIG(*),ISTSYM(MXRT,*),IRCIOR(MXRT,*),
     *          IVEX(*),IRRL(3),IRRR(3),LCMO(2),NFT2SO(3),N2INT(3),
     *          FFAC(0:1,0:1,0:1,0:1),IMS(3),NEEDDC(3),NEED2A(3),
     *          EZ2P(0:1,0:1,0:1,0:1),EZERO(2),PISFSO(*),L2VAL(*)
C
      PARAMETER (MXATM=500, MXAO=2047)
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, HALF=0.5D+00, TWO=2.0D+00)
C
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /GUGWFN/ NFZC,NMCC,NDOC,NAOS,NBOS,NALP,NVAL,NEXT,NFZV,
     *                IFORS,IEXCIT,ICICI,NOIRR
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,MFZC,NUMVEC,ICI,MXRT,NSTAT
      COMMON /RELWFN/ RMETHOD,QRQMT,CLIG,CLIG2,QRTOL,IQRORD,MODQR,NESOC,
     *                NRATOM,NUMU,NQMTR,NQRDAF,MORDA,NDARELB
      COMMON /MQIOFI/ IDAF50,NAV50,IODA50(400)
      COMMON /RESTAR/ TIMLIM,IREST,NREC,INTLOC,IST,JST,KST,LST
      COMMON /ZMAT  / NZMAT,NZVAR,NVAR,NSYMC,LINEAR
      COMMON /XYZPRP/ XP,YP,ZP,
     *                DIPMX,DIPMY,DIPMZ,
     *                QXX,QYY,QZZ,QXY,QXZ,QYZ,
     *                QMXX,QMYY,QMZZ,QMXY,QMXZ,QMYZ,
     *                OXXX,OXXY,OXXZ,OXYY,OYYY,OYYZ,
     *                OXZZ,OYZZ,OZZZ,OXYZ,
     *                OMXXX,OMXXY,OMXXZ,OMXYY,OMYYY,
     *                OMYYZ,OMXZZ,OMYZZ,OMZZZ,OMXYZ
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA   ELMOM/8HELMOM   /,IMS/-1,1,0/
#else
      CHARACTER*8 :: ELMOM_STR
      EQUIVALENCE (ELMOM, ELMOM_STR)
      DATA   ELMOM_STR/"ELMOM   "/,IMS/-1,1,0/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA   ITAG/4H LX ,4H LY ,4H LZ , 4H X  ,4H Y  ,4H Z  ,4HALZ /
#else
      CHARACTER*4 :: ITAG_STR(7)
      EQUIVALENCE (ITAG, ITAG_STR)
      DATA   ITAG_STR/" LX "," LY "," LZ ", " X  "," Y  "," Z  ","ALZ "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA   DEBUG,DBUGI,DBUGS/8HDEBUG   ,8HSOINT   ,8HSOZEFF  /,
     *       DBUGP/8HSOMCQDPT/,ANGL/'REQUIRED','REQUESTED','REQUESTED'/,
     *       RESC/8HRESC    /,DK/8HDK      /
#else
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGI_STR
      EQUIVALENCE (DBUGI, DBUGI_STR)
      CHARACTER*8 :: DBUGS_STR
      EQUIVALENCE (DBUGS, DBUGS_STR)
      CHARACTER*8 :: DBUGP_STR
      EQUIVALENCE (DBUGP, DBUGP_STR)
      CHARACTER*8 :: RESC_STR
      EQUIVALENCE (RESC, RESC_STR)
      CHARACTER*8 :: DK_STR
      EQUIVALENCE (DK, DK_STR)
      DATA   DEBUG_STR,DBUGI_STR,DBUGS_STR/"DEBUG   ","SOINT   ",
     *       "SOZEFF  "/,
     *       DBUGP_STR/"SOMCQDPT"/,ANGL/'REQUIRED','REQUESTED',
     *       'REQUESTED'/,
     *       RESC_STR/"RESC    "/,DK_STR/"DK      "/
#endif
C
C     ----- CALCULATE SPIN-ORBIT COUPLING -----
C     THIS CODE WRITTEN MAY-JULY 1988 BY SHIRO KOSEKI AT NDSU.
C               modified 1941-1999 by Dmitri Fedorov at ISU
C
C     for equal multiplicities S==S' one calculates
C         js==1            js==2
C     <Ms=S||Ms'=S>,                      is==1
C     <Ms=S-1||Ms'=S>                     is==2
C
C     for S=S'-1 one calculates
C         js==1            js==2
C     <Ms=S||Ms'=S+1>, <Ms=S||Ms'=S'>    is==1
C
      DBGINT = (EXETYP.EQ.DEBUG .OR. EXETYP.EQ.DBUGI) .AND. MASWRK
      DBGSO  = (EXETYP.EQ.DEBUG .OR. EXETYP.EQ.DBUGS) .AND. MASWRK
      DBGSOMP= (EXETYP.EQ.DEBUG .OR. EXETYP.EQ.DBUGP) .AND.
     *         MPLEVL.NE.0.AND.MASWRK
C
      NRED=1
C     nred=4
      N0PASS=NPASS
      L1 = NUM
      L2 = (L1*L1+L1)/2
      L3 = L1*L1
C
C     WE WORK ONLY WITH THE ACTIVE ORBITALS
C
      N1 = NOCC - NFZC
      N2 = (N1*N1+N1)/2
      N3 = N1*N1
C
      NMO=NMOFZC+NMODOC+NMOACT+NMOEXT
      NMODA=NMODOC+NMOACT
      NMOEI=NMODA*(NMODA+1)/2+NMOEXT*NMODA
      NMOII=NMODA*(NMODA+1)/2
      IF(MPLEVL.EQ.0) NMO=NOCC
      IF(MPLEVL.EQ.0) NMOFZC=NFZC
      NMO2=NMO*NMO
      IF(MP2SO.EQ.3.AND.NMODOC.EQ.0) MP2SO=1
      NEC=1
      IF(MP2SO.GT.1.AND.NMODOC.NE.NFZC) NEC=2
      IF(MP2SO.EQ.-1) THEN
         NEC=1
         MP2SO=3
      ENDIF
C     if(mp2so.gt.1) nec=2
      MHSO=1
      IF(MP2SO.GT.0) MHSO=2
C     nec gives the number of core densities (1 or 2). They come from
C     NFZC (CI) and NMODOC (MCQDPT) MOs.
C
C     now we get *all* 1e SOC integrals to cooperate with MCQDPT
C
      NAO1 = NDOC + NAOS + NBOS + NALP + NVAL
      NAEL= NDOC*2 + NAOS + NBOS + NALP
      MXSPIN=MIN(NAO1,NAEL)
      IF(IFORS.EQ.0) MXSPIN=MAX(NAO1,NAEL)
      SMALL=SYMTOL/(N1*N1)
      IF(MAXNCO.EQ.2) SMALL=SMALL/2
      SMALL1=SMALL/MXSPIN
C     this small needs to be levelled with small in SAVCIV
      NEEQ=0
C
C     nffbufi1 is the buffer size for one body form factors
C              (hardwired to the max possible number (a road overkill))
C              of the form <I|Ez,pr|J>, where I and J are CSFs.
C              (the buffer is rewritten for each combination I,J)
C     nffbufi2 is the buffer size for two body form factors
C              of the form <I|Ez,pr,rs|J>, where I and J are CSFs
C              (the buffer is rewritten for each combination I,J)
C     nffbuf2 is the buffer size for two body form factors
C              of the form <a|Ez,pr,rs|J>, where a is a CAS state
C              and J is a CSF
C              (the buffer is rewritten for each J)
C
      NACT2=NMOACT*NMOACT
      NFFBUFI1=NACT2
      NFFBUFI2=NACT2
      IF(NFFBUF2.EQ.0) THEN
         NFFBUFI1=0
         NFFBUFI2=0
      ENDIF
      MAXNFF2=0
C
      MXSPIN=MIN(NAO1+NFOCI,NAEL)
C     single reference CI
      IF(IFORS.EQ.0.AND.NFOCI.EQ.0) MXSPIN=NAOS+NBOS+NALP+IEXCIT*2
C     mxspin is the maximum number of singly occupied orbitals
      IF(MXSPIN.GE.64/NWDVAR) THEN
         WRITE(IW,*) 'MXSPIN ',MXSPIN,'.GE.',64/NWDVAR
         CALL ABRT
      ENDIF
      MXPRM=2**MXSPIN
      NHSO = 0
      NTDM=0
      DO 100 ICI=1,NUMCI
         NHSO = NHSO + IROOTS(ICI)*MULST(ICI)
         NTDM=NTDM+IROOTS(ICI)
  100    CONTINUE
C     ntdm=3*NHSO*(NHSO+1)/2
      NCOPCON=MXRT*MXRT*NUMCI*NUMCI
      N1PACK=MIN(16,N1*2)
      N2PACK=N1PACK*2/NWDVAR
      INTPACK=(N1-1)/N2PACK+1
      IF(NOSYM1.GT.0) LAMECSF=1
      IF(MPLEVL.NE.0) LAMECSF=0
C
C     write(iw,*) "attencion:",lamecsf
C     lamecsf - how lame can it get: (=1) do not store individual CSF
C     irred labels, because they are all of the same symmetry. This is
C     in fact normal situation. CSF will *not* be all of the same
C     symmetry if user chooses to lower symmetry in $DRT, but not in
C     $DATA.  If lamecsf!=0 the last CSF will be used to label all.
C     (only the divine utator kenneth the sothe).
C     SO-MCQDPT runs can not run with LAMECSF=1 since all orbital
C     occupations are needed in order to get 0-th order CSF energies.
C     Lame CSF option saves millions of words and kalpas of CPU time.
C
C     ----- GROW MEMORY -----
C
      CALL VALFM(LOADFM)
      NGOT=LIMFM-LOADFM
  105 CONTINUE
      LIVC = LOADFM + 1
      LHSO = LIVC + MXAO*2/NWDVAR +1
      LEIG = LHSO+ NHSO*NHSO*2*MHSO
      LTDM = LEIG+ NHSO*MHSO
      LLZ  = LTDM+ NTDM*NTDM*3
      LSOMO= LLZ + NTDM*NTDM
      IF(JZOPT.EQ.0) LSOMO= LLZ
      LTMMO   = LSOMO  + NMO*NMO*3
C     LZMO = LTMMO  + N3*3
      LZMO = LTMMO  + L3*3
C     LTMMO is used both as N3*3 (normal) and L3*2 (temporary buffer)
      LCMO(1) = LZMO + N3
      IF(JZOPT.EQ.0) LCMO(1) = LZMO
      LCMO(2) = LCMO(1)+ L3
      LSOL1MUL= LCMO(2)+ L3
      LSOL2MUL= LSOL1MUL+ MXRT*MXRT*3*MHSO
      LSTMMUL = LSOL2MUL+ MXRT*MXRT*3
      LLZMUL  = LSTMMUL + MXRT*MXRT*3
      LCOPCON = LLZMUL  + MXRT*MXRT
      IF(JZOPT.EQ.0) LCOPCON = LLZMUL
      LOMEGA  = LCOPCON+ MHSO*NCOPCON
      LAST1   = LOMEGA+NHSO*MHSO
      IF((RMETHOD.EQ.RESC.OR.RMETHOD.EQ.DK).AND.NESOC.GT.1) THEN
C     allocate storage for left/right transformation matrices
         LRESCA= LAST1
         LRESCB= LRESCA + L3
         LLEFTRA = LRESCB + L3
         LRIGHTRA = LLEFTRA + L1*L1
         LAST1=LRIGHTRA + L1*L1
C        the last two are used as l1*nfzc, l1*n1, l1*(nfzc+n1), l1*l1
      ENDIF
      IF(MPLEVL.NE.0) THEN
         LEORB=LAST1
         LVONEEL=LEORB+L1
         LUONEEL=LVONEEL+L3
         LVTWOEL=LUONEEL+L3*3
         LSOL1MPI=LVTWOEL+NMOEI*NMOII
         LSOL1MPJ=LSOL1MPI+MXRT*3
         LLIJMO=LSOL1MPJ+MXRT*3
         LAST1=LLIJMO+(NMO*NMO-1)/NWDVAR+1
      ELSE
         LEORB=LAST1
         LVONEEL=LAST1
         LVTWOEL=LAST1
         LSOL1MPI=LAST1
         LSOL1MPJ=LAST1
         LLIJMO=LAST1
      ENDIF
      LPQ     = LAST1
      LIQ     = LAST1
      LIQ1    = LAST1
C     lpq and liq,liq1 are not used except for maxnco.eq.2
      IF(MAXNCO.GT.1) THEN
         IF(NUMVEC.EQ.1) N23=N2
         IF(NUMVEC.NE.1) N23=N3
         IF(MAXNCO.EQ.2) THEN
            LPQ    = LAST1
            LIQ    = LPQ + N3
            LIQ1   = LIQ + (N3-1)/NWDVAR+1
            LSO2ACT= LIQ1+ (N3-1)/NWDVAR+1
            LSO2COR= LSO2ACT+ N23*(N23-NRED+1)*(4-NPASS)
C           lso2cor stores core-active 2e integrals in MO basis
C           lso2act stores active-active 2e integrals in MO basis
C           lso2cin stores core-active 2e integrals in AO basis
         ELSE
C           lso2act is not used
            LSO2ACT= LAST1
            LSO2COR= LAST1
         ENDIF
C---     LDC    = LSO2COR+ N3*(4-NPASS)
         LDC    = LSO2COR+ (NMO*NMO)*(4-NPASS)*NEC
         LSO2CIN= LDC    + L3*NEC
         LCOPCON1=LSO2CIN+ L3*NEC*3
         LAST1   =LCOPCON1+MHSO*NCOPCON
      ELSE
         LCOPCON1=LCOPCON
      ENDIF
      LSOAO   = LAST1
      LMW     = LSOAO + L2*3
                    LAST1= LMW + 36*3
      IF(MAXL.EQ.3) LAST1= LMW + 100*3
      IF(MAXL.EQ.4) LAST1= LMW + 225*3
      IF(MAXNCO.GT.1) THEN
         MMM=MAX(N2INT(1),N2INT(2),N2INT(3))
         IF(MMM.NE.0) NSO2BUF=MIN(MMM,NSO2BUF)
         IF(MCP2E.EQ.1) NSO2BUF=0
C        write(6,*) 'allocc',nso2buf,N2INT(1),N2INT(2),N2INT(3)
         LSO2AO= LAST1
         LWORK = LSO2AO+NSO2BUF
         LCOPART=LWORK +N1
         LAST1 = LCOPART +L1*NMO
         LAST1 = LCOPART +L1*N1
C        now grab the remaning space for the partial sum buffer
         IF(MAXNCO.EQ.2) THEN
           NEEDNOW=LAST1-LOADFM
C          write(iw,*) 'words left for the partial sums',ngot-neednow
           M1PART=MAX(MIN((NGOT-NEEDNOW)/(L1*L1*L1),N1),MINMO)
C          write(iw,*) 'can do ',m1part,' orbitals/pass'
           LPARTSUM=LAST1
           LAST1   =LPARTSUM + L1*L1*L1*M1PART
C          write(iw,*) 'allocc',l1*l1*l1*m1part
         ELSE
           M1PART=N1
           LPARTSUM=LAST1
C        not used
         ENDIF
      ENDIF
C
C     NOTE THAT CSF STORAGE OVERLAYS AO INTEGRAL STORAGE.
C
      NCSFP=(NCSF-1)/NPROC+1+1
C     allocate an extra CSF for skipping unwanted CSFs in parallel
      NCSFPC=NCSFP
      IF(LAMECSF.NE.0) NCSFPC=1
C     write(iw,*) 'grabb',N1,NCSFPC
      IOPENX = LSOAO
      ICASEX = IOPENX + (NCSFP-1)/NWDVAR + 1
      IECONX = ICASEX + (N1*NCSFP-1)/NWDVAR + 1
      ICOEF  = IECONX + (N1*NCSFPC-1)/NWDVAR + 1
      JCOEF  = ICOEF  + MXRT*NCSFP
      JECONF = JCOEF  + NCSF
      LBUFPK = JECONF + N1/NWDVAR + 1
      IPRIM  = LBUFPK +  MXSPIN*MXPRM
      JPRIM  = IPRIM  + (MXSPIN*MXPRM*2-1)/NWDVAR + 1
      LCGCI  = JPRIM  + (MXSPIN*MXPRM*2-1)/NWDVAR + 1
      LCGCJ  = LCGCI  + MXPRM*2
      LNTRAP = LCGCJ  + MXPRM*2
      LBK    = LNTRAP + (MXSPIN-1)/NWDVAR + 1
      JCASE  = LBK    + MXSPIN
      IICAS  = JCASE  +     N1/NWDVAR + 1
      JJCAS  = IICAS  + MXSPIN/NWDVAR + 1
      NTRAP  = JJCAS  + MXSPIN/NWDVAR + 1
      MARE   = NTRAP  + MXSPIN/NWDVAR + 1
      IEPACK = MARE   + (2**N1PACK-1)/NWDVAR +1
      JEPACK = IEPACK + (INTPACK*NCSFP-1)/NWDVAR +1
      LECONFIX=JEPACK + (INTPACK-1)/NWDVAR+1
      LAST2  = LECONFIX+NCSFP
      IF(NFFBUF2.NE.0) THEN
      LFFBUF1= LAST2
      LFFBUF2= LFFBUF1 + MXRT*NACT2
      LFFIND2= LFFBUF2 + MXRT*NFFBUF2*NACT2
      LFFBUFI1=LFFIND2 + (NFFBUF2*NACT2-1)/NWDVAR+1
      LFFINDI1=LFFBUFI1+ NFFBUFI1
      LFFBUFI2=LFFINDI1+ (NFFBUFI1-1)/NWDVAR+1
      LFFINDI2=LFFBUFI2+ NFFBUFI2
      NFF2    =LFFINDI2+ (NFFBUFI2-1)/NWDVAR+1
      LAST2   =NFF2    + (NACT2-1)/NWDVAR+1
      ENDIF
C
C     2 above stands for one and two bodies
C
      NEED = MAX(LAST1,LAST2) - LOADFM
      IF(NRED.GT.1.AND.MASWRK) WRITE(IW,*) 'LOW MEMORY MODULE LAUNCHED.'
      IF (MASWRK) WRITE(IW,930) NEED,ANGL(MAXNCO),NPASS,LAST1-LOADFM,
     *                          LAST2-LOADFM
      IF(MAXNCO.EQ.2.AND.NEED.GT.NGOT) THEN
         NPASS=NPASS+1
         IF(NPASS.GT.3) THEN
C        now we are at the end of the rope. Time to soap the cord.
            IF(LAST1.GT.LAST2.AND.NUMVEC.EQ.1.AND.NRED.LE.1) THEN
               NRED=(NEED-NGOT)/N23/(4-N0PASS)+2
C              write(iw,*) 'settt',nred,n23
               IF(NRED.GT.1.AND.NRED.LT.N23) THEN
                  NDPASS=(N23-1)/(N23-NRED)+1
                  NRED=N23-(N23-1)/NDPASS-1
C                 write(iw,*) 'setttuu',nred,ndpass,n23-nred
                  NPASS=N0PASS
                  GOTO 105
               ENDIF
            ENDIF
            WRITE(IW,*) NGOT,' WORDS AVAILABLE. '
            CALL ABRT
         ENDIF
         GOTO 105
      ENDIF
      CALL GETFM(NEED)
      IF(NRED.LE.1) NRED=0
      NDRAGO=N2-NRED
      IF(NUMVEC.EQ.1) N23A=NDRAGO
      IF(NUMVEC.EQ.1.AND.NRED.NE.0) N23A=NDRAGO+1
      IF(NUMVEC.NE.1) N23A=N3
      IF((RMETHOD.EQ.RESC.OR.RMETHOD.EQ.DK).AND.NESOC.GT.1.
     *   AND.NRED.NE.0) THEN
         IF(MASWRK) WRITE(IW,9600)
         CALL ABRT
      ENDIF
C     write(iw,*) 'hhh',nred,ndrago,n23
C
C     iprim(1) and jprim(2) used ti be used interchangeably,
C     depending on if ici1 is equal ici2
C
      IF(MAXNCO.EQ.2.OR.MPLEVL.NE.0) CALL FORMF(FFAC,EZ2P)
C
      DO 800 ISF=1,NISFSO
         EDSHFT=PISFSO(ISF)
         IF(MPLEVL.NE.0.AND.MASWRK) WRITE(IW,9330) EDSHFT
      CALL VCLR(X(LHSO),1,NHSO*NHSO*2*MHSO)
      CALL VCLR(X(LTDM),1,NTDM*NTDM*3)
      IF(JZOPT.NE.0) CALL VCLR(X(LLZ),1,NTDM*NTDM)
      IF(MHSO.LE.1) THEN
         CALL ADDZERO(NHSO,X(LHSO),X(LEIG),ENGYST,MULST,IROOTS,NZSPIN,
     *                EZERO(1))
      ELSE
         CALL ADDZERO(NHSO,X(LHSO),X(LEIG),EREFCAS,MULST,IROOTS,NZSPIN,
     *                EZERO(1))
         CALL ADD0MP(NHSO,X(LHSO+NHSO*NHSO*2),X(LEIG+NHSO),ENGYST,HMP2,
     *               MULST,IROOTS,NZSPIN,EZERO(2))
      ENDIF
      IF(MASWRK) CALL SOCINFO(NOSYM,IPRHSO)
C     initialise the second set of overlaps (ONE's)
C     to be used if ivex1==ivex2
      IF(NUMVEC.GT.1) THEN
         DO 110 I=NUM+1,NUM+N1
            DEIG(I)=ONE
  110    CONTINUE
      ENDIF
C
C     ----- COMPUTE DIPOLE INTEGRALS -----
C
      CALL CALCOM(XP,YP,ZP)
      CALL PRCALC(ELMOM,X(LSOAO),X(LMW),3,L2)
      DO 120 I=1,3
         LSOAOI=LSOAO+(I-1)*L2
         CALL DAWRIT(IDAF,IODA,X(LSOAOI),L2,94+I,0)
  120 CONTINUE
C
C     ---- READ two MO sets
C
      CALL DAREAD(IDAF,IODA,X(LCMO(1)),L3,15,0)
      CALL DAREAD(IDAF,IODA,X(LCMO(2)),L3,19,0)
C
C     ---- read RESC left (A) and right (B) transformation matrices
C
      IF((RMETHOD.EQ.RESC.OR.RMETHOD.EQ.DK).AND.NESOC.GT.1) THEN
C
C        RESC matrices are used only for 2e integrals,
C        1e integrals read them separately
C
         CALL DAREAD(IDAF,IODA,X(LRESCA),L3,NDARELB,0)
         CALL DSCAL(L3,TWO*CLIG,X(LRESCA),1)
         CALL DAREAD(IDAF,IODA,X(LRESCB),L3,NDARELB+1,0)
         CALL DSCAL(L3,TWO*CLIG,X(LRESCB),1)
      ENDIF
C
C     Compute the core contribution to the dipole moments
C     Get the core density to LTMMO and square dipole integrals to
C     LTMMO+L3.
C
      CALL MRARTR(X(LCMO(2)),L1,L1,NFZC,X(LCMO(1)),L1,L1,X(LTMMO),L1)
      DO I=1,3
         CALL CPYTSQ(X(LSOAO+L2*(I-1)),X(LTMMO+L3),L1,1)
         DMCORE(I)=-TWO*DDOT(L3,X(LTMMO+L3),1,X(LTMMO),1)
C        two is the occupation number, alpha+beta
C        minus comes from the charge of electron
      ENDDO
C
      CALL VCLR(X(LCOPCON),1,MHSO*NCOPCON)
      IF(MAXNCO.GT.1) THEN
         CALL VCLR(X(LCOPCON1),1,MHSO*NCOPCON)
         IF(NFZC.EQ.0) THEN
           CALL VCLR(X(LDC),1,L3)
C          will MRTRBR be ever smart enough to handle that?
         ELSE
C
C      core is used to contract the core orbitals with 2e integrals ->
C      when combined with the RESC transformation the core is changed
C
           LORB1=LCMO(1)
           LORB2=LCMO(2)
           IF((RMETHOD.EQ.RESC.OR.RMETHOD.EQ.DK).AND.NESOC.GT.1) THEN
C            since NFZC .ge. NMOFZC, the two calls below work for both
             CALL MRTRBR(X(LRESCA),L1,L1,L1,X(LORB1),L1,NFZC,
     *                   X(LLEFTRA),L1)
             CALL MRTRBR(X(LRESCB),L1,L1,L1,X(LORB2),L1,NFZC,
     *                   X(LRIGHTRA),L1)
             LORB1=LLEFTRA
             LORB2=LRIGHTRA
           ENDIF
C          C * C-dagger
           CALL MRARTR(X(LORB2),L1,L1,NFZC,X(LORB1),L1,L1,X(LDC),L1)
C          wouldn't hurt to symmetrise core density in any case
C          if(rmethod.eq.resc.nesoc.gt.1)
           IF(NOSYMTRZE.EQ.0) CALL SYMTRZE(X(LDC),L1,L1)
           IF(NEC.GT.1) THEN
             CALL MRARTR(X(LORB2+L1*NMOFZC),L1,L1,NMODOC,
     *                   X(LORB1+L1*NMOFZC),L1,L1,X(LDC+L3),L1)
             IF(NOSYMTRZE.EQ.0) CALL SYMTRZE(X(LDC+L3),L1,L1)
             IF(NMODOC.EQ.0) CALL VCLR(X(LDC+L3),1,L3)
           ENDIF
         ENDIF
         IF(DBGINT) THEN
            WRITE(IW,*) 'CORE DENSITY'
            CALL PRSQ(X(LDC),L1,L1,L1)
            IF(NEC.GT.1) WRITE(IW,*) 'MCQDPT CORE DENSITY'
            IF(NEC.GT.1) CALL PRSQ(X(LDC+L3),L1,L1,L1)
         ENDIF
      ENDIF
      LUNTWO=60
      IF(MPLEVL.NE.0) THEN
C
C        read orbital energies,
C        read one and two electron 1/r integrals (in MO basis)
C
         CALL MQOPDA(1)
         CALL MQDARE(IDAF50,IODA50,X(LEORB),NMO,9,0)
         CALL MQDARE(IDAF50,IODA50,X(LVONEEL),NMO*NMO,20,0)
         NDSIZE=MQDSIZ(NMO*NMO,'I')
         CALL MQDARE(IDAF50,IODA50,X(LLIJMO),NDSIZE,19,1)
C        confix is recomputed below anyway?
         CALL MQDARE(IDAF50,IODA50,X(LECONFIX),NWKSST(1),21,0)
         CLOSE(UNIT=IDAF50,STATUS='KEEP')
         CALL SEQOPN(LUNTWO,'MCQD60','UNKNOWN',.FALSE.,'UNFORMATTED')
         DO I=1,NMOII
           CALL MQMATR(LUNTWO,NMOEI,X(LVTWOEL+(I-1)*NMOEI))
         END DO
         CALL SEQCLO(LUNTWO,'KEEP')
         CALL MP2IRL(NMOACT,NMODOC,NMOFZC,NMO,NOSYM)
      ENDIF
      LLET='LZ'
      IF(NAT.EQ.1) LLET=' L'
C
      NEED1A=1
      DO I=1,3
        NEEDDC(I)=1
        NEED2A(I)=1
      ENDDO
C
      NCALC=0
      NPROP=0
      NHERM=0
      NSPRO=0
      NSPAC=0
      NZERO=0
      NSYMFOR=0
      HSOMAX=ZERO
      DO 220 ICI1=1,NUMCI
        DO 210 ICI2=ICI1,NUMCI
          MM1=MULST(ICI1)
          MM2=MULST(ICI2)
          SAMEMUL=MM1.EQ.MM2
          IF(MM1.EQ.1.AND.MM2.EQ.1.AND.SKIPDM.AND.JZOPT.EQ.0.AND.
     *      NOSYM.LE.1) THEN
            NSPRO=NSPRO+(MM1*(MM1+1))/2
            GOTO 210
C
C           no interaction between singlets
C           we must not skip if Jz or R matrix elements are needed.
C
          ENDIF
C
C         Luckily <A|L{xyz}|A> is zero for any A described with real
C         numbers. (so we don't have to compute it in the "if" below).
C
          IF(ICI1.EQ.ICI2.AND.IROOTS(ICI1).LE.1) THEN
            IF(NOSYM.LE.1.AND..NOT.TMOMNT) THEN
               NHERM=NHERM+(MM1*(MM1+1))/2
               GOTO 210
            ENDIF
C           no interaction between same states
          ENDIF
          IF(ABS(MM1-MM2).GT.2) THEN
            IF(NOSYM.LE.1) THEN
               NSPRO=NSPRO+MM1*IROOTS(ICI1)*MM2*IROOTS(ICI2)
               GOTO 210
            ENDIF
C           no interaction if |S1-S2|>1
          ENDIF
          LL1=L2VAL(ICI1)
          LL2=L2VAL(ICI2)
C         Note that in the case below both L and R matrix elements are
C         also zero by symmetry.
C         .not.lselrule(ll1,ll2,mm1,mm2,nat.eq.1).and.nosym.le.1) then
          IF(LL1.GE.0.AND.LL2.GE.0.AND.NOSYM.LE.1.AND.
     *       (ABS(LL1-LL2).GT.1.OR.(NAT.EQ.1.AND.LL1+LL2.EQ.0))) THEN
C            fine point: L=L'=0 is forbidden in atoms, because
C            addition of L and L' cannot generate L=1 for L in LS.
C            in diatomics L in LS is Sigma+Pi, so
C            Lz=Lz'=0 can interact with each other through that Sigma.
             IF(.NOT.LINEAR) THEN
                WRITE(IW,*) 'DO NOT SET LVAL FOR NONLINEAR MOLECULES'
                CALL ABRT
             ENDIF
             IF(MASWRK) WRITE(IW,9320) LLET,LL1,LLET,LL2
             NSPAC=NSPAC+MM1*IROOTS(ICI1)*MM2*IROOTS(ICI2)
             GOTO 210
C           (atoms) no interaction if |L1-L2|>1 or both L1=0 and L2=0
C           (diatomics) no interaction if W1!=W2, W is Omega=Sz+Lz
          ENDIF
          IF(NEED1A.NE.0) THEN
            LVST1=LCMO(IVEX(ICI1))
            LVST2=LCMO(IVEX(ICI2))
            LVST1A=LVST1+L1*NFZC
            LVST2A=LVST2+L1*NFZC
            DO 130 I=1,3
C
C          ---- READ 1E- SPIN ORBIT AO-INTEGRALS -----
C         ----- TRANSFORM 1E AO-INTEGRALS TO MO BASIS -----
C       NOTE THAT the spatial part of 1E SO OPERATOR IS ANTI-HERMITIAN.
C
              LSOMOI=LSOMO+(I-1)*(NMO*NMO)
              CALL DAREAD(IDAF,IODA,X(LSOAO),L2,83+I,0)
C
              CALL TMOINT(X(LSOMOI),X(LVST1),X(LVST2),X(LSOAO),ITAG(I),
     *                    2,NMO,L1,L2,DBGINT)
C         ----- TRANSFORM THE AO INTEGRALS TO MO INTEGRALS -----
C        IFORM=1 COMPUTES THE LENGTH FORM   - HERMITIAN OPERATOR
C
              LTMMOI=LTMMO+(I-1)*N3
              CALL DAREAD(IDAF,IODA,X(LSOAO),L2,94+I,0)
              CALL TMOINT(X(LTMMOI),X(LVST1A),X(LVST2A),X(LSOAO),
     *                    ITAG(I+3),1,N1,L1,L2,DBGINT)
              LU1I=LUONEEL+(I-1)*(NMO*NMO)
            IF(MPLEVL.GT.0.AND.MP2SO.EQ.0) CALL VCLR(X(LU1I),1,NMO*NMO)
              IF(MP2SO.GT.0) CALL DCOPY(NMO*NMO,X(LSOMOI),1,X(LU1I),1)
  130       CONTINUE
C
            IF(JZOPT.NE.0) THEN
C         ----- TRANSFORM THE AO INTEGRALS TO MO INTEGRALS -----
C            "Lz" AO integrals [r x d/dr]z, i.e. antihermitian.
C
              CALL DAREAD(IDAF,IODA,X(LSOAO),L2,379,0)
              CALL TMOINT(X(LZMO),X(LVST1A),X(LVST2A),X(LSOAO),
     *                    ITAG(7),2,N1,L1,L2,DBGINT)
            ENDIF
            IF(NUMVEC.EQ.1) NEED1A=0
C           as niggardly as it is
          ENDIF
C
          IF((RMETHOD.EQ.RESC.OR.RMETHOD.EQ.DK).AND.NESOC.GT.1) THEN
C
C       RESC transforms AO ints -> A * A * I2e * Bt * Bt
C       t stands for transposed (RESC uses math not phys convention)
C       RESC transformation of 2e integrals can be combined with the MO
C       4-index transformation
C       i.e. Ct*Ct*I2e*C*C -> (AtC)t (AtC)t * I2e * (BtC) * (BtC)
C       A and B do not define a symmetric transformation, but the
C       antisymmetrisation of MO 2e integrals is possible for NUMVEC=1
C       I2e(MO) -> (I2e(MO)+I2e(MO)t)/2
C
          CALL MRTRBR(X(LRESCA),L1,L1,L1,X(LVST1),L1,NMO,X(LLEFTRA),L1)
          CALL MRTRBR(X(LRESCB),L1,L1,L1,X(LVST2),L1,NMO,X(LRIGHTRA),L1)
            LLEFT2E=LLEFTRA+L1*NFZC
            LRIGHT2E=LRIGHTRA+L1*NFZC
            LLEFT2EC=LLEFTRA
            LRIGHT2EC=LRIGHTRA
          ELSE
            LLEFT2E=LVST1A
            LRIGHT2E=LVST2A
            LLEFT2EC=LVST1
            LRIGHT2EC=LVST2
          ENDIF
C
C     ----- BEGIN THE GUGA CALCULATION OF THE S.O.C. -----
C           and transition moments.
C
          J1=MM1-1
          J2=MM2-1
C         j1,j2 are also doubled (j**2*2)
          CALL TSECND(TYME)
          IF(TYME.GT.TIMLIM) THEN
            IF(MASWRK) WRITE(IW,9500)
            GOTO 590
          ENDIF
C             the work is divided between passes as follows:
C             npass=1     All in one
C             npass=2     ipass=1  <x> and <y> of 2e and all of 1e
C                         ipass=2  <z> of 2e
C             npass=3     ipass=1  <x> of 2e and all 1e
C                         ipass=2  <y> of 2e
C                         ipass=3  <z> of 2e
C
          CALL VCLR(X(LSOL1MUL),1,3*MXRT*MXRT*MHSO)
          CALL VCLR(X(LSOL2MUL),1,3*MXRT*MXRT)
          CALL VCLR(X(LSTMMUL),1,3*MXRT*MXRT)
          IF(JZOPT.NE.0) CALL VCLR(X(LLZMUL),1,MXRT*MXRT)
          NEEDPASS=NPASS
          IF(J1.EQ.0.AND.J2.EQ.0) NEEDPASS=1
C
C         drags run over fraction of 2e integrals for low memory
C
          DO 205 IDRAG=1,N2,NDRAGO
C         205 loop normally executes only once
          NDRAGS=MIN(NDRAGO,N2-IDRAG+1)
          IF(MAXNCO.EQ.2)
     *       CALL QINDEX(N1,IDRAG,NDRAGS,X(LPQ),X(LIQ),X(LIQ1))
          IF(NDRAGS.NE.N2.AND.MASWRK) WRITE(IW,9100) IDRAG
C
C         passes run over x,y,z components of L.
C
          DO 200 IPASS=1,NEEDPASS
C
C         -----  2E AO-INTEGRALS TO MO BASIS -----
C
            IF(J1.EQ.0.AND.J2.EQ.0.OR.
     *         NPASS.NE.1.AND.IPASS.EQ.NEEDPASS) THEN
              KARTMIN=3
              KARTMAX=3
            ELSE IF(NPASS.EQ.1) THEN
              KARTMIN=1
              KARTMAX=3
            ELSE IF(NPASS.EQ.2) THEN
              KARTMIN=1
              KARTMAX=2
            ELSE
              KARTMIN=IPASS
              KARTMAX=IPASS
            ENDIF
            KARTMIN0=KARTMIN
C
C      kartmin0 defines offset for the integrals
C      even though one calculates <Lz> only (i.e.kartmin=3) it may need
C      to be stored as the third element of the integral array (npass=1)
C      so that it can be reused later.
C      kartmin defines the starting value of (x,y,z) for the SOC
C      calculation CI state-wise. Note that CSF-wise kartmin/kartmax
C      can be changed if CSFs have different symmetry within a state
C      (non-Abelian groups within Abelian subgroups)
C
            DODM=SAMEMUL.AND.KARTMAX.EQ.3.AND..NOT.SKIPDM
            DOSOL=NOSYM.GT.0
            DO 140 KART=KARTMIN,KARTMAX
              CALL GCISOL(SYLYES(KART),ISTSYM,IMS(KART),ICI1,ICI2,
     *                   IROOTS,IZEROT,1,IRRL,0)
  140         IF(.NOT.SYLYES(KART).AND.NOSYM.LE.0) NSPAC=NSPAC+IZEROT
            DO 150 KART=1,3
  150         CALL GCISOL(SYRYES(KART),ISTSYM,IMS(KART),ICI1,ICI2,
     *                   IROOTS,IZEROT,0,IRRR,0)
C
C     the symmetry code below is copied from SOBOOK (quid vide)
C     here kartmin and kartmax are global - applicable to all CSF pairs
C     whereas in SOLOOP kirmin and kirmax are local - for each pair
C     third from the right argument indicates if it is <R> or <Hso>
C     which is only used for diagonal (not considered for Hso)
C
            MAX1NCO=MAXNCO
            IF(J1.EQ.0.AND.J2.EQ.0) MAX1NCO=1
C           do not bother for 2e part for singlet/singlet
            IF(NOSYM.LE.0) THEN
              DODM=DODM.AND.(SYRYES(1).OR.SYRYES(2).OR.SYRYES(3))
              DO 160 KART=KARTMIN,KARTMAX
                IF(SYLYES(KART)) THEN
                  DOSOL=.TRUE.
                  GOTO 165
                ELSE
                  KARTMIN=KARTMIN+1
                ENDIF
  160         CONTINUE
  165         CONTINUE
              DO 170 KART=KARTMAX,KARTMIN,-1
                IF(SYLYES(KART)) GOTO 175
                KARTMAX=KARTMAX-1
  170         CONTINUE
  175         CONTINUE
              IF(DODM) THEN
                KARTMAX=3
                IF(KARTMIN.GT.KARTMAX) KARTMIN=3
              ENDIF
              IF(.NOT.DOSOL) MAX1NCO=1
C             do not do 2e contributions if only <R> is wanted
            ENDIF
            IF(.NOT.DOSOL.AND..NOT.DODM) GOTO 200
            IF(MAX1NCO.GT.1) THEN
              LEECH=0
              DO 180 KART=KARTMIN,KARTMAX
                IF(NEEDDC(KART).EQ.0.AND.NEED2A(KART).EQ.0) GOTO 180
                IF(LEECH.EQ.0.AND.MASWRK) WRITE(IW,9000) IPASS
                TMO2=SYLYES(KART).OR.NOSYM.GT.0
                LEECH=1
                I=KART-KARTMIN0
                LSO2CORI=LSO2COR+I*NMO*NMO
                LSO2ACTI=LSO2ACT+I*N23*N23A
                LSO2CINI=LSO2CIN+(KART-1)*NEC*L3
                LUONEELI=LUONEEL+I*NMO*NMO
                CALL TMOINT2(X(LSO2ACTI),X(LSO2ACTI),X(LLEFT2E),
     *                       X(LRIGHT2E),X(LLEFT2EC),X(LRIGHT2EC),
     *                       X(LPARTSUM),X(LWORK),N2INT(KART),NSO2BUF,
     *                       X(LSO2AO),X(LSO2AO),NEEDDC(KART),X(LDC),
     *                       X(LSO2CINI),X(LCOPART),X(LSO2CORI),KART,N1,
     *                       NMO,N2,NDRAGS,L1,M1PART,4-NPASS,NEC,MAXNCO,
     *                       NFT2SO(KART),X(LIQ),X(LIQ1),DISTINT,TMO2,
     *                       MCP2E,MCPREC+KART,DBGINT)
C               core is the same for all states, do only once
                NEEDDC(KART)=0
                IF(NUMVEC.EQ.1.AND.NRED.EQ.0.AND.NPASS.EQ.N0PASS)
     *             NEED2A(KART)=0
C               Frankenstein's solution to redo the 2e transformations
C
                IF(NOSYMTRZE.NE.0) GOTO 177
C         RESC has to be symmetrised hence the long code below,
C         but DK is already symmetric.
                IF(RMETHOD.EQ.RESC.AND.NESOC.GT.1.AND.NUMVEC.EQ.1) THEN
C
C          Now perform the antisymmetrisation. To do that we need the
C          upper blocks of the (normally) (anti)symmetric matrix.
C          the (anti)symmetric properties of 2e integrals are properly
C          taken care of if one switches left/right transformations
C          and symmetrises (adds) the two triangular blocks.
C          Of course the "block" is of the order N**4!! and disk cache
C          will be performed!
C          On the other hand, core-active 2e integrals are got as
C          a square matrix - can do direct antisymmetrisation)
C          if we do call tmoint2, lso2cori is substituted by a dummy
C          also, needdc needs to have been set to 0 because otherwise
C          lso2cini (core-active 2e ints in AO basis) will have
C          opposite sign due to trasposed transformation matrices(?)
C
                 IF(MAXNCO.EQ.2) THEN
                  CALL CACHEVEC(NFTRESC,N2*NDRAGS,X(LSO2ACTI),1,NSO2BUF)
                  CALL TMOINT2(X(LSO2ACTI),X(LSO2ACTI),X(LRIGHT2E),
     *                         X(LLEFT2E),X(LRIGHT2EC),X(LLEFT2EC),
     *                         X(LPARTSUM),X(LWORK),N2INT(KART),NSO2BUF,
     *                         X(LSO2AO),X(LSO2AO),NEEDDC(KART),X(LDC),
     *                         X(LSO2CINI),X(LCOPART),X(LCOPART),KART,N1
     *                        ,NMO,N2,NDRAGS,L1,M1PART,4-NPASS,NEC,
     *                         MAXNCO,NFT2SO(KART),X(LIQ),X(LIQ1),
     *                         DISTINT,TMO2,MCP2E,MCPREC+KART,DBGINT)
C                 call prsq(x(lso2acti),ndrags,n2,n2)
                  CALL CACHEDAXPY(NFTRESC,N2*NDRAGS,ONE,X(LSO2ACTI),1,
     *                            X(LSO2AO),NSO2BUF)
                  CALL DSCAL(N2*NDRAGS,HALF,X(LSO2ACTI),1)
                 ENDIF
                 DO IK=1,NEC
                   CALL ASYMTRZE(X(LSO2CORI+(IK-1)*NMO2*(4-NPASS)),NMO,
     *                           NMO,ASYMM)
                   IF(DBGSO) WRITE(IW,*) I,'ASYMM',ASYMM
                 ENDDO
                ENDIF
                IF(RMETHOD.EQ.RESC.AND.NESOC.GT.1.AND.NUMVEC.EQ.2) THEN
                  DO IK=1,NEC
C
C           no active-active averaging symmetrisation is implemented.
C           The easiest way to do it seems to be to run TMOINT2 with
C           RESC matrices, symmetrised L3*L3 transformed AO integrals,
C           save non-zero ones and run TMOINT2 again with MO matrices
C           (similar to 1e case).
C           averaging symmetrisation is implemented for core-act 2e only
C              RESC transformation
C            note: lso2cini is obtained with DC, not VST1/2.
C
                    CALL MRARBR(X(LRESCA),L1,L1,L1,X(LSO2CINI+(IK-1)*
     *                          NMO2),L1,L1,X(LLEFTRA),L1)
                    CALL MRARTR(X(LLEFTRA),L1,L1,L1,X(LRESCB),L1,L1,
     *                          X(LRIGHTRA),L1)
                    CALL ASYMTRZE(X(LRIGHTRA),L1,L1,ASYMM)
                    IF(DBGSO) WRITE(IW,*) I,'ASYMM',ASYMM
C                   MO transformation
                    CALL MRARBR(X(LRIGHTRA),L1,L1,L1,X(LVST2),L1,NMO,
     *                          X(LCOPART),L1)
                    CALL MRTRBR(X(LVST1),L1,L1,NMO,X(LCOPART),L1,NMO,
     *                          X(LSO2CORI+(IK-1)*NMO2*(4-NPASS)),NMO)
                  ENDDO
                ENDIF
  177         CONTINUE
              NSHIFT=0
              IF(NEC.GT.1) NSHIFT=NMO2*(4-NPASS)
              IF(MP2SO.GT.1) CALL DAXPY(NMO*NMO,ONE,X(LSO2CORI+NSHIFT),
     *                                  1,X(LUONEELI),1)
  180         CONTINUE
              IF(LEECH.NE.0) CALL TIMIT(1)
              IF(MASWRK) WRITE(IW,*) ' '
              IF(IDRAG.NE.1) CALL VCLR(X(LSO2COR),1,NMO*NMO*NEC*3)
C
C         for several drags only the first drag adds core contribution
C
            ENDIF
            IF(MPLEVL.GT.1.AND.DBGINT.AND.MASWRK) THEN
              WRITE(IW,*) 'FINAL MCQDPT 1E PERTURBATION'
C             call prsq(x(lvoneel),nmo,nmo,nmo)
              WRITE(IW,*) 'FINAL MCQDPT 2E PERTURBATION',NMOEI,NMOII
C             call prsq(x(lvtwoel),NMOII,NMOEI,NMOEI)
              WRITE(IW,*) 'FINAL SO MCQDPT 1E PERTURBATION X',MP2SO
              CALL PRSQ(X(LUONEEL),NMO,NMO,NMO)
              WRITE(IW,*) 'FINAL SO MCQDPT 1E PERTURBATION Y',MP2SO
              CALL PRSQ(X(LUONEEL+NMO*NMO),NMO,NMO,NMO)
              WRITE(IW,*) 'FINAL SO MCQDPT 1E PERTURBATION Z',MP2SO
              CALL PRSQ(X(LUONEEL+NMO*NMO*2),NMO,NMO,NMO)
            ENDIF
C
            MPNCO=0
            IF((MP2SO.EQ.1.OR.MP2SO.EQ.3).AND.MAX1NCO.EQ.MAXNCO)
     *         MPNCO=1
C
C           extra discoincidency allowed due to PT. At present only 1e
C           SOC is used in PT, so mpnco can only be 1.
C           MAX1NCO.ne.MAXNCO only if all SOC matrix elements are zero
C           by symmetry (either spin or orbit).
C           MAX1NCO used only to determine if 2e contribution is added
C
            ISOLOOP=0
 6666       CONTINUE
C
C           When doing form factors, SOLOOP is called twice, because
C           SO-MCQDPT Hamiltonian is not Hermitian, so first
C           <bra|H|ket> and then <ket|H|bra> are computed.
C           If nffbuf.eq.0 then the same two matrix elements are
C           computed inside of LOOPRM with only one call to SOLOOP.
C           It is possible to run SOLOOP only once with form factors
C           also, but that would require outrageous memory waste.
C           NB! in the second call of SOLOOP the fundamental rule that
C           the multiplicity of ket is .ge. than that of bra is no
C           longer valid. Care must be taken.
C
            IF(ISOLOOP.EQ.0) THEN
              JCI1=ICI1
              JCI2=ICI2
            ELSE
              JCI1=ICI2
              JCI2=ICI1
            ENDIF
            CALL SOLOOP(IW,NFT17,N1,N2,N23A,KARTMIN0,KARTMIN,KARTMAX,
     *                  NWKSST(JCI1),NWKSST(JCI2),IROOTS(JCI1),
     *                 IROOTS(JCI2),X(LSOMO),X(LTMMO),X(LZMO),X(LSO2COR)
     *                 ,X(LSO2ACT),FFAC,EZ2P,MAX1NCO,MPNCO,X(IOPENX),
     *                  X(ICASEX),X(IECONX),X(ICOEF),X(JCOEF),X(JECONF),
     *                  X(LBUFPK),X(IPRIM),X(JPRIM),X(LCGCI),X(LCGCJ),
     *                  X(LNTRAP),X(LBK),X(JCASE),X(IICAS),X(JJCAS),
     *                  N1PACK,INTPACK,X(MARE),X(IEPACK),X(JEPACK),
     *                  X(LSOL1MUL),X(LSOL2MUL),X(LSTMMUL),X(LLZMUL),
     *                  DBGSO,DBGSOMP,JCI1,JCI2,NWKSST,IVCORB,MULST,DEIG
     *                 ,IVEX,IRRL,IRRR,MXSPIN,MXPRM,SKIPDM,NOSYM1,NAEL,
     *                  SMALL,SMALL1,JZOPT,SAMEMUL,X(LPQ),X(LIQ),X(LIQ1)
     *                 ,LAMECSF,X(LEORB),X(LECONFIX),EREF0(1,JCI1),
     *                  EREF0(1,JCI2),X(LVONEEL),X(LVTWOEL),X(LUONEEL),
     *                  X(LLIJMO),X(LSOL1MPI),X(LSOL1MPJ),X(LFFBUFI1),
     *                  X(LFFBUFI2),X(LFFINDI1),X(LFFINDI2),X(LFFBUF1),
     *                  X(LFFBUF2),X(LFFIND2),X(NFF2),MPLEVL,NMO,MHSO,
     *                  NMOACT,NMODOC,NMOEXT,NMOFZC,NMOEI,MP2SO,EDSHFT,
     *                  EEQTOL,NEEQ,DMCORE,NFFBUFI1,NFFBUFI2,NFFBUF2,
     *                  ISOLOOP,MAXNFF2)
            ISOLOOP=ISOLOOP+1
            IF(NFFBUF2.NE.0.AND.ISOLOOP.LT.2) GOTO 6666
C
            IF(IDRAG.NE.1) THEN
C             each drag repeats one-electron calculation -> halve it
              CALL DSCAL(3*MXRT*MXRT*MHSO,HALF,X(LSOL1MUL),1)
              CALL DSCAL(3*MXRT*MXRT,HALF,X(LSTMMUL),1)
              IF(JZOPT.NE.0) CALL DSCAL(MXRT*MXRT,HALF,X(LLZMUL),1)
            ENDIF
  200     CONTINUE
  205     CONTINUE
          JETLAG=((ICI1-1)+(ICI2-1)*NUMCI)*MXRT*MXRT*MHSO
C
C         bookkeeping: save and output the results
C
          ICI1TMP = ICI1
          ICI2TMP = ICI2
          CALL SOBOOK(IW,ICI1TMP,ICI2TMP,IROOTS,X(LSOL1MUL),X(LSOL2MUL),
     *                X(LSTMMUL),X(LLZMUL),ISTSYM,IRRR,IRRL,SYLYES,
     *                SYRYES,MAXNCO,MULST,IRCIOR,ADD2E,NPROP,NSYMFOR,
     *                NSPAC,NZERO,NCALC,SKIPDM,NOSYM,IPRHSO,NHSO,NTDM,
     *                MHSO,X(LHSO),X(LTDM),X(LLZ),X(LCOPCON+JETLAG),
     *                X(LCOPCON1+JETLAG),HSOTOL,HSOMAX,JZOPT,SAMEMUL)
          IF(IPRHSO.GE.0) CALL TIMIT(1)
  210   CONTINUE
  220 CONTINUE
  590 CONTINUE
C
      IF (MASWRK) WRITE(IW,9300) NHSO,NCSF
C
      IF(DBGSO) CALL SOSTATS(NCALC,NZERO,NPROP,NHERM,NSPRO,NSPAC,
     *                       NSYMFOR,NOSYM,NHSO,HSOTOL,HSOMAX)
      IF(.NOT.DBGSO.AND.MASWRK) WRITE(IW,9720) SYMTOL*HSOMAX
      IF(NEEQ.NE.0.AND.MASWRK) WRITE(IW,9730) NEEQ,EEQTOL
      IF(NFFBUF2.NE.0.AND.MASWRK) WRITE(IW,9740) MAXNFF2,NFFBUF2
C
C     MCQDPT Hamiltonian is not Hermitian, symmetrise the matrix first
C
C     if(mplevl.ne.0) call CSYMTRZE(X(LHSO),NHSO,NHSO)
C
C     Erefcas has to be used instead of ENGYST to determine degeneracies
C     used in the coupling constant analysis (Erefcas has CAS energies
C     and coupling constants are always computed in the basis of CAS
C     states; ENGYST has MCQDPT energies). So we copy Erefcas to
C     ENGYST and do not restore it back (no need so far).
C
      IF(MHSO.EQ.2) CALL DCOPY(MXRT*NUMCI,EREFCAS,1,ENGYST,1)
C
C     DIAGONALIZATION OF COMPLEX HERMITIAN MATRIX HSO and other results
C
      GLOLAB='  CI  '
      DO IH=0,MHSO-1
      IF(MHSO.EQ.2.AND.IH.EQ.0) GLOLAB='CASCI '
      IF(MHSO.EQ.2.AND.IH.EQ.1) GLOLAB='MCQDPT'
      IF(MHSO.EQ.2.AND.MASWRK) WRITE(IW,9800) GLOLAB
      IF(MHSO.EQ.2.AND.IH.EQ.1.AND.MASWRK) WRITE(IW,9810)
      PRTLZ=LINEAR.AND.NAT.GT.1.AND.IH.EQ.0
C     There is no Lz values for MCQDPT, only for CAS
C
C     Diagonalise Hso matrix and compute properties.
C
      CALL SPNDIA(N1,NHSO,NTDM,X(LHSO+IH*NHSO*NHSO*2),X(LEIG+IH*NHSO),
     *            X(LTDM),X(LLZ),PRTPRM,TMOMNT,MULST,IROOTS,ENGYST,L2VAL
     *           ,MAXNCO,X(LCOPCON+IH),X(LCOPCON1+IH),X(LOMEGA+IH*NHSO),
     *            JZOPT,NZSPIN,MHSO,GLOLAB,PRTLZ,DBGSO)
      ENDDO
      IF(MASWRK) CALL SOMP2OUT(MHSO,NHSO,X(LEIG),X(LOMEGA),EZERO,
     *                         JZOPT.NE.0.AND.NAT.GT.1)
  800 CONTINUE
      CALL RETFM(NEED)
      RETURN
  930 FORMAT(/1X,I9,' WORDS OF MEMORY ARE ',A9,' FOR ',I1,' PASS(ES)',
     *       /I10,' WORDS FOR INTEGRALS,',I10,' WORDS FOR CSFS.')
 9000 FORMAT(/1X,53("-")/,
     *       ' 4-INDEX TRANSFORMATION AO->MO 2E SO INTEGRALS, PASS ',I1,
     *       /1X,53(1H-)/)
 9100 FORMAT(/1X,30("@")/,' LOW MEMORY DRIVER: DRAG ',I4,/1X,30("@")/)
 9300 FORMAT(/1X,'>>> DIMENSION OF HSO MATRIX = ',I7,
     *       /1X,'    LARGEST NUMBER OF WALKS = ',I7)
C9310 format(1x,'symmetry allowance for states',2I3,
C    *          ' of multiplicities',
C    *       2F4.1,' is',3L2,'(for delta ms 1,0,-1)',3L2,'r',3L2)
 9320 FORMAT(/1X,'SKIPPING ',A2,'=',I2,' AND ',A2,'''=',I2)
 9330 FORMAT(/1X,'ISF SPIN-DEPENDENT PARAMETER SET TO',F15.9,/)
 9500 FORMAT(/1X,'RUNNYNG OUTE OF TYME.',/)
 9600 FORMAT(/1X,'NESOC.EQ.2 CANNOT RUN IN THE LOW MEMORY MODE.',
     *       /1X,'EITHER INCREASE THE MEMORY OR REDUCE NESOC.')
 9720 FORMAT(/5X,'THE MAX HSO MATRIX ELEMENT ABSOLUTE ERROR IS ',
     *            1P,E8.2,' CM-1.'/
     *        5X,'TO DECREASE THE ERROR MAKE SYMTOL SMALLER.')
 9730 FORMAT(/6X,'ADDITIONAL (OR ENFORCED) SYMMETRY WAS USED TO ',
     *       'ELIMINATE',/1X,I12,
     *       ' REDUNDANT DIAGRAM BATCHES. ENERGY THRESHOLD: ',E8.2/)
 9740 FORMAT(/6X,'FORM FACTOR STATISTICS: MAX SIZE',I9,' ALLOCATED',I9)
 9800 FORMAT(/1X,57("_"),/5X,A6,' SPIN-ORBIT COUPLING HAMILTONIAN ',
     *       'AND PROPERTIES',/1X,57(1H-)/)
 9810 FORMAT(/1X,'WARNING: MCQDPT HAMILTONIAN IS COMPUTED IN THE BASIS',
     *       /1X,'OF CAS STATES. ALL LABELS ("STATE ID" ETC) FOR THE ',
     *       /1X,'EIGENVECTORS REFER TO CAS, NOT MCQDPT STATES.',/)
      END
C*MODULE SOZEFF  *DECK SOLOOP
      SUBROUTINE SOLOOP(IW,NFT17,N1,N2,N2A,KARTMIN0,KARTMIN,KARTMAX,
     *                  NWKSI,NWKSJ,IROOTI,IROOTJ,SO1MO,TMINT,ALZINT,
     *                  SO2COR,SO2ACT,FFAC,EZ2P,MAXNCO,MPNCO,IOPENX,
     *                  ICASEX,IECONX,COEFIX,COEFJ,JECONF,BUFFPK,IPRIM,
     *                  JPRIM,CGCI,CGCJ,NTRAP,BK,JCASE,IICAS,JJCAS,
     *                 N1PACK,INTPACK,MARE,IEPACK,JEPACK,SOL1MUL,SOL2MUL
     *                 ,STMMUL,ALZMUL,DBGSO,DBGSOMP,ICI1,ICI2,NWKSST,
     *                  IVCORB,MULST,DEIG,IVEX,IRRL,IRRR,MXSPIN,MXPRM,
     *                  SKIPDM,NOSYM1,NAEL,SMALL,SMALL1,JZOPT,SAMEMUL,PQ
     *                 ,IQ,IQ1,LAMECSF,EORB,ECONFIX,EREF0I,EREF0J,VONEEL
     *                 ,VTWOEL,UONEEL,LIJMO,SOL1MPI,SOL1MPJ,FFBUFI1,
     *                  FFBUFI2,INDFFI1,INDFFI2,FFBUF1,FFBUF2,INDFF2,
     *                  NFF2,MPLEVL,NMO,MHSO,NMOACT,NMODOC,NMOEXT,
     *                  NMOFZC,NMOEI,MP2SO,EDSHFT,EEQTOL,NEEQ,DMCORE,
     *                  NFFBUFI1,NFFBUFI2,NFFBUF2,ISOLOOP,MAXNFF2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL DBGSO,DBGSOMP,GOPARR,DSKWRK,MASWRK,SYLYES(0:3),SYRYES(3),
     *        SKIPDM,DIFVEX,DODM,DOSOL,DOJZ,DODM0,DOJZ0,SAMEMUL,HERMI,
     *        HERMI0,SIMON,ABEL,ABELPT,SYLYESG(3)
      PARAMETER (MXATM=500,ZERO=0.0D+00,ONE=1.0D+00)
      PARAMETER (DFAC = 5.84375713555D+00)
C
      DIMENSION SO2ACT(N2,N2A,3),SO1MO(NMO,NMO,3),SO2COR(NMO,NMO,3),
     *          TMINT(N1,N1,3),ALZINT(N1,N1),IOPENX(*),ICASEX(N1,*),
     *          IECONX(N1,*),COEFIX(IROOTI,*),COEFJ(IROOTJ),JECONF(N1),
     *          IPRIM(MXSPIN,MXPRM,2),JPRIM(MXSPIN,MXPRM,2),NTRAP(*),
     *          BUFFPK(MXSPIN,*),CGCI(MXPRM,2),CGCJ(MXPRM,2),BK(*),
     *          JCASE(N1),IICAS(MXSPIN),JJCAS(MXSPIN),NWKSST(*),
     *          SOL1MUL(3,MXRT,MXRT,MHSO),SOL2MUL(3,MXRT,MXRT),MULST(*),
     *          STMMUL(3,MXRT,MXRT),ALZMUL(MXRT,MXRT),IVCORB(NUM,*),
     *          DEIG(*),IVEX(*),N1234(4),IRRL(3),IRRR(3),SOL1(3),IMS(3),
     *          SOL1MPI(MXRT,*),SOL1MPJ(MXRT,*),SOL2(3),STM(3),
     *          NPRJ(2),FFAC(0:1,0:1,0:1,0:1),EZ2P(0:1,0:1,0:1,0:1),
     *          IEPACK(INTPACK,*),JEPACK(INTPACK),MARE(0:2**N1PACK-1),
     *          PQ(N1,N1),IQ(N1,N1),IQ1(N1,N1),EORB(*),ECONFIX(*),
     *          EREF0I(*),EREF0J(*),VONEEL(NMO,*),VTWOEL(NMOEI,*),
     *          UONEEL(NMO,NMO,3),LIJMO(NMO,*),DMCORE(3),NFFI(2),
     *          FFBUFI1(NFFBUFI1),INDFFI1(NFFBUFI1),FFBUFI2(NFFBUFI2),
     *          INDFFI2(NFFBUFI2),FFBUF1(MXRT,NMOACT,*),NFF2(NMOACT,*),
     *          FFBUF2(MXRT,NFFBUF2,NMOACT,*),INDFF2(MXRT,NMOACT,*)
C
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /SYMMOL/ GROUP,COMPLX,IGROUP,NAXIS,ILABMO,ABEL
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
C
      DATA N1234/ 0, 1, -1, 0 /,IMS/-1,1,0/
C
C     ----- MAIN DRIVER FOR GUGA BASED SPIN ORBIT COUPLING -----
C
C     calculate matrix element for a pair of mupliplicities
C
C     DFAC = (7.2973506D-03)**2/2.0D+00
C     DFAC = 2.662566288D-05  HARTREE      *27.20967*8066.19
C                             (BECAUSE INTEGRALS HAVE A.U.)
C
C     NUMIJ  = MULST(ici1)*MULST(ici2)
      IF(DBGSO) WRITE(IW,9210) ICI1,MULST(ICI1),NWKSI,
     *                         ICI2,MULST(ICI2),NWKSJ
C
      HERMI0=ICI1.EQ.ICI2.AND.MPLEVL.EQ.0
      ABEL=ABELPT()
      DIFVEX=IVEX(ICI1).NE.IVEX(ICI2)
      IF(DIFVEX) THEN
         LODEIG=NFZC+1
C        real overlaps, excluding core
      ELSE
         IF(NUMVEC.GT.1) LODEIG=NUM+1
         IF(NUMVEC.LE.1) LODEIG=1
C        an array of one's. difvex being false does not imply numvec=1
      ENDIF
      CALL VICLR(IEPACK,1,((NWKSI-1)/NPROC+1)*INTPACK)
      CALL SETMARE(N1PACK,MARE)
      MAXNCO2=MAXNCO*2
      IF(MAXNCO.EQ.3) MAXNCO2=2
      IF(MPNCO.GT.0) MAXNCO2=4
C     MAXNCO2=MAXNCO1*2, doubled max number of discoincidencies
      NEXP=32/NWDVAR
      MASKL=2**NEXP-1
      LOONE=MOD(INTPACK,NWDVAR)
      LOONEY=LOONE+1
C     dodm0=ici1.eq.ici2.and.kartmax.eq.3.and..not.skipdm
      DODM0=SAMEMUL.AND.KARTMAX.EQ.3.AND..NOT.SKIPDM
C     dojz0=ici1.eq.ici2.and.kartmax.eq.3.and.jzopt.ne.0
      DOJZ0=SAMEMUL.AND.KARTMAX.EQ.3.AND.JZOPT.NE.0
      DODM=DODM0
      DOJZ=DOJZ0
      DO 25 KART=1,3
         SYLYES(KART)=.TRUE.
         SYRYES(KART)=.TRUE.
   25 CONTINUE
C
C     GET CSF INFO AND CI COEFS FOR THE LOWER SPIN STATE -I-
C
      CALL F17PNT(NFT17,ICI1,IW,NWKSST)
C
C     note that electronic occupations are stored in unpacked (ieconx)
C     and packed (iepack) arrays. The unpacked array is used only for
C     symmetry purposes so if no symmetry is to be used it is collapsed
C     into an array of 1 element
C
      IF(MPLEVL.NE.0) THEN
         EFC=ZERO
         DO I=1,NFZC
           EFC=EFC+2*EORB(I)
         ENDDO
      ENDIF
C
      NICSF=1
C     if(maswrk) write(6,*) 'i',NWKSI
      DO 30 ICSF=1,NWKSI
         NICSFC=NICSF
         IF(LAMECSF.NE.0) NICSFC=1
         READ(NFT17) IOPENX(NICSF), (ICASEX(ILEV,NICSF),ILEV=1,N1),
     *              (IECONX(IORB,NICSFC),IORB=1,N1),
     *              (COEFIX(IRT,NICSF),IRT=1,IROOTI)
         IF(MOD(ICSF,NPROC).EQ.ME) THEN
           IF(MPLEVL.NE.0) THEN
            ECONFIX(NICSF)=EFC+GETECSF(N1,EORB(NFZC+1),IECONX(1,NICSFC))
C
C           do i=1,irooti
C             if(econfix(nICSF).lt.eref0i(i).and.maswrk)
C    *          write(iw,8000) econfix(nICSF),eref0i(i),nICSF,i,ici1
C8000 FORMAT(1X,'WARN: EB=',F15.7,' <=',F15.7,' FOR CSF',I9,'STATE',I2,
C    *           'ICI',I2)
C           enddo
C
           ELSE
            ECONFIX(NICSF)=ZERO
           ENDIF
C          write(6,*) 'cfsi',nICSF,(IECONX(IORB,NICSFC),IORB=1,N1)
           CALL PACKIEC(N1,IECONX(1,NICSFC),IEPACK(1,NICSF))
           NICSF=NICSF+1
         ENDIF
   30 CONTINUE
      NICSF=NICSF-1
C
C     DEBUG PRINTOUT OF THE ORBITAL ORDERING, AND CI VECTOR
C
      IF(DBGSO) THEN
         WRITE(IW,*) 'ORBITAL ORDER FOR STATE 1'
         WRITE(IW,610) (IVCORB(K,ICI1),K=1,N1)
         WRITE(IW,*) 'ORBITAL ORDER FOR STATE 2'
         WRITE(IW,610) (IVCORB(K,ICI2),K=1,N1)
         WRITE(IW,*) 'THE CI VECTOR FOR STATE 1 IS'
         WRITE(IW,600) ((COEFIX(IRT,I),I=1,NWKSI),IRT=1,IROOTI)
      END IF
      IF(DBGSOMP) THEN
         WRITE(6,*) 'MCQDPT ORBITAL ENERGIES'
         DO I=1,NMO
            WRITE(6,*) I,EORB(I)
         ENDDO
         WRITE(6,*) 'MCQDPT CSF ENERGIES GROUP ',ICI1
         DO I=1,NWKSI
            WRITE(6,*) I,'STATE',ECONFIX(I)
         ENDDO
         WRITE(6,*) 'MCQDPT STATE ENERGIES GROUP ',ICI1
         DO I=1,IROOTI
            WRITE(6,*) I,'STATE',EREF0I(I)
         ENDDO
      ENDIF
C
C     LOOP OVER EACH CSF IN HIGHER SPIN STATE -J-
C     READ CSF INFO AND CI COEFFICIENT FOR 1 CSF AT A TIME
C
      IF(ICI1.NE.ICI2.OR.GOPARR) CALL F17PNT(NFT17,ICI2,IW,NWKSST)
C
C     otherwise will use bra arrays (iopenx etc). it is not
C     possible to use bra arrays divided over nodes in parallel
C
C     if(maswrk) write(6,*) 'j',NWKSJ
      IENDCSF=0
      DO 2000 JCSF=1,NWKSJ
         JCSFC=JCSF
         IF(LAMECSF.NE.0) JCSFC=1
         IF(ICI1.NE.ICI2.OR.GOPARR) THEN
           READ(NFT17) JOPEN, (JCASE(JLEV),JLEV=1,N1),
     *                 (JECONF(JORB),JORB=1,N1),
     *                 (COEFJ(JR),JR=1,IROOTJ)
           CALL VICLR(JEPACK,1,INTPACK)
           CALL PACKIEC(N1,JECONF,JEPACK)
         ELSE
           JOPEN=IOPENX(JCSF)
           CALL ICOPY(N1,ICASEX(1,JCSF),1,JCASE,1)
           CALL ICOPY(N1,IECONX(1,JCSFC),1,JECONF,1)
           CALL DCOPY(IROOTJ,COEFIX(1,JCSF),1,COEFJ,1)
           CALL ICOPY(INTPACK,IEPACK(1,JCSF),1,JEPACK,1)
         ENDIF
         COEFJMAX=COEFJ(IDAMAX(IROOTJ,COEFJ,1))
         IRRCSFJ=IRRCSF(N1,NFZC,JECONF)
         IF(MPLEVL.NE.0) THEN
            ECONFJ=EFC+GETECSF(N1,EORB(NFZC+1),JECONF)
C
C           do i=1,irootj
C             if(econfj.lt.eref0j(i).and.maswrk)
C    *          write(iw,8000) econfj,eref0j(i),jcsf,i,ici2
C           enddo
C
         ELSE
            ECONFJ=ZERO
         ENDIF
C        write(6,*) 'cfsj',jCSF,econfj
C
C             GET INDICES OF OPEN-SHELL ORBITALS OF THIS CSF IN -J-
C
         JPX = 0
         JBK = 0
         NPHJ = 1
         DO 40 J=1,N1
            JBK = JBK + N1234(JCASE(J))
            GO TO (40,42,42,44), JCASE(J)
C
   42       CONTINUE
            JPX=JPX+1
            JJCAS(JPX)=JCASE(J)
            GO TO 40
C
   44       CONTINUE
            JCHK = JBK/2
            JCHK = JBK - 2*JCHK
            IF(JCHK.EQ.1) NPHJ =-NPHJ
   40    CONTINUE
C
         IF(JOPEN.NE.JPX) THEN
            IF (MASWRK) THEN
               WRITE(IW,9001) JOPEN,JPX
               WRITE(IW,9002) ' JJCAS',(JJCAS(J),J=1,JPX)
               WRITE(IW,9002) ' JCASE',(JCASE(JLEV),JLEV=1,N1)
               WRITE(IW,9002) 'JECONF',(JECONF(JORB),JORB=1,N1)
            END IF
            CALL ABRT
         END IF
C
C             SPIN FUNCTION FOR THIS CSF IN -J-
C
         CALL VICLR(NPRJ,1,2)
         CALL SPNFNC(JOPEN,MULST(ICI2),JJCAS,MXSPIN,JPRIM(1,1,1),
     *               CGCJ(1,1),NPRJ(1),BUFFPK,NTRAP,BK)
         JS=1
         IF(ISOLOOP.EQ.0.AND..NOT.SAMEMUL.AND.MULST(ICI2).GT.1) THEN
C    *      .AND.(KARTMAX.EQ.3.or.mplevl.ne.0)) THEN
            JS=2
            CALL SMINUS(IW,JOPEN,JPRIM(1,1,1),CGCJ(1,1),NPRJ(1),
     *                  JPRIM(1,1,2),CGCJ(1,2),NPRJ(2),NTRAP,MXSPIN,
     *                  2**MXSPIN,DBGSO)
            IF(NPHJ.LT.0) CALL DSCAL(NPRJ(2),-ONE,CGCJ(1,2),1)
         ENDIF
C
         IF(NPHJ.LT.0) CALL DSCAL(NPRJ(1),-ONE,CGCJ(1,1),1)
         IF(NFFBUF2.NE.0) THEN
           SYLYESG(1)=.FALSE.
           SYLYESG(2)=.FALSE.
           SYLYESG(3)=.FALSE.
           CALL VICLR(NFF2,1,NMOACT*NMOACT)
           CALL VCLR(FFBUF1,1,MXRT*NMOACT*NMOACT)
         ENDIF
         IF(HERMI0) THEN
C
C           Use Hermiticity! Or anti-Hermiticity. whatever.
C           The tricky part in parallel runs is to find if icsf==jcsf,
C           to avoid double contribution from the diagonal.
C           As icsf is divided over nodes, iendcsf dwiddling is tricky.
C           Do not change unless understand perfectly or better.
C
            HERMI=MOD(JCSF,NPROC).NE.ME
            IF(.NOT.HERMI) IENDCSF=IENDCSF+1
C
C           i.e. do only icsf=1,jcsf lower triangle (divided over nodes)
C
         ELSE
C
C           hermi excludes the diagonal for the case when hermiticity
C           (hermi0) can be used.
C
            HERMI=.FALSE.
            IENDCSF=NICSF
C
C      nicsf has a fraction of this node's work (a band in the matrix)
C
         ENDIF
         SIMON=.TRUE.
C
C           LOOP FOR "EVERY" CSF IN THE LOWER SPIN STATE -I-
C
         DO 1000 ICSF=1,IENDCSF
C
C          this is a very heavily used part of the code.
C          filter out unwanted CSF pairs, which give zero contribution.
C
C          1) overall number of open shells (ie unpaired electrons)
C
           IF(ABS(IOPENX(ICSF)-JOPEN).GT.MAXNCO2) GOTO 1000
C
C          2) compare each open shell
C          HOW MANY NON-COINCIDENCES ARE THERE ?
C
           IF(LOONE.NE.0) THEN
             MATE=IEOR(IEPACK(1,ICSF),JEPACK(1))
             NCOIN=MARE(IAND(MATE,65535))+MARE(ISHFT(MATE,-16))
             IF(NCOIN.GT.MAXNCO2) GOTO 1000
           ELSE
             NCOIN=0
           ENDIF
           DO 80 I=LOONEY,INTPACK,NWDVAR
             IF(NWDVAR.EQ.2) THEN
               MATE=IEOR(IEPACK(I,ICSF),JEPACK(I))
               MAZE=IEOR(IEPACK(I+1,ICSF),JEPACK(I+1))
             ELSE
               MATE=IEOR(IEPACK(I,ICSF),JEPACK(I))
               MAZE=ISHFT(MATE,-32)
               MATE=IAND(MATE,MASKL)
             ENDIF
             NCOIN=NCOIN+MARE(IAND(MATE,65535))+MARE(ISHFT(MATE,-16))
     *                  +MARE(IAND(MAZE,65535))+MARE(ISHFT(MAZE,-16))
             IF(NCOIN.GT.MAXNCO2) GOTO 1000
   80      CONTINUE
C
C          3) absolute value filter
C
           IF(IROOTI.EQ.1) THEN
             COEFIMAX=COEFIX(1,ICSF)
           ELSE
             COEFIMAX=COEFIX(IDAMAX(IROOTI,COEFIX(1,ICSF),1),ICSF)
           ENDIF
           CIMAX=COEFIMAX*COEFJMAX
           IF(ABS(CIMAX).LT.SMALL) GOTO 1000
C
C          4) Point group symmetry filter
C
           ICSFC=ICSF
           IF(LAMECSF.NE.0) ICSFC=1
           IF(NOSYM1.LE.0) THEN
            IF(SIMON) THEN
             KIRMIN=KARTMIN
             KIRMAX=KARTMAX
             DOSOL=.FALSE.
             SYLYES(3)=.TRUE.
C            ICSFC=ICSF
C            IF(LAMECSF.NE.0) ICSFC=1
             IRRCSFI=IRRCSF(N1,NFZC,IECONX(1,ICSFC))
             DO 90 KART=1,3
   90          CALL CISOL(SYRYES(KART),IRRCSFI,IMS(KART),IRRCSFJ,IRRR,0)
             DO 95 KART=KARTMIN,KARTMAX
   95          CALL CISOL(SYLYES(KART),IRRCSFI,IMS(KART),IRRCSFJ,IRRL,0)
C            note the difference in selection rules:
C            Form-factor code operates in terms of L- and L+,
C            Zeff in terms of Lx and Ly separately
C            this is indicated by the last argument in cisol
             DODM=DODM0.AND.(SYRYES(1).OR.SYRYES(2).OR.SYRYES(3))
             DOJZ=DOJZ0.AND.SYLYES(3)
C            <iLz> has the same symmetry as the "SO" <Lz>
             DO 100 KART=KARTMIN,KARTMAX
               IF(SYLYES(KART)) THEN
                 DOSOL=.TRUE.
                 GOTO 105
               ELSE
                 KIRMIN=KIRMIN+1
               ENDIF
  100        CONTINUE
  105        CONTINUE
             DO 140 KART=KARTMAX,KIRMIN,-1
               IF(SYLYES(KART)) GOTO 145
               KIRMAX=KIRMAX-1
  140        CONTINUE
  145        CONTINUE
             IF(DODM.OR.DOJZ) THEN
               KIRMAX=3
               IF(KIRMIN.GT.KIRMAX) KIRMIN=3
C     even if SOC is zero by symmetry TM may need to be calculated.
C     In this case proceed also with Z component of SOC (*psi*).
             ENDIF
             SIMON=LAMECSF.EQ.0
C            simon governs if the symmetry analysis has to be done again
C            if lamecsf.ne.0 then it must be done only once because all
C            CSFs are of the same symmetry in a CI state
            ENDIF
           ELSE
             DOSOL=.TRUE.
             KIRMIN=KARTMIN
             KIRMAX=KARTMAX
           ENDIF
           IF(.NOT.DOSOL.AND..NOT.DODM.AND..NOT.DOJZ) GOTO 1000
C
C          if all filters fail, go on, do the calculation
C
           CALL VCLR(SOL1,1,3)
           IF(MPLEVL.NE.0) THEN
             CALL VCLR(SOL1MPI,1,MXRT*3)
             CALL VCLR(SOL1MPJ,1,MXRT*3)
           ENDIF
           CALL VCLR(SOL2,1,3)
           CALL VCLR(STM,1,3)
           SLZ=ZERO
           NFFI(1)=0
           NFFI(2)=0
           DO 150 ISS=KIRMIN,KIRMAX
 150         IF(SYLYES(ISS)) SYLYESG(ISS)=.TRUE.
C          write(6,*) 'Looping CSFs',icsf,jcsf
           CALL GETSOC(IW,KARTMIN0,KIRMIN,KIRMAX,IRRL,LAMECSF,
     *                 IOPENX(ICSF),IECONX(1,ICSFC),CIMAX,MULST(ICI1),
     *                 N1,N2,N2A,SO1MO,TMINT,ALZINT,SO2COR,
     *                 SO2ACT,EORB,ECONFIX(ICSF),ECONFJ,EREF0I,EREF0J,
     *                 VONEEL,VTWOEL,UONEEL,LIJMO,MPLEVL,NMO,NMOACT,
     *                 NMODOC,NMOEXT,NMOFZC,NMOEI,EDSHFT,IROOTI,IROOTJ,
     *                 FFAC,EZ2P,MAXNCO,MPNCO,IPRIM,JPRIM,CGCI,CGCJ,NPRI
     *                ,NPRJ,NTRAP,BK,BUFFPK,ICASEX(1,ICSF),JCASE,IICAS,
     *                 DBGSO,ICI1,ICI2,DEIG(LODEIG),IVCORB,MXSPIN,MXPRM,
     *                 SOL1,SOL1MPI,SOL1MPJ,SOL2,STM,SLZ,NAEL,DODM,DOJZ,
     *                 DIFVEX,SMALL1,COEFIMAX,COEFJMAX,COEFIX(1,ICSF),
     *                 COEFJ,SAMEMUL,JS,PQ,IQ,IQ1,EEQTOL,NEEQ,DMCORE,
     *                 FFBUFI1,FFBUFI2,INDFFI1,INDFFI2,NFFI(1),NFFI(2),
     *                 NFFBUFI1,NFFBUFI2,ISOLOOP)
C
C          Hermiticity being used to redeme the off-diagonal elements.
C          This is as cerebral furrow-deficient as the rest of the code,
C          a smart way is to include 1/2 into LOOPRM for icsf.eq.jcsf
C
C          do not add MP2 correction if doing form-factors
           MP2SA=0
           IF(NFFBUF2.EQ.0) MP2SA=MP2SO
C
C          sponsored by AT+T
C
           IF(ISOLOOP.EQ.0)
     *     CALL COLLECT(SOL1,SOL1MPI,SOL1MPJ,SOL2,STM,SLZ,IROOTI,IROOTJ,
     *                 SOL1MUL,SOL2MUL,STMMUL,ALZMUL,MXRT,COEFIX(1,ICSF)
     *                ,COEFJ,KIRMIN,KIRMAX,MAXNCO,SYLYES(1),SYRYES,DODM,
     *            DOJZ,MP2SA,DFAC,HERMI0.AND.(ICSF.NE.IENDCSF.OR.HERMI))
C    *                 ICI1.EQ.ICI2)
           IF(NFFBUFI1.NE.0) THEN
             CALL ADDNFF1(NFFI(1),IROOTI,NMOACT,MXRT,FFBUF1,
     *                    FFBUFI1,INDFFI1,COEFIX(1,ICSF),SMALL1)
           ENDIF
           IF(NFFBUFI2.NE.0) THEN
             CALL ADDNFF2(NFFBUF2,NFF2,NFFI(2),IROOTI,NMOACT,MXRT,FFBUF2
     *                 ,INDFF2,FFBUFI2,INDFFI2,COEFIX(1,ICSF),SMALL1)
           ENDIF
 1000    CONTINUE
C        surprisingly, SMALL1 in COLLMP2 does not give sufficient
C        accuracy  so it is divided by NMOACT.
C
         IF(NFFBUF2.NE.0)
     *   CALL COLLMP2(FFBUF1,NFFBUF2,NFF2,INDFF2,FFBUF2,KARTMIN,
     *                KARTMAX,IRRL,SYLYESG,EORB,ECONFJ,EREF0J,VONEEL,
     *                VTWOEL,UONEEL,LIJMO,NMO,NMOACT,NMODOC,NMOEXT,
     *                NMOFZC,NMOEI,EDSHFT,IROOTI,IROOTJ,SOL1MUL(1,1,1,2)
     *               ,SOL1MPJ,SMALL1/NMOACT,COEFJMAX,COEFJ,EEQTOL,NEEQ,
     *                DFAC,ISOLOOP,MAXNFF2)
 2000 CONTINUE
      IF(GOPARR.AND.(NFFBUF2.EQ.0.OR.ISOLOOP.NE.0)) THEN
        CALL DDI_GSUMF(2300,SOL1MUL,3*MXRT*MXRT*MHSO)
        CALL DDI_GSUMF(2301,SOL2MUL,3*MXRT*MXRT)
        CALL DDI_GSUMF(2303,STMMUL,3*MXRT*MXRT)
        IF(JZOPT.NE.0) CALL DDI_GSUMF(2304,ALZMUL,MXRT*MXRT)
      ENDIF
      RETURN
C
  600 FORMAT(1X,10F12.6)
  610 FORMAT(1X,40I3)
 9001 FORMAT(/1X,'JOPEN(',I5,' ) IS NOT EQUAL TO JPX(',I5,' ).')
 9002 FORMAT(1X,A6,'='/(1X,40I3))
 9210 FORMAT(/1X,'DEBUG: STATE NO., MULTIPLICITY, AND WALKS.',
     *       /8X,3I5,/8X,3I5)
C9300 FORMAT(/1X,'Stored ',I9,A5,' body form factors, nbuf ',I9,' max',
C    *           I9)
      END
C*MODULE SOZEFF  *DECK GETSOC
      SUBROUTINE GETSOC(IW,KMINI,KARTMIN,KARTMAX,IRRL,LAMECSF,IP,IECONF,
     *                 CIMAX,MULSTI,N1,N2,N2A,SO1MO,TMINT,ALZINT,SO2COR,
     *                  SO2ACT,EORB,ECONFI,ECONFJ,EREF0I,EREF0J,VONEEL,
     *                  VTWOEL,UONEEL,LIJMO,MPLEVL,NMO,NMOACT,NMODOC,
     *                  NMOEXT,NMOFZC,NMOEI,EDSHFT,IROOTI,IROOTJ,FFAC,
     *                  EZ2P,MAXNCO,MPNCO,IPRIM,JPRIM,CGCI,CGCJ,NPRI,
     *                  NPRJ,NTRAP,BK,BUFFPK,ICASE,JCASE,IICAS,OUT,ICI1,
     *                  ICI2,DEIG,IVCORB,MXSPIN,MXPRM,SOL1,SOL1MPI,
     *                  SOL1MPJ,SOL2,STM,SLZ,NAEL,DODM,DOJZ,DIFVEX,SMALL
     *                 ,COEFIMAX,COEFJMAX,COEFI,COEFJ,SAMEMUL,JS,PQ,IQ,
     *                  IQ1,EEQTOL,NEEQ,DMCORE,FFBUF1,FFBUF2,INDFF1,
     *                  INDFF2,NFF1,NFF2,NFFBUF1,NFFBUF2,ISOLOOP)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL OUT,GOPARR,DSKWRK,MASWRK,DIFVEX,DODM,DOJZ,SAMEMUL,DODIAG
C
      PARAMETER (MXATM=500,ONE=1.0D+00)
C
      DIMENSION SO1MO(NMO,NMO,3),TMINT(N1,N1,3),ALZINT(N1,N1),N1234(4),
     *          SO2COR(NMO,NMO,3),SO2ACT(N2,N2A,3),EORB(*),EREF0I(*),
     *          IECONF(*),EREF0J(*),VONEEL(NMO,*),VTWOEL(NMOEI,*),
     *          UONEEL(NMO,NMO,3),LIJMO(NMO,*),FFAC(0:1,0:1,0:1,0:1),
     *          EZ2P(0:1,0:1,0:1,0:1),IPRIM(MXSPIN,MXPRM,2),JPRIM(MXSPIN
     *         ,MXPRM,2),CGCI(MXPRM,2),CGCJ(MXPRM,2),NPRI(2),NPRJ(2),
     *          NTRAP(*),BK(*),BUFFPK(MXSPIN,*),ICASE(N1),JCASE(N1),
     *          IICAS(MXSPIN),DEIG(*),IVCORB(NUM,*),SOL1(3),IRRL(3),
     *          SOL1MPI(MXRT,*),SOL1MPJ(MXRT,*),SOL2(3),STM(3),COEFI(*),
     *          COEFJ(*),PQ(N1,N1),IQ(N1,N1),IQ1(N1,N1),DMCORE(3),
     *          FFBUF1(NFFBUF1),FFBUF2(NFFBUF2),INDFF1(NFFBUF1),
     *          INDFF2(NFFBUF2)
C
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /FMCOM / X(1)
C
      DATA N1234/ 0, 1, -1, 0 /
C
C     Prepare the CSF decomposition information for a pair of CSFs.
C     ----- S.O.C. CONTRIBUTION FOR CSF -J- WITH CSF -I- -----
C     WE ALREADY HAVE INFORMATION FOR CSF -J-, NEED TO GET -I-
C
      IPX = 0
      IBK = 0
      NPHI = 1
      DO 10 I=1,N1
       IBK = IBK + N1234(ICASE(I))
       GO TO (10,12,12,14), ICASE(I)
C
   12  CONTINUE
       IPX=IPX+1
       IICAS(IPX)=ICASE(I)
       GO TO 10
C
   14  CONTINUE
         ICHK = IBK/2
         ICHK = IBK - 2*ICHK
         IF(ICHK.EQ.1) NPHI =-NPHI
   10  CONTINUE
C
      IF(IP.NE.IPX) THEN
         IF (MASWRK) THEN
            WRITE(IW,9011) IP,IPX
            WRITE(IW,9012) (IICAS(I),I=1,IPX)
            WRITE(IW,9012) (ICASE(ILEV),ILEV=1,N1)
         END IF
         CALL ABRT
      END IF
C
C     GET IPRIM, CGCI, NPRI:
C     ---------------------
C     CALL VICLR(NPRI(1),1,2)
      CALL SPNFNC(IP,MULSTI,IICAS,MXSPIN,IPRIM(1,1,1),CGCI(1,1),NPRI(1),
     *            BUFFPK,NTRAP,BK)
C     IF(SAMEMUL.AND.MULSTI.GT.1.AND.KARTMIN.LT.3.and.mplevl.eq.0) THEN
      IS=1
      IF(ISOLOOP.NE.0.AND..NOT.SAMEMUL.AND.MULSTI.GT.1) THEN
         IS=2
         CALL SMINUS(IW,IP,IPRIM(1,1,1),CGCI(1,1),NPRI(1),IPRIM(1,1,2),
     *               CGCI(1,2),NPRI(2),NTRAP,MXSPIN,2**MXSPIN,OUT)
         IF(NPHI.LT.0) CALL DSCAL(NPRI(2),-ONE,CGCI(1,2),1)
      ENDIF
C
      IF(NPHI.LT.0) CALL DSCAL(NPRI(1),-ONE,CGCI(1,1),1)
C
C     IPRIM,CGCI,NPRI = PRIMITIVE SPIN FUNCTIONS + THOSE COEFFICIENTS
C     JPRIM,CGCJ,NPRJ = PRIMITIVE SPIN FUNCTIONS + THOSE COEFFICIENTS
C
C ... OBTAIN SPIN-ORBIT COUPLING CONSTANTS FOR EACH MS :
C
C     difvex?
      DODIAG=(ICI1.NE.ICI2.AND.NUMVEC.EQ.2).OR.DODM.OR.DOJZ
C     is=1
C     if(.NOT.SAMEMUL.and.isoloop.ne.0) is=2
C     JS=1
C     IF(.NOT.SAMEMUL.and.isoloop.eq.0) JS=2
      MS=0
      NAB=NFZC+N1
      I0=NFZC+1
      CALL VALFM(LOADFM)
      LICSF = LOADFM + 1
      LJCSF = LICSF  + (NAEL-1)/NWDVAR+1
      LIAB  = LJCSF  + (NAEL*NPRJ(JS)-1)/NWDVAR+1
      LICSFS= LIAB   + (2*NAB+1-1)/NWDVAR+1
      LJCSFS= LICSFS + (NAEL-1)/NWDVAR+1
      LJMOD = LJCSFS + (NAEL*NPRJ(JS)-1)/NWDVAR+1
      LAST  = LJMOD  + (NPRJ(JS)-1)/NWDVAR+1
      NEED = LAST - LOADFM
      CALL GETFM(NEED)
      CALL LOOPRM(KMINI,KARTMIN,KARTMAX,IRRL,LAMECSF,IECONF,ICASE,
     *            IPRIM(1,1,IS),CGCI(1,IS),NPRI(IS),JCASE,JPRIM(1,1,JS),
     *            CGCJ(1,JS),NPRJ(JS),CIMAX,SO1MO,TMINT,ALZINT,SO2COR,
     *            SO2ACT,SO2ACT,EORB,ECONFI,ECONFJ,EREF0I,EREF0J,VONEEL,
     *            VTWOEL,UONEEL,LIJMO,MPLEVL,NMO,NMOACT,NMODOC,NMOEXT,
     *            NMOFZC,NMOEI,EDSHFT,IROOTI,IROOTJ,FFAC,EZ2P,N1,N2,N2A,
     *            MAXNCO,MPNCO,DEIG,IVCORB(1,ICI1),IVCORB(1,ICI2),DIFVEX
     *           ,MXSPIN,NAEL,I0,NAB,DODM,DOJZ,X(LICSF),X(LJCSF),X(LIAB)
     *           ,X(LICSFS),X(LJCSFS),X(LJMOD),SOL1,SOL1MPI,SOL1MPJ,SOL2
     *           ,STM,SLZ,SMALL,COEFIMAX,COEFJMAX,COEFI,COEFJ,DODIAG,PQ,
     *            IQ,IQ1,EEQTOL,NEEQ,DMCORE,FFBUF1,FFBUF2,INDFF1,INDFF2,
     *            NFF1,NFF2,NFFBUF1,NFFBUF2,OUT)
      CALL RETFM(NEED)
C  50 CONTINUE
      IF(OUT) WRITE(IW,9050) MS,(SOL1(I),I=1,3),(SOL2(I),I=1,3),
     *                          (STM(I),I=1,3)
      RETURN
 9011 FORMAT(/1X,'IP(',I5,' ) IS NOT EQUAL TO IPX(',I5,' ).'
     *      //1X,'CHECK YOUR $DRT AND $TRNSTN GROUPS.')
 9012 FORMAT(1X,40I3)
 9050 FORMAT(5X,'LOOP=',I2,' : (X1,Y1,Z1)=',3F12.4/,'(X2,Y2,Z2)',3F12.4,
     *       /'(TX,TY,TZ)',3F12.4/)
      END
C*MODULE SOZEFF  *DECK LOOPRM
      SUBROUTINE LOOPRM(KMINI,KARTMIN,KARTMAX,IRRL,LAMECSF,IECONF,ICASE,
     *                  IPRIM,CGCI,NPRI,JCASE,JPRIM,CGCJ,NPRJ,CIPROD,
     *                  SO1MO,TMINT,ALZINT,SO2COR,SO2ACT,SO2ACT4,EORB,
     *                  ECONFI,ECONFJ,EREF0I,EREF0J,VONEEL,VTWOEL,UONEEL
     *                 ,LIJMO,MPLEVL,NMO,NMOACT,NMODOC,NMOEXT,NMOFZC,
     *                  NMOEI,EDSHFT,IROOTI,IROOTJ,FFAC,EZ2P,N1,N2,N2A,
     *                  MAXNCO,MPNCO,DEIG,IVCORBI,IVCORBJ,DIFVEX,MXSPIN,
     *                  NAEL,I0,NAB,DODM,DOJZ,ICSFSV,JCSFSV,IAB,ICSF,
     *                  JCSF,JMOD,SOL1,SOL1MPI,SOL1MPJ,SOL2,STM,SLZ,
     *                  SMALL,COEFIMAX,COEFJMAX,COEFI,COEFJ,DODIAG,PQ,
     *                  IQ,IQ1,EEQTOL,NEEQ,DMCORE,FFBUF1,FFBUF2,INDFF1,
     *                  INDFF2,NFF1,NFF2,NFFBUF1,NFFBUF2,SOME)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME,DIFVEX,DODM,DOJZ,DODIAG
      PARAMETER (HALF=0.5D+00,ONE=1.0D+00,TWO=2.0D+00)
C
      DIMENSION IRRL(3),ICASE(N1),IPRIM(MXSPIN,*),CGCI(*),JCASE(N1),
     *          JPRIM(MXSPIN,*),CGCJ(*),SO1MO(NMO,NMO,3),TMINT(N1,N1,3),
     *        SO2COR(NMO,NMO,KMINI:KARTMAX),SO2ACT(N2,N2A,KMINI:KARTMAX)
     *         ,ALZINT(N1,N1),SO2ACT4(N1,N1,N1,N1,KMINI:KARTMAX),EORB(*)
     *         ,EREF0I(*),EREF0J(*),VONEEL(NMO,*),VTWOEL(NMOEI,*),
     *          UONEEL(NMO,NMO,3),LIJMO(NMO,*),FFAC(0:1,0:1,0:1,0:1),
     *          EZ2P(0:1,0:1,0:1,0:1),DEIG(*),IVCORBI(*),IVCORBJ(*),
     *          ICSFSV(NAEL),JCSFSV(NAEL,NPRJ),IAB(-NAB:NAB),ICSF(NAEL),
     *          JCSF(NAEL,NPRJ),JMOD(NPRJ),SOL1(3),SOL1MPI(MXRT,*),
     *          SOL1MPJ(MXRT,*),SOL2(3),STM(3),PQ(N1,N1),IQ(N1,N1),
     *          IQ1(N1,N1),COEFI(*),COEFJ(*),IECONF(*),DMCORE(3),
     *          FFBUF1(*),FFBUF2(*),INDFF1(*),INDFF2(*)
C
      COMMON /IOFILE/ IR,IW,IDUMY(5),IODA(400)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
C
C     calculate matrix elements between two CSFs
C
      N3=N1*N1
C
      INIDOC=NMOFZC+1
      LASDOC=NMOFZC+NMODOC
      INIACT=NMOFZC+NMODOC+1
      INIEXT=NMOFZC+NMODOC+NMOACT+1
      LASEXT=NMOFZC+NMODOC+NMOACT+NMOEXT
C
C     save J determinants
      DO 900 JPR=1,NPRJ
        CALL MAKDET(N1,JCASE,JPRIM(1,JPR),IVCORBJ,JCSFSV(1,JPR),NSORBJ)
        IF(NSORBJ.NE.NAEL) THEN
          WRITE(IW,*) 'MISMATCH',NSORBJ,NAEL
          CALL ABRT
        ENDIF
  900 CONTINUE
      CALL VICLR(JMOD,1,NPRJ)
      MAXNCO1=MAXNCO
      IF(MAXNCO.EQ.3) MAXNCO1=1
      IF(MPNCO.GT.0) MAXNCO1=2
C
C     If doing SO-MCQDPT, one combines perturbation due to SOC with the
C     one due to the electron correlation, giving rise to 0,1,2,3 body
C     terms in the second order (first order is equivalent to SO-CAS,
C     and for this we can retain both 1 and 2e SOC). For the second
C     order, 0-body terms are zero. At present 3 body terms arising
C     from 2e SOC are neglected, which corresponds to a 1e SOC theory.
C     2 body terms couple 2 electrons, even for 1e SOC perturbation.
C     Only linear in SOC 1 and 2 body terms are retained.
C
C ... LOOP OVER PRIMITIVE FUNCTIONS OF STATE I.
C
      DO 1000 IPR=1,NPRI
C
C ...  MAKE SLATER DETERMINANTS: ALPHA(+), BETA(-)
C
       CALL MAKDET(N1,ICASE,IPRIM(1,IPR),IVCORBI,ICSFSV,NSORBI)
       CALL VICLR(IAB(I0),1,N1)
       CALL VICLR(IAB(-NAB),1,N1)
       DO 80 I=1,NSORBI
         IAB(ICSFSV(I))=1
   80  CONTINUE
       IMOD=0
C
C ...  LOOP OVER PRIMITIVE FUNCTIONS OF STATE J.
C
       DO 1100 JPR=1,NPRJ
C
C ... REORDERING OF CONFIGURATIONS, JCSF AND ICSF.
C     NC = NUMBER OF NON-COINCIDENCES
C           1 2 3 ... (NSORB-NC)   ...  NSORB
C     ICSF: <------ SAME ------> <-- DIFF -->
C     JCSF: <------ SAME ------> <-- DIFF -->
        NSORB =NSORBI
        IF(IMOD.EQ.0) THEN
           CALL ICOPY(NSORB,ICSFSV,1,ICSF,1)
           IMOD=1
        ENDIF
        IF(JMOD(JPR).EQ.0) THEN
           CALL ICOPY(NSORB,JCSFSV(1,JPR),1,JCSF(1,JPR),1)
           JMOD(JPR)=1
        ENDIF
      CALL CSFREO(MAXNCO1,NSORB,ICSF,JCSF(1,JPR),NAB,IAB,NPHASE,NC,SOME)
        IF(NC.LE.MAXNCO1) THEN
           IMOD=0
           JMOD(JPR)=0
C    in this case csfreo destroys ICSF and JCSF
C    it would have messed up the phase next time IPR or JPR gets used
C    so the arrays will be copied from the "saved" area (when needed)
        ENDIF
C
        IF(NC.GT.MAXNCO1) GO TO 1100
C
C ...   CALCULATE OVERLAP BETWEEN TWO CSF'S.
        OVER = ONE
        IF(DIFVEX) THEN
           MAXIE = NSORB -NC
           DO 20 IE=1,MAXIE
              IORB = ABS(ICSF(IE))-NFZC
              OVER = OVER*DEIG(IORB)
   20      CONTINUE
C          IF(OVER.LT.SMALL) THEN
C            IF (MASWRK) WRITE(IW,9030) OVER
C9030 FORMAT(/1X,'ERROR AT 9030 IN LOOPRM: OVERLAP=',F20.10)
C            CALL ABRT
C          END IF
        END IF
C
C ...   WEIGHT FACTOR:
        SUMFAC = CGCI(IPR) * CGCJ(JPR) * NPHASE * OVER
        IF(SOME) WRITE(6,*)'VVV',IPR,JPR,CGCI(IPR),CGCJ(JPR),OVER,SUMFAC
        IF(ABS(SUMFAC*CIPROD).LT.SMALL) GO TO 1100
C
C ...   BRANCHING FOLLOWED BY NUMBER OF NON-COINCIDENCES.
        NCP1=NC+1
        GO TO (1200,1300,1400),NCP1
        WRITE(IW,*) 'TOO MANY DISCOS'
        CALL ABRT
C
C     =====================
C ... ZERO NON-COINCIDENCE:
C     =====================
 1200 CONTINUE
      IF(.NOT.DODIAG.AND.MPLEVL.EQ.0) GOTO 1100
C
C     SOC ME between identical determinants (numvec=1!!) is zero
C
C ... 1E PART >>>
      IF(DODM) THEN
C       add the core dipole contribution
        DO I=1,3
          STM(I)=STM(I)+DMCORE(I)*SUMFAC/OVER
        ENDDO
      ENDIF
      DO 1210 IE=1,NSORB
         I1 = IABS(ICSF(IE)) -NFZC
C
C        transition moment... minus comes from the electron charge.
         IF(DODM) CALL DAXPY(3,-SUMFAC,TMINT(I1,I1,1),N3,STM,1)
C        IF(DODM) CALL DAXPY(3,-SUMFAC/DEIG(I1),TMINT(I1,I1,1),N3,STM,1)
         IF(DOJZ) SLZ=SLZ+ALZINT(I1,I1)*SUMFAC/DEIG(I1)
         IF(NUMVEC.EQ.1.AND.MPLEVL.EQ.0) GOTO 1210
C
C ...    DOUBLY OCCUPIED ORBITALS DO NOT CONTRIBUTE.
C        lamecsf.eq.0 means IECONF is available
C        if it is available, only not doubly occ. dets contribute
C        note that jconf is never used as bra and ket dets are the
C        same for zero number of discoincidences
C
         IX=3
         IF(LAMECSF.NE.0.OR.IECONF(I1).NE.2) THEN
         FAC= HALF*SUMFAC/DEIG(I1)
         IF(ICSF(IE).LT.0) FAC = -FAC
         IF(KARTMAX.LT.IX) GOTO 1210
         SOL1(IX) = SOL1(IX)+FAC*SO1MO(I1+NFZC,I1+NFZC,IX)
         IF(MAXNCO.GT.1) THEN
           SOL2(IX)=SOL2(IX)+FAC*SO2COR(I1+NFZC,I1+NFZC,IX)*TWO
C     2 because sum all sum all -> sum act sum core + sum core sum act
         ENDIF
         IF(MPLEVL.GT.0) THEN
C          SOL1(i) = SOL1(i) + SO-MRMP2(i) for each state
C          ket-dependent (j) <S||S'>, S'>=S
           IF(NFFBUF1.EQ.0) THEN
           CALL SO1MRMP2(SOL1MPJ(1,IX),I1+NFZC,I1+NFZC,FAC,COEFIMAX,
     *                   COEFJMAX,SMALL,ECONFJ,EREF0J,COEFJ,IROOTJ,
     *                   INIDOC,LASDOC,INIACT,INIEXT,LASEXT,NMOEI,NMO,
     *                   EORB,VONEEL,VTWOEL,UONEEL(1,1,IX),LIJMO,EDSHFT
     *                  ,IRRL(IX))
C          bra-dependent (i) <S'||S>, S'>=S
C          FAC is the same as above since Ezpq does not mix Ms values.
           CALL SO1MRMP2(SOL1MPI(1,IX),I1+NFZC,I1+NFZC,FAC,COEFJMAX,
     *                   COEFIMAX,SMALL,ECONFI,EREF0I,COEFI,IROOTI,
     *                   INIDOC,LASDOC,INIACT,INIEXT,LASEXT,NMOEI,NMO,
     *                   EORB,VONEEL,VTWOEL,UONEEL(1,1,IX),LIJMO,EDSHFT
     *                  ,IRRL(IX))
           ELSE
              IF(ABS(FAC).GT.SMALL) CALL ADDFF(1,NFFBUF1,NFF1,FFBUF1,
     *                                      INDFF1,I1,I1,0,0,FAC,SMALL)
           ENDIF
         ENDIF
         ENDIF
C ... 2E PART >>>
         IF(MAXNCO1.EQ.2) THEN
           IES=ICSF(IE)
           IS=0
           IF(IES.LT.0) IS=1
           DO 1280 JE=1,NSORB
            JES=ICSF(JE)
            J1=ABS(JES)-NFZC
            IF(LAMECSF.EQ.0.AND.IECONF(I1).EQ.2.AND.
     *         IECONF(J1).EQ.2) GOTO 1280
            JS=0
            IF(JES.LT.0) JS=1
            FAC=SUMFAC/DEIG(I1)/DEIG(J1)
            IF(MAXNCO.EQ.2.AND.NUMVEC.EQ.2) THEN
            SOL2(IX)=SOL2(IX)+(SO2ACT4(I1,I1,J1,J1,IX)*FFAC(IS,IS,JS,JS)
     *                        +SO2ACT4(J1,J1,I1,I1,IX)*FFAC(JS,JS,IS,IS)
     *                        -SO2ACT4(I1,J1,J1,I1,IX)*FFAC(IS,JS,JS,IS)
     *                        -SO2ACT4(J1,I1,I1,J1,IX)*FFAC(JS,IS,IS,JS)
     *                       )*FAC
            ENDIF
            IF(MPLEVL.GT.0) THEN
C             we get all 4 combinations here too.
              IF(NFFBUF2.EQ.0) THEN
              CALL SO2EMRMP2(SOL1MPI(1,IX),SOL1MPJ(1,IX),I1+NFZC,I1+NFZC
     *                      ,J1+NFZC,J1+NFZC,IS,IS,JS,JS,EZ2P,FAC,
     *                      COEFIMAX,COEFJMAX,SMALL,ECONFI,ECONFJ,EREF0I
     *                      ,EREF0J,COEFI,COEFJ,IROOTI,IROOTJ,NMOEI,NMO,
     *                       EORB,VTWOEL,UONEEL(1,1,IX),LIJMO,EDSHFT,
     *                       IRRL(IX),EEQTOL,NEEQ)
              ELSE
                 CALL SO2FMRMP2(I1,I1,J1,J1,IS,IS,JS,JS,EZ2P,FAC,
     *                          FFBUF2,INDFF2,NFF2,NFFBUF2,SMALL)
              ENDIF
            ENDIF
 1280      CONTINUE
         ENDIF
 1210 CONTINUE
      GO TO 1100
C
C     ====================
C ... ONE NON-COINCIDENCE:
C     ====================
C ... 1E PART >>>
 1300 CONTINUE
      NONCI = ICSF(NSORB)
      NONCJ = JCSF(NSORB,JPR)
      IF(NONCI*NONCJ.LT.0) THEN
         WRITE(6,*) 'NONSTOP'
         CALL ABRT
      ENDIF
      I2 = ABS(NONCI) -NFZC
      J2 = ABS(NONCJ) -NFZC
      FAC = HALF*SUMFAC
      IF(NONCI.LT.0) FAC = -FAC
C     do i=1,3
      DO I=KARTMIN,KARTMAX
C       at present active-active 1e integrals are stored for all orbs
        SOL1(I) = SOL1(I)+FAC*SO1MO(I2+NFZC,J2+NFZC,I)
        IF(MAXNCO.GT.1) THEN
          SOL2(I)=SOL2(I)+FAC*SO2COR(I2+NFZC,J2+NFZC,I)
        ENDIF
        IF(MPLEVL.GT.0) THEN
          IF(NFFBUF1.EQ.0) THEN
C         ket-dependent (j) <S||S'>, S'>=S
          CALL SO1MRMP2(SOL1MPJ(1,I),I2+NFZC,J2+NFZC,FAC,COEFIMAX,
     *                  COEFJMAX,SMALL,ECONFJ,EREF0J,COEFJ,IROOTJ,
     *                  INIDOC,LASDOC,INIACT,INIEXT,LASEXT,NMOEI,NMO,
     *                  EORB,VONEEL,VTWOEL,UONEEL(1,1,I),LIJMO,EDSHFT,
     *                  IRRL(I))
C         bra-dependent (i) <S'||S>, S'>=S
C         FAC is the same as above since Ezpq does not mix Ms values.
          CALL SO1MRMP2(SOL1MPI(1,I),J2+NFZC,I2+NFZC,FAC,COEFJMAX,
     *                  COEFIMAX,SMALL,ECONFI,EREF0I,COEFI,IROOTI,
     *                  INIDOC,LASDOC,INIACT,INIEXT,LASEXT,NMOEI,NMO,
     *                  EORB,VONEEL,VTWOEL,UONEEL(1,1,I),LIJMO,EDSHFT,
     *                  IRRL(I))
           ELSE
C             here only FFs are stored, and these do not depend upon I
              IF(I.EQ.KARTMIN.AND.ABS(FAC).GT.SMALL)
     *     CALL ADDFF(1,NFFBUF1,NFF1,FFBUF1,INDFF1,I2,J2,0,0,FAC,SMALL)
           ENDIF
        ENDIF
      ENDDO
C
C        Transition moment...
      IF(DODM) CALL DAXPY(3,-SUMFAC,TMINT(I2,J2,1),N3,STM,1)
      IF(DOJZ) SLZ=SLZ+ALZINT(I2,J2)*SUMFAC
C
C ... 2E PART >>>
      IF(MAXNCO1.EQ.2) THEN
        I2S=0
        IF(NONCI.LT.0) I2S=1
        J2S=0
        IF(NONCJ.LT.0) J2S=1
C       sum to N-1 because for N each det has identical columns - zero
        DO 1340 ID=1,NSORB-1
         II=ICSF(ID)
         I=ABS(II)-NFZC
         IS=0
         IF(II.LT.0) IS=1
         IF(MAXNCO.EQ.2) THEN
          IF(NUMVEC.EQ.2) THEN
           DO 1310 IX=KARTMIN,KARTMAX
 1310       SOL2(IX)=SOL2(IX)+(SO2ACT4(I,I,I2,J2,IX)*FFAC(IS,IS,I2S,J2S)
     *                        +SO2ACT4(I2,J2,I,I,IX)*FFAC(I2S,J2S,IS,IS)
     *                        -SO2ACT4(I,J2,I2,I,IX)*FFAC(IS,J2S,I2S,IS)
     *                        -SO2ACT4(I2,I,I,J2,IX)*FFAC(I2S,IS,IS,J2S)
     *                        )*SUMFAC/DEIG(I)
          ELSE
           DO 1320 IX=KARTMIN,KARTMAX
 1320       SOL2(IX)=SOL2(IX)+(
     *      +PQ(I2,J2)*SO2ACT(IQ(I2,J2),IQ1(I,I),IX)*FFAC(I2S,J2S,IS,IS)
     *      -PQ(I,J2)* SO2ACT(IQ(I,J2),IQ1(I2,I),IX)*FFAC(IS,J2S,I2S,IS)
     *      -PQ(I2,I)* SO2ACT(IQ(I2,I),IQ1(I,J2),IX)*FFAC(I2S,IS,IS,J2S)
     *                        )*SUMFAC
          ENDIF
         ENDIF
         IF(MPLEVL.GT.0) THEN
          FAC=SUMFAC/DEIG(I)
C         DO 1330 IX=1,3
          IF(NFFBUF2.EQ.0) THEN
          DO 1330 IX=KARTMIN,KARTMAX
            CALL SO2EMRMP2(SOL1MPI(1,IX),SOL1MPJ(1,IX),I+NFZC,I+NFZC,
     *                     I2+NFZC,J2+NFZC,IS,IS,I2S,J2S,EZ2P,FAC,
     *                     COEFIMAX,COEFJMAX,SMALL,ECONFI,ECONFJ,EREF0I,
     *                     EREF0J,COEFI,COEFJ,IROOTI,IROOTJ,NMOEI,NMO,
     *                     EORB,VTWOEL,UONEEL(1,1,IX),LIJMO,EDSHFT,
     *                     IRRL(IX),EEQTOL,NEEQ)
 1330     CONTINUE
          ELSE
            CALL SO2FMRMP2(I,I,I2,J2,IS,IS,I2S,J2S,EZ2P,FAC,
     *                     FFBUF2,INDFF2,NFF2,NFFBUF2,SMALL)
          ENDIF
         ENDIF
 1340   CONTINUE
      ENDIF
      GO TO 1100
C
C     =====================
C ... TWO NON-COINCIDENCES:
C     =====================
 1400 CONTINUE
C ... 1E PART >>> NO CONTRIBUTION
C ... transition moment >>> no contribution
      KK = ICSF(NSORB-1)
      MM = ICSF(NSORB)
      LL = JCSF(NSORB-1,JPR)
      NN = JCSF(NSORB,JPR)
C     only 0 or 1 spin disco allowed, but this is not used
      K=ABS(KK)-NFZC
      KS=0
      IF(KK.LT.0) KS=1
      L=ABS(LL)-NFZC
      LS=0
      IF(LL.LT.0) LS=1
      M=ABS(MM)-NFZC
      MMS=0
      IF(MM.LT.0) MMS=1
      N=ABS(NN)-NFZC
      NS=0
      IF(NN.LT.0) NS=1
      IF(MAXNCO.EQ.2) THEN
      IF(NUMVEC.EQ.2) THEN
      DO 1500 IX=KARTMIN,KARTMAX
       SOL2(IX)=SOL2(IX)+( SO2ACT4(K,L,M,N,IX)*FFAC(KS,LS,MMS,NS)
     *                    +SO2ACT4(M,N,K,L,IX)*FFAC(MMS,NS,KS,LS)
     *                    -SO2ACT4(K,N,M,L,IX)*FFAC(KS,NS,MMS,LS)
     *                   -SO2ACT4(M,L,K,N,IX)*FFAC(MMS,LS,KS,NS))*SUMFAC
 1500 CONTINUE
      ELSE
      DO 1600 IX=KARTMIN,KARTMAX
       SOL2(IX)=SOL2(IX)+(
     *    PQ(K,L)*SO2ACT(IQ(K,L),IQ1(M,N),IX)*FFAC(KS,LS,MMS,NS)
     *   +PQ(M,N)*SO2ACT(IQ(M,N),IQ1(K,L),IX)*FFAC(MMS,NS,KS,LS)
     *   -PQ(K,N)*SO2ACT(IQ(K,N),IQ1(M,L),IX)*FFAC(KS,NS,MMS,LS)
     *   -PQ(M,L)*SO2ACT(IQ(M,L),IQ1(K,N),IX)*FFAC(MMS,LS,KS,NS))*SUMFAC
 1600 CONTINUE
      ENDIF
      ENDIF
      IF(MPLEVL.GT.0) THEN
C     DO 1700 IX=1,3
        IF(NFFBUF2.EQ.0) THEN
          DO 1700 IX=KARTMIN,KARTMAX
        CALL SO2EMRMP2(SOL1MPI(1,IX),SOL1MPJ(1,IX),K+NFZC,L+NFZC,M+NFZC,
     *                 N+NFZC,KS,LS,MMS,NS,EZ2P,SUMFAC,COEFIMAX,COEFJMAX
     *                ,SMALL,ECONFI,ECONFJ,EREF0I,EREF0J,COEFI,COEFJ,
     *                IROOTI,IROOTJ,NMOEI,NMO,EORB,VTWOEL,UONEEL(1,1,IX)
     *                ,LIJMO,EDSHFT,IRRL(IX),EEQTOL,NEEQ)
 1700     CONTINUE
        ELSE
          CALL SO2FMRMP2(K,L,M,N,KS,LS,MMS,NS,EZ2P,SUMFAC,
     *                   FFBUF2,INDFF2,NFF2,NFFBUF2,SMALL)
        ENDIF
      ENDIF
C
 1100 CONTINUE
 1000 CONTINUE
C
      RETURN
      END
C*MODULE SOZEFF  *DECK CSFREO
      SUBROUTINE CSFREO(MAXNCO,NSORB,ICSF,JCSF,NAB,IAB,NPHASE,NC,OUT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL OUT
      DIMENSION ICSF(*),JCSF(*),IAB(-NAB:NAB)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
C
C     reorder the orbitals to shift discoincidencies to the end
C
      IF(OUT) THEN
         WRITE(IW,600) (ICSF(I),I=1,NSORB)
         WRITE(IW,600) (JCSF(J),J=1,NSORB)
      END IF
C
      NPHASE=1
C     quick check if two many discoincidencies
      NC=0
      DO 90 I=1,NSORB
         IF(IAB(JCSF(I)).EQ.0) THEN
            NC=NC+1
            IF(NC.GT.MAXNCO) RETURN
         ENDIF
   90 CONTINUE
      ITMP=0
      MTMP=NSORB
C
  100 CONTINUE
      ITMP = ITMP+1
      IF(ITMP.GT.MTMP) GO TO 510
      DO 110 JE=ITMP,NSORB
         IF(JCSF(JE).EQ.ICSF(ITMP)) GO TO 300
  110 CONTINUE
      IF(ITMP.EQ.MTMP) GO TO 500
C
C     NON-COINCIDENCE: MOVE THIS ORBITAL TO THE LAST POSITION IN ICSF
C
      ISAVE=ICSF(ITMP)
      DO 200 IE=ITMP,(MTMP-1)
         ICSF(IE)=ICSF(IE+1)
  200 CONTINUE
      ICSF(MTMP)=ISAVE
      ITMP = ITMP-1
      MTMP = MTMP-1
C
C     CHECK THE PHASE
C
      INDEX = (MTMP-ITMP)/2
      ICHK = MTMP -ITMP -2*INDEX
      IF(ICHK.EQ.1) NPHASE = -NPHASE
      GO TO 100
C
C     COINCIDENCE: THIS ORBITAL IS MOVED TO THE SAME POSITION IN JCSF.
C
  300 CONTINUE
      JTMP=JE
      IF(JTMP.EQ.ITMP) GO TO 100
C
      JSAVE=JCSF(JTMP)
      DO 310 JE=(JTMP-1),ITMP,-1
         JCSF(JE+1)=JCSF(JE)
  310 CONTINUE
      JCSF(ITMP)=JSAVE
C
C     CHECK THE PHASE
C
      INDEX = (JTMP-ITMP)/2
      ICHK = JTMP -ITMP -2*INDEX
      IF(ICHK.EQ.1) NPHASE = -NPHASE
      GO TO 100
C
  500 CONTINUE
      MTMP = MTMP -1
  510 CONTINUE
C     NC = NSORB - MTMP
C
C           1 2 3 ... MTMP   ...  NSORB
C     ICSF: <--- SAME ---> <-- DIFF -->
C     JCSF: <--- SAME ---> <-- DIFF -->
C
      IF(OUT) THEN
         WRITE(IW,601) NPHASE,NC
C        WRITE(6,600) (ICSF(I),I=1,NSORB)
C        WRITE(6,600) (JCSF(J),J=1,NSORB)
      END IF
      RETURN
C
  600 FORMAT(10X,30I3)
  601 FORMAT(1X,'PHASE =',I2,' :  NONCOINCEDENCES =',I3)
      END
C*MODULE SOZEFF  *DECK SPNDIA
      SUBROUTINE SPNDIA(N1,NHSO,NTDM,HSO,EIG,TDM,ALZ,PRTPRM,TMOMNT,MULST
     *                 ,IROOTS,ENGYST,L2VAL,MAXNCO,COPCON,COPCON1,OMEGA,
     *                  JZOPT,NZSPIN,MHSO,GLOLAB,PRTLZ,DBG)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL PRTPRM,TMOMNT,PRTLZ,DBG,GOPARR,DSKWRK,MASWRK
      COMPLEX*16 HSO(NHSO,NHSO)
      CHARACTER*6 GLOLAB
C
      DIMENSION EIG(*),TDM(3,NTDM,NTDM),ALZ(NTDM,NTDM),MULST(*),L2VAL(*)
     *         ,IROOTS(*),COPCON(*),COPCON1(*),ENGYST(MXRT,*),OMEGA(*)
      COMMON /FMCOM / H(1)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      CHARACTER*1 XYZ(3)
      DATA XYZ/'X','Y','Z'/
C
C     diagonalise the Hso matrix, output results
C
      LOADFM = 0
      CALL VALFM(LOADFM)
      LCWK =   1 + LOADFM
      LRWK = LCWK+ NHSO*4
      LID  = LRWK+ NHSO*3
      LAST = LID +(NHSO-1)/NWDVAR + 1
      IF(JZOPT.NE.0) THEN
         LAJZMAT= LAST
         LEIGJZ = LAJZMAT+NHSO*NHSO*2
         LID1   = LEIGJZ +NHSO
         LAST   = LID1   +(NHSO-1)/NWDVAR + 1
      ENDIF
      NEED = LAST-LOADFM
      CALL GETFM(NEED)
C
C     PUNCH OUT INFORMATION:
      IF(DBG) THEN
C     transition moment:
         WRITE(IW,*) 'THE LOWER TRIANGLE IS NOT PRINTED (SYMMETRICAL)'
         NCLM = 5
         DO 270 K=1,3
            WRITE(IW,9171) XYZ(K)
            DO 272 J=1,NTDM,NCLM
               WRITE(IW,9175) (L,L=J,MIN(J+NCLM-1,NTDM))
               DO 274 I=1,NTDM
             WRITE(IW,9176) I,(TDM(K,I,L),L=J,MIN(J+NCLM-1,NTDM))
  274          CONTINUE
  272       CONTINUE
  270    CONTINUE
      ENDIF
C
      IF(MASWRK) CALL HSORES(NHSO,HSO,EIG,H(LRWK),H(LCWK),IROOTS,MULST,
     *                       ENGYST,H(LID),MAXNCO,COPCON,COPCON1,PRTPRM,
     *                       NZSPIN,MHSO,GLOLAB)
C
C     ----- Calculate Jz eigenvalues for the spin-mixed states -----
C     hsores rs sets indx, not crucial print-out label array
      IF(JZOPT.NE.0) CALL SPNJZ(NHSO,NTDM,HSO,ALZ,H(LAJZMAT),H(LEIGJZ),
     *                          H(LRWK),H(LCWK),MULST,IROOTS,L2VAL,
     *                          H(LID1),OMEGA,GLOLAB,PRTPRM,PRTLZ,DBG)
C
C     ----- Calculate transition moments between spin-mixed states -----
      IF(TMOMNT) CALL SPNTMX(N1,NHSO,NTDM,HSO,EIG,TDM,H(LID),MULST,
     *                       IROOTS,GLOLAB)
C
C     ----- RESET FAST MEMORY -----
      CALL RETFM(NEED)
      CALL TEXIT(2,2)
      RETURN
 9171 FORMAT(/1X,'***** <',A1,'> *****')
 9175 FORMAT(/6X,   5(5X,I5,5X),/)
 9176 FORMAT( 1X,I3,5F15.6)
      END
C*MODULE SOZEFF  *DECK FORMF
      SUBROUTINE FORMF(FFAC,EZ2P)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO=0.0D+00,HALF=0.5D+00,ONE=1.0D+00,TWO=2.0D+00)
      DIMENSION FFAC(0:1,0:1,0:1,0:1),EZ2P(0:1,0:1,0:1,0:1)
C     DIMENSION FFAC(0:1,0:1,0:1,0:1,-1:1)
C
C     ms = -1,0,1
C     alpha 0, beta 1
C
C     2e-form factors for two pairs of orbitals,
C         [k(1)l(1)|S-ms(1)+2S-ms(2)|m(2)n(2)]=
C         <k(1)m(2)|S-ms(1)+2S-ms(2)|l(1)n(2)>
C
C     for the operator {L(1,2),ms}{S-ms(1)+2S-ms(2)}
C     i.e. L(1,2),ms = L(1,2)+, L(1,2)z or L(1,2)-
C               S-ms = S-, Sz or S+
C     exampli gratia ffac(0,1,0,1,-1)=
C                    <alp(1)alp(2)|S+(1)+2S+(2)|bet(1)bet(2)>
C     DO 100 MS=-1,1
      DO 100 MS=0,0
        DO 100 K=0,1
          DO 100 L=0,1
            DO 100 M=0,1
              DO 100 N=0,1
                F1=ZERO
                IF(M.EQ.N) THEN
                  IF(MS.EQ.0.AND.K+L.EQ.0) F1= HALF
                  IF(MS.EQ.0.AND.K+L.EQ.2) F1=-HALF
                  IF(MS.EQ. 1.AND.K.EQ.1.AND.L.EQ.0.OR.
     *               MS.EQ.-1.AND.K.EQ.0.AND.L.EQ.1) F1=ONE
                ENDIF
                F2=ZERO
                IF(K.EQ.L) THEN
                  IF(MS.EQ.0.AND.M+N.EQ.0) F2= HALF
                  IF(MS.EQ.0.AND.M+N.EQ.2) F2=-HALF
                  IF(MS.EQ. 1.AND.M.EQ.1.AND.N.EQ.0.OR.
     *               MS.EQ.-1.AND.M.EQ.0.AND.N.EQ.1) F2=ONE
                ENDIF
                FFAC(K,L,M,N)=F1+TWO*F2
C               FFAC(K,L,M,N,MS)=F1+TWO*F2
  100 CONTINUE
C     write(iw,*) '4-orbital 2-e form factors'
C     write(iw,*) (((((ffac(k,l,m,n,ms),k=0,1),l=0,1),m=0,1),
C    *            n=0,1),ms=-1,1)
      CALL VCLR(EZ2P,1,16)
C
C     1/2*<km|Ez(kl,mn)|ln>.
C     FFACs are linear combinations of two of these.
C     1/2 comes from the spin variable integration, <alpha|Sz|alpha>=1/2
C     (since Hso=(L*S) and S integration is not included into L
C     integrals).  It is also built into FFAC.
C
      EZ2P(0,0,0,0)= HALF
      EZ2P(0,0,1,1)= HALF
      EZ2P(1,1,0,0)=-HALF
      EZ2P(1,1,1,1)=-HALF
C     ez2p(0,0,0,0)= ONE
C     ez2p(0,0,1,1)= ONE
C     ez2p(1,1,0,0)=-ONE
C     ez2p(1,1,1,1)=-ONE
      RETURN
      END
C
C*MODULE SOZEFF  *DECK QINDEX
      SUBROUTINE QINDEX(N1,IDRAG,NDRAGS,PQ,IQ,IQ1)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ONE=1.0D+00)
      DIMENSION PQ(N1,N1),IQ(N1,N1),IQ1(N1,N1)
C
C     phases and indices for 2e integrals are calculated
C
      DO 200 J=1,N1
         DO 200 I=1,J
C
C           pq contains phase change for interchange of indices of
C           1st electron in 2e integrals (2nd electron is symmetric)
C           iq contains triangular matrix offsets
C
            PQ(J,I)=-ONE
            PQ(I,J)=ONE
C
C           iq has offsets for first particle in 2e integrals
C           iq1 has offsets for the second particle. If this array is
C           dragged, the indices for the part of the array outside of
C           drag will point to the last element of the array,
C           zeroed out before
C
            IQ(I,J)=I+(J*J-J)/2
            IQ(J,I)=IQ(I,J)
            IF(IQ(I,J).GE.IDRAG.AND.IQ(I,J).LT.IDRAG+NDRAGS) THEN
               IQ1(I,J)=IQ(I,J)-IDRAG+1
            ELSE
               IQ1(I,J)=NDRAGS+1
            ENDIF
            IQ1(J,I)=IQ1(I,J)
  200 CONTINUE
C     write(6,*) 'iq',((iq(i,j),i=1,n1),j=1,n1),'iq1',
C    *                ((iq1(i,j),i=1,n1),j=1,n1)
      RETURN
      END
C*MODULE SOZEFF  *DECK SETMARE
      SUBROUTINE SETMARE(N1PACK,MARE)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION MARE(0:2**N1PACK-1),M1234(0:3)
      DATA M1234/0,1,2,1/
C
C     mare contains the number of discoes given xor mask of two
C     orbital occupancies (16 bits or less)
C     In the simple case of 1 orbital m1234 array is the same as mare
C     ie. m1234(3) corresponds to xor=3D=11B, ie either 10-01 or 01-10
C     10 stands for occupation of 2 and 01 of 1 so this adds 1 disco
C
      NTOT=2**N1PACK-1
      DO 200 I=0,NTOT
         I1=I
         KOLT=0
  100    CONTINUE
            KOLT=KOLT+M1234(MOD(I1,4))
            I1=I1/4
         IF(I1.GT.0) GOTO 100
         MARE(I)=KOLT
  200 CONTINUE
      RETURN
      END
C*MODULE SOZEFF  *DECK PACKIEC
      SUBROUTINE PACKIEC(N1,IECONF,IEPACK)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IECONF(N1),IEPACK(*)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
C
      NBIT=64/2/NWDVAR
      K=1
      DO 100 I=1,N1
         IEPACK(K)=IOR(ISHFT(IEPACK(K),2),IECONF(I))
         IF(MOD(I,NBIT).EQ.0) K=K+1
  100 CONTINUE
      RETURN
      END
C*MODULE SOZEFF  *DECK COLLECT
      SUBROUTINE COLLECT(SOL1,SOL1MPI,SOL1MPJ,SOL2,STM,SLZ,IROOTI,IROOTJ
     *                  ,SOL1MUL,SOL2MUL,STMMUL,ALZMUL,MXRT,COEFI,COEFJ,
     *                   KARTMIN,KARTMAX,MAXNCO,SYLYES,SYRYES,DODM,DOJZ,
     *                   MP2SO,DFAC,HERM)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (TWO=2.0D+00)
      LOGICAL SYLYES(3),SYRYES(3),DODM,DOJZ,HERM
      DIMENSION SOL1MUL(3,MXRT,MXRT,2),SOL2MUL(3,MXRT,MXRT),STM(3),
     *          STMMUL(3,MXRT,MXRT),ALZMUL(MXRT,MXRT),SOL1(3),SOL2(3),
     *          SOL1MPI(MXRT,3),SOL1MPJ(MXRT,3),COEFI(*),COEFJ(*)
C
C
C     Add contribution of a matrix element between a pair of CSFs to
C     all CI states.
C
C     this subroutine is a cairn to a dumb waste of CPU time
C     daxpy need not run over all of irootj for ici1.eq.ici2
C
C     herm=.true. forces using hermiticity/antihermiticity
C
C     moron style
      CALL DSCAL(3,DFAC,SOL1,1)
C     sol1mpi and sol1mpj are multipled by DFAC below
      CALL DSCAL(3,DFAC,SOL2,1)
      DO 800 KART=KARTMIN,KARTMAX
        IF(SYLYES(KART)) THEN
          DO 100 IT1=1,IROOTI
            FF=COEFI(IT1)*SOL1(KART)
            CALL DAXPY(IROOTJ,FF,COEFJ,1,SOL1MUL(KART,IT1,1,1),3*MXRT)
            IF(HERM) THEN
C             1e SO anti-hermitian
              FF=-COEFJ(IT1)*SOL1(KART)
              CALL DAXPY(IROOTJ,FF,COEFI,1,SOL1MUL(KART,IT1,1,1),3*MXRT)
            ENDIF
            IF(MP2SO.GT.0) THEN
C
C             MP Hamiltonian is *not* Hermitian and it is
C             state-dependent (ket)
C             first, add transpose contribution (bra-dependent)
C             nota bene that '-' comes from antihermiticity, since
C             the operators are yet not multiplied by the imaginary
C             unity i.  halved are they for symmetrisation.
C
              FF=-COEFI(IT1)*SOL1MPI(IT1,KART)*DFAC/TWO
              CALL DAXPY(IROOTJ,FF,COEFJ,1,SOL1MUL(KART,IT1,1,2),3*MXRT)
            ENDIF
  100     CONTINUE
          IF(MP2SO.GT.0) THEN
            DO IT2=1,IROOTJ
C             second, add ket-dependent contribution
              FF= COEFJ(IT2)*SOL1MPJ(IT2,KART)*DFAC/TWO
              CALL DAXPY(IROOTI,FF,COEFI,1,SOL1MUL(KART,1,IT2,2),3)
C             the diagonal must be redone without antisymmetrisation
C             since it is zero after it.
            ENDDO
          ENDIF
          IF(MAXNCO.GT.1) THEN
            DO 200 IT1=1,IROOTI
              FF=COEFI(IT1)*SOL2(KART)
              CALL DAXPY(IROOTJ,FF,COEFJ,1,SOL2MUL(KART,IT1,1),3*MXRT)
              IF(HERM) THEN
C             2e SO anti-hermitian
                FF=-COEFJ(IT1)*SOL2(KART)
                CALL DAXPY(IROOTJ,FF,COEFI,1,SOL2MUL(KART,IT1,1),3*MXRT)
              ENDIF
  200       CONTINUE
          ENDIF
        ENDIF
  800 CONTINUE
      IF(DODM) THEN
      DO 900 KART=1,3
        IF(SYRYES(KART)) THEN
          DO 300 IT1=1,IROOTI
            FF=COEFI(IT1)*STM(KART)
            CALL DAXPY(IROOTJ,FF,COEFJ,1,STMMUL(KART,IT1,1),3*MXRT)
            IF(HERM) THEN
C             TM hermitian
              FF= COEFJ(IT1)*STM(KART)
              CALL DAXPY(IROOTJ,FF,COEFI,1,STMMUL(KART,IT1,1),3*MXRT)
            ENDIF
  300       CONTINUE
        ENDIF
  900 CONTINUE
      ENDIF
      IF(DOJZ) THEN
        DO 400 IT1=1,IROOTI
        CALL DAXPY(IROOTJ,COEFI(IT1)*SLZ,COEFJ,1,ALZMUL(IT1,1),MXRT)
C       "Lz" is antihermitian
        IF(HERM) CALL DAXPY(IROOTJ,-COEFJ(IT1)*SLZ,COEFI,1,ALZMUL(IT1,1)
     *                     ,MXRT)
  400   CONTINUE
      ENDIF
C     write(6,*) 'after x'
C     write(6,1) ((SOL1MUL(1,i,j),i=1,irooti),j=1,irootj)
C     write(6,*) 'after y'
C     write(6,1) ((SOL1MUL(2,i,j),i=1,irooti),j=1,irootj)
      RETURN
C   1 format(3F8.3)
      END
C*MODULE SOZEFF  *DECK GETECSF
      FUNCTION GETECSF(N1,EORB,JECONF)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION JECONF(*),EORB(*)
C
C     get 0th order energy of a CSF
C
      GETECSF=0.0D+00
      DO I=1,N1
         GETECSF=GETECSF+JECONF(I)*EORB(I)
      ENDDO
      RETURN
      END
C*MODULE SOZEFF  *DECK SO1MRMP2
      SUBROUTINE SO1MRMP2(SOL1MP,LP,LQ,EZPQ,COEFIMAX,COEFJMAX,SMALL,
     *                   ECONFJ,EREF0J,COEFJ,IROOTJ,INIDOC,LASDOC,INIACT
     *                   ,INIEXT,LASEXT,NMOEI,NMO,EORB,VONEEL,VTWOEL,
     *                    SO1MO,LIJMO,EDSHFT,IRRL)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C     logical debug
      DIMENSION SOL1MP(*),COEFJ(*),EREF0J(*),EORB(*),VONEEL(NMO,NMO),
     *       VTWOEL(NMOEI,*),SO1MO(NMO,NMO),LIJMO(NMO,NMO)
      PARAMETER (MXAO=2047)
      COMMON /SOSYM/  EULANG(4,48),GAM(48,48),IRMON(MXAO)
      COMMON /SYMMUL/ NIJREP(14,14),IJREP(2,14,14,14)
      COMMON/MQSYLB/ISYLAB(2,8,4)
C
C     get SO-MCQDPT corrections to the second order for 1 body terms
C     for a pair of active orbitals p and q.
C     This constitues the integral factor to be multiplied
C     by the UG generator matrix element, Ez(pq).
C     The code is nicked from the spin-free MCQDPT code and
C     everything is kept as close to that code as possible.
C     Ez stands for spin-polarised E,
C     Ez(pq)=a+(p,alpha)*a(q,alpha) - a+(p,beta)*a(q,beta)
C     where "a+" and "a" are creation/annihilation operators.
C     This differs from E(pq) by sign.
C     Ezpq is also multipled by CSF-determinant coefficients.
C     coefimax and coefj are used to filter out small contributions.
C     they are the CSF coefficients for bra and ket.
C
C     if(abs(Ezpq).lt.small) return
C     Ezpq is not zero by construction (that is, when it is zero, this
C     subroutine is never called)
C     write(6,*) 'wwpq',lp,lq
C     big=0.2D+00
      IF(ABS(EZPQ*COEFIMAX*COEFJMAX).LT.SMALL) RETURN
      EQ=EORB(LQ)
      EP=EORB(LP)
      DO 1000 JSTATE=1,IROOTJ
         S=0.0D+00
         IF(ABS(EZPQ*COEFIMAX*COEFJ(JSTATE)).LT.SMALL) GOTO 1000
C        debug=.false.
C        if(abs(Ezpq*coefimax*coefj(jstate)).gt.0.5D-01) debug=.true.
         ESI2=ECONFJ-EREF0J(JSTATE)
C     D-5
C        s5=s
         DO KI=INIDOC,LASDOC
            EI2=EORB(KI)-ESI2
            DELTA=EP-EI2
            S=S+(VONEEL(KI,LQ)*SO1MO(LP,KI)+SO1MO(KI,LQ)*VONEEL(LP,KI))/
     *          (DELTA+EDSHFT/DELTA)
         END DO
C        if(abs(s-s5).gt.big) write(6,*) lp,lq,'total D5=',s-s5
C     D-4
C        s4=s
         DO KE=INIEXT,LASEXT
            EE2=EORB(KE)+ESI2
            DELTA=EE2-EQ
            S=S-(VONEEL(LP,KE)*SO1MO(KE,LQ)+SO1MO(LP,KE)*VONEEL(KE,LQ))/
     *          (DELTA+EDSHFT/DELTA)
         END DO
C      if(debug.and.abs(s-s4).gt.big) write(6,*) lp,lq,'total D4=',s-s4,
C    *                                Ezpq*coefimax*coefj(jstate),jstate
C        if(abs(s-s4).gt.big) write(6,*) lp,lq,'total D4=',s-s4
C     D-7 and D-9
C        s79=s
         DO KAE=INIACT,LASEXT
            IS=IJREP(2,1,IRMON(KAE),IRRL)
            KA1P=ISYLAB(1,IS,2)
            KA2P=ISYLAB(2,IS,2)
C           DO KI =INIDOC,LASDOC
            DO KI =KA1P,KA2P
               EAEI2=EORB(KAE)-EORB(KI)+ESI2
               DELTA=EAEI2+EP-EQ
               DELTA1=EAEI2
               S=S-SO1MO(KI,KAE)
     *            *(   (
     *               -    VTWOEL(LIJMO(KAE,LQ),LIJMO(LP,KI)) )
     *                 /(DELTA+EDSHFT/DELTA)
     *            -
     *                 (
     *               -   VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LQ)) )
     *                 /(DELTA1+EDSHFT/DELTA1)
     *             )
C           write(6,*) '   ',kae,ki,so1mo(KI,KAE),EAEI2+EP-EQ,EAEI2
C      minus sign appearing on the separate line above comes from the
C      fact that so1mo is an antisymmetric matrix! (contrary to VONEEL).
            END DO
         END DO
C        if(abs(s-s79).gt.big) write(6,*) lp,lq,'total D79=',s-s79
         SOL1MP(JSTATE)=SOL1MP(JSTATE)+EZPQ*S
 1000 CONTINUE
      RETURN
      END
C*MODULE SOZEFF  *DECK SO2MRMP2
      SUBROUTINE SO2MRMP2(SOL1MP,LP,LQ,LR,LS,EZPQRS,COEFIMAX,COEFJMAX,
     *                   SMALL,ECONFJ,EREF0J,COEFJ,IROOTJ,NMOEI,NMO,EORB
     *                  ,VTWOEL,SO1MO,LIJMO,EDSHFT,IRRL,EEQTOL,NEEQ)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION SOL1MP(*),EREF0J(*),COEFJ(*),EORB(*),VTWOEL(NMOEI,*),
     *          SO1MO(NMO,NMO),LIJMO(NMO,NMO)
      PARAMETER (MXAO=2047,ZERO=0.0D+00)
      COMMON /SOSYM/  EULANG(4,48),GAM(48,48),IRMON(MXAO)
      COMMON /SYMMUL/ NIJREP(14,14),IJREP(2,14,14,14)
      COMMON/MQSYLB/ISYLAB(2,8,4)
C
C     get SO-MCQDPT corrections to the second order for 2 body terms
C     for a pair of active orbitals p,q,r and s.
C     This constitues the integral factor to be multiplied
C     by the UG generator matrix element, Ez(pq,rs).
C     The code is nicked from the spin-free MCQDPT code and
C     everything is kept as close to that code as possible.
C     Ez(pq,rs)=a+(p,alpha)*Ers*a(q,alpha) - a+(p,beta)*Ers*a(q,beta)
C     This again differs from spin-free E(pq,rs) by sign.
C     Ezpqrs is also multiplied by CSF-determinant coefficients.
C
C     write(6,*) 'wwpqrs',lp,lq,lr,ls,COEFIMAX,COEFJMAX,
C    *                   SMALL,ECONFJ,EREF0J(1),COEFJ(1),IROOTJ,
C    *                   NMOEI,NMO,(EORB(mm),mm=1,nmo),EDSHFT,irrl
C     big=0.2D+00
      IF(ABS(EZPQRS*COEFIMAX*COEFJMAX).LT.SMALL) RETURN
      EQ=EORB(LQ)
      EP=EORB(LP)
      ER=EORB(LR)
      ES=EORB(LS)
      EPSR=EP-ES+ER
      EQRS=EQ-ER+ES
C     Assuming Abelian symmetry but otherwise the code
C     is supposed to have aborted above.
C     IRRL has the irrep of L (operator in SO1MO integrals).
      IS=IJREP(2,1,IRMON(LP),IRRL)
C     the last index in ISYLAB numbers spaces:
C     1- fzc, 2- doc, 3- act, 4- ext.
      KI1P=ISYLAB(1,IS,2)
      KI2P=ISYLAB(2,IS,2)
      KE1P=ISYLAB(1,IS,4)
      KE2P=ISYLAB(2,IS,4)
      IS=IJREP(2,1,IRMON(LQ),IRRL)
      KI1Q=ISYLAB(1,IS,2)
      KI2Q=ISYLAB(2,IS,2)
      KE1Q=ISYLAB(1,IS,4)
      KE2Q=ISYLAB(2,IS,4)
      LRS=LIJMO(LR,LS)
C
C     DO 1000 JSTATE=1,IROOTJ
C     We unroll the jstate loop. If extra degeneracies are present
C     (mostly, in case of atoms), then two state energies can be equal.
C     In this case we don't have to do one diagram per each state,
C     instead, we do one diagram for the averaged energy of the two
C     (or more) states, and then use the diagram sum for each state
C     (no approximation).  This is also a not well trodden way to
C     make some state energies artificially equal for some purpose
C     (speed-up, intruder states).
C
      NJST=0
      JSTATE=0
      AEREF0J=ZERO
      NZERO=0
 1000 CONTINUE
        JSTATE=JSTATE+1
        NJST=NJST+1
        AEREF0J=AEREF0J+EREF0J(JSTATE)
        IF(ABS(EZPQRS*COEFIMAX*COEFJ(JSTATE)).LT.SMALL) NZERO=NZERO+1
        IF(JSTATE.LT.IROOTJ) THEN
           IF(ABS(EREF0J(JSTATE+1)-EREF0J(JSTATE)).LT.EEQTOL) GOTO 1000
        ENDIF
        AEREF0J=AEREF0J/NJST
        ESI2=ECONFJ-AEREF0J
        AEREF0J=ZERO
        NZERO1=NZERO
        NZERO=0
C       CI coefficient threshold test, all are small in a diagram batch
        IF(NZERO1.EQ.NJST) THEN
           NJST=0
           GOTO 1100
        ENDIF
        NEEQ=NEEQ+NJST-1
C       ESI2=ECONFJ-EREF0J(JSTATE)
C       IF(ABS(EZPQRS*COEFIMAX*COEFJ(JSTATE)).LT.SMALL) GOTO 1000
        S=ZERO
C       s1617=s
C       D-N refers to the diagram number in MCQDPT, counted from 1.
C       D-16
C*VDIR NOVECTOR
        DO KI=KI1Q,KI2Q
C       DO KI=INIDOC,LASDOC
          DELTA=EPSR-EORB(KI)+ESI2
          S=S+SO1MO(KI,LQ)*VTWOEL(LIJMO(LP,KI),LRS)/(DELTA+EDSHFT/DELTA)
        END DO
C       D-17
C*VDIR NOVECTOR
        DO KI=KI1P,KI2P
C       DO KI=INIDOC,LASDOC
          DELTA=EP-EORB(KI)+ESI2
          S=S+VTWOEL(LIJMO(KI,LQ),LRS)*SO1MO(LP,KI)/(DELTA+EDSHFT/DELTA)
        END DO
C       D-15
C*VDIR NOVECTOR
        DO KE=KE1P,KE2P
C        DO KE=INIEXT,LASEXT
C       s1415=s
          DELTA=EORB(KE)+ESI2-EQRS
          S=S-SO1MO(LP,KE)*VTWOEL(LIJMO(KE,LQ),LRS)/(DELTA+EDSHFT/DELTA)
        END DO
C       D-14
C*VDIR NOVECTOR
        DO KE=KE1Q,KE2Q
C        DO KE=INIEXT,LASEXT
C       s1415=s
          DELTA=EORB(KE)+ESI2-EQ
          S=S-VTWOEL(LIJMO(LP,KE),LRS)*SO1MO(KE,LQ)/(DELTA+EDSHFT/DELTA)
        END DO
C       if(abs(s-s1415).gt.big) write(6,*) lp,lq,lr,ls,'total D14,15=',
C    *                                  s-s1415
        DO JST=1,NJST
          JJST=JSTATE-NJST+JST
          SOL1MP(JJST)=SOL1MP(JJST)+EZPQRS*S
        ENDDO
        NJST=0
C       SOL1MP(JSTATE)=SOL1MP(JSTATE)+EZPQRS*S
 1100 CONTINUE
      IF(JSTATE.LT.IROOTJ) GOTO 1000
C1000 CONTINUE
      RETURN
      END
C*MODULE SOZEFF  *DECK SO2FMRMP2
      SUBROUTINE SO2FMRMP2(K,L,M,N,KS,LS,MS,NS,EZ2P,FAC,
     *                     FFBUF,INDFF,NFF,NFFBUF,SMALL)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION EZ2P(0:1,0:1,0:1,0:1),FFBUF(*),INDFF(*)
C
      IF(L.NE.N) THEN
        FAC1= EZ2P(KS,LS,MS,NS)*FAC
        IF(ABS(FAC1).GT.SMALL) CALL ADDFF(2,NFFBUF,NFF,FFBUF,INDFF,
     *                                    K,L,M,N,FAC1,SMALL)
        FAC1= EZ2P(MS,NS,KS,LS)*FAC
        IF(ABS(FAC1).GT.SMALL) CALL ADDFF(2,NFFBUF,NFF,FFBUF,INDFF,
     *                                    M,N,K,L,FAC1,SMALL)
        FAC1=-EZ2P(KS,NS,MS,LS)*FAC
        IF(ABS(FAC1).GT.SMALL) CALL ADDFF(2,NFFBUF,NFF,FFBUF,INDFF,
     *                                    K,N,M,L,FAC1,SMALL)
        FAC1=-EZ2P(MS,LS,KS,NS)*FAC
        IF(ABS(FAC1).GT.SMALL) CALL ADDFF(2,NFFBUF,NFF,FFBUF,INDFF,
     *                                    M,L,K,N,FAC1,SMALL)
      ELSE
C        then diagrams in the calls 1 and 3; 2 and 4 are the same
C        (except for the spin factor than is factored out)
        FAC1=(EZ2P(KS,LS,MS,NS)-EZ2P(KS,NS,MS,LS))*FAC
        IF(ABS(FAC1).GT.SMALL) CALL ADDFF(2,NFFBUF,NFF,FFBUF,INDFF,
     *                                    K,L,M,N,FAC1,SMALL)
        FAC1=(EZ2P(MS,NS,KS,LS)-EZ2P(MS,LS,KS,NS))*FAC
        IF(ABS(FAC1).GT.SMALL) CALL ADDFF(2,NFFBUF,NFF,FFBUF,INDFF,
     *                                    M,N,K,L,FAC1,SMALL)
      ENDIF
      RETURN
      END
C*MODULE SOZEFF  *DECK SO2EMRMP2
      SUBROUTINE SO2EMRMP2(SOL1MPI,SOL1MPJ,K,L,M,N,KS,LS,MS,NS,EZ2P,FAC,
     *                     COEFIMAX,COEFJMAX,SMALL,ECONFI,ECONFJ,EREF0I,
     *                     EREF0J,COEFI,COEFJ,IROOTI,IROOTJ,NMOEI,NMO,
     *                     EORB,VTWOEL,SO1MO,LIJMO,EDSHFT,IRRL,EEQTOL,
     *                     NEEQ)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION SOL1MPI(*),SOL1MPJ(*),EZ2P(0:1,0:1,0:1,0:1),EREF0I(*),
     *          EREF0J(*),COEFI(*),COEFJ(*),EORB(*),VTWOEL(NMOEI,*),
     *          SO1MO(NMO,NMO),LIJMO(NMO,NMO)
C
C     Compute 2 order SO-MCQDPT corrections for all combos of non-zero
C     unitary group generators Ezkl,mn with spin parts ks,ls,ms,ns.
C
C     first, get ket-dependent contribution
C     (k,l,m,n) are used in "chemical" notation, [11|22].
C
      IF(L.NE.N) THEN
      CALL SO2MRMP2(SOL1MPJ,K,L,M,N,EZ2P(KS,LS,MS,NS)*FAC,COEFIMAX,
     *             COEFJMAX,SMALL,ECONFJ,EREF0J,COEFJ,IROOTJ,
     *             NMOEI,NMO,EORB,VTWOEL,SO1MO,
     *             LIJMO,EDSHFT,IRRL,EEQTOL,NEEQ)
      CALL SO2MRMP2(SOL1MPJ,M,N,K,L,EZ2P(MS,NS,KS,LS)*FAC,COEFIMAX,
     *             COEFJMAX,SMALL,ECONFJ,EREF0J,COEFJ,IROOTJ,
     *             NMOEI,NMO,EORB,VTWOEL,SO1MO,
     *             LIJMO,EDSHFT,IRRL,EEQTOL,NEEQ)
      CALL SO2MRMP2(SOL1MPJ,K,N,M,L,-EZ2P(KS,NS,MS,LS)*FAC,COEFIMAX,
     *             COEFJMAX,SMALL,ECONFJ,EREF0J,COEFJ,IROOTJ,
     *             NMOEI,NMO,EORB,VTWOEL,SO1MO,
     *             LIJMO,EDSHFT,IRRL,EEQTOL,NEEQ)
      CALL SO2MRMP2(SOL1MPJ,M,L,K,N,-EZ2P(MS,LS,KS,NS)*FAC,COEFIMAX,
     *             COEFJMAX,SMALL,ECONFJ,EREF0J,COEFJ,IROOTJ,
     *             NMOEI,NMO,EORB,VTWOEL,SO1MO,
     *             LIJMO,EDSHFT,IRRL,EEQTOL,NEEQ)
      ELSE
C        then diagrams in the calls 1 and 3; 2 and 4 are the same
C        (except for the spin factor than is factored out)
      CALL SO2MRMP2(SOL1MPJ,K,L,M,N,(EZ2P(KS,LS,MS,NS)-EZ2P(KS,NS,MS,LS)
     *              )*FAC,COEFIMAX,COEFJMAX,SMALL,ECONFJ,EREF0J,COEFJ,
     *              IROOTJ,NMOEI,NMO,EORB,
     *              VTWOEL,SO1MO,LIJMO,EDSHFT,IRRL,EEQTOL,NEEQ)
      CALL SO2MRMP2(SOL1MPJ,M,N,K,L,(EZ2P(MS,NS,KS,LS)-EZ2P(MS,LS,KS,NS)
     *              )*FAC,COEFIMAX,COEFJMAX,SMALL,ECONFJ,EREF0J,COEFJ,
     *              IROOTJ,NMOEI,NMO,EORB,
     *              VTWOEL,SO1MO,LIJMO,EDSHFT,IRRL,EEQTOL,NEEQ)
      ENDIF
C
C     now, get "bra"-dependent contribution, that is for bra and ket
C     exchanged.  bra k,m and ket orbital indices l,n are also exchanged
C
      IF(K.NE.M) THEN
      CALL SO2MRMP2(SOL1MPI,L,K,N,M,EZ2P(LS,KS,NS,MS)*FAC,COEFJMAX,
     *             COEFIMAX,SMALL,ECONFI,EREF0I,COEFI,IROOTI,
     *             NMOEI,NMO,EORB,VTWOEL,SO1MO,
     *             LIJMO,EDSHFT,IRRL,EEQTOL,NEEQ)
      CALL SO2MRMP2(SOL1MPI,N,M,L,K,EZ2P(NS,MS,LS,KS)*FAC,COEFJMAX,
     *             COEFIMAX,SMALL,ECONFI,EREF0I,COEFI,IROOTI,
     *             NMOEI,NMO,EORB,VTWOEL,SO1MO,
     *             LIJMO,EDSHFT,IRRL,EEQTOL,NEEQ)
      CALL SO2MRMP2(SOL1MPI,L,M,N,K,-EZ2P(LS,MS,NS,KS)*FAC,COEFJMAX,
     *             COEFIMAX,SMALL,ECONFI,EREF0I,COEFI,IROOTI,
     *             NMOEI,NMO,EORB,VTWOEL,SO1MO,
     *             LIJMO,EDSHFT,IRRL,EEQTOL,NEEQ)
      CALL SO2MRMP2(SOL1MPI,N,K,L,M,-EZ2P(NS,KS,LS,MS)*FAC,COEFJMAX,
     *             COEFIMAX,SMALL,ECONFI,EREF0I,COEFI,IROOTI,
     *             NMOEI,NMO,EORB,VTWOEL,SO1MO,
     *             LIJMO,EDSHFT,IRRL,EEQTOL,NEEQ)
      ELSE
C        then diagrams in the calls 1 and 3; 2 and 4 are the same
C        (except for the spin factor than is factored out)
      CALL SO2MRMP2(SOL1MPI,L,K,N,M,(EZ2P(LS,KS,NS,MS)-EZ2P(LS,MS,NS,KS)
     *              )*FAC,COEFJMAX,COEFIMAX,SMALL,ECONFI,EREF0I,COEFI,
     *              IROOTI,NMOEI,NMO,EORB,
     *              VTWOEL,SO1MO,LIJMO,EDSHFT,IRRL,EEQTOL,NEEQ)
      CALL SO2MRMP2(SOL1MPI,N,M,L,K,(EZ2P(NS,MS,LS,KS)-EZ2P(NS,KS,LS,MS)
     *              )*FAC,COEFJMAX,COEFIMAX,SMALL,ECONFI,EREF0I,COEFI,
     *              IROOTI,NMOEI,NMO,EORB,
     *              VTWOEL,SO1MO,LIJMO,EDSHFT,IRRL,EEQTOL,NEEQ)
      ENDIF
      RETURN
      END
C*MODULE SOZEFF  *DECK MP2IRL
      SUBROUTINE MP2IRL(NMOACT,NMODOC,NMOFZC,NMO,NOSYM)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXAO=2047)
      COMMON /SOSYM/  EULANG(4,48),GAM(48,48),IRMON(MXAO)
      COMMON/MQSYLB/ISYLAB(2,8,4)
C
C     Generate ISYLAB array that gives initial and final locations
C     of each IRREP in each of the four subspaces.
C
C     GAMESS generates irreps in a somewhat bizarre order, e.g. in D2h
C     the order is Ag,Au,B3u,B3g,B1g,B1u,B2u,B2g, not what the "offical"
C     order is. MCQDPT faithfully adhers to the declared order
C     Ag,B1g,B2g,B3g,Au,B1u,B2u,B3u. Therefore, the MCQDPT array ISYLAB
C     cannot be used. It is, however, easy to regenerate it from IRMON.
C     Note that in IRMON all irreps are put together, like this:
C     1,1,1, 3,3,3,3, 6,6,6 (as a result of orbital sorting in MCQDPT)
C     and there is no need to reorder the orbitals.
C
      IF(NOSYM.LE.0) THEN
        CALL VICLR(ISYLAB,1,2*8*4)
        DO I=1,NMO
          ISPACE=1
          IF(I.GT.NMOFZC) ISPACE=2
          IF(I.GT.NMOFZC+NMODOC) ISPACE=3
          IF(I.GT.NMOFZC+NMODOC+NMOACT) ISPACE=4
          IRREP=IRMON(I)
          IF(ISYLAB(1,IRREP,ISPACE).EQ.0) ISYLAB(1,IRREP,ISPACE)=I
          ISYLAB(1,IRREP,ISPACE)=MIN(ISYLAB(1,IRREP,ISPACE),I)
          ISYLAB(2,IRREP,ISPACE)=MAX(ISYLAB(2,IRREP,ISPACE),I)
        ENDDO
        DO ISPACE=1,4
          DO IRREP=1,8
            IF(ISYLAB(1,IRREP,ISPACE).EQ.0) ISYLAB(1,IRREP,ISPACE)=1
C
C           this will prevent the loop from doing one "empty" run
C           from 0 to 0.
C           write(6,*) 'isylab debug:',(isylab(k,irrep,ispace),k=1,2)
C
          ENDDO
        ENDDO
      ELSE
C     simulate no symmetry by giving full orbital range for each irrep
        DO IRREP=1,8
          ISYLAB(1,IRREP,1)=1
          ISYLAB(2,IRREP,1)=NMOFZC
          ISYLAB(1,IRREP,2)=1+NMOFZC
          ISYLAB(2,IRREP,2)=NMOFZC+NMODOC
          ISYLAB(1,IRREP,3)=1+NMOFZC+NMODOC
          ISYLAB(2,IRREP,3)=NMOFZC+NMODOC+NMOACT
          ISYLAB(1,IRREP,4)=1+NMOFZC+NMODOC+NMOACT
          ISYLAB(2,IRREP,4)=NMO
        ENDDO
      ENDIF
      RETURN
      END
C*MODULE SOZEFF  *DECK ADDFF
      SUBROUTINE ADDFF(NBODY,MAXN,N,BUF,IND,I,J,K,L,FAC,TOL)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION BUF(*),IND(*)
C
C     add a 1 or 2 body form factor
C
      IF(NBODY.EQ.1) THEN
         NEWIND=IOR(ISHFT(I,8),J)
      ELSE
         NEWIND=IOR(ISHFT(I,24),IOR(ISHFT(J,16),IOR(ISHFT(K,8),L)))
      ENDIF
      DO II=1,N
         IF(IND(II).EQ.NEWIND) THEN
            BUF(II)=BUF(II)+FAC
C           write(6,*) 'updated a FF',nbody,buf(ii),i,j,k,l
C           if became zero, shift up the rest
            IF(ABS(BUF(II)).LT.TOL) THEN
               DO JJ=II,N-1
                  BUF(JJ)=BUF(JJ+1)
                  IND(JJ)=IND(JJ+1)
               ENDDO
               IF(N.GT.0) N=N-1
            ENDIF
            RETURN
         ENDIF
      ENDDO
      N=N+1
      IF(N.GT.MAXN) THEN
         WRITE(6,*) NBODY,' BODY FF BUFFER FULL'
         CALL ABRT
      ENDIF
      BUF(N)=FAC
      IND(N)=NEWIND
C     write(6,*) 'added a FF',nbody,fac,i,j,k,l
      RETURN
      END
C*MODULE SOZEFF  *DECK ADDNFF1
      SUBROUTINE ADDNFF1(M,NST,NACT,MXST,BUF,BUFI,INDI,CC,TOL)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION BUF(MXST,NACT,NACT),CC(*),BUFI(*),INDI(*)
C
C     add one body form factors coming from a pair of CSFs
C
      DO 100 IFF=1,M
        FAC=BUFI(IFF)
        IF(ABS(FAC).LT.TOL) GOTO 100
        LABEL=INDI(IFF)
        I=IAND(ISHFT(LABEL,-8),255)
        J=IAND(LABEL,255)
        CALL DAXPY(NST,FAC,CC,1,BUF(1,I,J),1)
C       write(6,*) 'Updated-1',(buf(ii,i,j),ii=1,nst)
  100 CONTINUE
      RETURN
      END
C*MODULE SOZEFF  *DECK ADDNFF2
      SUBROUTINE ADDNFF2(MAXN,N,M,NST,NACT,MXST,BUF,IND,BUFI,INDI,CC,
     *                   TOL)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION N(NACT,NACT),BUF(MXST,MAXN,NACT,NACT),
     *          IND(MAXN,NACT,NACT),CC(*),BUFI(*),INDI(*)
C
C     add two body form factors coming from a pair of CSFs
C     the two rightmost indices are packed and the two leftmost are
C     used as array indices This speeds up search at the cost of memory.
C     Having all four packed would slow down search and having all four
C     unpacked would use sparseness inefficiently: slow again).
C
      DO 100 IFF=1,M
        FAC=BUFI(IFF)
        IF(ABS(FAC).LT.TOL) GOTO 100
        LABEL=INDI(IFF)
        I=ISHFT(LABEL,-24)
        J=IAND(ISHFT(LABEL,-16),255)
        NEWIND=IAND(LABEL,65535)
        NN=N(I,J)
        DO II=1,NN
          IF(IND(II,I,J).EQ.NEWIND) THEN
            IF(NST.EQ.1) THEN
              BUF(1,II,I,J)=BUF(1,II,I,J)+FAC*CC(1)
              BUFM=BUF(1,II,I,J)
            ELSE
              CALL DAXPY(NST,FAC,CC,1,BUF(1,II,I,J),1)
              BUFM=BUF(IDAMAX(NST,BUF(1,II,I,J),1),II,I,J)
            ENDIF
C           if became zero, shift up the rest
            IF(ABS(BUFM).LT.TOL) THEN
               IF(NST.EQ.1) THEN
                 DO JJ=II,NN-1
                   IND(JJ,I,J)=IND(JJ+1,I,J)
                   BUF(1,JJ,I,J)=BUF(1,JJ+1,I,J)
                 ENDDO
               ELSE
                 DO JJ=II,NN-1
                   IND(JJ,I,J)=IND(JJ+1,I,J)
                   CALL DCOPY(NST,BUF(1,JJ+1,I,J),1,BUF(1,JJ,I,J),1)
                 ENDDO
               ENDIF
               IF(NN.GT.0) N(I,J)=N(I,J)-1
            ENDIF
            GOTO 100
          ENDIF
        ENDDO
        NN=NN+1
        N(I,J)=NN
        IF(NN.GT.MAXN) THEN
          WRITE(6,*) 'FF BUFFER FULL, INCREASE NFFBUF, ',MAXN
          CALL ABRT
        ENDIF
        IF(NST.EQ.1) THEN
          BUF(1,NN,I,J)=FAC*CC(1)
        ELSE
          CALL VCLR(BUF(1,NN,I,J),1,NST)
          CALL DAXPY(NST,FAC,CC,1,BUF(1,NN,I,J),1)
        ENDIF
        IND(NN,I,J)=NEWIND
C       write(6,*) 'stored-2',i,j,(buf(ii,nn,i,j),ii=1,nst),cc(1),fac
  100 CONTINUE
      RETURN
      END
C*MODULE SOZEFF  *DECK COLLMP2
      SUBROUTINE COLLMP2(BUF1,MAXN,N2,IND2,BUF2,KARTMIN,KARTMAX,IRRL,
     *                   SYLYES,EORB,ECONFJ,EREF0J,VONEEL,VTWOEL,UONEEL,
     *                   LIJMO,NMO,NMOACT,NMODOC,NMOEXT,NMOFZC,NMOEI,
     *                   EDSHFT,IROOTI,IROOTJ,SOL1MUL,SOL1MPJ,SMALL,
     *                  COEFJMAX,COEFJ,EEQTOL,NEEQ,DFAC,ISOLOOP,MAXNFF2)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL SYLYES(3)
      PARAMETER (ONE=1.0D+00,TWO=2.0D+00)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      DIMENSION BUF1(MXRT,NMOACT,NMOACT),N2(NMOACT,NMOACT),
     *          IND2(MAXN,NMOACT,NMOACT),BUF2(MXRT,MAXN,NMOACT,NMOACT),
     *          IRRL(3),EORB(*),EREF0J(*),VONEEL(NMO,*),VTWOEL(NMOEI,*),
     *          UONEEL(NMO,NMO,3),LIJMO(NMO,*),SOL1MUL(3,MXRT,MXRT),
     *          SOL1MPJ(MXRT,*),COEFJ(*)
C
C     Compute one and two body terms in SO-MCQDPT using precomputed
C     form factors (aka the coupling constants). Each of these is
C     multiplied by the appropriate diagram sum.
C
      INIDOC=NMOFZC+1
      LASDOC=NMOFZC+NMODOC
      INIACT=NMOFZC+NMODOC+1
      INIEXT=NMOFZC+NMODOC+NMOACT+1
      LASEXT=NMOFZC+NMODOC+NMOACT+NMOEXT
C
      FAC=ONE
      DO I=1,NMOACT
        DO J=1,NMOACT
C
C         one body
C
          COEFIMAX=BUF1(IDAMAX(IROOTI,BUF1(1,I,J),1),I,J)
          IF(ABS(COEFIMAX*COEFIMAX).GT.SMALL) THEN
            DO IX=KARTMIN,KARTMAX
              IF(SYLYES(IX)) THEN
                CALL VCLR(SOL1MPJ(1,IX),1,IROOTJ)
C               compute one-body diagram
                CALL SO1MRMP2(SOL1MPJ(1,IX),I+NFZC,J+NFZC,FAC,COEFIMAX,
     *                        COEFIMAX,SMALL,ECONFJ,EREF0J,COEFJ,IROOTJ,
     *                        INIDOC,LASDOC,INIACT,INIEXT,LASEXT,NMOEI,
     *                        NMO,EORB,VONEEL,VTWOEL,UONEEL(1,1,IX),
     *                        LIJMO,EDSHFT,IRRL(IX))
                DO IT2=1,IROOTJ
                  FF= COEFJ(IT2)*SOL1MPJ(IT2,IX)*DFAC/TWO
                  IF(ISOLOOP.EQ.0) THEN
               CALL DAXPY(IROOTI, FF,BUF1(1,I,J),1,SOL1MUL(IX,1,IT2),3)
                  ELSE
                  CALL DAXPY(IROOTI,-FF,BUF1(1,I,J),1,SOL1MUL(IX,IT2,1),
     *                       3*MXRT)
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDIF
C
C         two body
C
          N=N2(I,J)
          MAXNFF2=MAX(MAXNFF2,N)
          DO IFF=1,N
            LABEL=IND2(IFF,I,J)
            K=IAND(ISHFT(LABEL,-8),255)
            L=IAND(LABEL,255)
            COEFIMAX=BUF2(IDAMAX(IROOTI,BUF2(1,IFF,I,J),1),IFF,I,J)
            DO IX=KARTMIN,KARTMAX
              IF(SYLYES(IX)) THEN
                CALL VCLR(SOL1MPJ(1,IX),1,IROOTJ)
C               compute two-body diagram
                CALL SO2MRMP2(SOL1MPJ(1,IX),I+NFZC,J+NFZC,K+NFZC,L+NFZC,
     *                        FAC,COEFIMAX,COEFJMAX,SMALL,ECONFJ,EREF0J,
     *                        COEFJ,IROOTJ,NMOEI,NMO,EORB,VTWOEL,
     *                        UONEEL(1,1,IX),LIJMO,EDSHFT,IRRL(IX),
     *                        EEQTOL,NEEQ)
                DO IT2=1,IROOTJ
                  FF= COEFJ(IT2)*SOL1MPJ(IT2,IX)*DFAC/TWO
                  IF(ISOLOOP.EQ.0) THEN
                    CALL DAXPY(IROOTI, FF,BUF2(1,IFF,I,J),1,
     *                         SOL1MUL(IX,1,IT2),3)
                  ELSE
                    CALL DAXPY(IROOTI,-FF,BUF2(1,IFF,I,J),1,
     *                         SOL1MUL(IX,IT2,1),3*MXRT)
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      RETURN
      END
