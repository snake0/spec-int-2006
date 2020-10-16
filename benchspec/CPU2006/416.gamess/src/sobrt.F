C  9 dec 03 - mws - synch common block runopt
C  3 Sep 03 - DGF - implement MCP 2e core-active SOC integrals
C 26 Mar 02 - KRG - rename GAMMA->GAMMSO to avoid FORTRAN intrinsic name
C  1 aug 01 - DGF - SOBRTX: extra argument to addzero
C 13 Jun 01 - DGF - relocate some code to new source module solib
C 25 Mar 00 - DGF - code 1e NESC SOC
C 16 FEB 00 - JAB - DON'T WRITE EMPTY DATA RECORDS TO UNIT 51/52/53
C 10 Jan 00 - MWS - synchronize relwfn common block
C 21 Dec 99 - DGF - changes from 7/jan/98 to 2/may/99 brought online
C  2 May 99 - DGF - allow to duplicate/distribute SOC 2e integrals
C 25 Apr 99 - DGF - allow for same multiplicities in different $CIDRTs
C 23 Apr 99 - DGF - dodge 2e SOC integrals forbidden by symmetry
C 14 Dec 98 - DGF - defracture the 2e AO array into blocks
C  1 Dec 98 - DGF - redirect all SO integral needs to BRTHSO
C 27 Oct 98 - DGF - let the direct Hso method use 2e integrals in SOINT2
C 30 Sep 98 - DGF - put SOC constant calculation back into action
C  7 Aug 98 - DGF - parallellization: split recsize instead of no. recs
C 22 Jul 98 - DGF - parallelise brthso: full parallelisation of PB code
C 15 Jul 98 - DGF - add f and g functions to the integral code
C 12 Jul 98 - DGF - eliminate 1 out of 3 calculated matr. elements ms=-1
C  7 Jul 98 - DGF - add noirr to gugwfn
C  8 Jul 98 - DGF - do all CSF index transformations with one data read
C 14 May 98 - DGF - remarry the transition moment codes
C 29 Apr 98 - DGF - add dipole moment/remove double group selectn rules
C 26 Feb 98 - DGF - do some adjustments for double group selection rules
C 17 Jan 98 - DGF - generalise the code for arbitrary spin mult and
C                   arbitrary no. of CI runs, adjust matr. element phase
C  7 Jan 98 - DGF - rid spntrn of the bothersome arrays
C  6 Jan 98 - DGF - change CSF data order to optimize 64 bit machines
C                   calculate SOCC (SO coupling constant) itself
C                   parallelise gamma, adjust mem allocation in sobrtx
C 14 Oct 97 - DGF,TRF - allow for 10 active orbitals: sobrtx,gamma,etc
C 28 SEP 97 - MWS - CONVERT PARALLEL CALLS FROM TCGMSG TO DDI
C 16 JUL 97 - MWS - BRTHSO,SOIN1,SOINT2: DATA'S MADE LAST DECLARATIONS
C 24 JUN 97 - DGF - DESINSECT MEMORY ALLOCATION IN SOBRTX
C  2 APR 97 - DGF - ADAPT TOM FURLANI'S SOCC CODE FOR GAMESS
C
C*MODULE SOBRT   *DECK DENMAT
      SUBROUTINE DENMAT(IA,DA,DC,TPDM4F,TDENM)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IA(*),DA(*),DC(*),TPDM4F(*),TDENM(*)
      LOGICAL JJEQII,LLEQKK,IIEQJJ,KKEQLL
      COMMON /SHLDAT/ XI,YI,ZI,XJ,YJ,ZJ,XK,YK,ZK,XL,YL,ZL,
     1                IIATM,MINII,MAXII,IP1,IP2,
     2                JJATM,MINJJ,MAXJJ,JP1,JP2,
     3                KKATM,MINKK,MAXKK,KP1,KP2,
     4                LLATM,MINLL,MAXLL,LP1,LP2,
     5                IB1,IB2,JB1,JB2,KB1,KB2,LB1,LB2,
     *                IIEQJJ,KKEQLL,JJEQII,LLEQKK
      COMMON /DM3/    LOKII,LOKJJ,LOKKK,LOKLL,IIN,JJN,KKN,LLN,IJN
C
C     FORM THE ONE AND TWO PARTICLE DENSITY MATRIX I.E., THE
C     MATRIX ELEMENT THAT WILL MULTIPLY THE FOLLOWING INTEGRAL
C
C            <I(1),K(2)/L12,-M/J(1),L(2)>
C
C     THE MATRIX ELEMENT THAT WILL MULTIPLY THIS IS GIVEN BY
C     THE FOLLOWING FORMULA
C
C     TDENM(IJKL)= 1/(1+DELTA(KL)) * C4DC(KL)DA(IJ) + 3DC(KJ)DA(LI)
C     + 3DC(LJ)DA(KI) + 3DC(IL)DA(JK) + 3DC(IK)DA(JL) + TPDM4F(IJKL)!
C
C     WHERE DC  IS THE CORE DENSITY MATRIX IN THE AO BASIS (FROM CORORB)
C           DA  IS THE ACTIVE ONE PARTICLE TRANSITION DENSITY
C           MATRIX (AO BASIS, FORMED IN SUBROUTINE TOPDM)
C           TPDM4F IS THE TRANSFORMED TWO PARTICLE TRANSITION DENSITY
C           MATRIX (AO BASIS, FORMED IN TFORM4)
C           I,J,K,L REFER TO AO LABELS
C
      IJKL=0
      INDEX2=1
      INDEX3=1
      INDEX4=1
      DO 500 IPP=IB1,IB2
        NMAXJ=JB2
        IF (JJEQII) NMAXJ=IPP-1
        DO 400 JP=JB1,NMAXJ
          IF (IPP.GE.JP) THEN
            IABIJ=IA(IPP)+JP
            FOUR=4.0D+00
          ELSE
            IABIJ=IA(JP)+IPP
            FOUR=-4.0D+00
          ENDIF
          DA1=FOUR*DA(IABIJ)
          DO 300 KP=KB1,KB2
            NMAXL=LB2
            IF (LLEQKK) NMAXL=KP
            IF (KP.GE.JP) THEN
              IABKJ=IA(KP)+JP
              SIGNKJ=-3.0D+00
            ELSE
              IABKJ=IA(JP)+KP
              SIGNKJ=3.0D+00
            ENDIF
            IF (KP.GE.IPP) THEN
              IABKI=IA(KP)+IPP
              SIGNKI=3.0D+00
            ELSE
              IABKI=IA(IPP)+KP
              SIGNKI=-3.0D+00
            ENDIF
            DC2=3.0D+00*DC(IABKJ)
            DA3=SIGNKI*DA(IABKI)
            DA4=SIGNKJ*DA(IABKJ)
            DC5=3.0D+00*DC(IABKI)
C
            DO 200 LP=LB1,NMAXL
             IJKL=IJKL+1
              FACTOR=1.0D+00
              IF (KP.EQ.LP) FACTOR=0.5D+00
              IF (KP.GE.LP) THEN
                IABKL=IA(KP)+LP
              ELSE
                IABKL=IA(LP)+KP
              ENDIF
              IF (LP.GE.IPP) THEN
                IABLI=IA(LP)+IPP
                SIGNLI=1.0D+00
              ELSE
                IABLI=IA(IPP)+LP
                SIGNLI=-1.0D+00
              ENDIF
              IF (LP.GE.JP) THEN
                IABLJ=IA(LP)+JP
                SIGNLJ=-1.0D+00
              ELSE
                IABLJ=IA(JP)+LP
                SIGNLJ=1.0D+00
              ENDIF
              TDENM(IJKL)=TPDM4F(INDEX2)+DA1*DC(IABKL)
     *                    +DC2*SIGNLI*DA(IABLI)+DA3*DC(IABLJ)
     *                    +DA4*DC(IABLI)+DC5*SIGNLJ*DA(IABLJ)
              TDENM(IJKL)=FACTOR*TDENM(IJKL)
C            else
C             TDENM(IJKL)=ZERO
C            endif
              INDEX2=INDEX2+1
  200       CONTINUE
            INDEX3=INDEX3+LLN
            INDEX2=INDEX3
  300     CONTINUE
  400   CONTINUE
        INDEX4=INDEX4+LLN*KKN*JJN
        INDEX3=INDEX4
        INDEX2=INDEX3
  500 CONTINUE
C     call ddi_gsumf(2305,TDENM,ijkl)
      RETURN
      END
C
C*MODULE SOBRT   *DECK GAMMSO
      SUBROUTINE GAMMSO(ISODA,BEES,PEES,INDEXX,GAMMIJ,IGAMIJ,QS,QT,
     *                 NOCCUP,IROOTS,DEIG,IVEX,ICI1,ICI2,MAXQCI,OUT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INTEGER CSF,OVRLAP
      LOGICAL OUT,DIFVEX
      PARAMETER(ONE=1.0D+00,MAXCP=4096)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /SPINFO/ NCORE,MS,NAO,NAOD,NAO2,NAO4,MSKNAO
      COMMON /SOBUF/  IBNLEN,IBN2,MAXGAM,MAXFL,MAXFL2,ND1FZ,ND2FZ
      COMMON /SOGUG/  CP(MAXCP),NUNIQ,LIOBP,ISODAF,NSODA,LSTREC,JSODAF,
     *                JSODA,NRECJ
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
C
      DIMENSION BEES(NAO4,MXRT,MXRT),PEES(NAO2,MXRT,MXRT),INDEXX(*),
     *          GAMMIJ(*),IGAMIJ(*),QS(MAXQCI,MXRT),QT(MAXQCI,MXRT),
     *          NOCCUP(*),IROOTS(*),ISODA(*),DEIG(*),IVEX(*)
C      EQUIVALENCE( GAMMIJ,IGAMIJ)
C
C     ---------------------------------------------------------------
C     FORM THE ONE AND TWO PARTICLE TRANSITION DENSITY MATRICIES,
C     (PEES, BEES)
C     --------------------------------------------------------------
      IF(NWDVAR.EQ.1) THEN
         LEXP=32
      ELSE
         LEXP=16
      ENDIF
      MASKCI=2**LEXP-1
      DIFVEX=IVEX(ICI1).NE.IVEX(ICI2)
C
C     ---- FORM THE ONE PARTICLE TRANSITION DENSITY MATRIX
C
C     ---- INDEXX, WHICH GIVES THE NUMBER OF FORM FACTORS STORED
C     ---- FOR A GIVEN PAIR OR FOUR INDEX
C
C     if(ici1.eq.ici2) mxrt2=((mxrt+1)*mxrt)/2
C     if(ici1.ne.ici2) mxrt2=mxrt*mxrt
      CALL VCLR(PEES,1,NAO2*MXRT*MXRT)
      IJ = 0
      NORBT = LOGSHF(1,2*NAO)
      NORBT3 = LOGSHF(3,2*NAO)
      DO 1600 I=1,NAO
        DO 1500 J=1,NAO
          IJ = IJ+1
          NWORDS = INDEXX(IJ)
C
C         ---- READ IN THE ONE PARTICLE FORM FACTORS FOR ORBITAL PAIR IJ
C
          IF(NWORDS.GT.0) THEN
            IF(NWORDS.GT.MAXGAM) THEN
              IF(OUT) WRITE(IW,9000) NWORDS,MAXGAM
              CALL ABRT
            ENDIF
            CALL RAREAD(ISODAF,ISODA,GAMMIJ,NWORDS,IJ+LSTREC,0)
            NORBTJ = LOGSHF(NORBT,-J*2)
            DO 1300 CSF=2,NWORDS,2
C              INDEX = 2*(CSF-1) - 1
              INDEX = (CSF-1-1)*NWDVAR+1
              KFIGT = LOGAND( MASKCI,IGAMIJ(INDEX) )
              KFIGS = LOGAND( MASKCI,LOGSHF(IGAMIJ(INDEX),-LEXP) )
              A = ONE
              IF(DIFVEX) THEN
C               otherwise all overlaps are equal to 1 (same orbitals)
                OVRLAP = LOGXOR(NOCCUP(KFIGT),NORBTJ)
                DO 1250 II=NAO,1,-1
                  NBIT = LOGAND(3,OVRLAP)
                  OVRLAP = LOGSHF(OVRLAP,-2)
                  IF (NBIT.EQ.1.OR.(NBIT.EQ.2.AND.I.NE.J)) THEN
                    A = A*DEIG(II)
                  ELSE IF (NBIT.EQ.3) THEN
                    A = A*DEIG(II)*DEIG(II)
                  ENDIF
 1250           CONTINUE
              ENDIF
              FF=GAMMIJ(CSF)*A
              DO 1320 IROOT=1,IROOTS(ICI1)
                FF1=QS(KFIGS,IROOT)*FF
                IF(ICI1.NE.ICI2) THEN
                   MINJ=1
                ELSE
                   MINJ=IROOT
                ENDIF
                CALL DAXPY(IROOTS(ICI2)-MINJ+1,FF1,QT(KFIGT,MINJ),
     *                     MAXQCI,PEES(IJ,IROOT,MINJ),NAO2*MXRT)
C               do 1310 jroot=minj,iroots(ici2)
C                 if(ijsym(iroot,jroot).ne.0.or.nosym.gt.0) then
C                 PEES(IJ,iroot,jroot)=PEES(IJ,iroot,jroot)+ff1*
C    *                                QT(KFIGT,jroot)
C1310           continue
 1320         CONTINUE
C
C             The sum runs even over symmetry forbidden terms;
C             but the condition inside the two loops may be as bad if
C             not worth as multiplication, addition and storage
C             (given the internal parallel design of modern processors)
C
 1300       CONTINUE
          ENDIF
 1500   CONTINUE
 1600 CONTINUE
      CALL DDI_GSUMF(2300,PEES,NAO2*MXRT*MXRT)
C
C     ---- FORM THE TWO PARTICLE TRANSITION DENSITY MATRIX
C
      NRECRD = NAO2
      NOFF=NAO2
      CALL VCLR(BEES,1,NAO4*MXRT*MXRT)
      IJKL = 0
      DO 2200 I=1,NAO
        DO 2100 J=1,NAO
          DO 2000 K=1,NAO
            DO 1900 L=1,NAO
              IJKL = IJKL+1
              NRECRD = NRECRD+1
              IF (K.NE.L) THEN
                NORBTK=LOGSHF(NORBT,-K*2)
                NORBTL=LOGSHF(NORBT,-L*2)
                NORBTB=LOGOR(NORBTK,NORBTL)
              ELSE
                NORBTB=LOGSHF(NORBT3,-K*2)
              ENDIF
              NWORDS = INDEXX(NRECRD)
C
C             ---- READ IN THE TWO PARTICLE FORM FACTORS FOR IJKL
C
              IF(NWORDS.GT.0) THEN
                IF(NWORDS.GT.MAXGAM) THEN
                  IF(OUT) WRITE(IW,9000) NWORDS,MAXGAM
                  CALL ABRT
                ENDIF
              CALL RAREAD(ISODAF,ISODA,GAMMIJ,NWORDS,NOFF+IJKL+LSTREC,0)
                DO 1700 CSF=2,NWORDS,2
                  INDEX = (CSF-1-1)*NWDVAR+1
                  KFIGT = LOGAND( MASKCI,IGAMIJ(INDEX) )
                  KFIGS = LOGAND( MASKCI,LOGSHF(IGAMIJ(INDEX),-LEXP) )
                  A=ONE
                  IF(DIFVEX) THEN
                    OVRLAP=LOGXOR(NOCCUP(KFIGT),NORBTB)
                    DO 1650 II=NAO,1,-1
                      NBIT = LOGAND(3,OVRLAP)
                      OVRLAP = LOGSHF(OVRLAP,-2)
                      IF (NBIT.EQ.1.OR.(NBIT.EQ.2.AND.K.NE.L)) THEN
                        A = A*DEIG(II)
                      ELSE IF (NBIT .EQ. 3) THEN
                        A = A*DEIG(II)*DEIG(II)
                      ENDIF
 1650               CONTINUE
                  ENDIF
                  FF=GAMMIJ(CSF)*A
                  DO 1680 IROOT=1,IROOTS(ICI1)
                    FF1=QS(KFIGS,IROOT)*FF
                    IF(ICI1.NE.ICI2) THEN
                       MINJ=1
                    ELSE
                       MINJ=IROOT
                    ENDIF
                    CALL DAXPY(IROOTS(ICI2)-MINJ+1,FF1,QT(KFIGT,MINJ),
     *                         MAXQCI,BEES(IJKL,IROOT,MINJ),NAO4*MXRT)
C                   do 1670 jroot=minj,iroots(ici2)
C                     BEES(IJKL,iroot,jroot)=BEES(IJKL,iroot,jroot)+ff1
C    *                               *QT(KFIGT,jroot)
C1670               continue
 1680             CONTINUE
 1700           CONTINUE
              ENDIF
 1900       CONTINUE
 2000     CONTINUE
 2100   CONTINUE
 2200 CONTINUE
      CALL DDI_GSUMF(2301,BEES,NAO4*MXRT*MXRT)
      RETURN
 9000 FORMAT(/1X,'BEQUEATH THYSELF A LARGER BUFFER IN GAMMSO',I10,I10)
      END
C
C*MODULE SOBRT   *DECK BRTHSO
      SUBROUTINE BRTHSO(HSO,HSO1,ICI1,ICI2,ISTATE1,ISTATE2,NWKSST,C1,C2,
     *                  DA,DC,OPDM2,OPDM1,TPDM2,TPDM22,TPDM3,TPDM32,
     *                  TPDM33,TPDM34,TPDM4F,BEES,PEES,PKL,TFORM,QS,
     *                  QT,TDENM,MAXQCI,MAXFUN,MAXP,MAXP1,ND51,ND52,XINT
     *                 ,YINT,ZINT,XINTI,YINTI,ZINTI,XINTJ,YINTJ,ZINTJ,IA
     *                 ,ZNUX,NSAVE,L2,SO1AO,N2AO,SO2AO,NSO2BUF,SO2BUF,
     *                  N2INT,NFT2SO,GSYLYES,DISTINT,IONECNT,OUT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      COMPLEX*16 HSO,HSO1
C
      PARAMETER(MXATM=500,MXSH=1000,MXGTOT=5000,ZERO=0.0D+00)
C
      LOGICAL GSYLYES(3),OUT,IIEQJJ,KKEQLL,JJEQII,LLEQKK,GOPARR,DSKWRK,
     *        MASWRK,DISTINT
C
      DIMENSION C1(*),C2(*),DA(*),DC(*),OPDM2(*),OPDM1(*),TPDM2(*),
     *          TPDM22(*),TPDM3(*),TPDM32(*),TPDM33(*),TPDM34(*),IA(*),
     *          TPDM4F(*),BEES(NAO4,MXRT,MXRT),PEES(NAO2,MXRT,MXRT),
     *          PKL(*),TFORM(*),TDENM(*),QS(MAXQCI,MXRT),QT(MAXQCI,MXRT)
     *         ,NWKSST(*),XINT(*),YINT(*),ZINT(*),XINTI(*),YINTI(*),
     *          ZINTI(*),XINTJ(*),YINTJ(*),ZINTJ(*),SO1AO(L2,3),
     *          SO2AO(3,N2AO,N2AO,N2AO,N2AO),ZNUX(MXATM),
     *          SO2BUF(NSO2BUF,3),N2INT(3),NFT2SO(3),IPOINT(3)
C
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),MIN(MXSH),MAX(MXSH),NSHELL
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /SHLDAT/ XI,YI,ZI,XJ,YJ,ZJ,XK,YK,ZK,XL,YL,ZL,
     1                IIATM,MINII,MAXII,IP1,IP2,
     2                JJATM,MINJJ,MAXJJ,JP1,JP2,
     3                KKATM,MINKK,MAXKK,KP1,KP2,
     4                LLATM,MINLL,MAXLL,LP1,LP2,
     5                IB1,IB2,JB1,JB2,KB1,KB2,LB1,LB2,
     *                IIEQJJ,KKEQLL,JJEQII,LLEQKK
      COMMON /DM3/    LOKII,LOKJJ,LOKKK,LOKLL,IIN,JJN,KKN,LLN,IJN
      COMMON /SETD/   BP01,B00,B10,XCP00,XC00,YCP00,YC00,ZCP00,ZC00,
     1                F00,DXIJ,DYIJ,DZIJ,DXKL,DYKL,DZKL,
     2                ILAM,JLAM,KLAM,LLAM,NDER,
     3                MAXI,MAXJ,MAXK,MAXL,MAXIJ,MAXKL,MAXNM,JKLN
      COMMON /AIAJ/   T12,XOI,YOI,ZOI,T12LAM,SOC1ER,SOC2ER,AI,AJ
     *                ,SOC1EI,SOC2EI,ILAMM1
      COMMON /SPINFO/ NCORE,MS,NAO,NAOD,NAO2,NAO4,MSKNAO
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RELWFN/ RMETHOD,QRQMT,CLIG,CLIG2,QRTOL,IQRORD,MODQR,NESOC,
     *                NRATOM,NUMU,NQMTR,NQRDAF,MORDA,NDARELB
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA E/2.30258D+00/,ANESC/8HNESC    /
#else
      CHARACTER*8 :: ANESC_STR
      EQUIVALENCE (ANESC, ANESC_STR)
      DATA E/2.30258D+00/,ANESC_STR/"NESC    "/
#endif
C
C     C1 - SINGLET, C2 - TRIPLET MOLCAO COEFFICIENTS
C     ----------------------------------------------------------
C     COMPUTE THE MATRIX ELEMENT <Y(SINGLET)/HSO/Y(TRIPLET,MS)>
C     USING BOTH THE ONE AND TWO ELECTRON SPIN ORBIT TERMS IN
C     THE BREIT-PAULI HAMILTONIAN
C     ----------------------------------------------------------
C
C     nsave is a parameter for storing the 1 and 2e integrals:
C           = 0 no storage (form-factor driven PB)
C           = 1 store 1e only (direct Zeff)
C           = 2 store 1 and 2e (direct PB)
C     ionecnt tells whether to use only one centre 2e integrals
C     (1e SOC integrals are always computed fully).
C           = 0 compute all integrals
C           = 1 compute one-centre 1e and one-centre 2e SOc integrals
C           = 2 compute all 1e and one-centre 2e integrals
C
      FSC=7.2973506D-03
      CONFAC=219474.6D+00
      FSCSQ2=0.5D+00*FSC*FSC
C
      LFACTO=1+NCORE*NUM
C     LFACTO POINTS TO THE FIRST ACTIVE ORBITAL COEFFICIENT
C
C     this flipping is OK for 1e terms only!
C
      IF(RMETHOD.EQ.ANESC.AND.NESOC.GT.0) CALL FLIPBASIS(4)
      IF(OUT.AND.NSAVE.EQ.0) THEN
         WRITE(IW,8004) ICI1
         WRITE(IW,8001)(QS(KK,ISTATE1),KK=1,NWKSST(ICI1))
         WRITE(IW,8004) ICI2
         WRITE(IW,8001)(QT(KK,ISTATE2),KK=1,NWKSST(ICI2))
         WRITE(IW,9990)
         WRITE(IW,9991)(PEES(KL,ISTATE1,ISTATE2),KL=1,NAO2)
         WRITE(IW,9992)
         WRITE(IW,9994)(BEES(KL,ISTATE1,ISTATE2),KL=1,NAO4)
      ENDIF
      IF(NSAVE.EQ.0) CALL TOPDM(NUM,C1(LFACTO),C2(LFACTO),
     *                          PEES(1,ISTATE1,ISTATE2),DA,OPDM2,OPDM1)
      IF(NSAVE.GT.0.AND.MS.EQ.0) CALL VCLR(SO1AO,1,L2*3)
      CALL VICLR(IPOINT,1,3)
      MATURE=1
      SOC1ER=ZERO
      SOC2ER=ZERO
      SOC1EI=ZERO
      SOC2EI=ZERO
      IST=1
      JST=1
      KST=1
      LST=1
      IJKL=0
C      ICUT0 = IABS(ICUT)
C      IF (ICUT0 .LE. 0) ICUT0=9
C      CUTOFF = 1.0D+00/(10.0D+00**ICUT0)
      IF (ITOL .EQ. 0) ITOL=20
      TOL = E*ITOL
C      ------------------ DEFINITIONS ---------------------------
C
C        KSTART(II)      ADDRESS OF 1ST PRIMITIVE IN SHELL II
C        KNG(II)         NUMBER OF PRIMITIVES IN SHELL II
C        KMIN(II)        1ST SUBSHELL INDEX FOR SHELL II
C        KMAX(II)        LAST SUBSHELL INDEX FOR SHELL II
C        KLOC(II)        SERIAL INDEX OF 1ST FUNCTION IN SHELL II
C        KATOM(II)       ATOM ON WHICH SHELL II IS CENTERED
C        C(1,IIATM)      X COORDINATE OF ATOM IIATM
C        C(2,IIATM)      Y COORDINATE OF ATOM IIATM
C        C(3,IIATM)      Z COORDINATE OF ATOM IIATM
C        KTYPE(II)       RELATED TO ANGULAR MOMENTUM OF SHELL II
C        CS()            S-TYPE CONTRACTION COEFICIENTS
C        CP()            P-TYPE CONTRACTION COEFICIENTS
C        CD()            D-TYPE CONTRACTION COEFICIENTS
C        EX()            EXPONENTIAL PARAMETERS
C        NUM             NUMBER OF BASIS FUNCTIONS
C
C      ----------------------------------------------------------
C
C     ----- ISHELL -----
C
      DO 9000 II=IST,NSHELL
        IF(NSAVE.EQ.2.AND.MASWRK) WRITE(6,*) 'II=',II
        ILAM=KTYPE(II)-1
        IP1=KSTART(II)
        IP2=KSTART(II)-1+KNG(II)
        MINII=MIN(II)
        MAXII=MAX(II)
        IIN=MAXII+1-MINII
        LOKII=KLOC(II)-1
        IB1=KLOC(II)
        IB2=IB1+IIN-1
        IIATM=KATOM(II)
        XI=C(1,IIATM)
        YI=C(2,IIATM)
        ZI=C(3,IIATM)
        ILSHELL=0
        IF(ILAM.EQ.1.AND.MINII.EQ.1) ILSHELL=1
        ICOUNTS=0
        IF(ILAM.EQ.0) ICOUNTS=1
C
C        ----- JSHELL -----
C
        J0=JST
        DO 8000 JJ=J0,II
          JST=1
          JLAM=KTYPE(JJ)-1
          JP1=KSTART(JJ)
          JP2=KSTART(JJ)-1+KNG(JJ)
          MINJJ=MIN(JJ)
          MAXJJ=MAX(JJ)
          JJN=MAXJJ+1-MINJJ
          LOKJJ=KLOC(JJ)-1
          JB1=KLOC(JJ)
          JB2=JB1+JJN-1
          JJATM=KATOM(JJ)
          IF(IONECNT.EQ.1.AND.IIATM.NE.JJATM) GOTO 8000
          JLSHELL=0
          IF(JLAM.EQ.1.AND.MINJJ.EQ.1) JLSHELL=1
          JCOUNTS=0
          IF(JLAM.EQ.0) JCOUNTS=1
          XJ=C(1,JJATM)
          YJ=C(2,JJATM)
          ZJ=C(3,JJATM)
          IIEQJJ=II.EQ.JJ
          JJEQII=IIEQJJ
          IJN = IIN*JJN
          IF(NSAVE.EQ.0) CALL TFORM2(NUM,C1(LFACTO),C2(LFACTO),
     *        BEES(1,ISTATE1,ISTATE2),TPDM2,TPDM22,NAO,PKL,TFORM,MAXFUN)
          CALL SOINT1(TOL,IA,DA,MAXP,XINT,YINT,ZINT,XINTI,YINTI,ZINTI,
     *               XINTJ,YINTJ,ZINTJ,ZNUX,NSAVE,L2,SO1AO)
          IF(NSAVE.EQ.1.OR.(IONECNT.GT.0.AND.IIATM.NE.JJATM)) GOTO 8000
C
C         ----- KSHELL -----
C
          K0=KST
          DO 7000 KK=K0,NSHELL
            KST=1
            KLAM=KTYPE(KK)-1
            KP1=KSTART(KK)
            KP2=KSTART(KK)-1+KNG(KK)
            MINKK=MIN(KK)
            MAXKK=MAX(KK)
            KKN=MAXKK+1-MINKK
            LOKKK=KLOC(KK)-1
            KB1=KLOC(KK)
            KB2=KB1+KKN-1
            KKATM=KATOM(KK)
            IF(IONECNT.GT.0.AND.JJATM.NE.KKATM) GOTO 7000
            KLSHELL=0
            IF(KLAM.EQ.1.AND.MINKK.EQ.1) KLSHELL=1
            KCOUNTS=0
            IF(KLAM.EQ.0) KCOUNTS=1
            XK=C(1,KKATM)
            YK=C(2,KKATM)
            ZK=C(3,KKATM)
            IF(NSAVE.EQ.0) CALL TFORM3(NUM,C1(LFACTO),C2(LFACTO),TPDM2,
     *                               TPDM22,TPDM3,TPDM32,TPDM33,TPDM34)
C
C           ----- LSHELL -----
C
            L0=LST
            DO 6000 LL=L0,KK
              LST=1
              IJKL=IJKL+1
              IF(DISTINT.AND.MOD(IJKL,NPROC).NE.ME) GOTO 6000
C             do not parallelise integrals for direct driver
C
C             ----- CALCULATE Q4 FACTOR FOR THIS GROUP OF SHELLS -----
C
              LLAM=KTYPE(LL)-1
              LP1=KSTART(LL)
              LP2=KSTART(LL)-1+KNG(LL)
              MINLL=MIN(LL)
              MAXLL=MAX(LL)
              LLN=MAXLL+1-MINLL
              LOKLL=KLOC(LL)-1
              LB1=KLOC(LL)
              LB2=LB1+LLN-1
              LLATM=KATOM(LL)
C
C             if only one-centre integrals are requested,
C             skip some integrals.
C
              IF(IONECNT.GT.0.AND.KKATM.NE.LLATM) GOTO 6000
C
C             parity is conserved for one-centre integrals
C             total parity should be even
C             note that Gaussian D,F,G shells have contaminants
C             of the same parity so that no separate check is needed.
C             l-shells do not have definite parity so if any l is
C             involved,  no symmetry savings are gained.
C             Secondly, use angular momentum symmetry.
C             the total product of l for all four shells times the
C             momentum for the operator (l=1) should contain l=0.
C             In practice it only filters out (ss|sl) and permutations,
C             where l is any shell (note that (ss|sp) is allowed by
C             this rule but forbidden by parity), same for f.
C
              LLSHELL=0
              IF(LLAM.EQ.1.AND.MINLL.EQ.1) LLSHELL=1
              LCOUNTS=0
              IF(LLAM.EQ.0) LCOUNTS=1
              IF(IIATM.EQ.JJATM.AND.JJATM.EQ.KKATM.AND.KKATM.EQ.LLATM
     *      .AND.ILSHELL+JLSHELL+KLSHELL+LLSHELL.EQ.0.AND.
     *           (MOD(ILAM+JLAM+KLAM+LLAM,2).EQ.1.OR.
     *           ICOUNTS+JCOUNTS+KCOUNTS+LCOUNTS.GE.3)) GOTO 6000
              XL=C(1,LLATM)
              YL=C(2,LLATM)
              ZL=C(3,LLATM)
              KKEQLL=KK.EQ.LL
              LLEQKK=KKEQLL
C
C ***         FILL TPDM4 WITH CURRENT BLOCK OF
C ***         TWO-PARTICLE DENSITY MATRIX ELEMENTS IN AO BASIS.
C
              IF(NSAVE.EQ.0) THEN
                 CALL TFORM4(NUM,C1(LFACTO),C2(LFACTO),TPDM3,TPDM32,
     *                       TPDM33,TPDM34,TPDM4F)
                 CALL DENMAT(IA,DA,DC,TPDM4F,TDENM)
              ENDIF
              IF(MATURE.NE.0.AND.NSAVE.GT.0)
     *           CALL VCLR(SO2AO,1,3*N2AO*N2AO*N2AO*N2AO)
              MATURE=0
              CALL SOINT2(TOL,TDENM,MAXP,MAXP1,ND51,ND52,XINT,YINT,ZINT,
     *                    XINTI,YINTI,ZINTI,XINTJ,YINTJ,ZINTJ,NSAVE
     *                   ,N2AO,SO2AO,MATURE,GSYLYES)
              IF(MATURE.GT.0.AND.NSAVE.GT.0) CALL PACK2SO(N2AO,SO2AO,
     *                       NSO2BUF,SO2BUF,N2INT,IPOINT,NFT2SO,GSYLYES)
 6000       CONTINUE
 7000     CONTINUE
 8000   CONTINUE
 9000 CONTINUE
C *** END OF 'SHELL' LOOPS
C     CALL DDI_GSUMF(2302,SOC1ER,1)
C     CALL DDI_GSUMF(2303,SOC1EI,1)
      IF(NSAVE.EQ.0) THEN
        CALL DDI_GSUMF(2304,SOC2ER,1)
        CALL DDI_GSUMF(2305,SOC2EI,1)
      ELSE IF(NSAVE.GT.1) THEN
C       save left-overs
        DO 9005 KART=1,3
         IF(.NOT.GSYLYES(KART)) GOTO 9005
         KPOINT=IPOINT(KART)
         WRITE(NFT2SO(KART)) KPOINT
         IF(KPOINT.GT.0) WRITE(NFT2SO(KART)) (SO2BUF(M,KART),M=1,KPOINT)
         N2INT(KART)=N2INT(KART)+KPOINT
 9005   CONTINUE
      ENDIF
C
C *** CALCULATE ONE AND TWO PARTICLE CONTRIBUTIONS TO THE SOC CONSTANT
C
C     SOC1ER  = -FSCSQ2*CONFAC*SOC1ER
C     SOC2ER  =  FSCSQ2*CONFAC*SOC2ER
C     SOC1EI  = -FSCSQ2*CONFAC*SOC1EI
C     SOC2EI  =  FSCSQ2*CONFAC*SOC2EI
C     phase correction: (thesis has correct phases but not the code)
C     note that 2-e part has an extra minus in the operators
C     otherwise the correction is a+ib -> -b-ia
C     also for ms=+-1 somewhere a minus is missing? (from S- ??)
      SOC1EIA = -FSCSQ2*CONFAC*SOC1ER
      SOC2EIA =  FSCSQ2*CONFAC*SOC2ER
      SOC1ER  = -FSCSQ2*CONFAC*SOC1EI
      SOC2ER  =  FSCSQ2*CONFAC*SOC2EI
      SOC1EI  =  SOC1EIA
      SOC2EI  =  SOC2EIA
      IF(MS.NE.0) THEN
         SOC1ER=-SOC1ER
         SOC2ER=-SOC2ER
         SOC1EI=-SOC1EI
         SOC2EI=-SOC2EI
      ENDIF
      HSO=DCMPLX(SOC1ER+SOC2ER,SOC1EI+SOC2EI)
      HSO1=DCMPLX(SOC1ER,SOC1EI)
      IF(RMETHOD.EQ.ANESC.AND.NESOC.GT.0) CALL FLIPBASIS(0)
      RETURN
 8001 FORMAT(T5,6(2X,E15.8))
 8004 FORMAT(///,5X,'CI COEFFICIENTS OF CI N ',I2,/)
 9990 FORMAT(///20X,'ONE PARTICLE FORM FACTORS MO BASIS',/)
 9991 FORMAT(8(2X,E14.7))
 9992 FORMAT(///20X,'TWO PARTICLE FORM FACTORS MO BASIS ',/)
 9994 FORMAT(8(2X,E14.7))
      END
C
C*MODULE SOBRT   *DECK REAPCI
      SUBROUTINE REAPCI(NFTCI,ICI1,NWKSST,IROOTS,QA,MAXQCI)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION QA(MAXQCI,MXRT),IROOTS(*),NWKSST(*)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
C
C     ---- READ CI COEFFICIENTS
C
      CALL SEQREW(NFTCI)
      DO 300 JCI=1,ICI1-1
         DO 200 I=1,IROOTS(JCI)
            READ(NFTCI) (JDUM,J=1,NWKSST(JCI))
  200    CONTINUE
  300 CONTINUE
      I=JDUM
C     ftncheck thang
      DO 400 I=1,IROOTS(ICI1)
         READ(NFTCI) (QA(J,I),J=1,NWKSST(ICI1))
  400 CONTINUE
C
      RETURN
      END
C
C*MODULE SOBRT   *DECK PREINIT
      SUBROUTINE PREINIT(MAXIA,C1,DC,IA,JOB)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MXATM=500,ZERO=0.0D+00)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /SPINFO/ NCORE,MS,NAO,NAOD,NAO2,NAO4,MSKNAO
      DIMENSION C1(*),DC(*),IA(*)
C
C     job=0 only IA, otherwise also DC
C
      IA(1) = 0
      DO 5 I=2,MAXIA
        IA(I) = IA(I-1) + (I-1)
 5    CONTINUE
      IF(JOB.EQ.0) RETURN
C
C     ---- SET STARTING PARAMETERS
C
C     ---- FORM THE CORE DENSITY MATRIX IN THE AO BASIS ----
C     NOTE THE CORE ORBITALS ARE IDENTICAL FOR THE SINGLET AND
C     TRIPLET COEFFICIENTS
C
      IJ=0
      DO 500 I=1,NUM
         DO 450 J=1,I
           SUM=ZERO
           DO 425 K=1,NCORE
             SUM = SUM + C1(I+(K-1)*NUM)*C1(J+(K-1)*NUM)
  425      CONTINUE
           IJ = IJ + 1
           DC(IJ) = SUM
  450    CONTINUE
  500 CONTINUE
C      CALL MRTRBT(C1,NUM,NUM,NCORE,C1,NUM,DC,1,1)
C     CANNOT USE BECAUSE NEED C1*C1', NOT C1'*C1
C     OVERWRITE THE CORE SINGL/TRIPL. CORE OVERLAP WHICH IS 1 ANYWAY
C     DO 7010 I=1,NAO
C        DEIG(I)=DEIG(I+NCORE)
C010  CONTINUE
      RETURN
      END
C
C*MODULE SOBRT   *DECK SOBRTX
      SUBROUTINE SOBRTX(NFTS1,PRTPRM,ADD2E,ZEFF,SAVDSK,MSONLY,ACTION,
     *                  MAXL,NZSPIN,DEIG,ENGYST,NWKSST,MULST,IROOTS,IVEX
     *                 ,IOBP,ISTSYM,IRCIOR,IRRL,GSYLYES,IGAMMA,NOSYM,
     *                  SLOWFF,PHYSRC,IPRHSO,HSOTOL,MODPAR,IONECNT)
Cdg                    ,ndim,NIJDR,ijdrep)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,OUT,ADD2E,SAVDSK,ALLOWED,PRTPRM,LRES,
     *        SLOWFF,PHYSRC,GSYLYES(3),SAMEMUL
      PARAMETER (MAXNAO=26,NPAR=4,ONE=1.0D+00,TWO=2.0D+00,ZERO=0.0D+00)
      PARAMETER (MXATM=500,MXSH=1000,MAXCP=4096)
C     npar gives the number of integer parameters (msonly,nao,...) saved
C     on DA for restart check
C
      COMPLEX*16 HSO2, HSO1, CZERO
      DIMENSION MXGAMS(MAXNAO),LVST(2),DEIG(*),ENGYST(MXRT,*),NWKSST(*),
     *          MULST(*),IROOTS(*),IVEX(*),IOBP(3,*),ISTSYM(MXRT,*),
     *          IRCIOR(MXRT,*),IRRL(3),KARTEN(0:4),ZEFF(MXATM)
Cdg  *          ,NIJDR(nirred,ndim),ijdrep(nirred,NDIM,*)
C
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      COMMON /GUGWFN/ NFZC1,NMCC,NDOC,NAOS,NBOS,NALP,NVAL,NEXT,NFZV,
     *                IFORS,IEXCIT,ICICI,NOIRR
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /SPINFO/ NCORE,MS,NAO,NAOD,NAO2,NAO4,MSKNAO
      COMMON /SOBUF/  IBNLEN,IBN2,MAXGAM,MAXFL,MAXFL2,ND1FZ,ND2FZ
      COMMON /RESTAR/ TIMLIM,IREST,NREC,INTLOC,IST,JST,KST,LST
      COMMON /SOGUG/  CP(MAXCP),NUNIQ,LIOBP,ISODAF,NSODA,LSTREC,JSODAF,
     *                JSODA,NRECJ
Cdg   COMMON /SYMBLK/ NIRRED,NSALC,NSALC2,NSALC3
C     COMMON/SHLNRM/PNRM(35)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DEBUG,DBGME/8HDEBUG   ,8HBRTHSO  /,CHECK/8HCHECK   /,
     *     USUAL/8HNORMAL  /,DOSAVE/8HSAVE    /,DOREAD/8HREAD    /
#else
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBGME_STR
      EQUIVALENCE (DBGME, DBGME_STR)
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      CHARACTER*8 :: USUAL_STR
      EQUIVALENCE (USUAL, USUAL_STR)
      CHARACTER*8 :: DOSAVE_STR
      EQUIVALENCE (DOSAVE, DOSAVE_STR)
      CHARACTER*8 :: DOREAD_STR
      EQUIVALENCE (DOREAD, DOREAD_STR)
      DATA DEBUG_STR,DBGME_STR/"DEBUG   ","BRTHSO  "/,
     *     CHECK_STR/"CHECK   "/, USUAL_STR/"NORMAL  "/,
     *     DOSAVE_STR/"SAVE    "/,DOREAD_STR/"READ    "/
#endif
      DATA KARTEN/1,4,6,10,15/
C     data mxgams/0,2,4,24,68,372,1192,6710,22358,128802,
      DATA MXGAMS/0,2,4,30,136,672,1500,10290,39096,128802,
     *            999998,999998,999998,999998,999998,
     *            999998,999998,999998,999998,999998,
     *            999998,999998,999998,999998,999998,999998/
C
C     these are experimentally determined maximum lengths of binary
C     record on DA file for each NAO. The size of DAF is quasi-
C     proportional to mxgams(nao) <consider 2047 record length chunks>.
C     The numbers for nao>10 are not known.
C     it is sufficent to have these number greater or equal than
C     necessary.
C
C     MAXFUN - THE MAXIMUM DEGENERACY OF ORBITALS (S,D)==6
C     MAXP - THE MAXIMUM L-QUANTUM NUMBER+1 (THE NUMBER OF L'S)
C     MAXP1= MAXP+1 (NEED TO DO 1 EXTRA L FOR SO INTEGRALS)
C
C     The formal Ms values (which is the difference between the two
C     multiplicities) is set in such a way so that in corresponds to
C     (LS) -> Lms S-ms, the only non-zero contribution from LS
C     for a given set S,Ms, S',Ms'.
C
C     karten gives the number of idependent components of Cartesian
C            tensors as a function of L (true L)
C     kardeg gives the number of components of Cartesian tensors
C            as a function of pseudo-L
C     they should be the same except for L-shells
C     also beware M-shells and such
C
      IF(MASWRK) THEN
         WRITE(IW,9001)
         IF(ACTION.NE.DOREAD) WRITE(IW,9002)
         IF(ACTION.EQ.DOREAD) WRITE(IW,9004)
         IF(SAVDSK) THEN
            WRITE(IW,9002)
            WRITE(IW,9006)
         ENDIF
         IF(MSONLY.NE.-3) WRITE(IW,9003)
         IF(MSONLY.EQ.-2) WRITE(IW,9007)
         IF(ACTION.EQ.DOSAVE) WRITE(IW,9005)
         IF(MSONLY.EQ.-2) WRITE(IW,9008)
      ENDIF
      IBTYP1=MIN(IBTYP,LOGAND(MODPAR,1))
      IF(.NOT.GOPARR) IBTYP1=0
      IF(IBTYP1.EQ.1.AND.PHYSRC) THEN
         WRITE(IW,*) 'SETTING PHYSRC TO .FALSE.'
         PHYSRC=.FALSE.
C        each run in dynamic load is non-uniques, no stats can be acc'ed
      ENDIF
      CZERO=DCMPLX(ZERO,ZERO)
      OUT=(NPRINT.EQ.5.OR.EXETYP.EQ.DEBUG.OR.EXETYP.EQ.DBGME).AND.MASWRK
      NCORE = NFZC1 + NMCC
      NAO = NDOC + NAOS + NBOS + NALP + NVAL + NEXT
      NAO2 = NAO*NAO
      NAO4 = NAO2*NAO2
      NAOD = NAO+NAO
      MSKNAO = 2**NAO - 1
      MAXFUN=KARTEN(MAXL)
      NHSO=0
      MAXIOBP=0
      MAXQCI=0
      DO 100 ICI=1,NUMCI
         NHSO=NHSO+IROOTS(ICI)*MULST(ICI)
         MAXIOBP=MAX(IOBP(1,ICI),IOBP(2,ICI),IOBP(3,ICI),MAXIOBP)
         MAXQCI=MAX(NWKSST(ICI),MAXQCI)
  100 CONTINUE
      MAXGAM=MXGAMS(NAO)
      IF(MAXGAM.EQ.0) MAXGAM=MXGAMS(MAXNAO)
      IF(GOPARR) THEN
C
C        maxgam=maxgam/nproc
C        the work will not be exactly evenly distributed, some nodes
C        will have more form factors to store (see CIMAT)
C        maxqci/nao takes care of "dispersion"
C
         DUMMY = NPROC
         NPROC2=INT(SQRT(DUMMY)+0.5D+00)
         IF(IBTYP1.EQ.1) THEN
C           dynamic
            MAXX=(MAXGAM-1)/NPROC+1+NAO2*NPROC2
         ELSE
C           static
            MAXX=(MAXGAM-1)/NPROC+1+NAO2*NAO*NPROC2
         ENDIF
         MAXGAM=MIN(MAXGAM,MAXX)
C        make sure it is even
         MAXGAM=((MAXGAM+1)/2)*2
         IF(MASWRK.AND.IBTYP1.EQ.0) WRITE(IW,8910) 'STATIC', MAXGAM
         IF(MASWRK.AND.IBTYP1.EQ.1) WRITE(IW,8910) 'DYNAMIC',MAXGAM
      ENDIF
      IF(IGAMMA.NE.0) MAXGAM=IGAMMA
C     in parallel runs, reclen in $TRANST sets maxgam on each node
      IF(IGAMMA.NE.0.AND.MASWRK) WRITE(IW,8920) IGAMMA
      MAXFL2=MAXGAM*NAO
      MAXFL= MAXFL2/2
      MAXIA=MAX(MXSH,MXRT)
      NF2=MAXFUN*MAXFUN
      NF3=MAXFUN*NF2
      NF4=NF2*NF2
      MAXP=MAXL+1
      MAXP1=MAXP+1
      ND51=MAXP*MAXP
      ND52=MAXP1*MAXP1
C     write(iw,*) 'parrs',MAXL,MAXP,MAXP1,ND51,ND52,MAXFUN
C
C     NSODA IS THE NUMBER OF RECORDS IN ISODA per node
C     it is exactly equal to the number orbital indices even though only
C     first nao2+nao4 records are actually needed for SOC (the other
C     4*nao2 core FF are recalculated in the SOC code from the first
C     nao4+nao2 FFs)
C     maxgam gives maximum number of nonzero FFs for a given orbital
C     index (i.e. IJ or IJKL).
C     maxfl gives max number of FFs for a given orbital index, this
C     however includes FFs equal to zero, which are many, zero FFs will
C     not be saved, but only stored temporarily
C     maxfl is for IJ, maxfl2 for IJKL
C     both are hoped to suffice
C
      NSODA=NAO4+NAO2
      NSODA1=NSODA
      IF(ACTION.NE.USUAL.AND.MSONLY.LE.-2) NSODA=NSODA*2
C     -2 means MS=0,1
      NSODAT=0
      IF(ACTION.NE.USUAL.AND.MSONLY.LE.-2) NSODAT=3
      IF(ACTION.NE.USUAL.AND.MSONLY.GT.-1) NSODAT=2
C     reserve two or three records for technical info
      NSODA=NSODA+NSODAT
      ISODAF=30
      NCOPCON=MXRT*MXRT*NUMCI*NUMCI
C
C     L1=NUM
      L2=(NUM*NUM+NUM)/2
      L3=NUM*NUM
      CALL VALFM(LOADFM)
C
C     SOME of these ARE USED BY BOTH FORM FACTORS AND SOCC
C     the rest is read only once so it is in the common area
C
      LDC    = LOADFM+1
      LHSO   = LDC   + L2
      LEIG   = LHSO  + NHSO*NHSO*2
      LVST(1)= LEIG  + NHSO
      LVST(2)= LVST(1) + L3
      LGAMMA = LVST(2) + L3
      LISO   = LGAMMA+ MAXGAM
      LNOCCU = LISO  + (NSODA-1+NWDVAR+NPAR)/NWDVAR+1
C     liso will hold 2 extra integers and one real for some options
      LINDEX = LNOCCU+ (MAXQCI+NWDVAR-1)/NWDVAR
C     only triplet occupancies are needed
      LCOPCON= LINDEX+ (NAO2+NAO4+NWDVAR-1)/NWDVAR
      LCOPCON1=LCOPCON+NCOPCON
      LIA     =LCOPCON1+NCOPCON
C
C     THE FOLLOWING IS PECULIAR TO FORM FACTORS
C
      LBUFII= LIA   + (MAXIA+NWDVAR-1)/NWDVAR
      LBUFJJ= LBUFII+ (MAXIOBP-1)/NWDVAR+1
      LIFLGZ= LBUFJJ+ (MAXIOBP-1)/NWDVAR+1
      LJFLGZ= LIFLGZ+ (MAXFL-1)/NWDVAR+1
      LLOCA = LJFLGZ+ (MAXFL2-1)/NWDVAR+1
      LLOCB = LLOCA + (NAO-1)/NWDVAR+1
      LBP1Z = LLOCB + (NAO-1)/NWDVAR+1
      LBP2Z = LBP1Z + (NAO2+NWDVAR-1)/NWDVAR
      LIBNP1= LBP2Z + (NAO4+NWDVAR-1)/NWDVAR
      IF(SAVDSK) THEN
         NIBNP1=(NAO2+NAO4-1)/NWDVAR+1
      ELSE
         NIBNP1=0
      ENDIF
      LD1Z  = LIBNP1+ NIBNP1
      LD2Z  = LD1Z  + NAO2
      LAST  = LD2Z  + NAO4
C
C     work out the size of the arrays for sorting, get all the memory
C     available
C
      LEFT=LIMFM-LAST
      IBNLEN=LEFT/(NAO2+NAO4)
      IF(IBNLEN.GT.MAXGAM) IBNLEN=MAXGAM
C
C     make ibnlen a multiple of the physical record size
C
      LDARISO=NRASIZ(ISODAF)
      IF(PHYSRC) THEN
         IBNLEN=(IBNLEN/2)*2
         LDARISO=IBNLEN
      ENDIF
      IF(IBNLEN.EQ.MAXGAM) SLOWFF=.TRUE.
C     in this case the fast algorythm is better not to use
C     with no loss in speed
C
      IF(.NOT.SLOWFF) THEN
         IF(IBNLEN.GT.LDARISO) IBNLEN=(IBNLEN/LDARISO)*LDARISO
         IF(IBNLEN.LT.LDARISO) THEN
            IF(MASWRK) WRITE(IW,9100) (LDARISO-IBNLEN)*(NAO2+NAO4)
            SLOWFF=.TRUE.
         ENDIF
      ENDIF
C
      IBN2=IBNLEN*NWDVAR
      IF(IBNLEN.LT.2.AND.ACTION.NE.DOREAD) THEN
         IF(MASWRK) WRITE(IW,9015) LIMFM,LAST+2*(NAO2+NAO4)
         CALL ABRT
      ENDIF
      IF(ACTION.NE.DOREAD.AND.MASWRK)
     *   WRITE(IW,9400) IBNLEN,MAXGAM,IBNLEN*1.0D+02/MAXGAM
      LASTMX=LAST+MAXGAM*(NAO2+NAO4)
      LASTMN=LAST+2*(NAO2+NAO4)
      LASTPA=LAST+IBNLEN*NPROC*(NAO2+NAO4)
      LBIN1Z= LAST
      LBIN2Z= LBIN1Z+ IBNLEN*NAO2
      LAST  = LBIN2Z+ IBNLEN*NAO4
C
C     THE FOLLOWING OVERLAPS THE PREVIOUS AND IS PECULIAR TO SOCC
C
      LDA   = LIA   + (MAXIA+NWDVAR-1)/NWDVAR
      LOPDM2= LDA   + L2
      LDWRK1= LOPDM2+ L3
      LPDM2 = LDWRK1+ NUM*NAO
      LPDM22= LPDM2 + NF2*NAO2
      LPDM3 = LPDM22+ NF2*NAO2
      LPDM32= LPDM3 + NF3*NAO
      LPDM33= LPDM32+ NF3*NAO
      LPDM34= LPDM33+ NF3*NAO
      LPDM4F= LPDM34+ NF3*NAO
      LBEES = LPDM4F+ NF4
      LPEES = LBEES + NAO4*MXRT*MXRT
      LTFORM= LPEES + NAO2*MXRT*MXRT
      LTDENM= LTFORM+ MAXFUN*NA
      LXINT = LTDENM+ NF4
      LYINT = LXINT  +ND52*ND51
      LZINT = LYINT  +ND52*ND51
      LXINTI= LZINT  +ND52*ND51
      LYINTI= LXINTI +ND52*ND51
      LZINTI= LYINTI +ND52*ND51
      LXINTJ= LZINTI +ND52*ND51
      LYINTJ= LXINTJ +ND52*ND51
      LZINTJ= LYINTJ +ND52*ND51
      LQS   = LZINTJ +ND52*ND51
      LQT   = LQS   + MAXQCI*MXRT
      LCWORK= LQT   + MAXQCI*MXRT
      LRWORK= LCWORK+ NHSO*2*2
      LINDX = LRWORK+ NHSO*3
      LAST1 = LINDX  + (NHSO-1)/NWDVAR+1
C
C     nine integral arrays LXINT, etc are used for both
C     one and two electron integrals, the latter requiring more memory
C     LDWRK1 IS USED FOR BOTH OPDM1 AND PKL
C
      IF(ACTION.EQ.DOREAD) LAST=0
      IF(MSONLY.EQ.-3) LAST1=0
      NEED  = MAX(LAST,LAST1) - LOADFM - 1
      CALL GETFM(NEED)
      IF(MASWRK) THEN
         IF(ACTION.NE.DOREAD) THEN
            WRITE(IW,9010) LIMFM-LOADFM,NEED,MAX(LASTMN,LAST1)-LOADFM-1,
     *            MAX(LASTMX,LAST1)-LOADFM-1,MAX(LASTPA,LAST1)-LOADFM-1
         ELSE
            WRITE(IW,9011) LIMFM-LOADFM,NEED
         ENDIF
         CALL SOCINFO(NOSYM,IPRHSO)
      ENDIF
C
      CALL VCLR(X(LHSO),1,NHSO*NHSO*2)
      IF(EXETYP.EQ.CHECK) GO TO 600
C
C     AT THIS POINT REC 15 CONTAINS STATE #1 (SING) AND 19 - #2 (TRIP)
C     IF NUMVEC=1 THEN BOTH CONTAIN VECTORS OF STATE #1
C
      CALL DAREAD(IDAF,IODA,X(LVST(1)),L3,15,0)
      CALL DAREAD(IDAF,IODA,X(LVST(2)),L3,19,0)
      CALL PREINIT(MAXIA,X(LVST(1)),X(LDC),X(LIA),1)
C
C     NEED TO ADD TO THE DIAGONAL NON-RELATIVISTIC HAMILTONIAN, CONVERT
C     HARTREES TO CM-1, RELATIVE TO HSO(1,1) [SINGLET,NON-RELATIVISTIC]
C
C     add zero order energies
      CALL ADDZERO(NHSO,X(LHSO),X(LEIG),ENGYST,MULST,IROOTS,NZSPIN,
     *             EZERO)
      LSTREC=NSODAT
      MSDA=1
      IF(ACTION.NE.USUAL)
     *   CALL RAOPEN2(ISODAF,X(LISO),0,NSODA,MAXGAM,0,NPRINT)
C
C     unfortunately one has to set the third argument to 0 which
C     inflates the F30 file with garbage, because one needs to store
C     x(liso) into one record, which is to be big enough to hold it.
C
      IF(ACTION.EQ.DOREAD) THEN
         CALL GETDAINF(X(LISO),X(LISO),NSODA+NWDVAR+NPAR,MSDA,OUT)
         CALL CHECKPAR(X(LISO),NSODA,MSONLY,NAO,NPROC,ME,ACOS(-ONE))
      ENDIF
C
      IF(ACTION.EQ.DOSAVE) THEN
         CALL PUTDAINF(X(LISO),X(LISO),NSODA+NWDVAR+NPAR,MSDA,OUT)
         CALL PUTDAINF(X(LISO),X(LINDEX),NAO2+NAO4,2,OUT)
         IF(MSONLY.LE.-2)
     *      CALL PUTDAINF(X(LISO),X(LINDEX),NAO2+NAO4,3,OUT)
C
C     during these calls we only reserve space on DA as we write
C     nonsense.  the second two (ie records 2 and 3) are actually
C     atavisms, however it would be handy to have index arrays at the
C     beginning of the file should someone want to write a conversion
C     utility to convert DA files between incompatible architectures.
C
C     the structure of the DA file:
C     rec content if save/read                         content if usual
C       1 isoda(i.e. DA file structure)                FFs orb.indx. 1,1
C       2 ibinp(MS=1 or 0)(# of FFs for each orb.ind.) FFs orb.indx. 1,2
C       3 ibinp for MS=0 if needed, otherwise          FFs orb.indx. 1,3
C         FFs for orbital index 1,1
C     the record numbers are logical. Actual record numbers depend
C     on the size of arrays/physical record length.
C
      ENDIF
C
      CALL SETPNRM
C     write(iw,*) 'pnrm',(pnrm(ii),ii=1,35)
C
      NCALC=0
      NPROP=0
      NHERM=0
      NSPRO=0
      NSPAC=0
      NZERO=0
      NSYMFOR=0
      HSOMAX=ZERO
      CALL VCLR(X(LCOPCON),1,NCOPCON)
      CALL VCLR(X(LCOPCON1),1,NCOPCON)
      DO 170 ICI1=1,NUMCI
        DO 160 ICI2=ICI1,NUMCI
          LVST1=LVST(IVEX(ICI1))
          LVST2=LVST(IVEX(ICI2))
          IF(MSONLY.LE.-2) THEN
            MINMS=0
            MAXMS=1
          ELSE
            MINMS=MSONLY
            MAXMS=MSONLY
          ENDIF
          IF(MULST(ICI1).EQ.1.AND.MULST(ICI2).EQ.1) THEN
            IF(NOSYM.LE.1) THEN
               NSPRO=NSPRO+(IROOTS(ICI1)*(IROOTS(ICI1)+1))/2
               GOTO 160
            ENDIF
C           no singlet-singlet interactions
          ENDIF
          IF(ICI1.EQ.ICI2.AND.IROOTS(ICI1).LE.1) THEN
            IF(NOSYM.LE.1) THEN
              NHERM=NHERM+(MULST(ICI1)*(MULST(ICI1)+1))/2
              GOTO 160
            ENDIF
C           no interaction between same states
          ENDIF
          IF(ABS(MULST(ICI1)-MULST(ICI2)).GT.2) THEN
            IF(NOSYM.LE.1) THEN
           NSPRO=NSPRO+MULST(ICI1)*IROOTS(ICI1)*MULST(ICI2)*IROOTS(ICI2)
              GOTO 160
            ENDIF
C           no interaction if |S1-S2|>1
          ENDIF
          CALL READOCC(X(LNOCCU),NWKSST,X(JSODA),ICI2,OUT)
          J1=MULST(ICI1)-1
          J2=MULST(ICI2)-1
          SAMEMUL=J1.EQ.J2
          IF(J1.EQ.0.AND.J2.EQ.0) THEN
            MINMS=0
            MAXMS=0
          END IF
C
C         j1,j2 are also doubled (j**2*2), same for m1,m2
C
          DO 150 MS=MAXMS,MINMS,-1
            CALL TSECND(TYME)
            IF(TYME.GT.TIMLIM) THEN
               IF(MASWRK) WRITE(IW,9500)
               GOTO 590
            ENDIF
C           if(ici1.eq.ici2) then
            IF(SAMEMUL) THEN
               M1=J1
               M2=J2
               IF(MS.EQ.1) M1=M1-2
C              if(ms.eq.-1) m2=m2-2
            ELSE
               M1=J1
               M2=J2-(1-MS)*2
            ENDIF
            ALLOWED=.TRUE.
            IZEROT=0
            MSDA=MSDA+1
C
Cdg         call gcisom(llres,j1,m1,j2,m2,mulst,ndim,nijdr,ijdrep,
Cdg  *                     istsym,ici1,ici2,iroots,izerot)
C
            MSTMP = MS
            CALL GCISOL(LRES,ISTSYM,MSTMP,ICI1,ICI2,IROOTS,
     *                  IZEROT,1,IRRL,1)
C
Cdg         if(maswrk) write(iw,*) me,'double/point sym',llres,lres,
Cdg  *                 ici1,ici2,ms
Cdg         lres=lres.and.llres
C
            IF(.NOT.LRES) THEN
               IF(NOSYM.EQ.0) NSPAC=NSPAC+IZEROT
               ALLOWED=.FALSE.
C                 space symmetry forbidden
            ENDIF
            IF(MASWRK) THEN
              IF(ALLOWED) WRITE(IW,9020) J1/TWO,M1/TWO,J2/TWO,
     *                                   M2/TWO,'SOME','ALLOWED'
              IF(.NOT.ALLOWED) WRITE(IW,9020) J1/TWO,M1/TWO,J2/TWO,
     *                                        M2/TWO,'ALL','FORBIDDEN'
            ENDIF
            IF(.NOT.ALLOWED.AND.NOSYM.EQ.0) GOTO 150
            IF(MSONLY.EQ.-3) GOTO 141
            IF(ACTION.NE.DOREAD) THEN
C
C     NOTE THAT SOME PARAMETERS ARE PASSED VIA COMMON BLOCKS (MS ETC)
C
              ICI1TMP = ICI1
              ICI2TMP = ICI2
              CALL SOFFAC(X(LISO),X(LBUFII),X(LBUFJJ),X(LGAMMA),
     *                    X(LBIN1Z),X(LBIN2Z),X(LINDEX),X(LBP1Z),
     *                    X(LBP2Z),X(LIFLGZ),X(LJFLGZ),X(LLOCA),
     *                    X(LLOCB),X(LD1Z),X(LD2Z),X(LIBNP1),
     *                    SAMEMUL,SAVDSK,SLOWFF,ACTION,ICI1TMP,ICI2TMP,
     *                    LDARISO,IBTYP1)
            ELSE
               CALL GETDAINF(X(LISO),X(LINDEX),NAO2+NAO4,MSDA,OUT)
            ENDIF
            IF(MSONLY.EQ.-3) GOTO 141
C
C *** FORM THE ONE AND TWO PARTICLE FORM FACTORS
C
            IF(MASWRK) WRITE(IW,9200)
C
C           note that unfortunately we have to read in CI coefficients
C           after SOFFAC is called as the storage overlaps.
C
            CALL REAPCI(NFTS1,ICI1,NWKSST,IROOTS,X(LQS),MAXQCI)
            CALL REAPCI(NFTS1,ICI2,NWKSST,IROOTS,X(LQT),MAXQCI)
            CALL GAMMSO(X(LISO),X(LBEES),X(LPEES),X(LINDEX),X(LGAMMA),
     *                  X(LGAMMA),X(LQS),X(LQT),X(LNOCCU),IROOTS,
     *                  DEIG(NCORE+1),IVEX,ICI1,ICI2,MAXQCI,OUT)
            CALL TIMIT(1)
            DO 140 I=1,IROOTS(ICI1)
              MINJ=I
              IF(ICI1.NE.ICI2) MINJ=1
              DO 130 J=MINJ,IROOTS(ICI2)
                IF(I.EQ.J.AND.ICI1.EQ.ICI2.AND.NOSYM.LE.1) THEN
             IF(MS.EQ.MAXMS) NHERM=NHERM+(MULST(ICI1)*(MULST(ICI1)+1))/2
                  GOTO 130
C                 no interaction between same states
                ENDIF
                ALLOWED=.TRUE.
Cdg             call cisom(llres,istsym(i,ici1),j1,m1,istsym(j,ici2),
Cdg  *                             j2,m2,mulst,ndim,nijdr,ijdrep)
                CALL CISOL(LRES,ISTSYM(I,ICI1),MS,ISTSYM(J,ICI2),IRRL,1)
Cdg             if(maswrk) write(iw,*) me,'doub/lms sym',llres,lres,
Cdg  *                     ici1,ici2,ms,i,j
Cdg             if(.not.lres.or..not.llres) then
                IF(.NOT.LRES) THEN
                  IF(NOSYM.EQ.0) NSPAC=NSPAC+1
                  ALLOWED=.FALSE.
                  HSO2=CZERO
                  HSO1=CZERO
C                 if(nosym.ne.0) goto 130
C                    space symmetry forbidden
                ENDIF
                IF(ALLOWED.OR.NOSYM.NE.0) THEN
                  NSAVE=0
                  CALL BRTHSO(HSO2,HSO1,ICI1,ICI2,I,J,NWKSST,X(LVST1),
     *                        X(LVST2),X(LDA),X(LDC),X(LOPDM2),X(LDWRK1)
     *                       ,X(LPDM2),X(LPDM22),X(LPDM3),X(LPDM32),
     *                        X(LPDM33),X(LPDM34),X(LPDM4F),X(LBEES),
     *                        X(LPEES),X(LDWRK1),X(LTFORM),X(LQS),X(LQT)
     *                       ,X(LTDENM),MAXQCI,MAXFUN,MAXP,MAXP1,ND51,
     *                        ND52,X(LXINT),X(LYINT),X(LZINT),X(LXINTI),
     *                        X(LYINTI),X(LZINTI),X(LXINTJ),X(LYINTJ),
     *                        X(LZINTJ),X(LIA),ZEFF,NSAVE,L2,X,1,X,1,X,
     *                        X,X,GSYLYES,.TRUE.,IONECNT,OUT)
                  NCALC=NCALC+1
                  IF(ABS(HSO2).LT.HSOTOL) NZERO=NZERO+1
                  HSOMAX=MAX(HSOMAX,ABS(HSO2))
                ENDIF
                JETLAG=I-1+((J-1)+((ICI1-1)+(ICI2-1)*NUMCI)*MXRT)*MXRT
                ITMP = I
                JTMP = J
                ICI1TMP = ICI1
                ICI2TMP = ICI2
                CALL PROPAGATE(ICI1TMP,ICI2TMP,ITMP,JTMP,J1,M1,J2,M2,
     *                         HSO2,HSO1,NHSO,
     *                         X(LHSO),MULST,IROOTS,ISTSYM,IRCIOR,NPROP,
     *                         NSYMFOR,ADD2E,ALLOWED,NOSYM,IPRHSO,
     *                         X(LCOPCON+JETLAG),X(LCOPCON1+JETLAG),
     *                         HSOTOL,SAMEMUL)
                IF(IPRHSO.GE.0) CALL TIMIT(1)
  130         CONTINUE
  140       CONTINUE
  141       CONTINUE
            IF(ACTION.EQ.USUAL) CALL RACLOS(ISODAF,'KEEP')
C           it is open in SOFFAC after the record size is learned
            IF(ACTION.EQ.DOSAVE)
     *        CALL PUTDAINF(X(LISO),X(LINDEX),NAO2+NAO4,MSDA,OUT)
C
            IF(ACTION.NE.USUAL) LSTREC=LSTREC+NSODA1
C           nsodam records were written to DA for this MS
  150     CONTINUE
  160   CONTINUE
  170 CONTINUE
  590 CONTINUE
C
      IF(ACTION.EQ.DOSAVE) THEN
         CALL SAVEPAR(X(LISO),NSODA,MSONLY,NAO,NPROC,ME,ACOS(-ONE))
         CALL PUTDAINF(X(LISO),X(LISO),NSODA+NWDVAR+NPAR,1,OUT)
      ENDIF
      IF(ACTION.NE.USUAL) CALL RACLOS(ISODAF,'KEEP')
C
      IF(MASWRK) THEN
         WRITE(IW,9030)
         IF(ADD2E) THEN
            WRITE(IW,9060)
         ELSE
            WRITE(IW,9070)
         END IF
         IF(OUT) CALL SOSTATS(NCALC,NZERO,NPROP,NHERM,NSPRO,NSPAC,
     *                        NSYMFOR,NOSYM,NHSO,HSOTOL,HSOMAX)
         CALL HSORES(NHSO,X(LHSO),X(LEIG),X(LRWORK),X(LCWORK),IROOTS,
     *               MULST,ENGYST,X(LINDX),2,X(LCOPCON),X(LCOPCON1),
     *               PRTPRM,NZSPIN,1,'  CI  ')
      ENDIF
  600 CONTINUE
C
      CALL RETFM(NEED)
      RETURN
C
 8910 FORMAT(/,A7,' LOAD BALANCING CODE CORRECTED RECLEN TO',I7,/)
 8920 FORMAT(/1X,'OVERWRITING THE DEFAULT RECLEN BY',I7,/)
 9001 FORMAT(/1X,'YOU HAVE DEIGNED TO REQUEST TO:')
 9002 FORMAT(1X,'* CALCULATE FORM FACTORS (FFS)')
 9003 FORMAT(1X,'* CALCULATE MATRIX ELEMENTS OF HSO')
 9004 FORMAT(1X,'* READ FFS FROM DISK')
 9005 FORMAT(1X,'* SAVE FFS TO DISK FOR FUTURE CALCULATIONS')
 9006 FORMAT(1X,'* SAVE DISK SPACE IF POSSIBLE')
 9007 FORMAT(1X,'* CALCULATE SPIN-ORBIT COUPLING CONSTANTS (SOCCS)')
 9008 FORMAT(1X,'* DIAGONALISE HSO MATRIX')
 9010 FORMAT(/1X,I12,' WORDS/NODE ARE AVAILABLE',
     *       /1X,I12,' WORDS/NODE ARE USED BY SOCCC',
     *       /1X,I12,' WORDS/NODE ARE ABSOLUTELY REQUIRED',
     *       /1X,I12,' WORDS/NODE CAN BE USED BY SOCCC IF AVAILABLE',
     *       /1X,'[',I11,' WORDS WILL BE USED BY ALL NODES].'/)
 9011 FORMAT(/1X,I12,' WORDS ARE AVAILABLE',
     *       /1X,I12,' WORDS ARE ABSOLUTELY REQUIRED')
 9015 FORMAT(/1X,'NOT ENOUGH MEMORY TO DO SOCC',/,
     *        1X,I12,'WORDS AVAILABLE',I10,'WORDS NEEDED',//)
 9020 FORMAT(/1X,51("-")/1X,'FORM FACTORS FOR S=',
     *           F3.1,' MS=',F4.1,' AND S''=',F3.1,' MS''=',F4.1,/,
     *      3X,A4,' MATRIX ELEMENTS ARE ',A9,' BY SYMMETRY',/1X,51(1H-))
 9030 FORMAT(/1X,'FULL HAMILTONIAN MATRIX BUILT FROM')
 9060 FORMAT(1X,'ONE AND TWO ELECTRON TERMS')
 9070 FORMAT(1X,'ONLY ONE ELECTRON TERMS')
 9100 FORMAT(/1X,'WARNING: BECAUSE OF INSUFFICENT MEMORY EXPECT',
     *           ' EXCESSIVE I/O TRAFFIC.',
     *       /1X,'(',I10,' MORE WORDS WOULD HELP, OR TRY PHYSRC=.T.)')
 9200 FORMAT(/1X,'CSF INDEX TRANSFORMATIONS OF FORM FACTORS...',/)
C9310 format(/1x,'symmetry allowance for states',2I3,
C    *       ' of multiplicities',2F4.1,' is F',/)
 9400 FORMAT(/1X,'THE SIZE OF THE SORTING BUFFERS IS   ',I7,
     *       /1X,'THE MAXIMUM PREDICTED BUFFER SIZE IS ',I7,
     *       /1X,'PERCENTAGE OF FFS SORTED IN-MEMORY   ',F8.0,'%')
 9500 FORMAT(/1X,'RUNNYNG OUTE OF TYME.',/)
      END
C
C*MODULE SOBRT   *DECK SOINT1
      SUBROUTINE SOINT1(TOL,IA,DA,MAXP,XINT1,YINT1,ZINT1,XINT1I,YINT1I,
     *                  ZINT1I,XINT1J,YINT1J,ZINT1J,ZNUX,NSAVE,L2,SO1AO)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500,MXSH=1000,MXGTOT=5000,MAXSH=35,
     *           PI212=1.1283791670955D+00,ONE=1.0D+00)
C
      LOGICAL IIEQJJ,KKEQLL,JJEQII,LLEQKK
C
      DIMENSION IA(*),DA(*),NX(MAXSH),NY(MAXSH),NZ(MAXSH),CONI(MAXSH),
     *          CONJ(MAXSH),XINT1(0:MAXP,0:MAXP),YINT1(0:MAXP,0:MAXP),
     *          ZINT1(0:MAXP,0:MAXP),XINT1I(0:MAXP,0:MAXP),
     *          YINT1I(0:MAXP,0:MAXP),ZINT1I(0:MAXP,0:MAXP),
     *          XINT1J(0:MAXP,0:MAXP),YINT1J(0:MAXP,0:MAXP),
     *          ZINT1J(0:MAXP,0:MAXP),SO1AO(L2,3),ZNUX(MXATM)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),MIN(MXSH),MAX1(MXSH),NSHELL
      COMMON /SETD/   BP01,B00,B10,XCP00,XC00,YCP00,YC00,ZCP00,ZC00,
     1                F00,DXIJ,DYIJ,DZIJ,DXKL,DYKL,DZKL,
     2                ILAM,JLAM,KLAM,LLAM,NDER,
     3                MAXI,MAXJ,MAXK,MAXL,MAXIJ,MAXKL,MAXNM,JKLN
      COMMON /AIAJ/   T12,XOI,YOI,ZOI,T12LAM,SOC1ER,SOC2ER,AI,AJ,
     *                SOC1EI,SOC2EI,ILAMM1
      COMMON /ROOT/   XX,U(9),W(9),NROOTS
      COMMON /SHLDAT/ XI,YI,ZI,XJ,YJ,ZJ,XK,YK,ZK,XL,YL,ZL,
     1                IIATM,MINII,MAXII,IP1,IP2,
     2                JJATM,MINJJ,MAXJJ,JP1,JP2,
     3                KKATM,MINKK,MAXKK,KP1,KP2,
     4                LLATM,MINLL,MAXLL,LP1,LP2,
     5                IB1,IB2,JB1,JB2,KB1,KB2,LB1,LB2,
     *                IIEQJJ,KKEQLL,JJEQII,LLEQKK
      COMMON /SPINFO/ NCORE,MS,NAO,NAOD,NAO2,NAO4,MSKNAO
      COMMON/SHLNRM/PNRM(35)
C
C     DATA NX/0,1,0,0,2,0,0,1,1,0/
C     DATA NY/0,0,1,0,0,2,0,1,0,1/
C     DATA NZ/0,0,0,1,0,0,2,0,1,1/
C     NX,Y,Z are stolen from INT1.SRC
C     they give the Cartesian tensors in terms of powers of
C     X**nx Y**ny Z**nz (first s, x,y,z, xx,yy,zz, etc)
C
      DATA NX / 0, 1, 0, 0, 2, 0, 0, 1, 1, 0,
     *          3, 0, 0, 2, 2, 1, 0, 1, 0, 1,
     *          4, 0, 0, 3, 3, 1, 0, 1, 0, 2,
     *          2, 0, 2, 1, 1/
      DATA NY / 0, 0, 1, 0, 0, 2, 0, 1, 0, 1,
     *          0, 3, 0, 1, 0, 2, 2, 0, 1, 1,
     *          0, 4, 0, 1, 0, 3, 3, 0, 1, 2,
     *          0, 2, 1, 2, 1/
      DATA NZ / 0, 0, 0, 1, 0, 0, 2, 0, 1, 1,
     *          0, 0, 3, 0, 1, 0, 1, 2, 2, 1,
     *          0, 0, 4, 0, 1, 0, 1, 3, 3, 0,
     *          2, 2, 1, 1, 2/
C
C *** SOIN1 - ONE ELECTRON SPIN ORBIT INTEGRALS
C *** COMPUTE ONE-ELECTRON SPIN ORBIT INTEGRALS OVER
C *** BASIS FUNCTIONS FOR THE II,JJ BLOCK
C
C     maxsh is sum over karten from 0 to 4 (from s to g)
      NATOM=NAT
C
C *** MAXSUB=MAXIMUM SUBSHELL INDEX =1,4,10,20 FOR S,P,D,F
C
      PI12=2.0D+00/PI212
      DXIJ=XI-XJ
      DYIJ=YI-YJ
      DZIJ=ZI-ZJ
      RRIJ=DXIJ*DXIJ+DYIJ*DYIJ+DZIJ*DZIJ
C
      NDER=1
      MAXI =ILAM+1
      MAXJ =JLAM+1
      MAXIJ=ILAM+JLAM+1
      ILAMM1=MAX(0,ILAM-1)
      NROOTS=(ILAM+JLAM+1)/2+2
C
C     ----- I PRIMITIVE ----
C
      DO 8200 IPP=IP1,IP2
        AI=EX(IPP)
        AIRRIJ=AI*RRIJ
        AIXI=AI*XI
        AIYI=AI*YI
        AIZI=AI*ZI
C
C     STORE THE PROPER CONTRACTION COEFFICIENTS IN CONI() THESE WILL
C     REMAIN THE SAME DURING THE LOOP OVER SUBSHELLS, I.E., THEY
C     DEPEND ON THE PRIMITIVE INDEX AND NOT THE SUBSHELL INDEX
C
C     CS   S-TYPE CONTRACTION COEFFICIENTS
C     CP   P-TYPE CONTRACTION COEFFICIENTS FOR THE SUBSHELL LOOP OVER
C          PX, PY, AND PZ
C     CD   D-TYPE CONTRACTION COEFFICIENTS FOR THE SUBSHELL LOOP OVER
C          DXX, DYY, DZZ, DXY, DXZ, AND DYZ
C
        CALL SETCONI(CONI,IPP)
C
C     ----- J PRIMITIVE ----
C
        DO 8100 JP=JP1,JP2
          AJ=EX(JP)
          A=AI+AJ
          AARR=AJ*AIRRIJ/A
          IF(AARR.GT.TOL) GO TO 8100
          EXPE=EXP(-AARR)*PI212/A
          XA=(AIXI+AJ*XJ)/A
          YA=(AIYI+AJ*YJ)/A
          ZA=(AIZI+AJ*ZJ)/A
          AXA=A*XA
          AYA=A*YA
          AZA=A*ZA
          CALL SETCONI(CONJ,JP)
C
C     ----- LOOP OVER NUCLEI -----
C
          DO 7900 IC=1,NATOM
            ZNUC=ZNUX(IC)
            CX=C(1,IC)
            CY=C(2,IC)
            CZ=C(3,IC)
            XAMCX=XA-CX
            YAMCY=YA-CY
            ZAMCZ=ZA-CZ
            XX=A*(XAMCX*XAMCX+YAMCY*YAMCY+ZAMCZ*ZAMCZ)
C
C
C     ----- ROOTS AND WEIGHTS FOR QUADRATURE -----
C           NOTATION FOLLOWS THAT OF RYS THESIS ALSO
C           RYS DUPUIS KING J. COMP. CHEM. 1983.
C           FORTRAN CODE  RDK NOTATION
C              XX            X
C              U2           U*U
C               W            W
C               U           T*T/(1-T*T)
C
            IF(NROOTS.LE.3) CALL RT123
            IF(NROOTS.EQ.4) CALL ROOT4
            IF(NROOTS.EQ.5) CALL ROOT5
            IF(NROOTS.GE.6) CALL ROOT6
C
C     ----- LOOP OVER RYS ROOTS -----
C
            DO 7800 IROOT=1,NROOTS
              WW=W(IROOT)*ZNUC*EXPE
              U2=U(IROOT)*A
              TT=A+U2
              T12=1.0D+00/(TT+TT)
              XBAR=(AXA+U2*CX)/TT
              YBAR=(AYA+U2*CY)/TT
              ZBAR=(AZA+U2*CZ)/TT
              XOI=XBAR-XI
              YOI=YBAR-YI
              ZOI=ZBAR-ZI
              T12LAM=T12*ILAM
              XINT1(0,0)=PI12
              YINT1(0,0)=XINT1(0,0)
              ZINT1(0,0)=WW*XINT1(0,0)
              XINT1(1,0)=XOI*XINT1(0,0)
              YINT1(1,0)=YOI*YINT1(0,0)
              ZINT1(1,0)=ZOI*ZINT1(0,0)
              CALL XYZ1E(MAXP,XINT1,YINT1,ZINT1,XINT1I,YINT1I,ZINT1I,
     *                        XINT1J,YINT1J,ZINT1J)
C
C     ---- LOOP OVER SUBSHELL INDICES FOR SHELL II
C
              NSIGMA=IB1-1
              DO 7700 I=MINII,MAXII
                NSIGMA=NSIGMA+1
                NXI=NX(I)
                NYI=NY(I)
                NZI=NZ(I)
                FACI=CONI(I)*PNRM(I)
C
C     ---- LOOP OVER SUBSHELL INDICES FOR SHELL JJ
C
                JJMAX=MAXJJ
                IF(IIEQJJ) JJMAX=I-1
                NRHO=JB1-1
                DO 7600 J=MINJJ,JJMAX
                  NRHO=NRHO+1
                  NXJ=NX(J)
                  NYJ=NY(J)
                  NZJ=NZ(J)
                  IF (NSIGMA.GE.NRHO) THEN
                    INDEX=IA(NSIGMA)+NRHO
                    PHASE=ONE
                  ELSE
                    INDEX=IA(NRHO)+NSIGMA
                    PHASE=-ONE
                  ENDIF
                  IF(NSAVE.EQ.0) ADA=DA(INDEX)
                  IF(NSAVE.NE.0) ADA=ONE
                  FACJ=FACI*CONJ(J)*PNRM(J)
                  ADAFAC=PHASE*ADA*FACJ
C                 IF (MS.EQ.-1) THEN
C                   SOC1ER=SOC1ER+(YINT1I(NYI,NYJ)*ZINT1J(NZI,NZJ)-
C    *                             YINT1J(NYI,NYJ)*ZINT1I(NZI,NZJ))*
C    *                            XINT1(NXI,NXJ)*ADAFAC
C                   SOC1EI=SOC1EI+(ZINT1I(NZI,NZJ)*XINT1J(NXI,NXJ)-
C    *                             ZINT1J(NZI,NZJ)*XINT1I(NXI,NXJ))*
C    *                            YINT1(NYI,NYJ)*ADAFAC
C                 ELSE IF (MS.EQ.0) THEN
C                   SOC1ER=SOC1ER+(XINT1I(NXI,NXJ)*YINT1J(NYI,NYJ)-
C    *                             XINT1J(NXI,NXJ)*YINT1I(NYI,NYJ))*
C    *                            ZINT1(NZI,NZJ)*ADAFAC
C                 ELSE IF (MS.EQ.1) THEN
C                   SOC1ER=SOC1ER+(YINT1I(NYI,NYJ)*ZINT1J(NZI,NZJ)-
C    *                             YINT1J(NYI,NYJ)*ZINT1I(NZI,NZJ))*
C    *                            XINT1(NXI,NXJ)*ADAFAC
C                   SOC1EI=SOC1EI-(ZINT1I(NZI,NZJ)*XINT1J(NXI,NXJ)-
C    *                             ZINT1J(NZI,NZJ)*XINT1I(NXI,NXJ))*
C    *                            YINT1(NYI,NYJ)*ADAFAC
C                 ENDIF
C                 if(ms.ne.0) then
                   SOLX=(YINT1I(NYI,NYJ)*ZINT1J(NZI,NZJ)-YINT1J(NYI,NYJ)
     *                  *ZINT1I(NZI,NZJ))*XINT1(NXI,NXJ)*ADAFAC
                   SOLY=(ZINT1I(NZI,NZJ)*XINT1J(NXI,NXJ)-ZINT1J(NZI,NZJ)
     *                  *XINT1I(NXI,NXJ))*YINT1(NYI,NYJ)*ADAFAC
C                 else
                   SOLZ=(XINT1I(NXI,NXJ)*YINT1J(NYI,NYJ)-XINT1J(NXI,NXJ)
     *                  *YINT1I(NYI,NYJ))*ZINT1(NZI,NZJ)*ADAFAC
C                 endif
                  IF(MS.EQ.-1) THEN
                    SOC1ER=SOC1ER+SOLX
                    SOC1EI=SOC1EI+SOLY
                  ELSE IF(MS.EQ.0) THEN
                    SOC1ER=SOC1ER+SOLZ
                  ELSE IF(MS.EQ.1) THEN
                    SOC1ER=SOC1ER+SOLX
                    SOC1EI=SOC1EI-SOLY
                  ENDIF
                  IF(NSAVE.GT.0) THEN
                    SO1AO(INDEX,1)=SO1AO(INDEX,1)+SOLX
                    SO1AO(INDEX,2)=SO1AO(INDEX,2)+SOLY
                    SO1AO(INDEX,3)=SO1AO(INDEX,3)+SOLZ
                  ENDIF
 7600           CONTINUE
 7700         CONTINUE
 7800       CONTINUE
 7900     CONTINUE
 8100   CONTINUE
 8200 CONTINUE
      RETURN
      END
C
C*MODULE SOBRT   *DECK SOINT2
      SUBROUTINE SOINT2(TOL,TDENM,MAXP,MAXP1,ND51,ND52,XINT,YINT,ZINT,
     *                  XINTI,YINTI,ZINTI,XINTJ,YINTJ,ZINTJ,NSAVE,
     *                  N2AO,SO2AO,MATURE,GSYLYES)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MXSH=1000,MXGTOT=5000,MAXSH=35,ZERO=0.0D+00,
     *          PI252=34.986836655250D+00)
C
      LOGICAL IIEQJJ,KKEQLL,JJEQII,LLEQKK,GSYLYES(3),DOSOL(3)
C
      DIMENSION TDENM(*),NX(MAXSH),NY(MAXSH),NZ(MAXSH),SOL(3),
     *          CONI(MAXSH),CONJ(MAXSH),CONK(MAXSH),CONL(MAXSH),
     *          XINT(ND52,ND51),YINT(ND52,ND51),ZINT(ND52,ND51),
     *          XINTI(ND52,ND51),YINTI(ND52,ND51),ZINTI(ND52,ND51),
     *          XINTJ(ND52,ND51),YINTJ(ND52,ND51),ZINTJ(ND52,ND51),
     *          SO2AO(3*N2AO*N2AO*N2AO*N2AO)
C
      COMMON /AIAJ/   T12,XOI,YOI,ZOI,T12LAM,SOC1ER,SOC2ER,AI,AJ,
     *                SOC1EI,SOC2EI,ILAMM1
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),MIN(MXSH),MAX(MXSH),NSHELL
      COMMON /ROOT/   XX,U(9),W(9),NROOTS
      COMMON /SETD/   BP01,B00,B10,XCP00,XC00,YCP00,YC00,ZCP00,ZC00,
     *                F00,DXIJ,DYIJ,DZIJ,DXKL,DYKL,DZKL,
     *                ILAM,JLAM,KLAM,LLAM,NDER,
     *                MAXI,MAXJ,MAXK,MAXL,MAXIJ,MAXKL,MAXNM,JKLN
      COMMON /SHLDAT/ XI,YI,ZI,XJ,YJ,ZJ,XK,YK,ZK,XL,YL,ZL,
     *                IIATM,MINII,MAXII,IP1,IP2,
     *                JJATM,MINJJ,MAXJJ,JP1,JP2,
     *                KKATM,MINKK,MAXKK,KP1,KP2,
     *                LLATM,MINLL,MAXLL,LP1,LP2,
     *                IB1,IB2,JB1,JB2,KB1,KB2,LB1,LB2,
     *                IIEQJJ,KKEQLL,JJEQII,LLEQKK
      COMMON /SHLNRM/ PNRM(35)
      COMMON /SPINFO/ NCORE,MS,NAO,NAOD,NAO2,NAO4,MSKNAO
C
      DATA NX / 0, 1, 0, 0, 2, 0, 0, 1, 1, 0,
     *          3, 0, 0, 2, 2, 1, 0, 1, 0, 1,
     *          4, 0, 0, 3, 3, 1, 0, 1, 0, 2,
     *          2, 0, 2, 1, 1/
      DATA NY / 0, 0, 1, 0, 0, 2, 0, 1, 0, 1,
     *          0, 3, 0, 1, 0, 2, 2, 0, 1, 1,
     *          0, 4, 0, 1, 0, 3, 3, 0, 1, 2,
     *          0, 2, 1, 2, 1/
      DATA NZ / 0, 0, 0, 1, 0, 0, 2, 0, 1, 1,
     *          0, 0, 3, 0, 1, 0, 1, 2, 2, 1,
     *          0, 0, 4, 0, 1, 0, 1, 3, 3, 0,
     *          2, 2, 1, 1, 2/
C
C     ---- COMPUTE TWO-ELECTRON SPIN ORBIT INTEGRALS OVER BASIS
C     ---- FUNCTIONS FOR THE II,JJ,KK,LL BLOCK
C
C *** MAXSUB=MAXIMUM SUBSHELL INDEX =1,4,10,20 FOR S,P,D,F
C
C     so2er=ZERO
C     so2ei=ZERO
      DOSOL(1)=(MS.NE.0.OR.NSAVE.GT.1).AND.GSYLYES(1)
      DOSOL(2)=(MS.NE.0.OR.NSAVE.GT.1).AND.GSYLYES(2)
      DOSOL(3)=(MS.EQ.0.OR.NSAVE.GT.1).AND.GSYLYES(3)
      N2AO2=N2AO*N2AO*3
      N2AO3=N2AO*N2AO2
      DXIJ=XI-XJ
      DYIJ=YI-YJ
      DZIJ=ZI-ZJ
      DXKL=XK-XL
      DYKL=YK-YL
      DZKL=ZK-ZL
      RRIJ=DXIJ*DXIJ+DYIJ*DYIJ+DZIJ*DZIJ
      RRKL=DXKL*DXKL+DYKL*DYKL+DZKL*DZKL
C
      NDER=1
      MAXI =ILAM+1
      MAXJ =JLAM+1
      MAXK =KLAM+0
      MAXL =LLAM+0
      MAXIJ=ILAM+JLAM+1
      MAXKL=KLAM+LLAM
      MAXNM=ILAM+JLAM+KLAM+LLAM+1
      JKLN =JLAM+KLAM+LLAM+1
      NROOTS=MAXNM/2+1
C
C     ----- I PRIMITIVE ----
C
      DO 8200 IPP=IP1,IP2
      AI=EX(IPP)
      AIRRIJ=AI*RRIJ
      AIXI=AI*XI
      AIYI=AI*YI
      AIZI=AI*ZI
C
C     STORE THE PROPER CONTRACTION COEFFICIENTS IN CONI() THESE WILL
C     REMAIN THE SAME DURING THE LOOP OVER SUBSHELLS, I.E., THEY
C     DEPEND ON THE PRIMITIVE INDEX AND NOT THE SUBSHELL INDEX
C
C     CS   S-TYPE CONTRACTION COEFFICIENTS
C     CP   P-TYPE CONTRACTION COEFFICIENTS FOR THE SUBSHELL LOOP OVER
C          PX, PY, AND PZ
C     CD   D-TYPE CONTRACTION COEFFICIENTS FOR THE SUBSHELL LOOP OVER
C          DXX, DYY, DZZ, DXY, DXZ, AND DYZ
C
      CALL SETCONI(CONI,IPP)
C
C     ----- J PRIMITIVE ----
C
      DO 8100 JP=JP1,JP2
      AJ=EX(JP)
      A=AI+AJ
      AARR=AJ*AIRRIJ/A
      IF(AARR.GT.TOL) GO TO 8100
      XA=(AIXI+AJ*XJ)/A
      YA=(AIYI+AJ*YJ)/A
      ZA=(AIZI+AJ*ZJ)/A
      DXAI=XA-XI
      DYAI=YA-YI
      DZAI=ZA-ZI
      CALL SETCONI(CONJ,JP)
C
C     ----- K PRIMITIVE ----
C
      DO 8000 KP=KP1,KP2
      AK=EX(KP)
      AKRRKL=AK*RRKL
      AKXK=AK*XK
      AKYK=AK*YK
      AKZK=AK*ZK
      CALL SETCONI(CONK,KP)
C
C     ----- L PRIMITIVE ----
C
      DO 7900 LP=LP1,LP2
      AL=EX(LP)
      B=AK+AL
      DUM=(AL*AKRRKL/B) + AARR
      IF(DUM.GT.TOL) GO TO 7900
      XB=(AKXK+AL*XL)/B
      YB=(AKYK+AL*YL)/B
      ZB=(AKZK+AL*ZL)/B
      DXBK=XB-XK
      DXBA=XB-XA
      ADXBA=A*DXBA
      DYBK=YB-YK
      DYBA=YB-YA
      ADYBA=A*DYBA
      DZBK=ZB-ZK
      DZBA=ZB-ZA
      ADZBA=A*DZBA
      AB=A*B
      AANDB=A+B
      EXPE=PI252*EXP(-DUM)/(AB*SQRT(AANDB))
      RHO=AB/AANDB
      XX=RHO*(DXBA*DXBA+DYBA*DYBA+DZBA*DZBA)
      CALL SETCONI(CONL,LP)
C
C     ----- ROOTS AND WEIGHTS FOR QUADRATURE -----
C           NOTATION FOLLOWS THAT OF RYS THESIS ALSO
C           RYS DUPUIS KING J. COMP. CHEM. 1983.
C           FORTRAN CODE  RDK NOTATION
C              XX            X
C              U2           U*U
C               W            W
C               U           T*T/(1-T*T)
C     --------------------------------------------
C
      IF(NROOTS.LE.3) CALL RT123
      IF(NROOTS.EQ.4) CALL ROOT4
      IF(NROOTS.EQ.5) CALL ROOT5
      IF(NROOTS.GE.6) CALL ROOT6
C
C     ----- LOOP OVER RYS ROOTS -----
C
      DO 7800 IROOT=1,NROOTS
      F00=EXPE*W(IROOT)
      U2=U(IROOT)*RHO
      BP01=(A+U2)/((AB+U2*AANDB)+(AB+U2*AANDB))
      B10 =(B+U2)/((AB+U2*AANDB)+(AB+U2*AANDB))
      B00 =   U2 /((AB+U2*AANDB)+(AB+U2*AANDB))
      XC00 =DXAI+(B00+B00)*B*DXBA
      YC00 =DYAI+(B00+B00)*B*DYBA
      ZC00 =DZAI+(B00+B00)*B*DZBA
      XCP00=DXBK-(B00+B00)*ADXBA
      YCP00=DYBK-(B00+B00)*ADYBA
      ZCP00=DZBK-(B00+B00)*ADZBA
      CALL XYZ2E(MAXP,MAXP1,ND51,ND52,XINT,YINT,ZINT,XINTI,YINTI,ZINTI,
     *           XINTJ,YINTJ,ZINTJ)
C
C     ---- LOOP OVER SUBSHELL INDICES FOR SHELL II
C
      IJKL=1
      NRHO=0
C     ib1-1+nrho gives the position in the AO matrix
C     nrho points to a position in the block for a pair of AOs
C     (the block consists of all Cartesian components)
      DO 7700 I=MINII,MAXII
      NRHO=NRHO+1
      FACI=CONI(I)*PNRM(I)
C
C     ---- LOOP OVER SUBSHELL INDICES FOR SHELL JJ
C
      JJMAX=MAXJJ
      IF(IIEQJJ) JJMAX=I-1
      NSIGMA=0
      DO 7600 J=MINJJ,JJMAX
      NSIGMA=NSIGMA+1
      FACJ=FACI*CONJ(J)*PNRM(J)
      NXX=1+NX(I)+MAXP1*NX(J)
      NYY=1+NY(I)+MAXP1*NY(J)
      NZZ=1+NZ(I)+MAXP1*NZ(J)
C
C     ---- LOOP OVER SUBSHELL INDICES FOR SHELL KK
C
      KKMAX=MAXKK
C     NTAU=0
      IND2SO=1+(NRHO-1+(NSIGMA-1)*N2AO)*3
      DO 7500 K=MINKK,KKMAX
C     NTAU=NTAU+1
      FACK=FACJ*CONK(K)*PNRM(K)
C
C     ---- LOOP OVER SUBSHELL INDICES FOR SHELL LL
C
      LLMAX=MAXLL
      IF(KKEQLL) LLMAX=K
C     NPHI=0
      IND2SOA=IND2SO
      DO 7400 L=MINLL,LLMAX
      MX=1+NX(K)+MAXP*NX(L)
      MY=1+NY(K)+MAXP*NY(L)
      MZ=1+NZ(K)+MAXP*NZ(L)
C
      IF(DOSOL(1)) THEN
        SOL(1)=(YINTI(NYY,MY)*ZINTJ(NZZ,MZ)-YINTJ(NYY,MY)*ZINTI(NZZ,MZ))
     *         *XINT(NXX,MX)
      ELSE
        SOL(1)=ZERO
      ENDIF
      IF(DOSOL(2)) THEN
        SOL(2)=(ZINTI(NZZ,MZ)*XINTJ(NXX,MX)-ZINTJ(NZZ,MZ)*XINTI(NXX,MX))
     *         *YINT(NYY,MY)
      ELSE
        SOL(2)=ZERO
      ENDIF
      IF(DOSOL(3)) THEN
        SOL(3)=(XINTI(NXX,MX)*YINTJ(NYY,MY)-XINTJ(NXX,MX)*YINTI(NYY,MY))
     *         *ZINT(NZZ,MZ)
      ELSE
        SOL(3)=ZERO
      ENDIF
C     IF (MS.EQ.-1) THEN
C       SOC2ER=SOC2ER+(YINTI(NYY,MY)*ZINTJ(NZZ,MZ)-YINTJ(NYY,MY)
C    *  *ZINTI(NZZ,MZ))*XINT(NXX,MX)*TDENFC
C       SOC2EI=SOC2EI+(ZINTI(NZZ,MZ)*XINTJ(NXX,MX)-ZINTJ(NZZ,MZ)
C    *  *XINTI(NXX,MX))*YINT(NYY,MY)*TDENFC
C     ELSE IF (MS.EQ.0) THEN
C       SOC2ER=SOC2ER+(XINTI(NXX,MX)*YINTJ(NYY,MY)-XINTJ(NXX,MX)*
C    *  YINTI(NYY,MY))*ZINT(NZZ,MZ)*TDENFC
C     ELSE IF (MS.EQ.1) THEN
C       SOC2ER=SOC2ER+(YINTI(NYY,MY)*ZINTJ(NZZ,MZ)-YINTJ(NYY,MY)
C    *  *ZINTI(NZZ,MZ))*XINT(NXX,MX)*TDENFC
C       SOC2EI=SOC2EI-(ZINTI(NZZ,MZ)*XINTJ(NXX,MX)-ZINTJ(NZZ,MZ)
C    *  *XINTI(NXX,MX))*YINT(NYY,MY)*TDENFC
      IF(NSAVE.GT.1) THEN
        MATURE=1
C       NPHI=NPHI+1
C       call daxpy(3,-FACK*CONL(L)*pnrm(l),sol,1,so2ao(1,nrho,nsigma,
C    *             ntau,nphi),1)
        TDENFC=FACK*CONL(L)*PNRM(L)
        SO2AO(IND2SOA  )=SO2AO(IND2SOA  )-TDENFC*SOL(1)
        SO2AO(IND2SOA+1)=SO2AO(IND2SOA+1)-TDENFC*SOL(2)
        SO2AO(IND2SOA+2)=SO2AO(IND2SOA+2)-TDENFC*SOL(3)
        IND2SOA=IND2SOA+N2AO3
      ELSE
        TDENFC=TDENM(IJKL)*FACK*CONL(L)*PNRM(L)
        IF (MS.EQ.1) THEN
          SOC2ER=SOC2ER+SOL(1)*TDENFC
          SOC2EI=SOC2EI-SOL(2)*TDENFC
        ELSE IF (MS.EQ.0) THEN
          SOC2ER=SOC2ER+SOL(3)*TDENFC
        ELSE IF (MS.EQ.-1) THEN
          SOC2ER=SOC2ER+SOL(1)*TDENFC
          SOC2EI=SOC2EI+SOL(2)*TDENFC
        ENDIF
        IJKL=IJKL+1
      ENDIF
 7400 CONTINUE
      IND2SO=IND2SO+N2AO2
 7500 CONTINUE
 7600 CONTINUE
 7700 CONTINUE
 7800 CONTINUE
 7900 CONTINUE
 8000 CONTINUE
 8100 CONTINUE
 8200 CONTINUE
C     CALL DDI_GSUMF(2302,SO2ER,1)
C     CALL DDI_GSUMF(2303,SO2EI,1)
C     soc2er=soc2er+SO2ER
C     soc2ei=soc2ei+SO2EI
      RETURN
      END
C
C*MODULE SOBRT   *DECK TFORM2
      SUBROUTINE TFORM2(MAXCEE,CEES1,CEES3,BEES,TPDM2,TPDM22,MAXPKL,
     * PKL,TFORM,MAXFUN)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /DM3/    LOKII,LOKJJ,LOKKK,LOKLL,IIN,JJN,KKN,LLN,IJN
      COMMON /SPINFO/ NCORE,MS,NAO,NAOD,NAO2,NAO4,MSKNAO
      DIMENSION PKL(MAXPKL,*),TFORM(MAXFUN,*),BEES(*),TPDM2(*),TPDM22(*)
     *,         CEES1(MAXCEE,*),CEES3(MAXCEE,*)
C     PKL(MAXAC,MAXAC),TFORM(MAXFUN,MAXAC)
C     ------------------------------------------------------------------
C     PARTIAL TRANSFORMATION OF ACTIVE-SPACE-TWO-PARTICLE DENSITY MATRIX
C     TRANSFORM TPDM(I,J;K,L) TO TPDM2(IP,JP;K,L)
C     IP = SUBSHELL INDEX FOR AO'S IN SHELL II.  IP = 1,2,...IIN
C     JP = SUBSHELL INDEX FOR AO'S IN SHELL JJ.  JP = 1,2,...JJN
C     I,J,K,L ARE INDICES OF ACTIVE MO'S.
C     CEES1(Q,R) REFER TO THE COEFFICIENTS OF SINGLET ACTIVE MO'S
C     CEES3(Q,R) REFER TO THE COEFFICIENTS OF TRIPLET ACTIVE MO'S
C     ------------------------------------------------------------------
C
      NACTV3=NAO2*NAO
      JL=0
      DO 1200 L=1,NAO
        INDEX4=L
        DO 1200 J=1,NAO
          INDEX3=INDEX4
          JL=JL+1
          INDJL=JL
          INDJL2=INDJL
          DO 50 I=1,NAO
            INDEX2=INDEX3
              DO 40 K=1,NAO
                PKL(I,K)=BEES(INDEX2)
                INDEX2=INDEX2+NAO
   40         CONTINUE
              INDEX3=INDEX3+NACTV3
   50     CONTINUE
          INDEX4=INDEX4+NAO2
C
          DO 300 K=1,NAO
            DO 200 IPP=1,IIN
              TFORM(IPP,K)=DDOT(NAO,PKL(1,K),1,CEES1(LOKII+IPP,1),
     *        MAXCEE)
  200       CONTINUE
  300     CONTINUE
C
          DO 550 IPP=1,IIN
            DO 500 JP=1,JJN
              TPDM2(INDJL)=DDOT(NAO,TFORM(IPP,1),MAXFUN,
     *         CEES3(LOKJJ+JP,1),MAXCEE)
              INDJL=INDJL+NAO2
  500       CONTINUE
  550     CONTINUE
C
      DO 700 K=1,NAO
        DO 650 IPP=1,IIN
          TFORM(IPP,K)=DDOT(NAO,PKL(K,1),MAXPKL,CEES3(LOKII+IPP,1),
     *     MAXCEE)
  650   CONTINUE
  700 CONTINUE
      DO 1000 IPP=1,IIN
        DO 900 JP=1,JJN
           TPDM22(INDJL2)=DDOT(NAO,TFORM(IPP,1),MAXFUN,
     *      CEES1(LOKJJ+JP,1),MAXCEE)
        INDJL2=INDJL2+NAO2
  900   CONTINUE
 1000 CONTINUE
 1200 CONTINUE
      RETURN
      END
C
C*MODULE SOBRT   *DECK TFORM3
      SUBROUTINE TFORM3(MAXCEE,CEES1,CEES3,TPDM2,TPDM22,TPDM3,TPDM32,
     * TPDM33,TPDM34)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /DM3/    LOKII,LOKJJ,LOKKK,LOKLL,IIN,JJN,KKN,LLN,IJN
      COMMON /SPINFO/ NCORE,MS,NAO,NAOD,NAO2,NAO4,MSKNAO
      DIMENSION CEES1(MAXCEE,*),CEES3(MAXCEE,*),TPDM2(*),TPDM22(*),
     *TPDM3(*),TPDM32(*),TPDM33(*),TPDM34(*)
C
C     PARTIAL TRANSFORMATION OF ACTIVE-SPACE-TWO-PARTICLE DENSITY MATRIX
C     TRANSFORM TPDM2(IP,JP;K,L) TO TPDM3(IP,JP;KP,L)
C     IP,JP,KP ARE AO LABELS.  (IN SHELLS II,JJ AND KK RESPECTIVELY)
C     K,L ARE MO LABELS. (IN ACTIVE SPACE.)
C
      INDEX=1
      INDEX3=0
      DO 500 IPP=1,IIN
        DO 400 JP=1,JJN
          DO 300 KP=1,KKN
            NZZZ=LOKKK+KP
            INDEX2=INDEX3
            DO 200 L=1,NAO
              TPDM3(INDEX)=DDOT(NAO,TPDM2(INDEX2+1),1,CEES1(NZZZ,1),
     *         MAXCEE)
              TPDM32(INDEX)=DDOT(NAO,TPDM2(INDEX3+L),NAO,
     *         CEES3(NZZZ,1),MAXCEE)
              TPDM33(INDEX)=DDOT(NAO,TPDM22(INDEX2+1),1,CEES1(NZZZ,
     *         1),MAXCEE)
              TPDM34(INDEX)=DDOT(NAO,TPDM22(INDEX3+L),NAO,
     *         CEES3(NZZZ,1),MAXCEE)
              INDEX2=INDEX2+NAO
              INDEX=INDEX+1
  200       CONTINUE
  300     CONTINUE
          INDEX3=INDEX3+NAO2
  400   CONTINUE
  500 CONTINUE
C
      RETURN
      END
C
C*MODULE SOBRT   *DECK TFORM4
      SUBROUTINE TFORM4(MAXCEE,CEES1,CEES3,TPDM3,TPDM32,TPDM33,TPDM34,
     *                  TPDM4F)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION CEES1(MAXCEE,*),CEES3(MAXCEE,*),TPDM3(*),TPDM32(*),
     *          TPDM33(*),TPDM34(*),TPDM4F(*)
      COMMON /DM3/    LOKII,LOKJJ,LOKKK,LOKLL,IIN,JJN,KKN,LLN,IJN
      COMMON /SPINFO/ NCORE,MS,NAO,NAOD,NAO2,NAO4,MSKNAO
C
C *** FINAL STEP OF THE 4 INDEX TRANSFORMATION (FROM MO'S TO AO'S)
C
C
C     FILL UP ARRAY TPDM4 = ONE BLOCK OF 2-E DENSITY MATRIX IN AO BASIS
C     ASSUMES II,JJ,KK,LL IN STANDARD ORDER.
C
      INDEX=0
      INDEX2=0
      DO 500 IPP=1,IIN
       DO 400 JP=1,JJN
        DO 300 KP=1,KKN
         DO 200 LP=1,LLN
          INDEX=INDEX+1
            TPDM4= DDOT(NAO,TPDM3(INDEX2+1),1,CEES3(LP+LOKLL,1),MAXCEE)
            TPDM42=DDOT(NAO,TPDM32(INDEX2+1),1,CEES1(LP+LOKLL,1),MAXCEE)
            TPDM43=DDOT(NAO,TPDM33(INDEX2+1),1,CEES3(LP+LOKLL,1),MAXCEE)
            TPDM44=DDOT(NAO,TPDM34(INDEX2+1),1,CEES1(LP+LOKLL,1),MAXCEE)
C
C   NOW FORM TPDM4F(IJKL)=TPDM4(IJKL)+TPDM42(IJLK)-TPDM43(JIKL)
C                         -TPDM44(JILK)
C
C   WHERE I,J,K & L ARE AO LABELS
C
            TPDM4F(INDEX)=TPDM4+TPDM42-TPDM43-TPDM44
  200    CONTINUE
         INDEX2=INDEX2+NAO
  300   CONTINUE
  400  CONTINUE
  500 CONTINUE
      RETURN
      END
C
C*MODULE SOBRT   *DECK TOPDM
      SUBROUTINE TOPDM(MAXCEE,CEES1,CEES3,PEES,DA,OPDM2,OPDM1)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MXATM=500)
      DIMENSION OPDM2(MAXCEE*MAXCEE),OPDM1(MAXCEE,*),CEES1(MAXCEE,*),
     *          CEES3(MAXCEE,*),DA(*),PEES(*)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /SPINFO/ NCORE,MS,NAO,NAOD,NAO2,NAO4,MSKNAO
C
C     TRANSFORMATION OF ACTIVE-SPACE-ONE-PARTICLE DENSITY MATRIX
C     TRANSFORM OPDM(I,J) TO OPDM2(IP,JP)
C     THEN STORE THE ANTISYMMETRIC MATRIX DA(IP,JP)
C     WHERE  DA(IP,JP) = OPDM2(IP,JP) - OPDM2(JP,IP)
C     IP = INDEX FOR AO'S .  IP = 1,2,...NUM
C     JP = INDEX FOR AO'S .  JP = 1,2,...NUM
C     I,J ARE INDICES OF ACTIVE MO'S.
C     CEES1(Q,R) REFER TO THE COEFFICIENTS OF SINGLET MO'S
C     CEES3(Q,R) REFER TO THE COEFFICIENTS OF TRIPLET MO'S
C
C
C      FIRST TRANSFORM OVER THE I ACTIVE INDEX (SINGLET MO)
C
      DO 300 J=1,NAO
        DO 200 IP1=1,NUM
          OPDM1(IP1,J)=DDOT(NAO,PEES(J),NAO,CEES1(IP1,1),MAXCEE)
  200   CONTINUE
  300 CONTINUE
C
C     NOW TRANSFORM TO OPDM2(IP1,JP) WHERE THE DOUBLE SUBSCRIPTED
C     ARRAY WILL BE TREATED AS A SINGLE SUBSCRIPTED ARRAY
C     OPDM2(IP1,JP)=OPDM2(INDEX), WHERE INDEX = NUM(IP1-1) + JP
C
      INDEX=1
      DO 600 IP1=1,NUM
        DO 500 JP=1,NUM
          OPDM2(INDEX)=DDOT(NAO,OPDM1(IP1,1),MAXCEE,CEES3(JP,1),
     *     MAXCEE)
          INDEX=INDEX+1
  500   CONTINUE
  600 CONTINUE
C
C     FORM THE ANTISYMMETRIC MATRIX DA(INDEX)
C
      INDEX1=0
      INDEX=1
      DO 1000 IP1=1,NUM
        INDEX2=IP1
        DO 900 JP=1,IP1
          DA(INDEX)=OPDM2(INDEX1+JP)-OPDM2(INDEX2)
          INDEX=INDEX+1
          INDEX2=INDEX2+NUM
  900   CONTINUE
        INDEX1=INDEX1+NUM
 1000 CONTINUE
      RETURN
      END
C
C
C*MODULE SOBRT   *DECK XYZ1E
      SUBROUTINE XYZ1E(MAXP,XINT1,YINT1,ZINT1,XINT1I,YINT1I,ZINT1I,
     *                 XINT1J,YINT1J,ZINT1J)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /SETD/   BP01,B00,B10,XCP00,XC00,YCP00,YC00,ZCP00,ZC00,
     *                F00,DXIJ,DYIJ,DZIJ,DXKL,DYKL,DZKL,
     *                ILAM,JLAM,KLAM,LLAM,NDER,
     *                MAXI,MAXJ,MAXK,MAXL,MAXIJ,MAXKL,MAXNM,JKLN
      COMMON /AIAJ/   T12,XOI,YOI,ZOI,T12LAM,SOC1ER,SOC2ER,AI,AJ
     *                ,SOC1EI,SOC2EI,ILAMM1
      DIMENSION
     * XINT1(0:MAXP,0:MAXP),YINT1(0:MAXP,0:MAXP),ZINT1(0:MAXP,0:MAXP),
     * XINT1I(0:MAXP,0:MAXP),YINT1I(0:MAXP,0:MAXP),ZINT1I(0:MAXP,0:MAXP)
     *,XINT1J(0:MAXP,0:MAXP),YINT1J(0:MAXP,0:MAXP),ZINT1J(0:MAXP,0:MAXP)
C     ---------------------------------------------------------------
C     XYZ1E - COMPUTE XINT1,YINT1,ZINT1,XINT1I,YINT1I,ZINT1I,
C             XINT1J,YINT1J,ZINT1J FOR THE ONE ELECTRON SPIN
C             ORBIT INTEGRALS
C     ---------------------------------------------------------------
C
C     ALL VALUES IN /SETD/ MUST BE SET IN CALLING PROGRAM
C
C     NI = 0 TO ILAM+1
C     NJ = 0 TO JLAM+1
C
C     MAXI = ILAM+1
C     MAXJ = JLAM+1
C     MAXIJ = ILAM+JLAM+2
C
      AI2=AI+AI
      AJ2=AJ+AJ
C
C     FIRST FORM INT(NI,0) FOR NI = 2,ILAM+1
C     INT(0,0) AND INT(1,0) ARE FORMED IN CALLING PROGRAM
C
      DO 100 NI=2,ILAM+1
        NIM1=NI-1
        T12NIM=T12*NIM1
        NIM2=NI-2
        XINT1(NI,0)=XOI*XINT1(NIM1,0)+T12NIM*XINT1(NIM2,0)
        YINT1(NI,0)=YOI*YINT1(NIM1,0)+T12NIM*YINT1(NIM2,0)
        ZINT1(NI,0)=ZOI*ZINT1(NIM1,0)+T12NIM*ZINT1(NIM2,0)
  100 CONTINUE
C
C     NOW FILL IN THE REST OF THE BLOCK, INT(NI,NJ)
C     NI=1,ILAM+1  NJ=1,JLAM+1
C
      DO 300 NJ=1,JLAM+1
        NJM1=NJ-1
        T12NJ=T12*NJ
        DO 250 NI=0,ILAM
          NIP1=NI+1
          XINT1(NI,NJ)=XINT1(NIP1,NJM1)+DXIJ*XINT1(NI,NJM1)
          YINT1(NI,NJ)=YINT1(NIP1,NJM1)+DYIJ*YINT1(NI,NJM1)
          ZINT1(NI,NJ)=ZINT1(NIP1,NJM1)+DZIJ*ZINT1(NI,NJM1)
  250   CONTINUE
C
C ***  FORM INT1(ILAM+1,NJ) USING THE THREE TERM RECURRENCE RELATION
C
        XINT1(ILAM+1,NJ)=T12LAM*XINT1(ILAMM1,NJ)+T12NJ*XINT1(ILAM,NJM1)
     *                   +XOI*XINT1(ILAM,NJ)
        YINT1(ILAM+1,NJ)=T12LAM*YINT1(ILAMM1,NJ)+T12NJ*YINT1(ILAM,NJM1)
     *                   +YOI*YINT1(ILAM,NJ)
        ZINT1(ILAM+1,NJ)=T12LAM*ZINT1(ILAMM1,NJ)+T12NJ*ZINT1(ILAM,NJM1)
     *                   +ZOI*ZINT1(ILAM,NJ)
300    CONTINUE
C
C *** NOW FORM XINT1I,XINT1J,YINT1I,YINT1J,ZINT1I,ZINT1J FOR USE
C *** IN CALCULATING THE ONE ELECTRON SPIN-ORBIT INTEGRALS
C *** WHERE
C ***
C ***   XINT1I(NI,NJ)=NI*XINT1(NI-1,NJ)-2*AI*XINT1(NI+1,NJ)
C ***   XINT1J(NI,NJ)=NJ*XINT1(NI,NJ-1)-2*AJ*XINT1(NI,NJ+1)
C
      DO 500 NJ=0,JLAM
        NJM1=MAX(NJ-1,0)
        NJP1=NJ+1
        DO 400 NI=0,ILAM
          NIM1=MAX(NI-1,0)
          NIP1=NI+1
          XINT1I(NI,NJ)=NI*XINT1(NIM1,NJ)-AI2*XINT1(NIP1,NJ)
          YINT1I(NI,NJ)=NI*YINT1(NIM1,NJ)-AI2*YINT1(NIP1,NJ)
          ZINT1I(NI,NJ)=NI*ZINT1(NIM1,NJ)-AI2*ZINT1(NIP1,NJ)
          XINT1J(NI,NJ)=NJ*XINT1(NI,NJM1)-AJ2*XINT1(NI,NJP1)
          YINT1J(NI,NJ)=NJ*YINT1(NI,NJM1)-AJ2*YINT1(NI,NJP1)
          ZINT1J(NI,NJ)=NJ*ZINT1(NI,NJM1)-AJ2*ZINT1(NI,NJP1)
  400   CONTINUE
  500 CONTINUE
      RETURN
      END
C*MODULE SOBRT   *DECK XYZ2E
      SUBROUTINE XYZ2E(MAXP,MAXP1,ND51,ND52,XINT,YINT,ZINT,XINTI,YINTI,
     *                 ZINTI,XINTJ,YINTJ,ZINTJ)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(ONE=1.0D+00,ZERO=0.0D+00)
      COMMON /SETD/   BP01,B00,B10,XCP00,XC00,YCP00,YC00,ZCP00,ZC00,F00,
     *                DXIJ,DYIJ,DZIJ,DXKL,DYKL,DZKL,ILAM,JLAM,KLAM,LLAM,
     *                NDER,MAXI,MAXJ,MAXK,MAXL,MAXIJ,MAXKL,MAXNM,JKLN
      COMMON /AIAJ/   T12,XOI,YOI,ZOI,T12LAM,SOC1ER,SOC2ER,AI,AJ
     *                ,SOC1EI,SOC2EI,ILAMM1
      DIMENSION XINT(ND52,ND51),YINT(ND52,ND51),ZINT(ND52,ND51),
     *          XINTI(ND52,ND51),YINTI(ND52,ND51),ZINTI(ND52,ND51),
     *          XINTJ(ND52,ND51),YINTJ(ND52,ND51),ZINTJ(ND52,ND51)
C     -----------------------------------------------------------
C     COMPUTE XINT,YINT,ZINT,XINTI,YINTI,ZINTI,XINTJ,YINTJ,ZINTJ
C     FOR TWO ELECTRON SPIN ORBIT INTEGRALS
C     -----------------------------------------------------------
C
C      2-D INTEGRAL (NI,NJ//NK,NL) IS COMPUTED AND STORED IN
C     LOCATION XINT(1+NI+MAXP1*NJ,1+NK+MAXP*NL)
C     I.E. LOCATION 1+NI+MAXP*NJ+(MAXP*MAXP-1)*NK+MAXP*(MAXP*MAXP-1)*NL
C
C     ALL VALUES IN /SETD/ MUST BE SET IN CALLING PROGRAM
C     NI = 0 TO ILAM+1
C     NJ = 0 TO JLAM+1
C     NK = 0 TO KLAM
C     NL = 0 TO LLAM
C
C     THE INTERMEDIATE G(N,M) (PAGES 24-28 OF RYS' THESIS)
C     IS STORED IN LOCATION XINT(N+1,M+1) AND OVERWRITTEN.
C
      AI2=AI+AI
      AJ2=AJ+AJ
      XINT(1,1)=ONE
      YINT(1,1)=ONE
      ZINT(1,1)=F00
      XINT(2,1)=XC00
      YINT(2,1)=YC00
      ZINT(2,1)=ZC00*F00
      XINT(1,2)=XCP00
      YINT(1,2)=YCP00
      ZINT(1,2)=ZCP00*F00
      XINT(2,2)=XC00*XCP00+B00
      YINT(2,2)=YC00*YCP00+B00
      ZINT(2,2)=(ZC00*ZCP00+B00)*F00
      IF(MAXNM.LE.2)THEN
C
C *** FAST CODE FOR SSSS SPECIAL CASE
C
          XINT(3,1)=B10+XC00*XC00
          YINT(3,1)=B10+YC00*YC00
          ZINT(3,1)=(B10+ZC00*ZC00)*F00
          XINT(1,3)=BP01+XCP00*XCP00
          YINT(1,3)=BP01+YCP00*YCP00
          ZINT(1,3)=(BP01+ZCP00*ZCP00)*F00
C
C *** GENERAL CASE
C
      ELSE
          C10=ZERO
          CP10=B00
          DO 30 N=2,MAXIJ
              C10=C10+B10
              CP10=CP10+B00
              XINT(N+1,1)=C10*XINT(N-1,1)+XC00*XINT(N,1)
              XINT(N+1,2)=CP10*XINT(N,1)+XCP00*XINT(N+1,1)
              YINT(N+1,1)=C10*YINT(N-1,1)+YC00*YINT(N,1)
              YINT(N+1,2)=CP10*YINT(N,1)+YCP00*YINT(N+1,1)
              ZINT(N+1,1)=C10*ZINT(N-1,1)+ZC00*ZINT(N,1)
              ZINT(N+1,2)=CP10*ZINT(N,1)+ZCP00*ZINT(N+1,1)
   30     CONTINUE
          CP01=ZERO
          C01=B00
          DO 60 M=2,MAXKL
              CP01=CP01+BP01
              C01=C01+B00
              XINT(1,M+1)=CP01*XINT(1,M-1)+XCP00*XINT(1,M)
              XINT(2,M+1)=C01*XINT(1,M)+XC00*XINT(1,M+1)
              YINT(1,M+1)=CP01*YINT(1,M-1)+YCP00*YINT(1,M)
              YINT(2,M+1)=C01*YINT(1,M)+YC00*YINT(1,M+1)
              ZINT(1,M+1)=CP01*ZINT(1,M-1)+ZCP00*ZINT(1,M)
              ZINT(2,M+1)=C01*ZINT(1,M)+ZC00*ZINT(1,M+1)
              NMAX=1+MAXIJ
              IF(NMAX.GE.3)THEN
C
C ***  USE RECURRENCE RELATION 1.98 ON PAGE 26 OF RYS' THESIS.
C
                  CP10=B00
                  DO 50 N=3,NMAX
                      CP10=CP10+B00
                      XINT(N,M+1)=CP01*XINT(N,M-1)+CP10*XINT(N-1,M)+
     *                            XCP00*XINT(N,M)
                      YINT(N,M+1)=CP01*YINT(N,M-1)+CP10*YINT(N-1,M)+
     *                            YCP00*YINT(N,M)
                      ZINT(N,M+1)=CP01*ZINT(N,M-1)+CP10*ZINT(N-1,M)+
     *                            ZCP00*ZINT(N,M)
   50              CONTINUE
              ENDIF
   60     CONTINUE
      ENDIF
C
C     TRANSFER FROM CENTER II TO JJ
C     BACKWARD LOOP OVER NI IS REQUIRED
C     FOR THE II,JJ BLOCK
C     AT THIS POINT NOTHING IS ON CENTER LL
C
      JB1=1
      DO 130 NJ=1,MAXJ
          JB3=JB1
          JB2=JB1+1
          JB1=JB1+MAXP1
          DO 110 NI=MAXIJ-NJ,0,-1
          MM=1+MAXKL
          DO 110 M=1,MM
              XINT(JB1+NI,M)=XINT(JB2+NI,M)+DXIJ*XINT(JB3+NI,M)
              YINT(JB1+NI,M)=YINT(JB2+NI,M)+DYIJ*YINT(JB3+NI,M)
              ZINT(JB1+NI,M)=ZINT(JB2+NI,M)+DZIJ*ZINT(JB3+NI,M)
  110     CONTINUE
  130 CONTINUE
C
C     TRANSFER FROM CENTER KK TO LL
C     BACKWARD LOOP OVER NK IS REQUIRED
C
      NJ=0
      JBASE=1
  210 NI=MIN0(MAXI,MAXIJ-NJ)
      IF(NI.LT.0.OR.NJ.GT.MAXJ) GOTO 1200
  220 N=JBASE+NI
      LB1=1
      DO 240 NL=1,LLAM
          LB3=LB1
          LB2=LB1+1
          LB1=LB1+MAXP
          DO 230 NK=MAXKL-NL,0,-1
            XINT(N,LB1+NK)=XINT(N,LB2+NK)+DXKL*XINT(N,LB3+NK)
            YINT(N,LB1+NK)=YINT(N,LB2+NK)+DYKL*YINT(N,LB3+NK)
            ZINT(N,LB1+NK)=ZINT(N,LB2+NK)+DZKL*ZINT(N,LB3+NK)
  230     CONTINUE
  240 CONTINUE
      NI=NI-1
      IF(NI.GE.0)GO TO 220
      NJ=NJ+1
      JBASE=JBASE+MAXP1
      GO TO 210
C
C     FORM XINTI,XINTJ,YINTI,YINTJ,ZINTI,ZINTJ FOR USE IN SPIN
C     ORBIT CALCULATIONS
C     WHERE
C     XINTI(NI,NJ,NK,NL)=NI*XINT(NI-1,NJ,NK,NL)-2AI*XINT(NI+1,NJ,NK,NL)
C     XINTJ(NI,NJ,NK,NL)=NJ*XINT(NI,NJ-1,NK,NL)-2AJ*XINT(NI,NJ+1,NK,NL)
C
 1200 JBASE=1
      DO 800 NJ=0,MAXJ-1
        DO 790 NI=0,MAXI-1
          N=JBASE+NI
          NMINUS=N-1
          NPLUS=N+1
          NXMINS=N-MAXP1
          NXPLUS=N+MAXP1
          KBASE=1
          DO 680 NL=0,MAXL
            DO 560 NK=0,MAXK
              M=KBASE+NK
            IF (NI.EQ.0) THEN
              XINTI(N,M)=-AI2*XINT(NPLUS,M)
              YINTI(N,M)=-AI2*YINT(NPLUS,M)
              ZINTI(N,M)=-AI2*ZINT(NPLUS,M)
            ELSE
              XINTI(N,M)=NI*XINT(NMINUS,M)-AI2*XINT(NPLUS,M)
              YINTI(N,M)=NI*YINT(NMINUS,M)-AI2*YINT(NPLUS,M)
              ZINTI(N,M)=NI*ZINT(NMINUS,M)-AI2*ZINT(NPLUS,M)
            ENDIF
            IF (NJ.EQ.0) THEN
              XINTJ(N,M)=-AJ2*XINT(NXPLUS,M)
              YINTJ(N,M)=-AJ2*YINT(NXPLUS,M)
              ZINTJ(N,M)=-AJ2*ZINT(NXPLUS,M)
            ELSE
              XINTJ(N,M)=NJ*XINT(NXMINS,M)-AJ2*XINT(NXPLUS,M)
              YINTJ(N,M)=NJ*YINT(NXMINS,M)-AJ2*YINT(NXPLUS,M)
              ZINTJ(N,M)=NJ*ZINT(NXMINS,M)-AJ2*ZINT(NXPLUS,M)
            ENDIF
  560       CONTINUE
            KBASE=KBASE+MAXP
  680     CONTINUE
  790   CONTINUE
        JBASE=JBASE + MAXP1
  800 CONTINUE
      RETURN
      END
C
C*MODULE SOBRT   *DECK GETDAINF
      SUBROUTINE GETDAINF(ISODA,IARRAY,ISIZE,LREC,OUT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MAXCP=4096)
      LOGICAL OUT
      DIMENSION ISODA(*),IARRAY(*)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /SOGUG/  CP(MAXCP),NUNIQ,LIOBP,ISODAF,NSODA,LSTREC,JSODAF,
     *                JSODA,NRECJ
C
      DIMENSION ISODA1(1)
C
C     the subroutine in fact relies on certain facts about raread, if
C     the latter undergoes a change, it might be catastrophic
C     the true isoda is not known at this point so fake it
      ISODA1(1)=1
C     isoda1(2)=isoda1(1)+(maxgam)/IRECLN+1
C     isoda1(3)=isoda1(2)+(NAO4+NAO2-1)/IRECLN+1
C     write(iw,*) 'isora temp',(isoda1(i),i=1,3)
      IF(LREC.EQ.1) THEN
         CALL RAREAD(ISODAF,ISODA1,IARRAY,(ISIZE-1)/NWDVAR+1,LREC,0)
C        would it not be cool to write call raread(isodaf,1,iarray,...)
      ELSE
         CALL RAREAD(ISODAF,ISODA,IARRAY,(ISIZE-1)/NWDVAR+1,LREC,0)
      ENDIF
      IF(OUT) THEN
         WRITE(IW,*) 'READ IARRAY',ISIZE,'RECORD',LREC
         WRITE(IW,9000) (IARRAY(I),I=1,ISIZE)
      ENDIF
      RETURN
 9000 FORMAT(8I10)
      END
C
C*MODULE SOBRT   *DECK PUTDAINF
      SUBROUTINE PUTDAINF(ISODA,IARRAY,ISIZE,LREC,OUT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MAXCP=4096)
      LOGICAL OUT
      DIMENSION ISODA(*),IARRAY(*)
      COMMON /SOGUG/  CP(MAXCP),NUNIQ,LIOBP,ISODAF,NSODA,LSTREC,JSODAF,
     *                JSODA,NRECJ
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /SOBUF/  IBNLEN,IBN2,MAXGAM,MAXFL,MAXFL2,ND1FZ,ND2FZ
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /RAIOLN/ JRECLN(10),JRECST(10)
C
      IRECLN=JRECLN(ISODAF)
      MAXSIZ=((MAXGAM-1)/IRECLN+1)*IRECLN
C     this is the "unofficial" amount of words per logical record
C     it really assumes that the DA file is not structured by OS with
C     record size equal to maxgam.
C     write(iw,*) 'DA record size',maxsiz,'words'
      IF(MAXSIZ.LT.(ISIZE-1)/NWDVAR+1) THEN
         IF(OUT) WRITE(IW,*) 'SORRY, THE SAVE OPTION CANNOT WORK.',LREC
C        i.e. cannot fit all isoda/ibinp into record lrec
         CALL ABRT
      ENDIF
      CALL RAWRIT(ISODAF,ISODA,IARRAY,(ISIZE-1)/NWDVAR+1,LREC,0)
C
C     this is a nontrivial rawrit because we may (but should not)
C     change isoda while writing it. (iarray can be isoda)
C     this can be called twice to ensure isoda is written correctly
C
      IF(OUT) THEN
         WRITE(IW,*) 'WRITE IARRAY',ISIZE,'RECORD',LREC
         WRITE(IW,9000) (IARRAY(I),I=1,ISIZE)
      ENDIF
      RETURN
 9000 FORMAT(8I10)
      END
C
C*MODULE SOBRT   *DECK SAVEPAR
      SUBROUTINE SAVEPAR(IA, N, IPAR1, IPAR2, IPAR3, IPAR4, PAR1)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION IA(*)
C
      IA(N+1)=IPAR1
      IA(N+2)=IPAR2
      IA(N+3)=IPAR3
      IA(N+4)=IPAR4
      CALL SAVEIR(IA(N+5),PAR1)
      RETURN
      END
C
C*MODULE SOBRT   *DECK SAVEIR
      SUBROUTINE SAVEIR(A, P)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(*)
C
      A(1)=P
      RETURN
      END
C
C*MODULE SOBRT   *DECK CHECKPAR
      SUBROUTINE CHECKPAR(IA, N, IPAR1, IPAR2, IPAR3, IPAR4, PAR1)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (TOL=1.0D-15)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      DIMENSION IA(*)
C
      CALL READIR(IA(N+5),P)
C     it is allowed to switch between ms=-2 and ms=-3
      IF((IA(N+1).EQ.IPAR1.OR.(IA(N+1).LE.-2.AND.IPAR1.LE.-2)).AND.
     *   IA(N+2).EQ.IPAR2.AND.IA(N+3).EQ.IPAR3.AND.IA(N+4).EQ.IPAR4)THEN
         IF(P.EQ.PAR1) THEN
            IF(MASWRK) WRITE(IW,9020) ME
            RETURN
         ELSE IF(ABS(P-PAR1).LT.TOL) THEN
            IF(MASWRK) WRITE(IW,9010) ME,P,PAR1
            RETURN
         ENDIF
      ENDIF
      IF(MASWRK) WRITE(IW,9000) ME,IA(N+1),IPAR1,IA(N+2),IPAR2,IA(N+3),
     *                          IPAR3,IA(N+4),IPAR4,P,PAR1
      CALL ABRT
      RETURN
 9000 FORMAT(/,1X,
     * 'SAVED PARAMETERS DO NOT MATCH THE CURRENT SETTINGS ON NODE',I3,
     *       /6X,'READ ',21X,'CURRENT',
     *       /1X,'MS   ',I3,25X,I3,
     *       /1X,'NAO  ',I3,25X,I3,
     *       /1X,'NPROC',I3,25X,I3,
     *       /1X,'NODE ',I3,25X,I3,
     *       /1X,'PI   ',F17.15,10X,F17.15,//,
     * 1X,'YOUR DA FILE CANNOT BE USED WITH YOUR CURRENT INPUT OR CPU.'/
     * 1X,'HINT: IF YOU SEE STARS MOST LIKELY NAO OR CPU IS DIFFERENT.')
 9010 FORMAT(/1X,
     *  '*** WARNING: SAVED PARAMETERS SLIGHTLY MISMATCH ON NODE',I3/
     *  1X,'SAVED ON DISK IS',F17.15,'PI ON THIS MACHINE IS',F17.15/
     *  1X,'PERHAPS YOU ARE REUSING THE FF FILE FROM A DIFFERENT CPU?')
 9020 FORMAT(/1X,
     *   'SAVED PARAMETERS PASS THE INTERNAL CHECK EXACTLY ON NODE',I3)
      END
C
C*MODULE SOBRT   *DECK READIR
      SUBROUTINE READIR(A, P)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(*)
C
      P=A(1)
      RETURN
      END
C
C*MODULE SOBRT   *DECK SAVCIC
      SUBROUTINE SAVCIC(NFT17,NWKSST,IROOTS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      DIMENSION TITLE(10),TITLE1(10),NWKSST(*),IROOTS(*)
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /FMCOM / H(1)
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
C
C     this is a trancated version of savcim, used by PB code
C
C     ----- READ HEADER OF THE -DRT- FILE -----
C
      CALL SEQREW(NFT11)
      READ (NFT11) IDUM,IDUM,IDUM,IDUM,NWKS
      NWKSST(ICI) = IDUM
C     ftncheck thang
      NWKSST(ICI) = NWKS
C
      CALL VALFM(LOADFM)
      IH   = 1 + LOADFM
      LAST = IH  + NWKS
      NEED = LAST-LOADFM-1
      CALL GETFM(NEED)
      CALL SEQREW(NFT12)
C     READ (NFT12) IDUM,IDUM,DUM,DUM
      READ (NFT12) MSTATE,NWKSX,TITLE,TITLE1
      IF(MASWRK) WRITE (IW,9040) TITLE,TITLE1,MSTATE,NWKSX
      DO 200 I=1,IROOTS(ICI)
         CALL SQREAD(NFT12,H(IH),NWKS)
         CALL SQWRIT(NFT17,H(IH),NWKS)
  200 CONTINUE
      CALL RETFM(NEED)
C
      RETURN
 9040 FORMAT(/1X,'READING THE CI VECTOR FILE'/
     *       1X,'RUN TITLE=',10A8/1X,'DRT TITLE=',10A8/
     *       1X,I5,' STATES WERE COMPUTED, NWKS=',I10)
      END
C*MODULE SOBRT   *DECK READOCC
      SUBROUTINE READOCC(NOCCUP,NWKSST,JSOD,ICI2,OUT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MAXCP=4096)
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /SOGUG/  CP(MAXCP),NUNIQ,LIOBP,ISODAF,NSODA,LSTREC,JSODAF,
     *                JSODA,NRECJ
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      LOGICAL OUT
      DIMENSION NOCCUP(*),NWKSST(*),JSOD(*)
      IF(OUT) WRITE(IW,1327)
      CALL RAREAD(JSODAF,JSOD,NOCCUP,(NWKSST(ICI2)-1)/NWDVAR+1,
     *                        (ICI2-1)*NRECJ+4,1)
      IF(OUT) WRITE(IW,1328) (NOCCUP(L),L=1,NWKSST(ICI2))
      RETURN
 1327 FORMAT(//40X,'TRIPLET CSF OCCUPATIONS',/)
 1328 FORMAT(2X,6O13.13)
      END
C*MODULE SOBRT   *DECK SETPNRM
      SUBROUTINE SETPNRM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON/SHLNRM/PNRM(35)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      PARAMETER(ONE=1.0D+00,SQRT3=1.73205080756888D+00,MAXSH=35)
      PARAMETER(SQRT5=2.23606797749979D+00,SQRT7=2.64575131106459D+00)
C
C     stolen from GRD2A.SRC
C
      IF(NORMF.NE.1.OR.NORMP.NE.1) THEN
         SQRT53=SQRT5/SQRT3
C
         DO 170 I=1,MAXSH
           IF (I.EQ.1.OR.I.EQ.2.OR.I.EQ.5.OR.I.EQ.11.OR.I.EQ.21) THEN
              FI = ONE
           ELSE IF (I.EQ.8.OR.I.EQ.20.OR.I.EQ.33) THEN
              FI=FI*SQRT3
           ELSE IF (I.EQ.14) THEN
              FI=FI*SQRT5
           ELSE IF (I.EQ.24) THEN
              FI=FI*SQRT7
           ELSE IF (I.EQ.30) THEN
              FI=FI*SQRT53
           END IF
           PNRM(I)=FI
  170    CONTINUE
      ELSE
         CALL DACOPY(MAXSH,ONE,PNRM,1)
      ENDIF
      RETURN
      END
C*MODULE SOBRT   *DECK SETCONI
      SUBROUTINE SETCONI(CONI,IPP)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MXSH=1000,MXGTOT=5000)
      DIMENSION CONI(*),KARTEN(0:4)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),MIN(MXSH),MAX(MXSH),NSHELL
      DATA KARTEN/1,3,6,10,15/
C
      CONI(1)=CS(IPP)
      CALL DACOPY(KARTEN(1),CP(IPP),CONI(2),1)
      CALL DACOPY(KARTEN(2),CD(IPP),CONI(5),1)
      CALL DACOPY(KARTEN(3),CF(IPP),CONI(11),1)
      CALL DACOPY(KARTEN(4),CG(IPP),CONI(21),1)
      RETURN
      END
C*MODULE SOBRT   *DECK PACK2SO
      SUBROUTINE PACK2SO(N2AO,SO2AO,NSO2BUF,SO2BUF,N2INT,IPOINT,NFT2SO,
     *                   GSYLYES)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL JJEQII,LLEQKK,IIEQJJ,KKEQLL,GSYLYES(3)
      DIMENSION SO2AO(3,N2AO,N2AO,N2AO,N2AO),SO2BUF(NSO2BUF,3),
     *          IPOINT(3),NFT2SO(3),N2INT(3)
      COMMON /PCKLAB/ LABSIZ
      COMMON /SHLDAT/ XI,YI,ZI,XJ,YJ,ZJ,XK,YK,ZK,XL,YL,ZL,
     1                IIATM,MINII,MAXII,IP1,IP2,
     2                JJATM,MINJJ,MAXJJ,JP1,JP2,
     3                KKATM,MINKK,MAXKK,KP1,KP2,
     4                LLATM,MINLL,MAXLL,LP1,LP2,
     5                IB1,IB2,JB1,JB2,KB1,KB2,LB1,LB2,
     *                IIEQJJ,KKEQLL,JJEQII,LLEQKK
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
C
      LABFSIZ=(LABSIZ-1)/NWDVAR+1
      NLAST=NSO2BUF-LABFSIZ-1
      LOC=64/4/NWDVAR*LABSIZ
      TOL=1.0D-08
      MAXI=MAXII-MINII+1
      MAXJ=MAXJJ-MINJJ+1
      MAXK=MAXKK-MINKK+1
      MAXL=MAXLL-MINLL+1
      DO 1000 KART=1,3
       IF(.NOT.GSYLYES(KART)) GOTO 1000
       KPOINT=IPOINT(KART)
       DO 900 II=1,MAXI
        DO 900 JJ=1,MAXJ
         DO 900 KK=1,MAXK
          DO 900 LL=1,MAXL
           VAL=SO2AO(KART,II,JJ,KK,LL)
C
C      another hypermnemonic to asinine logistics
C      Along the AO diagonal the blocks of a pair of AOs will hang over
C      the other side (along the diagonal). The code does not use
C      a/hermiticity to sum the upper and lower triangle to give the
C      smallest number of integrals possible.
C
           IF(ABS(VAL).GT.TOL) THEN
             IF(KPOINT.GE.NLAST) THEN
                WRITE(NFT2SO(KART)) KPOINT
                WRITE(NFT2SO(KART)) (SO2BUF(M,KART),M=1,KPOINT)
                N2INT(KART)=N2INT(KART)+KPOINT
                KPOINT=0
             ENDIF
             KPOINT=KPOINT+1
             SO2BUF(KPOINT,KART)=VAL
             CALL STORE2L(II+IB1-1,JJ+JB1-1,KK+KB1-1,LL+LB1-1,
     *                    SO2BUF(KPOINT+1,KART),LOC)
C            write(6,*) II+IB1-1,JJ+JB1-1,KK+KB1-1,LL+LB1-1,VAL,kart
Cwww
             KPOINT=KPOINT+LABFSIZ
           ENDIF
  900  CONTINUE
       IPOINT(KART)=KPOINT
 1000 CONTINUE
      RETURN
      END
C*MODULE SOBRT   *DECK STORE2L
      SUBROUTINE STORE2L(I,J,K,L,IBUF,LOC)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /PCKLAB/ LABSIZ
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      DIMENSION IBUF(LABSIZ)
C
      IF(LABSIZ.EQ.1) THEN
        IBUF(NWDVAR)=IOR(I,IOR(ISHFT(J,LOC),IOR(ISHFT(K,LOC+LOC),
     *          ISHFT(L,LOC*3))))
      ELSE IF(LABSIZ.EQ.2) THEN
        IBUF(1)=IOR(I,ISHFT(J,LOC))
        IBUF(2)=IOR(K,ISHFT(L,LOC))
      ELSE
        CALL ABRT
      ENDIF
      RETURN
      END
C*MODULE SOBRT   *DECK MCPHSO
      SUBROUTINE MCPHSO(EXI,CSI,CPI,CDI,CFI,CGI,
     *                  ISTART,IATOM,ITYPE,ING,ILOC,IMIN,IMAX,INSHELL,
     *                  EXJ,CSJ,CPJ,CDJ,CFJ,CGJ,
     *                  JSTART,JATOM,JTYPE,JNG,JLOC,JMIN,JMAX,JNSHELL,
     *                  EXK,CSK,CPK,CDK,CFK,CGK,
     *                  KSTART,KATOM,KTYPE,KNG,KLOC,KMIN,KMAX,KNSHELL,
     *                  EXL,CSL,CPL,CDL,CFL,CGL,
     *                  LSTART,LATOM,LTYPE,LNG,LLOC,LMIN,LMAX,LNSHELL,
     *                  MAXP,MAXP1,ND51,ND52,XINT,YINT,ZINT,XINTI,YINTI,
     *                  ZINTI,XINTJ,YINTJ,ZINTJ,L1,SO2AC,IONECNT,IXCOOL,
     *                  GSYLYES)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER(MXATM=500)
C
      LOGICAL GSYLYES(3),IIEQJJ,KKEQLL,JJEQII,LLEQKK,
     *        GOPARR,DSKWRK,MASWRK
C
      DIMENSION XINT(*),YINT(*),ZINT(*),XINTI(*),YINTI(*),
     *          ZINTI(*),XINTJ(*),YINTJ(*),ZINTJ(*),SO2AC(L1,L1,3),
     *          EXI(*),CSI(*),CPI(*),CDI(*),CFI(*),CGI(*),ISTART(*),
     *          IATOM(*),ITYPE(*),ING(*),ILOC(*),IMIN(*),IMAX(*),
     *          EXJ(*),CSJ(*),CPJ(*),CDJ(*),CFJ(*),CGJ(*),JSTART(*),
     *          JATOM(*),JTYPE(*),JNG(*),JLOC(*),JMIN(*),JMAX(*),
     *          EXK(*),CSK(*),CPK(*),CDK(*),CFK(*),CGK(*),KSTART(*),
     *          KATOM(*),KTYPE(*),KNG(*),KLOC(*),KMIN(*),KMAX(*),
     *          EXL(*),CSL(*),CPL(*),CDL(*),CFL(*),CGL(*),LSTART(*),
     *          LATOM(*),LTYPE(*),LNG(*),LLOC(*),LMIN(*),LMAX(*)
C
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /SHLDAT/ XI,YI,ZI,XJ,YJ,ZJ,XK,YK,ZK,XL,YL,ZL,
     1                IIATM,MINII,MAXII,IP1,IP2,
     2                JJATM,MINJJ,MAXJJ,JP1,JP2,
     3                KKATM,MINKK,MAXKK,KP1,KP2,
     4                LLATM,MINLL,MAXLL,LP1,LP2,
     5                IB1,IB2,JB1,JB2,KB1,KB2,LB1,LB2,
     *                IIEQJJ,KKEQLL,JJEQII,LLEQKK
      COMMON /DM3/    LOKII,LOKJJ,LOKKK,LOKLL,IIN,JJN,KKN,LLN,IJN
      COMMON /SETD/   BP01,B00,B10,XCP00,XC00,YCP00,YC00,ZCP00,ZC00,
     1                F00,DXIJ,DYIJ,DZIJ,DXKL,DYKL,DZKL,
     2                ILAM,JLAM,KLAM,LLAM,NDER,
     3                MAXI,MAXJ,MAXK,MAXL,MAXIJ,MAXKL,MAXNM,JKLN
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      DATA E/2.30258D+00/
C
C     this routine is a clone of BRTHSO, q.v. for additional comments.
C     -------------------------------------------------------------
C     COMPUTE MCP two-electron core-valence integrals over basis AO
C     and MCP core orbitals, explicitly stored as a part of an MCP.
C     -------------------------------------------------------------
C     ixcool=1 Coulomb  terms sum(rho) [mu nu || rho rho]
C     ixcool=0 Exchange terms sum(rho) [mu rho || nu rho]
C     mu,nu run over basis AOs, rho runs over MCP core AOs
C     The routine must be called 2 times, with appropriate arrays of
C     bases (arrays EXI,CSI etc) provided to have e.g.
C     mu,nu,rho,rho order)
C     ionecnt
C           = 0 compute all integrals
C           = 1 compute one-centre 1e and one-centre 2e SOc integrals
C           = 2 compute all 1e and one-centre 2e integrals
C
      CALL VCLR(SO2AC,1,L1*L1*3)
      IF (ITOL .EQ. 0) ITOL=20
      TOL = E*ITOL
C
C     ----- ISHELL -----
C
      IJK=0
      DO 9000 II=1,INSHELL
        ILAM=ITYPE(II)-1
        IP1=ISTART(II)
        IP2=ISTART(II)-1+ING(II)
        MINII=IMIN(II)
        MAXII=IMAX(II)
        IIN=MAXII+1-MINII
        LOKII=ILOC(II)-1
        IB1=ILOC(II)
        IB2=IB1+IIN-1
        IIATM=IATOM(II)
        XI=C(1,IIATM)
        YI=C(2,IIATM)
        ZI=C(3,IIATM)
        ILSHELL=0
        IF(ILAM.EQ.1.AND.MINII.EQ.1) ILSHELL=1
        ICOUNTS=0
        IF(ILAM.EQ.0) ICOUNTS=1
C
C        ----- JSHELL -----
C
        JEND=JNSHELL
        IF(IXCOOL.EQ.1) JEND=II
C       ii-1 is also possible?
        DO 8000 JJ=1,JEND
          IJK=IJK+1
C         if(ixcool.eq.1) ijk=ijk+1
          JLAM=JTYPE(JJ)-1
          JP1=JSTART(JJ)
          JP2=JSTART(JJ)-1+JNG(JJ)
          MINJJ=JMIN(JJ)
          MAXJJ=JMAX(JJ)
          JJN=MAXJJ+1-MINJJ
          LOKJJ=JLOC(JJ)-1
          JB1=JLOC(JJ)
          JB2=JB1+JJN-1
          JJATM=JATOM(JJ)
          IF(IONECNT.GT.0.AND.IIATM.NE.JJATM) GOTO 8000
          JLSHELL=0
          IF(JLAM.EQ.1.AND.MINJJ.EQ.1) JLSHELL=1
          JCOUNTS=0
          IF(JLAM.EQ.0) JCOUNTS=1
          XJ=C(1,JJATM)
          YJ=C(2,JJATM)
          ZJ=C(3,JJATM)
          IIEQJJ=II.EQ.JJ.AND.IXCOOL.EQ.1
          JJEQII=IIEQJJ
          IJN = IIN*JJN
C
C         ----- KSHELL -----
C
          DO 7000 KK=1,KNSHELL
C           increment ijk only once no matter what jj
C           if(jj.eq.1.and.ixcool.eq.0) ijk=ijk+1
            IJK=IJK+1
            IF(MOD(IJK,NPROC).NE.ME) GOTO 7000
            KLAM=KTYPE(KK)-1
            KP1=KSTART(KK)
            KP2=KSTART(KK)-1+KNG(KK)
            MINKK=KMIN(KK)
            MAXKK=KMAX(KK)
            KKN=MAXKK+1-MINKK
            LOKKK=KLOC(KK)-1
            KB1=KLOC(KK)
            KB2=KB1+KKN-1
            KKATM=KATOM(KK)
            IF(IONECNT.GT.0.AND.JJATM.NE.KKATM) GOTO 7000
            KLSHELL=0
            IF(KLAM.EQ.1.AND.MINKK.EQ.1) KLSHELL=1
            KCOUNTS=0
            IF(KLAM.EQ.0) KCOUNTS=1
            XK=C(1,KKATM)
            YK=C(2,KKATM)
            ZK=C(3,KKATM)
C
C           ----- LSHELL -----
C
C           DO 6000 LL=1,lNSHELL
C
C             ----- CALCULATE Q4 FACTOR FOR THIS GROUP OF SHELLS -----
C
            LL=LNSHELL
            IF(IXCOOL.EQ.1) THEN
C              values for the last rho
               LL=KK
            ELSE
               LL=JJ
            ENDIF
            LLAM=LTYPE(LL)-1
            LP1=LSTART(LL)
            LP2=LSTART(LL)-1+LNG(LL)
            MINLL=LMIN(LL)
            MAXLL=LMAX(LL)
            LLN=MAXLL+1-MINLL
            LOKLL=LLOC(LL)-1
            LB1=LLOC(LL)
            LB2=LB1+LLN-1
            LLATM=LATOM(LL)
C           there is no loop over l-shell here since l-shell is equal to
C           either k (Coul.) or j (exch.) shell.
            LCOUNTS=0
            IF(LLAM.EQ.0) LCOUNTS=1
            IF(IXCOOL.EQ.1) THEN
C              sum of values for mu and nu
C              imnlsh=ishell+jshell
               IMNS=ICOUNTS+JCOUNTS
               IMNLAM=ILAM+JLAM
               IMNLAMM=ILAM-JLAM
            ELSE
C              imnlsh=ishell+kshell
               IMNS=ICOUNTS+KCOUNTS
               IMNLAM=ILAM+KLAM
               IMNLAMM=ILAM-KLAM
            ENDIF
C
C           for comments on this symmetry usage see BRTHSO.
C           in addition to symmetry rules for the four index integrals
C           we have similar ones for the two index integrals (that is,
C           four index summed over core). In case of two indices,
C           l-shells can be treated in the sense of symmetry as
C           p-shells, because integrals of s-shells with any other
C           shell are zero by symmetry.
C           Explanation:
C           mod(imnlam,2).eq.1 different parity
C           imns.ge.1          any s-shell present
C           abs(imnlamm).gt.1  |l-l'|>1, angular momentum difference
C
            IF(IIATM.EQ.JJATM.AND.JJATM.EQ.KKATM.AND.
     *         ( (MOD(IMNLAM,2).EQ.1.OR.IMNS.GE.1.OR.ABS(IMNLAMM).GT.1)
     *      .OR. (ILSHELL+JLSHELL+KLSHELL.EQ.0.AND.
     *            (MOD(ILAM+JLAM+KLAM+LLAM,2).EQ.1.OR.
     *            ICOUNTS+JCOUNTS+KCOUNTS+LCOUNTS.GE.3) ))) THEN
               GOTO 6000
            ENDIF
            XL=C(1,LLATM)
            YL=C(2,LLATM)
            ZL=C(3,LLATM)
            KKEQLL=KK.EQ.LL.AND.IXCOOL.EQ.1
            LLEQKK=KKEQLL
            CALL MCPSO2(EXI,CSI,CPI,CDI,CFI,CGI,EXJ,CSJ,CPJ,CDJ,CFJ,CGJ,
     *                  EXK,CSK,CPK,CDK,CFK,CGK,EXL,CSL,CPL,CDL,CFL,CGL,
     *                  TOL,MAXP,MAXP1,ND51,ND52,XINT,YINT,ZINT,
     *                  XINTI,YINTI,ZINTI,XINTJ,YINTJ,ZINTJ,L1,SO2AC,
     *                  IXCOOL,GSYLYES)
 6000     CONTINUE
 7000     CONTINUE
 8000   CONTINUE
 9000 CONTINUE
C *** END OF 'SHELL' LOOPS
      RETURN
      END
C
C*MODULE SOBRT   *DECK MCPSO2
      SUBROUTINE MCPSO2(EXI,CSI,CPI,CDI,CFI,CGI,
     *                  EXJ,CSJ,CPJ,CDJ,CFJ,CGJ,
     *                  EXK,CSK,CPK,CDK,CFK,CGK,
     *                  EXL,CSL,CPL,CDL,CFL,CGL,
     *                  TOL,MAXP,MAXP1,ND51,ND52,XINT,YINT,ZINT,
     *                  XINTI,YINTI,ZINTI,XINTJ,YINTJ,ZINTJ,L1,
     *                  SO2AC,IXCOOL,GSYLYES)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MAXSH=35,PI252=34.986836655250D+00)
C
      LOGICAL IIEQJJ,KKEQLL,JJEQII,LLEQKK,GSYLYES(3)
C
      DIMENSION SO2AC(L1,L1,3),NX(MAXSH),NY(MAXSH),NZ(MAXSH),
     *          CONI(MAXSH),CONJ(MAXSH),CONK(MAXSH),CONL(MAXSH),
     *          XINT(ND52,ND51),YINT(ND52,ND51),ZINT(ND52,ND51),
     *          XINTI(ND52,ND51),YINTI(ND52,ND51),ZINTI(ND52,ND51),
     *          XINTJ(ND52,ND51),YINTJ(ND52,ND51),ZINTJ(ND52,ND51),
     *          EXI(*),CSI(*),CPI(*),CDI(*),CFI(*),CGI(*),
     *          EXJ(*),CSJ(*),CPJ(*),CDJ(*),CFJ(*),CGJ(*),
     *          EXK(*),CSK(*),CPK(*),CDK(*),CFK(*),CGK(*),
     *          EXL(*),CSL(*),CPL(*),CDL(*),CFL(*),CGL(*)
C
      COMMON /AIAJ/   T12,XOI,YOI,ZOI,T12LAM,SOC1ER,SOC2ER,AI,AJ,
     *                SOC1EI,SOC2EI,ILAMM1
      COMMON /ROOT/   XX,U(9),W(9),NROOTS
      COMMON /SETD/   BP01,B00,B10,XCP00,XC00,YCP00,YC00,ZCP00,ZC00,
     *                F00,DXIJ,DYIJ,DZIJ,DXKL,DYKL,DZKL,
     *                ILAM,JLAM,KLAM,LLAM,NDER,
     *                MAXI,MAXJ,MAXK,MAXL,MAXIJ,MAXKL,MAXNM,JKLN
      COMMON /SHLDAT/ XI,YI,ZI,XJ,YJ,ZJ,XK,YK,ZK,XL,YL,ZL,
     *                IIATM,MINII,MAXII,IP1,IP2,
     *                JJATM,MINJJ,MAXJJ,JP1,JP2,
     *                KKATM,MINKK,MAXKK,KP1,KP2,
     *                LLATM,MINLL,MAXLL,LP1,LP2,
     *                IB1,IB2,JB1,JB2,KB1,KB2,LB1,LB2,
     *                IIEQJJ,KKEQLL,JJEQII,LLEQKK
      COMMON /SHLNRM/ PNRM(35)
C
      DATA NX / 0, 1, 0, 0, 2, 0, 0, 1, 1, 0,
     *          3, 0, 0, 2, 2, 1, 0, 1, 0, 1,
     *          4, 0, 0, 3, 3, 1, 0, 1, 0, 2,
     *          2, 0, 2, 1, 1/
      DATA NY / 0, 0, 1, 0, 0, 2, 0, 1, 0, 1,
     *          0, 3, 0, 1, 0, 2, 2, 0, 1, 1,
     *          0, 4, 0, 1, 0, 3, 3, 0, 1, 2,
     *          0, 2, 1, 2, 1/
      DATA NZ / 0, 0, 0, 1, 0, 0, 2, 0, 1, 1,
     *          0, 0, 3, 0, 1, 0, 1, 2, 2, 1,
     *          0, 0, 4, 0, 1, 0, 1, 3, 3, 0,
     *          2, 2, 1, 1, 2/
C
C     compute 2e SOC integrals for a tetrada of shells,
C     for all primitives therein and all Cartesiani components.
C     two indices belong to normal ("valence") AO and two to MCP core.
C     this routine is a clone of SOINT2, q.v. for additional comments.
C
      DXIJ=XI-XJ
      DYIJ=YI-YJ
      DZIJ=ZI-ZJ
      DXKL=XK-XL
      DYKL=YK-YL
      DZKL=ZK-ZL
      RRIJ=DXIJ*DXIJ+DYIJ*DYIJ+DZIJ*DZIJ
      RRKL=DXKL*DXKL+DYKL*DYKL+DZKL*DZKL
C
      NDER=1
      MAXI =ILAM+1
      MAXJ =JLAM+1
      MAXK =KLAM+0
      MAXL =LLAM+0
      MAXIJ=ILAM+JLAM+1
      MAXKL=KLAM+LLAM
      MAXNM=ILAM+JLAM+KLAM+LLAM+1
      JKLN =JLAM+KLAM+LLAM+1
      NROOTS=MAXNM/2+1
      DO 8200 IPP=IP1,IP2
       AI=EXI(IPP)
       AIRRIJ=AI*RRIJ
       AIXI=AI*XI
       AIYI=AI*YI
       AIZI=AI*ZI
       CALL GSETCON(CSI,CPI,CDI,CFI,CGI,CONI,IPP)
       DO 8100 JP=JP1,JP2
        AJ=EXJ(JP)
        A=AI+AJ
        AARR=AJ*AIRRIJ/A
        IF(AARR.GT.TOL) GO TO 8100
        XA=(AIXI+AJ*XJ)/A
        YA=(AIYI+AJ*YJ)/A
        ZA=(AIZI+AJ*ZJ)/A
        DXAI=XA-XI
        DYAI=YA-YI
        DZAI=ZA-ZI
        CALL GSETCON(CSJ,CPJ,CDJ,CFJ,CGJ,CONJ,JP)
        DO 8000 KP=KP1,KP2
         AK=EXK(KP)
         AKRRKL=AK*RRKL
         AKXK=AK*XK
         AKYK=AK*YK
         AKZK=AK*ZK
         CALL GSETCON(CSK,CPK,CDK,CFK,CGK,CONK,KP)
         DO 7900 LP=LP1,LP2
C        if(ixcool.eq.1) then
C          lp=kp
C        else
C          lp=jp
C        endif
         AL=EXL(LP)
         B=AK+AL
         DUM=(AL*AKRRKL/B) + AARR
         IF(DUM.GT.TOL) GO TO 7900
         XB=(AKXK+AL*XL)/B
         YB=(AKYK+AL*YL)/B
         ZB=(AKZK+AL*ZL)/B
         DXBK=XB-XK
         DXBA=XB-XA
         ADXBA=A*DXBA
         DYBK=YB-YK
         DYBA=YB-YA
         ADYBA=A*DYBA
         DZBK=ZB-ZK
         DZBA=ZB-ZA
         ADZBA=A*DZBA
         AB=A*B
         AANDB=A+B
         EXPE=PI252*EXP(-DUM)/(AB*SQRT(AANDB))
         RHO=AB/AANDB
         XX=RHO*(DXBA*DXBA+DYBA*DYBA+DZBA*DZBA)
         CALL GSETCON(CSL,CPL,CDL,CFL,CGL,CONL,LP)
C
         IF(NROOTS.LE.3) CALL RT123
         IF(NROOTS.EQ.4) CALL ROOT4
         IF(NROOTS.EQ.5) CALL ROOT5
         IF(NROOTS.GE.6) CALL ROOT6
         DO 7800 IROOT=1,NROOTS
          F00=EXPE*W(IROOT)
          U2=U(IROOT)*RHO
          BP01=(A+U2)/((AB+U2*AANDB)+(AB+U2*AANDB))
          B10 =(B+U2)/((AB+U2*AANDB)+(AB+U2*AANDB))
          B00 =   U2 /((AB+U2*AANDB)+(AB+U2*AANDB))
          XC00 =DXAI+(B00+B00)*B*DXBA
          YC00 =DYAI+(B00+B00)*B*DYBA
          ZC00 =DZAI+(B00+B00)*B*DZBA
          XCP00=DXBK-(B00+B00)*ADXBA
          YCP00=DYBK-(B00+B00)*ADYBA
          ZCP00=DZBK-(B00+B00)*ADZBA
          CALL XYZ2E(MAXP,MAXP1,ND51,ND52,XINT,YINT,ZINT,XINTI,YINTI,
     *               ZINTI,XINTJ,YINTJ,ZINTJ)
C         IJKL=1
          NRHO=0
C         ib1-1+nrho gives the position in the AO matrix
C         nrho points to a position in the block for a pair of AOs
C         (the block consists of all Cartesian components)
          DO 7700 I=MINII,MAXII
           NRHO=NRHO+1
           FACI=CONI(I)*PNRM(I)
           MU=IB1-1+NRHO
           JJMAX=MAXJJ
           IF(IIEQJJ) JJMAX=I-1
           NSIGMA=0
           DO 7600 J=MINJJ,JJMAX
            NSIGMA=NSIGMA+1
            FACJ=FACI*CONJ(J)*PNRM(J)
            NXX=1+NX(I)+MAXP1*NX(J)
            NYY=1+NY(I)+MAXP1*NY(J)
            NZZ=1+NZ(I)+MAXP1*NZ(J)
            KKMAX=MAXKK
            NTAU=0
C           IND2SO=1+(NRHO-1+(NSIGMA-1)*N2AO)*3
            DO 7500 K=MINKK,KKMAX
             NTAU=NTAU+1
             FACK=FACJ*CONK(K)*PNRM(K)
C            LLMAX=MAXLL
C            IF(KKEQLL) LLMAX=K
C            NPHI=0
C            DO 7400 L=MINLL,LLMAX
             IF(IXCOOL.EQ.1) THEN
               L=K
               NU=JB1-1+NSIGMA
             ELSE
               L=J
               NU=KB1-1+NTAU
             ENDIF
             MX=1+NX(K)+MAXP*NX(L)
             MY=1+NY(K)+MAXP*NY(L)
             MZ=1+NZ(K)+MAXP*NZ(L)
C
             TDENFC=FACK*CONL(L)*PNRM(L)
C            if(mu.le.4.and.nu.le.4) write(6,*) 'www',mu,nu
             IF(GSYLYES(1)) SO2AC(MU,NU,1)=SO2AC(MU,NU,1)-
     *        (YINTI(NYY,MY)*ZINTJ(NZZ,MZ)-YINTJ(NYY,MY)*ZINTI(NZZ,MZ))*
     *        XINT(NXX,MX)*TDENFC
             IF(GSYLYES(2)) SO2AC(MU,NU,2)=SO2AC(MU,NU,2)-
     *        (ZINTI(NZZ,MZ)*XINTJ(NXX,MX)-ZINTJ(NZZ,MZ)*XINTI(NXX,MX))*
     *        YINT(NYY,MY)*TDENFC
             IF(GSYLYES(3)) SO2AC(MU,NU,3)=SO2AC(MU,NU,3)-
     *        (XINTI(NXX,MX)*YINTJ(NYY,MY)-XINTJ(NXX,MX)*YINTI(NYY,MY))*
     *        ZINT(NZZ,MZ)*TDENFC
C7400        CONTINUE
 7500       CONTINUE
 7600      CONTINUE
 7700     CONTINUE
 7800    CONTINUE
 7900    CONTINUE
 8000   CONTINUE
 8100  CONTINUE
 8200 CONTINUE
      RETURN
      END
C
C*MODULE SOBRT   *DECK GSETCON
      SUBROUTINE GSETCON(CS,CP,CD,CF,CG,CONI,IPP)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION CONI(*),KARTEN(0:4),CS(*),CP(*),CD(*),CF(*),CG(*)
      DATA KARTEN/1,3,6,10,15/
C
      CONI(1)=CS(IPP)
      CALL DACOPY(KARTEN(1),CP(IPP),CONI(2),1)
      CALL DACOPY(KARTEN(2),CD(IPP),CONI(5),1)
      CALL DACOPY(KARTEN(3),CF(IPP),CONI(11),1)
      CALL DACOPY(KARTEN(4),CG(IPP),CONI(21),1)
      RETURN
      END
C*MODULE SOBRT   *DECK MCP2EINT
      SUBROUTINE MCP2EINT(L1,CORINT,CORIX,MPKATOM,CGMP,GSYLYES,MAXP,
     *                    MAXP1,ND51,ND52,XINT,YINT,ZINT,XINTI,YINTI,
     *                    ZINTI,XINTJ,YINTJ,ZINTJ,IONECNT,OUT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER(MXATM=500,MXSH=1000,MXGTOT=5000,
     *          TWO=2.0D+00,THREE=3.0D+00)
      PARAMETER (MXMPSH=2*MXATM,MXMPGT=5*MXMPSH)
      CHARACTER*1 KART(3)
      LOGICAL GSYLYES(3),OUT,GOPARR,DSKWRK,MASWRK
      DIMENSION CORINT(L1,L1,3),CORIX(L1,L1,3),MPKATOM(*),CGMP(*),
     *          XINT(*),YINT(*),ZINT(*),XINTI(*),YINTI(*),ZINTI(*),
     *          XINTJ(*),YINTJ(*),ZINTJ(*)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),MIN(MXSH),MAX(MXSH),NSHELL
      COMMON /MMP2  /BPAR(MXMPSH),EXPMP(MXMPGT),CSMP(MXMPGT),
     2               CPMP(MXMPGT),CDMP(MXMPGT),CFMP(MXMPGT),
     3               MPSKIP(MXATM),NOCOSH(MXATM),MPKSTA(MXMPSH),
     4               MPKNG(MXMPSH),MPKTYP(MXMPSH),MPKMIN(MXMPSH),
     5               MPKMAX(MXMPSH),MPKLOC(MXMPSH)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      DATA KART/'X','Y','Z'/
C
C     compute MCP core - valence 2e integrals summer over core
C     to produce effective 1e integrals. corint will accumulate
C     the result. "valence" above means all AOs.
C
C     prepare some shell arrays that MCP does not provide
C
      CALL VCLR(CGMP,1,MXMPGT)
      MPNSHELL=0
      JJ=0
      NPR=0
      DO MPAT=1,NAT
        IF (MPSKIP(MPAT).NE.1) THEN
           MPNSHELL=MPNSHELL+NOCOSH(MPAT)
           DO I=1,NOCOSH(MPAT)
             JJ=JJ+1
             MPKATOM(JJ)=MPAT
             NPR=NPR+MPKNG(JJ)
           ENDDO
        ENDIF
      ENDDO
C                          1   1   2   2
C     Coulomb-like terms (act-act-cor-cor)
C
      CALL MCPHSO(
     *     EX,CS,CP,CD,CF,CG,KSTART,KATOM,KTYPE,KNG,KLOC,MIN,MAX,NSHELL,
     *     EX,CS,CP,CD,CF,CG,KSTART,KATOM,KTYPE,KNG,KLOC,MIN,MAX,NSHELL,
     *     EXPMP,CSMP,CPMP,CDMP,CFMP,CGMP,
     *        MPKSTA,MPKATOM,MPKTYP,MPKNG,MPKLOC,MPKMIN,MPKMAX,MPNSHELL,
     *     EXPMP,CSMP,CPMP,CDMP,CFMP,CGMP,
     *        MPKSTA,MPKATOM,MPKTYP,MPKNG,MPKLOC,MPKMIN,MPKMAX,MPNSHELL,
     *     MAXP,MAXP1,ND51,ND52,XINT,YINT,ZINT,XINTI,YINTI,ZINTI,
     *     XINTJ,YINTJ,ZINTJ,L1,CORINT,IONECNT,1,GSYLYES)
C
C     only lower triangular matrix is computed above. Now copy
C     off-diagonal elements (asymtrze divides by two, so multiply
C     back later).
C
      DO I=1,3
         CALL ASYMTRZE(CORINT(1,1,I),L1,L1,ASYM)
      ENDDO
C     divide by NPROC as TMOINT2 will also sum over all nodes
      CALL DSCAL(L1*L1*3,TWO*TWO/NPROC,CORINT,1)
      IF(MASWRK) WRITE(IW,9000)
      CALL TIMIT(1)
C                           1   1   2   2
C     exchange-like terms (act-cor-act-cor)
C
      CALL MCPHSO(
     *     EX,CS,CP,CD,CF,CG,KSTART,KATOM,KTYPE,KNG,KLOC,MIN,MAX,NSHELL,
     *     EXPMP,CSMP,CPMP,CDMP,CFMP,CGMP,
     *        MPKSTA,MPKATOM,MPKTYP,MPKNG,MPKLOC,MPKMIN,MPKMAX,MPNSHELL,
     *     EX,CS,CP,CD,CF,CG,KSTART,KATOM,KTYPE,KNG,KLOC,MIN,MAX,NSHELL,
     *     EXPMP,CSMP,CPMP,CDMP,CFMP,CGMP,
     *        MPKSTA,MPKATOM,MPKTYP,MPKNG,MPKLOC,MPKMIN,MPKMAX,MPNSHELL,
     *     MAXP,MAXP1,ND51,ND52,XINT,YINT,ZINT,XINTI,YINTI,ZINTI,
     *     XINTJ,YINTJ,ZINTJ,L1,CORIX,IONECNT,0,GSYLYES)
      IF(OUT) THEN
         DO I=1,3
           IF(MASWRK) WRITE(6,*) 'COULOMB-LIKE SOC L',KART(I),' INTS'
           CALL PRSQ(CORINT(1,1,I),L1,L1,L1)
           IF(MASWRK) WRITE(6,*) 'EXCHANGE-LIKE SOC L',KART(I),' INTS'
           CALL PRSQ(CORIX(1,1,I),L1,L1,L1)
         ENDDO
      ENDIF
C
C     the integrals are antisymmetrised below. This comes in fact from
C     the inequivalence of electron 1 and 2 in 2e integrals (that
C     results in two exchange terms and after permutation one gets
C     minus sign).
C     asymtrze divides by two, so multiply back by two.
C     Note that the factor THREE below and TWO in the above Coulumb
C     integrals comes form the spin integration (see for details
C     D. Fedorov, PhD thesis, specifically, p.18, eq. 17ff)
C
      DO I=1,3
         CALL ASYMTRZE(CORIX(1,1,I),L1,L1,ASYM)
      ENDDO
      IF(MASWRK) WRITE(IW,9010)
      CALL TIMIT(1)
C     add Coulomb to exchange.
      CALL DAXPY(L1*L1*3,-THREE*TWO/NPROC,CORIX,1,CORINT,1)
C
C     final integrals will be printed in TMOINT2 later (if requested)
C
      CALL DDI_GSUMF(2308,CORINT,L1*L1*3)
      RETURN
 9000 FORMAT(/1X,'COULOMB-LIKE MCP 2E SOC INTEGRALS COMPUTED.')
 9010 FORMAT(/1X,'EXCHANGE-LIKE MCP 2E SOC INTEGRALS COMPUTED.')
      END
C*MODULE SOBRT   *DECK MPBASCHK
      SUBROUTINE MPBASCHK(LMAX)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MXATM=500,MXMPSH=2*MXATM,MXMPGT=5*MXMPSH)
      COMMON /MMP2  /BPAR(MXMPSH),EXPMP(MXMPGT),CSMP(MXMPGT),
     2               CPMP(MXMPGT),CDMP(MXMPGT),CFMP(MXMPGT),
     3               MPSKIP(MXATM),NOCOSH(MXATM),MPKSTA(MXMPSH),
     4               MPKNG(MXMPSH),MPKTYP(MXMPSH),MPKMIN(MXMPSH),
     5               MPKMAX(MXMPSH),MPKLOC(MXMPSH)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
C
C     RETURN THE HIGHEST ANGULAR MOMENTUM PRESENT IN THE MCP core BASIS.
C     NOTE THAT KTYPE=1,2,3,4,5 MEANS S, P(L), D, F, G FUNCTION.
C
      MPSHELL=0
      DO MPAT=1,NAT
        IF(MPSKIP(MPAT).NE.1) MPSHELL=MPSHELL+NOCOSH(MPAT)
      ENDDO
      KANG = 0
      DO 100 N=1,MPSHELL
          IF(MPKTYP(N).GT.KANG) KANG = MPKTYP(N)
  100 CONTINUE
      LMAX = KANG-1
      RETURN
      END
C*MODULE INT2A   *DECK BASLCHK
      SUBROUTINE BASCHKL
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL BASLCHK
C
      PARAMETER (MXSH=1000, MXGTOT=5000)
C
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
C
C     return true if there are any l-shells in the basis
C
      BASLCHK=.FALSE.
      DO 100 N=1,NSHELL
          IF(KTYPE(N).EQ.2.AND.KMIN(N).EQ.1) THEN
             BASLCHK=.TRUE.
             RETURN
          ENDIF
  100 CONTINUE
      WRITE(6,*) BASLCHK
      RETURN
      END
