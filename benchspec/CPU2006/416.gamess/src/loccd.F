C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C 12 DEC 02 - MWS - IMPLEMENT EXETYP=CHECK DURING MAKEFP JOBS
C 17 APR 02 - PND - A SMALL FORMAT CHANGE
C  6 SEP 01 - MWS - EDCMPM: FREE MEMORY WHEN DONE
C 25 JUN 01 - MWS - ALTER COMMON BLOCK WFNOPT
C 13 JUN 01 - MWS - PAD OUT EDCMP COMMON BLOCK
C 19 NOV 00 - RMM,HL - PAD OUT EDCMP COMMON BLOCK FOR POLAPP KEYWORD
C 26 OCT 00 - MWS - INTRODUCE MXAO PARAMETER
C 16 FEB 00 - VK  - MOID: FIX OUTPUT FOR UNUSUALLY DELOCALIZED LMOS
C  1 DEC 98 - BMB - ROLLED VIAINT INTO NEW ECPINT DRIVER
C 28 SEP 97 - MWS - CONVERT PARALLEL CALLS FROM TCGMSG TO DDI
C 13 JUN 96 - VAG - ADD VARIABLE FOR CI TYPE TO SCFOPT COMMON
C 29 MAY 96 - JHJ - MOID: REORDER LMOS ONLY WHEN EDCOMP=.T.
C  6 MAR 96 - SPW - MOIDM: FIX CORE ORB. COUNT FOR E AND POL DECOMP.
C 27 OCT 95 - JHJ - MOIDM: FIX CORE ORBITAL COUNT
C 14 SEP 95 - SPW - CORES NOT REORDERD
C 26 JUL 95 - JHJ - LOCENG: ADDED CALL TO VIAM, ADDED VIAM AND VIAINT.
C 21 APR 95 - JHJ - LOCENG: ZDO OPTION ADDED
C  1 FEB 95 - JHJ - ADDED QUADMM ROUTINE.
C 24 JAN 95 - MWS - REMOVE FTNCHEK WARNINGS
C 14 OCT 94 - MWS - GLOBAL SUM IN VIJINT
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C 18 JUL 94 - JHJ - LMOENG: CALL TO TXYZ; E-N PE CALCULATION RE-DONE.
C 21 JUN 94 - JHJ - NEW LOCCD MODULE: WORKS FOR RHF/ROHF ONLY
C
C*MODULE LOCCD   *DECK DIPLMM
      SUBROUTINE DIPLMM
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
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
      L1 = NUM
      L2 = (L1*L1+L1)/2
      LA = L1*NA
C
C     PARTITION FAST MEMORY
C
      CALL VALFM(LOADFM)
      LDPX  = 1     + LOADFM
      LDPY  = LDPX  + L2
      LDPZ  = LDPY  + L2
      LV    = LDPZ  + L2
      LDIP  = LV    + LA
      LAST  = LDIP  + 3*NA
C
      NEED = LAST - LOADFM - 1
      CALL GETFM(NEED)
      IF(EXETYP.EQ.CHECK) GO TO 800
C
      CALL DIPLMO(X(LDPX),X(LDPY),X(LDPZ),X(LV),X(LDIP),L1,L2,NA)
C
  800 CONTINUE
      CALL RETFM(NEED)
      RETURN
      END
C*MODULE LOCCD   *DECK DIPLMO
      SUBROUTINE DIPLMO(DIPX,DIPY,DIPZ,V,DIP,L1,L2,LNA)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,MOIDON,EDCOMP,DIPDCM,DEPRNT,
     *        QADDCM,ZDO,POLDCM,POLANG,POLAPP,KMIDPT
C
      PARAMETER (MXATM=500, ONE=1.0D+00, TWO=2.0D+00, NMO=500)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /EDCMP / MOIDNO(5,NMO),ZIJ(NMO),ZMO(5,NMO),IJMO(2,NMO),
     *                MOIJ(NMO),NMOIJ(NMO),NMOAT(NMO),OCCUP(NMO),MOIDON,
     *                EDCOMP,DIPDCM,IPROT(5),NPROT,DEPRNT,QADDCM,ZDO,
     *                POLDCM,POLANG,POLAPP,KMIDPT
      COMMON /XYZPRP/ XP,YP,ZP
     *               ,DMX,DMY,DMZ
     *               ,QXX,QYY,QZZ,QXY,QXZ,QYZ
     *               ,QMXX,QMYY,QMZZ,QMXY,QMXZ,QMYZ
     *               ,OXXX,OXXY,OXXZ,OXYY,OYYY,OYYZ
     *               ,OXZZ,OYZZ,OZZZ,OXYZ
     *               ,OMXXX,OMXXY,OMXXZ,OMXYY,OMYYY
     *               ,OMYYZ,OMXZZ,OMYZZ,OMZZZ,OMXYZ
C
      DIMENSION DIPX(L2), DIPY(L2), DIPZ(L2), V(L1,LNA), DIP(3,LNA)
C
      PARAMETER (DEBYE=2.541766D+00, ZERO=0.0D+00)
C
C     ----- THIS SUBROUTINE CALCULATES THE DIPOLE OF LOCALIZED
C           CHARGE DISTRIBUTIONS.  SEE: GORDON AND ENGLAND, JACS
C           94, 5168 (1972) -----
C
      CALL DAREAD(IDAF,IODA,DIPX,L2,95,0)
      CALL DAREAD(IDAF,IODA,DIPY,L2,96,0)
      CALL DAREAD(IDAF,IODA,DIPZ,L2,97,0)
      CALL DAREAD(IDAF,IODA,V,L1*LNA,71,0)
C
      IF (MASWRK) THEN
      WRITE(IW,*)
      WRITE(IW,*)' LCD DIPOLES '
      WRITE(IW,*)
      WRITE(IW,*) 'LMO    XP        YP        ZP        MUX       MUY',
     *'       MUZ     TOTAL'
C
      END IF
C
      DO 500 K = 1,LNA
         SUMX = ZERO
         SUMY = ZERO
         SUMZ = ZERO
         IJ = 0
         DO 400 I = 1,L1
            DO 300 J =1,I
               IJ = IJ + 1
               ONETWO = TWO
               IF(I.EQ.J) ONETWO = ONE
               SUMX = SUMX + DIPX(IJ)*V(I,K)*V(J,K)*ONETWO
               SUMY = SUMY + DIPY(IJ)*V(I,K)*V(J,K)*ONETWO
               SUMZ = SUMZ + DIPZ(IJ)*V(I,K)*V(J,K)*ONETWO
  300       CONTINUE
  400    CONTINUE
         SUMX = SUMX + XP
         SUMY = SUMY + YP
         SUMZ = SUMZ + ZP
         DIP(1,K) = SUMX
         DIP(2,K) = SUMY
         DIP(3,K) = SUMZ
         DX = ZERO
         DY = ZERO
         DZ = ZERO
         ICN = NMOAT(K)
         DO 450 JJ = 1,ICN
            ZIA = ZMO(JJ,K)
            XN = C(1,MOIDNO(JJ,K)) - SUMX
            YN = C(2,MOIDNO(JJ,K)) - SUMY
            ZN = C(3,MOIDNO(JJ,K)) - SUMZ
            DX = DX + ZIA*XN*DEBYE
            DY = DY + ZIA*YN*DEBYE
            DZ = DZ + ZIA*ZN*DEBYE
  450    CONTINUE
         TOTAL = SQRT(DX**2+DY**2+DZ**2)
         IF (MASWRK) WRITE(IW,9020)K,SUMX,SUMY,SUMZ,DX,DY,DZ,TOTAL
  500 CONTINUE
C
C  ---- WRITE LMO CENTROIDS TO DAF ---
C
      CALL DAWRIT(IDAF,IODA,DIP,3*LNA,250,0)
C
      IF (DEPRNT .AND. MASWRK) THEN
         WRITE(IW,FMT='(/A15/)') ' LMO CENTROIDS '
         DO 600 I = 1,LNA
            WRITE(IW,FMT='(1X,A4,3(F9.5,1X))') 'DU  ',(DIP(J,I),J=1,3)
  600    CONTINUE
         WRITE(IW,*)
      END IF
C
 9020 FORMAT(1X,I3,7(F9.5,1X))
      RETURN
      END
C*MODULE LOCCD   *DECK EDCMPM
      SUBROUTINE EDCMPM
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500, NMO=500)
C
      LOGICAL MOIDON,EDCOMP,DIPDCM,DEPRNT,QADDCM,ZDO,
     *        POLDCM,POLANG,POLAPP,KMIDPT
C
      COMMON /ECP2  / CLP(400),ZLP(400),NLP(400),KFIRST(MXATM,6),
     *                KLAST(MXATM,6),LMAX(MXATM),LPSKIP(MXATM),
     *                IZCORE(MXATM)
      COMMON /EDCMP / MOIDNO(5,NMO),ZIJ(NMO),ZMO(5,NMO),IJMO(2,NMO),
     *                MOIJ(NMO),NMOIJ(NMO),NMOAT(NMO),OCCUP(NMO),MOIDON,
     *                EDCOMP,DIPDCM,IPROT(5),NPROT,DEPRNT,QADDCM,ZDO,
     *                POLDCM,POLANG,POLAPP,KMIDPT
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /INTOPT/ ISCHWZ,IECP,NECP,IEFLD
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
C    ---- COUNT NUMBER OF CORES ---
C
      NCORE = 0
      IF(IECP.GT.0) THEN
         DO 100 I=1,NAT
            IF(LPSKIP(I).EQ.0) NCORE = NCORE + 1
  100    CONTINUE
      END IF
C
      L1 = NUM
      L2 = (L1*L1+L1)/2
      M1 = NA + NPROT + NCORE
      M2 = (M1*M1+M1)/2
      M3 = M1*M1
C
C     ----- PARTITION FAST MEMORY -----
C     PLEASE NOTE THAT THERE ARE ADDITIONAL CALLS FOR GETFM IN THE
C     FOLLOWING INTEGRATION ROUTINES: VIAM, VIJINT, TXYZ.  THESE
C     WILL NOT BE TESTED BY CHECK JOBS BECAUSE WE PUNT HERE.
C     THESE AMOUNTS ARE SMALL, 3*L2 IN TXYZ AND ECP INTEGRAL 
C     STORAGE IN THE OTHER TWO, SO THE POOR PROGRAMMING STYLE OF
C     NOT DESCENDING TO ALL GETFM CALLS IS PERMITTED TO EXIST.
C
      CALL VALFM(LOADFM)
      LCINT  = LOADFM + 1
      LXINT  = LCINT  + M2
      LANRIJ = LXINT  + M2
      LVEC   = LANRIJ + M2
      LARRAY = LVEC   + L1*M1
      LSQMAT = LARRAY + L2
      LEI    = LSQMAT + M3
      LAST   = LEI    + L1
      NEED = LAST - LOADFM - 1
      CALL GETFM(NEED)
      IF(EXETYP.EQ.CHECK) GO TO 800
C
      CALL LMOENG(X(LCINT),X(LXINT),X(LANRIJ),X(LVEC),X(LARRAY),
     *            X(LSQMAT),X(LEI),M1,M2,L1,L2,NA,NCORE)
C
  800 CONTINUE
      CALL RETFM(NEED)
      RETURN
      END
C*MODULE LOCCD   *DECK LMOENG
      SUBROUTINE LMOENG(CINT,XINT,ANREIJ,VEC,ARRAY,SQMAT,EI,M1,M2,L1,L2
     *                  ,NLMO,NCORE)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, NMO=500, MXAO=2047)
      PARAMETER (ZERO=0.00D+00, ONE=1.00D+00, TWO=2.00D+00,
     *           PT5=ONE/TWO, TM6=1.0D-06)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,OVRLAP,MOIDON,EDCOMP,DIPDCM,
     *        DEPRNT,QADDCM,ZDO,POLDCM,POLANG,POLAPP,KMIDPT
C
      COMMON /EDCMP / MOIDNO(5,NMO),ZIJ(NMO),ZMO(5,NMO),IJMO(2,NMO),
     *                MOIJ(NMO),NMOIJ(NMO),NMOAT(NMO),OCCUP(NMO),MOIDON,
     *                EDCOMP,DIPDCM,IPROT(5),NPROT,DEPRNT,QADDCM,ZDO,
     *                POLDCM,POLANG,POLAPP,KMIDPT
      COMMON /GSSORD/ IORDER(MXAO),JORDER(MXAO),NORDER
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      DIMENSION CINT(M2), XINT(M2), ANREIJ(M2), VEC(L1,M1), ARRAY(L2)
      DIMENSION EI(L1),SQMAT(M1,M1)
C
C     ----- THIS SUBROUTINE CALCULATES THE ENERGY OF LOCALIZED CHARGE
C     DISTRIBUTIONS.  SEE: ENGLAND AND GORDON, JACS, 93, 4649 (1971)
C                          JENSEN AND GORDON, JPC, 99, 8091 (1995)
C
C     ----- READ IN THE LMOS -----
C
      CALL DAREAD(IDAF,IODA,VEC,L1*NLMO,71,0)
C
C  OVERLAP INTEGRALS
C
      IF (ZDO) THEN
         CALL DAREAD(IDAF,IODA,ARRAY,L2,12,0)
         CALL VCLR(ANREIJ,1,M2)
         CALL TFTRI(ANREIJ,ARRAY,VEC,SQMAT,M1,L1,L1)
         IF (MASWRK) THEN
           CALL CPYTSQ(ANREIJ,SQMAT,M1,1)
           WRITE (IW,*)
           WRITE (IW,*) ' OVERLAP MATRIX'
           CALL PRSQ(SQMAT,M1,M1,M1)
         END IF
      END IF
      CALL DAREAD(IDAF,IODA,CINT,M2,59,0)
      CALL DAREAD(IDAF,IODA,XINT,M2,60,0)
C
      IF (DEPRNT .AND. MASWRK) THEN
         WRITE(IW,*)
         WRITE(IW,*)' COULOMB AND EXCHANGE INTEGRALS IN LMOENG'
         WRITE(IW,*)
      END IF
C
      IF (MASWRK .AND. DEPRNT) WRITE(IW,9000) ' COULOMB'
C
C     ----- REORDER THE INTEGRALS -----
C
      IF (NORDER.EQ.1) THEN
         CALL CPYTSQ(CINT,SQMAT,M1,1)
         CALL ICOPY(M1,IORDER,1,JORDER,1)
         CALL REORDR(SQMAT,IORDER,M1,M1)
         CALL TRPOSQ(SQMAT,M1)
         CALL ICOPY(M1,JORDER,1,IORDER,1)
         CALL REORDR(SQMAT,JORDER,M1,M1)
         CALL CPYSQT(SQMAT,CINT,M1,1)
      END IF
C
C     ----- THERE IS NO INTRA-ORBITAL E-E REPULSION FOR SINGLY
C           OCCUPIED ORBITALS -----
C
      IJ = 0
      DO 130 I = 1,M1
         DO 120 J = 1,I
            IJ = IJ + 1
            IF (I.EQ.J .AND.  OCCUP(I).EQ.ONE) CINT(IJ) = ZERO
  120    CONTINUE
  130 CONTINUE
      IF (DEPRNT) THEN
         CALL CPYTSQ(CINT,SQMAT,M1,1)
         CALL PRSQ(SQMAT,M1,M1,M1)
      END IF
C
      IF (MASWRK .AND. DEPRNT) WRITE(IW,9000) 'EXCHANGE'
C
C     ----- REORDER THE INTEGRALS -----
C
      IF (NORDER.EQ.1) THEN
         CALL CPYTSQ(XINT,SQMAT,M1,1)
         CALL ICOPY(M1,IORDER,1,JORDER,1)
         CALL REORDR(SQMAT,IORDER,M1,M1)
         CALL TRPOSQ(SQMAT,M1)
         CALL ICOPY(M1,JORDER,1,IORDER,1)
         CALL REORDR(SQMAT,JORDER,M1,M1)
         CALL CPYSQT(SQMAT,XINT,M1,1)
      END IF
C
C     ----- THERE IS NO INTRA-ORBITAL E-E REPULSION FOR SINGLY
C           OCCUPIED ORBITALS -----
C
      IJ = 0
      DO 160 I = 1,M1
         DO 150 J = 1,I
            IJ = IJ + 1
            IF (I.EQ.J .AND.  OCCUP(I).EQ.ONE) XINT(IJ) = ZERO
  150    CONTINUE
  160 CONTINUE
      IF (DEPRNT) THEN
         CALL CPYTSQ(XINT,SQMAT,M1,1)
         CALL PRSQ(SQMAT,M1,M1,M1)
      END IF
      IJ = 0
      DO 180 I = 1,M1
         DO 170 J = 1,I
            IJ = IJ + 1
            OVRLAP = (ABS(ANREIJ(IJ)) .GT. TM6) .AND. (I.NE.J)
            IF (ZDO .AND. OVRLAP) XINT(IJ)=ZERO
            IF (OCCUP(I).EQ.ONE .AND. OCCUP(J).EQ.ONE) THEN
               CINT(IJ) = PT5*OCCUP(I)*OCCUP(J)*(CINT(IJ)-XINT(IJ))
            ELSE
               CINT(IJ) = PT5*OCCUP(I)*OCCUP(J)*(CINT(IJ)-PT5*XINT(IJ))
            END IF
  170    CONTINUE
  180 CONTINUE
      IF (DEPRNT) THEN
         CALL CPYTSQ(CINT,SQMAT,M1,1)
         WRITE(IW,*)
         WRITE(IW,*) 'ELECTRON-ELECTRON REPULSION ENERGY'
         IF (ZDO) WRITE(IW,*)
     *       'ZDO OPTION SELECTED: INTERFRAGMENT EXCHANGE IS OMITTED'
         CALL PRSQ(SQMAT,M1,M1,M1)
      END IF
C
C     CALCULATE NUCLEAR REPULSION ENERGY
C
      DO 190 IPT = 1,NPROT
         ID = NLMO + IPT
         NMOAT(ID) = 1
         ZMO(1,ID) = 1.0D+00
         MOIDNO(1,ID) = IPROT(IPT)
  190 CONTINUE
C
      ANRE = 0
      IJ = 0
      DO 290 I = 1,M1
         ICN = NMOAT(I)
         DO 280 J = 1,I
            IJ = IJ + 1
            ANREIJ(IJ) = ZERO
            JCN = NMOAT(J)
            DO 240 K = 1,ICN
               ZI = ZMO(K,I)
               DO 230 L = 1,JCN
                  IF (MOIDNO(K,I).EQ.MOIDNO(L,J)) GO TO 230
                  ZJ = ZMO(L,J)
                  X = C(1,MOIDNO(K,I)) - C(1,MOIDNO(L,J))
                  Y = C(2,MOIDNO(K,I)) - C(2,MOIDNO(L,J))
                  Z = C(3,MOIDNO(K,I)) - C(3,MOIDNO(L,J))
                  RR = SQRT(X*X + Y*Y + Z*Z)
                  ANREIJ(IJ) = ANREIJ(IJ) + ZI*ZJ/(TWO*RR)
  230          CONTINUE
  240       CONTINUE
         ANRE = ANRE + TWO*ANREIJ(IJ)
         IF (I.EQ.J) ANRE = ANRE - ANREIJ(IJ)
  280    CONTINUE
  290 CONTINUE
C
      IF (DEPRNT .AND. MASWRK) THEN
         CALL CPYTSQ(ANREIJ,SQMAT,M1,1)
         WRITE(IW,*)
         WRITE(IW,*)' NRE FOR LCD I AND J'
         CALL PRSQ(SQMAT,M1,M1,M1)
      END IF
C
C    KINETIC ENERGY INTEGRALS
C
      CALL DAREAD(IDAF,IODA,ARRAY,L2,13,0)
C
      IF (DEPRNT) CALL TXYZ(VEC,SQMAT,OCCUP,L1,M1,NLMO)
C
      IF (MASWRK) WRITE(IW,*)
      DO 390 K = 1,NLMO
         EI(K) = ZERO
         IJ = 0
         DO 350 I = 1,L1
            DO 300 J = 1,I
               IJ = IJ + 1
               ONETWO = TWO
               IF (I.EQ.J) ONETWO = ONE
               EI(K) = EI(K) + ARRAY(IJ)*VEC(I,K)*VEC(J,K)*ONETWO
  300       CONTINUE
  350    CONTINUE
         EI(K) = OCCUP(K)*EI(K)
  390 CONTINUE
C
C    ELECTRON-NUCLEAR ATTRACTION
C
      CALL VCLR(SQMAT,1,M1*M1)
      DO 490 LAT=1,NAT
         CALL VIJINT(ARRAY,LAT,L1,L2)
         DO 480 I = 1,NLMO
            KL = 0
            DO 450 K = 1,L1
               DO 440 L = 1,K
                  KL = KL + 1
                  ONETWO = TWO
                  IF (K.EQ.L) ONETWO = ONE
                  SQMAT(I,LAT) = SQMAT(I,LAT) + (ARRAY(KL)*
     *                                  VEC(K,I)*VEC(L,I)*ONETWO)
  440          CONTINUE
  450       CONTINUE
            SQMAT(I,LAT) = OCCUP(I)*ZAN(LAT)*SQMAT(I,LAT)
  480    CONTINUE
  490 CONTINUE
C
      IF (MASWRK .AND. DEPRNT) THEN
         WRITE(IW,*) ' LMO-NUCLEAR PE: ROWS=LMOS, COLUMNS=ATOMS '
         CALL PRSQ(SQMAT,NAT,M1,M1)
      END IF
C
      IJ = 0
      DO 690 I = 1,M1
         DO 680 J = 1,I
            IJ = IJ + 1
            XINT(IJ) = ZERO
            IF (J .GT. NLMO) GO TO 680
C
C        CALCULATE ZJ INTEGRALS
C
            IF (I .LE. NLMO) THEN
               JCN = NMOAT(J)
               DO 630 JC = 1,JCN
                  JAT = MOIDNO(JC,J)
                  ZNUC = ZMO(JC,J)/ZAN(JAT)
                  XINT(IJ) = XINT(IJ) + PT5*SQMAT(I,JAT)*ZNUC
  630          CONTINUE
            END IF
C
C        CALCULATE ZI INTEGRALS
C
            ICN = NMOAT(I)
            DO 670 IC = 1,ICN
               IAT = MOIDNO(IC,I)
               ZNUC = ZMO(IC,I)/ZAN(IAT)
               XINT(IJ) = XINT(IJ) + PT5*SQMAT(J,IAT)*ZNUC
  670       CONTINUE
  680    CONTINUE
  690 CONTINUE
C
      IF (DEPRNT .AND. MASWRK) THEN
         CALL CPYTSQ(XINT,SQMAT,M1,1)
         WRITE(IW,*)
         WRITE(IW,*)' ELECTRON-NUCLEAR ATTRATIONS'
         CALL PRSQ(SQMAT,M1,M1,M1)
      END IF
C
C   --- POSSIBLE CORE CONTRIBUTIONS ---
C
      CALL VIAM(VEC,XINT,ARRAY,L1,L2,M1,M2,NCORE,NLMO)
C
C    SUMMING UP
C
      IF (MASWRK) THEN
         WRITE(IW,*)
         WRITE(IW,*)'  ENERGY DECOMPOSITION INTO LCDS '
         WRITE(IW,*)
         WRITE(IW,9050)
      END IF
      TTI = ZERO
      TVI = ZERO
      TGI = ZERO
      TANREI = ZERO
      TE = ZERO
      DO 800 I = 1,M1
         J1 = (I*I-I)/2 + 1
         JJ = (I*I+I)/2
         TI = ZERO
         GI = ZERO
         VI = ZERO
         ANREI = ZERO
         TI = ZERO
         IF (I .LE. NLMO) TI =  EI(I)
         DO 750 IJ = J1,JJ
            GI = GI + CINT(IJ)
            VI = VI + XINT(IJ)
            ANREI = ANREI + ANREIJ(IJ)
  750    CONTINUE
         XINT(JJ) = XINT(JJ) + TI
         II = JJ
         DO 760 IJ = I,M1-1
            II = II + IJ
            GI = GI + CINT(II)
            VI = VI + XINT(II)
            ANREI = ANREI + ANREIJ(II)
  760    CONTINUE
         EI(I) = TI + VI + GI + ANREI
         IF (MASWRK) WRITE(IW,9100) I,TI,VI,GI,ANREI,EI(I)
         TTI = TTI + TI
         TVI = TVI + VI
         TGI = TGI + GI
         TANREI = TANREI + ANREI
         TE = TE + EI(I)
  800 CONTINUE
      IF (MASWRK) THEN
         WRITE(IW,*)
         WRITE(IW,9110) TTI, TVI, TGI, TANREI, TE
      END IF
C
      DO 850 I = 1,M2
         CINT(I) = CINT(I) + XINT(I) + ANREIJ(I)
  850 CONTINUE
      IF (MASWRK) THEN
         WRITE(IW,*)
         WRITE(IW,*)' INTRA- AND INTER-LCD ENERGY'
      END IF
      CALL CPYTSQ(CINT,SQMAT,M1,1)
      CALL PRSQ(SQMAT,M1,M1,M1)
C
 9000 FORMAT(/1X,A8,' INTEGRAL MATRIX')
 9050 FORMAT(1X,'LMO',8X,'KE',10X,'N-E PE',8X,'E-E PE',9X,'N-N PE',
     *       7X,'TOTAL E')
 9100 FORMAT(1X,I3,5(2X,F12.6))
 9110 FORMAT(4X,5(2X,F12.6))
      RETURN
      END
C*MODULE LOCCD   *DECK MOIDM
      SUBROUTINE MOIDM
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500, MXAO=2047)
C
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /OPTLOC/ CVGLOC,MAXLOC,IPRTLO,ISYMLO,IFCORE,NOUTA,NOUTB,
     *                MOOUTA(MXAO),MOOUTB(MXAO)
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
      CALL LMOPSI(0,MCORE,MDOC,MACT,NUMLOC)
      IF (IFCORE.EQ.0) MCORE = 0
C
      L1 = NUM
      L2 = (L1*L1+L1)/2
      L3 = L1*L1
      M1 = NUMLOC - NOUTA
C
C     ----- PARTITION FAST MEMORY -----
C
      CALL VALFM(LOADFM)
      LSVEC  = 1 + LOADFM
      LV     = LSVEC  + L3
      LARRAY = LV     + L3
      LAMOD  = LARRAY + L2
      LTRAN  = LAMOD  + NAT
      LAST   = LTRAN  + M1*M1
C
      NEED = LAST - LOADFM - 1
      CALL GETFM(NEED)
      IF(EXETYP.EQ.CHECK) GO TO 800
C
      CALL MOID(X(LSVEC),X(LV),X(LARRAY),X(LAMOD),X(LTRAN),
     *          L1,L2,NA,NAT,M1,MCORE)
C
  800 CONTINUE
      CALL RETFM(NEED)
      RETURN
      END
C*MODULE LOCCD   *DECK MOID
      SUBROUTINE MOID(SVEC,V,ARRAY,AMODEN,TRAN,L1,L2,LNA,LNAT,M1,MCORE)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,MOIDON,EDCOMP,DIPDCM,DEPRNT,
     *        QADDCM,ZDO,POLDCM,POLANG,POLAPP,KMIDPT
C
      PARAMETER (MXATM=500, MXSH=1000, MXGTOT=5000, NMO=500, MXAO=2047)
C
      DIMENSION SVEC(L1,L1),V(L1,L1),ARRAY(L2),AMODEN(LNAT),TRAN(M1,M1)
      DIMENSION ITEMP(5), TEMP2(5)
C
      COMMON /EDCMP / MOIDNO(5,NMO),ZIJ(NMO),ZMO(5,NMO),IJMO(2,NMO),
     *                MOIJ(NMO),NMOIJ(NMO),NMOAT(NMO),OCCUP(NMO),MOIDON,
     *                EDCOMP,DIPDCM,IPROT(5),NPROT,DEPRNT,QADDCM,ZDO,
     *                POLDCM,POLANG,POLAPP,KMIDPT
      COMMON /GSSORD/ IORDER(MXAO),JORDER(MXAO),NORDER
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),
     *                KNG(MXSH),KLOC(MXSH),KMIN(MXSH),
     *                KMAX(MXSH),NSHELL
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      PARAMETER (ZERO=0.00D+00,ONE=1.0D+00, TWO=2.0D+00, VTOL=0.15D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RHF,ROHF/8HRHF     ,8HROHF    /
#else
      CHARACTER*8 :: ROHF_STR
      EQUIVALENCE (ROHF, ROHF_STR)
      CHARACTER*8 :: RHF_STR
      EQUIVALENCE (RHF, RHF_STR)
      DATA RHF_STR,ROHF_STR/"RHF     ","ROHF    "/
#endif
C
C     ----- THIS ROUTINE IDENTIFIES THE LMOS USING THE NORMALIZED
C      MULLIKEN POPULATION. ONCE THE LMOS ARE
C      IDENTIFIED, THEY ARE REORDERED (LONE PAIRS ON ATOM 1 FIRST
C      THEN ANY BOND BETWEEN ATOMS 1 2, ETC.) AND A NUCLEAR LOCAL
C      CHARGE DISTRIBUTION IS ASSIGNED -----
C
      CALL DAREAD(IDAF,IODA,V    ,L1*L1,71,0)
      CALL DAREAD(IDAF,IODA,ARRAY,L2   ,12,0)
      CALL CPYTSQ(ARRAY,SVEC,L1,1)
      CALL DAREAD(IDAF,IODA,TRAN ,M1*M1,73,0)
C
C     GENERATE THE OCCUPATION NUMBERS
C
      CALL VCLR(OCCUP,1,NAT)
      DO 100 I=1,LNA
         IF (SCFTYP .EQ. RHF)  OCCUP(I) = TWO
         IF (SCFTYP .EQ. ROHF) THEN
            IF (I.LE.NB) THEN
               OCCUP(I) = TWO
            ELSE
               OCCUP(I) = ONE
            END IF
         END IF
  100 CONTINUE
C
      IF (MASWRK) WRITE(IW,*) 'NORMALIZED MULL. POP. FOR EACH LMO'
      DO 490 K = 1,LNA
         CALL VCLR(AMODEN,1,NAT)
         ICN = 0
         I = 0
         DO 390 II = 1,NSHELL
            IAT = KATOM(II)
            MINI = KMIN(II)
            MAXI = KMAX(II)
            DO 380 N = MINI,MAXI
               I = I + 1
               CXS = ZERO
               DO 370 L = 1,L1
                  CXS = CXS + V(L,K)*SVEC(L,I)
  370          CONTINUE
               AMODEN(IAT) = AMODEN(IAT) + V(I,K)*CXS
  380       CONTINUE
  390    CONTINUE
         NMIN = MIN(NAT,11)
C
         IF (MASWRK) THEN
         WRITE(IW,FMT='(1X,I3,2X,11F6.3)') K, (AMODEN(JJ), JJ=1,NMIN)
         IF (NAT .GT. 11) WRITE(IW,FMT='(6X,      11F6.3)')
     *                                        (AMODEN(JJ), JJ=12,NAT)
         END IF
         DO 450 JJ = 1,NAT
            AVAL = AMODEN(JJ)
            IF (AVAL.GE.VTOL) THEN
               IATOM = JJ
               ICN = ICN + 1
               MOIDNO(ICN,K) = IATOM
            END IF
  450    CONTINUE
         NMOAT(K) = ICN
         DO 470 JJ = 1,ICN
            ZMO(JJ,K) = OCCUP(K)/ICN
  470    CONTINUE
  490 CONTINUE
C
      IF (.NOT. EDCOMP .AND. MASWRK) THEN
C
C     ----- $LOCAL VALUES ARE NOW TRANSFERRED TO APPROPRIATE
C     ARRAYS -----
C
         DO 495 L = 1,NMO
            I = IJMO(1,L)
            J = IJMO(2,L)
            IF (MOIJ(L) .GT. 0) MOIDNO(I,J) = MOIJ(L)
            IF (ZIJ(L) .GE. ZERO) ZMO(I,J) = ZIJ(L)
            IF (NMOIJ(L) .GE. ZERO) NMOAT(L) = NMOIJ(L)
  495    CONTINUE
         WRITE(IW,9010)
         DO 500 K = 1,LNA
            ICN = MIN(5,NMOAT(K))
            IF(ICN.LT.NMOAT(K)) THEN
              WRITE(IW,9115) K, (MOIDNO(J,K),J=1,ICN)
            ELSE
              WRITE(IW,9110) K, (MOIDNO(J,K),J=1,ICN)
            END IF
  500    CONTINUE
         RETURN
      END IF
C
C     ---- ORDERING OF LMO'S ----
C
      NORDER = 1
      DO 510 JJ = 1,MXAO
         IORDER(JJ) = JJ
  510 CONTINUE
      DO 570 K = 1,5
         ICNT = MCORE+1
         DO 560 IMIN = 0,NAT
            DO 550 J = MCORE+1, LNA
               ID = MOIDNO(6-K,J)
               IF (ID.EQ.IMIN) THEN
                  CALL ICOPY(5,MOIDNO(1,J),1,ITEMP,1)
                  CALL DCOPY(5,ZMO(1,J),1,TEMP2,1)
                  ITEMP3 = NMOAT(J)
                  ITEMP4 = IORDER(J)
                  ITEMP5 = INT(OCCUP(J))
                  LENGTH = J - ICNT
                  DO 530 JJ = 1,LENGTH
                     N1 = J - JJ
                     N2 = J - JJ+1
                     CALL ICOPY(5,MOIDNO(1,N1),1,MOIDNO(1,N2),1)
                     CALL DCOPY(5,ZMO(1,N1),1,ZMO(1,N2),1)
                     NMOAT(N2) = NMOAT(N1)
                     IORDER(N2) = IORDER(N1)
                     OCCUP(N2) = OCCUP(N1)
  530             CONTINUE
                  CALL ICOPY(5,ITEMP,1,MOIDNO(1,ICNT),1)
                  CALL DCOPY(5,TEMP2,1,ZMO(1,ICNT),1)
                  NMOAT(ICNT) = ITEMP3
                  IORDER(ICNT) = ITEMP4
                  OCCUP(ICNT) = ITEMP5
                  ICNT = ICNT + 1
               END IF
  550       CONTINUE
  560    CONTINUE
  570 CONTINUE
C
      NORDER = 1
      CALL ICOPY(L1,IORDER,1,JORDER,1)
      CALL REORDR(V,JORDER,L1,L1)
      CALL DAWRIT(IDAF,IODA,V,L1*L1,71,0)
C
      NCORE = LNA - M1
      ICNT = 1
      DO 610 I = 1,LNA
         J = IORDER(I) - NCORE
         IF (J .GT. 0) THEN
            JORDER(ICNT) = J
            ICNT = ICNT + 1
         END IF
  610 CONTINUE
      CALL REORDR(TRAN,JORDER,M1,M1)
      CALL DAWRIT(IDAF,IODA,TRAN,M1*M1,73,0)
C
C     ----- $LOCAL VALUES ARE NOW TRANSFERRED TO APPROPRIATE
C     ARRAYS -----
C
      DO 680 L = 1,NMO
         I = IJMO(1,L)
         J = IJMO(2,L)
         IF (MOIJ(L) .GT. 0) MOIDNO(I,J) = MOIJ(L)
         IF (ZIJ(L) .GE. ZERO) ZMO(I,J) = ZIJ(L)
         IF (NMOIJ(L) .GE. ZERO) NMOAT(L) = NMOIJ(L)
  680 CONTINUE
C
      IF (MASWRK) THEN
         WRITE(IW,9000)
         DO 700 K = 1,LNA
            ICN = MIN(5,NMOAT(K))
            IF(ICN.LT.NMOAT(K)) THEN
               WRITE(IW,9105) K, (MOIDNO(J,K),ZMO(J,K),J=1,ICN)
            ELSE
               WRITE(IW,9100) K, (MOIDNO(J,K),ZMO(J,K),J=1,ICN)
            END IF
  700    CONTINUE
      END IF
C
      RETURN
 9000 FORMAT(/1X,'ATOMS ARE ASSIGNED TO THE LMOS BASED ON THEIR',
     *           ' MULLIKEN POPULATION.'/
     *        1X,'LMO NUCLEAR CHARGE IS IN (). THE LMOS HAVE BEEN',
     *           ' REORDERED BASED ON THEIR'/
     *        1X,'CONNECTIVITY. (SEE THE MANUAL FOR FURTHER DETAILS)'/)
 9010 FORMAT(/1X,'ATOMS ARE ASSIGNED TO THE LMOS BASED ON THEIR ',
     *       'MULLIKEN POPULATION.'/)
 9100 FORMAT(1X,'ORBITAL ',I3,' BELONGS TO ATOM(S) ',
     *       5(I3,' (',F4.2,') '))
 9105 FORMAT(1X,'ORBITAL ',I3,' BELONGS TO ATOM(S) ',
     *       5(I3,' (',F4.2,') '),' ...')
 9110 FORMAT(1X,'ORBITAL ',I3,' BELONGS TO ATOM(S) ',5I3)
 9115 FORMAT(1X,'ORBITAL ',I3,' BELONGS TO ATOM(S) ',5I3,' ...')
      END
C*MODULE LOCCD   *DECK QUADMM
      SUBROUTINE QUADMM
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL PAPER,GOPARR,DSKWRK,MASWRK,MOIDON,EDCOMP,DIPDCM,
     *        DEPRNT,QADDCM,ZDO,POLDCM,POLANG,POLAPP,KMIDPT
C
      PARAMETER (MXATM=500, NMO=500)
C
      COMMON /EDCMP / MOIDNO(5,NMO),ZIJ(NMO),ZMO(5,NMO),IJMO(2,NMO),
     *                MOIJ(NMO),NMOIJ(NMO),NMOAT(NMO),OCCUP(NMO),MOIDON,
     *                EDCOMP,DIPDCM,IPROT(5),NPROT,DEPRNT,QADDCM,ZDO,
     *                POLDCM,POLANG,POLAPP,KMIDPT
      COMMON /ELPROP/ ELDLOC,ELMLOC,ELPLOC,ELFLOC,
     *                IEDEN,IEMOM,IEPOT,IEFLD,MODENS,
     *                IEDOUT,IEMOUT,IEPOUT,IEFOUT,
     *                IEDINT,IEMINT,IEPINT,IEFINT
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /XYZPRP/ XP,YP,ZP,
     *                DMX,DMY,DMZ,
     *                QXX,QYY,QZZ,QXY,QXZ,QYZ,
     *                QMXX,QMYY,QMZZ,QMXY,QMXZ,QMYZ,
     *                OXXX,OXXY,OXXZ,OXYY,OYYY,OYYZ,
     *                OXZZ,OYZZ,OZZZ,OXYZ,
     *                OMXXX,OMXXY,OMXXZ,OMXYY,OMYYY,
     *                OMYYZ,OMXZZ,OMYZZ,OMZZZ,OMXYZ
C
      PARAMETER (QFAC=1.345044D+00, PT5=0.5D+00, ONEPT5=1.5D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /,  DEBUG/8HDEBUG   /, DBUGME/8HELMOMC  /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA CHECK_STR/"CHECK   "/,  DEBUG_STR/"DEBUG   "/, 
     * DBUGME_STR/"ELMOMC  "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QDMOM/8HQDMOM   /
#else
      CHARACTER*8 :: QDMOM_STR
      EQUIVALENCE (QDMOM, QDMOM_STR)
      DATA QDMOM_STR/"QDMOM   "/
#endif
C
C     ----- CALCULATE ELECTROSTATIC MOMENTS -----
C
      IEMOM = 2
      LNA = NA
      PAPER = (IEMOUT.GE.0  .AND. NPRINT.NE.-5) .AND. MASWRK
      IF (EXETYP.EQ.DEBUG  .OR.  EXETYP.EQ.DBUGME) THEN
         PAPER = .TRUE. .AND. MASWRK
      END IF
      IF(PAPER) WRITE(IW,910)
      L1 = NUM
      L2 = (L1*L1+L1)/2
      NVAL = 6
C
C     ----- GROW MEMORY -----
C
      LOADFM = 0
      CALL VALFM(LOADFM)
      IELM   = LOADFM + 1
      IEMW   = IELM   + NVAL*L2
      IVEC   = IEMW   + NVAL*225
      IDENSA = IVEC   + L1*LNA
      LDIP   = IDENSA + L2
      LAST   = LDIP   + 3*LNA
      NEED = LAST - LOADFM - 1
      CALL GETFM(NEED)
      IF (EXETYP.EQ.CHECK) THEN
         CALL RETFM(NEED)
         RETURN
      END IF
C
C     ----- GET LMOS AND LCD DIPOLES
C
      CALL DAREAD(IDAF,IODA,X(IVEC),L1*LNA,71,0)
      CALL DAREAD(IDAF,IODA,X(LDIP),3*LNA,250,0)
C
C     ----- LOOP OVER POINTS FOR MULTIPOLE EXPANSION -----
C
      WRITE(IW,920)
      NLMO = 0
  210 NLMO = NLMO + 1
C
      IF (NLMO .GT. LNA) THEN
         CALL RETFM(NEED)
         RETURN
C        ******
      END IF
      XP = X(LDIP+3*(NLMO-1))
      YP = X(LDIP+1+3*(NLMO-1))
      ZP = X(LDIP+2+3*(NLMO-1))
C
C
C     ----- COMPUTE MULTIPOLE INTEGRALS OVER BASIS FUNCTIONS -----
C
      CALL PRCALC(QDMOM,X(IELM),X(IEMW),NVAL,L2)
C
C     ----- DENSITY FOR LMO NLMO -----
C
      CALL DMTX(X(IDENSA),X(IVEC+L1*(NLMO-1)),OCCUP(NLMO),1,L1,L1)
C
C                         QUADRUPOLE
      INDEX = IELM
      QMXX = -TRACEP(X(IDENSA),X(INDEX),L1)
      INDEX = INDEX + L2
      QMYY = -TRACEP(X(IDENSA),X(INDEX),L1)
      INDEX = INDEX + L2
      QMZZ = -TRACEP(X(IDENSA),X(INDEX),L1)
      INDEX = INDEX + L2
      QMXY = -TRACEP(X(IDENSA),X(INDEX),L1)
      INDEX = INDEX + L2
      QMXZ = -TRACEP(X(IDENSA),X(INDEX),L1)
      INDEX = INDEX + L2
      QMYZ = -TRACEP(X(IDENSA),X(INDEX),L1)
C
C                 ADD NUCLEAR CONTRIBUTION TO MOMENTS
C
      ICN = NMOAT(NLMO)
      DO 280  I=1,ICN
         ZNUC = ZMO(I,NLMO)
         XN = C(1,MOIDNO(I,NLMO)) - XP
         YN = C(2,MOIDNO(I,NLMO)) - YP
         ZN = C(3,MOIDNO(I,NLMO)) - ZP
         QMXX = QMXX + ZNUC*XN*XN
         QMYY = QMYY + ZNUC*YN*YN
         QMZZ = QMZZ + ZNUC*ZN*ZN
         QMXY = QMXY + ZNUC*XN*YN
         QMXZ = QMXZ + ZNUC*XN*ZN
         QMYZ = QMYZ + ZNUC*YN*ZN
  280 CONTINUE
C
C     ----- CONVERT UNITS, FORM QUADRUPOLE,OCTUPOLE TENSORS -----
C
      QMXX = QMXX*QFAC
      QMYY = QMYY*QFAC
      QMZZ = QMZZ*QFAC
      QMXY = QMXY*QFAC
      QMXZ = QMXZ*QFAC
      QMYZ = QMYZ*QFAC
C
      QXX = PT5*(QMXX + QMXX - QMYY - QMZZ)
      QYY = PT5*(QMYY + QMYY - QMXX - QMZZ)
      QZZ = PT5*(QMZZ + QMZZ - QMXX - QMYY)
      QXY = ONEPT5*QMXY
      QXZ = ONEPT5*QMXZ
      QYZ = ONEPT5*QMYZ
C
C     ---- OUTPUT RESULTS -----
C
      WRITE(IW,930) NLMO,XP,YP,ZP,QXX,QYY,QZZ,QXY,QXZ,QYZ
      GO TO 210
C
  910 FORMAT(10X,'LCD QUADRUPOLE MOMENTS (BUCKINGHAMS)'/)
  920 FORMAT(1X,' LMO ',5X,'X',11X,'Y',11X,'Z',12X,'QXX',9X,'QYY',9X,
     *       'QZZ',9X,'QXY',9X,'QXZ',9X,'QYZ')
  930 FORMAT(1X,I4,3F12.6,1X,6F12.6)
      END
C*MODULE LOCCD   *DECK TWEIIN
      SUBROUTINE TWEIIN(CINT,XINT,M2)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION CINT(M2), XINT(M2)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
C
      IMAX = 0
  300 CONTINUE
      IMIN = IMAX+1
      IMAX = IMAX+5
      IF (IMAX.GT.M2) IMAX=M2
      READ(IR,FMT='(1X,5E15.8)') (CINT(I),I=IMIN,IMAX)
      IF (IMAX.LT.M2) GO TO 300
C
      IMAX = 0
  305 CONTINUE
      IMIN = IMAX+1
      IMAX = IMAX+5
      IF (IMAX.GT.M2) IMAX=M2
      READ(IR,FMT='(1X,5E15.8)') (XINT(I),I=IMIN,IMAX)
      IF (IMAX.LT.M2) GO TO 305
C
      CALL DAWRIT(IDAF,IODA,CINT,M2,59,0)
      CALL DAWRIT(IDAF,IODA,XINT,M2,60,0)
C
      RETURN
      END
C*MODULE LOCCD   *DECK TXYZ
      SUBROUTINE TXYZ(VEC,SQMAT,OCCUP,L1,M1,NLMO)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
      PARAMETER (NMO=500)
C
      LOGICAL IANDJ,OUT,NORM,DOUBLE,GOPARR,DSKWRK,MASWRK
C
      DIMENSION GG(225),FTX(225),DIJ(225),XIN(125),YIN(125),ZIN(125),
     *          IX(35),IY(35),IZ(35),JX(35),JY(35),JZ(35),
     *          IJX(225),IJY(225),IJZ(225),VEC(L1,NLMO),SQMAT(M1,M1),
     *          OCCUP(NMO),FTY(225),FTZ(225)
C
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /SSGG  / S(225),G(225)
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
      COMMON /SYMIND/ TOL,II,JJ,LIT,LJT,MINI,MINJ,MAXI,MAXJ,IANDJ
C
      PARAMETER (ZERO=0.0D+00, PT5=0.5D+00, ONE=1.0D+00, TWO=2.0D+00,
     *           SQRT3=1.73205080756888D+00,
     *           SQRT5=2.23606797749979D+00, SQRT7=2.64575131106459D+00,
     *           RLN10=2.30258D+00)
C
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
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DBUGME/8HTXYZ    /
#else
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA DBUGME_STR/"TXYZ    "/
#endif
C
C     ----- INTIALIZE PARALLEL -----
C
      IPCOUNT = ME - 1
C
C     ----- CALCULATE OVERLAP, KINETIC ENERGY, AND BARE NUCLEUS
C           HAMILTONIAN ONE ELECTRON INTEGRALS -----
C     ----- MOPAC INTEGRALS ARE DONE ELSEWHERE -----
C
      TOL = RLN10*ITOL
      OUT = MASWRK .AND. EXETYP.EQ.DBUGME
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C     ----- GET CORE MEMORY -----
C     -TX- AT X(I10)
C     -TY- AT X(I20)
C     -TZ- AT X(I30)
C
      L2 = (NUM*(NUM+1))/2
C
      CALL VALFM(LOADFM)
      I10  = LOADFM + 1
      I20  = I10    + L2
      I30  = I20    + L2
      LAST = I30    + L2
      NEED = LAST-LOADFM-1
      CALL GETFM(NEED)
C
      NDUM = 3*L2
      DO 100 I = 1,NDUM
         X(I+LOADFM) = ZERO
  100 CONTINUE
C
C     ----- ISHELL
C
      DO 720 II = 1,NSHELL
         I = KATOM(II)
         XI = C(1,I)
         YI = C(2,I)
         ZI = C(3,I)
         I1 = KSTART(II)
         I2 = I1+KNG(II)-1
         LIT = KTYPE(II)
         MINI = KMIN(II)
         MAXI = KMAX(II)
         LOCI = KLOC(II)-MINI
C
C     ----- JSHELL
C
         DO 700 JJ = 1,II
C
C     ----- GO PARALLEL! -----
C
            IF (GOPARR) THEN
               IPCOUNT = IPCOUNT + 1
               IF (MOD(IPCOUNT,NPROC).NE.0) GO TO 690
            END IF
            J = KATOM(JJ)
            XJ = C(1,J)
            YJ = C(2,J)
            ZJ = C(3,J)
            J1 = KSTART(JJ)
            J2 = J1+KNG(JJ)-1
            LJT = KTYPE(JJ)
            MINJ = KMIN(JJ)
            MAXJ = KMAX(JJ)
            LOCJ = KLOC(JJ)-MINJ
            NROOTS = (LIT+LJT-2)/2+1
            RR = (XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
            IANDJ = II .EQ. JJ
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
            IJ = 0
            MAX = MAXJ
            DO 160 I = MINI,MAXI
               NX = IX(I)
               NY = IY(I)
               NZ = IZ(I)
               IF (IANDJ) MAX = I
               DO 140 J = MINJ,MAX
                  IJ = IJ+1
                  IJX(IJ) = NX+JX(J)
                  IJY(IJ) = NY+JY(J)
                  IJZ(IJ) = NZ+JZ(J)
                  FTX(IJ) = ONE + TWO*JX(J)
                  FTY(IJ) = ONE + TWO*JY(J)
                  FTZ(IJ) = ONE + TWO*JZ(J)
  140          CONTINUE
  160       CONTINUE
            DO 180 I = 1,IJ
               S(I) = ZERO
               GG(I) = ZERO
               G(I) = ZERO
  180       CONTINUE
C
C     ----- I PRIMITIVE
C
            JGMAX = J2
            DO 520 IG = I1,I2
               AI = EX(IG)
               ARRI = AI*RR
               AXI = AI*XI
               AYI = AI*YI
               AZI = AI*ZI
               CSI = CS(IG)
               CPI = CP(IG)
               CDI = CD(IG)
               CFI = CF(IG)
               CGI = CG(IG)
C
C
C     ----- J PRIMITIVE
C
               IF (IANDJ) JGMAX = IG
               DO 500 JG = J1,JGMAX
                  AJ = EX(JG)
                  AA = AI+AJ
                  AA1 = ONE/AA
                  DUM = AJ*ARRI*AA1
                  IF (DUM .GT. TOL) GO TO 500
                  FAC = EXP(-DUM)
                  CSJ = CS(JG)
                  CPJ = CP(JG)
                  CDJ = CD(JG)
                  CFJ = CF(JG)
                  CGJ = CG(JG)
                  AX = (AXI+AJ*XJ)*AA1
                  AY = (AYI+AJ*YJ)*AA1
                  AZ = (AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR
C
                  DOUBLE=IANDJ.AND.IG.NE.JG
                  MAX = MAXJ
                  NN = 0
                  DUM1 = ZERO
                  DUM2 = ZERO
                  DO 220 I = MINI,MAXI
                     IF (I.EQ.1) DUM1=CSI*FAC
                     IF (I.EQ.2) DUM1=CPI*FAC
                     IF (I.EQ.5) DUM1=CDI*FAC
                     IF ((I.EQ.8).AND.NORM) DUM1=DUM1*SQRT3
                     IF (I.EQ.11) DUM1=CFI*FAC
                     IF ((I.EQ.14).AND.NORM) DUM1=DUM1*SQRT5
                     IF ((I.EQ.20).AND.NORM) DUM1=DUM1*SQRT3
                     IF (I.EQ.21) DUM1=CGI*FAC
                     IF ((I.EQ.24).AND.NORM) DUM1=DUM1*SQRT7
                     IF ((I.EQ.30).AND.NORM) DUM1=DUM1*SQRT5/SQRT3
                     IF ((I.EQ.33).AND.NORM) DUM1=DUM1*SQRT3
                     IF (IANDJ) MAX = I
                     DO 200 J = MINJ,MAX
                        IF (J.EQ.1) THEN
                           DUM2=DUM1*CSJ
                           IF (DOUBLE) THEN
                              IF (I.LE.1) THEN
                                 DUM2=DUM2+DUM2
                              ELSE
                                 DUM2=DUM2+CSI*CPJ*FAC
                              END IF
                           END IF
                        ELSE IF (J.EQ.2) THEN
                           DUM2=DUM1*CPJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF (J.EQ.5) THEN
                           DUM2=DUM1*CDJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((J.EQ.8).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        ELSE IF (J.EQ.11) THEN
                           DUM2=DUM1*CFJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((J.EQ.14).AND.NORM) THEN
                           DUM2=DUM2*SQRT5
                        ELSE IF ((J.EQ.20).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        ELSE IF (J.EQ.21) THEN
                           DUM2=DUM1*CGJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((J.EQ.24).AND.NORM) THEN
                           DUM2=DUM2*SQRT7
                        ELSE IF ((J.EQ.30).AND.NORM) THEN
                           DUM2=DUM2*SQRT5/SQRT3
                        ELSE IF ((J.EQ.33).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        END IF
                        NN = NN+1
                        DIJ(NN) = DUM2
  200                CONTINUE
  220             CONTINUE
C
C     ----- OVERLAP AND KINETIC ENERGY
C
                  T = SQRT(AA1)
                  T1 = -TWO*AJ*AJ*T
                  T2 = -PT5*T
                  X0 = AX
                  Y0 = AY
                  Z0 = AZ
                  IN = -5
                  DO 320 I = 1,LIT
                     IN = IN+5
                     NI = I
                     DO 300 J = 1,LJT
                        JN = IN+J
                        NJ = J
                        CALL STVINT
                        XIN(JN) = XINT*T
                        YIN(JN) = YINT*T
                        ZIN(JN) = ZINT*T
                        NJ = J+2
                        CALL STVINT
                        XIN(JN+25) = XINT*T1
                        YIN(JN+25) = YINT*T1
                        ZIN(JN+25) = ZINT*T1
                        NJ = J-2
                        IF (NJ .GT. 0) THEN
                           CALL STVINT
                        ELSE
                           XINT = ZERO
                           YINT = ZERO
                           ZINT = ZERO
                        END IF
                        N = (J-1)*(J-2)
                        DUM = N * T2
                        XIN(JN+50) = XINT*DUM
                        YIN(JN+50) = YINT*DUM
                        ZIN(JN+50) = ZINT*DUM
  300                CONTINUE
  320             CONTINUE
                  DO 340 I = 1,IJ
                     NX = IJX(I)
                     NY = IJY(I)
                     NZ = IJZ(I)
                     YZ = YIN(NY)*ZIN(NZ)
                     DUM = YZ*XIN(NX)
                     DUMX = (XIN(NX+25)+XIN(NX+50))*YZ
                     DUMY = (YIN(NY+25)+YIN(NY+50))*XIN(NX)*ZIN(NZ)
                     DUMZ = (ZIN(NZ+25)+ZIN(NZ+50))*XIN(NX)*YIN(NY)
                     S(I)  = S(I) + DIJ(I)*(DUM*AJ*FTX(I)+DUMX)
                     G(I)  = G(I) + DIJ(I)*(DUM*AJ*FTY(I)+DUMY)
                     GG(I) = GG(I)+ DIJ(I)*(DUM*AJ*FTZ(I)+DUMZ)
  340             CONTINUE
C
C     ----- END PRIMITIVES -----
C
  500          CONTINUE
  520       CONTINUE
C
C     ----- SET UP KE MATRICES
C
            MAX = MAXJ
            NN = 0
            DO 620 I = MINI,MAXI
               LI = LOCI+I
               IN = (LI*(LI-1))/2
               IF (IANDJ) MAX = I
               DO 600 J = MINJ,MAX
                  LJ = LOCJ+J
                  JN = LJ+IN
                  NN = NN+1
                  X(JN-1+I10) = S(NN)
                  X(JN-1+I20) = G(NN)
                  X(JN-1+I30) = GG(NN)
  600          CONTINUE
  620       CONTINUE
C
C     ----- END PARALLEL
C
  690    CONTINUE
C
C     ----- END SHELLS -----
C
  700    CONTINUE
  720 CONTINUE
C
C     ----- SUM UP PARTIAL CONTRIBUTIONS IF PARALLEL -----
C
      IF (GOPARR) CALL DDI_GSUMF(1310,X(I10),3*L2)
C
C     OPTIONALLY, PRINT THEM OUT
C
      IF(OUT) THEN
         WRITE(IW,*) 'TX'
         CALL PRTRIL(X(I10),L1)
         WRITE(IW,*) 'TY'
         CALL PRTRIL(X(I20),L1)
         WRITE(IW,*) 'TZ'
         CALL PRTRIL(X(I30),L1)
      END IF
C
      DO 950 L = 1,3
        IF (L .EQ. 1) IX0 = I10
        IF (L .EQ. 2) IX0 = I20
        IF (L .EQ. 3) IX0 = I30
        DO 900 K = 1,NLMO
           SQMAT(K,L) = ZERO
           IJ = 0
           DO 850 I = 1,L1
              DO 800 J = 1,I
                 IJ = IJ + 1
                 ONETWO = TWO
                 IF (I.EQ.J) ONETWO = ONE
                 SQMAT(K,L) = SQMAT(K,L) +
     *                        X(IX0+IJ-1)*VEC(I,K)*VEC(J,K)*ONETWO
  800         CONTINUE
  850      CONTINUE
           SQMAT(K,L) = OCCUP(K)*SQMAT(K,L)
  900   CONTINUE
  950 CONTINUE
C
      DO 1020 K = 1,M1
         SQMAT(K,4) = ZERO
         DO 1010 L = 1,3
            SQMAT(K,4) = SQMAT(K,4) + SQMAT(K,L)
 1010    CONTINUE
 1020 CONTINUE
C
      IF (MASWRK) THEN
         WRITE(IW,*)
         WRITE(IW,*) 'CARTESIAN COMPONENTS OF THE KINETIC ENERGY'
         WRITE(IW,*)
         WRITE(IW,9000)
         DO 1030 K = 1,M1
            WRITE(IW,9100) K, (SQMAT(K,L),L=1,4)
 1030    CONTINUE
      END IF
C
C     ----- DONE WITH INTEGRALS -----
C
      CALL RETFM(NEED)
      RETURN
C
 9000 FORMAT(1X,'LMO',7X,'KE(X)',8X,'KE(Y) ',8X,'KE(Z) ',9X,'TOTAL ')
 9100 FORMAT(1X,I3,4(2X,F12.6))
      END
C*MODULE LOCCD   *DECK VIJINT
      SUBROUTINE VIJINT(HCORE,LAT,L1,L2)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL IANDJ,OUT,NORM,DOUBLE,GOPARR,DSKWRK,MASWRK
C
      DIMENSION HCORE(L2)
      DIMENSION DIJ(225),XIN(125),YIN(125),ZIN(125),
     *          IX(35),IY(35),IZ(35),JX(35),JY(35),JZ(35),
     *          IJX(225),IJY(225),IJZ(225)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /SSGG  / S(225),G(225)
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
      COMMON /SYMIND/ TOL,II,JJ,LIT,LJT,MINI,MINJ,MAXI,MAXJ,IANDJ
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00,
     *           PI212=1.1283791670955D+00, SQRT3=1.73205080756888D+00,
     *           SQRT5=2.23606797749979D+00, SQRT7=2.64575131106459D+00,
     *           RLN10=2.30258D+00)
C
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
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DEBUG/8HDEBUG   /,DBUGME/8HINT1    /
#else
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA DEBUG_STR/"DEBUG   "/,DBUGME_STR/"INT1    "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA NONE/4HNONE/
#else
      CHARACTER*4 :: NONE_STR
      EQUIVALENCE (NONE, NONE_STR)
      DATA NONE_STR/"NONE"/
#endif
C
C     ----- INTIALIZE PARALLEL -----
C
      IPCOUNT = ME - 1
C
C     ----- CALCULATE OVERLAP, KINETIC ENERGY, AND BARE NUCLEUS
C           HAMILTONIAN ONE ELECTRON INTEGRALS -----
C     ----- MOPAC INTEGRALS ARE DONE ELSEWHERE -----
C
      IF(MPCTYP.NE.NONE) THEN
         CALL MPCINT
         RETURN
      END IF
C
      TOL = RLN10*ITOL
      OUT = NPRINT.EQ.3  .OR.  EXETYP.EQ.DEBUG  .OR.  EXETYP.EQ.DBUGME
     *      .AND. MASWRK
      IF (OUT) WRITE (IW,9000)
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
      IF(GOPARR) CALL VCLR(HCORE,1,L2)
C
C     ----- ISHELL
C
      DO 720 II = 1,NSHELL
         I = KATOM(II)
         XI = C(1,I)
         YI = C(2,I)
         ZI = C(3,I)
         I1 = KSTART(II)
         I2 = I1+KNG(II)-1
         LIT = KTYPE(II)
         MINI = KMIN(II)
         MAXI = KMAX(II)
         LOCI = KLOC(II)-MINI
C
C     ----- JSHELL
C
         DO 700 JJ = 1,II
C
C     ----- GO PARALLEL! -----
C
            IF (GOPARR) THEN
               IPCOUNT = IPCOUNT + 1
               IF (MOD(IPCOUNT,NPROC).NE.0) GO TO 690
            END IF
            J = KATOM(JJ)
            XJ = C(1,J)
            YJ = C(2,J)
            ZJ = C(3,J)
            J1 = KSTART(JJ)
            J2 = J1+KNG(JJ)-1
            LJT = KTYPE(JJ)
            MINJ = KMIN(JJ)
            MAXJ = KMAX(JJ)
            LOCJ = KLOC(JJ)-MINJ
            NROOTS = (LIT+LJT-2)/2+1
            RR = (XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
            IANDJ = II .EQ. JJ
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
            IJ = 0
            MAX = MAXJ
            DO 160 I = MINI,MAXI
               NX = IX(I)
               NY = IY(I)
               NZ = IZ(I)
               IF (IANDJ) MAX = I
               DO 140 J = MINJ,MAX
                  IJ = IJ+1
                  IJX(IJ) = NX+JX(J)
                  IJY(IJ) = NY+JY(J)
                  IJZ(IJ) = NZ+JZ(J)
  140          CONTINUE
  160       CONTINUE
            DO 180 I = 1,IJ
               S(I) = ZERO
               G(I) = ZERO
  180       CONTINUE
C
C     ----- I PRIMITIVE
C
            JGMAX = J2
            DO 520 IG = I1,I2
               AI = EX(IG)
               ARRI = AI*RR
               AXI = AI*XI
               AYI = AI*YI
               AZI = AI*ZI
               CSI = CS(IG)
               CPI = CP(IG)
               CDI = CD(IG)
               CFI = CF(IG)
               CGI = CG(IG)
C
C     ----- J PRIMITIVE
C
               IF (IANDJ) JGMAX = IG
               DO 500 JG = J1,JGMAX
                  AJ = EX(JG)
                  AA = AI+AJ
                  AA1 = ONE/AA
                  DUM = AJ*ARRI*AA1
                  IF (DUM .GT. TOL) GO TO 500
                  FAC = EXP(-DUM)
                  CSJ = CS(JG)
                  CPJ = CP(JG)
                  CDJ = CD(JG)
                  CFJ = CF(JG)
                  CGJ = CG(JG)
                  AX = (AXI+AJ*XJ)*AA1
                  AY = (AYI+AJ*YJ)*AA1
                  AZ = (AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR
C
                  DOUBLE=IANDJ.AND.IG.NE.JG
                  MAX = MAXJ
                  NN = 0
                  DUM1 = ZERO
                  DUM2 = ZERO
                  DO 220 I = MINI,MAXI
                     IF (I.EQ.1) DUM1=CSI*FAC
                     IF (I.EQ.2) DUM1=CPI*FAC
                     IF (I.EQ.5) DUM1=CDI*FAC
                     IF ((I.EQ.8).AND.NORM) DUM1=DUM1*SQRT3
                     IF (I.EQ.11) DUM1=CFI*FAC
                     IF ((I.EQ.14).AND.NORM) DUM1=DUM1*SQRT5
                     IF ((I.EQ.20).AND.NORM) DUM1=DUM1*SQRT3
                     IF (I.EQ.21) DUM1=CGI*FAC
                     IF ((I.EQ.24).AND.NORM) DUM1=DUM1*SQRT7
                     IF ((I.EQ.30).AND.NORM) DUM1=DUM1*SQRT5/SQRT3
                     IF ((I.EQ.33).AND.NORM) DUM1=DUM1*SQRT3
                     IF (IANDJ) MAX = I
                     DO 200 J = MINJ,MAX
                        IF (J.EQ.1) THEN
                           DUM2=DUM1*CSJ
                           IF (DOUBLE) THEN
                              IF (I.LE.1) THEN
                                 DUM2=DUM2+DUM2
                              ELSE
                                 DUM2=DUM2+CSI*CPJ*FAC
                              END IF
                           END IF
                        ELSE IF (J.EQ.2) THEN
                           DUM2=DUM1*CPJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF (J.EQ.5) THEN
                           DUM2=DUM1*CDJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((J.EQ.8).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        ELSE IF (J.EQ.11) THEN
                           DUM2=DUM1*CFJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((J.EQ.14).AND.NORM) THEN
                           DUM2=DUM2*SQRT5
                        ELSE IF ((J.EQ.20).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        ELSE IF (J.EQ.21) THEN
                           DUM2=DUM1*CGJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((J.EQ.24).AND.NORM) THEN
                           DUM2=DUM2*SQRT7
                        ELSE IF ((J.EQ.30).AND.NORM) THEN
                           DUM2=DUM2*SQRT5/SQRT3
                        ELSE IF ((J.EQ.33).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        END IF
                        NN = NN+1
                        DIJ(NN) = DUM2
  200                CONTINUE
  220             CONTINUE
C
C     ----- NUCLEAR ATTRACTION
C
                  DUM = PI212*AA1
                  DO 400 I = 1,IJ
                     DIJ(I) = DIJ(I)*DUM
  400             CONTINUE
                  AAX = AA*AX
                  AAY = AA*AY
                  AAZ = AA*AZ
                  ZNUC = -ONE
                  CX = C(1,LAT)
                  CY = C(2,LAT)
                  CZ = C(3,LAT)
                  XX = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
                  IF (NROOTS.LE.3) CALL RT123
                  IF (NROOTS.EQ.4) CALL ROOT4
                  IF (NROOTS.EQ.5) CALL ROOT5
                  MM = 0
                  DO 430 K = 1,NROOTS
                     UU = AA*U(K)
                     WW = W(K)*ZNUC
                     TT = ONE/(AA+UU)
                     T = SQRT(TT)
                     X0 = (AAX+UU*CX)*TT
                     Y0 = (AAY+UU*CY)*TT
                     Z0 = (AAZ+UU*CZ)*TT
                     IN = -5+MM
                     DO 420 I = 1,LIT
                        IN = IN+5
                        NI = I
                        DO 410 J = 1,LJT
                           JN = IN+J
                           NJ = J
                           CALL STVINT
                           XIN(JN) = XINT
                           YIN(JN) = YINT
                           ZIN(JN) = ZINT*WW
  410                   CONTINUE
  420                CONTINUE
                     MM = MM+25
  430             CONTINUE
                  DO 450 I = 1,IJ
                     NX = IJX(I)
                     NY = IJY(I)
                     NZ = IJZ(I)
                     DUM = ZERO
                     MM = 0
                     DO 440 K = 1,NROOTS
                        DUM = DUM+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM)
                        MM = MM+25
  440                CONTINUE
                     G(I) = G(I)+DUM*DIJ(I)
  450             CONTINUE
C
C     ----- END PRIMITIVES -----
C
  500          CONTINUE
  520       CONTINUE
C
C     ----- SET UP H-CORE MATRIX
C
            MAX = MAXJ
            NN = 0
            DO 620 I = MINI,MAXI
               LI = LOCI+I
               IN = (LI*(LI-1))/2
               IF (IANDJ) MAX = I
               DO 600 J = MINJ,MAX
                  LJ = LOCJ+J
                  JN = LJ+IN
                  NN = NN+1
                  HCORE(JN) = G(NN)
  600          CONTINUE
  620       CONTINUE
C
C     ----- END PARALLEL
C
  690    CONTINUE
C
C     ----- END SHELLS -----
C
  700    CONTINUE
  720 CONTINUE
C
C     OPTIONALLY, PRINT THEM OUT
C
      IF(OUT) THEN
         WRITE(IW,*) 'BARE NUCLEUS HAMILTONIAN INTEGRALS (H=T+V)'
         CALL PRTRIL(HCORE,L1)
      END IF
C
C     ----- DONE WITH INTEGRALS -----
C
      IF (GOPARR) CALL DDI_GSUMF(1320,HCORE,L2)
      IF (OUT) WRITE (IW,9090)
      RETURN
C
 9000 FORMAT(/10X,20("*")/10X,'1 ELECTRON INTEGRALS'/10X,20("*"))
 9090 FORMAT(1X,'...... END OF ONE-ELECTRON INTEGRALS ......')
      END
C*MODULE LOCCD   *DECK VIAM
      SUBROUTINE VIAM(VEC,XINT,ARRAY,L1,L2,M1,M2,NCORE,NLMO)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500, NMO=500, ONE=1.0D+00, TWO=2.0D+00)
      PARAMETER (PT5=0.5D+00)
C
      LOGICAL MOIDON,EDCOMP,DIPDCM,DEPRNT,QADDCM,ZDO,
     *        POLDCM,POLANG,POLAPP,KMIDPT
C
      COMMON /ECPDIM/ NCOEF1,NCOEF2,J1LEN,J2LEN,LLIM,NLIM,NTLIM,J4LEN
      COMMON /ECP2  / CLP(400),ZLP(400),NLP(400),KFIRST(MXATM,6),
     *                KLAST(MXATM,6),LMAX(MXATM),LPSKIP(MXATM),
     *                IZCORE(MXATM)
      COMMON /EDCMP / MOIDNO(5,NMO),ZIJ(NMO),ZMO(5,NMO),IJMO(2,NMO),
     *                MOIJ(NMO),NMOIJ(NMO),NMOAT(NMO),OCCUP(NMO),MOIDON,
     *                EDCOMP,DIPDCM,IPROT(5),NPROT,DEPRNT,QADDCM,ZDO,
     *                POLDCM,POLANG,POLAPP,KMIDPT
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /INTOPT/ ISCHWZ,IECP,NECP,IEFLD
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
C
      DIMENSION VEC(L1,M1), XINT(M2), ARRAY(L2)
C
      IF(IECP.EQ.0) RETURN
C
      CALL VALFM(LOADFM)
      LDCF1  = LOADFM + 1
      LJLN   = LDCF1  +  NCOEF1
      LLB1   = LJLN   +  J1LEN/NWDVAR
      LDCF4  = LLB1   + (NCOEF1*9)/NWDVAR
      LDCF2  = LDCF4  +  J4LEN
      LJ2N   = LDCF2  +  NCOEF2
      LLB2   = LJ2N   +  J2LEN/NWDVAR
      LFPQR  = LLB2   + (NCOEF2*6)/NWDVAR
      LZLM   = LFPQR  +  15625
      LLMF   = LZLM   + 581
      LLMX   = LLMF   + 122/NWDVAR
      LLMY   = LLMX   + 582/NWDVAR
      LLMZ   = LLMY   + 582/NWDVAR
      LAST   = LLMY   + 582/NWDVAR
      NEED2  = LAST - LOADFM - 1
      CALL GETFM(NEED2)
C
      NLCD = M1 - NCORE
      DO 190 INAT=1,NAT
         IF(LPSKIP(INAT).EQ.1) GO TO 190
         CALL ECPINT(ARRAY,0,X(LDCF1),X(LJLN),X(LLB1),X(LDCF4),X(LDCF2),
     *               X(LJ2N),X(LLB2),X(LFPQR),X(LZLM),X(LLMF),X(LLMX),
     *               X(LLMY),X(LLMZ),L2,INAT)
         IJ = (NLCD*NLCD+NLCD)/2
         DO 180 I = 1,NLMO
            IJ = IJ + 1
            KL = 0
            DO 150 K = 1,L1
               DO 140 L = 1,K
                  KL = KL + 1
                  ONETWO = TWO
                  IF (K.EQ.L) ONETWO = ONE
                  XINT(IJ) = XINT(IJ) + ARRAY(KL) *
     *                         VEC(K,I)*VEC(L,I)*ONETWO
  140          CONTINUE
  150       CONTINUE
            XINT(IJ) = PT5*OCCUP(I)*XINT(IJ)
C           EI(NLCD+1) = EI(NLCD+1) + XINT(IJ)
  180    CONTINUE
         NLCD = NLCD + 1
  190 CONTINUE
C
      CALL RETFM(NEED2)
      RETURN
      END
