C 22 MAY 02 - MWS - PROVIDE ADDITIONAL TRFMCX AND RDCI12 ARGUMENTS
C 25 OCT 01 - JI  - IMPLEMENT FCORE OPTION
C  6 SEP 01 - JI  - MCIBV,MCREF: SMALL CHANGES
C  1 AUG 01 - JI,KR - NEW MODULE FOR JACOBI 2X2 MCSCF ORBITAL UPDATES
C
C*MODULE MCJAC   *DECK JIKRJA
      SUBROUTINE JIKRJA(INITAL,NTROT,ECON)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION METHOD
C
      PARAMETER (MXATM=500, MXRT=100, MXNORO=250)
C
      LOGICAL SOME,GOPARR,DSKWRK,MASWRK,PACK2E,CANONC,FCORE,FORS,NOCI,
     *        EKT,LINSER,INITAL,NOTRF,DOFOCK,DOEXCH,ECON,TESTCON,JACOBI,
     *        DDITRF,DOOOOO,DOVOOO,DOVVOO,DOVOVO
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /DETWFN/ WSTATE(MXRT),SPINS(MXRT),CRIT,PRTTOL,SDET,SZDET,
     *                GRPDET,STSYM,GLIST,
     *                NFLGDM(MXRT),IWTS(MXRT),NCORSV,NCOR,NACT,NORBDT,
     *                NADET,NBDET,KDET,KSTDET,IROOT,IPURES,MAXW1,NITDET,
     *                MAXP,NCIDET,IGPDET,KSTSYM,NFTGCI
      COMMON /ENRGYS/ ENUCR,EELCT,ETOT,SZ,SZZ,ECORE,ESCF,EERD,E1,E2,
     *                VEN,VEE,EPOT,EKIN,ESTATE(MXRT),STATN
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /JACOBI/ JACOBI,NJAOR,ELAST,ISTAT
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /MCINP / METHOD,CISTEP,ACURCY,ENGTOL,DAMP,MICIT,NWORD,NORB,
     *                NOROT(2,MXNORO),MOFRZ(15),NPFLG(10),
     *                NOFO,CANONC,FCORE,FORS,NOCI,EKT,LINSER
      COMMON /NTNOPT/ ENERGY,ENERG0,DEMAX,SQCDF,ITER,MICRO,NOTRF
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      SOME= NPRINT.NE.-5 .AND. MASWRK
      NFTJAC = 23
      NOFRZ  = 15
      ECON = .FALSE.
C
      M1 = NORB
      M2 = (M1*M1+M1)/2
      M4 = (M2*M2+M2)/2
C
      K1 = NACT+NCORSV
      K2 = (K1*K1+K1)/2
      K4 = (K2*K2+K2)/2
C
      L1 = NACT
      L2 = (L1*L1+L1)/2
      L4 = (L2*L2+L2)/2
C
      N2 = NUM*NUM
C
C  READ SYMMETRY LABELS
C
      CALL VALFM(LOADFM)
      LIBO = LOADFM + 1
      LAST = LIBO + M1
      NEED1 = LAST - LOADFM - 1
      CALL GETFM(NEED1)
      CALL DAREAD(IDAF,IODA,X(LIBO),NORB,262,1)
C
C  *** IF THIS IS THE FIRST PASS THEN MUST SET UP ***
C
      IF (INITAL) THEN
C
C   MAKE INTEGRALS OVER ALL NORB ORBITALS
C
         DOFOCK = .FALSE.
         DOEXCH = .FALSE.
         DDITRF = .FALSE.
         DOOOOO = .FALSE.
         DOVOOO = .FALSE.
         DOVVOO = .FALSE.
         DOVOVO = .FALSE.
         CALL TRFMCX(0,0,M1,M1,DOFOCK,DOEXCH,
     *               DDITRF,DOOOOO,DOVOOO,DOVVOO,DOVOVO)
C
C   READ AND WRITE INTEGRALS OVER ALL NORB ORBITALS
C
         ELAST = 0.0D+00
         CALL VALFM(LOADFM)
         LSINT1  = LOADFM  + 1
         LSINT2  = LSINT1 + M2
         LIA     = LSINT2 + M4
         LXX     = LIA    + M2/NWDVAR + 1
         LIXX    = LXX    + NINTMX
         LAST    = LIXX + NINTMX
         NEED2   = LAST - LOADFM - 1
         CALL GETFM(NEED2)
C
         CALL RDCI12(DDITRF,IJKT,X(LSINT1),X(LSINT2),0,M1,M2,M4,
     *               X(LIA),X(LXX),X(LIXX),NINTMX)
         CALL SEQREW(NFTJAC)
         CALL SQWRIT(NFTJAC,X(LSINT1),M2)
         CALL SQWRIT(NFTJAC,X(LSINT2),M4)
         CALL RETFM(NEED2)
C
C    DETERMINE VALID ROTATIONS AND SPIT OUT TO DISK
C
         CALL JACROT(NOJAC,NOROT,MXNORO,MOFRZ,NOFRZ,X(LIBO),NCORSV,
     *               NACT,NORB,FCORE)
         NJAOR = NOJAC*2
         CALL VALFM(LOADFM)
         LIROTS = LOADFM + 1
         LAST  = LIROTS + NJAOR/NWDVAR + 1
         NEED2  = LAST - LOADFM - 1
         CALL GETFM(NEED2)
C
         CALL JACSET(NJAOR,NOROT,MXNORO,MOFRZ,NOFRZ,X(LIBO),NCORSV,
     *               NACT,NORB,X(LIROTS),FCORE)
         CALL SQWINT(NFTJAC,X(LIROTS),NJAOR)
         CALL RETFM(NEED2)
         IF (SOME) WRITE(IW,9000)
         IF (SOME) WRITE(IW,9010) NOJAC
         IF (SOME) WRITE(IW,9020)
      ENDIF
C
      NOJAC = NJAOR/2
C
C        ALLOCATE MEMORY FOR THE JACOBI MCSCF
C
      CALL VALFM(LOADFM)
      LSINT1 = LOADFM + 1
      LSINT2 = LSINT1 + M2
      LXX    = LSINT2 + M4
      LIXX   = LXX    + NINTMX
      LDM1   = LIXX   + NINTMX
      LDM2   = LDM1   + K2
      LIROTS = LDM2   + K4
      LARGX  = LIROTS + NJAOR/NWDVAR + 1
      LARGY  = LARGX  + 65
      LIE1   = LARGY  + 65
      LIE2   = LIE1   + ((M1**3)+(5*M1-3*(M1**2))/2+2*M1-2)/NWDVAR + 1
      LAA    = LIE2   + 13
      LBB    = LAA    + 14
      LCC    = LBB    + 9
      LIE4   = LCC    + 9
      LVEC   = LIE4   + 4
      LAST   = LVEC   + N2
      NEED2  = LAST - LOADFM - 1
      CALL GETFM(NEED2)
C
C   READ CURRENT TRANSFORMATION MATRIX FOR AOS -> MOS
C
      CALL DAREAD(IDAF,IODA,X(LVEC),N2,15,0)
C
C   READ 2ND AND 1ST ORDER DENSITIES OVER ACTIVE ORBITALS.
C
      CALL M2DM2R(NFT15,L1,X(LSINT2),X(LXX),X(LIXX),NINTMX,X(LDM1))
      CALL DAREAD(IDAF,IODA,X(LSINT1),L2,320,0)
C
C   MODIFY THEM TO COOPERATE WITH THIS CODE.
C
      CALL MODDEN(X(LSINT1),X(LSINT2),L1,L2,L4)
C
C   PAD THE DENSITIES OUT TO INCLUDE THE CORE ORBITALS.
C
      CALL DENPAD(X(LSINT1),X(LSINT2),X(LDM1),X(LDM2),NACT,NCORSV,K2,K4)
C
C   READ INTEGRALS OVER ALL NORB ORBITALS.
C
      CALL SEQREW(NFTJAC)
      CALL SQREAD(NFTJAC,X(LSINT1),M2)
      CALL SQREAD(NFTJAC,X(LSINT2),M4)
C
C   READ LIST OF ORBITALS TO BE ROTATED.
C
      CALL SQRINT(NFTJAC,X(LIROTS),NJAOR)
C
C   RETURN THE VALUES OF SIN(THETA), COS(THETA) IN X(LARGX), X(LARGY)
C   FOR THE VALUES OF THETA = -PI -> PI WITH 64 INCREMEMENTS.
C
      CALL GETPI(X(LARGX),X(LARGY))
C
C   DETERMINE CONVERGENCE FACTORS
C
      CALL GETENY(X(LSINT1),X(LSINT2),X(LDM1),X(LDM2),
     *            K2,K4,EMC1,EMC2)
      EMCT = EMC1 + EMC2 + ENUCR
      ECII = ELAST - EMCT
      IF (INITAL) THEN
         ETAU = SQRT(ENGTOL)
      ELSE
         ETAU = ECII/(10.0D+00)
      ENDIF
C
      TESTCON=.FALSE.
      IF (ETAU.LE.ENGTOL) THEN
         ETAU=ENGTOL
         TESTCON=.TRUE.
      ENDIF
C
C     GET INTO THE SWEEPS AND OPTIMIZE ORBITALS
C
      NTROT  = 0
      NSWP   = 0
      ETNUE  = -ETAU/NOJAC
      RNOJAC = NOJAC
      RNOJAC = SQRT(RNOJAC)
      ETSW   = -ETAU/RNOJAC
      ETIM   = 0.0D+00
C
 1300 CALL SWEEP(X(LSINT1),X(LSINT2),X(LDM1),X(LDM2),L1,K1,M1,
     *   ETNUE,X(LIROTS),NJAOR,X(LARGX),X(LARGY),X(LIE1),X(LIE2),
     *   X(LAA),X(LBB),X(LCC),X(LIE4),NROT,X(LVEC),NUM,EIMP)
      NTROT = NTROT + NROT
      NSWP = NSWP + 1
      ETIM = ETIM + EIMP
      IF (EIMP.LE.ETSW) GOTO 1300
C
      CALL GETENY(X(LSINT1),X(LSINT2),X(LDM1),X(LDM2),
     *            K2,K4,EMC1,EMC2)
      ETOT = EMC1+EMC2+ENUCR
C
      IF(MASWRK) WRITE(IW,9030)
     *           ITER,ETOT,-ECII,ETOT-EMCT,NSWP,NTROT
C
      IF (TESTCON) THEN
      IF (ETIM.GT.ETSW.AND.(ELAST-ETOT).LT.ENGTOL.AND.ISTAT.EQ.0) THEN
         ECON=.TRUE.
      ENDIF
      ENDIF
C
      ELAST = ETOT
C
      CALL SEQREW(NFTJAC)
      CALL SQWRIT(NFTJAC,X(LSINT1),M2)
      CALL SQWRIT(NFTJAC,X(LSINT2),M4)
      CALL SQWINT(NFTJAC,X(LIROTS),NJAOR)
      CALL DAWRIT(IDAF,IODA,X(LVEC),N2,15,0)
C
C
      CALL RETFM(NEED2)
      CALL RETFM(NEED1)
      RETURN
C
 9000 FORMAT(/5X,50("-")/
     *       5X,'      AMES LABORATORY JACOBI MCSCF'/
     *       5X,'PROGRAM WRITTEN BY JOE IVANIC AND KLAUS RUEDENBERG'/
     *       5X,50(1H-))
 9010 FORMAT(/1X,'NUMBER OF ANGLES IN A SINGLE SWEEP = ',I6)
 9020 FORMAT(/1X,'ITER',4X,'TOTAL ENERGY',7X,'DEL(E)CI',
     *       8X,'DEL(E)ORB',4X,'SWEEPS',4X,'NOROTS')
 9030 FORMAT(1X,I3,3X,F15.9,2X,F15.9,2X,F13.9,4X,I3,6X,I5)
C
      END
C
C*MODULE MCJAC   *DECK JACROT
C
      SUBROUTINE JACROT(NOJAC,NOROT,MXNORO,MOFRZ,NOFRZ,IBO,NCOR,
     *               NACT,NORB,FCORE)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL FCORE
      DIMENSION NOROT(2,MXNORO),MOFRZ(NOFRZ),IBO(NORB)
C
      NOJAC = 0
C
      IF (FCORE) GOTO 37
C
      DO 20 II=1,NCOR
         ISYM1 = IBO(II)
         DO KK=1,NOFRZ
            IF (II.EQ.MOFRZ(KK)) GOTO 20
         ENDDO
         DO 10 JJ=NORB,NCOR+1,-1
            ISYM2 = IBO(JJ)
            IF (ISYM1.NE.ISYM2) GOTO 10
            DO KK=1,MXNORO
               I1 = NOROT(2,KK)
               J1 = NOROT(1,KK)
               IF (I1.EQ.II.AND.J1.EQ.JJ) GOTO 10
            ENDDO
            DO KK=1,NOFRZ
               IF (JJ.EQ.MOFRZ(KK)) GOTO 10
            ENDDO
            NOJAC = NOJAC + 1
   10    CONTINUE
   20 CONTINUE
C
   37 CONTINUE
C
      DO 40 II=NCOR+1,NACT+NCOR
         ISYM1 = IBO(II)
         DO KK=1,NOFRZ
            IF (II.EQ.MOFRZ(KK)) GOTO 40
         ENDDO
         DO 30 JJ=NORB,II+1,-1
            ISYM2 = IBO(JJ)
            IF (ISYM1.NE.ISYM2) GOTO 30
            DO KK=1,MXNORO
               I1 = NOROT(2,KK)
               J1 = NOROT(1,KK)
               IF (I1.EQ.II.AND.J1.EQ.JJ) GOTO 30
            ENDDO
            DO KK=1,NOFRZ
               IF (JJ.EQ.MOFRZ(KK)) GOTO 30
            ENDDO
            NOJAC = NOJAC + 1
   30    CONTINUE
   40 CONTINUE
C
      RETURN
      END
C
C*MODULE MCJAC   *DECK JACSET
C 
      SUBROUTINE JACSET(NJAOR,NOROT,MXNORO,MOFRZ,NOFRZ,IBO,NCOR,
     *               NACT,NORB,IROT,FCORE)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL FCORE
      DIMENSION NOROT(2,MXNORO),MOFRZ(NOFRZ),IBO(NORB)
      DIMENSION IROT(NJAOR)
C
      NORIND = 0
      IF (FCORE) GOTO 37
C
      DO 20 II=1,NCOR
         ISYM1 = IBO(II)
         DO KK=1,NOFRZ
            IF (II.EQ.MOFRZ(KK)) GOTO 20
         ENDDO
         DO 10 JJ=NORB,NCOR+1,-1
            ISYM2 = IBO(JJ)
            IF (ISYM1.NE.ISYM2) GOTO 10
            DO KK=1,MXNORO
               I1 = NOROT(2,KK)
               J1 = NOROT(1,KK)
               IF (I1.EQ.II.AND.J1.EQ.JJ) GOTO 10
            ENDDO
            DO KK=1,NOFRZ
               IF (JJ.EQ.MOFRZ(KK)) GOTO 10
            ENDDO
            IROT(NORIND+1) = II
            IROT(NORIND+2) = JJ
            NORIND = NORIND + 2
   10    CONTINUE
   20 CONTINUE
C
   37 CONTINUE
C 
      DO 40 II=NCOR+1,NACT+NCOR
         ISYM1 = IBO(II)
         DO KK=1,NOFRZ
            IF (II.EQ.MOFRZ(KK)) GOTO 40
         ENDDO
         DO 30 JJ=NORB,II+1,-1
            ISYM2 = IBO(JJ)
            IF (ISYM1.NE.ISYM2) GOTO 30
            DO KK=1,MXNORO
               I1 = NOROT(2,KK)
               J1 = NOROT(1,KK)
               IF (I1.EQ.II.AND.J1.EQ.JJ) GOTO 30
            ENDDO
            DO KK=1,NOFRZ
               IF (JJ.EQ.MOFRZ(KK)) GOTO 30
            ENDDO
            IROT(NORIND+1) = II
            IROT(NORIND+2) = JJ
            NORIND = NORIND + 2
   30    CONTINUE
   40 CONTINUE
C
      RETURN
      END
C
C*MODULE MCJAC   *DECK MODDEN
      SUBROUTINE MODDEN(DM1,DM2,L1,L2,L4)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (TWO = 2.0D+00+0)
      LOGICAL IEQJ,KEQL
      DIMENSION DM1(L2),DM2(L4)
C
      IJ = 0
      DO 20 II=1,L1
         DO 10 JJ=1,II-1
            IJ = IJ + 1
            DM1(IJ) = DM1(IJ)+DM1(IJ)
   10    CONTINUE
         IJ = IJ + 1
   20 CONTINUE
C
      IJKL = 0
      DO 100 I=1,L1
         DO 80 J=1,I
           IEQJ = I.EQ.J
           DO 70 K=1,I
              LMAX=K
              IF (K.EQ.I) LMAX=J
              DO 60 L=1,LMAX
                 KEQL = K.EQ.L
                 IJKL = IJKL + 1
                 IF (I.EQ.K.AND.J.EQ.L) DM2(IJKL) = DM2(IJKL)/TWO
                 IF (KEQL) DM2(IJKL) = DM2(IJKL)/TWO
                 IF (IEQJ) DM2(IJKL) = DM2(IJKL)/TWO
   60         CONTINUE
   70      CONTINUE
   80   CONTINUE
  100 CONTINUE
C
      CALL DSCAL(L4,4.0D+00,DM2,1)
C
      RETURN
      END
C
C*MODULE MCJAC   *DECK DENPAD
      SUBROUTINE DENPAD(S1,S2,D1,D2,NACT,NCOR,K2,K4)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (TWO = 2.00D+00, FOUR = 4.00D+00)
      PARAMETER (ONE = 1.00D+00, ZERO = 0.00D+00)
      DIMENSION S1(K2),S2(K4),D1(K2),D2(K4)
C
C  MODIFY 1E DENSITY MATRIX
C
      NORB = NACT + NCOR
      DO 45 II=1,NCOR
         IPO = (II*(II-1))/2
         DO 55 JJ=1,II-1
            D1(IPO+JJ) = ZERO
   55    CONTINUE
         D1(IPO+II) = TWO
   45 CONTINUE
C
      DO 66 II=1,NACT
         N1 = (II*(II-1))/2
         NO = II + NCOR
         IPO = (NO*(NO-1))/2
         DO 68 JJ=1,NCOR
            D1(IPO+JJ) = ZERO
   68    CONTINUE
         DO 70 JJ=NCOR+1,NORB
            D1(IPO+JJ) = S1(N1+JJ-NCOR)
   70    CONTINUE
   66 CONTINUE
C
C  MODIFY 2E DENSITY MATRIX
C
      IC1 = (NORB*(NORB+1))/2
      IC2 = (IC1*(IC1+1))/2
      DO 72 II=1,IC2
         D2(II) = ZERO
   72 CONTINUE
C
      DO 77 II=1,NCOR
         IP1 = (II*(II+1))/2
         IP2 = (IP1*(IP1-1))/2
         IP3 = IP1 - II
         D2(IP2+IP1) = ONE
         DO 88 JJ=1,II-1
            IQ1 = (JJ*(JJ+1))/2
            D2(IP2+IQ1) = FOUR
            IQ2 = IP3 + JJ
            IQ3 = (IQ2*(IQ2+1))/2
            D2(IQ3) = -TWO
   88    CONTINUE
   77 CONTINUE
C
      DO 113 II=NCOR+1,NORB
         I1 = (II*(II-1))/2
         IT1 = I1 + II
         TYP = D1(IT1)
         DO 115 KK=1,NCOR
            K1 = (KK*(KK-1))/2
            KT1 = K1 + KK
            JTT = (IT1*(IT1-1))/2 + KT1
            D2(JTT) = TWO*TYP
            J2 = I1 + KK
            J3 = (J2*(J2+1))/2
            D2(J3) = -TYP
  115    CONTINUE
         DO 118 JJ=NCOR+1,II
            K1 = I1 + JJ
            K3 = (JJ*(JJ-1))/2
            TYP = D1(K1)
            DO 130 KK=1,NCOR
               K2 = (KK*(KK-1))/2
               KT1 = K2 + KK
               KT2 = I1 + KK
               KT5 = K3 + KK
               IP1 = (K1*(K1-1))/2 + KT1
               IP2 = (KT2*(KT2-1))/2 + KT5
               D2(IP1) = TWO * TYP
               D2(IP2) = -TYP
  130       CONTINUE
  118    CONTINUE
  113 CONTINUE
C
C NOW FOR JUST THE ACTIVE SET
C
      J2 = 0
      DO 340 I=NCOR+1,NORB
         DO 350 J=NCOR+1,I
               I1 = (I*(I-1))/2 + J
            DO 360 K=NCOR+1,I-1
               DO 370 LL=NCOR+1,K
                  K2 = (K*(K-1))/2 + LL
                   J1 = (I1*(I1-1))/2 + K2
                  J2 = J2 + 1
                    D2(J1) = S2(J2)
  370          CONTINUE
  360       CONTINUE
            DO 380 LL=NCOR+1,J
               K2 = (I*(I-1))/2 + LL
               J1 = (I1*(I1-1))/2 + K2
               J2 = J2 + 1
               D2(J1) = S2(J2)
  380       CONTINUE
  350    CONTINUE
  340 CONTINUE
C
      RETURN
      END
C
C*MODULE MCJAC   *DECK GETPI
      SUBROUTINE GETPI(ARGX,ARGY)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION ARGX(65),ARGY(65)
C
C   RETURN THE VALUES OF SIN(THETA), COS(THETA) IN
C   ARGX, ARGY FOR THE VALUES OF THETA
C   -PI -> PI WITH 64 INCREMEMENTS.
C
      PI=3.141592653589793238462643383279502884197169399D+00
      ARGX(33) = 0.0D+00
      ARGY(33) = 1.0D+00
      ARGX(1) = 0.0D+00
      ARGY(1) = -1.0D+00
      ARGX(65) = 0.0D+00
      ARGY(65) = -1.0D+00
      DO 13 II=1,31
         GAMMA = (II/32.0D+00)*PI
         ARGX(II+33) = SIN(GAMMA)
         ARGY(II+33) = COS(GAMMA)
         ARGX(33-II) = -ARGX(II+33)
         ARGY(33-II) = ARGY(II+33)
   13 CONTINUE
      RETURN
      END
C
C*MODULE MCJAC   *DECK SWEEP
      SUBROUTINE SWEEP(SINT1,SINT2,DM1,DM2,L1,K1,M1,
     *      ETNUE,IROT,NJAOR,ARGX,ARGY,IE1,IE2,A,B,C,IE4,NROT,
     *      MO,NUM,EIMP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL FOUND
      DIMENSION IROT(NJAOR),IE1(*),IE2(*),IE4(4)
      DIMENSION SINT1(*),SINT2(*),DM1(*),DM2(*)
      DIMENSION ARGX(65),ARGY(65),A(14),B(9),C(9)
      DIMENSION MO(NUM*NUM)
C
      NC = K1-L1
      NROT = 0
      NIND = 0
      NANG = NJAOR/2
      EIMP = 0.0D+00
      DO 1000 IH=1,NANG
         IK = IROT(NIND+1)
         IJ = IROT(NIND+2)
         IF (IJ.LE.K1) THEN
         CALL MCINDAA(IE1,IK,IJ,K1,M1,IE2)
         CALL MCREAAA(SINT1,SINT2,DM1,DM2,A,B,C,IE1,IE2)
         ELSE
         CALL MCINDAV(IE1,IK,IJ,K1,M1,IE2)
         CALL MCREAAV(SINT1,SINT2,DM1,DM2,A,B,C,IE1,IE2)
         ENDIF
         NIND = NIND + 2
         CALL MCIBV(NC,IK,A,B,C,ARGX,ARGY,IE4,ING,ING2,DE,FOUND)
         IF ( .NOT. FOUND ) GOTO 1000
         CALL MCREF(B,C,ARGX(ING),ARGX(ING2),X,Y,DE)
         IF (DE.GT.ETNUE) GOTO 1000
         EIMP = EIMP + DE
         NROT = NROT + 1
         CALL MCROTAA(SINT1,SINT2,IE1,IE2,X,Y)
         CALL ADJTR(IK,IJ,NUM,Y,X,MO)
 1000 CONTINUE
C
      RETURN
      END
C
C*MODULE MCJAC   *DECK MCINDAA
      SUBROUTINE MCINDAA(INDX,I,J,NA,NT,INZ)
      IMPLICIT INTEGER(A-Z)
      DIMENSION INDX(*),INZ(*)
C
C  I'M NOT GOING TO EXPLAIN WHAT THIS DOES JUST YET.
C
      INDZ = 0
      NN = (NA*(NA+1))/2
C
      II = (I*(I+1))/2
      I1 = II - I
      IIII = (II*(II-1))/2
      JJ = (J*(J+1))/2
      JJJJ = (JJ*(JJ-1))/2
      J1 = JJ - J
      JI = J1 + I
      JI00 = (JI*(JI-1))/2
C
C  FIRST THE 1E INDICES
C  HII,HJI,HJJ
C
      INDX(1) = II
      INDX(2) = JI
      INDX(3) = JJ
      INDZ = 3
C
C  NOW FOR ALL RELEVANT P
C
      DO 13 P=1,I-1
         INDX(INDZ+1) = I1 + P
         INDX(INDZ+2) = J1 + P
         INDZ = INDZ + 2
   13 CONTINUE
      P1 = II + I
      DO 15 P=I+1,J-1
         INDX(INDZ+1) = P1
         INDX(INDZ+2) = J1 + P
         INDZ = INDZ + 2
         P1 = P1 + P
   15 CONTINUE
      P1 = JJ
      DO 17 P=J+1,NA
         INDX(INDZ+1) = P1 + I
         INDX(INDZ+2) = P1 + J
         INDZ = INDZ + 2
         P1 = P1 + P
   17 CONTINUE
C
      INZ(1) = INDZ
C
C  END OF THE 1E INDICES, NOW FOR THE 2E INDICES.
C
C  [PQ|IR], [PQ|JR] ALL RELEVANT P,Q,R IE WHERE I,J
C  OCCURS IN POSITION SPECIFIED AND INTEGRAL CORRESPONDS
C  TO REVERSE CANONICAL ORDER.
C
      IR = I1 + 1
      JR = J1 + 1
      IRS = (IR*(IR-1))/2
      JRS = (JR*(JR-1))/2
      DO 21 R=1,I-1
         DO 24 PQ=1,I1
            INDX(INDZ+1) = IRS+PQ
            INDX(INDZ+2) = JRS+PQ
            INDZ = INDZ + 2
   24    CONTINUE
         IRS = IRS + IR
         JRS = JRS + JR
         IR = IR + 1
         JR = JR + 1
   21 CONTINUE
C
C  [PQ|RI], [PQ|JR]
C
      JR = J1+I+1
      JRI = (JR*(JR-1))/2
      RI = II+I
      RII = (RI*(RI-1))/2
      DO 25 R=I+1,J-1
         RII = (RI*(RI-1))/2
         PQ = 1
         DO 27 P=1,R-1
            IF (P.EQ.I) GOTO 23
            DO 29 Q=1,P
               IF (Q.EQ.I) GOTO 28
               INDX(INDZ+1) = RII + PQ
               INDX(INDZ+2) = JRI + PQ
               INDZ = INDZ + 2
   28          PQ = PQ + 1
   29       CONTINUE
            GOTO 27
   23       PQ = PQ + P
   27    CONTINUE
         DO 22 Q=1,I-1
             INDX(INDZ+1) = RII + PQ
            INDX(INDZ+2) = JRI + PQ
            INDZ = INDZ + 2
            PQ = PQ + 1
   22    CONTINUE
         JRI = JRI + JR
         JR = JR + 1
         RI = RI + R
   25 CONTINUE
C
C  [PQ|RI],[PQ|RJ]
C
      RI = JJ + I
      RJ = JJ + J
      DO 35 R=J+1,NA
         RII = (RI*(RI-1))/2
         RJI = (RJ*(RJ-1))/2
         PQ = 1
         DO 37 P=1,R-1
            IF (P.EQ.I.OR.P.EQ.J) GOTO 39
            DO 34 Q=1,P
               IF (Q.EQ.I.OR.Q.EQ.J) GOTO 31
               INDX(INDZ+1) = RII + PQ
               INDX(INDZ+2) = RJI + PQ
               INDZ = INDZ + 2
   31          PQ = PQ + 1
   34       CONTINUE
            GOTO 37
   39       PQ = PQ + P
   37    CONTINUE
         DO 32 Q=1,I-1
            INDX(INDZ+1) = RII + PQ
            INDX(INDZ+2) = RJI + PQ
            INDZ = INDZ + 2
            PQ = PQ + 1
   32    CONTINUE
         RI = RI + R
         RJ = RJ + R
   35 CONTINUE
C
C  [IR|PQ], [PQ|JR]
C
      PQ = II + 1
      PQI = (PQ*(PQ-1))/2 + I1
      IX = J1+1
      IX2 = (IX*(IX-1))/2
      DO 40 P=I+1,J-1
         DO 41 Q=1,P
            IF (Q.EQ.I) GOTO 43
            JR = IX
            JRI = IX2
            DO 45 R=1,I-1
               INDX(INDZ+1) = PQI + R
               INDX(INDZ+2) = JRI + PQ
               INDZ = INDZ + 2
               JRI = JRI + JR
               JR = JR + 1
   45       CONTINUE
   43       PQI = PQI + PQ
            PQ = PQ + 1
   41    CONTINUE
   40 CONTINUE
C
C  [IR|PQ],[JR|PQ]
C
      PQ = JJ+1
      PQI = (PQ*(PQ-1))/2 + I1
      PQJ = PQI - I1 + J1
      DO 50 P=J+1,NA
         DO 51 Q=1,P
            IF (Q.EQ.I.OR.Q.EQ.J) GOTO 53
            DO 55 R=1,I-1
               INDX(INDZ+1) = PQI + R
               INDX(INDZ+2) = PQJ + R
               INDZ = INDZ + 2
   55       CONTINUE
   53       PQI = PQI + PQ
            PQJ = PQJ + PQ
            PQ = PQ + 1
   51    CONTINUE
   50 CONTINUE
C
C  [RI|PQ],[JR|PQ]
C
      RI = II + I
      JR = J1 + I+1
      IX = JJ+1
      IX2 = (IX*(IX-1))/2
      DO 62 R=I+1,J-1
         PQ = IX
         PQI = IX2
         DO 64 P=J+1,NA
            DO 65 Q=1,P
               IF (Q.EQ.I.OR.Q.EQ.J) GOTO 67
               INDX(INDZ+1) = PQI + RI
               INDX(INDZ+2) = PQI + JR
               INDZ = INDZ + 2
   67          PQI = PQI + PQ
               PQ = PQ + 1
   65       CONTINUE
   64    CONTINUE
         RI = RI + R
         JR = JR + 1
   62 CONTINUE
C
C   [RI|PQ],[RJ|PQ]
C
C
C
      RJ = JJ+J
      RI = JJ+I
      DO 70 R=J+1,NA
         PQ = (R*(R-1))/2+J+1
         PQI = (PQ*(PQ-1))/2 + RI
         PQJ = PQI - RI + RJ
         DO 72 Q=J+1,R
            INDX(INDZ+1) = PQI
            INDX(INDZ+2) = PQJ
            INDZ = INDZ + 2
            PQI = PQI + PQ
            PQJ = PQJ + PQ
            PQ = PQ + 1
   72    CONTINUE
         DO 75 P=R+1,NA
            DO 77 Q=1,P
               IF (Q.EQ.I.OR.Q.EQ.J) GOTO 79
               INDX(INDZ+1) = PQI
               INDX(INDZ+2) = PQJ
               INDZ = INDZ + 2
   79          PQI = PQI + PQ
               PQJ = PQJ + PQ
               PQ = PQ + 1
   77       CONTINUE
   75    CONTINUE
         RJ = RJ + R
         RI = RI + R
   70 CONTINUE
C
C   [RI|PQ],[PQ|RJ]
C
      RI = JJ+I
      RJ = JJ+J
      DO 80 R=J+1,NA
         RJI = (RJ*(RJ-1))/2
         PQ = RI+1
         PQI = (PQ*(PQ-1))/2 +RI
         DO 83 Q=I+1,J-1
            INDX(INDZ+1) = PQI
            INDX(INDZ+2) = RJI + PQ
            INDZ = INDZ + 2
            PQI = PQI + PQ
            PQ = PQ + 1
   83    CONTINUE
         RJ = RJ + R
         RI = RI + R
   80 CONTINUE
C
C   [RI|PQ],[PQ|JR]
C
      RI = II+I
      JR = J1+I+1
      JRI=(JR*(JR-1))/2
      DO 90 R=I+1,J-1
         PQ = RI+1
         PQI = (PQ*(PQ-1))/2 + RI
         DO 92 Q=I+1,R
            INDX(INDZ+1) = PQI
            INDX(INDZ+2) = JRI+PQ
            INDZ = INDZ + 2
            PQI = PQI + PQ
            PQ = PQ + 1
   92    CONTINUE
         DO 94 P=R+1,J-1
            DO 96 Q=1,P
               IF (Q.EQ.I) GOTO 97
               INDX(INDZ+1) = PQI
               INDX(INDZ+2) = JRI+PQ
               INDZ = INDZ + 2
   97          PQI = PQI + PQ
               PQ = PQ + 1
   96       CONTINUE
   94    CONTINUE
         RI = RI + R
         JRI = JRI + JR
         JR = JR + 1
   90 CONTINUE
C
      INZ(2) = INDZ
C
C   [IQ|IP],[IQ|JP],[IP|JQ],[JQ|JP]
C
      IP = I1+1
      IPI = (IP*(IP-1))/2
      JP = J1+1
      JPI = (JP*(JP-1))/2
      IX = JPI
      DO 100 P=1,I-1
         JQI = IX + IP
         JQ = J1 + 1
         IQ = I1 + 1
         DO 105 Q=1,P-1
            INDX(INDZ+1) = IPI+IQ
            INDX(INDZ+2) = JPI+IQ
            INDX(INDZ+3) = JQI
            INDX(INDZ+4) = JPI+JQ
            INDZ = INDZ + 4
            IQ = IQ+1
            JQI = JQI+JQ
            JQ = JQ + 1
  105    CONTINUE
         IPI = IPI + IP
         IP = IP + 1
         JPI = JPI + JP
         JP = JP + 1
  100 CONTINUE
C
      INZ(3) = INDZ
C
C   [IQ|PI],[IQ|JP],[PI|JQ],[JQ|JP]
C
      PI = II+I
      JP = J1+I + 1
      JPI = (JP*(JP-1))/2
      IX = J1+1
      JX = (IX*(IX-1))/2
      DO 110 P=I+1,J-1
         PII = (PI*(PI-1))/2 +I1+1
         JQ = J1+1
         JQI = JX + PI
         IQ = I1+1
         DO 115 Q=1,I-1
            INDX(INDZ+1) = PII
            INDX(INDZ+2) = JPI + IQ
            INDX(INDZ+3) = JQI
            INDX(INDZ+4) = JPI + JQ
            INDZ = INDZ + 4
            JQI = JQI + JQ
            JQ = JQ + 1
            IQ = IQ + 1
            PII = PII + 1
  115    CONTINUE
         PI = PI + P
         JPI = JPI + JP
         JP = JP + 1
  110 CONTINUE
C
C   [IQ|PI],[IQ|PJ],[JQ|PI],[JQ|PJ]
C
      PI = JJ+I
      PJ = JJ+J
      DO 120 P=J+1,NA
         PJI = (PJ*(PJ-1))/2
         PII = (PI*(PI-1))/2
         JQ = J1 + 1
         IQ = I1 + 1
         DO 125 Q=1,I-1
            INDX(INDZ+1) = PII + IQ
            INDX(INDZ+2) = PJI + IQ
            INDX(INDZ+3) = PII + JQ
            INDX(INDZ+4) = PJI + JQ
            INDZ = INDZ + 4
            JQ = JQ + 1
            IQ = IQ + 1
  125    CONTINUE
         PI = PI + P
         PJ = PJ + P
  120 CONTINUE
C
C   [QI|PI],[QI|JP],[PI|JQ],[JQ|JP]
C
      JP = J1+I+1
      IX = JP
      JPI = (JP*(JP-1))/2
      IX2 = JPI
      PI = II + I
      DO 130 P=I+1,J-1
         PII = (PI*(PI-1))/2
         JQ = IX
         JQI = IX2
         QI = II+I
         DO 135 Q=I+1,P-1
            INDX(INDZ+1) = PII + QI
            INDX(INDZ+2) = JPI + QI
            INDX(INDZ+3) = JQI + PI
            INDX(INDZ+4) = JPI + JQ
            INDZ = INDZ + 4
            JQI = JQI + JQ
            JQ = JQ + 1
            QI = QI + Q
  135    CONTINUE
         PI = PI + P
         JPI = JPI + JP
         JP = JP + 1
  130 CONTINUE
C
C   [QI|PI],[QI|PJ],[JQ|PI],[JQ|PJ]
C
      PI = JJ + I
      PJ = JJ + J
      DO 140 P=J+1,NA
         PII = (PI*(PI-1))/2
         PJI = (PJ*(PJ-1))/2
         QI = II + I
         JQ = J1 + I + 1
         DO 143 Q=I+1,J-1
            INDX(INDZ+1) = PII + QI
            INDX(INDZ+2) = PJI + QI
            INDX(INDZ+3) = PII + JQ
            INDX(INDZ+4) = PJI + JQ
            INDZ = INDZ + 4
            QI = QI + Q
            JQ = JQ + 1
  143    CONTINUE
         PI = PI + P
         PJ = PJ + P
  140 CONTINUE
C
C   [QI|PI],[QI|PJ],[QJ|PI],[QJ|PJ]
C
      PI = JJ + I
      PJ = JJ + J
      DO 150 P=J+1,NA
         PII = (PI*(PI-1))/2
         PJI = (PJ*(PJ-1))/2
         QI = JJ + I
         QJ = JJ + J
         DO 153 Q=J+1,P-1
            INDX(INDZ+1) = PII + QI
            INDX(INDZ+2) = PJI + QI
            INDX(INDZ+3) = PII + QJ
            INDX(INDZ+4) = PJI + QJ
            INDZ = INDZ + 4
            QI = QI + Q
            QJ = QJ + Q
  153    CONTINUE
         PI = PI + P
         PJ = PJ + P
  150 CONTINUE
C
      INZ(3) = INDZ
C
C   [IP|IP],[IP|JP],[JP|JP]
C
      IP = I1 + 1
      JP = J1 + 1
      JPI = (JP*(JP-1))/2
      IPI = (IP*(IP-1))/2
      DO 160 P=1,I-1
         INDX(INDZ+1) = IPI + IP
         INDX(INDZ+2) = JPI + IP
         INDX(INDZ+3) = JPI + JP
         INDZ = INDZ + 3
         IPI = IPI + IP
         JPI = JPI + JP
         IP = IP + 1
         JP = JP + 1
  160 CONTINUE
C
C   [PI|PI],[PI|JP],[JP|JP]
C
      PI = II + I
      JP = J1 + I + 1
      JPI = (JP*(JP-1))/2
      DO 170 P=I+1,J-1
         PII = (PI*(PI-1))/2
         INDX(INDZ+1) = PII + PI
         INDX(INDZ+2) = JPI + PI
         INDX(INDZ+3) = JPI + JP
         INDZ = INDZ + 3
         JPI = JPI + JP
         PI = PI + P
         JP = JP + 1
  170 CONTINUE
C
C   [PI|PI],[PI|PJ],[PJ|PJ]
C
      PI = JJ + I
      PJ = JJ + J
      DO 180 P=J+1,NA
         PII = (PI*(PI-1))/2
         PJI = (PJ*(PJ-1))/2
         INDX(INDZ+1) = PII + PI
         INDX(INDZ+2) = PJI + PI
         INDX(INDZ+3) = PJI + PJ
         INDZ = INDZ + 3
         PI = PI + P
         PJ = PJ + P
  180 CONTINUE
C
      INZ(4) = INDZ
C
C   [PQ|II],[PQ|JI],[PQ|JJ]
C
      DO 190 PQ=1,I1
         INDX(INDZ+1) = IIII+PQ
         INDX(INDZ+2) = JI00+PQ
         INDX(INDZ+3) = JJJJ+PQ
         INDZ = INDZ + 3
  190 CONTINUE
C
C    [II|PQ],[PQ|JI],[PQ|JJ]
C
      PQ = II + 1
      PQI = (PQ*(PQ-1))/2
      DO 200 P=I+1,J-1
         DO 210 Q=1,P
            IF (Q.EQ.I) GOTO 220
            INDX(INDZ+1) = PQI + II
            INDX(INDZ+2) = JI00 + PQ
            INDX(INDZ+3) = JJJJ+PQ
            INDZ = INDZ + 3
  220       PQI = PQI + PQ
            PQ = PQ + 1
  210    CONTINUE
  200 CONTINUE
C
C    [II|PQ],[JI|PQ],[JJ|PQ]
C
      PQ = JJ + 1
      PQI = (PQ*(PQ-1))/2
      DO 230 P=J+1,NA
         DO 233 Q=1,P
            IF (Q.EQ.I.OR.Q.EQ.J) GOTO 235
            INDX(INDZ+1) = PQI + II
            INDX(INDZ+2) = PQI + JI
            INDX(INDZ+3) = PQI + JJ
            INDZ = INDZ + 3
  235       PQI = PQI + PQ
            PQ = PQ + 1
  233    CONTINUE
  230 CONTINUE
C
      INZ(5) = INDZ
C
C   [IP|II],[IP|JI],[IP|JJ],[II|JP],[JP|JI],[JP|JJ]
C
      IP = I1 + 1
      JP = J1 + 1
      JPI = (JP*(JP-1))/2
      DO 240 P=1,I-1
         INDX(INDZ+1) = IIII + IP
         INDX(INDZ+2) = JI00 + IP
         INDX(INDZ+3) = JJJJ + IP
         INDX(INDZ+4) = JPI + II
         INDX(INDZ+5) = JI00 + JP
         INDX(INDZ+6) = JJJJ + JP
         INDZ = INDZ + 6
         JPI = JPI + JP
         JP = JP + 1
         IP = IP + 1
  240 CONTINUE
C
C   [II|PI],[PI|JI],[PI|JJ],[II|JP],[JI|JP],[JP|JJ]
C
      PI = II + I
      JP = J1 + I + 1
      JPI = (JP*(JP-1))/2
      DO 250 P=I+1,J-1
         PII = (PI*(PI-1))/2
         INDX(INDZ+1) = PII + II
         INDX(INDZ+2) = JI00 + PI
         INDX(INDZ+3) = JJJJ + PI
         INDX(INDZ+4) = JPI + II
         INDX(INDZ+5) = JPI + JI
         INDX(INDZ+6) = JJJJ + JP
         INDZ = INDZ + 6
         PI = PI + P
         JPI = JPI + JP
         JP = JP + 1
  250 CONTINUE
C
C   [II|PI],[JI|PI],[JJ|PI],[II|PJ],[JI|PJ],[JJ|PJ]
C
      PI = JJ + I
      PJ = JJ + J
      DO 260 P=J+1,NA
         PII = (PI*(PI-1))/2
         PJI = (PJ*(PJ-1))/2
         INDX(INDZ+1) = PII + II
         INDX(INDZ+2) = PII + JI
         INDX(INDZ+3) = PII + JJ
         INDX(INDZ+4) = PJI + II
         INDX(INDZ+5) = PJI + JI
         INDX(INDZ+6) = PJI + JJ
         INDZ = INDZ + 6
         PI = PI + P
         PJ = PJ + P
  260 CONTINUE
C
      INZ(6) = INDZ
C
      INDX(INDZ+1) = IIII + II
      INDX(INDZ+2) = JI00 + II
      INDX(INDZ+3) = JJJJ + II
      INDX(INDZ+4) = JI00 + JI
      INDX(INDZ+5) = JJJJ + JI
      INDX(INDZ+6) = JJJJ + JJ
      INDZ = INDZ + 6
C
      INZ(7) = INDZ
C
C     NOW FOR ALL INTEGRALS INVOLVING VIRTUAL INDICES.
C
      P1 = NN
      DO 917 P=NA+1,NT
         INDX(INDZ+1) = P1 + I
         INDX(INDZ+2) = P1 + J
         INDZ = INDZ + 2
         P1 = P1 + P
  917 CONTINUE
C
      INZ(8) = INDZ
C
      RI = NN + I
      RJ = NN + J
      DO 635 R=NA+1,NT
         RII = (RI*(RI-1))/2
         RJI = (RJ*(RJ-1))/2
         PQ = 1
         DO 637 P=1,R-1
            IF (P.EQ.I.OR.P.EQ.J) GOTO 639
            DO 634 Q=1,P
               IF (Q.EQ.I.OR.Q.EQ.J) GOTO 631
               INDX(INDZ+1) = RII + PQ
               INDX(INDZ+2) = RJI + PQ
               INDZ = INDZ + 2
  631          PQ = PQ + 1
  634       CONTINUE
            GOTO 637
  639       PQ = PQ + P
  637    CONTINUE
         DO 632 Q=1,I-1
            INDX(INDZ+1) = RII + PQ
            INDX(INDZ+2) = RJI + PQ
            INDZ = INDZ + 2
            PQ = PQ + 1
  632    CONTINUE
         RI = RI + R
         RJ = RJ + R
  635 CONTINUE
C
      PQ = NN + 1
      PQI = (PQ*(PQ-1))/2 + I1
      PQJ = PQI - I1 + J1
      DO 650 P=NA+1,NT
         DO 651 Q=1,P
            IF (Q.EQ.I.OR.Q.EQ.J) GOTO 653
            DO 655 R=1,I-1
               INDX(INDZ+1) = PQI + R
               INDX(INDZ+2) = PQJ + R
               INDZ = INDZ + 2
  655       CONTINUE
  653       PQI = PQI + PQ
            PQJ = PQJ + PQ
            PQ = PQ + 1
  651    CONTINUE
  650 CONTINUE
C
      RI = II + I
      JR = J1 + I+1
      IX = NN + 1
      IX2 = (IX*(IX-1))/2
      DO 662 R=I+1,J-1
         PQ = IX
         PQI = IX2
         DO 664 P=NA+1,NT
            DO 665 Q=1,P
               IF (Q.EQ.I.OR.Q.EQ.J) GOTO 667
               INDX(INDZ+1) = PQI + RI
               INDX(INDZ+2) = PQI + JR
               INDZ = INDZ + 2
  667          PQI = PQI + PQ
               PQ = PQ + 1
  665       CONTINUE
  664    CONTINUE
         RI = RI + R
         JR = JR + 1
  662 CONTINUE
C
      RJ = JJ + J
      RI = JJ + I
      NN1 = NN+1
      NN2 = (NN1*(NN1-1))/2
      DO 670 R=J+1,NA
         PQ = NN1
C        PQ = NN+1
CJOE
         PQI = NN2+RI
C        PQI = (PQ*(PQ-1))/2 + RI
         PQJ = PQI - RI + RJ
         DO 675 P=NA+1,NT
            DO 677 Q=1,P
               IF (Q.EQ.I.OR.Q.EQ.J) GOTO 679
               INDX(INDZ+1) = PQI
               INDX(INDZ+2) = PQJ
               INDZ = INDZ + 2
  679          PQI = PQI + PQ
               PQJ = PQJ + PQ
               PQ = PQ + 1
  677       CONTINUE
  675    CONTINUE
         RJ = RJ + R
         RI = RI + R
  670 CONTINUE
C
      RJ = NN + J
      RI = NN + I
C
      DO 770 R=NA+1,NT
         PQ = RJ+1
C        PQ = (R*(R-1))/2 + J+1
CJOE
        PQI = (PQ*(PQ-1))/2 + RI
         PQJ = PQI - RI + RJ
         DO 772 Q=J+1,R
            INDX(INDZ+1) = PQI
            INDX(INDZ+2) = PQJ
            INDZ = INDZ + 2
            PQI = PQI + PQ
            PQJ = PQJ + PQ
            PQ = PQ + 1
  772    CONTINUE
         DO 775 P=R+1,NT
            DO 777 Q=1,P
               IF (Q.EQ.I.OR.Q.EQ.J) GOTO 779
               INDX(INDZ+1) = PQI
               INDX(INDZ+2) = PQJ
               INDZ = INDZ + 2
  779          PQI = PQI + PQ
               PQJ = PQJ + PQ
               PQ = PQ + 1
  777       CONTINUE
  775    CONTINUE
         RI = RI + R
         RJ = RJ + R
CJOE
C        PQ = PQ + R
  770 CONTINUE
C
      RI = NN + I
      RJ = NN + J
      DO 680 R=NA+1,NT
         RJI = (RJ*(RJ-1))/2
         PQ = RI + 1
CJOE
         PQI = (RI*(RI+3))/2
C        PQI = (PQ*(PQ-1))/2+RI
         DO 683 Q=I+1,J-1
            INDX(INDZ+1) = PQI
            INDX(INDZ+2) = RJI + PQ
            INDZ = INDZ + 2
            PQI = PQI + PQ
            PQ = PQ + 1
  683    CONTINUE
         RJ = RJ + R
         RI = RI + R
  680 CONTINUE
C
      INZ(9) = INDZ
C
      PI = NN + I
      PJ = NN + J
      DO 820 P=NA+1,NT
         PJI = (PJ*(PJ-1))/2
         PII = (PI*(PI-1))/2
         JQ = J1 + 1
         IQ = I1 + 1
         DO 825 Q=1,I-1
            INDX(INDZ+1) = PII + IQ
            INDX(INDZ+2) = PJI + IQ
            INDX(INDZ+3) = PII + JQ
            INDX(INDZ+4) = PJI + JQ
            INDZ = INDZ + 4
            JQ = JQ + 1
            IQ = IQ + 1
  825    CONTINUE
         PI = PI + P
         PJ = PJ + P
  820 CONTINUE
C
      PI = NN + I
      PJ = NN + J
      DO 840 P=NA+1,NT
         PII = (PI*(PI-1))/2
         PJI = (PJ*(PJ-1))/2
         QI = II + I
         JQ = J1 + I + 1
         DO 843 Q=I+1,J-1
            INDX(INDZ+1) = PII + QI
            INDX(INDZ+2) = PJI + QI
            INDX(INDZ+3) = PII + JQ
            INDX(INDZ+4) = PJI + JQ
            INDZ = INDZ + 4
            QI = QI + Q
            JQ = JQ + 1
  843    CONTINUE
         PI = PI + P
         PJ = PJ + P
  840 CONTINUE
C
      PI = NN + I
      PJ = NN + J
      DO 850 P=NA+1,NT
         PII = (PI*(PI-1))/2
         PJI = (PJ*(PJ-1))/2
         QI = JJ + I
         QJ = JJ + J
         DO 853 Q=J+1,P-1
            INDX(INDZ+1) = PII + QI
            INDX(INDZ+2) = PJI + QI
            INDX(INDZ+3) = PII + QJ
            INDX(INDZ+4) = PJI + QJ
            INDZ = INDZ + 4
            QI = QI + Q
            QJ = QJ + Q
  853    CONTINUE
         PI = PI + P
         PJ = PJ + P
  850 CONTINUE
C
      INZ(10) = INDZ
C
      PI = NN + I
      PJ = NN + J
      DO 880 P=NA+1,NT
         PII = (PI*(PI-1))/2
         PJI = (PJ*(PJ-1))/2
         INDX(INDZ+1) = PII + PI
         INDX(INDZ+2) = PJI + PI
         INDX(INDZ+3) = PJI + PJ
         INDZ = INDZ + 3
         PI = PI + P
         PJ = PJ + P
  880 CONTINUE
C
      INZ(11) = INDZ
C
      PQ = NN + 1
      PQI = (PQ*(PQ-1))/2
      DO 930 P=NA+1,NT
         DO 933 Q=1,P
            IF (Q.EQ.I.OR.Q.EQ.J) GOTO 935
            INDX(INDZ+1) = PQI + II
            INDX(INDZ+2) = PQI + JI
            INDX(INDZ+3) = PQI + JJ
            INDZ = INDZ + 3
  935       PQI = PQI + PQ
            PQ = PQ + 1
  933    CONTINUE
  930 CONTINUE
C
      INZ(12) = INDZ
C
      PI = NN + I
      PJ = NN + J
      DO 860 P=NA+1,NT
         PII = (PI*(PI-1))/2
         PJI = (PJ*(PJ-1))/2
         INDX(INDZ+1) = PII + II
         INDX(INDZ+2) = PII + JI
         INDX(INDZ+3) = PII + JJ
         INDX(INDZ+4) = PJI + II
         INDX(INDZ+5) = PJI + JI
         INDX(INDZ+6) = PJI + JJ
         INDZ = INDZ + 6
         PI = PI + P
         PJ = PJ + P
  860 CONTINUE
C
      INZ(13) = INDZ
C
      RETURN
      END
C
C*MODULE MCJAC   *DECK MCREAAA
      SUBROUTINE MCREAAA(S1,S2,D1,D2,A,B,C,INDX,INZ)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION S1(*),S2(*),D1(*),D2(*),INDX(*)
      DIMENSION A(14),INZ(6)
      DIMENSION B(9),C(9)
C
      DO 23 II=1,14
         A(II) = 0.0D+00
   23 CONTINUE
C
      I1 = INDX(1)
      I2 = INDX(2)
      I3 = INDX(3)
C
      A(3)=D1(I1)*S1(I3)-D1(I2)*S1(I2)+D1(I3)*S1(I1)
      A(4)=2.0D+00*S1(I2)*(D1(I3)-D1(I1))+D1(I2)*(S1(I1)-S1(I3))
      A(5)=D1(I1)*S1(I1)+D1(I2)*S1(I2)+D1(I3)*S1(I3)
C
      DO 45 II=4,INZ(1),2
         I1=INDX(II)
         I2=INDX(II+1)
         A(1)=A(1)+ D1(I2)*S1(I1)-D1(I1)*S1(I2)
         A(2)=A(2)+ D1(I1)*S1(I1)+D1(I2)*S1(I2)
   45 CONTINUE
C
      DO 55 II=INZ(1)+1,INZ(2),2
         I1=INDX(II)
         I2=INDX(II+1)
         A(1)=A(1)+ D2(I2)*S2(I1)-D2(I1)*S2(I2)
         A(2)=A(2)+ D2(I1)*S2(I1)+D2(I2)*S2(I2)
   55 CONTINUE
C
      DO 66 II=INZ(2)+1,INZ(3),4
         I1=INDX(II)
         I2=INDX(II+1)
         I3=INDX(II+2)
         I4=INDX(II+3)
         A(3)=A(3)+ D2(I1)*S2(I4)-D2(I3)*S2(I2)-
     J              D2(I2)*S2(I3)+D2(I4)*S2(I1)
         A(4)=A(4)+ (D2(I4)-D2(I1))*(S2(I3)+S2(I2))+
     O              (D2(I3)+D2(I2))*(S2(I1)-S2(I4))
         A(5)=A(5)+ D2(I1)*S2(I1)+D2(I2)*S2(I2)+
     E              D2(I3)*S2(I3)+D2(I4)*S2(I4)
   66 CONTINUE
C
      DO 77 II=INZ(3)+1,INZ(4),3
         I1=INDX(II)
         I2=INDX(II+1)
         I3=INDX(II+2)
         A(3)=A(3)+ D2(I1)*S2(I3)-D2(I2)*S2(I2)+
     A              D2(I3)*S2(I1)
         A(4)=A(4)+ 2.0D+00*S2(I2)*(D2(I3)-D2(I1))+
     N              D2(I2)*(S2(I1)-S2(I3))
         A(5)=A(5)+ D2(I1)*S2(I1)+D2(I2)*S2(I2)+
     D              D2(I3)*S2(I3)
   77 CONTINUE
C
      DO 88 II=INZ(4)+1,INZ(5),3
         I1=INDX(II)
         I2=INDX(II+1)
         I3=INDX(II+2)
         A(3)=A(3)+ D2(I1)*S2(I3)-D2(I2)*S2(I2)+
     K              D2(I3)*S2(I1)
         A(4)=A(4)+ 2.0D+00*S2(I2)*(D2(I3)-D2(I1))+
     L              D2(I2)*(S2(I1)-S2(I3))
         A(5)=A(5) + D2(I1)*S2(I1)+D2(I2)*S2(I2)+
     A              D2(I3)*S2(I3)
   88 CONTINUE
C
      DO 99 II=INZ(5)+1,INZ(6),6
         I1=INDX(II)
         I2=INDX(II+1)
         I3=INDX(II+2)
         I4=INDX(II+3)
         I5=INDX(II+4)
         I6=INDX(II+5)
         A(6)=A(6)- D2(I1)*S2(I6)+D2(I2)*S2(I5)-D2(I3)*S2(I4)+
     U              D2(I4)*S2(I3)-D2(I5)*S2(I2)+D2(I6)*S2(I1)
         A(7)=A(7)+ D2(I1)*(S2(I3)+2.0D+00*S2(I5))+
     S              D2(I2)*(S2(I6)-S2(I2)-S2(I4))+
     *              D2(I3)*(S2(I1)-2.0D+00*S2(I5))+
     A              D2(I4)*(S2(I6)-2.0D+00*S2(I2))+
     M              D2(I5)*(S2(I1)-S2(I3)-S2(I5))+
     E              D2(I6)*(S2(I4)+2.0D+00*S2(I2))
         A(8)=A(8)- D2(I1)*(S2(I4)+2.0D+00*S2(I2))+
     S              D2(I2)*(S2(I1)-S2(I3)-S2(I5))-
     L              D2(I3)*(S2(I6)-2.0D+00*S2(I2))+
     A              D2(I4)*(S2(I1)-2.0D+00*S2(I5))-
     B              D2(I5)*(S2(I6)-S2(I2)-S2(I4))+
     *              D2(I6)*(S2(I3)+2.0D+00*S2(I5))
         A(9)=A(9)+ D2(I1)*S2(I1)+D2(I2)*S2(I2)+D2(I3)*S2(I3)+
     *              D2(I4)*S2(I4)+D2(I5)*S2(I5)+D2(I6)*S2(I6)
   99 CONTINUE
C
      IN=INZ(6)+1
      I1=INDX(IN)
      I2=INDX(IN+1)
      I3=INDX(IN+2)
      I4=INDX(IN+3)
      I5=INDX(IN+4)
      I6=INDX(IN+5)
C
      A(10)= D2(I1)*S2(I6)-D2(I2)*S2(I5)+D2(I3)*S2(I3)+
     *       D2(I4)*S2(I4)-D2(I5)*S2(I2)+D2(I6)*S2(I1)
      A(11)= D2(I1)*(-4.0D+00*S2(I5))+
     *       D2(I2)*(S2(I3)+2.0D+00*S2(I4)-S2(I6))+
     *       D2(I3)*(-2.0D+00*S2(I2)+2.0D+00*S2(I5))+
     *       D2(I4)*(-2.0D+00*S2(I2)+2.0D+00*S2(I5))+
     *       D2(I5)*(S2(I1)-2.0D+00*S2(I4)-S2(I3))+
     *       D2(I6)*(4.0D+00*S2(I2))
      A(12)= D2(I1)*(2.0D+00*S2(I3)+4.0D+00*S2(I4))+
     *       D2(I2)*(-3.0D+00*S2(I2)+3.0D+00*S2(I5))+
     *       D2(I3)*(S2(I1)-4.0D+00*S2(I4)+S2(I6))+
     *       D2(I4)*(S2(I1)+S2(I6)-2.0D+00*S2(I3)-
     *      2.0D+00*S2(I4))+
     *       D2(I5)*(3.0D+00*S2(I2)-3.0D+00*S2(I5))+
     *       D2(I6)*(2.0D+00*S2(I3)+4.0D+00*S2(I4))
      A(13)= D2(I1)*(-4.0D+00*S2(I2))+
     *       D2(I2)*(S2(I1)-S2(I3)-2.0D+00*S2(I4))+
     *       D2(I3)*(2.0D+00*S2(I2)-2.0D+00*S2(I5))+
     *       D2(I4)*(2.0D+00*S2(I2)-2.0D+00*S2(I5))+
     *       D2(I5)*(S2(I3)+2.0D+00*S2(I4)-S2(I6))+
     *       D2(I6)*(4.0D+00*S2(I5))
      A(14)= D2(I1)*S2(I1)+D2(I2)*S2(I2)+D2(I3)*S2(I3)+
     *       D2(I4)*S2(I4)+D2(I5)*S2(I5)+D2(I6)*S2(I6)
C
C    NOW FOR THE B AND THE C COEFFICIENTS
C
      B(1) = A(5) + A(14)
      B(2) = A(1) + A(8)
      B(3) = A(3) - A(5) + A(12) - 2.0D+00*A(14)
      B(4) = A(6) - A(8)
      B(5) = A(10) - A(12) + A(14)
      B(6) = A(2) + A(9)
      B(7) = A(4) + A(13)
      B(8) = A(7) - A(9)
      B(9) = A(11) - A(13)
C
      C(1) = B(7)
      C(2) = 2.0D+00*B(8) - B(6)
      C(3) = 3.0D+00*B(9) - 2.0D+00*B(7)
      C(4) = -3.0D+00*B(8)
      C(5) = -4.0D+00*B(9)
      C(6) = B(2)
      C(7) = 2.0D+00*B(3)
      C(8) = 3.0D+00*B(4)
      C(9) = 4.0D+00*B(5)
C
      RETURN
      END
C
C*MODULE MCJAC   *DECK MCINDAV
      SUBROUTINE MCINDAV(INDX,I,J,NA,NT,INZ)
      IMPLICIT INTEGER(A-Z)
      DIMENSION INDX(*),INZ(*)
C
      INDZ = 0
      NN = (NA*(NA+1))/2
C
      II = (I*(I+1))/2
      I1 = II - I
      IIII = (II*(II-1))/2
      JJ = (J*(J+1))/2
      JJJJ = (JJ*(JJ-1))/2
      J1 = JJ - J
      JI = J1 + I
      JI00 = (JI*(JI-1))/2
C
C  FIRST THE 1E INDICES
C  HII,HJI,HJJ
C
      INDX(1) = II
      INDX(2) = JI
      INDX(3) = JJ
      INDZ = 3
C
C  NOW FOR ALL RELEVANT P
C
      DO 13 P=1,I-1
         INDX(INDZ+1) = I1 + P
         INDX(INDZ+2) = J1 + P
         INDZ = INDZ + 2
   13 CONTINUE
      P1 = II + I
      DO 15 P=I+1,NA
         INDX(INDZ+1) = P1
         INDX(INDZ+2) = J1 + P
         INDZ = INDZ + 2
         P1 = P1 + P
   15 CONTINUE
C
      INZ(1) = INDZ
C
C  END OF THE 1E INDICES, NOW FOR THE 2E INDICES.
C
C  [PQ|IR], [PQ|JR] ALL RELEVANT P,Q,R IE WHERE I,J
C  OCCURS IN POSITION SPECIFIED AND INTEGRAL CORRESPONDS
C  TO REVERSE CANONICAL ORDER.
C
      IR = I1 + 1
      JR = J1 + 1
      IRS = (IR*(IR-1))/2
      JRS = (JR*(JR-1))/2
      DO 21 R=1,I-1
         DO 24 PQ=1,I1
            INDX(INDZ+1) = IRS+PQ
            INDX(INDZ+2) = JRS+PQ
            INDZ = INDZ + 2
   24    CONTINUE
         IRS = IRS + IR
         JRS = JRS + JR
         IR = IR + 1
         JR = JR + 1
   21 CONTINUE
C
C  [PQ|RI], [PQ|JR]
C
      JR = J1+I+1
      JRI = (JR*(JR-1))/2
      RI = II+I
      DO 25 R=I+1,NA
         RII = (RI*(RI-1))/2
         PQ = 1
         DO 27 P=1,R-1
            IF (P.EQ.I) GOTO 23
            DO 29 Q=1,P
               IF (Q.EQ.I) GOTO 28
               INDX(INDZ+1) = RII + PQ
               INDX(INDZ+2) = JRI + PQ
               INDZ = INDZ + 2
   28          PQ = PQ + 1
   29       CONTINUE
            GOTO 27
   23       PQ = PQ + P
   27    CONTINUE
         DO 22 Q=1,I-1
             INDX(INDZ+1) = RII + PQ
            INDX(INDZ+2) = JRI + PQ
            INDZ = INDZ + 2
            PQ = PQ + 1
   22    CONTINUE
         JRI = JRI + JR
         JR = JR + 1
         RI = RI + R
   25 CONTINUE
C
C  [IR|PQ], [PQ|JR]
C
      PQ = II + 1
      PQI = (PQ*(PQ-1))/2 + I1
      IX = J1+1
      IX2 = (IX*(IX-1))/2
      DO 40 P=I+1,NA
         DO 41 Q=1,P
            IF (Q.EQ.I) GOTO 43
            JR = IX
            JRI = IX2
            DO 45 R=1,I-1
               INDX(INDZ+1) = PQI + R
               INDX(INDZ+2) = JRI + PQ
               INDZ = INDZ + 2
               JRI = JRI + JR
               JR = JR + 1
   45       CONTINUE
   43       PQI = PQI + PQ
            PQ = PQ + 1
   41    CONTINUE
   40 CONTINUE
C
C   [RI|PQ],[PQ|JR]
C
      RI = II+I
      JR = J1+I+1
      JRI=(JR*(JR-1))/2
      DO 90 R=I+1,NA
         PQ = RI+1
CJOE
         PQI = (RI*(RI+3))/2
C        PQI = (PQ*(PQ-1))/2 + RI
         DO 92 Q=I+1,R
            INDX(INDZ+1) = PQI
            INDX(INDZ+2) = JRI+PQ
            INDZ = INDZ + 2
            PQI = PQI + PQ
            PQ = PQ + 1
   92    CONTINUE
         DO 94 P=R+1,NA
            DO 96 Q=1,P
               IF (Q.EQ.I) GOTO 97
               INDX(INDZ+1) = PQI
               INDX(INDZ+2) = JRI+PQ
               INDZ = INDZ + 2
   97          PQI = PQI + PQ
               PQ = PQ + 1
   96       CONTINUE
   94    CONTINUE
         RI = RI + R
         JRI = JRI + JR
         JR = JR + 1
   90 CONTINUE
C
      INZ(2) = INDZ
C
C   [IQ|IP],[IQ|JP],[IP|JQ],[JQ|JP]
C
      IP = I1+1
      IPI = (IP*(IP-1))/2
      JP = J1+1
      JPI = (JP*(JP-1))/2
      IX = JPI
      DO 100 P=1,I-1
         JQI = IX + IP
         JQ = J1 + 1
         IQ = I1 + 1
         DO 105 Q=1,P-1
            INDX(INDZ+1) = IPI+IQ
            INDX(INDZ+2) = JPI+IQ
            INDX(INDZ+3) = JQI
            INDX(INDZ+4) = JPI+JQ
            INDZ = INDZ + 4
            IQ = IQ+1
            JQI = JQI+JQ
            JQ = JQ + 1
  105    CONTINUE
         IPI = IPI + IP
         IP = IP + 1
         JPI = JPI + JP
         JP = JP + 1
  100 CONTINUE
C
C   [IQ|PI],[IQ|JP],[PI|JQ],[JQ|JP]
C
      PI = II+I
      JP = J1+I + 1
      JPI = (JP*(JP-1))/2
      IX = J1+1
      JX = (IX*(IX-1))/2
      DO 110 P=I+1,NA
         PII = (PI*(PI-1))/2 +I1+1
         JQ = J1+1
         JQI = JX + PI
         IQ = I1+1
         DO 115 Q=1,I-1
            INDX(INDZ+1) = PII
            INDX(INDZ+2) = JPI + IQ
            INDX(INDZ+3) = JQI
            INDX(INDZ+4) = JPI + JQ
            INDZ = INDZ + 4
            JQI = JQI + JQ
            JQ = JQ + 1
            IQ = IQ + 1
            PII = PII + 1
  115    CONTINUE
         PI = PI + P
         JPI = JPI + JP
         JP = JP + 1
  110 CONTINUE
C
C   [QI|PI],[QI|JP],[PI|JQ],[JQ|JP]
C
      JP = J1+I+1
      IX = JP
      JPI = (JP*(JP-1))/2
      IX2 = JPI
      PI = II + I
      DO 130 P=I+1,NA
         PII = (PI*(PI-1))/2
         JQ = IX
         JQI = IX2
         QI = II+I
         DO 135 Q=I+1,P-1
            INDX(INDZ+1) = PII + QI
            INDX(INDZ+2) = JPI + QI
            INDX(INDZ+3) = JQI + PI
            INDX(INDZ+4) = JPI + JQ
            INDZ = INDZ + 4
            JQI = JQI + JQ
            JQ = JQ + 1
            QI = QI + Q
  135    CONTINUE
         PI = PI + P
         JPI = JPI + JP
         JP = JP + 1
  130 CONTINUE
C
      INZ(3) = INDZ
C
C   [IP|IP],[IP|JP],[JP|JP]
C
      IP = I1 + 1
      JP = J1 + 1
      JPI = (JP*(JP-1))/2
      IPI = (IP*(IP-1))/2
      DO 160 P=1,I-1
         INDX(INDZ+1) = IPI + IP
         INDX(INDZ+2) = JPI + IP
         INDX(INDZ+3) = JPI + JP
         INDZ = INDZ + 3
         IPI = IPI + IP
         JPI = JPI + JP
         IP = IP + 1
         JP = JP + 1
  160 CONTINUE
C
C   [PI|PI],[PI|JP],[JP|JP]
C
      PI = II + I
      JP = J1 + I + 1
      JPI = (JP*(JP-1))/2
      DO 170 P=I+1,NA
         PII = (PI*(PI-1))/2
         INDX(INDZ+1) = PII + PI
         INDX(INDZ+2) = JPI + PI
         INDX(INDZ+3) = JPI + JP
         INDZ = INDZ + 3
         JPI = JPI + JP
         PI = PI + P
         JP = JP + 1
  170 CONTINUE
C
      INZ(4) = INDZ
C
C   [PQ|II],[PQ|JI],[PQ|JJ]
C
      DO 190 PQ=1,I1
         INDX(INDZ+1) = IIII+PQ
         INDX(INDZ+2) = JI00+PQ
         INDX(INDZ+3) = JJJJ+PQ
         INDZ = INDZ + 3
  190 CONTINUE
C
C    [II|PQ],[PQ|JI],[PQ|JJ]
C
      PQ = II + 1
      PQI = (PQ*(PQ-1))/2
      DO 200 P=I+1,NA
         DO 210 Q=1,P
            IF (Q.EQ.I) GOTO 220
            INDX(INDZ+1) = PQI + II
            INDX(INDZ+2) = JI00 + PQ
            INDX(INDZ+3) = JJJJ+PQ
            INDZ = INDZ + 3
  220       PQI = PQI + PQ
            PQ = PQ + 1
  210    CONTINUE
  200 CONTINUE
C
      INZ(5) = INDZ
C
C   [IP|II],[IP|JI],[IP|JJ],[II|JP],[JP|JI],[JP|JJ]
C
      IP = I1 + 1
      JP = J1 + 1
      JPI = (JP*(JP-1))/2
      DO 240 P=1,I-1
         INDX(INDZ+1) = IIII + IP
         INDX(INDZ+2) = JI00 + IP
         INDX(INDZ+3) = JJJJ + IP
         INDX(INDZ+4) = JPI + II
         INDX(INDZ+5) = JI00 + JP
         INDX(INDZ+6) = JJJJ + JP
         INDZ = INDZ + 6
         JPI = JPI + JP
         JP = JP + 1
         IP = IP + 1
  240 CONTINUE
C
C   [II|PI],[PI|JI],[PI|JJ],[II|JP],[JI|JP],[JP|JJ]
C
      PI = II + I
      JP = J1 + I + 1
      JPI = (JP*(JP-1))/2
      DO 250 P=I+1,NA
         PII = (PI*(PI-1))/2
         INDX(INDZ+1) = PII + II
         INDX(INDZ+2) = JI00 + PI
         INDX(INDZ+3) = JJJJ + PI
         INDX(INDZ+4) = JPI + II
         INDX(INDZ+5) = JPI + JI
         INDX(INDZ+6) = JJJJ + JP
         INDZ = INDZ + 6
         PI = PI + P
         JPI = JPI + JP
         JP = JP + 1
  250 CONTINUE
C
      INZ(6) = INDZ
C
      INDX(INDZ+1) = IIII + II
      INDX(INDZ+2) = JI00 + II
      INDX(INDZ+3) = JJJJ + II
      INDX(INDZ+4) = JI00 + JI
      INDX(INDZ+5) = JJJJ + JI
      INDX(INDZ+6) = JJJJ + JJ
      INDZ = INDZ + 6
C
      INZ(7) = INDZ
C
C     NOW FOR ALL INTEGRALS INVOLVING VIRTUAL INDICES.
C
      P1 = NN
      DO 14 P=NA+1,J-1
         INDX(INDZ+1) = P1 + I
         INDX(INDZ+2) = J1 + P
         INDZ = INDZ + 2
         P1 = P1 + P
   14 CONTINUE
C
      P1 = JJ
      DO 17 P=J+1,NT
         INDX(INDZ+1) = P1 + I
         INDX(INDZ+2) = P1 + J
         INDZ = INDZ + 2
         P1 = P1 + P
   17 CONTINUE
C
      INZ(8) = INDZ
C
C  2 E- INTEGRALS.
C
      JR = J1 + NA +1
      JRI = (JR*(JR-1))/2
      RI = NN + I
      DO 325 R=NA+1,J-1
         RII = (RI*(RI-1))/2
         PQ = 1
         DO 327 P=1,R-1
            IF (P.EQ.I) GOTO 323
            DO 329 Q=1,P
               IF (Q.EQ.I) GOTO 328
               INDX(INDZ+1) = RII + PQ
               INDX(INDZ+2) = JRI + PQ
               INDZ = INDZ + 2
  328          PQ = PQ + 1
  329       CONTINUE
            GOTO 327
  323       PQ = PQ + P
  327    CONTINUE
         DO 322 Q=1,I-1
            INDX(INDZ+1) = RII + PQ
            INDX(INDZ+2) = JRI + PQ
            INDZ = INDZ + 2
            PQ = PQ + 1
  322    CONTINUE
         JRI = JRI + JR
         JR = JR + 1
         RI = RI + R
  325 CONTINUE
C
      RI = JJ + I
      RJ = JJ + J
      DO 335 R=J+1,NT
         RII = (RI*(RI-1))/2
         RJI = (RJ*(RJ-1))/2
         PQ = 1
         DO 337 P=1,R-1
            IF (P.EQ.I.OR.P.EQ.J) GOTO 339
            DO 334 Q=1,P
               IF (Q.EQ.I.OR.Q.EQ.J) GOTO 331
               INDX(INDZ+1) = RII + PQ
               INDX(INDZ+2) = RJI + PQ
               INDZ = INDZ + 2
  331          PQ = PQ + 1
  334       CONTINUE
            GOTO 337
  339       PQ = PQ + P
  337    CONTINUE
         DO 332 Q=1,I-1
            INDX(INDZ+1) = RII + PQ
            INDX(INDZ+2) = RJI + PQ
            INDZ = INDZ + 2
            PQ = PQ + 1
  332    CONTINUE
         RI = RI + R
         RJ = RJ + R
  335 CONTINUE
C
      PQ = NN + 1
      PQI = (PQ*(PQ-1))/2 + I1
      IX = J1 + 1
      IX2 = (IX*(IX-1))/2
      DO 340 P=NA+1,J-1
         DO 341 Q=1,P
            IF (Q.EQ.I) GOTO 343
            JR = IX
            JRI = IX2
            DO 345 R=1,I-1
                 INDX(INDZ+1) = PQI + R
               INDX(INDZ+2) = JRI + PQ
               INDZ = INDZ + 2
               JRI = JRI + JR
               JR = JR + 1
  345       CONTINUE
  343       PQI = PQI + PQ
            PQ = PQ + 1
  341    CONTINUE
  340 CONTINUE
C
      PQ = JJ + 1
      PQI = (PQ*(PQ-1))/2 + I1
      PQJ = PQI - I1 + J1
      DO 350 P=J+1,NT
         DO 351 Q=1,P
            IF (Q.EQ.I.OR.Q.EQ.J) GOTO 353
            DO 355 R=1,I-1
               INDX(INDZ+1) = PQI + R
               INDX(INDZ+2) = PQJ + R
               INDZ = INDZ + 2
  355       CONTINUE
  353       PQI = PQI + PQ
            PQJ = PQJ + PQ
            PQ = PQ + 1
  351    CONTINUE
  350 CONTINUE
C
      RI = II + I
      JR = J1 + I+1
      IX = JJ+1
      IX2 =  (IX*(IX-1))/2
      DO 362 R=I+1,J-1
         PQ = IX
         PQI = IX2
         DO 364 P=J+1,NT
            DO 365 Q=1,P
               IF (Q.EQ.I.OR.Q.EQ.J) GOTO 367
               INDX(INDZ+1) = PQI + RI
               INDX(INDZ+2) = PQI + JR
               INDZ = INDZ + 2
  367          PQI = PQI + PQ
               PQ = PQ + 1
  365       CONTINUE
  364    CONTINUE
         RI = RI + R
         JR = JR + 1
  362 CONTINUE
C
      RJ = JJ + J
      RI = JJ + I
      DO 370 R=J+1,NT
         PQ = (R*(R-1))/2+J+1
         PQI = (PQ*(PQ-1))/2 + RI
         PQJ = PQI - RI + RJ
         DO 372 Q=J+1,R
            INDX(INDZ+1) = PQI
            INDX(INDZ+2) = PQJ
            INDZ = INDZ + 2
            PQI = PQI + PQ
            PQJ = PQJ + PQ
            PQ = PQ + 1
  372    CONTINUE
         DO 375 P=R+1,NT
            DO 377 Q=1,P
               IF (Q.EQ.I.OR.Q.EQ.J) GOTO 379
               INDX(INDZ+1) = PQI
               INDX(INDZ+2) = PQJ
               INDZ = INDZ + 2
  379          PQI = PQI + PQ
               PQJ = PQJ + PQ
               PQ = PQ + 1
  377       CONTINUE
  375    CONTINUE
         RJ = RJ + R
         RI = RI + R
  370 CONTINUE
C
      RI = JJ + I
      RJ = JJ + J
      DO 380 R=J+1,NT
         RJI = (RJ*(RJ-1))/2
         PQ = RI + 1
         PQI = (PQ*(PQ-1))/2 + RI
         DO 383 Q=I+1,J-1
            INDX(INDZ+1) = PQI
            INDX(INDZ+2) = RJI + PQ
            INDZ = INDZ + 2
            PQI = PQI + PQ
            PQ = PQ + 1
  383    CONTINUE
         RJ = RJ + R
         RI = RI + R
  380 CONTINUE
C
      RI = II + I
      JR = J1 + I + 1
      JRI = (JR*(JR-1))/2
      DO 390 R=I+1,NA
         PQ = NN + 1
         PQI = (PQ*(PQ-1))/2 + RI
         DO 394 P=NA+1,J-1
            DO 396 Q=1,P
               IF (Q.EQ.I) GOTO 397
               INDX(INDZ+1) = PQI
               INDX(INDZ+2) = JRI + PQ
               INDZ = INDZ + 2
  397          PQI = PQI + PQ
               PQ = PQ + 1
  396       CONTINUE
  394    CONTINUE
         RI = RI + R
         JRI = JRI + JR
         JR = JR + 1
  390 CONTINUE
C
      RI = NN + I
      JR = J1 + NA + 1
      JRI = (JR*(JR-1))/2
      DO 490 R=NA+1,J-1
         PQ = RI + 1
         PQI = (PQ*(PQ-1))/2 + RI
         DO 492 Q=I+1,R
            INDX(INDZ+1) = PQI
            INDX(INDZ+2) = JRI + PQ
            INDZ = INDZ + 2
            PQI = PQI + PQ
            PQ = PQ + 1
  492    CONTINUE
         DO 494 P=R+1,J-1
            DO 496 Q=1,P
               IF (Q.EQ.I) GOTO 497
               INDX(INDZ+1) = PQI
               INDX(INDZ+2) = JRI + PQ
               INDZ = INDZ + 2
  497          PQI = PQI + PQ
               PQ = PQ + 1
  496       CONTINUE
  494    CONTINUE
         RI = RI + R
         JRI = JRI + JR
         JR = JR + 1
  490 CONTINUE
C
      INZ(9) = INDZ
C
      PI = NN + I
      JP = J1 + NA + 1
      JPI = (JP*(JP-1))/2
      IX = J1 + 1
      JX = (IX*(IX-1))/2
      DO 510 P=NA+1,J-1
         PII = (PI*(PI-1))/2+I1+1
         JQ = J1 + 1
         JQI = JX + PI
         IQ = I1 + 1
         DO 515 Q=1,I-1
            INDX(INDZ+1) = PII
            INDX(INDZ+2) = JPI + IQ
            INDX(INDZ+3) = JQI
            INDX(INDZ+4) = JPI + JQ
            INDZ = INDZ + 4
            JQI = JQI + JQ
            JQ = JQ + 1
            IQ = IQ + 1
            PII = PII + 1
  515    CONTINUE
         PI = PI + P
         JPI = JPI + JP
         JP = JP + 1
  510 CONTINUE
C
      PI = JJ + I
      PJ = JJ + J
      DO 520 P=J+1,NT
         PJI = (PJ*(PJ-1))/2
         PII = (PI*(PI-1))/2
         JQ = J1 + 1
         IQ = I1 + 1
         DO 525 Q=1,I-1
            INDX(INDZ+1) = PII + IQ
            INDX(INDZ+2) = PJI + IQ
            INDX(INDZ+3) = PII + JQ
            INDX(INDZ+4) = PJI + JQ
            INDZ = INDZ + 4
            JQ = JQ + 1
            IQ = IQ + 1
  525    CONTINUE
         PI = PI + P
         PJ = PJ + P
  520 CONTINUE
C
      JP = J1 + NA + 1
      IX = J1 + I + 1
      JPI = (JP*(JP-1))/2
      IX2 = (IX*(IX-1))/2
      PI = NN + I
      DO 530 P=NA+1,J-1
         PII = (PI*(PI-1))/2
         JQ = IX
         JQI = IX2
         QI = II + I
         DO 535 Q=I+1,P-1
            INDX(INDZ+1) = PII + QI
            INDX(INDZ+2) = JPI + QI
            INDX(INDZ+3) = JQI + PI
            INDX(INDZ+4) = JPI + JQ
            INDZ = INDZ + 4
            JQI = JQI + JQ
            JQ = JQ + 1
            QI = QI + Q
  535    CONTINUE
         PI = PI + P
         JPI = JPI + JP
         JP = JP + 1
  530 CONTINUE
C
      PI = JJ + I
      PJ = JJ + J
      DO 540 P=J+1,NT
         PII = (PI*(PI-1))/2
         PJI = (PJ*(PJ-1))/2
         QI = II + I
         JQ = J1 + I + 1
         DO 543 Q=I+1,J-1
            INDX(INDZ+1) = PII + QI
            INDX(INDZ+2) = PJI + QI
            INDX(INDZ+3) = PII + JQ
            INDX(INDZ+4) = PJI + JQ
            INDZ = INDZ + 4
            QI = QI + Q
            JQ = JQ + 1
  543    CONTINUE
         PI = PI + P
         PJ = PJ + P
  540 CONTINUE
C
      PI = JJ + I
      PJ = JJ + J
      DO 550 P=J+1,NT
         PII = (PI*(PI-1))/2
         PJI = (PJ*(PJ-1))/2
         QI = JJ + I
         QJ = JJ + J
         DO 553 Q=J+1,P-1
            INDX(INDZ+1) = PII + QI
            INDX(INDZ+2) = PJI + QI
            INDX(INDZ+3) = PII + QJ
            INDX(INDZ+4) = PJI + QJ
            INDZ = INDZ + 4
            QI = QI + Q
            QJ = QJ + Q
  553    CONTINUE
         PI = PI + P
         PJ = PJ + P
  550 CONTINUE
C
      INZ(10) = INDZ
C
      PI = NN + I
      JP = J1 + NA + 1
      JPI = (JP*(JP-1))/2
      DO 570 P=NA+1,J-1
         PII = (PI*(PI-1))/2
         INDX(INDZ+1) = PII + PI
         INDX(INDZ+2) = JPI + PI
         INDX(INDZ+3) = JPI + JP
         INDZ = INDZ + 3
         JPI = JPI + JP
         PI = PI + P
         JP = JP + 1
  570 CONTINUE
C
      PI = JJ + I
      PJ = JJ + J
      DO 580 P=J+1,NT
         PII = (PI*(PI-1))/2
         PJI = (PJ*(PJ-1))/2
         INDX(INDZ+1) = PII + PI
         INDX(INDZ+2) = PJI + PI
         INDX(INDZ+3) = PJI + PJ
         INDZ = INDZ + 3
         PI = PI + P
         PJ = PJ + P
  580 CONTINUE
C
      INZ(11) = INDZ
C
      PQ = NN + 1
      PQI = (PQ*(PQ-1))/2
      DO 700 P=NA+1,J-1
         DO 710 Q=1,P
            IF (Q.EQ.I) GOTO 720
            INDX(INDZ+1) = PQI + II
            INDX(INDZ+2) = JI00 + PQ
            INDX(INDZ+3) = JJJJ + PQ
            INDZ = INDZ + 3
  720       PQI = PQI + PQ
            PQ = PQ + 1
  710    CONTINUE
  700 CONTINUE
C
      PQ = JJ + 1
      PQI = (PQ*(PQ-1))/2
      DO 730 P=J+1,NT
         DO 733 Q=1,P
            IF (Q.EQ.I.OR.Q.EQ.J) GOTO 735
            INDX(INDZ+1) = PQI + II
            INDX(INDZ+2) = PQI + JI
            INDX(INDZ+3) = PQI + JJ
            INDZ = INDZ + 3
  735       PQI = PQI + PQ
            PQ = PQ + 1
  733    CONTINUE
  730 CONTINUE
C
      INZ(12) = INDZ
C
      PI = NN + I
      JP = J1 + NA + 1
      JPI = (JP*(JP-1))/2
      DO 750 P=NA+1,J-1
         PII = (PI*(PI-1))/2
         INDX(INDZ+1) = PII + II
         INDX(INDZ+2) = JI00 + PI
         INDX(INDZ+3) = JJJJ + PI
         INDX(INDZ+4) = JPI + II
         INDX(INDZ+5) = JPI + JI
         INDX(INDZ+6) = JJJJ + JP
         INDZ = INDZ + 6
         PI = PI + P
         JPI = JPI + JP
         JP = JP + 1
  750 CONTINUE
C
      PI = JJ + I
      PJ = JJ + J
      DO 760 P=J+1,NT
         PII = (PI*(PI-1))/2
         PJI = (PJ*(PJ-1))/2
         INDX(INDZ+1) = PII + II
         INDX(INDZ+2) = PII + JI
         INDX(INDZ+3) = PII + JJ
         INDX(INDZ+4) = PJI + II
         INDX(INDZ+5) = PJI + JI
         INDX(INDZ+6) = PJI + JJ
         INDZ = INDZ + 6
         PI = PI + P
         PJ = PJ + P
  760 CONTINUE
C
      INZ(13) = INDZ
C
      RETURN
      END
C
C*MODULE MCJAC   *DECK MCREAAV
      SUBROUTINE MCREAAV(S1,S2,D1,D2,A,B,C,INDX,INZ)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION S1(*),S2(*),D1(*),D2(*),INDX(*)
      DIMENSION A(14),INZ(6)
      DIMENSION B(9),C(9)
C
      DO 23 II=1,14
         A(II) = 0.0D+00
   23 CONTINUE
C
      I1 = INDX(1)
      I2 = INDX(2)
      I3 = INDX(3)
C
      A(3) = D1(I1)*S1(I3)
      A(4) = -2.0D+00*D1(I1)*S1(I2)
      A(5) = D1(I1)*S1(I1)
C
      DO 45 II=4,INZ(1),2
         I1 = INDX(II)
         I2 = INDX(II+1)
         A(1) = A(1) - D1(I1)*S1(I2)
         A(2) = A(2) + D1(I1)*S1(I1)
   45 CONTINUE
C
      DO 55 II=INZ(1)+1,INZ(2),2
         I1 = INDX(II)
         I2 = INDX(II+1)
         A(1) = A(1) - D2(I1)*S2(I2)
         A(2) = A(2) + D2(I1)*S2(I1)
   55 CONTINUE
C
      DO 66 II=INZ(2)+1,INZ(3),4
         I1 = INDX(II)
         I2 = INDX(II+1)
         I3 = INDX(II+2)
         I4 = INDX(II+3)
         A(3) = A(3) + D2(I1)*S2(I4)
         A(4) = A(4) + D2(I1)*(-S2(I2)-S2(I3))
         A(5) = A(5) + D2(I1)*S2(I1)
   66 CONTINUE
C
      DO 77 II=INZ(3)+1,INZ(4),3
         I1 = INDX(II)
         I2 = INDX(II+1)
         I3 = INDX(II+2)
         A(3) = A(3) + D2(I1)*S2(I3)
         A(4) = A(4) - 2.0D+00*D2(I1)*S2(I2)
         A(5) = A(5) + D2(I1)*S2(I1)
   77 CONTINUE
C
      DO 88 II=INZ(4)+1,INZ(5),3
         I1 = INDX(II)
         I2 = INDX(II+1)
         I3 = INDX(II+2)
         A(3) = A(3) + D2(I1)*S2(I3)
         A(4) = A(4) -2.0D+00*D2(I1)*S2(I2)
         A(5) = A(5) + D2(I1)*S2(I1)
   88 CONTINUE
C
      DO 99 II=INZ(5)+1,INZ(6),6
         I1 = INDX(II)
         I2 = INDX(II+1)
         I3 = INDX(II+2)
         I4 = INDX(II+3)
         I5 = INDX(II+4)
         I6 = INDX(II+5)
         A(6) = A(6) - D2(I1)*S2(I6)
         A(7) = A(7) + D2(I1)*(S2(I3)+2.0D+00*S2(I5))
         A(8) = A(8) + D2(I1)*(-2.0D+00*S2(I2)-S2(I4))
         A(9) = A(9) + D2(I1)*S2(I1)
   99 CONTINUE
C
      IN = INZ(6) + 1
      I1 = INDX(IN)
      I2 = INDX(IN+1)
      I3 = INDX(IN+2)
      I4 = INDX(IN+3)
      I5 = INDX(IN+4)
      I6 = INDX(IN+5)
C
      A(10) = D2(I1)*S2(I6)
      A(11) = -4.0D+00*D2(I1)*S2(I5)
      A(12) = D2(I1)*(2.0D+00*S2(I3)+4.0D+00*S2(I4))
      A(13) = -4.0D+00*D2(I1)*S2(I2)
      A(14) = D2(I1)*S2(I1)
C
C NOW FOR THE B AND THE C COEFFICIENTS
C
      B(1) = A(5) + A(14)
      B(2) = A(1) + A(8)
      B(3) = A(3) - A(5) + A(12) - 2.0D+00*A(14)
      B(4) = A(6) - A(8)
      B(5) = A(10) - A(12) + A(14)
      B(6) = A(2) + A(9)
      B(7) = A(4) + A(13)
      B(8) = A(7) - A(9)
      B(9) = A(11) - A(13)
C
      C(1) = B(7)
      C(2) = 2.0D+00*B(8) - B(6)
      C(3) = 3.0D+00*B(9) - 2.0D+00*B(7)
      C(4) = -3.0D+00*B(8)
      C(5) = -4.0D+00*B(9)
      C(6) = B(2)
      C(7) = 2.0D+00*B(3)
      C(8) = 3.0D+00*B(4)
      C(9) = 4.0D+00*B(5)
C
      RETURN
      END
C
C*MODULE MCJAC   *DECK MCIBV
      SUBROUTINE MCIBV(NCOR,IOR,A,B,C,ARGX,ARGY,IBV,ING,ING2,DE,FOUND)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL FOUND
      INTEGER PING
      DIMENSION ARGX(65),ARGY(65),IBV(4)
      DIMENSION B(9),C(9),A(9)
C
C  OKAY, THIS IS COMPLICATED.
C  WE HAVE A FUNCTION OF THETA USING THE
C  COEFFICIENTS IN B AND C, B ARE THE COEFFICIENTS
C  FOR THE ENERGY AND C ARE THE COEFFICIENTS FOR THE
C  GRADIENT.  THE FUNCTION IS GIVEN IN
C  THE FUNCTION CALL FUNC1.
C  ARGX(ING),ARGX(ING2),ARGY(ING),ARGY(ING2) ARE
C  SIN(THE1),SIN(THE2),COS(THE1),COS(THE2) FOR
C  THE1 AND THE2 SUCH THAT THE DERIVATIVE OF THE
C  CHANGE IN ENERGY, WRT ANGLE THETA, CHANGES
C  FROM  NEGATIVE TO POSITIVE AS THETA GOES FROM
C  THE1 -> THE2.  WHERE THETA CAN ONLY BE BETWEEN
C  -PI -> PI IN 64TH INTERVALS.  WHERE THIS SIGN
C  CHANGE IS OBSERVED MORE THAN ONCE, THE ONE
C  FOR WHICH THE CHANGE IN ENERGY IS THE LOWEST
C  IS TAKEN.  THIS REALLY IS HARD TO EXPLAIN, TRUST ME,
C  READ THE PAPER.
C
      FOUND = .FALSE.
      E1 = FUNC1(0.0D+00,1.0D+00,B)
      ICO = 0
C
      IF (IOR.LE.NCOR) GOTO 113
C
      D2 = FUNC1(ARGX(1),ARGY(1),C)
      DO 45 II=5,65,4
         D1 = D2
         D2 = FUNC1(ARGX(II),ARGY(II),C)
         IF (D1.GE.0.0D+00) GOTO 45
         IF (D2.GE.0.0D+00) THEN
            ICO = ICO + 1
            IBV(ICO) = II-4
         ENDIF
   45 CONTINUE
      IF (ICO.EQ.0) RETURN
      GOTO 213
C
  113 D2 = FUNC1(ARGX(17),ARGY(17),C)
      DO 145 II=25,45,4
         D1 = D2
         D2 = FUNC1(ARGX(II),ARGY(II),C)
         IF (D1.GE.0.0D+00) GOTO 145
         IF (D2.GE.0.0D+00) THEN
            ICO = ICO + 1
            IBV(ICO) = II-4
         ENDIF
  145 CONTINUE
      IF (ICO.EQ.0) RETURN
C
  213 CONTINUE
C
      DO 33 II=1,ICO
         IND = IBV(II)+2
         D3 = FUNC1(ARGX(IND),ARGY(IND),C)
         IF (D3.LT.0) IBV(II) = IND
   33 CONTINUE
C
      DO 22 II=1,ICO
         IND = IBV(II)+1
         D3 = FUNC1(ARGX(IND),ARGY(IND),C)
         IF (D3.LT.0) IBV(II) = IND
   22 CONTINUE
C
      PING = 1
      ISMALL = ABS(IBV(1)-33)
      DO 11 II=2,ICO
         IF (ABS(IBV(II)-33).LT.ISMALL) PING = II
   11 CONTINUE
C
      ING = IBV(PING)
      ING2 = ING + 1
      DE = A(PING) - E1
C
      FOUND=.TRUE.
C
      RETURN
      END
C
C*MODULE MCJAC   *DECK MCREF
      SUBROUTINE MCREF(B,C,XS0,XS1,X2,Y2,DE)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION B(9),C(9)
C
C    GIVEN AN INITIAL INTERVAL, XS0,XS1, FOR THE
C    MINIMUM OF DE=FUNCTION OF THETA,WHERE
C    XS0 = SIN(THE1),XS1 = SIN(THE2) THEN
C    THE EXACT MINIMUM IS FOUND AND RETURNED
C   AS X2 = SIN(THEMIN), Y2 = COS(THEMIN).
C
      E0 = FUNC1(0.0D+00,1.0D+00,B)
      MXIT = 15
      X0 = XS0
      X1 = XS0
      ISIGN = +1
      IF (XS0.LT.XS1) X1 = XS1
      IF (XS0.GT.XS1) THEN
         X0 = XS1
         ISIGN = -1
      ENDIF
C
      X02 = X0*X0
      X12 = X1*X1
      Y0 = SQRT(ABS(1.0D+00-X02))
      Y1 = SQRT(ABS(1.0D+00-X12))
      IF (Y0.LT.1.0D-05.OR.Y1.LT.1.0D-05) THEN
          X2 = XS0
          Y2 = SQRT(ABS(1.0D+00-XS0))
          DE = FUNC1(X2,Y2,B) - E0
          RETURN
      ENDIF
C
      Y0 = ISIGN*Y0
      Y1 = ISIGN*Y1
C
      FD0 = FUNC2(X0,Y0,C)
      FD1 = FUNC2(X1,Y1,C)
C
      DO 13 II=1,MXIT
         X2 = X0 - ((X1-X0)*FD0)/(FD1 - FD0)
         X22 = X2*X2
         Y2 = ISIGN*SQRT(ABS(1-X22))
         FD2 = FUNC2(X2,Y2,C)
C
C    CONVERGENCE CHECK
C
        IF (ABS(FD2).GT.1.0D-07) GOTO 66
        F2 = FUNC1(X2,Y2,B)
        DE = F2 - E0
        IF (ABS(X2).LT.1.0D-08.OR.DE.GT.0.0D+00) THEN
           X2 = 0.0D+00
           Y2 = 1.0D+00
           DE = 0.0D+00
        ENDIF
        RETURN
C
   66   CONTINUE
C
        IF (FD2.LT.0.0D+00)  THEN
           FD0 = FD2
           X0 = X2
           Y0 = Y2
        ELSE
           FD1 = FD2
           X1 = X2
           Y1 = Y2
        ENDIF
   13 CONTINUE
C      WRITE(6,*) 'NOT CONVERGED AFTER ',MXIT
C      STOP
      X2 = 0.0D+00
      Y2 = 1.0D+00
      DE = 0.0D+00
      RETURN
      END
C
C*MODULE MCJAC   *DECK FUNC1
      FUNCTION FUNC1(X,Y,C)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION C(9)
C
C  FUNCION CALLS FOR THE ENERGY AND GRADIENT
C  SEE THE PAPER FOR MORE DETAILS
C
      X2 = X*X
      X3 = X2*X
      X4 = X3*X
      FUNC1 = C(1)+C(2)*X+C(3)*X2+C(4)*X3+C(5)*X4
     *   + Y*(C(6)+C(7)*X+C(8)*X2+C(9)*X3)
      RETURN
      END
C
C*MODULE MCJAC   *DECK FUNC2
      FUNCTION FUNC2(X,Y,C)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION C(9)
C
C  FUNCTION CALL FOR THE GRADIENT, SEE THE
C  PAPER.
C
      X2 = X*X
      X3 = X2*X
      X4 = X3*X
      FUNC2 = (C(1)+C(2)*X+C(3)*X2+C(4)*X3+C(5)*X4)/Y +
     *     C(6)+C(7)*X+C(8)*X2+C(9)*X3
      RETURN
      END
C
C*MODULE MCJAC   *DECK MCROTAA
      SUBROUTINE MCROTAA(S1,S2,INDX,INZ,X,Y)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION S1(*),S2(*),INDX(*),INZ(*)
C
      X2 = X*X
      Y2 = Y*Y
      XY = X*Y
      T1 = XY*2.0D+00
      T2 = (Y2-X2)
      Y3 = Y2*Y
      X3 = X2*X
      XY2 = XY*Y
      T3 = 2.0D+00*XY2
      X2Y = XY*X
      T4 = 2.0D+00*X2Y
      T5 = (Y3 - X2Y)
      T6 = (X3 - XY2)
      X4 = X3*X
      Y4 = Y3*Y
      X2Y2 = X2*Y2
      XY3 = XY2*Y
      X3Y = X2Y*X
C
      U1 = 4.0D+00*XY3
      U2 = 2.0D+00*X2Y2
      U3 = 2.0D+00*U2
      U4 = 4.0D+00*X3Y
      U5 = Y4 - 3.0D+00*X2Y2
      U6 = X3Y - XY3
      U7 = 2.0D+00*U6
      U8 = 3.0D+00*X2Y2 - X4
C
      I1 = INDX(1)
      I2 = INDX(2)
      I3 = INDX(3)
      C1 = S1(I1)
      C2 = S1(I2)
      C3 = S1(I3)
      S1(I1) = Y2*C1 - T1*C2 + X2*C3
      S1(I2) = XY*(C1-C3) + T2*C2
      S1(I3) = X2*C1 + T1*C2 + Y2*C3
C
      DO 45 II=4,INZ(1),2
         I1 = INDX(II)
         I2 = INDX(II+1)
         C1 = S1(I1)
         C2 = S1(I2)
         S1(I1) = Y*C1 - X*C2
         S1(I2) = X*C1 + Y*C2
   45 CONTINUE
C
      DO 945 II=INZ(7)+1,INZ(8),2
         I1 = INDX(II)
         I2 = INDX(II+1)
         C1 = S1(I1)
         C2 = S1(I2)
         S1(I1) = Y*C1 - X*C2
         S1(I2) = X*C1 + Y*C2
  945 CONTINUE
C
      DO 55 II=INZ(1)+1,INZ(2),2
         I1 = INDX(II)
         I2 = INDX(II+1)
         C1 = S2(I1)
         C2 = S2(I2)
         S2(I1) = Y*C1 - X*C2
         S2(I2) = X*C1 + Y*C2
   55 CONTINUE
C
      DO 955 II=INZ(8)+1,INZ(9),2
         I1 = INDX(II)
         I2 = INDX(II+1)
         C1 = S2(I1)
         C2 = S2(I2)
         S2(I1) = Y*C1 - X*C2
         S2(I2) = X*C1 + Y*C2
  955 CONTINUE
C
      DO 66 II=INZ(2)+1,INZ(3),4
         I1 = INDX(II)
         I2 = INDX(II+1)
         I3 = INDX(II+2)
         I4 = INDX(II+3)
         C1 = S2(I1)
         C2 = S2(I2)
         C3 = S2(I3)
         C4 = S2(I4)
         S2(I1) = Y2*C1 - XY*(C2+C3) + X2*C4
         S2(I2) = XY*(C1-C4) + Y2*C2 - X2*C3
         S2(I3) = XY*(C1-C4) - X2*C2 + Y2*C3
         S2(I4) = X2*C1 + XY*(C2+C3) + Y2*C4
   66 CONTINUE
C
      DO 966 II=INZ(9)+1,INZ(10),4
         I1 = INDX(II)
         I2 = INDX(II+1)
         I3 = INDX(II+2)
         I4 = INDX(II+3)
         C1 = S2(I1)
         C2 = S2(I2)
         C3 = S2(I3)
         C4 = S2(I4)
         S2(I1) = Y2*C1 - XY*(C2+C3) + X2*C4
         S2(I2) = XY*(C1-C4) + Y2*C2 - X2*C3
         S2(I3) = XY*(C1-C4) - X2*C2 + Y2*C3
         S2(I4) = X2*C1 + XY*(C2+C3) + Y2*C4
  966 CONTINUE
C
      DO 77 II=INZ(3)+1,INZ(4),3
         I1 = INDX(II)
         I2 = INDX(II+1)
         I3 = INDX(II+2)
         C1 = S2(I1)
         C2 = S2(I2)
         C3 = S2(I3)
         S2(I1) = Y2*C1 - T1*C2 + X2*C3
         S2(I2) = XY*(C1-C3) + T2*C2
         S2(I3) = X2*C1 + T1*C2 + Y2*C3
   77 CONTINUE
C
      DO 977 II=INZ(10)+1,INZ(11),3
         I1 = INDX(II)
         I2 = INDX(II+1)
         I3 = INDX(II+2)
         C1 = S2(I1)
         C2 = S2(I2)
         C3 = S2(I3)
         S2(I1) = Y2*C1 - T1*C2 + X2*C3
         S2(I2) = XY*(C1-C3) + T2*C2
         S2(I3) = X2*C1 + T1*C2 + Y2*C3
  977 CONTINUE
C
      DO 88 II=INZ(4)+1,INZ(5),3
         I1 = INDX(II)
         I2 = INDX(II+1)
         I3 = INDX(II+2)
         C1 = S2(I1)
         C2 = S2(I2)
         C3 = S2(I3)
         S2(I1) = Y2*C1 - T1*C2 + X2*C3
         S2(I2) = XY*(C1-C3) + T2*C2
         S2(I3) = X2*C1 + T1*C2 + Y2*C3
   88 CONTINUE
C
      DO 988 II=INZ(11)+1,INZ(12),3
         I1 = INDX(II)
         I2 = INDX(II+1)
         I3 = INDX(II+2)
         C1 = S2(I1)
         C2 = S2(I2)
         C3 = S2(I3)
         S2(I1) = Y2*C1 - T1*C2 + X2*C3
         S2(I2) = XY*(C1-C3) + T2*C2
         S2(I3) = X2*C1 + T1*C2 + Y2*C3
  988 CONTINUE
C
C
      DO 99 II=INZ(5)+1,INZ(6),6
         I1 = INDX(II)
         I2 = INDX(II+1)
         I3 = INDX(II+2)
         I4 = INDX(II+3)
         I5 = INDX(II+4)
         I6 = INDX(II+5)
         C1 = S2(I1)
         C2 = S2(I2)
         C3 = S2(I3)
         C4 = S2(I4)
         C5 = S2(I5)
         C6 = S2(I6)
         S2(I1) = Y3*C1-XY2*C4-T3*C2+X2Y*C3+T4*C5-X3*C6
         S2(I2) = XY2*C1-X2Y*C4+T5*C2-XY2*C3+T6*C5+X2Y*C6
         S2(I3) = X2Y*C1-X3*C4+T3*C2+Y3*C3-T4*C5-XY2*C6
         S2(I4) = XY2*C1+Y3*C4-T4*C2+X3*C3-T3*C5+X2Y*C6
         S2(I5) = X2Y*C1+XY2*C4-T6*C2-X2Y*C3+T5*C5-XY2*C6
         S2(I6) = X3*C1+X2Y*C4+T4*C2+XY2*C3+T3*C5+Y3*C6
   99 CONTINUE
C
      DO 999 II=INZ(12)+1,INZ(13),6
         I1 = INDX(II)
         I2 = INDX(II+1)
         I3 = INDX(II+2)
         I4 = INDX(II+3)
         I5 = INDX(II+4)
         I6 = INDX(II+5)
         C1 = S2(I1)
         C2 = S2(I2)
         C3 = S2(I3)
         C4 = S2(I4)
         C5 = S2(I5)
         C6 = S2(I6)
         S2(I1) = Y3*C1-XY2*C4-T3*C2+X2Y*C3+T4*C5-X3*C6
         S2(I2) = XY2*C1-X2Y*C4+T5*C2-XY2*C3+T6*C5+X2Y*C6
         S2(I3) = X2Y*C1-X3*C4+T3*C2+Y3*C3-T4*C5-XY2*C6
         S2(I4) = XY2*C1+Y3*C4-T4*C2+X3*C3-T3*C5+X2Y*C6
         S2(I5) = X2Y*C1+XY2*C4-T6*C2-X2Y*C3+T5*C5-XY2*C6
         S2(I6) = X3*C1+X2Y*C4+T4*C2+XY2*C3+T3*C5+Y3*C6
  999 CONTINUE
C
      IN = INZ(6) + 1
      I1 = INDX(IN)
      I2 = INDX(IN+1)
      I3 = INDX(IN+2)
      I4 = INDX(IN+3)
      I5 = INDX(IN+4)
      I6 = INDX(IN+5)
      C1 = S2(I1)
      C2 = S2(I2)
      C3 = S2(I3)
      C4 = S2(I4)
      C5 = S2(I5)
      C6 = S2(I6)
      S2(I1) = Y4*C1-U1*C2+U2*C3+U3*C4-U4*C5+X4*C6
      S2(I2) = XY3*C1+U5*C2+U6*C3+U7*C4+U8*C5-X3Y*C6
      S2(I3) = X2Y2*C1-U7*C2+(X4+Y4)*C3-U3*C4+U7*C5+X2Y2*C6
      S2(I4) = X2Y2*C1-U7*C2-U2*C3+(X4+Y4-U2)*C4+U7*C5+X2Y2*C6
      S2(I5) = X3Y*C1+U8*C2-U6*C3-U7*C4+U5*C5-XY3*C6
      S2(I6) = X4*C1+U4*C2+U2*C3+U3*C4+U1*C5+Y4*C6
C
      RETURN
      END
C
C*MODULE MCJAC   *DECK GETENY
      SUBROUTINE GETENY(S1,S2,D1,D2,K2,K4,E1,E2)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION S1(K2),D1(K2),S2(K4),D2(K4)
C
      E1 = 0.0D+00
      E2 = 0.0D+00
      DO II=1,K2
         E1 = E1 + S1(II)*D1(II)
      ENDDO
      DO II=1,K4
         E2 = E2 + S2(II)*D2(II)
      ENDDO
      RETURN
      END
C
C*MODULE MCJAC   *DECK ADJTR
      SUBROUTINE ADJTR(I,J,NT,A,B,T)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION T(NT,NT)
C
C   BASICALL DEFINE NEW ORBITALS WHERE
C   I = A*I - B*J
C   J = B*I + A*J
C
C   AND SO JUST MULTIPLY INTO A TRANSFORMATION
C   MATRIX T.
C
      DO 13 K=1,NT
         TKI=T(K,I)
         TKJ=T(K,J)
         T(K,I) = TKI*A - TKJ*B
         T(K,J) = TKI*B + TKJ*A
   13 CONTINUE
C
      RETURN
      END
