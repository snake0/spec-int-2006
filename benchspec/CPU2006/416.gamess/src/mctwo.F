C 12 DEC 02 - MWS - PNTNHES: ENSURE ENTIRE INDEX ARRAY IS FILLED
C 22 MAY 02 - GDF - FORM LAGRANGIAN AND HESSIAN FROM DDI INTEGRALS
C 12 NOV 98 - GDF - CHANGE BIT PACKING TO ISHIFT, DROP ALL M3 CODE
C  8 JAN 97 - GMC - MC1COU,M1EGH,M1EXC,M1LGR: CHANGES TO DROP MCC ORBS
C 13 JUN 96 - MWS - REMOVE FTNCHEK WARNINGS
C 10 NOV 94 - MWS - REMOVE FTNCHEK WARNINGS
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C 13 DEC 93 - TLW - M1EGHR: DON'T ASSUME TRANS. INTS ARE IN ANY ORDER
C 11 JAN 92 - TLW - MAKE READ PARALLEL
C 10 JAN 92 - TLW - CHANGE REWINDS TO CALL SEQREW;CALL ABRT BEFORE STOP
C  7 JAN 92 - TLW - MAKE WRITES PARALLEL; ADD COMMON PAR
C  7 OCT 91 - MWS - CHANGE UNIX BYTE UNPACKING
C  3 JUN 90 - JAM - CHANGE VAX BYTE PACKING TO FULL 8 BITS
C 21 DEC 89 - STE - ALPHABETIZE THE ROUTINES
C  3 JAN 89 - MWS - FIX VAX,CEL,CRY,FPS CODE IN M3SRT (IEH, NOT NEH)
C 12 OCT 88 - MWS - IMPLEMENT MICHEL'S HONDO7 MCSCF PROGRAM IN GAMESS,
C                   THIS FILE FORMS THE 2E- LAGRANGIAN AND HESSIAN
C
C*MODULE MCTWO   *DECK M1COU
      SUBROUTINE M1COU(NEH,EH,X,NFTG,G,GIJ,IX,
     *                 NIA,IA,NORB,IC,NINTMX,MI,MA,MAB,NCORBS,DROPC)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL IANDJ,KANDL,SAME,DROPC
C
      DIMENSION EH(NEH)
      DIMENSION IA(NIA),IC(NORB,*)
      DIMENSION X(MAB,*),G(NINTMX),IX(NINTMX),GIJ(*)
C
      COMMON /PCKLAB/ LABSIZ
C
      PARAMETER (GTOL=1.0D-08, ZERO=0.0D+00)
C
      DO 10 I=1,NIA
   10 IA(I)=(I*(I-1))/2
C
      IF (NCORBS.EQ.0 .OR. .NOT. DROPC) GO TO 80
C
C     CONTRIBUTION TO H(RI,SJ) FROM I J CORE
C
C     EH(RI,SJ) = EH(RI,SJ)  - 2 <RS|IJ>
C
      DO 35 M = 1,MA
C
       DO 30 I=1,NCORBS
       MIROT=IC(M+MI,I)
       IF(MIROT.EQ.0) GO TO 30
C
        DO 20 N = 1,MA
        MN=IA(MAX0(M,N))+MIN0(M,N)
C
         DO 15 J=1,NCORBS
         NJROT=IC(N+MI,J)
         IF(NJROT.EQ.0) GO TO 15
C
         IJ=IA(MAX0(I,J)) + MIN0(I,J)
         XC = X(MN,IJ)
         IF(XC.EQ.ZERO) GO TO 15
         XC = XC + XC
C
         NRX = IA(MAX0(MIROT,NJROT))+MIN0(MIROT,NJROT)
         EH(NRX) = EH(NRX) - XC
C
  15     CONTINUE
  20    CONTINUE
  30   CONTINUE
  35  CONTINUE
C
C
C     CONTRIBUTION TO H(RI,SJ) FROM I CORE J VALENCE
C
C     EH(RI,SJ) = EH(RI,SJ) - SUM GJL * ( <RS|IL> )
C                             (OVER L - VALENCE)
C
      DO 60 M = 1,MA
C
       DO 55 I = 1,NCORBS
       MIROT=IC(M+MI,I)
       IF(MIROT.EQ.0) GO TO 55
C
       DO 50 N = 1,MA
       MN=IA(MAX0(M,N))+MIN0(M,N)
C
        DO 45 L = NCORBS+1,MI
        IL=IA(L)+I
        XC = X(MN,IL)
        IF(XC.EQ.ZERO) GO TO 45
C
          DO 40 J = NCORBS+1,MI
          NJROT=IC(N+MI,J)
          IF(NJROT.EQ.0) GO TO 40
          J1=MAX0(J,L)-NCORBS
          L1=MIN0(J,L)-NCORBS
          NGJL = IA(J1)+L1
          XGJL = GIJ(NGJL)
          IF(XGJL.EQ.ZERO) GO TO 40
         FF = XGJL * (XC + XC)
         NRX = IA(MAX0(MIROT,NJROT))+MIN0(MIROT,NJROT)
         EH(NRX) = EH(NRX) - FF
C
  40   CONTINUE
  45   CONTINUE
  50  CONTINUE
  55  CONTINUE
  60  CONTINUE
C
  80  CONTINUE
C
C     ----- READ IN -DM2- FROM -NFTG- -----
C
      CALL SEQREW(NFTG)
  100 CALL PREAD(NFTG,G,IX,NG,NINTMX)
      IF(NG.EQ.0) GO TO 4100
      MGIJKL=IABS(NG)
      IF(MGIJKL.GT.NINTMX) THEN
         CALL ABRT
         STOP
      END IF
      DO 4000 MG=1,MGIJKL
      GGIJKL=G(MG)
      IF( ABS(GGIJKL).LT.GTOL) GO TO 4000
C
                       NPACK = MG
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
                       IGIJKL = IPACK
                       JGIJKL = JPACK
                       KGIJKL = KPACK
                       LGIJKL = LPACK
C
C     -----  I AND J VALENCE -------
C
      IANDJ=IGIJKL.EQ.JGIJKL
      KANDL=KGIJKL.EQ.LGIJKL
      SAME =IGIJKL.EQ.KGIJKL.AND.JGIJKL.EQ.LGIJKL
C
      IF (DROPC) THEN
         IGIJKL = IGIJKL + NCORBS
         JGIJKL = JGIJKL + NCORBS
         KGIJKL = KGIJKL + NCORBS
         LGIJKL = LGIJKL + NCORBS
      END IF
C
C     ----- COULOMB CONTRIBUTION TO HESSIAN -----
C
      GAB=GGIJKL
      IF(.NOT.IANDJ) GAB=GAB+GAB
      NAB=IA(IGIJKL)+JGIJKL
      GCD=GGIJKL
      IF(.NOT.KANDL) GCD=GCD+GCD
      NCD=IA(KGIJKL)+LGIJKL
C
      IG=IGIJKL
      JG=JGIJKL
      KL=NCD
      GMINJ=GCD
      NPASS=1
 2000 CONTINUE
C
      DO 2100 M=1,MA
      GM=GMINJ
      MIROT=IC(M+MI,IG)
      IF(MIROT.EQ.0) GO TO 2100
      MAXN=M
      IF(JG.GT.IG) MAXN=M-1
      IF(MAXN.EQ.0) GO TO 2100
      DO 2050 N=1,MAXN
      GIJKL=GM
      NJROT=IC(N+MI,JG)
      IF(NJROT.EQ.0) GO TO 2050
      MN=IA(M)+N
      XMNKL=X(MN,KL)
      IF(XMNKL.EQ.ZERO) GO TO 2050
      IF(N.NE.M.OR.JG.NE.IG) GIJKL=GIJKL+GIJKL
      MIH=MAX(MIROT,NJROT)
      NJH=MIN(MIROT,NJROT)
      MINJH=IA(MIH)+NJH
      EH(MINJH)=EH(MINJH)+ GIJKL*XMNKL
 2050 CONTINUE
 2100 CONTINUE
C
 2200 NPASS=NPASS+1
      GO TO (2300,2210,2220,2230,2300),NPASS
 2210 IF(IANDJ) GO TO 2200
      IG=JGIJKL
      JG=IGIJKL
      GO TO 2000
 2220 IF(SAME) GO TO 2300
      IG=KGIJKL
      JG=LGIJKL
      KL=NAB
      GMINJ=GAB
      GO TO 2000
 2230 IF(KANDL) GO TO 2300
      IG=LGIJKL
      JG=KGIJKL
      GO TO 2000
 2300 CONTINUE
C
 4000 CONTINUE
      IF(NG.GT.0) GO TO 100
 4100 CONTINUE
      RETURN
      END
C*MODULE MCTWO   *DECK M1COUR
      SUBROUTINE M1COUR(NFTI,OX,X,IX,IA,NINTMX,MI,MA,MIJ,MAB,NOSQUR)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SKIP
C
      DIMENSION OX(MAB,*),X(NINTMX),IX(NINTMX),IA(*)
C
      COMMON /PCKLAB/ LABSIZ
C
C     ----- READ (VIR,VIR//OCC,OCC) INTEGRALS FOR M1COU -----
C
      NIA=MAX(MI,MA)
      DO 10 I=1,NIA
   10 IA(I)=(I*(I-1))/2
      CALL VCLR(OX,1,MAB*MIJ)
C
C     ----- READ IN INTEGRALS FROM -NFTI- -----
C
  110 CALL PREAD(NFTI,X,IX,NX,NINTMX)
      IF(NX.EQ.0) GO TO 200
      MX=IABS(NX)
      IF(MX.GT.NINTMX) THEN
         CALL ABRT
         STOP
      END IF
      DO 120 M=1,MX
C
                       NPACK = M
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
                       K = IPACK
                       L = JPACK
                       I = KPACK
                       J = LPACK
C
         IF(NOSQUR.EQ.1) THEN
            IDUM=I
            JDUM=J
            I=K
            J=L
            K=IDUM
            L=JDUM
         END IF
         SKIP=(I.LE.MI).OR.(J.LE.MI).OR.(K.GT.MI).OR.(L.GT.MI)
         IF(SKIP) GO TO 120
         IJ=IA(I-MI)+(J-MI)
         KL=IA(K)+L
         OX(IJ,KL)=X(M)
  120 CONTINUE
      IF(NX.GT.0) GO TO 110
  200 CONTINUE
      RETURN
      END
C*MODULE MCTWO   *DECK M1EGH
      SUBROUTINE M1EGH(NEG,NEH,EG,EH,X,NFTG,G,GIJ,IX,
     *                 NIA,IA,NORB,IB,IC,NINTMX,MI,NCORBS,DROPC)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL IANDJ,KANDL,SAME,DROPC
C
      DIMENSION EG(NEG),EH(NEH)
      DIMENSION IA(NIA),IB(NORB),IC(NORB,*)
      DIMENSION X(*),G(NINTMX),IX(NINTMX),GIJ(*)
C
      COMMON /PCKLAB/ LABSIZ
C
      PARAMETER (GTOL=1.0D-08, ZERO=0.0D+00)
C
      DO 10 I=1,NIA
   10 IA(I)=(I*(I-1))/2
C
      IF (.NOT. DROPC) THEN
         CALL VCLR(EG,1,NEG)
         CALL VCLR(EH,1,NEH)
      END IF
C
      IF (NCORBS.EQ.0 .OR. .NOT. DROPC) GO TO 80
C
C     CONTRIBUTION TO H(RI,SJ) FROM I J CORE
C
C     EH(RI,SJ) = EH(RI,SJ) + 8 <RI|SJ> - 2 <RJ|SI> - 2 <RS|IJ>
C
      DO 35 M = NCORBS+1,MI
      M1 = IA(M)
C
       DO 30 I=1,NCORBS
       MIROT=IC(M,I)
       IF(MIROT.EQ.0) GO TO 30
       MII= M1 + I
C
        DO 25 N = NCORBS+1,M
        N1 = IA(N)
        MN=IA(M) + N
        NII= N1 + I
C
          DO 20 J=1,NCORBS
          NJROT=IC(N,J)
          IF(NJROT.EQ.0) GO TO 20
          IJ=IA(MAX0(I,J)) + MIN0(I,J)
C
          NXC = IA(MN) + IJ
          XNXC = X(NXC)
          NJJ= N1 + J
          NEC1=IA(MAX0(MII,NJJ))+ MIN0(NJJ,MII)
          XNEC1=X(NEC1)
          MJJ= M1 + J
          NEC2=IA(MAX0(MJJ,NII)) + MIN0(MJJ,NII)
          XNEC2=X(NEC2)
C
          XC  = (XNEC1 + XNEC1)+(XNEC1 + XNEC1) - XNEC2 -XNXC
          IF (XC.EQ.ZERO) GO TO 20
          XC = XC + XC
          IF(M.NE.N) XC = XC+XC
          NRX = IA(MAX0(MIROT,NJROT))+MIN0(MIROT,NJROT)
          EH(NRX) = EH(NRX) + XC
C
 20      CONTINUE
 25     CONTINUE
 30    CONTINUE
 35   CONTINUE
C
C     CONTRIBUTION TO H(RI,SJ) FROM I CORE J VALENCE
C
C     EH(RI,SJ) = EH(RI,SJ) + SUM GJL * ( 4 <RI|SL> - <RL|SI> - <RS|IL>)
C                             (OVER L - VALENCE)
C
      DO 65 M = 1,MI
      MINM=1
      IF(M.LE.NCORBS) MINM=NCORBS+1
C
       DO 60 I=1,NCORBS
       MIROT=IC(M,I)
       IF(MIROT.EQ.0) GO TO 60
C
        DO 50 N = MINM,MI
        MN=IA(MAX0(M,N))+MIN0(M,N)
C
        DO 40 L = NCORBS+1,MI
        IL=IA(L)+I
        NXC = IA(MAX0(MN,IL)) + MIN0(MN,IL)
        XNXC=X(NXC)
        MMM = IA(MAX0(M,I))+ MIN0(M,I)
        NNN = IA(MAX0(N,L)) + MIN0(N,L)
        NEC1=IA(MAX0(MMM,NNN)) + MIN0(MMM,NNN)
        XNEC1=X(NEC1)
        MMM = IA(MAX0(M,L))+ MIN0(M,L)
        NNN = IA(MAX0(N,I))+ MIN0(N,I)
        NEC2=IA(MAX0(MMM,NNN)) + MIN0(MMM,NNN)
        XNEC2=X(NEC2)
         XC  = (XNEC1+XNEC1)+(XNEC1+XNEC1) -XNEC2 - XNXC
C
         DO 40 J = NCORBS+1,MI
          NJROT=IC(N,J)
          IF(NJROT.EQ.0) GO TO 40
          J1=MAX0(J,L)-NCORBS
          L1=MIN0(J,L)-NCORBS
          NGJL = IA(J1) + L1
          XGJL = GIJ(NGJL)
         IF(XGJL.EQ.ZERO) GO TO 40
C
          FF =  XGJL * (XC + XC )
          IF (M.LT.I) FF = -FF
          IF (N.LT.J) FF = -FF
         NRX = IA(MAX0(MIROT,NJROT)) + MIN0(MIROT,NJROT)
         EH(NRX) = EH(NRX) + FF
  40    CONTINUE
  50   CONTINUE
  60  CONTINUE
  65  CONTINUE
C
  80  CONTINUE
C
C     ----- READ IN -DM2- FROM -NFTG- -----
C
      CALL SEQREW(NFTG)
  100 CALL PREAD(NFTG,G,IX,NG,NINTMX)
      IF(NG.EQ.0) GO TO 4100
      MGIJKL=IABS(NG)
      IF(MGIJKL.GT.NINTMX) THEN
         CALL ABRT
         STOP
      END IF
      DO 4000 MG=1,MGIJKL
      GGIJKL=G(MG)
      IF( ABS(GGIJKL).LT.GTOL) GO TO 4000
C
                       NPACK = MG
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
                       IGIJKL = IPACK
                       JGIJKL = JPACK
                       KGIJKL = KPACK
                       LGIJKL = LPACK
C
C
C     ----- I AND J VALENCE -------
C
      IANDJ=IGIJKL.EQ.JGIJKL
      KANDL=KGIJKL.EQ.LGIJKL
      SAME =IGIJKL.EQ.KGIJKL.AND.JGIJKL.EQ.LGIJKL
C
      IF (DROPC) THEN
         IGIJKL = IGIJKL + NCORBS
         JGIJKL = JGIJKL + NCORBS
         KGIJKL = KGIJKL + NCORBS
         LGIJKL = LGIJKL + NCORBS
      END IF
C
C     ----- LAGRANGIAN CONTRIBUTION -----
C
      GAB=GGIJKL
      IF(.NOT.IANDJ) GAB=GAB+GAB
      NAB=IA(IGIJKL)+JGIJKL
      GCD=GGIJKL
      IF(.NOT.KANDL) GCD=GCD+GCD
      NCD=IA(KGIJKL)+LGIJKL
C
      IG=IGIJKL
      JG=JGIJKL
      KG=KGIJKL
      LG=LGIJKL
      KL=NCD
      GIJKL=GCD
      NPASS=1
 1000 CONTINUE
C
C     ----- COMPUTE CONTRIBUTION TO THE LAGRANGIAN -----
C
      DO 1100 M=1,MI
         MM=MAX(M,JG)
         JJ=MIN(M,JG)
         MJ=IA(MM)+JJ
         MJX=MAX(MJ,KL)
         KLX=MIN(MJ,KL)
         MJKLX=IA(MJX)+KLX
         XMJKL=X(MJKLX)
         IF(XMJKL.EQ.ZERO) GO TO 1100
         MILGR=IB(M)+IG
         EG(MILGR)=EG(MILGR)+ GIJKL*XMJKL
 1100 CONTINUE
C
C     ----- CONTRIBUTIONS TO ORBITAL HESSIAN -----
C
 1200 NPASS=NPASS+1
      GO TO (1300,1210,1220,1230,1300),NPASS
 1210 IF(IANDJ) GO TO 1200
      IG=JGIJKL
      JG=IGIJKL
      GO TO 1000
 1220 IF(SAME) GO TO 1300
      IG=KGIJKL
      JG=LGIJKL
      KG=IGIJKL
      LG=JGIJKL
      KL=NAB
      GIJKL=GAB
      GO TO 1000
 1230 IF(KANDL) GO TO 1300
      IG=LGIJKL
      JG=KGIJKL
      GO TO 1000
 1300 CONTINUE
C
C     ----- COULOMB CONTRIBUTION TO HESSIAN -----
C
      IG=IGIJKL
      JG=JGIJKL
      KG=KGIJKL
      LG=LGIJKL
      KL=NCD
      GMINJ=GCD
      NPASS=1
 2000 CONTINUE
C
      DO 2100 M=1,MI
      GM=GMINJ
      IF(M -IG) 2010,2100,2020
 2010 GM=-GM
 2020 MIROT=IC(M,IG)
      IF(MIROT.EQ.0) GO TO 2100
      MAXN=M
      IF(JG.GT.IG) MAXN=M-1
      IF(MAXN.EQ.0) GO TO 2100
      DO 2050 N=1,MAXN
      GIJKL=GM
      IF(N -JG) 2030,2050,2040
 2030 GIJKL=-GIJKL
 2040 NJROT=IC(N,JG)
      IF(NJROT.EQ.0) GO TO 2050
      MN=IA(M)+N
      MNX=MAX(MN,KL)
      KLX=MIN(MN,KL)
      MNKLX=IA(MNX)+KLX
      XMNKL=X(MNKLX)
      IF(XMNKL.EQ.ZERO) GO TO 2050
      IF(N.NE.M.OR.JG.NE.IG) GIJKL=GIJKL+GIJKL
      MIH=MAX(MIROT,NJROT)
      NJH=MIN(MIROT,NJROT)
      MINJH=IA(MIH)+NJH
      EH(MINJH)=EH(MINJH)+ GIJKL*XMNKL
 2050 CONTINUE
 2100 CONTINUE
C
 2200 NPASS=NPASS+1
      GO TO (2300,2210,2220,2230,2300),NPASS
 2210 IF(IANDJ) GO TO 2200
      IG=JGIJKL
      JG=IGIJKL
      GO TO 2000
 2220 IF(SAME) GO TO 2300
      IG=KGIJKL
      JG=LGIJKL
      KG=IGIJKL
      LG=JGIJKL
      KL=NAB
      GMINJ=GAB
      GO TO 2000
 2230 IF(KANDL) GO TO 2300
      IG=LGIJKL
      JG=KGIJKL
      GO TO 2000
 2300 CONTINUE
C
C     ----- EXCHANGE CONTRIBUTION TO HESSIAN -----
C
      GDM2=GGIJKL+GGIJKL
      IG=IGIJKL
      JG=JGIJKL
      KG=KGIJKL
      LG=LGIJKL
      NPASS=1
 3000 CONTINUE
C
      DO 3100 M=1,MI
      GM=GDM2
      IF(M -IG) 3010,3100,3020
 3010 GM=-GM
 3020 MIROT=IC(M,IG)
      IF(MIROT.EQ.0) GO TO 3100
      MM=MAX(M,JG)
      JJ=MIN(M,JG)
      MJ=IA(MM)+JJ
      MAXN=M
      IF(KG.GT.IG) MAXN=M-1
      IF(MAXN.EQ.0) GO TO 3100
      DO 3050 N=1,MAXN
      GIJKL=GM
      IF(N -KG) 3030,3050,3040
 3030 GIJKL=-GIJKL
 3040 NKROT=IC(N,KG)
      IF(NKROT.EQ.0) GO TO 3050
      NN=MAX(N,LG)
      LL=MIN(N,LG)
      NL=IA(NN)+LL
      MJX=MAX(MJ,NL)
      NLX=MIN(MJ,NL)
      MJNLX=IA(MJX)+NLX
      XMJNL=X(MJNLX)
      IF(XMJNL.EQ.ZERO) GO TO 3050
      IF(N.NE.M.OR.KG.NE.IG) GIJKL=GIJKL+GIJKL
      MIH=MAX(MIROT,NKROT)
      NKH=MIN(MIROT,NKROT)
      MINKH=IA(MIH)+NKH
      EH(MINKH)=EH(MINKH)+ GIJKL*XMJNL
 3050 CONTINUE
 3100 CONTINUE
C
 3200 NPASS=NPASS+1
      GO TO (3300,3210,3220,3230,3240,3250,3260,3270,3300),NPASS
 3210 IF(IANDJ) GO TO 3200
      IG=JGIJKL
      JG=IGIJKL
      GO TO 3000
 3220 IF(KANDL) GO TO 3200
      IG=IGIJKL
      JG=JGIJKL
      KG=LGIJKL
      LG=KGIJKL
      GO TO 3000
 3230 IF(IANDJ.OR.KANDL) GO TO 3200
      IG=JGIJKL
      JG=IGIJKL
      GO TO 3000
 3240 IF(SAME) GO TO 3300
      IG=KGIJKL
      JG=LGIJKL
      KG=IGIJKL
      LG=JGIJKL
      GO TO 3000
 3250 IF(IANDJ) GO TO 3200
      KG=JGIJKL
      LG=IGIJKL
      GO TO 3000
 3260 IF(KANDL) GO TO 3200
      IG=LGIJKL
      JG=KGIJKL
      KG=IGIJKL
      LG=JGIJKL
      GO TO 3000
 3270 IF(IANDJ.OR.KANDL) GO TO 3300
      KG=JGIJKL
      LG=IGIJKL
      GO TO 3000
 3300 CONTINUE
C
 4000 CONTINUE
      IF(NG.GT.0) GO TO 100
 4100 CONTINUE
      RETURN
      END
C*MODULE MCTWO   *DECK M1EGHR
      SUBROUTINE M1EGHR(NFTI,OX,X,IX,IA,NINTMX,MI,NOSQUR)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION OX(*),X(NINTMX),IX(NINTMX),IA(*)
C
      COMMON /PCKLAB/ LABSIZ
C
C     ----- READ (OCC,OCC//OCC,OCC) INTEGRALS FOR M1EGH -----
C     INTEGRALS FROM THE FULL TRANSFORMATION ARE EXPECTED TO
C     BE IN REVERSE CANONICAL ORDER, INTEGRALS FROM THE PARTIAL
C     TRANSFORMATION IN NORMAL CANONICAL ORDER.  THE FORM OF
C     THE SORTED AO INTEGRAL FILE DISTINQUISHES THESE TWO CASES.
C
      M2=(MI*(MI+1))/2
      M4=(M2*(M2+1))/2
      DO 10 I=1,M2
   10 IA(I)=(I*(I-1))/2
      CALL VCLR(OX,1,M4)
C
C     ----- READ IN INTEGRALS FROM -NFTI- -----
C
  110 CALL PREAD(NFTI,X,IX,NX,NINTMX)
      IF(NX.EQ.0) GO TO 200
      MX=IABS(NX)
      IF(MX.GT.NINTMX) CALL ABRT
      DO 120 M=1,MX
C
                       NPACK = M
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
                       K = IPACK
                       L = JPACK
                       I = KPACK
                       J = LPACK
C
         IF(NOSQUR.EQ.1) THEN
            IDUM=I
            JDUM=J
            I=K
            J=L
            K=IDUM
            L=JDUM
            IF(I.GT.MI) GO TO 200
         ELSE
            IF(K.GT.MI) GO TO 120
            IF(I.GT.MI) GO TO 120
         END IF
         IJ=IA(I)+J
         KL=IA(K)+L
         IJKL=IA(IJ)+KL
         OX(IJKL)=X(M)
  120 CONTINUE
      IF(NX.GT.0) GO TO 110
C
  200 CONTINUE
      RETURN
      END
C*MODULE MCTWO   *DECK M1EXC
      SUBROUTINE M1EXC(NEH,EH,X,NFTG,G,GIJ,IX,
     *                 NIA,IA,NORB,IC,NINTMX,MI,MA,NCORBS,DROPC)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL IANDJ,KANDL,SAME,DROPC
C
      DIMENSION EH(NEH),IA(NIA),IC(NORB,*),X(*)
      DIMENSION G(NINTMX),IX(NINTMX),GIJ(*)
C
      COMMON /PCKLAB/ LABSIZ
C
      PARAMETER (GTOL=1.0D-08, ZERO=0.0D+00)
C
      DO 10 I=1,NIA
   10 IA(I)=(I*I-I)/2
C
      IF (NCORBS.EQ.0 .OR. .NOT. DROPC) GO TO 80
C
C     CONTRIBUTION TO H(RI,SJ) FROM I J CORE
C
C     EH(RI,SJ) = EH(RI,SJ) + 8 <RI|SJ> - 2 <RJ|SI>
C
      DO 35 M = 1,MA
      M1 = (M-1)*MI
C
      DO 30 I=1,NCORBS
       MIROT=IC(M+MI,I)
       IF(MIROT.EQ.0) GO TO 30
       MMI = M1 + I
C
       DO 20 N = 1,MA
       N1 = (N-1)*MI
       NNI = N1 + I
C
       DO 15 J=1,NCORBS
       NJROT=IC(N+MI,J)
       IF(NJROT.EQ.0) GO TO 15
C
       NNJ = N1 + J
       NEC1=IA(MAX0(MMI,NNJ)) + MIN0(MMI,NNJ)
       MMJ = M1 + J
       NEC2=IA(MAX0(MMJ,NNI))+MIN0(MMJ,NNI)
C
C      ----- EXCH -----
C
       XC=(X(NEC1) + X(NEC1))+(X(NEC1)+X(NEC1))-X(NEC2)
       IF(XC.EQ.ZERO) GO TO 15
       XC = XC + XC
       NRX = IA(MAX0(MIROT,NJROT))+MIN0(MIROT,NJROT)
       EH(NRX) = EH(NRX) + XC
C
 15   CONTINUE
 20   CONTINUE
 30   CONTINUE
 35   CONTINUE
C
C     ----- CONTRIBUTION TO H(RI,SJ) FROM I CORE J VALENCE -----
C
      DO 65 M = 1,MA
      M1 = (M-1)*MI
C
       DO 60 I=1,NCORBS
       MIROT=IC(M+MI,I)
       IF(MIROT.EQ.0) GO TO 60
       MMI = M1 + I
C
        DO 50 N = 1,MA
C       MN=IA(MAX0(M,N))+MIN0(M,N)
        N1 = (N-1)*MI
        NNI = N1 + I
C
         DO 45 L = NCORBS+1,MI
         NNL = N1 + L
         NEC1=IA(MAX0(MMI,NNL))+MIN0(MMI,NNL)
         MML = M1 + L
         NEC2=IA(MAX0(MML,NNI))+MIN0(MML,NNI)
         XC1 = (X(NEC1)+X(NEC1))+(X(NEC1)+X(NEC1)) - X(NEC2)
         IF(XC1.EQ.ZERO) GO TO 45
C
          DO 40 J = NCORBS+1,MI
          NJROT=IC(N+MI,J)
          IF(NJROT.EQ.0) GO TO 40
          J1=MAX0(J,L)-NCORBS
          L1=MIN0(J,L)-NCORBS
          NGJL = IA(J1)+L1
          XGJL = GIJ(NGJL)
          IF(XGJL.EQ.ZERO) GO TO 40
C
C    EH(RI,SJ) = EH(RI,SJ) + SUM GJL * ( 4 <RI|SL> - <RL|SI> )
C                             (OVER L - VALENCE)
C
           FF = XGJL * (XC1+XC1)
           NRX = IA(MAX0(MIROT,NJROT))+MIN0(MIROT,NJROT)
           EH(NRX) = EH(NRX) + FF
C
  40      CONTINUE
  45     CONTINUE
  50    CONTINUE
  60   CONTINUE
  65  CONTINUE
C
  80  CONTINUE
C
C     ----- READ IN -DM2- FROM -NFTG- -----
C
      CALL SEQREW(NFTG)
  100 CALL PREAD(NFTG,G,IX,NG,NINTMX)
      IF(NG.EQ.0) GO TO 4100
      MGIJKL=IABS(NG)
      IF(MGIJKL.GT.NINTMX) THEN
         CALL ABRT
         STOP
      END IF
      DO 4000 MG=1,MGIJKL
      GGIJKL=G(MG)
      IF( ABS(GGIJKL).LT.GTOL) GO TO 4000
C
                       NPACK = MG
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
                       IGIJKL = IPACK
                       JGIJKL = JPACK
                       KGIJKL = KPACK
                       LGIJKL = LPACK
C
C     ----- I AND J VALENCE -------
C
      IANDJ=IGIJKL.EQ.JGIJKL
      KANDL=KGIJKL.EQ.LGIJKL
      SAME =IGIJKL.EQ.KGIJKL.AND.JGIJKL.EQ.LGIJKL
C
      IF (DROPC) THEN
         IGIJKL = IGIJKL + NCORBS
         JGIJKL = JGIJKL + NCORBS
         KGIJKL = KGIJKL + NCORBS
         LGIJKL = LGIJKL + NCORBS
      END IF
C
C     ----- EXCHANGE CONTRIBUTION TO HESSIAN -----
C
      GDM2=GGIJKL+GGIJKL
      IG=IGIJKL
      JG=JGIJKL
      KG=KGIJKL
      LG=LGIJKL
      NPASS=1
 3000 CONTINUE
C
      DO 3100 M=1,MA
      GM=GDM2
      MIROT=IC(M+MI,IG)
      IF(MIROT.EQ.0) GO TO 3100
      MJ=(M-1)*MI+JG
      MAXN=M
      IF(KG.GT.IG) MAXN=M-1
      IF(MAXN.EQ.0) GO TO 3100
      DO 3050 N=1,MAXN
      GIJKL=GM
      NKROT=IC(N+MI,KG)
      IF(NKROT.EQ.0) GO TO 3050
      NL=(N-1)*MI+LG
      MJX=MAX(MJ,NL)
      NLX=MIN(MJ,NL)
      MJNLX=IA(MJX)+NLX
      XMJNL=X(MJNLX)
      IF(XMJNL.EQ.ZERO) GO TO 3050
      IF(N.NE.M.OR.KG.NE.IG) GIJKL=GIJKL+GIJKL
      MIH=MAX(MIROT,NKROT)
      NKH=MIN(MIROT,NKROT)
      MINKH=IA(MIH)+NKH
      EH(MINKH)=EH(MINKH)+ GIJKL*XMJNL
 3050 CONTINUE
 3100 CONTINUE
C
 3200 NPASS=NPASS+1
      GO TO (3300,3210,3220,3230,3240,3250,3260,3270,3300),NPASS
 3210 IF(IANDJ) GO TO 3200
      IG=JGIJKL
      JG=IGIJKL
      GO TO 3000
 3220 IF(KANDL) GO TO 3200
      IG=IGIJKL
      JG=JGIJKL
      KG=LGIJKL
      LG=KGIJKL
      GO TO 3000
 3230 IF(IANDJ.OR.KANDL) GO TO 3200
      IG=JGIJKL
      JG=IGIJKL
      GO TO 3000
 3240 IF(SAME) GO TO 3300
      IG=KGIJKL
      JG=LGIJKL
      KG=IGIJKL
      LG=JGIJKL
      GO TO 3000
 3250 IF(IANDJ) GO TO 3200
      KG=JGIJKL
      LG=IGIJKL
      GO TO 3000
 3260 IF(KANDL) GO TO 3200
      IG=LGIJKL
      JG=KGIJKL
      KG=IGIJKL
      LG=JGIJKL
      GO TO 3000
 3270 IF(IANDJ.OR.KANDL) GO TO 3300
      KG=JGIJKL
      LG=IGIJKL
      GO TO 3000
 3300 CONTINUE
C
 4000 CONTINUE
      IF(NG.GT.0) GO TO 100
 4100 CONTINUE
      RETURN
      END
C*MODULE MCTWO   *DECK M1EXCR
      SUBROUTINE M1EXCR(NFTI,OX,X,IX,IA,NINTMX,MI,MAJ,NOSQUR)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SKIP
C
      DIMENSION OX(*),X(NINTMX),IX(NINTMX),IA(*)
C
      COMMON /PCKLAB/ LABSIZ
C
C     ----- READ (VIR,OCC/VIR,OCC) INTEGRALS FOR M1EXC -----
C
      M2=MAJ
      M4=(M2*(M2+1))/2
      DO 10 I=1,M2
   10 IA(I)=(I*(I-1))/2
      CALL VCLR(OX,1,M4)
C
C     ----- READ IN INTEGRALS FROM -NFTI- -----
C     INTEGRALS ASSUMED IN REVERSE CANONICAL ORDER
C
  110 CALL PREAD(NFTI,X,IX,NX,NINTMX)
      IF(NX.EQ.0) GO TO 200
      MX=IABS(NX)
      IF(MX.GT.NINTMX) THEN
         CALL ABRT
         STOP
      END IF
      DO 120 M=1,MX
C
                       NPACK = M
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
                       K = IPACK
                       L = JPACK
                       I = KPACK
                       J = LPACK
C
         IF(NOSQUR.EQ.1) THEN
            IDUM=I
            JDUM=J
            I=K
            J=L
            K=IDUM
            L=JDUM
         END IF
         SKIP=(I.LE.MI).OR.(J.GT.MI).OR.(K.LE.MI).OR.(L.GT.MI)
         IF(SKIP) GO TO 120
         IJ=(I-MI-1)*MI+J
         KL=(K-MI-1)*MI+L
         IJKL=IA(IJ)+KL
         OX(IJKL)=X(M)
  120 CONTINUE
      IF(NX.GT.0) GO TO 110
  200 CONTINUE
      RETURN
      END
C*MODULE MCTWO   *DECK M1LGR
      SUBROUTINE M1LGR(NEG,NEH,EG,EH,X,NFTG,G,GIJ,IX,
     *                 NIA,IA,NORB,IB,IC,NINTMX,MI,MA,MIJ,NCORBS,DROPC)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL IANDJ,KANDL,SAME,DROPC
C
      DIMENSION EG(NEG),EH(NEH)
      DIMENSION IA(NIA),IB(NORB),IC(NORB,*)
      DIMENSION X(MA,*),G(NINTMX),IX(NINTMX),GIJ(*)
C
      COMMON /PCKLAB/ LABSIZ
C
      PARAMETER (GTOL=1.0D-08, ZERO=0.0D+00)
C
      DO 10 I=1,NIA
   10 IA(I)=(I*(I-1))/2
C
      IF (NCORBS.EQ.0 .OR. .NOT. DROPC) GO TO 80
C
C     ---- COULOMB CONTRIBUTION FROM I AND J CORE, I CORE J VAL,
C                                                  I VAL J CORE
C
      DO 25 M = 1,MA
C
      DO 20 I=1,NCORBS
       MIROT=IC(M+MI,I)
       IF(MIROT.EQ.0) GO TO 20
C
      DO 15 N = NCORBS+1,MI
C
        DO 5 J=1,NCORBS
         NJROT=IC(N,J)
         IF(NJROT.EQ.0) GO TO 5
C
         IJ=IA(MAX0(I,J))+MIN0(I,J)
         NXC =(N-1)*MIJ+IJ
         NN1 = IA(N)+J
         NEC1=(I-1)*MIJ +NN1
         NNN = IA(N)+ I
         NEC2=(J-1)*MIJ+NNN
C
      XC=(X(M,NEC1)+X(M,NEC1))+(X(M,NEC1)+X(M,NEC1))-X(M,NEC2)-X(M,NXC)
         IF(XC.EQ.ZERO) GO TO 5
      FF = (XC+XC) + (XC+XC)
         NRX = IA(MAX0(MIROT,NJROT))+ MIN0(MIROT,NJROT)
         EH(NRX) = EH(NRX) + FF
C
  5   CONTINUE
 15   CONTINUE
 20   CONTINUE
 25   CONTINUE
C
C     ----- I CORE J VALENCE OR I VALENCE AND J CORE -----
C
      DO 65 M = 1,MA
C
       DO 60 I=1,NCORBS
       MIROT=IC(M+MI,I)
C
        DO 50 N = 1,MI
        NIROT=IC(N,I)
        IF(MIROT.EQ.0.AND.NIROT.EQ.0) GO TO 50
C
         DO 45 L = NCORBS+1,MI
         IL=IA(L)+I
C
       NXC =(N-1)*MIJ+IL
       XNXC = X(M,NXC)
       NNN = IA(MAX0(N,L))+MIN0(N,L)
       NEC1=(I-1)*MIJ +NNN
       XNEC1 = X(M,NEC1)
       NNN = IA(MAX0(N,I))+MIN0(N,I)
       NEC2=(L-1)*MIJ+NNN
       XNEC2 = X(M,NEC2)
       XC1 = (XNEC1+XNEC1)+(XNEC1+XNEC1) - XNEC2 - XNXC
       XC2 = (XNEC2+XNEC2)+(XNEC2+XNEC2) - XNEC1 - XNXC
       IF(XC1.EQ.ZERO.AND.XC2.EQ.ZERO) GO TO 45
C
C      ----- EXCH -----
C
        DO 40 J = NCORBS+1,MI
         J1=MAX0(J,L)-NCORBS
         L1=MIN0(J,L)-NCORBS
         NGJL = IA(J1)+L1
         XGJL = GIJ(NGJL)
         IF(XGJL.EQ.ZERO) GO TO 40
C
         NJROT=IC(N,J)
          IF(NJROT.EQ.0.OR.MIROT.EQ.0) GO TO 35
         FF = XGJL * (XC1+XC1)
         IF (N.LT.J) FF = -FF
         NRX = IA(MAX0(MIROT,NJROT))+MIN0(MIROT,NJROT)
         EH(NRX) = EH(NRX) + FF
C
  35     MJROT=IC(M+MI,J)
          IF(MJROT.EQ.0.OR.NIROT.EQ.0) GO TO 40
         FF = XGJL * (XC2+XC2)
          IF (N.LT.I) FF = -FF
         NRX = IA(MAX0(MJROT,NIROT))+MIN0(MJROT,NIROT)
         EH(NRX) = EH(NRX) + FF
C
  40   CONTINUE
  45   CONTINUE
  50  CONTINUE
  60  CONTINUE
  65  CONTINUE
C
  80  CONTINUE
C
C     ----- READ IN -DM2- FROM -NFTG- -----
C
      CALL SEQREW(NFTG)
  100 CALL PREAD(NFTG,G,IX,NG,NINTMX)
      IF(NG.EQ.0) GO TO 4100
      MGIJKL=IABS(NG)
      IF(MGIJKL.GT.NINTMX) THEN
         CALL ABRT
         STOP
      END IF
      DO 4000 MG=1,MGIJKL
      GGIJKL=G(MG)
      IF( ABS(GGIJKL).LT.GTOL) GO TO 4000
C
                       NPACK = MG
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
                       IGIJKL = IPACK
                       JGIJKL = JPACK
                       KGIJKL = KPACK
                       LGIJKL = LPACK
C
C     ----- I AND J VALENCE -------
C
      IANDJ=IGIJKL.EQ.JGIJKL
      KANDL=KGIJKL.EQ.LGIJKL
      SAME =IGIJKL.EQ.KGIJKL.AND.JGIJKL.EQ.LGIJKL
C
      IF (DROPC) THEN
         IGIJKL = IGIJKL + NCORBS
         JGIJKL = JGIJKL + NCORBS
         KGIJKL = KGIJKL + NCORBS
         LGIJKL = LGIJKL + NCORBS
      END IF
C
C     ----- LAGRANGIAN CONTRIBUTION -----
C
      GAB=GGIJKL
      IF(.NOT.IANDJ) GAB=GAB+GAB
      NAB=IA(IGIJKL)+JGIJKL
      GCD=GGIJKL
      IF(.NOT.KANDL) GCD=GCD+GCD
      NCD=IA(KGIJKL)+LGIJKL
C
      IG=IGIJKL
      JG=JGIJKL
      KG=KGIJKL
      LG=LGIJKL
      KL=NCD
      GIJKL=GCD
      NPASS=1
 1000 CONTINUE
C
      DO 1100 M=1,MA
         JKLX=(JG-1)*MIJ+KL
         XMJKL=X(M,JKLX)
         IF(XMJKL.EQ.ZERO) GO TO 1100
         MILGR=IB(M+MI)+IG
         EG(MILGR)=EG(MILGR)+ GIJKL*XMJKL
 1100 CONTINUE
C
C     ----- CONTRIBUTIONS TO ORBITAL HESSIAN -----
C
 1200 NPASS=NPASS+1
      GO TO (1300,1210,1220,1230,1300),NPASS
 1210 IF(IANDJ) GO TO 1200
      IG=JGIJKL
      JG=IGIJKL
      GO TO 1000
 1220 IF(SAME) GO TO 1300
      IG=KGIJKL
      JG=LGIJKL
      KG=IGIJKL
      LG=JGIJKL
      KL=NAB
      GIJKL=GAB
      GO TO 1000
 1230 IF(KANDL) GO TO 1300
      IG=LGIJKL
      JG=KGIJKL
      GO TO 1000
 1300 CONTINUE
C
C     ----- COULOMB CONTRIBUTION TO HESSIAN -----
C
      IG=IGIJKL
      JG=JGIJKL
      KG=KGIJKL
      LG=LGIJKL
      KL=NCD
      GMINJ=GCD
      NPASS=1
 2000 CONTINUE
C
      DO 2100 M=1,MA
      GM=GMINJ
      MIROT=IC(M+MI,IG)
      IF(MIROT.EQ.0) GO TO 2100
      DO 2050 N=1,MI
      GIJKL=GM
      IF(N -JG) 2030,2050,2040
 2030 GIJKL=-GIJKL
 2040 NJROT=IC(N,JG)
      IF(NJROT.EQ.0) GO TO 2050
      NKLX=(N-1)*MIJ+KL
      XMNKL=X(M,NKLX)
      IF(XMNKL.EQ.ZERO) GO TO 2050
      GIJKL=GIJKL+GIJKL
      MIH=MAX(MIROT,NJROT)
      NJH=MIN(MIROT,NJROT)
      MINJH=IA(MIH)+NJH
      EH(MINJH)=EH(MINJH)+ GIJKL*XMNKL
 2050 CONTINUE
 2100 CONTINUE
C
 2200 NPASS=NPASS+1
      GO TO (2300,2210,2220,2230,2300),NPASS
 2210 IF(IANDJ) GO TO 2200
      IG=JGIJKL
      JG=IGIJKL
      GO TO 2000
 2220 IF(SAME) GO TO 2300
      IG=KGIJKL
      JG=LGIJKL
      KG=IGIJKL
      LG=JGIJKL
      KL=NAB
      GMINJ=GAB
      GO TO 2000
 2230 IF(KANDL) GO TO 2300
      IG=LGIJKL
      JG=KGIJKL
      GO TO 2000
 2300 CONTINUE
C
C     ----- EXCHANGE CONTRIBUTION TO HESSIAN -----
C
      GDM2=GGIJKL+GGIJKL
      IG=IGIJKL
      JG=JGIJKL
      KG=KGIJKL
      LG=LGIJKL
      NPASS=1
 3000 CONTINUE
C
      DO 3100 M=1,MA
      GM=GDM2
      MIROT=IC(M+MI,IG)
      IF(MIROT.EQ.0) GO TO 3100
      MJ=(JG-1)*MIJ
      DO 3050 N=1,MI
      GIJKL=GM
      IF(N -KG) 3030,3050,3040
 3030 GIJKL=-GIJKL
 3040 NKROT=IC(N,KG)
      IF(NKROT.EQ.0) GO TO 3050
      NN=MAX(N,LG)
      LL=MIN(N,LG)
      NL=IA(NN)+LL
      JNLX=MJ+NL
      XMJNL=X(M,JNLX)
      IF(XMJNL.EQ.ZERO) GO TO 3050
      GIJKL=GIJKL+GIJKL
      MIH=MAX(MIROT,NKROT)
      NKH=MIN(MIROT,NKROT)
      MINKH=IA(MIH)+NKH
      EH(MINKH)=EH(MINKH)+ GIJKL*XMJNL
 3050 CONTINUE
 3100 CONTINUE
C
 3200 NPASS=NPASS+1
      GO TO (3300,3210,3220,3230,3240,3250,3260,3270,3300),NPASS
 3210 IF(IANDJ) GO TO 3200
      IG=JGIJKL
      JG=IGIJKL
      GO TO 3000
 3220 IF(KANDL) GO TO 3200
      IG=IGIJKL
      JG=JGIJKL
      KG=LGIJKL
      LG=KGIJKL
      GO TO 3000
 3230 IF(IANDJ.OR.KANDL) GO TO 3200
      IG=JGIJKL
      JG=IGIJKL
      GO TO 3000
 3240 IF(SAME) GO TO 3300
      IG=KGIJKL
      JG=LGIJKL
      KG=IGIJKL
      LG=JGIJKL
      GO TO 3000
 3250 IF(IANDJ) GO TO 3200
      KG=JGIJKL
      LG=IGIJKL
      GO TO 3000
 3260 IF(KANDL) GO TO 3200
      IG=LGIJKL
      JG=KGIJKL
      KG=IGIJKL
      LG=JGIJKL
      GO TO 3000
 3270 IF(IANDJ.OR.KANDL) GO TO 3300
      KG=JGIJKL
      LG=IGIJKL
      GO TO 3000
 3300 CONTINUE
C
 4000 CONTINUE
      IF(NG.GT.0) GO TO 100
 4100 CONTINUE
      RETURN
      END
C*MODULE MCTWO   *DECK M1LGRR
      SUBROUTINE M1LGRR(NFTI,OX,X,IX,IA,NINTMX,MI,MA,MIJ,NOSQUR)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SKIP
C
      DIMENSION OX(MA,*),X(NINTMX),IX(NINTMX),IA(*)
C
      COMMON /PCKLAB/ LABSIZ
C
C     ----- READ (VIR,OCC//OCC,OCC) INTEGRALS FOR M1LGR -----
C
      DO 10 I=1,MI
   10 IA(I)=(I*I-I)/2
      MOX=MI*MIJ
      CALL VCLR(OX,1,MA*MOX)
C
C     ----- READ IN INTEGRALS FROM -NFTI- -----
C
  110 CALL PREAD(NFTI,X,IX,NX,NINTMX)
      IF(NX.EQ.0) GO TO 200
      MX=IABS(NX)
      IF(MX.GT.NINTMX) CALL ABRT
      DO 120 M=1,MX
C
                       NPACK = M
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
                       K = IPACK
                       L = JPACK
                       I = KPACK
                       J = LPACK
C
         IF(NOSQUR.EQ.1) THEN
            IDUM=I
            JDUM=J
            I=K
            J=L
            K=IDUM
            L=JDUM
         END IF
         SKIP=(I.LE.MI).OR.(J.GT.MI).OR.(K.GT.MI).OR.(L.GT.MI)
         IF(SKIP) GO TO 120
         KL=IA(K)+L
         JKL=(J-1)*MIJ+KL
         OX(I-MI,JKL)=X(M)
  120 CONTINUE
      IF(NX.GT.0) GO TO 110
  200 CONTINUE
      RETURN
      END
C*MODULE MCTWO   *DECK M2DM2R
      SUBROUTINE M2DM2R(NFTG,NORBS,OG,G,IX,NINTMX,IA)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION OG(*),G(NINTMX),IX(NINTMX),IA(*)
C
      COMMON /PCKLAB/ LABSIZ
C
C     ----- READ 2 PARTICLE DENSITY MATRIX FOR USE BY M2TEI -----
C
      NORB2=(NORBS*(NORBS+1))/2
      NORB4=(NORB2*(NORB2+1))/2
      DO 10 I=1,NORB2
   10 IA(I)=(I*(I-1))/2
      CALL VCLR(OG,1,NORB4)
C
C     ----- READ IN -DM2- FROM -NFTG- -----
C
      CALL SEQREW(NFTG)
  110 CALL PREAD(NFTG,G,IX,NG,NINTMX)
      IF(NG.EQ.0) GO TO 200
      MG=IABS(NG)
      IF(MG.GT.NINTMX) THEN
         CALL ABRT
         STOP
      END IF
      DO 120 M=1,MG
C
                       NPACK = M
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
                       I = IPACK
                       J = JPACK
                       K = KPACK
                       L = LPACK
C
         IF(I.GT.NORBS) GO TO 200
         IJ=IA(I)+J
         KL=IA(K)+L
         IJKL=IA(IJ)+KL
         OG(IJKL)=G(M)
  120 CONTINUE
      IF(NG.GT.0) GO TO 110
  200 CONTINUE
      RETURN
      END
C*MODULE MCTWO   *DECK M2TEI
      SUBROUTINE M2TEI(NE,NH2,EH,G,NFTX,X,IX,
     *                 IA,IB,IC,NINTMX,NIA,NORB,NORBS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL IANDJ,KANDL,SAME
C
      DIMENSION EH(*),IA(NIA),IB(NORB),IC(NORB,NORBS)
      DIMENSION G(*),X(NINTMX),IX(NINTMX)
C
      COMMON /PCKLAB/ LABSIZ
C
      PARAMETER (TOL=1.0D-09)
C
      DO 10 I=1,NIA
   10 IA(I)=(I*I-I)/2
      NEH=NE+NH2
      CALL VCLR(EH,1,NEH)
C
C     ----- READ IN -INT2- FROM -NFTX- -----
C
  100 CALL PREAD(NFTX,X,IX,NX,NINTMX)
      IF(NX.EQ.0) GO TO 4100
      MXIJKL=IABS(NX)
      IF(MXIJKL.GT.NINTMX) THEN
         CALL ABRT
         STOP
      END IF
      DO 4000 MX=1,MXIJKL
         XMX=X(MX)
C
                       NPACK = MX
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
                       IXIJKL = IPACK
                       JXIJKL = JPACK
                       KXIJKL = KPACK
                       LXIJKL = LPACK
C
      IANDJ=IXIJKL.EQ.JXIJKL
      KANDL=KXIJKL.EQ.LXIJKL
      SAME =IXIJKL.EQ.KXIJKL.AND.JXIJKL.EQ.LXIJKL
C
      XAB=XMX
      IF(.NOT.IANDJ) XAB=XAB+XAB
      NAB=IA(IXIJKL)+JXIJKL
      XCD=XMX
      IF(.NOT.KANDL) XCD=XCD+XCD
      NCD=IA(KXIJKL)+LXIJKL
C
C     ----- LAGRANGIAN CONTRIBUTION -----
C
      IG=IXIJKL
      JG=JXIJKL
      KG=KXIJKL
      LG=LXIJKL
      KL=NCD
      XIJKL=XCD
      NPASS=1
 1000 CONTINUE
      IF(JG.GT.NORBS.OR.KG.GT.NORBS.OR.LG.GT.NORBS) GO TO 1200
C
      DO 1100 M=1,NORBS
         MM=MAX(M,JG)
         JJ=MIN(M,JG)
         MJ=IA(MM)+JJ
         MJG=MAX(MJ,KL)
         KLG=MIN(MJ,KL)
         MJKLG=IA(MJG)+KLG
         GMJKL=G(MJKLG)
         IF( ABS(GMJKL).LT.TOL) GO TO 1100
         MILGR=IB(IG)+M
         EH(MILGR)=EH(MILGR)+XIJKL*GMJKL
 1100 CONTINUE
C
 1200 NPASS=NPASS+1
      GO TO (1300,1210,1220,1230,1300),NPASS
 1210 IF(IANDJ) GO TO 1200
      IG=JXIJKL
      JG=IXIJKL
      GO TO 1000
 1220 IF(SAME) GO TO 1300
      IG=KXIJKL
      JG=LXIJKL
      KG=IXIJKL
      LG=JXIJKL
      KL=NAB
      XIJKL=XAB
      GO TO 1000
 1230 IF(KANDL) GO TO 1300
      IG=LXIJKL
      JG=KXIJKL
      GO TO 1000
 1300 CONTINUE
C
C     ----- COULOMB CONTRIBUTION TO HESSIAN -----
C
      IG=IXIJKL
      JG=JXIJKL
      KG=KXIJKL
      LG=LXIJKL
      KL=NCD
      XMINJ=XCD
      NPASS=1
 2000 CONTINUE
      IF(KG.GT.NORBS.OR.LG.GT.NORBS) GO TO 2200
C
      DO 2100 M=1,NORBS
      XM=XMINJ
      IF(M -IG) 2020,2100,2010
 2010 XM=-XM
 2020 MIROT=IC(IG,M)
      IF(MIROT.EQ.0) GO TO 2100
      MAXN=M
      IF(JG.GT.IG) MAXN=M-1
      IF(MAXN.EQ.0) GO TO 2100
      DO 2050 N=1,MAXN
      XIJKL=XM
      IF(N -JG) 2040,2050,2030
 2030 XIJKL=-XIJKL
 2040 NJROT=IC(JG,N)
      IF(NJROT.EQ.0) GO TO 2050
      IF(N.NE.M.OR.JG.NE.IG) XIJKL=XIJKL+XIJKL
      MN=IA(M)+N
      MNG=MAX(MN,KL)
      KLG=MIN(MN,KL)
      MNKLG=IA(MNG)+KLG
      GMNKL=G(MNKLG)
      IF( ABS(GMNKL).LT.TOL) GO TO 2050
      MIH=MAX(MIROT,NJROT)
      NJH=MIN(MIROT,NJROT)
      MINJH=IA(MIH)+NJH
      MINJH=MINJH+NE
      EH(MINJH)=EH(MINJH)+XIJKL*GMNKL
 2050 CONTINUE
 2100 CONTINUE
C
 2200 NPASS=NPASS+1
      GO TO (2300,2210,2220,2230,2300),NPASS
 2210 IF(IANDJ) GO TO 2200
      IG=JXIJKL
      JG=IXIJKL
      GO TO 2000
 2220 IF(SAME) GO TO 2300
      IG=KXIJKL
      JG=LXIJKL
      KG=IXIJKL
      LG=JXIJKL
      KL=NAB
      XMINJ=XAB
      GO TO 2000
 2230 IF(KANDL) GO TO 2300
      IG=LXIJKL
      JG=KXIJKL
      GO TO 2000
 2300 CONTINUE
C
C     ----- EXCHANGE CONTRIBUTION TO HESSIAN -----
C
      XINT2=XMX+XMX
      IG=IXIJKL
      JG=JXIJKL
      KG=KXIJKL
      LG=LXIJKL
      NPASS=1
 3000 CONTINUE
      IF(JG.GT.NORBS.OR.LG.GT.NORBS) GO TO 3200
C
      DO 3100 M=1,NORBS
      XM=XINT2
      IF(M -IG) 3020,3100,3010
 3010 XM=-XM
 3020 MIROT=IC(IG,M)
      IF(MIROT.EQ.0) GO TO 3100
      MM=MAX(M,JG)
      JJ=MIN(M,JG)
      MJ=IA(MM)+JJ
      MAXN=M
      IF(KG.GT.IG) MAXN=M-1
      IF(MAXN.EQ.0) GO TO 3100
      DO 3050 N=1,MAXN
      XIJKL=XM
      IF(N -KG) 3040,3050,3030
 3030 XIJKL=-XIJKL
 3040 NKROT=IC(KG,N)
      IF(NKROT.EQ.0) GO TO 3050
      IF(N.NE.M.OR.KG.NE.IG) XIJKL=XIJKL+XIJKL
      NN=MAX(N,LG)
      LL=MIN(N,LG)
      NL=IA(NN)+LL
      MJG=MAX(MJ,NL)
      NLG=MIN(MJ,NL)
      MJNLG=IA(MJG)+NLG
      GMJNL=G(MJNLG)
      IF( ABS(GMJNL).LT.TOL) GO TO 3050
      MIH=MAX(MIROT,NKROT)
      NKH=MIN(MIROT,NKROT)
      MINKH=IA(MIH)+NKH
      MINKH=MINKH+NE
      EH(MINKH)=EH(MINKH)+XIJKL*GMJNL
 3050 CONTINUE
 3100 CONTINUE
C
 3200 NPASS=NPASS+1
      GO TO (3300,3210,3220,3230,3240,3250,3260,3270,3300),NPASS
 3210 IF(IANDJ) GO TO 3200
      IG=JXIJKL
      JG=IXIJKL
      GO TO 3000
 3220 IF(KANDL) GO TO 3200
      IG=IXIJKL
      JG=JXIJKL
      KG=LXIJKL
      LG=KXIJKL
      GO TO 3000
 3230 IF(IANDJ.OR.KANDL) GO TO 3200
      IG=JXIJKL
      JG=IXIJKL
      GO TO 3000
 3240 IF(SAME) GO TO 3300
      IG=KXIJKL
      JG=LXIJKL
      KG=IXIJKL
      LG=JXIJKL
      GO TO 3000
 3250 IF(IANDJ) GO TO 3200
      KG=JXIJKL
      LG=IXIJKL
      GO TO 3000
 3260 IF(KANDL) GO TO 3200
      IG=LXIJKL
      JG=KXIJKL
      KG=IXIJKL
      LG=JXIJKL
      GO TO 3000
 3270 IF(IANDJ.OR.KANDL) GO TO 3300
      KG=JXIJKL
      LG=IXIJKL
      GO TO 3000
 3300 CONTINUE
C
 4000 CONTINUE
      IF(NX.GT.0) GO TO 100
 4100 CONTINUE
      RETURN
      END
C
C GDF:  4/05/02  NEW PARALLEL SUBROUTINES
C
C*MODULE MCTWO   *DECK FVADDI
      SUBROUTINE FVADDI(NMOS,NCOR,NACT,DVAL,FVAL,BUFF)
C
C ---------------------------------------------------------------------- 
C
C  NEWTON-RAPHSON MCSCF. CALLED FROM NEWTON (MCSCF.SRC).
C  CONSTRUCT THE VALENCE FOCK OPERATOR DIRECTLY IN THE MO BASIS 
C  FROM TRANSFORMED INTEGRALS STORED IN THE DDI ARRAYS.
C
C  SYMBOLS:
C           NMOS = TOTAL NUMBER OF MOS
C           NCOR = NUMBER OF CORE MOS
C           NACT = NUMBER OF ACTIVE MOS
C           DVAL = 1-EL DENSITY SPANNING ACTIVE INDICES
C           FVAL = VALENCE FOCK OPERATOR IN THE MO BASIS
C           BUFF = A MESSAGE BUFFER
C
C  INTEGRALS NEED AT LEAST TWO ACTIVE INDICES IN THE FORMULA
C  FVAL(IJ) = SUM_OVER_KL{DVAL(KL)*[(IJ|KL)-1/2(IK|JL)]}, K,L ACTIVE, 
C  I,J GENERAL. FVAL,DVAL STORED IN TRIANGULAR FORM.
C
C ---------------------------------------------------------------------- 
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MXAO=2047, ZERO=0.0D+00, HALF=0.5D+00)
      LOGICAL INJ
      DOUBLE PRECISION  FVAL(*),DVAL(*),BUFF(*)
      INTEGER DDI_NP, DDI_ME, A,B,AB,BA,AI
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
     *,               NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /IJPAIR/ IA(MXAO)
C
      NCP1 = NCOR + 1
      NOCC = NCOR + NACT
      NVIR = NMOS - NOCC
      NMTR = (NMOS*NMOS+NMOS)/2
      NOTR = (NOCC*NOCC+NOCC)/2
      NVTR = (NVIR*NVIR+NVIR)/2
      NVSQ = NVIR*NVIR
      CALL DCOPY(NMTR,ZERO,0,FVAL,1)
      CALL DDI_NPROC(DDI_NP, DDI_ME)
C
C  (OO|OO) CLASS
C
      CALL DDI_DISTRIB(D_OOOO,DDI_ME,ILO,IHI,JLO,JHI)
      DO I = 1, NOCC
        DO J = 1, I
          IJCOL = IA(I) + J
          IF ((IJCOL.GE.JLO).AND.(IJCOL.LE.JHI)) THEN
            CALL DDI_GET(D_OOOO,1,NOTR,IJCOL,IJCOL,BUFF)
            IJ = IJCOL
C
C  COULOMB TERMS
C
            DO K = 1, NACT
              KN = K + NCOR
              DO L = 1, NACT
                LN = L + NCOR
                KL   = IA(MAX(K,L))   + MIN(K,L)
                KLN  = IA(MAX(KN,LN)) + MIN(KN,LN)
                FVAL(IJ) = FVAL(IJ)   + DVAL(KL)*BUFF(KLN)
              END DO
            END DO
C
C  EXCHANGE TERMS
C
            IF (I.GT.NCOR) THEN
              IN = I - NCOR
              DO K = NCP1, NOCC
                KN = K - NCOR
                IKN = IA(MAX(IN,KN)) + MIN(IN,KN)
                DIK = HALF*DVAL(IKN)
                DO L = 1, J
                  JL = IA(J) + L
                  KL = IA(MAX(K,L)) + MIN(K,L)
                  FVAL(JL) = FVAL(JL) - DIK*BUFF(KL)
                END DO
              END DO
              IF ((J.GT.NCOR).AND.(J.NE.I)) THEN
                JN = J - NCOR
                DO K = NCP1, NOCC
                  KN = K - NCOR
                  JKN = IA(MAX(JN,KN)) + MIN(JN,KN)
                  DJK = HALF*DVAL(JKN)
                  DO L = 1, I
                    IL = IA(I) + L
                    KL = IA(MAX(K,L)) + MIN(K,L)
                    FVAL(IL) = FVAL(IL) - DJK*BUFF(KL)
                  END DO
                END DO
              END IF
            END IF
          END IF  ! LOCAL
        END DO  ! J
      END DO  ! I
C
C  (VO|OO) CLASS
C
      CALL DDI_DISTRIB(D_VOOO,DDI_ME,ILO,IHI,JLO,JHI)
C
C  COULOMB TERMS (VO|AA)
C
      DO I = NCP1, NOCC
        IN = I - NCOR
        DO J = NCP1, I
          JN = J - NCOR
          IJN = IA(IN) + JN
          DIJ = DVAL(IJN)
          IF (I.NE.J) DIJ = DIJ + DIJ
          IJCOL = IA(I) + J
          DO K = 1, NOCC
            IJKCOL = (IJCOL-1)*NOCC + K
            IF ((IJKCOL.GE.JLO).AND.(IJKCOL.LE.JHI)) THEN
              CALL DDI_GET(D_VOOO,1,NVIR,IJKCOL,IJKCOL,BUFF)
              DO A = 1, NVIR
                NA = A + NOCC
                KA = IA(NA) + K
                FVAL(KA) = FVAL(KA) + DIJ*BUFF(A)
              END DO
            END IF  ! LOCAL
          END DO  ! K
        END DO  ! J
      END DO  ! I
C
C  EXCHANGE TERMS (VA|AO)
C
      DO I = 1, NOCC
        DO J = NCP1, NOCC
          JN = J - NCOR
          IJCOL = IA(MAX(I,J)) + MIN(I,J)
          DO K = NCP1, NOCC
            KN = K - NCOR
            IJKCOL = (IJCOL-1)*NOCC + K
            IF ((IJKCOL.GE.JLO).AND.(IJKCOL.LE.JHI)) THEN
              CALL DDI_GET(D_VOOO,1,NVIR,IJKCOL,IJKCOL,BUFF)
              JKN = IA(MAX(JN,KN)) + MIN(JN,KN)
              DJK = HALF*DVAL(JKN)
              DO A = 1, NVIR
                NA = A + NOCC
                AI = IA(NA) + I
                FVAL(AI) = FVAL(AI) - DJK*BUFF(A)
              END DO
            END IF  ! LOCAL
          END DO  ! K
        END DO  ! J
      END DO  ! I
C
C  (VV|AA) CLASS - COULOMB TERMS
C
      CALL DDI_DISTRIB(D_VVOO,DDI_ME,ILO,IHI,JLO,JHI)
      DO I = 1, NACT
        IN = I + NCOR
        DO J = 1, I
          IJ = IA(I) + J
          JN = J + NCOR
          IJCOL = IA(IN) + JN
          IF ((IJCOL.GE.JLO).AND.(IJCOL.LE.JHI)) THEN
            CALL DDI_GET(D_VVOO,1,NVTR,IJCOL,IJCOL,BUFF)
            DIJ = DVAL(IJ)
            IF (I.NE.J) DIJ = DIJ + DIJ
            DO A = 1, NVIR
              NA = A + NOCC
              DO B = 1, A
                AB   = IA(A) + B
                NB   = B + NOCC
                NAB  = IA(NA) + NB
                FVAL(NAB) = FVAL(NAB) + DIJ*BUFF(AB)
              END DO
            END DO
          END IF  ! LOCAL
        END DO  ! J
      END DO  ! I
C
C  (VA|VA) CLASS - EXCHANGE TERMS
C
      CALL DDI_DISTRIB(D_VOVO,DDI_ME,ILO,IHI,JLO,JHI)
      DO I = 1, NACT
        IN = I + NCOR
        DO J = 1, I
          IJ = IA(I) + J
          JN = J + NCOR
          IJCOL = IA(IN) + JN
          INJ = I.NE.J
          IF ((IJCOL.GE.JLO).AND.(IJCOL.LE.JHI)) THEN
            CALL DDI_GET(D_VOVO,1,NVSQ,IJCOL,IJCOL,BUFF)
            DIJ = HALF*DVAL(IJ)
            DO A = 1, NVIR
              NA = A + NOCC
              DO B = 1, A
                AB   = (A-1)*NVIR + B
                NB   = B + NOCC
                NAB  = IA(NA) + NB
                FVAL(NAB) = FVAL(NAB) - DIJ*BUFF(AB)
              END DO
            END DO
            IF (INJ) THEN
              DO A = 1, NVIR
                NA = A + NOCC
                DO B = 1, A
                  BA   = (B-1)*NVIR + A
                  NB   = B + NOCC
                  NAB  = IA(NA) + NB
                  FVAL(NAB) = FVAL(NAB) - DIJ*BUFF(BA)
                END DO
              END DO
            END IF
          END IF  ! LOCAL
        END DO  ! J
      END DO  ! I
C
C  GLOBALLY SUM THE VALENCE FOCK MATRIX
C
      CALL DDI_GSUMF( 2520, FVAL, NMTR )
      RETURN
      END
C*MODULE MCTWO   *DECK PNTNLAG
      SUBROUTINE PNTNLAG(NMOS,NCOR,NACT,FCOR,FVAL,OPDM,ILAG,LAGN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C -----------------------------------------------------------------
C
C  PARALLEL 1-EL CONTRIBUTIONS TO LAGRANGIAN 
C
C  SYMBOLS:
C           NMOS = TOTAL NUMBER OF MOS
C           NCOR = NUMBER OF CORE MOS
C           NACT = NUMBER OF ACTIVE MOS
C           FCOR = CORE FOCK MATRIX IN MO BASIS
C           FVAL = VALENCE FOCK MATRIX IN MO BASIS
C           OPDM = 1-EL DENSITY SPANNING ACTIVE INDICES
C           ILAG = INDEX TO THE LAGRANGIAN
C           LAGN = LAGRANGIAN
C
C -----------------------------------------------------------------
C
      PARAMETER (MXAO=2047, TWO=2.0D+00)
      INTEGER ILAG(*), DDI_NP,DDI_ME
      DOUBLE PRECISION  FVAL(*),FCOR(*),OPDM(*),LAGN(*)
      COMMON /IJPAIR/ IA(MXAO)
C
      CALL DDI_NPROC(DDI_NP,DDI_ME)
C
C  NUMBER OF VALENCE, OR 'ACTIVE' MOS
C
      DO M = 1, NMOS
        IF (MOD(M,DDI_NP).EQ.DDI_ME) THEN
C
C  LAGN(MI) = 2*(FC(MI) + FA(MI)),  I CORE, M GENERAL
C
          DO I = 1, NCOR
            IM = IA(MAX(M,I)) + MIN(M,I)
            MI = ILAG(M) + I
            FF = FCOR(IM) + FVAL(IM)
            LAGN(MI) = LAGN(MI) + TWO*FF
          END DO
C
C  LAGN(MI) = SUM_J  D(IJ)*FC(MJ),  I,J VALENCE, M GENERAL
C
          DO I = 1, NACT
            MI = ILAG(M) + I + NCOR
            DO J = 1, NACT
              JN = J + NCOR
              IJ = IA(MAX(I,J)) + MIN(I,J)
              MJ = IA(MAX(M,JN)) + MIN(M,JN)
              LAGN(MI) = LAGN(MI) + FCOR(MJ)*OPDM(IJ)
            END DO
          END DO
C
        END IF  !  PARALLEL
      END DO
      RETURN
      END
C*MODULE MCTWO   *DECK PNTNHES
      SUBROUTINE PNTNHES(NMOS,NCOR,NACT,NROT
     *,                  FCOR,FVAL,OPDM,IHES,HESS,IPIN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C -----------------------------------------------------------------
C
C  PARALLEL 1-EL TERMS OF HESSIAN
C
C  SYMBOLS:
C           NMOS = TOTAL NUMBER OF MOS
C           NCOR = NUMBER OF CORE MOS
C           NACT = NUMBER OF ACTIVE MOS
C           NROT = NUMBER OF ROTATIONS
C           FCOR = CORE FOCK OPERATOR
C           FVAL = VALENCE FOCK OPERATOR
C           OPDM = 1-EL DENSITY SPANNING ACTIVE INDICES
C           IHES = INDEX TO HESSIAN
C           HESS = HESSIAN MATRIX
C           IPIN = PAIR INDEX
C
C -----------------------------------------------------------------
C
      PARAMETER (HALF=0.5D+00, TWO=2.0D+00)
      INTEGER IHES(NMOS,*),IPIN(*), DDI_NP,DDI_ME
      DOUBLE PRECISION  FCOR(*),FVAL(*),OPDM(*),HESS(*)
C
      NCP1 = NCOR + 1
      NOCC = NCOR + NACT
      CALL DDI_NPROC(DDI_NP,DDI_ME)
C
C  CONVENIENT TO SET UP PAIR INDEX FOR HESSIAN 
C  HERE AND USE IN RD2PDM AND LH2DDI
C
      II = 0
      DO I = 1,MAX(NROT+1,NMOS,(NACT*NACT+NACT)/2)
        IPIN(I) = II
        II = II + I
      END DO
C
C  HESS(MI,NI) = 2*( FCOR(MN) + FVAL(MN) ), I CORE, M,N NOT CORE
C
      DO M = NCP1, NMOS
        DO N = NCP1, M
          MN = IPIN(M) + N
          IF (MOD(MN,DDI_NP).EQ.DDI_ME) THEN
            FF = TWO*( FCOR(MN) + FVAL(MN) )
            DO I = 1, NCOR
              MI = IHES(M,I)
              NI = IHES(N,I)
              MINI = IPIN(MAX(MI,NI)) + MIN(MI,NI)
              HESS(MINI) = HESS(MINI) + FF
            END DO
          END IF   !  PARALLEL
        END DO
      END DO
C
C  H(MI,NJ) = OPDM(IJ)*FCOR(MN), I,J VALENCE, M,N GENERAL
C
      DO M = 1, NMOS
        DO N = 1, M
          MN = IPIN(M) + N
          IF (MOD(MN,DDI_NP).EQ.DDI_ME) THEN
            DO I = NCP1, NOCC
              MI = IHES(M,I)
              NI = IHES(N,I)
              DO J = NCP1, I
                NJ = IHES(N,J)
                MJ = IHES(M,J)
                II = I - NCOR
                JJ = J - NCOR
                IJ = IPIN(II) + JJ
                XX = OPDM(IJ)*FCOR(MN)
                IF (M.EQ.N) XX = XX*HALF
                IF (I.EQ.J) XX = XX*HALF
                FF = XX
                IF (M.LT.I) FF = -FF
                IF (N.LT.J) FF = -FF
                MINJ = IPIN(MAX(MI,NJ)) + MIN(MI,NJ)
                IF (MI.EQ.NJ) FF = FF*TWO
                HESS(MINJ) = HESS(MINJ) + FF
                FF = XX
                IF (N.LT.I) FF = -FF
                IF (M.LT.J) FF = -FF
                NIMJ = IPIN(MAX(NI,MJ)) + MIN(NI,MJ)
                IF (NI.EQ.MJ) FF = FF*TWO
                HESS(NIMJ) = HESS(NIMJ) + FF
              END DO
            END DO
          END IF   !  PARALLEL
        END DO
      END DO
      RETURN
      END
C*MODULE MCTWO   *DECK RD2PDM
      SUBROUTINE RD2PDM(INFILE,TPDM,GBUF,LABS,NINTMX,IPIN)
C
C -----------------------------------------------------------------
C
C  READ 2-EL DENSITY SPANNING ACTIVE INDICES. WRITTEN BY WTDM12 
C  (ALDECI.SRC). NOTE LABELS START AT 1 FOR FIRST ACTIVE MO.
C
C  SYMBOLS:
C           INFILE = DISC FILE IDENTIFIER
C           TPDM   = 2-EL DENSITY SPANNING ACTIVE INDICES
C           GBUF   = DENSITY MATRIX ELEMENT I/O BUFFER
C           LABS   = I/O BUFFER FOR 2-PDM ACTIVE LABELS
C           NINTMX = I/O BUFFER LENGTH IN WORDS
C           IPIN   = PAIR INDEX
C
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL READMORE
      INTEGER LABS(*),IPIN(*)
      DOUBLE PRECISION  TPDM(*),GBUF(*)
      COMMON /PCKLAB/ LABSIZ
C
      CALL SEQREW(INFILE)
      READMORE = .TRUE.
      DO WHILE (READMORE)
        NG = 0
        CALL PREAD(INFILE,GBUF,LABS,NG,NINTMX)
        IF (NG.LE.0) READMORE = .FALSE.
        MG = IABS(NG)
        DO M = 1, MG
C
C  UNPACK LABELS
C
                       NPACK = M
                       IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                         LABEL = LABS( 2*NPACK - 1 )
                         IPACK = ISHFT( LABEL, -16 )
                         JPACK = IAND(  LABEL, 65535 )
                         LABEL = LABS( 2*NPACK     )
                         KPACK = ISHFT( LABEL, -16 )
                         LPACK = IAND(  LABEL, 65535 )
#endif
#if defined(SPEC_CPU_ILP64)
                         LABEL = LABS(NPACK)
                         IPACK = ISHFT( LABEL, -48 )
                         JPACK = IAND( ISHFT( LABEL, -32 ), 65535 )
                         KPACK = IAND( ISHFT( LABEL, -16 ), 65535 )
                         LPACK = IAND( LABEL, 65535 )
#endif
                       ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                         LABEL = LABS(NPACK)
                         IPACK = ISHFT( LABEL, -24 )
                         JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
                         KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
                         LPACK = IAND( LABEL, 255 )
#endif
#if defined(SPEC_CPU_ILP64)
                         IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                           LABEL = LABS( NPACK/2 )
                           IPACK = IAND( ISHFT( LABEL, -24 ), 255 )
                           JPACK = IAND( ISHFT( LABEL, -16 ), 255 )
                           KPACK = IAND( ISHFT( LABEL,  -8 ), 255 )
                           LPACK = IAND( LABEL, 255 )
                         ELSE
                           LABEL = LABS( (NPACK/2)+1 )
                           IPACK = ISHFT( LABEL, -56 )
                           JPACK = IAND( ISHFT( LABEL, -48 ), 255 )
                           KPACK = IAND( ISHFT( LABEL, -40 ), 255 )
                           LPACK = IAND( ISHFT( LABEL, -32 ), 255 )
                         END IF
#endif
                       END IF
                       I = IPACK
                       J = JPACK
                       K = KPACK
                       L = LPACK
          IJ = IPIN(MAX(I,J)) + MIN(I,J)
          KL = IPIN(MAX(K,L)) + MIN(K,L)
          IJKL = IPIN(MAX(IJ,KL)) + MIN(IJ,KL)
          TPDM(IJKL) = GBUF(M)
        END DO  ! CURRENT BATCH 
      END DO  ! NEXT BATCH
      RETURN
      END
C*MODULE MCTWO   *DECK CHNRGY
      SUBROUTINE CHNRGY(NCOR,NACT,HAMO,OPDM,ECOR)
C
C -----------------------------------------------------------------
C
C  COMPUTE THE 1-EL (OR CORE-HAMILTONIAN) ENERGY
C
C  SYMBOLS:
C           NCOR = NUMBER OF CORE MOS
C           NACT = NUMBER OF ACTIVE MOS
C           HAMO = 1-EL CORE HAMILTONIAN INTEGRALS
C           OPDM = 1-EL DENSITY SPANNING ACTIVE INDICES
C           ECOR = 1-EL 'CORE' ENERGY
C
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MXAO=2047, ZERO=0.0D+00, TWO=2.0D+00) 
      DOUBLE PRECISION HAMO(*),OPDM(*)
      COMMON /IJPAIR/ IA(MXAO)
C
      ECOR = ZERO
      II = 0
      DO I = 1, NCOR
        II = II + I
        ECOR = ECOR + TWO*HAMO(II)
      END DO
C
      DO I = 1, NACT
        IN = I + NCOR
        DO J = 1, NACT
          JN   = J + NCOR
          IJ   = IA(MAX(I,J))   + MIN(I,J)
          IJN  = IA(MAX(IN,JN)) + MIN(IN,JN)
          ECOR = ECOR + OPDM(IJ)*HAMO(IJN)
        END DO
      END DO
      RETURN
      END
C*MODULE MCTWO   *DECK TRASH_ON
      SUBROUTINE TRASH_ON(NMOS,NOCC,NROT,IHES)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C  INSTEAD OF TESTING FOR CONTRIBUTIONS TO REDUNDANT ROTATIONS
C  THEY ARE AUTOMATICALLY SUMMED TO A FALSE EXTRA ROW OF THE 
C  HESSIAN - THE 'TRASH BIN'.
C  TO USE HESSIAN TRASH-BIN SET REDUNDANT POINTERS TO NROT+1
C
      INTEGER IHES(NMOS,*)
C
      LTRASH = NROT + 1
      DO J = 1, NOCC
        DO I = 1, NMOS
          IF (IHES(I,J).EQ.0) IHES(I,J) = LTRASH
        END DO
      END DO
      END
C*MODULE MCTWO   *DECK TRASH_OFF
      SUBROUTINE TRASH_OFF(NMOS,NOCC,NROT,IHES)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C  UNSET TRASH BIN - RESTORE ORIGINAL INDEXING SCHEME
C
      INTEGER IHES(NMOS,*)
C
      LTRASH = NROT + 1
      DO J = 1, NOCC
        DO I = 1, NMOS
          IF (IHES(I,J).EQ.LTRASH) IHES(I,J) = 0
        END DO
      END DO
      END
C
C GDF:  4/05/02  MODIFICATIONS TO SKIP INNER LOOPS
C
C*MODULE MCTWO   *DECK LH2DDI
      SUBROUTINE LH2DDI(NMOS,NCOR,NOCC,NROT
     &,                 OPDM,TPDM,ILAG,LAGN,IHES,HESS,BUFF,IPIN)
C
C -----------------------------------------------------------------
C
C  NEWTON-RAPHSON MCSCF. CALLED FROM SUBROUTINE NEWTON (MCSCF.SRC).
C  2-EL CONTRIBUTIONS TO THE LAGRANGIAN AND HESSIAN. 
C  SEE MOTECC-90 (ESCOM), P293, EQNS (B.71-75).
C  TERMS ASSOCIATED WITH FOCK OPERATORS ARE DONE IN SUBROUTINES 
C  PNTNLAG AND PNTNHES. 
C  TO MAKE USE OF PARALLEL TRANSFORMATION (TRANDDI), CORE AND 
C  ACTIVE MO INDICES ARE SUBSETS OF THE OCCUPIED MO LIST. 
C  TRANDDI IS CALLED FROM SUBROUTINE TRFMCX (IN MODULE TRANS.SRC).
C
C  SYMBOLS:
C           NMOS = TOTAL NUMBER OF MOS
C           NCOR = NUMBER OF CORE MOS
C           NOCC = NUMBER OF OCCUPIED MOS
C           NROT = NUMBER OF ROTATIONS
C           OPDM = 1-EL DENSITY SPANNING ACTIVE INDICES
C           TPDM = 2-EL DENSITY SPANNING ACTIVE INDICES
C           ILAG = INDEX TO LAGRANGIAN
C           LAGN = LAGRANGIAN MATRIX
C           IHES = INDEX TO HESSIAN
C           HESS = HESSIAN MATRIX
C           BUFF = A MESSAGE BUFFER
C           IPIN = PAIR INDEX
C
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (TWO=2.0D+00, FOUR=4.0D+00, EIGHT=8.0D+00)
      LOGICAL INJ,INK,JNK,KEL,LEJ,ANB
      INTEGER IHES(NMOS,*),ILAG(*),IPIN(*)
      INTEGER DDI_NP,DDI_ME, A,AH,B,BH,AB,BA
      DOUBLE PRECISION  OPDM(*),TPDM(*),LAGN(*),HESS(*),BUFF(*)
C
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
     *,               NDOOOO, NDVOOO, NDVVOO, NDVOVO
C
      NCP1 = NCOR + 1
      NVIR = NMOS - NOCC
      NOTR = (NOCC*NOCC+NOCC)/2
      NVTR = (NVIR*NVIR+NVIR)/2
      NVSQ = NVIR*NVIR
      CALL DDI_NPROC(DDI_NP,DDI_ME)
      NROT1 = NROT + 1
C
C ----------- CONTRIBUTIONS FROM (OO|OO) TYPE INTEGRALS -----------
C
      CALL DDI_DISTRIB(D_OOOO,DDI_ME,ILO,IHI,JLO,JHI)
C
C  (ACTIVE-CORE|**) TYPES
C
      DO I = NCP1, NOCC
        DO J = 1, NCOR
          IJ = IPIN(MAX(I,J)) + MIN(I,J)
#if defined(SPEC_CPU)
          CALL PARR_ABRT
#endif
          IF ((IJ.GE.JLO).AND.(IJ.LE.JHI)) THEN
            CALL DDI_GET(D_OOOO,1,NOTR,IJ,IJ,BUFF)
            ID = I - NCOR
            IJH = IHES(I,J)
C
C  1. L(JM) <- D(IM|KL)*(JM|KL), J CORE  (REST ACTIVE)
C
            DO K = NCP1, NOCC
              KD = K - NCOR
              DO L = NCP1, NOCC
                LD = L - NCOR
                KL  = IPIN(MAX(K ,L )) + MIN(K ,L )
                KLD = IPIN(MAX(KD,LD)) + MIN(KD,LD)
                ERI = BUFF(KL)
                DO M = NCP1, NOCC
                  MD = M - NCOR
                  IMD = IPIN(MAX(ID,MD)) + MIN(ID,MD)
                  IMKLD = IPIN(MAX(IMD,KLD)) + MIN(IMD,KLD)
                  JM  = ILAG(J) + M
                  LAGN(JM) = LAGN(JM) + TPDM(IMKLD)*ERI
                END DO
              END DO
            END DO
C
C  2. H(IJ|KL) <- 8*(IJ|KL), J,L CORE
C
            IF (IJH.NE.NROT1) THEN
              DO K = NCP1, I
                MX = J
                IF (I.NE.K) MX = NCOR
                DO L = 1, MX
                  KL = IPIN(MAX(K,L)) + MIN(K,L)
                  KLH = IHES(K,L)
                  IJKLH = IPIN(MAX(IJH,KLH)) + MIN(IJH,KLH)
                  HESS(IJKLH) = HESS(IJKLH) + EIGHT*BUFF(KL)
                END DO
              END DO
            END IF
C
C  3. H(IJ|KL) <- -2*(IL|JK), J,L CORE
C
            DO K = I, NOCC
              MX = J
              IF (I.NE.K) MX = NCOR
              KJH = IHES(K,J)
              IF (KJH.NE.NROT1) THEN
                DO L = 1, MX
                  KL = IPIN(MAX(K,L)) + MIN(K,L)
                  ILH = IHES(I,L)
                  ILKJH = IPIN(MAX(ILH,KJH)) + MIN(ILH,KJH)
                  HESS(ILKJH) = HESS(ILKJH) - TWO*BUFF(KL)
                END DO
              END IF
            END DO
C
C  4. H(JK|LM) <- -4*D(IK)*(IJ|LM), J,M CORE
C
            DO K = NCP1, NOCC
              KD = K - NCOR
              IKD = IPIN(MAX(ID,KD)) + MIN(ID,KD)
              JKH = IHES(K,J)
              IF (JKH.NE.NROT1) THEN
                DIK = FOUR*OPDM(IKD)
                DO L = NCP1, K
                  MX = J
                  IF (K.NE.L) MX = NCOR
                  DO M = 1, MX
                    LM  = IPIN(MAX(L,M)) + MIN(L,M)
                    LMH = IHES(L,M)
                    JKLMH = IPIN(MAX(JKH,LMH)) + MIN(JKH,LMH)
                    HESS(JKLMH) = HESS(JKLMH) - DIK*BUFF(LM)
                  END DO
                END DO
              END IF
            END DO
C
C  5. H(IL|JK) <- D(KM)*(IJ|LM), J,L CORE
C
            DO K = NCP1, NOCC
              IF (I.LE.K) THEN
                KD = K - NCOR
                JKH = IHES(K,J)
                IF (JKH.NE.NROT1) THEN
                  MX = J
                  IF (I.NE.K) MX = NCOR
                  DO L = 1, MX
                    ILH = IHES(I,L)
                    DO M = NCP1, NOCC
                      MD = M - NCOR
                      LM = IPIN(MAX(L,M)) + MIN(L,M)
                      KMD = IPIN(MAX(KD,MD)) + MIN(KD,MD)
                      ILJKH = IPIN(MAX(ILH,JKH)) + MIN(ILH,JKH)
                      HESS(ILJKH) = HESS(ILJKH) + OPDM(KMD)*BUFF(LM)
                    END DO
                  END DO
                END IF
              END IF
            END DO
C
C  6. H(IJ|KL) <- -4*D(KM)*(IJ|LM), J,L CORE
C
            IF (IJH.NE.NROT1) THEN
              DO K = NCP1, I
                KD = K - NCOR
                MX = J
                IF (I.NE.K) MX = NCOR
                DO M = NCP1, NOCC
                  MD = M - NCOR
                  KMD = IPIN(MAX(KD,MD)) + MIN(KD,MD)
                  DKM = OPDM(KMD)*FOUR
                  DO L = 1, MX
                    KLH = IHES(K,L)
                    IJKLH = IPIN(MAX(IJH,KLH)) + MIN(IJH,KLH)
                    LM  = IPIN(MAX(L,M)) + MIN(L,M)
                    HESS(IJKLH) = HESS(IJKLH) - DKM*BUFF(LM)
                  END DO
                END DO
              END DO
            END IF
C
C  7. H(JK|LM) <- D(IL)*(IJ|KM), J,M CORE
C
            DO K = NCP1, NOCC
              JKH = IHES(K,J)
              IF (JKH.NE.NROT1) THEN
                DO L = NCP1, K
                  LD = L - NCOR
                  ILD = IPIN(MAX(ID,LD)) + MIN(ID,LD)
                  DIL = OPDM(ILD)
                  MX = J
                  IF (K.NE.L) MX = NCOR
                  DO M = 1, MX
                    KM = IPIN(MAX(K,M)) + MIN(K,M)
                    LMH = IHES(L,M)
                    JKLMH = IPIN(MAX(JKH,LMH)) + MIN(JKH,LMH)
                    HESS(JKLMH) = HESS(JKLMH) + DIL*BUFF(KM)
                  END DO
                END DO
              END IF
            END DO
C
C  8. H(JK|LN) <- 2*D(IK|LM)*(IJ|MN), J,N CORE
C
            DO K = NCP1, NOCC
              KD = K - NCOR
              IKD = IPIN(MAX(ID,KD)) + MIN(ID,KD)
              JKH = IHES(K,J)
              IF (JKH.NE.NROT1) THEN
                DO L = NCP1, K
                  LD = L - NCOR
                  MX = J
                  IF (K.NE.L) MX = NCOR
                  DO M = NCP1, NOCC
                    MD = M - NCOR
                    LMD = IPIN(MAX(LD,MD)) + MIN(LD,MD)
                    IKLMD = IPIN(MAX(IKD,LMD)) + MIN(IKD,LMD)
                    DD = TPDM(IKLMD)*TWO
                    DO N = 1, MX
                      MN = IPIN(MAX(M,N)) + MIN(M,N)
                      LNH = IHES(L,N)
                      JKLNH = IPIN(MAX(JKH,LNH)) + MIN(JKH,LNH)
                      HESS(JKLNH) = HESS(JKLNH) + DD*BUFF(MN)
                    END DO
                  END DO
                END DO
              END IF
            END DO
C
C  9. H(IJ|KM) <- 4*D(LM)*(IJ|KL) - D(LM)*(IK|JL), J CORE
C
            DO K = NCP1, NOCC
              DO L = NCP1, K-1
                KLH = IHES(K,L)
                IF (KLH.NE.NROT1) THEN
                  IJKLH = IPIN(MAX(IJH,KLH)) + MIN(IJH,KLH)
                  LD = L - NCOR
                  ILD = IPIN(MAX(ID,LD)) + MIN(ID,LD)
                  DIL = OPDM(ILD)
                  DO M = NCP1, NOCC
                    MD = M - NCOR
                    LMD = IPIN(MAX(LD,MD)) + MIN(LD,MD)
                    DLM = OPDM(LMD)*FOUR
                    KM = IPIN(MAX(K,M)) + MIN(K,M)
                    ERI = BUFF(KM)
                    JMH = IHES(J,M)
                    JMKLH = IPIN(MAX(JMH,KLH)) + MIN(JMH,KLH)
                    HESS(IJKLH) = HESS(IJKLH) + ERI*DLM
                    HESS(JMKLH) = HESS(JMKLH) - ERI*DIL
                  END DO 
                END IF
              END DO 
            END DO 
C
C  10. H(IL|JK) <- -D(LM)*(IJ|KM), J CORE
C
            DO K = NCP1, NOCC
              JKH = IHES(K,J)
              IF (JKH.NE.NROT1) THEN
                DO L = NCP1, I-1
                  LD = L - NCOR
                  ILH = IHES(I,L)
                  DO M = NCP1, NOCC
                    MD = M - NCOR
                    LMD = IPIN(MAX(LD,MD)) + MIN(LD,MD)
                    KM  = IPIN(MAX(K,M)) + MIN(K,M)
                    ILJKH = IPIN(MAX(ILH,JKH)) + MIN(ILH,JKH)
                    HESS(ILJKH) = HESS(ILJKH) - OPDM(LMD)*BUFF(KM)
                  END DO
                END DO
              END IF
            END DO
C
C  11. H(KL|IJ) <- -4*D(KM)*(IJ|LM) + D(KM)*(IL|JM), J CORE
C
            DO K = NCP1, NOCC
              KD = K - NCOR
              IKD = IPIN(MAX(ID,KD)) + MIN(ID,KD)
              DIK = OPDM(IKD)
              DO L = NCP1, K-1
                KLH = IHES(K,L)
                IF (KLH.NE.NROT1) THEN
                  IJKLH = IPIN(MAX(IJH,KLH)) + MIN(IJH,KLH)
                  DO M = NCP1, NOCC
                    MD = M - NCOR
                    KMD = IPIN(MAX(KD,MD)) + MIN(KD,MD)
                    DKM = OPDM(KMD)*FOUR
                    LM = IPIN(MAX(L,M)) + MIN(L,M)
                    ERI = BUFF(LM)
                    JMH = IHES(M,J)
                    JMKLH = IPIN(MAX(JMH,KLH)) + MIN(JMH,KLH)
                    HESS(IJKLH) = HESS(IJKLH) - DKM*ERI
                    HESS(JMKLH) = HESS(JMKLH) + DIK*ERI
                  END DO
                END IF
              END DO
            END DO
C
C  12. H(IM|JN) <- -D(KL|MN)*(IJ|KL), J CORE
C
            DO K = NCP1, NOCC
              KD = K - NCOR
              DO L = NCP1, NOCC
                LD = L - NCOR
                KLD = IPIN(MAX(KD,LD)) + MIN(KD,LD)
                KL = IPIN(MAX(K,L)) + MIN(K,L)
                ERI = BUFF(KL)
                DO M = NCP1, I-1
                  MD = M - NCOR
                  IMH = IHES(I,M)
                  IF (IMH.NE.NROT1) THEN
                    DO N = NCP1, NOCC
                      ND = N - NCOR
                      MND = IPIN(MAX(MD,ND)) + MIN(MD,ND)
                      KLMND = IPIN(MAX(KLD,MND)) + MIN(KLD,MND)
                      JNH = IHES(N,J)
                      IMJNH = IPIN(MAX(IMH,JNH)) + MIN(IMH,JNH)
                      HESS(IMJNH) = HESS(IMJNH) - TPDM(KLMND)*ERI
                    END DO
                  END IF
                END DO
              END DO
            END DO
C
C  13. H(KL|JN) <- -2*D(IN|LM)*(IJ|KM), J CORE
C
            DO K = NCP1, NOCC
              DO L = NCP1, K-1
                LD = L - NCOR
                KLH = IHES(K,L)
                IF (KLH.NE.NROT1) THEN
                  DO M = NCP1, NOCC
                    MD = M - NCOR
                    LMD = IPIN(MAX(LD,MD)) + MIN(LD,MD)
                    KM = IPIN(MAX(K,M)) + MIN(K,M)
                    ERI = BUFF(KM)*TWO
                    DO N = NCP1, NOCC
                      ND = N - NCOR
                      IND = IPIN(MAX(ID,ND)) + MIN(ID,ND)
                      INLMD = IPIN(MAX(IND,LMD)) + MIN(IND,LMD)
                      JNH = IHES(N,J)
                      KLJNH = IPIN(MAX(KLH,JNH)) + MIN(KLH,JNH)
                      HESS(KLJNH) = HESS(KLJNH) - TPDM(INLMD)*ERI
                    END DO
                  END DO
                END IF
              END DO
            END DO
C
C  14. H(KL|JN) <- 2*D(KM|IN)*(IJ|LM), J CORE
C
            DO K = NCP1, NOCC
              KD = K - NCOR
              DO L = NCP1, K-1
                KLH = IHES(K,L)
                IF (KLH.NE.NROT1) THEN
                  DO M = NCP1, NOCC
                    MD = M - NCOR
                    KMD = IPIN(MAX(KD,MD)) + MIN(KD,MD)
                    LM = IPIN(MAX(L,M)) + MIN(L,M)
                    ERI = BUFF(LM)*TWO
                    DO N = NCP1, NOCC
                      ND = N - NCOR
                      IND = IPIN(MAX(ID,ND)) + MIN(ID,ND)
                      KMIND = IPIN(MAX(KMD,IND)) + MIN(KMD,IND)
                      JNH = IHES(N,J)
                      KLJNH = IPIN(MAX(KLH,JNH)) + MIN(KLH,JNH)
                      HESS(KLJNH) = HESS(KLJNH) + TPDM(KMIND)*ERI
                    END DO
                  END DO
                END IF
              END DO
            END DO
C
C  END DISTRIBUTION LOOPS
C
          END IF  ! LOCAL
        END DO  ! J
      END DO  ! I
C
C  (ACTIVE-ACTIVE|**) TYPES
C
      DO I = NCP1, NOCC
        DO J = NCP1, I
          IJ = IPIN(MAX(I,J)) + MIN(I,J)
          IF ((IJ.GE.JLO).AND.(IJ.LE.JHI)) THEN
            CALL DDI_GET(D_OOOO,1,NOTR,IJ,IJ,BUFF)
            ID = I - NCOR
            JD = J - NCOR
            INJ = I.NE.J
            IJD = IPIN(MAX(ID,JD)) + MIN(ID,JD)
C
C  15. L(KM) <- D(IJ|LM)*(IJ|KL) - D(IJ|KL)*(IJ|LM)
C
            DO K = NCP1, NOCC
              DO L = NCP1, NOCC
                LD = L - NCOR
                KL = IPIN(MAX(K,L)) + MIN(K,L)
                ERI = BUFF(KL)
                IF (INJ) ERI = ERI*TWO
                DO M = NCP1, NOCC
                  MD = M - NCOR
                  LMD = IPIN(MAX(LD,MD)) + MIN(LD,MD)
                  IJLMD = IPIN(MAX(IJD,LMD)) + MIN(IJD,LMD)
                  KM = ILAG(K) + M
                  LAGN(KM) = LAGN(KM) + TPDM(IJLMD)*ERI
                END DO
              END DO
            END DO
C
C  16. H(IK|JL) <- -2*(IJ|KL), K,L CORE
C
            DO K = 1, NCOR
              IKH = IHES(I,K)
              IF (IKH.NE.NROT1) THEN
                MX = K
                IF (INJ) MX = NCOR
                DO L = 1, MX
                  KL = IPIN(MAX(K,L)) + MIN(K,L)
                  JLH = IHES(J,L)
                  IKJLH = IPIN(MAX(IKH,JLH)) + MIN(IKH,JLH)
                  HESS(IKJLH) = HESS(IKJLH) - TWO*BUFF(KL)
                END DO
              END IF
            END DO
C
C  17. H(KL|JM) <- D(IK)*(IJ|LM), L,M CORE
C
            DO K = NCP1, NOCC
              KD = K - NCOR
              INK = I.NE.K
              JNK = J.NE.K
              IF (J.LE.K) THEN
                IKD = IPIN(MAX(ID,KD)) + MIN(ID,KD)
                DIK = OPDM(IKD)
                DO L = 1, NCOR
                  KLH = IHES(K,L)
                  IF (KLH.NE.NROT1) THEN
                    MX = L
                    IF (JNK) MX = NCOR
                    DO M = 1, MX
                      LM = IPIN(MAX(L,M)) + MIN(L,M)
                      JMH = IHES(J,M)
                      KLJMH = IPIN(MAX(KLH,JMH)) + MIN(KLH,JMH)
                      HESS(KLJMH) = HESS(KLJMH) + DIK*BUFF(LM)
                    END DO
                  END IF
                END DO
              END IF
              IF (INJ.AND.(I.LE.K)) THEN
                JKD = IPIN(MAX(JD,KD)) + MIN(JD,KD)
                DJK = OPDM(JKD)
                DO L = 1, NCOR
                  KLH = IHES(K,L)
                  IF (KLH.NE.NROT1) THEN
                    MX = L
                    IF (INK) MX = NCOR
                    DO M = 1, MX
                      LM = IPIN(MAX(L,M)) + MIN(L,M)
                      IMH = IHES(I,M)
                      KLIMH = IPIN(MAX(KLH,IMH)) + MIN(KLH,IMH)
                      HESS(KLIMH) = HESS(KLIMH) + DJK*BUFF(LM)
                    END DO
                  END IF
                END DO
              END IF
            END DO
C
C  19. H(JK|LM) <- D(JL)*(IJ|KM), K,M CORE
C
            DO K = 1, NCOR
              IKH = IHES(I,K)
              IF (IKH.NE.NROT1) THEN
                DO L = NCP1, I
                  LD = L - NCOR
                  JLD = IPIN(MAX(JD,LD)) + MIN(JD,LD)
                  DJL = OPDM(JLD)
                  MX = K
                  IF (I.NE.L) MX = NCOR
                  DO M = 1, MX
                    KM = IPIN(MAX(K,M)) + MIN(K,M)
                    LMH = IHES(L,M)
                    IKLMH = IPIN(MAX(IKH,LMH)) + MIN(IKH,LMH)
                    HESS(IKLMH) = HESS(IKLMH) + DJL*BUFF(KM)
                  END DO
                END DO
              END IF
              IF (INJ) THEN
                JKH = IHES(J,K)
                IF (JKH.NE.NROT1) THEN
                  DO L = NCP1, J
                    LD = L - NCOR
                    ILD = IPIN(MAX(ID,LD)) + MIN(ID,LD)
                    DIL = OPDM(ILD)
                    MX = K
                    IF (J.NE.L) MX = NCOR
                    DO M = 1, MX
                      KM = IPIN(MAX(K,M)) + MIN(K,M)
                      LMH = IHES(L,M)
                      JKLMH = IPIN(MAX(JKH,LMH)) + MIN(JKH,LMH)
                      HESS(JKLMH) = HESS(JKLMH) + DIL*BUFF(KM)
                    END DO
                  END DO
                END IF
              END IF
            END DO
C
C  20. H(KL|MN) <- D(IJ|KM)*(IJ|LN), L,N CORE
C
            DO K = NCP1, NOCC
              KD = K - NCOR
              DO L = 1, NCOR
                KLH = IHES(K,L)
                IF (KLH.NE.NROT1) THEN
                  DO M = NCP1, K
                    MD = M - NCOR
                    KMD = IPIN(MAX(KD,MD)) + MIN(KD,MD)
                    IJKMD = IPIN(MAX(IJD,KMD)) + MIN(IJD,KMD)
                    DD = TPDM(IJKMD)
                    IF (INJ) DD = DD*TWO
                    MX = L
                    IF (K.NE.M) MX = NCOR
                    DO N = 1, MX
                      LN = IPIN(MAX(L,N)) + MIN(L,N)
                      MNH = IHES(M,N)
                      KLMNH = IPIN(MAX(KLH,MNH)) + MIN(KLH,MNH)
                      HESS(KLMNH) = HESS(KLMNH) + DD*BUFF(LN)
                    END DO
                  END DO
                END IF
              END DO
            END DO
C
C  21. H(IK|LM) <- D(JL)*(IJ|KM), K CORE
C
            DO K = 1, NCOR
              IKH = IHES(I,K)
              DO L = NCP1, NOCC
                LD = L - NCOR
                JLD = IPIN(MAX(JD,LD)) + MIN(JD,LD)
                DJL = OPDM(JLD)
                DO M = NCP1, L-1
                  KM = IPIN(MAX(K,M)) + MIN(K,M)
                  LMH = IHES(L,M)
                  IKLMH = IPIN(MAX(IKH,LMH)) + MIN(IKH,LMH)
                  HESS(IKLMH) = HESS(IKLMH) + DJL*BUFF(KM)
                END DO
                IF (INJ) THEN
                  JKH = IHES(J,K)
                  ILD = IPIN(MAX(ID,LD)) + MIN(ID,LD)
                  DIL = OPDM(ILD)
                  DO M = NCP1, L-1
                    KM = IPIN(MAX(K,M)) + MIN(K,M)
                    LMH = IHES(L,M)
                    JKLMH = IPIN(MAX(JKH,LMH)) + MIN(JKH,LMH)
                    HESS(JKLMH) = HESS(JKLMH) + DIL*BUFF(KM)
                  END DO
                END IF
              END DO
            END DO
C
C  22. H(KL|MN) <- D(IJ|KM)*(IJ|LN), N CORE
C
            DO K = NCP1, NOCC
              KD = K - NCOR
              DO L = NCP1, K-1
                KLH = IHES(K,L)
                IF (KLH.NE.NROT1) THEN
                  DO M = NCP1, NOCC
                    MD = M - NCOR
                    KMD = IPIN(MAX(KD,MD)) + MIN(KD,MD)
                    IJKMD = IPIN(MAX(IJD,KMD)) + MIN(IJD,KMD)
                    DD = TPDM(IJKMD)
                    IF (INJ) DD = DD*TWO
                    DO N = 1, NCOR
                      LN = IPIN(MAX(L,N)) + MIN(L,N)
                      MNH = IHES(M,N)
                      KLMNH = IPIN(MAX(KLH,MNH)) + MIN(KLH,MNH)
                      HESS(KLMNH) = HESS(KLMNH) + DD*BUFF(LN)
                    END DO
                  END DO
                END IF
              END DO
            END DO
C
C  23. H(IM|LN) <- 2*D(JM|KN)*(IJ|KL)
C
            DO K = NCP1, NOCC
              KD = K - NCOR
              DO L = NCP1, I
                KL = IPIN(MAX(K,L)) + MIN(K,L)
                ERI = BUFF(KL)*TWO
                DO M = NCP1, I-1
                  IMH = IHES(I,M)
                  IF (IMH.NE.NROT1) THEN
                    MD = M - NCOR
                    JMD = IPIN(MAX(JD,MD)) + MIN(JD,MD)
                    MX = L-1
                    IF (L.EQ.I) MX = M
                    DO N = NCP1, MX
                      ND = N - NCOR
                      KND = IPIN(MAX(KD,ND)) + MIN(KD,ND)
                      JMKND = IPIN(MAX(JMD,KND)) + MIN(JMD,KND)
                      LNH = IHES(L,N)
                      IMLNH = IPIN(MAX(IMH,LNH)) + MIN(IMH,LNH)
                      HESS(IMLNH) = HESS(IMLNH) + TPDM(JMKND)*ERI
                    END DO
                  END IF
                END DO
              END DO
            END DO
            IF (INJ) THEN
              DO K = NCP1, NOCC
                KD = K - NCOR
                DO L = NCP1, J
                  KL = IPIN(MAX(K,L)) + MIN(K,L)
                  ERI = BUFF(KL)*TWO
                  LEJ = L.EQ.J
                  DO M = NCP1, J-1
                    JMH = IHES(J,M)
                    IF (JMH.NE.NROT1) THEN
                      MD  = M - NCOR
                      IMD = IPIN(MAX(ID,MD)) + MIN(ID,MD)
                      MX = L-1
                      IF (LEJ) MX = M
                      DO N = NCP1, MX
                        ND = N - NCOR
                        KND = IPIN(MAX(KD,ND)) + MIN(KD,ND)
                        IMKND = IPIN(MAX(IMD,KND)) + MIN(IMD,KND)
                        LNH = IHES(L,N)
                        JMLNH = IPIN(MAX(JMH,LNH)) + MIN(JMH,LNH)
                        HESS(JMLNH) = HESS(JMLNH) + TPDM(IMKND)*ERI
                      END DO
                    END IF
                  END DO
                END DO
              END DO
            END IF
C
C  24. H(KM|LN) <- D(IJ|MN)*(IJ|KL)
C
            DO K = NCP1, NOCC
              DO L = NCP1, K
                KL = IPIN(MAX(K,L)) + MIN(K,L)
                ERI = BUFF(KL)
                IF (INJ) ERI = ERI*TWO
                KEL = K.EQ.L
                DO M = NCP1, K-1
                  KMH = IHES(K,M)
                  IF (KMH.NE.NROT1) THEN
                    MD = M - NCOR
                    MX = L-1
                    IF (KEL) MX = M
                    DO N = NCP1, MX
                      ND = N - NCOR
                      MND = IPIN(MAX(MD,ND)) + MIN(MD,ND)
                      IJMND = IPIN(MAX(IJD,MND)) + MIN(IJD,MND)
                      LNH = IHES(L,N)
                      KMLNH = IPIN(MAX(KMH,LNH)) + MIN(KMH,LNH)
                      HESS(KMLNH) = HESS(KMLNH) + TPDM(IJMND)*ERI
                    END DO
                  END IF
                END DO
              END DO
            END DO
C
C  25. H(KL|MN) <- D(IJ|KM)*(IJ|LN)
C
            DO K = NCP1, NOCC
              KD = K - NCOR
              DO L = NCP1, K-1
                KLH = IHES(K,L)
                IF (KLH.NE.NROT1) THEN
                  DO M = NCP1, K
                    MD = M - NCOR
                    KMD = IPIN(MAX(KD,MD)) + MIN(KD,MD)
                    IJKMD = IPIN(MAX(IJD,KMD)) + MIN(IJD,KMD)
                    DD = TPDM(IJKMD)
                    IF (INJ) DD = DD*TWO
                    MX = M-1
                    IF (K.EQ.M) MX = L
                    DO N = NCP1, MX
                      LN = IPIN(MAX(L,N)) + MIN(L,N)
                      MNH = IHES(M,N)
                      KLMNH = IPIN(MAX(KLH,MNH)) + MIN(KLH,MNH)
                      HESS(KLMNH) = HESS(KLMNH) + DD*BUFF(LN)
                    END DO
                  END DO
                END IF
              END DO
            END DO
C
C  26. H(JK|LN) <- 2*D(IK|LM)*(IJ|MN)
C
            DO K = NCP1, NOCC
              KD = K - NCOR
              IF (J.LT.K) THEN
                JKH = IHES(K,J)
                IF (JKH.NE.NROT1) THEN
                  IKD = IPIN(MAX(ID,KD)) + MIN(ID,KD)
                  DO L = NCP1, K
                    LD = L - NCOR
                    MX = L-1
                    IF (L.EQ.K) MX = J
                    DO M = NCP1, NOCC
                      MD = M - NCOR
                      LMD = IPIN(MAX(LD,MD)) + MIN(LD,MD)
                      IKLMD = IPIN(MAX(IKD,LMD)) + MIN(IKD,LMD)
                      DI = TPDM(IKLMD)*TWO
                      DO N = NCP1, MX
                        MN = IPIN(MAX(M,N)) + MIN(M,N)
                        LNH = IHES(L,N)
                        JKLNH = IPIN(MAX(JKH,LNH)) + MIN(JKH,LNH)
                        HESS(JKLNH) = HESS(JKLNH) + DI*BUFF(MN)
                      END DO
                    END DO
                  END DO
                END IF
              END IF
              IF (INJ.AND.(I.LT.K)) THEN
                IKH = IHES(I,K)
                IF (IKH.NE.NROT1) THEN
                  JKD = IPIN(MAX(JD,KD)) + MIN(JD,KD)
                  DO L = NCP1, K
                    LD = L - NCOR
                    MX = L-1
                    IF (L.EQ.K) MX = I
                    DO M = NCP1, NOCC
                      MD = M - NCOR
                      LMD = IPIN(MAX(LD,MD)) + MIN(LD,MD)
                      JKLMD = IPIN(MAX(JKD,LMD)) + MIN(JKD,LMD)
                      DJ = TPDM(JKLMD)*TWO
                      DO N = NCP1, MX
                        MN = IPIN(MAX(M,N)) + MIN(M,N)
                        LNH = IHES(L,N)
                        IKLNH = IPIN(MAX(IKH,LNH)) + MIN(IKH,LNH)
                        HESS(IKLNH) = HESS(IKLNH) + DJ*BUFF(MN)
                      END DO
                    END DO
                  END DO
                END IF
              END IF
            END DO
C
C  27. H(KL|MN) <- -D(IJ|KN)*(IJ|LM)
C
            DO K = NCP1, NOCC
              KD = K - NCOR
              DO L = NCP1, K-1
                KLH = IHES(K,L)
                IF (KLH.NE.NROT1) THEN
                  DO M = NCP1, K
                    LM = IPIN(MAX(L,M)) + MIN(L,M)
                    ERI = BUFF(LM)
                    IF (INJ) ERI = ERI*TWO
                    MX = M-1
                    IF (K.EQ.M) MX = L
                    DO N = NCP1, MX
                      ND = N - NCOR
                      KND = IPIN(MAX(KD,ND)) + MIN(KD,ND)
                      IJKND = IPIN(MAX(IJD,KND)) + MIN(IJD,KND)
                      MNH = IHES(M,N)
                      KLMNH = IPIN(MAX(KLH,MNH)) + MIN(KLH,MNH)
                      HESS(KLMNH) = HESS(KLMNH) - TPDM(IJKND)*ERI
                    END DO
                  END DO
                END IF
              END DO
            END DO
C
C  28.  H(KL|MN) <- -2*D(IJ|KN)*(IJ|LM)
C
            DO K = NCP1, NOCC
              KD = K - NCOR
              IF (J.LT.K) THEN
                JKH = IHES(K,J)
                IF (JKH.NE.NROT1) THEN
                  IKD = IPIN(MAX(ID,KD)) + MIN(ID,KD)
                  DO L = NCP1, K
                    MX = L-1
                    IF (K.EQ.L) MX = J
                    DO M = NCP1, NOCC
                      MD = M - NCOR
                      LM = IPIN(MAX(L,M)) + MIN(L,M)
                      ERI = BUFF(LM)*TWO
                      DO N = NCP1, MX
                        ND = N - NCOR
                        MND = IPIN(MAX(MD,ND)) + MIN(MD,ND)
                        IKMND = IPIN(MAX(IKD,MND)) + MIN(IKD,MND)
                        LNH = IHES(L,N)
                        JKLNH = IPIN(MAX(JKH,LNH)) + MIN(JKH,LNH)
                        HESS(JKLNH) = HESS(JKLNH) - TPDM(IKMND)*ERI
                      END DO
                    END DO
                  END DO
                END IF
              END IF
              IF (INJ.AND.(I.LT.K)) THEN
                IKH = IHES(K,I)
                IF (IKH.NE.NROT1) THEN
                  JKD = IPIN(MAX(JD,KD)) + MIN(JD,KD)
                  DO L = NCP1, K
                    MX = L-1
                    IF (K.EQ.L) MX = I
                    DO M = NCP1, NOCC
                      MD = M - NCOR
                      LM = IPIN(MAX(L,M)) + MIN(L,M)
                      ERI = BUFF(LM)*TWO
                      DO N = NCP1, MX
                        ND = N - NCOR
                        MND = IPIN(MAX(MD,ND)) + MIN(MD,ND)
                        JKMND = IPIN(MAX(JKD,MND)) + MIN(JKD,MND)
                        LNH = IHES(L,N)
                        IKLNH = IPIN(MAX(IKH,LNH)) + MIN(IKH,LNH)
                        HESS(IKLNH) = HESS(IKLNH) - TPDM(JKMND)*ERI
                      END DO
                    END DO
                  END DO
                END IF
              END IF
            END DO
C
C  29. H(KL|MN) <- -D(IJ|LM)*(IJ|KN)
C
            DO K = NCP1, NOCC
              DO L = NCP1, K-1
                LD = L - NCOR
                KLH = IHES(K,L)
                IF (KLH.NE.NROT1) THEN
                  DO M = NCP1, K
                    MD = M - NCOR
                    LMD = IPIN(MAX(LD,MD)) + MIN(LD,MD)
                    IJLMD = IPIN(MAX(IJD,LMD)) + MIN(IJD,LMD)
                    DD = TPDM(IJLMD)
                    IF (INJ) DD = DD*TWO
                    MX = M-1
                    IF (K.EQ.M) MX = L
                    DO N = NCP1, MX
                      KN = IPIN(MAX(K,N)) + MIN(K,N)
                      MNH = IHES(M,N)
                      KLMNH = IPIN(MAX(KLH,MNH)) + MIN(KLH,MNH)
                      HESS(KLMNH) = HESS(KLMNH) - DD*BUFF(KN)
                    END DO
                  END DO
                END IF
              END DO
            END DO
C
C  30. H(IK|LN) <- -2*D(JK|LM)*(IJ|MN)
C
            DO K = NCP1, I-1
              IKH = IHES(I,K)
              IF (IKH.NE.NROT1) THEN
                KD = K - NCOR
                JKD = IPIN(MAX(JD,KD)) + MIN(JD,KD)
                DO L = NCP1, I
                  LD = L - NCOR
                  MX = L-1
                  IF (L.EQ.I) MX = K
                  DO M = NCP1, NOCC
                    MD = M - NCOR
                    LMD = IPIN(MAX(LD,MD)) + MIN(LD,MD)
                    JKLMD = IPIN(MAX(JKD,LMD)) + MIN(JKD,LMD)
                    DJ = TPDM(JKLMD)*TWO
                    DO N = NCP1, MX
                      MN = IPIN(MAX(M,N)) + MIN(M,N)
                      LNH = IHES(L,N)
                      IKLNH = IPIN(MAX(IKH,LNH)) + MIN(IKH,LNH)
                      HESS(IKLNH) = HESS(IKLNH) - DJ*BUFF(MN)
                    END DO
                  END DO
                END DO
              END IF
            END DO
            IF (INJ) THEN
              DO K = NCP1, J-1
                JKH = IHES(J,K)
                IF (JKH.NE.NROT1) THEN
                  KD = K - NCOR
                  IKD = IPIN(MAX(ID,KD)) + MIN(ID,KD)
                  DO L = NCP1, J
                    LD = L - NCOR
                    MX = L-1
                    IF (L.EQ.J) MX = K
                    DO M = NCP1, NOCC
                      MD = M - NCOR
                      LMD = IPIN(MAX(LD,MD)) + MIN(LD,MD)
                      IKLMD = IPIN(MAX(IKD,LMD)) + MIN(IKD,LMD)
                      DI = TPDM(IKLMD)*TWO
                      DO N = NCP1, MX
                        MN = IPIN(MAX(M,N)) + MIN(M,N)
                        LNH = IHES(L,N)
                        JKLNH = IPIN(MAX(JKH,LNH)) + MIN(JKH,LNH)
                        HESS(JKLNH) = HESS(JKLNH) - DI*BUFF(MN)
                      END DO
                    END DO
                  END DO
                END IF
              END DO
            END IF
C
C  END DISTRIBUTION LOOPS
C
          END IF  ! LOCAL
        END DO  ! J
      END DO  ! I
C
C ----------- CONTRIBUTIONS FROM (VO|OO) TYPE INTEGRALS -----------
C
      CALL DDI_DISTRIB(D_VOOO,DDI_ME,ILO,IHI,JLO,JHI)
C
C  (ACTIVE-CORE|CORE-VIRTUAL)
C
      DO I = NCP1, NOCC
        DO J = 1, NCOR
          IJ = IPIN(MAX(I,J)) + MIN(I,J)
          DO K = 1, NCOR
            IJK = (IJ-1)*NOCC + K
            IF ((IJK.GE.JLO).AND.(IJK.LE.JHI)) THEN
              CALL DDI_GET(D_VOOO,1,NVIR,IJK,IJK,BUFF)
C
C  31. H(AK|IJ) <- 8*(AK|IJ) - 2*(AJ|IK), J,K CORE, A VIRTUAL
C
              IJH = IHES(I,J)
              IF (IJH.NE.NROT1) THEN
                DO A = 1, NVIR
                  AH = A + NOCC
                  ERI = BUFF(A)
                  KAH = IHES(AH,K)
                  IJKAH = IPIN(MAX(IJH,KAH)) + MIN(IJH,KAH)
                  HESS(IJKAH) = HESS(IJKAH) + ERI*EIGHT
                END DO
              END IF
C
C  32. H(AJ|IK) <- -2*(AJ|IK), J,K CORE, A VIRTUAL
C
              IKH = IHES(I,K)
              IF (IKH.NE.NROT1) THEN
                DO A = 1, NVIR
                  AH = A + NOCC
                  ERI = BUFF(A)
                  JAH = IHES(AH,J)
                  IKJAH = IPIN(MAX(IKH,JAH)) + MIN(IKH,JAH)
                  HESS(IKJAH) = HESS(IKJAH) - ERI*TWO
                END DO
              END IF
C
C  33. H(AK|LJ) <- -4*D(IL)*(AK|IJ), J,K CORE
C
              ID = I - NCOR
              DO A = 1, NVIR
                ERI = BUFF(A)
                AH = A + NOCC
                KAH = IHES(AH,K)
                IF (KAH.NE.NROT1) THEN
                  DO L = NCP1, NOCC
                    LD = L - NCOR
                    ILD = IPIN(MAX(ID,LD)) + MIN(ID,LD)
                    FAC = OPDM(ILD)*ERI
                    JLH = IHES(L,J)
                    JLKAH = IPIN(MAX(JLH,KAH)) + MIN(JLH,KAH)
                    HESS(JLKAH) = HESS(JLKAH) - FAC*FOUR
                  END DO
                END IF
C
C  34. H(AJ|LK) <- D(IL)*(AK|IJ), J,K CORE
C
                JAH = IHES(AH,J)
                IF (JAH.NE.NROT1) THEN
                  DO L = NCP1, NOCC
                    LD = L - NCOR
                    ILD = IPIN(MAX(ID,LD)) + MIN(ID,LD)
                    FAC = OPDM(ILD)*ERI
                    KLH = IHES(L,K)
                    KLJAH = IPIN(MAX(KLH,JAH)) + MIN(KLH,JAH)
                    HESS(KLJAH) = HESS(KLJAH) + FAC
                  END DO
                END IF
              END DO
C
C  END DISTRIBUTION LOOPS
C
            END IF  ! LOCAL
          END DO  ! K
        END DO  ! J
      END DO  ! I
C
C  (CORE-CORE|ACTIVE-VIRTUAL)
C
      DO I = 1, NCOR
        DO J = 1, I
          IJ = IPIN(MAX(I,J)) + MIN(I,J)
          DO K = NCP1, NOCC
            IJK = (IJ-1)*NOCC + K
            IF ((IJK.GE.JLO).AND.(IJK.LE.JHI)) THEN
              CALL DDI_GET(D_VOOO,1,NVIR,IJK,IJK,BUFF)
              INJ = I.NE.J
C
C  35. H(AI|JK) <- -2*(AK|IJ), I,J CORE
C
              JKH = IHES(K,J)
              IF (JKH.NE.NROT1) THEN
                DO A = 1, NVIR
                  AH = A + NOCC
                  IAH = IHES(AH,I)
                  JKIAH = IPIN(MAX(JKH,IAH)) + MIN(JKH,IAH)
                  HESS(JKIAH) = HESS(JKIAH) - BUFF(A)*TWO
                END DO
              END IF
              IF (INJ) THEN
                IKH = IHES(K,I)
                IF (IKH.NE.NROT1) THEN
                  DO A = 1, NVIR
                    AH = A + NOCC
                    JAH = IHES(AH,J)
                    IKJAH = IPIN(MAX(IKH,JAH)) + MIN(IKH,JAH)
                    HESS(IKJAH) = HESS(IKJAH) - BUFF(A)*TWO
                  END DO
                END IF
              END IF
C
C  36. H(AI|LJ) <- D(KL)*(AK|IJ), I,J CORE
C
              KD = K - NCOR
              DO A = 1, NVIR
                ERI = BUFF(A)
                AH = A + NOCC
                IAH = IHES(AH,I)
                IF (IAH.NE.NROT1) THEN
                  DO L = NCP1, NOCC
                    LD = L - NCOR
                    KLD = IPIN(MAX(KD,LD)) + MIN(KD,LD)
                    FAC = OPDM(KLD)*ERI
                    LJH = IHES(L,J)
                    LJIAH = IPIN(MAX(LJH,IAH)) + MIN(LJH,IAH)
                    HESS(LJIAH) = HESS(LJIAH) + FAC
                  END DO
                END IF
                IF (INJ) THEN
                  JAH = IHES(AH,J)
                  IF (JAH.NE.NROT1) THEN
                    DO L = NCP1, NOCC
                      LD = L - NCOR
                      KLD = IPIN(MAX(KD,LD)) + MIN(KD,LD)
                      FAC = OPDM(KLD)*ERI
                      LIH = IHES(L,I)
                      LIJAH = IPIN(MAX(LIH,JAH)) + MIN(LIH,JAH)
                      HESS(LIJAH) = HESS(LIJAH) + FAC
                    END DO
                  END IF
                END IF
              END DO
C
C  END DISTRIBUTION LOOPS
C
            END IF  ! LOCAL
          END DO  ! K
        END DO  ! J
      END DO  ! I
C
C  (ACTIVE-ACTIVE|CORE-VIRTUAL) 
C
      DO I = NCP1, NOCC
        DO J = NCP1, I
          IJ = IPIN(MAX(I,J)) + MIN(I,J)
          DO K = 1, NCOR
            IJK = (IJ-1)*NOCC + K
            IF ((IJK.GE.JLO).AND.(IJK.LE.JHI)) THEN
              CALL DDI_GET(D_VOOO,1,NVIR,IJK,IJK,BUFF)
              INJ = I.NE.J
              ID = I - NCOR
              JD = J - NCOR
C
C  37. H(AK|IL) <- 4*D(JL)*(AK|IJ), K CORE
C
              DO A = 1, NVIR
                ERI = BUFF(A)*FOUR
                AH = A + NOCC
                KAH = IHES(AH,K)
                IF (KAH.NE.NROT1) THEN
                  DO L = NCP1, I-1
                    LD = L - NCOR
                    JLD = IPIN(MAX(JD,LD)) + MIN(JD,LD)
                    ILH = IHES(I,L)
                    ILKAH = IPIN(MAX(ILH,KAH)) + MIN(ILH,KAH)
                    HESS(ILKAH) = HESS(ILKAH) + OPDM(JLD)*ERI 
                  END DO
                  IF (INJ) THEN
                    DO L = NCP1, J-1
                      LD = L - NCOR
                      ILD = IPIN(MAX(ID,LD)) + MIN(ID,LD)
                      JLH = IHES(J,L)
                      JLKAH = IPIN(MAX(JLH,KAH)) + MIN(JLH,KAH)
                      HESS(JLKAH) = HESS(JLKAH) + OPDM(ILD)*ERI 
                    END DO
                  END IF
                END IF
              END DO
C
C  38. H(AK|JL) <- -4*D(IL)*(AK|IJ), K CORE
C
              DO A = 1, NVIR
                ERI = BUFF(A)*FOUR
                AH = A + NOCC
                KAH = IHES(AH,K)
                IF (KAH.NE.NROT1) THEN
                  DO L = J+1, NOCC
                    LD = L - NCOR
                    ILD = IPIN(MAX(ID,LD)) + MIN(ID,LD)
                    JLH = IHES(L,J)
                    JLKAH = IPIN(MAX(JLH,KAH)) + MIN(JLH,KAH)
                    HESS(JLKAH) = HESS(JLKAH) - OPDM(ILD)*ERI
                  END DO
                  IF (INJ) THEN
                    DO L = I+1, NOCC
                      LD = L - NCOR
                      JLD = IPIN(MAX(JD,LD)) + MIN(JD,LD)
                      ILH = IHES(L,I)
                      ILKAH = IPIN(MAX(ILH,KAH)) + MIN(ILH,KAH)
                      HESS(ILKAH) = HESS(ILKAH) - OPDM(JLD)*ERI
                    END DO
                  END IF
                END IF
              END DO
C
C  39. H(AL|IK) <- -D(JL)*(AK|IJ), K CORE
C
              IKH = IHES(I,K)
              JKH = IHES(J,K)
              DO A = 1, NVIR
                ERI = BUFF(A)
                AH = A + NOCC
                DO L = NCP1, NOCC
                  LD = L - NCOR
                  JLD = IPIN(MAX(JD,LD)) + MIN(JD,LD)
                  LAH = IHES(AH,L)
                  IKLAH = IPIN(MAX(IKH,LAH)) + MIN(IKH,LAH)
                  HESS(IKLAH) = HESS(IKLAH) - OPDM(JLD)*ERI
                END DO 
                IF (INJ) THEN
                  DO L = NCP1, NOCC
                    LD = L - NCOR
                    ILD = IPIN(MAX(ID,LD)) + MIN(ID,LD)
                    LAH = IHES(AH,L)
                    JKLAH = IPIN(MAX(JKH,LAH)) + MIN(JKH,LAH)
                    HESS(JKLAH) = HESS(JKLAH) - OPDM(ILD)*ERI
                  END DO 
                END IF
              END DO
C
C  40. H(AL|KM) <- -D(IJ|LM)*(AK|IJ), K CORE
C
              IJD = IPIN(MAX(ID,JD)) + MIN(ID,JD)
              DO A = 1, NVIR
                ERI = BUFF(A)
                IF (INJ) ERI = ERI*TWO
                AH = A + NOCC
                DO L = NCP1, NOCC
                  LD = L - NCOR
                  LAH = IHES(AH,L)
                  IF (LAH.NE.NROT1) THEN
                    DO M = NCP1, NOCC
                      MD = M - NCOR
                      LMD = IPIN(MAX(LD,MD)) + MIN(LD,MD)
                      IJLMD = IPIN(MAX(IJD,LMD)) + MIN(IJD,LMD)
                      KMH = IHES(M,K)
                      KMLAH = IPIN(MAX(KMH,LAH)) + MIN(KMH,LAH)
                      HESS(KMLAH) = HESS(KMLAH) - TPDM(IJLMD)*ERI
                    END DO
                  END IF
                END DO
              END DO
C
C  END DISTRIBUTION LOOPS
C
            END IF  ! LOCAL
          END DO  ! K
        END DO  ! J
      END DO  ! I
C
C  (ACTIVE-CORE|ACTIVE-VIRTUAL)
C
      DO I = NCP1, NOCC
        DO J = 1, NCOR
          IJ = IPIN(MAX(I,J)) + MIN(I,J)
          DO K = NCP1, NOCC
            IJK = (IJ-1)*NOCC + K
            IF ((IJK.GE.JLO).AND.(IJK.LE.JHI)) THEN
              CALL DDI_GET(D_VOOO,1,NVIR,IJK,IJK,BUFF)
              ID = I - NCOR
              KD = K - NCOR
C
C  41. H(AJ|IL) <- -D(KL)*(AK|IJ) -D(KL)*(AI|JK), J CORE
C
              DO A = 1, NVIR
                ERI = BUFF(A)
                AH = A + NOCC
                JAH = IHES(AH,J)
                IF (JAH.NE.NROT1) THEN
                  DO L = NCP1, I-1
                    LD = L - NCOR
                    KLD = IPIN(MAX(KD,LD)) + MIN(KD,LD)
                    ILH = IHES(I,L)
                    ILJAH = IPIN(MAX(ILH,JAH)) + MIN(ILH,JAH)
                    HESS(ILJAH) = HESS(ILJAH) - OPDM(KLD)*ERI
                  END DO
                  DO L = NCP1, K-1
                    LD = L - NCOR
                    ILD = IPIN(MAX(ID,LD)) + MIN(ID,LD)
                    KLH = IHES(K,L)
                    KLJAH = IPIN(MAX(KLH,JAH)) + MIN(KLH,JAH)
                    HESS(KLJAH) = HESS(KLJAH) - OPDM(ILD)*ERI
                  END DO
                END IF
              END DO
C
C  42. H(AJ|IL) <- D(IK)*(AK|LJ) + D(IK)*(AL|JK), J CORE
C
              DO A = 1, NVIR
                ERI = BUFF(A)
                AH = A + NOCC
                JAH = IHES(AH,J)
                IF (JAH.NE.NROT1) THEN
                  DO L = I+1, NOCC
                    LD = L - NCOR
                    KLD = IPIN(MAX(KD,LD)) + MIN(KD,LD)
                    ILH = IHES(L,I)
                    ILJAH = IPIN(MAX(ILH,JAH)) + MIN(ILH,JAH)
                    HESS(ILJAH) = HESS(ILJAH) + OPDM(KLD)*ERI
                  END DO
                  DO L = K+1, NOCC
                    LD = L - NCOR
                    ILD = IPIN(MAX(ID,LD)) + MIN(ID,LD)
                    KLH = IHES(L,K)
                    KLJAH = IPIN(MAX(KLH,JAH)) + MIN(KLH,JAH)
                    HESS(KLJAH) = HESS(KLJAH) + OPDM(ILD)*ERI
                  END DO
                END IF
              END DO
C
C  43. H(AL|IJ) <- 4*D(KL)*(AK|IJ), J CORE
C
              IJH = IHES(I,J)
              IF (IJH.NE.NROT1) THEN
                DO A = 1, NVIR
                  ERI = BUFF(A)
                  AH = A + NOCC
                  DO L = NCP1, NOCC
                    LD = L - NCOR
                    KLD = IPIN(MAX(KD,LD)) + MIN(KD,LD)
                    LAH = IHES(AH,L)
                    IJLAH = IPIN(MAX(IJH,LAH)) + MIN(IJH,LAH)
                    HESS(IJLAH) = HESS(IJLAH) + OPDM(KLD)*ERI*FOUR
                  END DO
                END DO
              END IF
C
C  44. H(AL|IJ) <- -D(KL)*(AI|JK), J CORE
C
              KJH = IHES(K,J)
              IF (KJH.NE.NROT1) THEN
                DO A = 1, NVIR
                  ERI = BUFF(A)
                  AH = A + NOCC
                  DO L = NCP1, NOCC
                    LD = L - NCOR
                    ILD = IPIN(MAX(ID,LD)) + MIN(ID,LD)
                    LAH = IHES(AH,L)
                    KJLAH = IPIN(MAX(KJH,LAH)) + MIN(KJH,LAH)
                    HESS(KJLAH) = HESS(KJLAH) - OPDM(ILD)*ERI
                  END DO
                END DO
              END IF
C
C  45. H(AL|JM) <- -2*D(KL|IM)*(AK|IJ), J CORE
C
              DO A = 1, NVIR
                ERI = BUFF(A)*TWO
                AH = A + NOCC
                DO L = NCP1, NOCC
                  LAH = IHES(AH,L)
                  IF (LAH.NE.NROT1) THEN
                    LD = L - NCOR
                    KLD = IPIN(MAX(KD,LD)) + MIN(KD,LD)
                    DO M = NCP1, NOCC
                      MD = M - NCOR
                      IMD = IPIN(MAX(ID,MD)) + MIN(ID,MD)
                      KLIMD = IPIN(MAX(KLD,IMD)) + MIN(KLD,IMD)
                      JMH = IHES(M,J)
                      JMLAH = IPIN(MAX(JMH,LAH)) + MIN(JMH,LAH)
                      HESS(JMLAH) = HESS(JMLAH) - TPDM(KLIMD)*ERI
                    END DO
                  END IF
                END DO
              END DO
C
C  END DISTRIBUTION LOOPS
C
            END IF  ! LOCAL
          END DO  ! K
        END DO  ! J
      END DO  ! I
C
C  (ACTIVE-ACTIVE|ACTIVE-VIRTUAL)
C
      DO I = NCP1, NOCC
        DO J = NCP1, I
          IJ = IPIN(MAX(I,J)) + MIN(I,J)
          DO K = NCP1, NOCC
            IJK = (IJ-1)*NOCC + K
            IF ((IJK.GE.JLO).AND.(IJK.LE.JHI)) THEN
              CALL DDI_GET(D_VOOO,1,NVIR,IJK,IJK,BUFF)
              INJ = I.NE.J
              ID = I - NCOR
              JD = J - NCOR
              KD = K - NCOR
              IJD = IPIN(MAX(ID,JD)) + MIN(ID,JD)
C
C  46. L(AL) <- D(IJ|KL)*(AK|IJ)
C
              DO A = 1, NVIR
                ERI = BUFF(A)
                IF (INJ) ERI = ERI*TWO
                AH = A + NOCC
                DO L = NCP1, NOCC
                  LD = L - NCOR
                  KLD = IPIN(MAX(KD,LD)) + MIN(KD,LD)
                  IJKLD = IPIN(MAX(IJD,KLD)) + MIN(IJD,KLD)
                  LA = ILAG(AH) + L
                  LAGN(LA) = LAGN(LA) + TPDM(IJKLD)*ERI
                END DO
              END DO
C
C  47. H(AL|IM) <- 2*D(KL|JM)*(AK|IJ)
C
              DO A = 1, NVIR
                ERI = BUFF(A)*TWO
                AH = A + NOCC
                DO L = NCP1, NOCC
                  LAH = IHES(AH,L)
                  IF (LAH.NE.NROT1) THEN
                    LD = L - NCOR
                    KLD = IPIN(MAX(KD,LD)) + MIN(KD,LD)
                    DO M = NCP1, I-1
                      MD = M - NCOR
                      JMD = IPIN(MAX(JD,MD)) + MIN(JD,MD)
                      KLJMD = IPIN(MAX(KLD,JMD)) + MIN(KLD,JMD)
                      IMH = IHES(I,M)
                      IMLAH = IPIN(MAX(IMH,LAH)) + MIN(IMH,LAH)
                      HESS(IMLAH) = HESS(IMLAH) + TPDM(KLJMD)*ERI
                    END DO
                    DO M = J+1, NOCC
                      MD = M - NCOR
                      IMD = IPIN(MAX(ID,MD)) + MIN(ID,MD)
                      KLIMD = IPIN(MAX(KLD,IMD)) + MIN(KLD,IMD)
                      JMH = IHES(J,M)
                      JMLAH = IPIN(MAX(JMH,LAH)) + MIN(JMH,LAH)
                      HESS(JMLAH) = HESS(JMLAH) - TPDM(KLIMD)*ERI
                    END DO
C
C  48. H(AL|MI) <- -2*D(KL|JM)*(AK|IJ)
C
                    IF (INJ) THEN
                      DO M = NCP1, J-1
                        MD = M - NCOR
                        IMD = IPIN(MAX(ID,MD)) + MIN(ID,MD)
                        KLIMD = IPIN(MAX(KLD,IMD)) + MIN(KLD,IMD)
                        JMH = IHES(J,M)
                        JMLAH = IPIN(MAX(JMH,LAH)) + MIN(JMH,LAH)
                        HESS(JMLAH) = HESS(JMLAH) + TPDM(KLIMD)*ERI
                      END DO
                      DO M = I+1, NOCC
                        MD = M - NCOR
                        JMD = IPIN(MAX(JD,MD)) + MIN(JD,MD)
                        KLJMD = IPIN(MAX(KLD,JMD)) + MIN(KLD,JMD)
                        IMH = IHES(I,M)
                        IMLAH = IPIN(MAX(IMH,LAH)) + MIN(IMH,LAH)
                        HESS(IMLAH) = HESS(IMLAH) - TPDM(KLJMD)*ERI
                      END DO
                    END IF
                  END IF
                END DO
              END DO
C
C  49. H(AL|KM) <- D(LM|IJ)*(AK|IJ)
C
              DO A = 1, NVIR
                ERI = BUFF(A)
                IF (INJ) ERI = ERI*TWO
                AH = A + NOCC
                DO L = NCP1, NOCC
                  LAH = IHES(AH,L)
                  IF (LAH.NE.NROT1) THEN
                    LD = L - NCOR
                    DO M = NCP1, K-1
                      MD = M - NCOR
                      LMD = IPIN(MAX(LD,MD)) + MIN(LD,MD)
                      KMH = IHES(K,M)
                      IJLMD = IPIN(MAX(IJD,LMD)) + MIN(IJD,LMD)
                      KMLAH = IPIN(MAX(KMH,LAH)) + MIN(KMH,LAH)
                      HESS(KMLAH) = HESS(KMLAH) + TPDM(IJLMD)*ERI
                    END DO 
C
C  50. H(AL|MK) <- -D(LM|IJ)*(AK|IJ)
C
                    DO M = K+1, NOCC
                      MD = M - NCOR
                      LMD = IPIN(MAX(LD,MD)) + MIN(LD,MD)
                      KMH = IHES(K,M)
                      IJLMD = IPIN(MAX(IJD,LMD)) + MIN(IJD,LMD)
                      KMLAH = IPIN(MAX(KMH,LAH)) + MIN(KMH,LAH)
                      HESS(KMLAH) = HESS(KMLAH) - TPDM(IJLMD)*ERI
                    END DO 
                  END IF
                END DO 
              END DO 
C
C  END DISTRIBUTION LOOPS
C
            END IF  ! LOCAL
          END DO  ! K
        END DO  ! J
      END DO  ! I
C
C ----------- CONTRIBUTIONS FROM (VV|OO) TYPE INTEGRALS -----------
C
      CALL DDI_DISTRIB(D_VVOO,DDI_ME,ILO,IHI,JLO,JHI)
C
C  (VIRTUAL-VIRTUAL|CORE-CORE)
C
      DO I = 1, NCOR
        DO J = 1, I
          IJ = IPIN(MAX(I,J)) + MIN(I,J)
          IF ((IJ.GE.JLO).AND.(IJ.LE.JHI)) THEN
            CALL DDI_GET(D_VVOO,1,NVTR,IJ,IJ,BUFF)
C
C  51. H(AI|BJ) <- -2*(AB|IJ), I,J CORE
C
            DO A = 1, NVIR
              AH = A + NOCC
              IAH = IHES(AH,I)
              IF (IAH.NE.NROT1) THEN
                DO B = 1, A
                  BH = B + NOCC
                  AB = IPIN(MAX(A,B)) + MIN(A,B)
                  JBH = IHES(BH,J)
                  IAJBH = IPIN(MAX(IAH,JBH)) + MIN(IAH,JBH)
                  HESS(IAJBH) = HESS(IAJBH) - BUFF(AB)*TWO
                END DO
              END IF
            END DO
            IF (I.NE.J) THEN
              DO A = 1, NVIR
                AH = A + NOCC
                JAH = IHES(AH,J)
                IF (JAH.NE.NROT1) THEN
                  DO B = 1, A-1
                    BH = B + NOCC
                    AB = IPIN(MAX(A,B)) + MIN(A,B)
                    IBH = IHES(BH,I)
                    JAIBH = IPIN(MAX(JAH,IBH)) + MIN(JAH,IBH)
                    HESS(JAIBH) = HESS(JAIBH) - BUFF(AB)*TWO
                  END DO
                END IF
              END DO
            END IF
C
C  END DISTRIBUTION LOOPS
C
          END IF  ! LOCAL
        END DO  ! J
      END DO  ! I
C
C  (VIRTUAL-VIRTUAL|ACTIVE-CORE)
C
      DO I = NCP1, NOCC
        DO J = 1, NCOR
          IJ = IPIN(MAX(I,J)) + MIN(I,J)
          IF ((IJ.GE.JLO).AND.(IJ.LE.JHI)) THEN
            CALL DDI_GET(D_VVOO,1,NVTR,IJ,IJ,BUFF)
C
C  52. H(AK|BJ) <- -D(IK)*(AB|IJ), J CORE
C
            ID = I - NCOR
            DO A = 1, NVIR
              AH = A + NOCC
              DO B = 1, A
                BH = B + NOCC
                JBH = IHES(BH,J)
                IF (JBH.NE.NROT1) THEN
                  AB = IPIN(MAX(A,B)) + MIN(A,B)
                  ERI = BUFF(AB)
                  DO K = NCP1, NOCC
                    KD = K - NCOR
                    IKD = IPIN(MAX(ID,KD)) + MIN(ID,KD)
                    KAH = IHES(AH,K)
                    KAJBH = IPIN(MAX(KAH,JBH)) + MIN(KAH,JBH)
                    HESS(KAJBH) = HESS(KAJBH) - OPDM(IKD)*ERI
                  END DO
                END IF
              END DO
            END DO
            DO A = 1, NVIR
              AH = A + NOCC
              JAH = IHES(AH,J)
              IF (JAH.NE.NROT1) THEN
                DO B = 1, A-1
                  BH = B + NOCC
                  AB = IPIN(MAX(A,B)) + MIN(A,B)
                  ERI = BUFF(AB)
                  DO K = NCP1, NOCC
                    KD = K - NCOR
                    IKD = IPIN(MAX(ID,KD)) + MIN(ID,KD)
                    KBH = IHES(BH,K)
                    KBJAH = IPIN(MAX(KBH,JAH)) + MIN(KBH,JAH)
                    HESS(KBJAH) = HESS(KBJAH) - OPDM(IKD)*ERI
                  END DO
                END DO
              END IF
            END DO
C
C  END DISTRIBUTION LOOPS
C
          END IF  ! LOCAL
        END DO  ! J
      END DO  ! I
C
C  (VIRTUAL-VIRTUAL|ACTIVE-ACTIVE)
C
      DO I = NCP1, NOCC
        DO J = NCP1, I
          IJ = IPIN(MAX(I,J)) + MIN(I,J)
          IF ((IJ.GE.JLO).AND.(IJ.LE.JHI)) THEN
            CALL DDI_GET(D_VVOO,1,NVTR,IJ,IJ,BUFF)
C
C  53. H(AK|BL) <- D(IJ|KL)*(AB|IJ)
C
            INJ = I.NE.J
            ID = I - NCOR
            JD = J - NCOR
            IJD = IPIN(MAX(ID,JD)) + MIN(ID,JD)
            DO A = 1, NVIR
              AH = A + NOCC
              DO B = 1, A
                BH = B + NOCC
                AB = IPIN(MAX(A,B)) + MIN(A,B)
                ERI = BUFF(AB)
                IF (INJ) ERI = ERI*TWO
                ANB = A.NE.B
                DO K = NCP1, NOCC
                  KAH = IHES(AH,K)
                  IF (KAH.NE.NROT1) THEN
                    KD = K - NCOR
                    MX = K
                    IF (ANB) MX = NOCC
                    DO L = NCP1, MX
                      LD = L - NCOR
                      KLD = IPIN(MAX(KD,LD)) + MIN(KD,LD)
                      IJKLD = IPIN(MAX(IJD,KLD)) + MIN(IJD,KLD)
                      LBH = IHES(BH,L)
                      KALBH = IPIN(MAX(KAH,LBH)) + MIN(KAH,LBH)
                      HESS(KALBH) = HESS(KALBH) + TPDM(IJKLD)*ERI
                    END DO
                  END IF
                END DO
              END DO
            END DO
C
C  END DISTRIBUTION LOOPS
C
          END IF  ! LOCAL
        END DO  ! J
      END DO  ! I
C
C ----------- CONTRIBUTIONS FROM (VO|VO) TYPE INTEGRALS -----------
C
      CALL DDI_DISTRIB(D_VOVO,DDI_ME,ILO,IHI,JLO,JHI)
C
C  (VIRTUAL-CORE|VIRTUAL-CORE)
C
      DO I = 1, NCOR
        DO J = 1, I
          IJ = IPIN(MAX(I,J)) + MIN(I,J)
          IF ((IJ.GE.JLO).AND.(IJ.LE.JHI)) THEN
            CALL DDI_GET(D_VOVO,1,NVSQ,IJ,IJ,BUFF)
C
C  54. H(AI|BJ) <- 8*(AI|BJ) -2*(AJ|BI), I,J CORE
C
            INJ = I.NE.J
            DO A = 1, NVIR
              AH = A + NOCC
              IAH = IHES(AH,I)
              IF (IAH.NE.NROT1) THEN
                MX = A
                IF (INJ) MX = NVIR
                DO B = 1, MX
                  BH = B + NOCC
                  JBH = IHES(BH,J)
                  AB = (B-1)*NVIR + A
                  BA = (A-1)*NVIR + B
                  IAJBH = IPIN(MAX(IAH,JBH)) + MIN(IAH,JBH)
                  HESS(IAJBH) = HESS(IAJBH) + BUFF(AB)*EIGHT
                  HESS(IAJBH) = HESS(IAJBH) - BUFF(BA)*TWO
                END DO 
              END IF
            END DO 
C
C  END DISTRIBUTION LOOPS
C
          END IF  ! LOCAL
        END DO  ! J
      END DO  ! I
C
C  (VIRTUAL-ACTIVE|VIRTUAL-CORE)
C
      DO I = NCP1, NOCC
        DO J = 1, NCOR
          IJ = IPIN(MAX(I,J)) + MIN(I,J)
          IF ((IJ.GE.JLO).AND.(IJ.LE.JHI)) THEN
            CALL DDI_GET(D_VOVO,1,NVSQ,IJ,IJ,BUFF)
C
C  55. H(AK|BJ) <- 4*D(IK)*(AI|BJ) -D(IK)*(AJ|BI), J CORE
C
            ID = I - NCOR
            DO A = 1, NVIR
              AH = A + NOCC
              DO B = 1, NVIR
                BH = B + NOCC
                JBH = IHES(BH,J)
                IF (JBH.NE.NROT1) THEN
                  AB = (B-1)*NVIR + A
                  BA = (A-1)*NVIR + B
                  ERI1 = BUFF(AB)*FOUR
                  ERI2 = BUFF(BA)
                  DO K = NCP1, NOCC
                    KD = K - NCOR
                    IKD = IPIN(MAX(ID,KD)) + MIN(ID,KD)
                    DIK = OPDM(IKD)
                    KAH = IHES(AH,K)
                    KAJBH = IPIN(MAX(KAH,JBH)) + MIN(KAH,JBH)
                    HESS(KAJBH) = HESS(KAJBH) + DIK*ERI1
                    HESS(KAJBH) = HESS(KAJBH) - DIK*ERI2
                  END DO
                END IF
              END DO
            END DO
C
C  END DISTRIBUTION LOOPS
C
          END IF  ! LOCAL
        END DO  ! J
      END DO  ! I
C
C  (VIRTUAL-ACTIVE|VIRTUAL-ACTIVE)
C
      DO I = NCP1, NOCC
        DO J = NCP1, I
          IJ = IPIN(MAX(I,J)) + MIN(I,J)
          IF ((IJ.GE.JLO).AND.(IJ.LE.JHI)) THEN
            CALL DDI_GET(D_VOVO,1,NVSQ,IJ,IJ,BUFF)
C
C  56. H(AK|BL) <- 2*D(IK|JL)*(AI|BJ)
C
            ID = I - NCOR
            JD = J - NCOR
            DO A = 1, NVIR
              AH = A + NOCC
              DO B = 1, A
                BH = B + NOCC
                AB = (B-1)*NVIR + A
                ERI = BUFF(AB)*TWO
                ANB = A.NE.B
                DO K = NCP1, NOCC
                  KAH = IHES(AH,K)
                  IF (KAH.NE.NROT1) THEN
                    KD = K - NCOR
                    IKD = IPIN(MAX(ID,KD)) + MIN(ID,KD)
                    MX = K
                    IF (ANB) MX = NOCC
                    DO L = NCP1, MX
                      LD = L - NCOR
                      LBH = IHES(BH,L)
                      JLD = IPIN(MAX(JD,LD)) + MIN(JD,LD)
                      IKJLD = IPIN(MAX(IKD,JLD)) + MIN(IKD,JLD)
                      KALBH = IPIN(MAX(KAH,LBH)) + MIN(KAH,LBH)
                      HESS(KALBH) = HESS(KALBH) + TPDM(IKJLD)*ERI
                    END DO
                  END IF
                END DO
              END DO
            END DO
            IF (I.NE.J) THEN
              DO A = 1, NVIR
                AH = A + NOCC
                DO B = 1, A
                  BH = B + NOCC
                  BA = (A-1)*NVIR + B
                  ERI = BUFF(BA)*TWO
                  ANB = A.NE.B
                  DO K = NCP1, NOCC
                    KAH = IHES(AH,K)
                    IF (KAH.NE.NROT1) THEN
                      KD = K - NCOR
                      JKD = IPIN(MAX(JD,KD)) + MIN(JD,KD)
                      MX = K
                      IF (ANB) MX = NOCC
                      DO L = NCP1, MX
                        LD = L - NCOR
                        LBH = IHES(BH,L)
                        ILD = IPIN(MAX(ID,LD)) + MIN(ID,LD)
                        ILJKD = IPIN(MAX(ILD,JKD)) + MIN(ILD,JKD)
                        KALBH = IPIN(MAX(KAH,LBH)) + MIN(KAH,LBH)
                        HESS(KALBH) = HESS(KALBH) + TPDM(ILJKD)*ERI
                      END DO
                    END IF
                  END DO
                END DO
              END DO
            END IF
C
C  END DISTRIBUTION LOOPS
C
          END IF  ! LOCAL
        END DO  ! J
      END DO  ! I
C
C  FINISHED WITH DISTRIBUTED MATRICES
C  (DESTROYS IN REVERSE ORDER TO CREATES, SEE TRANDDI)
C
      CALL DDI_DESTROY(D_VVOO)
      CALL DDI_DESTROY(D_OOOO)
      CALL DDI_DESTROY(D_VOOO)
      CALL DDI_DESTROY(D_VOVO)
C
C  FINALLY, DOUBLE OFF-DIAGONAL ELEMENTS OF HESSIAN
C  FOR MATRIX MANIPULATIONS TO FOLLOW
C
      NROT2 = (NROT*NROT+NROT)/2
      CALL DSCAL(NROT2,TWO,HESS,1)
      DO I = 1, NROT
        II = IPIN(I) + I
        HESS(II) = HESS(II)*0.5D+00
      END DO
      RETURN
      END
C*MODULE MCTWO   *DECK NTNDVD
C
C GDF:  3/28/02  MODIFIED NTNDVD FOR PARALLEL
C
      SUBROUTINE NTNDVD(H,HD,V,W,T
     *,                 AA,A,VEC,EIG,WRK,IWRK, IA
     *,                 NWKS,MXXPAN,CVGTOL,MAXIT
     *,                 OUT,DBUG,PRTTOL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL CVGED,SOME,OUT,DBUG,PART1
C
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
C
      DIMENSION H(1),HD(1), V(NWKS,1),W(NWKS,1),T(NWKS)
      DIMENSION AA(1),A(1),VEC(MXXPAN,1),EIG(1),WRK(*),IWRK(*)
      DIMENSION IA(1), KCOEF(6),COEFK(6)
C
      DATA ZERO,ONE /0.0D+00,1.0D+00/
      DATA TOL      /1.0D-09/
C
C     ----- DAVIDSON'S METHOD MODIFIED TO DIAGONALIZE -----
C                 THE AUGMENTED HESSIAN MATRIX
C
C        ONLY ONE ROOT AND A THRESOLD VERY STRONG ( 1.0D-9 )
C
C        MINIMUM OF EXPANSIONS IS ONE ET MAXIMUM DEFAULT 50
C
C        MAXIMUM NUMBER OF ITERATIONS 200 (MAXDIA IN NUTON )
C
C     ----- GENERATE TRIAL EXPANSION VECTOR SET -----
C
      SOME =.FALSE.
      THRES=TOL*TOL
C
      KSTAT = 1
      V(1   ,KSTAT)=ONE
      DO 10 IWKS=2,NWKS
   10 V(IWKS,KSTAT)=ZERO
C
      IF(DBUG) WRITE(IW,9996) KSTAT,(IWKS,V(IWKS,1),IWKS=1,NWKS)
C
      DO 20 I=1,MXXPAN
      DO 20 J=1,I
      IJ=IA(I)+J
   20 AA(IJ)=ZERO
C
C     ----- START ITERATION -----
C
      IF(SOME) WRITE(IW,9998)
      ITER=0
      NXPAN0=1
  100 CONTINUE
      ITER=ITER+1
      IF(ITER.GT.MAXIT) GO TO 9000
      IF(OUT) WRITE(IW,9997) ITER,NXPAN0
      NVEC=NXPAN0-1
      IF(NXPAN0.GT.MXXPAN) GO TO 700
  110 CONTINUE
C
C     ----- CALCULATE W = H * V FOR NEW EXPANSION VECTORS -----
C
      CALL PDGHNTN(NWKS,V(1,NXPAN0),W(1,NXPAN0),H,IA)
C
C     ----- CALCULATE NEW TRIANGULAR PART OF INTERACTION MATRIX -----
C
      DUMIJ=ZERO
      DO 120 IWKS=1,NWKS
  120 DUMIJ=DUMIJ+V(IWKS,NXPAN0)*W(IWKS,NXPAN0)
      IJ=IA(NXPAN0)+NXPAN0
      AA(IJ)=DUMIJ
C
C     ----- CALCULATE NEW BAND OF INTERACTION MATRIX -----
C
      IF(NVEC.EQ.0) GO TO 200
C
      DO 170 J=1,NVEC
      DUMIJ=ZERO
      DO 160 IWKS=1,NWKS
  160 DUMIJ=DUMIJ+V(IWKS,NXPAN0)*W(IWKS,J)
      IJ=IA(NXPAN0)+J
      AA(IJ)=DUMIJ
  170 CONTINUE
C
C     ----- SOLVE (NXPAN0*NXPAN0) EIGENVALUE PROBLEM -----
C
  200 CONTINUE
      NDUM=(NXPAN0*(NXPAN0+1))/2
      DO 210 IDUM=1,NDUM
  210 A(IDUM)=AA(IDUM)
      IF(DBUG) CALL PRTRI(A,NXPAN0)
C
C GDF:  3/28/02  SET KDIAG=3 FOR JACOBI?
C
      CALL GLDIAG(NXPAN0,NXPAN0,NXPAN0,A,WRK,EIG,VEC,IERR,IWRK)
C
      IF(OUT) WRITE(IW,9988) EIG(1)
C
C     ----- FORM CORRECTION VECTORS -----
C
      DO 310 IWKS=1,NWKS
  310 T(IWKS)=ZERO
      PART1=.TRUE.
  320 CONTINUE
C
      EIGK=EIG(1)
      DO 360 I=1,NXPAN0
      AIK=VEC(I,1)
      IF(PART1) AIK=-AIK*EIGK
      DO 350 IWKS=1,NWKS
  350 T(IWKS)=T(IWKS)+AIK*V(IWKS,I)
  360 CONTINUE
C
      DO 380 I=1,NXPAN0
      DO 380 IWKS=1,NWKS
      DUM=V(IWKS,I)
      V(IWKS,I)=W(IWKS,I)
  380 W(IWKS,I)=DUM
      IF(.NOT.PART1) GO TO 390
      PART1=.FALSE.
      GO TO 320
  390 CONTINUE
C
C     ----- CHECK CONVERGENCE -----
C
      CVG=ZERO
      CVGED=.TRUE.
      DUM=ZERO
      DO 410 IWKS=1,NWKS
  410 DUM=DUM+T(IWKS)*T(IWKS)
      DUM= SQRT(DUM)
      IF(OUT) WRITE(IW,9989) DUM
      IF(DUM.GT.CVG)  CVG=DUM
      CVGED=CVGED.AND.(DUM.LT.CVGTOL)
      IF(SOME) WRITE(IW,9995) ITER,CVG,EIG(1)
      NVEC=NXPAN0
      IF(CVGED) GO TO 700
C
      IF(ITER.EQ.1) GO TO 460
      EIGK=EIG(1)
      DUM=ZERO
      DO 430 IWKS=1,NWKS
      DENOM=EIGK-HD(IWKS)
      IF( ABS(DENOM).LT.TOL) DENOM=TOL
      T(IWKS)=T(IWKS)/DENOM
  430 DUM=DUM+T(IWKS)*T(IWKS)
      DUM=ONE/ SQRT(DUM)
      DO 440 IWKS=1,NWKS
  440 T(IWKS)=T(IWKS)*DUM
C
  460 CONTINUE
C
C     ----- ORTHOGONALIZE CORRECTION VECTORS AND EXPANSION VECTORS -----
C     ----- UPDATE SET OF EXPANSION VECTORS                        -----
C
      DO 550 I=1,NXPAN0
      DUMIK=ZERO
      DO 530 IWKS=1,NWKS
  530 DUMIK=DUMIK+T(IWKS)*V(IWKS,I)
      DO 540 IWKS=1,NWKS
  540 T(IWKS)=T(IWKS)-DUMIK*V(IWKS,I)
  550 CONTINUE
C
      IVEC = 0
      DUM=ZERO
      DO 610 IWKS=1,NWKS
  610 DUM=DUM+T(IWKS)*T(IWKS)
      IF(DUM.LT.THRES) GO TO 670
      DUM=ONE/ SQRT(DUM)
      DO 620 IWKS=1,NWKS
  620 T(IWKS)=T(IWKS)*DUM
C
      IVEC = 1
C
  670 IF(OUT) WRITE(IW,9991) IVEC
C
      IF(IVEC.EQ.0) WRITE(IW,9990)
C
C     ----- END OF CYCLE -----
C
      NXPAN0 = NXPAN0+1
      DO 680 IWKS=1,NWKS
  680 V(IWKS,NXPAN0)=T(IWKS)
      IF(DBUG) WRITE(IW,9996) NXPAN0,(IWKS,V(IWKS,NXPAN0),IWKS=1,NWKS)
      GO TO 100
C
C     ----- RE-ORTHONORMALIZE EXPANSION COEFFICIENT MATRIX -----
C
  700 CONTINUE
      DUMIJ=ZERO
      DO 710 K=1,NVEC
  710 DUMIJ=DUMIJ+VEC(K,1)*VEC(K,1)
      DUMIJ=ONE/ SQRT(DUMIJ)
      DO 740 K=1,NVEC
  740 VEC(K,1)=VEC(K,1)*DUMIJ
C
C     ----- GET APPROXIMATE OR CONVERGED -CI- VECTORS -----
C
      PART1=.TRUE.
  800 CONTINUE
      DO 810 IWKS=1,NWKS
  810 T(IWKS)=ZERO
C
      DO 830 IVEC=1,NVEC
      AIK=VEC(IVEC,1)
      DO 820 IWKS=1,NWKS
  820 T(IWKS)=T(IWKS)+AIK*V(IWKS,IVEC)
  830 CONTINUE
C
      IF(CVGED) GO TO 900
C
C     ----- IF .NOT.CVGED, USE VECTORS AS NEW EXPANSION VECTORS -----
C
      IF(.NOT.PART1) GO TO 870
      DO 850 IVEC=1,NVEC
      DO 850 IWKS=1,NWKS
  850 V(IWKS,IVEC)=W(IWKS,IVEC)
C
      IF(DBUG) WRITE(IW,9996) KSTAT,(IWKS,T(IWKS),IWKS=1,NWKS)
      DO 860 IWKS=1,NWKS
  860 W(IWKS,1)=T(IWKS)
      PART1=.FALSE.
      GO TO 800
C
  870 CONTINUE
      DO 880 IWKS=1,NWKS
      V(IWKS,1)=W(IWKS,1)
  880 W(IWKS,1)=T(IWKS)
C
      AA(1)=EIG(1)
      NXPAN0=1
      IVEC=1
      IF(OUT) WRITE(IW,9987)
      GO TO 110
C
C     ----- PRINT FINAL VECTORS ----
C
  900 CONTINUE
C
C     IF(SOME) WRITE(IW,9993)   EIG(1)
C9993 FORMAT(/1X,'FIRST ROOT EIGENVALUE=',F18.9/1X,10("-"),8X,6("-"))
C
      NCOEF=0
      DO 910 IWKS=1,NWKS
      V(IWKS,1)=T(IWKS)
      DUM=V(IWKS,1)
      IF( ABS(DUM).LT.PRTTOL) GO TO 910
      NCOEF=NCOEF+1
      KCOEF(NCOEF)=IWKS
      COEFK(NCOEF)=DUM
      IF(NCOEF.LT.6) GO TO 910
      IF(SOME) WRITE(IW,9992) (KCOEF(K),COEFK(K),K=1,NCOEF)
      NCOEF=0
  910 CONTINUE
      IF(NCOEF.EQ.0) GO TO 920
      IF(SOME) WRITE(IW,9992) (KCOEF(K),COEFK(K),K=1,NCOEF)
      NCOEF=0
  920 CONTINUE
C
      RETURN
 9000 CONTINUE
      WRITE(IW,9994) CVG, CVGTOL
      RETURN
 9998 FORMAT(/,' ITER.    MAX.DEV.    STATE ENERGIES ',/,
     1       '        STATE NORM(D)                ')
 9997 FORMAT(' ITERATION ',I3,' WITH ',I3,' EXPANSION VECTORS.')
 9996 FORMAT(' EXPANSION VECTORS ',I3,/,(6(I5,F15.8)) )
 9995 FORMAT(I4  ,F12.8, F17.9)
 9994 FORMAT(' EXCESSIVE NUMBER OF ITERATIONS IN -NTNDVD-',
     1 ' DURING AUGMENTED HESSIAN MATRIX DIAGONALIZATION. STOP',/,
     2 ' CVG = ',E12.4,' CVGTOL = ',E12.4)
 9992 FORMAT(1X,6(I7,F14.7))
 9991 FORMAT(I3,' VECTOR(S) ADDED TO EXPANSION SET.')
 9990 FORMAT(' NO VECTORS ADDED TO THE EXPANSION SET IN -NTNDVD- ',/,
     1 ' EVEN THOUGH DIAGONALIZATION NOT CONVERGED TO REQUESTED',
     2 ' THRESHOLD.',/,' INCREASE CONVERGENCE THRESHOLD, IN',/,
     3 ' NAMELIST $NEWTON - STOP')
 9989 FORMAT(' CONVERGENCE CHECK FOR STATE  IS = ',F15.8)
 9988 FORMAT(' FIRST ROOT  EIGENVALUE = ',F17.9)
 9987 FORMAT(' .... EXPANSION VECTORS RESET .... ')
      END
C
C GDF:  3/27/02  MODIFIED DGHNTN FOR PARALLELISM
C
C*MODULE MCTWO   *DECK PDGHNTN
      SUBROUTINE PDGHNTN(NDIM,V,W,H,IA)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION V(1),W(1),H(1),IA(1)
      INTEGER DDI_NP,DDI_ME
      PARAMETER (ZERO=0.0D+00)
C
C  BLOCK DISTRIBUTION
C
      CALL DDI_NPROC(DDI_NP,DDI_ME)
      CALL DCOPY(NDIM,ZERO,0,W,1)
      LBLK = NDIM/DDI_NP
      NLFT = MOD(NDIM,DDI_NP)
      IMIN = DDI_ME*LBLK + 1
      IF (DDI_ME.LT.NLFT) THEN
        IMIN = IMIN + DDI_ME
      ELSE
        IMIN = IMIN + NLFT
      END IF
      IMAX = IMIN + LBLK
      IF (DDI_ME.GE.NLFT) IMAX = IMAX - 1
C
      DO I = IMIN, IMAX
        DUM = ZERO
        DO J = 1, I
          IJ = IA(I) + J
          DUM = DUM + V(J)*H(IJ)
        END DO
        DO J = I+1, NDIM
          IJ = IA(J) + I
          DUM = DUM + V(J)*H(IJ)
        END DO
        W(I) = DUM
      END DO
      CALL DDI_GSUMF(2300,W,NDIM)
      RETURN
      END
