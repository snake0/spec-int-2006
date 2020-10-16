C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C 12 NOV 98 - GDF - CHANGE BIT PACKING TO ISHIFT
C  6 JAN 98 - MWS - CHANGE RACLOS CALL
C  1 FEB 95 - MWS - USE UNIT 24 FOR ORDERED INTEGRALS
C 12 NOV 94 - MWS - REMOVE FTNCHEK WARNINGS
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C  5 AUG 94 - MWS - INCORE,ORDERA,ORDERB: USE DOUBLE LABEL PACKING
C  1 JUN 94 - MWS - ORDRJK: NOW OPENS THE ORDERED AO INTEGRAL FILE
C 12 MAR 92 - MWS - REDIMENSION TO 500 ATOMS
C 10 JAN 92 - TLW - CHANGE REWIND TO CALL SEQREW
C  7 JAN 92 - TLW - MAKE WRITES PARALLEL; ADD COMMON PAR
C  7 OCT 91 - MWS - CHANGE UNIX BYTE UNPACKING
C 12 SEP 90 - MWS - INTRODUCE MXATM
C  3 JUN 90 - JAM - CHANGE VAX BYTE PACKING TO FULL 8 BITS
C 29 DEC 89 - MWS - ORDRJK: GET RID OF IX EQUIVALENCE TO /FMCOM/
C 27 NOV 89 - MWS - IMPROVE "CANNOT USE MORE THAN ONE PASSES" DIAGNOSTIC
C 26 SEP 89 - MWS - ADD NFT13,NFT14 TO /CIFILS/
C 27 AUG 89 - CFJ - ADD IJKLX TO ALL RAWRIT CALLS IN ORDERA (CONVEX)
C 28 MAR 89 - MWS - GO AHEAD WITH CALL TO RAOPEN ON RUNTYP=CHECK
C 24 JAN 89 - MWS - DON'T ENTER RAOPEN ON RUNTYP=CHECK
C 30 NOV 88 - MWS - ADJUST NDAR IF MORE THAN ONE PASS NEEDED.
C 30 JUN 88 - MWS(MR) - FIX ORDERA FOR CONVEX COMPILER
C  7 APR 88 - MWS - IF(ABS(XP(MX)).EQ.ZERO) CHANGED TO (M) IN ORDERA
C  2 DEC 87 - STE - CHECK FOR MULTI-PASS SORT WHEN EXETYP=CHECK
C 15 NOV 87 - STE - USE EXETYP
C  6 AUG 87 - MWS - INCLUDE ETA VERSION
C 19 JUL 87 - MWS - RENAME VARIABLE NOCORE TO NOMEM
C  3 NOV 86 - STE - REMOVE NAV REFERENCES, USE PARAMETERS
C                   ABORT FOR MULTI-PASS SORTS
C 16 AUG 86 - MWS - PRINT NUMBER OF INTEGRAL RECORDS IN INCORE
C  9 JUL 86 - JAB - ADD BIT PACKING/UNPACKING FOR CRAY, CELERITY
C  7 JUL 86 - MWS - SANITIZE FLOATING POINT CONSTANTS
C 13 NOV 85 - STE - DELETE /BUF/; PASS BUFFERS TO ORDERA
C  7 NOV 85 - STE - USE GENERIC ABS,MAX,MIN; USE VCLR
C 11 JUL 85 - MWS - CHANGE IBM INDEX PACKING
C 30 MAY 85 - MWS - ADD NPRINT TO RAOPEN CALL
C 23 JAN 84 - STE - DELETE UNUSED PARMS AND SPACE IN ORDERA/B
C                   INITIALIZE ZERO IN ORDERA
C 19 DEC 83 - STE - DELETE UNUSED REGIONS IN ORDERA/B, DYNAMIC BUFFER
C 17 NOV 83 - STE - FPS PACKING/UNPACKING
C  5 NOV 83 - STE - DELETE CALL DARTRN FROM ORDRJK
C  6 MAY 83 - MWS - PACKING CHANGED
C  3 OCT 82 - MWS - CONVERT FOR IBM
C 27 AUG 82 - MWS - KILL PRINTOUT IF NPRINT=-5 IN ORDJK
C
C*MODULE ORDINT  *DECK INCORE
      SUBROUTINE INCORE(OX,IA,X,IX,L1,L2,L4,
     *                  IW,NFTI,NFTO,NINTBF,NSQUAR)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION OX(*),IA(*),X(NINTBF),IX(NINTBF)
C
      LOGICAL OUT,PACK2E,GOPARR,DSKWRK,MASWRK
C
C     ----- SORT TWO-ELECTRON INTEGRALS IN MEMORY -----
C
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /INTPRT/ QINT(3),VALINT(3),JCINT(16),OUT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCKLAB/ LABSIZ
C
      PARAMETER (ZERO=0.0D+00)
C
      DO 100 I = 1,L2
  100 IA(I) = (I*(I-1))/2
      CALL VCLR(OX,1,L4)
C
      I = 0
      J = 0
      K = 0
      L = 0
      VAL = ZERO
C
C     ----- READ IN INTEGRALS FROM -NFTI- -----
C
      CALL SEQREW(NFTI)
      NX = 0
  140 CALL PREAD(NFTI,X,IX,NX,NINTMX)
      IF (NX .EQ. 0) GO TO 180
      MX = ABS(NX)
      IF (MX .GT. NINTMX) CALL ABRT
      DO 160 M = 1,MX
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
C
         IJ = IA(I)+J
         KL = IA(K)+L
         IJKL = IA(IJ)+KL
         IF (NSQUAR .EQ. 0) GO TO 150
         IF(I.EQ.J) X(M)=X(M)+X(M)
         IF(K.EQ.L) X(M)=X(M)+X(M)
         IF(IJ.EQ.KL) X(M)=X(M)+X(M)
         IJKL = (IJ - 1) * L2 + KL
         KLIJ = (KL - 1) * L2 + IJ
         OX(KLIJ) = X(M)
  150    OX(IJKL) = X(M)
  160 CONTINUE
      IF (NX .GT. 0) GO TO 140
  180 CONTINUE
C
C     ----- WRITE OUT INTEGRALS ON -NFTO- -----
C
      CALL SEQREW(NFTO)
      NREC=0
      N = 0
      NX = 1
CCC
C     TRIANGULAR INTEGRAL LIST
CCC
      IF (NSQUAR .NE. 0) GO TO 300
      DO 260 I = 1,L1
      DO 240 J = 1,I
      DO 220 K = 1,I
      LMAX = K
      IF (K .EQ. I) LMAX = J
      DO 200 L = 1,LMAX
      N = N+1
      VAL = OX(N)
      IF (VAL .EQ. ZERO) GO TO 200
C
               NPACK = NX
               IPACK = I
               JPACK = J
               KPACK = K
               LPACK = L
               IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                 IX( 2*NPACK-1 ) = ISHFT( IPACK, 16 ) + JPACK
                 IX( 2*NPACK   ) = ISHFT( KPACK, 16 ) + LPACK
#endif
#if defined(SPEC_CPU_ILP64)
                 LABEL = ISHFT( IPACK, 48 ) + ISHFT( JPACK, 32 ) +
     *                   ISHFT( KPACK, 16 ) + LPACK
                 IX(NPACK) = LABEL
#endif
               ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                 LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                   ISHFT( KPACK,  8 ) + LPACK
                 IX(NPACK) = LABEL
#endif
#if defined(SPEC_CPU_ILP64)
                 IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                   LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                     ISHFT( KPACK,  8 ) + LPACK
                   IX( NPACK/2 ) = IX( NPACK/2 ) + LABEL
                 ELSE
                   LABEL = ISHFT( IPACK, 56 ) + ISHFT( JPACK, 48 ) +
     *                     ISHFT( KPACK, 40 ) + ISHFT( LPACK, 32 )
                   IX( (NPACK/2)+1 ) = LABEL
                 END IF
#endif
               END IF
C
      X(NX) = VAL
      IF (OUT) CALL INTOUT(I,J,K,L,ZERO,NX,VAL)
      NX = NX+1
      IF (NX .LE. NINTMX) GO TO 200
      CALL PWRIT(NFTO,X,IX,NINTMX,NINTMX)
      NREC=NREC+1
      NX = 1
  200 CONTINUE
  220 CONTINUE
  240 CONTINUE
  260 CONTINUE
      GO TO 400
CCC
C     SQUARE INTEGRAL LIST
CCC
  300 DO 320 I = 1,L1
      DO 340 J = 1,I
      DO 360 K = 1,L1
      DO 380 L = 1,K
      N = N+1
      VAL = OX(N)
      IF (VAL .EQ. ZERO) GO TO 380
C
               NPACK = NX
               IPACK = I
               JPACK = J
               KPACK = K
               LPACK = L
               IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                 IX( 2*NPACK-1 ) = ISHFT( IPACK, 16 ) + JPACK
                 IX( 2*NPACK   ) = ISHFT( KPACK, 16 ) + LPACK
#endif
#if defined(SPEC_CPU_ILP64)
                 LABEL = ISHFT( IPACK, 48 ) + ISHFT( JPACK, 32 ) +
     *                   ISHFT( KPACK, 16 ) + LPACK
                 IX(NPACK) = LABEL
#endif
               ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                 LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                   ISHFT( KPACK,  8 ) + LPACK
                 IX(NPACK) = LABEL
#endif
#if defined(SPEC_CPU_ILP64)
                 IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                   LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                     ISHFT( KPACK,  8 ) + LPACK
                   IX( NPACK/2 ) = IX( NPACK/2 ) + LABEL
                 ELSE
                   LABEL = ISHFT( IPACK, 56 ) + ISHFT( JPACK, 48 ) +
     *                     ISHFT( KPACK, 40 ) + ISHFT( LPACK, 32 )
                   IX( (NPACK/2)+1 ) = LABEL
                 END IF
#endif
               END IF
C
      X(NX) = VAL
      IF (OUT) CALL INTOUT(I,J,K,L,ZERO,NX,VAL)
      NX = NX+1
      IF (NX .LE. NINTMX) GO TO 380
      CALL PWRIT(NFTO,X,IX,NINTMX,NINTMX)
      NREC=NREC+1
      NX = 1
  380 CONTINUE
  360 CONTINUE
  340 CONTINUE
  320 CONTINUE
CCC
  400 IF (OUT) CALL INTOUT(I,J,K,L,ZERO,NX,VAL)
      NX = NX-1
      NX = -NX
      CALL PWRIT(NFTO,X,IX,NX,NINTMX)
      NREC=NREC+1
      IF (MASWRK) WRITE(IW,9000) NREC,NINTMX,NFTO,NSQUAR
      RETURN
 9000 FORMAT(1X,I8,' RECORDS OF',I6,' INTEGRALS WRITTEN ON UNIT',
     *       I3,', NSQUAR=',I3)
      END
C*MODULE ORDINT  *DECK ORDERA
      SUBROUTINE ORDERA(ILOW,IHIGH,NFTI,XP,IX,IDAF,IODA,
     +                  ICHAIN,ICOUNT,LOCBOX,IJKLAD,IA,IJKLX,XIJKL,
     +                  NBOX,LBOX,LBIN,L2,NINTBF,NSQUAR)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL PACK2E
C
C        NOTE THAT IJKLX AND XIJKL ARE EQUIVALENT BY CALL
C
      DIMENSION XP(NINTBF),IX(NINTBF)
      DIMENSION IODA(1),IA(1),ICOUNT(1),ICHAIN(1),IJKLX(1),XIJKL(1)
      DIMENSION LOCBOX(1),IJKLAD(1)
C
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /PCKLAB/ LABSIZ
C
      PARAMETER (ZERO=0.0D+00)
C
      LENGTH = 2*LBIN+2
      NDAF = 1
      DO 100 IBOX = 1,NBOX
      LOCBOX(IBOX) = (IBOX-1)*LENGTH
      IJKLAD(IBOX) = (IBOX-1)*LBOX +ILOW-1
      ICOUNT(IBOX) = 1
  100 ICHAIN(IBOX) = 0
      DO 120 I = 1,L2
  120 IA(I) = (I*(I-1))/2
C
      I = 0
      J = 0
      K = 0
      L = 0
C
C     ----- READ IN INTEGRALS FROM -NFTI- -----
C
      CALL SEQREW(NFTI)
      NX = 0
  140 CALL PREAD(NFTI,XP,IX,NX,NINTMX)
      IF (NX .EQ. 0) GO TO 200
      MX = ABS(NX)
      IF (MX .GT. NINTMX) CALL ABRT
      DO 180 M = 1,MX
         IF(ABS(XP(M)).EQ.ZERO) GO TO 180
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
      IJ = IA(I)+J
      KL = IA(K)+L
      IJKL = IA(IJ)+KL
      IF (NSQUAR .EQ. 0) GO TO 155
      IJKL = (IJ - 1) * L2 + KL
      KLIJ = (KL - 1) * L2 + IJ
      IF (I .EQ. J) XP(M) = XP(M) + XP(M)
      IF (K .EQ. L) XP(M) = XP(M) + XP(M)
      IF (IJ .EQ. KL) XP(M) = XP(M) + XP(M)
C
      IF (KLIJ .LT. ILOW .OR. KLIJ .GT. IHIGH) GO TO 155
      IBOX = (KLIJ-ILOW)/LBOX+1
      LCOUNT = ICOUNT(IBOX)
      LOCB = LOCBOX(IBOX)
      LOC = LOCB+LCOUNT
C
      XIJKL(LOC ) = XP(M)
      IJKLX((LOC+LBIN-1)*NWDVAR+1) = KLIJ-IJKLAD(IBOX)
C
      LCOUNT = LCOUNT+1
      IF (LCOUNT .LE. LBIN) GO TO 150
      LCOUNT = LCOUNT-1
      LOC = LOCB+(LBIN+LBIN)
      IJKLX((LOC+1-1)*NWDVAR+1) = LCOUNT
      IJKLX((LOC+2-1)*NWDVAR+1) = ICHAIN(IBOX)
C
C         PASS IJKLX AS THE LAST ARGUMENT TO RAWRIT SO THAT
C         OPTIMIZING COMPILERS ARE FORCED TO STORE IJKLX'S
C         BEFORE CALLING RAWRIT (CONVEX NEEDS THIS).
C
      CALL RAWRIT(IDAF,IODA,XIJKL(LOCB+1),LENGTH,NDAF,IJKLX)
      ICHAIN(IBOX) = NDAF
      NDAF = NDAF+1
      LCOUNT = 1
  150 CONTINUE
      ICOUNT(IBOX) = LCOUNT
      IF (IJ .NE. KL) GO TO 155
      GO TO 180
C
  155 IF (IJKL .LT. ILOW .OR. IJKL .GT. IHIGH) GO TO 180
      IBOX = (IJKL-ILOW)/LBOX+1
      LCOUNT = ICOUNT(IBOX)
      LOCB = LOCBOX(IBOX)
      LOC = LOCB+LCOUNT
C
      XIJKL(LOC ) = XP(M)
      IJKLX((LOC+LBIN-1)*NWDVAR+1) = IJKL-IJKLAD(IBOX)
C
      LCOUNT = LCOUNT+1
      IF (LCOUNT .LE. LBIN) GO TO 160
      LCOUNT = LCOUNT-1
      LOC = LOCB+(LBIN+LBIN)
      IJKLX((LOC+1-1)*NWDVAR+1) = LCOUNT
      IJKLX((LOC+2-1)*NWDVAR+1) = ICHAIN(IBOX)
      CALL RAWRIT(IDAF,IODA,XIJKL(LOCB+1),LENGTH,NDAF,IJKLX)
      ICHAIN(IBOX) = NDAF
      NDAF = NDAF+1
      LCOUNT = 1
  160 CONTINUE
      ICOUNT(IBOX) = LCOUNT
  180 CONTINUE
      IF (NX .GT. 0) GO TO 140
C
C     ----- DRAIN ALL THE BOXES -----
C
  200 CONTINUE
      DO 220 IBOX = 1,NBOX
         LOCB = LOCBOX(IBOX)
         LOC = LOCB+(LBIN+LBIN)
         LCOUNT = ICOUNT(IBOX)
         IJKLX((LOC+1-1)*NWDVAR+1) = LCOUNT-1
         IJKLX((LOC+2-1)*NWDVAR+1) = ICHAIN(IBOX)
         CALL RAWRIT(IDAF,IODA,XIJKL(LOCB+1),LENGTH,NDAF,IJKLX)
         ICHAIN(IBOX) = NDAF
         NDAF = NDAF+1
  220 CONTINUE
      RETURN
      END
C*MODULE ORDINT  *DECK ORDERB
      SUBROUTINE ORDERB(NBOX,NFTO,XO,IX,IDAF,IODA,ICHAIN,
     +                  XIJKL,XX,IXX,LBOX,LBIN,L1,I0,J0,K0,L0
     *                 ,NXO,NSQUAR,IW)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL OUT,PACK2E,GOPARR,DSKWRK,MASWRK
C
      DIMENSION XO(1),IX(1),IODA(1),ICHAIN(1),XIJKL(1)
      DIMENSION XX(1),IXX(1)
C
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /INTPRT/ QINT(3),VALINT(3),JCINT(16),OUT
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCKLAB/ LABSIZ
C
      PARAMETER (ZERO=0.0D+00)
C
      NREC = 0
      LENGTH = 2*LBIN+2
      Q = ZERO
      J = J0
      K = K0
      L = L0
      VAL = ZERO
C
      IBOX = 0
  100 IBOX = IBOX+1
      IF (IBOX .GT. NBOX) RETURN
      CALL VCLR(XIJKL,1,LBOX)
C
C     ----- READ IN BINS FROM -DAF- -----
C
      NDAF = ICHAIN(IBOX)
  140 CALL RAREAD(IDAF,IODA,XX,LENGTH,NDAF,0)
      NINT = IXX((LENGTH-1-1)*NWDVAR+1)
      DO 160 INT = 1,NINT
      IVAL = IXX((INT+LBIN-1)*NWDVAR+1)
  160 XIJKL(IVAL) = XX(INT)
      NDAF = IXX((LENGTH-1)*NWDVAR+1)
      IF (NDAF .GT. 0) GO TO 140
C
C     ----- WRITE INTEGRALS OUT ON -NFTO- -----
C
      LCOUNT = 0
      IF (NSQUAR .NE. 0) GO TO 300
C
      IST = I0
      DO 240 I = IST,L1
      II = I
      JST = J0
      DO 220 J = JST,I
      JJ = J
      J0 = 1
      KST = K0
      DO 200 K = KST,I
      KK = K
      K0 = 1
      LMAX = K
      IF (K .EQ. I) LMAX = J
      LST = L0
      DO 180 L = LST,LMAX
      LL = L
      L0 = 1
      LCOUNT = LCOUNT+1
      IF (LCOUNT .GT. LBOX) GO TO 420
      VAL = XIJKL(LCOUNT)
      IF (VAL .EQ. ZERO) GO TO 180
C
               NPACK = NXO
               IPACK = II
               JPACK = JJ
               KPACK = KK
               LPACK = LL
               IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                 IX( 2*NPACK-1 ) = ISHFT( IPACK, 16 ) + JPACK
                 IX( 2*NPACK   ) = ISHFT( KPACK, 16 ) + LPACK
#endif
#if defined(SPEC_CPU_ILP64)
                 LABEL = ISHFT( IPACK, 48 ) + ISHFT( JPACK, 32 ) +
     *                   ISHFT( KPACK, 16 ) + LPACK
                 IX(NPACK) = LABEL
#endif
               ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                 LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                   ISHFT( KPACK,  8 ) + LPACK
                 IX(NPACK) = LABEL
#endif
#if defined(SPEC_CPU_ILP64)
                 IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                   LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                     ISHFT( KPACK,  8 ) + LPACK
                   IX( NPACK/2 ) = IX( NPACK/2 ) + LABEL
                 ELSE
                   LABEL = ISHFT( IPACK, 56 ) + ISHFT( JPACK, 48 ) +
     *                     ISHFT( KPACK, 40 ) + ISHFT( LPACK, 32 )
                   IX( (NPACK/2)+1 ) = LABEL
                 END IF
#endif
               END IF
C
      XO(NXO) = VAL
      IF (OUT) CALL INTOUT(II,JJ,KK,LL,Q,NXO,VAL)
      NXO = NXO+1
      IF (NXO .LE. NINTMX) GO TO 180
      CALL PWRIT(NFTO,XO,IX,NINTMX,NINTMX)
      NREC = NREC + 1
      NXO = 1
  180 CONTINUE
  200 CONTINUE
  220 CONTINUE
  240 CONTINUE
      GO TO 400
C
  300 IST = I0
      DO 380 I = IST,L1
      II = I
      JST = J0
      DO 360 J = JST,I
      JJ = J
      J0 = 1
      KST = K0
      DO 340 K = KST,L1
      KK = K
      K0 = 1
      LST = L0
      DO 320 L = LST,K
         LL = L
         L0 = 1
         LCOUNT = LCOUNT+1
         IF (LCOUNT .GT. LBOX) GO TO 420
         VAL = XIJKL(LCOUNT)
         IF (VAL .EQ. ZERO) GO TO 320
C
               NPACK = NXO
               IPACK = II
               JPACK = JJ
               KPACK = KK
               LPACK = LL
               IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                 IX( 2*NPACK-1 ) = ISHFT( IPACK, 16 ) + JPACK
                 IX( 2*NPACK   ) = ISHFT( KPACK, 16 ) + LPACK
#endif
#if defined(SPEC_CPU_ILP64)
                 LABEL = ISHFT( IPACK, 48 ) + ISHFT( JPACK, 32 ) +
     *                   ISHFT( KPACK, 16 ) + LPACK
                 IX(NPACK) = LABEL
#endif
               ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                 LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                   ISHFT( KPACK,  8 ) + LPACK
                 IX(NPACK) = LABEL
#endif
#if defined(SPEC_CPU_ILP64)
                 IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                   LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                     ISHFT( KPACK,  8 ) + LPACK
                   IX( NPACK/2 ) = IX( NPACK/2 ) + LABEL
                 ELSE
                   LABEL = ISHFT( IPACK, 56 ) + ISHFT( JPACK, 48 ) +
     *                     ISHFT( KPACK, 40 ) + ISHFT( LPACK, 32 )
                   IX( (NPACK/2)+1 ) = LABEL
                 END IF
#endif
               END IF
C
            XO(NXO) = VAL
            IF (OUT) CALL INTOUT(II,JJ,KK,LL,Q,NXO,VAL)
            NXO = NXO+1
            IF (NXO .LE. NINTMX) GO TO 320
               CALL PWRIT(NFTO,XO,IX,NINTMX,NINTMX)
               NREC = NREC + 1
               NXO = 1
  320 CONTINUE
  340 CONTINUE
  360 CONTINUE
  380 CONTINUE
C
  400 NINT = -(NXO-1)
      CALL PWRIT(NFTO,XO,IX,NINT,NINTMX)
      IF (OUT) CALL INTOUT(I,J,K,L,Q,NXO,VAL)
      NNINT = NREC * NINTMX - NINT
      NREC = NREC+1
      IF (MASWRK) WRITE (IW,9008) NNINT, NREC, NINTMX
      RETURN
C
  420 CONTINUE
      I0 = II
      J0 = JJ
      K0 = KK
      L0 = LL
      GO TO 100
C
 9008 FORMAT(" TOTAL NUMBER OF ORDERED 2E-INTEGRALS KEPT = ",I10,
     +     4H IN ,I10,14H RECORD(S) OF ,I5,11H INTEGRALS.)
      END
C*MODULE ORDINT  *DECK ORDRJK
      SUBROUTINE ORDRJK
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      LOGICAL PACK2E,OUT,SOME,GOPARR,DSKWRK,MASWRK,INITAL
C
      COMMON /CIFILS/ NFT11,NFT12,NFT13,NFT14,NFT15,NFT16,IDAF20,NEMEMX
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NX,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /INTPRT/ QINT(3),VALINT(3),JCINT(16),OUT
      COMMON /IOFILE/ IR,IW,IP,IJK,IJKT,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /ORDOPT/ NORDER,NDAR,LDAR,NBOXMX,NWORD,NOMEM,NSQUAR
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
      SAVE INITAL,IJKO
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /, DEBUG/8HDEBUG   /, DBUGME/8HORDRJK  /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      DATA CHECK_STR/"CHECK   "/, DEBUG_STR/"DEBUG   "/, 
     *     DBUGME_STR/"ORDRJK  "/
#endif
      DATA INITAL/.TRUE./, IJKO/24/
C
      SOME = NPRINT .NE. -5 .AND. MASWRK
      OUT = NPRINT .EQ. 4 .AND. MASWRK
      IF (EXETYP .EQ. DEBUG  .OR. EXETYP .EQ. DBUGME) THEN
         SOME = .TRUE. .AND. MASWRK
         OUT  = .TRUE. .AND. MASWRK
      END IF
      IF(SOME) WRITE (IW,9008)
C
      IF(INITAL) THEN
         CALL SEQOPN(IJKO,'ORDINT','UNKNOWN',.FALSE.,'UNFORMATTED')
         INITAL = .FALSE.
      END IF
C
      L1 = NUM
      L2 = (L1*(L1+1))/2
      L4 = (L2*(L2+1))/2
      IF (NSQUAR .NE. 0) L4 = L2 * L2
C
      NFTI = IJK
      NFTO = IJKO
C
C     ----- GET MEMORY -----
C
      CALL VALFM(LOADFM)
      CALL GOTFM(NGOT)
      IF (NWORD.GT.0) NGOT = MIN(NGOT, NWORD)
C
      I10 = 1 + LOADFM
      I20 = I10 + (L2-1)/NWDVAR + 1
      I30 = I20 + L4
      I40 = I30 + NINTMX
      I50 = I40 + NINTMX
      LAST= I50 + NINTMX
      NEED = LAST - LOADFM - 1
C
      IF(SOME) WRITE (IW,9028) NEED,NGOT
      IF (NEED .GT. NGOT) GO TO 100
      IF (NOMEM .EQ. 1) GO TO 100
C
C     ----- ORDER THE INTEGRALS IN MEMORY -----
C
C     INDEX    ARRAY    AT X(I10)
C     SORTING  ARRAY    AT X(I20)
C
      IF(SOME) WRITE (IW,9048)
      CALL GETFM(NEED)
C
      IF (EXETYP .EQ. CHECK) GO TO 300
      CALL INCORE(X(I20),X(I10),X(I40),X(I50),L1,L2,L4,
     *            IW,NFTI,NFTO,NINTMX,NSQUAR)
      GO TO 300
C
C     ----- 2 STEP ORDERING TECHNIQUE -----
C
  100 CONTINUE
      IF(SOME) WRITE (IW,9068)
      NTRY = 0
      NDARSV = NDAR
  110 CONTINUE
C
C     NDAR          = # OF -DA- RECORDS
C     LDAR          = # OF INT. / -DA- RECORD
C
C     NBOX          = # OF BOXES IN 1 PASS
C     LBOX          = # OF INT. / BOX
C
C     NBIN          = # OF BINS IN 1 BOX
C     LBIN          = # OF INT. / BIN
C
C     NPASS         = # OF PASSES
C     LPASS         = # OF INT. / PASS
C
C     NBOXMX        = MAXIMUM # OF BOXES
C
C     L4            = MAXIMUM # OF INTEGRALS
C
C     +--------------------------------------------------------------+
C                                NGOT
C     +------+------+-------+--+--+--+--+--------+-------------+
C      NINTMX NINTMX NDARU+1   4*NBOXU      L2      (2*LDARU+2)**NBOXU
C     +------+------+-------+--+-----------------------------+
C      NINTMX NINTMX NDARU+1 NBOXU            LBOXU
C     ----- STEP -A-
C
C      X(I10)     INTEGRAL BUFFER FOR -NFTO-
C      X(I20)     INDEX    BUFFER FOR -NFTO-
C      X(I30)     -DA- FILE INDEX
C      X(I40)     BIN CHAINING INDEX
C      X(I50)     BIN FILLING COUNTER
C      X(I51)     BOX LOCATION BIAS
C      X(I52)     BOX ADDRESS BIAS
C      X(I60)     PAIR INDEX ARRAY
C      X(I70)     BIN SPACE
C      X(I70)     BIN SPACE
C
C     ----- STEP -B-
C
C      X(I10)     INTEGRAL BUFFER FOR -NFTO-
C      X(I20)     INDEX    BUFFER FOR -NFTO-
C      X(I30)     -DA- FILE INDEX
C      X(I40)     BIN CHAINING INDEX
C      X(I50)     BOX SPACE
C      X(I50)     BOX SPACE
C      X(I81)     INDEX    BUFFER FOR -IDAF20-
C      X(I81)     INTEGRAL BUFFER FOR -IDAF20-
C
      LWORDA = NGOT-2*NINTMX-(NDAR+1)-4*NBOXMX-L2
      LWORDB = NGOT-2*NINTMX-(NDAR+1)-NBOXMX-(2*LDAR+2)
      IF (LWORDA .GT. 0) GO TO 120
         MORE = -LWORDA
         IF (MASWRK) WRITE (IW,9088) NGOT,MORE
         CALL ABRT
C
  120 CONTINUE
      IF (LWORDB .GE. LDAR) GO TO 130
         MORE = LDAR-LWORDB
         IF (MASWRK) WRITE (IW,9088) NGOT,MORE
         CALL ABRT
C
  130 CONTINUE
C
      LBIN = LDAR
      NBIN = MIN (LWORDB/LDAR, NDAR)
      LBOX = LBIN*NBIN
C
      N1 = NDAR/NBIN
      N2 = LWORDA/(2*LDAR+2)
      N3 = (L4-1)/LBOX+1
      NBOX = MIN(N1,N2,N3,NBOXMX)
      IF (NBOX .GT. 0) GO TO 140
         IF (MASWRK) WRITE (IW,9128) NDAR,LDAR,NGOT,NBOXMX
         CALL ABRT
C
  140 CONTINUE
C
C     PARAMETERS GIVEN -   L4,     NDAR,   LDAR,   LWORDA
C                USED  -   L4,NBOX*NBIN,   LDAR,NBOX*LBIN
C
C     # OF PASSES               NPASS
C     # INT./PASS               LPASS=LBOX*NBOX=(LBIN*NBIN)*NBOX
C     # OF BOXES /PASS          NBOX
C     # INT./BOX                LBOX=LBIN*NBIN
C     # OF BINS  /BOX           NBIN
C     # INT./BIN                LBIN=LDAR
C
      NBOXU = NBOX
      NBINU = NBIN
      LBINU = LBIN
      LBOXU = LBINU*NBINU
      LPASSU = LBOXU*NBOXU
      NPASSU = (L4-1)/LPASSU+1
      LDARU = LDAR
      NDARU = NBINU*NBOXU
      LWORDA = 2*NINTMX+(NDARU+1)+4*NBOXU+L2+NBOXU*(2*LBINU+2)
      LWORDB = 2*NINTMX+(NDARU+1)+NBOXU+LBOXU+(2*LDARU+2)
      LWORDU = MAX(LWORDA,LWORDB)
      MXBOXU = NBOXU*NPASSU
      IF(NPASSU.GT.1  .AND.  NTRY.LE.5) THEN
         NDAR = NDAR+1000
         NTRY = NTRY+1
         GO TO 110
      END IF
      IF(SOME) WRITE (IW,9108) L4,NDAR,LDAR,NGOT,NDARU
     *          ,LDARU,LWORDU,NPASSU,LPASSU,NBOXU,NBOXMX,LBOXU
     *          ,NBINU,LBINU,MXBOXU
C
C     ----- GET MEMORY -----
C
      I10 = 1 + LOADFM
      I20 = I10+NINTMX
      I30 = I20+NINTMX
      I40 = I30+NDARU+1
      I50 = I40+NBOXU
      I51 = I50+NBOXU
      I52 = I51+NBOXU
      I60 = I52+NBOXU
      I70 = I60+L2
      I80 = I70+NBOXU*(2*LBINU+2)
      I81 = I50+LBOXU
      I82 = I81+(2*LDARU+2)
      LAST = MAX(I80,I82)
      NEED = LAST - LOADFM
      CALL GETFM(NEED)
C
C     ----- OPEN DIRECT ACCESS FILE -IDAF20- -----
C
      NDAF20 = NDARU+1
      LEND = 2*LBINU + 2
      CALL RAOPEN(IDAF20,X(I30),0,NDAF20,LEND,NPRINT)
      NFTI = IJK
      NFTO = IJKO
      CALL SEQREW(NFTO)
C
C     ----- LOOP OVER PASSES -----
C
      NXO = 1
      I0 = 1
      J0 = 1
      K0 = 1
      L0 = 1
      ILOW = 1
      IHIGH = LPASSU
      MBOX = NBOXU
      DO 160 IBOX = 1,MXBOXU,NBOXU
         IF (IBOX+NBOXU .GT. MXBOXU) MBOX = MXBOXU-IBOX+1
         IF (IBOX.NE.1) THEN
            IF (MASWRK) WRITE(IW,9198) NPASSU,NDARSV,NDAF20
            CALL ABRT
         END IF
         IF (EXETYP .EQ. CHECK) GO TO 160
C
         CALL ORDERA(ILOW,IHIGH,NFTI,X(I10),X(I20),IDAF20,X(I30),
     *               X(I40),X(I50), X(I51),X(I52),X(I60),X(I70),X(I70),
     *               NBOXU,LBOXU,LBINU,L2,NINTMX,NSQUAR)
C
         CALL ORDERB(MBOX,NFTO,X(I10),X(I20),IDAF20,X(I30),X(I40),
     *               X(I50),X(I81),X(I81),LBOXU,LBINU,L1,I0,J0,K0,
     *               L0,NXO,NSQUAR,IW)
C
         ILOW = ILOW+LPASSU
         IHIGH = IHIGH+LPASSU
  160 CONTINUE
C
      IF(EXETYP.NE.CHECK) CALL RACLOS(IDAF20,'DELETE')
C
C     ----- ALL DONE -----
C     NOTE THAT THE ORDERED AND UNORDERED UNIT NUMBERS ARE NOW EXCHANGED
C
  300 CONTINUE
      CALL RETFM(NEED)
C
      NDUM = IJK
      IJK  = IJKO
      IJKO = NDUM
      CALL SEQREW(IJK)
      CALL SEQREW(IJKO)
C
      IF (MASWRK) WRITE (IW,9148)
      CALL TIMIT(1)
      RETURN
C
 9008 FORMAT(/10X,17("-")/10X,"INTEGRAL ORDERING"/10X,17("-"))
 9028 FORMAT(1X,I10,' WORDS NEEDED TO ORDER IN MEMORY (',
     *          I10,' AVAILABLE).')
 9048 FORMAT(1X,'CHOOSING IN MEMORY ORDERING OF INTEGRALS.')
 9068 FORMAT(1X,'CHOOSING OUT OF MEMORY ORDERING OF INTEGRALS.')
 9088 FORMAT(' NOT ENOUGH MEMORY. NGOT =',I10,", NEED ",I10, " MORE.")
 9108 FORMAT(/" NUMBER OF INTEGRALS     = ",I10,/,
     +     27H GIVEN NDAR,LDAR,LWORD   = ,3I10,/,
     +     27H USED  NDAR,LDAR,LWORD   = ,3I10,/,
     +     27H NUMBER OF PASSES        = ,I10,/,
     +     27H NUMBER OF INTEGRALS/PASS= ,I10,/,
     +     27H NUMBER OF BOXES/PASS    = ,I10,17H MAXIMUM GIVEN = ,I10/
     +     27H NUMBER OF INTEGRALS/BOX = ,I10,/,
     +     27H NUMBER OF BINS/BOX      = ,I10,/,
     +     27H NUMBER OF INTEGRALS/BIN = ,I10,/,
     +     27H TOTAL NUMBER OF BOXES   = ,I10,/)
 9128 FORMAT(" INCOHERENT PARAMETERS IN THE ORDERING OF INTEGRALS.", /,
     +     34H GIVEN NDAR, LDAR, NGOT, NBOXMX = ,4I10,
     +     39H REEVALUATE THE INPUT PARAMETERS. STOP.)
 9148 FORMAT(1X,'...... END OF INTEGRAL ORDERING ......')
 9198 FORMAT(/1X,'*** ERROR ***',I5,' PASSES NEEDED TO SORT INTEGRALS'/
     *    1X,'BUT GAMESS DOES NOT WORK FOR MORE THAN ONE PASS.'//
     *    1X,'GAMESS HAS ALREADY ATTEMPTED TO ADJUST THE $INTGRL',
     *       'PARAMETER "NDAR" UPWARDS'/
     *    1X,'BY 1000 FIVE TIMES, BUT THIS IS STILL NOT ENOUGH.'/
     *    1X,'PLEASE CHECK HOW MUCH DISK YOUR COMPUTER HAS FOR THE ',
     *       'SORT FILE "DASORT",'/
     *    1X,'AND INCREASE "NDAR" ACCORDINGLY (TO MAYBE DOUBLE OR ',
     *       'TREBLE THE CURRENT?).'//
     *    1X,'YOUR INPUT NDAR WAS',I8,' THE CURRENT NDAR=',I8)
      END
