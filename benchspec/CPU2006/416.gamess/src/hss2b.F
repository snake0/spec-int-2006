C  9 DEC 03 - TJD - ADD 2E- DERIV. INT. TO ANALYTIC MCSCF HESSIAN
C 26 OCT 00 - MWS - INTRODUCE MXAO PARAMETER
C 12 NOV 98 - GDF - ELIMINATE USE OF ISOOUT
C 17 OCT 96 - SPW - JDDEND,JDDFCK: CHANGES FOR CI GRADIENTS
C  5 MAR 96 - MWS - CHANGE SHELL SYMMETRY PACKING COMMONS
C 11 NOV 94 - MWS - REMOVE FTNCHEK WARNINGS
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C  1 JUN 94 - MWS - JDDEND: CHANGE DEBUGGING OUTPUT
C  9 DEC 93 - MWS - JDDODF: CHANGE H-A DAF RECORD NUMBER
C 10 AUG 93 - MWS - MOVE SYMEG TO GRD1
C 22 MAR 92 - MWS - REPLACE /DDIJKL/,/DDWXYZ/ WITH DYNAMIC STORAGE
C 12 MAR 92 - MWS - REDIMENSION TO 500 ATOMS
C 10 JAN 92 - TLW - CHANGE REWINDS TO CALL SEQREW
C  6 JAN 92 - TLW - MAKE WRITE PARALLEL;ADD COMMON PAR
C 30 AUG 91 - RN  - JDDEND: DIMENSION SHLJ,SHLK
C 14 AUG 91 - TLW - RFCKR,SYMDDM,SYMEG,SYMEH: FTR,GTR IN COMMON SYMSPD
C  7 AUG 90 - TLW - ADD CF AND CG TO COMMON NSHEL
C 24 JUN 90 - MWS - JDDFCK: ROGVB MAKES J-A AND K-A INSTEAD OF F-A.
C                   JDDODF: ASSEMBLE F-A AND HCI-A FROM H-A, J-A, K-A
C 23 JUN 89 - MWS - CHANGES FOR ROHF AND GVB DERIVATIVE FOCK MATRICES.
C                   JDDFCK: CODE FOR ROHF AND GENERAL GVB CASE
C                   JDDEND: FORM SQUARE DF MATRICES OVER MOS
C                   SYMDF: CHANGE DF TO 4 SUBSCRIPT ARRAY
C 13 MAR 89 - MWS - JDDFCK: ADD CODE FOR UHF BETA FOCK DERIVATIVE
C                   SPLIT SUBROUTINE JDDEND INTO JDDESH AND JDDEND
C                   JDDEND: TRANSFORM DF MATRICES TO MO BASIS
C                   SYMDF: ALLOW FOR SYMMETRIZATION OF BETA MATRICES
C  8 MAR 89 - MWS - APPEND SKIP TO COMMON /JDDSKP/
C 26 FEB 89 - STE - JDDEND: DELETE 9999
C 23 FEB 89 - MWS - USE /DDIJKL/ AND /DDWXYZ/ TO AVOID ARG PASSING
C 19 JAN 89 - MWS - ADAPT ANALYTIC RHF HESSIAN CODE FROM MICHEL'S HONDO7
C
C*MODULE HSS2B   *DECK FIFJKL
      SUBROUTINE FIFJKL(ISH,JSH,KSH,LSH,WX,WY,WZ,GS,DS,NID,NJD,NKD)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL IANDJ,KANDL,SAME,NONORM
C
      DIMENSION WX(2808),WY(2808),WZ(2808),GS(6084),DS(9)
      DIMENSION MID(10),MJD(10),MIF(25),MJF(25),MJS(25)
C
      PARAMETER (MXGTOT=5000, MXSH=1000)
C
      COMMON /JDDSTV/ NFORB(4),NSORB(4),NFTABL(13,4),NSTABL(22,4),
     *                LFTABL(20,2),LSTABL(35,2)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /INTJDD/ FDRV(3,4),SDRV(9,4,4),ABDENS(1296)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
C
      PARAMETER (ZERO=0.0D+00, SQRT3=1.73205080756888D+00)
C
C     ----- ZERO CLEAR OF WORKING ARRAYS -----
C
      NONORM = NORMF .EQ. 1 .AND. NORMP .EQ. 1
      DXX = ZERO
      DYX = ZERO
      DZX = ZERO
      DXY = ZERO
      DYY = ZERO
      DZY = ZERO
      DXZ = ZERO
      DYZ = ZERO
      DZZ = ZERO
      DO 10 I = 1,2808
      WX(I) = ZERO
      WY(I) = ZERO
   10 WZ(I) = ZERO
C
C     ----- SETTING INITIAL PARAMETERS -----
C
      MINI = KMIN(ISH)
      MINJ = KMIN(JSH)
      MINK = KMIN(KSH)
      MINL = KMIN(LSH)
      MAXI = KMAX(ISH)
      MAXJ = KMAX(JSH)
      MAXK = KMAX(KSH)
      MAXL = KMAX(LSH)
      NL0  = MAXL-MINL+1
      NK0  = MAXK-MINK+1
      NJ0  = MAXJ-MINJ+1
      NI0  = MAXI-MINI+1
      NTYPJ= NJ0/2+1
      LTYPJ= NTYPJ/4+1
      NJ1  = NFORB(NTYPJ)
      IANDJ= ISH .EQ. JSH
      KANDL= KSH .EQ. LSH
      SAME = ((ISH .EQ. KSH .AND. JSH .EQ. LSH) .OR.
     1        (ISH .EQ. LSH .AND. JSH .EQ. KSH)     )
      NLF1 = 169*NK0
      NKF1 = 169
      NLF2 = 13 *NI0*NK0
      NKF2 = 13 *NI0
      DO 20 I = MINI,MAXI
      MID(I) = NID*(I-MINI)
   20 MIF(I) = 13 *(I-MINI)
      DO 21 J = MINJ,MAXJ
   21 MJD(J) = NJD*(J-MINJ)
      DO 22 J = 1,NJ1
      NORBJ  = NFTABL(J,NTYPJ)
      NORBJ  = LFTABL(NORBJ,LTYPJ)
      MJS(J) = 13 * (NORBJ-1)
   22 MJF(J) =       NORBJ
      DO 4200 L = 1,NL0
      LF1 = NLF1*(L-1)+1
      LF2 = NLF2*(L-1)
      IF(KANDL) NK0 = L
      DO 4200 K = 1,NK0
      KF1 = NKF1*(K-1)+LF1
      KF2 = NKF2*(K-1)+LF2
      DO 4100 J = 1,NJ1
      NF  = MJS(J)+KF1
      JF2 = MJF(J)+KF2
      DO 4000 I = MINI,MAXI
      GO TO (300,310,320,330,340,350,360,370,380,390),I
  300 X =            GS(NF+ 1)
      Y =            GS(NF+ 2)
      Z =            GS(NF+ 3)
      GO TO 400
  310 X =            GS(NF+ 4)+          GS(NF   )
      Y =            GS(NF+ 7)
      Z =            GS(NF+ 8)
      GO TO 400
  320 X =            GS(NF+ 7)
      Y =            GS(NF+ 5)+          GS(NF   )
      Z =            GS(NF+ 9)
      GO TO 400
  330 X =            GS(NF+ 8)
      Y =            GS(NF+ 9)
      Z =            GS(NF+ 6)+          GS(NF   )
      GO TO 400
  340 X =            GS(NF+3)+GS(NF   )+GS(NF   )
      Y =            GS(NF+6)
      Z =            GS(NF+ 7)
      GO TO 400
  350 X =            GS(NF+ 8)
      Y =            GS(NF+4)+GS(NF+1)+GS(NF+1)
      Z =            GS(NF+ 9)
      GO TO 400
  360 X =            GS(NF+10)
      Y =            GS(NF+11)
      Z =            GS(NF+ 5)+GS(NF+ 2)+GS(NF+ 2)
      GO TO 400
  370 X =            GS(NF+ 6)+          GS(NF+ 1)
      Y =            GS(NF+ 8)+          GS(NF   )
      Z =            GS(NF+12)
      GO TO 395
  380 X =            GS(NF+ 7)+          GS(NF+ 2)
      Y =            GS(NF+12)
      Z =            GS(NF+10)+          GS(NF   )
      GO TO 395
  390 X =            GS(NF+12)
      Y =            GS(NF+ 9)+GS(NF+ 2)
      Z =            GS(NF+11)+GS(NF+ 1)
  395 IF(NONORM) GO TO 400
      X =            X*SQRT3
      Y =            Y*SQRT3
      Z =            Z*SQRT3
  400 MAD = MIF(I) + JF2
      WX(MAD) =      WX(MAD) + X
      WY(MAD) =      WY(MAD) + Y
      WZ(MAD) =      WZ(MAD) + Z
 4000 CONTINUE
 4100 CONTINUE
 4200 CONTINUE
      NKL = 0
      DO 5200 L = 1,NL0
      LD = L
      LF1 = NLF2*(L-1)+1
      IF(KANDL) NK0 = L
      DO 5200 K = 1,NK0
      NKL = NKL + 1
      KD  = NKD *(K-1) + LD
      KF1 = NKF2*(K-1) + LF1
      NIJ = 0
      DO 5100 J = MINJ,MAXJ
      JD  = MJD(J) + KD
      IF(IANDJ) MAXI = J
      DO 5000 I = MINI,MAXI
      IF(.NOT.SAME) GO TO 500
      NIJ = NIJ + 1
      IF(NIJ .GT. NKL) GO TO 5000
  500 DENSTY = ABDENS(MID(I)+JD)
      NF = MIF(I)+KF1
      GO TO (600,610,620,630,640,650,660,670,680,690),J
  600 XX = WX(NF+ 1)
      YX = WX(NF+ 2)
      ZX = WX(NF+ 3)
      XY = WY(NF+ 1)
      YY = WY(NF+ 2)
      ZY = WY(NF+ 3)
      XZ = WZ(NF+ 1)
      YZ = WZ(NF+ 2)
      ZZ = WZ(NF+ 3)
      GO TO 700
  610 XX = WX(NF+ 4)+WX(NF   )
      YX = WX(NF+ 7)
      ZX = WX(NF+ 8)
      XY = WY(NF+ 4)+WY(NF   )
      YY = WY(NF+ 7)
      ZY = WY(NF+ 8)
      XZ = WZ(NF+ 4)+WZ(NF   )
      YZ = WZ(NF+ 7)
      ZZ = WZ(NF+ 8)
      GO TO 700
  620 XX = WX(NF+ 7)
      YX = WX(NF+ 5)+WX(NF   )
      ZX = WX(NF+ 9)
      XY = WY(NF+ 7)
      YY = WY(NF+ 5)+WY(NF   )
      ZY = WY(NF+ 9)
      XZ = WZ(NF+ 7)
      YZ = WZ(NF+ 5)+WZ(NF   )
      ZZ = WZ(NF+ 9)
      GO TO 700
  630 XX = WX(NF+ 8)
      YX = WX(NF+ 9)
      ZX = WX(NF+ 6)+WX(NF   )
      XY = WY(NF+ 8)
      YY = WY(NF+ 9)
      ZY = WY(NF+ 6)+WY(NF   )
      XZ = WZ(NF+ 8)
      YZ = WZ(NF+ 9)
      ZZ = WZ(NF+ 6)+WZ(NF   )
      GO TO 700
  640 XX = WX(NF+ 3)+WX(NF   )+WX(NF   )
      YX = WX(NF+ 6)
      ZX = WX(NF+ 7)
      XY = WY(NF+ 3)+WY(NF   )+WY(NF   )
      YY = WY(NF+ 6)
      ZY = WY(NF+ 7)
      XZ = WZ(NF+ 3)+WZ(NF   )+WZ(NF   )
      YZ = WZ(NF+ 6)
      ZZ = WZ(NF+ 7)
      GO TO 700
  650 XX = WX(NF+ 8)
      YX = WX(NF+ 4)+WX(NF+ 1)+WX(NF+ 1)
      ZX = WX(NF+ 9)
      XY = WY(NF+ 8)
      YY = WY(NF+ 4)+WY(NF+ 1)+WY(NF+ 1)
      ZY = WY(NF+ 9)
      XZ = WZ(NF+ 8)
      YZ = WZ(NF+ 4)+WZ(NF+ 1)+WZ(NF+ 1)
      ZZ = WZ(NF+ 9)
      GO TO 700
  660 XX = WX(NF+10)
      YX = WX(NF+11)
      ZX = WX(NF+ 5)+WX(NF+ 2)+WX(NF+ 2)
      XY = WY(NF+10)
      YY = WY(NF+11)
      ZY = WY(NF+ 5)+WY(NF+ 2)+WY(NF+ 2)
      XZ = WZ(NF+10)
      YZ = WZ(NF+11)
      ZZ = WZ(NF+ 5)+WZ(NF+ 2)+WZ(NF+ 2)
      GO TO 700
  670 XX = WX(NF+ 6)+WX(NF+ 1)
      YX = WX(NF+ 8)+WX(NF   )
      ZX = WX(NF+12)
      XY = WY(NF+ 6)+WY(NF+ 1)
      YY = WY(NF+ 8)+WY(NF   )
      ZY = WY(NF+12)
      XZ = WZ(NF+ 6)+WZ(NF+ 1)
      YZ = WZ(NF+ 8)+WZ(NF   )
      ZZ = WZ(NF+12)
      GO TO 695
  680 XX = WX(NF+ 7)+WX(NF+ 2)
      YX = WX(NF+12)
      ZX = WX(NF+10)+WX(NF   )
      XY = WY(NF+ 7)+WY(NF+ 2)
      YY = WY(NF+12)
      ZY = WY(NF+10)+WY(NF   )
      XZ = WZ(NF+ 7)+WZ(NF+ 2)
      YZ = WZ(NF+12)
      ZZ = WZ(NF+10)+WZ(NF   )
      GO TO 695
  690 XX = WX(NF+12)
      YX = WX(NF+ 9)+WX(NF+ 2)
      ZX = WX(NF+11)+WX(NF+ 1)
      XY = WY(NF+12)
      YY = WY(NF+ 9)+WY(NF+ 2)
      ZY = WY(NF+11)+WY(NF+ 1)
      XZ = WZ(NF+12)
      YZ = WZ(NF+ 9)+WZ(NF+ 2)
      ZZ = WZ(NF+11)+WZ(NF+ 1)
  695 IF(NONORM) GO TO 700
      XX = XX*SQRT3
      YX = YX*SQRT3
      ZX = ZX*SQRT3
      XY = XY*SQRT3
      YY = YY*SQRT3
      ZY = ZY*SQRT3
      XZ = XZ*SQRT3
      YZ = YZ*SQRT3
      ZZ = ZZ*SQRT3
  700 DXX = DXX + XX*DENSTY
      DYX = DYX + YX*DENSTY
      DZX = DZX + ZX*DENSTY
      DXY = DXY + XY*DENSTY
      DYY = DYY + YY*DENSTY
      DZY = DZY + ZY*DENSTY
      DXZ = DXZ + XZ*DENSTY
      DYZ = DYZ + YZ*DENSTY
      DZZ = DZZ + ZZ*DENSTY
 5000 CONTINUE
 5100 CONTINUE
 5200 CONTINUE
      DS(1) = DXX
      DS(2) = DYX
      DS(3) = DZX
      DS(4) = DXY
      DS(5) = DYY
      DS(6) = DZY
      DS(7) = DXZ
      DS(8) = DYZ
      DS(9) = DZZ
      RETURN
      END
C*MODULE HSS2B   *DECK FIJFKL
      SUBROUTINE FIJFKL(ISH,JSH,KSH,LSH,WX,WY,WZ,GS,DS,NID,NJD,NKD)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL IANDJ,KANDL,SAME,NONORM
C
      DIMENSION WX(2808),WY(2808),WZ(2808),GS(6084),DS(9)
      DIMENSION MID(10),MJD(10),MIF(25),MJF(25),MKF(25),MJS(25),MKS(25)
C
      PARAMETER (MXGTOT=5000, MXSH=1000)
C
      COMMON /INTJDD/ FDRV(3,4),SDRV(9,4,4),ABDENS(1296)
      COMMON /JDDSTV/ NFORB(4),NSORB(4),NFTABL(13,4),NSTABL(22,4),
     *                LFTABL(20,2),LSTABL(35,2)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
C
      PARAMETER (ZERO=0.0D+00, SQRT3=1.73205080756888D+00)
C
C     ----- ZERO CLEAR OF WORKING ARRAYS -----
C
      NONORM = NORMF .EQ. 1 .AND. NORMP .EQ. 1
      DXX = ZERO
      DYX = ZERO
      DZX = ZERO
      DXY = ZERO
      DYY = ZERO
      DZY = ZERO
      DXZ = ZERO
      DYZ = ZERO
      DZZ = ZERO
      DO 10 I = 1,2808
      WX(I) = ZERO
      WY(I) = ZERO
   10 WZ(I) = ZERO
C
C     ----- SETTING INITIAL PARAMETERS -----
C
      MINI = KMIN(ISH)
      MINJ = KMIN(JSH)
      MINK = KMIN(KSH)
      MINL = KMIN(LSH)
      MAXI = KMAX(ISH)
      MAXJ = KMAX(JSH)
      MAXK = KMAX(KSH)
      MAXL = KMAX(LSH)
      NL0  = MAXL-MINL+1
      NK0  = MAXK-MINK+1
      NJ0  = MAXJ-MINJ+1
      NI0  = MAXI-MINI+1
      NTYPK= NK0/2+1
      LTYPK= NTYPK/4+1
      NK1  = NFORB(NTYPK)
      IANDJ= ISH .EQ. JSH
      KANDL= KSH .EQ. LSH
      SAME = ((ISH .EQ. KSH .AND. JSH .EQ. LSH) .OR.
     1        (ISH .EQ. LSH .AND. JSH .EQ. KSH)     )
      NLF1 = 169*NJ0
      NJF1 = 169
      NLF2 = 13 *NI0*NJ0
      NJF2 = 13 *NI0
      DO 20 I = MINI,MAXI
      MID(I) = NID*(I-MINI)
   20 MIF(I) = 13 *(I-MINI)
      DO 21 J = MINJ,MAXJ
      MJS(J) = NJF1*(J-MINJ)
      MJF(J) = NJF2*(J-MINJ)
   21 MJD(J) = NJD *(J-MINJ)
      DO 22 K = 1,NK1
      NORBK  = NFTABL(K,NTYPK)
      NORBK  = LFTABL(NORBK,LTYPK)
      MKS(K) = 13 * (NORBK-1)
   22 MKF(K) =       NORBK
      DO 4200 L = 1,NL0
      LF1 = NLF1*(L-1)+1
      LF2 = NLF2*(L-1)
      DO 4200 K = 1,NK1
      KF1 = MKS(K)+LF1
      KF2 = MKF(K)+LF2
      DO 4100 J = MINJ,MAXJ
      NF  = MJS(J)+KF1
      JF2 = MJF(J)+KF2
      IF(IANDJ) MAXI = J
      DO 4000 I = MINI,MAXI
      GO TO (300,310,320,330,340,350,360,370,380,390),I
  300 X =            GS(NF+ 1)
      Y =            GS(NF+ 2)
      Z =            GS(NF+ 3)
      GO TO 400
  310 X =            GS(NF+ 4)+          GS(NF   )
      Y =            GS(NF+ 7)
      Z =            GS(NF+ 8)
      GO TO 400
  320 X =            GS(NF+ 7)
      Y =            GS(NF+ 5)+          GS(NF   )
      Z =            GS(NF+ 9)
      GO TO 400
  330 X =            GS(NF+ 8)
      Y =            GS(NF+ 9)
      Z =            GS(NF+ 6)+          GS(NF   )
      GO TO 400
  340 X =            GS(NF+3)+GS(NF   )+GS(NF   )
      Y =            GS(NF+6)
      Z =            GS(NF+ 7)
      GO TO 400
  350 X =            GS(NF+ 8)
      Y =            GS(NF+4)+GS(NF+1)+GS(NF+1)
      Z =            GS(NF+ 9)
      GO TO 400
  360 X =            GS(NF+10)
      Y =            GS(NF+11)
      Z =            GS(NF+ 5)+GS(NF+ 2)+GS(NF+ 2)
      GO TO 400
  370 X =            GS(NF+ 6)+          GS(NF+ 1)
      Y =            GS(NF+ 8)+          GS(NF   )
      Z =            GS(NF+12)
      GO TO 395
  380 X =            GS(NF+ 7)+          GS(NF+ 2)
      Y =            GS(NF+12)
      Z =            GS(NF+10)+          GS(NF   )
      GO TO 395
  390 X =            GS(NF+12)
      Y =            GS(NF+ 9)+GS(NF+ 2)
      Z =            GS(NF+11)+GS(NF+ 1)
  395 IF(NONORM) GO TO 400
      X =            X*SQRT3
      Y =            Y*SQRT3
      Z =            Z*SQRT3
  400 MAD = MIF(I) + JF2
      WX(MAD) =      WX(MAD) + X
      WY(MAD) =      WY(MAD) + Y
      WZ(MAD) =      WZ(MAD) + Z
 4000 CONTINUE
 4100 CONTINUE
 4200 CONTINUE
      NKL = 0
      DO 5200 L = MINL,MAXL
      LD  = L-MINL+1
      LF1 = NLF2*(L-MINL)+1
      IF(KANDL) MAXK = L
      DO 5200 K = MINK,MAXK
      NKL = NKL + 1
      KD  = NKD *(K-MINK) + LD
      NIJ = 0
      DO 5100 J = MINJ,MAXJ
      JD  = MJD(J) + KD
      JF1 = MJF(J) + LF1
      IF(IANDJ) MAXI = J
      DO 5000 I = MINI,MAXI
      IF(.NOT.SAME) GO TO 500
      NIJ = NIJ + 1
      IF(NIJ .GT. NKL) GO TO 5000
  500 DENSTY = ABDENS(MID(I)+JD)
      NF = MIF(I)+JF1
      GO TO (600,610,620,630,640,650,660,670,680,690),K
  600 XX = WX(NF+ 1)
      YX = WX(NF+ 2)
      ZX = WX(NF+ 3)
      XY = WY(NF+ 1)
      YY = WY(NF+ 2)
      ZY = WY(NF+ 3)
      XZ = WZ(NF+ 1)
      YZ = WZ(NF+ 2)
      ZZ = WZ(NF+ 3)
      GO TO 700
  610 XX = WX(NF+ 4)+WX(NF   )
      YX = WX(NF+ 7)
      ZX = WX(NF+ 8)
      XY = WY(NF+ 4)+WY(NF   )
      YY = WY(NF+ 7)
      ZY = WY(NF+ 8)
      XZ = WZ(NF+ 4)+WZ(NF   )
      YZ = WZ(NF+ 7)
      ZZ = WZ(NF+ 8)
      GO TO 700
  620 XX = WX(NF+ 7)
      YX = WX(NF+ 5)+WX(NF   )
      ZX = WX(NF+ 9)
      XY = WY(NF+ 7)
      YY = WY(NF+ 5)+WY(NF   )
      ZY = WY(NF+ 9)
      XZ = WZ(NF+ 7)
      YZ = WZ(NF+ 5)+WZ(NF   )
      ZZ = WZ(NF+ 9)
      GO TO 700
  630 XX = WX(NF+ 8)
      YX = WX(NF+ 9)
      ZX = WX(NF+ 6)+WX(NF   )
      XY = WY(NF+ 8)
      YY = WY(NF+ 9)
      ZY = WY(NF+ 6)+WY(NF   )
      XZ = WZ(NF+ 8)
      YZ = WZ(NF+ 9)
      ZZ = WZ(NF+ 6)+WZ(NF   )
      GO TO 700
  640 XX = WX(NF+ 3)+WX(NF   )+WX(NF   )
      YX = WX(NF+ 6)
      ZX = WX(NF+ 7)
      XY = WY(NF+ 3)+WY(NF   )+WY(NF   )
      YY = WY(NF+ 6)
      ZY = WY(NF+ 7)
      XZ = WZ(NF+ 3)+WZ(NF   )+WZ(NF   )
      YZ = WZ(NF+ 6)
      ZZ = WZ(NF+ 7)
      GO TO 700
  650 XX = WX(NF+ 8)
      YX = WX(NF+ 4)+WX(NF+ 1)+WX(NF+ 1)
      ZX = WX(NF+ 9)
      XY = WY(NF+ 8)
      YY = WY(NF+ 4)+WY(NF+ 1)+WY(NF+ 1)
      ZY = WY(NF+ 9)
      XZ = WZ(NF+ 8)
      YZ = WZ(NF+ 4)+WZ(NF+ 1)+WZ(NF+ 1)
      ZZ = WZ(NF+ 9)
      GO TO 700
  660 XX = WX(NF+10)
      YX = WX(NF+11)
      ZX = WX(NF+ 5)+WX(NF+ 2)+WX(NF+ 2)
      XY = WY(NF+10)
      YY = WY(NF+11)
      ZY = WY(NF+ 5)+WY(NF+ 2)+WY(NF+ 2)
      XZ = WZ(NF+10)
      YZ = WZ(NF+11)
      ZZ = WZ(NF+ 5)+WZ(NF+ 2)+WZ(NF+ 2)
      GO TO 700
  670 XX = WX(NF+ 6)+WX(NF+ 1)
      YX = WX(NF+ 8)+WX(NF   )
      ZX = WX(NF+12)
      XY = WY(NF+ 6)+WY(NF+ 1)
      YY = WY(NF+ 8)+WY(NF   )
      ZY = WY(NF+12)
      XZ = WZ(NF+ 6)+WZ(NF+ 1)
      YZ = WZ(NF+ 8)+WZ(NF   )
      ZZ = WZ(NF+12)
      GO TO 695
  680 XX = WX(NF+ 7)+WX(NF+ 2)
      YX = WX(NF+12)
      ZX = WX(NF+10)+WX(NF   )
      XY = WY(NF+ 7)+WY(NF+ 2)
      YY = WY(NF+12)
      ZY = WY(NF+10)+WY(NF   )
      XZ = WZ(NF+ 7)+WZ(NF+ 2)
      YZ = WZ(NF+12)
      ZZ = WZ(NF+10)+WZ(NF   )
      GO TO 695
  690 XX = WX(NF+12)
      YX = WX(NF+ 9)+WX(NF+ 2)
      ZX = WX(NF+11)+WX(NF+ 1)
      XY = WY(NF+12)
      YY = WY(NF+ 9)+WY(NF+ 2)
      ZY = WY(NF+11)+WY(NF+ 1)
      XZ = WZ(NF+12)
      YZ = WZ(NF+ 9)+WZ(NF+ 2)
      ZZ = WZ(NF+11)+WZ(NF+ 1)
  695 IF(NONORM) GO TO 700
      XX = XX*SQRT3
      YX = YX*SQRT3
      ZX = ZX*SQRT3
      XY = XY*SQRT3
      YY = YY*SQRT3
      ZY = ZY*SQRT3
      XZ = XZ*SQRT3
      YZ = YZ*SQRT3
      ZZ = ZZ*SQRT3
  700 DXX = DXX + XX*DENSTY
      DYX = DYX + YX*DENSTY
      DZX = DZX + ZX*DENSTY
      DXY = DXY + XY*DENSTY
      DYY = DYY + YY*DENSTY
      DZY = DZY + ZY*DENSTY
      DXZ = DXZ + XZ*DENSTY
      DYZ = DYZ + YZ*DENSTY
      DZZ = DZZ + ZZ*DENSTY
 5000 CONTINUE
 5100 CONTINUE
 5200 CONTINUE
      DS(1) = DXX
      DS(2) = DYX
      DS(3) = DZX
      DS(4) = DXY
      DS(5) = DYY
      DS(6) = DZY
      DS(7) = DXZ
      DS(8) = DYZ
      DS(9) = DZZ
      RETURN
      END
C*MODULE HSS2B   *DECK FSIJKL
      SUBROUTINE FSIJKL(ISH,JSH,KSH,LSH,GF,GS,GC,DF,DS,NID,NJD,NKD,NLD,
     1                  NGF,NGS,NGC)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL IANDJ,KANDL,SAME,NONORM
      LOGICAL FIRST,SECND,CPHF,BOTH,MFIRST,MSECND,MCPHF
C
      DIMENSION GF(NGF),GS(NGS),GC(NGC),DF(3),DS(9),MID(10),MJD(10),
     1          MKD(10),MLD(10)
C
      PARAMETER (MXGTOT=5000, MXSH=1000)
C
      COMMON /HSSPAR/ FIRST,SECND,CPHF,BOTH,MFIRST,MSECND,MCPHF
      COMMON /INTJDD/ FDRV(3,4),SDRV(9,4,4),ABDENS(1296)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
C
      DATA ZERO,SQRT3      /0.0D+00,1.73205080756888D+00/
      DATA TWO,THREE,FIVE  /2.0D+00,3.0D+00,5.0D+00/
C
C     ----- INITIAL SET OF PARAMETERS -----
C
      NONORM = NORMF .EQ. 1 .AND. NORMP .EQ. 1
      DX  = ZERO
      DY  = ZERO
      DZ  = ZERO
      DXX = ZERO
      DYX = ZERO
      DYY = ZERO
      DZX = ZERO
      DZY = ZERO
      DZZ = ZERO
      MINI = KMIN(ISH)
      MINJ = KMIN(JSH)
      MINK = KMIN(KSH)
      MINL = KMIN(LSH)
      MAXI = KMAX(ISH)
      MAXJ = KMAX(JSH)
      MAXK = KMAX(KSH)
      MAXL = KMAX(LSH)
      NK0 = MAXK-MINK+1
      NL0 = MAXL-MINL+1
      IANDJ = ISH .EQ. JSH
      KANDL = KSH .EQ. LSH
      SAME  = ((ISH .EQ. KSH .AND. JSH .EQ. LSH) .OR.
     1         (ISH .EQ. LSH .AND. JSH .EQ. KSH))
      NKF  = 13* (MAXJ-MINJ+1)
      NKS  = 22* (MAXJ-MINJ+1)
      NLF  = NKF*NK0
      NLS  = NKS*NK0
      DO 10 I = MINI,MAXI
   10 MID(I) = NID*(I-MINI)+1
      DO 11 J = MINJ,MAXJ
   11 MJD(J) = NJD*(J-MINJ)
      DO 12 K = 1,NK0
   12 MKD(K) = NKD*(K-1)
      DO 13 L = 1,NL0
   13 MLD(L) = NLD*(L-1)
C
C     ----- CALCULATION OF FIRST AND SECOND DERIVATIVES USING -----
C     ----- TWO ELECTRON INTEGRALS                            -----
C
      NKL   = 0
      NIJKL = -3
      DO 4200 L = 1,NL0
      LD = MLD(L)
      LF = NLF*(L-1)+1
      LS = NLS*(L-1)+1
      IF(KANDL) NK0=L
      DO 4200 K = 1,NK0
      NKL = NKL+1
      KD = MKD(K)+LD
      NF = NKF*(K-1)+LF
      NS = NKS*(K-1)+LS
      NIJ   = 0
      DO 4100 J = MINJ,MAXJ
      JD = MJD(J)+KD
      DO 4000 I = MINI,MAXI
      NIJ   = NIJ+1
      NIJKL = NIJKL + 3
      NABD = MID(I)+JD
      DENSTY = ABDENS(NABD)
      IF(.NOT.MFIRST) GO TO 500
      GO TO (300,310,320,330,340,350,360,370,380,390),I
  300 X =            GF(NF+ 1)
      Y =            GF(NF+ 2)
      Z =            GF(NF+ 3)
      GO TO 400
  310 X =            GF(NF+ 4)+GF(NF   )
      Y =            GF(NF+ 7)
      Z =            GF(NF+ 8)
      GO TO 400
  320 X =            GF(NF+ 7)
      Y =            GF(NF+ 5)+GF(NF   )
      Z =            GF(NF+ 9)
      GO TO 400
  330 X =            GF(NF+ 8)
      Y =            GF(NF+ 9)
      Z =            GF(NF+ 6)+GF(NF   )
      GO TO 400
  340 X =            GF(NF+ 3)+GF(NF   )+GF(NF   )
      Y =            GF(NF+ 6)
      Z =            GF(NF+ 7)
      GO TO 400
  350 X =            GF(NF+ 8)
      Y =            GF(NF+ 4)+GF(NF+ 1)+GF(NF+ 1)
      Z =            GF(NF+ 9)
      GO TO 400
  360 X =            GF(NF+10)
      Y =            GF(NF+11)
      Z =            GF(NF+ 5)+GF(NF+ 2)+GF(NF+ 2)
      GO TO 400
  370 X =            GF(NF+ 6)+GF(NF+ 1)
      Y =            GF(NF+ 8)+GF(NF   )
      Z =            GF(NF+12)
      GO TO 395
  380 X =            GF(NF+ 7)+GF(NF+ 2)
      Y =            GF(NF+12)
      Z =            GF(NF+10)+GF(NF   )
      GO TO 395
  390 X =            GF(NF+12)
      Y =            GF(NF+ 9)+GF(NF+ 2)
      Z =            GF(NF+11)+GF(NF+ 1)
  395 IF(NONORM) GO TO 400
      X =            X*SQRT3
      Y =            Y*SQRT3
      Z =            Z*SQRT3
  400 XMD =          X
      YMD =          Y
      ZMD =          Z
      IF(IANDJ .AND. I   .EQ. J  ) GO TO 410
      IF(SAME  .AND. NIJ .EQ. NKL) GO TO 410
      GO TO 420
  410 XMD = XMD + XMD
      YMD = YMD + YMD
      ZMD = ZMD + ZMD
  420 DX  = DX  + XMD*DENSTY
      DY  = DY  + YMD*DENSTY
      DZ  = DZ  + ZMD*DENSTY
      GC(NIJKL+1) = XMD
      GC(NIJKL+2) = YMD
      GC(NIJKL+3) = ZMD
  500 IF(.NOT.MSECND) GO TO 3900
      GO TO (600,610,620,630,640,650,660,670,680,690),I
  600 XX =           GS(NS   )+         GS(NS+ 4)
      YY =           GS(NS   )+         GS(NS+ 5)
      ZZ =           GS(NS   )+         GS(NS+ 6)
      YX =           GS(NS+ 7)
      ZX =           GS(NS+ 8)
      ZY =           GS(NS+ 9)
      GO TO 800
  610 XX =  THREE *  GS(NS+ 1)+         GS(NS+10)
      YY =           GS(NS+ 1)+         GS(NS+15)
      ZZ =           GS(NS+ 1)+         GS(NS+17)
      YX =           GS(NS+ 2)+         GS(NS+13)
      ZX =           GS(NS+ 3)+         GS(NS+14)
      ZY =           GS(NS+19)
      GO TO 800
  620 XX =           GS(NS+ 2)+         GS(NS+13)
      YY =  THREE *  GS(NS+ 2)+         GS(NS+11)
      ZZ =           GS(NS+ 2)+         GS(NS+18)
      YX =           GS(NS+ 1)+         GS(NS+15)
      ZX =           GS(NS+19)
      ZY =           GS(NS+ 3)+         GS(NS+16)
      GO TO 800
  630 XX =           GS(NS+ 3)+         GS(NS+14)
      YY =           GS(NS+ 3)+         GS(NS+16)
      ZZ =  THREE *  GS(NS+ 3)+         GS(NS+12)
      YX =           GS(NS+19)
      ZX =           GS(NS+ 1)+         GS(NS+17)
      ZY =           GS(NS+ 2)+         GS(NS+18)
      GO TO 800
  640 XX =  TWO   *  GS(NS   )+  FIVE * GS(NS+ 1)+GS(NS+ 7)
      YY =           GS(NS+ 1)+         GS(NS+16)
      ZZ =           GS(NS+ 1)+         GS(NS+17)
      YX =  TWO   *  GS(NS+ 4)+         GS(NS+10)
      ZX =  TWO   *  GS(NS+ 5)+         GS(NS+11)
      ZY =           GS(NS+19)
      GO TO 800
  650 XX =           GS(NS+ 2)+         GS(NS+16)
      YY =  TWO   *  GS(NS   )+  FIVE * GS(NS+ 2)+GS(NS+ 8)
      ZZ =           GS(NS+ 2)+         GS(NS+18)
      YX =  TWO   *  GS(NS+ 4)+         GS(NS+12)
      ZX =           GS(NS+20)
      ZY =  TWO   *  GS(NS+ 6)+         GS(NS+13)
      GO TO 800
  660 XX =           GS(NS+ 3)+         GS(NS+17)
      YY =           GS(NS+ 3)+         GS(NS+18)
      ZZ =  TWO   *  GS(NS   )+  FIVE * GS(NS+ 3)+GS(NS+ 9)
      YX =           GS(NS+21)
      ZX =  TWO   *  GS(NS+ 5)+         GS(NS+14)
      ZY =  TWO   *  GS(NS+ 6)+         GS(NS+15)
      GO TO 800
  670 XX =  THREE *  GS(NS+ 4)+         GS(NS+10)
      YY =  THREE *  GS(NS+ 4)+         GS(NS+12)
      ZZ =           GS(NS+ 4)+         GS(NS+21)
      YX =           GS(NS   )+         GS(NS+ 1)+GS(NS+ 2)+GS(NS+16)
      ZX =           GS(NS+ 6)+         GS(NS+19)
      ZY =           GS(NS+ 5)+         GS(NS+20)
      GO TO 695
  680 XX =  THREE *  GS(NS+ 5)+         GS(NS+11)
      YY =           GS(NS+ 5)+         GS(NS+20)
      ZZ =  THREE *  GS(NS+ 5)+         GS(NS+14)
      YX =           GS(NS+ 6)+         GS(NS+19)
      ZX =           GS(NS   )+         GS(NS+ 1)+GS(NS+ 3)+GS(NS+17)
      ZY =           GS(NS+ 4)+         GS(NS+21)
      GO TO 695
  690 XX =           GS(NS+ 6)+         GS(NS+19)
      YY =  THREE *  GS(NS+ 6)+         GS(NS+13)
      ZZ =  THREE *  GS(NS+ 6)+         GS(NS+15)
      YX =           GS(NS+ 5)+         GS(NS+20)
      ZX =           GS(NS+ 4)+         GS(NS+21)
      ZY =           GS(NS   )+         GS(NS+ 2)+GS(NS+ 3)+GS(NS+18)
  695 IF(NONORM) GO TO 800
      XX =           XX*SQRT3
      YY =           YY*SQRT3
      ZZ =           ZZ*SQRT3
      YX =           YX*SQRT3
      ZX =           ZX*SQRT3
      ZY =           ZY*SQRT3
  800 IF(IANDJ .AND. I   .EQ. J   ) GO TO 810
      IF(SAME  .AND. NIJ .EQ. NKL ) GO TO 810
      DXX = DXX   +       XX*DENSTY
      DYY = DYY   +       YY*DENSTY
      DZZ = DZZ   +       ZZ*DENSTY
      DYX = DYX   +       YX*DENSTY
      DZX = DZX   +       ZX*DENSTY
      DZY = DZY   +       ZY*DENSTY
      GO TO 3900
  810 DXX = DXX   +  (XX+XX)*DENSTY
      DYY = DYY   +  (YY+YY)*DENSTY
      DZZ = DZZ   +  (ZZ+ZZ)*DENSTY
      DYX = DYX   +  (YX+YX)*DENSTY
      DZX = DZX   +  (ZX+ZX)*DENSTY
      DZY = DZY   +  (ZY+ZY)*DENSTY
 3900 CONTINUE
 4000 CONTINUE
      NF = NF + 13
 4100 NS = NS + 22
 4200 CONTINUE
      DF(1) = DX
      DF(2) = DY
      DF(3) = DZ
      DS(1) = DXX
      DS(5) = DYY
      DS(9) = DZZ
      DS(2) = DYX
      DS(4) = DYX
      DS(3) = DZX
      DS(7) = DZX
      DS(6) = DZY
      DS(8) = DZY
      RETURN
      END
C*MODULE HSS2B   *DECK JCPEND
      SUBROUTINE JCPEND(DHE,DE,DLAG,FCD,HCD,GD,FVD,VEC,DMMO,IA,DFMO,
     *                  WRK,L1,L2,NAT,NCOR,NACT,NN2,NFT18)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION DHE(*),DE(*),DLAG(L1,L1,3*NAT),FCD(L2,3*NAT),
     *          HCD(3*NAT),GD(NN2,3*NAT),FVD(L2,3*NAT),VEC(L1,L1),
     *          DMMO(*),WRK(L1*L1),DFMO(L2)
      DIMENSION IA(L1)
C
      PARAMETER (MXATM=500)
C
      COMMON /FUNCT / EHF,EG(3*MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /RESTAR/ TIMLIM,NREST,NREC,INTLOC,IST,JST,KST,LST
C
      PARAMETER (ZERO=0.0D+00,TWO=2.0D+00)
C
C     ----- THE END OF INTEGRAL CONTRIBUTIONS TO HESSIAN -----
C
      NREST=0
      IST=1
      JST=1
      KST=1
      LST=1
C
C     ----- OUTPUT THE FINISHED GRADIENT VECTOR -----
C
      NEG = 3*NAT
      CALL DCOPY(NEG,DE,1,EG,1)
      CALL DAWRIT(IDAF,IODA,DE,NEG,3,0)
C
C     ----- WRITE GRADIENT + HESSIAN WITH ALL NON-CPHF TERMS ----
C
      NHE = 9*(NAT*NAT+NAT)/2
      NDHE=NEG+NHE
      CALL DAWRIT(IDAF,IODA,DHE,NDHE,67,0)
C
      L3 = L1*L1
      NXYZ = 3*NAT
      NOCC = NCOR+NACT
      CALL SEQREW(NFT18)
C
      DO 1000 IXYZ=1,NXYZ
C
C     COMPLETE TRANSFORMATION OF VALENCE PORTION OF DERIVATIVE
C     LAGRANGIAN
C
       CALL DCOPY(L3,DLAG(1,1,IXYZ),1,WRK,1)
       CALL VCLR(DLAG(1,1,IXYZ),1,L3) 
C
       DO 50 J=1,NACT
         DO 50 I=1,L1
           DVAL = ZERO
           DO 40 MU=1,L1
             MUJ = (J-1)*L1 + MU
             DVAL = DVAL + VEC(MU,I)*WRK(MUJ)
   40      CONTINUE
         DLAG(I,J+NCOR,IXYZ) = DVAL
   50  CONTINUE
C
C     TRANSFORM DERIVATIVE CORE FOCK MATRIX INTO THE MO BASIS
C     AND WRITE TO FILE ALONG WITH DERIVATIVE ERIS
C
        DO 100 I=1,L1
          II = IA(I) + I
          FCD(II,IXYZ) = TWO*FCD(II,IXYZ)
          FVD(II,IXYZ) = TWO*FVD(II,IXYZ)
  100   CONTINUE
C
        CALL TFTRI(DFMO,FCD(1,IXYZ),VEC,WRK,L1,L1,L1) 
        CALL SQWRIT(NFT18,DFMO,L2)
        CALL SQWRIT(NFT18,GD(1,IXYZ),NN2)
C
C     ADD DERIVATIVE CORE FOCK MATRIX CONTRIBUTION TO DERIVATIVE
C     LAGRANGIAN
C
      DO 600 I=1,L1
        DO 300 J=1,NCOR
          II = MAX(I,J)
          JJ = MIN(I,J)
          IJ = IA(II) + JJ
          DLAG(I,J,IXYZ) = DLAG(I,J,IXYZ) + TWO*DFMO(IJ)
  300   CONTINUE 
C
        DO 500 J=NCOR+1,NOCC
          DVAL = ZERO
          DO 400 K=NCOR+1,NOCC
            JJ = MAX(J-NCOR,K-NCOR) 
            KK = MIN(J-NCOR,K-NCOR)
            JK = IA(JJ) + KK
            II = MAX(I,K)
            KK = MIN(I,K)
            IK = IA(II) + KK
            DVAL = DVAL + DFMO(IK)*DMMO(JK)
  400     CONTINUE
          DLAG(I,J,IXYZ) = DLAG(I,J,IXYZ) + DVAL
C
  500   CONTINUE
  600 CONTINUE 
C
C     ADD DERIVATIVE VALENCE FOCK MATRIX CONTRIBUTION TO DERIVATIVE
C     LAGRANGIAN 
C
        CALL TFTRI(DFMO,FVD(1,IXYZ),VEC,WRK,L1,L1,L1)
C
        DO 700 I=1,L1
          DO 700 J=1,NCOR
            II = MAX(I,J)
            JJ = MIN(I,J)
            IJ = IA(II) + JJ
            DLAG(I,J,IXYZ) = DLAG(I,J,IXYZ) + DFMO(IJ)
  700   CONTINUE
C
C     WRITE DERIVATIVE LAGRANGIAN TO FILE 
C
        CALL SQWRIT(NFT18,DLAG(1,1,IXYZ),L3)
 1000 CONTINUE
C
      CALL DAWRIT(IDAF,IODA,HCD,NXYZ,259,0)
      RETURN
      END
C*MODULE HSS2B   *DECK JDDEGH
      SUBROUTINE JDDEGH(EG,EH,ISH,JSH,KSH,LSH,OUT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL OUT
C
      DIMENSION EG(3,*),EH(9,*)
C
      PARAMETER (MXATM=500)
C
      COMMON /ATMJDD/ NATOM(4),NPASS
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /INTJDD/ FDRV(3,4),SDRV(9,4,4),ABDENS(1296)
C
      NPASS1 = NPASS + 1
      DO 10 I = 1,NPASS1
      IATOM = NATOM(I)
      DO 11 K = 1,3
   11 EG(K,IATOM) = EG(K,IATOM) + FDRV(K,I)
      DO 10 J = 1,I
      JATOM = NATOM(J)
      IF( JATOM .GT. IATOM) GO TO 300
      N = IATOM*(IATOM-1)/2 + JATOM
      EH(1,    N) = EH(1,    N) + SDRV(1,J,I)
      EH(2,    N) = EH(2,    N) + SDRV(2,J,I)
      EH(3,    N) = EH(3,    N) + SDRV(3,J,I)
      EH(4,    N) = EH(4,    N) + SDRV(4,J,I)
      EH(5,    N) = EH(5,    N) + SDRV(5,J,I)
      EH(6,    N) = EH(6,    N) + SDRV(6,J,I)
      EH(7,    N) = EH(7,    N) + SDRV(7,J,I)
      EH(8,    N) = EH(8,    N) + SDRV(8,J,I)
      EH(9,    N) = EH(9,    N) + SDRV(9,J,I)
      GO TO 10
  300 N = JATOM*(JATOM-1)/2 + IATOM
      EH(1,    N) = EH(1,    N) + SDRV(1,J,I)
      EH(2,    N) = EH(2,    N) + SDRV(4,J,I)
      EH(3,    N) = EH(3,    N) + SDRV(7,J,I)
      EH(4,    N) = EH(4,    N) + SDRV(2,J,I)
      EH(5,    N) = EH(5,    N) + SDRV(5,J,I)
      EH(6,    N) = EH(6,    N) + SDRV(8,J,I)
      EH(7,    N) = EH(7,    N) + SDRV(3,J,I)
      EH(8,    N) = EH(8,    N) + SDRV(6,J,I)
      EH(9,    N) = EH(9,    N) + SDRV(9,J,I)
   10 CONTINUE
      IF(OUT) CALL JDDPRT(ISH,JSH,KSH,LSH)
      IF(OUT) CALL HSSPRT(NAT,EG,EH)
      RETURN
      END
C*MODULE HSS2B   *DECK JDDESH
      SUBROUTINE JDDESH(II,DHE,DE,DF,NAT,NUM,
     *                  MCPHF,UHFTYP,ROGVB,NFT18)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION MCSCF
C
      LOGICAL MCPHF,UHFTYP,ROGVB,GOPARR,DSKWRK,MASWRK,MCCI
C
      DIMENSION DE(*),DHE(*),DF(*)
C
      PARAMETER (MXGTOT=5000, MXSH=1000)
C
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RESTAR/ TIMLIM,NREST,NREC,INTLOC,IST,JST,KST,LST
      COMMON /TMVALS/ TI,TX,TIM
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA MCSCF /8HMCSCF   / 
#else
      CHARACTER*8 :: MCSCF_STR
      EQUIVALENCE (MCSCF, MCSCF_STR)
      DATA MCSCF_STR/"MCSCF   "/ 
#endif
C
      MCCI = SCFTYP.EQ.MCSCF
C
C     ----- END OF SHELL LOOP, SET UP POSSIBLE RESTART -----
C
      NREST=4
      ISTNU=1+II
C
C     ----- SAVE GRADIENT + RESTART DATA -----
C
      NEG = 3*NAT
      CALL DAWRIT(IDAF,IODA,DE,NEG,3,0)
      NHE = 9*(NAT*(NAT+1))/2
      NDHE=NEG+NHE
      CALL DAWRIT(IDAF,IODA,DHE,NDHE,67,0)
C
      IF(MCCI) GO TO 1000
      IF(MCPHF  .AND.  .NOT.ROGVB) THEN
         CALL SEQREW(NFT18)
         NDF=(NUM*(NUM+1))/2
         MDF=3*NAT
         IF(UHFTYP) MDF=6*NAT
         IDF=1
         DO 100 I=1,MDF
            CALL SQWRIT(NFT18,DF(IDF),NDF)
            IDF=IDF+NDF
  100    CONTINUE
         CALL SEQREW(NFT18)
      END IF
C
C     ----- CHECK CPU TIME -----
C
 1000 CONTINUE
      IF(ISTNU.GT.NSHELL) RETURN
      CALL TSECND(TIM)
      IF(TIM.LT.TIMLIM) RETURN
      IF (MASWRK) WRITE(IW,9998)
      CALL TEXIT(1,NREST)
      RETURN
C
 9998 FORMAT(' ... WARNING ... THIS JOB MUST BE RESTARTED ... ')
      END
C*MODULE HSS2B   *DECK JDDEND
      SUBROUTINE JDDEND(DHE,DE,HE,DF,V,EPSA,DFMO,WRK,SHLJ,SHLK,IA,
     *                  DHAM,MCPHF,NFT18,I36,NAT,NHAM,
     *                  L1,L2,L3,RHFTYP,UHFTYP,ROGVB,OUT,GCI)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL MCPHF,RHFTYP,UHFTYP,ROGVB,OUT,GCI
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      DIMENSION DHE(*),DE(*),HE(*),V(L3),EPSA(L3),DFMO(L2),WRK(L1),
     *          DF(L2,I36,NAT),DHAM(3,3,NAT),IA(L1),
     *          SHLJ(L2,NHAM,3,NAT),SHLK(L2,NHAM,3,NAT)
C
      PARAMETER (MXATM=500)
C
      COMMON /FUNCT / EHF,EG(3*MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RESTAR/ TIMLIM,NREST,NREC,INTLOC,IST,JST,KST,LST
C
C     ----- THE END OF INTEGRAL CONTRIBUTIONS TO HESSIAN -----
C
      NREST=0
      IST=1
      JST=1
      KST=1
      LST=1
C
C     ----- OUTPUT THE FINISHED GRADIENT VECTOR -----
C
      NEG = 3*NAT
      CALL DCOPY(NEG,DE,1,EG,1)
      CALL DAWRIT(IDAF,IODA,DE,NEG,3,0)
C
C     ----- WRITE GRADIENT + HESSIAN WITH ALL NON-CPHF TERMS ----
C
      NHE = 9*(NAT*NAT+NAT)/2
      NDHE=NEG+NHE
      CALL DAWRIT(IDAF,IODA,DHE,NDHE,67,0)
C
      IF(OUT) THEN
         IF(MASWRK) WRITE(IW,9996)
         CALL HSSPRT(NAT,DE,HE)
      END IF
C
C     ----- WRITE EACH ATOM'S DERIVATIVE FOCK MATRICES TO DISK -----
C     EACH IS TRANSFORMED TO THE MO BASIS FIRST.  IN GENERAL, THIS
C     MATRIX IS NOT SYMMETRIC, ALTHOUGH FOR RHF OR UHF IT IS.
C
      IF(MCPHF) THEN
         IF(OUT) WRITE(IW,*) '--DERIVATIVE FOCK MATRICES--'
         CALL SEQREW(NFT18)
         DO 510 IAT=1,NAT
            DO 500 IXYZ=1,3
C
               IF(RHFTYP.OR.GCI) THEN
                  IF(OUT) WRITE(IW,9997) 'AO',IAT,IXYZ
                  IF(OUT) CALL PRTRI(DF(1,IXYZ,IAT),L1)
                  CALL TFTRI(DFMO,DF(1,IXYZ,IAT),V,WRK,L1,L1,L1)
                  CALL SQWRIT(NFT18,DFMO,L2)
               END IF
C
               IF(UHFTYP) THEN
                  IF(OUT) WRITE(IW,9997) 'AO',IAT,IXYZ
                  IF(OUT) CALL PRTRI(DF(1,IXYZ,IAT),L1)
                  CALL TFTRI(DFMO,DF(1,IXYZ,IAT),V,WRK,L1,L1,L1)
                  CALL SQWRIT(NFT18,DFMO,L2)
C
                  IF(OUT) WRITE(IW,9997) 'AO',IAT,IXYZ
                  IF(OUT) CALL PRTRI(DF(1,IXYZ+3,IAT),L1)
                  CALL TFTRI(DFMO,DF(1,IXYZ+3,IAT),V,WRK,L1,L1,L1)
                  CALL SQWRIT(NFT18,DFMO,L2)
               END IF
C
C         THE MCSCF CODE IS CORRECT ONLY FOR THE 1 CSF CASE!
C
C              IF(MCCI) THEN
C                 IF(OUT) WRITE(IW,9997) 'AO',IAT,IXYZ
C                 IF(OUT) CALL PRTRI(DF(1,IXYZ,IAT),L1)
C                 CALL TFTRI(DFMO,DF(1,IXYZ,IAT),V,WRK,L1,L1,L1)
C                 CALL SQWRIT(NFT18,DFMO,L2)
C              END IF
C
  500       CONTINUE
  510    CONTINUE
C
         IF(ROGVB) THEN
            CALL JDDODF(EPSA,V,DF,DFMO,WRK,SHLJ,SHLK,DHAM,IA,
     *                  NAT,NHAM,L1,L2,NFT18,OUT)
         END IF
C
         CALL SEQREW(NFT18)
      END IF
      RETURN
C
 9997 FORMAT(1X,'-DF- IN ',A2,' BASIS FOR ATOM',I4,'  COORD',I2)
 9996 FORMAT(/5X,33("-")/5X,'TWO ELECTRON GRADIENT AND HESSIAN'/
     *        5X,33(1H-)/)
      END
C*MODULE HSS2B   *DECK JDDFCK
      SUBROUTINE JDDFCK(Q4,DF,DA,DB,DSHL,SHLJ,SHLK,GIJKL1,GIJKL2,
     *                  GIJKL3,GIJKL4,GIJKL7,GIJKL8,GIJKL9,L1,L2,
     *                  NHAM,NXYZ,RHFTYP,UHFTYP,ROGVB,MCCI,OUT,GCI)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL RHFTYP,UHFTYP,ROGVB,MCCI,OUT,GCI
      LOGICAL SHLON,SKIP
      LOGICAL IANDJ,KANDL,SAME
      LOGICAL NSKIP1,NSKIP2,NSKIP3,NSKIP4,NSKIP5,NSKIP6,
     *        NSKIP7,NSKIP8,NSKIP9
C
      PARAMETER (NIJKLD=3888)
C
      DIMENSION DF(L2,NXYZ),DA(L2),DB(L2),
     *          DSHL(L2,NHAM),SHLJ(L2,NHAM,NXYZ),SHLK(L2,NHAM,NXYZ),
     *          GIJKL1(NIJKLD),GIJKL2(NIJKLD),GIJKL3(NIJKLD),
     *          GIJKL4(NIJKLD),
     *          GIJKL7(NIJKLD),GIJKL8(NIJKLD),GIJKL9(NIJKLD)
      DIMENSION MIJ(6,6),MKL(6,6),MIJKL(36),MAD(4)
C
      PARAMETER (MXGTOT=5000, MXSH=1000, MXAO=2047)
C
      COMMON /ATMJDD/ NATOM(4),NPASS
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /JDDSKP/ MSHL(4),MGETDG(3),SHLON(6),SKIP(4)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /SHLJDD/ LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NNIJ,IJD,KLD,IJ,KL
      COMMON /SKPJDD/ NSKIP1,NSKIP2,NSKIP3,NSKIP4,NSKIP5,NSKIP6,
     *                NSKIP7,NSKIP8,NSKIP9
C
      PARAMETER (HALF=0.5D+00)
C
C     ----- FORM DERIVATIVE FOCK MATRICES -----
C
      I36=3
      IF(UHFTYP) I36=6
C
C     ----- TRANSFER OF FIRST DERIVATIVE TEI TO CANONICAL ORDER -----
C
      CALL VCLR(GIJKL1,1,NIJKLD)
      CALL VCLR(GIJKL2,1,NIJKLD)
      CALL VCLR(GIJKL3,1,NIJKLD)
      CALL VCLR(GIJKL4,1,NIJKLD)
C
      NPASS1 = NPASS + 1
      DO 110 I = 1,4
         IATOM = KATOM(MSHL(I))
         DO 100 J = 1,NPASS1
            IF(IATOM .EQ. NATOM(J)) MAD(I) = J
  100    CONTINUE
  110 CONTINUE
C
      ISH = MSHL(1)
      JSH = MSHL(2)
      KSH = MSHL(3)
      LSH = MSHL(4)
      CALL JDFIDX(ISH,JSH,KSH,LSH,MIJ,MKL,MIJKL,NIJKL)
C
      IF(NSKIP1) GO TO 300
      IGO = MAD(1)
      GO TO (210,220,230,300),IGO
  210 CALL JDFCK1(ISH,JSH,KSH,LSH,GIJKL1,GIJKL7,MIJ,MKL,MIJKL,NIJKLD)
      GO TO 300
  220 CALL JDFCK1(ISH,JSH,KSH,LSH,GIJKL2,GIJKL7,MIJ,MKL,MIJKL,NIJKLD)
      GO TO 300
  230 CALL JDFCK1(ISH,JSH,KSH,LSH,GIJKL3,GIJKL7,MIJ,MKL,MIJKL,NIJKLD)
C
  300 CONTINUE
      IF(NSKIP2) GO TO 400
      IGO = MAD(2)
      GO TO (310,320,330,400),IGO
  310 CALL JDFCK2(JSH,ISH,KSH,LSH,GIJKL1,GIJKL8,MIJ,MKL,MIJKL,NIJKLD)
      GO TO 400
  320 CALL JDFCK2(JSH,ISH,KSH,LSH,GIJKL2,GIJKL8,MIJ,MKL,MIJKL,NIJKLD)
      GO TO 400
  330 CALL JDFCK2(JSH,ISH,KSH,LSH,GIJKL3,GIJKL8,MIJ,MKL,MIJKL,NIJKLD)
C
  400 CONTINUE
      IF(NSKIP3) GO TO 500
      IGO = MAD(3)
      GO TO (410,420,430,500),IGO
  410 CALL JDFCK3(KSH,LSH,ISH,JSH,GIJKL1,GIJKL9,MIJ,MKL,MIJKL,NIJKLD)
      GO TO 500
  420 CALL JDFCK3(KSH,LSH,ISH,JSH,GIJKL2,GIJKL9,MIJ,MKL,MIJKL,NIJKLD)
      GO TO 500
  430 CALL JDFCK3(KSH,LSH,ISH,JSH,GIJKL3,GIJKL9,MIJ,MKL,MIJKL,NIJKLD)
C
C     ----- CREATE REDUNDANT FROM GIJKL1 GIJKL2 AND GIJKL3 -----
C
  500 CONTINUE
      NINT = 3 * NIJKL
      GO TO (510,520,530),NPASS
  510 CONTINUE
      DO 515 I = 1,NINT
         GIJKL2(I) = - GIJKL1(I)
  515 CONTINUE
      GO TO 600
  520 CONTINUE
      DO 525 I = 1,NINT
         GIJKL3(I) = - GIJKL1(I) - GIJKL2(I)
  525 CONTINUE
      GO TO 600
  530 CONTINUE
      DO 535 I = 1,NINT
         GIJKL4(I) = - GIJKL1(I) - GIJKL2(I) - GIJKL3(I)
  535 CONTINUE
C
C     ----- DF MATRICES : DF = OEI/DX + DENSTY * (TEI/DX) -----
C
  600 CONTINUE
      IANDJ =   ISH .EQ. JSH
      KANDL =   KSH .EQ. LSH
      SAME =.FALSE.
      NKL = 0
      NIJKL = 0
C
C         LOOPS OVER BASIS FUNCTIONS
C
      DO 4300 L = MINL,MAXL
      LNO = LOCL + L
      IF(KANDL) MAXK = L
      DO 4200 K = MINK,MAXK
      KNO = LOCK + K
      NKL = NKL  + 1
      NIJ = 0
      DO 4100 J = MINJ,MAXJ
      JNO = LOCJ + J
      IF(IANDJ) MAXI = J
      DO 4000 I = MINI,MAXI
      INO = LOCI + I
      NIJ = NIJ + 1
C
      IF(.NOT. SAME) GO TO 700
      IF(NKL .LT. NIJ) GO TO 4000
  700 NIJKL = NIJKL + 1
      LIJKL = 3 * (NIJKL-1)
      IF(INO .GE. JNO) GO TO 710
      LIJ = IA(JNO) + INO
      GO TO 715
  710 LIJ = IA(INO) + JNO
  715 IF(INO .GE. KNO) GO TO 720
      LIK = IA(KNO) + INO
      GO TO 725
  720 LIK = IA(INO) + KNO
  725 IF(INO .GE. LNO) GO TO 730
      LIL = IA(LNO) + INO
      GO TO 735
  730 LIL = IA(INO) + LNO
  735 IF(JNO .GE. KNO) GO TO 740
      LJK = IA(KNO) + JNO
      GO TO 745
  740 LJK = IA(JNO) + KNO
  745 IF(JNO .GE. LNO) GO TO 750
      LJL = IA(LNO) + JNO
      GO TO 755
  750 LJL = IA(JNO) + LNO
  755 IF(KNO .GE. LNO) GO TO 760
      LKL = IA(LNO) + KNO
      GO TO 800
  760 LKL = IA(KNO) + LNO
C
  800 CONTINUE
      DO 2000 IPASS = 1,NPASS1
      GO TO (810,820,830,840),IPASS
  810 VALX  = Q4 * GIJKL1(LIJKL+1)
      VALY  = Q4 * GIJKL1(LIJKL+2)
      VALZ  = Q4 * GIJKL1(LIJKL+3)
      GO TO 850
  820 VALX  = Q4 * GIJKL2(LIJKL+1)
      VALY  = Q4 * GIJKL2(LIJKL+2)
      VALZ  = Q4 * GIJKL2(LIJKL+3)
      GO TO 850
  830 VALX  = Q4 * GIJKL3(LIJKL+1)
      VALY  = Q4 * GIJKL3(LIJKL+2)
      VALZ  = Q4 * GIJKL3(LIJKL+3)
      GO TO 850
  840 VALX  = Q4 * GIJKL4(LIJKL+1)
      VALY  = Q4 * GIJKL4(LIJKL+2)
      VALZ  = Q4 * GIJKL4(LIJKL+3)
C
  850 IF(INO .NE. JNO) GO TO 860
      VALX = VALX * HALF
      VALY = VALY * HALF
      VALZ = VALZ * HALF
  860 IF(KNO .NE. LNO) GO TO 870
      VALX = VALX * HALF
      VALY = VALY * HALF
      VALZ = VALZ * HALF
  870 IF(LIJ .NE. LKL) GO TO 880
      VALX = VALX * HALF
      VALY = VALY * HALF
      VALZ = VALZ * HALF
C
  880 CONTINUE
      VALX2  = VALX + VALX
      VALY2  = VALY + VALY
      VALZ2  = VALZ + VALZ
      VALX4  = VALX2 + VALX2
      VALY4  = VALY2 + VALY2
      VALZ4  = VALZ2 + VALZ2
C
      IATOM = NATOM(IPASS)
      IXA   = I36*(IATOM-1) + 1
      IYA   = IXA + 1
      IZA   = IYA + 1
C
      IF(RHFTYP.OR.GCI) THEN
         DF(LIJ,IXA) = DF(LIJ,IXA) + VALX4 * DA(LKL)
         DF(LKL,IXA) = DF(LKL,IXA) + VALX4 * DA(LIJ)
         DF(LIK,IXA) = DF(LIK,IXA) - VALX  * DA(LJL)
         DF(LIL,IXA) = DF(LIL,IXA) - VALX  * DA(LJK)
         DF(LJK,IXA) = DF(LJK,IXA) - VALX  * DA(LIL)
         DF(LJL,IXA) = DF(LJL,IXA) - VALX  * DA(LIK)
C
         DF(LIJ,IYA) = DF(LIJ,IYA) + VALY4 * DA(LKL)
         DF(LKL,IYA) = DF(LKL,IYA) + VALY4 * DA(LIJ)
         DF(LIK,IYA) = DF(LIK,IYA) - VALY  * DA(LJL)
         DF(LIL,IYA) = DF(LIL,IYA) - VALY  * DA(LJK)
         DF(LJK,IYA) = DF(LJK,IYA) - VALY  * DA(LIL)
         DF(LJL,IYA) = DF(LJL,IYA) - VALY  * DA(LIK)
C
         DF(LIJ,IZA) = DF(LIJ,IZA) + VALZ4 * DA(LKL)
         DF(LKL,IZA) = DF(LKL,IZA) + VALZ4 * DA(LIJ)
         DF(LIK,IZA) = DF(LIK,IZA) - VALZ  * DA(LJL)
         DF(LIL,IZA) = DF(LIL,IZA) - VALZ  * DA(LJK)
         DF(LJK,IZA) = DF(LJK,IZA) - VALZ  * DA(LIL)
         DF(LJL,IZA) = DF(LJL,IZA) - VALZ  * DA(LIK)
      END IF
C
      IF(UHFTYP) THEN
         IXB   = IZA + 1
         IYB   = IXB + 1
         IZB   = IYB + 1
         DUMKL = DA(LKL)+DB(LKL)
         DUMIJ = DA(LIJ)+DB(LIJ)
         DF(LIJ,IXA) = DF(LIJ,IXA) + VALX4 * DUMKL
         DF(LKL,IXA) = DF(LKL,IXA) + VALX4 * DUMIJ
         DF(LIK,IXA) = DF(LIK,IXA) - VALX2 * DA(LJL)
         DF(LIL,IXA) = DF(LIL,IXA) - VALX2 * DA(LJK)
         DF(LJK,IXA) = DF(LJK,IXA) - VALX2 * DA(LIL)
         DF(LJL,IXA) = DF(LJL,IXA) - VALX2 * DA(LIK)
C
         DF(LIJ,IXB) = DF(LIJ,IXB) + VALX4 * DUMKL
         DF(LKL,IXB) = DF(LKL,IXB) + VALX4 * DUMIJ
         DF(LIK,IXB) = DF(LIK,IXB) - VALX2 * DB(LJL)
         DF(LIL,IXB) = DF(LIL,IXB) - VALX2 * DB(LJK)
         DF(LJK,IXB) = DF(LJK,IXB) - VALX2 * DB(LIL)
         DF(LJL,IXB) = DF(LJL,IXB) - VALX2 * DB(LIK)
C
         DF(LIJ,IYA) = DF(LIJ,IYA) + VALY4 * DUMKL
         DF(LKL,IYA) = DF(LKL,IYA) + VALY4 * DUMIJ
         DF(LIK,IYA) = DF(LIK,IYA) - VALY2 * DA(LJL)
         DF(LIL,IYA) = DF(LIL,IYA) - VALY2 * DA(LJK)
         DF(LJK,IYA) = DF(LJK,IYA) - VALY2 * DA(LIL)
         DF(LJL,IYA) = DF(LJL,IYA) - VALY2 * DA(LIK)
C
         DF(LIJ,IYB) = DF(LIJ,IYB) + VALY4 * DUMKL
         DF(LKL,IYB) = DF(LKL,IYB) + VALY4 * DUMIJ
         DF(LIK,IYB) = DF(LIK,IYB) - VALY2 * DB(LJL)
         DF(LIL,IYB) = DF(LIL,IYB) - VALY2 * DB(LJK)
         DF(LJK,IYB) = DF(LJK,IYB) - VALY2 * DB(LIL)
         DF(LJL,IYB) = DF(LJL,IYB) - VALY2 * DB(LIK)
C
         DF(LIJ,IZA) = DF(LIJ,IZA) + VALZ4 * DUMKL
         DF(LKL,IZA) = DF(LKL,IZA) + VALZ4 * DUMIJ
         DF(LIK,IZA) = DF(LIK,IZA) - VALZ2 * DA(LJL)
         DF(LIL,IZA) = DF(LIL,IZA) - VALZ2 * DA(LJK)
         DF(LJK,IZA) = DF(LJK,IZA) - VALZ2 * DA(LIL)
         DF(LJL,IXA) = DF(LJL,IZA) - VALZ2 * DA(LIK)
C
         DF(LIJ,IZB) = DF(LIJ,IZB) + VALZ4 * DUMKL
         DF(LKL,IZB) = DF(LKL,IZB) + VALZ4 * DUMIJ
         DF(LIK,IZB) = DF(LIK,IZB) - VALZ2 * DB(LJL)
         DF(LIL,IZB) = DF(LIL,IZB) - VALZ2 * DB(LJK)
         DF(LJK,IZB) = DF(LJK,IZB) - VALZ2 * DB(LIL)
         DF(LJL,IZB) = DF(LJL,IZB) - VALZ2 * DB(LIK)
      END IF
C
C        DERIVATIVE COULOMB/EXCHANGE OPERATORS, WHICH MUST BE
C        COMBINED LATER TO FORM THE FOCK DERIVATIVES.  SEE
C        EQUATIONS 30-35 IN JACS, 105, 7506-7511(1983).
C
      IF(ROGVB) THEN
        DO 1000 IHAM=1,NHAM
C
          SHLJ(LIJ,IHAM,IXA)=SHLJ(LIJ,IHAM,IXA) + VALX4*DSHL(LKL,IHAM)
          SHLJ(LKL,IHAM,IXA)=SHLJ(LKL,IHAM,IXA) + VALX4*DSHL(LIJ,IHAM)
          SHLK(LIK,IHAM,IXA)=SHLK(LIK,IHAM,IXA) + VALX2*DSHL(LJL,IHAM)
          SHLK(LIL,IHAM,IXA)=SHLK(LIL,IHAM,IXA) + VALX2*DSHL(LJK,IHAM)
          SHLK(LJK,IHAM,IXA)=SHLK(LJK,IHAM,IXA) + VALX2*DSHL(LIL,IHAM)
          SHLK(LJL,IHAM,IXA)=SHLK(LJL,IHAM,IXA) + VALX2*DSHL(LIK,IHAM)
C
          SHLJ(LIJ,IHAM,IYA)=SHLJ(LIJ,IHAM,IYA) + VALY4*DSHL(LKL,IHAM)
          SHLJ(LKL,IHAM,IYA)=SHLJ(LKL,IHAM,IYA) + VALY4*DSHL(LIJ,IHAM)
          SHLK(LIK,IHAM,IYA)=SHLK(LIK,IHAM,IYA) + VALY2*DSHL(LJL,IHAM)
          SHLK(LIL,IHAM,IYA)=SHLK(LIL,IHAM,IYA) + VALY2*DSHL(LJK,IHAM)
          SHLK(LJK,IHAM,IYA)=SHLK(LJK,IHAM,IYA) + VALY2*DSHL(LIL,IHAM)
          SHLK(LJL,IHAM,IYA)=SHLK(LJL,IHAM,IYA) + VALY2*DSHL(LIK,IHAM)
C
          SHLJ(LIJ,IHAM,IZA)=SHLJ(LIJ,IHAM,IZA) + VALZ4*DSHL(LKL,IHAM)
          SHLJ(LKL,IHAM,IZA)=SHLJ(LKL,IHAM,IZA) + VALZ4*DSHL(LIJ,IHAM)
          SHLK(LIK,IHAM,IZA)=SHLK(LIK,IHAM,IZA) + VALZ2*DSHL(LJL,IHAM)
          SHLK(LIL,IHAM,IZA)=SHLK(LIL,IHAM,IZA) + VALZ2*DSHL(LJK,IHAM)
          SHLK(LJK,IHAM,IZA)=SHLK(LJK,IHAM,IZA) + VALZ2*DSHL(LIL,IHAM)
          SHLK(LJL,IHAM,IZA)=SHLK(LJL,IHAM,IZA) + VALZ2*DSHL(LIK,IHAM)
 1000   CONTINUE
      END IF
C
C         THE MCSCF CODE IS CORRECT ONLY FOR THE 1 CSF CASE!
C
      IF(MCCI) THEN
         DF(LIJ,IXA) = DF(LIJ,IXA) + VALX4 * DA(LKL)
         DF(LKL,IXA) = DF(LKL,IXA) + VALX4 * DA(LIJ)
         DF(LIK,IXA) = DF(LIK,IXA) - VALX  * DA(LJL)
         DF(LIL,IXA) = DF(LIL,IXA) - VALX  * DA(LJK)
         DF(LJK,IXA) = DF(LJK,IXA) - VALX  * DA(LIL)
         DF(LJL,IXA) = DF(LJL,IXA) - VALX  * DA(LIK)
C
         DF(LIJ,IYA) = DF(LIJ,IYA) + VALY4 * DA(LKL)
         DF(LKL,IYA) = DF(LKL,IYA) + VALY4 * DA(LIJ)
         DF(LIK,IYA) = DF(LIK,IYA) - VALY  * DA(LJL)
         DF(LIL,IYA) = DF(LIL,IYA) - VALY  * DA(LJK)
         DF(LJK,IYA) = DF(LJK,IYA) - VALY  * DA(LIL)
         DF(LJL,IYA) = DF(LJL,IYA) - VALY  * DA(LIK)
C
         DF(LIJ,IZA) = DF(LIJ,IZA) + VALZ4 * DA(LKL)
         DF(LKL,IZA) = DF(LKL,IZA) + VALZ4 * DA(LIJ)
         DF(LIK,IZA) = DF(LIK,IZA) - VALZ  * DA(LJL)
         DF(LIL,IZA) = DF(LIL,IZA) - VALZ  * DA(LJK)
         DF(LJK,IZA) = DF(LJK,IZA) - VALZ  * DA(LIL)
         DF(LJL,IZA) = DF(LJL,IZA) - VALZ  * DA(LIK)
      END IF
C
 2000 CONTINUE
 4000 CONTINUE
 4100 CONTINUE
 4200 CONTINUE
 4300 CONTINUE
C
C     ----- PRINT ONLY ONE DERIVATIVE MATRIX -----
C
      IF(OUT) THEN
         WRITE(IW,*) 'IN JDDFCK, THE FIRST -DF- MATRIX IS'
         CALL PRTRI(DF,L1)
      END IF
      RETURN
      END
C*MODULE HSS2B   *DECK JDDFSD
      SUBROUTINE JDDFSD(GIJKL1,GIJKL2,GIJKL3,GIJKL4,GIJKL5,GIJKL6,
     *                  GIJKL7,GIJKL8,GIJKL9,WX,WY,WZ)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SHLON,SKIP
      LOGICAL FIRST,SECND,CPHF,BOTH,MFIRST,MSECND,MCPHF
C
      DIMENSION GIJKL1(2808),GIJKL2(2808),GIJKL3(2808),
     *          GIJKL4(4752),GIJKL5(4752),GIJKL6(4752),
     *          GIJKL7(6084),GIJKL8(6084),GIJKL9(6084),
     *          WX(2808),WY(2808),WZ(2808)
      DIMENSION MAD(4),FD(3,3),SD(9,3,3),CHECK(9)
C
      PARAMETER (MXGTOT=5000, MXSH=1000)
C
      COMMON /ATMJDD/ NATOM(4),NPASS
      COMMON /HSSPAR/ FIRST,SECND,CPHF,BOTH,MFIRST,MSECND,MCPHF
      COMMON /INTJDD/ FDRV(3,4),SDRV(9,4,4),ABDENS(1296)
      COMMON /JDDSKP/ MSHL(4),MGETDG(3),SHLON(6),SKIP(4)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
C
      PARAMETER (ZERO=0.0D+00)
C
      DATA NID,NJD,NKD,NLD      /   216,   36,  6,  1  /
      DATA NIJKL1,NIJKL4,NIJKL7 /  2808,  4752,  6084  /
      DATA NFIJKL               /  3888                /
C
      NPASS1 = NPASS + 1
      DO 10 I = 1,4
      DO 11 K = 1,3
   11 FDRV(K,I)   = ZERO
      DO 10 J = 1,4
      DO 10 K = 1,9
   10 SDRV(K,J,I) = ZERO
      DO 20 I = 1,3
      DO 21 K = 1,3
   21 FD(K,I)     = ZERO
      DO 20 J = 1,3
      DO 20 K = 1,9
   20 SD(K,J,I)   = ZERO
      DO 25 I = 1,9
   25 CHECK(I) = ZERO
      DO 30 I = 1,4
      IATOM = KATOM(MSHL(I ))
      DO 30 J = 1,NPASS1
      IF(IATOM .EQ. NATOM(J)) MAD(I) = J
   30 CONTINUE
      IF(.NOT.MSECND) GO TO 230
      IF( SHLON(4  )) GO TO 210
      ISH = MSHL(1)
      JSH = MSHL(2)
      KSH = MSHL(3)
      LSH = MSHL(4)
      CALL FIFJKL(ISH,JSH,KSH,LSH,WX,WY,WZ,GIJKL7,SD(1,2,1),
     *            NID,NJD,NKD)
  210 IF( SHLON(5  )) GO TO 220
      ISH = MSHL(1)
      JSH = MSHL(2)
      KSH = MSHL(3)
      LSH = MSHL(4)
      CALL FIJFKL(ISH,JSH,KSH,LSH,WX,WY,WZ,GIJKL8,SD(1,3,1),
     *            NID,NJD,NKD)
  220 IF( SHLON(6  )) GO TO 230
      ISH = MSHL(2)
      JSH = MSHL(1)
      KSH = MSHL(3)
      LSH = MSHL(4)
      CALL FIJFKL(ISH,JSH,KSH,LSH,WX,WY,WZ,GIJKL9,SD(1,3,2),
     *            NJD,NID,NKD)
  230 DO 35 I = 1,NFIJKL
      GIJKL7(I) = ZERO
      GIJKL8(I) = ZERO
      GIJKL9(I) = ZERO
   35 CONTINUE
      IF( SHLON(1  )) GO TO 240
      ISH = MSHL(1)
      JSH = MSHL(2)
      KSH = MSHL(3)
      LSH = MSHL(4)
      CALL FSIJKL(ISH,JSH,KSH,LSH,GIJKL1,GIJKL4,GIJKL7,FD(1,1),
     1            SD(1,1,1),NID,NJD,NKD,NLD,NIJKL1,NIJKL4,NIJKL7)
  240 IF( SHLON(2  )) GO TO 250
      ISH = MSHL(2)
      JSH = MSHL(1)
      KSH = MSHL(3)
      LSH = MSHL(4)
      CALL FSIJKL(ISH,JSH,KSH,LSH,GIJKL2,GIJKL5,GIJKL8,FD(1,2),
     1            SD(1,2,2),NJD,NID,NKD,NLD,NIJKL1,NIJKL4,NIJKL7)
  250 IF( SHLON(3  )) GO TO 300
      ISH = MSHL(3)
      JSH = MSHL(4)
      KSH = MSHL(1)
      LSH = MSHL(2)
      CALL FSIJKL(ISH,JSH,KSH,LSH,GIJKL3,GIJKL6,GIJKL9,FD(1,3),
     1            SD(1,3,3),NKD,NLD,NID,NJD,NIJKL1,NIJKL4,NIJKL7)
  300 DO 40 I = 1,3
      DO 40 J = 1,I
      SD(1,J,I) = SD(1,I,J)
      SD(4,J,I) = SD(2,I,J)
      SD(7,J,I) = SD(3,I,J)
      SD(2,J,I) = SD(4,I,J)
      SD(5,J,I) = SD(5,I,J)
      SD(8,J,I) = SD(6,I,J)
      SD(3,J,I) = SD(7,I,J)
      SD(6,J,I) = SD(8,I,J)
      SD(9,J,I) = SD(9,I,J)
   40 CONTINUE
C
      DO 60 I = 1,3
      IMAD = MAD(I)
      DO 61 K =1,3
   61 FDRV(K,IMAD)      = FDRV(K,IMAD)      + FD(K,I)
      DO 60 J = 1,3
      JMAD = MAD(J)
      DO 60 K =1,9
   60 SDRV(K,JMAD,IMAD) = SDRV(K,JMAD,IMAD) + SD(K,J,I)
      DO 70 I = 1,NPASS
      DO 71 K = 1,3
   71 FDRV(K,NPASS1)    = FDRV(K,NPASS1)    - FDRV(K,I)
      DO 70 J = 1,NPASS
      DO 70 K = 1,9
      SDRV(K,NPASS1,I)  = SDRV(K,NPASS1,I)  - SDRV(K,J,I)
   70 SDRV(K,J,NPASS1)  = SDRV(K,J,NPASS1)  - SDRV(K,J,I)
      DO 80 I = 1,NPASS
      DO 80 K = 1,9
      CHECK(K)              = CHECK(K)              - SDRV(K,NPASS1,I)
   80 SDRV(K,NPASS1,NPASS1) = SDRV(K,NPASS1,NPASS1) - SDRV(K,I,NPASS1)
      RETURN
      END
C*MODULE HSS2B   *DECK JDDLAG
      SUBROUTINE JDDLAG(Q4,GIJKL1,GIJKL2,GIJKL3,GIJKL4,GIJKL7,
     *                  GIJKL8,GIJKL9,V,DLAG,FCD,FVD,GD,DENC,DENV,
     *                  TQDM,NUM,L2,NCOR,NACT,NN2,NXYZ,MAXSHL)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SHLON,SKIP,IANDJ,KANDL,SAME
      LOGICAL NSKIP1,NSKIP2,NSKIP3,NSKIP4,NSKIP5,
     *        NSKIP6,NSKIP7,NSKIP8,NSKIP9
C
      PARAMETER (NIJKLD=3888)
C
      DIMENSION GIJKL1(NIJKLD),GIJKL2(NIJKLD),GIJKL3(NIJKLD),
     *          GIJKL4(NIJKLD),GIJKL7(NIJKLD),GIJKL8(NIJKLD),
     *          GIJKL9(NIJKLD)
      DIMENSION MIJ(6,6),MKL(6,6),MIJKL(36),MAD(4)
      DIMENSION V(NUM,NUM),FCD(L2,NXYZ),FVD(L2,NXYZ),GD(NN2,NXYZ),
     *          DENC(L2),DENV(L2),DLAG(NUM,NUM,NXYZ),
     *          TQDM(MAXSHL*MAXSHL,NACT*MAXSHL,4) 
C
      PARAMETER (MXGTOT=5000, MXSH=1000, MXAO=2047)
C
      COMMON /ATMJDD/ NATOM(4),NPASS
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /JDDSKP/ MSHL(4),MGETDG(3),SHLON(6),SKIP(4)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /SHLJDD/ LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NNIJ,IJD,KLD,IJ,KL
      COMMON /SKPJDD/ NSKIP1,NSKIP2,NSKIP3,NSKIP4,NSKIP5,
     *                NSKIP6,NSKIP7,NSKIP8,NSKIP9
C
      PARAMETER (HALF=0.5D+00)
C
      I36=3
      NOCC = NCOR + NACT
C
C     ----- TRANSFER OF FIRST DERIVATIVE TEI TO CANONICAL ORDER -----
C
      CALL VCLR(GIJKL1,1,NIJKLD)
      CALL VCLR(GIJKL2,1,NIJKLD)
      CALL VCLR(GIJKL3,1,NIJKLD)
      CALL VCLR(GIJKL4,1,NIJKLD)
C
      NPASS1 = NPASS + 1
      DO 110 I = 1,4
         IATOM = KATOM(MSHL(I))
         DO 100 J = 1,NPASS1
            IF(IATOM .EQ. NATOM(J)) MAD(I) = J
  100    CONTINUE
  110 CONTINUE
C
      ISH = MSHL(1)
      JSH = MSHL(2)
      KSH = MSHL(3)
      LSH = MSHL(4)
      CALL JDFIDX(ISH,JSH,KSH,LSH,MIJ,MKL,MIJKL,NIJKL)
C
      IF(NSKIP1) GO TO 300
      IGO = MAD(1)
      GO TO (210,220,230,300),IGO
  210 CALL JDFCK1(ISH,JSH,KSH,LSH,GIJKL1,GIJKL7,MIJ,MKL,MIJKL,NIJKLD)
      GO TO 300
  220 CALL JDFCK1(ISH,JSH,KSH,LSH,GIJKL2,GIJKL7,MIJ,MKL,MIJKL,NIJKLD)
      GO TO 300
  230 CALL JDFCK1(ISH,JSH,KSH,LSH,GIJKL3,GIJKL7,MIJ,MKL,MIJKL,NIJKLD)
C
  300 CONTINUE
      IF(NSKIP2) GO TO 400
      IGO = MAD(2)
      GO TO (310,320,330,400),IGO
  310 CALL JDFCK2(JSH,ISH,KSH,LSH,GIJKL1,GIJKL8,MIJ,MKL,MIJKL,NIJKLD)
      GO TO 400
  320 CALL JDFCK2(JSH,ISH,KSH,LSH,GIJKL2,GIJKL8,MIJ,MKL,MIJKL,NIJKLD)
      GO TO 400
  330 CALL JDFCK2(JSH,ISH,KSH,LSH,GIJKL3,GIJKL8,MIJ,MKL,MIJKL,NIJKLD)
C
  400 CONTINUE
      IF(NSKIP3) GO TO 500
      IGO = MAD(3)
      GO TO (410,420,430,500),IGO
  410 CALL JDFCK3(KSH,LSH,ISH,JSH,GIJKL1,GIJKL9,MIJ,MKL,MIJKL,NIJKLD)
      GO TO 500
  420 CALL JDFCK3(KSH,LSH,ISH,JSH,GIJKL2,GIJKL9,MIJ,MKL,MIJKL,NIJKLD)
      GO TO 500
  430 CALL JDFCK3(KSH,LSH,ISH,JSH,GIJKL3,GIJKL9,MIJ,MKL,MIJKL,NIJKLD)
C
C     ----- CREATE REDUNDANT FROM GIJKL1 GIJKL2 AND GIJKL3 -----
C
  500 CONTINUE
      NINT = 3 * NIJKL
      GO TO (510,520,530),NPASS
  510 CONTINUE
      DO 515 I = 1,NINT
         GIJKL2(I) = - GIJKL1(I)
  515 CONTINUE
      GO TO 600
  520 CONTINUE
      DO 525 I = 1,NINT
         GIJKL3(I) = - GIJKL1(I) - GIJKL2(I)
  525 CONTINUE
      GO TO 600
  530 CONTINUE
      DO 535 I = 1,NINT
         GIJKL4(I) = - GIJKL1(I) - GIJKL2(I) - GIJKL3(I)
  535 CONTINUE
C
  600 CONTINUE
      IANDJ =   ISH .EQ. JSH
      KANDL =   KSH .EQ. LSH
      SAME =.FALSE.
      NKL = 0
      NIJKL = 0
C
C         LOOPS OVER BASIS FUNCTIONS
C
      DO 4300 L = MINL,MAXL
      LNO = LOCL + L
      IF(KANDL) MAXK = L
      DO 4200 K = MINK,MAXK
      KNO = LOCK + K
      NKL = NKL  + 1
      NIJ = 0
      DO 4100 J = MINJ,MAXJ
      JNO = LOCJ + J
      IF(IANDJ) MAXI = J
      DO 4000 I = MINI,MAXI
      INO = LOCI + I
      NIJ = NIJ + 1
C
      IF(.NOT. SAME) GO TO 700
      IF(NKL .LT. NIJ) GO TO 4000
  700 NIJKL = NIJKL + 1
      LIJKL = 3 * (NIJKL-1)
      IF(INO .GE. JNO) GO TO 710
      LIJ = IA(JNO) + INO
      GO TO 715
  710 LIJ = IA(INO) + JNO
  715 IF(INO .GE. KNO) GO TO 720
      LIK = IA(KNO) + INO
      GO TO 725
  720 LIK = IA(INO) + KNO
  725 IF(INO .GE. LNO) GO TO 730
      LIL = IA(LNO) + INO
      GO TO 735
  730 LIL = IA(INO) + LNO
  735 IF(JNO .GE. KNO) GO TO 740
      LJK = IA(KNO) + JNO
      GO TO 745
  740 LJK = IA(JNO) + KNO
  745 IF(JNO .GE. LNO) GO TO 750
      LJL = IA(LNO) + JNO
      GO TO 755
  750 LJL = IA(JNO) + LNO
  755 IF(KNO .GE. LNO) GO TO 760
      LKL = IA(LNO) + KNO
      GO TO 800
  760 LKL = IA(KNO) + LNO
C
  800 CONTINUE
      DO 2000 IPASS = 1,NPASS1
      GO TO (810,820,830,840),IPASS
  810 VALX  = Q4 * GIJKL1(LIJKL+1)
      VALY  = Q4 * GIJKL1(LIJKL+2)
      VALZ  = Q4 * GIJKL1(LIJKL+3)
      GO TO 850
  820 VALX  = Q4 * GIJKL2(LIJKL+1)
      VALY  = Q4 * GIJKL2(LIJKL+2)
      VALZ  = Q4 * GIJKL2(LIJKL+3)
      GO TO 850
  830 VALX  = Q4 * GIJKL3(LIJKL+1)
      VALY  = Q4 * GIJKL3(LIJKL+2)
      VALZ  = Q4 * GIJKL3(LIJKL+3)
      GO TO 850
  840 VALX  = Q4 * GIJKL4(LIJKL+1)
      VALY  = Q4 * GIJKL4(LIJKL+2)
      VALZ  = Q4 * GIJKL4(LIJKL+3)
C
  850 IATOM = NATOM(IPASS)
      IXA   = I36*(IATOM-1) + 1
      IYA   = IXA + 1
      IZA   = IYA + 1
C
      IF(INO .NE. JNO) GO TO 860
      VALX = VALX * HALF
      VALY = VALY * HALF
      VALZ = VALZ * HALF
  860 IF(KNO .NE. LNO) GO TO 870
      VALX = VALX * HALF
      VALY = VALY * HALF
      VALZ = VALZ * HALF
  870 IF(LIJ .NE. LKL) GO TO 880
      VALX = VALX * HALF
      VALY = VALY * HALF
      VALZ = VALZ * HALF
C
  880 CONTINUE
      VALX = HALF*VALX
      VALY = HALF*VALY
      VALZ = HALF*VALZ
      VALX2  = VALX + VALX
      VALY2  = VALY + VALY
      VALZ2  = VALZ + VALZ
      VALX4  = VALX2 + VALX2
      VALY4  = VALY2 + VALY2
      VALZ4  = VALZ2 + VALZ2
      VALX8  = VALX4 + VALX4
      VALY8  = VALY4 + VALY4
      VALZ8  = VALZ4 + VALZ4
C
      FCD(LIJ,IXA) = FCD(LIJ,IXA) + VALX4 * DENC(LKL)
      FVD(LIJ,IXA) = FVD(LIJ,IXA) + VALX8 * DENV(LKL)
      FCD(LKL,IXA) = FCD(LKL,IXA) + VALX4 * DENC(LIJ)
      FVD(LKL,IXA) = FVD(LKL,IXA) + VALX8 * DENV(LIJ)
      FCD(LIK,IXA) = FCD(LIK,IXA) - VALX  * DENC(LJL)
      FVD(LIK,IXA) = FVD(LIK,IXA) - VALX2 * DENV(LJL)
      FCD(LIL,IXA) = FCD(LIL,IXA) - VALX  * DENC(LJK)
      FVD(LIL,IXA) = FVD(LIL,IXA) - VALX2 * DENV(LJK)
      FCD(LJK,IXA) = FCD(LJK,IXA) - VALX  * DENC(LIL)
      FVD(LJK,IXA) = FVD(LJK,IXA) - VALX2 * DENV(LIL)
      FCD(LJL,IXA) = FCD(LJL,IXA) - VALX  * DENC(LIK)
      FVD(LJL,IXA) = FVD(LJL,IXA) - VALX2 * DENV(LIK)
C
      FCD(LIJ,IYA) = FCD(LIJ,IYA) + VALY4 * DENC(LKL)
      FVD(LIJ,IYA) = FVD(LIJ,IYA) + VALY8 * DENV(LKL)
      FCD(LKL,IYA) = FCD(LKL,IYA) + VALY4 * DENC(LIJ)
      FVD(LKL,IYA) = FVD(LKL,IYA) + VALY8 * DENV(LIJ)
      FCD(LIK,IYA) = FCD(LIK,IYA) - VALY  * DENC(LJL)
      FVD(LIK,IYA) = FVD(LIK,IYA) - VALY2 * DENV(LJL)
      FCD(LIL,IYA) = FCD(LIL,IYA) - VALY  * DENC(LJK)
      FVD(LIL,IYA) = FVD(LIL,IYA) - VALY2 * DENV(LJK)
      FCD(LJK,IYA) = FCD(LJK,IYA) - VALY  * DENC(LIL)
      FVD(LJK,IYA) = FVD(LJK,IYA) - VALY2 * DENV(LIL)
      FCD(LJL,IYA) = FCD(LJL,IYA) - VALY  * DENC(LIK)
      FVD(LJL,IYA) = FVD(LJL,IYA) - VALY2 * DENV(LIK)
C
      FCD(LIJ,IZA) = FCD(LIJ,IZA) + VALZ4 * DENC(LKL)
      FVD(LIJ,IZA) = FVD(LIJ,IZA) + VALZ8 * DENV(LKL)
      FCD(LKL,IZA) = FCD(LKL,IZA) + VALZ4 * DENC(LIJ)
      FVD(LKL,IZA) = FVD(LKL,IZA) + VALZ8 * DENV(LIJ)
      FCD(LIK,IZA) = FCD(LIK,IZA) - VALZ  * DENC(LJL)
      FVD(LIK,IZA) = FVD(LIK,IZA) - VALZ2 * DENV(LJL)
      FCD(LIL,IZA) = FCD(LIL,IZA) - VALZ  * DENC(LJK)
      FVD(LIL,IZA) = FVD(LIL,IZA) - VALZ2 * DENV(LJK)
      FCD(LJK,IZA) = FCD(LJK,IZA) - VALZ  * DENC(LIL)
      FVD(LJK,IZA) = FVD(LJK,IZA) - VALZ2 * DENV(LIL)
      FCD(LJL,IZA) = FCD(LJL,IZA) - VALZ  * DENC(LIK)
      FVD(LJL,IZA) = FVD(LJL,IZA) - VALZ2 * DENV(LIK)
C
      IJKL = 1
      DO 1000 II=NCOR+1,NOCC
        DO 1000 JJ=NCOR+1,II
          DTMP1 = V(INO,II)*V(JNO,JJ) + V(INO,JJ)*V(JNO,II)
          DTMP2 = V(KNO,II)*V(LNO,JJ) + V(KNO,JJ)*V(LNO,II)
          DO 980 KK=NCOR+1,II
            LMAX = KK
            IF(II.EQ.KK) LMAX = JJ
            DO 980 LL=NCOR+1,LMAX
              DTMP3 = V(KNO,KK)*V(LNO,LL) + V(KNO,LL)*V(LNO,KK)
              DTMP4 = V(INO,KK)*V(JNO,LL) + V(INO,LL)*V(JNO,KK)
              DVAL = DTMP1*DTMP3 + DTMP2*DTMP4
              GD(IJKL,IXA) = GD(IJKL,IXA) + DVAL*VALX2
              GD(IJKL,IYA) = GD(IJKL,IYA) + DVAL*VALY2
              GD(IJKL,IZA) = GD(IJKL,IZA) + DVAL*VALZ2
              IJKL = IJKL + 1
  980     CONTINUE
 1000 CONTINUE
C
      DO 1500 M=1,NACT
        IVAL = (J-1)*NACT + M
        DVAL = TQDM(NKL,IVAL,1)
        DLAG(INO,M,IXA) = DLAG(INO,M,IXA) + DVAL*VALX4
        DLAG(INO,M,IYA) = DLAG(INO,M,IYA) + DVAL*VALY4
        DLAG(INO,M,IZA) = DLAG(INO,M,IZA) + DVAL*VALZ4
C
        IVAL = (I-1)*NACT + M
        DVAL = TQDM(NKL,IVAL,2)
        DLAG(JNO,M,IXA) = DLAG(JNO,M,IXA) + DVAL*VALX4
        DLAG(JNO,M,IYA) = DLAG(JNO,M,IYA) + DVAL*VALY4
        DLAG(JNO,M,IZA) = DLAG(JNO,M,IZA) + DVAL*VALZ4
C
        IVAL = (L-1)*NACT + M
        DVAL = TQDM(NIJ,IVAL,3)
        DLAG(KNO,M,IXA) = DLAG(KNO,M,IXA) + DVAL*VALX4
        DLAG(KNO,M,IYA) = DLAG(KNO,M,IYA) + DVAL*VALY4
        DLAG(KNO,M,IZA) = DLAG(KNO,M,IZA) + DVAL*VALZ4
C
        IVAL = (K-1)*NACT + M
        DVAL = TQDM(NIJ,IVAL,4)
        DLAG(LNO,M,IXA) = DLAG(LNO,M,IXA) + DVAL*VALX4
        DLAG(LNO,M,IYA) = DLAG(LNO,M,IYA) + DVAL*VALY4
        DLAG(LNO,M,IZA) = DLAG(LNO,M,IZA) + DVAL*VALZ4
 1500 CONTINUE
C 
 2000 CONTINUE
 4000 CONTINUE
 4100 CONTINUE
 4200 CONTINUE
 4300 CONTINUE
C
      RETURN
      END
C*MODULE HSS2B   *DECK JDDODF
      SUBROUTINE JDDODF(EPSA,V,DF,DFMO,WRK,SHLJ,SHLK,DHAM,IA,
     *                  NAT,NHAM,L1,L2,NFT18,OUT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL OUT,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXAO=2047)
C
      DIMENSION EPSA(L1,L1),V(L1,L1),DF(L2,3,NAT),DFMO(L2),WRK(L1),
     *          SHLJ(L2,NHAM,3,NAT),SHLK(L2,NHAM,3,NAT),
     *          DHAM(3,3,NAT),IA(L1)
C
      COMMON /GVBWFN/ CICOEF(2,12),F(25),ALPHA(325),BETA(325),NO(10),
     *                NCO,NSETO,NOPEN,NPAIR,NMOGVB,NCONF(MXAO),NHAMX
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (ZERO=0.0D+00, TWO=2.0D+00)
C
C     ----- FORM OPEN SHELL DERIVATIVE FOCK MATRIX IN MO BASIS -----
C     ----- FOR OS-TCSCF, FORM DERIVATIVE HAMILTONIAN MATRICES -----
C       ON ENTRY, -DF- CONTAINS THE 1ST DERIVATIVE OF THE 1E- H.
C       ON ENTRY, -SHLJ- AND -SHLK- CONTAIN EACH OCCUPIED SHELL'S
C                 DERIVATIVE COULOMB AND EXCHANGE OPERATOR.
C       FIRST, THESE ARE TRANSFORMED INTO THE MO BASIS.
C
      DO 120 IAT=1,NAT
         DO 110 IXYZ=1,3
            CALL TFTRI(DFMO,DF(1,IXYZ,IAT),V,WRK,L1,L1,L1)
            CALL DCOPY(L2,DFMO,1,DF(1,IXYZ,IAT),1)
            DO 100 IHAM=1,NHAM
               CALL TFTRI(DFMO,SHLJ(1,IHAM,IXYZ,IAT),V,WRK,L1,L1,L1)
               CALL DCOPY(L2,DFMO,1,SHLJ(1,IHAM,IXYZ,IAT),1)
               CALL TFTRI(DFMO,SHLK(1,IHAM,IXYZ,IAT),V,WRK,L1,L1,L1)
               CALL DCOPY(L2,DFMO,1,SHLK(1,IHAM,IXYZ,IAT),1)
  100       CONTINUE
  110    CONTINUE
  120 CONTINUE
C
C     ----- ASSEMBLE THE DERIVATIVE FOCK MATRICES -----
C     THESE ARE NOT SYMMETRIC.
C
      CALL SEQREW(NFT18)
      DO 280 IAT=1,NAT
         DO 270 IXYZ=1,3
            CALL VCLR(EPSA,1,L1*L1)
            DO 240 J=1,L1
               DO 230 I=1,NMOGVB
                  ISH = NCONF(I)
                  IJ = IA(I) + J
                  IF(J.GT.I) IJ = IA(J) + I
                  DUM = ZERO
                  DO 210 KSH=1,NHAM
                     IKSH = IA(ISH) + KSH
                     IF(KSH.GT.ISH) IKSH = IA(KSH) + ISH
                     DUM = DUM + ALPHA(IKSH)*SHLJ(IJ,KSH,IXYZ,IAT)
     *                         +  BETA(IKSH)*SHLK(IJ,KSH,IXYZ,IAT)
  210             CONTINUE
                  EPSA(I,J) = F(ISH) * DF(IJ,IXYZ,IAT) + DUM
  230          CONTINUE
  240       CONTINUE
            IF (MASWRK) WRITE(NFT18) EPSA
  270    CONTINUE
  280 CONTINUE
C
      IF(NPAIR.EQ.0) RETURN
      IF(NPAIR.GT.1) CALL ABRT
C
C        ----- OS-TCSCF DERIVATIVE HAMILTONIAN ELEMENTS -----
C
      JMAXSH = NHAM-2
      NCLOP = NMOGVB - 2
      M     = NMOGVB - 1
      N     = NMOGVB
      MM = IA(M) + M
      NN = IA(N) + N
      MSH = NCONF(M)
      NSH = NCONF(N)
      DO 380 IAT=1,NAT
         DO 370 IXYZ=1,3
            H00 = ZERO
            H11 = ZERO
            H22 = ZERO
            II = 0
            DO 320 I=1,NCLOP
               II  = II + I
               ISH = NCONF(I)
               H00 = H00 + TWO * F(ISH) * DF(II,IXYZ,IAT)
               DO 310 JSH=1,JMAXSH
                  IJSH = IA(ISH) + JSH
                  IF(ISH.LT.JSH) IJSH=IA(JSH)+ISH
                  H00 = H00 + ALPHA(IJSH)*SHLJ(II,JSH,IXYZ,IAT)
     *                      +  BETA(IJSH)*SHLK(II,JSH,IXYZ,IAT)
  310          CONTINUE
               H11 = H11 + F(ISH)*(TWO*SHLJ(II,MSH,IXYZ,IAT)
     *                               - SHLK(II,MSH,IXYZ,IAT))
               H22 = H22 + F(ISH)*(TWO*SHLJ(II,NSH,IXYZ,IAT)
     *                               - SHLK(II,NSH,IXYZ,IAT))
  320       CONTINUE
            H11 = H11 + H11
            H22 = H22 + H22
            H11 = H11 + TWO*DF(MM,IXYZ,IAT) + SHLJ(MM,MSH,IXYZ,IAT)
            H22 = H22 + TWO*DF(NN,IXYZ,IAT) + SHLJ(NN,NSH,IXYZ,IAT)
            H12 = SHLK(MM,NSH,IXYZ,IAT)
            DHAM(1,IXYZ,IAT) = H00 + H11
            DHAM(2,IXYZ,IAT) =       H12
            DHAM(3,IXYZ,IAT) = H00 + H22
  370    CONTINUE
  380 CONTINUE
C
      NDHAM = 3*3*NAT
      CALL DAWRIT(IDAF,IODA,DHAM,NDHAM,66,0)
C
      IF(.NOT.OUT) RETURN
      WRITE(IW,9000)
      DO 440 IAT=1,NAT
         DO 430 IXYZ=1,3
            WRITE(IW,9010) IAT,IXYZ,(DHAM(I,IXYZ,IAT),I=1,3)
  430    CONTINUE
  440 CONTINUE
      RETURN
C
 9000 FORMAT(/5X,'DERIVATIVE HAMILTONIAN MATRIX ELEMENTS'/
     *        1X,' ATOM',' XYZ ',11X,'H11',12X,'H12',12X,'H22')
 9010 FORMAT(1X,I3,I5,3X,3F15.6)
      END
C*MODULE HSS2B   *DECK JDDPRT
      SUBROUTINE JDDPRT(ISH,JSH,KSH,LSH)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL FIRST,SECND,CPHF,BOTH,MFIRST,MSECND,MCPHF
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      COMMON /ATMJDD/ NATOM(4),NPASS
      COMMON /HSSPAR/ FIRST,SECND,CPHF,BOTH,MFIRST,MSECND,MCPHF
      COMMON /INTJDD/ FDRV(3,4),SDRV(9,4,4),ABDENS(1296)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      IF (MASWRK) WRITE(IW,140) ISH,JSH,KSH,LSH
      NPASS1 = NPASS + 1
      IF(.NOT.MFIRST) GO TO 350
      IF (MASWRK) THEN
      DO 30 I = 1,NPASS1
      IA = NATOM(I)
   30 WRITE(IW,100) IA ,    FDRV(1,   I ),FDRV(2,   I ),FDRV(3,   I )
      ENDIF
  350 IF(.NOT.MSECND) GO TO 400
      IF (MASWRK) THEN
      DO 40 I = 1,NPASS1
      IA = NATOM(I)
      DO 40 J = 1,I
      JA = NATOM(J)
      WRITE(IW,110) IA,JA,  SDRV(1,J ,I ),SDRV(2,J ,I ),SDRV(3,J ,I )
      WRITE(IW,120)         SDRV(4,J ,I ),SDRV(5,J ,I ),SDRV(6,J ,I )
      WRITE(IW,130)         SDRV(7,J ,I ),SDRV(8,J ,I ),SDRV(9,J ,I )
   40 CONTINUE
      ENDIF
  400 CONTINUE
      RETURN
  100 FORMAT(" ",5X,'ATOM(',3X,I3,')  FD        D/DX    =',D18.10,
     1           5X,'D/DY    =',D18.10,5X,'D/DZ    =',D18.10)
  110 FORMAT(" ",5X,'ATOM(', 2I3,')  SD        D2/DX2  =',D18.10,
     1           5X,'D2/DYDX =',D18.10,5X,'D2/DZDX =',D18.10)
  120 FORMAT(" ",29X,                         'D2/DXDY =',D18.10,
     1           5X,'D2/DY2  =',D18.10,5X,'D2/DZDY =',D18.10)
  130 FORMAT(" ",29X,                         'D2/DXDZ =',D18.10,
     1           5X,'D2/DYDZ =',D18.10,5X,'D2/DZ2  =',D18.10)
  140 FORMAT(" ",5X,'FIRST AND SECOND DERIVATIVES IN (',
     1           4I5,'  )')
      END
C*MODULE HSS2B   *DECK JDDSPD
      SUBROUTINE JDDSPD(IJ,KL,DIJ,DKL,
     1                IJGT,KLGT,IJX,KLX,IJY,KLY,IJZ,KLZ,GOUT,
     2                NFDIJ,NFDKL,NIJKL)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION DIJ(NFDIJ),DKL(NFDKL),IJGT(NFDIJ),KLGT(NFDKL),
     1          IJX(NFDIJ),KLX(NFDKL),IJY(NFDIJ),KLY(NFDKL),
     2          IJZ(NFDIJ),KLZ(NFDKL)
      DIMENSION GOUT(NIJKL)
C
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /DDXYZ / XINT(2250),YINT(2250),ZINT(2250)
C
C     ----- FORM INTEGRALS OVER FUNCTIONS -----
C     INTEGRAL TYPES ARE [IJ/KL] [IJ/K'L] [IJ/K''L], WITH OR
C     WITHOUT VARIOUS 'S ON THE I AND J (NO MORE THAN 2 PRIMES).
C     TYPE [IJ/K'L'] DOES NOT OCCUR.  SAMPLE VECTOR LENGTHS KL ARE
C        SS = 1    S'S=PS    = 3    S''S=(S+D)S  =  7
C        SP = 3    S'P=PP    = 9    S''P=(S+D)P  = 21
C        DD =36    D'D=(P+F)D=78    D''D=(S+D+G)D=132
C
      GO TO (100,200,300,400,500,600),NROOTS
C
C     CODE FOR NROOTS=1, SCALAR CODE FOR ALL CASES
C
  100 CONTINUE
      DO 160 I = 1,IJ
      D1 = DIJ(I)
      NX = IJX(I)
      NY = IJY(I)
      NZ = IJZ(I)
      N1 = IJGT(I)
      DO 120 K = 1,KL
      MX = NX+KLX(K)
      MY = NY+KLY(K)
      MZ = NZ+KLZ(K)
      N = N1+KLGT(K)
      GOUT(N) = (XINT(MX  )*YINT(MY  )*ZINT(MZ  ))*D1*DKL(K) + GOUT(N)
  120 CONTINUE
  160 CONTINUE
      RETURN
C
C     NROOTS=2
C
  200 CONTINUE
      DO 260 I = 1,IJ
      D1 = DIJ(I)
      NX = IJX(I)
      NY = IJY(I)
      NZ = IJZ(I)
      N1 = IJGT(I)
      DO 220 K = 1,KL
      MX = NX+KLX(K)
      MY = NY+KLY(K)
      MZ = NZ+KLZ(K)
      N = N1+KLGT(K)
      GOUT(N) = (XINT(MX      )*YINT(MY      )*ZINT(MZ      )
     1          +XINT(MX+    1)*YINT(MY+    1)*ZINT(MZ+    1)
     2          )*D1*DKL(K)+GOUT(N)
  220 CONTINUE
  260 CONTINUE
      RETURN
C
C     NROOTS=3
C
  300 CONTINUE
      DO 360 I = 1,IJ
      D1 = DIJ(I)
      NX = IJX(I)
      NY = IJY(I)
      NZ = IJZ(I)
      N1 = IJGT(I)
      DO 320 K = 1,KL
      MX = NX+KLX(K)
      MY = NY+KLY(K)
      MZ = NZ+KLZ(K)
      N = N1+KLGT(K)
      GOUT(N) = (XINT(MX      )*YINT(MY      )*ZINT(MZ      )
     1          +XINT(MX+    1)*YINT(MY+    1)*ZINT(MZ+    1)
     2          +XINT(MX+    2)*YINT(MY+    2)*ZINT(MZ+    2)
     3          )*D1*DKL(K)+GOUT(N)
  320 CONTINUE
  360 CONTINUE
      RETURN
C
C     NROOTS=4
C
  400 CONTINUE
      DO 460 I = 1,IJ
      D1 = DIJ(I)
      NX = IJX(I)
      NY = IJY(I)
      NZ = IJZ(I)
      N1 = IJGT(I)
      DO 420 K = 1,KL
      MX = NX+KLX(K)
      MY = NY+KLY(K)
      MZ = NZ+KLZ(K)
      N = N1+KLGT(K)
      GOUT(N) = (XINT(MX      )*YINT(MY      )*ZINT(MZ      )
     1          +XINT(MX+    1)*YINT(MY+    1)*ZINT(MZ+    1)
     2          +XINT(MX+    2)*YINT(MY+    2)*ZINT(MZ+    2)
     3          +XINT(MX+    3)*YINT(MY+    3)*ZINT(MZ+    3)
     4          )*D1*DKL(K)+GOUT(N)
  420 CONTINUE
  460 CONTINUE
      RETURN
C
C     NROOTS=5
C
  500 CONTINUE
      DO 560 I = 1,IJ
      D1 = DIJ(I)
      NX = IJX(I)
      NY = IJY(I)
      NZ = IJZ(I)
      N1 = IJGT(I)
      DO 520 K = 1,KL
      MX = NX+KLX(K)
      MY = NY+KLY(K)
      MZ = NZ+KLZ(K)
      N = N1+KLGT(K)
      GOUT(N) = (XINT(MX      )*YINT(MY      )*ZINT(MZ      )
     1          +XINT(MX+    1)*YINT(MY+    1)*ZINT(MZ+    1)
     2          +XINT(MX+    2)*YINT(MY+    2)*ZINT(MZ+    2)
     3          +XINT(MX+    3)*YINT(MY+    3)*ZINT(MZ+    3)
     4          +XINT(MX+    4)*YINT(MY+    4)*ZINT(MZ+    4)
     5          )*D1*DKL(K)+GOUT(N)
  520 CONTINUE
  560 CONTINUE
      RETURN
C
C     NROOTS=6
C
  600 CONTINUE
      DO 660 I=1,IJ
      D1 = DIJ(I)
      NX = IJX(I)
      NY = IJY(I)
      NZ = IJZ(I)
      N1 = IJGT(I)
      DO 620 K=1,KL
      MX = NX+KLX(K)
      MY = NY+KLY(K)
      MZ = NZ+KLZ(K)
      N = N1+KLGT(K)
      GOUT(N) = (XINT(MX      )*YINT(MY      )*ZINT(MZ      )
     1          +XINT(MX+    1)*YINT(MY+    1)*ZINT(MZ+    1)
     2          +XINT(MX+    2)*YINT(MY+    2)*ZINT(MZ+    2)
     3          +XINT(MX+    3)*YINT(MY+    3)*ZINT(MZ+    3)
     4          +XINT(MX+    4)*YINT(MY+    4)*ZINT(MZ+    4)
     5          +XINT(MX+    5)*YINT(MY+    5)*ZINT(MZ+    5)
     6          )*D1*DKL(K)+GOUT(N)
  620 CONTINUE
  660 CONTINUE
      RETURN
      END
C*MODULE HSS2B   *DECK JDDTRN
      SUBROUTINE JDDTRN(TQDM,VEC,HBDM,IA,NCOR,N1,N2,L1,L2,M1)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SHLON,SKIP,IANDJ,KANDL
C 
      DIMENSION VEC(L1,L1),HBDM(L2,N2),TQDM(M1*M1,M1*N1,4)
      DIMENSION IA(L1)
C
      COMMON /JDDSKP/ MSHL(4),MGETDG(3),SHLON(6),SKIP(4)
      COMMON /SHLJDD/ LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NNIJ,IJD,KLD,IJ,KL
C
      PARAMETER (ZERO=0.0D+00)
C
      NOCC = NCOR+N1
      MVAL = 4*N1*M1*M1*M1
      CALL VCLR(TQDM,1,MVAL)
C
      ISH = MSHL(1)
      JSH = MSHL(2)
      KSH = MSHL(3)
      LSH = MSHL(4)
      IANDJ =   ISH .EQ. JSH
      KANDL =   KSH .EQ. LSH
C
C     PERFORM PARTIAL THIRD-QUARTER BACK-TRANSFORMATION OF
C     TWO PARTICLE DENSITY MATRIX 
C
      NKL = 0
      MAXKK = MAXK
C
      DO 2000 L=MINL,MAXL
        LNO = LOCL + L
        IF(KANDL) MAXKK = L
        DO 1000 K=MINK,MAXKK
          KNO = LOCK + K
          NKL = NKL + 1
          LG = MAX(LNO,KNO)
          KG = MIN(LNO,KNO)
          LKL = IA(LG) + KG
C
          DO 400 J=MINJ,MAXJ
            JNO = LOCJ + J
            DO 200 M=1,N1
              DVAL = ZERO
              DO 100 N=NCOR+1,NOCC
                MG = MAX(M,N-NCOR)
                NG = MIN(M,N-NCOR)
                MN = IA(MG) + NG
                DVAL = DVAL + VEC(JNO,N)*HBDM(LKL,MN)
  100         CONTINUE
              MJ = (J-1)*N1 + M
              TQDM(NKL,MJ,1) = TQDM(NKL,MJ,1) + DVAL 
  200       CONTINUE
  400     CONTINUE
C
          DO 800 I=MINI,MAXI
            INO = LOCI + I
            DO 600 M=1,N1
              DVAL = ZERO
              DO 500 N=NCOR+1,NOCC
                MG = MAX(M,N-NCOR)
                NG = MIN(M,N-NCOR)
                MN = IA(MG) + NG
                DVAL = DVAL + VEC(INO,N)*HBDM(LKL,MN)
  500         CONTINUE
              MI = (I-1)*N1 + M
              TQDM(NKL,MI,2) = TQDM(NKL,MI,2) + DVAL
  600       CONTINUE 
  800     CONTINUE
C
 1000   CONTINUE
 2000 CONTINUE 
C
C
      NIJ = 0
      MAXII = MAXI
C
      DO 5000 J=MINJ,MAXJ
        JNO = LOCJ + J
        IF(IANDJ) MAXII = J
        DO 4000 I=MINI,MAXII
          INO = LOCI + I
          NIJ = NIJ + 1
          JG = MAX(JNO,INO)
          IG = MIN(JNO,INO)
          LIJ = IA(JG) + IG
C
          DO 3400 L=MINL,MAXL
            LNO = LOCL + L
            DO 3200 M=1,N1
              DVAL = ZERO
              DO 3100 N=NCOR+1,NOCC
                MG = MAX(M,N-NCOR)
                NG = MIN(M,N-NCOR)
                MN = IA(MG) + NG
                DVAL = DVAL + VEC(LNO,N)*HBDM(LIJ,MN)
 3100         CONTINUE
              ML = (L-1)*N1 + M
              TQDM(NIJ,ML,3) = TQDM(NIJ,ML,3) + DVAL
 3200       CONTINUE
 3400     CONTINUE
C
          DO 3800 K=MINK,MAXK
            KNO = LOCK + K
            DO 3600 M=1,N1
              DVAL = ZERO
              DO 3500 N=NCOR+1,NOCC
                MG = MAX(M,N-NCOR)
                NG = MIN(M,N-NCOR)
                MN = IA(MG) + NG
                DVAL = DVAL + VEC(KNO,N)*HBDM(LIJ,MN)
 3500         CONTINUE
              MK = (K-1)*N1 + M
              TQDM(NIJ,MK,4) = TQDM(NIJ,MK,4) + DVAL
 3600       CONTINUE
 3800     CONTINUE
C
 4000   CONTINUE
 5000 CONTINUE
C
      RETURN
      END
C*MODULE HSS2B   *DECK JDDXYZ
      SUBROUTINE JDDXYZ
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL N0,N1,M0,M1
C
      COMMON /XYZJDD/ BP01,B00,B10,XCP00,XC00,YCP00,YC00,ZCP00,ZC00,
     *                F00,DXIJ,DYIJ,DZIJ,DXKL,DYKL,DZKL,I(9),K(7),
     *                NIMAX,NJMAX,NKMAX,NLMAX,NMAX,MMAX,IJ1,IJ2,KL1,KL2
      COMMON /DDXYZ / XINT(2250),YINT(2250),ZINT(2250)
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
C
      N0 = NMAX .EQ. 0
      N1 = NMAX .LE. 1
      M0 = MMAX .EQ. 0
      M1 = MMAX .LE. 1
C
C     ----- I(0,0) -----
C
      I1 = I(1)
      XINT(I1) = ONE
      YINT(I1) = ONE
      ZINT(I1) = F00
      IF (N0 .AND. M0) RETURN
      IF (N0) GO TO 100
C
C     ----- I(1,0) -----
C
      I2 = I(2)
      XINT(I2) = XC00
      YINT(I2) = YC00
      ZINT(I2) = ZC00*F00
  100 IF (M0) GO TO 120
C
C     ----- I(0,1) -----
C
      K2 = K(2)
      I3 = I1+K2
      XINT(I3) = XCP00
      YINT(I3) = YCP00
      ZINT(I3) = ZCP00*F00
      IF (N0) GO TO 120
C
C     ----- I(1,1) -----
C
      I3 = I2+K2
      CP10 = B00
      XINT(I3) = XCP00*XINT(I2)+CP10
      YINT(I3) = YCP00*YINT(I2)+CP10
      ZINT(I3) = ZCP00*ZINT(I2)+CP10*F00
  120 IF (N1) GO TO 180
      C10 = ZERO
      I3 = I1
      I4 = I2
      DO 160 N = 2,NMAX
      C10 = C10+B10
C
C     ----- I(N,0) -----
C
      I5 = I(N+1)
      XINT(I5) = C10*XINT(I3)+XC00*XINT(I4)
      YINT(I5) = C10*YINT(I3)+YC00*YINT(I4)
      ZINT(I5) = C10*ZINT(I3)+ZC00*ZINT(I4)
      IF (M0) GO TO 140
      CP10 = CP10+B00
C
C     ----- I(N,1) -----
C
      I3 = I5+K2
      XINT(I3) = XCP00*XINT(I5)+CP10*XINT(I4)
      YINT(I3) = YCP00*YINT(I5)+CP10*YINT(I4)
      ZINT(I3) = ZCP00*ZINT(I5)+CP10*ZINT(I4)
  140 I3 = I4
  160 I4 = I5
  180 IF (M1) GO TO 240
      CP01 = ZERO
      C01 = B00
      I3 = I1
      I4 = I1+K2
      DO 220 M = 2,MMAX
      CP01 = CP01+BP01
C
C     ----- I(0,M) -----
C
      I5 = I1+K(M+1)
      XINT(I5) = CP01*XINT(I3)+XCP00*XINT(I4)
      YINT(I5) = CP01*YINT(I3)+YCP00*YINT(I4)
      ZINT(I5) = CP01*ZINT(I3)+ZCP00*ZINT(I4)
      IF (N0) GO TO 200
      C01 = C01+B00
C
C     ----- I(1,M) -----
C
      I3 = I2+K(M+1)
      XINT(I3) = XC00*XINT(I5)+C01*XINT(I4)
      YINT(I3) = YC00*YINT(I5)+C01*YINT(I4)
      ZINT(I3) = ZC00*ZINT(I5)+C01*ZINT(I4)
  200 I3 = I4
  220 I4 = I5
  240 IF (N1 .OR. M1) GO TO 300
C
C     ----- I(N,M) -----
C
      C01 = B00
      K3 = K2
      DO 280 M = 2,MMAX
      K4 = K(M+1)
      C01 = C01+B00
      I3 = I1
      I4 = I2
      C10 = B10
      DO 260 N = 2,NMAX
      I5 = I(N+1)
      XINT(I5+K4) = C10*XINT(I3+K4)+XC00*XINT(I4+K4)+C01*XINT(I4+K3)
      YINT(I5+K4) = C10*YINT(I3+K4)+YC00*YINT(I4+K4)+C01*YINT(I4+K3)
      ZINT(I5+K4) = C10*ZINT(I3+K4)+ZC00*ZINT(I4+K4)+C01*ZINT(I4+K3)
      C10 = C10+B10
      I3 = I4
  260 I4 = I5
  280 K3 = K4
  300 IF (NJMAX .EQ. 0) GO TO 440
C
C     ----- I(NI,NJ,M) -----
C
      M = 0
      I5 = I(NMAX+1)
  320 MIN = NIMAX
      KM = K(M+1)
  340 N = NMAX
      I3 = I5+KM
  360 I4 = I(N)+KM
      XINT(I3) = XINT(I3)+DXIJ*XINT(I4)
      YINT(I3) = YINT(I3)+DYIJ*YINT(I4)
      ZINT(I3) = ZINT(I3)+DZIJ*ZINT(I4)
      I3 = I4
      N = N-1
      IF (N .GT. MIN) GO TO 360
      MIN = MIN+1
      IF (MIN .LT. NMAX) GO TO 340
      IF (NIMAX .EQ. 0) GO TO 420
      I3 = IJ2+I1+KM
      DO 400 NJ = 1,NJMAX
      I4 = I3
      DO 380 NI = 1,NIMAX
      XINT(I4) = XINT(I4+IJ1-IJ2)+DXIJ*XINT(I4-IJ2)
      YINT(I4) = YINT(I4+IJ1-IJ2)+DYIJ*YINT(I4-IJ2)
      ZINT(I4) = ZINT(I4+IJ1-IJ2)+DZIJ*ZINT(I4-IJ2)
  380 I4 = I4+IJ1
  400 I3 = I3+IJ2
  420 M = M+1
      IF (M .LE. MMAX) GO TO 320
  440 IF (NLMAX .EQ. 0) GO TO 600
C
C     ----- I(NI,NJ,NK,NL) -----
C
      I5 = K(MMAX+1)
      IA = I1
      NI = 0
  460 NJ = 0
      IB = IA
  480 MIN = NKMAX
  500 M = MMAX
      I3 = IB+I5
  520 I4 = IB+K(M)
      XINT(I3) = XINT(I3)+DXKL*XINT(I4)
      YINT(I3) = YINT(I3)+DYKL*YINT(I4)
      ZINT(I3) = ZINT(I3)+DZKL*ZINT(I4)
      I3 = I4
      M = M-1
      IF (M .GT. MIN) GO TO 520
      MIN = MIN+1
      IF (MIN .LT. MMAX) GO TO 500
      IF (NKMAX .EQ. 0) GO TO 580
      I3 = IB+KL2
      DO 560 NL = 1,NLMAX
      I4 = I3
      DO 540 NK = 1,NKMAX
      XINT(I4) = XINT(I4+KL1-KL2)+DXKL*XINT(I4-KL2)
      YINT(I4) = YINT(I4+KL1-KL2)+DYKL*YINT(I4-KL2)
      ZINT(I4) = ZINT(I4+KL1-KL2)+DZKL*ZINT(I4-KL2)
  540 I4 = I4+KL1
  560 I3 = I3+KL2
  580 NJ = NJ+1
      IB = IB+IJ2
      IF (NJ .LE. NJMAX) GO TO 480
      NI = NI+1
      IA = IA+IJ1
      IF (NI .LE. NIMAX) GO TO 460
  600 CONTINUE
      RETURN
      END
C*MODULE HSS2B   *DECK JDFCK1
      SUBROUTINE JDFCK1(ISH,JSH,KSH,LSH,GA,GB,MIJ,MKL,MIJKL,NIJKLD)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL KANDL,SAME
C
      DIMENSION GA(NIJKLD),GB(NIJKLD),MIJ(6,6),MKL(6,6),MIJKL(36)
C
      PARAMETER (MXGTOT=5000, MXSH=1000)
C
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
C
C     ----- INITIAL SET OF PARAMETERS -----
C
      MINI = KMIN(ISH)
      MINJ = KMIN(JSH)
      MINK = KMIN(KSH)
      MINL = KMIN(LSH)
      MAXI = KMAX(ISH)
      MAXJ = KMAX(JSH)
      MAXK = KMAX(KSH)
      MAXL = KMAX(LSH)
      NI0  = MAXI - MINI + 1
      NJ0  = MAXJ - MINJ + 1
      NK0  = MAXK - MINK + 1
      NL0  = MAXL - MINL + 1
      KANDL =   KSH .EQ. LSH
      SAME =.FALSE.
C
C     ----- TRANSFER FIRST DERIVATIVES TEI TO CANONICAL ORDER -----
C
      IJKL = -3
      DO 4200 L = 1,NL0
      IF(KANDL) NK0 = L
      DO 4200 K = 1,NK0
      NKL = MKL(K,L)
      DO 4100 J = 1,NJ0
      DO 4000 I = 1,NI0
      NIJ = MIJ(I,J)
      IJKL = IJKL + 3
      IF(SAME) GO TO 200
      NIJKL = MIJKL(NKL) + NIJ
      GO TO 220
  200 IF(NIJ .GE. NKL) GO TO 210
      NIJKL = MIJKL(NKL) + NIJ
      GO TO 220
  210 NIJKL = MIJKL(NIJ) + NKL
  220 LIJKL = 3 * (NIJKL-1)
      GA(LIJKL+1) = GA(LIJKL+1) + GB(IJKL+1)
      GA(LIJKL+2) = GA(LIJKL+2) + GB(IJKL+2)
      GA(LIJKL+3) = GA(LIJKL+3) + GB(IJKL+3)
 4000 CONTINUE
 4100 CONTINUE
 4200 CONTINUE
      RETURN
      END
C*MODULE HSS2B   *DECK JDFCK2
      SUBROUTINE JDFCK2(ISH,JSH,KSH,LSH,GA,GB,MIJ,MKL,MIJKL,NIJKLD)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL KANDL,SAME
C
      DIMENSION GA(NIJKLD),GB(NIJKLD),MIJ(6,6),MKL(6,6),MIJKL(36)
C
      PARAMETER (MXGTOT=5000, MXSH=1000)
C
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
C
C     ----- INITIAL SET OF PARAMETERS -----
C
      MINI = KMIN(ISH)
      MINJ = KMIN(JSH)
      MINK = KMIN(KSH)
      MINL = KMIN(LSH)
      MAXI = KMAX(ISH)
      MAXJ = KMAX(JSH)
      MAXK = KMAX(KSH)
      MAXL = KMAX(LSH)
      NI0  = MAXI - MINI + 1
      NJ0  = MAXJ - MINJ + 1
      NK0  = MAXK - MINK + 1
      NL0  = MAXL - MINL + 1
      KANDL =   KSH .EQ. LSH
      SAME =.FALSE.
C
C     ----- TRANSFER FIRST DERIVATIVES TEI TO CANONICAL ORDER -----
C
      IJKL = -3
      DO 4200 L = 1,NL0
      IF(KANDL) NK0 = L
      DO 4200 K = 1,NK0
      NKL = MKL(K,L)
      DO 4100 J = 1,NJ0
      DO 4000 I = 1,NI0
      NIJ = MIJ(J,I)
      IJKL = IJKL + 3
      IF(SAME) GO TO 200
      NIJKL = MIJKL(NKL) + NIJ
      GO TO 220
  200 IF(NIJ .GE. NKL) GO TO 210
      NIJKL = MIJKL(NKL) + NIJ
      GO TO 220
  210 NIJKL = MIJKL(NIJ) + NKL
  220 LIJKL = 3 * (NIJKL-1)
      GA(LIJKL+1) = GA(LIJKL+1) + GB(IJKL+1)
      GA(LIJKL+2) = GA(LIJKL+2) + GB(IJKL+2)
      GA(LIJKL+3) = GA(LIJKL+3) + GB(IJKL+3)
 4000 CONTINUE
 4100 CONTINUE
 4200 CONTINUE
      RETURN
      END
C*MODULE HSS2B   *DECK JDFCK3
      SUBROUTINE JDFCK3(ISH,JSH,KSH,LSH,GA,GB,MIJ,MKL,MIJKL,NIJKLD)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL KANDL,SAME
C
      DIMENSION GA(NIJKLD),GB(NIJKLD),MIJ(6,6),MKL(6,6),MIJKL(36)
C
      PARAMETER (MXGTOT=5000, MXSH=1000)
C
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
C
C     ----- INITIAL SET OF PARAMETERS -----
C
      MINI = KMIN(ISH)
      MINJ = KMIN(JSH)
      MINK = KMIN(KSH)
      MINL = KMIN(LSH)
      MAXI = KMAX(ISH)
      MAXJ = KMAX(JSH)
      MAXK = KMAX(KSH)
      MAXL = KMAX(LSH)
      NI0  = MAXI - MINI + 1
      NJ0  = MAXJ - MINJ + 1
      NK0  = MAXK - MINK + 1
      NL0  = MAXL - MINL + 1
      KANDL =   KSH .EQ. LSH
      SAME =.FALSE.
C
C     ----- TRANSFER FIRST DERIVATIVES TEI TO CANONICAL ORDER -----
C
      IJKL = -3
      DO 4200 L = 1,NL0
      IF(KANDL) NK0 = L
      DO 4200 K = 1,NK0
      NKL = MIJ(K,L)
      DO 4100 J = 1,NJ0
      DO 4000 I = 1,NI0
      NIJ = MKL(I,J)
      IJKL = IJKL + 3
      IF(SAME) GO TO 200
      NIJKL = MIJKL(NIJ) + NKL
      GO TO 220
  200 IF(NIJ .GE. NKL) GO TO 210
      NIJKL = MIJKL(NKL) + NIJ
      GO TO 220
  210 NIJKL = MIJKL(NIJ) + NKL
  220 LIJKL = 3 * (NIJKL-1)
      GA(LIJKL+1) = GA(LIJKL+1) + GB(IJKL+1)
      GA(LIJKL+2) = GA(LIJKL+2) + GB(IJKL+2)
      GA(LIJKL+3) = GA(LIJKL+3) + GB(IJKL+3)
 4000 CONTINUE
 4100 CONTINUE
 4200 CONTINUE
      RETURN
      END
C*MODULE HSS2B   *DECK JDFIDX
      SUBROUTINE JDFIDX(ISH,JSH,KSH,LSH,MIJ,MKL,MIJKL,NIJKL)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL IANDJ,KANDL,SAME
C
      DIMENSION MIJ(6,6),MKL(6,6),MIJKL(36)
C
      PARAMETER (MXGTOT=5000, MXSH=1000)
C
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
C
      IANDJ =   ISH .EQ. JSH
      KANDL =   KSH .EQ. LSH
      SAME =.FALSE.
      IGSH  = KMAX(ISH) - KMIN(ISH) + 1
      JGSH  = KMAX(JSH) - KMIN(JSH) + 1
      KGSH  = KMAX(KSH) - KMIN(KSH) + 1
      LGSH  = KMAX(LSH) - KMIN(LSH) + 1
      NIJ   = 0
      IF(IANDJ) GO TO 200
      DO 10 JG = 1,JGSH
      DO 10 IG = 1,IGSH
      NIJ = NIJ + 1
   10 MIJ(IG,JG) = NIJ
      GO TO 300
  200 DO 20 IG = 1,IGSH
      DO 20 JG = 1,IG
      NIJ = NIJ + 1
      MIJ(IG,JG) = NIJ
   20 MIJ(JG,IG) = NIJ
  300 NKL = 0
      IF(KANDL) GO TO 400
      DO 30 LG = 1,LGSH
      DO 30 KG = 1,KGSH
      NKL = NKL + 1
   30 MKL(KG,LG) = NKL
      GO TO 500
  400 DO 40 KG = 1,KGSH
      DO 40 LG = 1,KG
      NKL = NKL + 1
      MKL(KG,LG) = NKL
   40 MKL(LG,KG) = NKL
  500 IF(SAME) GO TO 600
      NIJKL = NIJ * NKL
      DO 50 KLG = 1,NKL
   50 MIJKL(KLG) = NIJ * (KLG-1)
      GO TO 700
  600 NIJKL = NIJ * (NIJ+1) / 2
      DO 60 IJG = 1,NIJ
   60 MIJKL(IJG) = IJG * (IJG-1) / 2
  700 RETURN
      END
C*MODULE HSS2B   *DECK RFCKR
      SUBROUTINE RFCKR(T,MINK,MAXK,LKT,MINL,MAXL,LLT,NTR)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (NDIM=35)
C
      DIMENSION V(NDIM),T(NDIM,NDIM)
C
      COMMON /SYMSPD/ PTR(3,144),DTR(6,288),FTR(10,480),GTR(15,720)
C
      DATA ZERO /0.0D+00/
C
C     ----- RIGHT MULTIPLY  T  BY  R,
C           RESULT BACK IN  T
C
      GO TO (260,180,100),LLT
C
C     ----- D SHELL
C
  100 ND = 6*(NTR-1)- 4
      DO 160 K = MINK,MAXK
      DO 140 L = 5,10
      DUM = ZERO
      DO 120 N = 5,10
  120 DUM = DUM+T(K,N)*DTR(N-4,ND+L)
  140 V(L) = DUM
      DO 160 L = 5,10
  160 T(K,L) = V(L)
      GO TO 260
C
C     ----- P SHELL
C
  180 NP = 3*(NTR-1)- 1
      DO 240 K = MINK,MAXK
      DO 220 L = 2,4
      DUM = ZERO
      DO 200 N = 2,4
  200 DUM = DUM+T(K,N)*PTR(N-1,NP+L)
  220 V(L) = DUM
      DO 240 L = 2,4
  240 T(K,L) = V(L)
  260 CONTINUE
C
C     ----- LEFT MULTIPLY  T  BY R
C           RESULT BACK IN  T
C
      GO TO (440,360,280),LKT
C
C     ----- D SHELL
C
  280 ND = 6*(NTR-1)-4
      DO 340 L = MINL,MAXL
      DO 320 K = 5,10
      DUM = ZERO
      DO 300 N = 5,10
  300 DUM = DUM+DTR(N-4,ND+K)*T(N,L)
  320 V(K) = DUM
      DO 340 K = 5,10
  340 T(K,L) = V(K)
      GO TO 440
C
C     ----- P SHELL
C
  360 NP = 3*(NTR-1)- 1
      DO 420 L = MINL,MAXL
      DO 400 K = 2,4
      DUM = ZERO
      DO 380 N = 2,4
  380 DUM = DUM+PTR(N-1,NP+K)*T(N,L)
  400 V(K) = DUM
      DO 420 K = 2,4
  420 T(K,L) = V(K)
  440 CONTINUE
      RETURN
      END
C*MODULE HSS2B   *DECK SYMDF
      SUBROUTINE SYMDF(DF,L2,NHAM,I36,NAT,IHAM,IX,IY,IZ,
     *                 TX,TY,TZ,UX,UY,UZ)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXGTOT=5000, MXSH=1000, MXATM=500, MXAO=2047)
C
      DIMENSION DF(L2,NHAM,I36,NAT),
     *          TX(35,35),TY(35,35),TZ(35,35),
     *          UX(35,35),UY(35,35),UZ(35,35)
      DIMENSION MI(48),MJ(48)
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /SYMSPD/ PTR(3,144),DTR(6,288),FTR(10,480),GTR(15,720)
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                T(432),INVT(48),NT
      COMMON /IJPAIR/ IA(MXAO)
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
C
C     ----- SYMMETRIZE THE SKELETON DERIVATIVE FOCK MATRIX -----
C     THREE SETS ARE SYMMETRIZED AT A TIME, IX,IY,IZ TELL WHICH ONES.
C     THESE ARE STORED IN THE ORDER DFA/DX, DFA/DY, DFA/DZ FOR -RHF-,
C        POSSIBLY FOLLOWED BY DFB/DX, DFB/DY, DFB/DZ FOR -UHF-.
C     OPEN SHELL CASE HAS SEVERAL (NHAM) SETS OF DFA/DX, SO -IHAM-
C        TELLS WHICH SET OF THREE IS BEING SYMMETRIZED.
C
      IF (NT .EQ. 1) RETURN
C
C     ----- GET SYMMETRY DATA -----
C
      CALL DAREAD(IDAF,IODA,PTR,  432, 7,0)
      CALL DAREAD(IDAF,IODA,DTR, 1728, 8,0)
CMWS  CALL DAREAD(IDAF,IODA,FTR, 4800, 9,0)
CMWS  CALL DAREAD(IDAF,IODA,GTR,10800,10,0)
C
C     ----- LOOP OVER UNIQUE BLOCKS (II,JJ) -----
C
      DO 3000 II=1,NSHELL
      DO 110 ITR=1,NT
      ISH=MAPSHL(II,ITR)
      IF(ISH.GT.II) GO TO 3000
  110 MI(ITR)=ISH
      MINI=KMIN(II)
      MAXI=KMAX(II)
      LOCI=KLOC(II)-MINI
C
      DO 2000 JJ=1,II
      DO 220 ITR=1,NT
      JSH=MAPSHL(JJ,ITR)
      MJ(ITR)=JSH
      IF(JSH.GT.II) GO TO 2000
      ISH=MI(ITR)
      IF(ISH.GE.JSH) GO TO 210
      NSH=ISH
      ISH=JSH
      JSH=NSH
  210 IF(ISH.EQ.II.AND.JSH.GT.JJ) GO TO 2000
  220 CONTINUE
      MINJ=KMIN(JJ)
      MAXJ=KMAX(JJ)
      LOCJ=KLOC(JJ)-MINJ
C
C     ----- LOOP OVER UNIQUE ATOMS -IAT- -----
C
      DO 1000 IAT=1,NAT
C
C     ----- APPLY PROJECTION OPERATOR -----
C
      DO 410 J=MINJ,MAXJ
      DO 410 I=MINI,MAXI
      UX(I,J)=ZERO
      UY(I,J)=ZERO
  410 UZ(I,J)=ZERO
C
      DO 500 ITR=1,NT
      KK=MI(ITR)
      LKT=KTYPE(KK)
      MINK=KMIN(KK)
      MAXK=KMAX(KK)
      LOCK=KLOC(KK)-MINK
      LL=MJ(ITR)
      LLT=KTYPE(LL)
      MINL=KMIN(LL)
      MAXL=KMAX(LL)
      LOCL=KLOC(LL)-MINL
C
      KAT=MAPCTR(IAT,ITR)
      DO 420 L=MINL,MAXL
      DO 420 K=MINK,MAXK
      LCK=LOCK+K
      LCL=LOCL+L
      LCKL=IA(MAX(LCK,LCL))+MIN(LCK,LCL)
      TX(K,L)=DF(LCKL,IHAM,IX,KAT)
      TY(K,L)=DF(LCKL,IHAM,IY,KAT)
      TZ(K,L)=DF(LCKL,IHAM,IZ,KAT)
  420 CONTINUE
C
      CALL RFCKR(TX,MINK,MAXK,LKT,MINL,MAXL,LLT,ITR)
      CALL RFCKR(TY,MINK,MAXK,LKT,MINL,MAXL,LLT,ITR)
      CALL RFCKR(TZ,MINK,MAXK,LKT,MINL,MAXL,LLT,ITR)
      DO 430 L=MINL,MAXL
      DO 430 K=MINK,MAXK
      UX(K,L)=UX(K,L)+PTR(1,3*(ITR-1)+1)*TX(K,L)
     1               +PTR(2,3*(ITR-1)+1)*TY(K,L)
     2               +PTR(3,3*(ITR-1)+1)*TZ(K,L)
      UY(K,L)=UY(K,L)+PTR(1,3*(ITR-1)+2)*TX(K,L)
     1               +PTR(2,3*(ITR-1)+2)*TY(K,L)
     2               +PTR(3,3*(ITR-1)+2)*TZ(K,L)
      UZ(K,L)=UZ(K,L)+PTR(1,3*(ITR-1)+3)*TX(K,L)
     1               +PTR(2,3*(ITR-1)+3)*TY(K,L)
     2               +PTR(3,3*(ITR-1)+3)*TZ(K,L)
  430 CONTINUE
C
  500 CONTINUE
C
      DUM=ONE/NT
      DO 510 J=MINJ,MAXJ
      DO 510 I=MINI,MAXI
      UX(I,J)=UX(I,J)*DUM
      UY(I,J)=UY(I,J)*DUM
      UZ(I,J)=UZ(I,J)*DUM
      LCI=LOCI+I
      LCJ=LOCJ+J
      LCIJ=IA(MAX(LCI,LCJ))+MIN(LCI,LCJ)
      DF(LCIJ,IHAM,IX,IAT)=UX(I,J)
      DF(LCIJ,IHAM,IY,IAT)=UY(I,J)
      DF(LCIJ,IHAM,IZ,IAT)=UZ(I,J)
  510 CONTINUE
C
C     ----- SET EQUIVALENT BLOCKS (KK,LL) FOR EQUIVALENT ATOMS -----
C
      DO 600 ITR=1,NT
      KK=MI(ITR)
      LKT=KTYPE(KK)
      MINK=KMIN(KK)
      MAXK=KMAX(KK)
      LOCK=KLOC(KK)-MINK
      LL=MJ(ITR)
      LLT=KTYPE(LL)
      MINL=KMIN(LL)
      MAXL=KMAX(LL)
      LOCL=KLOC(LL)-MINL
C
      DO 520 J=MINJ,MAXJ
      DO 520 I=MINI,MAXI
      LCI=LOCI+I
      LCJ=LOCJ+J
      LCIJ=IA(MAX(LCI,LCJ))+MIN(LCI,LCJ)
      TX(I,J)=DF(LCIJ,IHAM,IX,IAT)
      TY(I,J)=DF(LCIJ,IHAM,IY,IAT)
      TZ(I,J)=DF(LCIJ,IHAM,IZ,IAT)
  520 CONTINUE
C
      CALL RFCKR(TX,MINK,MAXK,LKT,MINL,MAXL,LLT,INVT(ITR))
      CALL RFCKR(TY,MINK,MAXK,LKT,MINL,MAXL,LLT,INVT(ITR))
      CALL RFCKR(TZ,MINK,MAXK,LKT,MINL,MAXL,LLT,INVT(ITR))
      DO 530 L=MINL,MAXL
      DO 530 K=MINK,MAXK
      UX(K,L)= PTR(1,3*(INVT(ITR)-1)+1)*TX(K,L)
     1        +PTR(2,3*(INVT(ITR)-1)+1)*TY(K,L)
     2        +PTR(3,3*(INVT(ITR)-1)+1)*TZ(K,L)
      UY(K,L)= PTR(1,3*(INVT(ITR)-1)+2)*TX(K,L)
     1        +PTR(2,3*(INVT(ITR)-1)+2)*TY(K,L)
     2        +PTR(3,3*(INVT(ITR)-1)+2)*TZ(K,L)
      UZ(K,L)= PTR(1,3*(INVT(ITR)-1)+3)*TX(K,L)
     1        +PTR(2,3*(INVT(ITR)-1)+3)*TY(K,L)
     2        +PTR(3,3*(INVT(ITR)-1)+3)*TZ(K,L)
  530 CONTINUE
C
      KAT=MAPCTR(IAT,ITR)
      DO 540 L=MINL,MAXL
      DO 540 K=MINK,MAXK
      LCK=LOCK+K
      LCL=LOCL+L
      LCKL=IA(MAX(LCK,LCL))+MIN(LCK,LCL)
      DF(LCKL,IHAM,IX,KAT)=UX(K,L)
      DF(LCKL,IHAM,IY,KAT)=UY(K,L)
      DF(LCKL,IHAM,IZ,KAT)=UZ(K,L)
C
  540 CONTINUE
  600 CONTINUE
C
 1000 CONTINUE
C
 2000 CONTINUE
 3000 CONTINUE
      RETURN
      END
C*MODULE HSS2B   *DECK SYMEH
      SUBROUTINE SYMEH(EH)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXSH=1000, MXATM=500)
C
      DIMENSION EH(9,*)
      DIMENSION TH(3,3),WH(3,3),RH(3,3),WK(3,3)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                T(432),INVT(48),NT
      COMMON /SYMSPD/ PTR(3,144),DTR(6,288),FTR(10,480),GTR(15,720)
C
      DATA ZERO,ONE /0.0D+00,1.0D+00/
C
C     ----- SYMMETRIZE HESSIAN MATRIX -----
C
      IF (NT .EQ. 1) RETURN
C
C     ----- READ IN TRANFORMATION MATRICES OF COORDINATES. -----
C
      CALL DAREAD(IDAF,IODA,PTR,432,7,0)
C
C     ----- LOOP OVER UNIQUE BLOCKS -----
C
      DO 3000 IC=1,NAT
      DO 110 IT=1,NT
      ICNEW=MAPCTR(IC,IT)
      IF(ICNEW.GT.IC) GO TO 3000
  110 CONTINUE
      DO 2000 JC=1,IC
      DO 120 IT=1,NT
      ICNEW=MAX(MAPCTR(IC,IT),MAPCTR(JC,IT))
      JCNEW=MIN(MAPCTR(IC,IT),MAPCTR(JC,IT))
      IF(ICNEW.GT.IC                ) GO TO 2000
      IF(ICNEW.EQ.IC.AND.JCNEW.GT.JC) GO TO 2000
  120 CONTINUE
C
C     ----- APPLY PROJECTION OPERATOR -----
C
      DO 210 J=1,3
      DO 210 I=1,3
  210 WH(I,J)=ZERO
      DO 400 IT = 1,NT
      ICNEW=MAX(MAPCTR(IC,IT),MAPCTR(JC,IT))
      JCNEW=MIN(MAPCTR(IC,IT),MAPCTR(JC,IT))
      IJC=(ICNEW*(ICNEW-1))/2+JCNEW
      IF(MAPCTR(JC,IT).GT.MAPCTR(IC,IT)) GO TO 230
      N=0
      DO 220 J=1,3
      DO 220 I=1,3
      N=N+1
  220 TH(I,J)=EH(N,IJC)
      GO TO 250
  230 N=0
      DO 240 I=1,3
      DO 240 J=1,3
      N=N+1
  240 TH(I,J)=EH(N,IJC)
  250 CONTINUE
      N=3*(IT-1)
      DO 260 J=1,3
      DO 260 I=1,3
  260 RH(I,J)=PTR(I,J+N)
      DO 280 J=1,3
      DO 280 I=1,3
      SUM=ZERO
      DO 270 K=1,3
  270 SUM=SUM+TH(I,K)*RH(K,J)
  280 WK(I,J)=SUM
      DO 300 J=1,3
      DO 300 I=1,3
      SUM=ZERO
      DO 290 K = 1,3
  290 SUM=SUM+RH(K,I)*WK(K,J)
  300 WH(I,J)=WH(I,J)+SUM
  400 CONTINUE
      DUM=ONE/NT
      N=0
      DO 410 J=1,3
      DO 410 I=1,3
      N=N+1
  410 WH(I,J)=WH(I,J)*DUM
C
C     ----- REMAP PROJECTED BLOCK ONTO EQUIVALENT BLOCKS -----
C
      DO 1000 IT=1,NT
      ICNEW=MAX(MAPCTR(IC,IT),MAPCTR(JC,IT))
      JCNEW=MIN(MAPCTR(IC,IT),MAPCTR(JC,IT))
      IJC=(ICNEW*(ICNEW-1))/2+JCNEW
      N=3*(INVT(IT)-1)
      DO 510 J=1,3
      DO 510 I=1,3
  510 RH(I,J)=PTR(I,J+N)
      DO 530 J=1,3
      DO 530 I=1,3
      SUM=ZERO
      DO 520 K=1,3
  520 SUM=SUM+WH(I,K)*RH(K,J)
  530 WK(I,J)=SUM
      DO 550 J=1,3
      DO 550 I=1,3
      SUM = ZERO
      DO 540 K=1,3
  540 SUM=SUM+RH(K,I)*WK(K,J)
  550 TH(I,J) = SUM
      IF(MAPCTR(JC,IT).GT.MAPCTR(IC,IT)) GO TO 570
      N=0
      DO 560 J=1,3
      DO 560 I=1,3
      N=N+1
  560 EH(N,IJC)=TH(I,J)
      GO TO 1000
  570 N=0
      DO 580 J=1,3
      DO 580 I=1,3
      N=N+1
  580 EH(N,IJC)=TH(J,I)
C
 1000 CONTINUE
C
 2000 CONTINUE
 3000 CONTINUE
C
      RETURN
      END
