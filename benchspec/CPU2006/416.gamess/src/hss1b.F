C  9 DEC 03 - TJD - SD,TVD: USE 1E- DER INTS FOR MCSCF HESSIANS
C  9 DEC 03 - OQ  - TVD,TVDSPD,SD,SDSPD,SDFIFJ,SDFSIJ,TVFIFJ:
C                   ADD ELECTRIC FIELD DERIVATIVES
C  4 NOV 03 - TJP - SDFIFJ,TVFIFJ: VECTORIZATION OFF (X1 COMPILER BUGS)
C 10 APR 00 - MWS - REMOVE STATIC MEMORY FROM COSMO COMMONS
C 25 MAR 00 - KKB/LNB - INCLUDE COSMO CHANGE
C  1 DEC 98 - BMB - MOVED DAWRIT OF EG AND EH TO MAIN DRIVER, STVDD
C 12 NOV 98 - MAF - SD: ALLOW FOR USE OF SPHERICAL HARMONICS
C 28 SEP 97 - MWS - CONVERT PARALLEL CALLS FROM TCGMSG TO DDI
C 17 OCT 96 - SPW - SD: CHANGES FOR CI GRADIENTS
C 13 JUN 96 - VAG - CHANGES TO INTRODUCE CITYP VARIABLE
C 11 NOV 94 - MWS - REMOVE FTNCHEK WARNINGS
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C  1 JUN 94 - MWS - TVDSPD,SDSPD,SHESS: PARALLEL CHANGES
C 16 JUL 93 - TLW - TVDSPD, SDSPD: MOVE NEXT VALUE TO OUTERMOST LOOP
C  6 NOV 92 - TLW - PARALLELIZE
C 22 MAR 92 - MWS - SHESS: USE DYNAMIC STORAGE FOR GRAD VECTORS
C 12 MAR 92 - MWS - REDIMENSION TO 500 ATOMS
C 10 JAN 92 - TLW - CHANGE REWINDS TO CALL SEQREW
C  6 JAN 92 - TLW - MAKE WRITES PARALLEL;ADD COMMON PAR
C  1 NOV 90 - MWS - ALLOW FOR DUMMY ATOMS IN SHESS
C  7 AUG 90 - TLW - ADD CF AND CG TO COMMON NSHEL
C 28 NOV 89 - MWS - NEW MODULE CREATED BY SPLITTING HSS1
C*MODULE HSS1B   *DECK SD
      SUBROUTINE SD(EXETYP,OUT,EG,EH,DAB,SDD,BSDD,NATM,L2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION MCSCF
C
      DIMENSION EG(3*NATM),EH(9*(NATM*NATM+NATM)/2),DAB(L2),SDD(*)
C
      LOGICAL OUT,BSDD
      LOGICAL GOPARR,DSKWRK,MASWRK,MCCI
C
      PARAMETER (MXGTOT=5000, MXSH=1000, MXATM=500)
C
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA MCSCF/8HMCSCF   /
#else
      CHARACTER*8 :: MCSCF_STR
      EQUIVALENCE (MCSCF, MCSCF_STR)
      DATA MCSCF_STR/"MCSCF   "/
#endif
C
C     ----- GET CORE MEMORY -----
C
      MCCI = SCFTYP.EQ.MCSCF
      CALL VALFM(LOADFM)
      NGOTMX=LIMFM-LOADFM
      L1 = NUM
      L2 = (NUM*NUM+NUM)/2
      L3 = NUM*NUM
C
      LSX    = 1     + LOADFM
      LSY    = LSX   + L2
      LSZ    = LSY   + L2
      LDSDX  = LSZ   + L2
      LDSDY  = LDSDX + L3
      LDSDZ  = LDSDY + L3
      LDRG   = LDSDZ + L3
      LLIMF  = LDRG  + NAT*NAT
      LLIMS  = LLIMF + NSHELL
      LDE    = LLIMS + NSHELL
      LEG3   = LDE   + NAT*3
      LAST   = LEG3  + NAT*3
      NEED   = LAST-LSX
      IF(NEED.GT.NGOTMX) THEN
         IF (MASWRK) WRITE(IW,*) 'IN SD, NEED,NGOT=',NEED,NGOTMX
         CALL ABRT
      END IF
      CALL GETFM(NEED)
C
C     ----- GET ENERGY-WEIGHTED DENSITY MATRIX -----
C
      IF(EXETYP.EQ.CHECK) THEN
         CALL VCLR(DAB,1,L2)
      ELSE
         CALL EIJDEN(DAB,X(LDSDX),X(LDSDY),X(LSX),X(LSY),
     *               L1,L2,L3,NQMT,X(LDSDY))
      END IF
C
C     ----- GET -S- DERIVATIVES -----
C
      CALL VCLR(X(LDSDX),1,L3)
      CALL VCLR(X(LDSDY),1,L3)
      CALL VCLR(X(LDSDZ),1,L3)
      IF(EXETYP.EQ.CHECK) GO TO 200
      CALL SDSPD(EG,EH,DAB,X(LSX),X(LSY),X(LSZ),
     *           X(LDSDX),X(LDSDY),X(LDSDZ),OUT,SDD,BSDD)
C
C         SAVE THE OVERLAP DERIVATIVE MATRICES
C
  200 CONTINUE
      CALL DAWRIT(IDAF,IODA,X(LDSDX),L3,63,0)
      CALL DAWRIT(IDAF,IODA,X(LDSDY),L3,64,0)
      CALL DAWRIT(IDAF,IODA,X(LDSDZ),L3,65,0)
C
C     ----- GET -S- CONTRIBUTION TO GRADIENT + HESSIAN -----
C
      CALL SHESS(DAB,X(LSX),L2,EG,EH,X(LDRG),NAT,OUT,X(LLIMF),
     *           X(LLIMS),KLOC,KATOM,NSHELL,X(LDE),X(LEG3),NAT)
C
      IF(MCCI) CALL DAWRIT(IDAF,IODA,X(LEG3),3*NAT,258,0)
      CALL RETFM(NEED)
      RETURN
      END
C*MODULE HSS1B   *DECK SDFIFJ
      SUBROUTINE SDFIFJ(GS,DS,NGS,DAB,NW,WX,WY,WZ,FOE)
      IMPLICIT DOUBLE PRECISION( A-H,O-Z)
      LOGICAL NONORM
C
      DIMENSION GS(NGS),DS(9),DAB(*),WX(NW),WY(NW),WZ(NW),FOE(36,9)
      DIMENSION MIF(25),MJF(25),MJS(25)
C
      COMMON /JDDSTV/ NFORB(4),NSORB(4),NFTABL(13,4),NSTABL(22,4),
     *                LFTABL(20,2),LSTABL(35,2)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /SHLOED/ LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJD,KLD,IJ,KL
C
      DATA ZERO,SQRT3 /0.0D+00,1.73205080756888D+00/
C
#if !defined(SPEC_CPU)
CDIR$ NOVECTOR
#endif
C
      IJFOE=1
C
C     ----- ZERO CLEAR OF WORKING ARREYS -----
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
      DO 10 I = 1,NW
      WX(I) = ZERO
      WY(I) = ZERO
   10 WZ(I) = ZERO
C
C     ----- SETTING INITIAL PARAMETERS -----
C
      NJ0  = MAXJ-MINJ+1
      NTYPJ= NJ0/2+1
      LTYPJ= NTYPJ/4+1
      NJ1  = NFORB(NTYPJ)
      DO 20 I = MINI,MAXI
   20 MIF(I) = 13 *(I-MINI) + 1
      DO 22 J = 1,NJ1
      NORBJ  = NFTABL(J,NTYPJ)
      NORBJ  = LFTABL(NORBJ,LTYPJ)
      MJS(J) = 13 * (NORBJ-1) + 1
   22 MJF(J) =       NORBJ-1
      DO 4100 J = 1,NJ1
      NF  = MJS(J)
      JF2 = MJF(J)
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
  340 X =            GS(NF+ 3)+GS(NF   )+GS(NF   )
      Y =            GS(NF+ 6)
      Z =            GS(NF+ 7)
      GO TO 400
  350 X =            GS(NF+ 8)
      Y =            GS(NF+ 4)+GS(NF+ 1)+GS(NF+ 1)
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
C
      DO 5100 J = MINJ,MAXJ
      JN = LOCJ + J
      DO 5000 I = MINI,MAXI
      IN = LOCI + I
      IF(JN .GT. IN) GO TO 500
      IJN = IN*(IN-1)/2 + JN
      GO TO 510
  500 IJN = JN*(JN-1)/2 + IN
  510 CONTINUE
      DENSTY = DAB(IJN)
      NF = MIF(I)
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
  700 CONTINUE
      DXX = DXX + DENSTY * XX
      DYX = DYX + DENSTY * YX
      DZX = DZX + DENSTY * ZX
      DXY = DXY + DENSTY * XY
      DYY = DYY + DENSTY * YY
      DZY = DZY + DENSTY * ZY
      DXZ = DXZ + DENSTY * XZ
      DYZ = DYZ + DENSTY * YZ
      DZZ = DZZ + DENSTY * ZZ
C
      FOE(IJFOE,1)=XX
      FOE(IJFOE,2)=YX
      FOE(IJFOE,3)=ZX
      FOE(IJFOE,4)=XY
      FOE(IJFOE,5)=YY
      FOE(IJFOE,6)=ZY
      FOE(IJFOE,7)=XZ
      FOE(IJFOE,8)=YZ
      FOE(IJFOE,9)=ZZ
      IJFOE=IJFOE+1
C
 5000 CONTINUE
 5100 CONTINUE
C
      DS(1) = DXX
      DS(2) = DYX
      DS(3) = DZX
      DS(4) = DXY
      DS(5) = DYY
      DS(6) = DZY
      DS(7) = DXZ
      DS(8) = DYZ
      DS(9) = DZZ
C
#if !defined(SPEC_CPU)
CDIR$ VECTOR
#endif
C
      RETURN
      END
C*MODULE HSS1B   *DECK SDFSIJ
      SUBROUTINE SDFSIJ(GF,GS,DS,NGF,NGS,NAO,DAB,WX,WY,WZ,FOE)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL NONORM
      LOGICAL FIRST,SECND,CPHF,BOTH,MFIRST,MSECND,MCPHF
C
      DIMENSION GF(NGF),GS(NGS),DS(9),DAB(*),WX(*),WY(*),WZ(*),
     *          FOE(36,9)
C
      COMMON /HSSPAR/ FIRST,SECND,CPHF,BOTH,MFIRST,MSECND,MCPHF
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /SHLOED/ LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJD,KLD,IJ,KL
C
      DATA ZERO,SQRT3      /0.0D+00,1.73205080756888D+00/
      DATA TWO,THREE,FIVE  /2.0D+00,3.0D+00,5.0D+00/
C
      IJFOE=1
C
C     ----- INITIAL SET OF PARAMETERS -----
C
      NONORM = NORMF .EQ. 1 .AND. NORMP .EQ. 1
      DXX = ZERO
      DYY = ZERO
      DZZ = ZERO
      DYX = ZERO
      DZX = ZERO
      DZY = ZERO
C
C     ----- CALCULATION OF FIRST AND SECOND DERIVATIVES USING -----
C     ----- TWO ELECTRON INTEGRALS                            -----
C
      DO 4100 J = MINJ,MAXJ
      JN = LOCJ + J
      NF = 13*(J-MINJ)+1
      NS = 22*(J-MINJ)+1
      DO 4000 I = MINI,MAXI
      IN = LOCI + I
      IF(JN .GT. IN) GO TO 200
      IJN = IN*(IN-1)/2 + JN
      GO TO 210
  200 IJN = JN*(JN-1)/2 + IN
  210 CONTINUE
      DENSTY = DAB(IJN)
      IJM = NAO * ( JN - 1 ) + IN
C
C     ----- TRANSFORMATION TO OBTAIN DERIVATIVES OF INTEGRALS -----
C
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
  400 CONTINUE
      WX(IJM) = X
      WY(IJM) = Y
      WZ(IJM) = Z
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
  800 CONTINUE
      DXX = DXX + DENSTY * XX
      DYY = DYY + DENSTY * YY
      DZZ = DZZ + DENSTY * ZZ
      DYX = DYX + DENSTY * YX
      DZX = DZX + DENSTY * ZX
      DZY = DZY + DENSTY * ZY
C
      FOE(IJFOE,1)=XX
      FOE(IJFOE,2)=YX
      FOE(IJFOE,3)=ZX
      FOE(IJFOE,4)=YX
      FOE(IJFOE,5)=YY
      FOE(IJFOE,6)=ZY
      FOE(IJFOE,7)=ZX
      FOE(IJFOE,8)=ZY
      FOE(IJFOE,9)=ZZ
      IJFOE=IJFOE+1
C
 3900 CONTINUE
C
 4000 CONTINUE
 4100 CONTINUE
      DS(1) = DXX
      DS(2) = DYX
      DS(3) = DZX
      DS(4) = DYX
      DS(5) = DYY
      DS(6) = DZY
      DS(7) = DZX
      DS(8) = DZY
      DS(9) = DZZ
      RETURN
      END
C*MODULE HSS1B   *DECK SDSPD
      SUBROUTINE SDSPD(EG,EH,DAB,SX,SY,SZ,WX,WY,WZ,OUT,SDD,BSDD)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL OUT,BSDD
      LOGICAL MASWRK,DSKWRK,GOPARR,NXT,
     *        FIRST,SECND,CPHF,BOTH,MFIRST,MSECND,MCPHF
C
      DIMENSION EG(3,*),EH(9,*),DAB(*),SX(*),SY(*),SZ(*),
     *          WX(*),WY(*),WZ(*),SDD(*)
      DIMENSION DIJ10(78),DIJ11(169),DIJ20(132),
     *          SIJ10(78),SIJ11(169),SIJ20(132),
     *          XIN(169),YIN(169),ZIN(169),DS(9),FOE(36,9)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXGSH=30, MXATM=500)
C
      COMMON /HSSPAR/ FIRST,SECND,CPHF,BOTH,MFIRST,MSECND,MCPHF
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /SHLOED/ LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJD,KLD,IJ,KL
      COMMON /XYZDER/ XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,
     *                NI,NJ,CX,CY,CZ
      COMMON /INFOED/ GA(MXGSH),CSA(MXGSH),CPA(MXGSH),CDA(MXGSH),
     1                GB(MXGSH),CSB(MXGSH),CPB(MXGSH),CDB(MXGSH),
     2                GC(MXGSH),CSC(MXGSH),CPC(MXGSH),CDC(MXGSH),
     3                GD(MXGSH),CSD(MXGSH),CPD(MXGSH),CDD(MXGSH),
     4                XA,YA,ZA,XB,YB,ZB,RR,XC,YC,ZC,XD,YD,ZD,RRC,
     5                NGA,NGB,NGC,NGD
      COMMON /DIJOED/ NADIJ1( 78),NADIJ2(169),NADIJ3(132)
      COMMON /IJXOED/ NPIJX1( 78),NPIJX2(169),NPIJX3(132)
      COMMON /IJYOED/ NPIJY1( 78),NPIJY2(169),NPIJY3(132)
      COMMON /IJZOED/ NPIJZ1( 78),NPIJZ2(169),NPIJZ3(132)
C
      DATA NFDIJ1,NFDIJ2,NFDIJ3,NW /  78, 169, 132, 169/
      DATA ONE /1.0D+00/
      DATA RLN10 /2.30258D+00/
C
C     ----- CALCULATE DERIVATIVES IF THE OVERLAP MATRIX -----
C
      TOL = RLN10*ITOL
      NRD=0
      IF(MFIRST) NRD = 1
      IF(MSECND) NRD = 2
C
      CALL STVJDD
C
C   INITIALIZATION FOR PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      MINE = -1
C
C     ----- I SHELL -----
C
      DO 780 II = 1,NSHELL
C
C           GO PARALLEL!
C
      IF (NXT .AND. GOPARR) THEN
         MINE = MINE + 1
         IF (MINE.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
         IF (NEXT.NE.MINE) GO TO 780
      END IF
C
      IAT = KATOM(II)
      XI = C(1,IAT)
      YI = C(2,IAT)
      ZI = C(3,IAT)
      XA = XI
      YA = YI
      ZA = ZI
      I1 = KSTART(II)
      I2 = I1+KNG(II)-1
      LIT = KTYPE(II) + NRD
      MINI = KMIN(II)
      MAXI = KMAX(II)
      LOCI = KLOC(II)-MINI
      NGA = 0
      DO 10 I = I1,I2
      NGA = NGA + 1
      GA( NGA) = EX(I)
      CSA(NGA) = CS(I)
      CPA(NGA) = CP(I)
      CDA(NGA) = CD(I)
   10 CONTINUE
C
C     ----- J SHELL -----
C
      DO 760 JJ = 1,NSHELL
C
C          GO PARALLEL!
C
      IF ((.NOT.NXT) .AND. GOPARR) THEN
         IPCOUNT = IPCOUNT + 1
         IF (MOD(IPCOUNT,NPROC).NE.0) GO TO 760
      END IF
C
      JAT = KATOM(JJ)
      XJ = C(1,JAT)
      YJ = C(2,JAT)
      ZJ = C(3,JAT)
      XB = XJ
      YB = YJ
      ZB = ZI
      J1 = KSTART(JJ)
      J2 = J1+KNG(JJ)-1
      LJT = KTYPE(JJ)
      IF(MSECND) LJT = LJT + 1
      MINJ = KMIN(JJ)
      MAXJ = KMAX(JJ)
      LOCJ = KLOC(JJ)-MINJ
      NGB = 0
      DO 20 J =J1,J2
      NGB = NGB + 1
      GB( NGB) = EX(J)
      CSB(NGB) = CS(J)
      CPB(NGB) = CP(J)
      CDB(NGB) = CD(J)
   20 CONTINUE
      RR = (XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS -----
C
      CALL STVIDX(0)
C
      CALL VCLR(SIJ10,1,NFDIJ1)
      CALL VCLR(SIJ11,1,NFDIJ2)
      CALL VCLR(SIJ20,1,NFDIJ3)
C
C     ----- I PRIMITIVE -----
C
      IIG = 0
      DO 640 IG = I1,I2
      IIG = IIG + 1
      AI = EX(IG)
      ARRI = AI*RR
      AXI = AI*XI
      AYI = AI*YI
      AZI = AI*ZI
      DUM = AI+AI
C
C     ----- J PRIMITIVE -----
C
      JJG = 0
      DO 620 JG = J1,J2
      JJG = JJG + 1
      AJ = EX(JG)
      AA = AI+AJ
      AA1 = ONE/AA
      DUM = AJ*ARRI*AA1
      IF (DUM .GT. TOL) GO TO 620
      AX = (AXI+AJ*XJ)*AA1
      AY = (AYI+AJ*YJ)*AA1
      AZ = (AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR -----
C
      CALL STVDIJ(0,IIG,JJG,NIJ10,NIJ11,NIJ20,
     1            DIJ10,DIJ11,DIJ20,XIN,YIN,ZIN,
     2            NFDIJ1,NFDIJ2,NFDIJ3)
C
C     ----- OVERLAP -----
C
      T =  SQRT(AA1)
      X0 = AX
      Y0 = AY
      Z0 = AZ
      IN = -4
      DO 580 I = 1,LIT
      IN = IN+4
      NI = I
      DO 580 J = 1,LJT
      JN = IN+J
      NJ = J
      CALL DERXYZ(0)
      XIN(JN) = XINT*T
      YIN(JN) = YINT*T
      ZIN(JN) = ZINT*T
  580 CONTINUE
      IF(.NOT.MFIRST) GO TO 590
      DO 40 I = 1,NIJ10
      N  = NADIJ1(I)
      NX = NPIJX1(I)
      NY = NPIJY1(I)
      NZ = NPIJZ1(I)
      SIJ10(N) = SIJ10(N) + DIJ10(I) * XIN(NX) * YIN(NY) * ZIN(NZ)
   40 CONTINUE
  590 IF(.NOT.MSECND) GO TO 600
      DO 41 I = 1,NIJ11
      N  = NADIJ2(I)
      NX = NPIJX2(I)
      NY = NPIJY2(I)
      NZ = NPIJZ2(I)
      SIJ11(N) = SIJ11(N) + DIJ11(I) * XIN(NX) * YIN(NY) * ZIN(NZ)
   41 CONTINUE
      DO 42 I = 1,NIJ20
      N  = NADIJ3(I)
      NX = NPIJX3(I)
      NY = NPIJY3(I)
      NZ = NPIJZ3(I)
      SIJ20(N) = SIJ20(N) + DIJ20(I) * XIN(NX) * YIN(NY) * ZIN(NZ)
   42 CONTINUE
  600 CONTINUE
C
C     ----- END OF PRIMITIVE LOOPS -----
C
  620 CONTINUE
  640 CONTINUE
C
C     ----- FORM INTEGRALS OVER DERIVATIVES -----
C
      CALL SDFSIJ(SIJ10,SIJ20,DS,NFDIJ1,NFDIJ3,NUM,DAB,WX,WY,WZ,FOE)
      IF(BSDD) CALL ADDFOE(SDD,IAT,IAT,FOE,ONE,ONE)
C
      IF(.NOT.MSECND) GO TO 700
      IIAT = IAT * (IAT - 1 ) / 2 + IAT
      DO 660 I = 1,9
  660 EH(I,IIAT) = EH(I,IIAT) + DS(I) + DS(I)
      IF(IAT .LT. JAT) GO TO 700
      CALL SDFIFJ(SIJ11,DS,NFDIJ2,DAB,NW,XIN,YIN,ZIN,FOE)
      IF(BSDD) CALL ADDFOE(SDD,IAT,JAT,FOE,ONE,ONE)
      IJAT = IAT * ( IAT - 1) / 2 + JAT
      DO 680 I = 1,9
  680 EH(I,IJAT) = EH(I,IJAT) + DS(I) + DS(I)
  700 CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
  760 CONTINUE
  780 CONTINUE
C
      IF (GOPARR.AND.NXT) CALL DDI_DLBRESET
C
C     ----- PRINT OUT PARTIAL RESULTS -----
C
      IF(OUT) THEN
         NEGH = 3*NAT+9*(NAT*(NAT+1))/2
         IF(GOPARR) CALL DDI_GSUMF(1625,EG,NEGH)
         IF(MASWRK) THEN
            WRITE(IW,9300)
            CALL HSSPRT(NAT,EG,EH)
         END IF
         IF(GOPARR) CALL DSCAL(NEGH,ONE/NPROC,EG,1)
      END IF
C
C          SUM OVERLAP DERIVATIVES. WX,WY,WZ ARE CONSECUTIVE STORAGE
C
      IF(GOPARR) CALL DDI_GSUMF(1610,WX,3*NUM*NUM)
C
C          TRIANGULAR STORAGE COPIES ARE NEEDED IN -SHESS-
C
      NS = 0
      DO 810 I = 1,NUM
         DO 800 J = 1,I
            IJ = NUM * ( J - 1) + I
            NS = NS + 1
            SX(NS) = WX(IJ)
            SY(NS) = WY(IJ)
            SZ(NS) = WZ(IJ)
  800    CONTINUE
  810 CONTINUE
C
      IF(OUT  .AND.  MASWRK) THEN
         WRITE(IW,9008)
         CALL PRTRI(SX,NUM)
         CALL PRTRI(SY,NUM)
         CALL PRTRI(SZ,NUM)
      END IF
      RETURN
C
 9008 FORMAT(' DERIVATIVES OF OVERLAP MATRIX ')
 9300 FORMAT(1X,'ENERGY GRADIENT AND HESSIAN IN -SDSPD-')
      END
C*MODULE HSS1B   *DECK SHESS
      SUBROUTINE SHESS(DD,SG,NDIM,EG,EH,DRG,MAT,OUT,
     *                 LIMINF,LIMSUP,KLOC,KATOM,NSH,DE,EG3,NATM)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL OUT,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500)
C
      DIMENSION EG(3,*),EH(9,*),DD(*),SG(NDIM,3),DRG(MAT,*),
     *          LIMINF(NSH),LIMSUP(NSH),KLOC(NSH),KATOM(NSH),
     *          DE(3,NATM),EG3(3,NATM)
      DIMENSION SDR(9)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      DATA ZERO,ONE,TWO,THREE /0.0D+00,1.0D+00,2.0D+00,3.0D+00/
C
C     ----- GRADIENT AND HESSIAN OF NUCLEAR REPULSION ENERGY -----
C     FIRST, SUM ALL 1E- INTEGRAL CONTRIBUTIONS TO GRADIENT AND HESSIAN
C
      IF (GOPARR) THEN
         NEGH = (3*NAT) + 9*((NAT*NAT+NAT)/2)
         CALL DDI_GSUMF(1615,EG,NEGH)
      END IF
C
C         SET UP BASIS FUNCTION RANGES
C
      LIMINF(1)=1
      LAT=1
      J=1
      DO 50 I=1,NSH
         IAT=KATOM(I)
         IF(IAT.EQ.LAT) GO TO 50
         LAT=IAT
         LIMSUP(J)=KLOC(I)-1
         J=J+1
         LIMINF(J)=KLOC(I)
   50 CONTINUE
      LIMSUP(J)=NUM
      IF(J.EQ.NAT) GO TO 100
C
C         TAKE CARE OF BASISLESS ATOMS THAT MIGHT BE AT THE END
C
      NEXT = J+1
      DO 60 IAT=NEXT,NAT
         LIMINF(IAT)=NUM+1
         LIMSUP(IAT)=NUM
   60 CONTINUE
C
C         SET UP DISTANCE MATRIX
C
  100 CONTINUE
      DRG(1,1) = ZERO
      DO 120 K = 2,NAT
      DRG(K,K) = ZERO
      K1 = K-1
      DO 120 L = 1,K1
      RKL = ZERO
      DO 110 I = 1,3
  110 RKL = RKL+(C(I,K)-C(I,L))**2
      DRG(K,L) = -ONE/RKL
  120 DRG(L,K) = SQRT(RKL)
C
C     ----- FIRST DERIVATIVE OF NUCLEAR REPULSION ENERGY -----
C
      DO 180 KK = 1,3
         DE(KK,1) = ZERO
         EG3(KK,1) = ZERO
         DO 140 K = 2,NAT
         DE(KK,K) = ZERO
         EG3(KK,K) = ZERO
         KINF = LIMINF(K)
         KSUP = LIMSUP(K)
         ZAK = ZAN(K)
         KM1 = K-1
         DO 140 L = 1,KM1
            LINF = LIMINF(L)
            LSUP = LIMSUP(L)
            ZAL = ZAN(L)
            PKL = (C(KK,K)-C(KK,L))/DRG(L,K)
            EG3(KK,K) = EG3(KK,K)+PKL*DRG(K,L)*ZAK*ZAL
            DO 140 I = KINF,KSUP
               DO 140 J = LINF,LSUP
                  IJ = (I*(I-1))/2+J
                  DE(KK,K) = DE(KK,K)-SG(IJ,KK)*DD(IJ)
  140    CONTINUE
         NAT1 = NAT-1
         DO 160 K = 1,NAT1
            ZAK = ZAN(K)
            KINF = LIMINF(K)
            KSUP = LIMSUP(K)
            KP1 = K+1
            DO 160 L = KP1,NAT
               ZAL = ZAN(L)
               LINF = LIMINF(L)
               LSUP = LIMSUP(L)
               PKL = (C(KK,K)-C(KK,L))/DRG(K,L)
               EG3(KK,K) = EG3(KK,K)+PKL*DRG(L,K)*ZAK*ZAL
               DO 160 I = KINF,KSUP
                  DO 160 J = LINF,LSUP
                     IJ = (J*(J-1))/2+I
                     DE(KK,K) = DE(KK,K)+SG(IJ,KK)*DD(IJ)
  160    CONTINUE
  180 CONTINUE
C
      DO 200 K = 1,NAT
      DO 200 KK = 1,3
  200 DE(KK,K) = TWO*DE(KK,K)
C
C     ----- ADD THESE CONTRIBUTIONS TO 1E- GRADIENT -----
C
      DO 220 I = 1,NAT
         DO 210 J = 1,3
            EG(J,I) = EG(J,I) + EG3(J,I) - DE(J,I)
  210    CONTINUE
  220 CONTINUE
C
C     ----- SECOND DERIVATIVE OF NUCLEAR REPULSION ENERGY -----
C
      DO 40 IAT = 1,NAT
      ZAI = ZAN(IAT)
      DO 40 JAT = 1,IAT
      ZAJ = ZAN(JAT)
      IF( IAT .EQ. JAT ) GO TO 40
      RX = C(1,IAT) - C(1,JAT)
      RY = C(2,IAT) - C(2,JAT)
      RZ = C(3,IAT) - C(3,JAT)
      RD = DRG(JAT,IAT)**5
      RXX = RX * RX
      RYY = RY * RY
      RZZ = RZ * RZ
      SDR(1) = ( TWO * RXX - RYY - RZZ )/RD
      SDR(5) = ( TWO * RYY - RZZ - RXX )/RD
      SDR(9) = ( TWO * RZZ - RXX - RYY )/RD
      SDR(2) = ( THREE * RX * RY )/RD
      SDR(4) = SDR(2)
      SDR(3) = ( THREE * RZ * RX )/RD
      SDR(7) = SDR(3)
      SDR(6) = ( THREE * RY * RZ )/RD
      SDR(8) = SDR(6)
      IIAT = IAT * ( IAT + 1 ) / 2
      IJAT = IAT * ( IAT - 1 ) / 2 + JAT
      JJAT = JAT * ( JAT + 1 ) / 2
      ZAIJ = ZAI * ZAJ
      DO 42 I = 1,9
   42 SDR(I) = ZAIJ * SDR(I)
      DO 44 I = 1,9
      EH(I,IIAT) = EH(I,IIAT) + SDR(I)
      EH(I,IJAT) = EH(I,IJAT) - SDR(I)
   44 EH(I,JJAT) = EH(I,JJAT) + SDR(I)
   40 CONTINUE
C
C     ----- PRINT SECTION -----
C
      IF(OUT  .AND.  MASWRK) THEN
         WRITE(IW,9010)
         CALL HSSPRT(NAT,EG,EH)
      END IF
      RETURN
C
 9010 FORMAT(/5X,33("-")/5X,'ONE ELECTRON GRADIENT AND HESSIAN'/
     *        5X,33(1H-)/)
      END
C*MODULE HSS1B   *DECK TVD
      SUBROUTINE TVD(EXETYP,NFT18,OUT,EG,EH,DAB,FD,FDD,BFDD,NATM,L2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION MCSCF
C
      DIMENSION EG(3*NATM),EH(9*(NATM*NATM+NATM)/2),DAB(L2),
     *          FD(3*NATM*L2),FDD(*)
C
      LOGICAL OUT,BFDD
      LOGICAL FIRST,SECND,CPHF,BOTH,MFIRST,MSECND,MCPHF,MCCI
C
      PARAMETER (MXATM=500)
C
      COMMON /HSSPAR/ FIRST,SECND,CPHF,BOTH,MFIRST,MSECND,MCPHF
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA MCSCF/8HMCSCF   /
#else
      CHARACTER*8 :: MCSCF_STR
      EQUIVALENCE (MCSCF, MCSCF_STR)
      DATA MCSCF_STR/"MCSCF   "/
#endif
C
      IF(EXETYP.EQ.CHECK) RETURN
      MCCI = SCFTYP.EQ.MCSCF
C
C     ----- GET -TV- DERIVATIVES -----
C
      CALL TVDSPD(EG,EH,DAB,FD,FDD,BFDD,OUT)
C
C        SAVE THE COMPLETED FOCK DERIVATIVES
C
      IF(MCCI  .OR.  MCPHF) THEN
         CALL SEQREW(NFT18)
         MFD=3*NAT
         IX=1
         DO 40 I=1,MFD
            CALL SQWRIT(NFT18,FD(IX),L2)
            IX=IX+L2
   40    CONTINUE
         CALL SEQREW(NFT18)
      END IF
C
      RETURN
      END
C*MODULE HSS1B   *DECK TVDSPD
      SUBROUTINE TVDSPD(EG,EH,DAB,FD,FDD,BFDD,OUT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION FDD(*)
      DOUBLE PRECISION FOE(36,9)
C
      LOGICAL DBUG,OUT,GOPARR,DSKWRK,MASWRK,NXT
      LOGICAL FIRST,SECND,CPHF,BOTH,MFIRST,MSECND,MCPHF
      LOGICAL BFDD
C
      DIMENSION EG(3,*),EH(9,*),DAB(*),FD(*)
      DIMENSION DIJ10( 78),DIJ11(169),DIJ20(132),
     1          FFT10( 78),FFT11(169),FFT20(132),
     2          GIJ10( 78),GIJ11(169),GIJ20(132),
     3          XIN(   80),YIN(   80),ZIN(   80)
      DIMENSION DF(3),DS(9,2),DOE(108)
C
      PARAMETER (MXGTOT=5000, MXSH=1000, MXGSH=30, MXATM=500)
C
      COMMON /HSSPAR/ FIRST,SECND,CPHF,BOTH,MFIRST,MSECND,MCPHF
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RESTAR/ TIMLIM,NREST,NREC,INTLOC,IST,JST,KST,LST
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /SHLOED/ LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJD,KLD,IJ,KL
      COMMON /XYZDER/ XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,
     *                NI,NJ,CX,CY,CZ
      COMMON /INFOED/ GA(MXGSH),CSA(MXGSH),CPA(MXGSH),CDA(MXGSH),
     *                GB(MXGSH),CSB(MXGSH),CPB(MXGSH),CDB(MXGSH),
     *                GC(MXGSH),CSC(MXGSH),CPC(MXGSH),CDC(MXGSH),
     *                GD(MXGSH),CSD(MXGSH),CPD(MXGSH),CDD(MXGSH),
     *                XA,YA,ZA,XB,YB,ZB,RR,XC,YC,ZC,XD,YD,ZD,RRC,
     *                NGA,NGB,NGC,NGD
      COMMON /DIJOED/ NADIJ1( 78),NADIJ2(169),NADIJ3(132)
      COMMON /IJXOED/ NPIJX1( 78),NPIJX2(169),NPIJX3(132)
      COMMON /IJYOED/ NPIJY1( 78),NPIJY2(169),NPIJY3(132)
      COMMON /IJZOED/ NPIJZ1( 78),NPIJZ2(169),NPIJZ3(132)
C
C     COSMO CHANGES 
C
      PARAMETER (MAXDEN=25*MXATM, LENABC=2000, NPPA=1082)
      LOGICAL ISEPS,USEPS
      COMMON /ISEPS / ISEPS,USEPS
      COMMON /SOLV  / FEPSI,RDS,DISEX2,COSURF(3,LENABC),SRAD(MXATM),
     *                QDEN(MAXDEN),AR(LENABC),
     *                NSPA,NPS,NPS2,NDEN,NPSPHER
      COMMON /COSMO3/ COSZAN(NPPA),CORZAN(3,NPPA)
C
      DATA PI212 /1.1283791670955D+00/
      DATA ZERO,PT5,ONE,TWO /0.0D+00,0.5D+00,1.0D+00,2.0D+00/
      DATA RLN10 /2.30258D+00/
      DATA NFDIJ1,NFDIJ2,NFDIJ3,NW / 78,169,132, 80/
C
      DBUG=.FALSE. .AND. MASWRK
      IF (OUT  .AND.  MASWRK) WRITE (IW,9008)
      TOL = RLN10*ITOL
      NRD=0
      IF(MFIRST) NRD = 1
      IF(MSECND) NRD = 2
      L2=(NUM*(NUM+1))/2
C
      CALL STVJDD
C
C INITIALIZE PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      MINE = -1
C
C     ----- I SHELL -----
C
      DO 1060 II = 1,NSHELL
C
C     ----- GO PARALLEL! -----
C
      IF (NXT .AND. GOPARR) THEN
         MINE = MINE + 1
         IF (MINE.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
         IF (NEXT.NE.MINE) GO TO 1060
      END IF
C
      IAT = KATOM(II)
      IFDX  = 3*L2*(IAT-1)
      IFDY  = IFDX + L2
      IFDZ  = IFDY + L2
      XI = C(1,IAT)
      YI = C(2,IAT)
      ZI = C(3,IAT)
      XA = XI
      YA = YI
      ZA = ZI
      I1 = KSTART(II)
      I2 = I1+KNG(II)-1
      LIT = KTYPE(II) + NRD
      MINI = KMIN(II)
      MAXI = KMAX(II)
      LOCI = KLOC(II)-MINI
      NGA = 0
      DO 10 I = I1,I2
      NGA = NGA + 1
      GA( NGA) = EX(I)
      CSA(NGA) = CS(I)
      CPA(NGA) = CP(I)
      CDA(NGA) = CD(I)
   10 CONTINUE
C
C     ----- J SHELL -----
C
      DO 1040 JJ = 1,NSHELL
C
C           GO PARALLEL!
C
      IF ((.NOT.NXT) .AND. GOPARR) THEN
         IPCOUNT = IPCOUNT + 1
         IF (MOD(IPCOUNT,NPROC).NE.0) GO TO 1040
      END IF
C
      JAT = KATOM(JJ)
      XJ = C(1,JAT)
      YJ = C(2,JAT)
      ZJ = C(3,JAT)
      XB = XJ
      YB = YJ
      ZB = ZJ
      J1 = KSTART(JJ)
      J2 = J1+KNG(JJ)-1
      LJT = KTYPE(JJ)
      IF(MSECND) LJT = LJT + 1
      MINJ = KMIN(JJ)
      MAXJ = KMAX(JJ)
      LOCJ = KLOC(JJ)-MINJ
      NGB = 0
      DO 20 J = J1,J2
      NGB = NGB + 1
      GB( NGB) = EX(J)
      CSB(NGB) = CS(J)
      CPB(NGB) = CP(J)
      CDB(NGB) = CD(J)
   20 CONTINUE
      IF(MFIRST) NROOTS = (LIT+LJT-2  )/2 + 1
      IF(MSECND) NROOTS = (LIT+LJT-2  )/2 + 1
      RR = (XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      CALL STVIDX(0)
C
      CALL VCLR(GIJ10,1,NFDIJ1)
      CALL VCLR(GIJ11,1,NFDIJ2)
      CALL VCLR(GIJ20,1,NFDIJ3)
C
C     ----- I PRIMITIVE
C
      IIG = 0
      DO 840 IG = I1,I2
      IIG = IIG + 1
      AI = EX(IG)
      ARRI = AI*RR
      AXI = AI*XI
      AYI = AI*YI
      AZI = AI*ZI
      DUM = AI+AI
C
C     ----- J PRIMITIVE
C
      JJG = 0
      DO 820 JG = J1,J2
      JJG = JJG + 1
      AJ = EX(JG)
      AA = AI+AJ
      AA1 = ONE/AA
      DUM = AJ*ARRI*AA1
      IF (DUM .GT. TOL) GO TO 820
C
      AX = (AXI+AJ*XJ)*AA1
      AY = (AYI+AJ*YJ)*AA1
      AZ = (AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR
C
      CALL STVDIJ(0,IIG,JJG,NIJ10,NIJ11,NIJ20,
     1            DIJ10,DIJ11,DIJ20,FFT10,FFT11,FFT20,
     2            NFDIJ1,NFDIJ2,NFDIJ3)
C
C     -----  KINETIC ENERGY -----
C
      T =  SQRT(AA1)
      T1 = -TWO*AJ*AJ*T
      T2 = -PT5*T
      X0 = AX
      Y0 = AY
      Z0 = AZ
      IN = -4
      DO 660 I = 1,LIT
      IN = IN+4
      NI = I
      DO 660 J = 1,LJT
      JN = IN+J
      NJ = J
      CALL DERXYZ(0)
      XIN(JN) = XINT*T
      YIN(JN) = YINT*T
      ZIN(JN) = ZINT*T
      NJ = J+2
      CALL DERXYZ(0)
      XIN(JN+20) = XINT*T1
      YIN(JN+20) = YINT*T1
      ZIN(JN+20) = ZINT*T1
      NJ = J-2
      IF (NJ .GT. 0) GO TO 620
      XINT = ZERO
      YINT = ZERO
      ZINT = ZERO
      GO TO 640
  620 CALL DERXYZ(0)
  640 N = (J-1)*(J-2)
      DUM = N*T2
      XIN(JN+40) = XINT*DUM
      YIN(JN+40) = YINT*DUM
      ZIN(JN+40) = ZINT*DUM
  660 CONTINUE
C
      IF(MFIRST) THEN
         DO 40 I = 1,NIJ10
            N  = NADIJ1(I)
            NX = NPIJX1(I)
            NY = NPIJY1(I)
            NZ = NPIJZ1(I)
            YZ = YIN(NY)*ZIN(NZ)
            DUM0     = YZ*XIN(NX)
            DUM1     = (XIN(NX+20)+XIN(NX+40)) * YZ
     1               + (YIN(NY+20)+YIN(NY+40)) * XIN(NX) * ZIN(NZ)
     2               + (ZIN(NZ+20)+ZIN(NZ+40)) * XIN(NX) * YIN(NY)
            GIJ10(N) = GIJ10(N) + DIJ10(I) * (DUM0*FFT10(I)*AJ+DUM1)
   40    CONTINUE
      END IF
C
      IF(MSECND) THEN
         IF(JJ .GT. II ) GO TO 685
         DO 41 I = 1,NIJ11
            N  = NADIJ2(I)
            NX = NPIJX2(I)
            NY = NPIJY2(I)
            NZ = NPIJZ2(I)
            YZ = YIN(NY)*ZIN(NZ)
            DUM0     = YZ*XIN(NX)
            DUM1     = (XIN(NX+20)+XIN(NX+40)) * YZ
     1               + (YIN(NY+20)+YIN(NY+40)) * XIN(NX) * ZIN(NZ)
     2               + (ZIN(NZ+20)+ZIN(NZ+40)) * XIN(NX) * YIN(NY)
            GIJ11(N) = GIJ11(N) + DIJ11(I) * (DUM0*FFT11(I)*AJ+DUM1)
   41    CONTINUE
  685    CONTINUE
         DO 42 I = 1,NIJ20
            N  = NADIJ3(I)
            NX = NPIJX3(I)
            NY = NPIJY3(I)
            NZ = NPIJZ3(I)
            YZ = YIN(NY)*ZIN(NZ)
            DUM0     = YZ*XIN(NX)
            DUM1     = (XIN(NX+20)+XIN(NX+40)) * YZ
     1               + (YIN(NY+20)+YIN(NY+40)) * XIN(NX) * ZIN(NZ)
     2               + (ZIN(NZ+20)+ZIN(NZ+40)) * XIN(NX) * YIN(NY)
            GIJ20(N) = GIJ20(N) + DIJ20(I) * (DUM0*FFT20(I)*AJ+DUM1)
   42    CONTINUE
      END IF
C
C     ----- NUCLEAR ATTRACTION -----
C
      DUM = PI212*AA1
      IF(MFIRST) THEN
         DO 50 I = 1,NIJ10
   50    DIJ10(I) = DIJ10(I) * DUM
      END IF
      IF(MSECND) THEN
         DO 51 I = 1,NIJ11
   51    DIJ11(I) = DIJ11(I) * DUM
         DO 52 I = 1,NIJ20
   52    DIJ20(I) = DIJ20(I) * DUM
      END IF
C
      AAX = AA*AX
      AAY = AA*AY
      AAZ = AA*AZ
C
      ICMAX = NAT
      IF(ISEPS) ICMAX=NAT+NPS
C
      DO 800 IC = 1,ICMAX
      IF(IC.LE.NAT) THEN
         ZNUC = -ZAN(IC)
         CX = C(1,IC)
         CY = C(2,IC)
         CZ = C(3,IC)
      ELSE
C                   COSMO PARTIAL CHARGES
         ZNUC = -COSZAN(IC-NAT)
         CX = CORZAN(1,IC-NAT)
         CY = CORZAN(2,IC-NAT)
         CZ = CORZAN(3,IC-NAT)
      ENDIF
C
      XX = AA*((AX-CX)**2+(AY-CY)**2+(AZ-CZ)**2)
      IF(NROOTS .LE. 3) CALL RT123
      IF(NROOTS .EQ. 4) CALL ROOT4
      MM = 0
      DO 740 K = 1,NROOTS
      UU = AA*U(K)
      WW = W(K)*ZNUC
      TT = ONE/(AA+UU)
      T =  SQRT(TT)
      X0 = (AAX+UU*CX)*TT
      Y0 = (AAY+UU*CY)*TT
      Z0 = (AAZ+UU*CZ)*TT
      IN = -4+MM
      DO 720 I = 1,LIT
      IN = IN+4
      NI = I
      DO 720 J = 1,LJT
      JN = IN+J
      NJ = J
      CALL DERXYZ(0)
      XIN(JN) = XINT
      YIN(JN) = YINT
      ZIN(JN) = ZINT*WW
  720 CONTINUE
  740 MM = MM+20
C
      IF(MFIRST) THEN
         DO 60 I = 1,NIJ10
            N  = NADIJ1(I)
            NX = NPIJX1(I)
            NY = NPIJY1(I)
            NZ = NPIJZ1(I)
            DUM = ZERO
            MM  = 0
            DO 61 K = 1,NROOTS
               DUM = DUM+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM)
               MM = MM+20
   61       CONTINUE
            GIJ10(N) = GIJ10(N) + DUM*DIJ10(I)
   60    CONTINUE
      END IF
C
      IF(MSECND) THEN
         IF(JJ .GT. II ) GO TO 790
         DO 62 I =1,NIJ11
            N  = NADIJ2(I)
            NX = NPIJX2(I)
            NY = NPIJY2(I)
            NZ = NPIJZ2(I)
            DUM = ZERO
            MM  = 0
            DO K = 1,NROOTS
               DUM = DUM+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM)
               MM = MM+20
            ENDDO
            GIJ11(N) = GIJ11(N) + DUM*DIJ11(I)
   62    CONTINUE
  790    CONTINUE
         DO 64 I =1,NIJ20
            N  = NADIJ3(I)
            NX = NPIJX3(I)
            NY = NPIJY3(I)
            NZ = NPIJZ3(I)
            DUM = ZERO
            MM  = 0
            DO 65 K = 1,NROOTS
               DUM = DUM+XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM)
               MM = MM+20
   65       CONTINUE
            GIJ20(N) = GIJ20(N) + DUM*DIJ20(I)
   64    CONTINUE
      END IF
  800 CONTINUE
C
C     ----- END OF PRIMITIVE LOOPS -----
C
  820 CONTINUE
  840 CONTINUE
C
C     ----- FORM INTEGRALS OVER DERIVATIVES -----
C
      DO 70 I = 1,9
   70 DS(I,2) = ZERO
      CALL TVFSIJ(GIJ10,GIJ20,DF,DS(1,1),DOE,NFDIJ1,NFDIJ3,DAB,FOE)
      IF(.NOT.MCPHF) GO TO 846
      N = -3
      DO 75 J = MINJ,MAXJ
      NJ = LOCJ + J
      DO 75 I = MINI,MAXI
      NI = LOCI + I
      IF(NI .GE. NJ) GO TO 842
      NIJ = NJ * (NJ-1)/2 + NI
      GO TO 844
  842 NIJ = NI * (NI-1)/2 + NJ
  844 N = N + 3
      FAC = ONE
      IF(NI .EQ. NJ) FAC = TWO
      FD(IFDX+NIJ) = FD(IFDX+NIJ) + FAC * DOE(N+1)
      FD(IFDY+NIJ) = FD(IFDY+NIJ) + FAC * DOE(N+2)
      FD(IFDZ+NIJ) = FD(IFDZ+NIJ) + FAC * DOE(N+3)
   75 CONTINUE
  846 CONTINUE
      IAT = KATOM(II)
      JAT = KATOM(JJ)
      FAC = ONE
      IF(IAT .EQ. JAT .AND. II .NE. JJ) FAC = TWO
      IF(BFDD) CALL ADDFOE(FDD,IAT,IAT,FOE,ONE,TWO)
C
      IF(.NOT.MSECND .OR. JJ .GT. II) GO TO 850
         CALL TVFIFJ(GIJ11,DS(1,2),XIN,YIN,ZIN,NW,NFDIJ2,DAB,FOE)
         IF(BFDD) CALL ADDFOE(FDD,IAT,JAT,FOE,FAC,TWO)
  850 CONTINUE
C
C     ----- CALCULATE CONTRIBUTION TO GRADIENT AND FORCE CONSTANTS -----
C
      IAT = KATOM(II)
      JAT = KATOM(JJ)
      FAC = ONE
      IF(IAT .EQ. JAT .AND. II .NE. JJ) FAC = TWO
      IF(JAT .GT. IAT) GO TO 860
      IIAT = IAT*(IAT-1)/2 + IAT
      IJAT = IAT*(IAT-1)/2 + JAT
      GO TO 865
  860 IIAT = IAT*(IAT-1)/2 + IAT
      IJAT = JAT*(JAT-1)/2 + IAT
  865 CONTINUE
      DO 80 I = 1,3
   80 EG(I, IAT) = EG(I, IAT) + DF(I)
      IF(MSECND) THEN
         DO 81 I = 1,9
            EH(I,IIAT) = EH(I,IIAT) + DS(I,1)
            EH(I,IJAT) = EH(I,IJAT) + DS(I,2) * FAC
   81    CONTINUE
         IF(DBUG) WRITE(IW,9028) II,JJ
         IF(DBUG) CALL HSSPRT(NAT,EG,EH)
      END IF
 1040 CONTINUE
 1060 CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
      IF (GOPARR) THEN
          IF(NXT) CALL DDI_DLBRESET
          CALL DDI_GSUMF(1605,FD,3*NAT*L2)
      END IF
C
      IF(MSECND) THEN
         DO 1080 IAT = 1,NAT
            IIAT = ( IAT * (IAT-1) ) / 2 + IAT
            FAC  = ( EH(2,IIAT) + EH(4,IIAT) ) / TWO
            EH(2,IIAT) = FAC
            EH(4,IIAT) = FAC
            FAC  = ( EH(3,IIAT) + EH(7,IIAT) ) / TWO
            EH(3,IIAT) = FAC
            EH(7,IIAT) = FAC
            FAC  = ( EH(6,IIAT) + EH(8,IIAT) ) / TWO
            EH(6,IIAT) = FAC
            EH(8,IIAT) = FAC
 1080    CONTINUE
      END IF
C
C     ----- PRINTING SECTION -----
C
      IF(OUT) THEN
         NEGH = 3*NAT+9*(NAT*(NAT+1))/2
         IF(GOPARR) CALL DDI_GSUMF(1624,EG,NEGH)
         IF(MASWRK) THEN
            WRITE(IW,9058)
            CALL HSSPRT(NAT,EG,EH)
         END IF
         IF(GOPARR) CALL DSCAL(NEGH,ONE/NPROC,EG,1)
C
         IF(MCPHF) THEN
            IF(MASWRK) WRITE(IW,9068)
            DO 1100 IC=1,NAT
               IF (MASWRK) WRITE(IW,9048) IC
               IFDX=3*L2*(IC-1)+1
               IFDY=IFDX+L2
               IFDZ=IFDY+L2
               CALL PRTRI(FD(IFDX),NUM)
               CALL PRTRI(FD(IFDY),NUM)
               CALL PRTRI(FD(IFDZ),NUM)
 1100       CONTINUE
         END IF
      END IF
C
C     ----- SET UP RESTART -----
C
      NREST = 4
      IST = 1
      JST = 1
      KST = 1
      LST = 1
      RETURN
C
 9008 FORMAT(/5X,50("-")/
     *        5X,'1E- INTEGRAL CONTRIBUTIONS TO GRADIENT AND HESSIAN'/
     *        5X,50(1H-))
 9028 FORMAT(' SHELLS II,JJ = ',2I5)
 9048 FORMAT(1X,'CONTRIBUTION TO FOCK DERIVATIVE MATRIX, ATOM= ',I5)
 9058 FORMAT(/1X,'GRADIENT AND HESSIAN AFTER -TVDSPD-')
 9068 FORMAT(/1X,'FOCK DERIVATIVES AFTER -TVDSPD-')
      END
C*MODULE HSS1B   *DECK TVFIFJ
      SUBROUTINE TVFIFJ(GS,DS,WX,WY,WZ,NW,NGS,DAB,FOE)
C
      IMPLICIT DOUBLE PRECISION( A-H,O-Z)
C
      LOGICAL NONORM
C
      DIMENSION GS(NGS),WX(NW),WY(NW),WZ(NW),DS(9),DAB(*),FOE(36,9)
      DIMENSION MIF(25),MJF(25),MJS(25)
C
      COMMON /JDDSTV/ NFORB(4),NSORB(4),NFTABL(13,4),NSTABL(22,4),
     *                LFTABL(20,2),LSTABL(35,2)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /SHLOED/ LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJD,KLD,IJ,KL
C
      DATA ZERO,TWO,SQRT3 /0.0D+00,2.0D+00,1.73205080756888D+00/
C
#if !defined(SPEC_CPU)
CDIR$ NOVECTOR
#endif
C
      IJFOE=1
C
C     ----- ZERO CLEAR OF WORKING ARREYS -----
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
      DO 10 I = 1,NW
      WX(I) = ZERO
      WY(I) = ZERO
   10 WZ(I) = ZERO
C
C     ----- SETTING INITIAL PARAMETERS -----
C
      NJ0  = MAXJ-MINJ+1
      NTYPJ= NJ0/2+1
      LTYPJ= NTYPJ/4+1
      NJ1  = NFORB(NTYPJ)
      DO 20 I = MINI,MAXI
   20 MIF(I) = 13 *(I-MINI) + 1
      DO 22 J = 1,NJ1
      NORBJ  = NFTABL(J,NTYPJ)
      NORBJ  = LFTABL(NORBJ,LTYPJ)
      MJS(J) = 13 * (NORBJ-1) + 1
   22 MJF(J) =       NORBJ-1
      DO 4100 J = 1,NJ1
      NF  = MJS(J)
      JF2 = MJF(J)
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
  340 X =            GS(NF+ 3)+GS(NF   )+GS(NF   )
      Y =            GS(NF+ 6)
      Z =            GS(NF+ 7)
      GO TO 400
  350 X =            GS(NF+ 8)
      Y =            GS(NF+ 4)+GS(NF+1)+GS(NF+1)
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
      DO 5100 J = MINJ,MAXJ
      JN = LOCJ + J
      DO 5000 I = MINI,MAXI
      IN = LOCI + I
      IF(JN .GT. IN) GO TO 500
      IJN = IN*(IN-1)/2 + JN
      GO TO 510
  500 IJN = JN*(JN-1)/2 + IN
  510 DENSTY = TWO * DAB(IJN)
C
      NF = MIF(I)
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
C
      FOE(IJFOE,1) = XX
      FOE(IJFOE,2) = YX
      FOE(IJFOE,3) = ZX
      FOE(IJFOE,4) = XY
      FOE(IJFOE,5) = YY
      FOE(IJFOE,6) = ZY
      FOE(IJFOE,7) = XZ
      FOE(IJFOE,8) = YZ
      FOE(IJFOE,9) = ZZ
      IJFOE=IJFOE+1
C
 5000 CONTINUE
 5100 CONTINUE
      DS(1) = DXX
      DS(2) = DYX
      DS(3) = DZX
      DS(4) = DXY
      DS(5) = DYY
      DS(6) = DZY
      DS(7) = DXZ
      DS(8) = DYZ
      DS(9) = DZZ
C
#if !defined(SPEC_CPU)
CDIR$ VECTOR
#endif
C
      RETURN
      END
C*MODULE HSS1B   *DECK TVFSIJ
      SUBROUTINE TVFSIJ(GF,GS,DF,DS,DOE,NGF,NGS,DAB,FOE)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL NONORM
      LOGICAL FIRST,SECND,CPHF,BOTH,MFIRST,MSECND,MCPHF
C
      DIMENSION DAB(*),GF(NGF),GS(NGS),DF(3),DS(9),DOE(108),
     *          FOE(36,9)
C
      COMMON /HSSPAR/ FIRST,SECND,CPHF,BOTH,MFIRST,MSECND,MCPHF
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /SHLOED/ LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJD,KLD,IJ,KL
C
      DATA ZERO,SQRT3 /0.0D+00,1.73205080756888D+00/
      DATA TWO,THREE,FIVE /2.0D+00,3.0D+00,5.0D+00/
C
      IJFOE=1
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
      IJM = -3
C
C     ----- CALCULATION OF FIRST AND SECOND DERIVATIVES USING -----
C     ----- TWO ELECTRON INTEGRALS                            -----
C
      DO 4100 J = MINJ,MAXJ
      JN = LOCJ + J
      NF = 13*(J-MINJ) + 1
      NS = 22*(J-MINJ) + 1
      DO 4000 I = MINI,MAXI
      IN = LOCI + I
      IF(JN .GT. IN) GO TO 200
      IJN = IN*(IN-1)/2 + JN
      GO TO 210
  200 IJN = JN*(JN-1)/2 + IN
  210 DENSTY = TWO * DAB(IJN)
      IJM = IJM + 3
      IF(.NOT.FIRST) GO TO 500
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
  400 CONTINUE
      DX  =   DX +   X*DENSTY
      DY  =   DY +   Y*DENSTY
      DZ  =   DZ +   Z*DENSTY
      DOE(IJM+1) = X
      DOE(IJM+2) = Y
      DOE(IJM+3) = Z
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
  800 CONTINUE
      DXX = DXX   +  XX*DENSTY
      DYY = DYY   +  YY*DENSTY
      DZZ = DZZ   +  ZZ*DENSTY
      DYX = DYX   +  YX*DENSTY
      DZX = DZX   +  ZX*DENSTY
      DZY = DZY   +  ZY*DENSTY
C
      FOE(IJFOE,1) = XX
      FOE(IJFOE,2) = YX
      FOE(IJFOE,3) = ZX
      FOE(IJFOE,4) = YX
      FOE(IJFOE,5) = YY
      FOE(IJFOE,6) = ZY
      FOE(IJFOE,7) = ZX
      FOE(IJFOE,8) = ZY
      FOE(IJFOE,9) = ZZ
      IJFOE=IJFOE+1
C
 3900 CONTINUE
 4000 CONTINUE
 4100 CONTINUE
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
