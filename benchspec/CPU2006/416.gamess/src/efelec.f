C  3 JUL 03 - MWS - DCHQUA,DCHOCT,DDPQUD,DQDQD: PASS IN WORKING STORAGE
C  7 AUG 02 - IA  - CHANGES TO MULTIPOLE SCREENING THROUGHOUT
C 17 APR 02 - MWS - SYNCH UP EFMULT AND FRGINF COMMON
C 13 JUN 01 - MAF - CHGCHG,CHGDIP: REMOVE CHARGE PENETRATION CALCULATION
C 18 SEP 96 - MWS - ELIMINATE FRAG-FRAG REPULSION COMMON
C 14 SEP 96 - BMB - CHGCHG,DCHCH: USE ERRF INSTEAD OF DERF
C 10 SEP 96 - MWS - INCLUDE THIS CODE IN DISTRIBUTION VERSION
C 13 JUN 96 - MWS - DELETE DEAD CODE PRNT,CHGIND,INDIND,QUDIND
C 24 MAY 96 - WC  - REMOVE EFPEX IN COMMOM/EFPPAR/
C 12 SEP 95 - WC  - EREPUL,DREPUL: READ FRAGMENT-FRAGMENT INTERACTION
C  6 JAN 95 - PND - CHANGES FOR UNSYMMETRIC POLARIZABILITY TENSORS
C 23 NOV 94 - MWS - REMOVE ALL FTNCHEK ERRORS
C 15 JUL 93 - PND - MODULE FOR INTERFRAGMENT ELECTROSTATICS
C
C CHGCHG - CHARGE-CHARGE INTERACTION ENERGY
C PRNT  -  ROUTINE FOR DEBUG PRINTING
C ELENER - "PARENT" ELECTROSTATIC CODE, FROM WALT STEVENS
C CHGDIP - CHARGE-DIPOLE INTERACTION ENERGY
C CHGQUA - CHARGE-QUADRUPOLE INTERACTION ENERGY
C CHGOCT - CHARGE-OCTUPOLE INTERACTION ENERGY
C DPLDPL - DIPOLE-DIPOLE INTERACTION ENERGY
C DPQUAD - DIPOLE-QUADRUPOLE INTERACTION ENERGY
C QUDQUD - QUADRUPOLE-QUADRUPOLE INTERACTION ENERGY
C DCHCH  - GRADIENT OF CHARGE-CHARGE INTERACTION ENERGY
C DCHDIP - GRADIENT OF CHARGE-DIPOLE INTERACTION ENERGY
C DDPDP  - GRADIENT OF DIPOLE-DIPOLE INTERACTION ENERGY
C DCHQUA - GRADIENT OF CHARGE-QUADRUPOLE INTERACTION ENERGY
C DCHOCT - GRADIENT OF CHARGE-OCTUPOLE INTERACTION ENERGY
C DDPQUD - GRADIENT OF DIPOLE-QUADRUPOLE INTERACTION ENERGY
C DQDQD  - GRADIENT OF QUADRUPOLE-QUADRUPOLE ENERGY
C CHGIND - CHARGE-INDUCED DIPOLE INTERACTION ENERGY
C DCHIND - GRADIENT OF CHARGE-INDUCED DIPOLE INTERACTION ENERGY
C INDIND - INDUCED DIPOLE-INDUCED DIPOLE INTERACTION ENERGY
C DININ - GRADIENT OF INDUCED DIPOLE-INDUCED DIPOLE INTERACTION ENERGY
C DPLIND - DIPOLE-INDUCED DIPOLE INTERACTION ENERGY
C DDPIND - GRADIENT OF DIPOLE-INDUCED DIPOLE INTERACTION ENERGY
C QUDIND - QUADRUPOLE-INDUCED DIPOLE INTERACTION ENERGY
C DQDIND - GRADIENT OF QUADRUPOLE-INDUCED DIPOLE INTERACTION ENERGY
C
C*MODULE EFELEC   *DECK CHGCHG
      SUBROUTINE CHGCHG(ELTOT)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      CHARACTER*8 FRGNME
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      PARAMETER (ONE=1.0D+00, TWO=2.0D+00, ZERO=0.0D+00)
      PARAMETER (ELEVEN=11.0D+00, SMALL=1.0D-10)
C
      PI=4*ATAN(1.D+00)
      ELTOT=ZERO
      IC1=0
      DO 280 IM = 1,NFRG
         DO 270 IP=1,NMPTS(IM)
            IC1=IC1+1
            X1 = EFC(1,IC1)
            Y1 = EFC(2,IC1)
            Z1 = EFC(3,IC1)
            Q1 = EFCHG(1,IC1)
            Q1N = EFCHG(2,IC1)
            IC2=0
            DO 260 JM = 1,NFRG
               IF (IM.GE.JM)THEN
                 IC2=IC2+NMPTS(JM)
                 GO TO 260
               END IF
               DO 250 JP = 1,NMPTS(JM)
                  IC2=IC2+1
                  X2 = EFC(1,IC2)
                  Y2 = EFC(2,IC2)
                  Z2 = EFC(3,IC2)
                  Q2 = EFCHG(1,IC2)
                  Q2N = EFCHG(2,IC2)
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  R2 = X*X + Y*Y + Z*Z
                  R = SQRT(R2)
                  IF(ICHGP.EQ.3.OR.ICHGP.EQ.2) THEN
                     AI=ZERO
                     AJ=ZERO
                     CF1 = ZERO
                     CF2 = ZERO
                     ALPHAA = EFATRM2(IC1)
                     BETAA = EFBTRM2(IC1)
                     ALPHAB = EFATRM2(IC2)
                  ELSE
                     AI=EFATRM(IC1)
                     AJ=EFATRM(IC2)
                     CF1 = EFBTRM(IC1)
                     CF2 = EFBTRM(IC2)
                     ALPHAA = ZERO
                     BETAA = ZERO
                     ALPHAB = ZERO
                  END IF
                  EX1 = EXP(-AI*R2)
                  EX2 = EXP(-AJ*R2)
            IF(Q1N.NE.ZERO.OR.Q2N.NE.ZERO) THEN
            SCREEN1 = 1.0D+00 - CF1*EX1
            SCREEN2 = 1.0D+00 - CF2*EX2
              ELTOT=ELTOT+(Q1N*Q2*SCREEN2
     *                    +Q2N*Q1*SCREEN1+Q1N*Q2N)/R
            END IF
            ELTOT=ELTOT+Q1*Q2*(1/R
     *                 -CF1*EX1/R
     *                 -CF2*EX2/R)
           IF(CF1*CF2.NE.ZERO)ELTOT=ELTOT+Q1*Q2*
     *                  CF1*CF2*(AI*AJ*SQRT(PI/(AI+AJ)**3)*
     *                  EXP(-R2*AI*AJ/(AI+AJ))*(1-ERRF(R*AI/SQRT(AI+
     *                  AJ))-ERRF(R*AJ/SQRT(AI+AJ)))+(AI*EX1+
     *                  AJ*EX2)/R/(AI+AJ))
C
      IF(ICHGP.EQ.1.OR.R.GT.ELEVEN.OR.BETAA.EQ.ZERO) GOTO 250
C
C     -------  CALCULATE CHARGE PENETRATION --------
C     THIS FORMULA INVOLVES FRAGMENT/FRAGMENT EXPONENTIAL SCREENING
C
               AP=ALPHAA*R
               BP=ALPHAB*R
               AA2=ALPHAA**2
               AB2=ALPHAB**2
               DIFF=ABS(ALPHAA-ALPHAB)
C
C          ALPHAA = ALPHAB?
C
               IF(DIFF.LT.SMALL) THEN
                  TPCHPE=-(EXP(-AP)/R)*(Q1*Q2*(ONE+(AP/TWO))
     *                   +Q1*Q2N+Q2*Q1N)
               ELSE
                  TPCHPE=-(ONE/(TWO*R))*(Q1*(Q2+TWO*Q2N)*EXP(-AP)
     *              +Q2*(Q1+TWO*Q1N)*EXP(-BP)
     *              +Q1*Q2*((AA2+AB2)/(AA2-AB2))*(EXP(-BP)-EXP(-AP)))
               ENDIF
C
C INCORPORATE INTO ELECTROSTATIC ENERGY
C
                ELTOT=ELTOT+TPCHPE
C
250         CONTINUE
260         CONTINUE
270      CONTINUE
280   CONTINUE
C
      RETURN
      END
C*MODULE EFELEC   *DECK CHGDIP
      SUBROUTINE CHGDIP(ELTOT)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      CHARACTER*8 FRGNME
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      PARAMETER (ZERO=0.0D+00)
C
      ELTOT=ZERO
C
      IC1=0
      DO 400 IM = 1,NFRG
         DO 390 IP = 1,NMPTS(IM)
            IC1=IC1+1
            IF(.NOT.DODIPO(IC1))GO TO 390
            X1 = EFC(1,IC1)
            Y1 = EFC(2,IC1)
            Z1 = EFC(3,IC1)
            DX1 = EFDIP(1,IC1)
            DY1 = EFDIP(2,IC1)
            DZ1 = EFDIP(3,IC1)
            IC2=0
            DO 380 JM = 1,NFRG
               IF (IM.EQ.JM)THEN
                 IC2=IC2+NMPTS(JM)
                 GO TO 380
               END IF
               CFX = ZERO
               CFY = ZERO
               CFZ = ZERO
               DO 370 JP = 1,NMPTS(JM)
                  IC2=IC2+1
            IF(.NOT.DOMONO(IC2))GO TO 370
                  X2 = EFC(1,IC2)
                  Y2 = EFC(2,IC2)
                  Z2 = EFC(3,IC2)
                  Q2 = EFCHG(1,IC2)+EFCHG(2,IC2)
                  IF(ICHGP.EQ.3.OR.ICHGP.EQ.2) THEN
                     EX = ZERO
                     CF = ZERO
                  ELSE
                     EX = EFATRM(IC2)
                     CF = EFBTRM(IC2)
                  END IF
C REMEMBER FIELDS AT 1 DUE TO MULTIPOLES AT 2
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  XX = X*X
                  YY = Y*Y
                  ZZ = Z*Z
                  R2 = XX+YY+ZZ
                  R = SQRT(R2)
                  R3 = R2*R
C
C.... POTENTIAL, FIELD, FIELD GRADIENT, AND FIELD SECOND DERIVATIVE
C.... AT POINT 'IP' IN MOLECULE 'IM' DUE TO POINTS IN JM (ACCUMULATED)
C
                  DUM = Q2/R3
                  IF(ICHGP.EQ.3.OR.ICHGP.EQ.2) THEN
                     CFX = CFX + X*DUM
                     CFY = CFY + Y*DUM
                     CFZ = CFZ + Z*DUM
                  ELSE
                     EX2=EXP(-EX*R2)
                     SCREEN=1.0D+00-CF*EX2
                     CFX = CFX + X*DUM*SCREEN - 2.0D+00*Q2*CF*X*EX*EX2/R
                     CFY = CFY + Y*DUM*SCREEN - 2.0D+00*Q2*CF*Y*EX*EX2/R
                     CFZ = CFZ + Z*DUM*SCREEN - 2.0D+00*Q2*CF*Z*EX*EX2/R
                  END IF
C
C.... END OF POINT 'JP' LOOP IN MOLECULE 'JM'
370            CONTINUE
C
C.... POTENTIAL, FIELD, FIELD GRADIENT, ETC. AT POINT 'IP' IN
C     IN MOLECULE 'IM' DUE TO ALL POINTS 'JP' IN MOLECULE 'JM'
C     HAVE NOW BEEN CALCULATED.  ADD INTERACTION OF MOMENTS AT 'IP'
C     WITH THESE FIELDS TO THE 'IM' WITH 'JM' INTERACTION.
C
C
C.... CHARGE(JM) - DIPOLE(IM)
C
               TERM = -(DX1*CFX+DY1*CFY+DZ1*CFZ)
               ELTOT = ELTOT + TERM
380         CONTINUE
390      CONTINUE
400   CONTINUE
C
      RETURN
      END
C*MODULE EFELEC   *DECK CHGQUA
      SUBROUTINE CHGQUA(ELTOT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      CHARACTER*8 FRGNME
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION QUA(6,MXFGPT)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      PARAMETER (ZERO=0.0D+00, TWO=2.0D+00, THREE=3.0D+00,
     *           PT5=0.5D+00, ONEPT5=1.5D+00)
C
C.... REPLACE THE SECOND MOMENTS OF THE CHARGE DISTRIBUTION ....
C.... WITH ELECTRIC QUADRUPOLE MOMENT TENSORS               ....
C.... SEE BUCKINGHAM, ET AL, "MOLECULAR QUADRUPOLE MOMENTS" ....
C.... QUART. REV. (LONDON) -13-, 183-214 (1959)             ....
C.... EQUATION 15.                                          ....
C
      ELTOT=ZERO
      DO 210 I = 1,NMTTPT
        IF(.NOT.DOQUAD(I))GO TO 210
        XX = EFQAD(1,I)
        YY = EFQAD(2,I)
        ZZ = EFQAD(3,I)
        XY = EFQAD(4,I)
        XZ = EFQAD(5,I)
        YZ = EFQAD(6,I)
        DUM = XX + YY + ZZ
        QUA(1,I) = PT5 * (THREE * XX - DUM)
        QUA(2,I) = PT5 * (THREE * YY - DUM)
        QUA(3,I) = PT5 * (THREE * ZZ - DUM)
        QUA(4,I) = ONEPT5 * XY
        QUA(5,I) = ONEPT5 * XZ
        QUA(6,I) = ONEPT5 * YZ
210   CONTINUE
C
      IC1=0
      DO 400 IM = 1,NFRG
         DO 390 IP = 1,NMPTS(IM)
            IC1=IC1+1
            IF(.NOT.DOQUAD(IC1))GO TO 390
            X1 = EFC(1,IC1)
            Y1 = EFC(2,IC1)
            Z1 = EFC(3,IC1)
            XX1 = QUA(1,IC1)
            YY1 = QUA(2,IC1)
            ZZ1 = QUA(3,IC1)
            XY1 = QUA(4,IC1)
            XZ1 = QUA(5,IC1)
            YZ1 = QUA(6,IC1)
            IC2=0
            DO 380 JM = 1,NFRG
               IF (IM.EQ.JM)THEN
                 IC2=IC2+NMPTS(JM)
                 GO TO 380
               END IF
               CFXX = ZERO
               CFYY = ZERO
               CFZZ = ZERO
               CFXY = ZERO
               CFXZ = ZERO
               CFYZ = ZERO
               DO 370 JP = 1,NMPTS(JM)
                  IC2=IC2+1
            IF(.NOT.DOMONO(IC2))GO TO 370
                  X2 = EFC(1,IC2)
                  Y2 = EFC(2,IC2)
                  Z2 = EFC(3,IC2)
                  Q2 = EFCHG(1,IC2)+EFCHG(2,IC2)
C REMEMBER FIELDS AT 1 DUE TO MULTIPOLES AT 2
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  XX = X*X
                  YY = Y*Y
                  ZZ = Z*Z
                  XY = X*Y
                  XZ = X*Z
                  YZ = Y*Z
                  R2 = XX+YY+ZZ
                  R = SQRT(R2)
                  R3 = R2*R
                  R5 = R2*R3
C
C.... POTENTIAL, FIELD, FIELD GRADIENT, AND FIELD SECOND DERIVATIVE
C.... AT POINT 'IP' IN MOLECULE 'IM' DUE TO POINTS IN JM (ACCUMULATED)
C
      DUM = Q2/R5
      CFXX = CFXX - (THREE*XX-R2)*DUM
      CFYY = CFYY - (THREE*YY-R2)*DUM
      CFZZ = CFZZ - (THREE*ZZ-R2)*DUM
      CFXY = CFXY - THREE*XY*DUM
      CFXZ = CFXZ - THREE*XZ*DUM
      CFYZ = CFYZ - THREE*YZ*DUM
C
370            CONTINUE
C
C.... CHARGE(JM) - QUADRUPOLE(IM)
C
               TERM = XX1*CFXX + YY1*CFYY + ZZ1*CFZZ
     1               +TWO*(XY1*CFXY + XZ1*CFXZ + YZ1*CFYZ)
               TERM = TERM/3
               ELTOT = ELTOT - TERM
380         CONTINUE
390      CONTINUE
400   CONTINUE
C
      RETURN
      END
C*MODULE EFELEC   *DECK CHGOCT
      SUBROUTINE CHGOCT(ELTOT)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      CHARACTER*8 FRGNME
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION OCT(10,MXFGPT)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      DATA PT5/0.5D+00/
      DATA THREE,FIVE,SIX/3.0D+00,5.0D+00,6.0D+00/
      DATA ZERO/0.0D+00/
C
C.... REPLACE THIRD MOMENTS WITH OCTUPOLE MOMENTS           ....
C.... SEE BUCKINGHAM, ET AL, "MOLECULAR QUADRUPOLE MOMENTS" ....
C.... QUART. REV. (LONDON) -13-, 183-214 (1959)             ....
C.... EQUATION 15.                                          ....
C
      ELTOT=ZERO
      DO 242 I = 1,NMTTPT
        IF(.NOT.DOOCTU(I))GO TO 242
         XXX = EFOCT(1,I) * PT5
         YYY = EFOCT(2,I) * PT5
         ZZZ = EFOCT(3,I) * PT5
         XXY = EFOCT(4,I) * PT5
         XXZ = EFOCT(5,I) * PT5
         XYY = EFOCT(6,I) * PT5
         YYZ = EFOCT(7,I) * PT5
         XZZ = EFOCT(8,I) * PT5
         YZZ = EFOCT(9,I) * PT5
         XYZ = EFOCT(10,I) * PT5
         DUMX = XXX + XYY + XZZ
         DUMY = XXY + YYY + YZZ
         DUMZ = XXZ + YYZ + ZZZ
         OCT(1,I) = FIVE * XXX - THREE * DUMX
         OCT(2,I) = FIVE * YYY - THREE * DUMY
         OCT(3,I) = FIVE * ZZZ - THREE * DUMZ
         OCT(4,I) = FIVE * XXY - DUMY
         OCT(5,I) = FIVE * XXZ - DUMZ
         OCT(6,I) = FIVE * XYY - DUMX
         OCT(7,I) = FIVE * YYZ - DUMZ
         OCT(8,I) = FIVE * XZZ - DUMX
         OCT(9,I) = FIVE * YZZ - DUMY
         OCT(10,I) = FIVE * XYZ
242   CONTINUE
C
      IC1=0
      DO 400 IM = 1,NFRG
         DO 390 IP = 1,NMPTS(IM)
            IC1=IC1+1
            IF(.NOT.DOOCTU(IC1))GO TO 390
            X1 = EFC(1,IC1)
            Y1 = EFC(2,IC1)
            Z1 = EFC(3,IC1)
            XXX1 = OCT(1,IC1)
            YYY1 = OCT(2,IC1)
            ZZZ1 = OCT(3,IC1)
            XXY1 = OCT(4,IC1)
            XXZ1 = OCT(5,IC1)
            XYY1 = OCT(6,IC1)
            YYZ1 = OCT(7,IC1)
            XZZ1 = OCT(8,IC1)
            YZZ1 = OCT(9,IC1)
            XYZ1 = OCT(10,IC1)
            IC2=0
            DO 380 JM = 1,NFRG
               IF (IM.EQ.JM)THEN
                 IC2=IC2+NMPTS(JM)
                 GO TO 380
               END IF
               CFXXX = ZERO
               CFYYY = ZERO
               CFZZZ = ZERO
               CFXXY = ZERO
               CFXXZ = ZERO
               CFXYY = ZERO
               CFYYZ = ZERO
               CFXZZ = ZERO
               CFYZZ = ZERO
               CFXYZ = ZERO
               DO 370 JP = 1,NMPTS(JM)
                  IC2=IC2+1
            IF(.NOT.DOMONO(IC2))GO TO 370
                  X2 = EFC(1,IC2)
                  Y2 = EFC(2,IC2)
                  Z2 = EFC(3,IC2)
                  Q2 = EFCHG(1,IC2)+EFCHG(2,IC2)
C REMEMBER FIELDS AT 1 DUE TO MULTIPOLES AT 2
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  XX = X*X
                  YY = Y*Y
                  ZZ = Z*Z
                  XY = X*Y
                  XZ = X*Z
                  YZ = Y*Z
                  XXX = XX*X
                  YYY = YY*Y
                  ZZZ = ZZ*Z
                  XXY = XX*Y
                  XXZ = XX*Z
                  XYY = XY*Y
                  YYZ = YY*Z
                  XZZ = XZ*Z
                  YZZ = YZ*Z
                  XYZ = XY*Z
                  R2 = XX+YY+ZZ
                  R = SQRT(R2)
                  R3 = R2*R
                  R5 = R2*R3
                  R7 = R2*R5
C
C.... POTENTIAL, FIELD, FIELD GRADIENT, AND FIELD SECOND DERIVATIVE
C.... AT POINT 'IP' IN MOLECULE 'IM' DUE TO POINTS IN JM (ACCUMULATED)
C
      DUM = Q2/R7
      RRX = R2*X
      RRY = R2*Y
      RRZ = R2*Z
      CFXXX = CFXXX + (FIVE*XXX-THREE*RRX)*DUM
      CFYYY = CFYYY + (FIVE*YYY-THREE*RRY)*DUM
      CFZZZ = CFZZZ + (FIVE*ZZZ-THREE*RRZ)*DUM
      CFXXY = CFXXY + (FIVE*XXY-RRY)*DUM
      CFXXZ = CFXXZ + (FIVE*XXZ-RRZ)*DUM
      CFXYY = CFXYY + (FIVE*XYY-RRX)*DUM
      CFYYZ = CFYYZ + (FIVE*YYZ-RRZ)*DUM
      CFXZZ = CFXZZ + (FIVE*XZZ-RRX)*DUM
      CFYZZ = CFYZZ + (FIVE*YZZ-RRY)*DUM
      CFXYZ = CFXYZ + (FIVE*XYZ)*DUM
C
C.... POTENTIAL, FIELD, AND FIELD GRADIENT DUE TO DIPOLE MOMENTS ON JM
C
C.... END OF POINT 'JP' LOOP IN MOLECULE 'JM'
370            CONTINUE
C
C
C.... POTENTIAL, FIELD, FIELD GRADIENT, ETC. AT POINT 'IP' IN
C     IN MOLECULE 'IM' DUE TO ALL POINTS 'JP' IN MOLECULE 'JM'
C     HAVE NOW BEEN CALCULATED.  ADD INTERACTION OF MOMENTS AT 'IP'
C     WITH THESE FIELDS TO THE 'IM' WITH 'JM' INTERACTION.
C
C.... CHARGE(JM) - OCTUPOLE(IM)
C
               TERM = XXX1*CFXXX + YYY1*CFYYY + ZZZ1*CFZZZ
     1              + THREE*(XXY1*CFXXY + XXZ1*CFXXZ + XYY1*CFXYY)
     1              + THREE*(YYZ1*CFYYZ + XZZ1*CFXZZ + YZZ1*CFYZZ)
     1              + SIX*XYZ1*CFXYZ
               TERM = TERM/FIVE
               ELTOT = ELTOT - TERM
C.... END OF 'JM' MOLECULE LOOP
380         CONTINUE
C
C.... END OF 'IP' LOOP IN 'IM' MOLECULE
390      CONTINUE
C
C
C.... END OF 'IM' MOLECULE LOOP
400   CONTINUE
C
      RETURN
      END
C*MODULE EFELEC   *DECK DPLDPL
      SUBROUTINE DPLDPL(ELTOT)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      CHARACTER*8 FRGNME
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      PARAMETER (ZERO=0.0D+00, THREE=3.0D+00)
C
      ELTOT=ZERO
      IC1=0
      DO 400 IM = 1,NFRG
         DO 390 IP = 1,NMPTS(IM)
            IC1=IC1+1
            IF(.NOT.DODIPO(IC1))GO TO 390
            X1 = EFC(1,IC1)
            Y1 = EFC(2,IC1)
            Z1 = EFC(3,IC1)
            DX1 = EFDIP(1,IC1)
            DY1 = EFDIP(2,IC1)
            DZ1 = EFDIP(3,IC1)
            IC2=0
            DO 380 JM = 1,NFRG
               IF (IM.GE.JM)THEN
                 IC2=IC2+NMPTS(JM)
                 GO TO 380
               END IF
               DFX = ZERO
               DFY = ZERO
               DFZ = ZERO
               DO 370 JP = 1,NMPTS(JM)
                  IC2=IC2+1
            IF(.NOT.DODIPO(IC2))GO TO 370
                  X2 = EFC(1,IC2)
                  Y2 = EFC(2,IC2)
                  Z2 = EFC(3,IC2)
                  DX2 = EFDIP(1,IC2)
                  DY2 = EFDIP(2,IC2)
                  DZ2 = EFDIP(3,IC2)
C REMEMBER FIELDS AT 1 DUE TO MULTIPOLES AT 2
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  XX = X*X
                  YY = Y*Y
                  ZZ = Z*Z
                  R2 = XX+YY+ZZ
                  R = SQRT(R2)
                  R3 = R2*R
                  R5 = R2*R3
C
      DOT = DX2*X + DY2*Y + DZ2*Z
      DUM = DOT*THREE/R5
      DFX = DFX - DX2/R3 + X*DUM
      DFY = DFY - DY2/R3 + Y*DUM
      DFZ = DFZ - DZ2/R3 + Z*DUM
C
370            CONTINUE
C
C.... DIPOLE(JM) - DIPOLE(IM)
C
               TERM = -(DX1*DFX+DY1*DFY+DZ1*DFZ)
               ELTOT = ELTOT + TERM
380         CONTINUE
390      CONTINUE
400   CONTINUE
C
      RETURN
      END
C*MODULE EFELEC   *DECK DPQUAD
      SUBROUTINE DPQUAD(ELTOT)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      CHARACTER*8 FRGNME
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION QUA(6,MXFGPT)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      PARAMETER (ZERO=0.0D+00, THREE=3.0D+00, PT5=0.5D+00,
     *           ONEPT5=1.5D+00, TWO=2.0D+00, FIFTEN=1.5D+01)
C
C.... REPLACE THE SECOND MOMENTS OF THE CHARGE DISTRIBUTION ....
C.... WITH ELECTRIC QUADRUPOLE MOMENT TENSORS               ....
C.... SEE BUCKINGHAM, ET AL, "MOLECULAR QUADRUPOLE MOMENTS" ....
C.... QUART. REV. (LONDON) -13-, 183-214 (1959)             ....
C.... EQUATION 15.                                          ....
C
      ELTOT=ZERO
      DO 210 I = 1,NMTTPT
        IF(.NOT.DOQUAD(I))GO TO 210
        XX = EFQAD(1,I)
        YY = EFQAD(2,I)
        ZZ = EFQAD(3,I)
        XY = EFQAD(4,I)
        XZ = EFQAD(5,I)
        YZ = EFQAD(6,I)
        DUM = XX + YY + ZZ
        QUA(1,I) = PT5 * (THREE * XX - DUM)
        QUA(2,I) = PT5 * (THREE * YY - DUM)
        QUA(3,I) = PT5 * (THREE * ZZ - DUM)
        QUA(4,I) = ONEPT5 * XY
        QUA(5,I) = ONEPT5 * XZ
        QUA(6,I) = ONEPT5 * YZ
210   CONTINUE
C
      IC1=0
      DO 400 IM = 1,NFRG
         DO 390 IP = 1,NMPTS(IM)
            IC1=IC1+1
            IF(.NOT.DOQUAD(IC1))GO TO 390
            X1 = EFC(1,IC1)
            Y1 = EFC(2,IC1)
            Z1 = EFC(3,IC1)
            XX1 = QUA(1,IC1)
            YY1 = QUA(2,IC1)
            ZZ1 = QUA(3,IC1)
            XY1 = QUA(4,IC1)
            XZ1 = QUA(5,IC1)
            YZ1 = QUA(6,IC1)
            IC2=0
            DO 380 JM = 1,NFRG
               IF (IM.EQ.JM)THEN
                 IC2=IC2+NMPTS(JM)
                 GO TO 380
               END IF
               DFXX = ZERO
               DFYY = ZERO
               DFZZ = ZERO
               DFXY = ZERO
               DFXZ = ZERO
               DFYZ = ZERO
               DO 370 JP = 1,NMPTS(JM)
                  IC2=IC2+1
            IF(.NOT.DODIPO(IC2))GO TO 370
                  X2 = EFC(1,IC2)
                  Y2 = EFC(2,IC2)
                  Z2 = EFC(3,IC2)
                  DX2 = EFDIP(1,IC2)
                  DY2 = EFDIP(2,IC2)
                  DZ2 = EFDIP(3,IC2)
C REMEMBER FIELDS AT 1 DUE TO MULTIPOLES AT 2
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  XX = X*X
                  YY = Y*Y
                  ZZ = Z*Z
                  XY = X*Y
                  XZ = X*Z
                  YZ = Y*Z
                  R2 = XX+YY+ZZ
                  R = SQRT(R2)
                  R3 = R2*R
                  R5 = R2*R3
                  R7 = R2*R5
C
C.... POTENTIAL, FIELD, AND FIELD GRADIENT DUE TO DIPOLE MOMENTS ON JM
C
      DOT = DX2*X + DY2*Y + DZ2*Z
C
      DUM = THREE/R5
C     DFXX = DFXX + (TWO*DX2*X+DOT)*DUM - FIFTEN*XX*DOT/R7
C     DFYY = DFYY + (TWO*DY2*Y+DOT)*DUM - FIFTEN*YY*DOT/R7
C     DFZZ = DFZZ + (TWO*DZ2*Z+DOT)*DUM - FIFTEN*ZZ*DOT/R7
      DFXX = DFXX + (TWO*DX2*X)*DUM - FIFTEN*XX*DOT/R7
      DFYY = DFYY + (TWO*DY2*Y)*DUM - FIFTEN*YY*DOT/R7
      DFZZ = DFZZ + (TWO*DZ2*Z)*DUM - FIFTEN*ZZ*DOT/R7
      DFXY = DFXY + (DX2*Y+DY2*X)*DUM - FIFTEN*XY*DOT/R7
      DFXZ = DFXZ + (DX2*Z+DZ2*X)*DUM - FIFTEN*XZ*DOT/R7
      DFYZ = DFYZ + (DY2*Z+DZ2*Y)*DUM - FIFTEN*YZ*DOT/R7
C
C     DFXX = DFXX + (TWO*DX2*X+DOT)*DUM
C     DFYY = DFYY + (TWO*DY2*Y+DOT)*DUM
C     DFZZ = DFZZ + (TWO*DZ2*Z+DOT)*DUM
C     DFXX = DFXX + (TWO*DX2*X)*DUM
C     DFYY = DFYY + (TWO*DY2*Y)*DUM
C     DFZZ = DFZZ + (TWO*DZ2*Z)*DUM
C     DFXY = DFXY + (DX2*Y+DY2*X)*DUM
C     DFXZ = DFXZ + (DX2*Z+DZ2*X)*DUM
C     DFYZ = DFYZ + (DY2*Z+DZ2*Y)*DUM
C
C     DFXX = DFXX - FIFTEN*XX*DOT/R7
C     DFYY = DFYY - FIFTEN*YY*DOT/R7
C     DFZZ = DFZZ - FIFTEN*ZZ*DOT/R7
C     DFXY = DFXY - FIFTEN*XY*DOT/R7
C     DFXZ = DFXZ - FIFTEN*XZ*DOT/R7
C     DFYZ = DFYZ - FIFTEN*YZ*DOT/R7
C.... END OF POINT 'JP' LOOP IN MOLECULE 'JM'
370            CONTINUE
C
C.... DIPOLE(JM) - QUADRUPOLE(IM)
C
C              TERM = XX1*DFXX
               TERM = XX1*DFXX + YY1*DFYY + ZZ1*DFZZ
     1              + TWO*(XY1*DFXY + XZ1*DFXZ + YZ1*DFYZ)
               TERM = TERM/3
               ELTOT = ELTOT - TERM
380         CONTINUE
390      CONTINUE
400   CONTINUE
C
      RETURN
      END
C*MODULE EFELEC   *DECK QUDQUD
      SUBROUTINE QUDQUD(ELTOT)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      CHARACTER*8 FRGNME
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C     DIMENSION QUAQUA(MXFRG,MXFRG),ASUM(MXFRG,MXFRG),QUA(6,MXFGPT)
      DIMENSION QUA(6,MXFGPT)
      DATA PT5,ONEPT5,TWO/0.5D+00,1.5D+00,2.0D+00/
      DATA THREE,SIX/3.0D+00,6.0D+00/
      DATA ZERO/0.0D+00/
C
C.... REPLACE THE SECOND MOMENTS OF THE CHARGE DISTRIBUTION ....
C.... WITH ELECTRIC QUADRUPOLE MOMENT TENSORS               ....
C.... SEE BUCKINGHAM, ET AL, "MOLECULAR QUADRUPOLE MOMENTS" ....
C.... QUART. REV. (LONDON) -13-, 183-214 (1959)             ....
C.... EQUATION 15.                                          ....
C
      ELTOT=ZERO
      DO 210 I = 1,NMTTPT
        IF(.NOT.DOQUAD(I))GO TO 210
        XX = EFQAD(1,I)
        YY = EFQAD(2,I)
        ZZ = EFQAD(3,I)
        XY = EFQAD(4,I)
        XZ = EFQAD(5,I)
        YZ = EFQAD(6,I)
        DUM = XX + YY + ZZ
        QUA(1,I) = PT5 * (THREE * XX - DUM)
        QUA(2,I) = PT5 * (THREE * YY - DUM)
        QUA(3,I) = PT5 * (THREE * ZZ - DUM)
        QUA(4,I) = ONEPT5 * XY
        QUA(5,I) = ONEPT5 * XZ
        QUA(6,I) = ONEPT5 * YZ
210   CONTINUE
C
      IC1=0
      DO 400 IM = 1,NFRG
         DO 390 IP = 1,NMPTS(IM)
            IC1=IC1+1
            IF(.NOT.DOQUAD(IC1))GO TO 390
            X1 = EFC(1,IC1)
            Y1 = EFC(2,IC1)
            Z1 = EFC(3,IC1)
            XX1 = QUA(1,IC1)
            YY1 = QUA(2,IC1)
            ZZ1 = QUA(3,IC1)
            XY1 = QUA(4,IC1)
            XZ1 = QUA(5,IC1)
            YZ1 = QUA(6,IC1)
            IC2=0
            DO 380 JM = 1,NFRG
               IF (IM.GE.JM)THEN
                 IC2=IC2+NMPTS(JM)
                 GO TO 380
               END IF
               QUAD = ZERO
               DO 370 JP = 1,NMPTS(JM)
                  IC2=IC2+1
            IF(.NOT.DOQUAD(IC2))GO TO 370
                  X2 = EFC(1,IC2)
                  Y2 = EFC(2,IC2)
                  Z2 = EFC(3,IC2)
                  XX2 = QUA(1,IC2)
                  YY2 = QUA(2,IC2)
                  ZZ2 = QUA(3,IC2)
                  XY2 = QUA(4,IC2)
                  XZ2 = QUA(5,IC2)
                  YZ2 = QUA(6,IC2)
C REMEMBER FIELDS AT 1 DUE TO MULTIPOLES AT 2
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  XX = X*X
                  YY = Y*Y
                  ZZ = Z*Z
                  XY = X*Y
                  XZ = X*Z
                  YZ = Y*Z
                  R2 = XX+YY+ZZ
                  R = SQRT(R2)
                  R3 = R2*R
                  R5 = R2*R3
                  R7 = R2*R5
                  R9 = R2*R7
C
C.... GET THE QUADRUPOLE(IM)-QUADRUPOLE CONTRIBUTION(JM) ....
C     THIS PART HANDLED SEPARATELY FROM REST UNTIL
C     FIELD GRADIENT DUE TO QUADRUPOLE IS DERIVED
C
                  VAL = ZERO
                  VAL = VAL + X*(X*XX2 + Y*XY2 + Z*XZ2)
                  VAL = VAL + Y*(X*XY2 + Y*YY2 + Z*YZ2)
                  VAL = VAL + Z*(X*XZ2 + Y*YZ2 + Z*ZZ2)
                  QUAD1 = XX1*XX2 + TWO*XY1*XY2 + TWO*XZ1*XZ2
                  QUAD1 = QUAD1 + YY1*YY2 + TWO*YZ1*YZ2 + ZZ1*ZZ2
                  QUAD = QUAD + QUAD1*SIX/R5
                  QUAD2 = X*(X*XX1+Y*XY1+Z*XZ1)
                  QUAD2 = Y*(X*XY1+Y*YY1+Z*YZ1) + QUAD2
                  QUAD2 = Z*(X*XZ1+Y*YZ1+Z*ZZ1) + QUAD2
                  QUAD = QUAD + QUAD2*VAL*105.0D 0 / R9
                  FACT1 = X*XX1 + Y*XY1 + Z*XZ1
                  FACT2 = X*XX2 + Y*XY2 + Z*XZ2
                  QUAD3 = FACT1 * FACT2
                  FACT1 = X*XY1 + Y*YY1 + Z*YZ1
                  FACT2 = X*XY2 + Y*YY2 + Z*YZ2
                  QUAD3 = QUAD3 + FACT1 * FACT2
                  FACT1 = X*XZ1 + Y*YZ1 + Z*ZZ1
                  FACT2 = X*XZ2 + Y*YZ2 + Z*ZZ2
                  QUAD3 = QUAD3 + FACT1 * FACT2
                  QUAD = QUAD - 60.0D 0 * QUAD3 / R7
C     FIELD DUE TO QUADRUPOLE MOMENTS ON JM.
C
C.... END OF POINT 'JP' LOOP IN MOLECULE 'JM'
370            CONTINUE
C
C.... QUADRUPOLE(JM) - QUADRUPOLE(IM)
C
               ELTOT = ELTOT + 2*QUAD/18.0D+00
380         CONTINUE
390      CONTINUE
400   CONTINUE
C
      RETURN
      END
C*MODULE EFELEC   *DECK DCHCH
      SUBROUTINE DCHCH(EF3)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      CHARACTER*8 FRGNME
C
      DIMENSION EF3(3,*)
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      PARAMETER (ELEVEN=11.0D+00, SMALL=1.0D-10)
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, TWO=2.0D+00)
C
      PI=4*ATAN(1.D+00)
      IC1=0
      DO 280 IM = 1,NFRG
         DO 270 IP=1,NMPTS(IM)
            IC1=IC1+1
            X1 = EFC(1,IC1)
            Y1 = EFC(2,IC1)
            Z1 = EFC(3,IC1)
            Q1 = EFCHG(1,IC1)
            Q1N = EFCHG(2,IC1)
            IC2=0
            DO 260 JM = 1,NFRG
               IF (IM.GE.JM)THEN
                 IC2=IC2+NMPTS(JM)
                 GO TO 260
               END IF
               CON1=ZERO
               DO 250 JP = 1,NMPTS(JM)
                  IC2=IC2+1
                  X2 = EFC(1,IC2)
                  Y2 = EFC(2,IC2)
                  Z2 = EFC(3,IC2)
                  Q2 = EFCHG(1,IC2)
                  Q2N = EFCHG(2,IC2)
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  R2 = X*X + Y*Y + Z*Z
                  R = SQRT(R2)
                  R3 = R * R * R
C
                  IF(ICHGP.EQ.1) THEN
                     AI=EFATRM(IC1)
                     AJ=EFATRM(IC2)
                     AIJ=AI+AJ
                     CF1 = EFBTRM(IC1)
                     CF2 = EFBTRM(IC2)
                  ELSE
                     AI= ZERO
                     AJ= ZERO
                     AIJ=ZERO
                     CF1 = ZERO
                     CF2 = ZERO
                  END IF
C
                  EX1 = EXP(-AI*R2)
                  EX2 = EXP(-AJ*R2)
                  SCREEN1 = 1.0D+00 - CF1*EX1
                  CON1=ZERO
                  IF(Q1N.NE.ZERO.OR.Q2N.NE.ZERO)THEN
                     SCREEN2 = 1.0D+00 - CF2*EX2
C THE NEXT LINE HAS TERMS 1A,2A,3A
                     CON1=(Q1N*Q2*SCREEN2
     *                    +Q2N*Q1*SCREEN1+Q1N*Q2N)/R3
                  END IF
C THE NEXT LINE HAS TERMS 4A,5A,6A
                     CON1=CON1+Q1*Q2*(SCREEN1-CF2*EX2)/R3
                  IF(CF1*CF2.NE.ZERO)THEN
C THE NEXT LINE HAS TERMS 7A,8A
                     CON1=CON1
     *                   +Q1*Q2*CF1*CF2*(AI*EX1+AJ*EX2)/(AIJ*R3)
C THE NEXT LINE HAS TERMS 2B,3B,5B,6B
                    CON1=CON1
     *       -TWO*(Q1*(Q2+Q2N)*AI*CF1*EX1+Q2*(Q1+Q1N)*AJ*CF2*EX2)/R
C THE NEXT LINE HAS TERMS 7B,8B
             CON1=CON1
     *       +TWO*Q1*Q2*CF1*CF2*(AI*AI*EX1+AJ*AJ*EX2)/AIJ/R
C  THE REST IS FOR TERM #9
             CON=AI*AJ*SQRT(PI/AIJ**3)*EXP(-R2*AI*AJ/AIJ)
             CON1=CON1+Q1*Q2*CF1*CF2*CON
     *        *((1-ERRF(R*AI/SQRT(AIJ))-ERRF(R*AJ/SQRT(AIJ)))*
     *       TWO*AI*AJ/AIJ
     *        +(AI*EXP(-AI*AI*R2/AIJ)+AJ*EXP(-AJ*AJ*R2/AIJ))
     *         *TWO/(R*SQRT(PI*AIJ)))
           END IF
C
            IF(ICHGP.EQ.3.OR.ICHGP.EQ.2) THEN
               ALPHAA = EFATRM2(IC1)
               BETAA = EFBTRM2(IC1)
               ALPHAB = EFATRM2(IC2)
            ELSE
               ALPHAA = ZERO
               BETAA = ZERO
               ALPHAB = ZERO
            END IF
C
C BEGINING OF CHARGE PENETRATION GRADIENT :
C
           IF(ICHGP.EQ.1.OR.R.GT.ELEVEN.OR.BETAA.EQ.ZERO)  GOTO 249
C
C     -------  CALCULATE CHARGE PENETRATION GRADIENT --------
C     FRAGMENT/FRAGMENT EXPONENTIAL SCREENING FORMULA
C
               AP=ALPHAA*R
               BP=ALPHAB*R
               AA2=ALPHAA**2
               AB2=ALPHAB**2
               DIFF=ABS(ALPHAA-ALPHAB)
               IF(DIFF.LT.SMALL) THEN
C
C       -------- FORMULA FOR ALPHAA=ALPHAB ---------
C
       GTPCHPE=-ONE*(EXP(-AP)/R3)*((Q1*Q2+Q1*Q2N+Q2*Q1N)*(ONE+
     *         AP)+(AA2*Q1*Q2*R2/TWO))
               ELSE
C
C IF ALPHAA NOT EQUAL TO ALPHAB:
C
      GTPCHPE=-ONE*((EXP(-AP-BP))/((AA2-AB2)*R3))*(Q2*(ONE+
     *        BP)*EXP(AP)*(Q1*AA2+Q1N*(AA2-AB2))+Q1*(ONE+
     *        AP)*EXP(BP)*(Q2N*(AA2-AB2)-Q2*AB2))
              END IF
C
            CON1=CON1+GTPCHPE
C
  249       CONTINUE
            CONX=CON1*X
            CONY=CON1*Y
            CONZ=CON1*Z
            EF3(1,IC2)=EF3(1,IC2)+CONX
            EF3(2,IC2)=EF3(2,IC2)+CONY
            EF3(3,IC2)=EF3(3,IC2)+CONZ
            EF3(1,IC1)=EF3(1,IC1)-CONX
            EF3(2,IC1)=EF3(2,IC1)-CONY
            EF3(3,IC1)=EF3(3,IC1)-CONZ
250         CONTINUE
260         CONTINUE
270      CONTINUE
280   CONTINUE
C
      RETURN
      END
C*MODULE EFELEC   *DECK DCHDIP
      SUBROUTINE DCHDIP(EF3)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      CHARACTER*8 FRGNME
C
      DIMENSION EF3(3,*)
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      PARAMETER (ZERO=0.0D+00)
C
      IC1=0
      DO 400 IM = 1,NFRG
         DO 390 IP = 1,NMPTS(IM)
            IC1=IC1+1
            IF(.NOT.DODIPO(IC1))GO TO 390
            X1 = EFC(1,IC1)
            Y1 = EFC(2,IC1)
            Z1 = EFC(3,IC1)
            DX1 = EFDIP(1,IC1)
            DY1 = EFDIP(2,IC1)
            DZ1 = EFDIP(3,IC1)
            IC2=0
            DO 380 JM = 1,NFRG
               IF (IM.EQ.JM)THEN
                 IC2=IC2+NMPTS(JM)
                 GO TO 380
               END IF
               CFR = ZERO
               DO 370 JP = 1,NMPTS(JM)
                  IC2=IC2+1
            IF(.NOT.DOMONO(IC2))GO TO 370
                  X2 = EFC(1,IC2)
                  Y2 = EFC(2,IC2)
                  Z2 = EFC(3,IC2)
                  Q2 = EFCHG(1,IC2)+EFCHG(2,IC2)
                  IF(ICHGP.EQ.3.OR.ICHGP.EQ.2) THEN
                     EX = ZERO
                     CF = ZERO
                  ELSE
                     EX = EFATRM(IC2)
                     CF = EFBTRM(IC2)
                  END IF
C REMEMBER FIELDS AT 1 DUE TO MULTIPOLES AT 2
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  XX = X*X
                  YY = Y*Y
                  ZZ = Z*Z
                  R2 = XX+YY+ZZ
                  R = SQRT(R2)
                  R3 = R2*R
                  R5 = R2*R3
C
C.... POTENTIAL, FIELD, FIELD GRADIENT, AND FIELD SECOND DERIVATIVE
C.... AT POINT 'IP' IN MOLECULE 'IM' DUE TO POINTS IN JM (ACCUMULATED)
C
      DUM = Q2/R3
      DOT = DX1*X + DY1*Y + DZ1*Z
      IF(ICHGP.EQ.3.OR.ICHGP.EQ.2) THEN
         CFR =  DUM
         CN1 = (3.0D+00/R5)*DOT*Q2
      ELSE
         EX2=EXP(-EX*R2)
         SCREEN=1.0D+00-CF*EX2
         CFR =  DUM*SCREEN - 2.0D+00*Q2*CF*EX*EX2/R
         CN1 = (3.0D+00*SCREEN/R5
     *        - 4.0D+00*CF*EX*EX2*(1.0D+00/R3 + EX/R))*DOT*Q2
      END IF
      CONX = DX1*CFR - X*CN1
      CONY = DY1*CFR - Y*CN1
      CONZ = DZ1*CFR - Z*CN1
      EF3(1,IC2) = EF3(1,IC2) + CONX
      EF3(2,IC2) = EF3(2,IC2) + CONY
      EF3(3,IC2) = EF3(3,IC2) + CONZ
      EF3(1,IC1) = EF3(1,IC1) - CONX
      EF3(2,IC1) = EF3(2,IC1) - CONY
      EF3(3,IC1) = EF3(3,IC1) - CONZ
      ATORQ(1,IM) = ATORQ(1,IM) - CFR*(DY1*Z-DZ1*Y)
      ATORQ(2,IM) = ATORQ(2,IM) - CFR*(DZ1*X-DX1*Z)
      ATORQ(3,IM) = ATORQ(3,IM) - CFR*(DX1*Y-DY1*X)
C
370            CONTINUE
380         CONTINUE
390      CONTINUE
400   CONTINUE
C
      RETURN
      END
C*MODULE EFELEC   *DECK DDPDP
      SUBROUTINE DDPDP(EF3)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      CHARACTER*8 FRGNME
C
      DIMENSION EF3(3,*)
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      PARAMETER (ZERO=0.0D+00, THREE=3.0D+00)
C
      IC1=0
      DO 400 IM = 1,NFRG
         DO 390 IP = 1,NMPTS(IM)
            IC1=IC1+1
            IF(.NOT.DODIPO(IC1))GO TO 390
            X1 = EFC(1,IC1)
            Y1 = EFC(2,IC1)
            Z1 = EFC(3,IC1)
            DX1 = EFDIP(1,IC1)
            DY1 = EFDIP(2,IC1)
            DZ1 = EFDIP(3,IC1)
            IC2=0
            DO 380 JM = 1,NFRG
               IF (IM.GE.JM)THEN
                 IC2=IC2+NMPTS(JM)
                 GO TO 380
               END IF
               DFX2 = ZERO
               DFY2 = ZERO
               DFZ2 = ZERO
               DFX1 = ZERO
               DFY1 = ZERO
               DFZ1 = ZERO
               DO 370 JP = 1,NMPTS(JM)
                  IC2=IC2+1
            IF(.NOT.DODIPO(IC2))GO TO 370
                  X2 = EFC(1,IC2)
                  Y2 = EFC(2,IC2)
                  Z2 = EFC(3,IC2)
                  DX2 = EFDIP(1,IC2)
                  DY2 = EFDIP(2,IC2)
                  DZ2 = EFDIP(3,IC2)
C REMEMBER FIELDS AT 1 DUE TO MULTIPOLES AT 2
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  XX = X*X
                  YY = Y*Y
                  ZZ = Z*Z
                  XY = X*Y
                  XZ = X*Z
                  YZ = Y*Z
                  R2 = XX+YY+ZZ
                  R = SQRT(R2)
                  R3 = R2*R
                  R5 = R2*R3
                  R7 = R2*R5
C
      DOT1 = DX1*X + DY1*Y + DZ1*Z
      DOT2 = DX2*X + DY2*Y + DZ2*Z
      DOTM = DX2*DX1 + DY2*DY1 + DZ2*DZ1
      DOTXY=DY2*DX1+DY1*DX2
      DOTXZ=DZ2*DX1+DZ1*DX2
      DOTYZ=DY2*DZ1+DY1*DZ2
      DOTX = 2.0D+00*DX2*DX1*X+DOTXY*Y+DOTXZ*Z
      DOTY = 2.0D+00*DY2*DY1*Y+DOTXY*X+DOTYZ*Z
      DOTZ = 2.0D+00*DZ2*DZ1*Z+DOTXZ*X+DOTYZ*Y
      DUM = THREE/R5
      DUM2=DUM*DOT2
      DUM1=DUM*DOT1
      TEMP = DX1*DX2*XX+DY1*DY2*YY+DZ1*DZ2*ZZ
     * +(DX1*DY2+DY1*DX2)*XY+(DX1*DZ2+DZ1*DX2)*XZ
     * +(DY1*DZ2+DZ1*DY2)*YZ
      DUM7 = 15.0D+00 * TEMP/R7
C     DFX2 = DFX2 - (DX2/R3 + X*DUM2)
C     DFY2 = DFY2 - (DY2/R3 + Y*DUM2)
C     DFZ2 = DFZ2 - (DZ2/R3 + Z*DUM2)
C     DFX1 = DFX1 - (DX1/R3 + X*DUM1)
C     DFY1 = DFY1 - (DY1/R3 + Y*DUM1)
C     DFZ1 = DFZ1 - (DZ1/R3 + Z*DUM1)
      DFX2 = -DX2/R3 + X*DUM2
      DFY2 = -DY2/R3 + Y*DUM2
      DFZ2 = -DZ2/R3 + Z*DUM2
      DFX1 = -DX1/R3 + X*DUM1
      DFY1 = -DY1/R3 + Y*DUM1
      DFZ1 = -DZ1/R3 + Z*DUM1
      CONX = DUM*(DOTM*X+DOTX)-DUM7*X
      CONY = DUM*(DOTM*Y+DOTY)-DUM7*Y
      CONZ = DUM*(DOTM*Z+DOTZ)-DUM7*Z
            EF3(1,IC2)=EF3(1,IC2)+CONX
            EF3(2,IC2)=EF3(2,IC2)+CONY
            EF3(3,IC2)=EF3(3,IC2)+CONZ
            EF3(1,IC1)=EF3(1,IC1)-CONX
            EF3(2,IC1)=EF3(2,IC1)-CONY
            EF3(3,IC1)=EF3(3,IC1)-CONZ
C
C
      ATORQ(1,IM)=ATORQ(1,IM)-DY1*DFZ2+DZ1*DFY2
      ATORQ(2,IM)=ATORQ(2,IM)-DZ1*DFX2+DX1*DFZ2
      ATORQ(3,IM)=ATORQ(3,IM)-DX1*DFY2+DY1*DFX2
      ATORQ(1,JM)=ATORQ(1,JM)-DY2*DFZ1+DZ2*DFY1
      ATORQ(2,JM)=ATORQ(2,JM)-DZ2*DFX1+DX2*DFZ1
      ATORQ(3,JM)=ATORQ(3,JM)-DX2*DFY1+DY2*DFX1
C
370            CONTINUE
380         CONTINUE
390      CONTINUE
400   CONTINUE
C
      RETURN
      END
C*MODULE EFELEC   *DECK DCHQUA
      SUBROUTINE DCHQUA(EF3,QUA)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      CHARACTER*8 FRGNME
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
C                   2ND DIMENSION OF QUA MUST BE NMTTPT...
      DIMENSION EF3(3,*),QUA(6,*)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      DATA PT5,ONEPT5,TWO/0.5D+00,1.5D+00,2.0D+00/
      DATA THREE/3.0D+00/, ZERO/0.0D+00/
C
C.... REPLACE THE SECOND MOMENTS OF THE CHARGE DISTRIBUTION ....
C.... WITH ELECTRIC QUADRUPOLE MOMENT TENSORS               ....
C.... SEE BUCKINGHAM, ET AL, "MOLECULAR QUADRUPOLE MOMENTS" ....
C.... QUART. REV. (LONDON) -13-, 183-214 (1959)             ....
C.... EQUATION 15.                                          ....
C
      DO 210 I = 1,NMTTPT
        IF(.NOT.DOQUAD(I))GO TO 210
        XX = EFQAD(1,I)
        YY = EFQAD(2,I)
        ZZ = EFQAD(3,I)
        XY = EFQAD(4,I)
        XZ = EFQAD(5,I)
        YZ = EFQAD(6,I)
        DUM = XX + YY + ZZ
        QUA(1,I) = PT5 * (THREE * XX - DUM)
        QUA(2,I) = PT5 * (THREE * YY - DUM)
        QUA(3,I) = PT5 * (THREE * ZZ - DUM)
        QUA(4,I) = ONEPT5 * XY
        QUA(5,I) = ONEPT5 * XZ
        QUA(6,I) = ONEPT5 * YZ
210   CONTINUE
C
      IC1=0
      DO 400 IM = 1,NFRG
         DO 390 IP = 1,NMPTS(IM)
            IC1=IC1+1
            IF(.NOT.DOQUAD(IC1))GO TO 390
            X1 = EFC(1,IC1)
            Y1 = EFC(2,IC1)
            Z1 = EFC(3,IC1)
            XX1 = QUA(1,IC1)
            YY1 = QUA(2,IC1)
            ZZ1 = QUA(3,IC1)
            XY1 = QUA(4,IC1)
            XZ1 = QUA(5,IC1)
            YZ1 = QUA(6,IC1)
            IC2=0
            DO 380 JM = 1,NFRG
               IF (IM.EQ.JM)THEN
                 IC2=IC2+NMPTS(JM)
                 GO TO 380
               END IF
               CFXX = ZERO
               CFYY = ZERO
               CFZZ = ZERO
               CFXY = ZERO
               CFXZ = ZERO
               CFYZ = ZERO
               DO 370 JP = 1,NMPTS(JM)
                  IC2=IC2+1
            IF(.NOT.DOMONO(IC2))GO TO 370
                  X2 = EFC(1,IC2)
                  Y2 = EFC(2,IC2)
                  Z2 = EFC(3,IC2)
                  Q2 = EFCHG(1,IC2)+EFCHG(2,IC2)
C REMEMBER FIELDS AT 1 DUE TO MULTIPOLES AT 2
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  XX = X*X
                  YY = Y*Y
                  ZZ = Z*Z
                  XY = X*Y
                  XZ = X*Z
                  YZ = Y*Z
                  R2 = XX+YY+ZZ
                  R = SQRT(R2)
                  R3 = R2*R
                  R5 = R2*R3
                  R7 = R2*R5
C
C.... POTENTIAL, FIELD, FIELD GRADIENT, AND FIELD SECOND DERIVATIVE
C.... AT POINT 'IP' IN MOLECULE 'IM' DUE TO POINTS IN JM (ACCUMULATED)
C
      DUM = 2.0D+00*Q2/R5
      DUM7 = 5.0D+00*Q2/R7/3.0D+00
      CFXX = (THREE*XX-R2)*DUM7
      CFYY = (THREE*YY-R2)*DUM7
      CFZZ = (THREE*ZZ-R2)*DUM7
      CFXY = THREE*XY*DUM7
      CFXZ = THREE*XZ*DUM7
      CFYZ = THREE*YZ*DUM7
C
      TMX = (XX1*X+XY1*Y+XZ1*Z)*DUM
      TMY = (XY1*X+YY1*Y+YZ1*Z)*DUM
      TMZ = (XZ1*X+YZ1*Y+ZZ1*Z)*DUM
C
               TERM = XX1*CFXX + YY1*CFYY + ZZ1*CFZZ
     1               +TWO*(XY1*CFXY + XZ1*CFXZ + YZ1*CFYZ)
               TERMX = TERM*X
               TERMY = TERM*Y
               TERMZ = TERM*Z
            EF3(1,IC2)=EF3(1,IC2)-TMX+TERMX
            EF3(2,IC2)=EF3(2,IC2)-TMY+TERMY
            EF3(3,IC2)=EF3(3,IC2)-TMZ+TERMZ
            EF3(1,IC1)=EF3(1,IC1)+TMX-TERMX
            EF3(2,IC1)=EF3(2,IC1)+TMY-TERMY
            EF3(3,IC1)=EF3(3,IC1)+TMZ-TERMZ
      ATORQ(1,IM)=ATORQ(1,IM)-Y*TMZ+Z*TMY
      ATORQ(2,IM)=ATORQ(2,IM)-Z*TMX+X*TMZ
      ATORQ(3,IM)=ATORQ(3,IM)-X*TMY+Y*TMX
370            CONTINUE
380         CONTINUE
390      CONTINUE
400   CONTINUE
C
      RETURN
      END
C*MODULE EFELEC   *DECK DCHOCT
      SUBROUTINE DCHOCT(EF3,OCT)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      CHARACTER*8 FRGNME
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
C                   2ND DIMENSION OF OCT MUST BE NMTTPT...
      DIMENSION EF3(3,*),OCT(10,*)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      DATA PT5,TWO/0.5D+00,2.0D+00/
      DATA TEN,FIFTEN,THIRTY/10.0D+00,15.0D+00,30.0D+00/
      DATA THREE,FIVE,SIX,SEVEN/3.0D+00,5.0D+00,6.0D+00,7.0D+00/
      DATA ZERO/0.0D+00/
C
C.... REPLACE THIRD MOMENTS WITH OCTUPOLE MOMENTS           ....
C.... SEE BUCKINGHAM, ET AL, "MOLECULAR QUADRUPOLE MOMENTS" ....
C.... QUART. REV. (LONDON) -13-, 183-214 (1959)             ....
C.... EQUATION 15.                                          ....
C
      DO 242 I = 1,NMTTPT
        IF(.NOT.DOOCTU(I))GO TO 242
         XXX = EFOCT(1,I) * PT5
         YYY = EFOCT(2,I) * PT5
         ZZZ = EFOCT(3,I) * PT5
         XXY = EFOCT(4,I) * PT5
         XXZ = EFOCT(5,I) * PT5
         XYY = EFOCT(6,I) * PT5
         YYZ = EFOCT(7,I) * PT5
         XZZ = EFOCT(8,I) * PT5
         YZZ = EFOCT(9,I) * PT5
         XYZ = EFOCT(10,I) * PT5
         DUMX = XXX + XYY + XZZ
         DUMY = XXY + YYY + YZZ
         DUMZ = XXZ + YYZ + ZZZ
         OCT(1,I) = FIVE * XXX - THREE * DUMX
         OCT(2,I) = FIVE * YYY - THREE * DUMY
         OCT(3,I) = FIVE * ZZZ - THREE * DUMZ
         OCT(4,I) = FIVE * XXY - DUMY
         OCT(5,I) = FIVE * XXZ - DUMZ
         OCT(6,I) = FIVE * XYY - DUMX
         OCT(7,I) = FIVE * YYZ - DUMZ
         OCT(8,I) = FIVE * XZZ - DUMX
         OCT(9,I) = FIVE * YZZ - DUMY
         OCT(10,I) = FIVE * XYZ
242   CONTINUE
C
      IC1=0
      DO 400 IM = 1,NFRG
         DO 390 IP = 1,NMPTS(IM)
            IC1=IC1+1
            IF(.NOT.DOOCTU(IC1))GO TO 390
            X1 = EFC(1,IC1)
            Y1 = EFC(2,IC1)
            Z1 = EFC(3,IC1)
            XXX1 = OCT(1,IC1)
            YYY1 = OCT(2,IC1)
            ZZZ1 = OCT(3,IC1)
            XXY1 = OCT(4,IC1)
            XXZ1 = OCT(5,IC1)
            XYY1 = OCT(6,IC1)
            YYZ1 = OCT(7,IC1)
            XZZ1 = OCT(8,IC1)
            YZZ1 = OCT(9,IC1)
            XYZ1 = OCT(10,IC1)
            IC2=0
            DO 380 JM = 1,NFRG
               IF (IM.EQ.JM)THEN
                 IC2=IC2+NMPTS(JM)
                 GO TO 380
               END IF
               CFXXX = ZERO
               CFYYY = ZERO
               CFZZZ = ZERO
               CFXXY = ZERO
               CFXXZ = ZERO
               CFXYY = ZERO
               CFYYZ = ZERO
               CFXZZ = ZERO
               CFYZZ = ZERO
               CFXYZ = ZERO
               DO 370 JP = 1,NMPTS(JM)
                  IC2=IC2+1
            IF(.NOT.DOMONO(IC2))GO TO 370
                  X2 = EFC(1,IC2)
                  Y2 = EFC(2,IC2)
                  Z2 = EFC(3,IC2)
                  Q2 = EFCHG(1,IC2)+EFCHG(2,IC2)
C REMEMBER FIELDS AT 1 DUE TO MULTIPOLES AT 2
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  XX = X*X
                  YY = Y*Y
                  ZZ = Z*Z
                  XY = X*Y
                  XZ = X*Z
                  YZ = Y*Z
                  XXX = XX*X
                  YYY = YY*Y
                  ZZZ = ZZ*Z
                  XXY = XX*Y
                  XXZ = XX*Z
                  XYY = XY*Y
                  YYZ = YY*Z
                  XZZ = XZ*Z
                  YZZ = YZ*Z
                  XYZ = XY*Z
                  R2 = XX+YY+ZZ
                  R = SQRT(R2)
                  R3 = R2*R
                  R5 = R2*R3
                  R7 = R2*R5
                  R9 = R2*R7
C
      DUM = Q2/R7/FIVE
      DUM9 = SEVEN*Q2/R9/FIVE
      RRX = R2*X
      RRY = R2*Y
      RRZ = R2*Z
      CFXXX = FIVE*XXX-THREE*RRX
      CFYYY = FIVE*YYY-THREE*RRY
      CFZZZ = FIVE*ZZZ-THREE*RRZ
      CFXXY = FIVE*XXY-RRY
      CFXXZ = FIVE*XXZ-RRZ
      CFXYY = FIVE*XYY-RRX
      CFYYZ = FIVE*YYZ-RRZ
      CFXZZ = FIVE*XZZ-RRX
      CFYZZ = FIVE*YZZ-RRY
      CFXYZ = FIVE*XYZ
C
C.... CHARGE(JM) - OCTUPOLE(IM)
C
               TERM = DUM9*(XXX1*CFXXX + YYY1*CFYYY + ZZZ1*CFZZZ
     1              + THREE*(XXY1*CFXXY + XXZ1*CFXXZ + XYY1*CFXYY)
     1              + THREE*(YYZ1*CFYYZ + XZZ1*CFXZZ + YZZ1*CFYZZ)
     1              + SIX*XYZ1*CFXYZ)
               TERMX = TERM*X
               TERMY = TERM*Y
               TERMZ = TERM*Z
        TMX1 = (XXX1*FIFTEN*XX
     1      + THREE*(XXY1*TEN*XY + XXZ1*TEN*XZ + XYY1*FIVE*YY
     1      + XZZ1*FIVE*ZZ) + THIRTY*XYZ1*YZ)*DUM
        TMX2 = (-XXX1*SIX*XX - YYY1*SIX*XY - ZZZ1*SIX*XZ
     1      + THREE*(-XXY1*TWO*XY - XXZ1*TWO*XZ - XYY1*TWO*XX
     1      - XZZ1*TWO*XX - YYZ1*TWO*XZ - YZZ1*TWO*XY))*DUM
        TMY1 = (YYY1*FIFTEN*YY
     1      + THREE*(XYY1*TEN*XY + YYZ1*TEN*YZ + XXY1*FIVE*XX
     1      + YZZ1*FIVE*ZZ) + THIRTY*XYZ1*XZ)*DUM
        TMY2 = (-YYY1*SIX*YY - XXX1*SIX*XY - ZZZ1*SIX*YZ
     1      + THREE*(-XYY1*TWO*XY - YYZ1*TWO*YZ - XXY1*TWO*YY
     1      - YZZ1*TWO*YY - XXZ1*TWO*YZ - XZZ1*TWO*XY))*DUM
        TMZ1 = (ZZZ1*FIFTEN*ZZ
     1      + THREE*(YZZ1*TEN*YZ + XZZ1*TEN*XZ + YYZ1*FIVE*YY
     1      + XXZ1*FIVE*XX) + THIRTY*XYZ1*XY)*DUM
        TMZ2 = (-ZZZ1*SIX*ZZ - YYY1*SIX*YZ - XXX1*SIX*XZ
     1      + THREE*(-YZZ1*TWO*YZ - XZZ1*TWO*XZ - YYZ1*TWO*ZZ
     1      - XXZ1*TWO*ZZ - XYY1*TWO*XZ - XXY1*TWO*YZ))*DUM
        TMX = TMX1 + TMX2
        TMY = TMY1 + TMY2
        TMZ = TMZ1 + TMZ2
            EF3(1,IC2)=EF3(1,IC2)+TMX-TERMX
            EF3(2,IC2)=EF3(2,IC2)+TMY-TERMY
            EF3(3,IC2)=EF3(3,IC2)+TMZ-TERMZ
            EF3(1,IC1)=EF3(1,IC1)-TMX+TERMX
            EF3(2,IC1)=EF3(2,IC1)-TMY+TERMY
            EF3(3,IC1)=EF3(3,IC1)-TMZ+TERMZ
      ATORQ(1,IM)=ATORQ(1,IM)+Y*TMZ1-Z*TMY1
      ATORQ(2,IM)=ATORQ(2,IM)+Z*TMX1-X*TMZ1
      ATORQ(3,IM)=ATORQ(3,IM)+X*TMY1-Y*TMX1
370            CONTINUE
380         CONTINUE
390      CONTINUE
400   CONTINUE
      RETURN
      END
C*MODULE EFELEC   *DECK DDPQUD
      SUBROUTINE DDPQUD(EF3,QUA)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      CHARACTER*8 FRGNME
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION EF3(3,*),QUA(6,*)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      DATA PT5,ONEPT5,TWO/0.5D+00,1.5D+00,2.0D+00/
      DATA FIFTEN/15.0D+00/
      DATA THREE,FIVE,SIX,SEVEN/3.0D+00,5.0D+00,6.0D+00,7.0D+00/
C
C.... REPLACE THE SECOND MOMENTS OF THE CHARGE DISTRIBUTION ....
C.... WITH ELECTRIC QUADRUPOLE MOMENT TENSORS               ....
C.... SEE BUCKINGHAM, ET AL, "MOLECULAR QUADRUPOLE MOMENTS" ....
C.... QUART. REV. (LONDON) -13-, 183-214 (1959)             ....
C.... EQUATION 15.                                          ....
C
      DO 210 I = 1,NMTTPT
        IF(.NOT.DOQUAD(I))GO TO 210
        XX = EFQAD(1,I)
        YY = EFQAD(2,I)
        ZZ = EFQAD(3,I)
        XY = EFQAD(4,I)
        XZ = EFQAD(5,I)
        YZ = EFQAD(6,I)
        DUM = XX + YY + ZZ
        QUA(1,I) = PT5 * (THREE * XX - DUM)
        QUA(2,I) = PT5 * (THREE * YY - DUM)
        QUA(3,I) = PT5 * (THREE * ZZ - DUM)
        QUA(4,I) = ONEPT5 * XY
        QUA(5,I) = ONEPT5 * XZ
        QUA(6,I) = ONEPT5 * YZ
210   CONTINUE
C
      IC1=0
      DO 400 IM = 1,NFRG
         DO 390 IP = 1,NMPTS(IM)
            IC1=IC1+1
            IF(.NOT.DOQUAD(IC1))GO TO 390
            X1 = EFC(1,IC1)
            Y1 = EFC(2,IC1)
            Z1 = EFC(3,IC1)
            XX1 = QUA(1,IC1)
            YY1 = QUA(2,IC1)
            ZZ1 = QUA(3,IC1)
            XY1 = QUA(4,IC1)
            XZ1 = QUA(5,IC1)
            YZ1 = QUA(6,IC1)
            IC2=0
            DO 380 JM = 1,NFRG
               IF (IM.EQ.JM)THEN
                 IC2=IC2+NMPTS(JM)
                 GO TO 380
               END IF
               DO 370 JP = 1,NMPTS(JM)
                  IC2=IC2+1
            IF(.NOT.DODIPO(IC2))GO TO 370
                  X2 = EFC(1,IC2)
                  Y2 = EFC(2,IC2)
                  Z2 = EFC(3,IC2)
                  DX2 = EFDIP(1,IC2)
                  DY2 = EFDIP(2,IC2)
                  DZ2 = EFDIP(3,IC2)
C REMEMBER FIELDS AT 1 DUE TO MULTIPOLES AT 2
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  XX = X*X
                  YY = Y*Y
                  ZZ = Z*Z
                  XY = X*Y
                  XZ = X*Z
                  YZ = Y*Z
                  R2 = XX+YY+ZZ
                  R = SQRT(R2)
                  R3 = R2*R
                  R5 = R2*R3
                  R7 = R2*R5
                  R9 = R2*R7
C
C.... POTENTIAL, FIELD, AND FIELD GRADIENT DUE TO DIPOLE MOMENTS ON JM
C
      DOT = DX2*X + DY2*Y + DZ2*Z
C
      DUM9 = SEVEN/R9
      DUMA = FIFTEN/R7
      AFXX = (TWO*DX2*X)*DUMA
      AFYY = (TWO*DY2*Y)*DUMA
      AFZZ = (TWO*DZ2*Z)*DUMA
      AFXY = (DX2*Y+DY2*X)*DUMA
      AFXZ = (DX2*Z+DZ2*X)*DUMA
      AFYZ = (DY2*Z+DZ2*Y)*DUMA
        TERM = XX1*AFXX + YY1*AFYY + ZZ1*AFZZ
     1       + TWO*(XY1*AFXY + XZ1*AFXZ + YZ1*AFYZ)
        TERM = TERM/3
C
      TERMX = TWO*(XX1*DX2+XY1*DY2+XZ1*DZ2)/R5
      TERMY = TWO*(YY1*DY2+XY1*DX2+YZ1*DZ2)/R5
      TERMZ = TWO*(ZZ1*DZ2+YZ1*DY2+XZ1*DX2)/R5
C
      TORQX = TWO*(XX1*X+XY1*Y+XZ1*Z)/R5
      TORQY = TWO*(YY1*Y+XY1*X+YZ1*Z)/R5
      TORQZ = TWO*(ZZ1*Z+YZ1*Y+XZ1*X)/R5
C
      DDOT=XX1*XX+YY1*YY+ZZ1*ZZ+TWO*(XY1*XY+XZ1*XZ+YZ1*YZ)
      TERMC = FIVE*DOT*DUM9*DDOT
      TORQC = FIVE*DDOT/R7
C
      TERMDX = FIVE*(XX1*(TWO*X*DOT+XX*DX2)+YY1*YY*DX2+ZZ1*ZZ*DX2
     $     +TWO*(XY1*(Y*DOT+XY*DX2)+XZ1*(Z*DOT+XZ*DX2)+YZ1*YZ*DX2))/R7
      TERMDY = FIVE*(YY1*(TWO*Y*DOT+YY*DY2)+XX1*XX*DY2+ZZ1*ZZ*DY2
     $     +TWO*(XY1*(X*DOT+XY*DY2)+YZ1*(Z*DOT+YZ*DY2)+XZ1*XZ*DY2))/R7
      TERMDZ = FIVE*(ZZ1*(TWO*Z*DOT+ZZ*DZ2)+XX1*XX*DZ2+YY1*YY*DZ2
     $     +TWO*(YZ1*(Y*DOT+YZ*DZ2)+XZ1*(X*DOT+XZ*DZ2)+XY1*XY*DZ2))/R7
C
      FXX = DUMA*DOT*XX-SIX*DX2*X/R5
      FYY = DUMA*DOT*YY-SIX*DY2*Y/R5
      FZZ = DUMA*DOT*ZZ-SIX*DZ2*Z/R5
      FXY = DUMA*DOT*XY-THREE*(DX2*Y+DY2*X)/R5
      FXZ = DUMA*DOT*XZ-THREE*(DX2*Z+DZ2*X)/R5
      FYZ = DUMA*DOT*YZ-THREE*(DZ2*Y+DY2*Z)/R5
C
C
            EF3(1,IC2)=EF3(1,IC2)-(TERM-TERMC)*X+TERMX-TERMDX
            EF3(2,IC2)=EF3(2,IC2)-(TERM-TERMC)*Y+TERMY-TERMDY
            EF3(3,IC2)=EF3(3,IC2)-(TERM-TERMC)*Z+TERMZ-TERMDZ
            EF3(1,IC1)=EF3(1,IC1)+(TERM-TERMC)*X-TERMX+TERMDX
            EF3(2,IC1)=EF3(2,IC1)+(TERM-TERMC)*Y-TERMY+TERMDY
            EF3(3,IC1)=EF3(3,IC1)+(TERM-TERMC)*Z-TERMZ+TERMDZ
C
      TDX = TORQY*DZ2-TORQZ*DY2+TORQC*(Z*DY2-Y*DZ2)
      TDY = TORQZ*DX2-TORQX*DZ2+TORQC*(X*DZ2-Z*DX2)
      TDZ = TORQX*DY2-TORQY*DX2+TORQC*(Y*DX2-X*DY2)
      ATORQ(1,JM)=ATORQ(1,JM)+TDX
      ATORQ(2,JM)=ATORQ(2,JM)+TDY
      ATORQ(3,JM)=ATORQ(3,JM)+TDZ
      ATORQ(1,IM)=ATORQ(1,IM)-TWO*(YZ1*FYY+ZZ1*FYZ+XZ1*FXY
     $                       -YY1*FYZ-YZ1*FZZ-XY1*FXZ)/THREE
      ATORQ(2,IM)=ATORQ(2,IM)-TWO*(XZ1*FZZ+XX1*FXZ+XY1*FYZ
     $                       -ZZ1*FXZ-XZ1*FXX-YZ1*FXY)/THREE
      ATORQ(3,IM)=ATORQ(3,IM)-TWO*(XY1*FXX+YY1*FXY+YZ1*FXZ
     $                       -XX1*FXY-XY1*FYY-XZ1*FYZ)/THREE
C     ATORQ(1,IM)=ATORQ(1,IM)-Z*(TERMY-TERMDY)+Y*(TERMZ-TERMDZ)-TDX
C     ATORQ(2,IM)=ATORQ(2,IM)-X*(TERMZ-TERMDZ)+Z*(TERMX-TERMDX)-TDY
C     ATORQ(3,IM)=ATORQ(3,IM)-Y*(TERMX-TERMDX)+X*(TERMY-TERMDY)-TDZ
370            CONTINUE
380         CONTINUE
390      CONTINUE
400   CONTINUE
C
      RETURN
      END
C*MODULE EFELEC   *DECK DQDQD
      SUBROUTINE DQDQD(EF3,QUA)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION NINE
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      CHARACTER*8 FRGNME
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION EF3(3,*),QUA(6,*)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      DATA PT5,ONEPT5,TWO/0.5D+00,1.5D+00,2.0D+00/
      DATA THREE,NINE/3.0D+00,9.0D+00/
C
C.... REPLACE THE SECOND MOMENTS OF THE CHARGE DISTRIBUTION ....
C.... WITH ELECTRIC QUADRUPOLE MOMENT TENSORS               ....
C.... SEE BUCKINGHAM, ET AL, "MOLECULAR QUADRUPOLE MOMENTS" ....
C.... QUART. REV. (LONDON) -13-, 183-214 (1959)             ....
C.... EQUATION 15.                                          ....
C
      TOTHR=TWO/THREE
      DO 210 I = 1,NMTTPT
        IF(.NOT.DOQUAD(I))GO TO 210
        XX = EFQAD(1,I)
        YY = EFQAD(2,I)
        ZZ = EFQAD(3,I)
        XY = EFQAD(4,I)
        XZ = EFQAD(5,I)
        YZ = EFQAD(6,I)
        DUM = XX + YY + ZZ
        QUA(1,I) = PT5 * (THREE * XX - DUM)
        QUA(2,I) = PT5 * (THREE * YY - DUM)
        QUA(3,I) = PT5 * (THREE * ZZ - DUM)
        QUA(4,I) = ONEPT5 * XY
        QUA(5,I) = ONEPT5 * XZ
        QUA(6,I) = ONEPT5 * YZ
210   CONTINUE
C
      IC1=0
      DO 400 IM = 1,NFRG
         DO 390 IP = 1,NMPTS(IM)
            IC1=IC1+1
            IF(.NOT.DOQUAD(IC1))GO TO 390
            X1 = EFC(1,IC1)
            Y1 = EFC(2,IC1)
            Z1 = EFC(3,IC1)
            XX1 = QUA(1,IC1)
            YY1 = QUA(2,IC1)
            ZZ1 = QUA(3,IC1)
            XY1 = QUA(4,IC1)
            XZ1 = QUA(5,IC1)
            YZ1 = QUA(6,IC1)
            IC2=0
            DO 380 JM = 1,NFRG
               IF (IM.GE.JM)THEN
                 IC2=IC2+NMPTS(JM)
                 GO TO 380
               END IF
               DO 370 JP = 1,NMPTS(JM)
                  IC2=IC2+1
            IF(.NOT.DOQUAD(IC2))GO TO 370
                  X2 = EFC(1,IC2)
                  Y2 = EFC(2,IC2)
                  Z2 = EFC(3,IC2)
                  XX2 = QUA(1,IC2)
                  YY2 = QUA(2,IC2)
                  ZZ2 = QUA(3,IC2)
                  XY2 = QUA(4,IC2)
                  XZ2 = QUA(5,IC2)
                  YZ2 = QUA(6,IC2)
C REMEMBER FIELDS AT 1 DUE TO MULTIPOLES AT 2
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  XX = X*X
                  YY = Y*Y
                  ZZ = Z*Z
                  XY = X*Y
                  XZ = X*Z
                  YZ = Y*Z
                  R2 = XX+YY+ZZ
                  R = SQRT(R2)
                  R3 = R2*R
                  R5 = R2*R3
                  R7 = R2*R5
                  R9 = R2*R7
                  R11 = R2*R9
C
                  QUAD1 = XX1*XX2 + TWO*XY1*XY2 + TWO*XZ1*XZ2
                  QUAD1 = QUAD1 + YY1*YY2 + TWO*YZ1*YZ2 + ZZ1*ZZ2
                  DQU1 = QUAD1 * 30.0D+00 / R7
                  VAL2X = X*XX2 + Y*XY2 + Z*XZ2
                  VAL2Y = X*XY2 + Y*YY2 + Z*YZ2
                  VAL2Z = X*XZ2 + Y*YZ2 + Z*ZZ2
                  VAL2 = X*VAL2X + Y*VAL2Y + Z*VAL2Z
                  VAL1X = X*XX1+Y*XY1+Z*XZ1
                  VAL1Y = X*XY1+Y*YY1+Z*YZ1
                  VAL1Z = X*XZ1+Y*YZ1+Z*ZZ1
                  VAL1 = X*VAL1X + Y*VAL1Y + Z*VAL1Z
                  FACT1X = X*XX1 + Y*XY1 + Z*XZ1
                  FACT2X = X*XX2 + Y*XY2 + Z*XZ2
                  FACT1Y = X*XY1 + Y*YY1 + Z*YZ1
                  FACT2Y = X*XY2 + Y*YY2 + Z*YZ2
                  FACT1Z = X*XZ1 + Y*YZ1 + Z*ZZ1
                  FACT2Z = X*XZ2 + Y*YZ2 + Z*ZZ2
                  DQU2 =  VAL1*VAL2*945.0D+00 / R11
                  DQU2X =  (FACT1X*VAL2+FACT2X*VAL1) * 210.0D+00 / R9
                  DQU2Y =  (FACT1Y*VAL2+FACT2Y*VAL1) * 210.0D+00 / R9
                  DQU2Z =  (FACT1Z*VAL2+FACT2Z*VAL1) * 210.0D+00 / R9
                  DQU3 = FACT1X*FACT2X+FACT1Y*FACT2Y+FACT1Z*FACT2Z
                  DQU3 = DQU3 * 420.0D+00 / R9
                  DQU3X = (XX1*FACT2X + XX2*FACT1X
     1                  + XY1*FACT2Y + XY2*FACT1Y
     1                  + XZ1*FACT2Z + XZ2*FACT1Z) * 60.0D+00 / R7
                  DQU3Y = (XY1*FACT2X + XY2*FACT1X
     1                  + YY1*FACT2Y + YY2*FACT1Y
     1                  + YZ1*FACT2Z + YZ2*FACT1Z) * 60.0D+00 / R7
                  DQU3Z = (XZ1*FACT2X + XZ2*FACT1X
     1                  + YZ1*FACT2Y + YZ2*FACT1Y
     1                  + ZZ1*FACT2Z + ZZ2*FACT1Z) * 60.0D+00 / R7
       F1XX = TWO*XX1/R5-20.0D+00*X*VAL1X/R7+35.0D+00*XX*VAL1/R9
       F1YY = TWO*YY1/R5-20.0D+00*Y*VAL1Y/R7+35.0D+00*YY*VAL1/R9
       F1ZZ = TWO*ZZ1/R5-20.0D+00*Z*VAL1Z/R7+35.0D+00*ZZ*VAL1/R9
       F1XY = TWO*XY1/R5-10.0D+00*(Y*VAL1X+X*VAL1Y)/R7
     *                  +35.0D+00*XY*VAL1/R9
       F1XZ = TWO*XZ1/R5-10.0D+00*(Z*VAL1X+X*VAL1Z)/R7
     *                  +35.0D+00*XZ*VAL1/R9
       F1YZ = TWO*YZ1/R5-10.0D+00*(Z*VAL1Y+Y*VAL1Z)/R7
     *                  +35.0D+00*YZ*VAL1/R9
C
       F2XX = TWO*XX2/R5-20.0D+00*X*VAL2X/R7+35.0D+00*XX*VAL2/R9
       F2YY = TWO*YY2/R5-20.0D+00*Y*VAL2Y/R7+35.0D+00*YY*VAL2/R9
       F2ZZ = TWO*ZZ2/R5-20.0D+00*Z*VAL2Z/R7+35.0D+00*ZZ*VAL2/R9
       F2XY = TWO*XY2/R5-10.0D+00*(Y*VAL2X+X*VAL2Y)/R7
     *                  +35.0D+00*XY*VAL2/R9
       F2XZ = TWO*XZ2/R5-10.0D+00*(Z*VAL2X+X*VAL2Z)/R7
     *                  +35.0D+00*XZ*VAL2/R9
       F2YZ = TWO*YZ2/R5-10.0D+00*(Z*VAL2Y+Y*VAL2Z)/R7
     *                  +35.0D+00*YZ*VAL2/R9
C
         EF3(1,IC2)=EF3(1,IC2)+(X*(DQU1+DQU2-DQU3)-DQU2X+DQU3X)/NINE
         EF3(2,IC2)=EF3(2,IC2)+(Y*(DQU1+DQU2-DQU3)-DQU2Y+DQU3Y)/NINE
         EF3(3,IC2)=EF3(3,IC2)+(Z*(DQU1+DQU2-DQU3)-DQU2Z+DQU3Z)/NINE
         EF3(1,IC1)=EF3(1,IC1)+(-X*(DQU1+DQU2-DQU3)+DQU2X-DQU3X)/NINE
         EF3(2,IC1)=EF3(2,IC1)+(-Y*(DQU1+DQU2-DQU3)+DQU2Y-DQU3Y)/NINE
         EF3(3,IC1)=EF3(3,IC1)+(-Z*(DQU1+DQU2-DQU3)+DQU2Z-DQU3Z)/NINE
      ATORQ(1,JM)=ATORQ(1,JM)+(YY2*F1YZ+YZ2*F1ZZ+XY2*F1XZ
     1            -(YZ2*F1YY+ZZ2*F1YZ+XZ2*F1XY))*TOTHR
      ATORQ(2,JM)=ATORQ(2,JM)+(ZZ2*F1XZ+XZ2*F1XX+YZ2*F1XY
     1            -(XZ2*F1ZZ+XX2*F1XZ+XY2*F1YZ))*TOTHR
      ATORQ(3,JM)=ATORQ(3,JM)+(XX2*F1XY+XY2*F1YY+XZ2*F1YZ
     1            -(XY2*F1XX+YY2*F1XY+YZ2*F1XZ))*TOTHR
C
      ATORQ(1,IM)=ATORQ(1,IM)+(YY1*F2YZ+YZ1*F2ZZ+XY1*F2XZ
     1            -(YZ1*F2YY+ZZ1*F2YZ+XZ1*F2XY))*TOTHR
      ATORQ(2,IM)=ATORQ(2,IM)+(ZZ1*F2XZ+XZ1*F2XX+YZ1*F2XY
     1            -(XZ1*F2ZZ+XX1*F2XZ+XY1*F2YZ))*TOTHR
      ATORQ(3,IM)=ATORQ(3,IM)+(XX1*F2XY+XY1*F2YY+XZ1*F2YZ
     1            -(XY1*F2XX+YY1*F2XY+YZ1*F2XZ))*TOTHR
C     TDX = (Z*(DQU2Y-DQU3Y)-Y*(DQU2Z-DQU3Z))/18.0D+00
C     TDY = (X*(DQU2Z-DQU3Z)-Z*(DQU2X-DQU3X))/18.0D+00
C     TDZ = (Y*(DQU2X-DQU3X)-X*(DQU2Y-DQU3Y))/18.0D+00
C     ATORQ(1,JM)=ATORQ(1,JM)+TDX
C     ATORQ(2,JM)=ATORQ(2,JM)+TDY
C     ATORQ(3,JM)=ATORQ(3,JM)+TDZ
C     ATORQ(1,IM)=ATORQ(1,IM)+TDX
C     ATORQ(2,IM)=ATORQ(2,IM)+TDY
C     ATORQ(3,IM)=ATORQ(3,IM)+TDZ
370            CONTINUE
380         CONTINUE
390      CONTINUE
400   CONTINUE
C
C
      RETURN
      END
C*MODULE EFELEC   *DECK DCHIND
      SUBROUTINE DCHIND(EF3)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      CHARACTER*8 FRGNME,POLNAM
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION EF3(3,*)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /EFPPAR/ EFP(3,MXFGPT),EFPOL(9,MXFGPT),
     *                ENO,DIND(3,MXFGPT),DINDD(3,MXFGPT),POLNAM(MXFGPT)
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      DATA ONE,TWO/1.0D+00,2.0D+00/
      DATA PT5/0.5D+00/
      DATA THREE,FOUR/3.0D+00,4.0D+00/
      DATA ZERO/0.0D+00/
C
      IC1=0
      DO 400 IM = 1,NFRG
         DO 390 IP = 1,NPPTS(IM)
            IC1=IC1+1
            X1 = EFP(1,IC1)
            Y1 = EFP(2,IC1)
            Z1 = EFP(3,IC1)
C           DX1 = DIND(1,IC1)
C           DY1 = DIND(2,IC1)
C           DZ1 = DIND(3,IC1)
            DX1 = PT5*(DIND(1,IC1)+DINDD(1,IC1))
            DY1 = PT5*(DIND(2,IC1)+DINDD(2,IC1))
            DZ1 = PT5*(DIND(3,IC1)+DINDD(3,IC1))
            IC2=0
            DO 380 JM = 1,NFRG
               IF (IM.EQ.JM)THEN
                 IC2=IC2+NMPTS(JM)
                 GO TO 380
               END IF
               CFR = ZERO
               DO 370 JP = 1,NMPTS(JM)
                  IC2=IC2+1
            IF(.NOT.DOMONO(IC2))GO TO 370
                  X2 = EFC(1,IC2)
                  Y2 = EFC(2,IC2)
                  Z2 = EFC(3,IC2)
C REMEMBER FIELDS AT 1 DUE TO MULTIPOLES AT 2
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  XX = X*X
                  YY = Y*Y
                  ZZ = Z*Z
                  R2 = XX+YY+ZZ
                  R = SQRT(R2)
                  R3 = R2*R
                  R5 = R2*R3
                  Q2 = EFCHG(1,IC2)+EFCHG(2,IC2)
                  IF(ICHGP.EQ.3.OR.ICHGP.EQ.2) THEN
                     EX = ZERO
                     CF = ZERO
                  ELSE
                     EX = EFATRM(IC2)
                     CF = EFBTRM(IC2)
                  END IF
C
C.... POTENTIAL, FIELD, FIELD GRADIENT, AND FIELD SECOND DERIVATIVE
C.... AT POINT 'IP' IN MOLECULE 'IM' DUE TO POINTS IN JM (ACCUMULATED)
C
      DUM = Q2/R3
      DOT = DX1*X + DY1*Y + DZ1*Z
      IF(ICHGP.EQ.3.OR.ICHGP.EQ.2) THEN
         CFR =  DUM
         CN1 = (THREE/R5)*DOT*Q2
      ELSE
         EX2=EXP(-EX*R2)
         SCREEN=1.0D+00-CF*EX2
         CFR =  DUM*SCREEN - TWO*Q2*CF*EX*EX2/R
         CN1 = (THREE*SCREEN/R5 - FOUR*CF*EX*EX2*(ONE/R3 + EX/R))*DOT*Q2
      END IF
      CONX = DX1*CFR - X*CN1
      CONY = DY1*CFR - Y*CN1
      CONZ = DZ1*CFR - Z*CN1
      IIC1=IC1+NMTTPT
      EF3(1,IC2) = EF3(1,IC2) + CONX
      EF3(2,IC2) = EF3(2,IC2) + CONY
      EF3(3,IC2) = EF3(3,IC2) + CONZ
      EF3(1,IIC1) = EF3(1,IIC1) - CONX
      EF3(2,IIC1) = EF3(2,IIC1) - CONY
      EF3(3,IIC1) = EF3(3,IIC1) - CONZ
      ATORQ(1,IM) = ATORQ(1,IM) - CFR*(DY1*Z-DZ1*Y)
      ATORQ(2,IM) = ATORQ(2,IM) - CFR*(DZ1*X-DX1*Z)
      ATORQ(3,IM) = ATORQ(3,IM) - CFR*(DX1*Y-DY1*X)
C
370            CONTINUE
380         CONTINUE
390      CONTINUE
400   CONTINUE
C
      RETURN
      END
C*MODULE EFELEC   *DECK DININ
      SUBROUTINE DININ(EF3)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      CHARACTER*8 POLNAM
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION EF3(3,*)
C
      COMMON /EFPPAR/ EFP(3,MXFGPT),EFPOL(9,MXFGPT),
     *                ENO,DIND(3,MXFGPT),DINDD(3,MXFGPT),POLNAM(MXFGPT)
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      DATA FIFTEN/15.0D+00/, THREE/3.0D+00/, ZERO/0.0D+00/
C
      IC1=0
      DO 400 IM = 1,NFRG
         DO 390 IP = 1,NPPTS(IM)
            IC1=IC1+1
            X1 = EFP(1,IC1)
            Y1 = EFP(2,IC1)
            Z1 = EFP(3,IC1)
            DX1 = DIND(1,IC1)
            DY1 = DIND(2,IC1)
            DZ1 = DIND(3,IC1)
            IC2=0
            DO 380 JM = 1,NFRG
C DOUBLE COUNTING
C              IF (IM.EQ.JM)THEN
               IF (IM.GE.JM)THEN
                 IC2=IC2+NPPTS(JM)
                 GO TO 380
               END IF
               DFX2 = ZERO
               DFY2 = ZERO
               DFZ2 = ZERO
               DFX1 = ZERO
               DFY1 = ZERO
               DFZ1 = ZERO
               DO 370 JP = 1,NPPTS(JM)
                  IC2=IC2+1
                  X2 = EFP(1,IC2)
                  Y2 = EFP(2,IC2)
                  Z2 = EFP(3,IC2)
C                 DX2 = DIND(1,IC2)
C                 DY2 = DIND(2,IC2)
C                 DZ2 = DIND(3,IC2)
                  DX2 = DINDD(1,IC2)
                  DY2 = DINDD(2,IC2)
                  DZ2 = DINDD(3,IC2)
C REMEMBER FIELDS AT 1 DUE TO MULTIPOLES AT 2
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  XX = X*X
                  YY = Y*Y
                  ZZ = Z*Z
                  XY = X*Y
                  XZ = X*Z
                  YZ = Y*Z
                  R2 = XX+YY+ZZ
                  R = SQRT(R2)
                  R3 = R2*R
                  R5 = R2*R3
                  R7 = R2*R5
C
      DOT1 = DX1*X + DY1*Y + DZ1*Z
      DOT2 = DX2*X + DY2*Y + DZ2*Z
      DOTM = DX2*DX1 + DY2*DY1 + DZ2*DZ1
      DOTXY=DY2*DX1+DY1*DX2
      DOTXZ=DZ2*DX1+DZ1*DX2
      DOTYZ=DY2*DZ1+DY1*DZ2
      DOTX = 2.0D+00*DX2*DX1*X+DOTXY*Y+DOTXZ*Z
      DOTY = 2.0D+00*DY2*DY1*Y+DOTXY*X+DOTYZ*Z
      DOTZ = 2.0D+00*DZ2*DZ1*Z+DOTXZ*X+DOTYZ*Y
      DUM = THREE/R5
      DUM2=DUM*DOT2
      DUM1=DUM*DOT1
      TEMP = DX1*DX2*XX+DY1*DY2*YY+DZ1*DZ2*ZZ
     * +(DX1*DY2+DY1*DX2)*XY+(DX1*DZ2+DZ1*DX2)*XZ
     * +(DY1*DZ2+DZ1*DY2)*YZ
      DUM7 = FIFTEN*TEMP/R7
      DFX2 = -DX2/R3 + X*DUM2
      DFY2 = -DY2/R3 + Y*DUM2
      DFZ2 = -DZ2/R3 + Z*DUM2
      DFX1 = -DX1/R3 + X*DUM1
      DFY1 = -DY1/R3 + Y*DUM1
      DFZ1 = -DZ1/R3 + Z*DUM1
      CONX = DUM*(DOTM*X+DOTX)-DUM7*X
      CONY = DUM*(DOTM*Y+DOTY)-DUM7*Y
      CONZ = DUM*(DOTM*Z+DOTZ)-DUM7*Z
      IIC1=IC1+NMTTPT
      IIC2=IC2+NMTTPT
            EF3(1,IIC2)=EF3(1,IIC2)+CONX
            EF3(2,IIC2)=EF3(2,IIC2)+CONY
            EF3(3,IIC2)=EF3(3,IIC2)+CONZ
            EF3(1,IIC1)=EF3(1,IIC1)-CONX
            EF3(2,IIC1)=EF3(2,IIC1)-CONY
            EF3(3,IIC1)=EF3(3,IIC1)-CONZ
C
      ATORQ(1,IM)=ATORQ(1,IM)-DY1*DFZ2+DZ1*DFY2
      ATORQ(2,IM)=ATORQ(2,IM)-DZ1*DFX2+DX1*DFZ2
      ATORQ(3,IM)=ATORQ(3,IM)-DX1*DFY2+DY1*DFX2
      ATORQ(1,JM)=ATORQ(1,JM)-DY2*DFZ1+DZ2*DFY1
      ATORQ(2,JM)=ATORQ(2,JM)-DZ2*DFX1+DX2*DFZ1
      ATORQ(3,JM)=ATORQ(3,JM)-DX2*DFY1+DY2*DFX1
C
370            CONTINUE
380         CONTINUE
390      CONTINUE
400   CONTINUE
C
      RETURN
      END
C*MODULE EFELEC   *DECK DDPIND
      SUBROUTINE DDPIND(EF3)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      CHARACTER*8 FRGNME,POLNAM
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION EF3(3,*)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /EFPPAR/ EFP(3,MXFGPT),EFPOL(9,MXFGPT),
     *                ENO,DIND(3,MXFGPT),DINDD(3,MXFGPT),POLNAM(MXFGPT)
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      DATA THREE/3.0D+00/, ZERO/0.0D+00/
      DATA PT5/0.5D+00/
C
      IC1=0
      DO 400 IM = 1,NFRG
         DO 390 IP = 1,NPPTS(IM)
            IC1=IC1+1
            X1 = EFP(1,IC1)
            Y1 = EFP(2,IC1)
            Z1 = EFP(3,IC1)
C           DX1 = (DIND(1,IC1))
C           DY1 = (DIND(2,IC1))
C           DZ1 = (DIND(3,IC1))
            DX1 = PT5*(DIND(1,IC1)+DINDD(1,IC1))
            DY1 = PT5*(DIND(2,IC1)+DINDD(2,IC1))
            DZ1 = PT5*(DIND(3,IC1)+DINDD(3,IC1))
            IC2=0
            DO 380 JM = 1,NFRG
               IF(IM.EQ.JM) THEN
                 IC2=IC2+NMPTS(JM)
                 GO TO 380
               END IF
               DFX2 = ZERO
               DFY2 = ZERO
               DFZ2 = ZERO
               DFX1 = ZERO
               DFY1 = ZERO
               DFZ1 = ZERO
               DO 370 JP = 1,NMPTS(JM)
                  IC2=IC2+1
                  IF(.NOT.DODIPO(IC2))GO TO 370
                  X2 = EFC(1,IC2)
                  Y2 = EFC(2,IC2)
                  Z2 = EFC(3,IC2)
                  DX2 = EFDIP(1,IC2)
                  DY2 = EFDIP(2,IC2)
                  DZ2 = EFDIP(3,IC2)
C REMEMBER FIELDS AT 1 DUE TO MULTIPOLES AT 2
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  XX = X*X
                  YY = Y*Y
                  ZZ = Z*Z
                  XY = X*Y
                  XZ = X*Z
                  YZ = Y*Z
                  R2 = XX+YY+ZZ
                  R = SQRT(R2)
                  R3 = R2*R
                  R5 = R2*R3
                  R7 = R2*R5
C
                  DOT1 = DX1*X + DY1*Y + DZ1*Z
                  DOT2 = DX2*X + DY2*Y + DZ2*Z
                  DOTM = DX2*DX1 + DY2*DY1 + DZ2*DZ1
                  DOTXY=DY2*DX1+DY1*DX2
                  DOTXZ=DZ2*DX1+DZ1*DX2
                  DOTYZ=DY2*DZ1+DY1*DZ2
                  DOTX = 2.0D+00*DX2*DX1*X+DOTXY*Y+DOTXZ*Z
                  DOTY = 2.0D+00*DY2*DY1*Y+DOTXY*X+DOTYZ*Z
                  DOTZ = 2.0D+00*DZ2*DZ1*Z+DOTXZ*X+DOTYZ*Y
                  DUM = THREE/R5
                  DUM2=DUM*DOT2
                  DUM1=DUM*DOT1
                  TEMP = DX1*DX2*XX+DY1*DY2*YY+DZ1*DZ2*ZZ
     *             +(DX1*DY2+DY1*DX2)*XY+(DX1*DZ2+DZ1*DX2)*XZ
     *             +(DY1*DZ2+DZ1*DY2)*YZ
                  DUM7 = 15.0D+00*TEMP/R7
                  DFX2 = -DX2/R3 + X*DUM2
                  DFY2 = -DY2/R3 + Y*DUM2
                  DFZ2 = -DZ2/R3 + Z*DUM2
                  DFX1 = -DX1/R3 + X*DUM1
                  DFY1 = -DY1/R3 + Y*DUM1
                  DFZ1 = -DZ1/R3 + Z*DUM1
                  CONX = DUM*(DOTM*X+DOTX)-DUM7*X
                  CONY = DUM*(DOTM*Y+DOTY)-DUM7*Y
                  CONZ = DUM*(DOTM*Z+DOTZ)-DUM7*Z
                  IIC1=IC1+NMTTPT
                  EF3(1,IC2)=EF3(1,IC2)+CONX
                  EF3(2,IC2)=EF3(2,IC2)+CONY
                  EF3(3,IC2)=EF3(3,IC2)+CONZ
                  EF3(1,IIC1)=EF3(1,IIC1)-CONX
                  EF3(2,IIC1)=EF3(2,IIC1)-CONY
                  EF3(3,IIC1)=EF3(3,IIC1)-CONZ
C
                  ATORQ(1,IM)=ATORQ(1,IM)-DY1*DFZ2+DZ1*DFY2
                  ATORQ(2,IM)=ATORQ(2,IM)-DZ1*DFX2+DX1*DFZ2
                  ATORQ(3,IM)=ATORQ(3,IM)-DX1*DFY2+DY1*DFX2
                  ATORQ(1,JM)=ATORQ(1,JM)-DY2*DFZ1+DZ2*DFY1
                  ATORQ(2,JM)=ATORQ(2,JM)-DZ2*DFX1+DX2*DFZ1
                  ATORQ(3,JM)=ATORQ(3,JM)-DX2*DFY1+DY2*DFX1
C
  370          CONTINUE
  380       CONTINUE
  390    CONTINUE
  400 CONTINUE
C
      RETURN
      END
C*MODULE EFELEC   *DECK DQDIND
      SUBROUTINE DQDIND(EF3,QUA)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      CHARACTER*8 FRGNME,POLNAM
C
      LOGICAL DOMONO,DODIPO,DOQUAD,DOOCTU
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      DIMENSION EF3(3,*),QUA(6,*)
C
      COMMON /DOMULT/ DOMONO(MXFGPT),DODIPO(MXFGPT),DOQUAD(MXFGPT),
     *                DOOCTU(MXFGPT)
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /EFPPAR/ EFP(3,MXFGPT),EFPOL(9,MXFGPT),
     *                ENO,DIND(3,MXFGPT),DINDD(3,MXFGPT),POLNAM(MXFGPT)
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
C
      DATA PT5,ONEPT5,TWO/0.5D+00,1.5D+00,2.0D+00/
      DATA FIFTEN/15.0D+00/
      DATA THREE,FIVE,SIX,SEVEN/3.0D+00,5.0D+00,6.0D+00,7.0D+00/
C
C.... REPLACE THE SECOND MOMENTS OF THE CHARGE DISTRIBUTION ....
C.... WITH ELECTRIC QUADRUPOLE MOMENT TENSORS               ....
C.... SEE BUCKINGHAM, ET AL, "MOLECULAR QUADRUPOLE MOMENTS" ....
C.... QUART. REV. (LONDON) -13-, 183-214 (1959)             ....
C.... EQUATION 15.                                          ....
C
      DO 210 I = 1,NMTTPT
        IF(.NOT.DOQUAD(I))GO TO 210
        XX = EFQAD(1,I)
        YY = EFQAD(2,I)
        ZZ = EFQAD(3,I)
        XY = EFQAD(4,I)
        XZ = EFQAD(5,I)
        YZ = EFQAD(6,I)
        DUM = XX + YY + ZZ
        QUA(1,I) = PT5 * (THREE * XX - DUM)
        QUA(2,I) = PT5 * (THREE * YY - DUM)
        QUA(3,I) = PT5 * (THREE * ZZ - DUM)
        QUA(4,I) = ONEPT5 * XY
        QUA(5,I) = ONEPT5 * XZ
        QUA(6,I) = ONEPT5 * YZ
210   CONTINUE
C
      IC1=0
      DO 400 IM = 1,NFRG
         DO 390 IP = 1,NMPTS(IM)
            IC1=IC1+1
            IF(.NOT.DOQUAD(IC1))GO TO 390
            X1 = EFC(1,IC1)
            Y1 = EFC(2,IC1)
            Z1 = EFC(3,IC1)
            XX1 = QUA(1,IC1)
            YY1 = QUA(2,IC1)
            ZZ1 = QUA(3,IC1)
            XY1 = QUA(4,IC1)
            XZ1 = QUA(5,IC1)
            YZ1 = QUA(6,IC1)
            IC2=0
            DO 380 JM = 1,NFRG
               IF (IM.EQ.JM)THEN
                 IC2=IC2+NPPTS(JM)
                 GO TO 380
               END IF
               DO 370 JP = 1,NPPTS(JM)
                  IC2=IC2+1
                  X2 = EFP(1,IC2)
                  Y2 = EFP(2,IC2)
                  Z2 = EFP(3,IC2)
C                 DX2 = (DIND(1,IC2))
C                 DY2 = (DIND(2,IC2))
C                 DZ2 = (DIND(3,IC2))
                  DX2 = PT5*(DIND(1,IC2)+DINDD(1,IC2))
                  DY2 = PT5*(DIND(2,IC2)+DINDD(2,IC2))
                  DZ2 = PT5*(DIND(3,IC2)+DINDD(3,IC2))
C REMEMBER FIELDS AT 1 DUE TO MULTIPOLES AT 2
                  X = X1 - X2
                  Y = Y1 - Y2
                  Z = Z1 - Z2
                  XX = X*X
                  YY = Y*Y
                  ZZ = Z*Z
                  XY = X*Y
                  XZ = X*Z
                  YZ = Y*Z
                  R2 = XX+YY+ZZ
                  R = SQRT(R2)
                  R3 = R2*R
                  R5 = R2*R3
                  R7 = R2*R5
                  R9 = R2*R7
C
C.... POTENTIAL, FIELD, AND FIELD GRADIENT DUE TO DIPOLE MOMENTS ON JM
C
      DOT = DX2*X + DY2*Y + DZ2*Z
C
      DUM9 = SEVEN/R9
      DUMA = FIFTEN/R7
      AFXX = (TWO*DX2*X)*DUMA
      AFYY = (TWO*DY2*Y)*DUMA
      AFZZ = (TWO*DZ2*Z)*DUMA
      AFXY = (DX2*Y+DY2*X)*DUMA
      AFXZ = (DX2*Z+DZ2*X)*DUMA
      AFYZ = (DY2*Z+DZ2*Y)*DUMA
        TERM = XX1*AFXX + YY1*AFYY + ZZ1*AFZZ
     1       + TWO*(XY1*AFXY + XZ1*AFXZ + YZ1*AFYZ)
        TERM = TERM/THREE
C
      TERMX = TWO*(XX1*DX2+XY1*DY2+XZ1*DZ2)/R5
      TERMY = TWO*(YY1*DY2+XY1*DX2+YZ1*DZ2)/R5
      TERMZ = TWO*(ZZ1*DZ2+YZ1*DY2+XZ1*DX2)/R5
C
      TORQX = TWO*(XX1*X+XY1*Y+XZ1*Z)/R5
      TORQY = TWO*(YY1*Y+XY1*X+YZ1*Z)/R5
      TORQZ = TWO*(ZZ1*Z+YZ1*Y+XZ1*X)/R5
C
      DDOT=XX1*XX+YY1*YY+ZZ1*ZZ+TWO*(XY1*XY+XZ1*XZ+YZ1*YZ)
      TERMC = FIVE*DOT*DUM9*DDOT
      TORQC = FIVE*DDOT/R7
C
      TERMDX = FIVE*(XX1*(TWO*X*DOT+XX*DX2)+YY1*YY*DX2+ZZ1*ZZ*DX2
     $     +TWO*(XY1*(Y*DOT+XY*DX2)+XZ1*(Z*DOT+XZ*DX2)+YZ1*YZ*DX2))/R7
      TERMDY = FIVE*(YY1*(TWO*Y*DOT+YY*DY2)+XX1*XX*DY2+ZZ1*ZZ*DY2
     $     +TWO*(XY1*(X*DOT+XY*DY2)+YZ1*(Z*DOT+YZ*DY2)+XZ1*XZ*DY2))/R7
      TERMDZ = FIVE*(ZZ1*(TWO*Z*DOT+ZZ*DZ2)+XX1*XX*DZ2+YY1*YY*DZ2
     $     +TWO*(YZ1*(Y*DOT+YZ*DZ2)+XZ1*(X*DOT+XZ*DZ2)+XY1*XY*DZ2))/R7
C
      FXX = DUMA*DOT*XX-SIX*DX2*X/R5
      FYY = DUMA*DOT*YY-SIX*DY2*Y/R5
      FZZ = DUMA*DOT*ZZ-SIX*DZ2*Z/R5
      FXY = DUMA*DOT*XY-THREE*(DX2*Y+DY2*X)/R5
      FXZ = DUMA*DOT*XZ-THREE*(DX2*Z+DZ2*X)/R5
      FYZ = DUMA*DOT*YZ-THREE*(DZ2*Y+DY2*Z)/R5
C
C
      IIC2=IC2+NMTTPT
            EF3(1,IIC2)=EF3(1,IIC2)-(TERM-TERMC)*X+TERMX-TERMDX
            EF3(2,IIC2)=EF3(2,IIC2)-(TERM-TERMC)*Y+TERMY-TERMDY
            EF3(3,IIC2)=EF3(3,IIC2)-(TERM-TERMC)*Z+TERMZ-TERMDZ
            EF3(1,IC1)=EF3(1,IC1)+(TERM-TERMC)*X-TERMX+TERMDX
            EF3(2,IC1)=EF3(2,IC1)+(TERM-TERMC)*Y-TERMY+TERMDY
            EF3(3,IC1)=EF3(3,IC1)+(TERM-TERMC)*Z-TERMZ+TERMDZ
C
      TDX = TORQY*DZ2-TORQZ*DY2+TORQC*(Z*DY2-Y*DZ2)
      TDY = TORQZ*DX2-TORQX*DZ2+TORQC*(X*DZ2-Z*DX2)
      TDZ = TORQX*DY2-TORQY*DX2+TORQC*(Y*DX2-X*DY2)
      ATORQ(1,JM)=ATORQ(1,JM)+TDX
      ATORQ(2,JM)=ATORQ(2,JM)+TDY
      ATORQ(3,JM)=ATORQ(3,JM)+TDZ
      ATORQ(1,IM)=ATORQ(1,IM)-TWO*(YZ1*FYY+ZZ1*FYZ+XZ1*FXY
     $                       -YY1*FYZ-YZ1*FZZ-XY1*FXZ)/THREE
      ATORQ(2,IM)=ATORQ(2,IM)-TWO*(XZ1*FZZ+XX1*FXZ+XY1*FYZ
     $                       -ZZ1*FXZ-XZ1*FXX-YZ1*FXY)/THREE
      ATORQ(3,IM)=ATORQ(3,IM)-TWO*(XY1*FXX+YY1*FXY+YZ1*FXZ
     $                       -XX1*FXY-XY1*FYY-XZ1*FYZ)/THREE
370            CONTINUE
380         CONTINUE
390      CONTINUE
400   CONTINUE
C
      RETURN
      END
C*MODULE EFELEC  *DECK EREPUL
      SUBROUTINE EREPUL(ELTOT,AREL,CREL,MXRPTS,NDFG2)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      DIMENSION AREL(MXRPTS,MXRPTS,NDFG2),CREL(MXRPTS,MXRPTS,NDFG2)
C
      CHARACTER*8 REPNAM
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG, MXDFG=5)
C
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /FRGTYP/ NDFRG,ISET(MXFRG),NAMIDX(MXDFG)
      COMMON /REPPAR/ CREP(3,MXFGPT),CLPR(4*MXFGPT),ZLPR(4*MXFGPT),
     *                NLPR(4*MXFGPT),KFR(MXFGPT),KLR(MXFGPT),
     *                REPNAM(MXFGPT)
C
      PARAMETER (ZERO = 0.00D+00)
C
      ELTOT = ZERO
C
      I=0
      DO 200 IFRG=1,NFRG
       IDX=ISET(IFRG)
       DO 100 IPR=1,NRPTS(IFRG)+1
        IF(IPR.LE.NRPTS(IFRG))I=I+1
      J=0
      DO 90 JFRG=1,NFRG
       IF(JFRG.LE.IFRG)THEN
        J=J+NRPTS(JFRG)
        GO TO 90
       END IF
       JDX=ISET(JFRG)
       IF (JDX.GE.IDX) THEN
          INDEX=JDX*(JDX-1)/2+IDX
       ELSE
          INDEX=IDX*(IDX-1)/2+JDX
       ENDIF
       DO 80 JPR=1,NRPTS(JFRG)+1
        IF(JPR.LE.NRPTS(JFRG))J=J+1
          RR = ZERO
        IF(IPR.LE.NRPTS(IFRG))THEN
         IF(JPR.LE.NRPTS(JFRG))THEN
          DO 50 K=1,3
            RR = RR + (CREP(K,I) - CREP(K,J))**2
   50     CONTINUE
         ELSE
          DO 51 K=1,3
            RR = RR + (CREP(K,I) - EFCENT(K,JFRG))**2
   51     CONTINUE
         END IF
        ELSE
         IF(JPR.LE.NRPTS(JFRG))THEN
          DO 52 K=1,3
            RR = RR + (EFCENT(K,IFRG) - CREP(K,J))**2
   52     CONTINUE
         ELSE
          DO 53 K=1,3
            RR = RR + (EFCENT(K,IFRG) - EFCENT(K,JFRG))**2
   53     CONTINUE
         END IF
        END IF
          RVAL = SQRT(RR)
          IF (JDX.GE.IDX) THEN
             ELTOT = ELTOT + CREL(JPR,IPR,INDEX)
     *                     * EXP(-AREL(JPR,IPR,INDEX)*RVAL)
C-BIEXP-     ELTOT = ELTOT + CREL(JPR,IPR,INDEX+MXDFG2)
C-BIEXP-     *             * EXP(-AREL(JPR,IPR,INDEX+MXDFG2)*RVAL)
          ELSE
             ELTOT = ELTOT + CREL(IPR,JPR,INDEX)
     *                     * EXP(-AREL(IPR,JPR,INDEX)*RVAL)
C-BIEXP-     ELTOT = ELTOT + CREL(IPR,JPR,INDEX+MXDFG2)
C-BIEXP-     *             * EXP(-AREL(IPR,JPR,INDEX+MXDFG2)*RVAL)
          ENDIF
   80   CONTINUE
   90   CONTINUE
  100   CONTINUE
  200 CONTINUE
C
      RETURN
      END
C*MODULE EFELEC  *DECK DREPUL
      SUBROUTINE DREPUL(EF3,AREL,CREL,MXRPTS,NDFG2)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      DIMENSION AREL(MXRPTS,MXRPTS,NDFG2),CREL(MXRPTS,MXRPTS,NDFG2),
     *          EF3(3,*)
C
      CHARACTER*8 REPNAM
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG, MXDFG=5)
C
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /FRGTYP/ NDFRG,ISET(MXFRG),NAMIDX(MXDFG)
      COMMON /REPPAR/ CREP(3,MXFGPT),CLPR(4*MXFGPT),ZLPR(4*MXFGPT),
     *                NLPR(4*MXFGPT),KFR(MXFGPT),KLR(MXFGPT),
     *                REPNAM(MXFGPT)
      COMMON /INTFRG/ EFF(3,MXFRG)
C
      PARAMETER (ZERO = 0.0D+00)
C
      DO 17 II=1,NFRG
      DO 17 K=1,3
      EFF(K,II)=ZERO
 17   CONTINUE
C
      I=0
      DO 200 IFRG=1,NFRG
       IDX=ISET(IFRG)
       DO 100 IPR=1,NRPTS(IFRG)+1
        IF(IPR.LE.NRPTS(IFRG))I=I+1
        II=I+NMTTPT+NPTTPT
      J=0
      DO 90 JFRG=1,NFRG
       IF(JFRG.LE.IFRG)THEN
        J=J+NRPTS(JFRG)
        GO TO 90
       END IF
       JDX=ISET(JFRG)
       DO 80 JPR=1,NRPTS(JFRG)+1
        IF(JPR.LE.NRPTS(JFRG))J=J+1
        JJ=J+NMTTPT+NPTTPT
        IF (JDX.GE.IDX) THEN
          INDEX=JDX*(JDX-1)/2+IDX
          CF = CREL(JPR,IPR,INDEX)
          AE = AREL(JPR,IPR,INDEX)
C-BIEXP-  DF = CREL(JPR,IPR,INDEX+MXDFG2)
C-BIEXP-  BE = AREL(JPR,IPR,INDEX+MXDFG2)
       ELSE
          INDEX=IDX*(IDX-1)/2+JDX
          CF = CREL(IPR,JPR,INDEX)
          AE = AREL(IPR,JPR,INDEX)
C-BIEXP-  DF = CREL(IPR,JPR,INDEX+MXDFG2)
C-BIEXP-  BE = AREL(IPR,JPR,INDEX+MXDFG2)
       ENDIF
          RR = ZERO
        IF(IPR.LE.NRPTS(IFRG))THEN
         IF(JPR.LE.NRPTS(JFRG))THEN
          DO 50 K=1,3
            RR = RR + (CREP(K,I) - CREP(K,J))**2
   50     CONTINUE
          RVAL = SQRT(RR)
          DO 60 K=1,3
          EF3(K,II) = EF3(K,II) - CF*EXP(-AE*RVAL)
     *              * AE*(CREP(K,I) - CREP(K,J))/RVAL
C-BIEXP-  EF3(K,II) = EF3(K,II) - DF*EXP(-BE*RVAL)
C-BIEXP-            * BE*(CREP(K,I) - CREP(K,J))/RVAL
          EF3(K,JJ) = EF3(K,JJ) + CF*EXP(-AE*RVAL)
     *              * AE*(CREP(K,I) - CREP(K,J))/RVAL
C-BIEXP-  EF3(K,JJ) = EF3(K,JJ) + DF*EXP(-BE*RVAL)
C-BIEXP-            * BE*(CREP(K,I) - CREP(K,J))/RVAL
   60     CONTINUE
         ELSE
          DO 51 K=1,3
            RR = RR + (CREP(K,I) - EFCENT(K,JFRG))**2
   51     CONTINUE
          RVAL = SQRT(RR)
          DO 61 K=1,3
          EF3(K,II) = EF3(K,II) - CF*EXP(-AE*RVAL)
     *              * AE*(CREP(K,I) - EFCENT(K,JFRG))/RVAL
C-BIEXP-  EF3(K,II) = EF3(K,II) - DF*EXP(-BE*RVAL)
C-BIEXP-            * BE*(CREP(K,I) - EFCENT(K,JFRG))/RVAL
          EFF(K,JFRG) = EFF(K,JFRG) + CF*EXP(-AE*RVAL)
     *                * AE*(CREP(K,I) - EFCENT(K,JFRG))/RVAL
C-BIEXP-  EFF(K,JFRG) = EFF(K,JFRG) + DF*EXP(-BE*RVAL)
C-BIEXP-              * BE*(CREP(K,I) - EFCENT(K,JFRG))/RVAL
   61     CONTINUE
         END IF
        ELSE
         IF(JPR.LE.NRPTS(JFRG))THEN
          DO 52 K=1,3
            RR = RR + (EFCENT(K,IFRG) - CREP(K,J))**2
   52     CONTINUE
          RVAL = SQRT(RR)
          DO 62 K=1,3
          EFF(K,IFRG) = EFF(K,IFRG) - CF*EXP(-AE*RVAL)
     *                * AE*(EFCENT(K,IFRG) - CREP(K,J))/RVAL
C-BIEXP-  EFF(K,IFRG) = EFF(K,IFRG) - DF*EXP(-BE*RVAL)
C-BIEXP-              * BE*(EFCENT(K,IFRG) - CREP(K,J))/RVAL
          EF3(K,JJ) = EF3(K,JJ) + CF*EXP(-AE*RVAL)
     *              * AE*(EFCENT(K,IFRG) - CREP(K,J))/RVAL
C-BIEXP-  EF3(K,JJ) = EF3(K,JJ) + DF*EXP(-BE*RVAL)
C-BIEXP-            * BE*(EFCENT(K,IFRG) - CREP(K,J))/RVAL
   62     CONTINUE
         ELSE
          DO 53 K=1,3
            RR = RR + (EFCENT(K,IFRG) - EFCENT(K,JFRG))**2
   53     CONTINUE
          RVAL = SQRT(RR)
          DO 63 K=1,3
          EFF(K,IFRG) = EFF(K,IFRG) - CF*EXP(-AE*RVAL)
     *                * AE*(EFCENT(K,IFRG) - EFCENT(K,JFRG))/RVAL
C-BIEXP-  EFF(K,IFRG) = EFF(K,IFRG) - DF*EXP(-BE*RVAL)
C-BIEXP-              * BE*(EFCENT(K,IFRG) - EFCENT(K,JFRG))/RVAL
          EFF(K,JFRG) = EFF(K,JFRG) + CF*EXP(-AE*RVAL)
     *                * AE*(EFCENT(K,IFRG) - EFCENT(K,JFRG))/RVAL
C-BIEXP-  EFF(K,JFRG) = EFF(K,JFRG) + DF*EXP(-BE*RVAL)
C-BIEXP-              * BE*(EFCENT(K,IFRG) - EFCENT(K,JFRG))/RVAL
   63     CONTINUE
         END IF
        END IF
   80   CONTINUE
   90   CONTINUE
  100   CONTINUE
  200 CONTINUE
C
      RETURN
      END
