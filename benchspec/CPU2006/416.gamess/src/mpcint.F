C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C  4 NOV 03 - GNM - DIAT: ADD NPQ VALUES FOR NA AND K
C  3 SEP 03 - MWS - H1ELEC,MHCORE: REMOVE TRANSLATION VECTOR
C 16 JUN 03 - HL  - INCREASE THE SIZE OF NHCO(4,200)
C 29 AUG 99 - CHC - MHCORE: RENAME COMMON BLOCK /FIELD/ TO /FIELDG/
C 13 JUN 96 - MWS - REMOVE FTNCHEK WARNINGS
C 12 NOV 94 - MWS - REMOVE FTNCHEK WARNINGS
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C  9 DEC 93 - MWS - MPCINT: CHANGE DAF RECORDS FOR S,W
C 21 MAR 92 - JHJ - NEW MODULE FOR MOPAC INTEGRAL EVALUATION
C
C         THE FOLLOWING STATEMENT APPEARED IN MOPAC 6.0 AND
C         IS REPRODUCED HERE TO COMPLY WITH SAID STATEMENT.
C
C         NOTICE OF PUBLIC DOMAIN NATURE OF THIS PROGRAM
C
C      'THIS COMPUTER PROGRAM IS A WORK OF THE UNITED STATES
C       GOVERNMENT AND AS SUCH IS NOT SUBJECT TO PROTECTION BY
C       COPYRIGHT (17 U.S.C. # 105.)  ANY PERSON WHO FRAUDULENTLY
C       PLACES A COPYRIGHT NOTICE OR DOES ANY OTHER ACT CONTRARY
C       TO THE PROVISIONS OF 17 U.S. CODE 506(C) SHALL BE SUBJECT
C       TO THE PENALTIES PROVIDED THEREIN.  THIS NOTICE SHALL NOT
C       BE ALTERED OR REMOVED FROM THIS SOFTWARE AND IS TO BE ON
C       ALL REPRODUCTIONS.'
C
C*MODULE MPCINT  *DECK AINTGS
      SUBROUTINE AINTGS (X,K)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /SETC/ A(7),B(7),SDUM(3),IDUM(2)
C
C    AINTGS FORMS THE "A" INTEGRALS FOR THE OVERLAP CALCULATION.
C
      C=EXP(-X)
      A(1)=C/X
      DO 10 I=1,K
         A(I+1)=(A(I)*I+C)/X
   10 CONTINUE
      RETURN
      END
C*MODULE MPCINT  *DECK BFN
      SUBROUTINE BFN(X,BF)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION BF(13)
      DIMENSION FACT(17)
      SAVE FACT
      DATA FACT/1.0D+00,2.0D+00,6.0D+00,24.0D+00,120.0D+00,
     *          720.0D+00,5040.0D+00,40320.0D+00,
     *          362880.0D+00,3628800.0D+00,39916800.0D+00,
     *          479001600.0D+00,6227020800.0D+00,8.71782912D+10,
     *          1.307674368D+12,2.092278989D+13,3.556874281D+14/
C
C     BINTGS FORMS THE "B" INTEGRALS FOR THE OVERLAP CALCULATION.
C
      K=12
      IO=0
      ABSX = ABS(X)
      IF (ABSX.GT.3.0D+00) GO TO 40
      IF (ABSX.LE.2.0D+00) GO TO 10
      LAST=15
      GO TO 60
   10 IF (ABSX.LE.1.0D+00) GO TO 20
      LAST=12
      GO TO 60
   20 IF (ABSX.LE.0.5D+00) GO TO 30
      LAST=7
      GO TO 60
   30 IF (ABSX.LE.1.0D-06) GO TO 90
      LAST=6
      GO TO 60
   40 EXPX=EXP(X)
      EXPMX=1.0D+00/EXPX
      BF(1)=(EXPX-EXPMX)/X
      DO 50 I=1,K
   50 BF(I+1)=(I*BF(I)+(-1.0D+00)**I*EXPX-EXPMX)/X
      RETURN
C
   60 CONTINUE
      DO 80 I=IO,K
         Y=0.0D+00
         DO 70 M=IO,LAST
            XF=1.0D+00
            IF(M.NE.0) XF=FACT(M)
            Y=Y+(-X)**M*(2*MOD(M+I+1,2))/(XF*(M+I+1))
   70    CONTINUE
         BF(I+1)=Y
   80 CONTINUE
      RETURN
C
   90 CONTINUE
      DO 100 I=IO,K
         BF(I+1)=(2*MOD(I+1,2))/(I+1.0D+00)
  100 CONTINUE
      RETURN
      END
C*MODULE MPCINT  *DECK BINGTS
      SUBROUTINE BINTGS (X,K)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION FACT(17)
C
      COMMON /SETC/ A(7),B(7),SDUM(3),IDUM(2)
C
      SAVE FACT
      DATA FACT/1.0D+00,2.0D+00,6.0D+00,24.00D+00,120.0D+00,
     *          720.0D+00,5040.0D+00,40320.0D+00,
     *          362880.0D+00,3628800.0D+00,39916800.0D+00,
     *          479001600.0D+00,6227020800.0D+00,8.71782912D+10,
     *          1.307674368D+12,2.092278989D+13,3.556874281D+14/
C
C     BINTGS FORMS THE "B" INTEGRALS FOR THE OVERLAP CALCULATION.
C
      IO=0
      ABSX = ABS(X)
      IF (ABSX.GT.3.0D+00) GO TO 40
      IF (ABSX.LE.2.0D+00) GO TO 10
      IF (K.LE.10) GO TO 40
      LAST=15
      GO TO 60
   10 IF (ABSX.LE.1.0D+00) GO TO 20
      IF (K.LE.7) GO TO 40
      LAST=12
      GO TO 60
   20 IF (ABSX.LE.0.5D+00) GO TO 30
      IF (K.LE.5) GO TO 40
      LAST=7
      GO TO 60
   30 IF (ABSX.LE.1.0D-06) GO TO 90
      LAST=6
      GO TO 60
C
   40 EXPX=EXP(X)
      EXPMX=1.0D+00/EXPX
      B(1)=(EXPX-EXPMX)/X
      DO 50 I=1,K
   50 B(I+1)=(I*B(I)+(-1.0D+00)**I*EXPX-EXPMX)/X
      RETURN
C
   60 DO 80 I=IO,K
         Y=0.0D+00
         DO 70 M=IO,LAST
            XF=1.0D+00
            IF(M.NE.0) XF=FACT(M)
   70    Y=Y+(-X)**M*(2*MOD(M+I+1,2))/(XF*(M+I+1))
   80 B(I+1)=Y
      RETURN
C
   90 CONTINUE
      DO I=IO,K
         B(I+1)=(2*MOD(I+1,2))/(I+1.0D+00)
      ENDDO
      RETURN
      END
C*MODULE MPCINT  *DECK COE
      SUBROUTINE COE(X1,Y1,Z1,X2,Y2,Z2,PQ1,PQ2,C,R)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER PQ1,PQ2,PQ
      DIMENSION C(75)
C
      XY=(X2-X1)**2+(Y2-Y1)**2
      R=SQRT(XY+(Z2-Z1)**2)
      XY=SQRT(XY)
      IF (XY.LT.1.0D-10) GO TO 10
      CA=(X2-X1)/XY
      CB=(Z2-Z1)/R
      SA=(Y2-Y1)/XY
      SB=XY/R
      GO TO 50
   10 IF (Z2-Z1) 20,30,40
   20 CA=-1.0D+00
      CB=-1.0D+00
      SA=0.0D+00
      SB=0.0D+00
      GO TO 50
   30 CA=0.0D+00
      CB=0.0D+00
      SA=0.0D+00
      SB=0.0D+00
      GO TO 50
   40 CA=1.0D+00
      CB=1.0D+00
      SA=0.0D+00
      SB=0.0D+00
   50 CONTINUE
      DO 60 I=1,75
   60 C(I)=0.0D+00
      IF (PQ1.GT.PQ2) GO TO 70
      PQ=PQ2
      GO TO 80
   70 PQ=PQ1
   80 CONTINUE
      C(37)=1.0D+00
      IF (PQ.LT.2) GO TO 90
      C(56)=CA*CB
      C(41)=CA*SB
      C(26)=-SA
      C(53)=-SB
      C(38)=CB
      C(23)=0.0D+00
      C(50)=SA*CB
      C(35)=SA*SB
      C(20)=CA
      IF (PQ.LT.3) GO TO 90
      C2A=2*CA*CA-1.0D+00
      C2B=2*CB*CB-1.0D+00
      S2A=2*SA*CA
      S2B=2*SB*CB
      C(75)=C2A*CB*CB+0.5D+00*C2A*SB*SB
      C(60)=0.5D+00*C2A*S2B
      C(45)=0.8660254037841D+00*C2A*SB*SB
      C(30)=-S2A*SB
      C(15)=-S2A*CB
      C(72)=-0.5D+00*CA*S2B
      C(57)=CA*C2B
      C(42)=0.8660254037841D+00*CA*S2B
      C(27)=-SA*CB
      C(12)=SA*SB
      C(69)=0.5773502691894D+00*SB*SB*1.5D+00
      C(54)=-0.8660254037841D+00*S2B
      C(39)=CB*CB-0.5D+00*SB*SB
      C(66)=-0.5D+00*SA*S2B
      C(51)=SA*C2B
      C(36)=0.8660254037841D+00*SA*S2B
      C(21)=CA*CB
      C(6)=-CA*SB
      C(63)=S2A*CB*CB+0.5D+00*S2A*SB*SB
      C(48)=0.5D+00*S2A*S2B
      C(33)=0.8660254037841D+00*S2A*SB*SB
      C(18)=C2A*SB
      C(3)=C2A*CB
   90 CONTINUE
      RETURN
      END
C*MODULE MPCINT  *DECK DIAT
      SUBROUTINE DIAT(NI,NJ,XI,XJ,DI)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C   DIAT CALCULATES THE DI-ATOMIC OVERLAP INTEGRALS BETWEEN ATOMS
C        CENTERED AT XI AND XJ.
C
C   ON INPUT NI  = ATOMIC NUMBER OF THE FIRST ATOM.
C            NJ  = ATOMIC NUMBER OF THE SECOND ATOM.
C            XI  = CARTESIAN COORDINATES OF THE FIRST ATOM.
C            XJ  = CARTESIAN COORDINATES OF THE SECOND ATOM.
C
C  ON OUTPUT DI  = DIATOMIC OVERLAP, IN A 9 * 9 MATRIX. LAYOUT OF
C                  ATOMIC ORBITALS IN DI IS
C                  1   2   3   4   5            6     7       8     9
C                  S   PX  PY  PZ  D(X**2-Y**2) D(XZ) D(Z**2) D(YZ)D(XY)
C
C   LIMITATIONS:  IN THIS FORMULATION, NI AND NJ MUST BE LESS THAN 107
C         EXPONENTS ARE ASSUMED TO BE PRESENT IN COMMON BLOCK EXPONT.
C
      COMMON /KEYWRD/KEYWRD
      CHARACTER*241 KEYWRD
      INTEGER A,PQ2,B,PQ1,AA,BB
      SAVE NPQ, IVAL
      LOGICAL ANALYT
      COMMON /EXPONT/ EMUS(107),EMUP(107),EMUD(107),EMUF(107),EMUG(107)
      DIMENSION DI(9,9),S(3,3,3),UL1(3),UL2(3),C(3,5,5),NPQ(107)
     1          ,XI(3),XJ(3), SLIN(27), IVAL(3,5)
     2, C1(3,5), C2(3,5), C3(3,5), C4(3,5), C5(3,5)
     3, S1(3,3), S2(3,3), S3(3,3)
      EQUIVALENCE(SLIN(1),S(1,1,1))
      EQUIVALENCE (C1(1,1),C(1,1,1)), (C2(1,1),C(1,1,2)),
     1            (C3(1,1),C(1,1,3)), (C4(1,1),C(1,1,4)),
     2            (C5(1,1),C(1,1,5)), (S1(1,1),S(1,1,1)),
     3            (S2(1,1),S(1,1,2)), (S3(1,1),S(1,1,3))
      DATA NPQ/1,0, 2,2,2,2,2,2,2,0, 3,3,3,3,3,3,3,0, 4,4,4,4,4,4,4,4,
     14,4,4,4,4,4,4,4,4,0, 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
     2,32*6,15*0,3,5*0/
      DATA IVAL/1,0,9,1,3,8,1,4,7,1,2,6,0,0,5/
C
      ANALYT=(INDEX(KEYWRD,'ANALYT').NE.0)
      X1=XI(1)
      X2=XJ(1)
      Y1=XI(2)
      Y2=XJ(2)
      Z1=XI(3)
      Z2=XJ(3)
      PQ1=NPQ(NI)
      PQ2=NPQ(NJ)
      DO 20 I=1,9
         DO 10 J=1,9
            DI(I,J)=0.0D+00
   10    CONTINUE
   20 CONTINUE
      CALL COE(X1,Y1,Z1,X2,Y2,Z2,PQ1,PQ2,C,R)
      IF(PQ1.EQ.0.OR.PQ2.EQ.0.OR.R.GE.10.0D+00) RETURN
      IF(R.LT.0.001)THEN
         RETURN
      ENDIF
      IA=MIN(PQ1,3)
      IB=MIN(PQ2,3)
      A=IA-1
      B=IB-1
      IF(ANALYT)THEN
         CALL SETUPG
         CALL GOVER(NI,NJ,XI,XJ,R,DI)
C#      WRITE(6,*)' OVERLAP FROM GOVER'
C#      WRITE(6,'(4F15.10)')SG
         RETURN
      ENDIF
      IF(NI.LT.18.AND.NJ.LT.18) THEN
         CALL DIAT2(NI,EMUS(NI),EMUP(NI),R,NJ,EMUS(NJ),EMUP(NJ),S)
      ELSE
         UL1(1)=EMUS(NI)
         UL2(1)=EMUS(NJ)
         UL1(2)=EMUP(NI)
         UL2(2)=EMUP(NJ)
         UL1(3)=MAX(EMUD(NI),0.3D+00)
         UL2(3)=MAX(EMUD(NJ),0.3D+00)
         DO 30 I=1,27
   30    SLIN(I)=0.0D+00
         NEWK=MIN(A,B)
         NK1=NEWK+1
         DO 40 I=1,IA
            ISS=I
            IB=B+1
            DO 40 J=1,IB
               JSS=J
               DO 40 K=1,NK1
                  IF(K.GT.I.OR.K.GT.J) GO TO 40
                  KSS=K
                  S(I,J,K)=SS(PQ1,PQ2,ISS,JSS,KSS,UL1(I),UL2(J),R)
   40    CONTINUE
      ENDIF
      DO 50 I=1,IA
         KMIN=4-I
         KMAX=2+I
         DO 50 J=1,IB
            IF(J.EQ.2)THEN
               AA=-1
               BB=1
            ELSE
               AA=1
               IF(J.EQ.3) THEN
                  BB=-1
               ELSE
                  BB=1
               ENDIF
            ENDIF
            LMIN=4-J
            LMAX=2+J
            DO 50 K=KMIN,KMAX
               DO 50 L=LMIN,LMAX
                  II=IVAL(I,K)
                  JJ=IVAL(J,L)
                  DI(II,JJ)=S1(I,J)*(C3(I,K)*C3(J,L))*AA+
     1(C4(I,K)*C4(J,L)+C2(I,K)*C2(J,L))*BB*S2(I,J)+(C5(I,K)*C5(J,L)
     2+C1(I,K)*C1(J,L))*S3(I,J)
   50 CONTINUE
C#      WRITE(6,*)' OVERLAP FROM DIAT2'
C#      DO 12 I=1,4
C#  12  WRITE(6,'(4F15.10)')(DI(J,I),J=1,4)
      RETURN
      END
C*MODULE MPCINT  *DECK DIAT2
      SUBROUTINE DIAT2(NA,ESA,EPA,R12,NB,ESB,EPB,S)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION S(3,3,3)
C***********************************************************************
C
C OVERLP CALCULATES OVERLAPS BETWEEN ATOMIC ORBITALS FOR PAIRS OF ATOMS
C        IT CAN HANDLE THE ORBITALS 1S, 2S, 3S, 2P, AND 3P.
C
C***********************************************************************
      COMMON /SETC/ A(7),B(7),SA,SB,FACTOR,ISP,IPS
      DIMENSION INMB(17),III(78)
      SAVE INMB, III
      DATA INMB/1,0,2,2,3,4,5,6,7,0,8,8,8,9,10,11,12/
C     NUMBERING CORRESPONDS TO BOND TYPE MATRIX GIVEN ABOVE
C      THE CODE IS
C
C     III=1      FIRST - FIRST  ROW ELEMENTS
C        =2      FIRST - SECOND
C        =3      FIRST - THIRD
C        =4      SECOND - SECOND
C        =5      SECOND - THIRD
C        =6      THIRD - THIRD
      DATA III/1,2,4,   2,4,4,   2,4,4,4,   2,4,4,4,4,
     1 2,4,4,4,4,4,   2,4,4,4,4,4,4,   3,5,5,5,5,5,5,6,
     2 3,5,5,5,5,5,5,6,6,   3,5,5,5,5,5,5,6,6,6,   3,5,5,5,5,5,5,6,6,6,6
     3, 3,5,5,5,5,5,5,6,6,6,6,6/
C
C      ASSIGNS BOND NUMBER
C
      JMAX=MAX0(INMB(NA),INMB(NB))
      JMIN=MIN0(INMB(NA),INMB(NB))
      NBOND=(JMAX*(JMAX-1))/2+JMIN
      II=III(NBOND)
      DO 10 I=1,3
         DO 10 J=1,3
            DO 10 K=1,3
   10 S(I,J,K)=0.0D+00
      RAB=R12/0.529167D+00
      GO TO (20,30,40,50,60,70), II
C
C     ------------------------------------------------------------------
C *** THE ORDERING OF THE ELEMENTS WITHIN S IS
C *** S(1,1,1)=(S(B)/S(A))
C *** S(1,2,1)=(P-SIGMA(B)/S(A))
C *** S(2,1,1)=(S(B)/P-SIGMA(A))
C *** S(2,2,1)=(P-SIGMA(B)/P-SIGMA(A))
C *** S(2,2,2)=(P-PI(B)/P-PI(A))
C     ------------------------------------------------------------------
C *** FIRST ROW - FIRST ROW OVERLAPS
C
   20 CALL SET (ESA,ESB,NA,NB,RAB,II)
      S(1,1,1)=.25D+00*SQRT((SA*SB*RAB*RAB)**3)*(A(3)*B(1)-B(3)*A(1))
      RETURN
C
C *** FIRST ROW - SECOND ROW OVERLAPS
C
   30 CALL SET (ESA,ESB,NA,NB,RAB,II)
      W=SQRT((SA**3)*(SB**5))*(RAB**4)*0.125D+00
      S(1,1,1) = SQRT(1.0D+00/3.0D+00)
      S(1,1,1)=W*S(1,1,1)*(A(4)*B(1)-B(4)*A(1)+A(3)*B(2)-B(3)*A(2))
      IF (NA.GT.1) CALL SET (EPA,ESB,NA,NB,RAB,II)
      IF (NB.GT.1) CALL SET (ESA,EPB,NA,NB,RAB,II)
      W=SQRT((SA**3)*(SB**5))*(RAB**4)*0.125D+00
      S(ISP,IPS,1)=W*(A(3)*B(1)-B(3)*A(1)+A(4)*B(2)-B(4)*A(2))
      RETURN
C
C *** FIRST ROW - THIRD ROW OVERLAPS
C
   40 CALL SET (ESA,ESB,NA,NB,RAB,II)
      W=SQRT((SA**3)*(SB**7)/7.5D+00)*(RAB**5)*0.0625D+00
      SROOT3 = SQRT(3.0D+00)
      S(1,1,1)=W*(A(5)*B(1)-B(5)*A(1)+
     12.0D+00*(A(4)*B(2)-B(4)*A(2)))/SROOT3
      IF (NA.GT.1) CALL SET (EPA,ESB,NA,NB,RAB,II)
      IF (NB.GT.1) CALL SET (ESA,EPB,NA,NB,RAB,II)
      W=SQRT((SA**3)*(SB**7)/7.5D+00)*(RAB**5)*0.0625D+00
      S(ISP,IPS,1)=W*(A(4)*(B(1)+B(3))-B(4)*(A(1)+A(3))+
     1B(2)*(A(3)+A(5))-A(2)*(B(3)+B(5)))
      RETURN
C
C *** SECOND ROW - SECOND ROW OVERLAPS
C
   50 CALL SET (ESA,ESB,NA,NB,RAB,II)
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D+00
      RT3=1.0D+00/SQRT(3.0D+00)
      S(1,1,1)=W*(A(5)*B(1)+B(5)*A(1)-2.0D+00*A(3)*B(3))/3.0D+00
      CALL SET (ESA,EPB,NA,NB,RAB,II)
      IF (NA.GT.NB) CALL SET (EPA,ESB,NA,NB,RAB,II)
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D+00
      D=A(4)*(B(1)-B(3))-A(2)*(B(3)-B(5))
      E=B(4)*(A(1)-A(3))-B(2)*(A(3)-A(5))
      S(ISP,IPS,1)=W*RT3*(D+E)
      CALL SET (EPA,ESB,NA,NB,RAB,II)
      IF (NA.GT.NB) CALL SET (ESA,EPB,NA,NB,RAB,II)
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D+00
      D=A(4)*(B(1)-B(3))-A(2)*(B(3)-B(5))
      E=B(4)*(A(1)-A(3))-B(2)*(A(3)-A(5))
      S(IPS,ISP,1)=-W*RT3*(E-D)
      CALL SET (EPA,EPB,NA,NB,RAB,II)
      W=SQRT((SA*SB)**5)*(RAB**5)*0.0625D+00
      S(2,2,1)=-W*(B(3)*(A(5)+A(1))-A(3)*(B(5)+B(1)))
      HD = 0.5D+00
      S(2,2,2)=HD*W*(A(5)*(B(1)-B(3))-B(5)*(A(1)-A(3))
     1-A(3)*B(1)+B(3)*A(1))
      RETURN
C
C *** SECOND ROW - THIRD ROW OVERLAPS
C
   60 CALL SET (ESA,ESB,NA,NB,RAB,II)
      W=SQRT((SA**5)*(SB**7)/7.5D+00)*(RAB**6)*0.03125D+00
      RT3 = 1.0D+00 / SQRT(3.0D+00)
      TD = 2.0D+00
      S(1,1,1)=W*(A(6)*B(1)+A(5)*B(2)-TD*(A(4)*B(3)+
     1A(3)*B(4))+A(2)*B(5)+A(
     21)*B(6))/3.0D+00
      CALL SET (ESA,EPB,NA,NB,RAB,II)
      IF (NA.GT.NB) CALL SET (EPA,ESB,NA,NB,RAB,II)
      W=SQRT((SA**5)*(SB**7)/7.5D+00)*(RAB**6)*0.03125D+00
      TD = 2.0D+00
      S(ISP,IPS,1)=W*RT3*(A(6)*B(2)+A(5)*B(1)-TD*(A(4)*B(4)+A(3)*B(3))
     1+A(2)*B(6)+A(1)*B(5))
      CALL SET (EPA,ESB,NA,NB,RAB,II)
      IF (NA.GT.NB) CALL SET (ESA,EPB,NA,NB,RAB,II)
      W=SQRT((SA**5)*SB**7/7.5D+00)*(RAB**6)*0.03125D+00
      TD = 2.0D+00
      S(IPS,ISP,1)=-W*RT3*(A(5)*(TD*B(3)-B(1))-B(5)*(TD*A(3)-A(1))-A(2
     1)*(B(6)-TD*B(4))+B(2)*(A(6)-TD*A(4)))
      CALL SET (EPA,EPB,NA,NB,RAB,II)
      W=SQRT((SA**5)*SB**7/7.5D+00)*(RAB**6)*0.03125D+00
      S(2,2,1)=-W*(B(4)*(A(1)+A(5))-A(4)*(B(1)+B(5))
     1+B(3)*(A(2)+A(6))-A(3)*(B(2)+B(6)))
      HD = 0.5D+00
      S(2,2,2)=HD*W*(A(6)*(B(1)-B(3))-B(6)*(A(1)-
     1A(3))+A(5)*(B(2)-B(4))-B(5
     2)*(A(2)-A(4))-A(4)*B(1)+B(4)*A(1)-A(3)*B(2)+B(3)*A(2))
      RETURN
C
C *** THIRD ROW - THIRD ROW OVERLAPS
C
   70 CALL SET (ESA,ESB,NA,NB,RAB,II)
      W=SQRT((SA*SB*RAB*RAB)**7)/480.0D+00
      RT3 = 1.0D+00 / SQRT(3.0D+00)
      S(1,1,1)=W*(A(7)*B(1)-3.0D+00*(A(5)*B(3)-A(3)*B(5))-A(1)*B(7))
     */3.0D+00
      CALL SET (ESA,EPB,NA,NB,RAB,II)
      IF (NA.GT.NB) CALL SET (EPA,ESB,NA,NB,RAB,II)
      W=SQRT((SA*SB*RAB*RAB)**7)/480.0D+00
      D=A(6)*(B(1)-B(3))-2.0D+00*A(4)*(B(3)-B(5))+A(2)*(B(5)-B(7))
      E=B(6)*(A(1)-A(3))-2.0D+00*B(4)*(A(3)-A(5))+B(2)*(A(5)-A(7))
      S(ISP,IPS,1)=W*RT3*(D-E)
      CALL SET (EPA,ESB,NA,NB,RAB,II)
      IF (NA.GT.NB) CALL SET (ESA,EPB,NA,NB,RAB,II)
      W=SQRT((SA*SB*RAB*RAB)**7)/480.0D+00
      D=A(6)*(B(1)-B(3))-2.0D+00*A(4)*(B(3)-B(5))+A(2)*(B(5)-B(7))
      E=B(6)*(A(1)-A(3))-2.0D+00*B(4)*(A(3)-A(5))+B(2)*(A(5)-A(7))
      S(IPS,ISP,1)=-W*RT3*(-D-E)
      CALL SET (EPA,EPB,NA,NB,RAB,II)
      W=SQRT((SA*SB*RAB*RAB)**7)/480.0D+00
      TD = 2.0D+00
      S(2,2,1)=-W*(A(3)*(B(7)+TD*B(3))-A(5)*(B(1)+
     1TD*B(5))-B(5)*A(1)+A(7)*B(3))
      HD = 0.5D+00
      S(2,2,2)=HD*W*(A(7)*(B(1)-B(3))+B(7)*(A(1)-
     1A(3))+A(5)*(B(5)-B(3)-B(1)
     2)+B(5)*(A(5)-A(3)-A(1))+2.0D+00*A(3)*B(3))
      RETURN
      END
C*MODULE MPCINT  *DECK GOVER
      SUBROUTINE GOVER(NI,NJ,XI,XJ,R,SG)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /NATYPE/ NZTYPE(107), MTYPE(30),LTYPE
      COMMON /TEMP/  C(60,6), Z(60,6)
      DIMENSION S(6,6), XI(3),XJ(3), SG(9,9)
      SAVE NGAUSS
      DATA NGAUSS/6/
C
C   GOVER CALCULATES THE OVERLAP INTEGRALS USING A GAUSSIAN EXPANSION
C         STO-6G BY R.F. STEWART, J. CHEM. PHYS., 52 431-438, 1970
C
C         ON INPUT   NI   =  ATOMIC NUMBER OF FIRST ATOM
C                    NJ   =  ATOMIC NUMBER OF SECOND ATOM
C                    R    =  INTERATOMIC DISTANCE IN ANGSTROMS
C         ON EXIT    S    =  9X9 ARRAY OF OVERLAPS, IN ORDER S,PX,PY,PZ
C
C    FIND START AND END OF GAUSSIAN
C
      IFA=NZTYPE(NI)*4-3
      IF(C(IFA+1,1).NE.0.0D+00)THEN
         ILA=IFA+3
      ELSE
         ILA=IFA
      ENDIF
      IFB=NZTYPE(NJ)*4-3
      IF(C(IFB+1,1).NE.0.0D+00)THEN
         ILB=IFB+3
      ELSE
         ILB=IFB
      ENDIF
C
C  CONVERT R INTO AU
C
      R=R/0.529167D+00
      R = R**2
      KA=0
      TOMB=1.0D+00
      DO 80 I=IFA,ILA
         KA=KA+1
         NAT=KA-1
         KB=0
         DO 80 J=IFB,ILB
            KB=KB+1
            NBT=KB-1
C
C         DECIDE IS IT AN S-S, S-P, P-S, OR P-P OVERLAP
C
            IF(NAT.GT.0.AND.NBT.GT.0) THEN
C    P-P
               IS=4
               TOMB=(XI(NAT)-XJ(NAT))*
     *              (XI(NBT)-XJ(NBT))*3.5711928576D+00
            ELSEIF(NAT.GT.0) THEN
C    P-S
               IS=3
               TOMB=(XI(NAT)-XJ(NAT))*1.88976D+00
            ELSEIF(NBT.GT.0) THEN
C    S-P
               IS=2
               TOMB=(XI(NBT)-XJ(NBT))*1.88976D+00
            ELSE
C    S-S
               IS=1
            ENDIF
            DO 60 K=1,NGAUSS
               DO 60 L=1,NGAUSS
                  S(K,L)=0.0D+00
                  AMB=Z(I,K)+Z(J,L)
                  APB=Z(I,K)*Z(J,L)
                  ADB=APB/AMB
C
C           CHECK OF OVERLAP IS NON-ZERO BEFORE STARTING
C
                  IF((ADB*R).LT.90.0D+00) THEN
                     ABN=1.0D+00
                     GO TO(50,10,20,30),IS
   10                ABN=2.0D+00*TOMB*Z(I,K)*SQRT(Z(J,L))/AMB
                     GO TO 50
   20                ABN=-2.0D+00*TOMB*Z(J,L)*SQRT(Z(I,K))/AMB
                     GO TO 50
   30                ABN=-ADB*TOMB
                     IF(NAT.EQ.NBT) ABN=ABN+0.5D+00
                     ABN=4.0D+00*ABN*SQRT(APB)/AMB
   50                S(K,L)=SQRT((2.0D+00*SQRT(APB)/AMB)**3)*
     *                      EXP(-ADB*R)*ABN
                  ENDIF
   60       CONTINUE
            SG(KA,KB)=0.0D+00
            DO 70 K=1,NGAUSS
               DO 70 L=1,NGAUSS
   70       SG(KA,KB)=SG(KA,KB)+S(K,L)*C(I,K)*C(J,L)
   80 CONTINUE
      RETURN
      END
C*MODULE MPCINT  *DECK H1ELEC
      SUBROUTINE H1ELEC(NI,NJ,XI,XJ,SMAT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION XI(3),XJ(3),SMAT(9,9),BI(9),BJ(9)
C
      COMMON /BETAS / BETAS(107),BETAP(107),BETAD(107)
      COMMON /MOLMEC/ HTYPE(4),NHCO(4,200),NNHCO,ITYPE
      COMMON /BETA3 / BETA3(153)
      COMMON /VSIPS / VS(107),VP(107),VD(107)
      COMMON /NATORB/ NATORB(107)
      COMMON /NUMCAL/ NUMCAL
C
      DATA ICALCN/0/
C
C  H1ELEC FORMS THE ONE-ELECTRON MATRIX BETWEEN TWO ATOMS.
C
C   ON INPUT    NI   = ATOMIC NO. OF FIRST ATOM.
C               NJ   = ATOMIC NO. OF SECOND ATOM.
C               XI   = COORDINATES OF FIRST ATOM.
C               XJ   = COORDINATES OF SECOND ATOM.
C
C   ON OUTPUT   SMAT = MATRIX OF ONE-ELECTRON INTERACTIONS.
C
      IF(NI.EQ.102.OR.NJ.EQ.102)THEN
         IF(SQRT((XI(1)-XJ(1))**2+
     1        (XI(2)-XJ(2))**2+
     2        (XI(3)-XJ(3))**2) .GT.1.8)THEN
            DO 10 I=1,9
               DO 10 J=1,9
   10       SMAT(I,J)=0.0D+00
            RETURN
         ENDIF
      ENDIF
C
      IF (ICALCN.NE.NUMCAL) ICALCN=NUMCAL
      CALL DIAT(NI,NJ,XI,XJ,SMAT)
C
      IF(ITYPE.NE.4) GO TO 80
C
C     START OF MNDO, AM1, OR PM3 OPTION
C
      II=MAX(NI,NJ)
      NBOND=(II*(II-1))/2+NI+NJ-II
      IF(NBOND.GT.153)GO TO 90
      BI(1)=BETA3(NBOND)*VS(NI)
      BI(2)=BETA3(NBOND)*VP(NI)
      BI(3)=BI(2)
      BI(4)=BI(2)
      BJ(1)=BETA3(NBOND)*VS(NJ)
      BJ(2)=BETA3(NBOND)*VP(NJ)
      BJ(3)=BJ(2)
      BJ(4)=BJ(2)
      GO TO 90
   80 CONTINUE
      BI(1)=BETAS(NI)*0.5D+00
      BI(2)=BETAP(NI)*0.5D+00
      BI(3)=BI(2)
      BI(4)=BI(2)
      BI(5)=BETAD(NI)*0.5D+00
      BI(6)=BI(5)
      BI(7)=BI(5)
      BI(8)=BI(5)
      BI(9)=BI(5)
      BJ(1)=BETAS(NJ)*0.5D+00
      BJ(2)=BETAP(NJ)*0.5D+00
      BJ(3)=BJ(2)
      BJ(4)=BJ(2)
      BJ(5)=BETAD(NJ)*0.5D+00
      BJ(6)=BJ(5)
      BJ(7)=BJ(5)
      BJ(8)=BJ(5)
      BJ(9)=BJ(5)
   90 CONTINUE
      NORBI=NATORB(NI)
      NORBJ=NATORB(NJ)
      IF(NORBI.EQ.9.OR.NORBJ.EQ.9) THEN
C
C    IN THE CALCULATION OF THE ONE-ELECTRON TERMS THE GEOMETRIC MEAN
C    OF THE TWO BETA VALUES IS BEING USED IF ONE OF THE ATOMS
C    CONTAINS D-ORBITALS.
         DO 100 J=1,NORBJ
            DO 100 I=1,NORBI
  100    SMAT(I,J)=-2.0D+00*SMAT(I,J)*SQRT(BI(I)*BJ(J))
      ELSE
         DO 110 J=1,NORBJ
            DO 110 I=1,NORBI
  110    SMAT(I,J)=SMAT(I,J)*(BI(I)+BJ(J))
      ENDIF
      RETURN
      END
C*MODULE MPCINT  *DECK MHCORE
      SUBROUTINE MHCORE(COORD,H,W,ENUCLR,L1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL FLDON
      DIMENSION COORD(3,*),H(*),W(L1)
      DIMENSION E1B(10),E2A(10),DI(9,9)
      PARAMETER (MXATM=500, MAXORB=4*MXATM)
C
      COMMON /FIELDG/ EFIELD(3)
      COMMON /MOLKST/ NUMAT,NAT(MXATM),NFIRST(MXATM),NMIDLE(MXATM),
     1                NLAST(MXATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /MOLORB/ USPD(MAXORB),DUMY(MAXORB)
      COMMON /MULTIP/ DD(107),QQ(107),AM(107),AD(107),AQ(107)
C
C   HCORE GENERATES THE ONE-ELECTRON MATRIX AND TWO ELECTRON INTEGRALS
C         FOR A GIVEN MOLECULE WHOSE GEOMETRY IS DEFINED IN CARTESIAN
C         COORDINATES.
C
C  ON INPUT  COORD   = COORDINATES OF THE MOLECULE.
C
C  ON OUTPUT  H      = ONE-ELECTRON MATRIX.
C             W      = TWO-ELECTRON INTEGRALS.
C             ENUCLR = NUCLEAR ENERGY
C
      SAVE IONE, CUTOFF
C
      IONE=1
      CUTOFF=1.0D+10
C
      IF ((EFIELD(1).NE.0.0D+00).OR.(EFIELD(2).NE.0.0D+00).OR.
     *    (EFIELD(3).NE.0.0D+00)) THEN
         FLDON = .TRUE.
         FLDCON = 51.4257D+00
      ELSE
         FLDON = .FALSE.
         FLDCON = 0.0D+00
      ENDIF
C
      DO 10 I=1,(NORBS*(NORBS+1))/2
   10 H(I)=0.0D+00
      ENUCLR=0.0D+00
      KR=1
      DO 110 I=1,NUMAT
         IA=NFIRST(I)
         IB=NLAST(I)
         IC=NMIDLE(I)
         NI=NAT(I)
C
C FIRST WE FILL THE DIAGONALS, AND OFF-DIAGONALS ON THE SAME ATOM
C
         DO 30 I1=IA,IB
            I2=I1*(I1-1)/2+IA-1
            DO 20 J1=IA,I1
               I2=I2+1
               H(I2)=0.0D+00
               IF (FLDON) THEN
                  IO1 = I1 - IA
                  JO1 = J1 - IA
                  IF ((JO1.EQ.0).AND.(IO1.EQ.1)) THEN
                     HTERME = -0.529177D+00*DD(NI)*EFIELD(1)*FLDCON
                     H(I2) = HTERME
                  ENDIF
                  IF ((JO1.EQ.0).AND.(IO1.EQ.2)) THEN
                     HTERME = -0.529177D+00*DD(NI)*EFIELD(2)*FLDCON
                     H(I2) = HTERME
                  ENDIF
                  IF ((JO1.EQ.0).AND.(IO1.EQ.3)) THEN
                     HTERME = -0.529177D+00*DD(NI)*EFIELD(3)*FLDCON
                     H(I2) = HTERME
                  ENDIF
               ENDIF
   20       CONTINUE
            H(I2) = USPD(I1)
            IF (FLDON) THEN
               FNUC = -(EFIELD(1)*COORD(1,I) + EFIELD(2)*COORD(2,I) +
     1              EFIELD(3)*COORD(3,I))*FLDCON
               H(I2) = H(I2) + FNUC
            ENDIF
   30    CONTINUE
C
C   FILL THE ATOM-OTHER ATOM ONE-ELECTRON MATRIX<PSI(LAMBDA)|PSI(SIGMA)>
C
         IM1=I-IONE
         DO 100 J=1,IM1
            HALF=1.0D+00
            IF(I.EQ.J)HALF=0.5D+00
            JA=NFIRST(J)
            JB=NLAST(J)
            JC=NMIDLE(J)
            NJ=NAT(J)
            CALL H1ELEC(NI,NJ,COORD(1,I),COORD(1,J),DI)
            I2=0
            DO 40 I1=IA,IB
               II=I1*(I1-1)/2+JA-1
               I2=I2+1
               J2=0
               JJ=MIN(I1,JB)
               DO 40 J1=JA,JJ
                  II=II+1
                  J2=J2+1
   40       H(II)=H(II)+DI(I2,J2)
C
C   CALCULATE THE TWO-ELECTRON INTEGRALS, W; THE ELECTRON NUCLEAR TERMS
C   E1B AND E2A; AND THE NUCLEAR-NUCLEAR TERM ENUC.
C
            CALL ROTATE(NI,NJ,COORD(1,I),COORD(1,J),
     1 W(KR), KR,E1B,E2A,ENUC,CUTOFF)
            ENUCLR = ENUCLR + ENUC
C
C   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM I.
C
            I2=0
            DO 60 I1=IA,IC
               II=I1*(I1-1)/2+IA-1
               DO 60 J1=IA,I1
                  II=II+1
                  I2=I2+1
   60       H(II)=H(II)+E1B(I2)*HALF
            DO  70 I1=IC+1,IB
               II=(I1*(I1+1))/2
   70       H(II)=H(II)+E1B(1)*HALF
C
C   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM J.
C
            I2=0
            DO 80 I1=JA,JC
               II=I1*(I1-1)/2+JA-1
               DO 80 J1=JA,I1
                  II=II+1
                  I2=I2+1
   80       H(II)=H(II)+E2A(I2)*HALF
            DO 90 I1=JC+1,JB
               II=(I1*(I1+1))/2
   90       H(II)=H(II)+E2A(1)*HALF
  100    CONTINUE
  110 CONTINUE
      RETURN
C      WRITE(6,'(//10X,''ONE-ELECTRON MATRIX FROM HCORE'')')
C      CALL VECPRT(H,NORBS)
C      J=MIN(400,KR)
C      WRITE(6,'(//10X,''TWO-ELECTRON MATRIX IN HCORE''/)')
C      WRITE(6,120)(W(I),I=1,J)
C     ENDIF
C 120 FORMAT(10F8.4)
C     RETURN
      END
C*MODULE MPCINT  *DECK MPCINT
      SUBROUTINE MPCINT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL OUT,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXATM=500, ONE = 1.00D+00, TOHART=ONE/27.211652D+00)
C
      COMMON /ENUCLR/ ENUCLR
      COMMON /FMCOM / X(1)
      COMMON /MPCGEO/ GEO(3,MXATM)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NNP,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /N2ELCT/ N2EL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DEBUG/8HDEBUG   /
#else
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      DATA DEBUG_STR/"DEBUG   "/
#endif
C
      IF(NPRINT.NE.-5 .AND. MASWRK) WRITE(IW,9000)
C
C     ----- ALLOCATE MEMORY FOR ONE- AND TWO-ELECTRON INTEGRALS
C           WRITE TO DAF -----
C
      OUT = (NPRINT.EQ.3 .OR. NPRINT.EQ.4 .OR. EXETYP.EQ. DEBUG)
     *      .AND. MASWRK
C
      L1 = NUM
      L2 = (L1*(L1+1))/2
      L4 = N2EL
      CALL VALFM(LOADFM)
C
      LH = 1 + LOADFM
      LS = LH + L2
      LW = LS + L2
      LAST = LW + L4
      NEED = LAST - LH
C
      IF(MASWRK) WRITE(IW,9010)NEED
C
      CALL GETFM(NEED)
C
      CALL VCLR(X(LS),1,L2)
      CALL VCLR(X(LH),1,L2)
      CALL VCLR(X(LW),1,L4)
C
C     ----- FILL COORD ARRAY ----
C
      CALL RDMOL
C
C     ----- OVERLAP MATRIX [X(LS)] IS UNITY -----
C
      II = 0
      DO 100 I = 1,L1
        II = II + I
        X(LS+II-1) = ONE
  100 CONTINUE
      CALL DAWRIT(IDAF,IODA,X(LS),L2,12,0)
C
C     ----- HCORE CALCULATES ONE- AND TWO-ELECTRON INTEGRALS -----
C
      CALL MHCORE(GEO,X(LH),X(LW),ENUCLR,L4)
C
C     ----- CALCULATE THE "ACTUAL" OVERLAP MATRIX FROM H -----
C
      CALL VCLR(X(LS),1,L2)
      CALL OVRLAP(X(LH),X(LS),L1)
C
C     ----- MOPAC ROUTINES USE EV UNIT, WE CONVERT TO HARTREES
C           THE TWO ELECTRON INTEGRALS CONVERTED IN MPCG -----
C
      CALL DSCAL(L2,TOHART,X(LH),1)
      ENUCLR = ENUCLR*TOHART
C
C     ----- WRITE S, H, AND W TO DAF -----
C
      CALL DAWRIT(IDAF,IODA,X(LH),L2,11,0)
      CALL DAWRIT(IDAF,IODA,X(LS),L2,52,0)
      CALL DAWRIT(IDAF,IODA,X(LW),L4,53,0)
C
C     ----- OPTIONAL PRINTOUT -----
C
      IF(OUT) THEN
        WRITE(IW,9040)
        CALL PRTRIL(X(LH),L1)
        WRITE(IW,9050)
        CALL PRTRIL(X(LS),L1)
        WRITE(IW,9060)
        WRITE(IW,9070)(X(LW+I-1), I=1,L4)
      ENDIF
      CALL RETFM(NEED)
      IF(MASWRK) WRITE(IW,9090)
      CALL TEXIT(1,1)
      RETURN
C
 9000 FORMAT(/10X,26("*")/10X,'1 AND 2 ELECTRON INTEGRALS'/
     *        10X,26(1H*))
 9010 FORMAT(/1X,'MOPAC ONE- AND TWO-ELECTRON INTEGRALS REQUIRE',
     *           I10,' WORDS.')
 9040 FORMAT(/10X,'H-CORE INTEGRALS')
 9050 FORMAT(/10X,'NON-ZDO OVERLAP INTEGRALS')
 9060 FORMAT(/10X,'TWO-ELECTRON INTEGRALS')
 9070 FORMAT(1X,10F8.4)
 9090 FORMAT(1X,'...... END OF ONE- AND TWO-ELECTRON INTEGRALS......')
      END
C*MODULE MPCINT  *DECK REPP
      SUBROUTINE REPP(NI,NJ,RIJ,RI,CORE)
C***********************************************************************
C
C..VECTOR VERSION WRITTEN BY ERNEST R. DAVIDSON, INDIANA UNIVERSITY
C
C
C  REPP CALCULATES THE TWO-ELECTRON REPULSION INTEGRALS AND THE
C       NUCLEAR ATTRACTION INTEGRALS.
C
C     ON INPUT RIJ     = INTERATOMIC DISTANCE
C              NI      = ATOM NUMBER OF FIRST ATOM
C              NJ      = ATOM NUMBER OF SECOND ATOM
C    (REF)     ADD     = ARRAY OF GAMMA, OR TWO-ELECTRON ONE-CENTER,
C                        INTEGRALS.
C    (REF)     TORE    = ARRAY OF NUCLEAR CHARGES OF THE ELEMENTS
C    (REF)     DD      = ARRAY OF DIPOLE CHARGE SEPARATIONS
C    (REF)     QQ      = ARRAY OF QUADRUPOLE CHARGE SEPARATIONS
C
C     THE COMMON BLOCKS ARE INITIALIZED IN BLOCK-DATA, AND NEVER CHANGED
C
C    ON OUTPUT RI      = ARRAY OF TWO-ELECTRON REPULSION INTEGRALS
C              CORE    = 4 X 2 ARRAY OF ELECTRON-CORE ATTRACTION
C                        INTEGRALS
C
C
C *** THIS ROUTINE COMPUTES THE TWO-CENTRE REPULSION INTEGRALS AND THE
C *** NUCLEAR ATTRACTION INTEGRALS.
C *** THE TWO-CENTRE REPULSION INTEGRALS (OVER LOCAL COORDINATES) ARE
C *** STORED AS FOLLOWS (WHERE P-SIGMA = O,  AND P-PI = P AND P* )
C     (SS/SS)=1,   (SO/SS)=2,   (OO/SS)=3,   (PP/SS)=4,   (SS/OS)=5,
C     (SO/SO)=6,   (SP/SP)=7,   (OO/SO)=8,   (PP/SO)=9,   (PO/SP)=10,
C     (SS/OO)=11,  (SS/PP)=12,  (SO/OO)=13,  (SO/PP)=14,  (SP/OP)=15,
C     (OO/OO)=16,  (PP/OO)=17,  (OO/PP)=18,  (PP/PP)=19,  (PO/PO)=20,
C     (PP/P*P*)=21,   (P*P/P*P)=22.
C *** THE STORAGE OF THE NUCLEAR ATTRACTION INTEGRALS  CORE(KL/IJ) IS
C     (SS/)=1,   (SO/)=2,   (OO/)=3,   (PP/)=4
C     WHERE IJ=1 IF THE ORBITALS CENTRED ON ATOM I,  =2 IF ON ATOM J.
C *** NI AND NJ ARE THE ATOMIC NUMBERS OF THE TWO ELEMENTS.
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL SI,SJ
      COMMON /MULTIP/ DD(107),QQ(107),ADD(107,3)
      COMMON /CORE/ TORE(107)
      COMMON /NATORB/ NATORB(107)
      DIMENSION RI(22),CORE(4,2)
      DIMENSION ARG(72),SQR(72)
      DATA  TD/2.0D+00/
      DATA PP/0.5D+00/
      DATA A0/0.529167D+00/ ,EV/27.21D+00/, EV1/13.605D+00/,
     *EV2/6.8025D+00/,EV3/3.40125D+00/, EV4/1.700625D+00/
C
C     ATOMIC UNITS ARE USED IN THE CALCULATION,
C     FINAL RESULTS ARE CONVERTED TO EV
C
      R=RIJ/A0
C
      SI = (NATORB(NI).GE.3)
      SJ = (NATORB(NJ).GE.3)
C
      IF ((.NOT.SI) .AND. (.NOT.SJ)) THEN
C
C     HYDROGEN - HYDROGEN  (SS/SS)
C
         AEE = PP/ADD(NI,1) + PP/ADD(NJ,1)
         AEE = AEE * AEE
         RI(1) = EV/SQRT(R*R+AEE)
         CORE(1,1 )= TORE(NJ)*RI(1)
         CORE(1,2) = TORE(NI)*RI(1)
C
      ELSE IF (SI .AND. (.NOT.SJ)) THEN
C
C     HEAVY ATOM - HYDROGEN
C
         AEE = PP/ADD(NI,1) + PP/ADD(NJ,1)
         AEE = AEE * AEE
         DA=DD(NI)
         QA=QQ(NI) * TD
         ADE = PP/ADD(NI,2) + PP/ADD(NJ,1)
         ADE = ADE * ADE
         AQE = PP/ADD(NI,3) + PP/ADD(NJ,1)
         AQE = AQE * AQE
         RSQ = R*R
         ARG(1) = RSQ + AEE
         XXX = R+DA
         ARG(2) = XXX*XXX + ADE
         XXX = R-DA
         ARG(3) = XXX*XXX + ADE
         XXX = R+QA
         ARG(4) = XXX*XXX + AQE
         XXX = R-QA
         ARG(5) = XXX*XXX + AQE
         ARG(6) = RSQ + AQE
         ARG(7) = ARG(6) + QA*QA
C$DOIT ASIS
         DO 10 I = 1,7
            SQR(I) = SQRT(ARG(I))
   10    CONTINUE
         EE = EV/SQR(1)
         RI(1) = EE
         RI(2) = EV1/SQR(2) - EV1/SQR(3)
         RI(3) = EE + EV2/SQR(4) + EV2/SQR(5) - EV1/SQR(6)
         RI(4) = EE + EV1/SQR(7) - EV1/SQR(6)
         CORE(1,1) = TORE(NJ)*RI(1)
         CORE(1,2) = TORE(NI)*RI(1)
         CORE(2,1) = TORE(NJ)*RI(2)
         CORE(3,1) = TORE(NJ)*RI(3)
         CORE(4,1) = TORE(NJ)*RI(4)
C
      ELSE IF ((.NOT.SI).AND.SJ) THEN
C
C     HYDROGEN - HEAVY ATOM
C
         AEE = PP/ADD(NI,1) + PP/ADD(NJ,1)
         AEE = AEE * AEE
         DB=DD(NJ)
         QB=QQ(NJ) * TD
         AED = PP/ADD(NI,1) + PP/ADD(NJ,2)
         AED = AED * AED
         AEQ = PP/ADD(NI,1) + PP/ADD(NJ,3)
         AEQ = AEQ * AEQ
         RSQ = R*R
         ARG(1) = RSQ + AEE
         XXX = R-DB
         ARG(2) = XXX*XXX + AED
         XXX = R+DB
         ARG(3) = XXX*XXX + AED
         XXX = R-QB
         ARG(4) = XXX*XXX + AEQ
         XXX = R+QB
         ARG(5) = XXX*XXX + AEQ
         ARG(6) = RSQ + AEQ
         ARG(7) = ARG(6) + QB*QB
C$DOIT ASIS
         DO 20 I = 1,7
            SQR(I) = SQRT(ARG(I))
   20    CONTINUE
         EE = EV/SQR(1)
         RI(1) = EE
         RI(5) = EV1/SQR(2)  - EV1/SQR(3)
         RI(11) = EE + EV2/SQR(4) + EV2/SQR(5) - EV1/SQR(6)
         RI(12) = EE + EV1/SQR(7) - EV1/SQR(6)
         CORE(1,1) = TORE(NJ)*RI(1)
         CORE(1,2) = TORE(NI)*RI(1)
         CORE(2,2) = TORE(NI)*RI(5)
         CORE(3,2) = TORE(NI)*RI(11)
         CORE(4,2) = TORE(NI)*RI(12)
C
      ELSE
C
C     HEAVY ATOM - HEAVY ATOM
C
C     DEFINE CHARGE SEPARATIONS.
         DA=DD(NI)
         DB=DD(NJ)
         QA=QQ(NI) * TD
         QB=QQ(NJ) * TD
C
         AEE = PP/ADD(NI,1) + PP/ADD(NJ,1)
         AEE = AEE * AEE
C
         ADE = PP/ADD(NI,2) + PP/ADD(NJ,1)
         ADE = ADE * ADE
         AQE = PP/ADD(NI,3) + PP/ADD(NJ,1)
         AQE = AQE * AQE
         AED = PP/ADD(NI,1) + PP/ADD(NJ,2)
         AED = AED * AED
         AEQ = PP/ADD(NI,1) + PP/ADD(NJ,3)
         AEQ = AEQ * AEQ
         AXX = PP/ADD(NI,2) + PP/ADD(NJ,2)
         AXX = AXX * AXX
         ADQ = PP/ADD(NI,2) + PP/ADD(NJ,3)
         ADQ = ADQ * ADQ
         AQD = PP/ADD(NI,3) + PP/ADD(NJ,2)
         AQD = AQD * AQD
         AQQ = PP/ADD(NI,3) + PP/ADD(NJ,3)
         AQQ = AQQ * AQQ
         RSQ = R * R
         ARG(1) = RSQ + AEE
         XXX = R + DA
         ARG(2) = XXX * XXX + ADE
         XXX = R - DA
         ARG(3) = XXX*XXX + ADE
         XXX = R - QA
         ARG(4) = XXX*XXX + AQE
         XXX = R + QA
         ARG(5) = XXX*XXX + AQE
         ARG(6) = RSQ + AQE
         ARG(7) = ARG(6) + QA*QA
         XXX = R-DB
         ARG(8) = XXX*XXX + AED
         XXX = R+DB
         ARG(9) = XXX*XXX + AED
         XXX = R - QB
         ARG(10) = XXX*XXX + AEQ
         XXX = R + QB
         ARG(11) = XXX*XXX + AEQ
         ARG(12) = RSQ + AEQ
         ARG(13) = ARG(12) + QB*QB
         XXX = DA-DB
         ARG(14) = RSQ + AXX + XXX*XXX
         XXX = DA+DB
         ARG(15) = RSQ + AXX + XXX*XXX
         XXX = R + DA - DB
         ARG(16) = XXX*XXX + AXX
         XXX = R - DA + DB
         ARG(17) = XXX*XXX + AXX
         XXX = R - DA - DB
         ARG(18) = XXX*XXX + AXX
         XXX = R + DA + DB
         ARG(19) = XXX*XXX + AXX
         XXX = R + DA
         ARG(20) = XXX*XXX + ADQ
         ARG(21) = ARG(20) + QB*QB
         XXX = R - DA
         ARG(22) = XXX*XXX + ADQ
         ARG(23) = ARG(22) + QB*QB
         XXX = R - DB
         ARG(24) = XXX*XXX + AQD
         ARG(25) = ARG(24) + QA*QA
         XXX = R + DB
         ARG(26) = XXX*XXX + AQD
         ARG(27) = ARG(26) + QA*QA
         XXX = R + DA - QB
         ARG(28) = XXX*XXX + ADQ
         XXX = R - DA - QB
         ARG(29) = XXX*XXX + ADQ
         XXX = R + DA + QB
         ARG(30) = XXX*XXX + ADQ
         XXX = R - DA + QB
         ARG(31) = XXX*XXX + ADQ
         XXX = R + QA - DB
         ARG(32) = XXX*XXX + AQD
         XXX = R + QA + DB
         ARG(33) = XXX*XXX + AQD
         XXX = R - QA - DB
         ARG(34) = XXX*XXX + AQD
         XXX = R - QA + DB
         ARG(35) = XXX*XXX + AQD
         ARG(36) = RSQ + AQQ
         XXX = QA - QB
         ARG(37) = ARG(36) + XXX*XXX
         XXX = QA + QB
         ARG(38) = ARG(36) + XXX*XXX
         ARG(39) = ARG(36) + QA*QA
         ARG(40) = ARG(36) + QB*QB
         ARG(41) = ARG(39) + QB*QB
         XXX = R - QB
         ARG(42) = XXX*XXX + AQQ
         ARG(43) = ARG(42) + QA*QA
         XXX = R + QB
         ARG(44) = XXX*XXX + AQQ
         ARG(45) = ARG(44) + QA*QA
         XXX = R + QA
         ARG(46) = XXX*XXX + AQQ
         ARG(47) = ARG(46) + QB*QB
         XXX = R - QA
         ARG(48) = XXX*XXX + AQQ
         ARG(49) = ARG(48) + QB*QB
         XXX = R + QA - QB
         ARG(50) = XXX*XXX + AQQ
         XXX = R + QA + QB
         ARG(51) = XXX*XXX + AQQ
         XXX = R - QA - QB
         ARG(52) = XXX*XXX + AQQ
         XXX = R - QA + QB
         ARG(53) = XXX*XXX + AQQ
         QA=QQ(NI)
         QB=QQ(NJ)
         XXX = DA - QB
         XXX = XXX*XXX
         YYY = R - QB
         YYY = YYY*YYY
         ZZZ = DA + QB
         ZZZ = ZZZ*ZZZ
         WWW = R + QB
         WWW = WWW*WWW
         ARG(54) = XXX + YYY + ADQ
         ARG(55) = XXX + WWW + ADQ
         ARG(56) = ZZZ + YYY + ADQ
         ARG(57) = ZZZ + WWW + ADQ
         XXX = QA - DB
         XXX = XXX*XXX
         YYY = QA + DB
         YYY = YYY*YYY
         ZZZ = R + QA
         ZZZ = ZZZ*ZZZ
         WWW = R - QA
         WWW = WWW*WWW
         ARG(58) = ZZZ + XXX + AQD
         ARG(59) = WWW + XXX + AQD
         ARG(60) = ZZZ + YYY + AQD
         ARG(61) = WWW + YYY + AQD
         XXX = QA - QB
         XXX = XXX*XXX
         ARG(62) = ARG(36) + TD*XXX
         YYY = QA + QB
         YYY = YYY*YYY
         ARG(63) = ARG(36) + TD*YYY
         ARG(64) = ARG(36) + TD*(QA*QA+QB*QB)
         ZZZ = R + QA - QB
         ZZZ = ZZZ*ZZZ
         ARG(65) = ZZZ + XXX + AQQ
         ARG(66) = ZZZ + YYY + AQQ
         ZZZ = R + QA + QB
         ZZZ = ZZZ*ZZZ
         ARG(67) = ZZZ + XXX + AQQ
         ARG(68) = ZZZ + YYY + AQQ
         ZZZ = R - QA - QB
         ZZZ = ZZZ*ZZZ
         ARG(69) = ZZZ + XXX + AQQ
         ARG(70) = ZZZ + YYY + AQQ
         ZZZ = R - QA + QB
         ZZZ = ZZZ*ZZZ
         ARG(71) = ZZZ + XXX + AQQ
         ARG(72) = ZZZ + YYY + AQQ
         DO 30 I = 1,72
            SQR(I) = SQRT(ARG(I))
   30    CONTINUE
         EE = EV/SQR(1)
         DZE = -EV1/SQR(2) + EV1/SQR(3)
         QZZE = EV2/SQR(4) + EV2/SQR(5) - EV1/SQR(6)
         QXXE = EV1/SQR(7) - EV1/SQR(6)
         EDZ = - EV1/SQR(8) + EV1/SQR(9)
         EQZZ  = EV2/SQR(10) + EV2/SQR(11) - EV1/SQR(12)
         EQXX  = EV1/SQR(13) - EV1/SQR(12)
         DXDX  = EV1/SQR(14) - EV1/SQR(15)
         DZDZ  = EV2/SQR(16) + EV2/SQR(17) - EV2/SQR(18) - EV2/SQR(19)
         DZQXX =  EV2/SQR(20) - EV2/SQR(21) - EV2/SQR(22) + EV2/SQR(23)
         QXXDZ =  EV2/SQR(24) - EV2/SQR(25) - EV2/SQR(26) + EV2/SQR(27)
         DZQZZ = -EV3/SQR(28) + EV3/SQR(29) - EV3/SQR(30) + EV3/SQR(31)
     1       - EV2/SQR(22) + EV2/SQR(20)
         QZZDZ = -EV3/SQR(32) + EV3/SQR(33) - EV3/SQR(34) + EV3/SQR(35)
     1       + EV2/SQR(24) - EV2/SQR(26)
         QXXQXX = EV3/SQR(37) + EV3/SQR(38) - EV2/SQR(39) - EV2/SQR(40)
     1       + EV2/SQR(36)
         QXXQYY = EV2/SQR(41) - EV2/SQR(39) - EV2/SQR(40) + EV2/SQR(36)
         QXXQZZ = EV3/SQR(43) + EV3/SQR(45) - EV3/SQR(42) - EV3/SQR(44)
     1       - EV2/SQR(39) + EV2/SQR(36)
         QZZQXX = EV3/SQR(47) + EV3/SQR(49) - EV3/SQR(46) - EV3/SQR(48)
     1       - EV2/SQR(40) + EV2/SQR(36)
         QZZQZZ = EV4/SQR(50) + EV4/SQR(51) + EV4/SQR(52) + EV4/SQR(53)
     1       - EV3/SQR(48) - EV3/SQR(46) - EV3/SQR(42) - EV3/SQR(44)
     2       + EV2/SQR(36)
         DXQXZ = -EV2/SQR(54) + EV2/SQR(55) + EV2/SQR(56) - EV2/SQR(57)
         QXZDX = -EV2/SQR(58) + EV2/SQR(59) + EV2/SQR(60) - EV2/SQR(61)
         QXZQXZ = EV3/SQR(65) - EV3/SQR(67) - EV3/SQR(69) + EV3/SQR(71)
     1       - EV3/SQR(66) + EV3/SQR(68) + EV3/SQR(70) - EV3/SQR(72)
         RI(1) = EE
         RI(2) = -DZE
         RI(3) = EE + QZZE
         RI(4) = EE + QXXE
         RI(5) = -EDZ
         RI(6) = DZDZ
         RI(7) = DXDX
         RI(8) = -EDZ -QZZDZ
         RI(9) = -EDZ -QXXDZ
         RI(10) = -QXZDX
         RI(11) =  EE + EQZZ
         RI(12) =  EE + EQXX
         RI(13) = -DZE -DZQZZ
         RI(14) = -DZE -DZQXX
         RI(15) = -DXQXZ
         RI(16) = EE +EQZZ +QZZE +QZZQZZ
         RI(17) = EE +EQZZ +QXXE +QXXQZZ
         RI(18) = EE +EQXX +QZZE +QZZQXX
         RI(19) = EE +EQXX +QXXE +QXXQXX
         RI(20) = QXZQXZ
         RI(21) = EE +EQXX +QXXE +QXXQYY
         RI(22) = PP * (QXXQXX -QXXQYY)
C
C     CALCULATE CORE-ELECTRON ATTRACTIONS.
C
         CORE(1,1) = TORE(NJ)*RI(1)
         CORE(2,1) = TORE(NJ)*RI(2)
         CORE(3,1) = TORE(NJ)*RI(3)
         CORE(4,1) = TORE(NJ)*RI(4)
         CORE(1,2) = TORE(NI)*RI(1)
         CORE(2,2) = TORE(NI)*RI(5)
         CORE(3,2) = TORE(NI)*RI(11)
         CORE(4,2) = TORE(NI)*RI(12)
C
      END IF
C
      RETURN
C
      END
C*MODULE MPCINT  *DECK ROTATE
      SUBROUTINE ROTATE (NI,NJ,XI,XJ,W,KR,E1B,E2A,ENUC,CUTOFF)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***********************************************************************
C
C..IMPROVED SCALAR VERSION
C..WRITTEN BY ERNEST R. DAVIDSON, INDIANA UNIVERSITY.
C
C
C   ROTATE CALCULATES THE TWO-PARTICLE INTERACTIONS.
C
C   ON INPUT  NI     = ATOMIC NUMBER OF FIRST ATOM.
C             NJ     = ATOMIC NUMBER OF SECOND ATOM.
C             XI     = COORDINATE OF FIRST ATOM.
C             XJ     = COORDINATE OF SECOND ATOM.
C
C ON OUTPUT W      = ARRAY OF TWO-ELECTRON REPULSION INTEGRALS.
C           E1B,E2A= ARRAY OF ELECTRON-NUCLEAR ATTRACTION INTEGRALS,
C                    E1B = ELECTRON ON ATOM NI ATTRACTING NUCLEUS OF NJ.
C           ENUC   = NUCLEAR-NUCLEAR REPULSION TERM.
C
C
C *** THIS ROUTINE COMPUTES THE REPULSION AND NUCLEAR ATTRACTION
C     INTEGRALS OVER MOLECULAR-FRAME COORDINATES.  THE INTEGRALS OVER
C     LOCAL FRAME COORDINATES ARE EVALUATED BY SUBROUTINE REPP AND
C     STORED AS FOLLOWS (WHERE P-SIGMA = O,   AND P-PI = P AND P* )
C     IN RI
C     (SS/SS)=1,   (SO/SS)=2,   (OO/SS)=3,   (PP/SS)=4,   (SS/OS)=5,
C     (SO/SO)=6,   (SP/SP)=7,   (OO/SO)=8,   (PP/SO)=9,   (PO/SP)=10,
C     (SS/OO)=11,  (SS/PP)=12,  (SO/OO)=13,  (SO/PP)=14,  (SP/OP)=15,
C     (OO/OO)=16,  (PP/OO)=17,  (OO/PP)=18,  (PP/PP)=19,  (PO/PO)=20,
C     (PP/P*P*)=21,   (P*P/P*P)=22.
C
C***********************************************************************
      SAVE ANALYT
      COMMON /MOLMEC/ HTYPE(4),NHCO(4,200),NNHCO,ITYPE
      CHARACTER*241 KEYWRD
      LOGICAL SI,SJ, ANALYT
      COMMON /NATORB/ NATORB(107)
      COMMON /TWOEL3/ F03(107)
      COMMON /ALPHA3/ ALP3(153)
      COMMON /ALPHA / ALP(107)
      COMMON /CORE  / TORE(107)
      COMMON /IDEAS / FN1(107,10),FN2(107,10),FN3(107,10)
      COMMON /ALPTM / ALPTM(30), EMUDTM(30)
      COMMON /ROTDUM/ CSS1,CSP1,CPPS1,CPPP1,CSS2,CSP2,CPPS2,CPPP2
      COMMON /ROTDU2/ X(3),Y(3),Z(3)
      COMMON /KEYWRD/ KEYWRD
      DIMENSION XI(3),XJ(3),W(100),E1B(10),E2A(10)
      DIMENSION RI(22),CCORE(4,2), BORON1(3,4), BORON2(3,4), BORON3(3,4)
      EQUIVALENCE (CCORE(1,1),CSS1)
      DATA BORON1/  0.182613D+00,  0.118587D+00, -0.073280D+00,
     1              0.412253D+00, -0.149917D+00,  0.000000D+00,
     2              0.261751D+00,  0.050275D+00,  0.000000D+00,
     3              0.359244D+00,  0.074729D+00,  0.000000D+00/
      DATA BORON2/  6.0D+00,  6.0D+00,  5.0D+00,
     1             10.0D+00,  6.0D+00,  0.0D+00,
     2              8.0D+00,  5.0D+00,  0.0D+00,
     3              9.0D+00,  9.0D+00,  0.0D+00/
      DATA BORON3/  0.727592D+00,  1.466639D+00,  1.570975D+00,
     1              0.832586D+00,  1.186220D+00,  0.000000D+00,
     2              1.063995D+00,  1.936492D+00,  0.000000D+00,
     3              0.819351D+00,  1.574414D+00,  0.000000D+00/
C
      ANALYT=(INDEX(KEYWRD,'ANALYT') .NE. 0)
C
      X(1)=XI(1)-XJ(1)
      X(2)=XI(2)-XJ(2)
      X(3)=XI(3)-XJ(3)
      RIJ=X(1)*X(1)+X(2)*X(2)+X(3)*X(3)
      IF (RIJ.LT.0.00002D+00) THEN
C
C     SMALL RIJ CASE
C
         DO 10 I=1,10
            E1B(I)=0.0D+00
            E2A(I)=0.0D+00
   10    CONTINUE
         W(KR)=0.0D+00
         ENUC=0.0D+00
C
      ELSE IF (ITYPE.EQ.4) THEN
C
C     MINDO CASE
C
         SUM=14.399D+00/SQRT(RIJ+(7.1995D+00/F03(NI)+
     *7.1995D+00/F03(NJ))**2)
         W(1)=SUM
         KR=KR+1
         DO 20 L=1,10
            E1B(L)=0.0D+00
            E2A(L)=0.0D+00
   20    CONTINUE
         E1B(1) = -SUM*TORE(NJ)
         E1B(3) = E1B(1)
         E1B(6) = E1B(1)
         E1B(10)= E1B(1)
         E2A(1) = -SUM*TORE(NI)
         E2A(3) = E2A(1)
         E2A(6) = E2A(1)
         E2A(10)= E2A(1)
         II = MAX(NI,NJ)
         NBOND = (II*(II-1))/2+NI+NJ-II
         RIJ = SQRT(RIJ)
         IF(NBOND.LT.154) THEN
            IF(NBOND.EQ.22 .OR. NBOND .EQ. 29) THEN
C              NBOND = 22 IS C-H CASE
C              NBOND = 29 IS N-H CASE
               SCALE=ALP3(NBOND)*EXP(-RIJ)
            ELSE
C              NBOND < 154  IS NI < 18 AND NJ < 18 CASE
               SCALE=EXP(-ALP3(NBOND)*RIJ)
            ENDIF
         ELSE
C              NBOND > 154 INVOLVES NI OR NJ > 18
            SCALE = 0
            IF(NATORB(NI).EQ.0) SCALE=      EXP(-ALP(NI)*RIJ)
            IF(NATORB(NJ).EQ.0) SCALE=SCALE+EXP(-ALP(NI)*RIJ)
         ENDIF
         IF (ABS(TORE(NI)).GT.20.0D+00 .AND. ABS(TORE(NJ)).GT.
     *20.0D+00) THEN
            ENUC=0.0D+00
         ELSE IF (RIJ.LT.1.0D+00 .AND. NATORB(NI)*NATORB(NJ).EQ.0) THEN
            ENUC=0.0D+00
         ELSE
            ENUC = TORE(NI)*TORE(NJ)*SUM
     1       + ABS(TORE(NI)*TORE(NJ)*(14.399D+00/RIJ-SUM)*SCALE)
         ENDIF
C
C     MNDO AND AM1 CASES
C
C *** THE REPULSION INTEGRALS OVER MOLECULAR FRAME (W) ARE STORED IN THE
C     ORDER IN WHICH THEY WILL LATER BE USED.  IE.  (I,J/K,L) WHERE
C     J.LE.I  AND  L.LE.K     AND L VARIES MOST RAPIDLY AND I LEAST
C     RAPIDLY.  (ANTI-NORMAL COMPUTER STORAGE)
C
      ELSE
C
         RIJX = SQRT(RIJ)
         RIJ = MIN(RIJX,CUTOFF)
C
C *** COMPUTE INTEGRALS IN DIATOMIC FRAME
C
         CALL REPP(NI,NJ,RIJ,RI,CCORE)
         IF(ANALYT)WRITE(2)(RI(I),I=1,22)
C
         GAM = RI(1)
         A=1.0D+00/RIJX
         X(1) = X(1)*A
         X(2) = X(2)*A
         X(3) = X(3)*A
         IF (ABS(X(3)).GT.0.99999999D+00) THEN
            X(3) = SIGN(1.0D+00,X(3))
            Y(1) = 0.0D+00
            Y(2) = 1.0D+00
            Y(3) = 0.0D+00
            Z(1) = 1.0D+00
            Z(2) = 0.0D+00
            Z(3) = 0.0D+00
         ELSE
            Z(3)=SQRT(1.0D+00-X(3)*X(3))
            A=1.0D+00/Z(3)
            Y(1)=-A*X(2)*SIGN(1.0D+00,X(1))
            Y(2)=ABS(A*X(1))
            Y(3)=0.0D+00
            Z(1)=-A*X(1)*X(3)
            Z(2)=-A*X(2)*X(3)
         ENDIF
         SI = (NATORB(NI).GT.1)
         SJ = (NATORB(NJ).GT.1)
         IF ( SI .OR. SJ) THEN
            XX11 = X(1)*X(1)
            XX21 = X(2)*X(1)
            XX22 = X(2)*X(2)
            XX31 = X(3)*X(1)
            XX32 = X(3)*X(2)
            XX33 = X(3)*X(3)
            YY11 = Y(1)*Y(1)
            YY21 = Y(2)*Y(1)
            YY22 = Y(2)*Y(2)
            ZZ11 = Z(1)*Z(1)
            ZZ21 = Z(2)*Z(1)
            ZZ22 = Z(2)*Z(2)
            ZZ31 = Z(3)*Z(1)
            ZZ32 = Z(3)*Z(2)
            ZZ33 = Z(3)*Z(3)
            YYZZ11 = YY11+ZZ11
            YYZZ21 = YY21+ZZ21
            YYZZ22 = YY22+ZZ22
            XY11 = 2.0D+00*X(1)*Y(1)
            XY21 =      X(1)*Y(2)+X(2)*Y(1)
            XY22 = 2.0D+00*X(2)*Y(2)
            XY31 =      X(3)*Y(1)
            XY32 =      X(3)*Y(2)
            XZ11 = 2.0D+00*X(1)*Z(1)
            XZ21 =      X(1)*Z(2)+X(2)*Z(1)
            XZ22 = 2.0D+00*X(2)*Z(2)
            XZ31 =      X(1)*Z(3)+X(3)*Z(1)
            XZ32 =      X(2)*Z(3)+X(3)*Z(2)
            XZ33 = 2.0D+00*X(3)*Z(3)
            YZ11 = 2.0D+00*Y(1)*Z(1)
            YZ21 =      Y(1)*Z(2)+Y(2)*Z(1)
            YZ22 = 2.0D+00*Y(2)*Z(2)
            YZ31 =      Y(1)*Z(3)
            YZ32 =      Y(2)*Z(3)
         ENDIF
C
C     (S S/S S)
         W(1)=RI(1)
         KI = 1
         IF (SJ) THEN
C     (S S/PX S)
            W(2)=RI(5)*X(1)
C     (S S/PX PX)
            W(3)=RI(11)*XX11+RI(12)*YYZZ11
C     (S S/PY S)
            W(4)=RI(5)*X(2)
C     (S S/PY PX)
            W(5)=RI(11)*XX21+RI(12)*YYZZ21
C     (S S/PY PY)
            W(6)=RI(11)*XX22+RI(12)*YYZZ22
C     (S S/PZ S)
            W(7)=RI(5)*X(3)
C     (S S/PZ PX)
            W(8)=RI(11)*XX31+RI(12)*ZZ31
C     (S S/PZ PY)
            W(9)=RI(11)*XX32+RI(12)*ZZ32
C     (S S/PZ PZ)
            W(10)=RI(11)*XX33+RI(12)*ZZ33
            KI = 10
         ENDIF
C
         IF (SI) THEN
C     (PX S/S S)
            W(11)=RI(2)*X(1)
            IF (SJ) THEN
C     (PX S/PX S)
               W(12)=RI(6)*XX11+RI(7)*YYZZ11
C     (PX S/PX PX)
               W(13)=X(1)*(RI(13)*XX11+RI(14)*YYZZ11)
     1           +RI(15)*(Y(1)*XY11+Z(1)*XZ11)
C     (PX S/PY S)
               W(14)=RI(6)*XX21+RI(7)*YYZZ21
C     (PX S/PY PX)
               W(15)=X(1)*(RI(13)*XX21+RI(14)*YYZZ21)
     1           +RI(15)*(Y(1)*XY21+Z(1)*XZ21)
C     (PX S/PY PY)
               W(16)=X(1)*(RI(13)*XX22+RI(14)*YYZZ22)
     1           +RI(15)*(Y(1)*XY22+Z(1)*XZ22)
C     (PX S/PZ S)
               W(17)=RI(6)*XX31+RI(7)*ZZ31
C     (PX S/PZ PX)
               W(18)=X(1)*(RI(13)*XX31+RI(14)*ZZ31)
     1           +RI(15)*(Y(1)*XY31+Z(1)*XZ31)
C     (PX S/PZ PY)
               W(19)=X(1)*(RI(13)*XX32+RI(14)*ZZ32)
     1           +RI(15)*(Y(1)*XY32+Z(1)*XZ32)
C     (PX S/PZ PZ)
               W(20)=X(1)*(RI(13)*XX33+RI(14)*ZZ33)
     1           +RI(15)*(          Z(1)*XZ33)
C     (PX PX/S S)
               W(21)=RI(3)*XX11+RI(4)*YYZZ11
C     (PX PX/PX S)
               W(22)=X(1)*(RI(8)*XX11+RI(9)*YYZZ11)
     1           +RI(10)*(Y(1)*XY11+Z(1)*XZ11)
C     (PX PX/PX PX)
               W(23) =
     1     (RI(16)*XX11+RI(17)*YYZZ11)*XX11+RI(18)*XX11*YYZZ11
     2     +RI(19)*(YY11*YY11+ZZ11*ZZ11)
     3     +RI(20)*(XY11*XY11+XZ11*XZ11)
     4     +RI(21)*(YY11*ZZ11+ZZ11*YY11)
     5     +RI(22)*YZ11*YZ11
C     (PX PX/PY S)
               W(24)=X(2)*(RI(8)*XX11+RI(9)*YYZZ11)
     1           +RI(10)*(Y(2)*XY11+Z(2)*XZ11)
C     (PX PX/PY PX)
               W(25) =
     1     (RI(16)*XX11+RI(17)*YYZZ11)*XX21+RI(18)*XX11*YYZZ21
     2     +RI(19)*(YY11*YY21+ZZ11*ZZ21)
     3     +RI(20)*(XY11*XY21+XZ11*XZ21)
     4     +RI(21)*(YY11*ZZ21+ZZ11*YY21)
     5     +RI(22)*YZ11*YZ21
C     (PX PX/PY PY)
               W(26) =
     1     (RI(16)*XX11+RI(17)*YYZZ11)*XX22+RI(18)*XX11*YYZZ22
     2     +RI(19)*(YY11*YY22+ZZ11*ZZ22)
     3     +RI(20)*(XY11*XY22+XZ11*XZ22)
     4     +RI(21)*(YY11*ZZ22+ZZ11*YY22)
     5     +RI(22)*YZ11*YZ22
C     (PX PX/PZ S)
               W(27)=X(3)*(RI(8)*XX11+RI(9)*YYZZ11)
     1           +RI(10)*(         +Z(3)*XZ11)
C     (PX PX/PZ PX)
               W(28) =
     1      (RI(16)*XX11+RI(17)*YYZZ11)*XX31
     2     +(RI(18)*XX11+RI(19)*ZZ11+RI(21)*YY11)*ZZ31
     3     +RI(20)*(XY11*XY31+XZ11*XZ31)
     4     +RI(22)*YZ11*YZ31
C     (PX PX/PZ PY)
               W(29) =
     1      (RI(16)*XX11+RI(17)*YYZZ11)*XX32
     2     +(RI(18)*XX11+RI(19)*ZZ11+RI(21)*YY11)*ZZ32
     3     +RI(20)*(XY11*XY32+XZ11*XZ32)
     4     +RI(22)*YZ11*YZ32
C     (PX PX/PZ PZ)
               W(30) =
     1      (RI(16)*XX11+RI(17)*YYZZ11)*XX33
     2     +(RI(18)*XX11+RI(19)*ZZ11+RI(21)*YY11)*ZZ33
     3     +RI(20)*XZ11*XZ33
C     (PY S/S S)
               W(31)=RI(2)*X(2)
C     (PY S/PX S)
               W(32)=RI(6)*XX21+RI(7)*YYZZ21
C     (PY S/PX PX)
               W(33)=X(2)*(RI(13)*XX11+RI(14)*YYZZ11)
     1           +RI(15)*(Y(2)*XY11+Z(2)*XZ11)
C     (PY S/PY S)
               W(34)=RI(6)*XX22+RI(7)*YYZZ22
C     (PY S/PY PX)
               W(35)=X(2)*(RI(13)*XX21+RI(14)*YYZZ21)
     1           +RI(15)*(Y(2)*XY21+Z(2)*XZ21)
C     (PY S/PY PY)
               W(36)=X(2)*(RI(13)*XX22+RI(14)*YYZZ22)
     1           +RI(15)*(Y(2)*XY22+Z(2)*XZ22)
C     (PY S/PZ S)
               W(37)=RI(6)*XX32+RI(7)*ZZ32
C     (PY S/PZ PX)
               W(38)=X(2)*(RI(13)*XX31+RI(14)*ZZ31)
     1           +RI(15)*(Y(2)*XY31+Z(2)*XZ31)
C     (PY S/PZ PY)
               W(39)=X(2)*(RI(13)*XX32+RI(14)*ZZ32)
     1           +RI(15)*(Y(2)*XY32+Z(2)*XZ32)
C     (PY S/PZ PZ)
               W(40)=X(2)*(RI(13)*XX33+RI(14)*ZZ33)
     1           +RI(15)*(         +Z(2)*XZ33)
C     (PY PX/S S)
               W(41)=RI(3)*XX21+RI(4)*YYZZ21
C     (PY PX/PX S)
               W(42)=X(1)*(RI(8)*XX21+RI(9)*YYZZ21)
     1           +RI(10)*(Y(1)*XY21+Z(1)*XZ21)
C     (PY PX/PX PX)
               W(43) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX11+RI(18)*XX21*YYZZ11
     2     +RI(19)*(YY21*YY11+ZZ21*ZZ11)
     3     +RI(20)*(XY21*XY11+XZ21*XZ11)
     4     +RI(21)*(YY21*ZZ11+ZZ21*YY11)
     5     +RI(22)*YZ21*YZ11
C     (PY PX/PY S)
               W(44)=X(2)*(RI(8)*XX21+RI(9)*YYZZ21)
     1           +RI(10)*(Y(2)*XY21+Z(2)*XZ21)
C     (PY PX/PY PX)
               W(45) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX21+RI(18)*XX21*YYZZ21
     2     +RI(19)*(YY21*YY21+ZZ21*ZZ21)
     3     +RI(20)*(XY21*XY21+XZ21*XZ21)
     4     +RI(21)*(YY21*ZZ21+ZZ21*YY21)
     5     +RI(22)*YZ21*YZ21
C     (PY PX/PY PY)
               W(46) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX22+RI(18)*XX21*YYZZ22
     2     +RI(19)*(YY21*YY22+ZZ21*ZZ22)
     3     +RI(20)*(XY21*XY22+XZ21*XZ22)
     4     +RI(21)*(YY21*ZZ22+ZZ21*YY22)
     5     +RI(22)*YZ21*YZ22
C     (PY PX/PZ S)
               W(47)=X(3)*(RI(8)*XX21+RI(9)*YYZZ21)
     1           +RI(10)*(         +Z(3)*XZ21)
C      (PY PX/PZ PX)
               W(48) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX31
     2     +(RI(18)*XX21+RI(19)*ZZ21+RI(21)*YY21)*ZZ31
     3     +RI(20)*(XY21*XY31+XZ21*XZ31)
     4     +RI(22)*YZ21*YZ31
C      (PY PX/PZ PY)
               W(49) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX32
     2     +(RI(18)*XX21+RI(19)*ZZ21+RI(21)*YY21)*ZZ32
     3     +RI(20)*(XY21*XY32+XZ21*XZ32)
     4     +RI(22)*YZ21*YZ32
C      (PY PX/PZ PZ)
               W(50) =
     1     (RI(16)*XX21+RI(17)*YYZZ21)*XX33
     2     +(RI(18)*XX21+RI(19)*ZZ21+RI(21)*YY21)*ZZ33
     3     +RI(20)*XZ21*XZ33
C     (PY PY/S S)
               W(51)=RI(3)*XX22+RI(4)*YYZZ22
C     (PY PY/PX S)
               W(52)=X(1)*(RI(8)*XX22+RI(9)*YYZZ22)
     1           +RI(10)*(Y(1)*XY22+Z(1)*XZ22)
C      (PY PY/PX PX)
               W(53) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX11+RI(18)*XX22*YYZZ11
     2     +RI(19)*(YY22*YY11+ZZ22*ZZ11)
     3     +RI(20)*(XY22*XY11+XZ22*XZ11)
     4     +RI(21)*(YY22*ZZ11+ZZ22*YY11)
     5     +RI(22)*YZ22*YZ11
C     (PY PY/PY S)
               W(54)=X(2)*(RI(8)*XX22+RI(9)*YYZZ22)
     1           +RI(10)*(Y(2)*XY22+Z(2)*XZ22)
C      (PY PY/PY PX)
               W(55) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX21+RI(18)*XX22*YYZZ21
     2     +RI(19)*(YY22*YY21+ZZ22*ZZ21)
     3     +RI(20)*(XY22*XY21+XZ22*XZ21)
     4     +RI(21)*(YY22*ZZ21+ZZ22*YY21)
     5     +RI(22)*YZ22*YZ21
C      (PY PY/PY PY)
               W(56) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX22+RI(18)*XX22*YYZZ22
     2     +RI(19)*(YY22*YY22+ZZ22*ZZ22)
     3     +RI(20)*(XY22*XY22+XZ22*XZ22)
     4     +RI(21)*(YY22*ZZ22+ZZ22*YY22)
     5     +RI(22)*YZ22*YZ22
C     (PY PY/PZ S)
               W(57)=X(3)*(RI(8)*XX22+RI(9)*YYZZ22)
     1           +RI(10)*(         +Z(3)*XZ22)
C      (PY PY/PZ PX)
               W(58) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX31
     2     +(RI(18)*XX22+RI(19)*ZZ22+RI(21)*YY22)*ZZ31
     3     +RI(20)*(XY22*XY31+XZ22*XZ31)
     4     +RI(22)*YZ22*YZ31
C      (PY PY/PZ PY)
               W(59) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX32
     2     +(RI(18)*XX22+RI(19)*ZZ22+RI(21)*YY22)*ZZ32
     3     +RI(20)*(XY22*XY32+XZ22*XZ32)
     4     +RI(22)*YZ22*YZ32
C      (PY PY/PZ PZ)
               W(60) =
     1     (RI(16)*XX22+RI(17)*YYZZ22)*XX33
     2     +(RI(18)*XX22+RI(19)*ZZ22+RI(21)*YY22)*ZZ33
     3     +RI(20)*XZ22*XZ33
C     (PZ S/SS)
               W(61)=RI(2)*X(3)
C     (PZ S/PX S)
               W(62)=RI(6)*XX31+RI(7)*ZZ31
C     (PZ S/PX PX)
               W(63)=X(3)*(RI(13)*XX11+RI(14)*YYZZ11)
     1           +RI(15)*(         +Z(3)*XZ11)
C     (PZ S/PY S)
               W(64)=RI(6)*XX32+RI(7)*ZZ32
C     (PZ S/PY PX)
               W(65)=X(3)*(RI(13)*XX21+RI(14)*YYZZ21)
     1           +RI(15)*(         +Z(3)*XZ21)
C     (PZ S/PY PY)
               W(66)=X(3)*(RI(13)*XX22+RI(14)*YYZZ22)
     1           +RI(15)*(         +Z(3)*XZ22)
C     (PZ S/PZ S)
               W(67)=RI(6)*XX33+RI(7)*ZZ33
C     (PZ S/PZ PX)
               W(68)=X(3)*(RI(13)*XX31+RI(14)*ZZ31)
     1           +RI(15)*(         +Z(3)*XZ31)
C     (PZ S/PZ PY)
               W(69)=X(3)*(RI(13)*XX32+RI(14)*ZZ32)
     1           +RI(15)*(         +Z(3)*XZ32)
C     (PZ S/PZ PZ)
               W(70)=X(3)*(RI(13)*XX33+RI(14)*ZZ33)
     1           +RI(15)*(         +Z(3)*XZ33)
C     (PZ PX/S S)
               W(71)=RI(3)*XX31+RI(4)*ZZ31
C     (PZ PX/PX S)
               W(72)=X(1)*(RI(8)*XX31+RI(9)*ZZ31)
     1           +RI(10)*(Y(1)*XY31+Z(1)*XZ31)
C      (PZ PX/PX PX)
               W(73) =
     1     (RI(16)*XX31+RI(17)*ZZ31)*XX11+RI(18)*XX31*YYZZ11
     2     +RI(19)*ZZ31*ZZ11
     3     +RI(20)*(XY31*XY11+XZ31*XZ11)
     4     +RI(21)*ZZ31*YY11
     5     +RI(22)*YZ31*YZ11
C     (PZ PX/PY S)
               W(74)=X(2)*(RI(8)*XX31+RI(9)*ZZ31)
     1           +RI(10)*(Y(2)*XY31+Z(2)*XZ31)
C      (PZ PX/PY PX)
               W(75) =
     1     (RI(16)*XX31+RI(17)*ZZ31)*XX21+RI(18)*XX31*YYZZ21
     2     +RI(19)*ZZ31*ZZ21
     3     +RI(20)*(XY31*XY21+XZ31*XZ21)
     4     +RI(21)*ZZ31*YY21
     5     +RI(22)*YZ31*YZ21
C      (PZ PX/PY PY)
               W(76) =
     1     (RI(16)*XX31+RI(17)*ZZ31)*XX22+RI(18)*XX31*YYZZ22
     2     +RI(19)*ZZ31*ZZ22
     3     +RI(20)*(XY31*XY22+XZ31*XZ22)
     4     +RI(21)*ZZ31*YY22
     5     +RI(22)*YZ31*YZ22
C     (PZ PX/PZ S)
               W(77)=X(3)*(RI(8)*XX31+RI(9)*ZZ31)
     1           +RI(10)*(         +Z(3)*XZ31)
C     (PZ PX/PZ PX)
               W(78) =
     1      (RI(16)*XX31+RI(17)*ZZ31)*XX31
     2     +(RI(18)*XX31+RI(19)*ZZ31)*ZZ31
     3     +RI(20)*(XY31*XY31+XZ31*XZ31)
     4     +RI(22)*YZ31*YZ31
C      (PZ PX/PZ PY)
               W(79) =
     1      (RI(16)*XX31+RI(17)*ZZ31)*XX32
     2     +(RI(18)*XX31+RI(19)*ZZ31)*ZZ32
     3     +RI(20)*(XY31*XY32+XZ31*XZ32)
     4     +RI(22)*YZ31*YZ32
C      (PZ PX/PZ PZ)
               W(80) =
     1      (RI(16)*XX31+RI(17)*ZZ31)*XX33
     2     +(RI(18)*XX31+RI(19)*ZZ31)*ZZ33
     3     +RI(20)*XZ31*XZ33
C     (PZ PY/S S)
               W(81)=RI(3)*XX32+RI(4)*ZZ32
C     (PZ PY/PX S)
               W(82)=X(1)*(RI(8)*XX32+RI(9)*ZZ32)
     1           +RI(10)*(Y(1)*XY32+Z(1)*XZ32)
C      (PZ PY/PX PX)
               W(83) =
     1     (RI(16)*XX32+RI(17)*ZZ32)*XX11+RI(18)*XX32*YYZZ11
     2     +RI(19)*ZZ32*ZZ11
     3     +RI(20)*(XY32*XY11+XZ32*XZ11)
     4     +RI(21)*ZZ32*YY11
     5     +RI(22)*YZ32*YZ11
C     (PZ PY/PY S)
               W(84)=X(2)*(RI(8)*XX32+RI(9)*ZZ32)
     1           +RI(10)*(Y(2)*XY32+Z(2)*XZ32)
C      (PZ PY/PY PX)
               W(85) =
     1     (RI(16)*XX32+RI(17)*ZZ32)*XX21+RI(18)*XX32*YYZZ21
     2     +RI(19)*ZZ32*ZZ21
     3     +RI(20)*(XY32*XY21+XZ32*XZ21)
     4     +RI(21)*ZZ32*YY21
     5     +RI(22)*YZ32*YZ21
C      (PZ PY/PY PY)
               W(86) =
     1     (RI(16)*XX32+RI(17)*ZZ32)*XX22+RI(18)*XX32*YYZZ22
     2     +RI(19)*ZZ32*ZZ22
     3     +RI(20)*(XY32*XY22+XZ32*XZ22)
     4     +RI(21)*ZZ32*YY22
     5     +RI(22)*YZ32*YZ22
C     (PZ PY/PZ S)
               W(87)=X(3)*(RI(8)*XX32+RI(9)*ZZ32)
     1           +RI(10)*(         +Z(3)*XZ32)
C      (PZ PY/PZ PX)
               W(88) =
     1      (RI(16)*XX32+RI(17)*ZZ32)*XX31
     2     +(RI(18)*XX32+RI(19)*ZZ32)*ZZ31
     3     +RI(20)*(XY32*XY31+XZ32*XZ31)
     4     +RI(22)*YZ32*YZ31
C      (PZ PY/PZ PY)
               W(89) =
     1      (RI(16)*XX32+RI(17)*ZZ32)*XX32
     2     +(RI(18)*XX32+RI(19)*ZZ32)*ZZ32
     3     +RI(20)*(XY32*XY32+XZ32*XZ32)
     4     +RI(22)*YZ32*YZ32
C       (PZ PY/PZ PZ)
               W(90) =
     1      (RI(16)*XX32+RI(17)*ZZ32)*XX33
     2     +(RI(18)*XX32+RI(19)*ZZ32)*ZZ33
     3     +RI(20)*XZ32*XZ33
C     (PZ PZ/S S)
               W(91)=RI(3)*XX33+RI(4)*ZZ33
C     (PZ PZ/PX S)
               W(92)=X(1)*(RI(8)*XX33+RI(9)*ZZ33)
     1           +RI(10)*(          Z(1)*XZ33)
C       (PZ PZ/PX PX)
               W(93) =
     1     (RI(16)*XX33+RI(17)*ZZ33)*XX11+RI(18)*XX33*YYZZ11
     2     +RI(19)*ZZ33*ZZ11
     3     +RI(20)*XZ33*XZ11
     4     +RI(21)*ZZ33*YY11
C     (PZ PZ/PY S)
               W(94)=X(2)*(RI(8)*XX33+RI(9)*ZZ33)
     1           +RI(10)*(         +Z(2)*XZ33)
C       (PZ PZ/PY PX)
               W(95) =
     1     (RI(16)*XX33+RI(17)*ZZ33)*XX21+RI(18)*XX33*YYZZ21
     2     +RI(19)*ZZ33*ZZ21
     3     +RI(20)*XZ33*XZ21
     4     +RI(21)*ZZ33*YY21
C       (PZ PZ/PY PY)
               W(96) =
     1     (RI(16)*XX33+RI(17)*ZZ33)*XX22+RI(18)*XX33*YYZZ22
     2     +RI(19)*ZZ33*ZZ22
     3     +RI(20)*XZ33*XZ22
     4     +RI(21)*ZZ33*YY22
C     (PZ PZ/PZ S)
               W(97)=X(3)*(RI(8)*XX33+RI(9)*ZZ33)
     1           +RI(10)*(         +Z(3)*XZ33)
C       (PZ PZ/PZ PX)
               W(98) =
     1      (RI(16)*XX33+RI(17)*ZZ33)*XX31
     2     +(RI(18)*XX33+RI(19)*ZZ33)*ZZ31
     3     +RI(20)*XZ33*XZ31
C       (PZ PZ/PZ PY)
               W(99) =
     1      (RI(16)*XX33+RI(17)*ZZ33)*XX32
     2     +(RI(18)*XX33+RI(19)*ZZ33)*ZZ32
     3     +RI(20)*XZ33*XZ32
C       (PZ PZ/PZ PZ)
               W(100) =
     1      (RI(16)*XX33+RI(17)*ZZ33)*XX33
     2     +(RI(18)*XX33+RI(19)*ZZ33)*ZZ33
     3     +RI(20)*XZ33*XZ33
               KI = 100
            ELSE
C     (PX S/S S)
               W(2)=RI(2)*X(1)
C     (PX PX/S S)
               W(3)=RI(3)*XX11+RI(4)*YYZZ11
C     (PY S/S S)
               W(4)=RI(2)*X(2)
C     (PY PX/S S)
               W(5)=RI(3)*XX21+RI(4)*YYZZ21
C     (PY PY/S S)
               W(6)=RI(3)*XX22+RI(4)*YYZZ22
C     (PZ S/SS)
               W(7)=RI(2)*X(3)
C     (PZ PX/S S)
               W(8)=RI(3)*XX31+RI(4)*ZZ31
C     (PZ PY/S S)
               W(9)=RI(3)*XX32+RI(4)*ZZ32
C     (PZ PZ/S S)
               W(10)=RI(3)*XX33+RI(4)*ZZ33
               KI = 10
            END IF
         END IF
C
C *** NOW ROTATE THE NUCLEAR ATTRACTION INTEGRALS.
C *** THE STORAGE OF THE NUCLEAR ATTRACTION INTEGRALS  CORE(KL/IJ) IS
C     (SS/)=1,   (SO/)=2,   (OO/)=3,   (PP/)=4
C
         E1B(1)=-CSS1
         IF(NATORB(NI).EQ.4) THEN
            E1B(2) = -CSP1 *X(1)
            E1B(3) = -CPPS1*XX11-CPPP1*YYZZ11
            E1B(4) = -CSP1 *X(2)
            E1B(5) = -CPPS1*XX21-CPPP1*YYZZ21
            E1B(6) = -CPPS1*XX22-CPPP1*YYZZ22
            E1B(7) = -CSP1 *X(3)
            E1B(8) = -CPPS1*XX31-CPPP1*ZZ31
            E1B(9) = -CPPS1*XX32-CPPP1*ZZ32
            E1B(10)= -CPPS1*XX33-CPPP1*ZZ33
         END IF
         E2A(1)=-CSS2
         IF(NATORB(NJ).EQ.4) THEN
            E2A(2) = -CSP2 *X(1)
            E2A(3) = -CPPS2*XX11-CPPP2*YYZZ11
            E2A(4) = -CSP2 *X(2)
            E2A(5) = -CPPS2*XX21-CPPP2*YYZZ21
            E2A(6) = -CPPS2*XX22-CPPP2*YYZZ22
            E2A(7) = -CSP2 *X(3)
            E2A(8) = -CPPS2*XX31-CPPP2*ZZ31
            E2A(9) = -CPPS2*XX32-CPPP2*ZZ32
            E2A(10)= -CPPS2*XX33-CPPP2*ZZ33
         END IF
         IF(ABS(TORE(NI)).GT.20.0D+00.AND.ABS(TORE(NJ)).GT.20.0D+00)THEN
C SPARKLE-SPARKLE INTERACTION
            ENUC=0.0D+00
            RETURN
         ELSEIF (RIJ.LT.1.0D+00.AND.NATORB(NI)*NATORB(NJ).EQ.0) THEN
            ENUC=0.0D+00
            RETURN
         ENDIF
         SCALE = EXP(-ALP(NI)*RIJ)+EXP(-ALP(NJ)*RIJ)
C
         IF (NI.EQ.24.AND.NJ.EQ.24) THEN
            SCALE = EXP(-ALPTM(NI)*RIJ)+EXP(-ALPTM(NJ)*RIJ)
         ENDIF
C
         NT=NI+NJ
         IF(NT.EQ.8.OR.NT.EQ.9) THEN
            IF(NI.EQ.7.OR.NI.EQ.8) SCALE=SCALE+(RIJ-1.0D+00)*
     *EXP(-ALP(NI)*RIJ)
            IF(NJ.EQ.7.OR.NJ.EQ.8) SCALE=SCALE+(RIJ-1.0D+00)*
     *EXP(-ALP(NJ)*RIJ)
         ENDIF
         ENUC = TORE(NI)*TORE(NJ)*GAM
         SCALE=ABS(SCALE*ENUC)
         IF(ITYPE.EQ.2.AND.(NI.EQ.5.OR.NJ.EQ.5))THEN
C
C   LOAD IN AM1 BORON GAUSSIANS
C
            NK=NI+NJ-5
C   NK IS THE ATOMIC NUMBER OF THE NON-BORON ATOM
            NL=1
            IF(NK.EQ.1)NL=2
            IF(NK.EQ.6)NL=3
            IF(NK.EQ.9.OR.NK.EQ.17.OR.NK.EQ.35.OR.NK.EQ.53)NL=4
            DO 30 I=1,3
               FN1(5,I)=BORON1(I,NL)
               FN2(5,I)=BORON2(I,NL)
   30       FN3(5,I)=BORON3(I,NL)
         ENDIF
         IF(ITYPE.EQ.2.OR.ITYPE.EQ.3) THEN
            DO 40 IG=1,10
               IF(ABS(FN1(NI,IG)).GT.0.0D+00) THEN
                  AX = FN2(NI,IG)*(RIJ-FN3(NI,IG))**2
                  IF(AX .LE. 25.0D+00) THEN
                     SCALE=SCALE +TORE(NI)*TORE(NJ)/RIJ*FN1(NI,IG)*EXP(-
     1AX)
                  ENDIF
               ENDIF
               IF(ABS(FN1(NJ,IG)).GT.0.0D+00) THEN
                  AX = FN2(NJ,IG)*(RIJ-FN3(NJ,IG))**2
                  IF(AX .LE. 25.0D+00) THEN
                     SCALE=SCALE +TORE(NI)*TORE(NJ)/RIJ*FN1(NJ,IG)*EXP(-
     1AX)
                  ENDIF
               ENDIF
   40       CONTINUE
         ENDIF
         ENUC=ENUC+SCALE
C
         IF(NATORB(NI)*NATORB(NJ).EQ.0)KI=0
         KR=KR+KI
C
C
      ENDIF
      RETURN
      END
C*MODULE MPCINT  *DECK SET
      SUBROUTINE SET (S1,S2,NA,NB,RAB,II)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /SETC/ A(7),B(7),SA,SB,FACTOR,ISP,IPS
C***********************************************************************
C
C     SET IS PART OF THE OVERLAP CALCULATION, CALLED BY OVERLP.
C         IT CALLS AINTGS AND BINTGS
C
C***********************************************************************
      IF (NA.GT.NB) GO TO 10
      ISP=1
      IPS=2
      SA=S1
      SB=S2
      GO TO 20
   10 ISP=2
      IPS=1
      SA=S2
      SB=S1
   20 J=II+2
      IF (II.GT.3) J=J-1
      ALPHA=0.5D+00*RAB*(SA+SB)
      BETA=0.5D+00*RAB*(SB-SA)
      JCALL=J-1
      CALL AINTGS (ALPHA,JCALL)
      CALL BINTGS (BETA,JCALL)
      RETURN
C
      END
C*MODULE MPCINT  *DECK SS
      DOUBLE PRECISION FUNCTION SS(NA,NB,LA1,LB1,M1,UA,UB,R1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL FIRST
      DIMENSION FA(0:13),AFF(0:2,0:2,0:2),AF(0:19),BF(0:19),
     1BI(0:12,0:12)
      SAVE AFF, FA, BI, FIRST
      DATA FIRST /.TRUE./
      DATA AFF/27*0.0D+00/
      DATA FA/1.0D+00,1.0D+00,2.0D+00,6.0D+00,24.0D+00,120.0D+00,
     *720.0D+00,5040.0D+00,40320.0D+00,
     1362880.0D+00,3628800.0D+00,39916800.0D+00,479001600.0D+00,
     *6227020800.0D+00/
      M=M1-1
      LB=LB1-1
      LA=LA1-1
      R=R1/0.529167D+00
      IF(FIRST) THEN
         FIRST=.FALSE.
C
C           INITIALISE SOME CONSTANTS
C
C                  BINOMIALS
C
         DO 10 I=0,12
            BI(I,0)=1.0D+00
            BI(I,I)=1.0D+00
   10    CONTINUE
         DO 20 I=0,11
            I1=I-1
            DO 20 J=0,I1
               BI(I+1,J+1)=BI(I,J+1)+BI(I,J)
   20    CONTINUE
         AFF(0,0,0)=1.0D+00
         AFF(1,0,0)=1.0D+00
         AFF(1,1,0)=1.0D+00
         AFF(2,0,0)=1.5D+00
         AFF(2,1,0)=1.73205D+00
         AFF(2,2,0)=1.224745D+00
         AFF(2,0,2)=-0.5D+00
      ENDIF
      P=(UA+UB)*R*0.5D+00
      B=(UA-UB)*R*0.5D+00
      QUO=1/P
      AF(0)=QUO*EXP(-P)
      DO 30 N=1,19
         AF(N)=N*QUO*AF(N-1)+AF(0)
   30 CONTINUE
      CALL BFN(B,BF)
      SUM=0.0D+00
      LAM1=LA-M
      LBM1=LB-M
C
C          START OF OVERLAP CALCULATION PROPER
C
      DO 50 I=0,LAM1,2
         IA=NA+I-LA
         IC=LA-I-M
         DO 50 J=0,LBM1,2
            IB=NB+J-LB
            ID=LB-J-M
            SUM1=0.0D+00
            IAB=IA+IB
            DO 40 K1=0,IA
               DO 40 K2=0,IB
                  DO 40 K3=0,IC
                     DO 40 K4=0,ID
                        DO 40 K5=0,M
                           IAF=IAB-K1-K2+K3+K4+2*K5
                           DO 40 K6=0,M
                              IBF=K1+K2+K3+K4+2*K6
                              JX=(-1)**(M+K2+K4+K5+K6)
                              SUM1=SUM1+BI(ID,K4)*
     1BI(M,K5)*BI(IC,K3)*BI(IB,K2)*BI(IA,K1)*
     2BI(M,K6)*JX*AF(IAF)*BF(IBF)
   40       CONTINUE
            SUM=SUM+SUM1*AFF(LA,M,I)*AFF(LB,M,J)
   50 CONTINUE
      SS=SUM*R**(NA+NB+1)*UA**NA*UB**NB/(2.0D+00**(M+1))*
     1SQRT(UA*UB/(FA(NA+NA)*FA(NB+NB))*((LA+LA+1)*(LB+LB+1)))
      RETURN
      END
