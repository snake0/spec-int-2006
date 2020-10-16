C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C  4 NOV 03 - DGF - ADD QUADRUPLE PRECISION DIAGONALIZATION, S+T INTS
C
C     THIS CODE IS USED FOR 128 BIT PRECISION DURING THE RESOLUTION
C     OF THE IDENTITY APPROXIMATION TO RELATIVISTIC PVP TRANSFORMATIONS.
C
C     IF THE DOUBLE PRECISION DATA TYPE IS NOT AVAILABLE ON A COMPUTER,
C     THE COMPILING SCRIPT MUST CONVERT THIS SOURCE TO DOUBLE PRECISION,
C     AFTER WHICH THE INPUT OPTION $RELWFN MODEQR=8 WILL REFUSE TO RUN.
C
C*MODULE QEIGEN  *DECK QJACDG
      SUBROUTINE QJACDG(A,VEC,EIG,JBIG,BIG,LDVEC,N)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION A(*),VEC(LDVEC,N),EIG(N),JBIG(N),BIG(N)
C
      PARAMETER (ONE=1.0D+00)
C
C     ----- JACOBI DIAGONALIZATION OF SYMMETRIC MATRIX -----
C     SYMMETRIC MATRIX -A- OF DIMENSION -N- IS DESTROYED ON EXIT.
C     ALL EIGENVECTORS ARE FOUND, SO -VEC- MUST BE SQUARE,
C     UNLESS SOMEONE TAKES THE TROUBLE TO LOOK AT -NMAX- BELOW.
C     -BIG- AND -JBIG- ARE SCRATCH WORK ARRAYS.
C
      CALL QCLR(VEC,1,LDVEC*N)
      DO 20 I = 1,N
        VEC(I,I) = ONE
   20 CONTINUE
C
      NB1 = N
      NB2 = (NB1*NB1+NB1)/2
      NMIN = 1
      NMAX = NB1
C
      CALL QJACDIA(A,VEC,NB1,NB2,LDVEC,NMIN,NMAX,BIG,JBIG)
C
      DO 30 I=1,N
        EIG(I) = A((I*I+I)/2)
   30 CONTINUE
C
      CALL QJACORD(VEC,EIG,NB1,LDVEC)
      RETURN
      END
C*MODULE EIGEN   *DECK QJACDIA
      SUBROUTINE QJACDIA(F,VEC,NB1,NB2,LDVEC,NMIN,NMAX,BIG,JBIG)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL GOPARR,DSKWRK,MASWRK
      DIMENSION F(NB2),VEC(LDVEC,NB1),BIG(NB1),JBIG(NB1)
C
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C     PARAMETER (ROOT2=0.707106781186548D+00 )
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, D1050=1.05D+00,
     *           D1500=1.5D+00, D3875=3.875D+00,
     *           D0500=0.5D+00, D1375=1.375D+00, D0250=0.25D+00 )
      PARAMETER (C2=1.0D-18, C3=4.0D-25,
     *           C4=2.0D-25, C5=8.0D-16, C6=3.0D-12 )
C     NOTE THAT THE DOUBLE PRECISION CODE USES
C     PARAMETER (C2=1.0D-12, C3=4.0D-16,
C    *           C4=2.0D-16, C5=8.0D-09, C6=3.0D-06 )
C     WARNING: IT IS NOT CLEAR WHAT THESE NUMBERS SHOULD BE IN QUADRUPLE
C              PRECISION, AND IN PARTICULAR IF THE NUMBER OF SIGNIFICANT
C              FIGURES IN QUADRUPLE PRECISION IS MACHINE-INDEPENDENT.
C              ONE THING IS CLEAR:
C         *** DO NOT CALL THIS ROUTINE ON MACHINES WHERE 128-BIT
C         PRECISION IS NOT AVAILABLE!! GARBAGE WILL BE PRODUCED DUE TO
C         TRYING TO MAKE SENSE OUT OF ZEROES. THE THRESHOLDS ASSUME
C         AT LEAST 25 SIGNIFICANT FIGURES (AIX APPEARS TO HAVE 30).
C         25 MAY NOT BE ENOUGH FOR TRUE QUADRUPLE PRECISION MATRICES!
C         (HOPEFULLY ENOUGH FOR D.P. DIAGONALISED IN QUADRUPLE)
C
C      F IS THE MATRIX TO BE DIAGONALIZED, F IS STORED TRIANGULAR
C      VEC IS THE ARRAY OF EIGENVECTORS, DIMENSION NB1*NB1
C      BIG AND JBIG ARE TEMPORARY SCRATCH AREAS OF DIMENSION NB1
C      THE ROTATIONS AMONG THE FIRST NMIN BASIS FUNCTIONS ARE NOT
C      ACCOUNTED FOR.
C      THE ROTATIONS AMONG THE LAST NB1-NMAX BASIS FUNCTIONS ARE NOT
C      ACCOUNTED FOR.
C
      ROOT2=SQRT(D0500)
      IEAA=0
      IEAB=0
      TT=ZERO
      EPS = 64.0D+00*QEPSLON(ONE)
C     EPS = 64.0D+00*EPSLON(1.0D+00)
C
C      LOOP OVER COLUMNS (K) OF TRIANGULAR MATRIX TO DETERMINE
C      LARGEST OFF-DIAGONAL ELEMENTS IN ROW(I).
C
      DO 20 I=1,NB1
         BIG(I)=ZERO
         JBIG(I)=0
         IF(I.LT.NMIN  .OR.  I.EQ.1) GO TO 20
         II = (I*I-I)/2
         J=MIN(I-1,NMAX)
         DO 10 K=1,J
            IF(ABS(BIG(I)).GE.ABS(F(II+K))) GO TO 10
            BIG(I)=F(II+K)
            JBIG(I)=K
   10    CONTINUE
C        WRITE(6,*) I,'-TH LARGEST ',BIG(I)
   20 CONTINUE
C
C     ----- 2X2 JACOBI ITERATIONS BEGIN HERE -----
C
      MAXIT=MAX(NB2*20,500)
      ITER=0
   30 CONTINUE
      ITER=ITER+1
C
C      FIND SMALLEST DIAGONAL ELEMENT
C
      SD=D1050
      JJ=0
      DO 40 J=1,NB1
         JJ=JJ+J
         SD= MIN(SD,ABS(F(JJ)))
   40 CONTINUE
      TEST = MAX(EPS, C2*MAX(SD,C6))
C
C      FIND LARGEST OFF-DIAGONAL ELEMENT
C
      T=ZERO
      I1=MAX(2,NMIN)
      IB = I1
      DO 50 I=I1,NB1
         IF(T.GE.ABS(BIG(I))) GO TO 50
         T= ABS(BIG(I))
         IB=I
   50 CONTINUE
C
C      TEST FOR CONVERGENCE, THEN DETERMINE ROTATION.
C
      IF(T.LT.TEST) RETURN
C                   ******
C
      IF(ITER.GT.MAXIT) THEN
         IF (MASWRK) THEN
            WRITE(6,*) 'JACOBI DIAGONALIZATION FAILS, DIMENSION=',NB1
            WRITE(6,9020) ITER,T,TEST,SD
         ENDIF
         CALL ABRT
         STOP
      END IF
C
      IA=JBIG(IB)
      IAA=IA*(IA-1)/2
      IBB=IB*(IB-1)/2
      DIF=F(IAA+IA)-F(IBB+IB)
      IF(ABS(DIF).GT.C3*T) GO TO 70
      SX=ROOT2
      CX=ROOT2
      GO TO 110
   70 T2X2=BIG(IB)/DIF
      T2X25=T2X2*T2X2
      IF(T2X25 . GT . C4) GO TO 80
      CX=ONE
      SX=T2X2
      GO TO 110
   80 IF(T2X25 . GT . C5) GO TO 90
      SX=T2X2*(ONE-D1500*T2X25)
      CX=ONE-D0500*T2X25
      GO TO 110
   90 IF(T2X25 . GT . C6) GO TO 100
      CX=ONE+T2X25*(T2X25*D1375 - D0500)
      SX= T2X2*(ONE + T2X25*(T2X25*D3875 - D1500))
      GO TO 110
  100 T=D0250  / SQRT(D0250   + T2X25)
      CX= SQRT(D0500   + T)
      SX= SIGN( SQRT(D0500   - T),T2X2)
  110 IEAR=IAA+1
      IEBR=IBB+1
C
      DO 230 IR=1,NB1
         T=F(IEAR)*SX
         F(IEAR)=F(IEAR)*CX+F(IEBR)*SX
         F(IEBR)=T-F(IEBR)*CX
         IF(IR-IA) 220,120,130
  120    TT=F(IEBR)
         IEAA=IEAR
         IEAB=IEBR
         F(IEBR)=BIG(IB)
         IEAR=IEAR+IR-1
         IF(JBIG(IR)) 200,220,200
  130    T=F(IEAR)
         IT=IA
         IEAR=IEAR+IR-1
         IF(IR-IB) 180,150,160
  150    F(IEAA)=F(IEAA)*CX+F(IEAB)*SX
         F(IEAB)=TT*CX+F(IEBR)*SX
         F(IEBR)=TT*SX-F(IEBR)*CX
         IEBR=IEBR+IR-1
         GO TO 200
  160    IF(  ABS(T) . GE .  ABS(F(IEBR))) GO TO 170
         IF(IB.GT.NMAX) GO TO 170
         T=F(IEBR)
         IT=IB
  170    IEBR=IEBR+IR-1
  180    IF(  ABS(T) . LT .  ABS(BIG(IR))) GO TO 190
         BIG(IR) = T
         JBIG(IR) = IT
         GO TO 220
  190    IF(IA . NE . JBIG(IR) . AND . IB . NE . JBIG(IR))  GO TO 220
  200    KQ=IEAR-IR-IA+1
         BIG(IR)=ZERO
         IR1=MIN(IR-1,NMAX)
         DO 210 I=1,IR1
            K=KQ+I
            IF(ABS(BIG(IR)) . GE . ABS(F(K)))  GO TO 210
            BIG(IR) = F(K)
            JBIG(IR)=I
  210    CONTINUE
  220    IEAR=IEAR+1
  230    IEBR=IEBR+1
C
      DO 240 I=1,NB1
         T1=VEC(I,IA)*CX + VEC(I,IB)*SX
         T2=VEC(I,IA)*SX - VEC(I,IB)*CX
         VEC(I,IA)=T1
         VEC(I,IB)=T2
  240 CONTINUE
      GO TO 30
C
 9020 FORMAT(1X,'ITER=',I6,' T,TEST,SD=',1P,3E20.10)
      END
C*MODULE EIGEN   *DECK QJACORD
      SUBROUTINE QJACORD(VEC,EIG,N,LDVEC)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION VEC(LDVEC,N),EIG(N)
C
C     ---- SORT EIGENDATA INTO ASCENDING ORDER -----
C
      DO 290 I = 1, N
         JJ = I
         DO 270 J = I, N
            IF (EIG(J) .LT. EIG(JJ)) JJ = J
  270    CONTINUE
         IF (JJ .EQ. I) GO TO 290
         T = EIG(JJ)
         EIG(JJ) = EIG(I)
         EIG(I) = T
         DO 280 J = 1, N
            T = VEC(J,JJ)
            VEC(J,JJ) = VEC(J,I)
            VEC(J,I) = T
  280    CONTINUE
  290 CONTINUE
      RETURN
      END
C*MODULE QEIGEN  *DECK QEPSLON
      DOUBLE PRECISION FUNCTION QEPSLON (X)
C*
C*    AUTHORS -
C*       THIS ROUTINE WAS TAKEN FROM EISPACK EDITION 3 DATED 4/6/83
C*       THIS VERSION IS BY S. T. ELBERT, AMES LABORATORY-USDOE NOV 1986
C*
C*    PURPOSE -
C*       ESTIMATE UNIT ROUNDOFF IN QUANTITIES OF SIZE X.
C*
C*    ON ENTRY -
C*       X      - WORKING PRECISION REAL
C*                VALUES TO FIND EPSLON FOR
C*
C*    ON EXIT -
C*       EPSLON - WORKING PRECISION REAL
C*                SMALLEST POSITIVE VALUE SUCH THAT X+EPSLON .NE. ZERO
C*
C*    QUALIFICATIONS -
C*       THIS ROUTINE SHOULD PERFORM PROPERLY ON ALL SYSTEMS
C*       SATISFYING THE FOLLOWING TWO ASSUMPTIONS,
C*          1.  THE BASE USED IN REPRESENTING FLOATING POINT
C*              NUMBERS IS NOT A POWER OF THREE.
C*          2.  THE QUANTITY  A  IN STATEMENT 10 IS REPRESENTED TO
C*              THE ACCURACY USED IN FLOATING POINT VARIABLES
C*              THAT ARE STORED IN MEMORY.
C*       THE STATEMENT NUMBER 10 AND THE GO TO 10 ARE INTENDED TO
C*       FORCE OPTIMIZING COMPILERS TO GENERATE CODE SATISFYING
C*       ASSUMPTION 2.
C*       UNDER THESE ASSUMPTIONS, IT SHOULD BE TRUE THAT,
C*              A  IS NOT EXACTLY EQUAL TO FOUR-THIRDS,
C*              B  HAS A ZERO FOR ITS LAST BIT OR DIGIT,
C*              C  IS NOT EXACTLY EQUAL TO ONE,
C*              EPS  MEASURES THE SEPARATION OF 1.0 FROM
C*                   THE NEXT LARGER FLOATING POINT NUMBER.
C*       THE DEVELOPERS OF EISPACK WOULD APPRECIATE BEING INFORMED
C*       ABOUT ANY SYSTEMS WHERE THESE ASSUMPTIONS DO NOT HOLD.
C*
C*    DIFFERENCES FROM EISPACK 3 -
C*       USE IS MADE OF PARAMETER STATEMENTS AND INTRINSIC FUNCTIONS
C*       --NO EXECUTEABLE CODE CHANGES--
C*
C*    NOTE -
C*       QUESTIONS AND COMMENTS CONCERNING EISPACK SHOULD BE DIRECTED TO
C*       B. S. GARBOW, APPLIED MATH. DIVISION, ARGONNE NATIONAL LAB.
C
      DOUBLE PRECISION A,B,C,EPS,X,ABS
      DOUBLE PRECISION ZERO, ONE, THREE, FOUR
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, THREE=3.0D+00, FOUR=4.0D+00)
C
C-----------------------------------------------------------------------
C
      A = FOUR/THREE
   10 B = A - ONE
      C = B + B + B
      EPS = ABS(C - ONE)
      IF (EPS .EQ. ZERO) GO TO 10
      QEPSLON = EPS*ABS(X)
      RETURN
      END
C*MODULE QEIGEN  *DECK QCLR
      SUBROUTINE QCLR(A,INCA,N)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION A(*)
C
      PARAMETER (ZERO=0.0D+00)
C
C     ----- ZERO OUT VECTOR -A-, USING INCREMENT -INCA- -----
C
      IF (INCA .NE. 1) GO TO 200
      DO 110 L=1,N
         A(L) = ZERO
  110 CONTINUE
      RETURN
C
  200 CONTINUE
      LA=1-INCA
      DO 210 L=1,N
         LA=LA+INCA
         A(LA) = ZERO
  210 CONTINUE
      RETURN
      END
C*MODULE QEIGEN  *DECK DQDIAG
      SUBROUTINE DQDIAG(A,VEC,EIG,LDVEC,N,IPREC)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(NDQ=2)
C
      DIMENSION A(*),VEC(LDVEC,N),EIG(N)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /FMCOM / X(1)
C
C     DIAGONALISE A DOUBLE PRECISION MATRIX WITHIN QUADRUPLE PRECISION
C     THE MATRIX A OF SIZE N IS STORED IN TRIANGULAR MODE.
C     IPREC=0: A IS STORED AS DOUBLE PRECISION
C     IPREC=1: A IS STORED AS QUADRUPLE PRECISION
C
      N1=(N*N+N)/2
      CALL VALFM(LOADFM)
      LQA=LOADFM+1
      LQVEC=LQA+N1*NDQ
      LQEIG=LQVEC+N*N*NDQ
      LJBIG=LQEIG+N*NDQ
      LBIG=LJBIG+(N-1)/NWDVAR+1
      LAST=LBIG+N*NDQ
      NEED=LAST-LOADFM-1
      CALL GETFM(NEED)
      IF(IPREC.EQ.0) THEN
        CALL DQCOPY(N1,A,1,X(LQA),1)
        CALL QJACDG(X(LQA),X(LQVEC),X(LQEIG),X(LJBIG),X(LBIG),N,N)
      ELSE
C       CALL QDCOPY(N1,A,1,X(LQA),1)
        CALL QJACDG(A,X(LQVEC),X(LQEIG),X(LJBIG),X(LBIG),N,N)
      ENDIF
      CALL QDCOPY(N,X(LQEIG),1,EIG,1)
      DO I=1,N
        CALL QDCOPY(N,X(LQVEC+(I-1)*N*NDQ),1,VEC(1,I),1)
      ENDDO
      CALL RETFM(NEED)
      RETURN
      END
C*MODULE QEIGEN  *DECK DQCOPY
      SUBROUTINE DQCOPY(N,DX,INCX,DY,INCY)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION DY
      DIMENSION DX(*),DY(*)
C
C     COPIES A VECTOR FROM DOUBLE TO QUADRUPLE PRECISION.
C           DY(I) <== DX(I)
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DY(IY) = DX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,7)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DY(I) = DX(I)
   30 CONTINUE
      IF( N .LT. 7 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,7
        DY(I) = DX(I)
        DY(I + 1) = DX(I + 1)
        DY(I + 2) = DX(I + 2)
        DY(I + 3) = DX(I + 3)
        DY(I + 4) = DX(I + 4)
        DY(I + 5) = DX(I + 5)
        DY(I + 6) = DX(I + 6)
   50 CONTINUE
      RETURN
      END
C*MODULE QEIGEN  *DECK QDCOPY
      SUBROUTINE QDCOPY(N,DX,INCX,DY,INCY)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION DX
      DIMENSION DX(*),DY(*)
C
C     COPIES A VECTOR FROM DOUBLE TO QUADRUPLE PRECISION.
C           DY(I) <== DX(I)
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DY(IY) = DX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,7)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DY(I) = DX(I)
   30 CONTINUE
      IF( N .LT. 7 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,7
        DY(I) = DX(I)
        DY(I + 1) = DX(I + 1)
        DY(I + 2) = DX(I + 2)
        DY(I + 3) = DX(I + 3)
        DY(I + 4) = DX(I + 4)
        DY(I + 5) = DX(I + 5)
        DY(I + 6) = DX(I + 6)
   50 CONTINUE
      RETURN
      END
C*MODULE QEIGEN  *DECK IQTEST
      SUBROUTINE IQTEST(SOME)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION QEPSLON,QE
      LOGICAL SOME,GOPARR,DSKWRK,MASWRK
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      E=EPSLON(1.0D+00)
      QE=QEPSLON(1.0D+00)
      IF(SOME) WRITE(IW,9100) E,QE
      IF(E.EQ.QE) THEN
         IF(MASWRK) WRITE(IW,9000)
         CALL ABRT
      ENDIF
      RETURN
 9000 FORMAT(/1X,'128 BIT PRECISION APPEARS TO BE UNAVAILABLE!')
 9100 FORMAT(1X,'64 BIT EPSILON:',E9.2,' AND 128 BIT EPSILON:',E9.2/)
      END
C*MODULE INT1    *DECK QSANDT
      SUBROUTINE QSANDT(SS,TT,S,T,LL2,SOME)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION MOROKM
C
      LOGICAL SOME,IANDJ,NORM,DOUBLE,GOPARR,DSKWRK,MASWRK
      DOUBLE PRECISION S(LL2),T(LL2),SBLK(225),TBLK(225),
     *        FT(225),DIJ(225),
     *        XIN(125),YIN(125),ZIN(125),
     *        XINT,YINT,ZINT,TAA,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,
     *        CSI,CPI,CDI,CFI,CGI,CSJ,CPJ,CDJ,CFJ,CGJ,
     *        QTOL,RR,ARRI,AXI,AYI,AZI,AI,AJ,AA,AA1,DUM,DUM1,DUM2,T1,T2,
     *        YZ,AX,AY,AZ,FAC,ZERO,PT5,ONE,TWO,THREE,FIVE,SEVEN,NINE,
     *        ELEVEN,SQRT3,SQRT5,SQRT7,RLN10
C
      DIMENSION IJX(225),IJY(225),IJZ(225),IX(35),IY(35),IZ(35),
     *          JX(35),JY(35),JZ(35),SS(*),TT(*)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500, NDQ=2)
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
      COMMON /QSTV  / XINT,YINT,ZINT,TAA,X0,Y0,Z0,
     *                XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
      COMMON /SYMIND/ TOL,II,JJ,LIT,LJT,MINI,MINJ,MAXI,MAXJ,IANDJ
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      PARAMETER (ZERO=0.0D+00, PT5=0.5D+00, ONE=1.0D+00, TWO=2.0D+00,
     *           THREE=3.0D+00, FIVE=5.0D+00, SEVEN=7.0D+00,
     *           NINE=9.0D+00, ELEVEN=11.0D+00)
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
      DATA MOROKM/8HMOROKUMA/,NONE/4HNONE/
#else
      CHARACTER*8 :: MOROKM_STR
      EQUIVALENCE (MOROKM, MOROKM_STR)
      CHARACTER*4 :: NONE_STR
      EQUIVALENCE (NONE, NONE_STR)
      DATA MOROKM_STR/"MOROKUMA"/,NONE_STR/"NONE"/
#endif
C
C     ----- COMPUTE CONVENTIONAL S, AND T INTEGRALS -----
C             ----- IN QUADRUPLE PRECISION -----
C     HERMIT COEFFICIENTS ARE ROUNDED TO DOUBLE PRECISION, HOWEVER,
C     THE LOSS OF ACCURACY TO LARGE EXTENT COMES FROM MULTIPLYING
C     LARGE EXPONENTS IN THIS ROUTINE AND WE GET FULL ACCURACY HERE.
C
C     PI212=SQRT(ONE/ACOS(ZERO))
      SQRT3=SQRT(THREE)
      SQRT5=SQRT(FIVE)
      SQRT7=SQRT(SEVEN)
      RLN10=LOG(NINE+ONE)
C     UNCON=RMETHOD.NE.ANONE.AND.MOD(MODQR,2).EQ.1
      QTOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C     ----- MOPAC INTEGRALS ARE DONE ELSEWHERE -----
C
      IF(MPCTYP.NE.NONE) THEN
C        CALL MPCINT
         CALL ABRT
         RETURN
      END IF
C
      IF (RUNTYP.EQ.MOROKM) THEN
         CALL ABRT
C        CALL STINT1(ISTART,IEND,JSTART,LOCIJ,NATST,NATED,ISAVE,L1,L2)
      ELSE
         ISTART = 1
         IEND   = NSHELL
         JSTART = 1
         LOCIJ  = 0
C        NATST  = 1
C        NATED  = NAT+NCHMAT
C        IF(ISEPS) NATED = NAT+NPS
C        ISAVE  = 0
C        L1 = NUM
C        IF(UNCON) L1=NUMU
C        L2 = (L1*(L1+1))/2
      END IF
C
      CALL QCLR(S,1,LL2)
      CALL QCLR(T,1,LL2)
C
C     ----- INTIALIZE PARALLEL -----
C
C     IPCOUNT = ME - 1
C
C     ----- I SHELL -----
C
      DO 720 II = ISTART,IEND
         I = KATOM(II)
         XI = C(1,I)
         YI = C(2,I)
         ZI = C(3,I)
         I1 = KSTART(II)
         I2 = I1+KNG(II)-1
         LIT = KTYPE(II)
         MINI = KMIN(II)
         MAXI = KMAX(II)
         LOCI = KLOC(II)-MINI-LOCIJ
C
C     ----- J SHELL -----
C
         DO 700 JJ = JSTART,II
C
C     ----- GO PARALLEL! -----
C     PENDING GLOBAL SUM FOR QUADRUPLE PRECISION
C
C           IF (GOPARR) THEN
C              IPCOUNT = IPCOUNT + 1
C              IF (MOD(IPCOUNT,NPROC).NE.0) GO TO 700
C           END IF
            J = KATOM(JJ)
            XJ = C(1,J)
            YJ = C(2,J)
            ZJ = C(3,J)
            J1 = KSTART(JJ)
            J2 = J1+KNG(JJ)-1
            LJT = KTYPE(JJ)
            MINJ = KMIN(JJ)
            MAXJ = KMAX(JJ)
            LOCJ = KLOC(JJ)-MINJ-LOCIJ
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
                  IF (J.LE.1) FT(IJ) = THREE
                  IF ((J.GT.1).AND.(J.LE.4)) FT(IJ) = FIVE
                  IF ((J.GT.4).AND.(J.LE.10)) FT(IJ) = SEVEN
                  IF ((J.GT.10).AND.(J.LE.20)) FT(IJ) = NINE
                  IF (J.GT.20) FT(IJ) = ELEVEN
  140          CONTINUE
  160       CONTINUE
C
            CALL QCLR(SBLK,1,IJ)
            CALL QCLR(TBLK,1,IJ)
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
                  IF (DUM .GT. QTOL) GO TO 500
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
                     IF ((I.EQ. 8).AND.NORM) DUM1=DUM1*SQRT3
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
                  TAA = SQRT(AA1)
                  T1 = -TWO*AJ*AJ*TAA
                  T2 = -PT5*TAA
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
                        CALL QSTVINT
                        XIN(JN) = XINT*TAA
                        YIN(JN) = YINT*TAA
                        ZIN(JN) = ZINT*TAA
                        NJ = J+2
                        CALL QSTVINT
                        XIN(JN+25) = XINT*T1
                        YIN(JN+25) = YINT*T1
                        ZIN(JN+25) = ZINT*T1
                        NJ = J-2
                        IF (NJ .GT. 0) THEN
                           CALL QSTVINT
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
                     DUM1 = (XIN(NX+25)+XIN(NX+50))*YZ+
     *                      (YIN(NY+25)+YIN(NY+50))*XIN(NX)*ZIN(NZ)+
     *                      (ZIN(NZ+25)+ZIN(NZ+50))*XIN(NX)*YIN(NY)
                     SBLK(I) = SBLK(I) + DIJ(I)*DUM
                     TBLK(I) = TBLK(I) + DIJ(I)*(DUM*AJ*FT(I)+DUM1)
  340             CONTINUE
C
C     ----- END OF PRIMITIVE LOOPS -----
C
  500          CONTINUE
  520       CONTINUE
C
C     ----- COPY BLOCK OVERLAP AND KINETIC ENERGY MATRICES
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
                  S(JN) = SBLK(NN)
                  T(JN) = TBLK(NN)
  600          CONTINUE
  620       CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
  700    CONTINUE
  720 CONTINUE
C
C     ----- SUM UP PARTIAL CONTRIBUTIONS IF PARALLEL -----
C
      IF (GOPARR) THEN
C        CALL DDI_GSUMF(911,S,L2)
C        CALL DDI_GSUMF(912,T,L2)
      END IF
C
C     ----- SAVE S, AND T MATRICES ON THE DAF -----
C
      CALL DAWRIT(IDAF,IODA,S,LL2*NDQ,394,0)
      CALL DAWRIT(IDAF,IODA,T,LL2*NDQ,395,0)
C
C     REWRITE THE OVERLAP AND KINETIC INTEGRALS
C
      CALL QDCOPY(LL2,S,1,SS,1)
      CALL QDCOPY(LL2,T,1,TT,1)
      CALL DAWRIT(IDAF,IODA,SS,LL2,12,0)
      CALL DAWRIT(IDAF,IODA,TT,LL2,13,0)
C     IF (ISAVE.EQ.1) THEN
C        CALL DAWRIT(IDAF,IODA,H,LL2,311,0)
C        CALL DAWRIT(IDAF,IODA,S,LL2,312,0)
C     END IF
      IF(SOME) WRITE(6,*)
     *     'QUADRUPLE PRECISION RELATIVISTIC INTEGRALS WILL BE USED.'
      RETURN
      END
C
C*MODULE INT1    *DECK QSTVINT
      SUBROUTINE QSTVINT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION H1,H2,H3,H4,H5,H6,H7,W1,W2,W3,W4,W5,W6,W7,H,W
      INTEGER NI,NJ,I,NPTS,IMIN,IMAX
C
      DIMENSION H(28),W(28),MIN(7),MAX(7)
C
      COMMON /HERMIT/ H1,H2(2),H3(3),H4(4),H5(5),H6(6),H7(7)
      COMMON /WERMIT/ W1,W2(2),W3(3),W4(4),W5(5),W6(6),W7(7)
      COMMON /QSTV  / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
C
      EQUIVALENCE (H(1),H1),(W(1),W1)
C
      PARAMETER (ZERO=0.0D+00)
C
      DATA MIN /1,2,4,7,11,16,22/
      DATA MAX /1,3,6,10,15,21,28/
C
C     ----- GAUSS-HERMITE QUADRATURE USING MINIMUM POINT FORMULA -----
C     HERMIT COEFFICIENTS ARE IN DOUBLE PRECISION!
C
      XINT = ZERO
      YINT = ZERO
      ZINT = ZERO
      NPTS = (NI+NJ-2)/2+1
      IMIN = MIN(NPTS)
      IMAX = MAX(NPTS)
      DO 300 I = IMIN,IMAX
         DUM = W(I)
         PX = DUM
         PY = DUM
         PZ = DUM
         DUM = H(I)*T
         PTX = DUM+X0
         PTY = DUM+Y0
         PTZ = DUM+Z0
         AX = PTX-XI
         AY = PTY-YI
         AZ = PTZ-ZI
         BX = PTX-XJ
         BY = PTY-YJ
         BZ = PTZ-ZJ
         GO TO (160,150,140,130,120,110,100),NI
  100       PX = PX*AX
            PY = PY*AY
            PZ = PZ*AZ
  110       PX = PX*AX
            PY = PY*AY
            PZ = PZ*AZ
  120       PX = PX*AX
            PY = PY*AY
            PZ = PZ*AZ
  130       PX = PX*AX
            PY = PY*AY
            PZ = PZ*AZ
  140       PX = PX*AX
            PY = PY*AY
            PZ = PZ*AZ
  150       PX = PX*AX
            PY = PY*AY
            PZ = PZ*AZ
  160       GO TO (270,260,250,240,230,220,210,200),NJ
  200          PX = PX*BX
               PY = PY*BY
               PZ = PZ*BZ
  210          PX = PX*BX
               PY = PY*BY
               PZ = PZ*BZ
  220          PX = PX*BX
               PY = PY*BY
               PZ = PZ*BZ
  230          PX = PX*BX
               PY = PY*BY
               PZ = PZ*BZ
  240          PX = PX*BX
               PY = PY*BY
               PZ = PZ*BZ
  250          PX = PX*BX
               PY = PY*BY
               PZ = PZ*BZ
  260          PX = PX*BX
               PY = PY*BY
               PZ = PZ*BZ
  270          XINT = XINT+PX
               YINT = YINT+PY
               ZINT = ZINT+PZ
  300 CONTINUE
      RETURN
      END
C*MODULE MTHLIB  *DECK QTFTRI
      SUBROUTINE QTFTRI(H,F,T,WRK,M,N,LDT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION H(*),F(*),T(LDT,M),WRK(N)
C
      PARAMETER (MXROWS=5)
      PARAMETER (ZERO=0.0D+00, SMALL=1.0D-25)
C
C     ----- TRANSFORM THE TRIANGULAR MATRIX F USING VECTORS T -----
C                      H = T-DAGGER * F * T
C     THE ORDER OF THE TRIANGULAR MATRICES H AND F ARE M AND N.
C
C
      IJ = 0
      DO 310 J = 1,M,MXROWS
         JJMAX = MIN(M,J+MXROWS-1)
C
C             FIRST CALCULATE T-DAGGER TIMES -F-, A ROW AT A TIME
C
         DO 300 JJ=J,JJMAX
            IK = 0
            DO 140 I = 1,N
               IM1 = I-1
               DUM = ZERO
               TDUM = T(I,JJ)
               IF (IM1.GT.0) THEN
                  DO 100 K = 1,IM1
                     IK = IK+1
                     WRK(K) = WRK(K)+F(IK)*TDUM
                     DUM = DUM+F(IK)*T(K,JJ)
  100             CONTINUE
               END IF
               IK = IK+1
               WRK(I) = DUM+F(IK)*TDUM
  140       CONTINUE
C
C             THEN TAKE THAT ROW TIMES EVERY COLUMN IN -T-
C
            DO 200 I = 1,JJ
               IJ = IJ+1
               HIJ = QDOT(N,T(1,I),1,WRK,1)
               IF(ABS(HIJ).LT.SMALL) HIJ=ZERO
               H(IJ)=HIJ
  200       CONTINUE
  300    CONTINUE
  310 CONTINUE
C
      RETURN
      END
C*MODULE BLAS1   *DECK QDOT
      DOUBLE PRECISION FUNCTION QDOT(N,DX,INCX,DY,INCY)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION DX(1),DY(1)
C
C     FORMS THE DOT PRODUCT OF TWO VECTORS.
C           DOT = DX(I) * DY(I)
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      QDOT = 0.0D+00
      DTEMP = 0.0D+00
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DTEMP = DTEMP + DX(IX)*DY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      QDOT = DTEMP
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DTEMP = DTEMP + DX(I)*DY(I)
   30 CONTINUE
      IF( N .LT. 5 ) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DTEMP = DTEMP + DX(I)*DY(I) + DX(I + 1)*DY(I + 1) +
     *   DX(I + 2)*DY(I + 2) + DX(I + 3)*DY(I + 3) + DX(I + 4)*DY(I + 4)
   50 CONTINUE
   60 QDOT = DTEMP
      RETURN
      END
