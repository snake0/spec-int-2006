C 26 MAR 02 - MWS - ADDED L2 DGER,DTRMV AND L3 DTRMM,DTRSM ROUTINES
C  1 MAY 00 - MWS - DGEMM: REWRITE USING 2 SUBSCRIPTS
C  6 JUN 99 - MWS - DGEMM: SPECIAL CASE ALPHA=1 BETA=0 CODE ADDED
C 12 NOV 98 - MWS - DGEMV,DGEMM: ERROR CHECKING ON MATRIX SHAPES
C 10 OCT 95 - MWS - INCLUDE A COPY OF DGEMM (LEVEL THREE ROUTINE)
C 10 NOV 94 - MWS - DNRM2: REMOVE FTNCHECK WARNINGS
C 11 JUN 94 - MWS - INCLUDE A COPY OF DGEMV (LEVEL TWO ROUTINE)
C 11 AUG 87 - MWS - SANITIZE FLOATING POINT CONSTANTS IN DNRM2
C 26 MAR 87 - MWS - USE GENERIC SIGN IN DROTG
C 28 NOV 86 - STE - SUPPLY ALL LEVEL ONE BLAS
C  7 JUL 86 - JAB - SANITIZE FLOATING POINT CONSTANTS
C Dec, 2004 - SPEC_CPU: Changed DX(1) -type dummy declarations
C                         to DX(*), to make them more standard compliant.
C
C BASIC LINEAR ALGEBRA SUBPROGRAMS (BLAS) FROM LINPACK  (LEVEL 1)
C
C   THIS MODULE SHOULD BE COMPILED ONLY IF SPECIALLY CODED
C   VERSIONS OF THESE ROUTINES ARE NOT AVAILABLE ON THE TARGET MACHINE
C
C*MODULE BLAS1   *DECK DASUM
      DOUBLE PRECISION FUNCTION DASUM(N,DX,INCX)
C
C     TAKES THE SUM OF THE ABSOLUTE VALUES.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DX(*),DTEMP
      INTEGER I,INCX,M,MP1,N,NINCX
C
      DASUM = 0.0D+00
      DTEMP = 0.0D+00
      IF(N.LE.0) RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
        DTEMP = DTEMP + ABS(DX(I))
   10 CONTINUE
      DASUM = DTEMP
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,6)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DTEMP = DTEMP + ABS(DX(I))
   30 CONTINUE
      IF( N .LT. 6 ) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,6
        DTEMP = DTEMP + ABS(DX(I)) + ABS(DX(I + 1)) + ABS(DX(I + 2))
     *  + ABS(DX(I + 3)) + ABS(DX(I + 4)) + ABS(DX(I + 5))
   50 CONTINUE
   60 DASUM = DTEMP
      RETURN
      END
C*MODULE BLAS1   *DECK DAXPY
      SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION DX(*),DY(*)
C
C     CONSTANT TIMES A VECTOR PLUS A VECTOR.
C           DY(I) = DY(I) + DA * DX(I)
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      IF(N.LE.0) RETURN
      IF (DA .EQ. 0.0D+00) RETURN
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
        DY(IY) = DY(IY) + DA*DX(IX)
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
   20 M = MOD(N,4)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      IF( N .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        DY(I) = DY(I) + DA*DX(I)
        DY(I + 1) = DY(I + 1) + DA*DX(I + 1)
        DY(I + 2) = DY(I + 2) + DA*DX(I + 2)
        DY(I + 3) = DY(I + 3) + DA*DX(I + 3)
   50 CONTINUE
      RETURN
      END
C*MODULE BLAS1   *DECK DCOPY
      SUBROUTINE  DCOPY(N,DX,INCX,DY,INCY)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION DX(*),DY(*)
C
C     COPIES A VECTOR.
C           DY(I) <== DX(I)
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      IF(N.LE.0) RETURN
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
C*MODULE BLAS1   *DECK DDOT
      DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION DX(*),DY(*)
C
C     FORMS THE DOT PRODUCT OF TWO VECTORS.
C           DOT = DX(I) * DY(I)
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DDOT = 0.0D+00
      DTEMP = 0.0D+00
      IF(N.LE.0) RETURN
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
      DDOT = DTEMP
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
   60 DDOT = DTEMP
      RETURN
      END
#if defined(SPEC_CPU)
      DOUBLE PRECISION FUNCTION DNRM2 ( N, DX, INCX)
      INTEGER          NEXT
      DOUBLE PRECISION   DX(*), CUTLO, CUTHI, HITEST, SUM, XMAX,ZERO,ONE
      DATA   ZERO, ONE /0.0D+00, 1.0D+00/
C
C     EUCLIDEAN NORM OF THE N-VECTOR STORED IN DX() WITH STORAGE
C     INCREMENT INCX .
C     IF    N .LE. 0 RETURN WITH RESULT = 0.
C     IF N .GE. 1 THEN INCX MUST BE .GE. 1
C
C           C.L.LAWSON, 1978 JAN 08
C
C     FOUR PHASE METHOD     USING TWO BUILT-IN CONSTANTS THAT ARE
C     HOPEFULLY APPLICABLE TO ALL MACHINES.
C         CUTLO = MAXIMUM OF  SQRT(U/EPS)  OVER ALL KNOWN MACHINES.
C         CUTHI = MINIMUM OF  SQRT(V)      OVER ALL KNOWN MACHINES.
C     WHERE
C         EPS = SMALLEST NO. SUCH THAT EPS + 1. .GT. 1.
C         U   = SMALLEST POSITIVE NO.   (UNDERFLOW LIMIT)
C         V   = LARGEST  NO.            (OVERFLOW  LIMIT)
C
C     BRIEF OUTLINE OF ALGORITHM..
C
C     PHASE 1    SCANS ZERO COMPONENTS.
C     MOVE TO PHASE 2 WHEN A COMPONENT IS NONZERO AND .LE. CUTLO
C     MOVE TO PHASE 3 WHEN A COMPONENT IS .GT. CUTLO
C     MOVE TO PHASE 4 WHEN A COMPONENT IS .GE. CUTHI/M
C     WHERE M = N FOR X() REAL AND M = 2*N FOR COMPLEX.
C
C     VALUES FOR CUTLO AND CUTHI..
C     FROM THE ENVIRONMENTAL PARAMETERS LISTED IN THE IMSL CONVERTER
C     DOCUMENT THE LIMITING VALUES ARE AS FOLLOWS..
C     CUTLO, S.P.   U/EPS = 2**(-102) FOR  HONEYWELL.  CLOSE SECONDS ARE
C                   UNIVAC AND DEC AT 2**(-103)
C                   THUS CUTLO = 2**(-51) = 4.44089E-16
C     CUTHI, S.P.   V = 2**127 FOR UNIVAC, HONEYWELL, AND DEC.
C                   THUS CUTHI = 2**(63.5) = 1.30438E19
C     CUTLO, D.P.   U/EPS = 2**(-67) FOR HONEYWELL AND DEC.
C                   THUS CUTLO = 2**(-33.5) = 8.23181D-11
C     CUTHI, D.P.   SAME AS S.P.  CUTHI = 1.30438D+19
C     DATA CUTLO, CUTHI / 8.232D-11,  1.304D+19 /
C     DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /
      DATA CUTLO, CUTHI / 8.232D-11,  1.304D+19 /
C
      J=0
      IF(N .GT. 0) GO TO 10
         DNRM2  = ZERO
         GO TO 300
C
C old code...
C  10 ASSIGN 30 TO NEXT
C ... replaced by new code
   10 NEXT = 30
C end of replacement
      SUM = ZERO
      NN = N * INCX
C                                                 BEGIN MAIN LOOP
      I = 1
C old code...
C  20    GO TO NEXT,(30, 50, 70, 110)
C ... replaced by new code
   20    IF (NEXT .EQ. 30) THEN
            GOTO 30
         ELSE IF (NEXT .EQ. 50) THEN
            GOTO 50
         ELSE IF (NEXT .EQ. 70) THEN
            GOTO 70
         ELSE IF (NEXT .EQ. 110) THEN
            GOTO 110
         END IF
C end of replacement
   30 IF( ABS(DX(I)) .GT. CUTLO) GO TO 85
C old code...
C     ASSIGN 50 TO NEXT
C ... replaced by new code
      NEXT = 50
C end of replacement
      XMAX = ZERO
C
C                        PHASE 1.  SUM IS ZERO
C
   50 IF( DX(I) .EQ. ZERO) GO TO 200
      IF( ABS(DX(I)) .GT. CUTLO) GO TO 85
C
C                                PREPARE FOR PHASE 2.
C old code...
C     ASSIGN 70 TO NEXT
C ... replaced by new code
      NEXT = 70
C end of replacement
      GO TO 105
C
C                                PREPARE FOR PHASE 4.
C
  100 I = J
C old code...
C     ASSIGN 110 TO NEXT
C ... replaced by new code
      NEXT = 110
C end of replacement
      SUM = (SUM / DX(I)) / DX(I)
  105 XMAX = ABS(DX(I))
      GO TO 115
C
C                   PHASE 2.  SUM IS SMALL.
C                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
C
   70 IF( ABS(DX(I)) .GT. CUTLO ) GO TO 75
C
C                     COMMON CODE FOR PHASES 2 AND 4.
C                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
C
  110 IF( ABS(DX(I)) .LE. XMAX ) GO TO 115
         SUM = ONE + SUM * (XMAX / DX(I))**2
         XMAX = ABS(DX(I))
         GO TO 200
C
  115 SUM = SUM + (DX(I)/XMAX)**2
      GO TO 200
C
C
C                  PREPARE FOR PHASE 3.
C
   75 SUM = (SUM * XMAX) * XMAX
C
C
C     FOR REAL OR D.P. SET HITEST = CUTHI/N
C     FOR COMPLEX      SET HITEST = CUTHI/(2*N)
C
   85 HITEST = CUTHI/N
C
C                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING.
C
      DO 95 J =I,NN,INCX
      IF(ABS(DX(J)) .GE. HITEST) GO TO 100
   95    SUM = SUM + DX(J)**2
      DNRM2 = SQRT( SUM )
      GO TO 300
C
  200 CONTINUE
      I = I + INCX
      IF ( I .LE. NN ) GO TO 20
C
C              END OF MAIN LOOP.
C
C              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.
C
      DNRM2 = XMAX * SQRT(SUM)
  300 CONTINUE
      RETURN
      END
#else
C*MODULE BLAS1   *DECK DNRM2
      DOUBLE PRECISION FUNCTION DNRM2 ( N, DX, INCX)
      INTEGER          NEXT
      DOUBLE PRECISION   DX(*), CUTLO, CUTHI, HITEST, SUM, XMAX,ZERO,ONE
      DATA   ZERO, ONE /0.0D+00, 1.0D+00/
C
C     EUCLIDEAN NORM OF THE N-VECTOR STORED IN DX() WITH STORAGE
C     INCREMENT INCX .
C     IF    N .LE. 0 RETURN WITH RESULT = 0.
C     IF N .GE. 1 THEN INCX MUST BE .GE. 1
C
C           C.L.LAWSON, 1978 JAN 08
C
C     FOUR PHASE METHOD     USING TWO BUILT-IN CONSTANTS THAT ARE
C     HOPEFULLY APPLICABLE TO ALL MACHINES.
C         CUTLO = MAXIMUM OF  SQRT(U/EPS)  OVER ALL KNOWN MACHINES.
C         CUTHI = MINIMUM OF  SQRT(V)      OVER ALL KNOWN MACHINES.
C     WHERE
C         EPS = SMALLEST NO. SUCH THAT EPS + 1. .GT. 1.
C         U   = SMALLEST POSITIVE NO.   (UNDERFLOW LIMIT)
C         V   = LARGEST  NO.            (OVERFLOW  LIMIT)
C
C     BRIEF OUTLINE OF ALGORITHM..
C
C     PHASE 1    SCANS ZERO COMPONENTS.
C     MOVE TO PHASE 2 WHEN A COMPONENT IS NONZERO AND .LE. CUTLO
C     MOVE TO PHASE 3 WHEN A COMPONENT IS .GT. CUTLO
C     MOVE TO PHASE 4 WHEN A COMPONENT IS .GE. CUTHI/M
C     WHERE M = N FOR X() REAL AND M = 2*N FOR COMPLEX.
C
C     VALUES FOR CUTLO AND CUTHI..
C     FROM THE ENVIRONMENTAL PARAMETERS LISTED IN THE IMSL CONVERTER
C     DOCUMENT THE LIMITING VALUES ARE AS FOLLOWS..
C     CUTLO, S.P.   U/EPS = 2**(-102) FOR  HONEYWELL.  CLOSE SECONDS ARE
C                   UNIVAC AND DEC AT 2**(-103)
C                   THUS CUTLO = 2**(-51) = 4.44089E-16
C     CUTHI, S.P.   V = 2**127 FOR UNIVAC, HONEYWELL, AND DEC.
C                   THUS CUTHI = 2**(63.5) = 1.30438E19
C     CUTLO, D.P.   U/EPS = 2**(-67) FOR HONEYWELL AND DEC.
C                   THUS CUTLO = 2**(-33.5) = 8.23181D-11
C     CUTHI, D.P.   SAME AS S.P.  CUTHI = 1.30438D+19
C     DATA CUTLO, CUTHI / 8.232D-11,  1.304D+19 /
C     DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /
      DATA CUTLO, CUTHI / 8.232D-11,  1.304D+19 /
C
      J=0
      IF(N .GT. 0) GO TO 10
         DNRM2  = ZERO
         GO TO 300
C
   10 ASSIGN 30 TO NEXT
      SUM = ZERO
      NN = N * INCX
C                                                 BEGIN MAIN LOOP
      I = 1
   20    GO TO NEXT,(30, 50, 70, 110)
   30 IF( ABS(DX(I)) .GT. CUTLO) GO TO 85
      ASSIGN 50 TO NEXT
      XMAX = ZERO
C
C                        PHASE 1.  SUM IS ZERO
C
   50 IF( DX(I) .EQ. ZERO) GO TO 200
      IF( ABS(DX(I)) .GT. CUTLO) GO TO 85
C
C                                PREPARE FOR PHASE 2.
      ASSIGN 70 TO NEXT
      GO TO 105
C
C                                PREPARE FOR PHASE 4.
C
  100 I = J
      ASSIGN 110 TO NEXT
      SUM = (SUM / DX(I)) / DX(I)
  105 XMAX = ABS(DX(I))
      GO TO 115
C
C                   PHASE 2.  SUM IS SMALL.
C                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
C
   70 IF( ABS(DX(I)) .GT. CUTLO ) GO TO 75
C
C                     COMMON CODE FOR PHASES 2 AND 4.
C                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
C
  110 IF( ABS(DX(I)) .LE. XMAX ) GO TO 115
         SUM = ONE + SUM * (XMAX / DX(I))**2
         XMAX = ABS(DX(I))
         GO TO 200
C
  115 SUM = SUM + (DX(I)/XMAX)**2
      GO TO 200
C
C
C                  PREPARE FOR PHASE 3.
C
   75 SUM = (SUM * XMAX) * XMAX
C
C
C     FOR REAL OR D.P. SET HITEST = CUTHI/N
C     FOR COMPLEX      SET HITEST = CUTHI/(2*N)
C
   85 HITEST = CUTHI/N
C
C                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING.
C
      DO 95 J =I,NN,INCX
      IF(ABS(DX(J)) .GE. HITEST) GO TO 100
   95    SUM = SUM + DX(J)**2
      DNRM2 = SQRT( SUM )
      GO TO 300
C
  200 CONTINUE
      I = I + INCX
      IF ( I .LE. NN ) GO TO 20
C
C              END OF MAIN LOOP.
C
C              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.
C
      DNRM2 = XMAX * SQRT(SUM)
  300 CONTINUE
      RETURN
      END
#endif


C*MODULE BLAS1   *DECK DROT
      SUBROUTINE  DROT (N,DX,INCX,DY,INCY,C,S)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION DX(*),DY(*)
C
C     APPLIES A PLANE ROTATION.
C           DX(I) =  C*DX(I) + S*DY(I)
C           DY(I) = -S*DX(I) + C*DY(I)
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      IF(N.LE.0) RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL
C         TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DTEMP = C*DX(IX) + S*DY(IY)
        DY(IY) = C*DY(IY) - S*DX(IX)
        DX(IX) = DTEMP
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C       CODE FOR BOTH INCREMENTS EQUAL TO 1
C
   20 DO 30 I = 1,N
        DTEMP = C*DX(I) + S*DY(I)
        DY(I) = C*DY(I) - S*DX(I)
        DX(I) = DTEMP
   30 CONTINUE
      RETURN
      END
C*MODULE BLAS1   *DECK DROTG
      SUBROUTINE DROTG(DA,DB,C,S)
C
C     CONSTRUCT GIVENS PLANE ROTATION.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DA,DB,C,S,ROE,SCALE,R,Z
      DOUBLE PRECISION ZERO, ONE
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
C
C-----------------------------------------------------------------------
C
C
      ROE = DB
      IF( ABS(DA) .GT. ABS(DB) ) ROE = DA
      SCALE = ABS(DA) + ABS(DB)
      IF( SCALE .NE. ZERO ) GO TO 10
         C = ONE
         S = ZERO
         R = ZERO
         GO TO 20
C
   10 R = SCALE*SQRT((DA/SCALE)**2 + (DB/SCALE)**2)
      R = SIGN(ONE,ROE)*R
      C = DA/R
      S = DB/R
   20 Z = ONE
      IF( ABS(DA) .GT. ABS(DB) ) Z = S
      IF( ABS(DB) .GE. ABS(DA) .AND. C .NE. ZERO ) Z = ONE/C
      DA = R
      DB = Z
      RETURN
      END
C*MODULE BLAS1   *DECK DSCAL
      SUBROUTINE  DSCAL(N,DA,DX,INCX)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION DX(*)
C
C     SCALES A VECTOR BY A CONSTANT.
C           DX(I) = DA * DX(I)
C     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      IF(N.LE.0) RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
        DX(I) = DA*DX(I)
   10 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DX(I) = DA*DX(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DX(I) = DA*DX(I)
        DX(I + 1) = DA*DX(I + 1)
        DX(I + 2) = DA*DX(I + 2)
        DX(I + 3) = DA*DX(I + 3)
        DX(I + 4) = DA*DX(I + 4)
   50 CONTINUE
      RETURN
      END
C*MODULE BLAS1   *DECK DSWAP
      SUBROUTINE  DSWAP (N,DX,INCX,DY,INCY)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION DX(*),DY(*)
C
C     INTERCHANGES TWO VECTORS.
C           DX(I) <==> DY(I)
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      IF(N.LE.0) RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL
C         TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DTEMP = DX(IX)
        DX(IX) = DY(IY)
        DY(IY) = DTEMP
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C       CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C       CLEAN-UP LOOP
C
   20 M = MOD(N,3)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DTEMP = DX(I)
        DX(I) = DY(I)
        DY(I) = DTEMP
   30 CONTINUE
      IF( N .LT. 3 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,3
        DTEMP = DX(I)
        DX(I) = DY(I)
        DY(I) = DTEMP
        DTEMP = DX(I + 1)
        DX(I + 1) = DY(I + 1)
        DY(I + 1) = DTEMP
        DTEMP = DX(I + 2)
        DX(I + 2) = DY(I + 2)
        DY(I + 2) = DTEMP
   50 CONTINUE
      RETURN
      END
C*MODULE BLAS1   *DECK IDAMAX
      INTEGER FUNCTION IDAMAX(N,DX,INCX)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION DX(*)
C
C     FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      IDAMAX = 0
      IF( N .LT. 1 ) RETURN
      IDAMAX = 1
      IF(N.EQ.1) RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      IX = 1
      RMAX = ABS(DX(1))
      IX = IX + INCX
      DO 10 I = 2,N
         IF(ABS(DX(IX)).LE.RMAX) GO TO 5
         IDAMAX = I
         RMAX = ABS(DX(IX))
    5    IX = IX + INCX
   10 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
   20 RMAX = ABS(DX(1))
      DO 30 I = 2,N
         IF(ABS(DX(I)).LE.RMAX) GO TO 30
         IDAMAX = I
         RMAX = ABS(DX(I))
   30 CONTINUE
      RETURN
      END
C*MODULE BLAS    *DECK DGER
      SUBROUTINE DGER  ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
C     .. SCALAR ARGUMENTS ..
      DOUBLE PRECISION   ALPHA
      INTEGER            INCX, INCY, LDA, M, N
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
C     ..
C
C  PURPOSE
C  =======
C
C  DGER   PERFORMS THE RANK 1 OPERATION
C
C     A := ALPHA*X*Y' + A,
C
C  WHERE ALPHA IS A SCALAR, X IS AN M ELEMENT VECTOR, Y IS AN N ELEMENT
C  VECTOR AND A IS AN M BY N MATRIX.
C
C  PARAMETERS
C  ==========
C
C  M      - INTEGER.
C           ON ENTRY, M SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
C           M MUST BE AT LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  N      - INTEGER.
C           ON ENTRY, N SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
C           N MUST BE AT LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  ALPHA  - DOUBLE PRECISION.
C           ON ENTRY, ALPHA SPECIFIES THE SCALAR ALPHA.
C           UNCHANGED ON EXIT.
C
C  X      - DOUBLE PRECISION ARRAY OF DIMENSION AT LEAST
C           ( 1 + ( M - 1 )*ABS( INCX ) ).
C           BEFORE ENTRY, THE INCREMENTED ARRAY X MUST CONTAIN THE M
C           ELEMENT VECTOR X.
C           UNCHANGED ON EXIT.
C
C  INCX   - INTEGER.
C           ON ENTRY, INCX SPECIFIES THE INCREMENT FOR THE ELEMENTS OF
C           X. INCX MUST NOT BE ZERO.
C           UNCHANGED ON EXIT.
C
C  Y      - DOUBLE PRECISION ARRAY OF DIMENSION AT LEAST
C           ( 1 + ( N - 1 )*ABS( INCY ) ).
C           BEFORE ENTRY, THE INCREMENTED ARRAY Y MUST CONTAIN THE N
C           ELEMENT VECTOR Y.
C           UNCHANGED ON EXIT.
C
C  INCY   - INTEGER.
C           ON ENTRY, INCY SPECIFIES THE INCREMENT FOR THE ELEMENTS OF
C           Y. INCY MUST NOT BE ZERO.
C           UNCHANGED ON EXIT.
C
C  A      - DOUBLE PRECISION ARRAY OF DIMENSION ( LDA, N ).
C           BEFORE ENTRY, THE LEADING M BY N PART OF THE ARRAY A MUST
C           CONTAIN THE MATRIX OF COEFFICIENTS. ON EXIT, A IS
C           OVERWRITTEN BY THE UPDATED MATRIX.
C
C  LDA    - INTEGER.
C           ON ENTRY, LDA SPECIFIES THE FIRST DIMENSION OF A AS DECLARED
C           IN THE CALLING (SUB) PROGRAM. LDA MUST BE AT LEAST
C           MAX( 1, M ).
C           UNCHANGED ON EXIT.
C
C
C  LEVEL 2 BLAS ROUTINE.
C
C  -- WRITTEN ON 22-OCTOBER-1986.
C     JACK DONGARRA, ARGONNE NATIONAL LAB.
C     JEREMY DU CROZ, NAG CENTRAL OFFICE.
C     SVEN HAMMARLING, NAG CENTRAL OFFICE.
C     RICHARD HANSON, SANDIA NATIONAL LABS.
C
C
C     .. PARAMETERS ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, J, JY, KX
C     .. EXTERNAL ROUTINES ..
      EXTERNAL           XERBLA
C     .. INTRINSIC FUNCTIONS ..
      INTRINSIC          MAX
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     TEST THE INPUT PARAMETERS.
C
      INFO = 0
      IF     ( M.LT.0 )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGER  ', INFO )
         RETURN
      END IF
C
C     QUICK RETURN IF POSSIBLE.
C
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     $   RETURN
C
C     START THE OPERATIONS. IN THIS VERSION THE ELEMENTS OF A ARE
C     ACCESSED SEQUENTIALLY WITH ONE PASS THROUGH A.
C
      IF( INCY.GT.0 )THEN
         JY = 1
      ELSE
         JY = 1 - ( N - 1 )*INCY
      END IF
      IF( INCX.EQ.1 )THEN
         DO 20, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               DO 10, I = 1, M
                  A( I, J ) = A( I, J ) + X( I )*TEMP
   10          CONTINUE
            END IF
            JY = JY + INCY
   20    CONTINUE
      ELSE
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( M - 1 )*INCX
         END IF
         DO 40, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               IX   = KX
               DO 30, I = 1, M
                  A( I, J ) = A( I, J ) + X( IX )*TEMP
                  IX        = IX        + INCX
   30          CONTINUE
            END IF
            JY = JY + INCY
   40    CONTINUE
      END IF
C
      RETURN
C
C     END OF DGER  .
C
      END
C*MODULE BLAS    *DECK DTRMM
      SUBROUTINE DTRMM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     $                   B, LDB )
C     .. SCALAR ARGUMENTS ..
      CHARACTER*1      SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      DOUBLE PRECISION   ALPHA
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
C     ..
C
C  PURPOSE
C  =======
C
C  DTRMM  PERFORMS ONE OF THE MATRIX-MATRIX OPERATIONS
C
C     B := ALPHA*OP( A )*B,   OR   B := ALPHA*B*OP( A ),
C
C  WHERE  ALPHA  IS A SCALAR,  B  IS AN M BY N MATRIX,  A  IS A UNIT, OR
C  NON-UNIT,  UPPER OR LOWER TRIANGULAR MATRIX  AND  OP( A )  IS ONE  OF
C
C     OP( A ) = A   OR   OP( A ) = A'.
C
C  PARAMETERS
C  ==========
C
C  SIDE   - CHARACTER*1.
C           ON ENTRY,  SIDE SPECIFIES WHETHER  OP( A ) MULTIPLIES B FROM
C           THE LEFT OR RIGHT AS FOLLOWS:
C
C              SIDE = 'L' OR 'L'   B := ALPHA*OP( A )*B.
C
C              SIDE = 'R' OR 'R'   B := ALPHA*B*OP( A ).
C
C           UNCHANGED ON EXIT.
C
C  UPLO   - CHARACTER*1.
C           ON ENTRY, UPLO SPECIFIES WHETHER THE MATRIX A IS AN UPPER OR
C           LOWER TRIANGULAR MATRIX AS FOLLOWS:
C
C              UPLO = 'U' OR 'U'   A IS AN UPPER TRIANGULAR MATRIX.
C
C              UPLO = 'L' OR 'L'   A IS A LOWER TRIANGULAR MATRIX.
C
C           UNCHANGED ON EXIT.
C
C  TRANSA - CHARACTER*1.
C           ON ENTRY, TRANSA SPECIFIES THE FORM OF OP( A ) TO BE USED IN
C           THE MATRIX MULTIPLICATION AS FOLLOWS:
C
C              TRANSA = 'N' OR 'N'   OP( A ) = A.
C
C              TRANSA = 'T' OR 'T'   OP( A ) = A'.
C
C              TRANSA = 'C' OR 'C'   OP( A ) = A'.
C
C           UNCHANGED ON EXIT.
C
C  DIAG   - CHARACTER*1.
C           ON ENTRY, DIAG SPECIFIES WHETHER OR NOT A IS UNIT TRIANGULAR
C           AS FOLLOWS:
C
C              DIAG = 'U' OR 'U'   A IS ASSUMED TO BE UNIT TRIANGULAR.
C
C              DIAG = 'N' OR 'N'   A IS NOT ASSUMED TO BE UNIT
C                                  TRIANGULAR.
C
C           UNCHANGED ON EXIT.
C
C  M      - INTEGER.
C           ON ENTRY, M SPECIFIES THE NUMBER OF ROWS OF B. M MUST BE AT
C           LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  N      - INTEGER.
C           ON ENTRY, N SPECIFIES THE NUMBER OF COLUMNS OF B.  N MUST BE
C           AT LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  ALPHA  - DOUBLE PRECISION.
C           ON ENTRY,  ALPHA SPECIFIES THE SCALAR  ALPHA. WHEN  ALPHA IS
C           ZERO THEN  A IS NOT REFERENCED AND  B NEED NOT BE SET BEFORE
C           ENTRY.
C           UNCHANGED ON EXIT.
C
C  A      - DOUBLE PRECISION ARRAY OF DIMENSION ( LDA, K ), WHERE K IS M
C           WHEN  SIDE = 'L' OR 'L'  AND IS  N  WHEN  SIDE = 'R' OR 'R'.
C           BEFORE ENTRY  WITH  UPLO = 'U' OR 'U',  THE  LEADING  K BY K
C           UPPER TRIANGULAR PART OF THE ARRAY  A MUST CONTAIN THE UPPER
C           TRIANGULAR MATRIX  AND THE STRICTLY LOWER TRIANGULAR PART OF
C           A IS NOT REFERENCED.
C           BEFORE ENTRY  WITH  UPLO = 'L' OR 'L',  THE  LEADING  K BY K
C           LOWER TRIANGULAR PART OF THE ARRAY  A MUST CONTAIN THE LOWER
C           TRIANGULAR MATRIX  AND THE STRICTLY UPPER TRIANGULAR PART OF
C           A IS NOT REFERENCED.
C           NOTE THAT WHEN  DIAG = 'U' OR 'U',  THE DIAGONAL ELEMENTS OF
C           A  ARE NOT REFERENCED EITHER,  BUT ARE ASSUMED TO BE  UNITY.
C           UNCHANGED ON EXIT.
C
C  LDA    - INTEGER.
C           ON ENTRY, LDA SPECIFIES THE FIRST DIMENSION OF A AS DECLARED
C           IN THE CALLING (SUB) PROGRAM.  WHEN  SIDE = 'L' OR 'L'  THEN
C           LDA  MUST BE AT LEAST  MAX( 1, M ),  WHEN  SIDE = 'R' OR 'R'
C           THEN LDA MUST BE AT LEAST MAX( 1, N ).
C           UNCHANGED ON EXIT.
C
C  B      - DOUBLE PRECISION ARRAY OF DIMENSION ( LDB, N ).
C           BEFORE ENTRY,  THE LEADING  M BY N PART OF THE ARRAY  B MUST
C           CONTAIN THE MATRIX  B,  AND  ON EXIT  IS OVERWRITTEN  BY THE
C           TRANSFORMED MATRIX.
C
C  LDB    - INTEGER.
C           ON ENTRY, LDB SPECIFIES THE FIRST DIMENSION OF B AS DECLARED
C           IN  THE  CALLING  (SUB)  PROGRAM.   LDB  MUST  BE  AT  LEAST
C           MAX( 1, M ).
C           UNCHANGED ON EXIT.
C
C
C  LEVEL 3 BLAS ROUTINE.
C
C  -- WRITTEN ON 8-FEBRUARY-1989.
C     JACK DONGARRA, ARGONNE NATIONAL LABORATORY.
C     IAIN DUFF, AERE HARWELL.
C     JEREMY DU CROZ, NUMERICAL ALGORITHMS GROUP LTD.
C     SVEN HAMMARLING, NUMERICAL ALGORITHMS GROUP LTD.
C
C
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. EXTERNAL ROUTINES ..
      EXTERNAL           XERBLA
C     .. INTRINSIC FUNCTIONS ..
      INTRINSIC          MAX
C     .. LOCAL SCALARS ..
      LOGICAL            LSIDE, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      DOUBLE PRECISION   TEMP
C     .. PARAMETERS ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     TEST THE INPUT PARAMETERS.
C
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
C
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.
     $         ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.
     $         ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND.
     $         ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
         INFO = 4
      ELSE IF( M  .LT.0               )THEN
         INFO = 5
      ELSE IF( N  .LT.0               )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DTRMM ', INFO )
         RETURN
      END IF
C
C     QUICK RETURN IF POSSIBLE.
C
      IF( N.EQ.0 )
     $   RETURN
C
C     AND WHEN  ALPHA.EQ.ZERO.
C
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
C
C     START THE OPERATIONS.
C
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
C
C           FORM  B := ALPHA*A*B.
C
            IF( UPPER )THEN
               DO 50, J = 1, N
                  DO 40, K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*B( K, J )
                        DO 30, I = 1, K - 1
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   30                   CONTINUE
                        IF( NOUNIT )
     $                     TEMP = TEMP*A( K, K )
                        B( K, J ) = TEMP
                     END IF
   40             CONTINUE
   50          CONTINUE
            ELSE
               DO 80, J = 1, N
                  DO 70 K = M, 1, -1
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP      = ALPHA*B( K, J )
                        B( K, J ) = TEMP
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )*A( K, K )
                        DO 60, I = K + 1, M
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   60                   CONTINUE
                     END IF
   70             CONTINUE
   80          CONTINUE
            END IF
         ELSE
C
C           FORM  B := ALPHA*B*A'.
C
            IF( UPPER )THEN
               DO 110, J = 1, N
                  DO 100, I = M, 1, -1
                     TEMP = B( I, J )
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( I, I )
                     DO 90, K = 1, I - 1
                        TEMP = TEMP + A( K, I )*B( K, J )
   90                CONTINUE
                     B( I, J ) = ALPHA*TEMP
  100             CONTINUE
  110          CONTINUE
            ELSE
               DO 140, J = 1, N
                  DO 130, I = 1, M
                     TEMP = B( I, J )
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( I, I )
                     DO 120, K = I + 1, M
                        TEMP = TEMP + A( K, I )*B( K, J )
  120                CONTINUE
                     B( I, J ) = ALPHA*TEMP
  130             CONTINUE
  140          CONTINUE
            END IF
         END IF
      ELSE
         IF( LSAME( TRANSA, 'N' ) )THEN
C
C           FORM  B := ALPHA*B*A.
C
            IF( UPPER )THEN
               DO 180, J = N, 1, -1
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 150, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  150             CONTINUE
                  DO 170, K = 1, J - 1
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 160, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  160                   CONTINUE
                     END IF
  170             CONTINUE
  180          CONTINUE
            ELSE
               DO 220, J = 1, N
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 190, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  190             CONTINUE
                  DO 210, K = J + 1, N
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 200, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  200                   CONTINUE
                     END IF
  210             CONTINUE
  220          CONTINUE
            END IF
         ELSE
C
C           FORM  B := ALPHA*B*A'.
C
            IF( UPPER )THEN
               DO 260, K = 1, N
                  DO 240, J = 1, K - 1
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = ALPHA*A( J, K )
                        DO 230, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  230                   CONTINUE
                     END IF
  240             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( K, K )
                  IF( TEMP.NE.ONE )THEN
                     DO 250, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  250                CONTINUE
                  END IF
  260          CONTINUE
            ELSE
               DO 300, K = N, 1, -1
                  DO 280, J = K + 1, N
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = ALPHA*A( J, K )
                        DO 270, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  270                   CONTINUE
                     END IF
  280             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( K, K )
                  IF( TEMP.NE.ONE )THEN
                     DO 290, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  290                CONTINUE
                  END IF
  300          CONTINUE
            END IF
         END IF
      END IF
C
      RETURN
C
C     END OF DTRMM .
C
      END
C*MODULE BLAS    *DECK DTRMV
      SUBROUTINE DTRMV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
C     .. SCALAR ARGUMENTS ..
      INTEGER            INCX, LDA, N
      CHARACTER*1      DIAG, TRANS, UPLO
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION   A( LDA, * ), X( * )
C     ..
C
C  PURPOSE
C  =======
C
C  DTRMV  PERFORMS ONE OF THE MATRIX-VECTOR OPERATIONS
C
C     X := A*X,   OR   X := A'*X,
C
C  WHERE X IS AN N ELEMENT VECTOR AND  A IS AN N BY N UNIT, OR NON-UNIT,
C  UPPER OR LOWER TRIANGULAR MATRIX.
C
C  PARAMETERS
C  ==========
C
C  UPLO   - CHARACTER*1.
C           ON ENTRY, UPLO SPECIFIES WHETHER THE MATRIX IS AN UPPER OR
C           LOWER TRIANGULAR MATRIX AS FOLLOWS:
C
C              UPLO = 'U' OR 'U'   A IS AN UPPER TRIANGULAR MATRIX.
C
C              UPLO = 'L' OR 'L'   A IS A LOWER TRIANGULAR MATRIX.
C
C           UNCHANGED ON EXIT.
C
C  TRANS  - CHARACTER*1.
C           ON ENTRY, TRANS SPECIFIES THE OPERATION TO BE PERFORMED AS
C           FOLLOWS:
C
C              TRANS = 'N' OR 'N'   X := A*X.
C
C              TRANS = 'T' OR 'T'   X := A'*X.
C
C              TRANS = 'C' OR 'C'   X := A'*X.
C
C           UNCHANGED ON EXIT.
C
C  DIAG   - CHARACTER*1.
C           ON ENTRY, DIAG SPECIFIES WHETHER OR NOT A IS UNIT
C           TRIANGULAR AS FOLLOWS:
C
C              DIAG = 'U' OR 'U'   A IS ASSUMED TO BE UNIT TRIANGULAR.
C
C              DIAG = 'N' OR 'N'   A IS NOT ASSUMED TO BE UNIT
C                                  TRIANGULAR.
C
C           UNCHANGED ON EXIT.
C
C  N      - INTEGER.
C           ON ENTRY, N SPECIFIES THE ORDER OF THE MATRIX A.
C           N MUST BE AT LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  A      - DOUBLE PRECISION ARRAY OF DIMENSION ( LDA, N ).
C           BEFORE ENTRY WITH  UPLO = 'U' OR 'U', THE LEADING N BY N
C           UPPER TRIANGULAR PART OF THE ARRAY A MUST CONTAIN THE UPPER
C           TRIANGULAR MATRIX AND THE STRICTLY LOWER TRIANGULAR PART OF
C           A IS NOT REFERENCED.
C           BEFORE ENTRY WITH UPLO = 'L' OR 'L', THE LEADING N BY N
C           LOWER TRIANGULAR PART OF THE ARRAY A MUST CONTAIN THE LOWER
C           TRIANGULAR MATRIX AND THE STRICTLY UPPER TRIANGULAR PART OF
C           A IS NOT REFERENCED.
C           NOTE THAT WHEN  DIAG = 'U' OR 'U', THE DIAGONAL ELEMENTS OF
C           A ARE NOT REFERENCED EITHER, BUT ARE ASSUMED TO BE UNITY.
C           UNCHANGED ON EXIT.
C
C  LDA    - INTEGER.
C           ON ENTRY, LDA SPECIFIES THE FIRST DIMENSION OF A AS DECLARED
C           IN THE CALLING (SUB) PROGRAM. LDA MUST BE AT LEAST
C           MAX( 1, N ).
C           UNCHANGED ON EXIT.
C
C  X      - DOUBLE PRECISION ARRAY OF DIMENSION AT LEAST
C           ( 1 + ( N - 1 )*ABS( INCX ) ).
C           BEFORE ENTRY, THE INCREMENTED ARRAY X MUST CONTAIN THE N
C           ELEMENT VECTOR X. ON EXIT, X IS OVERWRITTEN WITH THE
C           TRANFORMED VECTOR X.
C
C  INCX   - INTEGER.
C           ON ENTRY, INCX SPECIFIES THE INCREMENT FOR THE ELEMENTS OF
C           X. INCX MUST NOT BE ZERO.
C           UNCHANGED ON EXIT.
C
C
C  LEVEL 2 BLAS ROUTINE.
C
C  -- WRITTEN ON 22-OCTOBER-1986.
C     JACK DONGARRA, ARGONNE NATIONAL LAB.
C     JEREMY DU CROZ, NAG CENTRAL OFFICE.
C     SVEN HAMMARLING, NAG CENTRAL OFFICE.
C     RICHARD HANSON, SANDIA NATIONAL LABS.
C
C
C     .. PARAMETERS ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, J, JX, KX
      LOGICAL            NOUNIT
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. EXTERNAL ROUTINES ..
      EXTERNAL           XERBLA
C     .. INTRINSIC FUNCTIONS ..
      INTRINSIC          MAX
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     TEST THE INPUT PARAMETERS.
C
      INFO = 0
      IF     ( .NOT.LSAME( UPLO , 'U' ).AND.
     $         .NOT.LSAME( UPLO , 'L' )      )THEN
         INFO = 1
      ELSE IF( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 2
      ELSE IF( .NOT.LSAME( DIAG , 'U' ).AND.
     $         .NOT.LSAME( DIAG , 'N' )      )THEN
         INFO = 3
      ELSE IF( N.LT.0 )THEN
         INFO = 4
      ELSE IF( LDA.LT.MAX( 1, N ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DTRMV ', INFO )
         RETURN
      END IF
C
C     QUICK RETURN IF POSSIBLE.
C
      IF( N.EQ.0 )
     $   RETURN
C
      NOUNIT = LSAME( DIAG, 'N' )
C
C     SET UP THE START POINT IN X IF THE INCREMENT IS NOT UNITY. THIS
C     WILL BE  ( N - 1 )*INCX  TOO SMALL FOR DESCENDING LOOPS.
C
      IF( INCX.LE.0 )THEN
         KX = 1 - ( N - 1 )*INCX
      ELSE IF( INCX.NE.1 )THEN
         KX = 1
      END IF
C
C     START THE OPERATIONS. IN THIS VERSION THE ELEMENTS OF A ARE
C     ACCESSED SEQUENTIALLY WITH ONE PASS THROUGH A.
C
      IF( LSAME( TRANS, 'N' ) )THEN
C
C        FORM  X := A*X.
C
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 20, J = 1, N
                  IF( X( J ).NE.ZERO )THEN
                     TEMP = X( J )
                     DO 10, I = 1, J - 1
                        X( I ) = X( I ) + TEMP*A( I, J )
   10                CONTINUE
                     IF( NOUNIT )
     $                  X( J ) = X( J )*A( J, J )
                  END IF
   20          CONTINUE
            ELSE
               JX = KX
               DO 40, J = 1, N
                  IF( X( JX ).NE.ZERO )THEN
                     TEMP = X( JX )
                     IX   = KX
                     DO 30, I = 1, J - 1
                        X( IX ) = X( IX ) + TEMP*A( I, J )
                        IX      = IX      + INCX
   30                CONTINUE
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )*A( J, J )
                  END IF
                  JX = JX + INCX
   40          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 60, J = N, 1, -1
                  IF( X( J ).NE.ZERO )THEN
                     TEMP = X( J )
                     DO 50, I = N, J + 1, -1
                        X( I ) = X( I ) + TEMP*A( I, J )
   50                CONTINUE
                     IF( NOUNIT )
     $                  X( J ) = X( J )*A( J, J )
                  END IF
   60          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 80, J = N, 1, -1
                  IF( X( JX ).NE.ZERO )THEN
                     TEMP = X( JX )
                     IX   = KX
                     DO 70, I = N, J + 1, -1
                        X( IX ) = X( IX ) + TEMP*A( I, J )
                        IX      = IX      - INCX
   70                CONTINUE
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )*A( J, J )
                  END IF
                  JX = JX - INCX
   80          CONTINUE
            END IF
         END IF
      ELSE
C
C        FORM  X := A'*X.
C
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 100, J = N, 1, -1
                  TEMP = X( J )
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 90, I = J - 1, 1, -1
                     TEMP = TEMP + A( I, J )*X( I )
   90             CONTINUE
                  X( J ) = TEMP
  100          CONTINUE
            ELSE
               JX = KX + ( N - 1 )*INCX
               DO 120, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 110, I = J - 1, 1, -1
                     IX   = IX   - INCX
                     TEMP = TEMP + A( I, J )*X( IX )
  110             CONTINUE
                  X( JX ) = TEMP
                  JX      = JX   - INCX
  120          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 140, J = 1, N
                  TEMP = X( J )
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 130, I = J + 1, N
                     TEMP = TEMP + A( I, J )*X( I )
  130             CONTINUE
                  X( J ) = TEMP
  140          CONTINUE
            ELSE
               JX = KX
               DO 160, J = 1, N
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 150, I = J + 1, N
                     IX   = IX   + INCX
                     TEMP = TEMP + A( I, J )*X( IX )
  150             CONTINUE
                  X( JX ) = TEMP
                  JX      = JX   + INCX
  160          CONTINUE
            END IF
         END IF
      END IF
C
      RETURN
C
C     END OF DTRMV .
C
      END
C*MODULE BLAS    *DECK DTRSM
      SUBROUTINE DTRSM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     $                   B, LDB )
C     .. SCALAR ARGUMENTS ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      DOUBLE PRECISION   ALPHA
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
C     ..
C
C  PURPOSE
C  =======
C
C  DTRSM SOLVES ONE OF THE MATRIX EQUATIONS
C
C     OP( A )*X = ALPHA*B,   OR   X*OP( A ) = ALPHA*B,
C
C  WHERE ALPHA IS A SCALAR, X AND B ARE M BY N MATRICES, A IS A UNIT, OR
C  NON-UNIT,  UPPER OR LOWER TRIANGULAR MATRIX  AND  OP( A )  IS ONE  OF
C
C     OP( A ) = A   OR   OP( A ) = A'.
C
C  THE MATRIX X IS OVERWRITTEN ON B.
C
C  PARAMETERS
C  ==========
C
C  SIDE   - CHARACTER*1.
C           ON ENTRY, SIDE SPECIFIES WHETHER OP( A ) APPEARS ON THE LEFT
C           OR RIGHT OF X AS FOLLOWS:
C
C              SIDE = 'L' OR 'L'   OP( A )*X = ALPHA*B.
C
C              SIDE = 'R' OR 'R'   X*OP( A ) = ALPHA*B.
C
C           UNCHANGED ON EXIT.
C
C  UPLO   - CHARACTER*1.
C           ON ENTRY, UPLO SPECIFIES WHETHER THE MATRIX A IS AN UPPER OR
C           LOWER TRIANGULAR MATRIX AS FOLLOWS:
C
C              UPLO = 'U' OR 'U'   A IS AN UPPER TRIANGULAR MATRIX.
C
C              UPLO = 'L' OR 'L'   A IS A LOWER TRIANGULAR MATRIX.
C
C           UNCHANGED ON EXIT.
C
C  TRANSA - CHARACTER*1.
C           ON ENTRY, TRANSA SPECIFIES THE FORM OF OP( A ) TO BE USED IN
C           THE MATRIX MULTIPLICATION AS FOLLOWS:
C
C              TRANSA = 'N' OR 'N'   OP( A ) = A.
C
C              TRANSA = 'T' OR 'T'   OP( A ) = A'.
C
C              TRANSA = 'C' OR 'C'   OP( A ) = A'.
C
C           UNCHANGED ON EXIT.
C
C  DIAG   - CHARACTER*1.
C           ON ENTRY, DIAG SPECIFIES WHETHER OR NOT A IS UNIT TRIANGULAR
C           AS FOLLOWS:
C
C              DIAG = 'U' OR 'U'   A IS ASSUMED TO BE UNIT TRIANGULAR.
C
C              DIAG = 'N' OR 'N'   A IS NOT ASSUMED TO BE UNIT
C                                  TRIANGULAR.
C
C           UNCHANGED ON EXIT.
C
C  M      - INTEGER.
C           ON ENTRY, M SPECIFIES THE NUMBER OF ROWS OF B. M MUST BE AT
C           LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  N      - INTEGER.
C           ON ENTRY, N SPECIFIES THE NUMBER OF COLUMNS OF B.  N MUST BE
C           AT LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  ALPHA  - DOUBLE PRECISION.
C           ON ENTRY,  ALPHA SPECIFIES THE SCALAR  ALPHA. WHEN  ALPHA IS
C           ZERO THEN  A IS NOT REFERENCED AND  B NEED NOT BE SET BEFORE
C           ENTRY.
C           UNCHANGED ON EXIT.
C
C  A      - DOUBLE PRECISION ARRAY OF DIMENSION ( LDA, K ), WHERE K IS M
C           WHEN  SIDE = 'L' OR 'L'  AND IS  N  WHEN  SIDE = 'R' OR 'R'.
C           BEFORE ENTRY  WITH  UPLO = 'U' OR 'U',  THE  LEADING  K BY K
C           UPPER TRIANGULAR PART OF THE ARRAY  A MUST CONTAIN THE UPPER
C           TRIANGULAR MATRIX  AND THE STRICTLY LOWER TRIANGULAR PART OF
C           A IS NOT REFERENCED.
C           BEFORE ENTRY  WITH  UPLO = 'L' OR 'L',  THE  LEADING  K BY K
C           LOWER TRIANGULAR PART OF THE ARRAY  A MUST CONTAIN THE LOWER
C           TRIANGULAR MATRIX  AND THE STRICTLY UPPER TRIANGULAR PART OF
C           A IS NOT REFERENCED.
C           NOTE THAT WHEN  DIAG = 'U' OR 'U',  THE DIAGONAL ELEMENTS OF
C           A  ARE NOT REFERENCED EITHER,  BUT ARE ASSUMED TO BE  UNITY.
C           UNCHANGED ON EXIT.
C
C  LDA    - INTEGER.
C           ON ENTRY, LDA SPECIFIES THE FIRST DIMENSION OF A AS DECLARED
C           IN THE CALLING (SUB) PROGRAM.  WHEN  SIDE = 'L' OR 'L'  THEN
C           LDA  MUST BE AT LEAST  MAX( 1, M ),  WHEN  SIDE = 'R' OR 'R'
C           THEN LDA MUST BE AT LEAST MAX( 1, N ).
C           UNCHANGED ON EXIT.
C
C  B      - DOUBLE PRECISION ARRAY OF DIMENSION ( LDB, N ).
C           BEFORE ENTRY,  THE LEADING  M BY N PART OF THE ARRAY  B MUST
C           CONTAIN  THE  RIGHT-HAND  SIDE  MATRIX  B,  AND  ON EXIT  IS
C           OVERWRITTEN BY THE SOLUTION MATRIX  X.
C
C  LDB    - INTEGER.
C           ON ENTRY, LDB SPECIFIES THE FIRST DIMENSION OF B AS DECLARED
C           IN  THE  CALLING  (SUB)  PROGRAM.   LDB  MUST  BE  AT  LEAST
C           MAX( 1, M ).
C           UNCHANGED ON EXIT.
C
C
C  LEVEL 3 BLAS ROUTINE.
C
C
C  -- WRITTEN ON 8-FEBRUARY-1989.
C     JACK DONGARRA, ARGONNE NATIONAL LABORATORY.
C     IAIN DUFF, AERE HARWELL.
C     JEREMY DU CROZ, NUMERICAL ALGORITHMS GROUP LTD.
C     SVEN HAMMARLING, NUMERICAL ALGORITHMS GROUP LTD.
C
C
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. EXTERNAL ROUTINES ..
      EXTERNAL           XERBLA
C     .. INTRINSIC FUNCTIONS ..
      INTRINSIC          MAX
C     .. LOCAL SCALARS ..
      LOGICAL            LSIDE, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      DOUBLE PRECISION   TEMP
C     .. PARAMETERS ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     TEST THE INPUT PARAMETERS.
C
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
C
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.
     $         ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.
     $         ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND.
     $         ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
         INFO = 4
      ELSE IF( M  .LT.0               )THEN
         INFO = 5
      ELSE IF( N  .LT.0               )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DTRSM ', INFO )
         RETURN
      END IF
C
C     QUICK RETURN IF POSSIBLE.
C
      IF( N.EQ.0 )
     $   RETURN
C
C     AND WHEN  ALPHA.EQ.ZERO.
C
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
C
C     START THE OPERATIONS.
C
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
C
C           FORM  B := ALPHA*INV( A )*B.
C
            IF( UPPER )THEN
               DO 60, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 30, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
   30                CONTINUE
                  END IF
                  DO 50, K = M, 1, -1
                     IF( B( K, J ).NE.ZERO )THEN
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )/A( K, K )
                        DO 40, I = 1, K - 1
                           B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
   40                   CONTINUE
                     END IF
   50             CONTINUE
   60          CONTINUE
            ELSE
               DO 100, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 70, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
   70                CONTINUE
                  END IF
                  DO 90 K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )/A( K, K )
                        DO 80, I = K + 1, M
                           B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
   80                   CONTINUE
                     END IF
   90             CONTINUE
  100          CONTINUE
            END IF
         ELSE
C
C           FORM  B := ALPHA*INV( A' )*B.
C
            IF( UPPER )THEN
               DO 130, J = 1, N
                  DO 120, I = 1, M
                     TEMP = ALPHA*B( I, J )
                     DO 110, K = 1, I - 1
                        TEMP = TEMP - A( K, I )*B( K, J )
  110                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( I, I )
                     B( I, J ) = TEMP
  120             CONTINUE
  130          CONTINUE
            ELSE
               DO 160, J = 1, N
                  DO 150, I = M, 1, -1
                     TEMP = ALPHA*B( I, J )
                     DO 140, K = I + 1, M
                        TEMP = TEMP - A( K, I )*B( K, J )
  140                CONTINUE
                     IF( NOUNIT )
     $                  TEMP = TEMP/A( I, I )
                     B( I, J ) = TEMP
  150             CONTINUE
  160          CONTINUE
            END IF
         END IF
      ELSE
         IF( LSAME( TRANSA, 'N' ) )THEN
C
C           FORM  B := ALPHA*B*INV( A ).
C
            IF( UPPER )THEN
               DO 210, J = 1, N
                  IF( ALPHA.NE.ONE )THEN
                     DO 170, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
  170                CONTINUE
                  END IF
                  DO 190, K = 1, J - 1
                     IF( A( K, J ).NE.ZERO )THEN
                        DO 180, I = 1, M
                           B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
  180                   CONTINUE
                     END IF
  190             CONTINUE
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( J, J )
                     DO 200, I = 1, M
                        B( I, J ) = TEMP*B( I, J )
  200                CONTINUE
                  END IF
  210          CONTINUE
            ELSE
               DO 260, J = N, 1, -1
                  IF( ALPHA.NE.ONE )THEN
                     DO 220, I = 1, M
                        B( I, J ) = ALPHA*B( I, J )
  220                CONTINUE
                  END IF
                  DO 240, K = J + 1, N
                     IF( A( K, J ).NE.ZERO )THEN
                        DO 230, I = 1, M
                           B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
  230                   CONTINUE
                     END IF
  240             CONTINUE
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( J, J )
                     DO 250, I = 1, M
                       B( I, J ) = TEMP*B( I, J )
  250                CONTINUE
                  END IF
  260          CONTINUE
            END IF
         ELSE
C
C           FORM  B := ALPHA*B*INV( A' ).
C
            IF( UPPER )THEN
               DO 310, K = N, 1, -1
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( K, K )
                     DO 270, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  270                CONTINUE
                  END IF
                  DO 290, J = 1, K - 1
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = A( J, K )
                        DO 280, I = 1, M
                           B( I, J ) = B( I, J ) - TEMP*B( I, K )
  280                   CONTINUE
                     END IF
  290             CONTINUE
                  IF( ALPHA.NE.ONE )THEN
                     DO 300, I = 1, M
                        B( I, K ) = ALPHA*B( I, K )
  300                CONTINUE
                  END IF
  310          CONTINUE
            ELSE
               DO 360, K = 1, N
                  IF( NOUNIT )THEN
                     TEMP = ONE/A( K, K )
                     DO 320, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  320                CONTINUE
                  END IF
                  DO 340, J = K + 1, N
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = A( J, K )
                        DO 330, I = 1, M
                           B( I, J ) = B( I, J ) - TEMP*B( I, K )
  330                   CONTINUE
                     END IF
  340             CONTINUE
                  IF( ALPHA.NE.ONE )THEN
                     DO 350, I = 1, M
                        B( I, K ) = ALPHA*B( I, K )
  350                CONTINUE
                  END IF
  360          CONTINUE
            END IF
         END IF
      END IF
C
      RETURN
C
C     END OF DTRSM .
C
      END
C*MODULE BLAS    *DECK DGEMV
      SUBROUTINE DGEMV(FORMA,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*1 FORMA
      DIMENSION A(LDA,*),X(*),Y(*)
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
C
C        CLONE OF -DGEMV- WRITTEN BY MIKE SCHMIDT
C
      LOCY = 1
C
C                  Y = ALPHA * A * X + BETA * Y
C
      IF(FORMA.EQ.'N') THEN
         IF(ALPHA.EQ.ONE  .AND.  BETA.EQ.ZERO) THEN
            DO 110 I=1,M
               Y(LOCY) =       DDOT(N,A(I,1),LDA,X,INCX)
               LOCY = LOCY+INCY
  110       CONTINUE
         ELSE
            DO 120 I=1,M
               Y(LOCY) = ALPHA*DDOT(N,A(I,1),LDA,X,INCX) + BETA*Y(LOCY)
               LOCY = LOCY+INCY
  120       CONTINUE
         END IF
         RETURN
      END IF
C
C                  Y = ALPHA * A-TRANSPOSE * X + BETA * Y
C
      IF(FORMA.EQ.'T') THEN
         IF(ALPHA.EQ.ONE  .AND.  BETA.EQ.ZERO) THEN
            DO 210 I=1,N
               Y(LOCY) =       DDOT(M,A(1,I),1,X,INCX)
               LOCY = LOCY+INCY
  210       CONTINUE
         ELSE
            DO 220 I=1,N
               Y(LOCY) = ALPHA*DDOT(M,A(1,I),1,X,INCX) + BETA*Y(LOCY)
               LOCY = LOCY+INCY
  220       CONTINUE
         END IF
         RETURN
      END IF
C
      WRITE(6,900) FORMA
      CALL ABRT
      RETURN
  900 FORMAT(1X,'ERROR IN -DGEMV- ... UNRECOGNIZED FORMA=',A1)
      END
C*MODULE BLAS    *DECK DGEMM
      SUBROUTINE DGEMM(FORMA,FORMB,L,N,M,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*1 FORMA,FORMB
      DOUBLE PRECISION A(LDA,*), B(LDB,*), C(LDC,*)
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
C
C     THIS IS A PLAIN VANILLA FORTRAN CLONE OF DGEMM
C
      IF (FORMA.EQ.'N' .AND. FORMB.EQ.'N') THEN
         IF(ALPHA.EQ.ONE  .AND.  BETA.EQ.ZERO) THEN
            DO 30 IL = 1, L
               DO 20 IN = 1, N
                  T = ZERO
                  DO 10 IM = 1, M
                     T = T + A(IL,IM)*B(IM,IN)
   10             CONTINUE
                  C(IL,IN) = T
   20          CONTINUE
   30       CONTINUE
         ELSE
            DO 80 IL = 1, L
               DO 70 IN = 1, N
                  T = ZERO
                  DO 60 IM = 1, M
                     T = T + A(IL,IM)*B(IM,IN)
   60             CONTINUE
                  C(IL,IN) = ALPHA*T + BETA*C(IL,IN)
   70          CONTINUE
   80       CONTINUE
         END IF
         RETURN
      END IF
C
      IF (FORMA.EQ.'T' .AND. FORMB.EQ.'N') THEN
         IF(ALPHA.EQ.ONE  .AND.  BETA.EQ.ZERO) THEN
            DO 130 IL = 1, L
               DO 120 IN = 1, N
                  T = ZERO
                  DO 110 IM = 1, M
                     T = T + A(IM,IL)*B(IM,IN)
  110             CONTINUE
                  C(IL,IN) = T
  120          CONTINUE
  130       CONTINUE
         ELSE
            DO 180 IL = 1, L
               DO 170 IN = 1, N
                  T = ZERO
                  DO 160 IM = 1, M
                     T = T + A(IM,IL)*B(IM,IN)
  160             CONTINUE
                  C(IL,IN) = ALPHA*T + BETA*C(IL,IN)
  170          CONTINUE
  180       CONTINUE
         END IF
         RETURN
      END IF
C
      IF (FORMA.EQ.'N' .AND. FORMB.EQ.'T') THEN
         IF(ALPHA.EQ.ONE  .AND.  BETA.EQ.ZERO) THEN
            DO 230 IL = 1, L
               DO 220 IN = 1, N
                  T = ZERO
                  DO 210 IM = 1, M
                     T = T + A(IL,IM)*B(IN,IM)
  210             CONTINUE
                  C(IL,IN) = T
  220          CONTINUE
  230       CONTINUE
         ELSE
            DO 280 IL = 1, L
               DO 270 IN = 1, N
                  T = ZERO
                  DO 260 IM = 1, M
                     T = T + A(IL,IM)*B(IN,IM)
  260             CONTINUE
                  C(IL,IN) = ALPHA*T + BETA*C(IL,IN)
  270          CONTINUE
  280       CONTINUE
         END IF
         RETURN
      END IF
C
      IF (FORMA.EQ.'T' .AND. FORMB.EQ.'T') THEN
         IF(ALPHA.EQ.ONE  .AND.  BETA.EQ.ZERO) THEN
            DO 330 IL = 1, L
               DO 320 IN = 1, N
                  T = ZERO
                  DO 310 IM = 1, M
                     T = T + A(IM,IL)*B(IN,IM)
  310             CONTINUE
                  C(IL,IN) = T
  320          CONTINUE
  330       CONTINUE
         ELSE
            DO 380 IL = 1, L
               DO 370 IN = 1, N
                  T = ZERO
                  DO 360 IM = 1, M
                     T = T + A(IM,IL)*B(IN,IM)
  360             CONTINUE
                  C(IL,IN) = ALPHA*T + BETA*C(IL,IN)
  370          CONTINUE
  380       CONTINUE
         END IF
         RETURN
      END IF
C
      WRITE(6,900) FORMA,FORMB
      CALL ABRT
      RETURN
  900 FORMAT(1X,'ERROR IN -DGEMM- ... ILLEGAL FORMA/FORMB=',A1,1X,A1)
      END
