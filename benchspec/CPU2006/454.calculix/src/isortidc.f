*DECK ISORT
      SUBROUTINE ISORTIDC (IX, DY,CY, N, KFLAG)
!
!     modified to sort in addition a double (dy) and char*5 (cy) array!
!
C***BEGIN PROLOGUE  ISORT
C***PURPOSE  Sort an array and optionally make the same interchanges in
C            an auxiliary array.  The array may be sorted in increasing
C            or decreasing order.  A slightly modified QUICKSORT
C            algorithm is used.
C***LIBRARY   SLATEC
C***CATEGORY  N6A2A
C***TYPE      INTEGER (SSORT-S, DSORT-D, ISORT-I)
C***KEYWORDS  SINGLETON QUICKSORT, SORT, SORTING
C***AUTHOR  Jones, R. E., (SNLA)
C           Kahaner, D. K., (NBS)
C           Wisniewski, J. A., (SNLA)
C***DESCRIPTION
C
C   ISORT sorts array IX and optionally makes the same interchanges in
C   array IY.  The array IX may be sorted in increasing order or
C   decreasing order.  A slightly modified quicksort algorithm is used.
C
C   Description of Parameters
C      IX - integer array of values to be sorted
C      IY - integer array to be (optionally) carried along
C      N  - number of values in integer array IX to be sorted
C      KFLAG - control parameter
C            =  2  means sort IX in increasing order and carry IY along.
C            =  1  means sort IX in increasing order (ignoring IY)
C            = -1  means sort IX in decreasing order (ignoring IY)
C            = -2  means sort IX in decreasing order and carry IY along.
C
C***REFERENCES  R. C. Singleton, Algorithm 347, An efficient algorithm
C                 for sorting with minimal storage, Communications of
C                 the ACM, 12, 3 (1969), pp. 185-187.
C***ROUTINES CALLED  XERMSG
C***REVISION HISTORY  (YYMMDD)
C   761118  DATE WRITTEN
C   810801  Modified by David K. Kahaner.
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891009  Removed unreferenced statement labels.  (WRB)
C   891009  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   901012  Declared all variables; changed X,Y to IX,IY. (M. McClain)
C   920501  Reformatted the REFERENCES section.  (DWL, WRB)
C   920519  Clarified error messages.  (DWL)
C   920801  Declarations section rebuilt and code restructured to use
C           IF-THEN-ELSE-ENDIF.  (RWC, WRB)
C***END PROLOGUE  ISORT
C     .. Scalar Arguments ..
      INTEGER KFLAG, N
C     .. Array Arguments ..
      INTEGER IX(2,*)
      real*8 dy(2,*)
      character*5 cy(*)
C     .. Local Scalars ..
      REAL R
      INTEGER I, IJ, J, K, KK, L, M, NN, T, TT
      real*8 tty1,ty1,tty2,ty2,ttx2,tx2
      character*5 uuy,uy
C     .. Local Arrays ..
      INTEGER IL(21), IU(21)
C     .. External Subroutines ..
!      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC ABS, INT
C***FIRST EXECUTABLE STATEMENT  ISORT
      NN = N
      IF (NN .LT. 1) THEN
!         CALL XERMSG ('SLATEC', 'ISORT',
!     +      'The number of values to be sorted is not positive.', 1, 1)
         RETURN
      ENDIF
C
      KK = ABS(KFLAG)
      IF (KK.NE.1 .AND. KK.NE.2) THEN
!         CALL XERMSG ('SLATEC', 'ISORT',
!     +      'The sort control parameter, K, is not 2, 1, -1, or -2.', 2,
!     +      1)
         RETURN
      ENDIF
C
C     Alter array IX to get decreasing order if needed
C
      IF (KFLAG .LE. -1) THEN
         DO 10 I=1,NN
            IX(1,I) = -IX(1,I)
   10    CONTINUE
      ENDIF
C
      IF (KK .EQ. 2) GO TO 100
C
C     Sort IX only
C
      M = 1
      I = 1
      J = NN
      R = 0.375E0
C
   20 IF (I .EQ. J) GO TO 60
      IF (R .LE. 0.5898437E0) THEN
         R = R+3.90625E-2
      ELSE
         R = R-0.21875E0
      ENDIF
C
   30 K = I
C
C     Select a central element of the array and save it in location T
C
      IJ = I + INT((J-I)*R)
      T = IX(1,IJ)
C
C     If first element of array is greater than T, interchange with T
C
      IF (IX(1,I) .GT. T) THEN
         IX(1,IJ) = IX(1,I)
         IX(1,I) = T
         T = IX(1,IJ)
      ENDIF
      L = J
C
C     If last element of array is less than than T, interchange with T
C
      IF (IX(1,J) .LT. T) THEN
         IX(1,IJ) = IX(1,J)
         IX(1,J) = T
         T = IX(1,IJ)
C
C        If first element of array is greater than T, interchange with T
C
         IF (IX(1,I) .GT. T) THEN
            IX(1,IJ) = IX(1,I)
            IX(1,I) = T
            T = IX(1,IJ)
         ENDIF
      ENDIF
C
C     Find an element in the second half of the array which is smaller
C     than T
C
   40 L = L-1
      IF (IX(1,L) .GT. T) GO TO 40
C
C     Find an element in the first half of the array which is greater
C     than T
C
   50 K = K+1
      IF (IX(1,K) .LT. T) GO TO 50
C
C     Interchange these elements
C
      IF (K .LE. L) THEN
         TT = IX(1,L)
         IX(1,L) = IX(1,K)
         IX(1,K) = TT
         GO TO 40
      ENDIF
C
C     Save upper and lower subscripts of the array yet to be sorted
C
      IF (L-I .GT. J-K) THEN
         IL(M) = I
         IU(M) = L
         I = K
         M = M+1
      ELSE
         IL(M) = K
         IU(M) = J
         J = L
         M = M+1
      ENDIF
      GO TO 70
C
C     Begin again on another portion of the unsorted array
C
   60 M = M-1
      IF (M .EQ. 0) GO TO 190
      I = IL(M)
      J = IU(M)
C
   70 IF (J-I .GE. 1) GO TO 30
      IF (I .EQ. 1) GO TO 20
      I = I-1
C
   80 I = I+1
      IF (I .EQ. J) GO TO 60
      T = IX(1,I+1)
      IF (IX(1,I) .LE. T) GO TO 80
      K = I
C
   90 IX(1,K+1) = IX(1,K)
      K = K-1
      IF (T .LT. IX(1,K)) GO TO 90
      IX(1,K+1) = T
      GO TO 80
C
C     Sort IX and carry IY along
C
  100 M = 1
      I = 1
      J = NN
      R = 0.375E0
C
  110 IF (I .EQ. J) GO TO 150
      IF (R .LE. 0.5898437E0) THEN
         R = R+3.90625E-2
      ELSE
         R = R-0.21875E0
      ENDIF
C
  120 K = I
C
C     Select a central element of the array and save it in location T
C
      IJ = I + INT((J-I)*R)
      T = IX(1,IJ)
      TY1 = DY(1,IJ)
      TY2 = DY(2,IJ)
      TX2 = IX(2,IJ)
      uy = cy(ij)
C
C     If first element of array is greater than T, interchange with T
C
      IF (IX(1,I) .GT. T) THEN
         IX(1,IJ) = IX(1,I)
         IX(1,I) = T
         T = IX(1,IJ)
         DY(1,IJ) = DY(1,I)
         DY(2,IJ) = DY(2,I)
         IX(2,IJ) = IX(2,I)
         cy(ij) = cy(i)
         DY(1,I) = TY1
         DY(2,I) = TY2
         IX(2,I) = TX2
         cy(i) = uy
         TY1 = DY(1,IJ)
         TY2 = DY(2,IJ)
         TX2 = IX(2,IJ)
         uy = cy(ij)
      ENDIF
      L = J
C
C     If last element of array is less than T, interchange with T
C
      IF (IX(1,J) .LT. T) THEN
         IX(1,IJ) = IX(1,J)
         IX(1,J) = T
         T = IX(1,IJ)
         DY(1,IJ) = DY(1,J)
         DY(2,IJ) = DY(2,J)
         IX(2,IJ) = IX(2,J)
         cy(ij) = cy(j)
         DY(1,J) = TY1
         DY(2,J) = TY2
         IX(2,J) = TX2
         cy(j) = uy
         TY1 = DY(1,IJ)
         TY2 = DY(2,IJ)
         TX2 = IX(2,IJ)
         uy = cy(ij)
C
C        If first element of array is greater than T, interchange with T
C
         IF (IX(1,I) .GT. T) THEN
            IX(1,IJ) = IX(1,I)
            IX(1,I) = T
            T = IX(1,IJ)
            DY(1,IJ) = DY(1,I)
            DY(2,IJ) = DY(2,I)
            IX(2,IJ) = IX(2,I)
            cy(ij) = cy(i)
            DY(1,I) = TY1
            DY(2,I) = TY2
            IX(2,I) = TX2
            cy(i) = uy
            TY1 = DY(1,IJ)
            TY2 = DY(2,IJ)
            TX2 = IX(2,IJ)
            uy = cy(ij)
         ENDIF
      ENDIF
C
C     Find an element in the second half of the array which is smaller
C     than T
C
  130 L = L-1
      IF (IX(1,L) .GT. T) GO TO 130
C
C     Find an element in the first half of the array which is greater
C     than T
C
  140 K = K+1
      IF (IX(1,K) .LT. T) GO TO 140
C
C     Interchange these elements
C
      IF (K .LE. L) THEN
         TT = IX(1,L)
         IX(1,L) = IX(1,K)
         IX(1,K) = TT
         TTY1 = DY(1,L)
         TTY2 = DY(2,L)
         TTX2 = IX(2,L)
         uuy = cy(l)
         DY(1,L) = DY(1,K)
         DY(2,L) = DY(2,K)
         IX(2,L) = IX(2,K)
         cy(l) = cy(k)
         DY(1,K) = TTY1
         DY(2,K) = TTY2
         IX(2,K) = TTX2
         cy(k) = uuy
         GO TO 130
      ENDIF
C
C     Save upper and lower subscripts of the array yet to be sorted
C
      IF (L-I .GT. J-K) THEN
         IL(M) = I
         IU(M) = L
         I = K
         M = M+1
      ELSE
         IL(M) = K
         IU(M) = J
         J = L
         M = M+1
      ENDIF
      GO TO 160
C
C     Begin again on another portion of the unsorted array
C
  150 M = M-1
      IF (M .EQ. 0) GO TO 190
      I = IL(M)
      J = IU(M)
C
  160 IF (J-I .GE. 1) GO TO 120
      IF (I .EQ. 1) GO TO 110
      I = I-1
C
  170 I = I+1
      IF (I .EQ. J) GO TO 150
      T = IX(1,I+1)
      TY1 = DY(1,I+1)
      TY2 = DY(2,I+1)
      TX2 = IX(2,I+1)
      uy = cy(i+1)
      IF (IX(1,I) .LE. T) GO TO 170
      K = I
C
  180 IX(1,K+1) = IX(1,K)
      DY(1,K+1) = DY(1,K)
      DY(2,K+1) = DY(2,K)
      IX(2,K+1) = IX(2,K)
      cy(k+1) = cy(k)
      K = K-1
      IF (T .LT. IX(1,K)) GO TO 180
      IX(1,K+1) = T
      DY(1,K+1) = TY1
      DY(2,K+1) = TY2
      IX(2,K+1) = TX2
      cy(k+1) = uy
      GO TO 170
C
C     Clean up
C
  190 IF (KFLAG .LE. -1) THEN
         DO 200 I=1,NN
            IX(1,I) = -IX(1,I)
  200    CONTINUE
      ENDIF
      RETURN
      END
