C  9 DEC 03 - OQ  - SYNCH ARGUMENTS TO DIPDER
C  7 AUG 02 - JMS - DABPAU: CHANGES RELATED TO NEW SP GRADIENTS
C  7 AUG 02 - HT  - USE DYNAMIC MEMORY FOR PAULI REPULSION
C 17 APR 02 - PND - SMALL PRINTING CHANGE
C 24 JAN 02 - MWS - UPDATE CALL TO AOCPHF ROUTINE
C 20 FEB 01 - MWS - PAD PAULMO COMMON
C 26 OCT 00 - MWS - INTRODUCE MXAO PARAMETER
C 28 SEP 97 - MWS - CONVERT PARALLEL CALLS FROM TCGMSG TO DDI
C 24 JUN 97 - MWS - FZMAT: REMOVE UNUSED CALLING ARG TO AOCPHF
C 14 FEB 97 - JHJ - CORRECT TORQUE CONTRIBUTION TO GRADIENT
C 18 DEC 96 - JHJ - NEW MODULE TO COMPUTE EXCHANGE REPULSION ENERGY.
C
C*MODULE EFPAUL  *DECK PAULIR
      SUBROUTINE PAULIR(EEXCH,PROVEC,FOCKMA,SMAT,TMAT,WRK,
     *                  SIJ,TIJ,FASQ,FBSQ,MXBF,MXMO,MXMO2)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      PARAMETER (MXPT=100, MXPSH=5*MXPT, MXPG=5*MXPSH)
      PARAMETER (MXFRG=50, MXFGPT=MXPT*MXFRG)
      PARAMETER (ZERO=0.0D+00)
C
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /PAULIN/ EX(MXPG,MXFRG),CS(MXPG,MXFRG),CP(MXPG,MXFRG),
     *                CD(MXPG,MXFRG),CF(MXPG,MXFRG),CG(MXPG,MXFRG),
     *                PRNAME(MXFGPT),PRCORD(3,MXFGPT),EFZNUC(MXFGPT),
     *                KSTART(MXPSH,MXFRG),KATOM(MXPSH,MXFRG),
     *                KTYPE(MXPSH,MXFRG),KNG(MXPSH,MXFRG),
     *                KLOC(MXPSH,MXFRG), KMIN(MXPSH,MXFRG),
     *                KMAX(MXPSH,MXFRG),NSHELL(MXFRG),NGAUSS(MXFRG),
     *                NAT(MXFRG),NUM(MXFRG),NTPATM
      COMMON /PAULMO/ CENTNM(MXFGPT),CENTCD(3,MXFGPT),NORB(MXFRG),
     *                NPBF(MXFRG),NTMO
C
      DIMENSION SMAT(MXBF,MXBF), TMAT(MXBF,MXBF), WRK(MXBF)
      DIMENSION SIJ(MXMO,MXMO), TIJ(MXMO,MXMO),
     *          FASQ(MXMO,MXMO), FBSQ(MXMO,MXMO)
      DIMENSION PROVEC(MXBF,NTMO), FOCKMA(MXMO2,NFRG)
C
      IF(NTMO.EQ.0) RETURN
      EEXCH = ZERO
      INAT = 1
      IMO = 1
      DO IM = 1,NFRG
         JNAT = 1
         JMO = 1
         DO JM = 1,NFRG
            IF (IM.GE.JM) GO TO 500
            CALL SANDT(1,SMAT,TMAT,MXBF,MXBF,NAT(IM),NAT(JM),
     *                 NGAUSS(IM),NSHELL(IM),EX(1,IM),CS(1,IM),
     *                 CP(1,IM),CD(1,IM),CF(1,IM),CG(1,IM),KSTART(1,IM),  
     *                 KATOM(1,IM),KTYPE(1,IM),KNG(1,IM),KLOC(1,IM),
     *                 KMIN(1,IM),KMAX(1,IM),NGAUSS(JM),NSHELL(JM),
     *                 EX(1,JM),CS(1,JM),CP(1,JM),CD(1,JM),CF(1,JM),
     *                 CG(1,JM),KSTART(1,JM),KATOM(1,JM),KTYPE(1,JM),
     *                 KNG(1,JM),KLOC(1,JM),KMIN(1,JM),KMAX(1,JM),
     *                 PRCORD(1,INAT),PRCORD(1,JNAT))
            CALL VCLR(SIJ,1,MXMO*MXMO)
            CALL VCLR(TIJ,1,MXMO*MXMO)
            CALL TFSQP(SIJ,SMAT,PROVEC(1,JMO),PROVEC(1,IMO),WRK,
     *                 MXBF,MXBF,NORB(JM),NORB(IM),MXMO,MXMO)
            CALL TFSQP(TIJ,TMAT,PROVEC(1,JMO),PROVEC(1,IMO),WRK,
     *                 MXBF,MXBF,NORB(JM),NORB(IM),MXMO,MXMO)
C
C  EEXCH(V;S0)
C 
            CALL VSZERO(SIJ,CENTCD(1,JMO),CENTCD(1,IMO),EEXCH,
     *                  NORB(JM),NORB(IM),MXMO,MXMO)
C
C EEXCH(V;S1)
C
            CALL VSONE(SIJ,TIJ,FOCKMA(1,JM),FOCKMA(1,IM),FASQ,FBSQ,
     *                 EEXCH,NORB(JM),NORB(IM),MXMO,MXMO)
C
C EEXCH(V;S2)
C
            CALL VSTWO(SIJ,PRCORD(1,JNAT),PRCORD(1,INAT),CENTCD(1,JMO),
     *                 CENTCD(1,IMO),EFZNUC(JNAT),EFZNUC(INAT),EEXCH,
     *                 NORB(JM),NORB(IM),NAT(JM),NAT(IM),MXMO,MXMO)
C
  500       CONTINUE
            JMO = JMO + NORB(JM)
            JNAT = JNAT + NAT(JM)
         END DO
         IMO = IMO + NORB(IM)
         INAT = INAT + NAT(IM)
      END DO
      RETURN
      END
C*MODULE EFPAUL  *DECK SANDT
      SUBROUTINE SANDT(MODE,SMATCO,TMATCO,L1,L1CO,NATM,NATMCO,
     *                 NGAU,NSHL,EX,CS,CP,CD,CF,CG,
     *                 KSTART,KATOM,KTYPE,KNG,KLOC,KMIN,KMAX,
     *                 NGAUCO,NSHLCO,EXCO,CSCO,CPCO,CDCO,CFCO,CGCO,
     *                 KSTRCO,KATMCO,KTYPCO,KNGCO,KLOCCO,KMINCO,
     *                 KMAXCO,C,CCO)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL IANDJ,NORM,DOUBLE
C
      DIMENSION EX(NGAU),CS(NGAU),CP(NGAU),CD(NGAU),
     *          CF(NGAU),CG(NGAU),KSTART(NSHL),KATOM(NSHL),
     *          KTYPE(NSHL),KNG(NSHL),KLOC(NSHL),KMIN(NSHL),KMAX(NSHL),
     *          EXCO(NGAUCO),CSCO(NGAUCO),CPCO(NGAUCO),CDCO(NGAUCO),
     *          CFCO(NGAUCO),CGCO(NGAUCO),KSTRCO(NSHLCO),KATMCO(NSHLCO),
     *          KTYPCO(NSHLCO),KNGCO(NSHLCO),KLOCCO(NSHLCO),
     *          KMINCO(NSHLCO),KMAXCO(NSHLCO),SMATCO(L1CO,L1),
     *          C(3,NATM),CCO(3,NATMCO),TMATCO(L1CO,L1)
      DIMENSION DIJ(225),XIN(125),YIN(125),ZIN(125),FT(225),
     *          IX(35),IY(35),IZ(35),JX(35),JY(35),JZ(35),
     *          S(225),IJX(225),IJY(225),IJZ(225),TBLK(225)
C
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
      COMMON /SYMIND/ TOL,II,JJ,LIT,LJT,MINI,MINJ,MAXI,MAXJ,IANDJ
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00,
     *           SQRT3=1.73205080756888D+00,
     *           SQRT5=2.23606797749979D+00, SQRT7=2.64575131106459D+00,
     *           RLN10=2.30258D+00)
      PARAMETER (TWO=2.0D+00, PT5=ONE/TWO, THREE=3.0D+00, FIVE=5.0D+00)
      PARAMETER (SEVEN=7.0D+00, ANINE=9.0D+00, ELEVEN=11.0D+00)
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
C
C     ----- CALCULATE OVERLAP INTEGRALS -----
C     MODE=0 - SYMMETRIC OVERLAP MATRIX FOR ONE BASIS SET, USING
C              THE DATA IN NSHL,EX,...  ALL OF THE CALLING
C              ARGUMENTS NAMED *CO ARE UNUSED IN THIS CASE.
C     MODE=1 - RECTANGULAR OVERLAP MATRIX BETWEEN TWO BASIS SETS.
C              ONLY INTER-BASIS OVERLAPS ARE COMPUTED, FOR THE
C              TWO BASES GIVEN BY NSHL,EX,... AND NSHLCO,EXCO,...
C
      TOL = RLN10*ITOL
      NORM = NORMF.NE.1 .OR. NORMP.NE.1
      CSI = ZERO
      CPI = ZERO
      CDI = ZERO
      CFI = ZERO
      CGI = ZERO
      CSJ = ZERO
      CPJ = ZERO
      CDJ = ZERO
      CFJ = ZERO
      CGJ = ZERO
      CALL VCLR(SMATCO,1,L1CO*L1)
      CALL VCLR(TMATCO,1,L1CO*L1)
C
C     ----- I SHELL
C
      IF(MODE.EQ.0) THEN
         IIMAX = NSHL
      ELSE
         IIMAX = NSHLCO
      END IF
C
      DO 720 II = 1,IIMAX
         IF(MODE.EQ.0) THEN
            I = KATOM(II)
            XI = C(1,I)
            YI = C(2,I)
            ZI = C(3,I)
            I1 = KSTART(II)
            I2 = I1+KNG(II)-1
            LIT = KTYPE(II)
            MINI = KMIN(II)
            MAXI = KMAX(II)
            LOCI = KLOC(II)-MINI
         ELSE
            I = KATMCO(II)
            XI = CCO(1,I)
            YI = CCO(2,I)
            ZI = CCO(3,I)
            I1 = KSTRCO(II)
            I2 = I1+KNGCO(II)-1
            LIT = KTYPCO(II)
            MINI = KMINCO(II)
            MAXI = KMAXCO(II)
            LOCI = KLOCCO(II)-MINI
         END IF
C
C     ----- J SHELL
C
         IF(MODE.EQ.0) THEN
            JJMAX = II
         ELSE
            JJMAX = NSHL
         END IF
C
         DO 700 JJ = 1,JJMAX
            J = KATOM(JJ)
            XJ = C(1,J)
            YJ = C(2,J)
            ZJ = C(3,J)
            J1 = KSTART(JJ)
            J2 = J1+KNG(JJ)-1
            LJT = KTYPE(JJ)
            MINJ = KMIN(JJ)
            MAXJ = KMAX(JJ)
            LOCJ = KLOC(JJ)-MINJ
            NROOTS = (LIT+LJT-2)/2+1
            RR = (XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
            IF(MODE.EQ.0) THEN
               IANDJ = II.EQ.JJ
            ELSE
               IANDJ = .FALSE.
            END IF
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
                  IF ((J.GT.10).AND.(J.LE.20)) FT(IJ) = ANINE
                  IF (J.GT.20) FT(IJ) = ELEVEN
  140          CONTINUE
  160       CONTINUE
            DO 180 I = 1,IJ
               S(I) = ZERO
               TBLK(I) = ZERO
  180       CONTINUE
C
C     ----- I PRIMITIVE
C
            JGMAX = J2
            DO 520 IG = I1,I2
               IF(MODE.EQ.0) THEN
                  IF(LIT.LE.2) CSI = CS(IG)
                  IF(LIT.LE.2) CPI = CP(IG)
                  IF(LIT.EQ.3) CDI = CD(IG)
                  IF(LIT.EQ.4) CFI = CF(IG)
                  IF(LIT.EQ.5) CGI = CG(IG)
                  AI = EX(IG)
               ELSE
                  IF(LIT.LE.2) CSI = CSCO(IG)
                  IF(LIT.LE.2) CPI = CPCO(IG)
                  IF(LIT.EQ.3) CDI = CDCO(IG)
                  IF(LIT.EQ.4) CFI = CFCO(IG)
                  IF(LIT.EQ.5) CGI = CGCO(IG)
                  AI = EXCO(IG)
               END IF
               ARRI = AI*RR
               AXI = AI*XI
               AYI = AI*YI
               AZI = AI*ZI
C
C     ----- J PRIMITIVE
C
               IF (IANDJ) JGMAX = IG
               DO 500 JG = J1,JGMAX
                  AJ = EX(JG)
                  AA = AI+AJ
                  AA1 = ONE/AA
                  DUM = AJ*ARRI*AA1
                  IF (DUM .GT. TOL) GO TO 500
                  FAC = EXP(-DUM)
                  IF(LJT.LE.2) CSJ = CS(JG)
                  IF(LJT.LE.2) CPJ = CP(JG)
                  IF(LJT.EQ.3) CDJ = CD(JG)
                  IF(LJT.EQ.4) CFJ = CF(JG)
                  IF(LJT.EQ.5) CGJ = CG(JG)
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
                     IF ((I.EQ.8).AND.NORM) DUM1=DUM1*SQRT3
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
C      ----- DO OVERLAP INTEGRAL -----
C
                  T = SQRT(AA1)
                  T1 = -TWO*AJ*AJ*T
                  T2 = -PT5*T
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
                        CALL STVINT
                        XIN(JN) = XINT*T
                        YIN(JN) = YINT*T
                        ZIN(JN) = ZINT*T
                        NJ = J+2
                        CALL STVINT
                        XIN(JN+25) = XINT*T1
                        YIN(JN+25) = YINT*T1
                        ZIN(JN+25) = ZINT*T1
                        NJ = J-2
                        IF (NJ .GT. 0) THEN
                           CALL STVINT
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
                     DUM = XIN(NX)*YIN(NY)*ZIN(NZ)
                     DUM1 = (XIN(NX+25)+XIN(NX+50))*YIN(NY)*ZIN(NZ)+
     *                      (YIN(NY+25)+YIN(NY+50))*XIN(NX)*ZIN(NZ)+
     *                      (ZIN(NZ+25)+ZIN(NZ+50))*XIN(NX)*YIN(NY)
                     S(I) = S(I)+DIJ(I)*DUM
                     TBLK(I) = TBLK(I) + DIJ(I)*(DUM*AJ*FT(I)+DUM1)
  340             CONTINUE
C
C     ----- END PRIMITIVES -----
C
  500          CONTINUE
  520       CONTINUE
C
C     ----- STORE THE OVERLAPS FOR THESE SHELLS -----
C
            MAX = MAXJ
            NN = 0
            DO 620 I = MINI,MAXI
               LI = LOCI+I
               IN = (LI*LI-LI)/2
               IF (IANDJ) MAX = I
               DO 600 J = MINJ,MAX
                  NN = NN+1
                  IF(MODE.EQ.0) THEN
                     JN = IN+LOCJ+J
C                    SMAT(JN) = S(NN)
                  ELSE
                     LJ = LOCJ + J
                     SMATCO(LI,LJ) = S(NN)
                     TMATCO(LI,LJ) = TBLK(NN)
                  END IF
  600          CONTINUE
  620       CONTINUE
C
C     ----- END SHELLS -----
C
  700    CONTINUE
  720 CONTINUE
      RETURN
      END
C*MODULE EFPAUL  *DECK TFSQP
      SUBROUTINE TFSQP(H,F,T1,T2,WRK,N1,N2,M1,M2,MD1,MD2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION H(MD1,MD2),F(N1,N2),T1(N1,M1),T2(N2,M2),WRK(N2)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,NXT,PARR
C
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXSEQ=150, MXROWS=5)
C
C     ----- TRANSFORM THE SQUARE MATRIX F USING VECTORS T -----
C                      H = T1-DAGGER * F * T2
C     THE ORDER OF THE SQUARE MATRICES H, F, AND T IS N.
C
C     ----- INITIALIZATION FOR PARALLEL WORK -----
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT  = -1
      L2CNT = -1
      PARR = GOPARR  .AND.  N2.GT.MXSEQ
C
      DO 310 I = 1,M1,MXROWS
         IIMAX = MIN(M1,I+MXROWS-1)
C
C     ----- GO PARALLEL! -----
C     TO DECREASE NEXT VALUE OVERHEAD, WE DO -MXROWS- AT A TIME
C
         IF(PARR) THEN
            IF (NXT) THEN
               L2CNT = L2CNT + 1
               IF (L2CNT.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
               IF (NEXT.NE.L2CNT) THEN
                  DO 010 II=I,IIMAX
                     CALL VCLR(H(II,1),M2,M2)
  010             CONTINUE
                  GO TO 310
               END IF
            ELSE
               IPCOUNT = IPCOUNT + 1
               IF (MOD(IPCOUNT,NPROC).NE.0) THEN
                  DO 020 II=I,IIMAX
                     CALL VCLR(H(II,1),M2,M2)
  020             CONTINUE
                  GO TO 310
               END IF
            END IF
         END IF
C
         DO 300 II=I,IIMAX
            DO 100 L=1,N2
               WRK(L) = DDOT(N1,T1(1,II),1,F(1,L),1)
  100       CONTINUE
            DO 200 J=1,M2
               H(II,J) = DDOT(N2,WRK,1,T2(1,J),1)
  200       CONTINUE
  300    CONTINUE
  310 CONTINUE
C
      IF(PARR) THEN
         CALL DDI_GSUMF(515,H,M1*M2)
         IF(NXT) CALL DDI_DLBRESET
      END IF
C
      RETURN
      END
C*MODULE EFPAUL  *DECK VSZERO
      SUBROUTINE VSZERO(SIJ,CA,CB,EEXCH,NORBA,NORBB,MXMOA,MXMOB)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (TWO=2.0D+00, TM6=1.0D-06, PI=3.1415927D+00)
C
      DIMENSION SIJ(MXMOA,MXMOB), CA(3,NORBA), CB(3,NORBB)
C
      DO I = 1,NORBA
         DO J = 1,NORBB
            X = CA(1,I) - CB(1,J)
            Y = CA(2,I) - CB(2,J)
            Z = CA(3,I) - CB(3,J)
            RIJ = SQRT(X*X + Y*Y + Z*Z)
            IF(ABS(SIJ(I,J)) .LE. TM6)  GO TO 500
            A = TWO*SQRT(-TWO*LOG(ABS(SIJ(I,J)))/PI) 
            EEXCH = EEXCH - TWO*A*SIJ(I,J)*SIJ(I,J)/RIJ
  500    END DO
      END DO
C
      RETURN
      END
C*MODULE EFPAUL  *DECK VSTWO
      SUBROUTINE VSTWO(SIJ,CA,CB,CLMOA,CLMOB,ZA,ZB,EEXCH,NORBA,NORBB,
     *                 NATA,NATB,MXMOA,MXMOB)  
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, TWO=2.0D+00)
C
      DIMENSION SIJ(MXMOA,MXMOB), CA(3,NATA), CB(3,NATB)
      DIMENSION CLMOA(3,NORBA), CLMOB(3,NORBB), ZA(NATA), ZB(NATB)
C
      SVT= ZERO
      DO I = 1,NORBA
         XI = CLMOA(1,I)
         YI = CLMOA(2,I)
         ZI = CLMOA(3,I)
         DO J = 1,NORBB
            XJ = CLMOB(1,J)
            YJ = CLMOB(2,J)
            ZJ = CLMOB(3,J)
            SUMJJ = ZERO
            DO JJ = 1,NATB
               XJJ = XI - CB(1,JJ)
               YJJ = YI - CB(2,JJ)
               ZJJ = ZI - CB(3,JJ)
               RIJJ = SQRT(XJJ*XJJ + YJJ*YJJ + ZJJ*ZJJ)
               SUMJJ = SUMJJ - ZB(JJ)/RIJJ
            END DO
            SUML = ZERO
            DO L = 1,NORBB
               XL = XI - CLMOB(1,L)
               YL = YI - CLMOB(2,L)
               ZL = ZI - CLMOB(3,L)
               RIL = SQRT(XL*XL + YL*YL + ZL*ZL)
               SUML = SUML + TWO/RIL
            END DO
            SUMII = ZERO
            DO II = 1,NATA
               XII = XJ - CA(1,II)
               YII = YJ - CA(2,II)
               ZII = ZJ - CA(3,II)
               RJII = SQRT(XII*XII + YII*YII + ZII*ZII) 
               SUMII = SUMII - ZA(II)/RJII
            END DO
            SUMK = ZERO
            DO K = 1,NORBA
               XK = XJ - CLMOA(1,K)
               YK = YJ - CLMOA(2,K)
               ZK = ZJ - CLMOA(3,K)
               RKJ = SQRT(XK*XK + YK*YK + ZK*ZK)
               SUMK = SUMK + TWO/RKJ
            END DO
            X = XI - XJ
            Y = YI - YJ
            Z = ZI - ZJ
            RIJ = SQRT(X*X + Y*Y + Z*Z)
            EEXCH = EEXCH + TWO*SIJ(I,J)*SIJ(I,J)*
     *                      (SUMJJ+SUML+SUMII+SUMK-ONE/RIJ)
            SVT = SVT + TWO*SIJ(I,J)*SIJ(I,J)*
     *                   (SUMJJ+SUML+SUMII+SUMK-ONE/RIJ)
         END DO
      END DO
C
      RETURN
      END
C*MODULE EFPAUL  *DECK PAULIA
      SUBROUTINE PAULIA(EEXCH)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXPT=100)
      PARAMETER (MXFRG=50, MXFGPT=MXPT*MXFRG)
      PARAMETER (ZERO=0.0D+00, MXATM=500)
C
      COMMON /FMCOM / X(1)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PAULMO/ CENTNM(MXFGPT),CENTCD(3,MXFGPT),NORB(MXFRG),
     *                NPBF(MXFRG),NTMO
      COMMON /PRPOPT/ ILOCAL
C
      EEXCH = ZERO
      IF(NTMO.EQ.0) RETURN
C
      NPRSAV = NPRINT
      NPRINT = -23
C
C    USE BOYS LOCALIZED ORBITALS
C
      CALL DIPINT(ZERO,ZERO,ZERO,.FALSE.)
      ILOCSV = ILOCAL
      ILOCAL = 1
      CALL LMOINP
      CALL LMOX
C
C    ALLOCATE MEMORY
C
      MXMO=0
      MXBF=0
      DO I = 1, NFRG
         MXMO=MAX(MXMO,NORB(I))
         MXBF=MAX(MXBF,NPBF(I))
      END DO
      MXMO2=(MXMO*MXMO+MXMO)/2
      L1 = NUM
      L2 = (L1*L1+L1)/2
      LNA2 = (NA*NA+NA)/2
C
      CALL VALFM(LOADFM)
      LSMAT   = 1       + LOADFM
      LTMAT   = LSMAT   + L1*MXBF
      LSIJ    = LTMAT   + L1*MXBF
      LTIJ    = LSIJ    + NA*MXMO
      LWRK    = LTIJ    + NA*MXMO
      LVEC    = LWRK    + MXBF
      LARRAY  = LVEC    + L1*NA
      LFMO    = LARRAY  + L2
      LCCHG   = LFMO    + LNA2
      LWRK2   = LCCHG   + 3*NA
      LFMOSQ  = LWRK2   + L1
      LFMO2   = LFMOSQ  + NA*NA
      LARAY2  = LFMO2   + MXMO*MXMO
      LPROVEC = LARAY2  + L2
      LFOCKMA = LPROVEC + MXBF*NTMO
      LAST    = LFOCKMA + MXMO2*NFRG
C
      NEED = LAST - LOADFM - 1
      CALL GETFM(NEED)
C
      LENPV=MXBF*NTMO
      LENFM=MXMO2*NFRG
      IF (NFRG.GT.0.AND.NTMO.GT.0) THEN
         CALL DAREAD(IDAF,IODA,X(LPROVEC),LENPV,267,0)
         CALL DAREAD(IDAF,IODA,X(LFOCKMA),LENFM,268,0)
      END IF
C
      CALL REPAUL(X(LSMAT),X(LTMAT),X(LSIJ),X(LTIJ),X(LWRK),X(LVEC),
     *           X(LARRAY),X(LFMO),X(LCCHG),X(LWRK2),X(LFMOSQ),X(LFMO2),  
     *           X(LARAY2),EEXCH,L1,MXBF,NA,LNA2,MXMO,L2,
     *           X(LPROVEC),X(LFOCKMA),MXBF,MXMO2)
C  
      CALL RETFM(NEED)
      NPRINT = NPRSAV
      ILOCAL = ILOCSV 
      IF(MASWRK) WRITE(IW,9000) EEXCH
      RETURN
C
 9000 FORMAT(/1X,'EXCHANGE REPULSION ENERGY (',F20.10,') IS ADDED TO',
     *           ' THE TOTAL ENERGY')
      END
C*MODULE EFPAUL  *DECK REPAUL
      SUBROUTINE REPAUL(SMAT,TMAT,SIJ,TIJ,WRK,VEC,ARRAY,FMO,CCHG,
     *                  WRK2,FMOSQ,FMO2,ARRAY2,EEXCH,L1,L1EF,LNA,LNA2,
     *                  NAEF,L2,PROVEC,FOCKMA,MXBF,MXMO2)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      PARAMETER (MXPT=100, MXPSH=5*MXPT, MXPG=5*MXPSH)
      PARAMETER (MXFRG=50, MXFGPT=MXPT*MXFRG)
      PARAMETER (MXGTOT=5000, MXSH=1000, MXATM=500)
C
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)  
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /PAULIN/EXEF(MXPG,MXFRG),CSEF(MXPG,MXFRG),CPEF(MXPG,MXFRG),  
     *               CDEF(MXPG,MXFRG),CFEF(MXPG,MXFRG),CGEF(MXPG,MXFRG),
     *                PRNAME(MXFGPT),PRCORD(3,MXFGPT),EFZNUC(MXFGPT),
     *                KSTREF(MXPSH,MXFRG),KATMEF(MXPSH,MXFRG),
     *                KTYPEF(MXPSH,MXFRG),KNGEF(MXPSH,MXFRG),
     *                KLOCEF(MXPSH,MXFRG),KMINEF(MXPSH,MXFRG),
     *                KMAXEF(MXPSH,MXFRG),NSHLEF(MXFRG),NGSSEF(MXFRG),
     *                NATEF(MXFRG),NUMEF(MXFRG),NTPATM
      COMMON /PAULMO/ CENTNM(MXFGPT),CENTCD(3,MXFGPT),NORB(MXFRG),
     *                NPBF(MXFRG),NTMO
      COMMON /XYZPRP/ XP,YP,ZP
     *               ,DMX,DMY,DMZ
     *               ,QXX,QYY,QZZ,QXY,QXZ,QYZ
     *               ,QMXX,QMYY,QMZZ,QMXY,QMXZ,QMYZ
     *               ,OXXX,OXXY,OXXZ,OXYY,OYYY,OYYZ
     *               ,OXZZ,OYZZ,OZZZ,OXYZ
     *               ,OMXXX,OMXXY,OMXXZ,OMXYY,OMYYY
     *               ,OMYYZ,OMXZZ,OMYZZ,OMZZZ,OMXYZ
C
      DIMENSION SMAT(L1,L1EF), TMAT(L1,L1EF), SIJ(LNA,NAEF)
      DIMENSION TIJ(LNA,NAEF), WRK(L1EF), VEC(L1,LNA), ARRAY(L2)
      DIMENSION FMO(LNA2), CCHG(3,LNA), WRK2(L1), XYZP(3)
      DIMENSION FMOSQ(LNA,LNA), FMO2(NAEF,NAEF), ARRAY2(L2)
      DIMENSION PROVEC(MXBF,NTMO), FOCKMA(MXMO2,NFRG)
C
C   GET BOYS LMOS
C
      CALL DAREAD(IDAF,IODA,VEC,L1*LNA,71,0)
C
C    FILL XYZP
C
      XYZP(1) = XP
      XYZP(2) = YP
      XYZP(3) = ZP
C
C   CALCULATE CENTROIDS OF CHARGE
C
      DO I = 1,3
         CALL DAREAD(IDAF,IODA,ARRAY,L2,94+I,0)
         DO J = 1,LNA
            CALL TFTRI(CCHG(I,J),ARRAY,VEC(1,J),WRK2,1,L1,L1)
            CCHG(I,J) = CCHG(I,J) + XYZP(I)
         END DO
      END DO
      CALL DAWRIT(IDAF,IODA,CCHG,3*LNA,83,0)
C
C   CALCULATE FOCK MATRIX IN MO BASIS
C
C  F' (=H'+G)
      CALL DAREAD(IDAF,IODA,ARRAY,L2,14,0)
      IF (IEFP.GT.0) THEN
C  G = F' - H'
         CALL DAREAD(IDAF,IODA,ARRAY2,L2,80,0)
         CALL VSUB(ARRAY2,1,ARRAY,1,ARRAY,1,L2)
C  (F0 + HMULT) = G + (H0+HMULT)
         CALL DAREAD(IDAF,IODA,ARRAY2,L2,87,0)
         CALL VADD(ARRAY,1,ARRAY2,1,ARRAY,1,L2)
      END IF
C  F0 = (F0 + HMULT) - HMULT
      CALL DAREAD(IDAF,IODA,ARRAY2,L2,89,0)
      CALL VSUB(ARRAY2,1,ARRAY,1,ARRAY,1,L2)
      CALL TFTRI(FMO,ARRAY,VEC,WRK2,LNA,L1,L1)
      CALL DAWRIT(IDAF,IODA,FMO,LNA2,82,0)
      CALL VCLR(ARRAY,1,L2)
      CALL VCLR(ARRAY2,1,L2)
C
C  CALCULATE QM/MM PAULI REPULSION
C
      INAT = 1
      IMO = 1
      DO IM = 1,NFRG
         CALL SANDT(1,SMAT,TMAT,L1EF,L1,NATEF(IM),NAT,
     *              NGSSEF(IM),NSHLEF(IM),EXEF(1,IM),CSEF(1,IM),
     *              CPEF(1,IM),CDEF(1,IM),CFEF(1,IM),CGEF(1,IM),
     *              KSTREF(1,IM),KATMEF(1,IM),KTYPEF(1,IM),
     *              KNGEF(1,IM),KLOCEF(1,IM),KMINEF(1,IM),KMAXEF(1,IM),  
     *              MXGTOT,NSHELL,EX,CS,CP,CD,CF,CG,KSTART,KATOM,KTYPE,
     *              KNG,KLOC,KMIN,KMAX,PRCORD(1,INAT),C)
         CALL VCLR(SIJ,1,LNA*NAEF)
         CALL VCLR(TIJ,1,LNA*NAEF)
         CALL TFSQP(SIJ,SMAT,VEC,PROVEC(1,IMO),WRK,L1,L1EF,LNA,NAEF,
     *              LNA,NAEF)
         CALL TFSQP(TIJ,TMAT,VEC,PROVEC(1,IMO),WRK,L1,L1EF,LNA,NAEF,
     *              LNA,NAEF)
C
C  EEXCH(V;S0)
C
         CALL VSZERO(SIJ,CCHG,CENTCD(1,IMO),EEXCH,LNA,NORB(IM),LNA,NAEF)   
C
C EEXCH(V;S1)
C
         CALL VSONE(SIJ,TIJ,FMO,FOCKMA(1,IM),FMOSQ,FMO2,EEXCH,
     *              LNA,NORB(IM),LNA,NAEF)
C
C EEXCH(V;S2)
C
         CALL VSTWO(SIJ,C,PRCORD(1,INAT),CCHG,CENTCD(1,IMO),ZAN,
     *              EFZNUC(INAT),EEXCH,LNA,NORB(IM),NAT,NATEF(IM),
     *              LNA,NAEF)  
C
         INAT = INAT + NATEF(IM)
         IMO = IMO + NORB(IM)
      END DO
      RETURN
      END
C*MODULE EFPAUL  *DECK VSONE
      SUBROUTINE VSONE(SIJ,TIJ,FA,FB,FASQ,FBSQ,EEXCH,NORBA,NORBB,
     *                  MOA,MOB)  
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      PARAMETER (ZERO=0.0D+00, TWO=2.0D+00)
C
      DIMENSION SIJ(MOA,MOB), TIJ(MOA,MOB)
      DIMENSION FA((MOA*MOA+MOA)/2), FB((MOB*MOB+MOB)/2)
      DIMENSION FASQ(MOA,MOA), FBSQ(MOB,MOB)
C
      CALL CPYTSQ(FA,FASQ,MOA,1)
      CALL CPYTSQ(FB,FBSQ,MOB,1)
C
      SVO = ZERO
      DO I = 1,NORBA
         DO J = 1,NORBB
            SUMK = ZERO
            DO K = 1,NORBA
               SUMK = SUMK + FASQ(I,K)*SIJ(K,J)
            END DO
            SUML = ZERO
            DO L = 1,NORBB
               SUML = SUML + FBSQ(J,L)*SIJ(I,L)
            END DO
            EEXCH = EEXCH - TWO*SIJ(I,J)*(SUMK+SUML-TWO*TIJ(I,J))
            SVO = SVO - TWO*SIJ(I,J)*(SUMK+SUML-TWO*TIJ(I,J))
         END DO
      END DO
C
      RETURN
      END
C*MODULE EFPAUL  *DECK DPAULR
      SUBROUTINE DPAULR(EF3,NFRPTS,PROVEC,FOCKMA,SMAT,TMAT,EPS,EPT,
     *                  WRK,SIJ,TIJ,FASQ,FBSQ,MXBF,MXMO,MXMO2)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      PARAMETER (MXPT=100, MXPSH=5*MXPT, MXPG=5*MXPSH)
      PARAMETER (MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /PAULIN/ EX(MXPG,MXFRG),CS(MXPG,MXFRG),CP(MXPG,MXFRG),
     *                CD(MXPG,MXFRG),CF(MXPG,MXFRG),CG(MXPG,MXFRG),
     *                PRNAME(MXFGPT),PRCORD(3,MXFGPT),EFZNUC(MXFGPT),
     *                KSTART(MXPSH,MXFRG),KATOM(MXPSH,MXFRG),
     *                KTYPE(MXPSH,MXFRG),KNG(MXPSH,MXFRG),
     *                KLOC(MXPSH,MXFRG), KMIN(MXPSH,MXFRG),
     *                KMAX(MXPSH,MXFRG),NSHELL(MXFRG),NGAUSS(MXFRG),
     *                NAT(MXFRG),NUM(MXFRG),NTPATM
      COMMON /PAULMO/ CENTNM(MXFGPT),CENTCD(3,MXFGPT),NORB(MXFRG),
     *                NPBF(MXFRG),NTMO
C
      DIMENSION EF3(3,NFRPTS)
      DIMENSION SMAT(MXBF,MXBF), TMAT(MXBF,MXBF),
     *          EPS(MXBF,MXBF), EPT(MXBF,MXBF), WRK(MXBF)
      DIMENSION SIJ(MXMO,MXMO), TIJ(MXMO,MXMO),
     *          FASQ(MXMO,MXMO), FBSQ(MXMO,MXMO)
      DIMENSION PROVEC(MXBF,NTMO), FOCKMA(MXMO2,NFRG)
C
      IF(NTMO.EQ.0) RETURN
C
      INAT = 1
      IMO = 1
      IC1 = NMTTPT + NPTTPT 
      IC3 = IC1
      DO IM = 1,NFRG
         IC1 = IC1 + NAT(IM)
         JNAT = 1
         JMO = 1
         IC2 = NMTTPT + NPTTPT
         IC4 = IC2
         DO JM = 1,NFRG
            IC2 = IC2 + NAT(JM)
            IF(IM.GE.JM) GO TO 500
            CALL SANDT(1,SMAT,TMAT,MXBF,MXBF,NAT(IM),NAT(JM),
     *                 NGAUSS(IM),NSHELL(IM),EX(1,IM),CS(1,IM),
     *                 CP(1,IM),CD(1,IM),CF(1,IM),CG(1,IM),KSTART(1,IM),  
     *                 KATOM(1,IM),KTYPE(1,IM),KNG(1,IM),KLOC(1,IM),
     *                 KMIN(1,IM),KMAX(1,IM),NGAUSS(JM),NSHELL(JM),
     *                 EX(1,JM),CS(1,JM),CP(1,JM),CD(1,JM),CF(1,JM),
     *                 CG(1,JM),KSTART(1,JM),KATOM(1,JM),KTYPE(1,JM),
     *                 KNG(1,JM),KLOC(1,JM),KMIN(1,JM),KMAX(1,JM),
     *                 PRCORD(1,INAT),PRCORD(1,JNAT))
            CALL VCLR(SIJ,1,MXMO*MXMO)
            CALL VCLR(TIJ,1,MXMO*MXMO)
            CALL TFSQP(SIJ,SMAT,PROVEC(1,JMO),PROVEC(1,IMO),WRK,
     *                 MXBF,MXBF,NORB(JM),NORB(IM),MXMO,MXMO)
            CALL TFSQP(TIJ,TMAT,PROVEC(1,JMO),PROVEC(1,IMO),WRK,
     *                 MXBF,MXBF,NORB(JM),NORB(IM),MXMO,MXMO)
C
C  DERIVATIVES WITH RESPECT TO CENTROID POINTS
C
C EEXCH(V;S0)
C 
            CALL DVSZIP(SIJ,CENTCD(1,JMO),CENTCD(1,IMO),EF3,
     *                  NORB(JM),NORB(IM),MXMO,MXMO,IC1,IC2)
C
C EEXCH(V;S2)
C
            CALL DVSTWO(SIJ,PRCORD(1,JNAT),PRCORD(1,INAT),CENTCD(1,JMO),
     *                 CENTCD(1,IMO),EFZNUC(JNAT),EFZNUC(INAT),EF3,
     *                 NORB(JM),NORB(IM),NAT(JM),NAT(IM),MXMO,MXMO,
     *                 IC1,IC2)  
C
C  DERIVATIVES WITH RESPECT TO NUCLEAR POINTS
C
C SET UP "OVERLAP DENSITY MATRICES"
C
            CALL ODM(EPS,EPT,SIJ,TIJ,PROVEC(1,JMO),PROVEC(1,IMO),
     *               CENTCD(1,JMO),CENTCD(1,IMO),FOCKMA(1,JM),
     *               FOCKMA(1,IM),FASQ,FBSQ,PRCORD(1,JNAT),
     *               PRCORD(1,INAT),EFZNUC(JNAT),EFZNUC(INAT),NORB(JM),
     *               NORB(IM),NAT(JM),NAT(IM),MXBF,MXBF,MXMO,MXMO,MXMO2,
     *               MXMO2)
C
C  CALCULATE TORQUE
C
            CALL PTORQ(SMAT,TMAT,EPS,EPT,MXBF,MXBF,NSHELL(JM),
     *                 KMIN(1,JM),KMAX(1,JM),KLOC(1,JM),NSHELL(IM),
     *                 KMIN(1,IM),KMAX(1,IM),KLOC(1,IM),JM,IM)
C
C CALCULATE OVERLAP AND KE DERIVATIVES
C
            CALL DSANDT(EF3,EPS,EPT,MXBF,MXBF,NAT(IM),NAT(JM),
     *                 NGAUSS(IM),NSHELL(IM),EX(1,IM),CS(1,IM),
     *                 CP(1,IM),CD(1,IM),CF(1,IM),CG(1,IM),KSTART(1,IM),
     *                 KATOM(1,IM),KTYPE(1,IM),KNG(1,IM),KLOC(1,IM),
     *                 KMIN(1,IM),KMAX(1,IM),NGAUSS(JM),NSHELL(JM),
     *                 EX(1,JM),CS(1,JM),CP(1,JM),CD(1,JM),CF(1,JM),
     *                 CG(1,JM),KSTART(1,JM),KATOM(1,JM),KTYPE(1,JM),
     *                 KNG(1,JM),KLOC(1,JM),KMIN(1,JM),KMAX(1,JM),
     *                 PRCORD(1,INAT),PRCORD(1,JNAT),IC3,IC4,NFRPTS)
C
C CALCULATE REMANINING DERIVATIVE
C
            CALL DZADX(EF3,SIJ,PRCORD(1,JNAT),PRCORD(1,INAT),
     *                 CENTCD(1,JMO),CENTCD(1,IMO),EFZNUC(JNAT),
     *                 EFZNUC(INAT),NORB(JM),NORB(IM),NAT(JM),NAT(IM),
     *                 MXMO,MXMO,IC3,IC4,NFRPTS)
C
  500       CONTINUE
            JMO = JMO + NORB(JM)
            JNAT = JNAT + NAT(JM)
            IC2 = IC2 + NORB(JM) 
            IC4 = IC4 + NAT(JM) + NORB(JM)
         END DO
         IMO = IMO + NORB(IM)
         INAT = INAT + NAT(IM)
         IC1 = IC1 + NORB(IM)
         IC3 = IC3 + NAT(IM) + NORB(IM)
      END DO
C
      RETURN
      END
C*MODULE EFPAUL  *DECK DVSZIP
      SUBROUTINE DVSZIP(SIJ,CA,CB,EF3,NORBA,NORBB,MXMOA,MXMOB,IC1,IC2)  
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (TWO=2.0D+00, TM6=1.0D-06, PI=3.1415927D+00)
C
      DIMENSION SIJ(MXMOA,MXMOB), CA(3,NORBA), CB(3,NORBB)
      DIMENSION EF3(3,*)
C
      DO I = 1,NORBA
         DO J = 1,NORBB
            X = CA(1,I) - CB(1,J)
            Y = CA(2,I) - CB(2,J)
            Z = CA(3,I) - CB(3,J)
            RIJ = SQRT(X*X + Y*Y + Z*Z)
            R3 = RIJ*RIJ*RIJ
            IF(ABS(SIJ(I,J)) .LE. TM6)  GO TO 500
            A = TWO*SQRT(-TWO*LOG(ABS(SIJ(I,J)))/PI) 
            B = TWO*A*SIJ(I,J)*SIJ(I,J)/R3
            IF (IC2.GT.0) THEN
               EF3(1,IC2+I) = EF3(1,IC2+I) + B*X
               EF3(2,IC2+I) = EF3(2,IC2+I) + B*Y
               EF3(3,IC2+I) = EF3(3,IC2+I) + B*Z
            END IF
            EF3(1,IC1+J) = EF3(1,IC1+J) - B*X
            EF3(2,IC1+J) = EF3(2,IC1+J) - B*Y
            EF3(3,IC1+J) = EF3(3,IC1+J) - B*Z
  500    END DO
      END DO
C
      RETURN
      END
C*MODULE EFPAUL  *DECK DVSTWO
      SUBROUTINE DVSTWO(SIJ,CA,CB,CLMOA,CLMOB,ZA,ZB,EF3,NORBA,NORBB,
     *                 NATA,NATB,MXMOA,MXMOB,IC1,IC2)  
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (TWO=2.0D+00, FOUR=4.0D+00)
C
      DIMENSION SIJ(MXMOA,MXMOB), CA(3,NATA), CB(3,NATB)
      DIMENSION CLMOA(3,NORBA), CLMOB(3,NORBB), ZA(NATA), ZB(NATB)
      DIMENSION EF3(3,*)
C
      DO I = 1,NORBA
         XI = CLMOA(1,I)
         YI = CLMOA(2,I)
         ZI = CLMOA(3,I)
         DO J = 1,NORBB
            XJ = CLMOB(1,J)
            YJ = CLMOB(2,J)
            ZJ = CLMOB(3,J)
            S2 = TWO*SIJ(I,J)*SIJ(I,J)
            IF (IC2.EQ.0) GO TO 100
            DO JJ = 1,NATB
               XJJ = XI - CB(1,JJ)
               YJJ = YI - CB(2,JJ)
               ZJJ = ZI - CB(3,JJ)
               RIJJ = SQRT(XJJ*XJJ + YJJ*YJJ + ZJJ*ZJJ)
               R3 = RIJJ*RIJJ*RIJJ
               EF3(1,IC2+I) = EF3(1,IC2+I) + S2*ZB(JJ)*XJJ/R3
               EF3(2,IC2+I) = EF3(2,IC2+I) + S2*ZB(JJ)*YJJ/R3
               EF3(3,IC2+I) = EF3(3,IC2+I) + S2*ZB(JJ)*ZJJ/R3
            END DO
            DO L = 1,NORBB
               XL = XI - CLMOB(1,L)
               YL = YI - CLMOB(2,L)
               ZL = ZI - CLMOB(3,L)
               RIL = SQRT(XL*XL + YL*YL + ZL*ZL)
               R3 = RIL*RIL*RIL
               EF3(1,IC2+I) = EF3(1,IC2+I) - S2*TWO*XL/R3
               EF3(2,IC2+I) = EF3(2,IC2+I) - S2*TWO*YL/R3
               EF3(3,IC2+I) = EF3(3,IC2+I) - S2*TWO*ZL/R3
            END DO
  100       CONTINUE
            DO II = 1,NATA
               XII = XJ - CA(1,II)
               YII = YJ - CA(2,II)
               ZII = ZJ - CA(3,II)
               RJII = SQRT(XII*XII + YII*YII + ZII*ZII)
               R3 = RJII*RJII*RJII
               EF3(1,IC1+J) = EF3(1,IC1+J) + S2*ZA(II)*XII/R3
               EF3(2,IC1+J) = EF3(2,IC1+J) + S2*ZA(II)*YII/R3
               EF3(3,IC1+J) = EF3(3,IC1+J) + S2*ZA(II)*ZII/R3
            END DO
            DO K = 1,NORBA
               XK = XJ - CLMOA(1,K)
               YK = YJ - CLMOA(2,K)
               ZK = ZJ - CLMOA(3,K)
               RKJ = SQRT(XK*XK + YK*YK + ZK*ZK)
               R3 = RKJ*RKJ*RKJ
               EF3(1,IC1+J) = EF3(1,IC1+J) - S2*TWO*XK/R3
               EF3(2,IC1+J) = EF3(2,IC1+J) - S2*TWO*YK/R3
               EF3(3,IC1+J) = EF3(3,IC1+J) - S2*TWO*ZK/R3
C
            END DO
            X = XI - XJ
            Y = YI - YJ
            Z = ZI - ZJ
            RIJ = SQRT(X*X + Y*Y + Z*Z)
            R3 = RIJ*RIJ*RIJ
            IF (IC2.GT.0) THEN
             DO K = 1,NORBA
               EF3(1,IC2+I) = EF3(1,IC2+I)-FOUR*SIJ(K,J)*SIJ(K,J)*X/R3  
               EF3(2,IC2+I) = EF3(2,IC2+I)-FOUR*SIJ(K,J)*SIJ(K,J)*Y/R3  
               EF3(3,IC2+I) = EF3(3,IC2+I)-FOUR*SIJ(K,J)*SIJ(K,J)*Z/R3  
             END DO
            END IF
            DO L = 1,NORBB
               EF3(1,IC1+J) = EF3(1,IC1+J)+FOUR*SIJ(I,L)*SIJ(I,L)*X/R3  
               EF3(2,IC1+J) = EF3(2,IC1+J)+FOUR*SIJ(I,L)*SIJ(I,L)*Y/R3  
               EF3(3,IC1+J) = EF3(3,IC1+J)+FOUR*SIJ(I,L)*SIJ(I,L)*Z/R3  
            END DO
            IF (IC2.GT.0) THEN
               EF3(1,IC2+I) = EF3(1,IC2+I) + S2*X/R3
               EF3(2,IC2+I) = EF3(2,IC2+I) + S2*Y/R3
               EF3(3,IC2+I) = EF3(3,IC2+I) + S2*Z/R3
            END IF
            EF3(1,IC1+J) = EF3(1,IC1+J) - S2*X/R3
            EF3(2,IC1+J) = EF3(2,IC1+J) - S2*Y/R3
            EF3(3,IC1+J) = EF3(3,IC1+J) - S2*Z/R3
         END DO
      END DO
C
      RETURN
      END
C*MODULE EFPAUL  *DECK DSANDT
      SUBROUTINE DSANDT(DE,EPS,EPT,L1,L1CO,NATM,NATMCO,
     *                 NGAU,NSHL,EX,CS,CP,CD,CF,CG,
     *                 KSTART,KATOM,KTYPE,KNG,KLOC,KMIN,KMAX,
     *                 NGAUCO,NSHLCO,EXCO,CSCO,CPCO,CDCO,CFCO,CGCO,
     *                 KSTRCO,KATMCO,KTYPCO,KNGCO,KLOCCO,KMINCO,
     *                 KMAXCO,C,CCO,IC3,IC4,NAT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL NORM
      LOGICAL DBG,OUT
      LOGICAL GOPARR,DSKWRK,MASWRK,NXT
C
      PARAMETER (MXATM=500)
C
      DIMENSION EX(NGAU),CS(NGAU),CP(NGAU),CD(NGAU),
     *          CF(NGAU),CG(NGAU),KSTART(NSHL),KATOM(NSHL),
     *          KTYPE(NSHL),KNG(NSHL),KLOC(NSHL),KMIN(NSHL),KMAX(NSHL),
     *          EXCO(NGAUCO),CSCO(NGAUCO),CPCO(NGAUCO),CDCO(NGAUCO),
     *          CFCO(NGAUCO),CGCO(NGAUCO),KSTRCO(NSHLCO),KATMCO(NSHLCO),
     *          KTYPCO(NSHLCO),KNGCO(NSHLCO),KLOCCO(NSHLCO),
     *          KMINCO(NSHLCO),KMAXCO(NSHLCO),EPS(L1CO,L1),
     *          C(3,NATM),CCO(3,NATMCO),EPT(L1CO,L1),DE(3,NAT)
      DIMENSION DIJ(225), IJX(35), IJY(35),IJZ(35), DIJ2(225),
     *           XS(6,7), YS(6,7), ZS(6,7),XT(6,5), YT(6,5), ZT(6,5),
     *          DXS(5,5),DYS(5,5),DZS(5,5),
     *          DXT(5,5), DYT(5,5), DZT(5,5)
C
      COMMON /DSTV  / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,
     *                XJ,YJ,ZJ,NI,NJ,CX,CY,CZ
      COMMON /GRAD  / ABDE(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
      PARAMETER (SQRT3=1.73205080756888D+00)
      PARAMETER (SQRT5=2.23606797749979D+00, SQRT7=2.64575131106459D+00)
      PARAMETER (ONE=1.0D+00)
      PARAMETER (RLN10=2.30258D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DEBUG/8HDEBUG   /, DBUGME/8HSDER    /, GRD1/8HGRD1    /
#else
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      CHARACTER*8 :: GRD1_STR
      EQUIVALENCE (GRD1, GRD1_STR)
      DATA DEBUG_STR/"DEBUG   "/, DBUGME_STR/"SDER    "/, 
     * GRD1_STR/"GRD1    "/
#endif
C
      DATA IJX / 1, 2, 1, 1, 3, 1, 1, 2, 2, 1,
     *           4, 1, 1, 3, 3, 2, 1, 2, 1, 2,
     *           5, 1, 1, 4, 4, 2, 1, 2, 1, 3,
     *           3, 1, 3, 2, 2/
      DATA IJY / 1, 1, 2, 1, 1, 3, 1, 2, 1, 2,
     *           1, 4, 1, 2, 1, 3, 3, 1, 2, 2,
     *           1, 5, 1, 2, 1, 4, 4, 1, 2, 3,
     *           1, 3, 2, 3, 2/
      DATA IJZ / 1, 1, 1, 2, 1, 1, 3, 1, 2, 2,
     *           1, 1, 4, 1, 2, 1, 2, 3, 3, 2,
     *           1, 1, 5, 1, 2, 1, 2, 4, 4, 1,
     *           3, 3, 2, 2, 3/
C
      DBG = (EXETYP.EQ.DEBUG  .OR.  EXETYP.EQ.DBUGME)  .AND.  MASWRK
      OUT = (EXETYP.EQ.DEBUG  .OR.  EXETYP.EQ.DBUGME  .OR.
     *       EXETYP.EQ.GRD1   .OR.  NPRINT.EQ.-3)
      IF(DBG) WRITE(IW,9000)
C
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C     INITIALIZE PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT = ME - 1
      NEXT = -1
      MINE = -1
C
      IF(DBG) THEN
         WRITE(IW,9010)
         CALL PRTRI(EPS,L1)
      END IF
C
C     ----- I SHELL
C
      DO 780 II = 1,NSHLCO
C
C           GO PARALLEL!
C
      IF(NXT .AND. GOPARR) THEN
         MINE = MINE + 1
         IF(MINE.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
         IF(NEXT.NE.MINE) GO TO 780
      END IF
C
      IAT = KATMCO(II)
      XI = CCO(1,IAT)
      YI = CCO(2,IAT)
      ZI = CCO(3,IAT)
      I1 = KSTRCO(II)
      I2 = I1+KNGCO(II)-1
      LIT = KTYPCO(II)
      MINI = KMINCO(II)
      MAXI = KMAXCO(II)
      LOCI = KLOCCO(II)-MINI
      LITDER = LIT+1
C
C     ----- J SHELL
C
      DO 760 JJ = 1,NSHL
C
C           GO PARALLEL!
C
      IF((.NOT.NXT) .AND. GOPARR) THEN
         IPCOUNT = IPCOUNT + 1
         IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 760
      END IF
C
      JAT = KATOM(JJ)
      XJ = C(1,JAT)
      YJ = C(2,JAT)
      ZJ = C(3,JAT)
      J1 = KSTART(JJ)
      J2 = J1+KNG(JJ)-1
      LJT = KTYPE(JJ)
      MINJ = KMIN(JJ)
      MAXJ = KMAX(JJ)
      LOCJ = KLOC(JJ)-MINJ
      LJTMOD = LJT+2
      RR = (XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
C
C     ----- I PRIMITIVE
C
      DO 640 IG = I1,I2
        AI = EXCO(IG)
        ARRI = AI*RR
        AXI = AI*XI
        AYI = AI*YI
        AZI = AI*ZI
        CSI=CSCO(IG)
        CPI=CPCO(IG)
        CDI=CDCO(IG)
        CFI=CFCO(IG)
        CGI=CGCO(IG)
C
C     ----- J PRIMITIVE
C
        DO 620 JG = J1,J2
          AJ = EX(JG)
          AA = AI+AJ
          AA1 = ONE/AA
          DUM = AJ*ARRI*AA1
          IF(DUM .GT. TOL) GO TO 620
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
          IJ = 0
          DO 580 I = MINI,MAXI
            IF(I.EQ.1) DUM1=CSI*FAC
            IF(I.EQ.2) DUM1=CPI*FAC
            IF(I.EQ.5) DUM1=CDI*FAC
            IF(I.EQ.8.AND.NORM) DUM1=DUM1*SQRT3
            IF(I.EQ.11) DUM1=CFI*FAC
            IF(I.EQ.14.AND.NORM) DUM1=DUM1*SQRT5
            IF(I.EQ.20.AND.NORM) DUM1=DUM1*SQRT3
            IF(I.EQ.21) DUM1=CGI*FAC
            IF(I.EQ.24.AND.NORM) DUM1=DUM1*SQRT7
            IF(I.EQ.30.AND.NORM) DUM1=DUM1*SQRT5/SQRT3
            IF(I.EQ.33.AND.NORM) DUM1=DUM1*SQRT3
            DO 570 J = MINJ,MAXJ
              IF(J.EQ.1) DUM2=DUM1*CSJ
              IF(J.EQ.2) DUM2=DUM1*CPJ
              IF(J.EQ.5) DUM2=DUM1*CDJ
              IF(J.EQ.8.AND.NORM) DUM2=DUM2*SQRT3
              IF(J.EQ.11) DUM2=DUM1*CFJ
              IF(J.EQ.14.AND.NORM) DUM2=DUM2*SQRT5
              IF(J.EQ.20.AND.NORM) DUM2=DUM2*SQRT3
              IF(J.EQ.21) DUM2=DUM1*CGJ
              IF(J.EQ.24.AND.NORM) DUM2=DUM2*SQRT7
              IF(J.EQ.30.AND.NORM) DUM2=DUM2*SQRT5/SQRT3
              IF(J.EQ.33.AND.NORM) DUM2=DUM2*SQRT3
              IJ=IJ+1
              DEN = EPS(LOCI+I,LOCJ+J)
              DIJ(IJ) = DUM2*DEN
C
              DEN2 = EPT(LOCI+I,LOCJ+J)
              DIJ2(IJ)=DUM2*DEN2
  570       CONTINUE
  580     CONTINUE
C
C     ----- OVERLAP
C
      T = SQRT(AA1)
      X0 = AX
      Y0 = AY
      Z0 = AZ
      DO 590 J = 1,LJTMOD
        NJ = J
        DO 590 I = 1,LITDER
          NI = I
          CALL VINT
          XS(I,J)=XINT*T
          YS(I,J)=YINT*T
          ZS(I,J)=ZINT*T
  590 CONTINUE
C
      CALL DTXYZ(XT,YT,ZT,XS,YS,ZS,LITDER,LJT,AJ)
      CALL DERI(DXS,DYS,DZS,XS,YS,ZS,LIT,LJT,AI)
      CALL DERI(DXT,DYT,DZT,XT,YT,ZT,LIT,LJT,AI)
C
      IJ=0
      DO 600 I=MINI,MAXI
        IX=IJX(I)
        IY=IJY(I)
        IZ=IJZ(I)
        DO 600 J=MINJ,MAXJ
          JX=IJX(J)
          JY=IJY(J)
          JZ=IJZ(J)
          DUMX=DXS(IX,JX)* YS(IY,JY)* ZS(IZ,JZ)
          DUMY= XS(IX,JX)*DYS(IY,JY)* ZS(IZ,JZ)
          DUMZ= XS(IX,JX)* YS(IY,JY)*DZS(IZ,JZ)
          DUMX2=DXT(IX,JX)* YS(IY,JY)* ZS(IZ,JZ)
     1         +DXS(IX,JX)* YT(IY,JY)* ZS(IZ,JZ)
     2         +DXS(IX,JX)* YS(IY,JY)* ZT(IZ,JZ)
          DUMY2= XT(IX,JX)*DYS(IY,JY)* ZS(IZ,JZ)
     1         + XS(IX,JX)*DYT(IY,JY)* ZS(IZ,JZ)
     2         + XS(IX,JX)*DYS(IY,JY)* ZT(IZ,JZ)
          DUMZ2= XT(IX,JX)* YS(IY,JY)*DZS(IZ,JZ)
     1         + XS(IX,JX)* YT(IY,JY)*DZS(IZ,JZ)
     2         + XS(IX,JX)* YS(IY,JY)*DZT(IZ,JZ)
C
          IJ=IJ+1
          IF (IC4.EQ.0) THEN
             ABDE(1,IAT)=ABDE(1,IAT)+(DUMX*DIJ(IJ)+DUMX2*DIJ2(IJ))
             ABDE(2,IAT)=ABDE(2,IAT)+(DUMY*DIJ(IJ)+DUMY2*DIJ2(IJ))
             ABDE(3,IAT)=ABDE(3,IAT)+(DUMZ*DIJ(IJ)+DUMZ2*DIJ2(IJ))
          ELSE
             DE(1,IC4+IAT)=DE(1,IC4+IAT)+(DUMX*DIJ(IJ)+DUMX2*DIJ2(IJ))  
             DE(2,IC4+IAT)=DE(2,IC4+IAT)+(DUMY*DIJ(IJ)+DUMY2*DIJ2(IJ))    
             DE(3,IC4+IAT)=DE(3,IC4+IAT)+(DUMZ*DIJ(IJ)+DUMZ2*DIJ2(IJ))
          END IF
          DE(1,IC3+JAT)=DE(1,IC3+JAT)-(DUMX*DIJ(IJ)+DUMX2*DIJ2(IJ)) 
          DE(2,IC3+JAT)=DE(2,IC3+JAT)-(DUMY*DIJ(IJ)+DUMY2*DIJ2(IJ))  
          DE(3,IC3+JAT)=DE(3,IC3+JAT)-(DUMZ*DIJ(IJ)+DUMZ2*DIJ2(IJ))  
  600 CONTINUE
C
  620 CONTINUE
  640 CONTINUE
C
C     ----- END OF PRIMITIVE LOOPS -----
C
      IF(DBG) THEN
         WRITE(IW,9100) II,JJ
         CALL EGOUT(DE,NAT)
      END IF
  760 CONTINUE
  780 CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
      IF(OUT) THEN
         IF(GOPARR) CALL DDI_GSUMF(1501,DE,3*NAT)
         IF(MASWRK) THEN
            WRITE(IW,9000)
            CALL EGOUT(DE,NAT)
         END IF
         IF(GOPARR) CALL DSCAL(3*NAT,ONE/NPROC,DE,1)
      END IF
C
      IF(GOPARR .AND. NXT) CALL DDI_DLBRESET
      RETURN
C
 9000 FORMAT(/10X,33("-")/10X,'GRADIENT INCLUDING DENSITY FORCES'/
     *        10X,33(1H-))
 9010 FORMAT(1X,'THE LAGRANGIAN IN THE AO BASIS IS')
 9100 FORMAT(1X,'SDER: SHELLS II,JJ=',2I5)
      END
C*MODULE EFPAUL  *DECK ODM
      SUBROUTINE ODM(EPS,EPT,SIJ,TIJ,VECA,VECB,CLMOA,CLMOB,FA,FB,
     *               FASQ,FBSQ,CA,CB,ZA,ZB,NORBA,NORBB,NATA,NATB,
     *               MXBFA,MXBFB,MXMOA,MXMOB,MXMO2A,MXMO2B)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (ZERO=0.0D+00,ONE=1.0D+00,FOUR=4.0D+00)
      PARAMETER (TWO=2.0D+00, TM6=1.0D-06, PI=3.1415927D+00)
C
      DIMENSION EPS(MXBFA,MXBFB), EPT(MXBFA,MXBFB), SIJ(MXMOA,MXMOB)
      DIMENSION TIJ(MXMOA,MXMOB), VECA(MXBFA,NORBA), VECB(MXBFB,NORBB)
      DIMENSION CLMOA(3,NORBA), CLMOB(3,NORBB), FA(MXMO2A), FB(MXMO2B)
      DIMENSION FASQ(MXMOA,MXMOA), FBSQ(MXMOB,MXMOB), CA(3,NATA)
      DIMENSION CB(3,NATB), ZA(NATA), ZB(NATB)
C
      CALL VCLR(EPS,1,MXBFA*MXBFB)
      CALL VCLR(EPT,1,MXBFA*MXBFB)
      CALL CPYTSQ(FA,FASQ,MXMOA,1)
      CALL CPYTSQ(FB,FBSQ,MXMOB,1)
C
      DO MU = 1,MXBFA
         DO NU = 1,MXBFB
            DO I = 1,NORBA
               XI = CLMOA(1,I)
               YI = CLMOA(2,I)
               ZI = CLMOA(3,I)
               DO J = 1,NORBB
                  XJ = CLMOB(1,J)
                  YJ = CLMOB(2,J)
                  ZJ = CLMOB(3,J)
                  X = XI - XJ
                  Y = YI - YJ
                  Z = ZI - ZJ
C DE(V;S0)
                  RIJ = SQRT(X*X + Y*Y + Z*Z)
                  IF(ABS(SIJ(I,J)) .LE. TM6)  GO TO 100
                  A = FOUR*SQRT(-TWO*LOG(ABS(SIJ(I,J)))/PI)
                  B = SQRT( -TWO/(PI*LOG(ABS(SIJ(I,J)))) )
                  EPS(MU,NU) = EPS(MU,NU) - TWO*(A-B)*(SIJ(I,J)/RIJ)*
     *                         VECA(MU,I)*VECB(NU,J)
  100             CONTINUE
C DE(V;S1)
C 
                  EPT(MU,NU) = EPT(MU,NU) + FOUR*SIJ(I,J)*VECA(MU,I)*
     *                                      VECB(NU,J)
C
                  SUMK = ZERO
                  DO K = 1,NORBA
                     EPS(MU,NU) = EPS(MU,NU) - TWO*SIJ(I,J)*FASQ(I,K)*  
     *                                         VECA(MU,K)*VECB(NU,J) 
                     SUMK = SUMK + FASQ(I,K)*SIJ(K,J)
                  END DO
                  SUML = ZERO
                  DO L = 1,NORBB
                     EPS(MU,NU) = EPS(MU,NU) - TWO*SIJ(I,J)*FBSQ(J,L)*
     *                                         VECA(MU,I)*VECB(NU,L)
                     SUML = SUML + FBSQ(J,L)*SIJ(I,L)
                  END DO
                  EPS(MU,NU) = EPS(MU,NU) - 
     *                TWO*(SUMK+SUML-TWO*TIJ(I,J))*VECA(MU,I)*VECB(NU,J)  
C DE(V;S2)
                  SUMJJ = ZERO
                  DO JJ = 1,NATB
                     XJJ = XI - CB(1,JJ)
                     YJJ = YI - CB(2,JJ)
                     ZJJ = ZI - CB(3,JJ)
                     RIJJ = SQRT(XJJ*XJJ + YJJ*YJJ + ZJJ*ZJJ)
                     SUMJJ = SUMJJ - ZB(JJ)/RIJJ
                  END DO
                  SUML = ZERO
                  DO L = 1,NORBB
                     XL = XI - CLMOB(1,L)
                     YL = YI - CLMOB(2,L)
                     ZL = ZI - CLMOB(3,L)
                     RIL = SQRT(XL*XL + YL*YL + ZL*ZL)
                     SUML = SUML + TWO/RIL
                  END DO
                  SUMII = ZERO
                  DO II = 1,NATA
                     XII = XJ - CA(1,II)
                     YII = YJ - CA(2,II)
                     ZII = ZJ - CA(3,II)
                     RJII = SQRT(XII*XII + YII*YII + ZII*ZII)
                     SUMII = SUMII - ZA(II)/RJII
                  END DO
                  SUMK = ZERO
                  DO K = 1,NORBA
                     XK = XJ - CLMOA(1,K)
                     YK = YJ - CLMOA(2,K)
                     ZK = ZJ - CLMOA(3,K)
                     RKJ = SQRT(XK*XK + YK*YK + ZK*ZK)
                     SUMK = SUMK + TWO/RKJ
                  END DO
                  EPS(MU,NU) = EPS(MU,NU) + FOUR*SIJ(I,J)*
     *                         (SUMJJ+SUML+SUMII+SUMK-ONE/RIJ)*
     *                         VECA(MU,I)*VECB(NU,J)
C         EPS(MU,NU) = EPS(MU,NU) + TWO*SIJ(I,J)*VECA(MU,I)*VECB(NU,J)
               END DO
            END DO
         END DO
      END DO
C
      RETURN
      END
C*MODULE EFPAUL  *DECK DZADX
      SUBROUTINE DZADX(EF3,SIJ,CA,CB,CLMOA,CLMOB,ZA,ZB,NORBA,NORBB,
     *                 NATA,NATB,MXMOA,MXMOB,IC3,IC4,NFRGPT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (TWO=2.0D+00)
      PARAMETER (MXATM=500)
C
      COMMON /GRAD  / DE(3,MXATM)
C
      DIMENSION SIJ(MXMOA,MXMOB), CA(3,NATA), CB(3,NATB)
      DIMENSION CLMOA(3,NORBA), CLMOB(3,NORBB), ZA(NATA), ZB(NATB)
      DIMENSION EF3(3,NFRGPT)
C
      DO I = 1,NORBA
         XI = CLMOA(1,I)
         YI = CLMOA(2,I)
         ZI = CLMOA(3,I)
         DO J = 1,NORBB
            S2 = TWO*SIJ(I,J)*SIJ(I,J)
            XJ = CLMOB(1,J)
            YJ = CLMOB(2,J)
            ZJ = CLMOB(3,J)
            DO JJ = 1,NATB
               XJJ = XI - CB(1,JJ)
               YJJ = YI - CB(2,JJ)
               ZJJ = ZI - CB(3,JJ)
               RIJJ = SQRT(XJJ*XJJ + YJJ*YJJ + ZJJ*ZJJ)
               R3 = RIJJ*RIJJ*RIJJ
               EF3(1,IC3+JJ) = EF3(1,IC3+JJ) - S2*ZB(JJ)*XJJ/R3
               EF3(2,IC3+JJ) = EF3(2,IC3+JJ) - S2*ZB(JJ)*YJJ/R3
               EF3(3,IC3+JJ) = EF3(3,IC3+JJ) - S2*ZB(JJ)*ZJJ/R3
            END DO
            DO II = 1,NATA
               XII = XJ - CA(1,II)
               YII = YJ - CA(2,II)
               ZII = ZJ - CA(3,II)
               RJII = SQRT(XII*XII + YII*YII + ZII*ZII)
               R3 = RJII*RJII*RJII
               IF (IC4.EQ.0) THEN
                  DE(1,II) = DE(1,II) - S2*ZA(II)*XII/R3
                  DE(2,II) = DE(2,II) - S2*ZA(II)*YII/R3
                  DE(3,II) = DE(3,II) - S2*ZA(II)*ZII/R3
               ELSE 
                  EF3(1,IC4+II) = EF3(1,IC4+II) - S2*ZA(II)*XII/R3
                  EF3(2,IC4+II) = EF3(2,IC4+II) - S2*ZA(II)*YII/R3
                  EF3(3,IC4+II) = EF3(3,IC4+II) - S2*ZA(II)*ZII/R3
               END IF
            END DO
         END DO
      END DO
C
      RETURN
      END           
C*MODULE EFPAUL  *DECK DPAULA
      SUBROUTINE DPAULA
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
      PARAMETER (MXATM=500, MXAO=2047)
C
      COMMON /FMCOM / X(1)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /OPTLOC/ CVGLOC,MAXLOC,IPRTLO,ISYMLO,IFCORE,NOUTA,NOUTB,
     *                MOOUTA(MXAO),MOOUTB(MXAO)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAULMO/ CENTNM(MXFGPT),CENTCD(3,MXFGPT),NORB(MXFRG),
     *                NPBF(MXFRG),NTMO
C
      IF(NTMO.EQ.0) RETURN
C
      NPRSAV = NPRINT
      NPRINT = -5
C
C    ALLOCATE MEMORY
C
      MXMO=0
      MXBF=0
      DO I = 1, NFRG
         MXMO=MAX(MXMO,NORB(I))
         MXBF=MAX(MXBF,NPBF(I))
      END DO
      MXMO2=(MXMO*MXMO+MXMO)/2
      L1 = NUM
      L2 = (L1*L1+L1)/2
      LNA2 = (NA*NA+NA)/2
      NLOC = NA-NOUTA
      NVIR = L1-NA
C
      CALL VALFM(LOADFM)
      LSMAT   = 1       + LOADFM
      LTMAT   = LSMAT   + L1*MXBF
      LEPS    = LTMAT   + L1*MXBF
      LEPT    = LEPS    + L1*MXBF
      LSIJ    = LEPT    + L1*MXBF
      LTIJ    = LSIJ    + NA*MXMO
      LWRK    = LTIJ    + NA*MXMO
      LVEC    = LWRK    + MXBF
      LARRAY  = LVEC    + L1*NA
      LFMO    = LARRAY  + L2
      LCCHG   = LFMO    + LNA2
      LFMOSQ  = LCCHG   + 3*NA
      LFMO2   = LFMOSQ  + NA*NA
      LARAY2  = LFMO2   + MXMO*MXMO
      LARAY3  = LARAY2  + L2
      LCMO    = LARAY3  + L2
      LTLOC   = LCMO    + L1*L1
      LSMJ    = LTLOC   + NA*NA
      LXMK    = LSMJ    + L1*MXMO
      LXMKVI  = LXMK    + L1*NA
      LTMJ    = LXMKVI  + NVIR*NA
      LXMI    = LTMJ    + L1*MXMO
      LYMI    = LXMI    + L1*NA
      LZMI    = LYMI    + L1*NA
      LZQQ    = LZMI    + L1*NA
      LPROVEC = LZQQ    + NA
      LFOCKMA = LPROVEC + MXBF*NTMO
      LAST    = LFOCKMA + MXMO2*NFRG
C
      NEED = LAST - LOADFM - 1
      CALL GETFM(NEED)
C
      LENPV=MXBF*NTMO
      LENFM=MXMO2*NFRG
      IF (NFRG.GT.0.AND.NTMO.GT.0) THEN
         CALL DAREAD(IDAF,IODA,X(LPROVEC),LENPV,267,0)
         CALL DAREAD(IDAF,IODA,X(LFOCKMA),LENFM,268,0)
      END IF
C
      CALL DRPAUL(X(LSMAT),X(LTMAT),X(LEPS),X(LEPT),X(LSIJ),
     *           X(LTIJ),X(LWRK),X(LVEC),
     *           X(LARRAY),X(LFMO),X(LCCHG),X(LFMOSQ),X(LFMO2),  
     *           X(LARAY2),X(LARAY3),X(LCMO),X(LTLOC),X(LSMJ),X(LXMK),
     *           X(LXMKVI),X(LTMJ),X(LXMI),X(LYMI),X(LZMI),X(LZQQ),
     *           L1,MXBF,NA,LNA2,MXMO,L2,NLOC,NVIR,
     *           X(LPROVEC),X(LFOCKMA),MXBF,MXMO2)
C  
      CALL RETFM(NEED)
      NPRINT = NPRSAV
      RETURN
      END
C*MODULE EFPAUL  *DECK DRPAUL
      SUBROUTINE DRPAUL(SMAT,TMAT,EPS,EPT,SIJ,TIJ,WRK,VEC,ARRAY,FMO,
     *                  CCHG,FMOSQ,FMO2,ARRAY2,ARRAY3,CMO,TLOC,SMJ,XMK,
     *                  XMKVIR,TMJ,XMI,YMI,ZMI,ZQQ,L1,L1EF,LNA,LNA2,
     *                  NAEF,L2,NLOC,NVIR,PROVEC,FOCKMA,MXBF,MXMO2)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      PARAMETER (MXPT=100, MXPSH=5*MXPT, MXPG=5*MXPSH)
      PARAMETER (MXFRG=50, MXFGPT=MXPT*MXFRG)
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
      PARAMETER (MXGTOT=5000, MXSH=1000, MXATM=500)
C
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG)
     *                ,EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG)
     *                ,ATORQ(3,MXFRG)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)  
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /PAULIN/EXEF(MXPG,MXFRG),CSEF(MXPG,MXFRG),CPEF(MXPG,MXFRG),  
     *               CDEF(MXPG,MXFRG),CFEF(MXPG,MXFRG),CGEF(MXPG,MXFRG),
     *                PRNAME(MXFGPT),PRCORD(3,MXFGPT),EFZNUC(MXFGPT),
     *                KSTREF(MXPSH,MXFRG),KATMEF(MXPSH,MXFRG),
     *                KTYPEF(MXPSH,MXFRG),KNGEF(MXPSH,MXFRG),
     *                KLOCEF(MXPSH,MXFRG),KMINEF(MXPSH,MXFRG),
     *                KMAXEF(MXPSH,MXFRG),NSHLEF(MXFRG),NGSSEF(MXFRG),
     *                NATEF(MXFRG),NUMEF(MXFRG),NTPATM
      COMMON /PAULMO/ CENTNM(MXFGPT),CENTCD(3,MXFGPT),NORB(MXFRG),
     *                NPBF(MXFRG),NTMO
C
      DIMENSION SMAT(L1,L1EF), TMAT(L1,L1EF), SIJ(LNA,NAEF)
      DIMENSION TIJ(LNA,NAEF), WRK(L1EF), VEC(L1,LNA), ARRAY(L2)
      DIMENSION FMO(LNA2), CCHG(3,LNA), EPS(L1,L1EF), EPT(L1,L1EF)
      DIMENSION FMOSQ(LNA,LNA), FMO2(NAEF,NAEF), ARRAY2(L2), ARRAY3(L2)
      DIMENSION CMO(L1,L1),TLOC(LNA,LNA),SMJ(L1,NAEF),XMK(L1,LNA)  
      DIMENSION XMKVIR(NVIR,LNA), TMJ(L1,NAEF), XMI(L1,LNA), YMI(L1,LNA)
      DIMENSION ZMI(L1,LNA), ZQQ(LNA)
      DIMENSION PROVEC(MXBF,NTMO), FOCKMA(MXMO2,NFRG)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DPAULI /8HDPAULI  /
#else
      CHARACTER*8 :: DPAULI_STR
      EQUIVALENCE (DPAULI, DPAULI_STR)
      DATA DPAULI_STR/"DPAULI  "/
#endif
C
C   GET BOYS LMOS
C
      CALL VCLR(ARRAY,1,L2)
      CALL VCLR(ARRAY2,1,L2)
      CALL VCLR(ARRAY3,1,L2)
      CALL VCLR(XMK,1,L1*LNA)
      CALL VCLR(XMKVIR,1,NVIR*LNA)
      CALL VCLR(ZQQ,1,LNA)
C
      CALL DAWRIT(IDAF,IODA,ARRAY,L2,78,0)
      CALL DAWRIT(IDAF,IODA,ARRAY2,L2,79,0)
C
      CALL DAREAD(IDAF,IODA,VEC,L1*LNA,71,0)
      CALL DAREAD(IDAF,IODA,CCHG,3*LNA,83,0)
      CALL DAREAD(IDAF,IODA,FMO,LNA2,82,0)
      CALL DAREAD(IDAF,IODA,CMO,L1*L1,15,0)
      CALL DAREAD(IDAF,IODA,TLOC,LNA*LNA,73,0)
      CALL DAREAD(IDAF,IODA,ARRAY,L2,95,0)
      CALL DAREAD(IDAF,IODA,ARRAY2,L2,96,0)
      CALL DAREAD(IDAF,IODA,ARRAY3,L2,97,0)
C
      CALL EXPMAT(TLOC,LNA,NLOC,LNA)
      NOUT = LNA-NLOC
      DO I = 1,LNA
         DO J = 1,LNA
            IF (I.LE.NOUT) TLOC(I,J) = ZERO
            IF (J.LE.NOUT) TLOC(I,J) = ZERO
         END DO
         IF(I.LE.NOUT) TLOC(I,I) = ONE
      END DO
C
      CALL TFTRAB(CMO,L1,ARRAY,VEC,LNA,XMI,WRK)
      CALL TFTRAB(CMO,L1,ARRAY2,VEC,LNA,YMI,WRK)
      CALL TFTRAB(CMO,L1,ARRAY3,VEC,LNA,ZMI,WRK)
      CALL VCLR(ARRAY,1,L2)
      CALL VCLR(ARRAY2,1,L2)
      CALL VCLR(ARRAY3,1,L2)
C
C  CALCULATE QM/MM PAULI REPULSION
C
      INAT = 1
      IMO = 1
      IC1 = NMTTPT + NPTTPT
      IC3 = IC1
      DO IM = 1,NFRG
         IC1 = IC1 + NATEF(IM)
         CALL SANDT(1,SMAT,TMAT,L1EF,L1,NATEF(IM),NAT,
     *              NGSSEF(IM),NSHLEF(IM),EXEF(1,IM),CSEF(1,IM),
     *              CPEF(1,IM),CDEF(1,IM),CFEF(1,IM),CGEF(1,IM),
     *              KSTREF(1,IM),KATMEF(1,IM),KTYPEF(1,IM),
     *              KNGEF(1,IM),KLOCEF(1,IM),KMINEF(1,IM),KMAXEF(1,IM),  
     *              MXGTOT,NSHELL,EX,CS,CP,CD,CF,CG,KSTART,KATOM,KTYPE,
     *              KNG,KLOC,KMIN,KMAX,PRCORD(1,INAT),C)
         CALL VCLR(SIJ,1,LNA*NAEF)
         CALL VCLR(TIJ,1,LNA*NAEF)
         CALL VCLR(SMJ,1,L1*NAEF)
         CALL VCLR(TMJ,1,L1*NAEF)
         CALL TFSQP(SIJ,SMAT,VEC,PROVEC(1,IMO),WRK,L1,L1EF,LNA,NAEF,
     *              LNA,NAEF)
         CALL TFSQP(TIJ,TMAT,VEC,PROVEC(1,IMO),WRK,L1,L1EF,LNA,NAEF,
     *              LNA,NAEF)
         CALL TFSQP(SMJ,SMAT,CMO,PROVEC(1,IMO),WRK,L1,L1EF,L1,NAEF,
     *              L1,NAEF)
         CALL TFSQP(TMJ,TMAT,CMO,PROVEC(1,IMO),WRK,L1,L1EF,L1,NAEF,
     *              L1,NAEF)
C
C
C  DERIVATIVES WITH RESPECT TO CENTROID POINTS
C
C EEXCH(V;S0)
C
         CALL DVSZIP(SIJ,CCHG,CENTCD(1,IMO),DEF,
     *               LNA,NORB(IM),LNA,NAEF,IC1,0)
C
C EEXCH(V;S2)
C
         CALL DVSTWO(SIJ,C,PRCORD(1,INAT),CCHG,
     *              CENTCD(1,IMO),ZAN,EFZNUC(INAT),DEF,
     *              LNA,NORB(IM),NAT,NATEF(IM),LNA,NAEF,
     *              IC1,0)
C
C  DERIVATIVES WITH RESPECT TO NUCLEAR POINTS
C
C SET UP "OVERLAP DENSITY MATRICES"
C
         CALL ODM(EPS,EPT,SIJ,TIJ,VEC,PROVEC(1,IMO),
     *            CCHG,CENTCD(1,IMO),FMO,
     *            FOCKMA(1,IM),FMOSQ,FMO2,C,
     *            PRCORD(1,INAT),ZAN,EFZNUC(INAT),LNA,
     *            NORB(IM),NAT,NATEF(IM),L1,L1EF,LNA,NAEF,LNA2,
     *            MXMO2)
C
C  CALCULATE TORQUE
C
            CALL PTORQ(SMAT,TMAT,EPS,EPT,L1,L1EF,NSHELL,
     *                 KMIN,KMAX,KLOC,NSHLEF(IM),
     *                 KMINEF(1,IM),KMAXEF(1,IM),KLOCEF(1,IM),0,IM)
C
C CALCULATE OVERLAP AND KE DERIVATIVES
C
         CALL DSANDT(DEF,EPS,EPT,L1EF,L1,NATEF(IM),NAT,
     *              NGSSEF(IM),NSHLEF(IM),EXEF(1,IM),CSEF(1,IM),
     *              CPEF(1,IM),CDEF(1,IM),CFEF(1,IM),CGEF(1,IM),
     *              KSTREF(1,IM),KATMEF(1,IM),KTYPEF(1,IM),
     *              KNGEF(1,IM),KLOCEF(1,IM),KMINEF(1,IM),KMAXEF(1,IM),
     *              MXGTOT,NSHELL,EX,CS,CP,CD,CF,CG,KSTART,KATOM,KTYPE,
     *              KNG,KLOC,KMIN,KMAX,PRCORD(1,INAT),C,IC3,0,MXFGPT)
C
C CALCULATE REMANINING DERIVATIVE
C
         CALL DZADX(DEF,SIJ,C,PRCORD(1,INAT),
     *              CCHG,CENTCD(1,IMO),ZAN,
     *              EFZNUC(INAT),LNA,NORB(IM),NAT,NATEF(IM),
     *              LNA,NAEF,IC3,0,MXFGPT)
C
C AB INITIO CENTROIDS POINTS
C
         CALL ABCENT(ARRAY,ARRAY2,ARRAY3,SIJ,VEC,
     *               CCHG,CENTCD(1,IMO),PRCORD(1,INAT),
     *               EFZNUC(INAT),LNA,NORB(IM),NATEF(IM),L1,L2)
C
         CALL XIJ(XMK,SMJ,TMJ,SIJ,TIJ,XMI,YMI,ZMI,TLOC,
     *            CCHG,CENTCD(1,IMO),FMO,
     *            FOCKMA(1,IM),FMOSQ,FMO2,C,
     *            PRCORD(1,INAT),ZAN,EFZNUC(INAT),LNA,
     *            NORB(IM),NAT,NATEF(IM),L1,NAEF,LNA2,
     *            MXMO2)
C
         CALL FRMZQQ(ZQQ,SMJ,L1,LNA,NORB(IM),NAEF)
C
         IMO = IMO + NORB(IM)
         INAT = INAT + NATEF(IM)
         IC1 = IC1 + NORB(IM)
         IC3 = IC3 + NATEF(IM) + NORB(IM)
      END DO
C
      CALL FZMAT(XMK,XMKVIR,CMO,ZQQ,L1,L2,NVIR,LNA)
C
      CALL DIPDER(ARRAY,ARRAY2,ARRAY3,DUMMY,L2,DUMMY,0,0,0,DPAULI)
C
      RETURN
      END
C*MODULE EFPAUL  *DECK ABCENT
      SUBROUTINE ABCENT(Q1,Q2,Q3,SIJ,VECA,CLMOA,CLMOB,CB,ZB,
     *                  NORBA,NORBB,NATB,L1,L2)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      PARAMETER (TWO=2.0D+00, PI=3.1415927D+00)
      PARAMETER (TM6=1.0D-06, FOUR=4.0D+00)
C
      DIMENSION Q1(L2), Q2(L2), Q3(L2), SIJ(NORBA,NORBB), VECA(L1,NORBA)  
      DIMENSION CLMOA(3,NORBA), CLMOB(3,NORBB), CB(3,NATB)
      DIMENSION ZB(NATB)
C
      MUNU = 0
      DO MU = 1,L1
         DO NU = 1,MU
            MUNU = MUNU + 1
            DO I = 1,NORBA
               XI = CLMOA(1,I) 
               YI = CLMOA(2,I) 
               ZI = CLMOA(3,I) 
               DO J = 1,NORBB
                  XJ = CLMOB(1,J)
                  YJ = CLMOB(2,J)
                  ZJ = CLMOB(3,J)
                  X = XI - XJ
                  Y = YI - YJ
                  Z = ZI - ZJ
                  SVECMN = TWO*SIJ(I,J)*SIJ(I,J)*VECA(MU,I)*VECA(NU,I)  
C DE(V;S0)
                  RIJ = SQRT(X*X + Y*Y + Z*Z)
                  R3 = RIJ*RIJ*RIJ
                  IF(ABS(SIJ(I,J)) .LE. TM6)  GO TO 100
                  A = TWO*SQRT(-TWO*LOG(ABS(SIJ(I,J)))/PI)
                  B = A*SVECMN/R3
                  Q1(MUNU) = Q1(MUNU) + B*X
                  Q2(MUNU) = Q2(MUNU) + B*Y
                  Q3(MUNU) = Q3(MUNU) + B*Z
  100             CONTINUE
C DE(V;S2)
                  DO K = 1,NORBA
                     A = FOUR*SIJ(K,J)*SIJ(K,J)*VECA(MU,I)*VECA(NU,I)/R3  
                     Q1(MUNU) = Q1(MUNU) - X*A
                     Q2(MUNU) = Q2(MUNU) - Y*A
                     Q3(MUNU) = Q3(MUNU) - Z*A 
                  END DO
                  A = SVECMN/R3
                  Q1(MUNU) = Q1(MUNU) + X*A
                  Q2(MUNU) = Q2(MUNU) + Y*A
                  Q3(MUNU) = Q3(MUNU) + Z*A
                  DO JJ = 1,NATB
                     XJJ = XI - CB(1,JJ)
                     YJJ = YI - CB(2,JJ)
                     ZJJ = ZI - CB(3,JJ)
                     RIJJ = SQRT(XJJ*XJJ + YJJ*YJJ + ZJJ*ZJJ)
                     R3 = RIJJ*RIJJ*RIJJ
                     Q1(MUNU) = Q1(MUNU) + SVECMN*ZB(JJ)*XJJ/R3
                     Q2(MUNU) = Q2(MUNU) + SVECMN*ZB(JJ)*YJJ/R3
                     Q3(MUNU) = Q3(MUNU) + SVECMN*ZB(JJ)*ZJJ/R3
                   END DO
                   DO L = 1,NORBB
                     XL = XI - CLMOB(1,L)
                     YL = YI - CLMOB(2,L)
                     ZL = ZI - CLMOB(3,L)
                     RIL = SQRT(XL*XL + YL*YL + ZL*ZL)
                     R3 = RIL*RIL*RIL
                     Q1(MUNU) = Q1(MUNU) - SVECMN*TWO*XL/R3
                     Q2(MUNU) = Q2(MUNU) - SVECMN*TWO*YL/R3
                     Q3(MUNU) = Q3(MUNU) - SVECMN*TWO*ZL/R3
                  END DO
               END DO
            END DO
         END DO
      END DO
C
      RETURN
      END
C*MODULE EFPAUL  *DECK QMADD
      SUBROUTINE QMADD(D,Q,L2,NREC)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
C
      DIMENSION D(L2), Q(L2)
C
      CALL DAREAD(IDAF,IODA,Q,L2,NREC,0)
C
      CALL VADD(D,1,Q,1,D,1,L2)
C
      RETURN
      END 
C*MODULE EFPAUL  *DECK DABPAU
      SUBROUTINE DABPAU(II,JJ,KK,LL,UHFTYP,DA,DB,DAB,DABMAX,Q4,POPLE)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION DA(*),DB(*),DAB(*)
      LOGICAL UHFTYP,POPLE
C
      PARAMETER (MXGTOT=5000, MXSH=1000, MXAO=2047)
C
      LOGICAL OUT,DBG
      COMMON /DSHLT / RTOL,DTOL,VTOL1,VTOL2,VTOLS,OUT,DBG
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /INDD80/ IMAX,JMAX,KKMAX,LMAX
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /SHLLMN/ IGXYZ(4,35),JGXYZ(4,35),KGXYZ(4,35),LGXYZ(4,35)
      COMMON /SHLNRM/ PNRM(35)
      LOGICAL IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
      COMMON /SHLEQU/ IIEQJJ,KKEQLL,IJEQKL,IJGTKL,IJLTKL
C
CJMS  LABELLED COMMON GSPG80 DEFINED FOR COMPUTATIONAL EFFICIENCY.
CJMS  FOR SP BASES ONLY, IT CONTAINS THE E ARRAY WHICH IS THE DAB
CJMS  ARRAY WITH INDICES IN REVERSE ORDER: E(I,J,K,L)= DAB(L,K,J,I)
CJMS  AND IS USED IN SUB JKDG80 (MODULE GRD2B). IT ORIGINATES IN:
CJMS  
CJMS     1. SUBS DABCLU, DABDFT, DABGVB, DABMC, DABMP2 AND DABUMP
CJMS        (MODULE GRD2A) AND SUB DABPAU (MODULE EFPAUL) WHICH ARE
CJMS        ALL CALLED BY SUB JKDER (MODULE GRD2A)
CJMS  
CJMS     2. SUB DABCLU (MODULE GRD2A) WHICH IS CALLED BY SUB EFDEN OF
CJMS        MODULE EFGRD2
CJMS  
CJMS     3. SUB PAR2PDM (MODULE MP2DDI) WHICH IS CALLED BY SUB PJKDMP2
CJMS        OF MODULE MP2DDI
C
      COMMON /GSPG80/ E(4,4,4,4)
C
      PARAMETER (ZER=0.0D+00, PT5=0.5D+00, P25=.25D+00, F04=4.0D+00)
C
C     ----- THIS ROUTINE FORMS THE PRODUCT OF DENSITY       -----
C           MATRICES FOR USE IN FORMING THE TWO ELECTRON
C           GRADIENT.  VALID FOR CLOSED AND OPEN SHELL SCF.
C
      DABMAX= ZER
      MINI= KMIN(II)
      MINJ= KMIN(JJ)
      MINK= KMIN(KK)
      MINL= KMIN(LL)
      LOCI= KLOC(II)-MINI
      LOCJ= KLOC(JJ)-MINJ
      LOCK= KLOC(KK)-MINK
      LOCL= KLOC(LL)-MINL
      IF(POPLE) THEN
         DO 110 L=1,LMAX
            DO 110 K=1,KKMAX
               KAL= MAX0(LOCK+K,LOCL+L)
               KIL= MIN0(LOCK+K,LOCL+L)
               DO 110 J=1,JMAX
                  DO 110 I=1,IMAX
                     IAJ= MAX0(LOCI+I,LOCJ+J)
                     IIJ= MIN0(LOCI+I,LOCJ+J)
                     IN = IAJ
                     JN = IIJ
                     KN = KAL
                     LN = KIL
                     IF(IN.LT.KN .OR.(IN.EQ.KN .AND. JN.LT.LN)) THEN
                        IN = KAL
                        JN = KIL
                        KN = IAJ
                        LN = IIJ
                     ENDIF
C        -----  NOW CALCULATE DENSITY MATRIX CONTRIBUTION.      -----
C        -----  (IJ/KL),(IJ/IL),(IJ/JL),(IJ/KJ)                 -----
C        -----  EIGHT DISTINCT INTEGRALS                        -----
C        -----  CONTRIBUTION TO THE ENERGY                      -----
C        -----  4(IJ)(KL) - (IK)(JL) - (JK)(IL)                 -----
                     IJ = IA(IN)+JN
                     IK = IA(IN)+KN
                     IL = IA(IN)+LN
                     JK = IA(MAX0(JN,KN))+MIN0(JN,KN)
                     JL = IA(JN)+LN
                     IF(JN.LT.KN) JL = IA(MAX0(JN,LN))+MIN0(JN,LN)
                     KL = IA(KN)+LN
                     DF1= DA(IJ)*DA(KL)+
     2                    DA(IJ)*DB(KL)+DB(IJ)*DA(KL)
                     DQ1= DA(IK)*DA(JL)+DA(IL)*DA(JK)+
     2                    DA(IK)*DB(JL)+DA(IL)*DB(JK)+
     3                    DB(IK)*DA(JL)+DB(IL)*DA(JK) 
                     IF(UHFTYP) DQ1=DQ1+DB(IK)*DB(JL)+DB(IL)*DB(JK)
                     DF1=(DF1-P25*DQ1)*Q4
                     IF(DABMAX.LT. ABS(DF1)) DABMAX= ABS(DF1)
                     E(I,J,K,L)= DF1
                     IF(OUT) WRITE(IW,9010) II,JJ,KK,LL,I,J,K,L,DF1
  110    CONTINUE
      ELSE
C
C D AND HIGHER FUNCTIONS OR HONDO ONLY RUNS
C
         MAXI= KMAX(II)
         MAXJ= KMAX(JJ)
         MAXK= KMAX(KK)
         MAXL= KMAX(LL)
         DO 210 I=MINI,MAXI
            P1I = PNRM(I)
            JMAX= MAXJ
            IF(IIEQJJ) JMAX= I
            DO 210 J=MINJ,JMAX
               P2J = P1I*PNRM(J)
               IAJ= MAX0(LOCI+I,LOCJ+J)
               IIJ= MIN0(LOCI+I,LOCJ+J)
               KMMAX=MAXK
               IF(IJEQKL) KMMAX= I
               DO 210 K=MINK,KMMAX
                  P3K = P2J*PNRM(K)
                  LMAX= MAXL
                  IF(KKEQLL) LMAX= K
                  IF(IJEQKL .AND. K.EQ.I) LMAX= J
                  DO 210 L=MINL,LMAX
                     P4L = P3K*PNRM(L)
                     KAL= MAX0(LOCK+K,LOCL+L)
                     KIL= MIN0(LOCK+K,LOCL+L)
                     IN = IAJ
                     JN = IIJ
                     KN = KAL
                     LN = KIL
                     IF(IN.LT.KN .OR.(IN.EQ.KN .AND. JN.LT.LN)) THEN
                        IN = KAL
                        JN = KIL
                        KN = IAJ
                        LN = IIJ
                     ENDIF
                     IJ = IA(IN)+JN
                     IK = IA(IN)+KN
                     IL = IA(IN)+LN
                     JK = IA(MAX0(JN,KN))+MIN0(JN,KN)
                     JL = IA(JN)+LN
                     IF(JN.LT.KN) JL = IA(MAX0(JN,LN))+MIN0(JN,LN)
                     KL = IA(KN)+LN
                     DF1= DA(IJ)*DA(KL)+
     2                    DA(IJ)*DB(KL)+DB(IJ)*DA(KL)
                     DQ1= DA(IK)*DA(JL)+DA(IL)*DA(JK)+
     2                    DA(IK)*DB(JL)+DA(IL)*DB(JK)+
     3                    DB(IK)*DA(JL)+DB(IL)*DA(JK) 
                     IF(UHFTYP) DQ1=DQ1+DB(IK)*DB(JL)+DB(IL)*DB(JK)
                     DF1= DF1*F04-DQ1
                     IF(JN.EQ.IN               ) DF1= DF1*PT5
                     IF(LN.EQ.KN               ) DF1= DF1*PT5
                     IF(KN.EQ.IN .AND. LN.EQ.JN) DF1= DF1*PT5
                     IF(DABMAX.LT. ABS(DF1)) DABMAX= ABS(DF1)
C
C IGXYZ AND J, K, AND L ARE SET UP IN JKDNDX
C
                     IJKL=IGXYZ(1,I)+JGXYZ(1,J)+KGXYZ(1,K)+LGXYZ(1,L)
                     DAB(IJKL)= DF1*P4L
                     IF(OUT) WRITE(IW,9020) II,JJ,KK,LL,I,J,K,L,IJKL,DF1
  210    CONTINUE
      END IF
      RETURN
 9010 FORMAT(' -DABPAU,POPLE- ',4I4,4I3,D20.12)
 9020 FORMAT(' -DABPAU,HONDO- ',4I4,4I3,I5,D20.12)
      END
C*MODULE EFPAUL  *DECK FZMAT
      SUBROUTINE FZMAT(XMK,XMKVIR,CMO,ZQQ,L1,L2,NVIR,NOC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL PACK2E
C
      COMMON /DIISSO/ SOGTOL,ETHRSH,MAXDII,IRAF
      COMMON /FMCOM / X(1)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
C
      DIMENSION XMK(L1,NOC), XMKVIR(NVIR,NOC), CMO(L1,L1), ZQQ(NOC)  
C
      MAXIT  = 50
      MAXIO  = 2*MAXDII
      MAXIT2 = (MAXIT*MAXIT+MAXIT)/2
C
      NBF = L1
      NBF2 = (L1*L1+L1)/2
      NBF3 = L1*L1
      NROT = NVIR*NOC
C
      CALL VALFM(LOADFM)
      IRHO  = 1     + LOADFM
      IXX   = IRHO  + NBF2
      IY    = IXX   + NBF3
      IXLMN = IY    + NBF3
      LBUF  = IXLMN + NBF3
      IBUF  = LBUF  + NINTMX
      IPAI  = IBUF  + NINTMX
      IPBJ  = IPAI  + NROT
      IERR  = IPBJ  + NROT
      IENG  = IERR  + NROT
      LGIN  = IENG  + NBF
      LARRAY= LGIN  + NOC*NOC
      IAM   = LARRAY+ NBF2
      IX    = IAM   + MAXDII*MAXDII
      IPVT  = IX    + MAXIT
      IB    = IPVT  + MAXIT
      IODIIS= IB    + MAXIT2
      LAST  = IODIIS+ MAXIO
      NEED  = LAST - LOADFM - 1
      CALL GETFM(NEED)
C
      CALL DAREAD(IDAF,IODA,X(IENG),NBF,17,0)
C
      DO NI = 1,NOC
         DO NA = 1,NVIR
            XMKVIR(NA,NI) = XMK(NA+NOC,NI)
         END DO
      END DO
      CALL XIJP(X(IRHO),X(IXX),X(IY),X(IXLMN),X(LBUF),X(IBUF),
     *          XMK,XMKVIR,X(LGIN),X(IPAI),NOC,NVIR,NBF,CMO,
     *          X(IENG),ZQQ,NROT)
      WRITE(IW,9980)
      CALL AOCPHF(X(IRHO),X(IXX),X(IY),X(IXLMN),X(LBUF),X(IBUF),
     *            XMKVIR,X(IPAI),X(IPBJ),NOC,NVIR,NBF,CMO,
     *            X(IERR),X(IENG),X(IAM),X(IX),X(IPVT),X(IB),
     *            X(IODIIS),X(IENG),NROT,MAXIT)
C
      CALL DCOPY(NROT,X(IPAI),1,XMKVIR,1)
C
C   FRISCH PAPER HAS THE ORB. ENERGIES SWITCHED, I.E. SIGN CHANGED
C
      DO NI = 1,NOC
         DO NA = 1,NVIR
            XMK(NA+NOC,NI) = -XMKVIR(NA,NI)
         END DO
      END DO
C
      CALL XII(X(LARRAY),XMK,CMO,NOC,NBF,NBF2)
C
      CALL FBMAT(X(LARRAY),XMK,X(LGIN),X(IRHO),X(IXX),X(IY),X(LBUF),
     *           CMO,X(IENG),X(IXLMN),ZQQ,X(IBUF),NOC,NBF,L2,
     *           NINTMX)  
C
      CALL RETFM(NEED)
C
      RETURN
 9980 FORMAT(/' ..... SOLVING CPHF IN THE AO BASIS .....')
      END
C*MODULE EFPAUL  *DECK FBMAT
      SUBROUTINE FBMAT(ARRAY,XMK,GIN,PMN,XX,Y,BUF,C,E,XLMN,ZQQ,IBUF,
     *                 NOC,NBF,L2,NINTMX)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, TWO=2.0D+00)
      PARAMETER (TOL=1.0D-04)
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
C
      DIMENSION ARRAY(L2), XMK(NBF,NOC), PMN(L2), GIN(NOC,NOC)
      DIMENSION XX(NBF,NBF), Y(NBF,NBF), BUF(NINTMX), IBUF(NINTMX)
      DIMENSION C(NBF,NBF), E(NBF), XLMN(NBF,NBF), ZQQ(NOC)
C
C
C     ---- CALCULATE PAI USING EQUATION 10 IN FRISCH'S PAPER ----
C
      DO NI=1,NOC
         DO NA=1,NOC
            IF ( ABS(E(NI)-E(NA)) .GE.TOL) THEN
               XMK(NA,NI)=XMK(NA,NI)/(E(NI)-E(NA))
            ELSE
               IF(NA.NE.NI) WRITE(IW,*) 'WARNING:',NI,NA,E(NI),E(NA)
               XMK(NA,NI) = ZERO
            END IF
         END DO
         XMK(NI,NI) = ZQQ(NI)
      END DO
C
C
C        BEGIN TRANSFORMING -PBJ- TO AO BASIS AS -PMN-
C
      CALL VCLR(Y,1,NBF*NBF)
C
      CALL MRARTR(XMK,NBF,NBF,NOC,C,NBF,NBF,XX,NBF)
      CALL DGEMM('N','N',NBF,NBF,NBF,ONE,C,NBF,XX,NBF,
     *     ONE,Y,NBF)
C
      MUNU = 0
      DO 110 MU=1,NBF
         DO 100 NU=1,MU
            MUNU=MUNU+1
            PMN(MUNU) = (Y(MU,NU) + Y(NU,MU))/TWO
  100    CONTINUE
  110 CONTINUE
C
      CALL DAWRIT(IDAF,IODA,PMN,L2,79,0)
C
C           NOW FORM FOCK-LIKE MATRIX
C           DIRECT METHOD = RECOMPUTE 2E- AO INTEGRALS
C           STANDARD METHOD = PROCESS INTEGRALS FROM DISK
C
      CALL DSCAL(L2,TWO,PMN,1)
      CALL FLMAT(PMN,Y,XLMN,BUF,IBUF,NBF)
      CALL VCLR(GIN,1,NOC*NOC)
      CALL MRARBR(Y,NBF,NBF,NBF,C,NBF,NOC,XX,NBF)
      CALL DGEMM('T','N',NOC,NOC,NBF,ONE,C,NBF,XX,NBF,
     *           ONE,GIN,NOC)
      CALL VCLR(Y,1,NBF*NBF)
      CALL MRARTR(GIN,NOC,NOC,NOC,C,NBF,NBF,XX,NBF)
      CALL MRARBR(C,NBF,NBF,NOC,XX,NBF,NBF,Y,NBF)
      MUNU = 0
      DO 160 MU=1,NBF
         DO 150 NU=1,MU
            MUNU=MUNU+1
            ARRAY(MUNU) = ARRAY(MUNU) - (Y(MU,NU) + Y(NU,MU))/TWO
  150    CONTINUE
  160 CONTINUE
C
      DO 200 NI=1,NOC
         DO 210 NA=1,NBF
            XMK(NA,NI)=XMK(NA,NI)*E(NI)
  210    CONTINUE
  200 CONTINUE
C
      CALL VCLR(Y,1,NBF*NBF)
C
      CALL MRARTR(XMK,NBF,NBF,NOC,C,NBF,NBF,XX,NBF)
      CALL DGEMM('N','N',NBF,NBF,NBF,ONE,C,NBF,XX,NBF,
     *     ONE,Y,NBF)
C
      MUNU = 0
      DO 260 MU=1,NBF
         DO 250 NU=1,MU
            MUNU=MUNU+1
            ARRAY(MUNU) = ARRAY(MUNU) - (Y(MU,NU) + Y(NU,MU))/TWO
  250    CONTINUE
  260 CONTINUE
C
      CALL DAWRIT(IDAF,IODA,ARRAY,L2,78,0)
C 
      RETURN
      END
C*MODULE EFPAUL  *DECK EXPMAT
      SUBROUTINE EXPMAT(VEC,ICOL,IROW,LDV)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION VEC(LDV*ICOL)
C
      IOFF = (ICOL-IROW)*ICOL + (ICOL-IROW)
      DO IIP = IROW,1,-1
         IS     = (IIP-1)*IROW + 1
         ISHDBE = IOFF + (IIP-1)*ICOL + 1
         CALL DCOPY(ICOL,VEC(IS),1,VEC(ISHDBE),1)
      END DO
C
      RETURN
      END
C*MODULE EFPAUL  *DECK XIJP
      SUBROUTINE XIJP(PMN,XX,Y,XLMN,BUF,IBUF,XMK,XMKVIR,GIN,PAI,
     *                NOC,NVIR,NBF,C,E,ZQQ,NROT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL PACK2E
C
      DIMENSION PAI(NVIR,NOC), PMN(*)
      DIMENSION XX(NBF,NBF), XLMN(*), XMKVIR(NVIR,NOC), XMK(NBF,NOC)  
      DIMENSION Y(NBF,NBF),   BUF(NINTMX), IBUF(NINTMX)
      DIMENSION C(NBF,*), E(NBF), GIN(NOC,NOC)
      DIMENSION ZQQ(NOC)
C
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, TWO=2.0D+00)
      PARAMETER (TOL=1.0D-04)
C
      DO NI=1,NOC
         DO NA=1,NOC
            IF ( ABS(E(NI)-E(NA)) .GE.TOL) THEN
               GIN(NA,NI)=XMK(NA,NI)/(E(NI)-E(NA))
            ELSE
               IF(NA.NE.NI) WRITE(IW,*) 'WARNING:',NI,NA,E(NI),E(NA)
               GIN(NA,NI) = ZERO
            END IF
         END DO
         GIN(NI,NI) = ZQQ(NI)
      END DO
C
C     ----- FORM D(LAMDA,SIGMA) WHICH WE CALL PMN -----
C     ACCORDING TO EQUATION 17 OF THE FRISCH PAPER.
C
C        BEGIN TRANSFORMING -PBJ- TO AO BASIS AS -PMN-
C
      CALL VCLR(Y,1,NBF*NBF)
C
      CALL MRARTR(GIN,NOC,NOC,NOC,C,NBF,NBF,XX,NBF)
      CALL DGEMM('N','N',NBF,NBF,NOC,TWO,C,NBF,XX,NBF,
     *     ONE,Y,NBF)
C
      MUNU = 0
      DO 110 MU=1,NBF
         DO 100 NU=1,MU
            MUNU=MUNU+1
            PMN(MUNU) = Y(MU,NU) + Y(NU,MU)
  100    CONTINUE
  110 CONTINUE
C
C           NOW FORM FOCK-LIKE MATRIX
C           DIRECT METHOD = RECOMPUTE 2E- AO INTEGRALS
C           STANDARD METHOD = PROCESS INTEGRALS FROM DISK
C
      CALL FLMAT(PMN,Y,XLMN,BUF,IBUF,NBF)
C
C     ---- TRANSFORM THE FOCK-LIKE MATRIX G(MU,NU) TO MO BASIS ----
C
      CALL VCLR(PAI,1,NROT)
C
      CALL MRARBR(Y,NBF,NBF,NBF,C,NBF,NOC,XX,NBF)
      CALL DGEMM('T','N',NVIR,NOC,NBF,ONE,C(1,NOC+1),NBF,XX,NBF,
     *           ONE,PAI,NVIR)
C
C     ---- CALCULATE PAI USING EQUATION 10 IN FRISCH'S PAPER ----
C
      DO 700 NI=1,NOC
         DO 710 NA=1,NVIR
            XMKVIR(NA,NI)=XMKVIR(NA,NI) + PAI(NA,NI)
  710    CONTINUE
  700 CONTINUE
C
      RETURN
      END 
C*MODULE EFPAUL  *DECK XII
      SUBROUTINE XII(ARRAY,XMK,VEC,NORBA,L1,L2)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (TWO=2.0D+00)
C
      DIMENSION ARRAY(L2), XMK(L1,NORBA), VEC(L1,L1)
C
      CALL VCLR(ARRAY,1,L2)
      MUNU = 0
      DO MU = 1,L1
         DO NU = 1,MU
            MUNU = MUNU + 1
            DO I = 1,NORBA
               ARRAY(MUNU)=ARRAY(MUNU)-XMK(I,I)*VEC(MU,I)*VEC(NU,I)/TWO  
            END DO
         END DO
      END DO
C
      RETURN
      END
C*MODULE EFPAUL  *DECK TFTRAB
      SUBROUTINE TFTRAB(A,NA,B,C,NC,D,WRK)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (ZERO=0.0D+00)
C
      DIMENSION C(NA,NC), B(NA*(NA+1)/2), A(NA,NA), D(NA,NC), WRK(NA)
C
C     D = AT * B * C  WHERE B=SYMMETRIC, A=SQUARE, C=RECTANGULAR
C
      CALL MTARBR(B,NA,C,NC,D,NA,1)
C
      DO 190 J=1,NC
         DO 170 I=1,NA
            DUM = ZERO
            DO 160 K=1,NA
               DUM = DUM + A(K,I)*D(K,J)
  160       CONTINUE
            WRK(I) = DUM
  170    CONTINUE
         DO 180 I=1,NA
            D(I,J) = WRK(I)
  180    CONTINUE
  190 CONTINUE
C
      RETURN
      END
C*MODULE EFPAUL  *DECK XIJ
      SUBROUTINE XIJ(XMK,SMJ,TMJ,SIJ,TIJ,XMI,YMI,ZMI,TLOC,CLMOA,CLMOB,
     *               FA,FB,FASQ,FBSQ,CA,CB,ZA,ZB,NORBA,NORBB,NATA,NATB,  
     *               L1,NAEF,MXMO2A,MXMO2B)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (ZERO=0.0D+00,ONE=1.0D+00,FOUR=4.0D+00)
      PARAMETER (TWO=2.0D+00, TM6=1.0D-06, PI=3.1415927D+00)
C
      DIMENSION SIJ(NORBA,NAEF), TIJ(NORBA,NAEF)
      DIMENSION CLMOA(3,NORBA), CLMOB(3,NORBB), FA(MXMO2A), FB(MXMO2B)
      DIMENSION FASQ(NORBA,NORBA), FBSQ(NAEF,NAEF), CA(3,NATA)
      DIMENSION CB(3,NATB), ZA(NATA), ZB(NATB), XMK(L1,NORBA)
      DIMENSION SMJ(L1,NAEF), TMJ(L1,NAEF), XMI(L1,NORBA)
      DIMENSION YMI(L1,NORBA), ZMI(L1,NORBA), TLOC(NORBA,NORBA)
C
      CALL CPYTSQ(FA,FASQ,NORBA,1)
      CALL CPYTSQ(FB,FBSQ,NAEF,1)
C
      DO M = 1,L1
         DO IQ = 1,NORBA
            DO I = 1,NORBA
               XI = CLMOA(1,I)
               YI = CLMOA(2,I)
               ZI = CLMOA(3,I)
               DO J = 1,NORBB
                  XJ = CLMOB(1,J)
                  YJ = CLMOB(2,J)
                  ZJ = CLMOB(3,J)
                  X = XI - XJ
                  Y = YI - YJ
                  Z = ZI - ZJ
C DE(V;S0)
                  RIJ = SQRT(X*X + Y*Y + Z*Z)
                  RIJ3 = RIJ*RIJ*RIJ
                  SUMS = ZERO
                  SUMR = ZERO
                  IF(ABS(SIJ(I,J)) .LE. TM6)  GO TO 100
                  A = FOUR*SQRT(-TWO*LOG(ABS(SIJ(I,J)))/PI)
                  B = SQRT( -TWO/(PI*LOG(ABS(SIJ(I,J)))) )
                  SUMS =  -TWO*(A-B)*(SIJ(I,J)/RIJ)
                  SUMR =  -A*SIJ(I,J)*SIJ(I,J)
  100             CONTINUE
C DE(V;S1)
C 
                  SUMT = FOUR*SIJ(I,J)
C
                  SUMK = ZERO
                  DO K = 1,NORBA
                     SUMS = SUMS - FOUR*SIJ(K,J)*FASQ(I,K)
                     SUMK = SUMK + SIJ(K,J)*SIJ(K,J)
                  END DO
                  SUML = ZERO
                  DO L = 1,NORBB
                     SUMS = SUMS - FOUR*SIJ(I,L)*FBSQ(J,L)
                     SUML = SUML + SIJ(I,L)*SIJ(I,L)
                  END DO
                  SUMS = SUMS + FOUR*TIJ(I,J)
                  SUMR = SUMR + FOUR*(SUML+SUMK) - TWO*SIJ(I,J)*SIJ(I,J)  
C DE(V;S2)
                  SUMJJ = ZERO
                  DO JJ = 1,NATB
                     XJJ = XI - CB(1,JJ)
                     YJJ = YI - CB(2,JJ)
                     ZJJ = ZI - CB(3,JJ)
                     RIJJ = SQRT(XJJ*XJJ + YJJ*YJJ + ZJJ*ZJJ)
                     SUMJJ = SUMJJ - ZB(JJ)/RIJJ
C
                    YMQIJ =FOUR*(XJJ*XMI(M,I)+YJJ*YMI(M,I)+ZJJ*ZMI(M,I))  
                    YMQIJ = -YMQIJ*TLOC(IQ,I)/(RIJJ*RIJJ*RIJJ)
                    XMK(M,IQ) = XMK(M,IQ)-ZB(JJ)*SIJ(I,J)*SIJ(I,J)*YMQIJ    
                  END DO
                  SUML = ZERO
                  DO L = 1,NORBB
                     XL = XI - CLMOB(1,L)
                     YL = YI - CLMOB(2,L)
                     ZL = ZI - CLMOB(3,L)
                     RIL = SQRT(XL*XL + YL*YL + ZL*ZL)
                     SUML = SUML + TWO/RIL
                  END DO
                  SUMII = ZERO
                  DO II = 1,NATA
                     XII = XJ - CA(1,II)
                     YII = YJ - CA(2,II)
                     ZII = ZJ - CA(3,II)
                     RJII = SQRT(XII*XII + YII*YII + ZII*ZII)
                     SUMII = SUMII - ZA(II)/RJII
                  END DO
                  SUMK = ZERO
                  DO K = 1,NORBA
                     XK = XJ - CLMOA(1,K)
                     YK = YJ - CLMOA(2,K)
                     ZK = ZJ - CLMOA(3,K)
                     RKJ = SQRT(XK*XK + YK*YK + ZK*ZK)
                     SUMK = SUMK + TWO/RKJ
                  END DO
                  SUMS = SUMS + FOUR*SIJ(I,J)*
     *                               (SUMJJ+SUML+SUMII+SUMK-ONE/RIJ)
C
                  XMK(M,IQ) = XMK(M,IQ) + SUMS*TLOC(IQ,I)*SMJ(M,J)
                  XMK(M,IQ) = XMK(M,IQ) + SUMT*TLOC(IQ,I)*TMJ(M,J)
                  YMQIJ = TWO*(X*XMI(M,I)+Y*YMI(M,I)+Z*ZMI(M,I))
                  YMQIJ = YMQIJ*TLOC(IQ,I)/RIJ3
                  XMK(M,IQ) = XMK(M,IQ) - SUMR*YMQIJ
               END DO
            END DO
         END DO
      END DO
C
      RETURN
      END
C*MODULE EFPAUL  *DECK FRMZQQ
      SUBROUTINE FRMZQQ(ZQQ,SMJ,L1,LNA,NORBB,NAEF)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (TWO=2.0D+00)
C
      DIMENSION ZQQ(LNA), SMJ(L1,NAEF)
C
      DO IQ = 1,LNA
         DO J = 1,NORBB
            ZQQ(IQ)=ZQQ(IQ)-TWO*SMJ(IQ,J)*SMJ(IQ,J)
         END DO
      END DO
C
      RETURN
      END
C*MODULE EFPAUL  *DECK PTORQ
      SUBROUTINE PTORQ(SMAT,TMAT,EPS,EPT,MXBFA,MXBFB,NSHLA,KMINA,
     *                 KMAXA,KLOCA,NSHLB,KMINB,KMAXB,KLOCB,IFRG,JFRG)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION SMAT(MXBFA,MXBFB), TMAT(MXBFA,MXBFB)
      DIMENSION EPS(MXBFA,MXBFB), EPT(MXBFA,MXBFB)
      DIMENSION KMINA(NSHLA), KMAXA(NSHLA), KLOCA(NSHLA)
      DIMENSION KMINB(NSHLB), KMAXB(NSHLB), KLOCB(NSHLB)
C
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
C
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                EFCENT(3,MXFRG),FRGMAS(MXFRG),FRGMI(6,MXFRG),
     *                ATORQ(3,MXFRG)
C
      PARAMETER (ZERO=0.0D+00, TWO=2.0D+00)
C
      DO II = 1,NSHLA
         MINI = KMINA(II)
         MAXI = KMAXA(II)
         LOCI = KLOCA(II) - MINI
         DO JJ = 1, NSHLB
            MINJ = KMINB(JJ)
            MAXJ = KMAXB(JJ)
            LOCJ = KLOCB(JJ) - MINJ
            DO I = MINI,MAXI
               MU = LOCI + I
               DO J = MINJ,MAXJ
                  NU = LOCJ + J
                  TIX = ZERO
                  TIY = ZERO
                  TIZ = ZERO
                  TJX = ZERO
                  TJY = ZERO
                  TJZ = ZERO
                  IF (I.EQ.2) THEN
                    TIY = -EPS(MU,NU)*SMAT(MU+2,NU) -
     *                     EPT(MU,NU)*TMAT(MU+2,NU)
                    TIZ =  EPS(MU,NU)*SMAT(MU+1,NU) +
     *                     EPT(MU,NU)*TMAT(MU+1,NU)
                  END IF
                  IF (J.EQ.2) THEN
                    TJY = -EPS(MU,NU)*SMAT(MU,NU+2) -
     *                     EPT(MU,NU)*TMAT(MU,NU+2)
                    TJZ =  EPS(MU,NU)*SMAT(MU,NU+1) +
     *                     EPT(MU,NU)*TMAT(MU,NU+1)
                  END IF
                  IF (I.EQ.3) THEN
                    TIX =  EPS(MU,NU)*SMAT(MU+1,NU) +
     *                     EPT(MU,NU)*TMAT(MU+1,NU)
                    TIZ = -EPS(MU,NU)*SMAT(MU-1,NU) -
     *                     EPT(MU,NU)*TMAT(MU-1,NU)
                  END IF
                  IF (J.EQ.3) THEN
                    TJX =  EPS(MU,NU)*SMAT(MU,NU+1) +
     *                     EPT(MU,NU)*TMAT(MU,NU+1)
                    TJZ = -EPS(MU,NU)*SMAT(MU,NU-1) -
     *                     EPT(MU,NU)*TMAT(MU,NU-1)
                  END IF
                  IF (I.EQ.4) THEN
                    TIX = -EPS(MU,NU)*SMAT(MU-1,NU) -
     *                     EPT(MU,NU)*TMAT(MU-1,NU)
                    TIY =  EPS(MU,NU)*SMAT(MU-2,NU) +
     *                     EPT(MU,NU)*TMAT(MU-2,NU)
                  END IF
                  IF (J.EQ.4) THEN
                    TJX = -EPS(MU,NU)*SMAT(MU,NU-1) -
     *                     EPT(MU,NU)*TMAT(MU,NU-1)
                    TJY =  EPS(MU,NU)*SMAT(MU,NU-2) +
     *                     EPT(MU,NU)*TMAT(MU,NU-2)
                  END IF
                  IF (I.EQ.5) THEN
                    TIY = -TWO*EPS(MU,NU)*SMAT(MU+4,NU) -
     *                     TWO*EPT(MU,NU)*TMAT(MU+4,NU)
                    TIZ =  TWO*EPS(MU,NU)*SMAT(MU+3,NU) +
     *                     TWO*EPT(MU,NU)*TMAT(MU+3,NU)
                  END IF
                  IF (J.EQ.5) THEN
                    TJY = -TWO*EPS(MU,NU)*SMAT(MU,NU+4) -
     *                     TWO*EPT(MU,NU)*TMAT(MU,NU+4)
                    TJZ =  TWO*EPS(MU,NU)*SMAT(MU,NU+3) +
     *                     TWO*EPT(MU,NU)*TMAT(MU,NU+3)
                  END IF
                  IF (I.EQ.6) THEN
                    TIX =  TWO*EPS(MU,NU)*SMAT(MU+4,NU) +
     *                     TWO*EPT(MU,NU)*TMAT(MU+4,NU)
                    TIZ = -TWO*EPS(MU,NU)*SMAT(MU+2,NU) -
     *                     TWO*EPT(MU,NU)*TMAT(MU+2,NU)
                  END IF
                  IF (J.EQ.6) THEN
                    TJX =  TWO*EPS(MU,NU)*SMAT(MU,NU+4) +
     *                     TWO*EPT(MU,NU)*TMAT(MU,NU+4)
                    TJZ = -TWO*EPS(MU,NU)*SMAT(MU,NU+2) -
     *                     TWO*EPT(MU,NU)*TMAT(MU,NU+2)
                  END IF
                  IF (I.EQ.7) THEN
                    TIX = -TWO*EPS(MU,NU)*SMAT(MU+3,NU) -
     *                     TWO*EPT(MU,NU)*TMAT(MU+3,NU)
                    TIY =  TWO*EPS(MU,NU)*SMAT(MU+2,NU) +
     *                     TWO*EPT(MU,NU)*TMAT(MU+2,NU)
                  END IF
                  IF (J.EQ.7) THEN
                    TJX = -TWO*EPS(MU,NU)*SMAT(MU,NU+3) -
     *                     TWO*EPT(MU,NU)*TMAT(MU,NU+3)
                    TJY =  TWO*EPS(MU,NU)*SMAT(MU,NU+2) +
     *                     TWO*EPT(MU,NU)*TMAT(MU,NU+2)
                  END IF
                  IF (I.EQ.8) THEN
                    TIX =  EPS(MU,NU)*SMAT(MU+1,NU) +
     *                     EPT(MU,NU)*TMAT(MU+1,NU)
                    TIY = -EPS(MU,NU)*SMAT(MU+2,NU) -
     *                     EPT(MU,NU)*TMAT(MU+2,NU)
                    TIZ =  EPS(MU,NU)*(SMAT(MU-2,NU) - SMAT(MU-3,NU)) +  
     *                     EPT(MU,NU)*(TMAT(MU-2,NU) - TMAT(MU-3,NU))
                  END IF
                  IF (J.EQ.8) THEN
                    TJX =  EPS(MU,NU)*SMAT(MU,NU+1) +
     *                     EPT(MU,NU)*TMAT(MU,NU+1)
                    TJY =  EPS(MU,NU)*SMAT(MU,NU+2) +
     *                     EPT(MU,NU)*TMAT(MU,NU+2)
                    TJZ =  EPS(MU,NU)*(SMAT(MU,NU-2) - SMAT(MU,NU-3)) +  
     *                     EPT(MU,NU)*(TMAT(MU,NU-2) - TMAT(MU,NU-3))
                  END IF
                  IF (I.EQ.9) THEN
                    TIX = -EPS(MU,NU)*SMAT(MU-1,NU) -
     *                     EPT(MU,NU)*TMAT(MU-1,NU)
                    TIY =  EPS(MU,NU)*(SMAT(MU-4,NU) - SMAT(MU-2,NU)) +  
     *                     EPT(MU,NU)*(TMAT(MU-4,NU) - TMAT(MU-2,NU))
                    TIZ =  EPS(MU,NU)*SMAT(MU+1,NU) +
     *                     EPT(MU,NU)*TMAT(MU+1,NU)
                  END IF
                  IF (J.EQ.9) THEN
                    TJX = -EPS(MU,NU)*SMAT(MU,NU-1) -
     *                     EPT(MU,NU)*TMAT(MU,NU-1)
                    TJY =  EPS(MU,NU)*(SMAT(MU,NU-4) - SMAT(MU,NU-2)) +  
     *                     EPT(MU,NU)*(TMAT(MU,NU-4) - TMAT(MU,NU-2))
                    TJZ =  EPS(MU,NU)*SMAT(MU,NU+1) +
     *                     EPT(MU,NU)*TMAT(MU,NU+1)
                  END IF
                  IF (I.EQ.10) THEN
                    TIX =  EPS(MU,NU)*(SMAT(MU-3,NU) - SMAT(MU-4,NU)) +  
     *                     EPT(MU,NU)*(TMAT(MU-3,NU) - TMAT(MU-4,NU))
                    TIY =  EPS(MU,NU)*SMAT(MU-2,NU) +
     *                     EPT(MU,NU)*TMAT(MU-2,NU)
                    TIZ = -EPS(MU,NU)*SMAT(MU-1,NU) -
     *                     EPT(MU,NU)*TMAT(MU-1,NU)
                  END IF
                  IF (J.EQ.10) THEN
                    TJX =  EPS(MU,NU)*(SMAT(MU,NU-3) - SMAT(MU,NU-4)) +  
     *                     EPT(MU,NU)*(TMAT(MU,NU-3) - TMAT(MU,NU-4))
                    TJY =  EPS(MU,NU)*SMAT(MU,NU-2) +
     *                     EPT(MU,NU)*TMAT(MU,NU-2)
                    TJZ = -EPS(MU,NU)*SMAT(MU,NU-1) -
     *                     EPT(MU,NU)*TMAT(MU,NU-1)
                  END IF
                  IF (I.GT.0) THEN
                     ATORQ(1,IFRG) = ATORQ(1,IFRG) + TIX
                     ATORQ(2,IFRG) = ATORQ(2,IFRG) + TIY
                     ATORQ(3,IFRG) = ATORQ(3,IFRG) + TIZ
                  END IF
                  ATORQ(1,JFRG) = ATORQ(1,JFRG) + TJX
                  ATORQ(2,JFRG) = ATORQ(2,JFRG) + TJY
                  ATORQ(3,JFRG) = ATORQ(3,JFRG) + TJZ
               END DO
            END DO
         END DO
      END DO
C 
      RETURN
      END
