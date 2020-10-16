C  7 AUG 02 - JMS - HIGHER PERF. SP GRAD. (JKDG80 SPLIT, NEW GRDPER)
C 16 NOV 01 - JMS - CLARIFYING OF TOLERANCES
C  8 OCT 01 - JMS - ADDITIONAL MODIFICATIONS, AND INCREASE ACCURACY
C  1 AUG 01 - JMS - COMPLETE REWRITE OF ROTATED AXIS GRADIENT CODE.
C                   HISTORY BELOW IS KEPT MAINLY FOR HISTORICAL REASONS
C  6 JAN 95 - MK  - JKDG80 CORRECTED FOR MXGSH=30
C 12 NOV 94 - MWS - REMOVE FTNCHEK WARNINGS
C 20 SEP 94 - CS  - JKDG80: INITIALIZE CSS,ETC TO ZERO
C 10 AUG 94 - MWS - INCREASE NUMBER OF DAF RECORDS
C 20 JUL 94 - BMB - FINISHED CONVERSION FROM HONDO8
C
C THIS FILE CONTAINS THE CODE USED ONLY BY THE POPLE PACKAGE.
C
C*MODULE GRD2B   *DECK JKDG80
      SUBROUTINE JKDG80(DABMAX,INEW,JNEW,KNEW,LNEW,
     2                  JTYPE,IAT,JAT,KAT,LAT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     ---- JKDG80 DRIVER FOR 2-E INTEGRAL DERIVATIVES FOR SP BASES ----
C     FROM GAUSSIAN-80, MICHEL DUPUIS, 6/6/85.
C     FROM HONDO8, BRETT BODE, 7/15/94.
C     REWRITTEN USING LOOPS AND INLINING FPPPP, JOSE SIERRA, 7/2001
C
      PARAMETER (MXGTOT=5000, MXSH=1000, MXATM=500)
      PARAMETER (MXGSH=30, MXG2=MXGSH*MXGSH)
C
      LOGICAL OUT,DBG
      COMMON /DSHLT / RTOL,DTOL,VTOL1,VTOL2,VTOLS,OUT,DBG
      COMMON /GRAD  / FXYZ(3,MXATM)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NX,NE,NA,NB,ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     2                CF(MXGTOT),CG(MXGTOT),
     3                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     4                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
C
C     ==================================================================
C     NOTE THAT GAMGEN IS THE PLACE THAT SETS COMMON BLOCK /INTACC/
C     THIS GROUP OF STATEMENTS IS INTERRELATED FOR ROUTINES
C     GAMGEN, GENR70 AND SP0S1S IN INT2B, AND JKDG80, GRDG80 AND GRDPER
C     IN GRD2B. IF YOU CHANGE THESE, CHANGE ALL SIX PLACES EQUIVALENTLY.
C     GAMGEN HAS COMMENTS EXPLAINING THIS IN THE MOST DETAIL.
C
      PARAMETER (M3R=401)
      COMMON /INTACC/ EI1,EI2,CUF,RFI,CUG,RGI,CUX,R(3,0:5,0:M3R)
C     ==================================================================
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
      COMMON /GSPG80/ E(0:3,0:3,0:3,0:3)
C
CJMS  LABELLED COMMON JMSG80 IS ONLY USED IN THIS MODULE GRD2B.
C
      LOGICAL IJSAME,KLSAME,IKSMJL
      COMMON /JMSG80/ PI4,PIF,P12(3,3),R12,P34(3,3),R34,FCS(4,3),E34MAX,
     2                TX21(MXG2),TH21(MXG2),TY01(MXG2),TY1Y(MXG2),
     3                TF12(MXG2),P0(3,MXG2),TE12(MXG2),CSMAB(MXG2),
     4                TX43(MXG2),TH43(MXG2),TY03(MXG2),TY3Y(MXG2),
     5                TF34(MXG2),Q0(3,MXG2),TE34(MXG2),CSMCD(MXG2),
     6                IGBEG,JGBEG,KGBEG,LGBEG,IGEND,JGEND,KGEND,LGEND,
     7                IJSAME,KLSAME,IKSMJL
C
      PARAMETER (ZER=0.0D+00, PT5=0.5D+00, ONE=1.0D+00)
CC    PARAMETER (PIDIV4=0.785398163397448309615660845819D+00)
      PARAMETER (PIDIV4=0.785398163397448D+00)
      PARAMETER (PITO52=0.349868366552497D+02)
C
CJMS  THRESHOLD VALUES TO USE 'GRDPER' INSTEAD OF THE USUAL 'GRDG80'
C
      LOGICAL   CONCPT
      DIMENSION LIMG80(3:6)
      DATA      LIMG80/ 8, 8,20,20/
C
CC    PIDIV4= ATAN(ONE)
      PI4= PIDIV4
CC    PI = PI4+PI4+PI4+PI4
CC    PITO52=(PI +PI )*PI * SQRT(PI )
      PIF= PITO52
C
C     ----- TWO ELECTRON CONTRIBUTION TO THE GRADIENT -----
C
      IJSAME= INEW.EQ.JNEW
      KLSAME= KNEW.EQ.LNEW
      IKSMJL=(INEW.EQ.KNEW).AND.(JNEW.EQ.LNEW)
      IGBEG= KSTART(INEW)
      JGBEG= KSTART(JNEW)
      KGBEG= KSTART(KNEW)
      LGBEG= KSTART(LNEW)
      IGEND= IGBEG+KNG(INEW)-1
      JGEND= JGBEG+KNG(JNEW)-1
      KGEND= KGBEG+KNG(KNEW)-1
      LGEND= LGBEG+KNG(LNEW)-1
C
      MFQ= JTYPE-1
      IF(JTYPE.LE.3) MFQ= JTYPE
C
      R12= ZER
      R34= ZER
      DO 110 M=1,3
         P12(M,1)= C(M,IAT)
         P12(M,2)= C(M,JAT)
         P12(M,3)= P12(M,2)-P12(M,1)
      R12= R12+P12(M,3)*P12(M,3)
         P34(M,1)= C(M,KAT)
         P34(M,2)= C(M,LAT)
         P34(M,3)= P34(M,2)-P34(M,1)
  110 R34= R34+P34(M,3)*P34(M,3)
C
C     ----- PRELIMINARY SCAN OF MAGNITUDE -----
C
      LK= 0
      E34MAX=ZER
      K0= 1
      DO 125 KG=KGBEG,KGEND
         X03= EX(KG)
         CSUMC= ABS(CS(KG))+ ABS(CP(KG))
         LGND=LGEND
         IF(KLSAME) LGND=KG
         KL=K0
         DO 120 LG=LGBEG,LGND
            X04= EX(LG)
            X34= X03+X04
            X43= ONE/X34
            H43= PT5*X43
            Y03= X03*X43
            Y04= ONE-Y03
            Y34= Y03*X04
            TE34(KL)= ZER
            R34Y34= R34*Y34
            IF(R34Y34.GT.CUX) GO TO 120
            E34= X43* EXP(-R34Y34)
            IF(KLSAME.AND.(KG.NE.LG)) E34=E34+E34
            TE34(KL)= E34
            E34= E34*CSUMC*( ABS(CS(LG))+ ABS(CP(LG)))
            IF(E34MAX.LT.E34) E34MAX=E34
            CSMCD(KL)=E34*E34
            TX43(KL)= X43
            TH43(KL)= H43
            TY03(KL)= Y03
            TY3Y(KL)= Y03*Y04
            TF34(KL)= Y34+Y34
            DO 115 M=1,3
  115       Q0(M,KL)= P34(M,1)+P34(M,3)*Y04
C
            LK=LK+1
C
  120    KL=KL+1
  125 K0=K0+MXGSH
      IF(E34MAX*DABMAX.LT.VTOL1) GO TO 999
C
      JI= 0
      I0= 1
      DO 940 IG=IGBEG,IGEND
         X01= EX(IG)
         CSUMA= DABMAX*( ABS(CS(IG))+ ABS(CP(IG)))
         JGND=JGEND
         IF(IJSAME) JGND=IG
         IJ=I0
         DO 930 JG=JGBEG,JGND
            X02= EX(JG)
            X12= X01+X02
            X21= ONE/X12
            H21= PT5*X21
            Y01= X01*X21
            Y02= ONE-Y01
            Y12= Y01*X02
            TE12(IJ)= ZER
            R12Y12= R12*Y12
            IF(R12Y12.GT.CUX) GO TO 930
            E12= PIF*X21* EXP(-R12Y12)
            IF(IJSAME.AND.(IG.NE.JG)) E12=E12+E12
            CAB= E12*CSUMA*( ABS(CS(JG))+ ABS(CP(JG)))
            IF(E34MAX*CAB.LT.VTOL2) GO TO 930
            TE12(IJ)= E12
            CSMAB(IJ)=CAB*CAB
            TX21(IJ)= X21
            TH21(IJ)= H21
            TY01(IJ)= Y01
            TY1Y(IJ)= Y01*Y02
            TF12(IJ)= Y12+Y12
            DO 152 M=1,3
  152       P0(M,IJ)= P12(M,1)+P12(M,3)*Y02
C
            JI=JI+1
C
  930    IJ=IJ+1
  940 I0=I0+MXGSH
      IF(JI.EQ.0) GO TO 999
C
      DO 135 N=1,3
         DO 135 M=1,4
  135 FCS(M,N)= ZER
C
      CONCPT=.TRUE.
      IF(JTYPE.LT.3) THEN
C FOR JTYPE= 1 OR 2, USE THE 'GRDG80' SUBROUTINE
      ELSEIF(JTYPE.EQ.3) THEN
         IF(JI*LK.GE.LIMG80(JTYPE)) CONCPT=.FALSE.
      ELSEIF(JTYPE.EQ.4) THEN
         IF(JI*LK.GE.LIMG80(JTYPE)) CONCPT=.FALSE.
      ELSEIF(JTYPE.EQ.5) THEN
         IF(JI*LK.GE.LIMG80(JTYPE)) CONCPT=.FALSE.
      ELSEIF(JTYPE.EQ.6) THEN
         IF(JI*LK.GE.LIMG80(JTYPE)) CONCPT=.FALSE.
      ENDIF
      IF(CONCPT) THEN
         CALL GRDG80(E,JTYPE,MFQ)
      ELSE
         CALL GRDPER(E,JTYPE,MFQ)
      ENDIF
C
C SUMMATION OF THE CONTRIBUTIONS FROM THE SHELLS
C
      DO 990 M=1,3
         FCS(4,M)=-(FCS(1,M)+FCS(2,M)+FCS(3,M))
         FXYZ(M,IAT)= FXYZ(M,IAT)+FCS(1,M)
         FXYZ(M,JAT)= FXYZ(M,JAT)+FCS(2,M)
         FXYZ(M,KAT)= FXYZ(M,KAT)+FCS(3,M)
         FXYZ(M,LAT)= FXYZ(M,LAT)+FCS(4,M)
  990 CONTINUE
      IF(OUT) THEN
         WRITE(IW,9010) INEW,JNEW,KNEW,LNEW,IAT,JAT,KAT,LAT,
     2                 ((FCS(M,N),M=1,4),N=1,3)
         WRITE(IW,9020)(FXYZ(M,IAT),FXYZ(M,JAT),
     2                  FXYZ(M,KAT),FXYZ(M,LAT),M=1,3)
      ENDIF
  999 CONTINUE
C
      RETURN
 9010 FORMAT(' SHELLS AND CENTERS ',8I4/' ---- PARTIAL CONTRIBUTION'/
     2       4D15.7/4D15.7/4D15.7)
 9020 FORMAT(' ---- GRADIENT UP TO THIS POINT'/
     2       4D15.7/4D15.7/4D15.7)
      END
C*MODULE GRD2B   *DECK GRDG80
      SUBROUTINE GRDG80(E,JTYPE,MFQ)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION E(0:3,0:3,0:3,0:3)
C
C     ----- GRDG80 DRIVES 2-E INTEGRAL DERIVATIVES FOR SP BASES -----
C     FROM GAUSSIAN-80, MICHEL DUPUIS, 6/6/85.
C     FROM HONDO8, BRETT BODE, 7/15/94.
C     REWRITTEN USING LOOPS AND INLINING FPPPP, JOSE SIERRA, 7/2001
C
      PARAMETER (MXGTOT=5000, MXSH=1000)
      PARAMETER (MXGSH=30, MXG2=MXGSH*MXGSH)
C
      LOGICAL OUT,DBG
      COMMON /DSHLT / RTOL,DTOL,VTOL1,VTOL2,VTOLS,OUT,DBG
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     2                CF(MXGTOT),CG(MXGTOT),
     3                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     4                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /INDD80/ LA,LB,LC,LD
C
C     ==================================================================
C     NOTE THAT GAMGEN IS THE PLACE THAT SETS COMMON BLOCK /INTACC/
C     THIS GROUP OF STATEMENTS IS INTERRELATED FOR ROUTINES
C     GAMGEN, GENR70 AND SP0S1S IN INT2B, AND JKDG80, GRDG80 AND GRDPER
C     IN GRD2B. IF YOU CHANGE THESE, CHANGE ALL SIX PLACES EQUIVALENTLY.
C     GAMGEN HAS COMMENTS EXPLAINING THIS IN THE MOST DETAIL.
C
      PARAMETER (M3R=401)
      COMMON /INTACC/ EI1,EI2,CUF,RFI,CUG,RGI,CUX,R(3,0:5,0:M3R)
C     ==================================================================
C
CJMS  LABELLED COMMON JMSG80 IS ONLY USED IN THIS MODULE GRD2B.
C
      LOGICAL IJSAME,KLSAME,IKSMJL
      COMMON /JMSG80/ PI4,PIF,P12(3,3),R12,P34(3,3),R34,FCS(4,3),E34MAX,
     2                TX21(MXG2),TH21(MXG2),TY01(MXG2),TY1Y(MXG2),
     3                TF12(MXG2),P0(3,MXG2),TE12(MXG2),CSMAB(MXG2),
     4                TX43(MXG2),TH43(MXG2),TY03(MXG2),TY3Y(MXG2),
     5                TF34(MXG2),Q0(3,MXG2),TE34(MXG2),CSMCD(MXG2),
     6                IGBEG,JGBEG,KGBEG,LGBEG,IGEND,JGEND,KGEND,LGEND,
     7                IJSAME,KLSAME,IKSMJL
C
      DIMENSION       G(0:3,0:3,0:3,0:3),V(0:3,0:3,0:3,0:3),Z(3),
     2                PIJM0(3),PIJ0N(3),PIJ(3,3),
     3                PKLM0(3),PKL0N(3),PKL(3,3),
     4                P1(3),P2(3,3),FQ(0:5),VEA(12),WEA(12),
     5                VES(9),WES(6),WS(8),WP(8)
C
      PARAMETER (ZER=0.0D+00, PT5=0.5D+00, ONE=1.0D+00, TWO=2.0D+00)
C
      DO 130 M=0,5
  130 FQ(M)= ZER
      DO 140 N=1,12
         VEA(N)= ZER
         WEA(N)= ZER
  140 CONTINUE
C
      CSP= ZER
      CPP= ZER
      DO 145 N=1,3
         DO 145 M=1,3
  145 PIJ(M,N)= ZER
C
C LOOP OVER THE UNCONTRACTED GAUSSIANS WITHIN THE SHELLS
C
C ... GAUSSIANS AT CENTER A.
C
C     ----- I PRIM -----
C
      I0= 1
      DO 940 IG=IGBEG,IGEND
         X01= EX(IG)
         CSA= CS(IG)
         CPA= CP(IG)
C
C ... GAUSSIANS AT CENTER B.
C
C     ----- J PRIM -----
C
         JGND=JGEND
         IF(IJSAME) JGND=IG
         IJ=I0
         DO 930 JG=JGBEG,JGND
            IF(TE12(IJ).EQ.ZER) GO TO 930
            X02= EX(JG)
            CSB= CS(JG)
            CPB= CP(JG)
            X12= X01+X02
            X21= TX21(IJ)
            H21= TH21(IJ)
            Y01= TY01(IJ)
            F12= TF12(IJ)
            X24= X12+X12
            CSB= CSB*TE12(IJ)
            CPB= CPB*TE12(IJ)
            CSS= CSA*CSB
            CPS= CPA*CSB
            CSP= CSA*CPB
            CPP= CPA*CPB
            DO 152 M=1,3
               PIJM0(M)= P0(M,IJ)-P12(M,1)
               PIJ0N(M)= P0(M,IJ)-P12(M,2)
  152       CONTINUE
            IF(LA.NE.1 .AND. LB.NE.1) THEN
               CSP= CSA*CPB
               CPP= CPA*CPB
               DO 156 N=1,3
                  DO 154 M=1,N
  154             PIJ(M,N)= PIJM0(M)*PIJ0N(N)
  156          PIJ(N,N)= PIJ(N,N)+H21
               PIJ(2,1)= PIJ(1,2)
               PIJ(3,1)= PIJ(1,3)
               PIJ(3,2)= PIJ(2,3)
            ENDIF
C
            VE0SUM= ZER
            DO 158 N=1,9
  158       VES(N)= ZER
C
C ... GAUSSIANS AT CENTER C.
C
C     ----- K PRIM -----
C
            KGND=KGEND
            IF(IKSMJL) KGND=IG
            K0= 1
            DO 920 KG=KGBEG,KGND
               X03= EX(KG)
               CSC= CS(KG)
               CPC= CP(KG)
               CSSS= CSS*CSC
               CPSS= CPS*CSC
               CSPS= CSP*CSC
               CPPS= CPP*CSC
               CSSP= CSS*CPC
               CPSP= CPS*CPC
               CSPP= CSP*CPC
               CPPP= CPP*CPC
C
C ... GAUSSIANS AT CENTER D.
C
C     ----- L PRIM -----
C
               LGND=LGEND
               IF(KLSAME) LGND=KG
               IF(IKSMJL.AND.(IG.EQ.KG)) LGND=JG
               KL=K0
               DO 910 LG=LGBEG,LGND
                  IF(TE34(KL).EQ.ZER) GO TO 910
                  X04= EX(LG)
                  CSD= CS(LG)
                  CPD= CP(LG)
                  X34= X03+X04
                  X14= X12+X34
                  X41= ONE/X14
                  VTEST= CSMAB(IJ)*CSMCD(KL)*X41
                  IF(VTEST.LT.VTOLS) GO TO 910
                  H41= PT5*X41
                  Z02= X12*X41
                  Z04= ONE-Z02
C
                  X43= TX43(KL)
                  H43= TH43(KL)
                  Y03= TY03(KL)
C
CJMS              RHO= X12*X34*X41
                  RHO= X12*Z04
                  XVA= ZER
                  DO 160 M=1,3
                     T00 = P0(M,IJ)-Q0(M,KL)
                     P1(M)=T00*Z04
  160             XVA= XVA+T00*T00
                  XVA= XVA*RHO
                  IF(XVA.LT.CUG) THEN
                     TMP= XVA*RGI
                     N  = INT(TMP)
                     C02= TMP-N
                     TMP= C02*(ONE-C02)
                     C03=-TMP*(TWO-C02)
                     C04=-TMP*(ONE+C02)
                     DO 165 M=0,MFQ
                        FQ(M)= R(1,M,N)+C02*R(2,M,N)+C03*R(3,M,N)+
     2                         C04*R(3,M,N+1)
  165                CONTINUE
                  ELSE
                     IF(VTEST.LT.VTOLS*XVA) GO TO 910
                     XIN= ONE/XVA
                     FQ(0)= SQRT(PI4*XIN)
                     TMP= PT5*XIN
                     DO 170 M=1,MFQ
                        FQ(M)= FQ(M-1)*TMP
  170                TMP= TMP+XIN
                  ENDIF
C
                  F34= TF34(KL)
                  E34= TE34(KL)* SQRT(X41)
                  IF(IKSMJL.AND.(IJ.NE.KL)) E34=E34+E34
                  CSD= CSD*E34
                  CPD= CPD*E34
                  WS(1)= CSSS*CSD
                  WS(2)= CPSS*CSD
                  ES1= E(0,0,0,0)*WS(1)
C
                  IF(JTYPE.LT.3) THEN
                     IF(JTYPE.EQ.1) THEN
C
C SPECIAL (SS,SS) SECTION
C
                        DO 175 I=1,6
  175                   WES(I)= ZER
                        VE0= ES1*FQ(0)
                        VE1= ES1*FQ(1)
                     ELSEIF(JTYPE.EQ.2) THEN
C
C SPECIAL (PS,SS) SECTION
C
                        T01=-Z04*FQ(1)
                        ES2= ZER
                        DO 180 I=1,3
                           TMP= E(I,0,0,0)*WS(2)
                           VES(  I)=VES(  I)+TMP*FQ(0)
                           WES(  I)=ZER
                           WES(3+I)=TMP*T01
                           ES1= ES1+TMP*PIJM0(I)
                           ES2= ES2+TMP*P1(I)
  180                   CONTINUE
                        VE0= ES1*FQ(0)-ES2*FQ(1)
                        VE1= ES1*FQ(1)-ES2*FQ(2)
                     ENDIF
                     VE0SUM= VE0SUM+VE0
                     FE0= F34*VE0
                     FE1= X24*VE1
                     GO TO 900
                  ENDIF
C
                  DO 185 N=1,3
                     PKLM0(N)= Q0(N,KL)-P34(N,1)
                     PKL0N(N)= Q0(N,KL)-P34(N,2)
                     DO 185 M=1,N
  185             P2(M,N)= P1(M)*P1(N)
                  P2(2,1)= P2(1,2)
                  P2(3,1)= P2(1,3)
                  P2(3,2)= P2(2,3)
C
                  IF(LC.NE.1 .AND. LD.NE.1) THEN
                     DO 195 N=1,3
                        DO 190 M=1,N
  190                   PKL(M,N)= PKLM0(M)*PKL0N(N)
  195                PKL(N,N)= PKL(N,N)+H43
                     PKL(2,1)= PKL(1,2)
                     PKL(3,1)= PKL(1,3)
                     PKL(3,2)= PKL(2,3)
                  ENDIF
                  XX1=-X12*X43
                  XX2= XX1*XX1
                  XXI=-X21*X34
                  HF2= H41*FQ(1)
                  IF(JTYPE.NE.4) THEN
                     WS(3)= CSPS*CSD
                     WS(4)= CPPS*CSD
                  ENDIF
                  IF(JTYPE.GT.3) THEN
                     XF2= XX1*FQ(1)
                     XF3= XX1*FQ(2)
                     XF4= XX1*FQ(3)
                     WS(5)= CSSP*CSD
                     WS(6)= CPSP*CSD
                     IF(JTYPE.GT.4) THEN
                        WS(7)= CSPP*CSD
                        WS(8)= CPPP*CSD
                        IF(JTYPE.GT.5) THEN
                           WP(1)= CSSS*CPD
                           WP(2)= CPSS*CPD
                           WP(3)= CSPS*CPD
                           WP(4)= CPPS*CPD
                           WP(5)= CSSP*CPD
                           WP(6)= CPSP*CPD
                           WP(7)= CSPP*CPD
                           WP(8)= CPPP*CPD
                        ENDIF
                     ENDIF
                  ENDIF
C
C BEGINING OF THE TWO PASSES THROUGH THE INTEGRAL CALCULATIONS
C
                  DO 800 ICP=1,2
                     HF1= HF2
                     HF2= H41*FQ(2)
                     IF(JTYPE.GT.3) THEN
                        XF1= XF2
                        XF2= XF3
                        XF3= XF4
                        IF(JTYPE.GT.4) XF4= XX1*FQ(4)
                     ENDIF
C
C INTEGRAL EVALUATION SECTION
C
C (SS,SS) SECTION
C
CC    G(0,0,0,0)= FQ(0)
CC    V(0,0,0,0)= FQ(0)
      VE0= ES1*FQ(0)
C
C (PS,SS) SECTION
C
      T20= WS(2)*FQ(0)
      DO 210 I=1,3
         G(I,0,0,0)=-P1(I)*FQ(1)
         V(I,0,0,0)= G(I,0,0,0)+PIJM0(I)*FQ(0)
         VEA(  I)  = E(I,0,0,0)*T20
  210 CONTINUE
      VE0= VE0+WS(2)*(
     1V(1,0,0,0)*E(1,0,0,0)+V(2,0,0,0)*E(2,0,0,0)+V(3,0,0,0)*E(3,0,0,0))
C
      IF(JTYPE.EQ.4) GO TO 400
C
C (PP,SS) + (SP,SS) SECTION
C
      T01= XXI*HF1
      DO 320 J=1,3
         V(0,J,0,0)= V(J,0,0,0)-P12(J,3)*FQ(0)
         T00= T01
         DO 310 I=J,3
            G(I,J,0,0)= T00+P2(I,J)*FQ(2)
            T= PIJ(I,J)*FQ(0)+G(I,J,0,0)
            IF(I.EQ.J) THEN
               V(I,J,0,0)=(PIJM0(J)+PIJ0N(J))*G(J,0,0,0)+T
            ELSE
               V(I,J,0,0)= PIJM0(I)*G(J,0,0,0)+PIJ0N(J)*G(I,0,0,0)+T
               G(J,I,0,0)= G(I,J,0,0)
               V(J,I,0,0)= PIJM0(J)*G(I,0,0,0)+PIJ0N(I)*G(J,0,0,0)+T
            ENDIF
  310    T00= ZER
  320 CONTINUE
      VX3=
     1V(0,1,0,0)*E(0,1,0,0)+V(0,2,0,0)*E(0,2,0,0)+V(0,3,0,0)*E(0,3,0,0)
      T30= WS(3)*FQ(0)
      VX4= ZER
      DO 340 I=1,3
         VEA(3+I)= E(0,I,0,0)*T30+WS(4)*(
     1V(1,0,0,0)*E(1,I,0,0)+V(2,0,0,0)*E(2,I,0,0)+V(3,0,0,0)*E(3,I,0,0))
         VEA(  I)= VEA(  I)+WS(4)*(
     1V(0,1,0,0)*E(I,1,0,0)+V(0,2,0,0)*E(I,2,0,0)+V(0,3,0,0)*E(I,3,0,0))
  340 VX4= VX4+
     1V(I,1,0,0)*E(I,1,0,0)+V(I,2,0,0)*E(I,2,0,0)+V(I,3,0,0)*E(I,3,0,0)
      VE0= VE0+WS(3)*VX3+WS(4)*VX4
C
      IF(JTYPE.EQ.3) GO TO 700
C
  400 CONTINUE
C
C (PS,PS) + (SS,PS) SECTION
C
      DO 410 J=1,3
         G(0,0,J,0)=-P1(J)*XF1
         V(0,0,J,0)= PKLM0(J)*FQ(0)+G(0,0,J,0)
  410 CONTINUE
      DO 430 K=1,3
         T00= HF1
         DO 420 I=K,3
            G(I,0,K,0)= T00+P2(I,K)*XF2
            V(I,0,K,0)= PIJM0(I)*V(0,0,K,0)+G(I,0,0,0)*PKLM0(K)+
     2                  G(I,0,K,0)
            IF(I.NE.K) THEN
CJMS  EQUIVALENCES BY JOSE M. SIERRA
               G(K,0,I,0)= G(I,0,K,0)
               V(K,0,I,0)= PIJM0(K)*V(0,0,I,0)+G(K,0,0,0)*PKLM0(I)+
     2                     G(I,0,K,0)
            ENDIF
  420    T00= ZER
  430 CONTINUE
      VX5=
     1V(0,0,1,0)*E(0,0,1,0)+V(0,0,2,0)*E(0,0,2,0)+V(0,0,3,0)*E(0,0,3,0)
      T50= WS(5)*FQ(0)
      VX6= ZER
      DO 440 I=1,3
         VEA(6+I)= E(0,0,I,0)*T50+WS(6)*(
     1V(1,0,0,0)*E(1,0,I,0)+V(2,0,0,0)*E(2,0,I,0)+V(3,0,0,0)*E(3,0,I,0))
         VEA(  I)= VEA(  I)+WS(6)*(
     1V(0,0,1,0)*E(I,0,1,0)+V(0,0,2,0)*E(I,0,2,0)+V(0,0,3,0)*E(I,0,3,0))
  440 VX6= VX6+
     1V(I,0,1,0)*E(I,0,1,0)+V(I,0,2,0)*E(I,0,2,0)+V(I,0,3,0)*E(I,0,3,0)
      VE0= VE0+WS(5)*VX5+WS(6)*VX6
C
      IF(JTYPE.EQ.4) GO TO 700
C
C (PP,PS) + (SP,PS) SECTION
C
      DO 510 K=1,3
         DO 510 J=1,3
  510 V(0,J,K,0)= V(0,J,0,0)*PKLM0(K)+PIJ0N(J)*G(0,0,K,0)+G(J,0,K,0)
      VX7= ZER
      DO 520 J=1,3
         WEA(6+J)= WEA(6+J)+
     1V(0,1,0,0)*E(0,1,J,0)+V(0,2,0,0)*E(0,2,J,0)+V(0,3,0,0)*E(0,3,J,0)
         WEA(3+J)= WEA(3+J)+
     1V(0,0,1,0)*E(0,J,1,0)+V(0,0,2,0)*E(0,J,2,0)+V(0,0,3,0)*E(0,J,3,0)
  520 VX7= VX7+
     1V(0,J,1,0)*E(0,J,1,0)+V(0,J,2,0)*E(0,J,2,0)+V(0,J,3,0)*E(0,J,3,0)
      VE0= VE0+WS(7)*VX7
      DO 530 N=4,9
         VEA(N)= VEA(N)+WS(7)*WEA(N)
         WEA(N)= ZER
  530 CONTINUE
      EQ0=-P1(3)*P2(2,1)*XF3
      DO 540 I=1,3
         TMP= P2(I,I)*XF3+HF2
         G(I,I,I,0)=-P1(I)*(TMP+HF2+HF2)
         DO 540 J=1,3
            IF(I.NE.J) THEN
               K=6-I-J
CJMS  EQUIVALENCES FROM FPPPP
               G(I,J,K,0)= EQ0
               G(I,K,J,0)= EQ0
CJMS  EQUIVALENCES FROM FPPPP
               EQ2=-P1(J)* TMP
               G(J,I,I,0)= EQ2
               G(I,J,I,0)= EQ2
               G(I,I,J,0)= EQ2
            ENDIF
  540 CONTINUE
      DO 550 K=1,3
         DO 550 J=1,3
            DO 550 I=1,3
  550 V(I,J,K,0)= V(I,J,0,0)*PKLM0(K)+PIJ(I,J)*G(0,0,K,0)+
     2            PIJM0(I)*G(J,0,K,0)+PIJ0N(J)*G(I,0,K,0)+G(I,J,K,0)
      VX8= ZER
      DO 560 J=1,3
         DO 560 I=1,3
            WEA(6+J)= WEA(6+J)+
     1V(I,1,0,0)*E(I,1,J,0)+V(I,2,0,0)*E(I,2,J,0)+V(I,3,0,0)*E(I,3,J,0)
            WEA(3+J)= WEA(3+J)+
     1V(I,0,1,0)*E(I,J,1,0)+V(I,0,2,0)*E(I,J,2,0)+V(I,0,3,0)*E(I,J,3,0)
            WEA(  I)= WEA(  I)+
     1V(0,J,1,0)*E(I,J,1,0)+V(0,J,2,0)*E(I,J,2,0)+V(0,J,3,0)*E(I,J,3,0)
  560 VX8= VX8+
     1V(I,J,1,0)*E(I,J,1,0)+V(I,J,2,0)*E(I,J,2,0)+V(I,J,3,0)*E(I,J,3,0)
      VE0= VE0+WS(8)*VX8
      DO 570 N=1,9
         VEA(N)= VEA(N)+WS(8)*WEA(N)
         WEA(N)= ZER
  570 CONTINUE
C
      IF(JTYPE.EQ.5) GO TO 700
C
C (PP,PP), (PP,SP), (PS,PP), (SP,PP), (SS,PP), (SP,SP), (PS,SP) AND
C (SS,SP) SECTION
C
      V(0,0,0,1)= V(0,0,1,0)-P34(1,3)*FQ(0)
      V(0,0,0,2)= V(0,0,2,0)-P34(2,3)*FQ(0)
      V(0,0,0,3)= V(0,0,3,0)-P34(3,3)*FQ(0)
      VE0= VE0+WP(1)*(
     1V(0,0,0,1)*E(0,0,0,1)+V(0,0,0,2)*E(0,0,0,2)+V(0,0,0,3)*E(0,0,0,3))
      DO 605 L=1,3
         DO 605 I=1,3
  605 V(I,0,0,L)= V(I,0,0,0)*PKL0N(L)+PIJM0(I)*G(0,0,L,0)+G(I,0,L,0)
      T10= WP(1)*FQ(0)
      VXE= ZER
      DO 610 L=1,3
         VEA(9+L)= E(0,0,0,L)*T10+WP(2)*(
     1V(1,0,0,0)*E(1,0,0,L)+V(2,0,0,0)*E(2,0,0,L)+V(3,0,0,0)*E(3,0,0,L))
         VEA(  L)= VEA(  L)+WP(2)*(
     1V(0,0,0,1)*E(L,0,0,1)+V(0,0,0,2)*E(L,0,0,2)+V(0,0,0,3)*E(L,0,0,3))
  610 VXE= VXE+
     1V(L,0,0,1)*E(L,0,0,1)+V(L,0,0,2)*E(L,0,0,2)+V(L,0,0,3)*E(L,0,0,3)
      VE0= VE0+WP(2)*VXE
      DO 615 L=1,3
         DO 615 J=1,3
  615 V(0,J,0,L)= V(0,J,0,0)*PKL0N(L)+PIJ0N(J)*G(0,0,L,0)+G(J,0,L,0)
      VXE= ZER
      DO 620 J=1,3
         VEA(9+J)= VEA(9+J)+WP(3)*(
     1V(0,1,0,0)*E(0,1,0,J)+V(0,2,0,0)*E(0,2,0,J)+V(0,3,0,0)*E(0,3,0,J))
         VEA(3+J)= VEA(3+J)+WP(3)*(
     1V(0,0,0,1)*E(0,J,0,1)+V(0,0,0,2)*E(0,J,0,2)+V(0,0,0,3)*E(0,J,0,3))
  620 VXE= VXE+
     1V(0,J,0,1)*E(0,J,0,1)+V(0,J,0,2)*E(0,J,0,2)+V(0,J,0,3)*E(0,J,0,3)
      VE0= VE0+WP(3)*VXE
      DO 625 L=1,3
         DO 625 J=1,3
            DO 625 I=1,3
  625 V(I,J,0,L)= V(I,J,L,0)-V(I,J,0,0)*P34(L,3)
      VXE= ZER
      DO 630 J=1,3
         DO 630 I=1,3
            WEA(9+J)= WEA(9+J)+
     1V(I,1,0,0)*E(I,1,0,J)+V(I,2,0,0)*E(I,2,0,J)+V(I,3,0,0)*E(I,3,0,J)
            WEA(3+J)= WEA(3+J)+
     1V(I,0,0,1)*E(I,J,0,1)+V(I,0,0,2)*E(I,J,0,2)+V(I,0,0,3)*E(I,J,0,3)
            WEA(  I)= WEA(  I)+
     1V(0,J,0,1)*E(I,J,0,1)+V(0,J,0,2)*E(I,J,0,2)+V(0,J,0,3)*E(I,J,0,3)
  630 VXE= VXE+
     1V(I,J,0,1)*E(I,J,0,1)+V(I,J,0,2)*E(I,J,0,2)+V(I,J,0,3)*E(I,J,0,3)
      VE0= VE0+WP(4)*VXE
      DO 635 N=1,3
         VEA(  N)= VEA(  N)+WP(4)*WEA(  N)
         WEA(  N)= ZER
         VEA(3+N)= VEA(3+N)+WP(4)*WEA(3+N)
         WEA(3+N)= ZER
         VEA(9+N)= VEA(9+N)+WP(4)*WEA(9+N)
         WEA(9+N)= ZER
  635 CONTINUE
      DO 640 L=1,3
         DO 640 K=L,3
            T= PKL(K,L)*FQ(0)+G(K,L,0,0)*XX2
            IF(K.EQ.L) THEN
               V(0,0,K,L)= T+G(0,0,K,0)*(PKLM0(K)+PKL0N(K))
            ELSE
               V(0,0,K,L)= T+PKLM0(K)*G(0,0,L,0)+PKL0N(L)*G(0,0,K,0)
               V(0,0,L,K)= T+PKLM0(L)*G(0,0,K,0)+PKL0N(K)*G(0,0,L,0)
            ENDIF
  640 CONTINUE
      VXE= ZER
      DO 645 K=1,3
         VEA(9+K)= VEA(9+K)+WP(5)*(
     1V(0,0,1,0)*E(0,0,1,K)+V(0,0,2,0)*E(0,0,2,K)+V(0,0,3,0)*E(0,0,3,K))
         VEA(6+K)= VEA(6+K)+WP(5)*(
     1V(0,0,0,1)*E(0,0,K,1)+V(0,0,0,2)*E(0,0,K,2)+V(0,0,0,3)*E(0,0,K,3))
  645 VXE= VXE+
     1V(0,0,K,1)*E(0,0,K,1)+V(0,0,K,2)*E(0,0,K,2)+V(0,0,K,3)*E(0,0,K,3)
      VE0= VE0+WP(5)*VXE
C
      T00= XX1*XF4
      T01= H41*XF3
      T02= T01+T01
      T05= T01+T02+T02
      T25= H41*HF2
      T75= T25+T25+T25
      EQ0= G(3,2,1,0)*XX1
      DO 650 I=1,3
         V(I,0,I,I)= G(I,I,I,0)*XX1
         TMP= P2(I,I)* T00+T01
         V(I,I,I,I)= P2(I,I)*(TMP+T05)+T75
         DO 650 J=1,3
            IF(I.NE.J) THEN
               K=6-I-J
CJMS  EQUIVALENCES BY JOSE M. SIERRA
               V(I,0,J,K)= EQ0
               V(I,0,K,J)= EQ0
CJMS  EQUIVALENCES BY JOSE M. SIERRA
               EQ1= G(I,I,J,0)*XX1
               V(J,0,I,I)= EQ1
               V(I,0,J,I)= EQ1
               V(I,0,I,J)= EQ1
CJMS  EQUIVALENCES FROM FPPPP
               EQ2= P2(J,I)*(TMP+T02)
               EQ3= P2(J,J)* TMP+T25 +P2(I,I)*T01
               EQ4= P2(K,J)* TMP
               V(J,I,I,I)= EQ2
               V(I,J,I,I)= EQ2
               V(I,I,J,I)= EQ2
               V(I,I,I,J)= EQ2
               V(J,J,I,I)= EQ3
               V(J,I,J,I)= EQ3
               V(I,J,J,I)= EQ3
               V(J,I,I,J)= EQ3
               V(I,J,I,J)= EQ3
               V(I,I,J,J)= EQ3
               V(K,J,I,I)= EQ4
               V(K,I,J,I)= EQ4
               V(K,I,I,J)= EQ4
               V(J,K,I,I)= EQ4
               V(I,K,J,I)= EQ4
               V(I,K,I,J)= EQ4
               V(J,I,K,I)= EQ4
               V(I,J,K,I)= EQ4
               V(I,I,K,J)= EQ4
               V(J,I,I,K)= EQ4
               V(I,J,I,K)= EQ4
               V(I,I,J,K)= EQ4
            ENDIF
  650 CONTINUE
      DO 657 L=1,3
         DO 657 K=1,3
            T01= PKL(K,L)
            T02= V(0,0,K,L)
            DO 653 I=1,3
               Z(I)= V(I,0,K,L)+G(I,0,0,0)*T01+
     2               G(I,0,K,0)*PKL0N(L)+G(I,0,L,0)*PKLM0(K)
               V(I,0,K,L)= Z(I)+PIJM0(I)*T02
               V(0,I,K,L)= Z(I)+PIJ0N(I)*T02
  653       CONTINUE
            DO 655 J=1,3
               DO 655 I=1,3
                  V(I,J,K,L)=V(I,J,K,L)+ Z(I)*PIJ0N(J)+Z(J)*PIJM0(I)+
     2                       G(I,J,0,0)*T01+PIJ(I,J)*T02+
     3                       G(I,J,K,0)*PKL0N(L)+G(I,J,L,0)*PKLM0(K)
  655       CONTINUE
  657 CONTINUE
C
      VXE= ZER
      DO 660 K=1,3
         DO 660 I=1,3
            WEA(9+K)= WEA(9+K)+
     1V(I,0,1,0)*E(I,0,1,K)+V(I,0,2,0)*E(I,0,2,K)+V(I,0,3,0)*E(I,0,3,K)
            WEA(6+K)= WEA(6+K)+
     1V(I,0,0,1)*E(I,0,K,1)+V(I,0,0,2)*E(I,0,K,2)+V(I,0,0,3)*E(I,0,K,3)
            WEA(  I)= WEA(  I)+
     1V(0,0,K,1)*E(I,0,K,1)+V(0,0,K,2)*E(I,0,K,2)+V(0,0,K,3)*E(I,0,K,3)
  660 VXE= VXE+
     1V(I,0,K,1)*E(I,0,K,1)+V(I,0,K,2)*E(I,0,K,2)+V(I,0,K,3)*E(I,0,K,3)
      VE0= VE0+WP(6)*VXE
      DO 665 N=1,3
         VEA(  N)= VEA(  N)+WP(6)*WEA(  N)
         WEA(  N)= ZER
         VEA(6+N)= VEA(6+N)+WP(6)*WEA(6+N)
         WEA(6+N)= ZER
         VEA(9+N)= VEA(9+N)+WP(6)*WEA(9+N)
         WEA(9+N)= ZER
  665 CONTINUE
      VXE= ZER
      DO 675 K=1,3
         DO 675 J=1,3
            WEA(9+K)= WEA(9+K)+
     1V(0,J,1,0)*E(0,J,1,K)+V(0,J,2,0)*E(0,J,2,K)+V(0,J,3,0)*E(0,J,3,K)
            WEA(6+K)= WEA(6+K)+
     1V(0,J,0,1)*E(0,J,K,1)+V(0,J,0,2)*E(0,J,K,2)+V(0,J,0,3)*E(0,J,K,3)
            WEA(3+J)= WEA(3+J)+
     1V(0,0,K,1)*E(0,J,K,1)+V(0,0,K,2)*E(0,J,K,2)+V(0,0,K,3)*E(0,J,K,3)
  675 VXE= VXE+
     1V(0,J,K,1)*E(0,J,K,1)+V(0,J,K,2)*E(0,J,K,2)+V(0,J,K,3)*E(0,J,K,3)
      VE0= VE0+WP(7)*VXE
      DO 680 N=4,12
         VEA(N)= VEA(N)+WP(7)*WEA(N)
         WEA(N)= ZER
  680 CONTINUE
      VXE= ZER
      DO 685 L=1,3
         DO 685 K=1,3
            DO 685 J=1,3
               DO 685 I=1,3
  685 VXE= VXE+V(I,J,K,L)*E(I,J,K,L)
      VE0= VE0+WP(8)*VXE
      DO 690 K=1,3
         DO 690 J=1,3
            DO 690 I=1,3
               WEA(9+K)= WEA(9+K)+
     1V(I,J,1,0)*E(I,J,1,K)+V(I,J,2,0)*E(I,J,2,K)+V(I,J,3,0)*E(I,J,3,K)
               WEA(6+K)= WEA(6+K)+
     1V(I,J,0,1)*E(I,J,K,1)+V(I,J,0,2)*E(I,J,K,2)+V(I,J,0,3)*E(I,J,K,3)
               WEA(3+J)= WEA(3+J)+
     1V(I,0,K,1)*E(I,J,K,1)+V(I,0,K,2)*E(I,J,K,2)+V(I,0,K,3)*E(I,J,K,3)
               WEA(  I)= WEA(  I)+
     1V(0,J,K,1)*E(I,J,K,1)+V(0,J,K,2)*E(I,J,K,2)+V(0,J,K,3)*E(I,J,K,3)
  690 CONTINUE
      DO 695 N=1,12
         VEA(N)= VEA(N)+WP(8)*WEA(N)
         WEA(N)= ZER
  695 CONTINUE
C
  700                CONTINUE
                     IF(ICP.EQ.1) THEN
                        DO 710 M=1,MFQ
  710                   FQ(M-1)= FQ(M)
                        VE0SUM= VE0SUM+VE0
                        FE0= F34*VE0
                        DO 720 M=1,3
                           VES(  M)= VES(  M)+VEA(  M)
                           VES(3+M)= VES(3+M)+VEA(3+M)
                           T01 = VEA(6+M)
                           T02 = VEA(9+M)
                           WES(  M)=-T01+(T01+T02)*Y03
  720                   CONTINUE
                     ELSE
                        FE1= X24*VE0
                        DO 730 M=1,3
                           T01 = VEA(  M)+VEA(3+M)
                           T02 = VEA(6+M)+VEA(9+M)
                           WES(3+M)=-T01+(T01+T02)*Z02
  730                   CONTINUE
                     ENDIF
  800             CONTINUE
  900             CONTINUE
C
C SUMMATION OF CONTRIBUTIONS FROM THE UNCONTRACTED GAUSSIANS
C
                  DO 905 M=1,3
                     WES(  M)= WES(  M)+P34(M,3)*FE0
                     WES(3+M)= WES(3+M)-P1(M)*FE1
                     VES(6+M)= VES(6+M)+WES(3+M)
                     FCS(3,M)= FCS(3,M)-WES(3+M)*Y03+WES(  M)
  905             CONTINUE
C
  910          KL=KL+1
  920       K0=K0+MXGSH
C
            TMP= F12*VE0SUM
            DO 925 M=1,3
               T01= VES(  M)
               T02= VES(3+M)+VES(6+M)
               T01=-T01+(T01+T02)*Y01+P12(M,3)*TMP
               FCS(1,M)= FCS(1,M)+T01
               FCS(2,M)= FCS(2,M)-T01+VES(6+M)
  925       CONTINUE
C
  930    IJ=IJ+1
  940 I0=I0+MXGSH
C
      RETURN
      END
C*MODULE GRD2B   *DECK GRDPER
      SUBROUTINE GRDPER(E,JTYPE,MFQ)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION E(0:3,0:3,0:3,0:3)
C
C     ----- GRDPER DRIVES 2-E INTEGRAL DERIVATIVES FOR SP BASES -----
C     PERFORMANCE VERSION, REWRITTEN FROM GRDG80, JOSE SIERRA, 6/2002
C
      PARAMETER (MXGTOT=5000, MXSH=1000)
      PARAMETER (MXGSH=30, MXG2=MXGSH*MXGSH)
C
      LOGICAL OUT,DBG
      COMMON /DSHLT / RTOL,DTOL,VTOL1,VTOL2,VTOLS,OUT,DBG
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     2                CF(MXGTOT),CG(MXGTOT),
     3                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     4                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
C
C     ==================================================================
C     NOTE THAT GAMGEN IS THE PLACE THAT SETS COMMON BLOCK /INTACC/
C     THIS GROUP OF STATEMENTS IS INTERRELATED FOR ROUTINES
C     GAMGEN, GENR70 AND SP0S1S IN INT2B, AND JKDG80, GRDG80 AND GRDPER
C     IN GRD2B. IF YOU CHANGE THESE, CHANGE ALL SIX PLACES EQUIVALENTLY.
C     GAMGEN HAS COMMENTS EXPLAINING THIS IN THE MOST DETAIL.
C
      PARAMETER (M3R=401)
      COMMON /INTACC/ EI1,EI2,CUF,RFI,CUG,RGI,CUX,R(3,0:5,0:M3R)
C     ==================================================================
C
CJMS  LABELLED COMMON JMSG80 IS ONLY USED IN THIS MODULE GRD2B.
C
      LOGICAL IJSAME,KLSAME,IKSMJL
      COMMON /JMSG80/ PI4,PIF,P12(3,3),R12,P34(3,3),R34,FCS(4,3),E34MAX,
     2                TX21(MXG2),TH21(MXG2),TY01(MXG2),TY1Y(MXG2),
     3                TF12(MXG2),P0(3,MXG2),TE12(MXG2),CSMAB(MXG2),
     4                TX43(MXG2),TH43(MXG2),TY03(MXG2),TY3Y(MXG2),
     5                TF34(MXG2),Q0(3,MXG2),TE34(MXG2),CSMCD(MXG2),
     6                IGBEG,JGBEG,KGBEG,LGBEG,IGEND,JGEND,KGEND,LGEND,
     7                IJSAME,KLSAME,IKSMJL
C
      DIMENSION P1(3),FQ(0:5),F1(0:4),F2(0:4),WS(8),WP(8)
      DIMENSION VEA(12),VES(9),WES(6)
C
      DIMENSION T(0:20),U(0:20)
      DIMENSION P1D(3,3),P1P(3,3),P24(3,3),P3D(3,3),P3P(3,3)
      DIMENSION Q1(6,3,3),R1(6,3,3),R2(6,3,3),R3(0:9,3,3,3)
      DIMENSION T1R(6,3,3,3),T9A(0:13,3,3,3)
      DIMENSION T3R(6,3,3,3),T9B(0:20,3,3,3)
C
      DIMENSION F5X(0:12,3,3,3),F6X(0: 6,3,3,3,3)
C
      DIMENSION A31(0:3,0:3),C31(2,0:3),A32(0:3,0:3),C32(2,0:3)
      DIMENSION A41(0:3,0:3),C41(2,0:3),A42(0:3,0:3),C42(2,0:3)
      DIMENSION A33(16),FIJ00(2),A43(16),FI0K0(2)
      DIMENSION A51(0:3,3),C51(2,  3),A52(0:3,3),C52(2,  3)
      DIMENSION SEIKK0(0:3),A53(16,0:3),C53(2,0:3)
      DIMENSION SEJJK0(  3),A54(16,  3),C54(2,  3)
      DIMENSION SEKJK0(  3),A55(16,  3),C55(2,  3)
      DIMENSION A56(0:22,3,0:3),C56(2,0:3)
C
      DIMENSION A60(0:3,0:3),C60(2,0:3),A61(0:3,0:3),C61(2,0:3)
      DIMENSION A62(16),FI00L(2),A63(16),F0J0L(2)
      DIMENSION A64(0:3,0:3),C64(2,  3),A65(0:3,0:3),C65(2,  3)
      DIMENSION A66(18,  3),C66(2,  3)
      DIMENSION A67(18,  3),C67(2,  3)
      DIMENSION A68(18,  3),C68(2,  3)
      DIMENSION A69(0:3,  3),C69(2,0:3),A70(0:3,  3),C70(2,0:3)
      DIMENSION A71(18,  3),C71(2,  3)
      DIMENSION A72(18,  3),C72(2,  3)
      DIMENSION A73(18,0:3),C73(2,0:3)
C
      DIMENSION SE0KKL(  3),A74(16,3),C74(2,0:3)
      DIMENSION SE0LKL(  3),A75(16,3),C75(2,0:3)
      DIMENSION SE0JLL(  3),A76(16,3),C76(2,0:3)
      DIMENSION A77(0:25,3,0:3),C77(2,0:3),A78(0:31,3,0:3),C78(2,0:3)
      DIMENSION A79(0:31,3,0:3),C79(2,0:3)
      DIMENSION A80(0: 2,2),A81(0:24,3),A82(0:31,2),A83(0:22,2)
      DIMENSION A84(0:13,2),A85(0:13,2),A86(0: 6)
      DIMENSION S4(0:14),Q4(0:4),FIJKL(2)
C
      PARAMETER (ZER=0.0D+00, PT5=0.5D+00, ONE=1.0D+00, TWO=2.0D+00)
      PARAMETER (THR=3.0D+00, SIX=6.0D+00)
C
      IF(JTYPE.LT.3) THEN
         IF(OUT) WRITE(IW,9010)
         CALL ABRT
      ENDIF
 9010 FORMAT(' GRDPER ONLY VALID FOR JTYPE= 3, 4, 5 OR 6')
C
      DO 140 N=1,12
  140 VEA(N)= ZER
C
C LOOP OVER THE UNCONTRACTED GAUSSIANS WITHIN THE SHELLS
C
C ... GAUSSIANS AT CENTER A.
C
C     ----- I PRIM -----
C
      JI= 0
      I0= 1
      DO 940 IG=IGBEG,IGEND
         X01= EX(IG)
         CSA= CS(IG)
         CPA= CP(IG)
C
C ... GAUSSIANS AT CENTER B.
C
C     ----- J PRIM -----
C
         JGND=JGEND
         IF(IJSAME) JGND=IG
         IJ=I0
         DO 930 JG=JGBEG,JGND
            IF(TE12(IJ).EQ.ZER) GO TO 930
            X02= EX(JG)
            CSB= CS(JG)
            CPB= CP(JG)
            X12= X01+X02
            H21= TH21(IJ)
            Y01= TY01(IJ)
            Y02= ONE-Y01
            Y1Y= TY1Y(IJ)
            F12= TF12(IJ)
            X24= X12+X12
            CSB= CSB*TE12(IJ)
            CPB= CPB*TE12(IJ)
            CSS= CSA*CSB
            CPS= CPA*CSB
            CSP= CSA*CPB
            CPP= CPA*CPB
C
            VE0SUM= ZER
            DO 158 N=1,9
  158       VES(N)= ZER
C
C ... GAUSSIANS AT CENTER C.
C
C     ----- K PRIM -----
C
            KGND=KGEND
            IF(IKSMJL) KGND=IG
            LK= 0
            K0= 1
            DO 920 KG=KGBEG,KGND
               X03= EX(KG)
               CSC= CS(KG)
               CPC= CP(KG)
               CSSS= CSS*CSC
               CPSS= CPS*CSC
               CSPS= CSP*CSC
               CPPS= CPP*CSC
               CSSP= CSS*CPC
               CPSP= CPS*CPC
               CSPP= CSP*CPC
               CPPP= CPP*CPC
C
C ... GAUSSIANS AT CENTER D.
C
C     ----- L PRIM -----
C
               LGND=LGEND
               IF(KLSAME) LGND=KG
               IF(IKSMJL.AND.(IG.EQ.KG)) LGND=JG
               KL=K0
               DO 910 LG=LGBEG,LGND
                  IF(TE34(KL).EQ.ZER) GO TO 910
                  X04= EX(LG)
                  CSD= CS(LG)
                  CPD= CP(LG)
                  X34= X03+X04
                  X14= X12+X34
                  X41= ONE/X14
                  VTEST= CSMAB(IJ)*CSMCD(KL)*X41
                  IF(VTEST.LT.VTOLS) GO TO 910
                  Z02= X12*X41
                  Z04= ONE-Z02
C
                  X43= TX43(KL)
                  H43= TH43(KL)
                  Y03= TY03(KL)
                  Y04= ONE-Y03
                  Y3Y= TY3Y(KL)
C
CJMS              RHO= X12*X34*X41
                  RHO= X12*Z04
                  XVA= ZER
                  DO 160 M=1,3
                     T00 = P0(M,IJ)-Q0(M,KL)
                     P1(M)=T00*Z04
  160             XVA= XVA+T00*T00
                  XVA= XVA*RHO
                  IF(XVA.LT.CUG) THEN
                     TMP= XVA*RGI
                     N  = INT(TMP)
                     C02= TMP-N
                     TMP= C02*(ONE-C02)
                     C03=-TMP*(TWO-C02)
                     C04=-TMP*(ONE+C02)
                     DO 165 M=0,MFQ
                        FQ(M)= R(1,M,N)+C02*R(2,M,N)+C03*R(3,M,N)+
     2                         C04*R(3,M,N+1)
  165                CONTINUE
                  ELSE
                     IF(VTEST.LT.VTOLS*XVA) GO TO 910
                     XIN= ONE/XVA
                     FQ(0)= SQRT(PI4*XIN)
                     TMP= PT5*XIN
                     DO 170 M=1,MFQ
                        FQ(M)= FQ(M-1)*TMP
  170                TMP= TMP+XIN
                  ENDIF
C
C COEFFICIENTS FOR THE TWO PASSES OF THE INTEGRAL CALCULATIONS
C
                  TMP= ONE
                  DO 171 M=0,MFQ-1
                     F1(M)= FQ(M  )*TMP
                     F2(M)= FQ(M+1)*TMP
  171             TMP= TMP*Z04
C
                  DO 175 N=1,6
  175             WES(N)= ZER
C
                  F34= TF34(KL)
                  E34= TE34(KL)* SQRT(X41)
                  IF(IKSMJL.AND.(IJ.NE.KL)) E34=E34+E34
                  CSD= CSD*E34
                  CPD= CPD*E34
                  WS(1)= CSSS*CSD
                  WS(2)= CPSS*CSD
                  ES1= E(0,0,0,0)*WS(1)
C
                  XX1=-X12*X43
                  XX2= XX1*XX1
                  Y31= Y03*XX1
                  Y41= XX1-Y31
                  IF(JTYPE.NE.4) THEN
                     WS(3)= CSPS*CSD
                     WS(4)= CPPS*CSD
                  ENDIF
                  IF(JTYPE.GT.3) THEN
                     WS(5)= CSSP*CSD
                     WS(6)= CPSP*CSD
                     IF(JTYPE.GT.4) THEN
                        WS(7)= CSPP*CSD
                        WS(8)= CPPP*CSD
                        IF(JTYPE.GT.5) THEN
                           WP(1)= CSSS*CPD
                           WP(2)= CPSS*CPD
                           WP(3)= CSPS*CPD
                           WP(4)= CPPS*CPD
                           WP(5)= CSSP*CPD
                           WP(6)= CPSP*CPD
                           WP(7)= CSPP*CPD
                           WP(8)= CPPP*CPD
                        ENDIF
                     ENDIF
                  ENDIF
C
      JI= JI+1
      IF(JI.EQ.1) THEN
C
CJMS  THE BULK CALCULATION IS MADE IN THIS BLOCK.
CJMS  IT IS ONLY CALCULATED ONCE PER SUBROUTINE CALL
C
         DO 210 N=1,3
            D1N= P12(N,1)-P34(N,1)
            DO 210 M=1,3
               P1D(M,N)= P12(M,3)*D1N
               P1P(M,N)= P12(M,3)*P12(N,3)
               P24(M,N)= P12(M,3)*P34(N,3)
               P3D(M,N)= P34(M,3)*D1N
               P3P(M,N)= P34(M,3)*P34(N,3)
  210    CONTINUE
         DO 220 N=1,3
            DO 220 M=1,3
               Q1(1,M,N)= P1D(M,N)
               Q1(2,M,N)= P1P(M,N)
               Q1(3,M,N)= P24(M,N)
               Q1(4,M,N)= P3D(N,M)
               Q1(5,M,N)= P3P(M,N)
               R1(1,M,N)= P1D(M,N)+P1P(M,N)
               R1(2,M,N)= P1D(M,N)+P1D(N,M)+P1P(M,N)+P1P(M,N)+P1P(M,N)
               R1(3,M,N)= P1P(M,N)+P1P(M,N)
               R1(4,M,N)= P24(M,N)
               R1(5,M,N)= P24(M,N)+P24(N,M)
C P2(M,N)
               R2(1,M,N)=(P12(M,1)-P34(M,1))*(P12(N,1)-P34(N,1))
               R2(2,M,N)= P1D(M,N)+P1D(N,M)
               R2(3,M,N)= P1P(M,N)
               R2(4,M,N)= P3D(M,N)+P3D(N,M)
               R2(5,M,N)= P24(M,N)+P24(N,M)
               R2(6,M,N)= P3P(M,N)
  220    CONTINUE
C
         IF(JTYPE.NE.4) THEN
            DO 233 J=0,3
               DO 231 M=0,2
                  A31(M,J)= ZER
                  A32(M,J)= ZER
  231          CONTINUE
               DO 233 I=1,3
                  D1I= P12(I,1)-P34(I,1)
                  A31(0,J)= A31(0,J)+D1I     *E(J,I,0,0)
                  A31(1,J)= A31(1,J)+P12(I,3)*E(J,I,0,0)
                  A31(2,J)= A31(2,J)+P34(I,3)*E(J,I,0,0)
                  A32(0,J)= A32(0,J)+D1I     *E(I,J,0,0)
                  A32(1,J)= A32(1,J)+P12(I,3)*E(I,J,0,0)
                  A32(2,J)= A32(2,J)+P34(I,3)*E(I,J,0,0)
  233       CONTINUE
            AEIJ00= ZER
            DO 234 J=1,3
               DO 234 I=1,3
  234       AEIJ00= AEIJ00+P1P(I,J)*E(I,J,0,0)
            SEJJ00= E(1,1,0,0)+E(2,2,0,0)+E(3,3,0,0)
            DO 235 N=1,4
               A33(  N)= ZER
               A33(8+N)= ZER
  235       CONTINUE
            A33(13)= ZER
            A33(14)= ZER
            DO 239 J=1,3
               DO 239 I=1,3
C V(I,J,0,0)
                  GEIJ00= E(I,J,0,0)
                  DO 237 N=1,4
                     A33(  N)= A33(  N)+R1(N,I,J)*GEIJ00
                     A33(8+N)= A33(8+N)+R2(N,I,J)*GEIJ00
  237             CONTINUE
                  A33(13)= A33(13)+R2(5,I,J)*GEIJ00
                  A33(14)= A33(14)+R2(6,I,J)*GEIJ00
  239       CONTINUE
            A33( 5)= A33(13)
         ENDIF
         IF(JTYPE.LT.4) GO TO 490
C
         DO 243 K=0,3
            DO 241 M=0,2
               A41(M,K)= ZER
               A42(M,K)= ZER
  241       CONTINUE
            DO 243 I=1,3
               D1I= P12(I,1)-P34(I,1)
               A41(0,K)= A41(0,K)+D1I     *E(K,0,I,0)
               A41(1,K)= A41(1,K)+P12(I,3)*E(K,0,I,0)
               A41(2,K)= A41(2,K)+P34(I,3)*E(K,0,I,0)
               A42(0,K)= A42(0,K)+D1I     *E(I,0,K,0)
               A42(1,K)= A42(1,K)+P12(I,3)*E(I,0,K,0)
               A42(2,K)= A42(2,K)+P34(I,3)*E(I,0,K,0)
  243    CONTINUE
         DO 245 N=1,5
            A43(  N)= ZER
            A43(8+N)= ZER
  245    CONTINUE
         SEK0K0= E(1,0,1,0)+E(2,0,2,0)+E(3,0,3,0)
         DO 249 K=1,3
            DO 249 I=1,3
C V(I,0,K,0)
               GEI0K0= E(I,0,K,0)
               DO 247 N=1,5
                  A43(  N)= A43(  N)+Q1(N,I,K)*GEI0K0
                  A43(8+N)= A43(8+N)+R2(N,I,K)*GEI0K0
  247          CONTINUE
  249    CONTINUE
         A43(14)= A43( 5)
         IF(JTYPE.LT.5) GO TO 490
C
         DO 255 K=1,3
            DO 250 M=0,2
               A51(M,K)= ZER
               A52(M,K)= ZER
  250       CONTINUE
            DO 255 J=1,3
               D1J = P12(J,1)-P34(J,1)
               A51(0,K)= A51(0,K)+D1J     *E(0,K,J,0)
               A51(1,K)= A51(1,K)+P12(J,3)*E(0,K,J,0)
               A51(2,K)= A51(2,K)+P34(J,3)*E(0,K,J,0)
               A52(0,K)= A52(0,K)+D1J     *E(0,J,K,0)
               A52(1,K)= A52(1,K)+P12(J,3)*E(0,J,K,0)
               A52(2,K)= A52(2,K)+P34(J,3)*E(0,J,K,0)
  255    CONTINUE
         DO 268 K=1,3
            SEJJK0(K)= E(1,1,K,0)+E(2,2,K,0)+E(3,3,K,0)
            DO 262 N=1,4
               A54(  N,K)= ZER
               A54(8+N,K)= ZER
  262       CONTINUE
            A54(13,K)= ZER
            A54(14,K)= ZER
            DO 266 J=1,3
               DO 266 I=1,3
C V(I,J,0,0)
                  GEIJK0= E(I,J,K,0)
                  DO 264 N=1,4
                     A54(  N,K)= A54(  N,K)+R1(N,I,J)*GEIJK0
                     A54(8+N,K)= A54(8+N,K)+R2(N,I,J)*GEIJK0
  264             CONTINUE
                  A54(13,K)= A54(13,K)+R2(5,I,J)*GEIJK0
                  A54(14,K)= A54(14,K)+R2(6,I,J)*GEIJK0
  266       CONTINUE
            A54( 5,K)= A54(13,K)
  268    CONTINUE
         DO 278 J=1,3
            SEKJK0(J)= E(1,J,1,0)+E(2,J,2,0)+E(3,J,3,0)
            DO 272 N=1,5
               A55(  N,J)= ZER
               A55(8+N,J)= ZER
  272       CONTINUE
            DO 276 K=1,3
               DO 276 I=1,3
C V(I,0,K,0)
                  GEIJK0= E(I,J,K,0)
                  DO 274 N=1,5
                     A55(  N,J)= A55(  N,J)+Q1(N,I,K)*GEIJK0
                     A55(8+N,J)= A55(8+N,J)+R2(N,I,K)*GEIJK0
  274             CONTINUE
  276       CONTINUE
            A55(14,J)= A55( 5,J)
  278    CONTINUE
         DO 288 I=0,3
            SEIKK0(I)= E(I,1,1,0)+E(I,2,2,0)+E(I,3,3,0)
            DO 282 N=1,5
               A53(  N,I)= ZER
               A53(8+N,I)= ZER
  282       CONTINUE
            DO 286 K=1,3
               DO 286 J=1,3
C V(0,J,K,0)
                  GEIJK0= E(I,J,K,0)
                  DO 284 N=1,5
                     A53(  N,I)= A53(  N,I)+Q1(N,J,K)*GEIJK0
                     A53(8+N,I)= A53(8+N,I)+R2(N,J,K)*GEIJK0
  284             CONTINUE
  286       CONTINUE
            A53(14,I)= A53( 5,I)
  288    CONTINUE
C
         DO 293 K=1,3
            DO 293 J=1,3
               DO 293 I=1,3
                  DO 291 M=1,6
                     T1R(M,I,J,K)= P12(I,3)*R2(M,J,K)
                     T3R(M,I,J,K)= P34(I,3)*R2(M,J,K)
  291             CONTINUE
                  T1R(6,I,J,K)=-T1R(6,I,J,K)
                  T3R(6,I,J,K)=-T3R(6,I,J,K)
  293    CONTINUE
C
         DO 295 N=1,3
            DO 295 M=1,3
               DO 295 L=1,3
C P3(L,M,N)
                  D1L= P12(L,1)-P34(L,1)
                  R3(0,L,M,N)= D1L*R2(1,M,N)
                  R3(1,L,M,N)= D1L*R2(2,M,N)+T1R(1,L,M,N)
                  R3(2,L,M,N)= D1L*R2(3,M,N)+T1R(2,L,M,N)
                  R3(3,L,M,N)= T1R(3,L,M,N)
                  R3(4,L,M,N)=-D1L*R2(4,M,N)-T3R(1,L,M,N)
                  R3(5,L,M,N)=-D1L*R2(5,M,N)-T1R(4,L,M,N)-T3R(2,L,M,N)
                  R3(6,L,M,N)=-T1R(5,L,M,N) -T3R(3,L,M,N)
                  R3(7,L,M,N)= D1L*R2(6,M,N)+T3R(4,L,M,N)
                  R3(8,L,M,N)=-T1R(6,L,M,N) +T3R(5,L,M,N)
                  R3(9,L,M,N)= T3R(6,L,M,N)
  295    CONTINUE
C
         DO 299 I=1,3
            D1I= P12(I,1)-P34(I,1)
            F5X(0,I,I,I)=-D1I     *THR
            F5X(1,I,I,I)=-P12(I,3)*THR
            F5X(2,I,I,I)= P34(I,3)*THR
            DO 297 M=0,9
  297       F5X(3+M,I,I,I)=-R3(M,I,I,I)
            DO 299 J=1,3
               IF(I.NE.J) THEN
                  K=6-I-J
                  F5X(0,K,J,I)= ZER
                  F5X(1,K,J,I)= ZER
                  F5X(2,K,J,I)= ZER
                  D1J= P12(J,1)-P34(J,1)
                  F5X(0,J,I,I)=-D1J
                  F5X(1,J,I,I)=-P12(J,3)
                  F5X(2,J,I,I)= P34(J,3)
                  F5X(0,I,J,I)=-D1J
                  F5X(1,I,J,I)=-P12(J,3)
                  F5X(2,I,J,I)= P34(J,3)
                  F5X(0,I,I,J)=-D1J
                  F5X(1,I,I,J)=-P12(J,3)
                  F5X(2,I,I,J)= P34(J,3)
                  DO 298 M=0,9
                     F5X(3+M,K,J,I)=-R3(M,3,2,1)
                     F5X(3+M,J,I,I)=-R3(M,J,I,I)
                     F5X(3+M,I,J,I)=-R3(M,J,I,I)
                     F5X(3+M,I,I,J)=-R3(M,J,I,I)
  298             CONTINUE
               ENDIF
  299    CONTINUE
C
         M56=(JTYPE-5)*3
         DO 300 L=0,M56
            DO 300 M=1,3
               DO 300 N=0,17
  300    A56(N,M,L)= ZER
         DO 309 K=1,3
            DO 309 J=1,3
               DO 309 I=1,3
                  MM= 8
                  IF(I.EQ.J) MM=11
                  T01= P12(I,3)*P1D(J,K)
                  T02= P12(I,3)*P1P(J,K)
                  T03= P1D(J,I)*P34(K,3)
                  T04= P24(J,I)*P34(K,3)
                  T( 0)=-T01
                  T( 1)= T01-T02
                  T( 2)= T02
                  T( 3)= P12(I,3)*P24(J,K)
                  T( 4)=-T03
                  T( 5)=(P1D(I,J)-P1P(I,J))*P34(K,3)+T03
                  T( 6)=(P1P(I,J)+P1P(I,J))*P34(K,3)
                  T( 7)= T04
                  T( 8)=-P24(I,J)*P34(K,3)-T04
                  T( 9)=-P12(K,1)+P34(K,1)
                  T(10)=-P12(K,3)
                  T(11)= P34(K,3)
                  T(12)=-P12(I,3)
                  T(13)= P12(J,3)
                  U( 0)= F5X(0,I,J,K)
                  U( 1)= F5X(1,I,J,K)
                  U( 2)= F5X(2,I,J,K)
                  U( 3)=-T1R(1,J,I,K)
                  U( 4)= T1R(1,I,J,K)+T1R(1,J,I,K)-T1R(2,J,I,K)
                  U( 5)= T1R(2,I,J,K)+T1R(2,J,I,K)-T1R(3,J,I,K)
                  U( 6)= T1R(3,I,J,K)+T1R(3,J,I,K)
                  U( 7)= T1R(4,J,I,K)
                  U( 8)= T1R(4,I,J,K)+T1R(4,J,I,K)-T1R(5,J,I,K)
                  U( 9)= T1R(5,I,J,K)+T1R(5,J,I,K)
                  U(10)= T1R(6,J,I,K)
                  U(11)=-T1R(6,I,J,K)-T1R(6,J,I,K)
                  DO 301 M=1,6
  301             U(11+M)= T3R(M,K,I,J)
                  DO 309 L=0,M56
                     GEIJKL= E(I,J,K,L)
C V(I,J,K,0)
                     DO 303 M=0,MM
  303                A56( M,1,L)= A56( M,1,L)+T(M)*GEIJKL
                     IF(J.EQ.K) A56(12,1,L)= A56(12,1,L)+T(12)*GEIJKL
                     IF(I.EQ.K) A56(13,1,L)= A56(13,1,L)+T(13)*GEIJKL
                     DO 305 M=0,17
  305                A56( M,2,L)= A56( M,2,L)+U( M)*GEIJKL
                     DO 307 M=0,9
  307                A56( M,3,L)= A56( M,3,L)+F5X(3+M,I,J,K)*GEIJKL
  309    CONTINUE
         IF(JTYPE.LT.6) GO TO 490
C
         DO 312 L=0,3
            DO 310 M=0,2
               A60(M,L)= ZER
               A61(M,L)= ZER
  310       CONTINUE
            DO 312 I=1,3
               D1I= P12(I,1)-P34(I,1)
C P1(I)
               A60(0,L)= A60(0,L)+D1I     *E(I,0,0,L)
               A60(1,L)= A60(1,L)+P12(I,3)*E(I,0,0,L)
               A60(2,L)= A60(2,L)+P34(I,3)*E(I,0,0,L)
C V(0,0,0,L)
               A61(0,L)= A61(0,L)+D1I     *E(L,0,0,I)
               A61(1,L)= A61(1,L)+P12(I,3)*E(L,0,0,I)
               A61(2,L)= A61(2,L)+P34(I,3)*E(L,0,0,I)
  312    CONTINUE
         DO 314 N=1,6
            A62(  N)= ZER
            A62(8+N)= ZER
            A63(  N)= ZER
            A63(8+N)= ZER
  314    CONTINUE
         SEL00L= E(1,0,0,1)+E(2,0,0,2)+E(3,0,0,3)
         DO 318 L=1,3
            DO 318 I=1,3
               GEI00L= E(I,0,0,L)
C V(I,0,0,L)
               A62( 1)= A62( 1)+ P1D(I,L)*GEI00L
               A62( 2)= A62( 2)+ P1P(I,L)*GEI00L
               A62( 3)= A62( 3)+ P24(I,L)*GEI00L
               A62( 4)= A62( 4)+ P3D(L,I)*GEI00L
               A62( 5)= A62( 5)- P3P(I,L)*GEI00L
               DO 316 N=1,6
  316          A62(8+N)= A62(8+N)+R2(N,I,L)*GEI00L
  318    CONTINUE
         SE0L0L= E(0,1,0,1)+E(0,2,0,2)+E(0,3,0,3)
         DO 322 L=1,3
            DO 322 J=1,3
               GE0J0L= E(0,J,0,L)
C V(0,J,0,L)
               A63( 1)= A63( 1)+ P1D(J,L)*GE0J0L
               A63( 2)= A63( 2)+ P1P(J,L)*GE0J0L
               A63( 3)= A63( 3)+ P24(J,L)*GE0J0L
               A63( 4)= A63( 4)+ P3D(L,J)*GE0J0L
               A63( 5)= A63( 5)- P3P(J,L)*GE0J0L
               DO 320 N=1,6
  320          A63(8+N)= A63(8+N)+R2(N,J,L)*GE0J0L
  322    CONTINUE
         DO 326 L=1,3
            DO 324 M=0,2
               A64(M,L)= ZER
               A65(M,L)= ZER
  324       CONTINUE
            DO 326 J=1,3
               D1J= P12(J,1)-P34(J,1)
               A64(0,L)= A64(0,L)+D1J     *E(0,J,0,L)
               A64(1,L)= A64(1,L)+P12(J,3)*E(0,J,0,L)
               A64(2,L)= A64(2,L)+P34(J,3)*E(0,J,0,L)
               A65(0,L)= A65(0,L)+D1J     *E(0,L,0,J)
               A65(1,L)= A65(1,L)+P12(J,3)*E(0,L,0,J)
               A65(2,L)= A65(2,L)+P34(J,3)*E(0,L,0,J)
  326    CONTINUE
         DO 328 L=1,3
            DO 328 M=0,2
               A69(M,L)= ZER
               A70(M,L)= ZER
  328    CONTINUE
         DO 329 L=1,3
            DO 329 N=1,6
               A66(   N,L)= ZER
               A66(10+N,L)= ZER
               A67(   N,L)= ZER
               A67(10+N,L)= ZER
               A68(   N,L)= ZER
               A68(10+N,L)= ZER
               A71(   N,L)= ZER
               A71(10+N,L)= ZER
               A72(   N,L)= ZER
               A72(10+N,L)= ZER
  329    CONTINUE
         DO 330 L=0,3
            DO 330 N=1,6
               A73(   N,L)= ZER
               A73(10+N,L)= ZER
  330    CONTINUE
         DO 338 L=1,3
            A66( 1,L)= E(1,1,0,L)+E(2,2,0,L)+E(3,3,0,L)
            A67( 1,L)= E(1,L,0,1)+E(2,L,0,2)+E(3,L,0,3)
            A68( 1,L)= E(L,1,0,1)+E(L,2,0,2)+E(L,3,0,3)
            DO 338 J=1,3
               DO 338 I=1,3
                  GEIJ0L= E(I,J,0,L)
C V(I,J,0,0)
                  A66( 2,L)= A66( 2,L)+ P1D(J,I)*GEIJ0L
                  A66( 3,L)= A66( 3,L)+ P1P(I,J)*GEIJ0L
                  A66( 4,L)= A66( 4,L)+ P24(J,I)*GEIJ0L
                  A66( 5,L)= A66( 5,L)+ P1D(I,J)*GEIJ0L
                  A66( 6,L)= A66( 6,L)- P24(I,J)*GEIJ0L
                  DO 332 N=1,6
  332             A66(10+N,L)= A66(10+N,L)+R2(N,I,J)*GEIJ0L
C V(I,0,0,L)
                  A67( 2,J)= A67( 2,J)+ P1D(I,L)*GEIJ0L
                  A67( 3,J)= A67( 3,J)+ P1P(I,L)*GEIJ0L
                  A67( 4,J)= A67( 4,J)+ P24(I,L)*GEIJ0L
                  A67( 5,J)= A67( 5,J)+ P3D(L,I)*GEIJ0L
                  A67( 6,J)= A67( 6,J)- P3P(I,L)*GEIJ0L
                  DO 334 N=1,6
  334             A67(10+N,J)= A67(10+N,J)+R2(N,I,L)*GEIJ0L
C V(0,J,0,L)
                  A68( 2,I)= A68( 2,I)+ P1D(J,L)*GEIJ0L
                  A68( 3,I)= A68( 3,I)+ P1P(J,L)*GEIJ0L
                  A68( 4,I)= A68( 4,I)+ P24(J,L)*GEIJ0L
                  A68( 5,I)= A68( 5,I)+ P3D(L,J)*GEIJ0L
                  A68( 6,I)= A68( 6,I)- P3P(J,L)*GEIJ0L
                  DO 336 N=1,6
  336             A68(10+N,I)= A68(10+N,I)+R2(N,J,L)*GEIJ0L
  338    CONTINUE
         DO 342 L=1,3
            D1L= P12(L,1)-P34(L,1)
            DO 342 K=1,3
               D1K= P12(K,1)-P34(K,1)
C V(0,0,L,0)
               A69(0,L)= A69(0,L)+D1K     *E(0,0,K,L)
               A69(1,L)= A69(1,L)+P12(K,3)*E(0,0,K,L)
               A69(2,L)= A69(2,L)+P34(K,3)*E(0,0,K,L)
C V(0,0,0,L)
               A70(0,K)= A70(0,K)+D1L     *E(0,0,K,L)
               A70(1,K)= A70(1,K)+P12(L,3)*E(0,0,K,L)
               A70(2,K)= A70(2,K)+P34(L,3)*E(0,0,K,L)
  342    CONTINUE
         DO 348 L=1,3
            A71( 1,L)= E(1,0,1,L)+E(2,0,2,L)+E(3,0,3,L)
            A72( 1,L)= E(1,0,L,1)+E(2,0,L,2)+E(3,0,L,3)
            DO 348 K=1,3
               DO 348 I=1,3
                  GEI0KL= E(I,0,K,L)
C V(I,0,K,0)
                  A71( 2,L)= A71( 2,L)+ P1D(I,K)*GEI0KL
                  A71( 3,L)= A71( 3,L)+ P1P(I,K)*GEI0KL
                  A71( 4,L)= A71( 4,L)+ P24(I,K)*GEI0KL
                  A71( 5,L)= A71( 5,L)+ P3D(K,I)*GEI0KL
                  A71( 6,L)= A71( 6,L)- P3P(I,K)*GEI0KL
                  DO 344 N=1,6
  344             A71(10+N,L)= A71(10+N,L)+R2(N,I,K)*GEI0KL
C V(I,0,0,L)
                  A72( 2,K)= A72( 2,K)+ P1D(I,L)*GEI0KL
                  A72( 3,K)= A72( 3,K)+ P1P(I,L)*GEI0KL
                  A72( 4,K)= A72( 4,K)+ P24(I,L)*GEI0KL
                  A72( 5,K)= A72( 5,K)+ P3D(L,I)*GEI0KL
                  A72( 6,K)= A72( 6,K)- P3P(I,L)*GEI0KL
                  DO 346 N=1,6
  346             A72(10+N,K)= A72(10+N,K)+R2(N,I,L)*GEI0KL
  348    CONTINUE
            A73( 1,0)= E(0,0,1,1)+E(0,0,2,2)+E(0,0,3,3)
         DO 353 L=1,3
            A73( 1,L)= E(L,0,1,1)+E(L,0,2,2)+E(L,0,3,3)
            DO 353 K=1,3
               DO 353 I=0,3
                  GEI0KL= E(I,0,K,L)
                  A73( 2,I)= A73( 2,I)+ P3D(L,K)*GEI0KL
                  A73( 3,I)= A73( 3,I)+ P24(K,L)*GEI0KL
                  A73( 4,I)= A73( 4,I)- P3P(K,L)*GEI0KL
                  A73( 5,I)= A73( 5,I)+ P3D(K,L)*GEI0KL
                  A73( 6,I)= A73( 6,I)+ P24(L,K)*GEI0KL
                  DO 351 N=1,6
  351             A73(10+N,I)= A73(10+N,I)+R2(N,K,L)*GEI0KL
  353    CONTINUE
C
         DO 359 N=1,3
            DO 357 M=1,5
               A74(  M,N)= ZER
               A74(8+M,N)= ZER
               A75(  M,N)= ZER
               A75(8+M,N)= ZER
               A76(  M,N)= ZER
               A76(8+M,N)= ZER
  357       CONTINUE
            A75(14,N)= ZER
            A76(14,N)= ZER
  359    CONTINUE
         DO 369 L=1,3
            SE0KKL(L)= E(0,1,1,L)+E(0,2,2,L)+E(0,3,3,L)
            SE0LKL(L)= E(0,1,L,1)+E(0,2,L,2)+E(0,3,L,3)
            SE0JLL(L)= E(0,L,1,1)+E(0,L,2,2)+E(0,L,3,3)
            DO 367 K=1,3
               DO 367 J=1,3
                  GE0JKL= E(0,J,K,L)
C V(0,J,K,0)
                  DO 361 N=1,5
                     A74(  N,L)= A74(  N,L)+Q1(N,J,K)*GE0JKL
                     A74(8+N,L)= A74(8+N,L)+R2(N,J,K)*GE0JKL
  361             CONTINUE
C V(0,J,0,L)
                  A75( 1,K)= A75( 1,K)+ P1D(J,L)*GE0JKL
                  A75( 2,K)= A75( 2,K)+ P1P(J,L)*GE0JKL
                  A75( 3,K)= A75( 3,K)+ P24(J,L)*GE0JKL
                  A75( 4,K)= A75( 4,K)+ P3D(L,J)*GE0JKL
                  A75( 5,K)= A75( 5,K)- P3P(J,L)*GE0JKL
                  DO 363 N=1,6
  363             A75(8+N,K)= A75(8+N,K)+R2(N,J,L)*GE0JKL
C V(0,0,K,L)
                  A76( 1,J)= A76( 1,J)+ P3D(L,K)*GE0JKL
                  A76( 2,J)= A76( 2,J)+ P24(K,L)*GE0JKL
                  A76( 3,J)= A76( 3,J)- P3P(K,L)*GE0JKL
                  A76( 4,J)= A76( 4,J)+ P3D(K,L)*GE0JKL
                  A76( 5,J)= A76( 5,J)+ P24(L,K)*GE0JKL
                  DO 365 N=1,6
  365             A76(8+N,J)= A76(8+N,J)+R2(N,K,L)*GE0JKL
  367       CONTINUE
            A74(14,L)= A74( 5,L)
  369    CONTINUE
C
         DO 372 M=0,14
  372    S4(M)= ZER
         DO 378 L=1,3
            DO 378 K=1,3
               DO 374 N=1,6
  374          U(N)= R2(N,K,L)
               DO 378 J=1,3
                  DO 378 I=1,3
                     T( 0)= R2(1,I,J)*U(1)
                     T( 1)= R2(1,I,J)*U(2)+R2(2,I,J)*U(1)
                     T( 2)= R2(1,I,J)*U(3)+R2(2,I,J)*U(2)+R2(3,I,J)*U(1)
                     T( 3)= R2(2,I,J)*U(3)+R2(3,I,J)*U(2)
                     T( 4)= R2(3,I,J)*U(3)
                     T( 5)=-R2(1,I,J)*U(4)-R2(4,I,J)*U(1)
                     T( 6)=-R2(1,I,J)*U(5)-R2(2,I,J)*U(4)
     2                     -R2(4,I,J)*U(2)-R2(5,I,J)*U(1)
                     T( 7)=-R2(2,I,J)*U(5)-R2(3,I,J)*U(4)
     2                     -R2(4,I,J)*U(3)-R2(5,I,J)*U(2)
                     T( 8)=-R2(3,I,J)*U(5)-R2(5,I,J)*U(3)
                     T( 9)= R2(1,I,J)*U(6)+R2(4,I,J)*U(4)+R2(6,I,J)*U(1)
                     T(10)= R2(2,I,J)*U(6)+R2(4,I,J)*U(5)
     2                     +R2(5,I,J)*U(4)+R2(6,I,J)*U(2)
                     T(11)= R2(3,I,J)*U(6)+R2(5,I,J)*U(5)+R2(6,I,J)*U(3)
                     T(12)=-R2(4,I,J)*U(6)-R2(6,I,J)*U(4)
                     T(13)=-R2(5,I,J)*U(6)-R2(6,I,J)*U(5)
                     T(14)= R2(6,I,J)*U(6)
                     GEIJKL= E(I,J,K,L)
                     DO 376 M=0,14
  376                S4(M)= S4(M)+T(M)*GEIJKL
  378    CONTINUE
C
         DO 386 I=1,3
            F6X(0,I,I,I,I)= THR
            DO 382 M=1,6
  382       F6X(M,I,I,I,I)= R2(M,I,I)*SIX
            DO 386 J=1,3
               IF(I.NE.J) THEN
                  K=6-I-J
                  F6X(0,J,I,I,I)= ZER
                  F6X(0,I,J,I,I)= ZER
                  F6X(0,I,I,J,I)= ZER
                  F6X(0,I,I,I,J)= ZER
                  F6X(0,J,J,I,I)= ONE
                  F6X(0,J,I,J,I)= ONE
                  F6X(0,I,J,J,I)= ONE
                  F6X(0,J,I,I,J)= ONE
                  F6X(0,I,J,I,J)= ONE
                  F6X(0,I,I,J,J)= ONE
                  F6X(0,K,J,I,I)= ZER
                  F6X(0,K,I,J,I)= ZER
                  F6X(0,I,K,J,I)= ZER
                  F6X(0,K,I,I,J)= ZER
                  F6X(0,I,K,I,J)= ZER
                  F6X(0,I,I,K,J)= ZER
                  DO 384 M=1,6
                     EQ0= R2(M,J,I)*THR
                     F6X(M,J,I,I,I)= EQ0
                     F6X(M,I,J,I,I)= EQ0
                     F6X(M,I,I,J,I)= EQ0
                     F6X(M,I,I,I,J)= EQ0
                     EQ1= R2(M,I,I)+R2(M,J,J)
                     F6X(M,J,J,I,I)= EQ1
                     F6X(M,J,I,J,I)= EQ1
                     F6X(M,I,J,J,I)= EQ1
                     F6X(M,J,I,I,J)= EQ1
                     F6X(M,I,J,I,J)= EQ1
                     F6X(M,I,I,J,J)= EQ1
                     F6X(M,K,J,I,I)= R2(M,K,J)
                     F6X(M,K,I,J,I)= R2(M,K,J)
                     F6X(M,I,K,J,I)= R2(M,K,J)
                     F6X(M,K,I,I,J)= R2(M,K,J)
                     F6X(M,I,K,I,J)= R2(M,K,J)
                     F6X(M,I,I,K,J)= R2(M,K,J)
  384             CONTINUE
               ENDIF
  386    CONTINUE
C
         DO 395 N=1,2
            DO 391 M=0,20
  391       A82( M,N)= ZER
            DO 392 M=0,14
  392       A83( M,N)= ZER
            DO 393 M=0,9
               A84(M,N)= ZER
               A85(M,N)= ZER
  393       CONTINUE
  395    CONTINUE
         DO 397 M=0,6
  397    A86( M)= ZER
C
         DO 399 K=1,3
            DO 399 J=1,3
               DO 399 I=1,3
                  T9A( 0,I,J,K)= P12(I,3)*P3D(J,K)
                  T9A( 1,I,J,K)= P12(I,3)*P24(K,J)
                  T9A( 2,I,J,K)=-P12(I,3)*P3P(J,K)
                  T9A( 3,I,J,K)=-P12(I,3)*P3D(K,J)
                  T9A( 4,I,J,K)=-P12(I,3)*P24(J,K)
                  T9A( 5,I,J,K)= P12(I,3)*P3P(J,K)
                  T9A( 6,I,J,K)=-P34(J,3)*P3D(K,I)
                  T9A( 7,I,J,K)=-P34(J,3)*P24(I,K)
                  T9A( 8,I,J,K)= P34(J,3)*P3P(I,K)
                  T9A( 9,I,J,K)= P12(I,1)-P34(I,1)
                  T9A(10,I,J,K)=-P12(I,3)
                  T9A(11,I,J,K)=-P34(I,3)
                  T9A(12,I,J,K)= P34(K,3)
                  T9A(13,I,J,K)=-P34(J,3)
                  T9B( 0,I,J,K)= F5X(0,I,J,K)
                  T9B( 1,I,J,K)= F5X(1,I,J,K)
                  T9B( 2,I,J,K)= F5X(2,I,J,K)
                  DO 398 M=1,6
                     T9B(M+ 2,I,J,K)= T3R(M,K,I,J)
                     T9B(M+ 8,I,J,K)= T1R(M,I,J,K)
                     T9B(M+14,I,J,K)= T3R(M,J,I,K)
  398             CONTINUE
  399    CONTINUE
C
         A80(0,1)= ZER
         A80(1,1)= ZER
         A80(0,2)= ZER
         A80(1,2)= ZER
         DO 408 L=1,3
            DO 408 K=1,3
               A80(1,2)= A80(1,2)+E(K,K,L,L)
               DO 408 J=1,3
                  DO 408 I=1,3
                     GEIJKL= E(I,J,K,L)
                     A80(0,1)= A80(0,1)+P1P(I,J)*P3P(K,L)*GEIJKL
                     T02=-P12(J,3)*GEIJKL
                     T03= P12(I,3)*GEIJKL
                     DO 401 M=0,20
                        A82( M,1)= A82( M,1)+T9B( M,I,K,L)*T02
                        A82( M,2)= A82( M,2)+T9B( M,J,K,L)*T03
  401                CONTINUE
                     T04=-P34(L,3)*GEIJKL
                     T05= P34(K,3)*GEIJKL
                     DO 402 M=0,2
                        A83( M,1)= A83( M,1)+F5X( M,I,J,K)*T04
                        A83( M,2)= A83( M,2)+F5X( M,I,J,L)*T05
  402                CONTINUE
                     T00=-P1P(I,J)*GEIJKL
                     T01=-P3P(K,L)*GEIJKL
                     DO 403 M=1,6
                        A83(2+M,1)= A83(2+M,1)+R2(M,K,L)*T00
                        A83(2+M,2)= A83(2+M,2)+R2(M,I,J)*T01
  403                CONTINUE
                     IF(I.EQ.J) THEN
                        A80(1,1)= A80(1,1)+T01
                        DO 404 M=1,6
  404                   A83(8+M,1)= A83(8+M,1)+R2(M,K,L)*GEIJKL
                     ENDIF
                     IF(K.EQ.L) THEN
                        A80(0,2)= A80(0,2)+T00
                        DO 405 M=1,6
  405                   A83(8+M,2)= A83(8+M,2)+R2(M,I,J)*GEIJKL
                     ENDIF
                     DO 406 M=0,9
                        A84(M,1)= A84(M,1)+F5X(3+M,I,K,L)*T02
                        A84(M,2)= A84(M,2)+F5X(3+M,J,K,L)*T03
                        A85(M,1)= A85(M,1)+F5X(3+M,I,J,K)*T04
                        A85(M,2)= A85(M,2)+F5X(3+M,I,J,L)*T05
  406                CONTINUE
                     DO 407 M=0,6
  407                A86(M)= A86(M)+F6X(M,I,J,K,L)*GEIJKL
  408    CONTINUE
C
         DO 409 L=0,3
            DO 409 M=1,3
               DO 409 N=0,20
                  A77(N,M,L)= ZER
                  A78(N,M,L)= ZER
                  A79(N,M,L)= ZER
  409    CONTINUE
         DO 419 L=1,3
            DO 419 J=1,3
               DO 419 I=1,3
                  MM= 8
                  IF(I.EQ.J) MM=11
                  T( 0)=-P12(I,3)*P1D(J,L)
                  T( 1)=-P12(I,3)*P1P(J,L)
                  T( 2)= P12(I,3)*P24(J,L)
                  T( 3)=-P12(I,3)*P3D(L,J)
                  T( 4)=-P12(I,3)*P24(J,L)
                  T( 5)= P12(I,3)*P3P(J,L)
                  T( 6)= P12(J,3)*P3D(L,I)
                  T( 7)= P12(J,3)*P24(I,L)
                  T( 8)=-P12(J,3)*P3P(I,L)
                  T( 9)= P12(L,1)-P34(L,1)
                  T(10)= P12(L,3)
                  T(11)=-P34(L,3)
                  T(12)= P12(I,3)
                  T(13)=-P12(J,3)
                  U( 0)= F5X(0,I,J,L)
                  U( 1)= F5X(1,I,J,L)
                  U( 2)= F5X(2,I,J,L)
                  DO 411 M=1,6
                     U(M+ 2)= T1R(M,I,J,L)
                     U(M+ 8)=-T1R(M,J,I,L)
                     U(M+14)=-T3R(M,L,I,J)
  411             CONTINUE
                  DO 419 K=0,3
                     GEIJKL= E(I,J,K,L)
C V(I,J,0,L)
                     DO 413 M=0,MM
  413                A77( M,1,K)= A77( M,1,K)+T( M)*GEIJKL
                     IF(J.EQ.L) A77(12,1,K)= A77(12,1,K)+T(12)*GEIJKL
                     IF(I.EQ.L) A77(13,1,K)= A77(13,1,K)+T(13)*GEIJKL
                     DO 415 M=0,20
  415                A77( M,2,K)= A77( M,2,K)+U( M)*GEIJKL
                     DO 417 M=0,9
  417                A77( M,3,K)= A77( M,3,K)+F5X(3+M,I,J,L)*GEIJKL
  419    CONTINUE
         DO 429 L=1,3
            DO 429 K=1,3
               MM= 8
               IF(K.EQ.L) MM=11
                  A78(12,1,0)= A78(12,1,0)+T9A(12,K,K,L)*E(K,0,K,L)
                  A78(13,1,0)= A78(13,1,0)+T9A(13,L,K,L)*E(L,0,K,L)
               DO 429 I=1,3
                  A78(12,1,I)= A78(12,1,I)+T9A(12,K,K,L)*E(K,I,K,L)
                  A78(13,1,I)= A78(13,1,I)+T9A(13,L,K,L)*E(L,I,K,L)
                  DO 429 J=0,3
                     GEIJKL= E(I,J,K,L)
C V(I,0,K,L)
                     DO 425 M=0,MM
  425                A78( M,1,J)= A78( M,1,J)+T9A( M,I,K,L)*GEIJKL
                     DO 427 M=0,20
  427                A78( M,2,J)= A78( M,2,J)+T9B( M,I,K,L)*GEIJKL
                     DO 428 M=0,9
  428                A78( M,3,J)= A78( M,3,J)+F5X(3+M,I,K,L)*GEIJKL
  429    CONTINUE
         DO 439 L=1,3
            DO 439 K=1,3
               MM=11
               IF(K.EQ.L) MM=14
                  A79(15,1,0)= A79(15,1,0)+T9A(12,K,K,L)*E(0,K,K,L)
                  A79(16,1,0)= A79(16,1,0)+T9A(13,L,K,L)*E(0,L,K,L)
               DO 439 J=1,3
                  A79(15,1,J)= A79(15,1,J)+T9A(12,K,K,L)*E(J,K,K,L)
                  A79(16,1,J)= A79(16,1,J)+T9A(13,L,K,L)*E(J,L,K,L)
                  T( 0)= T9A( 0,J,L,K)
                  T( 1)= T9A( 1,J,L,K)
                  T( 2)= T9A( 2,J,L,K)
                  DO 431 M=0,13
  431             T(3+M)=T9A( M,J,K,L)
                  DO 439 I=0,3
                     GEIJKL= E(I,J,K,L)
C V(0,J,K,L)
                     IF(K.EQ.L) A79(17,1,I)=A79(17,1,I)-P12(J,3)*GEIJKL
                     DO 435 M=0,MM
  435                A79( M,1,I)= A79( M,1,I)+T( M)*GEIJKL
                     DO 437 M=0,20
  437                A79( M,2,I)= A79( M,2,I)+T9B( M,J,K,L)*GEIJKL
                     DO 438 M=0,9
  438                A79( M,3,I)= A79( M,3,I)+F5X(3+M,J,K,L)*GEIJKL
  439    CONTINUE
         DO 441 N=1,3
            DO 441 M=0,13
  441    A81( M,N)= ZER
         DO 449 L=1,3
            DO 449 K=1,3
               A81(12,3)= A81(12,3)+E(K,K,L,L)
               MM= 8
               IF(K.EQ.L) MM=11
               T( 0)= P3D(L,K)
               T( 1)= P24(K,L)
               T( 2)=-P3P(K,L)
               T( 3)= P3D(K,L)
               T( 4)= P24(L,K)
               T( 6)= P3P(K,L)
               T( 7)= T( 0)
               T( 8)= T( 1)
               T( 9)= T( 2)
               T(10)= T( 3)
               T(11)= T( 4)
               DO 449 J=1,3
                  DO 449 I=1,3
                     GEIJKL= E(I,J,K,L)
                     T01= P12(J,3)*GEIJKL
                     T02= P12(I,3)*GEIJKL
                     DO 443 M=0,MM
                        A81( M,1)= A81( M,1)-T9A(M,I,K,L)*T01
                        A81( M,2)= A81( M,2)+T9A(M,J,K,L)*T02
  443                CONTINUE
                     IF(I.EQ.K) A81(12,1)= A81(12,1)-T9A(12,I,K,L)*T01
                     IF(I.EQ.L) A81(13,1)= A81(13,1)-T9A(13,I,K,L)*T01
                     IF(J.EQ.K) A81(12,2)= A81(12,2)+T9A(12,J,K,L)*T02
                     IF(J.EQ.L) A81(13,2)= A81(13,2)+T9A(13,J,K,L)*T02
                     TMP= P1P(I,J)*GEIJKL
                     DO 445 M=0,4
  445                A81( M,3) = A81( M,3)+T( M)*TMP
                     IF(K.EQ.L)A81( 5,3)=A81( 5,3)+TMP
                     IF(I.EQ.J) THEN
                        DO 447 M=6,11
  447                   A81( M,3) = A81( M,3)+T( M)*GEIJKL
                     ENDIF
  449    CONTINUE
      ENDIF
  490 CONTINUE
C
      LK= LK+1
      IF(LK.EQ.1) THEN
C
CJMS  THE DEPENDENCY ON X01 AND X02 IS TREATED IN THIS BLOCK.
CJMS  IT IS ONLY CALCULATED ONCE PER OUTER LOOP (LOOPS 940 AND 930)
C
         IF(JTYPE.NE.4) THEN
            DO 510 J=0,3
               A31(3,J)= A31(0,J)+ A31(1,J)*Y02
               A32(3,J)= A32(0,J)+ A32(1,J)*Y02
  510       CONTINUE
            T01= SEJJ00*H21
            A33( 6)=-AEIJ00*Y1Y+T01
            A33( 7)= A33( 1)-(A33( 2)- A33( 3)*Y01)*Y01+T01
            A33( 8)= A33( 4)- A33( 5)*Y01
            A33(15)= A33( 9)+(A33(10)+ A33(11)*Y02)*Y02
            A33(16)= A33(12)+ A33(13)*Y02
         ENDIF
         IF(JTYPE.LT.4) GO TO 590
C
         DO 520 K=0,3
            A41(3,K)= A41(0,K)+ A41(1,K)*Y02
            A42(3,K)= A42(0,K)+ A42(1,K)*Y02
  520    CONTINUE
         A43( 6)= SEK0K0*H21
         A43( 7)=(A43( 1)+ A43( 2)*Y02)*Y02
         A43( 8)= A43( 3)*Y02
         A43(15)= A43( 9)+(A43(10)+ A43(11)*Y02)*Y02
         A43(16)= A43(12)+ A43(13)*Y02
         IF(JTYPE.LT.5) GO TO 590
C
         DO 530 N=1,3
            A51(3,N)= A51(0,N)+ A51(1,N)*Y02
            A52(3,N)= A52(0,N)+ A52(1,N)*Y02
C V(I,J,0,0)
            T01= SEJJK0(N)*H21
            A54( 6,N)=-A54(11,N)*Y1Y+T01
            A54( 7,N)= A54( 1,N)-(A54( 2,N)- A54( 3,N)*Y01)*Y01+T01
            A54( 8,N)= A54( 4,N)- A54( 5,N)*Y01
            A54(15,N)= A54( 9,N)+(A54(10,N)+ A54(11,N)*Y02)*Y02
            A54(16,N)= A54(12,N)+ A54(13,N)*Y02
C V(I,0,K,0)
            A55( 6,N)= SEKJK0(N)*H21
            A55( 7,N)=(A55( 1,N)+ A55( 2,N)*Y02)*Y02
            A55( 8,N)= A55( 3,N)*Y02
            A55(15,N)= A55( 9,N)+(A55(10,N)+ A55(11,N)*Y02)*Y02
            A55(16,N)= A55(12,N)+ A55(13,N)*Y02
  530    CONTINUE
         DO 535 M=0,3
C V(0,J,K,0)
            A53( 6,M)= SEIKK0(M)*H21
            A53( 7,M)=(A53( 1,M)+ A53( 2,M)*Y02)*Y01
            A53( 8,M)=-A53( 3,M)*Y01
            A53(15,M)= A53( 9,M)+(A53(10,M)+ A53(11,M)*Y02)*Y02
            A53(16,M)= A53(12,M)+ A53(13,M)*Y02
  535    CONTINUE
C
         DO 540 M=0,M56
            T01        = A56( 9,1,M)+ A56(13,1,M)
     2                 +(A56(10,1,M)- A56(13,1,M)+ A56(12,1,M))*Y02
            T02        = A56(11,1,M)*H21
            A56(18,1,M)=(A56( 0,1,M)+(A56( 1,1,M)+ A56( 2,1,M)*Y02)*Y02)
     2                  *Y02- T01*H21
            A56(19,1,M)= A56( 3,1,M)*Y1Y- T02
            A56(20,1,M)= A56( 4,1,M)+(A56( 5,1,M)+ A56( 6,1,M)*Y02)*Y02
     2                  +T02
            A56(21,1,M)= A56( 7,1,M)+ A56( 8,1,M)*Y02
            A56(18,2,M)=(A56( 0,2,M)+ A56( 1,2,M)*Y02)*H21
     2                 -(A56( 3,2,M)+(A56( 4,2,M)+(A56( 5,2,M)
     3                              + A56( 6,2,M)*Y02)*Y02)*Y02)
            A56(19,2,M)= A56( 2,2,M)*H21
     2                 -(A56( 7,2,M)-(A56( 8,2,M)+ A56( 9,2,M)*Y02)*Y02)
            A56(20,2,M)= A56(10,2,M)+ A56(11,2,M)*Y02
            A56(21,2,M)= A56(12,2,M)+(A56(13,2,M)+ A56(14,2,M)*Y02)*Y02
            A56(22,2,M)= A56(15,2,M)+ A56(16,2,M)*Y02
            A56(18,3,M)= A56( 0,3,M)+(A56( 1,3,M)
     2                 +(A56( 2,3,M)+ A56( 3,3,M)*Y02)*Y02)*Y02
            A56(19,3,M)= A56( 4,3,M)+(A56( 5,3,M)+ A56( 6,3,M)*Y02)*Y02
            A56(20,3,M)= A56( 7,3,M)+ A56( 8,3,M)*Y02
            A56(21,3,M)= A56( 9,3,M)
  540    CONTINUE
         IF(JTYPE.LT.6) GO TO 590
C
         A62( 6)= SEL00L*H21
         A62( 7)=(A62( 1)+ A62( 2)*Y02)*Y02
         A62( 8)= A62( 4)+ A62( 3)*Y02
         A62(15)= A62( 9)+(A62(10)+A62(11)*Y02)*Y02
         A62(16)= A62(12)+ A62(13)*Y02
         A63( 6)= SE0L0L*H21
         A63( 7)=(A63( 1)+ A63( 2)*Y02)*Y01
         A63( 8)= A63( 4)+ A63( 3)*Y02
         A63(15)= A63( 9)+(A63(10)+A63(11)*Y02)*Y02
         A63(16)= A63(12)+ A63(13)*Y02
         DO 550 N=1,3
            A64(3,N)= A64(0,N)+ A64(1,N)*Y02
            A65(3,N)= A65(0,N)+ A65(1,N)*Y02
            T02      = A66( 3,N)*Y02
            A66( 7,N)= A66( 1,N)*H21-(A66( 2,N)+T02)
     2               +(A66( 2,N)+ A66( 5,N)+T02+T02)*Y02
            A66( 8,N)= A66( 4,N)*Y01+ A66( 6,N)*Y02
            A66( 9,N)= A66( 1,N)*H21-T02*Y01
            A66(17,N)= A66(11,N)+(A66(12,N)+ A66(13,N)*Y02)*Y02
            A66(18,N)= A66(14,N)+ A66(15,N)*Y02
            T02      = A67( 4,N)*Y02
            A67( 7,N)= A67( 1,N)*H21
            A67( 8,N)=(A67( 2,N)+ A67( 3,N)*Y02)*Y02
            A67( 9,N)= A67( 5,N)+T02
            A67(10,N)= T02
            A67(17,N)= A67(11,N)+(A67(12,N)+ A67(13,N)*Y02)*Y02
            A67(18,N)= A67(14,N)+ A67(15,N)*Y02
            T02      = A68( 4,N)*Y02
            A68( 7,N)= A68( 1,N)*H21
            A68( 8,N)=(A68( 2,N)+ A68( 3,N)*Y02)*Y01
            A68( 9,N)= A68( 5,N)+T02
            A68(10,N)= A68( 4,N)-T02
            A68(17,N)= A68(11,N)+(A68(12,N)+ A68(13,N)*Y02)*Y02
            A68(18,N)= A68(14,N)+ A68(15,N)*Y02
            A69(3,N)= A69(0,N)+ A69(1,N)*Y02
            A70(3,N)= A70(0,N)+ A70(1,N)*Y02
            T02      = A71( 4,N)*Y02
            A71( 7,N)= A71( 1,N)*H21
            A71( 8,N)=(A71( 2,N)+ A71( 3,N)*Y02)*Y02
            A71( 9,N)= A71( 5,N)+T02
            A71(10,N)= T02
            A71(17,N)= A71(11,N)+(A71(12,N)+ A71(13,N)*Y02)*Y02
            A71(18,N)= A71(14,N)+ A71(15,N)*Y02
            T02      = A72( 4,N)*Y02
            A72( 7,N)= A72( 1,N)*H21
            A72( 8,N)=(A72( 2,N)+ A72( 3,N)*Y02)*Y02
            A72( 9,N)= A72( 5,N)+T02
            A72(10,N)= T02
            A72(17,N)= A72(11,N)+(A72(12,N)+ A72(13,N)*Y02)*Y02
            A72(18,N)= A72(14,N)+ A72(15,N)*Y02
C V(0,J,K,0)
            A74( 6,N)= SE0KKL(N)*H21
            A74( 7,N)=(A74( 1,N)+ A74( 2,N)*Y02)*Y01
            A74( 8,N)=-A74( 3,N)*Y01
            A74(15,N)= A74( 9,N)+(A74(10,N)+ A74(11,N)*Y02)*Y02
            A74(16,N)= A74(12,N)+ A74(13,N)*Y02
            A75( 6,N)= SE0LKL(N)*H21
            A75( 7,N)=(A75( 1,N)+ A75( 2,N)*Y02)*Y01
            A75( 8,N)= A75( 4,N)+ A75( 3,N)*Y02
            A75(15,N)= A75( 9,N)+(A75(10,N)+ A75(11,N)*Y02)*Y02
            A75(16,N)= A75(12,N)+ A75(13,N)*Y02
            A76( 6,N)= SE0JLL(N)*H21
            A76( 7,N)= A76( 1,N)+ A76( 2,N)*Y02
            A76( 8,N)= A76( 4,N)+ A76( 5,N)*Y02
            A76(15,N)= A76( 9,N)+(A76(10,N)+ A76(11,N)*Y02)*Y02
            A76(16,N)= A76(12,N)+ A76(13,N)*Y02
  550    CONTINUE
         DO 560 M=0,3
            A60(3,M)= A60(1,M)*Y02
            A61(3,M)= A61(0,M)+ A61(1,M)*Y02
            A73( 7,M)= A73( 1,M)*H21
            A73( 8,M)= A73( 2,M)+ A73( 3,M)*Y02
            A73( 9,M)= A73( 5,M)+ A73( 6,M)*Y02
            A73(17,M)= A73(11,M)+(A73(12,M)+ A73(13,M)*Y02)*Y02
            A73(18,M)= A73(14,M)+ A73(15,M)*Y02
            A77(21,1,M)=(A77( 0,1,M)+ A77( 1,1,M)*Y02)*Y1Y
     2                 +(A77( 9,1,M)+ A77(13,1,M)
     3                 +(A77(10,1,M)+ A77(12,1,M)- A77(13,1,M))*Y02)*H21
            T01= A77(11,1,M)*H21
            A77(22,1,M)= A77( 2,1,M)*Y1Y+T01
            A77(23,1,M)= A77( 6,1,M)
     2                 +(A77( 3,1,M)- A77( 6,1,M)+ A77( 7,1,M)
     3                 +(A77( 4,1,M)- A77( 7,1,M))*Y02)*Y02+T01
            A77(24,1,M)= A77( 8,1,M)+(A77( 5,1,M)- A77( 8,1,M))*Y02
            A77(21,2,M)=(A77( 0,2,M)+ A77( 1,2,M)*Y02)*H21
     2                 -(A77( 9,2,M)
     3                 +(A77( 3,2,M)- A77( 9,2,M)+ A77(10,2,M)
     4                 +(A77( 4,2,M)- A77(10,2,M)+ A77(11,2,M)
     5                 +(A77( 5,2,M)- A77(11,2,M))*Y02)*Y02)*Y02)
            A77(22,2,M)= A77( 2,2,M)*H21
     2                 + A77(12,2,M)
     3                 +(A77( 6,2,M)- A77(12,2,M)+ A77(13,2,M)
     4                 +(A77( 7,2,M)- A77(13,2,M))*Y02)*Y02
            A77(23,2,M)= A77(14,2,M)+(A77( 8,2,M)- A77(14,2,M))*Y02
            A77(24,2,M)= A77(15,2,M)+(A77(16,2,M)+ A77(17,2,M)*Y02)*Y02
            A77(25,2,M)= A77(18,2,M)+ A77(19,2,M)*Y02
            A77(21,3,M)= A77( 0,3,M)+(A77( 1,3,M)+(A77( 2,3,M)
     2                              + A77( 3,3,M)*Y02)*Y02)*Y02
            A77(22,3,M)= A77( 4,3,M)+(A77( 5,3,M)+ A77( 6,3,M)*Y02)*Y02
            A77(23,3,M)= A77( 7,3,M)+ A77( 8,3,M)*Y02
            A77(24,3,M)= A77( 9,3,M)
            T01=(A78( 3,1,M)+ A78( 4,1,M)*Y02)*Y02
            T02= A78( 5,1,M)*Y02
            A78(21,1,M)=(A78( 0,1,M)+ A78( 1,1,M)*Y02)*Y02-T01+T02
     2                 -(A78(11,1,M)- A78(12,1,M)+ A78(13,1,M))*H21
            A78(22,1,M)= A78( 2,1,M)*Y02-T02
            A78(23,1,M)= T01
     2                 -(A78( 9,1,M)- A78(10,1,M)*Y02+ A78(12,1,M))*H21
            A78(24,1,M)= A78( 5,1,M)*Y02
            A78(25,1,M)= A78( 6,1,M)+ A78( 7,1,M)*Y02
            A78(26,1,M)= A78( 8,1,M)
            A78(27,1,M)= A78(10,1,M)*Y02*H21
            T01=(A78( 9,2,M)+(A78(10,2,M)+ A78(11,2,M)*Y02)*Y02)*Y02
            T02=(A78(12,2,M)+ A78(13,2,M)*Y02)*Y02
            A78(21,2,M)=(A78( 0,2,M)+ A78( 1,2,M)*Y02)*H21-T01
            A78(22,2,M)= A78( 2,2,M)*H21+T02
            A78(23,2,M)= A78( 3,2,M)+(A78( 4,2,M)+ A78( 5,2,M)*Y02)*Y02
            A78(24,2,M)= A78( 6,2,M)+ A78( 7,2,M)*Y02
            A78(25,2,M)= A78( 8,2,M)
            A78(26,2,M)= T01
            A78(27,2,M)= T02
            A78(28,2,M)= A78(14,2,M)*Y02
            A78(29,2,M)= A78( 3,2,M)+ A78(15,2,M)
     2                 +(A78( 4,2,M)+ A78(16,2,M)
     3                 +(A78( 5,2,M)+ A78(17,2,M))*Y02)*Y02
            A78(30,2,M)= A78( 6,2,M)+ A78(18,2,M)
     2                 +(A78( 7,2,M)+ A78(19,2,M))*Y02
            A78(31,2,M)= A78( 8,2,M)+ A78(20,2,M)
            A78(21,3,M)= A78( 0,3,M)+(A78( 1,3,M)+(A78( 2,3,M)
     2                              + A78( 3,3,M)*Y02)*Y02)*Y02
            A78(22,3,M)= A78( 4,3,M)+(A78( 5,3,M)+ A78( 6,3,M)*Y02)*Y02
            A78(23,3,M)= A78( 7,3,M)+ A78( 8,3,M)*Y02
            A78(24,3,M)= A78( 9,3,M)
            T01= A79( 0,1,M)
     2         +(A79( 1,1,M)+ A79( 6,1,M)+ A79( 7,1,M)*Y02)*Y02
            T02= A79( 2,1,M)+ A79( 8,1,M)*Y02
            T03= A79( 5,1,M)*Y01
            T04= A79(12,1,M)- A79(13,1,M)*Y02+ A79(15,1,M)
            A79(21,1,M)= T01-T04*H21
            A79(22,1,M)= T02+T03
            A79(23,1,M)=(A79( 3,1,M)+ A79( 4,1,M)*Y02)*Y01+T01-T02
     2                 +(A79(14,1,M)- A79(15,1,M)+ A79(16,1,M))*H21
            A79(24,1,M)= T03
            A79(25,1,M)= A79( 9,1,M)+ A79(10,1,M)*Y02
            A79(26,1,M)= A79(11,1,M)
            A79(27,1,M)= T04
            A79(28,1,M)= A79(14,1,M)- A79(15,1,M)+ A79(16,1,M)
            A79(29,1,M)=(A79(13,1,M)*Y02- A79(13,1,M))*H21
            A79(30,1,M)= A79(17,1,M)*Y01
            T01=(A79( 9,2,M)+(A79(10,2,M)+ A79(11,2,M)*Y02)*Y02)*Y01
            T02=(A79(12,2,M)+ A79(13,2,M)*Y02)*Y01
            A79(21,2,M)=(A79( 0,2,M)+ A79( 1,2,M)*Y02)*H21+T01
            A79(22,2,M)= A79( 2,2,M)*H21-T02
            A79(23,2,M)= A79( 3,2,M)+(A79( 4,2,M)+ A79( 5,2,M)*Y02)*Y02
            A79(24,2,M)= A79( 6,2,M)+ A79( 7,2,M)*Y02
            A79(25,2,M)= A79( 8,2,M)
            A79(26,2,M)= T01
            A79(27,2,M)= T02
            A79(28,2,M)= A79(14,2,M)*Y01
            A79(29,2,M)= A79( 3,2,M)+ A79(15,2,M)
     2                 +(A79( 4,2,M)+ A79(16,2,M)
     3                 +(A79( 5,2,M)+ A79(17,2,M))*Y02)*Y02
            A79(30,2,M)= A79( 6,2,M)+ A79(18,2,M)
     2                 +(A79( 7,2,M)+ A79(19,2,M))*Y02
            A79(31,2,M)= A79( 8,2,M)+ A79(20,2,M)
            A79(21,3,M)= A79( 0,3,M)+(A79( 1,3,M)+(A79( 2,3,M)
     2                              + A79( 3,3,M)*Y02)*Y02)*Y02
            A79(22,3,M)= A79( 4,3,M)+(A79( 5,3,M)+ A79( 6,3,M)*Y02)*Y02
            A79(23,3,M)= A79( 7,3,M)+ A79( 8,3,M)*Y02
            A79(24,3,M)= A79( 9,3,M)
  560    CONTINUE
         A80(2,1)= A80(0,1)*Y1Y+A80(1,1)*H21
         A80(2,2)= A80(0,2)*Y1Y+A80(1,2)*H21
         DO 562 N=1,2
            A81(14,N)=(A81( 0,N)+A81( 1,N)*Y02)*Y02
            A81(15,N)= A81( 2,N)*Y02
            A81(16,N)=(A81( 3,N)+A81( 4,N)*Y02)*Y02
            A81(17,N)= A81( 5,N)*Y02
            A81(18,N)= A81( 6,N)+A81( 7,N)*Y02
            A81(19,N)= A81( 8,N)
            A81(20,N)= A81( 9,N)-A81(10,N)*Y02
            A81(21,N)= A81(10,N)*Y02
            A81(22,N)= A81(11,N)
            A81(23,N)= A81(12,N)
            A81(24,N)= A81(13,N)
  562    CONTINUE
         DO 564 M=14,24
  564    A81( M,1)= A81( M,1)+(A81( M,2)- A81( M,1))*Y02
         Y22= Y02*Y02
         DO 566 M=0,5
            T02   = A81(7+M,3)*H21
  566    A81(14+M,3)=A81( M,3)*Y22-T02
         A81(14,1)= A81(14,1)-(A81(17,3)+ A81(18,3)*Y02)
         A81(15,1)= A81(15,1)- A81(16,3)
         A81(16,1)= A81(16,1)+(A81(14,3)+ A81(15,3)*Y02)
         A81(17,1)= A81(17,1)+ A81(16,3)
         A81(18,1)= A81(18,1)- A81( 6,3)*H21
         T02 =(T02+ A81(20,1)+ A81(23,1))*H21
         A81(20,1)=(A81(21,1)+ A81(19,3))*H21
         A81(14,1)= A81(14,1)- A81(16,1)+ A81(17,1)
     2            -(A81(22,1)- A81(23,1)+ A81(24,1))*H21
         A81(15,1)= A81(15,1)- A81(17,1)
         A81(16,1)= A81(16,1)- T02
         DO 570 N=1,2
            A82(21,N)=(A82( 0,N)+ A82( 1,N)*Y02)
            A82(22,N)= A82( 2,N)
            A82(23,N)=(A82( 3,N)+(A82( 4,N)+ A82( 5,N)*Y02)*Y02)
            A82(24,N)=(A82( 6,N)+ A82( 7,N)*Y02)
            A82(25,N)= A82( 8,N)
            A82(26,N)=(A82( 9,N)+(A82(10,N)+ A82(11,N)*Y02)*Y02)*Y02
            A82(27,N)=(A82(12,N)+ A82(13,N)*Y02)*Y02
            A82(28,N)= A82(14,N)*Y02
            A82(29,N)= A82( 3,N)+ A82(15,N)+(A82( 4,N)+ A82(16,N)
     2               +(A82( 5,N)+ A82(17,N))*Y02)*Y02
            A82(30,N)= A82( 6,N)+ A82(18,N)+(A82( 7,N)+ A82(19,N))*Y02
            A82(31,N)= A82( 8,N)+ A82(20,N)
  570    CONTINUE
         DO 572 M=21,31
  572    A82( M,1)= A82( M,1)+(A82( M,2)-A82( M,1))*Y02
         DO 574 N=1,2
            A83(15,N)= A83( 0,N)+ A83( 1,N)*Y02
            A83(16,N)= A83( 2,N)
            A83(17,N)= A83( 3,N)+(A83( 4,N)+ A83( 5,N)*Y02)*Y02
            A83(18,N)= A83( 6,N)+ A83( 7,N)*Y02
            A83(19,N)= A83( 8,N)
            A83(20,N)=(A83( 9,N)+(A83(10,N)+ A83(11,N)*Y02)*Y02)*H21
            A83(21,N)=(A83(12,N)+ A83(13,N)*Y02)*H21
            A83(22,N)= A83(14,N)*H21
  574    CONTINUE
         T01= A83(15,1)*H21
         T02= A83(16,1)*H21
         A82(21,1)=(A82(21,1)- A86( 0)*H21)*H21
     2                           - A82(26,1)-(A83(17,1)*Y22+ A83(20,1))
         A82(22,1)= A82(22,1)*H21+ A82(27,1)+(A83(18,1)*Y22+ A83(21,1))
         A82(23,1)= A82(23,1)+ A83(20,2)+T01
         A82(24,1)= A82(24,1)+ A83(21,2)+T01-T02
         A82(25,1)= A82(25,1)- A83(22,2)+T02
         A82(28,1)= A82(28,1)-(A83(19,1)*Y22+ A83(22,1))
         A83(15,2)= A83(15,2)*H21- A82(29,1)
         A83(16,2)= A83(16,2)*H21+ A82(30,1)
         DO 576 N=1,2
            A84(10,N)= A84( 0,N)+(A84( 1,N)+(A84( 2,N)
     2                                     + A84( 3,N)*Y02)*Y02)*Y02
            A84(11,N)= A84( 4,N)+(A84( 5,N)+ A84( 6,N)*Y02)*Y02
            A84(12,N)= A84( 7,N)+ A84( 8,N)*Y02
            A84(13,N)= A84( 9,N)
  576    CONTINUE
         DO 578 M=10,13
  578    A84( M,1)= A84( M,1)+(A84( M,2)- A84( M,1))*Y02
         A84(10,1)= A84(10,1)-(A86(1)+(A86(2)+ A86(3)*Y02)*Y02)*H21
         A84(11,1)= A84(11,1)+(A86(4)+ A86(5)*Y02)*H21
         A84(12,1)= A84(12,1)- A86(6)*H21
         DO 580 N=1,2
            A85(10,N)= A85( 0,N)+(A85( 1,N)+(A85( 2,N)
     2                                     + A85( 3,N)*Y02)*Y02)*Y02
            A85(11,N)= A85( 4,N)+(A85( 5,N)+ A85( 6,N)*Y02)*Y02
            A85(12,N)= A85( 7,N)+ A85( 8,N)*Y02
            A85(13,N)= A85( 9,N)
  580    CONTINUE
         A85(10,2)= A85(10,2)- A85(10,1)+ A85(11,1)
         A85(11,2)= A85(11,2)- A85(11,1)+ A85(12,1)
         A85(12,2)= A85(12,2)- A85(12,1)+ A85(13,1)
         A85(13,2)= A85(13,2)- A85(13,1)
         Q4(0)= S4( 0)+(S4( 1)+(S4( 2)+(S4( 3)+S4( 4)*Y02)*Y02)*Y02)*Y02
         Q4(1)= S4( 5)+(S4( 6)+(S4( 7)+ S4( 8)*Y02)*Y02)*Y02
         Q4(2)= S4( 9)+(S4(10)+ S4(11)*Y02)*Y02
         Q4(3)= S4(12)+ S4(13)*Y02
         Q4(4)= S4(14)
      ENDIF
  590 CONTINUE
C
      IF(JTYPE.NE.4) THEN
         DO 610 M=0,3
            T(0)= A31(3,M)- A31(0,M)- A31(1,M)
            T(1)= A31(3,M)- A31(2,M)*Y04
            C31(1,M)= T(0)*F1(0)-T(1)*F1(1)
            C31(2,M)= T(0)*F2(0)-T(1)*F2(1)
            T(0)= A32(3,M)- A32(0,M)
            T(1)= A32(3,M)- A32(2,M)*Y04
            C32(1,M)= T(0)*F1(0)-T(1)*F1(1)
            C32(2,M)= T(0)*F2(0)-T(1)*F2(1)
  610    CONTINUE
         T(0)= A33( 6)
         T(1)= A33( 7)- A33( 8)*Y04
         T(2)= A33(15)-(A33(16)- A33(14)*Y04)*Y04
         FIJ00(1)= T(0)*F1(0)-T(1)*F1(1)+T(2)*F1(2)
         FIJ00(2)= T(0)*F2(0)-T(1)*F2(1)+T(2)*F2(2)
      ENDIF
      IF(JTYPE.LT.4) GO TO 690
C
      DO 620 M=0,3
         T(0)= A41(2,M)*Y04
         T(1)=(A41(3,M)-T(0))*XX1
         C41(1,M)= T(0)*F1(0)-T(1)*F1(1)
         C41(2,M)= T(0)*F2(0)-T(1)*F2(1)
         T(0)= A42(3,M)- A42(0,M)
         T(1)= A42(3,M)- A42(2,M)*Y04
         C42(1,M)= T(0)*F1(0)-T(1)*F1(1)
         C42(2,M)= T(0)*F2(0)-T(1)*F2(1)
  620 CONTINUE
      T01 = A43( 8)*Y04
      T(0)= T01
      T(1)= T01+(A43( 4)- A43( 5)*Y04)*Y04
     2         +(A43( 6)+ A43( 7)-T01)*XX1
      T(2)=(A43(15)-(A43(16)- A43(14)*Y04)*Y04)*XX1
      FI0K0(1)= T(0)*F1(0)-T(1)*F1(1)+T(2)*F1(2)
      FI0K0(2)= T(0)*F2(0)-T(1)*F2(1)+T(2)*F2(2)
      IF(JTYPE.LT.5) GO TO 690
C
      DO 630 N=1,3
         T(0)= A51(2,N)*Y04
         T(1)=(A51(3,N)-T(0))*XX1
         C51(1,N)= T(0)*F1(0)-T(1)*F1(1)
         C51(2,N)= T(0)*F2(0)-T(1)*F2(1)
         T(0)= A52(3,N)- A52(0,N)- A52(1,N)
         T(1)= A52(3,N)- A52(2,N)*Y04
         C52(1,N)= T(0)*F1(0)-T(1)*F1(1)
         C52(2,N)= T(0)*F2(0)-T(1)*F2(1)
C V(I,J,0,0)
         T(0)= A54( 6,N)
         T(1)= A54( 7,N)- A54( 8,N)*Y04
         T(2)= A54(15,N)-(A54(16,N)- A54(14,N)*Y04)*Y04
         C54(1,N)= T(0)*F1(0)-T(1)*F1(1)+T(2)*F1(2)
         C54(2,N)= T(0)*F2(0)-T(1)*F2(1)+T(2)*F2(2)
C V(I,0,K,0)
         T01 = A55( 8,N)*Y04
         T(0)= T01
         T(1)= T01+(A55( 4,N)- A55( 5,N)*Y04)*Y04
     2            +(A55( 6,N)+ A55( 7,N)-T01)*XX1
         T(2)=(A55(15,N)-(A55(16,N)- A55(14,N)*Y04)*Y04)*XX1
         C55(1,N)= T(0)*F1(0)-T(1)*F1(1)+T(2)*F1(2)
         C55(2,N)= T(0)*F2(0)-T(1)*F2(1)+T(2)*F2(2)
  630 CONTINUE
      DO 640 M=0,3
C V(0,J,K,0)
         T01 = A53( 8,M)*Y04
         T(0)= T01
         T(1)= T01+(A53( 3,M)+ A53( 4,M)- A53( 5,M)*Y04)*Y04
     2            +(A53( 6,M)- A53( 7,M)- T01)*XX1
         T(2)=(A53(15,M)-(A53(16,M)- A53(14,M)*Y04)*Y04)*XX1
         C53(1,M)= T(0)*F1(0)-T(1)*F1(1)+T(2)*F1(2)
         C53(2,M)= T(0)*F2(0)-T(1)*F2(1)+T(2)*F2(2)
  640 CONTINUE
      DO 650 M=0,M56
         T(0)=-A56(19,1,M)*Y04
         T(1)=(A56(18,1,M)-T(0))*XX1
     2       +(A56(20,1,M)+ A56(21,1,M)*Y04)*Y04
         T(2)=(A56(18,2,M)+(A56(19,2,M)- A56(20,2,M)*Y04)*Y04)*XX1
     2       -(A56(21,2,M)-(A56(22,2,M)+ A56(17,2,M)*Y04)*Y04)*Y04
         T(3)=(A56(18,3,M)+(A56(19,3,M)+(A56(20,3,M)
     2                                 + A56(21,3,M)*Y04)*Y04)*Y04)*XX1
         C56(1,M)= T(0)*F1(0)-T(1)*F1(1)-T(2)*F1(2)+T(3)*F1(3)
         C56(2,M)= T(0)*F2(0)-T(1)*F2(1)-T(2)*F2(2)+T(3)*F2(3)
  650 CONTINUE
      IF(JTYPE.LT.6) GO TO 690
C
      T02 = A62( 4)-A62( 8)
      T(0)= T02*Y03
      T(1)=(A62( 6)+ A62( 7)-T(0)+T02)*XX1
     2    -(A62( 8)+ A62( 5)*Y04)*Y03
      T(2)=(A62(15)-(A62(16)-A62(14)*Y04)*Y04)*XX1
      FI00L(1)= T(0)*F1(0)-T(1)*F1(1)+T(2)*F1(2)
      FI00L(2)= T(0)*F2(0)-T(1)*F2(1)+T(2)*F2(2)
      T02 = A63( 3)+A63( 4)-A63( 8)
      T(0)= T02*Y03
      T(1)=(A63( 6)- A63( 7)-T(0)+T02)*XX1
     2    -(A63( 8)+ A63( 5)*Y04)*Y03
      T(2)=(A63(15)-(A63(16)-A63(14)*Y04)*Y04)*XX1
      F0J0L(1)= T(0)*F1(0)-T(1)*F1(1)+T(2)*F1(2)
      F0J0L(2)= T(0)*F2(0)-T(1)*F2(1)+T(2)*F2(2)
      DO 660 N=1,3
         T(0)= A64(3,N)- A64(0,N)- A64(1,N)
         T(1)= A64(3,N)- A64(2,N)*Y04
         C64(1,N)= T(0)*F1(0)-T(1)*F1(1)
         C64(2,N)= T(0)*F2(0)-T(1)*F2(1)
         T(0)=-A65(2,N)*Y03
         T(1)=(A65(3,N)- A65(2,N)-T(0))*XX1
         C65(1,N)= T(0)*F1(0)-T(1)*F1(1)
         C65(2,N)= T(0)*F2(0)-T(1)*F2(1)
         T(0)= A66( 9,N)
         T(1)= A66( 7,N)+ A66( 8,N)*Y04
         T(2)= A66(17,N)-(A66(18,N)- A66(16,N)*Y04)*Y04
         C66(1,N)= T(0)*F1(0)-T(1)*F1(1)+T(2)*F1(2)
         C66(2,N)= T(0)*F2(0)-T(1)*F2(1)+T(2)*F2(2)
         T(0)=-A67(10,N)*Y03
         T(1)=(A67( 7,N)+ A67( 8,N)- A67(10,N)-T(0))*XX1
     2       -(A67( 9,N)+ A67( 6,N)*Y04)*Y03
         T(2)=(A67(17,N)-(A67(18,N)- A67(16,N)*Y04)*Y04)*XX1
         C67(1,N)= T(0)*F1(0)-T(1)*F1(1)+T(2)*F1(2)
         C67(2,N)= T(0)*F2(0)-T(1)*F2(1)+T(2)*F2(2)
         T(0)= A68(10,N)*Y03
         T(1)=(A68( 7,N)- A68( 8,N)+ A68(10,N)-T(0))*XX1
     2       -(A68( 9,N)+ A68( 6,N)*Y04)*Y03
         T(2)=(A68(17,N)-(A68(18,N)- A68(16,N)*Y04)*Y04)*XX1
         C68(1,N)= T(0)*F1(0)-T(1)*F1(1)+T(2)*F1(2)
         C68(2,N)= T(0)*F2(0)-T(1)*F2(1)+T(2)*F2(2)
C V(0,0,L,0)
         T(0)= A69(2,N)*Y04
         T(1)=(A69(3,N)-T(0))*XX1
         C69(1,N)= T(0)*F1(0)-T(1)*F1(1)
         C69(2,N)= T(0)*F2(0)-T(1)*F2(1)
C V(0,0,0,L)
         T02 = A70(2,N)*Y04
         T(0)=-A70(2,N)+T02
         T(1)=(A70(3,N)-T02)*XX1
         C70(1,N)= T(0)*F1(0)-T(1)*F1(1)
         C70(2,N)= T(0)*F2(0)-T(1)*F2(1)
         T(0)= A71(10,N)*Y04
         T(1)=(A71( 7,N)+ A71( 8,N)-T(0))*XX1
     2       +(A71( 9,N)+ A71( 6,N)*Y04)*Y04
         T(2)=(A71(17,N)-(A71(18,N)- A71(16,N)*Y04)*Y04)*XX1
         C71(1,N)= T(0)*F1(0)-T(1)*F1(1)+T(2)*F1(2)
         C71(2,N)= T(0)*F2(0)-T(1)*F2(1)+T(2)*F2(2)
         T(0)=-A72(10,N)*Y03
         T(1)=(A72( 7,N)+ A72( 8,N)- A72(10,N)-T(0))*XX1
     2       -(A72( 9,N)+ A72( 6,N)*Y04)*Y03
         T(2)=(A72(17,N)-(A72(18,N)- A72(16,N)*Y04)*Y04)*XX1
         C72(1,N)= T(0)*F1(0)-T(1)*F1(1)+T(2)*F1(2)
         C72(2,N)= T(0)*F2(0)-T(1)*F2(1)+T(2)*F2(2)
C V(0,J,K,0)
         T01 = A74( 8,N)*Y04
         T(0)= T01
         T(1)= T01+(A74( 3,N)+ A74( 4,N)- A74( 5,N)*Y04)*Y04
     2            +(A74( 6,N)- A74( 7,N)-T01)*XX1
         T(2)=(A74(15,N)-(A74(16,N)- A74(14,N)*Y04)*Y04)*XX1
         C74(1,N)= T(0)*F1(0)-T(1)*F1(1)+T(2)*F1(2)
         C74(2,N)= T(0)*F2(0)-T(1)*F2(1)+T(2)*F2(2)
         T02 = A75( 3,N)+ A75( 4,N)- A75( 8,N)
         T(0)= T02*Y03
         T(1)=(A75( 6,N)- A75( 7,N)-T(0)+T02)*XX1
     2       -(A75( 8,N)+ A75( 5,N)*Y04)*Y03
         T(2)=(A75(15,N)-(A75(16,N)- A75(14,N)*Y04)*Y04)*XX1
         C75(1,N)= T(0)*F1(0)-T(1)*F1(1)+T(2)*F1(2)
         C75(2,N)= T(0)*F2(0)-T(1)*F2(1)+T(2)*F2(2)
         T01 = A76( 6,N)*XX1
         T(0)= A76( 3,N)*Y3Y-T01
         T(1)=(T01- A76( 7,N)-(A76( 3,N)- A76( 7,N)- A76( 8,N)
     2                       -(A76( 3,N)+ A76( 3,N))*Y04)*Y04)*XX1
         T(2)=(A76(15,N)-(A76(16,N)- A76(14,N)*Y04)*Y04)*XX2
         C76(1,N)= T(0)*F1(0)-T(1)*F1(1)+T(2)*F1(2)
         C76(2,N)= T(0)*F2(0)-T(1)*F2(1)+T(2)*F2(2)
  660 CONTINUE
      DO 670 M=0,3
         T(0)= A60(3,M)
         T(1)= A60(3,M)+ A60(0,M)- A60(2,M)*Y04
         C60(1,M)= T(0)*F1(0)-T(1)*F1(1)
         C60(2,M)= T(0)*F2(0)-T(1)*F2(1)
         T01 =-A61(2,M)*Y03
         T(0)= T01
         T(1)=(A61(3,M)- A61(2,M)-T01)*XX1
         C61(1,M)= T(0)*F1(0)-T(1)*F1(1)
         C61(2,M)= T(0)*F2(0)-T(1)*F2(1)
         T(0)= A73( 4,M)*Y3Y- A73( 7,M)*XX1
         T(1)=(A73( 8,M)+T(0)
     2       -(A73( 8,M)+ A73( 9,M)+ A73( 4,M)*Y04)*Y04)*XX1
         T(2)=(A73(17,M)-(A73(18,M)- A73(16,M)*Y04)*Y04)*XX2
         C73(1,M)= T(0)*F1(0)+T(1)*F1(1)+T(2)*F1(2)
         C73(2,M)= T(0)*F2(0)+T(1)*F2(1)+T(2)*F2(2)
         T(0)= A77(22,1,M)*Y03
         T(1)=(A77(21,1,M)+ A77(22,1,M)*Y04)*XX1
     2       +(A77(23,1,M)+ A77(24,1,M)*Y04)*Y03
         T(2)=(A77(21,2,M)+(A77(22,2,M)+ A77(23,2,M)*Y04)*Y04)*XX1
     2       -(A77(24,2,M)-(A77(25,2,M)+ A77(20,2,M)*Y04)*Y04)*Y03
         T(3)=(A77(21,3,M)+(A77(22,3,M)+(A77(23,3,M)
     2                                 + A77(24,3,M)*Y04)*Y04)*Y04)*XX1
         C77(1,M)= T(0)*F1(0)-T(1)*F1(1)-T(2)*F1(2)+T(3)*F1(3)
         C77(2,M)= T(0)*F2(0)-T(1)*F2(1)-T(2)*F2(2)+T(3)*F2(3)
         T(0)=(A78(22,1,M)+ A78(24,1,M))*Y3Y+ A78(27,1,M)*XX1
         T(1)=(A78(23,1,M)+(A78(21,1,M)+A78(22,1,M)*Y04)*Y04)*XX1
     2       +(A78(25,1,M)+ A78(26,1,M)*Y04)*Y3Y- A78(27,1,M)*XX2
         T(2)=(A78(21,2,M)+(A78(22,2,M)+ A78(28,2,M)*Y04)*Y04)*XX2
     2       +(A78(23,2,M)-(A78(24,2,M)+ A78(25,2,M)*Y04)*Y04)*XX1
     3       -(A78(29,2,M)-(A78(30,2,M)+ A78(31,2,M)*Y04)*Y04)*Y41
         T(3)=(A78(21,3,M)+(A78(22,3,M)+(A78(23,3,M)
     2                                 + A78(24,3,M)*Y04)*Y04)*Y04)*XX2
         C78(1,M)= T(0)*F1(0)-T(1)*F1(1)-T(2)*F1(2)+T(3)*F1(3)
         C78(2,M)= T(0)*F2(0)-T(1)*F2(1)-T(2)*F2(2)+T(3)*F2(3)
         T(0)=-A79(24,1,M)*Y3Y+ A79(30,1,M)*H43
         T(1)=(A79(21,1,M)-(A79(23,1,M)+ A79(22,1,M)*Y04)*Y04)*XX1
     2       +(A79(25,1,M)+ A79(26,1,M)*Y04)*Y3Y- A79(29,1,M)*XX2
         T(2)=(A79(21,2,M)+(A79(22,2,M)- A79(28,2,M)*Y04)*Y04)*XX2
     2       +(A79(23,2,M)-(A79(24,2,M)+ A79(25,2,M)*Y04)*Y04)*XX1
     3       -(A79(29,2,M)-(A79(30,2,M)+ A79(31,2,M)*Y04)*Y04)*Y41
         T(3)=(A79(21,3,M)+(A79(22,3,M)+(A79(23,3,M)
     2                                 + A79(24,3,M)*Y04)*Y04)*Y04)*XX2
         C79(1,M)= T(0)*F1(0)-T(1)*F1(1)-T(2)*F1(2)+T(3)*F1(3)
         C79(2,M)= T(0)*F2(0)-T(1)*F2(1)-T(2)*F2(2)+T(3)*F2(3)
  670 CONTINUE
      T(0)= A80( 2,1)*Y3Y+ A80( 2,2)*H43
      T(1)=(A81(16,1)+(A81(14,1)+A81(15,1)*Y04)*Y04)*XX1
     2    +(A81(18,1)+ A81(19,1)*Y04)*Y3Y- A81(20,1)*XX2
      T(2)=(A82(21,1)+(A82(22,1)+ A82(28,1)*Y04)*Y04)*XX2
     2    +(A82(23,1)-(A82(24,1)+ A82(25,1)*Y04)*Y04)*XX1
     3    +(A83(15,2)+(A83(16,2)+ A82(31,1)*Y04)*Y04)*Y41
     4    -(A83(17,2)-(A83(18,2)- A83(19,2)*Y04)*Y04)*Y3Y
      T(3)=(A84(10,1)+(A84(11,1)+(A84(12,1)+A84(13,1)*Y04)*Y04)*Y04)*XX2
     2    +(A85(10,1)+(A85(10,2)+(A85(11,2)+(A85(12,2)
     3                          + A85(13,2)*Y04)*Y04)*Y04)*Y04)*XX1
      T(4)=(Q4(0)+(Q4(1)+(Q4(2)+(Q4(3)+Q4(4)*Y04)*Y04)*Y04)*Y04)*XX2
      FIJKL(1)= T(0)*F1(0)-T(1)*F1(1)-T(2)*F1(2)+T(3)*F1(3)+T(4)*F1(4)
      FIJKL(2)= T(0)*F2(0)-T(1)*F2(1)-T(2)*F2(2)+T(3)*F2(3)+T(4)*F2(4)
  690 CONTINUE
C
C BEGINING OF THE TWO PASSES THROUGH THE INTEGRAL CALCULATIONS
C
                  DO 800 ICP=1,2
                     T20= FQ(ICP-1)*WS(2)
                     VE0= FQ(ICP-1)*ES1
                     IF(JTYPE.EQ.3) THEN
C
C (SS,SS), (PS,SS), (PP,SS) AND (SP,SS) SECTION
C
                        T30= FQ(ICP-1)*WS(3)
                        DO 693 N=1,3
                           VEA(  N)= E(N,0,0,0)*T20  +C31(ICP,N)*WS(4)
                           VEA(3+N)= E(0,N,0,0)*T30  +C32(ICP,N)*WS(4)
  693                   CONTINUE
                        VE0= VE0+C32(ICP,0)*WS(2)
     2                          +C31(ICP,0)*WS(3)+FIJ00(ICP)*WS(4)
                        GO TO 700
                     ENDIF
                     T50= FQ(ICP-1)*WS(5)
                     IF(JTYPE.EQ.4) THEN
C
C (SS,SS), (PS,SS), (PS,PS) AND (SS,PS) SECTION
C
                        DO 694 N=1,3
                           VEA(  N)= E(N,0,0,0)*T20  +C41(ICP,N)*WS(6)
                           VEA(6+N)= E(0,0,N,0)*T50  +C42(ICP,N)*WS(6)
  694                   CONTINUE
                        VE0= VE0+C42(ICP,0)*WS(2)
     2                          +C41(ICP,0)*WS(5)+FI0K0(ICP)*WS(6)
                        GO TO 700
                     ENDIF
C
C (SS,SS), (PS,SS), (PP,SS), (SP,SS), (PS,PS), (SS,PS), (PP,PS) AND
C (SP,PS) SECTION
C
                     T30= FQ(ICP-1)*WS(3)
                     DO 695 N=1,3
                        VEA(  N)= E(N,0,0,0)*T20  +C31(ICP,N)*WS(4)
     2                           +C41(ICP,N)*WS(6)+C53(ICP,N)*WS(8)
                        VEA(3+N)= E(0,N,0,0)*T30  +C32(ICP,N)*WS(4)
     2                           +C51(ICP,N)*WS(7)+C55(ICP,N)*WS(8)
                        VEA(6+N)= E(0,0,N,0)*T50  +C42(ICP,N)*WS(6)
     2                           +C52(ICP,N)*WS(7)+C54(ICP,N)*WS(8)
  695                CONTINUE
                     VE0= VE0+C32(ICP,0)*WS(2)
     2                       +C31(ICP,0)*WS(3)+FIJ00(ICP)*WS(4)
     3                       +C41(ICP,0)*WS(5)+FI0K0(ICP)*WS(6)
     4                       +C53(ICP,0)*WS(7)+C56(ICP,0)*WS(8)
                     IF(JTYPE.LT.6) GO TO 700
C
C (PP,PP), (PP,SP), (PS,PP), (SP,PP), (SS,PP), (SP,SP), (PS,SP) AND
C (SS,SP) SECTION
C
                     T10= FQ(ICP-1)*WP(1)
                     DO 696 N=1,3
                        VEA(  N)= VEA(  N)
     2                           +C61(ICP,N)*WP(2)+C68(ICP,N)*WP(4)
     3                           +C73(ICP,N)*WP(6)+C79(ICP,N)*WP(8)
                        VEA(3+N)= VEA(3+N)
     2                           +C65(ICP,N)*WP(3)+C67(ICP,N)*WP(4)
     3                           +C76(ICP,N)*WP(7)+C78(ICP,N)*WP(8)
                        VEA(6+N)= VEA(6+N)
     2                           +C70(ICP,N)*WP(5)+C72(ICP,N)*WP(6)
     3                           +C75(ICP,N)*WP(7)+C77(ICP,N)*WP(8)
                        VEA(9+N)= E(0,0,0,N)*T10  +C60(ICP,N)*WP(2)
     2                           +C64(ICP,N)*WP(3)+C66(ICP,N)*WP(4)
     3                           +C69(ICP,N)*WP(5)+C71(ICP,N)*WP(6)
     4                           +C74(ICP,N)*WP(7)+C56(ICP,N)*WP(8)
  696                CONTINUE
                     VE0= VE0+C61(ICP,0)*WP(1)+FI00L(ICP)*WP(2)
     2                       +F0J0L(ICP)*WP(3)+C77(ICP,0)*WP(4)
     3                       +C73(ICP,0)*WP(5)+C78(ICP,0)*WP(6)
     4                       +C79(ICP,0)*WP(7)+FIJKL(ICP)*WP(8)
  700                CONTINUE
                     IF(ICP.EQ.1) THEN
                        VE0SUM= VE0SUM+VE0
                        FE0= F34*VE0
                        DO 720 M=1,3
                           VES(  M)= VES(  M)+VEA(  M)
                           VES(3+M)= VES(3+M)+VEA(3+M)
                           T01 = VEA(6+M)
                           T02 = VEA(9+M)
                           WES(  M)=-T01+(T01+T02)*Y03
  720                   CONTINUE
                     ELSE
                        FE1= X24*VE0
                        DO 730 M=1,3
                           T01 = VEA(  M)+VEA(3+M)
                           T02 = VEA(6+M)+VEA(9+M)
                           WES(3+M)=-T01+(T01+T02)*Z02
  730                   CONTINUE
                     ENDIF
  800             CONTINUE
C
C SUMMATION OF CONTRIBUTIONS FROM THE UNCONTRACTED GAUSSIANS
C
                  DO 905 M=1,3
                     WES(  M)= WES(  M)+P34(M,3)*FE0
                     WES(3+M)= WES(3+M)-P1(M)*FE1
                     VES(6+M)= VES(6+M)+WES(3+M)
                     FCS(3,M)= FCS(3,M)-WES(3+M)*Y03+WES(  M)
  905             CONTINUE
C
  910          KL=KL+1
  920       K0=K0+MXGSH
C
            TMP= F12*VE0SUM
            DO 925 M=1,3
               T01= VES(  M)
               T02= VES(3+M)+VES(6+M)
               T01=-T01+(T01+T02)*Y01+P12(M,3)*TMP
               FCS(1,M)= FCS(1,M)+T01
               FCS(2,M)= FCS(2,M)-T01+VES(6+M)
  925       CONTINUE
C
  930    IJ=IJ+1
  940 I0=I0+MXGSH
C
      RETURN
      END
