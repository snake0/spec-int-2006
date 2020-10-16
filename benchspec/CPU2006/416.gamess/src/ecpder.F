C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C 26 MAR 03 - JMS - EXTENSIVE REWRITE,
C                   FORMII: STORAGE FIXED, FORMDR: F' TYPO, G' PARENS
C 26 MAR 02 - JMS - FORMII: CORRECT LOOP INDEX FOR F,G FUNCTIONS
C  8 OCT 01 - JMS - FORMDR,FORMII: CORRECT TYPOS, ESP. G2(6) IN F' GRAD
C 25 JUN 01 - MWS - ALTER COMMON BLOCK WFNOPT
C 26 OCT 00 - MWS - INTRODUCE MXAO PARAMETER
C 13 MAR 99 - MWS - ECPHES,ECPDER: FIX LENGTH OF INTEGER DAF RECORDS
C  1 DEC 98 - BMB - NEW FILE CONTAINING CODE FOR ECP DERIVATIVES
C
C*MODULE ECPDER  *DECK ECPHES
      SUBROUTINE ECPHES(EXETYP,OUT,EG,EH,DAB,FD)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL OUT
      DIMENSION EG(3,*),EH(9,*),DAB(*),FD(*)
C
      PARAMETER (MXATM=500)
C
      COMMON /ECPDIM/ NCOEF1,NCOEF2,J1LEN,J2LEN,LLIM,NLIM,NTLIM,J4LEN
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      LOGICAL SOME
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK,DBUGME/8HCHECK   ,8HECPHES  /
#else
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR,DBUGME_STR/"CHECK   ","ECPHES  "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RHF  ,UHF   /8HRHF     ,8HUHF     /
#else
      CHARACTER*8 :: UHF_STR
      EQUIVALENCE (UHF, UHF_STR)
      CHARACTER*8 :: RHF_STR
      EQUIVALENCE (RHF, RHF_STR)
      DATA RHF_STR ,UHF_STR/"RHF     ","UHF     "/
#endif
C
      SOME= MASWRK .AND.(OUT .OR. EXETYP.EQ.DBUGME)
      L2 =(NUM*NUM+NUM)/2
      T0 = 0.0D+00
      CALL TSECND(T0)
C
C SIMPLE DRIVER TO ALLOCATE MEMORY AND CALL THE ROUTINES TO
C COMPUTE THE GRADIENT AND HESSIAN ECP MODIFICATIONS
C
      CALL VALFM(LOADFM)
      LDCF1 = LOADFM+ 1
      LJLN  = LDCF1 + NCOEF1
      LLB1  = LJLN  +(J1LEN-1)/NWDVAR+1
      LDCF4 = LLB1  +(9*NCOEF1-1)/NWDVAR+1
      LDCF2 = LDCF4 + J4LEN
      LJ2N  = LDCF2 + NCOEF2
      LLB2  = LJ2N  +(J2LEN-1)/NWDVAR+1
      LFPQR = LLB2  +(6*NCOEF2)/NWDVAR
      LFP   = LFPQR + 25*25*25
      LFP2  = LFP   + 2*11*11*11
      LZLM  = LFP2  + 2*11*11*11
      LLMF  = LZLM  + 581
      LLMX  = LLMF  +(121+1)/NWDVAR
      LLMY  = LLMX  +(581+1)/NWDVAR
      LLMZ  = LLMY  +(581+1)/NWDVAR
C
C THIS ARRAY IS USED ONLY BY THE GRADIENT PORTION
C
      LGG   = LLMZ  +(581+1)/NWDVAR
      LGG2  = LGG   + 21*15
      LXIN  = LGG2  + 21*15
      LYIN  = LXIN  + 15*15
      LZIN  = LYIN  + 15*15
      LDEECP= LZIN  + 15*15
C
C THESE ARE USED ONLY BY THE HESSIAN CODE AND THUS OVERWRITE
C THE GRADIENT ONLY TERMS
C
      LGL11 = LLMZ  +(581+1)/NWDVAR
      LGL2  = LGL11 + 9*15*15
      LGM2  = LGL2  + 9*15*15
      LGLL  = LGM2  + 21*21
      LGLP2 = LGLL  + 21*21
      LGM1M1= LGLP2 + 21*21
      LGM1P1= LGM1M1+ 21*21
      LGP1M1= LGM1P1+ 21*21
      LGP1P1= LGP1M1+ 21*21
      LAST  = LGP1P1+ 21*21
      LAST  = MAX0((LDEECP+3*NAT),LAST)
      NECP  = LAST  - LDCF1
      CALL GETFM(NECP)
      IF(EXETYP.EQ.CHECK) GO TO 110
C
C     -----  READ IN ECP FORMULAS AND DATA  -----
C
      LDAF91=(J1LEN-1)/NWDVAR+1+(9*NCOEF1-1)/NWDVAR+1
      LDAF93=(J2LEN-1)/NWDVAR+1+(6*NCOEF2)/NWDVAR
      CALL DAREAD(IDAF,IODA,X(LDCF1),NCOEF1,90,0)
      CALL DAREAD(IDAF,IODA,X(LJLN) ,LDAF91,91,1)
      CALL DAREAD(IDAF,IODA,X(LDCF2),NCOEF2,92,0)
      CALL DAREAD(IDAF,IODA,X(LJ2N) ,LDAF93,93,1)
C
C THE NEXT CALLS JUST FILL OUT TABLES WHICH HAVE PROBABLY ALREADY
C BEEN DONE IN THE ENERGY STEP, BUT JUST IN CASE THIS IS A RESTART RUN
C
      CALL DAWT
      CALL ERRT
      CALL DAWERT
      CALL ECPINI(X(LLMF),X(LLMX),X(LLMY),X(LLMZ))
      CALL ZTAB(X(LZLM))
      CALL FTAB(X(LFPQR),NLIM-1)
      CALL ECCOD3(X(LFPQR),X(LDCF4),X(LZLM),X(LLMF),X(LLMX),X(LLMY),
     *            X(LLMZ))
C
C NOW COMPUTE THE FIRST DERIVATIVE TERMS!
C
      CALL ECP1D(EG,DAB,L2,X(LDCF1),X(LJLN),X(LLB1),X(LDCF4),
     *     X(LDCF2),X(LJ2N),X(LLB2),X(LDEECP),X(LFPQR),X(LFP),X(LFP2),
     *     X(LGG),X(LGG2),X(LXIN),X(LYIN),X(LZIN),X(LZLM),X(LLMF),
     *     X(LLMX),X(LLMY),X(LLMZ),FD,1)
      IF(SOME) THEN
         CALL TSECND(T1)
         TG = T1-T0
         WRITE(IW,9010) TG
         T0 = T1
      END IF
C
C COMPUTE THE 2ND DERIVATIVE TERMS
C
      CALL ECP2D(EG,EH,DAB,L2,X(LDCF1),X(LJLN),X(LLB1),X(LDCF4),
     *     X(LDCF2),X(LJ2N),X(LLB2),X(LFPQR),X(LGL11),X(LGL2),
     *     X(LFP),X(LGM2),X(LGLL),X(LGLP2),X(LGM1M1),X(LGM1P1),
     *     X(LGP1M1),X(LGP1P1),X(LZLM),X(LLMF),X(LLMX),X(LLMY),X(LLMZ))
      IF(SOME) THEN
         CALL TSECND(T1)
         TG = T1-T0
         WRITE(IW,9020) TG
         T0 = T1
      END IF
  110 CONTINUE
      CALL RETFM(NECP)
C
C SYMMETRIZE THE FOCK DERIVATIVES (SINGLE DETERMINANT JOBS
C WILL CATCH THIS LATER, AFTER 2 ELECTRON CONTRIBUTIONS)
C
      IF(SCFTYP.EQ.RHF .OR. SCFTYP.EQ.UHF) RETURN
C
      LTX  = LOADFM+ 1
      LTY  = LTX   + 35*35
      LTZ  = LTY   + 35*35
      LUX  = LTZ   + 35*35
      LUY  = LUX   + 35*35
      LUZ  = LUY   + 35*35
      LAST = LUZ   + 35*35
      NEED = LAST  - LTX
      CALL GETFM(NEED)
      CALL SYMDF(FD,L2,1,3,NAT,1,1,2,3,
     *           X(LTX),X(LTY),X(LTZ),X(LUX),X(LUY),X(LUZ))
      CALL RETFM(NEED)
C
      RETURN
 9010 FORMAT(' TIME TO DO ECP 1ST DERIVATIVE INTEGRALS=',F10.2)
 9020 FORMAT(' TIME TO DO ECP 2ND DERIVATIVE INTEGRALS=',F10.2)
      END
C*MODULE ECPDER  *DECK ECPGRD
      SUBROUTINE ECPGRD(DE,DATOT,EXETYP,SOME)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      LOGICAL SOME
      DIMENSION DE(3,MXATM), DATOT(*)
C
      COMMON /ECPDIM/ NCOEF1,NCOEF2,J1LEN,J2LEN,LLIM,NLIM,NTLIM,J4LEN
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
C
      L2 =(NUM*NUM+NUM)/2
      T0 = 0.0D+00
      CALL TSECND(T0)
C
C  SIMPLE DRIVER TO ALLOCATE MEMORY AND CALL THE ROUTINE TO
C  COMPUTE THE ECP MODIFICATIONS TO THE GRADIENT
C
      CALL VALFM(LOADFM)
      LDCF1 = LOADFM+ 1
      LJLN  = LDCF1 + NCOEF1
      LLB1  = LJLN  +(J1LEN-1)/NWDVAR+1
      LDCF4 = LLB1  +(9*NCOEF1-1)/NWDVAR+1
      LDCF2 = LDCF4 + J4LEN
      LJ2N  = LDCF2 + NCOEF2
      LLB2  = LJ2N  +(J2LEN-1)/NWDVAR+1
      LDEECP= LLB2  +(6*NCOEF2)/NWDVAR
      LFPQR = LDEECP+ 3*NAT
      LFP   = LFPQR + 25*25*25
      LFP2  = LFP   + 2*11*11*11
      LGG   = LFP2  + 2*11*11*11
      LGG2  = LGG   + 21*15
      LXIN  = LGG2  + 21*15
      LYIN  = LXIN  + 15*15
      LZIN  = LYIN  + 15*15
      LZLM  = LZIN  + 15*15
      LLMF  = LZLM  + 581
      LLMX  = LLMF  +(121+1)/NWDVAR
      LLMY  = LLMX  +(581+1)/NWDVAR
      LLMZ  = LLMY  +(581+1)/NWDVAR
      LAST  = LLMZ  +(581+1)/NWDVAR
      NECP  = LAST  - LDCF1
      CALL GETFM(NECP)
      IF(EXETYP.EQ.CHECK) GO TO 110
C
C     -----  READ IN ECP FORMULAS AND DATA  -----
C
      LDAF91=(J1LEN-1)/NWDVAR+1+(9*NCOEF1-1)/NWDVAR+1
      LDAF93=(J2LEN-1)/NWDVAR+1+(6*NCOEF2)/NWDVAR
      CALL DAREAD(IDAF,IODA,X(LDCF1),NCOEF1,90,0)
      CALL DAREAD(IDAF,IODA,X(LJLN) ,LDAF91,91,1)
      CALL DAREAD(IDAF,IODA,X(LDCF2),NCOEF2,92,0)
      CALL DAREAD(IDAF,IODA,X(LJ2N) ,LDAF93,93,1)
C
C THE NEXT CALLS JUST FILL OUT TABLES WHICH HAVE PROBABLY ALREADY
C BEEN DONE IN THE ENERGY STEP, BUT JUST IN CASE THIS IS A RESTART RUN
C
      CALL DAWT
      CALL ERRT
      CALL DAWERT
      CALL ECPINI(X(LLMF),X(LLMX),X(LLMY),X(LLMZ))
      CALL ZTAB(X(LZLM))
      CALL FTAB(X(LFPQR),NLIM-1)
      CALL ECCOD3(X(LFPQR),X(LDCF4),X(LZLM),X(LLMF),X(LLMX),X(LLMY),
     *            X(LLMZ))
C
C NOW COMPUTE THE GRADIENT!
C
      FDDUM = 0.0D+00
      CALL ECP1D(DE,DATOT,L2,X(LDCF1),X(LJLN),X(LLB1),X(LDCF4),
     *     X(LDCF2),X(LJ2N),X(LLB2),X(LDEECP),X(LFPQR),X(LFP),X(LFP2),
     *     X(LGG),X(LGG2),X(LXIN),X(LYIN),X(LZIN),X(LZLM),X(LLMF),
     *     X(LLMX),X(LLMY),X(LLMZ),FDDUM,0)
C
      IF(SOME) THEN
         CALL TSECND(T1)
         TG = T1-T0
         WRITE(IW,9010) TG
      END IF
  110 CONTINUE
      CALL RETFM(NECP)
C
      RETURN
 9010 FORMAT(' TIME TO DO      ECP GRADIENT INTEGRALS=',F10.2)
      END
C*MODULE ECPDER  *DECK ECP1D
      SUBROUTINE ECP1D(DE,DATOT,L2,DCOEF1,JFST1,LBECP1,DCOEF4,
     *                 DCOEF2,JFST2,LBECP2,DELOC,FPQR,FP,FP2,G,G2,
     *                 XIN,YIN,ZIN,ZLM,LMF,LMX,LMY,LMZ,FD,IFDFLAG)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXGSH=30, MXATM=500, MXAO=2047)
C
      DIMENSION DE(3,MXATM),DATOT(L2),DCOEF1(*),JFST1(*),LBECP1(9,*),
     *          DCOEF4(*),DCOEF2(*),JFST2(*),LBECP2(6,*),DELOC(3,*)
C FP IS USED FOR RADIAL INTEGRAL STORAGE AND MUST BE AT LEAST 2*11**3
      DIMENSION FPQR(25,25,25),FP(2*11*11*11),FP2(2*11*11*11)
      DIMENSION G(21*15),G2(21*15),XIN(15*15),YIN(15*15),ZIN(15*15)
      DIMENSION ZLM(*),LMF(*),LMX(*),LMY(*),LMZ(*),FD(*)
C
C ... ECP PARAMETERS ... GENERATED IN ECPPAR
C
      COMMON /ECP1  / X01,CAX,CAY,CAZ,CA,XCA,YCA,ZCA,
     *                X02,BAX,BAY,BAZ,BA,XBA,YBA,ZBA,
     *                PHASE,DAX,DAY,DAZ,DA,XDA,YDA,ZDA,XINT,KCNTR
      COMMON /ECP2  / CLP(400),ZLP(400),NLP(400),KFRST(MXATM,6),
     *                KLAST(MXATM,6),LMAX(MXATM),LPSKIP(MXATM),
     *                IZCORE(MXATM)
      LOGICAL CANDB
      COMMON /ECP4  / P12(3,2),R12,ACO(3),CANDB
C IAMIN/MAX ARE THE ANGULAR   FUNCTION MIN AND MAX FOR I
C IPMIN/MAX ARE THE PRIMITIVE FUNCTION MIN AND MAX FOR I
C KF1/KL1 ARE THE FIRST AND LAST PRIMITIVES FORMING THE LMAX ECP
C     AND THUS THE TYPE 1 INTEGRAL
      LOGICAL IANDJ,NORM,NORMI,NORMJ
      COMMON /ECPIDX/ Q2,IAMIN,IAMAX,JAMIN,JAMAX,IPMIN,IPMAX,JPMIN,
     *                JPMAX,KF1,KL1,LLMX,NPC,NPB,IANDJ,NORM,NORMI,NORMJ
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
C THE FOLLOWING ARE TO MAKE USE OF SYMMETRY
C
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                T(432),INVT(48),NT
C
      LOGICAL CANDA,AANDB,DBUG,NXT
C
C LOCAL STORAGE
      DIMENSION COEFI(MXGSH),COEFJ(MXGSH),COEFIP(MXGSH)
C
C DUMMY ARGUMENTS FOR ECPDRA WHEN IC4C=0
      DIMENSION COEFQ(1)
C
      PARAMETER (ZER=0.0D+00)
C
C IANG GIVES THE MAXIMUM ANGULAR MOMENTUM FOR EACH SHELL
C
      DIMENSION IANG(56),IAMINA(7),LI(3)
      DATA IANG/1,3*2,6*3,10*4,15*5,21*6/
      DATA IAMINA/1,2,5,11,21,36,57/
      DATA LI/ 1,-1, 1/
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DEBUG/8HDEBUG   /,DBUGME/8HECP1D   /,GRD1/8HGRD1    /
#else
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      CHARACTER*8 :: GRD1_STR
      EQUIVALENCE (GRD1, GRD1_STR)
      DATA DEBUG_STR/"DEBUG   "/,DBUGME_STR/"ECP1D   "/,
     * GRD1_STR/"GRD1    "/
#endif
C
C        -----  ROUTINE CALCULATES THE GRADIENT OF THE ECP      -----
C        -----  INTEGRALS AND PRODUCES AN OUTPUT VECTOR.        -----
C        -----  ROUTINE BASED ON NEW VERSION OF ECPINT, BY      -----
C        -----  L.    KAHN, OF BATTELE.                         -----
C        -----  GRADIENT MODIFICATIONS BY IMS GROUP IN JAPAN.   -----
C        -----  GRADSCF VERSION  AUGUST  -1981-                 -----
C        REWORKED FOR GAMESS BY BRETT BODE 1998
C
      COEFQ(1)=ZER
C
C INITIALIZE PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT= ME-1
      NEXT=-1
      LCNT=-1
C
      DBUG= EXETYP.EQ.DBUGME .OR. EXETYP.EQ.DEBUG
      DBUG= MASWRK .AND.(NPRINT.EQ.-3 .OR. EXETYP.EQ.GRD1 .OR. DBUG)
C
C THE SHELL BEING DIFFERENTIATED WILL BE NORMALIZED WHEN THE DERIVATIVE
C IS FORMED IN FORMDR, SO DON'T NORM IN THE RADIAL ROUTINES
C
      IANDJ =.FALSE.
      NORM  = NORMF.NE.1 .OR. NORMP.NE.1
      NORMI =.FALSE.
      NORMJ = NORM
C
C ZERO OUT THE GRADIENT VECTOR
C
      DO 110 J=1,NAT
         DO 110 I=1,3
  110 DELOC(I,J)= ZER
C
C        -----  LOOP OVER  ISHELL.                              -----
C        -----  NOTE LOOPS RUN OVER FULL SQUARE ARRAY           -----
C        -----  OF ECP MATRIX FOR DERIVATIVES.                  -----
C
      DO 240 II=1,NSHELL
C
C        ----- GO PARALLEL! -----
C
         IF(NXT .AND. GOPARR) THEN
            LCNT= LCNT+1
            IF(LCNT.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
            IF(LCNT.NE.NEXT) GO TO 240
         END IF
         I1 = KSTART(II)
         I2 = I1+KNG(II)-1
         IPMIN = I1
         IPMAX = I2
         ICNTR = KATOM(II)
         IMIN = KMIN(II)
         IMAX = KMAX(II)
         LOCI = KLOC(II)-IMIN
         IAMIN = IMIN
         IAMAX = IMAX
         IIMAX = 1
         IF(IMIN.EQ.1 .AND. IMAX.EQ.4) IIMAX = 2
         DO 230 III=1,IIMAX
            IF(IIMAX.EQ.2) THEN
               IF(III.EQ.1) THEN
                  IAMIN= 1
                  IAMAX= 1
               ELSE
                  IAMIN= 2
                  IAMAX= 4
               END IF
            END IF
            NPC0= IANG(IAMAX)
C
C STORE THE COEFS FOR LATER USE. SINCE WE ARE TAKING THE DERIVATIVE
C OF THE ISHELL WE CAN COMBINE THE COEF WITH THE FACTOR OF 2EX
C
            IF(IAMIN.LE.35) ITEMP= 5
            IF(IAMIN.LE.20) ITEMP= 4
            IF(IAMIN.LE.10) ITEMP= 3
            IF(IAMIN.LE. 4) ITEMP= 2
            IF(IAMIN.EQ. 1) ITEMP= 1
            DO 120 IG=IPMIN,IPMAX
               IF(IAMIN.LE.35) T01= CG(IG)
               IF(IAMIN.LE.20) T01= CF(IG)
               IF(IAMIN.LE.10) T01= CD(IG)
               IF(IAMIN.LE. 4) T01= CP(IG)
               IF(IAMIN.EQ. 1) T01= CS(IG)
               COEFI(IG-IPMIN+1) = T01
               COEFIP(IG-IPMIN+1)= T01*(-2.0D+00*EX(IG))
  120       CONTINUE
C
C        -----  JSHELL  -----
C
            DO 200 JJ=1,NSHELL
C CHECK SYMMETRY
               N2=0
               II0= MAX0(II,JJ)
               JJ0= MIN0(II,JJ)
               DO 130 IT=1,NT
                  ID=MAPSHL(II,IT)
                  JD=MAPSHL(JJ,IT)
                  IDD= MAX0(ID,JD)
                  JDD= MIN0(ID,JD)
                  IF(IDD.GT.II0) GO TO 200
                  IF(IDD.LT.II0) GO TO 130
                  IF(JDD.GT.JJ0) GO TO 200
                  IF(JDD.LT.JJ0) GO TO 130
                  N2=N2+1
  130          CONTINUE
               Q2 = NT
               Q2 = Q2/N2
C
C        -----  GO PARALLEL!  -----
C
               IF((.NOT.NXT).AND. GOPARR) THEN
                  IPCOUNT= IPCOUNT+1
                  IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 200
               END IF
C
               J1 = KSTART(JJ)
               J2 = J1+KNG(JJ)-1
               JPMIN = J1
               JPMAX = J2
               JCNTR = KATOM(JJ)
               JMIN = KMIN(JJ)
               JMAX = KMAX(JJ)
               LOCJ = KLOC(JJ)-JMIN
               JAMIN = JMIN
               JAMAX = JMAX
               JJMAX = 1
               IF(JMIN.EQ.1 .AND. JMAX.EQ.4) JJMAX = 2
               DO 190 JJJ=1,JJMAX
                  IF(JJMAX.EQ.2) THEN
                     IF(JJJ.EQ.1) THEN
                        JAMIN = 1
                        JAMAX = 1
                     ELSE
                        IF(IANDJ .AND. IAMIN.EQ.1) GO TO 190
                        JAMIN = 2
                        JAMAX = 4
                     END IF
                  END IF
                  DO 140 JG=JPMIN,JPMAX
                     IF(JAMIN.LE.35) T01= CG(JG)
                     IF(JAMIN.LE.20) T01= CF(JG)
                     IF(JAMIN.LE.10) T01= CD(JG)
                     IF(JAMIN.LE. 4) T01= CP(JG)
                     IF(JAMIN.EQ. 1) T01= CS(JG)
                     COEFJ(JG-JPMIN+1)= T01
  140             CONTINUE
                  NPB0= IANG(JAMAX)
          IJMAX=(IAMINA(ITEMP+2)-IAMINA(ITEMP+1))*(JAMAX-JAMIN+1)
                  CANDB= ICNTR.EQ.JCNTR
                  R12= ZER
                  DO 150 M=1,3
C CENTER C
                     P12(M,1)= C(M,ICNTR)
C CENTER B
                     P12(M,2)= C(M,JCNTR)
  150             R12= R12+(P12(M,2)-P12(M,1))*(P12(M,2)-P12(M,1))
C
C NOW LOOP OVER EACH CENTER WITH AN ECP POTENTIAL
C
                  DO 180 IKCNTR=1,NAT
                     IF(ICNTR.EQ.IKCNTR) GO TO 180
C
C ZERO OUT THE ARRAYS WHICH WILL COLLECT INDIVIDUAL INTEGRALS FOR
C LATER GRADIENT FORMATION
C
                     CALL VCLR(G ,1,IJMAX)
                     CALL VCLR(G2,1,IJMAX)
                     KCNTR= IKCNTR
C CENTER A
                     ACO(1)= C(1,KCNTR)
                     ACO(2)= C(2,KCNTR)
                     ACO(3)= C(3,KCNTR)
                     IF(LPSKIP(KCNTR).EQ.1) GO TO 180
                     LLMX = LMAX(KCNTR)+1
                     KF1 = KFRST(KCNTR,1)
                     KL1 = KLAST(KCNTR,1)
                     CANDA= ICNTR.EQ.KCNTR
                     AANDB= KCNTR.EQ.JCNTR
C
                     CALL ECPCBA(CANDA,AANDB,ICAB,IPOW)
C
C SET UP TABLES OF THE POWERS OF THE CARTESIAN DISTANCES (CAX, CAY ...)
C FOR LATER USE. PASS IN THE MAXIMUM ANGULAR MOMENTUM FOR I AND J
C USE MAX TO INDEX IANG TO MAKE SURE WE GET THE MAX (L SHELLS)
C
                     IF(ICNTR.NE.KCNTR .OR. KCNTR.NE.JCNTR)
     *               CALL ECPPWR(IPOW,NPC0+1,NPB0)
C
                     NPB= NPB0
                     DO 160 IJCASE=1,3
                        NPC= NPC0+LI(IJCASE)
                        IF(NPC.LT.1) GO TO 160
C N+N' IS THE SUM OF THE ANGULAR MOMENTUM PLUS 1 TO INDEX ARRAYS
                        NPNP= NPC+NPB-1
                        IAMIN= IAMINA(ITEMP+LI(IJCASE))
                        IAMAX= IAMINA(ITEMP+LI(IJCASE)+1)-1
                        IJMAX=(IAMAX-IAMIN+1)*(JAMAX-JAMIN+1)
                        IF(IJCASE.EQ.1) THEN
C CALCULATE THE INTEGRALS FOR THE PORTION SHIFTED UP
                           IF(ICAB.EQ.1) THEN
                  CALL ECPAA1(NPNP,FPQR,COEFIP,COEFJ,DCOEF4,G2)
                           ELSE IF(ICAB.EQ.2 .OR. ICAB.EQ.3) THEN
                  CALL ECPRA2(ICAB,NPNP,FP,COEFIP,COEFJ,
     *                        DCOEF1,JFST1,LBECP1,DCOEF4,
     *                        DCOEF2,JFST2,LBECP2,G2,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           ELSE IF(ICAB.EQ.4) THEN
                              IC4C=-1
                  CALL ECPDRA(IC4C,NPNP,FP,FP2,COEFIP,COEFI,COEFJ,
     *                        DCOEF1,JFST1,LBECP1,
     *                        DCOEF2,JFST2,LBECP2,G2,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           END IF
                        ELSEIF(IJCASE.EQ.2) THEN
C AND NOW THE PART SHIFTED DOWN
                           IF(ICAB.EQ.1) THEN
                  CALL ECPAA1(NPNP,FPQR,COEFI,COEFJ,DCOEF4,G)
                           ELSE IF(ICAB.EQ.2 .OR. ICAB.EQ.3) THEN
                  CALL ECPRA2(ICAB,NPNP,FP,COEFI,COEFJ,
     *                        DCOEF1,JFST1,LBECP1,DCOEF4,
     *                        DCOEF2,JFST2,LBECP2,G,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           ELSE IF(ICAB.EQ.4) THEN
                              IC4C= 1
                  CALL ECPDRA(IC4C,NPNP,FP,FP2,COEFI,COEFQ,COEFJ,
     *                        DCOEF1,JFST1,LBECP1,
     *                        DCOEF2,JFST2,LBECP2,G,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           END IF
                        END IF
  160                CONTINUE
                     NPC = NPC0
                     NPB = NPB0
                     NPNP= NPC+NPB-1
                     IAMIN = IAMINA(ITEMP)
                     IAMAX = IAMINA(ITEMP+1)-1
C DO NOT RESET IJMAX FOR USE BELOW
C
C FORM THE DERIVITIVE INTEGRALS FROM THE COMPUTED REGULAR INTEGRALS
C
           CALL FORMDR(G,G2,XIN,YIN,ZIN,IAMIN,IAMAX,JAMIN,JAMAX,NORM)
C
C COMPUTE THE GRADIENT BY MULTIPLYING THE DERIVITIVE TERMS BY THE
C CORRESPONDING DENSITY MATRIX ELEMENTS
C
                     N= 1
                     DO 170 J=JAMIN,JAMAX
                        JN = LOCJ+J
                        DO 170 I=IAMIN,IAMAX
                           IN = LOCI+I
                           NN = IA(MAX0(IN,JN))+MIN0(IN,JN)
                           DUM=-DATOT(NN)*2.0D+00
C
C NEXT THREE LINES PRODUCE THE DERIVATIVE ON THE BASIS FUNCTION CENTER
C
                           DELOC(1,ICNTR)= DELOC(1,ICNTR)+DUM*XIN(N)
                           DELOC(2,ICNTR)= DELOC(2,ICNTR)+DUM*YIN(N)
                           DELOC(3,ICNTR)= DELOC(3,ICNTR)+DUM*ZIN(N)
C
C NEXT THREE LINES PRODUCE THE GRADIENT DUE TO THE DERIVATIVE
C ON THE ECP CENTER
C
                           DELOC(1,KCNTR)= DELOC(1,KCNTR)-DUM*XIN(N)
                           DELOC(2,KCNTR)= DELOC(2,KCNTR)-DUM*YIN(N)
                           DELOC(3,KCNTR)= DELOC(3,KCNTR)-DUM*ZIN(N)
C
C THE NEXT BLOCK STORES THE 1ST DERIVATIVE ELEMENTS OF THE SECOND
C DERIVATIVE. (IFDFLAG SHOULD ONLY BE 1 FOR ANALYTIC HESSIANS)
C
                           IF(IFDFLAG.NE.1) GO TO 170
                           DUM= 1.0D+00
                           IF(IN.EQ.JN) DUM= 2.0D+00
                           NNFD = NN+(3*(ICNTR-1)*L2)
                           FD(NNFD)     = FD(NNFD)     -DUM*XIN(N)
                           FD(NNFD+L2)  = FD(NNFD+L2)  -DUM*YIN(N)
                           FD(NNFD+2*L2)= FD(NNFD+2*L2)-DUM*ZIN(N)
                           NNFD = NN+(3*(KCNTR-1)*L2)
                           FD(NNFD)     = FD(NNFD)     +DUM*XIN(N)
                           FD(NNFD+L2)  = FD(NNFD+L2)  +DUM*YIN(N)
                           FD(NNFD+2*L2)= FD(NNFD+2*L2)+DUM*ZIN(N)
  170                N= N+1
C
C END OF KCNTR ECP POTENTIAL CENTER LOOP
C
  180             CONTINUE
  190          CONTINUE
  200       CONTINUE
C
  230    CONTINUE
  240 CONTINUE
C
C SYMMETRIZE THE ECP CONTRIBUTION TO THE GRADIENT
C
      CALL SYMEG(DELOC)
C
C NOW ADD TO DE
C
      DO 250 J=1,NAT
         DO 250 I=1,3
  250 DE(I,J)= DE(I,J)+DELOC(I,J)
C
      IF(NXT .AND. GOPARR) CALL DDI_DLBRESET()
C
      IF(DBUG) THEN
         WRITE(IW,9010)
         CALL EGOUT(DELOC,NAT)
         WRITE(IW,9020)
      END IF
      RETURN
 9010 FORMAT(/10X,30("-")
     2       /10X,'-ECP- CONTRIBUTION TO GRADIENT'
     3       /10X,30(1H-))
 9020 FORMAT(/' ...... END OF -ECP- GRADIENT ..... ')
      END
C*MODULE ECPDER  *DECK ECP2D
      SUBROUTINE ECP2D(EG,EH,DATOT,L2,DCOEF1,JFST1,LBECP1,DCOEF4,
     *                 DCOEF2,JFST2,LBECP2,FPQR,GL1L1,GL2L,FP,GLM2L,GLL,
     *                 GLP2L,GLM1LM1,GLM1LP1,GLP1LM1,GLP1LP1,
     *                 ZLM,LMF,LMX,LMY,LMZ)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXGSH=30, MXATM=500, MXAO=2047)
C
C DIMENSION PASSED IN ARRAYS
C 15*15 HOLDS FORMED INTEGRALS GXG
C 21*21 HOLDS INTERMEDIATE INTEGRALS HXH (BIGGER THAN GXI)
C NOTE: SOME ARRAYS COULD BE SMALLER, BUT THE CODE IS SIMPLER THIS WAY
C
      DIMENSION EG(3,*),EH(9,*),DATOT(L2),DCOEF1(*),JFST1(*),
     *          LBECP1(9,*),DCOEF4(*),DCOEF2(*),JFST2(*),LBECP2(6,*)
      DIMENSION FPQR(25,25,25),GL1L1(9,15*15),GL2L(9,15*15)
C FP IS USED FOR RADIAL INTEGRAL STORAGE AND MUST BE AT LEAST 2*11**3
      DIMENSION FP(2*11*11*11),GLM2L(21*21),GLL(21*21),GLP2L(21*21),
     *          GLM1LM1(21*21),GLM1LP1(21*21),GLP1LM1(21*21),
     *          GLP1LP1(21*21)
      DIMENSION ZLM(*),LMF(*),LMX(*),LMY(*),LMZ(*)
C
C ... ECP PARAMETERS ... GENERATED IN ECPPAR
C
      COMMON /ECP1  / X01,CAX,CAY,CAZ,CA,XCA,YCA,ZCA,
     *                X02,BAX,BAY,BAZ,BA,XBA,YBA,ZBA,
     *                PHASE,DAX,DAY,DAZ,DA,XDA,YDA,ZDA,XINT,KCNTR
      COMMON /ECP2  / CLP(400),ZLP(400),NLP(400),KFRST(MXATM,6),
     *                KLAST(MXATM,6),LMAX(MXATM),LPSKIP(MXATM),
     *                IZCORE(MXATM)
      LOGICAL CANDB
      COMMON /ECP4  / P12(3,2),R12,ACO(3),CANDB
C IAMIN/MAX ARE THE ANGULAR   FUNCTION MIN AND MAX FOR I
C IPMIN/MAX ARE THE PRIMITIVE FUNCTION MIN AND MAX FOR I
C KF1/KL1 ARE THE FIRST AND LAST PRIMITIVES FORMING THE LMAX ECP
C     AND THUS THE TYPE 1 INTEGRAL
      LOGICAL IANDJ,NORM,NORMI,NORMJ
      COMMON /ECPIDX/ Q2,IAMIN,IAMAX,JAMIN,JAMAX,IPMIN,IPMAX,JPMIN,
     *                JPMAX,KF1,KL1,LLMX,NPC,NPB,IANDJ,NORM,NORMI,NORMJ
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
C THE FOLLOWING ARE TO MAKE USE OF SYMMETRY
C
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                T(432),INVT(48),NT
C
      LOGICAL CANDA,AANDB,DBUG,NXT
C
C LOCAL STORAGE
      DIMENSION CI(MXGSH),CJ(MXGSH),CIP1(MXGSH),CIP2(MXGSH),CJP1(MXGSH)
C
C DUMMY ARGUMENTS FOR ECPDRA WHEN IC4C=0
      DIMENSION FQ(1),COEFQ(1)
C
      PARAMETER (ZER=0.0D+00)
C
C IANG GIVES THE MAXIMUM ANGULAR MOMENTUM FOR EACH SHELL
C
      DIMENSION IANG(84),IAMINA(8),LI(8),LJ(8)
      DATA IANG/1,3*2,6*3,10*4,15*5,21*6,28*7/
      DATA IAMINA/1,2,5,11,21,36,57,85/
      DATA LI/ 0, 2,-2, 1, 1,-1,-1, 0/
      DATA LJ/ 0, 0, 0, 1,-1, 1,-1, 0/
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DEBUG/8HDEBUG   /,DBUGME/8HECPHES  /,HSS1/8HHSS1    /
#else
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      CHARACTER*8 :: HSS1_STR
      EQUIVALENCE (HSS1, HSS1_STR)
      DATA DEBUG_STR/"DEBUG   "/,DBUGME_STR/"ECPHES  "/,
     * HSS1_STR/"HSS1    "/
#endif
C
C        -----  ROUTINE CALCULATES THE HESSIAN OF THE ECP       -----
C        -----  INTEGRALS AND PRODUCES AN OUTPUT VECTOR.        -----
C        -----  ROUTINE BASED ON NEW VERSION OF ECP1D, BY       -----
C        -----  BRETT BODE                                      -----
C
      COEFQ(1)=ZER
C
C INITIALIZE PARALLEL
C
      NXT = IBTYP.EQ.1
      IPCOUNT= ME-1
      NEXT=-1
      LCNT=-1
C
      DBUG= EXETYP.EQ.DBUGME .OR. EXETYP.EQ.DEBUG
      DBUG= MASWRK .AND.(NPRINT.EQ.-3 .OR. EXETYP.EQ.HSS1 .OR. DBUG)
C
C THE SHELL BEING DIFFERENTIATED WILL BE NORMALIZED WHEN THE DERIVATIVE
C IS FORMED IN FORMXX, SO DON'T NORM IN THE RADIAL ROUTINES
C
      IANDJ=.FALSE.
      NORM = NORMF.NE.1 .OR. NORMP.NE.1
      NORMI=.FALSE.
      NORMJ=.FALSE.
C
C        -----  LOOP OVER  ISHELL.                              -----
C        -----  NOTE LOOPS RUN OVER FULL SQUARE ARRAY           -----
C        -----  OF ECP MATRIX FOR DERIVATIVES.                  -----
C
      DO 240 II=1,NSHELL
C
C        ----- GO PARALLEL! -----
C
         IF(NXT .AND. GOPARR) THEN
            LCNT= LCNT+1
            IF(LCNT.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
            IF(LCNT.NE.NEXT) GO TO 240
         END IF
         I1 = KSTART(II)
         I2 = I1+KNG(II)-1
         IPMIN = I1
         IPMAX = I2
         ICNTR = KATOM(II)
         IMIN = KMIN(II)
         IMAX = KMAX(II)
         LOCI = KLOC(II)-IMIN
         IAMIN = IMIN
         IAMAX = IMAX
         IIMAX = 1
         IF(IMIN.EQ.1 .AND. IMAX.EQ.4) IIMAX = 2
         DO 230 III=1,IIMAX
            IF(IIMAX.EQ.2) THEN
               IF(III.EQ.1) THEN
                  IAMIN = 1
                  IAMAX = 1
               ELSE
                  IAMIN = 2
                  IAMAX = 4
               END IF
            END IF
            NPC0= IANG(IAMAX)
C
C STORE THE COEFS FOR LATER USE. SINCE WE ARE TAKING THE DERIVATIVE
C OF THE ISHELL WE CAN COMBINE THE COEF WITH THE FACTOR OF 2EX
C
            IF(IAMIN.LE.35) ITYPE= 5
            IF(IAMIN.LE.20) ITYPE= 4
            IF(IAMIN.LE.10) ITYPE= 3
            IF(IAMIN.LE. 4) ITYPE= 2
            IF(IAMIN.EQ. 1) ITYPE= 1
            DO 120 IG=IPMIN,IPMAX
               IF(IAMIN.LE.35) T01= CG(IG)
               IF(IAMIN.LE.20) T01= CF(IG)
               IF(IAMIN.LE.10) T01= CD(IG)
               IF(IAMIN.LE. 4) T01= CP(IG)
               IF(IAMIN.EQ. 1) T01= CS(IG)
               CI(IG-IPMIN+1)= T01
               T02= T01*(-2.0D+00*EX(IG))
               CIP1(IG-IPMIN+1)= T02
               CIP2(IG-IPMIN+1)= T02*(-2.0D+00*EX(IG))
  120       CONTINUE
C
C        -----  JSHELL  -----
C
            DO 200 JJ=1,NSHELL
C CHECK SYMMETRY
               N2=0
               II0= MAX0(II,JJ)
               JJ0= MIN0(II,JJ)
               DO 130 IT=1,NT
                  ID=MAPSHL(II,IT)
                  JD=MAPSHL(JJ,IT)
                  IDD= MAX0(ID,JD)
                  JDD= MIN0(ID,JD)
                  IF(IDD.GT.II0) GO TO 200
                  IF(IDD.LT.II0) GO TO 130
                  IF(JDD.GT.JJ0) GO TO 200
                  IF(JDD.LT.JJ0) GO TO 130
                  N2=N2+1
  130          CONTINUE
               Q2 = NT
               Q2 = Q2/N2
C
C        -----  GO PARALLEL!  -----
C
               IF((.NOT.NXT).AND. GOPARR) THEN
                  IPCOUNT= IPCOUNT+1
                  IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 200
               END IF
C
               J1 = KSTART(JJ)
               J2 = J1+KNG(JJ)-1
               JPMIN = J1
               JPMAX = J2
               JCNTR = KATOM(JJ)
               JMIN = KMIN(JJ)
               JMAX = KMAX(JJ)
               LOCJ = KLOC(JJ)-JMIN
               JAMIN = JMIN
               JAMAX = JMAX
               JJMAX = 1
               IF(JMIN.EQ.1 .AND. JMAX.EQ.4) JJMAX = 2
               DO 190 JJJ=1,JJMAX
                  IF(JJMAX.EQ.2) THEN
                     IF(JJJ.EQ.1) THEN
                        JAMIN = 1
                        JAMAX = 1
                     ELSE
                        IF(IANDJ .AND. IAMIN.EQ.1) GO TO 190
                        JAMIN = 2
                        JAMAX = 4
                     END IF
                  END IF
                  IF(JAMIN.LE.35) JTYPE= 5
                  IF(JAMIN.LE.20) JTYPE= 4
                  IF(JAMIN.LE.10) JTYPE= 3
                  IF(JAMIN.LE. 4) JTYPE= 2
                  IF(JAMIN.EQ. 1) JTYPE= 1
                  DO 140 JG=JPMIN,JPMAX
                     IF(JAMIN.LE.35) T01= CG(JG)
                     IF(JAMIN.LE.20) T01= CF(JG)
                     IF(JAMIN.LE.10) T01= CD(JG)
                     IF(JAMIN.LE. 4) T01= CP(JG)
                     IF(JAMIN.EQ. 1) T01= CS(JG)
                     CJ(JG-JPMIN+1)= T01
                     CJP1(JG-JPMIN+1)= T01*(-2.0D+00*EX(JG))
  140             CONTINUE
                  NPB0= IANG(JAMAX)
                  CANDB= ICNTR.EQ.JCNTR
                  R12= ZER
                  DO 150 M=1,3
C CENTER C
                     P12(M,1)= C(M,ICNTR)
C CENTER B
                     P12(M,2)= C(M,JCNTR)
  150             R12= R12+(P12(M,2)-P12(M,1))*(P12(M,2)-P12(M,1))
C
C NOW LOOP OVER EACH CENTER WITH AN ECP POTENTIAL
C
                  DO 180 IKCNTR=1,NAT
                     KCNTR= IKCNTR
                     IF(ICNTR.EQ.KCNTR .AND. JCNTR.EQ.KCNTR) GO TO 180
                     IJMAX= MAX0((IAMINA(ITYPE+2)-IAMINA(ITYPE+1))*
     *                           (IAMINA(JTYPE+2)-IAMINA(JTYPE+1)),
     *                           (IAMINA(ITYPE+3)-IAMINA(ITYPE+2))*
     *                           (IAMINA(JTYPE+1)-IAMINA(JTYPE  )))
C
C ZERO OUT THE ARRAYS WHICH WILL COLLECT INDIVIDUAL INTEGRALS FOR
C LATER HESSIAN FORMATION
C
                     CALL VCLR(GLM2L  ,1,IJMAX)
                     CALL VCLR(GLL    ,1,IJMAX)
                     CALL VCLR(GLP2L  ,1,IJMAX)
                     CALL VCLR(GLM1LM1,1,IJMAX)
                     CALL VCLR(GLM1LP1,1,IJMAX)
                     CALL VCLR(GLP1LM1,1,IJMAX)
                     CALL VCLR(GLP1LP1,1,IJMAX)
C CENTER A
                     ACO(1)= C(1,KCNTR)
                     ACO(2)= C(2,KCNTR)
                     ACO(3)= C(3,KCNTR)
                     IF(LPSKIP(KCNTR).EQ.1) GO TO 180
                     LLMX = LMAX(KCNTR)+1
                     KF1 = KFRST(KCNTR,1)
                     KL1 = KLAST(KCNTR,1)
                     CANDA= ICNTR.EQ.KCNTR
                     AANDB= KCNTR.EQ.JCNTR
C
                     CALL ECPCBA(CANDA,AANDB,ICAB,IPOW)
C
C SET UP TABLES OF THE POWERS OF THE CARTESIAN DISTANCES (CAX, CAY ...)
C FOR LATER USE. PASS IN THE MAXIMUM ANGULAR MOMENTUM FOR I AND J
C USE MAX TO INDEX IANG TO MAKE SURE WE GET THE MAX (L SHELLS)
C
                     IF(ICNTR.NE.KCNTR .OR. KCNTR.NE.JCNTR)
     *               CALL ECPPWR(IPOW,NPC0+2,NPB0+1)
C
                     IF(ICNTR.NE.KCNTR) THEN
                        I1= 1
                        NORMJ= NORM
                     ELSE
                        I1= 4
                     ENDIF
                     IC4C= 0
                     DO 160 IJCASE=I1, 8
                        NPC= NPC0+LI(IJCASE)
                        NPB= NPB0+LJ(IJCASE)
                        IF(NPC.LT.1 .OR. NPB.LT.1) GO TO 160
C N+N' IS THE SUM OF THE ANGULAR MOMENTUM PLUS 1 TO INDEX ARRAYS
                        NPNP=NPC+NPB-1
                        IAMIN= IAMINA(ITYPE+LI(IJCASE))
                        IAMAX= IAMINA(ITYPE+LI(IJCASE)+1)-1
                        JAMIN= IAMINA(JTYPE+LJ(IJCASE))
                        JAMAX= IAMINA(JTYPE+LJ(IJCASE)+1)-1
                        IJMAX=(IAMAX-IAMIN+1)*(JAMAX-JAMIN+1)
                        IF(IJCASE.EQ.1) THEN
C CALCULATE THE <L|U|L> TERM
                           IF(ICAB.EQ.1) THEN
                  CALL ECPAA1(NPNP,FPQR,CIP1,CJ,DCOEF4,GLL)
                           ELSE IF(ICAB.EQ.2 .OR. ICAB.EQ.3) THEN
                  CALL ECPRA2(ICAB,NPNP,FP,CIP1,CJ,
     *                        DCOEF1,JFST1,LBECP1,DCOEF4,
     *                        DCOEF2,JFST2,LBECP2,GLL,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           ELSE IF(ICAB.EQ.4) THEN
                  CALL ECPDRA(IC4C,NPNP,FP,FQ,CIP1,COEFQ,CJ,
     *                        DCOEF1,JFST1,LBECP1,
     *                        DCOEF2,JFST2,LBECP2,GLL,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           END IF
                        ELSEIF(IJCASE.EQ.2) THEN
C CALCULATE THE <L+2|U|L> TERM
                           IF(ICAB.EQ.1) THEN
                  CALL ECPAA1(NPNP,FPQR,CIP2,CJ,DCOEF4,GLP2L)
                           ELSE IF(ICAB.EQ.2 .OR. ICAB.EQ.3) THEN
                  CALL ECPRA2(ICAB,NPNP,FP,CIP2,CJ,
     *                        DCOEF1,JFST1,LBECP1,DCOEF4,
     *                        DCOEF2,JFST2,LBECP2,GLP2L,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           ELSE IF(ICAB.EQ.4) THEN
                  CALL ECPDRA(IC4C,NPNP,FP,FQ,CIP2,COEFQ,CJ,
     *                        DCOEF1,JFST1,LBECP1,
     *                        DCOEF2,JFST2,LBECP2,GLP2L,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           END IF
                        ELSEIF(IJCASE.EQ.3) THEN
C CALCULATE THE <L-2|U|L> TERM
                           IF(ICAB.EQ.1) THEN
                  CALL ECPAA1(NPNP,FPQR,CI,CJ,DCOEF4,GLM2L)
                           ELSE IF(ICAB.EQ.2 .OR. ICAB.EQ.3) THEN
                  CALL ECPRA2(ICAB,NPNP,FP,CI,CJ,
     *                        DCOEF1,JFST1,LBECP1,DCOEF4,
     *                        DCOEF2,JFST2,LBECP2,GLM2L,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           ELSE IF(ICAB.EQ.4) THEN
                  CALL ECPDRA(IC4C,NPNP,FP,FQ,CI,COEFQ,CJ,
     *                        DCOEF1,JFST1,LBECP1,
     *                        DCOEF2,JFST2,LBECP2,GLM2L,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           END IF
                        ELSEIF(IJCASE.EQ.4) THEN
C CALCULATE THE <L+1|U|L+1> TERM
C TURN OFF J NORMALIZATION SINCE IT IS NOW ALSO DERIVATIZED
                           NORMJ=.FALSE.
                           IF(ICAB.EQ.1) THEN
                  CALL ECPAA1(NPNP,FPQR,CIP1,CJP1,DCOEF4,GLP1LP1)
                           ELSE IF(ICAB.EQ.2 .OR. ICAB.EQ.3) THEN
                  CALL ECPRA2(ICAB,NPNP,FP,CIP1,CJP1,
     *                        DCOEF1,JFST1,LBECP1,DCOEF4,
     *                        DCOEF2,JFST2,LBECP2,GLP1LP1,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           ELSE IF(ICAB.EQ.4) THEN
                  CALL ECPDRA(IC4C,NPNP,FP,FQ,CIP1,COEFQ,CJP1,
     *                        DCOEF1,JFST1,LBECP1,
     *                        DCOEF2,JFST2,LBECP2,GLP1LP1,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           END IF
                        ELSEIF(IJCASE.EQ.5) THEN
C CALCULATE THE <L+1|U|L-1> TERM
                           IF(ICAB.EQ.1) THEN
                  CALL ECPAA1(NPNP,FPQR,CIP1,CJ,DCOEF4,GLP1LM1)
                           ELSE IF(ICAB.EQ.2 .OR. ICAB.EQ.3) THEN
                  CALL ECPRA2(ICAB,NPNP,FP,CIP1,CJ,
     *                        DCOEF1,JFST1,LBECP1,DCOEF4,
     *                        DCOEF2,JFST2,LBECP2,GLP1LM1,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           ELSE IF(ICAB.EQ.4) THEN
                  CALL ECPDRA(IC4C,NPNP,FP,FQ,CIP1,COEFQ,CJ,
     *                        DCOEF1,JFST1,LBECP1,
     *                        DCOEF2,JFST2,LBECP2,GLP1LM1,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           END IF
                        ELSEIF(IJCASE.EQ.6) THEN
C CALCULATE THE <L-1|U|L+1> TERM
                           IF(ICAB.EQ.1) THEN
                  CALL ECPAA1(NPNP,FPQR,CI,CJP1,DCOEF4,GLM1LP1)
                           ELSE IF(ICAB.EQ.2 .OR. ICAB.EQ.3) THEN
                  CALL ECPRA2(ICAB,NPNP,FP,CI,CJP1,
     *                        DCOEF1,JFST1,LBECP1,DCOEF4,
     *                        DCOEF2,JFST2,LBECP2,GLM1LP1,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           ELSE IF(ICAB.EQ.4) THEN
                  CALL ECPDRA(IC4C,NPNP,FP,FQ,CI,COEFQ,CJP1,
     *                        DCOEF1,JFST1,LBECP1,
     *                        DCOEF2,JFST2,LBECP2,GLM1LP1,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           END IF
                        ELSEIF(IJCASE.EQ.7) THEN
C CALCULATE THE <L-1|U|L-1> TERM
                           IF(ICAB.EQ.1) THEN
                  CALL ECPAA1(NPNP,FPQR,CI,CJ,DCOEF4,GLM1LM1)
                           ELSE IF(ICAB.EQ.2 .OR. ICAB.EQ.3) THEN
                  CALL ECPRA2(ICAB,NPNP,FP,CI,CJ,
     *                        DCOEF1,JFST1,LBECP1,DCOEF4,
     *                        DCOEF2,JFST2,LBECP2,GLM1LM1,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           ELSE IF(ICAB.EQ.4) THEN
                  CALL ECPDRA(IC4C,NPNP,FP,FQ,CI,COEFQ,CJ,
     *                        DCOEF1,JFST1,LBECP1,
     *                        DCOEF2,JFST2,LBECP2,GLM1LM1,
     *                        ZLM,LMF,LMX,LMY,LMZ)
                           END IF
                        ENDIF
  160                CONTINUE
C
C FORM THE DERIVITIVE INTEGRALS FROM THE COMPUTED REGULAR INTEGRALS
C
                     CALL FORMII(GLP1LP1,GLP1LM1,GLM1LP1,GLM1LM1,GL1L1,
     *                           IAMIN,IAMAX,JAMIN,JAMAX,NORM)
                     CALL FORMIJ(GLP2L,GLL,GLM2L,GL2L,
     *                           IAMIN,IAMAX,JAMIN,JAMAX,NORM)
C
C COMPUTE THE GRADIENT BY MULTIPLYING THE DERIVITIVE TERMS BY THE
C CORRESPONDING DENSITY MATRIX ELEMENTS
C
                     N= 1
                     DO 170 J=JAMIN,JAMAX
                        JN = LOCJ+J
                        DO 170 I=IAMIN,IAMAX
                           IN = LOCI+I
                           NN = IA(MAX0(IN,JN))+MIN0(IN,JN)
                           DUM= DATOT(NN)*2.0D+00
                           IC = ICNTR*(ICNTR-1)/2
                           KC = KCNTR*(KCNTR-1)/2
                           DO 165 LL=1,9
                              IF(ICNTR.NE.KCNTR) THEN
C
C 2ND DERIVATIVE OF THE BASIS FUNCTION
C
                                 NC = IC+ICNTR
                                 EH(LL,NC)= EH(LL,NC)+DUM*GL2L(LL,N)
C
C 2ND DERIVATIVE OF THE ECP CENTER
C
                                 NC = KC+KCNTR
                                 EH(LL,NC)= EH(LL,NC)+DUM*GL2L(LL,N)
C
C ONE DERIVATIVE ON THE ECP CENTER
C THERE ARE TWO SUCH TERMS WHICH ARE EQUAL WHEN I=K,
C AND RELATED BY EXCHANGE OF THE ORDER OF DIFFERENTIATION OTHERWISE.
C THUS ONLY ONE OFF DIAGONAL TERM IS STORED
C
                                 IF(ICNTR.GE.KCNTR) THEN
                                    NC = IC+KCNTR
                                    EH(LL,NC)= EH(LL,NC)-DUM*GL2L(LL,N)
                                 ELSE
                                    NC = KC+ICNTR
                                    EH(LL,NC)= EH(LL,NC)-DUM*GL2L(LL,N)
                                 END IF
                              END IF
C
C TERM FROM THE TWO FIRST DERIVATIVES ON ECP CENTER
C
                              NC = KC+KCNTR
                              EH(LL,NC)= EH(LL,NC)+DUM*GL1L1(LL,N)
C
C DERIVATIVES ON EACH BASIS FUNCTION CENTER
C
                              IF(ICNTR.GE.JCNTR) THEN
                                 NC = IC+JCNTR
                                 EH(LL,NC)= EH(LL,NC)+DUM*GL1L1(LL,N)
                              END IF
C
C FIRST DERIVATIVE TERMS WITH ONE DERIVATIVE ON THE ECP CENTER
C
                              IF(ICNTR.GE.KCNTR) THEN
                                 NC = IC+KCNTR
                                 EH(LL,NC)= EH(LL,NC)-DUM*GL1L1(LL,N)
                              END IF
                              IF(JCNTR.LE.KCNTR) THEN
                                 NC = KC+JCNTR
                                 EH(LL,NC)= EH(LL,NC)-DUM*GL1L1(LL,N)
                              END IF
  165                      CONTINUE
  170                N= N+1
C
C END OF KCNTR ECP POTENTIAL CENTER LOOP
C
  180             CONTINUE
  190          CONTINUE
  200       CONTINUE
C
  230    CONTINUE
  240 CONTINUE
C
C SYMMETRIZE THE ECP CONTRIBUTION TO THE HESSIAN
C
      CALL SYMEH(EH)
C
      IF(NXT .AND. GOPARR) CALL DDI_DLBRESET()
C
      IF(DBUG) THEN
         WRITE(IW,9010)
         CALL HSSPRT(NAT,EG,EH)
         WRITE(IW,9020)
      END IF
      RETURN
 9010 FORMAT(/10X,30("-")
     2       /10X,'-ECP- CONTRIBUTION TO HESSIAN'
     3       /10X,30(1H-))
 9020 FORMAT(/' ...... END OF -ECP- HESSIAN ..... ')
      END
C*MODULE ECPDER  *DECK ECPCBA
      SUBROUTINE ECPCBA(CANDA,AANDB,ICAB,IPOW)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL CANDA,AANDB
C
C ... ECP PARAMETERS ... GENERATED IN ECPPAR
C
      COMMON /ECP1  / X01,CAX,CAY,CAZ,CA,XCA,YCA,ZCA,
     2                X02,BAX,BAY,BAZ,BA,XBA,YBA,ZBA,
     3                PHASE,DAX,DAY,DAZ,DA,XDA,YDA,ZDA,XINT,KCNTR
      LOGICAL CANDB
      COMMON /ECP4  / P12(3,2),R12,ACO(3),CANDB
C ECP COMMON STUFF
      COMMON /ZFNCM / X,Y,Z
C
      PARAMETER (ZER=0.0D+00)
C
      IF(CANDA) THEN
         IF(AANDB) THEN
C SPECIAL CASE <A|A|A> ONLY ONE CENTER
            ICAB = 1
         ELSE
C CASE <A|A|B>
            CAX= ZER
            CAY= ZER
            CAZ= ZER
            CA = ZER
            BAX= P12(1,2)-ACO(1)
            BAY= P12(2,2)-ACO(2)
            BAZ= P12(3,2)-ACO(3)
            BA = SQRT(BAX*BAX+BAY*BAY+BAZ*BAZ)
            X  = BAX/BA
            Y  = BAY/BA
            Z  = BAZ/BA
            ICAB= 2
            IPOW= 1
         END IF
      ELSE
         IF(AANDB) THEN
C CASE <C|A|A>
            CAX= P12(1,1)-ACO(1)
            CAY= P12(2,1)-ACO(2)
            CAZ= P12(3,1)-ACO(3)
            CA = SQRT(CAX*CAX+CAY*CAY+CAZ*CAZ)
            X  = CAX/CA
            Y  = CAY/CA
            Z  = CAZ/CA
            BAX= ZER
            BAY= ZER
            BAZ= ZER
            BA = ZER
            ICAB= 3
            IPOW=-1
         ELSE
C GENERAL CASE <C|A|B> THREE-CENTER INTEGRAL
C ACTUALLY C AND B MAY STILL BE EQUAL
            CAX= P12(1,1)-ACO(1)
            CAY= P12(2,1)-ACO(2)
            CAZ= P12(3,1)-ACO(3)
            CA = SQRT(CAX*CAX+CAY*CAY+CAZ*CAZ)
            XCA= CAX/CA
            YCA= CAY/CA
            ZCA= CAZ/CA
            BAX= P12(1,2)-ACO(1)
            BAY= P12(2,2)-ACO(2)
            BAZ= P12(3,2)-ACO(3)
            BA = SQRT(BAX*BAX+BAY*BAY+BAZ*BAZ)
            XBA= BAX/BA
            YBA= BAY/BA
            ZBA= BAZ/BA
            ICAB= 4
            IPOW= 0
         END IF
      END IF
      RETURN
      END
C*MODULE ECPDER  *DECK FORMDR
      SUBROUTINE FORMDR(G,G2,XIN,YIN,ZIN,MINI,MAXI,MINJ,MAXJ,NORM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION G(*),G2(*),XIN(*),YIN(*),ZIN(*)
      LOGICAL NORM
C
      PARAMETER (TWO=2.0D+00, THR=3.0D+00, FOR=4.0D+00)
      PARAMETER (SR3=1.73205080756888D+00, SR5=2.23606797749979D+00)
      PARAMETER (SR7=2.64575131106459D+00)
      PARAMETER (S35=SR3*SR5, S57=SR5*SR7, S53=S57/SR3)
C
C FORM THE FIRST DERIVATIVE INTEGRALS
C THIS ROUTINE IS COMPLETLY GENERAL AND GOOD THROUGH G'
C
      NN= 0
      NI= MAXI-MINI+1
      NJ= MAXJ-MINJ+1
C S'
      IF(MINI.EQ.1) THEN
         DO 110 J=1,NJ
            NN= NN+1
            XIN(NN)= G2(J)
            YIN(NN)= G2(J+NJ)
            ZIN(NN)= G2(J+NJ* 2)
  110    CONTINUE
         GO TO 999
      END IF
C P'
      IF(MINI.LE.2) THEN
         DO 120 J=1,NJ
            NN= NN+1
            XIN(NN)= G2(J)      +G(J)
            YIN(NN)= G2(J+NJ* 3)
            ZIN(NN)= G2(J+NJ* 4)
            NN= NN+1
            XIN(NN)= G2(J+NJ* 3)
            YIN(NN)= G2(J+NJ)   +G(J)
            ZIN(NN)= G2(J+NJ* 5)
            NN= NN+1
            XIN(NN)= G2(J+NJ* 4)
            YIN(NN)= G2(J+NJ* 5)
            ZIN(NN)= G2(J+NJ* 2)+G(J)
  120    CONTINUE
         GO TO 999
      END IF
C D'
      IF(MINI.LE.5) THEN
         DO 130 J=1,NJ
            NN= NN+1
            XIN(NN)= G2(J)      +G(J)      *TWO
            YIN(NN)= G2(J+NJ* 3)
            ZIN(NN)= G2(J+NJ* 4)
            NN= NN+1
            XIN(NN)= G2(J+NJ* 5)
            YIN(NN)= G2(J+NJ)   +G(J+NJ)   *TWO
            ZIN(NN)= G2(J+NJ* 6)
            NN= NN+1
            XIN(NN)= G2(J+NJ* 7)
            YIN(NN)= G2(J+NJ* 8)
            ZIN(NN)= G2(J+NJ* 2)+G(J+NJ* 2)*TWO
            NN= NN+1
            XIN(NN)= G2(J+NJ* 3)+G(J+NJ)
            YIN(NN)= G2(J+NJ* 5)+G(J)
            ZIN(NN)= G2(J+NJ* 9)
            NN= NN+1
            XIN(NN)= G2(J+NJ* 4)+G(J+NJ* 2)
            YIN(NN)= G2(J+NJ* 9)
            ZIN(NN)= G2(J+NJ* 7)+G(J)
            NN= NN+1
            XIN(NN)= G2(J+NJ* 9)
            YIN(NN)= G2(J+NJ* 6)+G(J+NJ* 2)
            ZIN(NN)= G2(J+NJ* 8)+G(J+NJ)
  130    CONTINUE
         IF(.NOT.NORM) GO TO 999
         N1= 3
         DO 133 J=1,NJ
            DO 132 NN=N1+1,N1+3
               XIN(NN)= XIN(NN)*SR3
               YIN(NN)= YIN(NN)*SR3
               ZIN(NN)= ZIN(NN)*SR3
  132       CONTINUE
  133    N1= N1+NI
         GO TO 999
      END IF
C F'
      IF(MINI.LE.11) THEN
         DO 140 J=1,NJ
            NN= NN+1
            XIN(NN)= G2(J)      +G(J)      *THR
            YIN(NN)= G2(J+NJ* 3)
            ZIN(NN)= G2(J+NJ* 4)
            NN= NN+1
            XIN(NN)= G2(J+NJ* 5)
            YIN(NN)= G2(J+NJ)   +G(J+NJ)   *THR
            ZIN(NN)= G2(J+NJ* 6)
C--JOSE SUGGESTED CHANGE FROM G TO G2 IN PREVIOUS LINE, BRETT CONFIRMS
            NN= NN+1
            XIN(NN)= G2(J+NJ* 7)
            YIN(NN)= G2(J+NJ* 8)
            ZIN(NN)= G2(J+NJ* 2)+G(J+NJ* 2)*THR
            NN= NN+1
            XIN(NN)= G2(J+NJ* 3)+G(J+NJ* 3)*TWO
            YIN(NN)= G2(J+NJ* 9)+G(J)
            ZIN(NN)= G2(J+NJ*12)
            NN= NN+1
            XIN(NN)= G2(J+NJ* 4)+G(J+NJ* 4)*TWO
            YIN(NN)= G2(J+NJ*12)
            ZIN(NN)= G2(J+NJ*10)+G(J)
            NN= NN+1
            XIN(NN)= G2(J+NJ* 9)+G(J+NJ)
            YIN(NN)= G2(J+NJ* 5)+G(J+NJ* 3)*TWO
            ZIN(NN)= G2(J+NJ*13)
            NN= NN+1
            XIN(NN)= G2(J+NJ*13)
            YIN(NN)= G2(J+NJ* 6)+G(J+NJ* 5)*TWO
            ZIN(NN)= G2(J+NJ*11)+G(J+NJ)
            NN= NN+1
            XIN(NN)= G2(J+NJ*10)+G(J+NJ* 2)
            YIN(NN)= G2(J+NJ*14)
C  JOSE SUGGESTED NEXT LINE SHOULD BE 7, NOT 13
            ZIN(NN)= G2(J+NJ* 7)+G(J+NJ* 4)*TWO
            NN= NN+1
            XIN(NN)= G2(J+NJ*14)
            YIN(NN)= G2(J+NJ*11)+G(J+NJ* 2)
            ZIN(NN)= G2(J+NJ* 8)+G(J+NJ* 5)*TWO
            NN= NN+1
            XIN(NN)= G2(J+NJ*12)+G(J+NJ* 5)
            YIN(NN)= G2(J+NJ*13)+G(J+NJ* 4)
            ZIN(NN)= G2(J+NJ*14)+G(J+NJ* 3)
  140    CONTINUE
         IF(.NOT.NORM) GO TO 999
         N1= 3
         DO 145 J=1,NJ
            N2= N1+6
            DO 142 NN=N1+1,N2
               XIN(NN)= XIN(NN)*SR5
               YIN(NN)= YIN(NN)*SR5
               ZIN(NN)= ZIN(NN)*SR5
  142       CONTINUE
            NN= N2+1
C           DO 144 NN=N2+1,N2+1
               XIN(NN)= XIN(NN)*S35
               YIN(NN)= YIN(NN)*S35
               ZIN(NN)= ZIN(NN)*S35
C 144       CONTINUE
  145    N1= N1+NI
         GO TO 999
      END IF
C G'
      IF(MINI.LE.21) THEN
         DO 150 J=1,NJ
            NN= NN+1
            XIN(NN)= G2(J)      +G(J)      *FOR
            YIN(NN)= G2(J+NJ* 3)
            ZIN(NN)= G2(J+NJ* 4)
            NN= NN+1
            XIN(NN)= G2(J+NJ* 5)
            YIN(NN)= G2(J+NJ)   +G(J+NJ)   *FOR
            ZIN(NN)= G2(J+NJ* 6)
            NN= NN+1
            XIN(NN)= G2(J+NJ* 7)
            YIN(NN)= G2(J+NJ* 8)
            ZIN(NN)= G2(J+NJ* 2)+G(J+NJ* 2)*FOR
            NN= NN+1
            XIN(NN)= G2(J+NJ* 3)+G(J+NJ* 3)*THR
            YIN(NN)= G2(J+NJ* 9)+G(J)
            ZIN(NN)= G2(J+NJ*15)
            NN= NN+1
            XIN(NN)= G2(J+NJ* 4)+G(J+NJ* 4)*THR
            YIN(NN)= G2(J+NJ*15)
            ZIN(NN)= G2(J+NJ*10)+G(J)
            NN= NN+1
            XIN(NN)= G2(J+NJ*11)+G(J+NJ)
            YIN(NN)= G2(J+NJ* 5)+G(J+NJ* 5)*THR
            ZIN(NN)= G2(J+NJ*16)
            NN= NN+1
            XIN(NN)= G2(J+NJ*16)
            YIN(NN)= G2(J+NJ* 6)+G(J+NJ* 6)*THR
            ZIN(NN)= G2(J+NJ*12)+G(J+NJ)
            NN= NN+1
            XIN(NN)= G2(J+NJ*13)+G(J+NJ* 2)
            YIN(NN)= G2(J+NJ*17)
            ZIN(NN)= G2(J+NJ* 7)+G(J+NJ* 7)*THR
            NN= NN+1
            XIN(NN)= G2(J+NJ*17)
            YIN(NN)= G2(J+NJ*14)+G(J+NJ* 2)
            ZIN(NN)= G2(J+NJ* 8)+G(J+NJ* 8)*THR
            NN= NN+1
            XIN(NN)= G2(J+NJ* 9)+G(J+NJ* 5)*TWO
            YIN(NN)= G2(J+NJ*11)+G(J+NJ* 3)*TWO
            ZIN(NN)= G2(J+NJ*18)
            NN= NN+1
            XIN(NN)= G2(J+NJ*10)+G(J+NJ* 7)*TWO
            YIN(NN)= G2(J+NJ*19)
            ZIN(NN)= G2(J+NJ*13)+G(J+NJ* 4)*TWO
            NN= NN+1
            XIN(NN)= G2(J+NJ*20)
            YIN(NN)= G2(J+NJ*12)+G(J+NJ* 8)*TWO
            ZIN(NN)= G2(J+NJ*14)+G(J+NJ* 6)*TWO
            NN= NN+1
            XIN(NN)= G2(J+NJ*15)+G(J+NJ* 9)*TWO
            YIN(NN)= G2(J+NJ*18)+G(J+NJ* 4)
            ZIN(NN)= G2(J+NJ*19)+G(J+NJ* 3)
            NN= NN+1
            XIN(NN)= G2(J+NJ*18)+G(J+NJ* 6)
            YIN(NN)= G2(J+NJ*16)+G(J+NJ* 9)*TWO
            ZIN(NN)= G2(J+NJ*20)+G(J+NJ* 5)
            NN= NN+1
            XIN(NN)= G2(J+NJ*19)+G(J+NJ* 8)
            YIN(NN)= G2(J+NJ*20)+G(J+NJ* 7)
            ZIN(NN)= G2(J+NJ*17)+G(J+NJ* 9)*TWO
  150    CONTINUE
         IF(.NOT.NORM) GO TO 999
C  JOSE APPLIED CORRECT RE-NORMALIZATION FACTORS HERE
         N1= 3
         DO 157 J=1,NJ
            N2= N1+6
            N3= N2+3
            DO 152 NN=N1+1,N2
               XIN(NN)= XIN(NN)*SR7
               YIN(NN)= YIN(NN)*SR7
               ZIN(NN)= ZIN(NN)*SR7
  152       CONTINUE
            DO 154 NN=N2+1,N3
               XIN(NN)= XIN(NN)*S53
               YIN(NN)= YIN(NN)*S53
               ZIN(NN)= ZIN(NN)*S53
  154       CONTINUE
            DO 156 NN=N3+1,N3+3
               XIN(NN)= XIN(NN)*S57
               YIN(NN)= YIN(NN)*S57
               ZIN(NN)= ZIN(NN)*S57
  156       CONTINUE
  157    N1= N1+NI
         GO TO 999
      ELSE
         WRITE(6,*) 'IN FORMDR H ATTEMPT',MINI,MAXI,MINJ,MAXJ
         CALL ABRT
      END IF
C
  999 CONTINUE
      RETURN
      END
C*MODULE ECPDER  *DECK FORMII
      SUBROUTINE FORMII(GP1P1,GP1M1,GM1P1,GM1M1,GPP,
     *                  MINI,MAXI,MINJ,MAXJ,NORM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION GP1P1(*),GP1M1(*),GM1P1(*),GM1M1(*),GPP(9,*)
      LOGICAL NORM
C
      PARAMETER (ZER=0.0D+00, TWO=2.0D+00, THR=3.0D+00, FOR=4.0D+00)
      PARAMETER (SR3=1.73205080756888D+00, SR5=2.23606797749979D+00)
      PARAMETER (SR7=2.64575131106459D+00)
      PARAMETER (S35=SR3*SR5, S57=SR5*SR7, S53=S57/SR3)
C
      DIMENSION XM1(21*10),YM1(21*10),ZM1(21*10),
     2          XP1(21*21),YP1(21*21),ZP1(21*21)
C
C FORM THE SECOND DERIVATIVE INTEGRALS OF TYPE <D'| |D'>
C THIS ROUTINE IS COMPLETLY GENERAL AND GOOD THROUGH G',G'
C
      DO 101 I=1,21*10
         XM1(I)= ZER
         YM1(I)= ZER
         ZM1(I)= ZER
  101 CONTINUE
      DO 102 I=1,21*21
         XP1(I)= ZER
         YP1(I)= ZER
         ZP1(I)= ZER
  102 CONTINUE
C
C FIRST FORM THE INTEGRALS DUE TO THE DERIVATIVE ON THE LEFT
C SINCE THIS IS JUST LIKE A FIRST DERIVATIVE FORMDR IS USED
C
      MINJM1= 1
      MAXJM1= 1
      MINJP1= 2
      MAXJP1= 4
      JTYPE = 1
      IF(MINJ.EQ. 2) THEN
         MINJP1= 5
         MAXJP1=10
         JTYPE = 2
      ELSEIF(MINJ.EQ. 5) THEN
         MINJM1= 2
         MAXJM1= 4
         MINJP1=11
         MAXJP1=20
         JTYPE = 3
      ELSEIF(MINJ.EQ.11) THEN
         MINJM1= 5
         MAXJM1=10
         MINJP1=21
         MAXJP1=35
         JTYPE = 4
      ELSEIF(MINJ.EQ.21) THEN
         MINJM1=11
         MAXJM1=20
         MINJP1=36
         MAXJP1=56
         JTYPE = 5
      END IF
C
      IF(JTYPE.GE.2)
     *CALL FORMDR(GM1M1,GP1M1,XM1,YM1,ZM1,MINI,MAXI,MINJM1,MAXJM1,NORM)
      CALL FORMDR(GM1P1,GP1P1,XP1,YP1,ZP1,MINI,MAXI,MINJP1,MAXJP1,NORM)
C
C NOW FORM THE TOTAL SECOND DERIVATIVE BY FORMING THE DERIVATIVE ON THE
C RIGHT SIDE
C
C NOTE THAT THIS CODE IS SIMILAR TO THE REGULAR FORMDR CODE, BUT WE ARE
C FORMING THE DERIVATIVE ON THE RIGHT AND FORMING A SECOND DERIVATIVE
C
      NN= 0
      NI= MAXI-MINI+1
C S'
      IF(MINJ.EQ.1) THEN
         DO 110 I=1,NI
            GPP(1,I)= XP1(I)
            GPP(2,I)= XP1(I+NI)
            GPP(3,I)= XP1(I+NI* 2)
            GPP(4,I)= YP1(I)
            GPP(5,I)= YP1(I+NI)
            GPP(6,I)= YP1(I+NI* 2)
            GPP(7,I)= ZP1(I)
            GPP(8,I)= ZP1(I+NI)
            GPP(9,I)= ZP1(I+NI* 2)
  110    CONTINUE
         GO TO 999
      END IF
C P'
      IF(MINJ.LE.2) THEN
         DO 120 I=1,NI
            NN= I
            GPP(1,NN)= XP1(I)      +XM1(I)
            GPP(2,NN)= XP1(I+NI* 3)
            GPP(3,NN)= XP1(I+NI* 4)
            GPP(4,NN)= YP1(I)      +YM1(I)
            GPP(5,NN)= YP1(I+NI* 3)
            GPP(6,NN)= YP1(I+NI* 4)
            GPP(7,NN)= ZP1(I)      +ZM1(I)
            GPP(8,NN)= ZP1(I+NI* 3)
            GPP(9,NN)= ZP1(I+NI* 4)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI* 3)
            GPP(2,NN)= XP1(I+NI   )+XM1(I)
            GPP(3,NN)= XP1(I+NI* 5)
            GPP(4,NN)= YP1(I+NI* 3)
            GPP(5,NN)= YP1(I+NI   )+YM1(I)
            GPP(6,NN)= YP1(I+NI* 5)
            GPP(7,NN)= ZP1(I+NI* 3)
            GPP(8,NN)= ZP1(I+NI   )+ZM1(I)
            GPP(9,NN)= ZP1(I+NI* 5)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI* 4)
            GPP(2,NN)= XP1(I+NI* 5)
            GPP(3,NN)= XP1(I+NI* 2)+XM1(I)
            GPP(4,NN)= YP1(I+NI* 4)
            GPP(5,NN)= YP1(I+NI* 5)
            GPP(6,NN)= YP1(I+NI* 2)+YM1(I)
            GPP(7,NN)= ZP1(I+NI* 4)
            GPP(8,NN)= ZP1(I+NI* 5)
            GPP(9,NN)= ZP1(I+NI* 2)+ZM1(I)
  120    CONTINUE
         GO TO 999
      END IF
C D'
      IF(MINJ.LE.5) THEN
         DO 130 I=1,NI
            NN= I
            GPP(1,NN)= XP1(I)      +XM1(I)      *TWO
            GPP(2,NN)= XP1(I+NI* 3)
            GPP(3,NN)= XP1(I+NI* 4)
            GPP(4,NN)= YP1(I)      +YM1(I)      *TWO
            GPP(5,NN)= YP1(I+NI* 3)
            GPP(6,NN)= YP1(I+NI* 4)
            GPP(7,NN)= ZP1(I)      +ZM1(I)      *TWO
            GPP(8,NN)= ZP1(I+NI* 3)
            GPP(9,NN)= ZP1(I+NI* 4)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI* 5)
            GPP(2,NN)= XP1(I+NI)   +XM1(I+NI)   *TWO
            GPP(3,NN)= XP1(I+NI* 6)
            GPP(4,NN)= YP1(I+NI* 5)
            GPP(5,NN)= YP1(I+NI)   +YM1(I+NI)   *TWO
            GPP(6,NN)= YP1(I+NI* 6)
            GPP(7,NN)= ZP1(I+NI* 5)
            GPP(8,NN)= ZP1(I+NI)   +ZM1(I+NI)   *TWO
            GPP(9,NN)= ZP1(I+NI* 6)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI* 7)
            GPP(2,NN)= XP1(I+NI* 8)
            GPP(3,NN)= XP1(I+NI* 2)+XM1(I+NI* 2)*TWO
            GPP(4,NN)= YP1(I+NI* 7)
            GPP(5,NN)= YP1(I+NI* 8)
            GPP(6,NN)= YP1(I+NI* 2)+YM1(I+NI* 2)*TWO
            GPP(7,NN)= ZP1(I+NI* 7)
            GPP(8,NN)= ZP1(I+NI* 8)
            GPP(9,NN)= ZP1(I+NI* 2)+ZM1(I+NI* 2)*TWO
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI* 3)+XM1(I+NI)
            GPP(2,NN)= XP1(I+NI* 5)+XM1(I)
            GPP(3,NN)= XP1(I+NI* 9)
            GPP(4,NN)= YP1(I+NI* 3)+YM1(I+NI)
            GPP(5,NN)= YP1(I+NI* 5)+YM1(I)
            GPP(6,NN)= YP1(I+NI* 9)
            GPP(7,NN)= ZP1(I+NI* 3)+ZM1(I+NI)
            GPP(8,NN)= ZP1(I+NI* 5)+ZM1(I)
            GPP(9,NN)= ZP1(I+NI* 9)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI* 4)+XM1(I+NI* 2)
            GPP(2,NN)= XP1(I+NI* 9)
            GPP(3,NN)= XP1(I+NI* 7)+XM1(I)
            GPP(4,NN)= YP1(I+NI* 4)+YM1(I+NI* 2)
            GPP(5,NN)= YP1(I+NI* 9)
            GPP(6,NN)= YP1(I+NI* 7)+YM1(I)
            GPP(7,NN)= ZP1(I+NI* 4)+ZM1(I+NI* 2)
            GPP(8,NN)= ZP1(I+NI* 9)
            GPP(9,NN)= ZP1(I+NI* 7)+ZM1(I)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI* 9)
            GPP(2,NN)= XP1(I+NI* 6)+XM1(I+NI* 2)
            GPP(3,NN)= XP1(I+NI* 8)+XM1(I+NI)
            GPP(4,NN)= YP1(I+NI* 9)
            GPP(5,NN)= YP1(I+NI* 6)+YM1(I+NI* 2)
            GPP(6,NN)= YP1(I+NI* 8)+YM1(I+NI)
            GPP(7,NN)= ZP1(I+NI* 9)
            GPP(8,NN)= ZP1(I+NI* 6)+ZM1(I+NI* 2)
            GPP(9,NN)= ZP1(I+NI* 8)+ZM1(I+NI)
  130    CONTINUE
         IF(.NOT.NORM) GO TO 999
         N1= NI+NI+NI
         DO 132 NN=N1+1,N1+N1
            DO 131 M=1,9
  131       GPP(M,NN)= GPP(M,NN)*SR3
  132    CONTINUE
         GO TO 999
      END IF
C F'
      IF(MINJ.LE.11) THEN
         DO 140 I=1,NI
            NN= I
            GPP(1,NN)= XP1(I)      +XM1(I)      *THR
            GPP(2,NN)= XP1(I+NI* 3)
            GPP(3,NN)= XP1(I+NI* 4)
            GPP(4,NN)= YP1(I)      +YM1(I)      *THR
            GPP(5,NN)= YP1(I+NI* 3)
            GPP(6,NN)= YP1(I+NI* 4)
            GPP(7,NN)= ZP1(I)      +ZM1(I)      *THR
            GPP(8,NN)= ZP1(I+NI* 3)
            GPP(9,NN)= ZP1(I+NI* 4)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI* 5)
            GPP(2,NN)= XP1(I+NI)   +XM1(I+NI)   *THR
            GPP(3,NN)= XP1(I+NI* 6)
            GPP(4,NN)= YP1(I+NI* 5)
            GPP(5,NN)= YP1(I+NI)   +YM1(I+NI)   *THR
            GPP(6,NN)= YP1(I+NI* 6)
            GPP(7,NN)= ZP1(I+NI* 5)
            GPP(8,NN)= ZP1(I+NI)   +ZM1(I+NI)   *THR
            GPP(9,NN)= ZP1(I+NI* 6)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI* 7)
            GPP(2,NN)= XP1(I+NI* 8)
            GPP(3,NN)= XP1(I+NI* 2)+XM1(I+NI* 2)*THR
            GPP(4,NN)= YP1(I+NI* 7)
            GPP(5,NN)= YP1(I+NI* 8)
            GPP(6,NN)= YP1(I+NI* 2)+YM1(I+NI* 2)*THR
            GPP(7,NN)= ZP1(I+NI* 7)
            GPP(8,NN)= ZP1(I+NI* 8)
            GPP(9,NN)= ZP1(I+NI* 2)+ZM1(I+NI* 2)*THR
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI* 3)+XM1(I+NI* 3)*TWO
            GPP(2,NN)= XP1(I+NI* 9)+XM1(I)
            GPP(3,NN)= XP1(I+NI*12)
            GPP(4,NN)= YP1(I+NI* 3)+YM1(I+NI* 3)*TWO
            GPP(5,NN)= YP1(I+NI* 9)+YM1(I)
            GPP(6,NN)= YP1(I+NI*12)
            GPP(7,NN)= ZP1(I+NI* 3)+ZM1(I+NI* 3)*TWO
            GPP(8,NN)= ZP1(I+NI* 9)+ZM1(I)
            GPP(9,NN)= ZP1(I+NI*12)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI* 4)+XM1(I+NI* 4)*TWO
            GPP(2,NN)= XP1(I+NI*12)
            GPP(3,NN)= XP1(I+NI*10)+XM1(I)
            GPP(4,NN)= YP1(I+NI* 4)+YM1(I+NI* 4)*TWO
            GPP(5,NN)= YP1(I+NI*12)
            GPP(6,NN)= YP1(I+NI*10)+YM1(I)
            GPP(7,NN)= ZP1(I+NI* 4)+ZM1(I+NI* 4)*TWO
            GPP(8,NN)= ZP1(I+NI*12)
            GPP(9,NN)= ZP1(I+NI*10)+ZM1(I)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI* 9)+XM1(I+NI)
            GPP(2,NN)= XP1(I+NI* 5)+XM1(I+NI* 3)*TWO
            GPP(3,NN)= XP1(I+NI*13)
            GPP(4,NN)= YP1(I+NI* 9)+YM1(I+NI)
            GPP(5,NN)= YP1(I+NI* 5)+YM1(I+NI* 3)*TWO
            GPP(6,NN)= YP1(I+NI*13)
            GPP(7,NN)= ZP1(I+NI* 9)+ZM1(I+NI)
            GPP(8,NN)= ZP1(I+NI* 5)+ZM1(I+NI* 3)*TWO
            GPP(9,NN)= ZP1(I+NI*13)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI*13)
            GPP(2,NN)= XP1(I+NI* 6)+XM1(I+NI* 5)*TWO
            GPP(3,NN)= XP1(I+NI*11)+XM1(I+NI)
            GPP(4,NN)= YP1(I+NI*13)
            GPP(5,NN)= YP1(I+NI* 6)+YM1(I+NI* 5)*TWO
            GPP(6,NN)= YP1(I+NI*11)+YM1(I+NI)
            GPP(7,NN)= ZP1(I+NI*13)
            GPP(8,NN)= ZP1(I+NI* 6)+ZM1(I+NI* 5)*TWO
            GPP(9,NN)= ZP1(I+NI*11)+ZM1(I+NI)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI*10)+XM1(I+NI* 2)
            GPP(2,NN)= XP1(I+NI*14)
            GPP(3,NN)= XP1(I+NI*13)+XM1(I+NI* 4)*TWO
            GPP(4,NN)= YP1(I+NI*10)+YM1(I+NI* 2)
            GPP(5,NN)= YP1(I+NI*14)
            GPP(6,NN)= YP1(I+NI*13)+YM1(I+NI* 4)*TWO
            GPP(7,NN)= ZP1(I+NI*10)+ZM1(I+NI* 2)
            GPP(8,NN)= ZP1(I+NI*14)
            GPP(9,NN)= ZP1(I+NI*13)+ZM1(I+NI* 4)*TWO
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI*14)
            GPP(2,NN)= XP1(I+NI*11)+XM1(I+NI* 2)
            GPP(3,NN)= XP1(I+NI* 8)+XM1(I+NI* 5)*TWO
            GPP(4,NN)= YP1(I+NI*14)
            GPP(5,NN)= YP1(I+NI*11)+YM1(I+NI* 2)
            GPP(6,NN)= YP1(I+NI* 8)+YM1(I+NI* 5)*TWO
            GPP(7,NN)= ZP1(I+NI*14)
            GPP(8,NN)= ZP1(I+NI*11)+ZM1(I+NI* 2)
            GPP(9,NN)= ZP1(I+NI* 8)+ZM1(I+NI* 5)*TWO
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI*12)+XM1(I+NI* 5)
            GPP(2,NN)= XP1(I+NI*13)+XM1(I+NI* 4)
            GPP(3,NN)= XP1(I+NI*14)+XM1(I+NI* 3)
            GPP(4,NN)= YP1(I+NI*12)+YM1(I+NI* 5)
            GPP(5,NN)= YP1(I+NI*13)+YM1(I+NI* 4)
            GPP(6,NN)= YP1(I+NI*14)+YM1(I+NI* 3)
            GPP(7,NN)= ZP1(I+NI*12)+ZM1(I+NI* 5)
            GPP(8,NN)= ZP1(I+NI*13)+ZM1(I+NI* 4)
            GPP(9,NN)= ZP1(I+NI*14)+ZM1(I+NI* 3)
  140    CONTINUE
         IF(.NOT.NORM) GO TO 999
         N1= NI+NI+NI
         N2= N1+N1+N1
         DO 142 NN=N1+1,N2
            DO 141 M=1,9
  141       GPP(M,NN)= GPP(M,NN)*SR5
  142    CONTINUE
         DO 144 NN=N2+1,N2+NI
            DO 143 M=1,9
  143       GPP(M,NN)= GPP(M,NN)*S35
  144    CONTINUE
         GO TO 999
      END IF
C G'
      IF(MINJ.LE.21) THEN
         DO 150 I=1,NI
            NN= I
            GPP(1,NN)= XP1(I)      +XM1(I)      *FOR
            GPP(2,NN)= XP1(I+NI* 3)
            GPP(3,NN)= XP1(I+NI* 4)
            GPP(4,NN)= YP1(I)      +YM1(I)      *FOR
            GPP(5,NN)= YP1(I+NI* 3)
            GPP(6,NN)= YP1(I+NI* 4)
            GPP(7,NN)= ZP1(I)      +ZM1(I)      *FOR
            GPP(8,NN)= ZP1(I+NI* 3)
            GPP(9,NN)= ZP1(I+NI* 4)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI* 5)
            GPP(2,NN)= XP1(I+NI)   +XM1(I+NI)   *FOR
            GPP(3,NN)= XP1(I+NI* 6)
            GPP(4,NN)= YP1(I+NI* 5)
            GPP(5,NN)= YP1(I+NI)   +YM1(I+NI)   *FOR
            GPP(6,NN)= YP1(I+NI* 6)
            GPP(7,NN)= ZP1(I+NI* 5)
            GPP(8,NN)= ZP1(I+NI)   +ZM1(I+NI)   *FOR
            GPP(9,NN)= ZP1(I+NI* 6)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI* 7)
            GPP(2,NN)= XP1(I+NI* 8)
            GPP(3,NN)= XP1(I+NI* 2)+XM1(I+NI* 2)*FOR
            GPP(4,NN)= YP1(I+NI* 7)
            GPP(5,NN)= YP1(I+NI* 8)
            GPP(6,NN)= YP1(I+NI* 2)+YM1(I+NI* 2)*FOR
            GPP(7,NN)= ZP1(I+NI* 7)
            GPP(8,NN)= ZP1(I+NI* 8)
            GPP(9,NN)= ZP1(I+NI* 2)+ZM1(I+NI* 2)*FOR
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI* 3)+XM1(I+NI* 3)*THR
            GPP(2,NN)= XP1(I+NI* 9)+XM1(I)
            GPP(3,NN)= XP1(I+NI*15)
            GPP(4,NN)= YP1(I+NI* 3)+YM1(I+NI* 3)*THR
            GPP(5,NN)= YP1(I+NI* 9)+YM1(I)
            GPP(6,NN)= YP1(I+NI*15)
            GPP(7,NN)= ZP1(I+NI* 3)+ZM1(I+NI* 3)*THR
            GPP(8,NN)= ZP1(I+NI* 9)+ZM1(I)
            GPP(9,NN)= ZP1(I+NI*15)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI* 4)+XM1(I+NI* 4)*THR
            GPP(2,NN)= XP1(I+NI*15)
            GPP(3,NN)= XP1(I+NI*10)+XM1(I)
            GPP(4,NN)= YP1(I+NI* 4)+YM1(I+NI* 4)*THR
            GPP(5,NN)= YP1(I+NI*15)
            GPP(6,NN)= YP1(I+NI*10)+YM1(I)
            GPP(7,NN)= ZP1(I+NI* 4)+ZM1(I+NI* 4)*THR
            GPP(8,NN)= ZP1(I+NI*15)
            GPP(9,NN)= ZP1(I+NI*10)+ZM1(I)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI*11)+XM1(I+NI)
            GPP(2,NN)= XP1(I+NI* 5)+XM1(I+NI* 5)*THR
            GPP(3,NN)= XP1(I+NI*16)
            GPP(4,NN)= YP1(I+NI*11)+YM1(I+NI)
            GPP(5,NN)= YP1(I+NI* 5)+YM1(I+NI* 5)*THR
            GPP(6,NN)= YP1(I+NI*16)
            GPP(7,NN)= ZP1(I+NI*11)+ZM1(I+NI)
            GPP(8,NN)= ZP1(I+NI* 5)+ZM1(I+NI* 5)*THR
            GPP(9,NN)= ZP1(I+NI*16)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI*16)
            GPP(2,NN)= XP1(I+NI* 6)+XM1(I+NI* 6)*THR
            GPP(3,NN)= XP1(I+NI*12)+XM1(I+NI)
            GPP(4,NN)= YP1(I+NI*16)
            GPP(5,NN)= YP1(I+NI* 6)+YM1(I+NI* 6)*THR
            GPP(6,NN)= YP1(I+NI*12)+YM1(I+NI)
            GPP(7,NN)= ZP1(I+NI*16)
            GPP(8,NN)= ZP1(I+NI* 6)+ZM1(I+NI* 6)*THR
            GPP(9,NN)= ZP1(I+NI*12)+ZM1(I+NI)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI*13)+XM1(I+NI* 2)
            GPP(2,NN)= XP1(I+NI*17)
            GPP(3,NN)= XP1(I+NI* 7)+XM1(I+NI* 7)*THR
            GPP(4,NN)= YP1(I+NI*13)+YM1(I+NI* 2)
            GPP(5,NN)= YP1(I+NI*17)
            GPP(6,NN)= YP1(I+NI* 7)+YM1(I+NI* 7)*THR
            GPP(7,NN)= ZP1(I+NI*13)+ZM1(I+NI* 2)
            GPP(8,NN)= ZP1(I+NI*17)
            GPP(9,NN)= ZP1(I+NI* 7)+ZM1(I+NI* 7)*THR
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI*17)
            GPP(2,NN)= XP1(I+NI*14)+XM1(I+NI* 2)
            GPP(3,NN)= XP1(I+NI* 8)+XM1(I+NI* 8)*THR
            GPP(4,NN)= YP1(I+NI*17)
            GPP(5,NN)= YP1(I+NI*14)+YM1(I+NI* 2)
            GPP(6,NN)= YP1(I+NI* 8)+YM1(I+NI* 8)*THR
            GPP(7,NN)= ZP1(I+NI*17)
            GPP(8,NN)= ZP1(I+NI*14)+ZM1(I+NI* 2)
            GPP(9,NN)= ZP1(I+NI* 8)+ZM1(I+NI* 8)*THR
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI* 9)+XM1(I+NI* 5)*TWO
            GPP(2,NN)= XP1(I+NI*11)+XM1(I+NI* 3)*TWO
            GPP(3,NN)= XP1(I+NI*18)
            GPP(4,NN)= YP1(I+NI* 9)+YM1(I+NI* 5)*TWO
            GPP(5,NN)= YP1(I+NI*11)+YM1(I+NI* 3)*TWO
            GPP(6,NN)= YP1(I+NI*18)
            GPP(7,NN)= ZP1(I+NI* 9)+ZM1(I+NI* 5)*TWO
            GPP(8,NN)= ZP1(I+NI*11)+ZM1(I+NI* 3)*TWO
            GPP(9,NN)= ZP1(I+NI*18)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI*10)+XM1(I+NI* 7)*TWO
            GPP(2,NN)= XP1(I+NI*19)
            GPP(3,NN)= XP1(I+NI*13)+XM1(I+NI* 4)*TWO
            GPP(4,NN)= YP1(I+NI*10)+YM1(I+NI* 7)*TWO
            GPP(5,NN)= YP1(I+NI*19)
            GPP(6,NN)= YP1(I+NI*13)+YM1(I+NI* 4)*TWO
            GPP(7,NN)= ZP1(I+NI*10)+ZM1(I+NI* 7)*TWO
            GPP(8,NN)= ZP1(I+NI*19)
            GPP(9,NN)= ZP1(I+NI*13)+ZM1(I+NI* 4)*TWO
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI*20)
            GPP(2,NN)= XP1(I+NI*12)+XM1(I+NI* 8)*TWO
            GPP(3,NN)= XP1(I+NI*14)+XM1(I+NI* 6)*TWO
            GPP(4,NN)= YP1(I+NI*20)
            GPP(5,NN)= YP1(I+NI*12)+YM1(I+NI* 8)*TWO
            GPP(6,NN)= YP1(I+NI*14)+YM1(I+NI* 6)*TWO
            GPP(7,NN)= ZP1(I+NI*20)
            GPP(8,NN)= ZP1(I+NI*12)+ZM1(I+NI* 8)*TWO
            GPP(9,NN)= ZP1(I+NI*14)+ZM1(I+NI* 6)*TWO
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI*15)+XM1(I+NI* 9)*TWO
            GPP(2,NN)= XP1(I+NI*18)+XM1(I+NI* 4)
            GPP(3,NN)= XP1(I+NI*19)+XM1(I+NI* 3)
            GPP(4,NN)= YP1(I+NI*15)+YM1(I+NI* 9)*TWO
            GPP(5,NN)= YP1(I+NI*18)+YM1(I+NI* 4)
            GPP(6,NN)= YP1(I+NI*19)+YM1(I+NI* 3)
            GPP(7,NN)= ZP1(I+NI*15)+ZM1(I+NI* 9)*TWO
            GPP(8,NN)= ZP1(I+NI*18)+ZM1(I+NI* 4)
            GPP(9,NN)= ZP1(I+NI*19)+ZM1(I+NI* 3)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI*18)+XM1(I+NI* 6)
            GPP(2,NN)= XP1(I+NI*16)+XM1(I+NI* 9)*TWO
            GPP(3,NN)= XP1(I+NI*20)+XM1(I+NI* 5)
            GPP(4,NN)= YP1(I+NI*18)+YM1(I+NI* 6)
            GPP(5,NN)= YP1(I+NI*16)+YM1(I+NI* 9)*TWO
            GPP(6,NN)= YP1(I+NI*20)+YM1(I+NI* 5)
            GPP(7,NN)= ZP1(I+NI*18)+ZM1(I+NI* 6)
            GPP(8,NN)= ZP1(I+NI*16)+ZM1(I+NI* 9)*TWO
            GPP(9,NN)= ZP1(I+NI*20)+ZM1(I+NI* 5)
            NN=NN+NI
            GPP(1,NN)= XP1(I+NI*19)+XM1(I+NI* 8)
            GPP(2,NN)= XP1(I+NI*20)+XM1(I+NI* 7)
            GPP(3,NN)= XP1(I+NI*17)+XM1(I+NI* 9)*TWO
            GPP(4,NN)= YP1(I+NI*19)+YM1(I+NI* 8)
            GPP(5,NN)= YP1(I+NI*20)+YM1(I+NI* 7)
            GPP(6,NN)= YP1(I+NI*17)+YM1(I+NI* 9)*TWO
            GPP(7,NN)= ZP1(I+NI*19)+ZM1(I+NI* 8)
            GPP(8,NN)= ZP1(I+NI*20)+ZM1(I+NI* 7)
            GPP(9,NN)= ZP1(I+NI*17)+ZM1(I+NI* 9)*TWO
  150    CONTINUE
         IF(.NOT.NORM) GO TO 999
         N1= NI+NI+NI
         N2= N1+N1+N1
         N3= N2+N1
         DO 152 NN=N1+1,N2
            DO 151 M=1,9
  151       GPP(M,NN)= GPP(M,NN)*SR7
  152    CONTINUE
         DO 154 NN=N2+1,N3
            DO 153 M=1,9
  153       GPP(M,NN)= GPP(M,NN)*S53
  154    CONTINUE
         DO 156 NN=N3+1,N3+N1
            DO 155 M=1,9
  155       GPP(M,NN)= GPP(M,NN)*S57
  156    CONTINUE
         GO TO 999
      ELSE
         WRITE(6,*)'IN FORMII H ATTEMPT',MINI,MAXI,MINJ,MAXJ
         CALL ABRT
      END IF
C
  999 CONTINUE
      RETURN
      END
C*MODULE ECPDER  *DECK FORMIJ
      SUBROUTINE FORMIJ(GP2,G,GM2,GPP,MINI,MAXI,MINJ,MAXJ,NORM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION GP2(*),G(*),GM2(*),GPP(9,*)
      LOGICAL NORM
C
      PARAMETER (TWO=2.0D+00, THR=3.0D+00, FOR=4.0D+00,
     2           FIV=5.0D+00, SIX=6.0D+00, SEV=7.0D+00,
     3           RNINE=9.0D+00, TWELVE=12.D+00)
      PARAMETER (SR3=1.73205080756888D+00, SR5=2.23606797749979D+00)
      PARAMETER (SR7=2.64575131106459D+00)
      PARAMETER (S35=SR3*SR5, S57=SR5*SR7, S53=S57/SR3)
C
C FORM THE SECOND DERIVATIVE INTEGRALS OF TYPE <D''| |D>
C THIS ROUTINE IS COMPLETLY GENERAL AND GOOD THROUGH G''
C REMEMBER THAT ORDER OF DERIVATIVES IS NOT IMPORTANT SO WE COMPUTE
C 6 TERMS AND EQUATE TO THE OTHER 3 (IE XY = YX)
C
      NN= 0
      NI= MAXI-MINI+1
      NJ= MAXJ-MINJ+1
C S''
      IF(MINI.EQ.1) THEN
         DO 110 J=1,NJ
            NN= NN+1
            GPP(1,NN)= GP2(J)      +G(J)
            GPP(2,NN)= GP2(J+NJ* 3)
            GPP(3,NN)= GP2(J+NJ* 4)
            GPP(4,NN)= GPP(2,NN)
            GPP(5,NN)= GP2(J+NJ)   +G(J)
            GPP(6,NN)= GP2(J+NJ* 5)
            GPP(7,NN)= GPP(3,NN)
            GPP(8,NN)= GPP(6,NN)
            GPP(9,NN)= GP2(J+NJ* 2)+G(J)
  110    CONTINUE
         GO TO 999
      END IF
C P''
      IF(MINI.LE.2) THEN
         DO 120 J=1,NJ
            NN= NN+1
            GPP(1,NN)= GP2(J)      +G(J)      *THR
            GPP(2,NN)= GP2(J+NJ* 3)+G(J+NJ)
            GPP(3,NN)= GP2(J+NJ* 4)+G(J+NJ* 2)
            GPP(4,NN)= GPP(2,NN)
            GPP(5,NN)= GP2(J+NJ* 5)+G(J)
            GPP(6,NN)= GP2(J+NJ* 9)
            GPP(7,NN)= GPP(3,NN)
            GPP(8,NN)= GPP(6,NN)
            GPP(9,NN)= GP2(J+NJ* 7)+G(J)
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ* 3)+G(J+NJ)
            GPP(2,NN)= GP2(J+NJ* 5)+G(J)
            GPP(3,NN)= GP2(J+NJ* 9)
            GPP(4,NN)= GPP(2,NN)
            GPP(5,NN)= GP2(J+NJ)   +G(J+NJ)   *THR
            GPP(6,NN)= GP2(J+NJ* 6)+G(J+NJ* 2)
            GPP(7,NN)= GPP(3,NN)
            GPP(8,NN)= GPP(6,NN)
            GPP(9,NN)= GP2(J+NJ* 8)+G(J+NJ)
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ* 4)+G(J+NJ* 2)
            GPP(2,NN)= GP2(J+NJ* 9)
            GPP(3,NN)= GP2(J+NJ* 7)+G(J)
            GPP(4,NN)= GPP(2,NN)
            GPP(5,NN)= GP2(J+NJ* 6)+G(J+NJ* 2)
            GPP(6,NN)= GP2(J+NJ* 8)+G(J+NJ)
            GPP(7,NN)= GPP(3,NN)
            GPP(8,NN)= GPP(6,NN)
            GPP(9,NN)= GP2(J+NJ* 2)+G(J+NJ* 2)*THR
  120    CONTINUE
         GO TO 999
      END IF
C D''
      IF(MINI.LE.5) THEN
         DO 130 J=1,NJ
            NN= NN+1
            GPP(1,NN)= GP2(J)      +G(J)      *FIV+GM2(J)*TWO
            GPP(2,NN)= GP2(J+NJ* 3)+G(J+NJ* 3)*TWO
            GPP(3,NN)= GP2(J+NJ* 4)+G(J+NJ* 4)*TWO
            GPP(4,NN)= GP2(J+NJ* 9)+G(J)
            GPP(5,NN)= GP2(J+NJ*12)
            GPP(6,NN)= GP2(J+NJ*10)+G(J)
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ* 9)+G(J+NJ)
            GPP(2,NN)= GP2(J+NJ* 5)+G(J+NJ* 3)*TWO
            GPP(3,NN)= GP2(J+NJ*13)
            GPP(4,NN)= GP2(J+NJ)   +G(J+NJ)   *FIV+GM2(J)*TWO
            GPP(5,NN)= GP2(J+NJ* 6)+G(J+NJ* 5)*TWO
            GPP(6,NN)= GP2(J+NJ*11)+G(J+NJ)
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*10)+G(J+NJ* 2)
            GPP(2,NN)= GP2(J+NJ*14)
            GPP(3,NN)= GP2(J+NJ* 7)+G(J+NJ* 4)*TWO
            GPP(4,NN)= GP2(J+NJ*11)+G(J+NJ* 2)
            GPP(5,NN)= GP2(J+NJ* 8)+G(J+NJ* 5)*TWO
            GPP(6,NN)= GP2(J+NJ* 2)+G(J+NJ* 2)*FIV+GM2(J)*TWO
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ* 3)+G(J+NJ* 3)*THR
            GPP(2,NN)= GP2(J+NJ* 9)+G(J)      +G(J+NJ)   +GM2(J)
            GPP(3,NN)= GP2(J+NJ*12)+G(J+NJ* 5)
            GPP(4,NN)= GP2(J+NJ* 5)+G(J+NJ* 3)*THR
            GPP(5,NN)= GP2(J+NJ*13)+G(J+NJ* 4)
            GPP(6,NN)= GP2(J+NJ*14)+G(J+NJ* 3)
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ* 4)+G(J+NJ* 4)*THR
            GPP(2,NN)= GP2(J+NJ*12)+G(J+NJ* 5)
            GPP(3,NN)= GP2(J+NJ*10)+G(J)      +G(J+NJ* 2)+GM2(J)
            GPP(4,NN)= GP2(J+NJ*13)+G(J+NJ* 4)
            GPP(5,NN)= GP2(J+NJ*14)+G(J+NJ* 3)
            GPP(6,NN)= GP2(J+NJ* 7)+G(J+NJ* 4)*THR
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*12)+G(J+NJ* 5)
            GPP(2,NN)= GP2(J+NJ*13)+G(J+NJ* 4)
            GPP(3,NN)= GP2(J+NJ*14)+G(J+NJ* 3)
            GPP(4,NN)= GP2(J+NJ* 6)+G(J+NJ* 5)*THR
            GPP(5,NN)= GP2(J+NJ*11)+G(J+NJ)   +G(J+NJ* 2)+GM2(J)
            GPP(6,NN)= GP2(J+NJ* 8)+G(J+NJ* 5)*THR
  130    CONTINUE
         IF(.NOT.NORM) GO TO 160
         N1= 3
         DO 133 J=1,NJ
            DO 132 NN=N1+1,N1+3
               DO 131 M=1,6
  131          GPP(M,NN)= GPP(M,NN)*SR3
  132       CONTINUE
  133    N1= N1+NI
         GO TO 160
      END IF
C F''
      IF(MINI.LE.11) THEN
         DO 140 J=1,NJ
            NN= NN+1
            GPP(1,NN)= GP2(J)      +G(J)      *SEV+GM2(J)      *SIX
            GPP(2,NN)= GP2(J+NJ* 3)+G(J+NJ* 3)*THR
            GPP(3,NN)= GP2(J+NJ* 4)+G(J+NJ* 4)*THR
            GPP(4,NN)= GP2(J+NJ* 9)+G(J)
            GPP(5,NN)= GP2(J+NJ*15)
            GPP(6,NN)= GP2(J+NJ*10)+G(J)
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*11)+G(J+NJ)
            GPP(2,NN)= GP2(J+NJ* 5)+G(J+NJ* 5)*THR
            GPP(3,NN)= GP2(J+NJ*16)
            GPP(4,NN)= GP2(J+NJ)   +G(J+NJ)   *SEV+GM2(J+NJ)   *SIX
            GPP(5,NN)= GP2(J+NJ* 6)+G(J+NJ* 6)*THR
            GPP(6,NN)= GP2(J+NJ*12)+G(J+NJ)
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*13)+G(J+NJ* 2)
            GPP(2,NN)= GP2(J+NJ*17)
            GPP(3,NN)= GP2(J+NJ* 7)+G(J+NJ* 7)*THR
            GPP(4,NN)= GP2(J+NJ*14)+G(J+NJ* 2)
            GPP(5,NN)= GP2(J+NJ* 8)+G(J+NJ* 8)*THR
            GPP(6,NN)= GP2(J+NJ* 2)+G(J+NJ* 2)*SEV+GM2(J+NJ* 2)*SIX
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ* 3)+G(J+NJ* 3)*FIV+GM2(J+NJ)   *TWO
            GPP(2,NN)= GP2(J+NJ* 9)+G(J)      +G(J+NJ* 5)*TWO+
     *                 GM2(J)      *TWO
            GPP(3,NN)= GP2(J+NJ*15)+G(J+NJ* 9)*TWO
            GPP(4,NN)= GP2(J+NJ*11)+G(J+NJ* 3)*THR
            GPP(5,NN)= GP2(J+NJ*18)+G(J+NJ* 4)
            GPP(6,NN)= GP2(J+NJ*19)+G(J+NJ* 3)
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ* 4)+G(J+NJ* 4)*FIV+GM2(J+NJ* 2)*TWO
            GPP(2,NN)= GP2(J+NJ*15)+G(J+NJ* 9)*TWO
            GPP(3,NN)= GP2(J+NJ*10)+G(J)      +G(J+NJ* 7)*TWO+
     *                 GM2(J)      *TWO
            GPP(4,NN)= GP2(J+NJ*18)+G(J+NJ* 7)
            GPP(5,NN)= GP2(J+NJ*19)+G(J+NJ* 3)
            GPP(6,NN)= GP2(J+NJ*13)+G(J+NJ* 4)*THR
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ* 9)+G(J+NJ* 5)*THR
            GPP(2,NN)= GP2(J+NJ*11)+G(J+NJ)   +G(J+NJ* 3)*TWO+
     *                 GM2(J+NJ)   *TWO
            GPP(3,NN)= GP2(J+NJ*18)+G(J+NJ* 6)
            GPP(4,NN)= GP2(J+NJ* 5)+G(J+NJ* 5)*FIV+GM2(J)      *TWO
            GPP(5,NN)= GP2(J+NJ*16)+G(J+NJ* 9)*TWO
            GPP(6,NN)= GP2(J+NJ*20)+G(J+NJ* 3)
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*18)+G(J+NJ* 6)
            GPP(2,NN)= GP2(J+NJ*16)+G(J+NJ* 9)*TWO
            GPP(3,NN)= GP2(J+NJ*20)+G(J+NJ* 5)
            GPP(4,NN)= GP2(J+NJ* 6)+G(J+NJ* 6)*FIV+GM2(J+NJ* 2)*TWO
            GPP(5,NN)= GP2(J+NJ*12)+G(J+NJ)   +G(J+NJ* 8)*TWO+
     *                 GM2(J+NJ)   *TWO
            GPP(6,NN)= GP2(J+NJ*14)+G(J+NJ* 6)*THR
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*10)+G(J+NJ* 7)*THR
            GPP(2,NN)= GP2(J+NJ*19)+G(J+NJ* 8)
            GPP(3,NN)= GP2(J+NJ*13)+G(J+NJ* 2)+G(J+NJ* 4)*TWO+
     *                 GM2(J+NJ* 2)*TWO
            GPP(4,NN)= GP2(J+NJ*20)+G(J+NJ* 7)
            GPP(5,NN)= GP2(J+NJ*17)+G(J+NJ* 9)*TWO
            GPP(6,NN)= GP2(J+NJ* 7)+G(J+NJ* 7)*FIV+GM2(J)      *TWO
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*19)+G(J+NJ* 8)
            GPP(2,NN)= GP2(J+NJ*20)+G(J+NJ* 7)
            GPP(3,NN)= GP2(J+NJ*17)+G(J+NJ* 9)*TWO
            GPP(4,NN)= GP2(J+NJ*12)+G(J+NJ* 8)*THR
            GPP(5,NN)= GP2(J+NJ*14)+G(J+NJ* 2)+G(J+NJ* 6)*TWO+
     *                 GM2(J+NJ* 2)*TWO
            GPP(6,NN)= GP2(J+NJ* 8)+G(J+NJ* 8)*FIV+GM2(J+NJ)   *TWO
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*15)+G(J+NJ* 9)*THR
            GPP(2,NN)= GP2(J+NJ*18)+G(J+NJ* 6)+G(J+NJ* 4)+GM2(J+NJ* 2)
            GPP(3,NN)= GP2(J+NJ*19)+G(J+NJ* 8)+G(J+NJ* 3)+GM2(J+NJ)
            GPP(4,NN)= GP2(J+NJ*16)+G(J+NJ* 9)*THR
            GPP(5,NN)= GP2(J+NJ*20)+G(J+NJ* 7)+G(J+NJ* 5)+GM2(J)
            GPP(6,NN)= GP2(J+NJ*17)+G(J+NJ* 9)*THR
  140    CONTINUE
         IF(.NOT.NORM) GO TO 160
         N1= 3
         DO 145 J=1,NJ
            N2= N1+6
            DO 142 NN=N1+1,N2
               DO 141 M=1,6
  141          GPP(M,NN)= GPP(M,NN)*SR5
  142       CONTINUE
            NN= N2+1
C           DO 144 NN=N2+1,N2+1
               DO 143 M=1,6
  143          GPP(M,NN)= GPP(M,NN)*S35
C 144       CONTINUE
  145    N1= N1+NI
         GO TO 160
      END IF
C G''
      IF(MINI.LE.21) THEN
         DO 150 J=1,NJ
            NN= NN+1
            GPP(1,NN)= GP2(J)      +G(J)      *RNINE+GM2(J)      *TWELVE
            GPP(2,NN)= GP2(J+NJ* 3)+G(J+NJ* 3)*FOR
            GPP(3,NN)= GP2(J+NJ* 4)+G(J+NJ* 4)*FOR
            GPP(4,NN)= GP2(J+NJ* 9)+G(J)
            GPP(5,NN)= GP2(J+NJ*15)
            GPP(6,NN)= GP2(J+NJ*10)+G(J)
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*11)+G(J+NJ)
            GPP(2,NN)= GP2(J+NJ* 5)+G(J+NJ* 5)*FOR
            GPP(3,NN)= GP2(J+NJ*17)
            GPP(4,NN)= GP2(J+NJ)   +G(J+NJ)   *RNINE+GM2(J+NJ)   *TWELVE
            GPP(5,NN)= GP2(J+NJ* 6)+G(J+NJ* 6)*FOR
            GPP(6,NN)= GP2(J+NJ*12)+G(J+NJ)
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*13)+G(J+NJ* 2)
            GPP(2,NN)= GP2(J+NJ*17)
            GPP(3,NN)= GP2(J+NJ* 7)+G(J+NJ* 7)*FOR
            GPP(4,NN)= GP2(J+NJ*14)+G(J+NJ* 2)
            GPP(5,NN)= GP2(J+NJ* 8)+G(J+NJ* 8)*FOR
            GPP(6,NN)= GP2(J+NJ* 2)+G(J+NJ* 2)*RNINE+GM2(J+NJ* 2)*TWELVE
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ* 3)+G(J+NJ* 3)*SEV+GM2(J+NJ* 3)*SIX
            GPP(2,NN)= GP2(J+NJ* 9)+G(J)      +G(J+NJ* 9)*THR+
     *                 GM2(J)      *THR
            GPP(3,NN)= GP2(J+NJ*15)+G(J+NJ*12)*THR
            GPP(4,NN)= GP2(J+NJ*18)+G(J+NJ* 3)*THR
            GPP(5,NN)= GP2(J+NJ*21)+G(J+NJ* 4)
            GPP(6,NN)= GP2(J+NJ*22)+G(J+NJ* 3)
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ* 4)+G(J+NJ* 4)*SEV+GM2(J+NJ* 4)*SIX
            GPP(2,NN)= GP2(J+NJ*15)+G(J+NJ*12)*THR
            GPP(3,NN)= GP2(J+NJ*10)+G(J)      +G(J+NJ*10)*THR+
     *                 GM2(J)      *THR
            GPP(4,NN)= GP2(J+NJ*21)+G(J+NJ* 4)
            GPP(5,NN)= GP2(J+NJ*22)+G(J+NJ* 3)
            GPP(6,NN)= GP2(J+NJ*19)+G(J+NJ* 4)*THR
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*18)+G(J+NJ* 5)*THR
            GPP(2,NN)= GP2(J+NJ*11)+G(J+NJ)   +G(J+NJ* 9)*THR+
     *                 GM2(J+NJ)   *THR
            GPP(3,NN)= GP2(J+NJ*23)+G(J+NJ* 6)
            GPP(4,NN)= GP2(J+NJ* 5)+G(J+NJ* 5)*SEV+GM2(J+NJ* 3)*SIX
            GPP(5,NN)= GP2(J+NJ*16)+G(J+NJ*13)*THR
            GPP(6,NN)= GP2(J+NJ*24)+G(J+NJ* 5)
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*23)+G(J+NJ* 6)
            GPP(2,NN)= GP2(J+NJ*16)+G(J+NJ*13)*THR
            GPP(3,NN)= GP2(J+NJ*24)+G(J+NJ* 5)
            GPP(4,NN)= GP2(J+NJ* 8)+G(J+NJ* 6)*SEV+GM2(J+NJ* 5)*SIX
            GPP(5,NN)= GP2(J+NJ*12)+G(J+NJ)   +G(J+NJ*11)*THR+
     *                 GM2(J+NJ)   *THR
            GPP(6,NN)= GP2(J+NJ*20)+G(J+NJ* 6)*THR
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*19)+G(J+NJ* 7)*THR
            GPP(2,NN)= GP2(J+NJ*25)+G(J+NJ* 8)
            GPP(3,NN)= GP2(J+NJ*13)+G(J+NJ* 2)+G(J+NJ*10)*THR+
     *                 GM2(J+NJ* 2)*THR
            GPP(4,NN)= GP2(J+NJ*26)+G(J+NJ* 7)
            GPP(5,NN)= GP2(J+NJ*17)+G(J+NJ*14)*THR
            GPP(6,NN)= GP2(J+NJ* 7)+G(J+NJ* 7)*SEV+GM2(J+NJ* 4)*SIX
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*25)+G(J+NJ* 8)
            GPP(2,NN)= GP2(J+NJ*26)+G(J+NJ* 7)
            GPP(3,NN)= GP2(J+NJ*17)+G(J+NJ*14)*THR
            GPP(4,NN)= GP2(J+NJ*20)+G(J+NJ* 8)*THR
            GPP(5,NN)= GP2(J+NJ*14)+G(J+NJ* 2)+G(J+NJ*11)*THR+
     *                 GM2(J+NJ* 2)*THR
            GPP(6,NN)= GP2(J+NJ* 8)+G(J+NJ* 8)*SEV+GM2(J+NJ* 5)*SIX
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ* 9)+G(J+NJ* 9)*FIV+GM2(J+NJ)   *TWO
            GPP(2,NN)= GP2(J+NJ*18)+(G(J+NJ* 3)+G(J+NJ* 5))*TWO+
     *                 GM2(J+NJ* 3)*FOR
            GPP(3,NN)= GP2(J+NJ*21)+G(J+NJ*13)*TWO
            GPP(4,NN)= GP2(J+NJ*11)+G(J+NJ* 9)*FIV+GM2(J)*TWO
            GPP(5,NN)= GP2(J+NJ*23)+G(J+NJ*12)*TWO
            GPP(6,NN)= GP2(J+NJ*27)+G(J+NJ* 9)
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*10)+G(J+NJ*10)*FIV+GM2(J+NJ* 2)*TWO
            GPP(2,NN)= GP2(J+NJ*22)+G(J+NJ*14)*TWO
            GPP(3,NN)= GP2(J+NJ*19)+(G(J+NJ* 4)+G(J+NJ* 7))*TWO+
     *                 GM2(J+NJ* 4)*FOR
            GPP(4,NN)= GP2(J+NJ*27)+G(J+NJ*10)
            GPP(5,NN)= GP2(J+NJ*25)+G(J+NJ*12)*TWO
            GPP(6,NN)= GP2(J+NJ*13)+G(J+NJ*10)*FIV+GM2(J)*TWO
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*27)+G(J+NJ*11)
            GPP(2,NN)= GP2(J+NJ*24)+G(J+NJ*14)*TWO
            GPP(3,NN)= GP2(J+NJ*26)+G(J+NJ*13)*TWO
            GPP(4,NN)= GP2(J+NJ*12)+G(J+NJ*11)*FIV+GM2(J+NJ* 2)*TWO
            GPP(5,NN)= GP2(J+NJ*20)+(G(J+NJ* 6)+G(J+NJ* 8))*TWO+
     *                 GM2(J+NJ* 5)*FOR
            GPP(6,NN)= GP2(J+NJ*14)+G(J+NJ*11)*FIV+GM2(J+NJ)*TWO
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*15)+G(J+NJ*12)*FIV+GM2(J+NJ* 5)*TWO
            GPP(2,NN)= GP2(J+NJ*21)+G(J+NJ*13)*TWO+G(J+NJ* 4)+
     *                 GM2(J+NJ* 4)*TWO
            GPP(3,NN)= GP2(J+NJ*22)+G(J+NJ*14)*TWO+G(J+NJ* 3)+
     *                 GM2(J+NJ* 3)*TWO
            GPP(4,NN)= GP2(J+NJ*23)+G(J+NJ*12)*THR
            GPP(5,NN)= GP2(J+NJ*27)+G(J+NJ*10)+G(J+NJ* 9)+GM2(J)
            GPP(6,NN)= GP2(J+NJ*25)+G(J+NJ*12)*THR
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*21)+G(J+NJ*13)*THR
            GPP(2,NN)= GP2(J+NJ*23)+G(J+NJ*12)*TWO+G(J+NJ* 6)+
     *                 GM2(J+NJ* 5)*TWO
            GPP(3,NN)= GP2(J+NJ*27)+G(J+NJ*11)+G(J+NJ* 9)+GM2(J+NJ)
            GPP(4,NN)= GP2(J+NJ*16)+G(J+NJ*13)*FIV+GM2(J+NJ* 4)*TWO
            GPP(5,NN)= GP2(J+NJ*24)+G(J+NJ*14)*TWO+G(J+NJ* 5)+
     *                 GM2(J+NJ* 3)*TWO
            GPP(6,NN)= GP2(J+NJ*26)+G(J+NJ*13)*THR
            NN= NN+1
            GPP(1,NN)= GP2(J+NJ*22)+G(J+NJ*14)*THR
            GPP(2,NN)= GP2(J+NJ*27)+G(J+NJ*11)+G(J+NJ*10)+GM2(J+NJ* 2)
            GPP(3,NN)= GP2(J+NJ*25)+G(J+NJ*12)*TWO+G(J+NJ* 8)+
     *                 GM2(J+NJ* 5)*TWO
            GPP(4,NN)= GP2(J+NJ*24)+G(J+NJ*14)*THR
            GPP(5,NN)= GP2(J+NJ*26)+G(J+NJ*13)*TWO+G(J+NJ* 7)+
     *                 GM2(J+NJ* 4)*TWO
            GPP(6,NN)= GP2(J+NJ*17)+G(J+NJ*14)*FIV+GM2(J+NJ* 3)*TWO
  150    CONTINUE
         IF(.NOT.NORM) GO TO 160
         N1= 3
         DO 157 J=1,NJ
            N2= N1+6
            N3= N2+3
            DO 152 NN=N1+1,N2
               DO 151 M=1,6
  151          GPP(M,NN)= GPP(M,NN)*SR7
  152       CONTINUE
            DO 154 NN=N2+1,N3
               DO 153 M=1,6
  153          GPP(M,NN)= GPP(M,NN)*S53
  154       CONTINUE
            DO 156 NN=N3+1,N3+3
               DO 155 M=1,6
  155          GPP(M,NN)= GPP(M,NN)*S57
  156       CONTINUE
  157    N1= N1+NI
         GO TO 160
      ELSE
         WRITE(6,*) 'IN FORMIJ H ATTEMPT',MINI,MAXI,MINJ,MAXJ
         CALL ABRT
      END IF
      GO TO 999
  160 CONTINUE
      DO 170 NN=1,NJ*NI
         GPP(9,NN)= GPP(6,NN)
         GPP(8,NN)= GPP(5,NN)
         GPP(7,NN)= GPP(3,NN)
         GPP(6,NN)= GPP(5,NN)
         GPP(5,NN)= GPP(4,NN)
         GPP(4,NN)= GPP(2,NN)
  170 CONTINUE
C
  999 CONTINUE
      RETURN
      END
