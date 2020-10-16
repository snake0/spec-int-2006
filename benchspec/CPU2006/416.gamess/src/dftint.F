C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C 26 OCT 00 - MWS - INTRODUCE MXAO PARAMETER
C 25 MAR 00 - KRG - NEW MODULE FOR GRIDLESS DFT INTEGRAL EVALUATION
C
C*MODULE DFTINT  *DECK DFTINT
      SUBROUTINE DFTINT
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL DIRSCF,FDIFF,PACK2E,SOME,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      COMMON /DFTPAR/ DFTTYP(20),EXENA,EXENB,EXENC,IDFT34,NAUXFUN,
     *                                                    NAUXSHL
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OPTSCF/ DIRSCF,FDIFF
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK  /8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
C
      SOME = MASWRK  .AND.  NPRINT.NE.-5
      IF(SOME) WRITE(IW,9000)
      L1 = NUM
      L2 = (NUM*(NUM+1))/2
C
      IF(IDFT34.NE.3  .AND.  IDFT34.NE.4) THEN
         WRITE(IW,*) ' IDFT34 CONFUSED IN DFTINT'
         CALL ABRT
      END IF
C
C     -----DO THREE CENTER INTEGRALS FOR DFT
C
      IF(IDFT34.EQ.3) THEN
         IF(.NOT. DIRSCF) THEN
            CALL VALFM(LOADFM)
            ITMP=LOADFM+1
            ISS=ITMP+15*15*15
            ILOC=ISS+NINTMX
            LAST=ILOC+NINTMX
            NEED = LAST - LOADFM - 1
            CALL GETFM(NEED)
            IF(EXETYP.NE.CHECK) THEN
               CALL THREEC(SOME,X(ISS),X(ILOC),X(ITMP),NINTMX)
            END IF
            CALL RETFM(NEED)
         END IF
      END IF
C
C     -----DO FOUR CENTER INTEGRALS FOR DFT
C
      IF(IDFT34 .EQ. 4) THEN
         IF(.NOT. DIRSCF) THEN
            CALL VALFM(LOADFM)
            ISCRN = LOADFM + 1
            ITMP  = ISCRN  + (NSHELL*NSHELL+NSHELL)/2
            ISS   = ITMP   + 15*15*15*15
            ILOC  = ISS    + NINTMX
            LAST  = ILOC   + NINTMX
            NEED  = LAST - LOADFM - 1
            CALL GETFM(NEED)
            IF(EXETYP.NE.CHECK) THEN
               CALL FORSCR(SOME,X(ISCRN),X(ITMP))
               CALL FOURC(SOME,X(ISCRN),X(ISS),X(ILOC),X(ITMP),NINTMX)
            END IF
            CALL RETFM(NEED)
         ELSE
            CALL VALFM(LOADFM)
            ISCRN=LOADFM+1
            ITMP=ISCRN+(NSHELL*NSHELL+NSHELL)/2
            LAST=ITMP+15*15*15*15
            NEED = LAST - LOADFM - 1
            CALL GETFM(NEED)
            IF(EXETYP.NE.CHECK) CALL FORSCR(SOME,X(ISCRN),X(ITMP))
            CALL RETFM(NEED)
         END IF
      END IF
C
C     -----DO AUXILIARY FUNCITON INTEGRALS IF WANTED
C          THIS WILL INCLUDE GRADIENT INTEGRALS
C
      IF(DFTTYP(20) .NE. 0.0D+00) THEN
         CALL DFTAXDRV(SOME)
         GO TO 800
      END IF
C
C     ----- DO ELECTRON GRADIENT INTEGRALS FOR NON-LOCAL DFT
C     ----- AND ALSO FOR THE CASE OF DFT NUCLEAR GRADIENTS
C
      CALL DERCHK(MAXDER)
      IF ((DFTTYP(1).GE.2.0D+00)  .OR.
     *    (DFTTYP(1).GE.1.0D+00  .AND.  MAXDER.GT.0)) THEN
         CALL TSECND(TIM0)
         CALL VALFM(LOADFM)
         LDDX = LOADFM + 1
         LDDY = LDDX   + L2
         LDDZ = LDDY   + L2
         LAST = LDDZ   + L2
         NEED = LAST - LOADFM - 1
         CALL GETFM(NEED)
         IF (EXETYP .NE. CHECK) THEN
            CALL DIPVEL(EXETYP,X(LDDX),X(LDDY),X(LDDZ),L1,L2)
            CALL DAWRIT (IDAF,IODA,X(LDDX),L2,84,0)
            CALL DAWRIT (IDAF,IODA,X(LDDY),L2,85,0)
            CALL DAWRIT (IDAF,IODA,X(LDDZ),L2,86,0)
         END IF
         CALL RETFM(NEED)
         CALL TSECND(TIM)
         TIM = TIM-TIM0
         IF(SOME) WRITE(IW,9030) TIM
      END IF
C
  800 CONTINUE
      IF(MASWRK) WRITE(IW,9050)
      CALL TIMIT(1)
      RETURN
C
 9000 FORMAT(/10X,23("-")/10X,'DFT INTEGRAL EVALUATION'/10X,23("-"))
 9030 FORMAT(1X,'TIME TO EVALUATE GRADIENT CORRECTION INTEGRALS=',F12.1)
 9050 FORMAT(1X,'..... END OF DFT INTEGRALS.....')
      END
C*MODULE DFTINT  *DECK FOURC
      SUBROUTINE FOURC(SOME,SMAX,S,IX,TEMPS,NINTMX)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SAMECEN,SVDSKW,SOME,DSKWRK,MASWRK,GOPARR
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500, MXAO=2047)
C
      COMMON /IJPAIR/ INDEX2(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCKLAB/ LABSIZ
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                T(432),INVT(48),NT
      COMMON /RESTAR/ TIMLIM,IREST,NREC,INTLOC,IST,JST,KST,LST
C
      DIMENSION S(NINTMX),SMAX(NSHELL*(NSHELL+1)/2),IX(*),
     *          TEMPS(15*15*15*15)
      DIMENSION DFAC(0:21),LDAT(35),MDAT(35),NDAT(35),
     *          CONTA(35),CONTB(35),CONTC(35),CONTE(35),
     *          A21(0:4*4*4*4),IDUMMY(0:4),MI(48),MJ(48),MK(48),
     *          XXXX(-1:4,-1:4,-1:4,-1:4),
     *          YYYY(-1:4,-1:4,-1:4,-1:4),
     *          ZZZZ(-1:4,-1:4,-1:4,-1:4)
C
      PARAMETER (PI=3.141592653589793238462643D+00, RLN10=2.30258D+00)
C
C  X,Y,Z EXPONENTS OF CARTESIAN S,PX,PY,PZ,DXX,...
C
      DATA LDAT/0,1,0,0,2,0,0,1,1,0,3,0,0,2,2,1,0,1,0,1,4,0,0,3,3,
     *          1,0,1,0,2,2,0,2,1,1/
      DATA MDAT/0,0,1,0,0,2,0,1,0,1,0,3,0,1,0,2,2,0,1,1,0,4,0,1,0,
     *          3,3,0,1,2,0,2,1,2,1/
      DATA NDAT/0,0,0,1,0,0,2,0,1,1,0,0,3,0,1,0,1,2,2,1,0,0,4,0,1,
     *          0,1,3,3,0,2,2,1,1,2/
C
C  CALCULATES IVALUE!!
C     WARNING DFAC(0)=-1!!,DFAC(1)=0!!,DFAC(2)=1!! ETC.
C
      DATA DFAC/1.0D+00,1.0D+00,1.0D+00,2.0D+00,3.0D+00,8.0D+00,
     *          15.0D+00,48.0D+00,105.0D+00,384.0D+00,945.0D+00,
     *          3840.0D+00,10395.0D+00,46080.0D+00,135135.0D+00,
     *          645120.0D+00,2027025.0D+00,10321920.0D+00,
     *          34459425.0D+00,185794560.0D+00,
     *          654729075.0D+00,3715891200.0D+00/
C
C     HELPS DEAL WITH L SHELLS (SHELL THAT ENDS ON 1 MIGHT START WITH 0)
C
      DATA IDUMMY/0,0,2,3,4/
C
C   THIS ROUTINE CALCULATES THE ONE ELECTRON INTEGRAL
C   <ABCE> FOR CARTESIAN GAUSSIANS.  (UP TO G-SHELLS)
C
      IF ((NORMF .EQ. 1 .OR. NORMP .EQ. 1).AND. MASWRK) THEN
        WRITE(IW,*)' SORRY BUT THE FOUR CENTER ONE ELECTRON OVELAP'
        WRITE(IW,*)' INTEGRALS USED FOR DFT DO NOT SUPPORT THE'
        WRITE(IW,*)' NO NORMALIZATION OPTIONS'
        CALL ABRT
      END IF
C
C     ----- INITIALIZATION FOR PARALLEL WORK -----
C
      IPCOUNT = ME - 1
      SVDSKW = DSKWRK
      DSKWRK = .TRUE.
C
      IF(SOME) WRITE(IW,9000)
C
      CALL TSECND(TIM0)
C
      CUTOFF = 1.0D+00/(10.0D+00**ICUT)
      TOL=ITOL*RLN10
      IWHERE=0
      CALL VCLR(XXXX,1,6*6*6*6)
      CALL VCLR(YYYY,1,6*6*6*6)
      CALL VCLR(ZZZZ,1,6*6*6*6)
      XXXX(0,0,0,0)=1.0D+00
      YYYY(0,0,0,0)=1.0D+00
      ZZZZ(0,0,0,0)=1.0D+00
C
      NSCHWZ=0
      NREC=0
      NINTS=0
      MAXAO=255
      LABSIZ = 1
      IF(NUM.GT.MAXAO) LABSIZ = 2
      CALL SEQOPN(21,'DFTINTS','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQREW(21)
C
C    LOOP OVER SHELLS A B C AND E FOR <AB|CE>
C
      DO 100 IASHL=1,NSHELL
C
C     ----- CHECK CPU TIME -----
C
      CALL TSECND(TIM)
      IF(TIM.GE.TIMLIM) THEN
         WRITE(IW,*) 'NO TIME LEFT IN -FOURC-'
         RETURN
      END IF
C
C  TRY TO ELIMINATE A SHELL
C
      DO 1020 IT = 1,NT
         ID = MAPSHL(IASHL,IT)
         IF (ID .GT. IASHL) GO TO 100
         MI(IT) = ID
 1020 CONTINUE
C
      KASHL=INDEX2(IASHL)
C  POSITION OF THE SHELL'S ATOM
      IAATOM=KATOM(IASHL)
      AX=C(1,IAATOM)
      AY=C(2,IAATOM)
      AZ=C(3,IAATOM)
C  INFO ABOUT THE PRIMATIVES
      ISTARTA=KSTART(IASHL)
      IENDA=ISTARTA+KNG(IASHL)-1
C  INFO ABOUT THE SUBSHELLS
      IAMAX=KMAX(IASHL)
      IAMIN=KMIN(IASHL)
      LOCA =KLOC(IASHL)-IAMIN
C
C  FIND THE HIGHEST SPIN VALUE IN THIS SHELL
      KATYPE=KTYPE(IASHL)-1
      JATYPE=1+IAMAX-IAMIN
C
C   B SHELL
C
      DO 101 IBSHL=1,IASHL
C
      DO 200 IT = 1,NT
      ID = MI(IT)
      JD = MAPSHL(IBSHL,IT)
      MJ(IT) = JD
      IF (ID .GE. JD) GO TO 1060
      ND = ID
      ID = JD
      JD = ND
 1060 IF (ID-IASHL) 200,1080,101
 1080 IF (JD-IBSHL) 200,200,101
  200 CONTINUE
C
      KBSHL=INDEX2(IBSHL)
      IBATOM=KATOM(IBSHL)
      BX=C(1,IBATOM)
      BY=C(2,IBATOM)
      BZ=C(3,IBATOM)
      ISTARTB=KSTART(IBSHL)
      IENDB=ISTARTB+KNG(IBSHL)-1
      IBMAX=KMAX(IBSHL)
      IBMIN=KMIN(IBSHL)
      LOCB = KLOC(IBSHL)-IBMIN
      KBTYPE=KTYPE(IBSHL)-1
      JBTYPE=1+IBMAX-IBMIN
      ABRR=(AX-BX)*(AX-BX)+(AY-BY)*(AY-BY)+(AZ-BZ)*(AZ-BZ)
C
C    C SHELL
C
      DO 102 ICSHL=1,IBSHL
      DO 340 IT = 1,NT
      ID = MI(IT)
      JD = MJ(IT)
      KD = MAPSHL(ICSHL,IT)
      MK(IT) = KD
  240 IF (ID .GE. JD) GO TO 260
      ND = ID
      ID = JD
      JD = ND
  260 IF (JD .GE. KD) GO TO 280
      ND = JD
      JD = KD
      KD = ND
      GO TO 240
  280 IF (ID-IASHL) 340,300,102
  300 IF (JD-IBSHL) 340,320,102
  320 IF (KD-ICSHL) 340,340,102
  340 CONTINUE
      KCSHL=INDEX2(ICSHL)
      ICATOM=KATOM(ICSHL)
      CX=C(1,ICATOM)
      CY=C(2,ICATOM)
      CZ=C(3,ICATOM)
      ISTARTC=KSTART(ICSHL)
      IENDC=ISTARTC+KNG(ICSHL)-1
      ICMAX=KMAX(ICSHL)
      ICMIN=KMIN(ICSHL)
      LOCC = KLOC(ICSHL)-ICMIN
      KCTYPE=KTYPE(ICSHL)-1
      JCTYPE=1+ICMAX-ICMIN
      ACRR=(AX-CX)*(AX-CX)+(AY-CY)*(AY-CY)+(AZ-CZ)*(AZ-CZ)
      BCRR=(BX-CX)*(BX-CX)+(BY-CY)*(BY-CY)+(BZ-CZ)*(BZ-CZ)
C
C    E SHELL
C
      DO 103 IESHL=1,ICSHL
      N4 = 0
      DO 540 IT = 1,NT
      ID = MI(IT)
      JD = MJ(IT)
      KD = MK(IT)
      LD = MAPSHL(IESHL,IT)
  380 IF (ID .GE. JD) GO TO 400
      ND = ID
      ID = JD
      JD = ND
  400 IF (JD .GE. KD) GO TO 420
      ND = JD
      JD = KD
      KD = ND
      GO TO 380
  420 IF (KD .GE. LD) GO TO 440
      ND = KD
      KD = LD
      LD = ND
      GO TO 400
  440 IF (ID-IASHL) 540,460,103
  460 IF (JD-IBSHL) 540,480,103
  480 IF (KD-ICSHL) 540,500,103
  500 IF (LD-IESHL) 540,520,103
  520 N4 = N4+1
  540 CONTINUE
      Q4=NT
      Q4=Q4/N4
C
C     ----- GO PARALLEL! -----
C
      IF (GOPARR) THEN
         IPCOUNT = IPCOUNT + 1
         IF (MOD(IPCOUNT,NPROC).NE.0) GO TO 103
      END IF
C
      IEATOM=KATOM(IESHL)
      EXX=C(1,IEATOM)
      EY=C(2,IEATOM)
      EZ=C(3,IEATOM)
      ISTARTE=KSTART(IESHL)
      IENDE=ISTARTE+KNG(IESHL)-1
      IEMAX=KMAX(IESHL)
      IEMIN=KMIN(IESHL)
      LOCE = KLOC(IESHL)-IEMIN
      KETYPE=KTYPE(IESHL)-1
      JETYPE=1+IEMAX-IEMIN
      AERR=(AX-EXX)*(AX-EXX)+(AY-EY)*(AY-EY)+(AZ-EZ)*(AZ-EZ)
      BERR=(BX-EXX)*(BX-EXX)+(BY-EY)*(BY-EY)+(BZ-EZ)*(BZ-EZ)
      CERR=(CX-EXX)*(CX-EXX)+(CY-EY)*(CY-EY)+(CZ-EZ)*(CZ-EZ)
C
C END OF SHELLS
C
C
C PRE-SCREEN THE THE INTEGRALS
C
      IF(SMAX(KASHL+IBSHL)*SMAX(KCSHL+IESHL) .LE. 1.0D-18 .OR.
     *   SMAX(KASHL+ICSHL)*SMAX(KBSHL+IESHL) .LE. 1.0D-18 .OR.
     *   SMAX(KASHL+IESHL)*SMAX(KBSHL+ICSHL) .LE. 1.0D-18) THEN
      NSCHWZ=NSCHWZ+1
      GO TO 103
      END IF
C
C    CHECK TO SEE IF THEY ARE ALL ON THE SAME CENTER
C
      IF(IAATOM .EQ. IBATOM .AND. IAATOM .EQ. ICATOM .AND. IAATOM
     *    .EQ. IEATOM) THEN
        IF(  MOD(KATYPE+KBTYPE+KCTYPE+KETYPE,2).EQ.1 .AND.
     *       .NOT.(IAMIN.EQ.1 .AND. IAMAX.EQ.4).AND.
     *       .NOT.(IBMIN.EQ.1 .AND. IBMAX.EQ.4).AND.
     *       .NOT.(ICMIN.EQ.1 .AND. ICMAX.EQ.4).AND.
     *       .NOT.(IEMIN.EQ.1 .AND. IEMAX.EQ.4)) GO TO 103
C            NO L SHELLS TO CONFUSE THINGS, AND TOTAL
C            MOMENTUM IS ODD, THUS INTEGRALS ARE ZERO
        SAMECEN=.TRUE.
      ELSE
        SAMECEN=.FALSE.
      END IF
C
C    LOOP OVER GUASSIAN PRIMATIVES WHILE READING
C          IN EXPONENTS AND CONTRACTIONS
C
C     DO SOMETHING SPECIAL FOR (SSSS) CASE
C
       IF(KATYPE.EQ.0 .AND. KBTYPE.EQ.0 .AND. KCTYPE.EQ.0 .AND.
     *                                        KETYPE.EQ.0) THEN
       TEMPS(1)=0.0D+00
       IF (SAMECEN) THEN
       DO 910 IAPRM=ISTARTA,IENDA
        A1=EX(IAPRM)
        SCONTA=CS(IAPRM)
        DO 911 IBPRM=ISTARTB,IENDB
         A2=EX(IBPRM)+A1
         SCONTB=CS(IBPRM)*SCONTA
         DO 912 ICPRM=ISTARTC,IENDC
          A3=EX(ICPRM)+A2
          SCONTC=CS(ICPRM)*SCONTB
         DO 913 IEPRM=ISTARTE,IENDE
          A5=EX(IEPRM)+A3
          SCONTE=CS(IEPRM)*SCONTC
C
            TEMPS(1)=TEMPS(1)+SCONTE/(A5*SQRT(A5))
C
  913    CONTINUE
  912    CONTINUE
  911   CONTINUE
  910  CONTINUE
       TEMPS(1)=TEMPS(1)*PI*SQRT(PI)
      ELSE
       DO 810 IAPRM=ISTARTA,IENDA
        A1=EX(IAPRM)
        SCONTA=CS(IAPRM)
        DO 811 IBPRM=ISTARTB,IENDB
         A2=EX(IBPRM)
         IF(ABRR*A1*A2/(A1+A2) .GT. TOL) GO TO 811
         SCONTB=CS(IBPRM)*SCONTA
C
C        GET INFO ABOUT A,B,AND P
C
            A6=A1+A2
            PX=((A1*AX+A2*BX)/A6)
            PY=((A1*AY+A2*BY)/A6)
            PZ=((A1*AZ+A2*BZ)/A6)
            ABK=-((A1*A2/A6)*
     *          ((AX-BX)*(AX-BX)+(AY-BY)*(AY-BY)+(AZ-BZ)*(AZ-BZ)))
C
C
         DO 812 ICPRM=ISTARTC,IENDC
          A3=EX(ICPRM)
          IF(ACRR*A1*A3/(A1+A3) .GT. TOL .OR.
     *       BCRR*A2*A3/(A2+A3) .GT. TOL) GO TO 812
          SCONTC=CS(ICPRM)*SCONTB
         DO 813 IEPRM=ISTARTE,IENDE
          A5=EX(IEPRM)
          IF(AERR*A1*A5/(A1+A5) .GT. TOL .OR.
     *       BERR*A2*A5/(A2+A5) .GT. TOL .OR.
     *       CERR*A3*A5/(A3+A5) .GT. TOL) GO TO 813
          SCONTE=CS(IEPRM)*SCONTC
C
          A4=A1+A2+A3+A5
          A4INV=1.0D+00/A4
C
            A7=A3+A5
            A7INV=1.0D+00/A7
            QX=(A3*CX+A5*EXX)*A7INV
            QY=(A3*CY+A5*EY)*A7INV
            QZ=(A3*CZ+A5*EZ)*A7INV
C
C   CALCULATE
C
            TEMPS(1)=TEMPS(1)+SCONTE*(((PI*A4INV)*SQRT(PI*A4INV))*
     *                EXP(ABK
     *                                 -((A3*A5*A7INV)*
     *          ((CX-EXX)*(CX-EXX)+(CY-EY)*(CY-EY)+(CZ-EZ)*(CZ-EZ)))
     *                                 -((A6*A7*A4INV)*
     *          ((PX-QX)*(PX-QX)+(PY-QY)*(PY-QY)+(PZ-QZ)*(PZ-QZ)))))
C
  813    CONTINUE
  812    CONTINUE
  811   CONTINUE
  810  CONTINUE
      END IF
C
C      END SPECIAL (SSSS) CASE
C
       ELSE
C
       CALL VCLR(TEMPS,1,JATYPE*JBTYPE*JCTYPE*JETYPE)
       DO 110 IAPRM=ISTARTA,IENDA
        A1=EX(IAPRM)
        CALL FORCNT(IAPRM,IAMAX,CONTA)
        DO 111 IBPRM=ISTARTB,IENDB
         A2=EX(IBPRM)
         CALL FORCNT(IBPRM,IBMAX,CONTB)
C
C        GET INFO ABOUT A,B,AND P
C
          IF(.NOT. SAMECEN) THEN
C
            A6=A1+A2
            PX=((A1*AX+A2*BX)/A6)
            PY=((A1*AY+A2*BY)/A6)
            PZ=((A1*AZ+A2*BZ)/A6)
            ABK=(A1*A2/A6)*ABRR
            IF (ABK .GT. TOL) GO TO 111
          END IF
C
C
         DO 112 ICPRM=ISTARTC,IENDC
          A3=EX(ICPRM)
          CALL FORCNT(ICPRM,ICMAX,CONTC)
          IF(ACRR*A1*A3/(A1+A3) .GT. TOL .OR.
     *       BCRR*A2*A3/(A2+A3) .GT. TOL) GO TO 112
         DO 113 IEPRM=ISTARTE,IENDE
          A5=EX(IEPRM)
          CALL FORCNT(IEPRM,IEMAX,CONTE)
C
C   CALCULATE FINAL GAUSSIAN CENTER AND OTHER THINGS
C   P=A AND B, Q=C AND E, D=FINAL CENTER
C
          A4=A1+A2+A3+A5
          A4INV=1.0D+00/A4
C
          IF(.NOT. SAMECEN) THEN
            IF(AERR*A1*A5/(A1+A5) .GT. TOL .OR.
     *         BERR*A2*A5/(A2+A5) .GT. TOL .OR.
     *         CERR*A3*A5/(A3+A5) .GT. TOL) GO TO 113
C
            A7=A3+A5
            A7INV=1.0D+00/A7
            A10=0.5D+00*A4INV
C
            QX=(A3*CX+A5*EXX)*A7INV
            QY=(A3*CY+A5*EY)*A7INV
            QZ=(A3*CZ+A5*EZ)*A7INV
            DX=(A1*AX+A2*BX+A3*CX+A5*EXX)*A4INV
            DY=(A1*AY+A2*BY+A3*CY+A5*EY)*A4INV
            DZ=(A1*AZ+A2*BZ+A3*CZ+A5*EZ)*A4INV
            DAX=DX-AX
            DAY=DY-AY
            DAZ=DZ-AZ
            DBX=DX-BX
            DBY=DY-BY
            DBZ=DZ-BZ
            DCX=DX-CX
            DCY=DY-CY
            DCZ=DZ-CZ
            DEX=DX-EXX
            DEY=DY-EY
            DEZ=DZ-EZ
            PQRR=(PX-QX)*(PX-QX)+(PY-QY)*(PY-QY)+(PZ-QZ)*(PZ-QZ)
C
C   CALCULATE KABC
C
            ABCK=((PI*A4INV)*SQRT(PI*A4INV))*EXP(-ABK
     *                                 -((A3*A5*A7INV)*CERR)
     *                                 -((A6*A7*A4INV)*PQRR))
C
C
C   USE RECURSION TO GENERATE HIGHER ANGULAR MOMENTUM XXXX,ETC
C
C     RECURSION BASED ON J
      DO 2000 J=1,KETYPE
                  XXXX(J,0,0,0)= DEX*XXXX(J-1,0  ,0  ,0  )+A10*(
     *                         (J-1)*XXXX(J-2,0  ,0  ,0  ))
                  YYYY(J,0,0,0)= DEY*YYYY(J-1,0  ,0  ,0  )+A10*(
     *                         (J-1)*YYYY(J-2,0  ,0  ,0  ))
                  ZZZZ(J,0,0,0)= DEZ*ZZZZ(J-1,0  ,0  ,0  )+A10*(
     *                         (J-1)*ZZZZ(J-2,0  ,0  ,0  ))
 2000 CONTINUE
C      RECURSION BASED ON K
       DO 2010 K=1,KCTYPE
        DO 2010 J=0,KETYPE
                  XXXX(J,K,0,0)= DCX*XXXX(J  ,K-1,0  ,0  )+A10*(
     *                             J*XXXX(J-1,K-1,0  ,0  ) +
     *                         (K-1)*XXXX(J  ,K-2,0  ,0  ))
                  YYYY(J,K,0,0)= DCY*YYYY(J  ,K-1,0  ,0  )+A10*(
     *                             J*YYYY(J-1,K-1,0  ,0  ) +
     *                         (K-1)*YYYY(J  ,K-2,0  ,0  ))
                  ZZZZ(J,K,0,0)= DCZ*ZZZZ(J  ,K-1,0  ,0  )+A10*(
     *                             J*ZZZZ(J-1,K-1,0  ,0  ) +
     *                         (K-1)*ZZZZ(J  ,K-2,0  ,0  ))
 2010  CONTINUE
C       RECURSION BASED ON L
        DO 2020 L=1,KBTYPE
          DO 2020 K=0,KCTYPE
            DO 2020 J=0,KETYPE
                  XXXX(J,K,L,0)= DBX*XXXX(J  ,K  ,L-1,0  )+A10*(
     *                             J*XXXX(J-1,K  ,L-1,0  ) +
     *                             K*XXXX(J  ,K-1,L-1,0  ) +
     *                         (L-1)*XXXX(J  ,K  ,L-2,0  ))
                  YYYY(J,K,L,0)= DBY*YYYY(J  ,K  ,L-1,0  )+A10*(
     *                             J*YYYY(J-1,K  ,L-1,0  ) +
     *                             K*YYYY(J  ,K-1,L-1,0  ) +
     *                         (L-1)*YYYY(J  ,K  ,L-2,0  ))
                  ZZZZ(J,K,L,0)= DBZ*ZZZZ(J  ,K  ,L-1,0  )+A10*(
     *                             J*ZZZZ(J-1,K  ,L-1,0  ) +
     *                             K*ZZZZ(J  ,K-1,L-1,0  ) +
     *                         (L-1)*ZZZZ(J  ,K  ,L-2,0  ))
 2020   CONTINUE
C         RECURSION BASED ON I
          DO 2030 I=1,KATYPE
            DO 2030 L=0,KBTYPE
              DO 2030 K=0,KCTYPE
                DO 2030 J=0,KETYPE
                  XXXX(J,K,L,I)= DAX*XXXX(J  ,K  ,L  ,I-1)+A10*(
     *                         (I-1)*XXXX(J  ,K  ,L  ,I-2) +
     *                             J*XXXX(J-1,K  ,L  ,I-1) +
     *                             K*XXXX(J  ,K-1,L  ,I-1) +
     *                             L*XXXX(J  ,K  ,L-1,I-1))
                  YYYY(J,K,L,I)= DAY*YYYY(J  ,K  ,L  ,I-1)+A10*(
     *                         (I-1)*YYYY(J  ,K  ,L  ,I-2) +
     *                             J*YYYY(J-1,K  ,L  ,I-1) +
     *                             K*YYYY(J  ,K-1,L  ,I-1) +
     *                             L*YYYY(J  ,K  ,L-1,I-1))
                  ZZZZ(J,K,L,I)= DAZ*ZZZZ(J  ,K  ,L  ,I-1)+A10*(
     *                         (I-1)*ZZZZ(J  ,K  ,L  ,I-2) +
     *                             J*ZZZZ(J-1,K  ,L  ,I-1) +
     *                             K*ZZZZ(J  ,K-1,L  ,I-1) +
     *                             L*ZZZZ(J  ,K  ,L-1,I-1))
 2030     CONTINUE
C
C    LOOP OVER SUBSHELLS TO GET VALUES OF THE INTEGRALS
C
          IMOVE=0
          DO 120 IASUB=IAMIN,IAMAX
C          THE X COORDINATE IN S(NUM,NUM,NUM,NUM)
           IAPOSIT=LOCA+IASUB
           L1=LDAT(IASUB)
           M1=MDAT(IASUB)
           N1=NDAT(IASUB)
           CONTRA=ABCK*CONTA(IASUB)
C
           DO 121 IBSUB=IBMIN,IBMAX
C           THE Y COORDINATE IN S(NUM,NUM,NUM,NUM)
            IBPOSIT=LOCB+IBSUB
            IF(IBPOSIT .GT. IAPOSIT) GO TO 120
            L2=LDAT(IBSUB)
            M2=MDAT(IBSUB)
            N2=NDAT(IBSUB)
            CONTRB=CONTRA*CONTB(IBSUB)
C
            DO 122 ICSUB=ICMIN,ICMAX
C            THE Z COORDINATE IN S(NUM,NUM,NUM,NUM)
             ICPOSIT=LOCC+ICSUB
             IF(ICPOSIT .GT. IBPOSIT) GO TO 121
             L3=LDAT(ICSUB)
             M3=MDAT(ICSUB)
             N3=NDAT(ICSUB)
             CONTRC=CONTRB*CONTC(ICSUB)
C
            DO 123 IESUB=IEMIN,IEMAX
C            THE E COORDINATE IN S(NUM,NUM,NUM,NUM)
             IEPOSIT=LOCE+IESUB
             IF(IEPOSIT .GT. ICPOSIT) GO TO 122
             L5=LDAT(IESUB)
             M5=MDAT(IESUB)
             N5=NDAT(IESUB)
             CONTRE=CONTRC*CONTE(IESUB)
             IMOVE=IMOVE+1
C
              TEMPS(IMOVE) = TEMPS(IMOVE)
     *            +CONTRE*XXXX(L5,L3,L2,L1)*
     *               YYYY(M5,M3,M2,M1)*ZZZZ(N5,N3,N2,N1)
C
  123       CONTINUE
  122       CONTINUE
  121      CONTINUE
  120     CONTINUE
C
          ELSE
C
C         SPECIAL SAME CENTER CASE
C
          IMOVE=0
          RADIAL=(PI*A4INV)*SQRT(PI*A4INV)
          A20=SQRT(0.5D+00*A4INV)
          IDUM1=IDUMMY(KATYPE)+IDUMMY(KBTYPE)+IDUMMY(KCTYPE)+
     *         IDUMMY(KETYPE)
          IDUM2=KATYPE+KBTYPE+KCTYPE+KETYPE
          DO  I=IDUM1,IDUM2
            A21(I)=A20**I
          END DO
          DO 220 IASUB=IAMIN,IAMAX
C          THE X COORDINATE IN S(NUM,NUM,NUM,NUM)
           IAPOSIT=LOCA+IASUB
           L1=LDAT(IASUB)
           M1=MDAT(IASUB)
           N1=NDAT(IASUB)
           CONTRA=CONTA(IASUB)*RADIAL
C
           DO 221 IBSUB=IBMIN,IBMAX
C           THE Y COORDINATE IN S(NUM,NUM,NUM,NUM)
            IBPOSIT=LOCB+IBSUB
            IF(IBPOSIT .GT. IAPOSIT) GO TO 220
            L2=L1+LDAT(IBSUB)
            M2=M1+MDAT(IBSUB)
            N2=N1+NDAT(IBSUB)
            CONTRB=CONTRA*CONTB(IBSUB)
C
            DO 222 ICSUB=ICMIN,ICMAX
C            THE Z COORDINATE IN S(NUM,NUM,NUM,NUM)
             ICPOSIT=LOCC+ICSUB
             IF(ICPOSIT .GT. IBPOSIT) GO TO 221
             L3=L2+LDAT(ICSUB)
             M3=M2+MDAT(ICSUB)
             N3=N2+NDAT(ICSUB)
             CONTRC=CONTRB*CONTC(ICSUB)
C
            DO 223 IESUB=IEMIN,IEMAX
C            THE E COORDINATE IN S(NUM,NUM,NUM,NUM)
             IEPOSIT=LOCE+IESUB
             IF(IEPOSIT .GT. ICPOSIT) GO TO 222
             IMOVE=IMOVE+1
             L5=L3+LDAT(IESUB)
             IF(MOD(L5,2) .EQ. 1) GO TO 223
             M5=M3+MDAT(IESUB)
             IF(MOD(M5,2) .EQ. 1) GO TO 223
             N5=N3+NDAT(IESUB)
             IF(MOD(N5,2) .EQ. 1) GO TO 223
             CONTRE=CONTRC*CONTE(IESUB)
C
C          THE IF MOD(N5,2) GETS RID OF ODD EXPONENTS
C
            XYZI=DFAC(L5)*DFAC(M5)*DFAC(N5)*CONTRE*A21(L5+N5+M5)
            TEMPS(IMOVE)=TEMPS(IMOVE)+XYZI
C
  223       CONTINUE
  222       CONTINUE
  221      CONTINUE
  220     CONTINUE
C
C        END SPECIAL SAME CENTER CASE
C
          END IF
C
C
  113    CONTINUE
  112    CONTINUE
  111   CONTINUE
  110  CONTINUE
C      END THE SSSS IF
       END IF
C
C   LOOP OVER TEMPORARY MATRIX
C
         IMOVE=0
         DO 421 IAPOSIT=IAMIN+LOCA,IAMAX+LOCA
C          MIN LETS US SKIP (5611) SINCE WE DO (6511)
           DO 422 IBPOSIT=IBMIN+LOCB,MIN(IBMAX+LOCB,IAPOSIT)
C
            DO 423 ICPOSIT=ICMIN+LOCC,MIN(ICMAX+LOCC,IBPOSIT)
C
             DO 424 IEPOSIT=IEMIN+LOCE,MIN(IEMAX+LOCE,ICPOSIT)
              IMOVE=IMOVE+1
C             POSTSCREEN THE INTEGRALS
              IF(ABS(TEMPS(IMOVE)) .GT. CUTOFF) THEN
               IWHERE=IWHERE+1
               NPACK = IWHERE
               IPACK = IAPOSIT
               JPACK = IBPOSIT
               KPACK = ICPOSIT
               LPACK = IEPOSIT
               IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                 LABEL1 = ISHFT( IPACK, 16 ) + JPACK
                 LABEL2 = ISHFT( KPACK, 16 ) + LPACK
                 IX( 2*NPACK-1 ) = LABEL1
                 IX( 2*NPACK   ) = LABEL2
#endif
#if defined(SPEC_CPU_ILP64)
                 LABEL = ISHFT( IPACK, 48 ) + ISHFT( JPACK, 32 ) +
     *                   ISHFT( KPACK, 16 ) + LPACK
                 IX(NPACK) = LABEL
#endif
               ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                 LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                   ISHFT( KPACK,  8 ) + LPACK
                 IX(NPACK) = LABEL
#endif
#if defined(SPEC_CPU_ILP64)
                 IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                   LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                     ISHFT( KPACK,  8 ) + LPACK
                   IX( NPACK/2 ) = IX( NPACK/2 ) + LABEL
                 ELSE
                   LABEL = ISHFT( IPACK, 56 ) + ISHFT( JPACK, 48 ) +
     *                     ISHFT( KPACK, 40 ) + ISHFT( LPACK, 32 )
                   IX( (NPACK/2)+1 ) = LABEL
                 END IF
#endif
               END IF
C             TAKE INTO ACCOUNT SKIPPED INTERGRALS AND STORE
              S(IWHERE)=TEMPS(IMOVE)*Q4
C
C                 WRITE OUT BUFFER OF INTEGRALS IF ITS FULL
C
              IF(IWHERE .EQ. NINTMX) THEN
                CALL PWRIT(21,S,IX,NINTMX,NINTMX)
                IWHERE=0
                NREC=NREC+1
                NINTS=NINTS+NINTMX
              END IF
C
            END IF
  424     CONTINUE
  423    CONTINUE
  422   CONTINUE
  421  CONTINUE
C
  103 CONTINUE
  102 CONTINUE
  101 CONTINUE
  100 CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
C     WRITE FINAL PARTIAL BUFFER LOAD OF INTEGRALS TO DISK
C
      NREC=NREC+1
      NINTS=NINTS+IWHERE
      IWHERE=-IWHERE
      CALL PWRIT(21,S,IX,IWHERE,NINTMX)
C
      IF(GOPARR) THEN
         CALL DDI_GSUMI(1055,NSCHWZ,1)
         CALL DDI_GSUMI(1056,NINTS,1)
         CALL DDI_GSUMI(1057,NRECS,1)
      END IF
      IF(SOME) WRITE(IW,9010) NINTS,NREC,21
      IF(SOME) WRITE(IW,9020) NSCHWZ
      CALL TSECND(TIM)
      TIM = TIM-TIM0
      IF(SOME) WRITE(IW,9030) TIM
C
      DSKWRK = SVDSKW
      RETURN
C
 9000 FORMAT(1X,'COMPUTING FOUR CENTER OVERLAP INTEGRALS (IJKL)')
 9010 FORMAT(1X,'TOTAL NUMBER OF NONZERO OVERLAP INTEGRALS=',I20/
     *       1X,I10,' INTEGRAL RECORDS WERE STORED ON DISK FILE',I3,'.')
 9020 FORMAT(1X,'SCHWARZ INEQUALITY TEST SKIPPED',I10,
     *    ' INTEGRAL BLOCKS.')
 9030 FORMAT(1X,'TIME TO EVALUATE (IJKL) OVERLAP INTEGRALS     =',F12.1)
      END
C*MODULE DFTINT  *DECK FORDIR
      SUBROUTINE FORDIR(SMAX,TEMPS,ADENS,BDENS,R,ROPEN,HISPIN)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SAMECEN,HISPIN,SOME,DSKWRK,MASWRK,GOPARR
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500, MXAO=2047)
C
      COMMON /IJPAIR/ INDEX2(MXAO)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                T(432),INVT(48),NT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
      DIMENSION SMAX(NSHELL*(NSHELL+1)/2),TEMPS(15*15*15*15),
     *          ADENS(NUM,NUM),BDENS(NUM,NUM),R(NUM,NUM),ROPEN(NUM,NUM)
      DIMENSION MI(48),MJ(48),MK(48),DFAC(0:21),
     *          LDAT(35),MDAT(35),NDAT(35),
     *          CONTA(35),CONTB(35),CONTC(35),CONTE(35),
     *          A21(0:4*4*4*4),IDUMMY(0:4),
     *          XXXX(-1:4,-1:4,-1:4,-1:4),
     *          YYYY(-1:4,-1:4,-1:4,-1:4),
     *          ZZZZ(-1:4,-1:4,-1:4,-1:4)
C
      SAVE NSCHWZ
C
      PARAMETER (PI=3.141592653589793238462643D+00)
      PARAMETER (RLN10=2.30258D+00)
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DEBUG/8HDEBUG   /
#else
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      DATA DEBUG_STR/"DEBUG   "/
#endif
C
C  X,Y,Z EXPONENTS OF CARTESIAN S,PX,PY,PZ,DXX,...
C
      DATA LDAT/0,1,0,0,2,0,0,1,1,0,3,0,0,2,2,1,0,1,0,1,4,0,0,3,3,
     *          1,0,1,0,2,2,0,2,1,1/
      DATA MDAT/0,0,1,0,0,2,0,1,0,1,0,3,0,1,0,2,2,0,1,1,0,4,0,1,0,
     *          3,3,0,1,2,0,2,1,2,1/
      DATA NDAT/0,0,0,1,0,0,2,0,1,1,0,0,3,0,1,0,1,2,2,1,0,0,4,0,1,
     *          0,1,3,3,0,2,2,1,1,2/
C
C  CALCULATES IVALUE!!
C     WARNING DFAC(0)=-1!!,DFAC(1)=0!!,DFAC(2)=1!! ETC.
C
      DATA DFAC/1.0D+00,1.0D+00,1.0D+00,2.0D+00,3.0D+00,8.0D+00,
     *          15.0D+00,48.0D+00,105.0D+00,384.0D+00,945.0D+00,
     *          3840.0D+00,10395.0D+00,46080.0D+00,135135.0D+00,
     *          645120.0D+00,2027025.0D+00,10321920.0D+00,
     *          34459425.0D+00,185794560.0D+00,
     *          654729075.0D+00,3715891200.0D+00/
C
C     HELPS DEAL WITH L SHELLS (SHELL THAT ENDS ON 1 MIGHT START WITH 0)
C
      DATA IDUMMY/0,0,2,3,4/
C
C   THIS SUBROUTINE CALCULATES THE ONE ELECTRON INTEGRAL
C   <ABCE> FOR CARTESIAN GAUSSIANS.  (UP TO G-SHELLS)
C
C   THIS IS THE DIRECT VERSION OF FOURC
C
      IF ((NORMF .EQ. 1 .OR. NORMP .EQ. 1).AND. MASWRK) THEN
        WRITE(IW,*)' SORRY BUT THE FOUR CENTER ONE ELECTRON OVELAP'
        WRITE(IW,*)' INTEGRALS USED FOR DFT DO NOT SUPPORT THE'
        WRITE(IW,*)' NO NORMALIZATION OPTIONS'
        CALL ABRT()
      END IF
C
C
C     ----- INITIALIZATION FOR PARALLEL WORK -----
C
      IPCOUNT = ME - 1
C
      CALL VCLR(XXXX,1,6*6*6*6)
      CALL VCLR(YYYY,1,6*6*6*6)
      CALL VCLR(ZZZZ,1,6*6*6*6)
      XXXX(0,0,0,0)=1.0D+00
      YYYY(0,0,0,0)=1.0D+00
      ZZZZ(0,0,0,0)=1.0D+00
C
      SOME = (EXETYP .EQ. DEBUG) .AND. MASWRK
      IF(SOME) WRITE(IW,*)' DOING (IJKL) INTEGRALS'
      CALL DAREAD(IDAF,IODA,SMAX,NSHELL*(NSHELL+1)/2,342,0)
C
      TOL=ITOL*RLN10
      CUTOFF = 1.0D+00/(10.0D+00**ICUT)
C
      NSCHWZ=0
C
C    LOOP OVER SHELLS A B C AND E FOR <AB|CE>
C
      DO 100 IASHL=1,NSHELL
C
C  TRY TO ELIMINATE A SHELL
C
      DO 1020 IT = 1,NT
         ID =  MAPSHL(IASHL,IT)
         IF (ID .GT. IASHL) GO TO 100
         MI(IT) = ID
 1020 CONTINUE
      KASHL=INDEX2(IASHL)
C  POSITION OF THE SHELL'S ATOM
      IAATOM=KATOM(IASHL)
      AX=C(1,IAATOM)
      AY=C(2,IAATOM)
      AZ=C(3,IAATOM)
C  INFO ABOUT THE PRIMATIVES
      ISTARTA=KSTART(IASHL)
      IENDA=ISTARTA+KNG(IASHL)-1
C  INFO ABOUT THE SUBSHELLS
      IAMAX=KMAX(IASHL)
      IAMIN=KMIN(IASHL)
      LOCA =KLOC(IASHL)-IAMIN
C
C  FIND THE HIGHEST SPIN VALUE IN THIS SHELL
      KATYPE=KTYPE(IASHL)-1
      JATYPE=1+IAMAX-IAMIN
C
C   B SHELL
C
      DO 101 IBSHL=1,IASHL
      DO 200 IT = 1,NT
      ID = MI(IT)
      JD = MAPSHL(IBSHL,IT)
      MJ(IT) = JD
      IF (ID .GE. JD) GO TO 1060
      ND = ID
      ID = JD
      JD = ND
 1060 IF (ID-IASHL) 200,1080,101
 1080 IF (JD-IBSHL) 200,200,101
  200 CONTINUE
      KBSHL=INDEX2(IBSHL)
      IBATOM=KATOM(IBSHL)
      BX=C(1,IBATOM)
      BY=C(2,IBATOM)
      BZ=C(3,IBATOM)
      ISTARTB=KSTART(IBSHL)
      IENDB=ISTARTB+KNG(IBSHL)-1
      IBMAX=KMAX(IBSHL)
      IBMIN=KMIN(IBSHL)
      LOCB = KLOC(IBSHL)-IBMIN
      KBTYPE=KTYPE(IBSHL)-1
      JBTYPE=1+IBMAX-IBMIN
      ABRR=(AX-BX)*(AX-BX)+(AY-BY)*(AY-BY)+(AZ-BZ)*(AZ-BZ)
C
C    C SHELL
C
      DO 102 ICSHL=1,IBSHL
      DO 340 IT = 1,NT
      ID = MI(IT)
      JD = MJ(IT)
      KD = MAPSHL(ICSHL,IT)
      MK(IT) = KD
  240 IF (ID .GE. JD) GO TO 260
      ND = ID
      ID = JD
      JD = ND
  260 IF (JD .GE. KD) GO TO 280
      ND = JD
      JD = KD
      KD = ND
      GO TO 240
  280 IF (ID-IASHL) 340,300,102
  300 IF (JD-IBSHL) 340,320,102
  320 IF (KD-ICSHL) 340,340,102
  340 CONTINUE
      KCSHL=INDEX2(ICSHL)
      ICATOM=KATOM(ICSHL)
      CX=C(1,ICATOM)
      CY=C(2,ICATOM)
      CZ=C(3,ICATOM)
      ISTARTC=KSTART(ICSHL)
      IENDC=ISTARTC+KNG(ICSHL)-1
      ICMAX=KMAX(ICSHL)
      ICMIN=KMIN(ICSHL)
      LOCC = KLOC(ICSHL)-ICMIN
      KCTYPE=KTYPE(ICSHL)-1
      JCTYPE=1+ICMAX-ICMIN
      ACRR=(AX-CX)*(AX-CX)+(AY-CY)*(AY-CY)+(AZ-CZ)*(AZ-CZ)
      BCRR=(BX-CX)*(BX-CX)+(BY-CY)*(BY-CY)+(BZ-CZ)*(BZ-CZ)
C
C    E SHELL
C
      DO 103 IESHL=1,ICSHL
      N4 = 0
      DO 540 IT = 1,NT
      ID = MI(IT)
      JD = MJ(IT)
      KD = MK(IT)
      LD = MAPSHL(IESHL,IT)
  380 IF (ID .GE. JD) GO TO 400
      ND = ID
      ID = JD
      JD = ND
  400 IF (JD .GE. KD) GO TO 420
      ND = JD
      JD = KD
      KD = ND
      GO TO 380
  420 IF (KD .GE. LD) GO TO 440
      ND = KD
      KD = LD
      LD = ND
      GO TO 400
  440 IF (ID-IASHL) 540,460,103
  460 IF (JD-IBSHL) 540,480,103
  480 IF (KD-ICSHL) 540,500,103
  500 IF (LD-IESHL) 540,520,103
  520 N4 = N4+1
  540 CONTINUE
      Q4=NT
      Q4=Q4/N4
C
C     ----- GO PARALLEL! -----
C
      IF (GOPARR) THEN
         IPCOUNT = IPCOUNT + 1
         IF (MOD(IPCOUNT,NPROC).NE.0) GO TO 103
      END IF
C
      IEATOM=KATOM(IESHL)
      EXX=C(1,IEATOM)
      EY=C(2,IEATOM)
      EZ=C(3,IEATOM)
      ISTARTE=KSTART(IESHL)
      IENDE=ISTARTE+KNG(IESHL)-1
      IEMAX=KMAX(IESHL)
      IEMIN=KMIN(IESHL)
      LOCE = KLOC(IESHL)-IEMIN
      KETYPE=KTYPE(IESHL)-1
      JETYPE=1+IEMAX-IEMIN
      AERR=(AX-EXX)*(AX-EXX)+(AY-EY)*(AY-EY)+(AZ-EZ)*(AZ-EZ)
      BERR=(BX-EXX)*(BX-EXX)+(BY-EY)*(BY-EY)+(BZ-EZ)*(BZ-EZ)
      CERR=(CX-EXX)*(CX-EXX)+(CY-EY)*(CY-EY)+(CZ-EZ)*(CZ-EZ)
C
C END OF SHELLS
C
C
C PRE-SCREEN THE THE INTEGRALS
C
      IF(SMAX(KASHL+IBSHL)*SMAX(KCSHL+IESHL) .LE. 1.0D-18 .OR.
     *   SMAX(KASHL+ICSHL)*SMAX(KBSHL+IESHL) .LE. 1.0D-18 .OR.
     *   SMAX(KASHL+IESHL)*SMAX(KBSHL+ICSHL) .LE. 1.0D-18) THEN
      NSCHWZ=NSCHWZ+1
      GO TO 103
      END IF
C
C    CHECK TO SEE IF THEY ARE ALL ON THE SAME CENTER
C
      IF(IAATOM .EQ. IBATOM .AND. IAATOM .EQ. ICATOM .AND. IAATOM
     *    .EQ. IEATOM) THEN
        IF(  MOD(KATYPE+KBTYPE+KCTYPE+KETYPE,2).EQ.1 .AND.
     *       .NOT.(IAMIN.EQ.1 .AND. IAMAX.EQ.4).AND.
     *       .NOT.(IBMIN.EQ.1 .AND. IBMAX.EQ.4).AND.
     *       .NOT.(ICMIN.EQ.1 .AND. ICMAX.EQ.4).AND.
     *       .NOT.(IEMIN.EQ.1 .AND. IEMAX.EQ.4)) GO TO 103
C            NO L SHELLS TO CONFUSE THINGS, AND TOTAL
C            MOMENTUM IS ODD, THUS INTEGRALS ARE ZERO
        SAMECEN=.TRUE.
      ELSE
        SAMECEN=.FALSE.
      END IF
C
C
C
C    LOOP OVER GUASSIAN PRIMATIVES WHILE READING
C          IN EXPONENTS AND CONTRACTIONS
C
C     DO SOMETHING SPECIAL FOR (SSSS) CASE
C
       IF(KATYPE.EQ.0 .AND. KBTYPE.EQ.0 .AND. KCTYPE.EQ.0 .AND.
     *                                        KETYPE.EQ.0) THEN
       TEMPS(1)=0.0D+00
       IF (SAMECEN) THEN
       DO 910 IAPRM=ISTARTA,IENDA
        A1=EX(IAPRM)
        SCONTA=CS(IAPRM)
        DO 911 IBPRM=ISTARTB,IENDB
         A2=EX(IBPRM)+A1
         SCONTB=CS(IBPRM)*SCONTA
         DO 912 ICPRM=ISTARTC,IENDC
          A3=EX(ICPRM)+A2
          SCONTC=CS(ICPRM)*SCONTB
         DO 913 IEPRM=ISTARTE,IENDE
          A5=EX(IEPRM)+A3
          SCONTE=CS(IEPRM)*SCONTC
C
            TEMPS(1)=TEMPS(1)+SCONTE/(A5*SQRT(A5))
C
  913    CONTINUE
  912    CONTINUE
  911   CONTINUE
  910  CONTINUE
       TEMPS(1)=TEMPS(1)*PI*SQRT(PI)
      ELSE
       DO 810 IAPRM=ISTARTA,IENDA
        A1=EX(IAPRM)
        SCONTA=CS(IAPRM)
        DO 811 IBPRM=ISTARTB,IENDB
         A2=EX(IBPRM)
         IF(ABRR*A1*A2/(A1+A2) .GT. TOL) GO TO 811
         SCONTB=CS(IBPRM)*SCONTA
C
C        GET INFO ABOUT A,B,AND P
C
            A6=A1+A2
            PX=((A1*AX+A2*BX)/A6)
            PY=((A1*AY+A2*BY)/A6)
            PZ=((A1*AZ+A2*BZ)/A6)
            ABK=-((A1*A2/A6)*
     *          ((AX-BX)*(AX-BX)+(AY-BY)*(AY-BY)+(AZ-BZ)*(AZ-BZ)))
C
C
         DO 812 ICPRM=ISTARTC,IENDC
          A3=EX(ICPRM)
          IF(ACRR*A1*A3/(A1+A3) .GT. TOL .OR.
     *       BCRR*A2*A3/(A2+A3) .GT. TOL) GO TO 812
          SCONTC=CS(ICPRM)*SCONTB
         DO 813 IEPRM=ISTARTE,IENDE
          A5=EX(IEPRM)
          IF(AERR*A1*A5/(A1+A5) .GT. TOL .OR.
     *       BERR*A2*A5/(A2+A5) .GT. TOL .OR.
     *       CERR*A3*A5/(A3+A5) .GT. TOL) GO TO 813
          SCONTE=CS(IEPRM)*SCONTC
C
          A4=A1+A2+A3+A5
          A4INV=1.0D+00/A4
C
            A7=A3+A5
            A7INV=1.0D+00/A7
            QX=((A3*CX+A5*EXX)/A7)
            QY=((A3*CY+A5*EY)/A7)
            QZ=((A3*CZ+A5*EZ)/A7)
C
C   CALCULATE
C
            TEMPS(1)=TEMPS(1)+SCONTE*(((PI*A4INV)*SQRT(PI*A4INV))*
     *                        EXP(ABK
     *                                 -((A3*A5*A7INV)*
     *          ((CX-EXX)*(CX-EXX)+(CY-EY)*(CY-EY)+(CZ-EZ)*(CZ-EZ)))
     *                                 -((A6*A7*A4INV)*
     *          ((PX-QX)*(PX-QX)+(PY-QY)*(PY-QY)+(PZ-QZ)*(PZ-QZ)))))
C
  813    CONTINUE
  812    CONTINUE
  811   CONTINUE
  810  CONTINUE
       END IF
C
C       END SPECIAL (SSSS) CASE
C
       ELSE
C
       CALL VCLR(TEMPS,1,JATYPE*JBTYPE*JCTYPE*JETYPE)
       DO 110 IAPRM=ISTARTA,IENDA
        A1=EX(IAPRM)
        CALL FORCNT(IAPRM,IAMAX,CONTA)
        DO 111 IBPRM=ISTARTB,IENDB
         A2=EX(IBPRM)
         CALL FORCNT(IBPRM,IBMAX,CONTB)
C
C        GET INFO ABOUT A,B,AND P
C
          IF(.NOT. SAMECEN) THEN
C
            A6=A1+A2
            PX=((A1*AX+A2*BX)/A6)
            PY=((A1*AY+A2*BY)/A6)
            PZ=((A1*AZ+A2*BZ)/A6)
            ABK=(A1*A2/A6)*ABRR
            IF (ABK .GT. TOL) GO TO 111
          END IF
C
C
         DO 112 ICPRM=ISTARTC,IENDC
          A3=EX(ICPRM)
          CALL FORCNT(ICPRM,ICMAX,CONTC)
          IF(ACRR*A1*A3/(A1+A3) .GT. TOL .OR.
     *       BCRR*A2*A3/(A2+A3) .GT. TOL) GO TO 112
         DO 113 IEPRM=ISTARTE,IENDE
          A5=EX(IEPRM)
          CALL FORCNT(IEPRM,IEMAX,CONTE)
C
C   CALCULATE FINAL GAUSSIAN CENTER AND OTHER THINGS
C   P=A AND B, Q=C AND E, D=FINAL CENTER
C
          A4=A1+A2+A3+A5
          A4INV=1.0D+00/A4
C
          IF(.NOT. SAMECEN) THEN
            IF(AERR*A1*A5/(A1+A5) .GT. TOL .OR.
     *         BERR*A2*A5/(A2+A5) .GT. TOL .OR.
     *         CERR*A3*A5/(A3+A5) .GT. TOL) GO TO 113
C
            A7=A3+A5
            A7INV=1.0D+00/A7
            A10=0.5D+00*A4INV
C
            QX=(A3*CX+A5*EXX)*A7INV
            QY=(A3*CY+A5*EY)*A7INV
            QZ=(A3*CZ+A5*EZ)*A7INV
            DX=(A1*AX+A2*BX+A3*CX+A5*EXX)*A4INV
            DY=(A1*AY+A2*BY+A3*CY+A5*EY)*A4INV
            DZ=(A1*AZ+A2*BZ+A3*CZ+A5*EZ)*A4INV
            DAX=DX-AX
            DAY=DY-AY
            DAZ=DZ-AZ
            DBX=DX-BX
            DBY=DY-BY
            DBZ=DZ-BZ
            DCX=DX-CX
            DCY=DY-CY
            DCZ=DZ-CZ
            DEX=DX-EXX
            DEY=DY-EY
            DEZ=DZ-EZ
            PQRR=(PX-QX)*(PX-QX)+(PY-QY)*(PY-QY)+(PZ-QZ)*(PZ-QZ)
C
C   CALCULATE KABC
C
            ABCK=((PI*A4INV)*SQRT(PI*A4INV))*EXP(-ABK
     *                                 -((A3*A5*A7INV)*CERR)
     *                                 -((A6*A7*A4INV)*PQRR))
C
C
C   USE RECURSION TO GENERATE HIGHER ANGULAR MOMENTUM XXXX,ETC
C
C     RECURSION BASED ON J
      DO 2000 J=1,KETYPE
                  XXXX(J,0,0,0)= DEX*XXXX(J-1,0  ,0  ,0  )+A10*(
     *                         (J-1)*XXXX(J-2,0  ,0  ,0  ))
                  YYYY(J,0,0,0)= DEY*YYYY(J-1,0  ,0  ,0  )+A10*(
     *                         (J-1)*YYYY(J-2,0  ,0  ,0  ))
                  ZZZZ(J,0,0,0)= DEZ*ZZZZ(J-1,0  ,0  ,0  )+A10*(
     *                         (J-1)*ZZZZ(J-2,0  ,0  ,0  ))
 2000 CONTINUE
C      RECURSION BASED ON K
       DO 2010 K=1,KCTYPE
        DO 2010 J=0,KETYPE
                  XXXX(J,K,0,0)= DCX*XXXX(J  ,K-1,0  ,0  )+A10*(
     *                             J*XXXX(J-1,K-1,0  ,0  ) +
     *                         (K-1)*XXXX(J  ,K-2,0  ,0  ))
                  YYYY(J,K,0,0)= DCY*YYYY(J  ,K-1,0  ,0  )+A10*(
     *                             J*YYYY(J-1,K-1,0  ,0  ) +
     *                         (K-1)*YYYY(J  ,K-2,0  ,0  ))
                  ZZZZ(J,K,0,0)= DCZ*ZZZZ(J  ,K-1,0  ,0  )+A10*(
     *                             J*ZZZZ(J-1,K-1,0  ,0  ) +
     *                         (K-1)*ZZZZ(J  ,K-2,0  ,0  ))
 2010  CONTINUE
C       RECURSION BASED ON L
        DO 2020 L=1,KBTYPE
          DO 2020 K=0,KCTYPE
            DO 2020 J=0,KETYPE
                  XXXX(J,K,L,0)= DBX*XXXX(J  ,K  ,L-1,0  )+A10*(
     *                             J*XXXX(J-1,K  ,L-1,0  ) +
     *                             K*XXXX(J  ,K-1,L-1,0  ) +
     *                         (L-1)*XXXX(J  ,K  ,L-2,0  ))
                  YYYY(J,K,L,0)= DBY*YYYY(J  ,K  ,L-1,0  )+A10*(
     *                             J*YYYY(J-1,K  ,L-1,0  ) +
     *                             K*YYYY(J  ,K-1,L-1,0  ) +
     *                         (L-1)*YYYY(J  ,K  ,L-2,0  ))
                  ZZZZ(J,K,L,0)= DBZ*ZZZZ(J  ,K  ,L-1,0  )+A10*(
     *                             J*ZZZZ(J-1,K  ,L-1,0  ) +
     *                             K*ZZZZ(J  ,K-1,L-1,0  ) +
     *                         (L-1)*ZZZZ(J  ,K  ,L-2,0  ))
 2020   CONTINUE
C         RECURSION BASED ON I
          DO 2030 I=1,KATYPE
            DO 2030 L=0,KBTYPE
              DO 2030 K=0,KCTYPE
                DO 2030 J=0,KETYPE
                  XXXX(J,K,L,I)= DAX*XXXX(J  ,K  ,L  ,I-1)+A10*(
     *                         (I-1)*XXXX(J  ,K  ,L  ,I-2) +
     *                             J*XXXX(J-1,K  ,L  ,I-1) +
     *                             K*XXXX(J  ,K-1,L  ,I-1) +
     *                             L*XXXX(J  ,K  ,L-1,I-1))
                  YYYY(J,K,L,I)= DAY*YYYY(J  ,K  ,L  ,I-1)+A10*(
     *                         (I-1)*YYYY(J  ,K  ,L  ,I-2) +
     *                             J*YYYY(J-1,K  ,L  ,I-1) +
     *                             K*YYYY(J  ,K-1,L  ,I-1) +
     *                             L*YYYY(J  ,K  ,L-1,I-1))
                  ZZZZ(J,K,L,I)= DAZ*ZZZZ(J  ,K  ,L  ,I-1)+A10*(
     *                         (I-1)*ZZZZ(J  ,K  ,L  ,I-2) +
     *                             J*ZZZZ(J-1,K  ,L  ,I-1) +
     *                             K*ZZZZ(J  ,K-1,L  ,I-1) +
     *                             L*ZZZZ(J  ,K  ,L-1,I-1))
 2030     CONTINUE
C
C    LOOP OVER SUBSHELLS TO GET VALUES OF THE INTEGRALS
C
          IMOVE=0
          DO 120 IASUB=IAMIN,IAMAX
C          THE X COORDINATE IN S(NUM,NUM,NUM,NUM)
           IAPOSIT=LOCA+IASUB
           L1=LDAT(IASUB)
           M1=MDAT(IASUB)
           N1=NDAT(IASUB)
           CONTRA=ABCK*CONTA(IASUB)
C
           DO 121 IBSUB=IBMIN,IBMAX
C           THE Y COORDINATE IN S(NUM,NUM,NUM,NUM)
            IBPOSIT=LOCB+IBSUB
            IF(IBPOSIT .GT. IAPOSIT) GO TO 120
            L2=LDAT(IBSUB)
            M2=MDAT(IBSUB)
            N2=NDAT(IBSUB)
            CONTRB=CONTRA*CONTB(IBSUB)
C
            DO 122 ICSUB=ICMIN,ICMAX
C            THE Z COORDINATE IN S(NUM,NUM,NUM,NUM)
             ICPOSIT=LOCC+ICSUB
             IF(ICPOSIT .GT. IBPOSIT) GO TO 121
             L3=LDAT(ICSUB)
             M3=MDAT(ICSUB)
             N3=NDAT(ICSUB)
             CONTRC=CONTRB*CONTC(ICSUB)
C
            DO 123 IESUB=IEMIN,IEMAX
C            THE E COORDINATE IN S(NUM,NUM,NUM,NUM)
             IEPOSIT=LOCE+IESUB
             IF(IEPOSIT .GT. ICPOSIT) GO TO 122
             L5=LDAT(IESUB)
             M5=MDAT(IESUB)
             N5=NDAT(IESUB)
             CONTRE=CONTRC*CONTE(IESUB)
             IMOVE=IMOVE+1
C
              TEMPS(IMOVE) = TEMPS(IMOVE)
     *            +CONTRE*XXXX(L5,L3,L2,L1)*
     *               YYYY(M5,M3,M2,M1)*ZZZZ(N5,N3,N2,N1)
C
  123       CONTINUE
  122       CONTINUE
  121      CONTINUE
  120     CONTINUE
C
          ELSE
C
C         SPECIAL SAME CENTER CASE
C
          IMOVE=0
          RADIAL=(PI*A4INV)*SQRT(PI*A4INV)
          A20=SQRT(0.5D+00*A4INV)
          IDUM1=IDUMMY(KATYPE)+IDUMMY(KBTYPE)+IDUMMY(KCTYPE)+
     *         IDUMMY(KETYPE)
          IDUM2=KATYPE+KBTYPE+KCTYPE+KETYPE
          DO  I=IDUM1,IDUM2
            A21(I)=A20**I
          END DO
          DO 220 IASUB=IAMIN,IAMAX
C          THE X COORDINATE IN S(NUM,NUM,NUM,NUM)
           IAPOSIT=LOCA+IASUB
           L1=LDAT(IASUB)
           M1=MDAT(IASUB)
           N1=NDAT(IASUB)
           CONTRA=CONTA(IASUB)*RADIAL
C
           DO 221 IBSUB=IBMIN,IBMAX
C           THE Y COORDINATE IN S(NUM,NUM,NUM,NUM)
            IBPOSIT=LOCB+IBSUB
            IF(IBPOSIT .GT. IAPOSIT) GO TO 220
            L2=L1+LDAT(IBSUB)
            M2=M1+MDAT(IBSUB)
            N2=N1+NDAT(IBSUB)
            CONTRB=CONTRA*CONTB(IBSUB)
C
            DO 222 ICSUB=ICMIN,ICMAX
C            THE Z COORDINATE IN S(NUM,NUM,NUM,NUM)
             ICPOSIT=LOCC+ICSUB
             IF(ICPOSIT .GT. IBPOSIT) GO TO 221
             L3=L2+LDAT(ICSUB)
             M3=M2+MDAT(ICSUB)
             N3=N2+NDAT(ICSUB)
             CONTRC=CONTRB*CONTC(ICSUB)
C
            DO 223 IESUB=IEMIN,IEMAX
C            THE E COORDINATE IN S(NUM,NUM,NUM,NUM)
             IEPOSIT=LOCE+IESUB
             IF(IEPOSIT .GT. ICPOSIT) GO TO 222
             IMOVE=IMOVE+1
             L5=L3+LDAT(IESUB)
             IF(MOD(L5,2) .EQ. 1) GO TO 223
             M5=M3+MDAT(IESUB)
             IF(MOD(M5,2) .EQ. 1) GO TO 223
             N5=N3+NDAT(IESUB)
             IF(MOD(N5,2) .EQ. 1) GO TO 223
             CONTRE=CONTRC*CONTE(IESUB)
C
C          THE IF MOD(N5,2) GETS RID OF ODD EXPONENTS
C
            XYZI=DFAC(L5)*DFAC(M5)*DFAC(N5)*CONTRE*A21(L5+N5+M5)
            TEMPS(IMOVE)=TEMPS(IMOVE)+XYZI
C
  223       CONTINUE
  222       CONTINUE
  221      CONTINUE
  220     CONTINUE
C
C       END SPECIAL SAME CENTER CASE
C
          END IF
C
C
  113    CONTINUE
  112    CONTINUE
  111   CONTINUE
  110  CONTINUE
C      END THE SSSS IF
       END IF
C
C   LOOP OVER TEMPORARY MATRIX AND FORM M[RHO]
C
C     LOOP OVER K,L,N,M  (STRANGE ORDER, BUT CORRECT)
C
         IMOVE=0
         DO 421 K=IAMIN+LOCA,IAMAX+LOCA
C          MIN LETS US SKIP (5611) SINCE WE DO (6511)
           DO 422 L=IBMIN+LOCB,MIN(IBMAX+LOCB,K)
C
            DO 423 N=ICMIN+LOCC,MIN(ICMAX+LOCC,L)
C
             DO 424 M=IEMIN+LOCE,MIN(IEMAX+LOCE,N)
              IMOVE=IMOVE+1
C             POSTSCREEN THE INTEGRALS
              IF(ABS(TEMPS(IMOVE)) .GT. CUTOFF) THEN
C             TAKE INTO ACCOUNT SKIPPED INTERGRALS
              A4OVKLNM=TEMPS(IMOVE)*Q4
C             MAKE R AND ROPEN
C
              IF(.NOT. HISPIN) THEN
C
                 R(K,M)=R(K,M)+A4OVKLNM*ADENS(L,N)
             IF(L .NE. N) THEN
                 R(K,L)=R(K,L)+A4OVKLNM*ADENS(N,M)
                 R(N,M)=R(N,M)+A4OVKLNM*ADENS(K,L)
             END IF
             IF(N .NE. M .AND. L .NE. K) THEN
                 R(L,N)=R(L,N)+A4OVKLNM*ADENS(K,M)
                 R(K,N)=R(K,N)+A4OVKLNM*ADENS(L,M)
                 R(L,M)=R(L,M)+A4OVKLNM*ADENS(K,N)
             ELSE IF(L .NE. K) THEN
                 R(L,M)=R(L,M)+A4OVKLNM*ADENS(K,N)
             ELSE IF(N .NE. M) THEN
                 R(K,N)=R(K,N)+A4OVKLNM*ADENS(L,M)
             END IF
C
               ELSE
C
                 R    (K,M)=R    (K,M)+A4OVKLNM*ADENS(L,N)
                 ROPEN(K,M)=ROPEN(K,M)+A4OVKLNM*BDENS(L,N)
             IF(L .NE. N) THEN
                 R    (K,L)=R    (K,L)+A4OVKLNM*ADENS(N,M)
                 ROPEN(K,L)=ROPEN(K,L)+A4OVKLNM*BDENS(N,M)
                 R    (N,M)=R    (N,M)+A4OVKLNM*ADENS(K,L)
                 ROPEN(N,M)=ROPEN(N,M)+A4OVKLNM*BDENS(K,L)
             END IF
             IF(N .NE. M .AND. L .NE. K) THEN
                 R    (L,N)=R    (L,N)+A4OVKLNM*ADENS(K,M)
                 ROPEN(L,N)=ROPEN(L,N)+A4OVKLNM*BDENS(K,M)
                 R    (K,N)=R    (K,N)+A4OVKLNM*ADENS(L,M)
                 ROPEN(K,N)=ROPEN(K,N)+A4OVKLNM*BDENS(L,M)
                 R    (L,M)=R    (L,M)+A4OVKLNM*ADENS(K,N)
                 ROPEN(L,M)=ROPEN(L,M)+A4OVKLNM*BDENS(K,N)
             ELSE IF(L .NE. K) THEN
                 R    (L,M)=R    (L,M)+A4OVKLNM*ADENS(K,N)
                 ROPEN(L,M)=ROPEN(L,M)+A4OVKLNM*BDENS(K,N)
             ELSE IF(N .NE. M) THEN
                 R    (K,N)=R    (K,N)+A4OVKLNM*ADENS(L,M)
                 ROPEN(K,N)=ROPEN(K,N)+A4OVKLNM*BDENS(L,M)
             END IF
C
               END IF
C
            END IF
  424     CONTINUE
  423    CONTINUE
  422   CONTINUE
  421  CONTINUE
C
  103 CONTINUE
  102 CONTINUE
  101 CONTINUE
  100 CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
C     SUM UP SCHWZ SAVINGS
      IF(GOPARR) CALL DDI_GSUMI(1055,NSCHWZ,1)
      IF(SOME) WRITE(IW,9020) NSCHWZ
      RETURN
 9020 FORMAT(1X,'SCHWARZ INEQUALITY TEST SKIPPED',I10,
     *    ' INTEGRAL BLOCKS.')
      END
C*MODULE DFTINT  *DECK FORCNT
      SUBROUTINE FORCNT(IPRIM,IMAX,THEDATA)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION THEDATA(35)
C
      PARAMETER (MXSH=1000, MXGTOT=5000)
C
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
C
C  NORMALIZATION FACTORS FOR CARTESIAN S, PX,PY,PZ, DXX,DYY,...
C
      PARAMETER (SQRT3=1.73205080756888D+00,
     *           SQRT5=2.23606797749979D+00,
     *           SQRT7=2.64575131106459D+00,
     *           SQRT35=5.916079783099616D+00,
     *           SQRT353=3.415650255319866D+00,
     *           SQRT15=3.872983346207417D+00)
C
      GO TO(1,
     *     2,2,2,
     *     5,5,5,5,5,5,
     *     11,11,11,11,11,11,11,11,11,11,
     *     21,21,21,21,21,21,21,21,21,21,21,21,21,21,21)IMAX
C
C  1 S
 1    THEDATA(1)=CS(IPRIM)
      RETURN
C  3 P   (INCLUDING L SHELLS)
 2    THEDATA(1)=CS(IPRIM)
      THEDATA(2)=CP(IPRIM)
      THEDATA(3)=CP(IPRIM)
      THEDATA(4)=CP(IPRIM)
      RETURN
C  6 D
 5    THEDATA(5)=CD(IPRIM)
      THEDATA(6)=CD(IPRIM)
      THEDATA(7)=CD(IPRIM)
      THEDATA(8)=CD(IPRIM)*SQRT3
      THEDATA(9)=CD(IPRIM)*SQRT3
      THEDATA(10)=CD(IPRIM)*SQRT3
      RETURN
C 10 F
 11   THEDATA(11)=CF(IPRIM)
      THEDATA(12)=CF(IPRIM)
      THEDATA(13)=CF(IPRIM)
      THEDATA(14)=CF(IPRIM)*SQRT5
      THEDATA(15)=CF(IPRIM)*SQRT5
      THEDATA(16)=CF(IPRIM)*SQRT5
      THEDATA(17)=CF(IPRIM)*SQRT5
      THEDATA(18)=CF(IPRIM)*SQRT5
      THEDATA(19)=CF(IPRIM)*SQRT5
      THEDATA(20)=CF(IPRIM)*SQRT15
      RETURN
C 15 G
 21   THEDATA(21)=CG(IPRIM)
      THEDATA(22)=CG(IPRIM)
      THEDATA(23)=CG(IPRIM)
      THEDATA(24)=CG(IPRIM)*SQRT7
      THEDATA(25)=CG(IPRIM)*SQRT7
      THEDATA(26)=CG(IPRIM)*SQRT7
      THEDATA(27)=CG(IPRIM)*SQRT7
      THEDATA(28)=CG(IPRIM)*SQRT7
      THEDATA(29)=CG(IPRIM)*SQRT7
      THEDATA(30)=CG(IPRIM)*SQRT353
      THEDATA(31)=CG(IPRIM)*SQRT353
      THEDATA(32)=CG(IPRIM)*SQRT353
      THEDATA(33)=CG(IPRIM)*SQRT35
      THEDATA(34)=CG(IPRIM)*SQRT35
      THEDATA(35)=CG(IPRIM)*SQRT35
      RETURN
      END
C*MODULE DFTINT  *DECK FORSCR
      SUBROUTINE FORSCR(SOME,S,TEMPS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500, MXAO=2047)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,SOME
C
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /IJPAIR/ INDEX2(MXAO)
C
      DIMENSION S((NSHELL*NSHELL+NSHELL)/2),TEMPS(15*15*15*15)
      DIMENSION LDAT(35),MDAT(35),NDAT(35),
     *          CONTA(35),CONTB(35),CONTC(35),CONTE(35),
     *          XXXX(-1:4,-1:4,-1:4,-1:4),
     *          YYYY(-1:4,-1:4,-1:4,-1:4),
     *          ZZZZ(-1:4,-1:4,-1:4,-1:4)
C
      PARAMETER (PI=3.141592653589793238462643D+00)
      PARAMETER (RLN10=2.30258D+00)
C
C  X,Y,Z EXPONENTS OF CARTESIAN S,PX,PY,PZ,DXX,...
C
      DATA LDAT/0,1,0,0,2,0,0,1,1,0,3,0,0,2,2,1,0,1,0,1,4,0,0,3,3,
     *          1,0,1,0,2,2,0,2,1,1/
      DATA MDAT/0,0,1,0,0,2,0,1,0,1,0,3,0,1,0,2,2,0,1,1,0,4,0,1,0,
     *          3,3,0,1,2,0,2,1,2,1/
      DATA NDAT/0,0,0,1,0,0,2,0,1,1,0,0,3,0,1,0,1,2,2,1,0,0,4,0,1,
     *          0,1,3,3,0,2,2,1,1,2/
C
      TOL=ITOL*RLN10
C
C     ----- INTIALIZE PARALLEL -----
C
      IPCOUNT = ME - 1
      IF (GOPARR) CALL VCLR(S,1,NSHELL*(NSHELL+1)/2)
C
      CALL TSECND(TIM0)
C
      CALL VCLR(XXXX,1,6*6*6*6)
      CALL VCLR(YYYY,1,6*6*6*6)
      CALL VCLR(ZZZZ,1,6*6*6*6)
      XXXX(0,0,0,0)=1.0D+00
      YYYY(0,0,0,0)=1.0D+00
      ZZZZ(0,0,0,0)=1.0D+00
C
C    LOOP OVER SHELLS A B FOR <AABB>
C
      DO 100 IASHL=1,NSHELL
      KASHL=INDEX2(IASHL)
C  POSITION OF THE SHELL'S ATOM
      IAATOM=KATOM(IASHL)
      AX=C(1,IAATOM)
      AY=C(2,IAATOM)
      AZ=C(3,IAATOM)
C  INFO ABOUT THE PRIMATIVES
      ISTARTA=KSTART(IASHL)
      IENDA=ISTARTA+KNG(IASHL)-1
C  INFO ABOUT THE SUBSHELLS
      IAMAX=KMAX(IASHL)
      IAMIN=KMIN(IASHL)
      LOCA =KLOC(IASHL)-IAMIN
C
C  FIND THE HIGHEST SPIN VALUE IN THIS SHELL
      KATYPE=KTYPE(IASHL)-1
      JATYPE=1+IAMAX-IAMIN
C
C   B SHELL
C
      DO 101 IBSHL=1,IASHL
C
C     ----- GO PARALLEL! -----
C
      IF (GOPARR) THEN
         IPCOUNT = IPCOUNT + 1
         IF (MOD(IPCOUNT,NPROC).NE.0) GO TO 101
      END IF
      IBATOM=KATOM(IBSHL)
      BX=C(1,IBATOM)
      BY=C(2,IBATOM)
      BZ=C(3,IBATOM)
      ISTARTB=KSTART(IBSHL)
      IENDB=ISTARTB+KNG(IBSHL)-1
      IBMAX=KMAX(IBSHL)
      IBMIN=KMIN(IBSHL)
      LOCB = KLOC(IBSHL)-IBMIN
      KBTYPE=KTYPE(IBSHL)-1
      JBTYPE=1+IBMAX-IBMIN
      ABRR=(AX-BX)*(AX-BX)+(AY-BY)*(AY-BY)+(AZ-BZ)*(AZ-BZ)
C
C END OF SHELLS
C
C    LOOP OVER GUASSIAN PRIMATIVES WHILE READING
C          IN EXPONENTS AND CONTRACTIONS
C
       CALL VCLR(TEMPS,1,JATYPE*JBTYPE*JATYPE*JBTYPE)
       DO 110 IAPRM=ISTARTA,IENDA
        A1=EX(IAPRM)
        CALL FORCNT(IAPRM,IAMAX,CONTA)
        DO 111 ICPRM=ISTARTA,IENDA
         A2=EX(ICPRM)
         CALL FORCNT(ICPRM,IAMAX,CONTC)
            A6=A1+A2
C
C
         DO 112 IBPRM=ISTARTB,IENDB
          A3=EX(IBPRM)
          CALL FORCNT(IBPRM,IBMAX,CONTB)
          IF(ABRR*A1*A3/(A1+A3) .GT. TOL .OR.
     *       ABRR*A2*A3/(A2+A3) .GT. TOL) GO TO 112
         DO 113 IEPRM=ISTARTB,IENDB
          A5=EX(IEPRM)
          CALL FORCNT(IEPRM,IBMAX,CONTE)
C
C
          A4=A1+A2+A3+A5
          A4INV=1.0D+00/A4
C
          IF(ABRR*A1*A5/(A1+A5) .GT. TOL .OR.
     *       ABRR*A2*A5/(A2+A5) .GT. TOL) GO TO 113
C
            A7=A3+A5
            A10=0.5D+00/A4
C
            DX=(A1*AX+A2*AX+A3*BX+A5*BX)*A4INV
            DY=(A1*AY+A2*AY+A3*BY+A5*BY)*A4INV
            DZ=(A1*AZ+A2*AZ+A3*BZ+A5*BZ)*A4INV
            DAX=DX-AX
            DAY=DY-AY
            DAZ=DZ-AZ
            DBX=DX-BX
            DBY=DY-BY
            DBZ=DZ-BZ
C
C   CALCULATE KABC
C
            ABCK=((PI*A4INV)*SQRT(PI*A4INV))*EXP(-ABRR*A6*A7*A4INV)
C
C   GENERATE HIGHER ANGULAR MOMENTUM XXXX,ETC
C
C     RECURSION BASED ON J
C
      DO 2000 J=1,KATYPE
                  XXXX(0,J,0,0)= DAX*XXXX(0,J-1,0  ,0  )+A10*(
     *                         (J-1)*XXXX(0,J-2,0  ,0  ))
                  YYYY(0,J,0,0)= DAY*YYYY(0,J-1,0  ,0  )+A10*(
     *                         (J-1)*YYYY(0,J-2,0  ,0  ))
                  ZZZZ(0,J,0,0)= DAZ*ZZZZ(0,J-1,0  ,0  )+A10*(
     *                         (J-1)*ZZZZ(0,J-2,0  ,0  ))
 2000 CONTINUE
C
C      RECURSION BASED ON K
C
       DO 2010 K=1,KBTYPE
        DO 2010 J=0,KATYPE
                  XXXX(0,J,K,0)= DBX*XXXX(0,J  ,K-1,0  )+A10*(
     *                             J*XXXX(0,J-1,K-1,0  ) +
     *                         (K-1)*XXXX(0,J  ,K-2,0  ))
                  YYYY(0,J,K,0)= DBY*YYYY(0,J  ,K-1,0  )+A10*(
     *                             J*YYYY(0,J-1,K-1,0  ) +
     *                         (K-1)*YYYY(0,J  ,K-2,0  ))
                  ZZZZ(0,J,K,0)= DBZ*ZZZZ(0,J  ,K-1,0  )+A10*(
     *                             J*ZZZZ(0,J-1,K-1,0  ) +
     *                         (K-1)*ZZZZ(0,J  ,K-2,0  ))
 2010  CONTINUE
C
C       RECURSION BASED ON L
C
        DO 2020 L=1,KBTYPE
          DO 2020 K=0,KBTYPE
            DO 2020 J=0,KATYPE
                  XXXX(0,J,K,L)= DBX*XXXX(0,J  ,K  ,L-1)+A10*(
     *                             J*XXXX(0,J-1,K  ,L-1) +
     *                             K*XXXX(0,J  ,K-1,L-1) +
     *                         (L-1)*XXXX(0,J  ,K  ,L-2))
                  YYYY(0,J,K,L)= DBY*YYYY(0,J  ,K  ,L-1)+A10*(
     *                             J*YYYY(0,J-1,K  ,L-1) +
     *                             K*YYYY(0,J  ,K-1,L-1) +
     *                         (L-1)*YYYY(0,J  ,K  ,L-2))
                  ZZZZ(0,J,K,L)= DBZ*ZZZZ(0,J  ,K  ,L-1)+A10*(
     *                             J*ZZZZ(0,J-1,K  ,L-1) +
     *                             K*ZZZZ(0,J  ,K-1,L-1) +
     *                         (L-1)*ZZZZ(0,J  ,K  ,L-2))
 2020   CONTINUE
C
C         RECURSION BASED ON I
C
          DO 2030 L=0,KBTYPE
            DO 2030 K=0,KBTYPE
              DO 2030 J=0,KATYPE
                DO 2030 I=1,KATYPE
                  XXXX(I,J,K,L)= DAX*XXXX(I-1,J  ,K  ,L  )+A10*(
     *                         (I-1)*XXXX(I-2,J  ,K  ,L  ) +
     *                             J*XXXX(I-1,J-1,K  ,L  ) +
     *                             K*XXXX(I-1,J  ,K-1,L  ) +
     *                             L*XXXX(I-1,J  ,K  ,L-1))
                  YYYY(I,J,K,L)= DAY*YYYY(I-1,J  ,K  ,L  )+A10*(
     *                         (I-1)*YYYY(I-2,J  ,K  ,L  ) +
     *                             J*YYYY(I-1,J-1,K  ,L  ) +
     *                             K*YYYY(I-1,J  ,K-1,L  ) +
     *                             L*YYYY(I-1,J  ,K  ,L-1))
                  ZZZZ(I,J,K,L)= DAZ*ZZZZ(I-1,J  ,K  ,L  )+A10*(
     *                         (I-1)*ZZZZ(I-2,J  ,K  ,L  ) +
     *                             J*ZZZZ(I-1,J-1,K  ,L  ) +
     *                             K*ZZZZ(I-1,J  ,K-1,L  ) +
     *                             L*ZZZZ(I-1,J  ,K  ,L-1))
 2030     CONTINUE
C
C
C    LOOP OVER SUBSHELLS TO GET VALUES OF THE INTEGRALS
C
      IMOVE=0
          DO 120 IASUB=IAMIN,IAMAX
C          THE X COORDINATE IN S(NUM,NUM,NUM,NUM)
           IAPOSIT=LOCA+IASUB
           L1=LDAT(IASUB)
           M1=MDAT(IASUB)
           N1=NDAT(IASUB)
           CONTRA=CONTA(IASUB)*CONTC(IASUB)
C
           DO 121 IBSUB=IBMIN,IBMAX
C           THE Y COORDINATE IN S(NUM,NUM,NUM,NUM)
            IBPOSIT=LOCB+IBSUB
            IF(IBPOSIT .GT. IAPOSIT) GO TO 120
            L2=LDAT(IBSUB)
            M2=MDAT(IBSUB)
            N2=NDAT(IBSUB)
            CONTRE=CONTRA*CONTB(IBSUB)*CONTE(IBSUB)
C
              IMOVE=IMOVE+1
              TEMPS(IMOVE) = TEMPS(IMOVE)
     *            +ABCK*CONTRE*XXXX(L1,L1,L2,L2)*
     *               YYYY(M1,M1,M2,M2)*ZZZZ(N1,N1,N2,N2)
C
  121      CONTINUE
  120     CONTINUE
C
C
  113    CONTINUE
  112    CONTINUE
  111   CONTINUE
  110  CONTINUE
C
C   LOOP OVER TEMPORARY MATRIX
C
        DUM=0.0D+00
         IMOVE=0
         DO 421 IAPOSIT=IAMIN+LOCA,IAMAX+LOCA
           DO 422 IBPOSIT=IBMIN+LOCB,MIN(IBMAX+LOCB,IAPOSIT)
C
              IMOVE=IMOVE+1
              IF(TEMPS(IMOVE) .GT. DUM) DUM=TEMPS(IMOVE)
  422   CONTINUE
  421  CONTINUE
       S(KASHL+IBSHL)=DUM
C
C     END SHELL LOOPS
C
  101 CONTINUE
  100 CONTINUE
C
C     ----- SUM UP PARTIAL CONTRIBUTIONS IF PARALLEL -----
C
      IF(GOPARR)CALL DDI_GSUMF(1039,S,NSHELL*(NSHELL+1)/2)
C
      CALL DAWRIT(IDAF,IODA,S,NSHELL*(NSHELL+1)/2,342,0)
C
      CALL TSECND(TIM)
      TIM = TIM-TIM0
      IF(SOME) WRITE(IW,9000) TIM
      RETURN
C
 9000 FORMAT(1X,'(IIJJ) INTEGRAL SCHWARZ INEQUALITY OVERHEAD: T=',F12.2)
      END
C*MODULE DFTINT  *DECK THREEC
      SUBROUTINE THREEC(SOME,S,IX,TEMPS,NINTMX)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      LOGICAL DSKWRK,MASWRK,GOPARR,SVDSKW,SOME
C
      DIMENSION S(NINTMX),IX(*),TEMPS(15*15*15)
      DIMENSION CONTA(35),CONTB(35),CONTC(35),
     *          LDAT(35),MDAT(35),NDAT(35),
     *          XXXX(-1:4,-1:4,-1:4),
     *          YYYY(-1:4,-1:4,-1:4),
     *          ZZZZ(-1:4,-1:4,-1:4)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCKLAB/ LABSIZ
      COMMON /RESTAR/ TIMLIM,IREST,NREC,INTLOC,IST,JST,KST,LST
C
      PARAMETER (PI=3.141592653589793238462643D+00)
      PARAMETER (RLN10=2.30258D+00)
C
C  X,Y,Z EXPONENTS OF CARTESIAN S,PX,PY,PZ,DXX,...
C
      DATA LDAT/0,1,0,0,2,0,0,1,1,0,3,0,0,2,2,1,0,1,0,1,4,0,0,3,3,
     *          1,0,1,0,2,2,0,2,1,1/
      DATA MDAT/0,0,1,0,0,2,0,1,0,1,0,3,0,1,0,2,2,0,1,1,0,4,0,1,0,
     *          3,3,0,1,2,0,2,1,2,1/
      DATA NDAT/0,0,0,1,0,0,2,0,1,1,0,0,3,0,1,0,1,2,2,1,0,0,4,0,1,
     *          0,1,3,3,0,2,2,1,1,2/
C
      IF ((NORMF .EQ. 1 .OR. NORMP .EQ. 1).AND. MASWRK) THEN
        WRITE(IW,*)' SORRY BUT THE THREE CENTER ONE ELECTRON OVELAP'
        WRITE(IW,*)' INTEGRALS USED FOR DFT DO NOT SUPPORT THE'
        WRITE(IW,*)' NO NORMALIZATION OPTIONS'
        CALL ABRT
      END IF
C
C     ----- INITIALIZATION FOR PARALLEL WORK -----
C
      IPCOUNT = ME - 1
      SVDSKW = DSKWRK
      DSKWRK = .TRUE.
C
      IF(SOME) WRITE(IW,9000)
C
C    INITIALIZE
C
      CALL TSECND(TIM0)
C
      TOL=ITOL*RLN10
      CUTOFF = 1.0D+00/(10.0D+00**ICUT)
C
      DO 10 K=-1,0
      DO 10 J=-1,0
      DO 10 I=-1,0
      XXXX(I,J,K)=0.0D+00
      YYYY(I,J,K)=0.0D+00
      ZZZZ(I,J,K)=0.0D+00
 10   CONTINUE
      XXXX(0,0,0)=1.0D+00
      YYYY(0,0,0)=1.0D+00
      ZZZZ(0,0,0)=1.0D+00
C
      LABSIZ = 1
      MAXAO=255
      IF(NUM.GT.MAXAO) LABSIZ = 2
C
      IWHERE=0
      NINTS=0
      NREC=0
      CALL SEQOPN(21,'DFTINTS','UNKNOWN',.FALSE.,'UNFORMATTED')
      CALL SEQREW(21)
C
C    LOOP OVER SHELLS A B AND C FOR <AB|C>
C
      DO 100 IASHL=1,NSHELL
C
C     ----- CHECK CPU TIME -----
C
      CALL TSECND(TIM)
      IF(TIM.GE.TIMLIM) THEN
         WRITE(IW,*)' NO TIME LEFT IN THREE'
C        BAD NEWS
         RETURN
      END IF
C  POSITION OF THE SHELL'S ATOM
      IAATOM=KATOM(IASHL)
      AX=C(1,IAATOM)
      AY=C(2,IAATOM)
      AZ=C(3,IAATOM)
      AA=AX*AX+AY*AY+AZ*AZ
C  INFO ABOUT THE PRIMATIVES
      ISTARTA=KSTART(IASHL)
      IENDA=ISTARTA+KNG(IASHL)-1
C  INFO ABOUT THE SUBSHELLS
      IAMAX=KMAX(IASHL)
      IAMIN=KMIN(IASHL)
      LOCA =KLOC(IASHL)-IAMIN
C  FIND THE HIGHEST SPIN VALUE IN THIS SHELL
      KATYPE=KTYPE(IASHL)-1
C
      DO 101 IBSHL=1,IASHL
      IBATOM=KATOM(IBSHL)
      BX=C(1,IBATOM)
      BY=C(2,IBATOM)
      BZ=C(3,IBATOM)
      BB=BX*BX+BY*BY+BZ*BZ
      ISTARTB=KSTART(IBSHL)
      IENDB=ISTARTB+KNG(IBSHL)-1
      IBMAX=KMAX(IBSHL)
      IBMIN=KMIN(IBSHL)
      LOCB = KLOC(IBSHL)-IBMIN
      ABRR=(AX-BX)*(AX-BX)+(AY-BY)*(AY-BY)+(AZ-BZ)*(AZ-BZ)
      KBTYPE=KTYPE(IBSHL)-1
C
      DO 102 ICSHL=1,IBSHL
C
C     ----- GO PARALLEL! -----
C
      IF (GOPARR) THEN
         IPCOUNT = IPCOUNT + 1
         IF (MOD(IPCOUNT,NPROC).NE.0) GO TO 102
      END IF
C
      ICATOM=KATOM(ICSHL)
      CX=C(1,ICATOM)
      CY=C(2,ICATOM)
      CZ=C(3,ICATOM)
      CC=CX*CX+CY*CY+CZ*CZ
      ISTARTC=KSTART(ICSHL)
      IENDC=ISTARTC+KNG(ICSHL)-1
      ICMAX=KMAX(ICSHL)
      ICMIN=KMIN(ICSHL)
      LOCC = KLOC(ICSHL)-ICMIN
      ACRR=(AX-CX)*(AX-CX)+(AY-CY)*(AY-CY)+(AZ-CZ)*(AZ-CZ)
      BCRR=(BX-CX)*(BX-CX)+(BY-CY)*(BY-CY)+(BZ-CZ)*(BZ-CZ)
      KCTYPE=KTYPE(ICSHL)-1
C
C    LOOP OVER GUASSIAN PRIMATIVES WHILE READING
C          IN EXPONENTS AND CONTRACTIONS
C
      CALL VCLR(TEMPS,1,15*15*15)
       DO 110 IAPRM=ISTARTA,IENDA
        A1=EX(IAPRM)
        CALL FORCNT(IAPRM,IAMAX,CONTA)
        DO 111 IBPRM=ISTARTB,IENDB
         A2=EX(IBPRM)
         CALL FORCNT(IBPRM,IBMAX,CONTB)
         IF(ABRR*(A1*A2/(A1+A2)) .GT. TOL) GO TO 111
         DO 112 ICPRM=ISTARTC,IENDC
          A3=EX(ICPRM)
          CALL FORCNT(ICPRM,ICMAX,CONTC)
          IF(ACRR*A1*A3/(A1+A3) .GT. TOL .OR.
     *       BCRR*A2*A3/(A2+A3) .GT. TOL) GO TO 112
C
C   CALCULATE FINAL GAUSSIAN CENTER D
C
          A4=A1+A2+A3
          A4INV=1.0D+00/A4
          A10=0.5D+00*A4INV
          DX=(A1*AX+A2*BX+A3*CX)*A4INV
          DY=(A1*AY+A2*BY+A3*CY)*A4INV
          DZ=(A1*AZ+A2*BZ+A3*CZ)*A4INV
          DBX=DX-BX
          DBY=DY-BY
          DBZ=DZ-BZ
          DCX=DX-CX
          DCY=DY-CY
          DCZ=DZ-CZ
          DAX=DX-AX
          DAY=DY-AY
          DAZ=DZ-AZ
C
C   CALCULATE KABC
C
           ABCK=((PI*A4INV)*SQRT(PI*A4INV))*
     *          EXP(((A1*AX+A2*BX+A3*CX)*(A1*AX+A2*BX+A3*CX)+
     *               (A1*AY+A2*BY+A3*CY)*(A1*AY+A2*BY+A3*CY)+
     *               (A1*AZ+A2*BZ+A3*CZ)*(A1*AZ+A2*BZ+A3*CZ))*A4INV
     *              -(A1*AA+A2*BB+A3*CC))
C
C   USE RECURSION TO GENERATE HIGHER ANGULAR MOMENTUM XXXX,ETC
C
C              RECURSION BASED ON J
               DO 2000 J=1,KATYPE
                  XXXX(J,0,0)= DAX*XXXX(J-1,0  ,0  )+A10*(
     *                         (J-1)*XXXX(J-2,0  ,0  ))
                  YYYY(J,0,0)= DAY*YYYY(J-1,0  ,0  )+A10*(
     *                         (J-1)*YYYY(J-2,0  ,0  ))
                  ZZZZ(J,0,0)= DAZ*ZZZZ(J-1,0  ,0  )+A10*(
     *                         (J-1)*ZZZZ(J-2,0  ,0  ))
 2000 CONTINUE
C              RECURSION BASED ON K
               DO 2010 K=1,KBTYPE
                DO 2010 J=0,KATYPE
                  XXXX(J,K,0)= DBX*XXXX(J  ,K-1,0  )+A10*(
     *                             J*XXXX(J-1,K-1,0  ) +
     *                         (K-1)*XXXX(J  ,K-2,0  ))
                  YYYY(J,K,0)= DBY*YYYY(J  ,K-1,0  )+A10*(
     *                             J*YYYY(J-1,K-1,0  ) +
     *                         (K-1)*YYYY(J  ,K-2,0  ))
                  ZZZZ(J,K,0)= DBZ*ZZZZ(J  ,K-1,0  )+A10*(
     *                             J*ZZZZ(J-1,K-1,0  ) +
     *                         (K-1)*ZZZZ(J  ,K-2,0  ))
 2010  CONTINUE
C              RECURSION BASED ON L
               DO 2020 L=1,KCTYPE
               DO 2020 K=0,KBTYPE
               DO 2020 J=0,KATYPE
                  XXXX(J,K,L)= DCX*XXXX(J  ,K  ,L-1)+A10*(
     *                             J*XXXX(J-1,K  ,L-1) +
     *                             K*XXXX(J  ,K-1,L-1) +
     *                         (L-1)*XXXX(J  ,K  ,L-2))
                  YYYY(J,K,L)= DCY*YYYY(J  ,K  ,L-1)+A10*(
     *                             J*YYYY(J-1,K  ,L-1) +
     *                             K*YYYY(J  ,K-1,L-1) +
     *                         (L-1)*YYYY(J  ,K  ,L-2))
                  ZZZZ(J,K,L)= DCZ*ZZZZ(J  ,K  ,L-1)+A10*(
     *                             J*ZZZZ(J-1,K  ,L-1) +
     *                             K*ZZZZ(J  ,K-1,L-1) +
     *                         (L-1)*ZZZZ(J  ,K  ,L-2))
 2020   CONTINUE
C
C   LOOP OVER SUBSHELLS
C
          IMOVE=0
          DO 120 IASUB=IAMIN,IAMAX
C          THE X COORDINATE IN S(NUM,NUM,NUM)
           IAPOSIT=LOCA+IASUB
           L1=LDAT(IASUB)
           M1=MDAT(IASUB)
           N1=NDAT(IASUB)
           ANORM=CONTA(IASUB)
           DO 121 IBSUB=IBMIN,IBMAX
C           THE Y COORDINATE IN S(NUM,NUM,NUM)
            IBPOSIT=LOCB+IBSUB
            IF(IBPOSIT .GT. IAPOSIT) GO TO 120
            L2=LDAT(IBSUB)
            M2=MDAT(IBSUB)
            N2=NDAT(IBSUB)
            ABNORM=CONTB(IBSUB)*ANORM
            DO 122 ICSUB=ICMIN,ICMAX
C            THE Z COORDINATE IN S(NUM,NUM,NUM)
             ICPOSIT=LOCC+ICSUB
             IF(ICPOSIT .GT. IBPOSIT) GO TO 121
             L3=LDAT(ICSUB)
             M3=MDAT(ICSUB)
             N3=NDAT(ICSUB)
             ABCNORM=CONTC(ICSUB)*ABNORM
             IMOVE=IMOVE+1
C
C   MULTIPLY IT ALL TOGETHER
C
             SHELL=ABCK*ABCNORM*XXXX(L1,L2,L3)*
     *               YYYY(M1,M2,M3)*ZZZZ(N1,N2,N3)
C
C   ADD THIS TO <AB|C>
C
             TEMPS(IMOVE)=TEMPS(IMOVE)+SHELL
  122       CONTINUE
  121      CONTINUE
  120     CONTINUE
  112    CONTINUE
  111   CONTINUE
  110  CONTINUE
C
C    DONE WITH SUBSHELLS AND GAUSSIANS.  STORE TEMPS INTO S
C
         IMOVE=0
         DO 421 IAPOSIT=IAMIN+LOCA,IAMAX+LOCA
C          MIN LETS US SKIP (561) SINCE WE DO (651)
           DO 422 IBPOSIT=IBMIN+LOCB,MIN(IBMAX+LOCB,IAPOSIT)
C
            DO 423 ICPOSIT=ICMIN+LOCC,MIN(ICMAX+LOCC,IBPOSIT)
C
              IMOVE=IMOVE+1
C             POSTSCREEN THE INTEGRALS
              IF(ABS(TEMPS(IMOVE)) .GT. CUTOFF) THEN
               IWHERE=IWHERE+1
C
               NPACK = IWHERE
               IPACK = IAPOSIT
               JPACK = IBPOSIT
               KPACK = ICPOSIT
               LPACK = 0
               IF (LABSIZ .EQ. 2) THEN
#if !defined(SPEC_CPU_ILP64)
                 LABEL1 = ISHFT( IPACK, 16 ) + JPACK
                 LABEL2 = ISHFT( KPACK, 16 ) + LPACK
                 IX( 2*NPACK-1 ) = LABEL1
                 IX( 2*NPACK   ) = LABEL2
#endif
#if defined(SPEC_CPU_ILP64)
                 LABEL = ISHFT( IPACK, 48 ) + ISHFT( JPACK, 32 ) +
     *                   ISHFT( KPACK, 16 ) + LPACK
                 IX(NPACK) = LABEL
#endif
               ELSE IF (LABSIZ .EQ. 1) THEN
#if !defined(SPEC_CPU_ILP64)
                 LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                   ISHFT( KPACK,  8 ) + LPACK
                 IX(NPACK) = LABEL
#endif
#if defined(SPEC_CPU_ILP64)
                 IF ( MOD(NPACK,2) .EQ. 0 ) THEN
                   LABEL = ISHFT( IPACK, 24 ) + ISHFT( JPACK, 16 ) +
     *                     ISHFT( KPACK,  8 ) + LPACK
                   IX( NPACK/2 ) = IX( NPACK/2 ) + LABEL
                 ELSE
                   LABEL = ISHFT( IPACK, 56 ) + ISHFT( JPACK, 48 ) +
     *                     ISHFT( KPACK, 40 ) + ISHFT( LPACK, 32 )
                   IX( (NPACK/2)+1 ) = LABEL
                 END IF
#endif
               END IF
C
              S(IWHERE)=TEMPS(IMOVE)
C
C             WRITE OUT INTEGRAL BUFFER IF FILLED
C
              IF(IWHERE .EQ. NINTMX) THEN
                CALL PWRIT(21,S,IX,NINTMX,NINTMX)
                IWHERE=0
                NREC=NREC+1
                NINTS=NINTS+NINTMX
              END IF
C
            END IF
  423    CONTINUE
  422   CONTINUE
  421  CONTINUE
C
  102 CONTINUE
  101 CONTINUE
  100 CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
C     WRITE FINAL PARTIAL BUFFER LOAD OF INTEGRALS TO DISK
C
      NREC=NREC+1
      NINTS=NINTS+IWHERE
      IWHERE=-IWHERE
      CALL PWRIT(21,S,IX,IWHERE,NINTMX)
      IF(GOPARR) THEN
         CALL DDI_GSUMI(1056,NINTS,1)
         CALL DDI_GSUMI(1057,NRECS,1)
      END IF
      IF(SOME) WRITE(IW,9010) NINTS,NREC,21
      CALL TSECND(TIM)
      TIM = TIM - TIM0
      IF(SOME) WRITE(IW,9030) TIM
C
      DSKWRK = SVDSKW
      RETURN
C
 9000 FORMAT(1X,'COMPUTING 3 CENTER OVERLAP INTEGRALS (IJK) TO',
     *          ' APPROXIMATE (IJKL)')
 9010 FORMAT(1X,'TOTAL NUMBER OF NONZERO OVERLAP INTEGRALS=',I20/
     *       1X,I10,' INTEGRAL RECORDS WERE STORED ON DISK FILE',I3,'.')
 9030 FORMAT(1X,'TIME TO EVALUATE (IJK) OVERLAP INTEGRALS      =',F12.1)
      END
C*MODULE DFTINT  *DECK THRDIR
      SUBROUTINE THRDIR(TEMPS,HISPIN,FIRST,R,ROPEN,ADENS,BDENS,
     *                  DD,DDVV,DOPVV,DOP)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL DSKWRK,MASWRK,GOPARR,SOME,HISPIN,FIRST
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
      DIMENSION TEMPS(15*15*15),R(NUM,NUM),ROPEN(NUM,NUM),
     *          ADENS(NUM,NUM),BDENS(NUM,NUM),
     *          DD(NUM),DDVV(NUM),DOPVV(NUM),DOP(NUM)
      DIMENSION CONTA(35),CONTB(35),CONTC(35),
     *          LDAT(35),MDAT(35),NDAT(35),
     *          XXXX(-1:4,-1:4,-1:4),
     *          YYYY(-1:4,-1:4,-1:4),
     *          ZZZZ(-1:4,-1:4,-1:4)
C
      PARAMETER (RLN10=2.30258D+00)
      PARAMETER (PI=3.141592653589793238462643D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DEBUG/8HDEBUG   /
#else
      CHARACTER*8 :: DEBUG_STR
      EQUIVALENCE (DEBUG, DEBUG_STR)
      DATA DEBUG_STR/"DEBUG   "/
#endif
C
C  X,Y,Z EXPONENTS OF CARTESIAN S,PX,PY,PZ,DXX,...
C
      DATA LDAT/0,1,0,0,2,0,0,1,1,0,3,0,0,2,2,1,0,1,0,1,4,0,0,3,3,
     *          1,0,1,0,2,2,0,2,1,1/
      DATA MDAT/0,0,1,0,0,2,0,1,0,1,0,3,0,1,0,2,2,0,1,1,0,4,0,1,0,
     *          3,3,0,1,2,0,2,1,2,1/
      DATA NDAT/0,0,0,1,0,0,2,0,1,1,0,0,3,0,1,0,1,2,2,1,0,0,4,0,1,
     *          0,1,3,3,0,2,2,1,1,2/
C
C    THIS IS THE DIRECT VERSION OF --THREEC--
C
      IF ((NORMF .EQ. 1 .OR. NORMP .EQ. 1).AND. MASWRK) THEN
        WRITE(IW,*)' SORRY BUT THE THREE CENTER ONE ELECTRON OVELAP'
        WRITE(IW,*)' INTEGRALS USED FOR DFT DO NOT SUPPORT THE'
        WRITE(IW,*)' NO NORMALIZATION OPTIONS'
        CALL ABRT
      END IF
C
C     ----- INITIALIZATION FOR PARALLEL WORK -----
C
      IPCOUNT = ME - 1
C
      SOME = (EXETYP .EQ. DEBUG) .AND. MASWRK
C
C    INITIALIZE
C
      IF(SOME) THEN
         WRITE(IW,*)' DOING (JKL) INTEGRALS TO APPROXIMATE (IJKL)'
      END IF
      TOL=ITOL*RLN10
      CUTOFF = 1.0D+00/(10.0D+00**ICUT)
      DO 10 K=-1,0
      DO 10 J=-1,0
      DO 10 I=-1,0
      XXXX(I,J,K)=0.0D+00
      YYYY(I,J,K)=0.0D+00
      ZZZZ(I,J,K)=0.0D+00
 10   CONTINUE
      XXXX(0,0,0)=1.0D+00
      YYYY(0,0,0)=1.0D+00
      ZZZZ(0,0,0)=1.0D+00
C
C    LOOP OVER SHELLS A B AND C FOR <AB|C>
C
      DO 100 IASHL=1,NSHELL
C  POSITION OF THE SHELL'S ATOM
      IAATOM=KATOM(IASHL)
      AX=C(1,IAATOM)
      AY=C(2,IAATOM)
      AZ=C(3,IAATOM)
      AA=AX*AX+AY*AY+AZ*AZ
C  INFO ABOUT THE PRIMATIVES
      ISTARTA=KSTART(IASHL)
      IENDA=ISTARTA+KNG(IASHL)-1
C  INFO ABOUT THE SUBSHELLS
      IAMAX=KMAX(IASHL)
      IAMIN=KMIN(IASHL)
      LOCA =KLOC(IASHL)-IAMIN
C  FIND THE HIGHEST SPIN VALUE IN THIS SHELL
      KATYPE=KTYPE(IASHL)-1
C
      DO 101 IBSHL=1,IASHL
      IBATOM=KATOM(IBSHL)
      BX=C(1,IBATOM)
      BY=C(2,IBATOM)
      BZ=C(3,IBATOM)
      BB=BX*BX+BY*BY+BZ*BZ
      ISTARTB=KSTART(IBSHL)
      IENDB=ISTARTB+KNG(IBSHL)-1
      IBMAX=KMAX(IBSHL)
      IBMIN=KMIN(IBSHL)
      LOCB = KLOC(IBSHL)-IBMIN
      ABRR=(AX-BX)*(AX-BX)+(AY-BY)*(AY-BY)+(AZ-BZ)*(AZ-BZ)
      KBTYPE=KTYPE(IBSHL)-1
C
      DO 102 ICSHL=1,IBSHL
C
C     ----- GO PARALLEL! -----
C
      IF (GOPARR) THEN
         IPCOUNT = IPCOUNT + 1
         IF (MOD(IPCOUNT,NPROC).NE.0) GO TO 102
      END IF
C
      ICATOM=KATOM(ICSHL)
      CX=C(1,ICATOM)
      CY=C(2,ICATOM)
      CZ=C(3,ICATOM)
      CC=CX*CX+CY*CY+CZ*CZ
      ISTARTC=KSTART(ICSHL)
      IENDC=ISTARTC+KNG(ICSHL)-1
      ICMAX=KMAX(ICSHL)
      ICMIN=KMIN(ICSHL)
      LOCC = KLOC(ICSHL)-ICMIN
      ACRR=(AX-CX)*(AX-CX)+(AY-CY)*(AY-CY)+(AZ-CZ)*(AZ-CZ)
      BCRR=(BX-CX)*(BX-CX)+(BY-CY)*(BY-CY)+(BZ-CZ)*(BZ-CZ)
      KCTYPE=KTYPE(ICSHL)-1
C
C    LOOP OVER GUASSIAN PRIMATIVES WHILE READING
C          IN EXPONENTS AND CONTRACTIONS
C
      CALL VCLR(TEMPS,1,15*15*15)
       DO 110 IAPRM=ISTARTA,IENDA
        A1=EX(IAPRM)
        CALL FORCNT(IAPRM,IAMAX,CONTA)
        DO 111 IBPRM=ISTARTB,IENDB
         A2=EX(IBPRM)
         CALL FORCNT(IBPRM,IBMAX,CONTB)
         IF(ABRR*(A1*A2/(A1+A2)) .GT. TOL) GO TO 111
         DO 112 ICPRM=ISTARTC,IENDC
          A3=EX(ICPRM)
          CALL FORCNT(ICPRM,ICMAX,CONTC)
          IF(ACRR*A1*A3/(A1+A3) .GT. TOL .OR.
     *       BCRR*A2*A3/(A2+A3) .GT. TOL) GO TO 112
C
C   CALCULATE FINAL GAUSSIAN CENTER D
C
          A4=A1+A2+A3
          A4INV=1.0D+00/A4
          A10=0.5D+00*A4INV
          DX=(A1*AX+A2*BX+A3*CX)*A4INV
          DY=(A1*AY+A2*BY+A3*CY)*A4INV
          DZ=(A1*AZ+A2*BZ+A3*CZ)*A4INV
          DBX=DX-BX
          DBY=DY-BY
          DBZ=DZ-BZ
          DCX=DX-CX
          DCY=DY-CY
          DCZ=DZ-CZ
          DAX=DX-AX
          DAY=DY-AY
          DAZ=DZ-AZ
C
C   CALCULATE KABC
C
           ABCK=((PI*A4INV)*SQRT(PI*A4INV))*
     *          EXP(((A1*AX+A2*BX+A3*CX)*(A1*AX+A2*BX+A3*CX)+
     *               (A1*AY+A2*BY+A3*CY)*(A1*AY+A2*BY+A3*CY)+
     *               (A1*AZ+A2*BZ+A3*CZ)*(A1*AZ+A2*BZ+A3*CZ))*A4INV
     *              -(A1*AA+A2*BB+A3*CC))
C
C   USE RECURSION TO GENERATE HIGHER ANGULAR MOMENTUM XXXX,ETC
C
C              RECURSION BASED ON J
               DO 2000 J=1,KATYPE
                  XXXX(J,0,0)= DAX*XXXX(J-1,0  ,0  )+A10*(
     *                         (J-1)*XXXX(J-2,0  ,0  ))
                  YYYY(J,0,0)= DAY*YYYY(J-1,0  ,0  )+A10*(
     *                         (J-1)*YYYY(J-2,0  ,0  ))
                  ZZZZ(J,0,0)= DAZ*ZZZZ(J-1,0  ,0  )+A10*(
     *                         (J-1)*ZZZZ(J-2,0  ,0  ))
 2000 CONTINUE
C              RECURSION BASED ON K
               DO 2010 K=1,KBTYPE
                DO 2010 J=0,KATYPE
                  XXXX(J,K,0)= DBX*XXXX(J  ,K-1,0  )+A10*(
     *                             J*XXXX(J-1,K-1,0  ) +
     *                         (K-1)*XXXX(J  ,K-2,0  ))
                  YYYY(J,K,0)= DBY*YYYY(J  ,K-1,0  )+A10*(
     *                             J*YYYY(J-1,K-1,0  ) +
     *                         (K-1)*YYYY(J  ,K-2,0  ))
                  ZZZZ(J,K,0)= DBZ*ZZZZ(J  ,K-1,0  )+A10*(
     *                             J*ZZZZ(J-1,K-1,0  ) +
     *                         (K-1)*ZZZZ(J  ,K-2,0  ))
 2010  CONTINUE
C              RECURSION BASED ON L
               DO 2020 L=1,KCTYPE
               DO 2020 K=0,KBTYPE
               DO 2020 J=0,KATYPE
                  XXXX(J,K,L)= DCX*XXXX(J  ,K  ,L-1)+A10*(
     *                             J*XXXX(J-1,K  ,L-1) +
     *                             K*XXXX(J  ,K-1,L-1) +
     *                         (L-1)*XXXX(J  ,K  ,L-2))
                  YYYY(J,K,L)= DCY*YYYY(J  ,K  ,L-1)+A10*(
     *                             J*YYYY(J-1,K  ,L-1) +
     *                             K*YYYY(J  ,K-1,L-1) +
     *                         (L-1)*YYYY(J  ,K  ,L-2))
                  ZZZZ(J,K,L)= DCZ*ZZZZ(J  ,K  ,L-1)+A10*(
     *                             J*ZZZZ(J-1,K  ,L-1) +
     *                             K*ZZZZ(J  ,K-1,L-1) +
     *                         (L-1)*ZZZZ(J  ,K  ,L-2))
 2020   CONTINUE
C
C   LOOP OVER SUBSHELLS
C
          IMOVE=0
          DO 120 IASUB=IAMIN,IAMAX
C          THE X COORDINATE IN S(NUM,NUM,NUM)
           IAPOSIT=LOCA+IASUB
           L1=LDAT(IASUB)
           M1=MDAT(IASUB)
           N1=NDAT(IASUB)
           ANORM=CONTA(IASUB)
           DO 121 IBSUB=IBMIN,IBMAX
C           THE Y COORDINATE IN S(NUM,NUM,NUM)
            IBPOSIT=LOCB+IBSUB
            IF(IBPOSIT .GT. IAPOSIT) GO TO 120
            L2=LDAT(IBSUB)
            M2=MDAT(IBSUB)
            N2=NDAT(IBSUB)
            ABNORM=CONTB(IBSUB)*ANORM
            DO 122 ICSUB=ICMIN,ICMAX
C            THE Z COORDINATE IN S(NUM,NUM,NUM)
             ICPOSIT=LOCC+ICSUB
             IF(ICPOSIT .GT. IBPOSIT) GO TO 121
             L3=LDAT(ICSUB)
             M3=MDAT(ICSUB)
             N3=NDAT(ICSUB)
             ABCNORM=CONTC(ICSUB)*ABNORM
             IMOVE=IMOVE+1
C
C   MULTIPLY IT ALL TOGETHER
C
             SHELL=ABCK*ABCNORM*XXXX(L1,L2,L3)*
     *               YYYY(M1,M2,M3)*ZZZZ(N1,N2,N3)
C
C   ADD THIS TO <ABC>
C
             TEMPS(IMOVE)=TEMPS(IMOVE)+SHELL
  122       CONTINUE
  121      CONTINUE
  120     CONTINUE
  112    CONTINUE
  111   CONTINUE
  110  CONTINUE
C
C    DONE WITH SUBSHELLS AND GAUSSIANS.
C
         IMOVE=0
         DO 421 K=IAMIN+LOCA,IAMAX+LOCA
C          MIN LETS US SKIP (561) SINCE WE DO (651)
           DO 422 L=IBMIN+LOCB,MIN(IBMAX+LOCB,K)
C
            DO 423 N=ICMIN+LOCC,MIN(ICMAX+LOCC,L)
C
              IMOVE=IMOVE+1
C             POSTSCREEN THE INTEGRALS
              IF(ABS(TEMPS(IMOVE)) .GT. CUTOFF) THEN
                A3OVKLN=TEMPS(IMOVE)
C
C     LOW-SPIN FIRST TIME
C
      IF((.NOT. HISPIN) .AND. FIRST) THEN
C
         IF(K .NE. L .AND. L .NE. N) THEN
C            (KLN)
             DD(K)=DD(K)+A3OVKLN*ADENS(L,N)
             DD(L)=DD(L)+A3OVKLN*ADENS(K,N)
             DD(N)=DD(N)+A3OVKLN*ADENS(K,L)
         ELSE IF(K .NE. L) THEN
C            (KLL)
             DD(K)=DD(K)+A3OVKLN*ADENS(L,L)
             DD(L)=DD(L)+A3OVKLN*ADENS(K,L)
         ELSE IF(L .NE. N) THEN
C            (KKN)
             DD(K)=DD(K)+A3OVKLN*ADENS(K,N)
             DD(N)=DD(N)+A3OVKLN*ADENS(K,K)
         ELSE
C            (KKK)
             DD(K)=DD(K)+A3OVKLN*ADENS(K,K)
         END IF
C
C     LOW-SPIN SECOND TIME
C
      ELSE IF((.NOT. HISPIN) .AND. (.NOT. FIRST)) THEN
C
         IF(K .NE. L .AND. L .NE. N) THEN
C            (KLN)
             R(K,L)=R(K,L)+A3OVKLN*DDVV(N)
             R(K,N)=R(K,N)+A3OVKLN*DDVV(L)
             R(L,N)=R(L,N)+A3OVKLN*DDVV(K)
         ELSE IF(K .NE. L) THEN
C            (KLL)
             R(K,L)=R(K,L)+A3OVKLN*DDVV(L)
             R(L,L)=R(L,L)+A3OVKLN*DDVV(K)
         ELSE IF(L .NE. N) THEN
C            (KKN)
             R(K,K)=R(K,K)+A3OVKLN*DDVV(N)
             R(K,N)=R(K,N)+A3OVKLN*DDVV(K)
         ELSE
C            (KKK)
             R(K,K)=R(K,K)+A3OVKLN*DDVV(K)
         END IF
C
C     HIGH-SPIN FIRST TIME
C
      ELSE IF(HISPIN .AND. FIRST) THEN
C
         IF(K .NE. L .AND. L .NE. N) THEN
C            (KLN)
             DD(K)=DD(K)+A3OVKLN*ADENS(L,N)
             DD(L)=DD(L)+A3OVKLN*ADENS(K,N)
             DD(N)=DD(N)+A3OVKLN*ADENS(K,L)
             DOP(K)=DOP(K)+A3OVKLN*BDENS(L,N)
             DOP(L)=DOP(L)+A3OVKLN*BDENS(K,N)
             DOP(N)=DOP(N)+A3OVKLN*BDENS(K,L)
         ELSE IF(K .NE. L) THEN
C            (KLL)
             DD(K)=DD(K)+A3OVKLN*ADENS(L,L)
             DD(L)=DD(L)+A3OVKLN*ADENS(K,L)
             DOP(K)=DOP(K)+A3OVKLN*BDENS(L,L)
             DOP(L)=DOP(L)+A3OVKLN*BDENS(K,L)
         ELSE IF(L .NE. N) THEN
C            (KKN)
             DD(K)=DD(K)+A3OVKLN*ADENS(K,N)
             DD(N)=DD(N)+A3OVKLN*ADENS(K,K)
             DOP(K)=DOP(K)+A3OVKLN*BDENS(K,N)
             DOP(N)=DOP(N)+A3OVKLN*BDENS(K,K)
         ELSE
C            (KKK)
             DD(K)=DD(K)+A3OVKLN*ADENS(K,K)
             DOP(K)=DOP(K)+A3OVKLN*BDENS(K,K)
         END IF
C
C     HIGH-SPIN SECOND TIME
C
      ELSE
C
         IF(K .NE. L .AND. L .NE. N) THEN
C            (KLN)
             R(K,L)=R(K,L)+A3OVKLN*DDVV(N)
             R(K,N)=R(K,N)+A3OVKLN*DDVV(L)
             R(L,N)=R(L,N)+A3OVKLN*DDVV(K)
             ROPEN(K,L)=ROPEN(K,L)+A3OVKLN*DOPVV(N)
             ROPEN(K,N)=ROPEN(K,N)+A3OVKLN*DOPVV(L)
             ROPEN(L,N)=ROPEN(L,N)+A3OVKLN*DOPVV(K)
         ELSE IF(K .NE. L) THEN
C            (KLL)
             R(K,L)=R(K,L)+A3OVKLN*DDVV(L)
             R(L,L)=R(L,L)+A3OVKLN*DDVV(K)
             ROPEN(K,L)=ROPEN(K,L)+A3OVKLN*DOPVV(L)
             ROPEN(L,L)=ROPEN(L,L)+A3OVKLN*DOPVV(K)
         ELSE IF(L .NE. N) THEN
C            (KKN)
             R(K,K)=R(K,K)+A3OVKLN*DDVV(N)
             R(K,N)=R(K,N)+A3OVKLN*DDVV(K)
             ROPEN(K,K)=ROPEN(K,K)+A3OVKLN*DOPVV(N)
             ROPEN(K,N)=ROPEN(K,N)+A3OVKLN*DOPVV(K)
         ELSE
C            (KKK)
             R(K,K)=R(K,K)+A3OVKLN*DDVV(K)
             ROPEN(K,K)=ROPEN(K,K)+A3OVKLN*DOPVV(K)
         END IF
C
      END IF
C
              END IF
  423    CONTINUE
  422   CONTINUE
  421  CONTINUE
C
C
  102 CONTINUE
  101 CONTINUE
  100 CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
      RETURN
      END
