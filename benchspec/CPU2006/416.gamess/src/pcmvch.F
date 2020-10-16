C  8 Oct 01 - HL  - parallelize PCM
C  6 Sep 01 - MWS - add dummy arguments to nameio call
C 29 Dec 00 - MWS - preceed all stops by abrt calls
C 11 Oct 00 - PB  - Changed COMMON BLOCK PCMPAR and PCMCHG
C 26 MAR 97 - BM  - POTREP: CORRECT FACTOR FOR CHARGED SOLUTE
C 17 MAR 97 - PISA - NEW MODULE TO DEFINE ESCAPED CHARGE CAVITY
C          affects the electronic induced charge (B. Mennucci)
C          and computes repulsion term (C. Amovilli and B. Mennucci)
C
C*MODULE PCMVCH  *DECK CHGOUT
      SUBROUTINE CHGOUT(XCTS,YCTS,ZCTS,XCTS1,YCTS1,ZCTS1,
     *                  AS,VEC,SCR,CEL,NUM2,NTS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION XCTS(NTS),YCTS(NTS),ZCTS(NTS),
     *          XCTS1(NTS),YCTS1(NTS),ZCTS1(NTS),
     *          AS(NTS),VEC(*),SCR(3,225),CEL(NUM2)
C
      COMMON /ELPROP/ ELDLOC,ELMLOC,ELPLOC,ELFLOC,
     *                IEDEN,IEMOM,IEPOT,IEFLD,MODENS,
     *                IEDOUT,IEMOUT,IEPOUT,IEFOUT,
     *                IEDINT,IEMINT,IEPINT,IEFINT
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /XYZPRP/ XP,YP,ZP,DMY(35)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA ELFLD  /8HELFLD   /
#else
      CHARACTER*8 :: ELFLD_STR
      EQUIVALENCE (ELFLD, ELFLD_STR)
      DATA ELFLD_STR/"ELFLD   "/
#endif
C
      CALL VCLR(CEL,1,NUM2)
      IEFLDOLD=IEFLD
      IEFLD=1
C
C  1) Calculation of < chi(mu) | alpha/r | chi(nu) >
C     (elect. field components) integrals on representative
C     points of tesserae.
C
      DO ITS = 1, NTS
        XP=XCTS(ITS)
        YP=YCTS(ITS)
        ZP=ZCTS(ITS)
        CNX=(XP-XCTS1(ITS))/DR
        CNY=(YP-YCTS1(ITS))/DR
        CNZ=(ZP-ZCTS1(ITS))/DR
        CALL PRCALC(ELFLD,VEC,SCR,3,NUM2)
        DO IBAS = 1, NUM2
          FLDN=VEC(IBAS       )*CNX
     *       + VEC(IBAS+  NUM2)*CNY
     *       + VEC(IBAS+2*NUM2)*CNZ
          CEL(IBAS) = CEL(IBAS) + FLDN * AS(ITS)
        ENDDO
      ENDDO
      IEFLD=IEFLDOLD
C
      CALL DAWRIT(IDAF,IODA,CEL,NUM2,330,0)
      RETURN
      END
C*MODULE PCMVCH  *DECK SFUG
      SUBROUTINE SFUG(D,CEL,CSF,DNEL,NUM2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION D(NUM2),CEL(NUM2)
C
      PARAMETER (MXATM=500, MXTS=2500)
C
C   hui li
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
C   hui li
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
C
      DATA PI/3.141592653589793D+00/
C
      FPI=4.0D+00*PI
      CALL DAREAD(IDAF,IODA,CEL,NUM2,330,0)
C
      DNE=NE
      CSF=TRACEP(D,CEL,NUM)
      DD=QNUC/(DNE+ICH)
      CSF=CSF/(DD*FPI)
      IF(ABS(DNEL).GT.1.0D-07) THEN
         IF(NPRINT.EQ.5 .AND. MASWRK) WRITE(IW,102) DD,CSF,DNE+CSF
         CSF=DNE+CSF
      END IF
      RETURN
C
 102  FORMAT(1X,'FACTOR ',F6.4,', INT CHG',1X,F8.4,', ESC CHG',1X,F8.4)
      END
C*MODULE PCMVCH  *DECK XMATEF
      SUBROUTINE XMATEF(D,XX,Q,SCR,DMATM1,CQEF,QEFF,ELD,
     *                  QETOT,TCH,QESC,QETOTN,L2,NTSX,CSF,NFT27)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
      DIMENSION D(L2),XX(L2),Q(NTSX),SCR(L2),DMATM1(NTSX,NTSX),
     *          CQEF(NTSX),QEFF(NTSX),ELD(NTSX)
      DIMENSION WORK(225)
C
C   hui li
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /ELPROP/ ELDLOC,ELMLOC,ELPLOC,ELFLOC,
     *                IEDEN,IEMOM,IEPOT,IEFLD,MODENS,
     *                IEDOUT,IEMOUT,IEPOUT,IEFOUT,
     *                IEDINT,IEMINT,IEPINT,IEFINT
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
C   hui li
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
      COMMON /PCMUGG/ CSF0,SUMEL,CSF1(3),SUMEL1(3),FE1(3)
      COMMON /XYZPRP/ XP,YP,ZP,DMY(35)
C
      DATA ZERO, ONE, TWO  /0.0D+00, 1.0D+00, 2.0D+00/
      DATA PI/3.141592653589793D+00/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA ELDEN/8HELDEN   /
#else
      CHARACTER*8 :: ELDEN_STR
      EQUIVALENCE (ELDEN, ELDEN_STR)
      DATA ELDEN_STR/"ELDEN   "/
#endif
C
C     Routine per il calcolo della matrice XX (interazione tra le
C     cariche virtuali elettroniche e il potenziale elettronico del
C     soluto):
C         XX(m,n) = sum_i[ V(m,n;i)QSE(i) ]
C         QSE(i) = a(i)sum_j[ DMATM1(i,j){sum_m,n[ D(m,n)dV(m,n;i)dn ]}
C     che viene usata nella routine SCF per la correzione degli
C     integrali bielettronici.
C
      FPI=4.0D+00*PI
      TPI=TWO*PI
      COEF=(EPS-ONE)/EPS
      NUM2 = NUM*(NUM+1)/2
C
C        Disk file NFT27 contains the electric field integrals.
C
      CALL SEQREW(NFT27)
C
      DO ITS = 1, NTS
         QEFF(ITS)=ZERO
         CALL SQREAD(NFT27,SCR,NUM2)
         Q(ITS) = TRACEP(D,SCR,NUM)
      ENDDO
C
      CALL SEQREW(NFT27)
C
C     ----- CALCULATE ELECTRIC DENSITY AT ALL POINTS -----
C
       IEDENOLD=IEDEN
       IEDEN=1
       NVAL = 1
       SUMEL=ZERO
       DO 30 INR=1,NTS
        XP=XCTS(INR)
        YP=YCTS(INR)
        ZP=ZCTS(INR)
        CALL PRCALC(ELDEN,XX,WORK,NVAL,NUM2)
        ELD(INR) = TRACEP(D,XX,NUM)
        SUMEL=SUMEL+ELD(INR)*AS(INR)
   30  CONTINUE
C
      QESF=ZERO
      DO ITS = 1, NTS
       CQEF(ITS)=ZERO
       QEFF(ITS)=COEF*ELD(ITS)*CSF/SUMEL
       QESF=QESF+QEFF(ITS)*AS(ITS)
      ENDDO
C
      DO ITS = 1, NTS
       L=ISPHE(ITS)
       ETA=SQRT(AS(ITS)/(FPI*RE(L)**2))
       DI= - TPI*(ONE-ETA)*QEFF(ITS)
       K=ITS+NTS
       XI=XCTS(ITS)
       YI=YCTS(ITS)
       ZI=ZCTS(ITS)
       CX=(XCTS(K)-XCTS(ITS))/DR
       CY=(YCTS(K)-YCTS(ITS))/DR
       CZ=(ZCTS(K)-ZCTS(ITS))/DR
       DO 150 JTS=1,NTS
       IF(JTS.EQ.ITS) GO TO 150
        XJ=XCTS(JTS)
        YJ=YCTS(JTS)
        ZJ=ZCTS(JTS)
        AVF=AS(JTS)
       RIJ=SQRT((XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2)
       IF(RIJ.LT.0.026D+00) GO TO 150
       DI=DI-AVF*QEFF(JTS)*((XI-XJ)*CX+
     *                  (YI-YJ)*CY+(ZI-ZJ)*CZ)/RIJ**3
  150 CONTINUE
      CQEF(ITS)=DI
      ENDDO
C
 333  CONTINUE
      QETOT = ZERO
      QT0 = ZERO
      DO ITS = 1, NTS
       QSE(ITS) = ZERO
       Q0=ZERO
       DO IJ = 1, NTS
        QSE(ITS)=QSE(ITS)+DMATM1(ITS,IJ)*(Q(IJ)+CQEF(IJ))
        Q0=Q0+DMATM1(ITS,IJ)*Q(IJ)
       ENDDO
       QSE(ITS) = QSE(ITS)*AS(ITS)
       QETOT = QETOT + QSE(ITS)
       QT0 = QT0 + Q0*AS(ITS)
      ENDDO
      CSF0=CSF
C
C  4) Normalizzazione delle cariche virtuali elementari elettroniche.
C     La correzione e' effettuata con fattori costanti.
C
      NELEC = NE
      TCH = (NELEC-CSF0) * (EPS - ONE) / EPS
      QETOTN = ZERO
        FE = TCH / QETOT
        DO ITS = 1, NTS
          QSE(ITS) = QSE(ITS) * FE
          QETOTN = QETOTN + QSE(ITS)
        ENDDO
C
C     Stampa i valori della carica prima e dopo la normalizzazione
C
      QESC=-QESF/COEF
      IF(NPRINT.EQ.5 .AND. MASWRK) WRITE(IW,1000) QETOT,TCH,QESC,QETOTN
C
      CALL VCLR(XX,1,NUM2)
C
C     Calcola X, PB, PC, PX
C
      PB = ZERO
      PC = ZERO
      PX = ZERO
      DO ITS = 1, NTS
        QQ=QSE(ITS)+QEFF(ITS)*AS(ITS)
        CALL INTMEP(SCR,XCTS(ITS),YCTS(ITS),ZCTS(ITS))
C
C       Calcola gli elementi della matrice X
C
        DO IBAS = 1, NUM2
          XX(IBAS) = XX(IBAS) - SCR(IBAS) * QQ
        ENDDO
C
C       Calcola PB = interazione elettroni-cariche nucleari e
C               PX = interazione elettroni-cariche elettroniche
        VEL = - TRACEP(D,SCR,NUM)
        PB = PB + VEL * QSN(ITS)
        PX = PX + VEL * QQ
C
C       Calcola PC = interazione nuclei-cariche elettroniche
C
        DO JATOM = 1, NAT
          R2 = (C(1,JATOM)-XCTS(ITS))**2 + (C(2,JATOM)-
     *         YCTS(ITS))**2 + (C(3,JATOM)-ZCTS(ITS))**2
          R = SQRT(R2)
           PC = PC + QQ * ZAN(JATOM) / R
        ENDDO
      ENDDO
      IEDEN=IEDENOLD
      RETURN
C
 1000 FORMAT(1X,'INDUCED ELECT.CHARGE',F10.5,' (',
     * F10.5,')  ESCAPED',F10.5,' FINAL', F10.5)
      END
C*MODULE PCMVCH  *DECK POTREP
      SUBROUTINE POTREP(XX,SCR,CEL,NUM2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION XX(NUM2),SCR(NUM2),CEL(NUM2)
C
      PARAMETER (MXATM=500, MXTS=2500)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMREP/ RHOW,PM,NEVAL
C
      DATA PI/3.141592653589793D+00/
C
      FPI=4.0D+00*PI
      FACT=0.063D+00*RHOW*NEVAL/PM
C
      DNE= NE
      CH = ICH
      FNEL=QNUC/(DNE+CH)
C
      CALL DAREAD(IDAF,IODA,CEL,NUM2,330,0)
C
      DO IBAS = 1, NUM2
         FF=FACT*(CEL(IBAS)/FPI +FNEL*SCR(IBAS))
         XX(IBAS) = FF
      ENDDO
      RETURN
      END
C*MODULE PCMVCH  *DECK MORETS
      SUBROUTINE MORETS(CV,JTR,OLDTR,EDNEW,INTSPH,X,Y,Z,X1,Y1,Z1,
     *                  AS,SCR,SCR2,CEL,L2,MXTSEC,MXVERT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      INTEGER OLDTR(100,100),EDNEW(90,100)
      DIMENSION CV(MXVERT,3),JTR(MXTSEC,3),INTSPH(MXTSEC,10),X(MXTSEC),
     *          Y(MXTSEC),Z(MXTSEC),X1(MXTSEC),Y1(MXTSEC),Z1(MXTSEC),
     *          AS(MXTSEC),SCR(L2,3),SCR2(225,3),CEL(L2)
      DIMENSION PP(3),PP1(3),PTS(3,10),CCC(3,10)
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMPAR/ IPCM,NFT26,NFT27,IKREP,IEF,IP_F
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMREP/ RHOW,PM,NEVAL
      COMMON /PCMTES/ CCX,CCY,CCZ,XCT(MXTSPT),YCT(MXTSPT),
     *                ZCT(MXTSPT),AT(MXTS),RDIF,NVERT(MXTS),NTESS
C
      PARAMETER (TOANGS=0.52917724924D+00, ANTOAU=1.0D+00/TOANGS)
C
      PARAMETER (NNAM=5)
      DIMENSION QNAM(NNAM),KQNAM(NNAM)
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CAVWD/8HNEWCAV  /
#else
      CHARACTER*8 :: CAVWD_STR
      EQUIVALENCE (CAVWD, CAVWD_STR)
      DATA CAVWD_STR/"NEWCAV  "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QNAM/8HIPTYPE  ,8HITSNUM  ,
     *          8HRHOW    ,8HPM      ,8HNEVAL   /
#else
      CHARACTER*8 :: QNAM_STR(NNAM) 
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"IPTYPE  ","ITSNUM  ",
     *          "RHOW    ","PM      ","NEVAL   "/
#endif
      DATA KQNAM/1,1,3,3,1/
C
      DATA PI/3.141592653589793D+00/
      DATA ZERO/0.0D+00/
C
C     ----- read the $NEWCAV input group -----
C
C-- new cavity parameters (same as traditional GEPOL)
C
      IPTYPE = 2
      ITSNUM = 60
C
C-- repulsion parameters (solvent: water)
C
      RHOW=1.0D+00
      PM=18.0D+00
      NEVAL=8
C
      JRET=0
      CALL NAMEIO(IR,JRET,CAVWD,NNAM,QNAM,KQNAM,
     *            IPTYPE,ITSNUM,RHOW,PM,NEVAL,
     *            0,0,0,0,   0,0,0,0,0,
     *     0,0,0,0,0,   0,0,0,0,0,
     *     0,0,0,0,0,   0,0,0,0,0,   0,0,0,0,0,   0,0,0,0,0,
     *     0,0,0,0,0,   0,0,0,0,0,   0,0,0,0,0,   0,0,0,0,0)
      IF(JRET.EQ.2) THEN
         IF(MASWRK) WRITE(IW,930) CAVWD
         CALL ABRT
      END IF
C
      IF(JRET.EQ.0.AND.MASWRK.AND.IKREP.EQ.1)
     *   WRITE(IW,950) RHOW,PM,NEVAL
C
  930 FORMAT(1X,'**** ERROR IN $',A8,' INPUT')
  950 FORMAT(/5X,'INPUT FOR PCM REPULSION CALCULATION '/5X,35("-")/
     *   5X,'RHOW =',F8.4,5X,'PM =',F8.4,5X,'NEVAL =',I8/)
C
C----------------- START CALCULATION -----------------
C
      IF(MASWRK)WRITE(IW,800)
C
      FPI=4.0D+00*PI
C
      CALL VCLR(CV,1,3*MXVERT)
C
C  This routine works in ANGSTROM, only at the end data are
C  reconverted in bohr.
C
      DO I=1,NESF
         RE(I)=RE(I)/ANTOAU
      ENDDO
      DO I=1,NESF
         XE(I)=XE(I)/ANTOAU
         YE(I)=YE(I)/ANTOAU
         ZE(I)=ZE(I)/ANTOAU
      ENDDO
C
      CCX = CCX / ANTOAU
      CCY = CCY / ANTOAU
      CCZ = CCZ / ANTOAU
      DR = DR / ANTOAU
C*********************************************************************
C     Division of the surface in tesserae
C*********************************************************************
C
      IPTYPE0 = IPTYPE
      ITSNUM0 = ITSNUM
      NN = 0
      DO 300 NSFE = 1, NESF
      XEN = XE(NSFE)
      YEN = YE(NSFE)
      ZEN = ZE(NSFE)
      REN = RE(NSFE)
C--------------------------------------------------------------------
      IPTYPE = IPTYPE0
      ITSNUM = ITSNUM0
      CALL POLYGEN(IPTYPE,ITSNUM,XEN,YEN,ZEN,REN,ITSEFF,
     *             CV,JTR,OLDTR,EDNEW,MXTSEC,MXVERT,IW)
C
      DO 310 ITS = 1, ITSEFF
         N1 = JTR(ITS,1)
         N2 = JTR(ITS,2)
         N3 = JTR(ITS,3)
         DO K=1,3
            PTS(K,1)=CV(N1,K)
            PTS(K,2)=CV(N2,K)
            PTS(K,3)=CV(N3,K)
         ENDDO
         NV=3
         DO JJ = 1, 3
            PP(JJ) = ZERO
            PP1(JJ) = ZERO
         ENDDO
C
C     Per ciascuna tessera, trova la porzione scoperta e ne calcola
C     l'area con il teorema di Gauss-Bonnet; il punto rappresentativo
C     e' definito come media dei vertici della porzione scoperta di
C     tessera e passato in PP (mentre in PP1 ci sono le coordinate del
C     punto sulla normale interna).
C     In INTSPH(mxtsec,10) sono registrate le sfere a cui appartengono
C     i lati delle tessere.
C
         CALL TES2(NSFE,NV,PTS,CCC,PP,PP1,AREA,INTSPH,MXTSEC)
         IF(AREA.EQ.0.0D+00) GO TO 310
         NN = NN + 1
         X(NN) = PP(1)
         Y(NN) = PP(2)
         Z(NN) = PP(3)
         X1(NN) = PP1(1)
         Y1(NN) = PP1(2)
         Z1(NN) = PP1(3)
         AS(NN) = AREA
         DO IV = 1, NV
            INTSPH(NN,IV) = INTSPH(MXTSEC,IV)
         ENDDO
 310  CONTINUE
C
 300  CONTINUE
      NTS = NN
      IF(NTS.GT.MXTSEC) THEN
         IF(MASWRK)WRITE(IW,*) 'IN MORETS: TOO MANY POINTS'
         CALL ABRT
         STOP
      END IF
C
 410  CONTINUE
 400  CONTINUE
C
      ST=ZERO
      DO 510 I=1,NTS
 510  ST=ST+AS(I)
      IF(MASWRK)WRITE(IW,1300) NTS,ST
C
      DO I=1,NESF
        RE(I)=RE(I)*ANTOAU
        XE(I)=XE(I)*ANTOAU
        YE(I)=YE(I)*ANTOAU
        ZE(I)=ZE(I)*ANTOAU
      ENDDO
      DO I=1,NTS
        AS(I)=AS(I)*ANTOAU*ANTOAU
        X(I) =X(I) *ANTOAU
        Y(I) =Y(I) *ANTOAU
        Z(I) =Z(I) *ANTOAU
        X1(I)=X1(I)*ANTOAU
        Y1(I)=Y1(I)*ANTOAU
        Z1(I)=Z1(I)*ANTOAU
      ENDDO
      CCX = CCX * ANTOAU
      CCY = CCY * ANTOAU
      CCZ = CCZ * ANTOAU
      DR = DR * ANTOAU
C
      QNUC=ZERO
      DO 200 I=1,NTS
       XI=X(I)
       YI=Y(I)
       ZI=Z(I)
       XK=X1(I)
       YK=Y1(I)
       ZK=Z1(I)
       CX=(XI-XK)/DR
       CY=(YI-YK)/DR
       CZ=(ZI-ZK)/DR
       VN = ZERO
       DO 210 L=1,NAT
        XL=C(1,L)
        YL=C(2,L)
        ZL=C(3,L)
        RIL=SQRT((XI-XL)**2+(YI-YL)**2+(ZI-ZL)**2)
        XN=((XI-XL)*CX+(YI-YL)*CY+(ZI-ZL)*CZ)/RIL**3
        VN = VN + XN*ZAN(L)
  210  CONTINUE
       QNUC=QNUC+VN*AS(I)
  200 CONTINUE
      QNUC=QNUC/FPI
      IF(MASWRK)WRITE(IW,1400) QNUC
      CALL CHGOUT(X,Y,Z,X1,Y1,Z1,AS,SCR,SCR2,CEL,L2,NTS)
      RETURN
C
  800 FORMAT(/1X,'**** NEW CAVITY - POLYGEN *****')
 1200 FORMAT(I5,4F15.9,F18.9)
 1300 FORMAT(1X,'TOTAL NUMBER OF TESSERAE ',I8,
     *          ' SURFACE AREA',F15.8,' (A**2)')
 1400 FORMAT(1X,'BEST ESTIMATE OF NUCLEAR CHARGE',F15.5)
      END
C*MODULE PCMVCH  *DECK TES2
      SUBROUTINE TES2(NS,NV,PTS,CCC,PP,PP1,AREA,INTSPH,MXTSEC)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXTS=2500, MXSP=250)
C
C   hui li
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      DIMENSION PTS(3,10),CCC(3,10),PP(3),PP1(3),INTSPH(MXTSEC,10)
      DIMENSION P1(3),P2(3),P3(3),P4(3),POINT(3),
     *          PSCR(3,10),CCCP(3,10),POINTL(3,10),
     *          LTYP(10),IND(10),INTSCR(10)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
C   hui li
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPH(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
C
C     Coord. del centro che sottende l`arco tra i vertici
C     n e n+1 (per i primi tre vertici e' sicuramente il centro della
C     sfera) e sfera alla cui intersezione con NS appartiene l'arco (se
C     appartiene alla sfera originaria INTSPH(4000,N)=NS)
C
      AREA = 0.0D+00
      DO J=1, 3
        CCC(1,J) = XE(NS)
        CCC(2,J) = YE(NS)
        CCC(3,J) = ZE(NS)
      ENDDO
C
C     INTSPH viene riferito alla tessera 4000, e in seguito riceve il
C     numero corretto.
C
      DO N = 1, 3
        INTSPH(MXTSEC,N) = NS
      ENDDO
C
C     Loop sulle altre sfere
C
      DO 150 NSFE1=1,NESF
      IF(NSFE1.EQ.NS) GO TO 150
C
C     Memorizza i vertici e i centri che sottendono gli archi
      DO J =1, NV
        INTSCR(J) = INTSPH(MXTSEC,J)
      DO I = 1,3
        PSCR(I,J) = PTS(I,J)
        CCCP(I,J) = CCC(I,J)
      ENDDO
      ENDDO
C
      ICOP = 0
      DO J =1, 10
        IND(J) = 0
        LTYP(J) = 0
      ENDDO
C     Loop sui vertici della tessera considerata
      DO 100 I=1,NV
        DELR2=(PTS(1,I)-XE(NSFE1))**2+(PTS(2,I)-YE(NSFE1))**2+
     *  (PTS(3,I)-ZE(NSFE1))**2
        DELR=SQRT(DELR2)
        IF(DELR.LT.RE(NSFE1)) THEN
          IND(I) = 1
          ICOP = ICOP+1
        END IF
 100  CONTINUE
C     Se la tessera e' completamente coperta, la trascura
      IF(ICOP.EQ.NV) RETURN
C
C     Controlla e classifica i lati della tessera: LTYP = 0 (coperto),
C     1 (tagliato con il II vertice coperto), 2 (tagliato con il I
C     vertice coperto), 3 (bitagliato), 4 (libero)
C     Loop sui lati
      DO L = 1, NV
        IV1 = L
        IV2 = L+1
        IF(L.EQ.NV) IV2 = 1
        IF(IND(IV1).EQ.1.AND.IND(IV2).EQ.1) THEN
          LTYP(L) = 0
        ELSE IF(IND(IV1).EQ.0.AND.IND(IV2).EQ.1) THEN
          LTYP(L) = 1
        ELSE IF(IND(IV1).EQ.1.AND.IND(IV2).EQ.0) THEN
          LTYP(L) = 2
        ELSE IF(IND(IV1).EQ.0.AND.IND(IV2).EQ.0) THEN
          LTYP(L) = 4
C
          RC2 = (CCC(1,L)-PTS(1,L))**2 + (CCC(2,L)-PTS(2,L))**2 +
     *          (CCC(3,L)-PTS(3,L))**2
          RC = SQRT(RC2)
C     Su ogni lato si definiscono 11 punti equispaziati, che vengono
C     controllati
          TOL = - 1.0D-10
          DO II = 1, 11
          POINT(1) = PTS(1,IV1) + II * (PTS(1,IV2)-PTS(1,IV1)) / 11
          POINT(2) = PTS(2,IV1) + II * (PTS(2,IV2)-PTS(2,IV1)) / 11
          POINT(3) = PTS(3,IV1) + II * (PTS(3,IV2)-PTS(3,IV1)) / 11
          POINT(1) = POINT(1) - CCC(1,L)
          POINT(2) = POINT(2) - CCC(2,L)
          POINT(3) = POINT(3) - CCC(3,L)
          DNORM = SQRT(POINT(1)**2 + POINT(2)**2 + POINT(3)**2)
          POINT(1) = POINT(1) * RC / DNORM + CCC(1,L)
          POINT(2) = POINT(2) * RC / DNORM + CCC(2,L)
          POINT(3) = POINT(3) * RC / DNORM + CCC(3,L)
          DIST = SQRT( (POINT(1)-XE(NSFE1))**2 +
     *    (POINT(2)-YE(NSFE1))**2 + (POINT(3)-ZE(NSFE1))**2 )
          IF((DIST - RE(NSFE1)) .LT. TOL) THEN
C         IF(DIST.LT.RE(NSFE1)) then
            LTYP(L) = 3
            DO JJ = 1, 3
              POINTL(JJ,L) = POINT(JJ)
            ENDDO
            GO TO 160
          END IF
          ENDDO
        END IF
 160    CONTINUE
C
      ENDDO
C
C     Se la tessera e' spezzata in due o piu' tronconi, la trascura
      ICUT = 0
      DO L = 1, NV
        IF(LTYP(L).EQ.1.OR.LTYP(L).EQ.2) ICUT = ICUT + 1
        IF(LTYP(L).EQ.3) ICUT = ICUT + 2
      ENDDO
      ICUT = ICUT / 2
      IF(ICUT.GT.1) RETURN
C
C     Creazione dei nuovi vertici e lati della tessera
C     Loop sui lati
      N = 1
      DO 300 L = 1, NV
C     Se il lato L e' coperto:
        IF(LTYP(L).EQ.0) GO TO 300
C
        IV1 = L
        IV2 = L+1
        IF(L.EQ.NV) IV2 = 1
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     Se il lato L e' tagliato (con il I vertice scoperto):
        IF(LTYP(L).EQ.1) THEN
        DO JJ = 1, 3
          PTS(JJ,N) = PSCR(JJ,IV1)
          CCC(JJ,N) = CCCP(JJ,IV1)
        ENDDO
        INTSPH(MXTSEC,N) = INTSCR(IV1)
        N = N+1
C
C     Trova l'intersezione tra i due vertici del lato L
C
C     P1 = coord. del primo vertice
C     P2 = coord. del secondo vertice
C     P3 = coord. del centro dell`arco sotteso
C     P4 = coord. dell'intersezione
C
        DO JJ = 1, 3
          P1(JJ) = PSCR(JJ,IV1)
          P2(JJ) = PSCR(JJ,IV2)
          P3(JJ) = CCCP(JJ,IV1)
        ENDDO
        CALL INTER(P1,P2,P3,P4,NSFE1,0)
C     Aggiorna i vertici della tessera e il centro dell'arco
        DO JJ = 1,3
          PTS(JJ,N) = P4(JJ)
        ENDDO
C
C     Il nuovo arco sara' sotteso tra questo e il prossimo punto
C     di intersezione: il centro che lo sottende
C     sara' il centro del cerchio di intersezione tra la sfera NS
C     e la sfera NSFE1.
C
        DE2 = (XE(NSFE1)-XE(NS))**2+(YE(NSFE1)-YE(NS))**2+
     *        (ZE(NSFE1)-ZE(NS))**2
        CCC(1,N)=XE(NS)+(XE(NSFE1)-XE(NS))*
     *           (RE(NS)**2-RE(NSFE1)**2+DE2)/(2.0D+00*DE2)
        CCC(2,N)=YE(NS)+(YE(NSFE1)-YE(NS))*
     *           (RE(NS)**2-RE(NSFE1)**2+DE2)/(2.0D+00*DE2)
        CCC(3,N)=ZE(NS)+(ZE(NSFE1)-ZE(NS))*
     *           (RE(NS)**2-RE(NSFE1)**2+DE2)/(2.0D+00*DE2)
        INTSPH(MXTSEC,N) = NSFE1
        N = N+1
        END IF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     Se il lato L e' tagliato (con il II vertice scoperto):
        IF(LTYP(L).EQ.2) THEN
C     Trova l'intersezione tra i due vertici del lato L
C
C     P1 = coord. del primo vertice
C     P2 = coord. del secondo vertice
C     P3 = coord. del centro dell`arco sotteso
C     P4 = coord. dell'intersezione
C
        DO JJ = 1, 3
          P1(JJ) = PSCR(JJ,IV1)
          P2(JJ) = PSCR(JJ,IV2)
          P3(JJ) = CCCP(JJ,IV1)
        ENDDO
        CALL INTER(P1,P2,P3,P4,NSFE1,1)
C     Aggiorna i vertici della tessera e il centro dell'arco
        DO JJ = 1,3
          PTS(JJ,N) = P4(JJ)
          CCC(JJ,N) = CCCP(JJ,IV1)
        ENDDO
        INTSPH(MXTSEC,N) = INTSCR(IV1)
        N = N+1
        END IF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     Se il lato e' intersecato due volte:
        IF(LTYP(L).EQ.3) THEN
        DO JJ = 1, 3
          PTS(JJ,N) = PSCR(JJ,IV1)
          CCC(JJ,N) = CCCP(JJ,IV1)
        ENDDO
        INTSPH(MXTSEC,N) = INTSCR(IV1)
        N = N+1
C
C     Trova l'intersezione tra il primo vertice e un punto intermedio
C     coperto
C
C     P1 = coord. del primo vertice
C     P2 = coord. del secondo vertice
C     P3 = coord. del centro dell`arco sotteso
C     P4 = coord. dell'intersezione
C
        DO JJ = 1, 3
          P1(JJ) = PSCR(JJ,IV1)
          P2(JJ) = POINTL(JJ,L)
          P3(JJ) = CCCP(JJ,IV1)
        ENDDO
        CALL INTER(P1,P2,P3,P4,NSFE1,0)
C     Aggiorna i vertici della tessera e il centro dell'arco
        DO JJ = 1,3
          PTS(JJ,N) = P4(JJ)
        ENDDO
C
C     Il nuovo arco sara' sotteso tra questo e il prossimo punto
C     di intersezione: il centro che lo sottende
C     sara' il centro del cerchio di intersezione tra la sfera NS
C     e la sfera NSFE1.
C
        DE2 = (XE(NSFE1)-XE(NS))**2+(YE(NSFE1)-YE(NS))**2+
     *        (ZE(NSFE1)-ZE(NS))**2
        CCC(1,N)=XE(NS)+(XE(NSFE1)-XE(NS))*
     *           (RE(NS)**2-RE(NSFE1)**2+DE2)/(2.0D+00*DE2)
        CCC(2,N)=YE(NS)+(YE(NSFE1)-YE(NS))*
     *           (RE(NS)**2-RE(NSFE1)**2+DE2)/(2.0D+00*DE2)
        CCC(3,N)=ZE(NS)+(ZE(NSFE1)-ZE(NS))*
     *           (RE(NS)**2-RE(NSFE1)**2+DE2)/(2.0D+00*DE2)
        INTSPH(MXTSEC,N) = NSFE1
        N = N+1
C
C     Trova l'intersezione tra un punto intermedio coperto e il
C     secondo vertice
C
C     P1 = coord. del primo vertice
C     P2 = coord. del secondo vertice
C     P3 = coord. del centro dell`arco sotteso
C     P4 = coord. dell'intersezione
C
        DO JJ = 1, 3
          P1(JJ) = POINTL(JJ,L)
          P2(JJ) = PSCR(JJ,IV2)
          P3(JJ) = CCCP(JJ,IV1)
        ENDDO
        CALL INTER(P1,P2,P3,P4,NSFE1,1)
C     Aggiorna il vertice e il centro dell'arco
        DO JJ = 1,3
          PTS(JJ,N) = P4(JJ)
          CCC(JJ,N) = CCCP(JJ,IV1)
        ENDDO
        INTSPH(MXTSEC,N) = INTSCR(IV1)
        N = N + 1
        END IF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     Se il lato e' scoperto:
        IF(LTYP(L).EQ.4) THEN
        DO JJ = 1, 3
          PTS(JJ,N) = PSCR(JJ,IV1)
          CCC(JJ,N) = CCCP(JJ,IV1)
        ENDDO
        INTSPH(MXTSEC,N) = INTSCR(IV1)
        N = N+1
        END IF
C
 300  CONTINUE
C
      NV = N - 1
C     Controlla che il numero di vertici creati non sia eccessivo
      IF(NV.GT.10) THEN
         IF(MASWRK) WRITE(IW,*)
     *       'TROPPI VERTICI CREATI IN TESSERA: BYE BYE...',NV
         CALL ABRT
         STOP
      END IF
 150  CONTINUE
C
C     Se la tessera non e' stata scartata, a questo punto ne troviamo
C     l'area e il punto rappresentativo
C
      CALL GBON(NV,NS,PTS,CCC,PP,PP1,AREA,INTSPH,MXTSEC)
      RETURN
      END
C*MODULE PCMVCH  *DECK GBON
      SUBROUTINE GBON(NV,NS,PTS,CCC,PP,PP1,AREA,INTSPH,MXTSEC)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXTS=2500, MXSP=250)
C
C   hui li
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      DIMENSION PTS(3,10),CCC(3,10),PP(3),PP1(3),INTSPH(MXTSEC,10)
      DIMENSION P1(3),P2(3),P3(3),U1(3),U2(3)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
C   hui li
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
C
      DATA PI/3.141592653589793D+00/
C
C     Sfrutta il teorema di Gauss-Bonnet per calcolare l'area
C     della tessera con vertici PTS(3,NV). Consideriamo sempre
C     che il lato N della tessera e' quello compreso tra i vertici
C     N e N+1 (oppure NV e 1). In CCC(3,NV) sono le posizioni dei
C     centri degli archi che sottendono i vari lati della tessera.
C     La formula di Gauss-Bonet per le sfere e':
C            Area=R^2[2pi+S(Phi(N)cosT(N))-S(Beta(N))]
C     dove Phi(N) e' la lunghezza d'arco (in radianti) del lato N,
C     T(N) e' l'angolo polare del lato N, Beta(N) l'angolo esterno
C     relativo al vertice N.
C
      TPI=2.0D+00*PI
C
C     Calcola la prima sommatoria
C
      SUM1 = 0.0D+00
      DO 100 N = 1, NV
      X1 = PTS(1,N) - CCC(1,N)
      Y1 = PTS(2,N) - CCC(2,N)
      Z1 = PTS(3,N) - CCC(3,N)
      IF(N.LT.NV) THEN
        X2 = PTS(1,N+1) - CCC(1,N)
        Y2 = PTS(2,N+1) - CCC(2,N)
        Z2 = PTS(3,N+1) - CCC(3,N)
      ELSE
        X2 = PTS(1,1) - CCC(1,N)
        Y2 = PTS(2,1) - CCC(2,N)
        Z2 = PTS(3,1) - CCC(3,N)
      END IF
      DNORM1 = X1*X1 + Y1*Y1 + Z1*Z1
      DNORM2 = X2*X2 + Y2*Y2 + Z2*Z2
      SCAL = X1*X2 + Y1*Y2 + Z1*Z2
      COSPHIN = SCAL / (SQRT(DNORM1*DNORM2))
      IF(COSPHIN.GT.1.0D+00) COSPHIN = 1.0D+00
      PHIN = ACOS(COSPHIN)
C
C     NSFE1 e' la sfera con cui la sfera NS si interseca (eventualmente)
        NSFE1 = INTSPH(MXTSEC,N)
        X1 = XE(NSFE1) - XE(NS)
        Y1 = YE(NSFE1) - YE(NS)
        Z1 = ZE(NSFE1) - ZE(NS)
      DNORM1 = SQRT(X1*X1 + Y1*Y1 + Z1*Z1)
      IF(DNORM1.EQ.0.0D+00) DNORM1 = 1.0D+00
        X2 = PTS(1,N) - XE(NS)
        Y2 = PTS(2,N) - YE(NS)
        Z2 = PTS(3,N) - ZE(NS)
      DNORM2 = SQRT(X2*X2 + Y2*Y2 + Z2*Z2)
      COSTN = (X1*X2+Y1*Y2+Z1*Z2)/(DNORM1*DNORM2)
      SUM1 = SUM1 + PHIN * COSTN
 100  CONTINUE
C
C     Calcola la seconda sommatoria: l'angolo esterno Beta(N) e'
C     definito usando i versori (u(N-1),u(N)) tangenti alla sfera
C     nel vertice N lungo le direzioni dei lati N-1 e N:
C                cos( Pi-Beta(N) )=u(N-1)*u(N)
C            u(N-1) = [V(N) x (V(N) x V(N-1))]/NORM
C            u(N) = [V(N) x (V(N) x V(N+1))]/NORM
C     dove V(I) e' il vettore posizione del vertice I RISPETTO AL
C     CENTRO DELL'ARCO CHE SI STA CONSIDERANDO.
C
      SUM2 = 0.0D+00
C     Loop sui vertici
      DO 200 N = 1, NV
      DO JJ = 1, 3
      P1(JJ) = 0.0D+00
      P2(JJ) = 0.0D+00
      P3(JJ) = 0.0D+00
      ENDDO
      N1 = N
      IF(N.GT.1) N0 = N - 1
      IF(N.EQ.1) N0 = NV
      IF(N.LT.NV) N2 = N + 1
      IF(N.EQ.NV) N2 = 1
C     Trova i vettori posizione rispetto ai centri corrispondenti
C     e i versori tangenti
C
C     Lato N0-N1:
      DO JJ = 1, 3
      P1(JJ) = PTS(JJ,N1) - CCC(JJ,N0)
      P2(JJ) = PTS(JJ,N0) - CCC(JJ,N0)
      ENDDO
C
      CALL VECP(P1,P2,P3,DNORM1)
      DO JJ = 1, 3
      P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM1)
      DO JJ = 1, 3
      U1(JJ) = P3(JJ)/DNORM1
      ENDDO
C
C     Lato N1-N2:
      DO JJ = 1, 3
      P1(JJ) = PTS(JJ,N1) - CCC(JJ,N1)
      P2(JJ) = PTS(JJ,N2) - CCC(JJ,N1)
      ENDDO
C
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
      P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
      U2(JJ) = P3(JJ)/DNORM3
      ENDDO
C
      BETAN = ACOS(U1(1)*U2(1)+U1(2)*U2(2)+U1(3)*U2(3))
      SUM2 = SUM2 + (PI - BETAN)
 200  CONTINUE
C     Calcola l'area della tessera
        AREA = RE(NS)*RE(NS)*(TPI + SUM1 - SUM2)
C     Trova il punto rappresentativo (come media dei vertici)
      DO JJ = 1, 3
      PP(JJ) = 0.0D+00
      ENDDO
      DO I = 1, NV
      PP(1) = PP(1) + (PTS(1,I)-XE(NS))
      PP(2) = PP(2) + (PTS(2,I)-YE(NS))
      PP(3) = PP(3) + (PTS(3,I)-ZE(NS))
      ENDDO
      DNORM = 0.0D+00
      DO JJ = 1, 3
      DNORM = DNORM + PP(JJ)*PP(JJ)
      ENDDO
      PP(1) = XE(NS) + PP(1) * RE(NS) / SQRT(DNORM)
      PP(2) = YE(NS) + PP(2) * RE(NS) / SQRT(DNORM)
      PP(3) = ZE(NS) + PP(3) * RE(NS) / SQRT(DNORM)
C     Trova il punto sulla normale (interna!) distante DR dal punto
C     rappresentativo
      PP1(1) = XE(NS) + (PP(1) - XE(NS)) * (RE(NS) - DR) / RE(NS)
      PP1(2) = YE(NS) + (PP(2) - YE(NS)) * (RE(NS) - DR) / RE(NS)
      PP1(3) = ZE(NS) + (PP(3) - ZE(NS)) * (RE(NS) - DR) / RE(NS)
C
C     A causa delle approssimazioni numeriche, l'area di alcune piccole
C     tessere puo' risultare negativa, e viene in questo caso trascurata
      IF(AREA.LT.0.0D+00) THEN
        IF(MASWRK)WRITE(IW,1000)NS
        AREA = 0.0D+00
 1000   FORMAT(/,'ATTENTION: AREA OF A TESSERA ON SPHERE ',I3,
     *  ' IS TOO SMALL: IT WILL BE NEGLECTED')
      END IF
      RETURN
      END
C*MODULE PCMVCH  *DECK POLYGEN
      SUBROUTINE POLYGEN(PTYPE,TSNUM,XEN,YEN,ZEN,REN,
     *                   TSEFF,CV,JTR,OLDTR,EDNEW,MXTS,MXVERT,IW)
C
      IMPLICIT DOUBLE PRECISION (D)
C
      DOUBLE PRECISION CV(MXVERT,3)
      INTEGER JTR(MXTS,3),OLDTR(100,100),EDNEW(90,100)
C
      PARAMETER (ICOSA = 1)
      PARAMETER (NVI=12, NTI=20, NEI=30)
      PARAMETER (IPENTA = 2)
      PARAMETER (NVP=32, NTP=60, NEP=90)
      PARAMETER (ITETRA = 3)
      PARAMETER (NVT= 4, NTT=4 , NET= 6)
C
C   hui li
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      INTEGER I,J,K,L,M,N,II,JJ,NV,NT0,NE0,NF,IW
      INTEGER NTPT,NVPT,PTYPE,TSNUM,TSEFF
      INTEGER NFI,NFP,NFT,NDI,NDP,NDT
      INTEGER TRVO(60,3),TREO(60,3),EDO(90,2)
      INTEGER INDTR(60)
      INTEGER TETTRV(3,4), TETTRE(3,4), TETED(2,6)
      INTEGER ICOTRV(3,20),ICOTRE(3,20),ICOED(2,30)
      INTEGER PENTRV(3,60),PENTRE(3,60),PENED(2,90)
C
      DOUBLE PRECISION TETVE(3,4),ICOVE(3,12),PENVE(3,32)
      DOUBLE PRECISION DNORM
      DOUBLE PRECISION V1(3),V2(3),V3(3)
      DOUBLE PRECISION ALPHA,BETA,COSTHETA,THETA,COS1,COS2,SINTHETA
      DOUBLE PRECISION XEN,YEN,ZEN,REN
C   hui li
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C
C Data for original polyhedra
C
C 1) Tetrahedron
C
      DATA TETED/1,2,2,3,1,3,1,4,3,4,2,4/
      DATA TETTRE/1,2,3,1,6,4,3,5,4,2,5,6/
      DATA TETTRV/1,2,3,1,2,4,1,3,4,2,3,4/
      DATA TETVE/
     *  -.577350269D+00, .577350269D+00, .577350269D+00,
     *  .577350269D+00,-.577350269D+00, .577350269D+00,
     *  -.577350269D+00,-.577350269D+00,-.577350269D+00,
     *  .577350269D+00, .577350269D+00,-.577350269D+00/
C
C 2) Icosaedron
C
      DATA ICOED/
     *  1, 2, 2, 3, 1, 3, 2, 6, 1, 6, 3, 4, 1, 4, 4, 5, 1, 5, 5, 6, 3,
     *  7, 2, 7, 6,11, 2,11, 7,11, 4, 8, 3, 8, 7, 8, 5, 9, 4, 9, 8, 9,
     *  6,10, 5,10, 9,10,10,11, 8,12, 7,12,11,12, 9,12,10,12/
      DATA ICOTRE/
     *  1, 2, 3, 1, 4, 5, 3, 6, 7, 7, 8, 9, 9,10, 5, 2,11,12,
     *  4,13,14,12,15,14, 6,16,17,11,18,17, 8,19,20,16,21,20,10,22,23,
     *  19,24,23,22,25,13,18,26,27,15,28,27,21,29,26,24,30,29,25,28,30/
      DATA ICOTRV/
     *  1 , 2, 3, 1, 2, 6, 1, 3, 4, 1, 4, 5, 1, 5, 6, 2, 3, 7,
     *  2, 6,11, 2, 7,11, 3, 4, 8, 3, 7, 8, 4, 5, 9, 4, 8, 9, 5, 6,10,
     *  5, 9,10, 6,10,11, 7, 8,12, 7,11,12, 8, 9,12, 9,10,12,10,11,12/
      DATA ICOVE/
     *  .000000000D+00,.000000000D+00, .100000000D+01,.276393202D+00,
     *  .850650808D+00, .447213595D+00,-.723606798D+00, .525731112D+00,
     *  .447213595D+00,-.723606798D+00,-.525731112D+00, .447213595D+00,
     *  .276393202D+00,-.850650808D+00, .447213595D+00,.894427191D+00,
     *  -.219071479D-15, .447213595D+00,-.276393202D+00, .850650808D+00,
     *  -.447213595D+00,-.894427191D+00, .109535740D-15,-.447213595D+00,
     *  -.276393202D+00,-.850650808D+00,-.447213595D+00,.723606798D+00,
     *  -.525731112D+00,-.447213595D+00,.723606798D+00, .525731112D+00,
     *  -.447213595D+00,.000000000D+00, .000000000D+00,-.100000000D+01/
C
C 3) Pentakisdodecahedron
C
      DATA PENED/
     *  1 , 2, 2, 3, 1, 3, 2, 6, 1, 6, 3, 4, 1, 4, 4, 5, 1, 5, 5, 6, 3,
     *  8,2, 8, 6, 7, 2, 7, 7,12, 2,12, 8,12, 4, 9, 3, 9, 8,13, 3,13, 9,
     *  13, 5,10, 4,10, 9,14, 4,14,10,14, 6,11, 5,11,10,15, 5,15,11,15,
     *  7,16, 6,16,11,16,12,17, 7,17,16,17,12,18, 8,18,13,18,13,19, 9,
     *  19,14,19,14,20,10,20,15,20,15,21,11,21,16,21,17,22,12,22,18,22,
     *  18,23,13,23,19,23,19,24,14,24,20,24,20,25,15,25,21,25,17,26,16,
     *  26,21,26,22,27,17,27,26,27,22,28,18,28,23,28,23,29,19,29,24,29,
     *  24,30,20,30,25,30,25,31,21,31,26,31,27,28,28,29,29,30,30,31,27,
     *  31,28,32,27,32,31,32,29,32,30,32/
      DATA PENTRE/
     *  1, 2, 3, 1, 4, 5, 3, 6, 7, 7, 8, 9, 9,10, 5, 2,11,12, 4,13,14,
     *  14,15,16,12,17,16, 6,18,19,11,20,21,19,22,21, 8,23,24,18,25,26,
     *  24,27,26,10,28,29,23,30,31,29,32,31,13,33,34,28,35,34,15,36,37,
     *  33,38,37,17,39,40,20,41,40,22,42,43,25,44,43,27,45,46,30,47,46,
     *  32,48,49,35,50,49,36,51,52,39,53,52,41,54,55,42,56,55,44,57,58,
     *  45,59,58,47,60,61,48,62,61,38,63,64,50,65,64,51,66,67,63,68,67,
     *  53,69,70,54,71,70,56,72,73,57,74,73,59,75,76,60,77,76,62,78,79,
     *  65,80,79,66,81,69,71,82,72,74,83,75,77,84,78,68,85,80,81,86,87,
     *  85,88,87,82,89,86,83,90,89,84,88,90/
      DATA PENTRV/
     *  1, 2, 3, 1, 2, 6, 1, 3, 4, 1, 4, 5, 1, 5, 6, 2, 3, 8, 2, 6, 7,
     *  2, 7,12, 2, 8,12, 3, 4, 9, 3, 8,13, 3, 9,13, 4, 5,10, 4, 9,14,
     *  4,10,14, 5, 6,11, 5,10,15, 5,11,15, 6, 7,16, 6,11,16, 7,12,17,
     *  7,16,17, 8,12,18, 8,13,18, 9,13,19, 9,14,19,10,14,20,10,15,20,
     *  11,15,21,11,16,21,12,17,22,12,18,22,13,18,23,13,19,23,14,19,24,
     *  14,20,24,15,20,25,15,21,25,16,17,26,16,21,26,17,22,27,17,26,27,
     *  18,22,28,18,23,28,19,23,29,19,24,29,20,24,30,20,25,30,21,25,31,
     *  21,26,31,22,27,28,23,28,29,24,29,30,25,30,31,26,27,31,27,28,32,
     *  27,31,32,28,29,32,29,30,32,30,31,32/
      DATA ((PENVE(II,JJ),II=1,3),JJ=1,16)/
     *  .000000000D+00, .000000000D+00, .100000000D+01, .491123473D+00,
     *  .356822090D+00, .794654472D+00,-.187592474D+00, .577350269D+00,
     *  .794654472D+00,-.607061998D+00, .540159669D-09, .794654472D+00,
     * -.187592475D+00,-.577350269D+00, .794654472D+00, .491123473D+00,
     * -.356822091D+00, .794654472D+00, .894427191D+00, .000000000D+00,
     *  .447213595D+00, .276393203D+00, .850650808D+00, .447213595D+00,
     * -.723606797D+00, .525731113D+00, .447213595D+00,-.723606799D+00,
     * -.525731111D+00, .447213595D+00, .276393201D+00,-.850650809D+00,
     *  .447213595D+00, .794654472D+00, .577350269D+00, .187592474D+00,
     * -.303530999D+00, .934172359D+00, .187592474D+00,-.982246946D+00,
     *  .873996703D-09, .187592474D+00,-.303531000D+00,-.934172359D+00,
     *  .187592474D+00, .794654471D+00,-.577350271D+00, .187592474D+00/
      DATA ((PENVE(II,JJ),II=1,3),JJ=17,32)/
     *  .982246946D+00, .000000000D+00,-.187592474D+00, .303530999D+00,
     *  .934172359D+00,-.187592474D+00,-.794654472D+00, .577350270D+00,
     * -.187592474D+00,-.794654473D+00,-.577350268D+00,-.187592474D+00,
     *  .303530997D+00,-.934172359D+00,-.187592474D+00, .723606798D+00,
     *  .525731112D+00,-.447213596D+00,-.276393202D+00, .850650808D+00,
     * -.447213596D+00,-.894427191D+00, .795855278D-09,-.447213596D+00,
     * -.276393203D+00,-.850650808D+00,-.447213596D+00, .723606797D+00,
     * -.525731113D+00,-.447213596D+00, .607061998D+00, .000000000D+00,
     * -.794654472D+00, .187592474D+00, .577350269D+00,-.794654472D+00,
     * -.491123473D+00, .356822090D+00,-.794654472D+00,-.491123474D+00,
     * -.356822089D+00,-.794654472D+00, .187592473D+00,-.577350269D+00,
     * -.794654472D+00,.000000000D+00, .000000000D+00,-.100000000D+01/
C
C Polygen: a program to generate spherical polyhedra with triangular
C faces.  An equilater division algorithm is used.
C
C  A seconda delle due opzioni di funzionamento (Area ottimale o
C  numero di tessere ottimale ) vengono stabilite il tipo di poliedro
C  (Ptype) e la  frequenza di divisione (NF)
C
C  -si controlla che TsNum <= MxTs
C
      IF (TSNUM.GT.MXTS .AND. MASWRK) THEN
         WRITE(IW,*) 'TOO MANY TESSERAE IN POLYGEN'
      END IF
C
C  -calcola il valore di NF ottimale per ciascuna famiglia di poliedri.
C
      NFI = INT( SQRT(TSNUM / ( NTI * 1.0D+00 )) + 0.5D+00)
      NFP = INT( SQRT(TSNUM / ( NTP * 1.0D+00 )) + 0.5D+00)
      NFT = INT( SQRT(TSNUM / ( NTT * 1.0D+00 )) + 0.5D+00)
C
C     write(iw,*) 'NFI,NFP,NFT',NFI,NFP,NFT
C
C  -trova lo scostamento del numero di tessere in input
C
      NDI = IABS( TSNUM - NTI * NFI**2 )
      NDP = IABS( TSNUM - NTP * NFP**2 )
      NDT = IABS( TSNUM - NTT * NFT**2 )
C
C  -sceglie tipo e frequenza di divisione
C  a parita' di differenza in numero di tessere sceglie quello con NF
C  minore
C
      IF((NDP.LE.NDI).AND.(NDP.LE.NDT)) THEN
        PTYPE = IPENTA
        NF = NFP
        TSEFF = NTP * NFP**2
      ELSE
        IF (NDI.LE.NDT) THEN
          PTYPE = ICOSA
          NF = NFI
          TSEFF = NTI * NFI**2
        ELSE
          PTYPE = ITETRA
          NF = NFT
          TSEFF = NTT * NFT**2
        END IF
      END IF
C
C     write(6,9001) TsNum,TsEff, Ptype , 4.0d+00*PI*REN**2/TsEff
C9001 format('TsNum=',I5,1x,'TsEff=',I5,1x,'PType=',I5,1x,
C    *  'Area=',F7.4)
C
C  -carica gli opportuni valori iniziali
C
      IF (PTYPE.EQ.1) THEN
C
C  icosaedro
C
        NT0=NTI
        NV=NVI
        NE0=NEI
        DO I=1, NT0
          DO K=1,3
            TRVO(I,K)=ICOTRV(K,I)
            TREO(I,K)=ICOTRE(K,I)
          ENDDO
        ENDDO
C
        DO I=1,NV
          DO K=1,3
            CV(I,K)=ICOVE(K,I)
          ENDDO
        ENDDO
C
        DO I=1,NE0
          DO K=1,2
            EDO(I,K)=ICOED(K,I)
          ENDDO
        ENDDO
C
      ELSE IF (PTYPE.EQ.2) THEN
C
C  -pentakisdodecaedro
C
        NT0=NTP
        NV=NVP
        NE0=NEP
C
        DO I=1, NT0
          DO K=1,3
            TRVO(I,K)=PENTRV(K,I)
            TREO(I,K)=PENTRE(K,I)
          ENDDO
        ENDDO
C
        DO I=1,NV
          DO K=1,3
            CV(I,K)=PENVE(K,I)
          ENDDO
        ENDDO
C
        DO I=1,NE0
          DO K=1,2
            EDO(I,K)=PENED(K,I)
          ENDDO
        ENDDO
C
      ELSE IF (PTYPE.EQ.3) THEN
C
C  tetraedro
C
        NT0=NTT
        NV=NVT
        NE0=NET
C
        DO I=1, NT0
          DO K=1,3
            TRVO(I,K)=TETTRV(K,I)
            TREO(I,K)=TETTRE(K,I)
          ENDDO
        ENDDO
C
        DO I=1,NV
          DO K=1,3
            CV(I,K)=TETVE(K,I)
          ENDDO
        ENDDO
C
        DO I=1,NE0
          DO K=1,2
            EDO(I,K)=TETED(K,I)
          ENDDO
        ENDDO
      END IF
C
      NVPT=NV+1
C
C  -nuovi vertici posti lungo i vecchi spigoli
C  -le regole di calcolo derivano da  calcoli di algebra
C   lineare e trigonometria
C  -i vertici vengono memorizzati in ordine progressivo ed riferiti
C  tramite ednew al vertice di appartenenza
C
      DO J=1,NE0
        DO K=1,3
          V1(K)=CV(EDO(J,1),K)
          V2(K)=CV(EDO(J,2),K)
        ENDDO
        COSTHETA=
     *    ( V1(1)*V2(1) + V1(2)*V2(2) + V1(3)*V2(3)) /
     *    (SQRT(V1(1)**2 + V1(2)**2 + V1(3)**2) *
     *     SQRT(V2(1)**2 + V2(2)**2 + V2(3)**2))
        THETA=ACOS(COSTHETA)
        SINTHETA=SIN(THETA)
        DO L=1,NF-1
          M=NF-L
          COS1=COS(THETA*L/NF)
          COS2=COS(THETA*(NF-L)/NF)
          ALPHA=(COS1-COSTHETA*COS2)/SINTHETA**2
          BETA=(COS2-COSTHETA*COS1)/SINTHETA**2
          DO K=1,3
            V3(K)=ALPHA*V1(K)+BETA*V2(K)
          ENDDO
          DNORM = SQRT(V3(1)**2 + V3(2)**2 + V3(3)**2)
          DO K=1,3
            V3(K) = V3(K) /DNORM
          ENDDO
          DO K=1,3
            CV(NVPT,K)=V3(K)
          ENDDO
          EDNEW(J,L+1)=NVPT
          NVPT=NVPT+1
        ENDDO
      ENDDO
C
C  -nuovi vertici non posti lungo i vecchi spigoli
C  -a partire dai vertici in ednew secondo regole analoghe alle
C  precedenti, vengono memorizzati a seconda del triangolo in trnew
C  trnew(triangolo,fila,n ordine)=nvertice
C
      DO J=1,NT0
        II=TREO(J,1)
        JJ=TREO(J,3)
        INDTR(J)=NVPT
        DO L=3,NF
          DO K=1,3
            V1(K)=CV(EDNEW(II,L),K)
            V2(K)=CV(EDNEW(JJ,L),K)
          ENDDO
        COSTHETA=
     *    ( V1(1)*V2(1) + V1(2)*V2(2) + V1(3)*V2(3)) /
     *    (SQRT(V1(1)**2 + V1(2)**2 + V1(3)**2) *
     *     SQRT(V2(1)**2 + V2(2)**2 + V2(3)**2))
          THETA=ACOS(COSTHETA)
          SINTHETA=SIN(THETA)
          DO N=1,L-2
            COS1=COS(THETA*N/(L-1))
            COS2=COS(THETA*(L-1-N)/(L-1))
            ALPHA=(COS1-COSTHETA*COS2)/SINTHETA**2
            BETA=(COS2-COSTHETA*COS1)/SINTHETA**2
            DO K=1,3
              V3(K)=ALPHA*V1(K)+BETA*V2(K)
            ENDDO
            DNORM = SQRT(V3(1)**2 + V3(2)**2 + V3(3)**2)
            DO K=1,3
              V3(K) = V3(K) /DNORM
            ENDDO
            DO K=1,3
              CV(NVPT,K)=V3(K)
            ENDDO
C           trnew(j,l,n+1)=NVPT
            NVPT=NVPT+1
          ENDDO
        ENDDO
      ENDDO
C
      NV=NVPT-1
C
C  -ora per ogni triangolo originario vengono posti nella matrice oldtr
C  i numeri di ordine dei vertici originali,creati lungo gli spigoli e
C  no dei vecchi triangoli secondo lo schema:
C
C          11                    La disposizione e' secondo
C          | \                   la posizione geometrica del
C          |  \                  vertice.
C          21--22
C          | \  |\               i vertici (1,1)-(NF+1,1)-(NF+1,NF+1)
C          |  \ | \              sono quelli originari.
C          31--32--33
C          | \  |  | \           i nuovi triangoli sono:
C          |  \ |  |  \          (i,j)-(i+1,j)-(i+1,j+1)
C          41--42--43--44        i=1,...,NF j=1,...,i
C
C                                (i,j)--(i,j+1),(i+1,j+1)
C                                i=2,...,NF j=1,...,i-1
C
C
      NTPT=1
C
      DO N=1,NT0
C
C  -1 vecchi spigoli
C
        OLDTR(1,1)=TRVO(N,1)
        OLDTR(NF+1,1)=TRVO(N,2)
        OLDTR(NF+1,NF+1)=TRVO(N,3)
C
C  -2 nuovi vertici lungo i vecchi spigoli
C
        DO L=2,NF
          OLDTR(L,1)=EDNEW(TREO(N,1),L)
          OLDTR(NF+1,L)=EDNEW(TREO(N,2),L)
          OLDTR(L,L)=EDNEW(TREO(N,3),L)
        ENDDO
C
C  -3 nuovi vertici non lungo i vecchi spigoli
C
        NNTR=INDTR(N)
        DO L=3,NF
          DO M=2,L-1
            OLDTR(L,M)=NNTR
            NNTR=NNTR+1
          ENDDO
        ENDDO
C
C  -ora si creano i nuovi triangoli
C
        DO I=1,NF
          DO J=1,I
            JTR(NTPT,1)=OLDTR(I,J)
            JTR(NTPT,2)=OLDTR(I+1,J)
            JTR(NTPT,3)=OLDTR(I+1,J+1)
            NTPT=NTPT+1
          ENDDO
        ENDDO
C
        DO I=2,NF
          DO J=1,I-1
            JTR(NTPT,1)=OLDTR(I,J)
            JTR(NTPT,2)=OLDTR(I,J+1)
            JTR(NTPT,3)=OLDTR(I+1,J+1)
            NTPT=NTPT+1
          ENDDO
        ENDDO
      ENDDO
C
C scrittura della JVT
C
      DO I=1,NV
         CV(I,1) = CV(I,1) * REN + XEN
         CV(I,2) = CV(I,2) * REN + YEN
         CV(I,3) = CV(I,3) * REN + ZEN
      ENDDO
C
      RETURN
      END
