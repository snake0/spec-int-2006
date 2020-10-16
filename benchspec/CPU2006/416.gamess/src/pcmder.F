C  9 dec 03 - mws - synch common block runopt
C 17 Jun 03 - HL  - changes for pcm gradients
C  8 Oct 01 - HL  - parallelize PCM
C 20 Feb 01 - BM  - DERIVA: further gradient fixes involving NSJR
C 29 Dec 00 - BM  - Corrected the gradient bug
C 11 Oct 00 - PB,BM  - interfaced EFP+PCM
C 25 AUG 00 - BM  - added IEF solvation model
C 18 MAR 97 - PISA - NEW MODULE FOR PCM DERIVATIVES
C
C*MODULE PCMDER  *DECK DERPCM
      SUBROUTINE DERPCM
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS,MXSP=250)
C
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /FMCOM / XX(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMPAR/ IPCM,NFT26,NFT27,IKREP,IEF,IP_F
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
C
C     ----- polarizable continuum model gradient corrections -----
C
C        regenerate original cavity if necessary
C
C     IF(ICAV.EQ.1  .OR.  IDISP.EQ.1) CALL PEDRAM
C
      L2 = (NUM*NUM+NUM)/2
      NATM = NAT
      NESFT=NESF
      NTE = NTS
C
      CALL VALFM(LOADFM)
      LDM    = LOADFM + 1
      LDM2   = LDM    + NTE*NTE
      LD     = LDM2   + NTE*NTE
      LVEC   = LD     + L2
      LQN    = LVEC   + L2
      LQE    = LQN    + NTE
      LEF    = LQE    + NTE
      LVDER  = LEF    + 2*NTE*3
      LDMATM = LVDER  + NTE*3*NATM
      LDRPNT = LDMATM + NTE*NTE
      LDRTES = LDRPNT + NTE*NATM*3*3
      LDRCNT = LDRTES + NTE*NATM*3
      LDRRAD = LDRCNT + NESFT*NATM*3*3
      LDRSLV = LDRRAD + NESFT*NATM*3
      LDRCAV = LDRSLV + 3*NATM
      LDRDIS = LDRCAV + 3*NATM
      LDRREP = LDRDIS + 3*NATM
      LQNDER = LDRREP + 3*NATM
      LQEDER = LQNDER + 3*NATM
      LAST   = LQEDER + 3*NATM
      NEED = LAST - LOADFM -1
      CALL GETFM(NEED)
      IF(EXETYP.EQ.CHECK) GO TO 600
C
C        read previously computed quantities saved on disk:
C        derpunt, dertes, dercentr, derrad, D matrix inverse
C
      CALL SEQREW(NFT26)
                                   LEN = NTE*NATM*3*3
      CALL SQREAD(NFT26,XX(LDRPNT),LEN)
                                   LEN = NTE*NATM*3
      CALL SQREAD(NFT26,XX(LDRTES),LEN)
C
C                                  LEN = NATM*NATM*3*3
C     CALL SQREAD(NFT26,XX(LDRCNT),LEN)
C                                  LEN = NATM*NATM*3
C     CALL SQREAD(NFT26,XX(LDRRAD),LEN)
                                   LEN = NESFT*NATM*3*3
      CALL SQREAD(NFT26,XX(LDRCNT),LEN)
                                   LEN = NESFT*NATM*3
      CALL SQREAD(NFT26,XX(LDRRAD),LEN)
                                   LEN = NTE*NTE
      CALL SQREAD(NFT26,XX(LDMATM),LEN)
      CALL SEQREW(NFT26)
C
      CALL DERBEM(XX(LDM),XX(LDM2),XX(LD),XX(LVEC),XX(LQN),
     *            XX(LQE),XX(LEF),XX(LVDER),XX(LDMATM),
     *            XX(LDRPNT),XX(LDRTES),XX(LDRCNT),XX(LDRRAD),
     *            XX(LDRSLV),XX(LDRCAV),XX(LDRDIS),XX(LDRREP),
     *            XX(LQNDER),XX(LQEDER),NUM,L2,NTE,NATM,NESFT)
C
  600 CONTINUE
      CALL RETFM(NEED)
      IF(MASWRK)WRITE(IW,*)
      IF(MASWRK)WRITE(IW,*)
     *      '.... DONE WITH PCM CONTRIBUTION TO GRADIENT ....'
      CALL TIMIT(1)
      RETURN
      END
C*MODULE PCMDER  *DECK DERBEM
      SUBROUTINE DERBEM(DDM1,DD,DEN,VEC,DQN,DQE,EF,VDER,DMATM1,
     *                  DERPUNT,DERTES,DERCENTR,DERRAD,
     *                  DERSOLV,DERCAV,DERDIS,DERREP,
     *                  QNDER,QEDER,L1,L2,NTE,NATM,NESFT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
      DIMENSION DDM1(NTE,NTE),DD(NTE,NTE),DEN(L2),VEC(L2),DQN(NTE),
     *          DQE(NTE),EF(2*NTE,3),VDER(NTE,3,NATM),DMATM1(NTE,NTE),
     *          DERPUNT(NTE,NATM,3,3),DERTES(NTE,NATM,3),
     *          DERCENTR(NESFT,NATM,3,3),DERRAD(NESFT,NATM,3),
     *          DERSOLV(3,NATM),DERCAV(3,NATM),DERDIS(3,NATM),
     *          DERREP(3,NATM),QNDER(3,NATM),QEDER(3,NATM)
C
C   hui li
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /ELPROP/ ELDLOC,ELMLOC,ELPLOC,ELFLOC,
     *                IEDEN,IEMOM,IEPOT,IEFLD,MODENS,
     *                IEDOUT,IEMOUT,IEPOUT,IEFOUT,
     *                IEDINT,IEMINT,IEPINT,IEFINT
      COMMON /FRZCRT/ IFZCRT(3*MXATM),NFZCRT
      COMMON /FUNCT / E,EG(3,MXATM)
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
      COMMON /PCMPRT/ GCAVP,GCAVS,GDISP,GREP,EHFGAS
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
      COMMON /PCMCAV/ OMEGA,RET,FRO,ALPHA(MXSP),RIN(MXSP),ICENT,
     *                IPRINT,IRETCAV
      COMMON /PCMSPH/ INA(MXSP),INF(MXSP)
C
      DATA AUTOKAL /627.509541D+00/
      DATA ZERO, PT5, ONE /0.0D+00, 0.5D+00, 1.0D+00/
C---------------------------------------------------------------------
C
C     Calcola la derivata degli elementi di matrice BEM rispetto alle
C     coordinate (ICOORD) degli atomi di soluto (NSJ). Indicando con
C     G(x) la derivata di G rispetto a x, si ha {JCP, 101, 3888 (1994)}:
C     G(x) = 1/2 tr[Ph'(x)] + 1/2 tr[PG'(x)(P)] - tr[S(x)W(P)] + V'(x)nn
C
C     Oltre alle quantita' gia' calcolate da HONDO per il gradiente nel
C     vuoto, rimangono da determinare, in questa routine:
C     1/2 tr[P(J(x)+Y(x))] + 1/2 tr[PX(x)(P)] + 1/2 U(x)nn
C
C     Le matrici J e X e lo scalare Unn sono calcolate nelle routines
C     CCVE, JMAT, XMAT, VNN
C
C---------------------------------------------------------------------
C
C     Solo le opzioni di normalizzazione ICOMP = 0, 2 sono compatibili
C     con il calcolo del gradiente
C
      IF(ICOMP.NE.0.AND.ICOMP.NE.2) THEN
         IF(MASWRK)WRITE(IW,1000)
         CALL ABRT
         STOP
      END IF
 1000   FORMAT(//,'WARNING : OPTION 0 OR 2 FOR CHARGE ',
     *   'NORMALIZATION MUST BE USED',//)
C
C     Se ICOMP = 0, i fattori di normalizzazione devono essere 1
C
      IF(ICOMP.EQ.0) THEN
        FN = ONE
        FE = ONE
      END IF
C
C     Legge la matrice densita'
C
      CALL DAREAD(IDAF,IODA,DEN,L2,16,0)
C
C     1) Preliminare
C     Gradiente dell'energia EFC con le cariche virtuali nucleari
C     fisse: calcolato con una routine (modificata) di HONDO8,
C     usato per la derivata di J
C
      INDQ=1
      CALL CHGDER(DEN,QSN,QNDER,QEDER,L2,NTE,NATM,INDQ)
C
C     2) Preliminare
C     Gradiente dell'energia EFC con le cariche virtuali elettroniche
C     fisse: calcolato con una routine (modificata) di HONDO8,
C     usato per la derivata di X
C
      INDQ=2
      CALL CHGDER(DEN,QSE,QNDER,QEDER,L2,NTE,NATM,INDQ)
C
C     3) Preliminare
C     Gradiente di una "pseudo energia EFC" (uno per ogni tessera),
C     calcolato con una routine (modificata) di HONDO8,
C     usato per il gradiente della carica virtuale elettronica
C
      DO ITS = 1, NTS
        CALL CHGDER2(DEN,VDER,L2,NTE,NATM,ITS)
      ENDDO
C
C     4) Preliminare
C     Campo elettrico sulle tessere
C
      IEFLDOLD=IEFLD
      IEFLD=1
      DO ITS = 1, 2*NTS
        CALL ELFNEW(ITS,DEN,ELFLDX,ELFLDY,ELFLDZ,L1,L2)
        EF(ITS,1) = ELFLDX
        EF(ITS,2) = ELFLDY
        EF(ITS,3) = ELFLDZ
      ENDDO
      IEFLD=IEFLDOLD
C
C     5) Preliminare
C     Se ICOMP=2, riottiene le cariche non normalizzate
C
      IF (ICOMP.EQ.2) THEN
        DO ITS = 1, NTS
          QSN(ITS) = QSN(ITS) / FN
          QSE(ITS) = QSE(ITS) / FE
        ENDDO
      END IF
C
C     Loop sugli atomi e sulle coordinate
C
      DO 100 NSJ = 1, NAT
        DO 100 ICOORD = 1, 3
C
C         Derivata della matrice DMATM1
C
          CALL DERDMAT(NSJ,ICOORD,DDM1,DD,DMATM1,
     *         DERPUNT,DERTES,DERCENTR,DERRAD,NTE,NATM,NESFT)
C
C         Derivata delle cariche virtuali non normalizzate
C
          DO ITS = 1, NTS
            CALL DERQSN(ITS,NSJ,ICOORD,DDM1,DMATM1,DQN(ITS),
     *           DERPUNT,DERTES,DERCENTR,DERRAD,NTE,NATM,NESFT)
            CALL DERQSEB(ITS,NSJ,ICOORD,DDM1,EF,VDER,DMATM1,DQE(ITS),
     *           DERPUNT,DERTES,DERCENTR,DERRAD,NTE,NATM,NESFT)
          ENDDO
C
C         Derivata delle cariche virtuali normalizzate
C
          DO ITS = 1, NTS
            DQN(ITS) = FN * DQN(ITS)
            DQE(ITS) = FE * DQE(ITS)
          ENDDO
C
          DERJ = ZERO
          DERY = ZERO
          DERX = ZERO
          DERU = ZERO
C
C         Una parte della derivata del potenziale elettronico
C         moltiplicata per la carica virtuale nucleare e elettronica
C         e' gia' stata calcolata e sommata
C         sulle tessere nello step preliminare 1:
C
          DERJ = QNDER(ICOORD,NSJ)
          DERX = QEDER(ICOORD,NSJ)
C
C
C         Loop sulle tessere
C
          DO 200 ITS = 1, NTS
            L = ISPHE(ITS)
C
C           Punto rappresentativo
C
            XI = XCTS(ITS)
            YI = YCTS(ITS)
            ZI = ZCTS(ITS)
C
C           Derivata del punto rappresentativo
C
            DXI = DERPUNT(ITS,NSJ,ICOORD,1)+DERCENTR(L,NSJ,ICOORD,1)
            DYI = DERPUNT(ITS,NSJ,ICOORD,2)+DERCENTR(L,NSJ,ICOORD,2)
            DZI = DERPUNT(ITS,NSJ,ICOORD,3)+DERCENTR(L,NSJ,ICOORD,3)
C
C           Campo elettrico elettronico sulla tessera
C           per la derivata del punto
C
            PROD=EF(ITS,1)*DXI+EF(ITS,2)*DYI+EF(ITS,3)*DZI
            DERJ = DERJ - FN * QSN(ITS) * PROD
            DERX = DERX - FE * QSE(ITS) * PROD
C
C           Potenziale elettronico sulla tessera:
C
               CALL INTMEP(VEC,XCTS(ITS),YCTS(ITS),ZCTS(ITS))
            VEL = - TRACEP(DEN,VEC,L1)
C
C           Potenziale nucleare sulla tessera e sua derivata
C
            VNUC = ZERO
            DVNUC = ZERO
C
C           Loop sui nuclei
C
            DO NUC = 1, NAT
              XN = C(1,NUC)
              YN = C(2,NUC)
              ZN = C(3,NUC)
              DIST = SQRT( (XN-XI)**2+(YN-YI)**2+(ZN-ZI)**2 )
              VNUC = VNUC + ZAN(NUC) / DIST
C
              IF(ICENT.EQ.2)THEN
              DO J=1,NESFP
              IF(NUC.EQ.INA(J))THEN
                PROD = (XI - XN) * (DERCENTR(NUC,NSJ,ICOORD,1) - DXI) +
     *                 (YI - YN) * (DERCENTR(NUC,NSJ,ICOORD,2) - DYI) +
     *                 (ZI - ZN) * (DERCENTR(NUC,NSJ,ICOORD,3) - DZI)
              ELSE
               DDX=0.D0
               DDY=0.D0
               DDZ=0.D0
               ICENTRO=NSJ-NUC
               IF(ICENTRO.EQ.0)THEN
                IF(ICOORD.EQ.1)DDX=1.D0
                IF(ICOORD.EQ.2)DDY=1.D0
                IF(ICOORD.EQ.3)DDZ=1.D0
               ENDIF
               PROD = (XI - XN) * (DDX - DXI) +
     *                (YI - YN) * (DDY - DYI) +
     *                (ZI - ZN) * (DDZ - DZI)
              ENDIF
              ENDDO
C
              ELSE
                PROD = (XI - XN) * (DERCENTR(NUC,NSJ,ICOORD,1) - DXI) +
     *                 (YI - YN) * (DERCENTR(NUC,NSJ,ICOORD,2) - DYI) +
     *                 (ZI - ZN) * (DERCENTR(NUC,NSJ,ICOORD,3) - DZI)
              ENDIF
C
              DVNUC = DVNUC + ZAN(NUC) * PROD / DIST**3
            ENDDO
C
            DERJ = DERJ + VEL * DQN(ITS)
            DERX = DERX + VEL * DQE(ITS)
            DERY = DERY + DVNUC * FE * QSE(ITS) + VNUC * DQE(ITS)
            DERU = DERU + DVNUC * FN * QSN(ITS) + VNUC * DQN(ITS)
C
 200      CONTINUE
C
        DERSOLV(ICOORD,NSJ) = PT5 * ( DERJ + DERY + DERX + DERU )
C
 100  CONTINUE
C
C     Se ICOMP=2, riottiene le cariche normalizzate
C
      IF (ICOMP.EQ.2) THEN
        DO ITS = 1, NTS
          QSN(ITS) = QSN(ITS) * FN
          QSE(ITS) = QSE(ITS) * FE
        ENDDO
      END IF
C
C     if(maswrk) call timit(1)
C
C     compute dercav, derdis, derrep numerically (finite displacement)
C     because they are cheap. note nprint=817 characterizes this.
C
      CALL VCLR(DERCAV,1,3*NATM)
      CALL VCLR(DERDIS,1,3*NATM)
      CALL VCLR(DERREP,1,3*NATM)
C
      IF(ICAV.EQ.1.OR.IDISP.EQ.1)THEN
      NPRTBK=NPRINT
      NPRINT=817
C
      IF(ICAV.EQ.1) CALL CAVIT
      IF(IDISP.EQ.1) CALL DISRPM
      GCAV0=GCAVP
      GDIS0=GDISP
      GREP0=GREP
C
      DO 150 IAT = 1, NAT
        DO IC = 1, 3
C         -- skip frozen coordinates to save time
          IF(NFZCRT.GT.0)THEN
            III=(IAT-1)*3+IC
            DO I=1,3*MXATM
              IF(IFZCRT(I).EQ.III)GOTO 150
            ENDDO
          END IF
C
C         -- displacement --
C            note it is a.u. for c(*)
C            1.0d-06 is good, cannot be smaller
C
          COLD=C(IC,IAT)
C
          C(IC,IAT)=COLD+1.0D-06
C
C         -- compute cavitation, dispersion, repulsion energy --
C
          IF(ICAV.EQ.1.AND.RIN(IAT).GT.0.020)THEN
            CALL CAVIT
          ELSE
            GCAVP=GCAV0
          END IF
          IF(IDISP.EQ.1) CALL DISRPM
C
C         -- compute dercav, derdis, derrep --
C            note it is also a.u. here
C
          DERCAV(IC,IAT)=(GCAVP-GCAV0)/1.0D-06/AUTOKAL
          DERDIS(IC,IAT)=(GDISP-GDIS0)/1.0D-06/AUTOKAL
          DERREP(IC,IAT)=(GREP -GREP0)/1.0D-06/AUTOKAL
C
          C(IC,IAT)=COLD
        END DO
 150  CONTINUE
C
      NPRINT=NPRTBK
      END IF
C
C     add contributions to the gradient vector
C
C     if(maswrk)
C    *write(iw,300)'ATOM','  XYZ',
C    *'     DERCAV','     DERDIS','     DERREP'
C300  format(/1x,a4,a5,3a11)
C
      DO 610 IAT=1,NAT
         DO 600 IXYZ=1,3
                          EG(IXYZ,IAT)=EG(IXYZ,IAT)
     *                                +DERSOLV(IXYZ,IAT)
           IF(ICAV.EQ.1)  EG(IXYZ,IAT)=EG(IXYZ,IAT)
     *                                +DERCAV(IXYZ,IAT)
           IF(IDISP.EQ.1) EG(IXYZ,IAT)=EG(IXYZ,IAT)
     *                                +DERDIS(IXYZ,IAT)
     *                                +DERREP(IXYZ,IAT)
C
C         if(maswrk)write(iw,210) iat,ixyz,dercav(ixyz,iat),
C    *              derdis(ixyz,iat),derrep(ixyz,iat)
C210      format(i5,i5,3f11.7)
C
  600    CONTINUE
  610 CONTINUE
C
      RETURN
      END
C*MODULE PCMDER  *DECK GRADVEC
      SUBROUTINE GRADVEC(X,Y,Z,DX,DY,DZ,GRADX,GRADY,GRADZ)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C     Calcola il gradiente del vettore:
C     P / |P|^3;   P = (X, Y, Z)
C
      DNORM = SQRT( X * X + Y * Y + Z * Z)
      PROD = X * DX + Y * DY + Z * DZ
C
      GRADX = DX - 3.0D+00 * PROD * X / DNORM**2
      GRADY = DY - 3.0D+00 * PROD * Y / DNORM**2
      GRADZ = DZ - 3.0D+00 * PROD * Z / DNORM**2
C
      GRADX = GRADX / DNORM**3
      GRADY = GRADY / DNORM**3
      GRADZ = GRADZ / DNORM**3
C
      RETURN
      END
C*MODULE PCMDER  *DECK DERDMAT
      SUBROUTINE DERDMAT(NSJ,ICOORD,DERDM1,DD,DMATM1,
     *           DERPUNT,DERTES,DERCENTR,DERRAD,NTE,NATM,NESFT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
      DIMENSION DERDM1(NTE,NTE),DD(NTE,NTE),DMATM1(NTE,NTE),
     *          DERPUNT(NTE,NATM,3,3),DERTES(NTE,NATM,3),
     *          DERCENTR(NESFT,NATM,3,3),DERRAD(NESFT,NATM,3)
C
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
C
      PARAMETER (TOANGS=0.52917724924D+00, ANTOAU=1.0D+00/TOANGS)
C
      DATA FPI/12.56637062D+00/
C
C     Calcola la matrice DERDM1, derivata di DMATM1 rispetto alla
C     coordinata ICOORD della sfera NSJ:
C     DERDM1(x) = - DMATM1 * DERDM(x) * DMATM1
C
C     Loop sugli elementi di DMAT
C
      DO ITS = 1, NTS
        L = ISPHE(ITS)
C
C       Normale (interna!)
C
        XNI = (XE(L) - XCTS(ITS)) / RE(L)
        YNI = (YE(L) - YCTS(ITS)) / RE(L)
        ZNI = (ZE(L) - ZCTS(ITS)) / RE(L)
C
C       Derivata del versore normale
C
        DXNI = - DERPUNT(ITS,NSJ,ICOORD,1) / RE(L) -
     *         XNI * DERRAD(L,NSJ,ICOORD) / RE(L)
        DYNI = - DERPUNT(ITS,NSJ,ICOORD,2) / RE(L) -
     *         YNI * DERRAD(L,NSJ,ICOORD) / RE(L)
        DZNI = - DERPUNT(ITS,NSJ,ICOORD,3) / RE(L) -
     *         ZNI * DERRAD(L,NSJ,ICOORD) / RE(L)
C
        DO JTS = 1, NTS
          LJ = ISPHE(JTS)
C
C         Elementi diagonali di DMAT(x)
C
          IF (ITS.EQ.JTS) THEN
            XI = SQRT( AS(ITS) / (FPI * RE(L) * RE(L)) )
            FAC = 1.0D+00 / (4.0D+00 * RE(L) * RE(L) * XI)
            DERDM1(ITS,ITS) = FAC*(DERTES(ITS,NSJ,ICOORD)*ANTOAU -
     *      2.0D+00 * AS(ITS) * DERRAD(L,NSJ,ICOORD) / RE(L) )
C
C         Elementi fuori diagonale di DMAT(x)
C
          ELSE
            XIJ = XCTS(ITS) - XCTS(JTS)
            YIJ = YCTS(ITS) - YCTS(JTS)
            ZIJ = ZCTS(ITS) - ZCTS(JTS)
            DIJ = SQRT( XIJ*XIJ + YIJ*YIJ + ZIJ*ZIJ )
C
            DXIJ=DERPUNT(ITS,NSJ,ICOORD,1)+DERCENTR(L,NSJ,ICOORD,1)
     *          -DERPUNT(JTS,NSJ,ICOORD,1)-DERCENTR(LJ,NSJ,ICOORD,1)
            DYIJ=DERPUNT(ITS,NSJ,ICOORD,2)+DERCENTR(L,NSJ,ICOORD,2)
     *          -DERPUNT(JTS,NSJ,ICOORD,2)-DERCENTR(LJ,NSJ,ICOORD,2)
            DZIJ=DERPUNT(ITS,NSJ,ICOORD,3)+DERCENTR(L,NSJ,ICOORD,3)
     *          -DERPUNT(JTS,NSJ,ICOORD,3)-DERCENTR(LJ,NSJ,ICOORD,3)
C
            PROD = (XIJ*XNI + YIJ*YNI + ZIJ*ZNI) / DIJ**3
            DUM = DERTES(JTS,NSJ,ICOORD) * ANTOAU * PROD
C
            CALL GRADVEC(XIJ,YIJ,ZIJ,DXIJ,DYIJ,DZIJ,DX,DY,DZ)
            PROD = DX * XNI + DY * YNI + DZ * ZNI
            DUM = DUM + AS(JTS) * PROD
C
            PROD = (XIJ*DXNI + YIJ*DYNI + ZIJ*DZNI) / DIJ**3
            DUM = DUM + AS(JTS) * PROD
C
            DERDM1(ITS,JTS) = - DUM
          END IF
        ENDDO
      ENDDO
C
      CALL MRARBR(DMATM1,NTE,NTS,NTS,DERDM1,NTE,NTE,DD,NTE)
      CALL MRARBR(DD,NTE,NTE,NTE,DMATM1,NTE,NTS,DERDM1,NTE)
      RETURN
      END
C*MODULE PCMDER  *DECK DERQSN
      SUBROUTINE DERQSN(ITS,NSJ,ICOORD,DDM1,DMATM1,DQN,
     *           DERPUNT,DERTES,DERCENTR,DERRAD,NTE,NATM,NESFT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
      DIMENSION DDM1(NTE,NTE),DMATM1(NTE,NTE),
     *          DERPUNT(NTE,NATM,3,3),DERTES(NTE,NATM,3),
     *          DERCENTR(NESFT,NATM,3,3),DERRAD(NESFT,NATM,3)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
      COMMON /PCMCAV/ OMEGA,RET,FRO,ALPHA(MXSP),RIN(MXSP),ICENT,
     *                IPRINT,IRETCAV
      COMMON /PCMSPH/ INA(MXSP),INF(MXSP)
C
      PARAMETER (TOANGS=0.52917724924D+00, ANTOAU=1.0D+00/TOANGS)
C
      DATA ZERO/0.0D+00/
C
C     Calcola la derivata della carica virtuale nucleare sulla
C     tessera ITS rispetto alla coordinata ICOORD della sfera NSJ.
C
C     1) Contributo dalla derivata dell'area di ITS
C
      DQN = DERTES(ITS,NSJ,ICOORD) * ANTOAU * QSN(ITS) / AS(ITS)
C
C     2) Loop sulle tessere
C
      DERQ = ZERO
      DO JTS = 1, NTS
        L = ISPHE(JTS)
C
C       Punto rappresentativo
C
        XJ = XCTS(JTS)
        YJ = YCTS(JTS)
        ZJ = ZCTS(JTS)
C
C       Normale (interna!)
        XNJ = (XE(L) - XJ) / RE(L)
        YNJ = (YE(L) - YJ) / RE(L)
        ZNJ = (ZE(L) - ZJ) / RE(L)
C
C       Derivata del versore normale
        DXNJ = - DERPUNT(JTS,NSJ,ICOORD,1) / RE(L) -
     *         XNJ * DERRAD(L,NSJ,ICOORD) / RE(L)
        DYNJ = - DERPUNT(JTS,NSJ,ICOORD,2) / RE(L) -
     *         YNJ * DERRAD(L,NSJ,ICOORD) / RE(L)
        DZNJ = - DERPUNT(JTS,NSJ,ICOORD,3) / RE(L) -
     *         ZNJ * DERRAD(L,NSJ,ICOORD) / RE(L)
C
C       Loop sui nuclei
        SUM1 = ZERO
        SUM2 = ZERO
        SUM3 = ZERO
        DO NUC = 1, NAT
          XNUCJ = C(1,NUC) - XJ
          YNUCJ = C(2,NUC) - YJ
          ZNUCJ = C(3,NUC) - ZJ
          DNORM = SQRT(XNUCJ*XNUCJ+YNUCJ*YNUCJ+ZNUCJ*ZNUCJ)
C
C         Derivata del vettore nucleo-tessera
C
          IF(ICENT.EQ.2)THEN
            DO J=1,NESFP
              IF(NUC.EQ.INA(J))THEN
                 DXNUCJ = DERCENTR(NUC,NSJ,ICOORD,1) -
     *             DERPUNT(JTS,NSJ,ICOORD,1) - DERCENTR(L,NSJ,ICOORD,1)
                 DYNUCJ = DERCENTR(NUC,NSJ,ICOORD,2) -
     *             DERPUNT(JTS,NSJ,ICOORD,2) - DERCENTR(L,NSJ,ICOORD,2)
                 DZNUCJ = DERCENTR(NUC,NSJ,ICOORD,3) -
     *             DERPUNT(JTS,NSJ,ICOORD,3) - DERCENTR(L,NSJ,ICOORD,3)
              ELSE
               DDX=0.D0
               DDY=0.D0
               DDZ=0.D0
               ICENTRO=NSJ-NUC
               IF(ICENTRO.EQ.0)THEN
                IF(ICOORD.EQ.1)DDX=1.D0
                IF(ICOORD.EQ.2)DDY=1.D0
                IF(ICOORD.EQ.3)DDZ=1.D0
               ENDIF
               DXNUCJ = DDX - DERPUNT(JTS,NSJ,ICOORD,1)
     *                  - DERCENTR(L,NSJ,ICOORD,1)
               DYNUCJ = DDY - DERPUNT(JTS,NSJ,ICOORD,2)
     *                  - DERCENTR(L,NSJ,ICOORD,2)
               DZNUCJ = DDZ - DERPUNT(JTS,NSJ,ICOORD,3)
     *                  - DERCENTR(L,NSJ,ICOORD,3)
              ENDIF
            ENDDO
          ELSE
            DXNUCJ = DERCENTR(NUC,NSJ,ICOORD,1) -
     *        DERPUNT(JTS,NSJ,ICOORD,1) - DERCENTR(L,NSJ,ICOORD,1)
            DYNUCJ = DERCENTR(NUC,NSJ,ICOORD,2) -
     *        DERPUNT(JTS,NSJ,ICOORD,2) - DERCENTR(L,NSJ,ICOORD,2)
            DZNUCJ = DERCENTR(NUC,NSJ,ICOORD,3) -
     *        DERPUNT(JTS,NSJ,ICOORD,3) - DERCENTR(L,NSJ,ICOORD,3)
          ENDIF
C
          PROD = (XNUCJ*XNJ + YNUCJ*YNJ + ZNUCJ*ZNJ) / DNORM**3
          SUM1 = SUM1 + ZAN(NUC) * PROD
C
          CALL GRADVEC(XNUCJ,YNUCJ,ZNUCJ,
     *                 DXNUCJ,DYNUCJ,DZNUCJ,DX,DY,DZ)
          PROD = DX * XNJ + DY * YNJ + DZ * ZNJ
          SUM2 = SUM2 + ZAN(NUC) * PROD
C
          PROD=(XNUCJ*DXNJ+YNUCJ*DYNJ+ZNUCJ*DZNJ)/DNORM**3
          SUM3 = SUM3 + ZAN(NUC) * PROD
        ENDDO
C
        DERQ = DERQ + DDM1(ITS,JTS) * SUM1
C
        DERQ = DERQ + DMATM1(ITS,JTS) * (SUM2 + SUM3)
      ENDDO
C
      DQN = DQN + AS(ITS) * DERQ
      RETURN
      END
C*MODULE PCMDER  *DECK DERQSEB
      SUBROUTINE DERQSEB(ITS,NSJ,ICOORD,DDM1,EF,VDER,DMATM1,DQE,
     *           DERPUNT,DERTES,DERCENTR,DERRAD,NTE,NATM,NESFT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
      DIMENSION DDM1(NTE,NTE),EF(2*NTE,3),VDER(NTE,3,NATM),
     *          DMATM1(NTE,NTE),
     *          DERPUNT(NTE,NATM,3,3),DERTES(NTE,NATM,3),
     *          DERCENTR(NESFT,NATM,3,3),DERRAD(NESFT,NATM,3)
C
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
C
      PARAMETER (TOANGS=0.52917724924D+00, ANTOAU=1.0D+00/TOANGS)
C
      DATA ZERO/0.0D+00/
C
C     Calcola la derivata della carica virtuale elettronica sulla
C     tessera ITS rispetto alla coordinata ICOORD della sfera NSJ.
C
C     1) Contributo dalla derivata dell'area di ITS
      DQE = DERTES(ITS,NSJ,ICOORD) * ANTOAU * QSE(ITS) / AS(ITS)
C
C     2) Loop sulle tessere
      DERQ = ZERO
      DO JTS = 1, NTS
        L = ISPHE(JTS)
C
C       Punto rappresentativo
        XJ = XCTS(JTS)
        YJ = YCTS(JTS)
        ZJ = ZCTS(JTS)
C
C       Derivata del punto rappresentativo
        DXJ = DERPUNT(JTS,NSJ,ICOORD,1)+DERCENTR(L,NSJ,ICOORD,1)
        DYJ = DERPUNT(JTS,NSJ,ICOORD,2)+DERCENTR(L,NSJ,ICOORD,2)
        DZJ = DERPUNT(JTS,NSJ,ICOORD,3)+DERCENTR(L,NSJ,ICOORD,3)
C
C       Normale (interna!)
        XNJ = (XE(L) - XJ) / RE(L)
        YNJ = (YE(L) - YJ) / RE(L)
        ZNJ = (ZE(L) - ZJ) / RE(L)
C
C       Derivata del versore normale
        DXNJ = - DERPUNT(JTS,NSJ,ICOORD,1) / RE(L) -
     *         XNJ * DERRAD(L,NSJ,ICOORD) / RE(L)
        DYNJ = - DERPUNT(JTS,NSJ,ICOORD,2) / RE(L) -
     *         YNJ * DERRAD(L,NSJ,ICOORD) / RE(L)
        DZNJ = - DERPUNT(JTS,NSJ,ICOORD,3) / RE(L) -
     *         ZNJ * DERRAD(L,NSJ,ICOORD) / RE(L)
C
C       Campo elettrico sulla tessera e sul punto interno
        FLD1X = EF(JTS,1)
        FLD1Y = EF(JTS,2)
        FLD1Z = EF(JTS,3)
        FLD2X = EF(JTS+NTS,1)
        FLD2Y = EF(JTS+NTS,2)
        FLD2Z = EF(JTS+NTS,3)
C
C       Componente normale del campo sulla tessera
        FLDN = FLD1X * XNJ + FLD1Y * YNJ + FLD1Z * ZNJ
C
        DERQ = DERQ - DDM1(ITS,JTS) * FLDN
C
C       Derivata della componente normale del campo sulla tessera
        PROD = FLD1X * DXJ + FLD1Y * DYJ + FLD1Z * DZJ
C
        PROD1 = FLD2X * (DXJ+DR*DXNJ) + FLD2Y * (DYJ+DR*DYNJ) +
     *          FLD2Z * (DZJ + DR * DZNJ)
C
        FLDER = ( VDER(JTS,ICOORD,NSJ) + PROD - PROD1 ) / DR
C
        DERQ = DERQ + DMATM1(ITS,JTS) * FLDER
C
      ENDDO
      DQE = DQE + AS(ITS) * DERQ
      RETURN
      END
C*MODULE PCMDER  *DECK CHGDER
      SUBROUTINE CHGDER(DM,QS,QNDER,QEDER,L2,NTS,NATM,INDQ)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL NORM
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      DIMENSION DM(L2),QS(NTS),QNDER(3,NATM),QEDER(3,NATM)
      DIMENSION DIJ(225),IJX(35),IJY(35),IJZ(35),
     *          XS(6,5,5),YS(6,5,5),ZS(6,5,5),
     *          DXSDI(5,5,5),DYSDI(5,5,5),DZSDI(5,5,5)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500, MXAO=2047,
     *           MXTS=2500, MXTSPT=2*MXTS)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTESS
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,
     *                YJ,ZJ,NI,NJ
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
      PARAMETER (RLN10=2.30258D+00)
      PARAMETER (PI212=1.12837916709550D+00)
      PARAMETER (SQRT3=1.73205080756888D+00)
      PARAMETER (SQRT5=2.23606797749979D+00)
      PARAMETER (SQRT7=2.64575131106459D+00)
C
      DATA IJX    / 1, 2, 1, 1, 3, 1, 1, 2, 2, 1,
     1              4, 1, 1, 3, 3, 2, 1, 2, 1, 2,
     2              5, 1, 1, 4, 4, 2, 1, 2, 1, 3,
     3              3, 1, 3, 2, 2/
      DATA IJY    / 1, 1, 2, 1, 1, 3, 1, 2, 1, 2,
     1              1, 4, 1, 2, 1, 3, 3, 1, 2, 2,
     2              1, 5, 1, 2, 1, 4, 4, 1, 2, 3,
     3              1, 3, 2, 3, 2/
      DATA IJZ    / 1, 1, 1, 2, 1, 1, 3, 1, 2, 2,
     1              1, 1, 4, 1, 2, 1, 2, 3, 3, 2,
     2              1, 1, 5, 1, 2, 1, 2, 4, 4, 1,
     3              3, 3, 2, 2, 3/
C
      CALL DERCHK(MAXDER)
      IF(MAXDER.EQ.0) RETURN
C
      IF(INDQ.EQ.1) THEN
        CALL VCLR(QNDER,1,3*NATM)
      ELSE
        CALL VCLR(QEDER,1,3*NATM)
      END IF
C
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C     ----- CALCULATE FIELD DERIVATIVES -----
C
      NDER=1
C
C     ----- ISHELL -----
C
C     hui li  initialize parallel
      IPCOUNT = ME - 1
C
      DO 9000 II=1,NSHELL
C
C     hui li  go parallel!
      IF(GOPARR) THEN
        IPCOUNT = IPCOUNT + 1
        IF(MOD(IPCOUNT,NPROC).NE.0) GO TO 9000
      END IF
C
      I=KATOM(II)
      XI=C(1,I)
      YI=C(2,I)
      ZI=C(3,I)
      I1=KSTART(II)
      I2=I1+KNG(II)-1
      LIT=KTYPE(II)
      MINI=KMIN(II)
      MAXI=KMAX(II)
      LOCI=KLOC(II)-MINI
C
      LITDER=LIT+NDER
      IAT=I
C
C     ----- JSHELL -----
C
      DO 8000 JJ=1,NSHELL
C
      J=KATOM(JJ)
      XJ=C(1,J)
      YJ=C(2,J)
      ZJ=C(3,J)
      J1=KSTART(JJ)
      J2=J1+KNG(JJ)-1
      LJT=KTYPE(JJ)
      MINJ=KMIN(JJ)
      MAXJ=KMAX(JJ)
      LOCJ=KLOC(JJ)-MINJ
C
      RR=(XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
      NROOTS=(LIT+LJT+NDER-2)/2+1
C
C     ----- I PRIMITIVE -----
C
      DO 7000 IG=I1,I2
      AI=EX(IG)
      ARRI=AI*RR
      AXI=AI*XI
      AYI=AI*YI
      AZI=AI*ZI
      CSI=CS(IG)
      CPI=CP(IG)
      CDI=CD(IG)
      CFI=CF(IG)
      CGI=CG(IG)
C
C     ----- J PRIMITIVE -----
C
      DO 6000 JG=J1,J2
      AJ=EX(JG)
      AA =AI+AJ
      AA1=ONE/AA
      DUM=AJ*ARRI*AA1
      IF(DUM.GT.TOL) GO TO 6000
      FAC= EXP(-DUM)
      CSJ=CS(JG)
      CPJ=CP(JG)
      CDJ=CD(JG)
      CFJ=CF(JG)
      CGJ=CG(JG)
      AX=(AXI+AJ*XJ)*AA1
      AY=(AYI+AJ*YJ)*AA1
      AZ=(AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR -----
C
      IJ=0
      DO 360 I=MINI,MAXI
      GO TO (110,120,220,220,130,220,220,140,220,220,
     1       150,220,220,160,220,220,220,220,220,170,
     2       180,220,220,190,220,220,220,220,220,200,
     3       220,220,210,220,220),I
  110 DUM1=CSI*FAC
      GO TO 220
  120 DUM1=CPI*FAC
      GO TO 220
  130 DUM1=CDI*FAC
      GO TO 220
  140 IF(NORM) DUM1=DUM1*SQRT3
      GO TO 220
  150 DUM1=CFI*FAC
      GO TO 220
  160 IF(NORM) DUM1=DUM1*SQRT5
      GO TO 220
  170 IF(NORM) DUM1=DUM1*SQRT3
      GO TO 220
  180 DUM1=CGI*FAC
      GO TO 220
  190 IF(NORM) DUM1=DUM1*SQRT7
      GO TO 220
  200 IF(NORM) DUM1=DUM1*SQRT5/SQRT3
      GO TO 220
  210 IF(NORM) DUM1=DUM1*SQRT3
  220 CONTINUE
C
      DO 360 J=MINJ,MAXJ
      GO TO (230,250,350,350,260,350,350,270,350,350,
     1       280,350,350,290,350,350,350,350,350,300,
     2       310,350,350,320,350,350,350,350,350,330,
     3       350,350,340,350,350),J
  230 DUM2=DUM1*CSJ
      GO TO 350
  250 DUM2=DUM1*CPJ
      GO TO 350
  260 DUM2=DUM1*CDJ
      GO TO 350
  270 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 350
  280 DUM2=DUM1*CFJ
      GO TO 350
  290 IF(NORM) DUM2=DUM2*SQRT5
      GO TO 350
  300 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 350
  310 DUM2=DUM1*CGJ
      GO TO 350
  320 IF(NORM) DUM2=DUM2*SQRT7
      GO TO 350
  330 IF(NORM) DUM2=DUM2*SQRT5/SQRT3
      GO TO 350
  340 IF(NORM) DUM2=DUM2*SQRT3
  350 CONTINUE
C
      NN=IA(MAX0(LOCI+I,LOCJ+J))
     1  +   MIN0(LOCI+I,LOCJ+J)
      DEN=DM(NN)
      DEN=DEN+DEN
      IJ=IJ+1
  360 DIJ(IJ)=DUM2*DEN
C
C     ----- -FLD- DERIVATIVES -----
C
      AAX=AA*AX
      AAY=AA*AY
      AAZ=AA*AZ
      DO 500 IC=1,NTS
      ZNUC = -QS(IC)
      XC = XCTS(IC)
      YC = YCTS(IC)
      ZC = ZCTS(IC)
      XX=AA*((AX-XC)**2+(AY-YC)**2+(AZ-ZC)**2)
      IF (NROOTS.LE.3) CALL RT123
      IF (NROOTS.EQ.4) CALL ROOT4
      IF (NROOTS.EQ.5) CALL ROOT5
C
C       LOOP OVER ROOTS OF RYS POLYNOMIAL TO CALCULATE INTEGRALS
C
      DO 420  K=1,NROOTS
      UU = AA*U(K)
      WW = W(K)*ZNUC
      TT = ONE/(AA+UU)
      T  = SQRT(TT)
      X0 = (AAX + UU*XC)*TT
      Y0 = (AAY + UU*YC)*TT
      Z0 = (AAZ + UU*ZC)*TT
C
      DO 370 J=1,LJT
      NJ=J
      DO 370 I=1,LITDER
      NI=I
      CALL DSXYZ
      XS(I,J,K)=XINT
      YS(I,J,K)=YINT
      ZS(I,J,K)=ZINT*WW
  370 CONTINUE
C
      CALL DERI(DXSDI(1,1,K),DYSDI(1,1,K),DZSDI(1,1,K),
     *                XS(1,1,K),YS(1,1,K),ZS(1,1,K),LIT,LJT,AI)
  420 CONTINUE
C
      IJ=0
      DO 390 I=MINI,MAXI
      IX=IJX(I)
      IY=IJY(I)
      IZ=IJZ(I)
      DO 380 J=MINJ,MAXJ
      JX=IJX(J)
      JY=IJY(J)
      JZ=IJZ(J)
      DUMX=ZERO
      DUMY=ZERO
      DUMZ=ZERO
C
      DO 430 IROOT=1,NROOTS
      DUMX=DUMX+DXSDI(IX,JX,IROOT)*YS(IY,JY,IROOT)*ZS(IZ,JZ,IROOT)
      DUMY=DUMY+DYSDI(IY,JY,IROOT)*XS(IX,JX,IROOT)*ZS(IZ,JZ,IROOT)
  430 DUMZ=DUMZ+DZSDI(IZ,JZ,IROOT)*YS(IY,JY,IROOT)*XS(IX,JX,IROOT)
      IJ=IJ+1
C
      IF(INDQ.EQ.1) THEN
        QNDER(1,IAT)=QNDER(1,IAT)+DUMX*(AA1*PI212*DIJ(IJ))
        QNDER(2,IAT)=QNDER(2,IAT)+DUMY*(AA1*PI212*DIJ(IJ))
        QNDER(3,IAT)=QNDER(3,IAT)+DUMZ*(AA1*PI212*DIJ(IJ))
      ELSE
        QEDER(1,IAT)=QEDER(1,IAT)+DUMX*(AA1*PI212*DIJ(IJ))
        QEDER(2,IAT)=QEDER(2,IAT)+DUMY*(AA1*PI212*DIJ(IJ))
        QEDER(3,IAT)=QEDER(3,IAT)+DUMZ*(AA1*PI212*DIJ(IJ))
      END IF
  380 CONTINUE
  390 CONTINUE
  500 CONTINUE
C
C     ----- END LOOPS OVER PRIMITVES AND SHELLS -----
C
 6000 CONTINUE
 7000 CONTINUE
 8000 CONTINUE
 9000 CONTINUE
C
C  hui li  sum up
        IF(GOPARR)THEN
          IF(INDQ.EQ.1) CALL DDI_GSUMF(2450,QNDER,3*NATM)
          IF(INDQ.EQ.2) CALL DDI_GSUMF(2450,QEDER,3*NATM)
        END IF
C
      RETURN
      END
C*MODULE PCMDER  *DECK CHGDER2
      SUBROUTINE CHGDER2(DM,VELDER,L2,NTS,NATM,ITS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL NORM
C
      DIMENSION DM(L2),VELDER(NTS,3,NATM)
      DIMENSION DIJ(225),IJX(35),IJY(35),IJZ(35),
     *          XS(6,5,5),YS(6,5,5),ZS(6,5,5),
     *          DXSDI(5,5,5),DYSDI(5,5,5),DZSDI(5,5,5)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500, MXAO=2047,
     *           MXTS=2500, MXTSPT=2*MXTS)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTESS
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,
     *                YJ,ZJ,NI,NJ
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
      PARAMETER (RLN10=2.30258D+00)
      PARAMETER (PI212=1.12837916709550D+00)
      PARAMETER (SQRT3=1.73205080756888D+00)
      PARAMETER (SQRT5=2.23606797749979D+00)
      PARAMETER (SQRT7=2.64575131106459D+00)
C
      DATA IJX    / 1, 2, 1, 1, 3, 1, 1, 2, 2, 1,
     1              4, 1, 1, 3, 3, 2, 1, 2, 1, 2,
     2              5, 1, 1, 4, 4, 2, 1, 2, 1, 3,
     3              3, 1, 3, 2, 2/
      DATA IJY    / 1, 1, 2, 1, 1, 3, 1, 2, 1, 2,
     1              1, 4, 1, 2, 1, 3, 3, 1, 2, 2,
     2              1, 5, 1, 2, 1, 4, 4, 1, 2, 3,
     3              1, 3, 2, 3, 2/
      DATA IJZ    / 1, 1, 1, 2, 1, 1, 3, 1, 2, 2,
     1              1, 1, 4, 1, 2, 1, 2, 3, 3, 2,
     2              1, 1, 5, 1, 2, 1, 2, 4, 4, 1,
     3              3, 3, 2, 2, 3/
C
      CALL DERCHK(MAXDER)
      IF(MAXDER.EQ.0) RETURN
C
      DO ICC=1,3
        DO IATM=1,NATM
         VELDER(ITS,ICC,IATM)=ZERO
        ENDDO
      ENDDO
C
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C     ----- CALCULATE FIELD DERIVATIVES -----
C
      NDER=1
C
C     ----- ISHELL -----
C
      DO 9000 II=1,NSHELL
C
      I=KATOM(II)
      XI=C(1,I)
      YI=C(2,I)
      ZI=C(3,I)
      I1=KSTART(II)
      I2=I1+KNG(II)-1
      LIT=KTYPE(II)
      MINI=KMIN(II)
      MAXI=KMAX(II)
      LOCI=KLOC(II)-MINI
C
      LITDER=LIT+NDER
      IAT=I
C
C     ----- JSHELL -----
C
      DO 8000 JJ=1,NSHELL
C
      J=KATOM(JJ)
      XJ=C(1,J)
      YJ=C(2,J)
      ZJ=C(3,J)
      J1=KSTART(JJ)
      J2=J1+KNG(JJ)-1
      LJT=KTYPE(JJ)
      MINJ=KMIN(JJ)
      MAXJ=KMAX(JJ)
      LOCJ=KLOC(JJ)-MINJ
C
      RR=(XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
      NROOTS=(LIT+LJT+NDER-2)/2+1
C
C     ----- I PRIMITIVE -----
C
      DO 7000 IG=I1,I2
      AI=EX(IG)
      ARRI=AI*RR
      AXI=AI*XI
      AYI=AI*YI
      AZI=AI*ZI
      DUM=AI+AI
      CSI=CS(IG)
      CPI=CP(IG)
      CDI=CD(IG)
      CFI=CF(IG)
      CGI=CG(IG)
C
C     ----- J PRIMITIVE -----
C
      DO 6000 JG=J1,J2
      AJ=EX(JG)
      AA =AI+AJ
      AA1=ONE/AA
      DUM=AJ*ARRI*AA1
      IF(DUM.GT.TOL) GO TO 6000
      FAC= EXP(-DUM)
      CSJ=CS(JG)
      CPJ=CP(JG)
      CDJ=CD(JG)
      CFJ=CF(JG)
      CGJ=CG(JG)
      AX=(AXI+AJ*XJ)*AA1
      AY=(AYI+AJ*YJ)*AA1
      AZ=(AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR -----
C
      IJ=0
      DO 360 I=MINI,MAXI
      GO TO (110,120,220,220,130,220,220,140,220,220,
     1       150,220,220,160,220,220,220,220,220,170,
     2       180,220,220,190,220,220,220,220,220,200,
     3       220,220,210,220,220),I
  110 DUM1=CSI*FAC
      GO TO 220
  120 DUM1=CPI*FAC
      GO TO 220
  130 DUM1=CDI*FAC
      GO TO 220
  140 IF(NORM) DUM1=DUM1*SQRT3
      GO TO 220
  150 DUM1=CFI*FAC
      GO TO 220
  160 IF(NORM) DUM1=DUM1*SQRT5
      GO TO 220
  170 IF(NORM) DUM1=DUM1*SQRT3
      GO TO 220
  180 DUM1=CGI*FAC
      GO TO 220
  190 IF(NORM) DUM1=DUM1*SQRT7
      GO TO 220
  200 IF(NORM) DUM1=DUM1*SQRT5/SQRT3
      GO TO 220
  210 IF(NORM) DUM1=DUM1*SQRT3
  220 CONTINUE
C
      DO 360 J=MINJ,MAXJ
      GO TO (230,250,350,350,260,350,350,270,350,350,
     1       280,350,350,290,350,350,350,350,350,300,
     2       310,350,350,320,350,350,350,350,350,330,
     3       350,350,340,350,350),J
  230 DUM2=DUM1*CSJ
      GO TO 350
  250 DUM2=DUM1*CPJ
      GO TO 350
  260 DUM2=DUM1*CDJ
      GO TO 350
  270 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 350
  280 DUM2=DUM1*CFJ
      GO TO 350
  290 IF(NORM) DUM2=DUM2*SQRT5
      GO TO 350
  300 IF(NORM) DUM2=DUM2*SQRT3
      GO TO 350
  310 DUM2=DUM1*CGJ
      GO TO 350
  320 IF(NORM) DUM2=DUM2*SQRT7
      GO TO 350
  330 IF(NORM) DUM2=DUM2*SQRT5/SQRT3
      GO TO 350
  340 IF(NORM) DUM2=DUM2*SQRT3
  350 CONTINUE
C
      NN=IA(MAX0(LOCI+I,LOCJ+J))
     1  +   MIN0(LOCI+I,LOCJ+J)
      DEN=DM(NN)
      DEN=DEN+DEN
      IJ=IJ+1
  360 DIJ(IJ)=DUM2*DEN
C
C     ----- -FLD- DERIVATIVES -----
C
      AAX=AA*AX
      AAY=AA*AY
      AAZ=AA*AZ
      DO 500 IC=1,2
      IF(IC.EQ.1) THEN
        XC = XCTS(ITS)
        YC = YCTS(ITS)
        ZC = ZCTS(ITS)
        ZNUC = ONE
      ELSE
        XC = XCTS(ITS+NTS)
        YC = YCTS(ITS+NTS)
        ZC = ZCTS(ITS+NTS)
        ZNUC = - ONE
      END IF
      XX=AA*((AX-XC)**2+(AY-YC)**2+(AZ-ZC)**2)
      IF (NROOTS.LE.3) CALL RT123
      IF (NROOTS.EQ.4) CALL ROOT4
      IF (NROOTS.EQ.5) CALL ROOT5
C
C       LOOP OVER ROOTS OF RYS POLYNOMIAL TO CALCULATE INTEGRALS
C
      DO 420  K=1,NROOTS
      UU = AA*U(K)
      WW = W(K)*ZNUC
      TT = ONE/(AA+UU)
      T  = SQRT(TT)
      X0 = (AAX + UU*XC)*TT
      Y0 = (AAY + UU*YC)*TT
      Z0 = (AAZ + UU*ZC)*TT
C
      DO 370 J=1,LJT
      NJ=J
      DO 370 I=1,LITDER
      NI=I
      CALL DSXYZ
      XS(I,J,K)=XINT
      YS(I,J,K)=YINT
      ZS(I,J,K)=ZINT*WW
  370 CONTINUE
C
      CALL DERI(DXSDI(1,1,K),DYSDI(1,1,K),DZSDI(1,1,K),
     *                XS(1,1,K),YS(1,1,K),ZS(1,1,K),LIT,LJT,AI)
  420 CONTINUE
C
      IJ=0
      DO 390 I=MINI,MAXI
      IX=IJX(I)
      IY=IJY(I)
      IZ=IJZ(I)
      DO 380 J=MINJ,MAXJ
      JX=IJX(J)
      JY=IJY(J)
      JZ=IJZ(J)
      DUMX=ZERO
      DUMY=ZERO
      DUMZ=ZERO
C
      DO 430 IROOT=1,NROOTS
      DUMX=DUMX+DXSDI(IX,JX,IROOT)*YS(IY,JY,IROOT)*ZS(IZ,JZ,IROOT)
      DUMY=DUMY+DYSDI(IY,JY,IROOT)*XS(IX,JX,IROOT)*ZS(IZ,JZ,IROOT)
  430 DUMZ=DUMZ+DZSDI(IZ,JZ,IROOT)*YS(IY,JY,IROOT)*XS(IX,JX,IROOT)
      IJ=IJ+1
C
      VELDER(ITS,1,IAT)=VELDER(ITS,1,IAT)+DUMX*(AA1*PI212*DIJ(IJ))
      VELDER(ITS,2,IAT)=VELDER(ITS,2,IAT)+DUMY*(AA1*PI212*DIJ(IJ))
      VELDER(ITS,3,IAT)=VELDER(ITS,3,IAT)+DUMZ*(AA1*PI212*DIJ(IJ))
  380 CONTINUE
  390 CONTINUE
  500 CONTINUE
C
C     ----- END LOOPS OVER PRIMITVES AND SHELLS -----
C
 6000 CONTINUE
 7000 CONTINUE
 8000 CONTINUE
 9000 CONTINUE
C
      RETURN
      END
C*MODULE PCMDER  *DECK ELFNEW
      SUBROUTINE ELFNEW(ITS,D,EFX,EFY,EFZ,L1,L2)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION D(L2)
C
      PARAMETER (MXTS=2500, MXTSPT=2*MXTS)
C
      COMMON /FMCOM / X(1)
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTESS
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
C     ----- CALCULATE ELECTRIC FIELD/GRADIENT -----
C
      XP=XCTS(ITS)
      YP=YCTS(ITS)
      ZP=ZCTS(ITS)
C
C          PARTITION MEMORY
C
      NVAL = 3
C
      LOADFM = 0
      CALL VALFM(LOADFM)
      IELF   = LOADFM + 1
      IEFW   = IELF   + NVAL*L2
      LAST   = IEFW   + NVAL*225
      NEED  = LAST - LOADFM - 1
      CALL GETFM(NEED)
C
      CALL PRCALC(ELFLD,X(IELF),X(IEFW),NVAL,L2)
C
C            EXPECTATION VALUE FOR ELECTRIC FIELD
C
      INDEX = IELF
      EFX = TRACEP(D,X(INDEX),L1)
      INDEX = INDEX + L2
      EFY = TRACEP(D,X(INDEX),L1)
      INDEX = INDEX + L2
      EFZ = TRACEP(D,X(INDEX),L1)
C
      CALL RETFM(NEED)
      RETURN
      END
C*MODULE PCMDER  *DECK DSXYZ
      SUBROUTINE DSXYZ
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION H(28),W(28),MIN(7),MAX(7)
C
      COMMON /STV   / XINT,YINT,ZINT,T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
      COMMON /HERMIT/ H1,H2(2),H3(3),H4(4),H5(5),H6(6),H7(7)
      COMMON /WERMIT/ W1,W2(2),W3(3),W4(4),W5(5),W6(6),W7(7)
C
      EQUIVALENCE (H(1),H1),(W(1),W1)
C
      DATA MIN  /1,2,4, 7,11,16,22/
      DATA MAX  /1,3,6,10,15,21,28/
      DATA ZERO /0.0D+00/
C
C     ----- GAUSS-HERMITE QUADRATURE USING MINIMUM POINT FORMULA -----
C
      XINT=ZERO
      YINT=ZERO
      ZINT=ZERO
      NPTS=(NI+NJ-2)/2+1
      IMIN=MIN(NPTS)
      IMAX=MAX(NPTS)
      DO 16 I=IMIN,IMAX
      DUM=W(I)
      PX=DUM
      PY=DUM
      PZ=DUM
      DUM=H(I)*T
      PTX=DUM+X0
      PTY=DUM+Y0
      PTZ=DUM+Z0
      AX=PTX-XI
      AY=PTY-YI
      AZ=PTZ-ZI
      BX=PTX-XJ
      BY=PTY-YJ
      BZ=PTZ-ZJ
      GO TO (7,6,5,4,3,2,1),NI
    1 PX=PX*AX
      PY=PY*AY
      PZ=PZ*AZ
    2 PX=PX*AX
      PY=PY*AY
      PZ=PZ*AZ
    3 PX=PX*AX
      PY=PY*AY
      PZ=PZ*AZ
    4 PX=PX*AX
      PY=PY*AY
      PZ=PZ*AZ
    5 PX=PX*AX
      PY=PY*AY
      PZ=PZ*AZ
    6 PX=PX*AX
      PY=PY*AY
      PZ=PZ*AZ
    7 GO TO (15,14,13,12,11,10,9,8),NJ
    8 PX=PX*BX
      PY=PY*BY
      PZ=PZ*BZ
    9 PX=PX*BX
      PY=PY*BY
      PZ=PZ*BZ
   10 PX=PX*BX
      PY=PY*BY
      PZ=PZ*BZ
   11 PX=PX*BX
      PY=PY*BY
      PZ=PZ*BZ
   12 PX=PX*BX
      PY=PY*BY
      PZ=PZ*BZ
   13 PX=PX*BX
      PY=PY*BY
      PZ=PZ*BZ
   14 PX=PX*BX
      PY=PY*BY
      PZ=PZ*BZ
   15 CONTINUE
      XINT=XINT+PX
      YINT=YINT+PY
      ZINT=ZINT+PZ
   16 CONTINUE
      RETURN
      END
C*MODULE PCMDER  *DECK DERIVA
      SUBROUTINE DERIVA(NSJ,NSJR,ICOORD,INTSPH,VERT,CENTR,NEWSPH,
     *                  NUMTS,NUMSPH,DERPUNT,DERTES,DERCENTR,DERRAD,
     *                  NTE,NATM,NESFT)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION INTSPH(NUMTS,10),VERT(NUMTS,10,3),CENTR(NUMTS,10,3),
     *          NEWSPH(NUMSPH,2),
     *          DERPUNT(NTE,NATM,3,3),DERTES(NTE,NATM,3),
     *          DERCENTR(NESFT,NATM,3,3),DERRAD(NESFT,NATM,3)
      DIMENSION DISTKI(3),DERP(3),DERPT(3)
      INTEGER ALGE(63),CASCA(10)
C
      PARAMETER (MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
C   hui li
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
C   hui li
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
C
      DATA ZERO/0.0D+00/
C
C     Calcola le derivate dell'area e della posizione del punto
C     rappresentativo di ciascuna tessera rispetto alla coordinata
C     ICOORD (1=X,2=Y,3=Z) della sfera NSJ.
C
C     Le derivate contengono termini dovuti direttamente allo
C     spostamento del centro della sfera NSJ, e termini "mediati" dagli
C     spostamenti del centro e dal cambiamento del raggio delle sfere
C     "aggiunte" (create da PEDRA, oltre a quelle originarie).
C     Chiamiamo ITS la tessera, I la sfera a cui ITS appartiene, J la
C     sfera che si muove. Le derivate che cerchiamo sono dS(ITS)/dx(J) e
C     dQ(ITS)/dx(J) (indicando con x(J) una qualsiasi coordinata di J.
C     Per prima cosa calcoliamo le derivate dell'area e del punto
C     rappresentativo delle tessere DIRETTAMENTE tagliate da J, poi di
C     quelle che appartengono a J e sono tagliate da altre sfere.
C     Poi consideriamo le "sfere aggiunte" che vengono modificate dal
C     movimento di J: per ogni sfera aggiunta si esaminano tutte le
C     tessere e caso per caso si calcolano le derivate.
C
C     In ogni caso le derivate totali dS/dx(J) e dQ/dx(J) si scrivono
C     come una combinazione delle seguenti derivate parziali:
C          dS/dR(I)  ;  dS/dz(I)  ;  dS/dR(K)  ;  dS/dz(K)
C          dQ/dR(I)  ;  dQ/dz(I)  ;  dQ/dR(K)  ;  dQ/dz(K)
C     dove, ancora una volta, z indica una qualunque coordinata e K
C     un'altra sfera (la stessa J o una sfera aggiunta modificata dal
C     movimento di J) che taglia S.
C
C     Memorizza in DERRAD(NS,NSJ,ICOORD) la derivata del raggio di
C     NS e in DERCENTR(NS,NSJ,ICOORD,3) le derivate delle
C     coordinate del centro di NS rispetto alla coord. ICOORD della
C     sfera NSJ.
C
C     Se NS e' una sfera originaria queste derivate sono 0, tranne
C     DERCENTR(NSJ,NSJ,ICOORD,ICOORD)=1:
C
      DO NSFE = 1, NESFP
         DERRAD(NSFE,NSJ,ICOORD) = ZERO
         DO JJ = 1, 3
            DERCENTR(NSFE,NSJ,ICOORD,JJ) = ZERO
         ENDDO
      ENDDO
      DERCENTR(NSJR,NSJ,ICOORD,ICOORD) = 1.0D+00
C
C     1) Effetti diretti.
C     Loop sulle tessere.
C
      DO 100 ITS= 1, NTS
      ITSCPY = ITS
      DERTS = ZERO
      DO JJ = 1, 3
         DERPT(JJ) = ZERO
      ENDDO
      NV = NVERT(ITS)
      NSI = ISPHE(ITS)
      IF(NSI.EQ.NSJR) GO TO 200
C
C     Derivate nel caso I non = J
C
C               dS/dx(J), dQ/dx(J)
C     ITS ha un lato su J ?
C
      ICONT = 0
      DO N = 1, NV
         IF(INTSPH(ITS,N).EQ.NSJR) ICONT = ICONT + 1
      ENDDO
      IF(ICONT.GE.1) THEN
         CALL DSDX(ITSCPY,ICOORD,NSJR,DERS,DERP,INTSPH,VERT,CENTR,NUMTS)
         DERTS = DERTS + DERS
         DO JJ = 1, 3
            DERPT(JJ) = DERPT(JJ) + DERP(JJ)
         ENDDO
      END IF
      GO TO 150
C
C     Derivate nel caso I = J
C
C     Loop sulle sfere K che intersecano I
C
 200  CONTINUE
      DO 300 NSK = 1, NESF
      IF(NSK.EQ.NSI) GO TO 300
C
C     ITS ha un lato su K ?
C
      ICONT = 0
      DO N = 1, NV
        IF(INTSPH(ITS,N).EQ.NSK) ICONT = ICONT + 1
      ENDDO
      IF(ICONT.EQ.0) GO TO 300
      NSKCPY = NSK
      CALL DSDX(ITSCPY,ICOORD,NSKCPY,DERS,DERP,INTSPH,VERT,CENTR,NUMTS)
      DERTS = DERTS - DERS
        DO JJ = 1, 3
          DERPT(JJ) = DERPT(JJ) - DERP(JJ)
        ENDDO
 300  CONTINUE
 150  DERTES(ITS,NSJ,ICOORD) = DERTS
      DO JJ = 1, 3
        DERPUNT(ITS,NSJ,ICOORD,JJ) = DERPT(JJ)
      ENDDO
 100  CONTINUE
C
C     2) Effetti indiretti.
C     Loop sulle sfere aggiunte
C
      DO 500 NSA = NESFP+1, NESF
      NSACPY = NSA
      DO II = 1, 63
        ALGE(II) = 0
      ENDDO
C
C     Costruiamo l'"albero genealogico" della sfera NSA
C
      ALGE(1) = NSA
      ALGE(2) = ABS(NEWSPH(NSA,1))
      ALGE(3) = ABS(NEWSPH(NSA,2))
      LIVEL = 3
      NUMBER = 2
 510  NSUB = 1
      DO II = LIVEL-NUMBER+1, LIVEL
        IF(ALGE(II).GT.NESFP) THEN
          ALGE(LIVEL+NSUB)   = ABS(NEWSPH(ALGE(II),1))
          ALGE(LIVEL+NSUB+1) = ABS(NEWSPH(ALGE(II),2))
        END IF
        NSUB = NSUB + 2
      ENDDO
      NUMBER = NUMBER * 2
      LIVEL = LIVEL + NUMBER
      IF(NUMBER.LT.32) GO TO 510
C
C     Si accerta che nell'ultimo livello ci siano solo sfere originarie
C
      DO II = 34, 63
        IF(ALGE(II).GT.NESFP) THEN
          IF(MASWRK)WRITE(IW,10) NSA, ALGE(II)
 10       FORMAT(/,'SPHERE ',I3,/,
     *    'AT LEVEL 5 SPHERE',I3,' IS ADDED')
        END IF
      ENDDO
C
C     Quando un elemento di ALGE e' = NSJR, costruisce la corrispondente
C     "cascata" di sfere aggiunte che collega NSJR a NSA
C
      DO 600 LIVEL = 2, 6
      MIN = 2**(LIVEL-1)
      MAX = (2**LIVEL) - 1
      DO 700 II = MIN, MAX
      IF(ALGE(II).NE.NSJR) GO TO 700
      DO K = 1, 10
        CASCA(K) = 0
      ENDDO
      CASCA(1) = NSJR
      INDEX = II
      K = 2
      DO LL = LIVEL, 2, -1
        FACT = (INDEX - 2**(LL-1)) / 2.0D+00
        INDEX = INT(2**(LL-2) + FACT)
        CASCA(K) = ALGE(INDEX)
        K = K + 1
      ENDDO
C     Contiamo gli elementi diversi da 0 in CASCA
      ICONT = 0
      DO K = 1, 10
        IF(CASCA(K).NE.0) ICONT = ICONT + 1
      ENDDO
C
C     Costruiamo le derivate composte del raggio e delle coordinate di
C     NSA (ultimo elemento di CASCA)
C     rispetto alla coordinata ICOORD di NSJR (primo elemento di CASCA)
C
      NS1 = CASCA(1)
      NS2 = CASCA(2)
      CALL DRDC(NS2,ICOORD,NS1,DR,NEWSPH,NUMSPH)
      CALL DCDC(1,NS2,ICOORD,NS1,DX,NEWSPH,NUMSPH)
      CALL DCDC(2,NS2,ICOORD,NS1,DY,NEWSPH,NUMSPH)
      CALL DCDC(3,NS2,ICOORD,NS1,DZ,NEWSPH,NUMSPH)
      DO 800 I = 3, ICONT
      DDR = ZERO
      DDX = ZERO
      DDY = ZERO
      DDZ = ZERO
      NS1 = CASCA(I-1)
      NS2 = CASCA(I)
C
C     Derivata del raggio dell'elemento I di CASCA rispetto
C     alla coord. ICOORD dell'elemento 1 di CASCA
C
      CALL DRDR(NS2,NS1,DER,NEWSPH,NUMSPH)
      DDR = DER * DR
      CALL DRDC(NS2,1,NS1,DER,NEWSPH,NUMSPH)
      DDR = DDR + DER * DX
      CALL DRDC(NS2,2,NS1,DER,NEWSPH,NUMSPH)
      DDR = DDR + DER * DY
      CALL DRDC(NS2,3,NS1,DER,NEWSPH,NUMSPH)
      DDR = DDR + DER * DZ
C
C     Derivata della coord. X dell'elemento I di CASCA rispetto
C     alla coord. ICOORD dell'elemento 1 di CASCA
C
      CALL DCDR(1,NS2,NS1,DER,NEWSPH,NUMSPH)
      DDX = DER * DR
      CALL DCDC(1,NS2,1,NS1,DER,NEWSPH,NUMSPH)
      DDX = DDX + DER * DX
      CALL DCDC(1,NS2,2,NS1,DER,NEWSPH,NUMSPH)
      DDX = DDX + DER * DY
      CALL DCDC(1,NS2,3,NS1,DER,NEWSPH,NUMSPH)
      DDX = DDX + DER * DZ
C
C     Derivata della coord. Y dell'elemento I di CASCA rispetto
C     alla coord. ICOORD dell'elemento 1 di CASCA
C
      CALL DCDR(2,NS2,NS1,DER,NEWSPH,NUMSPH)
      DDY = DER * DR
      CALL DCDC(2,NS2,1,NS1,DER,NEWSPH,NUMSPH)
      DDY = DDY + DER * DX
      CALL DCDC(2,NS2,2,NS1,DER,NEWSPH,NUMSPH)
      DDY = DDY + DER * DY
      CALL DCDC(2,NS2,3,NS1,DER,NEWSPH,NUMSPH)
      DDY = DDY + DER * DZ
C
C     Derivata della coord. Z dell'elemento I di CASCA rispetto
C     alla coord. ICOORD dell'elemento 1 di CASCA
C
      CALL DCDR(3,NS2,NS1,DER,NEWSPH,NUMSPH)
      DDZ = DER * DR
      CALL DCDC(3,NS2,1,NS1,DER,NEWSPH,NUMSPH)
      DDZ = DDZ + DER * DX
      CALL DCDC(3,NS2,2,NS1,DER,NEWSPH,NUMSPH)
      DDZ = DDZ + DER * DY
      CALL DCDC(3,NS2,3,NS1,DER,NEWSPH,NUMSPH)
      DDZ = DDZ + DER * DZ
C
      DR = DDR
      DX = DDX
      DY = DDY
      DZ = DDZ
 800  CONTINUE
C
C     Se NS e' una sfera aggiunta, memorizza le derivate del raggio
C     e delle coordinate del centro:
C
      DERRAD(NSA,NSJ,ICOORD) = DR
      DERCENTR(NSA,NSJ,ICOORD,1) = DX
      DERCENTR(NSA,NSJ,ICOORD,2) = DY
      DERCENTR(NSA,NSJ,ICOORD,3) = DZ
C
C     Per ogni sfera aggiunta connessa a NSJ, fa passare tutte le
C     tessere, calcolando, se e' il caso, le derivate
C
      DO 900 ITS = 1, NTS
      ITSCPY=ITS
      DERTS = ZERO
      DO JJ = 1, 3
        DERPT(JJ) = ZERO
      ENDDO
      NV = NVERT(ITS)
      NSI = ISPHE(ITS)
C
C     Derivate delle tessere appartenenti a NSA
C
      IF(NSI.EQ.NSA) THEN
C
C       Derivate relative al raggio di NSI: 1) scaling
C
        DERTS = 2.0D+00 * AS(ITS) * DR / RE(NSI)
        DERPT(1) = (XCTS(ITS) - XE(NSI)) * DR / RE(NSI)
        DERPT(2) = (YCTS(ITS) - YE(NSI)) * DR / RE(NSI)
        DERPT(3) = (ZCTS(ITS) - ZE(NSI)) * DR / RE(NSI)
C
C       Loop sulle altre sfere K che tagliano ITS
C
        DO 950 NSK = 1, NESF
          NSKCPY = NSK
          IF(NSK.EQ.NSI) GO TO 950
          ICONT = 0
          DO N = 1, NV
            IF(INTSPH(ITS,N).EQ.NSK) ICONT = ICONT + 1
          ENDDO
          IF(ICONT.EQ.0) GO TO 950
C
C      Derivate relative al raggio di NSI: 2) raggio delle altre sfere
C
          CALL DSDR(ITSCPY,NSKCPY,DERS,DERP,INTSPH,VERT,CENTR,NUMTS)
          DERTS = DERTS - DERS * RE(NSK) * DR / RE(NSI)
          DO JJ = 1, 3
            DERPT(JJ) = DERPT(JJ) - DERP(JJ) * RE(NSK) * DR / RE(NSI)
          ENDDO
C
C      Derivate relative al raggio di NSI: 3) coord. delle altre sfere
C
          DISTKI(1) = XE(NSK) - XE(NSI)
          DISTKI(2) = YE(NSK) - YE(NSI)
          DISTKI(3) = ZE(NSK) - ZE(NSI)
          FAC = DR / RE(NSI)
          DO JJ = 1, 3
             JJCPY = JJ
            CALL DSDX(ITSCPY,JJCPY,NSKCPY,DERS,DERP,INTSPH,
     *                VERT,CENTR,NUMTS)
                   IF(DISTKI(JJ).NE.ZERO) THEN
              DERTS = DERTS - FAC * DERS * DISTKI(JJ)
              DO LL = 1, 3
                DERPT(LL) = DERPT(LL) - FAC * DERP(LL) * DISTKI(JJ)
              ENDDO
            END IF
          ENDDO
C
C      Derivate relative alle coordinate di NSI
C
          CALL DSDX(ITSCPY,1,NSKCPY,DERS,DERP,INTSPH,VERT,CENTR,NUMTS)
          DERTS = DERTS - DX * DERS
          DO JJ = 1, 3
            DERPT(JJ) = DERPT(JJ) - DX * DERP(JJ)
          ENDDO
          CALL DSDX(ITSCPY,2,NSKCPY,DERS,DERP,INTSPH,VERT,CENTR,NUMTS)
          DERTS = DERTS - DY * DERS
          DO JJ = 1, 3
            DERPT(JJ) = DERPT(JJ) - DY * DERP(JJ)
          ENDDO
          CALL DSDX(ITSCPY,3,NSKCPY,DERS,DERP,INTSPH,VERT,CENTR,NUMTS)
          DERTS = DERTS - DZ * DERS
          DO JJ = 1, 3
            DERPT(JJ) = DERPT(JJ) - DZ * DERP(JJ)
          ENDDO
 950    CONTINUE
        DERTES(ITS,NSJ,ICOORD) = DERTES(ITS,NSJ,ICOORD) + DERTS
        DO JJ = 1, 3
          DERPUNT(ITS,NSJ,ICOORD,JJ) = DERPUNT(ITS,NSJ,ICOORD,JJ) +
     *    DERPT(JJ)
        ENDDO
      ELSE
C
C       Derivate delle tessere tagliate da NSA
C
        ICONT = 0
        DO N = 1, NV
          IF(INTSPH(ITS,N).EQ.NSA) ICONT = ICONT + 1
        ENDDO
C
Controllo
C
      IF(ICONT.GT.1 .AND. MASWRK) WRITE(IW,*)'5) ICONT ',ICONT
      IF(ICONT.GT.1) ICONT = 0
        IF(ICONT.EQ.0) GO TO 900
C
C       Derivate relative al raggio di NSA
C
        CALL DSDR(ITSCPY,NSACPY,DERS,DERP,INTSPH,VERT,CENTR,NUMTS)
        DERTS = DERS * DR
        DO JJ = 1, 3
          DERPT(JJ) = DERP(JJ) * DR
        ENDDO
C
C       Derivate relative alle coordinate di NSA
C
        CALL DSDX(ITSCPY,1,NSACPY,DERS,DERP,INTSPH,VERT,CENTR,NUMTS)
        DERTS = DERTS + DX * DERS
        DO JJ = 1, 3
          DERPT(JJ) = DERPT(JJ) + DX * DERP(JJ)
        ENDDO
        CALL DSDX(ITSCPY,2,NSACPY,DERS,DERP,INTSPH,VERT,CENTR,NUMTS)
        DERTS = DERTS + DY * DERS
        DO JJ = 1, 3
          DERPT(JJ) = DERPT(JJ) + DY * DERP(JJ)
        ENDDO
        CALL DSDX(ITSCPY,3,NSACPY,DERS,DERP,INTSPH,VERT,CENTR,NUMTS)
        DERTS = DERTS + DZ * DERS
        DO JJ = 1, 3
          DERPT(JJ) = DERPT(JJ) + DZ * DERP(JJ)
        ENDDO
C
        DERTES(ITS,NSJ,ICOORD) = DERTES(ITS,NSJ,ICOORD) + DERTS
        DO JJ = 1, 3
          DERPUNT(ITS,NSJ,ICOORD,JJ) = DERPUNT(ITS,NSJ,ICOORD,JJ) +
     *    DERPT(JJ)
        ENDDO
      END IF
 900  CONTINUE
 700  CONTINUE
 600  CONTINUE
 500  CONTINUE
C
C     Controlla che nessuna derivata sia eccessiva (per imprecisioni
C     numeriche)
C
      DO ITS = 1, NTS
        IF(ABS(DERTES(ITS,NSJ,ICOORD)) .GT. 10.0D+00 ) THEN
          IF(MASWRK)WRITE(IW,*)
     *       'GEOMETRICAL DERIVATIVE EXCESSIVE: NEGLECTED'
          DERTES(ITS,NSJ,ICOORD) = 0.0D+00
        END IF
      ENDDO
C
C     Calcola la derivata della superficie totale
C
      DER = ZERO
      DO ITS = 1, NTS
         DER = DER + DERTES(ITS,NSJ,ICOORD)
      ENDDO
      RETURN
      END
C*MODULE PCMDER  *DECK DSDX
      SUBROUTINE DSDX(ITS,ICOORD,NSJ,DERS,DERP,INTSPH,VERT,CENTR,NUMTS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION DERP(3),INTSPH(NUMTS,10),VERT(NUMTS,10,3),
     *          CENTR(NUMTS,10,3)
      DIMENSION DP(10,3),SUMDER(3),SUMVERT(3),PT(3)
C
      PARAMETER (MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
C
      DATA ZERO/0.0D+00/
C
C     Trova le derivate dell'area e del punto rappresentativo della
C     tessera ITS rispetto alla coordinata ICOORD(1=X, 2=Y, 3=Z)
C     della sfera NSJ.
C     ITS appartiene alla sfera NSI (diversa da NSJ, e intersecata
C     da NSJ).
C
      DERTS = ZERO
      NSI = ISPHE(ITS)
      NV = NVERT(ITS)
      DO L = 1, NV
        DP(L,1) = ZERO
        DP(L,2) = ZERO
        DP(L,3) = ZERO
      ENDDO
      DO 100 L = 1, NV
      IF(INTSPH(ITS,L).NE.NSJ) GO TO 100
C     L1 e' il lato di ITS che sta sulla sfera NSJ
      L1 = L
      L0 = L - 1
      IF(L1.EQ.1) L0 = NV
      L2 = L1 + 1
      IF(L1.EQ.NV) L2 = 1
      L3 = L2 + 1
      IF(L2.EQ.NV) L3 = 1
C
C     Trova la derivata del vertice L1: DP(L1)
C
      CALL DERVER(ICOORD,ITS,L0,L1,L2,DP(L1,1),DP(L1,2),DP(L1,3),
     *            INTSPH,VERT,CENTR,NUMTS)
C
C     Trova la derivata del vertice L2: DP(L2)
C
      CALL DERVER(ICOORD,ITS,L1,-L2,L3,DP(L2,1),DP(L2,2),DP(L2,3),
     *            INTSPH,VERT,CENTR,NUMTS)
C
C     1) Derivata dell'area della tessera.
C     Calcola i contributi dovuti ai lati L0-L1, L1-L2, L2-L3
C
      IF(INTSPH(ITS,L0).NE.NSI) THEN
        CALL DERPHI(ICOORD,NSJ,ITS,L0,L1,DP,DA,INTSPH,VERT,CENTR,NUMTS)
        DERTS = DERTS + DA
      END IF
      CALL DERPHI(ICOORD,NSJ,ITS,L1,L2,DP,DA,INTSPH,VERT,CENTR,NUMTS)
      DERTS = DERTS + DA
      IF(INTSPH(ITS,L2).NE.ISPHE(ITS)) THEN
        CALL DERPHI(ICOORD,NSJ,ITS,L2,L3,DP,DA,INTSPH,VERT,CENTR,NUMTS)
        DERTS = DERTS + DA
      END IF
C
C     Calcola i contributi dovuti ai vertici L1 e L2
C
      CALL DERBETA(ICOORD,NSJ,ITS,L0,L1,L2,DP,DA,INTSPH,VERT,CENTR,
     *             NUMTS)
      DERTS = DERTS - DA
      CALL DERBETA(ICOORD,NSJ,ITS,L1,L2,L3,DP,DA,INTSPH,VERT,CENTR,
     *             NUMTS)
      DERTS = DERTS - DA
 100  CONTINUE
      DERS = DERTS
C
C     2) Derivata del punto rappresentativo.
C     Trova il punto rappresentativo riferito al centro della sfera
C
      PT(1) = XCTS(ITS) - XE(NSI)
      PT(2) = YCTS(ITS) - YE(NSI)
      PT(3) = ZCTS(ITS) - ZE(NSI)
C
C     Trova il modulo della somma dei vertici della tessera
C
      DO JJ = 1, 3
        SUMVERT(JJ) = ZERO
      ENDDO
      DO L = 1, NV
        SUMVERT(1) = SUMVERT(1) + (VERT(ITS,L,1) - XE(NSI))
        SUMVERT(2) = SUMVERT(2) + (VERT(ITS,L,2) - YE(NSI))
        SUMVERT(3) = SUMVERT(3) + (VERT(ITS,L,3) - ZE(NSI))
      ENDDO
      SUM = ZERO
      DO JJ = 1, 3
        SUM = SUM + SUMVERT(JJ) * SUMVERT(JJ)
      ENDDO
      SUM = SQRT(SUM)
C
C     Trova la somma delle derivate dei vertici
C
      DO JJ = 1, 3
        SUMDER(JJ) = DP(L1,JJ) + DP(L2,JJ)
      ENDDO
C
      PROD = ZERO
      DO JJ = 1, 3
        PROD = PROD + PT(JJ) * SUMDER(JJ)
      ENDDO
      DO JJ = 1, 3
        DERP(JJ) = ( SUMDER(JJ) * RE(NSI) / SUM ) -
     *             ( PT(JJ) * PROD / (RE(NSI)*SUM) )
      ENDDO
      RETURN
      END
C*MODULE PCMDER  *DECK DSDR
      SUBROUTINE DSDR(ITS,NSJ,DERS,DERP,INTSPH,VERT,CENTR,NUMTS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION DERP(3),INTSPH(NUMTS,10),VERT(NUMTS,10,3),
     *          CENTR(NUMTS,10,3)
      DIMENSION DP(10,3),SUMDER(3),SUMVERT(3),PT(3)
C
      PARAMETER (MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
C
      DATA ZERO/0.0D+00/
C
C     Trova le derivate dell'area e del punto rappresentativo di ITS
C     rispetto al raggio della sfera NSJ.
C     ITS appartiene alla sfera NSI (diversa da NSJ, e intersecata
C     da NSJ).
C
      DERTS = ZERO
      NSI = ISPHE(ITS)
      NV = NVERT(ITS)
      DO L = 1, NV
        DP(L,1) = ZERO
        DP(L,2) = ZERO
        DP(L,3) = ZERO
      ENDDO
      DO 100 L = 1, NV
      IF(INTSPH(ITS,L).NE.NSJ) GO TO 100
C
C     L1 e' il lato di ITS che sta sulla sfera NSJ
C
      L1 = L
      L0 = L - 1
      IF(L1.EQ.1) L0 = NV
      L2 = L1 + 1
      IF(L1.EQ.NV) L2 = 1
      L3 = L2 + 1
      IF(L2.EQ.NV) L3 = 1
C
C     Trova la derivata del vertice L1: DP(L1)
C
      CALL DERVER1(ITS,L0,L1,L2,DP(L1,1),DP(L1,2),DP(L1,3),
     *             INTSPH,VERT,CENTR,NUMTS)
C
C     Trova la derivata del vertice L2: DP(L2)
C
      CALL DERVER1(ITS,L1,-L2,L3,DP(L2,1),DP(L2,2),DP(L2,3),
     *             INTSPH,VERT,CENTR,NUMTS)
C
C     1) Derivata dell'area della tessera.
C     Calcola i contributi dovuti ai lati L0-L1, L1-L2, L2-L3
C
      IF(INTSPH(ITS,L0).NE.NSI) THEN
        CALL DERPHI1(NSJ,ITS,L0,L1,DP,DA,INTSPH,VERT,CENTR,NUMTS)
        DERTS = DERTS + DA
      END IF
      CALL DERPHI1(NSJ,ITS,L1,L2,DP,DA,INTSPH,VERT,CENTR,NUMTS)
      DERTS = DERTS + DA
      IF(INTSPH(ITS,L2).NE.ISPHE(ITS)) THEN
        CALL DERPHI1(NSJ,ITS,L2,L3,DP,DA,INTSPH,VERT,CENTR,NUMTS)
        DERTS = DERTS + DA
      END IF
C
C     Calcola i contributi dovuti ai vertici L1 e L2
C
      CALL DERBETA1(NSJ,ITS,L0,L1,L2,DP,DA,INTSPH,VERT,CENTR,NUMTS)
      DERTS = DERTS - DA
      CALL DERBETA1(NSJ,ITS,L1,L2,L3,DP,DA,INTSPH,VERT,CENTR,NUMTS)
      DERTS = DERTS - DA
 100  CONTINUE
      DERS = DERTS
C
C     2) Derivata del punto rappresentativo.
C     Trova il punto rappresentativo riferito al centro della sfera
C
      PT(1) = XCTS(ITS) - XE(NSI)
      PT(2) = YCTS(ITS) - YE(NSI)
      PT(3) = ZCTS(ITS) - ZE(NSI)
C
C     Trova il modulo della somma dei vertici della tessera
C
      DO JJ = 1, 3
        SUMVERT(JJ) = ZERO
      ENDDO
      DO L = 1, NV
        SUMVERT(1) = SUMVERT(1) + (VERT(ITS,L,1) - XE(NSI))
        SUMVERT(2) = SUMVERT(2) + (VERT(ITS,L,2) - YE(NSI))
        SUMVERT(3) = SUMVERT(3) + (VERT(ITS,L,3) - ZE(NSI))
      ENDDO
      SUM = ZERO
      DO JJ = 1, 3
        SUM = SUM + SUMVERT(JJ) * SUMVERT(JJ)
      ENDDO
      SUM = SQRT(SUM)
C
C     Trova la somma delle derivate dei vertici
C
      DO JJ = 1, 3
        SUMDER(JJ) = DP(L1,JJ) + DP(L2,JJ)
      ENDDO
C
      PROD = ZERO
      DO JJ = 1, 3
        PROD = PROD + PT(JJ) * SUMDER(JJ)
      ENDDO
      DO JJ = 1, 3
        DERP(JJ) = ( SUMDER(JJ) * RE(NSI) / SUM ) -
     *             ( PT(JJ) * PROD / (RE(NSI)*SUM) )
      ENDDO
      RETURN
      END
C*MODULE PCMDER  *DECK DRDR
      SUBROUTINE DRDR(NSI,NSJ,DR,NEWSPH,NUMSPH)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION NEWSPH(NUMSPH,2)
C
      PARAMETER (MXTS=2500, MXSP=250)
C
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMDAT/ EPS,EPSINF,DELTAR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
C
C     Trova la derivata del raggio della sfera NSI rispetto al raggio
C     della sfera NSJ.
C
C     La sfera NSI (che appartiene alle sfere "aggiunte" da PEDRA)
C     dipende dalle due sfere "precedenti" NSJ e NSK
C     Se NSJ o NSK sono negativi, la sfera aggiunta e' di tipo C
C     e la generatrice "principale" corrisponde al label negativo
C     (cfr. JCC 11, 1047 (1990))
C
      IF(NEWSPH(NSI,1).LT.0 .OR. NEWSPH(NSI,2).LT.0) GO TO 100
      NSK = NEWSPH(NSI,1)
      IF(NSK.EQ.NSJ) NSK = NEWSPH(NSI,2)
      RS = RSOLV
      RJ = RE(NSJ) + RS
      RK = RE(NSK) + RS
      RI = RE(NSI) + RS
      D2 = (XE(NSJ)-XE(NSK))**2 + (YE(NSJ)-YE(NSK))**2 +
     *   (ZE(NSJ)-ZE(NSK))**2
      D = SQRT(D2)
      DR = (-3.0D+00*RJ*RJ + RK*RK + 2.0D+00*RJ*RK
     *      + 3.0D+00*D*RJ - D*RK) / (4.0D+00*D*RI)
      GO TO 200
C
 100  CONTINUE
      NSK = NEWSPH(NSI,1)
      IF(ABS(NSK).EQ.NSJ) NSK = NEWSPH(NSI,2)
C
      IF(NSK.GT.0) THEN
        RS = RSOLV
        RJ = RE(NSJ) + RS
        RK = RE(NSK) + RS
        RI = RE(NSI) + RS
        D2 = (XE(NSJ)-XE(NSK))**2 + (YE(NSJ)-YE(NSK))**2 +
     *     (ZE(NSJ)-ZE(NSK))**2
        D = SQRT(D2)
        DR = ( 2.0D+00*D*RJ + 2.0D+00*D*RE(NSJ) - 2.0D+00*RJ*RE(NSJ) +
     *       D*D - RJ*RJ - RK*RK ) / (2.0D+00*D*RI)
      ELSE
        RS = RSOLV
        RJ = RE(NSJ) + RS
        RI = RE(NSI) + RS
        D2 = (XE(NSJ)-XE(ABS(NSK)))**2 + (YE(NSJ)-YE(ABS(NSK)))**2 +
     *     (ZE(NSJ)-ZE(ABS(NSK)))**2
        D = SQRT(D2)
        DR = ( RE(ABS(NSK)) * RJ ) / ( D*RI)
      END IF
 200  CONTINUE
      RETURN
      END
C*MODULE PCMDER  *DECK DRDC
      SUBROUTINE DRDC(NSI,ICOORD,NSJ,DR,NEWSPH,NUMSPH)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION NEWSPH(NUMSPH,2)
      DIMENSION COORDJ(3),COORDK(3)
C
      PARAMETER (MXTS=2500, MXSP=250)
C
      COMMON /PCMDAT/ EPS,EPSINF,DELTAR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
C
C     Trova la derivata del raggio della sfera NSI rispetto alla
C     coordinata ICOORD (1=X, 2=Y, 3=Z) della sfera NSJ, che interseca
C     NSI.
C
C     La sfera NSI (che appartiene alle sfere "aggiunte" da PEDRA)
C     dipende dalle due sfere "precedenti" NSJ e K
C
C     Se NSJ o NSK sono negativi, la sfera aggiunta e' di tipo C
C     e la generatrice "principale" corrisponde al label negativo
C     (cfr. JCC 11, 1047 (1990))
C
      IF(NEWSPH(NSI,1).LT.0.OR.NEWSPH(NSI,2).LT.0) GO TO 100
      K = NEWSPH(NSI,1)
      IF(K.EQ.NSJ) K = NEWSPH(NSI,2)
      COORDJ(1) = XE(NSJ)
      COORDJ(2) = YE(NSJ)
      COORDJ(3) = ZE(NSJ)
      COORDK(1) = XE(K)
      COORDK(2) = YE(K)
      COORDK(3) = ZE(K)
      D2 = (XE(NSJ)-XE(K))**2 + (YE(NSJ)-YE(K))**2 + (ZE(NSJ)-ZE(K))**2
      D = SQRT(D2)
      B = 0.5D+00 * (D + RE(NSJ) - RE(K))
      RS = RSOLV
      A = ((RE(NSJ)+RS)**2 + D2 - (RE(K)+RS)**2) / D
      DR = (2.0D+00*A*B - 2.0D+00*B*D - A*D) *
     *     (COORDJ(ICOORD)-COORDK(ICOORD)) / (4.0D+00*D2*(RE(NSI)+RS))
      GO TO 200
C
 100  CONTINUE
      NSK = NEWSPH(NSI,1)
      IF(ABS(NSK).EQ.NSJ) NSK = NEWSPH(NSI,2)
      COORDJ(1) = XE(NSJ)
      COORDJ(2) = YE(NSJ)
      COORDJ(3) = ZE(NSJ)
      COORDK(1) = XE(ABS(NSK))
      COORDK(2) = YE(ABS(NSK))
      COORDK(3) = ZE(ABS(NSK))
      RI = RE(NSI) + RSOLV
      RJ = RE(NSJ) + RSOLV
      RK = RE(ABS(NSK)) + RSOLV
      DIFF = COORDJ(ICOORD) - COORDK(ICOORD)
      D2 = (COORDJ(1)-COORDK(1))**2 + (COORDJ(2)-COORDK(2))**2 +
     *     (COORDJ(3)-COORDK(3))**2
      D = SQRT(D2)
      FAC = RE(NSJ) * ( RJ*RJ - D*D - RK*RK )
      IF(NSK.LT.0) FAC = RE(ABS(NSK)) * (RK*RK - D*D - RJ*RJ )
      DR = DIFF * FAC / ( 2.0D+00 * D**3 * RI )
C
 200  CONTINUE
      RETURN
      END
C*MODULE PCMDER  *DECK DCDR
      SUBROUTINE DCDR(JJ,NSI,NSJ,DC,NEWSPH,NUMSPH)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION NEWSPH(NUMSPH,2)
      DIMENSION COORDJ(3), COORDK(3)
C
      PARAMETER (MXTS=2500, MXSP=250)
C
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
C
      DATA ZERO/0.0D+00/
C
C     Trova la derivata della coordinata JJ del centro della sfera
C     NSI rispetto al raggio dellla sfera NSJ.
C
C     La sfera NSI (che appartiene alle sfere "aggiunte" da PEDRA)
C     dipende dalle due sfere "precedenti" NSJ e NSK
C
C     Se NSJ o NSK sono negativi, la sfera aggiunta e' di tipo C
C     e la generatrice "principale" corrisponde al label negativo
C     (cfr. JCC 11, 1047 (1990))
C
      IF(NEWSPH(NSI,1).LT.0.OR.NEWSPH(NSI,2).LT.0) GO TO 100
      NSK = NEWSPH(NSI,1)
      IF(NSK.EQ.NSJ) NSK = NEWSPH(NSI,2)
      COORDJ(1) = XE(NSJ)
      COORDJ(2) = YE(NSJ)
      COORDJ(3) = ZE(NSJ)
      COORDK(1) = XE(NSK)
      COORDK(2) = YE(NSK)
      COORDK(3) = ZE(NSK)
      D2 = (XE(NSJ)-XE(NSK))**2 + (YE(NSJ)-YE(NSK))**2 +
     *     (ZE(NSJ)-ZE(NSK))**2
      D = SQRT(D2)
      DC = - (COORDJ(JJ) - COORDK(JJ)) / (2.0D+00 * D)
      GO TO 200
C
 100  CONTINUE
      NSK = NEWSPH(NSI,1)
      IF(ABS(NSK).EQ.NSJ) NSK = NEWSPH(NSI,2)
      DC = ZERO
      IF(NSK.LT.ZERO) GO TO 200
      COORDJ(1) = XE(NSJ)
      COORDJ(2) = YE(NSJ)
      COORDJ(3) = ZE(NSJ)
      COORDK(1) = XE(NSK)
      COORDK(2) = YE(NSK)
      COORDK(3) = ZE(NSK)
      D2 = (XE(NSJ)-XE(NSK))**2 + (YE(NSJ)-YE(NSK))**2 +
     *     (ZE(NSJ)-ZE(NSK))**2
      D = SQRT(D2)
      DC = - ( COORDJ(JJ) - COORDK(JJ) ) / D
C
 200  CONTINUE
      RETURN
      END
C*MODULE PCMDER  *DECK DCDC
      SUBROUTINE DCDC(JJ,NSI,ICOORD,NSJ,DC,NEWSPH,NUMSPH)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION NEWSPH(NUMSPH,2)
      DIMENSION COORDJ(3), COORDK(3)
C
      PARAMETER (MXTS=2500, MXSP=250)
C
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
C
C     Trova la derivata della coordinata JJ del centro della sfera
C     NSI rispetto alla coordinata ICOORD di NSJ, che interseca NSI.
C
C     La sfera NSI (che appartiene alle sfere "aggiunte" da PEDRA)
C     dipende dalle due sfere "precedenti" NSJ e NSK
C
C     Se NSJ o NSK sono negativi, la sfera aggiunta e' di tipo C
C     e la generatrice "principale" corrisponde al label negativo
C     (cfr. JCC 11, 1047 (1990))
C
      IF(NEWSPH(NSI,1).LT.0.OR.NEWSPH(NSI,2).LT.0) GO TO 100
      K = NEWSPH(NSI,1)
      IF(K.EQ.NSJ) K = NEWSPH(NSI,2)
      COORDJ(1) = XE(NSJ)
      COORDJ(2) = YE(NSJ)
      COORDJ(3) = ZE(NSJ)
      COORDK(1) = XE(K)
      COORDK(2) = YE(K)
      COORDK(3) = ZE(K)
      D2 = (XE(NSJ)-XE(K))**2 + (YE(NSJ)-YE(K))**2 + (ZE(NSJ)-ZE(K))**2
      D = SQRT(D2)
      DC = (RE(NSJ)-RE(K)) * (COORDJ(ICOORD)-COORDK(ICOORD)) *
     *        (COORDJ(JJ) - COORDK(JJ)) / (2.0D+00 * D**3)
      IF(JJ.EQ.ICOORD)DC = DC + 0.5D+00 - (RE(NSJ)-RE(K)) / (2.0D+00*D)
      GO TO 200
C
 100  CONTINUE
      NSK = NEWSPH(NSI,1)
      IF(ABS(NSK).EQ.NSJ) NSK = NEWSPH(NSI,2)
      COORDJ(1) = XE(NSJ)
      COORDJ(2) = YE(NSJ)
      COORDJ(3) = ZE(NSJ)
      COORDK(1) = XE(ABS(NSK))
      COORDK(2) = YE(ABS(NSK))
      COORDK(3) = ZE(ABS(NSK))
      D2 = (COORDJ(1)-COORDK(1))**2 + (COORDJ(2)-COORDK(2))**2 +
     *     (COORDJ(3)-COORDK(3))**2
      D = SQRT(D2)
      IF(NSK.GT.0) THEN
        DC = RE(NSJ) * (COORDJ(JJ)-COORDK(JJ)) * (COORDJ(ICOORD)-
     *  COORDK(ICOORD)) / D**3
        IF(ICOORD.EQ.JJ) DC = DC + 1.0D+00 - RE(NSJ) / D
      ELSE
        DC = - RE(ABS(NSK)) * (COORDK(JJ)-COORDJ(JJ)) * (COORDK(ICOORD)-
     *  COORDJ(ICOORD)) / D**3
        IF(ICOORD.EQ.JJ) DC = DC + RE(ABS(NSK)) / D
      END IF
C
 200  CONTINUE
      RETURN
      END
C*MODULE PCMDER  *DECK DERVER
      SUBROUTINE DERVER(IC,ITS,L0,L,L2,DX,DY,DZ,INTSPH,VERT,CENTR,NUMTS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION INTSPH(NUMTS,10),VERT(NUMTS,10,3),CENTR(NUMTS,10,3)
      DIMENSION P(3),P1(3),P2(3),P3(3)
C
      PARAMETER (MXTS=2500, MXSP=250)
C
C   hui li
      LOGICAL GOPARR, DSKWRK, MASWRK
C
C   hui li
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
C
      DATA ZERO/0.0D+00/
C
C     Trova la derivata della posizione del vertice L della tessera ITS
C     rispetto alla coordinata IC della sfera che, intersecando
C     ISPHE(ITS), forma il lato in esame.
C     Se stiamo considerando il primo vertice del lato : L > 0,
C     il lato che si muove e' L-L2, e il vettore derivata e' diretto
C     lungo la tangente al lato precedente (L0-L).
C     Se stiamo considerando il secondo vertice del lato : L < 0,
C     il lato che si muove e' L0-L, e il vettore derivata e' tangente
C     al lato seguente (L-L2).
C
      IF(L.GT.0) THEN
        L1 = L
        NSJ = INTSPH(ITS,L1)
      ELSE
        L1 = - L
        NSJ = INTSPH(ITS,L0)
      END IF
C
C     Il vettore P indica la posizione del vertice rispetto al centro
C     della sfera NSJ
      P(1) = VERT(ITS,L1,1) - XE(NSJ)
      P(2) = VERT(ITS,L1,2) - YE(NSJ)
      P(3) = VERT(ITS,L1,3) - ZE(NSJ)
C
C     Trova il versore tangente al lato L0-L1 se stiamo considerando il
C     primo vertice, L2-L1 se consideriamo il secondo
      IF(L.GT.0) THEN
        DO JJ = 1, 3
          P1(JJ) = VERT(ITS,L1,JJ) - CENTR(ITS,L0,JJ)
          P2(JJ) = VERT(ITS,L0,JJ) - CENTR(ITS,L0,JJ)
        ENDDO
      ELSE
        DO JJ = 1, 3
          P1(JJ) = VERT(ITS,L1,JJ) - CENTR(ITS,L1,JJ)
          P2(JJ) = VERT(ITS,L2,JJ) - CENTR(ITS,L1,JJ)
        ENDDO
      END IF
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P3(JJ) = P3(JJ) / DNORM3
      ENDDO
C     Trova la derivata
      PROD = P(1)*P3(1) + P(2)*P3(2) + P(3)*P3(3)
      IF(PROD.EQ.ZERO.AND.P(IC).NE.ZERO) THEN
         IF(MASWRK)WRITE(6,*) 'DEATH IN ROUTINE DERVER'
         CALL ABRT
         STOP
      END IF
      IF(PROD.EQ.ZERO) PROD = 1.0D+00
      FACT = P(IC) / PROD
      DX = FACT * P3(1)
      DY = FACT * P3(2)
      DZ = FACT * P3(3)
      RETURN
      END
C*MODULE PCMDER  *DECK DERVER1
      SUBROUTINE DERVER1(ITS,L0,L,L2,DX,DY,DZ,INTSPH,VERT,CENTR,NUMTS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION INTSPH(NUMTS,10),VERT(NUMTS,10,3),CENTR(NUMTS,10,3)
      DIMENSION P(3),P1(3),P2(3),P3(3)
C
      PARAMETER (MXTS=2500, MXSP=250)
C
C   hui li
      LOGICAL GOPARR, DSKWRK, MASWRK
C
C   hui li
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
C
      DATA ZERO/0.0D+00/
C
C     Trova la derivata della posizione del vertice L della tessera ITS
C     rispetto al raggio della sfera aggiunta NSJ che, intersecando
C     la tessera ITS, crea il lato considerato.
C     Se stiamo considerando il primo vertice del lato : L > 0,
C     il lato che si muove e' L-L2, e il vettore derivata e' diretto
C     lungo la tangente al lato precedente (L0-L).
C     Se stiamo considerando il secondo vertice del lato : L < 0,
C     il lato che si muove e' L0-L, e il vettore derivata e' tangente
C     al lato seguente (L-L2).
C
      IF(L.GT.0) THEN
        L1 = L
        NSJ = INTSPH(ITS,L1)
      ELSE
        L1 = - L
        NSJ = INTSPH(ITS,L0)
      END IF
C
C     Il vettore P indica la posizione del vertice rispetto al centro
C     della sfera NSJ
      P(1) = VERT(ITS,L1,1) - XE(NSJ)
      P(2) = VERT(ITS,L1,2) - YE(NSJ)
      P(3) = VERT(ITS,L1,3) - ZE(NSJ)
C
C     Trova il versore tangente al lato L0-L1 se stiamo considerando il
C     primo vertice, L2-L1 se consideriamo il secondo
      IF(L.GT.0) THEN
        DO JJ = 1, 3
          P1(JJ) = VERT(ITS,L1,JJ) - CENTR(ITS,L0,JJ)
          P2(JJ) = VERT(ITS,L0,JJ) - CENTR(ITS,L0,JJ)
        ENDDO
      ELSE
        DO JJ = 1, 3
          P1(JJ) = VERT(ITS,L1,JJ) - CENTR(ITS,L1,JJ)
          P2(JJ) = VERT(ITS,L2,JJ) - CENTR(ITS,L1,JJ)
        ENDDO
      END IF
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P3(JJ) = P3(JJ) / DNORM3
      ENDDO
C     Trova la derivata
      PROD = P(1)*P3(1) + P(2)*P3(2) + P(3)*P3(3)
      IF(PROD.EQ.ZERO) THEN
         IF(MASWRK)WRITE(6,*) 'DEATH IN ROUTINE DERVER1'
         CALL ABRT
         STOP
      END IF
      FACT = RE(NSJ) / PROD
      DX = FACT * P3(1)
      DY = FACT * P3(2)
      DZ = FACT * P3(3)
      RETURN
      END
C*MODULE PCMDER  *DECK DERPHI
      SUBROUTINE DERPHI(IC,NSJ,ITS,L1,L2,DP,DA,INTSPH,VERT,CENTR,NUMTS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION DP(10,3),INTSPH(NUMTS,10),VERT(NUMTS,10,3),
     *          CENTR(NUMTS,10,3)
      DIMENSION V1(3),V2(3),VEC1(3),VEC2(3),VEC3(3),VEC4(3)
C
      PARAMETER (MXTS=2500, MXSP=250)
C
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
C
C     Trova la derivata della quantita':
C                Phi(L1) Cos[Theta(L1)]
C     riferita al lato L1 della tessera ITS, conoscendo le derivate
C     della posizione dei vertici che lo delimitano: DP(L1) e DP(L2).
C
C     NS1 e' la sfera su cui si trova la tessera, NS2 quella che
C     intersecandola crea il lato L1.
C
      NS1 = ISPHE(ITS)
      NS2 = INTSPH(ITS,L1)
C
C     Trova le coordinate dei vertici rispetto al centro del cerchio
C     su cui si trova l'arco L1, e il raggio di tale cerchio.
C
      DO JJ = 1, 3
        V1(JJ) = VERT(ITS,L1,JJ) - CENTR(ITS,L1,JJ)
        V2(JJ) = VERT(ITS,L2,JJ) - CENTR(ITS,L1,JJ)
      ENDDO
      RC2 = V1(1)**2 + V1(2)**2 + V1(3)**2
      PROD = V1(1)*V2(1) + V1(2)*V2(2) + V1(3)*V2(3)
      COSPHI = PROD / RC2
C
C     Puo' accadere che la tessera considerata sia intersecata
C     molto vicino al vertice: in questo caso le approssimazioni
C     numeriche possono far si' che PROD>RC2, e quindi
C     COSPHI>1. Rimediamo:
C
      IF(COSPHI.GE.1.0D+00) COSPHI = 0.999999999999D+00
      SENPHI = SQRT(1 - COSPHI*COSPHI)
C     Calcola la derivata di Phi(L1)
      DO JJ = 1, 3
      VEC1(JJ) = V1(JJ) - COSPHI*V2(JJ)
      VEC2(JJ) = DP(L2,JJ)
      VEC3(JJ) = V2(JJ) - COSPHI*V1(JJ)
      VEC4(JJ) = DP(L1,JJ)
      ENDDO
C
C     Se il lato in questione e' proprio quello creato dalla sfera
C     che si muove, alcune componenti vengono corrette (per effetto
C     della derivata della distanza tra i centri delle sfere)
      IF(NS2.EQ.NSJ) THEN
        DIST2 = (XE(NS1)-XE(NSJ))**2 + (YE(NS1)-YE(NSJ))**2
     *        + (ZE(NS1)-ZE(NSJ))**2
        FACT = (RE(NS1)**2 - RE(NSJ)**2 + DIST2) /(2.0D+00 * DIST2)
        VEC2(IC) = VEC2(IC) - FACT
        VEC4(IC) = VEC4(IC) - FACT
      END IF
C
      DPHI = 0.0D+00
      DO JJ = 1, 3
        DPHI = DPHI - (VEC1(JJ)*VEC2(JJ)+VEC3(JJ)*VEC4(JJ))
      ENDDO
      DPHI = DPHI / (RC2*SENPHI)
C
C     Calcola il coseno dell'angolo polare
      DNORM1 = 0.0D+00
      DNORM2 = 0.0D+00
      V1(1) = VERT(ITS,L1,1) - XE(NS1)
      V1(2) = VERT(ITS,L1,2) - YE(NS1)
      V1(3) = VERT(ITS,L1,3) - ZE(NS1)
      V2(1) = XE(NS2) - XE(NS1)
      V2(2) = YE(NS2) - YE(NS1)
      V2(3) = ZE(NS2) - ZE(NS1)
      DO JJ = 1, 3
        DNORM1 = DNORM1 + V1(JJ)*V1(JJ)
        DNORM2 = DNORM2 + V2(JJ)*V2(JJ)
      ENDDO
      DNORM1 = SQRT(DNORM1)
      DNORM2 = SQRT(DNORM2)
      COSTH=(V1(1)*V2(1)+V1(2)*V2(2)+V1(3)*V2(3)) / (DNORM1*DNORM2)
C
C     Se il lato considerato NON e' quello formato dalla sfera che
C     si muove, la derivata dell'angolo polare e' nulla.
      DCOS = 0.0D+00
      IF(NS2.NE.NSJ) GO TO 100
C     Altrimenti:
      DO JJ = 1, 3
      DCOS = DCOS + V2(JJ) * DP(L1,JJ)
      ENDDO
      DCOS = DCOS + V1(IC) - RE(NS1)*COSTH*V2(IC)/DNORM2
      DCOS = DCOS / ( RE(NS1) * DNORM2 )
 100  CONTINUE
      DA = COSTH * DPHI + ACOS(COSPHI) * DCOS
      DA = RE(NS1) * RE(NS1) * DA
      RETURN
      END
C*MODULE PCMDER  *DECK DERPHI1
      SUBROUTINE DERPHI1(NSJ,ITS,L1,L2,DP,DA,INTSPH,VERT,CENTR,NUMTS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION DP(10,3),INTSPH(NUMTS,10),VERT(NUMTS,10,3),
     *          CENTR(NUMTS,10,3)
      DIMENSION V1(3),V2(3),T12(3),VEC1(3),VEC2(3),VEC3(3),VEC4(3)
C
      PARAMETER (MXTS=2500, MXSP=250)
C
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
C
C     Trova la derivata della quantita':
C                Phi(L1) Cos[Theta(L1)]
C     riferita al lato L1 della tessera ITS, rispetto al raggio della
C     sfera NSJ, conoscendo le derivate
C     della posizione dei vertici che lo delimitano: DP(L1) e DP(L2).
C
C     NS1 e' la sfera su cui si trova la tessera, NS2 quella che
C     intersecandola crea il lato L1.
C
      NS1 = ISPHE(ITS)
      NS2 = INTSPH(ITS,L1)
C
C     Trova le coordinate dei vertici rispetto al centro del cerchio
C     su cui si trova l'arco L1, e il raggio di tale cerchio.
C
      DO JJ = 1, 3
        V1(JJ) = VERT(ITS,L1,JJ) - CENTR(ITS,L1,JJ)
        V2(JJ) = VERT(ITS,L2,JJ) - CENTR(ITS,L1,JJ)
      ENDDO
      RC2 = V1(1)**2 + V1(2)**2 + V1(3)**2
      PROD = V1(1)*V2(1) + V1(2)*V2(2) + V1(3)*V2(3)
      COSPHI = PROD / RC2
C
C     Puo' accadere che la tessera considerata sia intersecata
C     molto vicino al vertice: in questo caso le approssimazioni
C     numeriche possono far si' che PROD>RC2, e quindi
C     COSPHI>1. Rimediamo:
C
      IF(COSPHI.GE.1.0D+00) COSPHI = 0.999999999999D+00
      SENPHI = SQRT(1 - COSPHI*COSPHI)
C     Calcola la derivata di Phi(L1)
      DO JJ = 1, 3
      VEC1(JJ) = V1(JJ) - COSPHI*V2(JJ)
      VEC2(JJ) = DP(L2,JJ)
      VEC3(JJ) = V2(JJ) - COSPHI*V1(JJ)
      VEC4(JJ) = DP(L1,JJ)
      ENDDO
C
C     Se il lato in questione e' proprio quello creato dalla sfera
C     che si muove, alcune componenti vengono corrette (per effetto
C     della derivata del raggio della sfera NSJ)
      IF(NS2.EQ.NSJ) THEN
        T12(1) = XE(NSJ) - XE(NS1)
        T12(2) = YE(NSJ) - YE(NS1)
        T12(3) = ZE(NSJ) - ZE(NS1)
        DIST2 = T12(1)*T12(1) + T12(2)*T12(2) + T12(3)*T12(3)
        DO JJ = 1, 3
          VEC2(JJ) = VEC2(JJ) + RE(NSJ) * T12(JJ) / DIST2
          VEC4(JJ) = VEC4(JJ) + RE(NSJ) * T12(JJ) / DIST2
        ENDDO
      END IF
C
      DPHI = 0.0D+00
      DO JJ = 1, 3
        DPHI = DPHI - (VEC1(JJ)*VEC2(JJ)+VEC3(JJ)*VEC4(JJ))
      ENDDO
      DPHI = DPHI / (RC2*SENPHI)
C
C     Calcola il coseno dell'angolo polare
      DNORM1 = 0.0D+00
      DNORM2 = 0.0D+00
      V1(1) = VERT(ITS,L1,1) - XE(NS1)
      V1(2) = VERT(ITS,L1,2) - YE(NS1)
      V1(3) = VERT(ITS,L1,3) - ZE(NS1)
      V2(1) = XE(NS2) - XE(NS1)
      V2(2) = YE(NS2) - YE(NS1)
      V2(3) = ZE(NS2) - ZE(NS1)
      DO JJ = 1, 3
        DNORM1 = DNORM1 + V1(JJ)*V1(JJ)
        DNORM2 = DNORM2 + V2(JJ)*V2(JJ)
      ENDDO
      DNORM1 = SQRT(DNORM1)
      DNORM2 = SQRT(DNORM2)
      COSTH=(V1(1)*V2(1)+V1(2)*V2(2)+V1(3)*V2(3)) / (DNORM1*DNORM2)
C
C     Se il lato considerato NON e' quello formato dalla sfera che
C     si muove, la derivata dell'angolo polare e' nulla.
      DCOS = 0.0D+00
      IF(NS2.NE.NSJ) GO TO 100
C     Altrimenti:
      DO JJ = 1, 3
      DCOS = DCOS + V2(JJ) * DP(L1,JJ)
      ENDDO
      DCOS = DCOS / ( RE(NS1) * DNORM2 )
 100  CONTINUE
      DA = COSTH * DPHI + ACOS(COSPHI) * DCOS
      DA = RE(NS1) * RE(NS1) * DA
      RETURN
      END
C*MODULE DERPCM  *DECK DERBETA
      SUBROUTINE DERBETA(IC,NSFE,ITS,L0,L1,L2,DP,DA,INTSPH,VERT,CENTR,
     *                   NUMTS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION DP(10,3),INTSPH(NUMTS,10),VERT(NUMTS,10,3),
     *          CENTR(NUMTS,10,3)
      DIMENSION V1(3),V2(3),V3(3),V4(3),DV1(3),DV2(3),DV3(3),DV4(3),
     *          P1(3),P2(3),P3(3),T0(3),T1(3),T12(3),DT0(3),DT1(3)
C
      PARAMETER (MXTS=2500, MXSP=250)
C
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
C
      DATA PI/3.1415927D+00/
C
C     Trova la derivata dell'angolo esterno del vertice L1 della tessera
C     ITS, formato dai lati L0-L1 e L1-L2, rispetto alla coordinata IC
C     della sfera NSFE, conoscendo la derivata della posizione del
C     vertice P1.
C
      NS1 = ISPHE(ITS)
      NS2 = INTSPH(ITS,L0)
      NS3 = INTSPH(ITS,L1)
C
C     Trova i vettori posizione dei vertici L0, L1 rispetto al centro
C     dell'arco L0, e dei vertici L1, L2 rispetto al centro dell'arco L1
C
      DO JJ = 1, 3
        V1(JJ) = VERT(ITS,L0,JJ) - CENTR(ITS,L0,JJ)
        V2(JJ) = VERT(ITS,L1,JJ) - CENTR(ITS,L0,JJ)
        V3(JJ) = VERT(ITS,L1,JJ) - CENTR(ITS,L1,JJ)
        V4(JJ) = VERT(ITS,L2,JJ) - CENTR(ITS,L1,JJ)
      ENDDO
C
C     Trova la derivata dei vettori posizione
C
      DO JJ = 1, 3
        DV1(JJ) = DP(L0,JJ)
        DV2(JJ) = DP(L1,JJ)
        DV3(JJ) = DP(L1,JJ)
        DV4(JJ) = DP(L2,JJ)
      ENDDO
C
C     Corregge la derivata dei vettori posizione se il lato considerato
C     appartiene alla sfera che si muove (distinguendo se ITS appartiene
C     o no alla sfera che si muove)
C
      INDEX = 0
      IF(NS1.EQ.NSFE.AND.NS2.NE.NSFE) INDEX = 1
      IF(NS1.NE.NSFE.AND.NS2.EQ.NSFE) INDEX = 1
      IF(INDEX.EQ.1) THEN
        T12(1) = XE(NS2) - XE(NS1)
        T12(2) = YE(NS2) - YE(NS1)
        T12(3) = ZE(NS2) - ZE(NS1)
        DIST2 = T12(1)*T12(1) + T12(2)*T12(2) + T12(3)*T12(3)
        DO JJ = 1, 3
          DV1(JJ) = DV1(JJ) + (RE(NS1)**2 - RE(NS2)**2) *
     *              T12(IC) * T12(JJ) / (DIST2*DIST2)
          DV2(JJ) = DV2(JJ) + (RE(NS1)**2 - RE(NS2)**2) *
     *              T12(IC) * T12(JJ) / (DIST2*DIST2)
        ENDDO
        FACT = (RE(NS1)**2-RE(NS2)**2+DIST2) / (2.0D+00*DIST2)
        DV1(IC) = DV1(IC) - FACT
        DV2(IC) = DV2(IC) - FACT
      END IF
C
      INDEX = 0
      IF(NS1.EQ.NSFE.AND.NS3.NE.NSFE) INDEX = 1
      IF(NS1.NE.NSFE.AND.NS3.EQ.NSFE) INDEX = 1
      IF(INDEX.EQ.1) THEN
        T12(1) = XE(NS3) - XE(NS1)
        T12(2) = YE(NS3) - YE(NS1)
        T12(3) = ZE(NS3) - ZE(NS1)
        DIST2 = T12(1)*T12(1) + T12(2)*T12(2) + T12(3)*T12(3)
        DO JJ = 1, 3
          DV3(JJ) = DV3(JJ) + (RE(NS1)**2 - RE(NS3)**2) *
     *              T12(IC) * T12(JJ) / (DIST2*DIST2)
          DV4(JJ) = DV4(JJ) + (RE(NS1)**2 - RE(NS3)**2) *
     *              T12(IC) * T12(JJ) / (DIST2*DIST2)
        ENDDO
        FACT = (RE(NS1)**2-RE(NS3)**2+DIST2) / (2.0D+00*DIST2)
        DV3(IC) = DV3(IC) - FACT
        DV4(IC) = DV4(IC) - FACT
      END IF
C
C     Trova il vettore tangente al lato L0-L1: T0 = V2 x (V2 x V1)
C
      DO JJ = 1, 3
        P1(JJ) = V2(JJ)
        P2(JJ) = V1(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        T0(JJ) = P3(JJ)
      ENDDO
      DNORMT0 = DNORM3
C
C     Trova il vettore tangente al lato L1-L2: T1 = V3 x (V3 x V4)
C
      DO JJ = 1, 3
        P1(JJ) = V3(JJ)
        P2(JJ) = V4(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        T1(JJ) = P3(JJ)
      ENDDO
      DNORMT1 = DNORM3
C
C     Trova l'angolo Beta(L1)
C
      PROD = T0(1)*T1(1) + T0(2)*T1(2) + T0(3)*T1(3)
      BETA = PI - ACOS( PROD / (DNORMT0*DNORMT1) )
      COSBETA = COS(BETA)
      SENBETA = SIN(BETA)
C
C     Trova la derivata della tangente T0 :
C     DT0 = DV2 x (V2 x V1) + V2 x (DV2 x V1) + V2 x (V2 x DV1)
C
      DO JJ = 1, 3
        DT0(JJ) = 0.0D+00
      ENDDO
C
      DO JJ = 1, 3
        P1(JJ) = V2(JJ)
        P2(JJ) = V1(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P1(JJ) = DV2(JJ)
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        DT0(JJ) = DT0(JJ) + P3(JJ)
      ENDDO
C
      DO JJ = 1, 3
        P1(JJ) = DV2(JJ)
        P2(JJ) = V1(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P1(JJ) = V2(JJ)
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        DT0(JJ) = DT0(JJ) + P3(JJ)
      ENDDO
C
      DO JJ = 1, 3
        P1(JJ) = V2(JJ)
        P2(JJ) = DV1(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P1(JJ) = V2(JJ)
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        DT0(JJ) = DT0(JJ) + P3(JJ)
      ENDDO
C
C     Trova la derivata della tangente T1 :
C     DT1 = DV3 x (V3 x V4) + V3 x (DV3 x V4) + V3 x (V3 x DV4)
C
      DO JJ = 1, 3
        DT1(JJ) = 0.0D+00
      ENDDO
C
      DO JJ = 1, 3
        P1(JJ) = V3(JJ)
        P2(JJ) = V4(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P1(JJ) = DV3(JJ)
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        DT1(JJ) = DT1(JJ) + P3(JJ)
      ENDDO
C
      DO JJ = 1, 3
        P1(JJ) = DV3(JJ)
        P2(JJ) = V4(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P1(JJ) = V3(JJ)
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        DT1(JJ) = DT1(JJ) + P3(JJ)
      ENDDO
C
      DO JJ = 1, 3
        P1(JJ) = V3(JJ)
        P2(JJ) = DV4(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P1(JJ) = V3(JJ)
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        DT1(JJ) = DT1(JJ) + P3(JJ)
      ENDDO
C
C     Infine calcola la derivata dell'angolo Beta(L1)
C
      DO JJ = 1, 3
        P1(JJ) = T1(JJ) + COSBETA * DNORMT1 * T0(JJ) / DNORMT0
        P2(JJ) = T0(JJ) + COSBETA * DNORMT0 * T1(JJ) / DNORMT1
      ENDDO
      DO JJ = 1, 3
        DBETA = 0.0D+00
      ENDDO
      DO JJ = 1, 3
        DBETA = DBETA + ( P1(JJ) * DT0(JJ) + P2(JJ) * DT1(JJ) )
      ENDDO
      DBETA = DBETA / ( SENBETA * DNORMT0 * DNORMT1 )
      DA = RE(NS1) * RE(NS1) * DBETA
      RETURN
      END
C*MODULE PCMDER  *DECK DERBETA1
      SUBROUTINE DERBETA1(NSFE,ITS,L0,L1,L2,DP,DA,INTSPH,VERT,CENTR,
     *                    NUMTS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION DP(10,3),INTSPH(NUMTS,10),VERT(NUMTS,10,3),
     *          CENTR(NUMTS,10,3)
      DIMENSION V1(3),V2(3),V3(3),V4(3),DV1(3),DV2(3),DV3(3),DV4(3),
     *          P1(3),P2(3),P3(3),T0(3),T1(3),T12(3),DT0(3),DT1(3)
C
      PARAMETER (MXTS=2500, MXSP=250)
C
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
C
      DATA PI/3.1415927D+00/
C
C     Trova la derivata dell'angolo esterno del vertice L1 della tessera
C     ITS, formato dai lati L0-L1 e L1-L2, rispetto al raggio della
C     sfera NSFE, conoscendo la derivata della posizione del vertice P1.
C
      NS1 = ISPHE(ITS)
      NS2 = INTSPH(ITS,L0)
      NS3 = INTSPH(ITS,L1)
C
C     Trova i vettori posizione dei vertici L0, L1 rispetto al centro
C     dell'arco L0, e dei vertici L1, L2 rispetto al centro dell'arco L1
      DO JJ = 1, 3
        V1(JJ) = VERT(ITS,L0,JJ) - CENTR(ITS,L0,JJ)
        V2(JJ) = VERT(ITS,L1,JJ) - CENTR(ITS,L0,JJ)
        V3(JJ) = VERT(ITS,L1,JJ) - CENTR(ITS,L1,JJ)
        V4(JJ) = VERT(ITS,L2,JJ) - CENTR(ITS,L1,JJ)
      ENDDO
C     Trova la derivata dei vettori posizione
      DO JJ = 1, 3
        DV1(JJ) = DP(L0,JJ)
        DV2(JJ) = DP(L1,JJ)
        DV3(JJ) = DP(L1,JJ)
        DV4(JJ) = DP(L2,JJ)
      ENDDO
C
C     Corregge la derivata dei vettori posizione se il lato considerato
C     appartiene alla sfera che si muove (distinguendo se ITS appartiene
C     o no alla sfera che si muove)
C
      INDEX = 0
      IF(NS1.EQ.NSFE.AND.NS2.NE.NSFE) INDEX = 1
      IF(NS1.NE.NSFE.AND.NS2.EQ.NSFE) INDEX = 1
      IF(INDEX.EQ.1) THEN
        T12(1) = XE(NS2) - XE(NS1)
        T12(2) = YE(NS2) - YE(NS1)
        T12(3) = ZE(NS2) - ZE(NS1)
        DIST2 = T12(1)*T12(1) + T12(2)*T12(2) + T12(3)*T12(3)
        DO JJ = 1, 3
          DV1(JJ) = DV1(JJ) + RE(NS2) * T12(JJ) / DIST2
          DV2(JJ) = DV2(JJ) + RE(NS2) * T12(JJ) / DIST2
        ENDDO
      END IF
C
      INDEX = 0
      IF(NS1.EQ.NSFE.AND.NS3.NE.NSFE) INDEX = 1
      IF(NS1.NE.NSFE.AND.NS3.EQ.NSFE) INDEX = 1
      IF(INDEX.EQ.1) THEN
        T12(1) = XE(NS3) - XE(NS1)
        T12(2) = YE(NS3) - YE(NS1)
        T12(3) = ZE(NS3) - ZE(NS1)
        DIST2 = T12(1)*T12(1) + T12(2)*T12(2) + T12(3)*T12(3)
        DO JJ = 1, 3
          DV3(JJ) = DV3(JJ) + RE(NS3) * T12(JJ) / DIST2
          DV4(JJ) = DV4(JJ) + RE(NS3) * T12(JJ) / DIST2
        ENDDO
      END IF
C
C     Trova il vettore tangente al lato L0-L1: T0 = V2 x (V2 x V1)
      DO JJ = 1, 3
        P1(JJ) = V2(JJ)
        P2(JJ) = V1(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        T0(JJ) = P3(JJ)
      ENDDO
      DNORMT0 = DNORM3
C     Trova il vettore tangente al lato L1-L2: T1 = V3 x (V3 x V4)
      DO JJ = 1, 3
        P1(JJ) = V3(JJ)
        P2(JJ) = V4(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        T1(JJ) = P3(JJ)
      ENDDO
      DNORMT1 = DNORM3
C
C     Trova l'angolo Beta(L1)
      PROD = T0(1)*T1(1) + T0(2)*T1(2) + T0(3)*T1(3)
      BETA = PI - ACOS( PROD / (DNORMT0*DNORMT1) )
      COSBETA = COS(BETA)
      SENBETA = SIN(BETA)
C
C     Trova la derivata della tangente T0 :
C     DT0 = DV2 x (V2 x V1) + V2 x (DV2 x V1) + V2 x (V2 x DV1)
C
      DO JJ = 1, 3
        DT0(JJ) = 0.0D+00
      ENDDO
C
      DO JJ = 1, 3
        P1(JJ) = V2(JJ)
        P2(JJ) = V1(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P1(JJ) = DV2(JJ)
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        DT0(JJ) = DT0(JJ) + P3(JJ)
      ENDDO
C
      DO JJ = 1, 3
        P1(JJ) = DV2(JJ)
        P2(JJ) = V1(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P1(JJ) = V2(JJ)
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        DT0(JJ) = DT0(JJ) + P3(JJ)
      ENDDO
C
      DO JJ = 1, 3
        P1(JJ) = V2(JJ)
        P2(JJ) = DV1(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P1(JJ) = V2(JJ)
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        DT0(JJ) = DT0(JJ) + P3(JJ)
      ENDDO
C
C     Trova la derivata della tangente T1 :
C     DT1 = DV3 x (V3 x V4) + V3 x (DV3 x V4) + V3 x (V3 x DV4)
C
      DO JJ = 1, 3
        DT1(JJ) = 0.0D+00
      ENDDO
C
      DO JJ = 1, 3
        P1(JJ) = V3(JJ)
        P2(JJ) = V4(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P1(JJ) = DV3(JJ)
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        DT1(JJ) = DT1(JJ) + P3(JJ)
      ENDDO
C
      DO JJ = 1, 3
        P1(JJ) = DV3(JJ)
        P2(JJ) = V4(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P1(JJ) = V3(JJ)
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        DT1(JJ) = DT1(JJ) + P3(JJ)
      ENDDO
C
      DO JJ = 1, 3
        P1(JJ) = V3(JJ)
        P2(JJ) = DV4(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        P1(JJ) = V3(JJ)
        P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
        DT1(JJ) = DT1(JJ) + P3(JJ)
      ENDDO
C
C     Infine calcola la derivata dell'angolo Beta(L1)
      DO JJ = 1, 3
        P1(JJ) = T1(JJ) + COSBETA * DNORMT1 * T0(JJ) / DNORMT0
        P2(JJ) = T0(JJ) + COSBETA * DNORMT0 * T1(JJ) / DNORMT1
      ENDDO
      DO JJ = 1, 3
        DBETA = 0.0D+00
      ENDDO
      DO JJ = 1, 3
        DBETA = DBETA + ( P1(JJ) * DT0(JJ) + P2(JJ) * DT1(JJ) )
      ENDDO
      DBETA = DBETA / ( SENBETA * DNORMT0 * DNORMT1 )
      DA = RE(NS1) * RE(NS1) * DBETA
      RETURN
      END
