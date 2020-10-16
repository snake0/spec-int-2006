C  9 dec 03 - OQ  - ADD NPARAM ARGUMENT TO F2NONS
C  8 Oct 01 - HL  - parallelize PCM
C 11 Oct 00 - PB  - Added arguments to COMMON BLOCK PCMPAR and PCMCHG
C  9 Feb 99 - BM  - introduction of ief model
C 28 Sep 97 - BM  - wd3xmat: compute dqse, fplcvg: change conv. tests
C 18 MAR 97 - PISA - NEW MODULE FOR PCM POLARIZABILITIES
C
C*MODULE PCMPOL  *DECK WDXMAT
      SUBROUTINE WDXMAT(D,XX,EL,SCR,DMATM1,BEMCHG,SE,DE,
     *                  Q,DQSE,DMFR,DQQ,DQFR,DQ,
     *                  ICOOR,ITW,L1,NUM2,NTS,NTS2,NFT27)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS)
C
      DIMENSION D(L1,L1),XX(L1,L1),EL(L1,L1),SCR(NUM2),
     *          DMATM1(NTS,NTS),BEMCHG(NUM2),DMFR(NTS,NTS),
     *          SE(NTS2,NTS2),DE(NTS2,NTS2),
     *          Q(NTS),DQSE(NTS),DQQ(NTS),DQFR(NTS,3),DQ(NTS,3)
C
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMPAR/ IPCM,NFT26,NFTB,IRPPCM,IEF,IP_F
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTESS
      COMMON /PCMTHF/ DFEFR(3),D2FEFR(3,3,3),DFE(3),D2FE(3,3)
C
      DATA ZERO, ONE, TWO  /0.0D+00, 1.0D+00, 2.0D+00/
C
C     Routine per il calcolo della matrice XX (interazione tra
C     pseudo-cariche virtuali elettroniche e il potenziale elettronico
C     del soluto) usando la DERIVATA della matrice densita' rispetto
C     alla componente ICOOR del campo elettrico esterno (dipendente
C     dalla frequenza W):
C                 XX(W)mn = sum_i[ V(m,n;i) d[QSE_i(W)]/dEx ]
C         d[QSE_i(W)]/dEx = sum_m,n[ dD(W,m,n)/dEx qse_i(W)mn ]
C     dove Ex e' la componente x del campo elettrico esterno.
C     Il fattore di normalizzazione deve essere anch'esso derivato:
C         d[Q(W)_norm]/dEx = dQSE(W)/dEx * FE(0)  +  QSE(0) * dFE(W)/dEx
C               dFE(W)/dEx = - FE(0)**2 * dQ(W)/dEx / Qteorica
C     dove QSE sono le cariche di polarizzazione NON normalizzate.
C
C     Viene usata per il calcolo delle polarizzabilita' e iperpolariz-
C     zabilita' A FREQUENZA W in soluzione.
C
C     La matrice dD/dEx e' passata in D
C
C      d[QSE_i(0)]/dEx = a(i)sum_j[ DMATM1(i,j)
C           {sum_m,n[ dD(m,n)/Ex(-dV(m,n;i)/dn) ]}
C      d[QSE_i(W)]/dEx = a(i)sum_j[ dmfr(i,j)
C           {sum_m,n[ dD(m,n)dEx(-dV(m,n;i)/dn) ]}
C
      IF (ICOOR.GT.3) THEN
         IF(MASWRK)WRITE(IW,*)
     *      'ICOOR IS EXPECTED TO BE IN THE RANGE 1 TO 3'
         CALL ABRT
      END IF
C
C     Legge le cariche elementari (mu,nu) su ogni tessera, e forma la
C     matrice completa
C     ITW=0 caso statico: [qse_i(0)]mn
C     ITW=1 caso dinamico (W): [qse_i(W)]mn
C     ITW>1 caso dinamico (2W,3W): non viene memorizzato
C
      IF(IEF.EQ.0.OR.IEF.GE.3)THEN
      CALL SEQREW(NFT27)
C
      DO ITS = 1, NTS
C
        CALL SQREAD(NFT27,BEMCHG,NUM2)
        IF(IEF.EQ.0)THEN
         IPS = 0
         DO I = 1, L1
            DO J = 1, I
               IPS = IPS + 1
               EL(I,J) = BEMCHG(IPS)
               EL(J,I) = BEMCHG(IPS)
            ENDDO
         ENDDO
        ELSE
C
C  IEF => 3
C
         IPS = 0
         DO I = 1, L1
            DO J = 1, I
              IPS = IPS + 1
              EL(I,J) = -BEMCHG(IPS)
              EL(J,I) = -BEMCHG(IPS)
            ENDDO
         ENDDO
C
        ENDIF
C
         Q(ITS)=ZERO
         DO I = 1, L1
            DO JJ = 1, L1
               Q(ITS) = Q(ITS) + D(I,JJ)*EL(I,JJ)
            ENDDO
         ENDDO
      ENDDO
C
      ELSE
C
C  0 < IEF < 3
C
      DO ITS = 1, NTS
         XP=XCTS(ITS)
         YP=YCTS(ITS)
         ZP=ZCTS(ITS)
         CALL INTMEP(SCR,XP,YP,ZP)
         CALL SQREAD(NFT27,BEMCHG,NUM2)
C
         IPS = 0
         DO I = 1, L1
            DO J = 1, I
              IPS = IPS + 1
              EL(I,J) = SCR(IPS)
              EL(J,I) = SCR(IPS)
              XX(I,J) = BEMCHG(IPS)
              XX(J,I) = BEMCHG(IPS)
            ENDDO
         ENDDO
C
         DQQ(ITS)=ZERO
         DQSE(ITS)=ZERO
         DO II = 1, L1
            DO JJ = 1, L1
              DQQ(ITS) = DQQ(ITS) + D(II,JJ)*EL(II,JJ)
              DQSE(ITS) = DQSE(ITS) + D(II,JJ)*XX(II,JJ)
            ENDDO
         ENDDO
      ENDDO
C
      DO I=1,NTS
       VECGP =ZERO
       DO J=1,NTS
          SEIJ=SE(I,J)
          IF(ITW.GE.1) SEIJ=SE(I,J)*EPS/EPSINF
          DELTAIJ=ZERO
          IF(I.EQ.J)DELTAIJ=AS(I)
          VECGP=VECGP-(DELTAIJ/TWO-DE(I,J))*DQQ(J)
     *               -SEIJ*DQSE(J)
       ENDDO
       Q(I) = VECGP
      ENDDO
C
      ENDIF
C
      CALL SEQREW(NFT27)
C
      DQETOT = ZERO
      DO 20 ITS = 1, NTS
       IF(ITW.EQ.0) THEN
        DQ(ITS,ICOOR) = ZERO
       ELSE
        IF(ITW.EQ.1) DQFR(ITS,ICOOR) = ZERO
        DQQ(ITS) = ZERO
       END IF
C
       DO 10 ITSJ = 1, NTS
        IF(ITW.EQ.0) THEN
         DQ(ITS,ICOOR) = DQ(ITS,ICOOR)+
     *            DMATM1(ITS,ITSJ)*Q(ITSJ)
        ELSEIF(ITW.EQ.1) THEN
           DQFR(ITS,ICOOR) = DQFR(ITS,ICOOR)+
     *                DMFR(ITS,ITSJ)*Q(ITSJ)
        ELSE
           DQQ(ITS) = DQQ(ITS)+DMFR(ITS,ITSJ)*Q(ITSJ)
        END IF
  10   CONTINUE
C
        IF(ITW.EQ.0) THEN
          DQ(ITS,ICOOR) = DQ(ITS,ICOOR)*AS(ITS)
          DQSE(ITS) = DQ(ITS,ICOOR)
         DQETOT = DQETOT + DQ(ITS,ICOOR)
        ELSEIF(ITW.EQ.1) THEN
            DQFR(ITS,ICOOR) = DQFR(ITS,ICOOR)*AS(ITS)
            DQSE(ITS) = DQFR(ITS,ICOOR)
            DQETOT = DQETOT + DQFR(ITS,ICOOR)
        ELSE
            DQQ(ITS) = DQQ(ITS)*AS(ITS)
            DQSE(ITS) = DQQ(ITS)
            DQETOT = DQETOT + DQQ(ITS)
        END IF
  20   CONTINUE
C
      IF(ICOMP.NE.0) THEN
      NELEC = NE
      QETEOR = NELEC * ( EPS - ONE ) / EPS
      DF = - FE * FE * DQETOT / QETEOR
       IF(ITW.EQ.0)DFE(ICOOR) = DF
       IF(ITW.EQ.1)DFEFR(ICOOR) = DF
C
       QETOTN = ZERO
       DO ITS = 1, NTS
        IF(ITW.EQ.0) THEN
         DQSE(ITS) = DQ(ITS,ICOOR)*FE + DFE(ICOOR)*QSE(ITS)/FE
        ELSEIF(ITW.EQ.1) THEN
           DQSE(ITS) = DQFR(ITS,ICOOR)*FE + DFEFR(ICOOR)*QSE(ITS)/FE
        ELSE
           DQSE(ITS) = DQQ(ITS)*FE + DF*QSE(ITS)/FE
        END IF
        QETOTN = QETOTN + DQSE(ITS)
        ENDDO
C     Stampa i valori della carica prima e dopo la normalizzazione
C       if(maswrk)WRITE(IW,1000) DQETOT , QETOTN
C  1000 FORMAT('SOMMA DERIVATE PRIMA ',F10.6,' E DOPO LA ',
C      * 'NORMALIZZAZIONE ',F10.6)
      END IF
C
      IF(IEF.GE.3)CALL SEQREW(NFT27)
      NSQR=L1*L1
      IF(IEF.NE.0)CALL VCLR(XX,1,NSQR)
C
C     Calcola X(dD/dEx)
      DO ITS = 1, NTS
       IF(IEF.LT.3)THEN
        CALL INTMEP(SCR,XCTS(ITS),YCTS(ITS),ZCTS(ITS))
       ELSE
        CALL SQREAD(NFT27,SCR,NUM2)
       ENDIF
       IPS = 0
       DO I = 1, L1
         DO J = 1, I
           IPS = IPS + 1
           EL(I,J) = - SCR(IPS)
           EL(J,I) = - SCR(IPS)
          ENDDO
        ENDDO
C
C       Calcola gli elementi della matrice X
       DO IBAS = 1, L1
         DO JBAS = 1, L1
           XX(IBAS,JBAS) = XX(IBAS,JBAS) + EL(IBAS,JBAS) * DQSE(ITS)
         ENDDO
        ENDDO
C
      ENDDO
      IF(IEF.GE.3)CALL SEQREW(NFT27)
      RETURN
      END
C*MODULE PCMPOL  *DECK NEWD
      SUBROUTINE NEWD(DMFR,IPVT,WORK,DMATM1,NTS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION DMFR(NTS,NTS),IPVT(NTS),WORK(NTS),DMATM1(NTS,NTS)
      DIMENSION DET(2)
C
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
C
      DATA ONE/1.0D+00/
      DATA FPI/12.56637061D+00/
C
      IF(MASWRK)THEN
      WRITE(IW,*)'---------------------------'
      WRITE(IW,*)'-- MATRIX D^(-1)[EPS(W)] --'
      WRITE(IW,*)'---------------------------'
      END IF
C
      DO 180 I=1,NTS
      DO 180 J=1,NTS
        DMFR(I,J)=DMATM1(I,J)
  180 CONTINUE
C
      INFO=0
      CALL DGEFA(DMFR,NTS,NTS,IPVT,INFO)
      IF(INFO.NE.0) THEN
         IF(MASWRK)WRITE(IW,*) 'D MATRIX IS SINGULAR'
         CALL ABRT
      END IF
C
      CALL DGEDI(DMFR,NTS,NTS,IPVT,DET,WORK,01)
C
      DO 140 I=1,NTS
       DMFR(I,I)=DMFR(I,I)+
     *          FPI*(EPS/(EPS-ONE)-EPSINF/(EPSINF-ONE))
  140 CONTINUE
C
      INFO=0
      CALL DGEFA(DMFR,NTS,NTS,IPVT,INFO)
      IF(INFO.NE.0) THEN
         IF(MASWRK)WRITE(IW,*) 'D MATRIX IS SINGULAR'
         CALL ABRT
      END IF
C
      CALL DGEDI(DMFR,NTS,NTS,IPVT,DET,WORK,01)
C
      IF(MASWRK)WRITE(IW,*)
     *       '..... DONE GENERATING NEW D INVERSE MATRIX .....'
      CALL TIMIT(1)
      END
C*MODULE PCMPOL  *DECK WD2XMAT
      SUBROUTINE WD2XMAT(D,XX,EL,SCR,DMATM1,BEMCHG,SE,DE,Q,DQSE,
     *                   CQEF,DMFR,DQFR,DQ,D2QFR1,D2QFR2,D2QFR3,D2Q,
     *                   IC1,IC2,ITW,L1,NUM2,NTS,NTS2,NFT27)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS)
C
      DIMENSION D(L1,L1),XX(L1,L1),EL(L1,L1),SCR(NUM2),DMFR(NTS,NTS),
     *          DMATM1(NTS,NTS),BEMCHG(NUM2),Q(NTS),DQSE(NTS),
     *          CQEF(NTS),SE(NTS2,NTS2),DE(NTS2,NTS2),
     *          DQFR(NTS,3),DQ(NTS,3),D2QFR1(NTS,3,3),D2QFR2(NTS,3,3),
     *          D2QFR3(NTS,3,3),D2Q(NTS,3,3)
C
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTESS
      COMMON /PCMTHF/ DFEFR(3),D2FEFR(3,3,3),DFE(3),D2FE(3,3)
      COMMON /PCMPAR/ IPCM,NFT26,NFTB,IRPPCM,IEF,IP_F
C
      DATA ZERO, ONE, TWO  /0.0D+00, 1.0D+00, 2.0D+00/
C
C     Routine per il calcolo della matrice XX (interazione tra
C     pseudo-cariche virtuali elettroniche e il potenziale elettronico
C     del soluto) usando la DERIVATA  SECONDA della matrice densita'
C     rispetto alle componenti IC1 e IC2 del campo elettrico esterno
C     (con frequenza Wt=W1+W2):
C         XX(Wt)mn = sum_i[ V(m,n;i) d2[QSE_i(W1,W2)]/dExdEy ]
C         d2[QSE_i(W1,W2)]/dExdEy =
C            sum_m,n{ d2[D(W1,W2)mn]/dExdEy qse_i(Wt)mn }
C     dove Ex,Ey sono le componenti x,y del campo elettrico esterno.
C
C     Il fattore di normalizzazione deve essere anch'esso derivato.
C     d2[Q(W1,W2)_norm]/dExdEy = d2QSE(W1,W2)/dExdEy * FE(0)
C                              + QSE(0) * d2FE(W1,W2)/dExdEy
C                              + [dFE(W1)/dEx]*[dQSE(W2)/dEy]
C                              + [dFE(W2)/dEy]*[dQSE(W1)/dEx]
C           d2FE(W1,W2)/dExdEy = - {FE(0)**2 *
C                  sum(i) [d2QSE_i(W1,W2)/dExdEy]} / Qteorica +
C                          2 * [dFE(W1)/dEx * dFE(W2)/dEy ] / FE(0)
C     dove QSE sono le cariche di polarizzazione NON normalizzate.
C
C     Viene usata per il calcolo delle polarizzabilita' e
C     iperpolarizzabilita' A FREQUENZA W in soluzione.
C
C     La matrice  d2D(W1,W2)/dExdEy e' passata in D.
C
C      d[QSE_i(0)]/dEx = a(i)sum_j[ DMATM1(i,j)
C           {sum_m,n[ dD(m,n)/Ex(-dV(m,n;i)/dn) ]}
C      d[QSE_i(W)]/dEx = a(i)sum_j[ DMFR(i,j)
C           {sum_m,n[ dD(m,n)dEx(-dV(m,n;i)/dn) ]}
C
      IF (IC1.GT.3.OR.IC2.GT.3) THEN
         IF(MASWRK)WRITE(IW,*) 'IC1 AND IC2 ARE EXPECTED TO BE',
     *               ' IN THE RANGE 1 TO 3'
         CALL ABRT
      END IF
C
C     Legge le cariche elementari (mu,nu) su ogni tessera, e forma la
C     matrice completa
C      ITW=0   Static                       W1=0,  W2=0 ;  Wt=0
C      ITW=1   Second harmonic generation   W1=+W, W2=+W;  Wt=2W
C      ITW=2   Electrooptic Pockels effect  W1=0,  W2=+W;  Wt=W
C      ITW=3   Optical rectification        W1=+W, W2=-W;  Wt=0
C
      CALL SEQREW(NFT27)
C
      IF(IEF.EQ.0.OR.IEF.GE.3)THEN
C
C  ISOTROPIC SOLVENTS
C           PCM (IEF=0)
C           IEF (IEF>=3)
C
      DO ITS = 1, NTS
         CALL SQREAD(NFT27,BEMCHG,NUM2)
C
        IF(IEF.EQ.0)THEN
         IPS = 0
         DO I = 1, L1
            DO J = 1, I
               IPS = IPS + 1
               EL(I,J) = BEMCHG(IPS)
               EL(J,I) = BEMCHG(IPS)
            ENDDO
         ENDDO
C
        ELSE
C
         IPS = 0
         DO I = 1, L1
            DO J = 1, I
               IPS = IPS + 1
               EL(I,J) = -BEMCHG(IPS)
               EL(J,I) = -BEMCHG(IPS)
            ENDDO
         ENDDO
C
        ENDIF
C
         Q(ITS)=ZERO
         DO I = 1, L1
            DO JJ = 1, L1
               Q(ITS) = Q(ITS) + D(I,JJ)*EL(I,JJ)
            ENDDO
         ENDDO
      ENDDO
C
      ELSE
C
C   ANISOTROPIC SOLVENTS (IEF=1)
C   IONIC SOLUTIONS (IEF=2)
C
      DO ITS = 1, NTS
         XP=XCTS(ITS)
         YP=YCTS(ITS)
         ZP=ZCTS(ITS)
         CALL INTMEP(SCR,XP,YP,ZP)
         CALL SQREAD(NFT27,BEMCHG,NUM2)
C
         IPS = 0
         DO I = 1, L1
            DO J = 1, I
               IPS = IPS + 1
               EL(I,J) = SCR(IPS)
               EL(J,I) = SCR(IPS)
               XX(I,J) = BEMCHG(IPS)
               XX(J,I) = BEMCHG(IPS)
            ENDDO
         ENDDO
C
         CQEF(ITS)=ZERO
         DQSE(ITS)=ZERO
         DO II = 1, L1
            DO JJ = 1, L1
              DQSE(ITS) = DQSE(ITS) + D(II,JJ)*XX(II,JJ)
              CQEF(ITS) = CQEF(ITS) + D(II,JJ)*EL(II,JJ)
            ENDDO
         ENDDO
C
      ENDDO
C
      DO I=1,NTS
        VECGP =ZERO
        DO J=1,NTS
          SEIJ=SE(I,J)
          IF(ITW.EQ.1.OR.ITW.EQ.2) SEIJ=SE(I,J)*EPS/EPSINF
          DELTAIJ=ZERO
          IF(I.EQ.J)DELTAIJ=AS(I)
          VECGP=VECGP-(DELTAIJ/TWO-DE(I,J))*CQEF(J)
     *               -SEIJ*DQSE(J)
       ENDDO
       Q(I) = VECGP
       ENDDO
C
      ENDIF
C
      CALL SEQREW(NFT27)
C
      DQETOT = ZERO
      DO ITS = 1, NTS
       IF(ITW.EQ.0) THEN
        D2Q(ITS,IC1,IC2) = ZERO
       ELSE
        IF(ITW.EQ.1)D2QFR1(ITS,IC1,IC2) = ZERO
        IF(ITW.EQ.2)D2QFR2(ITS,IC1,IC2) = ZERO
        IF(ITW.EQ.3)D2QFR3(ITS,IC1,IC2) = ZERO
       END IF
C
      DO ITSJ = 1, NTS
      IF(ITW.EQ.0) THEN
        D2Q(ITS,IC1,IC2) =
     *  D2Q(ITS,IC1,IC2) + DMATM1(ITS,ITSJ)*Q(ITSJ)
      ELSE
       IF(ITW.EQ.1)D2QFR1(ITS,IC1,IC2)=
     *             D2QFR1(ITS,IC1,IC2)+DMFR(ITS,ITSJ)*Q(ITSJ)
       IF(ITW.EQ.2)D2QFR2(ITS,IC1,IC2)=
     *             D2QFR2(ITS,IC1,IC2)+DMFR(ITS,ITSJ)*Q(ITSJ)
       IF(ITW.EQ.3)D2QFR3(ITS,IC1,IC2)=
     *             D2QFR3(ITS,IC1,IC2)+DMATM1(ITS,ITSJ)*Q(ITSJ)
      END IF
      ENDDO
      IF(ITW.EQ.0) THEN
         D2Q(ITS,IC1,IC2) = D2Q(ITS,IC1,IC2)*AS(ITS)
         DQSE(ITS)=D2Q(ITS,IC1,IC2)
         DQETOT = DQETOT + D2Q(ITS,IC1,IC2)
      ELSEIF(ITW.EQ.1) THEN
         D2QFR2(ITS,IC1,IC2)=D2QFR1(ITS,IC1,IC2)*AS(ITS)
         DQSE(ITS)=D2QFR2(ITS,IC1,IC2)
         DQETOT = DQETOT + D2QFR1(ITS,IC1,IC2)
      ELSEIF(ITW.EQ.2) THEN
         D2QFR2(ITS,IC1,IC2)=D2QFR2(ITS,IC1,IC2)*AS(ITS)
         DQSE(ITS)=D2QFR2(ITS,IC1,IC2)
         DQETOT = DQETOT + D2QFR2(ITS,IC1,IC2)
      ELSE
         D2QFR3(ITS,IC1,IC2)=D2QFR3(ITS,IC1,IC2)*AS(ITS)
         DQSE(ITS)=D2QFR3(ITS,IC1,IC2)
         DQETOT = DQETOT + D2QFR3(ITS,IC1,IC2)
      END IF
      ENDDO
C
C
      IF(ICOMP.NE.0) THEN
      NELEC = NE
      QETEOR = NELEC * ( EPS - ONE ) / EPS
      QCST= - FE * FE * DQETOT / QETEOR
C
C      ITW=0   Static (0 ; 0,0)
C            D2FE(a,b)=-(FE**2/Qe)*{ sum(i) d2[QSE_i(0)]/dEadEb }+
C                              2*DFE(a)*DFE(b)/FE
C      ITW=1   Second harmonic generation (-2W ; +W,+W)
C            D2FEFR(a,b)=-(FE**2/Qe)*{ sum(i) d2[QSE_i(+W,+W)]/dEadEb }+
C                              2*DFEFR(a)*DFEFR(b)/FE
C      ITW=2   Electrooptic Pockels effect (-W ; 0,+W)
C            D2FEFR(a,b)=-(FE**2/Qe)*{ sum(i) d2[QSE_i(W,0)]/dEadEb }+
C                              2*DFEFR(a)*DFE(b)/FE
C      ITW=3   Optical rectification  (0 ; +W,-W)
C            D2FEFR(a,b)=-(FE**2/Qe)*{ sum(i) d2[QSE_i(+W,-W)]/dEadEb }+
C                              2*DFEFR(a)*DFEFR(b)/FE
C
      IF(ITW.EQ.0) THEN
        D2FE(IC1,IC2) = QCST +
     *                  TWO * DFE(IC1) * DFE(IC2) / FE
      ELSE
        FE2= DFEFR(IC2)
        IF(ITW.EQ.2)FE2= DFE(IC2)
        D2FEFR(IC1,IC2,ITW) = QCST + TWO*FE2*DFEFR(IC1)/FE
      END IF
C
       QETOTN = ZERO
       DO ITS = 1, NTS
         IF(ITW.EQ.0) THEN
          DQSE(ITS)= D2Q(ITS,IC1,IC2)*FE+
     *                DQ(ITS,IC1)*DFE(IC2)+
     *                DQ(ITS,IC2)*DFE(IC1)+
     *                D2FE(IC1,IC2)*QSE(ITS)/FE
         ELSE
C      ITW=1   Second harmonic generation   W1=+W, W2=+W  Wt=2W
C      ITW=2   Electrooptic Pockels effect  W1=+W, W2=0   Wt=W
C      ITW=3   Optical rectification        W1=+W, W2=-W  Wt=0
         CST=D2FEFR(IC1,IC2,ITW)*QSE(ITS)/FE
         IF(ITW.EQ.1) DQSE(ITS)= D2QFR1(ITS,IC1,IC2)*FE+
     *                           DQFR(ITS,IC1)*DFEFR(IC2)+
     *                           DQFR(ITS,IC2)*DFEFR(IC1)+CST
         IF(ITW.EQ.2) DQSE(ITS)= D2QFR2(ITS,IC1,IC2)*FE+
     *                           DQFR(ITS,IC1)*DFE(IC2)+
     *                           DQ(ITS,IC2)*DFEFR(IC1)+CST
         IF(ITW.EQ.3) DQSE(ITS)= D2QFR3(ITS,IC1,IC2)*FE+
     *                           DQFR(ITS,IC1)*DFEFR(IC2)+
     *                           DQFR(ITS,IC2)*DFEFR(IC1)+CST
         END IF
         QETOTN = QETOTN + DQSE(ITS)
        ENDDO
C     Stampa i valori della carica prima e dopo la normalizzazione
C       WRITE(IW,1000) DQETOT , QETOTN
C  1000 FORMAT('SOMMA DERIVATE PRIMA ',F10.6,' E DOPO LA ',
C      * 'NORMALIZZAZIONE ',F10.6)
      END IF
C
      NSQR=L1*L1
      IF(IEF.NE.0)CALL VCLR(XX,1,NSQR)
      IF(IEF.GE.3)CALL SEQREW(NFT27)
C
C     Calcola X(dD/dEx)
      DO ITS = 1, NTS
      IF(IEF.LT.3)THEN
       CALL INTMEP(SCR,XCTS(ITS),YCTS(ITS),ZCTS(ITS))
      ELSE
        CALL SQREAD(NFT27,SCR,NUM2)
      ENDIF
       IPS = 0
       DO I = 1, L1
         DO J = 1, I
           IPS = IPS + 1
           EL(I,J) = - SCR(IPS)
           EL(J,I) = - SCR(IPS)
         ENDDO
       ENDDO
C
C       Calcola gli elementi della matrice X
       DO IBAS = 1, L1
         DO JBAS = 1, L1
           XX(IBAS,JBAS) = XX(IBAS,JBAS) + EL(IBAS,JBAS) * DQSE(ITS)
         ENDDO
       ENDDO
C
      ENDDO
      IF(IEF.GE.3)CALL SEQREW(NFT27)
      RETURN
      END
C*MODULE PCMPOL  *DECK WDXEFF
      SUBROUTINE WDXEFF(D,XX,EL,SCR,CEL,DMATM1,BEMCHG,Q,DQSE,
     *                  CQEF,QEFF,ELD,QEF1,ELD1,DQ1,
     *                  ICOOR,L1,NUM2,NTS,NFT27)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
      DIMENSION D(L1,L1),XX(L1,L1),EL(L1,L1),SCR(NUM2),CEL(NUM2),
     *          DMATM1(NTS,NTS),BEMCHG(NUM2),Q(NTS),CQEF(NTS),DQSE(NTS),
     *          QEFF(NTS),ELD(NTS),QEF1(3,NTS),ELD1(3,NTS),DQ1(3,NTS)
      DIMENSION WORK(225)
C
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /ELPROP/ ELDLOC,ELMLOC,ELPLOC,ELFLOC,
     *                IEDEN,IEMOM,IEPOT,IEFLD,MODENS,
     *                IEDOUT,IEMOUT,IEPOUT,IEFOUT,
     *                IEDINT,IEMINT,IEPINT,IEFINT
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMCAV/ OMEGA,RET,FRO,ALPHA(MXSP),RIN(MXSP),ICENT,
     *                IPRINT,IRETCAV
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTESS
      COMMON /PCMUGG/ CSF0,SUMEL,CSF1(3),SUMEL1(3),FE1(3)
      COMMON /XYZPRP/ XP,YP,ZP,DMY(35)
C
      DATA ZERO, ONE/0.0D+00, 1.0D+00/
      DATA PI/3.141592653589793D+00/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA ELDEN/8HELDEN   /
#else
      CHARACTER*8 :: ELDEN_STR
      EQUIVALENCE (ELDEN, ELDEN_STR)
      DATA ELDEN_STR/"ELDEN   "/
#endif
C
C     Routine per il calcolo della matrice XX (interazione tra
C     pseudo-cariche virtuali elettroniche e il potenziale elettronico
C     del soluto) usando la DERIVATA della matrice densita' rispetto
C     alla componente ICOOR del campo elettrico esterno statico (W=0)
C         XX(0)mn = sum_i[ V(m,n;i) d[QSE_i(0)]/dEx ]
C         d[QSE_i(0)]/dEx = sum_m,n[ dD(0,m,n)/dEx qse_i(0)mn ]
C     dove Ex e' la componente x del campo elettrico esterno.
C
C     La matrice dD/dEx e' passata in D
C
C      d[QSE_i(0)]/dEx = a(i)sum_j[ DMATM1(i,j)
C           {sum_m,n[ dD(m,n)/Ex(-dV(m,n;i)/dn) ]}
C
      FPI=4.0D+00*PI
      TPI=2.0D+00*PI
      COEF=(EPS-ONE)/EPS
C
      IF (ICOOR.GT.3) THEN
         IF(MASWRK)WRITE(IW,*)
     *     'ICOOR IS EXPECTED TO BE IN THE RANGE 1 TO 3'
         CALL ABRT
      END IF
C
C     Legge le cariche elementari (mu,nu) su ogni tessera, e forma la
C     matrice completa
C     ITW=0 caso statico: [qse_i(0)]mn
C
      SUMEL1(ICOOR)=ZERO
      FE1(ICOOR)=ZERO
      CSF1(ICOOR)=ZERO
C
      CALL SEQREW(NFT27)
C
      DO ITS = 1, NTS
         CQEF(ITS)=ZERO
         QEF1(ICOOR,ITS)=ZERO
         ELD1(ICOOR,ITS)=ZERO
         DQ1(ICOOR,ITS)=ZERO
C
         CALL SQREAD(NFT27,BEMCHG,NUM2)
         IPS = 0
         DO I = 1, L1
            DO J = 1, I
               IPS = IPS + 1
               EL(I,J) = BEMCHG(IPS)
               EL(J,I) = BEMCHG(IPS)
               SCR(IPS)= D(I,J)
            ENDDO
         ENDDO
C
         Q(ITS)=ZERO
         DO I = 1, L1
            DO JJ = 1, L1
               Q(ITS) = Q(ITS) + D(I,JJ)*EL(I,JJ)
            ENDDO
         ENDDO
      ENDDO
C
      CALL SEQREW(NFT27)
C
      QC=ZERO
      CALL SFUG(SCR,CEL,CSF,QC,NUM2)
      IF(ABS(CSF).LE.0.008D+00) GO TO 33
      CSF1(ICOOR)=CSF
C
C     ----- CALCULATE ELECTRIC DENSITY AT ALL POINTS -----
C
      IEDENOLD=IEDEN
      IEDEN=1
      NVAL = 1
      QDT=ZERO
      DO 30 INR=1,NTS
         XP=XCTS(INR)
         YP=YCTS(INR)
         ZP=ZCTS(INR)
         CALL PRCALC(ELDEN,XX,WORK,NVAL,NUM2)
         ELD1(ICOOR,INR) = TRACEP(SCR,XX,L1)
         QDT=QDT+ELD1(ICOOR,INR)*AS(INR)
   30 CONTINUE
      SUMEL1(ICOOR)=QDT
C
      DO ITS = 1, NTS
         QQF=(CSF*ELD(ITS)+CSF0*ELD1(ICOOR,ITS))/SUMEL
         QEF1(ICOOR,ITS)=COEF*QQF-QEFF(ITS)*QDT/SUMEL
      ENDDO
C
      DO ITS = 1, NTS
         FF=ELD1(ICOOR,ITS)/ELD(ITS)-SUMEL1(ICOOR)/SUMEL
         L=ISPHE(ITS)
         ETA=SQRT(AS(ITS)/(FPI*RE(L)**2))
         DI =-TPI*(ONE-ETA)*QEFF(ITS)*(FF+CSF/CSF0)
         K=ITS+NTS
         XI=XCTS(ITS)
         YI=YCTS(ITS)
         ZI=ZCTS(ITS)
         CX=(XCTS(K)-XI)/DR
         CY=(YCTS(K)-YI)/DR
         CZ=(ZCTS(K)-ZI)/DR
         DJ=ZERO
         DO 150 JTS=1,NTS
            IF(JTS.EQ.ITS) GO TO 150
            FF=ELD1(ICOOR,JTS)/ELD(JTS)-SUMEL1(ICOOR)/SUMEL
            FF=FF+CSF/CSF0
            XJ=XCTS(JTS)
            YJ=YCTS(JTS)
            ZJ=ZCTS(JTS)
            AVF=AS(JTS)
            RIJ=SQRT((XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2)
            IF(RIJ.LT..026D+00) GO TO 150
            DJ=DJ-AVF*QEFF(JTS)*FF*((XI-XJ)*CX+
     *               (YI-YJ)*CY+(ZI-ZJ)*CZ)/RIJ**3
  150    CONTINUE
         CQEF(ITS)=DI+DJ
      ENDDO
C
 33   CONTINUE
      DQETOT = ZERO
      DQSF = ZERO
      DQE0=ZERO
      SUMV=ZERO
      DO 20 ITS = 1, NTS
         SUMV=SUMV+QSE(ITS)/FE
         DQ =ZERO
         DQ0=ZERO
         DO 10 ITSJ = 1, NTS
            DMT=DMATM1(ITS,ITSJ)
            DQ =DQ+DMT*(CQEF(ITSJ)+Q(ITSJ))
            DQ0=DQ0+DMT*Q(ITSJ)
  10     CONTINUE
         DQ1(ICOOR,ITS)=DQ*AS(ITS)
         DQSE(ITS)=DQ1(ICOOR,ITS)
         DQETOT   =DQETOT + DQ1(ICOOR,ITS)
         DQE0=DQE0+DQ0*AS(ITS)
         DQSF=DQSF+QEF1(ICOOR,ITS)*AS(ITS)
  20  CONTINUE
C
      NELEC =NE
      QETEOR=(NELEC-CSF0)*(EPS-ONE)/EPS
      F1=-COEF*CSF/SUMV
      FE1(ICOOR) = F1-FE*FE*DQETOT/QETEOR
C
      IF(ICOMP.NE.0) THEN
         QETOTN = ZERO
         DO ITS=1,NTS
            DQSE(ITS)=DQ1(ICOOR,ITS)*FE + FE1(ICOOR)*QSE(ITS)/FE
            QETOTN   =QETOTN + DQSE(ITS)
         ENDDO
      END IF
C
      IF((IPRINT.GE.1.AND.ABS(CSF).GT.0.008D+00) .AND. MASWRK)
     *   WRITE(IW,110) ICOOR,DQETOT+DQSF,-DQSF*EPS/(EPS-1.0D+00),
     *                 DQE0,QETOTN+DQSF
C
      CALL VCLR(XX,1,NUM2)
C
C     Calcola X(dD/dEx)
C
      DO ITS = 1, NTS
         DQSE(ITS)=DQSE(ITS)+QEF1(ICOOR,ITS)*AS(ITS)
         CALL INTMEP(SCR,XCTS(ITS),YCTS(ITS),ZCTS(ITS))
C
         IPS = 0
         DO I = 1, L1
            DO J = 1, I
               IPS = IPS + 1
               EL(I,J) = - SCR(IPS)
               EL(J,I) = - SCR(IPS)
            ENDDO
         ENDDO
C
C       Calcola gli elementi della matrice X
C
         DO IBAS = 1, L1
            DO JBAS = 1, L1
               XX(IBAS,JBAS) = XX(IBAS,JBAS) + EL(IBAS,JBAS) * DQSE(ITS)
            ENDDO
         ENDDO
      ENDDO
C
      IEDEN=IEDENOLD
      RETURN
C
 110  FORMAT(I3,' CHARGE:TOT,ESC,UNC,FIN',4(1X,F8.4))
      END
C*MODULE PCMPOL  *DECK WD2XEFF
      SUBROUTINE WD2XEFF(D,XX,EL,SCR,CEL,DMATM1,BEMCHG,Q,DQSE,
     *                   CQEF,QEFF2,QEFF,ELD,QEF1,ELD1,DQ1,
     *                   IC1,IC2,L1,NUM2,NTS,NFT27)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
      DIMENSION D(L1,L1),XX(L1,L1),EL(L1,L1),SCR(NUM2),CEL(NUM2),
     *          DMATM1(NTS,NTS),BEMCHG(NUM2),
     *          Q(NTS),DQSE(NTS),CQEF(NTS),QEFF2(NTS),
     *          QEFF(NTS),ELD(NTS),QEF1(3,NTS),ELD1(3,NTS),DQ1(3,NTS)
      DIMENSION WORK(225)
C
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /ELPROP/ ELDLOC,ELMLOC,ELPLOC,ELFLOC,
     *                IEDEN,IEMOM,IEPOT,IEFLD,MODENS,
     *                IEDOUT,IEMOUT,IEPOUT,IEFOUT,
     *                IEDINT,IEMINT,IEPINT,IEFINT
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMCAV/ OMEGA,RET,FRO,ALPHA(MXSP),RIN(MXSP),ICENT,
     *                IPRINT,IRETCAV
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTESS
      COMMON /PCMUGG/ CSF0,SUMEL,CSF1(3),SUMEL1(3),FE1(3)
      COMMON /XYZPRP/ XP,YP,ZP,DMY(35)
C
      DATA ZERO, ONE/0.0D+00, 1.0D+00/
      DATA PI/3.141592653589793D+00/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA ELDEN/8HELDEN   /
#else
      CHARACTER*8 :: ELDEN_STR
      EQUIVALENCE (ELDEN, ELDEN_STR)
      DATA ELDEN_STR/"ELDEN   "/
#endif
C
C     Routine per il calcolo della matrice XX (interazione tra
C     pseudo-cariche virtuali elettroniche e il potenziale elettronico
C     del soluto) usando la DERIVATA  SECONDA della matrice densita'
C     rispetto alle componenti IC1 e IC2 del campo elettrico esterno
C     statico (Wt=0, W1=W2=0)
C         XX(0)mn = sum_i[ V(m,n;i) d2[QSE_i(0,0)]/dExdEy ]
C         d2[QSE_i(0,0)]/dExdEy =
C            sum_m,n{ d2[D(0,0)mn]/dExdEy qse_i(0)mn }
C     dove Ex,Ey sono le componenti x,y del campo elettrico esterno.
C
C      d[QSE_i(0)]/dEx = a(i)sum_j[ DMATM1(i,j)
C             {sum_m,n[ dD(m,n)/Ex(-dV(m,n;i)/dn) ]}
C
      FPI=4.0D+00*PI
      TPI=2.0D+00*PI
      COEF=(EPS-ONE)/EPS
C
      IF (IC1.GT.3.OR.IC2.GT.3) THEN
         IF(MASWRK)WRITE(IW,*)
     *       'IC1 AND IC2 ARE EXPECTED TO BE FROM 1 TO 3'
         CALL ABRT
      END IF
C
C      ITW=0   Static                       W1=0,  W2=0   Wt=0
C     Moltiplica per la derivata della matrice densita' per trovare la
C     derivata della carica virtuale elettronica sulla tessera
C
      CALL SEQREW(NFT27)
C
      DO ITS = 1, NTS
         CQEF(ITS) =ZERO
         QEFF2(ITS)=ZERO
C
         CALL SQREAD(NFT27,BEMCHG,NUM2)
         IPS = 0
         DO I = 1, L1
            DO J = 1, I
               IPS = IPS + 1
               EL(I,J) = BEMCHG(IPS)
               EL(J,I) = BEMCHG(IPS)
               SCR(IPS)=D(I,J)
            ENDDO
         ENDDO
C
         Q(ITS)=ZERO
         DO I = 1, L1
            DO JJ = 1, L1
               Q(ITS) = Q(ITS) + D(I,JJ)*EL(I,JJ)
            ENDDO
         ENDDO
      ENDDO
C
      CALL SEQREW(NFT27)
C
C     ----- CALCOLA LA CARICA FUORI DELLA CAVITA' -----
C
      QC=ZERO
      CALL SFUG(SCR,CEL,CSF,QC,NUM2)
      IF(ABS(CSF).LE.0.008D+00) GO TO 34
C
C     ----- CALCULATE ELECTRIC DENSITY AT ALL POINTS -----
C
       IEDENOLD=IEDEN
       IEDEN=1
       NVAL =1
       QDT=ZERO
       DO 30 INR=1,NTS
          XP=XCTS(INR)
          YP=YCTS(INR)
          ZP=ZCTS(INR)
          CALL PRCALC(ELDEN,XX,WORK,NVAL,NUM2)
          QEFF2(INR)=TRACEP(SCR,XX,L1)
          QDT=QDT+QEFF2(INR)*AS(INR)
   30  CONTINUE
C
      QESF=ZERO
      DO ITS = 1, NTS
         ED1=ELD1(IC1,ITS)
         ED2=ELD1(IC2,ITS)
         QQ0=QEFF(ITS)*QDT/SUMEL
         QQ1=(QEF1(IC1,ITS)*SUMEL1(IC2)+
     *        QEF1(IC2,ITS)*SUMEL1(IC1))/SUMEL
         QQ21=CSF*ELD(ITS)+CSF0*QEFF2(ITS)
         QQ22=CSF1(IC1)*ED2+CSF1(IC2)*ED1
         QQ2=(QQ21+QQ22)/SUMEL
         QEFF2(ITS)=COEF*QQ2-QQ0-QQ1
         QESF=QESF+QEFF2(ITS)*AS(ITS)
      ENDDO
C
      DO ITS = 1, NTS
         L=ISPHE(ITS)
         ETA=SQRT(AS(ITS)/(FPI*RE(L)**2))
         DI =-TPI*(ONE-ETA)*QEFF2(ITS)
         K=ITS+NTS
         XI=XCTS(ITS)
         YI=YCTS(ITS)
         ZI=ZCTS(ITS)
         CX=(XCTS(K)-XCTS(ITS))/DR
         CY=(YCTS(K)-YCTS(ITS))/DR
         CZ=(ZCTS(K)-ZCTS(ITS))/DR
         DJ=ZERO
         DO 150 JTS=1,NTS
            IF(JTS.EQ.ITS) GO TO 150
            XJ=XCTS(JTS)
            YJ=YCTS(JTS)
            ZJ=ZCTS(JTS)
            AVF=AS(JTS)
            RIJ=SQRT((XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2)
            IF(RIJ.LT.0.026D+00) GO TO 150
            DJ=DJ-AVF*QEFF2(JTS)*
     *            ((XI-XJ)*CX+(YI-YJ)*CY+(ZI-ZJ)*CZ)/RIJ**3
  150   CONTINUE
        CQEF(ITS)=DI+DJ
      ENDDO
C
  34  CONTINUE
      DQETOT=ZERO
      DQSF  =ZERO
      DQE0  =ZERO
      SUMV =ZERO
      SUMV1=ZERO
      SUMV2=ZERO
      DO ITS = 1, NTS
       SUMV1=SUMV1+DQ1(IC1,ITS)
       SUMV2=SUMV2+DQ1(IC2,ITS)
       SUMV =SUMV+QSE(ITS)/FE
       D2Q=ZERO
       DQ0=ZERO
       DO ITSJ = 1, NTS
       DMT=DMATM1(ITS,ITSJ)
        QQ=Q(ITSJ)+CQEF(ITSJ)
        D2Q=D2Q+DMT*QQ
       DQ0=DQ0+DMT*Q(ITSJ)
       ENDDO
       DQSE(ITS)=D2Q*AS(ITS)
       DQETOT   =DQETOT+DQSE(ITS)
       DQE0=DQE0+DQ0*AS(ITS)
       DQSF=DQSF+QEFF2(ITS)*AS(ITS)
      ENDDO
C
      NELEC = NE
      QETEOR = (NELEC-CSF0) * ( EPS - ONE ) / EPS
      DF=(FE1(IC1)*SUMV2 + SUMV1*FE1(IC2))/SUMV
      DF=-DF-FE*FE*DQETOT/QETEOR - COEF*CSF/SUMV
C
      IF(ICOMP.NE.0) THEN
        QETOTN = ZERO
        DO ITS = 1, NTS
        FACT=FE1(IC1)*DQ1(IC2,ITS)+FE1(IC2)*DQ1(IC1,ITS)
         DQSE(ITS) = DQSE(ITS)*FE + DF*QSE(ITS)/FE +FACT
         QETOTN = QETOTN + DQSE(ITS)
        ENDDO
      END IF
C
      IF((IPRINT.GE.1.AND.ABS(CSF).GT.0.008D+00).AND.MASWRK) THEN
        WRITE(IW,110)IC1,IC2,DQETOT+DQSF,-DQSF*EPS/(EPS-1.0D+00),DQE0,
     *                            QETOTN+DQSF
      END IF
C
      CALL VCLR(XX,1,NUM2)
C
C     Calcola X(dD/dEx)
C
      DO ITS = 1, NTS
         DQSE(ITS)=DQSE(ITS)+QEFF2(ITS)*AS(ITS)
         CALL INTMEP(SCR,XCTS(ITS),YCTS(ITS),ZCTS(ITS))
C
         IPS = 0
         DO I = 1, L1
            DO J = 1, I
               IPS = IPS + 1
               EL(I,J) = - SCR(IPS)
               EL(J,I) = - SCR(IPS)
            ENDDO
         ENDDO
C
C       Calcola gli elementi della matrice X
C
         DO IBAS = 1, L1
            DO JBAS = 1, L1
               XX(IBAS,JBAS) = XX(IBAS,JBAS) + EL(IBAS,JBAS) * DQSE(ITS)
            ENDDO
         ENDDO
      ENDDO
C
      IEDEN=IEDENOLD
      RETURN
 110  FORMAT(2(I3,1X),'CHARGE:TOT,ESC,UNC,FIN',4(1X,F8.4))
      END
C*MODULE PCMPOL  *DECK GAMPCM
      SUBROUTINE GAMPCM(NORB,NTS,NTS2,NBMCHG,
     *                  VEC,EIGS,UA,UBC,F,GA,GBC,T,
     *                  DSCR,EBC,TM,SCR,USCR,SCR2,
     *                  DM,DMATM1,BEMCHG,SE,DE,Q,Q3,DQSE,DQFR,DQ,
     *                  D2QFR1,D2QFR2,D2QFR3,D2Q)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     ----- THIS ROUTINE CALCULATES THE  SECOND HYPERPOLARIZABILTY
C           FROM THE FIRST & SECOND DERIVATIVES OF THE CLOSED SHELL
C           SCF MOLECULAR ORBITALS AND THE FIRST & SECOND DERIVATIVE
C           FOCK MATRICES (IN THE MO BASIS)
C
C
C           REQUIRED RECORDS FROM POLARIZABILITY CALCULATION ARE
C           RECORD  -X - DIPOLE MATRIX -95-
C                   -Y - DIPOLE MATRIX -96-
C                   -Z - DIPOLE MATRIX -97-
C
C  ------------------------------------------
C                                  (0)  (W)
C  ------------------------------------------
C   -U'- MATRIX FOR -X- DIRECTION -101- 107
C   -U'- MATRIX FOR -Y- DIRECTION -102- 108
C   -U'- MATRIX FOR -Z- DIRECTION -103- 109
C   -G'- MATRIX FOR -X- DIRECTION -104- 110
C   -G'- MATRIX FOR -Y- DIRECTION -105- 111
C   -G'- MATRIX FOR -Z- DIRECTION -106- 112
C  -----------------------------------------------------
C                                  (0,0)(W,W)(0,W)(-W,W)
C  -----------------------------------------------------
C   -U"- MATRIX FOR -XX- DIRECTION -125- 149  173  209
C   -U"- MATRIX FOR -XY- DIRECTION -126- 150  174  210
C   -U"- MATRIX FOR -XZ- DIRECTION -127- 151  175  211
C   -U"- MATRIX FOR -YX- DIRECTION            176  212
C   -U"- MATRIX FOR -YY- DIRECTION -128- 152  177  213
C   -U"- MATRIX FOR -YZ- DIRECTION -129- 153  178  214
C   -U"- MATRIX FOR -ZX- DIRECTION            179  215
C   -U"- MATRIX FOR -ZY- DIRECTION            180  216
C   -U"- MATRIX FOR -ZZ- DIRECTION -130- 154  181  217
C   -G"- MATRIX FOR -XX- DIRECTION -131- 155  182  218
C   -G"- MATRIX FOR -XY- DIRECTION -132- 156  183  219
C   -G"- MATRIX FOR -XZ- DIRECTION -133- 157  184  220
C   -G"- MATRIX FOR -YX- DIRECTION            185  221
C   -G"- MATRIX FOR -YY- DIRECTION -134- 158  186  222
C   -G"- MATRIX FOR -YZ- DIRECTION -135- 159  187  223
C   -G"- MATRIX FOR -ZX- DIRECTION            188  224
C   -G"- MATRIX FOR -ZY- DIRECTION            189  225
C   -G"- MATRIX FOR -ZZ- DIRECTION -136- 160  190  226
C   -E"- MATRIX FOR -XX- DIRECTION -137- 161  191  227
C   -E"- MATRIX FOR -XY- DIRECTION -138- 162  192  228
C   -E"- MATRIX FOR -XZ- DIRECTION -139- 163  193  229
C   -E"- MATRIX FOR -YX- DIRECTION            194  230
C   -E"- MATRIX FOR -YY- DIRECTION -140- 164  195  231
C   -E"- MATRIX FOR -YZ- DIRECTION -141- 165  196  232
C   -E"- MATRIX FOR -ZX- DIRECTION            197  233
C   -E"- MATRIX FOR -ZY- DIRECTION            198  234
C   -E"- MATRIX FOR -ZZ- DIRECTION -142- 166  199  235
C  -----------------------------------------------------
C   -U-1"- MATRIX FOR -XX- DIRECTION 143 167  200  236
C   -U-1"- MATRIX FOR -XY- DIRECTION 144 168  201  237
C   -U-1"- MATRIX FOR -XZ- DIRECTION 145 169  202  238
C   -U-1"- MATRIX FOR -YX- DIRECTION          203  239
C   -U-1"- MATRIX FOR -YY- DIRECTION 146 170  204  240
C   -U-1"- MATRIX FOR -YZ- DIRECTION 146 171  205  241
C   -U-1"- MATRIX FOR -ZX- DIRECTION          206  242
C   -U-1"- MATRIX FOR -ZY- DIRECTION          207  243
C   -U-1"- MATRIX FOR -ZZ- DIRECTION 147 172  208  244
C
      CHARACTER*1 DIR(3)
      CHARACTER*1 FRQL(6),FRQM(6)
      CHARACTER*2 FRQR(6)
      CHARACTER*36 WORK(6)
      LOGICAL PACK2E
      LOGICAL GTHG,GEFISH,GIDRI,GOKE,BSHG,BEOPE,BOR
C
      PARAMETER (MXATM=500)
C
      COMMON /FMCOM / X(1)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /TDHFP / ATOL,BTOL,OMEGA,FREQ(10),
     X                MAXITA,MAXITU,NCLOSE,NFREQ,
     X                GTHG,GEFISH,GIDRI,GOKE,BSHG,BEOPE,BOR,
     X                INIB,INIG
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /PCMPAR/ IPCM,NFT26,NFT27,IRPPCM,IEF,IP_F
      COMMON /PCMDIS/ WB,WA,ETA2,GD,EVAC,IDP
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
C from hondo
      COMMON/DATF2H/GAMMA(81),IOP
      COMMON/HONFPL/VNEW,VOLD,CNVA,CNVB,DAMP(19)
C
      DIMENSION VEC(NORB,NORB),EIGS(NORB),
     *          UA(NORB,NORB),UBC(NORB,NORB),
     *          F(NORB,NORB),GA(NORB,NORB),GBC(NORB,NORB),
     *          T(NORB,NORB),DSCR(NORB,NORB),EBC(NORB,NORB),
     *          TM(NORB,NORB),SCR(NORB,NORB),USCR(NORB,NORB),
     *          SCR2(NORB,NORB),DM(NTS,NTS),DMATM1(NTS,NTS),
     *          SE(NTS2,NTS2),DE(NTS2,NTS2),
     *          BEMCHG(NBMCHG),Q(NTS),Q3(NTS),DQSE(NTS),
     *          DQFR(NTS,3),DQ(NTS,3),D2QFR1(NTS,3,3),D2QFR2(NTS,3,3),
     *          D2QFR3(NTS,3,3),D2Q(NTS,3,3)
C
      DIMENSION IPT(3,3)
      DIMENSION NUGREC(6,3),NUG2RC(6,3)
      DIMENSION NGU(6,3),NEU(6,3),NUMU(6,3)
C
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      PARAMETER (ALPHA=137.035963D+00,ANGS=0.52917715D+00)
      PARAMETER (BOHR=0.52917715D-08,ELCHG=4.8032424D-10)
      PARAMETER (ZERO=0.0D+00, ONE = 1.0D+00, TWO=2.0D+00)
      PARAMETER (ESU39=1.0D+39*BOHR**7/ELCHG**2)
C
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      DATA DIR    /'X','Y','Z'/
      DATA FRQL   /'0','0','0','W','W','0'/
      DATA FRQM   /'0','0','W','W','W','W'/
      DATA FRQR   /' 0',' W',' W',' W','-W','-W'/
      DATA WORK/ ' STATIC SECOND HYPERPOLARIZABILITY  ',
     1           '       OPTICAL KERR EFFECT          ',
     2           '    ELECTRIC FIELD INDUCED S H G    ',
     3           '     THIRD HARMONIC GENERATION      ',
     4           'INTENSITY DEPENDENT REFRACTIVE INDEX',
     5           '  DC ELECTRIC FIELD INDUCED OR      '/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
C
C     ----- DIR  XX, XY, XZ, YX, YY, YZ, ZX, ZY, ZZ
      DATA IPT /  1,  4,  7,  2,  5,  8, 3,  6,  9 /
C
      PI= ACOS(-ONE)
      CNVA=0.005D+000
      CNVB=0.005D+000
      DO II=1,19
       DAMP(II)=0.2D+000
      ENDDO
C
C     ----- SET POINTERS FOR PARTITIONING OF CORE -----
C     ----- FOR U" ITERATIVE CALCULATION -----
C
      L0= NORB
      L1= NORB
      L2=(NORB*(NORB+1))/2
      L3= NORB*NORB
C
      CALL VALFM(LOADFM)
      I15  = 1   + LOADFM
      I60  = I15 + L2
      I61  = I60 + NINTMX
      LAST = I61 + NINTMX
         IF(IPCM.EQ.1) THEN
            LF0S = LAST
            LF1S = LF0S + L2*3
            LF2S = LF1S + L3
            LF3S = LF2S + L3
            LF4S = LF3S + L2
            LAST = LF4S + L1
         ELSE
            LF0S = LAST
            LF1S = LAST
            LF2S = LAST
            LF3S = LAST
            LF4S = LAST
         END IF
      NEED =LAST-LOADFM-1
      CALL GETFM(NEED)
C
      IF(EXETYP.EQ.CHECK) GO TO 700
C
      IF(MASWRK)WRITE(IW,9000)
 9000 FORMAT(/,10X,44("-"),
     1       /,10X,'DIPOLE SECOND HYPERPOLARIZABILITY VIA -CPHF-',
     1       /,10X,44(1H-))
C
      CALL DAREAD (IDAF,IODA,VEC,L3,15,0)
      CALL DAREAD (IDAF,IODA,EIGS,L1,17,0)
C
C     ----- RECORD CONTROL ARRAYS FOR DIFFERENT PROCESSES -----
C
       DO 5 I = 1,6
       DO 5 J = 1,3
        IF(I.EQ.1.OR.I.EQ.4) THEN
         NGU(I,J)  = 0
         NEU(I,J)  = 0
         NUMU(I,J) = 0
        ELSE
         NGU(I,J)  = 3
         NEU(I,J)  = 6
         NUMU(I,J) = 9
        END IF
         NUGREC(I,J) = 0
         NUG2RC(I,J) = 0
    5  CONTINUE
       NUGREC(2,3) = 6
       NUGREC(3,2) = 6
       NUGREC(3,3) = 6
       NUGREC(4,1) = 6
       NUGREC(4,2) = 6
       NUGREC(4,3) = 6
       NUGREC(5,1) = 6
       NUGREC(5,2) = 6
       NUGREC(5,3) = 6
       NUGREC(6,2) = 6
       NUGREC(6,3) = 6
C
C   ITW
C    1   STATIC                               (0;0,0,0)
C    2   OPTICAL KERR EFFECT                  (W;0,0,W)
C    3   ELECTRIC FIELD INDUCED S H G         (2W;0,W,W)
C    4   THIRD HARMONIC GENERATION            (3W;W,W,W)
C    5   INTENSITY DEPENDENT REFRACTIVE INDEX (W;W,W,-W)
C    6   DC ELECTRIC FIELD INDUCED OR         (0;0,W,-W)
C
       NGU(2,3) = 0
       NGU(3,1) = 0
       NGU(5,3) = 0
       NEU(2,3) = 0
       NEU(3,1) = 0
       NEU(5,3) = 0
       NUMU(2,3) = 0
       NUMU(3,1) = 0
       NUMU(5,3) = 0
       NUG2RC(2,1) = 48
       NUG2RC(2,2) = 48
       NUG2RC(3,1) = 24
       NUG2RC(3,2) = 48
       NUG2RC(3,3) = 48
       NUG2RC(4,1) = 24
       NUG2RC(4,2) = 24
       NUG2RC(4,3) = 24
       NUG2RC(5,1) = 84
       NUG2RC(5,2) = 84
       NUG2RC(5,3) = 24
       NUG2RC(6,1) = 84
       NUG2RC(6,2) = 48
       NUG2RC(6,3) = 48
C
C      ----- FREQUENCY PARAMETERS -----
C
C     ----- RECORD LOCATION PARAMETERS -----
C
      JUREC  = 100
      JGREC  = 103
      JU2REC = 124
      JG2REC = 130
      JEPREC = 136
      JU2MRC = 142
C
C      ----- FREQUENCY PARAMETERS -----
C
         ALAMB1=ZERO
         ALAMB2=ZERO
         ALAMB3=ZERO
         WOM1=ZERO
         WOM2=ZERO
         WOM3=ZERO
         ITWMIN=1
         ITWMAX=ITWMIN
         IF (OMEGA .NE. ZERO) ITWMAX=6
        DO 500 ITW=ITWMIN,ITWMAX
C
       JDIR=9
       IF (ITW .EQ. 1) GO TO 30
       CONST=TWO*PI*ALPHA*ANGS
       WOM3   = OMEGA
       ALAMB3 = CONST/WOM3
       IF (ITW .EQ. 2) GO TO 30
       ALAMB2 = ALAMB3
       WOM2   = WOM3
       IF (ITW .EQ. 3) GO TO 30
       ALAMB1 = ALAMB2
       WOM1   = WOM2
       IF (ITW .EQ. 4) GO TO 30
       ALAMB3 = -ALAMB2
       WOM3   = -WOM2
       IF (ITW .EQ. 5) GO TO 30
       ALAMB1 = ZERO
       WOM1   = ZERO
   30  WOM    = WOM1+WOM2+WOM3
C
C     ----- LOOP OVER DIRECTIONS -----
C
         IOP = 0
         IAB = 0
         DO 400 IA = 1,3
          IB1 = 1
          IF(ITW.EQ.1)IB1 = IA
          DO 300 IB = IB1,3
            IAB=IPT(IA,IB)
            I12 = 0
            IF(IB.EQ.IA)I12 = 1
            IC1=1
            IF(ITW.EQ.1)IC1 = IB
            DO 200 IC = IC1,3
              IF(I12.NE.1) THEN
                IF(IC.NE.IA.AND.IC.NE.IB)GO TO 200
                IF(IC.EQ.IA)IDC = IB
                IF(IC.EQ.IB)IDC = IA
              ELSE
                IDC = IC
              END IF
              IBC = IPT(IB,IC)
              ICA = IPT(IA,IC)
              ID  = IA
C
C     -----  READ A DIPOLE MATRIX TO RECALC SOME GAMMAS CHECKING U".
C            ID SETS WHICH DIPOLE INTEGRALS USED WITH D" FOR BETA
C
        CALL DAREAD(IDAF,IODA,X(I15),L2,94+ID,0)
C
         ADAMP = DAMP(JDIR)
         VNEW  = ZERO
         VOLD  = ZERO
         ITER  = 0
         IEND  = 0
         IMOVE = 1
   50      NRC1  = NUGREC(ITW,IMOVE)
           NRC23 = NUG2RC(ITW,IMOVE)
           N23G  = NRC23+NGU(ITW,IMOVE)
           N23E  = NRC23+NEU(ITW,IMOVE)
           N23UM = NRC23+NUMU(ITW,IMOVE)
           GO TO (60,70,80),IMOVE
   60      J1  = IA
           J23 = IBC
           GO TO 90
   70      J1  = IB
           J23 = ICA
           GO TO 90
   80      J1  = IC
           J23 = IAB
   90      CONTINUE
C
C    1   STATIC                               (0;0,0,0)
C    2   OPTICAL KERR EFFECT                  (W;0,0,W)
C    3   ELECTRIC FIELD INDUCED S H G         (2W;0,W,W)
C    4   THIRD HARMONIC GENERATION            (3W;W,W,W)
C    5   INTENSITY DEPENDENT REFRACTIVE INDEX (W;W,W,-W)
C    6   DC ELECTRIC FIELD INDUCED OR         (0;0,W,-W)
C
       IF(ITW.EQ.1.OR.ITW.EQ.4.OR.(ITW.EQ.2.AND.IMOVE.EQ.3).OR.
     *    (ITW.EQ.3.AND.IMOVE.EQ.1).OR.(ITW.EQ.5.AND.IMOVE.EQ.3)) THEN
        IF(J23.EQ.5.OR.J23.EQ.6)J23=J23-1
        IF(J23.EQ.9)J23=6
       END IF
C
C     ----- READ U(IA) U(IBC) (ORBITAL DERIVATIVE TRANSFORMATIONS)---
C     ----- READ G(IA) G(IBC) (MO DERIVATIVE FOCK MATRICES) -----
C
       CALL DAREAD(IDAF,IODA,UA ,L3,JUREC +NRC1 +J1 ,0)
       CALL DAREAD(IDAF,IODA,GA ,L3,JGREC +NRC1 +J1 ,0)
       CALL DAREAD(IDAF,IODA,UBC,L3,JU2REC+NRC23+J23,0)
       CALL DAREAD(IDAF,IODA,GBC,L3,JG2REC+N23G +J23,0)
       CALL DAREAD(IDAF,IODA,EBC,L3,JEPREC+N23E +J23,0)
          IF (NRC23 .EQ. 0) GO TO 100
       CALL DAREAD(IDAF,IODA,F  ,L3,JU2MRC+N23UM+J23,0)
C
          GO TO 110
C
  100  CALL F2HPUM(F,UBC,NORB,L0,L1)
  110  CONTINUE
C
C     ----- CALCULATE CONSTANT MATRICES ------
C
       CALL F2HPT(T,TM,GA,GBC,F,UA,UBC,SCR,EBC,L0,L1,IMOVE,ITW)
       CALL F2HPD0(VEC,SCR2,GBC,F,UA,UBC,SCR,L0,L1,IMOVE,NRC1,
     *             NRC23,ITW)
       IMOVE = IMOVE+1
       IF(IMOVE .LE. 3) GO TO 50
C
C     ----- PREPARE FOR ZERO-TH ITERATION USING 2*D0 AS DENSITY -----
C           (ALSO SET U MATRIX TO ZERO FOR 0TH CONVERGENCE TEST)
C
       CALL FHPGES(SCR2,GBC,UA,NORB,L1)
       GO TO 170
C
C 150  CALL DAREAD(IDAF,IODA,GBC,L3,94,0)
C
C     ----- START THE ITERATIVE CYCLE -----
C     ----- CALCULATE THE FOCK DERIVATIVE MATRIX -----
C
  160  CONTINUE
       CALL VCLR (F,1,L3)
       CALL F2NONS(GBC,F,X(I60),X(I61),NINTMX,NOPK,1)
C..............................................................
C ADD SOLVATION CONTRIBUTIONS
C..............................................................
           IF(IPCM.EQ.1) THEN
             CALL VCLR (X(LF1S),1,L3)
              CALL WD3XMAT(GBC,X(LF1S),X(LF2S),X(LF3S),
     *                     DMATM1,BEMCHG,SE,DE,Q,DQSE,DM,DQFR,DQ,
     *                     D2QFR1,D2QFR2,D2QFR3,D2Q,
     *                     Q3,IA,IB,IC,ITW,NORB,L2,NTS,NTS2,NFT27)
             CALL VADD(F,1,X(LF1S),1,F,1,L3)
C
C   --- add dispersion interaction (if idp=1) ---
C
             IF(IDP.EQ.1) THEN
                CALL DDISP(GBC,X(LF3S),X(LF0S),X(LF1S),X(LF2S),
     *                     X(LF4S),NORB,L2,NFT27)
                CALL VADD (F,1,X(LF1S),1,F,1,L3)
             END IF
           END IF
C
C     ----- CALCULATE THE DERIVATIVE ORBITAL TRANS'N MATRIX -----
C
          CALL FPLCFC(VEC,F,UA,SCR,NORB,L0,L1)
          CALL FHPU(EIGS,T,UA,SCR,WOM,L0,L1)
C
C     ----- CALCULATE THE DERIVATIVE DENSITY MATRIX -----
C
          CALL FHPDEN(VEC,TM,SCR2,GBC,UA,UBC,DSCR,SCR,
     1                L0,L1,ITER,ADAMP,ITW)
C
  170     CONTINUE
C
          CALL CPYTSQ(X(I15),SCR,NORB,1)
          CALL FPLCVG(GBC,SCR,UA,UBC,USCR,NORB,NA,L0,L1,IEND,ITER)
            IF(IEND.EQ.1.AND.ADAMP.EQ.ZERO) GO TO 180
            IF(IEND.EQ.1) ADAMP=ZERO
            ITER = ITER + 1
            IF(ITER.LE.MAXITU) GO TO 160
C
C           ----- ITERATIVE CALCULATION COMPLETED -----
C
  180       CONTINUE
C
C           ----- WRITE THE DENSITY MATRIX ON THE USCR VECTOR
C
            DO KU=1,NORB
            DO JU=1,NORB
             USCR(KU,JU)=UA(KU,JU)
            ENDDO
            ENDDO
C           CALL DAWRIT(IDAF,IODA,UA,L3,93,0)
C
C     ----- CALCULATE  SECOND HYPERPOLARIZABILITY COMPONENTS -----
C
         CALL F2HPTN(GBC,SCR,X(I15),NORB,L1,IDC)
  200    CONTINUE
  300    CONTINUE
  400    CONTINUE
C
        IF(MASWRK)WRITE(IW,9012)
     *      WORK(ITW),WOM1,ALAMB1,WOM2,ALAMB2,WOM3,ALAMB3
        IF(MASWRK)WRITE(IW,9019)
 9012 FORMAT(/,10X,54("-"),/,19X,A36,9X,/,
     1 /,12X,'FOR W1 =',F12.8,1X,'A.U.',1X,'(',F9.1,1X,'ANGSTROMS)',
     2 /,12X,'    W2 =',F12.8,1X,'A.U.',1X,'(',F9.1,1X,'ANGSTROMS)',
     3 /,12X,'AND W3 =',F12.8,1X,'A.U.',1X,'(',F9.1,1X,'ANGSTROMS)',
     4       /,10X,54(1H-))
 9019 FORMAT(/,9X,'SECOND HYPERPOLARIZABILITY TENSOR',
     1            ' (W.R.T. TRANSLATED INPUT AXES)',
     2 /,12X,'COMPONENT',15X,'(BOHR**7)/E**2',5X,'10**-39 E.S.U.',/)
C
        IABCD = 0
        DO 480 IA = 1,3
         IB1=1
         IF(ITW.EQ.1)IB1=IA
          DO 470 IB = IB1,3
           IC1=1
           I12=0
           IF(IB.EQ.IA)I12=1
           IF(ITW.EQ.1)IC1=IB
            DO 460 IC = IC1,3
              IF(I12.NE.1) THEN
                IF(IC.NE.IA.AND.IC.NE.IB)GO TO 460
                IF(IC.EQ.IA)ID=IB
                IF(IC.EQ.IB)ID=IA
              ELSE
                ID=IC
              END IF
             IABCD=IABCD+1
             IF(MASWRK)WRITE(IW,9013)
     *             DIR(ID),DIR(IA),DIR(IB),DIR(IC),FRQL(ITW),
     *             FRQM(ITW),FRQR(ITW),GAMMA(IABCD),GAMMA(IABCD)*ESU39
 460         CONTINUE
 470       CONTINUE
 480    CONTINUE
C
      IF(MASWRK)WRITE(IW,9018)
C
  500 CONTINUE
C
  700 CONTINUE
      CALL RETFM(NEED)
C
 9013 FORMAT(10X,'GAMMA ',4A1,'(',A1,',',A1,',',A2,')',2F18.4)
 9018 FORMAT(1X)
      RETURN
      END
C*MODULE PCMPOL  *DECK F2HPUM
      SUBROUTINE F2HPUM(UM,U,NUM,L0,NDIM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION UM(NDIM,*), U(NDIM,*)
C
      PARAMETER (ZERO=0.0D+00)
C
      DO 1 J = 1,NUM
      DO 1 I = 1,NUM
    1 UM(I,J) = ZERO
      DO 5 J = 1,L0
      DO 5 I = 1,L0
       UM(I,J) = U(I,J)
    5 CONTINUE
      RETURN
      END
C*MODULE PCMPOL  *DECK F2HPT
      SUBROUTINE F2HPT(T,TM,GA,GBC,UBCM,UA,UBC,EA,EBC,L0,NDIM,INI,ITW)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      DIMENSION T(NDIM,*),TM(NDIM,*),GA(NDIM,*),GBC(NDIM,*),
     1          UBCM(NDIM,*),UA(NDIM,*),UBC(NDIM,*),
     2          EA(NDIM,*),EBC(NDIM,*)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
C
      PARAMETER (ZERO=0.0D+00, PT5=0.5D+00)
C
C     ------------------------------------------------------------------
C     ---- CALCULATE CONSTANT OCC-OCC AND VIRT-VIRT BLOCKS OF U ABC ----
C                                AND
C               CONSTRUCT THE OCC-VIRT ; VIRT-OCC BLOCKS OF
C                           T ABC MATRIX
C     ------------------------------------------------------------------
C
C     ----- CALC OCC,OCC AND VIRT,VIRT; CLEAR OCC,VIRT -----
C     ----- INITIALIZATION STEP ------
C
      DO 1 J=1,L0
       DO 1 I=1,L0
       EA(I,J) = ZERO
   1  CONTINUE
      IF (INI .NE. 1 ) GO TO 5
      DO 2 J=1,L0
      DO 2 I=1,L0
       T (I,J) = ZERO
       TM(I,J) = ZERO
   2  CONTINUE
      GO TO 6
   5   IF(INI .EQ. 2 .AND. ITW .EQ. 6) GO TO 200
       IF(INI .EQ. 3 .AND. ITW .GT. 4) GO TO 105
   6  CONTINUE
      DO 30 J=1,L0
         DO 20 I=1,L0
            YY = ZERO
       IF (I.LE.NA.AND.J.GT.NA) GO TO 20
       IF (I.GT.NA.AND.J.LE.NA) GO TO 20
C
C     ----- COMPUTE THE CONSTANT DIAGONAL BLOCKS OF UABC -----
C
        DO 10   K =  1,L0
            YY = YY + UA(I,K)*UBC(K,J) - UBCM(K,I)*UA(K,J)
   10   CONTINUE
            YY=YY*PT5
        T(I,J)=T(I,J)+YY
        EA(I,J)=GA(I,J)
   20   CONTINUE
   30   CONTINUE
C
C     ----- COMPUTE THE CONSTANT OFF-DIAGONAL BLOCKS OF UABC -----
C
         DO 60 J = 1,L0
         DO 50 I = 1,L0
            YY=ZERO
            IF(I.LE.NA .AND. J.LE.NA) GO TO 50
            IF(I.GT.NA .AND. J.GT.NA) GO TO 50
         DO 40  K = 1,L0
           YY = YY + GA(I,K)*UBC(K,J) + GBC(I,K)*UA(K,J)
     1             - UA(I,K)*EBC(K,J) - UBC(I,K)*EA(K,J)
   40    CONTINUE
         T(I,J)=T(I,J)+YY
   50    CONTINUE
   60    CONTINUE
         IF (ITW .EQ. 1) GO TO 100
C
C     ----- COMPUTE THE CONSTANT PART OF UABC(0,0,-W) -----
C           OR (0-W,-W), OR (-W,-W,-W)
C
         DO  90 J=1,L0
           DO 80  I=1,L0
            YY = ZERO
           DO  70 K= 1,L0
           YY = YY + UA(I,K)*UBC(K,J) - UBCM(K,I)*UA(K,J)
   70      CONTINUE
           TM(J,I) = TM(J,I) + YY
   80    CONTINUE
   90    CONTINUE
  100    CONTINUE
        RETURN
  105    CONTINUE
C
C     ----- THIS PART IS FOR ITW= 5 AND 6 IN THE INI=3   -----
C     ----- COMPUTE THE CONSTANT DIAGONAL BLOCKS OF UABC -----
C
      DO 130 J=1,L0
        DO 120 I=1,L0
        IF (I.LE.NA.AND.J.GT.NA) GO TO 120
         IF (I.GT.NA.AND.J.LE.NA) GO TO 120
           YY = ZERO
          DO 110   K =  1,L0
          YY = YY - UA(K,I)*UBC(K,J) + UBCM(K,I)*UA(J,K)
  110    CONTINUE
             YY=YY*PT5
         T(I,J)=T(I,J)+YY
         EA(I,J)=GA(I,J)
  120   CONTINUE
  130  CONTINUE
C
C     ----- COMPUTE THE CONSTANT OFF-DIAGONAL BLOCKS OF UABC  -----
C
         DO 160 J = 1,L0
          DO 150 I = 1,L0
            IF(I.LE.NA .AND. J.LE.NA) GO TO 150
            IF(I.GT.NA .AND. J.GT.NA) GO TO 150
            YY=ZERO
           DO 140  K = 1,L0
           YY = YY + GA(K,I)*UBC(K,J) - GBC(I,K)*UA(J,K)
     1             + UA(K,I)*EBC(K,J) - UBC(I,K)*EA(J,K)
  140      CONTINUE
           T(I,J)=T(I,J)+YY
  150     CONTINUE
  160    CONTINUE
C
C     ----- COMPUTE THE CONSTANT PART OF UABC(W,W,-W) OR
C                                        UABC(0,W,-W)
C
         DO 190 J=1,L0
           DO 180  I=1,L0
            YY = ZERO
           DO  170 K= 1,L0
           YY = YY - UA(K,I)*UBC(K,J) + UBCM(K,I)*UA(J,K)
  170      CONTINUE
           TM(J,I) = TM(J,I) + YY
  180    CONTINUE
  190    CONTINUE
        RETURN
  200    CONTINUE
C
C     ----- THIS PART IS ONLY FOR ITW=6 IN THE INI = 2 -----
C          COMPUTE THE CONSTANT DIAGONAL BLOCKS OF UABC
C
      DO 230 J=1,L0
         DO 220 I=1,L0
       IF (I.LE.NA.AND.J.GT.NA) GO TO 220
       IF (I.GT.NA.AND.J.LE.NA) GO TO 220
            YY = ZERO
        DO 210   K =  1,L0
            YY = YY + UA(I,K)*UBCM(K,J) - UBC(K,I)*UA(K,J)
  210   CONTINUE
            YY=YY*PT5
        T(I,J)=T(I,J)+YY
       EA(I,J)=GA(I,J)
  220   CONTINUE
  230   CONTINUE
C
C     ----- COMPUTE THE CONSTANT OFF-DIAGONAL BLOCKS OF UABC -----
C
         DO 260 J = 1,L0
          DO 250 I = 1,L0
            IF(I.LE.NA .AND. J.LE.NA) GO TO 250
            IF(I.GT.NA .AND. J.GT.NA) GO TO 250
            YY=ZERO
            DO 240  K = 1,L0
            YY = YY + GA(I,K)*UBCM(K,J) + GBC(K,I)*UA(K,J)
     1              - UA(I,K)*EBC(J,K) - UBCM(I,K)*EA(K,J)
  240       CONTINUE
           T(I,J)=T(I,J)+YY
  250     CONTINUE
  260    CONTINUE
C
C    ----- COMPUTE THE CONSTANT PART OF UABC(0,0,-W) -----
C
         DO 290 J=1,L0
          DO 280  I=1,L0
            YY = ZERO
            DO  270 K= 1,L0
            YY = YY + UA(I,K)*UBCM(K,J) - UBC(K,I)*UA(K,J)
  270       CONTINUE
           TM(J,I) = TM(J,I) + YY
  280     CONTINUE
  290    CONTINUE
        RETURN
         END
C*MODULE PCMPOL  *DECK F2HPDO
      SUBROUTINE F2HPD0(C0,D0,CA,UBCM,UA,UBC,CBC,L0,NDIM,INI,N1,N23,ITW)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      DIMENSION C0(NDIM,*),CA(NDIM,*),CBC(NDIM,*),D0(NDIM,*),
     *          UA(NDIM,*),UBC(NDIM,*),UBCM(NDIM,*)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
C
      PARAMETER (ZERO=0.0D+00)
C
C     ----- CONSTRUCT PART OF THE 2ND DERIVATIVE DENSITY MATRIX: -----
C
C
      DO 1 J = 1,NUM
        DO 1 I = 1,NUM
         CA(I,J) = ZERO
    1    CBC(I,J) = ZERO
       IF (INI .NE.1) GO TO 5
      DO 2 J=1,NUM
       DO 2 I=1,NUM
         D0(I,J) = ZERO
    2  CONTINUE
        GO TO 6
    5  IF (INI .EQ. 2 .AND. ITW .EQ. 6) GO TO 35
       IF (INI .EQ. 3 .AND. ITW .GT. 4) GO TO 31
C
    6 CONTINUE
      DO 30 J=1,L0
         DO 20 I=1,NUM
           DUM1=ZERO
           DUM2=ZERO
            DO 10 K=1,L0
             DUM1 = DUM1 + C0(I,K)*UA(K,J)
             DUM2 = DUM2 + C0(I,K)*UBCM(K,J)
   10       CONTINUE
            CA(I,J) = DUM1
            CBC(I,J) = DUM2
   20    CONTINUE
   30 CONTINUE
       GO TO 39
   31 CONTINUE
            DO 34 J = 1,L0
             DO 33 I = 1,NUM
               DUM1=ZERO
               DUM2=ZERO
              DO 32 K = 1,L0
              DUM1 = DUM1 - C0(I,K)*UA(J,K)
              DUM2 = DUM2 + C0(I,K)*UBCM(K,J)
   32         CONTINUE
            CA(I,J) = DUM1
            CBC(I,J) = DUM2
   33         CONTINUE
   34         CONTINUE
              GO TO 39
   35       CONTINUE
            DO 38 J = 1,L0
             DO 37 I = 1,NUM
               DUM1=ZERO
               DUM2=ZERO
              DO 36 K = 1,L0
              DUM1 = DUM1 + C0(I,K)*UA(K,J)
              DUM2 = DUM2 + C0(I,K)*UBC(K,J)
   36         CONTINUE
            CA(I,J) = DUM1
            CBC(I,J) = DUM2
   37         CONTINUE
   38         CONTINUE
C
C     ----- CONSTRUCT FIRST HALF OF D0 -----
C
  39   CONTINUE
       DO 60 K = 1,NA
        DO 50 J = 1,NUM
          DO 40 I = 1,NUM
          D0(I,J) = D0(I,J) + CA(I,K) * CBC(J,K)
   40    CONTINUE
   50   CONTINUE
   60  CONTINUE
        IF (N1 .EQ. 0) GO TO 100
C
C     ----- CONSTRUCT CA+(-W) -----
C
         DO 65 J= 1,NUM
          DO 65 I = 1,NUM
          CA(I,J) = ZERO
 65       CONTINUE
        IF (INI .EQ. 3 .AND. ITW .GT. 4) GO TO 91
         DO 90 J = 1,L0
           DO 80 I = 1,NUM
            DUM2 = ZERO
            DO 70 K = 1,L0
               DUM2 = DUM2 - C0(I,K)*UA(J,K)
   70       CONTINUE
         CA(I,J) = DUM2
   80    CONTINUE
   90 CONTINUE
         GO TO 100
   91   CONTINUE
           DO 95 J = 1,L0
            DO 94 I = 1,NUM
              DUM2 = ZERO
              DO 92 K = 1,L0
            DUM2 = DUM2 + C0(I,K) * UA(K,J)
   92       CONTINUE
             CA(I,J) = DUM2
   94       CONTINUE
   95       CONTINUE
  100      IF (N23 .EQ. 0) GO TO 149
C
C     ----- CONSTRUCT CBC -----
C
        DO 105 J = 1,NUM
         DO 105 I = 1,NUM
         CBC(I,J) = ZERO
105   CONTINUE
        IF (INI .EQ. 2 .AND. ITW .EQ. 6) GO TO 141
      DO 130 J = 1,L0
       DO 120 I = 1,NUM
        DUM1 = ZERO
        DO 110 K = 1,L0
           DUM1 = DUM1 + C0(I,K)*UBC(K,J)
 110      CONTINUE
           CBC(I,J) = DUM1
 120     CONTINUE
 130    CONTINUE
         GO TO 149
 141   CONTINUE
      DO 145 J = 1,L0
       DO 144 I = 1,NUM
        DUM1 = ZERO
        DO 142 K = 1,L0
           DUM1 = DUM1 + C0(I,K)*UBCM(K,J)
 142      CONTINUE
           CBC(I,J) = DUM1
 144     CONTINUE
 145    CONTINUE
 149    CONTINUE
C
C      ----- CONSTRUCT REMAINING OF D0 -----
C
       DO 170 K = 1,NA
        DO 160 J=1,NUM
          DO 150 I = 1,NUM
          D0(I,J) = D0(I,J) + CBC(I,K)*CA(J,K)
 150      CONTINUE
 160    CONTINUE
 170   CONTINUE
      RETURN
      END
C*MODULE PCMPOL  *DECK FHPGES
      SUBROUTINE FHPGES(D0,D,U,NUM,NDIM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION D(NDIM,*),D0(NDIM,*),U(NDIM,*)
C
      PARAMETER (ZERO=0.0D+00, TWO=2.0D+00)
C
C     ----- USE LOWER ORDER PART OF D" AS INITIAL GUESS FOR D" -----
C           (MULTIPLIED BY OCCUPATION NUMBER) AND CLEAR U
C
      DO 10 J = 1,NUM
        DO 10 I = 1,NUM
          D(I,J)=D0(I,J)*TWO
          U(I,J)=ZERO
   10 CONTINUE
      RETURN
      END
C*MODULE PCMPOL  *DECK FHPU
      SUBROUTINE FHPU(E,T,U,G,W,L0,NDIM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      DIMENSION E(*),T(NDIM,*),U(NDIM,*),G(NDIM,*)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
C
C     ----- CONSTRUCT THE TRANSFORMATION MATRIX -U"- USED -----
C           TO GET THE 2ND DERIVATIVE WAVEFUNCTION BY
C           PROJECTING OUT THE 'IJ' ELEMENT OF THE
C           2ND DERIVATIVE FOCK MATRIX, ADDING CONTRIBUTION
C           FROM 1ST DERIVATIVE MATRICES, AND WEIGHTING IT
C           BY THE ENERGY DENOMINATOR.
C
      NVIRT = NA + 1
C
C     ----- GET CONSTANT OCC-OCC AND VIRT-VIRT PARTS FROM T -----
C           (OCC-VIRT AND VIRT-OCC WILL BE OVERWRITTEN)
C
      DO 10 J=1,L0
         DO 10 I=1,L0
            U(I,J) = T(I,J)
   10    CONTINUE
C
C     ----- COMPUTE THE ITERATIVE PART  OF UAB
C
       DO 30 J=NVIRT,L0
         DO 20 I=1,NA
            EIJ = E(J)-E(I)-W
            EJI = E(I)-E(J)-W
            U(I,J) = (G(I,J) + T(I,J))/EIJ
            U(J,I) = (G(J,I) + T(J,I))/EJI
   20    CONTINUE
   30  CONTINUE
      RETURN
      END
C*MODULE PCMPOL  *DECK FHPDEN
      SUBROUTINE FHPDEN(C0,TM,D0,D,U,DOLD,DSCR,CAB,
     *                  L0,NDIM,ITER,ADAMP,ITW)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      DIMENSION C0(NDIM,*),U(NDIM,*),TM(NDIM,*),D(NDIM,*),
     *          DSCR(NDIM,*),D0(NDIM,*),DOLD(NDIM,*),CAB(NDIM,*)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
C
      PARAMETER (ZERO=0.0D+00,ONE=1.0D+00,TWO=2.0D+00)
C
C     ----- CONSTRUCT THE 2ND DERIVATIVE DENSITY MATRIX: -----
C           D" = 2*((C"C+) + (CC"+) + (C'C'+) + (C'C'+))
C           LAST 2 TERMS ALREADY IN D0 FROM D2HD0
C
      MBF2=NUM*NUM
      DO 1 J=1,NUM
      DO 1 I= 1,NUM
         CAB(I,J) = ZERO
    1 CONTINUE
C
C     ----- GET THE CONSTANT PART OF D" FROM D0 -----
C
      DO 10 J=1,NUM
       DO 10 I=1,NUM
         D(I,J)=D0(I,J)
   10 CONTINUE
      DO 40 J=1,L0
         DO 30 I=1,NUM
           DUM=ZERO
            DO 20 K=1,L0
              DUM = DUM + C0(I,K)*U(K,J)
   20       CONTINUE
            CAB(I,J) = DUM
   30    CONTINUE
   40 CONTINUE
C
C     ----- COMPUTE HALF OF THE DENSITY MATRIX -----
C
      DO 45 K = 1,NA
       DO 44 J = 1,NUM
        DO 43 I = 1,NUM
         D(I,J) = D(I,J)+CAB(I,K)*C0(J,K)
   43   CONTINUE
   44  CONTINUE
   45 CONTINUE
C
      IF (ITW .EQ. 1 ) GO TO 75
C
C     ----- FORM CAB(0,-W)=C*UAB( 0,-W)  -----
C     ----- OR   CAB(-W,-W)=C*UAB(-W,-W) -----
C
       DO 46 J = 1,NUM
        DO 46 I = 1,NUM
        CAB(I,J) =ZERO
   46  CONTINUE
      DO 70 J = 1,L0
       DO 60 I = 1,NUM
         DUM = ZERO
          DO 50 K = 1,L0
           DUM = DUM + C0(I,K)*(TM(K,J)-U(J,K))
   50     CONTINUE
          CAB(I,J) = DUM
   60  CONTINUE
   70 CONTINUE
   75 CONTINUE
C
C     ----- CONSTRUCT THE REMAINING PART OF THE DENSITY MATRIX -----
C
      DO 100 K = 1,NA
       DO 90 J=1,NUM
        DO 80 I=1,NUM
         D(I,J)=D(I,J) + C0(I,K)*CAB(J,K)
   80   CONTINUE
   90  CONTINUE
  100 CONTINUE
      DO 110 I = 1,NUM
      DO 110 J = 1,NUM
       D(I,J) = D(I,J) * TWO
  110 CONTINUE
C
C     ----- DAMP THE DENSITY MATRIX IF NECESSARY -----
C           AND WRITE IT TO DA FILE AS RECORD -JDPREC(31)-
C
      IF(ADAMP.EQ.ZERO) GO TO 200
      IF(ITER.LE.1) GO TO 200
      BDAMP=ONE - ADAMP
      CALL DENDMP(D,DOLD,DSCR,ADAMP,BDAMP,MBF2)
  200 CONTINUE
      DO KU=1,NUM
      DO JU=1,NUM
        DSCR(KU,JU)=D(KU,JU)
      ENDDO
      ENDDO
C     CALL DAWRIT(IDAF,IODA,D,MBF2,94,0)
      RETURN
      END
C*MODULE PCMPOL  *DECK DENDMP
      SUBROUTINE DENDMP(D,D0,DSCR,ADAMP,BDAMP,MBF2)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION D(*),D0(*),DSCR(*)
C
      DO 10 I=1,MBF2
         D0(I)=DSCR(I)
         D(I)=D(I)*BDAMP+D0(I)*ADAMP
   10 CONTINUE
      RETURN
      END
C*MODULE PCMPOL  *DECK FPLCFC
      SUBROUTINE FPLCFC(C,F,FC,G,NUM,L0,NDIM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION C(NDIM,*),F(NDIM,*),FC(NDIM,*),G(NDIM,*)
C
      PARAMETER (ZERO=0.0D+00)
C
C     ----- TRANSFORM THE AO BASIS DERIVATIVE FOCK MATRIX -F'-  -----
C           INTO THE MO BASIS DERIVATIVE FOCK MATRIX -G'-
C           BY CALCULATING C+F'C
C
      DO 40 J = 1,L0
         DO 30 I=1,NUM
            FC(I,J) = ZERO
             XX     = ZERO
            DO 20 K=1,NUM
              XX = XX + F(I,K)*C(K,J)
   20       CONTINUE
         FC(I,J) = XX
   30    CONTINUE
   40 CONTINUE
      DO 70 J = 1,L0
         DO 60 I=1,L0
             G(I,J) = ZERO
            XX=ZERO
            DO 50 K=1,NUM
              XX = XX + C(K,I)*FC(K,J)
   50       CONTINUE
            G(I,J) = XX
   60    CONTINUE
   70 CONTINUE
      RETURN
      END
C*MODULE PCMPOL  *DECK FPLCVG
      SUBROUTINE FPLCVG(D,H,U,U0,USCR,NUM,NA,L0,NDIM,IEND,ITER)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION D(NDIM,*),H(NDIM,*),U(NDIM,*),U0(NDIM,*),USCR(NDIM,*)
C
      COMMON/HONFPL/VNEW,VOLD,CNVA,CNVB,DAMP(19)
C
      PARAMETER (ZERO=0.0D+00)
C
C     ----- THIS ROUTINE CHECKS FOR CONVERGENCE OF THE CPHF -----
C           CALCULATION.  CONVERGENCE IS DETERMINED BY TWO
C           CRITERIA: THE CONVERGENCE OF THE POLARIZABILITY
C           FOR THE XX, YY, OR ZZ DIRECTION AND THE
C           CONVERGENCE OF THE INDIVIDUAL ELEMENTS OF THE U
C           MATRIX.
C
      IF (ITER .EQ. 0) GO TO 30
C
C     ----- ITER > 0 -----
C
      VOLD  = VNEW
      VNEW  = AVAL(H,D,NUM)
      DIFF  = VOLD - VNEW
      UDELM = ZERO
C
      DO IU=1,NUM
      DO JU=1,NUM
       U0(IU,JU)=USCR(IU,JU)
      ENDDO
      ENDDO
C     CALL DAREAD(IDAF,IODA,U0,NUM2,93,0)
C
      NBR   = NA * (L0 - NA)
      NVIRT = NA + 1
      ICON  = NBR
      DO 20 J=NVIRT,L0
         DO 10 I=1,NA
            UDEL = U(I,J) - U0(I,J)
            IF(ABS(UDEL).LT.CNVB) ICON = ICON - 1
            IF(ABS(UDEL).LE.ABS(UDELM)) UDELM = UDEL
   10    CONTINUE
   20 CONTINUE
       DO KU=1,NUM
       DO JU=1,NUM
         USCR(KU,JU)=U(KU,JU)
       ENDDO
       ENDDO
C
C     CALL DAWRIT(IDAF,IODA,U,NUM2,93,0)
C
      IF(ABS(DIFF).LT.CNVA.AND.ICON.EQ.0) IEND = 1
      RETURN
   30 CONTINUE
C
C     ----- ITER = 0 -----
C
      VOLD = VNEW
      VNEW = AVAL(H,D,NUM)
      DIFF = VOLD - VNEW
      UDELM = ZERO
C
      DO KU=1,NUM
         DO JU=1,NUM
            USCR(KU,JU)=U(KU,JU)
         ENDDO
      ENDDO
C
C     CALL DAWRIT(IDAF,IODA,U,NUM2,93,0)
C
      ICON = NA * (L0 - NA)
C
      RETURN
      END
C*MODULE PCMPOL  *DECK F2HPTN
      SUBROUTINE F2HPTN(D,HP,H,NUM,NDIM,IDC)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION D(NDIM,*),HP(NDIM,*),H(*)
C
      COMMON /DATF2H/ GAMMA(81),IOP
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
C
      NUM2=(NUM*(NUM+1))/2
      IOP=IOP+1
       CALL DAREAD(IDAF,IODA,H,NUM2,94+IDC,0)
       CALL CPYTSQ(H,HP,NUM,1)
       GAMMA(IOP) = AVAL(HP,D,NUM)
      RETURN
      END
C*MODULE PCMPOL  *DECK FHPEPT
      SUBROUTINE FHPEPT(GA,GB,UA,UB,EPA,EPB,EPS,L0,NDIM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      DIMENSION GA(NDIM,*),GB(NDIM,*),UA(NDIM,*),UB(NDIM,*),
     *          EPA(NDIM,*),EPB(NDIM,*),EPS(NDIM,*)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
C
      NV=NA+1
      DO 1 J = 1,L0
       DO 1 I = 1,L0
       EPA(I,J) = 0.0D+00
       EPB(I,J) = 0.0D+00
       EPS(I,J) = 0.0D+00
         IF (I .LE. NA .AND. J .GT. NA ) GO TO 1
         IF (I .GT. NA .AND. J .LE. NA ) GO TO 1
       EPA(I,J) = GA(I,J)
       EPB(I,J) = GB(I,J)
   1   CONTINUE
C
       DO 20 J = 1,NA
        DO 20 I = 1,NA
         YY = 0.0D+00
         DO 10 K = 1,L0
         YY = YY + GA(I,K)*UB(K,J)  +  GB(I,K)*UA(K,J)
     1           - UA(I,K)*EPB(K,J) -  UB(I,K)*EPA(K,J)
  10     CONTINUE
        EPS(I,J) = YY
  20   CONTINUE
       DO 40 J =NV,L0
        DO 40 I =NV,L0
         YY = 0.0D+00
         DO 30 K = 1,L0
         YY = YY + GA(I,K)*UB(K,J)  +  GB(I,K)*UA(K,J)
     1           - UA(I,K)*EPB(K,J) -  UB(I,K)*EPA(K,J)
  30     CONTINUE
         EPS(I,J) = YY
  40   CONTINUE
C
       RETURN
       END
C*MODULE PCMPOL  *DECK FHPEPS
      SUBROUTINE FHPEPS(E,EPS,UAB,GAB,W,L0,NDIM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      DIMENSION E(*),EPS(NDIM,*),UAB(NDIM,*),GAB(NDIM,*)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
C
C     ----- THIS ROUTINE COMPUTES THE ITERATIVE PART -----
C           OF THE LAGRANGIAN MULTIPLIER MATRIX
C
       NV=NA+1
       DO 10 J = 1,NA
        DO 10 I = 1,NA
           EIJ = E(I)-E(J)
           EIJW = EIJ+W
        EPS(I,J) = EPS(I,J) + GAB(I,J) + UAB(I,J)*EIJW
  10   CONTINUE
       DO 20 J =NV,L0
        DO 20 I =NV,L0
           EIJ = E(I)-E(J)
           EIJW = EIJ+W
        EPS(I,J) = EPS(I,J) + GAB(I,J) + UAB(I,J)*EIJW
  20   CONTINUE
       RETURN
       END
C*MODULE PCMPOL  *DECK WD3XMAT
      SUBROUTINE WD3XMAT(D,XX,EL,SCR,DMATM1,BEMCHG,SE,DE,Q,DQSE,
     *                   DMFR,DQFR,DQ,D2QFR1,D2QFR2,D2QFR3,D2Q,
     *                   D3Q,IC1,IC2,IC3,ITW,L1,L2,NTS,NTS2,NFT27)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS)
C
      DIMENSION D(L1,L1),XX(L1,L1),EL(L1,L1),SCR(L2),
     *          DMATM1(NTS,NTS),BEMCHG(L2),Q(NTS),DQSE(NTS),
     *          SE(NTS2,NTS2),DE(NTS2,NTS2),
     *          DMFR(NTS,NTS),DQFR(NTS,3),DQ(NTS,3),
     *          D2QFR1(NTS,3,3),D2QFR2(NTS,3,3),
     *          D2QFR3(NTS,3,3),D2Q(NTS,3,3),D3Q(NTS)
C
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMPAR/ IPCM,NFT26,NFTB,IRPPCM,IEF,IP_F
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTESS
      COMMON /PCMTHF/ DFEFR(3),D2FEFR(3,3,3),DFE(3),D2FE(3,3)
C
      DATA ZERO,ONE,TWO,SIX/0.0D+00,1.0D+00,2.0D+00,6.0D+00/
C
      IF (IC1.GT.3.OR.IC2.GT.3.OR.IC3.GT.3) THEN
         IF(MASWRK) WRITE(IW,*)
     *      'ICOOR1, ICOOR2 AND ICOOR3 ARE EXPECTED TO BE',
     *               ' IN THE RANGE 1 TO 3'
         CALL ABRT
      END IF
C
C     ITW=1  STATIC                               GAMMA (0 ; 0,0,0)
C     ITW=2  OPTICAL KERR EFFECT                  GAMMA (-W ; 0,0,+W)
C     ITW=3  ELECTRIC FIELD INDUCED S H G         GAMMA (-2W ; 0,+W,+W)
C     ITW=4  THIRD HARMONIC GENERATION            GAMMA (-3W ; +W,+W,+W)
C     ITW=5  INTENSITY DEPENDENT REFRACTIVE INDEX GAMMA (-W ; +W,+W,-W)
C     ITW=6  DC ELECTRIC FIELD INDUCED OR         GAMMA (0 ; 0,+W,-W)
C
      CALL SEQREW(NFT27)
      IF(IEF.EQ.0.OR.IEF.GE.3)THEN
C
C  ISOTROPIC SOLVENTS
C           PCM (IEF=0)
C           IEF (IEF>=3)
C
C
      DO ITS = 1, NTS
         CALL SQREAD(NFT27,BEMCHG,L2)
         IF(IEF.EQ.0)THEN
         IPS = 0
         DO I = 1, L1
            DO J = 1, I
               IPS = IPS + 1
               EL(I,J) = BEMCHG(IPS)
               EL(J,I) = BEMCHG(IPS)
            ENDDO
         ENDDO
C
         ELSE
C
         IPS = 0
         DO I = 1, L1
            DO J = 1, I
               IPS = IPS + 1
               EL(I,J) = -BEMCHG(IPS)
               EL(J,I) = -BEMCHG(IPS)
            ENDDO
         ENDDO
C
         ENDIF
C
         Q(ITS)=ZERO
         DO I = 1, L1
            DO JJ = 1, L1
               Q(ITS) = Q(ITS) + D(I,JJ)*EL(I,JJ)
            ENDDO
         ENDDO
      ENDDO
C
      ELSE
C
C   ANISOTROPIC SOLVENTS (IEF=1)
C   IONIC SOLUTIONS (IEF=2)
C
      DO ITS = 1, NTS
         XP=XCTS(ITS)
         YP=YCTS(ITS)
         ZP=ZCTS(ITS)
         CALL INTMEP(SCR,XP,YP,ZP)
         CALL SQREAD(NFT27,BEMCHG,L2)
C
         IPS = 0
         DO I = 1, L1
            DO J = 1, I
               IPS = IPS + 1
               EL(I,J) = SCR(IPS)
               EL(J,I) = SCR(IPS)
               XX(I,J) = BEMCHG(IPS)
               XX(J,I) = BEMCHG(IPS)
            ENDDO
         ENDDO
C
         D3Q(ITS)=ZERO
         DQSE(ITS)=ZERO
         DO II = 1, L1
            DO JJ = 1, L1
               D3Q(ITS) = D3Q(ITS) + D(II,JJ)*EL(II,JJ)
               DQSE(ITS) = DQSE(ITS) + D(II,JJ)*XX(II,JJ)
            ENDDO
         ENDDO
      ENDDO
C
      DO I=1,NTS
       VECGP =ZERO
       DO J=1,NTS
          SEIJ=SE(I,J)
          IF(ITW.NE.1.AND.ITW.NE.6) SEIJ=SE(I,J)*EPS/EPSINF
          DELTAIJ=ZERO
          IF(I.EQ.J)DELTAIJ=AS(I)
          VECGP=VECGP-(DELTAIJ/TWO-DE(I,J))*D3Q(J)
     *               -SEIJ*DQSE(J)
       ENDDO
       Q(I) = VECGP
      ENDDO
C
      ENDIF
C
      CALL SEQREW(NFT27)
C
      DQETOT = ZERO
      DO ITS = 1, NTS
       D3Q(ITS) = ZERO
       DO ITSJ = 1, NTS
       IF(ITW.EQ.1.OR.ITW.EQ.6) THEN
        D3Q(ITS) = D3Q(ITS) + DMATM1(ITS,ITSJ)*Q(ITSJ)
       ELSE
        D3Q(ITS) = D3Q(ITS) + DMFR(ITS,ITSJ)*Q(ITSJ)
       END IF
       ENDDO
        D3Q(ITS) = D3Q(ITS)*AS(ITS)
        DQSE(ITS) = D3Q(ITS)
        DQETOT = DQETOT + D3Q(ITS)
      ENDDO
C
C------------------------------------------------------
C
      IF(ICOMP.NE.0) THEN
C
      NELEC = NE
      QETEOR = NELEC * ( EPS - ONE ) / EPS
C
C     ITW=1  W1=0,  W2=0,  W3=0    ==> Wt=0
C     ITW=2  W1=0,  W2=0,  W3=+W   ==> Wt=W
C     ITW=3  W1=0,  W2=+W, W3=+W   ==> Wt=2W
C     ITW=4  W1=+W, W2=+W, W3=+W   ==> Wt=3W
C     ITW=5  W1=+W, W2=+W, W3=-W   ==> Wt=W
C     ITW=6  W1=0,  W2=+W, W3=-W   ==> Wt=0
C
      IF(ITW.EQ.1) THEN
         FE1=DFE(IC1)
         FE2=DFE(IC2)
         FE3=DFE(IC3)
         FFE12=D2FE(IC1,IC2)
         FFE13=D2FE(IC1,IC3)
         FFE23=D2FE(IC2,IC3)
      ELSE
         FE3=DFEFR(IC3)
C
C     ITW=2  W1=0, W2=0, W3=+W   ==> Wt=W
C
         IF(ITW.EQ.2) THEN
           FE1=DFE(IC1)
           FE2=DFE(IC2)
           FFE12=D2FE(IC1,IC2)
           FFE13=D2FEFR(IC1,IC3,1)
           FFE23=D2FEFR(IC2,IC3,1)
           GO TO 33
         END IF
C
C     ITW=3  W1=0, W2=+W, W3=+W  ==> Wt=2W
C     ITW=6  W1=0, W2=+W, W3=-W  ==> Wt=0
C
        IF(ITW.EQ.3.OR.ITW.EQ.6) THEN
          FE1=DFE(IC1)
          FE2=DFEFR(IC2)
          FFE12=D2FEFR(IC1,IC2,1)
          FFE13=D2FEFR(IC1,IC3,1)
          FFE23=D2FEFR(IC2,IC3,2)
          IF(ITW.EQ.6)FFE23=D2FEFR(IC2,IC3,3)
          GO TO 33
        END IF
C
C     ITW=4  W1=+W, W2=+W, W3=+W ==> Wt=3W
C     ITW=5  W1=+W, W2=+W, W3=-W ==> Wt=W
C
        IF(ITW.EQ.4.OR.ITW.EQ.5) THEN
          FE1=DFEFR(IC1)
          FE2=DFEFR(IC2)
          FFE12=D2FEFR(IC1,IC2,2)
          FFE13=D2FEFR(IC1,IC3,ITW-2)
          FFE23=D2FEFR(IC2,IC3,ITW-2)
        END IF
      END IF
 33   CONTINUE
C
C     Derivata del fattore di compensazione delle cariche
C     d3FE(W1,W2,W3)/dExdEydEz =
C         - {FE(0)**2 *sum(i)[d3QSE_i(W1,W2,W3)/dExdEydEz]}/Qteorica +
C            2*[ dFE(W1)/dEx * d2FE(W2,W3)/dEydEz +
C            dFE(W2)/dEy * d2FE(W1,W3)/dExdEz +
C            dFE(W3)/dEy * d2FE(W1,W2)/dExdEy ]/FE(0) +
C         -  6*[ dFE(W1)/dEx*dFE(W2)/dEy*dFE(W3)/dEz ]/(FE(0)**2)
C     FEi=dFE(Wi)/dEi   i=1,2,3=x,y,z
C     FFEij=d2FE(Wi,Wj)/dEidEj   i,j=1,2,3=x,y,z
C
      F2 = FE * FE
      D3FE = - F2*DQETOT / QETEOR
     *       + TWO*( FE1*FFE23 + FE2*FFE13 + FE3*FFE12 ) / FE
     *       - SIX*FE1*FE2*FE3 / F2
C
C     d3[Q(W1,W2,W3)_norm]/dExdEydEz =
C        d3QSE(W1,W2,W3)/dExdEydEz * FE(0) +
C                           {[d2FE(W1,W2)/dExdEy]*[dQSE(W3)/dEz] +
C                            [d2FE(W1,W3)/dExdEz]*[dQSE(W2)/dEy] +
C                            [d2FE(W2,W3)/dEydEz]*[dQSE(W1)/dEx]} +
C                           {[dFE(W1)/dEx]*[d2QSE(W2,W3)/dEydEz] +
C                            [dFE(W2)/dEy]*[d2QSE(W1,W3)/dExdEz] +
C                            [dFE(W3)/dEz]*[d2QSE(W1,W2)/dExdEy]} +
C                           QSE(0) * d3FE(W1,W2,W3)/dExdEydEz
C     Qi=dQ(Wi)/dEi   i=1,2,3=x,y,z
C     QQij=d2QSE(Wi,Wj)/dEidEj   i,j=1,2,3=x,y,z
C
        QETOTN = ZERO
      DO ITS = 1, NTS
        IF(ITW.EQ.1) THEN
         Q1=DQ(ITS,IC1)
         Q2=DQ(ITS,IC2)
         Q3=DQ(ITS,IC3)
         QQ12= D2Q(ITS,IC1,IC2)
         QQ13= D2Q(ITS,IC1,IC3)
         QQ23= D2Q(ITS,IC2,IC3)
        ELSE
         Q3=DQFR(ITS,IC3)
         IF(ITW.EQ.2.OR.ITW.EQ.3) THEN
C     ITW=2  W1=0, W2=0,  W3=+W  ==> Wt=W
C     ITW=3  W1=0, W2=+W, W3=+W  ==> Wt=2W
           Q1=DQ(ITS,IC1)
           QQ13= D2QFR1(ITS,IC1,IC3)
           IF(ITW.EQ.2) THEN
             Q2=DQ(ITS,IC2)
             QQ12= D2Q(ITS,IC1,IC2)
             QQ23= D2QFR1(ITS,IC2,IC3)
           ELSE
             Q2=DQFR(ITS,IC2)
             QQ12= D2QFR1(ITS,IC1,IC2)
             QQ23= D2QFR2(ITS,IC2,IC3)
           END IF
         END IF
         IF(ITW.EQ.4.OR.ITW.EQ.5) THEN
C
C     ITW=4  W1=+W, W2=+W, W3=+W ==> Wt=3W
C     ITW=5  W1=+W, W2=+W, W3=-W ==> Wt=W
C
           Q1=DQFR(ITS,IC1)
           Q2=DQFR(ITS,IC2)
           QQ12= D2QFR2(ITS,IC1,IC2)
           IF(ITW.EQ.4) THEN
             QQ13= D2QFR2(ITS,IC1,IC3)
             QQ23= D2QFR2(ITS,IC2,IC3)
           ELSE
             QQ13= D2QFR3(ITS,IC1,IC3)
             QQ23= D2QFR3(ITS,IC2,IC3)
           END IF
         END IF
         IF(ITW.EQ.6) THEN
C
C     ITW=6  W1=0, W2=+W, W3=-W  ==> Wt=0
C
           Q1=DQ(ITS,IC1)
           Q2=DQFR(ITS,IC2)
           QQ12= D2QFR1(ITS,IC1,IC2)
           QQ13= D2QFR1(ITS,IC1,IC3)
           QQ23= D2QFR3(ITS,IC2,IC3)
         END IF
        END IF
C
          DQSE(ITS) = D3Q(ITS) * FE +
     *                Q1 * FFE23 + Q2 * FFE13 + Q3 * FFE12 +
     *                QQ12 * FE3 + QQ23 * FE1 + QQ13 * FE2 +
     *                D3FE * QSE(ITS) / FE
C
        QETOTN = QETOTN + DQSE(ITS)
      ENDDO
C
C       WRITE(IW,1000) DQETOT , QETOTN
C  1000 FORMAT('SUM DERIVATIVES BEFORE',F10.6,' AND AFTER ',
C      * 'RENORMALIZATION ',F10.6)
C
      END IF
C
      IF(IEF.GE.3)CALL SEQREW(NFT27)
      NSQR=L1*L1
      IF(IEF.NE.0)CALL VCLR(XX,1,NSQR)
C
C     Calcola X(dD/dEx)
C
      DO ITS = 1, NTS
        IF(IEF.LT.3)THEN
         CALL INTMEP(SCR,XCTS(ITS),YCTS(ITS),ZCTS(ITS))
        ELSE
          CALL SQREAD(NFT27,SCR,L2)
        ENDIF
        IPS = 0
        DO I = 1, L1
          DO J = 1, I
            IPS = IPS + 1
            EL(I,J) = - SCR(IPS)
            EL(J,I) = - SCR(IPS)
          ENDDO
        ENDDO
C
C       Calcola gli elementi della matrice X
C
        DO IBAS = 1, L1
          DO JBAS = 1, L1
            XX(IBAS,JBAS) = XX(IBAS,JBAS) + EL(IBAS,JBAS) * DQSE(ITS)
          ENDDO
        ENDDO
      ENDDO
C
      IF(IEF.GE.3)CALL SEQREW(NFT27)
      RETURN
      END
C*MODULE PCMPOL  *DECK DDISP
      SUBROUTINE DDISP(XD,POT,FLD,DIS2,BK,COL1,L1,L2,NFT27)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXTS=2500, MXTSPT=2*MXTS, MXAO=2047)
C
      DIMENSION XD(L1,L1),POT(L2),DIS2(L1,L1),BK(L1,L1),COL1(L1)
      DIMENSION FLD(L2,3),FLW(225,3)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /PCMPAR/ IPCM,NFT26,NFTB,IRPPCM,IEF,IP_F
      COMMON /PCMDIS/ WB,WA,ETA2,GD,EVAC,IDP
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
      COMMON /ELPROP/ ELDLOC,ELMLOC,ELPLOC,ELFLOC,
     *                IEDEN,IEMOM,IEPOT,IEFLD,MODENS,
     *                IEDOUT,IEMOUT,IEPOUT,IEFOUT,
     *                IEDINT,IEMINT,IEPINT,IEFINT
      COMMON /XYZPRP/ XP,YP,ZP,DMY(35)
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
C
      DATA ZERO, ONE/0.0D+00, 1.0D+00/
      DATA PI/3.141592653589793D+00/
      DATA CORF/0.425D+00/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA ELFLD  /8HELFLD   /
#else
      CHARACTER*8 :: ELFLD_STR
      EQUIVALENCE (ELFLD, ELFLD_STR)
      DATA ELFLD_STR/"ELFLD   "/
#endif
C
      ETA=SQRT(ETA2)
      E1=ETA2-ONE
      E2=ETA*(ETA+WA/WB)
      FACT=E1/(E2*8.0D+00*PI)
C
      L3=L1*L1
C
      CALL VCLR(DIS2,1,L3)
C
C        position the disk file containing electric field integrals
C
      IF(IEF.LT.3)THEN
        CALL SEQREW(NFT27)
      ELSE
        IEFLDOLD=IEFLD
        IEFLD=1
      ENDIF
C
      DO ITS = 1, NTS
C
C            first, a term involving electrostatic potential integrals
C
         CALL INTMEP(POT,XCTS(ITS),YCTS(ITS),ZCTS(ITS))
C
         CALL VCLR(BK,1,L3)
         DO IDR=1,L1
            DO IDT=1,L1
               BB=ZERO
               DO IDS=1,L1
                  IRS=IA(MAX(IDR,IDS)) + MIN(IDR,IDS)
                  BB=BB-POT(IRS)*XD(IDS,IDT)
               ENDDO
               BK(IDR,IDT)=BB
            ENDDO
         ENDDO
C
C        -pot- is now used as a buffer for electric field integrals
C
         IF(IEF.LT.3)THEN
          CALL SQREAD(NFT27,POT,L2)
         ELSE
           ITS2=ITS+NTS
           XP=XCTS(ITS)
           YP=YCTS(ITS)
           ZP=ZCTS(ITS)
           CNX=(XP-XCTS(ITS2))/DR
           CNY=(YP-YCTS(ITS2))/DR
           CNZ=(ZP-ZCTS(ITS2))/DR
           CALL PRCALC(ELFLD,FLD,FLW,3,L2)
           DO IBAS = 1, L2
            POT(IBAS) = FLD(IBAS,1)*CNX
     *                    + FLD(IBAS,2)*CNY
     *                    + FLD(IBAS,3)*CNZ
           ENDDO
         ENDIF
C
         DO IDR=1,L1
            DO IDU=1,L1
               BB=ZERO
               DO IDT=1,L1
                  ITU= IA(MAX(IDT,IDU))+MIN(IDT,IDU)
                  BB=BB + POT(ITU)*BK(IDR,IDT)
               ENDDO
               COL1(IDU)=BB
            ENDDO
            DO IDU=1,L1
               BK(IDR,IDU)=COL1(IDU)
            ENDDO
         ENDDO
C
         WK=0.5D+00*AS(ITS)*CORF*FACT
         DO I=1,L1
            DO J=1,L1
               DIS2(I,J)=DIS2(I,J)+WK*(BK(I,J)+BK(J,I))
            ENDDO
         ENDDO
      ENDDO
C
      IF(IEF.LT.3)THEN
        CALL SEQREW(NFT27)
      ELSE
        IEFLD=IEFLDOLD
      ENDIF
C
      RETURN
      END
C*MODULE PCMPOL  *DECK QEXTF
      SUBROUTINE QEXTF(XX,EL,SCR,DMATM1,Q,DQSE,DMFR,DQQ,DQFR,DQ,
     *                 ICOOR,ITW,L1,NUM2,NTS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
      DIMENSION XX(L1,L1),EL(L1,L1),SCR(NUM2),
     *          DMATM1(NTS,NTS),DMFR(NTS,NTS),
     *          Q(NTS),DQSE(NTS),DQQ(NTS),DQFR(NTS,3),DQ(NTS,3)
C
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMPAR/ IPCM,NFT26,NFTB,IRPPCM,IEF,IP_F
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTESS
C
      DATA ZERO/0.0D+00/
C
      IF (ICOOR.GT.3) THEN
         IF(MASWRK) WRITE(IW,*)
     *      'ICOOR IS EXPECTED TO BE IN THE RANGE 1 TO 3'
         CALL ABRT
      END IF
C
      DO ITS = 1, NTS
       L=ISPHE(ITS)
       XP=XCTS(ITS)
       YP=YCTS(ITS)
       ZP=ZCTS(ITS)
       CX=(XE(L)-XP)/RE(L)
       CY=(YE(L)-YP)/RE(L)
       CZ=(ZE(L)-ZP)/RE(L)
       IF(ICOOR.EQ.1)THEN
        Q(ITS)=CX
       ELSEIF(ICOOR.EQ.2)THEN
        Q(ITS)=CY
       ELSE
        Q(ITS)=CZ
       ENDIF
      ENDDO
C
C
      DQETOT = ZERO
      DO 20 ITS = 1, NTS
       IF(ITW.EQ.0.AND.IEF.EQ.0) THEN
        DQ(ITS,ICOOR) = ZERO
       ELSEIF(ITW.EQ.1.OR.IEF.NE.0)THEN
        DQFR(ITS,ICOOR) = ZERO
       ELSE
        DQQ(ITS) = ZERO
       END IF
C
       DO 10 ITSJ = 1, NTS
        IF(ITW.EQ.0.AND.IEF.EQ.0) THEN
         DQ(ITS,ICOOR) = DQ(ITS,ICOOR)+
     *                   DMATM1(ITS,ITSJ)*Q(ITSJ)
        ELSEIF(ITW.EQ.1.OR.IEF.NE.0) THEN
         DQFR(ITS,ICOOR) = DQFR(ITS,ICOOR)+
     *                     DMFR(ITS,ITSJ)*Q(ITSJ)
        ELSE
         DQQ(ITS) = DQQ(ITS)+DMFR(ITS,ITSJ)*Q(ITSJ)
        END IF
  10   CONTINUE
C
        IF(ITW.EQ.0.AND.IEF.EQ.0) THEN
          DQ(ITS,ICOOR) = DQ(ITS,ICOOR)*AS(ITS)
          DQSE(ITS) = DQ(ITS,ICOOR)
          DQETOT = DQETOT + DQ(ITS,ICOOR)
        ELSEIF(ITW.EQ.1.OR.IEF.NE.0) THEN
          DQFR(ITS,ICOOR) = DQFR(ITS,ICOOR)*AS(ITS)
          DQSE(ITS) = DQFR(ITS,ICOOR)
          DQETOT = DQETOT + DQFR(ITS,ICOOR)
        ELSE
          DQQ(ITS) = DQQ(ITS)*AS(ITS)
          DQSE(ITS) = DQQ(ITS)
          DQETOT = DQETOT + DQQ(ITS)
        END IF
  20   CONTINUE
C
      DO ITS = 1, NTS
       CALL INTMEP(SCR,XCTS(ITS),YCTS(ITS),ZCTS(ITS))
       IPS = 0
       DO I = 1, L1
         DO J = 1, I
           IPS = IPS + 1
           EL(I,J) = SCR(IPS)
           EL(J,I) = SCR(IPS)
         ENDDO
       ENDDO
C
C      Calcola gli elementi della matrice n
C
       DO IBAS = 1, L1
         DO JBAS = 1, L1
           XX(IBAS,JBAS) = XX(IBAS,JBAS) + EL(IBAS,JBAS) * DQSE(ITS)
         ENDDO
       ENDDO
C
      ENDDO
C
C     ----- REACTION FIELD ON THE NUCLEI ------
C
         DO 150 I=1,NAT
            IF(ZAN(I).EQ.0) GO TO 150
            CEXA=ZERO
            CEYA=ZERO
            CEZA=ZERO
            PTZA=ZERO
            DO 140 J=1,NTS
               QS=DQSE(J)
               XU=XCTS(J)-C(1,I)
               YU=YCTS(J)-C(2,I)
               ZU=ZCTS(J)-C(3,I)
               R1=SQRT((XU**2+YU**2+ZU**2)**3)
               R2=SQRT( XU**2+YU**2+ZU**2)
               CEXA=CEXA+QS*XU/R1
               CEYA=CEYA+QS*YU/R1
               CEZA=CEZA+QS*ZU/R1
               PTZA=PTZA+QS/R2
  140       CONTINUE
            IF(MASWRK) WRITE(IW,1600) I,ZAN(I),CEXA,CEYA,CEZA
  150    CONTINUE
 1600 FORMAT(1X,I5,F6.2,4F12.6)
      RETURN
      END
