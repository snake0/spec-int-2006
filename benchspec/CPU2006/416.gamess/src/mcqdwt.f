C 26 MAR 02 - KRG - USE ABRT CALL
C  6 SEP 01 - HU  - USE -SEQREW- INSTEAD OF REWINDS THROUGHOUT
C                 - CHECK MASWRK FOR PARALLEL EXECUTION
C 29 DEC 99 - HN,SV - ADD CODE FOR MCQDPT REFERENCE WEIGHT CALCULATION
C
C*MODULE MCQDWT  *DECK MQWGT1
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++ MQWGT1 +++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MQWGT1(LUNOUT,LPOUT ,LUNFTA,LUNFT1,
     $                  IALPHA,INIACT,LASACT,
     $                  NSTATE,NCSF  ,NOCF  ,MAXSPF,LENGTH,NOCFI ,
     $                  GENZRO,
     $                  NSNSF ,I1EX1 ,I1EX2 ,
     $                  GEN1WK,GEN1  ,
     $                  LAB1R ,WORKR ,LAB1W ,WORKW ,VAL2  ,
     $                  LUNTWO,NMOFZC,NMODOC,NMOACT,NMOEXT,
     $                  NMO   ,NMOEI ,NMOII ,THRGEN,
     $                  HEFF  ,CASVEC,EORB  ,LIJMO ,VONEEL,VTWOEL,
     $                  ECONF ,EREF0 ,CASENE,KREF  ,MOSYM ,
     $                  NCSFNW,LOD2NW,
     $                  ISWTCH,
     $                  EPART0,EPART1,EPART2,OVRLP0,OVRLP1,OVRLP2)
C=======================================================================
C====                                                              =====
C====             THIS ROUTINE WAS CODED BY H.NAKANO               =====
C====                                                              =====
C====     INTELLIGENT MODELING LABORATORY, UNIVERSITY OF TOKYO     =====
C====       2-11-16 YAYOI, BUNKYO-KU, TOKYO 113-8656, JAPAN        =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL   KLOOP
      DIMENSION NSNSF(NOCF+1)
      DIMENSION I1EX1(INIACT:LASACT,0:NOCF)
      DIMENSION I1EX2(INIACT:LASACT,0:NOCFI)
      DIMENSION GEN1WK(INIACT:LASACT,MAXSPF,MAXSPF)
      DIMENSION GEN1(INIACT:LASACT,0:NCSFNW)
      DIMENSION LAB1R(3,LENGTH), LAB1W(2,LENGTH)
      DIMENSION WORKR(LENGTH), WORKW(LENGTH)
      DIMENSION VAL2(NSTATE)
C
      DIMENSION HEFF  (NSTATE,NSTATE,0:3)
      DIMENSION CASVEC(NCSF,NSTATE), EORB(NMO)
      DIMENSION LIJMO (NMO,NMO)
      DIMENSION VONEEL(NMO,NMO) , VTWOEL(NMOEI,NMOII)
      DIMENSION ECONF (NCSF)    , EREF0 (NSTATE)     , CASENE(NSTATE)
      LOGICAL   KREF(NCSF)
      DIMENSION MOSYM(NMO)
      DIMENSION LOD2NW(NCSF)
      DIMENSION EPART0(NSTATE,NSTATE)
      DIMENSION EPART1(INIACT:NMO,NSTATE,NSTATE)
      DIMENSION EPART2(INIACT:NMO,INIACT:NMO,NSTATE,NSTATE,0:1)
      DIMENSION OVRLP0(NSTATE,NSTATE)
      DIMENSION OVRLP1(INIACT:NMO,NSTATE,NSTATE)
      DIMENSION OVRLP2(INIACT:NMO,INIACT:NMO,NSTATE,NSTATE,0:1)
      DIMENSION ISYM(8,8)
      COMMON/MQSYLB/ISYLAB(2,8,4)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DATA ISYM /1,2,3,4,5,6,7,8,
     $           2,1,4,3,6,5,8,7,
     $           3,4,1,2,7,8,5,6,
     $           4,3,2,1,8,7,6,5,
     $           5,6,7,8,1,2,3,4,
     $           6,5,8,7,2,1,4,3,
     $           7,8,5,6,3,4,1,2,
     $           8,7,6,5,4,3,2,1/
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQWGT1 ***'')')
        CALL MQDEBG(LUNOUT,
     $  'LUNFTA','LUNFT1','IALPHA','INIACT','LASACT','NSTATE','  NCSF',
     $   LUNFTA , LUNFT1 , IALPHA,  INIACT , LASACT , NSTATE ,   NCSF )
        CALL MQDEBG(LUNOUT,
     $  '  NOCF','MAXSPF','LENGTH',' NOCFI','LUNTWO','NMOFZC','NMODOC',
     $     NOCF , MAXSPF , LENGTH ,  NOCFI , LUNTWO , NMOFZC , NMODOC )
        CALL MQDEBG(LUNOUT,
     $  'NMOACT','NMOEXT','   NMO',' NMOEI',' NMOII','NCSFNW','     -',
     $   NMOACT , NMOEXT ,    NMO ,  NMOEI ,  NMOII , NCSFNW ,      0 )
      END IF
C==== SET CONSTANTS ====================================================
      INIDOC=NMOFZC+1
      LASDOC=NMOFZC+NMODOC
      INIACT=NMOFZC+NMODOC+1
      LASACT=NMOFZC+NMODOC+NMOACT
      INIEXT=NMOFZC+NMODOC+NMOACT+1
      LASEXT=NMOFZC+NMODOC+NMOACT+NMOEXT
C==== CLEAR HEFF =======================================================
      ISTATE=IALPHA
      IF(ISTATE.EQ.1) THEN
        DO J=1,NSTATE
          DO I=1,NSTATE
            HEFF(I,J,0)=0.0D+00
            HEFF(I,J,1)=0.0D+00
            HEFF(I,J,2)=0.0D+00
            HEFF(I,J,3)=0.0D+00
          END DO
        END DO
      END IF
      IF(ISTATE.EQ.1) THEN
        IF(ISWTCH.EQ.1 .OR. ISWTCH.EQ.3) THEN
          DO L=1,NSTATE
            DO K=1,NSTATE
              EPART0(K,L)=0.0D+00
              DO J=INIACT,LASEXT
                EPART1(J,K,L)=0.0D+00
                DO I=INIACT,LASEXT
                  EPART2(I,J,K,L,0)=0.0D+00
                  EPART2(I,J,K,L,1)=0.0D+00
                END DO
              END DO
            END DO
          END DO
        END IF
        IF(ISWTCH.EQ.2 .OR. ISWTCH.EQ.3) THEN
          DO L=1,NSTATE
            DO K=1,NSTATE
              OVRLP0(K,L)=0.0D+00
              DO J=INIACT,LASEXT
                OVRLP1(J,K,L)=0.0D+00
                DO I=INIACT,LASEXT
                  OVRLP2(I,J,K,L,0)=0.0D+00
                  OVRLP2(I,J,K,L,1)=0.0D+00
                END DO
              END DO
            END DO
          END DO
        END IF
      END IF
C==== READ TWO-ELECTRON INTEGERALS =====================================
      CALL SEQREW(LUNTWO)
      DO I=1,NMOII
        CALL MQMATR(LUNTWO,NMOEI,VTWOEL(1,I))
      END DO
C=======================================================================
C====              =====================================================
C==== ZEROTH ORDER =====================================================
C====              =====================================================
      HEFF(ISTATE,ISTATE,0)=EREF0(ISTATE)
C=======================================================================
C====             ======================================================
C==== FIRST ORDER ======================================================
C====             ======================================================
      HEFF(ISTATE,ISTATE,1)=CASENE(ISTATE)
C==== DEBUG OUTPUT =====================================================
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** HEFF(0-1) ***'')')
        CALL MQWMAG(LUNOUT,HEFF(1,1,1),NSTATE,NSTATE,NSTATE,'SYMM')
      END IF
C=======================================================================
C====              =====================================================
C==== SECOND ORDER =====================================================
C====              =====================================================
C=======================================================================
C==== ZERO-BODY GENERATOR (GET ECORE) ==================================
C====                                 ==================================
C.... CPU TIME .........................................................
      CALL TSECND(STIME)
C
C 0-B <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      DO I=1,NCSF
        IF(KREF(I)) THEN
          KLOOP=.FALSE.
          DO JSTATE=1,NSTATE
            VAL2(JSTATE)=CASVEC(I,ISTATE)*CASVEC(I,JSTATE)
            IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
          END DO
          IF(KLOOP) THEN
            ESI=ECONF(I)-EREF0(ISTATE)
            S=0.0D+00
            DO KAE=INIACT,LASEXT
              DO KI =INIDOC,LASDOC
                S=S-2.0D+00*VONEEL(KI,KAE)**2
     $            /(EORB(KAE)-EORB(KI)+ESI)
              END DO
            END DO
            DO KBF=INIACT,LASACT
              DO KAE=INIACT,LASEXT
                DO KJ =INIDOC,LASDOC
                  DO KI =INIDOC,LASDOC
                    S=S-VTWOEL(LIJMO(KI,KAE),LIJMO(KJ,KBF))
     $                *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(KBF,KJ))
     $                -VTWOEL(LIJMO(KAE,KJ),LIJMO(KBF,KI))
     $                )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)+ESI)
                  END DO
                END DO
              END DO
            END DO
            DO KBF=INIEXT,LASEXT
              DO KAE=INIACT,LASACT
                DO KJ =INIDOC,LASDOC
                  DO KI =INIDOC,LASDOC
                    S=S-VTWOEL(LIJMO(KJ,KBF),LIJMO(KI,KAE))
     $                *(2.0D+00*VTWOEL(LIJMO(KBF,KJ),LIJMO(KAE,KI))
     $                -VTWOEL(LIJMO(KBF,KI),LIJMO(KAE,KJ))
     $                )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)+ESI)
                  END DO
                END DO
              END DO
            END DO
            DO JSTATE=1,NSTATE
              IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
                HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
     $            +S*VAL2(JSTATE)
              END IF
            END DO
          END IF
        END IF
      END DO
C 0-B >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C=======================================================================
C==== ZERO-BODY GENERATOR (GET ECORE) ==================================
C====                                 ==================================
      IF(ISWTCH.EQ.1 .OR. ISWTCH.EQ.3) THEN
        DO JSTATE=1,ISTATE
          DO I=1,NCSF
            IF(KREF(I)) THEN
              CACA=CASVEC(I,ISTATE)*CASVEC(I,JSTATE)
              IF(ABS(CACA).GE.THRGEN) THEN
                ESI=ECONF(I)-EREF0(JSTATE)
                DO KAE=INIACT,LASEXT
                  DO KI =INIDOC,LASDOC
                    EPART1(KAE,ISTATE,JSTATE)=EPART1(KAE,ISTATE,JSTATE)
     $                -2.0D+00*VONEEL(KI,KAE)**2
     $                /(EORB(KAE)-EORB(KI)+ESI)
     $                *CACA
                  END DO
                END DO
                DO KBF=INIACT,LASACT
                  DO KAE=INIACT,LASEXT
                    DO KJ =INIDOC,LASDOC
                      DO KI =INIDOC,LASDOC
                        EPART2(KAE,KBF,ISTATE,JSTATE,0)=
     $                    EPART2(KAE,KBF,ISTATE,JSTATE,0)
     $                    -VTWOEL(LIJMO(KI,KAE),LIJMO(KJ,KBF))
     $                    *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(KBF,KJ))
     $                    -VTWOEL(LIJMO(KAE,KJ),LIJMO(KBF,KI))
     $                    )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)+ESI)
     $                    *CACA
                      END DO
                    END DO
                  END DO
                END DO
                DO KBF=INIEXT,LASEXT
                  DO KAE=INIACT,LASACT
                    DO KJ =INIDOC,LASDOC
                      DO KI =INIDOC,LASDOC
                        EPART2(KAE,KBF,ISTATE,JSTATE,0)=
     $                    EPART2(KAE,KBF,ISTATE,JSTATE,0)
     $                    -VTWOEL(LIJMO(KJ,KBF),LIJMO(KI,KAE))
     $                    *(2.0D+00*VTWOEL(LIJMO(KBF,KJ),LIJMO(KAE,KI))
     $                    -VTWOEL(LIJMO(KBF,KI),LIJMO(KAE,KJ))
     $                    )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)+ESI)
     $                    *CACA
                      END DO
                    END DO
                  END DO
                END DO
              END IF
            END IF
          END DO
        END DO
      END IF
C=======================================================================
C==== ZERO-BODY GENERATOR (GET ECORE) ==================================
C====                                 ==================================
      IF(ISWTCH.EQ.2 .OR. ISWTCH.EQ.3) THEN
        DO JSTATE=1,ISTATE
          DO I=1,NCSF
            IF(KREF(I)) THEN
              CACA=CASVEC(I,ISTATE)*CASVEC(I,JSTATE)
              IF(ABS(CACA).GE.THRGEN) THEN
                ESII=ECONF(I)-EREF0(ISTATE)
                ESIJ=ECONF(I)-EREF0(JSTATE)
                DO KAE=INIACT,LASEXT
                  DO KI =INIDOC,LASDOC
                    OVRLP1(KAE,ISTATE,JSTATE)=OVRLP1(KAE,ISTATE,JSTATE)
     $                -2.0D+00*VONEEL(KI,KAE)**2
     $                /(EORB(KAE)-EORB(KI)+ESII)
     $                /(EORB(KAE)-EORB(KI)+ESIJ)
     $                *CACA
                  END DO
                END DO
                DO KBF=INIACT,LASACT
                  DO KAE=INIACT,LASEXT
                    DO KJ =INIDOC,LASDOC
                      DO KI =INIDOC,LASDOC
                        OVRLP2(KAE,KBF,ISTATE,JSTATE,0)=
     $                    OVRLP2(KAE,KBF,ISTATE,JSTATE,0)
     $                    -VTWOEL(LIJMO(KI,KAE),LIJMO(KJ,KBF))
     $                    *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(KBF,KJ))
     $                    -VTWOEL(LIJMO(KAE,KJ),LIJMO(KBF,KI))
     $                    )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)+ESII)
     $                    /(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)+ESIJ)
     $                    *CACA
                      END DO
                    END DO
                  END DO
                END DO
                DO KBF=INIEXT,LASEXT
                  DO KAE=INIACT,LASACT
                    DO KJ =INIDOC,LASDOC
                      DO KI =INIDOC,LASDOC
                        OVRLP2(KAE,KBF,ISTATE,JSTATE,0)=
     $                    OVRLP2(KAE,KBF,ISTATE,JSTATE,0)
     $                    -VTWOEL(LIJMO(KJ,KBF),LIJMO(KI,KAE))
     $                    *(2.0D+00*VTWOEL(LIJMO(KBF,KJ),LIJMO(KAE,KI))
     $                    -VTWOEL(LIJMO(KBF,KI),LIJMO(KAE,KJ))
     $                    )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)+ESII)
     $                    /(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)+ESIJ)
     $                    *CACA
                      END DO
                    END DO
                  END DO
                END DO
              END IF
            END IF
          END DO
        END DO
      END IF
C==== DEBUG OUTPUT =====================================================
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** HEFF(2) <- ZERO-BODY GENERATOR ***'')')
        CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
      END IF
C.... CPU TIME .........................................................
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      IF(MASWRK) WRITE(LUNOUT,
     $  '('' CPU TIME FOR 0-BODY TERMS ='',F15.3,'' SEC.'')') TIME
C**** MP2 CASE *********************************************************
      IF(NMOACT.EQ.0) RETURN
C***********************************************************************
C****       ************************************************************
C**** START ************************************************************
C****       ************************************************************
C.... CPU TIME .........................................................
      CALL TSECND(STIME)
C**** CLEAR ARRAYS *****************************************************
      DO K=1,MAXSPF
        DO J=1,MAXSPF
          DO I=INIACT,LASACT
            GEN1WK(I,J,K)=0.0D+00
          END DO
        END DO
      END DO
      DO I=INIACT,LASACT
        DO J=0,NCSFNW
          GEN1(I,J)=0.0D+00
        END DO
      END DO
      CALL SEQREW(LUNFT1)
      CALL SEQREW(LUNFTA)
C***********************************************************************
C**** START ************************************************************
C****       ************************************************************
C**** READ COUPLING CONSTANT ON CSF BASE *******************************
  128 CONTINUE
      MAXMU=0
  130 CONTINUE
        READ(LUNFTA) KCONTR,L1,IOCF2,NWORD
        CALL MQGENR(LUNFTA,3*NWORD,NWORD,LAB1R,WORKR)
        DO I=1,NWORD
          L2=LAB1R(1,I)
          MU=LAB1R(2,I)
          IF(MAXMU.LT.MU) MAXMU=MU
          NU=LAB1R(3,I)
          GEN1WK(L2,MU,NU)=WORKR(I)
        END DO
      IF(KCONTR.EQ.100) GO TO 130
      MAXNU=NSNSF(IOCF2+1)-NSNSF(IOCF2)
C**** GET COUPLING CONSTANT <STATE/EIJ/CSF> ****************************
      NSPF2=NSNSF(IOCF2+1)-NSNSF(IOCF2)
      DO L2=INIACT,LASACT
        IOCF1=I1EX2(L1,I1EX1(L2,IOCF2))
        IF(IOCF1.NE.0) THEN
          NSPF1=NSNSF(IOCF1+1)-NSNSF(IOCF1)
          DO MU=1,NSPF1
            ICSF1=NSNSF(IOCF1)+MU
            DO NU=1,NSPF2
              ICSF2=NSNSF(IOCF2)+NU
              ICSF2N=LOD2NW(ICSF2)
              GEN1(L2,ICSF2N)=GEN1(L2,ICSF2N)
     $          +CASVEC(ICSF1,IALPHA)*GEN1WK(L2,MU,NU)
            END DO
          END DO
        END IF
      END DO
C**** IF END OF OCF2, REFRESH GEN1WK AND GO TO 'READ' ******************
      IF(KCONTR.EQ.10) THEN
        DO K=1,MAXNU
          DO J=1,MAXMU
            DO I=INIACT,LASACT
              GEN1WK(I,J,K)=0.0D+00
            END DO
          END DO
        END DO
        GO TO 128
C**** IF END OF ORB1, GET <STATE/EIJ/OCF> AND GO TO 'READ' *************
      ELSE IF(KCONTR.EQ.1) THEN
C=======================================================================
C==== ONE-BODY GENERATOR ===============================================
C====                    ===============================================
        DO II=INIACT,LASACT
          LP=L1
          LQ=II
          ISP=MOSYM(LP)
          ISQ=MOSYM(LQ)
          IF(ISP.EQ.ISQ) THEN
            DO JJ=1,NOCF
              NSPINF=NSNSF(JJ+1)-NSNSF(JJ)
              DO JSTATE=1,NSTATE
                VAL2(JSTATE)=0.0D+00
              END DO
              DO JSTATE=1,NSTATE
                DO KK=1,NSPINF
                  M2=NSNSF(JJ)+KK
                  M2N=LOD2NW(M2)
                  IF(KREF(M2)) THEN
                    VAL2(JSTATE)=VAL2(JSTATE)
     $                +GEN1(II,M2N)*CASVEC(M2,JSTATE)
                  END IF
                END DO
              END DO
              KLOOP=.FALSE.
              DO JSTATE=1,NSTATE
                IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
              END DO
C 1-B <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
              IF(KLOOP) THEN
                ESI2=ECONF(M2)-EREF0(ISTATE)
                EQ=EORB(LQ)
                EP=EORB(LP)
                S=0.0D+00
                DO KI=INIDOC,LASDOC
                  EI2=EORB(KI)-ESI2
                  S=S+VONEEL(KI,LQ)*VONEEL(LP,KI)/(EP-EI2)
                END DO
                DO KE=INIEXT,LASEXT
                  EE2=EORB(KE)+ESI2
                  S=S-VONEEL(LP,KE)*VONEEL(KE,LQ)/(EE2-EQ)
                END DO
                DO KAE=INIACT,LASEXT
                  DO KI =INIDOC,LASDOC
                    EAEI2=EORB(KAE)-EORB(KI)+ESI2
                    S=S-VONEEL(KI,KAE)
     $                *( ( 2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                -      VTWOEL(LIJMO(KAE,LQ),LIJMO(LP,KI)) )
     $                /(EAEI2+EP-EQ)
     $                +( 2.0D+00*VTWOEL(LIJMO(KI,KAE),LIJMO(LP,LQ))
     $                -      VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LQ)) )
     $                / EAEI2
     $                )
                  END DO
                END DO
                DO KI =INIDOC,LASDOC
                  DO KJ =INIDOC,LASDOC
                    DO KAE=INIACT,LASACT
                      EAEIJ2=EORB(KAE)-EORB(KI)-EORB(KJ)+ESI2
                      S=S+VTWOEL(LIJMO(KJ,KAE),LIJMO(KI,LQ))
     $                  *( 2.0D+00*VTWOEL(LIJMO(KAE,KJ),LIJMO(LP,KI))
     $                  -      VTWOEL(LIJMO(KAE,KI),LIJMO(LP,KJ)) )
     $                  /(EAEIJ2+EP)
                    END DO
                  END DO
                END DO
                DO KI =INIDOC,LASDOC
                  DO KJ =INIDOC,LASDOC
                    IS=ISYM(MOSYM(LP),ISYM(MOSYM(KI),MOSYM(KJ)))
                    DO KAE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KAE=INIEXT,LASEXT
                      EAEIJ2=EORB(KAE)-EORB(KI)-EORB(KJ)+ESI2
                      S=S+VTWOEL(LIJMO(KJ,KAE),LIJMO(KI,LQ))
     $                  *( 2.0D+00*VTWOEL(LIJMO(KAE,KJ),LIJMO(LP,KI))
     $                  -      VTWOEL(LIJMO(KAE,KI),LIJMO(LP,KJ)) )
     $                  /(EAEIJ2+EP)
                    END DO
                  END DO
                END DO
                DO KI =INIDOC,LASDOC
                  DO KBF=INIACT,LASACT
                    DO KAE=INIACT,LASACT
                      EAEBF2=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2
                      S=S-VTWOEL(LIJMO(KI,KAE),LIJMO(LP,KBF))
     $                  *( 2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(KBF,LQ))
     $                  -      VTWOEL(LIJMO(KAE,LQ),LIJMO(KBF,KI)) )
     $                  /(EAEBF2-EQ)
                    END DO
                  END DO
                END DO
                DO KI =INIDOC,LASDOC
                  DO KBF=INIACT,LASACT
                    IS=ISYM(MOSYM(LP),ISYM(MOSYM(KI),MOSYM(KBF)))
                    DO KAE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KAE=INIEXT,LASEXT
                      EAEBF2=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2
                      S=S-VTWOEL(LIJMO(KI,KAE),LIJMO(LP,KBF))
     $                  *( 2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(KBF,LQ))
     $                  -      VTWOEL(LIJMO(KAE,LQ),LIJMO(KBF,KI)) )
     $                  /(EAEBF2-EQ)
                    END DO
                  END DO
                END DO
                DO KI =INIDOC,LASDOC
                  DO KAE=INIACT,LASACT
                    IS=ISYM(MOSYM(LP),ISYM(MOSYM(KI),MOSYM(KAE)))
                    DO KBF=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KBF=INIEXT,LASEXT
                      EAEBF2=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2
                      S=S-VTWOEL(LIJMO(LP,KBF),LIJMO(KI,KAE))
     $                  *( 2.0D+00*VTWOEL(LIJMO(KBF,LQ),LIJMO(KAE,KI))
     $                  -      VTWOEL(LIJMO(KBF,KI),LIJMO(KAE,LQ)) )
     $                  /(EAEBF2-EQ)
                    END DO
                  END DO
                END DO
                DO JSTATE=1,NSTATE
                  IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
                    HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
     $                +S*VAL2(JSTATE)
                  END IF
                END DO
              END IF
C 1-B >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            END DO
          END IF
        END DO
C=======================================================================
C==== ONE-BODY GENERATOR ===============================================
C====                    ===============================================
      IF(ISWTCH.EQ.1 .OR. ISWTCH.EQ.3) THEN
        DO II=INIACT,LASACT
          DO JJ=1,NOCF
            NSPINF=NSNSF(JJ+1)-NSNSF(JJ)
            LP=L1
            LQ=II
            EQ=EORB(LQ)
            EP=EORB(LP)
            DO JSTATE=1,ISTATE
              VAL2OD=0.0D+00
              DO KK=1,NSPINF
                M2=NSNSF(JJ)+KK
                M2N=LOD2NW(M2)
                IF(KREF(M2)) THEN
                  VAL2OD=VAL2OD+GEN1(II,M2N)*CASVEC(M2,JSTATE)
                END IF
              END DO
              IF(ABS(VAL2OD).GE.THRGEN) THEN
                ESI2=ECONF(M2)-EREF0(JSTATE)
                S=0.0D+00
                DO KI=INIDOC,LASDOC
                  EI2=EORB(KI)-ESI2
                  EPART0(ISTATE,JSTATE)=EPART0(ISTATE,JSTATE)
     $              +VONEEL(KI,LQ)*VONEEL(LP,KI)
     $              /(EP-EI2)*VAL2OD
                END DO
                DO KE=INIEXT,LASEXT
                  EE2=EORB(KE)+ESI2
                  EPART1(KE,ISTATE,JSTATE)=EPART1(KE,ISTATE,JSTATE)
     $              -VONEEL(LP,KE)*VONEEL(KE,LQ)
     $              /(EE2-EQ)*VAL2OD
                END DO
                DO KAE=INIACT,LASEXT
                  DO KI =INIDOC,LASDOC
                    EAEI2=EORB(KAE)-EORB(KI)+ESI2
                    EPART1(KAE,ISTATE,JSTATE)=EPART1(KAE,ISTATE,JSTATE)
     $                -VONEEL(KI,KAE)
     $                *( ( 2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                -      VTWOEL(LIJMO(KAE,LQ),LIJMO(LP,KI)) )
     $                /(EAEI2+EP-EQ)*VAL2OD
     $                +( 2.0D+00*VTWOEL(LIJMO(KI,KAE),LIJMO(LP,LQ))
     $                -      VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LQ)) )
     $                / EAEI2*VAL2OD
     $                )
                  END DO
                END DO
                DO KAE=INIACT,LASEXT
                  DO KI =INIDOC,LASDOC
                    DO KJ =INIDOC,LASDOC
                      EAEIJ2=EORB(KAE)-EORB(KI)-EORB(KJ)+ESI2
                      EPART1(KAE,ISTATE,JSTATE)=
     $                  EPART1(KAE,ISTATE,JSTATE)
     $                  +VTWOEL(LIJMO(KJ,KAE),LIJMO(KI,LQ))
     $                  *( 2.0D+00*VTWOEL(LIJMO(KAE,KJ),LIJMO(LP,KI))
     $                  -      VTWOEL(LIJMO(KAE,KI),LIJMO(LP,KJ)) )
     $                  /(EAEIJ2+EP)*VAL2OD
                    END DO
                  END DO
                END DO
                DO KBF=INIACT,LASACT
                  DO KAE=INIACT,LASEXT
                    DO KI =INIDOC,LASDOC
                      EAEBF2=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2
                      EPART2(KAE,KBF,ISTATE,JSTATE,0)=
     $                  EPART2(KAE,KBF,ISTATE,JSTATE,0)
     $                  -VTWOEL(LIJMO(KI,KAE),LIJMO(LP,KBF))
     $                  *( 2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(KBF,LQ))
     $                  -      VTWOEL(LIJMO(KAE,LQ),LIJMO(KBF,KI)) )
     $                  /(EAEBF2-EQ)*VAL2OD
                    END DO
                  END DO
                END DO
                DO KBF=INIEXT,LASEXT
                  DO KAE=INIACT,LASACT
                    DO KI =INIDOC,LASDOC
                      EAEBF2=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2
                      EPART2(KAE,KBF,ISTATE,JSTATE,0)=
     $                  EPART2(KAE,KBF,ISTATE,JSTATE,0)
     $                  -VTWOEL(LIJMO(LP,KBF),LIJMO(KI,KAE))
     $                  *( 2.0D+00*VTWOEL(LIJMO(KBF,LQ),LIJMO(KAE,KI))
     $                  -      VTWOEL(LIJMO(KBF,KI),LIJMO(KAE,LQ)) )
     $                  /(EAEBF2-EQ)*VAL2OD
                    END DO
                  END DO
                END DO
              END IF
            END DO
          END DO
        END DO
      END IF
C=======================================================================
C==== ONE-BODY GENERATOR ===============================================
C====                    ===============================================
      IF(ISWTCH.EQ.2 .OR. ISWTCH.EQ.3) THEN
        DO II=INIACT,LASACT
          DO JJ=1,NOCF
            NSPINF=NSNSF(JJ+1)-NSNSF(JJ)
            LP=L1
            LQ=II
            EQ=EORB(LQ)
            EP=EORB(LP)
            DO JSTATE=1,ISTATE
              VAL2OD=0.0D+00
              DO KK=1,NSPINF
                M2=NSNSF(JJ)+KK
                M2N=LOD2NW(M2)
                IF(KREF(M2)) THEN
                  VAL2OD=VAL2OD+GEN1(II,M2N)*CASVEC(M2,JSTATE)
                END IF
              END DO
              IF(ABS(VAL2OD).GE.THRGEN) THEN
                ESI2I=ECONF(M2)-EREF0(ISTATE)
                ESI2J=ECONF(M2)-EREF0(JSTATE)
                S=0.0D+00
                DO KI=INIDOC,LASDOC
                  EI2I=EORB(KI)-ESI2I
                  EI2J=EORB(KI)-ESI2J
                  OVRLP0(ISTATE,JSTATE)=OVRLP0(ISTATE,JSTATE)
     $              +VONEEL(KI,LQ)*VONEEL(LP,KI)
     $              /(EP-EI2I)
     $              /(EP-EI2J)*VAL2OD
                END DO
                DO KE=INIEXT,LASEXT
                  EE2I=EORB(KE)+ESI2I
                  EE2J=EORB(KE)+ESI2J
                  OVRLP1(KE,ISTATE,JSTATE)=OVRLP1(KE,ISTATE,JSTATE)
     $              -VONEEL(LP,KE)*VONEEL(KE,LQ)
     $              /(EE2I-EQ)
     $              /(EE2J-EQ)*VAL2OD
                END DO
                DO KAE=INIACT,LASEXT
                  DO KI =INIDOC,LASDOC
                    EAEI2I=EORB(KAE)-EORB(KI)+ESI2I
                    EAEI2J=EORB(KAE)-EORB(KI)+ESI2J
                    OVRLP1(KAE,ISTATE,JSTATE)=OVRLP1(KAE,ISTATE,JSTATE)
     $                -VONEEL(KI,KAE)
     $                *( ( 2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                -      VTWOEL(LIJMO(KAE,LQ),LIJMO(LP,KI)) )
     $                /(EAEI2I+EP-EQ)
     $                /(EAEI2J+EP-EQ)*VAL2OD
     $                +( 2.0D+00*VTWOEL(LIJMO(KI,KAE),LIJMO(LP,LQ))
     $                -      VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LQ)) )
     $                / EAEI2I
     $                / EAEI2J*VAL2OD
     $                )
                  END DO
                END DO
                DO KAE=INIACT,LASEXT
                  DO KI =INIDOC,LASDOC
                    DO KJ =INIDOC,LASDOC
                      EAEIJ2I=EORB(KAE)-EORB(KI)-EORB(KJ)+ESI2I
                      EAEIJ2J=EORB(KAE)-EORB(KI)-EORB(KJ)+ESI2J
                      OVRLP1(KAE,ISTATE,JSTATE)=
     $                  OVRLP1(KAE,ISTATE,JSTATE)
     $                  +VTWOEL(LIJMO(KJ,KAE),LIJMO(KI,LQ))
     $                  *( 2.0D+00*VTWOEL(LIJMO(KAE,KJ),LIJMO(LP,KI))
     $                  -      VTWOEL(LIJMO(KAE,KI),LIJMO(LP,KJ)) )
     $                  /(EAEIJ2I+EP)
     $                  /(EAEIJ2J+EP)*VAL2OD
                    END DO
                  END DO
                END DO
                DO KBF=INIACT,LASACT
                  DO KAE=INIACT,LASEXT
                    DO KI =INIDOC,LASDOC
                      EAEBF2I=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2I
                      EAEBF2J=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2J
                      OVRLP2(KAE,KBF,ISTATE,JSTATE,0)=
     $                  OVRLP2(KAE,KBF,ISTATE,JSTATE,0)
     $                  -VTWOEL(LIJMO(KI,KAE),LIJMO(LP,KBF))
     $                  *( 2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(KBF,LQ))
     $                  -      VTWOEL(LIJMO(KAE,LQ),LIJMO(KBF,KI)) )
     $                  /(EAEBF2I-EQ)
     $                  /(EAEBF2J-EQ)*VAL2OD
                    END DO
                  END DO
                END DO
                DO KBF=INIEXT,LASEXT
                  DO KAE=INIACT,LASACT
                    DO KI =INIDOC,LASDOC
                      EAEBF2I=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2I
                      EAEBF2J=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2J
                      OVRLP2(KAE,KBF,ISTATE,JSTATE,0)=
     $                  OVRLP2(KAE,KBF,ISTATE,JSTATE,0)
     $                  -VTWOEL(LIJMO(LP,KBF),LIJMO(KI,KAE))
     $                  *( 2.0D+00*VTWOEL(LIJMO(KBF,LQ),LIJMO(KAE,KI))
     $                  -      VTWOEL(LIJMO(KBF,KI),LIJMO(KAE,LQ)) )
     $                  /(EAEBF2I-EQ)
     $                  /(EAEBF2J-EQ)*VAL2OD
                    END DO
                  END DO
                END DO
              END IF
            END DO
          END DO
        END DO
      END IF
C.... WRITE GEN1 .......................................................
        NWRITE=0
        DO I=INIACT,LASACT
          DO J=1,NCSF
            JN=LOD2NW(J)
            IF(JN.NE.0) THEN
              S=ABS(GEN1(I,JN))
              IF(S.GT.GENZRO) THEN
                NWRITE=NWRITE+1
                IF(NWRITE.GT.LENGTH) THEN
                  KCONTW=10
                  WRITE(LUNFT1) KCONTW,L1,LENGTH
                  CALL MQGENW(LUNFT1,2*LENGTH,LENGTH,LAB1W,WORKW)
                  NWRITE=1
                END IF
                LAB1W(1,NWRITE)=I
                LAB1W(2,NWRITE)=J
                WORKW(  NWRITE)=GEN1(I,JN)
              END IF
            END IF
          END DO
        END DO
        KCONTW=1
        WRITE(LUNFT1) KCONTW,L1,NWRITE
        CALL MQGENW(LUNFT1,2*NWRITE,NWRITE,LAB1W,WORKW)
C.... CLEAR ARRAYS .....................................................
        DO K=1,MAXNU
          DO J=1,MAXMU
            DO I=INIACT,LASACT
              GEN1WK(I,J,K)=0.0D+00
            END DO
          END DO
        END DO
        DO I=INIACT,LASACT
          DO J=0,NCSFNW
            GEN1(I,J)=0.0D+00
          END DO
        END DO
        GO TO 128
C**** IF END OF STATE, GET <STATE/EIJ/OCS> *****************************
      ELSE IF(KCONTR.EQ.0) THEN
C=======================================================================
C==== ONE-BODY GENERATOR ===============================================
C====                    ===============================================
        DO II=INIACT,LASACT
          LP=L1
          LQ=II
          ISP=MOSYM(LP)
          ISQ=MOSYM(LQ)
          IF(ISP.EQ.ISQ) THEN
            DO JJ=1,NOCF
              NSPINF=NSNSF(JJ+1)-NSNSF(JJ)
              DO JSTATE=1,NSTATE
                VAL2(JSTATE)=0.0D+00
              END DO
              DO JSTATE=1,NSTATE
                DO KK=1,NSPINF
                  M2=NSNSF(JJ)+KK
                  M2N=LOD2NW(M2)
                  IF(KREF(M2)) THEN
                    VAL2(JSTATE)=VAL2(JSTATE)
     $                +GEN1(II,M2N)*CASVEC(M2,JSTATE)
                  END IF
                END DO
              END DO
              KLOOP=.FALSE.
              DO JSTATE=1,NSTATE
                IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
              END DO
C 1-B <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
              IF(KLOOP) THEN
                ESI2=ECONF(M2)-EREF0(ISTATE)
                EQ=EORB(LQ)
                EP=EORB(LP)
                S=0.0D+00
                DO KI=INIDOC,LASDOC
                  EI2=EORB(KI)-ESI2
                  S=S+VONEEL(KI,LQ)*VONEEL(LP,KI)/(EP-EI2)
                END DO
                DO KE=INIEXT,LASEXT
                  EE2=EORB(KE)+ESI2
                  S=S-VONEEL(LP,KE)*VONEEL(KE,LQ)/(EE2-EQ)
                END DO
                DO KAE=INIACT,LASEXT
                  DO KI =INIDOC,LASDOC
                    EAEI2=EORB(KAE)-EORB(KI)+ESI2
                    S=S-VONEEL(KI,KAE)
     $                *( ( 2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                -      VTWOEL(LIJMO(KAE,LQ),LIJMO(LP,KI)) )
     $                /(EAEI2+EP-EQ)
     $                +( 2.0D+00*VTWOEL(LIJMO(KI,KAE),LIJMO(LP,LQ))
     $                -      VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LQ)) )
     $                / EAEI2
     $                )
                  END DO
                END DO
                DO KI =INIDOC,LASDOC
                  DO KJ =INIDOC,LASDOC
                    DO KAE=INIACT,LASACT
                      EAEIJ2=EORB(KAE)-EORB(KI)-EORB(KJ)+ESI2
                      S=S+VTWOEL(LIJMO(KJ,KAE),LIJMO(KI,LQ))
     $                  *( 2.0D+00*VTWOEL(LIJMO(KAE,KJ),LIJMO(LP,KI))
     $                  -      VTWOEL(LIJMO(KAE,KI),LIJMO(LP,KJ)) )
     $                  /(EAEIJ2+EP)
                    END DO
                  END DO
                END DO
                DO KI =INIDOC,LASDOC
                  DO KJ =INIDOC,LASDOC
                    IS=ISYM(MOSYM(LP),ISYM(MOSYM(KI),MOSYM(KJ)))
                    DO KAE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KAE=INIEXT,LASEXT
                      EAEIJ2=EORB(KAE)-EORB(KI)-EORB(KJ)+ESI2
                      S=S+VTWOEL(LIJMO(KJ,KAE),LIJMO(KI,LQ))
     $                  *( 2.0D+00*VTWOEL(LIJMO(KAE,KJ),LIJMO(LP,KI))
     $                  -      VTWOEL(LIJMO(KAE,KI),LIJMO(LP,KJ)) )
     $                  /(EAEIJ2+EP)
                    END DO
                  END DO
                END DO
                DO KI =INIDOC,LASDOC
                  DO KBF=INIACT,LASACT
                    DO KAE=INIACT,LASACT
                      EAEBF2=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2
                      S=S-VTWOEL(LIJMO(KI,KAE),LIJMO(LP,KBF))
     $                  *( 2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(KBF,LQ))
     $                  -      VTWOEL(LIJMO(KAE,LQ),LIJMO(KBF,KI)) )
     $                  /(EAEBF2-EQ)
                    END DO
                  END DO
                END DO
                DO KI =INIDOC,LASDOC
                  DO KBF=INIACT,LASACT
                    IS=ISYM(MOSYM(LP),ISYM(MOSYM(KI),MOSYM(KBF)))
                    DO KAE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KAE=INIEXT,LASEXT
                      EAEBF2=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2
                      S=S-VTWOEL(LIJMO(KI,KAE),LIJMO(LP,KBF))
     $                  *( 2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(KBF,LQ))
     $                  -      VTWOEL(LIJMO(KAE,LQ),LIJMO(KBF,KI)) )
     $                  /(EAEBF2-EQ)
                    END DO
                  END DO
                END DO
                DO KI =INIDOC,LASDOC
                  DO KAE=INIACT,LASACT
                    IS=ISYM(MOSYM(LP),ISYM(MOSYM(KI),MOSYM(KAE)))
                    DO KBF=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KBF=INIEXT,LASEXT
                      EAEBF2=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2
                      S=S-VTWOEL(LIJMO(LP,KBF),LIJMO(KI,KAE))
     $                  *( 2.0D+00*VTWOEL(LIJMO(KBF,LQ),LIJMO(KAE,KI))
     $                  -      VTWOEL(LIJMO(KBF,KI),LIJMO(KAE,LQ)) )
     $                  /(EAEBF2-EQ)
                    END DO
                  END DO
                END DO
                DO JSTATE=1,NSTATE
                  IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
                    HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
     $                +S*VAL2(JSTATE)
                  END IF
                END DO
              END IF
C 1-B >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            END DO
          END IF
        END DO
C=======================================================================
C==== ONE-BODY GENERATOR ===============================================
C====                    ===============================================
        IF(ISWTCH.EQ.1 .OR. ISWTCH.EQ.3) THEN
          DO II=INIACT,LASACT
            DO JJ=1,NOCF
              NSPINF=NSNSF(JJ+1)-NSNSF(JJ)
              LP=L1
              LQ=II
              EQ=EORB(LQ)
              EP=EORB(LP)
              DO JSTATE=1,ISTATE
                VAL2OD=0.0D+00
                DO KK=1,NSPINF
                  M2=NSNSF(JJ)+KK
                  M2N=LOD2NW(M2)
                  IF(KREF(M2)) VAL2OD=VAL2OD+
     $              GEN1(II,M2N)*CASVEC(M2,JSTATE)
                END DO
                IF(ABS(VAL2OD).GE.THRGEN) THEN
                  ESI2=ECONF(M2)-EREF0(JSTATE)
                  S=0.0D+00
                  DO KI=INIDOC,LASDOC
                    EI2=EORB(KI)-ESI2
                    EPART0(ISTATE,JSTATE)=EPART0(ISTATE,JSTATE)
     $                +VONEEL(KI,LQ)*VONEEL(LP,KI)
     $                /(EP-EI2)*VAL2OD
                  END DO
                  DO KE=INIEXT,LASEXT
                    EE2=EORB(KE)+ESI2
                    EPART1(KE,ISTATE,JSTATE)=EPART1(KE,ISTATE,JSTATE)
     $                -VONEEL(LP,KE)*VONEEL(KE,LQ)
     $                /(EE2-EQ)*VAL2OD
                  END DO
                  DO KAE=INIACT,LASEXT
                    DO KI =INIDOC,LASDOC
                      EAEI2=EORB(KAE)-EORB(KI)+ESI2
                      EPART1(KAE,ISTATE,JSTATE)=
     $                  EPART1(KAE,ISTATE,JSTATE)
     $                  -VONEEL(KI,KAE)
     $                  *( ( 2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                  -      VTWOEL(LIJMO(KAE,LQ),LIJMO(LP,KI)) )
     $                  /(EAEI2+EP-EQ)*VAL2OD
     $                  +( 2.0D+00*VTWOEL(LIJMO(KI,KAE),LIJMO(LP,LQ))
     $                  -      VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LQ)) )
     $                  / EAEI2*VAL2OD
     $                  )
                    END DO
                  END DO
                  DO KAE=INIACT,LASEXT
                    DO KI =INIDOC,LASDOC
                      DO KJ =INIDOC,LASDOC
                        EAEIJ2=EORB(KAE)-EORB(KI)-EORB(KJ)+ESI2
                        EPART1(KAE,ISTATE,JSTATE)=
     $                    EPART1(KAE,ISTATE,JSTATE)
     $                    +VTWOEL(LIJMO(KJ,KAE),LIJMO(KI,LQ))
     $                    *( 2.0D+00*VTWOEL(LIJMO(KAE,KJ),LIJMO(LP,KI))
     $                    -      VTWOEL(LIJMO(KAE,KI),LIJMO(LP,KJ)) )
     $                    /(EAEIJ2+EP)*VAL2OD
                      END DO
                    END DO
                  END DO
                  DO KBF=INIACT,LASACT
                    DO KAE=INIACT,LASEXT
                      DO KI =INIDOC,LASDOC
                        EAEBF2=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2
                        EPART2(KAE,KBF,ISTATE,JSTATE,0)=
     $                    EPART2(KAE,KBF,ISTATE,JSTATE,0)
     $                    -VTWOEL(LIJMO(KI,KAE),LIJMO(LP,KBF))
     $                    *( 2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(KBF,LQ))
     $                    -      VTWOEL(LIJMO(KAE,LQ),LIJMO(KBF,KI)) )
     $                    /(EAEBF2-EQ)*VAL2OD
                      END DO
                    END DO
                  END DO
                  DO KBF=INIEXT,LASEXT
                    DO KAE=INIACT,LASACT
                      DO KI =INIDOC,LASDOC
                        EAEBF2=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2
                        EPART2(KAE,KBF,ISTATE,JSTATE,0)=
     $                    EPART2(KAE,KBF,ISTATE,JSTATE,0)
     $                    -VTWOEL(LIJMO(LP,KBF),LIJMO(KI,KAE))
     $                    *( 2.0D+00*VTWOEL(LIJMO(KBF,LQ),LIJMO(KAE,KI))
     $                    -      VTWOEL(LIJMO(KBF,KI),LIJMO(KAE,LQ)) )
     $                    /(EAEBF2-EQ)*VAL2OD
                      END DO
                    END DO
                  END DO
                END IF
              END DO
            END DO
          END DO
        END IF
C=======================================================================
C==== ONE-BODY GENERATOR ===============================================
C====                    ===============================================
        IF(ISWTCH.EQ.2 .OR. ISWTCH.EQ.3) THEN
          DO II=INIACT,LASACT
            DO JJ=1,NOCF
              NSPINF=NSNSF(JJ+1)-NSNSF(JJ)
              LP=L1
              LQ=II
              EQ=EORB(LQ)
              EP=EORB(LP)
              DO JSTATE=1,ISTATE
                VAL2OD=0.0D+00
                DO KK=1,NSPINF
                  M2=NSNSF(JJ)+KK
                  M2N=LOD2NW(M2)
                  IF(KREF(M2)) VAL2OD=VAL2OD+
     $              GEN1(II,M2N)*CASVEC(M2,JSTATE)
                END DO
                IF(ABS(VAL2OD).GE.THRGEN) THEN
                  ESI2I=ECONF(M2)-EREF0(ISTATE)
                  ESI2J=ECONF(M2)-EREF0(JSTATE)
                  S=0.0D+00
                  DO KI=INIDOC,LASDOC
                    EI2I=EORB(KI)-ESI2I
                    EI2J=EORB(KI)-ESI2J
                    OVRLP0(ISTATE,JSTATE)=OVRLP0(ISTATE,JSTATE)
     $                +VONEEL(KI,LQ)*VONEEL(LP,KI)
     $                /(EP-EI2I)
     $                /(EP-EI2J)*VAL2OD
                  END DO
                  DO KE=INIEXT,LASEXT
                    EE2I=EORB(KE)+ESI2I
                    EE2J=EORB(KE)+ESI2J
                    OVRLP1(KE,ISTATE,JSTATE)=OVRLP1(KE,ISTATE,JSTATE)
     $                -VONEEL(LP,KE)*VONEEL(KE,LQ)
     $                /(EE2I-EQ)
     $                /(EE2J-EQ)*VAL2OD
                  END DO
                  DO KAE=INIACT,LASEXT
                    DO KI =INIDOC,LASDOC
                      EAEI2I=EORB(KAE)-EORB(KI)+ESI2I
                      EAEI2J=EORB(KAE)-EORB(KI)+ESI2J
                      OVRLP1(KAE,ISTATE,JSTATE)=
     $                  OVRLP1(KAE,ISTATE,JSTATE)
     $                  -VONEEL(KI,KAE)
     $                  *( ( 2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                  -      VTWOEL(LIJMO(KAE,LQ),LIJMO(LP,KI)) )
     $                  /(EAEI2I+EP-EQ)
     $                  /(EAEI2J+EP-EQ)*VAL2OD
     $                  +( 2.0D+00*VTWOEL(LIJMO(KI,KAE),LIJMO(LP,LQ))
     $                  -      VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LQ)) )
     $                  / EAEI2I
     $                  / EAEI2J*VAL2OD
     $                  )
                    END DO
                  END DO
                  DO KAE=INIACT,LASEXT
                    DO KI =INIDOC,LASDOC
                      DO KJ =INIDOC,LASDOC
                        EAEIJ2I=EORB(KAE)-EORB(KI)-EORB(KJ)+ESI2I
                        EAEIJ2J=EORB(KAE)-EORB(KI)-EORB(KJ)+ESI2J
                        OVRLP1(KAE,ISTATE,JSTATE)=
     $                    OVRLP1(KAE,ISTATE,JSTATE)
     $                    +VTWOEL(LIJMO(KJ,KAE),LIJMO(KI,LQ))
     $                    *( 2.0D+00*VTWOEL(LIJMO(KAE,KJ),LIJMO(LP,KI))
     $                    -      VTWOEL(LIJMO(KAE,KI),LIJMO(LP,KJ)) )
     $                    /(EAEIJ2I+EP)
     $                    /(EAEIJ2J+EP)*VAL2OD
                      END DO
                    END DO
                  END DO
                  DO KBF=INIACT,LASACT
                    DO KAE=INIACT,LASEXT
                      DO KI =INIDOC,LASDOC
                        EAEBF2I=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2I
                        EAEBF2J=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2J
                        OVRLP2(KAE,KBF,ISTATE,JSTATE,0)=
     $                    OVRLP2(KAE,KBF,ISTATE,JSTATE,0)
     $                    -VTWOEL(LIJMO(KI,KAE),LIJMO(LP,KBF))
     $                    *( 2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(KBF,LQ))
     $                    -      VTWOEL(LIJMO(KAE,LQ),LIJMO(KBF,KI)) )
     $                    /(EAEBF2I-EQ)
     $                    /(EAEBF2J-EQ)*VAL2OD
                      END DO
                    END DO
                  END DO
                  DO KBF=INIEXT,LASEXT
                    DO KAE=INIACT,LASACT
                      DO KI =INIDOC,LASDOC
                        EAEBF2I=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2I
                        EAEBF2J=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2J
                        OVRLP2(KAE,KBF,ISTATE,JSTATE,0)=
     $                    OVRLP2(KAE,KBF,ISTATE,JSTATE,0)
     $                    -VTWOEL(LIJMO(LP,KBF),LIJMO(KI,KAE))
     $                    *( 2.0D+00*VTWOEL(LIJMO(KBF,LQ),LIJMO(KAE,KI))
     $                    -      VTWOEL(LIJMO(KBF,KI),LIJMO(KAE,LQ)) )
     $                    /(EAEBF2I-EQ)
     $                    /(EAEBF2J-EQ)*VAL2OD
                      END DO
                    END DO
                  END DO
                END IF
              END DO
            END DO
          END DO
        END IF
C.... WRITE GEN1 .......................................................
        NWRITE=0
        DO I=INIACT,LASACT
          DO J=1,NCSF
            JN=LOD2NW(J)
            IF(JN.NE.0) THEN
              S=ABS(GEN1(I,JN))
              IF(S.GT.GENZRO) THEN
                NWRITE=NWRITE+1
                IF(NWRITE.GT.LENGTH) THEN
                  KCONTW=10
                  WRITE(LUNFT1) KCONTW,L1,LENGTH
                  CALL MQGENW(LUNFT1,2*LENGTH,LENGTH,LAB1W,WORKW)
                  NWRITE=1
                END IF
                LAB1W(1,NWRITE)=I
                LAB1W(2,NWRITE)=J
                WORKW(  NWRITE)=GEN1(I,JN)
              END IF
            END IF
          END DO
        END DO
        KCONTW=0
        WRITE(LUNFT1) KCONTW,L1,NWRITE
        CALL MQGENW(LUNFT1,2*NWRITE,NWRITE,LAB1W,WORKW)
      ELSE
        IF(MASWRK) WRITE(LUNOUT,*) '*** ERROR IN MQWGT1 ***'
        CALL ABRT
      END IF
C==== DEBUG OUTPUT =====================================================
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** HEFF(2) <- ONE-BODY GENERATOR ***'')')
        CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
      END IF
C.... CPU TIME .........................................................
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      IF(MASWRK) WRITE(LUNOUT,
     $  '('' CPU TIME FOR 1-BODY TERMS ='',F15.3,'' SEC.'')') TIME
C***********************************************************************
C****        ***********************************************************
C**** RETURN ***********************************************************
C****        ***********************************************************
      RETURN
      END
C*MODULE MCQDWT  *DECK MQWGT2
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++ MQWGT2 +++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MQWGT2(LUNOUT,LPOUT ,LUNFTA,LUNFT1,LUNFT2,
     $                  INIACT,LASACT,NCSF  ,NOCF  ,MAXSPF,LENGTH,
     $                  NOCFI ,
     $                  GENZRO,
     $                  NSNSF ,I1EX1 ,I1EX2 ,
     $                  GEN1WK,GEN1  ,GEN2  ,
     $                  LABA2 ,LAB1  ,WORK  ,VAL2  ,
     $                  NMOFZC,NMODOC,NMOACT,NMOEXT,
     $                  ISTATE,NSTATE,NMO   ,NMOEI ,NMOII ,THRGEN,
     $                  HEFF  ,CASVEC,EORB  ,LIJMO ,VONEEL,VTWOEL,
     $                  ECONF ,EREF0 ,KREF  ,MOSYM ,
     $                  NCSFNW,LOD2NW,
     $                  ISWTCH,
     $                  EPART0,EPART1,EPART2,OVRLP0,OVRLP1,OVRLP2)
C=======================================================================
C====                                                              =====
C====             THIS ROUTINE WAS CODED BY H.NAKANO               =====
C====                                                              =====
C====     INTELLIGENT MODELING LABORATORY, UNIVERSITY OF TOKYO     =====
C====       2-11-16 YAYOI, BUNKYO-KU, TOKYO 113-8656, JAPAN        =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL   KLOOP
      DIMENSION NSNSF(NOCF+1)
      DIMENSION I1EX1(INIACT:LASACT,0:NOCF)
      DIMENSION I1EX2(INIACT:LASACT,0:NOCFI)
      DIMENSION GEN1WK(INIACT:LASACT,MAXSPF,MAXSPF)
      DIMENSION GEN1(INIACT:LASACT,0:NCSFNW)
      DIMENSION GEN2(INIACT:LASACT,INIACT:LASACT,0:NCSFNW)
      DIMENSION LABA2(3,LENGTH), LAB1(2,LENGTH)
      DIMENSION WORK(LENGTH)
      DIMENSION VAL2(NSTATE)
C
      DIMENSION HEFF  (NSTATE,NSTATE,0:3)
      DIMENSION CASVEC(NCSF,NSTATE), EORB(NMO)
      DIMENSION LIJMO (NMO,NMO)
      DIMENSION VONEEL(NMO,NMO) , VTWOEL(NMOEI,NMOII)
      DIMENSION ECONF (NCSF)    , EREF0 (NSTATE)
      LOGICAL   KREF(NCSF)
      DIMENSION MOSYM(NMO)
      DIMENSION LOD2NW(NCSF)
      DIMENSION EPART0(NSTATE,NSTATE)
      DIMENSION EPART1(INIACT:NMO,NSTATE,NSTATE)
      DIMENSION EPART2(INIACT:NMO,INIACT:NMO,NSTATE,NSTATE,0:1)
      DIMENSION OVRLP0(NSTATE,NSTATE)
      DIMENSION OVRLP1(INIACT:NMO,NSTATE,NSTATE)
      DIMENSION OVRLP2(INIACT:NMO,INIACT:NMO,NSTATE,NSTATE,0:1)
      DIMENSION ISYM(8,8)
      COMMON/MQSYLB/ISYLAB(2,8,4)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DATA ISYM /1,2,3,4,5,6,7,8,
     $           2,1,4,3,6,5,8,7,
     $           3,4,1,2,7,8,5,6,
     $           4,3,2,1,8,7,6,5,
     $           5,6,7,8,1,2,3,4,
     $           6,5,8,7,2,1,4,3,
     $           7,8,5,6,3,4,1,2,
     $           8,7,6,5,4,3,2,1/
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQWGT2 ***'')')
        CALL MQDEBG(LUNOUT,
     $  'LUNFTA','LUNFT1','LUNFT2','INIACT','LASACT','  NCSF','  NOCF',
     $   LUNFTA , LUNFT1 , LUNFT2 , INIACT , LASACT ,   NCSF ,   NOCF )
        CALL MQDEBG(LUNOUT,
     $  'MAXSPF','LENGTH',' NOCFI','NMOFZC','NMODOC','NMOACT','NMOEXT',
     $   MAXSPF , LENGTH ,  NOCFI , NMOFZC , NMODOC , NMOACT , NMOEXT )
        CALL MQDEBG(LUNOUT,
     $  'ISTATE','NSTATE','   NMO',' NMOEI',' NMOII','NCSFNW','     -',
     $   ISTATE , NSTATE ,    NMO ,  NMOEI ,  NMOII , NCSFNW ,      0 )
      END IF
C**** MP2 CASE *********************************************************
      IF(NMOACT.EQ.0) RETURN
C==== SET CONSTANTS ====================================================
      INIDOC=NMOFZC+1
      LASDOC=NMOFZC+NMODOC
      INIACT=NMOFZC+NMODOC+1
      LASACT=NMOFZC+NMODOC+NMOACT
      INIEXT=NMOFZC+NMODOC+NMOACT+1
      LASEXT=NMOFZC+NMODOC+NMOACT+NMOEXT
C***********************************************************************
C****                 **************************************************
C**** LOOP FOR STATES **************************************************
C****                 **************************************************
      CALL SEQREW(LUNFT2)
      CALL SEQREW(LUNFT1)
C**** CLEAR ARRAYS *****************************************************
  102 CONTINUE
      DO K=1,MAXSPF
        DO J=1,MAXSPF
          DO I=INIACT,LASACT
            GEN1WK(I,J,K)=0.0D+00
          END DO
        END DO
      END DO
      DO I=INIACT,LASACT
        DO J=0,NCSFNW
          GEN1(I,J)=0.0D+00
        END DO
      END DO
      DO J=INIACT,LASACT
        DO I=INIACT,LASACT
          DO K=0,NCSFNW
            GEN2(I,J,K)=0.0D+00
          END DO
        END DO
      END DO
C***********************************************************************
C**** START ************************************************************
C****       ************************************************************
C**** READ COUPLING CONSTANT ON CSF BASE *******************************
  120 CONTINUE
        READ(LUNFT1) KCNTR1,L1,NWORD
        CALL MQGENR(LUNFT1,2*NWORD,NWORD,LAB1,WORK)
        DO I=1,NWORD
          L2=LAB1(1,I)
          ICSF3=LAB1(2,I)
          ICSF3N=LOD2NW(ICSF3)
          GEN1(L2,ICSF3N)=WORK(I)
        END DO
      IF(KCNTR1.EQ.10) GO TO 120
      CALL SEQREW(LUNFTA)
  124 CONTINUE
      MAXNU=0
  126 CONTINUE
        READ(LUNFTA) KCNTRA,L3,IOCF2,NWORD
        CALL MQGENR(LUNFTA,3*NWORD,NWORD,LABA2,WORK)
        DO I=1,NWORD
          L4=LABA2(1,I)
          NU=LABA2(2,I)
          IF(MAXNU.LT.NU) MAXNU=NU
          MU=LABA2(3,I)
          GEN1WK(L4,NU,MU)=WORK(I)
        END DO
      IF(KCNTRA.EQ.100) GO TO 126
      MAXMU=NSNSF(IOCF2+1)-NSNSF(IOCF2)
      NSPF2=MAXMU
      L2=L3
      DO L4=INIACT,LASACT
        IF(I1EX2(L1,I1EX1(L4,IOCF2)).NE.0) THEN
          DO MU=1,NSPF2
            ICSF2=NSNSF(IOCF2)+MU
            ICSF2N=LOD2NW(ICSF2)
            GEN2(L2,L4,ICSF2N)=GEN2(L2,L4,ICSF2N)
     $        -GEN1(L4,ICSF2N)
          END DO
        END IF
      END DO
      DO L4=INIACT,LASACT
        IOCF3=I1EX2(L3,I1EX1(L4,IOCF2))
        IF(IOCF3.NE.0) THEN
          NSPF3=NSNSF(IOCF3+1)-NSNSF(IOCF3)
          DO L2=INIACT,LASACT
            IF(I1EX2(L1,I1EX1(L2,IOCF3)).NE.0) THEN
              DO MU=1,NSPF2
                ICSF2=NSNSF(IOCF2)+MU
                ICSF2N=LOD2NW(ICSF2)
                DO NU=1,NSPF3
                  ICSF3=NSNSF(IOCF3)+NU
                  ICSF3N=LOD2NW(ICSF3)
                  GEN2(L2,L4,ICSF2N)=GEN2(L2,L4,ICSF2N)
     $              +GEN1(L2,ICSF3N)*GEN1WK(L4,NU,MU)
                END DO
              END DO
            END IF
          END DO
        END IF
      END DO
C**** IF END OF OCF2, REFRESH GEN1WK AND GO TO 'READ' ******************
      IF(KCNTRA.EQ.10) THEN
        DO K=1,MAXMU
          DO J=1,MAXNU
            DO I=INIACT,LASACT
              GEN1WK(I,J,K)=0.0D+00
            END DO
          END DO
        END DO
        GO TO 124
C**** IF END OF ORB1, GET <STATE/EIJ/OCF> AND GO TO 'READ' *************
      ELSE IF(KCNTRA.EQ.1) THEN
C=======================================================================
C==== TWO-BODY GENERATOR ===============================================
C====                    ===============================================
        DO II=INIACT,LASACT
          DO JJ=INIACT,LASACT
            LP=L1
            LR=L3
            LQ=II
            LS=JJ
C
            ISPQRS=ISYM(ISYM(MOSYM(LP),MOSYM(LQ)),
     $                  ISYM(MOSYM(LR),MOSYM(LS)))
            IF(ISPQRS.EQ.1) THEN
C
              DO KK=1,NOCF
                NSPINF=NSNSF(KK+1)-NSNSF(KK)
                DO JSTATE=1,NSTATE
                  VAL2(JSTATE)=0.0D+00
                END DO
                DO JSTATE=1,NSTATE
                  DO LL=1,NSPINF
                    M2=NSNSF(KK)+LL
                    M2N=LOD2NW(M2)
                    IF(KREF(M2)) THEN
                      VAL2(JSTATE)=VAL2(JSTATE)
     $                  +GEN2(II,JJ,M2N)*CASVEC(M2,JSTATE)
                    END IF
                  END DO
                  IF(LP.EQ.LR) VAL2(JSTATE)=VAL2(JSTATE)*0.5D+00
                END DO
                KLOOP=.FALSE.
                DO JSTATE=1,NSTATE
                  IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
                END DO
C 2-B <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                IF(KLOOP) THEN
                  ESI2=ECONF(M2)-EREF0(ISTATE)
                  S=0.0D+00
                  EP=EORB(LP)
                  EQ=EORB(LQ)
                  ER=EORB(LR)
                  ES=EORB(LS)
C     PQRS
                  EPR=EP+ER
                  EQS=EQ+ES
                  ERS=ER-ES
                  EQR=EQ-ER
                  EPSR=EP-ES+ER
                  EQRS=EQ-ER+ES
C     RSPQ
                  ESQ=ES+EQ
                  EPQ=EP-EQ
                  ESP=ES-EP
                  ERQP=ER-EQ+EP
                  ESPQ=ES-EP+EQ
                  DO KI=INIDOC,LASDOC
                    EKI2=EORB(KI)-ESI2
                    S=S
     $                +VONEEL(KI,LQ)*VTWOEL(LIJMO(LP,KI),LIJMO(LR,LS))
     $                /(EPSR-EKI2)
     $                +VTWOEL(LIJMO(KI,LQ),LIJMO(LR,LS))*VONEEL(LP,KI)
     $                /(EP-EKI2)
     $                +VONEEL(KI,LS)*VTWOEL(LIJMO(LR,KI),LIJMO(LP,LQ))
     $                /(ERQP-EKI2)
     $                +VTWOEL(LIJMO(KI,LS),LIJMO(LP,LQ))*VONEEL(LR,KI)
     $                /(ER-EKI2)
                  END DO
                  DO KE=INIEXT,LASEXT
                    EKE2=EORB(KE)+ESI2
                    S=S
     $                -VONEEL(LP,KE)*VTWOEL(LIJMO(KE,LQ),LIJMO(LR,LS))
     $                /(EKE2-EQRS)
     $                -VTWOEL(LIJMO(LP,KE),LIJMO(LR,LS))*VONEEL(KE,LQ)
     $                /(EKE2-EQ)
     $                -VONEEL(LR,KE)*VTWOEL(LIJMO(KE,LS),LIJMO(LP,LQ))
     $                /(EKE2-ESPQ)
     $                -VTWOEL(LIJMO(LR,KE),LIJMO(LP,LQ))*VONEEL(KE,LS)
     $                /(EKE2-ES)
                  END DO
                  DO KI=INIDOC,LASDOC
                    DO KJ=INIDOC,LASDOC
                      EKIJ2=EORB(KI)+EORB(KJ)-ESI2
                      S=S- VTWOEL(LIJMO(KI,LQ),LIJMO(KJ,LS))
     $                  *VTWOEL(LIJMO(LP,KI),LIJMO(LR,KJ))
     $                  /(EPR-EKIJ2)
                    END DO
                  END DO
                  DO KA=INIACT,LASACT
                    IS=ISYM(MOSYM(KA),ISYM(MOSYM(LP),MOSYM(LR)))
                    DO KE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KE=INIEXT,LASEXT
                      EKEA2=EORB(KE)+EORB(KA)+ESI2
                      S=S- VTWOEL(LIJMO(LP,KE),LIJMO(LR,KA))
     $                  *VTWOEL(LIJMO(KE,LQ),LIJMO(KA,LS))
     $                  /(EKEA2-EQS)
     $                  - VTWOEL(LIJMO(LR,KE),LIJMO(LP,KA))
     $                  *VTWOEL(LIJMO(KE,LS),LIJMO(KA,LQ))
     $                  /(EKEA2-ESQ)
                    END DO
                  END DO
                  DO KI =INIDOC,LASDOC
                    DO KAE=INIACT,LASACT
                      EKAEI2=EORB(KAE)-EORB(KI)+ESI2
                      S=S
     $                  -( VTWOEL(LIJMO(KI,KAE),LIJMO(LP,LQ))
     $                  *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LR,LS))
     $                  -VTWOEL(LIJMO(KAE,LS),LIJMO(LR,KI)))
     $                  -VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LQ))
     $                  *VTWOEL(LIJMO(KAE,KI),LIJMO(LR,LS))
     $                  )/(EKAEI2+ERS)
     $                  -( VTWOEL(LIJMO(KI,KAE),LIJMO(LR,LS))
     $                  *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                  -VTWOEL(LIJMO(KAE,LQ),LIJMO(LP,KI)))
     $                  -VTWOEL(LIJMO(LR,KAE),LIJMO(KI,LS))
     $                  *VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                  )/(EKAEI2+EPQ)
                     END DO
                  END DO
                  DO KI =INIDOC,LASDOC
                    IS=ISYM(MOSYM(KI),ISYM(MOSYM(LP),MOSYM(LQ)))
                    DO KAE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KAE=INIEXT,LASEXT
                      EKAEI2=EORB(KAE)-EORB(KI)+ESI2
                      S=S
     $                  -( VTWOEL(LIJMO(KI,KAE),LIJMO(LP,LQ))
     $                  *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LR,LS))
     $                  -VTWOEL(LIJMO(KAE,LS),LIJMO(LR,KI)))
     $                  -VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LQ))
     $                  *VTWOEL(LIJMO(KAE,KI),LIJMO(LR,LS))
     $                  )/(EKAEI2+ERS)
     $                  -( VTWOEL(LIJMO(KI,KAE),LIJMO(LR,LS))
     $                  *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                  -VTWOEL(LIJMO(KAE,LQ),LIJMO(LP,KI)))
     $                  -VTWOEL(LIJMO(LR,KAE),LIJMO(KI,LS))
     $                  *VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                  )/(EKAEI2+EPQ)
                    END DO
                  END DO
                  DO KI =INIDOC,LASDOC
                    DO KAE=INIACT,LASACT
                      EKAEI2=EORB(KAE)-EORB(KI)+ESI2
                      S=S
     $                  + VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LS))
     $                  *VTWOEL(LIJMO(KAE,LQ),LIJMO(LR,KI))
     $                  /(EKAEI2-EQR)
     $                  + VTWOEL(LIJMO(LR,KAE),LIJMO(KI,LQ))
     $                  *VTWOEL(LIJMO(KAE,LS),LIJMO(LP,KI))
     $                  /(EKAEI2-ESP)
                    END DO
                  END DO
                  DO KI =INIDOC,LASDOC
                    IS=ISYM(MOSYM(KI),ISYM(MOSYM(LP),MOSYM(LS)))
                    DO KAE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KAE=INIEXT,LASEXT
                      EKAEI2=EORB(KAE)-EORB(KI)+ESI2
                      S=S
     $                  + VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LS))
     $                  *VTWOEL(LIJMO(KAE,LQ),LIJMO(LR,KI))
     $                  /(EKAEI2-EQR)
     $                  + VTWOEL(LIJMO(LR,KAE),LIJMO(KI,LQ))
     $                  *VTWOEL(LIJMO(KAE,LS),LIJMO(LP,KI))
     $                  /(EKAEI2-ESP)
                    END DO
                  END DO
                  DO JSTATE=1,NSTATE
                    IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
                      HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
     $                  +S*VAL2(JSTATE)
                    END IF
                  END DO
                END IF
C 2-B >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
              END DO
            END IF
          END DO
        END DO
C=======================================================================
C==== TWO-BODY GENERATOR ===============================================
C====                    ===============================================
        IF(ISWTCH.EQ.1 .OR. ISWTCH.EQ.3) THEN
          DO II=INIACT,LASACT
            DO JJ=INIACT,LASACT
              DO KK=1,NOCF
                NSPINF=NSNSF(KK+1)-NSNSF(KK)
                LP=L1
                LR=L3
                LQ=II
                LS=JJ
                DO JSTATE=1,ISTATE
                  VAL2OD=0.0D+00
                  DO LL=1,NSPINF
                    M2=NSNSF(KK)+LL
                    M2N=LOD2NW(M2)
                    IF(KREF(M2)) THEN
                      VAL2OD=VAL2OD
     $                  +GEN2(II,JJ,M2N)*CASVEC(M2,JSTATE)
                    END IF
                  END DO
                  IF(LP.EQ.LR) VAL2OD=VAL2OD*0.5D+00
                  IF(ABS(VAL2OD).GE.THRGEN) THEN
                    ESI2=ECONF(M2)-EREF0(JSTATE)
                    S=0.0D+00
                    EP=EORB(LP)
                    EQ=EORB(LQ)
                    ER=EORB(LR)
                    ES=EORB(LS)
C     PQRS
                    EPR=EP+ER
                    EQS=EQ+ES
                    ERS=ER-ES
                    EQR=EQ-ER
                    EPSR=EP-ES+ER
                    EQRS=EQ-ER+ES
C     RSPQ
                    ESQ=ES+EQ
                    EPQ=EP-EQ
                    ESP=ES-EP
                    ERQP=ER-EQ+EP
                    ESPQ=ES-EP+EQ
                    DO KI=INIDOC,LASDOC
                      EKI2=EORB(KI)-ESI2
                      EPART0(ISTATE,JSTATE)=EPART0(ISTATE,JSTATE)
     $                  +VONEEL(KI,LQ)*VTWOEL(LIJMO(LP,KI),LIJMO(LR,LS))
     $                  /(EPSR-EKI2)*VAL2OD
     $                  +VTWOEL(LIJMO(KI,LQ),LIJMO(LR,LS))*VONEEL(LP,KI)
     $                  /(EP-EKI2)*VAL2OD
     $                  +VONEEL(KI,LS)*VTWOEL(LIJMO(LR,KI),LIJMO(LP,LQ))
     $                  /(ERQP-EKI2)*VAL2OD
     $                  +VTWOEL(LIJMO(KI,LS),LIJMO(LP,LQ))*VONEEL(LR,KI)
     $                  /(ER-EKI2)*VAL2OD
                    END DO
                    DO KE=INIEXT,LASEXT
                      EKE2=EORB(KE)+ESI2
                      EPART1(KE,ISTATE,JSTATE)=EPART1(KE,ISTATE,JSTATE)
     $                  -VONEEL(LP,KE)*VTWOEL(LIJMO(KE,LQ),LIJMO(LR,LS))
     $                  /(EKE2-EQRS)*VAL2OD
     $                  -VTWOEL(LIJMO(LP,KE),LIJMO(LR,LS))*VONEEL(KE,LQ)
     $                  /(EKE2-EQ)*VAL2OD
     $                  -VONEEL(LR,KE)*VTWOEL(LIJMO(KE,LS),LIJMO(LP,LQ))
     $                  /(EKE2-ESPQ)*VAL2OD
     $                  -VTWOEL(LIJMO(LR,KE),LIJMO(LP,LQ))*VONEEL(KE,LS)
     $                  /(EKE2-ES)*VAL2OD
                    END DO
                    DO KI=INIDOC,LASDOC
                      DO KJ=INIDOC,LASDOC
                        EKIJ2=EORB(KI)+EORB(KJ)-ESI2
                        EPART0(ISTATE,JSTATE)=EPART0(ISTATE,JSTATE)
     $                    - VTWOEL(LIJMO(KI,LQ),LIJMO(KJ,LS))
     $                    *VTWOEL(LIJMO(LP,KI),LIJMO(LR,KJ))
     $                    /(EPR-EKIJ2)*VAL2OD
                      END DO
                    END DO
                    DO KA=INIACT,LASACT
                      DO KE=INIEXT,LASEXT
                        EKEA2=EORB(KE)+EORB(KA)+ESI2
                        EPART2(KA,KE,ISTATE,JSTATE,0)=
     $                    EPART2(KA,KE,ISTATE,JSTATE,0)
     $                    - VTWOEL(LIJMO(LP,KE),LIJMO(LR,KA))
     $                    *VTWOEL(LIJMO(KE,LQ),LIJMO(KA,LS))
     $                    /(EKEA2-EQS)*VAL2OD
     $                    - VTWOEL(LIJMO(LR,KE),LIJMO(LP,KA))
     $                    *VTWOEL(LIJMO(KE,LS),LIJMO(KA,LQ))
     $                    /(EKEA2-ESQ)*VAL2OD
                      END DO
                    END DO
                    DO KAE=INIACT,LASEXT
                      DO KI =INIDOC,LASDOC
                        EKAEI2=EORB(KAE)-EORB(KI)+ESI2
                        EPART1(KAE,ISTATE,JSTATE)=
     $                    EPART1(KAE,ISTATE,JSTATE)
     $                    -( VTWOEL(LIJMO(KI,KAE),LIJMO(LP,LQ))
     $                    *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LR,LS))
     $                    -VTWOEL(LIJMO(KAE,LS),LIJMO(LR,KI)))
     $                    -VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LQ))
     $                    *VTWOEL(LIJMO(KAE,KI),LIJMO(LR,LS))
     $                    )/(EKAEI2+ERS)*VAL2OD
     $                    + VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LS))
     $                    *VTWOEL(LIJMO(KAE,LQ),LIJMO(LR,KI))
     $                    /(EKAEI2-EQR)*VAL2OD
     $                    -( VTWOEL(LIJMO(KI,KAE),LIJMO(LR,LS))
     $                    *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                    -VTWOEL(LIJMO(KAE,LQ),LIJMO(LP,KI)))
     $                    -VTWOEL(LIJMO(LR,KAE),LIJMO(KI,LS))
     $                    *VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                    )/(EKAEI2+EPQ)*VAL2OD
     $                    + VTWOEL(LIJMO(LR,KAE),LIJMO(KI,LQ))
     $                    *VTWOEL(LIJMO(KAE,LS),LIJMO(LP,KI))
     $                    /(EKAEI2-ESP)*VAL2OD
                      END DO
                    END DO
                  END IF
                END DO
              END DO
            END DO
          END DO
        END IF
C=======================================================================
C==== TWO-BODY GENERATOR ===============================================
C====                    ===============================================
        IF(ISWTCH.EQ.2 .OR. ISWTCH.EQ.3) THEN
          DO II=INIACT,LASACT
            DO JJ=INIACT,LASACT
              DO KK=1,NOCF
                NSPINF=NSNSF(KK+1)-NSNSF(KK)
                LP=L1
                LR=L3
                LQ=II
                LS=JJ
                DO JSTATE=1,ISTATE
                  VAL2OD=0.0D+00
                  DO LL=1,NSPINF
                    M2=NSNSF(KK)+LL
                    M2N=LOD2NW(M2)
                    IF(KREF(M2)) THEN
                      VAL2OD=VAL2OD
     $                  +GEN2(II,JJ,M2N)*CASVEC(M2,JSTATE)
                    END IF
                  END DO
                  IF(LP.EQ.LR) VAL2OD=VAL2OD*0.5D+00
                  IF(ABS(VAL2OD).GE.THRGEN) THEN
                    ESI2I=ECONF(M2)-EREF0(ISTATE)
                    ESI2J=ECONF(M2)-EREF0(JSTATE)
                    S=0.0D+00
                    EP=EORB(LP)
                    EQ=EORB(LQ)
                    ER=EORB(LR)
                    ES=EORB(LS)
C     PQRS
                    EPR=EP+ER
                    EQS=EQ+ES
                    ERS=ER-ES
                    EQR=EQ-ER
                    EPSR=EP-ES+ER
                    EQRS=EQ-ER+ES
C     RSPQ
                    ESQ=ES+EQ
                    EPQ=EP-EQ
                    ESP=ES-EP
                    ERQP=ER-EQ+EP
                    ESPQ=ES-EP+EQ
                    DO KI=INIDOC,LASDOC
                      EKI2I=EORB(KI)-ESI2I
                      EKI2J=EORB(KI)-ESI2J
                      OVRLP0(ISTATE,JSTATE)=OVRLP0(ISTATE,JSTATE)
     $                  +VONEEL(KI,LQ)*VTWOEL(LIJMO(LP,KI),LIJMO(LR,LS))
     $                  /(EPSR-EKI2I)
     $                  /(EPSR-EKI2J)*VAL2OD
     $                  +VTWOEL(LIJMO(KI,LQ),LIJMO(LR,LS))*VONEEL(LP,KI)
     $                  /(EP-EKI2I)
     $                  /(EP-EKI2J)*VAL2OD
     $                  +VONEEL(KI,LS)*VTWOEL(LIJMO(LR,KI),LIJMO(LP,LQ))
     $                  /(ERQP-EKI2I)
     $                  /(ERQP-EKI2J)*VAL2OD
     $                  +VTWOEL(LIJMO(KI,LS),LIJMO(LP,LQ))*VONEEL(LR,KI)
     $                  /(ER-EKI2I)
     $                  /(ER-EKI2J)*VAL2OD
                    END DO
                    DO KE=INIEXT,LASEXT
                      EKE2I=EORB(KE)+ESI2I
                      EKE2J=EORB(KE)+ESI2J
                      OVRLP1(KE,ISTATE,JSTATE)=OVRLP1(KE,ISTATE,JSTATE)
     $                  -VONEEL(LP,KE)*VTWOEL(LIJMO(KE,LQ),LIJMO(LR,LS))
     $                  /(EKE2I-EQRS)
     $                  /(EKE2J-EQRS)*VAL2OD
     $                  -VTWOEL(LIJMO(LP,KE),LIJMO(LR,LS))*VONEEL(KE,LQ)
     $                  /(EKE2I-EQ)
     $                  /(EKE2J-EQ)*VAL2OD
     $                  -VONEEL(LR,KE)*VTWOEL(LIJMO(KE,LS),LIJMO(LP,LQ))
     $                  /(EKE2I-ESPQ)
     $                  /(EKE2J-ESPQ)*VAL2OD
     $                  -VTWOEL(LIJMO(LR,KE),LIJMO(LP,LQ))*VONEEL(KE,LS)
     $                  /(EKE2I-ES)
     $                  /(EKE2J-ES)*VAL2OD
                    END DO
                    DO KI=INIDOC,LASDOC
                      DO KJ=INIDOC,LASDOC
                        EKIJ2I=EORB(KI)+EORB(KJ)-ESI2I
                        EKIJ2J=EORB(KI)+EORB(KJ)-ESI2J
                        OVRLP0(ISTATE,JSTATE)=OVRLP0(ISTATE,JSTATE)
     $                    - VTWOEL(LIJMO(KI,LQ),LIJMO(KJ,LS))
     $                    *VTWOEL(LIJMO(LP,KI),LIJMO(LR,KJ))
     $                    /(EPR-EKIJ2I)
     $                    /(EPR-EKIJ2J)      *VAL2OD
                      END DO
                    END DO
                    DO KA=INIACT,LASACT
                      DO KE=INIEXT,LASEXT
                        EKEA2I=EORB(KE)+EORB(KA)+ESI2I
                        EKEA2J=EORB(KE)+EORB(KA)+ESI2J
                        OVRLP2(KA,KE,ISTATE,JSTATE,0)=
     $                    OVRLP2(KA,KE,ISTATE,JSTATE,0)
     $                    - VTWOEL(LIJMO(LP,KE),LIJMO(LR,KA))
     $                    *VTWOEL(LIJMO(KE,LQ),LIJMO(KA,LS))
     $                    /(EKEA2I-EQS)
     $                    /(EKEA2J-EQS)      *VAL2OD
     $                    - VTWOEL(LIJMO(LR,KE),LIJMO(LP,KA))
     $                    *VTWOEL(LIJMO(KE,LS),LIJMO(KA,LQ))
     $                    /(EKEA2I-ESQ)
     $                    /(EKEA2J-ESQ)      *VAL2OD
                      END DO
                    END DO
                    DO KAE=INIACT,LASEXT
                      DO KI =INIDOC,LASDOC
                        EKAEI2I=EORB(KAE)-EORB(KI)+ESI2I
                        EKAEI2J=EORB(KAE)-EORB(KI)+ESI2J
                        OVRLP1(KAE,ISTATE,JSTATE)=
     $                    OVRLP1(KAE,ISTATE,JSTATE)
     $                    -( VTWOEL(LIJMO(KI,KAE),LIJMO(LP,LQ))
     $                    *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LR,LS))
     $                    -VTWOEL(LIJMO(KAE,LS),LIJMO(LR,KI)))
     $                    -VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LQ))
     $                    *VTWOEL(LIJMO(KAE,KI),LIJMO(LR,LS))
     $                    )/(EKAEI2I+ERS)
     $                    /(EKAEI2J+ERS)*VAL2OD
     $                    + VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LS))
     $                    *VTWOEL(LIJMO(KAE,LQ),LIJMO(LR,KI))
     $                    /(EKAEI2I-EQR)
     $                    /(EKAEI2J-EQR)*VAL2OD
     $                    -( VTWOEL(LIJMO(KI,KAE),LIJMO(LR,LS))
     $                    *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                    -VTWOEL(LIJMO(KAE,LQ),LIJMO(LP,KI)))
     $                    -VTWOEL(LIJMO(LR,KAE),LIJMO(KI,LS))
     $                    *VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                    )/(EKAEI2I+EPQ)
     $                    /(EKAEI2J+EPQ)*VAL2OD
     $                    + VTWOEL(LIJMO(LR,KAE),LIJMO(KI,LQ))
     $                    *VTWOEL(LIJMO(KAE,LS),LIJMO(LP,KI))
     $                    /(EKAEI2I-ESP)
     $                    /(EKAEI2J-ESP)*VAL2OD
                      END DO
                    END DO
                  END IF
                END DO
              END DO
            END DO
          END DO
        END IF
C.... WRITE GEN2 .......................................................
        NWRITE=0
        DO I=INIACT,LASACT
          DO J=INIACT,LASACT
            DO K=1,NCSF
              KN=LOD2NW(K)
              IF(KN.NE.0) THEN
                S=ABS(GEN2(I,J,KN))
                IF(S.GT.GENZRO) THEN
                  NWRITE=NWRITE+1
                  IF(NWRITE.GT.LENGTH) THEN
                    KCONTW=10
                    WRITE(LUNFT2) KCONTW,L1,L3,LENGTH
                    CALL MQGENW(LUNFT2,3*LENGTH,LENGTH,LABA2,WORK)
                    NWRITE=1
                  END IF
                  LABA2(1,NWRITE)=I
                  LABA2(2,NWRITE)=J
                  LABA2(3,NWRITE)=K
                  WORK (  NWRITE)=GEN2(I,J,KN)
                END IF
              END IF
            END DO
          END DO
        END DO
        KCONTW=1
        WRITE(LUNFT2) KCONTW,L1,L3,NWRITE
        CALL MQGENW(LUNFT2,3*NWRITE,NWRITE,LABA2,WORK)
C.... CLEAR ARRAYS .....................................................
        IF(L1.EQ.L3) GO TO 102
        DO K=1,MAXMU
          DO J=1,MAXNU
            DO I=INIACT,LASACT
              GEN1WK(I,J,K)=0.0D+00
            END DO
          END DO
        END DO
        DO I=INIACT,LASACT
          DO J=INIACT,LASACT
            DO K=0,NCSFNW
              GEN2(I,J,K)=0.0D+00
            END DO
          END DO
        END DO
        GO TO 124
C**** IF END OF STATE, GET <STATE/EIJ/OCS> *****************************
      ELSE IF(KCNTRA.EQ.0) THEN
C=======================================================================
C==== TWO-BODY GENERATOR ===============================================
C====                    ===============================================
        DO II=INIACT,LASACT
          DO JJ=INIACT,LASACT
            LP=L1
            LR=L3
            LQ=II
            LS=JJ
C
            ISPQRS=ISYM(ISYM(MOSYM(LP),MOSYM(LQ)),
     $                  ISYM(MOSYM(LR),MOSYM(LS)))
            IF(ISPQRS.EQ.1) THEN
C
              DO KK=1,NOCF
                NSPINF=NSNSF(KK+1)-NSNSF(KK)
                DO JSTATE=1,NSTATE
                  VAL2(JSTATE)=0.0D+00
                END DO
                DO JSTATE=1,NSTATE
                  DO LL=1,NSPINF
                    M2=NSNSF(KK)+LL
                    M2N=LOD2NW(M2)
                    IF(KREF(M2)) THEN
                      VAL2(JSTATE)=VAL2(JSTATE)
     $                  +GEN2(II,JJ,M2N)*CASVEC(M2,JSTATE)
                    END IF
                  END DO
                  IF(LP.EQ.LR) VAL2(JSTATE)=VAL2(JSTATE)*0.5D+00
                END DO
                KLOOP=.FALSE.
                DO JSTATE=1,NSTATE
                  IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
                END DO
C 2-B <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                IF(KLOOP) THEN
                  ESI2=ECONF(M2)-EREF0(ISTATE)
                  S=0.0D+00
                  EP=EORB(LP)
                  EQ=EORB(LQ)
                  ER=EORB(LR)
                  ES=EORB(LS)
C     PQRS
                  EPR=EP+ER
                  EQS=EQ+ES
                  ERS=ER-ES
                  EQR=EQ-ER
                  EPSR=EP-ES+ER
                  EQRS=EQ-ER+ES
C     RSPQ
                  ESQ=ES+EQ
                  EPQ=EP-EQ
                  ESP=ES-EP
                  ERQP=ER-EQ+EP
                  ESPQ=ES-EP+EQ
                  DO KI=INIDOC,LASDOC
                    EKI2=EORB(KI)-ESI2
                    S=S
     $                +VONEEL(KI,LQ)*VTWOEL(LIJMO(LP,KI),LIJMO(LR,LS))
     $                /(EPSR-EKI2)
     $                +VTWOEL(LIJMO(KI,LQ),LIJMO(LR,LS))*VONEEL(LP,KI)
     $                /(EP-EKI2)
     $                +VONEEL(KI,LS)*VTWOEL(LIJMO(LR,KI),LIJMO(LP,LQ))
     $                /(ERQP-EKI2)
     $                +VTWOEL(LIJMO(KI,LS),LIJMO(LP,LQ))*VONEEL(LR,KI)
     $                /(ER-EKI2)
                  END DO
                  DO KE=INIEXT,LASEXT
                    EKE2=EORB(KE)+ESI2
                    S=S
     $                -VONEEL(LP,KE)*VTWOEL(LIJMO(KE,LQ),LIJMO(LR,LS))
     $                /(EKE2-EQRS)
     $                -VTWOEL(LIJMO(LP,KE),LIJMO(LR,LS))*VONEEL(KE,LQ)
     $                /(EKE2-EQ)
     $                -VONEEL(LR,KE)*VTWOEL(LIJMO(KE,LS),LIJMO(LP,LQ))
     $                /(EKE2-ESPQ)
     $                -VTWOEL(LIJMO(LR,KE),LIJMO(LP,LQ))*VONEEL(KE,LS)
     $                /(EKE2-ES)
                  END DO
                  DO KI=INIDOC,LASDOC
                    DO KJ=INIDOC,LASDOC
                      EKIJ2=EORB(KI)+EORB(KJ)-ESI2
                      S=S- VTWOEL(LIJMO(KI,LQ),LIJMO(KJ,LS))
     $                  *VTWOEL(LIJMO(LP,KI),LIJMO(LR,KJ))
     $                  /(EPR-EKIJ2)
                    END DO
                  END DO
                  DO KA=INIACT,LASACT
                    IS=ISYM(MOSYM(KA),ISYM(MOSYM(LP),MOSYM(LR)))
                    DO KE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KE=INIEXT,LASEXT
                      EKEA2=EORB(KE)+EORB(KA)+ESI2
                      S=S- VTWOEL(LIJMO(LP,KE),LIJMO(LR,KA))
     $                  *VTWOEL(LIJMO(KE,LQ),LIJMO(KA,LS))
     $                  /(EKEA2-EQS)
     $                  - VTWOEL(LIJMO(LR,KE),LIJMO(LP,KA))
     $                  *VTWOEL(LIJMO(KE,LS),LIJMO(KA,LQ))
     $                  /(EKEA2-ESQ)
                    END DO
                  END DO
                  DO KI =INIDOC,LASDOC
                    DO KAE=INIACT,LASACT
                      EKAEI2=EORB(KAE)-EORB(KI)+ESI2
                      S=S
     $                  -( VTWOEL(LIJMO(KI,KAE),LIJMO(LP,LQ))
     $                  *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LR,LS))
     $                  -VTWOEL(LIJMO(KAE,LS),LIJMO(LR,KI)))
     $                  -VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LQ))
     $                  *VTWOEL(LIJMO(KAE,KI),LIJMO(LR,LS))
     $                  )/(EKAEI2+ERS)
     $                  -( VTWOEL(LIJMO(KI,KAE),LIJMO(LR,LS))
     $                  *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                  -VTWOEL(LIJMO(KAE,LQ),LIJMO(LP,KI)))
     $                  -VTWOEL(LIJMO(LR,KAE),LIJMO(KI,LS))
     $                  *VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                  )/(EKAEI2+EPQ)
                     END DO
                  END DO
                  DO KI =INIDOC,LASDOC
                    IS=ISYM(MOSYM(KI),ISYM(MOSYM(LP),MOSYM(LQ)))
                    DO KAE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KAE=INIEXT,LASEXT
                      EKAEI2=EORB(KAE)-EORB(KI)+ESI2
                      S=S
     $                  -( VTWOEL(LIJMO(KI,KAE),LIJMO(LP,LQ))
     $                  *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LR,LS))
     $                  -VTWOEL(LIJMO(KAE,LS),LIJMO(LR,KI)))
     $                  -VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LQ))
     $                  *VTWOEL(LIJMO(KAE,KI),LIJMO(LR,LS))
     $                  )/(EKAEI2+ERS)
     $                  -( VTWOEL(LIJMO(KI,KAE),LIJMO(LR,LS))
     $                  *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                  -VTWOEL(LIJMO(KAE,LQ),LIJMO(LP,KI)))
     $                  -VTWOEL(LIJMO(LR,KAE),LIJMO(KI,LS))
     $                  *VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                  )/(EKAEI2+EPQ)
                    END DO
                  END DO
                  DO KI =INIDOC,LASDOC
                    DO KAE=INIACT,LASACT
                      EKAEI2=EORB(KAE)-EORB(KI)+ESI2
                      S=S
     $                  + VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LS))
     $                  *VTWOEL(LIJMO(KAE,LQ),LIJMO(LR,KI))
     $                  /(EKAEI2-EQR)
     $                  + VTWOEL(LIJMO(LR,KAE),LIJMO(KI,LQ))
     $                  *VTWOEL(LIJMO(KAE,LS),LIJMO(LP,KI))
     $                  /(EKAEI2-ESP)
                    END DO
                  END DO
                  DO KI =INIDOC,LASDOC
                    IS=ISYM(MOSYM(KI),ISYM(MOSYM(LP),MOSYM(LS)))
                    DO KAE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                   DO KAE=INIEXT,LASEXT
                      EKAEI2=EORB(KAE)-EORB(KI)+ESI2
                      S=S
     $                  + VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LS))
     $                  *VTWOEL(LIJMO(KAE,LQ),LIJMO(LR,KI))
     $                  /(EKAEI2-EQR)
     $                  + VTWOEL(LIJMO(LR,KAE),LIJMO(KI,LQ))
     $                  *VTWOEL(LIJMO(KAE,LS),LIJMO(LP,KI))
     $                  /(EKAEI2-ESP)
                    END DO
                  END DO
                  DO JSTATE=1,NSTATE
                    IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
                      HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
     $                  +S*VAL2(JSTATE)
                    END IF
                  END DO
                END IF
C 2-B >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
              END DO
            END IF
          END DO
        END DO
C=======================================================================
C==== TWO-BODY GENERATOR ===============================================
C====                    ===============================================
        IF(ISWTCH.EQ.1 .OR. ISWTCH.EQ.3) THEN
          DO II=INIACT,LASACT
            DO JJ=INIACT,LASACT
              DO KK=1,NOCF
                NSPINF=NSNSF(KK+1)-NSNSF(KK)
                LP=L1
                LR=L3
                LQ=II
                LS=JJ
                DO JSTATE=1,ISTATE
                  VAL2OD=0.0D+00
                  DO LL=1,NSPINF
                    M2=NSNSF(KK)+LL
                    M2N=LOD2NW(M2)
                    IF(KREF(M2)) THEN
                      VAL2OD=VAL2OD
     $                  +GEN2(II,JJ,M2N)*CASVEC(M2,JSTATE)
                    END IF
                  END DO
                  IF(LP.EQ.LR) VAL2OD=VAL2OD*0.5D+00
                  IF(ABS(VAL2OD).GE.THRGEN) THEN
                    ESI2=ECONF(M2)-EREF0(JSTATE)
                    S=0.0D+00
                    EP=EORB(LP)
                    EQ=EORB(LQ)
                    ER=EORB(LR)
                    ES=EORB(LS)
C     PQRS
                    EPR=EP+ER
                    EQS=EQ+ES
                    ERS=ER-ES
                    EQR=EQ-ER
                    EPSR=EP-ES+ER
                    EQRS=EQ-ER+ES
C     RSPQ
                    ESQ=ES+EQ
                    EPQ=EP-EQ
                    ESP=ES-EP
                    ERQP=ER-EQ+EP
                    ESPQ=ES-EP+EQ
                    DO KI=INIDOC,LASDOC
                      EKI2=EORB(KI)-ESI2
                      EPART0(ISTATE,JSTATE)=EPART0(ISTATE,JSTATE)
     $                  +VONEEL(KI,LQ)*VTWOEL(LIJMO(LP,KI),LIJMO(LR,LS))
     $                  /(EPSR-EKI2)*VAL2OD
     $                  +VTWOEL(LIJMO(KI,LQ),LIJMO(LR,LS))*VONEEL(LP,KI)
     $                  /(EP-EKI2)*VAL2OD
     $                  +VONEEL(KI,LS)*VTWOEL(LIJMO(LR,KI),LIJMO(LP,LQ))
     $                  /(ERQP-EKI2)*VAL2OD
     $                  +VTWOEL(LIJMO(KI,LS),LIJMO(LP,LQ))*VONEEL(LR,KI)
     $                  /(ER-EKI2)*VAL2OD
                    END DO
                    DO KE=INIEXT,LASEXT
                      EKE2=EORB(KE)+ESI2
                      EPART1(KE,ISTATE,JSTATE)=EPART1(KE,ISTATE,JSTATE)
     $                  -VONEEL(LP,KE)*VTWOEL(LIJMO(KE,LQ),LIJMO(LR,LS))
     $                  /(EKE2-EQRS)*VAL2OD
     $                  -VTWOEL(LIJMO(LP,KE),LIJMO(LR,LS))*VONEEL(KE,LQ)
     $                  /(EKE2-EQ)*VAL2OD
     $                  -VONEEL(LR,KE)*VTWOEL(LIJMO(KE,LS),LIJMO(LP,LQ))
     $                  /(EKE2-ESPQ)*VAL2OD
     $                  -VTWOEL(LIJMO(LR,KE),LIJMO(LP,LQ))*VONEEL(KE,LS)
     $                  /(EKE2-ES)*VAL2OD
                    END DO
                    DO KI=INIDOC,LASDOC
                      DO KJ=INIDOC,LASDOC
                        EKIJ2=EORB(KI)+EORB(KJ)-ESI2
                        EPART0(ISTATE,JSTATE)=EPART0(ISTATE,JSTATE)
     $                    - VTWOEL(LIJMO(KI,LQ),LIJMO(KJ,LS))
     $                    *VTWOEL(LIJMO(LP,KI),LIJMO(LR,KJ))
     $                    /(EPR-EKIJ2)*VAL2OD
                      END DO
                    END DO
                    DO KA=INIACT,LASACT
                      DO KE=INIEXT,LASEXT
                        EKEA2=EORB(KE)+EORB(KA)+ESI2
                        EPART2(KA,KE,ISTATE,JSTATE,0)=
     $                    EPART2(KA,KE,ISTATE,JSTATE,0)
     $                    - VTWOEL(LIJMO(LP,KE),LIJMO(LR,KA))
     $                    *VTWOEL(LIJMO(KE,LQ),LIJMO(KA,LS))
     $                    /(EKEA2-EQS)*VAL2OD
     $                    - VTWOEL(LIJMO(LR,KE),LIJMO(LP,KA))
     $                    *VTWOEL(LIJMO(KE,LS),LIJMO(KA,LQ))
     $                    /(EKEA2-ESQ)*VAL2OD
                      END DO
                    END DO
                    DO KAE=INIACT,LASEXT
                      DO KI =INIDOC,LASDOC
                        EKAEI2=EORB(KAE)-EORB(KI)+ESI2
                        EPART1(KAE,ISTATE,JSTATE)=
     $                    EPART1(KAE,ISTATE,JSTATE)
     $                    -( VTWOEL(LIJMO(KI,KAE),LIJMO(LP,LQ))
     $                    *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LR,LS))
     $                    -VTWOEL(LIJMO(KAE,LS),LIJMO(LR,KI)))
     $                    -VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LQ))
     $                    *VTWOEL(LIJMO(KAE,KI),LIJMO(LR,LS))
     $                    )/(EKAEI2+ERS)*VAL2OD
     $                    + VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LS))
     $                    *VTWOEL(LIJMO(KAE,LQ),LIJMO(LR,KI))
     $                    /(EKAEI2-EQR)*VAL2OD
     $                    -( VTWOEL(LIJMO(KI,KAE),LIJMO(LR,LS))
     $                    *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                    -VTWOEL(LIJMO(KAE,LQ),LIJMO(LP,KI)))
     $                    -VTWOEL(LIJMO(LR,KAE),LIJMO(KI,LS))
     $                    *VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                    )/(EKAEI2+EPQ)*VAL2OD
     $                    + VTWOEL(LIJMO(LR,KAE),LIJMO(KI,LQ))
     $                    *VTWOEL(LIJMO(KAE,LS),LIJMO(LP,KI))
     $                    /(EKAEI2-ESP)*VAL2OD
                      END DO
                    END DO
                  END IF
                END DO
              END DO
            END DO
          END DO
        END IF
C=======================================================================
C==== TWO-BODY GENERATOR ===============================================
C====                    ===============================================
        IF(ISWTCH.EQ.2 .OR. ISWTCH.EQ.3) THEN
          DO II=INIACT,LASACT
            DO JJ=INIACT,LASACT
              DO KK=1,NOCF
                NSPINF=NSNSF(KK+1)-NSNSF(KK)
                LP=L1
                LR=L3
                LQ=II
                LS=JJ
                DO JSTATE=1,ISTATE
                  VAL2OD=0.0D+00
                  DO LL=1,NSPINF
                    M2=NSNSF(KK)+LL
                    M2N=LOD2NW(M2)
                    IF(KREF(M2)) THEN
                      VAL2OD=VAL2OD
     $                  +GEN2(II,JJ,M2N)*CASVEC(M2,JSTATE)
                    END IF
                  END DO
                  IF(LP.EQ.LR) VAL2OD=VAL2OD*0.5D+00
                  IF(ABS(VAL2OD).GE.THRGEN) THEN
                    ESI2I=ECONF(M2)-EREF0(ISTATE)
                    ESI2J=ECONF(M2)-EREF0(JSTATE)
                    S=0.0D+00
                    EP=EORB(LP)
                    EQ=EORB(LQ)
                    ER=EORB(LR)
                    ES=EORB(LS)
C     PQRS
                    EPR=EP+ER
                    EQS=EQ+ES
                    ERS=ER-ES
                    EQR=EQ-ER
                    EPSR=EP-ES+ER
                    EQRS=EQ-ER+ES
C     RSPQ
                    ESQ=ES+EQ
                    EPQ=EP-EQ
                    ESP=ES-EP
                    ERQP=ER-EQ+EP
                    ESPQ=ES-EP+EQ
                    DO KI=INIDOC,LASDOC
                      EKI2I=EORB(KI)-ESI2I
                      EKI2J=EORB(KI)-ESI2J
                      OVRLP0(ISTATE,JSTATE)=OVRLP0(ISTATE,JSTATE)
     $                  +VONEEL(KI,LQ)*VTWOEL(LIJMO(LP,KI),LIJMO(LR,LS))
     $                  /(EPSR-EKI2I)
     $                  /(EPSR-EKI2J)*VAL2OD
     $                  +VTWOEL(LIJMO(KI,LQ),LIJMO(LR,LS))*VONEEL(LP,KI)
     $                  /(EP-EKI2I)
     $                  /(EP-EKI2J)*VAL2OD
     $                  +VONEEL(KI,LS)*VTWOEL(LIJMO(LR,KI),LIJMO(LP,LQ))
     $                  /(ERQP-EKI2I)
     $                  /(ERQP-EKI2J)*VAL2OD
     $                  +VTWOEL(LIJMO(KI,LS),LIJMO(LP,LQ))*VONEEL(LR,KI)
     $                  /(ER-EKI2I)
     $                  /(ER-EKI2J)*VAL2OD
                    END DO
                    DO KE=INIEXT,LASEXT
                      EKE2I=EORB(KE)+ESI2I
                      EKE2J=EORB(KE)+ESI2J
                      OVRLP1(KE,ISTATE,JSTATE)=OVRLP1(KE,ISTATE,JSTATE)
     $                  -VONEEL(LP,KE)*VTWOEL(LIJMO(KE,LQ),LIJMO(LR,LS))
     $                  /(EKE2I-EQRS)
     $                  /(EKE2J-EQRS)*VAL2OD
     $                  -VTWOEL(LIJMO(LP,KE),LIJMO(LR,LS))*VONEEL(KE,LQ)
     $                  /(EKE2I-EQ)
     $                  /(EKE2J-EQ)*VAL2OD
     $                  -VONEEL(LR,KE)*VTWOEL(LIJMO(KE,LS),LIJMO(LP,LQ))
     $                  /(EKE2I-ESPQ)
     $                  /(EKE2J-ESPQ)*VAL2OD
     $                  -VTWOEL(LIJMO(LR,KE),LIJMO(LP,LQ))*VONEEL(KE,LS)
     $                  /(EKE2I-ES)
     $                  /(EKE2J-ES)*VAL2OD
                    END DO
                    DO KI=INIDOC,LASDOC
                      DO KJ=INIDOC,LASDOC
                        EKIJ2I=EORB(KI)+EORB(KJ)-ESI2I
                        EKIJ2J=EORB(KI)+EORB(KJ)-ESI2J
                        OVRLP0(ISTATE,JSTATE)=OVRLP0(ISTATE,JSTATE)
     $                    - VTWOEL(LIJMO(KI,LQ),LIJMO(KJ,LS))
     $                    *VTWOEL(LIJMO(LP,KI),LIJMO(LR,KJ))
     $                    /(EPR-EKIJ2I)
     $                    /(EPR-EKIJ2J)      *VAL2OD
                      END DO
                    END DO
                    DO KA=INIACT,LASACT
                      DO KE=INIEXT,LASEXT
                        EKEA2I=EORB(KE)+EORB(KA)+ESI2I
                        EKEA2J=EORB(KE)+EORB(KA)+ESI2J
                        OVRLP2(KA,KE,ISTATE,JSTATE,0)=
     $                    OVRLP2(KA,KE,ISTATE,JSTATE,0)
     $                    - VTWOEL(LIJMO(LP,KE),LIJMO(LR,KA))
     $                    *VTWOEL(LIJMO(KE,LQ),LIJMO(KA,LS))
     $                    /(EKEA2I-EQS)
     $                    /(EKEA2J-EQS)      *VAL2OD
     $                    - VTWOEL(LIJMO(LR,KE),LIJMO(LP,KA))
     $                    *VTWOEL(LIJMO(KE,LS),LIJMO(KA,LQ))
     $                    /(EKEA2I-ESQ)
     $                    /(EKEA2J-ESQ)      *VAL2OD
                      END DO
                    END DO
                    DO KAE=INIACT,LASEXT
                      DO KI =INIDOC,LASDOC
                        EKAEI2I=EORB(KAE)-EORB(KI)+ESI2I
                        EKAEI2J=EORB(KAE)-EORB(KI)+ESI2J
                        OVRLP1(KAE,ISTATE,JSTATE)=
     $                    OVRLP1(KAE,ISTATE,JSTATE)
     $                    -( VTWOEL(LIJMO(KI,KAE),LIJMO(LP,LQ))
     $                    *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LR,LS))
     $                    -VTWOEL(LIJMO(KAE,LS),LIJMO(LR,KI)))
     $                    -VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LQ))
     $                    *VTWOEL(LIJMO(KAE,KI),LIJMO(LR,LS))
     $                    )/(EKAEI2I+ERS)
     $                    /(EKAEI2J+ERS)*VAL2OD
     $                    + VTWOEL(LIJMO(LP,KAE),LIJMO(KI,LS))
     $                    *VTWOEL(LIJMO(KAE,LQ),LIJMO(LR,KI))
     $                    /(EKAEI2I-EQR)
     $                    /(EKAEI2J-EQR)*VAL2OD
     $                    -( VTWOEL(LIJMO(KI,KAE),LIJMO(LR,LS))
     $                    *(2.0D+00*VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                    -VTWOEL(LIJMO(KAE,LQ),LIJMO(LP,KI)))
     $                    -VTWOEL(LIJMO(LR,KAE),LIJMO(KI,LS))
     $                    *VTWOEL(LIJMO(KAE,KI),LIJMO(LP,LQ))
     $                    )/(EKAEI2I+EPQ)
     $                    /(EKAEI2J+EPQ)*VAL2OD
     $                    + VTWOEL(LIJMO(LR,KAE),LIJMO(KI,LQ))
     $                    *VTWOEL(LIJMO(KAE,LS),LIJMO(LP,KI))
     $                    /(EKAEI2I-ESP)
     $                    /(EKAEI2J-ESP)*VAL2OD
                      END DO
                    END DO
                  END IF
                END DO
              END DO
            END DO
          END DO
        END IF
C.... WRITE GEN2 .......................................................
        NWRITE=0
        DO I=INIACT,LASACT
          DO J=INIACT,LASACT
            DO K=1,NCSF
              KN=LOD2NW(K)
              IF(KN.NE.0) THEN
                S=ABS(GEN2(I,J,KN))
                IF(S.GT.GENZRO) THEN
                  NWRITE=NWRITE+1
                  IF(NWRITE.GT.LENGTH) THEN
                    KCONTW=10
                    WRITE(LUNFT2) KCONTW,L1,L3,LENGTH
                    CALL MQGENW(LUNFT2,3*LENGTH,LENGTH,LABA2,WORK)
                    NWRITE=1
                  END IF
                  LABA2(1,NWRITE)=I
                  LABA2(2,NWRITE)=J
                  LABA2(3,NWRITE)=K
                  WORK (  NWRITE)=GEN2(I,J,KN)
                END IF
              END IF
            END DO
          END DO
        END DO
        KCONTW=0
        WRITE(LUNFT2) KCONTW,L1,L3,NWRITE
        CALL MQGENW(LUNFT2,3*NWRITE,NWRITE,LABA2,WORK)
        IF(KCNTR1.NE.0) THEN
          IF(MASWRK) WRITE(LUNOUT,1240)
          CALL ABRT
        END IF
 1240 FORMAT(' *** ERROR IN MQWGT2 ***'/
     $       ' GEN2 FILE AND <A/E_IJ/B> FILE ARE INCONSISTENT.'/
     $       ' *** CHECK PROGRAM!')
      ELSE
        IF(MASWRK) WRITE(LUNOUT,*) '*** ERROR IN MQWGT2 ***'
        CALL ABRT
      END IF
C==== DEBUG OUTPUT =====================================================
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** HEFF(2) <- TWO-BODY GENERATOR ***'')')
        CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
      END IF
C***********************************************************************
C****        ***********************************************************
C**** RETURN ***********************************************************
C****        ***********************************************************
      RETURN
      END
C*MODULE MCQDWT  *DECK MQWGT3
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++ MQWGT3 +++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MQWGT3(LUNOUT,LPOUT ,LUNFTA,LUNFT2,
     $                  INIACT,LASACT,NCSF  ,NOCF  ,MAXSPF,LENGTH,
     $                  NOCFI ,
     $                  NSNSF ,I1EX1 ,I1EX2 ,IOCSF ,
     $                  GEN1WK,GEN2  ,GEN3  ,
     $                  LABA2 ,WORKR ,VAL2  ,
     $                  NMOFZC,NMODOC,NMOACT,NMOEXT,
     $                  ISTATE,NSTATE,NMO   ,NMOEI ,NMOII ,THRGEN,
     $                  HEFF  ,CASVEC,EORB  ,LIJMO ,VTWOEL,
     $                  ECONF ,EREF0 ,KREF  ,MOSYM ,
     $                  NCSFNW,LOD2NW,
     $                  ISWTCH,
     $                  EPART0,EPART1,OVRLP0,OVRLP1)
C=======================================================================
C====                                                              =====
C====             THIS ROUTINE WAS CODED BY H.NAKANO               =====
C====                                                              =====
C====     INTELLIGENT MODELING LABORATORY, UNIVERSITY OF TOKYO     =====
C====       2-11-16 YAYOI, BUNKYO-KU, TOKYO 113-8656, JAPAN        =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL   KLOOP
      DIMENSION NSNSF(NOCF+1)
      DIMENSION I1EX1(INIACT:LASACT,0:NOCF)
      DIMENSION I1EX2(INIACT:LASACT,0:NOCFI)
      DIMENSION IOCSF(2,NCSF)
      DIMENSION GEN1WK(INIACT:LASACT,MAXSPF,MAXSPF)
      DIMENSION GEN2(INIACT:LASACT,INIACT:LASACT,0:NCSFNW)
      DIMENSION GEN3(INIACT:LASACT,INIACT:LASACT,INIACT:LASACT,MAXSPF)
      DIMENSION LABA2(3,LENGTH)
      DIMENSION WORKR(LENGTH)
      DIMENSION VAL2(NSTATE)
C
      DIMENSION HEFF  (NSTATE,NSTATE,0:3)
      DIMENSION CASVEC(NCSF,NSTATE), EORB(NMO)
      DIMENSION LIJMO (NMO,NMO)
      DIMENSION VTWOEL(NMOEI,NMOII)
      DIMENSION ECONF (NCSF)    , EREF0 (NSTATE)
      LOGICAL   KREF(NCSF)
      DIMENSION MOSYM(NMO)
      DIMENSION LOD2NW(NCSF)
      DIMENSION EPART0(NSTATE,NSTATE)
      DIMENSION EPART1(INIACT:NMO,NSTATE,NSTATE)
      DIMENSION OVRLP0(NSTATE,NSTATE)
      DIMENSION OVRLP1(INIACT:NMO,NSTATE,NSTATE)
      DIMENSION ISYM(8,8)
      COMMON/MQSYLB/ISYLAB(2,8,4)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DATA ISYM /1,2,3,4,5,6,7,8,
     $           2,1,4,3,6,5,8,7,
     $           3,4,1,2,7,8,5,6,
     $           4,3,2,1,8,7,6,5,
     $           5,6,7,8,1,2,3,4,
     $           6,5,8,7,2,1,4,3,
     $           7,8,5,6,3,4,1,2,
     $           8,7,6,5,4,3,2,1/
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQWGT3 ***'')')
        CALL MQDEBG(LUNOUT,
     $  'LUNFTA','LUNFT2','INIACT','LASACT','  NCSF','  NOCF','MAXSPF',
     $   LUNFTA , LUNFT2 , INIACT , LASACT ,   NCSF ,   NOCF , MAXSPF )
        CALL MQDEBG(LUNOUT,
     $  'LENGTH','NMOFZC','NMODOC','NMOACT','NMOEXT','ISTATE','NSTATE',
     $   LENGTH , NMOFZC , NMODOC , NMOACT , NMOEXT , ISTATE , NSTATE )
        CALL MQDEBG(LUNOUT,
     $  '   NMO',' NMOEI',' NMOII','NCSFNW','     -','     -','     -',
     $      NMO ,  NMOEI ,  NMOII , NCSFNW ,      0 ,      0 ,      0 )
      END IF
C**** MP2 CASE *********************************************************
      IF(NMOACT.EQ.0) RETURN
C==== SET CONSTANS =====================================================
      INIDOC=NMOFZC+1
      LASDOC=NMOFZC+NMODOC
      INIACT=NMOFZC+NMODOC+1
      LASACT=NMOFZC+NMODOC+NMOACT
      INIEXT=NMOFZC+NMODOC+NMOACT+1
      LASEXT=NMOFZC+NMODOC+NMOACT+NMOEXT
C***********************************************************************
C****       ************************************************************
C**** START ************************************************************
C****       ************************************************************
      CALL SEQREW(LUNFT2)
C**** CLEAR ARRAYS *****************************************************
      DO K=1,MAXSPF
        DO J=1,MAXSPF
          DO I=INIACT,LASACT
            GEN1WK(I,J,K)=0.0D+00
          END DO
        END DO
      END DO
      DO MU=1,MAXSPF
        DO K=INIACT,LASACT
          DO J=INIACT,LASACT
            DO I=INIACT,LASACT
              GEN3(I,J,K,MU)=0.0D+00
            END DO
          END DO
        END DO
      END DO
      DO I=INIACT,LASACT
        DO J=INIACT,LASACT
          DO K=0,NCSFNW
            GEN2(I,J,K)=0.0D+00
          END DO
        END DO
      END DO
C**** READ 2-BODY COUPLING CONSTANT ************************************
  124 CONTINUE
        READ(LUNFT2) KCNTR2,L1,L3,NWORD
        CALL MQGENR(LUNFT2,3*NWORD,NWORD,LABA2,WORKR)
        DO I=1,NWORD
          L2=LABA2(1,I)
          L4=LABA2(2,I)
          ICSF3=LABA2(3,I)
          IOCF3=IOCSF(1,ICSF3)
          ICSF3N=LOD2NW(ICSF3)
          GEN2(L2,L4,ICSF3N)=WORKR(I)
        END DO
      IF(KCNTR2.EQ.10) GO TO 124
C**** READ 1-BODY COUPLING CONSTANT ************************************
      CALL SEQREW(LUNFTA)
  128 CONTINUE
      MAXNU=0
  130 CONTINUE
        READ(LUNFTA) KCNTRA,L5,IOCF2,NWORD
        CALL MQGENR(LUNFTA,3*NWORD,NWORD,LABA2,WORKR)
        DO I=1,NWORD
          L6=LABA2(1,I)
          NU=LABA2(2,I)
          IF(MAXNU.LT.NU) MAXNU=NU
          MU=LABA2(3,I)
          GEN1WK(L6,NU,MU)=WORKR(I)
        END DO
      IF(KCNTRA.EQ.100) GO TO 130
      MAXMU=NSNSF(IOCF2+1)-NSNSF(IOCF2)
C**** CALCULATE 3-BODY COUPLING CONSTANT *******************************
      NSPF2=MAXMU
C
      IS13=ISYM(MOSYM(L1),MOSYM(L3))
      IS135=ISYM(IS13,MOSYM(L5))
C
      DO L6=INIACT,LASACT
C
        IS6=ISYM(IS135,MOSYM(L6))
C
        IOCF3=I1EX2(L5,I1EX1(L6,IOCF2))
        IF(IOCF3.NE.0) THEN
          NSPF3=NSNSF(IOCF3+1)-NSNSF(IOCF3)
          DO L4=INIACT,LASACT
C
            IS64=ISYM(IS6,MOSYM(L4))
C
            DO L2=INIACT,LASACT
C
              IS642=ISYM(IS64,MOSYM(L2))
              IF(IS642.EQ.1) THEN
C
                DO NU=1,NSPF3
                  ICSF3=NSNSF(IOCF3)+NU
                  ICSF3N=LOD2NW(ICSF3)
                  IF(GEN2(L2,L4,ICSF3N).NE.0.0D+00) THEN
                    DO MU=1,NSPF2
                      IF(KREF(NSNSF(IOCF2)+MU)) THEN
                        GEN3(L2,L4,L6,MU)=GEN3(L2,L4,L6,MU)
     $                    +GEN2(L2,L4,ICSF3N)*GEN1WK(L6,NU,MU)
                      END IF
                    END DO
                  END IF
                END DO
C
              END IF
C
            END DO
          END DO
        END IF
      END DO
      DO L6=INIACT,LASACT
C
        IS6=ISYM(IS13,MOSYM(L6))
C
        DO L24=INIACT,LASACT
C
          IS642=ISYM(IS6,MOSYM(L24))
          IF(IS642.EQ.1) THEN
C
            DO MU=1,NSPF2
              ICSF2=NSNSF(IOCF2)+MU
              IF(KREF(ICSF2)) THEN
                GEN3(L5 ,L24,L6,MU)=GEN3(L5 ,L24,L6,MU)
     $            -GEN2(L6 ,L24,LOD2NW(ICSF2))
              END IF
            END DO
            DO MU=1,NSPF2
              ICSF2=NSNSF(IOCF2)+MU
              IF(KREF(ICSF2)) THEN
                GEN3(L24,L5 ,L6,MU)=GEN3(L24,L5 ,L6,MU)
     $            -GEN2(L24,L6 ,LOD2NW(ICSF2))
              END IF
            END DO
C
          END IF
C
        END DO
      END DO
C=======================================================================
C==== THREE-BODY GENERATOR =============================================
C====                      =============================================
      DO KK=INIACT,LASACT
        DO JJ=INIACT,LASACT
          DO II=INIACT,LASACT
            LP=L1
            LR=L3
            LT=L5
            LQ=II
            LS=JJ
            LU=KK
            IF(LP.NE.LT) THEN
C
              IS=ISYM(MOSYM(LP),MOSYM(LQ))
              IS=ISYM(IS,ISYM(MOSYM(LR),MOSYM(LS)))
              IS=ISYM(IS,ISYM(MOSYM(LT),MOSYM(LU)))
              IF(IS.EQ.1) THEN
C
                DO JSTATE=1,NSTATE
                  VAL2(JSTATE)=0.0D+00
                END DO
                DO JSTATE=1,NSTATE
                  DO LL=1,NSPF2
                    M2=NSNSF(IOCF2)+LL
                    IF(KREF(M2)) THEN
                      VAL2(JSTATE)=VAL2(JSTATE)
     $                  +GEN3(II,JJ,KK,LL)*CASVEC(M2,JSTATE)
                    END IF
                  END DO
                  IF(LP.EQ.LR .OR. LR.EQ.LT) THEN
                    VAL2(JSTATE)=VAL2(JSTATE)*0.5D+00
                  END IF
                END DO
                KLOOP=.FALSE.
                DO JSTATE=1,NSTATE
                  IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
                END DO
C 3-B <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                IF(KLOOP) THEN
                  ESI2=ECONF(M2)-EREF0(ISTATE)
                  S=0.0D+00
                  EP  =EORB(LP)
                  EQ  =EORB(LQ)
                  ER  =EORB(LR)
                  ES  =EORB(LS)
                  ET  =EORB(LT)
                  EU  =EORB(LU)
C1    PQRSTU
                  EPTU=EP+ET-EU
                  EQTU=EQ-ET+EU
C2    PQTURS
                  EPRS=EP+ER-ES
                  EQRS=EQ-ER+ES
C3    RSPQTU
                  ERTU=ER+ET-EU
                  ESTU=ES-ET+EU
C4    RSTUPQ
                  ERPQ=ER+EP-EQ
                  ESPQ=ES-EP+EQ
C5    TUPQRS
                  ETRS=ET+ER-ES
                  EURS=EU-ER+ES
C6    TURSPQ
                  ETPQ=ET+EP-EQ
                  EUPQ=EU-EP+EQ
                  DO KI=INIDOC,LASDOC
                    EKI2=EORB(KI)-ESI2
                    S=S+ VTWOEL(LIJMO(KI,LQ),LIJMO(LR,LS))
     $                *VTWOEL(LIJMO(LP,KI),LIJMO(LT,LU))/(EPTU-EKI2)
     $                + VTWOEL(LIJMO(KI,LQ),LIJMO(LT,LU))
     $                *VTWOEL(LIJMO(LP,KI),LIJMO(LR,LS))/(EPRS-EKI2)
     $                + VTWOEL(LIJMO(KI,LS),LIJMO(LP,LQ))
     $                *VTWOEL(LIJMO(LR,KI),LIJMO(LT,LU))/(ERTU-EKI2)
     $                + VTWOEL(LIJMO(KI,LS),LIJMO(LT,LU))
     $                *VTWOEL(LIJMO(LR,KI),LIJMO(LP,LQ))/(ERPQ-EKI2)
     $                + VTWOEL(LIJMO(KI,LU),LIJMO(LP,LQ))
     $                *VTWOEL(LIJMO(LT,KI),LIJMO(LR,LS))/(ETRS-EKI2)
     $                + VTWOEL(LIJMO(KI,LU),LIJMO(LR,LS))
     $                *VTWOEL(LIJMO(LT,KI),LIJMO(LP,LQ))/(ETPQ-EKI2)
                  END DO
C
                  IS=ISYM(MOSYM(LP),ISYM(MOSYM(LR),MOSYM(LS)))
                  DO KE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                 DO KE=INIEXT,LASEXT
                    EKE2=EORB(KE)+ESI2
                    S=S-VTWOEL(LIJMO(LP,KE),LIJMO(LR,LS))
     $                *VTWOEL(LIJMO(KE,LQ),LIJMO(LT,LU))/(EKE2-EQTU)
                  END DO
                  IS=ISYM(MOSYM(LP),ISYM(MOSYM(LT),MOSYM(LU)))
                  DO KE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                  DO KE=INIEXT,LASEXT
                    EKE2=EORB(KE)+ESI2
                    S=S-VTWOEL(LIJMO(LP,KE),LIJMO(LT,LU))
     $                *VTWOEL(LIJMO(KE,LQ),LIJMO(LR,LS))/(EKE2-EQRS)
                  END DO
                  IS=ISYM(MOSYM(LP),ISYM(MOSYM(LQ),MOSYM(LR)))
                  DO KE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                 DO KE=INIEXT,LASEXT
                    EKE2=EORB(KE)+ESI2
                    S=S-VTWOEL(LIJMO(LR,KE),LIJMO(LP,LQ))
     $                *VTWOEL(LIJMO(KE,LS),LIJMO(LT,LU))/(EKE2-ESTU)
                  END DO
                  IS=ISYM(MOSYM(LP),ISYM(MOSYM(LQ),MOSYM(LS)))
                  DO KE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                 DO KE=INIEXT,LASEXT
                    EKE2=EORB(KE)+ESI2
                    S=S-VTWOEL(LIJMO(LR,KE),LIJMO(LT,LU))
     $                *VTWOEL(LIJMO(KE,LS),LIJMO(LP,LQ))/(EKE2-ESPQ)
                  END DO
                  IS=ISYM(MOSYM(LP),ISYM(MOSYM(LQ),MOSYM(LT)))
                  DO KE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                 DO KE=INIEXT,LASEXT
                    EKE2=EORB(KE)+ESI2
                    S=S-VTWOEL(LIJMO(LT,KE),LIJMO(LP,LQ))
     $                *VTWOEL(LIJMO(KE,LU),LIJMO(LR,LS))/(EKE2-EURS)
                  END DO
                  IS=ISYM(MOSYM(LP),ISYM(MOSYM(LQ),MOSYM(LU)))
                  DO KE=ISYLAB(1,IS,4),ISYLAB(2,IS,4)
C                 DO KE=INIEXT,LASEXT
                    EKE2=EORB(KE)+ESI2
                    S=S-VTWOEL(LIJMO(LT,KE),LIJMO(LR,LS))
     $                *VTWOEL(LIJMO(KE,LU),LIJMO(LP,LQ))/(EKE2-EUPQ)
                  END DO
                  DO JSTATE=1,NSTATE
                    IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
                      HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
     $                  +S*VAL2(JSTATE)
                    END IF
                  END DO
                END IF
C 3-B >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
              END IF
            END IF
          END DO
        END DO
      END DO
C=======================================================================
C==== THREE-BODY GENERATOR =============================================
C====                      =============================================
      IF(ISWTCH.EQ.1 .OR. ISWTCH.EQ.3) THEN
        DO KK=INIACT,LASACT
          DO JJ=INIACT,LASACT
            DO II=INIACT,LASACT
              LP=L1
              LR=L3
              LT=L5
              LQ=II
              LS=JJ
              LU=KK
              IF(LP.NE.LT) THEN
                DO JSTATE=1,ISTATE
                  VAL2OD=0.0D+00
                  DO LL=1,NSPF2
                    M2=NSNSF(IOCF2)+LL
                    IF(KREF(M2)) VAL2OD=
     $                VAL2OD+GEN3(II,JJ,KK,LL)*CASVEC(M2,JSTATE)
                  END DO
                  IF(LP.EQ.LR .OR. LR.EQ.LT) VAL2OD=VAL2OD*0.5D+00
                  IF(ABS(VAL2OD).GE.THRGEN) THEN
                    ESI2=ECONF(M2)-EREF0(JSTATE)
                    S=0.0D+00
                    EP  =EORB(LP)
                    EQ  =EORB(LQ)
                    ER  =EORB(LR)
                    ES  =EORB(LS)
                    ET  =EORB(LT)
                    EU  =EORB(LU)
C1    PQRSTU
                    EPTU=EP+ET-EU
                    EQTU=EQ-ET+EU
C2    PQTURS
                    EPRS=EP+ER-ES
                    EQRS=EQ-ER+ES
C3    RSPQTU
                    ERTU=ER+ET-EU
                    ESTU=ES-ET+EU
C4    RSTUPQ
                    ERPQ=ER+EP-EQ
                    ESPQ=ES-EP+EQ
C5    TUPQRS
                    ETRS=ET+ER-ES
                    EURS=EU-ER+ES
C6    TURSPQ
                    ETPQ=ET+EP-EQ
                    EUPQ=EU-EP+EQ
                    DO KI=INIDOC,LASDOC
                      EKI2=EORB(KI)-ESI2
                      EPART0(ISTATE,JSTATE)=EPART0(ISTATE,JSTATE)
     $                  + VTWOEL(LIJMO(KI,LQ),LIJMO(LR,LS))
     $                  *VTWOEL(LIJMO(LP,KI),LIJMO(LT,LU))
     $                  /(EPTU-EKI2)*VAL2OD
     $                  + VTWOEL(LIJMO(KI,LQ),LIJMO(LT,LU))
     $                  *VTWOEL(LIJMO(LP,KI),LIJMO(LR,LS))
     $                  /(EPRS-EKI2)*VAL2OD
     $                  + VTWOEL(LIJMO(KI,LS),LIJMO(LP,LQ))
     $                  *VTWOEL(LIJMO(LR,KI),LIJMO(LT,LU))
     $                  /(ERTU-EKI2)*VAL2OD
     $                  + VTWOEL(LIJMO(KI,LS),LIJMO(LT,LU))
     $                  *VTWOEL(LIJMO(LR,KI),LIJMO(LP,LQ))
     $                  /(ERPQ-EKI2)*VAL2OD
     $                  + VTWOEL(LIJMO(KI,LU),LIJMO(LP,LQ))
     $                  *VTWOEL(LIJMO(LT,KI),LIJMO(LR,LS))
     $                  /(ETRS-EKI2)*VAL2OD
     $                  + VTWOEL(LIJMO(KI,LU),LIJMO(LR,LS))
     $                  *VTWOEL(LIJMO(LT,KI),LIJMO(LP,LQ))
     $                  /(ETPQ-EKI2)*VAL2OD
                    END DO
                    DO KE=INIEXT,LASEXT
                      EKE2=EORB(KE)+ESI2
                      EPART1(KE,ISTATE,JSTATE)=EPART1(KE,ISTATE,JSTATE)
     $                  - VTWOEL(LIJMO(LP,KE),LIJMO(LR,LS))
     $                  *VTWOEL(LIJMO(KE,LQ),LIJMO(LT,LU))
     $                  /(EKE2-EQTU)*VAL2OD
     $                  - VTWOEL(LIJMO(LP,KE),LIJMO(LT,LU))
     $                  *VTWOEL(LIJMO(KE,LQ),LIJMO(LR,LS))
     $                  /(EKE2-EQRS)*VAL2OD
     $                  - VTWOEL(LIJMO(LR,KE),LIJMO(LP,LQ))
     $                  *VTWOEL(LIJMO(KE,LS),LIJMO(LT,LU))
     $                  /(EKE2-ESTU)*VAL2OD
     $                  - VTWOEL(LIJMO(LR,KE),LIJMO(LT,LU))
     $                  *VTWOEL(LIJMO(KE,LS),LIJMO(LP,LQ))
     $                  /(EKE2-ESPQ)*VAL2OD
     $                  - VTWOEL(LIJMO(LT,KE),LIJMO(LP,LQ))
     $                  *VTWOEL(LIJMO(KE,LU),LIJMO(LR,LS))
     $                  /(EKE2-EURS)*VAL2OD
     $                  - VTWOEL(LIJMO(LT,KE),LIJMO(LR,LS))
     $                  *VTWOEL(LIJMO(KE,LU),LIJMO(LP,LQ))
     $                  /(EKE2-EUPQ)*VAL2OD
                    END DO
                  END IF
                END DO
              END IF
            END DO
          END DO
        END DO
      END IF
C=======================================================================
C==== THREE-BODY GENERATOR =============================================
C====                      =============================================
      IF(ISWTCH.EQ.2 .OR. ISWTCH.EQ.3) THEN
        DO KK=INIACT,LASACT
          DO JJ=INIACT,LASACT
            DO II=INIACT,LASACT
              LP=L1
              LR=L3
              LT=L5
              LQ=II
              LS=JJ
              LU=KK
              IF(LP.NE.LT) THEN
                DO JSTATE=1,ISTATE
                  VAL2OD=0.0D+00
                  DO LL=1,NSPF2
                    M2=NSNSF(IOCF2)+LL
                    IF(KREF(M2)) VAL2OD=
     $                VAL2OD+GEN3(II,JJ,KK,LL)*CASVEC(M2,JSTATE)
                  END DO
                  IF(LP.EQ.LR .OR. LR.EQ.LT) VAL2OD=VAL2OD*0.5D+00
                  IF(ABS(VAL2OD).GE.THRGEN) THEN
                    ESI2I=ECONF(M2)-EREF0(ISTATE)
                    ESI2J=ECONF(M2)-EREF0(JSTATE)
                    S=0.0D+00
                    EP  =EORB(LP)
                    EQ  =EORB(LQ)
                    ER  =EORB(LR)
                    ES  =EORB(LS)
                    ET  =EORB(LT)
                    EU  =EORB(LU)
C1    PQRSTU
                    EPTU=EP+ET-EU
                    EQTU=EQ-ET+EU
C2    PQTURS
                    EPRS=EP+ER-ES
                    EQRS=EQ-ER+ES
C3    RSPQTU
                    ERTU=ER+ET-EU
                    ESTU=ES-ET+EU
C4    RSTUPQ
                    ERPQ=ER+EP-EQ
                    ESPQ=ES-EP+EQ
C5    TUPQRS
                    ETRS=ET+ER-ES
                    EURS=EU-ER+ES
C6    TURSPQ
                    ETPQ=ET+EP-EQ
                    EUPQ=EU-EP+EQ
                    DO KI=INIDOC,LASDOC
                      EKI2I=EORB(KI)-ESI2I
                      EKI2J=EORB(KI)-ESI2J
                      OVRLP0(ISTATE,JSTATE)=OVRLP0(ISTATE,JSTATE)
     $                  + VTWOEL(LIJMO(KI,LQ),LIJMO(LR,LS))
     $                  *VTWOEL(LIJMO(LP,KI),LIJMO(LT,LU))
     $                  /(EPTU-EKI2I)
     $                  /(EPTU-EKI2J)*VAL2OD
     $                  + VTWOEL(LIJMO(KI,LQ),LIJMO(LT,LU))
     $                  *VTWOEL(LIJMO(LP,KI),LIJMO(LR,LS))
     $                  /(EPRS-EKI2I)
     $                  /(EPRS-EKI2J)*VAL2OD
     $                  + VTWOEL(LIJMO(KI,LS),LIJMO(LP,LQ))
     $                  *VTWOEL(LIJMO(LR,KI),LIJMO(LT,LU))
     $                  /(ERTU-EKI2I)
     $                  /(ERTU-EKI2J)*VAL2OD
     $                  + VTWOEL(LIJMO(KI,LS),LIJMO(LT,LU))
     $                  *VTWOEL(LIJMO(LR,KI),LIJMO(LP,LQ))
     $                  /(ERPQ-EKI2I)
     $                  /(ERPQ-EKI2J)*VAL2OD
     $                  + VTWOEL(LIJMO(KI,LU),LIJMO(LP,LQ))
     $                  *VTWOEL(LIJMO(LT,KI),LIJMO(LR,LS))
     $                  /(ETRS-EKI2I)
     $                  /(ETRS-EKI2J)*VAL2OD
     $                  + VTWOEL(LIJMO(KI,LU),LIJMO(LR,LS))
     $                  *VTWOEL(LIJMO(LT,KI),LIJMO(LP,LQ))
     $                  /(ETPQ-EKI2I)
     $                  /(ETPQ-EKI2J)*VAL2OD
                    END DO
                    DO KE=INIEXT,LASEXT
                      EKE2I=EORB(KE)+ESI2I
                      EKE2J=EORB(KE)+ESI2J
                      OVRLP1(KE,ISTATE,JSTATE)=OVRLP1(KE,ISTATE,JSTATE)
     $                  - VTWOEL(LIJMO(LP,KE),LIJMO(LR,LS))
     $                  *VTWOEL(LIJMO(KE,LQ),LIJMO(LT,LU))
     $                  /(EKE2I-EQTU)
     $                  /(EKE2J-EQTU)*VAL2OD
     $                  - VTWOEL(LIJMO(LP,KE),LIJMO(LT,LU))
     $                  *VTWOEL(LIJMO(KE,LQ),LIJMO(LR,LS))
     $                  /(EKE2I-EQRS)
     $                  /(EKE2J-EQRS)*VAL2OD
     $                  - VTWOEL(LIJMO(LR,KE),LIJMO(LP,LQ))
     $                  *VTWOEL(LIJMO(KE,LS),LIJMO(LT,LU))
     $                  /(EKE2I-ESTU)
     $                  /(EKE2J-ESTU)*VAL2OD
     $                  - VTWOEL(LIJMO(LR,KE),LIJMO(LT,LU))
     $                  *VTWOEL(LIJMO(KE,LS),LIJMO(LP,LQ))
     $                  /(EKE2I-ESPQ)
     $                  /(EKE2J-ESPQ)*VAL2OD
     $                  - VTWOEL(LIJMO(LT,KE),LIJMO(LP,LQ))
     $                  *VTWOEL(LIJMO(KE,LU),LIJMO(LR,LS))
     $                  /(EKE2I-EURS)
     $                  /(EKE2J-EURS)*VAL2OD
     $                  - VTWOEL(LIJMO(LT,KE),LIJMO(LR,LS))
     $                  *VTWOEL(LIJMO(KE,LU),LIJMO(LP,LQ))
     $                  /(EKE2I-EUPQ)
     $                  /(EKE2J-EUPQ)*VAL2OD
                    END DO
                  END IF
                END DO
              END IF
            END DO
          END DO
        END DO
      END IF
C**** IF END OF OCF2-GROUP, ... ****************************************
      IF(KCNTRA.EQ.10) THEN
        DO K=1,MAXMU
          DO I=INIACT,LASACT
            DO J=1,MAXNU
              GEN1WK(I,J,K)=0.0D+00
            END DO
            DO J=INIACT,LASACT
              DO L=INIACT,LASACT
                GEN3(L,J,I,K)=0.0D+00
              END DO
            END DO
          END DO
        END DO
        GO TO 128
C**** IF END OF L5-GROUP, ... ******************************************
      ELSE IF(KCNTRA.EQ.1) THEN
        DO K=1,MAXMU
          DO I=INIACT,LASACT
            DO J=1,MAXNU
              GEN1WK(I,J,K)=0.0D+00
            END DO
            DO J=INIACT,LASACT
              DO L=INIACT,LASACT
                GEN3(L,J,I,K)=0.0D+00
              END DO
            END DO
          END DO
        END DO
        IF(L3.EQ.L5) THEN
          DO I=INIACT,LASACT
            DO J=INIACT,LASACT
              DO K=0,NCSFNW
                GEN2(I,J,K)=0.0D+00
              END DO
            END DO
          END DO
          GO TO 124
        END IF
        GO TO 128
C**** IF END OF 1-BODY COUPLING CONSTANT FILE **************************
      ELSE
        IF(KCNTR2.EQ.0) THEN
          GO TO 214
        ELSE
          DO K=1,MAXMU
            DO I=INIACT,LASACT
              DO J=1,MAXNU
                GEN1WK(I,J,K)=0.0D+00
              END DO
              DO J=INIACT,LASACT
                DO L=INIACT,LASACT
                  GEN3(L,J,I,K)=0.0D+00
                END DO
              END DO
            END DO
          END DO
          DO I=INIACT,LASACT
            DO J=INIACT,LASACT
              DO K=0,NCSFNW
                GEN2(I,J,K)=0.0D+00
              END DO
            END DO
          END DO
          GO TO 124
        END IF
      END IF
C==== DEBUG OUTPUT =====================================================
  214 CONTINUE
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** HEFF(2) <- THREE-BODY GENERATOR ***'')')
        CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
      END IF
C***********************************************************************
C****        ***********************************************************
C**** RETURN ***********************************************************
C****        ***********************************************************
      RETURN
      END
C*MODULE MCQDWT  *DECK MQWGTR
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++ MQWGTR +++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MQWGTR(LUNOUT,LPOUT ,LUNTWO,LUNFT1,LUNFT2,
     $                  NMOFZC,NMODOC,NMOACT,NMOEXT,NMO   ,
     $                  INIDOC,LASDOC,INIACT,LASACT,INIEXT,LASEXT,
     $                  NCSF  ,NSTATE,LENGTH,ISTATE,THRGEN,
     $                  ISTART,IEND  ,
     $                  IROT  ,
     $                  HEFF  ,CASVEC,EORB  ,ECONF ,
     $                  EREF0 ,LABEL1,LABEL2,WORK  ,KREF  ,IOCSF ,
     $                  MOSYM ,VAL2  ,VTWOEL,
     $                  ISWTCH,
     $                  EPART2,OVRLP2)
C=======================================================================
C====                                                              =====
C====             THIS ROUTINE WAS CODED BY H.NAKANO               =====
C====                                                              =====
C====     INTELLIGENT MODELING LABORATORY, UNIVERSITY OF TOKYO     =====
C====       2-11-16 YAYOI, BUNKYO-KU, TOKYO 113-8656, JAPAN        =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***********************************************************************
C****                         ******************************************
C****   ARRAYS AND CONSTANT   ******************************************
C****                         ******************************************
      LOGICAL   KLOOP
      DIMENSION HEFF  (NSTATE,NSTATE,0:3)
      DIMENSION CASVEC(NCSF,NSTATE), EORB(NMO)
      DIMENSION ECONF (NCSF)    , EREF0 (NSTATE)
      DIMENSION LABEL1(2,LENGTH), LABEL2(3,LENGTH)
      DIMENSION WORK  (  LENGTH)
      LOGICAL   KREF(NCSF)
      DIMENSION IOCSF(2,NCSF)
      DIMENSION MOSYM(NMO)
      DIMENSION VAL2(NSTATE)
      DIMENSION VTWOEL(INIDOC:LASACT,INIEXT:LASEXT,
     $                 INIDOC:LASACT,ISTART:IEND  )
      DIMENSION EPART2(INIACT:NMO,INIACT:NMO,NSTATE,NSTATE,0:1)
      DIMENSION OVRLP2(INIACT:NMO,INIACT:NMO,NSTATE,NSTATE,0:1)
      DIMENSION ISYM(8,8)
      COMMON/MQSYLB/ISYLAB(2,8,4)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DATA ISYM /1,2,3,4,5,6,7,8,
     $           2,1,4,3,6,5,8,7,
     $           3,4,1,2,7,8,5,6,
     $           4,3,2,1,8,7,6,5,
     $           5,6,7,8,1,2,3,4,
     $           6,5,8,7,2,1,4,3,
     $           7,8,5,6,3,4,1,2,
     $           8,7,6,5,4,3,2,1/
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQWGTR ***'')')
        CALL MQDEBG(LUNOUT,
     $  'LUNTWO','LUNFT1','LUNFT2','NMOFZC','NMODOC','NMOACT','NMOEXT',
     $   LUNTWO , LUNFT1 , LUNFT2 , NMOFZC , NMODOC , NMOACT , NMOEXT )
        CALL MQDEBG(LUNOUT,
     $  '   NMO','INIDOC','LASDOC','INIACT','LASACT','INIEXT','LASEXT',
     $      NMO , INIDOC , LASDOC , INIACT , LASACT , INIEXT , LASEXT )
        CALL MQDEBG(LUNOUT,
     $  '  NCSF','NSTATE','LENGTH','ISTATE','ISTART','  IEND','     -',
     $     NCSF , NSTATE , LENGTH , ISTATE , ISTART ,   IEND ,      0 )
      END IF
C**** READ TWO-ELECTRON INTEGRALS **************************************
      NDSIZE=(NMODOC+NMOACT)*NMOEXT
      DO J=ISTART,IEND
        DO I=INIDOC,LASACT
          CALL MQMATR(LUNTWO,NDSIZE,VTWOEL(INIDOC,INIEXT,I,J))
        END DO
      END DO
C***********************************************************************
C****              *****************************************************
C**** SECOND ORDER *****************************************************
C****              *****************************************************
      CALL SEQREW(LUNFT1)
      CALL SEQREW(LUNFT2)
C***********************************************************************
C**** ZERO-BODY GENERATOR (GET ECORE) **********************************
C****                                 **********************************
C.... CPU TIME .........................................................
      CALL TSECND(STIME)
C
      IF(IROT.EQ.0 .OR. IROT.EQ.1) THEN
        DO I=1,NCSF
          IF(KREF(I)) THEN
            KLOOP=.FALSE.
            DO JSTATE=1,NSTATE
              VAL2(JSTATE)=CASVEC(I,ISTATE)*CASVEC(I,JSTATE)
              IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
            END DO
            IF(KLOOP) THEN
              ESI=ECONF(I)-EREF0(ISTATE)
              S=0.0D+00
              DO KBF=ISTART,IEND
                DO KAE=INIEXT,LASEXT
                  IF(IROT.NE.0 .AND.
     $              KAE.GE.INIEXT .AND. KBF.GE.INIEXT) THEN
                    ESIDUM=0.0D+00
                  ELSE
                    ESIDUM=ESI
                  END IF
                  DO KJ =INIDOC,LASDOC
                    DO KI =INIDOC,LASDOC
                      S=S-VTWOEL(KI,KAE,KJ,KBF)
     $                  *(2.0D+00*VTWOEL(KI,KAE,KJ,KBF)
     $                  -VTWOEL(KJ,KAE,KI,KBF)
     $                  )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)
     $                  +ESIDUM)
                    END DO
                  END DO
                END DO
              END DO
              DO JSTATE=1,NSTATE
                IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
                  HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
     $              +S*VAL2(JSTATE)
                END IF
              END DO
            END IF
          END IF
        END DO
      ELSE
        DO I=1,NCSF
          IF(KREF(I)) THEN
            KLOOP=.FALSE.
            DO JSTATE=1,NSTATE
              VAL2(JSTATE)=CASVEC(I,ISTATE)*CASVEC(I,JSTATE)
              IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
            END DO
            IF(KLOOP) THEN
              S=0.0D+00
              T=0.0D+00
              ESI=ECONF(I)-EREF0(ISTATE)
              DO KBF=ISTART,IEND
                DO KAE=INIEXT,LASEXT
                  IF(IROT.NE.0 .AND.
     $              KAE.GE.INIEXT .AND. KBF.GE.INIEXT) THEN
                    ESIDUM=0.0D+00
                  ELSE
                    ESIDUM=ESI
                  END IF
                  DO KJ =INIDOC,LASDOC
                    DO KI =INIDOC,LASDOC
                      S=S-VTWOEL(KI,KAE,KJ,KBF)
     $                  *(2.0D+00*VTWOEL(KI,KAE,KJ,KBF)
     $                  -VTWOEL(KJ,KAE,KI,KBF)
     $                  )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)+ESI)
                      T=T-VTWOEL(KI,KAE,KJ,KBF)
     $                  *(2.0D+00*VTWOEL(KI,KAE,KJ,KBF)
     $                  -VTWOEL(KJ,KAE,KI,KBF)
     $                  )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)
     $                  +ESIDUM)
                    END DO
                  END DO
                END DO
              END DO
              DO JSTATE=1,NSTATE
                IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
                  HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
     $              +S*VAL2(JSTATE)
                  HEFF(ISTATE,JSTATE,3)=HEFF(ISTATE,JSTATE,3)
     $              +(T-S)*VAL2(JSTATE)
                END IF
              END DO
            END IF
          END IF
        END DO
      END IF
      IF(IROT.EQ.0 .OR. IROT.EQ.1) THEN
C
        IF(ISWTCH.EQ.1 .OR. ISWTCH.EQ.3) THEN
          DO JSTATE=1,NSTATE
            DO I=1,NCSF
              IF(KREF(I)) THEN
                CACA=CASVEC(I,ISTATE)*CASVEC(I,JSTATE)
                IF(ABS(CACA).GE.THRGEN) THEN
                  ESI=ECONF(I)-EREF0(JSTATE)
                  DO KBF=ISTART,IEND
                    DO KAE=INIEXT,LASEXT
                      IF(IROT.NE.0 .AND.
     $                  KAE.GE.INIEXT .AND. KBF.GE.INIEXT) THEN
                        ESIDUM=0.0D+00
                      ELSE
                        ESIDUM=ESI
                      END IF
                      DO KJ =INIDOC,LASDOC
                        DO KI =INIDOC,LASDOC
                          EPART2(KAE,KBF,ISTATE,JSTATE,0)=
     $                      EPART2(KAE,KBF,ISTATE,JSTATE,0)
     $                      -VTWOEL(KI,KAE,KJ,KBF)
     $                      *(2.0D+00*VTWOEL(KI,KAE,KJ,KBF)
     $                      -VTWOEL(KJ,KAE,KI,KBF)
     $                      )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)
     $                      +ESIDUM)
     $                      *CACA
                        END DO
                      END DO
                    END DO
                  END DO
                END IF
              END IF
            END DO
          END DO
        END IF
        IF(ISWTCH.EQ.2 .OR. ISWTCH.EQ.3) THEN
          DO JSTATE=1,ISTATE
            DO I=1,NCSF
              IF(KREF(I)) THEN
                CACA=CASVEC(I,ISTATE)*CASVEC(I,JSTATE)
                IF(ABS(CACA).GE.THRGEN) THEN
                  ESII=ECONF(I)-EREF0(ISTATE)
                  ESIJ=ECONF(I)-EREF0(JSTATE)
                  DO KBF=ISTART,IEND
                    DO KAE=INIEXT,LASEXT
                      IF(IROT.NE.0 .AND.
     $                  KAE.GE.INIEXT .AND. KBF.GE.INIEXT) THEN
                        ESIDUMI=0.0D+00
                        ESIDUMJ=0.0D+00
                      ELSE
                        ESIDUMI=ESII
                        ESIDUMJ=ESIJ
                      END IF
                      DO KJ =INIDOC,LASDOC
                        DO KI =INIDOC,LASDOC
                          OVRLP2(KAE,KBF,ISTATE,JSTATE,0)=
     $                      OVRLP2(KAE,KBF,ISTATE,JSTATE,0)
     $                      -VTWOEL(KI,KAE,KJ,KBF)
     $                      *(2.0D+00*VTWOEL(KI,KAE,KJ,KBF)
     $                      -VTWOEL(KJ,KAE,KI,KBF)
     $                      )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)
     $                      +ESIDUMI)
     $                      /(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)
     $                      +ESIDUMJ)
     $                      *CACA
                        END DO
                      END DO
                    END DO
                  END DO
                END IF
              END IF
            END DO
          END DO
        END IF
C
      ELSE
C
        IF(ISWTCH.EQ.1 .OR. ISWTCH.EQ.3) THEN
          DO JSTATE=1,NSTATE
            S=0.0D+00
            T=0.0D+00
            DO I=1,NCSF
              IF(KREF(I)) THEN
                CACA=CASVEC(I,ISTATE)*CASVEC(I,JSTATE)
                IF(ABS(CACA).GE.THRGEN) THEN
                  ESI=ECONF(I)-EREF0(JSTATE)
                  DO KBF=ISTART,IEND
                    DO KAE=INIEXT,LASEXT
                      IF(IROT.NE.0 .AND.
     $                  KAE.GE.INIEXT .AND. KBF.GE.INIEXT) THEN
                        ESIDUM=0.0D+00
                      ELSE
                        ESIDUM=ESI
                      END IF
                      DO KJ =INIDOC,LASDOC
                        DO KI =INIDOC,LASDOC
                          EPART2(KAE,KBF,ISTATE,JSTATE,0)=
     $                      EPART2(KAE,KBF,ISTATE,JSTATE,0)
     $                      -VTWOEL(KI,KAE,KJ,KBF)
     $                      *(2.0D+00*VTWOEL(KI,KAE,KJ,KBF)
     $                      -VTWOEL(KJ,KAE,KI,KBF)
     $                      )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)
     $                      +ESI)
     $                      *CACA
                          EPART2(KAE,KBF,ISTATE,JSTATE,1)=
     $                      EPART2(KAE,KBF,ISTATE,JSTATE,1)
     $                      -VTWOEL(KI,KAE,KJ,KBF)
     $                      *(2.0D+00*VTWOEL(KI,KAE,KJ,KBF)
     $                      -VTWOEL(KJ,KAE,KI,KBF)
     $                      )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)
     $                      +ESIDUM)
     $                      *CACA
     $                      +VTWOEL(KI,KAE,KJ,KBF)
     $                      *(2.0D+00*VTWOEL(KI,KAE,KJ,KBF)
     $                      -VTWOEL(KJ,KAE,KI,KBF)
     $                      )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)
     $                      +ESI)
     $                      *CACA
                        END DO
                      END DO
                    END DO
                  END DO
                END IF
              END IF
            END DO
          END DO
        END IF
        IF(ISWTCH.EQ.2 .OR. ISWTCH.EQ.3) THEN
          DO JSTATE=1,ISTATE
            S=0.0D+00
            T=0.0D+00
            DO I=1,NCSF
              IF(KREF(I)) THEN
                CACA=CASVEC(I,ISTATE)*CASVEC(I,JSTATE)
                IF(ABS(CACA).GE.THRGEN) THEN
                  ESII=ECONF(I)-EREF0(ISTATE)
                  ESIJ=ECONF(I)-EREF0(JSTATE)
                  DO KBF=ISTART,IEND
                    DO KAE=INIEXT,LASEXT
                      IF(IROT.NE.0 .AND.
     $                  KAE.GE.INIEXT .AND. KBF.GE.INIEXT) THEN
                        ESIDUMI=0.0D+00
                        ESIDUMJ=0.0D+00
                      ELSE
                        ESIDUMI=ESII
                        ESIDUMJ=ESIJ
                      END IF
                      DO KJ =INIDOC,LASDOC
                        DO KI =INIDOC,LASDOC
                          OVRLP2(KAE,KBF,ISTATE,JSTATE,0)=
     $                      OVRLP2(KAE,KBF,ISTATE,JSTATE,0)
     $                      -VTWOEL(KI,KAE,KJ,KBF)
     $                      *(2.0D+00*VTWOEL(KI,KAE,KJ,KBF)
     $                      -VTWOEL(KJ,KAE,KI,KBF)
     $                      )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)
     $                      +ESII)
     $                      /(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)
     $                      +ESIJ)
     $                      *CACA
                          OVRLP2(KAE,KBF,ISTATE,JSTATE,1)=
     $                      OVRLP2(KAE,KBF,ISTATE,JSTATE,1)
     $                      -VTWOEL(KI,KAE,KJ,KBF)
     $                      *(2.0D+00*VTWOEL(KI,KAE,KJ,KBF)
     $                      -VTWOEL(KJ,KAE,KI,KBF)
     $                      )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)
     $                      +ESIDUMI)
     $                      /(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)
     $                      +ESIDUMJ)
     $                      *CACA
     $                      +VTWOEL(KI,KAE,KJ,KBF)
     $                      *(2.0D+00*VTWOEL(KI,KAE,KJ,KBF)
     $                      -VTWOEL(KJ,KAE,KI,KBF)
     $                      )/(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)
     $                      +ESII)
     $                      /(EORB(KAE)-EORB(KI)+EORB(KBF)-EORB(KJ)
     $                      +ESIJ)
     $                      *CACA
                        END DO
                      END DO
                    END DO
                  END DO
                END IF
              END IF
            END DO
          END DO
        END IF
C
      END IF
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** HEFF(2) <- ZERO-BODY GENERATOR ***'')')
        CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
        WRITE(LUNOUT,
     $    '('' *** HEFF(2) (ROT-2) <- ZERO-BODY GENERATOR ***'')')
        CALL MQWMAG(LUNOUT,HEFF(1,1,3),NSTATE,NSTATE,NSTATE,'    ')
      END IF
C.... CPU TIME .........................................................
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      IF(MASWRK) WRITE(LUNOUT,
     $     '('' CPU TIME FOR 0-BODY TERMS ='',F15.3,'' SEC.'')') TIME
C**** MP2 CASE *********************************************************
      IF(NMOACT.EQ.0) RETURN
C***********************************************************************
C**** ONE-BODY GENERATOR ***********************************************
C****                    ***********************************************
C.... CPU TIME .........................................................
      CALL TSECND(STIME)
C
  100 CONTINUE
        READ(LUNFT1) KCONT,LP,NWORD
        CALL MQGENR(LUNFT1,2*NWORD,NWORD,LABEL1,WORK)
        DO JSTATE=1,NSTATE
          VAL2(JSTATE)=0.0D+00
        END DO
        DO M=1,NWORD-1
          LQ =LABEL1(1,M)
          M2 =LABEL1(2,M)
          IO =IOCSF (1,M2)
          MP1=M+1
          LQNEXT=LABEL1(1,MP1)
          IONEXT=IOCSF (1,LABEL1(2,MP1))
          IF(LQ.EQ.LQNEXT .AND. IO.EQ.IONEXT) THEN
            IF(KREF(M2)) THEN
              DO JSTATE=1,NSTATE
                VAL2(JSTATE)=VAL2(JSTATE)+WORK(M)*CASVEC(M2,JSTATE)
              END DO
            END IF
          ELSE
            IF(KREF(M2)) THEN
              DO JSTATE=1,NSTATE
                VAL2(JSTATE)=VAL2(JSTATE)+WORK(M)*CASVEC(M2,JSTATE)
              END DO
            END IF
            KLOOP=.FALSE.
            DO JSTATE=1,NSTATE
              IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
            END DO
            IF(KLOOP) THEN
              ESI2=ECONF(M2)-EREF0(ISTATE)
              EQ=EORB(LQ)
C             EP=EORB(LP)
              S=0.0D+00
              DO KBF=ISTART,IEND
                ISPB=ISYM(MOSYM(LP),MOSYM(KBF))
                ISQB=ISYM(MOSYM(LQ),MOSYM(KBF))
                IF(ISPB.EQ.ISQB) THEN
                  DO KI =INIDOC,LASDOC
                    ISPBI=ISYM(ISPB,MOSYM(KI))
                    DO KAE=ISYLAB(1,ISPBI,4),ISYLAB(2,ISPBI,4)
C                   DO KAE=INIEXT,LASEXT
                      EAEBF2=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2
                      S=S-VTWOEL(KI,KAE,LP,KBF)
     $                  *( 2.0D+00*VTWOEL(KI,KAE,LQ,KBF)
     $                  -      VTWOEL(LQ,KAE,KI,KBF) )
     $                  /(EAEBF2-EQ)
                    END DO
                  END DO
                END IF
              END DO
              DO JSTATE=1,NSTATE
                IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
                  HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
     $              +S*VAL2(JSTATE)
                END IF
              END DO
            END IF
            DO JSTATE=1,NSTATE
              VAL2(JSTATE)=0.0D+00
            END DO
          END IF
        END DO
C
        IF(NWORD.NE.0) THEN
          M=NWORD
          LQ =LABEL1(1,M)
          M2 =LABEL1(2,M)
          IF(KREF(M2)) THEN
            DO JSTATE=1,NSTATE
              VAL2(JSTATE)=VAL2(JSTATE)+WORK(M)*CASVEC(M2,JSTATE)
            END DO
          END IF
          KLOOP=.FALSE.
          DO JSTATE=1,NSTATE
            IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
          END DO
          IF(KLOOP) THEN
            ESI2=ECONF(M2)-EREF0(ISTATE)
            EQ=EORB(LQ)
C           EP=EORB(LP)
            S=0.0D+00
            DO KBF=ISTART,IEND
              ISPB=ISYM(MOSYM(LP),MOSYM(KBF))
              ISQB=ISYM(MOSYM(LQ),MOSYM(KBF))
              IF(ISPB.EQ.ISQB) THEN
                DO KI =INIDOC,LASDOC
                  ISPBI=ISYM(ISPB,MOSYM(KI))
                  DO KAE=ISYLAB(1,ISPBI,4),ISYLAB(2,ISPBI,4)
C                 DO KAE=INIEXT,LASEXT
                    EAEBF2=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2
                    S=S-VTWOEL(KI,KAE,LP,KBF)
     $                *( 2.0D+00*VTWOEL(KI,KAE,LQ,KBF)
     $                -      VTWOEL(LQ,KAE,KI,KBF) )
     $                /(EAEBF2-EQ)
                  END DO
                END DO
              END IF
            END DO
            DO JSTATE=1,NSTATE
              IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
                HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
     $            +S*VAL2(JSTATE)
              END IF
            END DO
          END IF
        END IF
        IF(ISWTCH.EQ.1 .OR. ISWTCH.EQ.3) THEN
          DO M=1,NWORD
            LQ =LABEL1(1,M)
            M2 =LABEL1(2,M)
            IF(KREF(M2)) THEN
              EQ=EORB(LQ)
C             EP=EORB(LP)
              DO JSTATE=1,NSTATE
                VAL2OD=WORK(M)*CASVEC(M2,JSTATE)
                IF(ABS(VAL2OD).GE.THRGEN) THEN
                  ESI2=ECONF(M2)-EREF0(JSTATE)
                  S=0.0D+00
                  DO KBF=ISTART,IEND
                    DO KAE=INIEXT,LASEXT
                      DO KI =INIDOC,LASDOC
                        EAEBF2=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2
                        EPART2(KAE,KBF,ISTATE,JSTATE,0)=
     $                    EPART2(KAE,KBF,ISTATE,JSTATE,0)
     $                    -VTWOEL(KI,KAE,LP,KBF)
     $                    *( 2.0D+00*VTWOEL(KI,KAE,LQ,KBF)
     $                    -      VTWOEL(LQ,KAE,KI,KBF) )
     $                    /(EAEBF2-EQ)*VAL2OD
                      END DO
                    END DO
                  END DO
                END IF
              END DO
            END IF
          END DO
        END IF
        IF(ISWTCH.EQ.2 .OR. ISWTCH.EQ.3) THEN
          DO M=1,NWORD
            LQ =LABEL1(1,M)
            M2 =LABEL1(2,M)
            IF(KREF(M2)) THEN
              EQ=EORB(LQ)
C             EP=EORB(LP)
              DO JSTATE=1,ISTATE
                VAL2OD=WORK(M)*CASVEC(M2,JSTATE)
                IF(ABS(VAL2OD).GE.THRGEN) THEN
                  ESI2I=ECONF(M2)-EREF0(ISTATE)
                  ESI2J=ECONF(M2)-EREF0(JSTATE)
                  S=0.0D+00
                  DO KBF=ISTART,IEND
                    DO KAE=INIEXT,LASEXT
                      DO KI =INIDOC,LASDOC
                        EAEBF2I=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2I
                        EAEBF2J=EORB(KAE)+EORB(KBF)-EORB(KI)+ESI2J
                        OVRLP2(KAE,KBF,ISTATE,JSTATE,0)=
     $                    OVRLP2(KAE,KBF,ISTATE,JSTATE,0)
     $                    -VTWOEL(KI,KAE,LP,KBF)
     $                    *( 2.0D+00*VTWOEL(KI,KAE,LQ,KBF)
     $                    -      VTWOEL(LQ,KAE,KI,KBF) )
     $                    /(EAEBF2I-EQ)
     $                    /(EAEBF2J-EQ)*VAL2OD
                      END DO
                    END DO
                  END DO
                END IF
              END DO
            END IF
          END DO
        END IF
C.... GO TO READ .......................................................
      IF(KCONT.NE.0) GO TO 100
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** HEFF(2) <- ONE-BODY GENERATOR ***'')')
        CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
      END IF
C.... CPU TIME .........................................................
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      IF(MASWRK) WRITE(LUNOUT,
     $     '('' CPU TIME FOR 1-BODY TERMS ='',F15.3,'' SEC.'')') TIME
C***********************************************************************
C**** TWO-BODY GENERATOR ***********************************************
C****                    ***********************************************
C.... CPU TIME .........................................................
      CALL TSECND(STIME)
C
  102 CONTINUE
        READ(LUNFT2) KCONT,LP,LR,NWORD
        CALL MQGENR(LUNFT2,3*NWORD,NWORD,LABEL2,WORK)
        DO JSTATE=1,NSTATE
          VAL2(JSTATE)=0.0D+00
        END DO
        DO M=1,NWORD-1
          LQ =LABEL2(1,M)
          LS =LABEL2(2,M)
          M2 =LABEL2(3,M)
          IO =IOCSF (1,M2)
          MP1=M+1
          LQNEXT=LABEL2(1,MP1)
          LSNEXT=LABEL2(2,MP1)
          IONEXT=IOCSF (1,LABEL2(3,MP1))
          IF(LQ.EQ.LQNEXT .AND. LS.EQ.LSNEXT .AND. IO.EQ.IONEXT) THEN
            IF(KREF(M2)) THEN
              IF(LP.EQ.LR) THEN
                DO JSTATE=1,NSTATE
                  VAL2(JSTATE)=VAL2(JSTATE)
     $              +WORK(M)*0.5D+00*CASVEC(M2,JSTATE)
                END DO
              ELSE
                DO JSTATE=1,NSTATE
                  VAL2(JSTATE)=VAL2(JSTATE)
     $              +WORK(M)*CASVEC(M2,JSTATE)
                END DO
              END IF
            END IF
          ELSE
            IF(KREF(M2)) THEN
              IF(LP.EQ.LR) THEN
                DO JSTATE=1,NSTATE
                  VAL2(JSTATE)=VAL2(JSTATE)
     $              +WORK(M)*0.5D+00*CASVEC(M2,JSTATE)
                END DO
              ELSE
                DO JSTATE=1,NSTATE
                  VAL2(JSTATE)=VAL2(JSTATE)
     $              +WORK(M)*CASVEC(M2,JSTATE)
                END DO
              END IF
            END IF
            KLOOP=.FALSE.
            DO JSTATE=1,NSTATE
              IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
            END DO
            IF(KLOOP) THEN
              ESI2=ECONF(M2)-EREF0(ISTATE)
              S=0.0D+00
C             EP=EORB(LP)
              EQ=EORB(LQ)
C             ER=EORB(LR)
              ES=EORB(LS)
C     PQRS
C             EPR=EP+ER
              EQS=EQ+ES
C             ERS=ER-ES
C             EQR=EQ-ER
C             EPSR=EP-ES+ER
C             EQRS=EQ-ER+ES
C     RSPQ
C             ESQ=ES+EQ
C             EPQ=EP-EQ
C             ESP=ES-EP
C             ERQP=ER-EQ+EP
C             ESPQ=ES-EP+EQ
C
              ISPR=ISYM(MOSYM(LP),MOSYM(LR))
              ISQS=ISYM(MOSYM(LQ),MOSYM(LS))
              IF(ISPR.EQ.ISQS) THEN
                DO KF =ISTART,IEND
                  ISPRF=ISYM(ISPR,MOSYM(KF))
                  DO KAE=ISYLAB(1,ISPRF,4),ISYLAB(2,ISPRF,4)
C                 DO KAE=INIEXT,LASEXT
                    EKAEF2=EORB(KAE)+EORB(KF)+ESI2
                    S=S- VTWOEL(LP,KAE,LR,KF)
     $                *VTWOEL(LQ,KAE,LS,KF)
     $                /(EKAEF2-EQS)
C    &                 - VTWOEL(LR,KAE,LP,KF)
C    &                *VTWOEL(LS,KAE,LQ,KF)
C    &                /(EKAEF2-ESQ)*0.5D+00
                  END DO
                END DO
              END IF
              DO JSTATE=1,NSTATE
                IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
                  HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
     $              +S*VAL2(JSTATE)
                END IF
              END DO
            END IF
            DO JSTATE=1,NSTATE
              VAL2(JSTATE)=0.0D+00
            END DO
          END IF
        END DO
C
        IF(NWORD.NE.0) THEN
          M=NWORD
          LQ =LABEL2(1,M)
          LS =LABEL2(2,M)
          M2 =LABEL2(3,M)
          IF(KREF(M2)) THEN
            IF(LP.EQ.LR) THEN
              DO JSTATE=1,NSTATE
                VAL2(JSTATE)=VAL2(JSTATE)
     $            +WORK(M)*0.5D+00*CASVEC(M2,JSTATE)
              END DO
            ELSE
              DO JSTATE=1,NSTATE
                VAL2(JSTATE)=VAL2(JSTATE)+WORK(M)*CASVEC(M2,JSTATE)
              END DO
            END IF
          END IF
          KLOOP=.FALSE.
          DO JSTATE=1,NSTATE
            IF(ABS(VAL2(JSTATE)).GE.THRGEN) KLOOP=.TRUE.
          END DO
          IF(KLOOP) THEN
            ESI2=ECONF(M2)-EREF0(ISTATE)
            S=0.0D+00
C           EP=EORB(LP)
            EQ=EORB(LQ)
C           ER=EORB(LR)
            ES=EORB(LS)
C     PQRS
C           EPR=EP+ER
            EQS=EQ+ES
C           ERS=ER-ES
C           EQR=EQ-ER
C           EPSR=EP-ES+ER
C           EQRS=EQ-ER+ES
C     RSPQ
C           ESQ=ES+EQ
C           EPQ=EP-EQ
C           ESP=ES-EP
C           ERQP=ER-EQ+EP
C           ESPQ=ES-EP+EQ
            ISPR=ISYM(MOSYM(LP),MOSYM(LR))
            ISQS=ISYM(MOSYM(LQ),MOSYM(LS))
            IF(ISPR.EQ.ISQS) THEN
              DO KF =ISTART,IEND
                ISPRF=ISYM(ISPR,MOSYM(KF))
                DO KAE=ISYLAB(1,ISPRF,4),ISYLAB(2,ISPRF,4)
C               DO KAE=INIEXT,LASEXT
                  EKAEF2=EORB(KAE)+EORB(KF)+ESI2
                  S=S- VTWOEL(LP,KAE,LR,KF)
     $              *VTWOEL(LQ,KAE,LS,KF)
     $              /(EKAEF2-EQS)
C    &               - VTWOEL(LR,KAE,LP,KF)
C    &              *VTWOEL(LS,KAE,LQ,KF)
C    &              /(EKAEF2-ESQ)*0.5D+00
                END DO
              END DO
            END IF
            DO JSTATE=1,NSTATE
              IF(ABS(VAL2(JSTATE)).GE.THRGEN) THEN
                HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)
     $            +S*VAL2(JSTATE)
              END IF
            END DO
          END IF
        END IF
        IF(ISWTCH.EQ.1 .OR. ISWTCH.EQ.3) THEN
          DO M=1,NWORD
            LQ =LABEL2(1,M)
            LS =LABEL2(2,M)
            M2 =LABEL2(3,M)
            IF(KREF(M2)) THEN
              IF(LP.EQ.LR) THEN
                VAL=WORK(M)*0.5D+00
              ELSE
                VAL=WORK(M)
              END IF
              DO JSTATE=1,NSTATE
                VAL2OD=VAL*CASVEC(M2,JSTATE)
                IF(ABS(VAL2OD).GE.THRGEN) THEN
                  ESI2=ECONF(M2)-EREF0(JSTATE)
                  S=0.0D+00
                  EQ=EORB(LQ)
                  ES=EORB(LS)
                  EQS=EQ+ES
                  DO KF =ISTART,IEND
                    DO KAE=INIEXT,LASEXT
                      EKAEF2=EORB(KAE)+EORB(KF)+ESI2
                      EPART2(KAE,KF,ISTATE,JSTATE,0)=
     $                  EPART2(KAE,KF,ISTATE,JSTATE,0)
     $                  - VTWOEL(LP,KAE,LR,KF)
     $                  *VTWOEL(LQ,KAE,LS,KF)
     $                  /(EKAEF2-EQS)*VAL2OD
                    END DO
                  END DO
                END IF
              END DO
            END IF
          END DO
        END IF
        IF(ISWTCH.EQ.2 .OR. ISWTCH.EQ.3) THEN
          DO M=1,NWORD
            LQ =LABEL2(1,M)
            LS =LABEL2(2,M)
            M2 =LABEL2(3,M)
            IF(KREF(M2)) THEN
              IF(LP.EQ.LR) THEN
                VAL=WORK(M)*0.5D+00
              ELSE
                VAL=WORK(M)
              END IF
              DO JSTATE=1,ISTATE
                VAL2OD=VAL*CASVEC(M2,JSTATE)
                IF(ABS(VAL2OD).GE.THRGEN) THEN
                  ESI2I=ECONF(M2)-EREF0(ISTATE)
                  ESI2J=ECONF(M2)-EREF0(JSTATE)
                  S=0.0D+00
                  EQ=EORB(LQ)
                  ES=EORB(LS)
                  EQS=EQ+ES
                  DO KF =ISTART,IEND
                    DO KAE=INIEXT,LASEXT
                      EKAEF2I=EORB(KAE)+EORB(KF)+ESI2I
                      EKAEF2J=EORB(KAE)+EORB(KF)+ESI2J
                      OVRLP2(KAE,KF,ISTATE,JSTATE,0)=
     $                  OVRLP2(KAE,KF,ISTATE,JSTATE,0)
     $                  - VTWOEL(LP,KAE,LR,KF)
     $                  *VTWOEL(LQ,KAE,LS,KF)
     $                  /(EKAEF2I-EQS)
     $                  /(EKAEF2J-EQS)*VAL2OD
                    END DO
                  END DO
                END IF
              END DO
            END IF
          END DO
        END IF
C.... GO TO READ .......................................................
      IF(KCONT.NE.0) GO TO 102
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LE.-5 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** HEFF(2) <- TWO-BODY GENERATOR ***'')')
        CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
      END IF
C.... CPU TIME .........................................................
      CALL TSECND(ETIME)
      TIME=ETIME-STIME
      IF(MASWRK) WRITE(LUNOUT,
     $     '('' CPU TIME FOR 2-BODY TERMS ='',F15.3,'' SEC.'')') TIME
C***********************************************************************
C****            *******************************************************
C****   RETURN   *******************************************************
C****            *******************************************************
      RETURN
      END
C*MODULE MCQDWT  *DECK MQWVSM
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++ MQWVSM +++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MQWVSM(LUNOUT,LPOUT ,LUNIII,LUNFT0,
     $                  NMO   ,NMOACT,INIMO ,LASMO ,NMOAA ,NCSF  ,
     $                  NSTATE,MAXERI,MAXCSF,
     $                  HEFF  ,CASVEC,ECONF ,EREF0 ,LWCD  ,LABEL1,
     $                  WORKER,WORKGE,LIJMO ,HCORE ,VONEEL,VTWOEL,
     $                  VSML  ,KREF  ,
     $                  ISWTCH,EPART0,OVRLP0)
C=======================================================================
C====                                                              =====
C====             THIS ROUTINE WAS CODED BY H.NAKANO               =====
C====                                                              =====
C====     INTELLIGENT MODELING LABORATORY, UNIVERSITY OF TOKYO     =====
C====       2-11-16 YAYOI, BUNKYO-KU, TOKYO 113-8656, JAPAN        =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***********************************************************************
C****                     **********************************************
C**** ARRAYS AND CONSTANT **********************************************
C****                     **********************************************
      DIMENSION HEFF  (NSTATE,NSTATE,0:3)
      DIMENSION CASVEC(NCSF,NSTATE)
      DIMENSION ECONF (NCSF)    , EREF0 (NSTATE)
C:::: INTEGER*2 LWCD(2,MAXERI)
      DIMENSION LWCD(2,MAXERI)
      DIMENSION LABEL1(4,MAXCSF)
      DIMENSION WORKER(MAXERI), WORKGE(MAXCSF)
      DIMENSION LIJMO(INIMO:LASMO,INIMO:LASMO)
      DIMENSION HCORE(NMO,NMO), VONEEL(INIMO:LASMO,INIMO:LASMO)
      DIMENSION VTWOEL(NMOAA,NMOAA)
      DIMENSION VSML(NCSF,NSTATE)
      LOGICAL   KREF(NCSF)
      DIMENSION EPART0(NSTATE,NSTATE)
      DIMENSION OVRLP0(NSTATE,NSTATE)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C**** THRESHOLD ZERO ***************************************************
      THRZRO=1.0D-15
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQCACI ***'')')
        CALL MQDEBG(LUNOUT,
     $  'LUNIII','LUNFT0','   NMO','NMOACT',' INIMO',' LASMO',' NMOAA',
     $   LUNIII , LUNFT0 ,    NMO , NMOACT ,  INIMO ,  LASMO ,  NMOAA )
        CALL MQDEBG(LUNOUT,
     $  '  NCSF','NSTATE','MAXERI','MAXCSF','     -','     -','     -',
     $     NCSF , NSTATE , MAXERI , MAXCSF ,      0 ,      0 ,      0 )
      END IF
C**** CLEAR ARRAYS *****************************************************
      DO 104 J=INIMO,LASMO
        DO 102 I=INIMO,LASMO
          VONEEL(I,J)=0.0D+00
  102   CONTINUE
  104 CONTINUE
      DO 108 J=1,NMOAA
        DO 106 I=1,NMOAA
          VTWOEL(I,J)=0.0D+00
  106   CONTINUE
  108 CONTINUE
C**** PREPARE LIJMO ****************************************************
      N=0
      DO 112 I=INIMO,LASMO
        DO 110 J=INIMO,I
          N=N+1
          LIJMO(I,J)=N
          LIJMO(J,I)=N
  110   CONTINUE
  112 CONTINUE
C**** PREPARE PERTURBATION MATRICES ************************************
C
C     VTWOEL AND VONEEL
C
C      REWIND LUNIII
      CALL SEQREW(LUNIII)
      LUNIN=LUNIII
  114 READ(LUNIN) KCONT,IMOAB,JMOAB,NWORD
C:::: CALL MQGN2R(LUNIN,2*NWORD,NWORD,LWCD,WORKER)
      CALL MQGENR(LUNIN,2*NWORD,NWORD,LWCD,WORKER)
      DO 116 I=1,NWORD
        LP=IMOAB
        LQ=JMOAB
        LR=LWCD(1,I)
        LS=LWCD(2,I)
        V =WORKER(I)
        IF(LP.GE.INIMO .AND. LQ.GE.INIMO .AND.
     $     LR.LT.INIMO .AND. LS.LT.INIMO .AND. LR.EQ.LS) THEN
          IF(LP.NE.LQ) THEN
            VONEEL(LP,LQ)=VONEEL(LP,LQ)+2.0D+00*V
            VONEEL(LQ,LP)=VONEEL(LQ,LP)+2.0D+00*V
          ELSE
            VONEEL(LP,LQ)=VONEEL(LP,LQ)+2.0D+00*V
          END IF
        ELSE IF(LP.GE.INIMO .AND. LQ.LT.INIMO .AND.
     $          LR.GE.INIMO .AND. LS.LT.INIMO .AND. LQ.EQ.LS) THEN
          VONEEL(LP,LR)=VONEEL(LP,LR)-V
        ELSE IF(LP.GE.INIMO .AND. LQ.GE.INIMO .AND.
     $          LR.GE.INIMO .AND. LS.GE.INIMO) THEN
          VTWOEL(LIJMO(LP,LQ),LIJMO(LR,LS))=V
        END IF
  116 CONTINUE
      IF(KCONT.NE.0) GO TO 114
      DO 122 K=INIMO,LASMO
        DO 120 J=INIMO,LASMO
          DO 118 I=INIMO,LASMO
            VONEEL(I,J)=VONEEL(I,J)
     $        -0.5D+00*VTWOEL(LIJMO(I,K),LIJMO(K,J))
  118     CONTINUE
  120   CONTINUE
  122 CONTINUE
C**** DEBUG OUTPUT *****************************************************
      IF (MASWRK) THEN
      IF(LPOUT.LE.-10) THEN
        WRITE(LUNOUT,'('' *** MATRIX LIJMO ***'')')
        CALL MQWMAI(LUNOUT,LIJMO,NMOACT,NMOACT,NMOACT,'    ')
        WRITE(LUNOUT,'('' *** MATRIX HCORE ***'')')
        CALL MQWMAG(LUNOUT,HCORE,NMO,NMO,NMO,'TRIG')
        WRITE(LUNOUT,'('' *** MATRIX VONEEL ***'')')
        CALL MQWMAG(LUNOUT,VONEEL,NMOACT,NMOACT,NMOACT,'    ')
        IF(LPOUT.LE.-100) THEN
          WRITE(LUNOUT,'('' *** MATRIX VTWOEL ***'')')
          CALL MQWMAG(LUNOUT,VTWOEL,NMOAA,NMOAA,NMOAA,'    ')
        END IF
      END IF
C**** WRITE 2ND-ORDER EFFECTIVE HAMILTONIAN ****************************
      WRITE(LUNOUT,'('' ###   RESULTS   ###'')')
      WRITE(LUNOUT,1230)
 1230 FORMAT(' *** 2ND-ORDER EFFECTIVE HAMILTONIAN',
     $       ' (BEFORE SYMMETRIZATION) ***')
      CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
      WRITE(LUNOUT,'('' ===>'')')
      END IF
C**** CASVEC -> H*CASVEC ***********************************************
      DO 134 ISTATE=1,NSTATE
        DO 132 I=1,NCSF
          VSML(I,ISTATE)=0.0D+00
  132   CONTINUE
  134 CONTINUE
C      REWIND LUNFT0
      CALL SEQREW(LUNFT0)
  136 READ(LUNFT0) KCONT,NWORD
      CALL MQGENR(LUNFT0,4*NWORD,NWORD,LABEL1,WORKGE)
      DO 144 I=1,NWORD
        M1 =LABEL1(1,I)
        M2 =LABEL1(2,I)
        LP =LABEL1(3,I)
        LQ =LABEL1(4,I)
        VALI=WORKGE(I)
        V=(HCORE(LP,LQ)+VONEEL(LP,LQ))*VALI
        IF(.NOT.KREF(M1) .AND. KREF(M2)) THEN
          DO 138 ISTATE=1,NSTATE
            VSML(M1,ISTATE)=VSML(M1,ISTATE)+V*CASVEC(M2,ISTATE)
  138     CONTINUE
        END IF
        DO 142 J=1,NWORD
C         M3 =LABEL1(1,J)
          M4 =LABEL1(2,J)
          LR =LABEL1(3,J)
          LS =LABEL1(4,J)
          VALJ=WORKGE(J)
          V=0.5D+00*VTWOEL(LIJMO(LQ,LP),LIJMO(LR,LS))*VALI*VALJ
          IF(.NOT.KREF(M2) .AND. KREF(M4)) THEN
            DO 140 ISTATE=1,NSTATE
              VSML(M2,ISTATE)=VSML(M2,ISTATE)+V*CASVEC(M4,ISTATE)
  140       CONTINUE
          END IF
  142   CONTINUE
  144 CONTINUE
      IF(KCONT.NE.0) GO TO 136
C**** CALCULATE SECOND ORDER CONTRIBUTION TO HEFF **********************
      DO 150 ISTATE=1,NSTATE
        DO 148 JSTATE=1,NSTATE
          DO 146 ICSF=1,NCSF
            IF(.NOT.KREF(ICSF)) THEN
              VI=VSML(ICSF,ISTATE)
              VJ=VSML(ICSF,JSTATE)
              IF(ABS(VI).GE.THRZRO .AND. ABS(VJ).GE.THRZRO) THEN
                HEFF(ISTATE,JSTATE,2)=HEFF(ISTATE,JSTATE,2)+
     $          VI*VJ/(EREF0(JSTATE)-ECONF(ICSF))
              END IF
            END IF
  146     CONTINUE
  148   CONTINUE
  150 CONTINUE
      IF(ISWTCH.EQ.1 .OR. ISWTCH.EQ.3) THEN
        DO ISTATE=1,NSTATE
          DO JSTATE=1,NSTATE
            DO ICSF=1,NCSF
              IF(.NOT.KREF(ICSF)) THEN
                VI=VSML(ICSF,ISTATE)
                VJ=VSML(ICSF,JSTATE)
                IF(ABS(VI).GE.THRZRO .AND. ABS(VJ).GE.THRZRO) THEN
C
C     NOTE: OVRLPN'S HAVE INVERSE SIGN.
C
                  EPART0(ISTATE,JSTATE)=EPART0(ISTATE,JSTATE)+
     $              VI*VJ/(EREF0(JSTATE)-ECONF(ICSF))
                END IF
              END IF
            END DO
          END DO
        END DO
      END IF
      IF(ISWTCH.EQ.2 .OR. ISWTCH.EQ.3) THEN
        DO ISTATE=1,NSTATE
          DO JSTATE=1,ISTATE
            DO ICSF=1,NCSF
              IF(.NOT.KREF(ICSF)) THEN
                VI=VSML(ICSF,ISTATE)
                VJ=VSML(ICSF,JSTATE)
                IF(ABS(VI).GE.THRZRO .AND. ABS(VJ).GE.THRZRO) THEN
C
C     NOTE: OVRLPN'S HAVE INVERSE SIGN.
C
                  OVRLP0(ISTATE,JSTATE)=OVRLP0(ISTATE,JSTATE)-
     $              VI*VJ/(EREF0(ISTATE)-ECONF(ICSF))
     $              /(EREF0(JSTATE)-ECONF(ICSF))
                END IF
              END IF
            END DO
          END DO
        END DO
      END IF
C**** WRITE 2ND-ORDER EFFECTIVE HAMILTONIAN ****************************
      IF (MASWRK)
     *     CALL MQWMAG(LUNOUT,HEFF(1,1,2),NSTATE,NSTATE,NSTATE,'    ')
C**** RETURN ***********************************************************
      RETURN
      END
C*MODULE MCQDWT  *DECK MQGETW
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++ MQGETW +++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MQGETW(LUNOUT,LPOUT ,
     $                  NSTATE,IROT  ,
     $                  INIACT,INIEXT,NMO   ,
     $                  ISWTCH,
     $                  EPART0,EPART1,EPART2,OVRLP0,OVRLP1,OVRLP2,
     $                  EPARTN,OVRLAP,OVRPER,EPAR21,OVRL21)
C=======================================================================
C====                                                              =====
C====             THIS ROUTINE WAS CODED BY H.NAKANO               =====
C====                                                              =====
C====     INTELLIGENT MODELING LABORATORY, UNIVERSITY OF TOKYO     =====
C====       2-11-16 YAYOI, BUNKYO-KU, TOKYO 113-8656, JAPAN        =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C***********************************************************************
C****                         ******************************************
C****   ARRAYS AND CONSTANT   ******************************************
C****                         ******************************************
      DIMENSION EPART0(NSTATE,NSTATE)
      DIMENSION EPART1(INIACT:NMO,NSTATE,NSTATE)
      DIMENSION EPART2(INIACT:NMO,INIACT:NMO,NSTATE,NSTATE,0:1)
      DIMENSION OVRLP0(NSTATE,NSTATE)
      DIMENSION OVRLP1(INIACT:NMO,NSTATE,NSTATE)
      DIMENSION OVRLP2(INIACT:NMO,INIACT:NMO,NSTATE,NSTATE,0:1)
C
      DIMENSION EPARTN(NSTATE,NSTATE,0:5)
      DIMENSION OVRLAP(NSTATE,NSTATE,0:5), OVRPER(NSTATE,0:1)
C
      DIMENSION EPAR21(INIACT:NMO,NSTATE,NSTATE)
      DIMENSION OVRL21(INIACT:NMO,NSTATE,NSTATE)
      DIMENSION LABMAX(20),AMAX(10)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C**** DEBUG OUTPUT *****************************************************
      IF(LPOUT.LT.0 .AND. MASWRK) THEN
        WRITE(LUNOUT,'('' *** DEBUG OUTPUT IN SUB.MQGETW ***'')')
        CALL MQDEBG(LUNOUT,
     $  'NSTATE','  IROT','INIACT','INIEXT','   NMO','ISWTCH','     -',
     $   NSTATE ,   IROT , INIACT , INIEXT ,    NMO , ISWTCH ,      0 )
      END IF
C**** SET PARAMETERS ***************************************************
      LASACT=INIEXT-1
      LASEXT=NMO
      NMOEXT=LASEXT-LASACT
      IF(ISWTCH.EQ.1 .OR. ISWTCH.EQ.3) THEN
C**** INITIALIZE OVRLAP ************************************************
      DO JSTATE=1,NSTATE
        DO ISTATE=1,NSTATE
          EPARTN(ISTATE,JSTATE,0)=0.0D+00
          EPARTN(ISTATE,JSTATE,1)=0.0D+00
          EPARTN(ISTATE,JSTATE,2)=0.0D+00
          EPARTN(ISTATE,JSTATE,3)=0.0D+00
          EPARTN(ISTATE,JSTATE,4)=0.0D+00
          EPARTN(ISTATE,JSTATE,5)=0.0D+00
C@@@@
          DO I=INIACT,NMO
            EPAR21(I,ISTATE,JSTATE)=0.0D+00
            OVRL21(I,ISTATE,JSTATE)=0.0D+00
          END DO
C@@@@
        END DO
      END DO
C**** GET FINAL RESULTS! ***********************************************
      DO ISTATE=1,NSTATE
        DO JSTATE=1,NSTATE
C.... OVRLP1 -> OVRLP0 .................................................
          DO I=INIACT,LASACT
            EPART0(ISTATE,JSTATE)=EPART0(ISTATE,JSTATE)
     $        +EPART1(I,ISTATE,JSTATE)
          END DO
C.... OVRLP2 -> OVRLP0 .................................................
          DO J=INIACT,LASACT
            DO I=INIACT,LASACT
              EPART0(ISTATE,JSTATE)=EPART0(ISTATE,JSTATE)
     $          +EPART2(I,J,ISTATE,JSTATE,0)
            END DO
          END DO
C.... OVRLP2 -> OVRLP1 .................................................
          DO J=INIACT,LASACT
            DO I=INIEXT,LASEXT
              EPART1(I,ISTATE,JSTATE)=EPART1(I,ISTATE,JSTATE)
     $          +EPART2(I,J,ISTATE,JSTATE,0)
     $          +EPART2(J,I,ISTATE,JSTATE,0)
            END DO
          END DO
C.... I >= J ...........................................................
          DO I=INIEXT,LASEXT
            DO J=INIEXT,I-1
              EPART2(I,J,ISTATE,JSTATE,0)=
     $          EPART2(I,J,ISTATE,JSTATE,0)+
     $          EPART2(J,I,ISTATE,JSTATE,0)
            END DO
          END DO
C.... FOR IROT=2 .......................................................
          IF(IROT.EQ.2) THEN
            DO I=INIEXT,LASEXT
              DO J=INIEXT,I-1
                EPART2(I,J,ISTATE,JSTATE,1)=
     $            EPART2(I,J,ISTATE,JSTATE,1)+
     $            EPART2(J,I,ISTATE,JSTATE,1)
              END DO
            END DO
            DO I=INIEXT,LASEXT
              DO J=INIEXT,I
                EPART2(I,J,ISTATE,JSTATE,1)=
     $            EPART2(I,J,ISTATE,JSTATE,0)+
     $            EPART2(I,J,ISTATE,JSTATE,1)
              END DO
            END DO
          END IF
C**** GET INTERNAL, SEMI-INTERNAL, EXTERNAL AND TOTAL OVRLAP ***********
C
C     OVRLP(*,*,0) INTERNAL
C     OVRLP(*,*,1) SEMI-INTERNAL
C     OVRLP(*,*,2) EXTERNAL
C     OVRLP(*,*,3) TOTAL
C     OVRLP(*,*,4) EXTERNAL (IROT=2)
C     OVRLP(*,*,5) TOTAL    (IROT=2)
C
          EPARTN(ISTATE,JSTATE,0)=EPART0(ISTATE,JSTATE)
          DO I=INIEXT,LASEXT
            EPARTN(ISTATE,JSTATE,1)=EPARTN(ISTATE,JSTATE,1)
     $        +EPART1(I,ISTATE,JSTATE)
            DO J=INIEXT,I
              EPARTN(ISTATE,JSTATE,2)=EPARTN(ISTATE,JSTATE,2)
     $          +EPART2(I,J,ISTATE,JSTATE,0)
            END DO
          END DO
          IF(IROT.EQ.2) THEN
            DO I=INIEXT,LASEXT
              DO J=INIEXT,I
                EPARTN(ISTATE,JSTATE,4)=EPARTN(ISTATE,JSTATE,4)
     $            +EPART2(I,J,ISTATE,JSTATE,1)
              END DO
            END DO
          END IF
          EPARTN(ISTATE,JSTATE,3)=
     $      EPARTN(ISTATE,JSTATE,0)+
     $      EPARTN(ISTATE,JSTATE,1)+
     $      EPARTN(ISTATE,JSTATE,2)
          IF(IROT.EQ.2) THEN
            EPARTN(ISTATE,JSTATE,5)=
     $        EPARTN(ISTATE,JSTATE,0)+
     $        EPARTN(ISTATE,JSTATE,1)+
     $        EPARTN(ISTATE,JSTATE,4)
          END IF
C@@@@
          DO I=INIEXT,LASEXT
            DO J=INIEXT,I
              S=0.5D0*EPART2(I,J,ISTATE,JSTATE,0)
              EPAR21(I,ISTATE,JSTATE)=EPAR21(I,ISTATE,JSTATE)+S
              EPAR21(J,ISTATE,JSTATE)=EPAR21(J,ISTATE,JSTATE)+S
            END DO
          END DO
C@@@@
        END DO
      END DO
C***********************************************************************
C****               ****************************************************
C**** WRITE RESULTS ****************************************************
C****               ****************************************************
      IF (MASWRK) THEN
      WRITE(LUNOUT,'('' ###   RESULTS   ###'')')
      WRITE(LUNOUT,'('' *** ENERGY PARTITIONING ***'')')
      WRITE(LUNOUT,'('' INTERNAL'')')
      CALL MQWMAG(LUNOUT,EPARTN(1,1,0),NSTATE,NSTATE,NSTATE,'SYMM')
      WRITE(LUNOUT,'('' SEMI-INTERNAL'')')
      CALL MQWMAG(LUNOUT,EPARTN(1,1,1),NSTATE,NSTATE,NSTATE,'SYMM')
      WRITE(LUNOUT,'('' EXTERNAL'')')
      CALL MQWMAG(LUNOUT,EPARTN(1,1,2),NSTATE,NSTATE,NSTATE,'SYMM')
      WRITE(LUNOUT,'('' TOTAL'')')
      CALL MQWMAG(LUNOUT,EPARTN(1,1,3),NSTATE,NSTATE,NSTATE,'SYMM')
      IF(IROT.EQ.2) THEN
        WRITE(LUNOUT,'('' EXTERNAL (IROT)'')')
        CALL MQWMAG(LUNOUT,EPARTN(1,1,4),NSTATE,NSTATE,NSTATE,'SYMM')
        WRITE(LUNOUT,'('' TOTAL (IROT)'')')
        CALL MQWMAG(LUNOUT,EPARTN(1,1,5),NSTATE,NSTATE,NSTATE,'SYMM')
      END IF
C
      WRITE(LUNOUT,'('' *** ENERGY PARTITIONING (ANALYSIS) ***'')')
 1220 FORMAT(' ******** (ISTATE,JSTATE)=(' ,I2,',',I2,')')
      NWRIT1=MIN0(10,NMOEXT)
      DO ISTATE=1,NSTATE
        DO JSTATE=1,ISTATE
          WRITE(LUNOUT,1220) ISTATE,JSTATE
          WRITE(LUNOUT,'('' EPART0 ='',1P,D10.3)') EPART0(ISTATE,JSTATE)
          WRITE(LUNOUT,'('' EPART1'')')
          CALL MQMXO1(LUNOUT,INIACT,INIEXT,LASEXT,NWRIT1,
     $      LABMAX,EPART1(INIACT,ISTATE,JSTATE),AMAX)
          WRITE(LUNOUT,'('' EPART2'')')
          CALL MQMXO2(LUNOUT,INIACT,INIEXT,LASEXT,NWRIT1,
     $      LABMAX,EPART2(INIACT,INIACT,ISTATE,JSTATE,0),AMAX)
C@@@@
          WRITE(LUNOUT,'('' REDUCED EPART2'')')
          CALL MQMXO1(LUNOUT,INIACT,INIEXT,LASEXT,NWRIT1,
     $      LABMAX,EPAR21(INIACT,ISTATE,JSTATE),AMAX)
C@@@@
          IF(IROT.EQ.2) THEN
            WRITE(LUNOUT,'('' EPART2 (IROT)'')')
            CALL MQMXO2(LUNOUT,INIACT,INIEXT,LASEXT,NWRIT1,
     $        LABMAX,EPART2(INIACT,INIACT,ISTATE,JSTATE,1),AMAX)
          END IF
        END DO
      END DO
      END IF
      END IF
      IF(ISWTCH.EQ.2 .OR. ISWTCH.EQ.3) THEN
C**** INITIALIZE OVRLAP ************************************************
      DO JSTATE=1,NSTATE
        DO ISTATE=1,NSTATE
          OVRLAP(ISTATE,JSTATE,0)=0.0D+00
          OVRLAP(ISTATE,JSTATE,1)=0.0D+00
          OVRLAP(ISTATE,JSTATE,2)=0.0D+00
          OVRLAP(ISTATE,JSTATE,3)=0.0D+00
          OVRLAP(ISTATE,JSTATE,4)=0.0D+00
          OVRLAP(ISTATE,JSTATE,5)=0.0D+00
        END DO
      END DO
C**** GET FINAL RESULTS! ***********************************************
      DO ISTATE=1,NSTATE
        DO JSTATE=1,ISTATE
C.... OVRLP1 -> OVRLP0 .................................................
          DO I=INIACT,LASACT
            OVRLP0(ISTATE,JSTATE)=OVRLP0(ISTATE,JSTATE)
     $        +OVRLP1(I,ISTATE,JSTATE)
          END DO
C.... OVRLP2 -> OVRLP0 .................................................
          DO J=INIACT,LASACT
            DO I=INIACT,LASACT
              OVRLP0(ISTATE,JSTATE)=OVRLP0(ISTATE,JSTATE)
     $          +OVRLP2(I,J,ISTATE,JSTATE,0)
            END DO
          END DO
C.... OVRLP2 -> OVRLP1 .................................................
          DO J=INIACT,LASACT
            DO I=INIEXT,LASEXT
              OVRLP1(I,ISTATE,JSTATE)=OVRLP1(I,ISTATE,JSTATE)
     $          +OVRLP2(I,J,ISTATE,JSTATE,0)
     $          +OVRLP2(J,I,ISTATE,JSTATE,0)
            END DO
          END DO
C.... I >= J ...........................................................
          DO I=INIEXT,LASEXT
            DO J=INIEXT,I-1
              OVRLP2(I,J,ISTATE,JSTATE,0)=
     $          OVRLP2(I,J,ISTATE,JSTATE,0)+
     $          OVRLP2(J,I,ISTATE,JSTATE,0)
            END DO
          END DO
C.... FOR IROT=2 .......................................................
          IF(IROT.EQ.2) THEN
            DO I=INIEXT,LASEXT
              DO J=INIEXT,I-1
                OVRLP2(I,J,ISTATE,JSTATE,1)=
     $            OVRLP2(I,J,ISTATE,JSTATE,1)+
     $            OVRLP2(J,I,ISTATE,JSTATE,1)
              END DO
            END DO
            DO I=INIEXT,LASEXT
              DO J=INIEXT,I
                OVRLP2(I,J,ISTATE,JSTATE,1)=
     $            OVRLP2(I,J,ISTATE,JSTATE,0)+
     $            OVRLP2(I,J,ISTATE,JSTATE,1)
              END DO
            END DO
          END IF
C.... ADJUST SIGN ......................................................
          OVRLP0(ISTATE,JSTATE)=-OVRLP0(ISTATE,JSTATE)
          DO I=INIEXT,LASEXT
            OVRLP1(I,ISTATE,JSTATE)=-OVRLP1(I,ISTATE,JSTATE)
            DO J=INIEXT,I
              OVRLP2(I,J,ISTATE,JSTATE,0)=-OVRLP2(I,J,ISTATE,JSTATE,0)
            END DO
          END DO
          IF(IROT.EQ.2) THEN
            DO I=INIEXT,LASEXT
              DO J=INIEXT,I
                OVRLP2(I,J,ISTATE,JSTATE,1)=-OVRLP2(I,J,ISTATE,JSTATE,1)
              END DO
            END DO
          END IF
C**** GET INTERNAL, SEMI-INTERNAL, EXTERNAL AND TOTAL OVRLAP ***********
C
C     OVRLP(*,*,0) INTERNAL
C     OVRLP(*,*,1) SEMI-INTERNAL
C     OVRLP(*,*,2) EXTERNAL
C     OVRLP(*,*,3) TOTAL
C     OVRLP(*,*,4) EXTERNAL (IROT=2)
C     OVRLP(*,*,5) TOTAL    (IROT=2)
C
          OVRLAP(ISTATE,JSTATE,0)=OVRLP0(ISTATE,JSTATE)
          DO I=INIEXT,LASEXT
            OVRLAP(ISTATE,JSTATE,1)=OVRLAP(ISTATE,JSTATE,1)
     $        +OVRLP1(I,ISTATE,JSTATE)
            DO J=INIEXT,I
              OVRLAP(ISTATE,JSTATE,2)=OVRLAP(ISTATE,JSTATE,2)
     $          +OVRLP2(I,J,ISTATE,JSTATE,0)
            END DO
          END DO
          IF(IROT.EQ.2) THEN
            DO I=INIEXT,LASEXT
              DO J=INIEXT,I
                OVRLAP(ISTATE,JSTATE,4)=OVRLAP(ISTATE,JSTATE,4)
     $            +OVRLP2(I,J,ISTATE,JSTATE,1)
              END DO
            END DO
          END IF
          OVRLAP(ISTATE,JSTATE,3)=
     $      OVRLAP(ISTATE,JSTATE,0)+
     $      OVRLAP(ISTATE,JSTATE,1)+
     $      OVRLAP(ISTATE,JSTATE,2)
          IF(IROT.EQ.2) THEN
            OVRLAP(ISTATE,JSTATE,5)=
     $        OVRLAP(ISTATE,JSTATE,0)+
     $        OVRLAP(ISTATE,JSTATE,1)+
     $        OVRLAP(ISTATE,JSTATE,4)
          END IF
C@@@@
          DO I=INIEXT,LASEXT
            DO J=INIEXT,I
              S=0.5D0*OVRLP2(I,J,ISTATE,JSTATE,0)
              OVRL21(I,ISTATE,JSTATE)=OVRL21(I,ISTATE,JSTATE)+S
              OVRL21(J,ISTATE,JSTATE)=OVRL21(J,ISTATE,JSTATE)+S
            END DO
          END DO
C@@@@
        END DO
      END DO
C**** GET REFERENCE PERCENTAGE *****************************************
      DO I=1,NSTATE
        OVRPER(I,0)=100.0D+00/(1.0D+00+OVRLAP(I,I,3))
      END DO
      IF(IROT.EQ.2) THEN
        DO I=1,NSTATE
          OVRPER(I,1)=100.0D+00/(1.0D+00+OVRLAP(I,I,5))
        END DO
      END IF
C***********************************************************************
C****               ****************************************************
C**** WRITE RESULTS ****************************************************
C****               ****************************************************
      IF (MASWRK) THEN
      WRITE(LUNOUT,'('' ###   RESULTS   ###'')')
      WRITE(LUNOUT,'('' *** WAVEFUNCTION OVERLAP ***'')')
      WRITE(LUNOUT,'('' INTERNAL'')')
      CALL MQWMAG(LUNOUT,OVRLAP(1,1,0),NSTATE,NSTATE,NSTATE,'SYMM')
      WRITE(LUNOUT,'('' SEMI-INTERNAL'')')
      CALL MQWMAG(LUNOUT,OVRLAP(1,1,1),NSTATE,NSTATE,NSTATE,'SYMM')
      WRITE(LUNOUT,'('' EXTERNAL'')')
      CALL MQWMAG(LUNOUT,OVRLAP(1,1,2),NSTATE,NSTATE,NSTATE,'SYMM')
      WRITE(LUNOUT,'('' TOTAL'')')
      CALL MQWMAG(LUNOUT,OVRLAP(1,1,3),NSTATE,NSTATE,NSTATE,'SYMM')
      WRITE(LUNOUT,'('' REFERENCE WEIGHT'')')
      INIT=1
  100 LAST=MIN(INIT+7,NSTATE)
      WRITE(LUNOUT,'(1X,71(1H-)/3X,8(4X,I4))') (J,J=INIT,LAST)
      WRITE(LUNOUT,'(3X,8(2X,F4.1,'' %''))') (OVRPER(J,0),J=INIT,LAST)
      INIT=LAST+1
      IF(INIT.LE.NSTATE) GO TO 100
      WRITE(LUNOUT,'(1X,71(1H-))')
C     CALL MQWMAG(LUNOUT,OVRPER(1,0),1,1,NSTATE,'    ')
      IF(IROT.EQ.2) THEN
        WRITE(LUNOUT,'('' EXTERNAL (IROT)'')')
        CALL MQWMAG(LUNOUT,OVRLAP(1,1,4),NSTATE,NSTATE,NSTATE,'SYMM')
        WRITE(LUNOUT,'('' TOTAL (IROT)'')')
        CALL MQWMAG(LUNOUT,OVRLAP(1,1,5),NSTATE,NSTATE,NSTATE,'SYMM')
        WRITE(LUNOUT,'('' REFERENCE WEIGHT (IROT)'')')
        INIT=1
  102   LAST=MIN(INIT+7,NSTATE)
        WRITE(LUNOUT,'(1X,71(1H-)/3X,8(4X,I4))') (J,J=INIT,LAST)
        WRITE(LUNOUT,'(3X,8(2X,F4.1,'' %''))') (OVRPER(J,1),J=INIT,LAST)
        INIT=LAST+1
        IF(INIT.LE.NSTATE) GO TO 102
        WRITE(LUNOUT,'(1X,71(1H-))')
C       CALL MQWMAG(LUNOUT,OVRPER(1,1),1,1,NSTATE,'    ')
      END IF
C
      WRITE(LUNOUT,'('' *** WAVEFUNCTION OVERLAP (ANALYSIS) ***'')')
      NWRIT1=MIN0(10,NMOEXT)
 1210 FORMAT(' ******** (ISTATE,JSTATE)=(' ,I2,',',I2,')')
      DO ISTATE=1,NSTATE
        DO JSTATE=1,ISTATE
          WRITE(LUNOUT,1210) ISTATE,JSTATE
          WRITE(LUNOUT,'('' OVRLP0 ='',1P,D10.3)') OVRLP0(ISTATE,JSTATE)
          WRITE(LUNOUT,'('' OVRLP1'')')
          CALL MQMXO1(LUNOUT,INIACT,INIEXT,LASEXT,NWRIT1,
     $      LABMAX,OVRLP1(INIACT,ISTATE,JSTATE),AMAX)
          WRITE(LUNOUT,'('' OVRLP2'')')
          CALL MQMXO2(LUNOUT,INIACT,INIEXT,LASEXT,NWRIT1,
     $      LABMAX,OVRLP2(INIACT,INIACT,ISTATE,JSTATE,0),AMAX)
C@@@@
          WRITE(LUNOUT,'('' REDUCED OVRLP2'')')
          CALL MQMXO1(LUNOUT,INIACT,INIEXT,LASEXT,NWRIT1,
     $      LABMAX,OVRL21(INIACT,ISTATE,JSTATE),AMAX)
C@@@@
          IF(IROT.EQ.2) THEN
            WRITE(LUNOUT,'('' OVRLP2 (IROT)'')')
            CALL MQMXO2(LUNOUT,INIACT,INIEXT,LASEXT,NWRIT1,
     $        LABMAX,OVRLP2(INIACT,INIACT,ISTATE,JSTATE,1),AMAX)
          END IF
        END DO
      END DO
      END IF
      END IF
C**** RETURN AND END ***************************************************
      RETURN
      END
C*MODULE MCQDWT  *DECK MQMXO1
      SUBROUTINE MQMXO1(LUNOUT,INIACT,INIEXT,LASEXT,NMAX  ,
     $                  LABMAX,A     ,AMAX  )
C=======================================================================
C====                                                              =====
C====             THIS ROUTINE WAS CODED BY H.NAKANO               =====
C====                                                              =====
C====     INTELLIGENT MODELING LABORATORY, UNIVERSITY OF TOKYO     =====
C====       2-11-16 YAYOI, BUNKYO-KU, TOKYO 113-8656, JAPAN        =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
C**** DIMENSION ********************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION LABMAX(NMAX)
      DIMENSION A     (INIACT:LASEXT) ,AMAX (NMAX)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C**** GET LABMAX *******************************************************
      S=-1.0D+00
      DO I=INIEXT,LASEXT
        AG=ABS(A(I))
        IF(AG.GT.S) THEN
          S   =AG
          IMAX=I
        END IF
      END DO
      AMAX  (1)=A(IMAX)
      LABMAX(1)=IMAX
      DO K=2,NMAX
        S=-1.0D+00
        AGMBEF=ABS(AMAX(K-1))
        DO I=INIEXT,LASEXT
          AG=ABS(A(I))
          IF((AGMBEF.GT.AG).AND.(AG.GT.S)) THEN
            S   =AG
            IMAX=I
          END IF
        END DO
        AMAX  (K)=A(IMAX)
        LABMAX(K)=IMAX
      END DO
C**** WRITING RESULT ***************************************************
      IF (MASWRK) THEN
      INILAB=1
  300 LASLAB=INILAB+4
      IF(LASLAB.GT.NMAX) LASLAB=NMAX
      WRITE(LUNOUT,'(1X,71(1H.)/5X,5(10X,I2))') (I,I=INILAB,LASLAB)
      WRITE(LUNOUT,'(1X,71(1H.))')
      WRITE(LUNOUT,'(3X,''  LABEL '',5(2X,I10))')
     $  (LABMAX(I),I=INILAB,LASLAB)
      WRITE(LUNOUT,'(3X,''  VALUE '',1P,5(2X,D10.3))')
     $  (AMAX  (I),I=INILAB,LASLAB)
      INILAB=LASLAB+1
      IF(INILAB.LE.NMAX) GO TO 300
      WRITE(LUNOUT,'(1X,71(1H.))')
      END IF
C**** RETURN AND END ***************************************************
      RETURN
      END
C*MODULE MCQDWT  *DECK MQMXO2
      SUBROUTINE MQMXO2(LUNOUT,INIACT,INIEXT,LASEXT,NMAX  ,
     $                  LABMAX,A     ,AMAX )
C=======================================================================
C====                                                              =====
C====             THIS ROUTINE WAS CODED BY H.NAKANO               =====
C====                                                              =====
C====     INTELLIGENT MODELING LABORATORY, UNIVERSITY OF TOKYO     =====
C====       2-11-16 YAYOI, BUNKYO-KU, TOKYO 113-8656, JAPAN        =====
C====             E-MAIL: NAKANO@QCL.T.U-TOKYO.AC.JP               =====
C====                                                              =====
C=======================================================================
C**** DIMENSION ********************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION LABMAX(NMAX,2)
      DIMENSION A     (INIACT:LASEXT,INIACT:LASEXT) ,AMAX (NMAX)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C**** GET LABMAX *******************************************************
      S=-1.0D+00
      DO I=INIEXT,LASEXT
        DO J=INIEXT,I
          AG=ABS(A(I,J))
          IF(AG.GT.S) THEN
            S   =AG
            IMAX=I
            JMAX=J
          END IF
        END DO
      END DO
      AMAX  (1)=A(IMAX,JMAX)
      LABMAX(1,1)=IMAX
      LABMAX(1,2)=JMAX
      DO K=2,NMAX
        S=-1.0D+00
        AGMBEF=ABS(AMAX(K-1))
        DO I=INIEXT,LASEXT
          DO J=INIEXT,I
            AG=ABS(A(I,J))
            IF((AGMBEF.GT.AG).AND.(AG.GT.S)) THEN
              S   =AG
              IMAX=I
              JMAX=J
            END IF
          END DO
        END DO
        AMAX  (K)=A(IMAX,JMAX)
        LABMAX(K,1)=IMAX
        LABMAX(K,2)=JMAX
      END DO
C**** WRITING RESULT ***************************************************
      IF (MASWRK) THEN
      INILAB=1
  300 LASLAB=INILAB+4
      IF(LASLAB.GT.NMAX) LASLAB=NMAX
      WRITE(LUNOUT,'(1X,71(1H.)/5X,5(10X,I2))') (I,I=INILAB,LASLAB)
      WRITE(LUNOUT,'(1X,71(1H.))')
      WRITE(LUNOUT,'(3X,''  LABEL '',5(2X,I10))')
     $  (LABMAX(I,1),I=INILAB,LASLAB)
      WRITE(LUNOUT,'(3X,''        '',5(2X,I10))')
     $  (LABMAX(I,2),I=INILAB,LASLAB)
      WRITE(LUNOUT,'(3X,''  VALUE '',1P,5(2X,D10.3))')
     $  (AMAX  (I),I=INILAB,LASLAB)
      INILAB=LASLAB+1
      IF(INILAB.LE.NMAX) GO TO 300
      WRITE(LUNOUT,'(1X,71(1H.))')
      END IF
C**** RETURN AND END ***************************************************
      RETURN
      END
