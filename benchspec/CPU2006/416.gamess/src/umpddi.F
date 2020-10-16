C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C  3 SEP 03 - CMA - CHANGES TO ALLOW CPHF ITERATION LIMIT INPUT
C 15 MAY 03 - CMA - ADD MODULE OF DISTRIBUTED DATA UMP2 GRADIENT
C
C  <><><><><><>  PARALLEL UMP2 ENERGY AND GRADIENT MODULE <><><><><><><>
C  <><><><><><>          WRITTEN BY C. AIKENS             <><><><><><><>
C  <><><><><><>  (BASED ON MP2DDI MODULE BY G. FLETCHER)  <><><><><><><>
C        -         -         -         -         -         -         - C
C*MODULE UMPDDI   *DECK UTRAN2
      SUBROUTINE UTRAN2(CMO,CMOB,TRAN,TRANB,TMP1,IJSHL,NOFFIJ)
C
C -----------------------------------------------------------------
C  SECOND TRANSFORMATION (BASED ON TRAN2)
C  CALLED FROM PARTRAN IN MODULE MP2DDI
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
      LOGICAL OUTPUT,MP2ENG,UMP2GD,NDCOUL
      DOUBLE PRECISION CMO(NBF,*),TRAN(NBF,MXSHL*MXSHL,*),TMP1(IJSHL,*)
      DOUBLE PRECISION CMOB(NBF,*),TRANB(NBF,MXSHL*MXSHL,*)
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
      INTEGER         D_UB,D_E,D_EB
      INTEGER         D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB,
     *                D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      LOGICAL         NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB,
     *                NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
     *,               NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /UMPDMS/ D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB
     *,               D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
     *,               NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB
     *,               NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
     *,               D_UB,D_E,D_EB
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
C
C  AT THIS POINT THE DECISION IS ABOUT WHETHER WE ARE AIMING FOR
C  ONE OF THE 'COULOMB'-TYPE CLASSES (OOOO,VOOO, OR VVOO), OR JUST
C  THE 'EXCHANGE'-TYPE VOVO CLASS, OR BOTH.
C
      NDCOUL = NDOOOO.OR.NDVOOO.OR.NDVVOO
      IF (NDCOUL) THEN
        IF (NDVOVO) THEN
          KSTT = 1
          KRNG = NMOS
          KSTTB= 1
          KRNGB= NMOS
        ELSE
          KSTT = 1
          KRNG = NOCC
          KSTTB= 1
          KRNGB= NOCCB
        END IF
      ELSE IF (NDVOVO) THEN
        KSTT = NOP1
        KRNG = NVIR
        KSTTB= NOP1B
        KRNGB= NVIRB
      END IF
C
      ICNT = 0
      DO IOCC = IOST, NOCC
        ICNT = ICNT + 1
        CALL DGEMM('T','N',IJSHL,KRNG,NBF
     *,            ONE,TRAN(1,1,ICNT),NBF
     *,            CMO(1,KSTT),NBF
     *,            ZERO,TMP1,IJSHL)
        DO IUV = 1, IJSHL
          CALL DCOPY(KRNG,TMP1(IUV,1),IJSHL,TRAN(1,IUV,ICNT),1)
        END DO
      END DO
      ICNT = 0
      DO IOCC = IOSTB, NOCCB
        ICNT = ICNT + 1
        CALL DGEMM('T','N',IJSHL,KRNGB,NBF
     *,            ONE,TRANB(1,1,ICNT),NBF
     *,            CMOB(1,KSTTB),NBF
     *,            ZERO,TMP1,IJSHL)
        DO IUV = 1, IJSHL
          CALL DCOPY(KRNGB,TMP1(IUV,1),IJSHL,TRANB(1,IUV,ICNT),1)
        END DO
      END DO
C
C  PACK SEND BUFFER AND PUT HALF-TRANSFORMED [**|OO) INTEGRALS
C
      IF (NDCOUL) THEN
        IBEG = NOFFIJ + 1
        IEND = NOFFIJ + IJSHL
        IJ = 0
        DO IOCC = 1, NOCC
          DO JOCC = 1, IOCC
            IJ = IJ + 1
            DO IUV = 1, IJSHL
              TMP1(IUV,IJ) = TRAN(JOCC,IUV,IOCC)
            END DO
          END DO
        END DO
        CALL DDI_PUT(D_VVOO,IBEG,IEND,1,NOTR,TMP1)
        IJ = 0
        DO IOCC = 1, NOCCB
          DO JOCC = 1, IOCC
            IJ = IJ + 1
            DO IUV = 1, IJSHL
              TMP1(IUV,IJ) = TRANB(JOCC,IUV,IOCC)
            END DO
          END DO
        END DO
        CALL DDI_PUT(D_VVOOBB,IBEG,IEND,1,NOTB,TMP1)
      END IF
      RETURN
      END
C*MODULE UMPDDI   *DECK UTRAN3
      SUBROUTINE UTRAN3(CMO,CMOB,TRAN,TRANB,TMP1,TMP2,TMP1B,TMP2B,ISHL,
     *                  JSHL)
C
C -----------------------------------------------------------------
C  THIRD TRANSFORMATION FOR (VO|VO) INTEGRALS (BASED ON TRAN3)
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXSH=1000, MXGTOT=5000, ZERO=0.0D+00)
      LOGICAL OUTPUT,MP2ENG,UMP2GD,IANJ
      DOUBLE PRECISION CMO(NBF,*),TRAN(NBF,MXSHL*MXSHL,*)
     *,      CMOB(NBF,*),TRANB(NBF,MXSHL*MXSHL,*)
     *,      TMP1(NVIR,ISHL,*),TMP2(NVIR,JSHL,*)
     *,      TMP1B(NVIRB,ISHL,*),TMP2B(NVIRB,JSHL,*)
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
      INTEGER         D_UB,D_E,D_EB
      INTEGER         D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB,
     *                D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      LOGICAL         NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB,
     *                NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
     *,               NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /UMPDMS/ D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB
     *,               D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
     *,               NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB
     *,               NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
     *,               D_UB,D_E,D_EB
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
      COMMON /SHLG70/ ISHELL,JSHELL,KSHELL,LSHELL,INEW,JNEW,KNEW,LNEW
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
C
      IF (.NOT.NDVOVO) RETURN
C
      IANJ = ISHELL.EQ.JSHELL
      MINI = KMIN(ISHELL)
      MAXI = KMAX(ISHELL)
      LOCI = KLOC(ISHELL) - MINI
      MINJ = KMIN(JSHELL)
      MAXJ = KMAX(JSHELL)
      LOCJ = KLOC(JSHELL) - MINJ
      IF (MP2ENG) THEN
        NOFF = 0
        NOFFB= 0
      ELSE
        NOFF = NOCC
        NOFFB= NOCCB
      END IF
C
C  LOOP OVER NODE PARTITIONS TO SCALE THE ACC BUFFER SIZE
C
      CALL DDI_NPROC( NPROC, ME )
      DO IP = 0, NPROC-1
C
C  FIGURE OUT DISTRIBUTION OF ALL ALPHA VOVO SET
C  AND TRANSFORM THIRD ALPHA INDEX
C
        CALL DDI_DISTRIB( D_VOVO, IP, ILO, IHI, JLO, JHI )
        CALL DCOPY(NVIR*(JHI-JLO+1)*ISHL,ZERO,0,TMP1,1)
        CALL DCOPY(NVIR*(JHI-JLO+1)*JSHL,ZERO,0,TMP2,1)
        IJP  = 0
        IJ   = 0
        ICNT = 0
        DO IOCC = IOST, NOCC
          ICNT = ICNT + 1
          DO JOCC = IOST, IOCC
            IJ = IJ + 1
            IF ( IJ.GE.JLO .AND. IJ.LE.JHI ) THEN
              IJP  = IJP + 1
              IJN  = 0
              JMAX = MAXJ
              II   = 0
              DO I = MINI, MAXI
                II = II + 1
                IF (IANJ) JMAX = I
                JJ = 0
                DO J = MINJ, JMAX
                  JJ  = JJ  + 1
                  IJN = IJN + 1
                  IF ( I+LOCI .NE. J+LOCJ ) THEN
                    DO IVIR = 1, NVIR
                     TMP1(IVIR,II,IJP) = TMP1(IVIR,II,IJP) +
     *               TRAN(NOFF+IVIR,IJN,ICNT)*CMO(J+LOCJ,JOCC)
                     TMP2(IVIR,JJ,IJP) = TMP2(IVIR,JJ,IJP) +
     *               TRAN(NOFF+IVIR,IJN,ICNT)*CMO(I+LOCI,JOCC)
                    END DO
                  ELSE
                    DO IVIR = 1, NVIR
                     TMP1(IVIR,II,IJP) = TMP1(IVIR,II,IJP) +
     *               TRAN(NOFF+IVIR,IJN,ICNT)*CMO(J+LOCJ,JOCC)
                    END DO
                  END IF
                END DO
              END DO
            END IF
          END DO
        END DO
C
C  ACCUMULATE [V*|VO) ALL ALPHA INTEGRALS
C
        ILO1 = ( LOCI + MINI - 1 )*NVIR + 1
        IHI1 = ILO1 + NVIR*ISHL -1
        CALL DDI_ACC(D_VOVO,ILO1,IHI1,JLO,JHI,TMP1)
        ILO2 = ( LOCJ + MINJ - 1 )*NVIR + 1
        IHI2 = ILO2 + NVIR*JSHL -1
        CALL DDI_ACC(D_VOVO,ILO2,IHI2,JLO,JHI,TMP2)
C
C  FIGURE OUT DISTRIBUTION OF ALL BETA VOVO INTEGRALS
C  AND TRANSFORM THIRD INDEX
C
        CALL DDI_DISTRIB( D_VOVOBB, IP, ILO, IHI, JLO, JHI )
        CALL DCOPY(NVIRB*(JHI-JLO+1)*ISHL,ZERO,0,TMP1B,1)
        CALL DCOPY(NVIRB*(JHI-JLO+1)*JSHL,ZERO,0,TMP2B,1)
        IJP  = 0
        IJ   = 0
        ICNT = 0
        DO IOCC = IOSTB, NOCCB
          ICNT = ICNT + 1
          DO JOCC = IOSTB, IOCC
            IJ = IJ + 1
            IF ( IJ.GE.JLO .AND. IJ.LE.JHI ) THEN
              IJP  = IJP + 1
              IJN  = 0
              JMAX = MAXJ
              II   = 0
              DO I = MINI, MAXI
                II = II + 1
                IF (IANJ) JMAX = I
                JJ = 0
                DO J = MINJ, JMAX
                  JJ  = JJ  + 1
                  IJN = IJN + 1
                  IF ( I+LOCI .NE. J+LOCJ ) THEN
                    DO IVIR = 1, NVIRB
                     TMP1B(IVIR,II,IJP) = TMP1B(IVIR,II,IJP) +
     *               TRANB(NOFFB+IVIR,IJN,ICNT)*CMOB(J+LOCJ,JOCC)
                     TMP2B(IVIR,JJ,IJP) = TMP2B(IVIR,JJ,IJP) +
     *               TRANB(NOFFB+IVIR,IJN,ICNT)*CMOB(I+LOCI,JOCC)
                    END DO
                  ELSE
                    DO IVIR = 1, NVIRB
                     TMP1B(IVIR,II,IJP) = TMP1B(IVIR,II,IJP) +
     *               TRANB(NOFFB+IVIR,IJN,ICNT)*CMOB(J+LOCJ,JOCC)
                    END DO
                  END IF
                END DO
              END DO
            END IF
          END DO
        END DO
C
C  ACCUMULATE [V*|VO) ALL BETA INTEGRALS
C
        ILO1 = ( LOCI + MINI - 1 )*NVIRB + 1
        IHI1 = ILO1 + NVIRB*ISHL -1
        CALL DDI_ACC(D_VOVOBB,ILO1,IHI1,JLO,JHI,TMP1B)
        ILO2 = ( LOCJ + MINJ - 1 )*NVIRB + 1
        IHI2 = ILO2 + NVIRB*JSHL -1
        CALL DDI_ACC(D_VOVOBB,ILO2,IHI2,JLO,JHI,TMP2B)
C
C  FIGURE OUT DISTRIBUTION OF MIXED SPIN VOVO INTEGRALS
C  AND TRANSFORM THIRD INDEX
C
        CALL DDI_DISTRIB( D_VOVOAB, IP, ILO, IHI, JLO, JHI )
        CALL DCOPY(NVIRB*(JHI-JLO+1)*ISHL,ZERO,0,TMP1B,1)
        CALL DCOPY(NVIRB*(JHI-JLO+1)*JSHL,ZERO,0,TMP2B,1)
        IJP  = 0
        IJ   = 0
        ICNT = 0
        DO IOCC = IOSTB, NOCCB
          ICNT = ICNT + 1
          DO JOCC = IOST, NOCC
            IJ = IJ + 1
            IF ( IJ.GE.JLO .AND. IJ.LE.JHI ) THEN
              IJP  = IJP + 1
              IJN  = 0
              JMAX = MAXJ
              II   = 0
              DO I = MINI, MAXI
                II = II + 1
                IF (IANJ) JMAX = I
                JJ = 0
                DO J = MINJ, JMAX
                  JJ  = JJ  + 1
                  IJN = IJN + 1
                  IF ( I+LOCI .NE. J+LOCJ ) THEN
                    DO IVIR = 1, NVIRB
                     TMP1B(IVIR,II,IJP) = TMP1B(IVIR,II,IJP) +
     *               TRANB(NOFFB+IVIR,IJN,ICNT)*CMO(J+LOCJ,JOCC)
                     TMP2B(IVIR,JJ,IJP) = TMP2B(IVIR,JJ,IJP) +
     *               TRANB(NOFFB+IVIR,IJN,ICNT)*CMO(I+LOCI,JOCC)
                    END DO
                  ELSE
                    DO IVIR = 1, NVIRB
                     TMP1B(IVIR,II,IJP) = TMP1B(IVIR,II,IJP) +
     *               TRANB(NOFFB+IVIR,IJN,ICNT)*CMO(J+LOCJ,JOCC)
                    END DO
                  END IF
                END DO
              END DO
            END IF
          END DO
        END DO
C
C  ACCUMULATE [V*|VO) ALL MIXED SPIN INTEGRALS
C
        ILO1 = ( LOCI + MINI - 1 )*NVIRB + 1
        IHI1 = ILO1 + NVIRB*ISHL -1
        CALL DDI_ACC(D_VOVOAB,ILO1,IHI1,JLO,JHI,TMP1B)
        ILO2 = ( LOCJ + MINJ - 1 )*NVIRB + 1
        IHI2 = ILO2 + NVIRB*JSHL -1
        CALL DDI_ACC(D_VOVOAB,ILO2,IHI2,JLO,JHI,TMP2B)
      END DO
      RETURN
      END
C*MODULE UMPDDI   *DECK UTRAN34
      SUBROUTINE UTRAN34(CMO,CMOB,TMP1,TMP2,TMP3,MOLABS,MOLABSB)
C
C -----------------------------------------------------------------
C  THIRD AND FOURTH TRANSFORMATIONS FOR (VV|OO), (VO|OO), (OO|OO)
C  FOURTH TRANSFORMATION FOR (VO|VO) INTEGRALS
C  AND SCREEN INTEGRALS VANISHING BY SYMMETRY
C  SUBROUTINE BASED ON TRAN34
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXSH=1000, MXGTOT=5000, ZERO=0.0D+00, ONE=1.0D+00)
      LOGICAL GOPARR,DSKWRK,MASWRK,OUTPUT,MP2ENG,UMP2GD,SYM2EI,NDCOUL
      DOUBLE PRECISION CMO(NBF,*),CMOB(NBF,*),TMP1(*),TMP2(*),TMP3(*)
      INTEGER MOLABS(*),MOLABSB(*)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
      INTEGER         D_UB,D_E,D_EB
      INTEGER         D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB,
     *                D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      LOGICAL         NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB,
     *                NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
     *,               NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /UMPDMS/ D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB
     *,               D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
     *,               NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB
     *,               NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
     *,               D_UB,D_E,D_EB
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
C
      NDCOUL = NDOOOO.OR.NDVOOO.OR.NDVVOO
      IF (NDCOUL) THEN
         CALL DDI_DISTRIB(D_VVOO,ME,ILO,IHI,JLO,JHI)
         CALL DDI_DISTRIB(D_VVOOBB,ME,ILOB,IHIB,JLOB,JHIB)
      ELSE IF (NDVOVO) THEN
         CALL DDI_DISTRIB(D_VOVO,ME,ILO,IHI,JLO,JHI)
         CALL DDI_DISTRIB(D_VOVOBB,ME,ILOB,IHIB,JLOB,JHIB)
      ELSE
         WRITE(ICHANL,*) 'UTRAN34: ERROR, NO INTEGRAL TYPE'
         CALL ABRT()
      END IF
C
      IJ = 0
      DO IOCC = IOST, NOCC
       ISM = MOLABS(IOCC)
       DO JOCC = IOST, IOCC
         JSM = MOLABS(JOCC)
         IJ = IJ + 1
         IF (IJ.GE.JLO .AND. IJ.LE.JHI) THEN
C
C  DO THE (OO|OO), (VO|OO), AND (VV|OO) CLASSES
C  WITH TWO ALPHA OCC INDICES
C
          IF (NDCOUL) THEN
            CALL DDI_GET(D_VVOO,1,NBTR,IJ,IJ,TMP1)
C
C  NOTICE TRAN2 DID NOT SCATTER THE [**|OO) INTEGRALS
C  INTO THE VVOO ARRAY IN STRICT AO-ORDER...
C
            IJN = 0
            DO II = 1, NSHELL
              DO JJ = 1, II
C
C ... NOW THE CORRECT AO-LIST IS RECOVERED
C
                DO I = KLOC(II), KLOC(II) - KMIN(II) + KMAX(II)
                  JMAX = KLOC(JJ) - KMIN(JJ) + KMAX(JJ)
                  IF (II.EQ.JJ) JMAX = I
                  DO J = KLOC(JJ), JMAX
                    IJN = IJN + 1
                    TMP3((I-1)*NBF+J) = TMP1(IJN)
                    TMP3((J-1)*NBF+I) = TMP1(IJN)
                  END DO
                END DO
              END DO
            END DO
C
C  FIRST, DO THE TRANSFORMATION TO ALL ALPHA
C  (OO|OO), (VO|OO), AND (VV|OO)
C
C  THIRD QUARTER-TRANSFORMATION TO ALPHA OCCUPIED INDICES
C
            IF (NDOOOO.OR.NDVOOO) THEN
              CALL DGEMM('N','N',NBF,NOCC,NBF
     *,                  ONE,TMP3,NBF
     *,                  CMO,NBF
     *,                  ZERO,TMP1,NBF)
            END IF
C
C  THIRD QUARTER-TRANSFORMATION TO ALPHA VIRTUAL INDICES
C
            IF (NDVVOO) THEN
              CALL DGEMM('N','N',NBF,NVIR,NBF
     *,                  ONE,TMP3,NBF
     *,                  CMO(1,NOP1),NBF
     *,                  ZERO,TMP1( (NOCC*NBF)+1 ),NBF)
            END IF
C
C  FOURTH QUARTER-TRANSFORMATION TO ALPHA OCC-OCC INDICES
C  (NOTE SPECIAL MULTIPLY ROUTINE THAT USES SYMMETRY)
C
            IF (NDOOOO) THEN
              CALL ARBRCS('T','N',NOCC,NBF
     *,                   ONE,CMO,NBF
     *,                   TMP1,NBF
     *,                   ZERO,TMP2,NBF)
            END IF
C
C  FOURTH QUARTER-TRANSFORMATION TO ALPHA VIR-OCC INDICES
C
            IF (NDVOOO) THEN
              CALL DGEMM('T','N',NVIR,NOCC,NBF
     *,                  ONE,CMO(1,NOP1),NBF
     *,                  TMP1,NBF
     *,                  ZERO,TMP2( NOP1 ),NBF)
            END IF
C
C  FOURTH QUARTER-TRANSFORMATION TO ALPHA VIR-VIR INDICES
C  (NOTE SPECIAL MULTIPLY ROUTINE THAT USES SYMMETRY)
C
           IF (NDVVOO) THEN
             CALL ARBRCS('T','N',NVIR,NBF
     *,                  ONE,CMO(1,NOP1),NBF
     *,                  TMP1( (NOCC*NBF)+1 ),NBF
     *,                  ZERO,TMP2( (NOCC*NBF)+NOP1 ),NBF)
           END IF
C
           IF (NDOOOO) THEN
C
C  PUT (OO|OO) INTEGRALS IN DISTRIBUTED MEMORY
C
             IJN = 0
             DO I = 1, NOCC
               KSM = MOLABS(I)
               DO J = 1, I
                 LSM = MOLABS(J)
                 IJN = IJN + 1
                 TMP1(IJN) = TMP2( (I-1)*NBF + J )
                 IF(.NOT.SYM2EI(ISM,JSM,KSM,LSM)) TMP1(IJN) = ZERO
               END DO
             END DO
             CALL DDI_PUT(D_OOOO,1,IJN,IJ,IJ,TMP1)
           END IF
           IF (NDVVOO) THEN
C
C  PUT VVOO INTEGRALS IN DISTRIBUTED MEMORY
C
             IJN = 0
             DO I = NOP1, NMOS
               KSM = MOLABS(I)
               DO J = NOP1, I
                 LSM = MOLABS(J)
                 IJN = IJN + 1
                 TMP1(IJN) = TMP2( (I-1)*NBF + J )
                 IF(.NOT.SYM2EI(ISM,JSM,KSM,LSM)) TMP1(IJN) = ZERO
               END DO
             END DO
             CALL DDI_PUT(D_VVOO,1,IJN,IJ,IJ,TMP1)
           END IF
           IF (NDVOOO) THEN
C
C  PUT VOOO INTEGRALS IN DISTRIBUTED MEMORY
C
             IJN = 0
             DO J = 1, NOCC
               KSM = MOLABS(J)
               DO I = NOP1, NMOS
                 LSM = MOLABS(I)
                 IJN = IJN + 1
                 TMP1(IJN) = TMP2( (J-1)*NBF + I )
                 IF(.NOT.SYM2EI(ISM,JSM,KSM,LSM)) TMP1(IJN) = ZERO
               END DO
             END DO
             CALL DDI_PUT(D_VOOO,1,NVIR,(IJ-1)*NOCC+1,IJ*NOCC,TMP1)
           END IF
C
C  NOW DO THE TRANSFORMATION TO MIXED SPIN (OO|OO), (VO|OO), AND (VV|OO)
C
C  THIRD QUARTER-TRANSFORMATION TO BETA OCCUPIED INDEX
C
            IF (NDOOOO.OR.NDVOOO) THEN
              CALL DGEMM('N','N',NBF,NOCCB,NBF
     *,                  ONE,TMP3,NBF
     *,                  CMOB,NBF
     *,                  ZERO,TMP1,NBF)
            END IF
C
C  THIRD QUARTER-TRANSFORMATION TO BETA VIRTUAL INDEX
C
            IF (NDVVOO) THEN
              CALL DGEMM('N','N',NBF,NVIRB,NBF
     *,                  ONE,TMP3,NBF
     *,                  CMOB(1,NOP1B),NBF
     *,                  ZERO,TMP1( (NOCCB*NBF)+1 ),NBF)
            END IF
C
C  FOURTH QUARTER-TRANSFORMATION TO BETA OCC-OCC INDICES
C  (NOTE SPECIAL MULTIPLY ROUTINE THAT USES SYMMETRY)
C
            IF (NDOOOO) THEN
              CALL ARBRCS('T','N',NOCCB,NBF
     *,                   ONE,CMOB,NBF
     *,                   TMP1,NBF
     *,                   ZERO,TMP2,NBF)
            END IF
C
C  FOURTH QUARTER-TRANSFORMATION TO BETA VIR-OCC INDICES
C
            IF (NDVOOO) THEN
              CALL DGEMM('T','N',NVIRB,NOCCB,NBF
     *,                  ONE,CMOB(1,NOP1B),NBF
     *,                  TMP1,NBF
     *,                  ZERO,TMP2( NOP1B ),NBF)
            END IF
C
C  FOURTH QUARTER-TRANSFORMATION TO BETA VIR-VIR INDICES
C  (NOTE SPECIAL MULTIPLY ROUTINE THAT USES SYMMETRY)
C
           IF (NDVVOO) THEN
             CALL ARBRCS('T','N',NVIRB,NBF
     *,                  ONE,CMOB(1,NOP1B),NBF
     *,                  TMP1( (NOCCB*NBF)+1 ),NBF
     *,                  ZERO,TMP2( (NOCCB*NBF)+NOP1B ),NBF)
           END IF
C
           IF (NDOOOO) THEN
C
C  PUT MIXED SPIN (OO|OO) INTEGRALS IN DISTRIBUTED MEMORY
C
             IJN = 0
             DO I = 1, NOCCB
               KSM = MOLABSB(I)
               DO J = 1, I
                 LSM = MOLABSB(J)
                 IJN = IJN + 1
                 TMP1(IJN) = TMP2( (I-1)*NBF + J )
                 IF(.NOT.SYM2EI(ISM,JSM,KSM,LSM)) TMP1(IJN) = ZERO
               END DO
             END DO
             CALL DDI_PUT(D_OOOOAB,1,IJN,IJ,IJ,TMP1)
           END IF
           IF (NDVVOO) THEN
C
C  PUT VVOO INTEGRALS IN DISTRIBUTED MEMORY
C
             IJN = 0
             DO I = NOP1B, NMOS
               KSM = MOLABSB(I)
               DO J = NOP1B, I
                 LSM = MOLABSB(J)
                 IJN = IJN + 1
                 TMP1(IJN) = TMP2( (I-1)*NBF + J )
                 IF(.NOT.SYM2EI(ISM,JSM,KSM,LSM)) TMP1(IJN) = ZERO
               END DO
             END DO
             CALL DDI_PUT(D_VVOOBA,1,IJN,IJ,IJ,TMP1)
           END IF
           IF (NDVOOO) THEN
C
C  PUT VOOO INTEGRALS IN DISTRIBUTED MEMORY
C
             IJN = 0
             DO J = 1, NOCCB
               KSM = MOLABSB(J)
               DO I = NOP1B, NMOS
                 LSM = MOLABSB(I)
                 IJN = IJN + 1
                 TMP1(IJN) = TMP2( (J-1)*NBF + I )
                 IF(.NOT.SYM2EI(ISM,JSM,KSM,LSM)) TMP1(IJN) = ZERO
               END DO
             END DO
             CALL DDI_PUT(D_VOOOBA,1,NVIRB,(IJ-1)*NOCCB+1,IJ*NOCCB,TMP1)
           END IF
C
         END IF  ! NDCOUL
         IF (NDVOVO) THEN
C
C  TRANSFORM REMAINING AO INDEX OF VOVO CLASS
C
           CALL DDI_GET(D_VOVO,1,NBF*NVIR,IJ,IJ,TMP1)
           CALL DGEMM('N','N',NVIR,NVIR,NBF
     *,               ONE,TMP1,NVIR
     *,               CMO(1,NOP1),NBF
     *,               ZERO,TMP2,NVIR)
           IJN = 0
           DO I = NOP1, NMOS
             KSM = MOLABS(I)
             DO J = NOP1, NMOS
               LSM = MOLABS(J)
               IJN = IJN + 1
               IF(.NOT.SYM2EI(ISM,JSM,KSM,LSM)) TMP2(IJN) = ZERO
             END DO
           END DO
           CALL DDI_PUT(D_VOVO,1,NVSQ,IJ,IJ,TMP2)
          END IF
        END IF  ! IJ-LOCAL
       END DO  ! JOCC
      END DO  ! IOCC
C
C  NOW, DO THE SAME PROCEDURE FOR TWO BETA OCC INDICES
C
      IJ = 0
      DO IOCC = IOSTB, NOCCB
       ISM = MOLABSB(IOCC)
       DO JOCC = IOSTB, IOCC
         JSM = MOLABSB(JOCC)
         IJ = IJ + 1
         IF (IJ.GE.JLOB .AND. IJ.LE.JHIB) THEN
C
C  DO THE [(OO|OO)], (VO|OO), AND (VV|OO) CLASSES
C  WITH TWO BETA OCC INDICES
C
          IF (NDCOUL) THEN
            CALL DDI_GET(D_VVOOBB,1,NBTR,IJ,IJ,TMP1)
C
C  NOTICE TRAN2 DID NOT SCATTER THE [**|OO) INTEGRALS
C  INTO THE VVOO ARRAY IN STRICT AO-ORDER...
C
            IJN = 0
            DO II = 1, NSHELL
              DO JJ = 1, II
C
C ... NOW THE CORRECT AO-LIST IS RECOVERED
C
                DO I = KLOC(II), KLOC(II) - KMIN(II) + KMAX(II)
                  JMAX = KLOC(JJ) - KMIN(JJ) + KMAX(JJ)
                  IF (II.EQ.JJ) JMAX = I
                  DO J = KLOC(JJ), JMAX
                    IJN = IJN + 1
                    TMP3((I-1)*NBF+J) = TMP1(IJN)
                    TMP3((J-1)*NBF+I) = TMP1(IJN)
                  END DO
                END DO
              END DO
            END DO
C
C  FIRST, DO THE TRANSFORMATION TO MIXED SPIN
C      [(OO|OO)], (VO|OO), AND (VV|OO)
C
C  THIRD QUARTER-TRANSFORMATION TO ALPHA OCCUPIED INDICES
C
            IF (NDOOOO.OR.NDVOOO) THEN
              CALL DGEMM('N','N',NBF,NOCC,NBF
     *,                  ONE,TMP3,NBF
     *,                  CMO,NBF
     *,                  ZERO,TMP1,NBF)
            END IF
C
C  THIRD QUARTER-TRANSFORMATION TO ALPHA VIRTUAL INDICES
C
            IF (NDVVOO) THEN
              CALL DGEMM('N','N',NBF,NVIR,NBF
     *,                  ONE,TMP3,NBF
     *,                  CMO(1,NOP1),NBF
     *,                  ZERO,TMP1( (NOCC*NBF)+1 ),NBF)
            END IF
C
C  FOURTH QUARTER-TRANSFORMATION TO ALPHA OCC-OCC INDICES
C  (NOTE SPECIAL MULTIPLY ROUTINE THAT USES SYMMETRY)
C
C            IF (NDOOOO) THEN
C              CALL ARBRCS('T','N',NOCC,NBF
C     *,                   ONE,CMO,NBF
C     *,                   TMP1,NBF
C     *,                   ZERO,TMP2,NBF)
C            END IF
C
C  FOURTH QUARTER-TRANSFORMATION TO ALPHA VIR-OCC INDICES
C
            IF (NDVOOO) THEN
              CALL DGEMM('T','N',NVIR,NOCC,NBF
     *,                  ONE,CMO(1,NOP1),NBF
     *,                  TMP1,NBF
     *,                  ZERO,TMP2( NOP1 ),NBF)
            END IF
C
C  FOURTH QUARTER-TRANSFORMATION TO ALPHA VIR-VIR INDICES
C  (NOTE SPECIAL MULTIPLY ROUTINE THAT USES SYMMETRY)
C
           IF (NDVVOO) THEN
             CALL ARBRCS('T','N',NVIR,NBF
     *,                  ONE,CMO(1,NOP1),NBF
     *,                  TMP1( (NOCC*NBF)+1 ),NBF
     *,                  ZERO,TMP2( (NOCC*NBF)+NOP1 ),NBF)
           END IF
C
           IF (NDVVOO) THEN
C
C  PUT VVOO INTEGRALS IN DISTRIBUTED MEMORY
C
             IJN = 0
             DO I = NOP1, NMOS
               KSM = MOLABS(I)
               DO J = NOP1, I
                 LSM = MOLABS(J)
                 IJN = IJN + 1
                 TMP1(IJN) = TMP2( (I-1)*NBF + J )
                 IF(.NOT.SYM2EI(ISM,JSM,KSM,LSM)) TMP1(IJN) = ZERO
               END DO
             END DO
             CALL DDI_PUT(D_VVOOAB,1,IJN,IJ,IJ,TMP1)
           END IF
C
           IF (NDVOOO) THEN
C
C  PUT VOOO INTEGRALS IN DISTRIBUTED MEMORY
C
             IJN = 0
             DO J = 1, NOCC
               KSM = MOLABS(J)
               DO I = NOP1, NMOS
                 LSM = MOLABS(I)
                 IJN = IJN + 1
                 TMP1(IJN) = TMP2( (J-1)*NBF + I )
                 IF(.NOT.SYM2EI(ISM,JSM,KSM,LSM)) TMP1(IJN) = ZERO
               END DO
             END DO
             CALL DDI_PUT(D_VOOOAB,1,NVIR,(IJ-1)*NOCC+1,IJ*NOCC,TMP1)
           END IF
C
C  NOW DO THE TRANSFORMATION TO BETA SPIN (OO|OO), (VO|OO), AND (VV|OO)
C
C  THIRD QUARTER-TRANSFORMATION TO BETA OCCUPIED INDEX
C
            IF (NDOOOO.OR.NDVOOO) THEN
              CALL DGEMM('N','N',NBF,NOCCB,NBF
     *,                  ONE,TMP3,NBF
     *,                  CMOB,NBF
     *,                  ZERO,TMP1,NBF)
            END IF
C
C  THIRD QUARTER-TRANSFORMATION TO BETA VIRTUAL INDEX
C
            IF (NDVVOO) THEN
              CALL DGEMM('N','N',NBF,NVIRB,NBF
     *,                  ONE,TMP3,NBF
     *,                  CMOB(1,NOP1B),NBF
     *,                  ZERO,TMP1( (NOCCB*NBF)+1 ),NBF)
            END IF
C
C  FOURTH QUARTER-TRANSFORMATION TO BETA OCC-OCC INDICES
C  (NOTE SPECIAL MULTIPLY ROUTINE THAT USES SYMMETRY)
C
            IF (NDOOOO) THEN
              CALL ARBRCS('T','N',NOCCB,NBF
     *,                   ONE,CMOB,NBF
     *,                   TMP1,NBF
     *,                   ZERO,TMP2,NBF)
            END IF
C
C  FOURTH QUARTER-TRANSFORMATION TO BETA VIR-OCC INDICES
C
            IF (NDVOOO) THEN
              CALL DGEMM('T','N',NVIRB,NOCCB,NBF
     *,                  ONE,CMOB(1,NOP1B),NBF
     *,                  TMP1,NBF
     *,                  ZERO,TMP2( NOP1B ),NBF)
            END IF
C
C  FOURTH QUARTER-TRANSFORMATION TO BETA VIR-VIR INDICES
C  (NOTE SPECIAL MULTIPLY ROUTINE THAT USES SYMMETRY)
C
           IF (NDVVOO) THEN
             CALL ARBRCS('T','N',NVIRB,NBF
     *,                  ONE,CMOB(1,NOP1B),NBF
     *,                  TMP1( (NOCCB*NBF)+1 ),NBF
     *,                  ZERO,TMP2( (NOCCB*NBF)+NOP1B ),NBF)
           END IF
C
           IF (NDOOOO) THEN
C
C  PUT BETA SPIN (OO|OO) INTEGRALS IN DISTRIBUTED MEMORY
C
             IJN = 0
             DO I = 1, NOCCB
               KSM = MOLABSB(I)
               DO J = 1, I
                 LSM = MOLABSB(J)
                 IJN = IJN + 1
                 TMP1(IJN) = TMP2( (I-1)*NBF + J )
                 IF(.NOT.SYM2EI(ISM,JSM,KSM,LSM)) TMP1(IJN) = ZERO
               END DO
             END DO
             CALL DDI_PUT(D_OOOOBB,1,IJN,IJ,IJ,TMP1)
           END IF
C
           IF (NDVVOO) THEN
C
C  PUT VVOO INTEGRALS IN DISTRIBUTED MEMORY
C
             IJN = 0
             DO I = NOP1B, NMOS
               KSM = MOLABSB(I)
               DO J = NOP1B, I
                 LSM = MOLABSB(J)
                 IJN = IJN + 1
                 TMP1(IJN) = TMP2( (I-1)*NBF + J )
                 IF(.NOT.SYM2EI(ISM,JSM,KSM,LSM)) TMP1(IJN) = ZERO
               END DO
             END DO
             CALL DDI_PUT(D_VVOOBB,1,IJN,IJ,IJ,TMP1)
           END IF
C
           IF (NDVOOO) THEN
C
C  PUT VOOO INTEGRALS IN DISTRIBUTED MEMORY
C
             IJN = 0
             DO J = 1, NOCCB
               KSM = MOLABSB(J)
               DO I = NOP1B, NMOS
                 LSM = MOLABSB(I)
                 IJN = IJN + 1
                 TMP1(IJN) = TMP2( (J-1)*NBF + I )
                 IF(.NOT.SYM2EI(ISM,JSM,KSM,LSM)) TMP1(IJN) = ZERO
               END DO
             END DO
             CALL DDI_PUT(D_VOOOBB,1,NVIRB,(IJ-1)*NOCCB+1,IJ*NOCCB,TMP1)
           END IF
C
         END IF  ! NDCOUL
C
         IF (NDVOVO) THEN
C
C  TRANSFORM REMAINING AO INDEX OF VOVO CLASS, ALL BETA SPIN
C
           CALL DDI_GET(D_VOVOBB,1,NBF*NVIRB,IJ,IJ,TMP1)
           CALL DGEMM('N','N',NVIRB,NVIRB,NBF
     *,               ONE,TMP1,NVIRB
     *,               CMOB(1,NOP1B),NBF
     *,               ZERO,TMP2,NVIRB)
           IJN = 0
           DO I = NOP1B, NMOS
             KSM = MOLABSB(I)
             DO J = NOP1B, NMOS
               LSM = MOLABSB(J)
               IJN = IJN + 1
               IF(.NOT.SYM2EI(ISM,JSM,KSM,LSM)) TMP2(IJN) = ZERO
             END DO
           END DO
           CALL DDI_PUT(D_VOVOBB,1,NVSB,IJ,IJ,TMP2)
          END IF
        END IF  ! IJ-LOCAL
       END DO  ! JOCC
      END DO  ! IOCC
C
C  NOW DO MIXED SPIN VOVO FINAL INDEX TRANSFORMATION
C
      IF (NDVOVO) THEN
      CALL DDI_DISTRIB(D_VOVOAB,ME,ILO,IHI,JLO,JHI)
      IJ = 0
      DO IOCC = IOSTB, NOCCB
       ISM = MOLABSB(IOCC)
       DO JOCC = IOST, NOCC
         JSM = MOLABS(JOCC)
         IJ = IJ + 1
         IF (IJ.GE.JLO .AND. IJ.LE.JHI) THEN
           CALL DDI_GET(D_VOVOAB,1,NVIRB*NBF,IJ,IJ,TMP1)
           CALL DGEMM('N','N',NVIRB,NVIR,NBF
     *,               ONE,TMP1,NVIRB
     *,               CMO(1,NOP1),NBF
     *,               ZERO,TMP2,NVIRB)
           IJN = 0
           DO I = NOP1, NMOS
             KSM = MOLABS(I)
             DO J = NOP1B, NMOS
               LSM = MOLABSB(J)
               IJN = IJN + 1
               IF(.NOT.SYM2EI(ISM,JSM,KSM,LSM)) TMP2(IJN) = ZERO
             END DO
           END DO
           CALL DDI_PUT(D_VOVOAB,1,NVIRB*NVIR,IJ,IJ,TMP2)
        END IF  ! IJ-LOCAL
       END DO  ! JOCC
      END DO  ! IOCC
      END IF  ! NDVOVO
C
      RETURN
      END
C*MODULE UMPDDI   *DECK PEUMP2
      SUBROUTINE PEUMP2(EORBA,EORBB,TMP1,TMP2)
C
C -----------------------------------------------------------------
C  COMPUTE UMP2 ENERGY (BASED ON PEMP2)
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXRT=100)
      LOGICAL GOPARR,DSKWRK,MASWRK,OUTPUT,MP2ENG,UMP2GD
      INTEGER A,B
      DOUBLE PRECISION EORBA(*),EORBB(*),TMP1(NVIR,*),TMP2(NVIRB,*)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
      INTEGER         D_UB,D_E,D_EB
      INTEGER         D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB,
     *                D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      LOGICAL         NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB,
     *                NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
     *,               NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /UMPDMS/ D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB
     *,               D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
     *,               NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB
     *,               NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
     *,               D_UB,D_E,D_EB
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
      COMMON /ENRGYS/ ENUC,EELEC,ETOT,SZ,SZZ,ECORE,ESCF,EERD,E1,E2,
     *                VEN,VEE,EPOT,EKIN,ESTATE(MXRT),STATN
      COMMON /ENRGMP/ EMP2,EMP3,EMP4
C
C GET ALL-ALPHA CONTRIBUTION TO ENERGY
C
      CALL DDI_DISTRIB(D_VOVO,ME,IL_VOVO,IH_VOVO,JL_VOVO,JH_VOVO)
      E2 = 0.0D+00
      IJN  = 0
      DO I = NCP1, NOCC
        DO J = NCP1, I
          IF (MP2ENG) THEN
            IJN = IJN + 1
          ELSE
            IJN = (I*I-I)/2 + J
          END IF
          IF (IJN.GE.JL_VOVO .AND. IJN.LE.JH_VOVO) THEN
            CALL DDI_GET(D_VOVO, 1, NVSQ, IJN, IJN, TMP1)
            DO A = 1, NVIR
              DO B = 1, NVIR
                IF (I.NE.J) THEN
                  E2 = E2 + 2.0D+00*
     *            TMP1(A,B)*(TMP1(A,B)-TMP1(B,A))/
     *            (EORBA(I)+EORBA(J)-EORBA(NOCC+A)-EORBA(NOCC+B))
                ELSE
                  E2 = E2 +
     *            TMP1(A,B)*(TMP1(A,B)-TMP1(B,A))/
     *            (EORBA(I)+EORBA(J)-EORBA(NOCC+A)-EORBA(NOCC+B))
                END IF
              END DO
            END DO
          END IF
        END DO
      END DO
C
C GET ALL-BETA CONTRIBUTION TO ENERGY
C
      CALL DDI_DISTRIB(D_VOVOBB,ME,IL_VOVO,IH_VOVO,JL_VOVO,JH_VOVO)
      IJN  = 0
      DO I = NCP1B, NOCCB
        DO J = NCP1B, I
          IF (MP2ENG) THEN
            IJN = IJN + 1
          ELSE
            IJN = (I*I-I)/2 + J
          END IF
          IF (IJN.GE.JL_VOVO .AND. IJN.LE.JH_VOVO) THEN
            CALL DDI_GET(D_VOVOBB, 1, NVSB, IJN, IJN, TMP2)
            DO A = 1, NVIRB
              DO B = 1, NVIRB
                IF (I.NE.J) THEN
                  E2 = E2 + 2.0D+00*
     *            TMP2(A,B)*(TMP2(A,B)-TMP2(B,A))/
     *            (EORBB(I)+EORBB(J)-EORBB(NOCCB+A)-EORBB(NOCCB+B))
                ELSE
                  E2 = E2 +
     *            TMP2(A,B)*(TMP2(A,B)-TMP2(B,A))/
     *            (EORBB(I)+EORBB(J)-EORBB(NOCCB+A)-EORBB(NOCCB+B))
                END IF
              END DO
            END DO
          END IF
        END DO
      END DO
C
C SCALE CONTRIBUTION SO FAR BY THE FACTOR OF ONE HALF
C
      E2 = 0.5D+00*E2
C
C GET MIXED SPIN CONTRIBUTION TO ENERGY
C
      CALL DDI_DISTRIB(D_VOVOAB,ME,IL_VOVO,IH_VOVO,JL_VOVO,JH_VOVO)
      IJN  = 0
      DO I = NCP1B, NOCCB
        DO J = NCP1, NOCC
          IF (MP2ENG) THEN
            IJN = IJN + 1
          ELSE
            IJN = (I-1)*NOCC + J
          END IF
          IF (IJN.GE.JL_VOVO .AND. IJN.LE.JH_VOVO) THEN
            CALL DDI_GET(D_VOVOAB, 1, NVIRB*NVIR, IJN, IJN, TMP2)
            DO B = 1, NVIR
              DO A = 1, NVIRB
                E2 = E2 +
     *          TMP2(A,B)*TMP2(A,B)/
     *          (EORBB(I)+EORBA(J)-EORBB(NOCCB+A)-EORBA(NOCC+B))
              END DO
            END DO
          END IF
        END DO
      END DO
C
C  GLOBALLY-SUM THE MP2 ENERGY CONTRIBUTIONS
C
      CALL DDI_GSUMF(999,E2,1)
      EMP2 = ESCF + E2
      IF (OUTPUT) THEN
        WRITE(ICHANL,5) ESCF, E2, EMP2
        CALL FLSHBF(ICHANL)
      END IF
      RETURN
5     FORMAT(20X,' E(SCF)= ',1F20.10,/
     *,      20X,'   E(2)= ',1F20.10,/
     *,      20X,' E(MP2)= ',1F20.10)
      END
C*MODULE UMPDDI   *DECK UPAR1PDM
      SUBROUTINE UPAR1PDM(CMOA,CMOB,EORBA,EORBB,PMAT,PMATB,PSCFA,PSCFB,
     *                    WMAT,WMATB,SCHWA,MOLABS,MOLABSB)
C
C -------------------------------------------------------------------
C  UMP2 1-PARTICLE DENSITY DRIVING ROUTINE (UPAR1PDM,WPUMP2,UWOV)
C  SUBROUTINE BASED ON PAR1PDM
C -------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXSH=1000, MXATM=500, MXIRR=14)
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
      LOGICAL GOPARR,DSKWRK,MASWRK, OUTPUT,MP2ENG,UMP2GD,ABELPT
      INTEGER MOLABS(*),MOLABSB(*),A
      DOUBLE PRECISION CMOA(NBF,*),EORBA(*),PMAT(NBF,*),PSCFA(NBF,*)
     *,      WMAT(NBF,*),SCHWA(*)
      DOUBLE PRECISION CMOB(NBF,*),EORBB(*),PMATB(NBF,*),PSCFB(NBF,*)
     *,      WMATB(NBF,*)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U, D_UB
      INTEGER         D_E,D_EB
      INTEGER         D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB,
     *                D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      LOGICAL         NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB,
     *                NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
     *,               NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /UMPDMS/ D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB
     *,               D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
     *,               NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB
     *,               NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
     *,               D_UB,D_E,D_EB
      COMMON /FMCOM / XX(1)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /MP2PAR/ OSPT,TOL,METHOD,NWORD,MPPROP,NACORE_MP2PAR,
     *                NBCORE_MP2PAR,NOA,NOB,NO,NBF_MP2PAR,NOMIT,MOCPHF,
     *                MAXITC
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48)
     *,               TSYM(432),INVT(48),NT
C
      IF (OUTPUT) THEN
        WRITE(ICHANL,1)
        CALL FLSHBF(ICHANL)
      END IF
C
C  CONSTRUCT TERMS OF P(2), W(2), AND LAGRANGIAN
C
      CALL VALFM(LOADFM)
      ITMP1 = 1     + LOADFM
      ITMP2 = ITMP1 + NBSQ
      LAST  = ITMP2 + NBSQ
      NEED  = LAST  - LOADFM
      CALL GETFM(NEED)
      CALL WPUMP2(PMAT,PMATB
     *,           WMAT,WMATB
     *,           EORBA,EORBB
     *,           XX(ITMP1),XX(ITMP2))
      CALL RETFM(NEED)
C
C  DONE WITH (OO|OO) INTEGRALS
C
      CALL DDI_DESTROY( D_OOOOBB )
      CALL DDI_DESTROY( D_OOOOAB )
      CALL DDI_DESTROY( D_OOOO )
C
C  COMPUTE VVVO TERMS OF THE LAGRANGIAN
C
      CALL ULAGVVV(WMAT,WMATB,PMAT,PMATB,CMOA,CMOB,EORBA,EORBB,SCHWA)
C
C  GLOBAL SUM OF W(2)
C
      CALL DDI_GSUMF(1002,WMAT,NBSQ)
      CALL DDI_GSUMF(1004,WMATB,NBSQ)
C
C  COMBINE TERMS OF LAGRANGIAN - COPY INTO XX(IZ) AND XX(IZB)
C
      CALL VALFM(LOADFM)
      IZ    = 1      + LOADFM
      IZB   = IZ     + NOV
      IRLAG = IZB    + NOVB
      IRLAGB= IRLAG  + NOV
      LAST  = IRLAGB + NOVB
      NEEDZ = LAST  - LOADFM
      CALL GETFM(NEEDZ)
      IA = IZ - 1
      DO I = 1, NOCC
        DO A = NOP1, NMOS
          IA = IA + 1
          IF (MOLABS(I).EQ.MOLABS(A)) THEN
            XX(IA) = -WMAT(A,I) -WMAT(I,A)
          ELSE
            XX(IA) = ZERO
          END IF
        END DO
      END DO
      IA = IZB - 1
      DO I = 1, NOCCB
        DO A = NOP1B, NMOS
          IA = IA + 1
          IF (MOLABSB(I).EQ.MOLABSB(A)) THEN
            XX(IA) = -WMATB(A,I) -WMATB(I,A)
          ELSE
            XX(IA) = ZERO
          END IF
        END DO
      END DO
      IF (OUTPUT) THEN
        WRITE(ICHANL,2)
        CALL FLSHBF(ICHANL)
      END IF
C
C  SOLVE Z-MATRIX EQUATION FOR OCCUPIED-VIRTUAL BLOCK OF P(2)
C
      CALL VALFM(LOADFM)
      ITMP = 1    + LOADFM
      NSYM = ITMP + MXIRR
      NSYMB= NSYM + NMOS
      NVSM = NSYMB+ NMOS
      NVSMB= NVSM + MXIRR
      LVSM = NVSMB+ MXIRR
      LVSMB= LVSM + MXIRR*NMOS
      LAST = LVSMB+ MXIRR*NMOS
      NEEDS= LAST - LOADFM
      CALL GETFM(NEEDS)
C
C  IN CASE OF NON-ABELIAN POINT GROUPS...
C
      NTSAVE = NT
      IF (.NOT.ABELPT()) NT = 1
C
C  GROUP VIRTUAL MOS ACCORDING TO SYMMETRY
C
      CALL SYMVMO(MOLABS,XX(ITMP),XX(NSYM)
     *,           XX(NVSM),XX(LVSM),MXIRR,NOCC,NOP1,NMOS)
      CALL SYMVMO(MOLABSB,XX(ITMP),XX(NSYMB)
     *,           XX(NVSMB),XX(LVSMB),MXIRR,NOCCB,NOP1B,NMOS)
C
C  FORM REDUCED LAGRANGIAN (ALPHA AND BETA)
C
      LENRL = NOV
      CALL SYMRL(MOLABS,XX(IZ),XX(IRLAG),LENRL,NOCC,NOP1,NMOS)
      LEN = LENRL
      LENRLB = NOVB
      CALL SYMRL(MOLABSB,XX(IZB),XX(IRLAGB),LENRLB,NOCCB,NOP1B,NMOS)
      LENB = LENRLB
C
C  CREATE THE DM FOR STORING THE TRIAL VECTORS
C
      MAXC = MAXITC
      CALL DDI_CREATE(LEN,MAXC,D_U)
      CALL DDI_CREATE(LENB,MAXC,D_UB)
      CALL DDI_CREATE(LEN,MAXC,D_E)
      CALL DDI_CREATE(LENB,MAXC,D_EB)
      MAXC2 = (MAXC*MAXC+MAXC)/2
C
C  ALLOCATE WORKSPACES FOR CPHF SOLVER
C
      CALL VALFM(LOADFM)
      ITMP1  = 1      + LOADFM
      ITMP2  = ITMP1  + LEN                               ! U
      ITMP3  = ITMP2  + LEN                               ! UNXT
      ITMP4  = ITMP3  + LEN                               ! PRHS
      ITMP5  = ITMP4  + MAXC2                             ! B
      ITMP6  = ITMP5  + MAXC                              ! CC
      ITMP7  = ITMP6  + MAXC                              ! IPVT
      ITMP8  = ITMP7  + NVSQ                              ! BUF
      ITMP9  = ITMP8  + NVSQ                              ! BUF1
      ITMP10 = ITMP9  + NVTR                              ! BUF2
      ITMP11 = ITMP10 + MAXC*MAXC                         ! ALPHA
      ITMP12 = ITMP11 + LENB                              ! UB
      ITMP13 = ITMP12 + LENB                              ! UNXTB
      ITMP14 = ITMP13 + LENB                              ! PRHSB
      ITMP15 = ITMP14 + NVIRB*MAX0(NVIRB,NVIR)            ! BUFB
      ITMP16 = ITMP15 + NVIRB*MAX0(NVIRB,NVIR)            ! BUF1B
      LAST   = ITMP16 + NVTB                              ! BUF2B
      NEED   = LAST   - LOADFM
      CALL GETFM(NEED)
C
C  NOW SOLVE FOR THE REDUCED Z-VECTOR
C
      CALL UZVECTR(EORBA,EORBB,XX(IRLAG),XX(IRLAGB)
     *,            XX(ITMP1),XX(ITMP2),XX(ITMP3)
     *,            XX(ITMP4),XX(ITMP5),XX(ITMP6)
     *,            XX(ITMP7),XX(ITMP8),XX(ITMP9)
     *,            XX(ITMP10),XX(ITMP11),XX(ITMP12)
     *,            XX(ITMP13),XX(ITMP14),XX(ITMP15)
     *,            XX(ITMP16)
     *,            MOLABS,MOLABSB,XX(NSYM),XX(NVSM),XX(LVSM)
     *,            XX(NSYMB),XX(NVSMB),XX(LVSMB)
     *,            MAXC,LEN,LENB,MXIRR)
      CALL RETFM(NEED)
      CALL DDI_DESTROY( D_EB )
      CALL DDI_DESTROY( D_E )
      CALL DDI_DESTROY( D_UB )
      CALL DDI_DESTROY( D_U )
      CALL RETFM(NEEDS)
C
C  RESTORE SOLUTION VECTOR TO FULL P(OV)
C
      CALL SYMPOV(MOLABS,XX(IZ),XX(IRLAG),LENRL,NOCC,NOP1,NMOS)
      CALL SYMPOV(MOLABSB,XX(IZB),XX(IRLAGB),LENRLB,NOCCB,NOP1B,NMOS)
C
C  IN CASE OF NON-ABELIAN GROUPS...
C
      NT = NTSAVE
      IF (OUTPUT) THEN
        WRITE(ICHANL,3)
        CALL FLSHBF(ICHANL)
      END IF
C
C  TERMS OF THE W(2) DEPENDENT ON THE P(OV)
C
      CALL VALFM(LOADFM)
      ITMP1 = 1     + LOADFM
      ITMP2 = ITMP1 + NOSQ
      ITMP3 = ITMP2 + NOSB
      LAST  = ITMP3 + MAX0( NOSQ, MAX0( NOSB, MAX0(NVIR,NVIRB)))
      NEED  = LAST  - LOADFM
      CALL GETFM(NEED)
      CALL UWOV(PMAT,PMATB,XX(IZ),XX(IZB),WMAT,WMATB,EORBA,EORBB,
     *          XX(ITMP1),XX(ITMP2),XX(ITMP3))
      CALL RETFM(NEED)
      CALL RETFM(NEEDZ)
C
C  DONE WITH THE (VV|OO) AND (VO|OO) INTEGRALS
C
      CALL DDI_DESTROY( D_VVOOBB )
      CALL DDI_DESTROY( D_VVOOBA )
      CALL DDI_DESTROY( D_VVOOAB )
      CALL DDI_DESTROY( D_VVOO   )
      CALL DDI_DESTROY( D_VOOOBB )
      CALL DDI_DESTROY( D_VOOOBA )
      CALL DDI_DESTROY( D_VOOOAB )
      CALL DDI_DESTROY( D_VOOO   )
C
C  FINISH OFF 1-PARTICLE DENSITY MATRICES
C
      CALL VALFM(LOADFM)
      ITMP1 = 1     + LOADFM
      ITMP2 = ITMP1 + NBSQ
      LAST  = ITMP2 + NBSQ
      NEED  = LAST  - LOADFM
      CALL GETFM(NEED)
C
C  BACK-TRANSFORM P(2) ALPHA AND BETA TO AO-BASIS
C  ALPHA:
C
      CALL CACT(NBF,NMOS
     *,         PMAT,NBF
     *,         CMOA,NBF
     *,         XX(ITMP1),NBF,XX(ITMP2))
C
C  SYMMETRISE P(2) AND PACK INTO TRIANGLE
C
      CALL SQTR(XX(ITMP1),PMAT,NBF)
C
C  NOW BETA:
C
      CALL CACT(NBF,NMOS
     *,         PMATB,NBF
     *,         CMOB,NBF
     *,         XX(ITMP1),NBF,XX(ITMP2))
C
C  SYMMETRISE P(2) AND PACK INTO TRIANGLE
C
      CALL SQTR(XX(ITMP1),PMATB,NBF)
C
C  FORM P(SCF)
C  ALPHA:
C
      CALL DGEMM('N','T',  NBF,NBF,NOCC
     *,           ONE,CMOA,NBF
     *,           CMOA,NBF
     *,           ZERO,XX(ITMP1),NBF)
C
C  SYMMETRISE ALPHA P(SCF) (TRIANGLE)
C
      CALL SQTR(XX(ITMP1),PSCFA,NBF)
C
C  BETA:
C
      CALL DGEMM('N','T',  NBF,NBF,NOCCB
     *,           ONE,CMOB,NBF
     *,           CMOB,NBF
     *,           ZERO,XX(ITMP1),NBF)
C
C  SYMMETRISE BETA P(SCF) (TRIANGLE)
C
      CALL SQTR(XX(ITMP1),PSCFB,NBF)
C
C  SAVE P(SCF) ALPHA ON DAF RECORD 308
C
      CALL DAWRIT(IDAF,IODA,PSCFA,NBTR,308,0)
C
C  SAVE P(SCF) BETA ON DAF RECORD 368
C
      CALL DAWRIT(IDAF,IODA,PSCFB,NBTR,368,0)
C
C  ALPHA:
C  P(MP2) = P(2) + P(SCF)
C
      CALL DCOPY(NBTR,PSCFA,1,XX(ITMP1),1)
      CALL DAXPY(NBTR,ONE,PMAT,1,XX(ITMP1),1)
C
C  SAVE P(MP2) TO DISC
C
      CALL DAWRIT(IDAF,IODA,XX(ITMP1),NBTR,16,0)
C
C  BETA:
C  P(MP2) = P(2) + P(SCF)
C
      CALL DCOPY(NBTR,PSCFB,1,XX(ITMP1),1)
      CALL DAXPY(NBTR,ONE,PMATB,1,XX(ITMP1),1)
C
C  SAVE P(MP2) TO DISC
C
      CALL DAWRIT(IDAF,IODA,XX(ITMP1),NBTR,20,0)
C
C  ADD W(SCF) TERM TO FORM W(MP2) IN THE MO BASIS
C  ALPHA:
C
      DO I = 1, NOCC
        WMAT(I,I) = WMAT(I,I)-EORBA(I)
      END DO
C
C  BETA:
C
      DO I = 1, NOCCB
        WMATB(I,I) = WMATB(I,I)-EORBB(I)
      END DO
C
C  BACK-TRANSFORM W(MP2) TO AO-BASIS (ALPHA)
C
      CALL CACT(NBF,NMOS
     *,         WMAT,NBF
     *,         CMOA,NBF
     *,         WMAT,NBF,XX(ITMP1))
C
C  SYMMETRISE W(MP2) (ALPHA)
C
      CALL SQTR(WMAT,XX(ITMP1),NBF)
C
C  SAVE W(MP2) (ALPHA) TO DISC
C
      CALL DAWRIT(IDAF,IODA,XX(ITMP1),NBTR,309,0)
C
C  BACK-TRANSFORM W(MP2) TO AO-BASIS (BETA)
C
      CALL CACT(NBF,NMOS
     *,         WMATB,NBF
     *,         CMOB,NBF
     *,         WMATB,NBF,XX(ITMP1))
C
C  SYMMETRISE W(MP2) (BETA)
C
      CALL SQTR(WMATB,XX(ITMP1),NBF)
C
C  SAVE W(MP2) (BETA) TO DISC
C
      CALL DAWRIT(IDAF,IODA,XX(ITMP1),NBTR,369,0)
C
C  FREE WORKSPACE
C
      CALL RETFM(NEED)
C
C  RESET DLB COUNTER - THIS SEEMS TO BE NEEDED FOR
C  RUNNING THE 1-PARTICLE GRADIENT IN PARALLEL
C
      IF (IBTYP.EQ.1) CALL DDI_DLBRESET()
      RETURN
1     FORMAT(/,6X,'COMPUTING CONTRIBUTIONS TO THE 1-PARTICLE DENSITY',
     *            ' MATRICES')
2     FORMAT(/,6X,'SOLVING FOR Z-VECTOR')
3     FORMAT(/,6X,'COMPUTING LAST CONTRIBUTIONS TO THE 1-PARTICLE',
     *            ' DENSITY')
      END
C*MODULE UMPDDI   *DECK WPUMP2
      SUBROUTINE WPUMP2(PMAT,PMATB,WMAT,WMATB,EORBA,EORBB,BUF1,BUF2)
C
C -----------------------------------------------------------------
C  CONSTRUCT TERMS OF W(2) AND P(2) (BASED ON WP)
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO=0.0D+00, HALF=0.5D+00, ONE=1.0D+00)
      LOGICAL GOPARR,DSKWRK,MASWRK,OUTPUT,MP2ENG,UMP2GD
      INTEGER A,B,AB,BA
      DOUBLE PRECISION PMAT(NBF,NBF),WMAT(NBF,NBF),EORBA(NBF)
     *,      PMATB(NBF,NBF),WMATB(NBF,NBF),EORBB(NBF)
     *,      BUF1(NBF*NBF),BUF2(NBF*NBF)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
      INTEGER         D_UB,D_E,D_EB
      INTEGER         D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB,
     *                D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      LOGICAL         NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB,
     *                NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
     *,               NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /UMPDMS/ D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB
     *,               D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
     *,               NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB
     *,               NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
     *,               D_UB,D_E,D_EB
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
C
      CALL DCOPY(NBSQ,ZERO,0,WMAT,1)
      CALL DCOPY(NBSQ,ZERO,0,PMAT,1)
      CALL DCOPY(NBSQ,ZERO,0,WMATB,1)
      CALL DCOPY(NBSQ,ZERO,0,PMATB,1)
      CALL DDI_DISTRIB(D_VOVO,ME,IL_VOVO,IH_VOVO,JL_VOVO,JH_VOVO)
      CALL DDI_DISTRIB(D_VOVOAB,ME,IL_VOVO,IH_VOVO,JL_VOVOAB,JH_VOVOAB)
      CALL DDI_DISTRIB(D_VOVOBB,ME,IL_VOVO,IH_VOVO,JL_VOVOBB,JH_VOVOBB)
      CALL DDI_DISTRIB(D_VVOOAB,ME,IL_VOVO,IH_VOVO,JL_VVOOAB,JH_VVOOAB)
      CALL DDI_DISTRIB(D_VVOOBA,ME,IL_VOVO,IH_VOVO,JL_VVOOBA,JH_VVOOBA)
C
C  TERMS INVOLVING VOVO-TYPE INTEGRALS ONLY:
C  VIRTUAL BLOCK OF P(2) AND TERM OF VIRTUAL BLOCK OF W(2) (ALL ALPHA)
C
      DO I = NCP1, NOCC
       DO J = NCP1, I
        IJ = (I*I-I)/2 + J
        IF (IJ.GE.JL_VOVO.AND.IJ.LE.JH_VOVO) THEN
         CALL DDI_GET(D_VOVO,1,NVSQ,IJ,IJ,BUF2)
         BA = 0
         DO A = 1, NVIR
          AB = A - NVIR
          DO B = 1, NVIR
           BA = BA + 1
           AB = AB + NVIR
           BUF1(BA) = (BUF2(AB)-BUF2(BA))/
     *              (EORBA(I)+EORBA(J)-EORBA(NOCC+A)-EORBA(NOCC+B))
          END DO
         END DO
         CALL DGEMM('N','N',NVIR,NVIR,NVIR
     *,             -ONE,BUF2,NVIR
     *,             BUF1,NVIR
     *,             ONE,WMAT(NOP1,NOP1),NBF)
         IF (I.NE.J) THEN
          CALL DGEMM('T','T',NVIR,NVIR,NVIR
     *,              -ONE,BUF2,NVIR
     *,              BUF1,NVIR
     *,              ONE,WMAT(NOP1,NOP1),NBF)
         END IF
         BA = 0
         DO A = 1, NVIR
          AB = A - NVIR
          DO B = 1, NVIR
           BA = BA + 1
           AB = AB + NVIR
           BUF2(BA) = BUF2(BA)/
     *              (EORBA(I)+EORBA(J)-EORBA(NOCC+A)-EORBA(NOCC+B))
          END DO
         END DO
         CALL DGEMM('T','T',NVIR,NVIR,NVIR
     *,             ONE,BUF1,NVIR
     *,             BUF2,NVIR
     *,             ONE,PMAT(NOP1,NOP1),NBF)
         IF (I.NE.J) THEN
          CALL DGEMM('N','N',NVIR,NVIR,NVIR
     *,              ONE,BUF1,NVIR
     *,              BUF2,NVIR
     *,              ONE,PMAT(NOP1,NOP1),NBF)
         END IF
        END IF
       END DO
      END DO
C
C     NOW GET CROSS TERM CONTRIBUTIONS TO PVIRT AND WVIRT
C
      DO J = NCP1B, NOCCB
       DO I = NCP1, NOCC
        IJ = (J-1)*NOCC + I
        IF (IJ.GE.JL_VOVOAB.AND.IJ.LE.JH_VOVOAB) THEN
         CALL DDI_GET(D_VOVOAB,1,NVIRB*NVIR,IJ,IJ,BUF2)
C
C      DO (BETA,ALPHA) CONTRIBUTION TO PVIRT AND WVIRT
C
         BA = 0
         DO A = 1, NVIR
          AB = A - NVIR
          DO B = 1, NVIRB
           BA = BA + 1
           AB = AB + NVIR
           BUF1(AB) = BUF2(BA)/
     *              (EORBA(I)+EORBB(J)-EORBA(NOCC+A)-EORBB(NOCCB+B))
          END DO
         END DO
         CALL DGEMM('N','N',NVIRB,NVIRB,NVIR
     *,             -ONE,BUF2,NVIRB
     *,             BUF1,NVIR
     *,             ONE,WMATB(NOP1B,NOP1B),NBF)
         CALL DGEMM('T','N',NVIRB,NVIRB,NVIR
     *,             ONE,BUF1,NVIR
     *,             BUF1,NVIR
     *,             ONE,PMATB(NOP1B,NOP1B),NBF)
C
C      DO (ALPHA,BETA) CONTRIBUTION TO PVIRT AND WVIRT
C
         BA = 0
         DO A = 1, NVIR
          AB = A - NVIR
          DO B = 1, NVIRB
           BA = BA + 1
           AB = AB + NVIR
           BUF1(AB) = BUF2(BA)
           BUF2(BA) = BUF2(BA)/
     *              (EORBA(I)+EORBB(J)-EORBA(NOCC+A)-EORBB(NOCCB+B))
          END DO
         END DO
         CALL DGEMM('N','N',NVIR,NVIR,NVIRB
     *,             -ONE,BUF1,NVIR
     *,             BUF2,NVIRB
     *,             ONE,WMAT(NOP1,NOP1),NBF)
         CALL DGEMM('T','N',NVIR,NVIR,NVIRB
     *,             ONE,BUF2,NVIRB
     *,             BUF2,NVIRB
     *,             ONE,PMAT(NOP1,NOP1),NBF)
        END IF
       END DO
      END DO
C
C  VIRTUAL BLOCK OF P(2) AND TERM OF VIRTUAL BLOCK OF W(2) (ALL BETA)
C
      DO I = NCP1B, NOCCB
       DO J = NCP1B, I
        IJ = (I*I-I)/2 + J
        IF (IJ.GE.JL_VOVOBB.AND.IJ.LE.JH_VOVOBB) THEN
         CALL DDI_GET(D_VOVOBB,1,NVSB,IJ,IJ,BUF2)
         BA = 0
         DO A = 1, NVIRB
          AB = A - NVIRB
          DO B = 1, NVIRB
           BA = BA + 1
           AB = AB + NVIRB
           BUF1(BA) = (BUF2(AB)-BUF2(BA))/
     *              (EORBB(I)+EORBB(J)-EORBB(NOCCB+A)-EORBB(NOCCB+B))
          END DO
         END DO
         CALL DGEMM('N','N',NVIRB,NVIRB,NVIRB
     *,             -ONE,BUF2,NVIRB
     *,             BUF1,NVIRB
     *,             ONE,WMATB(NOP1B,NOP1B),NBF)
         IF (I.NE.J) THEN
          CALL DGEMM('T','T',NVIRB,NVIRB,NVIRB
     *,              -ONE,BUF2,NVIRB
     *,              BUF1,NVIRB
     *,              ONE,WMATB(NOP1B,NOP1B),NBF)
         END IF
         BA = 0
         DO A = 1, NVIRB
          AB = A - NVIRB
          DO B = 1, NVIRB
           BA = BA + 1
           AB = AB + NVIRB
           BUF2(BA) = BUF2(BA)/
     *              (EORBB(I)+EORBB(J)-EORBB(NOCCB+A)-EORBB(NOCCB+B))
          END DO
         END DO
         CALL DGEMM('T','T',NVIRB,NVIRB,NVIRB
     *,             ONE,BUF1,NVIRB
     *,             BUF2,NVIRB
     *,             ONE,PMATB(NOP1B,NOP1B),NBF)
         IF (I.NE.J) THEN
          CALL DGEMM('N','N',NVIRB,NVIRB,NVIRB
     *,              ONE,BUF1,NVIRB
     *,              BUF2,NVIRB
     *,              ONE,PMATB(NOP1B,NOP1B),NBF)
         END IF
        END IF
       END DO
      END DO
C
C  OCCUPIED BLOCK OF P(2) AND TERM OF OCCUPIED BLOCK OF W(2) (ALL ALPHA)
C
      NCPOFFA = NACORE*NOCC + NCP1
      NCAOFFA = NACORE*NOCC + 1
      ITASK = 0
      DO A = 1, NVIR
       DO B = 1, A
         ITASK = ITASK + 1
         IF (MOD(ITASK,NPROC).EQ.ME) THEN
C
C  GET BLOCK OF VOVO INTEGRALS FOR ALL OCC-INDICES
C
         AB = (A-1)*NVIR + B
         CALL DDI_GET(D_VOVO,AB,AB,1,NOTR,BUF1)
         IC = 0
         DO I = 1, NOCC
           DO J = 1, I
             IC = IC + 1
             JI = (J-1)*NOCC + I
             IJ = (I-1)*NOCC + J
             BUF2(JI) = BUF1(IC)
             BUF2(IJ) = BUF1(IC)
           END DO
         END DO
         IF (A.NE.B) THEN
           BA = (B-1)*NVIR + A
           CALL DDI_GET(D_VOVO,BA,BA,1,NOTR,BUF1)
           IC = 0
           DO I = 1, NOCC
             DO J = 1, I
               IC = IC + 1
               IJ = (I-1)*NOCC + J
               BUF2(IJ) = BUF1(IC)
             END DO
           END DO
         END IF
         DO I = NCP1, NOCC
          DO J = NCP1, NOCC
           IJ = (I-1)*NOCC + J
           JI = (J-1)*NOCC + I
           BUF1(IJ) = (BUF2(JI)-BUF2(IJ))/
     *       (EORBA(I)+EORBA(J)-EORBA(NOCC+A)-EORBA(NOCC+B))
          END DO
         END DO
         CALL DGEMM('N','N',NACT,NACT,NACT
     *,             -ONE,BUF2(NCPOFFA),NOCC
     *,             BUF1(NCPOFFA),NOCC
     *,             ONE,WMAT(NCP1,NCP1),NBF)
         IF (A.NE.B) THEN
          CALL DGEMM('T','T',NACT,NACT,NACT
     *,              -ONE,BUF2(NCPOFFA),NOCC
     *,              BUF1(NCPOFFA),NOCC
     *,              ONE,WMAT(NCP1,NCP1),NBF)
         END IF
         CALL DGEMM('N','N',NACORE,NACT,NACT
     *,             -ONE,BUF2(NCAOFFA),NOCC
     *,             BUF1(NCPOFFA),NOCC
     *,             ONE,PMAT(1,NCP1),NBF)
         IF (A.NE.B) THEN
           CALL DGEMM('T','T',NACORE,NACT,NACT
     *,               -ONE,BUF2(NCP1),NOCC
     *,               BUF1(NCPOFFA),NOCC
     *,               ONE,PMAT(1,NCP1),NBF)
         END IF
         DO I = NCP1, NOCC
          DO J = NCP1, NOCC
           IJ = (I-1)*NOCC + J
           BUF2(IJ) = BUF2(IJ)/
     *       (EORBA(I)+EORBA(J)-EORBA(NOCC+A)-EORBA(NOCC+B))
          END DO
         END DO
         CALL DGEMM('T','T',NACT,NACT,NACT
     *,             -ONE,BUF1(NCPOFFA),NOCC
     *,             BUF2(NCPOFFA),NOCC
     *,             ONE,PMAT(NCP1,NCP1),NBF)
         IF (A.NE.B) THEN
          CALL DGEMM('N','N',NACT,NACT,NACT
     *,              -ONE,BUF1(NCPOFFA),NOCC
     *,              BUF2(NCPOFFA),NOCC
     *,              ONE,PMAT(NCP1,NCP1),NBF)
         END IF
        END IF
       END DO
      END DO
C
C  OCCUPIED BLOCK OF P(2) AND TERM OF OCCUPIED BLOCK OF W(2) (ALL BETA)
C
      NCPOFFB = NBCORE*NOCCB + NCP1B
      NCAOFFB = NBCORE*NOCCB + 1
      ITASK = 0
      DO A = 1, NVIRB
       DO B = 1, A
         ITASK = ITASK + 1
         IF (MOD(ITASK,NPROC).EQ.ME) THEN
C
C  GET BLOCK OF VOVO INTEGRALS FOR ALL OCC-INDICES
C
         AB = (A-1)*NVIRB + B
         CALL DDI_GET(D_VOVOBB,AB,AB,1,NOTB,BUF1)
         IC = 0
         DO I = 1, NOCCB
           DO J = 1, I
             IC = IC + 1
             JI = (J-1)*NOCCB + I
             IJ = (I-1)*NOCCB + J
             BUF2(JI) = BUF1(IC)
             BUF2(IJ) = BUF1(IC)
           END DO
         END DO
         IF (A.NE.B) THEN
           BA = (B-1)*NVIRB + A
           CALL DDI_GET(D_VOVOBB,BA,BA,1,NOTB,BUF1)
           IC = 0
           DO I = 1, NOCCB
             DO J = 1, I
               IC = IC + 1
               IJ = (I-1)*NOCCB + J
               BUF2(IJ) = BUF1(IC)
             END DO
           END DO
         END IF
         DO I = NCP1B, NOCCB
          DO J = NCP1B, NOCCB
           IJ = (I-1)*NOCCB + J
           JI = (J-1)*NOCCB + I
           BUF1(IJ) = (BUF2(JI)-BUF2(IJ))/
     *       (EORBB(I)+EORBB(J)-EORBB(NOCCB+A)-EORBB(NOCCB+B))
          END DO
         END DO
         CALL DGEMM('N','N',NACTB,NACTB,NACTB
     *,             -ONE,BUF2(NCPOFFB),NOCCB
     *,             BUF1(NCPOFFB),NOCCB
     *,             ONE,WMATB(NCP1B,NCP1B),NBF)
         IF (A.NE.B) THEN
          CALL DGEMM('T','T',NACTB,NACTB,NACTB
     *,              -ONE,BUF2(NCPOFFB),NOCCB
     *,              BUF1(NCPOFFB),NOCCB
     *,              ONE,WMATB(NCP1B,NCP1B),NBF)
         END IF
         CALL DGEMM('N','N',NBCORE,NACTB,NACTB
     *,             -ONE,BUF2(NCAOFFB),NOCCB
     *,             BUF1(NCPOFFB),NOCCB
     *,             ONE,PMATB(1,NCP1B),NBF)
         IF (A.NE.B) THEN
           CALL DGEMM('T','T',NBCORE,NACTB,NACTB
     *,               -ONE,BUF2(NCP1B),NOCCB
     *,               BUF1(NCPOFFB),NOCCB
     *,               ONE,PMATB(1,NCP1B),NBF)
         END IF
         DO I = NCP1B, NOCCB
          DO J = NCP1B, NOCCB
           IJ = (I-1)*NOCCB + J
           BUF2(IJ) = BUF2(IJ)/
     *       (EORBB(I)+EORBB(J)-EORBB(NOCCB+A)-EORBB(NOCCB+B))
          END DO
         END DO
         CALL DGEMM('T','T',NACTB,NACTB,NACTB
     *,             -ONE,BUF1(NCPOFFB),NOCCB
     *,             BUF2(NCPOFFB),NOCCB
     *,             ONE,PMATB(NCP1B,NCP1B),NBF)
         IF (A.NE.B) THEN
          CALL DGEMM('N','N',NACTB,NACTB,NACTB
     *,              -ONE,BUF1(NCPOFFB),NOCCB
     *,              BUF2(NCPOFFB),NOCCB
     *,              ONE,PMATB(NCP1B,NCP1B),NBF)
         END IF
        END IF
       END DO
      END DO
C
C  OCCUPIED BLOCK OF P(2)
C  AND TERM OF OCCUPIED BLOCK OF W(2) (CROSS TERMS)
C
      NCPOFF1 = NBCORE*NOCC + NCP1
      NCPOFF2 = NACORE*NOCCB+ NCP1B
      NCAOFF1 = NACORE*NOCC + 1
      ITASK = 0
      DO A = 1, NVIR
       DO B = 1, NVIRB
         ITASK = ITASK + 1
         IF (MOD(ITASK,NPROC).EQ.ME) THEN
C
C  GET BLOCK OF VOVO INTEGRALS FOR ALL OCC-INDICES
C
         AB = (A-1)*NVIRB + B
         CALL DDI_GET(D_VOVOAB,AB,AB,1,NOCC*NOCCB,BUF2)
         DO J = NCP1B, NOCCB
          DO I = NCP1, NOCC
           IJ = (J-1)*NOCC  + I
           JI = (I-1)*NOCCB + J
           BUF1(JI) = BUF2(IJ)/
     *       (EORBA(I)+EORBB(J)-EORBA(NOCC+A)-EORBB(NOCCB+B))
          END DO
         END DO
C
C  CROSS TERM 1
C
         CALL DGEMM('N','N',NACT,NACT,NACTB
     *,             -ONE,BUF2(NCPOFF1),NOCC
     *,             BUF1(NCPOFF2),NOCCB
     *,             ONE,WMAT(NCP1,NCP1),NBF)
         CALL DGEMM('N','N',NACORE,NACT,NACTB
     *,             -ONE,BUF2(NCAOFF1),NOCC
     *,             BUF1(NCPOFF2),NOCCB
     *,             ONE,PMAT(1,NCP1),NBF)
C
C  CROSS TERM 2
C
         CALL DGEMM('T','T',NACTB,NACTB,NACT
     *,             -ONE,BUF2(NCPOFF1),NOCC
     *,             BUF1(NCPOFF2),NOCCB
     *,             ONE,WMATB(NCP1B,NCP1B),NBF)
C         CALL DGEMM('N','N',NACTB,NBCORE,NACT
C     *,             -ONE,BUF1(NCPOFF2),NOCCB
C     *,             BUF2(NCP1),NOCC
C     *,             ONE,PMATB(NCP1B,1),NBF)
         CALL DGEMM('T','T',NBCORE,NACTB,NACT
     *,             -ONE,BUF2(NCP1),NOCC
     *,             BUF1(NCPOFF2),NOCCB
     *,             ONE,PMATB(1,NCP1B),NBF)
C
         DO J = NCP1B, NOCCB
          DO I = NCP1, NOCC
           IJ = (J-1)*NOCC + I
           BUF2(IJ) = BUF2(IJ)/
     *       (EORBA(I)+EORBB(J)-EORBA(NOCC+A)-EORBB(NOCCB+B))
          END DO
         END DO
C
C  CROSS TERM 1
C
         CALL DGEMM('N','N',NACT,NACT,NACTB
     *,             -ONE,BUF2(NCPOFF1),NOCC
     *,             BUF1(NCPOFF2),NOCCB
     *,             ONE,PMAT(NCP1,NCP1),NBF)
C
C  CROSS TERM 2
C
         CALL DGEMM('N','N',NACTB,NACTB,NACT
     *,             -ONE,BUF1(NCPOFF2),NOCCB
     *,             BUF2(NCPOFF1),NOCC
     *,             ONE,PMATB(NCP1B,NCP1B),NBF)
        END IF
       END DO
      END DO
C
C  CORE-VALENCE BLOCKS OF P(2) AND W(2)
C
      DO I = 1, NACORE
       DO J = NCP1, NOCC
        WMAT(I,J) = PMAT(I,J)*0.5D+00
        WMAT(J,I) = WMAT(I,J)
        PMAT(I,J) = PMAT(I,J)/(EORBA(I)-EORBA(J))
        PMAT(J,I) = PMAT(I,J)
       END DO
      END DO
C
C  CORE-VALENCE BLOCKS OF P(2) AND W(2)
C
      DO I = 1, NBCORE
       DO J = NCP1B, NOCCB
        WMATB(I,J) = PMATB(I,J)*0.5D+00
        WMATB(J,I) = WMATB(I,J)
        PMATB(I,J) = PMATB(I,J)/(EORBB(I)-EORBB(J))
        PMATB(J,I) = PMATB(I,J)
       END DO
      END DO
C
C  NOW DO A GLOBAL-SUM OF P(2)
C
      CALL DDI_GSUMF(1001,PMAT,NBSQ)
      CALL DDI_GSUMF(1003,PMATB,NBSQ)
C
C  TERMS INVOLVING VOOO-TYPE INTEGRALS:
C  OCCUPIED-VIRTUAL BLOCK OF W(2) AND LAGRANGIAN (ALL ALPHA FIRST)
C  (STORED IN THE VIRTUAL-OCCUPIED BLOCK OF W(2)).
C
      IJ = 0
      DO K = 1, NOCC
       DO J = 1, K
        IJ = IJ + 1
        IF (IJ.GE.JL_VOVO.AND.IJ.LE.JH_VOVO) THEN
         CALL DDI_GET(D_VOVO,1,NVSQ,IJ,IJ,BUF2)
         BA = 0
         DO A = 1, NVIR
          AB = A - NVIR
          DO B = 1, NVIR
           BA = BA + 1
           AB = AB + NVIR
           BUF1(BA) = (BUF2(AB)-BUF2(BA))/
     *              (EORBA(J)+EORBA(K)-EORBA(NOCC+A)-EORBA(NOCC+B))
          END DO
         END DO
         IA = 1
         DO I = 1, NOCC
          IF (I.GE.K) THEN
           JK = ((I*I-I)/2+K-1)*NOCC + J
          ELSE
           JK = ((K*K-K)/2+I-1)*NOCC + J
          END IF
          CALL DDI_GET(D_VOOO,1,NVIR,JK,JK,BUF2(IA))
          IA = IA + NVIR
         END DO
C
C  OCC-VIRT TERM OF W(2)
C
         IF (J.LE.NACORE.OR.K.LE.NACORE) THEN
C  (SKIP CORE INDICES)
         ELSE
         CALL DGEMM('T','N',NOCC,NVIR,NVIR
     *,             -ONE,BUF2,NVIR
     *,             BUF1,NVIR
     *,             ONE,WMAT(1,NOP1),NBF)
         END IF
C
C  P(2)*VOOO TERM OF LAGRANGIAN
C
         IA = 0
         DO I = 1, NOCC
          DO A = NOP1, NMOS
           IA = IA + 1
           XIAJB = BUF2(IA)
           WMAT(A,J) = WMAT(A,J) + XIAJB*PMAT(I,K)*ONE
           WMAT(A,I) = WMAT(A,I) - XIAJB*PMAT(J,K)*HALF
           WMAT(A,K) = WMAT(A,K) - XIAJB*PMAT(J,I)*HALF
          END DO
         END DO
C
         IF (J.NE.K) THEN
          IA = 1
          DO I = 1, NOCC
           IF (I.GE.J) THEN
            JK = ((I*I-I)/2+J-1)*NOCC + K
           ELSE
            JK = ((J*J-J)/2+I-1)*NOCC + K
           END IF
           CALL DDI_GET(D_VOOO,1,NVIR,JK,JK,BUF2(IA))
           IA = IA + NVIR
          END DO
C
C  OCC-VIRT TERM OF W(2)
C
         IF (J.LE.NACORE.OR.K.LE.NACORE) THEN
C  (SKIP CORE INDICES)
         ELSE
          CALL DGEMM('T','T',NOCC,NVIR,NVIR
     *,              -ONE,BUF2,NVIR
     *,              BUF1,NVIR
     *,              ONE,WMAT(1,NOP1),NBF)
         END IF
C
C  P(2)*VOOO TERM OF LAGRANGIAN
C
          IA = 0
          DO I = 1, NOCC
           DO A = NOP1, NMOS
            IA = IA + 1
            XIAJB = BUF2(IA)
            WMAT(A,K) = WMAT(A,K) + XIAJB*PMAT(I,J)*ONE
            WMAT(A,I) = WMAT(A,I) - XIAJB*PMAT(K,J)*HALF
            WMAT(A,J) = WMAT(A,J) - XIAJB*PMAT(K,I)*HALF
           END DO
          END DO
         END IF
        END IF
       END DO
      END DO
C
C  TERMS INVOLVING VOOO-TYPE INTEGRALS:
C  OCCUPIED-VIRTUAL BLOCK OF W(2) AND LAGRANGIAN (ALL BETA NEXT)
C  (STORED IN THE VIRTUAL-OCCUPIED BLOCK OF W(2)).
C
      IJ = 0
      DO K = 1, NOCCB
       DO J = 1, K
        IJ = IJ + 1
        IF (IJ.GE.JL_VOVOBB.AND.IJ.LE.JH_VOVOBB) THEN
         CALL DDI_GET(D_VOVOBB,1,NVSB,IJ,IJ,BUF2)
         BA = 0
         DO A = 1, NVIRB
          AB = A - NVIRB
          DO B = 1, NVIRB
           BA = BA + 1
           AB = AB + NVIRB
           BUF1(BA) = (BUF2(AB)-BUF2(BA))/
     *              (EORBB(J)+EORBB(K)-EORBB(NOCCB+A)-EORBB(NOCCB+B))
          END DO
         END DO
         IA = 1
         DO I = 1, NOCCB
          IF (I.GE.K) THEN
           JK = ((I*I-I)/2+K-1)*NOCCB + J
          ELSE
           JK = ((K*K-K)/2+I-1)*NOCCB + J
          END IF
          CALL DDI_GET(D_VOOOBB,1,NVIRB,JK,JK,BUF2(IA))
          IA = IA + NVIRB
         END DO
C
C  OCC-VIRT TERM OF W(2)
C
         IF (J.LE.NBCORE.OR.K.LE.NBCORE) THEN
C  (SKIP CORE INDICES)
         ELSE
         CALL DGEMM('T','N',NOCCB,NVIRB,NVIRB
     *,             -ONE,BUF2,NVIRB
     *,             BUF1,NVIRB
     *,             ONE,WMATB(1,NOP1B),NBF)
         END IF
C
C  P(2)*VOOO TERM OF LAGRANGIAN
C
         IA = 0
         DO I = 1, NOCCB
          DO A = NOP1B, NMOS
           IA = IA + 1
           XIAJB = BUF2(IA)
           WMATB(A,J) = WMATB(A,J) + XIAJB*PMATB(I,K)*ONE
           WMATB(A,I) = WMATB(A,I) - XIAJB*PMATB(J,K)*HALF
           WMATB(A,K) = WMATB(A,K) - XIAJB*PMATB(J,I)*HALF
          END DO
         END DO
C
         IF (J.NE.K) THEN
          IA = 1
          DO I = 1, NOCCB
           IF (I.GE.J) THEN
            JK = ((I*I-I)/2+J-1)*NOCCB + K
           ELSE
            JK = ((J*J-J)/2+I-1)*NOCCB + K
           END IF
           CALL DDI_GET(D_VOOOBB,1,NVIRB,JK,JK,BUF2(IA))
           IA = IA + NVIRB
          END DO
C
C  OCC-VIRT TERM OF W(2)
C
         IF (J.LE.NBCORE.OR.K.LE.NBCORE) THEN
C  (SKIP CORE INDICES)
         ELSE
          CALL DGEMM('T','T',NOCCB,NVIRB,NVIRB
     *,              -ONE,BUF2,NVIRB
     *,              BUF1,NVIRB
     *,              ONE,WMATB(1,NOP1B),NBF)
         END IF
C
C  P(2)*VOOO TERM OF LAGRANGIAN
C
          IA = 0
          DO I = 1, NOCCB
           DO A = NOP1B, NMOS
            IA = IA + 1
            XIAJB = BUF2(IA)
            WMATB(A,K) = WMATB(A,K) + XIAJB*PMATB(I,J)*ONE
            WMATB(A,I) = WMATB(A,I) - XIAJB*PMATB(K,J)*HALF
            WMATB(A,J) = WMATB(A,J) - XIAJB*PMATB(K,I)*HALF
           END DO
          END DO
         END IF
        END IF
       END DO
      END DO
C
C  OCCUPIED-VIRTUAL BLOCK OF W(2) AND ALPHA LAGRANGIAN (CROSS TERM 1)
C  (STORED IN THE VIRTUAL-OCCUPIED BLOCK OF W(2)).
C
      DO K = 1, NOCC
       DO J = 1, NOCCB
        IJ = (J-1)*NOCC + K
        IF (IJ.GE.JL_VOVOAB.AND.IJ.LE.JH_VOVOAB) THEN
         CALL DDI_GET(D_VOVOAB,1,NVIRB*NVIR,IJ,IJ,BUF2)
         BA = 0
         DO A = 1, NVIR
          DO B = 1, NVIRB
           BA = BA + 1
           BUF1(BA) = BUF2(BA)/
     *              (EORBB(J)+EORBA(K)-EORBA(NOCC+A)-EORBB(NOCCB+B))
          END DO
         END DO
         IA = 1
         DO I = 1, NOCC
          IF (I.GE.K) THEN
           JK = ((I*I-I)/2+K-1)*NOCCB + J
          ELSE
           JK = ((K*K-K)/2+I-1)*NOCCB + J
          END IF
          CALL DDI_GET(D_VOOOBA,1,NVIRB,JK,JK,BUF2(IA))
          IA = IA + NVIRB
         END DO
C
C  OCC-VIRT TERM OF W(2)
C
         IF (J.LE.NBCORE.OR.K.LE.NACORE) THEN
C  (SKIP CORE INDICES)
         ELSE
         CALL DGEMM('T','N',NOCC,NVIR,NVIRB
     *,             -ONE,BUF2,NVIRB
     *,             BUF1,NVIRB
     *,             ONE,WMAT(1,NOP1),NBF)
         END IF
C
C  P(2)*VOOO TERM OF LAGRANGIAN
C
         IA = 0
         DO I = 1, NOCC
          DO A = NOP1B, NMOS
           IA = IA + 1
           XIAJB = BUF2(IA)
           WMATB(A,J) = WMATB(A,J) + XIAJB*PMAT(I,K)*ONE
          END DO
         END DO
C
C  OCCUPIED-VIRTUAL BLOCK OF W(2) AND BETA LAGRANGIAN (CROSS TERM 2)
C  (STORED IN THE VIRTUAL-OCCUPIED BLOCK OF W(2)).
C
         IA = 1
         DO I = 1, NOCCB
          IF (I.GE.J) THEN
           KJ = ((I*I-I)/2+J-1)*NOCC + K
          ELSE
           KJ = ((J*J-J)/2+I-1)*NOCC + K
          END IF
          CALL DDI_GET(D_VOOOAB,1,NVIR,KJ,KJ,BUF2(IA))
          IA = IA + NVIR
         END DO
C
C  OCC-VIRT TERM OF W(2)
C
         IF (J.LE.NBCORE.OR.K.LE.NACORE) THEN
C  (SKIP CORE INDICES)
         ELSE
         CALL DGEMM('T','T',NOCCB,NVIRB,NVIR
     *,             -ONE,BUF2,NVIR
     *,             BUF1,NVIRB
     *,             ONE,WMATB(1,NOP1B),NBF)
         END IF
C
C  P(2)*VOOO TERM OF LAGRANGIAN
C
         IA = 0
         DO I = 1, NOCCB
          DO A = NOP1, NMOS
           IA = IA + 1
           XIAJB = BUF2(IA)
           WMAT(A,K) = WMAT(A,K) + XIAJB*PMATB(I,J)*ONE
          END DO
         END DO
C
        END IF
       END DO
      END DO
C
C  TERMS INVOLVING VVOO- & OOOO-TYPE INTEGRALS: OCCUPIED BLOCK OF W(2).
C  ALL ALPHA FIRST:
C
      IJ = 0
      DO I = 1, NOCC
       DO J = 1, I
        IJ = IJ + 1
        IF (IJ.GE.JL_VOVO.AND.IJ.LE.JH_VOVO) THEN
         CALL DDI_GET(D_OOOO,1,NOTR,IJ,IJ,BUF1)
         DO K = 1, NOCC
          DO L = 1, NOCC
           KL = (K*K-K)/2 + L
           IF (K.LT.L) KL = (L*L-L)/2 + K
           XIAJB = BUF1(KL)
           WMAT(I,J) = WMAT(I,J) - XIAJB*PMAT(K,L)*ONE
           WMAT(I,K) = WMAT(I,K) + XIAJB*PMAT(J,L)*HALF
           WMAT(I,L) = WMAT(I,L) + XIAJB*PMAT(J,K)*HALF
          END DO
         END DO
         IF (I.NE.J) THEN
          DO K = 1, NOCC
           DO L = 1, NOCC
            KL = (K*K-K)/2 + L
            IF (K.LT.L) KL = (L*L-L)/2 + K
            XIAJB = BUF1(KL)
            WMAT(J,I) = WMAT(J,I) - XIAJB*PMAT(K,L)*ONE
            WMAT(J,K) = WMAT(J,K) + XIAJB*PMAT(I,L)*HALF
            WMAT(J,L) = WMAT(J,L) + XIAJB*PMAT(I,K)*HALF
           END DO
          END DO
         END IF
         CALL DDI_GET(D_VVOO,1,NVTR,IJ,IJ,BUF1)
         CALL DDI_GET(D_VOVO,1,NVSQ,IJ,IJ,BUF2)
         DO A = 1, NVIR
          DO B = 1, NVIR
           AB = (A*A-A)/2 + B
           IF (A.LT.B) AB = (B*B-B)/2 + A
           XIAJB = BUF1(AB)
           WMAT(I,J) = WMAT(I,J) - XIAJB*PMAT(A+NOCC,B+NOCC)*ONE
           AB = (B-1)*NVIR + A
           XIAJB = BUF2(AB)
           WMAT(I,J) = WMAT(I,J) + XIAJB*PMAT(A+NOCC,B+NOCC)*HALF
           WMAT(J,I) = WMAT(J,I) + XIAJB*PMAT(A+NOCC,B+NOCC)*HALF
          END DO
         END DO
         IF (I.NE.J) THEN
          DO A = 1, NVIR
           DO B = 1, NVIR
            AB = (A*A-A)/2 + B
            IF (A.LT.B) AB = (B*B-B)/2 + A
            XIAJB = BUF1(AB)
            WMAT(J,I) = WMAT(J,I) - XIAJB*PMAT(A+NOCC,B+NOCC)*ONE
            AB = (A-1)*NVIR + B
            XIAJB = BUF2(AB)
            WMAT(J,I) = WMAT(J,I) + XIAJB*PMAT(A+NOCC,B+NOCC)*HALF
            WMAT(I,J) = WMAT(I,J) + XIAJB*PMAT(A+NOCC,B+NOCC)*HALF
           END DO
          END DO
         END IF
        END IF
       END DO
      END DO
C
C  CROSS TERM 1 NEXT:
C
      IJ = 0
      DO I = 1, NOCC
       DO J = 1, I
        IJ = IJ + 1
        IF (IJ.GE.JL_VVOOBA.AND.IJ.LE.JH_VVOOBA) THEN
         CALL DDI_GET(D_OOOOAB,1,NOTB,IJ,IJ,BUF1)
         DO K = 1, NOCCB
          DO L = 1, NOCCB
           KL = (K*K-K)/2 + L
           IF (K.LT.L) KL = (L*L-L)/2 + K
           XIAJB = BUF1(KL)
           WMAT(I,J) = WMAT(I,J) - XIAJB*PMATB(K,L)*ONE
           IF (I.NE.J) THEN
            WMAT(J,I) = WMAT(J,I) - XIAJB*PMATB(K,L)*ONE
           END IF
          END DO
         END DO
         CALL DDI_GET(D_VVOOBA,1,NVTB,IJ,IJ,BUF1)
         DO A = 1, NVIRB
          DO B = 1, NVIRB
           AB = (A*A-A)/2 + B
           IF (A.LT.B) AB = (B*B-B)/2 + A
           XIAJB = BUF1(AB)
           WMAT(I,J) = WMAT(I,J) - XIAJB*PMATB(A+NOCCB,B+NOCCB)*ONE
           IF (I.NE.J) THEN
            WMAT(J,I) = WMAT(J,I) - XIAJB*PMATB(A+NOCCB,B+NOCCB)*ONE
           END IF
          END DO
         END DO
        END IF
       END DO
      END DO
C
C  CROSS TERM 2 NEXT:
C
      IJ = 0
      DO I = 1, NOCCB
       DO J = 1, I
        IJ = IJ + 1
        IF (IJ.GE.JL_VVOOAB.AND.IJ.LE.JH_VVOOAB) THEN
         CALL DDI_GET(D_OOOOAB,IJ,IJ,1,NOTR,BUF1)
         DO K = 1, NOCC
          DO L = 1, NOCC
           KL = (K*K-K)/2 + L
           IF (K.LT.L) KL = (L*L-L)/2 + K
           XIAJB = BUF1(KL)
           WMATB(I,J) = WMATB(I,J) - XIAJB*PMAT(K,L)*ONE
           IF (I.NE.J) THEN
            WMATB(J,I) = WMATB(J,I) - XIAJB*PMAT(K,L)*ONE
           END IF
          END DO
         END DO
         CALL DDI_GET(D_VVOOAB,1,NVTR,IJ,IJ,BUF1)
         DO A = 1, NVIR
          DO B = 1, NVIR
           AB = (A*A-A)/2 + B
           IF (A.LT.B) AB = (B*B-B)/2 + A
           XIAJB = BUF1(AB)
           WMATB(I,J) = WMATB(I,J) - XIAJB*PMAT(A+NOCC,B+NOCC)*ONE
           IF (I.NE.J) THEN
            WMATB(J,I) = WMATB(J,I) - XIAJB*PMAT(A+NOCC,B+NOCC)*ONE
           END IF
          END DO
         END DO
        END IF
       END DO
      END DO
C
C  LAST, ALL BETA:
C
      IJ = 0
      DO I = 1, NOCCB
       DO J = 1, I
        IJ = IJ + 1
        IF (IJ.GE.JL_VOVOBB.AND.IJ.LE.JH_VOVOBB) THEN
         CALL DDI_GET(D_OOOOBB,1,NOTB,IJ,IJ,BUF1)
         DO K = 1, NOCCB
          DO L = 1, NOCCB
           KL = (K*K-K)/2 + L
           IF (K.LT.L) KL = (L*L-L)/2 + K
           XIAJB = BUF1(KL)
           WMATB(I,J) = WMATB(I,J) - XIAJB*PMATB(K,L)*ONE
           WMATB(I,K) = WMATB(I,K) + XIAJB*PMATB(J,L)*HALF
           WMATB(I,L) = WMATB(I,L) + XIAJB*PMATB(J,K)*HALF
          END DO
         END DO
         IF (I.NE.J) THEN
          DO K = 1, NOCCB
           DO L = 1, NOCCB
            KL = (K*K-K)/2 + L
            IF (K.LT.L) KL = (L*L-L)/2 + K
            XIAJB = BUF1(KL)
            WMATB(J,I) = WMATB(J,I) - XIAJB*PMATB(K,L)*ONE
            WMATB(J,K) = WMATB(J,K) + XIAJB*PMATB(I,L)*HALF
            WMATB(J,L) = WMATB(J,L) + XIAJB*PMATB(I,K)*HALF
           END DO
          END DO
         END IF
         CALL DDI_GET(D_VVOOBB,1,NVTB,IJ,IJ,BUF1)
         CALL DDI_GET(D_VOVOBB,1,NVSB,IJ,IJ,BUF2)
         DO A = 1, NVIRB
          DO B = 1, NVIRB
           AB = (A*A-A)/2 + B
           IF (A.LT.B) AB = (B*B-B)/2 + A
           XIAJB = BUF1(AB)
           WMATB(I,J) = WMATB(I,J) - XIAJB*PMATB(A+NOCCB,B+NOCCB)*ONE
           AB = (B-1)*NVIRB + A
           XIAJB = BUF2(AB)
           WMATB(I,J) = WMATB(I,J) + XIAJB*PMATB(A+NOCCB,B+NOCCB)*HALF
           WMATB(J,I) = WMATB(J,I) + XIAJB*PMATB(A+NOCCB,B+NOCCB)*HALF
          END DO
         END DO
         IF (I.NE.J) THEN
          DO A = 1, NVIRB
           DO B = 1, NVIRB
            AB = (A*A-A)/2 + B
            IF (A.LT.B) AB = (B*B-B)/2 + A
            XIAJB = BUF1(AB)
            WMATB(J,I) = WMATB(J,I) - XIAJB*PMATB(A+NOCCB,B+NOCCB)*ONE
            AB = (A-1)*NVIRB + B
            XIAJB = BUF2(AB)
            WMATB(J,I) = WMATB(J,I) + XIAJB*PMATB(A+NOCCB,B+NOCCB)*HALF
            WMATB(I,J) = WMATB(I,J) + XIAJB*PMATB(A+NOCCB,B+NOCCB)*HALF
           END DO
          END DO
         END IF
        END IF
       END DO
      END DO
      RETURN
      END
C*MODULE UMPDDI   *DECK ULAGVVV
      SUBROUTINE ULAGVVV(WMAT,WMATB,PMAT,PMATB,CMO,CMOB,
     *                   EORB,EORBB,SCHWA)
C
C -----------------------------------------------------------------
C  DRIVER FOR COMPUTING TERMS OF THE Z-VECTOR 'LAGRANGIAN'
C  INVOLVING (VV|VO) INTEGRALS; UNRESTRICTED CASE (BASED ON LAGVVV)
C  (ULAGVVV,BTAMPSAA,BTAMPSBB,BTAMPSAB,UMKVVVO,SYMIKJL,TRANO,
C  TRANO_P,USUMAO,GETAMPSA,GETAMPSB,GETAMPSM,UMKVOVO)
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXGSH=30, MXG2=MXGSH*MXGSH)
      PARAMETER (MXSH=1000,MXATM=500)
      LOGICAL OUTPUT,MP2ENG,UMP2GD,ABELPT,NOCORE
      DOUBLE PRECISION WMAT(*),PMAT(NBF,*),CMO(NBF,*),EORB(*),SCHWA(*)
      DOUBLE PRECISION WMATB(*),PMATB(NBF,*),CMOB(NBF,*),EORBB(*)
      COMMON /FMCOM / XX(1)
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48)
     *,               TSYM(432),INVT(48),NT
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
C
      IF (OUTPUT) THEN
        WRITE(ICHANL,1)
        CALL FLSHBF(ICHANL)
      END IF
      CALL VALFM(LOADFM)
      IPAOAA  = 1      + LOADFM
      IPAOBB  = IPAOAA + NBSQ
      LAST    = IPAOBB + NBSQ
      NEEDP   = LAST  - LOADFM
      CALL GETFM(NEEDP)
C
C  BACK TRANSFORM AMPLITUDES
C
      CALL VALFM(LOADFM)
      ITMP1 = 1     + LOADFM
      ITMP2 = ITMP1 + NBSQ
      LAST  = ITMP2 + NBSQ
      NEED  = LAST  - LOADFM
      CALL GETFM(NEED)
C
C  CORE MO INDICES NEEDED FOR THIS TERM
C
      NOCORE = .FALSE.
      CALL BTAMPSAA(EORB,CMO,XX(ITMP1),XX(ITMP2),NOCORE)
      CALL BTAMPSBB(EORBB,CMOB,XX(ITMP1),XX(ITMP2),NOCORE)
      CALL BTAMPSAB(EORB,EORBB,CMO,CMOB,XX(ITMP1),XX(ITMP2),NOCORE)
C
C  BACK TRANSFORM P(2)
C
      CALL CACT(NBF,NVIR
     *,         PMAT(NOP1,NOP1),NBF
     *,         CMO(1,NOP1),NBF
     *,         XX(IPAOAA),NBF,XX(ITMP1))
      CALL CACT(NBF,NVIRB
     *,         PMATB(NOP1B,NOP1B),NBF
     *,         CMOB(1,NOP1B),NBF
     *,         XX(IPAOBB),NBF,XX(ITMP1))
      CALL RETFM(NEED)
C
C  COMPUTE VVVO TERMS 3/4 IN THE AO BASIS
C
      CALL VALFM(LOADFM)
      IGOUT  = 1      + LOADFM
      ITRANA = IGOUT  + MXSHL**4
      ITRANB = ITRANA + MXSHL*MXSHL*(MAX(NBF*NOCC*2,NOTB))
      IAMPSA = ITRANB + MXSHL*MXSHL*(MAX(NBF*NOCCB*2,NOTR))
      IAMPSB = IAMPSA + NOSQ*MXSHL*MXSHL
      IAMPSAB= IAMPSB + NOSB*MXSHL*MXSHL
      IAMPSBA= IAMPSAB + NOCC*NOCCB*MXSHL*MXSHL
      ILAGNA = IAMPSBA + NOCC*NOCCB*MXSHL*MXSHL
      ILAGNB = ILAGNA + NOCC*NBF
      IDDIJ  = ILAGNB + NOCCB*NBF
      LAST   = IDDIJ  + 16*MXG2
      NEED   = LAST  - LOADFM
      CALL GETFM(NEED)
C
C  IN CASE OF NON-ABELIAN POINT GROUPS ...
C
      NTSAVE = NT
      IF (.NOT.ABELPT()) NT = 1
      CALL UMKVVVO(WMAT,WMATB,XX(IPAOAA),XX(IPAOBB),CMO,CMOB,SCHWA
     *,            XX(IDDIJ),XX(IGOUT),XX(ITRANA),XX(ITRANB),XX(IAMPSA)
     *,            XX(IAMPSB),XX(IAMPSAB),XX(IAMPSBA),XX(ILAGNA)
     *,            XX(ILAGNB))
      NT = NTSAVE
      CALL RETFM(NEED)
      CALL RETFM(NEEDP)
C
C  RESTORE VOVO INTEGRALS
C
      CALL VALFM(LOADFM)
      ITMP1 = 1     + LOADFM
      ITMP2 = ITMP1 + NBSQ
      ICM1  = ITMP2 + NBSQ
      ICM1B = ICM1  + NBSQ
      LAST  = ICM1B + NBSQ
      NEED  = LAST  - LOADFM
      CALL GETFM(NEED)
      CALL UMKVOVO(XX(ITMP1),XX(ITMP2),XX(ICM1),XX(ICM1B),
     *             EORB,EORBB,CMO,CMOB)
      CALL RETFM(NEED)
      RETURN
1     FORMAT(/,6X,'COMPUTING THREE-VIRTUAL CONTRIBUTIONS TO THE',
     *            ' LAGRANGIAN')
      END
C*MODULE UMPDDI   *DECK BTAMPSAA
      SUBROUTINE BTAMPSAA(EORB,CMO,BUF,DUM,NOCORE)
C
C -----------------------------------------------------------------
C  BACK-TRANSFORM VIRTUAL INDICES OF THE UMP2 INTEGRALS
C  TO THE AO BASIS OVERWRITING THE VOVO INTEGRALS (ALL ALPHA)
C  (SIMILAR TO BTAMPS)
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXSH=1000, MXGTOT=5000)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      PARAMETER (ONE=1.0D+00)
      LOGICAL GOPARR,DSKWRK,MASWRK, NOCORE,OUTPUT,MP2ENG,UMP2GD
      DOUBLE PRECISION EORB(*),CMO(NBF,*),BUF(*),DUM(*)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
     *,               NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
C
      IF (OUTPUT) THEN
        WRITE(ICHANL,1)
        CALL FLSHBF(ICHANL)
      END IF
      NOFF = 1
      IF (NOCORE) NOFF = NCP1
C
C  LOOP OVER LOCAL VOVO INTEGRALS
C
      CALL DDI_DISTRIB(D_VOVO,ME,IL_VOVO,IH_VOVO,JL_VOVO,JH_VOVO)
      DO I = NOFF, NOCC
        DO J = NOFF, I
          IJ = (I*I-I)/2 + J
          IF (IJ.GE.JL_VOVO.AND.IJ.LE.JH_VOVO) THEN
            CALL DDI_GET(D_VOVO,1,NVSQ,IJ,IJ,BUF)
C
C  FORM A BLOCK OF MP2 AMPLITUDES SPANNING ALL VIRTUAL
C  MO INDICES
C
            KL = 0
            DO K = 1, NVIR
              DO L = 1, NVIR
                KL = KL + 1
C                KL = (L-1)*NVIR + K
C                LK = (K-1)*NVIR + L
C                X = BUF(KL)
C                Y = BUF(LK)
                DENOM = ONE/
     *          (EORB(I)+EORB(J)-EORB(K+NOCC)-EORB(L+NOCC))
                BUF(KL) = BUF(KL)*DENOM
              END DO
            END DO
C
C  BACK-TRANSFORM THE VIRTUAL INDICES
C
            CALL CACT(NBF,NVIR
     *,               BUF,NVIR,CMO(1,NOP1),NBF
     *,               BUF,NBF,DUM)
C
C  MAP AO-INDICES INTO SHELL-BLOCKS TO REDUCE NUMBER OF GETS
C
            ICOUNT = 0
            DO ISHELL = 1, NSHELL
              IBEG = KLOC(ISHELL)
              IEND = IBEG + KMAX(ISHELL) - KMIN(ISHELL)
              DO JSHELL = 1, NSHELL
                JBEG = KLOC(JSHELL)
                JEND = JBEG + KMAX(JSHELL) - KMIN(JSHELL)
                DO IAO = IBEG, IEND
                  DO JAO = JBEG, JEND
                    IJAO = (IAO-1)*NBF + JAO
                    ICOUNT = ICOUNT + 1
                    DUM(ICOUNT) = BUF(IJAO)
                  END DO
                END DO
              END DO
            END DO
            CALL DDI_PUT(D_VOVO,1,NBSQ,IJ,IJ,DUM)
          END IF
        END DO
      END DO
      RETURN
1     FORMAT(6X,'FORMING ALL ALPHA BACK-TRANSFORMED AMPLITUDES')
      END
C*MODULE UMPDDI   *DECK BTAMPSBB
      SUBROUTINE BTAMPSBB(EORBB,CMOB,BUF,DUM,NOCORE)
C
C -----------------------------------------------------------------
C  BACK-TRANSFORM VIRTUAL INDICES OF THE UMP2 INTEGRALS
C  TO THE AO BASIS OVERWRITING THE VOVO INTEGRALS (ALL BETA)
C  (SIMILAR TO BTAMPS)
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXSH=1000, MXGTOT=5000)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      PARAMETER (ONE=1.0D+00)
      LOGICAL GOPARR,DSKWRK,MASWRK, NOCORE,OUTPUT,MP2ENG,UMP2GD
      DOUBLE PRECISION EORBB(*),CMOB(NBF,*),BUF(*),DUM(*)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      INTEGER         D_UB,D_E,D_EB
      INTEGER         D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB,
     *                D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
      LOGICAL         NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB,
     *                NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
      COMMON /UMPDMS/ D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB
     *,               D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
     *,               NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB
     *,               NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
     *,               D_UB,D_E,D_EB
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
C
      IF (OUTPUT) THEN
        WRITE(ICHANL,1)
        CALL FLSHBF(ICHANL)
      END IF
      NOFF = 1
      IF (NOCORE) NOFF = NCP1B
C
C  LOOP OVER LOCAL VOVO INTEGRALS
C
      CALL DDI_DISTRIB(D_VOVOBB,ME,IL_VOVO,IH_VOVO,JL_VOVO,JH_VOVO)
      DO I = NOFF, NOCCB
        DO J = NOFF, I
          IJ = (I*I-I)/2 + J
          IF (IJ.GE.JL_VOVO.AND.IJ.LE.JH_VOVO) THEN
            CALL DDI_GET(D_VOVOBB,1,NVSB,IJ,IJ,BUF)
C
C  FORM A BLOCK OF MP2 AMPLITUDES SPANNING ALL VIRTUAL
C  MO INDICES
C
            KL = 0
            DO K = 1, NVIRB
              DO L = 1, NVIRB
                 KL = KL + 1
C                KL = (L-1)*NVIRB + K
C                LK = (K-1)*NVIRB + L
C                X = BUF(KL)
C                Y = BUF(LK)
                DENOM = ONE/
     *          (EORBB(I)+EORBB(J)-EORBB(K+NOCCB)-EORBB(L+NOCCB))
                 BUF(KL) = BUF(KL)*DENOM
              END DO
            END DO
C
C  BACK-TRANSFORM THE VIRTUAL INDICES
C
            CALL CACT(NBF,NVIRB
     *,               BUF,NVIRB,CMOB(1,NOP1B),NBF
     *,               BUF,NBF,DUM)
C
C  MAP AO-INDICES INTO SHELL-BLOCKS TO REDUCE NUMBER OF GETS
C
            ICOUNT = 0
            DO ISHELL = 1, NSHELL
              IBEG = KLOC(ISHELL)
              IEND = IBEG + KMAX(ISHELL) - KMIN(ISHELL)
              DO JSHELL = 1, NSHELL
                JBEG = KLOC(JSHELL)
                JEND = JBEG + KMAX(JSHELL) - KMIN(JSHELL)
                DO IAO = IBEG, IEND
                  DO JAO = JBEG, JEND
                    IJAO = (IAO-1)*NBF + JAO
                    ICOUNT = ICOUNT + 1
                    DUM(ICOUNT) = BUF(IJAO)
                  END DO
                END DO
              END DO
            END DO
            CALL DDI_PUT(D_VOVOBB,1,NBSQ,IJ,IJ,DUM)
          END IF
        END DO
      END DO
      RETURN
1     FORMAT(6X,'FORMING ALL BETA BACK-TRANSFORMED AMPLITUDES')
      END
C*MODULE UMPDDI   *DECK BTAMPSAB
      SUBROUTINE BTAMPSAB(EORB,EORBB,CMO,CMOB,BUF,DUM,NOCORE)
C
C -----------------------------------------------------------------
C  BACK-TRANSFORM VIRTUAL INDICES OF THE UMP2 INTEGRALS
C  TO THE AO BASIS OVERWRITING THE VOVO INTEGRALS (CROSS TERMS)
C  (SIMILAR TO BTAMPS)
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXSH=1000, MXGTOT=5000)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      PARAMETER (ONE=1.0D+00, ZERO=0.0D+00)
      LOGICAL GOPARR,DSKWRK,MASWRK, NOCORE,OUTPUT,MP2ENG,UMP2GD
      DOUBLE PRECISION EORB(*),EORBB(*),CMO(NBF,*),CMOB(NBF,*),
     *                BUF(*),DUM(*)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      INTEGER         D_UB,D_E,D_EB
      INTEGER         D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB,
     *                D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
      LOGICAL         NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB,
     *                NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
      COMMON /UMPDMS/ D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB
     *,               D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
     *,               NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB
     *,               NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
     *,               D_UB,D_E,D_EB
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
C
      IF (OUTPUT) THEN
        WRITE(ICHANL,1)
        CALL FLSHBF(ICHANL)
      END IF
      NOFFA = 1
      NOFFB = 1
      IF (NOCORE) THEN
        NOFFA = NCP1
        NOFFB = NCP1B
      END IF
C
C  LOOP OVER LOCAL VOVO INTEGRALS
C
      CALL DDI_DISTRIB(D_VOVOAB,ME,IL_VOVO,IH_VOVO,JL_VOVO,JH_VOVO)
      DO J = NOFFB, NOCCB
       DO I = NOFFA, NOCC
        IJ = (J-1)*NOCC + I
          IF (IJ.GE.JL_VOVO.AND.IJ.LE.JH_VOVO) THEN
            CALL DDI_GET(D_VOVOAB,1,NVIRB*NVIR,IJ,IJ,BUF)
C
C  FORM A BLOCK OF MP2 AMPLITUDES SPANNING ALL VIRTUAL
C  MO INDICES
C
            KL = 0
            DO K = 1, NVIR
              DO L = 1, NVIRB
                KL = KL + 1
                DENOM = ONE/
     *          (EORB(I)+EORBB(J)-EORB(K+NOCC)-EORBB(L+NOCCB))
                BUF(KL) = BUF(KL)*DENOM
              END DO
            END DO
C
C  BACK-TRANSFORM THE VIRTUAL INDICES
C
            CALL DGEMM('N','T',NVIRB,NBF,NVIR
     *,                ONE,BUF,NVIRB
     *,                CMO(1,NOP1),NBF
     *,                ZERO,DUM,NVIRB)
            CALL DGEMM('N','N',NBF,NBF,NVIRB
     *,                ONE,CMOB(1,NOP1B),NBF
     *,                DUM,NVIRB
     *,                ZERO,BUF,NBF)
C
C  MAP AO-INDICES INTO SHELL-BLOCKS TO REDUCE NUMBER OF GETS
C
            ICOUNT = 0
            DO ISHELL = 1, NSHELL
              IBEG = KLOC(ISHELL)
              IEND = IBEG + KMAX(ISHELL) - KMIN(ISHELL)
              DO JSHELL = 1, NSHELL
                JBEG = KLOC(JSHELL)
                JEND = JBEG + KMAX(JSHELL) - KMIN(JSHELL)
                DO IAO = IBEG, IEND
                  DO JAO = JBEG, JEND
                    IJAO = (IAO-1)*NBF + JAO
                    ICOUNT = ICOUNT + 1
                    DUM(ICOUNT) = BUF(IJAO)
                  END DO
                END DO
              END DO
            END DO
            CALL DDI_PUT(D_VOVOAB,1,NBSQ,IJ,IJ,DUM)
          END IF
        END DO
      END DO
C
C  SYNCHRONIZE NODES AFTER THIS OPERATION
C
      CALL DDI_SYNC(9005)
      RETURN
1     FORMAT(6X,'FORMING MIXED SPIN BACK-TRANSFORMED AMPLITUDES')
      END
C*MODULE UMPDDI   *DECK UMKVVVO
      SUBROUTINE UMKVVVO(WMAT,WMATB,PAOAA,PAOBB,CMO,CMOB,SCHWA,DDIJ
     *,                 GOUT,TRANA,TRANB,T2AOAA,T2AOBB,T2AOAB,T2AOBA
     *,                 LAGA,LAGB)
C
C -----------------------------------------------------------------
C  DIRECT VVVO TERMS (BASED ON MKVVVO)
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXSH=1000, MXGTOT=5000, MXAO=2047)
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
      LOGICAL POPLE,PACK2E,IANDJ,KANDL,SAME,GOPARR,DSKWRK,MASWRK,
     *        OUT,OUTPUT,MP2ENG,UMP2GD,DLB,DOTASK
      DOUBLE PRECISION WMAT(NBF,*),WMATB(NBF,*),PAOAA(*),PAOBB(*)
     *,       CMO(NBF,*),CMOB(NBF,*),SCHWA(*),DDIJ(*),GOUT(*)
     *,       TRANA(*),TRANB(*),T2AOAA(*),T2AOBB(*),T2AOAB(*)
     *,       T2AOBA(*),LAGA(*),LAGB(*)
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /MISC  / IANDJ,KANDL,SAME
      COMMON /GOUT  / GPOPLE(768),NORG
      COMMON /INTFIL/ NINTMX,NHEX,NTUPL,PACK2E,INTG76
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /SHLG70/ ISHELL,JSHELL,KSHELL,LSHELL,INEW,JNEW,KNEW,LNEW
      COMMON /SHLNOS/ QQ4,LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJ,KL,IJKL
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
C
      IF (OUTPUT) THEN
        WRITE(ICHANL,1)
        CALL FLSHBF(ICHANL)
      END IF
C
C  INITIALIZE PARAMETERS
C
      LTRANA = NOCC*NBF*MXSHL*MXSHL*2
      LTRANB = NOCCB*NBF*MXSHL*MXSHL*2
      MXSHL4 = MXSHL**4
      IANDJ  = .FALSE.
      KANDL  = .FALSE.
      SAME   = .FALSE.
      Q4     = ONE
      QQ4    = Q4
      NSCHWZ = 0
      NORG   = 0
      LOCTSK = 0
      CALL DCOPY(NBF*NOCC,ZERO,0,LAGA,1)
      CALL DCOPY(NBF*NOCCB,ZERO,0,LAGB,1)
C
C  DYNAMIC LOAD BALANCING
C
      DLB = IBTYP.EQ.1
      IF (DLB) THEN
        CALL DDI_DLBRESET()
        CALL DDI_DLBNEXT( MYTASK )
      END IF
C
C  II-SHELL,KK-SHELL
C
      DO II = 1, NSHELL
C
       ZMINI=1.0D+30
       IG1=KSTART(II)
       IG2=IG1+KNG(II)-1
       DO IG=IG1,IG2
          ZMINI=MIN(ZMINI,EX(IG))
       END DO
       DO KK = 1, II
C
        ZMINK=1.0D+30
        KG1=KSTART(KK)
        KG2=KG1+KNG(KK)-1
        DO KG=KG1,KG2
           ZMINK=MIN(ZMINK,EX(KG))
        END DO
C
C  SYMMETRY (IK|
C
        CALL SYMIJ(II,KK,Q2)
        IF ( Q2 .GT. ZERO ) THEN
C
C  LOAD BALANCER
C
        IF (DLB) THEN
          DOTASK = LOCTSK.EQ.MYTASK
        ELSE
          DOTASK = MOD(LOCTSK,NPROC).EQ.ME
        END IF
        IF (DOTASK) THEN
C
C  GET HALF-TRANSFORMED AMPLITUDES (ALPHA,BETA,MIXED SPIN)
C
         CALL GETAMPSA(T2AOAA,TRANA,TRANB,II,KK)
         CALL GETAMPSB(T2AOBB,TRANB,TRANA,II,KK)
         CALL GETAMPSM(T2AOAB,T2AOBA,TRANA,II,KK)
         CALL DCOPY(LTRANA,ZERO,0,TRANA,1)
         CALL DCOPY(LTRANB,ZERO,0,TRANB,1)
C
C  JJ-SHELL
C
         DO JJ = 1, NSHELL
C
         ZMINJ=1.0D+30
         JG1=KSTART(JJ)
         JG2=JG1+KNG(JJ)-1
         DO JG=JG1,JG2
            ZMINJ=MIN(ZMINJ,EX(JG))
         END DO
C
C  LL-SHELL
C
           DO LL = 1, NSHELL
C
C  SYMMETRY (IJ|KL)
C
            CALL SYMIKJL(II,JJ,KK,LL,Q4,NSHELL)
            IF ( Q4 .GT. ZERO ) THEN
             QQ4 = Q4
C
             ZMINL=1.0D+30
             LG1=KSTART(LL)
             LG2=LG1+KNG(LL)-1
             DO LG=LG1,LG2
                ZMINL=MIN(ZMINL,EX(LG))
             END DO
             ZMIN = MIN(ZMINI,ZMINJ,ZMINK,ZMINL)
C
C  SCHWARZ INEQUALITY TEST
C
             IIJJ = IA(MAX0(II,JJ)) + MIN0(II,JJ)
             KKLL = IA(MAX0(KK,LL)) + MIN0(KK,LL)
             TEST = Q4*SCHWA(IIJJ)*SCHWA(KKLL)
             IF (TEST.LT.CUTOFF) THEN
               NSCHWZ = NSCHWZ + 1
             ELSE
C
C  COMPUTE (IJ|KL) AND TRANSFORM 1 INDEX
C
C     USE POPLE CODE FOR ANY PURE SP INTEGRAL BLOCKS,
C     USE HONDO RYS POLYNOMIAL CODE FOR OTHER BLOCKS
C
              POPLE=.TRUE.
              IF(INTG76.EQ.0) POPLE=.FALSE.
              IF(KTYPE(II).GT.2) POPLE=.FALSE.
              IF(KTYPE(JJ).GT.2) POPLE=.FALSE.
              IF(KTYPE(KK).GT.2) POPLE=.FALSE.
              IF(KTYPE(LL).GT.2) POPLE=.FALSE.
              IF(ZMIN.LT.0.10D+00) POPLE=.FALSE.
              ISHELL = II
              JSHELL = JJ
              KSHELL = KK
              LSHELL = LL
              IF (POPLE) THEN
               CALL DCOPY(256,ZERO,0,GPOPLE,1)
               CALL GENR70(1,.FALSE.)
               CALL DCOPY(256,GPOPLE,1,GOUT,1)
               CALL TRANO_P(GOUT,CMO,TRANA,CMOB,TRANB)
              ELSE
               CALL DCOPY(MXSHL4,ZERO,0,GOUT,1)
               CALL PARSHEL(1,II,JJ,II,JJ)
               CALL IJPRIM(DDIJ)
               IF (NIJ.EQ.0) GO TO 170
               CALL PARSHEL(2,II,JJ,KK,LL)
               IF(IJKL.EQ.1) THEN
                 CALL S0000(GOUT,DDIJ)
               ELSE
                 CALL GENRAL(GOUT,DDIJ)
               END IF
               CALL TRANO(GOUT,CMO,TRANA,CMOB,TRANB)
              END IF
             END IF   ! SCHWARZ
            END IF    ! SYM
           END DO     !  LL-SHELL
 170      CONTINUE
         END DO       !  JJ-SHELL
         CALL USUMAO(T2AOAA,T2AOBB,T2AOAB,T2AOBA,TRANA,TRANB,LAGA,LAGB,
     *               PAOAA,PAOBB,II,KK)
         IF (DLB) CALL DDI_DLBNEXT( MYTASK )
         END IF       !  LOAD BALANCER
         LOCTSK = LOCTSK + 1
        END IF        !  SYM (IK|
       END DO         !  KK-SHELL
      END DO          !  II-SHELL
C
C  TRANSFORM AO INDICES OF THE LAGRANGIAN TO VIRTUAL MO INDICES
C  SUMMING INTO THE VIRTUAL-OCCUPIED BLOCK OF W(2)
C
      CALL DGEMM('T','N',NVIR,NOCC,NBF
     *,          ONE,CMO(1,NOP1),NBF
     *,          LAGA,NBF
     *,          ONE,WMAT(NOP1,1),NBF)
      CALL DGEMM('T','N',NVIRB,NOCCB,NBF
     *,          ONE,CMOB(1,NOP1B),NBF
     *,          LAGB,NBF
     *,          ONE,WMATB(NOP1B,1),NBF)
C
      CALL DDI_GSUMI(1099,NSCHWZ,1)
      IF(DLB) CALL DDI_DLBRESET
C
      IF (OUTPUT) THEN
        WRITE(ICHANL,3) NSCHWZ
        CALL FLSHBF(ICHANL)
      END IF
      RETURN
1     FORMAT(6X,'COMPUTING (VV|VO) INTEGRAL CLASS ')
3     FORMAT(6X,'SCHWARZ INEQUALITY TEST SKIPPED ',I10,
     * ' INTEGRAL BLOCKS')
      END
C*MODULE UMPDDI   *DECK GETAMPSA
      SUBROUTINE GETAMPSA(T2AO,BUFF,BUFF2,II,KK)
C
C -----------------------------------------------------------------
C  GET A BLOCK OF HALF-TRANSFORMED BETA BETA AMPLITUDES
C  WITH AT MOST TWO DDI_GETS
C  (BASED ON GETAMPS)
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXSH=1000, MXGTOT=5000)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      LOGICAL OUTPUT,MP2ENG,UMP2GD
      DOUBLE PRECISION T2AO(NOCC,NOCC,*),BUFF(*),BUFF2(*)
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
     *,               NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
C
      LENI = KMAX(II) - KMIN(II) + 1
      LENK = KMAX(KK) - KMIN(KK) + 1
      LENIK = LENI*LENK
C
C  FIND BEGIN,END SHELL BLOCK-IK
C
      IKB = NBF*( KLOC(II) - 1 ) + LENI*( KLOC(KK) - 1 ) + 1
      IKE = IKB + LENIK - 1
      CALL DDI_GET(D_VOVO, IKB, IKE, 1, NOTR, BUFF)
C
C  FIND BEGIN,END SHELL BLOCK-KI
C
      KIB = NBF*( KLOC(KK) - 1 ) + LENK*( KLOC(II) - 1 ) + 1
      KIE = KIB + LENIK - 1
      CALL DDI_GET(D_VOVO, KIB, KIE, 1, NOTR, BUFF2)
C
C  TRANSPOSE AMPLITUDES FOR EFFICIENT TRANSFORMATION
C
      NL = 0
      DO N = 1, LENI
        DO L = 1, LENK
          NL = NL + 1
          NLIJ = NL
          NLJI = (L-1)*LENI + N
          DO I = 1, NOCC
            DO J = 1, I
              T2AO(I,J,NL) =  BUFF(NLIJ) - BUFF2(NLJI)
              T2AO(J,I,NL) = -BUFF(NLIJ) + BUFF2(NLJI)
              NLIJ = NLIJ + LENIK
              NLJI = NLJI + LENIK
            END DO
          END DO
        END DO
      END DO
      RETURN
      END
C
C*MODULE UMPDDI   *DECK GETAMPSB
      SUBROUTINE GETAMPSB(T2AO,BUFF,BUFF2,II,KK)
C
C -----------------------------------------------------------------
C  GET A BLOCK OF HALF-TRANSFORMED BETA BETA AMPLITUDES
C  WITH AT MOST TWO DDI_GETS
C  (BASED ON GETAMPS)
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXSH=1000, MXGTOT=5000)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      LOGICAL OUTPUT,MP2ENG,UMP2GD
      DOUBLE PRECISION T2AO(NOCCB,NOCCB,*),BUFF(*),BUFF2(*)
      INTEGER         D_UB,D_E,D_EB
      INTEGER         D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB,
     *                D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
      LOGICAL         NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB,
     *                NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
      COMMON /UMPDMS/ D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB
     *,               D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
     *,               NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB
     *,               NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
     *,               D_UB,D_E,D_EB
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
C
      LENI = KMAX(II) - KMIN(II) + 1
      LENK = KMAX(KK) - KMIN(KK) + 1
      LENIK = LENI*LENK
C
C  FIND BEGIN,END SHELL BLOCK-IK
C
      IKB = NBF*( KLOC(II) - 1 ) + LENI*( KLOC(KK) - 1 ) + 1
      IKE = IKB + LENIK - 1
      CALL DDI_GET(D_VOVOBB, IKB, IKE, 1, NOTB, BUFF)
C
C  FIND BEGIN,END SHELL BLOCK-KI
C
      KIB = NBF*( KLOC(KK) - 1 ) + LENK*( KLOC(II) - 1 ) + 1
      KIE = KIB + LENIK - 1
      CALL DDI_GET(D_VOVOBB, KIB, KIE, 1, NOTB, BUFF2)
C
C  TRANSPOSE AMPLITUDES FOR EFFICIENT TRANSFORMATION
C
      NL = 0
      DO N = 1, LENI
        DO L = 1, LENK
          NL = NL + 1
          NLIJ = NL
          NLJI = (L-1)*LENI + N
          DO I = 1, NOCCB
            DO J = 1, I
              T2AO(I,J,NL) =  BUFF(NLIJ) - BUFF2(NLJI)
              T2AO(J,I,NL) = -BUFF(NLIJ) + BUFF2(NLJI)
              NLIJ = NLIJ + LENIK
              NLJI = NLJI + LENIK
            END DO
          END DO
        END DO
      END DO
      RETURN
      END
C
C*MODULE UMPDDI   *DECK GETAMPSM
      SUBROUTINE GETAMPSM(T2AO,T2AOB,BUFF,II,KK)
C
C -----------------------------------------------------------------
C  GET A BLOCK OF HALF-TRANSFORMED MIXED SPIN AMPLITUDES
C  WITH AT MOST TWO DDI_GETS
C  (BASED ON GETAMPS)
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXSH=1000, MXGTOT=5000)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      LOGICAL OUTPUT,MP2ENG,UMP2GD
      DOUBLE PRECISION T2AO(NOCC,NOCCB,*),T2AOB(NOCCB,NOCC,*),BUFF(*)
      INTEGER         D_UB,D_E,D_EB
      INTEGER         D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB,
     *                D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
      LOGICAL         NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB,
     *                NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
      COMMON /UMPDMS/ D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB
     *,               D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
     *,               NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB
     *,               NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
     *,               D_UB,D_E,D_EB
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
C
      LENI = KMAX(II) - KMIN(II) + 1
      LENK = KMAX(KK) - KMIN(KK) + 1
      LENIK = LENI*LENK
C
C  FIND BEGIN,END SHELL BLOCK-IK
C
      IKB = NBF*( KLOC(II) - 1 ) + LENI*( KLOC(KK) - 1 ) + 1
      IKE = IKB + LENIK - 1
      CALL DDI_GET(D_VOVOAB, IKB, IKE, 1, NOCC*NOCCB, BUFF)
C
C  TRANSPOSE AMPLITUDES FOR EFFICIENT TRANSFORMATION
C
      NL = 0
      DO N = 1, LENI
        DO L = 1, LENK
          NL = NL + 1
          NLIJ = NL
          DO J = 1, NOCCB
            DO I = 1, NOCC
              T2AOB(J,I,NL) = BUFF(NLIJ)
              NLIJ = NLIJ + LENIK
            END DO
          END DO
        END DO
      END DO
      IF (II.NE.KK) THEN
C
C  FIND BEGIN,END SHELL BLOCK-KI
C
        KIB = NBF*( KLOC(KK) - 1 ) + LENK*( KLOC(II) - 1 ) + 1
        KIE = KIB + LENIK - 1
        CALL DDI_GET(D_VOVOAB, KIB, KIE, 1, NOCC*NOCCB, BUFF)
      END IF
C
C  TRANSPOSE AMPLITUDES FOR EFFICIENT TRANSFORMATION
C
      NL = 0
      DO N = 1, LENI
        DO L = 1, LENK
          NL = NL + 1
          NLIJ = (L-1)*LENI + N
          DO J = 1, NOCCB
            DO I = 1, NOCC
              T2AO(I,J,NL) = BUFF(NLIJ)
              NLIJ = NLIJ + LENIK
            END DO
          END DO
        END DO
      END DO
      RETURN
      END
C
C*MODULE UMPDDI   *DECK USUMAO
      SUBROUTINE USUMAO(T2AOAA,T2AOBB,T2AOAB,T2AOBA,TRANA,TRANB,LAGA,
     *                  LAGB,PAOAA,PAOBB,II,KK)
C
C -----------------------------------------------------------------
C  CONTRACT QUARTER-TRANSFORMED INTEGRALS WITH
C  HALF-TRANSFORMED AMPLITUDES AND BACK-TRANSFORMED P(2)
C  (BASED ON SUMAO)
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXSH=1000, MXGTOT=5000)
      PARAMETER (HALF=0.5D+00, ONE=1.0D+00)
      LOGICAL OUTPUT,MP2ENG,UMP2GD
      DOUBLE PRECISION T2AOAA(NOCC,NOCC,*),T2AOBB(NOCCB,NOCCB,*)
     *,      T2AOAB(NOCC,NOCCB,*),T2AOBA(NOCCB,NOCC,*)
     *,      TRANA(NBF,NOCC,MXSHL,MXSHL,2)
     *,      TRANB(NBF,NOCCB,MXSHL,MXSHL,2),LAGA(NBF,NOCC)
     *,      LAGB(NBF,NOCCB),PAOAA(NBF,NBF),PAOBB(NBF,NBF)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
C
      IBEG = KLOC(II)
      IEND = IBEG + KMAX(II) - KMIN(II)
      KBEG = KLOC(KK)
      KEND = KBEG + KMAX(KK) - KMIN(KK)
      IX = 0
      KL = 0
      DO I = IBEG, IEND
        IX = IX + 1
        KX = 0
        DO K = KBEG, KEND
          KX = KX + 1
          KL = KL + 1
          IF (I.GE.K) THEN
              CALL DGEMM('N','N',NBF,NACT,NACT
     *,                  ONE,TRANA(1,NCP1,IX,KX,1),NBF
     *,                  T2AOAA(NCP1,NCP1,KL),NOCC
     *,                  ONE,LAGA(1,NCP1),NBF)
              CALL DGEMM('N','N',NBF,NACTB,NACTB
     *,                  ONE,TRANB(1,NCP1B,IX,KX,1),NBF
     *,                  T2AOBB(NCP1B,NCP1B,KL),NOCCB
     *,                  ONE,LAGB(1,NCP1B),NBF)
C             --- CROSS TERM CONTRIBUTIONS ---
              CALL DGEMM('N','N',NBF,NACTB,NACT
     *,                ONE,TRANA(1,NCP1,IX,KX,1),NBF
     *,                T2AOAB(NCP1,NCP1B,KL),NOCC
     *,                ONE,LAGB(1,NCP1B),NBF)
              CALL DGEMM('N','N',NBF,NACT,NACTB
     *,                ONE,TRANB(1,NCP1B,IX,KX,1),NBF
     *,                T2AOBA(NCP1B,NCP1,KL),NOCCB
     *,                ONE,LAGA(1,NCP1),NBF)
            DO LO = 1, NOCC
              DO J = 1, NBF
                LAGA(K,LO) =LAGA(K,LO)
     *                     +TRANA(J,LO,IX,KX,1)*PAOAA(I,J)*ONE
                LAGA(I,LO) =LAGA(I,LO)
     *                     -TRANA(J,LO,IX,KX,1)*PAOAA(K,J)*HALF
                LAGA(J,LO) =LAGA(J,LO)
     *                     -TRANA(J,LO,IX,KX,1)*PAOAA(I,K)*HALF
C               CROSS TERM CONTRIBUTION
                LAGA(K,LO)=LAGA(K,LO)
     *                     +TRANA(J,LO,IX,KX,1)*PAOBB(I,J)*ONE
              END DO
            END DO
            DO LO = 1, NOCCB
              DO J = 1, NBF
                LAGB(K,LO) =LAGB(K,LO)
     *                     +TRANB(J,LO,IX,KX,1)*PAOBB(I,J)*ONE
                LAGB(I,LO) =LAGB(I,LO)
     *                     -TRANB(J,LO,IX,KX,1)*PAOBB(K,J)*HALF
                LAGB(J,LO) =LAGB(J,LO)
     *                     -TRANB(J,LO,IX,KX,1)*PAOBB(I,K)*HALF
C               CROSS TERM CONTRIBUTION
                LAGB(K,LO)=LAGB(K,LO)
     *                     +TRANB(J,LO,IX,KX,1)*PAOAA(I,J)*ONE
              END DO
            END DO
            IF (I.NE.K) THEN
              CALL DGEMM('N','T',NBF,NACT,NACT
     *,                  ONE,TRANA(1,NCP1,KX,IX,2),NBF
     *,                  T2AOAA(NCP1,NCP1,KL),NOCC
     *,                  ONE,LAGA(1,NCP1),NBF)
              CALL DGEMM('N','T',NBF,NACTB,NACTB
     *,                  ONE,TRANB(1,NCP1B,KX,IX,2),NBF
     *,                  T2AOBB(NCP1B,NCP1B,KL),NOCCB
     *,                  ONE,LAGB(1,NCP1B),NBF)
C             --- CROSS TERM CONTRIBUTIONS ---
              CALL DGEMM('N','T',NBF,NACTB,NACT
     *,                  ONE,TRANA(1,NCP1,KX,IX,2),NBF
     *,                  T2AOBA(NCP1B,NCP1,KL),NOCCB
     *,                  ONE,LAGB(1,NCP1B),NBF)
              CALL DGEMM('N','T',NBF,NACT,NACTB
     *,                  ONE,TRANB(1,NCP1B,KX,IX,2),NBF
     *,                  T2AOAB(NCP1,NCP1B,KL),NOCC
     *,                  ONE,LAGA(1,NCP1),NBF)
              DO LO = 1, NOCC
                DO J = 1, NBF
                  LAGA(I,LO)=LAGA(I,LO)
     *                      +TRANA(J,LO,KX,IX,2)*PAOAA(K,J)*ONE
                  LAGA(K,LO)=LAGA(K,LO)
     *                      -TRANA(J,LO,KX,IX,2)*PAOAA(I,J)*HALF
                  LAGA(J,LO)=LAGA(J,LO)
     *                      -TRANA(J,LO,KX,IX,2)*PAOAA(K,I)*HALF
C               CROSS TERM CONTRIBUTION
                  LAGA(I,LO)=LAGA(I,LO)
     *                      +TRANA(J,LO,KX,IX,2)*PAOBB(K,J)*ONE
                END DO
              END DO
              DO LO = 1, NOCCB
                DO J = 1, NBF
                  LAGB(I,LO)=LAGB(I,LO)
     *                      +TRANB(J,LO,KX,IX,2)*PAOBB(K,J)*ONE
                  LAGB(K,LO)=LAGB(K,LO)
     *                      -TRANB(J,LO,KX,IX,2)*PAOBB(I,J)*HALF
                  LAGB(J,LO)=LAGB(J,LO)
     *                      -TRANB(J,LO,KX,IX,2)*PAOBB(K,I)*HALF
C               CROSS TERM CONTRIBUTION
                  LAGB(I,LO)=LAGB(I,LO)
     *                      +TRANB(J,LO,KX,IX,2)*PAOAA(K,J)*ONE
                END DO
              END DO
            END IF
          END IF
        END DO
      END DO
      RETURN
      END
C*MODULE UMPDDI   *DECK UMKVOVO
      SUBROUTINE UMKVOVO(TMP1,TMP2,CM1,CM1B,EORB,EORBB,CMO,CMOB)
C
C -----------------------------------------------------------------
C  REVERSE THE PROCESS BY WHICH BACK-TRANSFORMED AMPLITUDES WERE
C  CONSTRUCTED FROM VOVO INTEGRALS IN BTAMPS (BASED ON MKVOVO)
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXSH=1000, MXGTOT=5000)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
      LOGICAL GOPARR,DSKWRK,MASWRK,OUTPUT,MP2ENG,UMP2GD
      INTEGER A,B,AV,BV,AB
      DOUBLE PRECISION TMP1(*),TMP2(*),CMO(NBF,*),CM1(NBF,*),EORB(*)
      DOUBLE PRECISION CMOB(NBF,*),CM1B(NBF,*),EORBB(*)
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
      INTEGER         D_UB,D_E,D_EB
      INTEGER         D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB,
     *                D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      LOGICAL         NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB,
     *                NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
     *,               NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /UMPDMS/ D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB
     *,               D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
     *,               NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB
     *,               NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
     *,               D_UB,D_E,D_EB
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
C
      IF (OUTPUT) THEN
        WRITE(ICHANL,1)
        CALL FLSHBF(ICHANL)
      END IF
C
C  FORM INVERSE MO COEFFICIENT MATRIX:
C
C  1) GET OVERLAP INTEGRALS
C
      CALL DAREAD(IDAF,IODA,TMP1,NBTR,12,0)
      CALL CPYTSQ(TMP1,TMP2,NBF,1)
C
C  2) COMPUTE C(-1) = C(T)*S (BOTH ALPHA AND BETA)
C
      CALL DGEMM('T','N',NVIR,NBF,NBF
     *,          ONE,CMO(1,NOP1),NBF
     *,          TMP2,NBF
     *,          ZERO,CM1,NVIR)
      CALL DGEMM('T','N',NVIRB,NBF,NBF
     *,          ONE,CMOB(1,NOP1B),NBF
     *,          TMP2,NBF
     *,          ZERO,CM1B,NVIRB)
C
C  FIRST RECREATE ALL ALPHA VOVO INTEGRALS
C
      CALL DDI_DISTRIB(D_VOVO,ME,IL_VOVO,IH_VOVO,JL_VOVO,JH_VOVO)
      IJ = 0
      DO I = 1, NOCC
        DO J = 1, I
          IJ = IJ + 1
          IF (IJ.GE.JL_VOVO.AND.IJ.LE.JH_VOVO) THEN
            CALL DDI_GET(D_VOVO,1,NBSQ,IJ,IJ,TMP1)
C
C  MAP SHELL-BLOCKS BACK TO REGULAR AO-ORDER
C
            ICOUNT = 0
            DO ISHELL = 1, NSHELL
              IBEG = KLOC(ISHELL)
              IEND = IBEG + KMAX(ISHELL) - KMIN(ISHELL)
              DO JSHELL = 1, NSHELL
                JBEG = KLOC(JSHELL)
                JEND = JBEG + KMAX(JSHELL) - KMIN(JSHELL)
                DO IAO = IBEG, IEND
                  DO JAO = JBEG, JEND
                    IJAO = (IAO-1)*NBF + JAO
                    ICOUNT = ICOUNT + 1
                    TMP2(IJAO) = TMP1(ICOUNT)
                  END DO
                END DO
              END DO
            END DO
C
C  TRANSFORM AMPLITUDES TO MO BASIS
C
            CALL CACT(NVIR,NBF
     *,               TMP2,NBF,CM1,NVIR
     *,               TMP2,NVIR,TMP1)
C
C  CONVERT AMPLITUDES BACK TO REGULAR VOVO-CLASS INTEGRALS
C
            AB = 0
            EIJ = EORB(I) + EORB(J)
            DO A = 1, NVIR
              AV = A + NOCC
              DO B = 1, NVIR
                BV = B + NOCC
                AB = AB + 1
                DENOM = EIJ-EORB(AV)-EORB(BV)
                TMP2(AB) = TMP2(AB)*DENOM
              END DO
            END DO
            CALL DDI_PUT(D_VOVO,1,NVSQ,IJ,IJ,TMP2)
          END IF
        END DO
      END DO
C
C  NOW RECREATE ALL BETA VOVO INTEGRALS:
C
      CALL DDI_DISTRIB(D_VOVOBB,ME,IL_VOVO,IH_VOVO,JL_VOVO,JH_VOVO)
      IJ = 0
      DO I = 1, NOCCB
        DO J = 1, I
          IJ = IJ + 1
          IF (IJ.GE.JL_VOVO.AND.IJ.LE.JH_VOVO) THEN
            CALL DDI_GET(D_VOVOBB,1,NBSQ,IJ,IJ,TMP1)
C
C  MAP SHELL-BLOCKS BACK TO REGULAR AO-ORDER
C
            ICOUNT = 0
            DO ISHELL = 1, NSHELL
              IBEG = KLOC(ISHELL)
              IEND = IBEG + KMAX(ISHELL) - KMIN(ISHELL)
              DO JSHELL = 1, NSHELL
                JBEG = KLOC(JSHELL)
                JEND = JBEG + KMAX(JSHELL) - KMIN(JSHELL)
                DO IAO = IBEG, IEND
                  DO JAO = JBEG, JEND
                    IJAO = (IAO-1)*NBF + JAO
                    ICOUNT = ICOUNT + 1
                    TMP2(IJAO) = TMP1(ICOUNT)
                  END DO
                END DO
              END DO
            END DO
C
C  TRANSFORM AMPLITUDES TO MO BASIS
C
            CALL CACT(NVIRB,NBF
     *,               TMP2,NBF,CM1B,NVIRB
     *,               TMP2,NVIRB,TMP1)
C
C  CONVERT AMPLITUDES BACK TO REGULAR VOVO-CLASS INTEGRALS
C
            AB = 0
            EIJ = EORBB(I) + EORBB(J)
            DO A = 1, NVIRB
              AV = A + NOCCB
              DO B = 1, NVIRB
                BV = B + NOCCB
                AB = AB + 1
                DENOM = EIJ-EORBB(AV)-EORBB(BV)
                TMP2(AB) = TMP2(AB)*DENOM
              END DO
            END DO
            CALL DDI_PUT(D_VOVOBB,1,NVSB,IJ,IJ,TMP2)
          END IF
        END DO
      END DO
C
C  NOW RECREATE MIXED SPIN VOVO INTEGRALS:
C
      CALL DDI_DISTRIB(D_VOVOAB,ME,IL_VOVO,IH_VOVO,JL_VOVO,JH_VOVO)
      IJ = 0
      DO J = 1, NOCCB
        DO I = 1, NOCC
          IJ = (J-1)*NOCC + I
          IF (IJ.GE.JL_VOVO.AND.IJ.LE.JH_VOVO) THEN
            CALL DDI_GET(D_VOVOAB,1,NBSQ,IJ,IJ,TMP1)
C
C  MAP SHELL-BLOCKS BACK TO REGULAR AO-ORDER
C
            ICOUNT = 0
            DO ISHELL = 1, NSHELL
              IBEG = KLOC(ISHELL)
              IEND = IBEG + KMAX(ISHELL) - KMIN(ISHELL)
              DO JSHELL = 1, NSHELL
                JBEG = KLOC(JSHELL)
                JEND = JBEG + KMAX(JSHELL) - KMIN(JSHELL)
                DO IAO = IBEG, IEND
                  DO JAO = JBEG, JEND
                    IJAO = (IAO-1)*NBF + JAO
                    ICOUNT = ICOUNT + 1
                    TMP2(IJAO) = TMP1(ICOUNT)
                  END DO
                END DO
              END DO
            END DO
C
C  TRANSFORM AMPLITUDES TO MO BASIS
C
            CALL DGEMM('N','T',NBF,NVIR,NBF
     *,                ONE,TMP2,NBF
     *,                CM1,NVIR
     *,                ZERO,TMP1,NBF)
            CALL DGEMM('N','N',NVIRB,NVIR,NBF
     *,                ONE,CM1B,NVIRB
     *,                TMP1,NBF
     *,                ZERO,TMP2,NVIRB)
C
C  CONVERT AMPLITUDES BACK TO REGULAR VOVO-CLASS INTEGRALS
C
            AB = 0
            EIJ = EORB(I) + EORBB(J)
            DO A = 1, NVIR
              AV = A + NOCC
              DO B = 1, NVIRB
                BV = B + NOCCB
                AB = AB + 1
                DENOM = EIJ-EORB(AV)-EORBB(BV)
                TMP2(AB) = TMP2(AB)*DENOM
              END DO
            END DO
            CALL DDI_PUT(D_VOVOAB,1,NVIRB*NVIR,IJ,IJ,TMP2)
          END IF
        END DO
      END DO
      RETURN
1     FORMAT(6X,'RESTORING (VO|VO) INTEGRAL CLASS')
      END
C*MODULE UMPDDI   *DECK UZVECTR
      SUBROUTINE UZVECTR(EORB,EORBB,RHS,RHSB
     *,                 U,UNXT,PRHS,B,CC,IPVT
     *,                 BUF,BUF1,BUF2,ALPHA
     *,                 UB,UNXTB,PRHSB
     *,                 BUFB,BUF1B,BUF2B
     *,                 MOLABS,MOLABSB,NSYM,NVSM,LVSM,NSYMB,NVSMB,LVSMB
     *,                 MAXC,LEN,LENB,MXIRR)
C
C -----------------------------------------------------------------
C  SOLVE THE Z-VECTOR EQUATION USING DIIS-BASED
C  ITERATIVE SOLUTION OF SIMULTANEOUS LINEAR EQUATIONS
C  (UZVECTR,UHTPROD,UDIISP)
C  ROUTINE DIFFERENT FROM ZVECTR
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00
     *,          TICH=1.0D-24)
      LOGICAL GOPARR,DSKWRK,MASWRK, OUTPUT,MP2ENG,UMP2GD
      INTEGER MOLABS(*),MOLABSB(*),NSYM(*),NVSM(*),LVSM(*)
     *,      NSYMB(*),NVSMB(*),LVSMB(*),IPVT(*)
      DOUBLE PRECISION EORB(*),EORBB(*),RHS(*),U(*),UNXT(*),PRHS(*)
     *,      B(*),CC(*),ALPHA(MAXC,*),BUF(*),BUF1(*),BUF2(*)
     *,      RHSB(*),UB(*),UNXTB(*),PRHSB(*)
     *,      BUFB(*),BUF1B(*),BUF2B(*)
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U, D_UB
      INTEGER         D_E, D_EB
      INTEGER         D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB,
     *                D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      LOGICAL         NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB,
     *                NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
     *,               NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /UMPDMS/ D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB
     *,               D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
     *,               NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB
     *,               NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
     *,               D_UB,D_E,D_EB
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
C
      CALL DDI_DISTRIB(D_U,ME,IL_U,IH_U,JL_U,JH_U)
      CALL DDI_DISTRIB(D_UB,ME,IL_UB,IH_UB,JL_UB,JH_UB)
      UCONV = 1.0D-09
      CALL DCOPY(LEN,ZERO,0,U,1)
      CALL DCOPY(LEN,ZERO,0,UNXT,1)
      CALL DCOPY(LEN,ZERO,0,PRHS,1)
      CALL DCOPY(LENB,ZERO,0,UB,1)
      CALL DCOPY(LENB,ZERO,0,UNXTB,1)
      CALL DCOPY(LENB,ZERO,0,PRHSB,1)
      CALL DCOPY(MAXC,ZERO,0,CC,1)
      IF (OUTPUT) THEN
        WRITE(ICHANL,20)
        CALL FLSHBF(ICHANL)
      END IF
C
C  GET ZEROTH-ORDER ESTIMATES
C
      CALL SYMEIG(MOLABS,EORB,RHS,U,NOCC,NOP1,NMOS)
      CALL SYMEIG(MOLABSB,EORBB,RHSB,UB,NOCCB,NOP1B,NMOS)
C
      DO I = 1, LEN
        IF (ABS(U(I)).LE.TICH) U(I) = ZERO
      END DO
      IF (1.GE.JL_U.AND.1.LE.JH_U)
     *  CALL DDI_PUT(D_U, 1, LEN, 1, 1, U)
      DO I = 1, LENB
        IF (ABS(UB(I)).LE.TICH) UB(I) = ZERO
      END DO
      IF (1.GE.JL_UB.AND.1.LE.JH_UB)
     *  CALL DDI_PUT(D_UB, 1, LENB, 1, 1, UB)
C
C  START OF ITERATIVE PROCESS. 50 ITERATIONS ARE ALLOWED
C
      DO ITR = 1, MAXC
C
C  FORM PRODUCT OF HESSIAN*TRIAL-VECTOR
C
        CALL DCOPY(LEN,ZERO,0,UNXT,1)
        CALL DCOPY(LENB,ZERO,0,UNXTB,1)
        CALL UHTPROD(U,UB,UNXT,UNXTB,BUF,BUF1,BUF2
     *,              BUFB,BUF1B,BUF2B
     *,              NSYM,NVSM,LVSM
     *,              NSYMB,NVSMB,LVSMB,MXIRR)
        CALL DDI_GSUMF(101+ITR,UNXT,LEN)
        CALL DDI_GSUMF(151+ITR,UNXTB,LENB)
        CALL DAXPY(LEN,-ONE,RHS,1,UNXT,1)
        CALL DSCAL(LEN,-ONE,UNXT,1)
        CALL SYMEIG(MOLABS,EORB,UNXT,PRHS,NOCC,NOP1,NMOS)
        CALL DAXPY(LENB,-ONE,RHSB,1,UNXTB,1)
        CALL DSCAL(LENB,-ONE,UNXTB,1)
        CALL SYMEIG(MOLABSB,EORBB,UNXTB,PRHSB,NOCCB,NOP1B,NMOS)
C
C  CHECK FOR CONVERGENCE OF SOLUTION VECTORS
C
        IF (ITR.NE.1) THEN
          CALL DAXPY(LEN,-ONE,PRHS,1,U,1)
          CALL DAXPY(LENB,-ONE,PRHSB,1,UB,1)
          GNORM = DDOT(LEN,U,1,U,1)/(NOCC*NVIR)
          GNORM = SQRT(GNORM)
          GNORMB = DDOT(LENB,UB,1,UB,1)/(NOCCB*NVIRB)
          GNORMB = SQRT(GNORMB)
          GMAX = MAX(ZERO,GNORM)
          GMAXB= MAX(ZERO,GNORMB)
          IF (OUTPUT) THEN
            WRITE(ICHANL,30) ITR,GMAX,GMAXB
            CALL FLSHBF(ICHANL)
          END IF
          IF (GMAX.LE.UCONV .AND. GMAXB.LE.UCONV) THEN
            IF (OUTPUT) THEN
              WRITE(ICHANL,50)
              CALL FLSHBF(ICHANL)
            END IF
C
C           IF WE ARE DONE, COPY SOLUTION VECTOR INTO CORRECT STORAGE
C
            CALL DCOPY(LEN,PRHS,1,RHS,1)
            CALL DCOPY(LENB,PRHSB,1,RHSB,1)
            GO TO 3
          END IF
C
C  SAVE NEW SOLUTION VECTOR IN GLOBAL ARRAY
C
          NXTR = ITR + 1
          IF (NXTR.GE.JL_U.AND.NXTR.LE.JH_U)
     *      CALL DDI_PUT(D_U, 1, LEN, NXTR, NXTR, PRHS)
          IF (NXTR.GE.JL_UB.AND.NXTR.LE.JH_UB)
     *      CALL DDI_PUT(D_UB, 1, LENB, NXTR, NXTR, PRHSB)
C
C  GET NEW INTERPOLATED VECTOR USING DIIS
C
          MAXC2 = (MAXC*MAXC + MAXC)/2
          CALL UDIISP(U,UB,ALPHA,CC,B,IPVT,PRHS,PRHSB,
     *                  LEN,LENB,ITR,MAXC,MAXC2)
          CALL DCOPY(LEN,PRHS,1,U,1)
          CALL DCOPY(LENB,PRHSB,1,UB,1)
        END IF
C
C  FOR ITR=1, SAVE NEW SOLUTION VECTOR IN GLOBAL ARRAY
C
        IF (ITR.EQ.1) THEN
          NXTR = 2
          IF (NXTR.GE.JL_U.AND.NXTR.LE.JH_U)
     *       CALL DDI_PUT(D_U, 1, LEN, NXTR, NXTR, PRHS)
          CALL DCOPY(LEN,PRHS,1,U,1)
          IF (NXTR.GE.JL_UB.AND.NXTR.LE.JH_UB)
     *       CALL DDI_PUT(D_UB, 1, LENB, NXTR, NXTR, PRHSB)
          CALL DCOPY(LENB,PRHSB,1,UB,1)
        END IF
C
C  END OF LOOP
C
      END DO
      IF (OUTPUT) THEN
        WRITE(ICHANL,80) MAXC
        CALL FLSHBF(ICHANL)
      END IF
      CALL ABRT
 3    CONTINUE
      RETURN
20    FORMAT(/,6X,'ITERATION',5X,'ALPHA ERROR',4X
     *,      'BETA ERROR',/,6X,47('=')/)
30    FORMAT(I10,5X,F15.10,F15.10)
50    FORMAT(/,6X,'CONVERGED - WAVEFUNCTIONS STATIONARY',/)
80    FORMAT(/,6X,'!NO FULL CONVERGENCE AFTER',I4,' ITERATIONS!',
     *       /,/,1X,'PLEASE RECONSIDER:'/
     *       6X,'IF THE SCF HAS CONVERGED TO THE RIGHT ORBITAL',
     *          ' OCCUPANCY OR NOT'/
     *       6X,'IF THIS CHEMICAL SYSTEM IS A SINGLE',
     *          ' REFERENCE WAVEFUNCTION OR NOT'/
     *       1X,'IF YOU BELIEVE THIS WAVEFUNCTION IS',
     *          ' CORRECT AND CONVERGING,'/
     *       1X,'CONSIDER INCREASING MAXITC IN $MP2.')
C        -         -         -         -         -         -         - C
      END
C*MODULE UMPDDI   *DECK UDIISP
      SUBROUTINE UDIISP(ERR,ERRB,A,X,B,IPVT,WRK,WRKB,
     *                  LEN,LENB,ITR,MAXC,MAXC2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL DBG,MASWRK,GOPARR,DSKWRK
C
      DIMENSION ERR(LEN),ERRB(LENB),
     *          WRK(LEN),WRKB(LENB),
     *          A(MAXC,MAXC),X(MAXC),IPVT(MAXC),B(MAXC2)
C
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U, D_UB
      INTEGER         D_E,D_EB
      INTEGER         D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB,
     *                D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      LOGICAL         NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB,
     *                NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
     *,               NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /UMPDMS/ D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB
     *,               D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
     *,               NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB
     *,               NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
     *,               D_UB,D_E,D_EB
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DBUG,DBUGME/8HDEBUG   ,8HUDIISP  /
#else
      CHARACTER*8 :: DBUGME_STR
      EQUIVALENCE (DBUGME, DBUGME_STR)
      CHARACTER*8 :: DBUG_STR
      EQUIVALENCE (DBUG, DBUG_STR)
      DATA DBUG_STR,DBUGME_STR/"DEBUG   ","UDIISP  "/
#endif
C
C                 THIS ROUTINE PERFORMS THE
C        *** DIRECT INVERSION IN THE ITERATIVE SUBSPACE ***
C        INTERPOLATION USING THE CURRENT AND PREVIOUS CPHF
C        SOLUTIONS TO SPEED UP THE CONVERGENCE OF THE SELF
C        CONSISTENT SOLUTION.
C        IT IS A MODIFIED VERSION OF THE SUBROUTINE DIIS.
C
C        THE METHOD IS P.PULAY, J.COMPUT.CHEM., 3, 556-560 (1982).
C        BUT SEE ALSO P.PULAY, CHEM.PHYS.LETT., 73, 393-398 (1980),
C        T.P.HAMILTON, P.PULAY, J.CHEM.PHYS., 84, 5728-5734 (1986).
C
C     ON ENTRY, -ERR- IS THE CURRENT CPHF
C               ERROR MATRIX, EXPRESSED IN THE MO BASIS.
C     ON EXIT, -ERR- IS DESTROYED, AND -WRK- WILL CONTAIN
C               THE INTERPOLATED CPHF SOLUTION
C
C  SAVE THE CURRENT ERROR MATRIX IN GLOBAL ARRAY
C
        CALL DDI_DISTRIB(D_U,ME,IL_U,IH_U,JL_U,JH_U)
        CALL DDI_DISTRIB(D_UB,ME,IL_UB,IH_UB,JL_UB,JH_UB)
        CALL DDI_DISTRIB(D_E,ME,IL_E,IH_E,JL_E,JH_E)
        CALL DDI_DISTRIB(D_EB,ME,IL_EB,IH_EB,JL_EB,JH_EB)
        NITR = ITR
        IF (NITR.GE.JL_E.AND.NITR.LE.JH_E)
     *     CALL DDI_PUT(D_E, 1, LEN, NITR-1, NITR-1, ERR)
        IF (NITR.GE.JL_EB.AND.NITR.LE.JH_EB)
     *     CALL DDI_PUT(D_EB, 1, LENB, NITR-1, NITR-1, ERRB)
C
      DBG = (NPRINT.EQ.5  .OR.  EXETYP.EQ.DBUG  .OR.  EXETYP.EQ.DBUGME)
     *      .AND. MASWRK
C
      IF (MASWRK  .AND.  NITR.EQ.2) WRITE(IW,900)
C
C  FORM LAST ROW (ROW NITR) OF TRIANGULAR MATRIX B,
C  FIRST DOING THE DIAGONAL AND THE FIRST ELEMENT OF THE ROW.
C
       BJJ=DDOT(LEN,ERR,1,ERR,1) + DDOT(LENB,ERRB,1,ERRB,1)
C
       IF(NITR.EQ.2) THEN
          CALL VCLR(B,1,MAXC2)
          B(1)=ZERO
          B(2)=-ONE
          B(3)=BJJ
          RETURN
       ELSE
          J1   = (NITR*NITR-NITR)/2 + 1
          JJ   = (NITR*NITR+NITR)/2
          B(J1)= -ONE
          B(JJ)= BJJ
       END IF
C
C        THE REST OF THE BIJ'S.
C
      DO 320 IX=1,NITR-2
        BIJA = ZERO
        BIJB = ZERO
        IF (IX.GE.JL_E.AND.IX.LE.JH_E) THEN
          IXTMP = IX
          CALL DDI_GET(D_E, 1, LEN, IXTMP, IXTMP, WRK)
          BIJA=DDOT(LEN,ERR,1,WRK,1)
        END IF
        IF (IX.GE.JL_EB.AND.IX.LE.JH_EB) THEN
          IXTMP = IX
          CALL DDI_GET(D_EB, 1, LENB, IXTMP, IXTMP, WRKB)
          BIJB=DDOT(LENB,ERRB,1,WRKB,1)
        END IF
        X(IX)=BIJA + BIJB
  320 CONTINUE
      CALL DDI_GSUMF(201+NITR,X,NITR-2)
      DO 322 IX=1,NITR-2
        B(J1+IX)=X(IX)
  322 CONTINUE
C
      IF(DBG) THEN
         WRITE(IW,*) 'DIIS EQUATIONS'
         CALL PRTRI(B,NITR)
      END IF
C
C     ----- SET UP AND SOLVE THE SET OF DIIS LINEAR EQUATIONS (NITR > 2)
C
  325  CONTINUE
       CALL DIISEQ(A,NITR,B,JJ,NITR)
       CALL VCLR(X,1,NITR)
         X(1)=-ONE
         IERR=0
         CALL SLVLEQ(A,X,IPVT,NITR,NITR,0,IERR)
         IF(IERR.NE.0) THEN
           NITR = NITR - 1
           IF (MASWRK) WRITE(IW,*)
     *         'TRUNCATING UHF MP2 RESPONSE VECTOR DIIS EQUATIONS'
           IF (NITR .NE. 1) GO TO 325
           IF (MASWRK) WRITE(IW,*)
     *         'ERROR: RESPONSE VECTOR DIIS EQUATIONS ARE SINGULAR'
           CALL ABRT
           STOP
         END IF
         IF(DBG) THEN
            WRITE(IW,*) 'DIIS INTERPOLATION COEFFICIENTS'
            WRITE(IW,910) (X,I=1,NITR)
         END IF
C
C     ----- FORM THE INTERPOLATED CPHF SOLUTION -----
C     THE INTERPOLATION IS SUMMATION C(I)*PAI(I)
C
      CALL VCLR(WRK,1,LEN)
      CALL VCLR(WRKB,1,LENB)
      DO 420 IX=2,NITR
        CI = X(IX)
        IF (IX.GE.JL_U.AND.IX.LE.JH_U) THEN
          CALL DDI_GET(D_U, 1, LEN, IX+1, IX+1, ERR)
          CALL DAXPY(LEN,CI,ERR,1,WRK,1)
        END IF
        IF (IX.GE.JL_UB.AND.IX.LE.JH_UB) THEN
          CALL DDI_GET(D_UB, 1, LENB, IX+1, IX+1, ERRB)
          CALL DAXPY(LENB,CI,ERRB,1,WRKB,1)
        END IF
  420 CONTINUE
      CALL DDI_GSUMF(301+NITR,WRK,LEN)
      CALL DDI_GSUMF(351+NITR,WRKB,LENB)
C
      IF(DBG) THEN
         WRITE(IW,*) 'DIIS INTERPOLATED ALPHA CPHF SOLUTION IS'
         CALL PRSQ(WRK,1,LEN,LEN)
         WRITE(IW,*) 'DIIS INTERPOLATED BETA CPHF SOLUTION IS'
         CALL PRSQ(WRKB,1,LENB,LENB)
      END IF
C
      RETURN
C
  900 FORMAT(' ..... INITIATING DIIS PROCEDURE .....')
  910 FORMAT(1X,5F13.8)
      END
C*MODULE UMPDDI   *DECK UHTPROD
      SUBROUTINE UHTPROD(V1A,V1B,V2A,V2B,BUFA,BUF1,BUF2
     *,                  BUFB,BUF3,BUF4
     *,                  NOSYMA,NVCLASSA,LVCLASSA
     *,                  NOSYMB,NVCLASSB,LVCLASSB,MXIRR)
C
C -----------------------------------------------------------------
C  PARALLEL ORBITAL-HESSIAN*TRIAL-VECTOR PRODUCT (UNRESTRICTED)
C  BASED ON HTPROD
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ONE=1.0D+00, TWO=2.0D+00)
      LOGICAL GOPARR,DSKWRK,MASWRK,OUTPUT,MP2ENG,UMP2GD
      INTEGER A,B,AB,NOSYMA(*),NVCLASSA(*),LVCLASSA(MXIRR,*)
      INTEGER NOSYMB(*),NVCLASSB(*),LVCLASSB(MXIRR,*)
      DOUBLE PRECISION V1A(*),V2A(*),BUFA(NVIR,*),BUF1(NVIR,*),BUF2(*)
      DOUBLE PRECISION V1B(*),V2B(*),BUFB(NVIRB,*),BUF3(NVIRB,*),BUF4(*)
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
      INTEGER         D_UB,D_E,D_EB
      INTEGER         D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB,
     *                D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      LOGICAL         NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB,
     *                NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
     *,               NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /UMPDMS/ D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB
     *,               D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
     *,               NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB
     *,               NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
     *,               D_UB,D_E,D_EB
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
C
C  FIRST GET THE CONTRIBUTION TO THE NEW VECTOR FROM ALL ALPHA TERMS
C
      CALL DDI_DISTRIB(D_VOVO,ME,IL_VOVO,IH_VOVO,JL_VOVO,JH_VOVO)
      IJ  = 0
      DO I = 1, NOCC
        ISYM = NOSYMA(I)                  !  IRREP IDENTIFIER
        NVI = NVCLASSA(ISYM)              !  NO. VIRT. IN CLASS
        IOFF = 1
        DO IOCC = 1, I-1                     !
          IOCLASS = NOSYMA(IOCC)             ! OFF-SET OF
          IOFF = IOFF + NVCLASSA(IOCLASS)    !  VIRT. BLOCK
        END DO                               !
        DO J = 1, I
          JSYM = NOSYMA(J)
          NVJ = NVCLASSA(JSYM)
          JOFF = 1
          DO JOCC = 1, J-1
            JOCLASS = NOSYMA(JOCC)
            JOFF = JOFF + NVCLASSA(JOCLASS)
          END DO
C
C  GET VOVO AND VVOO INTEGRALS
C
          IJ = IJ + 1
          IF (IJ.GE.JL_VOVO.AND.IJ.LE.JH_VOVO) THEN
            CALL DDI_GET(D_VOVO,1,NVSQ,IJ,IJ,BUF1)
            CALL DDI_GET(D_VVOO,1,NVTR,IJ,IJ,BUF2)
C
C  GENERATE BLOCK OF REDUCED ORBITAL-HESSIAN
C
            DO IVCLASS = 1, NVI              !  LOOP OVER
              A = LVCLASSA(ISYM,IVCLASS)     !  VIRTUAL
              DO JVCLASS = 1, NVJ            !  INDICES OF
                B = LVCLASSA(JSYM,JVCLASS)   !  EACH IRREP
                IF (A.GE.B) THEN
                  AB = (A*A-A)/2 + B
                ELSE
                  AB = (B*B-B)/2 + A
                END IF
                X = BUF1(A,B)
                Y = BUF1(B,A)
                Z = BUF2(AB)
                BUFA(IVCLASS,JVCLASS) = TWO*X-Y-Z
              END DO
            END DO
C
C  THE HESSIAN-VECTOR PRODUCT
C
            CALL DGEMM('N','N', NVI,1,NVJ
     *,                ONE,BUFA,NVIR
     *,                V1A(JOFF),NVIR
     *,                ONE,V2A(IOFF),NVIR)
            IF (I.NE.J)
     *      CALL DGEMM('T','N', NVJ,1,NVI
     *,                ONE,BUFA,NVIR
     *,                V1A(IOFF),NVIR
     *,                ONE,V2A(JOFF),NVIR)
          END IF
        END DO
      END DO
C
C  NOW DO THE SAME FOR THE ALL BETA TERMS
C
      CALL DDI_DISTRIB(D_VOVOBB,ME,IL_VOVO,IH_VOVO,JL_VOVO,JH_VOVO)
      IJ  = 0
      DO I = 1, NOCCB
        ISYM = NOSYMB(I)                  !  IRREP IDENTIFIER
        NVI = NVCLASSB(ISYM)              !  NO. VIRT. IN CLASS
        IOFF = 1
        DO IOCC = 1, I-1                     !
          IOCLASS = NOSYMB(IOCC)             ! OFF-SET OF
          IOFF = IOFF + NVCLASSB(IOCLASS)    !  VIRT. BLOCK
        END DO                               !
        DO J = 1, I
          JSYM = NOSYMB(J)
          NVJ = NVCLASSB(JSYM)
          JOFF = 1
          DO JOCC = 1, J-1
            JOCLASS = NOSYMB(JOCC)
            JOFF = JOFF + NVCLASSB(JOCLASS)
          END DO
C
C  GET VOVO AND VVOO INTEGRALS
C
          IJ = IJ + 1
          IF (IJ.GE.JL_VOVO.AND.IJ.LE.JH_VOVO) THEN
            CALL DDI_GET(D_VOVOBB,1,NVSB,IJ,IJ,BUF3)
            CALL DDI_GET(D_VVOOBB,1,NVTB,IJ,IJ,BUF4)
C
C  GENERATE BLOCK OF REDUCED ORBITAL-HESSIAN
C
            DO IVCLASS = 1, NVI              !  LOOP OVER
              A = LVCLASSB(ISYM,IVCLASS)     !  VIRTUAL
              DO JVCLASS = 1, NVJ            !  INDICES OF
                B = LVCLASSB(JSYM,JVCLASS)   !  EACH IRREP
                IF (A.GE.B) THEN
                  AB = (A*A-A)/2 + B
                ELSE
                  AB = (B*B-B)/2 + A
                END IF
                X = BUF3(A,B)
                Y = BUF3(B,A)
                Z = BUF4(AB)
                BUFB(IVCLASS,JVCLASS) = TWO*X-Y-Z
              END DO
            END DO
C
C  THE HESSIAN-VECTOR PRODUCT
C
            CALL DGEMM('N','N', NVI,1,NVJ
     *,                ONE,BUFB,NVIRB
     *,                V1B(JOFF),NVIRB
     *,                ONE,V2B(IOFF),NVIRB)
            IF (I.NE.J)
     *      CALL DGEMM('T','N', NVJ,1,NVI
     *,                ONE,BUFB,NVIRB
     *,                V1B(IOFF),NVIRB
     *,                ONE,V2B(JOFF),NVIRB)
          END IF
        END DO
      END DO
C
C  NOW DO THE SAME FOR THE CROSS TERMS
C
      CALL DDI_DISTRIB(D_VOVOAB,ME,IL_VOVO,IH_VOVO,JL_VOVO,JH_VOVO)
      DO I = 1, NOCCB
        ISYM = NOSYMB(I)                  !  IRREP IDENTIFIER
        NVI = NVCLASSB(ISYM)              !  NO. VIRT. IN CLASS
        IOFF = 1
        DO IOCC = 1, I-1                     !
          IOCLASS = NOSYMB(IOCC)             ! OFF-SET OF
          IOFF = IOFF + NVCLASSB(IOCLASS)    !  VIRT. BLOCK
        END DO                               !
        DO J = 1, NOCC
          JSYM = NOSYMA(J)
          NVJ = NVCLASSA(JSYM)
          JOFF = 1
          DO JOCC = 1, J-1
            JOCLASS = NOSYMA(JOCC)
            JOFF = JOFF + NVCLASSA(JOCLASS)
          END DO
C
C  GET VOVO AND VVOO INTEGRALS
C
          IJ = (I-1)*NOCC + J
          IF (IJ.GE.JL_VOVO.AND.IJ.LE.JH_VOVO) THEN
            CALL DDI_GET(D_VOVOAB,1,NVIRB*NVIR,IJ,IJ,BUF3)
C
C  GENERATE BLOCK OF REDUCED ORBITAL-HESSIAN
C
            DO IVCLASS = 1, NVI              !  LOOP OVER
              A = LVCLASSB(ISYM,IVCLASS)     !  VIRTUAL
              DO JVCLASS = 1, NVJ            !  INDICES OF
                B = LVCLASSA(JSYM,JVCLASS)   !  EACH IRREP
C
                X = BUF3(A,B)
                BUFB(IVCLASS,JVCLASS) = TWO*X
              END DO
            END DO
C
C  THE HESSIAN-VECTOR PRODUCT
C
            CALL DGEMM('N','N', NVI,1,NVJ
     *,                ONE,BUFB,NVIRB
     *,                V1A(JOFF),NVIR
     *,                ONE,V2B(IOFF),NVIRB)
            CALL DGEMM('T','N', NVJ,1,NVI
     *,                ONE,BUFB,NVIRB
     *,                V1B(IOFF),NVIRB
     *,                ONE,V2A(JOFF),NVIR)
          END IF
        END DO
      END DO
      RETURN
      END
C*MODULE UMPDDI   *DECK UWOV
      SUBROUTINE UWOV(PMAT,PMATB,ZA,ZB,WMAT,WMATB,EORB,EORBB,T,TB,BUF)
C
C -----------------------------------------------------------------
C  COMPLETE REMAINING TERMS OF W(2) THAT DEPEND ON THE
C  OCCUPIED-VIRTUAL BLOCK OF P(2) (BASED ON WOV)
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (HALF=0.5D+00, TWO=2.0D+00)
      LOGICAL GOPARR,DSKWRK,MASWRK,OUTPUT,MP2ENG,UMP2GD
      INTEGER A,B,AV
      DOUBLE PRECISION PMAT(NBF,*),WMAT(NBF,*),ZA(*),EORB(*),
     *                 PMATB(NBF,*),WMATB(NBF,*),ZB(*),EORBB(*),
     *                 T(NOCC,*),TB(NOCCB,*),BUF(*)
      INTEGER         D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
      INTEGER         D_UB,D_E,D_EB
      INTEGER         D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB,
     *                D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
      LOGICAL         NDOOOO, NDVOOO, NDVVOO, NDVOVO
      LOGICAL         NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB,
     *                NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /MP2DMS/ D_OOOO, D_VOOO, D_VVOO, D_VOVO, D_U
     *,               NDOOOO, NDVOOO, NDVVOO, NDVOVO
      COMMON /UMPDMS/ D_OOOOAB,D_OOOOBB,D_VOOOAB,D_VOOOBA,D_VOOOBB
     *,               D_VVOOAB,D_VVOOBA,D_VVOOBB,D_VOVOAB,D_VOVOBB
     *,               NDVVOOBA, NDVVOOAB, NDVVOOBB, NDVOVOAB, NDVOVOBB
     *,               NDVOOOBA, NDVOOOAB, NDVOOOBB, NDOOOOAB, NDOOOOBB
     *,               D_UB,D_E,D_EB
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
C
C  PUT Z-VECTOR CONTRIBUTION INTO P MATRIX (BOTH ALPHA AND BETA)
C
      IA = 0
      DO I = 1, NOCC
        DO A = NOP1, NMOS
          IA = IA + 1
          PMAT(A,I) = ZA(IA)
          PMAT(I,A) = ZA(IA)
        END DO
      END DO
C
      IA = 0
      DO I = 1, NOCCB
        DO A = NOP1B, NMOS
          IA = IA + 1
          PMATB(A,I) = ZB(IA)
          PMATB(I,A) = ZB(IA)
        END DO
      END DO
C
C  TERMS OF W(2) INVOLVING EIGENVALUES TIMES P(2) ELEMENTS
C
      DO I = 1, NOCC
        DO J = 1, NOCC
          WMAT(I,J) = WMAT(I,J) -HALF*PMAT(I,J)*(EORB(I)+EORB(J))
        END DO
      END DO
      DO A = NOP1, NMOS
        DO B = NOP1, NMOS
          WMAT(A,B) = WMAT(A,B) -HALF*PMAT(A,B)*(EORB(A)+EORB(B))
        END DO
      END DO
      DO A = NOP1, NMOS
        DO I = 1, NOCC
          WMAT(I,A) = WMAT(I,A) -PMAT(I,A)*EORB(I)
          WMAT(A,I) = WMAT(I,A)
        END DO
      END DO
C
      DO I = 1, NOCCB
        DO J = 1, NOCCB
          WMATB(I,J) = WMATB(I,J) -HALF*PMATB(I,J)*(EORBB(I)+EORBB(J))
        END DO
      END DO
      DO A = NOP1B, NMOS
        DO B = NOP1B, NMOS
          WMATB(A,B) = WMATB(A,B) -HALF*PMATB(A,B)*(EORBB(A)+EORBB(B))
        END DO
      END DO
      DO A = NOP1B, NMOS
        DO I = 1, NOCCB
          WMATB(I,A) = WMATB(I,A) -PMATB(I,A)*EORBB(I)
          WMATB(A,I) = WMATB(I,A)
        END DO
      END DO
C
C  CONTRIBUTION TO THE OCCUPIED BLOCK OF W(2) FROM THE
C  VOOO-CLASS INTEGRALS
C  FIRST, DO ALL ALPHA
C
      CALL DDI_DISTRIB(D_VOOO,ME,IL_VOOO,IH_VOOO,JL_VOOO,JH_VOOO)
      CALL DCOPY(NOSQ,0.0D+00,0,T,1)
      IJK = 0
      DO I = 1, NOCC
        DO J = 1, I
          DO K = 1, NOCC
            IJK = IJK + 1
            IF (IJK.GE.JL_VOOO.AND.IJK.LE.JH_VOOO) THEN
              CALL DDI_GET(D_VOOO,1,NVIR,IJK,IJK,BUF)
              DO A = 1, NVIR
                AV = A + NOCC
                T(I,J) = T(I,J) + BUF(A)*PMAT(AV,K)*TWO
                T(I,K) = T(I,K) - BUF(A)*PMAT(AV,J)
                T(J,K) = T(J,K) - BUF(A)*PMAT(AV,I)
              END DO
              IF (I.NE.J) THEN
                DO A = 1, NVIR
                  AV = A + NOCC
                  T(J,I) = T(J,I) + BUF(A)*PMAT(AV,K)*TWO
                  T(K,I) = T(K,I) - BUF(A)*PMAT(AV,J)
                  T(K,J) = T(K,J) - BUF(A)*PMAT(AV,I)
                END DO
              END IF
            END IF
          END DO
        END DO
      END DO
C
C  DO CROSS TERM 1
C
      CALL DDI_DISTRIB(D_VOOOBA,ME,IL_VOOO,IH_VOOO,JL_VOOO,JH_VOOO)
      IJK = 0
      DO I = 1, NOCC
        DO J = 1, I
          DO K = 1, NOCCB
            IJK = IJK + 1
            IF (IJK.GE.JL_VOOO.AND.IJK.LE.JH_VOOO) THEN
              CALL DDI_GET(D_VOOOBA,1,NVIRB,IJK,IJK,BUF)
              DO A = 1, NVIRB
                AV = A + NOCCB
                T(I,J) = T(I,J) + BUF(A)*PMATB(AV,K)*TWO
              END DO
              IF (I.NE.J) THEN
                DO A = 1, NVIRB
                  AV = A + NOCCB
                  T(J,I) = T(J,I) + BUF(A)*PMATB(AV,K)*TWO
                END DO
              END IF
            END IF
          END DO
        END DO
      END DO
C
C  NEXT, ALL BETA
C
      CALL DDI_DISTRIB(D_VOOOBB,ME,IL_VOOO,IH_VOOO,JL_VOOO,JH_VOOO)
      CALL DCOPY(NOSB,0.0D+00,0,TB,1)
      IJK = 0
      DO I = 1, NOCCB
        DO J = 1, I
          DO K = 1, NOCCB
            IJK = IJK + 1
            IF (IJK.GE.JL_VOOO.AND.IJK.LE.JH_VOOO) THEN
              CALL DDI_GET(D_VOOOBB,1,NVIRB,IJK,IJK,BUF)
              DO A = 1, NVIRB
                AV = A + NOCCB
                TB(I,J) = TB(I,J) + BUF(A)*PMATB(AV,K)*TWO
                TB(I,K) = TB(I,K) - BUF(A)*PMATB(AV,J)
                TB(J,K) = TB(J,K) - BUF(A)*PMATB(AV,I)
              END DO
              IF (I.NE.J) THEN
                DO A = 1, NVIRB
                  AV = A + NOCCB
                  TB(J,I) = TB(J,I) + BUF(A)*PMATB(AV,K)*TWO
                  TB(K,I) = TB(K,I) - BUF(A)*PMATB(AV,J)
                  TB(K,J) = TB(K,J) - BUF(A)*PMATB(AV,I)
                END DO
              END IF
            END IF
          END DO
        END DO
      END DO
C
C  DO CROSS TERM 2
C
      CALL DDI_DISTRIB(D_VOOOAB,ME,IL_VOOO,IH_VOOO,JL_VOOO,JH_VOOO)
      IJK = 0
      DO I = 1, NOCCB
        DO J = 1, I
          DO K = 1, NOCC
            IJK = IJK + 1
            IF (IJK.GE.JL_VOOO.AND.IJK.LE.JH_VOOO) THEN
              CALL DDI_GET(D_VOOOAB,1,NVIR,IJK,IJK,BUF)
              DO A = 1, NVIR
                AV = A + NOCC
                TB(I,J) = TB(I,J) + BUF(A)*PMAT(AV,K)*TWO
              END DO
              IF (I.NE.J) THEN
                DO A = 1, NVIR
                  AV = A + NOCC
                  TB(J,I) = TB(J,I) + BUF(A)*PMAT(AV,K)*TWO
                END DO
              END IF
            END IF
          END DO
        END DO
      END DO
C
C  GLOBALLY SUM CONTRIBUTION TO W(OCC,OCC) (THIRD TERM)
C
      CALL DDI_GSUMF(1005,T,NOSQ)
      DO I = 1, NOCC
        DO J = 1, NOCC
          WMAT(I,J) = WMAT(I,J) - T(I,J)
        END DO
      END DO
      CALL DDI_GSUMF(1006,TB,NOSB)
      DO I = 1, NOCCB
        DO J = 1, NOCCB
          WMATB(I,J) = WMATB(I,J) - TB(I,J)
        END DO
      END DO
      RETURN
      END
C*MODULE UMPDDI   *DECK UBTRAN2
      SUBROUTINE UBTRAN2(CMO,CMOB,OAOA_AA,OAOA_BB,OAOA_AB,OAOA_BA
     *,                  GNS,MINJ,JSHL,DUM)
C
C -----------------------------------------------------------------
C  SECOND HALF BACK-TRANSFORMATION OF AMPLITUDES (BASED ON BTRAN2)
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
      LOGICAL OUTPUT,MP2ENG,UMP2GD
      DOUBLE PRECISION CMO(NBF,*),CMOB(NBF,*),GNS(NBF,*),DUM(*)
     *,                OAOA_AA(NOCC,*),OAOA_BB(NOCCB,*)
     *,                OAOA_AB(NOCC,*),OAOA_BA(NOCCB,*)
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC
     *,               NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1
     *,               NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
      COMMON /UMP2XB/ NBCORE,NACTB,NOCCB,NVIRB,NOVB,NOSB,NVSB,NOTB,
     *                NVTB,NCP1B,NOP1B,NATB,IOSTB
C
C  FIRST, DO THE ALL ALPHA CONTRIBUTION:
C  BACK-TRANSFORM OCCUPIED INDICES TO SHELL AO INDICES
C
      CALL DGEMM('N','T', NACT,JSHL,NACT
     *,          ONE,OAOA_AA(NCP1,NCP1),NOCC
     *,          CMO(MINJ,NCP1),NBF
     *,          ZERO,DUM,NACT)
C
C  BACK-TRANSFORM OCCUPIED INDICES TO ALL AO INDICES
C
      CALL DGEMM('N','N', NBF,JSHL,NACT
     *,          ONE,CMO(1,NCP1),NBF
     *,          DUM,NACT
     *,          ZERO,GNS,NBF)
C
C  NEXT, DO THE ALL BETA CONTRIBUTION:
C  BACK-TRANSFORM OCCUPIED INDICES TO SHELL AO INDICES
C
      CALL DGEMM('N','T', NACTB,JSHL,NACTB
     *,          ONE,OAOA_BB(NCP1B,NCP1B),NOCCB
     *,          CMOB(MINJ,NCP1B),NBF
     *,          ZERO,DUM,NACTB)
C
C  BACK-TRANSFORM OCCUPIED INDICES TO ALL AO INDICES
C
      CALL DGEMM('N','N', NBF,JSHL,NACTB
     *,          ONE,CMOB(1,NCP1B),NBF
     *,          DUM,NACTB
     *,          ONE,GNS,NBF)
C
C  NEXT, DO THE FIRST CROSS TERM CONTRIBUTION:
C  BACK-TRANSFORM OCCUPIED INDICES TO SHELL AO INDICES
C
      CALL DGEMM('N','T', NACT,JSHL,NACTB
     *,          ONE,OAOA_AB(NCP1,NCP1B),NOCC
     *,          CMOB(MINJ,NCP1B),NBF
     *,          ZERO,DUM,NACT)
C
C  BACK-TRANSFORM OCCUPIED INDICES TO ALL AO INDICES
C
      CALL DGEMM('N','N', NBF,JSHL,NACT
     *,          ONE,CMO(1,NCP1),NBF
     *,          DUM,NACT
     *,          ONE,GNS,NBF)
C
C  NEXT, DO THE SECOND CROSS TERM CONTRIBUTION:
C  BACK-TRANSFORM OCCUPIED INDICES TO SHELL AO INDICES
C
      CALL DGEMM('N','T', NACTB,JSHL,NACT
     *,          ONE,OAOA_BA(NCP1B,NCP1),NOCCB
     *,          CMO(MINJ,NCP1),NBF
     *,          ZERO,DUM,NACTB)
C
C  BACK-TRANSFORM OCCUPIED INDICES TO ALL AO INDICES
C
      CALL DGEMM('N','N', NBF,JSHL,NACTB
     *,          ONE,CMOB(1,NCP1B),NBF
     *,          DUM,NACTB
     *,          ONE,GNS,NBF)
      RETURN
      END
C*MODULE UMPDDI   *DECK UPAR2PDM
      SUBROUTINE UPAR2PDM(INEW,JNEW,KNEW,LNEW,INVPERM,Q4,SFAC,
     2                   PHFA,PHFB,PMP2A,PMP2B,GNS,DAB,ISHL,JSHL,KSHL,
     3                   DABMAX,POPLE)
C
C -----------------------------------------------------------------
C  COMBINE TERMS OF 2-PARTICLE DENSITY MATRIX FOR A SHELL OF
C  DERIVATIVE INTEGRALS - COMPUTING SCF AND SEPARABLE TERMS
C  CALLED FROM PJKDMP2; BASED ON PAR2PDM
C -----------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION INVPERM(4),PHFA(*),PHFB(*),PMP2A(*),PMP2B(*),DAB(*)
      LOGICAL POPLE
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXAO=2047)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CCG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /SHLLMN/ IGXYZ(4,35),JGXYZ(4,35),KGXYZ(4,35),LGXYZ(4,35)
      COMMON /SHLNRM/ PNRM(35)
      LOGICAL OUTPUT,MP2ENG,UMP2GD
      COMMON /MP2XTR/ NBF,NACORE,NBSQ,NBTR,NSTR,MXSHL,NACT,NOCC,
     *                NVIR,NOV,NOSQ,NVSQ,NOTR,NVTR,NCP1,NOP1,
     *                NATR,ICHANL,OUTPUT,MP2ENG,IOST,NMOS,UMP2GD
C
CJMS  LABELLED COMMON GSPG80 DEFINED FOR COMPUTATIONAL EFFICIENCY.
CJMS  FOR SP BASES ONLY, IT CONTAINS THE E ARRAY WHICH IS THE DAB
CJMS  ARRAY WITH INDICES IN REVERSE ORDER: E(I,J,K,L)= DAB(L,K,J,I)
CJMS  AND IS USED IN SUB JKDG80 (MODULE GRD2B). IT ORIGINATES IN:
CJMS
CJMS     1. SUBS DABCLU, DABDFT, DABGVB, DABMC AND DABMP2 (MODULE
CJMS        GRD2A) AND SUB DABPAU (MODULE EFPAUL) WHICH ARE ALL
CJMS        CALLED BY SUB JKDER (MODULE GRD2A)
CJMS
CJMS     2. SUB DABCLU (MODULE GRD2A) WHICH IS CALLED BY SUB EFDEN OF
CJMS        MODULE EFGRD2
CJMS
CJMS     3. SUB PAR2PDM (MODULE MP2DDI) WHICH IS CALLED BY SUB PJKDMP2
CJMS        OF MODULE MP2DDI
C
CCMA  AND NOW:
CCMA     4. SUB UPAR2PDM (MODULE UMPDDI) WHICH IS CALLED BY SUB PJKDMP2
CCMA        OF MODULE MP2DDI
C
      COMMON /GSPG80/ E(4,4,4,4)
C
C  NON-SEPARABLE TERMS ARE IN GNS
C  GNS HAS 4 DIMENSIONS TO SIMPLIFY INDEXING LATER
C  LEADING DIMENSION MUST BE SET TO NBF
C
      DIMENSION GNS(NBF,JSHL,KSHL,ISHL)
C
      DIMENSION IINDX(4)
C
      PARAMETER (ZER=0.0D+00, PT5=0.5D+00, F02=2.0D+00, F04=4.0D+00)
C
      DABMAX= ZER
      MINI= KMIN(INEW)
      MINJ= KMIN(JNEW)
      MINK= KMIN(KNEW)
      MINL= KMIN(LNEW)
      MAXI= KMAX(INEW)
      MAXJ= KMAX(JNEW)
      MAXK= KMAX(KNEW)
      MAXL= KMAX(LNEW)
      LOCI= KLOC(INEW)-MINI
      LOCJ= KLOC(JNEW)-MINJ
      LOCK= KLOC(KNEW)-MINK
      LOCL= KLOC(LNEW)-MINL
C
      IF(POPLE) THEN
         DO I=1,4
            IGXYZ(1,I)=64*(I-1)+1
            JGXYZ(1,I)=16*(I-1)
            KGXYZ(1,I)= 4*(I-1)
            LGXYZ(1,I)=   (I-1)
         END DO
C
         DO L=MINL,MAXL
            IINDX(4) = L-(MINL-1)
            NNU = LOCL+L
            DO K=MINK,MAXK
               IINDX(3) = K-(MINK-1)
               NMU = LOCK+K
               MUNU= IA(MAX0(NMU,NNU))+MIN0(NMU,NNU)
               DO J=MINJ,MAXJ
                  IINDX(2) = J-(MINJ-1)
                  NSI = LOCJ+J
                  DO I=MINI,MAXI
                     IINDX(1) = I-(MINI-1)
                     NLA = LOCI+I
                     LASI= IA(MAX0(NLA,NSI))+MIN0(NLA,NSI)
                     MUSI= IA(MAX0(NMU,NSI))+MIN0(NMU,NSI)
                     LANU= IA(MAX0(NLA,NNU))+MIN0(NLA,NNU)
                     MULA= IA(MAX0(NMU,NLA))+MIN0(NMU,NLA)
                     NUSI= IA(MAX0(NNU,NSI))+MIN0(NNU,NSI)
C
CJMS  LET'S SAVE A FEW MULTIPLICATIONS
CCMA  FOLLOWING CODE FROM JMS VERSION OF DABUMP
C
                     DF1 =(PMP2A(MUNU)+PHFA (MUNU)+
     2                     PMP2B(MUNU)+PHFB (MUNU))*PHFA(LASI)+
     3                    (PMP2A(MUNU)+PHFA (MUNU)+
     4                     PMP2B(MUNU)+PHFB (MUNU))*PHFB(LASI)+
     5                    (PMP2A(LASI)+PMP2B(LASI))*PHFA(MUNU)+
     6                    (PMP2A(LASI)+PMP2B(LASI))*PHFB(MUNU)
                     DQ1 =(PMP2A(MUSI)+PHFA (MUSI))*PHFA(LANU)+
     2                    (PMP2A(MULA)+PHFA (MULA))*PHFA(NUSI)+
     3                    (PMP2B(MUSI)+PHFB (MUSI))*PHFB(LANU)+
     4                    (PMP2B(MULA)+PHFB (MULA))*PHFB(NUSI)+
     5                     PMP2A(NUSI)*PHFA(MULA)+
     6                     PMP2B(NUSI)*PHFB(MULA)+
     7                     PMP2A(LANU)*PHFA(MUSI)+
     8                     PMP2B(LANU)*PHFB(MUSI)
C
C  MP2 NON-SEPARABLE TERM
C
                     IP = IINDX(INVPERM(1))
                     JP = IINDX(INVPERM(2))
                     KP = IINDX(INVPERM(3))
                     LP = IINDX(INVPERM(4))
C
C  TOTAL MP2 DENSITY
C
                     DF1=-F02*DQ1+F04*(DF1+F02*GNS(LP,JP,KP,IP))
C
C  NON-CANONICAL SHELL 4-LABEL WEIGHT
C
                     DF1= DF1*Q4*SFAC
C
                     IF(DABMAX.LT. ABS(DF1)) DABMAX = ABS(DF1)
                     E(I,J,K,L)= DF1
                  END DO
               END DO
            END DO
         END DO
      ELSE
C
C D AND HIGHER FUNCTIONS OR HONDO ONLY RUNS
C
         DO I=MINI,MAXI
            P1I= PNRM(I)
            IINDX(1) = I-(MINI-1)
            NLA = LOCI+I
            DO J=MINJ,MAXJ
               P2J= P1I*PNRM(J)
               IINDX(2) = J-(MINJ-1)
               NSI = LOCJ+J
               LASI= IA(MAX0(NLA,NSI))+MIN0(NLA,NSI)
               DO K=MINK,MAXK
                  P3K= P2J*PNRM(K)
                  IINDX(3) = K-(MINK-1)
                  NMU = LOCK+K
                  DO L=MINL,MAXL
                     P4L= P3K*PNRM(L)
                     IINDX(4) = L-(MINL-1)
                     NNU = LOCL+L
                     MUNU= IA(MAX0(NMU,NNU))+MIN0(NMU,NNU)
                     MUSI= IA(MAX0(NMU,NSI))+MIN0(NMU,NSI)
                     LANU= IA(MAX0(NLA,NNU))+MIN0(NLA,NNU)
                     MULA= IA(MAX0(NMU,NLA))+MIN0(NMU,NLA)
                     NUSI= IA(MAX0(NNU,NSI))+MIN0(NNU,NSI)
C
CJMS  LET'S SAVE A FEW MULTIPLICATIONS
CCMA  FOLLOWING CODE FROM JMS VERSION OF DABUMP
C
                     DF1 =(PMP2A(MUNU)+PHFA (MUNU)+
     2                     PMP2B(MUNU)+PHFB (MUNU))*PHFA(LASI)+
     3                    (PMP2A(MUNU)+PHFA (MUNU)+
     4                     PMP2B(MUNU)+PHFB (MUNU))*PHFB(LASI)+
     5                    (PMP2A(LASI)+PMP2B(LASI))*PHFA(MUNU)+
     6                    (PMP2A(LASI)+PMP2B(LASI))*PHFB(MUNU)
                     DQ1 =(PMP2A(MUSI)+PHFA (MUSI))*PHFA(LANU)+
     2                    (PMP2A(MULA)+PHFA (MULA))*PHFA(NUSI)+
     3                    (PMP2B(MUSI)+PHFB (MUSI))*PHFB(LANU)+
     4                    (PMP2B(MULA)+PHFB (MULA))*PHFB(NUSI)+
     5                     PMP2A(NUSI)*PHFA(MULA)+
     6                     PMP2B(NUSI)*PHFB(MULA)+
     7                     PMP2A(LANU)*PHFA(MUSI)+
     8                     PMP2B(LANU)*PHFB(MUSI)
C
C  MP2 NON-SEPARABLE TERM
C
                     IP = IINDX(INVPERM(1))
                     JP = IINDX(INVPERM(2))
                     KP = IINDX(INVPERM(3))
                     LP = IINDX(INVPERM(4))
C
C  TOTAL MP2 DENSITY
C
                     DF1=-F02*DQ1+F04*(DF1+F02*GNS(LP,JP,KP,IP))
C
C  NON-CANONICAL AO 4-LABEL WEIGHT
C
                     DF1= DF1*P4L*SFAC
                     IF(NMU .EQ.NNU ) DF1= DF1*PT5
                     IF(NLA .EQ.NSI ) DF1= DF1*PT5
                     IF(MUNU.EQ.LASI) DF1= DF1*PT5
C
                     IF(DABMAX.LT. ABS(DF1)) DABMAX = ABS(DF1)
                     IJKL=IGXYZ(1,I)+JGXYZ(1,J)+KGXYZ(1,K)+LGXYZ(1,L)
                     DAB(IJKL) = DF1
                  END DO
               END DO
            END DO
         END DO
      END IF
C
C  SYMMETRISE DENSITY MATRIX
C
      IF(INEW.EQ.JNEW) THEN
         DO L=MINL,MAXL
            DO K=MINK,MAXK
               DO J=MINJ,MAXJ
                  DO I=MINI,MAXI
                     IF(POPLE) THEN
                        DF1=(E(I,J,K,L)+E(J,I,K,L))*PT5
                        E(I,J,K,L)= DF1
                        E(J,I,K,L)= DF1
                     ELSE
                        IJKL=IGXYZ(1,I)+JGXYZ(1,J)+KGXYZ(1,K)+LGXYZ(1,L)
                        JIKL=IGXYZ(1,J)+JGXYZ(1,I)+KGXYZ(1,K)+LGXYZ(1,L)
                        DF1=(DAB(IJKL) + DAB(JIKL))*PT5
                        DAB(IJKL) = DF1
                        DAB(JIKL) = DF1
                     END IF
                  END DO
               END DO
            END DO
         END DO
      END IF
      IF(KNEW.EQ.LNEW) THEN
         DO L=MINL,MAXL
            DO K=MINK,MAXK
               DO J=MINJ,MAXJ
                  DO I=MINI,MAXI
                     IF(POPLE) THEN
                        DF1=(E(I,J,K,L)+E(I,J,L,K))*PT5
                        E(I,J,K,L)= DF1
                        E(I,J,L,K)= DF1
                     ELSE
                        IJKL=IGXYZ(1,I)+JGXYZ(1,J)+KGXYZ(1,K)+LGXYZ(1,L)
                        IJLK=IGXYZ(1,I)+JGXYZ(1,J)+KGXYZ(1,L)+LGXYZ(1,K)
                        DF1=(DAB(IJKL) + DAB(IJLK))*PT5
                        DAB(IJKL) = DF1
                        DAB(IJLK) = DF1
                     END IF
                  END DO
               END DO
            END DO
         END DO
      END IF
      IJ = IA(MAX0(INEW,JNEW))+MIN0(INEW,JNEW)
      KL = IA(MAX0(KNEW,LNEW))+MIN0(KNEW,LNEW)
      IF(IJ.EQ.KL) THEN
         DO L=MINL,MAXL
            DO K=MINK,MAXK
               DO J=MINJ,MAXJ
                  DO I=MINI,MAXI
                     IF(POPLE) THEN
                        DF1=(E(I,J,K,L)+E(K,L,I,J))*PT5
                        E(I,J,K,L)= DF1
                        E(K,L,I,J)= DF1
                     ELSE
                        IJKL=IGXYZ(1,I)+JGXYZ(1,J)+KGXYZ(1,K)+LGXYZ(1,L)
                        KLIJ=IGXYZ(1,K)+JGXYZ(1,L)+KGXYZ(1,I)+LGXYZ(1,J)
                        DF1=(DAB(IJKL) + DAB(KLIJ))*PT5
                        DAB(IJKL) = DF1
                        DAB(KLIJ) = DF1
                     END IF
                  END DO
               END DO
            END DO
         END DO
      END IF
      RETURN
      END
C
