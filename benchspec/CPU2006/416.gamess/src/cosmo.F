C 26 MAR 02 - KRG - MOVE L2/L3 BLAS DGER,DTRMM,DTRMV,DTRSM TO BLAS.SRC
C  8 OCT 01 - KRG - CONSTS: CHANGE VARIABLE ICH TO ICHG
C  6 SEP 01 - MWS - ADD DUMMY ARGUMENTS TO NAMEIO CALL
C 25 JUN 01 - MWS - USE COMMON BLOCK WFNOPT
C 10 APR 00 - MWS - REMOVE STATIC MEMORY FROM COMMONS
C 25 MAR 00 - KKB,AK - NEW MODULE FOR COSMO SOLVATION SCHEME
C
C*MODULE EFINTB  *DECK COSMIN
      SUBROUTINE COSMIN
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      LOGICAL ISEPS, USEPS
      LOGICAL GOPARR,MASWRK,DSKWRK
C
      PARAMETER (MXATM=500, MAXDEN=25*MXATM, LENABC=2000, NPPA=1082)
C
      COMMON /COSFRQ/ FCOORD(3,MXATM),POS0(NPPA),
     *                COSZAN0(NPPA),EDIEL0,FINDEX,ICFREQ,ICFREQ1
      COMMON /COSMO1/ SE2,SECORR,ETOTS,CDUM,QVCOSMO,
     *                CSPOT(NPPA),ICORR,ITRIPO,ITRIP2,ITRIP3,ITRIP4,
     *                NATCOS,NQS,ITERC
      COMMON /COSMO2/ QENUC,ELAST,EMP2COS,EMP2COS2,ETOTSMP,SAVESE,
     *                EMP2LAST,MP2TRIP,MP2ITER,MP2FACT
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /ISEPS / ISEPS, USEPS
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /SOLV  / FEPSI,RDS,DISEX2,COSURF(3,LENABC),SRAD(MXATM),
     *                QDEN(MAXDEN),AR(LENABC),
     *                NSPA,NPS,NPS2,NDEN,NPSPHER
      COMMON /SOLVI / IATSP(LENABC+1),N0(2),NP1,NP2,ISUPSKIP
      COMMON /SOLV1 / EPSI, RSOLV, DELSC, DISEX, ITRIP
C
      PARAMETER (NNAM=5)
      DIMENSION QNAM(NNAM),KQNAM(NNAM)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA SCOSMO/8HCOSGMS  /
#else
      CHARACTER*8 :: SCOSMO_STR
      EQUIVALENCE (SCOSMO, SCOSMO_STR)
      DATA SCOSMO_STR/"COSGMS  "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QNAM/8HEPSI    ,8HNSPA    ,8HRSOLV   ,8HDELSC   ,8HDISEX   /
#else
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"EPSI    ","NSPA    ","RSOLV   ","DELSC   ",
     * "DISEX   "/
#endif
      DATA KQNAM/3,1,3,3,3/
C
      MP2ITER=0
      NQS=0
      NPS=0
      ICORR=0
C
      ISEPS = .FALSE.
      USEPS = .FALSE.
C
      ITRIP = 0
      ICORR = 0
      ISUPSKIP = 0
      ITRIPO = 0
      ITRIP2 = 0
      ITRIP3 = 0
      ITRIP4 = 0
      ITERC = 0
      MP2TRIP = 0
      ICFREQ = 0
C
C READ $COSGMS.
C        OPTIONS DELSC AND DISEX ARE FOR POSSIBLE FUTURE USE
C
      EPSI = 0.0D+00
      RSOLV = 1.2D+00
      NSPA  = 92
      DELSC = 1.0D+00
      DISEX = 2.0D+00
C
      JRET = 0
      CALL NAMEIO(IR,JRET,SCOSMO,NNAM,QNAM,KQNAM,
     *            EPSI,NSPA,RSOLV,DELSC,DISEX,
     *            0,0,0,0,
     *       0,0,0,0,0,   0,0,0,0,0,     0,0,0,0,0,
     *       0,0,0,0,0,   0,0,0,0,0,     0,0,0,0,0,   0,0,0,0,0,
     *       0,0,0,0,0,   0,0,0,0,0,     0,0,0,0,0,   0,0,0,0,0)
      IF(JRET.EQ.2) THEN
        IF (MASWRK) WRITE(IW,9028)
        CALL ABRT
      END IF
      IF(JRET.EQ.1) RETURN
C
      ISEPS = .TRUE.
      CALL SVINIT
C
C  PRINT $COSGMS OPTIONS
C
      IF(MASWRK) WRITE(IW,9058) EPSI,NSPA,RSOLV,DELSC,DISEX
C
      RETURN
C
 9028 FORMAT(1X,'ERROR IN $COSGMS INPUT - STOP ')
 9058 FORMAT(/5X,'$COSGMS OPTIONS'/5X,15("-")/
     *  5X,7HEPSI  =,F8.3,5X,7HNSPA  =,I8,5X,7HRSOLV =,F8.2/
     *  5X,7HDELSC =,F8.2,5X,7HDISEX =,F8.2)
C
      END
C*MODULE COSMO   *DECK BTOC
      SUBROUTINE BTOC(COORD,ABCMAT,BH,LABC,LENAB2)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500, MAXDEN=25*MXATM, LENABC=2000, NPPA=1082)
      PARAMETER (ONE=1.0D+00)
      PARAMETER (TOANGS=0.52917724924D+00, TOBOHR=ONE/TOANGS)
C
      DIMENSION COORD(3,*),ABCMAT(LENAB2),BH(LABC)
      DIMENSION XX(3),XA(3)
C
      COMMON /ATLIM / LIMLOW(MXATM),LIMSUP(MXATM)
      COMMON /COSMO1/ SE2,SECORR,ETOTS,CDUM,QVCOSMO,
     *                CSPOT(NPPA),ICORR,ITRIPO,ITRIP2,ITRIP3,ITRIP4,
     *                NATCOS,NQS,ITERC
      COMMON /COSMO3/ COSZAN(NPPA),CORZAN(3,NPPA)
      COMMON /INFOA / NATHF,ICH,MUL,NUM,NQMT,NE,NORBA,NORBB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /MOLKST/ NUMAT,NAT(MXATM),NFIRST(MXATM),NMIDLE(MXATM),
     1                NLAST(MXATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /MULTIP/ DD(107),QQ(107),AM(107),AD(107),AQ(107)
      COMMON /SOLV  / FEPSI,RDS,DISEX2,COSURF(3,LENABC),SRAD(MXATM),
     *                QDEN(MAXDEN),AR(LENABC),
     *                NSPA,NPS,NPS2,NDEN,NPSPHER
      COMMON /SOLVI / IATSP(LENABC+1),N0(2),NP1,NP2,ISUPSKIP
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA NONE/4HNONE/
#else
      CHARACTER*4 :: NONE_STR
      EQUIVALENCE (NONE, NONE_STR)
      DATA NONE_STR/"NONE"/
#endif
C
      SQ0=SQRT(3.0D+00)
      SQ1=SQRT(1.5D+00)
      SQ2=SQRT(15.0D+00)
      SQ3=SQRT(2.5D+00)
      SQ4=SQRT(5.0D+00)
      SQ5=SQRT(17.5D+00)
      SQ6=SQRT(35.0D+00)
C
#if defined(SPEC_CPU)
      WRITE(6,'(A,I8,I8)')'ENTERING BTOC (VALUE OF NSP,NSP2):',NPS,NPS2
#else
      WRITE(6,*)'ENTERING BTOC (VALUE OF NSP,NSP2):',NPS,NPS2
#endif

      CALL AOLIM()
      IF(MPCTYP.EQ.NONE) THEN
         NUMAT=NATHF
         NQS=NPS
         DO 2 I=1,NUMAT
            DO 3 J=1,3
               COORD(J,I)=TOANGS*COORD(J,I)
  3         CONTINUE
  2      CONTINUE
      ENDIF
C
C      WRITE(68,*)'COSURF AT THE TOP OF BTOC:'
C
      DO 10 I=1,NPS
         J=IATSP(I)
         RI=SRAD(J)-RDS
         DO 11 IX=1,3
            COSURF(IX,I)=COSURF(IX,I)*RI+COORD(IX,J)
            IF(MPCTYP.EQ.NONE) CORZAN(IX,I)=COSURF(IX,I)*TOBOHR
   11    CONTINUE
   10 CONTINUE
C
C FILLING B-MATRIX
C
      WRITE(6,*) "FILLING B MATRIX:"
      I0=NPS2-NDEN
      IDEN=0
      DO 50 I=1,NUMAT
         IA=LIMLOW(I)
         IDEL=LIMSUP(I)-IA
         NATI=INT(ZAN(I))
         IF(MPCTYP.EQ.NONE) THEN
            DDI=  0.529177D+00
            QQI2=(0.529177D+00)**2*0.50D+00
            SOI= (0.529177D+00)**3*0.50D+00
            SHI= (0.529177D+00)**4*0.50D+00
C                FMI= 0.529177D+00
C                FDI=(0.529177D+00)**2
C                FQI=(0.529177D+00)**3*0.50D+00
C                FOI=(0.529177D+00)**4*0.50D+00
C                FHI=(0.529177D+00)**5*0.50D+00
         ELSE
            DDI=DD(NATI)*2*0.529177D+00
            QQI2=(0.529177D+00*QQ(NATI))**2
         ENDIF
         DO 20 IX=1,3
            XX(IX)=COORD(IX,I)
   20    CONTINUE
C         DO 1 IK=1,NPS
C               WRITE(11,*)COSURF(1,IK),COSURF(2,IK),COSURF(3,IK)
C   1     CONTINUE
         DO 40 IPS=1,NPS
C            WRITE(6,*)"VALUE OF IPS,NPS IN LOOP 40:",IPS,NPS
            I1=I0+IPS*NDEN
            DIST=0.0D+00
            DO 30 IX=1,3
               XA(IX)=COSURF(IX,IPS)-XX(IX)
               DIST=DIST+XA(IX)**2
   30       CONTINUE
C
C            WRITE(11,*)"NPSPHER= ",NPSPHER
C            IF(DIST.LE.0.05D+00) THEN
C              WRITE(6,*)"DIST LESS THAN .05 BOHR:",DIST,IPS,I,NPSPHER,
C     *XA(1),XA(2),XA(3),COSURF(1,IPS),COSURF(2,IPS),COSURF(3,IPS)
C                CALL ABRT
C            ENDIF
            RM1=1.0D+00/SQRT(DIST)
C
C        MONOPOLE TYPE B-MATRIX ELEMENT
C
C           WRITE(6,*)"MONOPOLE TYPE B-MATRIX ELEMENTS:",IDEN,IPS
C
            ABCMAT(IDEN+1+I1)=RM1
C            ABCMAT(IDEN+1+I1)=RM1*FMI
C
C        IF WE HAVE A HYDROGEN, THEN, WE ONLY HAVE ONE ABCMAT VALUE:
C
            IF (IDEL.EQ.0  .AND.  MPCTYP.NE.NONE) GO TO 40
C
            RM3=RM1**3
            RM5=RM1**5
            RM7=RM1**7
            RM9=RM1**9
            R2=DIST
C
C        DIPOLE TYPE QUANTITIES
C          NOTE THAT FOR THE GAMESS IMPLEMENTATION, THE ORDERING
C          IS SEQUENTIAL (MONOPOLE, 3*DIPOLES, 6*QUADRUPOLES,
C          7*OCTUPOLES, AND 9*HEXADECAPOLES).
C
C          WRITE(6,*)"DIPOLE TYPE B-MATRIX ELEMENTS:",IDEN,IPS
C
            IF(MPCTYP.EQ.NONE) THEN
               ABCMAT(IDEN+2+I1)=XA(1)*DDI*RM3
               ABCMAT(IDEN+3+I1)=XA(2)*DDI*RM3
               ABCMAT(IDEN+4+I1)=XA(3)*DDI*RM3
C                  ABCMAT(IDEN+2+I1)=XA(1)*FDI*RM3
C                  ABCMAT(IDEN+3+I1)=XA(2)*FDI*RM3
C                  ABCMAT(IDEN+4+I1)=XA(3)*FDI*RM3
            ELSE
               ABCMAT(IDEN+2+I1)=XA(1)*DDI*RM3
               ABCMAT(IDEN+4+I1)=XA(2)*DDI*RM3
               ABCMAT(IDEN+7+I1)=XA(3)*DDI*RM3
            ENDIF
C
C        QUADRUPOLE QUANTITIES:
C
            IF(MPCTYP.EQ.NONE) THEN
C
C     NOTE:  THESE WERE THE CARTESIAN QUADRUPOLES, AS IMPLEMENTED FIRST
C
C               ABCMAT(IDEN+3+I1)=3.0D+00*XA(1)**2*QQI2*RM5-QQI2*RM3
C               ABCMAT(IDEN+6+I1)=3.0D+00*XA(2)**2*QQI2*RM5-QQI2*RM3
C               ABCMAT(IDEN+10+I1)=3.0D+00*XA(3)**2*QQI2*RM5-QQI2*RM3
C               ABCMAT(IDEN+5+I1)=6.0D+00*XA(1)*XA(2)*QQI2*RM5
C               ABCMAT(IDEN+8+I1)=6.0D+00*XA(1)*XA(3)*QQI2*RM5
C               ABCMAT(IDEN+9+I1)=6.0D+00*XA(3)*XA(2)*QQI2*RM5
C
C            THESE ARE THE SPHERICAL QUADRUPOLES
C
C               FQUAD=-QQI2*RM5
               FQUAD=QQI2*RM5
C               FQUAD=FQI*RM5
C
               ABCMAT(IDEN+5+I1)=FQUAD*(3.0D+00*XA(3)**2-R2)
C               ABCMAT(IDEN+5+I1)=FQUAD*(3.0D+00*XA(3)**2)
               ABCMAT(IDEN+6+I1)=FQUAD*XA(1)*XA(3)*SQ0*2.0D+00
               ABCMAT(IDEN+7+I1)=FQUAD*XA(2)*XA(3)*SQ0*2.0D+00
               ABCMAT(IDEN+8+I1)=FQUAD*(XA(1)**2-XA(2)**2)*SQ0
               ABCMAT(IDEN+9+I1)=FQUAD*XA(2)*XA(1)*SQ0*2.0D+00
C
C            WRITE(78,*)"SPHERICAL QUADS,BTOC (IDEN+5+I1):",IDEN+5+I1
C            WRITE(78,*)ABCMAT(IDEN+5+I1),ABCMAT(IDEN+6+I1),
C     *     ABCMAT(IDEN+7+I1),ABCMAT(IDEN+8+I1),ABCMAT(IDEN+9+I1)
C
            ELSE
               ABCMAT(IDEN+3+I1)=RM1+3.0D+00*XA(1)**2*QQI2*RM5-QQI2*RM3
               ABCMAT(IDEN+6+I1)=RM1+3.0D+00*XA(2)**2*QQI2*RM5-QQI2*RM3
               ABCMAT(IDEN+10+I1)=RM1+3.0D+00*XA(3)**2*QQI2*RM5-QQI2*RM3
               ABCMAT(IDEN+5+I1)=6.0D+00*XA(1)*XA(2)*QQI2*RM5
               ABCMAT(IDEN+8+I1)=6.0D+00*XA(1)*XA(3)*QQI2*RM5
               ABCMAT(IDEN+9+I1)=6.0D+00*XA(3)*XA(2)*QQI2*RM5
            ENDIF
C
C       SPHERICAL OCTUPOLE TYPE QUANTITIES (NOT YET IMPLEMENTED
C       FOR SEMIEMPIRICAL)
C
            IF(MPCTYP.NE.NONE) GOTO 40
C
            FOCT=RM7*SOI
C            FOCT=RM7*FOI
            ABCMAT(IDEN+10+I1)=FOCT*XA(3)*
     *             (5.0D+00*XA(3)**2-3.0D+00*R2)
            ABCMAT(IDEN+11+I1)=FOCT*SQ1*XA(1)*
     *             (5.0D+00*XA(3)**2-R2)
            ABCMAT(IDEN+12+I1)=FOCT*SQ1*XA(2)*
     *             (5.0D+00*XA(3)**2-R2)
            ABCMAT(IDEN+13+I1)=FOCT*SQ2*XA(3)*
     *             (XA(1)**2-XA(2)**2)
            ABCMAT(IDEN+14+I1)=FOCT*2.0D+00*SQ2*
     *              XA(1)*XA(2)*XA(3)
            ABCMAT(IDEN+15+I1)=FOCT*SQ3*XA(1)*
     *             (XA(1)**2-3.0D+00*XA(2)**2)
            ABCMAT(IDEN+16+I1)=FOCT*SQ3*XA(2)*
     *             (3.0D+00*XA(1)**2-XA(2)**2)
C
C          IF(IPS.LT.10) THEN
C            WRITE(78,*)"OCTUPOLES IN BTOC:"
C            WRITE(78,*)ABCMAT(IDEN+10+I1),ABCMAT(IDEN+11+I1),
C     *       ABCMAT(IDEN+12+I1),ABCMAT(IDEN+13+I1),ABCMAT(IDEN+14+I1),
C     *       ABCMAT(IDEN+15+I1),ABCMAT(IDEN+16+I1)
C          ENDIF
C
C
C
C       SPHERICAL HEXADECAPOLE TYPE QUANTITIES (NOT YET IMPLEMENTED
C       FOR SEMIEMPIRICAL)
C
C           WRITE(6,*)"HEXADECAPOLE TYPE B MATRIX ELEMENTS:",IDEN,IPS
C
            FHEX=RM9*SHI
C            FHEX=RM9*FHI
            ABCMAT(IDEN+17+I1)=FHEX*0.25D+00*
     *          (35.0D+00*XA(3)**4-30.0D+00*XA(3)**2*R2+3.0D+00*R2**2)
            ABCMAT(IDEN+18+I1)=FHEX*SQ3*XA(1)*XA(3)*
     *          (7.0D+00*XA(3)**2-3.0D+00*R2)
            ABCMAT(IDEN+19+I1)=FHEX*SQ3*XA(2)*XA(3)*
     *          (7.0D+00*XA(3)**2-3.0D+00*R2)
            ABCMAT(IDEN+20+I1)=FHEX*SQ4*0.50D+00*(XA(1)**2-XA(2)**2)*
     *          (7.0D+00*XA(3)**2-R2)
            ABCMAT(IDEN+21+I1)=FHEX*SQ4*XA(1)*XA(2)*
     *          (7.0D+00*XA(3)**2-R2)
            ABCMAT(IDEN+22+I1)=FHEX*SQ5*XA(3)*XA(1)*
     *          (XA(1)**2-3*XA(2)**2)
            ABCMAT(IDEN+23+I1)=FHEX*SQ5*XA(3)*XA(2)*
     *          (3.0D+00*XA(1)**2-XA(2)**2)
            ABCMAT(IDEN+24+I1)=FHEX*SQ6*0.25D+00*
     *          (XA(1)**4-6.0D+00*XA(1)**2*XA(2)**2+XA(2)**4)
            ABCMAT(IDEN+25+I1)=FHEX*SQ6*XA(1)*XA(2)*
     *          (XA(1)**2-XA(2)**2)
C
   40    CONTINUE
C
      IF(MPCTYP.EQ.NONE) THEN
         IDEN=IDEN+25
      ELSE
         IDEN=IDEN+1+IDEL**2
      ENDIF
C
 50   CONTINUE
      I1=NPS2+NDEN*NPS
C
C  FILLING C-MATRIX
C
       WRITE(6,*)"FILLING C-MATRIX:"
C
C      FACT=-0.5D+00*2*13.6058D+00*0.5292D+00*FEPSI
       FACT=-0.5D+00*0.5292D+00*FEPSI
C        FACT=-0.5D+00*FEPSI
C
      DO 110 I=1,NDEN
         DO 80 K=1,NPS
            BHK=0.0D+00
            KK2=(K*(K-1))/2
            DO 60 L=1,K
   60       BHK=BHK+ABCMAT(I+L*NDEN+I0)*ABCMAT(KK2+L)
            DO 70 L=K+1,NPS
   70       BHK=BHK+ABCMAT(I+L*NDEN+I0)*ABCMAT((L*(L-1))/2+K)
            BH(K)=BHK
   80    CONTINUE
         DO 100 J=1,I
            CIJ=0.0D+00
            DO 90 K=1,NPS
   90       CIJ=CIJ+BH(K)*ABCMAT(J+K*NDEN+I0)
            I1=I1+1
            ABCMAT(I1)=FACT*CIJ
  100    CONTINUE
  110 CONTINUE
C      ENDIF
C
      DO 130 I=1,NPS
         J=IATSP(I)
         RM=SRAD(J)-RDS
         DO 130 IX=1,3
            COSURF(IX,I)=(COSURF(IX,I)-COORD(IX,J))/RM
  130    CONTINUE
C
       DO 175 IF=1,NUMAT
          DO 235 JF=1,3
             COORD(JF,IF)=TOBOHR*COORD(JF,IF)
  235     CONTINUE
  175  CONTINUE
C
      RETURN
      END
C*MODULE COSMO   *DECK CONSTS
      SUBROUTINE CONSTS(COORD,ABCMAT,XSP,NSET,NSETF,IPIV,DIN,
     *                  TM,DIRTM,CONORM,NAR,NIPA,LIPA,SUDE,ISUDE,
     *                  NN,LABC,LENAB2,LPPA,NATM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500, MAXDEN=25*MXATM, LENABC=2000, NPPA=1082)
      PARAMETER (NSRING=20)
      PARAMETER (ONE=1.0D+00, FOUR=4.0D+00)
      PARAMETER (TOANGS=0.52917724924D+00, TOBOHR=ONE/TOANGS)
C
C          DIMENSION OF -NSET- IS NPPA*MXATM/2
C          DIMENSION OF -LIPA- SHOULD BE 10 TIMES THAT OF -NIPA-
      DIMENSION COORD(3,*),ABCMAT(LENAB2),XSP(3,LABC),
     *          NSET(*),NSETF(LABC),IPIV(LABC),TM(3,3,NATM),
     *          DIRTM(3,LPPA),CONORM(3,LABC),NAR(LABC),
     *          NIPA(400),LIPA(4000),SUDE(4,500),ISUDE(4,500),
     *          NN(3,NATM)
      DIMENSION XX(3),XA(3),XI(3),XJ(3),FZ(2),YX(2),YY(3),TRP(3),
     *          SVX(3),SVY(3),TVX(3),TVY(3),PHISET(50),ISET(50),
     *          CC(3),SS(3),EM(3),EE(3,3),
     *          XD(3),RVX(3),RVY(3),XJA(3),XJB(3),XJC(3),XTA(3,3)
C
      LOGICAL DIN(LPPA)
      LOGICAL ISUP
C
      COMMON /COSDIR/ DIRVEC(3,NPPA),DIRSM(3,NPPA),DIRSMH(3,NPPA/3)
      COMMON /INFOA / NATHF,ICH,MUL,NUM,NQMT,NE,NORBA,NORBB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MOLKST/ NUMAT,NAT(MXATM),NFIRST(MXATM),NMIDLE(MXATM),
     1                NLAST(MXATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /SOLV  / FEPSI,RDS,DISEX2,COSURF(3,LENABC),SRAD(MXATM),
     *                QDEN(MAXDEN),AR(LENABC),
     *                NSPA,NPS,NPS2,NDEN,NPSPHER
      COMMON /SOLVI / IATSP(LENABC+1),N0(2),NP1,NP2,ISUPSKIP
      COMMON /SOLV1 / EPSI, RSOLV, DELSC, DISEX, ITRIP
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA NONE/4HNONE/
#else
      CHARACTER*4 :: NONE_STR
      EQUIVALENCE (NONE, NONE_STR)
      DATA NONE_STR/"NONE"/
#endif
C
C THIS ROUTINE CONSTRUCTS OR UPDATES THE SOLVENT-ACCESSIBLE SURFACE
C
      WRITE(6,*) "JUST INSIDE CONSTS:"
C
      IF(MPCTYP.EQ.NONE) THEN
         NUMAT=NATHF
         DO 2 I=1,NUMAT
            DO 3 J=1,3
               COORD(J,I)=TOANGS*COORD(J,I)
  3         CONTINUE
  2      CONTINUE
      ENDIF
C
C MAKE COORDINATES A BIT ASYMMETRIC IN ORDER TO AVOID
C SYMMETRY PROBLEMS WITH CAVITY CONSTRUCTION
C
C ADDITIONALLY, THE CHANGE OVER TOANGS/TOBOHR IS MADE TO GET
C THE SAME DISTORTED GEOMETRY AS IN TURBOMOL
C
      DO 4 I=1,NUMAT
         DO 4 J=1,3
C            COORD(J,I)=COORD(J,I)+COS(I*J*0.1D+00)*3.0D-09
             COORD(J,I)=COORD(J,I)+COS(5.0D+00*I+J)*1.0D-05
C            COORD(J,I)=COORD(J,I)*TOANGS+COS(5.0*I+J)*1.0D-05
C            COORD(J,I)=COORD(J,I)/TOANGS
C            WRITE(6,*)"I=",I,COORD(J,I)
   4  CONTINUE
C
      DO 5 I=1,NUMAT
          WRITE(6,987) (COORD(J,I),J=1,3)
   5  CONTINUE
 987  FORMAT(3F15.10)
C
C     BUT, AS WE WANT TO MAKE THE WHOLE CONSTRUCTION SYMMETRIC THIS
C     MUST ALSO RUN WITH SYMMETRIC COORDINATES; VOLKER NOV 1998
C     SO THIS HAS TO BE MOVED OUT AT SOME POINT IN THE FUTURE
C
      ISUP=(ISUPSKIP.EQ.1)
      MAXNPS=INT(SQRT(2.0D+00*LENAB2+0.251D+00)-NDEN-0.5D+00)
      MAXNPS=MIN(MAXNPS,LENABC)
      IF (MAXNPS .LT. 3*NUMAT) THEN
         WRITE(IW,*) 'INCREASE -IPS- GUESS IN CALLER'
         CALL ABRT
      ENDIF
C
      IF (ISUP) THEN
         NPS3=LENABC-NPS
         DO 10 I=NPS,1,-1
            IATSP(NPS3+I)=IATSP(I)
            DO 11 IX=1,3
               COSURF(IX,NPS3+I)=COSURF(IX,I)
   11       CONTINUE
   10    CONTINUE
         NPS3=NPS3+1
      END IF
C
C      WRITE(6,*)"SET UP STUFF IN CONSTS, NPPA::",NPPA
C
      SDIS=0.0D+00
      FDIAG=1.05D+00*SQRT(NPPA+0.0D+00)
      PI=2.0D+00*ACOS(0.D+00)
      FDIAGR=2.1D+00*SQRT(PI)
      FPINPPA = (FOUR*PI)/NPPA
      IATSP(LENABC+1)=0
      INSET=1
      ILIPA=0
      NPS = 0
      AREA=0.0D+00
      VOLUME=0.0D+00
      THRSH=1.0D-08
C
C     WRITE(6,*)"LOOP OVER ATOMS IN CONSTS, NSPA:",NSPA
C
      DO 340 I=1,NUMAT
         NIPI=0
         DS=SQRT(4.0D+00/NSPA)
         IF(INT(ZAN(I)).EQ.1  .AND.  MPCTYP.NE.NONE) DS=2*DS
         C2DS=COS(2.0D+00*DS)
         R=SRAD(I)
         RI=R-RDS
         FVOL=(4.0D+00*PI*RI**2)/NPPA
C         WRITE(6,*)"I,FVOL:",I,FVOL
         DO 20 IX=1,3
   20    XA(IX)=COORD(IX,I)
         NPS0=NPS+1
         IF(ISUP) THEN
            IF (NPS .GE. NPS3) THEN
               WRITE(6,*) 'NPS .GT. NPS3'
               CALL ABRT
            END IF
            NPS2=NPS3
            IF (IATSP(NPS3) .NE. I) GO TO 340
            DO 30 IPS=NPS2,LENABC+1
   30       IF(IATSP(IPS) .NE. I) GO TO 40
   40       NPS3=IPS
C
C TRANSFORM COSURF ACCORDING TO TM(INV)
C
C            WRITE(6,*)"TRANSFORM COSURF:"
C
            DO 50 J=NPS2,NPS3-1
               XX(1)=COSURF(1,J)
               XX(2)=COSURF(2,J)
               XX(3)=COSURF(3,J)
               COSURF(1,J)=XX(1)*TM(1,1,I)+XX(2)*TM(1,2,I)+XX(3)*TM(1,3,
     1I)
               COSURF(2,J)=XX(1)*TM(2,1,I)+XX(2)*TM(2,2,I)+XX(3)*TM(2,3,
     1I)
               COSURF(3,J)=XX(1)*TM(3,1,I)+XX(2)*TM(3,2,I)+XX(3)*TM(3,3,
     1I)
   50       CONTINUE
            NN1=NN(1,I)
            NN2=NN(2,I)
            NN3=NN(3,I)
         ELSE
C
C SEARCH FOR 3 NEAREST NEIGHBOR ATOMS
C
C            WRITE(6,*)"SEARCH FOR NEAREST NEIGHBORS:"
C
            DIST1=1.0D+20
            DIST2=1.0D+20
            DIST3=1.0D+20
            NN1=0
            NN2=0
            NN3=0
         DO 70 J=1,NUMAT
            IF (J.EQ. I) GO TO 70
            DIST=0.D+00
            DO 60 IX=1,3
   60       DIST=DIST+(XA(IX)-COORD(IX,J))**2
            IF(DIST.LT.(R+SRAD(J))**2) THEN
              ILIPA=ILIPA+1
              NIPI=NIPI+1
              LIPA(ILIPA)=J
            END IF
            IF ((DIST+0.05D+00-DIST3).LT.THRSH) THEN
               DIST3=DIST
               NN3=J
            END IF
            IF ((DIST3+0.05D+00-DIST2).LT.THRSH) THEN
               DIST=DIST2
               DIST2=DIST3
               DIST3=DIST
               NN3=NN2
               NN2=J
            END IF
            IF ((DIST2+0.05D+00-DIST1).LT.THRSH) THEN
               DIST=DIST1
               DIST1=DIST2
               DIST2=DIST
               NN2=NN1
               NN1=J
            END IF
   70    CONTINUE
         NIPA(I)=NIPI
         NN(1,I)=NN1
         NN(2,I)=NN2
         NN(3,I)=NN3
         ENDIF
C
C BUILD NEW TRANSFORMATION MATRIX
C
C         WRITE(6,*)"BUILD NEW TRANSFORMATION MATRIX:"
C
         IF (NN1 .EQ. 0) THEN
            TM(1,1,I)=1.0D+00
            TM(1,2,I)=0.0D+00
            TM(1,3,I)=0.0D+00
         ELSE
            DIST1=0.0D+00
            DO 80 IX=1,3
   80       DIST1=DIST1+(XA(IX)-COORD(IX,NN1))**2
            DIST=1.0D+00/SQRT(DIST1)
            TM(1,1,I)=(COORD(1,NN1)-XA(1))*DIST
            TM(1,2,I)=(COORD(2,NN1)-XA(2))*DIST
            TM(1,3,I)=(COORD(3,NN1)-XA(3))*DIST
         END IF
   90    IF(NN2.EQ.0) THEN
            DIST=SQRT(TM(1,2,I)**2+TM(1,1,I)**2)
            TM(2,1,I)=-TM(1,2,I)/DIST
            TM(2,2,I)=TM(1,1,I)/DIST
            TM(2,3,I)=0.D+00
            SP=XX(1)*TM(1,1,I)+XX(2)*TM(1,2,I)+XX(3)*TM(1,3,I)
         ELSE
            DIST2=0.0D+00
            DO 100 IX=1,3
  100       DIST2=DIST2+(XA(IX)-COORD(IX,NN2))**2
            DIST=1.0D+00/SQRT(DIST2)
            XX(1)=(COORD(1,NN2)-XA(1))*DIST
            XX(2)=(COORD(2,NN2)-XA(2))*DIST
            XX(3)=(COORD(3,NN2)-XA(3))*DIST
            SP=XX(1)*TM(1,1,I)+XX(2)*TM(1,2,I)+XX(3)*TM(1,3,I)
            IF (SP*SP .GT. 0.99D+00) THEN
               NN2=NN3
               NN3=0
               DIST2=DIST3
               GO TO 90
            END IF
            SININV=1.0D+00/SQRT(1.0D+00-SP*SP)
            TM(2,1,I)=(XX(1)-SP*TM(1,1,I))*SININV
            TM(2,2,I)=(XX(2)-SP*TM(1,2,I))*SININV
            TM(2,3,I)=(XX(3)-SP*TM(1,3,I))*SININV
         END IF
         TM(3,1,I)=TM(1,2,I)*TM(2,3,I)-TM(2,2,I)*TM(1,3,I)
         TM(3,2,I)=TM(1,3,I)*TM(2,1,I)-TM(2,3,I)*TM(1,1,I)
         TM(3,3,I)=TM(1,1,I)*TM(2,2,I)-TM(2,1,I)*TM(1,2,I)
C
C TRANSFORM DIRVEC ACCORDING TO TM
C
C         WRITE(6,*)"TRANSFORM DIRVEC:"
C
         DO 110 J=1,NPPA
            XX(1)=DIRVEC(1,J)
            XX(2)=DIRVEC(2,J)
            XX(3)=DIRVEC(3,J)
            DO 110 IX=1,3
               X=XX(1)*TM(1,IX,I)+XX(2)*TM(2,IX,I)+XX(3)*TM(3,IX,I)
               DIRTM(IX,J)=X
  110    CONTINUE
C
C FIND THE POINTS OF THE BASIC GRID ON THE SAS
C
         NAREA=0
         DO 160 J = 1,NPPA
            DIN(J)=.FALSE.
            DO 130 IX=1,3
               XX(IX) = XA(IX) + DIRTM(IX,J)* R
  130       CONTINUE
C
C           --- ONLY USE POINTS WHICH DO NOT LIE INSIDE ANOTHER ATOM
C           --- MARK THOSE BY SETTING DIN = .TRUE.
C           --- WE NEED ONLY TRY THOSE ATOMS INTERSECTING ATOM I
C
C
            DO 150 IK=ILIPA-NIPA(I)+1,ILIPA
               K = LIPA(IK)
               DIST=0.0D+00
               DO 140 IX=1,3
                  DIST = DIST + (XX(IX) - COORD(IX,K))**2
  140          CONTINUE
               DIST=SQRT(DIST)-SRAD(K)
               IF (DIST.LT.0.0D+00) GO TO 160
  150       CONTINUE
            NAREA=NAREA+1
            VOLUME=VOLUME+FVOL*
     *      (DIRTM(1,J)*XA(1)+DIRTM(2,J)*XA(2)+DIRTM(3,J)*XA(3)+RI)
            DIN(J)=.TRUE.
  160    CONTINUE
         IF(NAREA.EQ.0) GOTO 340
         AREA=AREA+NAREA*RI*RI
C
C         WRITE(6,*)"DO 120 LOOP:"
C
         IF (ISUP) THEN
            DO 120 J=NPS2,NPS3-1
               NPS=NPS+1
               IATSP(NPS)=I
               XX(1)=COSURF(1,J)
               XX(2)=COSURF(2,J)
               XX(3)=COSURF(3,J)
               COSURF(1,NPS)=XX(1)*TM(1,1,I)+XX(2)*TM(2,1,I)+XX(3)*TM(3,
     11,I)
               COSURF(2,NPS)=XX(1)*TM(1,2,I)+XX(2)*TM(2,2,I)+XX(3)*TM(3,
     12,I)
               COSURF(3,NPS)=XX(1)*TM(1,3,I)+XX(2)*TM(2,3,I)+XX(3)*TM(3,
     13,I)
  120       CONTINUE
C
         ELSE
C
         IF(ZAN(I).EQ.1) THEN
            DO 43 J=1,N0(2)
               NPS=NPS+1
               IATSP(NPS)=I
               XX(1)=DIRSMH(1,J)
               XX(2)=DIRSMH(2,J)
               XX(3)=DIRSMH(3,J)
         COSURF(1,NPS)=XX(1)*TM(1,1,I)+XX(2)*TM(2,1,I)+XX(3)*TM(3,1,I)
         COSURF(2,NPS)=XX(1)*TM(1,2,I)+XX(2)*TM(2,2,I)+XX(3)*TM(3,2,I)
         COSURF(3,NPS)=XX(1)*TM(1,3,I)+XX(2)*TM(2,3,I)+XX(3)*TM(3,3,I)
  43        CONTINUE
         ELSE
            DO 44 J=1,N0(1)
               NPS=NPS+1
               IATSP(NPS)=I
               XX(1)=DIRSM(1,J)
               XX(2)=DIRSM(2,J)
               XX(3)=DIRSM(3,J)
         COSURF(1,NPS)=XX(1)*TM(1,1,I)+XX(2)*TM(2,1,I)+XX(3)*TM(3,1,I)
         COSURF(2,NPS)=XX(1)*TM(1,2,I)+XX(2)*TM(2,2,I)+XX(3)*TM(3,2,I)
         COSURF(3,NPS)=XX(1)*TM(1,3,I)+XX(2)*TM(2,3,I)+XX(3)*TM(3,3,I)
   44       CONTINUE
          ENDIF
         ENDIF
C
  200    SDIS0=SDIS
         DO 210 IPS=NPS0,NPS
            NAR(IPS)=0
            XSP(1,IPS)=0.0D+00
            XSP(2,IPS)=0.0D+00
            XSP(3,IPS)=0.0D+00
  210    CONTINUE
C
C
         DO 250 J=1,NPPA
            IF (.NOT. DIN(J)) GO TO 250
            SPM=-1.0D+00
            X1=DIRTM(1,J)
            X2=DIRTM(2,J)
            X3=DIRTM(3,J)
            DO 220 IPS=NPS0,NPS
               SP=X1*COSURF(1,IPS)+X2*COSURF(2,IPS)+X3*COSURF(3,IPS)
               IF (SP .LT. SPM) GO TO 220
               SPM=SP+1.0D-05
               IPM=IPS
  220       CONTINUE
            IF (SPM .LT. C2DS) THEN
               NPS=NPS+1
               IF (NPS .GT. MAXNPS) THEN
                  WRITE(IW,*) 'NPS IS GREATER THAN MAXNPS'
                  WRITE(IW,*) 'USE SMALLER NSPA'
                  CALL ABRT
               END IF
               DO 230 IX=1,3
  230          COSURF(IX,NPS)=DIRTM(IX,J)
               IATSP(NPS)=I
               GO TO 200
            END IF
            NAR(IPM)=NAR(IPM)+1
            DO 240 IX=1,3
  240       XSP(IX,IPM)=XSP(IX,IPM)+DIRTM(IX,J)
  250    CONTINUE
         SDIS=0.0D+00
         IPS=NPS0-1
         IF(NPS.LT.IPS) GOTO 200
  260    IPS=IPS+1
  352  IF(NAR(IPS).EQ.0)THEN
       NPS=NPS-1
       IF(NPS.LT.IPS) GOTO 200
       DO 369 JPS=IPS,NPS
       NAR(JPS)=NAR(JPS+1)
       XSP(1,JPS)=XSP(1,JPS+1)
       XSP(2,JPS)=XSP(2,JPS+1)
  369  XSP(3,JPS)=XSP(3,JPS+1)
       GOTO 352
       ENDIF
         DIST=0.0D+00
         DO 280 IX=1,3
            X=XSP(IX,IPS)
            DIST=DIST+X*X
  280    CONTINUE
         SDIS=SDIS+DIST
         DIST=1.0D+00/SQRT(DIST)
         DO 290 IX=1,3
  290    COSURF(IX,IPS)=XSP(IX,IPS)*DIST
         IF(IPS.LT.NPS) GOTO 260
         IF (ABS(SDIS-SDIS0) .GT. 1.0D-05) GO TO 200
         DO 310 IPS=NPS0,NPS
            NSETF(IPS)=INSET
            INSET=INSET+NAR(IPS)
            NAR(IPS)=0
            DO 300 IX=1,3
  300       XSP(IX,IPS)=XA(IX)+COSURF(IX,IPS)*RI
  310    CONTINUE
         DO 330 J=1,NPPA
            IF (.NOT. DIN(J)) GO TO 330
            SPM=-1.0D+00
            X1=DIRTM(1,J)
            X2=DIRTM(2,J)
            X3=DIRTM(3,J)
            DO 320 IPS=NPS0,NPS
               SP=X1*COSURF(1,IPS)+X2*COSURF(2,IPS)+X3*COSURF(3,IPS)
               IF (SP .LT. SPM) GO TO 320
               SPM=SP+1.0D-05
               IPM=IPS
  320       CONTINUE
            IF (SPM .LT. C2DS) GO TO 330
            NARA=NAR(IPM)
            NSET(NSETF(IPM)+NARA)=J
            NAR(IPM)=NARA+1
            AR(IPM) = FPINPPA*RI*RI*NAR(IPM)
  330    CONTINUE
  340 CONTINUE
C
C
C   SAVE A COPY OF NORMALS
C
      DO 342 IC=1,NPS
         DO 344 IX=1,3
            CONORM(IX,IC)=COSURF(IX,IC)
  344 CONTINUE
  342 CONTINUE
C
      AREA=AREA*4.0D+00*3.14159D+00/NPPA
      WRITE(6,*)"VALUE OF AREA IN CONSTS BEFORE SURFACE CLOSURE:",AREA
C
C     SURFACE CLOSURE BEGINS HERE
C
      NPSPHER = NPS
      DO 2900 I=1,NUMAT
2900  DIN(I)=.TRUE.
      DO 2901 J=1,NPS
2901  DIN(IATSP(J))=.FALSE.
      NIP=0
C
C GENERATION OF SEGMENTS ALONG THE INTERSECTION RINGS
C
      ILIPA=0
      DO 3800 IA=1,NUMAT-1
        IF(DIN(IA)) GO TO 3800
        RA=SRAD(IA)
        DO 3001 IX=1,3
3001    XTA(IX,1)=COORD(IX,IA)
        DO 3700 IIB=ILIPA+1,ILIPA+NIPA(IA)
          IB=LIPA(IIB)
          IF(IB .LE.IA) GO TO 3700
          IF(DIN(IB)) GO TO 3700
          RB=SRAD(IB)
          DAB=0.0D+00
          NSAB=0
          DO 3002 IX=1,3
            XTA(IX,2)=COORD(IX,IB)
            XX(IX)=XTA(IX,2)-XTA(IX,1)
3002      DAB=DAB+XX(IX)**2
          DAB=SQRT(DAB)
          IF(RA+RB .LT. DAB) GO TO 3700
          COSA=(RA**2+DAB**2-RB**2)/(2*DAB*RA)
          COSB=(RB**2+DAB**2-RA**2)/(2*DAB*RB)
          SINA=SQRT(1.0D+00-COSA**2)
          DA=RA*COSA
          HH=RA*SINA
          DDD=RSOLV*(COSA+COSB)/DAB
          FZ(1)=(1.0D+00-COS(HH*PI/RA))/2
          FZ(2)=(1.0D+00-COS(HH*PI/RB))/2
          IF(COSA*COSB .LT. 0.0D+00) FZ(1)=1.0D+00
          IF(COSA*COSB .LT. 0.0D+00) FZ(2)=1.0D+00
C         COSGZ=(RA*RA+RB*RB-DAB*DAB)/(RA*RB)
C         DC=RSOLV*SQRT(2-COSGZ)
          YX(1)=RSOLV/SRAD(IA)
          YX(2)=RSOLV/SRAD(IB)
          DO 3005 IX=1,3
3005      XD(IX)=XTA(IX,1)+DA*XX(IX)/DAB
C
C
C  CREATE RING VECTORS
C
          RVX(1)=XX(2)*DIRTM(3,1)-XX(3)*DIRTM(2,1)
          RVX(2)=XX(3)*DIRTM(1,1)-XX(1)*DIRTM(3,1)
          RVX(3)=XX(1)*DIRTM(2,1)-XX(2)*DIRTM(1,1)
          DIST=SQRT(RVX(1)**2+RVX(2)**2+RVX(3)**2)
          DO 3006 IX=1,3
3006      RVX(IX)=HH*RVX(IX)/DIST
          RVY(1)=(XX(2)*RVX(3)-XX(3)*RVX(2))/DAB
          RVY(2)=(XX(3)*RVX(1)-XX(1)*RVX(3))/DAB
          RVY(3)=(XX(1)*RVX(2)-XX(2)*RVX(1))/DAB
#if !defined(SPEC_CPU)
          PRINT *,IA,IB,HH
#endif
C
C
C NOW ALL TRIPLE POINTS ON THE RING ARE SEARCHED
          NTRP=0
          NTRP2=0
          DO 3390 IIC=ILIPA+1,ILIPA+NIPA(IA)
            IC=LIPA(IIC)
            IF (IC.EQ.IB) THEN
               GO TO 3390
            ENDIF
            RC=SRAD(IC)
            DABC=0.0D+00
            SP=0.0D+00
            DO 3302 IX=1,3
              XXX=COORD(IX,IC)-XD(IX)
              XTA(IX,3)=COORD(IX,IC)
              SP=SP+XXX*XX(IX)
3302        DABC=DABC+XXX**2
            DABC=SQRT(DABC)
            COSA=SP/DAB/DABC
            SINA=SQRT(1.0D+00-COSA*COSA)
            CJ=(DABC*DABC+HH*HH-RC*RC)/(2*DABC*HH*SINA)
            IF (CJ .LT. 1.0D+00) NTRP2=NTRP2+2
            IF (CJ .GT. 1.0D+00 .OR. CJ .LT. -1.0D+00) THEN
               GO TO 3390
            ENDIF
            SJ=SQRT(1.0D+00-CJ*CJ)
C
C  CREATE RING VECTORS
            DO 3305 IX=1,3
3305        TVX(IX)=(XTA(IX,3)-XD(IX))-COSA*DABC*XX(IX)/DAB
            DIST=SQRT(TVX(1)**2+TVX(2)**2+TVX(3)**2)
            DO 3306 IX=1,3
3306        TVX(IX)=HH*TVX(IX)/DIST
            TVY(1)=(XX(2)*TVX(3)-XX(3)*TVX(2))/DAB
            TVY(2)=(XX(3)*TVX(1)-XX(1)*TVX(3))/DAB
            TVY(3)=(XX(1)*TVX(2)-XX(2)*TVX(1))/DAB
            DO 3380 L=-1,1,2
              IL=NTRP+1
              DO 3310 IX=1,3
3310          TRP(IX)=XD(IX)+CJ*TVX(IX)+SJ*TVY(IX)*L
              DO 3320 IK=ILIPA+1,ILIPA+NIPA(IA)
                K=LIPA(IK)
                IF(K.EQ.IB .OR. K.EQ.IC) GO TO 3320
                DABCK=0.0D+00
                DO 3315 IX=1,3
3315            DABCK=DABCK+(TRP(IX)-COORD(IX,K))**2
                DABCK=SQRT(DABCK)
                IF (DABCK .LT. SRAD(K)) GO TO 3380
3320          CONTINUE
              NTRP=NTRP+1
              SPX=0.0D+00
              SPY=0.0D+00
              DO 3322 IX=1,3
                SPX=SPX+RVX(IX)*(TRP(IX)-XD(IX))
                SPY=SPY+RVY(IX)*(TRP(IX)-XD(IX))
3322          CONTINUE
              PHI=ACOS(SPX/HH**2)
              IF(SPY.LT.0.0D+00) PHI=-PHI
              PHISET(IL)=PHI
              SP=0.0D+00
              DO 3324 IX=1,3
3324          SP=SP+(-SPY*RVX(IX)+SPX*RVY(IX))
     &               *(TRP(IX)-XTA(IX,3))
              ISET(NTRP)=1
              IF(SP.LT.0.0D+00) ISET(NTRP)=-1
C IF THE TRIPLE POINT IS NEW THE CORRESPONDING SURFACE PATCHES ARE ADDED
              IF(IC.LE.IB) GO TO 3380
              DO 3325 IX=1,3
                SVX(IX)=XTA(IX,2)-XTA(IX,1)
                SVY(IX)=XTA(IX,3)-XTA(IX,1)
3325          EM(IX)=0.0D+00
              DO 3330 IIIA=1,3
                DIST=0.0D+00
                DO 3326 IX=1,3
                  XXX=XTA(IX,IIIA)-TRP(IX)
                  DIST=DIST+XXX*XXX
3326            EE(IX,IIIA)=XXX
                DIST=SQRT(DIST)
                DO 3328 IX=1,3
                  XXX=EE(IX,IIIA)/DIST
                  EM(IX)=EM(IX)+XXX
3328            EE(IX,IIIA)=XXX
3330          CONTINUE
              DIST=0.0D+00
              SPN=0.0D+00
              DO 3334 IIIA=1,3
                DIST=DIST+EM(IIIA)**2
                IIIB=MOD(IIIA,3)+1
                IIIC=6-IIIA-IIIB
                XXX=SVX(IIIB)*SVY(IIIC)-SVX(IIIC)*SVY(IIIB)
                SPN=SPN+XXX*(XTA(IIIA,1)-TRP(IIIA))
                YY(IIIA)=XXX
                SP=0.0D+00
                DO 3332 IX=1,3
3332            SP=SP+EE(IX,IIIB)*EE(IX,IIIC)
                CC(IIIA)=SP
3334          SS(IIIA)=SQRT(1.0D+00-SP*SP)
              SAR=-PI
              DIST=SQRT(DIST)
              DO 3338 IIIA=1,3
                EM(IIIA)=EM(IIIA)/DIST
                IIIB=MOD(IIIA,3)+1
                IIIC=6-IIIA-IIIB
3338          SAR=SAR+
     &        ACOS((CC(IIIA)-CC(IIIB)*CC(IIIC))/SS(IIIB)/SS(IIIC))
              SAR=SAR*RSOLV**2/3
              DO 3370 IIIA=1,3
                DIST=0.0D+00
                SPA=0.0D+00
                DO 3345 IX=1,3
                  XXX=1.4D+00*EM(IX)+EE(IX,IIIA)
                  DIST=DIST+XXX*XXX
                  SPA=SPA+YY(IX)*XXX
3345            CC(IX)=XXX
                DIST=1.0D+00/SQRT(DIST)
                FACT=RSOLV*DIST
C
C IF THE SEGMENT CENTER LIES BELOW THE PLANE OF THE THREE ATOMS DROP IT
                IF(SPA*FACT/SPN .GT. 1) THEN
                  GO TO 3370
                END IF
                NPS=NPS+1
                III=IA
                IF(IIIA.EQ.2) III=IB
                IF(IIIA.EQ.3) III=IC
                IATSP(NPS)=III
                AR(NPS)=SAR
                AREA=AREA+SAR
                SPNN=0.0D+00
                DO 3348 IX=1,3
                  CONORM(IX,NPS)=-CC(IX)*DIST
                  XSP(IX,NPS)=TRP(IX)+FACT*CC(IX)
                  COSURF(IX,NPS)=(XSP(IX,NPS)-COORD(IX,III))/
     *(SRAD(III)-RDS)
                  SPNN=SPNN+XSP(IX,NPS)*CONORM(IX,NPS)
3348            CONTINUE
                VOLUME=VOLUME+SAR*SPNN
3370          CONTINUE
3380        CONTINUE
3390      CONTINUE
C
C
C SORT THE SET OF TRIPLE POINTS ON THE RING
          IF(MOD(NTRP,2).NE.0 ) THEN
             WRITE(6,*) 'ODD NTRP'
             CALL ABRT
          END IF
          IF( NTRP.GT.18) THEN
             WRITE(6,*) 'NTRP ZU GRO_'
             CALL ABRT
          END IF
          IF (NTRP+NTRP2 .EQ.0) THEN
            PHISET(1)=0
            PHISET(2)=2*PI
            ISET(1)=1
            ISET(2)=-1
            NTRP=2
          END IF
C
C
3400      IC=0
          DO 3410 L=2,NTRP
          IF (PHISET(L).LT.PHISET(L-1)) THEN
            PHI=PHISET(L)
            III=ISET(L)
            PHISET(L)=PHISET(L-1)
            ISET(L)=ISET(L-1)
            PHISET(L-1)=PHI
            ISET(L-1)=III
            IC=IC+1
          END IF
3410      CONTINUE
          IF(IC.GT.0) GO TO 3400
          IF(ISET(1) .EQ. -1) THEN
            PHISET(1)=PHISET(1)+2*PI
            GO TO 3400
          END IF
C
C NOW FOR EACH CONTINUOUS SECTION OF THE RING TRIANGLES ARE CREATED
C
          SUMPHI=0.0D+00
          IPS0=NPS
          DO 3600 L=2,NTRP,2
            K=L-1
            PHIU=PHISET(K)
            PHIO=PHISET(L)
C---        NSA=(PHIO-PHIU)/2/PI*NSRING
            NSA=INT((NSRING*(PHIO-PHIU))/(2.0D+00*PI))
            NSA=MAX(NSA+1,2)
            SUMPHI=SUMPHI+PHIO-PHIU
            DP=(PHIO-PHIU)/(NSA-1)
            DO 3550 ICHG=1,2
            DO 3550 JA=ICHG,NSA,2
              JB=MAX(JA-1,1)
              JC=MIN(JA+1,NSA)
              PHI=PHIU+(JA-1)*DP
              CPHI=COS(PHI)
              SPHI=SIN(PHI)
              DO 3545 IX=1,3
                CA=XD(IX)+(CPHI*RVX(IX)+SPHI*RVY(IX))*FZ(ICHG)
                CA=CA+(XTA(IX,ICHG)-CA)*YX(ICHG)
3545          XJA(IX)=CA+(XTA(IX,3-ICHG)-XTA(IX,ICHG))*DDD*
     &                (1.0D+00-FZ(ICHG))
              PHI=PHIU+(JB-1)*DP
              CPHI=COS(PHI)
              SPHI=SIN(PHI)
              DO 3546 IX=1,3
                CA=XD(IX)+CPHI*RVX(IX)+SPHI*RVY(IX)
3546          XJB(IX)=CA+(XTA(IX,3-ICHG)-CA)*YX(3-ICHG)
              PHI=PHIU+(JC-1)*DP
              CPHI=COS(PHI)
              SPHI=SIN(PHI)
              DO 3547 IX=1,3
                CA=XD(IX)+CPHI*RVX(IX)+SPHI*RVY(IX)
3547          XJC(IX)=CA+(XTA(IX,3-ICHG)-CA)*YX(3-ICHG)
              NPS=NPS+1
              NSAB=NSAB+1
              SP=0.0D+00
              D1=0.0D+00
              D2=0.0D+00
              SPN=0.0D+00
              DIST=0.0D+00
              IATSP(NPS)=IB
              IF(ICHG.EQ.2) IATSP(NPS)=IA
              IAT=IATSP(NPS)
              DO 3548 IX=1,3
                XX1=XJC(IX)-XJB(IX)
                XX2=XJA(IX)-XJB(IX)
                D1=D1+XX1*XX1
                D2=D2+XX2*XX2
                SP=SP+XX1*XX2
                XSP(IX,NPS)=(XJA(IX)*0.5D+00+XJB(IX)+XJC(IX))/2.5D+00
                COSURF(IX,NPS)=(XSP(IX,NPS)-COORD(IX,IAT))/
     *                         (SRAD(IAT)-RDS)
                I2=MOD(IX,3)+1
                I3=MOD(I2,3)+1
                CONORM(IX,NPS)=(XJC(I2)-XJB(I2))*(XJA(I3)-XJB(I3))
     *                        -(XJA(I2)-XJB(I2))*(XJC(I3)-XJB(I3))
                DIST=DIST+CONORM(IX,NPS)**2
                SPN=SPN+CONORM(IX,NPS)*(XSP(IX,NPS)-COORD(IX,IAT))
3548          CONTINUE
              DIST=1.0D+00/SQRT(DIST)
              IF(SPN .LT. 0.0D+00) DIST=-DIST
              SPN=0.0D+00
              DO 3549 IX=1,3
                CONORM(IX,NPS)=CONORM(IX,NPS)*DIST
3549          SPN=SPN+CONORM(IX,NPS)*XSP(IX,NPS)
              DL=SQRT(D2-SP*SP/D1)
              AR(NPS)=(JC-JB)*DP*HH*(1.0D+00-YX(3-ICHG))*DL/2
              VOLUME=VOLUME+AR(NPS)*SPN
3550        AREA=AREA+AR(NPS)
3600      CONTINUE
          IF (SUMPHI.GT.1.0D-10) THEN
            NIP=NIP+1
            ISUDE(1,NIP)=IA
            ISUDE(2,NIP)=IB
            ISUDE(3,NIP)=NSAB
            ISUDE(4,NIP)=IPS0
            CALL ANSUDE(RA-RSOLV,RB-RSOLV,DAB,RSOLV,
     &                AA,AB,AAR,ABR,AAD,ABD)
            SUMPHI=SUMPHI/(2*PI)
            SUDE(1,NIP)=AAR*SUMPHI
            SUDE(2,NIP)=ABR*SUMPHI
            SUDE(3,NIP)=AAD*SUMPHI
            SUDE(4,NIP)=ABD*SUMPHI
          ENDIF
3700    CONTINUE
3800  ILIPA=ILIPA+NIPA(IA)
      VOLUME=VOLUME/3.0D+00
      WRITE(6,*) "VOLUME INSIDE CONSTS:",VOLUME
      WRITE(6,*) "TOTAL AREA INSIDE CONSTS:",AREA
      IF(NPS .GT. MAXNPS) THEN
         WRITE(6,*) 'NPS .GT. MAXNPS. AFTER CLOSURE'
         WRITE(6,*) 'NPS,MAXNPS,LENAB2=',NPS,MAXNPS,LENAB2
         CALL ABRT
      END IF
C
C FILLING AAMAT
C
      DO 450 IPS=1,NPS
      IF(IPS.GT.NPSPHER) THEN
         AA=FDIAGR/SQRT(AR(IPS))
      ELSE
         I=IATSP(IPS)
         RI=SRAD(I)-RDS
         NARI=NAR(IPS)
         NSETFI=NSETF(IPS)
         AA=0.0D+00
         DO 350 K=NSETFI,NSETFI+NARI-1
            J1=NSET(K)
            AA=AA+FDIAG
            X1=DIRVEC(1,J1)
            X2=DIRVEC(2,J1)
            X3=DIRVEC(3,J1)
            DO 350 L=NSETFI,K-1
               J2=NSET(L)
               AA=AA+2.0D+00/SQRT((X1-DIRVEC(1,J2))**2+
     1             (X2-DIRVEC(2,J2))**2+(X3-DIRVEC(3,J2))**2)
  350    CONTINUE
         AA=AA/RI/NARI**2
       ENDIF
         ABCMAT(IPS+(IPS-1)*NPS)=AA
         DO 360 IX=1,3
            XI(IX)=COORD(IX,I)
  360    XA(IX)=XSP(IX,IPS)
         DO 440 JPS=IPS+1,NPS
            J=IATSP(JPS)
            DIST=0.0D+00
            DO 370 IX=1,3
               XJ(IX)=COORD(IX,J)-XI(IX)
  370       DIST=DIST+(XSP(IX,JPS)-XA(IX))**2
            IF (DIST .LT. DISEX2 .AND.
     &      IPS.LE.NPSPHER.AND.JPS.LE.NPSPHER) THEN
               NARJ=NAR(JPS)
               NSETFJ=NSETF(JPS)
               RJ=SRAD(J)-RDS
               AIJ=0.0D+00
               DO 430 K=NSETFI,NSETFI+NARI-1
                  J1=NSET(K)
                  DO 380 IX=1,3
  380             XX(IX)=DIRVEC(IX,J1)*RI
                  IF (I .NE. J) THEN
                     X1=XX(1)*TM(1,1,I)+XX(2)*TM(2,1,I)+XX(3)*TM(3,1,I)-
     1XJ(1)
                     X2=XX(1)*TM(1,2,I)+XX(2)*TM(2,2,I)+XX(3)*TM(3,2,I)-
     1XJ(2)
                     X3=XX(1)*TM(1,3,I)+XX(2)*TM(2,3,I)+XX(3)*TM(3,3,I)-
     1XJ(3)
                     DO 400 L=NSETFJ,NSETFJ+NARJ-1
                        J2=NSET(L)
                        DO 390 IX=1,3
  390                   XX(IX)=DIRVEC(IX,J2)*RJ
                        Y1=XX(1)*TM(1,1,J)+XX(2)*TM(2,1,J)+XX(3)*TM(3,1,
     1J)-X1
                        Y2=XX(1)*TM(1,2,J)+XX(2)*TM(2,2,J)+XX(3)*TM(3,2,
     1J)-X2
                        Y3=XX(1)*TM(1,3,J)+XX(2)*TM(2,3,J)+XX(3)*TM(3,3,
     1J)-X3
                        AIJ=AIJ+1.0D+00/SQRT(Y1*Y1+Y2*Y2+Y3*Y3)
  400                CONTINUE
                  ELSE
  410                DO 420 L=NSETFJ,NSETFJ+NARJ-1
                        J2=NSET(L)
                        AIJ=AIJ+((DIRVEC(1,J2)*RJ-XX(1))**2
     *                         + (DIRVEC(2,J2)*RJ-XX(2))**2
     *                         + (DIRVEC(3,J2)*RJ-XX(3))**2)**(-.5D+00)
  420                CONTINUE
                  END IF
  430          CONTINUE
               AIJ=AIJ/NARI/NARJ
            ELSE
               AIJ=1.0D+00/SQRT(DIST)
            END IF
            ABCMAT(IPS+(JPS-1)*NPS)=AIJ
            ABCMAT(JPS+(IPS-1)*NPS)=AIJ
  440    CONTINUE
  450 CONTINUE
C
C INVERT A-MATRIX
C
C
      CALL DGETRF(NPS,NPS,ABCMAT,NPS,IPIV,INFO)
      CALL DGETRI(NPS,ABCMAT,NPS,IPIV,XSP, 3*LENABC,INFO)
C
C  STORE INV. A-MATRIX AS LOWER TRIANGLE
C
C
      II=0
      DO 460 I=1,NPS
         DO 460 J=1,I
            II=II+1
            ABCMAT(II)=ABCMAT(J+(I-1)*NPS)
  460 CONTINUE
      NPS2=II
C
      IF(MPCTYP.EQ.NONE) THEN
         DO 465 I=1,NUMAT
            DO 565 J=1,3
               COORD(J,I)=TOBOHR*COORD(J,I)
  565       CONTINUE
  465    CONTINUE
      ENDIF
C
      RETURN
      END
C
C*MODULE COSMO   *DECK ANSUDE
      SUBROUTINE ANSUDE(RA,RB,D,RS,ARA,ARB,AAR,ABR,ARAD,ARBD)
C
C   THIS ROUTINE CALCULATES THE AREA OF TWO INTERSECTING SPHERES
C   WITH RADII RA AND RB AT A DISTANCE D AND A SOLVENT PROBE
C   RADIUS RS. THE TWO AREAS ARE CALCULATED SEPARATELY (ARA,ARB).
C   FOR BOTH AREAS ANALYTIC DERIVATIVES WITH RESPECT TO THE DISTANCE
C   D ARE CALCULATED (ARAD,ARBD).    (WRITTEN BY ANDREAS KLAMT, 9/9/96)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PI=2.0D+00*ASIN(1.0D+00)
      QA=RA+RS
      QB=RB+RS
      CA=(QA**2+D**2-QB**2)/(2.0D+00*QA*D)
      CB=(QB**2+D**2-QA**2)/(2.0D+00*QB*D)
      SA=SQRT(1.0D+00-CA*CA)
      SB=SQRT(1.0D+00-CB*CB)
      TA=PI*SA
      TB=PI*SB
      FZA=(1.0D+00-COS(TA))/2
      FZB=(1.0D+00-COS(TB))/2
      IF (SA.LT.0 .OR. SB.LT.0) FZA=1.0D+00
      IF (SA.LT.0 .OR. SB.LT.0) FZB=1.0D+00
      XA=FZB**1*RS*(CA+CB)
      XB=FZA**1*RS*(CA+CB)
      YA=RA*SA-FZB*RB*SB
      YB=RB*SB-FZA*RA*SA
      ZA=SQRT(XA*XA+YA*YA)
      ZB=SQRT(XB*XB+YB*YB)
      ARA=PI*RA*(2.0D+00*(1.0D+00+CA)*RA+SA*ZA)
      ARB=PI*RB*(2.0D+00*(1.0D+00+CB)*RB+SB*ZB)
      AAR=PI*RA*(SA*ZA)
      ABR=PI*RB*(SB*ZB)
C NOW DERIVATIVES
      CAD=(QB**2+D**2-QA**2)/(2.0D+00*QA*D*D)
      CBD=(QA**2+D**2-QB**2)/(2.0D+00*QB*D*D)
      SAD=-CA*CAD/SA
      SBD=-CB*CBD/SB
      TAD=PI*SAD
      TBD=PI*SBD
      FZAD=SIN(TA)*0.5D+00
      FZBD=SIN(TB)*0.5D+00
      IF (SA.LT.0 .OR. SB.LT.0) FZAD=0.0D+00
      IF (SA.LT.0 .OR. SB.LT.0) FZBD=0.0D+00
      XAD=RS*((CA+CB)*FZBD*TBD+FZB*(CAD+CBD))
      XBD=RS*((CA+CB)*FZAD*TAD+FZA*(CAD+CBD))
      YAD=RA*SAD-FZBD*TBD*RB*SB-FZB*RB*SBD
      YBD=RB*SBD-FZAD*TAD*RA*SA-FZA*RA*SAD
      ZAD=(XA*XAD+YA*YAD)/ZA
      ZBD=(XB*XBD+YB*YBD)/ZB
      ARAD=PI*RA*(SAD*ZA+SA*ZAD+2.0D+00*RA*CAD)
      ARBD=PI*RB*(SBD*ZB+SB*ZBD+2.0D+00*RB*CBD)
      END
C
C
C*MODULE COSMO   *DECK CQDEN
      SUBROUTINE CQDEN(P)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION P(*)
C
      PARAMETER (MXATM=500, MAXDEN=25*MXATM, LENABC=2000)
C
      COMMON /ATLIM / LIMLOW(MXATM),LIMSUP(MXATM)
      COMMON /CORE  / CORE(107)
      COMMON /INFOA / NATHF,ICH,MUL,NUM,NQMT,NE,NORBA,NORBB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /MOLKST/ NUMAT,NAT(MXATM),NFIRST(MXATM),NMIDLE(MXATM),
     1                NLAST(MXATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /SOLV  / FEPSI,RDS,DISEX2,COSURF(3,LENABC),SRAD(MXATM),
     *                QDEN(MAXDEN),AR(LENABC),
     *                NSPA,NPS,NPS2,NDEN,NPSPHER
C
      WRITE(6,*)'ENTERING CQDEN:'
      CALL AOLIM()
      IDEN=0
      DO 30 I=1,NUMAT
         IA=LIMLOW(I)
         IDEL=LIMSUP(I)-IA
         IM=(IA*(IA+1))/2
         IDEN=IDEN+1
         QDEN(IDEN)=CORE(INT(ZAN(I)))-P(IM)
         DO 20 IC=1,IDEL
            IM=IM+IA-1
            DO 10 ID=0,IC
               IM=IM+1
               IDEN=IDEN+1
               QDEN(IDEN)=-P(IM)
   10       CONTINUE
   20    CONTINUE
   30 CONTINUE
      RETURN
      END
C*MODULE COSMO   *DECK DECORR
      SUBROUTINE DECORR(ABCMAT,LENAB2)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500, MAXDEN=25*MXATM, LENABC=2000, NPPA=1082)
      PARAMETER (TOANGS=0.52917724924D+00)
C
      DIMENSION ABCMAT(LENAB2)
C
      COMMON /COSFRQ/ FCOORD(3,MXATM),POS0(NPPA),
     *                COSZAN0(NPPA),EDIEL0,FINDEX,ICFREQ,ICFREQ1
      COMMON /COSMO1/ SE2,SECORR,ETOTS,CDUM,QVCOSMO,
     *                CSPOT(NPPA),ICORR,ITRIPO,ITRIP2,ITRIP3,ITRIP4,
     *                NATCOS,NQS,ITERC
      COMMON /COSMO3/ COSZAN(NPPA),CORZAN(3,NPPA)
      COMMON /INFOA / NATHF,ICH,MUL,NUM,NQMT,NE,NORBA,NORBB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /SOLV  / FEPSI,RDS,DISEX2,COSURF(3,LENABC),SRAD(MXATM),
     *                QDEN(MAXDEN),AR(LENABC),
     *                NSPA,NPS,NPS2,NDEN,NPSPHER
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA NONE/4HNONE/
#else
      CHARACTER*4 :: NONE_STR
      EQUIVALENCE (NONE, NONE_STR)
      DATA NONE_STR/"NONE"/
#endif
C
#if defined(SPEC_CPU)
      WRITE(6,'(A,I8,I8,I8)')'ENTERING DECORR (NATHF,NPS,NPS2):',NATHF,
     * NPS,NPS2
#else
      WRITE(6,*)'ENTERING DECORR (NATHF,NPS,NPS2):',NATHF,NPS,NPS2
#endif
C
      CALL AOLIM()
C
C     CALCULATE THE SCREENING CHARGES TO FEED BACK INTO
C     THE QUANTUM MECHANICS AS POINT CHARGES:
C               ZAN(NPS)=-FEPSI*A'BQ
C     WITH CORRESPONDING COORDINATES GIVEN IN COSURF
C
      IF(MPCTYP.EQ.NONE) THEN
      WRITE(6,*)'CALCULATING SCREENING CHARGES TO FEED BACK INTO GAMESS'
      DO 135 I=1,NPS
C         ZAN(I+NATHF)=0.0D+00
         COSZAN(I)=0.0D+00
         CSPOT(I)=0.0D+00
  135 CONTINUE
      I0=NPS2-NDEN
      DO 170 I=1,NPS
         I2=(I*(I-1))/2
         I1=I0+I*NDEN
         POSI=0.0D+00
         DO 140 J=1,NDEN
            POSI=POSI+QDEN(J)*ABCMAT(J+I1)
CDEBUG
C       IF(I.LT.10) THEN
C       WRITE(78,*)"QDEN(J),ABCMAT(J+I1):",J,J+I1,QDEN(J),ABCMAT(J+I1)
C       ENDIF
CDEBUG
  140    CONTINUE
         CSPOT(I)=POSI
CFREQ
         IF(ICFREQ.EQ.3) POSI=POSI-POS0(I)
CFREQ
CFREQ  FOR SLIGHTLY PERTURBED GEOMETRY IN NUMERICAL FREQUENCY RUN,
CFREQ  THE FOLLOWING WILL THEN ONLY BE THE DQ TERM.
CFREQ
         DO 150 K=1,I
C            ZAN(NATHF+K)=ZAN(NATHF+K)+POSI*ABCMAT(K+I2)
            COSZAN(K)=COSZAN(K)+POSI*ABCMAT(K+I2)
  150    CONTINUE
         DO 160 K=I+1,NPS
C            ZAN(NATHF+K)=ZAN(NATHF+K)+POSI*ABCMAT(I+(K*(K-1))/2)
            COSZAN(K)=COSZAN(K)+POSI*ABCMAT(I+(K*(K-1))/2)
  160    CONTINUE
  170 CONTINUE
C
C      WRITE(86,*)'COSZAN (SCREENING) CHARGES GOING BACK INTO GAMESS:'
C
      ZSUM=0.0D+00
      ZSUM2=0.0D+00
      ZSUM3=0.0D+00
          WRITE(6,*)"THERE ARE THIS MANY COSZANS:",NPS
C          WRITE(86,*)"ZAN'S FOR THIS ITERATION:"
      DO 888 I=1,NPS
         ZSUM2 = ZSUM2 + COSZAN(I)
         ZSUM3 = ZSUM3 + COSZAN(I)*COSZAN(I)
 888  CONTINUE
CFREQ
CFREQ  IF EQUILIBRIUM GEOMETRY, CALCULATE Q=F(E)*Q(*), ELSE, IF
CFREQ  PERTURBED GEOMETRY ON FREQUENCY RUN, ADD IN INITIAL Q'S
CFREQ  TO DQ'S
CFREQ
         IF(ICFREQ.EQ.1) THEN
            WRITE(6,*)"WRITING A COPY OF GRD STATE CHARGES:"
            DO 889 I=1,NPS
               COSZAN0(I)=COSZAN(I)
 889        CONTINUE
         ENDIF
         IF(ICFREQ.EQ.3) THEN
            WRITE(6,*)"CALCULATING DQ'S:"
            DO 890 I=1,NPS
               COSZAN(I)=COSZAN0(I)-FINDEX*COSZAN(I)
 890        CONTINUE
         ELSE
            WRITE(6,*)"CALCULATING FINAL CHARGES, Q:"
            DO 891 I=1,NPS
               COSZAN(I)=-FEPSI*COSZAN(I)
 891        CONTINUE
         ENDIF
CFREQ
         DO 892 I=1,NPS
            ZSUM = ZSUM + COSZAN(I)
  892    CONTINUE
C
      WRITE(6,*)'***********************************************'
      WRITE(6,*)'SUM OF SCREENING CHARGES:',ZSUM2
      WRITE(6,*)'SUM OF SQUARED SCREENING CHARGES:',ZSUM3
      WRITE(6,*)'SUM OF SCALED SCREENING CHARGES:',ZSUM
C
C     NOW CALCULATE THE CORRECTION TO THE TOTAL ENERGY:
C            QBQ
C     ENERGY GAIN OF SYSTEM DUE TO THE INTERACTION OF THE
C     SCREENING CHARGES WITH THE MOLECULE.
C
      I0=NPS2-NDEN
      SE=0.0D+00
      DO 171 I=1,NPS
         I1=I0+I*NDEN
         SEP=0.0D+00
         DO 172 J1=1,NDEN
            SEP = SEP + QDEN(J1)*ABCMAT(J1+I1)
  172    CONTINUE
C
C  NOW SEP IS THE POTENTIAL ON SEGMENT I AND IT HAS TO BE
C  MULTIPLIED BY THE C SCREENING CHARGE ON THIS SEGMENT:
C   (RECALL:  ZAN=Q=-FEPSI*A'*B*Q
C         QBQ = ENERGY GAIN OF SYSTEM DUE TO INTERACTION OF
C               SCREENING CHARGES WITH MOLECULE (NEGATIVE)
C
            SE = SE + SEP*COSZAN(I)
  171 CONTINUE
C
C     -0.5*QBQ  =  -0.5QV
C  POLARIZATION CORRECTION (POSITIVE)
C  UNITS OF SE :  E/ANGS  SO, SECORR MUST HAVE THE TOANGS
C
CFREQ
CFREQ  SAVE OFF A COPY OF GROUND STATE EDIEL0 FOR FREQUENCY RUN
CFREQ
      SE2 = 0.5D+00*SE
      IF(ICFREQ.EQ.1) THEN
         EDIEL0=SE2
         ICFREQ1=1
      ENDIF
C
      WRITE(6,*)'SCREENING ENERGY, SE2 = -0.5QBQ:',SE2
      SECORR = TOANGS*SE2
C      SECORR=SE2
      WRITE(6,*)'QBQ=QV (SE, SHOULD BE NEGATIVE)  = ',SE
      QVCOSMO=SE*0.5292D+00
C      QVCOSMO=SE
      WRITE(6,*)"QV (COSMO) IN A.U. :",QVCOSMO
      WRITE(6,*)'SCREENING ENERGY,SE2 IN A.U. (-.5*QBQ(=QV)):',SECORR
C
C     ALSO, COULD CALCULATE THE DIELECTRIC ENERGY USING DIELEN (QCQ)
C     (SHOULD BE EQUAL TO SECORR, ABOVE, BUT OPPOSITE IN SIGN)
C         - KIM
C
      CALL DIELEN(EDIE,P,ABCMAT,LENAB2)
C      SECORR2=(EDIE*23.06D+00)/627.517D+00
      WRITE(6,*)'EDIE (HARTREE) FROM DIELEN:',EDIE
C
C     THE TOTAL ENERGY CORRECTION SHOULD NOW BE -QV' + .5QV
C     WHERE -QV' IS FROM GAMESS, AND .5QV IS SECORR.
C       NATHF GIVES GAMESS THE TOTAL NUMBER OF CHARGES IT NOW NEEDS TO
C       CONSIDER, WITH THE NEW SURFACE CHARGES, PLUS THE ATOMIC CHARGES.
C
       ENDIF
C
      RETURN
      END
C*MODULE COSMO   *DECK DIELEN
      SUBROUTINE DIELEN (EDIE,P,ABCMAT,LENAB2)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION ABCMAT(LENAB2)
C
      PARAMETER (MXATM=500, MAXDEN=25*MXATM, LENABC=2000)
C
      COMMON /SOLV  / FEPSI,RDS,DISEX2,COSURF(3,LENABC),SRAD(MXATM),
     *                QDEN(MAXDEN),AR(LENABC),
     *                NSPA,NPS,NPS2,NDEN,NPSPHER
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA NONE/4HNONE/
#else
      CHARACTER*4 :: NONE_STR
      EQUIVALENCE (NONE, NONE_STR)
      DATA NONE_STR/"NONE"/
#endif
C
      WRITE(6,*) 'ENTERING DIELEN:'
      IF(MPCTYP.NE.NONE) CALL CQDEN(P)
      EDIE=0.0D+00
      I0=NPS2+NDEN*NPS
      DO 20 I=1,NDEN
         QI=QDEN(I)
         DO 10 J=1,I-1
            I0=I0+1
            EDIE=EDIE+2*QI*ABCMAT(I0)*QDEN(J)
   10    CONTINUE
         I0=I0+1
         EDIE=EDIE+QI*ABCMAT(I0)*QI
   20 CONTINUE
C
      RETURN
      END
C*MODULE COSMO   *DECK DVFILL
      SUBROUTINE DVFILL(NPPA,DIRVEC)
C          FUELLEN DES FELDES DIRVEC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DIRVEC(3,*)
      INTEGER FSET(3,20), KSET(2,30)
      DATA KSET/ 1, 2, 1, 3, 1, 4, 1, 5, 1, 6,
     1            12,11,12,10,12, 9,12, 8,12, 7,
     2             2, 3, 3, 4, 4, 5, 5, 6, 6, 2,
     3             7, 8, 8, 9, 9,10,10,11,11, 7,
     4             2,7,7,3,3,8,8,4,4,9,9,5,5,10,10,6,6,11,11,2/
      DATA FSET/ 1, 2, 3, 1, 3, 4, 1, 4, 5, 1, 5, 6, 1, 6, 2,
     1            12,11,10,12,10, 9,12, 9, 8,12, 8, 7,12, 7,11,
     2             2, 3, 7, 3, 4, 8, 4, 5, 9, 5, 6,10, 6, 2,11,
     3             7, 8, 3, 8, 9, 4, 9,10, 5,10,11, 6,11, 7, 2/
C
C
      DIRVEC (1,1) =  -1.0D+00
      DIRVEC (2,1) =   0.0D+00
      DIRVEC (3,1) =   0.0D+00
      ND=1
      R=SQRT(0.8D+00)
      H=SQRT(0.2D+00)
      DO 10 I= -1,1,2
         DO 10 J= 1,5
            ND=ND+1
            BETA=1.0D+00+ J*1.25663706D+00 + (I+1)*0.3141593D+00
            DIRVEC(2,ND)=R*COS(BETA)
            DIRVEC(3,ND)=R*SIN(BETA)
            DIRVEC(1,ND)=I*H
   10 CONTINUE
      DIRVEC (2,12) =  0.0D+00
      DIRVEC (3,12) =  0.0D+00
      DIRVEC (1,12) =  1.0D+00
      ND=12
C  NPPA=10*3**K*4**L+2
      M=(NPPA-2)/10
      DO 20 K=0,10
         IF ((M/3)*3 .NE. M) GO TO 30
   20 M=M/3
   30 DO 40 L=0,10
         IF ((M/4)*4 .NE. M) GO TO 50
   40 M=M/4
   50 CONTINUE
C
      IF (10*3**K*4**L+2 .NE. NPPA) THEN
         WRITE(6,9020) K,L,NPPA
         CALL ABRT
      END IF
 9020 FORMAT(1X,'VALUE OF NPPA NOT ALLOWED: IT MUST BE 10*3**K*4**L+2'/
     *       1X,'K,L,NPPA=',3I10)
C
      KH=K/2
      M=2**L*3**KH
C CREATE ON EACH EDGE 2**L*3**KH-1 NEW POINTS
      DO 70 I=1,30
         NA=KSET(1,I)
         NB=KSET(2,I)
         DO 70 J=1,M-1
            ND=ND+1
            DO 60 IX=1,3
   60       DIRVEC(IX,ND)=DIRVEC(IX,NA)*(M-J)+DIRVEC(IX,NB)*J
   70 CONTINUE
C CREATE POINTS WITHIN EACH TRIANGLE
      DO 90 I=1,20
         NA=FSET(1,I)
         NB=FSET(2,I)
         NC=FSET(3,I)
         DO 90 J1=1,M-1
            DO 90 J2=1,M-J1-1
               ND=ND+1
               DO 80 IX=1,3
   80          DIRVEC(IX,ND)=DIRVEC(IX,NA)*(M-J1-J2)
     1                     +DIRVEC(IX,NB)*J1+DIRVEC(IX,NC)*J2
   90 CONTINUE
      IF (K .EQ. 2*KH) GO TO 140
C CREATE TO ADDITIONAL SUBGRIDS
      T=1.0D+00/3.0D+00
      DO 110 I=1,20
         NA=FSET(1,I)
         NB=FSET(2,I)
         NC=FSET(3,I)
         DO 110 J1=0,M-1
            DO 110 J2=0,M-J1-1
               ND=ND+1
               DO 100 IX=1,3
  100          DIRVEC(IX,ND)=DIRVEC(IX,NA)*(M-J1-J2-2*T)
     1                 +DIRVEC(IX,NB)*(J1+T)+DIRVEC(IX,NC)*(J2+T)
  110 CONTINUE
      T=2.0D+00/3.0D+00
      DO 130 I=1,20
         NA=FSET(1,I)
         NB=FSET(2,I)
         NC=FSET(3,I)
         DO 130 J1=0,M-2
            DO 130 J2=0,M-J1-2
               ND=ND+1
               DO 120 IX=1,3
  120          DIRVEC(IX,ND)=DIRVEC(IX,NA)*(M-J1-J2-2*T)
     1                  +DIRVEC(IX,NB)*(J1+T)+DIRVEC(IX,NC)*(J2+T)
  130 CONTINUE
C NORMALIZE ALL VECTORS
  140 DO 170 I=1,NPPA
         DIST=0.0D+00
         DO 150 IX=1,3
  150    DIST=DIST+DIRVEC(IX,I)**2
         DIST=1.0D+00/SQRT(DIST)
         DO 160 IX=1,3
  160    DIRVEC(IX,I)=DIRVEC(IX,I)*DIST
  170 CONTINUE
      RETURN
      END
C
C*MODULE COSMO   *DECK SVINIT
      SUBROUTINE SVINIT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500, MAXDEN=25*MXATM, LENABC=2000, NPPA=1082)
C
      DIMENSION RVDW(53), USEVDW(53)
      DIMENSION DIRVEC1(6,91),DIRVEC2(6,91),DIRVEC3(6,91),
     *          DIRVEC4(6,91),DIRVEC5(6,91),DIRVEC6(6,86)
      DIMENSION CCORE(107)
C
      COMMON /CORE  / CORE(107)
      COMMON /COSDIR/ DIRVEC(3,NPPA),DIRSM(3,NPPA),DIRSMH(3,NPPA/3)
      COMMON /INFOA / NATHF,ICH,MUL,NUM,NQMT,NE,NORBA,NORBB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MOLKST/ NUMAT,NAT(MXATM),NFIRST(MXATM),NMIDLE(MXATM),
     1                NLAST(MXATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /SOLV  / FEPSI,RDS,DISEX2,COSURF(3,LENABC),SRAD(MXATM),
     *                QDEN(MAXDEN),AR(LENABC),
     *                NSPA,NPS,NPS2,NDEN,NPSPHER
      COMMON /SOLVI / IATSP(LENABC+1),N0(2),NP1,NP2,ISUPSKIP
      COMMON /SOLV1 / EPSI, RSOLV, DELSC, DISEX, ITRIP
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA NONE/4HNONE/
#else
      CHARACTER*4 :: NONE_STR
      EQUIVALENCE (NONE, NONE_STR)
      DATA NONE_STR/"NONE"/
#endif
C
C     THE FOLLOWING RVDW VALUES ARE IN ACCORD WITH J. EMSLEY ELEMENTS.
C        H    WAS 1.08, CHANGED TO 1.20
C        B    WAS 999.0D+00, CHANGED TO 2.08
C        C    WAS 1.53, CHANGED TO 1.85
C        N    WAS 1.48, CHANGED TO 1.54
C        O    WAS 1.36, CHANGED TO 1.40
C        P    WAS 1.75, CHANGED TO 1.90
C        S    WAS 1.70, CHANGED TO 1.85
C        K    WAS 2.8,  CHANGED TO 2.31
C       NA    WAS 2.3,  CHANGED TO 2.31
C       CA    WAS 2.75, CHANGED TO 1.74
C       CL    WAS 1.65, CHANGED TO 1.81
C       BR    WAS 1.8,  CHANGED TO 1.95
C       RB    WAS 999,  CHANGED TO 2.44
C       NE    WAS 999,  CHANGED TO 1.50
C
C    THESE ARE THE OPTIMIZED RADII OF KLAMT AND JONAS, APPROPRIATE
C    FOR MP2 CALCULATIONS, AND IN PARTICULAR, FOR SUBSEQUENT COSMO-RS
C    THE 15% FACTOR HAS ALREADY BEEN ACCOMMODATED FOR; THUS, IF ONE OF
C    THE ABOVE IS USED, THEY MUST BE CHANGED BY 15%.
C
C        H  1.30    F  1.72
C        C  2.00   BR  2.16
C        N  1.83    I  2.32
C        O  1.72    S  2.16
C       CL  2.05
C
      DATA RVDW   /1.30D+00, 1.0D+00, 1.80D+00, 999.0D+00, 2.08D+00,
     1 2.00D+00,   1.83D+00,  1.72D+00,  1.72D+00,  1.50D+00,
     2 2.31D+00,   999.0D+00, 2.05D+00,  2.10D+00,  1.90D+00,
     3 2.16D+00,   2.05D+00,  999.0D+00, 2.31D+00,  1.74D+00,
     4 999.0D+00,  999.0D+00, 999.0D+00, 999.0D+00, 999.0D+00,
     5 999.0D+00,  999.0D+00, 999.0D+00, 999.0D+00, 1.39D+00,
     6 999.0D+00,  999.0D+00, 999.0D+00, 999.0D+00, 2.160D+00,
     7 999.0D+00,  2.44D+00, 999.0D+00, 999.0D+00, 999.0D+00,
     8 999.0D+00,  999.0D+00, 999.0D+00, 999.0D+00, 999.0D+00,
     9 999.0D+00,  999.0D+00, 999.0D+00, 999.0D+00, 999.0D+00,
     * 999.0D+00,  999.0D+00, 2.32D+00 /
C
C   CORE IS THE CHARGE ON THE ATOM AS SEEN BY THE ELECTRONS
C
      DATA CCORE/1.0D+00,0.0D+00,
     1 1.0D+00,2.0D+00,3.0D+00,4.0D+00,5.0D+00,6.0D+00,7.0D+00,0.0D+00,
     2 1.0D+00,2.0D+00,3.0D+00,4.0D+00,5.0D+00,6.0D+00,7.0D+00,0.0D+00,
     3 1.0D+00,2.0D+00,3.0D+00,4.0D+00,5.0D+00,6.0D+00,7.0D+00,8.0D+00,
     *9.0D+00,10.0D+00,11.0D+00,2.0D+00,
     4 3.0D+00,4.0D+00,5.0D+00,6.0D+00,7.0D+00,0.0D+00,
     5 1.0D+00,2.0D+00,3.0D+00,4.0D+00,5.0D+00,6.0D+00,7.0D+00,8.0D+00,
     *9.0D+00,10.0D+00,11.0D+00,2.0D+00,
     6 3.0D+00,4.0D+00,5.0D+00,6.0D+00,7.0D+00,0.0D+00,
     7 1.0D+00,2.0D+00,3.0D+00,4.0D+00,5.0D+00,6.0D+00,7.0D+00,8.0D+00,
     *9.0D+00,10.0D+00,
     8 11.0D+00,12.0D+00,13.0D+00,14.0D+00,15.0D+00,16.0D+00,
     9 3.0D+00,4.0D+00,5.0D+00,6.0D+00,7.0D+00,8.0D+00,9.0D+00,10.0D+00,
     *11.0D+00,2.0D+00,
     1 3.0D+00,4.0D+00,5.0D+00,6.0D+00,7.0D+00,0.0D+00,
     2  15*0.0D+00,1.0D+00,2.0D+00,1.0D+00,-2.0D+00,-1.0D+00,0.0D+00/
C
C    DIRVEC1 - DIRVEC6 CONTAIN THE BASIC GRID OF 1082 POINTS, 2 POINTS
C    IN A LINE, SPLITTED INTO 6 BLOCKS AND THEN COMBINED TOGETHER
C    INTO DIRVEC
C
      DATA DIRVEC1/
     1-0.9549128, 0.2968866, 0.0000845,-0.5933439,-0.4098201, 0.6928135,
     1-0.6722710,-0.6644925,-0.3263454,-0.4107225, 0.1818896,-0.8934334,
     1-0.1706614, 0.9588254,-0.2269988,-0.2833310, 0.5939016, 0.7529970,
     1 0.1705895,-0.9588512, 0.2269441, 0.2833210,-0.5939103,-0.7529939,
     1 0.5932725, 0.4096061,-0.6930012, 0.6725665, 0.6645031, 0.3257145,
     1 0.4108047,-0.1817824, 0.8934174, 0.9544481,-0.2983769,-0.0000335,
     1-0.9731867, 0.1833445, 0.1388970,-0.9594079, 0.0605210, 0.2754517,
     1-0.9112807,-0.0671421, 0.4062752,-0.8308647,-0.1913900, 0.5225262,
     1-0.7238775,-0.3069138, 0.6179040,-0.9889957, 0.1325515,-0.0657088,
     1-0.9908190,-0.0406643,-0.1289343,-0.9575314,-0.2163757,-0.1905652,
     1-0.8903039,-0.3839001,-0.2449075,-0.7936914,-0.5341944,-0.2910160,
     1-0.9360858, 0.3024369,-0.1796534,-0.8862845, 0.2955620,-0.3565711,
     1-0.8038640, 0.2793915,-0.5251124,-0.6931842, 0.2540032,-0.6745206,
     1-0.5595423, 0.2220041,-0.7985153,-0.8873858, 0.4587911,-0.0453561,
     1-0.7895660, 0.6068834,-0.0909845,-0.6618342, 0.7375955,-0.1338965,
     1-0.5111884, 0.8421238,-0.1717961,-0.3449932, 0.9164901,-0.2025478,
     1-0.9105044, 0.3846924, 0.1516364,-0.8349955, 0.4606528, 0.3009676,
     1-0.7285126, 0.5223324, 0.4432135,-0.5964952, 0.5660933, 0.5689743,
     1-0.4463721, 0.5894714, 0.6732573, 0.9363571,-0.3015735, 0.1796909,
     1 0.8863572,-0.2956027, 0.3563566, 0.8038090,-0.2795972, 0.5250872,
     1 0.6927529,-0.2544780, 0.6747847, 0.5599299,-0.2215787, 0.7983617,
     1 0.9890676,-0.1324206, 0.0648846, 0.9907776, 0.0406252, 0.1292646,
     1 0.9575454, 0.2162091, 0.1906840, 0.8902337, 0.3837378, 0.2454165,
     1 0.7937654, 0.5340719, 0.2910391, 0.9731045,-0.1835404,-0.1392138,
     1 0.9594534,-0.0602414,-0.2753545, 0.9113671, 0.0673543,-0.4060462,
     1 0.8310719, 0.1917861,-0.5220513, 0.7235033, 0.3068322,-0.6183826,
     1 0.9102897,-0.3852772,-0.1514403, 0.8350577,-0.4603362,-0.3012793,
     1 0.7286475,-0.5220658,-0.4433059, 0.5968371,-0.5656973,-0.5690098,
     1 0.4460175,-0.5899102,-0.6731080, 0.8874851,-0.4585396, 0.0459530,
     1 0.7895767,-0.6068918, 0.0908350, 0.6617946,-0.7376738, 0.1336608,
     1 0.5110989,-0.8422313, 0.1715353, 0.3450622,-0.9163667, 0.2029881,
     1-0.6654642,-0.4996399, 0.5545425,-0.7174943,-0.5741164, 0.3944520,
     1-0.7463700,-0.6293610, 0.2164175,-0.7477935,-0.6632078, 0.0309880,
     1-0.7215874,-0.6753013,-0.1525771,-0.6834518,-0.5580055,-0.4706627,
     1-0.6732778,-0.4297482,-0.6016756,-0.6395519,-0.2833600,-0.7146192,
     1-0.5817989,-0.1274818,-0.8032798,-0.5035544, 0.0295540,-0.8634579,
     1-0.4017481, 0.3549231,-0.8441730,-0.3796456, 0.5195989,-0.7654320,
     1-0.3440344, 0.6696839,-0.6581518,-0.2951506, 0.7963461,-0.5279384,
     1-0.2363701, 0.8933919,-0.3820734,-0.2101446, 0.9762984,-0.0517755,
     1-0.2430065, 0.9614184, 0.1289279,-0.2686024, 0.9124099, 0.3088055,
     1-0.2843787, 0.8312736, 0.4776117,-0.2890854, 0.7231429, 0.6272909,
     1-0.3727281, 0.4489030, 0.8121329,-0.4515334, 0.2858324, 0.8452322,
     1-0.5176209, 0.1093139, 0.8485983,-0.5644111,-0.0709828, 0.8224363,
     1-0.5892457,-0.2467640, 0.7693485, 0.2100139,-0.9763302, 0.0517068,
     1 0.2431465,-0.9613764,-0.1289778, 0.2684190,-0.9124912,-0.3087250,
     1 0.2841798,-0.8312739,-0.4777296, 0.2893515,-0.7230977,-0.6272205,
     1 0.3726965,-0.4487874,-0.8122113, 0.4518120,-0.2857635,-0.8451066,
     1 0.5174839,-0.1094706,-0.8486616, 0.5640766, 0.0711694,-0.8226497,
     1 0.5895644, 0.2465207,-0.7691822, 0.6653011, 0.4999599,-0.5544497,
     1 0.7178810, 0.5737404,-0.3942954, 0.7465268, 0.6291352,-0.2165333,
     1 0.7473791, 0.6636785,-0.0309074, 0.7218015, 0.6750505, 0.1526743,
     1 0.6831334, 0.5583200, 0.4707521, 0.6735693, 0.4294047, 0.6015946,
     1 0.6397344, 0.2834281, 0.7144287, 0.5814895, 0.1275627, 0.8034910,
     1 0.5035744,-0.0297182, 0.8634406, 0.4014164,-0.3549110, 0.8443359,
     1 0.3799675,-0.5196275, 0.7652529, 0.3440235,-0.6695626, 0.6582809,
     1 0.2949414,-0.7964324, 0.5279252, 0.2364766,-0.8933981, 0.3819931,
     1-0.4956735,-0.5587936, 0.6648741,-0.3797989,-0.6923149, 0.6135576,
     1-0.2482284,-0.8043110, 0.5398763,-0.1075894,-0.8875955, 0.4478825,
     1 0.0335857,-0.9390086, 0.3422496, 0.0175244,-0.9903079, 0.1377794,
     1-0.1398316,-0.9892915, 0.0418273,-0.2953684,-0.9536694,-0.0572033,
     1-0.4407831,-0.8843516,-0.1537287,-0.5677580,-0.7862441,-0.2438670,
     1-0.5436616,-0.7132390,-0.4424050,-0.3943455,-0.7391990,-0.5459638,
     1-0.2280544,-0.7396332,-0.6331934,-0.0537536,-0.7141447,-0.6979311,
     1 0.1185313,-0.6645761,-0.7377594, 0.1704139,-0.4944237,-0.8523522,
     1 0.0492978,-0.3759856,-0.9253132,-0.0755360,-0.2422231,-0.9672757,
     1-0.1982524,-0.1005289,-0.9749820,-0.3116206, 0.0423808,-0.9492610,
     1-0.2473704, 0.2444008,-0.9375906,-0.0733380, 0.2999045,-0.9511461,
     1 0.1077646, 0.3465816,-0.9318090, 0.2849372, 0.3803860,-0.8798393,
     1 0.4484887, 0.4011797,-0.7986944, 0.4956347, 0.5589575,-0.6647652,
     1 0.3794536, 0.6924846,-0.6135797, 0.2481214, 0.8043696,-0.5398382,
     1 0.1079680, 0.8875407,-0.4479001,-0.0338894, 0.9389621,-0.3423474,
     1-0.0172523, 0.9902921,-0.1379272, 0.1394389, 0.9893469,-0.0418261,
     1 0.2954378, 0.9536448, 0.0572544, 0.4410567, 0.8842189, 0.1537073,
     1 0.5680266, 0.7859774, 0.2441009, 0.5433577, 0.7132517, 0.4427578,
     1 0.3940129, 0.7393723, 0.5459693, 0.2279888, 0.7396393, 0.6332100,
     1 0.0542259, 0.7140666, 0.6979745,-0.1187760, 0.6647547, 0.7375592,
     1-0.1701688, 0.4945379, 0.8523350,-0.0495612, 0.3760926, 0.9252557,
     1 0.0755993, 0.2422371, 0.9672672, 0.1984776, 0.1003999, 0.9749495,
     1 0.3114313,-0.0425023, 0.9493177, 0.2475098,-0.2446822, 0.9374804,
     1 0.0728446,-0.2997593, 0.9512297,-0.1077612,-0.3465725, 0.9318128,
     1-0.2844546,-0.3806319, 0.8798892,-0.4485129,-0.4008295, 0.7988566,
     1-0.9964999,-0.0171828, 0.0818091,-0.9821083,-0.1878395, 0.0134030,
     1-0.9312386,-0.3616564,-0.0447146,-0.8486218,-0.5214485,-0.0890648,
     1-0.9660822,-0.1373179, 0.2186983,-0.9329488,-0.3256513, 0.1534857,
     1-0.8583891,-0.5041852, 0.0946862,-0.9012400,-0.2650112, 0.3428345,
     1-0.8436419,-0.4581531, 0.2799360,-0.8071265,-0.3882273, 0.4447767,
     1-0.9558104, 0.1140135,-0.2709748,-0.8903083, 0.1084107,-0.4422649,
     1-0.7916318, 0.0880333,-0.6046233,-0.6711178, 0.0533712,-0.7394270,
     1-0.9424440,-0.0616081,-0.3286394,-0.8567618,-0.0785782,-0.5096907,
     1-0.7349961,-0.1050501,-0.6698845,-0.8918103,-0.2350041,-0.3865844,
     1-0.7822948,-0.2591332,-0.5664493,-0.8079003,-0.3904050,-0.4414533/
C
      DATA DIRVEC2/
     1-0.8434783, 0.4754578,-0.2499686,-0.7325564, 0.6164954,-0.2886080,
     1-0.5906964, 0.7365880,-0.3294176,-0.4331699, 0.8224785,-0.3686366,
     1-0.7806524, 0.4598317,-0.4232454,-0.6460507, 0.6013779,-0.4700671,
     1-0.4814874, 0.7132786,-0.5093168,-0.6832461, 0.4383519,-0.5839712,
     1-0.5248594, 0.5719623,-0.6303822,-0.5589495, 0.4144066,-0.7182218,
     1-0.8148219, 0.5677254, 0.1172739,-0.7275305, 0.6324355, 0.2659413,
     1-0.6065881, 0.6857973, 0.4021602,-0.4641168, 0.7227237, 0.5121191,
     1-0.7039975, 0.7068850, 0.0685653,-0.5921852, 0.7750033, 0.2206504,
     1-0.4483906, 0.8199033, 0.3559558,-0.5632027, 0.8258832, 0.0268286,
     1-0.4274190, 0.8862988, 0.1782902,-0.4043136, 0.9146185,-0.0018946,
     1-0.9095711, 0.2626435, 0.3220229,-0.8822993, 0.1337435, 0.4512878,
     1-0.8173462, 0.0063221, 0.5761121,-0.7212291,-0.1082184, 0.6841910,
     1-0.8192927, 0.3358243, 0.4647382,-0.7700755, 0.2007307, 0.6055501,
     1-0.6816824, 0.0669558, 0.7285781,-0.6984264, 0.3894608, 0.6004339,
     1-0.6248473, 0.2488506, 0.7400265,-0.5576993, 0.4179829, 0.7171205,
     1 0.9558438,-0.1142498, 0.2707574, 0.9427519, 0.0613425, 0.3278049,
     1 0.8917542, 0.2348394, 0.3868138, 0.8077012, 0.3909478, 0.4413373,
     1 0.8898509,-0.1083842, 0.4431909, 0.8568876, 0.0787021, 0.5094601,
     1 0.7822452, 0.2591071, 0.5665297, 0.7921429,-0.0876291, 0.6040123,
     1 0.7348405, 0.1048548, 0.6700857, 0.6713079,-0.0532944, 0.7392600,
     1 0.9965465, 0.0163909,-0.0814025, 0.9662231, 0.1371702,-0.2181679,
     1 0.9009636, 0.2651190,-0.3434771, 0.8067362, 0.3884701,-0.4452725,
     1 0.9819711, 0.1885298,-0.0137610, 0.9331406, 0.3248620,-0.1539913,
     1 0.8436179, 0.4583969,-0.2796090, 0.9312192, 0.3617043, 0.0447308,
     1 0.8582835, 0.5043968,-0.0945160, 0.8492109, 0.5205982, 0.0884220,
     1 0.9096777,-0.2623310,-0.3219765, 0.8197992,-0.3354457,-0.4641179,
     1 0.6978742,-0.3898748,-0.6008071, 0.5576756,-0.4180790,-0.7170829,
     1 0.8819888,-0.1336517,-0.4519214, 0.7701002,-0.2008191,-0.6054894,
     1 0.6249657,-0.2486940,-0.7399791, 0.8173788,-0.0062430,-0.5760668,
     1 0.6817714,-0.0667776,-0.7285111, 0.7213536, 0.1077271,-0.6841373,
     1 0.8144228,-0.5682023,-0.1177354, 0.7046165,-0.7063026,-0.0682069,
     1 0.5629121,-0.8260856,-0.0266941, 0.4043089,-0.9146204, 0.0019603,
     1 0.7270526,-0.6328985,-0.2661465, 0.5918875,-0.7751623,-0.2208903,
     1 0.4277003,-0.8861530,-0.1783406, 0.6067490,-0.6857559,-0.4019881,
     1 0.4483205,-0.8198606,-0.3561424, 0.4641127,-0.7227647,-0.5120651,
     1 0.8432367,-0.4757359, 0.2502543, 0.7811665,-0.4593715, 0.4227963,
     1 0.6833248,-0.4381665, 0.5840182, 0.5589473,-0.4142341, 0.7183231,
     1 0.7320875,-0.6168533, 0.2890326, 0.6457488,-0.6015810, 0.4702220,
     1 0.5249079,-0.5720891, 0.6302268, 0.5910709,-0.7362917, 0.3294083,
     1 0.4814046,-0.7133460, 0.5093005, 0.4330615,-0.8225131, 0.3686868,
     1-0.5550078,-0.6680243, 0.4956913,-0.4299054,-0.7895351, 0.4379677,
     1-0.2917484,-0.8884247, 0.3543789,-0.1556470,-0.9552607, 0.2514975,
     1-0.6006290,-0.7292251, 0.3278346,-0.4615565,-0.8515426, 0.2486781,
     1-0.3075115,-0.9397283, 0.1494901,-0.6162222,-0.7744035, 0.1434204,
     1-0.4614584,-0.8857251, 0.0504688,-0.5986786,-0.8001156,-0.0374022,
     1-0.5299415,-0.5883169,-0.6107743,-0.3725140,-0.6059528,-0.7028901,
     1-0.2001683,-0.5936251,-0.7794498,-0.0307072,-0.5512836,-0.8337527,
     1-0.5137539,-0.4496919,-0.7306395,-0.3369255,-0.4504695,-0.8267759,
     1-0.1475652,-0.4228623,-0.8940984,-0.4678917,-0.2963586,-0.8326158,
     1-0.2747462,-0.2825049,-0.9190786,-0.3958397,-0.1446245,-0.9068598,
     1-0.2123257, 0.4409248,-0.8720683,-0.0346689, 0.4870850,-0.8726662,
     1 0.1471176, 0.5274731,-0.8367368, 0.3131846, 0.5596834,-0.7672483,
     1-0.1891490, 0.5978474,-0.7789744, 0.0043145, 0.6501882,-0.7597609,
     1 0.1950876, 0.6847526,-0.7021785,-0.1468029, 0.7382810,-0.6583237,
     1 0.0559203, 0.7842606,-0.6179064,-0.0882652, 0.8478239,-0.5228804,
     1-0.0412569, 0.9966351, 0.0708258, 0.1170957, 0.9797205, 0.1625923,
     1 0.2707903, 0.9263818, 0.2617048, 0.4015514, 0.8425035, 0.3590881,
     1-0.0753819, 0.9655732, 0.2489698, 0.0912704, 0.9297834, 0.3566126,
     1 0.2471753, 0.8532371, 0.4592284,-0.0967572, 0.8995406, 0.4259869,
     1 0.0734093, 0.8405176, 0.5367879,-0.1012803, 0.8056751, 0.5836351,
     1-0.2522998, 0.3120423, 0.9159555,-0.1267962, 0.1906638, 0.9734321,
     1-0.0005211, 0.0511850, 0.9986891, 0.1114811,-0.0936282, 0.9893461,
     1-0.3294583, 0.1456940, 0.9328615,-0.1968151, 0.0016667, 0.9804392,
     1-0.0636062,-0.1506883, 0.9865330,-0.3872668,-0.0356024, 0.9212800,
     1-0.2463884,-0.1910624, 0.9501515,-0.4166761,-0.2124974, 0.8838699,
     1 0.0412200,-0.9966416,-0.0707568,-0.1169479,-0.9797386,-0.1625896,
     1-0.2705089,-0.9264474,-0.2617635,-0.4014435,-0.8425413,-0.3591201,
     1 0.0757656,-0.9655604,-0.2489031,-0.0913071,-0.9298086,-0.3565375,
     1-0.2473414,-0.8531369,-0.4593253, 0.0964853,-0.8995501,-0.4260284,
     1-0.0736459,-0.8404444,-0.5368701, 0.1012458,-0.8057173,-0.5835828,
     1 0.2522390,-0.3121796,-0.9159254, 0.1270059,-0.1907368,-0.9733904,
     1 0.0005198,-0.0511251,-0.9986921,-0.1114138, 0.0936556,-0.9893511,
     1 0.3296004,-0.1457054,-0.9328095, 0.1967639,-0.0017960,-0.9804493,
     1 0.0636600, 0.1506465,-0.9865359, 0.3870624, 0.0356406,-0.9213644,
     1 0.2460544, 0.1914125,-0.9501676, 0.4166554, 0.2124250,-0.8838970,
     1 0.5548388, 0.6680854,-0.4957981, 0.4303768, 0.7893874,-0.4377709,
     1 0.2915983, 0.8884990,-0.3543162, 0.1556815, 0.9552719,-0.2514335,
     1 0.6006000, 0.7293029,-0.3277147, 0.4616434, 0.8514539,-0.2488205,
     1 0.3076298, 0.9396901,-0.1494871, 0.6161293, 0.7744908,-0.1433481,
     1 0.4610362, 0.8859591,-0.0502213, 0.5988273, 0.8000080, 0.0373252,
     1 0.5299939, 0.5884581, 0.6105927, 0.3728335, 0.6056088, 0.7030171,
     1 0.2000617, 0.5936648, 0.7794469, 0.0306952, 0.5512444, 0.8337790,
     1 0.5138545, 0.4495858, 0.7306341, 0.3370984, 0.4506436, 0.8266106,
     1 0.1475693, 0.4227966, 0.8941288, 0.4677343, 0.2963645, 0.8327021,
     1 0.2744222, 0.2825559, 0.9191597, 0.3959738, 0.1447478, 0.9067816,
     1 0.2123542,-0.4408431, 0.8721027, 0.0350413,-0.4872804, 0.8725422,
     1-0.1471809,-0.5274925, 0.8367135,-0.3132067,-0.5597032, 0.7672248,
     1 0.1893585,-0.5978569, 0.7789162,-0.0042972,-0.6500839, 0.7598503,
     1-0.1951792,-0.6847633, 0.7021426, 0.1465996,-0.7382719, 0.6583791,
     1-0.0561445,-0.7842261, 0.6179297, 0.0882676,-0.8477514, 0.5229975,
     1-0.9812591, 0.1906010, 0.0283154,-0.9989277, 0.0254834,-0.0386537/
C
      DATA DIRVEC3/
     1-0.9834591,-0.1488439,-0.1032172,-0.9326253,-0.3229891,-0.1608975,
     1-0.8500769,-0.4828136,-0.2103813,-0.7425310,-0.6222835,-0.2478123,
     1-0.9829661, 0.0766674, 0.1670321,-0.9868132,-0.1212884, 0.1071856,
     1-0.9553364,-0.2930902, 0.0378221,-0.8873701,-0.4609851,-0.0081897,
     1-0.7772315,-0.6252409,-0.0706046,-0.9520239,-0.0484036, 0.3021715,
     1-0.9396461,-0.2420445, 0.2418255,-0.8826115,-0.4351105, 0.1779772,
     1-0.7894708,-0.6029041, 0.1150761,-0.8869887,-0.1759242, 0.4269681,
     1-0.8590968,-0.3713767, 0.3521819,-0.7741773,-0.5578997, 0.2989942,
     1-0.7930808,-0.2969957, 0.5318049,-0.7342842,-0.4913948, 0.4683565,
     1-0.6756492,-0.4073069, 0.6144910,-0.9666798, 0.2375671,-0.0953522,
     1-0.9322567, 0.2397941,-0.2709175,-0.8658904, 0.2296341,-0.4444119,
     1-0.7684919, 0.2080234,-0.6051004,-0.6452074, 0.1784977,-0.7428634,
     1-0.5071407, 0.1396995,-0.8504660,-0.9853676, 0.0686671,-0.1559984,
     1-0.9340202, 0.0496932,-0.3537468,-0.8510901, 0.0429217,-0.5232622,
     1-0.7417719, 0.0095592,-0.6705841,-0.5887429,-0.0152508,-0.8081765,
     1-0.9701622,-0.1067559,-0.2176888,-0.9038559,-0.1274342,-0.4084178,
     1-0.7949656,-0.1504441,-0.5877042,-0.6565654,-0.1735207,-0.7340385,
     1-0.9193727,-0.2805596,-0.2757537,-0.8341684,-0.2900428,-0.4690823,
     1-0.7028670,-0.3263740,-0.6320269,-0.8365796,-0.4392398,-0.3274186,
     1-0.7257662,-0.4634108,-0.5084426,-0.7280817,-0.5765164,-0.3708448,
     1-0.9273296, 0.3640034,-0.0869560,-0.8458844, 0.5174984,-0.1291321,
     1-0.7328066, 0.6583191,-0.1720769,-0.5921748, 0.7770403,-0.2133949,
     1-0.4325469, 0.8666843,-0.2485185,-0.2649879, 0.9230097,-0.2789886,
     1-0.8946831, 0.3599881,-0.2644821,-0.7871562, 0.5227960,-0.3272147,
     1-0.6605617, 0.6575745,-0.3622898,-0.5099206, 0.7577143,-0.4072469,
     1-0.3231427, 0.8432888,-0.4294681,-0.8292772, 0.3471574,-0.4379282,
     1-0.7091797, 0.5005054,-0.4965465,-0.5521321, 0.6336749,-0.5418544,
     1-0.3759914, 0.7311777,-0.5692184,-0.7316243, 0.3259235,-0.5987485,
     1-0.5950753, 0.4818933,-0.6431673,-0.4186057, 0.5911954,-0.6893890,
     1-0.6087896, 0.2980938,-0.7351975,-0.4490586, 0.4325733,-0.7818099,
     1-0.4672796, 0.2658349,-0.8431973,-0.9175257, 0.3955333, 0.0412312,
     1-0.8594213, 0.4739671, 0.1917034,-0.7689627, 0.5419043, 0.3391697,
     1-0.6483138, 0.5954330, 0.4744985,-0.5059730, 0.6297209, 0.5894429,
     1-0.3508659, 0.6459225, 0.6779950,-0.8359442, 0.5487708,-0.0069258,
     1-0.7495592, 0.6438899, 0.1535146,-0.6473233, 0.7003911, 0.3007071,
     1-0.5127433, 0.7488706, 0.4198656,-0.3475340, 0.7645414, 0.5428596,
     1-0.7233881, 0.6884379,-0.0525635,-0.6241823, 0.7744512, 0.1030622,
     1-0.4900211, 0.8338494, 0.2541152,-0.3359180, 0.8604027, 0.3832315,
     1-0.5826450, 0.8073234,-0.0935614,-0.4723172, 0.8784191, 0.0727766,
     1-0.3146146, 0.9263902, 0.2069269,-0.4235432, 0.8969308,-0.1269895,
     1-0.2870328, 0.9575846, 0.0253761,-0.2544952, 0.9552220,-0.1509407,
     1-0.9507290, 0.2887910, 0.1127573,-0.9540731, 0.1695501, 0.2469762,
     1-0.9241092, 0.0425042, 0.3797572,-0.8589364,-0.0846225, 0.5050419,
     1-0.7640350,-0.2043223, 0.6119664,-0.6464223,-0.3094724, 0.6973987,
     1-0.8908184, 0.3726203, 0.2599935,-0.8735007, 0.2444101, 0.4210229,
     1-0.8298904, 0.1116656, 0.5466376,-0.7463960,-0.0050286, 0.6654831,
     1-0.6284676,-0.1430920, 0.7645608,-0.7995930, 0.4426983, 0.4057946,
     1-0.7673441, 0.3133302, 0.5594704,-0.6948157, 0.1725520, 0.6981812,
     1-0.5918369, 0.0354547, 0.8052776,-0.6790215, 0.4958641, 0.5413397,
     1-0.6357538, 0.3500632, 0.6879483,-0.5344905, 0.2160156, 0.8171029,
     1-0.5375287, 0.5284579, 0.6571112,-0.4631005, 0.3867355, 0.7974795,
     1-0.3828815, 0.5397248, 0.7497325, 0.9668648,-0.2371558, 0.0944964,
     1 0.9854379,-0.0687132, 0.1555335, 0.9703637, 0.1066273, 0.2168521,
     1 0.9192735, 0.2801336, 0.2765164, 0.8364133, 0.4395542, 0.3274217,
     1 0.7278914, 0.5767977, 0.3707810, 0.9323145,-0.2392953, 0.2711595,
     1 0.9338087,-0.0504401, 0.3541993, 0.9039214, 0.1276307, 0.4082113,
     1 0.8342385, 0.2900721, 0.4689396, 0.7254214, 0.4636750, 0.5086937,
     1 0.8658881,-0.2293913, 0.4445418, 0.8514788,-0.0422840, 0.5226815,
     1 0.7948437, 0.1502827, 0.5879104, 0.7030994, 0.3263167, 0.6317980,
     1 0.7680402,-0.2084809, 0.6055162, 0.7417785,-0.0095462, 0.6705770,
     1 0.6564091, 0.1735566, 0.7341698, 0.6458778,-0.1781603, 0.7423616,
     1 0.5887295, 0.0150791, 0.8081895, 0.5071413,-0.1396181, 0.8504790,
     1 0.9809761,-0.1919695,-0.0288723, 0.9829531,-0.0766441,-0.1671194,
     1 0.9520801, 0.0479097,-0.3020731, 0.8872575, 0.1763487,-0.4262339,
     1 0.7924127, 0.2972706,-0.5326465, 0.6754298, 0.4073950,-0.6146738,
     1 0.9989403,-0.0252802, 0.0384611, 0.9870109, 0.1207057,-0.1060174,
     1 0.9392787, 0.2423722,-0.2429223, 0.8591109, 0.3711544,-0.3523818,
     1 0.7341569, 0.4917390,-0.4681948, 0.9834198, 0.1495596, 0.1025540,
     1 0.9553674, 0.2929182,-0.0383682, 0.8828938, 0.4348252,-0.1772725,
     1 0.7741035, 0.5579715,-0.2990513, 0.9325793, 0.3226704, 0.1618011,
     1 0.8873608, 0.4610140, 0.0075385, 0.7893150, 0.6030857,-0.1151933,
     1 0.8503355, 0.4824908, 0.2100764, 0.7775575, 0.6248041, 0.0708818,
     1 0.7430684, 0.6218086, 0.2473933, 0.9505013,-0.2895129,-0.1128258,
     1 0.8909964,-0.3724502,-0.2596271, 0.7996674,-0.4426285,-0.4057241,
     1 0.6794690,-0.4953244,-0.5412721, 0.5368648,-0.5289105,-0.6572898,
     1 0.3830114,-0.5397558,-0.7496439, 0.9540329,-0.1694330,-0.2472117,
     1 0.8738048,-0.2435842,-0.4208704, 0.7669684,-0.3139594,-0.5596329,
     1 0.6357659,-0.3499980,-0.6879703, 0.4631713,-0.3866466,-0.7974815,
     1 0.9238671,-0.0425346,-0.3803425, 0.8298864,-0.1118796,-0.5466000,
     1 0.6949633,-0.1723403,-0.6980866, 0.5347067,-0.2159574,-0.8169768,
     1 0.8591040, 0.0852991,-0.5046428, 0.7462989, 0.0050140,-0.6655921,
     1 0.5915851,-0.0355550,-0.8054582, 0.7643600, 0.2038572,-0.6117157,
     1 0.6287199, 0.1430686,-0.7643577, 0.6462127, 0.3092641,-0.6976854,
     1 0.9170745,-0.3966052,-0.0409730, 0.8363429,-0.5481609, 0.0070805,
     1 0.7234841,-0.6883230, 0.0527469, 0.5826837,-0.8073374, 0.0931990,
     1 0.4230743,-0.8971118, 0.1272736, 0.2547875,-0.9551312, 0.1510221,
     1 0.8591054,-0.4743216,-0.1922422, 0.7492113,-0.6441720,-0.1540291,
     1 0.6243073,-0.7744098,-0.1026153, 0.4723344,-0.8784008,-0.0728846,
     1 0.2870123,-0.9575879,-0.0254818, 0.7688567,-0.5420358,-0.3391998,
     1 0.6472879,-0.7005077,-0.3005118, 0.4898026,-0.8339246,-0.2542897,
     1 0.3150281,-0.9262668,-0.2068505, 0.6483935,-0.5951524,-0.4747415/
C
      DATA DIRVEC4/
     1 0.5126790,-0.7488122,-0.4200482, 0.3356368,-0.8604901,-0.3832815,
     1 0.5064962,-0.6296693,-0.5890485, 0.3476371,-0.7644433,-0.5429318,
     1 0.3504511,-0.6460955,-0.6780447, 0.9273379,-0.3637188, 0.0880513,
     1 0.8949703,-0.3596463, 0.2639747, 0.8295843,-0.3467895, 0.4376379,
     1 0.7315840,-0.3262101, 0.5986417, 0.6082751,-0.2979594, 0.7356777,
     1 0.4676247,-0.2656751, 0.8430563, 0.8456342,-0.5178135, 0.1295064,
     1 0.7868054,-0.5232641, 0.3273103, 0.7092913,-0.5002757, 0.4966187,
     1 0.5952377,-0.4819207, 0.6429966, 0.4489007,-0.4326996, 0.7818307,
     1 0.7327558,-0.6583304, 0.1722499, 0.6607049,-0.6572710, 0.3625795,
     1 0.5519621,-0.6337749, 0.5419107, 0.4189099,-0.5910608, 0.6893197,
     1 0.5919734,-0.7772817, 0.2130744, 0.5098071,-0.7578528, 0.4071313,
     1 0.3757389,-0.7312587, 0.5692811, 0.4330401,-0.8663886, 0.2486905,
     1 0.3231223,-0.8433218, 0.4294187, 0.2646563,-0.9230771, 0.2790800,
     1-0.5869274,-0.5050019, 0.6328422,-0.4795177,-0.6442833, 0.5957867,
     1-0.3549277,-0.7674340, 0.5339207,-0.2173323,-0.8659199, 0.4504991,
     1-0.0766925,-0.9334900, 0.3503065, 0.0585034,-0.9693521, 0.2386080,
     1-0.6507731,-0.5843348, 0.4848167,-0.5300694,-0.7376464, 0.4182156,
     1-0.3949630,-0.8486699, 0.3518007,-0.2602363,-0.9316901, 0.2534376,
     1-0.0927095,-0.9852062, 0.1441307,-0.6927674,-0.6486794, 0.3151005,
     1-0.5665248,-0.7880406, 0.2409183,-0.4111485,-0.8995035, 0.1478187,
     1-0.2494939,-0.9673453, 0.0446755,-0.7081035,-0.6937788, 0.1313791,
     1-0.5600594,-0.8263807, 0.0585523,-0.4015986,-0.9142393,-0.0537127,
     1-0.6948038,-0.7172169,-0.0533626,-0.5390788,-0.8294764,-0.1461609,
     1-0.6549991,-0.7200014,-0.2292906,-0.6306600,-0.6462877,-0.4296278,
     1-0.4921168,-0.6854297,-0.5366630,-0.3336491,-0.6998573,-0.6315679,
     1-0.1618684,-0.6871844,-0.7082204, 0.0116576,-0.6484326,-0.7611828,
     1 0.1767325,-0.5863713,-0.7905278,-0.6335216,-0.5285980,-0.5650084,
     1-0.4712011,-0.5486585,-0.6906109,-0.3038068,-0.5563297,-0.7734330,
     1-0.1339807,-0.5239114,-0.8411694, 0.0641187,-0.4768313,-0.8766531,
     1-0.6128334,-0.3904342,-0.6870199,-0.4457889,-0.3995332,-0.8010277,
     1-0.2521039,-0.3860683,-0.8873527,-0.0584110,-0.3489869,-0.9353054,
     1-0.5667982,-0.2375203,-0.7888750,-0.3811515,-0.2491430,-0.8903097,
     1-0.1839153,-0.2100251,-0.9602420,-0.4980211,-0.0811277,-0.8633616,
     1-0.3034297,-0.0677757,-0.9504404,-0.4108046, 0.0699984,-0.9090323,
     1-0.3459350, 0.2746389,-0.8971636,-0.1785845, 0.3302650,-0.9268401,
     1-0.0001023, 0.3803707,-0.9248341, 0.1810903, 0.4209943,-0.8888026,
     1 0.3516237, 0.4492126,-0.8213214, 0.5021493, 0.4673361,-0.7276283,
     1-0.3339964, 0.4418054,-0.8326190,-0.1418100, 0.5165181,-0.8444519,
     1 0.0406314, 0.5558533,-0.8302868, 0.2129330, 0.5965647,-0.7738023,
     1 0.3994696, 0.6080369,-0.6860868,-0.3059970, 0.5998257,-0.7393071,
     1-0.1150210, 0.6669700,-0.7361530, 0.0883131, 0.7126296,-0.6959596,
     1 0.2759418, 0.7323078,-0.6225604,-0.2635564, 0.7396986,-0.6191801,
     1-0.0583638, 0.7912377,-0.6087171, 0.1381809, 0.8309631,-0.5388937,
     1-0.2089623, 0.8516291,-0.4806898,-0.0040341, 0.8978402,-0.4403031,
     1-0.1442756, 0.9316766,-0.3334117,-0.1272633, 0.9838774,-0.1256558,
     1 0.0274284, 0.9989314,-0.0371993, 0.1850998, 0.9808868, 0.0599921,
     1 0.3379549, 0.9276990, 0.1586221, 0.4740942, 0.8433286, 0.2530446,
     1 0.5854407, 0.7351261, 0.3418315,-0.1666051, 0.9847705, 0.0496981,
     1 0.0028896, 0.9857902, 0.1679561, 0.1631017, 0.9519844, 0.2590821,
     1 0.3018936, 0.8818113, 0.3623108, 0.4497889, 0.7699209, 0.4526718,
     1-0.1965987, 0.9529637, 0.2306711,-0.0309395, 0.9377607, 0.3459013,
     1 0.1397925, 0.8786253, 0.4565913, 0.2914714, 0.7828005, 0.5497889,
     1-0.2177587, 0.8871928, 0.4067802,-0.0379313, 0.8569218, 0.5140489,
     1 0.1192603, 0.7699712, 0.6268344,-0.2274176, 0.7920376, 0.5665312,
     1-0.0551187, 0.7328144, 0.6781924,-0.2236212, 0.6753226, 0.7028036,
     1-0.2757446, 0.5027445, 0.8192759,-0.1578245, 0.3972861, 0.9040217,
     1-0.0340254, 0.2711520, 0.9619350, 0.0918200, 0.1322031, 0.9869607,
     1 0.2092515,-0.0111949, 0.9777978, 0.3107679,-0.1533089, 0.9380403,
     1-0.3617386, 0.3515384, 0.8634616,-0.2364651, 0.2111145, 0.9484276,
     1-0.1059905, 0.0839330, 0.9908185, 0.0091142,-0.0627312, 0.9979888,
     1 0.1453821,-0.2146281, 0.9658151,-0.4356171, 0.1813575, 0.8816730,
     1-0.3103389, 0.0383450, 0.9498523,-0.1689542,-0.1174535, 0.9786006,
     1-0.0334413,-0.2674723, 0.9629851,-0.4927480, 0.0011437, 0.8701713,
     1-0.3481680,-0.1426124, 0.9265208,-0.2141633,-0.3081559, 0.9269164,
     1-0.5279658,-0.1777799, 0.8304495,-0.3852186,-0.3340986, 0.8602237,
     1-0.5394444,-0.3454955, 0.7678753, 0.1275803,-0.9838467, 0.1255741,
     1-0.0278204,-0.9989201, 0.0372118,-0.1851108,-0.9808879,-0.0599401,
     1-0.3377076,-0.9277871,-0.1586338,-0.4738291,-0.8434794,-0.2530384,
     1-0.5853876,-0.7353343,-0.3414744, 0.1666045,-0.9847729,-0.0496520,
     1-0.0026727,-0.9857927,-0.1679455,-0.1628592,-0.9520220,-0.2590965,
     1-0.3018106,-0.8818092,-0.3623849,-0.4499790,-0.7698261,-0.4526440,
     1 0.1968761,-0.9529286,-0.2305794, 0.0308820,-0.9377874,-0.3458339,
     1-0.1399279,-0.8785611,-0.4566735,-0.2918021,-0.7826545,-0.5498214,
     1 0.2175899,-0.8872063,-0.4068411, 0.0375738,-0.8569164,-0.5140842,
     1-0.1192427,-0.7699801,-0.6268268, 0.2274281,-0.7920837,-0.5664627,
     1 0.0556157,-0.7328189,-0.6781470, 0.2232392,-0.6753545,-0.7028945,
     1 0.2760064,-0.5027142,-0.8192063, 0.1574999,-0.3972528,-0.9040929,
     1 0.0339792,-0.2712201,-0.9619174,-0.0915116,-0.1322334,-0.9869853,
     1-0.2092961, 0.0112206,-0.9777879,-0.3108528, 0.1531040,-0.9380457,
     1 0.3618404,-0.3515194,-0.8634267, 0.2367893,-0.2112015,-0.9483273,
     1 0.1059258,-0.0839084,-0.9908275,-0.0091287, 0.0626665,-0.9979928,
     1-0.1452364, 0.2145286,-0.9658591, 0.4355729,-0.1814424,-0.8816773,
     1 0.3102590,-0.0384726,-0.9498733, 0.1690921, 0.1175333,-0.9785672,
     1 0.0329881, 0.2676892,-0.9629404, 0.4926776,-0.0010457,-0.8702113,
     1 0.3476018, 0.1428012,-0.9267043, 0.2141736, 0.3081045,-0.9269311,
     1 0.5280552, 0.1775926,-0.8304327, 0.3858563, 0.3340454,-0.8599584,
     1 0.5391623, 0.3454737,-0.7680833, 0.5870046, 0.5049690,-0.6327970,
     1 0.4791310, 0.6445789,-0.5957781, 0.3547911, 0.7675057,-0.5339086,
     1 0.2177919, 0.8658577,-0.4503966, 0.0764952, 0.9335162,-0.3502800,
     1-0.0585397, 0.9693241,-0.2387132, 0.6505572, 0.5845570,-0.4848385,
     1 0.5306298, 0.7372965,-0.4181219, 0.3947351, 0.8487868,-0.3517743/
C
      DATA DIRVEC5/
     1 0.2602775, 0.9316662,-0.2534830, 0.0929004, 0.9851825,-0.1441697,
     1 0.6926108, 0.6488399,-0.3151144, 0.5666521, 0.7879489,-0.2409192,
     1 0.4113097, 0.8994460,-0.1477199, 0.2490734, 0.9674587,-0.0445664,
     1 0.7080775, 0.6938154,-0.1313257, 0.5594869, 0.8267670,-0.0585730,
     1 0.4017021, 0.9141924, 0.0537371, 0.6950818, 0.7169408, 0.0534530,
     1 0.5394056, 0.8292271, 0.1463697, 0.6551153, 0.7199305, 0.2291812,
     1 0.6306466, 0.6465274, 0.4292869, 0.4916614, 0.6855118, 0.5369754,
     1 0.3335701, 0.6999115, 0.6315496, 0.1623017, 0.6869928, 0.7083072,
     1-0.0118485, 0.6485228, 0.7611030,-0.1766420, 0.5865015, 0.7904515,
     1 0.6333331, 0.5287238, 0.5651021, 0.4718283, 0.5484528, 0.6903460,
     1 0.3036165, 0.5562899, 0.7735364, 0.1340375, 0.5239568, 0.8411321,
     1-0.0640353, 0.4768182, 0.8766663, 0.6128587, 0.3903040, 0.6870713,
     1 0.4458411, 0.3995738, 0.8009785, 0.2522571, 0.3860783, 0.8873048,
     1 0.0580340, 0.3489910, 0.9353274, 0.5668561, 0.2375957, 0.7888108,
     1 0.3807252, 0.2493114, 0.8904449, 0.1839808, 0.2100375, 0.9602267,
     1 0.4980306, 0.0810835, 0.8633603, 0.3037443, 0.0676693, 0.9503474,
     1 0.4105759,-0.0699557, 0.9091390, 0.3462895,-0.2746042, 0.8970374,
     1 0.1780205,-0.3302521, 0.9269532, 0.0000576,-0.3803362, 0.9248483,
     1-0.1806131,-0.4212172, 0.8887941,-0.3517541,-0.4491441, 0.8213030,
     1-0.5019904,-0.4671356, 0.7278668, 0.3339404,-0.4417116, 0.8326912,
     1 0.1423439,-0.5165680, 0.8443315,-0.0407051,-0.5558923, 0.8302571,
     1-0.2129459,-0.5965704, 0.7737944,-0.3995505,-0.6079886, 0.6860825,
     1 0.3061288,-0.5998409, 0.7392402, 0.1150017,-0.6668808, 0.7362368,
     1-0.0883482,-0.7126251, 0.6959598,-0.2762765,-0.7322056, 0.6225321,
     1 0.2635001,-0.7396668, 0.6192421, 0.0580696,-0.7912036, 0.6087896,
     1-0.1382130,-0.8309106, 0.5389663, 0.2088877,-0.8516239, 0.4807314,
     1 0.0044294,-0.8978436, 0.4402922, 0.1439599,-0.9317125, 0.3334477,
     1-0.9946661, 0.0869798, 0.0554418,-0.9965854,-0.0817145,-0.0118475,
     1-0.9636612,-0.2558764,-0.0767096,-0.8958908,-0.4254459,-0.1279670,
     1-0.8004345,-0.5751097,-0.1689777,-0.9805601,-0.0305103, 0.1938324,
     1-0.9656975,-0.2246372, 0.1302554,-0.9151385,-0.3963417, 0.0737207,
     1-0.8234752,-0.5672327, 0.0116449,-0.9321809,-0.1558786, 0.3267118,
     1-0.9011772,-0.3510910, 0.2541943,-0.8206851,-0.5355927, 0.1990388,
     1-0.8521451,-0.2828549, 0.4402747,-0.7938607,-0.4775304, 0.3764968,
     1-0.7459890,-0.3999312, 0.5324993,-0.9668420, 0.1765738,-0.1844947,
     1-0.9168588, 0.1748835,-0.3588674,-0.8343219, 0.1618301,-0.5269895,
     1-0.7226347, 0.1338441,-0.6781481,-0.5928227, 0.0969275,-0.7994788,
     1-0.9699905, 0.0034802,-0.2431179,-0.9003948,-0.0140102,-0.4348482,
     1-0.8036700,-0.0355623,-0.5940117,-0.6662921,-0.0600380,-0.7432700,
     1-0.9376006,-0.1731023,-0.3015639,-0.8500286,-0.1857794,-0.4928868,
     1-0.7230215,-0.2183429,-0.6554131,-0.8693055,-0.3395024,-0.3592299,
     1-0.7590543,-0.3633469,-0.5401996,-0.7725854,-0.4859881,-0.4085675,
     1-0.8904558, 0.4221933,-0.1698271,-0.7939639, 0.5704461,-0.2102678,
     1-0.6666219, 0.7021100,-0.2503134,-0.5145216, 0.8066306,-0.2908859,
     1-0.3514222, 0.8776908,-0.3258239,-0.8429582, 0.4119959,-0.3459492,
     1-0.7206877, 0.5658054,-0.4005913,-0.5808608, 0.6836399,-0.4418566,
     1-0.4050619, 0.7830994,-0.4718900,-0.7622079, 0.3928323,-0.5145113,
     1-0.6235325, 0.5450852,-0.5604367,-0.4519280, 0.6562453,-0.6042377,
     1-0.6496297, 0.3704780,-0.6638729,-0.4903978, 0.5050238,-0.7102542,
     1-0.5167241, 0.3422013,-0.7847894,-0.8709881, 0.4847892, 0.0797450,
     1-0.7983283, 0.5565004, 0.2301721,-0.6933051, 0.6159239, 0.3741199,
     1-0.5593680, 0.6620297, 0.4988228,-0.4100155, 0.6884863, 0.5982257,
     1-0.7746655, 0.6316206, 0.0308038,-0.6748978, 0.7133887, 0.1886519,
     1-0.5551162, 0.7667973, 0.3222856,-0.4010091, 0.7966355, 0.4522870,
     1-0.6480389, 0.7614529,-0.0153332,-0.5345168, 0.8321081, 0.1479459,
     1-0.3825100, 0.8794987, 0.2831399,-0.4960564, 0.8668281,-0.0503695,
     1-0.3597289, 0.9274407, 0.1022200,-0.3319551, 0.9401683,-0.0767421,
     1-0.9354423, 0.2774126, 0.2190661,-0.9237863, 0.1527172, 0.3511359,
     1-0.8772869, 0.0229369, 0.4794180,-0.7951535,-0.0994428, 0.5981990,
     1-0.6876737,-0.2095892, 0.6951095,-0.8602440, 0.3564100, 0.3646259,
     1-0.8269193, 0.2233063, 0.5160802,-0.7621580, 0.0983591, 0.6398755,
     1-0.6600597,-0.0384582, 0.7502280,-0.7535836, 0.4207443, 0.5050604,
     1-0.7066615, 0.2767280, 0.6511921,-0.6107866, 0.1425955, 0.7788493,
     1-0.6214418, 0.4618352, 0.6328652,-0.5474019, 0.3198574, 0.7733320,
     1-0.4734773, 0.4815197, 0.7375351, 0.9669741,-0.1764209, 0.1839475,
     1 0.9699197,-0.0041274, 0.2433899, 0.9377529, 0.1725750, 0.3013923,
     1 0.8691044, 0.3396994, 0.3595300, 0.7721989, 0.4864232, 0.4087804,
     1 0.9166987,-0.1747968, 0.3593182, 0.9008388, 0.0145903, 0.4339085,
     1 0.8498354, 0.1857871, 0.4932169, 0.7594191, 0.3632796, 0.5397319,
     1 0.8338095,-0.1621787, 0.5276930, 0.8037086, 0.0356742, 0.5939527,
     1 0.7229914, 0.2181498, 0.6555106, 0.7232452,-0.1336615, 0.6775330,
     1 0.6660108, 0.0601639, 0.7435119, 0.5930198,-0.0970189, 0.7993215,
     1 0.9945765,-0.0879654,-0.0554944, 0.9806522, 0.0308272,-0.1933155,
     1 0.9325103, 0.1554201,-0.3259894, 0.8515741, 0.2833738,-0.4410452,
     1 0.7455631, 0.4003665,-0.5327685, 0.9965652, 0.0818687, 0.0124643,
     1 0.9657316, 0.2239113,-0.1312489, 0.9008891, 0.3517374,-0.2543218,
     1 0.7943125, 0.4769614,-0.3762652, 0.9634493, 0.2566397, 0.0768212,
     1 0.9153815, 0.3957705,-0.0737735, 0.8208851, 0.5353412,-0.1988905,
     1 0.8962098, 0.4247697, 0.1279794, 0.8230203, 0.5678902,-0.0117609,
     1 0.8010045, 0.5744232, 0.1686111, 0.9354892,-0.2773622,-0.2189294,
     1 0.8605432,-0.3560186,-0.3643023, 0.7540302,-0.4201349,-0.5049011,
     1 0.6208741,-0.4622944,-0.6330871, 0.4733588,-0.4814627,-0.7376484,
     1 0.9238595,-0.1520653,-0.3512262, 0.8265672,-0.2241111,-0.5162954,
     1 0.7067119,-0.2766506,-0.6511703, 0.5475783,-0.3198832,-0.7731964,
     1 0.8771257,-0.0223858,-0.4797389, 0.7623274,-0.0982717,-0.6396871,
     1 0.6109365,-0.1424120,-0.7787653, 0.7954274, 0.0992958,-0.5978592,
     1 0.6595899, 0.0384125,-0.7506435, 0.6881598, 0.2092553,-0.6947289,
     1 0.8705730,-0.4854498,-0.0802571, 0.7751407,-0.6310402,-0.0307442,
     1 0.6481669,-0.7613497, 0.0150426, 0.4957203,-0.8670080, 0.0505817,
     1 0.3317458,-0.9402503, 0.0766430, 0.7980346,-0.5567023,-0.2307020,
     1 0.6744373,-0.7139714,-0.1880937, 0.5348450,-0.8318982,-0.1479397/
C
      DATA DIRVEC6/
     1 0.3598729,-0.9273907,-0.1021665, 0.6932405,-0.6157917,-0.3744573,
     1 0.5549787,-0.7667927,-0.3225330, 0.3823542,-0.8794949,-0.2833620,
     1 0.5596409,-0.6619808,-0.4985814, 0.4007500,-0.7967527,-0.4523103,
     1 0.4104152,-0.6882858,-0.5981822, 0.8902445,-0.4224709, 0.1702444,
     1 0.8432105,-0.4119514, 0.3453870, 0.7624086,-0.3929582, 0.5141177,
     1 0.6494057,-0.3704652, 0.6640992, 0.5162220,-0.3423129, 0.7850711,
     1 0.7936391,-0.5708094, 0.2105080, 0.7206823,-0.5653034, 0.4013092,
     1 0.6236708,-0.5450604, 0.5603069, 0.4908507,-0.5048516, 0.7100637,
     1 0.6663917,-0.7024213, 0.2500527, 0.5806318,-0.6838672, 0.4418058,
     1 0.4516953,-0.6564175, 0.6042247, 0.5148987,-0.8063412, 0.2910210,
     1 0.4049495,-0.7830911, 0.4720002, 0.3514826,-0.8777269, 0.3256617,
     1-0.5741986,-0.5902505, 0.5673626,-0.4572405,-0.7213513, 0.5201763,
     1-0.3241154,-0.8327146, 0.4489273,-0.1852432,-0.9165815, 0.3543492,
     1-0.0492096,-0.9678435, 0.2466929,-0.6293584,-0.6607738, 0.4090061,
     1-0.4989237,-0.7992700, 0.3350263,-0.3627678,-0.8970774, 0.2522928,
     1-0.2012459,-0.9683246, 0.1478094,-0.6610522,-0.7139885, 0.2307170,
     1-0.5134317,-0.8441232, 0.1544148,-0.3564022,-0.9331947, 0.0460990,
     1-0.6599911,-0.7499144, 0.0451691,-0.5032522,-0.8628002,-0.0480944,
     1-0.6307553,-0.7644434,-0.1333192,-0.5834546,-0.6207624,-0.5236743,
     1-0.4346918,-0.6497759,-0.6235658,-0.2681278,-0.6526838,-0.7085981,
     1-0.0947411,-0.6247977,-0.7750174, 0.0730389,-0.5721932,-0.8168600,
     1-0.5771803,-0.4922676,-0.6515639,-0.4062434,-0.5024842,-0.7632011,
     1-0.2364254,-0.4898072,-0.8391615,-0.0420147,-0.4528305,-0.8906061,
     1-0.5465940,-0.3441318,-0.7634188,-0.3606945,-0.3518802,-0.8637591,
     1-0.1657043,-0.3167279,-0.9339302,-0.4862120,-0.1895937,-0.8530253,
     1-0.2910317,-0.1761900,-0.9403498,-0.4060257,-0.0382488,-0.9130609,
     1-0.2807652, 0.3602947,-0.8895834,-0.1073275, 0.4111160,-0.9052428,
     1 0.0743886, 0.4546683,-0.8875489, 0.2510970, 0.4915045,-0.8338906,
     1 0.4096456, 0.5164260,-0.7519938,-0.2633007, 0.5228382,-0.8107484,
     1-0.0687272, 0.5867543,-0.8068432, 0.1094958, 0.6270989,-0.7712053,
     1 0.2990695, 0.6500687,-0.6985472,-0.2309857, 0.6732257,-0.7024334,
     1-0.0267734, 0.7249893,-0.6882396, 0.1689257, 0.7639614,-0.6227577,
     1-0.1791261, 0.7998827,-0.5728014, 0.0256008, 0.8461918,-0.5322631,
     1-0.1168153, 0.8946835,-0.4311561,-0.0848265, 0.9960233,-0.0272417,
     1 0.0724705, 0.9953776, 0.0630189, 0.2306115, 0.9598795, 0.1595298,
     1 0.3748482, 0.8901164, 0.2591940, 0.4962355, 0.7936214, 0.3520161,
     1-0.1218941, 0.9811236, 0.1501279, 0.0476823, 0.9633263, 0.2640622,
     1 0.1978086, 0.9111921, 0.3613872, 0.3508059, 0.8166090, 0.4583503,
     1-0.1505304, 0.9320013, 0.3297182, 0.0270846, 0.8985689, 0.4379958,
     1 0.1848511, 0.8153993, 0.5485928,-0.1633721, 0.8508571, 0.4993514,
     1 0.0087241, 0.7914866, 0.6111242,-0.1630627, 0.7451560, 0.6466476,
     1-0.2654500, 0.4095351, 0.8728215,-0.1430577, 0.2959685, 0.9444242,
     1-0.0154690, 0.1640726, 0.9863270, 0.1050132, 0.0198279, 0.9942731,
     1 0.2123115,-0.1236013, 0.9693537,-0.3476346, 0.2502277, 0.9036240,
     1-0.2182038, 0.1065274, 0.9700716,-0.0942161,-0.0307252, 0.9950775,
     1 0.0413340,-0.1835488, 0.9821412,-0.4165224, 0.0746883, 0.9060523,
     1-0.2738504,-0.0710156, 0.9591469,-0.1399227,-0.2331800, 0.9623142,
     1-0.4609730,-0.1072375, 0.8809109,-0.3178705,-0.2641198, 0.9106037,
     1-0.4804636,-0.2801733, 0.8310581, 0.0845436,-0.9960479, 0.0272214,
     1-0.0724931,-0.9953736,-0.0630572,-0.2304246,-0.9599243,-0.1595302,
     1-0.3745598,-0.8902245,-0.2592398,-0.4961193,-0.7937156,-0.3519676,
     1 0.1220288,-0.9811267,-0.1499979,-0.0473866,-0.9633648,-0.2639750,
     1-0.1977663,-0.9111596,-0.3614924,-0.3509901,-0.8165046,-0.4583952,
     1 0.1504454,-0.9319856,-0.3298016,-0.0273128,-0.8985848,-0.4379490,
     1-0.1851302,-0.8153015,-0.5486440, 0.1630754,-0.8509219,-0.4993379,
     1-0.0086679,-0.7914374,-0.6111887, 0.1634183,-0.7451878,-0.6465211,
     1 0.2653198,-0.4094776,-0.8728880, 0.1430903,-0.2959006,-0.9444406,
     1 0.0157422,-0.1641452,-0.9863106,-0.1051246,-0.0198375,-0.9942612,
     1-0.2123369, 0.1234662,-0.9693653, 0.3477462,-0.2504185,-0.9035282,
     1 0.2181560,-0.1066805,-0.9700656, 0.0942973, 0.0308248,-0.9950668,
     1-0.0411706, 0.1835616,-0.9821457, 0.4165618,-0.0745739,-0.9060436,
     1 0.2737324, 0.0709356,-0.9591865, 0.1394305, 0.2333123,-0.9623536,
     1 0.4604550, 0.1073354,-0.8811698, 0.3180750, 0.2642661,-0.9104898,
     1 0.4808277, 0.2798625,-0.8309523, 0.5738813, 0.5904162,-0.5675113,
     1 0.4570608, 0.7215432,-0.5200681, 0.3245568, 0.8325555,-0.4489034,
     1 0.1850080, 0.9166377,-0.3543267, 0.0491866, 0.9678165,-0.2468035,
     1 0.6297501, 0.6604245,-0.4089674, 0.4988231, 0.7992936,-0.3351197,
     1 0.3628809, 0.8970724,-0.2521480, 0.2014538, 0.9682848,-0.1477865,
     1 0.6612397, 0.7138801,-0.2305152, 0.5133900, 0.8441297,-0.1545175,
     1 0.3559511, 0.9333673,-0.0460906, 0.6595473, 0.7503029,-0.0451989,
     1 0.5034866, 0.8626514, 0.0483099, 0.6309652, 0.7642950, 0.1331767,
     1 0.5833249, 0.6210477, 0.5234805, 0.4344454, 0.6497186, 0.6237972,
     1 0.2685859, 0.6525299, 0.7085664, 0.0945236, 0.6248144, 0.7750305,
     1-0.0729672, 0.5723201, 0.8167775, 0.5775928, 0.4919985, 0.6514016,
     1 0.4064080, 0.5026211, 0.7630233, 0.2363960, 0.4896798, 0.8392441,
     1 0.0421499, 0.4528227, 0.8906037, 0.5465752, 0.3439867, 0.7634976,
     1 0.3606860, 0.3520052, 0.8637117, 0.1654399, 0.3168926, 0.9339212,
     1 0.4860458, 0.1898491, 0.8530632, 0.2910578, 0.1760509, 0.9403677,
     1 0.4061752, 0.0382107, 0.9129960, 0.2803887,-0.3601329, 0.8897676,
     1 0.1071928,-0.4112043, 0.9052186,-0.0739346,-0.4548112, 0.8875136,
     1-0.2512108,-0.4915098, 0.8338532,-0.4096311,-0.5163594, 0.7520474,
     1 0.2637560,-0.5227891, 0.8106320, 0.0688522,-0.5866673, 0.8068958,
     1-0.1095417,-0.6271576, 0.7711510,-0.2991372,-0.6500565, 0.6985295,
     1 0.2308942,-0.6732905, 0.7024014, 0.0266832,-0.7249015, 0.6883355,
     1-0.1692009,-0.7638781, 0.6227851, 0.1789880,-0.7998016, 0.5729579,
     1-0.0256437,-0.8462066, 0.5322376, 0.1170362,-0.8946752, 0.4311134/
C
C     THE ARRAY DIRVEC WAS READ IN FROM FILE DIRFILL.DAT IN FORMER
C     VERSIONS; NOW INCLUDED AS DATA GROUP; VOLKER NOV 1998
C
      DO 8 J=1,91
         DO 8 I=1,3
         DIRVEC(I,2*J-1)=DIRVEC1(I,J)
         DIRVEC(I,2*J)=DIRVEC1(I+3,J)
         DIRVEC(I,2*J+181)=DIRVEC2(I,J)
         DIRVEC(I,2*J+182)=DIRVEC2(I+3,J)
         DIRVEC(I,2*J+363)=DIRVEC3(I,J)
         DIRVEC(I,2*J+364)=DIRVEC3(I+3,J)
         DIRVEC(I,2*J+545)=DIRVEC4(I,J)
         DIRVEC(I,2*J+546)=DIRVEC4(I+3,J)
         DIRVEC(I,2*J+727)=DIRVEC5(I,J)
         DIRVEC(I,2*J+728)=DIRVEC5(I+3,J)
         IF (J.GT.86) GOTO 8
         DIRVEC(I,2*J+909)=DIRVEC6(I,J)
         DIRVEC(I,2*J+910)=DIRVEC6(I+3,J)
   8  CONTINUE
C
C     CHECK FOR CORRECT IMPLEMENTATION
C
      CALL AOLIM()
      IF(MPCTYP.EQ.NONE) NUMAT=NATHF
C
      DO 10 I=1,53
         USEVDW(I)=RVDW(I)
   10 CONTINUE
C
      FEPSI=(EPSI-1.0D+00)/(EPSI+0.5D+00)
C
      IF(MPCTYP.EQ.NONE) THEN
         NDEN=25*NUMAT
      ELSE
         NDEN=3*NORBS-2*NUMAT
      ENDIF
C
      IF (RSOLV .LT. 0.) THEN
         WRITE(6,*) ' RSOLV MUST NOT BE NEGATIVE'
         CALL ABRT
      END IF
      IF (DELSC .LT. 0.1D+00) WRITE(IW,*) ' DELSC TOO SMALL: SET TO 0.1'
      IF (DELSC .GT. RSOLV+0.5D+00) THEN
         WRITE(6,*) ' DELSC UNREASONABLY LARGE'
         CALL ABRT
      END IF
      RDS=MAX(DELSC,0.1D+00)
C
      DO 20 I=1,NUMAT
         IAT=INT(ZAN(I))
         IF (IAT .GT. 53) THEN
            WRITE(6,*) 'MISSING VAN DER WAALS RADIUS'
            CALL ABRT
         ELSE
            AVDW=USEVDW(IAT)
            IF (AVDW .GT. 10.0D+00) THEN
               WRITE(6,*) 'MISSING VAN DER WAALS RADIUS'
               CALL ABRT
            END IF
         END IF
         SRAD(I)=AVDW+RSOLV
   20 CONTINUE
C
      X0=LOG(NSPA*0.1D+00-0.199999D+00)
      Z3=LOG(3.0D+00)
      Z4=LOG(4.0D+00)
      I4=INT(X0/Z4)
      NP1=0
      DO 7 I=0,I4
         X=X0-I*Z4
         N=3**INT(X/Z3)*4**I
         IF(N.GT.NP1)NP1=N
   7  CONTINUE
      NP2=NP1/3
      IF(MOD(NP1,3).NE.0)NP2=NP1/4
      NP1=10*NP1+2
      NP2=MAX(12,NP2*10+2)
      CALL DVFILL(NP1,DIRSM)
      CALL DVFILL(NP2,DIRSMH)
      N0(1)=NP1
      N0(2)=NP2
      DISEX2=(4*(1.5D+00+RSOLV-RDS)*DISEX)**2/NSPA
C
C     ALL THE FOLLOWING LINES ARE COMMENTED OUT BECAUSE THE BASIC GRID
C     IS READ IN ABOVE AS DATA FILE, VOLKER NOV 1998
C
C     CALL DVFILL(NPPA,DIRVEC)
C
C     CALL SEQOPN(76,'DIRDATA','OLD',.TRUE.,'FORMATTED')
C     READ(76,7676)((DIRVEC(I,J),I=1,3),J=1,1082)
C7676 FORMAT(F10.7,2F11.7)
C      WRITE(77,7676)((DIRVEC(I,J),I=1,3),J=1,1082)
C
      DO 100 I=1,107
      CORE(I) = CCORE(I)
100   CONTINUE
      RETURN
      END
C*MODULE COSMO   *DECK DGETF2
      SUBROUTINE DGETF2( M, N, A, LDA, IPIV, INFO )
C
C  -- LAPACK ROUTINE (VERSION 1.0B) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     JUNE 30, 1992
C
C     .. SCALAR ARGUMENTS ..
      INTEGER            INFO, LDA, M, N
C     ..
C     .. ARRAY ARGUMENTS ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * )
C     ..
C
C  PURPOSE
C  =======
C
C  DGETF2 COMPUTES AN LU FACTORIZATION OF A GENERAL M-BY-N MATRIX A
C  USING PARTIAL PIVOTING WITH ROW INTERCHANGES.
C
C  THE FACTORIZATION HAS THE FORM
C     A = P * L * U
C  WHERE P IS A PERMUTATION MATRIX, L IS LOWER TRIANGULAR WITH UNIT
C  DIAGONAL ELEMENTS (LOWER TRAPEZOIDAL IF M > N), AND U IS UPPER
C  TRIANGULAR (UPPER TRAPEZOIDAL IF M < N).
C
C  THIS IS THE RIGHT-LOOKING LEVEL 2 BLAS VERSION OF THE ALGORITHM.
C
C  ARGUMENTS
C  =========
C
C  M       (INPUT) INTEGER
C          THE NUMBER OF ROWS OF THE MATRIX A.  M >= 0.
C
C  N       (INPUT) INTEGER
C          THE NUMBER OF COLUMNS OF THE MATRIX A.  N >= 0.
C
C  A       (INPUT/OUTPUT) DOUBLE PRECISION ARRAY, DIMENSION (LDA,N)
C          ON ENTRY, THE M BY N MATRIX TO BE FACTORED.
C          ON EXIT, THE FACTORS L AND U FROM THE FACTORIZATION
C          A = P*L*U; THE UNIT DIAGONAL ELEMENTS OF L ARE NOT STORED.
C
C  LDA     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,M).
C
C  IPIV    (OUTPUT) INTEGER ARRAY, DIMENSION (MIN(M,N))
C          THE PIVOT INDICES; FOR 1 <= I <= MIN(M,N), ROW I OF THE
C          MATRIX WAS INTERCHANGED WITH ROW IPIV(I).
C
C  INFO    (OUTPUT) INTEGER
C          = 0: SUCCESSFUL EXIT
C          < 0: IF INFO = -K, THE K-TH ARGUMENT HAD AN ILLEGAL VALUE
C          > 0: IF INFO = K, U(K,K) IS EXACTLY ZERO. THE FACTORIZATION
C               HAS BEEN COMPLETED, BUT THE FACTOR U IS EXACTLY
C               SINGULAR, AND DIVISION BY ZERO WILL OCCUR IF IT IS USED
C               TO SOLVE A SYSTEM OF EQUATIONS.
C
C  =====================================================================
C
C     .. PARAMETERS ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            J, JP
C     ..
C     .. EXTERNAL FUNCTIONS ..
      INTEGER            IDAMAX
      EXTERNAL           IDAMAX
C     ..
C     .. EXTERNAL ROUTINES ..
      EXTERNAL           DGER, DSCAL, DSWAP, XERBLA
C     ..
C     .. INTRINSIC FUNCTIONS ..
      INTRINSIC          MAX, MIN
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     TEST THE INPUT PARAMETERS.
C
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGETF2', -INFO )
         RETURN
      END IF
C
C     QUICK RETURN IF POSSIBLE
C
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
C
      DO 10 J = 1, MIN( M, N )
C
C        FIND PIVOT AND TEST FOR SINGULARITY.
C
         JP = J - 1 + IDAMAX( M-J+1, A( J, J ), 1 )
         IPIV( J ) = JP
         IF( A( JP, J ).NE.ZERO ) THEN
C
C           APPLY THE INTERCHANGE TO COLUMNS 1:N.
C
            IF( JP.NE.J )
     $         CALL DSWAP( N, A( J, 1 ), LDA, A( JP, 1 ), LDA )
C
C           COMPUTE ELEMENTS J+1:M OF J-TH COLUMN.
C
            IF( J.LT.M )
     $         CALL DSCAL( M-J, ONE / A( J, J ), A( J+1, J ), 1 )
C
         ELSE IF( INFO.EQ.0 ) THEN
C
            INFO = J
         END IF
C
         IF( J.LT.MIN( M, N ) ) THEN
C
C           UPDATE TRAILING SUBMATRIX.
C
            CALL DGER( M-J, N-J, -ONE, A( J+1, J ), 1, A( J, J+1 ), LDA,
     $                 A( J+1, J+1 ), LDA )
         END IF
   10 CONTINUE
      RETURN
C
C     END OF DGETF2
C
      END
C*MODULE COSMO   *DECK DGETRF
      SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
C
C  -- LAPACK ROUTINE (VERSION 1.0B) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     FEBRUARY 29, 1992
C
C     .. SCALAR ARGUMENTS ..
      INTEGER            INFO, LDA, M, N
C     ..
C     .. ARRAY ARGUMENTS ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * )
C     ..
C
C  PURPOSE
C  =======
C
C  DGETRF COMPUTES AN LU FACTORIZATION OF A GENERAL M-BY-N MATRIX A
C  USING PARTIAL PIVOTING WITH ROW INTERCHANGES.
C
C  THE FACTORIZATION HAS THE FORM
C     A = P * L * U
C  WHERE P IS A PERMUTATION MATRIX, L IS LOWER TRIANGULAR WITH UNIT
C  DIAGONAL ELEMENTS (LOWER TRAPEZOIDAL IF M > N), AND U IS UPPER
C  TRIANGULAR (UPPER TRAPEZOIDAL IF M < N).
C
C  THIS IS THE RIGHT-LOOKING LEVEL 3 BLAS VERSION OF THE ALGORITHM.
C
C  ARGUMENTS
C  =========
C
C  M       (INPUT) INTEGER
C          THE NUMBER OF ROWS OF THE MATRIX A.  M >= 0.
C
C  N       (INPUT) INTEGER
C          THE NUMBER OF COLUMNS OF THE MATRIX A.  N >= 0.
C
C  A       (INPUT/OUTPUT) DOUBLE PRECISION ARRAY, DIMENSION (LDA,N)
C          ON ENTRY, THE M BY N MATRIX TO BE FACTORED.
C          ON EXIT, THE FACTORS L AND U FROM THE FACTORIZATION
C          A = P*L*U; THE UNIT DIAGONAL ELEMENTS OF L ARE NOT STORED.
C
C  LDA     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,M).
C
C  IPIV    (OUTPUT) INTEGER ARRAY, DIMENSION (MIN(M,N))
C          THE PIVOT INDICES; FOR 1 <= I <= MIN(M,N), ROW I OF THE
C          MATRIX WAS INTERCHANGED WITH ROW IPIV(I).
C
C  INFO    (OUTPUT) INTEGER
C          = 0: SUCCESSFUL EXIT
C          < 0: IF INFO = -K, THE K-TH ARGUMENT HAD AN ILLEGAL VALUE
C          > 0: IF INFO = K, U(K,K) IS EXACTLY ZERO. THE FACTORIZATION
C               HAS BEEN COMPLETED, BUT THE FACTOR U IS EXACTLY
C               SINGULAR, AND DIVISION BY ZERO WILL OCCUR IF IT IS USED
C               TO SOLVE A SYSTEM OF EQUATIONS.
C
C  =====================================================================
C
C     .. PARAMETERS ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            I, IINFO, J, JB, NB
C     ..
C     .. EXTERNAL ROUTINES ..
      EXTERNAL           DGEMM, DGETF2, DLASWP, DTRSM, XERBLA
C     ..
C     .. EXTERNAL FUNCTIONS ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
C     ..
C     .. INTRINSIC FUNCTIONS ..
      INTRINSIC          MAX, MIN
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     TEST THE INPUT PARAMETERS.
C
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGETRF', -INFO )
         RETURN
      END IF
C
C     QUICK RETURN IF POSSIBLE
C
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
C
C     DETERMINE THE BLOCK SIZE FOR THIS ENVIRONMENT.
C
      NB = ILAENV( 1, 'DGETRF', ' ', M, N, -1, -1 )
      IF( NB.LE.1 .OR. NB.GE.MIN( M, N ) ) THEN
C
C        USE UNBLOCKED CODE.
C
         CALL DGETF2( M, N, A, LDA, IPIV, INFO )
      ELSE
C
C        USE BLOCKED CODE.
C
         DO 20 J = 1, MIN( M, N ), NB
            JB = MIN( MIN( M, N )-J+1, NB )
C
C           FACTOR DIAGONAL AND SUBDIAGONAL BLOCKS AND TEST FOR EXACT
C           SINGULARITY.
C
            CALL DGETF2( M-J+1, JB, A( J, J ), LDA, IPIV( J ), IINFO )
C
C           ADJUST INFO AND THE PIVOT INDICES.
C
            IF( INFO.EQ.0 .AND. IINFO.GT.0 )
     $         INFO = IINFO + J - 1
            DO 10 I = J, MIN( M, J+JB-1 )
               IPIV( I ) = J - 1 + IPIV( I )
   10       CONTINUE
C
C           APPLY INTERCHANGES TO COLUMNS 1:J-1.
C
            CALL DLASWP( J-1, A, LDA, J, J+JB-1, IPIV, 1 )
C
            IF( J+JB.LE.N ) THEN
C
C              APPLY INTERCHANGES TO COLUMNS J+JB:N.
C
               CALL DLASWP( N-J-JB+1, A( 1, J+JB ), LDA, J, J+JB-1,
     $                      IPIV, 1 )
C
C              COMPUTE BLOCK ROW OF U.
C
               CALL DTRSM( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'UNIT', JB,
     $                     N-J-JB+1, ONE, A( J, J ), LDA, A( J, J+JB ),
     $                     LDA )
               IF( J+JB.LE.M ) THEN
C
C                 UPDATE TRAILING SUBMATRIX.
C
                  CALL DGEMM( 'NO TRANSPOSE', 'NO TRANSPOSE', M-J-JB+1,
     $                        N-J-JB+1, JB, -ONE, A( J+JB, J ), LDA,
     $                        A( J, J+JB ), LDA, ONE, A( J+JB, J+JB ),
     $                        LDA )
               END IF
            END IF
   20    CONTINUE
      END IF
      RETURN
C
C     END OF DGETRF
C
      END
C*MODULE COSMO   *DECK DGETRI
      SUBROUTINE DGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
C
C  -- LAPACK ROUTINE (VERSION 1.0B) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     JUNE 30, 1992
C
C     .. SCALAR ARGUMENTS ..
      INTEGER            INFO, LDA, LWORK, N
C     ..
C     .. ARRAY ARGUMENTS ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * ), WORK( LWORK )
C     ..
C
C  PURPOSE
C  =======
C
C  DGETRI COMPUTES THE INVERSE OF A MATRIX USING THE LU FACTORIZATION
C  COMPUTED BY DGETRF.
C
C  THIS METHOD INVERTS U AND THEN COMPUTES INV(A) BY SOLVING THE SYSTEM
C  INV(A)*L = INV(U) FOR INV(A).
C
C  ARGUMENTS
C  =========
C
C  N       (INPUT) INTEGER
C          THE ORDER OF THE MATRIX A.  N >= 0.
C
C  A       (INPUT/OUTPUT) DOUBLE PRECISION ARRAY, DIMENSION (LDA,N)
C          ON ENTRY, THE FACTORS L AND U FROM THE FACTORIZATION
C          A = P*L*U AS COMPUTED BY DGETRF.
C          ON EXIT, IF INFO = 0, THE INVERSE OF THE ORIGINAL MATRIX A.
C
C  LDA     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,N).
C
C  IPIV    (INPUT) INTEGER ARRAY, DIMENSION (N)
C          THE PIVOT INDICES FROM DGETRF; FOR 1<=I<=N, ROW I OF THE
C          MATRIX WAS INTERCHANGED WITH ROW IPIV(I).
C
C  WORK    (WORKSPACE) DOUBLE PRECISION ARRAY, DIMENSION (LWORK)
C          IF INFO RETURNS 0, THEN WORK(1) RETURNS N*NB, THE MINIMUM
C          VALUE OF LWORK REQUIRED TO USE THE OPTIMAL BLOCKSIZE.
C
C  LWORK   (INPUT) INTEGER
C          THE DIMENSION OF THE ARRAY WORK.  LWORK >= MAX(1,N).
C          FOR OPTIMAL PERFORMANCE LWORK SHOULD BE AT LEAST N*NB,
C          WHERE NB IS THE OPTIMAL BLOCKSIZE RETURNED BY ILAENV.
C
C  INFO    (OUTPUT) INTEGER
C          = 0:  SUCCESSFUL EXIT
C          < 0: IF INFO = -K, THE K-TH ARGUMENT HAD AN ILLEGAL VALUE
C          > 0: IF INFO = K, U(K,K) IS EXACTLY ZERO; THE MATRIX IS
C               SINGULAR AND ITS INVERSE COULD NOT BE COMPUTED.
C
C  ====================================================================
C
C     .. PARAMETERS ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      INTEGER            I, IWS, J, JB, JJ, JP, LDWORK, NB, NBMIN, NN
C     ..
C     .. EXTERNAL FUNCTIONS ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
C     ..
C     .. EXTERNAL ROUTINES ..
      EXTERNAL           DGEMM, DGEMV, DSWAP, DTRSM, DTRTRI, XERBLA
C     ..
C     .. INTRINSIC FUNCTIONS ..
      INTRINSIC          MAX, MIN
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     TEST THE INPUT PARAMETERS.
C
      INFO = 0
      WORK( 1 ) = MAX( N, 1 )
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -3
      ELSE IF( LWORK.LT.MAX( 1, N ) ) THEN
         INFO = -6
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGETRI', -INFO )
         RETURN
      END IF
C
C     QUICK RETURN IF POSSIBLE
C
      IF( N.EQ.0 )
     $   RETURN
C
C     FORM INV(U).  IF INFO > 0 FROM DTRTRI, THEN U IS SINGULAR,
C     AND THE INVERSE IS NOT COMPUTED.
C
      CALL DTRTRI( 'UPPER', 'NON-UNIT', N, A, LDA, INFO )
      IF( INFO.GT.0 )
     $   RETURN
C
C     DETERMINE THE BLOCK SIZE FOR THIS ENVIRONMENT.
C
      NB = ILAENV( 1, 'DGETRI', ' ', N, -1, -1, -1 )
      NBMIN = 2
      LDWORK = N
      IF( NB.GT.1 .AND. NB.LT.N ) THEN
         IWS = MAX( LDWORK*NB, 1 )
         IF( LWORK.LT.IWS ) THEN
            NB = LWORK / LDWORK
            NBMIN = MAX( 2, ILAENV( 2, 'DGETRI', ' ', N, -1, -1, -1 ) )
         END IF
      ELSE
         IWS = N
      END IF
C
C     SOLVE THE EQUATION INV(A)*L = INV(U) FOR INV(A).
C
      IF( NB.LT.NBMIN .OR. NB.GE.N ) THEN
C
C        USE UNBLOCKED CODE.
C
         DO 20 J = N, 1, -1
C
C           COPY CURRENT COLUMN OF L TO WORK AND REPLACE WITH ZEROS.
C
            DO 10 I = J + 1, N
               WORK( I ) = A( I, J )
               A( I, J ) = ZERO
   10       CONTINUE
C
C           COMPUTE CURRENT COLUMN OF INV(A).
C
            IF( J.LT.N )
     $         CALL DGEMV( 'NO TRANSPOSE', N, N-J, -ONE, A( 1, J+1 ),
     $                     LDA, WORK( J+1 ), 1, ONE, A( 1, J ), 1 )
   20    CONTINUE
      ELSE
C
C        USE BLOCKED CODE.
C
         NN = ( ( N-1 ) / NB )*NB + 1
         DO 50 J = NN, 1, -NB
            JB = MIN( NB, N-J+1 )
C
C           COPY CURRENT BLOCK COLUMN OF L TO WORK AND REPLACE WITH
C           ZEROS.
C
            DO 40 JJ = J, J + JB - 1
               DO 30 I = JJ + 1, N
                  WORK( I+( JJ-J )*LDWORK ) = A( I, JJ )
                  A( I, JJ ) = ZERO
   30          CONTINUE
   40       CONTINUE
C
C           COMPUTE CURRENT BLOCK COLUMN OF INV(A).
C
            IF( J+JB.LE.N )
     $         CALL DGEMM( 'NO TRANSPOSE', 'NO TRANSPOSE', N, JB,
     $                     N-J-JB+1, -ONE, A( 1, J+JB ), LDA,
     $                     WORK( J+JB ), LDWORK, ONE, A( 1, J ), LDA )
            CALL DTRSM( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', N, JB,
     $                  ONE, WORK( J ), LDWORK, A( 1, J ), LDA )
   50    CONTINUE
      END IF
C
C     APPLY COLUMN INTERCHANGES.
C
      DO 60 J = N - 1, 1, -1
         JP = IPIV( J )
         IF( JP.NE.J )
     $      CALL DSWAP( N, A( 1, J ), 1, A( 1, JP ), 1 )
   60 CONTINUE
C
      WORK( 1 ) = IWS
      RETURN
C
C     END OF DGETRI
C
      END
C*MODULE COSMO   *DECK DLASWP
      SUBROUTINE DLASWP( N, A, LDA, K1, K2, IPIV, INCX )
C
C  -- LAPACK AUXILIARY ROUTINE (VERSION 1.0B) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     OCTOBER 31, 1992
C
C     .. SCALAR ARGUMENTS ..
      INTEGER            INCX, K1, K2, LDA, N
C     ..
C     .. ARRAY ARGUMENTS ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * )
C     ..
C
C  PURPOSE
C  =======
C
C  DLASWP PERFORMS A SERIES OF ROW INTERCHANGES ON THE MATRIX A.
C  ONE ROW INTERCHANGE IS INITIATED FOR EACH OF ROWS K1 THROUGH K2 OF A.
C
C  ARGUMENTS
C  =========
C
C  N       (INPUT) INTEGER
C          THE NUMBER OF COLUMNS OF THE MATRIX A.
C
C  A       (INPUT/OUTPUT) DOUBLE PRECISION ARRAY, DIMENSION (LDA,N)
C          ON ENTRY, THE MATRIX OF COLUMN DIMENSION N TO WHICH THE ROW
C          INTERCHANGES WILL BE APPLIED.
C          ON EXIT, THE PERMUTED MATRIX.
C
C  LDA     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY A.
C
C  K1      (INPUT) INTEGER
C          THE FIRST ELEMENT OF IPIV FOR WHICH A ROW INTERCHANGE WILL
C          BE DONE.
C
C  K2      (INPUT) INTEGER
C          THE LAST ELEMENT OF IPIV FOR WHICH A ROW INTERCHANGE WILL
C          BE DONE.
C
C  IPIV    (INPUT) INTEGER ARRAY, DIMENSION (M*ABS(INCX))
C          THE VECTOR OF PIVOT INDICES.  ONLY THE ELEMENTS IN POSITIONS
C          K1 THROUGH K2 OF IPIV ARE ACCESSED.
C          IPIV(K) = L IMPLIES ROWS K AND L ARE TO BE INTERCHANGED.
C
C  INCX    (INPUT) INTEGER
C          THE INCREMENT BETWEEN SUCCESSIVE VALUES OF IPIV.  IF IPIV
C          IS NEGATIVE, THE PIVOTS ARE APPLIED IN REVERSE ORDER.
C
C =====================================================================
C
C     .. LOCAL SCALARS ..
      INTEGER            I, IP, IX
C     ..
C     .. EXTERNAL ROUTINES ..
      EXTERNAL           DSWAP
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     INTERCHANGE ROW I WITH ROW IPIV(I) FOR EACH OF ROWS K1 THROUGH K2.
C
      IF( INCX.EQ.0 )
     $   RETURN
      IF( INCX.GT.0 ) THEN
         IX = K1
      ELSE
         IX = 1 + ( 1-K2 )*INCX
      END IF
      IF( INCX.EQ.1 ) THEN
         DO 10 I = K1, K2
            IP = IPIV( I )
            IF( IP.NE.I )
     $         CALL DSWAP( N, A( I, 1 ), LDA, A( IP, 1 ), LDA )
   10    CONTINUE
      ELSE IF( INCX.GT.1 ) THEN
         DO 20 I = K1, K2
            IP = IPIV( IX )
            IF( IP.NE.I )
     $         CALL DSWAP( N, A( I, 1 ), LDA, A( IP, 1 ), LDA )
            IX = IX + INCX
   20    CONTINUE
      ELSE IF( INCX.LT.0 ) THEN
         DO 30 I = K2, K1, -1
            IP = IPIV( IX )
            IF( IP.NE.I )
     $         CALL DSWAP( N, A( I, 1 ), LDA, A( IP, 1 ), LDA )
            IX = IX + INCX
   30    CONTINUE
      END IF
C
      RETURN
C
C     END OF DLASWP
C
      END
C*MODULE COSMO   *DECK DTRTRI
      SUBROUTINE DTRTRI( UPLO, DIAG, N, A, LDA, INFO )
C
C  -- LAPACK ROUTINE (VERSION 1.0B) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     FEBRUARY 29, 1992
C
C     .. SCALAR ARGUMENTS ..
      CHARACTER          DIAG, UPLO
      INTEGER            INFO, LDA, N
C     ..
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION   A( LDA, * )
C     ..
C
C  PURPOSE
C  =======
C
C  DTRTRI COMPUTES THE INVERSE OF A REAL UPPER OR LOWER TRIANGULAR
C  MATRIX A.
C
C  THIS IS THE LEVEL 3 BLAS VERSION OF THE ALGORITHM.
C
C  ARGUMENTS
C  =========
C
C  UPLO    (INPUT) CHARACTER*1
C          SPECIFIES WHETHER THE MATRIX A IS UPPER OR LOWER TRIANGULAR.
C          = 'U':  UPPER TRIANGULAR
C          = 'L':  LOWER TRIANGULAR
C
C  DIAG    (INPUT) CHARACTER*1
C          SPECIFIES WHETHER OR NOT THE MATRIX A IS UNIT TRIANGULAR.
C          = 'N':  NON-UNIT TRIANGULAR
C          = 'U':  UNIT TRIANGULAR
C
C  N       (INPUT) INTEGER
C          THE ORDER OF THE MATRIX A.  N >= 0.
C
C  A       (INPUT/OUTPUT) DOUBLE PRECISION ARRAY, DIMENSION (LDA,N)
C
C          ON ENTRY, THE TRIANGULAR MATRIX A.  IF UPLO = 'U', THE
C          LEADING N BY N UPPER TRIANGULAR PART OF THE ARRAY A CONTAINS
C          THE UPPER TRIANGULAR MATRIX, AND THE STRICTLY LOWER
C          TRIANGULAR PART OF A IS NOT REFERENCED.  IF UPLO = 'L', THE
C          LEADING N BY N LOWER TRIANGULAR PART OF THE ARRAY A CONTAINS
C          THE LOWER TRIANGULAR MATRIX, AND THE STRICTLY UPPER
C          TRIANGULAR PART OF A IS NOT REFERENCED.  IF DIAG = 'U', THE
C          DIAGONAL ELEMENTS OF A ARE ALSO NOT REFERENCED AND ARE
C          ASSUMED TO BE 1.
C
C          ON EXIT, THE (TRIANGULAR) INVERSE OF THE ORIGINAL MATRIX, IN
C          THE SAME STORAGE FORMAT.
C
C  LDA     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,N).
C
C  INFO    (OUTPUT) INTEGER
C          = 0: SUCCESSFUL EXIT
C          > 0: IF INFO = K, A(K,K) IS EXACTLY ZERO.  THE TRIANGULAR
C               MATRIX IS SINGULAR AND ITS INVERSE CAN NOT BE COMPUTED.
C          < 0: IF INFO = -K, THE K-TH ARGUMENT HAD AN ILLEGAL VALUE
C
C  =====================================================================
C
C     .. PARAMETERS ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      LOGICAL            NOUNIT, UPPER
      INTEGER            J, JB, NB, NN
C     ..
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
C     ..
C     .. EXTERNAL ROUTINES ..
      EXTERNAL           DTRMM, DTRSM, DTRTI2, XERBLA
C     ..
C     .. INTRINSIC FUNCTIONS ..
      INTRINSIC          MAX, MIN
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     TEST THE INPUT PARAMETERS.
C
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      NOUNIT = LSAME( DIAG, 'N' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOUNIT .AND. .NOT.LSAME( DIAG, 'U' ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DTRTRI', -INFO )
         RETURN
      END IF
C
C     QUICK RETURN IF POSSIBLE
C
      IF( N.EQ.0 )
     $   RETURN
C
C     CHECK FOR SINGULARITY IF NON-UNIT.
C
      IF( NOUNIT ) THEN
         DO 10 INFO = 1, N
            IF( A( INFO, INFO ).EQ.ZERO )
     $         RETURN
   10    CONTINUE
         INFO = 0
      END IF
C
C     DETERMINE THE BLOCK SIZE FOR THIS ENVIRONMENT.
C
      NB = ILAENV( 1, 'DTRTRI', UPLO // DIAG, N, -1, -1, -1 )
      IF( NB.LE.1 .OR. NB.GE.N ) THEN
C
C        USE UNBLOCKED CODE
C
         CALL DTRTI2( UPLO, DIAG, N, A, LDA, INFO )
      ELSE
C
C        USE BLOCKED CODE
C
         IF( UPPER ) THEN
C
C           COMPUTE INVERSE OF UPPER TRIANGULAR MATRIX
C
            DO 20 J = 1, N, NB
               JB = MIN( NB, N-J+1 )
C
C              COMPUTE ROWS 1:J-1 OF CURRENT BLOCK COLUMN
C
               CALL DTRMM( 'LEFT', 'UPPER', 'NO TRANSPOSE', DIAG, J-1,
     $                     JB, ONE, A, LDA, A( 1, J ), LDA )
               CALL DTRSM( 'RIGHT', 'UPPER', 'NO TRANSPOSE', DIAG, J-1,
     $                     JB, -ONE, A( J, J ), LDA, A( 1, J ), LDA )
C
C              COMPUTE INVERSE OF CURRENT DIAGONAL BLOCK
C
               CALL DTRTI2( 'UPPER', DIAG, JB, A( J, J ), LDA, INFO )
   20       CONTINUE
         ELSE
C
C           COMPUTE INVERSE OF LOWER TRIANGULAR MATRIX
C
            NN = ( ( N-1 ) / NB )*NB + 1
            DO 30 J = NN, 1, -NB
               JB = MIN( NB, N-J+1 )
               IF( J+JB.LE.N ) THEN
C
C                 COMPUTE ROWS J+JB:N OF CURRENT BLOCK COLUMN
C
                  CALL DTRMM( 'LEFT', 'LOWER', 'NO TRANSPOSE', DIAG,
     $                        N-J-JB+1, JB, ONE, A( J+JB, J+JB ), LDA,
     $                        A( J+JB, J ), LDA )
                  CALL DTRSM( 'RIGHT', 'LOWER', 'NO TRANSPOSE', DIAG,
     $                        N-J-JB+1, JB, -ONE, A( J, J ), LDA,
     $                        A( J+JB, J ), LDA )
               END IF
C
C              COMPUTE INVERSE OF CURRENT DIAGONAL BLOCK
C
               CALL DTRTI2( 'LOWER', DIAG, JB, A( J, J ), LDA, INFO )
   30       CONTINUE
         END IF
      END IF
C
      RETURN
C
C     END OF DTRTRI
C
      END
C*MODULE COSMO   *DECK DTRTI2
      SUBROUTINE DTRTI2( UPLO, DIAG, N, A, LDA, INFO )
C
C  -- LAPACK ROUTINE (VERSION 1.0B) --
C     UNIV. OF TENNESSEE, UNIV. OF CALIFORNIA BERKELEY, NAG LTD.,
C     COURANT INSTITUTE, ARGONNE NATIONAL LAB, AND RICE UNIVERSITY
C     FEBRUARY 29, 1992
C
C     .. SCALAR ARGUMENTS ..
      CHARACTER          DIAG, UPLO
      INTEGER            INFO, LDA, N
C     ..
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION   A( LDA, * )
C     ..
C
C  PURPOSE
C  =======
C
C  DTRTI2 COMPUTES THE INVERSE OF A REAL UPPER OR LOWER TRIANGULAR
C  MATRIX.
C
C  THIS IS THE LEVEL 2 BLAS VERSION OF THE ALGORITHM.
C
C  ARGUMENTS
C  =========
C
C  UPLO    (INPUT) CHARACTER*1
C          SPECIFIES WHETHER THE MATRIX A IS UPPER OR LOWER TRIANGULAR.
C          = 'U':  UPPER TRIANGULAR
C          = 'L':  LOWER TRIANGULAR
C
C  DIAG    (INPUT) CHARACTER*1
C          SPECIFIES WHETHER OR NOT THE MATRIX A IS UNIT TRIANGULAR.
C          = 'N':  NON-UNIT TRIANGULAR
C          = 'U':  UNIT TRIANGULAR
C
C  N       (INPUT) INTEGER
C          THE ORDER OF THE MATRIX A.  N >= 0.
C
C  A       (INPUT/OUTPUT) DOUBLE PRECISION ARRAY, DIMENSION (LDA,N)
C          ON ENTRY, THE TRIANGULAR MATRIX A.  IF UPLO = 'U', THE
C          LEADING N BY N UPPER TRIANGULAR PART OF THE ARRAY A CONTAINS
C          THE UPPER TRIANGULAR MATRIX, AND THE STRICTLY LOWER
C          TRIANGULAR PART OF A IS NOT REFERENCED.  IF UPLO = 'L', THE
C          LEADING N BY N LOWER TRIANGULAR PART OF THE ARRAY A CONTAINS
C          THE LOWER TRIANGULAR MATRIX, AND THE STRICTLY UPPER
C          TRIANGULAR PART OF A IS NOT REFERENCED.  IF DIAG = 'U', THE
C          DIAGONAL ELEMENTS OF A ARE ALSO NOT REFERENCED AND ARE
C          ASSUMED TO BE 1.
C
C          ON EXIT, THE (TRIANGULAR) INVERSE OF THE ORIGINAL MATRIX, IN
C          THE SAME STORAGE FORMAT.
C
C  LDA     (INPUT) INTEGER
C          THE LEADING DIMENSION OF THE ARRAY A.  LDA >= MAX(1,N).
C
C  INFO    (OUTPUT) INTEGER
C          = 0: SUCCESSFUL EXIT
C          < 0: IF INFO = -K, THE K-TH ARGUMENT HAD AN ILLEGAL VALUE
C
C  =====================================================================
C
C     .. PARAMETERS ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
C     ..
C     .. LOCAL SCALARS ..
      LOGICAL            NOUNIT, UPPER
      INTEGER            J
      DOUBLE PRECISION   AJJ
C     ..
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..
C     .. EXTERNAL ROUTINES ..
      EXTERNAL           DSCAL, DTRMV, XERBLA
C     ..
C     .. INTRINSIC FUNCTIONS ..
      INTRINSIC          MAX
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     TEST THE INPUT PARAMETERS.
C
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      NOUNIT = LSAME( DIAG, 'N' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOUNIT .AND. .NOT.LSAME( DIAG, 'U' ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DTRTI2', -INFO )
         RETURN
      END IF
C
      IF( UPPER ) THEN
C
C        COMPUTE INVERSE OF UPPER TRIANGULAR MATRIX.
C
         DO 10 J = 1, N
            IF( NOUNIT ) THEN
               A( J, J ) = ONE / A( J, J )
               AJJ = -A( J, J )
            ELSE
               AJJ = -ONE
            END IF
C
C           COMPUTE ELEMENTS 1:J-1 OF J-TH COLUMN.
C
            CALL DTRMV( 'UPPER', 'NO TRANSPOSE', DIAG, J-1, A, LDA,
     $                  A( 1, J ), 1 )
            CALL DSCAL( J-1, AJJ, A( 1, J ), 1 )
   10    CONTINUE
      ELSE
C
C        COMPUTE INVERSE OF LOWER TRIANGULAR MATRIX.
C
         DO 20 J = N, 1, -1
            IF( NOUNIT ) THEN
               A( J, J ) = ONE / A( J, J )
               AJJ = -A( J, J )
            ELSE
               AJJ = -ONE
            END IF
            IF( J.LT.N ) THEN
C
C              COMPUTE ELEMENTS J+1:N OF J-TH COLUMN.
C
               CALL DTRMV( 'LOWER', 'NO TRANSPOSE', DIAG, N-J,
     $                     A( J+1, J+1 ), LDA, A( J+1, J ), 1 )
               CALL DSCAL( N-J, AJJ, A( J+1, J ), 1 )
            END IF
   20    CONTINUE
      END IF
C
      RETURN
C
C     END OF DTRTI2
C
      END
