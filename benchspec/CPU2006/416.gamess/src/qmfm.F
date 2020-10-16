C  9 DEC 03 - MWS - SYNCH COMMON BLOCK RUNOPT
C  3 JUN 03 - CHC - NEARJ: RESET LOAD BALANCE COUNTER AT END
C 15 MAY 03 - CHC - ENABLE DFT QFMM JOBS TO PROCEED
C 28 JAN 03 - MWS - KEEP ARGS FOR SHELLS ROUTINE CONSISTENT
C  7 AUG 02 - CHC - NEARJ: FIX TO ALLOW FOR DFT
C 25 OCT 01 - MWS - QFMMIN: USE D.P. INSTEAD OF C*8 DECLARATIONS
C  6 SEP 01 - CHC - NEW MODULE FOR QFMM (QUANTUM FAST MULTIPOLE
C                   METHOD). THIS IS THE MAIN DRIVER FOR QUANTUM PART.
C                   DUE TO THE MULTIPOLE APPROXIMATION, THE STANDARD
C                   INTEGRAL ROUTINES NEEDED TO BE MODIFIED.
C
C*MODULE QMFM    *DECK INITPRMT
      SUBROUTINE INITPRMT(NCXYZ,MQOPS)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DOUBLE PRECISION INTCT
      LOGICAL QFMM,OUT,QOPS,GOPARR,DSKWRK,MASWRK
      PARAMETER (ZERO=0.0D+00,ONE=1.0D+00,TEN=1.0D+01)
      PARAMETER (E=2.30258D+00,TWO=2.0D+00)
      PARAMETER (SLOPE=-0.962780401D+00,INTCT=3.105952915D+00)
      PARAMETER (MXATM=500)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /IQMFM / SCLF,ITGERR,IEPS,IDPGD
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /QMFM  / SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *                ITERMS,QOPS,ISCUT
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
      DIMENSION VMAX(3)
C
      CALL QDIAMTR(SIZE,NAT,C,VMAX)
C
C     IF QOPS=.TRUE., ICUT, ITOL, IEPS,IDPGD, D ARE DETERMINED, WHERE D
C     IS THE DIMENSION OF THE SYSTEM.
C     IF MQOPS=0, THE ROUTINE FURTHER DETERMINES, SCLF AND NS.
C     IF MQOPS=1, USER SHOULD PROVIDE SCLF AND NS.
C
      IF (QOPS) THEN
         ICUT=INT(INTCT-SLOPE*ITGERR+1)
         IF (ICUT.LT.ZERO) ICUT=0
         ITOL=ICUT+4
C
C        TOL : GAUSSIAN DISTRIBUTION CUTOFF
C        CUTOFF : INTEGRAL CUTOFF OF DIRECT SCF
C
         TOL = E*ITOL
         CUTOFF = ONE/(TEN**ICUT)/TWO
C
         CALL NZXYZ(NCXYZ,NYP)
C
         IEPS=ICUT
         IDPGD=ITGERR+1
         DPGD=ONE/(TEN**IDPGD)
         EPS=ONE/(TEN**IEPS)
         ID=INT(VMAX(1)+VMAX(2)+VMAX(3))
         IF (ID.EQ.0) ID=1
         IF (MQOPS.EQ.1) THEN
            SIZE=SIZE*SCLF
         ENDIF
            BOXSIZE=SIZE/2**NS
            TR=IWS+1.0D+00
            NP=IGETNP(EPS,TR,BOXSIZE)
         IF (MQOPS.EQ.0) THEN
            CALL SQOPS(NYP,EPS,ID,SCLF,NCXYZ)
         ENDIF
      ELSE
         DPGD=ONE/(TEN**IDPGD)
         EPS=ONE/(TEN**IEPS)
         CALL NZXYZ(NCXYZ,NYP)
      ENDIF
C
      IF (MASWRK) WRITE(IW,9000) QOPS,ID,SIZE,NP,NS,
     *   IWS,IDPGD,IEPS,ISCUT,ITOL,ICUT,ITGERR,SCLF,MQOPS,
     *   NCXYZ,NYP
C
      RETURN
 9000 FORMAT(/5X,'$FMM OPTIONS'/5X,12("-")/
     *  5X,7HQOPS  =,L8,5X,7HD     =,I8,5X,7HCUBE  =,F8.2/
     *  5X,7HNP    =,I8,5X,7HNS    =,I8,5X,7HMINWS =,I8/
     *  5X,7HIDPGD =,I8,5X,7HIEPS  =,I8,5X,7HISCUT =,I8/
     *  5X,7HITOL  =,I8,5X,7HICUT  =,I8,5X,7HITGERR=,I8/
     *  5X,7HSCLF  =,F8.2,5X,7HMQOPS =,I8,5X,7HNCXYZ =,I8/
     *  5X,7HNTP   =,I8/)
      END
C*MODULE QMFM    *DECK INITQFMM
      SUBROUTINE INITQFMM(L2,S,NSHL2,LDST,ISHELL,LSHL,TS,NCXYZ,CXYZ,
     *     IYP,IBS,ISP,IPP,IDXWS,INDX,MAXWS,MAXNYP,NSH2,IDXSHL,IPNTR,
     *     NBOX,NTBOX,CLM,FLM,NFTPL)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL QFMM,QOPS,GOPARR,DSKWRK,MASWRK
C
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /QMFM  / SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *                ITERMS,QOPS,ISCUT
C
      DIMENSION S(L2),TS(ISHELL),LDST(NSHL2),LSHL(ISHELL+1)
      DIMENSION CXYZ(NCXYZ,3),IYP(NCXYZ),
     *          INDX(NCXYZ),IBS(NCXYZ,4),ISP(NCXYZ,2),
     *          IPP(NCXYZ,2),IDXWS(NCXYZ)
      DIMENSION IDXSHL(NSH2+1)
      DIMENSION IPNTR(2**NS,3),FLM(-NP:NP),CLM(-NP:NP)
C
C
      CALL SORTS(S,L2,LDST,NSHL2,LSHL,TS,ISHELL)
C      IF (MASWRK) WRITE(IW,9500)
C      IF (MASWRK) WRITE(IW,9510) NP,NS
      CALL QMPM(NFTPL,NCXYZ,CXYZ,IYP,IBS,ISP,IPP,IDXWS,MAXWS,MAXNYP,
     *      IDXSHL,NSH2,NYP)
      IFLG=2**NS
      IF ( MAXWS.GT.IFLG ) THEN
         IF(MASWRK) WRITE(IW,*) 'NS MUST BE GREATER THAN',NS,'!!'
         IF(MASWRK) WRITE(IW,*) 'THIS MEANS THE PROBLEM AT HAND IS NOT'
         IF(MASWRK) WRITE(IW,*) 'BIG ENOUGH TO USE QFMM EFFECTIVELY.'
         CALL ABRT
      ENDIF
C
      CALL GETCLM(CLM)
      CALL GETFLM(FLM)
      CALL INITIDX(NCXYZ,INDX)
C
C        GET THE NUMBER OF NON-EMPTY BOX (NBOX) IN THE LOWEST
C        SUBDIVISION LEVEL.
C
      CALL GETNBOX(NCXYZ,CXYZ,INDX,IDXWS,IPNTR,NBOX)
      CALL UPPBOX(NBOX,NTBOX)
C
      RETURN
C9500 FORMAT(/10X,40("-")/10X,
C    *40HQUANTUM FAST MULTIPOLE (QFMM) STATISTICS/10X,40(1H-))
C9510 FORMAT(/10X,
C    *'THE HIGEST MULTIPOLE MOMENT IN QFMM  : ',9X,I6,
C    */10X,'THE LEVEL OF MAXIMUM SUB-DIVISION    : ',9X,I6)
      END
C*MODULE QMFM    *DECK QFMMBOX
      SUBROUTINE QFMMBOX(NCXYZ,NMAXBOX)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL QFMM,QOPS
C
      COMMON /FMCOM / X(1)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /QMFM  / SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *                ITERMS,QOPS,ISCUT
C
C        RETURNS MAXIMUM NUMBER OF BOXES -NMAXBOX-
C
      CALL VALFM(LOADFM)
      LCXYZ  = LOADFM + 1
      LINDX  = LCXYZ  + NCXYZ*3
      LIDXWS = LINDX  + NCXYZ/NWDVAR+1
      LPNTR  = LIDXWS + NCXYZ/NWDVAR+1
      LAST   = LPNTR  + 3*2**NS/NWDVAR+1
      NEED  = LAST -LOADFM -1
      CALL GETFM(NEED)
      CALL GENCRD(NCXYZ,X(LCXYZ))
      CALL GETNBOX(NCXYZ,X(LCXYZ),X(LINDX),X(LIDXWS),X(LPNTR),NBOX)
      CALL RETFM(NEED)
      NMAXBOX=NBOX
C
      RETURN
      END
C*MODULE QMFM    *DECK INITFMM
      SUBROUTINE INITFMM(NCXYZ,CXYZ,INDX,INDX2,INDX3,IYP,IBS,
     *                   MAXWS,IDXWS,IDXIJK,IPNTR,ITBL,IYZTBL,
     *                   NTBOX,IDXBOX,MBOX,LEBOX,NUMWS,IYZPNT,NSBOX,
     *                   NTMPL,F,G,CLM,FLM,ZLL,TMPGPS,TMPGPL,MAXNYP,
     *                   NFTPL,NFTPLT)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL QFMM,QOPS,GOPARR,DSKWRK,MASWRK
      COMPLEX*16 TMPGPS,TMPGPL
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /QMFM  / SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *                ITERMS,QOPS,ISCUT
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
C
      DIMENSION CXYZ(NCXYZ,3),INDX(NCXYZ),INDX2(NCXYZ),INDX3(NCXYZ),
     *          IDXWS(NCXYZ),IDXIJK(NCXYZ,3),IYP(NCXYZ),IBS(NCXYZ,4),
     *          IPNTR(2**NS,3),
     *          ITBL(2**(NS+1)-1,2**(NS+1)-1,2**(NS+1)-1),
     *          IYZTBL(2**(NS+1)-1,2**(NS+1)-1,2**(NS+1)-1)
      DIMENSION IDXBOX(3,NTBOX),MBOX(NTBOX),LEBOX(0:NTBOX),
     *          IYZPNT(NTBOX,MAXWS/2),NUMWS(NTBOX,MAXWS/2),NSBOX(20)
      DIMENSION F((NP*(NP+1)*(NP+2)*4/3)+(NP+1)),CLM(-NP:NP),
     *          G((NP*(NP+1)*(NP+2)*4/3)+(NP+1)),FLM(-NP:NP),
     *          ZLL(0:2*NP+1),
     *          TMPGPS((NPGP+1)*(NPGP+2)/2,MAXNYP),
     *          TMPGPL((NP+1)*(NP+2)/2,MAXNYP)
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA CHECK/8HCHECK   /
#else
      CHARACTER*8 :: CHECK_STR
      EQUIVALENCE (CHECK, CHECK_STR)
      DATA CHECK_STR/"CHECK   "/
#endif
C
      CALL DIVIDE(NCXYZ,CXYZ,INDX,IPNTR,ITBL,
     *     NTBOX,IDXBOX,MBOX,LEBOX,
     *     NSBOX,IDXWS,MAXNB,IDXIJK)
      CALL SORTWS(NCXYZ,INDX,INDX2,INDX3,IDXWS,
     *     CXYZ,NTBOX,MBOX,NSBOX,MAXWS,NUMWS,NTMPL)
      CALL CMPTBL(ITBL,IYZTBL,NTBOX,IDXBOX,MBOX,
     *     NSBOX,MAXWS,NUMWS,IYZPNT,NTMPL)
C
C     IF (MASWRK) WRITE(IW,9520) NBOX,NTBOX,NTMPL
C
      IF(EXETYP.EQ.CHECK) GO TO 800
C
C     TRANSLATE MULTIPOLE MOMENTS TO THE CENTER OF EACH BOXES.
C
      IF (ITERMS.NE.2) THEN
         IF (MPMTHD.EQ.0) THEN
            CALL TRANM(NCXYZ,IYP,INDX2,IDXIJK,IBS,
     *                 CXYZ,F,G,FLM,CLM,ZLL,
     *                 MAXNYP,TMPGPS,TMPGPL,NFTPL,NFTPLT)
         ELSEIF (MPMTHD.EQ.1) THEN
            CALL REWRT(NCXYZ,IYP,MAXNYP,TMPGPS,NFTPL,NFTPLT)
         ENDIF
      ENDIF
C
  800 CONTINUE
      IF(MASWRK) WRITE(IW,9530)
      CALL TIMIT(1)
      RETURN
C
C9520 FORMAT(10X,
C    *'NON-EMPTY BOXES IN THE LOWEST LEVEL  : ',9X,I6,
C    */10X,'UPPER BOUND OF NUMBER OF BOXES       : ',9X,I6,
C    */10X,'TOTAL NUMBER OF MULTIPOLES           : ',9X,I6/)
 9530 FORMAT(1X,'.... DONE INITIALIZING QFMM COMPUTATION ....')
      END
C*MODULE QMFM    *DECK IMAXJ
      INTEGER FUNCTION IMAXJ(I,J)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      IMAXJ=I
      IF (J.GT.I) IMAXJ=J
C
      RETURN
      END
C*MODULE QMFM    *DECK DERFINV
      DOUBLE PRECISION FUNCTION DERFINV(Y)
C
C     DERFINV : THIS ROUTINE RETURNS THE INVERSE ERROR FUNCTION OF Y.
C     THE DOMAIN IS -1 <= Y <= 1
C     HOWEVER Y SHOULD NOT BE -1 AND 1, SINCE THEY CORRESPOND POSITIVE
C     AND NEGATIVE INFINITIES.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (A1= 0.886226899D+00,A2=-1.645349621D+00,
     *           A3= 0.914624893D+00,A4=-0.140543331D+00)
      PARAMETER (B1=-2.118377725D+00,B2= 1.442710462D+00,
     *           B3=-0.329097515D+00,B4= 0.012229801D+00)
      PARAMETER (C1=-1.970840454D+00,C2=-1.624906493D+00,
     *           C3= 3.429567803D+00,C4= 1.641345311D+00)
      PARAMETER (D1= 3.543889200D+00,D2= 1.637067800D+00)
      PARAMETER (ONE=1.0D+00,TWO=2.0D+00)
C
      Y0=0.7D+00
      IF (ABS(Y).LE.Y0) THEN
         Z=Y*Y
         DERFINV=((A4*Z+A3)*Z+A2)*Z+A1
         DERFINV=Y*DERFINV/((((B4*Z+B3)*Z+B2)*Z+B1)*Z+ONE)
      ELSEIF ( (Y0.LT.Y).AND.(Y.LT.ONE) ) THEN
         Z=SQRT(-LOG((ONE-Y)/TWO))
         DERFINV=((C4*Z+C3)*Z+C2)*Z+C1
         DERFINV=DERFINV/((D2*Z+D1)*Z+ONE)
      ELSEIF ( (-Y0.GT.Y).AND.(Y.GT.-ONE) ) THEN
         Z=SQRT(-LOG((ONE+Y)/TWO))
         DERFINV=((C4*Z+C3)*Z+C2)*Z+C1
         DERFINV=-DERFINV/((D2*Z+D1)*Z+1)
      ENDIF
C
C     ONE CAN FURTHER INCREASE THE ACCURACY OF THIS ROUTINE BY
C     USING NEWTON-RALPHSON CORRECTION.
C     HOWEVER THAT WOULD MAKE THIS ROUTINE 3 TIMES SLOWER.
C     THE ACCURACY WITHOUT THE CORRECTION IS ABOUT 6 DIGITS.
C
      RETURN
      END
C*MODULE QMFM    *DECK NZXYZ
      SUBROUTINE NZXYZ(NCXYZ,NYP)
C
C     NCXYZ: NUMBER OF NUN-ZERO PRODUCT OF PRIMITIVE GAUSSIANS.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXSH=1000,MXGTOT=5000,MXATM=500)
      PARAMETER (RLN10=2.30258D+00)
C
      LOGICAL IANDJ
C
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
C
      TOL=RLN10*ITOL
C
      NCXYZ=0
      NYP=0
C
C     FIRST SHELL LOOP.
C
      DO II=1,NSHELL
         I=KATOM(II)
         XI=C(1,I)
         YI=C(2,I)
         ZI=C(3,I)
         I1=KSTART(II)
         I2=I1+KNG(II)-1
         MINI=KMIN(II)
         MAXI=KMAX(II)
C
C
C    SECOND SHELL LOOP.
C
         DO JJ=1,II
            J=KATOM(JJ)
            XJ=C(1,J)
            YJ=C(2,J)
            ZJ=C(3,J)
            J1=KSTART(JJ)
            J2=J1+KNG(JJ)-1
            MINJ=KMIN(JJ)
            MAXJ=KMAX(JJ)
            RR=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
            IANDJ=II.EQ.JJ
C
C
C           I PRIMITIVE.
C
            JGMAX=J2
            DO IG=I1,I2
               AI=EX(IG)
               ARRI=AI*RR
C           J PRIMITIVE.
               IF(IANDJ) JGMAX=IG
               DO JG=J1,JGMAX
                  AJ=EX(JG)
                  AA=AI+AJ
                  DUM=AJ*ARRI/AA
                  IF(DUM.GT.TOL) GO TO 6000
C
                  NCXYZ=NCXYZ+1
                  MAX=MAXJ
                  DO I=MINI,MAXI
                     IF(IANDJ)MAX=I
                     DO J=MINJ,MAX
                        NYP=NYP+1
                     ENDDO
                  ENDDO
C
6000           ENDDO
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C*MODULE QMFM    *DECK GENCRD
      SUBROUTINE GENCRD(NXYZ,CXYZ)
C
C     NCXYZ: NUMBER OF NUN-ZERO PRODUCT OF PRIMITIVE GAUSSIANS.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXSH=1000,MXGTOT=5000,MXATM=500)
      PARAMETER (RLN10=2.30258D+00)
C
      LOGICAL IANDJ
C
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      DIMENSION CXYZ(NXYZ,3)
C
      TOL=RLN10*ITOL
C
      NCXYZ=0
C
C     FIRST SHELL LOOP.
C
      DO II=1,NSHELL
         I=KATOM(II)
         XI=C(1,I)
         YI=C(2,I)
         ZI=C(3,I)
         I1=KSTART(II)
         I2=I1+KNG(II)-1
C
C
C    SECOND SHELL LOOP.
C
         DO JJ=1,II
            J=KATOM(JJ)
            XJ=C(1,J)
            YJ=C(2,J)
            ZJ=C(3,J)
            J1=KSTART(JJ)
            J2=J1+KNG(JJ)-1
            RR=(XI-XJ)*(XI-XJ)+(YI-YJ)*(YI-YJ)+(ZI-ZJ)*(ZI-ZJ)
            IANDJ=II.EQ.JJ
C
C
C           I PRIMITIVE.
C
            JGMAX=J2
            DO IG=I1,I2
               AI=EX(IG)
               ARRI=AI*RR
               AXI=AI*XI
               AYI=AI*YI
               AZI=AI*ZI
C           J PRIMITIVE.
               IF(IANDJ) JGMAX=IG
               DO JG=J1,JGMAX
                  AJ=EX(JG)
                  AA=AI+AJ
                  DUM=AJ*ARRI/AA
                  IF(DUM.GT.TOL) GO TO 6000
                  AX=(AXI+AJ*XJ)/AA
                  AY=(AYI+AJ*YJ)/AA
                  AZ=(AZI+AJ*ZJ)/AA
C
                  NCXYZ=NCXYZ+1
                  CXYZ(NCXYZ,1)=AX
                  CXYZ(NCXYZ,2)=AY
                  CXYZ(NCXYZ,3)=AZ
C
6000           ENDDO
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C*MODULE QMFM    *DECK QMPM
      SUBROUTINE QMPM(NFTPL,NCXYZ,CXYZ,IYP,IBS,
     *                ISP,IPP,IDXWS,MAXWS,MAXNYP,IDXSHL,NSH2,NYP)
C
C     THIS ROUTINE GENERATES MULTIPOLE MOMENTS, Y, OF A PRODUCT OF TWO
C     PRIMITIVE GAUSSIAN FUNCTIONS.
C     INITIALLY CARTESIAN MOMENT INTEGRALS ARE EVALUATED AND CONVERTED
C     INTO COMPLEX REGULAR HARMONICS, Y.
C
C     ICXYZ: TWO-DIMENSIONAL ARRAY OF PRIMITIVE GAUSSIAN PAIR INDEX
C           MATRIX.
C     CXYZ: THE CENTER OF PRODUCT OF PRIMITIVE GAUSSIANS.
C     YP  : MULTIPOLE REPRESENTATIONS OF PRODUCT OF PRIMITIVE GAUSSIANS.
C           NOTE IT IS COMPLEX.
C     NCXYZ: NUMBER OF NUN-ZERO PRODUCT OF PRIMITIVE GAUSSIANS. THIS
C            WILL BE REPLACED WITH A NEW ONE IN THIS ROUTINE.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXSH=1000,MXGTOT=5000,MXATM=500)
      PARAMETER (RLN10=2.30258D+00)
      PARAMETER (SQRT3=1.73205080756888D+00,SQRT5=2.23606797749979D+00)
      PARAMETER (ZERO=0.0D+00,ONE=1.0D+00,ONEHALF=1.5D+00,TWO=2.0D+00,
     *           THREE=3.0D+00,FOUR=4.0D+00,SIX=6.0D+00,EIGHT=8.0D+00)
      PARAMETER (F11=0.70710678118655D+00,F20=0.50000000000000D+00,
     *           F21=1.22474487139159D+00,F22=0.61237243569579D+00,
     *           F30=1.00000000000000D+00,F31=0.43301270189222D+00,
     *           F32=1.36930639376292D+00,F33=0.55901699437495D+00,
     *           F40=0.12500000000000D+00,F41=0.55901699437495D+00,
     *           F42=0.39528470752105D+00,F43=1.47901994577490D+00,
     *           F44=0.52291251658380D+00)
      PARAMETER (MAXNP=50)
      COMPLEX*16 TEMP
C
      LOGICAL IANDJ,NORM,DOUBLE,GOPARR,DSKWRK,TMPDSK,MASWRK,QFMM
      LOGICAL QOPS
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /XYZMOM/ TX0,TY0,TZ0,TX1,TY1,TZ1,TX2,TY2,TZ2,TX3,TY3,TZ3,
     *                TX4,TY4,TZ4,T,PXI,PYI,PZI,PXJ,PYJ,PZJ,NI,NJ,CX,
     *                CY,CZ
      COMMON /QMFM  / SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *                ITERMS,QOPS,ISCUT
C
      DIMENSION CXYZ(NCXYZ,3),IYP(NCXYZ),IDXSHL(NSH2+1),
     *          IBS(NCXYZ,4),ISP(NCXYZ,2),
     *          IPP(NCXYZ,2),IDXWS(NCXYZ),TEMP((MAXNP+1)*(MAXNP+2)/2)
      DIMENSION DIJ(225),IJX(225),IJY(225),IJZ(225)
      DIMENSION X0(25),Y0(25),Z0(25)
      DIMENSION X1(25),Y1(25),Z1(25)
      DIMENSION X2(25),Y2(25),Z2(25)
      DIMENSION X3(25),Y3(25),Z3(25)
      DIMENSION X4(25),Y4(25),Z4(25)
      DIMENSION JX(35),JY(35),JZ(35),IX(35),IY(35),IZ(35)
      DATA JX / 0, 1, 0, 0, 2, 0, 0, 1, 1, 0,
     1          3, 0, 0, 2, 2, 1, 0, 1, 0, 1,
     2          4, 0, 0, 3, 3, 1, 0, 1, 0, 2,
     3          2, 0, 2, 1, 1/
      DATA IX / 1, 6, 1, 1,11, 1, 1, 6, 6, 1,
     1         16, 1, 1,11,11, 6, 1, 6, 1, 6,
     2         21, 1, 1,16,16, 6, 1, 6, 1,11,
     3         11, 1,11, 6, 6/
      DATA JY / 0, 0, 1, 0, 0, 2, 0, 1, 0, 1,
     1          0, 3, 0, 1, 0, 2, 2, 0, 1, 1,
     2          0, 4, 0, 1, 0, 3, 3, 0, 1, 2,
     3          0, 2, 1, 2, 1/
      DATA IY / 1, 1, 6, 1, 1,11, 1, 6, 1, 6,
     1          1,16, 1, 6, 1,11,11, 1, 6, 6,
     2          1,21, 1, 6, 1,16,16, 1, 6,11,
     3          1,11, 6,11, 6/
      DATA JZ / 0, 0, 0, 1, 0, 0, 2, 0, 1, 1,
     1          0, 0, 3, 0, 1, 0, 1, 2, 2, 1,
     2          0, 0, 4, 0, 1, 0, 1, 3, 3, 0,
     3          2, 2, 1, 1, 2/
      DATA IZ / 1, 1, 1, 6, 1, 1,11, 1, 6, 6,
     1          1, 1,16, 1, 6, 1, 6,11,11, 6,
     2          1, 1,21, 1, 6, 1, 6,16,16, 1,
     3         11,11, 6, 6,11/
      TOL=RLN10*ITOL
      NORM=NORMF.NE.1.OR.NORMP.NE.1
C
      IF (GOPARR) THEN
          TMPDSK=DSKWRK
          DSKWRK=.TRUE.
      ENDIF
C
C     OPEN SEQUENTIAL FILE TO SAVE THE PRIMITIVE MULTIPOLES OF THE
C     PRODUCT OF TWO PRIMITIVE GAUSSIAN FUNCTIONS.
C     THIS FILE WILL BE USED TO CONSTRUCT THE MULTIPOLES OF EACH BOX OF
C     FMM METHOD.
C
      MAXWS=IWS
C
C     ZEROING OUT
C
      NKP=0
      NYP=0
      MAXNYP=0
      LNPGP=(NPGP+1)*(NPGP+2)/2
      DO ILKP=1,NCXYZ
         IYP(ILKP)=0
      ENDDO
      DO ILKP=1,(NP+1)*(NP+2)/2
         TEMP(ILKP)=ZERO
      ENDDO
C          CAUTION, THE NEXT VARIABLE IS USED AS RUN TIME DIMENSION
C          ITS ZERO SHOULD COME AFTER IYP ZEROING.
      NNCXYZ=NCXYZ
      NCXYZ=0
C
C     COMPUTE BASE EXTENT AND PRINT OUT THE RESULTS.
C     THE FOLLOWING LINE REQUIRES INVERSE ERROR FUNCTION, DERFINV.
C
      BEX=SQRT(2.0D+00)*DERFINV(ONE-DPGD)
      BOXSIZE=SIZE/2**NS
C      IF (MASWRK) WRITE(IW,9000) SIZE,BOXSIZE
C
      IDXSHL(1)=0
      IPSHL=0
      ICSHL=1
C
C     FIRST SHELL LOOP.
C
      DO II=1,NSHELL
         I=KATOM(II)
         XI=C(1,I)
         YI=C(2,I)
         ZI=C(3,I)
         I1=KSTART(II)
         I2=I1+KNG(II)-1
         LIT=KTYPE(II)
         MINI=KMIN(II)
         MAXI=KMAX(II)
C
C
C    SECOND SHELL LOOP.
C
         DO JJ=1,II
            J=KATOM(JJ)
            XJ=C(1,J)
            YJ=C(2,J)
            ZJ=C(3,J)
            J1=KSTART(JJ)
            J2=J1+KNG(JJ)-1
            LJT=KTYPE(JJ)
            MINJ=KMIN(JJ)
            MAXJ=KMAX(JJ)
            RR=(XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
            IANDJ=II.EQ.JJ
            ICSHL=ICSHL+1
C
C           PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS.
C
            IJ=0
            MAX=MAXJ
            DO I=MINI,MAXI
               NX=IX(I)
               NY=IY(I)
               NZ=IZ(I)
               IF(IANDJ) MAX=I
               DO J=MINJ,MAX
                  IJ=IJ+1
                  IJX(IJ)=NX+JX(J)
                  IJY(IJ)=NY+JY(J)
                  IJZ(IJ)=NZ+JZ(J)
               ENDDO
            ENDDO
C
C           I PRIMITIVE.
C
C    IJP : INDEX OF PRIMITIVE PAIR
C
            IJP=0
            JGMAX=J2
            DO IG=I1,I2
               IGP=IG-I1+1
               AI=EX(IG)
               ARRI=AI*RR
               AXI=AI*XI
               AYI=AI*YI
               AZI=AI*ZI
               CSI=CS(IG)
               CPI=CP(IG)
               CDI=CD(IG)
               CFI=CF(IG)
C              J PRIMITIVE.
               IF(IANDJ) JGMAX=IG
               DO JG=J1,JGMAX
                  JGP=JG-J1+1
                  AJ=EX(JG)
                  AA=AI+AJ
                  DUM=AJ*ARRI/AA
                  NKP=NKP+1
                  IF(DUM.GT.TOL) GO TO 6000
                  IJP=IJP+1
                  FAC= EXP(-DUM)
                  CSJ=CS(JG)
                  CPJ=CP(JG)
                  CDJ=CD(JG)
                  CFJ=CF(JG)
                  AX=(AXI+AJ*XJ)/AA
                  AY=(AYI+AJ*YJ)/AA
                  AZ=(AZI+AJ*ZJ)/AA
                  NCXYZ=NCXYZ+1
                  IF(NCXYZ.GT.NNCXYZ) THEN
                     IF(MASWRK) WRITE(IW,*) 'QMPM: PROBLEM WITH NCXYZ'
                     CALL ABRT
                  END IF
                  CXYZ(NCXYZ,1)=AX
                  CXYZ(NCXYZ,2)=AY
                  CXYZ(NCXYZ,3)=AZ
                  IREXT=2*(INT(BEX/(SQRT(AA)*BOXSIZE)))
                  IDXWS(NCXYZ)=IMAXJ(IREXT,IWS)
                  IF (IDXWS(NCXYZ).GT.MAXWS) MAXWS=IDXWS(NCXYZ)
C
C     ALL STANDARD STUFF TO THIS POINT.
C     DENSITY FACTOR.  MORE STANDARD STUFF.
C
                  DOUBLE=IANDJ.AND.IG.NE.JG
                  MAX=MAXJ
                  NN=0
                  DO 310 I=MINI,MAXI
                     GO TO ( 70, 80,180,180, 90,180,180,100,180,180,
     *               110,180,180,120,180,180,180,180,180,130),I
   70                DUM1=CSI*FAC
                     GO TO 180
   80                DUM1=CPI*FAC
                     GO TO 180
   90                DUM1=CDI*FAC
                     GO TO 180
  100                IF(NORM) DUM1=DUM1*SQRT3
                     GO TO 180
  110                DUM1=CFI*FAC
                     GO TO 180
  120                IF(NORM) DUM1=DUM1*SQRT5
                     GO TO 180
  130                IF(NORM) DUM1=DUM1*SQRT3
  180                IF(IANDJ) MAX=I
                     DO 310 J=MINJ,MAX
                        GO TO (190,200,300,300,210,300,300,220,300,300,
     *                  230,300,300,240,300,300,300,300,300,250),J
  190                   DUM2=DUM1*CSJ
                        IF(.NOT.DOUBLE) GO TO 300
                        IF(I.GT.1) GO TO 195
                        DUM2=DUM2+DUM2
                        GO TO 300
  195                   DUM2=DUM2+CSI*CPJ*FAC
                        GO TO 300
  200                   DUM2=DUM1*CPJ
                        IF(DOUBLE) DUM2=DUM2+DUM2
                        GO TO 300
  210                   DUM2=DUM1*CDJ
                        IF(DOUBLE) DUM2=DUM2+DUM2
                        GO TO 300
  220                   IF(NORM) DUM2=DUM2*SQRT3
                        GO TO 300
  230                   DUM2=DUM1*CFJ
                        IF(DOUBLE) DUM2=DUM2+DUM2
                        GO TO 300
  240                   IF(NORM) DUM2=DUM2*SQRT5
                        GO TO 300
  250                   IF(NORM) DUM2=DUM2*SQRT3
  300                   NN=NN+1
  310             DIJ(NN)=DUM2
C
C    MOMENT INTEGRALS.
C
C    WE FIRST TRANSLATE THE CENTER OF THE TWO PRIMITIVE GAUSSIANS
C     W.R.T. THE CENTER OF THE PRODUCT OF THE TWO GAUSSIANS.
C
                  T=SQRT(AA)
                  PXI=XI-AX
                  PYI=YI-AY
                  PZI=ZI-AZ
                  PXJ=XJ-AX
                  PYJ=YJ-AY
                  PZJ=ZJ-AZ
C
C    LOOP OVER EXPANSION POINT(S) FOR THESE PRIMITIVE PRODUCTS.
C
                  CX=ZERO
                  CY=ZERO
                  CZ=ZERO
                  IN=-5
                  DO I=1,LIT
                     IN=IN+5
                     NI=I
                     DO J=1,LJT
                        JN=IN+J
                        NJ=J
C
C     GAUSS-HERMITE QUADRATURE AROUND EXPANSION POINT.
C
                        CALL STNXYZ
                        X0(JN)=TX0/T
                        Y0(JN)=TY0/T
                        Z0(JN)=TZ0/T
                        X1(JN)=TX1/T
                        Y1(JN)=TY1/T
                        Z1(JN)=TZ1/T
                        X2(JN)=TX2/T
                        Y2(JN)=TY2/T
                        Z2(JN)=TZ2/T
                        X3(JN)=TX3/T
                        Y3(JN)=TY3/T
                        Z3(JN)=TZ3/T
                        X4(JN)=TX4/T
                        Y4(JN)=TY4/T
                        Z4(JN)=TZ4/T
                     ENDDO
                  ENDDO
                  MAX=MAXJ
                  NN=0
                  DO I=MINI,MAXI
                     IF(IANDJ)MAX=I
                     DO J=MINJ,MAX
                        NN=NN+1
                        NX=IJX(NN)
                        NY=IJY(NN)
                        NZ=IJZ(NN)
C
                        NYP=NYP+1
C
C     CONTRIBUTIONS TO MOMENTS FROM CURRENT PRIMITIVE PRODUCT.
C
                        FCHG=-DIJ(NN)*X0(NX)*Y0(NY)*Z0(NZ)
                        X=   -DIJ(NN)*X1(NX)*Y0(NY)*Z0(NZ)
                        Y=   -DIJ(NN)*X0(NX)*Y1(NY)*Z0(NZ)
                        Z=   -DIJ(NN)*X0(NX)*Y0(NY)*Z1(NZ)
                        XX=  -DIJ(NN)*X2(NX)*Y0(NY)*Z0(NZ)
                        YY=  -DIJ(NN)*X0(NX)*Y2(NY)*Z0(NZ)
                        ZZ=  -DIJ(NN)*X0(NX)*Y0(NY)*Z2(NZ)
                        XY=  -DIJ(NN)*X1(NX)*Y1(NY)*Z0(NZ)
                        XZ=  -DIJ(NN)*X1(NX)*Y0(NY)*Z1(NZ)
                        YZ=  -DIJ(NN)*X0(NX)*Y1(NY)*Z1(NZ)
                        XXX= -DIJ(NN)*X3(NX)*Y0(NY)*Z0(NZ)
                        YYY= -DIJ(NN)*X0(NX)*Y3(NY)*Z0(NZ)
                        ZZZ= -DIJ(NN)*X0(NX)*Y0(NY)*Z3(NZ)
                        XXY= -DIJ(NN)*X2(NX)*Y1(NY)*Z0(NZ)
                        XXZ= -DIJ(NN)*X2(NX)*Y0(NY)*Z1(NZ)
                        XYY= -DIJ(NN)*X1(NX)*Y2(NY)*Z0(NZ)
                        YYZ= -DIJ(NN)*X0(NX)*Y2(NY)*Z1(NZ)
                        XZZ= -DIJ(NN)*X1(NX)*Y0(NY)*Z2(NZ)
                        YZZ= -DIJ(NN)*X0(NX)*Y1(NY)*Z2(NZ)
                        XYZ= -DIJ(NN)*X1(NX)*Y1(NY)*Z1(NZ)
                        XXXX=-DIJ(NN)*X4(NX)*Y0(NY)*Z0(NZ)
                        YYYY=-DIJ(NN)*X0(NX)*Y4(NY)*Z0(NZ)
                        ZZZZ=-DIJ(NN)*X0(NX)*Y0(NY)*Z4(NZ)
                        XXXY=-DIJ(NN)*X3(NX)*Y1(NY)*Z0(NZ)
                        XXXZ=-DIJ(NN)*X3(NX)*Y0(NY)*Z1(NZ)
                        XXYY=-DIJ(NN)*X2(NX)*Y2(NY)*Z0(NZ)
                        XXYZ=-DIJ(NN)*X2(NX)*Y1(NY)*Z1(NZ)
                        XXZZ=-DIJ(NN)*X2(NX)*Y0(NY)*Z2(NZ)
                        XYYY=-DIJ(NN)*X1(NX)*Y3(NY)*Z0(NZ)
                        XYYZ=-DIJ(NN)*X1(NX)*Y2(NY)*Z1(NZ)
                        XYZZ=-DIJ(NN)*X1(NX)*Y1(NY)*Z2(NZ)
                        XZZZ=-DIJ(NN)*X1(NX)*Y0(NY)*Z3(NZ)
                        YYYZ=-DIJ(NN)*X0(NX)*Y3(NY)*Z1(NZ)
                        YYZZ=-DIJ(NN)*X0(NX)*Y2(NY)*Z2(NZ)
                        YZZZ=-DIJ(NN)*X0(NX)*Y1(NY)*Z3(NZ)
C
                        CHG=FCHG
                        TEMP(1)= CHG
                        TEMP(2)= Z
                        TEMP(3)= F11*DCMPLX(-X,Y)
                        TEMP(4)= F20*(TWO*ZZ-XX-YY)
                        TEMP(5)= F21*DCMPLX(-XZ,YZ)
                        TEMP(6)= F22*DCMPLX(XX-YY,-TWO*XY)
                        TEMP(7)= F30*(ZZZ-ONEHALF*(XXZ+YYZ))
                        TEMP(8)= F31*DCMPLX(XXX+XYY-FOUR*XZZ,
     *                           FOUR*YZZ-XXY-YYY)
                        TEMP(9)= F32*DCMPLX(XXZ-YYZ,-TWO*XYZ)
                        TEMP(10)=F33*DCMPLX(THREE*XYY-XXX,
     *                           THREE*XXY-YYY)
                        TEMP(11)=F40*(THREE*(XXXX+TWO*XXYY+YYYY)
     *                           +EIGHT*(ZZZZ-THREE*XXZZ-THREE*YYZZ))
                        TEMP(12)=F41*DCMPLX(THREE*(XXXZ+XYYZ)
     *                           -FOUR*XZZZ,FOUR*YZZZ
     *                           -THREE*(XXYZ+YYYZ))
                        TEMP(13)=F42*DCMPLX(SIX*(XXZZ-YYZZ)
     *                           -XXXX+YYYY,TWO*(XXXY-SIX*XYZZ
     *                           +XYYY))
                        TEMP(14)=F43*DCMPLX(-XXXZ+THREE*XYYZ,
     *                           THREE*XXYZ-YYYZ)
                        TEMP(15)=F44*DCMPLX(XXXX-SIX*XXYY+YYYY,
     *                           -FOUR*(XXXY-XYYY))
                        CALL SQWRIT(NFTPL,TEMP,LNPGP*2)
                     ENDDO
                  ENDDO
C
C     THE FOLLOWING ARRAYS ARE NEEDED, SINCE QUANTUM CALCULATION
C     REQUIRES SPECIFIC INDICES OF BASIS SETS AND SHELL PAIRS.
C     NCXYZ        : THE NUMBER OF TOTAL MULTIPOLE.
C     IBS(NCXYZ,4) : CONTAINS THE RANGE OF BASIS SET INDICES OF A GIVEN
C                    MULTIPOLE.
C     IPP(NCXYZ,2) : CONTAINS PRIMITIVE INDICES OF A GIVEN MULTIPOLE.
C     ISP(NCXYZ,2) : CONTAINS THE SHELL INDICES OF A GIVEN MULTIPOLE.
C     IYP(NCXYZ)   : CONTAINS THE POINTER OF A GIVEN MULTIPOLE.
C
C                 BASIS INFO
                  IF (NN.GT.MAXNYP) MAXNYP=NN
                  IBS(NCXYZ,1)=KLOC(II)
                  IBS(NCXYZ,2)=KLOC(II)+MAXI-MINI
                  IBS(NCXYZ,3)=KLOC(JJ)
                  IBS(NCXYZ,4)=KLOC(JJ)+MAXJ-MINJ
C                 PRIMITIVE INFO
                  IPP(NCXYZ,1)=IGP
                  IPP(NCXYZ,2)=JGP
C                 SHELL INFO
                  ISP(NCXYZ,1)=II
                  ISP(NCXYZ,2)=JJ
                  IYP(NCXYZ)=NYP
6000           ENDDO
            ENDDO
C           END OF PRIMITIVE GAUSSIAN PAIR
            IPSHL=IPSHL+IJP
            IDXSHL(ICSHL)=IPSHL
         ENDDO
      ENDDO
C      IF (MASWRK) WRITE(IW,9100) NCXYZ,NYP,MAXWS
      IF (GOPARR) DSKWRK=TMPDSK
      RETURN
C9000 FORMAT(
C    *10X,'THE LENGTH OF THE CUBE               : ',F15.10,
C    */10X,'THE LENGTH OF THE SMALLEST CUBE      : ',F15.10)
C9100 FORMAT(10X,
C    *'NUMBER OF PRIMITIVE GAUSSIAN PRODUCT :      ',I10,
C    */10X,'NUMBER OF POINTS                     :      ',I10,
C    */10X,'THE MAXIUM IWS                       :      ',I10)
      END
C*MODULE QMFM    *DECK FORMJK
      SUBROUTINE FORMJK(SCFTYP,IA,DA,FA,DB,FB,GHONDO,
     *                  L2,NINT,NXYZ)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL IANDJ,KANDL,SAME,OUT,UROHF
C
      DIMENSION IA(*),DA(*),FA(*),DB(*),FB(*),GHONDO(*)
C
      PARAMETER (MXAO=2047)
      LOGICAL QFMM,QOPS
      COMMON /QMFM  /SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *       ITERMS,QOPS,ISCUT
C
      COMMON /DFTPAR/ DFTTYP(20),EXENA,EXENB,EXENC,IDFT34,NAUXFUN,
     *                                                    NAUXSHL
      COMMON /GVBWFN/ CICOEF(2,12),F(25),ALPHA(325),BETA(325),NO(10),
     *                NCO,NSETO,NOPEN,NPAIR,NORB,NCONF(MXAO),NHAM
      COMMON /INTDEX/ IJGT(225),IJX(225),IJY(225),IJZ(225),IK(225),
     *                KLGT(225),KLX(225),KLY(225),KLZ(225)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MISC  / IANDJ,KANDL,SAME
      COMMON /SHLNOS/ QQ4,LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJX,IJ,KL,IJKL
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
      DATA HALF /0.5D+00/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RHF,UHF,ROHF/8HRHF     ,8HUHF     ,8HROHF     /
#else
      CHARACTER*8 :: ROHF_STR
      EQUIVALENCE (ROHF, ROHF_STR)
      CHARACTER*8 :: RHF_STR
      EQUIVALENCE (RHF, RHF_STR)
      CHARACTER*8 :: UHF_STR
      EQUIVALENCE (UHF, UHF_STR)
      DATA RHF_STR,UHF_STR,ROHF_STR/"RHF     ","UHF     ","ROHF    " /
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA GVB/8HGVB     /
#else
      CHARACTER*8 :: GVB_STR
      EQUIVALENCE (GVB, GVB_STR)
      DATA GVB_STR/"GVB     "/
#endif
C
C-NEXT STATEMENT IS FOR VARIOUS IBM XLF 3.X AND 5.X COMPILERS-
C
      SAVE IJN,KLN
C
C     ----- FORM FOCK OPERATOR DIRECTLY FROM INTEGRALS -----
C     THIS ROUTINE WAS PIECED TOGETHER FROM QOUT AND HSTAR
C     BY FRANK JENSEN AT ODENSE UNIVERSITY IN MARCH 1990.
C     SCF FUNCTIONS BESIDES RHF ADDED BY MWS IN AUGUST 1991.
C
C     NOTE THAT OFF-DIAGONAL ELEMENTS WILL NEED TO BE HALVED LATER.
C
      HFSCAL=DFTTYP(3)
      UROHF = SCFTYP.EQ.UHF  .OR.  SCFTYP.EQ.ROHF
      CUTINT = CUTOFF/ISCUT
C
      IF(SCFTYP.EQ.GVB) THEN
         NSHL = NHAM
         IF(NCO.GT.0) NSHL=NSHL-1
      END IF
C
      IJN = 0
      JMAX = MAXJ
      DO 360 I = MINI,MAXI
         IF (IANDJ) JMAX = I
         DO 340 J = MINJ,JMAX
            IJN = IJN+1
            N1 = IJGT(IJN)
            LMAX = MAXL
            KLN = 0
            DO 320 K =  MINK,MAXK
               IF (KANDL) LMAX = K
               DO 300 L = MINL,LMAX
                  KLN = KLN+1
                  IF(SAME .AND. KLN.GT.IJN) GO TO 340
                  NN = N1+KLGT(KLN)
                  VAL = GHONDO(NN)
C
                  IF(ABS(VAL).LT.CUTINT) GO TO 300
                  NINT = NINT + 1
C
                  II = LOCI+I
                  JJ = LOCJ+J
                  KK = LOCK+K
                  LL = LOCL+L
                  IF (II .GE. JJ) GO TO 100
                  N = II
                  II = JJ
                  JJ = N
  100             IF (KK .GE. LL) GO TO 120
                  N = KK
                  KK = LL
                  LL = N
  120             IF (II-KK) 140,160,180
  140             N = II
                  II = KK
                  KK = N
                  N = JJ
                  JJ = LL
                  LL = N
                  GO TO 180
  160             IF (JJ .LT. LL) GO TO 140
  180             CONTINUE
C
                  IF(II.EQ.JJ) VAL = VAL*HALF
                  IF(KK.EQ.LL) VAL = VAL*HALF
                  IF(II.EQ.KK  .AND.  JJ.EQ.LL) VAL = VAL*HALF
C
C      WE NOW HAVE EVERYTHING READY, PUT INTEGRALS INTO FOCK MATRIX
C
                  NIJ = IA(II)+JJ
                  NKL = IA(KK)+LL
                  NIK = IA(II)+KK
                  NIL = IA(II)+LL
                  NJK = IA(JJ)+KK
                  NJL = IA(JJ)+LL
                  IF(JJ.LT.KK) NJK = IA(KK)+JJ
                  IF(JJ.LT.LL) NJL = IA(LL)+JJ
C
                  VAL2 = VAL+VAL
                  VAL4 = VAL2+VAL2
C
C       NXYZ DISTINGUISHES CODE FOR RHF OR RHF RESPONSE EQUATIONS
C
C       HFSCAL DISTINGUISHES CODE FOR HF OR DFT
C
                  IF(HFSCAL .EQ. 1.0D+00) THEN
                  IF(SCFTYP.EQ.RHF) THEN
                     IF(NXYZ.EQ.1) THEN
                        FA(NIJ) = FA(NIJ)+VAL4*DA(NKL)
                        FA(NKL) = FA(NKL)+VAL4*DA(NIJ)
                        FA(NIK) = FA(NIK)- VAL*DA(NJL)
                        FA(NIL) = FA(NIL)- VAL*DA(NJK)
                        FA(NJK) = FA(NJK)- VAL*DA(NIL)
                        FA(NJL) = FA(NJL)- VAL*DA(NIK)
                     ELSE
                        NIJ = (NIJ-1)*NXYZ+1
                        NKL = (NKL-1)*NXYZ+1
                        NIK = (NIK-1)*NXYZ+1
                        NIL = (NIL-1)*NXYZ+1
                        NJK = (NJK-1)*NXYZ+1
                        NJL = (NJL-1)*NXYZ+1
                        DO 210 IXYZ=0,NXYZ-1
                          FA(NIJ+IXYZ) = FA(NIJ+IXYZ)+VAL4*DA(NKL+IXYZ)
                          FA(NKL+IXYZ) = FA(NKL+IXYZ)+VAL4*DA(NIJ+IXYZ)
                          FA(NIK+IXYZ) = FA(NIK+IXYZ)- VAL*DA(NJL+IXYZ)
                          FA(NIL+IXYZ) = FA(NIL+IXYZ)- VAL*DA(NJK+IXYZ)
                          FA(NJK+IXYZ) = FA(NJK+IXYZ)- VAL*DA(NIL+IXYZ)
                          FA(NJL+IXYZ) = FA(NJL+IXYZ)- VAL*DA(NIK+IXYZ)
  210                   CONTINUE
                     END IF
                  END IF
C
                  IF(UROHF) THEN
                     DUM = VAL4*(DA(NKL)+DB(NKL))
                     FA(NIJ) = FA(NIJ)+DUM
                     FB(NIJ) = FB(NIJ)+DUM
                     DUM = VAL4*(DA(NIJ)+DB(NIJ))
                     FA(NKL) = FA(NKL)+DUM
                     FB(NKL) = FB(NKL)+DUM
                     FA(NIK) = FA(NIK)-VAL2*DA(NJL)
                     FB(NIK) = FB(NIK)-VAL2*DB(NJL)
                     FA(NIL) = FA(NIL)-VAL2*DA(NJK)
                     FB(NIL) = FB(NIL)-VAL2*DB(NJK)
                     FA(NJK) = FA(NJK)-VAL2*DA(NIL)
                     FB(NJK) = FB(NJK)-VAL2*DB(NIL)
                     FA(NJL) = FA(NJL)-VAL2*DA(NIK)
                     FB(NJL) = FB(NJL)-VAL2*DB(NIK)
                  END IF
C
                  IF(SCFTYP.EQ.GVB) THEN
                     IF(NCO.GT.0) THEN
                        FA(NIJ) = FA(NIJ)+VAL4*DA(NKL)
                        FA(NKL) = FA(NKL)+VAL4*DA(NIJ)
                        FA(NIK) = FA(NIK)-VAL *DA(NJL)
                        FA(NIL) = FA(NIL)-VAL *DA(NJK)
                        FA(NJK) = FA(NJK)-VAL *DA(NIL)
                        FA(NJL) = FA(NJL)-VAL *DA(NIK)
                        IOFF1 = L2
                        IOFF2 = L2
                     ELSE
                        IOFF1 = 0
                        IOFF2 = 0
                     END IF
                     DO 220 IFO = 1,NSHL
                        FA(NIJ+IOFF1)    = FA(NIJ+IOFF1)
     *                              + VAL4*DA(NKL+IOFF2)
                        FA(NKL+IOFF1)    = FA(NKL+IOFF1)
     *                              + VAL4*DA(NIJ+IOFF2)
                        FA(NIK+IOFF1+L2) = FA(NIK+IOFF1+L2)
     *                              + VAL *DA(NJL+IOFF2)
                        FA(NIL+IOFF1+L2) = FA(NIL+IOFF1+L2)
     *                              + VAL *DA(NJK+IOFF2)
                        FA(NJK+IOFF1+L2) = FA(NJK+IOFF1+L2)
     *                              + VAL *DA(NIL+IOFF2)
                        FA(NJL+IOFF1+L2) = FA(NJL+IOFF1+L2)
     *                              + VAL *DA(NIK+IOFF2)
                        IOFF1 = IOFF1+L2+L2
                        IOFF2 = IOFF2+L2
  220                CONTINUE
                  END IF
                  ELSE
C
C                 DFT CASE
C
                  IF(SCFTYP.EQ.RHF) THEN
                     IF(NXYZ.EQ.1) THEN
                        FA(NIJ) = FA(NIJ)+VAL4*DA(NKL)
                        FA(NKL) = FA(NKL)+VAL4*DA(NIJ)
                        FA(NIK) = FA(NIK)- VAL*DA(NJL)*HFSCAL
                        FA(NIL) = FA(NIL)- VAL*DA(NJK)*HFSCAL
                        FA(NJK) = FA(NJK)- VAL*DA(NIL)*HFSCAL
                        FA(NJL) = FA(NJL)- VAL*DA(NIK)*HFSCAL
                     ELSE
                        NIJ = (NIJ-1)*NXYZ+1
                        NKL = (NKL-1)*NXYZ+1
                        NIK = (NIK-1)*NXYZ+1
                        NIL = (NIL-1)*NXYZ+1
                        NJK = (NJK-1)*NXYZ+1
                        NJL = (NJL-1)*NXYZ+1
                        DO 310 IXYZ=0,NXYZ-1
                          FA(NIJ+IXYZ) = FA(NIJ+IXYZ)+VAL4*DA(NKL+IXYZ)
                          FA(NKL+IXYZ) = FA(NKL+IXYZ)+VAL4*DA(NIJ+IXYZ)
                          FA(NIK+IXYZ) = FA(NIK+IXYZ)- VAL*DA(NJL+IXYZ)
     *                                                          *HFSCAL
                          FA(NIL+IXYZ) = FA(NIL+IXYZ)- VAL*DA(NJK+IXYZ)
     *                                                          *HFSCAL
                          FA(NJK+IXYZ) = FA(NJK+IXYZ)- VAL*DA(NIL+IXYZ)
     *                                                          *HFSCAL
                          FA(NJL+IXYZ) = FA(NJL+IXYZ)- VAL*DA(NIK+IXYZ)
     *                                                          *HFSCAL
  310                   CONTINUE
                     END IF
                  END IF
C
                  IF(UROHF) THEN
                     DUM = VAL4*(DA(NKL)+DB(NKL))
                     FA(NIJ) = FA(NIJ)+DUM
                     FB(NIJ) = FB(NIJ)+DUM
                     DUM = VAL4*(DA(NIJ)+DB(NIJ))
                     FA(NKL) = FA(NKL)+DUM
                     FB(NKL) = FB(NKL)+DUM
                     FA(NIK) = FA(NIK)-VAL2*DA(NJL)*HFSCAL
                     FB(NIK) = FB(NIK)-VAL2*DB(NJL)*HFSCAL
                     FA(NIL) = FA(NIL)-VAL2*DA(NJK)*HFSCAL
                     FB(NIL) = FB(NIL)-VAL2*DB(NJK)*HFSCAL
                     FA(NJK) = FA(NJK)-VAL2*DA(NIL)*HFSCAL
                     FB(NJK) = FB(NJK)-VAL2*DB(NIL)*HFSCAL
                     FA(NJL) = FA(NJL)-VAL2*DA(NIK)*HFSCAL
                     FB(NJL) = FB(NJL)-VAL2*DB(NIK)*HFSCAL
                  END IF
C
                  IF(SCFTYP.EQ.GVB) THEN
                    WRITE(IW,*)'DIRECT GVB DFT NOT IMPLEMENTED'
                    CALL ABRT()
C                   I COULDN'T FIGURE OUT THE DO 220 LOOP  :-)
                  END IF
                  END IF
C
  300          CONTINUE
  320       CONTINUE
  340    CONTINUE
  360 CONTINUE
      RETURN
      END
C*MODULE QMFM    *DECK FORMJ
      SUBROUTINE FORMJ(SCFTYP,IA,DA,FA,DB,FB,GHONDO,
     *                  L2,NINT,NXYZ)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL IANDJ,KANDL,SAME,OUT,UROHF
C
      DIMENSION IA(*),DA(*),FA(*),DB(*),FB(*),GHONDO(*)
C
      PARAMETER (MXAO=2047)
C
      COMMON /DFTPAR/ DFTTYP(20),EXENA,EXENB,EXENC,IDFT34,NAUXFUN,
     *                                                    NAUXSHL
      COMMON /GVBWFN/ CICOEF(2,12),F(25),ALPHA(325),BETA(325),NO(10),
     *                NCO,NSETO,NOPEN,NPAIR,NORB,NCONF(MXAO),NHAM
      COMMON /INTDEX/ IJGT(225),IJX(225),IJY(225),IJZ(225),IK(225),
     *                KLGT(225),KLX(225),KLY(225),KLZ(225)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MISC  / IANDJ,KANDL,SAME
      COMMON /SHLNOS/ QQ4,LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJX,IJ,KL,IJKL
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
      DATA HALF /0.5D+00/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RHF,UHF,ROHF/8HRHF     ,8HUHF     ,8HROHF     /
#else
      CHARACTER*8 :: ROHF_STR
      EQUIVALENCE (ROHF, ROHF_STR)
      CHARACTER*8 :: RHF_STR
      EQUIVALENCE (RHF, RHF_STR)
      CHARACTER*8 :: UHF_STR
      EQUIVALENCE (UHF, UHF_STR)
      DATA RHF_STR,UHF_STR,ROHF_STR/"RHF     ","UHF     ","ROHF    " /
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA GVB/8HGVB     /
#else
      CHARACTER*8 :: GVB_STR
      EQUIVALENCE (GVB, GVB_STR)
      DATA GVB_STR/"GVB     "/
#endif
C
C-NEXT STATEMENT IS FOR VARIOUS IBM XLF 3.X AND 5.X COMPILERS-
C
      SAVE IJN,KLN
C
C     ----- FORM FOCK OPERATOR DIRECTLY FROM INTEGRALS -----
C     THIS ROUTINE WAS PIECED TOGETHER FROM QOUT AND HSTAR
C     BY FRANK JENSEN AT ODENSE UNIVERSITY IN MARCH 1990.
C     SCF FUNCTIONS BESIDES RHF ADDED BY MWS IN AUGUST 1991.
C
C     NOTE THAT OFF-DIAGONAL ELEMENTS WILL NEED TO BE HALVED LATER.
C
      HFSCAL=DFTTYP(3)
      UROHF = SCFTYP.EQ.UHF  .OR.  SCFTYP.EQ.ROHF
      CUTINT = CUTOFF
C
      IF(SCFTYP.EQ.GVB) THEN
         NSHL = NHAM
         IF(NCO.GT.0) NSHL=NSHL-1
      END IF
C
      IJN = 0
      JMAX = MAXJ
      DO 360 I = MINI,MAXI
         IF (IANDJ) JMAX = I
         DO 340 J = MINJ,JMAX
            IJN = IJN+1
            N1 = IJGT(IJN)
            LMAX = MAXL
            KLN = 0
            DO 320 K =  MINK,MAXK
               IF (KANDL) LMAX = K
               DO 300 L = MINL,LMAX
                  KLN = KLN+1
                  IF(SAME .AND. KLN.GT.IJN) GO TO 340
                  NN = N1+KLGT(KLN)
                  VAL = GHONDO(NN)
C
                  IF(ABS(VAL).LT.CUTINT) GO TO 300
                  NINT = NINT + 1
C
                  II = LOCI+I
                  JJ = LOCJ+J
                  KK = LOCK+K
                  LL = LOCL+L
                  IF (II .GE. JJ) GO TO 100
                  N = II
                  II = JJ
                  JJ = N
  100             IF (KK .GE. LL) GO TO 120
                  N = KK
                  KK = LL
                  LL = N
  120             IF (II-KK) 140,160,180
  140             N = II
                  II = KK
                  KK = N
                  N = JJ
                  JJ = LL
                  LL = N
                  GO TO 180
  160             IF (JJ .LT. LL) GO TO 140
  180             CONTINUE
C
                  IF(II.EQ.JJ) VAL = VAL*HALF
                  IF(KK.EQ.LL) VAL = VAL*HALF
                  IF(II.EQ.KK  .AND.  JJ.EQ.LL) VAL = VAL*HALF
C
C      WE NOW HAVE EVERYTHING READY, PUT INTEGRALS INTO FOCK MATRIX
C
                  NIJ = IA(II)+JJ
                  NKL = IA(KK)+LL
C
                  VAL2 = VAL+VAL
                  VAL4 = VAL2+VAL2
C
C       NXYZ DISTINGUISHES CODE FOR RHF OR RHF RESPONSE EQUATIONS
C
C       HFSCAL DISTINGUISHES CODE FOR HF OR DFT
C
                  IF(HFSCAL .EQ. 1.0D+00) THEN
                  IF(SCFTYP.EQ.RHF) THEN
                     IF(NXYZ.EQ.1) THEN
                        FA(NIJ) = FA(NIJ)+VAL4*DA(NKL)
                        FA(NKL) = FA(NKL)+VAL4*DA(NIJ)
                     ELSE
                        NIJ = (NIJ-1)*NXYZ+1
                        NKL = (NKL-1)*NXYZ+1
                        DO 210 IXYZ=0,NXYZ-1
                          FA(NIJ+IXYZ) = FA(NIJ+IXYZ)+VAL4*DA(NKL+IXYZ)
                          FA(NKL+IXYZ) = FA(NKL+IXYZ)+VAL4*DA(NIJ+IXYZ)
  210                   CONTINUE
                     END IF
                  END IF
C
                  IF(UROHF) THEN
                     DUM = VAL4*(DA(NKL)+DB(NKL))
                     FA(NIJ) = FA(NIJ)+DUM
                     FB(NIJ) = FB(NIJ)+DUM
                     DUM = VAL4*(DA(NIJ)+DB(NIJ))
                     FA(NKL) = FA(NKL)+DUM
                     FB(NKL) = FB(NKL)+DUM
                  END IF
C
                  IF(SCFTYP.EQ.GVB) THEN
                     IF(NCO.GT.0) THEN
                        FA(NIJ) = FA(NIJ)+VAL4*DA(NKL)
                        FA(NKL) = FA(NKL)+VAL4*DA(NIJ)
                        IOFF1 = L2
                        IOFF2 = L2
                     ELSE
                        IOFF1 = 0
                        IOFF2 = 0
                     END IF
                     DO 220 IFO = 1,NSHL
                        FA(NIJ+IOFF1)    = FA(NIJ+IOFF1)
     *                              + VAL4*DA(NKL+IOFF2)
                        FA(NKL+IOFF1)    = FA(NKL+IOFF1)
     *                              + VAL4*DA(NIJ+IOFF2)
                        IOFF1 = IOFF1+L2+L2
                        IOFF2 = IOFF2+L2
  220                CONTINUE
                  END IF
                  ELSE
C
C                 DFT CASE
C
                  IF(SCFTYP.EQ.RHF) THEN
                     IF(NXYZ.EQ.1) THEN
                        FA(NIJ) = FA(NIJ)+VAL4*DA(NKL)
                        FA(NKL) = FA(NKL)+VAL4*DA(NIJ)
                     ELSE
                        NIJ = (NIJ-1)*NXYZ+1
                        NKL = (NKL-1)*NXYZ+1
                        DO 310 IXYZ=0,NXYZ-1
                          FA(NIJ+IXYZ) = FA(NIJ+IXYZ)+VAL4*DA(NKL+IXYZ)
                          FA(NKL+IXYZ) = FA(NKL+IXYZ)+VAL4*DA(NIJ+IXYZ)
  310                   CONTINUE
                     END IF
                  END IF
C
                  IF(UROHF) THEN
                     DUM = VAL4*(DA(NKL)+DB(NKL))
                     FA(NIJ) = FA(NIJ)+DUM
                     FB(NIJ) = FB(NIJ)+DUM
                     DUM = VAL4*(DA(NIJ)+DB(NIJ))
                     FA(NKL) = FA(NKL)+DUM
                     FB(NKL) = FB(NKL)+DUM
                  END IF
C
                  IF(SCFTYP.EQ.GVB) THEN
                    WRITE(IW,*) 'DIRECT GVB DFT NOT IMPLEMENTED'
                    CALL ABRT()
C                   I COULDN'T FIGURE OUT THE DO 220 LOOP  :-)
                  END IF
                  END IF
C
  300          CONTINUE
  320       CONTINUE
  340    CONTINUE
  360 CONTINUE
      RETURN
      END
C*MODULE QMFM    *DECK FORMK
      SUBROUTINE FORMK(SCFTYP,IEXCH,POPLE,IA,DA,FA,DB,FB,GHONDO,
     *                  L2,NINT,NXYZ)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL IANDJ,KANDL,SAME,OUT,POPLE,UROHF
C
      DIMENSION IA(*),DA(*),FA(*),DB(*),FB(*),GHONDO(*)
      DIMENSION IBPOP(4,4)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXAO=2047)
C
      LOGICAL QFMM,QOPS
      COMMON /QMFM  /SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *       ITERMS,QOPS,ISCUT
C
      COMMON /DFTPAR/ DFTTYP(20),EXENA,EXENB,EXENC,IDFT34,NAUXFUN,
     *                                                    NAUXSHL
      COMMON /FLIPS / IB(4,3)
      COMMON /GOUT  / GPOPLE(768),NORG
      COMMON /GVBWFN/ CICOEF(2,12),F(25),ALPHA(325),BETA(325),NO(10),
     *                NCO,NSETO,NOPEN,NPAIR,NORB,NCONF(MXAO),NHAM
      COMMON /INTDEX/ IJGT(225),IJX(225),IJY(225),IJZ(225),IK(225),
     *                KLGT(225),KLX(225),KLY(225),KLZ(225)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MISC  / IANDJ,KANDL,SAME
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /SHLG70/ ISH,JSH,KSH,LSH,IJKLXX(4)
      COMMON /SHLNOS/ QQ4,LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJX,IJ,KL,IJKL
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
      DATA IBPOP/0,0,0,0,64,16,4,1,128,32,8,2,192,48,12,3/
      DATA HALF /0.5D+00/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RHF,UHF,ROHF/8HRHF     ,8HUHF     ,8HROHF     /
#else
      CHARACTER*8 :: ROHF_STR
      EQUIVALENCE (ROHF, ROHF_STR)
      CHARACTER*8 :: RHF_STR
      EQUIVALENCE (RHF, RHF_STR)
      CHARACTER*8 :: UHF_STR
      EQUIVALENCE (UHF, UHF_STR)
      DATA RHF_STR,UHF_STR,ROHF_STR/"RHF     ","UHF     ","ROHF    " /
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA GVB/8HGVB     /
#else
      CHARACTER*8 :: GVB_STR
      EQUIVALENCE (GVB, GVB_STR)
      DATA GVB_STR/"GVB     "/
#endif
C
C-NEXT STATEMENT IS FOR VARIOUS IBM XLF 3.X AND 5.X COMPILERS-
C
      SAVE IJN,KLN,IBB,JBB,KBB,LBB
C
C     ----- FORM FOCK OPERATOR DIRECTLY FROM INTEGRALS -----
C     THIS ROUTINE WAS PIECED TOGETHER FROM QOUT AND HSTAR
C     BY FRANK JENSEN AT ODENSE UNIVERSITY IN MARCH 1990.
C     SCF FUNCTIONS BESIDES RHF ADDED BY MWS IN AUGUST 1991.
C
C     NOTE THAT OFF-DIAGONAL ELEMENTS WILL NEED TO BE HALVED LATER.
C
      HFSCAL=DFTTYP(3)
      UROHF = SCFTYP.EQ.UHF  .OR.  SCFTYP.EQ.ROHF
      CUTINT = CUTOFF/ISCUT
C
      IF(SCFTYP.EQ.GVB) THEN
         NSHL = NHAM
         IF(NCO.GT.0) NSHL=NSHL-1
      END IF
C
      IF(POPLE) THEN
         SAME = ISH .EQ. KSH .AND. JSH .EQ. LSH
         IBB  = IB(1,IEXCH)
         JBB  = IB(2,IEXCH)
         KBB  = IB(3,IEXCH)
         LBB  = IB(4,IEXCH)
         MINI = KMIN(ISH)
         MINJ = KMIN(JSH)
         MINK = KMIN(KSH)
         MINL = KMIN(LSH)
         MAXI = KMAX(ISH)
         MAXJ = KMAX(JSH)
         MAXK = KMAX(KSH)
         MAXL = KMAX(LSH)
         IANDJ = ISH .EQ. JSH
         KANDL = KSH .EQ. LSH
         LOCI = KLOC(ISH)-MINI
         LOCJ = KLOC(JSH)-MINJ
         LOCK = KLOC(KSH)-MINK
         LOCL = KLOC(LSH)-MINL
      END IF
C
      IJN = 0
      JMAX = MAXJ
      DO 360 I = MINI,MAXI
         IF (IANDJ) JMAX = I
         DO 340 J = MINJ,JMAX
            IJN = IJN+1
            IF(POPLE) THEN
               N1 = IBPOP(IBB,I)+IBPOP(JBB,J)+1+NORG
            ELSE
               N1 = IJGT(IJN)
            END IF
            LMAX = MAXL
            KLN = 0
            DO 320 K =  MINK,MAXK
               IF (KANDL) LMAX = K
               DO 300 L = MINL,LMAX
                  KLN = KLN+1
                  IF(SAME .AND. KLN.GT.IJN) GO TO 340
                  IF(POPLE) THEN
                     NN = N1+IBPOP(KBB,K)+IBPOP(LBB,L)
                     VAL = GPOPLE(NN)
                  ELSE
                     NN = N1+KLGT(KLN)
                     VAL = GHONDO(NN)
                  END IF
C
                  IF(ABS(VAL).LT.CUTINT) GO TO 300
                  NINT = NINT + 1
C
                  II = LOCI+I
                  JJ = LOCJ+J
                  KK = LOCK+K
                  LL = LOCL+L
                  IF (II .GE. JJ) GO TO 100
                  N = II
                  II = JJ
                  JJ = N
  100             IF (KK .GE. LL) GO TO 120
                  N = KK
                  KK = LL
                  LL = N
  120             IF (II-KK) 140,160,180
  140             N = II
                  II = KK
                  KK = N
                  N = JJ
                  JJ = LL
                  LL = N
                  GO TO 180
  160             IF (JJ .LT. LL) GO TO 140
  180             CONTINUE
C
                  IF(II.EQ.JJ) VAL = VAL*HALF
                  IF(KK.EQ.LL) VAL = VAL*HALF
                  IF(II.EQ.KK  .AND.  JJ.EQ.LL) VAL = VAL*HALF
C
C      WE NOW HAVE EVERYTHING READY, PUT INTEGRALS INTO FOCK MATRIX
C
                  NIK = IA(II)+KK
                  NIL = IA(II)+LL
                  NJK = IA(JJ)+KK
                  NJL = IA(JJ)+LL
                  IF(JJ.LT.KK) NJK = IA(KK)+JJ
                  IF(JJ.LT.LL) NJL = IA(LL)+JJ
C
                  VAL2 = VAL+VAL
C
C       NXYZ DISTINGUISHES CODE FOR RHF OR RHF RESPONSE EQUATIONS
C
C       HFSCAL DISTINGUISHES CODE FOR HF OR DFT
C
                  IF(HFSCAL .EQ. 1.0D+00) THEN
                  IF(SCFTYP.EQ.RHF) THEN
                     IF(NXYZ.EQ.1) THEN
                        FA(NIK) = FA(NIK)- VAL*DA(NJL)
                        FA(NIL) = FA(NIL)- VAL*DA(NJK)
                        FA(NJK) = FA(NJK)- VAL*DA(NIL)
                        FA(NJL) = FA(NJL)- VAL*DA(NIK)
                    ELSE
                        NIK = (NIK-1)*NXYZ+1
                        NIL = (NIL-1)*NXYZ+1
                        NJK = (NJK-1)*NXYZ+1
                        NJL = (NJL-1)*NXYZ+1
                        DO 210 IXYZ=0,NXYZ-1
                          FA(NIK+IXYZ) = FA(NIK+IXYZ)- VAL*DA(NJL+IXYZ)
                          FA(NIL+IXYZ) = FA(NIL+IXYZ)- VAL*DA(NJK+IXYZ)
                          FA(NJK+IXYZ) = FA(NJK+IXYZ)- VAL*DA(NIL+IXYZ)
                          FA(NJL+IXYZ) = FA(NJL+IXYZ)- VAL*DA(NIK+IXYZ)
  210                   CONTINUE
                     END IF
                  END IF
C
                  IF(UROHF) THEN
                     FA(NIK) = FA(NIK)-VAL2*DA(NJL)
                     FB(NIK) = FB(NIK)-VAL2*DB(NJL)
                     FA(NIL) = FA(NIL)-VAL2*DA(NJK)
                     FB(NIL) = FB(NIL)-VAL2*DB(NJK)
                     FA(NJK) = FA(NJK)-VAL2*DA(NIL)
                     FB(NJK) = FB(NJK)-VAL2*DB(NIL)
                     FA(NJL) = FA(NJL)-VAL2*DA(NIK)
                     FB(NJL) = FB(NJL)-VAL2*DB(NIK)
                  END IF
C
                  IF(SCFTYP.EQ.GVB) THEN
                     IF(NCO.GT.0) THEN
                        FA(NIK) = FA(NIK)-VAL *DA(NJL)
                        FA(NIL) = FA(NIL)-VAL *DA(NJK)
                        FA(NJK) = FA(NJK)-VAL *DA(NIL)
                        FA(NJL) = FA(NJL)-VAL *DA(NIK)
                        IOFF1 = L2
                        IOFF2 = L2
                     ELSE
                        IOFF1 = 0
                        IOFF2 = 0
                     END IF
                     DO 220 IFO = 1,NSHL
                        FA(NIK+IOFF1+L2) = FA(NIK+IOFF1+L2)
     *                              + VAL *DA(NJL+IOFF2)
                        FA(NIL+IOFF1+L2) = FA(NIL+IOFF1+L2)
     *                              + VAL *DA(NJK+IOFF2)
                        FA(NJK+IOFF1+L2) = FA(NJK+IOFF1+L2)
     *                              + VAL *DA(NIL+IOFF2)
                        FA(NJL+IOFF1+L2) = FA(NJL+IOFF1+L2)
     *                              + VAL *DA(NIK+IOFF2)
                        IOFF1 = IOFF1+L2+L2
                        IOFF2 = IOFF2+L2
  220                CONTINUE
                  END IF
                  ELSE
C
C                 DFT CASE
C
                  IF(SCFTYP.EQ.RHF) THEN
                     IF(NXYZ.EQ.1) THEN
                        FA(NIK) = FA(NIK)- VAL*DA(NJL)*HFSCAL
                        FA(NIL) = FA(NIL)- VAL*DA(NJK)*HFSCAL
                        FA(NJK) = FA(NJK)- VAL*DA(NIL)*HFSCAL
                        FA(NJL) = FA(NJL)- VAL*DA(NIK)*HFSCAL
                     ELSE
                        NIK = (NIK-1)*NXYZ+1
                        NIL = (NIL-1)*NXYZ+1
                        NJK = (NJK-1)*NXYZ+1
                        NJL = (NJL-1)*NXYZ+1
                        DO 310 IXYZ=0,NXYZ-1
                          FA(NIK+IXYZ) = FA(NIK+IXYZ)- VAL*DA(NJL+IXYZ)
     *                                                          *HFSCAL
                          FA(NIL+IXYZ) = FA(NIL+IXYZ)- VAL*DA(NJK+IXYZ)
     *                                                          *HFSCAL
                          FA(NJK+IXYZ) = FA(NJK+IXYZ)- VAL*DA(NIL+IXYZ)
     *                                                          *HFSCAL
                          FA(NJL+IXYZ) = FA(NJL+IXYZ)- VAL*DA(NIK+IXYZ)
     *                                                          *HFSCAL
  310                   CONTINUE
                     END IF
                  END IF
C
                  IF(UROHF) THEN
                     FA(NIK) = FA(NIK)-VAL2*DA(NJL)*HFSCAL
                     FB(NIK) = FB(NIK)-VAL2*DB(NJL)*HFSCAL
                     FA(NIL) = FA(NIL)-VAL2*DA(NJK)*HFSCAL
                     FB(NIL) = FB(NIL)-VAL2*DB(NJK)*HFSCAL
                     FA(NJK) = FA(NJK)-VAL2*DA(NIL)*HFSCAL
                     FB(NJK) = FB(NJK)-VAL2*DB(NIL)*HFSCAL
                     FA(NJL) = FA(NJL)-VAL2*DA(NIK)*HFSCAL
                     FB(NJL) = FB(NJL)-VAL2*DB(NIK)*HFSCAL
                  END IF
C
                  IF(SCFTYP.EQ.GVB) THEN
                    WRITE(IW,*)'DIRECT GVB DFT NOT IMPLEMENTED'
                    CALL ABRT()
C                   I COULDN'T FIGURE OUT THE DO 220 LOOP  :-)
                  END IF
                  END IF
C
  300          CONTINUE
  320       CONTINUE
  340    CONTINUE
  360 CONTINUE
      RETURN
      END
C*MODULE QMFM    *DECK QFMMIN
      SUBROUTINE QFMMIN
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (ONE=1.0D+00)
C
      LOGICAL DIRSCF,FDIFF,GOPARR,DSKWRK,MASWRK,OK,QFMM,QOPS
C
      DOUBLE PRECISION METHOD,DISK,SEMI,FULL,TERMS,JMTX,KMTX,JADK
      CHARACTER*1 TAG(5)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /IQMFM / SCLF,ITGERR,IEPS,IDPGD
      COMMON /OPTSCF/ DIRSCF,FDIFF
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /QMFM  / SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *                ITERMS,QOPS,ISCUT
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      PARAMETER (NNAM=13)
      DIMENSION QNAM(NNAM),KQNAM(NNAM)
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA FMM  /8HFMM     /
#else
      CHARACTER*8 :: FMM_STR
      EQUIVALENCE (FMM, FMM_STR)
      DATA FMM_STR/"FMM     "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QNAM /8HNP      ,8HNS      ,8HIWS     ,8HIDPGD   ,8HIEPS    ,
     *8HMETHOD  ,8HNUMRD   ,8HTERMS   ,8HQOPS    ,8HISCUT   ,8HITGERR  ,
     *8HSCLF    ,8HMQOPS   /
#else
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"NP      ","NS      ","IWS     ","IDPGD   ",
     * "IEPS    ",
     * "METHOD  ","NUMRD   ","TERMS   ","QOPS    ","ISCUT   ",
     * "ITGERR  ", "SCLF    ","MQOPS   "/
#endif
      DATA KQNAM/1,1,1,1,1,5,1,5,0,1,1,3,1/
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DISK,SEMI,FULL /8HDISK    ,8HSEMIDRCT,8HFULLDRCT/
#else
      CHARACTER*8 :: FULL_STR
      EQUIVALENCE (FULL, FULL_STR)
      CHARACTER*8 :: DISK_STR
      EQUIVALENCE (DISK, DISK_STR)
      CHARACTER*8 :: SEMI_STR
      EQUIVALENCE (SEMI, SEMI_STR)
      DATA DISK_STR,SEMI_STR,FULL_STR/"DISK    ","SEMIDRCT","FULLDRCT"/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA JMTX,KMTX,JADK /8HCOULOMB ,8HEXCHANGE,8HJANDK   /
#else
      CHARACTER*8 :: JADK_STR
      EQUIVALENCE (JADK, JADK_STR)
      CHARACTER*8 :: JMTX_STR
      EQUIVALENCE (JMTX, JMTX_STR)
      CHARACTER*8 :: KMTX_STR
      EQUIVALENCE (KMTX, KMTX_STR)
      DATA JMTX_STR,KMTX_STR,JADK_STR/"COULOMB ","EXCHANGE","JANDK   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RHF,UHF,ROHF/8HRHF     ,8HUHF     ,8HROHF    /
#else
      CHARACTER*8 :: ROHF_STR
      EQUIVALENCE (ROHF, ROHF_STR)
      CHARACTER*8 :: RHF_STR
      EQUIVALENCE (RHF, RHF_STR)
      CHARACTER*8 :: UHF_STR
      EQUIVALENCE (UHF, UHF_STR)
      DATA RHF_STR,UHF_STR,ROHF_STR/"RHF     ","UHF     ","ROHF    "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RNONE,NONE/8HNONE    ,4HNONE/
#else
      CHARACTER*4 :: NONE_STR
      EQUIVALENCE (NONE, NONE_STR)
      CHARACTER*8 :: RNONE_STR
      EQUIVALENCE (RNONE, RNONE_STR)
      DATA RNONE_STR,NONE_STR/"NONE    ","NONE"/
#endif
      DATA TAG/'S','P','D','F','G'/
C
C     DEFAULT VALUES
C
C     ITGERR : TARGET ERROR OF 10**(-ITGERR) IN HF ENERGY.
C     NP     : THE HIGHEST ORDER OF MULTIPOLE EXPANSION
C     NS     : THE SUBDIVISION LEVEL OF QFMM
C     IWS    : DEFAULT WELL-SEPARATENESS
C     IEPS   : TARGET ERROR IN FMM ENERGY
C     IDPGD  : TARGET ERROR IN GAUSSIAN DISTRIBUTION DUE TO POINT
C              CHARGE APPROX.
C     ISCUT  : DIVISION FACTOR DUE TO QFMM
C     METHOD : MULTIPOLE SAVE OPTION
C     TERMS  : COULOMB ONLY, EXCHANGE ONLY OR BOTH
C     NUMRD  : WHEN METHOD <=1, DETERMINES CACHE OF READING DISK FILE.
C
      ITGERR=7
      NP=15
      NS=2
      IWS=2
      IEPS=9
      IDPGD=9
      ISCUT=5
      METHOD=DISK
      MPMTHD=0
      TERMS=JADK
      NUMRD=1
      SCLF=ONE
      QOPS=.TRUE.
      MQOPS=0
C
C     ----- READ NAMELIST -$FMM -----
C
      JRET = 0
      CALL NAMEIO(IR,JRET,FMM,NNAM,QNAM,KQNAM,
     *            NP,NS,IWS,IDPGD,IEPS,METHOD,NUMRD,TERMS,QOPS,ISCUT,
     *            ITGERR,SCLF,MQOPS,
     *            0,
     *     0,0,0,0,0,   0,0,0,0,0,
     *     0,0,0,0,0,   0,0,0,0,0,   0,0,0,0,0,   0,0,0,0,0,
     *     0,0,0,0,0,   0,0,0,0,0,   0,0,0,0,0,   0,0,0,0,0)
C
      NUMRD=1
C
C     ----- DETERMINE MULTIPOLE SAVE OPTION -----
C
      IF (METHOD.EQ.DISK) MPMTHD=0
      IF (METHOD.EQ.SEMI) MPMTHD=1
      IF (METHOD.EQ.FULL) MPMTHD=2
C     CURRENTLY ONLY SUPPORT METHOD 0
      MPMTHD=0
C
C     ----- DETERMINE THE TERMS TO BE COMPUTED -----
C
      ITERMS=0
      IF (TERMS.EQ.JADK) ITERMS=0
      IF (TERMS.EQ.JMTX) ITERMS=1
      IF (TERMS.EQ.KMTX) ITERMS=2
C
C          ERROR CHECKING
C
      NERR=0
      IF (.NOT.DIRSCF) THEN
         IF (MASWRK) WRITE(IW,9100)
         NERR=NERR+1
      ENDIF
C
C     NPGP RETURN THE HIGHEST ANGULAR MOMENTUM PRESENT IN THE BASIS.
C     NOTE THAT KTYPE=1,2,3,4,5 MEANS S, P(L), D, F, G FUNCTION.
C     CURRENTLY QFMM SUPPORTS ONLY UP TO D.
C
      CALL BASCHK(NPGP)
      IF (NPGP.GE.4) THEN
         IF (MASWRK) WRITE(IW,9200) TAG(NPGP)
         NERR=NERR+1
      ENDIF
      NPGP=NPGP*2
C
C     AND THIS MUST BE AN ORDINARY SCF OR DFT, BUT NOT GVB OR MCSCF
C
      OK = SCFTYP.EQ.RHF  .OR.  SCFTYP.EQ.UHF  .OR.  SCFTYP.EQ.ROHF
      OK = OK .AND. (CITYP.EQ.RNONE  .AND.   CCTYP.EQ.RNONE  .AND.
     *               MPLEVL.EQ.0  .AND.  MPCTYP.EQ.NONE)
      IF(.NOT.OK) THEN
         IF(MASWRK) WRITE(IW,9400)
         NERR=NERR+1
      END IF
      IF(NERR.GT.0) CALL ABRT
C
      CALL INITPRMT(NCXYZ,MQOPS)
      RETURN
C
 9100 FORMAT(1X,'*** ERROR *** IN -QFMMIN-'/,
     *  1X,'QFMM ONLY RUNS IN DIRECT SCF MODE. '/,
     *  1X,'INCLUDE DIRSCF=.T. IN $SCF GROUP.'/)
 9200 FORMAT(/1X,'THE HIGHEST ANGULAR MOMENTUM IS ',A1/,
     *  1X,'QFMM WILL NOT RUN WITH ANGULAR MOMENTUM HIGHER THAN D'/
     *  1X,'FUTURE VERSION WILL REMOVE THIS RESTRICTION'/)
 9400 FORMAT(/1X,'THE SCFTYP FOR QFMM MUST BE RHF, UHF, OR ROHF,'/
     *        1X,'CORRELATION METHODS MPLEVL, CITYP, CCTYP CANNOT',
     *           ' BE USED,'/
     *        1X,'NOR ANY SEMI-EMPIRICAL HAMILTONIAN.')
      END
C*MODULE QMFM    *DECK NEARJ
      SUBROUTINE NEARJ(SCFTYP,DIRSCF,SCHWRZ,NINT,NSCHWZ,L1,L2,
     *                 XINTS,NSH2,GHONDO,MAXG,DDIJ,
     *                 IA,DA,FA,DB,FB,DSH,NXYZ,
     *                 NCXYZ,INDX,IPP,ISP,
     *                 ITSP,ITPP,ITSP2,ITPP2,
     *                 NTBOX,IDXBOX,MBOX,LEBOX,NSBOX,IYZTBL,NBR,
     *                 MAXWS,NUMWS,IYZPNT)
C
C
C       MANY ARGUMENTS ARE OPTIONAL, YOU MUST ALLOCATE STORAGE FOR
C              ALL CALLS: GHONDO, DDIJ, XINTS
C           CONVENTIONAL: BUFP, IX, AND POSSIBLY BUFK
C             DIRECT SCF: IA, DSH, DA, FA, AND POSSIBLY DB, FB
C             DIRECT NLO: DNLO, FNLO
C  DIRECT TRANSFORMATION: BUFP, IX
C     RESPONSE EQUATIONS: MUST DEFINE NXYZ.NE.1
C     THIS ROUTINE FORMS NEAR FIELD J MATRIX (COULOMB POTENTIAL).
C     FA : ALPHA FOCK MATRIX.
C
C     YP : COMPLEX[(NP+1)*(NP+2)/2 X NZ]
C
C     C. H. CHOI DEC 1999
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL DIRSCF
      LOGICAL SCHWRZ,TMPDSK,GOPARR,DSKWRK,MASWRK
      LOGICAL QFMM,QOPS,OK
C
      DIMENSION IDXBOX(3,NTBOX),MBOX(NTBOX),LEBOX(0:NTBOX),NSBOX(20),
     *          NUMWS(NTBOX,MAXWS/2),IYZPNT(NTBOX,MAXWS/2)
      DIMENSION XINTS(NSH2),GHONDO(MAXG),IA(L1),DA(L2),FA(L2),
     *          DB(L2),FB(L2),DSH(NSH2),DDIJ(*)
      DIMENSION INDX(NCXYZ),
     *          IPP(NCXYZ,2),ISP(NCXYZ,2),
     *          ITPP(NCXYZ,2),ITSP(NCXYZ,2),
     *          ITPP2(NCXYZ,2),ITSP2(NCXYZ,2),
     *          IYZTBL(2**(NS+1)-1,2**(NS+1)-1,2**(NS+1)-1),
     *          NBR((MAXWS*2+1)**3)
C
      COMMON /DFGRID/ DFTTHR,SW0,NDFTFG,NRAD,NTHE,NPHI,NRAD0,NTHE0,NPHI0
      COMMON /DFTPAR/ DFTTYP(20),EXENA,EXENB,EXENC,IDFT34,NAUXFUN,
     *                                                    NAUXSHL
      COMMON /FMCOM / X(1)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /QMFM  / SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *                ITERMS,QOPS,ISCUT
C
      NEXT = -1
      MINE = -1
      IF (GOPARR) THEN
         TMPDSK=DSKWRK
         DSKWRK=.TRUE.
      ENDIF
C
C     CHECK FOR DFT
C
      ITEMP=ITERMS
      OK =  NDFTFG.EQ.1 .OR. DFTTYP(1).NE.1.0D+00
      IF (OK) THEN
         IF (DFTTYP(3).EQ.0.0D+00) THEN
            ITERMS=1
         ENDIF
      ENDIF
C
      CALL VALFM(LOADFM)
      LPTR     = 1     + LOADFM
      LTBL     = LPTR  + NCXYZ*2/NWDVAR
      LPTR2    = LTBL  + NCXYZ/NWDVAR+2
      LTBL2    = LPTR2 + NCXYZ*2/NWDVAR
      LPPIJ    = LTBL2 + NCXYZ/NWDVAR+2
      LPPKL    = LPPIJ + NCXYZ*2/NWDVAR
      LAST     = LPPKL + NCXYZ*2/NWDVAR
      NEED = LAST - LOADFM
      CALL GETFM(NEED)
C
C     LOOP OVER NON-EMPTY BOXES IN THE LOWEST LEVEL.
C
      DO NON=1,NSBOX(1)
       IF (GOPARR) THEN
          MINE=MINE+1
          IF (MINE.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
          IF (NEXT.NE.MINE) THEN
             GOTO 50
          ENDIF
       ENDIF
C
         I=IDXBOX(1,NON)
         J=IDXBOX(2,NON)
         K=IDXBOX(3,NON)
C
         IXYZ=LEBOX(IYZTBL(I,J,K))-MBOX(IYZTBL(I,J,K))
C
C        DO LOOP OVER DISTRIBUTIONS IN A GIVEN BOX.
C
         DO M=1,MBOX(NON)
            IXYZ=IXYZ+1
            IEND=INDX(IXYZ)
C
C           PUT THE SHELL PAIR AND PRIMITIVE GAUSSIAN PRODUCT
C           INDICES IN THE TEMPORARY SPACE.
C
            ITSP(M,1)=ISP(IEND,1)
            ITSP(M,2)=ISP(IEND,2)
            ITPP(M,1)=IPP(IEND,1)
            ITPP(M,2)=IPP(IEND,2)
         ENDDO
C
C        SORTING OUT ITSP AND ITPP IN ORDER TO OPTIMIZE INTEGRAL.
C
         CALL SORTSP(NCXYZ,M-1,ITSP,ITPP,X(LPTR),
     *        X(LTBL),IXTBL)
         IF (ITERMS.EQ.0) THEN
         CALL COREJK(SCFTYP,DIRSCF,SCHWRZ,NINT,NSCHWZ,L1,L2,
     *               XINTS,NSH2,GHONDO,MAXG,DDIJ,
     *               IA,DA,FA,DB,FB,DSH,NXYZ,NCXYZ,
     *               ITSP,ITPP,IXTBL,X(LTBL),X(LPPIJ),X(LPPKL))
         ELSEIF (ITERMS.EQ.1) THEN
         CALL COREJ(SCFTYP,DIRSCF,SCHWRZ,NINT,NSCHWZ,L1,L2,
     *              XINTS,NSH2,GHONDO,MAXG,DDIJ,
     *              IA,DA,FA,DB,FB,DSH,NXYZ,NCXYZ,
     *              ITSP,ITPP,IXTBL,X(LTBL),X(LPPIJ),X(LPPKL))
         ENDIF
C
 50   ENDDO
C
C     INTEGRALS BETWEEN BOXES
C     NSBOX(1) CONTAINS THE NUMBER OF NON-EMPTY BOXES IN THE LOWEST
C     LEVEL, NS. REMOVE BLANK SPACE.
C
      IBOXS=1
      IBOXE=NSBOX(1)
      MAXSIZE=2**NS
      MINX=MAXSIZE
      MAXX=1
      MINY=MAXSIZE
      MAXY=1
      MINZ=MAXSIZE
      MAXZ=1
C
      DO I=IBOXS,IBOXE
         MX=IDXBOX(1,I)
         MY=IDXBOX(2,I)
         MZ=IDXBOX(3,I)
         IF (MX.LT.MINX) MINX=MX
         IF (MX.GT.MAXX) MAXX=MX
         IF (MY.LT.MINY) MINY=MY
         IF (MY.GT.MAXY) MAXY=MY
         IF (MZ.LT.MINZ) MINZ=MZ
         IF (MZ.GT.MAXZ) MAXZ=MZ
      ENDDO
C
      IF (GOPARR) THEN
         CALL DDI_SYNC(ITAG)
         CALL DDI_DLBRESET
      ENDIF
      NEXT = -1
      MINE = -1
      DO NON=1,NSBOX(1)
C
       IF (GOPARR) THEN
          MINE=MINE+1
          IF (MINE.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
          IF (NEXT.NE.MINE) THEN
             GOTO 100
          ENDIF
       ENDIF
C
C        IDXBOX CONTAINS THE ACTUAL (I,J,K) INDEX OF A GIVEN BOX, NON.
         I=IDXBOX(1,NON)
         J=IDXBOX(2,NON)
         K=IDXBOX(3,NON)
C
C        LOOP OVER THE IWS INDEX OF THE FIRST BOX
C
C        IYZTBL RETURNS ACTUAL SEQUENCE OF A GIVEN BOX (I,J,K)
C        MBOX RETURNS THE NUMBER OF PARTICLES IN THAT BOX
C        LEBOX RETURNS THE CUMULATIVE NUMBER OF PATICLES.
C        SO IXYZ NOW POINTS THE ACTUAL SEQUENCE OF A POINT.
C
         IXYZ=LEBOX(IYZTBL(I,J,K))-MBOX(IYZTBL(I,J,K))
         DO IDXWS=1,MAXWS/2
C
C           NUMWS RETURNS THE NUMBER OF PARTICLE OF BOX, NON WITH
            INPTCL=NUMWS(NON,IDXWS)
            IF ( INPTCL.NE.0 ) THEN
C
C              NOW PUT SHELL PAIR AND PRIMITIVE PAIR INDICES OF I
C              BRANCH PARTICLES WITH IWS=IDXWS*2 INTO TEMPORARY MEMORY.
C
               DO M=1,INPTCL
                  IXYZ=IXYZ+1
                  IPTCL=INDX(IXYZ)
C
C                 PUT THE SHELL PAIR AND PRIMITIVE GAUSSIAN PRODUCT
C                 INDICES IN THE TEMPORARY SPACE.
C
                  ITSP(M,1)=ISP(IPTCL,1)
                  ITSP(M,2)=ISP(IPTCL,2)
                  ITPP(M,1)=IPP(IPTCL,1)
                  ITPP(M,2)=IPP(IPTCL,2)
               ENDDO
C              SORTING OUT ITSP AND ITPP IN ORDER TO OPTIMIZE INTEGRAL.
               CALL SORTSP(NCXYZ,M-1,ITSP,ITPP,X(LPTR),
     *               X(LTBL),IXTBL)
C
C              LOOP OVER THE IWS INDEX OF THE SECOND BOX
C
               DO IDXIT=1,MAXWS/2
                  IJWS=IDXWS+IDXIT
                  CALL GETNBR(I,J,K,IJWS,NS,NTBOX,
     *               NBR,IDX,IYZTBL,IDXIT,MAXWS,IYZPNT,
     *             MINX,MAXX,MINY,MAXY,MINZ,MAXZ)
C
C                 LOOP OVER THE NEIGHBORS
C
                  IF ( IDX.GT.0 ) THEN
                     IST=0
                     DO NN=1,IDX
                        JXYZ=LEBOX(NBR(NN))-MBOX(NBR(NN))
                        DO MWS=1,IDXIT-1
                           JNPTCL=NUMWS(NBR(NN),MWS)
                           JXYZ=JXYZ+JNPTCL
                        ENDDO
                        JNPTCL=NUMWS(NBR(NN),IDXIT)
                        IF ( JNPTCL.NE.0 ) THEN
                           DO M=1,JNPTCL
                              JXYZ=JXYZ+1
                              JPTCL=INDX(JXYZ)
                              IST=IST+1
C                    PUT THE SHELL PAIR AND PRIMITIVE GAUSSIAN PRODUCT
C                    INDICES IN THE TEMPORARY SPACE.
                              ITSP2(IST,1)=ISP(JPTCL,1)
                              ITSP2(IST,2)=ISP(JPTCL,2)
                              ITPP2(IST,1)=IPP(JPTCL,1)
                              ITPP2(IST,2)=IPP(JPTCL,2)
                           ENDDO
                        ENDIF
                     ENDDO
C
C             SORTING OUT ITSP2 AND ITPP2 IN ORDER TO OPTIMIZE INTEGRAL.
C
                     CALL SORTSP(NCXYZ,IST,ITSP2,ITPP2,X(LPTR2),
     *                      X(LTBL2),IXTBL2)
                     IF (ITERMS.EQ.0) THEN
                     CALL NBRJK(SCFTYP,DIRSCF,SCHWRZ,NINT,NSCHWZ,L1,L2,
     *                          XINTS,NSH2,GHONDO,MAXG,DDIJ,
     *                          IA,DA,FA,DB,FB,DSH,NXYZ,NCXYZ,
     *                          ITSP,ITPP,ITSP2,ITPP2,IXTBL,IXTBL2,
     *                          X(LTBL),X(LTBL2),X(LPPIJ),X(LPPKL))
                     ELSEIF (ITERMS.EQ.1) THEN
                     CALL NBRJ(SCFTYP,DIRSCF,SCHWRZ,NINT,NSCHWZ,L1,L2,
     *                         XINTS,NSH2,GHONDO,MAXG,DDIJ,
     *                         IA,DA,FA,DB,FB,DSH,NXYZ,NCXYZ,
     *                         ITSP,ITPP,ITSP2,ITPP2,IXTBL,IXTBL2,
     *                         X(LTBL),X(LTBL2),X(LPPIJ),X(LPPKL))
                      ENDIF
C
                  ENDIF
               ENDDO
C           IF OF INPTCL
            ENDIF
         ENDDO
 100  ENDDO
C
      ITERMS=ITEMP
      CALL RETFM(NEED)
      IF (GOPARR) THEN
         CALL DDI_DLBRESET
         DSKWRK=TMPDSK
      END IF
      RETURN
      END
C*MODULE QMFM    *DECK COREJK
      SUBROUTINE COREJK(SCFTYP,DIRSCF,SCHWRZ,NINT,NSCHWZ,L1,L2,
     *                  XINTS,NSH2,GHONDO,MAXG,DDIJ,
     *                  IA,DA,FA,DB,FB,DSH,NXYZ,NCXYZ,
     *                  ITSP,ITPP,IXTBL,ITBL,IPPIJ,IPPKL)
C
C       MANY ARGUMENTS ARE OPTIONAL, YOU MUST ALLOCATE STORAGE FOR
C              ALL CALLS: GHONDO, DDIJ, XINTS
C           CONVENTIONAL: BUFP, IX, AND POSSIBLY BUFK
C             DIRECT SCF: IA, DSH, DA, FA, AND POSSIBLY DB, FB
C             DIRECT NLO: DNLO, FNLO
C  DIRECT TRANSFORMATION: BUFP, IX
C     RESPONSE EQUATIONS: MUST DEFINE NXYZ.NE.1
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DIRSCF,OUT,SCHWRZ,SCHSKP
C
      DIMENSION XINTS(NSH2),GHONDO(MAXG),IA(L1),DA(L2),FA(L2),
     *          DB(L2),FB(L2),DSH(NSH2),DDIJ(*)
      DIMENSION NORGH(3)
C
      DIMENSION ITPP(NCXYZ,2), ITSP(NCXYZ,2),ITBL(IXTBL)
      DIMENSION IPPIJ(NCXYZ,2), IPPKL(NCXYZ,2)
C
      COMMON /SHLG70/ ISH,JSH,KSH,LSH,IJKLXX(4)
      COMMON /SHLNOS/ QQ4,LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJ,KL,IJKL
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
      PARAMETER (ZERO=0.0D+00)
C
C     ----- TWO-ELECTRON INTEGRALS -----
C     ----- THIS VERSION CAN HANDLE G SHELLS -----
C
      TIM = ZERO
      CALL TSECND(TIM)
C
C           PERHAPS CALL XABI LOPEZ/JOSE UGALDE'S SCREENED INTEGRAL CODE
C
C     ----- INITIALIZATION FOR PARALLEL WORK -----
C
      CALL BASCHK(LMAX)
                    NANGM =  4
      IF(LMAX.EQ.2) NANGM =  6
      IF(LMAX.EQ.3) NANGM = 10
      IF(LMAX.EQ.4) NANGM = 15
      NORGH(1) = 0
      NORGH(2) = NORGH(1) + NANGM**4
      NORGH(3) = NORGH(2) + NANGM**4
C
      SCHSKP=.FALSE.
      DENMAX = ZERO
C
C     ----- I & J SHELL -----
C
      IJSP=0
      Q4=1.0D+00
      QQ4 = Q4
      DO 920 IJSHLL = 1,IXTBL
         IJNUM=ITBL(IJSHLL)
         IJSP=IJSP+IJNUM
         ISH=ITSP(IJSP,1)
         JSH=ITSP(IJSP,2)
C
         CALL GETSHL(1,ISH,JSH,1,1,NCXYZ,ITPP,IPPIJ,IJNUM,IJSP)
         CALL GETIJPP(DDIJ,IJNUM,NCXYZ,IPPIJ)
C
C        K & L SHELL
C
         KLSP=0
         DO 880 KLSHLL=1,IJSHLL
            KLNUM=ITBL(KLSHLL)
            KLSP=KLSP+KLNUM
            KSH=ITSP(KLSP,1)
            LSH=ITSP(KLSP,2)
C
C     ----- (II,JJ//KK,LL) -----
C
C           APPLY THE SCHWARZ INEQUALITY, WHICH IS
C           (II,JJ//KK,LL) .LE.  SQRT( (II,JJ//II,JJ)*(KK,LL//KK,LL) )
C           SEE J.L.WHITTEN, J.CHEM.PHYS. 58,4496-4501(1973)
C
            IF(SCHWRZ) THEN
               IJIJ = (ISH*ISH-ISH)/2 + JSH
               KLKL = (KSH*KSH-KSH)/2 + LSH
               TEST = QQ4*XINTS(IJIJ)*XINTS(KLKL)
               IF(DIRSCF) THEN
                  DENMAX = SCHWDN(DSH,ISH,JSH,KSH,LSH,IA)
                  TEST = TEST*DENMAX
               END IF
               SCHSKP = TEST.LT.CUTOFF
               IF(SCHSKP) NSCHWZ = NSCHWZ + 1
            END IF
C
C           QFMM USES ONLY HONDO CODE
C
            IF(SCHSKP) GO TO 820
C
C        ----- GET INFORMATION ABOUT ISH AND JSH -----
C        ----- FORM PAIRS OF PRIMITIVES FROM ISH AND JSH -----
C        ----- GET INFORMATION ABOUT KSH AND LSH -----
C
            IF(NIJ.EQ.0) GO TO 820
            CALL GETSHL(2,ISH,JSH,KSH,LSH,NCXYZ,ITPP,IPPKL,
     *           KLNUM,KLSP)
            CALL ZQOUT(GHONDO)
C
C        ----- DO INTEGRAL BATCH, SSSS IS A SPECIAL CASE -----
C
            IF(IJKL.EQ.1) THEN
               CALL GETSSSS(GHONDO,DDIJ,NCXYZ,IPPKL,KLNUM)
            ELSE
               CALL GETHNT(GHONDO,DDIJ,NCXYZ,IPPKL,KLNUM)
            END IF
            CALL FORMJK(SCFTYP,IA,DA,FA,DB,FB,GHONDO,
     *                  L2,NINT,NXYZ)
  820       CONTINUE
  880    CONTINUE
  920 CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
      RETURN
      END
C*MODULE QMFM    *DECK COREJ
      SUBROUTINE COREJ(SCFTYP,DIRSCF,SCHWRZ,NINT,NSCHWZ,L1,L2,
     *                 XINTS,NSH2,GHONDO,MAXG,DDIJ,
     *                 IA,DA,FA,DB,FB,DSH,NXYZ,NCXYZ,
     *                 ITSP,ITPP,IXTBL,ITBL,IPPIJ,IPPKL)
C
C       MANY ARGUMENTS ARE OPTIONAL, YOU MUST ALLOCATE STORAGE FOR
C              ALL CALLS: GHONDO, DDIJ, XINTS
C           CONVENTIONAL: BUFP, IX, AND POSSIBLY BUFK
C             DIRECT SCF: IA, DSH, DA, FA, AND POSSIBLY DB, FB
C             DIRECT NLO: DNLO, FNLO
C  DIRECT TRANSFORMATION: BUFP, IX
C     RESPONSE EQUATIONS: MUST DEFINE NXYZ.NE.1
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DIRSCF,OUT,SCHWRZ,SCHSKP
C
      DIMENSION XINTS(NSH2),GHONDO(MAXG),IA(L1),DA(L2),FA(L2),
     *          DB(L2),FB(L2),DSH(NSH2),DDIJ(*)
      DIMENSION NORGH(3)
C
      DIMENSION ITPP(NCXYZ,2), ITSP(NCXYZ,2),ITBL(IXTBL)
      DIMENSION IPPIJ(NCXYZ,2), IPPKL(NCXYZ,2)
C
      COMMON /SHLG70/ ISH,JSH,KSH,LSH,IJKLXX(4)
      COMMON /SHLNOS/ QQ4,LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJ,KL,IJKL
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
      PARAMETER (ZERO=0.0D+00)
C
C     ----- TWO-ELECTRON INTEGRALS -----
C     ----- THIS VERSION CAN HANDLE G SHELLS -----
C
      TIM = ZERO
      CALL TSECND(TIM)
C
C           PERHAPS CALL XABI LOPEZ/JOSE UGALDE'S SCREENED INTEGRAL CODE
C
C     ----- INITIALIZATION FOR PARALLEL WORK -----
C
      CALL BASCHK(LMAX)
                    NANGM =  4
      IF(LMAX.EQ.2) NANGM =  6
      IF(LMAX.EQ.3) NANGM = 10
      IF(LMAX.EQ.4) NANGM = 15
      NORGH(1) = 0
      NORGH(2) = NORGH(1) + NANGM**4
      NORGH(3) = NORGH(2) + NANGM**4
C
      SCHSKP=.FALSE.
      DENMAX = ZERO
C
C     ----- I & J SHELL -----
C
      IJSP=0
      Q4=1.0D+00
      QQ4 = Q4
      DO 920 IJSHLL = 1,IXTBL
         IJNUM=ITBL(IJSHLL)
         IJSP=IJSP+IJNUM
         ISH=ITSP(IJSP,1)
         JSH=ITSP(IJSP,2)
         CALL GETSHL(1,ISH,JSH,1,1,NCXYZ,ITPP,IPPIJ,IJNUM,IJSP)
         CALL GETIJPP(DDIJ,IJNUM,NCXYZ,IPPIJ)
C
C        K & L SHELL
C
         KLSP=0
         DO 880 KLSHLL=1,IJSHLL
            KLNUM=ITBL(KLSHLL)
            KLSP=KLSP+KLNUM
            KSH=ITSP(KLSP,1)
            LSH=ITSP(KLSP,2)
C
C     ----- (II,JJ//KK,LL) -----
C
C           APPLY THE SCHWARZ INEQUALITY, WHICH IS
C           (II,JJ//KK,LL) .LE.  SQRT( (II,JJ//II,JJ)*(KK,LL//KK,LL) )
C           SEE J.L.WHITTEN, J.CHEM.PHYS. 58,4496-4501(1973)
C
            IF(SCHWRZ) THEN
               IJIJ = (ISH*ISH-ISH)/2 + JSH
               KLKL = (KSH*KSH-KSH)/2 + LSH
               TEST = QQ4*XINTS(IJIJ)*XINTS(KLKL)
               IF(DIRSCF) THEN
                  DENMAX = SCHWDN(DSH,ISH,JSH,KSH,LSH,IA)
                  TEST = TEST*DENMAX
               END IF
               SCHSKP = TEST.LT.CUTOFF
               IF(SCHSKP) NSCHWZ = NSCHWZ + 1
            END IF
C
C           QFMM USES ONLY HONDO CODE
C
            IF(SCHSKP) GO TO 820
C
C        ----- GET INFORMATION ABOUT ISH AND JSH -----
C        ----- FORM PAIRS OF PRIMITIVES FROM ISH AND JSH -----
C        ----- GET INFORMATION ABOUT KSH AND LSH -----
C
            IF(NIJ.EQ.0) GO TO 820
            CALL GETSHL(2,ISH,JSH,KSH,LSH,NCXYZ,ITPP,IPPKL,
     *           KLNUM,KLSP)
            CALL ZQOUT(GHONDO)
C
C        ----- DO INTEGRAL BATCH, SSSS IS A SPECIAL CASE -----
C
            IF(IJKL.EQ.1) THEN
               CALL GETSSSS(GHONDO,DDIJ,NCXYZ,IPPKL,KLNUM)
            ELSE
               CALL GETHNT(GHONDO,DDIJ,NCXYZ,IPPKL,KLNUM)
            END IF
            CALL FORMJ(SCFTYP,IA,DA,FA,DB,FB,GHONDO,
     *                 L2,NINT,NXYZ)
  820       CONTINUE
  880    CONTINUE
  920 CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
      RETURN
      END
C*MODULE QMFM    *DECK NBRJK
      SUBROUTINE NBRJK(SCFTYP,DIRSCF,SCHWRZ,NINT,NSCHWZ,L1,L2,
     *                 XINTS,NSH2,GHONDO,MAXG,DDIJ,
     *                 IA,DA,FA,DB,FB,DSH,NXYZ,
     *                 NCXYZ,ITSP,ITPP,ITSP2,ITPP2,IXTBL,
     *                 IXTBL2,ITBL,ITBL2,IPPIJ,IPPKL)
C
C       MANY ARGUMENTS ARE OPTIONAL, YOU MUST ALLOCATE STORAGE FOR
C              ALL CALLS: GHONDO, DDIJ, XINTS
C           CONVENTIONAL: BUFP, IX, AND POSSIBLY BUFK
C             DIRECT SCF: IA, DSH, DA, FA, AND POSSIBLY DB, FB
C             DIRECT NLO: DNLO, FNLO
C     DIRECT TRANSFORMATION: BUFP, IX
C     RESPONSE EQUATIONS: MUST DEFINE NXYZ.NE.1
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DIRSCF,OUT,SCHWRZ,SCHSKP
C
      DIMENSION XINTS(NSH2),GHONDO(MAXG),IA(L1),DA(L2),FA(L2),
     *          DB(L2),FB(L2),DSH(NSH2),DDIJ(*)
      DIMENSION NORGH(3)
C
      DIMENSION ITPP(NCXYZ,2), ITSP(NCXYZ,2),ITBL(IXTBL)
      DIMENSION ITPP2(NCXYZ,2), ITSP2(NCXYZ,2) , ITBL2(IXTBL2)
      DIMENSION IPPIJ(NCXYZ,2), IPPKL(NCXYZ,2)
C
      COMMON /SHLG70/ ISH,JSH,KSH,LSH,IJKLXX(4)
      COMMON /SHLNOS/ QQ4,LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJ,KL,IJKL
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
      PARAMETER (ZERO=0.0D+00)
C
C     ----- TWO-ELECTRON INTEGRALS -----
C     ----- THIS VERSION CAN HANDLE G SHELLS -----
C
      TIM = ZERO
      CALL TSECND(TIM)
C
      CALL BASCHK(LMAX)
                    NANGM =  4
      IF(LMAX.EQ.2) NANGM =  6
      IF(LMAX.EQ.3) NANGM = 10
      IF(LMAX.EQ.4) NANGM = 15
      NORGH(1) = 0
      NORGH(2) = NORGH(1) + NANGM**4
      NORGH(3) = NORGH(2) + NANGM**4
C
      SCHSKP=.FALSE.
      DENMAX = ZERO
C
C
C     ----- I & J SHELL -----
C
      IJSP=0
      Q4=1.0D+00
      QQ4 = Q4
      DO 920 IJSHLL = 1,IXTBL
         IJNUM=ITBL(IJSHLL)
         IJSP=IJSP+IJNUM
         ISH=ITSP(IJSP,1)
         JSH=ITSP(IJSP,2)
         KLSP=0
         CALL GETSHL(1,ISH,JSH,1,1,NCXYZ,ITPP,IPPIJ,IJNUM,IJSP)
         CALL GETIJPP(DDIJ,IJNUM,NCXYZ,IPPIJ)
         DO 880 KLSHLL=1,IXTBL2
            KLNUM=ITBL2(KLSHLL)
            KLSP=KLSP+KLNUM
            KSH=ITSP2(KLSP,1)
            LSH=ITSP2(KLSP,2)
C
C        ----- COMPUTE TWO-ELECTRON INTEGRALS ----
C
C
C           APPLY THE SCHWARZ INEQUALITY, WHICH IS
C           (II,JJ//KK,LL) .LE.  SQRT( (II,JJ//II,JJ)*(KK,LL//KK,LL) )
C           SEE J.L.WHITTEN, J.CHEM.PHYS. 58,4496-4501(1973)
C
            IF(SCHWRZ) THEN
               IJIJ = (ISH*ISH-ISH)/2 + JSH
               KLKL = (KSH*KSH-KSH)/2 + LSH
               TEST = QQ4*XINTS(IJIJ)*XINTS(KLKL)
               IF(DIRSCF) THEN
                  DENMAX = SCHWDN(DSH,ISH,JSH,KSH,LSH,IA)
                  TEST = TEST*DENMAX
               END IF
               SCHSKP = TEST.LT.CUTOFF
               IF(SCHSKP) NSCHWZ = NSCHWZ + 1
            END IF
C
C           QFMM ONLY USES HONDO INTEGRAL
C
            IF(SCHSKP) GO TO 880
C
            IF(NIJ.EQ.0) GO TO 880
            CALL GETSHL(2,ISH,JSH,KSH,LSH,NCXYZ,ITPP2,IPPKL,
     *              KLNUM,KLSP)
            CALL ZQOUT(GHONDO)
C
C           ----- DO INTEGRAL BATCH, SSSS IS A SPECIAL CASE -----
C
            IF(IJKL.EQ.1) THEN
               CALL GETSSSS(GHONDO,DDIJ,NCXYZ,IPPKL,KLNUM)
            ELSE
               CALL GETHNT(GHONDO,DDIJ,NCXYZ,IPPKL,KLNUM)
            END IF
            CALL FORMJK(SCFTYP,IA,DA,FA,DB,FB,GHONDO,
     *                  L2,NINT,NXYZ)
C
C           ----- IF ISH=KSH AND JSH=LSH, COMPUTE -----
C
            IF ( (ISH.EQ.KSH).AND.(JSH.EQ.LSH) ) THEN
              CALL GETSHL(1,ISH,JSH,KSH,LSH,NCXYZ,ITPP2,IPPKL,
     *              KLNUM,KLSP)
              CALL GETIJPP(DDIJ,KLNUM,NCXYZ,IPPKL)
              IF(NIJ.EQ.0) GO TO 880
              CALL GETSHL(2,ISH,JSH,KSH,LSH,NCXYZ,ITPP,IPPIJ,
     *              IJNUM,IJSP)
              CALL ZQOUT(GHONDO)
              IF(IJKL.EQ.1) THEN
               CALL GETSSSS(GHONDO,DDIJ,NCXYZ,IPPIJ,IJNUM)
              ELSE
               CALL GETHNT(GHONDO,DDIJ,NCXYZ,IPPIJ,IJNUM)
              END IF
              CALL FORMJK(SCFTYP,IA,DA,FA,DB,FB,GHONDO,
     *                    L2,NINT,NXYZ)
C
              CALL GETSHL(1,ISH,JSH,KSH,LSH,NCXYZ,ITPP,IPPIJ,
     *              IJNUM,IJSP)
              CALL GETIJPP(DDIJ,IJNUM,NCXYZ,IPPIJ)
            ENDIF
C
  880    CONTINUE
  920 CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
      RETURN
      END
C*MODULE QMFM    *DECK NBRJ
      SUBROUTINE NBRJ(SCFTYP,DIRSCF,SCHWRZ,NINT,NSCHWZ,L1,L2,
     *                XINTS,NSH2,GHONDO,MAXG,DDIJ,
     *                IA,DA,FA,DB,FB,DSH,NXYZ,
     *                NCXYZ,ITSP,ITPP,ITSP2,ITPP2,IXTBL,
     *                IXTBL2,ITBL,ITBL2,IPPIJ,IPPKL)
C
C       MANY ARGUMENTS ARE OPTIONAL, YOU MUST ALLOCATE STORAGE FOR
C              ALL CALLS: GHONDO, DDIJ, XINTS
C           CONVENTIONAL: BUFP, IX, AND POSSIBLY BUFK
C             DIRECT SCF: IA, DSH, DA, FA, AND POSSIBLY DB, FB
C             DIRECT NLO: DNLO, FNLO
C     DIRECT TRANSFORMATION: BUFP, IX
C     RESPONSE EQUATIONS: MUST DEFINE NXYZ.NE.1
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL DIRSCF,OUT,SCHWRZ,SCHSKP
C
      DIMENSION XINTS(NSH2),GHONDO(MAXG),IA(L1),DA(L2),FA(L2),
     *          DB(L2),FB(L2),DSH(NSH2),DDIJ(*)
      DIMENSION NORGH(3)
C
      DIMENSION ITPP(NCXYZ,2), ITSP(NCXYZ,2),ITBL(IXTBL)
      DIMENSION ITPP2(NCXYZ,2), ITSP2(NCXYZ,2) , ITBL2(IXTBL2)
      DIMENSION IPPIJ(NCXYZ,2), IPPKL(NCXYZ,2)
C
      COMMON /SHLG70/ ISH,JSH,KSH,LSH,IJKLXX(4)
      COMMON /SHLNOS/ QQ4,LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJ,KL,IJKL
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
      PARAMETER (ZERO=0.0D+00)
C
C     ----- TWO-ELECTRON INTEGRALS -----
C     ----- THIS VERSION CAN HANDLE G SHELLS -----
C
      TIM = ZERO
      CALL TSECND(TIM)
C
      CALL BASCHK(LMAX)
                    NANGM =  4
      IF(LMAX.EQ.2) NANGM =  6
      IF(LMAX.EQ.3) NANGM = 10
      IF(LMAX.EQ.4) NANGM = 15
      NORGH(1) = 0
      NORGH(2) = NORGH(1) + NANGM**4
      NORGH(3) = NORGH(2) + NANGM**4
C
      SCHSKP=.FALSE.
      DENMAX = ZERO
C
C
C     ----- I & J SHELL -----
C
      IJSP=0
      Q4=1.0D+00
      QQ4 = Q4
      DO 920 IJSHLL = 1,IXTBL
         IJNUM=ITBL(IJSHLL)
         IJSP=IJSP+IJNUM
         ISH=ITSP(IJSP,1)
         JSH=ITSP(IJSP,2)
         KLSP=0
         CALL GETSHL(1,ISH,JSH,1,1,NCXYZ,ITPP,IPPIJ,IJNUM,IJSP)
         CALL GETIJPP(DDIJ,IJNUM,NCXYZ,IPPIJ)
         DO 880 KLSHLL=1,IXTBL2
            KLNUM=ITBL2(KLSHLL)
            KLSP=KLSP+KLNUM
            KSH=ITSP2(KLSP,1)
            LSH=ITSP2(KLSP,2)
C
C        ----- COMPUTE TWO-ELECTRON INTEGRALS ----
C
C
C           APPLY THE SCHWARZ INEQUALITY, WHICH IS
C           (II,JJ//KK,LL) .LE.  SQRT( (II,JJ//II,JJ)*(KK,LL//KK,LL) )
C           SEE J.L.WHITTEN, J.CHEM.PHYS. 58,4496-4501(1973)
C
            IF(SCHWRZ) THEN
               IJIJ = (ISH*ISH-ISH)/2 + JSH
               KLKL = (KSH*KSH-KSH)/2 + LSH
               TEST = QQ4*XINTS(IJIJ)*XINTS(KLKL)
               IF(DIRSCF) THEN
                  DENMAX = SCHWDN(DSH,ISH,JSH,KSH,LSH,IA)
                  TEST = TEST*DENMAX
               END IF
               SCHSKP = TEST.LT.CUTOFF
               IF(SCHSKP) NSCHWZ = NSCHWZ + 1
            END IF
C
C           QFMM ONLY USES HONDO INTEGRAL
C
            IF(SCHSKP) GO TO 880
C
            IF(NIJ.EQ.0) GO TO 880
            CALL GETSHL(2,ISH,JSH,KSH,LSH,NCXYZ,ITPP2,IPPKL,
     *              KLNUM,KLSP)
            CALL ZQOUT(GHONDO)
C
C           ----- DO INTEGRAL BATCH, SSSS IS A SPECIAL CASE -----
C
            IF(IJKL.EQ.1) THEN
               CALL GETSSSS(GHONDO,DDIJ,NCXYZ,IPPKL,KLNUM)
            ELSE
               CALL GETHNT(GHONDO,DDIJ,NCXYZ,IPPKL,KLNUM)
            END IF
            CALL FORMJ(SCFTYP,IA,DA,FA,DB,FB,GHONDO,
     *                 L2,NINT,NXYZ)
C
C           ----- IF ISH=KSH AND JSH=LSH, COMPUTE -----
C
            IF ( (ISH.EQ.KSH).AND.(JSH.EQ.LSH) ) THEN
              CALL GETSHL(1,ISH,JSH,KSH,LSH,NCXYZ,ITPP2,IPPKL,
     *              KLNUM,KLSP)
              CALL GETIJPP(DDIJ,KLNUM,NCXYZ,IPPKL)
              IF(NIJ.EQ.0) GO TO 880
              CALL GETSHL(2,ISH,JSH,KSH,LSH,NCXYZ,ITPP,IPPIJ,
     *              IJNUM,IJSP)
              CALL ZQOUT(GHONDO)
              IF(IJKL.EQ.1) THEN
               CALL GETSSSS(GHONDO,DDIJ,NCXYZ,IPPIJ,IJNUM)
              ELSE
               CALL GETHNT(GHONDO,DDIJ,NCXYZ,IPPIJ,IJNUM)
              END IF
              CALL FORMJ(SCFTYP,IA,DA,FA,DB,FB,GHONDO,
     *                   L2,NINT,NXYZ)
C
              CALL GETSHL(1,ISH,JSH,KSH,LSH,NCXYZ,ITPP,IPPIJ,
     *              IJNUM,IJSP)
              CALL GETIJPP(DDIJ,IJNUM,NCXYZ,IPPIJ)
            ENDIF
C
  880    CONTINUE
  920 CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
      RETURN
      END
C*MODULE QMFM    *DECK SORTSP
      SUBROUTINE SORTSP(NCXYZ,M,ITSP,ITPP,IPTR,ITBL,IXTBL)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION ITPP(NCXYZ,2),ITSP(NCXYZ,2),IPTR(NCXYZ,2),
     *          ITBL(NCXYZ)
C
      CALL IDIVIDE(NCXYZ,M,ITSP,ITPP,IPTR,ITBL,IXTBL)
C
      RETURN
      END
C*MODULE QMFM    *DECK GETSHL
      SUBROUTINE GETSHL(NELEC,ISH,JSH,KSH,LSH,NCXYZ,
     *                  ITPP,ISPP,ISNUM,ISPNUM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL IANDJ,KANDL,SAME,FIRST
C
      DIMENSION ITPP(NCXYZ,2),ISPP(NCXYZ,2)
      DIMENSION IX(35),IY(35),IZ(35),
     *          JX(35),JY(35),JZ(35),
     *          KX(35),KY(35),KZ(35),
     *          LX(35),LY(35),LZ(35)
C
      PARAMETER (MXSH=1000, MXGSH=30, MXGTOT=5000, MXATM=500)
C
      COMMON /GOUT  / GPOPLE(768),NORG
      COMMON /INTDEX/ IJGT(225),IJX(225),IJY(225),IJZ(225),IK(225),
     *                KLGT(225),KLX(225),KLY(225),KLZ(225)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /MISC  / IANDJ,KANDL,SAME
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /SHLNOS/ QQ4,LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     +                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     +                NIJ,IJ,KL,IJKL
      COMMON /SHLINF/ GA(MXGSH),CSA(MXGSH),CPA(MXGSH),CDA(MXGSH),
     *                CFA(MXGSH),CGA(MXGSH),
     *                GB(MXGSH),CSB(MXGSH),CPB(MXGSH),CDB(MXGSH),
     *                CFB(MXGSH),CGB(MXGSH),
     *                GC(MXGSH),CSC(MXGSH),CPC(MXGSH),CDC(MXGSH),
     *                CFC(MXGSH),CGC(MXGSH),
     *                GD(MXGSH),CSD(MXGSH),CPD(MXGSH),CDD(MXGSH),
     *                CFD(MXGSH),CGD(MXGSH),
     *                AX,AY,AZ,BX,BY,BZ,RAB,CX,CY,CZ,DX,DY,DZ,RCD,
     *                NGA,NGB,NGC,NGD
C
      SAVE FIRST,IGT,JGT,KGT,LGT
C
      DATA LX /   0,  1,  0,  0,  2,  0,  0,  1,  1,  0,
     *            3,  0,  0,  2,  2,  1,  0,  1,  0,  1,
     *            4,  0,  0,  3,  3,  1,  0,  1,  0,  2,
     *            2,  0,  2,  1,  1/
      DATA KX /   0,  5,  0,  0, 10,  0,  0,  5,  5,  0,
     *           15,  0,  0, 10, 10,  5,  0,  5,  0,  5,
     *           20,  0,  0, 15, 15,  5,  0,  5,  0, 10,
     *           10,  0, 10,  5,  5/
      DATA JX /   0, 25,  0,  0, 50,  0,  0, 25, 25,  0,
     *           75,  0,  0, 50, 50, 25,  0, 25,  0, 25,
     *          100,  0,  0, 75, 75, 25,  0, 25,  0, 50,
     *           50,  0, 50, 25, 25/
      DATA IX /   1,126,  1,  1,251,  1,  1,126,126,  1,
     *          376,  1,  1,251,251,126,  1,126,  1,126,
     *          501,  1,  1,376,376,126,  1,126,  1,251,
     *          251,  1,251,126,126/
      DATA LY /   0,  0,  1,  0,  0,  2,  0,  1,  0,  1,
     *            0,  3,  0,  1,  0,  2,  2,  0,  1,  1,
     *            0,  4,  0,  1,  0,  3,  3,  0,  1,  2,
     *            0,  2,  1,  2,  1/
      DATA KY /   0,  0,  5,  0,  0, 10,  0,  5,  0,  5,
     *            0, 15,  0,  5,  0, 10, 10,  0,  5,  5,
     *            0, 20,  0,  5,  0, 15, 15,  0,  5, 10,
     *            0, 10,  5, 10,  5/
      DATA JY /   0,  0, 25,  0,  0, 50,  0, 25,  0, 25,
     *            0, 75,  0, 25,  0, 50, 50,  0, 25, 25,
     *            0,100,  0, 25,  0, 75, 75,  0, 25, 50,
     *            0, 50, 25, 50, 25/
      DATA IY /   1,  1,126,  1,  1,251,  1,126,  1,126,
     *            1,376,  1,126,  1,251,251,  1,126,126,
     *            1,501,  1,126,  1,376,376,  1,126,251,
     *            1,251,126,251,126/
      DATA LZ /   0,  0,  0,  1,  0,  0,  2,  0,  1,  1,
     *            0,  0,  3,  0,  1,  0,  1,  2,  2,  1,
     *            0,  0,  4,  0,  1,  0,  1,  3,  3,  0,
     *            2,  2,  1,  1,  2/
      DATA KZ /   0,  0,  0,  5,  0,  0, 10,  0,  5,  5,
     *            0,  0, 15,  0,  5,  0,  5, 10, 10,  5,
     *            0,  0, 20,  0,  5,  0,  5, 15, 15,  0,
     *           10, 10,  5,  5, 10/
      DATA JZ /   0,  0,  0, 25,  0,  0, 50,  0, 25, 25,
     *            0,  0, 75,  0, 25,  0, 25, 50, 50, 25,
     *            0,  0,100,  0, 25,  0, 25, 75, 75,  0,
     *           50, 50, 25, 25, 50/
      DATA IZ /   1,  1,  1,126,  1,  1,251,  1,126,126,
     *            1,  1,376,  1,126,  1,126,251,251,126,
     *            1,  1,501,  1,126,  1,126,376,376,  1,
     *          251,251,126,126,251/
      DATA FIRST/.TRUE./
C
      IF(FIRST) THEN
         FIRST=.FALSE.
         CALL BASCHK(LMAX)
                       NANGM =  4
         IF(LMAX.EQ.2) NANGM =  6
         IF(LMAX.EQ.3) NANGM = 10
         IF(LMAX.EQ.4) NANGM = 15
         LGT = 1
         KGT = LGT * NANGM
         JGT = KGT * NANGM
         IGT = JGT * NANGM
      END IF
      IST=ISPNUM-ISNUM+1
C
C     PREPARE SHELL INFORMATION/FOR HONDO INTEGRATION
C
      IF(NELEC.EQ.2) GO TO 200
C
C     ----- PERMUTE ISH AND JSH SHELLS, FOR THEIR TYPE
C
      IANDJ = ISH .EQ. JSH
      IF (KTYPE(ISH) .LT. KTYPE(JSH)) THEN
         INU = JSH
         JNU = ISH
         NGTI = JGT
         NGTJ = IGT
         DO IJNUM=IST,ISPNUM
            IJNUMPP=IJNUM-IST+1
            ISPP(IJNUMPP,1)=ITPP(IJNUM,2)
            ISPP(IJNUMPP,2)=ITPP(IJNUM,1)
         ENDDO
      ELSE
         INU = ISH
         JNU = JSH
         NGTI = IGT
         NGTJ = JGT
         DO IJNUM=IST,ISPNUM
            IJNUMPP=IJNUM-IST+1
            ISPP(IJNUMPP,2)=ITPP(IJNUM,2)
            ISPP(IJNUMPP,1)=ITPP(IJNUM,1)
         ENDDO
      END IF
C
C     ----- ISHELL
C
      I = KATOM(INU)
      AX = C(1,I)
      AY = C(2,I)
      AZ = C(3,I)
      I1 = KSTART(INU)
      I2 = I1+KNG(INU)-1
      LIT = KTYPE(INU)
      MINI = KMIN(INU)
      MAXI = KMAX(INU)
      LOCI = KLOC(INU)-MINI
      NGA = 0
      DO 140 I = I1,I2
         NGA = NGA+1
         GA(NGA) = EX(I)
         CSA(NGA) = CS(I)
         CPA(NGA) = CP(I)
         CDA(NGA) = CD(I)
         CFA(NGA) = CF(I)
         CGA(NGA) = CG(I)
  140 CONTINUE
C
C     ----- JSHELL
C
      J = KATOM(JNU)
      BX = C(1,J)
      BY = C(2,J)
      BZ = C(3,J)
      J1 = KSTART(JNU)
      J2 = J1+KNG(JNU)-1
      LJT = KTYPE(JNU)
      MINJ = KMIN(JNU)
      MAXJ = KMAX(JNU)
      LOCJ = KLOC(JNU)-MINJ
      NGB = 0
      DO 160 J = J1,J2
         NGB = NGB+1
         GB(NGB) = EX(J)
         CSB(NGB) = CS(J)
         CPB(NGB) = CP(J)
         CDB(NGB) = CD(J)
         CFB(NGB) = CF(J)
         CGB(NGB) = CG(J)
  160 CONTINUE
      RAB = ((AX-BX)*(AX-BX) + (AY-BY)*(AY-BY) + (AZ-BZ)*(AZ-BZ))
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
      IJ = 0
      JMAX = MAXJ
      DO 190 I = MINI,MAXI
         NX = IX(I)
         NY = IY(I)
         NZ = IZ(I)
         IF (IANDJ) JMAX = I
         DO 180 J = MINJ,JMAX
            IJ = IJ+1
            IJX(IJ) = NX+JX(J)
            IJY(IJ) = NY+JY(J)
            IJZ(IJ) = NZ+JZ(J)
            IJGT(IJ) = NGTI*(I-MINI)+NGTJ*(J-MINJ)+1+NORG
  180    CONTINUE
  190 CONTINUE
      RETURN
C     ******
C
C        K AND L SHELL
C
  200 CONTINUE
      KANDL = KSH .EQ. LSH
      SAME = ISH .EQ. KSH .AND. JSH .EQ. LSH
C
C     ----- PERMUTE KSH AND LSH SHELLS, FOR THEIR TYPE
C
      IF (KTYPE(KSH) .LT. KTYPE(LSH)) THEN
         KNU = LSH
         LNU = KSH
         NGTK = LGT
         NGTL = KGT
         DO IJNUM=IST,ISPNUM
            IJNUMPP=IJNUM-IST+1
            ISPP(IJNUMPP,1)=ITPP(IJNUM,2)
            ISPP(IJNUMPP,2)=ITPP(IJNUM,1)
         ENDDO
      ELSE
         KNU = KSH
         LNU = LSH
         NGTK = KGT
         NGTL = LGT
         DO IJNUM=IST,ISPNUM
            IJNUMPP=IJNUM-IST+1
            ISPP(IJNUMPP,2)=ITPP(IJNUM,2)
            ISPP(IJNUMPP,1)=ITPP(IJNUM,1)
         ENDDO
      END IF
C
C     ----- K SHELL
C
      K = KATOM(KNU)
      CX = C(1,K)
      CY = C(2,K)
      CZ = C(3,K)
      K1 = KSTART(KNU)
      K2 = K1+KNG(KNU)-1
      LKT = KTYPE(KNU)
      MINK = KMIN(KNU)
      MAXK = KMAX(KNU)
      LOCK = KLOC(KNU)-MINK
      NGC = 0
      DO 260 K = K1,K2
         NGC = NGC+1
         GC(NGC) = EX(K)
         CSC(NGC) = CS(K)
         CPC(NGC) = CP(K)
         CDC(NGC) = CD(K)
         CFC(NGC) = CF(K)
         CGC(NGC) = CG(K)
  260 CONTINUE
C
C     ----- LSHELL
C
      L = KATOM(LNU)
      DX = C(1,L)
      DY = C(2,L)
      DZ = C(3,L)
      L1 = KSTART(LNU)
      L2 = L1+KNG(LNU)-1
      LLT = KTYPE(LNU)
      MINL = KMIN(LNU)
      MAXL = KMAX(LNU)
      LOCL = KLOC(LNU)-MINL
      NGD = 0
      DO 280 L = L1,L2
         NGD = NGD+1
         GD(NGD) = EX(L)
         CSD(NGD) = CS(L)
         CPD(NGD) = CP(L)
         CDD(NGD) = CD(L)
         CFD(NGD) = CF(L)
         CGD(NGD) = CG(L)
  280 CONTINUE
      NROOTS = (LIT+LJT+LKT+LLT-2)/2
      RCD = ((CX-DX)*(CX-DX) + (CY-DY)*(CY-DY) + (CZ-DZ)*(CZ-DZ))
C
C     ----- PREPARE INDICES FOR PAIRS OF (K,L) FUNCTIONS
C
      KL = 0
      LMAX = MAXL
      DO 310 K = MINK,MAXK
         NX = KX(K)
         NY = KY(K)
         NZ = KZ(K)
         IF (KANDL) LMAX = K
         DO 300 L = MINL,LMAX
            KL = KL+1
            KLX(KL) = NX+LX(L)
            KLY(KL) = NY+LY(L)
            KLZ(KL) = NZ+LZ(L)
            KLGT(KL) = NGTK*(K-MINK)+NGTL*(L-MINL)
  300    CONTINUE
  310 CONTINUE
      MAX = KL
      DO 320 I = 1,IJ
      IF (SAME) MAX = I
  320 IK(I) = MAX
      IJKL = IJ*KL
      IF (SAME) IJKL = IJ*(IJ+1)/2
      RETURN
      END
C*MODULE QMFM    *DECK GETIJPP
      SUBROUTINE GETIJPP(DDIJ,IJNUM,NCXYZ,ISPP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL IANDJ,KANDL,SAME,OUT,NORM
C
      PARAMETER (MXGSH=30, MXG2=MXGSH*MXGSH)
C
      DIMENSION ISPP(NCXYZ,2)
      DIMENSION DDIJ(16*MXG2)
C
      COMMON /IJGNRL/ A(MXG2),R(MXG2),X1(MXG2),Y1(MXG2),Z1(MXG2),
     *                IJD(225)
      COMMON /MISC  / IANDJ,KANDL,SAME
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /SHLINF/ AG(MXGSH),CSA(MXGSH),CPA(MXGSH),CDA(MXGSH),
     *                CFA(MXGSH),CGA(MXGSH),
     *                BG(MXGSH),CSB(MXGSH),CPB(MXGSH),CDB(MXGSH),
     *                CFB(MXGSH),CGB(MXGSH),
     *                CG(MXGSH),CSC(MXGSH),CPC(MXGSH),CDC(MXGSH),
     *                CFC(MXGSH),CGC(MXGSH),
     *                DG(MXGSH),CSD(MXGSH),CPD(MXGSH),CDD(MXGSH),
     *                CFD(MXGSH),CGD(MXGSH),
     *                XI,YI,ZI,XJ,YJ,ZJ,RRI,XK,YK,ZK,XL,YL,ZL,RRK,
     *                NGA,NGB,NGC,NGD
      COMMON /SHLNOS/ QQ4,LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJ,KL,IJKL
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
      PARAMETER (SQRT3=1.73205080756888D+00,
     *           SQRT5=2.23606797749979D+00,
     *           SQRT7=2.64575131106459D+00,
     *           ZERO=0.0D+00, ONE=1.0D+00)
C
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
      MAX = MAXJ
      N = 0
      NN = 0
      NM = -2**20
      DO 180 I = MINI,MAXI
         GO TO (100,100,120,120,100,120,120,100,120,120,
     1          100,120,120,100,120,120,120,120,120,100,
     1          100,120,120,100,120,120,120,120,120,100,
     1          120,120,100,120,120),I
  100    NM = NN
  120    NN = NM
         IF (IANDJ) MAX = I
         DO 170 J = MINJ,MAX
            GO TO (140,140,160,160,140,160,160,140,160,160,
     1             140,160,160,140,160,160,160,160,160,140,
     1             140,160,160,140,160,160,160,160,160,140,
     1             160,160,140,160,160),J
  140       NN = NN+1
  160       N = N+1
            IJD(N) = NN
  170    CONTINUE
  180 CONTINUE
C
C     ----- I PRIMITIVE
C
      NIJ = 0
      DO 540 IJAB=1,IJNUM
         IA=ISPP(IJAB,1)
         AI = AG(IA)
         ARRI = AI*RRI
         AXI = AI*XI
         AYI = AI*YI
         AZI = AI*ZI
         CSI = CSA(IA)
         CPI = CPA(IA)
         CDI = CDA(IA)
         CFI = CFA(IA)
         CGI = CGA(IA)
C
C        ----- J PRIMITIVE
C
         JB=ISPP(IJAB,2)
         AJ = BG(JB)
         AA = AI+AJ
         AAINV = ONE/AA
         DUM = AJ*ARRI*AAINV
         IF (DUM .GT. TOL) GO TO 540
         CSJ = CSB(JB)
         CPJ = CPB(JB)
         CDJ = CDB(JB)
         CFJ = CFB(JB)
         CGJ = CGB(JB)
         NM = 16*NIJ
         NN = NM
         NIJ = NIJ+1
         R(NIJ) = DUM
         A(NIJ) = AA
         X1(NIJ) = (AXI+AJ*XJ)*AAINV
         Y1(NIJ) = (AYI+AJ*YJ)*AAINV
         Z1(NIJ) = (AZI+AJ*ZJ)*AAINV
C
C           ----- DENSITY FACTOR
C
         DUM1 = ZERO
         DUM2 = ZERO
         DO 420 I = MINI,MAXI
            GO TO (200,220,420,420,240,420,420,260,420,420,
     1             261,420,420,262,420,420,420,420,420,263,
     1             264,420,420,265,420,420,420,420,420,266,
     1             420,420,267,420,420),I
  200       DUM1 = CSI*AAINV
            GO TO 280
  220       DUM1 = CPI*AAINV
            GO TO 280
  240       DUM1 = CDI*AAINV
            GO TO 280
  260       IF (NORM) DUM1 = DUM1*SQRT3
            GO TO 280
  261       DUM1 = CFI*AAINV
            GO TO 280
  262       IF (NORM) DUM1 = DUM1*SQRT5
            GO TO 280
  263       IF (NORM) DUM1 = DUM1*SQRT3
            GO TO 280
  264       DUM1 = CGI*AAINV
            GO TO 280
  265       IF (NORM) DUM1 = DUM1*SQRT7
            GO TO 280
  266       IF (NORM) DUM1 = DUM1*SQRT5/SQRT3
            GO TO 280
  267       IF (NORM) DUM1 = DUM1*SQRT3
  280       IF (IANDJ) MAX = I
            DO 400 J = MINJ,MAX
               GO TO (300,320,400,400,340,400,400,360,400,400,
     1                361,400,400,362,400,400,400,400,400,363,
     1                364,400,400,365,400,400,400,400,400,366,
     1                400,400,367,400,400),J
  300          DUM2 = DUM1*CSJ
               GO TO 380
  320          DUM2 = DUM1*CPJ
               GO TO 380
  340          DUM2 = DUM1*CDJ
               GO TO 380
  360          IF (NORM) DUM2 = DUM2*SQRT3
               GO TO 380
  361          DUM2 = DUM1*CFJ
               GO TO 380
  362          IF (NORM) DUM2 = DUM2*SQRT5
               GO TO 380
  363          IF (NORM) DUM2 = DUM2*SQRT3
               GO TO 380
  364          DUM2 = DUM1*CGJ
               GO TO 380
  365          IF (NORM) DUM2 = DUM2*SQRT7
               GO TO 380
  366          IF (NORM) DUM2 = DUM2*SQRT5/SQRT3
               GO TO 380
  367          IF (NORM) DUM2 = DUM2*SQRT3
  380          NN = NN+1
               DDIJ(NN) = DUM2
  400       CONTINUE
  420    CONTINUE
         IF ( .NOT. IANDJ) GO TO 540
         IF (IA .EQ. JB) GO TO 540
         GO TO (500,440,460,455,450),LIT
  440    IF (MINI .EQ. 2) GO TO 500
         DDIJ(NM+2) = DDIJ(NM+2)+CSI*CPJ*AAINV
         GO TO 480
  450    DDIJ(NM+10) = DDIJ(NM+10)+DDIJ(NM+10)
         DDIJ(NM+9) = DDIJ(NM+9)+DDIJ(NM+9)
         DDIJ(NM+8) = DDIJ(NM+8)+DDIJ(NM+8)
         DDIJ(NM+7) = DDIJ(NM+7)+DDIJ(NM+7)
  455    DDIJ(NM+6) = DDIJ(NM+6)+DDIJ(NM+6)
         DDIJ(NM+5) = DDIJ(NM+5)+DDIJ(NM+5)
         DDIJ(NM+4) = DDIJ(NM+4)+DDIJ(NM+4)
  460    DDIJ(NM+2) = DDIJ(NM+2)+DDIJ(NM+2)
  480    DDIJ(NM+3) = DDIJ(NM+3)+DDIJ(NM+3)
  500    DDIJ(NM+1) = DDIJ(NM+1)+DDIJ(NM+1)
  540 CONTINUE
      RETURN
      END
C*MODULE QMFM    *DECK GETSSSS
      SUBROUTINE GETSSSS(GHONDO,DDIJ,NCXYZ,ISPP,IPPNUM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL IANDJ,KANDL,SAME,OUT
C
      PARAMETER (MXGSH=30, MXG2=MXGSH*MXGSH)
C
      DIMENSION GHONDO(*),DDIJ(16*MXG2)
      DIMENSION ISPP(NCXYZ,2)
C
      COMMON /IJGNRL/ A(MXG2),R(MXG2),X1(MXG2),Y1(MXG2),Z1(MXG2),
     *                IJD(225)
      COMMON /GOUT  / GPOPLE(768),NORG
      COMMON /MISC  / IANDJ,KANDL,SAME
      COMMON /SHLINF/ AG(MXGSH),CSA(MXGSH),CPA(MXGSH),CDA(MXGSH),
     *                CFA(MXGSH),CGA(MXGSH),
     *                BG(MXGSH),CSB(MXGSH),CPB(MXGSH),CDB(MXGSH),
     *                CFB(MXGSH),CGB(MXGSH),
     *                CG(MXGSH),CSC(MXGSH),CPC(MXGSH),CDC(MXGSH),
     *                CFC(MXGSH),CGC(MXGSH),
     *                DG(MXGSH),CSD(MXGSH),CPD(MXGSH),CDD(MXGSH),
     *                CFD(MXGSH),CGD(MXGSH),
     *                XI,YI,ZI,XJ,YJ,ZJ,RRI,XK,YK,ZK,XL,YL,ZL,RRK,
     *                NGA,NGB,NGC,NGD
      COMMON /SHLNOS/ QQ4,LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJ,KL,IJKL
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
      PARAMETER (PI252=34.986836655250D+00, PIE4=7.85398163397448D-01,
     *           ZERO=0.0D+00, ONE=1.0D+00)
C
C     SPECIAL SSSS INTEGRAL ROUTINE WHEN USING HONDO INTEGRALS
C
      GGOUT = ZERO
      DO 300 IPP=1,IPPNUM
         KG=ISPP(IPP,1)
C
         BK = CG(KG)
         BRRK = BK*RRK
         BXK = BK*XK
         BYK = BK*YK
         BZK = BK*ZK
         CSK = CSC(KG)
C
         LG=ISPP(IPP,2)
         BL = DG(LG)
         BB = BK+BL
         BBINV = ONE/BB
         DUM = BL*BRRK*BBINV
         IF (DUM .GT. TOL) GO TO 300
         BBRRK = DUM
         D2 = CSD(LG)*CSK*BBINV
         IF (KANDL .AND. LG .NE. KG) D2 = D2+D2
         BBX = (BXK+BL*XL)*BBINV
         BBY = (BYK+BL*YL)*BBINV
         BBZ = (BZK+BL*ZL)*BBINV
         SUM = ZERO
         DO 260 N = 1,NIJ
         NN=(N-1)*16+1
         DUM = BBRRK+R(N)
         IF (DUM .GT. TOL) GO TO 260
         EXPE = EXP(-DUM)
         AA = A(N)
         AB = AA+BB
         DUM = X1(N)-BBX
         XX = DUM*DUM
         DUM = Y1(N)-BBY
         XX = DUM*DUM+XX
         DUM = Z1(N)-BBZ
         XX = DUM*DUM+XX
         X = XX*AA*BB/AB
C
         IF (X .GT. 5.0D+00) GO TO 160
         IF (X .GT. 1.0D+00) GO TO 120
         IF (X .GT. 3.0D-07) GO TO 100
         WW1 = 1.0D+00-X/3.0D+00
         GO TO 240
C
  100    CONTINUE
         F1 = ((((((((-8.36313918003957D-08*X+
     *     1.21222603512827D-06 )*X-
     *     1.15662609053481D-05 )*X+9.25197374512647D-05 )*X-
     *     6.40994113129432D-04 )*X+3.78787044215009D-03 )*X-
     *     1.85185172458485D-02 )*X+7.14285713298222D-02 )*X-
     *     1.99999999997023D-01 )*X+3.33333333333318D-01
         WW1 = (X+X)*F1+EXP(-X)
         GO TO 240
C
  120    CONTINUE
         IF (X .GT. 3.0D+00) GO TO 140
         Y = X-2.0D+00
         F1 = ((((((((((-1.61702782425558D-10*Y+
     *     1.96215250865776D-09 )*Y-
     +     2.14234468198419D-08 )*Y+2.17216556336318D-07 )*Y-
     +     1.98850171329371D-06 )*Y+1.62429321438911D-05 )*Y-
     +     1.16740298039895D-04 )*Y+7.24888732052332D-04 )*Y-
     +     3.79490003707156D-03 )*Y+1.61723488664661D-02 )*Y-
     +     5.29428148329736D-02 )*Y+1.15702180856167D-01
         WW1 = (X+X)*F1+EXP(-X)
         GO TO 240
C
  140    CONTINUE
         Y = X-4.0D+00
         F1 = ((((((((((-2.62453564772299D-11*Y+
     *     3.24031041623823D-10 )*Y-
     +     3.614965656163D-09)*Y+3.760256799971D-08)*Y-
     +     3.553558319675D-07)*Y+3.022556449731D-06)*Y-
     +     2.290098979647D-05)*Y+1.526537461148D-04)*Y-
     +     8.81947375894379D-04 )*Y+4.33207949514611D-03 )*Y-
     +     1.75257821619926D-02 )*Y+5.28406320615584D-02
         WW1 = (X+X)*F1+EXP(-X)
         GO TO 240
C
  160    CONTINUE
         IF (X .GT. 15.0D+00) GO TO 200
         E = EXP(-X)
         IF (X .GT. 10.0D+00) GO TO 180
         XINV = ONE/X
         WW1 = (((((( 4.6897511375022D-01*XINV-
     *     6.9955602298985D-01)*XINV +
     +     5.3689283271887D-01)*XINV-3.2883030418398D-01)*XINV +
     +     2.4645596956002D-01)*XINV-4.9984072848436D-01)*XINV -
     +     3.1501078774085D-06)*E + SQRT(PIE4*XINV)
         GO TO 240
C
  180    CONTINUE
         XINV = ONE/X
         WW1 = (((-1.8784686463512D-01*XINV+
     *         2.2991849164985D-01)*XINV
     +         -4.9893752514047D-01)*XINV-2.1916512131607D-05)*E
     +         + SQRT(PIE4*XINV)
         GO TO 240
C
  200    CONTINUE
         IF (X .GT. 33.0D+00) GO TO 220
         XINV = ONE/X
         E = EXP(-X)
         WW1 = (( 1.9623264149430D-01*XINV-
     *     4.9695241464490D-01)*XINV -
     +     6.0156581186481D-05)*E + SQRT(PIE4*XINV)
         GO TO 240
C
  220    WW1 = SQRT(PIE4/X)
C
  240    SUM = SUM+DDIJ(NN)*WW1*EXPE/SQRT(AB)
 260     CONTINUE
         GGOUT = GGOUT+D2*SUM
  300 CONTINUE
      GHONDO(NORG+1) = GGOUT*PI252*QQ4
      RETURN
      END
C*MODULE QMFM    *DECK S0000X
      SUBROUTINE S0000X(GHONDO,DDIJ,KLPP,KLCNT,IJTBLP,IJTBL)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL IANDJ,KANDL,SAME,OUT
C
      PARAMETER (MXGSH=30, MXG2=MXGSH*MXGSH)
C
      DIMENSION GHONDO(*),DDIJ(16*MXG2)
      DIMENSION KLPP(100,2),IJTBLP(100+1),IJTBL(1000)
C
      COMMON /IJGNRL/ A(MXG2),R(MXG2),X1(MXG2),Y1(MXG2),Z1(MXG2),
     *                IJD(225)
      COMMON /GOUT  / GPOPLE(768),NORG
      COMMON /MISC  / IANDJ,KANDL,SAME
      COMMON /SHLINF/ AG(MXGSH),CSA(MXGSH),CPA(MXGSH),CDA(MXGSH),
     *                CFA(MXGSH),CGA(MXGSH),
     *                BG(MXGSH),CSB(MXGSH),CPB(MXGSH),CDB(MXGSH),
     *                CFB(MXGSH),CGB(MXGSH),
     *                CG(MXGSH),CSC(MXGSH),CPC(MXGSH),CDC(MXGSH),
     *                CFC(MXGSH),CGC(MXGSH),
     *                DG(MXGSH),CSD(MXGSH),CPD(MXGSH),CDD(MXGSH),
     *                CFD(MXGSH),CGD(MXGSH),
     *                XI,YI,ZI,XJ,YJ,ZJ,RRI,XK,YK,ZK,XL,YL,ZL,RRK,
     *                NGA,NGB,NGC,NGD
      COMMON /SHLNOS/ QQ4,LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJ,KL,IJKL
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
      PARAMETER (PI252=34.986836655250D+00, PIE4=7.85398163397448D-01,
     *           ZERO=0.0D+00, ONE=1.0D+00)
C
C     SPECIAL SSSS INTEGRAL ROUTINE WHEN USING HONDO INTEGRALS
C
      GGOUT = ZERO
      DO 300 IPP=1,KLCNT
         KG=KLPP(IPP,1)
C
         BK = CG(KG)
         BRRK = BK*RRK
         BXK = BK*XK
         BYK = BK*YK
         BZK = BK*ZK
         CSK = CSC(KG)
C
         LG=KLPP(IPP,2)
         BL = DG(LG)
         BB = BK+BL
         BBINV = ONE/BB
         DUM = BL*BRRK*BBINV
         IF (DUM .GT. TOL) GO TO 300
         BBRRK = DUM
         D2 = CSD(LG)*CSK*BBINV
         IF (KANDL .AND. LG .NE. KG) D2 = D2+D2
         BBX = (BXK+BL*XL)*BBINV
         BBY = (BYK+BL*YL)*BBINV
         BBZ = (BZK+BL*ZL)*BBINV
         SUM = ZERO
         DO 260 M = IJTBLP(IPP),IJTBLP(IPP+1)-1
         N=IJTBL(M)
         NN=(N-1)*16+1
         DUM = BBRRK+R(N)
         IF (DUM .GT. TOL) GO TO 260
         EXPE = EXP(-DUM)
         AA = A(N)
         AB = AA+BB
         DUM = X1(N)-BBX
         XX = DUM*DUM
         DUM = Y1(N)-BBY
         XX = DUM*DUM+XX
         DUM = Z1(N)-BBZ
         XX = DUM*DUM+XX
         X = XX*AA*BB/AB
C
         IF (X .GT. 5.0D+00) GO TO 160
         IF (X .GT. 1.0D+00) GO TO 120
         IF (X .GT. 3.0D-07) GO TO 100
         WW1 = 1.0D+00-X/3.0D+00
         GO TO 240
C
  100    CONTINUE
         F1 = ((((((((-8.36313918003957D-08*X+
     *     1.21222603512827D-06 )*X-
     *     1.15662609053481D-05 )*X+9.25197374512647D-05 )*X-
     *     6.40994113129432D-04 )*X+3.78787044215009D-03 )*X-
     *     1.85185172458485D-02 )*X+7.14285713298222D-02 )*X-
     *     1.99999999997023D-01 )*X+3.33333333333318D-01
         WW1 = (X+X)*F1+EXP(-X)
         GO TO 240
C
  120    CONTINUE
         IF (X .GT. 3.0D+00) GO TO 140
         Y = X-2.0D+00
         F1 = ((((((((((-1.61702782425558D-10*Y+
     *     1.96215250865776D-09 )*Y-
     +     2.14234468198419D-08 )*Y+2.17216556336318D-07 )*Y-
     +     1.98850171329371D-06 )*Y+1.62429321438911D-05 )*Y-
     +     1.16740298039895D-04 )*Y+7.24888732052332D-04 )*Y-
     +     3.79490003707156D-03 )*Y+1.61723488664661D-02 )*Y-
     +     5.29428148329736D-02 )*Y+1.15702180856167D-01
         WW1 = (X+X)*F1+EXP(-X)
         GO TO 240
C
  140    CONTINUE
         Y = X-4.0D+00
         F1 = ((((((((((-2.62453564772299D-11*Y+
     *     3.24031041623823D-10 )*Y-
     +     3.614965656163D-09)*Y+3.760256799971D-08)*Y-
     +     3.553558319675D-07)*Y+3.022556449731D-06)*Y-
     +     2.290098979647D-05)*Y+1.526537461148D-04)*Y-
     +     8.81947375894379D-04 )*Y+4.33207949514611D-03 )*Y-
     +     1.75257821619926D-02 )*Y+5.28406320615584D-02
         WW1 = (X+X)*F1+EXP(-X)
         GO TO 240
C
  160    CONTINUE
         IF (X .GT. 15.0D+00) GO TO 200
         E = EXP(-X)
         IF (X .GT. 10.0D+00) GO TO 180
         XINV = ONE/X
         WW1 = (((((( 4.6897511375022D-01*XINV-
     *     6.9955602298985D-01)*XINV +
     +     5.3689283271887D-01)*XINV-3.2883030418398D-01)*XINV +
     +     2.4645596956002D-01)*XINV-4.9984072848436D-01)*XINV -
     +     3.1501078774085D-06)*E + SQRT(PIE4*XINV)
         GO TO 240
C
  180    CONTINUE
         XINV = ONE/X
         WW1 = (((-1.8784686463512D-01*XINV+
     *         2.2991849164985D-01)*XINV
     +         -4.9893752514047D-01)*XINV-2.1916512131607D-05)*E
     +         + SQRT(PIE4*XINV)
         GO TO 240
C
  200    CONTINUE
         IF (X .GT. 33.0D+00) GO TO 220
         XINV = ONE/X
         E = EXP(-X)
         WW1 = (( 1.9623264149430D-01*XINV-
     *     4.9695241464490D-01)*XINV -
     +     6.0156581186481D-05)*E + SQRT(PIE4*XINV)
         GO TO 240
C
  220    WW1 = SQRT(PIE4/X)
C
  240    SUM = SUM+DDIJ(NN)*WW1*EXPE/SQRT(AB)
 260     CONTINUE
         GGOUT = GGOUT+D2*SUM
  300 CONTINUE
      GHONDO(NORG+1) = GGOUT*PI252*QQ4
      RETURN
      END
C*MODULE QMFM    *DECK HONDOX
      SUBROUTINE HONDOX(GHONDO,DDIJ,KLPP,KLCNT,
     *   IJTBLP,IJTBL)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION GHONDO(*),DDIJ(*)
      DIMENSION KLPP(100,2),IJTBLP(100+1),IJTBL(1000)
C
      LOGICAL IANDJ,KANDL,SAME,OUT,NORM,DOUBLE
C
      PARAMETER (MXGSH=30, MXG2=MXGSH*MXGSH)
C
      COMMON /DENS  / DKL(225),DIJ(225)
      COMMON /IJGNRL/ AA(MXG2),R(MXG2),X1(MXG2),Y1(MXG2),Z1(MXG2),
     *                IJD(225)
      COMMON /MISC  / IANDJ,KANDL,SAME
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /SETINT/ IN(9),KN(9),NI,NJ,NK,NL,NMAX,MMAX,
     +                BP01,B00,B10,XCP00,XC00,YCP00,YC00,ZCP00,ZC00,F00,
     +                DXIJ,DYIJ,DZIJ,DXKL,DYKL,DZKL
      COMMON /SHLINF/ AG(MXGSH),CSA(MXGSH),CPA(MXGSH),CDA(MXGSH),
     *                CFA(MXGSH),CGA(MXGSH),
     *                BG(MXGSH),CSB(MXGSH),CPB(MXGSH),CDB(MXGSH),
     *                CFB(MXGSH),CGB(MXGSH),
     *                CG(MXGSH),CSC(MXGSH),CPC(MXGSH),CDC(MXGSH),
     *                CFC(MXGSH),CGC(MXGSH),
     *                DG(MXGSH),CSD(MXGSH),CPD(MXGSH),CDD(MXGSH),
     *                CFD(MXGSH),CGD(MXGSH),
     *                XI,YI,ZI,XJ,YJ,ZJ,RRI,XK,YK,ZK,XL,YL,ZL,RRK,
     *                NGA,NGB,NGC,NGD
      COMMON /SHLNOS/ QQ4,LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     +                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     +                NIJ,IJ,KL,IJKL
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
      DIMENSION IN1(9)
C
      PARAMETER (SQRT3=1.73205080756888D+00, SQRT5=2.23606797749979D+00,
     *           SQRT7=2.64575131106459D+00, PI252=34.986836655250D+00,
     *           ZERO=0.0D+00, HALF=0.5D+00, ONE=1.0D+00)
C
C     GENERAL INTEGRAL ROUTINE FOR SPD FUNCTIONS
C
      FACTOR = PI252*QQ4
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
      NI = LIT-1
      NJ = LJT-1
      NK = LKT-1
      NL = LLT-1
      DXIJ = XI-XJ
      DYIJ = YI-YJ
      DZIJ = ZI-ZJ
      DXKL = XK-XL
      DYKL = YK-YL
      DZKL = ZK-ZL
      NMAX = NI+NJ
      MMAX = NK+NL
      MAX = NMAX+1
      DO 100 I = 1,MAX
         N = I-1
         IF (N .LE. NI) IN1(I) = 125*N+1
         IF (N .GT. NI) IN1(I) = 125*NI+25*(N-NI)+1
  100 CONTINUE
      MAX = MMAX+1
      DO 120 K = 1,MAX
         N = K-1
         IF (N .LE. NK) KN(K) = 5*N
         IF (N .GT. NK) KN(K) = 5*NK+N-NK
  120 CONTINUE
C
C     ----- K PRIMITIVE
C
C
      DO 480 IKL=1,KLCNT
         KG=KLPP(IKL,1)
C
C      DO 480 KG = 1,NGC
         AK = CG(KG)
         BRRK = AK*RRK
         AKXK = AK*XK
         AKYK = AK*YK
         AKZK = AK*ZK
         CSK = CSC(KG)*FACTOR
         CPK = CPC(KG)*FACTOR
         CDK = CDC(KG)*FACTOR
         CFK = CFC(KG)*FACTOR
         CGK = CGC(KG)*FACTOR
C
C        ----- L PRIMITIVE
C
         LG=KLPP(IKL,2)
         AL = DG(LG)
         B = AK+AL
         BINV = ONE/B
         BBRRK = AL*BRRK*BINV
         IF (BBRRK .GT. TOL) GO TO 480
         CSL = CSD(LG)
         CPL = CPD(LG)
         CDL = CDD(LG)
         CFL = CFD(LG)
         CGL = CGD(LG)
         XB = (AKXK+AL*XL)*BINV
         YB = (AKYK+AL*YL)*BINV
         ZB = (AKZK+AL*ZL)*BINV
         BXBK = B*(XB-XK)
         BYBK = B*(YB-YK)
         BZBK = B*(ZB-ZK)
         BXBI = B*(XB-XI)
         BYBI = B*(YB-YI)
         BZBI = B*(ZB-ZI)
C
C           ----- DENSITY FACTOR
C
         DOUBLE=KANDL.AND.KG.NE.LG
         N = 0
         MAX = MAXL
         DUM1 = ZERO
         DUM2 = ZERO
         DO 370 K = MINK,MAXK
            GO TO (140,160,220,220,180,220,220,200,220,220,
     1             201,220,220,202,220,220,220,220,220,203,
     1             204,220,220,205,220,220,220,220,220,206,
     1             220,220,207,220,220),K
  140       DUM1 = CSK*BINV
            GO TO 220
  160       DUM1 = CPK*BINV
            GO TO 220
  180       DUM1 = CDK*BINV
            GO TO 220
  200       IF (NORM) DUM1 = DUM1*SQRT3
            GO TO 220
  201       DUM1 = CFK*BINV
            GO TO 220
  202       IF (NORM) DUM1 = DUM1*SQRT5
            GO TO 220
  203       IF (NORM) DUM1 = DUM1*SQRT3
            GO TO 220
  204       DUM1 = CGK*BINV
            GO TO 220
  205       IF (NORM) DUM1 = DUM1*SQRT7
            GO TO 220
  206       IF (NORM) DUM1 = DUM1*SQRT5/SQRT3
            GO TO 220
  207       IF (NORM) DUM1 = DUM1*SQRT3
  220       IF (KANDL) MAX = K
            DO 360 L = MINL,MAX
               GO TO (240,280,340,340,300,340,340,320,340,340,
     1                321,340,340,322,340,340,340,340,340,323,
     1                324,340,340,325,340,340,340,340,340,326,
     1                340,340,327,340,340),L
  240          DUM2 = DUM1*CSL
               IF ( .NOT. DOUBLE) GO TO 340
               IF (K .GT. 1) GO TO 260
               DUM2 = DUM2+DUM2
               GO TO 340
  260          DUM2 = DUM2+CSK*CPL*BINV
               GO TO 340
  280          DUM2 = DUM1*CPL
               IF (DOUBLE) DUM2 = DUM2+DUM2
               GO TO 340
  300          DUM2 = DUM1*CDL
               IF (DOUBLE) DUM2 = DUM2+DUM2
               GO TO 340
  320          IF (NORM) DUM2 = DUM2*SQRT3
               GO TO 340
  321          DUM2 = DUM1*CFL
               IF (DOUBLE) DUM2 = DUM2+DUM2
               GO TO 340
  322          IF (NORM) DUM2 = DUM2*SQRT5
               GO TO 340
  323          IF (NORM) DUM2 = DUM2*SQRT3
               GO TO 340
  324          DUM2 = DUM1*CGL
               IF (DOUBLE) DUM2 = DUM2+DUM2
               GO TO 340
  325          IF (NORM) DUM2 = DUM2*SQRT7
               GO TO 340
  326          IF (NORM) DUM2 = DUM2*SQRT5/SQRT3
               GO TO 340
  327          IF (NORM) DUM2 = DUM2*SQRT3
  340          N = N+1
               DKL(N) = DUM2
  360       CONTINUE
  370    CONTINUE
C
C           ----- PAIR OF I,J PRIMITIVES
C
         NN = 0
C         DO 440 N = 1,NIJ
         DO 440 MX = IJTBLP(IKL),IJTBLP(IKL+1)-1
            N=IJTBL(MX)
            NN=(N-1)*16
            DUM = BBRRK+R(N)
            IF (DUM .GT. TOL) GO TO 440
            DO 380 I = 1,IJ
               DIJ(I) = DDIJ(IJD(I)+NN)
  380       CONTINUE
            A = AA(N)
            AB = A*B
            AANDB = A+B
            EXPE = EXP(-DUM)/SQRT(AANDB)
            RHO = AB/AANDB
            XA = X1(N)
            YA = Y1(N)
            ZA = Z1(N)
            XX = RHO*((XA-XB)*(XA-XB) + (YA-YB)*(YA-YB)
     *                                + (ZA-ZB)*(ZA-ZB))
            AXAK = A*(XA-XK)
            AYAK = A*(YA-YK)
            AZAK = A*(ZA-ZK)
            AXAI = A*(XA-XI)
            AYAI = A*(YA-YI)
            AZAI = A*(ZA-ZI)
            C1X = BXBK+AXAK
            C2X = A*BXBK
            C3X = BXBI+AXAI
            C4X = B*AXAI
            C1Y = BYBK+AYAK
            C2Y = A*BYBK
            C3Y = BYBI+AYAI
            C4Y = B*AYAI
            C1Z = BZBK+AZAK
            C2Z = A*BZBK
            C3Z = BZBI+AZAI
            C4Z = B*AZAI
C
C              ----- ROOTS AND WEIGHTS FOR QUADRATURE
C
            IF (NROOTS .LE. 3) CALL RT123
            IF (NROOTS .EQ. 4) CALL ROOT4
            IF (NROOTS .EQ. 5) CALL ROOT5
            IF (NROOTS .GE. 6) CALL ROOT6
            MM = 0
            MAX = NMAX+1
C
C              COMPUTE TWO-ELECTRON INTEGRALS FOR EACH ROOT
C
            DO 420 M = 1,NROOTS
               U2 = U(M)*RHO
               F00 = EXPE*W(M)
               DO 400 I = 1,MAX
                  IN(I) = IN1(I)+MM
  400          CONTINUE
               DUMINV = ONE/(AB+U2*AANDB)
               DM2INV = HALF*DUMINV
               BP01 = (A+U2)*DM2INV
               B00 = U2*DM2INV
               B10 = (B+U2)*DM2INV
               XCP00 = (U2*C1X+C2X)*DUMINV
               XC00 = (U2*C3X+C4X)*DUMINV
               YCP00 = (U2*C1Y+C2Y)*DUMINV
               YC00 = (U2*C3Y+C4Y)*DUMINV
               ZCP00 = (U2*C1Z+C2Z)*DUMINV
               ZC00 = (U2*C3Z+C4Z)*DUMINV
               CALL XYZINT
               MM = MM+625
  420       CONTINUE
C
C              ----- FORM (I,J//K,L) INTEGRALS OVER FUNCTIONS
C
            CALL FORMS(GHONDO)
  440    CONTINUE
  480 CONTINUE
C
      RETURN
      END
C*MODULE QMFM    *DECK GETHNT
      SUBROUTINE GETHNT(GHONDO,DDIJ,NCXYZ,IPPKL,KLNUM)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION GHONDO(*),DDIJ(*)
      DIMENSION IPPKL(NCXYZ,2)
C
      LOGICAL IANDJ,KANDL,SAME,OUT,NORM,DOUBLE
C
      PARAMETER (MXGSH=30, MXG2=MXGSH*MXGSH)
C
      COMMON /DENS  / DKL(225),DIJ(225)
      COMMON /IJGNRL/ AA(MXG2),R(MXG2),X1(MXG2),Y1(MXG2),Z1(MXG2),
     *                IJD(225)
      COMMON /MISC  / IANDJ,KANDL,SAME
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /SETINT/ IN(9),KN(9),NI,NJ,NK,NL,NMAX,MMAX,
     +                BP01,B00,B10,XCP00,XC00,YCP00,YC00,ZCP00,ZC00,F00,
     +                DXIJ,DYIJ,DZIJ,DXKL,DYKL,DZKL
      COMMON /SHLINF/ AG(MXGSH),CSA(MXGSH),CPA(MXGSH),CDA(MXGSH),
     *                CFA(MXGSH),CGA(MXGSH),
     *                BG(MXGSH),CSB(MXGSH),CPB(MXGSH),CDB(MXGSH),
     *                CFB(MXGSH),CGB(MXGSH),
     *                CG(MXGSH),CSC(MXGSH),CPC(MXGSH),CDC(MXGSH),
     *                CFC(MXGSH),CGC(MXGSH),
     *                DG(MXGSH),CSD(MXGSH),CPD(MXGSH),CDD(MXGSH),
     *                CFD(MXGSH),CGD(MXGSH),
     *                XI,YI,ZI,XJ,YJ,ZJ,RRI,XK,YK,ZK,XL,YL,ZL,RRK,
     *                NGA,NGB,NGC,NGD
      COMMON /SHLNOS/ QQ4,LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     +                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     +                NIJ,IJ,KL,IJKL
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
      DIMENSION IN1(9)
C
      PARAMETER (SQRT3=1.73205080756888D+00, SQRT5=2.23606797749979D+00,
     *           SQRT7=2.64575131106459D+00, PI252=34.986836655250D+00,
     *           ZERO=0.0D+00, HALF=0.5D+00, ONE=1.0D+00)
C
C     GENERAL INTEGRAL ROUTINE FOR SPD FUNCTIONS
C
      FACTOR = PI252*QQ4
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
      NI = LIT-1
      NJ = LJT-1
      NK = LKT-1
      NL = LLT-1
      DXIJ = XI-XJ
      DYIJ = YI-YJ
      DZIJ = ZI-ZJ
      DXKL = XK-XL
      DYKL = YK-YL
      DZKL = ZK-ZL
      NMAX = NI+NJ
      MMAX = NK+NL
      MAX = NMAX+1
      DO 100 I = 1,MAX
         N = I-1
         IF (N .LE. NI) IN1(I) = 125*N+1
         IF (N .GT. NI) IN1(I) = 125*NI+25*(N-NI)+1
  100 CONTINUE
      MAX = MMAX+1
      DO 120 K = 1,MAX
         N = K-1
         IF (N .LE. NK) KN(K) = 5*N
         IF (N .GT. NK) KN(K) = 5*NK+N-NK
  120 CONTINUE
C
C     ----- K PRIMITIVE
C
C
      DO 480 IKL=1,KLNUM
         KG=IPPKL(IKL,1)
C
C      DO 480 KG = 1,NGC
         AK = CG(KG)
         BRRK = AK*RRK
         AKXK = AK*XK
         AKYK = AK*YK
         AKZK = AK*ZK
         CSK = CSC(KG)*FACTOR
         CPK = CPC(KG)*FACTOR
         CDK = CDC(KG)*FACTOR
         CFK = CFC(KG)*FACTOR
         CGK = CGC(KG)*FACTOR
C
C        ----- L PRIMITIVE
C
         LG=IPPKL(IKL,2)
C
         AL = DG(LG)
         B = AK+AL
         BINV = ONE/B
         BBRRK = AL*BRRK*BINV
         IF (BBRRK .GT. TOL) GO TO 480
         CSL = CSD(LG)
         CPL = CPD(LG)
         CDL = CDD(LG)
         CFL = CFD(LG)
         CGL = CGD(LG)
         XB = (AKXK+AL*XL)*BINV
         YB = (AKYK+AL*YL)*BINV
         ZB = (AKZK+AL*ZL)*BINV
         BXBK = B*(XB-XK)
         BYBK = B*(YB-YK)
         BZBK = B*(ZB-ZK)
         BXBI = B*(XB-XI)
         BYBI = B*(YB-YI)
         BZBI = B*(ZB-ZI)
C
C           ----- DENSITY FACTOR
C
         DOUBLE=KANDL.AND.KG.NE.LG
         N = 0
         MAX = MAXL
         DUM1 = ZERO
         DUM2 = ZERO
         DO 370 K = MINK,MAXK
            GO TO (140,160,220,220,180,220,220,200,220,220,
     1             201,220,220,202,220,220,220,220,220,203,
     1             204,220,220,205,220,220,220,220,220,206,
     1             220,220,207,220,220),K
  140       DUM1 = CSK*BINV
            GO TO 220
  160       DUM1 = CPK*BINV
            GO TO 220
  180       DUM1 = CDK*BINV
            GO TO 220
  200       IF (NORM) DUM1 = DUM1*SQRT3
            GO TO 220
  201       DUM1 = CFK*BINV
            GO TO 220
  202       IF (NORM) DUM1 = DUM1*SQRT5
            GO TO 220
  203       IF (NORM) DUM1 = DUM1*SQRT3
            GO TO 220
  204       DUM1 = CGK*BINV
            GO TO 220
  205       IF (NORM) DUM1 = DUM1*SQRT7
            GO TO 220
  206       IF (NORM) DUM1 = DUM1*SQRT5/SQRT3
            GO TO 220
  207       IF (NORM) DUM1 = DUM1*SQRT3
  220       IF (KANDL) MAX = K
            DO 360 L = MINL,MAX
               GO TO (240,280,340,340,300,340,340,320,340,340,
     1                321,340,340,322,340,340,340,340,340,323,
     1                324,340,340,325,340,340,340,340,340,326,
     1                340,340,327,340,340),L
  240          DUM2 = DUM1*CSL
               IF ( .NOT. DOUBLE) GO TO 340
               IF (K .GT. 1) GO TO 260
               DUM2 = DUM2+DUM2
               GO TO 340
  260          DUM2 = DUM2+CSK*CPL*BINV
               GO TO 340
  280          DUM2 = DUM1*CPL
               IF (DOUBLE) DUM2 = DUM2+DUM2
               GO TO 340
  300          DUM2 = DUM1*CDL
               IF (DOUBLE) DUM2 = DUM2+DUM2
               GO TO 340
  320          IF (NORM) DUM2 = DUM2*SQRT3
               GO TO 340
  321          DUM2 = DUM1*CFL
               IF (DOUBLE) DUM2 = DUM2+DUM2
               GO TO 340
  322          IF (NORM) DUM2 = DUM2*SQRT5
               GO TO 340
  323          IF (NORM) DUM2 = DUM2*SQRT3
               GO TO 340
  324          DUM2 = DUM1*CGL
               IF (DOUBLE) DUM2 = DUM2+DUM2
               GO TO 340
  325          IF (NORM) DUM2 = DUM2*SQRT7
               GO TO 340
  326          IF (NORM) DUM2 = DUM2*SQRT5/SQRT3
               GO TO 340
  327          IF (NORM) DUM2 = DUM2*SQRT3
  340          N = N+1
               DKL(N) = DUM2
  360       CONTINUE
  370    CONTINUE
C
C           ----- PAIR OF I,J PRIMITIVES
C
         NN = 0
         DO 440 N = 1,NIJ
            NN=(N-1)*16
            DUM = BBRRK+R(N)
            IF (DUM .GT. TOL) GO TO 440
            DO 380 I = 1,IJ
               DIJ(I) = DDIJ(IJD(I)+NN)
  380       CONTINUE
            A = AA(N)
            AB = A*B
            AANDB = A+B
            EXPE = EXP(-DUM)/SQRT(AANDB)
            RHO = AB/AANDB
            XA = X1(N)
            YA = Y1(N)
            ZA = Z1(N)
            XX = RHO*((XA-XB)*(XA-XB) + (YA-YB)*(YA-YB)
     *                                + (ZA-ZB)*(ZA-ZB))
            AXAK = A*(XA-XK)
            AYAK = A*(YA-YK)
            AZAK = A*(ZA-ZK)
            AXAI = A*(XA-XI)
            AYAI = A*(YA-YI)
            AZAI = A*(ZA-ZI)
            C1X = BXBK+AXAK
            C2X = A*BXBK
            C3X = BXBI+AXAI
            C4X = B*AXAI
            C1Y = BYBK+AYAK
            C2Y = A*BYBK
            C3Y = BYBI+AYAI
            C4Y = B*AYAI
            C1Z = BZBK+AZAK
            C2Z = A*BZBK
            C3Z = BZBI+AZAI
            C4Z = B*AZAI
C
C              ----- ROOTS AND WEIGHTS FOR QUADRATURE
C
            IF (NROOTS .LE. 3) CALL RT123
            IF (NROOTS .EQ. 4) CALL ROOT4
            IF (NROOTS .EQ. 5) CALL ROOT5
            IF (NROOTS .GE. 6) CALL ROOT6
            MM = 0
            MAX = NMAX+1
C
C              COMPUTE TWO-ELECTRON INTEGRALS FOR EACH ROOT
C
            DO 420 M = 1,NROOTS
               U2 = U(M)*RHO
               F00 = EXPE*W(M)
               DO 400 I = 1,MAX
                  IN(I) = IN1(I)+MM
  400          CONTINUE
               DUMINV = ONE/(AB+U2*AANDB)
               DM2INV = HALF*DUMINV
               BP01 = (A+U2)*DM2INV
               B00 = U2*DM2INV
               B10 = (B+U2)*DM2INV
               XCP00 = (U2*C1X+C2X)*DUMINV
               XC00 = (U2*C3X+C4X)*DUMINV
               YCP00 = (U2*C1Y+C2Y)*DUMINV
               YC00 = (U2*C3Y+C4Y)*DUMINV
               ZCP00 = (U2*C1Z+C2Z)*DUMINV
               ZC00 = (U2*C3Z+C4Z)*DUMINV
               CALL XYZINT
               MM = MM+625
  420       CONTINUE
C
C              ----- FORM (I,J//K,L) INTEGRALS OVER FUNCTIONS
C
            CALL FORMS(GHONDO)
  440    CONTINUE
  480 CONTINUE
C
      RETURN
      END
C*MODULE QMFM    *DECK SORTS
      SUBROUTINE SORTS(S,L2,LDST,NDST,LSHL,TS,ISHELL)
C
C     THIS ROUTINE RETURNS THE SORTED OVERLAP INTEGRAL
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL IANDJ,OUT
C
      PARAMETER (MXSH=1000, MXGTOT=5000)
C
      DIMENSION S(L2),TS(ISHELL),LDST(NDST),LSHL(ISHELL+1)
C
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
C
      ISTART = 1
      IEND   = NSHELL
      JSTART = 1
      LOCIJ  = 0
C
      IJ=0
      LSHL(1)=1
C
C     ----- I SHELL -----
C
      DO 720 II = ISTART,IEND
         MINI = KMIN(II)
         MAXI = KMAX(II)
         LOCI = KLOC(II)-MINI-LOCIJ
C
C     ----- J SHELL -----
C
         CALL VCLR(TS,1,NSHELL)
         NIJ=1
         DO 700 JJ = JSTART,IEND
            MINJ = KMIN(JJ)
            MAXJ = KMAX(JJ)
            LOCJ = KLOC(JJ)-MINJ-LOCIJ
            IANDJ = II .EQ. JJ
C
            MAX = MAXJ
            DO 620 I = MINI,MAXI
               LI = LOCI+I
               IN = (LI*(LI-1))/2
               IF (IANDJ) MAX = I
               DO 600 J = MINJ,MAX
                  LJ = LOCJ+J
                  IF (JJ.GT.II) THEN
                     JN = (LJ*(LJ-1))/2
                     IJN = LI+JN
                  ELSE
                     IJN = LJ+IN
                  ENDIF
                  IF (ABS(S(IJN)).GT.ABS(TS(NIJ))) TS(NIJ)=ABS(S(IJN))
  600          CONTINUE
  620       CONTINUE
            IF (ABS(TS(NIJ)).GT.CUTOFF) THEN
               IJ=IJ+1
               LDST(IJ)=JJ
               NIJ=NIJ+1
            ENDIF
C
C     ----- END OF SHELL LOOPS -----
C
  700    CONTINUE
         LSHL(II+1)=IJ+1
C
         LSIZE=LSHL(II+1)-LSHL(II)
         IF (LSIZE.EQ.0) GOTO 720
         CALL DSORT(LSIZE,TS,LDST(IJ-LSIZE+1))
C
         DO III=1,(LSIZE+1)/2
           IDX=IJ-LSIZE+III
           ITEMP=LDST(IDX)
           LDST(IDX) = LDST(IJ-III+1)
           LDST(IJ-III+1) = ITEMP
         ENDDO
C
  720 CONTINUE
C
C     ----- SAVE LDST, LSHL ------
C
C      MAXS=LSHL(ISHELL+1)-1
C      CALL DAWRIT(IDAF,IODA,LDST,MAXS,257,1)
C      CALL DAWRIT(IDAF,IODA,LSHL,ISHELL+1,258,1)
C
      RETURN
      END
C*MODULE QMFM    *DECK SORTD
      SUBROUTINE SORTD(LDST,NSHL2,LSHL,TS,ISHELL,XINTS,SHLDEN,
     *  NSH2)
C
C     THIS ROUTINE RETURNS THE SORTED OVERLAP INTEGRAL
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL OUT
      DIMENSION TS(ISHELL),LDST(NSHL2),LSHL(ISHELL+1),
     *          SHLDEN(NSH2),XINTS(NSH2)
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
      IJ=0
      LSHL(1)=1
C
C     ----- I SHELL -----
C
      DO 720 II = 1,ISHELL
         CALL VCLR(TS,1,ISHELL)
         LI = (II*II-II)/2
C
C     ----- J SHELL -----
C
         NIJ=1
         DO 700 JJ = 1,ISHELL
            LJ = (JJ*JJ-JJ)/2
            JJJJ = LJ + JJ
            IF (JJ.GT.II) THEN
               IJN = LJ+II
            ELSE
               IJN = LI+JJ
            ENDIF
C
            TEST=ABS(SHLDEN(IJN)*XINTS(JJJJ))
            IF (TEST.GT.CUTOFF) THEN
               IJ=IJ+1
               LDST(IJ)=JJ
               TS(NIJ)=TEST
               NIJ=NIJ+1
            ENDIF
C
C
C     ----- END OF SHELL LOOPS -----
C
  700    CONTINUE
         LSHL(II+1)=IJ+1
C
         LSIZE=LSHL(II+1)-LSHL(II)
         IF (LSIZE.EQ.0) GOTO 720
         CALL DSORT(LSIZE,TS,LDST(IJ-LSIZE+1))
C
         DO III=1,(LSIZE+1)/2
           ITEMP=LDST(IJ-LSIZE+III)
           LDST(IJ-LSIZE+III) = LDST(IJ-III+1)
           LDST(IJ-III+1) = ITEMP
         ENDDO
  720 CONTINUE
      RETURN
C
      END
C*MODULE QMFM    *DECK LEX
      SUBROUTINE LEX(SCFTYP,NINT,L1,L2,
     *               XINTS,NSH2,NSHL2,GHONDO,MAXG,DDIJ,
     *               IA,DA,FA,DB,FB,DSH,NXYZ,LSLIST,LDLIST,LSLN,
     *               LDLN,ISHELL,MLLIST,LMLPNT,IDXSHL,
     *               IPP,INDX2,IDXIJK,IDXWS,NCXYZ)
C
C       MANY ARGUMENTS ARE OPTIONAL, YOU MUST ALLOCATE STORAGE FOR
C              ALL CALLS: GHONDO, DDIJ, XINTS
C           CONVENTIONAL: BUFP, IX, AND POSSIBLY BUFK
C             DIRECT SCF: IA, DSH, DA, FA, AND POSSIBLY DB, FB
C     RESPONSE EQUATIONS: MUST DEFINE NXYZ.NE.1
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL OUT,GOPARR,DSKWRK,MASWRK,TFLAG,QFMM,QOPS,POPLE
C
C      COMPLEX*16 TGPL,TGPS,CYP,PYP,YTP,YTP2
C
      COMMON /QMFM  / SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *                ITERMS,QOPS,ISCUT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /SHLNOS/ QQ4,LIT,LJT,LKT,LLT,LOCI,LOCJ,LOCK,LOCL,
     *                MINI,MINJ,MINK,MINL,MAXI,MAXJ,MAXK,MAXL,
     *                NIJ,IJ,KL,IJKL
      COMMON /SHLT  / TOL,CUTOFF,ICOUNT,OUT
C
      DIMENSION XINTS(NSH2),
     *          GHONDO(MAXG),IA(L1),DA(L2),FA(L2),DB(L2),FB(L2),
     *          DSH(NSH2),DDIJ(*)
      DIMENSION NORGH(3)
      DIMENSION LDLIST(NSHL2),LSLIST(NSHL2),LSLN(ISHELL+1),
     *          MLLIST(2*NSHL2,2),LDLN(ISHELL+1),
     *          LMLPNT(ISHELL,2),IDXSHL(NSH2+1),IPP(NCXYZ,2),
     *          IDXWS(NCXYZ),IDXIJK(NCXYZ,3),INDX2(NCXYZ)
      DIMENSION IJTPP(100,2),KLTPP(100,2),IJTBLP(100+1),IJTBL(1000)
      DIMENSION IJSPP(100,2),KLSPP(100,2)
C
C     ----- TWO-ELECTRON INTEGRALS -----
C     ----- THIS VERSION CAN HANDLE G SHELLS -----
C
      POPLE=.FALSE.
      CALL BASCHK(LMAX)
                    NANGM =  4
      IF(LMAX.EQ.2) NANGM =  6
      IF(LMAX.EQ.3) NANGM = 10
      IF(LMAX.EQ.4) NANGM = 15
      NORGH(1) = 0
      NORGH(2) = NORGH(1) + NANGM**4
      NORGH(3) = NORGH(2) + NANGM**4
C
      CUTINT = CUTOFF
      ISHLCNT=0
      ISHRCNT=0
C
C     PARALLEL INITIALIZATION
C
      NEXT = -1
      MINE = -1
      IF (GOPARR) THEN
         TFLAG=DSKWRK
         DSKWRK=.TRUE.
      ENDIF
C
C     ----- I SHELL -----
C
      DO II = 1,ISHELL
C
C     ----- J SHELL -----
C
         INX=(II*II-II)/2
         JJMIN=LSLN(II)
         JJMAX=LSLN(II+1)-1
         DO JJA = JJMIN,JJMAX
            JJ=LSLIST(JJA)
            IF (JJ.GT.II) GOTO 1000
C
            IF (GOPARR) THEN
               MINE=MINE+1
               IF (MINE.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
               IF (NEXT.NE.MINE) THEN
                  GOTO 1000
               ENDIF
            ENDIF
C
            JNX=(JJ*JJ-JJ)/2
            IJIJ = INX + JJ
            ML=0
            IFLAG=0
C
C     ----- FIRST KL PAIRS -----
C
C     ----- K SHELL -----
C
             DO KKA=LDLN(II),LDLN(II+1)-1
                KK=LDLIST(KKA)
                IF (KK.GT.II) GOTO 150
                KNX=(KK*KK-KK)/2
                KNXD=INX+KK
C
C     ----- L SHELL -----
C
                DO LLA=LSLN(KK),LSLN(KK+1)-1
                   LL=LSLIST(LLA)
                   IF (LL.GT.KK) GOTO 50
                   KLKL=KNX+LL
                   TEST = ABS(DSH(KNXD)*XINTS(IJIJ)*XINTS(KLKL))
                   IF (TEST.GE.CUTINT) THEN
                      ML=ML+1
                      MLLIST(ML,1)=KK
                      MLLIST(ML,2)=LL
                   ELSE
                      GOTO 100
                   ENDIF
 50             ENDDO
 100            CONTINUE
                IF (ML.EQ.IFLAG) THEN
                   GOTO 200
                ELSE
                   IFLAG=ML
                ENDIF
 150         ENDDO
 200         CONTINUE
             IFLAG=ML
C
C     ----- SECOND KL PAIRS -----
C
C     ----- K SHELL -----
C
             DO KKA=LDLN(JJ),LDLN(JJ+1)-1
                KK=LDLIST(KKA)
                IF (KK.GT.II) GOTO 350
                KNX=(KK*KK-KK)/2
                IF (KK.GT.JJ) THEN
                   KNXD=KNX+JJ
                ELSE
                   KNXD=JNX+KK
                ENDIF
C
C     ----- L SHELL -----
C
                DO LLA=LSLN(KK),LSLN(KK+1)-1
                   LL=LSLIST(LLA)
                   IF (LL.GT.KK) GOTO 250
                   KLKL=KNX+LL
                   TEST = ABS(DSH(KNXD)*XINTS(IJIJ)*XINTS(KLKL))
                   IF (TEST.GE.CUTINT) THEN
                      ML=ML+1
                      MLLIST(ML,1)=KK
                      MLLIST(ML,2)=LL
                   ELSE
                      GOTO 300
                   ENDIF
 250            ENDDO
 300            CONTINUE
                IF (ML.EQ.IFLAG) THEN
                   GOTO 400
                ELSE
                   IFLAG=ML
                ENDIF
 350        ENDDO
 400        CONTINUE
C
            IF (ML.EQ.0) GOTO 1000
C
C           ------ SORT ------
C
            CALL IDIVIDEML(NSHL2,ML,ISHELL,MLLIST,LMLPNT,
     *           IXTBL)
C
C           ------ PURGE ------
C
            MLSS=0
            ITAG=MLLIST(1,1)
            JTAG=MLLIST(1,2)
            IF (ITAG.GT.JJ) THEN
               IF (.NOT.((II.EQ.ITAG).AND.(JTAG.GT.JJ)) ) THEN
                  MLSS=1
               ENDIF
            ELSE
               MLSS=1
            ENDIF
C
            DO I=1,ML
               IF (ITAG.NE.MLLIST(I,1)) THEN
                  ITAG=MLLIST(I,1)
                  JTAG=MLLIST(I,2)
                  IF (ITAG.GT.JJ) THEN
                     IF (.NOT.((II.EQ.ITAG).AND.(JTAG.GT.JJ))) THEN
                        MLSS=MLSS+1
                        MLLIST(MLSS,1)=ITAG
                        MLLIST(MLSS,2)=JTAG
                     ENDIF
                  ELSE
                     MLSS=MLSS+1
                     MLLIST(MLSS,1)=ITAG
                     MLLIST(MLSS,2)=JTAG
                  ENDIF
               ELSE
                  IF (JTAG.NE.MLLIST(I,2)) THEN
                     JTAG=MLLIST(I,2)
                     IF (ITAG.GT.JJ) THEN
                        IF (.NOT.((II.EQ.ITAG).AND.(JTAG.GT.JJ))) THEN
                           MLSS=MLSS+1
                           MLLIST(MLSS,1)=ITAG
                           MLLIST(MLSS,2)=JTAG
                        ENDIF
                     ELSE
                        MLSS=MLSS+1
                        MLLIST(MLSS,1)=ITAG
                        MLLIST(MLSS,2)=JTAG
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
C
C           ----- SCREENING FOR THE DUPLICATED INTEGRALS -----
C
C           ----- PREPARE INTEGRAL INDEX OF I AND J SHELLS -----
C
            IF (ITERMS.EQ.0) THEN
               IPST=IDXSHL(INX+JJ)+1
               IPED=IDXSHL(INX+JJ+1)
               DO IJPP=IPST,IPED
                  ISTRT=IJPP-IPST+1
                  IJTPP(ISTRT,1)=IPP(IJPP,1)
                  IJTPP(ISTRT,2)=IPP(IJPP,2)
               ENDDO
               IJSIZE=IPED-IPST+1
               CALL GETSHL(1,II,JJ,1,1,100,
     *            IJTPP,IJSPP,IJSIZE,IJSIZE)
               CALL GETIJPP(DDIJ,IJSIZE,100,IJSPP)
               IEXCH=1
               Q4 = 1.0D+00
               QQ4 = Q4
C
               ISHLCNT=ISHLCNT+MLSS
               IJTBLP(1)=1
               DO KLSHLL=1,MLSS
                  KK=MLLIST(KLSHLL,1)
                  KNX=(KK*KK-KK)/2
                  LL=MLLIST(KLSHLL,2)
C
                  JPST=IDXSHL(KNX+LL)+1
                  JPED=IDXSHL(KNX+LL+1)
                  KLCNT=0
                  IJCNT=0
                  IKLFLAG=IJCNT
                  DO KLBOX=JPST,JPED
                     LBOX=INDX2(KLBOX)
                     IBOXJ=IDXIJK(LBOX,1)
                     JBOXJ=IDXIJK(LBOX,2)
                     KBOXJ=IDXIJK(LBOX,3)
                     DO IJBOX=IPST,IPED
                        MBOX=INDX2(IJBOX)
                        IBOXI=IDXIJK(MBOX,1)
                        JBOXI=IDXIJK(MBOX,2)
                        KBOXI=IDXIJK(MBOX,3)
C
                        ID=ABS(IBOXI-IBOXJ)
                        JD=ABS(JBOXI-JBOXJ)
                        KD=ABS(KBOXI-KBOXJ)
                        IJWS=(IDXWS(MBOX)+IDXWS(LBOX))/2
                        IF ((ID.GT.IJWS).OR.(JD.GT.IJWS).OR.
     *                     (KD.GT.IJWS)) THEN
                           IJCNT=IJCNT+1
                           IJTBL(IJCNT)=IJBOX-IPST+1
                        ENDIF
                     ENDDO
                     IF (IJCNT.GT.IKLFLAG) THEN
                        IKLFLAG=IJCNT
                        KLCNT=KLCNT+1
                        KLTPP(KLCNT,1)=IPP(KLBOX,1)
                        KLTPP(KLCNT,2)=IPP(KLBOX,2)
                        IJTBLP(KLCNT+1)=IJCNT+1
                     ENDIF
                  ENDDO
C
                  IF (IJCNT.GT.0) THEN
                     ISHRCNT=ISHRCNT+1
                     KLSIZE=JPED-JPST+1
                     IF(NIJ.EQ.0) GO TO 840
                     CALL GETSHL(2,II,JJ,KK,LL,100,
     *                  KLTPP,KLSPP,KLSIZE,KLSIZE)
                     CALL ZQOUT(GHONDO)
C
C           ----- DO INTEGRAL BATCH, SSSS IS A SPECIAL CASE -----
C
                     IF(IJKL.EQ.1) THEN
                        CALL S0000X(GHONDO,DDIJ,KLSPP,
     *                     KLCNT,IJTBLP,IJTBL)
                     ELSE
                        CALL HONDOX(GHONDO,DDIJ,KLSPP,
     *                     KLCNT,IJTBLP,IJTBL)
                     END IF
                        CALL FORMK(SCFTYP,1,POPLE,IA,DA,
     *                     FA,DB,FB,GHONDO,L2,NINT,NXYZ)
                  ENDIF
 840           ENDDO
C
C--------------------------------------------------------------
            ELSE
               CALL SHELLS(1,II,JJ,1,1,.TRUE.)
               CALL IJPRIM(DDIJ)
C
C           ----- DELAY THE SHORT-RANGE INTEGRAL -----
C
               IEXCH=1
               Q4 = 1.0D+00
               QQ4 = Q4
C
               ISHLCNT=ISHLCNT+MLSS
               DO KLSHLL=1,MLSS
                  KK=MLLIST(KLSHLL,1)
                  LL=MLLIST(KLSHLL,2)
C
C                 QFMM USES ONLY HONDO CODE
C
                  CALL SHELLS(2,II,JJ,KK,LL,.TRUE.)
                  CALL ZQOUT(GHONDO)
C
C        ----- DO INTEGRAL BATCH, SSSS IS A SPECIAL CASE -----
C
                  IF(IJKL.EQ.1) THEN
                     CALL S0000(GHONDO,DDIJ)
                  ELSE
                     CALL GENRAL(GHONDO,DDIJ)
                  END IF
                  CALL FORMK(SCFTYP,IEXCH,POPLE,IA,DA,FA,DB,FB,
     *                GHONDO,L2,NINT,NXYZ)
               ENDDO
               ISHRCNT=ISHLCNT
            ENDIF
 1000    ENDDO
      ENDDO
      IF (GOPARR) THEN
         CALL DDI_SYNC(ITAG)
         CALL DDI_DLBRESET
         DSKWRK=TFLAG
      ENDIF
      RETURN
      END
C*MODULE QMFM    *DECK Q_FMM
      SUBROUTINE Q_FMM(SCFTYP,NCXYZ,IYP,INDX2,IDXIJK,IDXWS,CXYZ,
     *   IBS,IYZTBL,NTMPL,YP,ZP,L2,DMAT,DMATB,NTBOX,MAXWS,IYZPNT,
     *   F,G,ZLL,CLM,FLM,MAXNYP,TMPGPS,TMPGPL,IDXBOX,NSBOX,
     *   NZ,FAO,FBO,NFTPL,NFTPLT)
C
C     MAIN DRIVER OF QFMM (QUANTUM FAST MULTIPOLE METHOD)
C
C     THIS ROUTINE CONSTRUCTS THE COULOMB MATRIX ON THE BASIS OF
C     LINEAR SCALING ALGORITHM.
C
C     C. H. CHOI OCT 2000
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMPLEX*16 YP,ZP,TMPGPL,TMPGPS
      LOGICAL QFMM,QOPS,GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /QMFM  /SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *       ITERMS,QOPS,ISCUT
C
      DIMENSION IYP(NCXYZ),CXYZ(NCXYZ,3),IBS(NCXYZ,4),IDXIJK(NCXYZ,3),
     *   TMPGPL(*),INDX2(NCXYZ),TMPGPS(*),IDXWS(NCXYZ),
     *   IYZTBL(2**(NS+1)-1,2**(NS+1)-1,2**(NS+1)-1),
     *   F((NP*(NP+1)*(NP+2)*4/3)+(NP+1)),CLM(-NP:NP),
     *   G((NP*(NP+1)*(NP+2)*4/3)+(NP+1)),FLM(-NP:NP),
     *   ZLL(0:2*NP+1),YP((NP+1)*(NP+2)/2,NTMPL),
     *   ZP((NP+1)*(NP+2)/2,NTMPL),DMAT(L2),DMATB(L2),
     *   IYZPNT(NTBOX,MAXWS/2)
      DIMENSION FAO(L2),FBO(L2),IDXBOX(3,NTBOX),NSBOX(20)
C
C     PHASE I : OBTAIN THE MULTIPOLE EXPANSION OF NON-EMPTY BOXS IN THE
C               LOWEST SUBDIVISION LEVEL, NS.
C
      IF (MPMTHD.EQ.0) THEN
         CALL GETY_DISK(SCFTYP,NCXYZ,IYP,INDX2,IDXIJK,IDXWS,IBS,
     *      IYZTBL,NTMPL,YP,L2,DMAT,DMATB,NTBOX,MAXWS,IYZPNT,MAXNYP,
     *      TMPGPL,NFTPLT)
      ELSEIF (MPMTHD.EQ.1) THEN
         CALL GETY_SEMI(NCXYZ,IYP,INDX2,IDXIJK,IDXWS,CXYZ,IBS,IYZTBL,
     *      NTMPL,YP,L2,DMAT,NTBOX,MAXWS,IYZPNT,F,G,ZLL,CLM,FLM,
     *      MAXNYP,TMPGPS,NFTPL,NFTPLT)
      ENDIF
C
      ILYP=(NP+1)*(NP+2)*NTMPL
      IF(GOPARR) THEN
         CALL DDI_GSUMF(4000,YP,ILYP)
         CALL DDI_DLBRESET
      END IF
C
C     PHASE II : OBTAIN THE LONG-RANGE POTENTIAL USING FMM
C     THIS PROCESS IS COMPOSED OF C2P, P2P AND P2C.
C
      CALL C2P(IYZTBL,F,G,CLM,FLM,ZLL,NTMPL,YP,NTBOX,IDXBOX,NSBOX,
     *   MAXWS,IYZPNT,IC2P)
C
      CALL SP2P(IYZTBL,F,G,CLM,FLM,ZLL,NTMPL,YP,ZP,NTBOX,IDXBOX,
     *   NSBOX,MAXWS,IYZPNT,IP2P)
      IF(GOPARR) THEN
         CALL DDI_GSUMF(4001,ZP,ILYP)
         CALL DDI_GSUMI(4002,IP2P,1)
         CALL DDI_DLBRESET
      END IF
C
      CALL P2C(IYZTBL,F,G,CLM,FLM,ZLL,NTMPL,ZP,IP2C,NTBOX,MAXWS,IYZPNT)
C
C    PHASE III: CONSTRUCT THE COULOMB MATRIX OF THE LONG-RANGE POTENTIAL
C
      IF (MPMTHD.EQ.0) THEN
         CALL FORMFJ_DISK(SCFTYP,NCXYZ,IYP,INDX2,IDXIJK,IBS,L2,FAO,FBO,
     *                    NTMPL,IYZTBL,ZP,NTBOX,MAXWS,IYZPNT,IDXWS,
     *                    MAXNYP,TMPGPL,NFTPLT)
      ELSEIF (MPMTHD.EQ.1) THEN
         CALL FORMFJ_SEMI(NCXYZ,IYP,INDX2,IDXIJK,CXYZ,IBS,L2,FAO,NZ,
     *                    IYZTBL,ZP,NTBOX,MAXWS,IYZPNT,IDXWS,F,G,ZLL,
     *                    CLM,FLM,MAXNYP,TMPGPS,NFTPLT)
      ENDIF
C
      RETURN
      END
C
