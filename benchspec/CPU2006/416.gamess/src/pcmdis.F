C  8 Oct 01 - HL  - parallelize PCM
C  6 Sep 01 - MWS - add dummy arguments to nameio call
C 29 Dec 00 - MWS - precede stop statements by abrt calls
C 11 Oct 00 - PB,BM - interfaced EFP+PCM
C 25 AUG 00 - BM  - added IEF solvation model
C 18 MAR 97 - CA,BM - new module to calculate SC PCM dispersion energy
C
C     authors: C. Amovilli and B. Mennucci
C
C*MODULE PCMDIS  *DECK ENLBS
      SUBROUTINE ENLBS(NKTYP,XYZE,MXDBS,NUMBX)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      DIMENSION XYZE(4,MXDBS),NKTYP(MXDBS)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMDBS/ NADD,NUMB
C
      PARAMETER (HALF=0.5D+00, ZERO=0.0D+00,
     *           PT75=0.75D+00, PT187=1.875D+00, PT6562=6.5625D+00)
C
      PARAMETER (NNAM=3)
      DIMENSION QNAM(NNAM),KQNAM(NNAM)
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA ENBS/8HDISBS   /
#else
      CHARACTER*8 :: ENBS_STR
      EQUIVALENCE (ENBS, ENBS_STR)
      DATA ENBS_STR/"DISBS   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA QNAM/8HXYZE    ,8HNKTYP   ,8HNADD    /
#else
      CHARACTER*8 :: QNAM_STR(NNAM)
      EQUIVALENCE (QNAM, QNAM_STR)
      DATA QNAM_STR/"XYZE    ","NKTYP   ","NADD    "/
#endif
      DATA KQNAM/-3,-1,1/
C
      KQNAM(1) = 3 + 10*4*MXDBS
      KQNAM(2) = 1 + 10*MXDBS
C
C        the default is an empty auxiliary basis
C
      NADD=0
      DO 50 I=1,MXDBS
         NKTYP(I) = 0
   50 CONTINUE
      CALL VCLR(XYZE,1,4*MXDBS)
C
      JRET=0
      CALL NAMEIO(IR,JRET,ENBS,NNAM,QNAM,KQNAM,
     *            XYZE,NKTYP,NADD,
     *            0,
     *    0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,
     *    0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,
     *    0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0,  0,0,0,0,0)
C
      IF(JRET.EQ.2) THEN
         IF(MASWRK) WRITE(IW,930) ENBS
         CALL ABRT
      END IF
C
      IF(MASWRK.AND.NADD.NE.0) THEN
         WRITE(IW,950)
         DO J=1,NADD
            WRITE(IW,55) NKTYP(J),(XYZE(K,J),K=1,4)
         ENDDO
      END IF
C
      PI=4.0D+00*ATAN(1.0D+00)
      PI32 = PI * SQRT(PI)
C
C        extend the basis set with auxiliary functions for dispersion
C
      INAT=NAT
      KLOC(NSHELL+1)=KLOC(NSHELL)+KMAX(NSHELL)-KMIN(NSHELL)+1
C
      DO I=NSHELL+1,NSHELL+NADD
C
      IF(I.GT.MXSH) THEN
         IF(MASWRK) WRITE(IW,*)
     *      'ENLBS: EXCEEDED MAXIMUM SHELL COUNT'
         CALL ABRT
         STOP
      END IF
C
      INAT=INAT+1
      IF(INAT.GT.MXATM) THEN
         IF(MASWRK) WRITE(IW,*) 'ENLBS: EXCEEDED MAXIMUM ATOM COUNT'
         CALL ABRT
         STOP
      END IF
C
      II=I-NSHELL
      EXPO=XYZE(4,II)
      IF(EXPO.LE.ZERO) THEN
         IF(MASWRK) WRITE(IW,*)
     *      'ZERO EXPONENT FOUND IN DISPERSION BASIS SET'
         IF(MASWRK) WRITE(IW,*) 'PLEASE FIX $DISBS AND RESUBMIT'
         CALL ABRT
      END IF
C
      C(1,INAT)=XYZE(1,II)
      C(2,INAT)=XYZE(2,II)
      C(3,INAT)=XYZE(3,II)
      KATOM(I)=INAT
      KSTART(I)=KSTART(I-1)+KNG(I-1)
      IF(KSTART(I).GT.MXGTOT) THEN
         IF(MASWRK) WRITE(IW,*)
     *       'ENLBS: EXCEEDED MAXIMUM GAUSSIAN PRIMITIVE COUNT'
         CALL ABRT
         STOP
      END IF
      KNG(I)=1
      EX(KSTART(I))=EXPO
      CS(KSTART(I))=0.0D+00
      CP(KSTART(I))=0.0D+00
      CD(KSTART(I))=0.0D+00
      CF(KSTART(I))=0.0D+00
      CG(KSTART(I))=0.0D+00
      KTYPE(I)=NKTYP(I-NSHELL)+1
      EE = EXPO*2.0D+00
      FACS = PI32/(EE*SQRT(EE))
      FACP = HALF*FACS/EE
      FACD = PT75*FACS/(EE*EE)
      FACF = PT187*FACS/(EE**3)
      FACG = PT6562*FACS/(EE**4)
C
      GO TO (1,2,3,4,5), KTYPE(I)
C
 1    KMIN(I)=1
      KMAX(I)=1
      KLOC(I+1)=KLOC(I)+1
         CS(KSTART(I)) = 1.0D+00/SQRT(FACS)
      GO TO 10
 2    KMIN(I)=2
      KMAX(I)=4
      KLOC(I+1)=KLOC(I)+3
         CP(KSTART(I)) = 1.0D+00/SQRT(FACP)
      GO TO 10
 3    KMIN(I)=5
      KMAX(I)=10
      KLOC(I+1)=KLOC(I)+6
         CD(KSTART(I)) = 1.0D+00/SQRT(FACD)
      GO TO 10
 4    KMIN(I)=11
      KMAX(I)=20
      KLOC(I+1)=KLOC(I)+10
         CF(KSTART(I)) = 1.0D+00/SQRT(FACF)
      GO TO 10
 5    KMIN(I)=21
      KMAX(I)=35
      KLOC(I+1)=KLOC(I)+15
         CG(KSTART(I)) = 1.0D+00/SQRT(FACG)
 10   CONTINUE
      ENDDO
C
      NUMB=NUM
      DO I=1,NADD
         NUMB=NUMB+1+NKTYP(I)*(NKTYP(I)+3)/2
      ENDDO
      IF(MASWRK) WRITE(IW,*)
     *      ' EXTENDED BASIS SET DIMENSION:',NUMB
      IF(MASWRK)WRITE(IW,*)
C
      NUMBX = NUMB
      RETURN
C
  930 FORMAT(1X,'**** ERROR IN $',A8,' INPUT')
  950 FORMAT(/3X,'BASIS SET INPUT FOR DISPERSION CALCULATION '/3X,
     *        43(1H-)/3X,'TYPE         COORDINATES            EXPON. '/)
  55  FORMAT(I4,4(2X,F8.4))
      END
C*MODULE PCMDIS  *DECK DBSINV
      SUBROUTINE DBSINV(SINV,SSQR,IPVT,WORK,NUMB,NUMB2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION SINV(NUMB2),SSQR(NUMB,NUMB),IPVT(NUMB),WORK(NUMB)
C
C   hui li
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
C
C   hui li
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C        generate the inverse of the overlap matrix for
C        the original basis set plus the dispersion basis set.
C
C        generate overlap matrix for the combined basis set
C
      CALL SBIG(SINV,NUMB2)
C
C        copy S matrix to square storage
C
      CALL CPYTSQ(SINV,SSQR,NUMB,1)
C
C        generate s-inverse matrix
C
      INFO=0
      CALL DGEFA(SSQR,NUMB,NUMB,IPVT,INFO)
      IF(INFO.NE.0) THEN
         IF(MASWRK) WRITE(IW,*) ' DBSINV: S MATRIX IS SINGULAR'
         CALL ABRT
         STOP
      END IF
      CALL DGEDI(SSQR,NUMB,NUMB,IPVT,DET,WORK,01)
C
C        copy s-inverse to triangular storage
C
      CALL CPYSQT(SSQR,SINV,NUMB,1)
      RETURN
      END
C*MODULE PCMDIS  *DECK INTVE
      SUBROUTINE INTVE(SINV,VX,EX,A,POT,FLD,FLW,NUMB,NUMB2,L3)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C        on entry, sinv is -numb- triangle,
C        on exit, it is -num- triangle.
C        note that numb exceeds num.
C
      DIMENSION SINV(*),VX(NUMB),EX(NUMB),A(L3),POT(NUMB2),
     *          FLD(NUMB2,3),FLW(225,3)
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /ELPROP/ ELDLOC,ELMLOC,ELPLOC,ELFLOC,
     *                IEDEN,IEMOM,IEPOT,IEFLD,MODENS,
     *                IEDOUT,IEMOUT,IEPOUT,IEFOUT,
     *                IEDINT,IEMINT,IEPINT,IEFINT
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
      COMMON /XYZPRP/ XP,YP,ZP,DMY(35)
C
      DATA ZERO/0.0D+00/
C
      IEFSAV=IEFLD
      IEFLD=1
C
C  1) Calculation of electric field integrals on representative
C     points of tesserae.
C
      CALL VCLR(A,1,L3)
      DO ITS = 1, NTS
        ITS2=ITS+NTS
        XP=XCTS(ITS)
        YP=YCTS(ITS)
        ZP=ZCTS(ITS)
        CNX=AS(ITS)*(XP-XCTS(ITS2))/DR
        CNY=AS(ITS)*(YP-YCTS(ITS2))/DR
        CNZ=AS(ITS)*(ZP-ZCTS(ITS2))/DR
        CALL PRCALC2(FLD,FLW,3,NUMB2)
        CALL INTMEP2(POT,XP,YP,ZP)
C
        DO I=1,NUM
           IT = (I*I-I)/2
           DO J=1,NUM
              JT = (J*J-J)/2
              DO IBAS = 1, NUMB
                 IBAST = (IBAS*IBAS-IBAS)/2
                 VX(IBAS)=ZERO
                 JIBAS= JT + IBAS
                 IF(J.LT.IBAS) JIBAS = IBAST + J
                 DO JBAS = 1, NUMB
                    JBAST = (JBAS*JBAS-JBAS)/2
                    IJBAS= IT + JBAS
                    IF(I.LT.JBAS) IJBAS = JBAST + I
                    IJBB= IBAST + JBAS
                    IF(IBAS.LT.JBAS) IJBB = JBAST + IBAS
                    VX(IBAS) = VX(IBAS) - POT(IJBAS)*SINV(IJBB)
                 ENDDO
                 EX(IBAS) = FLD(JIBAS,1)*CNX
     *                    + FLD(JIBAS,2)*CNY
     *                    + FLD(JIBAS,3)*CNZ
               ENDDO
C
               AA=ZERO
               DO IBAS=1,NUMB
                  AA = AA + VX(IBAS)*EX(IBAS)
               ENDDO
C
               IJ=J+(I-1)*NUM
               A(IJ) = A(IJ) + AA
            ENDDO
         ENDDO
      ENDDO
C
      IEFLD=IEFSAV
C
C     MWS cannot tell what is being stored into -SINV-...
C     This array is called -VEF- where it is used in routine DISP.
C
      IJ=0
      DO I=1,NUM
         DO J=1,I
            IJ=IJ+1
            IJ1=J+(I-1)*NUM
            IJ2=I+(J-1)*NUM
            SINV(IJ) = (A(IJ1)+A(IJ2))/2.0D+00
         ENDDO
      ENDDO
C
      RETURN
      END
C*MODULE PCMDIS  *DECK DISP
      SUBROUTINE DISP(XD,POT,FLD,DIS1,DIS2,BK,COL1,L1,L2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXTS=2500, MXTSPT=2*MXTS, MXAO=2047)
C
      DIMENSION POT(L2),XD(L2),DIS1(L2),DIS2(L2),BK(L1,L1),
     *          COL1(L1)
      DIMENSION FLD(L2,3),FLW(225,3)
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /ELPROP/ ELDLOC,ELMLOC,ELPLOC,ELFLOC,
     *                IEDEN,IEMOM,IEPOT,IEFLD,MODENS,
     *                IEDOUT,IEMOUT,IEPOUT,IEFOUT,
     *                IEDINT,IEMINT,IEPINT,IEFINT
      COMMON /XYZPRP/ XP,YP,ZP,DMY(35)
      COMMON /PCMPAR/ IPCM,NFT26,NFT27,IKREP,IEF,IP_F
      COMMON /PCMDIS/ WB,WA,ETA2,GD,EVAC,IDP
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
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
      CALL VCLR(DIS1,1,L2)
      CALL VCLR(DIS2,1,L2)
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
                  IST=IA(MAX(IDS,IDT)) + MIN(IDS,IDT)
                  BB=BB-POT(IRS)*XD(IST)
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
         IJ=0
         DO I=1,L1
            DO J=1,I
               IJ=IJ+1
               DIS2(IJ)=DIS2(IJ)+WK*(BK(I,J)+BK(J,I))
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
C        now use -pot- to buffer in the -vef- matrix
C
      CALL DAREAD(IDAF,IODA,POT,L2,331,0)
      IJ=0
      DO I=1,L1
         DO J=1,I
            IJ=IJ+1
            DIS1(IJ)=-CORF*FACT*POT(IJ)
         ENDDO
      ENDDO
C
C     DISPERSION FREE ENERGY CONTRIBUTION
C
      GD1=TRACEP(XD,DIS1,L1)
      GD2=TRACEP(XD,DIS2,L1)
      GD=GD1+0.5D+00*GD2
C
      RETURN
      END
C*MODULE PCMDIS  *DECK JMATDIS
      SUBROUTINE JMATDIS(XSCR,XH1,XREP,XVEC,CEL,L1,NUM2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION XSCR(NUM2),XREP(NUM2),XH1(NUM2),XVEC(NUM2),CEL(NUM2)
C
      PARAMETER (MXTS=2500, MXTSPT=2*MXTS)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMPAR/ IPCM,NFT26,NFT27,IKREP,IEF,IP_F
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
C
      NUM2 = (L1*L1+L1)/2
C
      CALL VCLR(XVEC,1,NUM2)
C
      DO ITS = 1, NTS
         CALL INTMEP(XSCR,XCTS(ITS),YCTS(ITS),ZCTS(ITS))
         DO IBAS = 1, NUM2
            XVEC(IBAS) = XVEC(IBAS) - XSCR(IBAS) * QSN(ITS)
         ENDDO
      ENDDO
C
C Add repulsion potential
C
      IF(IKREP.EQ.1) THEN
         CALL DAREAD(IDAF,IODA,XSCR,NUM2,12,0)
         CALL DAREAD(IDAF,IODA,CEL,NUM2,330,0)
         CALL POTREP(XREP,XSCR,CEL,NUM2)
      END IF
C
      DO IBAS = 1, NUM2
         IF(IKREP.EQ.1) XVEC(IBAS)=XVEC(IBAS)+XREP(IBAS)
         XH1(IBAS) = XH1(IBAS) + XVEC(IBAS)
      ENDDO
C
C     Write one-electron integrals modified
C
      CALL DAWRIT(IDAF,IODA,XH1,NUM2,11,0)
      RETURN
      END
C*MODULE PCMDIS  *DECK WTRANSA
      SUBROUTINE WTRANSA(EIM,NUM)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION EIM(*)
C
C   hui li
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
C   hui li
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      COMMON /PCMDIS/ WB,WA,ETA2,GD,EVAC,IDP
C
      DATA ZERO /0.0D+00/
C
C -- Calculate transition frequency of the solute molecule
C
      DELTA=1.1D+00
      I1=0
      I2=0
      EM=ZERO
      EI=ZERO
      DO I=1,NUM
         IF(EIM(I).LE.DELTA.AND.EIM(I).GE.-DELTA) THEN
            IF(EIM(I).LT.ZERO) EI=EI+EIM(I)
            IF(EIM(I).LT.ZERO) I1=I1+1
            IF(EIM(I).GT.ZERO) EM=EM+EIM(I)
            IF(EIM(I).GT.ZERO) I2=I2+1
         END IF
      ENDDO
      IF(I2.EQ.0) THEN
         IF(MASWRK) WRITE(IW,*)
     *       'ATTENTION: VIRTUAL ORBITALS NOT USED'
         EM=DELTA
         I2=1
      END IF
      WA = EM/I2 - EI/I1
C
      IF(MASWRK) WRITE(IW,*) 'INTRODUCTION OF THE SOLVENT'
      IF(MASWRK) WRITE(IW,*) 'TRANSITION FREQUENCY.',WA
      IF(MASWRK) WRITE(IW,*)
C
      RETURN
      END
C*MODULE PCMDIS  *DECK SBIG
      SUBROUTINE SBIG(S,NUMB2)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL IANDJ,NORM,DOUBLE
C
      DIMENSION S(NUMB2)
      DIMENSION SBLK(225),DIJ(225),
     *          IJX(225),IJY(225),IJZ(225),
     *          XIN(125),YIN(125),ZIN(125),
     *          IX(35),IY(35),IZ(35),JX(35),JY(35),JZ(35)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PCMDBS/ NADD,NUMB
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /STV   / XINT,YINT,ZINT,TAA,X0,Y0,Z0,
     *                XI,YI,ZI,XJ,YJ,ZJ,NI,NJ
      COMMON /SYMIND/ TOL,II,JJ,LIT,LJT,MINI,MINJ,MAXI,MAXJ,IANDJ
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00,
     *           SQRT3=1.73205080756888D+00,
     *           SQRT5=2.23606797749979D+00, SQRT7=2.64575131106459D+00,
     *           RLN10=2.30258D+00)
C
      DATA JX / 0, 1, 0, 0, 2, 0, 0, 1, 1, 0,
     *          3, 0, 0, 2, 2, 1, 0, 1, 0, 1,
     *          4, 0, 0, 3, 3, 1, 0, 1, 0, 2,
     *          2, 0, 2, 1, 1/
      DATA IX / 1, 6, 1, 1,11, 1, 1, 6, 6, 1,
     *         16, 1, 1,11,11, 6, 1, 6, 1, 6,
     *         21, 1, 1,16,16, 6, 1, 6, 1,11,
     *         11, 1,11, 6, 6/
      DATA JY / 0, 0, 1, 0, 0, 2, 0, 1, 0, 1,
     *          0, 3, 0, 1, 0, 2, 2, 0, 1, 1,
     *          0, 4, 0, 1, 0, 3, 3, 0, 1, 2,
     *          0, 2, 1, 2, 1/
      DATA IY / 1, 1, 6, 1, 1,11, 1, 6, 1, 6,
     *          1,16, 1, 6, 1,11,11, 1, 6, 6,
     *          1,21, 1, 6, 1,16,16, 1, 6,11,
     *          1,11, 6,11, 6/
      DATA JZ / 0, 0, 0, 1, 0, 0, 2, 0, 1, 1,
     *          0, 0, 3, 0, 1, 0, 1, 2, 2, 1,
     *          0, 0, 4, 0, 1, 0, 1, 3, 3, 0,
     *          2, 2, 1, 1, 2/
      DATA IZ / 1, 1, 1, 6, 1, 1,11, 1, 6, 6,
     *          1, 1,16, 1, 6, 1, 6,11,11, 6,
     *          1, 1,21, 1, 6, 1, 6,16,16, 1,
     *         11,11, 6, 6,11/
C
C     ----- COMPUTE CONVENTIONAL  S INTEGRALS -----
C
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
      CALL VCLR(S,1,NUMB2)
C
C     ----- I SHELL -----
C
      DO 720 II = 1,NSHELL+NADD
         I = KATOM(II)
         XI = C(1,I)
         YI = C(2,I)
         ZI = C(3,I)
         I1 = KSTART(II)
         I2 = I1+KNG(II)-1
         LIT = KTYPE(II)
         MINI = KMIN(II)
         MAXI = KMAX(II)
         LOCI = KLOC(II)-MINI
C
C     ----- J SHELL -----
C
         DO 700 JJ = 1,II
            J = KATOM(JJ)
            XJ = C(1,J)
            YJ = C(2,J)
            ZJ = C(3,J)
            J1 = KSTART(JJ)
            J2 = J1+KNG(JJ)-1
            LJT = KTYPE(JJ)
            MINJ = KMIN(JJ)
            MAXJ = KMAX(JJ)
            LOCJ = KLOC(JJ)-MINJ
            NROOTS = (LIT+LJT-2)/2+1
            RR = (XI-XJ)**2+(YI-YJ)**2+(ZI-ZJ)**2
            IANDJ = II .EQ. JJ
C
C     ----- PREPARE INDICES FOR PAIRS OF (I,J) FUNCTIONS
C
            IJ = 0
            MAX = MAXJ
            DO 160 I = MINI,MAXI
               NX = IX(I)
               NY = IY(I)
               NZ = IZ(I)
               IF (IANDJ) MAX = I
               DO 140 J = MINJ,MAX
                  IJ = IJ+1
                  IJX(IJ) = NX+JX(J)
                  IJY(IJ) = NY+JY(J)
                  IJZ(IJ) = NZ+JZ(J)
  140          CONTINUE
  160       CONTINUE
C
            DO 180 I = 1,IJ
               SBLK(I) = ZERO
  180       CONTINUE
C
C     ----- I PRIMITIVE
C
            JGMAX = J2
            DO 520 IG = I1,I2
               AI = EX(IG)
               ARRI = AI*RR
               AXI = AI*XI
               AYI = AI*YI
               AZI = AI*ZI
               CSI = CS(IG)
               CPI = CP(IG)
               CDI = CD(IG)
               CFI = CF(IG)
               CGI = CG(IG)
C
C     ----- J PRIMITIVE
C
               IF (IANDJ) JGMAX = IG
               DO 500 JG = J1,JGMAX
                  AJ = EX(JG)
                  AA = AI+AJ
                  AA1 = ONE/AA
                  DUM = AJ*ARRI*AA1
                  IF (DUM .GT. TOL) GO TO 500
                  FAC = EXP(-DUM)
                  CSJ = CS(JG)
                  CPJ = CP(JG)
                  CDJ = CD(JG)
                  CFJ = CF(JG)
                  CGJ = CG(JG)
                  AX = (AXI+AJ*XJ)*AA1
                  AY = (AYI+AJ*YJ)*AA1
                  AZ = (AZI+AJ*ZJ)*AA1
C
C     ----- DENSITY FACTOR
C
                  DOUBLE=IANDJ.AND.IG.NE.JG
                  MAX = MAXJ
                  NN = 0
                  DUM1 = ZERO
                  DUM2 = ZERO
                  DO 220 I = MINI,MAXI
                     IF (I.EQ.1) DUM1=CSI*FAC
                     IF (I.EQ.2) DUM1=CPI*FAC
                     IF (I.EQ.5) DUM1=CDI*FAC
                     IF ((I.EQ. 8).AND.NORM) DUM1=DUM1*SQRT3
                     IF (I.EQ.11) DUM1=CFI*FAC
                     IF ((I.EQ.14).AND.NORM) DUM1=DUM1*SQRT5
                     IF ((I.EQ.20).AND.NORM) DUM1=DUM1*SQRT3
                     IF (I.EQ.21) DUM1=CGI*FAC
                     IF ((I.EQ.24).AND.NORM) DUM1=DUM1*SQRT7
                     IF ((I.EQ.30).AND.NORM) DUM1=DUM1*SQRT5/SQRT3
                     IF ((I.EQ.33).AND.NORM) DUM1=DUM1*SQRT3
                     IF (IANDJ) MAX = I
                     DO 200 J = MINJ,MAX
                        IF (J.EQ.1) THEN
                           DUM2=DUM1*CSJ
                           IF (DOUBLE) THEN
                              IF (I.LE.1) THEN
                                 DUM2=DUM2+DUM2
                              ELSE
                                 DUM2=DUM2+CSI*CPJ*FAC
                              END IF
                           END IF
                        ELSE IF (J.EQ.2) THEN
                           DUM2=DUM1*CPJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF (J.EQ.5) THEN
                           DUM2=DUM1*CDJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((J.EQ.8).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        ELSE IF (J.EQ.11) THEN
                           DUM2=DUM1*CFJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((J.EQ.14).AND.NORM) THEN
                           DUM2=DUM2*SQRT5
                        ELSE IF ((J.EQ.20).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        ELSE IF (J.EQ.21) THEN
                           DUM2=DUM1*CGJ
                           IF (DOUBLE) DUM2=DUM2+DUM2
                        ELSE IF ((J.EQ.24).AND.NORM) THEN
                           DUM2=DUM2*SQRT7
                        ELSE IF ((J.EQ.30).AND.NORM) THEN
                           DUM2=DUM2*SQRT5/SQRT3
                        ELSE IF ((J.EQ.33).AND.NORM) THEN
                           DUM2=DUM2*SQRT3
                        END IF
                        NN = NN+1
                        DIJ(NN) = DUM2
  200                CONTINUE
  220             CONTINUE
C
C     ----- OVERLAP
C
                  TAA = SQRT(AA1)
                  X0 = AX
                  Y0 = AY
                  Z0 = AZ
                  IN = -5
                  DO 320 I = 1,LIT
                     IN = IN+5
                     NI = I
                     DO 300 J = 1,LJT
                        JN = IN+J
                        NJ = J
                        CALL STVINT
                        XIN(JN) = XINT*TAA
                        YIN(JN) = YINT*TAA
                        ZIN(JN) = ZINT*TAA
  300                CONTINUE
  320             CONTINUE
                  DO 340 I = 1,IJ
                     NX = IJX(I)
                     NY = IJY(I)
                     NZ = IJZ(I)
                     YZ = YIN(NY)*ZIN(NZ)
                     DUM = YZ*XIN(NX)
                     SBLK(I) = SBLK(I) + DIJ(I)*DUM
  340             CONTINUE
C
  500      CONTINUE
  520      CONTINUE
C
C     ----- COPY BLOCK INTO  OVERLAP MATRIX
C
            MAX = MAXJ
            NN = 0
            DO 620 I = MINI,MAXI
               LI = LOCI+I
               IN = (LI*(LI-1))/2
               IF (IANDJ) MAX = I
               DO 600 J = MINJ,MAX
                  LJ = LOCJ+J
                  JN = LJ+IN
                  NN = NN+1
                  S(JN) = SBLK(NN)
  600          CONTINUE
  620       CONTINUE
C
C     ----- END OF SHELL LOOPS -----
C
  700    CONTINUE
  720 CONTINUE
C
      RETURN
      END
C*MODULE PCMDIS  *DECK PRCALC2
      SUBROUTINE PRCALC2(XVAL,WINT,NVAL,L2)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL IANDJ,NORM,DOUBLE
C
      DIMENSION XVAL(NVAL*L2),WINT(NVAL*225)
      DIMENSION IX(35),IY(35),IZ(35),JX(35),JY(35),JZ(35),
     *          DIJ(225),IJX(225),IJY(225),IJZ(225)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PCMDBS/ NADD,NUMB
      COMMON /XYZORB/ T,X0,Y0,Z0,XI,YI,ZI,XJ,YJ,ZJ,NI,NJ,NM
C
      PARAMETER (SQRT3=1.73205080756888D+00)
      PARAMETER (SQRT5=2.23606797749979D+00)
      PARAMETER (SQRT7=2.64575131106459D+00)
      PARAMETER (ZERO=0.0D+00)
      PARAMETER (ONE=1.0D+00)
      PARAMETER (RLN10=2.30258D+00)
C
      DATA JX / 0, 1, 0, 0, 2, 0, 0, 1, 1, 0,
     *          3, 0, 0, 2, 2, 1, 0, 1, 0, 1,
     *          4, 0, 0, 3, 3, 1, 0, 1, 0, 2,
     *          2, 0, 2, 1, 1/
      DATA IX / 1, 6, 1, 1,11, 1, 1, 6, 6, 1,
     *         16, 1, 1,11,11, 6, 1, 6, 1, 6,
     *         21, 1, 1,16,16, 6, 1, 6, 1,11,
     *         11, 1,11, 6, 6/
      DATA JY / 0, 0, 1, 0, 0, 2, 0, 1, 0, 1,
     *          0, 3, 0, 1, 0, 2, 2, 0, 1, 1,
     *          0, 4, 0, 1, 0, 3, 3, 0, 1, 2,
     *          0, 2, 1, 2, 1/
      DATA IY / 1, 1, 6, 1, 1,11, 1, 6, 1, 6,
     *          1,16, 1, 6, 1,11,11, 1, 6, 6,
     *          1,21, 1, 6, 1,16,16, 1, 6,11,
     *          1,11, 6,11, 6/
      DATA JZ / 0, 0, 0, 1, 0, 0, 2, 0, 1, 1,
     *          0, 0, 3, 0, 1, 0, 1, 2, 2, 1,
     *          0, 0, 4, 0, 1, 0, 1, 3, 3, 0,
     *          2, 2, 1, 1, 2/
      DATA IZ / 1, 1, 1, 6, 1, 1,11, 1, 6, 6,
     *          1, 1,16, 1, 6, 1, 6,11,11, 6,
     *          1, 1,21, 1, 6, 1, 6,16,16, 1,
     *         11,11, 6, 6,11/
C
C       CALCULATE INTEGRALS NECESSARY FOR EVALUATION OF PROPERTIES.
C
      NIJ = 225*NVAL
      TOL = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C                         LOOP OVER SHELLS II
C
      DO 500  II=1,NSHELL+NADD
      I    = KATOM(II)
      XI   = C(1,I)
      YI   = C(2,I)
      ZI   = C(3,I)
      I1   = KSTART(II)
      I2   = I1 + KNG(II) - 1
      LIT  = KTYPE(II)
      MINI = KMIN(II)
      MAXI = KMAX(II)
      LOCI = KLOC(II) - MINI
C
C                         LOOP OVER SHELLS JJ
C
      DO 500  JJ=1,II
C
      J    = KATOM(JJ)
      XJ   = C(1,J)
      YJ   = C(2,J)
      ZJ   = C(3,J)
      J1   = KSTART(JJ)
      J2   = J1 + KNG(JJ) - 1
      LJT  = KTYPE(JJ)
      MINJ = KMIN(JJ)
      MAXJ = KMAX(JJ)
      LOCJ = KLOC(JJ) - MINJ
C
      RR     = (XI-XJ)**2 + (YI-YJ)**2 + (ZI-ZJ)**2
      IANDJ  = II.EQ.JJ
C
C             PREPARE INDICES FOR PAIRS OF (I,J) ORBITALS
C
      IJ = 0
      MAX = MAXJ
      DO 100  I=MINI,MAXI
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
C
      IF (IANDJ) MAX = I
      DO 100  J=MINJ,MAX
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
  100 CONTINUE
C
      DO 120  I=1,NIJ
  120 WINT(I) = ZERO
C
C                     LOOP OVER PRIMITIVES IG
C
      JGMAX = J2
      DO 400  IG=I1,I2
      AI  = EX(IG)
      CSI = CS(IG)
      CPI = CP(IG)
      CDI = CD(IG)
      CFI = CF(IG)
      CGI = CG(IG)
C
C                     LOOP OVER PRIMITIVES JG
C
      IF (IANDJ) JGMAX = IG
      DO 400 JG=J1,JGMAX
      AJ  = EX(JG)
      CSJ = CS(JG)
      CPJ = CP(JG)
      CDJ = CD(JG)
      CFJ = CF(JG)
      CGJ = CG(JG)
C
      AA  = AI + AJ
      AA1 = ONE/AA
      AX  = (AI*XI + AJ*XJ)*AA1
      AY  = (AI*YI + AJ*YJ)*AA1
      AZ  = (AI*ZI + AJ*ZJ)*AA1
C
      DUM = AI*AJ*RR*AA1
C
      IF(DUM .GT. TOL) GO TO 400
C
      FAC = EXP(-DUM)
C
C                   CALCULATE DENSITY FACTORS
C
      DOUBLE = IANDJ.AND.IG.NE.JG
      MAX = MAXJ
      NN  = 0
C
      DUM1 = ZERO
      DUM2 = ZERO
      DO 380 I = MINI,MAXI
         IF (I.EQ.1) DUM1=CSI*FAC
         IF (I.EQ.2) DUM1=CPI*FAC
         IF (I.EQ.5) DUM1=CDI*FAC
         IF ((I.EQ.8).AND.NORM) DUM1=DUM1*SQRT3
         IF (I.EQ.11) DUM1 = CFI*FAC
         IF ((I.EQ.14).AND.NORM) DUM1 = DUM1*SQRT5
         IF ((I.EQ.20).AND.NORM) DUM1 = DUM1*SQRT3
         IF (I.EQ.21) DUM1 = CGI*FAC
         IF ((I.EQ.24).AND.NORM) DUM1 = DUM1*SQRT7
         IF ((I.EQ.30).AND.NORM) DUM1 = DUM1*SQRT5/SQRT3
         IF ((I.EQ.33).AND.NORM) DUM1 = DUM1*SQRT3
         IF(IANDJ) MAX = I
         DO 380 J = MINJ,MAX
            NN = NN+1
            IF(J.EQ.1) THEN
              DUM2 = DUM1*CSJ
              IF(DOUBLE .AND. I.EQ.1) DUM2 = DUM2 + DUM2
              IF(DOUBLE .AND. I.GT.1) DUM2 = DUM2 + CSI*CPJ*FAC
C
            ELSE IF(J.EQ.2) THEN
              DUM2 = DUM1*CPJ
              IF(DOUBLE) DUM2 = DUM2 + DUM2
C
            ELSE IF(J.EQ.5) THEN
              DUM2 = DUM1*CDJ
              IF(DOUBLE) DUM2 = DUM2 + DUM2
C
            ELSE IF((J.EQ.8).AND.NORM) THEN
              DUM2 = DUM2*SQRT3
C
            ELSE IF (J.EQ.11) THEN
              DUM2 = DUM1*CFJ
              IF (DOUBLE) DUM2 = DUM2+DUM2
C
            ELSE IF ((J.EQ.14).AND.NORM) THEN
              DUM2 = DUM2*SQRT5
C
            ELSE IF ((J.EQ.20).AND.NORM) THEN
              DUM2 = DUM2*SQRT3
C
            ELSE IF (J.EQ.21) THEN
              DUM2 = DUM1*CGJ
              IF (DOUBLE) DUM2 = DUM2+DUM2
C
            ELSE IF ((J.EQ.24).AND.NORM) THEN
              DUM2 = DUM2*SQRT7
C
            ELSE IF ((J.EQ.30).AND.NORM) THEN
              DUM2 = DUM2*SQRT5/SQRT3
C
            ELSE IF ((J.EQ.33).AND.NORM) THEN
              DUM2 = DUM2*SQRT3
C
            END IF
            DIJ(NN) = DUM2
  380 CONTINUE
C
C             CALCULATE INTEGRALS FOR SPECIFIC PROPERTIES
C
      IF(JJ.LE.NSHELL)
     *   CALL INTEFL(LIT,LJT,IJ,IJX,IJY,IJZ,DIJ,WINT,AA,AX,AY,AZ)
C
  400 CONTINUE
C
C        END OF LOOPS OVER PRIMITIVES
C
C                  SET UP EXPECTATION VALUE MATRICES
C
      MAX = MAXJ
      DO 460  K=1,NVAL
         NL2 = (K-1)*L2
         NN  = (K-1)*IJ
         DO 450  I=MINI,MAXI
            LI = LOCI + I
            IN = LI*(LI-1)/2 + NL2
            IF (IANDJ) MAX = I
            DO 440  J=MINJ,MAX
               LJ = LOCJ + J
               JN = LJ + IN
               NN = NN+1
               XVAL(JN) = WINT(NN)
               IF(JJ.GT.NSHELL)XVAL(JN)=ZERO
  440       CONTINUE
  450    CONTINUE
  460 CONTINUE
C
  500 CONTINUE
      RETURN
      END
C*MODULE PCMDIS  *DECK INTMEP2
      SUBROUTINE INTMEP2(VALUE,XPP,YPP,ZPP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL IANDJ,NORM,DOUBLE
C
      DIMENSION VALUE(*)
      DIMENSION IX(35),IY(35),IZ(35),JX(35),JY(35),JZ(35),
     *          XIN(125),YIN(125),ZIN(125),WINT(225),WORK(225),
     *          DIJ(225),IJX(225),IJY(225),IJZ(225),
     *          HP(28),WP(28),MINP(7),MAXP(7)
C
      PARAMETER (MXSH=1000, MXGTOT=5000, MXATM=500)
C
      COMMON /HERMIT/ H1,H2(2),H3(3),H4(4),H5(5),H6(6),H7(7)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /NSHEL / EX(MXGTOT),CS(MXGTOT),CP(MXGTOT),CD(MXGTOT),
     *                CF(MXGTOT),CG(MXGTOT),
     *                KSTART(MXSH),KATOM(MXSH),KTYPE(MXSH),KNG(MXSH),
     *                KLOC(MXSH),KMIN(MXSH),KMAX(MXSH),NSHELL
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PCMDBS/ NADD,NUMB
      COMMON /ROOT  / XX,U(9),W(9),NROOTS
      COMMON /WERMIT/ W1,W2(2),W3(3),W4(4),W5(5),W6(6),W7(7)
C
      EQUIVALENCE (HP(1),H1),(WP(1),W1)
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
      PARAMETER (PI212=1.1283791670955D+00)
      PARAMETER (SQRT3=1.73205080756888D+00)
      PARAMETER (SQRT5=2.23606797749979D+00)
      PARAMETER (SQRT7=2.64575131106459D+00)
      PARAMETER (RLN10=2.30258D+00)
      DATA MINP /1,2,4,7,11,16,22/
      DATA MAXP /1,3,6,10,15,21,28/
      DATA JX / 0, 1, 0, 0, 2, 0, 0, 1, 1, 0,
     *          3, 0, 0, 2, 2, 1, 0, 1, 0, 1,
     *          4, 0, 0, 3, 3, 1, 0, 1, 0, 2,
     *          2, 0, 2, 1, 1/
      DATA IX / 1, 6, 1, 1,11, 1, 1, 6, 6, 1,
     *         16, 1, 1,11,11, 6, 1, 6, 1, 6,
     *         21, 1, 1,16,16, 6, 1, 6, 1,11,
     *         11, 1,11, 6, 6/
      DATA JY / 0, 0, 1, 0, 0, 2, 0, 1, 0, 1,
     *          0, 3, 0, 1, 0, 2, 2, 0, 1, 1,
     *          0, 4, 0, 1, 0, 3, 3, 0, 1, 2,
     *          0, 2, 1, 2, 1/
      DATA IY / 1, 1, 6, 1, 1,11, 1, 6, 1, 6,
     *          1,16, 1, 6, 1,11,11, 1, 6, 6,
     *          1,21, 1, 6, 1,16,16, 1, 6,11,
     *          1,11, 6,11, 6/
      DATA JZ / 0, 0, 0, 1, 0, 0, 2, 0, 1, 1,
     *          0, 0, 3, 0, 1, 0, 1, 2, 2, 1,
     *          0, 0, 4, 0, 1, 0, 1, 3, 3, 0,
     *          2, 2, 1, 1, 2/
      DATA IZ / 1, 1, 1, 6, 1, 1,11, 1, 6, 6,
     *          1, 1,16, 1, 6, 1, 6,11,11, 6,
     *          1, 1,21, 1, 6, 1, 6,16,16, 1,
     *         11,11, 6, 6,11/
C
C       EVALUATE THE -MEP- VALUE FOR ALL POINTS GIVEN IN -XYZ-
C
      TOL  = RLN10*ITOL
      NORM = NORMF .NE. 1 .OR. NORMP .NE. 1
C
C                    LOOP OVER SHELLS II
C
      DO 510 II=1,NSHELL+NADD
      I    = KATOM(II)
      XI   = C(1,I)
      YI   = C(2,I)
      ZI   = C(3,I)
      I1   = KSTART(II)
      I2   = I1 + KNG(II) - 1
      LIT  = KTYPE(II)
      MINI = KMIN(II)
      MAXI = KMAX(II)
      LOCI = KLOC(II) - MINI
C
C                    LOOP OVER SHELLS JJ
C
      DO 500  JJ=1,II
C
      J    = KATOM(JJ)
      XJ   = C(1,J)
      YJ   = C(2,J)
      ZJ   = C(3,J)
      J1   = KSTART(JJ)
      J2   = J1 + KNG(JJ) - 1
      LJT  = KTYPE(JJ)
      MINJ = KMIN(JJ)
      MAXJ = KMAX(JJ)
      LOCJ = KLOC(JJ) - MINJ
C
      RR     = (XI-XJ)**2 + (YI-YJ)**2 + (ZI-ZJ)**2
      NROOTS = (LIT + LJT - 2)/2 + 1
      IANDJ  = II.EQ.JJ
C
C             PREPARE INDICES FOR PAIRS OF (I,J) ORBITALS
C
      IJ = 0
      MAX = MAXJ
      DO 100  I=MINI,MAXI
      NX = IX(I)
      NY = IY(I)
      NZ = IZ(I)
C
      IF (IANDJ) MAX = I
      DO 100  J=MINJ,MAX
      IJ = IJ+1
      IJX(IJ) = NX+JX(J)
      IJY(IJ) = NY+JY(J)
      IJZ(IJ) = NZ+JZ(J)
  100 CONTINUE
C
      CALL VCLR(WORK,1,225)
      IF(JJ.GT.NSHELL) GO TO 411
C
C                     LOOP OVER PRIMITIVES IG
C
      JGMAX = J2
      DO 410  IG=I1,I2
      AI  = EX(IG)
      CSI = CS(IG)
      CPI = CP(IG)
      CDI = CD(IG)
      CFI = CF(IG)
      CGI = CG(IG)
C
C                     LOOP OVER PRIMITIVES JG
C
      IF (IANDJ) JGMAX = IG
      DO 400 JG=J1,JGMAX
      AJ  = EX(JG)
      CSJ = CS(JG)
      CPJ = CP(JG)
      CDJ = CD(JG)
      CFJ = CF(JG)
      CGJ = CG(JG)
C
      AA  = AI + AJ
      AA1 = ONE/AA
      FI  = PI212*AA1
C
      AAX = (AI*XI + AJ*XJ)
      AAY = (AI*YI + AJ*YJ)
      AAZ = (AI*ZI + AJ*ZJ)
C
      AX  = AAX*AA1
      AY  = AAY*AA1
      AZ  = AAZ*AA1
C
      DUM = AI*AJ*RR*AA1
      IF(DUM .GT. TOL) GO TO 400
      FAC = FI*EXP(-DUM)
C
C                       CALCULATE DENSITY FACTORS
C
      DOUBLE = IANDJ.AND.IG.NE.JG
      MAX = MAXJ
      NN  = 0
C
      DUM1 = ZERO
      DUM2 = ZERO
      DO 200 I = MINI,MAXI
         IF(I.EQ.1) DUM1=CSI*FAC
         IF(I.EQ.2) DUM1=CPI*FAC
         IF(I.EQ.5) DUM1=CDI*FAC
         IF(I.EQ.8.AND.NORM) DUM1=DUM1*SQRT3
         IF(I.EQ.11) DUM1 = CFI*FAC
         IF((I.EQ.14).AND.NORM) DUM1 = DUM1*SQRT5
         IF((I.EQ.20).AND.NORM) DUM1 = DUM1*SQRT3
         IF(I.EQ.21) DUM1 = CGI*FAC
         IF((I.EQ.24).AND.NORM) DUM1 = DUM1*SQRT7
         IF((I.EQ.30).AND.NORM) DUM1 = DUM1*SQRT5/SQRT3
         IF((I.EQ.33).AND.NORM) DUM1 = DUM1*SQRT3
         IF(IANDJ) MAX = I
         DO 180 J = MINJ,MAX
            NN = NN+1
            IF(J.EQ.1) THEN
              DUM2 = DUM1*CSJ
              IF(DOUBLE .AND. I.EQ.1) DUM2 = DUM2 + DUM2
              IF(DOUBLE .AND. I.GT.1) DUM2 = DUM2 + CSI*CPJ*FAC
C
            ELSE IF(J.EQ.2) THEN
              DUM2 = DUM1*CPJ
              IF(DOUBLE) DUM2 = DUM2 + DUM2
C
            ELSE IF(J.EQ.5) THEN
              DUM2 = DUM1*CDJ
              IF(DOUBLE) DUM2 = DUM2 + DUM2
C
            ELSE IF((J.EQ.8).AND.NORM) THEN
              DUM2 = DUM2*SQRT3
C
            ELSE IF (J.EQ.11) THEN
              DUM2 = DUM1*CFJ
              IF (DOUBLE) DUM2 = DUM2+DUM2
C
            ELSE IF ((J.EQ.14).AND.NORM) THEN
              DUM2 = DUM2*SQRT5
C
            ELSE IF ((J.EQ.20).AND.NORM) THEN
              DUM2 = DUM2*SQRT3
C
            ELSE IF (J.EQ.21) THEN
              DUM2 = DUM1*CGJ
              IF (DOUBLE) DUM2 = DUM2+DUM2
C
            ELSE IF ((J.EQ.24).AND.NORM) THEN
              DUM2 = DUM2*SQRT7
C
            ELSE IF ((J.EQ.30).AND.NORM) THEN
              DUM2 = DUM2*SQRT5/SQRT3
C
            ELSE IF ((J.EQ.33).AND.NORM) THEN
              DUM2 = DUM2*SQRT3
C
            END IF
C
            DIJ(NN) = DUM2
  180    CONTINUE
  200 CONTINUE
C
C       CALCULATE POINTS AND WEIGHTS FOR RYS POLYNOMIAL
C
      XX  = AA * ((AX-XPP)**2 + (AY-YPP)**2 + (AZ-ZPP)**2)
      IF (NROOTS.LE.3) CALL RT123
      IF (NROOTS.EQ.4) CALL ROOT4
      IF (NROOTS.EQ.5) CALL ROOT5
C
C       LOOP OVER ROOTS OF RYS POLYNOMIAL TO CALCULATE INTEGRALS
C
      MM = 0
      DO 340  K=1,NROOTS
C
      UU = AA*U(K)
      WW = W(K)
      TT = ONE/(AA+UU)
      T  = SQRT(TT)
C
      X0 = (AAX + UU*XPP)*TT
      Y0 = (AAY + UU*YPP)*TT
      Z0 = (AAZ + UU*ZPP)*TT
C
C      CALCULATE 1-DIMENSIONAL INTEGRALS OVER ALL ANGULAR MOMENTA
C
      IN = -5+MM
      DO 320  I=1,LIT
      IN = IN+5
      NI = I
C
      DO 320  J=1,LJT
      JN = IN+J
      NJ = J
C
C       EVALUATE MOMENT INTEGRALS USING GAUSS-HERMITE QUADRATURE:
C
      XINT0 = ZERO
      YINT0 = ZERO
      ZINT0 = ZERO
C
      NPTS = (NI + NJ - 2)/2 + 1
      IMIN = MINP(NPTS)
      IMAX = MAXP(NPTS)
C
      DO 310  IROOT=IMIN,IMAX
C
      DUM = WP(IROOT)
      PX = DUM
      PY = DUM
      PZ = DUM
C
      DUM = HP(IROOT)*T
      PTX = DUM + X0
      PTY = DUM + Y0
      PTZ = DUM + Z0
C
      AXI = PTX - XI
      AYI = PTY - YI
      AZI = PTZ - ZI
C
      BXI = PTX - XJ
      BYI = PTY - YJ
      BZI = PTZ - ZJ
C
      GO TO (250,240,230,220,210),NI
C
  210 PX = PX*AXI
      PY = PY*AYI
      PZ = PZ*AZI
C
  220 PX = PX*AXI
      PY = PY*AYI
      PZ = PZ*AZI
C
  230 PX = PX*AXI
      PY = PY*AYI
      PZ = PZ*AZI
C
  240 PX = PX*AXI
      PY = PY*AYI
      PZ = PZ*AZI
C
  250 CONTINUE
C
      GO TO (300,290,280,270,260),NJ
C
  260 PX = PX*BXI
      PY = PY*BYI
      PZ = PZ*BZI
C
  270 PX = PX*BXI
      PY = PY*BYI
      PZ = PZ*BZI
C
  280 PX = PX*BXI
      PY = PY*BYI
      PZ = PZ*BZI
C
  290 PX = PX*BXI
      PY = PY*BYI
      PZ = PZ*BZI
C
  300 CONTINUE
C
      XINT0 = XINT0 + PX
      YINT0 = YINT0 + PY
      ZINT0 = ZINT0 + PZ
C
  310 CONTINUE
C
      XIN(JN) = XINT0
      YIN(JN) = YINT0
      ZIN(JN) = ZINT0*WW
C
  320 CONTINUE
C
      MM = MM+25
  340 CONTINUE
C
C                      LOOP OVER ORBITAL PRODUCTS
C
      DO 360 I=1,IJ
         NX = IJX(I)
         NY = IJY(I)
         NZ = IJZ(I)
         SUM = ZERO
         MM = 0
         DO 350 K=1,NROOTS
            SUM = SUM + XIN(NX+MM)*YIN(NY+MM)*ZIN(NZ+MM)
            MM = MM+25
  350    CONTINUE
         WINT(I) = SUM*DIJ(I)
  360 CONTINUE
C
C              SET UP EXPECTATION VALUE MATRICES
C
      INDEX = 1
      MAX = MAXJ
      NN2 = 0
      DO 380 I=MINI,MAXI
         IF (IANDJ) MAX = I
         DO 370 J=MINJ,MAX
            INDEX = INDEX + 1
            NN2 = NN2 + 1
            WORK(INDEX) = WORK(INDEX) + WINT(NN2)
  370    CONTINUE
  380 CONTINUE
C
  400 CONTINUE
  410 CONTINUE
C
C        END OF LOOPS OVER PRIMITIVES
C
  411 CONTINUE
      INDEX = 1
      MAX = MAXJ
      DO 420  I=MINI,MAXI
         LI = LOCI + I
         IN = LI*(LI-1)/2
         IF (IANDJ) MAX = I
         DO 420  J=MINJ,MAX
            LJ = LOCJ + J
            JN = LJ + IN
            INDEX = INDEX + 1
            DW  = WORK(INDEX)
            IF(JJ.GT.NSHELL)DW=ZERO
            VALUE(JN) = DW
  420 CONTINUE
C
  500 CONTINUE
  510 CONTINUE
C
C        END OF LOOPS OVER SHELLS
C
      RETURN
      END
C
