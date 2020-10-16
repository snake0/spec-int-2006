C  6 SEP 01 - CHC - NEW MODULE FOR QFMM (QUANTUM FAST MULTIPOLE METHOD)
C
C     REFERENCES
C  1) STEINBORN, E. O.; RUEDENBERG, K. ADV. IN QUANTUM CHEM.
C     1973, V7, 1.
C  2) GREENGARD, L. THE RAPID EVALUATION OF POTENTIAL FIELDS
C     IN PARTICLE SYSTEMS (MIT, CAMBRIDGE, 1987)
C  3) CHOI, C. H.; IVANIC, J.; GORDON, M. S.; RUEDENBERG, K.
C     J. CHEM. PHYS. 1999, 111, 8825.
C  4) CHOI, C. H.; RUEDENBERG, K.; GORDON, M. S.
C     J. COMP. CHEM. 2001, 22, 1484.
C
C*MODULE QFMM    *DECK C2P
      SUBROUTINE C2P(IYZTBL,F,G,CLM,FLM,ZLL,NTMPL,YP,NTBOX,IDXBOX,
     *               NSBOX,MAXWS,IYZPNT,IC2P)
C
C     C2P : CHILDREN TO PARENTS Y2Y UPWARD TRANSLATION.
C
C     C. H. CHOI MAY 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MAXCOF=23821)
      PARAMETER (HALF=0.5D+00,MAXNP=50)
      LOGICAL QFMM,QOPS
      COMMON /QMFM  /SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *       ITERMS,QOPS,ISCUT
      DIMENSION IYZTBL(2**(NS+1)-1,2**(NS+1)-1,2**(NS+1)-1),
     *          F((NP*(NP+1)*(NP+2)*4/3)+(NP+1),8),CLM(-NP:NP),
     *          G((NP*(NP+1)*(NP+2)*4/3)+(NP+1),8),FLM(-NP:NP),
     *          YP((NP+1)*(NP+2)/2,NTMPL),ZLL(0:NP),
     *          CYP((MAXNP+1)*(MAXNP+2)/2),
     *          PYP((MAXNP+1)*(MAXNP+2)/2),COEFF(MAXCOF)
      DIMENSION IDXBOX(3,NTBOX),NSBOX(20),
     *          IYZPNT(NTBOX,MAXWS/2)
      INTEGER PARENT
      COMPLEX*16 CYP, PYP, YP
      PARENT(IPT)=INT((IPT+1)*HALF)
C
      TRNS=SQRT(3.0D+00)*SIZE*HALF**(NS+1)
      LNP=INT((NP+1)*(NP+2)*HALF)
      LFG=(NP*(NP+1)*(NP+2)*4/3)+(NP+1)
C
C     GET THE ROTATION MATRICES
C
      CALL GETD(NP,CLM,F,G)
C
      INS=1
      MINCBOX=1
      MAXCBOX=NSBOX(1)
      IMAXWS=MAXWS
      IC2P=0
      ICST=0
      IPST=0
C
      DO NSS=NS,1,-1
         NSS2=2**NSS
         IPST=IPST+NSS2
         CALL GETZLL(ZLL,TRNS,NP)
         CALL GETCOF2(NP,FLM,ZLL,COEFF)
C
         IMAXWS=(IMAXWS+1)/2
C
C        LOOP OVER NON-EMPTY BOXES OF CHILDREN
C
         DO NONC=MINCBOX,MAXCBOX
C
C           LOOP OVER BOXES WITH DIFFERENT IWS INDEX
C
            IC=IDXBOX(1,NONC)+ICST
            JC=IDXBOX(2,NONC)+ICST
            KC=IDXBOX(3,NONC)+ICST
C
            DO NWS=1,IMAXWS
               IF ( IYZPNT(NONC,NWS).NE.0 ) THEN
                  IPWS=(NWS+1)/2
                  IP=PARENT(IC-ICST)+IPST
                  JP=PARENT(JC-ICST)+IPST
                  KP=PARENT(KC-ICST)+IPST
                  NONP=IYZTBL(IP,JP,KP)
C
                  IYPC=IYZPNT(NONC,NWS)
                  IYPP=IYZPNT(NONP,IPWS)
               IF ( IYPP.NE.0 ) THEN
                  CALL WHCHBOX(IC,JC,KC,IBX)
C
                  IC2P=IC2P+1
                  CALL RY2YIR(LNP,YP(1,IYPC),PYP,
     *                 CYP,LFG,F(1,IBX),G(1,IBX),
     *                 NP,COEFF)
                  DO L=1,LNP
                     YP(L,IYPP)=YP(L,IYPP)+PYP(L)
                  ENDDO
               ENDIF
               ENDIF
            ENDDO
         ENDDO
         TRNS=TRNS*2
         ICST=IPST
C
         MINCBOX=MAXCBOX+1
         INS=INS+1
         MAXCBOX=MAXCBOX+NSBOX(INS)
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK TRANM
      SUBROUTINE TRANM(NCXYZ,IYP,INDX2,IDXIJK,IBS,CXYZ,F,G,FLM,CLM,ZLL,
     *                MAXNYP,TMPGPS,TMPGPL,NFTPL,NFTPLT)
C
C     THIS ROUTINE ONLY TRANSLATES YGP (MULTIPOLE MOMENTS OF PRODUCT
C     OF GAUSSIAN) TO THE CENTER OF ITS BOX.
C
C     SIZE : ONE SIDE LENGTH OF THE ORIGINAL BOX.
C     NCXYZ: DIMENSION OF IYP, INDX,CXYZ
C     IYP : ACTUAL POINTER TO YGP
C     INDX: REORDERED CXYZ POINTER
C     CXYZ: REORDERED POSITION OF EACH PRODUCT OF GAUSSIANS W.R.T.
C           BOX.
C     F,G : D=F+IG, WHERE D IS COMPLEX ROTATION MATRIX
C     FLM,CLM,ZLL: PRE-COMPUTED COEFFICIENTS
C
C     C. H. CHOI DEC 1999
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO=0.00D+00,MAXNP=50,
     *           MAXCOF=23821)
      LOGICAL QFMM,QOPS
      LOGICAL TMPDSK,GOPARR,DSKWRK,MASWRK
      COMPLEX*16 CYP, YTP,TMPGPS,TMPGPL
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /QMFM  /SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *       ITERMS,QOPS,ISCUT
      DIMENSION IYP(NCXYZ),CXYZ(NCXYZ,3),IDXIJK(NCXYZ,3),INDX2(NCXYZ),
     *          IBS(NCXYZ,4),F((NP*(NP+1)*(NP+2)*4/3)+(NP+1)),
     *          CLM(-NP:NP),G((NP*(NP+1)*(NP+2)*4/3)+(NP+1)),
     *          FLM(-NP:NP),COEFF(MAXCOF),ZLL(0:2*NP+1),
     *          TMPGPS((NPGP+1)*(NPGP+2)/2,MAXNYP),
     *          TMPGPL((NP+1)*(NP+2)/2,MAXNYP),
     *          CYP((MAXNP+1)*(MAXNP+2)/2),YTP((MAXNP+1)*(MAXNP+2)/2),
     *          CTR(4),PNT(4),EZ(3),RT(3,3)
C
      IF (GOPARR) THEN
         TMPDSK=DSKWRK
         DSKWRK=.TRUE.
      ENDIF
C
      CALL SEQREW(NFTPL)
      CALL SEQREW(NFTPLT)
C
      NS2=2**NS
      LNP=(NP+1)*(NP+2)/2
      LNPGP=(NPGP+1)*(NPGP+2)/2
      LFG=(NP*(NP+1)*(NP+2)*4/3)+(NP+1)
C
      DO I=1,MAXNYP
         DO J=1,LNP
            TMPGPL(J,I)=ZERO
         ENDDO
      ENDDO
C
      EZ(1)=0.0D+00
      EZ(2)=0.0D+00
      EZ(3)=1.0D+00
C
      IIP=0
      KP=0
      IF (MPMTHD.EQ.0) THEN
      DO N=1,NCXYZ,NUMRD
       NNEW=N+NUMRD-1
       IF (NNEW.GT.NCXYZ) NNEW=NCXYZ
       IIP=IYP(NNEW)-KP
       KP=KP+IIP
       JP=0
       LENIO = LNPGP*2
       DO II=1,IIP
          CALL SQREAD(NFTPL,TMPGPS(1,II),LENIO)
       ENDDO
C
       DO M=N,NNEW
         INEW=INDX2(M)
C        FIND OUT THE CENTER OF THE BOX TO WHICH THE POINT BELONGS.
         I=IDXIJK(INEW,1)
         J=IDXIJK(INEW,2)
         K=IDXIJK(INEW,3)
         CALL GETCTR(SIZE,NS2,I,J,K,CTR)
C        FIND OUT THE TRANSLATION VECTOR OF THE POINT
         DO II=1,3
            PNT(II)=CXYZ(INEW,II)-CTR(II)
         ENDDO
C        GET THE ROTATION MATRICES
         TY2Y=SQRT(PNT(1)*PNT(1)+PNT(2)*PNT(2)+
     *        PNT(3)*PNT(3))
         IF (NP.LT.NPGP) NP=NPGP
         CALL GETZLL(ZLL,TY2Y,2*NP+1)
         CALL GETCOF2(NP,FLM,ZLL,COEFF)
         CALL ROTR(PNT,EZ,RT)
         CALL GETROT(F,G,RT,NP,CLM)
         DO II=1,LNP
            CYP(II)=ZERO
         ENDDO
C
         DO IB=IBS(M,1),IBS(M,2)
            DO JB=IBS(M,3),IBS(M,4)
               IF (IB.GE.JB) THEN
                  JP=JP+1
                  IF (IB.EQ.JB) THEN
                     DO II=1,LNPGP
                        CYP(II)=TMPGPS(II,JP)
                     ENDDO
                     CALL RY2YIR(LNP,CYP,TMPGPL(1,JP),
     *                  YTP,LFG,F,G,
     *                  NP,COEFF)
                  ELSE
                     DO II=1,LNPGP
                        CYP(II)=TMPGPS(II,JP)
                     ENDDO
                     CALL RY2YIR(LNP,CYP,TMPGPL(1,JP),
     *                  YTP,LFG,F,G,
     *                  NP,COEFF)
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
       ENDDO
       CALL SQWRIT(NFTPLT,TMPGPL(1,1),LNP*2*JP)
C
      ENDDO
      ELSEIF (MPMTHD.EQ.1) THEN
      DO N=1,NCXYZ,NUMRD
       NNEW=N+NUMRD-1
       IF (NNEW.GT.NCXYZ) NNEW=NCXYZ
       IIP=IYP(NNEW)-KP
       KP=KP+IIP
       LENIO = LNPGP*2
       DO II=1,IIP
          CALL SQREAD(NFTPL,TMPGPS(1,II),LENIO)
       ENDDO
       CALL SQWRIT(NFTPLT,TMPGPS(1,1),LNPGP*2*IIP)
      ENDDO
      ENDIF
C
      IF (GOPARR) DSKWRK=TMPDSK
      RETURN
      END
C*MODULE QFMM    *DECK REWRT
      SUBROUTINE REWRT(NCXYZ,IYP,MAXNYP,TMPGPS,NFTPL,NFTPLT)
C
C     C. H. CHOI DEC 1999
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL QFMM,QOPS
      COMPLEX*16 TMPGPS
      LOGICAL TMPDSK,GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /QMFM  / SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *                ITERMS,QOPS,ISCUT
      DIMENSION IYP(NCXYZ),TMPGPS((NPGP+1)*(NPGP+2)/2,MAXNYP)
C
      IF (GOPARR) THEN
         TMPDSK=DSKWRK
         DSKWRK=.TRUE.
      ENDIF
      CALL SEQREW(NFTPL)
      CALL SEQREW(NFTPLT)
C
      LNPGP=(NPGP+1)*(NPGP+2)/2
C
      IIP=0
      KP=0
      DO N=1,NCXYZ,NUMRD
       NNEW=N+NUMRD-1
       IF (NNEW.GT.NCXYZ) NNEW=NCXYZ
       IIP=IYP(NNEW)-KP
       KP=KP+IIP
       LENIO = LNPGP*2
       DO II=1,IIP
          CALL SQREAD(NFTPL,TMPGPS(1,II),LENIO)
       ENDDO
       CALL SQWRIT(NFTPLT,TMPGPS(1,1),LNPGP*2*IIP)
      ENDDO
C
      IF (GOPARR) DSKWRK=TMPDSK
      RETURN
      END
C*MODULE QFMM    *DECK CMPTBL
      SUBROUTINE CMPTBL(ITBL,IYZTBL,NTBOX,IDXBOX,MBOX,NSBOX,
     *            MAXWS,NUMWS,IYZPNT,NTMPL)
C
C     THIS ROUTINE COMPLETES THE LOOK-UP TABLE.
C
C     C. H. CHOI APRIL 2000
C
C     ITBL : HAS THE NUMBER OF POINTS IN EACH BOX.
C     IDXBOX : INDEX OF NON-EMPTY BOX
C     MBOX  : THE NUMBER OF POINTS IN EACH BOX
C     NSBOX : THE NUMBER OF NON-EMPTY BOX OF EACH LEVEL
C     NUMWS : THE NUMBER OF POINTS IN EACH IWS INDEX IN A GIVEN BOX
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (HALF=0.5D+00)
      LOGICAL QFMM,QOPS
      COMMON /QMFM  /SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *       ITERMS,QOPS,ISCUT
      DIMENSION ITBL(2**(NS+1)-1,2**(NS+1)-1,2**(NS+1)-1),
     *          IYZTBL(2**(NS+1)-1,2**(NS+1)-1,2**(NS+1)-1)
      DIMENSION IDXBOX(3,NTBOX),MBOX(NTBOX),NSBOX(20),
     *          IYZPNT(NTBOX,MAXWS/2),NUMWS(NTBOX,MAXWS/2)
      INTEGER PARENT
      PARENT(IPT)=INT((IPT+1)*HALF)
C
C     THE FOLLOWING CONDITION MUST BE SATISFIED TO CONTINUE THE
C     CALCULATIONS.
C
C     NS >= LOG(MAXWS)/LOG(2)
C
      NS2=2**NS
      IEND=2*(NS2-1)+1
      DO I=NS2+1,IEND
         DO J=NS2+1,IEND
            DO K=NS2+1,IEND
               ITBL(I,J,K)=0
            ENDDO
         ENDDO
      ENDDO
      DO I=1,IEND
         DO J=1,IEND
            DO K=1,IEND
               IYZTBL(I,J,K)=0
            ENDDO
         ENDDO
      ENDDO
C
C
      DO I=NSBOX(1)+1,NTBOX
         DO J=1,(MAXWS+1)/2
            NUMWS(I,J)=0
         ENDDO
      ENDDO
C
      IPST=0
      ICST=0
      IPOINTER=0
C
C     FIRST, COMPLETE THE ITBL TABLE
C
      DO NSS=NS,1,-1
         NSS2=2**NSS
         IPST=IPST+NSS2
         DO I=1,NSS2
            DO J=1,NSS2
               DO K=1,NSS2
                  IP=PARENT(I)+IPST
                  JP=PARENT(J)+IPST
                  KP=PARENT(K)+IPST
                  IC=I+ICST
                  JC=J+ICST
                  KC=K+ICST
                  IF ( ITBL(IC,JC,KC).NE.0 ) THEN
                     IPOINTER=IPOINTER+1
                     IYZTBL(IC,JC,KC)=IPOINTER
                     ITBL(IP,JP,KP)=ITBL(IP,JP,KP)+ITBL(IC,
     *                           JC,KC)
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
         ICST=IPST
      ENDDO
      IYZTBL(IC+1,JC+1,KC+1)=IPOINTER+1
C
C     SECOND, COMPLETES IDXBOX,MBOX, NSBOX
C
      IST=NS2
      INSBOX=1
      IBOX=NSBOX(1)
      DO NSS=NS-1,0,-1
         NSS2=2**NSS
         DO I=1,NSS2
            DO J=1,NSS2
               DO K=1,NSS2
                  II=I+IST
                  JJ=J+IST
                  KK=K+IST
                  IF ( ITBL(II,JJ,KK).NE.0 ) THEN
                     IBOX=IBOX+1
                     IDXBOX(1,IBOX)=I
                     IDXBOX(2,IBOX)=J
                     IDXBOX(3,IBOX)=K
                     MBOX(IBOX)=ITBL(II,JJ,KK)
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
         IST=IST+NSS2
         INSBOX=INSBOX+1
         NSBOX(INSBOX)=IBOX
      ENDDO
      DO I=NS,1,-1
         NSBOX(I+1)=NSBOX(I+1)-NSBOX(I)
      ENDDO
C
C     THIRD, COMPLETES NUMWS TABLE
C
      IPST=0
      INST=1
      INEND=NSBOX(1)
      IMAXWS=MAXWS
      DO NSS=NS,1,-1
         NSS2=2**NSS
         IPST=IPST+NSS2
         IMAXWS=(IMAXWS+1)/2
         DO NON=INST,INEND
            IC=IDXBOX(1,NON)
            JC=IDXBOX(2,NON)
            KC=IDXBOX(3,NON)
            IP=PARENT(IC)+IPST
            JP=PARENT(JC)+IPST
            KP=PARENT(KC)+IPST
            DO NIWS=1,IMAXWS
               IF ( NUMWS(NON,NIWS).NE.0 ) THEN
                  NPIWS=(NIWS+1)/2
                  NONP=IYZTBL(IP,JP,KP)
                  NUMWS(NONP,NPIWS)=NUMWS(NONP,NPIWS)
     *                             +NUMWS(NON,NIWS)
               ENDIF
C
            ENDDO
         ENDDO
         INST=INEND+1
         INEND=INEND+NSBOX(NS-NSS+2)
      ENDDO
C
C     FOURTH, COMPLETES IYZPNT AND IYZTBL
C
      DO I=1,NTBOX
         DO J=1,(MAXWS+1)/2
            IYZPNT(I,J)=0
         ENDDO
      ENDDO
C
      IST=0
      INST=1
      INEND=NSBOX(1)
      IMAXWS=MAXWS
      ICNTR=0
      DO NSS=NS,0,-1
         NSS2=2**NSS
         IMAXWS=(IMAXWS+1)/2
         DO NON=INST,INEND
            DO NIWS=1,IMAXWS
               IF ( NUMWS(NON,NIWS).NE.0 ) THEN
                  ICNTR=ICNTR+1
                  IYZPNT(NON,NIWS)=ICNTR
               ENDIF
            ENDDO
         ENDDO
         IST=IST+NSS2
         INST=INEND+1
         INEND=INEND+NSBOX(NS-NSS+2)
      ENDDO
C
      NTMPL=ICNTR
C
      RETURN
      END
C*MODULE QFMM    *DECK DIAMTR
      SUBROUTINE QDIAMTR(DIAMTR,M,A,VMAX)
C
C     THIS ROUTINE RETURNS THE SMALLEST DIAMETER OF THE SPHERE
C     THAT ENCLOSES THE POINTS OF A.
C
C     C. H. CHOI MAY 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(ZERO=0.0D+00,ONE=1.0D+00,CUT=5.0D+00)
      DIMENSION VMAX(3),A(3,M)
C
      DIAMTR=ABS(A(1,1))
      VMAX(1)=ZERO
      VMAX(2)=ZERO
      VMAX(3)=ZERO
      DO J=1,3
         TEMP=ZERO
         DO I=1,M
            IF (ABS(A(J,I)).GT.DIAMTR) DIAMTR=ABS(A(J,I))
            IF (ABS(A(J,I)).GT.TEMP) TEMP=ABS(A(J,I))
         ENDDO
         VMAX(J)=TEMP
      ENDDO
C
      DO I=1,3
         IF (VMAX(I).GT.CUT) THEN
            VMAX(I)=ONE
         ELSE
            VMAX(I)=ZERO
         ENDIF
      ENDDO
C
      DIAMTR=DIAMTR*2.05D+00
C
      RETURN
      END
C*MODULE QFMM    *DECK DIVIDE
      SUBROUTINE DIVIDE(M,A,INDX,IPNTR,ITBL,NBOX,IDXBOX,MBOX,
     *                  LEBOX,NSBOX,IDXWS,MAXNB,IDXIJK)
C
C     THIS ROUTINE RETURNS REORDERED A AND ITS CORRESPODING
C     ITBL WHICH CONTAINS THE NUMBER OF POINTS IN EACH BOXES
C     INDEXED WITH (I,J,K).
C     I,J,K = {1,2,...2**NS}
C     THIS ROUTINE UTILIZES HEAP-SORT ALGOTITHM WHICH SCALES
C     M LOG2 M.
C
C     C. H. CHOI MAY 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL QFMM,QOPS,FLAG
      COMMON /QMFM  /SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *       ITERMS,QOPS,ISCUT
      DIMENSION A(M,3),INDX(M),IDXWS(M),IDXIJK(M,3),IPNTR(2**NS,3),
     *          ITBL(2**(NS+1)-1,2**(NS+1)-1,2**(NS+1)-1),
     *          IDXBOX(3,NBOX),MBOX(NBOX),LEBOX(0:NBOX),NSBOX(20)
C
      NS2=2**NS
      DO I=1,2**NS
         IPNTR(I,1)=0
      ENDDO
C
      IBOX=0
      LEBOX(0)=0
      MAXNB=1
C
      CALL DSORT5(M,A(1,1),A(1,2),A(1,3),INDX,IDXWS)
C
C     SORT W.R.T. X
C
      UNIT=SIZE/NS2
      BASE=UNIT-SIZE/2
C
      IX=1
      NX=1
      FLAG=.TRUE.
      DO WHILE (FLAG)
         IF (A(IX,1).LE.BASE) THEN
            IPNTR(NX,1)=IPNTR(NX,1)+1
            IDXIJK(IX,1)=NX
         ELSE
            BASE=BASE+UNIT
            NX=NX+1
            IX=IX-1
            IF (NX.GT.NS2) FLAG=.FALSE.
         ENDIF
         IX=IX+1
         IF (IX.GT.M) FLAG=.FALSE.
      ENDDO
C
C     SORT W.R.T. Y
C
      ISTY=1
      DO NX=1,NS2
         DO I=1,2**NS
            IPNTR(I,2)=0
         ENDDO
         IF (IPNTR(NX,1).GT.0) THEN
            CALL DSORT5(IPNTR(NX,1),A(ISTY,2),A(ISTY,1),A(ISTY,3),
     *                 INDX(ISTY),IDXWS(ISTY))
            BASE=UNIT-SIZE/2
            IY=ISTY
            NY=1
            FLAG=.TRUE.
C
            DO WHILE (FLAG)
               IF (A(IY,2).LE.BASE) THEN
                  IPNTR(NY,2)=IPNTR(NY,2)+1
                  IDXIJK(IY,2)=NY
               ELSE
                  BASE=BASE+UNIT
                  NY=NY+1
                  IY=IY-1
                  IF (NY.GT.NS2) FLAG=.FALSE.
               ENDIF
               IY=IY+1
               IF (IY-ISTY+1.GT.IPNTR(NX,1)) FLAG=.FALSE.
            ENDDO
C
C            SORT W.R.T. Z
C
            ISTZ=ISTY
            DO NY=1,NS2
               DO I=1,2**NS
                  IPNTR(I,3)=0
               ENDDO
               IF (IPNTR(NY,2).GT.0) THEN
                  CALL DSORT5(IPNTR(NY,2),A(ISTZ,3),A(ISTZ,2),A(ISTZ,1),
     *                 INDX(ISTZ),IDXWS(ISTZ))
                  BASE=UNIT-SIZE/2
                  IZ=ISTZ
                  NZ=1
                  FLAG=.TRUE.
C
                  DO WHILE (FLAG)
                     IF (A(IZ,3).LE.BASE) THEN
                        IPNTR(NZ,3)=IPNTR(NZ,3)+1
                        IDXIJK(IZ,3)=NZ
                     ELSE
                        BASE=BASE+UNIT
                        NZ=NZ+1
                        IZ=IZ-1
                        IF (NZ.GT.NS2) FLAG=.FALSE.
                     ENDIF
                     IZ=IZ+1
                     IF (IZ-ISTZ+1.GT.IPNTR(NY,2)) FLAG=.FALSE.
                  ENDDO
                  DO IDXZ=1,NS2
                     ITBL(NX,NY,IDXZ)=IPNTR(IDXZ,3)
                     IF ( ITBL(NX,NY,IDXZ).GT.MAXNB ) THEN
                        MAXNB=ITBL(NX,NY,IDXZ)
                     ENDIF
                     IF (IPNTR(IDXZ,3).GT.0) THEN
                        IBOX=IBOX+1
                        IDXBOX(1,IBOX)=NX
                        IDXBOX(2,IBOX)=NY
                        IDXBOX(3,IBOX)=IDXZ
                        MBOX(IBOX)=IPNTR(IDXZ,3)
                        LEBOX(IBOX)=LEBOX(IBOX-1)+MBOX(IBOX)
                     ENDIF
                  ENDDO
               ELSE
                  DO IDXZ=1,NS2
                     ITBL(NX,NY,IDXZ)=0
                  ENDDO
               ENDIF
               ISTZ=ISTZ+IPNTR(NY,2)
            ENDDO
C
         ELSE
            DO IDXY=1,NS2
               DO IDXZ=1,NS2
                  ITBL(NX,IDXY,IDXZ)=0
               ENDDO
            ENDDO
C        ENDIF OF (IPNTR(NX,1).GT.0)
         ENDIF
         ISTY=ISTY+IPNTR(NX,1)
C     ENDDO OF NX=1,NS2
      ENDDO
      NSBOX(1)=IBOX
      MAXNB=MAXNB*8**(NS-2)
C
      RETURN
      END
C*MODULE QFMM    *DECK IDIVIDE
      SUBROUTINE IDIVIDE(NTXYZ,M,ITSP,ITPP,IPNTR,ITBL,IXTBL)
C
C     THIS ROUTINE UTILIZES HEAP-SORT ALGOTITHM WHICH SCALES
C     M LOG2 M.
C
C     C. H. CHOI DEC 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION ITSP(NTXYZ,2),ITPP(NTXYZ,2),IPNTR(NTXYZ,2),ITBL(NTXYZ)
C
      CALL ISORT4(NTXYZ,M,ITSP(1,1),ITSP(1,2),ITPP(1,1),ITPP(1,2))
C
C     SORT W.R.T. I
C
      DO I=1,M
         IPNTR(I,1)=0
      ENDDO
      NX=1
      IREF=ITSP(1,1)
C
      DO I=1,M
         IF (ITSP(I,1).EQ.IREF) THEN
            IPNTR(NX,1)=IPNTR(NX,1)+1
         ELSE
            IREF=ITSP(I,1)
            NX=NX+1
            IPNTR(NX,1)=1
         ENDIF
      ENDDO
C
C     SORT W.R.T. J
C
      ISTY=1
      IXTBL=0
      DO INX=1,NX
         DO I=1,M
            IPNTR(I,2)=0
         ENDDO
C
         CALL ISORT4(NTXYZ,IPNTR(INX,1),ITSP(ISTY,2),ITSP(ISTY,1),
     *               ITPP(ISTY,1),ITPP(ISTY,2))
         NY=1
         IREF=ITSP(ISTY,2)
C
         DO JNY=1,IPNTR(INX,1)
            IY=ISTY+JNY-1
            IF (ITSP(IY,2).EQ.IREF) THEN
               IPNTR(NY,2)=IPNTR(NY,2)+1
            ELSE
               IREF=ITSP(IY,2)
               NY=NY+1
               IPNTR(NY,2)=1
            ENDIF
         ENDDO
C
         ISTY=ISTY+IPNTR(INX,1)
         DO I=1,NY
            IXTBL=IXTBL+1
            ITBL(IXTBL)=IPNTR(I,2)
         ENDDO
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK IDIVIDEML
      SUBROUTINE IDIVIDEML(NSHL2,NTXYZ,M,ITSP,IPNTR,IXTBL)
C
C     THIS ROUTINE UTILIZES HEAP-SORT ALGOTITHM WHICH SCALES
C     M LOG2 M.
C
C     C. H. CHOI DEC 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION ITSP(2*NSHL2,2),IPNTR(M,2)
C
      CALL ISORT2(NTXYZ,ITSP(1,1),ITSP(1,2))
C
C     SORT W.R.T. I
C
      DO I=1,M
         IPNTR(I,1)=0
      ENDDO
      NX=1
      IREF=ITSP(1,1)
C
      DO I=1,NTXYZ
         IF (ITSP(I,1).EQ.IREF) THEN
            IPNTR(NX,1)=IPNTR(NX,1)+1
         ELSE
            IREF=ITSP(I,1)
            NX=NX+1
            IPNTR(NX,1)=1
         ENDIF
      ENDDO
C
C
C     SORT W.R.T. J
C
      ISTY=1
      IXTBL=0
      DO INX=1,NX
         DO I=1,M
            IPNTR(I,2)=0
         ENDDO
C
         CALL ISORT2(IPNTR(INX,1),ITSP(ISTY,2),
     *         ITSP(ISTY,1))
         NY=1
         IREF=ITSP(ISTY,2)
C
         DO JNY=1,IPNTR(INX,1)
            IY=ISTY+JNY-1
            IF (ITSP(IY,2).EQ.IREF) THEN
               IPNTR(NY,2)=IPNTR(NY,2)+1
            ELSE
               IREF=ITSP(IY,2)
               NY=NY+1
               IPNTR(NY,2)=1
            ENDIF
         ENDDO
C
         ISTY=ISTY+IPNTR(INX,1)
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK FACT
      DOUBLE PRECISION FUNCTION FACT(N)
C
C     THIS ROUTINE RETURNS FACTORIALS OF N.
C
C     C. H. CHOI MAY 1999
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      FACT=1.0D+00
      IF (N.LE.0) THEN
        FACT=1.0D+00
      ELSE
        DO I=1,N
           FACT=FACT*I
        ENDDO
      ENDIF
C
      RETURN
      END
C*MODULE QFMM    *DECK FORMFJ_SEMI
      SUBROUTINE FORMFJ_SEMI(NCXYZ,IYP,INDX2,IDXIJK,CXYZ,IBS,LFA,FA,
     *                 NZ,IYZTBL,ZP,NTBOX,MAXWS,IYZPNT,IDXWS,F,G,
     *                 ZLL,CLM,FLM,MAXNYP,TMPGPL,NFTPLT)
C
C     THIS ROUTINE FORMS FAR FIELD J MATRIX (COULOMB POTENTIAL).
C     FA : ALPHA FOCK MATRIX.
C
C     C. H. CHOI SEP 2000
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO=0.00D+00,MAXNP=50)
      PARAMETER (TWO=2.0D+00,MAXCOF=23821)
      COMPLEX*16 ZP, TMPGPL, PYP,CYP, YTP
      LOGICAL QOPS,TFLAG,GOPARR,DSKWRK,MASWRK,QFMM
      COMMON /QMFM  /SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *          ITERMS,QOPS,ISCUT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DIMENSION IYZTBL(2**(NS+1)-1,2**(NS+1)-1,2**(NS+1)-1)
      DIMENSION IYZPNT(NTBOX,MAXWS/2),IDXWS(NCXYZ)
      DIMENSION IYP(NCXYZ),CXYZ(NCXYZ,3),IBS(NCXYZ,4),
     *          TMPGPL((NPGP+1)*(NPGP+2)/2,MAXNYP),
     *          IDXIJK(NCXYZ,3),INDX2(NCXYZ),
     *          F((NP*(NP+1)*(NP+2)*4/3)+(NP+1)),CLM(-NP:NP),
     *          G((NP*(NP+1)*(NP+2)*4/3)+(NP+1)),FLM(-NP:NP),
     *          ZLL(0:2*NP+1),
     *          ZP((NP+1)*(NP+2)/2,NZ),CYP((MAXNP+1)*(MAXNP+2)/2),
     *          PYP((MAXNP+1)*(MAXNP+2)/2),YTP((MAXNP+1)*(MAXNP+2)/2),
     *          RT(3,3),PNT(4),CTR(4),EZ(3),
     *          FA(LFA),COEFF(MAXCOF)
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
      CALL SEQREW(NFTPLT)
C
      NS2=2**NS
      LNP=(NP+1)*(NP+2)/2
      LFG=(NP*(NP+1)*(NP+2)*4/3)+(NP+1)
      LNPGP=(NPGP+1)*(NPGP+2)/2
C
      EZ(1)=0.0D+00
      EZ(2)=0.0D+00
      EZ(3)=1.0D+00
C
      IIP=0
      KP=0
      DO N=1,NCXYZ,NUMRD
       NNEW=N+NUMRD-1
       IF (NNEW.GT.NCXYZ) NNEW=NCXYZ
       IIP=IYP(NNEW)-KP
       KP=KP+IIP
       JP=0
C
       IF (GOPARR) THEN
          MINE=MINE+1
          IF (MINE.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
          IF (NEXT.NE.MINE) THEN
             CALL SEQADV(NFTPLT)
             GOTO 100
          ENDIF
       ENDIF
       LENIO = LNPGP*2*IIP
       CALL SQREAD(NFTPLT,TMPGPL(1,1),LENIO)
       DO M=N,NNEW
         INEW=INDX2(M)
C        FIND OUT THE CENTER OF THE BOX TO WHICH THE POINT BELONGS.
         I=IDXIJK(INEW,1)
         J=IDXIJK(INEW,2)
         K=IDXIJK(INEW,3)
         CALL GETCTR(SIZE,NS2,I,J,K,CTR)
C        FIND OUT THE TRANSLATION VECTOR OF THE POINT
         DO II=1,3
            PNT(II)=CXYZ(INEW,II)-CTR(II)
         ENDDO
C        FIND OUT THE WS INDEX
         NWS=IDXWS(INEW)/2
C        FIND OUT THE ACTUAL POSITION OF YP OF BOX (I,J,K)
         IZP=IYZPNT(IYZTBL(I,J,K),NWS)
C        GET THE ROTATION MATRICES
         TY2Y=SQRT(PNT(1)*PNT(1)+PNT(2)*PNT(2)+
     *        PNT(3)*PNT(3))
         IF (NP.LT.NPGP) NP=NPGP
         CALL GETZLL(ZLL,TY2Y,2*NP+1)
         CALL GETCOF2(NP,FLM,ZLL,COEFF)
         CALL ROTR(PNT,EZ,RT)
         CALL GETROT(F,G,RT,NP,CLM)
C
         DO III=1,LNP
            CYP(III)=ZERO
            PYP(III)=ZERO
            YTP(III)=ZERO
         ENDDO
C
         DO IB=IBS(M,1),IBS(M,2)
            LIN=IB*(IB-1)/2
            DO JB=IBS(M,3),IBS(M,4)
               LJN=JB+LIN
               IF (IB.GE.JB) THEN
                  IF (IB.EQ.JB) THEN
                     JP=JP+1
C                     LJN=JB+LIN
                     SUM=0.0D+00
                     SUB=0.0D+00
                     DO III=1,LNPGP
                        CYP(III)=TMPGPL(III,JP)
                     ENDDO
                     CALL RY2YIR(LNP,CYP,PYP,
     *                  YTP,LFG,F,G,
     *                  NP,COEFF)
                     DO II=1,LNP
#if defined(SPEC_CPU)
                        SUM=SUM+DBLE(PYP(II)*ZP(II,IZP))
#else
                        SUM=SUM+DREAL(PYP(II)*ZP(II,IZP))
#endif
                     ENDDO
                     IK=1
                     DO II=0,NP
                        IK=IK+II
#if defined(SPEC_CPU)
                        SUB=SUB+DBLE(PYP(IK)*ZP(IK,IZP))
#else
                        SUB=SUB+DREAL(PYP(IK)*ZP(IK,IZP))
#endif
                     ENDDO
                     SUM=TWO*SUM-SUB
                     FA(LJN)=FA(LJN)+SUM
                  ELSE
                     JP=JP+1
                     SUM=0.0D+00
                     SUB=0.0D+00
                     DO III=1,LNPGP
                        CYP(III)=TMPGPL(III,JP)
                     ENDDO
                     CALL RY2YIR(LNP,CYP,PYP,
     *                  YTP,LFG,F,G,
     *                  NP,COEFF)
                     DO II=1,LNP
#if defined(SPEC_CPU)
                        SUM=SUM+DBLE(PYP(II)*ZP(II,IZP))
#else
                        SUM=SUM+DREAL(PYP(II)*ZP(II,IZP))
#endif
                     ENDDO
                     IK=1
                     DO II=0,NP
                        IK=IK+II
#if defined(SPEC_CPU)
                        SUB=SUB+DBLE(PYP(IK)*ZP(IK,IZP))
#else
                        SUB=SUB+DREAL(PYP(IK)*ZP(IK,IZP))
#endif
                     ENDDO
                     SUM=TWO*(TWO*SUM-SUB)
                     FA(LJN)=FA(LJN)+SUM
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
C
       ENDDO
 100  ENDDO
C      ENDIF
C
C
      IF (GOPARR) THEN
         CALL DDI_SYNC(ITAG)
         CALL DDI_DLBRESET
         DSKWRK=TFLAG
      ENDIF
C
      RETURN
 9000 FORMAT(/10X,48("-")/10X,
     *       48HCONSTRUCT FAR-FIELD CONTRIBUTIONS TO FOCK MATRIX/10X,
     *         48(1H-))
 9100 FORMAT(2X,'BOX(',3I2,')',
     *            ' FA(',2I3,')=',F15.10)
      END
C*MODULE QFMM    *DECK FORMFJ_DISK
      SUBROUTINE FORMFJ_DISK(SCFTYP,NCXYZ,IYP,INDX2,IDXIJK,IBS,LFA,FA,
     *                 FB,NZ,IYZTBL,ZP,NTBOX,MAXWS,IYZPNT,IDXWS,
     *                 MAXNYP,TMPGPL,NFTPLT)
C
C     DISK BASED FORMFJ
C     THIS ROUTINE FORMS FAR FIELD J MATRIX (COULOMB POTENTIAL).
C     FA : ALPHA FOCK MATRIX.
C
C     C. H. CHOI SEP 2000
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (TWO=2.0D+00)
      COMPLEX*16 ZP, TMPGPL
      LOGICAL QOPS,TFLAG,GOPARR,DSKWRK,MASWRK,QFMM,UROHF
      COMMON /QMFM  /SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *       ITERMS,QOPS,ISCUT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DIMENSION IYZTBL(2**(NS+1)-1,2**(NS+1)-1,2**(NS+1)-1),
     *          IYZPNT(NTBOX,MAXWS/2),IDXWS(NCXYZ)
      DIMENSION IYP(NCXYZ),IBS(NCXYZ,4),TMPGPL((NP+1)*(NP+2)/2,MAXNYP),
     *          IDXIJK(NCXYZ,3),INDX2(NCXYZ),
     *          ZP((NP+1)*(NP+2)/2,NZ),
     *          FA(LFA),FB(LFA)
C
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
      CALL SEQREW(NFTPLT)
C
      UROHF = SCFTYP.EQ.UHF  .OR.  SCFTYP.EQ.ROHF
C
C      WRITE(IW,9000)
C
      LNP=(NP+1)*(NP+2)/2
C
      IIP=0
      KP=0
      DO N=1,NCXYZ,NUMRD
       NNEW=N+NUMRD-1
       IF (NNEW.GT.NCXYZ) NNEW=NCXYZ
       IIP=IYP(NNEW)-KP
       KP=KP+IIP
       JP=0
C
       IF (GOPARR) THEN
          MINE=MINE+1
          IF (MINE.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
          IF (NEXT.NE.MINE) THEN
             CALL SEQADV(NFTPLT)
             GOTO 100
          ENDIF
       ENDIF
C
       LENIO = LNP*2*IIP
       CALL SQREAD(NFTPLT,TMPGPL(1,1),LENIO)
C
       DO M=N,NNEW
         INEW=INDX2(M)
C        FIND OUT THE CENTER OF THE BOX TO WHICH THE POINT BELONGS.
         I=IDXIJK(INEW,1)
         J=IDXIJK(INEW,2)
         K=IDXIJK(INEW,3)
C        FIND OUT THE WS INDEX
         NWS=IDXWS(INEW)/2
C        FIND OUT THE ACTUAL POSITION OF YP OF BOX (I,J,K)
         IZP=IYZPNT(IYZTBL(I,J,K),NWS)
C
         IF (SCFTYP.EQ.RHF) THEN
         DO IB=IBS(M,1),IBS(M,2)
            LIN=IB*(IB-1)/2
            DO JB=IBS(M,3),IBS(M,4)
               LJN=JB+LIN
               IF (IB.GE.JB) THEN
                  JP=JP+1
                  IF (IB.EQ.JB) THEN
                     LJN=JB+LIN
                     SUM=0.0D+00
                     SUB=0.0D+00
                     DO II=1,LNP
#if defined(SPEC_CPU)
                        SUM=SUM+DBLE(TMPGPL(II,JP)*ZP(II,IZP))
#else
                        SUM=SUM+DREAL(TMPGPL(II,JP)*ZP(II,IZP))
#endif
                     ENDDO
                     IK=1
                     DO II=0,NP
                        IK=IK+II
#if defined(SPEC_CPU)
                        SUB=SUB+DBLE(TMPGPL(IK,JP)*ZP(IK,IZP))
#else
                        SUB=SUB+DREAL(TMPGPL(IK,JP)*ZP(IK,IZP))
#endif
                     ENDDO
                     SUM=TWO*SUM-SUB
                     FA(LJN)=FA(LJN)+SUM
                  ELSE
                     LJN=JB+LIN
                     SUM=0.0D+00
                     SUB=0.0D+00
                     DO II=1,LNP
#if defined(SPEC_CPU)
                        SUM=SUM+DBLE(TMPGPL(II,JP)*ZP(II,IZP))
#else
                        SUM=SUM+DREAL(TMPGPL(II,JP)*ZP(II,IZP))
#endif
                     ENDDO
                     IK=1
                     DO II=0,NP
                        IK=IK+II
#if defined(SPEC_CPU)
                        SUB=SUB+DBLE(TMPGPL(IK,JP)*ZP(IK,IZP))
#else
                        SUB=SUB+DREAL(TMPGPL(IK,JP)*ZP(IK,IZP))
#endif
                     ENDDO
                     SUM=TWO*(TWO*SUM-SUB)
                     FA(LJN)=FA(LJN)+SUM
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         ELSEIF (UROHF) THEN
         DO IB=IBS(M,1),IBS(M,2)
            LIN=IB*(IB-1)/2
            DO JB=IBS(M,3),IBS(M,4)
               LJN=JB+LIN
               IF (IB.GE.JB) THEN
                  JP=JP+1
                  IF (IB.EQ.JB) THEN
                     LJN=JB+LIN
                     SUM=0.0D+00
                     SUB=0.0D+00
                     DO II=1,LNP
#if defined(SPEC_CPU)
                        SUM=SUM+DBLE(TMPGPL(II,JP)*ZP(II,IZP))
#else
                        SUM=SUM+DREAL(TMPGPL(II,JP)*ZP(II,IZP))
#endif
                     ENDDO
                     IK=1
                     DO II=0,NP
                        IK=IK+II
#if defined(SPEC_CPU)
                        SUB=SUB+DBLE(TMPGPL(IK,JP)*ZP(IK,IZP))
#else
                        SUB=SUB+DREAL(TMPGPL(IK,JP)*ZP(IK,IZP))
#endif
                     ENDDO
                     SUM=TWO*SUM-SUB
                     FA(LJN)=FA(LJN)+SUM
                     FB(LJN)=FB(LJN)+SUM
                  ELSE
                     LJN=JB+LIN
                     SUM=0.0D+00
                     SUB=0.0D+00
                     DO II=1,LNP
#if defined(SPEC_CPU)
                        SUM=SUM+DBLE(TMPGPL(II,JP)*ZP(II,IZP))
#else
                        SUM=SUM+DREAL(TMPGPL(II,JP)*ZP(II,IZP))
#endif
                     ENDDO
                     IK=1
                     DO II=0,NP
                        IK=IK+II
#if defined(SPEC_CPU)
                        SUB=SUB+DBLE(TMPGPL(IK,JP)*ZP(IK,IZP))
#else
                        SUB=SUB+DREAL(TMPGPL(IK,JP)*ZP(IK,IZP))
#endif
                     ENDDO
                     SUM=TWO*(TWO*SUM-SUB)
                     FA(LJN)=FA(LJN)+SUM
                     FB(LJN)=FB(LJN)+SUM
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         ENDIF
       ENDDO
C
 100  ENDDO
      IF (GOPARR) THEN
         CALL DDI_SYNC(ITAG)
         CALL DDI_DLBRESET
         DSKWRK=TFLAG
      ENDIF
C
      RETURN
C9000 FORMAT(/10X,48("-")/10X,
C    *       48HCONSTRUCT FAR-FIELD CONTRIBUTIONS TO FOCK MATRIX/10X,
C    *         48(1H-))
      END
C*MODULE QFMM    *DECK EO
      DOUBLE PRECISION FUNCTION EO(IX)
C
C     THIS ROUTINE RETURNS EITHER 1 OR -1.
C     EO = 1, IF IX IS EVEN OR ZERO
C     EO =-1, IF IX IS ODD.
C
C     C. H. CHOI APRIL 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (ONE=1.0D+00,ZERO=0.0D+00)
      EO=-MOD(ABS(IX),2)
      IF (EO.EQ.ZERO) THEN
         EO=ONE
      ENDIF
C
      RETURN
      END
C*MODULE QFMM    *DECK GET3DX
C      SUBROUTINE GET3DX(N,NS2,IDX)
C
C     THIS ROUTINE RETURNS THE VIRTUAL INDEX (I,J,K) OF A
C     CELL N WITH THE SUBDIVISION LEVEL NS.
C     NS2=2**NS
C
C     I,J,K = 0,1,...2**NS-1
C
C     C. H. CHOI APRIL 1999
C
C      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C      PARAMETER (ONE=1.0D+00)
C      DIMENSION IDX(3)
C
C      IDX(1)=FLOOR((N-ONE)/(NS2*NS2))
C      IDX(2)=FLOOR((N-IDX(1)*NS2*NS2-ONE)/NS2)
C      IDX(3)=N-NS2*(IDX(1)*NS2+IDX(2))-1
C
C      RETURN
C      END
C*MODULE QFMM    *DECK GETCOF
      SUBROUTINE GETCOF(NP,FLM,ZLL,COEFF)
C
C     COEFFICIENTS FOR Y2Z
C
C     C. H. CHOI JUNE 1999
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MAXCOF=23821)
      DIMENSION COEFF(MAXCOF,2),
     *          ZLL(0:2*NP+1),FLM(0:2*NP)
C
      LENG=0
      DO L=0,NP
         SGN_L=EO(L)
         DO M=0,L
            SGN=EO(M)
            DO LK=M,NP
               LENG=LENG+1
               COEFF(LENG,1)=SGN_L*SGN*FLM(L+LK)*FLM(L+LK)
     *             /(ZLL(L+LK+1)*FLM(LK+M)*FLM(LK-M)*
     *              FLM(L+M)*FLM(L-M))
               COEFF(LENG,2)=COEFF(LENG,1)*SGN_L*EO(LK)
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK GETCOF2
      SUBROUTINE GETCOF2(NP,FLM,ZLL,COEFF)
C
C     COEFFICIENTS FOR Y2Y
C
C     C. H. CHOI JULY 2000
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MAXCOF=23821)
      DIMENSION COEFF(MAXCOF),
     *          ZLL(0:2*NP+1),FLM(0:2*NP)
C
      LENG=0
      DO L=0,NP
         DO M=0,L
            DO LK=M,L
               LENG=LENG+1
               COEFF(LENG)=FLM(L+M)*FLM(L-M)*ZLL(L-LK)/(FLM(LK+M)*
     *             FLM(LK-M)*FLM(L-LK)*FLM(L-LK))
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK GETNBR
      SUBROUTINE GETNBR(I,J,K,IWS,NS,NTBOX,NBR,IDX,
     *                   IYZTBL,INTR,MAXWS,IYZPNT,
     *             MINX,MAXX,MINY,MAXY,MINZ,MAXZ)
C
C     THIS ROUTINE RETURNS NEIGHBORS OF CELL (I,J,K).
C     IWS : WELL-SEPARATENESS DETERMINES THE LAYER OF
C     NEIGHBORS.
C
C     C. H. CHOI JUNE 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION NBR((MAXWS*2+1)**3)
      DIMENSION IYZTBL(2**(NS+1)-1,2**(NS+1)-1,2**(NS+1)-1)
      DIMENSION IYZPNT(NTBOX,MAXWS/2)
C
      NS2=2**NS
      NS2=2**NS
      IST=I-IWS
      JST=J-IWS
      KST=K-IWS
      IND=I+IWS
      JND=J+IWS
      KND=K+IWS
C
      IF (MINX.GT.IST) IST=MINX
      IF (MINY.GT.JST) JST=MINY
      IF (MINZ.GT.KST) KST=MINZ
      IF (MAXX.LT.IND) IND=MAXX
      IF (MAXY.LT.JND) JND=MAXY
      IF (MAXZ.LT.KND) KND=MAXZ
C
      IDX=0
      DO II=IST,IND
         DI=(II-I)*NS2*NS2
         DO JJ=JST,JND
            DJ=(JJ-J)*NS2
            DO KK=KST,KND
               DK=KK-K
               NBOX=IYZTBL(II,JJ,KK)
               IF ((NBOX.GT.0).AND.(IYZPNT(NBOX,INTR).GT.0)) THEN
                   IF ( DI+DJ+DK.GT.0 ) THEN
                     IDX=IDX+1
                     NBR(IDX)=NBOX
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK IGETNP
      INTEGER FUNCTION IGETNP(TOL,R,DL)
C
C     THIS ROUTINE RETURNS THE ORDER OF EXPANSION TO ENSURE THAT
C     THE ERROR IS LESS THAN TOL.
C     TOL : ERROR
C     DL : THE DIMENSION OF BOX.
C     Q : CHARGE.
C
C     C. H. CHOI JULY 1999
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (S3=1.732050807568877D+00,HALF=0.5D+00)
C
      IGETNP=INT(LOG10((R-S3*HALF)*DL*TOL)/LOG10(S3*HALF/R))
C
      RETURN
      END
C*MODULE QFMM    *DECK SQOPS
      SUBROUTINE SQOPS(M,ERR,ID,SCLF,NCXYZ)
C
C     D : DIMENSIONALITY
C
C     C. H. CHOI NOV 2000
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL FLAG,QFMM,QOPS,GOPARR,DSKWRK,MASWRK
      PARAMETER (MAXCNT=15,ONE=1.0D+00,TWO=2.0D+00,CNT=1.0D-5)
      COMMON /IOFILE/ IR,IW,IP,IJK,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /QMFM  / SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *                ITERMS,QOPS,ISCUT
C
C     SETTING INITIAL VALUES
C
      NS=2
      R=IWS+ONE
      UNIT=SIZE/NS**2
      NP=IGETNP(ERR,R,UNIT)
      TSIZE=SIZE
      FLAG=.TRUE.
      OLDA=0
      SCLF=ONE
      BYZ=2**(3-ID)
C
      SFACTOR=ONE
C
      IF (MASWRK) WRITE(IW,9000)
      I=0
      DO WHILE(FLAG)
         I=I+1
         A=OPTBOX(NS,NP,TSIZE,IWS,M,ID)/BYZ
         IF (ID.NE.1) THEN
            CALL QFMMBOX(NCXYZ,NBOX)
            NBOX=INT(NBOX/BYZ)
            SFACTOR=NBOX/TWO**(ID*NS)
            A=A/SFACTOR
         ENDIF
C
         IF (ID.EQ.1) THEN
            NS=INT(LOG10(A)/LOG10(TWO)+ONE)
         ELSEIF (ID.EQ.3) THEN
            NS=INT(LOG10(A)/LOG10(8.0D+00)+ONE)
         ELSEIF (ID.EQ.2) THEN
            NS=INT(LOG10(A)/LOG10(4.0D+00)+ONE)
         ENDIF
C
         IF (NS.LT.2) GOTO 2000
C
         SCLF=(TWO**(ID*NS)/A)**(ONE/ID)
         TSIZE=SCLF*SIZE
         UNIT=TSIZE/NS**TWO
         NP=IGETNP(ERR,R,UNIT)
         TEST=ABS(OLDA-A)
         IF (MASWRK) WRITE(IW,9010) I,TEST,NS,NP,SCLF,TSIZE,SFACTOR
         IF (TEST.LE.CNT) THEN
            FLAG=.FALSE.
         ELSE
            OLDA=A
         ENDIF
         IF (I.GT.MAXCNT) GOTO 1000
      ENDDO
      IF (SCLF.GE.ONE) SIZE=SIZE*SCLF
      IF(MASWRK) WRITE(IW,9040) I
      RETURN
C
 1000 CONTINUE
      IF(MASWRK) WRITE(IW,9020) I
      IF (SCLF.GE.ONE) SIZE=SIZE*SCLF
      RETURN
C
 2000 CONTINUE
      IF(MASWRK) WRITE(IW,9030)
      NS=2
C
      RETURN
 9000 FORMAT(/14X,17("-")/14X,"QOPS OPTIMIZATION"/14X,17("-")/
     *5X,'ITER           DIFF        NS     NP       SCLF',
     *'    CUBE SIZE      SFACTOR'/,2X,78(1H-))
 9010 FORMAT(5X,I3,4X,F15.8,4X,I3,4X,I3,4X,F8.5,2X,F10.5,4X,F8.5)
 9020 FORMAT(/10X,'QOPS WAS NOT CONVERGED AT ITERATION :',I3)
 9030 FORMAT(/10X,'THE SYSTEM IS TOO SMALL TO USE QOPS'/)
 9040 FORMAT(/10X,'QOPS CONVERGED AFTER ',I3,' ITERATIONS')
      END
C*MODULE QFMM    *DECK GETZLL
      SUBROUTINE GETZLL(ZLL,Z,NP)
C
C     THIS ROUTINE GETS THE CLL DATA.
C     ZLL(LP-L)=Z**(LP-L)
C
C     C. H. CHOI MAY 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION ZLL(0:NP)
      ZLL(0)=1.0D+00
      DO LLM=1,NP
         ZLL(LLM)=Z*ZLL(LLM-1)
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK GETCLM
      SUBROUTINE GETCLM(CLM)
C
C     THIS ROUTINE GETS THE CLM DATA.
C
C     C. H. CHOI APRIL 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL QOPS,QFMM
      COMMON /QMFM  /SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *       ITERMS,QOPS,ISCUT
      DIMENSION CLM(-NP:NP)
      DO LLM=-NP,NP
#if defined(SPEC_CPU)
         CLM(LLM)=SQRT(DBLE(NP+LLM))
#else
         CLM(LLM)=SQRT(DFLOAT(NP+LLM))
#endif
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK GETCTR
      SUBROUTINE GETCTR(SIZE,NS2,I,J,K,CTR)
C
C     THIS ROUTINE RETURNS THE CENTER OF M-TH CELL OF
C     SUBDIVISION LEVEL NS.
C     NS2 = 2**NS
C
C     C. H. CHOI APRIL 1999
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (HALF=0.5D+00)
      DIMENSION CTR(4)
C
      UNIT=SIZE/NS2
      BASE=HALF*(UNIT-SIZE)
      CTR(1)=BASE+(I-1)*UNIT
      CTR(2)=BASE+(J-1)*UNIT
      CTR(3)=BASE+(K-1)*UNIT
      CTR(4)=0.0D+00
C
      RETURN
      END
C*MODULE QFMM    *DECK GETD
      SUBROUTINE GETD(NP,CLM,F,G)
C
C     THIS ROUTINE RETURNS THE FOUR WIGNER ROTATION MATRICES,
C     D (=F+ I G).
C
C     C. H. CHOI MAY 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (ONE=1.0D+00,ZERO=0.0D+00)
      DIMENSION F((NP*(NP+1)*(NP+2)*4/3)+(NP+1),8),EZ(3),T(3),RT(3,3),
     *          G((NP*(NP+1)*(NP+2)*4/3)+(NP+1),8),CLM(-NP:NP)
C
C     Z-AXIS
      EZ(1)=ZERO
      EZ(2)=ZERO
      EZ(3)=ONE
C
      M=1
      DO I=1,2
      T(1)=-ONE+(I-1)*2
         DO J=1,2
            T(2)=-ONE+(J-1)*2
            DO K=1,2
               T(3)=-ONE+(K-1)*2
               CALL ROTR(T,EZ,RT)
               CALL GETROT(F(1,M),G(1,M),RT,NP,CLM)
               M=M+1
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK GETFLM
      SUBROUTINE GETFLM(FLM)
C
C     THIS ROUTINE GETS THE FLM DATA.
C
C     C. H. CHOI APRIL 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL QOPS,QFMM
      COMMON /QMFM  /SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *       ITERMS,QOPS,ISCUT
      DIMENSION FLM(-NP:NP)
      DO LLM=-NP,NP
         FLM(LLM)=SQRT(FACT(NP+LLM))
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK GETROT
      SUBROUTINE GETROT(F,G,R,L,CLM)
C
C     THIS ROUTINE CALCULATES THE WIGNER ROTATION MATRICES (D=F+I*G)
C     OF ORDER 0,1,2,...L ON THE BASIS OF R WHICH MUST BE
C     GIVEN BY THE USER.
C     THESE MATRICES ARE RETURNED IN F AND G IN ORDER OF INCREASING L.
C     FOR EXAMPLE, THE FIRST ELEMENT IN F/G IS 1 FOR L=0.
C     THE NEXT NINE ELEMENTS MAKE UP THE WIGNER 3X3 MATRIX FOR L=1.
C     THE GROUP OF ELEMENTS IN F/G, STARTING FROM POSITION
C
C        START=(L-1)*L*(L+1)*4/3+(L+1),
C
C     WHICH MAKE UP THE (2*L+1)X(2*L+1)
C     MATRIX, F/G, MAY BE UNPACKED IN THE FOLLOWING MANNER :
C
C        F(I,J)=F(START+((J-1)*(2*L+1))+I-1)
C
C     THE LABELLING OF THE COLUMNS AND ROWS OF F/G WITH RESPECT TO L,M
C     IS GIVEN IN THE SUBROUTINE ROTL.
C
C     C.H. CHOI, MARCH 1999
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ONE=1.0D+00,ZERO=0.0D+00)
      DIMENSION R(3,3), F((L*(L+1)*(L+2)*4/3)+(L+1)),
     *   G((L*(L+1)*(L+2)*4/3)+(L+1)),CLM(-L:L)
C     ASSIGN F/G OF ORDER 0.
      F(1)=ONE
      G(1)=ZERO
C     GET F/G OF ORDER 1.
      CALL ROTC(F(2),G(2),R)
C     CALCULATE F/G OF ORDER 2,3,...L
      DO LN=2,L
         IDL = (LN-2)*(LN-1)*LN*4/3+LN
         IDU = (LN-1)*LN*(LN+1)*4/3+LN+1
         CALL ROTL(F(2),G(2),F(IDL),G(IDL),F(IDU),
     *            G(IDU),CLM(-L),LN)
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK GETY_SEMI
      SUBROUTINE GETY_SEMI(NCXYZ,IYP,INDX2,IDXIJK,IDXWS,CXYZ,IBS,
     *                IYZTBL,NTMPL,YP,
     *                L2,DMAT,NTBOX,MAXWS,
     *                IYZPNT,F,G,ZLL,CLM,FLM,MAXNYP,TMPGPL,NFTPL,NFTPLT)
C
C     THIS ROUTINE RETURNS YP (MULTIPOLE MOMENTS OF BOXES) FOR QFMM,
C     THE OUTPUT IYZTBL CONTAINS THE POINTER WHICH POINTS THE
C     CORRESPONDING YP(:,POINTER).
C
C     YP : COMPLEX[(NP+1)*(NP+2)/2 X NZ]
C
C     C. H. CHOI SEP 2000
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO=0.00D+00,MAXNP=50)
      PARAMETER (MAXCOF=23821)
      COMPLEX*16 YP,TMPGPL, PYP, YTP,YTP2
      LOGICAL QOPS,QFMM
      LOGICAL TMPDSK,GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /QMFM  /SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *       ITERMS,QOPS,ISCUT
      DIMENSION IYP(NCXYZ),CXYZ(NCXYZ,3),IBS(NCXYZ,4),
     *          TMPGPL((NPGP+1)*(NPGP+2)/2,MAXNYP),
     *          IDXIJK(NCXYZ,3),INDX2(NCXYZ),IDXWS(NCXYZ),
     *          IYZTBL(2**(NS+1)-1,2**(NS+1)-1,2**(NS+1)-1),
     *          F((NP*(NP+1)*(NP+2)*4/3)+(NP+1)),CLM(-NP:NP),
     *          G((NP*(NP+1)*(NP+2)*4/3)+(NP+1)),FLM(-NP:NP),
     *          COEFF(MAXCOF),ZLL(0:2*NP+1),YP((NP+1)*(NP+2)/2,NTMPL),
     *          PYP((MAXNP+1)*(MAXNP+2)/2),
     *          YTP((MAXNP+1)*(MAXNP+2)/2),YTP2((MAXNP+1)*(MAXNP+2)/2),
     *          DMAT(L2),RT(3,3),PNT(4),CTR(4),EZ(3),
     *          IYZPNT(NTBOX,MAXWS/2)
C
C
C     PARALLEL INITIALIZATION
C
      NEXT = -1
      MINE = -1
C
      IF (GOPARR) THEN
         TMPDSK=DSKWRK
         DSKWRK=.TRUE.
      ENDIF
      CALL SEQREW(NFTPL)
      CALL SEQREW(NFTPLT)
C
      NS2=2**NS
      LNP=(NP+1)*(NP+2)/2
      LFG=(NP*(NP+1)*(NP+2)*4/3)+(NP+1)
      LNPGP=(NPGP+1)*(NPGP+2)/2
      DO J=1,NTMPL
         DO I=1,LNP
            YP(I,J)=ZERO
         ENDDO
      ENDDO
C
      EZ(1)=0.0D+00
      EZ(2)=0.0D+00
      EZ(3)=1.0D+00
C
C     LOOP OVER ALL THE POINTS IN THE LOWEST LEVEL
C
      IIP=0
      KP=0
      DO N=1,NCXYZ,NUMRD
       NNEW=N+NUMRD-1
       IF (NNEW.GT.NCXYZ) NNEW=NCXYZ
       IIP=IYP(NNEW)-KP
       KP=KP+IIP
       JP=0
C
       IF (GOPARR) THEN
          MINE=MINE+1
          IF (MINE.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
          IF (NEXT.NE.MINE) THEN
             CALL SEQADV(NFTPLT)
             GOTO 100
          ENDIF
       ENDIF
C
       LENIO = LNPGP*2*IIP
       CALL SQREAD(NFTPLT,TMPGPL(1,1),LENIO)
       DO M=N,NNEW
         INEW=INDX2(M)
C
C        FIND OUT THE CENTER OF THE BOX TO WHICH THE POINT BELONGS.
         I=IDXIJK(INEW,1)
         J=IDXIJK(INEW,2)
         K=IDXIJK(INEW,3)
         CALL GETCTR(SIZE,NS2,I,J,K,CTR)
C        FIND OUT THE TRANSLATION VECTOR OF THE POINT
         DO II=1,3
            PNT(II)=CXYZ(INEW,II)-CTR(II)
         ENDDO
C        FIND OUT THE WS INDEX
         NWS=IDXWS(INEW)/2
C        FIND OUT THE ACTUAL POSITION OF YP OF BOX (I,J,K)
         IZP=IYZPNT(IYZTBL(I,J,K),NWS)
C        GET THE ROTATION MATRICES
         TY2Y=SQRT(PNT(1)*PNT(1)+PNT(2)*PNT(2)+
     *        PNT(3)*PNT(3))
         IF (NP.LT.NPGP) NP=NPGP
         CALL GETZLL(ZLL,TY2Y,2*NP+1)
         CALL GETCOF2(NP,FLM,ZLL,COEFF)
         CALL ROTR(PNT,EZ,RT)
         CALL GETROT(F,G,RT,NP,CLM)
C
         DO III=1,LNP
            PYP(III)=ZERO
            YTP(III)=ZERO
            YTP2(III)=ZERO
         ENDDO
C
         DO IB=IBS(M,1),IBS(M,2)
            LIN=IB*(IB-1)/2
            DO JB=IBS(M,3),IBS(M,4)
               LJN=JB+LIN
               IF (IB.GE.JB) THEN
                  IF (IB.EQ.JB) THEN
                     JP=JP+1
                     DO II=1,LNPGP
                        PYP(II)=PYP(II)
     *                      +DMAT(LJN)*TMPGPL(II,JP)
                     ENDDO
                  ELSE
                     JP=JP+1
                     D2MAT=DMAT(LJN)*2.0D+00
                     DO II=1,LNPGP
                        PYP(II)=PYP(II)
     *                     +D2MAT*TMPGPL(II,JP)
                     ENDDO
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
C
         CALL RY2YIR(LNP,PYP,YTP2,
     *        YTP,LFG,F,G,
     *        NP,COEFF)
         DO II=1,LNP
            YP(II,IZP)=YP(II,IZP)+YTP2(II)
         ENDDO
       ENDDO
 100  ENDDO
C
      IF (GOPARR) THEN
C         CALL DDI_SYNC(ITAG)
C         CALL DDI_DLBRESET
         DSKWRK=TMPDSK
      ENDIF
      RETURN
      END
C*MODULE QFMM    *DECK GETY_DISK
      SUBROUTINE GETY_DISK(SCFTYP,NCXYZ,IYP,INDX2,IDXIJK,IDXWS,IBS,
     *                IYZTBL,NTMPL,YP,L2,DMAT,DMATB,NTBOX,MAXWS,
     *                IYZPNT,MAXNYP,TMPGPL,NFTPLT)
C
C     THIS ROUTINE RETURNS YP (MULTIPOLE MOMENTS OF BOXES) FOR QFMM,
C     THE OUTPUT IYZTBL CONTAINS THE POINTER WHICH POINTS THE
C     CORRESPONDING YP(:,POINTER).
C
C     YP : COMPLEX[(NP+1)*(NP+2)/2 X NZ]
C
C     C. H. CHOI SEP 2000
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO=0.00D+00)
      COMPLEX*16 YP, TMPGPL
      LOGICAL QOPS,QFMM,UROHF
      LOGICAL TMPDSK,GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /QMFM  /SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *       ITERMS,QOPS,ISCUT
      DIMENSION IYP(NCXYZ),IBS(NCXYZ,4),IDXIJK(NCXYZ,3),INDX2(NCXYZ),
     *          IDXWS(NCXYZ),TMPGPL((NP+1)*(NP+2)/2,MAXNYP),
     *          IYZTBL(2**(NS+1)-1,2**(NS+1)-1,2**(NS+1)-1),
     *          YP((NP+1)*(NP+2)/2,NTMPL),DMAT(L2),DMATB(L2)
      DIMENSION IYZPNT(NTBOX,MAXWS/2)
C
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
C
C     PARALLEL INITIALIZATION
C
      NEXT = -1
      MINE = -1
C
      IF (GOPARR) THEN
         TMPDSK=DSKWRK
         DSKWRK=.TRUE.
      ENDIF
C
      CALL SEQREW(NFTPLT)
C
      UROHF = SCFTYP.EQ.UHF  .OR.  SCFTYP.EQ.ROHF
C
      LNP=(NP+1)*(NP+2)/2
C
      DO J=1,NTMPL
         DO I=1,LNP
            YP(I,J)=ZERO
         ENDDO
      ENDDO
C
C     LOOP OVER ALL THE POINTS IN THE LOWEST LEVEL
C
      IIP=0
      KP=0
      DO N=1,NCXYZ,NUMRD
       NNEW=N+NUMRD-1
       IF (NNEW.GT.NCXYZ) NNEW=NCXYZ
       IIP=IYP(NNEW)-KP
       KP=KP+IIP
       JP=0
C
       IF (GOPARR) THEN
          MINE=MINE+1
          IF (MINE.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
          IF (NEXT.NE.MINE) THEN
             CALL SEQADV(NFTPLT)
             GOTO 100
          ENDIF
       ENDIF
       LENIO = LNP*2*IIP
       CALL SQREAD(NFTPLT,TMPGPL(1,1),LENIO)
C
       DO M=N,NNEW
          INEW=INDX2(M)
C         FIND OUT THE CENTER OF THE BOX TO WHICH THE POINT BELONGS.
          I=IDXIJK(INEW,1)
          J=IDXIJK(INEW,2)
          K=IDXIJK(INEW,3)
C
          NWS=IDXWS(INEW)/2
C         FIND OUT THE ACTUAL POSITION OF YP OF BOX (I,J,K)
          IZP=IYZPNT(IYZTBL(I,J,K),NWS)
C
          IF (SCFTYP.EQ.RHF) THEN
             DO IB=IBS(M,1),IBS(M,2)
                LIN=IB*(IB-1)/2
                DO JB=IBS(M,3),IBS(M,4)
                   LJN=JB+LIN
                   IF (IB.GE.JB) THEN
                      JP=JP+1
                      IF (IB.EQ.JB) THEN
                         DO II=1,LNP
                            YP(II,IZP)=YP(II,IZP)
     *                          +DMAT(LJN)*TMPGPL(II,JP)
                         ENDDO
                      ELSE
                         D2MAT=DMAT(LJN)*2.0D+00
                         DO II=1,LNP
                            YP(II,IZP)=YP(II,IZP)
     *                         +D2MAT*TMPGPL(II,JP)
                         ENDDO
                      ENDIF
                   ENDIF
                ENDDO
             ENDDO
          ELSEIF (UROHF) THEN
             DO IB=IBS(M,1),IBS(M,2)
                LIN=IB*(IB-1)/2
                DO JB=IBS(M,3),IBS(M,4)
                   LJN=JB+LIN
                   IF (IB.GE.JB) THEN
                      JP=JP+1
                      IF (IB.EQ.JB) THEN
                         DO II=1,LNP
                            YP(II,IZP)=YP(II,IZP)
     *                          +(DMAT(LJN)+DMATB(LJN))*TMPGPL(II,JP)
                         ENDDO
                      ELSE
                         D2MAT=(DMAT(LJN)+DMATB(LJN))*2.0D+00
                         DO II=1,LNP
                            YP(II,IZP)=YP(II,IZP)
     *                         +D2MAT*TMPGPL(II,JP)
                         ENDDO
                      ENDIF
                   ENDIF
                ENDDO
             ENDDO
          ENDIF
       ENDDO
 100  ENDDO
      IF (GOPARR) THEN
C         CALL DDI_SYNC(ITAG)
C         CALL DDI_DLBRESET
         DSKWRK=TMPDSK
      ENDIF
C
      RETURN
      END
C*MODULE QFMM    *DECK SORTWS
      SUBROUTINE SORTWS(NCXYZ,INDX,INDX2,INDX3,IDXWS,CXYZ
     *                ,NTBOX,MBOX,NSBOX,MAXWS,NUMWS,NTMPL)
C
C     C. H. CHOI APRIL 2000
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION INDX(NCXYZ),INDX2(NCXYZ),IDXWS(NCXYZ),CXYZ(NCXYZ,3)
      DIMENSION INDX3(NCXYZ),MBOX(NTBOX),NUMWS(NTBOX,MAXWS/2),NSBOX(20)
C
      MST=0
      NTMPL=0
      DO NON=1,NSBOX(1)
         IF (NON.EQ.1) THEN
            MST=1
         ELSE
            MST=MST+MBOX(NON-1)
         ENDIF
         MSIZE=MBOX(NON)
         CALL ISORT5(MSIZE,IDXWS(MST),CXYZ(MST,1),CXYZ(MST,2),
     *         CXYZ(MST,3),INDX(MST))
C
         IREF=2
         MIDX=MST-1
         ISIZWS=1
         DO I=1,MAXWS/2
            NUMWS(NON,I)=0
         ENDDO
C
         DO I=1,MSIZE
            J=MIDX+I
            IFLAG=IDXWS(J)
            IF ( IFLAG.GT.IREF ) THEN
               IDIFF=(IFLAG-IREF)/2
               ISIZWS=ISIZWS+IDIFF
               IREF=IFLAG
            ENDIF
            NUMWS(NON,ISIZWS)=NUMWS(NON,ISIZWS)+1
         ENDDO
         DO I=1,MAXWS/2
            IF (NUMWS(NON,I).NE.0) NTMPL=NTMPL+1
         ENDDO
      ENDDO
      DO I=1,NCXYZ
         INDX2(I)=I
         INDX3(I)=INDX(I)
      ENDDO
      CALL ISORT2(NCXYZ,INDX3,INDX2)
C
      RETURN
      END
C*MODULE QFMM    *DECK IROTLT
      SUBROUTINE IROTLT(LFG,F,G,LNP,YLM,NYLM,LP)
C
C     THIS ROUTINE ROTATES IRREGULAR YLM INTO NYLM VIA D
C     BACKWARD.
C       NYLM=YLM*D
C     WHERE D = F + I G
C
C     LP IS THE ORDER OF MULTIPOLE IN THE EXPANSION.
C
C     C. H. CHOI APRIL 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (ZERO=0.0D+00)
      COMPLEX*16  YLM, NYLM
      DIMENSION NYLM(0:LNP-1),YLM(0:LNP-1),F(LFG),G(LFG)
C
      DO L=0,LP
         ID=(L-1)*L*(L+1)*4/3+(L+1)+L
         IYLM=L*(L+1)/2
         DO M=0,L
            IMD=ID+M
            LM=IYLM+M
C
            NYLM(LM)=ZERO
            KM=IMD+L*(2*L+1)
            NYLM(LM)=NYLM(LM)+DCMPLX(F(KM),-G(KM))*YLM(IYLM)
            SIG=1.0D+00
            DO K=1,L
               KM=IMD+(K+L)*(2*L+1)
               KMN=IMD+(-K+L)*(2*L+1)
               LK=IYLM+K
               SIG=-SIG
               NYLM(LM)=NYLM(LM)+SIG*DCMPLX(F(KMN),-G(KMN))*
     *                DCONJG(YLM(LK))
               NYLM(LM)=NYLM(LM)+DCMPLX(F(KM),-G(KM))*YLM(LK)
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK IROTMM
      SUBROUTINE IROTMM(LFG,F,G,LNP,YLM,NYLM,LP)
C
C     THIS ROUTINE ROTATES COMPLEX CONJUGATE OF REGULAR
C     YLM INTO NYLM VIA D BACKWARD.
C       NYLM=YLM*D
C     WHERE D = F + I G
C
C     LP IS THE ORDER OF MULTIPOLE IN THE EXPANSION.
C
C     C. H. CHOI APRIL 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (ZERO=0.0D+00)
      COMPLEX*16 YLM, NYLM
      DIMENSION NYLM(0:LNP-1),YLM(0:LNP-1),
     *          F(LFG),G(LFG)
C
      DO L=0,LP
         ID=(L-1)*L*(L+1)*4/3+(L+1)+L
         IYLM=L*(L+1)/2
         DO M=0,L
            IMD=ID+M
            LM=IYLM+M
C
            NYLM(LM)=ZERO
            KM=IMD+L*(2*L+1)
            NYLM(LM)=NYLM(LM)+DCMPLX(F(KM),G(KM))*YLM(IYLM)
            SIG=1.0D+00
            DO K=1,L
               KMN=IMD+(-K+L)*(2*L+1)
               KM=IMD+(K+L)*(2*L+1)
               LK=IYLM+K
               SIG=-SIG
               NYLM(LM)=NYLM(LM)+SIG*DCMPLX(F(KMN),G(KMN))*
     *               DCONJG(YLM(LK))
               NYLM(LM)=NYLM(LM)+DCMPLX(F(KM),G(KM))*YLM(LK)
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK WHCHBOX
      SUBROUTINE WHCHBOX(IC,JC,KC,IBOX)
C
C     IN FACT, IBOX SHOULD BE BETWEEN 1 TO 8 DEPENDING
C     ON THE RELATIVE POSITION OF
C     CHILD BOX W.R.T. ITS PARENT'S.
C     ISGN = 1, WHEN IBOX = 1,2,3,4
C     ISGN = 2, WHEN IBOX = 5,6,7,8
C
C     C. H. CHOI MAY 1999
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      IBOX=1
      IF (MOD(IC,2).EQ.0) THEN
         IBOX=IBOX+4
      ENDIF
      IF (MOD(JC,2).EQ.0) IBOX=IBOX+2
      IF (MOD(KC,2).EQ.0) IBOX=IBOX+1
C     ENDIF
C
      RETURN
      END
C*MODULE QFMM    *DECK OPTBOX
      DOUBLE PRECISION FUNCTION OPTBOX(NS,NP,SIZE,IWS,M,ID)
C
C     D : DIMENSIONALITY
C     M : TOTAL NUMBER OF MULTIPOLE EXPANSIONS
C
C     C. H. CHOI NOV 2000
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (TD=4.68124470306409D-08,DC=7.00798664936392D+00)
      PARAMETER (A=0.011022177044024D+00,TS=1.09687499999999D-07)
      PARAMETER (ZERO=0.0D+00,ONE=1.0D+00,TWO=2.0D+00)
C
C     SETTING INITIAL VALUES
C
      SLB=SIZE/2**NS
      FSUM=ZERO
C
      DO I=2,NS
         F1=(TWO*(IWS+DC/SLB/TWO**(NS-I))+ONE)
         T1=( (TWO*F1)**ID-F1**ID+TWO )
         FSUM=FSUM+T1
      ENDDO
      T2=TS*(2*(2*NP+ONE)**3+(NP+ONE)**3)
      TT=FSUM*T2
C
      F2=2*(IWS+DC/SLB)+ONE
      OPTBOX=M*SQRT(TD*(F2**ID+ONE)/(TWO*(TT+A)))
C
      RETURN
      END
C*MODULE QFMM    *DECK P2C
      SUBROUTINE P2C(IYZTBL,F,G,CLM,FLM,ZLL
     *                 ,NZ,ZP,IP2C,NTBOX,MAXWS,IYZPNT)
C
C
C     C. H. CHOI JUNE 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (HALF=0.5D+00,MAXNP=50)
      COMPLEX*16 CZP, PYP, ZP
      LOGICAL QOPS,QFMM
      COMMON /QMFM  /SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *       ITERMS,QOPS,ISCUT
      DIMENSION IYZTBL(2**(NS+1)-1,2**(NS+1)-1,2**(NS+1)-1),
     *          F((NP*(NP+1)*(NP+2)*4/3)+(NP+1),8),
     *          G((NP*(NP+1)*(NP+2)*4/3)+(NP+1),8),
     *          CLM(-NP:NP),T(3),
     *          FLM(-NP:NP),
     *          ZLL(0:2*NP+1,2),
     *          ZP((NP+1)*(NP+2)/2,NZ),
     *          CZP((MAXNP+1)*(MAXNP+2)/2),
     *          PYP((MAXNP+1)*(MAXNP+2)/2),CTRP(3),CTRC(3)
      DIMENSION IYZPNT(NTBOX,MAXWS/2)
C
      IP2C=0
      LNP=INT((NP+1)*(NP+2)*HALF)
      LFG=(NP*(NP+1)*(NP+2)*4/3)+(NP+1)
      CALL GETD(NP,CLM,F,G)
C
      DO NSS=2,NS-1
         NSS2=2**NSS
C
         DO ISET=1,NSS2/2
           IF ( ISET.GT.MAXWS/2 ) GOTO 100
            IREFWS=ISET
            DO IS2S=ISET*2-1,ISET*2
           IF ( IS2S.GT.MAXWS/2 ) GOTO 100
               JREFWS=IS2S
C
         IPST=2**(NS+1)-2**(NSS+1)
         UNIT=SIZE/NSS2
         UNIT2=UNIT/2
         BASE=HALF*(UNIT-SIZE)
         BASE2=HALF*(UNIT2-SIZE)
C
         DO I=1,NSS2
            IP=I+IPST
            CTRP(1)=BASE+(I-1)*UNIT
            DO J=1,NSS2
               JP=J+IPST
               CTRP(2)=BASE+(J-1)*UNIT
               DO K=1,NSS2
                  KP=K+IPST
                  CTRP(3)=BASE+(K-1)*UNIT
C
               NONST=IYZTBL(IP,JP,KP)
               IYPST=IYZPNT(NONST,IREFWS)
                  IF (NONST.GT.0) THEN
               IYPST=IYZPNT(NONST,IREFWS)
                 IF (IYPST.GT.0) THEN
C
C        Z2Z TRANSLATIONS.
C
                     ICST=2**(NS+1)-2**(NSS+2)
                     I2=I*2
                     J2=J*2
                     K2=K*2
                     IBOX=0
C
C     GET THE ROTATION MATRICES
C
                     DO II=I2-1,I2
                        IC=II+ICST
                        CTRC(1)=BASE2+(II-1)*UNIT2
                        DO JJ=J2-1,J2
                           JC=JJ+ICST
                           CTRC(2)=BASE2+(JJ-1)*UNIT2
                           DO KK=K2-1,K2
                              KC=KK+ICST
                              CTRC(3)=BASE2+(KK-1)*UNIT2
                              IBOX=IBOX+1
                              NONEND=IYZTBL(IC,JC,KC)
C
                              IF (NONEND.GT.0) THEN
                              IYPEND=IYZPNT(NONEND,JREFWS)
                              IF (IYPEND.GT.0 ) THEN
                                 DO III=1,3
                                    T(III)=CTRC(III)-CTRP(III)
                                 ENDDO
                                 TZ2Z=SQRT(T(1)*T(1)+T(2)*T(2)+
     *                                T(3)*T(3))
                                 CALL GETZLL(ZLL,TZ2Z,2*NP+1)
                                 CALL RZ2ZIR(LNP,ZP(1,IYPST),
     *                                CZP,PYP,LFG,F(1,IBOX),
     *                                G(1,IBOX),NP,FLM,ZLL)
                                 DO L=1,LNP
                                    ZP(L,IYPEND)=
     *                               ZP(L,IYPEND)+CZP(L)
                                 ENDDO
                                 IP2C=IP2C+1
                              ENDIF
                              ENDIF
                           ENDDO
                        ENDDO
                     ENDDO
                  ENDIF
                 ENDIF
               ENDDO
            ENDDO
         ENDDO
         ENDDO
        ENDDO
 100  ENDDO
C
      RETURN
 9000 FORMAT(/10X,18("-")/10X,"P TO C TRANSLATION"/10X,18("-"))
      END
C*MODULE QFMM    *DECK ROTC
      SUBROUTINE ROTC(F,G,R)
C
C     THIS ROUTINE RETURNS THE WIGNER ROTATION MATRIX (D) OF ORDER 1
C     ON THE BASIS OF 3X3 ORTHOGONAL COORDINATE TRANSFORMATION MATRIX
C     R.
C     F AND G ARE REAL AND IMAGINARY PARTS OF IT.
C     CONSEQUENTLY, D = F + I*G.
C     AND F AND G ARE DETERMINED ENTIRELY BY R.
C
C     REF : CHOI, C.H.; IVANIC, J.; GORDON, M.S.; RUEDENBERG, K,
C           J. CHEM. PHYS.
C
C     C. H. CHOI MARCH 1999
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (FS=0.70710678118655D+00,F2=0.5D+00,ZERO=0.0D+00)
      DIMENSION R(3,3),F(-1:1,-1:1),G(-1:1,-1:1)
C
      F(-1,-1)=F2*(R(2,2)+R(1,1))
      F( 0,-1)= FS*R(3,1)
      F( 1,-1)=F2*(R(2,2)-R(1,1))
      F(-1, 0)= FS*R(1,3)
      F( 0, 0)=    R(3,3)
      F( 1, 0)=-FS*R(1,3)
      F(-1, 1)=F2*(R(2,2)-R(1,1))
      F( 0, 1)=-FS*R(3,1)
      F( 1, 1)=F2*(R(2,2)+R(1,1))
      G(-1,-1)=F2*(R(2,1)-R(1,2))
      G( 0,-1)=-FS*R(3,2)
      G( 1,-1)=F2*(R(2,1)+R(1,2))
      G(-1, 0)= FS*R(2,3)
      G( 0, 0)= ZERO
      G( 1, 0)= FS*R(2,3)
      G(-1, 1)=-F2*(R(2,1)+R(1,2))
      G( 0, 1)=-FS*R(3,2)
      G( 1, 1)=F2*(R(1,2)-R(2,1))
C
      RETURN
      END
C*MODULE QFMM    *DECK ROTL
      SUBROUTINE ROTL(F,G,FO,GO,FN,GN,CLM,L)
C
C     THIS ROUTINE CALCULATES THE ROTATION MATRIX (D=FN+I*GN)
C     OF ORDER L WHICH ROTATES COMPLEX SPHERICAL HARMONICS OF ORDER L
C     ON THE BASIS OF ORDER L-1 (F,G) AND 1 (FO,GO).
C     THERE IS EXACT CORRESPONDENCE BETWEEN THE ROWS(I) AND COLUMNS(J)
C     OF THE MATRICES, AND M AND M' INDICES.
C
C        FN/GN(M,M') : (M,M') ELEMENT OF THE FN/GN
C
C     C.H. CHOI, MARCH 1999
C
C     REF : CHOI, C.H.; IVANIC, J.; GORDON, M.S.; RUEDENBERG, K.
C           J. CHEM. PHYS. 1999, 111, 8825.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (SQ=1.414213562373095D+00)
      DIMENSION F(-1:1,-1:1),G(-1:1,-1:1),FO(-L+1:L-1,-L+1:L-1),
     *   GO(-L+1:L-1,-L+1:L-1),FN(-L:L,-L:L),GN(-L:L,-L:L),
     *   CLM(-L:L)
C     LET'S DEFINE FREQUENT INDICES.
      IP  =  L-1
      IN  = -L+1
      I2P =  L-2
      I2N = -L+2
C     ---------------------------------------------
C            THE CASES OF ABS(M') = L
C     ---------------------------------------------
C
C     CASE 1 : M=L,-L    M'=L,-L
      D=CLM(L)*CLM(IP)/(CLM(L)*CLM(IP))
      FN( L, L)=D*(F( 1,1)*FO(IP,IP)-G( 1,1)*GO(IP,IP))
      GN( L, L)=D*(F( 1,1)*GO(IP,IP)+G( 1,1)*FO(IP,IP))
      FN(-L, L)=D*(F(-1,1)*FO(IN,IP)-G(-1,1)*GO(IN,IP))
      GN(-L, L)=D*(F(-1,1)*GO(IN,IP)+G(-1,1)*FO(IN,IP))
C     DUE TO THE PARITY RELATIONS.
      FN( L,-L)= FN(-L,L)
      GN( L,-L)=-GN(-L,L)
      FN(-L,-L)= FN( L,L)
      GN(-L,-L)=-GN( L,L)
C
C     CASE 2 : M= L-1,  M'=L,-L
C     CASE 3 : M=-L+1,  M'=L,-L
C
      D=CLM(IP)*CLM(I2P)/(CLM(L)*CLM(IP))
      C=SQ*CLM(IP)*CLM(IN)/(CLM(L)*CLM(IP))
      FN(IP, L)=D*(F(1, 1)*FO(I2P,IP)-G(1, 1)*GO(I2P,IP))
     *         +C*(F(0, 1)*FO(IP, IP)-G(0, 1)*GO(IP, IP))
      GN(IP, L)=D*(F(1, 1)*GO(I2P,IP)+G(1, 1)*FO(I2P,IP))
     *         +C*(F(0, 1)*GO(IP, IP)+G(0, 1)*FO(IP, IP))
      FN(IP,-L)=D*(F(1,-1)*FO(I2P,IN)-G(1,-1)*GO(I2P,IN))
     *         +C*(F(0,-1)*FO(IP, IN)-G(0,-1)*GO(IP, IN))
      GN(IP,-L)=D*(F(1,-1)*GO(I2P,IN)+G(1,-1)*FO(I2P,IN))
     *         +C*(F(0,-1)*GO(IP, IN)+G(0,-1)*FO(IP, IN))
      FN(IN,-L)= -FN(IP, L)
      GN(IN,-L)=  GN(IP, L)
      FN(IN, L)= -FN(IP,-L)
      GN(IN, L)=  GN(IP,-L)
C     CASE 4 : ABS(M) < L-1, M'=L, M'=-L
      SIG=-EO(L)
      DO LMM=0,I2P
         LMMP=LMM+1
         LMMM=LMM-1
         D1=CLM(-LMM)*CLM(-LMM-1)/(CLM(L)*CLM(IP))
         D2=CLM(LMM)*CLM(LMMM)/(CLM(L)*CLM(IP))
         C=SQ*CLM(LMM)*CLM(-LMM)/(CLM(L)*CLM(IP))
         FN(LMM, L)=D1*(F(-1, 1)*FO(LMMP,IP)-G(-1, 1)*GO(LMMP,IP))
     *             +D2*(F( 1, 1)*FO(LMMM,IP)-G( 1, 1)*GO(LMMM,IP))
     *             +C* (F( 0, 1)*FO(LMM, IP)-G( 0, 1)*GO(LMM, IP))
         GN(LMM, L)=D1*(F(-1, 1)*GO(LMMP,IP)+G(-1, 1)*FO(LMMP,IP))
     *             +D2*(F( 1, 1)*GO(LMMM,IP)+G( 1, 1)*FO(LMMM,IP))
     *             +C* (F( 0, 1)*GO(LMM, IP)+G( 0, 1)*FO(LMM, IP))
         FN(LMM,-L)=D1*(F(-1,-1)*FO(LMMP,IN)-G(-1,-1)*GO(LMMP,IN))
     *             +D2*(F( 1,-1)*FO(LMMM,IN)-G( 1,-1)*GO(LMMM,IN))
     *             +C* (F( 0,-1)*FO(LMM, IN)-G( 0,-1)*GO(LMM, IN))
         GN(LMM,-L)=D1*(F(-1,-1)*GO(LMMP,IN)+G(-1,-1)*FO(LMMP,IN))
     *             +D2*(F( 1,-1)*GO(LMMM,IN)+G( 1,-1)*FO(LMMM,IN))
     *             +C* (F( 0,-1)*GO(LMM, IN)+G( 0,-1)*FO(LMM, IN))
         SIG=-SIG
         FN(-LMM,-L)= SIG*FN(LMM, L)
         GN(-LMM,-L)=-SIG*GN(LMM, L)
         FN(-LMM, L)= SIG*FN(LMM,-L)
         GN(-LMM, L)=-SIG*GN(LMM,-L)
      ENDDO
C     ---------------------------------------------
C            NOW THE CASES OF ABS(M') < L
C     ---------------------------------------------
C
C     CASE 5 : M=L,-L    ABS(M') < L
      SIG=-EO(L)
      DO LMP=0,IP
        B=CLM(L)*CLM(IP)/(SQ*CLM(LMP)*CLM(-LMP))
        FN( L,LMP)=B*(F( 1,0)*FO(IP,LMP)-G( 1,0)*GO(IP,LMP))
        GN( L,LMP)=B*(F( 1,0)*GO(IP,LMP)+G( 1,0)*FO(IP,LMP))
        FN(-L,LMP)=B*(F(-1,0)*FO(IN,LMP)-G(-1,0)*GO(IN,LMP))
        GN(-L,LMP)=B*(F(-1,0)*GO(IN,LMP)+G(-1,0)*FO(IN,LMP))
        SIG=-SIG
        FN(-L,-LMP)= SIG*FN( L,LMP)
        GN(-L,-LMP)=-SIG*GN( L,LMP)
        FN( L,-LMP)= SIG*FN(-L,LMP)
        GN( L,-LMP)=-SIG*GN(-L,LMP)
      ENDDO
C
C     CASE 6 : M=L-1,-L+1    ABS(M') < L
C
      SIG=-EO(IP)
      DO LMP=0,IP
        B=CLM(IP)*CLM(I2P)/(SQ*CLM(LMP)*CLM(-LMP))
        A=CLM(IP)*CLM(IN)/(CLM(LMP)*CLM(-LMP))
        FN(IP,LMP)=B*(F( 1,0)*FO(I2P,LMP)-G( 1,0)*GO(I2P,LMP))
     *             +A*F( 0,0)*FO(IP, LMP)
        GN(IP,LMP)=B*(F( 1,0)*GO(I2P,LMP)+G( 1,0)*FO(I2P,LMP))
     *             +A*F( 0,0)*GO(IP, LMP)
        FN(IN,LMP)=B*(F(-1,0)*FO(I2N,LMP)-G(-1,0)*GO(I2N,LMP))
     *             +A*F( 0,0)*FO(IN, LMP)
        GN(IN,LMP)=B*(F(-1,0)*GO(I2N,LMP)+G(-1,0)*FO(I2N,LMP))
     *             +A*F( 0,0)*GO(IN, LMP)
        SIG=-SIG
        FN(IN,-LMP) = SIG*FN(IP,LMP)
        GN(IN,-LMP) =-SIG*GN(IP,LMP)
        FN(IP,-LMP) = SIG*FN(IN,LMP)
        GN(IP,-LMP) =-SIG*GN(IN,LMP)
      ENDDO
C
C     CASE 7 : ABS(M) < L-1,   ABS(M') < L
C
      DO LMP=IN,IP
         SIG=-EO(LMP)
         DO LMM=0,I2P
            LMMM=LMM-1
            LMMP=LMM+1
            B1=CLM(-LMM)*CLM(-LMM-1)/(SQ*CLM(LMP)*CLM(-LMP))
            B2=CLM(LMM)*CLM(LMMM)/(SQ*CLM(LMP)*CLM(-LMP))
            A =CLM(LMM)*CLM(-LMM)/(CLM(LMP)*CLM(-LMP))
            FN(LMM,LMP)=B1*(F(-1,0)*FO(LMMP,LMP)-G(-1,0)*GO(LMMP,LMP))
     *                 +B2*(F( 1,0)*FO(LMMM,LMP)-G( 1,0)*GO(LMMM,LMP))
     *                 +A*  F( 0,0)*FO(LMM, LMP)
            GN(LMM,LMP)=B1*(F(-1,0)*GO(LMMP,LMP)+G(-1,0)*FO(LMMP,LMP))
     *                 +B2*(F( 1,0)*GO(LMMM,LMP)+G( 1,0)*FO(LMMM,LMP))
     *                 +A*  F( 0,0)*GO(LMM, LMP)
            SIG=-SIG
            FN(-LMM,-LMP)= SIG*FN(LMM,LMP)
            GN(-LMM,-LMP)=-SIG*GN(LMM,LMP)
         ENDDO
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK ROTLT
      SUBROUTINE ROTLT(LFG,F,G,LNP,YLM,NYLM,LP)
C
C     THIS ROUTINE ROTATES IRREGULAR YLM INTO NYLM VIA D
C     FORWARD.
C       NYLM=YLM*D
C     WHERE D = F + I G
C
C     LP IS THE ORDER OF MULTIPOLE IN THE EXPANSION.
C
C     C. H. CHOI APRIL 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (ZERO=0.0D+00)
      COMPLEX*16 YLM, NYLM
      DIMENSION NYLM(0:LNP-1),YLM(0:LNP-1),
     *          F(LFG),G(LFG)
C
      DO L=0,LP
         ID=(L-1)*L*(L+1)*4/3+(L+1)+L*(2*L+1)
         IYLM=L*(L+1)/2
         DO M=0,L
            IMD=ID+M*(2*L+1)
            LM=IYLM+M
C
            NYLM(LM)=ZERO
            KM=IMD+L
            NYLM(LM)=NYLM(LM)+DCMPLX(F(KM),G(KM))*YLM(IYLM)
            SIG=1.0D+00
            DO K=1,L
               KM=IMD+K+L
               KMN=IMD-K+L
               LK=IYLM+K
               SIG=-SIG
               NYLM(LM)=NYLM(LM)+SIG*DCMPLX(F(KMN),G(KMN))*
     *               DCONJG(YLM(LK))
               NYLM(LM)=NYLM(LM)+DCMPLX(F(KM),G(KM))*YLM(LK)
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK ROTMM
      SUBROUTINE ROTMM(LFG,F,G,LNP,YLM,NYLM,LP)
C
C     THIS ROUTINE ROTATES COMPLEX CONJUGATD OF REGULAR
C     YLM INTO NYLM VIA D FORWARD.
C       NYLM=YLM*D
C     WHERE D = F + I G
C
C     LP IS THE ORDER OF MULTIPOLE IN THE EXPANSION.
C
C     C. H. CHOI APRIL 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (ZERO=0.0D+00)
      COMPLEX*16 YLM, NYLM
C       DIMENSION NYLM(0:(LNP-1)*2),YLM(0:(LNP-1)*2),
      DIMENSION NYLM(0:LNP-1),YLM(0:LNP-1),
     *          F(LFG),G(LFG)
C
      DO L=0,LP
         ID=(L-1)*L*(L+1)*4/3+(L+1)+L*(2*L+1)
         IYLM=L*(L+1)/2
         DO M=0,L
            IMD=ID+M*(2*L+1)
            LM=(IYLM+M)
C
            NYLM(LM)=ZERO
            KM=IMD+L
            NYLM(LM)=NYLM(LM)+DCMPLX(F(KM),-G(KM))*YLM(IYLM)
            SIG=1.0D+00
            DO K=1,L
               KM=IMD+K+L
               KMN=IMD+L-K
               LK=(IYLM+K)
               SIG=-SIG
               NYLM(LM)=NYLM(LM)+SIG*DCMPLX(F(KMN),-G(KMN))*
     *               DCONJG(YLM(LK))
               NYLM(LM)=NYLM(LM)+DCMPLX(F(KM),-G(KM))*YLM(LK)
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK ROTR
      SUBROUTINE ROTR(A,B,R)
C
C     THIS ROUTINE RETURNS THE 3X3 COORDINATE ROTATION MATRIX R WHICH
C     ROTATE A TO B, WHERE A AND B ARE ROW VECTORS.
C     B=A*R
C
C     C. H. CHOI MARCH 1999
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO=0.0D+00,ONE=1.0D+00,PI=3.141592653589793238D+00,
     *           EPS=1.0D-14)
      DIMENSION A(3), B(3), R(3,3)
C     CALCULATE THE ANGLE BETWEEN A AND B.
      AN=SQRT(A(1)*A(1)+A(2)*A(2)+A(3)*A(3))
      BN=SQRT(B(1)*B(1)+B(2)*B(2)+B(3)*B(3))
      DOT=A(1)*B(1)+A(2)*B(2)+A(3)*B(3)
      ANG=ACOS(DOT/(AN*BN))
      IF (ANG.EQ.ZERO) THEN
         DO I=1,3
            DO J=1,I
               R(I,J)=ZERO
               R(J,I)=ZERO
               IF (I.EQ.J) THEN
                  R(I,J)=ONE
               ENDIF
            ENDDO
         ENDDO
      ELSEIF (ABS(ABS(ANG)-PI).LE.EPS) THEN
         DO I=1,3
            DO J=1,I
               R(I,J)=ZERO
               R(J,I)=ZERO
               IF (I.EQ.J) THEN
                  R(I,J)=-ONE
               ENDIF
            ENDDO
         ENDDO
      ELSE
         CW=COS(ANG)
         SW=SIN(ANG)
         CWN=1.0D+00-CW
C        CALCULATE THE CROSS PRODUCT BETWEEN A AND B.
         X=B(2)*A(3)-B(3)*A(2)
         Y=B(3)*A(1)-B(1)*A(3)
         Z=B(1)*A(2)-B(2)*A(1)
C        NORMALIZE THE CROSS PRODUCT.
         CN=SQRT(X*X+Y*Y+Z*Z)
         X=X/CN
         Y=Y/CN
         Z=Z/CN
C        CALCULATE R.
         R(1,1)=CWN*X*X+CW
         R(2,1)=CWN*X*Y+Z*SW
         R(3,1)=CWN*X*Z-Y*SW
         R(1,2)=CWN*X*Y-Z*SW
         R(2,2)=CWN*Y*Y+CW
         R(3,2)=CWN*Y*Z+X*SW
         R(1,3)=CWN*X*Z+Y*SW
         R(2,3)=CWN*Y*Z-X*SW
         R(3,3)=CWN*Z*Z+CW
      ENDIF
C
      RETURN
      END
C*MODULE QFMM    *DECK RY2YIR
      SUBROUTINE RY2YIR(LNP,CMEX,TCMEX,TMPX,LFG,F,G,NP,
     *                  COEFF)
C
C     THIS ROUTINE RETURNS THE TRANSLATED MOMENTS OF
C     EXTERNAL EXPANSION (AKA REGULAR SPHERICAL HARMONICS)
C     ABOUT A NEW ORIGIN TRANSLATED
C     BY T ALONG ANY AXIS.
C     NP IS THE NUMBER OF TERMS IN THE EXPANSION.
C     C.H.C CALLS IT Y TO Y TRANSLATION.
C
C     C. H. CHOI APRIL 1999
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMPLEX*16 CMEX, TCMEX, TMPX
      PARAMETER (MAXCOF=23821)
      DIMENSION CMEX(LNP),TCMEX(LNP),F(LFG),G(LFG),
     *          TMPX(LNP),COEFF(MAXCOF)
C
      DO I=1,LNP
         TMPX(I)=CMEX(I)
      ENDDO
      CALL ROTMM(LFG,F,G,LNP,TMPX,TCMEX,NP)
      CALL ZTRNSM(LNP,TCMEX,TMPX,NP,COEFF)
      CALL IROTMM(LFG,F,G,LNP,TMPX,TCMEX,NP)
C
      RETURN
      END
C*MODULE QFMM    *DECK RY2ZIR
      SUBROUTINE RY2ZIR(LNP,CMEX,TCMEX,TMPX,LFG,F,G,NP,
     *           COEFF)
C
C     THIS ROUTINE RETURNS THE TRANSLATED MOMENTS OF
C     EXTERNAL EXPANSION (AKA REGULAR SPHERICAL HARMONICS)
C     ABOUT A NEW ORIGIN TRANSLATED
C     BY T ALONG ANY AXIS.
C     NP IS THE NUMBER OF TERMS IN THE EXPANSION.
C     C.H.C CALLS IT Y TO Y TRANSLATION.
C
C     C. H. CHOI JUNE 1999
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMPLEX*16 CMEX, TCMEX, TMPX
      PARAMETER (MAXCOF=23821)
      DIMENSION CMEX(LNP),TCMEX(LNP),F(LFG),G(LFG),
     *          TMPX(LNP),COEFF(MAXCOF,2)
C
      DO I=1,LNP
         TMPX(I)=CMEX(I)
      ENDDO
      CALL ROTMM(LFG,F,G,LNP,TMPX,TCMEX,NP)
      CALL ZTRNSML(LNP,TCMEX,TMPX,NP,COEFF)
      CALL IROTLT(LFG,F,G,LNP,TMPX,TCMEX,NP)
C
C
      RETURN
      END
C*MODULE QFMM    *DECK RZ2ZIR
      SUBROUTINE RZ2ZIR(LNP,CMEX,TCMEX,TMPX,LFG,F,G,NP,FLM,ZLL)
C
C     C.H.C CALLS IT Z TO Z TRANSLATION VIA ROTATION.
C
C     C. H. CHOI JUNE 1999
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMPLEX*16 CMEX, TCMEX, TMPX
      DIMENSION CMEX(LNP),TCMEX(LNP),F(LFG),FLM(-NP:NP),G(LFG),
     *          ZLL(0:NP),TMPX(LNP)
C
      DO I=1,LNP
         TMPX(I)=CMEX(I)
      ENDDO
      CALL ROTLT(LFG,F,G,LNP,TMPX,TCMEX,NP)
      CALL ZTRNSLL(LNP,TCMEX,TMPX,NP,FLM,ZLL)
      CALL IROTLT(LFG,F,G,LNP,TMPX,TCMEX,NP)
C
      RETURN
      END
C*MODULE QFMM    *DECK DSORT5
      SUBROUTINE DSORT5(N,RA,RB,RC,IRD,IWS)
C
C     HEAP SORTING-TAKEN FROM NUMERICAL RECIPES.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION RA(N), RB(N), RC(N), IRD(N),IWS(N)
C
      IF (N.EQ.1) RETURN
      L=N/2+1
      IR=N
 10   CONTINUE
      IF (L.GT.1) THEN
         L=L-1
         RRA=RA(L)
         RRB=RB(L)
         RRC=RC(L)
         IRRD=IRD(L)
         IRWS=IWS(L)
      ELSE
         RRA=RA(IR)
         RRB=RB(IR)
         RRC=RC(IR)
         IRRD=IRD(IR)
         IRWS=IWS(IR)
         RA(IR)=RA(1)
         RB(IR)=RB(1)
         RC(IR)=RC(1)
         IRD(IR)=IRD(1)
         IWS(IR)=IWS(1)
         IR=IR-1
         IF (IR.EQ.1) THEN
            RA(1)=RRA
            RB(1)=RRB
            RC(1)=RRC
            IRD(1)=IRRD
            IWS(1)=IRWS
            RETURN
         ENDIF
      ENDIF
      I=L
      J=L+L
 20   IF (J.LE.IR) THEN
         IF (J.LT.IR) THEN
            IF (RA(J).LT.RA(J+1)) J=J+1
         ENDIF
         IF (RRA.LT.RA(J)) THEN
            RA(I)=RA(J)
            RB(I)=RB(J)
            RC(I)=RC(J)
            IRD(I)=IRD(J)
            IWS(I)=IWS(J)
            I=J
            J=J+J
         ELSE
            J=IR+1
         ENDIF
      GOTO 20
      ENDIF
      RA(I)=RRA
      RB(I)=RRB
      RC(I)=RRC
      IRD(I)=IRRD
      IWS(I)=IRWS
      GOTO 10
C
      END
C*MODULE QFMM    *DECK ISORT5
      SUBROUTINE ISORT5(N,IRA,RB,RC,RD,IWS)
C
C     HEAP SORTING-TAKEN FROM NUMERICAL RECIPES.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IRA(N), RB(N), RC(N), RD(N),IWS(N)
C
      IF (N.EQ.1) RETURN
      L=N/2+1
      IR=N
 10   CONTINUE
      IF (L.GT.1) THEN
         L=L-1
         IRRA=IRA(L)
         RRB=RB(L)
         RRC=RC(L)
         RRD=RD(L)
         IRWS=IWS(L)
      ELSE
         IRRA=IRA(IR)
         RRB=RB(IR)
         RRC=RC(IR)
         RRD=RD(IR)
         IRWS=IWS(IR)
         IRA(IR)=IRA(1)
         RB(IR)=RB(1)
         RC(IR)=RC(1)
         RD(IR)=RD(1)
         IWS(IR)=IWS(1)
         IR=IR-1
         IF (IR.EQ.1) THEN
            IRA(1)=IRRA
            RB(1)=RRB
            RC(1)=RRC
            RD(1)=RRD
            IWS(1)=IRWS
            RETURN
         ENDIF
      ENDIF
      I=L
      J=L+L
 20   IF (J.LE.IR) THEN
         IF (J.LT.IR) THEN
            IF (IRA(J).LT.IRA(J+1)) J=J+1
         ENDIF
         IF (IRRA.LT.IRA(J)) THEN
            IRA(I)=IRA(J)
            RB(I)=RB(J)
            RC(I)=RC(J)
            RD(I)=RD(J)
            IWS(I)=IWS(J)
            I=J
            J=J+J
         ELSE
            J=IR+1
         ENDIF
      GOTO 20
      ENDIF
      IRA(I)=IRRA
      RB(I)=RRB
      RC(I)=RRC
      RD(I)=RRD
      IWS(I)=IRWS
      GOTO 10
C
      END
C*MODULE QFMM    *DECK ZTRNSLL
      SUBROUTINE ZTRNSLL(LNP,CMEX,TCMEX,NP,FLM,ZLL)
C
C     Z(R) TO Z(R-B) TRANSLATION.
C     IT TRANSLATES A TAYLOR EXPANSION OF A POINT R ABOUT THE
C     ORIGIN TO AN EXPANSION OF R ABOUT A POINT B.
C
C     C. H. CHOI JUNE 1999
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO=0.0D+00)
      COMPLEX*16 CMEX, TCMEX
      DIMENSION CMEX(0:LNP-1),TCMEX(0:LNP-1),
     *          ZLL(0:2*NP+1),FLM(0:2*NP)
C
      DO L=0,NP
         IDXL=L*(L+1)/2
         DO M=0,L
            IDXA=IDXL+M
            TCMEX(IDXA)=ZERO
            DO LK=L,NP
               IDXB=LK*(LK+1)/2+M
               TMP=FLM(LK-M)*FLM(LK+M)
     *             *ZLL(LK-L)/(FLM(LK-L)*FLM(LK-L))
               TCMEX(IDXA)=TCMEX(IDXA)+TMP*CMEX(IDXB)
            ENDDO
            TCMEX(IDXA)=TCMEX(IDXA)/(FLM(L+M)*FLM(L-M))
         ENDDO
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK ISORT2
      SUBROUTINE ISORT2(N,IRA,IRB)
C
C     HEAP SORTING-TAKEN FROM NUMERICAL RECIPES.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IRA(N),IRB(N)
C
      IF (N.EQ.1) RETURN
      L=N/2+1
      IR=N
 10   CONTINUE
      IF (L.GT.1) THEN
         L=L-1
         IRRA=IRA(L)
         IRRB=IRB(L)
      ELSE
         IRRA=IRA(IR)
         IRRB=IRB(IR)
         IRA(IR)=IRA(1)
         IRB(IR)=IRB(1)
         IR=IR-1
         IF (IR.EQ.1) THEN
            IRA(1)=IRRA
            IRB(1)=IRRB
            RETURN
         ENDIF
      ENDIF
      I=L
      J=L+L
 20   IF (J.LE.IR) THEN
         IF (J.LT.IR) THEN
            IF (IRA(J).LT.IRA(J+1)) J=J+1
         ENDIF
         IF (IRRA.LT.IRA(J)) THEN
            IRA(I)=IRA(J)
            IRB(I)=IRB(J)
            I=J
            J=J+J
         ELSE
            J=IR+1
         ENDIF
      GOTO 20
      ENDIF
      IRA(I)=IRRA
      IRB(I)=IRRB
      GOTO 10
C
      END
C*MODULE QFMM    *DECK DSORT
      SUBROUTINE DSORT(N,RA,IRB)
C
C     HEAP SORTING-TAKEN FROM NUMERICAL RECIPES.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION RA(N),IRB(N)
C
      IF (N.EQ.1) RETURN
      L=N/2+1
      IR=N
 10   CONTINUE
      IF (L.GT.1) THEN
         L=L-1
         RRA=RA(L)
         IRRB=IRB(L)
      ELSE
         RRA=RA(IR)
         IRRB=IRB(IR)
         RA(IR)=RA(1)
         IRB(IR)=IRB(1)
         IR=IR-1
         IF (IR.EQ.1) THEN
            RA(1)=RRA
            IRB(1)=IRRB
            RETURN
         ENDIF
      ENDIF
      I=L
      J=L+L
 20   IF (J.LE.IR) THEN
         IF (J.LT.IR) THEN
            IF (RA(J).LT.RA(J+1)) J=J+1
         ENDIF
         IF (RRA.LT.RA(J)) THEN
            RA(I)=RA(J)
            IRB(I)=IRB(J)
            I=J
            J=J+J
         ELSE
            J=IR+1
         ENDIF
      GOTO 20
      ENDIF
      RA(I)=RRA
      IRB(I)=IRRB
      GOTO 10
C
      END
C*MODULE QFMM    *DECK ISORT4
      SUBROUTINE ISORT4(NMAX,N,IRA,IRB,IRC,IRD)
C
C     HEAP SORTING-TAKEN FROM NUMERICAL RECIPES.
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IRA(NMAX),IRB(NMAX),IRC(NMAX),IRD(NMAX)
C
      IF (N.EQ.1) RETURN
      L=N/2+1
      IR=N
 10   CONTINUE
      IF (L.GT.1) THEN
         L=L-1
         IRRA=IRA(L)
         IRRB=IRB(L)
         IRRC=IRC(L)
         IRRD=IRD(L)
      ELSE
         IRRA=IRA(IR)
         IRRB=IRB(IR)
         IRRC=IRC(IR)
         IRRD=IRD(IR)
         IRA(IR)=IRA(1)
         IRB(IR)=IRB(1)
         IRC(IR)=IRC(1)
         IRD(IR)=IRD(1)
         IR=IR-1
         IF (IR.EQ.1) THEN
            IRA(1)=IRRA
            IRB(1)=IRRB
            IRC(1)=IRRC
            IRD(1)=IRRD
            RETURN
         ENDIF
      ENDIF
      I=L
      J=L+L
 20   IF (J.LE.IR) THEN
         IF (J.LT.IR) THEN
            IF (IRA(J).LT.IRA(J+1)) J=J+1
         ENDIF
         IF (IRRA.LT.IRA(J)) THEN
            IRA(I)=IRA(J)
            IRB(I)=IRB(J)
            IRC(I)=IRC(J)
            IRD(I)=IRD(J)
            I=J
            J=J+J
         ELSE
            J=IR+1
         ENDIF
      GOTO 20
      ENDIF
      IRA(I)=IRRA
      IRB(I)=IRRB
      IRC(I)=IRRC
      IRD(I)=IRRD
      GOTO 10
C
      END
C*MODULE QFMM    *DECK ZTRNSM
      SUBROUTINE ZTRNSM(LNP,CMEX,TCMEX,NP,COEFF)
C
C     THIS ROUTINE RETURNS THE TRANSLATED MOMENTS OF
C     EXTERNAL EXPANSION (AKA REGULAR SPHERICAL HARMONICS)
C     ABOUT A NEW ORIGIN TRANSLATED
C     BY Z ALONG THE Z-AXIS.
C     NP IS THE NUMBER OF TERMS IN THE EXPANSION.
C
C     C. H. CHOI APRIL 1999
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO=0.0D+00,MAXCOF=23821)
      COMPLEX*16 CMEX, TCMEX
      DIMENSION CMEX(0:LNP-1),TCMEX(0:LNP-1),
     *          COEFF(MAXCOF)
C
      LCOF=0
      DO L=0,NP
         IDXL=L*(L+1)/2
         DO M=0,L
            IDXA=IDXL+M
            TCMEX(IDXA)=ZERO
            DO LK=M,L
               IDXB=LK*(LK+1)/2+M
               LCOF=LCOF+1
               TCMEX(IDXA)=TCMEX(IDXA)+COEFF(LCOF)*CMEX(IDXB)
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK RBS
      SUBROUTINE RBS(NTBOX,IDXBOX,NSSBOX,NS,NSS,
     *           MINX,MAXX,MINY,MAXY,MINZ,MAXZ,MAX,MAXI,MAXJ,MAXK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION IDXBOX(3,NTBOX),NSSBOX(20)
C
      IDX=NS-NSS+1
C
C     REMOVE BLANK SPACE.
C
      IF (NSS.EQ.NS) THEN
         IBOXS=1
         IBOXE=NSSBOX(IDX)
      ELSE
         IBOXS=NSSBOX(IDX-1)+1
         IBOXE=NSSBOX(IDX)
      ENDIF
      MAXSIZE=2**NSS
      MINX=MAXSIZE
      MAXX=1
      MINY=MAXSIZE
      MAXY=1
      MINZ=MAXSIZE
      MAXZ=1
C
      DO I=IBOXS,IBOXE
         IX=IDXBOX(1,I)
         IY=IDXBOX(2,I)
         IZ=IDXBOX(3,I)
         IF (IX.LT.MINX) MINX=IX
         IF (IX.GT.MAXX) MAXX=IX
         IF (IY.LT.MINY) MINY=IY
         IF (IY.GT.MAXY) MAXY=IY
         IF (IZ.LT.MINZ) MINZ=IZ
         IF (IZ.GT.MAXZ) MAXZ=IZ
      ENDDO
      MAXI=MAX
      MAXJ=MAX
      MAXK=MAX
      IF ( ABS(MAXX-MINX).LT.MAXI ) THEN
         MAXI=ABS(MAXX-MINX)
      ENDIF
      IF ( ABS(MAXY-MINY).LT.MAXJ ) THEN
         MAXJ=ABS(MAXY-MINY)
      ENDIF
      IF ( ABS(MAXZ-MINZ).LT.MAXK ) THEN
         MAXK=ABS(MAXZ-MINZ)
      ENDIF
C
      RETURN
      END
C*MODULE QFMM    *DECK GETIJK
      SUBROUTINE GETIJK(I,J,K,IX,IY,IZ,NSS2,ISP,JSP,KSP,
     *           MAX,MINX,MAXX,MINY,MAXY,MINZ,MAXZ,IST,JST,KST,
     *           IND,JND,KND)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      ISP=1
      JSP=1
      KSP=1
C
C     IN THE CASE OF IX.EQ.MAX AND POSITIVE TRANSLATION,
C     ONLY ODD NUMBER OF IST IS ALLOWED.
C     IF IT IS NEGATIVE TRANSLATION, ONLY EVEN NUMBER OF IST IS ALLOWED.
C     SO BY SETTING ISP=2, ONLY EVEN OR ODD NUMBERS ARE USED.
C
      IF (IX.EQ.MAX) ISP=2
      IF (IY.EQ.MAX) JSP=2
      IF (IZ.EQ.MAX) KSP=2
      IF (I.GE.0) THEN
         IST=1
         IND=NSS2-I
         IF (IST.LT.MINX) IST=MINX
         IF (IND.GT.MAXX) IND=MAXX
         IF ((ISP.EQ.2).AND.(EO(IST).EQ.1)) IST=IST+1
      ELSEIF (I.LT.0) THEN
         IST=1-I
         IND=NSS2
         IF (IST.LT.MINX) IST=MINX
         IF (IND.GT.MAXX) IND=MAXX
         IF ((ISP.EQ.2).AND.(EO(IST).EQ.-1)) IST=IST+1
      ENDIF
      IF (J.GE.0) THEN
         JST=1
         JND=NSS2-J
         IF (JST.LT.MINY) JST=MINY
         IF (JND.GT.MAXY) JND=MAXY
         IF ((JSP.EQ.2).AND.(EO(JST).EQ.1)) JST=JST+1
      ELSEIF (J.LT.0) THEN
         JST=1-J
         JND=NSS2
         IF (JST.LT.MINY) JST=MINY
         IF (JND.GT.MAXY) JND=MAXY
         IF ((JSP.EQ.2).AND.(EO(JST).EQ.-1)) JST=JST+1
      ENDIF
      IF (K.GE.0) THEN
         KST=1
         KND=NSS2-K
         IF (KST.LT.MINZ) KST=MINZ
         IF (KND.GT.MAXZ) KND=MAXZ
         IF ((KSP.EQ.2).AND.(EO(KST).EQ.1)) KST=KST+1
      ELSEIF (K.LT.0) THEN
         KST=1-K
         KND=NSS2
         IF (KST.LT.MINZ) KST=MINZ
         IF (KND.GT.MAXZ) KND=MAXZ
         IF ((KSP.EQ.2).AND.(EO(KST).EQ.-1)) KST=KST+1
      ENDIF
C
      RETURN
      END
C*MODULE QFMM    *DECK SP2P
      SUBROUTINE SP2P(IYZTBL,F,G,CLM,FLM,ZLL
     *                 ,NZ,YP,ZP,NTBOX,IDXBOX,NSBOX,
     *                 MAXWS,IYZPNT,IP2P)
C     SERIAL VERSION OF P2P
C     SIZE : THE SIDE LENGTH OF A CUBE CONTAINING ALL THE POINTS.
C     NS   : THE NUMBER OF HIGHEST SUBDIVISION LEVEL
C     EPS  : ERROR TOLERANCE BETWEEN EXACT AND FMM RESULTS
C     IYZTBL : CONTAINS THE ACTUAL ADDRESS OF Y AND Z IN THE YP AND
C              ZP ARRAY
C     NP   : THE HIGHEST ORDER OF L QUANTUM NUMBER
C     F, G : D=F+I G
C     CLM, FLM, ZLL : PRE-COMPUTED COEFFICIENTS
C     NZ   : TOTAL NUMBER OF NON-EMPTY BOXES
C     YP   : CONTAINS THE REGULAR HARMONICS
C     ZP   : CONTAINTS THE IRREGULAR HARMONICS
C     IP2P : RETURNS THE TOTAL NUMBER OF P2P TRANSLATIONS
C
C     C. H. CHOI AUGUST 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (ZERO=0.0D+00,MAXNP=50,MAXCOF=23821)
      LOGICAL QOPS,QFMM
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMPLEX*16 CZP, PZP, YP, ZP
      COMMON /QMFM  /SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *       ITERMS,QOPS,ISCUT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DIMENSION IDXBOX(3,NTBOX),NSBOX(20),
     *          NSSBOX(20),IYZPNT(NTBOX,MAXWS/2)
      DIMENSION IYZTBL(2**(NS+1)-1,2**(NS+1)-1,2**(NS+1)-1),
     *          F((NP*(NP+1)*(NP+2)*4/3)+(NP+1),8),
     *          G((NP*(NP+1)*(NP+2)*4/3)+(NP+1),8),
     *          CLM(-NP:NP),T(4),FLM(-NP:NP),COEFF(MAXCOF,2),
     *          ZLL(0:2*NP+1,2),YP((NP+1)*(NP+2)/2,NZ),
     *          ZP((NP+1)*(NP+2)/2,NZ),CZP((MAXNP+1)*(MAXNP+2)/2),
     *          PZP((MAXNP+1)*(MAXNP+2)/2)
C
C
C     PARALLEL INITIALIZATION
C
      NEXT = -1
      MINE = -1
      LNP=(NP+1)*(NP+2)/2
C
C     INITIALIZATION ZP WITH ZEROS.
C
      DO I=1,LNP
         DO J=1,NZ
            ZP(I,J)=ZERO
         ENDDO
      ENDDO
C
      NSSBOX(1)=NSBOX(1)
      DO I=1,NS-1
         NSSBOX(I+1)=NSSBOX(I)+NSBOX(I+1)
      ENDDO
C
      IP2P=0
C--------------------------------------------------------C
C                    THE MAIN LOOP                       C
C--------------------------------------------------------C
C      WRITE(IW,9000)
      DO NSS=2,NS
         NSS2=2**NSS
         IPST=2**(NS+1)-2**(NSS+1)
         UNIT=SIZE/NSS2
         IBRNCH=MAXWS/2
         IF (IBRNCH.GT.NSS2/2) IBRNCH=NSS2/2
C
         DO ISET=1,IBRNCH
            IREFWS=ISET*2
            DO IS2S=1,ISET
               JREFWS=IS2S*2
               IJWS=(IREFWS+JREFWS)/2
               MAX=( (ISET+1)/2+(IS2S+1)/2 )*2+1
               MIN=ISET+IS2S
         IF (NSS2 .LT. MIN) GOTO 100
C
C        GET MINX ~ MAXZ
C
         CALL RBS(NTBOX,IDXBOX,NSSBOX,NS,NSS,
     *        MINX,MAXX,MINY,MAXY,MINZ,MAXZ,MAX,MAXI,MAXJ,MAXK)
C
C        MAXI, MAXJ, MAXK ARE THE MAXIMUM COMPONENTS OF TRANSLATION
C        VECTOR, SO (I,J,K) FORMS THE REAL TRANSLATION VECTOR.
C
         DO I=0,MAXI
            IX=ABS(I)
            IX2=I*NSS2*NSS2
            T(1)=-I*UNIT
            DO J=-MAXJ,MAXJ
            IF (GOPARR) THEN
               MINE=MINE+1
               IF (MINE.GT.NEXT) CALL DDI_DLBNEXT(NEXT)
               IF (NEXT.NE.MINE) GOTO 200
            ENDIF
               IY=ABS(J)
               IY2=J*NSS2
               T(2)=-J*UNIT
               DO K=-MAXK,MAXK
                  IZ=ABS(K)
                  IZ2=K
                  T(3)=-K*UNIT
C
C                FIGURING OUT THE INTERACTION LIST.
C                AT LEAST ONE COMPONENT OF TRANSLATION VECTOR
C                MUST BE GREATER THAN IWS.
C
                  IF ((IX.GT.IJWS).OR.(IY.GT.IJWS).OR.(IZ.GT.IJWS)) THEN
                     IF ((IX2+IY2+IZ2).GT.ZERO) THEN
                     CALL GETIJK(I,J,K,IX,IY,IZ,NSS2,ISP,JSP,KSP,
     *               MAX,MINX,MAXX,MINY,MAXY,MINZ,MAXZ,IST,JST,KST,
     *               IND,JND,KND)
                     CALL PRECOM(T,IX,IY,IZ,EPS,UNIT,NP,NPS,LNPS,LFGS,
     *                    ZLL,CLM,FLM,COEFF,F,G)
C
                     DO II=IST,IND,ISP
                        DO JJ=JST,JND,JSP
                           DO KK=KST,KND,KSP
                              IP=II+IPST
                              JP=JJ+IPST
                              KP=KK+IPST
                              IC=IP+I
                              JC=JP+J
                              KC=KP+K
                              NONST=IYZTBL(IP,JP,KP)
                              NONEND=IYZTBL(IC,JC,KC)
                              IF (NONST*NONEND.GT.0) THEN
                                 IYPST=IYZPNT(NONST,ISET)
                                 IYPEND=IYZPNT(NONEND,IS2S)
                                 IF (IYPST.NE.0) THEN
                                    IF (IYPEND.NE.0) THEN
                                       CALL RY2ZIR(LNPS,YP(1,IYPEND)
     *                                     ,PZP,CZP,LFGS,
     *                                     F,G,NPS,
     *                                     COEFF(1,1))
                                       DO L=1,LNPS
                                          ZP(L,IYPST)=
     *                                       ZP(L,IYPST)+PZP(L)
                                       ENDDO
                                       CALL RY2ZIR(LNPS,YP(1,IYPST)
     *                                     ,PZP,CZP,LFGS,
     *                                     F,G,NPS,
     *                                     COEFF(1,2))
                                       DO L=1,LNPS
                                          ZP(L,IYPEND)=
     *                                       ZP(L,IYPEND)+PZP(L)
                                       ENDDO
                                       IP2P=IP2P+2
                                    ENDIF
                                 ENDIF
                                 IF (ISET.NE.IS2S) THEN
                                    IYPST=IYZPNT(NONST,IS2S)
                                    IYPEND=IYZPNT(NONEND,ISET)
                                    IF (IYPST.NE.0) THEN
                                       IF (IYPEND.NE.0) THEN
                                          CALL RY2ZIR(LNPS,YP(1,IYPEND)
     *                                       ,PZP,CZP,LFGS,
     *                                       F,G,NPS,
     *                                       COEFF(1,1))
                                          DO L=1,LNPS
                                             ZP(L,IYPST)=
     *                                          ZP(L,IYPST)+PZP(L)
                                          ENDDO
                                          CALL RY2ZIR(LNPS,YP(1,IYPST)
     *                                       ,PZP,CZP,LFGS,
     *                                        F,G,NPS,
     *                                        COEFF(1,2))
                                          DO L=1,LNPS
                                              ZP(L,IYPEND)=
     *                                        ZP(L,IYPEND)+PZP(L)
                                          ENDDO
                                          IP2P=IP2P+2
                                       ENDIF
                                    ENDIF
                                 ENDIF
                             ENDIF
                           ENDDO
                        ENDDO
                     ENDDO
C
                   ENDIF
                ENDIF
               ENDDO
 200        ENDDO
         ENDDO
         ENDDO
         ENDDO
 100  ENDDO
C
      RETURN
 9000 FORMAT(/10X,18("-")/10X,"P TO P TRANSLATION"/10X,18("-"))
      END
C*MODULE QFMM    *DECK PRECOM
      SUBROUTINE PRECOM(T,IX,IY,IZ,EPS,UNIT,NP,NPS,LNPS,LFGS,ZLL,
     *                 CLM,FLM,COEFF,F,G)
C     COMPUTES FACTORS NEEDED IN SUBSEQUENT CALCULATIONS.
C     EPS  : ERROR TOLERANCE BETWEEN EXACT AND FMM RESULTS
C     NP  : THE HIGHEST ORDER OF L QUANTUM NUMBER
C     F, G : D=F+I G
C     CLM, FLM, ZLL : PRE-COMPUTED COEFFICIENTS
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (HALF=0.5D+00,MAXCOF=23821)
      DIMENSION F((NP*(NP+1)*(NP+2)*4/3)+(NP+1),8),
     *          G((NP*(NP+1)*(NP+2)*4/3)+(NP+1),8),
     *          CLM(-NP:NP),T(4),
     *          FLM(-NP:NP),
     *          COEFF(MAXCOF,2),
     *          EZ(3),
     *          RT(3,3),
     *          ZLL(0:2*NP+1,2)
C
      EZ(1)=0.0D+00
      EZ(2)=0.0D+00
      EZ(3)=1.0D+00
      TY2Z=SQRT(T(1)*T(1)+T(2)*T(2)+T(3)*T(3))
      TR=IX*IX + IY*IY + IZ*IZ
      TR=SQRT(TR)
C
C     USE VERY FAST MULTIPOLE IDEA
      NPS=IGETNP(EPS,TR,UNIT)
      IF (NPS.GT.NP) NPS=NP
      LNPS=INT((NPS+1)*(NPS+2)*HALF)
      LFGS=(NPS*(NPS+1)*(NPS+2)*4/3)+(NPS+1)
      CALL GETZLL(ZLL,TY2Z,2*NPS+1)
      CALL GETCOF(NPS,FLM,ZLL,COEFF)
      CALL ROTR(T,EZ,RT)
      CALL GETROT(F,G,RT,NPS,CLM)
C
      RETURN
      END
C
C*MODULE QFMM    *DECK ZTRNSML
      SUBROUTINE ZTRNSML(LNP,CMEX,TCMEX,NP,COEFF)
C
C     Y TO Z TRANSLATION.
C
C     C. H. CHOI JUNE 1999
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO=0.0D+00,HALF=0.5D+00,MAXCOF=23821)
      DIMENSION CMEX(0:(LNP-1)*2),TCMEX(0:(LNP-1)*2),
     *          COEFF(MAXCOF,2)
C
      LCOF=0
      DO L=0,NP
         IDXL=INT(L*(L+1)*HALF)
         DO M=0,L
            IDXA=(IDXL+M)*2
            TCMEX(IDXA)=ZERO
            TCMEX(IDXA+1)=ZERO
            DO LK=M,NP
               IDXB=INT((LK*(LK+1)*HALF+M)*2)
               LCOF=LCOF+1
                TCMEX(IDXA)=TCMEX(IDXA)+COEFF(LCOF,1)*CMEX(IDXB)
                TCMEX(IDXA+1)=TCMEX(IDXA+1)-COEFF(LCOF,1)*CMEX(IDXB+1)
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK INITIDX
      SUBROUTINE INITIDX(M,INDX)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION INDX(M)
C
      DO I=1,M
         INDX(I)=I
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK GETNBOX
      SUBROUTINE GETNBOX(M,A,INDX,IDXWS,IPNTR,NBOX)
C
C     THIS ROUTINE RETURNS REORDERED A.
C     IT ALSO RETURNS TOTAL NUMBER OF NON-EMPTY BOX, NBOX.
C
C     THIS ROUTINE UTILIZES HEAP-SORT ALGOTITHM WHICH SCALES
C     M LOG2 M.
C
C     C. H. CHOI MAY 1999
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL QOPS,QFMM,FLAG
      COMMON /QMFM  /SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *       ITERMS,QOPS,ISCUT
      DIMENSION A(M,3),INDX(M),IDXWS(M),IPNTR(2**NS,3)
C
      NS2=2**NS
      DO I=1,2**NS
         IPNTR(I,1)=0
      ENDDO
C
      NBOX=0
C
      CALL DSORT5(M,A(1,1),A(1,2),A(1,3),INDX,IDXWS)
C
C     SORT W.R.T. X
C
      UNIT=SIZE/NS2
      BASE=UNIT-SIZE/2
C
      IX=1
      NX=1
      FLAG=.TRUE.
      DO WHILE (FLAG)
         IF (A(IX,1).LE.BASE) THEN
            IPNTR(NX,1)=IPNTR(NX,1)+1
         ELSE
            BASE=BASE+UNIT
            NX=NX+1
            IX=IX-1
            IF (NX.GT.NS2) FLAG=.FALSE.
         ENDIF
         IX=IX+1
         IF (IX.GT.M) FLAG=.FALSE.
      ENDDO
C
C     SORT W.R.T. Y
C
      ISTY=1
      DO NX=1,NS2
         DO I=1,2**NS
            IPNTR(I,2)=0
         ENDDO
         IF (IPNTR(NX,1).GT.0) THEN
            CALL DSORT5(IPNTR(NX,1),A(ISTY,2),A(ISTY,1),A(ISTY,3),
     *                 INDX(ISTY),IDXWS(ISTY))
            BASE=UNIT-SIZE/2
            IY=ISTY
            NY=1
            FLAG=.TRUE.
C
            DO WHILE (FLAG)
               IF (A(IY,2).LE.BASE) THEN
                  IPNTR(NY,2)=IPNTR(NY,2)+1
               ELSE
                  BASE=BASE+UNIT
                  NY=NY+1
                  IY=IY-1
                  IF (NY.GT.NS2) FLAG=.FALSE.
               ENDIF
               IY=IY+1
               IF (IY-ISTY+1.GT.IPNTR(NX,1)) FLAG=.FALSE.
            ENDDO
C
C            SORT W.R.T. Z
C
            ISTZ=ISTY
            DO NY=1,NS2
               DO I=1,2**NS
                  IPNTR(I,3)=0
               ENDDO
               IF (IPNTR(NY,2).GT.0) THEN
                  CALL DSORT5(IPNTR(NY,2),A(ISTZ,3),A(ISTZ,2),A(ISTZ,1),
     *                 INDX(ISTZ),IDXWS(ISTZ))
                  BASE=UNIT-SIZE/2
                  IZ=ISTZ
                  NZ=1
                  FLAG=.TRUE.
C
                  DO WHILE (FLAG)
                     IF (A(IZ,3).LE.BASE) THEN
                        IPNTR(NZ,3)=IPNTR(NZ,3)+1
                     ELSE
                        BASE=BASE+UNIT
                        NZ=NZ+1
                        IZ=IZ-1
                        IF (NZ.GT.NS2) FLAG=.FALSE.
                     ENDIF
                     IZ=IZ+1
                     IF (IZ-ISTZ+1.GT.IPNTR(NY,2)) FLAG=.FALSE.
                  ENDDO
                  DO IDXZ=1,NS2
                     IF (IPNTR(IDXZ,3).GT.0) NBOX=NBOX+1
                  ENDDO
               ENDIF
               ISTZ=ISTZ+IPNTR(NY,2)
            ENDDO
C
C        ENDIF OF (IPNTR(NX,1).GT.0)
         ENDIF
         ISTY=ISTY+IPNTR(NX,1)
C     ENDDO OF NX=1,NS2
      ENDDO
C
      RETURN
      END
C*MODULE QFMM    *DECK UPPBOX
      SUBROUTINE UPPBOX(NBOX,NTBOX)
C
C     THIS ROUTINE RETURNS UPPER BOUND OF THE TOTAL NUMBER
C     OF NON-EMPTY BOX, NTBOX.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL QOPS,QFMM
      COMMON /QMFM  /SIZE,EPS,DPGD,QFMM,NP,NS,IWS,NPGP,MPMTHD,NUMRD,
     *       ITERMS,QOPS,ISCUT
C
      IMAX=NBOX
      NTBOX=NBOX
      DO I=NS-1,1,-1
         NTBOX=NTBOX+IMAX
      ENDDO
C
      NTBOX=NTBOX+1
C
      RETURN
      END
C
