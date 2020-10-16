C 12 Dec 03 - JAB - use generic functions
C  3 Jul 03 - MWS - PEDRA: use dynamic memory
C 23 Jun 03 - HL  - add gepol-rt. print control for gradients
C  7 Aug 02 - HL  - PEDRA: parallelize gepol-gb, print a warning message
C                   GAUBON: print a warning message for negative area
C 17 Apr 02 - MWS - synch up efmult and frginf common
C  8 Oct 01 - HL  - parallelize PCM
C 13 Jun 01 - HL  - SEPARA: buffer/EFP+PCM, extra check for 2 cavities
C 20 Feb 01 - BM  - PEDRA: additional argument for DERIVA call
C 29 Dec 00 - BM  - corrected the gradient bug
C  1 Nov 00 - PB  - fix the cases of IEF=1 and 2
C 11 Oct 00 - PB,BM - interfaced EFP+PCM
C 25 Aug 00 - BM  - added IEF solvation model
C 12 Nov 98 - AG  - PEDRA: ensure -dr- is in angstroms here
C 18 Mar 97 - PISA - NEW MODULE FOR PCM CAVITY DEFINITION
C
C*MODULE PCMCAV  *DECK PEDRAM
      SUBROUTINE PEDRAM
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL SOME
C
      PARAMETER (MXTS=2500, MXSP=250, MXFRG=50)
C
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /FMCOM / X(1)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C     ----- set memory pointers for polyhedra setup -----
C
      SOME = NPRINT.NE.-5
C
C     maximum number of tessalations is not known, take worst case
C     maximum number of spheres is not known, take worst case
C
      NUMTS  = MXTS
      NUMSPH = MXSP
      NMTPTS = NMTTPT
C
      CALL VALFM(LOADFM)
      LINTSP = LOADFM + 1
      LVERT  = LINTSP + NUMTS*10/NWDVAR
      LCENTR = LVERT  + NUMTS*10*3
      LNEWSP = LCENTR + NUMTS*10*3
      LICAV1 = LNEWSP + NUMSPH*2/NWDVAR
      LICAV2 = LICAV1 + NUMSPH
      LX     = LICAV2 + NUMSPH
      LY     = LX     + NUMTS
      LZ     = LY     + NUMTS
      LAAF   = LZ     + NUMTS
      LINTSPT= LAAF   + 3*NMTPTS
      LVERTT = LINTSPT+ 960*10
      LCENTRT= LVERTT + 960*10*3
      LXVALT = LCENTRT+ 960*10*3
      LYVALT = LXVALT + 960
      LZVALT = LYVALT + 960
      L_AST  = LZVALT + 960
      LXCTST = L_AST  + 960
      LYCTST = LXCTST + 960
      LZCTST = LYCTST + 960
      LISPHET= LZCTST + 960
      LNVERTT= LISPHET+ 960
      LTESTMP= LNVERTT+ 960
      LAST   = LTESTMP+ 9000*10
C
      NEED = LAST - LOADFM - 1
      IF(MASWRK) WRITE(IW,910) NEED
      CALL GETFM(NEED)
C
      CALL PEDRA(X(LINTSP),X(LVERT),X(LCENTR),X(LNEWSP),X(LICAV1),
     *           X(LICAV2),X(LX),X(LY),X(LZ),X(LAAF),
     *           NUMTS,NUMSPH,NMTPTS,SOME,
     *           X(LINTSPT),X(LVERTT),X(LCENTRT),
     *           X(LXVALT),X(LYVALT),X(LZVALT),X(L_AST),
     *           X(LXCTST),X(LYCTST),X(LZCTST),
     *           X(LISPHET),X(LNVERTT),X(LTESTMP))
C
      CALL RETFM(NEED)
      RETURN
C
  910 FORMAT(/1X,'MEMORY USED TO GENERATE CAVITY=',I10/)
      END
C*MODULE PCMCAV  *DECK PEDRA
      SUBROUTINE PEDRA(INTSPH,VERT,CENTR,NEWSPH,ICAV1,ICAV2,
     *                 XVAL,YVAL,ZVAL,AAF,NUMTS,NUMSPH,NMTPTS,SOME,
     *                 INTSPT,VERTT,CENTRT,XVALT,YVALT,ZVALT,AST,
     *                 XCTST,YCTST,ZCTST,ISPHET,NVERTT,TESTMP)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      CHARACTER*8 FRGNME
      LOGICAL SOME
C
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      DIMENSION INTSPH(NUMTS,10),VERT(NUMTS,10,3),CENTR(NUMTS,10,3),
     *          NEWSPH(NUMSPH,2),ICAV1(NUMSPH),ICAV2(NUMSPH),
     *          XVAL(NUMTS),YVAL(NUMTS),ZVAL(NUMTS),AAF(3,NMTPTS),
     *          INTSPT(960,10),VERTT(960,10,3),CENTRT(960,10,3),
     *          XVALT(960),YVALT(960),ZVALT(960),AST(960),
     *          XCTST(960),YCTST(960),ZCTST(960),ISPHET(960),
     *          NVERTT(960),TESTMP(9000,10)
      DIMENSION IDUM(360),THEV(24),FIV(24),JVT1(6,60),
     *          PP(3),PP1(3),PTS(3,10),CCC(3,10),CV(122,3)
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG, MXAO=2047)
C
      COMMON /EFMULT/ EFC(3,MXFGPT),EFCHG(2,MXFGPT),EFATRM(MXFGPT),
     *                EFBTRM(MXFGPT),EFATRM2(MXFGPT),EFBTRM2(MXFGPT),
     *                EFDIP(3,MXFGPT),EFQAD(6,MXFGPT),
     *                EFOCT(10,MXFGPT),FRGNME(MXFGPT)
      COMMON /FMCOM / XX(1)
      COMMON /FRGINF/ NMPTS(MXFRG),NMTTPT,IEFC,IEFD,IEFQ,IEFO,
     *                NPPTS(MXFRG),NPTTPT,IEFP,
     *                NRPTS(MXFRG),NRTTPT,IREP,ICHGP,NFRG
      COMMON /FRGMSS/ FPMASS(MXPT),FMASS(MXPT,MXFRG),
     *                FPNUC(MXPT),FGNUC(MXFGPT)
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMAG / I_NESF,L_AST
      COMMON /PCMCAV/ OMEGA,RET,FRO,ALPHA(MXSP),RIN(MXSP),ICENT,
     *                IPRINT,IRETCAV
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMF  / IPCDER,IFAST,Q2(MXTS),CHG2N(MXTS)
      COMMON /PCMPAR/ IPCM,NFT26,NFT27,IKREP,IEF,IP_F
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
      COMMON /PCMSPH/ INA(MXSP),INF(MXSP)
      COMMON /RUNLAB/ TITLE(10),ANAM(MXATM),BNAM(MXATM),BFLAB(MXAO)
      COMMON /TESSEL/ AREATL,AREAKP,BONDRY,INITS(MXSP),
     *                METHOD(MXSP),KEEPSM
C
      EQUIVALENCE (IDUM(1),JVT1(1,1))
C
      PARAMETER (TOANGS=0.52917724924D+00, ANTOAU=1.0D+00/TOANGS)
C
      DATA ZERO/0.0D+00/,FIRST/0.0174533D+00/
C
C     Angoli che individuano i centri e i vertici di un poliedro
C     inscritto in una sfera di raggio unitario centrata nell'origine
C
      DATA THEV/0.6523581398D+00,1.107148718D+00,1.382085796D+00,
     *          1.759506858D+00,2.034443936D+00,2.489234514D+00,
     *                         0.3261790699D+00,0.5535743589D+00,
     *          0.8559571251D+00,0.8559571251D+00,1.017221968D+00,
     *          1.229116717D+00,1.229116717D+00,1.433327788D+00,
     *          1.570796327D+00,1.570796327D+00,1.708264866D+00,
     *          1.912475937D+00,1.912475937D+00,2.124370686D+00,
     *          2.285635528D+00,2.285635528D+00,2.588018295D+00,
     *          2.815413584D+00/
      DATA FIV/               0.6283185307D+00,0.0000000000D+00,
     *         0.6283185307D+00,0.0000000000D+00,0.6283185307D+00,
     *         0.0000000000D+00,0.6283185307D+00,0.0000000000D+00,
     *         0.2520539002D+00,1.004583161D+00,0.6283185307D+00,
     *         0.3293628477D+00,0.9272742138D+00,0.0000000000D+00,
     *         0.3141592654D+00,0.9424777961D+00,0.6283185307D+00,
     *         0.2989556830D+00,0.9576813784D+00,0.0000000000D+00,
     *         0.3762646305D+00,0.8803724309D+00,0.6283188307D+00,
     *         0.0000000000D+00/
      DATA FIR/1.256637061D+00/
C
C     Il vettore IDUM, ripreso nella matrice JVT1, indica quali sono
C     i vertici delle varie tessere (using less than 19 continuations)
C
      DATA (IDUM(III),III=1,280)/
     *   1, 6, 2, 32, 36, 37, 1, 2, 3, 33, 32, 38, 1, 3, 4, 34,
     *   33, 39, 1, 4, 5, 35, 34, 40, 1, 5, 6, 36, 35, 41, 7, 2, 6, 51,
     *   42, 37, 8, 3, 2, 47, 43, 38, 9, 4, 3, 48, 44, 39, 10, 5, 4,
     *   49, 45, 40, 11, 6, 5, 50, 46, 41, 8, 2, 12, 62, 47, 52, 9,
     *   3, 13, 63, 48, 53, 10, 4, 14, 64, 49, 54, 11, 5, 15, 65, 50,
     *   55, 7, 6, 16, 66, 51, 56, 7, 12, 2, 42, 57, 52, 8, 13, 3,
     *   43, 58, 53, 9, 14, 4, 44, 59, 54, 10, 15, 5, 45, 60, 55, 11,
     *   16, 6, 46, 61, 56, 8, 12, 18, 68, 62, 77, 9, 13, 19, 69, 63,
     *   78, 10, 14, 20, 70, 64, 79, 11, 15, 21, 71, 65, 80, 7, 16,
     *   17, 67, 66, 81, 7, 17, 12, 57, 67, 72, 8, 18, 13, 58, 68, 73,
     *   9, 19, 14, 59, 69, 74, 10, 20, 15, 60, 70, 75, 11, 21, 16,
     *   61, 71, 76, 22, 12, 17, 87, 82, 72, 23, 13, 18, 88, 83, 73,
     *   24, 14, 19, 89, 84, 74, 25, 15, 20, 90, 85, 75, 26, 16, 21,
     *   91, 86, 76, 22, 18, 12, 82, 92, 77, 23, 19, 13, 83, 93, 78,
     *   24, 20, 14, 84, 94, 79, 25, 21, 15, 85, 95, 80, 26, 17, 16,
     *   86, 96, 81, 22, 17, 27, 102, 87, 97, 23, 18, 28, 103, 88, 98,
     *   24, 19, 29, 104, 89, 99, 25, 20, 30, 105, 90, 100, 26, 21,
     *   31, 106, 91, 101, 22, 28, 18, 92, 107, 98, 23, 29, 19, 93/
      DATA (IDUM(III),III=281,360)/
     *   108, 99, 24, 30, 20, 94, 109, 100, 25, 31, 21, 95, 110, 101,
     *   26, 27, 17, 96, 111, 97, 22, 27, 28, 107, 102, 112, 23, 28,
     *   29, 108, 103, 113, 24, 29, 30, 109, 104, 114, 25, 30, 31,
     *   110, 105, 115, 26, 31, 27, 111, 106, 116, 122, 28, 27, 117,
     *   118, 112, 122, 29, 28, 118, 119, 113, 122, 30, 29, 119, 120,
     *   114, 122, 31, 30, 120, 121, 115, 122, 27, 31, 121, 117, 116 /
C
C     It defines the solute's cavity and calculates vertices,
C     representative points and areas of tesserae with the
C     Gauss Bonnet Theorem.
C
C     this routine wants to work in Angstrom units - Alex Granovsky
C
      DR = DR / ANTOAU
C
C     New cavity to calculate dispersion and repulsion contributions
C
      IDISREP = 0
      IF (IDISP.EQ.2) IDISREP = 1
C
C B. Mennucci/1999
C if icent=0
C     radii from input and centers on nuclei
C if icent=1
C     radii and centers from input
C if icent=2
C     radii from input and centers on selected nuclei defined by
C     index ina(k) with k=1,nesfp. Example: xe(k)=c(1,ina(k))
C
C P. Bandyopadhyay/2000
C if icent=2
C     radii from input and centers on selected nuclei on fragments
C     defined by index inf(k) with k=1,nesff.
C     Example: xe(k)=AAF(1,inf(k))
C
      I_NESFP=NESFP
C
      IF(ICENT.LE.0) THEN
         JK=0
C
         DO J=1,NESFP
            XE(J)=C(1,J)
            YE(J)=C(2,J)
            ZE(J)=C(3,J)
            JK=JK+1
         ENDDO
C
        IBT=0
         IF(IP_F.EQ.1)THEN
          IAT=0
          DO 5120 IFRG=1,NFRG
             DO 5110 III=1,NMPTS(IFRG)
               IAT=IAT+1
               IF(FMASS(III,IFRG).GT.ZERO)THEN
               IBT=IBT+1
                 AAF(1,IBT) = EFC(1,IAT)
                 AAF(2,IBT) = EFC(2,IAT)
                 AAF(3,IBT) = EFC(3,IAT)
               ENDIF
5110      ENDDO
5120      ENDDO
C
         DO J=NESFP+1,NESFP+IBT
             KN=J-NESFP
             XE(J)=AAF(1,KN)
             YE(J)=AAF(2,KN)
             ZE(J)=AAF(3,KN)
          ENDDO
C
        END IF
        I_NESFP=NESFP+IBT
      END IF
C
      IF(ICENT.EQ.2)THEN
         DO J=1,NESFP
           XE(J)=C(1,INA(J))
           YE(J)=C(2,INA(J))
           ZE(J)=C(3,INA(J))
         ENDDO
         IF(IP_F.EQ.1)THEN
          IAT=0
          IBT=0
          DO 5121 IFRG=1,NFRG
             DO 5111 III=1,NMPTS(IFRG)
               IAT=IAT+1
               IF(FMASS(III,IFRG).GT.ZERO)THEN
                IBT=IBT+1
                AAF(1,IBT) = EFC(1,IAT)
                AAF(2,IBT) = EFC(2,IAT)
                AAF(3,IBT) = EFC(3,IAT)
               ENDIF
 5111        CONTINUE
 5121     CONTINUE
          DO J=1,NESFF
C           write(6,*)'INF',J,INF(J)
             XE(NESFP+J)=AAF(1,INF(J))
             YE(NESFP+J)=AAF(2,INF(J))
             ZE(NESFP+J)=AAF(3,INF(J))
          ENDDO
         ENDIF
C
         I_NESFP=NESFP+NESFF
      END IF
      NESF=NESFP
      I_NESF=I_NESFP
C
C  PEDRA prevede che i dati geometrici siano espressi in ANGSTROM :
C  vengono trasformati, e solo alla fine i risultati tornano in bohr.
C
      L_AST=I_NESFP
      DO I=1,L_AST
         RE(I)=RIN(I)
         XE(I)=XE(I)/ANTOAU
         YE(I)=YE(I)/ANTOAU
         ZE(I)=ZE(I)/ANTOAU
C           It is better not to touch ALPHA(*)
C        we donot use this routine for cav+dis+rep
C        IF(IDISREP.EQ.1) ALPHA(I) = 1.0D+00
C        IF(IRETCAV.EQ.1) ALPHA(I) = 1.0D+00
         RE(I) = RE(I) * ALPHA(I)
      ENDDO
C
C Se stiamo calcolando il contributo dispersivo e
C repulsivo, costruisce una nuova cavita', i cui centri coincidono
C con quelli iniziali, con i raggi incrementati di RSOLV.
C In questo caso PEDRA e' stata chiamata da DISREP,
C alla fine dei cicli PCM.
C
      IF(IDISREP.EQ.1) THEN
C         DO I = 1, NESFP
         DO I = 1,L_AST
            RE(I) = RE(I) + RDIF
         ENDDO
         RET = 100.0D+00
      END IF
C
C Se stiamo calcolando l'energia di cavitazione con il
C metodo di Pierotti sfera per sfera, e vogliamo usare solo le sfere
C iniziali (metodo Pierotti-Claverie), abbiamo IRETCAV=1.
C
      IF(IRETCAV.EQ.1) RET = 100.0D+00
C
C     Determinazione del centro delle cariche positive
C
      QTOT = 0.0D+00
      DO I = 1, NAT
         QTOT = QTOT + ZAN(I)
      ENDDO
      CCX = 0.0D+00
      CCY = 0.0D+00
      CCZ = 0.0D+00
      DO I = 1, NAT
         CCX = CCX + C(1,I)*ZAN(I)/QTOT
         CCY = CCY + C(2,I)*ZAN(I)/QTOT
         CCZ = CCZ + C(3,I)*ZAN(I)/QTOT
      ENDDO
      CCX = CCX / ANTOAU
      CCY = CCY / ANTOAU
      CCZ = CCZ / ANTOAU
C     IF(SOME .AND. MASWRK) WRITE(IW,9000) CCX,CCY,CCZ
      IF(MASWRK) WRITE(IW,9000) CCX,CCY,CCZ
C
      CALL VCLR(VERT,1,NUMTS*10*3)
      CALL VCLR(CENTR,1,NUMTS*10*3)
C
C     ----- Creation of added spheres -----
C
C      DO N = 1, NESF
      DO N = 1,I_NESF
         NEWSPH(N,1) = 0
         NEWSPH(N,2) = 0
      ENDDO
C
C          save number of electrons to restore on exit...
C
      NELEC=NE
C
      ITYPC = 0
      OMEGA0=OMEGA*FIRST
      SENOM=SIN(OMEGA0)
      COSOM2=(COS(OMEGA0))**2
      RTDD=RET+RSOLV
      RTDD2=RTDD*RTDD
C      NET=NESF
      NET=L_AST
      NN=2
C      NE=NESF
      NE=L_AST
C      NEV=NESF
      NEV=L_AST
      GO TO 100
  110 CONTINUE
      NN=NE+1
      NE=NET
  100 CONTINUE
C
C     check on the number of spheres
C
      IF(NE.GT.MXSP) THEN
         IF(MASWRK)WRITE(IW,*)
     *       ' PEDRA: TOO MANY SPHERES, -MXSP- MUST BE LARGER.'
         CALL ABRT
         STOP
      END IF
C
      DO 120 I=NN,NE
      NES=I-1
      DO 130 J=1,NES
      RIJ2=(XE(I)-XE(J))**2+
     $     (YE(I)-YE(J))**2+
     $     (ZE(I)-ZE(J))**2
      RIJ=SQRT(RIJ2)
      RJD=RE(J)+RSOLV
      TEST1=RE(I)+RJD+RSOLV
      IF(RIJ.GE.TEST1) GO TO 130
      REG=MAX(RE(I),RE(J))
      REP=MIN(RE(I),RE(J))
      REG2=REG*REG
      REP2=REP*REP
      TEST2=REP*SENOM+SQRT(REG2-REP2*COSOM2)
      IF(RIJ.LE.TEST2) GO TO 130
      REGD2=(REG+RSOLV)*(REG+RSOLV)
      TEST3=(REGD2+REG2-RTDD2)/REG
      IF(RIJ.GE.TEST3) GO TO 130
      DO 140 K=1,NEV
      IF(K.EQ.J .OR. K.EQ.I) GO TO 140
      RJK2=(XE(J)-XE(K))**2+
     $     (YE(J)-YE(K))**2+
     $     (ZE(J)-ZE(K))**2
      IF(RJK2.GE.RIJ2) GO TO 140
      RIK2=(XE(I)-XE(K))**2+
     $     (YE(I)-YE(K))**2+
     $     (ZE(I)-ZE(K))**2
      IF(RIK2.GE.RIJ2) GO TO 140
       RJK=SQRT(RJK2)
       RIK=SQRT(RIK2)
       SP=(RIJ+RJK+RIK)/2.0D+00
       HH=4*(SP*(SP-RIJ)*(SP-RIK)*(SP-RJK))/RIJ2
       REO=RE(K)*FRO
      IF(K.GE.NE)REO=0.0002D+00
      REO2=REO*REO
      IF(HH.LT.REO2) GO TO 130
  140 CONTINUE
      REPD2=(REP+RSOLV)**2
      TEST8=SQRT(REPD2-RTDD2)+SQRT(REGD2-RTDD2)
      IF(RIJ.LE.TEST8) GO TO 150
      REND2=REGD2+REG2-(REG/RIJ)*(REGD2+RIJ2-REPD2)
      IF(REND2.LE.RTDD2) GO TO 130
      REN=SQRT(REND2)-RSOLV
      FC=REG/(RIJ-REG)
      TEST7=REG-RE(I)
      KG=I
      KP=J
      IF(TEST7.LE.0.000000001D+00) GO TO 160
      KG=J
      KP=I
  160 FC1=FC+1.0D+00
      XEN=(XE(KG)+FC*XE(KP))/FC1
      YEN=(YE(KG)+FC*YE(KP))/FC1
      ZEN=(ZE(KG)+FC*ZE(KP))/FC1
      ITYPC = 1
      GO TO 170
  150 R2GN=RIJ-REP+REG
      RGN=R2GN/2.0D+00
      FC=R2GN/(RIJ+REP-REG)
      FC1=FC+1.0D+00
      TEST7=REG-RE(I)
      KG=I
      KP=J
      IF(TEST7.LE.0.000000001D+00) GO TO 180
      KG=J
      KP=I
  180 XEN=(XE(KG)+FC*XE(KP))/FC1
      YEN=(YE(KG)+FC*YE(KP))/FC1
      ZEN=(ZE(KG)+FC*ZE(KP))/FC1
      REN=SQRT(REGD2+RGN*(RGN-(REGD2+RIJ2-REPD2)/RIJ))-RSOLV
  170 NET=NET+1
      XE(NET)=XEN
      YE(NET)=YEN
      ZE(NET)=ZEN
      RE(NET)=REN
C
C     The numbers of the spheres, which generate the new sphere
C     NET are memorized in the matrix NEWSPH(NESF,2):
C     if the latter is of type A or B both these numbers are positive,
C     if it is of type C the number of the main sphere is negative.
C     For the definition of the types see JCC 11 (1990) 1047.
C
      IF(ITYPC.EQ.0) THEN
        NEWSPH(NET,1) = KG
        NEWSPH(NET,2) = KP
      ELSE IF(ITYPC.EQ.1) THEN
        NEWSPH(NET,1) = - KG
        NEWSPH(NET,2) = KP
      END IF
C
  130 CONTINUE
      NEV=NET
  120 CONTINUE
      IF(NET.NE.NE) GO TO 110
C NESF has to be updated!!
      NESF=NET
      L_AST=NET
C
C
C
C
C
C
C
C     ----- Partition of the cavity surface into tesserae -----
C
      VOL=ZERO
      STOT=ZERO
C
C*****COORDINATES OF VERTICES OF TESSERAE IN A SPHERE WITH UNIT RADIUS.
C
C     Vengono memorizzati i vertici (nella matrice CV) e i centri (nei
C     vettori XC,YC,ZC) di 240 tessere (60 grandi divise in 4 piu'
C     piccole) La matrice JVT1(i,j) indica quale e' il numero d'ordine
C     del vertice i-esimo della j-esima tessera grande. In ogni tessera
C     grande i 6 vertici sono cosi' disposti:
C
C                                    1
C
C                                 4     5
C
C                              3     6     2
C
      CV(1,1)=0.0D+00
      CV(1,2)=0.0D+00
      CV(1,3)=1.0D+00
      CV(122,1)=0.0D+00
      CV(122,2)=0.0D+00
      CV(122,3)=-1.0D+00
      II=1
      DO 200 I=1,24
      TH=THEV(I)
      FI=FIV(I)
      CTH=COS(TH)
      STH=SIN(TH)
      DO 210 J=1,5
      FI=FI+FIR
      IF(J.EQ.1) FI=FIV(I)
      II=II+1
      CV(II,1)=STH*COS(FI)
      CV(II,2)=STH*SIN(FI)
      CV(II,3)=CTH
  210 CONTINUE
  200 CONTINUE
C
C
C
C     Controlla se ciascuna tessera e' scoperta o va tagliata
C
      K1=0
      K2=0
      DO III=1,NAT
         IF(METHOD(III).EQ.1)K1=1
         IF(METHOD(III).EQ.3)K2=1
      END DO
      IF(MASWRK)WRITE(IW,*)' '
      IF(MASWRK)CALL TIMIT(1)
      IF(MASWRK)WRITE(IW,*)' '
      IF(K1.EQ.1.AND.K2.EQ.0)THEN
      IF(MASWRK)WRITE(IW,*)'GEPOL-GB: GENERATING TESSERAE'
      ELSE IF(K2.EQ.1.AND.K1.EQ.0)THEN
      IF(MASWRK)WRITE(IW,*)'GEPOL-RT: GENERATING TESSERAE'
      ELSE IF(K2+K1.EQ.2)THEN
      IF(MASWRK)WRITE(IW,*)
     * 'COMBINED GEPOL-RT AND GEPOL-GB: GENERATING TESSERAE'
      ELSE
      IF(MASWRK)WRITE(IW,*)
     * 'COMBINED GEPOL-RT-GB: GENERATING TESSERAE'
      END IF
      IF(MASWRK)WRITE(IW,*)' '
C
      NN = 0
C      DO 300 NSFE = 1, NESF
      DO 300 NSFE = 1, L_AST
      IF(RE(NSFE).LT.0.010D+00) GO TO 300
      XEN = XE(NSFE)
      YEN = YE(NSFE)
      ZEN = ZE(NSFE)
      REN = RE(NSFE)
C
      NSFECPY = NSFE
      IF(METHOD(NSFE).EQ.1) THEN
        CALL PEDGB(NSFECPY,XEN,YEN,ZEN,REN,NN,JVT1,
     *             CV,PTS,CCC,PP,PP1,NUMTS,INTSPH,
     *             XCTST,YCTST,ZCTST,XVALT,YVALT,ZVALT,
     *             AST,ISPHET,NVERTT,VERTT,CENTRT,INTSPT,
     *             XVAL,YVAL,ZVAL,VERT,CENTR)
      ELSE IF(METHOD(NSFE).EQ.3) THEN
        CALL PEDRT(NSFECPY,XEN,YEN,ZEN,REN,NN,JVT1,
     *             CV,PTS,CCC,PP,PP1,NUMTS,INTSPH,
     *             XVAL,YVAL,ZVAL,VERT,CENTR,TESTMP)
      END IF
C
C
 300  CONTINUE
      NTS = NN
      IF(MASWRK)WRITE(IW,*)' '
      IF(MASWRK)WRITE(IW,*)' '
      IF(MASWRK)CALL TIMIT(1)
C     IF(MASWRK)WRITE(IW,*)'GEPOL-GB: TESSERAE GENERATED'
      IF(MASWRK)WRITE(IW,*)'TESSERAE GENERATED'
      IF(MASWRK)WRITE(IW,*)' '
C
C     Verifica se due tessere sono troppo vicine
C
      TEST = 0.020D+00
      TEST2 = TEST*TEST
      DO 400 I = 1, NTS-1
      IF(AS(I).EQ.ZERO) GO TO 400
      XI = XCTS(I)
      YI = YCTS(I)
      ZI = ZCTS(I)
      II = I + 1
      DO 410 J = II , NTS
      IF(ISPHE(I).EQ.ISPHE(J)) GO TO 410
      IF(AS(J).EQ.ZERO) GO TO 410
      XJ = XCTS(J)
      YJ = YCTS(J)
      ZJ = ZCTS(J)
      RIJ = (XI-XJ)**2 + (YI-YJ)**2 + (ZI-ZJ)**2
      IF(RIJ.GT.TEST2) GO TO 410
C
C     La routine originaria sostituiva le due tessere troppo vicine
C     con una sola tessera. Nel caso Gauss-Bonnet, anche i vertici
C     delle tessere e i centri degli archi vengono memorizzati ed e'
C     impossibile sostituirli nello stesso modo: percio' si limitera'
C     ad avvisare che due tessere sono molto vicine (in CCVE dovrebbero
C     essere comunque escluse dall'auto- polarizzazione).
C
      IF(MASWRK)WRITE(IW,9010) I,J,SQRT(RIJ),TEST
 410  CONTINUE
 400  CONTINUE
C
C     Calcola il volume della cavita' con la formula (t. di Gauss):
C                V=SOMMAsulleTESSERE{A r*n}/3
C     dove r e' la distanza del punto rappresentativo dall'origine,
C     n e' il versore normale alla tessera, A l'area della tessera,
C     e * indica il prodotto scalare.
C
      VOL = ZERO
      DO ITS = 1, NTS
         NSFE = ISPHE(ITS)
C
C     Trova il versore normale
C
         XN = (XCTS(ITS) - XE(NSFE)) / RE(NSFE)
         YN = (YCTS(ITS) - YE(NSFE)) / RE(NSFE)
         ZN = (ZCTS(ITS) - ZE(NSFE)) / RE(NSFE)
C
C     Trova il prodotto scalare
C
         PROD = XCTS(ITS)*XN + YCTS(ITS)*YN + ZCTS(ITS)*ZN
         VOL = VOL + AS(ITS) * PROD / 3.0D+00
      ENDDO
C
C     Stampa la geometria della cavita'
C
      STOT=ZERO
C      DO 500 I=1,NESF
      DO 500 I=1,L_AST
         SSFE(I)=ZERO
  500 CONTINUE
      DO 510 I=1,NTS
         K=ISPHE(I)
         SSFE(K)=SSFE(K)+AS(I)
  510 CONTINUE
C
C     IF(SOME) THEN
C         if(maswrk)WRITE(IW,9020) NESF
         IF(MASWRK)WRITE(IW,9020) L_AST
C         DO 520 I=1,NESF
         DO 520 I=1,L_AST
            IF(MASWRK)
     * WRITE(IW,9030) I,XE(I),YE(I),ZE(I),RE(I),SSFE(I)
            STOT=STOT+SSFE(I)
  520    CONTINUE
         IF(MASWRK)WRITE(IW,9040) NTS,STOT,VOL
C     END IF
C
C     ----- set up for possible gradient calculation -----
C     Se richiesto calcola la derivata dell'area e del punto
C     rappresentativo di ciascuna tessera rispetto
C     alle coordinate dei centri delle sfere originarie.
C
      CALL DERCHK(NDER)
      IF(NDER.GT.0.AND.IPCDER.EQ.1) THEN
C
         IF(IP_F.EQ.0)THEN
           IF(NESFP.GT.NAT) THEN
             IF(MASWRK)WRITE(IW,9050) NESFP,NAT
             CALL ABRT
             STOP
           ENDIF
         ELSE
           NATNEW=NAT+3*NFRG
           IF(I_NESF.GT.NATNEW)THEN
             IF(MASWRK)WRITE(IW,9050)I_NESF,NATNEW
             CALL ABRT
             STOP
           END IF
         END IF
C
         CALL VALFM(LOADFM)
         LDRPNT = LOADFM + 1
         LDRTES = LDRPNT + NTS*NAT*3*3
         LDRCNT = LDRTES + NTS*NAT*3
         LDRRAD = LDRCNT + L_AST*NAT*3*3
         LAST   = LDRRAD + L_AST*NAT*3
         NEEDG = LAST - LOADFM - 1
C        IF(SOME .AND. MASWRK) WRITE(IW,9060) NEEDG
         IF(MASWRK) WRITE(IW,9060) NEEDG
         CALL GETFM(NEEDG)
C
         CALL VCLR(XX(LDRCNT),1,L_AST*NAT*3*3)
         CALL VCLR(XX(LDRRAD),1,L_AST*NAT*3)
         NESFT=L_AST
         DO NSFE = 1,NAT
            NATSPH=0
            NSFER=NSFE
            IF(ICENT.EQ.2)THEN
              NATSPH=1
              DO JJ=1,NESFP
                IF(INA(JJ).EQ.NSFE)THEN
                 NATSPH=0
                 NSFER=JJ
                ENDIF
              ENDDO
            ENDIF
            NSFECPY = NSFE
            DO ICOORD = 1, 3
               IF(NATSPH.EQ.0) THEN
                  ICRDCPY = ICOORD
                  CALL DERIVA(NSFECPY,NSFER,ICRDCPY,INTSPH,VERT,CENTR,
     *                        NEWSPH,NUMTS,NUMSPH,XX(LDRPNT),
     *                        XX(LDRTES),XX(LDRCNT),XX(LDRRAD),
     *                        NTS,NAT,NESFT)
               END IF
            ENDDO
         ENDDO
C
C Mennucci 2000: no more required
C     fill in extra spheres for case -nat- exceeding -nefsp-.
C
COLD         IF(NAT.GT.NESFP) CALL DERIVB(XX(LDRCNT),NAT,NESFP)
CC        IF(NATNEW.GT.I_NESF) CALL DERIVB(XX(LDRCNT),NAT,I_NESF)
C
C     store results, this is -before- the d-inverse matrix
C
         CALL SEQREW(NFT26)
         CALL SQWRIT(NFT26,XX(LDRPNT),NTS*NAT*3*3)
         CALL SQWRIT(NFT26,XX(LDRTES),NTS*NAT*3)
         CALL SQWRIT(NFT26,XX(LDRCNT),L_AST*NAT*3*3)
         CALL SQWRIT(NFT26,XX(LDRRAD),L_AST*NAT*3)
C
         CALL RETFM(NEEDG)
      END IF
C
C     Trasform results in bohr
C
C      DO I=1,NESF
      DO I=1,L_AST
         RE(I)=RE(I)*ANTOAU
         XE(I)=XE(I)*ANTOAU
         YE(I)=YE(I)*ANTOAU
         ZE(I)=ZE(I)*ANTOAU
      ENDDO
      DO I=1,NTS
         AS(I)=AS(I)*ANTOAU*ANTOAU
         XCTS(I)=XCTS(I)*ANTOAU
         YCTS(I)=YCTS(I)*ANTOAU
         ZCTS(I)=ZCTS(I)*ANTOAU
         XCTS(I+NTS)=XVAL(I)*ANTOAU
         YCTS(I+NTS)=YVAL(I)*ANTOAU
         ZCTS(I+NTS)=ZVAL(I)*ANTOAU
      ENDDO
      CCX = CCX * ANTOAU
      CCY = CCY * ANTOAU
      CCZ = CCZ * ANTOAU
      DR = DR * ANTOAU
C
C
      IF(ICENT.EQ.3)THEN
C
C  prints the closer tessera for each atom
C
      IF(MASWRK)WRITE(6,32)
      DO IAT=1,NAT
        XI=C(1,IAT)
        YI=C(2,IAT)
        ZI=C(3,IAT)
        RMIN=1.D9
        IC=0
        DO I=1,NTS
          RIA=SQRT((XI-XCTS(I))**2+(YI-YCTS(I))**2+(ZI-ZCTS(I))**2)
          IF(RIA.LT.RMIN) THEN
            RMIN=RIA
            IC=I
          ENDIF
        ENDDO
        IF(MASWRK)WRITE(6,34)
     *     IAT,ANAM(IAT),XI/ANTOAU,YI/ANTOAU,ZI/ANTOAU
     $                 ,RMIN/ANTOAU,ISPHE(IC)
      ENDDO
   32 FORMAT(/'   ----- APPARENT CHARGES CLOSEST TO EACH ATOM'
     $        ,' (ANGST) -----'
     $      ,/'   ATOM    ---- ATOM POSITION ----    DISTANCE'
     $       ,' SPHERE')
   34 FORMAT(I4,1X,A4,3F8.3,F12.3,I6)
      ENDIF
C
C B. Mennucci/1999
C     If IEF is equal to 1 or 2 (anisotropic dielectrics and ionic
C     solutions) store results, this is -before- the d-inverse matrix
C
      IF(IEF.GT.0.AND.IEF.LT.3)THEN
         CALL VALFM(LOADFM)
         LVERT  = LOADFM + 1
         LCENTR = LVERT  + NTS*3*10
         LAST   = LCENTR + NTS*3*10
         NEEDG  = LAST - LOADFM - 1
C
         IF(SOME .AND. MASWRK) WRITE(IW,9061) NEEDG
         CALL GETFM(NEEDG)
C
         IV=0
         DO I=1,NTS
         DO J=1,10
           DO L=1,3
             IV=IV+1
             XX(LVERT-1+IV) = VERT(I,J,L)*ANTOAU
             XX(LCENTR-1+IV) = CENTR(I,J,L)*ANTOAU
          ENDDO
         ENDDO
         ENDDO
C
         IF(NDER.LE.0)CALL SEQREW(NFT26)
         CALL SQWRIT(NFT26,XX(LVERT),NTS*3*10)
         CALL SQWRIT(NFT26,XX(LCENTR),NTS*3*10)
C
         CALL RETFM(NEEDG)
      ENDIF
C
      IF(IPRINT.GE.1 .AND. MASWRK) THEN
         WRITE(IW,9070)
         WRITE(IW,9080)
         WRITE(IW,9090) (I,ISPHE(I),AS(I),XCTS(I),YCTS(I),ZCTS(I),
     *                   XCTS(NTS+I),YCTS(NTS+I),ZCTS(NTS+I), I=1,NTS)
      END IF
C----------------------------------------------------------------------
      NE=NELEC
C
C     Call the routine which checks if the cavity is single or divided.
C
      CALL SEPARA(ICAV1,ICAV2,NUMSPH,NCAV1,NCAV2)
C
C     The dispersion calculation is allowed only in the case of
C     single cavity.
C
      IF(NCAV2.NE.0) IDISP=0
C
      IDISREP = 0
      IRETCAV = 0
      IF (IDISP.EQ.2) IDISP = 1
      RETURN
C
 9000 FORMAT(10X,'-- CENTER OF CHARGE --'/
     *        1X,'X =',F8.4,' A  Y =',F8.4,' A  Z =',F8.4,' A')
 9010 FORMAT(1X,'WARNING: THE DISTANCE BETWEEN TESSERAE ',I6,
     *       ' AND ',I6,' IS ',F6.4,' A, LESS THAN ',F6.4,' A')
 9020 FORMAT(/1X,'TOTAL NUMBER OF SPHERES=',I5/
     *        1X,'SPHERE             CENTER  (X,Y,Z) (A)            ',
     *           '   RADIUS (A)      AREA(A*A)')
 9030 FORMAT(I4,4F15.9,F15.9)
 9040 FORMAT(/1X,'TOTAL NUMBER OF TESSERAE=',I8/
     *        1X,'SURFACE AREA=',F14.8,'(A**2)',4X,'CAVITY VOLUME=',
     *            F14.8,' (A**3)')
 9050 FORMAT(1X,'PEDRA: CONFUSION ABOUT SPHERE COUNTS. NESFP,NAT=',2I6)
 9060 FORMAT(/1X,'ADDITIONAL MEMORY NEEDED TO SETUP GRADIENT RUN=',I10)
 9061 FORMAT(/1X,'ADDITIONAL MEMORY NEEDED TO SETUP IEF RUN=',I10)
 9070 FORMAT(1X,'***  SUDDIVISIONE DELLA SUPERFICIE  ***')
 9080 FORMAT(' TESSERA  SFERA   AREA   X Y Z CENTRO TESSERA  ',
     *       'X Y Z PUNTO NORMALE')
 9090 FORMAT(2I4,7F12.7)
      END
C*MODULE PCMCAV  *DECK TESSERA
      SUBROUTINE TESSERA(NS,NV,PTS,CCC,PP,PP1,AREA,INTSPH,NUMTS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION PTS(3,10),CCC(3,10),PP(3),PP1(3),INTSPH(NUMTS,10)
      DIMENSION P1(3),P2(3),P3(3),P4(3),POINT(3),
     *          PSCR(3,10),CCCP(3,10),POINTL(3,10),
     *          IND(10),LTYP(10),INTSCR(10)
C
      PARAMETER (MXTS=2500, MXSP=250)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PCMAG / I_NESF,L_AST
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
C
C
      AREA = 0.0D+00
C
C     copy the sphere center coordinates to
C     CCC(*,*).
C
      DO J=1, 3
        CCC(1,J) = XE(NS)
        CCC(2,J) = YE(NS)
        CCC(3,J) = ZE(NS)
      ENDDO
C
C     copy the sphere number to INTSPH(*,*)
C     note the 3 initial vertices are copied
C
      DO N = 1, 3
        INTSPH(NUMTS,N) = NS
      ENDDO
C
C     loop over other spheres
C     note the number of vertices are changing in the loop
C
      DO 150 NSFE1=1,L_AST
      IF(NSFE1.EQ.NS) GO TO 150
C
C
      IF(NSFE1.EQ.NS .OR. RE(NSFE1).LT.0.020D+00)    GOTO 150
      IF(ABS(XE(NS)-XE(NSFE1)).GT.(RE(NS)+RE(NSFE1)))GOTO 150
      IF(ABS(YE(NS)-YE(NSFE1)).GT.(RE(NS)+RE(NSFE1)))GOTO 150
      IF(ABS(ZE(NS)-ZE(NSFE1)).GT.(RE(NS)+RE(NSFE1)))GOTO 150
C
      R12 = SQRT((XE(NS)-XE(NSFE1))**2 +
     *            (YE(NS)-YE(NSFE1))**2 +
     *            (ZE(NS)-ZE(NSFE1))**2  )
      IF(R12.GE.RE(NS)+RE(NSFE1)) GO TO 150
      IF(R12.LE.ABS(RE(NS)-RE(NSFE1)) .AND.
     *   RE(NSFE1).LT.RE(NS)) GO TO 150
      IF(R12.LE.ABS(RE(NS)-RE(NSFE1)) .AND.
     *   RE(NSFE1).GT.RE(NS)) RETURN
C
      IF(NV.EQ.3) THEN
      DDD1=SQRT((PTS(1,1)-PTS(1,2))**2 +
     *           (PTS(2,1)-PTS(2,2))**2 +
     *           (PTS(3,1)-PTS(3,2))**2  )
      DDD2=SQRT((PTS(1,3)-PTS(1,2))**2 +
     *           (PTS(2,3)-PTS(2,2))**2 +
     *           (PTS(3,3)-PTS(3,2))**2  )
      DDD3=SQRT((PTS(1,3)-PTS(1,1))**2 +
     *           (PTS(2,3)-PTS(2,1))**2 +
     *           (PTS(3,3)-PTS(3,1))**2  )
      IF(DDD1.LT.1.0D-05 .OR.
     *   DDD2.LT.1.0D-05 .OR.
     *   DDD3.LT.1.0D-05) RETURN
      END IF
C
C     copy sphere number into temperary INTSCR(J)
C     copy vertix coordinates into temp PSCR(I,J)
C     copy center point coordinates of the sphere into temp CCCP(I,J)
C
      DO J =1, NV
        INTSCR(J) = INTSPH(NUMTS,J)
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
C
C
C
      DO 100 I=1,NV
        DELR2=(PTS(1,I)-XE(NSFE1))**2+(PTS(2,I)-YE(NSFE1))**2+
     *  (PTS(3,I)-ZE(NSFE1))**2
        DELR=SQRT(DELR2)
        IF(DELR.LT.RE(NSFE1)) THEN
          IND(I) = 1
          ICOP = ICOP+1
        END IF
 100  CONTINUE
C
C     if all the vertices are overlapped by any one of
C     the other sphere, finish
C
      IF(ICOP.EQ.NV) RETURN
C                    ******
C
C
C     classify the sidelines into 5 types:
C     0. whole line in the sphere
C     1. head out of the sphere but the end in the sphere
C     2. head in the sphere but the end out the sphere
C     3. both head and end out, but middle part in the sphere
C     4. whole line out of the sphere
C
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
C
C     Su ogni lato si definiscono 11 punti equispaziati, che vengono
C     controllati
C
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
      ENDDO
C
C      determine how many cut points for the tessera.
C      type 1 and 2 have 1 cut point
C      type 3 has 2 cut points.
C      if a tessera has more than 2 cut points at
C      any one of the other spheres, finish, because
C      it means this tessera is too close to other sphere
C
C
      ICUT = 0
      DO L = 1, NV
        IF(LTYP(L).EQ.1.OR.LTYP(L).EQ.2) ICUT = ICUT + 1
        IF(LTYP(L).EQ.3) ICUT = ICUT + 2
      ENDDO
      ICUT = ICUT / 2
      IF(ICUT.GT.1) RETURN
C
C     find out the cut points for each line and the
C     intersection cycle if the line is in type=1
C     or type=3
C
      N = 1
      DO 300 L = 1, NV
        IV1 = L
        IV2 = L+1
        IF(L.EQ.NV) IV2 = 1
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C      if type=0, whole line in the other sphere
C
        IF(LTYP(L).EQ.0) GO TO 300
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C      if type=1, head out, end in
C
        IF(LTYP(L).EQ.1) THEN
        DO JJ = 1, 3
          PTS(JJ,N) = PSCR(JJ,IV1)
          CCC(JJ,N) = CCCP(JJ,IV1)
        ENDDO
        INTSPH(NUMTS,N) = INTSCR(IV1)
        N = N+1
C
C     determine the cut point (P4) through P1, P2, P3
C     with INTER
C
C     P1 = head of the line
C     P2 = end of the line
C     P3 = center of the sphere where the line locats
C     P4 = the cut point
C
        DO JJ = 1, 3
          P1(JJ) = PSCR(JJ,IV1)
          P2(JJ) = PSCR(JJ,IV2)
          P3(JJ) = CCCP(JJ,IV1)
        ENDDO
        CALL INTER(P1,P2,P3,P4,NSFE1,0)
C
C     copy the cut point to PTS(*,*)
C
        DO JJ = 1,3
          PTS(JJ,N) = P4(JJ)
        ENDDO
C
C     determine the center of the intesection cycle between
C     the sphere NS and the other sphere NSFE1
C
        DE2 = (XE(NSFE1)-XE(NS))**2+(YE(NSFE1)-YE(NS))**2+
     *        (ZE(NSFE1)-ZE(NS))**2
        CCC(1,N)=XE(NS)+(XE(NSFE1)-XE(NS))*
     *           (RE(NS)**2-RE(NSFE1)**2+DE2)/(2.0D+00*DE2)
        CCC(2,N)=YE(NS)+(YE(NSFE1)-YE(NS))*
     *           (RE(NS)**2-RE(NSFE1)**2+DE2)/(2.0D+00*DE2)
        CCC(3,N)=ZE(NS)+(ZE(NSFE1)-ZE(NS))*
     *           (RE(NS)**2-RE(NSFE1)**2+DE2)/(2.0D+00*DE2)
        INTSPH(NUMTS,N) = NSFE1
        N = N+1
        END IF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     if type=2, head in, end out
C
        IF(LTYP(L).EQ.2) THEN
C
C     determine the cut point (P4) through P1, P2, P3
C     with INTER
C
C     P1 = head of the line
C     P2 = end of the line
C     P3 = center of the sphere where the line locats
C     P4 = the cut point
C
        DO JJ = 1, 3
          P1(JJ) = PSCR(JJ,IV1)
          P2(JJ) = PSCR(JJ,IV2)
          P3(JJ) = CCCP(JJ,IV1)
        ENDDO
        CALL INTER(P1,P2,P3,P4,NSFE1,1)
C
C     copy the cut point to PTS(*,*)
C
        DO JJ = 1,3
          PTS(JJ,N) = P4(JJ)
          CCC(JJ,N) = CCCP(JJ,IV1)
        ENDDO
        INTSPH(NUMTS,N) = INTSCR(IV1)
        N = N+1
        END IF
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     if type=3, middle in, head and end out
C
        IF(LTYP(L).EQ.3) THEN
        DO JJ = 1, 3
          PTS(JJ,N) = PSCR(JJ,IV1)
          CCC(JJ,N) = CCCP(JJ,IV1)
        ENDDO
        INTSPH(NUMTS,N) = INTSCR(IV1)
        N = N+1
C
C     determine the cut point (P4) through P1, P2, P3
C     with INTER
C
C     P1 = head of the line
C     P2 = end of the line
C     P3 = center of the sphere where the line locats
C     P4 = the cut point
C
        DO JJ = 1, 3
          P1(JJ) = PSCR(JJ,IV1)
          P2(JJ) = POINTL(JJ,L)
          P3(JJ) = CCCP(JJ,IV1)
        ENDDO
        CALL INTER(P1,P2,P3,P4,NSFE1,0)
C
C     copy the cut point to PTS(*,*)
C
        DO JJ = 1,3
          PTS(JJ,N) = P4(JJ)
        ENDDO
C
C     determine the center of the intesection sphere between
C     the sphere NS and the other sphere NSFE1
C
        DE2 = (XE(NSFE1)-XE(NS))**2+(YE(NSFE1)-YE(NS))**2+
     *        (ZE(NSFE1)-ZE(NS))**2
        CCC(1,N)=XE(NS)+(XE(NSFE1)-XE(NS))*
     *           (RE(NS)**2-RE(NSFE1)**2+DE2)/(2.0D+00*DE2)
        CCC(2,N)=YE(NS)+(YE(NSFE1)-YE(NS))*
     *           (RE(NS)**2-RE(NSFE1)**2+DE2)/(2.0D+00*DE2)
        CCC(3,N)=ZE(NS)+(ZE(NSFE1)-ZE(NS))*
     *           (RE(NS)**2-RE(NSFE1)**2+DE2)/(2.0D+00*DE2)
        INTSPH(NUMTS,N) = NSFE1
        N = N+1
C
C     determine the cut point (P4) through P1, P2, P3
C     with INTER
C
C     P1 = head of the line
C     P2 = end of the line
C     P3 = center of the sphere where the line locats
C     P4 = the cut point
C
        DO JJ = 1, 3
          P1(JJ) = POINTL(JJ,L)
          P2(JJ) = PSCR(JJ,IV2)
          P3(JJ) = CCCP(JJ,IV1)
        ENDDO
        CALL INTER(P1,P2,P3,P4,NSFE1,1)
C
C     copy the cut point to PTS(*,*)
C
        DO JJ = 1,3
          PTS(JJ,N) = P4(JJ)
          CCC(JJ,N) = CCCP(JJ,IV1)
        ENDDO
        INTSPH(NUMTS,N) = INTSCR(IV1)
        N = N + 1
        END IF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     if type=4, whole line out
C
        IF(LTYP(L).EQ.4) THEN
        DO JJ = 1, 3
          PTS(JJ,N) = PSCR(JJ,IV1)
          CCC(JJ,N) = CCCP(JJ,IV1)
        ENDDO
        INTSPH(NUMTS,N) = INTSCR(IV1)
        N = N+1
        END IF
C
 300  CONTINUE
C
      NV = N - 1
C
C     Check if the number of vertices is over 10
C
      IF(NV.GT.10) THEN
         WRITE(IW,*)
     *   ' ERROR: THE NUMBER OF VERTICES IS OVER 10'
         CALL ABRT
         STOP
      END IF
 150  CONTINUE
C
C     calculate the area of the tessera
C
      CALL GAUBON(NV,NS,PTS,CCC,PP,PP1,AREA,INTSPH,NUMTS)
      RETURN
      END
C*MODULE PCMCAV  *DECK INTER
      SUBROUTINE INTER(P1,P2,P3,P4,NS,I)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION P1(3),P2(3),P3(3),P4(3)
C
      PARAMETER (MXTS=2500, MXSP=250)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
C
C     Trova il punto P4, sull`arco P1-P2 sotteso dal centro P3, che
C     si trova sulla superficie della sfera NS
C     P4 e' definito come combinazioe lineare di P1 e P2, con
C     il parametro ALPHA ottimizzato per tentativi.
C
      R2 = (P1(1)-P3(1))**2+(P1(2)-P3(2))**2+(P1(3)-P3(3))**2
      R = SQRT(R2)
      TOL = 1.0D-10
      ALPHA = 0.5D+00
      DELTA = 0.0D+00
      M = 1
  10  CONTINUE
      IF (M.GT.1000) THEN
         WRITE(IW,*)
     *      'WARNING: NOT CONVERGED IN INTER'
C        CALL ABRT
         RETURN
C        STOP
      END IF
      ALPHA = ALPHA + DELTA
      DNORM = 0.0D+00
      DO JJ = 1,3
       P4(JJ)=P1(JJ)+ALPHA*(P2(JJ)-P1(JJ))-P3(JJ)
       DNORM = DNORM + P4(JJ)**2
      ENDDO
      DNORM = SQRT(DNORM)
      DO JJ = 1,3
       P4(JJ)= P4(JJ)*R/DNORM + P3(JJ)
      ENDDO
      DIFF2=(P4(1)-XE(NS))**2 + (P4(2)-YE(NS))**2 + (P4(3)-ZE(NS))**2
      DIFF = SQRT(DIFF2) - RE(NS)
C
      IF(ABS(DIFF).LT.TOL) RETURN
C                          ******
      IF(I.EQ.0) THEN
       IF(DIFF.GT.0.0D+00) DELTA =  1.0D+00/(2.0D+00**(M+1))
       IF(DIFF.LT.0.0D+00) DELTA = -1.0D+00/(2.0D+00**(M+1))
       M = M + 1
       GO TO 10
      END IF
      IF(I.EQ.1) THEN
       IF(DIFF.GT.0.0D+00) DELTA = -1.0D+00/(2.0D+00**(M+1))
       IF(DIFF.LT.0.0D+00) DELTA =  1.0D+00/(2.0D+00**(M+1))
       M = M + 1
       GO TO 10
      END IF
C          the code probably never reaches this return
      RETURN
      END
C*MODULE PCMCAV  *DECK GAUBON
      SUBROUTINE GAUBON(NV,NS,PTS,CCC,PP,PP1,AREA,INTSPH,NUMTS)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXTS=2500, MXSP=250)
C
      DIMENSION PTS(3,10),CCC(3,10),PP(3),PP1(3),INTSPH(NUMTS,10)
      DIMENSION P1(3),P2(3),P3(3),U1(3),U2(3)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
C
      DATA PI/3.141592653589793D+00/
      DATA ZERO/0.0D+00/
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
      TPI=2*PI
C
C     Calcola la prima sommatoria
      SUM1 = ZERO
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
      IF(COSPHIN.LT.-1.0D+00) COSPHIN = -1.0D+00
      PHIN = ACOS(COSPHIN)
C
C     NSFE1 e' la sfera con cui la sfera NS si interseca (eventualmente)
        NSFE1 = INTSPH(NUMTS,N)
        X1 = XE(NSFE1) - XE(NS)
        Y1 = YE(NSFE1) - YE(NS)
        Z1 = ZE(NSFE1) - ZE(NS)
      DNORM1 = SQRT(X1*X1 + Y1*Y1 + Z1*Z1)
      IF(DNORM1.LT.1.0D-06) DNORM1 = 1.0D+00
C     IF(DNORM1.EQ.ZERO) DNORM1 = 1.0D+00
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
      SUM2 = ZERO
C     Loop sui vertici
      DO 200 N = 1, NV
      DO JJ = 1, 3
      P1(JJ) = ZERO
      P2(JJ) = ZERO
      P3(JJ) = ZERO
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
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
      P2(JJ) = P3(JJ)
      ENDDO
      CALL VECP(P1,P2,P3,DNORM3)
      DO JJ = 1, 3
      U1(JJ) = P3(JJ)/DNORM3
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
      BETAN = U1(1)*U2(1)+U1(2)*U2(2)+U1(3)*U2(3)
      IF(BETAN.GT. 1.0D+00) BETAN= 1.0D+00
      IF(BETAN.LT.-1.0D+00) BETAN=-1.0D+00
      BETAN = ACOS(BETAN)
C     BETAN = ACOS(U1(1)*U2(1)+U1(2)*U2(2)+U1(3)*U2(3))
      SUM2 = SUM2 + (PI - BETAN)
 200  CONTINUE
C     Calcola l'area della tessera
        AREA = RE(NS)*RE(NS)*(TPI + SUM1 - SUM2)
C     Trova il punto rappresentativo (come media dei vertici)
C     determine the center point of the tessera
C
      DO JJ = 1, 3
      PP(JJ) = ZERO
      ENDDO
      DO I = 1, NV
      PP(1) = PP(1) + (PTS(1,I)-XE(NS))
      PP(2) = PP(2) + (PTS(2,I)-YE(NS))
      PP(3) = PP(3) + (PTS(3,I)-ZE(NS))
      ENDDO
      DNORM = ZERO
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
      IF(AREA.LT.ZERO)THEN
        IF(NPRINT.NE.817)WRITE(IW,1000) NS,AREA
 1000   FORMAT(1X,'WARNING: THE AREA OF A TESSERA ON SPHERE ',I6,
     *  ' IS NEGATIVE (',E10.3,' A^2 ), THUS DISCARDED')
        AREA = ZERO
      END IF
C
C     IF(AREA.LT.1.0D-04.AND.METHOD(NS).EQ.1)THEN
C       IF(NPRINT.NE.817)WRITE(IW,1005) NS,AREA
C1005   FORMAT(1X,'WARNING: THE AREA OF A TESSERA ON SPHERE ',I6,
C    *  ' IS TOO SMALL (',E10.3,' A^2 )')
C     END IF
C
C          THE AREAS ARE NOT EQUAL FOR NTS=240 OR 960!
C     IF(AREA.GT.1.10*12.567*(RE(NS)**2)/DBLE(INITS(NS)))THEN
C       IF(NPRINT.NE.817)WRITE(IW,1010) NS,AREA
C1010   FORMAT(1X,'WARNING: THE AREA OF A TESSERA ON SPHERE ',I6,
C    *  ' IS TOO LARGE (',E10.3,' A^2 )')
C     END IF
      RETURN
      END
C*MODULE PCMCAV  *DECK VECP
      SUBROUTINE VECP(P1,P2,P3,DNORM3)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION P1(3),P2(3),P3(3)
C
C     Esegue il prodotto vettoriale P3 = P1 x P2
C
      P3(1) = P1(2)*P2(3) - P1(3)*P2(2)
      P3(2) = P1(3)*P2(1) - P1(1)*P2(3)
      P3(3) = P1(1)*P2(2) - P1(2)*P2(1)
      DNORM3 = SQRT(P3(1)*P3(1) + P3(2)*P3(2) + P3(3)*P3(3))
      RETURN
      END
C*MODULE PCMCAV  *DECK SEPARA
      SUBROUTINE SEPARA(ICAV1,ICAV2,NUMSPH,NCAV1,NCAV2)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION ICAV1(NUMSPH),ICAV2(NUMSPH)
C
      PARAMETER (MXTS=2500, MXSP=250)
C
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMAG / I_NESF,L_AST
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
C
C  QUESTA ROUTINE CONTROLLA SE IL SOLUTO E' CONTENUTO IN UNA
C  UNICA CAVITA' O IN CAVITA' DISTINTE: IN TAL CASO I NUMERI
C  D'ORDINE DELLE SFERE CHE COSTITUISCONO LA PRIMA E LA SECONDA
C  CAVITA' VENGONO MEMORIZZATI RISPETTIVAMENTE IN ICAV1 E ICAV2
C
      DO I=1,MXSP
        ICAV1(I)=0
        ICAV2(I)=0
      ENDDO
      NCAV1=0
      NCAV2=0
      N1=1
      N2=1
      NN=1
      ICAV1(N1)=1
      N1=N1+1
  50  CONTINUE
      DO I=1,MXSP
        ICAV2(I)=0
      ENDDO
C
      N2=1
      N=ICAV1(NN)
      L_CH=0
      M_CH=0
C
      DO 100 ICEN=1,L_AST
        L_CH=L_CH+1
        IF(ICEN.EQ.N) GO TO 100
        DO I=1,L_AST
          IF(ICEN.EQ.ICAV1(I)) GO TO 100
        ENDDO
        M_CH=M_CH+1
        X=XE(N)-XE(ICEN)
        XX=X*X
        Y=YE(N)-YE(ICEN)
        YY=Y*Y
        Z=ZE(N)-ZE(ICEN)
        ZZ=Z*Z
        RR=XX+YY+ZZ
        R=SQRT(RR)
        SUM=RE(N)+RE(ICEN)
        IF(SUM.GT.R) THEN
          ICAV1(N1)=ICEN
          N1=N1+1
          ELSE
          ICAV2(N2)=ICEN
          N2=N2+1
        END IF
 100  CONTINUE
      NN=NN+1
      IF(ICAV1(NN).NE.0) GO TO 50
      NCAV1=NN-1
      NCAV2=L_AST-NCAV1
C
C         this is Hui Li's double check if there's really two cavities
C
      NNCAV2=NCAV2
      DO I = 1,NCAV1
         DO J = 1,NCAV2
            IF(ICAV2(J).EQ.ICAV1(I)) NNCAV2=NNCAV2-1
         ENDDO
      ENDDO
      NCAV2=NNCAV2
C
      IF(MASWRK) THEN
         IF(NCAV2.EQ.0) THEN
            WRITE(IW,200)
         ELSE
            WRITE(IW,300) NCAV1,NCAV2
            WRITE(IW,400)
            WRITE(IW,410) (ICAV1(I),I=1,NCAV1)
            WRITE(IW,500)
            WRITE(IW,410) (ICAV2(I),I=1,NCAV2)
         END IF
      END IF
      RETURN
C
  200 FORMAT(/10X,'THE SOLUTE IS ENCLOSED IN ONE CAVITY')
  300 FORMAT(/10X,'THE SOLUTE IS ENCLOSED IN TWO DISTINCT CAVITIES'/
     *        10X,'OF',I4,' AND',I4,' SPHERE(S), RESPECTIVELY')
  400 FORMAT(/10X,'THE FIRST CAVITY IS FORMED BY SPHERE(S) :'/)
  410 FORMAT(1X,10I6)
  500 FORMAT(/10X,'THE SECOND CAVITY IS FORMED BY SPHERE(S) :'/)
      END
C*MODULE PCMCAV  *DECK CAVITM
      SUBROUTINE CAVITM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXATM=500, MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMTX,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /FMCOM / XX(1)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMCAV/ OMEGA,RET,FRO,ALPHA(MXSP),RIN(MXSP),ICENT,
     *                IPRINT,IRETCAV
      COMMON /PCMCHG/ QSN(MXTS),QSE(MXTS),PB,PX,PC,UNZ,QNUC,FN,FE,
     *                Q_FS(MXTS),Q_IND(MXTS)
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMABC/ AXYZ(3),BXYZ(3),CXYZ(3),NAB,NAC
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),RE(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,NESF,NESFP,NC(30),NESFF
      DATA ZERO/0.0D+00/
C
C     this routine is called for final PCM computations:
C       a) optional reaction field due to the apparent surface charges.
C       b) optional computation of the cavitation energy.
C
C-----------------------------------------------------
C                        INPUT
C
C   COORD.(in ANG) OF HIGHER LEFT CORNER OF THE MAP
C     AXYZ(1),AXYZ(2),AXYZ(3)
C   COORD.(in ANG) OF LOWER LEFT CORNER OF THE MAP
C     BXYZ(1),BXYZ(2),BXYZ(3)
C   COORD.(in ANG) OF HIGHER RIGHT CORNER OF THE MAP
C     CXYZ(1),CXYZ(2),CXYZ(3)
C   VERTICAL (NAB), AND ORIZONTAL(NAC) SUBDIVISION
C     NAB,NAC
C-----------------------------------------------------
C
      IF(IFIELD.EQ.2) THEN
         NAB1=NAB + 1
         NAC1=NAC + 1
         MXP=NAB1*NAC1
      ELSE
         MXP=1
      ENDIF
C
      CALL VALFM(LOADFM)
      LCEX  = LOADFM + 1
      LCEY  = LCEX   + MXP
      LCEZ  = LCEY   + MXP
      LPTZ  = LCEZ   + MXP
      LCEI  = LPTZ   + MXP
      LRTX  = LCEI   + MXP
      LRTY  = LRTX   + MXP
      LRTZ  = LRTY   + MXP
      LAST  = LRTZ   + MXP
      NEED = LAST - LOADFM - 1
      IF(IFIELD.EQ.2 .AND. MASWRK) WRITE(IW,9010) NEED
      CALL GETFM(NEED)
C
C-----------------------------------------------------------------------
C     REACTION FIELD AND REACTION POTENTIAL
C-----------------------------------------------------------------------
C
      IF(IPRINT.GE.1) THEN
         IF(MASWRK)WRITE(IW,1400)
         DO 130 I=1,NTS
            L=ISPHE(I)
            IF(MASWRK)WRITE(IW,1450)
     *       L,I,XCTS(I),YCTS(I),ZCTS(I),QSE(I)+QSN(I)
  130    CONTINUE
      END IF
C
C     ----- ON THE NUCLEI ------
C
      IF(IFIELD.EQ.1) THEN
         IF(MASWRK)WRITE(IW,1500)
         DO 150 I=1,NAT
            IF(ZAN(I).EQ.0) GO TO 150
            CEXA=ZERO
            CEYA=ZERO
            CEZA=ZERO
            PTZA=ZERO
            DO 140 J=1,NTS
               QS=QSE(J)+QSN(J)
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
            CEIA=SQRT(CEXA**2+CEYA**2+CEZA**2)
            IF(MASWRK)WRITE(IW,1600) I,ZAN(I),CEXA,CEYA,CEZA,CEIA,PTZA
  150    CONTINUE
      END IF
C
C     ---- REACTION FIELD ON a grid of points ----
C
      IF(IFIELD.EQ.2) THEN
C
         CALL RTIC(XX(LRTX),XX(LRTY),XX(LRTZ),NAB1,NAC1,MXP)
C
         DO 200 J=1,MXP
            XX(LCEX-1+J)=ZERO
            XX(LCEY-1+J)=ZERO
            XX(LCEZ-1+J)=ZERO
            XX(LPTZ-1+J)=ZERO
            DO 190 I=1,NTS
               QS=QSE(I)+QSN(I)
               XU=XCTS(I)-XX(LRTX-1+J)
               YU=YCTS(I)-XX(LRTY-1+J)
               ZU=ZCTS(I)-XX(LRTZ-1+J)
               R1=SQRT((XU**2+YU**2+ZU**2)**3)
               R2=SQRT(XU**2+YU**2+ZU**2)
               XX(LCEX-1+J)=XX(LCEX-1+J)+QS*XU/R1
               XX(LCEY-1+J)=XX(LCEY-1+J)+QS*YU/R1
               XX(LCEZ-1+J)=XX(LCEZ-1+J)+QS*ZU/R1
               XX(LPTZ-1+J)=XX(LPTZ-1+J)+QS/R2
  190       CONTINUE
         XX(LCEI-1+J) = SQRT(XX(LCEX-1+J)**2+
     *                       XX(LCEY-1+J)**2+
     *                       XX(LCEZ-1+J)**2)
  200    CONTINUE
C
        IF(IPRINT.GE.1) THEN
          DO I=1,MXP
           IF(MASWRK)WRITE(IW,6070)
     *       XX(LRTX-1+I),XX(LRTY-1+I),XX(LRTZ-1+I),
     *       XX(LCEX-1+I),XX(LCEY-1+I),XX(LCEZ-1+I),XX(LCEI-1+I),
     *       XX(LPTZ-1+I)*627.50959D+00
          ENDDO
        END IF
      END IF
      CALL RETFM(NEED)
C
C         generate cavitation energy
C
      IF(ICAV.EQ.1) THEN
         IF(MASWRK)WRITE(IW,9000)
         CALL CAVIT
      ENDIF
      RETURN
C
 1400 FORMAT(/,' SFERA,TESSERA,X,Y,Z,CARICA')
 1450 FORMAT(I3,2X,I4,3F10.6,2X,F10.6)
 1500 FORMAT(//1X,'PCM ELECTRIC REACTION FIELD AND REACTION POTENTIAL',
     *            ' AT THE NUCLEI'/
     *         1X,'ATOM/NUC.CHARGE     FIELD COMPONENTS (X,Y,Z)       ',
     *            'STRENGTH   POTENTIAL (A.U.)')
 1600 FORMAT(1X,I5,F6.2,5F12.6)
 6070 FORMAT(//,' SITE ',3F10.5,/,'   REACTION FIELD COMPONENTS '
     *,3F7.3,' INTENSITY ',F10.5,' REACT. POTENTIAL ',F7.3)
 9000 FORMAT(/10X,17("-")/10X,'CAVITATION ENERGY'/10X,17("-")/)
 9010 FORMAT(1X,'IFIELD.GT.0 COMPUTATION USES',I10,' WORDS.')
C
      END
C*MODULE PCMCAV  *DECK CAVIT
      SUBROUTINE CAVIT
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER (MXTS=2500, MXTSPT=2*MXTS, MXSP=250)
C
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /OUTPUT/ NPRINT,ITOL,ICUT,NORMF,NORMP,NOPK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMCAV/ OMEGA,RET,FRO,ALPHA(MXSP),RIN(MXSP),ICENT,
     *                IPRINT,IRETCAV
      COMMON /PCMDAT/ EPS,EPSINF,DR,RSOLV,VMOL,TCE,STEN,DSTEN,
     *                CMF,TABS,ICOMP,IFIELD,ICAV,IDISP
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP),SRD(MXSP),SSFE(MXSP),
     *                ISPHE(MXTS),STOT,VOL,N,NESFP,NC(30),NESFF
      COMMON /PCMPRT/ GCAVP,GCAVS,GDISP,GREP,EHFGAS
      COMMON /PCMTES/ CCX,CCY,CCZ,XCTS(MXTSPT),YCTS(MXTSPT),
     *                ZCTS(MXTSPT),AS(MXTS),RDIF,NVERT(MXTS),NTS
C
      PARAMETER (TOANGS=0.52917724924D+00, ANTOAU=1.0D+00/TOANGS)
C
      DATA PT5,ONE,TWO/0.5D+00,1.0D+00,2.0D+00/
      DATA GC,AVGDR,CEKM/1.9865D+00,0.60228D+00,0.0014389D+00/
      DATA PI/  3.141592653589793D+00 /
C
      FPI=4*PI
C
C     CAVITATION FREE ENERGY
C
      DMOL = 2.0D+00 * RSOLV
C-----------------------------------------------------------------------
C   CAVITATION ENERGY:    R.A. PIEROTTI
C                         CHEM. REVIEW, 76,717,(1976).
C-----------------------------------------------------------------------
C     Creates a new cavity with the initial spheres for the
C     cavitation energy calculation
C
      IF(MASWRK.AND.NPRINT.NE.817)WRITE(IW,1000)
      IRETCAV = 1
      CALL PEDFSM
      IF(MASWRK.AND.NPRINT.NE.817)THEN
      WRITE(IW,*)
      WRITE(IW,*) ' ************************************'
      WRITE(IW,*) '        CAVITATION FREE ENERGY'
      WRITE(IW,*) ' ************************************'
      END IF
C
C     A single cavity
C
      DCAV=EXP(LOG(6.0D+00*VOL/PI)/3.0D+00)
      TABS2=TABS**2
      DENUM=AVGDR/VMOL
      YP=DENUM*PI*DMOL**3/6.0D+00
      YPM1=ONE-YP
      RP=DCAV/DMOL
      ECF=TCE*GC*TABS2*YP/(YPM1**3)
      EC=ECF*(YPM1**2+3.0D+00*YPM1*RP
     *               +3.0D+00*(ONE+TWO*YP)*RP*RP)/1000.0D+00
      SIG=(DCAV+DMOL)*PT5
      RAP=YP/YPM1
      RAP2=RAP**2
      PK0=-LOG(YPM1)+4.5D+00*RAP2
      PK1=-(6.0D+00*RAP+18.0D+00*RAP2)/DMOL
      PK2=(12.0D+00*RAP+18*RAP2)/DMOL**2
      ELC=(PK2*SIG+PK1)*SIG+PK0
      ELC=ELC*GC*TABS/1000.0D+00
      IF(MASWRK.AND.NPRINT.NE.817)WRITE (IW,1100)
     *        ' VOLUME',ELC,EC,TCE,DENUM,DMOL,DCAV
      DCAV=SQRT(STOT/PI)
      TABS2=TABS**2
      DENUM=AVGDR/VMOL
      YP=DENUM*PI*DMOL**3/6.0D+00
      YPM1=ONE-YP
      RP=DCAV/DMOL
      ECF=TCE*GC*TABS2*YP/(YPM1**3)
      EC=ECF*(YPM1**2+3.0D+00*YPM1*RP
     *               +3.0D+00*(ONE+TWO*YP)*RP*RP)/1000.0D+00
      SIG=(DCAV+DMOL)*PT5
      RAP=YP/YPM1
      RAP2=RAP**2
      PK0=-LOG(YPM1)+4.5D+00*RAP2
      PK1=-(6.0D+00*RAP+18.0D+00*RAP2)/DMOL
      PK2=(12.0D+00*RAP+18*RAP2)/DMOL**2
      ELC=(PK2*SIG+PK1)*SIG+PK0
      ELC=ELC*GC*TABS/1000.0D+00
      IF(MASWRK.AND.NPRINT.NE.817)WRITE (IW,1100)
     *   'SURFACE',ELC,EC,TCE,DENUM,DMOL,DCAV
C
      ELCPIER = 0.0D+00
      DO 100 I=1,N
         RSFE = SRD(I) / ANTOAU
         SUPEFF = 0.0D+00
         DO J=1,NTS
            L = ISPHE(J)
            IF (L.EQ.I) THEN
               AREATS = AS(J) / (ANTOAU*ANTOAU)
               SUPEFF = SUPEFF + AREATS
            END IF
         ENDDO
         AREATOT = FPI * RSFE * RSFE
         FRAZ = SUPEFF / AREATOT
         DCAV=2.0D+00 * RSFE
         TABS2=TABS**2
         DENUM=AVGDR/VMOL
         YP=DENUM*PI*DMOL**3/6.0D+00
         YPM1=ONE-YP
         RP=DCAV/DMOL
         ECF=TCE*GC*TABS2*YP/(YPM1**3)
         EC=ECF*(YPM1**2+3.0D+00*YPM1*RP
     *                  +3.0D+00*(ONE+TWO*YP)*RP*RP)/1000.0D+00
         SIG=(DCAV+DMOL)*PT5
         RAP=YP/YPM1
         RAP2=RAP**2
         PK0=-LOG(YPM1)+4.5D+00*RAP2
         PK1=-(6.0D+00*RAP+18.0D+00*RAP2)/DMOL
         PK2=(12.0D+00*RAP+18*RAP2)/DMOL**2
         ELC=(PK2*SIG+PK1)*SIG+PK0
         ELCPI=ELC*GC*TABS/1000.0D+00
         ELCPIER = ELCPIER + ELCPI * FRAZ
  100 CONTINUE
      IF(MASWRK.AND.NPRINT.NE.817)WRITE (IW,1200)
     *  ELCPIER,EC,TCE,DENUM,DMOL
      GCAVP = ELCPIER
C
 1000 FORMAT(/1X,'CREATION OF A NEW CAVITY WITH THE ORIGINAL SPHERE'/
     *       1X,'SIZES FOR THE COMPUTATION OF THE CAVITATION ENERGY.')
 1100 FORMAT(/' PIEROTTI (SINGLE SPHERE, RADIUS FROM TOTAL ',A7,'):',
     * F10.5,' KCAL/MOL'/
     * ' CAVITATION ENERGY (ENTHALPY) =',F10.5,' KCAL/MOL'/
     * ' THERMAL EXPANSION COEF. =',F14.6/
     * ' NUMERAL DENSITY =',F22.6/
     * ' SOLVENT MOLECULE DIAMETER =',F12.6,' ANG.'/
     * ' CAVITY DIAMETER =',F22.6,' ANG.')
 1200 FORMAT(/' CLAVERIE-PIEROTTI (SPHERE BY SPHERE, SURFACE):',F10.5,
     * '  KCAL/MOL'/
     * ' CAVITATION ENERGY (ENTHALPY) =',F10.5,' KCAL/MOL'/
     * ' THERMAL EXPANSION COEF. =',F14.6/
     * ' NUMERAL DENSITY =',F22.6/
     * ' SOLVENT MOLECULE DIAMETER =',F12.6,' ANG.')
C
C-----------------------------------------------------------------------
C    CAVITATION ENERGY:  BIRNSTOCK, HOFMANN, KOHLER
C                             T.C.A. 42,311,(1976)
C-----------------------------------------------------------------------
C
      DCAV=SQRT(STOT/PI)
      RP=DCAV/DMOL
      CMFM=(CMF-ONE)/RP**2+ONE
      ELCSIN=STEN*CMFM*STOT*CEKM
      EC=ELCSIN*(ONE+DSTEN-TWO*TABS*TCE/3.0D+00)
      IF(MASWRK.AND.NPRINT.NE.817)WRITE (IW,1300)
     *  ELCSIN,EC,CMF,STEN,DSTEN,TABS,DCAV
      GCAVS = ELCSIN
C
1300  FORMAT(/' SINANOGLU (SINGLE SPHERE, RADIUS FROM TOTAL SURFACE):',
     * F10.5,' KCAL/MOL'/
     * ' CAVITATION ENERGY (ENTHALPY) =',F12.5,' KCAL/MOL'/
     * ' MICROSCOPIC CAVITY FACTOR =',F12.6/
     * ' SURFACE TENSION =',F22.6/
     * ' SURFACE TENSION DERIVATIVE = ',F10.6/
     * ' ABSOLUTE TEMPERATURE =',F17.6/
     * ' CAVITY DIAMETER =',F22.6,' ANG.')
      RETURN
      END
C*MODULE PCMCAV  *DECK RTIC
      SUBROUTINE RTIC(RTX,RTY,RTZ,NAB1,NAC1,MXP)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      DIMENSION RTX(MXP),RTY(MXP),RTZ(MXP)
C
      LOGICAL GOPARR, DSKWRK, MASWRK
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /PCMABC/ AXYZ(3),BXYZ(3),CXYZ(3),NAB,NAC
C
      PARAMETER (TOANGS=0.52917724924D+00, ANTOAU=1.0D+00/TOANGS)
C
C
      AX=AXYZ(1)
      AY=AXYZ(2)
      AZ=AXYZ(3)
      BX=BXYZ(1)
      BY=BXYZ(2)
      BZ=BXYZ(3)
      CX=CXYZ(1)
      CY=CXYZ(2)
      CZ=CXYZ(3)
      DXC=(CX-AX)/NAC
      DYC=(CY-AY)/NAC
      DZC=(CZ-AZ)/NAC
      DXB=(BX-AX)/NAB
      DYB=(BY-AY)/NAB
      DZB=(BZ-AZ)/NAB
C
      K=0
      DO 210 J=1,NAB1
         DO 200 I=1,NAC1
            K=K+1
            IF(K.GT.MXP .AND. MASWRK) THEN
               WRITE(IW,*) ' RTIC: EXCEEDED -MXP- DIMENSION'
               CALL ABRT
               STOP
            END IF
            RTX(K)=(AX + (I-1)*DXC + (J-1)*DXB)*ANTOAU
            RTY(K)=(AY + (I-1)*DYC + (J-1)*DYB)*ANTOAU
            RTZ(K)=(AZ + (I-1)*DZC + (J-1)*DZB)*ANTOAU
  200    CONTINUE
  210 CONTINUE
      RETURN
      END
