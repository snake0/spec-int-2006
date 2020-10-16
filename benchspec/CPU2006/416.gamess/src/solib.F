C  9 dec 03 - mws - synch common block runopt
C 15 Jul 03 - MWS - include changes dated 10-Nov-01 to 23-Jul-02 below
C 23 Jul 02 - DGF - rewrite Jz matrix computation
C 29 May 02 - DGF - add omega analysis for diatomics
C  1 Mar 02 - DGF - add Lz analysis for diatomics
C 23 Nov 01 - TN  - changes for DK
C 10 Nov 01 - DGF - implement MCP 2e core-active SOC integrals
C 19 Sep 01 - MWS - convert mxaoci paramter to mxao
C  6 Sep 01 - HU  - use maswrk for writing to punch file
C  1 Aug 01 - DGF - SOINTS,TMOINT: don't reuse ints at other geometries
C                   ADDZERO,ADD0MP,SOMP2OUT: printing changes
C 13 Jun 01 - DGF - support internally uncontracted one elec SOC RESC
C 10 May 01 - DGF - new module to relocate commonly used SOC routines
C
C*MODULE SOLIB   *DECK ADD0MP
      SUBROUTINE ADD0MP(NHSO,HSO,EIG,ENGYST,HMP2,MULST,IROOTS,NZSPIN,
     *                  EREF)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL GOPARR,DSKWRK,MASWRK
      PARAMETER (XKIZER=219478.36857D+00,ZERO=0.0D+00)
C                       27.20967*8066.19
      COMPLEX*16 HSO(NHSO,NHSO),HIJ
C
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      DIMENSION ENGYST(MXRT,*),HMP2(MXRT,MXRT,*),EIG(*),MULST(*),
     *          IROOTS(*)
C
C     Fill SO-MCQDPT Hamiltonian with spin-free MCQDPT Hamiltonian
C     matrix elements, shifted by the MCQDPT ground state energy.
C
      ICIREF=NZSPIN/1000
      IRTREF=MOD(NZSPIN,1000)
      IF(NZSPIN.EQ.0) ICIREF=1
      IF(NZSPIN.EQ.0) IRTREF=1
      IF(ICIREF.GT.NUMCI.OR.ICIREF.LT.1.OR.
     *   IRTREF.GT.IROOTS(ICIREF).OR.IRTREF.LT.1) THEN
         IF(MASWRK) THEN
            WRITE(IW,*) 'ILLEGAL NZSPIN',NZSPIN,ICIREF,IRTREF
            WRITE(IW,*) 'ILLEGAL NZSPIN',NUMCI,IROOTS(ICIREF)
         END IF
         CALL ABRT
      ENDIF
C
C     IF(MASWRK) WRITE(IW,2) IRTREF,ICIREF,NZSPIN
C   2 FORMAT(/1X,'REFERENCE STATE IS SET TO ',I4,' IN THE DRT NO.',
C    *       I2,' (',I6,').')
C
      EREF=ENGYST(IRTREF,ICIREF)
      IEIG=0
      ISHIFT=0
      DO 200 ICI=1,NUMCI
C       write(6,*) ici,'MP2 Hamiltonian is',IROOTS(ICI)
C       call prsq(hmp2(1,1,ici),IROOTS(ICI),IROOTS(ICI),mxrt)
        ISHIFT0=ISHIFT
        DO 180 I=1,IROOTS(ICI)
          JSHIFT=ISHIFT0
          DO 150 J=1,IROOTS(ICI)
            EREF0=ZERO
            IF(I.EQ.J) EREF0=EREF
            HIJ=DCMPLX((HMP2(I,J,ICI)-EREF0)*XKIZER,ZERO)
            DO 100 MS=1,MULST(ICI)
              HSO(ISHIFT+MS,JSHIFT+MS)=HIJ
  100       CONTINUE
            JSHIFT=JSHIFT+MULST(ICI)
  150     CONTINUE
          DO MS=1,MULST(ICI)
            IEIG=IEIG+1
            EIG(IEIG)=(ENGYST(I,ICI)-EREF)*XKIZER
          ENDDO
          ISHIFT=ISHIFT+MULST(ICI)
  180   CONTINUE
  200 CONTINUE
      IF(MASWRK) WRITE(IP,*) ' MCQDPT SPIN-FREE HAMILTONIAN'
      CALL ZPUSQL(HSO,NHSO,NHSO,NHSO)
      RETURN
      END
C*MODULE SOLIB   *DECK ADDZERO
      SUBROUTINE ADDZERO(NHSO,HSO,EIG,ENGYST,MULST,IROOTS,NZSPIN,EREF)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (XKIZER=219478.36857D+00,ZERO=0.0D+00)
C                       27.20967*8066.19
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMPLEX*16 HSO(NHSO,NHSO)
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      DIMENSION ENGYST(MXRT,*),EIG(*),MULST(*),IROOTS(*)
C
      ICIREF=NZSPIN/1000
      IRTREF=MOD(NZSPIN,1000)
      IF(NZSPIN.EQ.0) ICIREF=1
      IF(NZSPIN.EQ.0) IRTREF=1
      IF(ICIREF.GT.NUMCI.OR.ICIREF.LT.1.OR.
     *   IRTREF.GT.IROOTS(ICIREF).OR.IRTREF.LT.1) THEN
         IF(MASWRK) THEN
            WRITE(IW,*) 'ILLEGAL NZSPIN',NZSPIN,ICIREF,IRTREF
            WRITE(IW,*) 'ILLEGAL NZSPIN',NUMCI,IROOTS(ICIREF)
         END IF
         CALL ABRT
      ENDIF
      IF(MASWRK) WRITE(IW,9062) IRTREF,ICIREF,NZSPIN
      EREF=ENGYST(IRTREF,ICIREF)
      ISHIFT=0
      DO 200 ICI=1,NUMCI
         DO 200 I=1,IROOTS(ICI)
            DO 200 MS=1,MULST(ICI)
             ISHIFT=ISHIFT+1
             EIG(ISHIFT)=(ENGYST(I,ICI)-EREF)*XKIZER
             HSO(ISHIFT,ISHIFT)=DCMPLX(EIG(ISHIFT),ZERO)
C            write(iw,*) me,':',ici,i,ms,hso(ishift,ishift)
  200 CONTINUE
      RETURN
 9062 FORMAT(/1X,'REFERENCE STATE IS SET TO ',I4,' IN THE DRT NO.',
     *       I2,' (',I6,').')
      END
C*MODULE SOLIB   *DECK ASSORCI
      SUBROUTINE ASSORCI(ISTSYM,NCIOR,IRCIOR,IROOTS)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION ISTSYM(MXRT,*),NCIOR(NIRRED+1,NUMCI),IRCIOR(MXRT,NUMCI),
     *          IROOTS(*)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      COMMON /SYMBLK/ NIRRED,NSALC,NSALC2,NSALC3
C
C     assign labels for iprhso==0
C
      CALL VICLR(NCIOR,1,(NIRRED+1)*NUMCI)
      DO 200 ICI=1,NUMCI
         DO 100 IRT=1,IROOTS(ICI)
            IRR=ISTSYM(IRT,ICI)
            IF(IRR.GT.NIRRED.OR.IRR.LE.0) IRR=NIRRED+1
C           the last one is reserved for undetermined/dirty symmetry
            NCIOR(IRR,ICI)=NCIOR(IRR,ICI)+1
            IRCIOR(IRT,ICI)=NCIOR(IRR,ICI)
C           for degenerate irreps the numbering is consequent:
C           1,2, 3,4, ... for the first two 2-fold states
  100    CONTINUE
  200 CONTINUE
      RETURN
      END
C*MODULE SOLIB   *DECK CACHEVEC
      SUBROUTINE CACHEVEC(NFT,N,A,INCA,NCHUNK)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(INCA,N)
C
C     cache an array a(n) onto a disk file with chunks of size nchunk
C
      IF(N.EQ.0) RETURN
      IF(NCHUNK.EQ.0) CALL ABRT
      CALL SEQREW(NFT)
      NDONE=0
100   CONTINUE
         NSAVE=MIN(NCHUNK,N-NDONE)
         WRITE(NFT) (A(1,NDONE+I),I=1,NSAVE)
         NDONE=NDONE+NSAVE
      IF(NDONE.LT.N) GOTO 100
      RETURN
      END
C*MODULE SOLIB   *DECK CACHEDAXPY
      SUBROUTINE CACHEDAXPY(NFT,N,DA,Y,INCY,BUF,NCHUNK)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Y(INCY,N),BUF(NCHUNK)
C
C     perform a daxpy with y = y + da * x
C     where x should be cached on disk with chunk size nchunk
C     arrays x(n) and y(n)
C
      IF(N.EQ.0) RETURN
      IF(NCHUNK.EQ.0) CALL ABRT
      CALL SEQREW(NFT)
      NDONE=0
100   CONTINUE
         NREAD=MIN(NCHUNK,N-NDONE)
         READ(NFT) (BUF(I),I=1,NREAD)
         CALL DAXPY(NREAD,DA,BUF,1,Y(1,NDONE+1),INCY)
         NDONE=NDONE+NREAD
      IF(NDONE.LT.N) GOTO 100
      RETURN
      END
C*MODULE SOLIB   *DECK CISOL
      SUBROUTINE CISOL(CISOLE,IGP,MS,IGP1,IRRL,LINXY)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL CISOLE
      PARAMETER (MAXIRR=14)
      COMMON /SYMMUL/ NIJREP(14,14),IJREP(2,14,14,14)
C     COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      DIMENSION LGAM(2),IGAR(MAXIRR),IGAR1(MAXIRR),IRRL(3)
C
C     determines if direct product Gamma X Gamma ms X Gamma' contains
C     the totally symmetric irrep, Gamma ms being irreps of Lms
C     this will need to be *rewritten* if groups like C3 will recover
C     their Abelian status (may it happen soon).
C
C     There is a big difference between Zeff and PB, namely
C     Zeff calculates directly Lx and Ly, whereas PB calculates
C     directly Lx+iLy and Lx-iLy. The selection rules therefore
C     be somewhat different.
C     Note that even though Zeff persues to calculate Lx/Ly it does so
C     while getting at the matrix element of L+/L- anyway.
C
C               linxy=0       linxy=1
C     ms = -1     x            x-iy (actually x and y)
C           0     z             z
C           1     y            x+iy (actually x and y)
C
      IF(MS.EQ.0) THEN
         NLGAM=1
         LGAM(1)=IRRL(3)
      ELSE
         IF(LINXY.NE.0) THEN
            NLGAM=2
            LGAM(1)=IRRL(1)
            LGAM(2)=IRRL(2)
C        again, alas poor Yorick. GAMESS does not separate x+iy/x-iy
C        and lumps both into one 2-fold irrep for groups like C3
         ELSE
            NLGAM=1
            IF(MS.EQ.-1) LGAM(1)=IRRL(1)
            IF(MS.EQ. 1) LGAM(1)=IRRL(2)
         ENDIF
      ENDIF
C
C     unpack the arrays of irreps
C
      CALL IRRUNP(IGP,IGAR,NGAM)
      CALL IRRUNP(IGP1,IGAR1,NGAM1)
C     write(iw,*) 'unpacked',ngam,(iGar(i),i=1,ngam)
C     write(iw,*) 'unpacked1',ngam1,(iGar1(i),i=1,ngam1)
C
      DO 250 I=1,NGAM
         IG=IGAR(I)
         DO 240 I1=1,NGAM1
            IG1=IGAR1(I1)
            IF(IG.EQ.0.OR.IG1.EQ.0) THEN
C              if the symmetry of CI roots be unbeknownst
               CISOLE=.TRUE.
               RETURN
            ENDIF
            DO 230 IMS=1,NLGAM
               IGMS=LGAM(IMS)
               NN=NIJREP(IGMS,IG1)
               DO 220 IGG=1,NN
C           2 refers to the label, (1 to the number of occurances)
                  IF(IJREP(2,IGG,IGMS,IG1).EQ.IG) THEN
C                    write(iw,*) 'dolbug',igms,'x',ig1,'=',
C    *                           ijrep(2,igg,igms,ig1),'x',ig
                     CISOLE=.TRUE.
                     RETURN
                  ENDIF
  220          CONTINUE
  230       CONTINUE
  240    CONTINUE
  250 CONTINUE
      CISOLE=.FALSE.
      RETURN
      END
C*MODULE SOLIB   *DECK F17PNT
      SUBROUTINE F17PNT(NFT17,ICI1,IW,NWKSST)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
      CHARACTER*8 DRTNAM,CDRT
      EQUIVALENCE (CDRT,DRTNAM1)
      DIMENSION NWKSST(*)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C     Structure of NFT17:
C     1. wrote drtnam
C     2. wrote CI information at a time.
C
      CALL SEQREW(NFT17)
      IF(ICI1.GT.1) THEN
         DO 100 I=1,(ICI1-1)
            READ(NFT17,ERR=900,END=900) DRTNAM
C           write(iw,210) drtnam,nft17
C 210 FORMAT(/10X,'SKIP DATA OF ',A8,' FROM NFT',I2,'.')
            DO 120 J=1,NWKSST(I)
               READ(NFT17,ERR=901,END=901) IOPEN
  120          CONTINUE
  100       CONTINUE
         ENDIF
      READ(NFT17,ERR=902,END=902) DRTNAM
C     write(iw,200) drtnam,nft17
C 200 FORMAT(/10X,'READ DATA OF ',A8,' FROM NFT',I2,'.')
      RETURN
C
  900 CONTINUE
         IF(I.LE.9) THEN
            WRITE(UNIT=CDRT(1:8),FMT='(A3,I1)') 'DRT',I
         ELSE IF(I.LE.99) THEN
            WRITE(UNIT=CDRT(1:8),FMT='(A3,I2)') 'DRT',I
         ELSE
            WRITE(UNIT=CDRT(1:8),FMT='(A3,I3)') 'DRT',I
         ENDIF
      IF (MASWRK) WRITE(IW,910) I,DRTNAM1,DRTNAM
      CALL ABRT
  901 CONTINUE
      IF (MASWRK) WRITE(IW,911) I,IOPEN
      CALL ABRT
  902 CONTINUE
      IF(ICI1.LE.9) THEN
         WRITE(UNIT=CDRT(1:8),FMT='(A3,I1)') 'DRT',ICI1
      ELSE IF(ICI1.LE.99) THEN
         WRITE(UNIT=CDRT(1:8),FMT='(A3,I2)') 'DRT',ICI1
      ELSE
         WRITE(UNIT=CDRT(1:8),FMT='(A3,I3)') 'DRT',ICI1
      ENDIF
      IF (MASWRK) WRITE(IW,910) ICI1,DRTNAM1,DRTNAM
      CALL ABRT
      STOP
C
  910 FORMAT(//1X,'ERROR: FAIL TO FIND THE',I3,'-TH CI INFORMATION.',
     *       //1X,'       ID =',A8,//15X,A8,' WAS FOUND.')
  911 FORMAT(//1X,'ERROR: FAIL TO READ THE',I3,'-TH CI DATA.',
     *       //1X,'       IOPEN =',I5)
      END
C*MODULE SOLIB   *DECK GCISOL
      SUBROUTINE GCISOL(GCISOLE,ISTSYM,MS,ICI1,ICI2,IROOTS,IZEROT,
     *                  IDIAG,IRRL,LINXY)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IROOTS(*),ISTSYM(MXRT,NUMCI),IRRL(3)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      LOGICAL GCISOLE,CISOLE
C
C     determine if all states of a given pair of multiplicities do not
C     interact (forbidden by space symmetry). If so, form factors are
C     not needed (for a given ms)!
C     returns true if there is a non-zero matrix element
C     izerot gives the number of zero matrix elements using symmetries
C     of Lms.
C     idiag indicates if the diagonal elements (ici1==ici2,i=j) must be
C     considered, that is, idiag=0 for <R> and idiag=1 for <Hso>
C
      DO 140 I=1,IROOTS(ICI1)
         IF(ICI1.NE.ICI2) THEN
            MINJ=1
         ELSE
            MINJ=I+IDIAG
         ENDIF
         DO 130 J=MINJ,IROOTS(ICI2)
          CALL CISOL(CISOLE,ISTSYM(I,ICI1),MS,ISTSYM(J,ICI2),IRRL,LINXY)
            IF(CISOLE) THEN
               GCISOLE=.TRUE.
               IZEROT=0
               RETURN
            ENDIF
  130    CONTINUE
  140 CONTINUE
      GCISOLE=.FALSE.
      IF(ICI1.EQ.ICI2) THEN
         IZEROT=(IROOTS(ICI1)*(IROOTS(ICI1)-1))/2
      ELSE
         IZEROT=IROOTS(ICI1)*IROOTS(ICI2)
      ENDIF
      RETURN
      END
C*MODULE SOLIB   *DECK GETSTSYM
      SUBROUTINE GETSTSYM(ISTSYM,JSOD,IROOTS)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MAXCP=4096)
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /SOGUG/  CP(MAXCP),NUNIQ,LIOBP,ISODAF,NSODA,LSTREC,JSODAF,
     *                JSODA,NRECJ
      DIMENSION ISTSYM(MXRT,NUMCI),IROOTS(NUMCI),JSOD(*)
C
      DO 100 ICI=1,NUMCI
         CALL RAREAD(JSODAF,JSOD,ISTSYM(1,ICI),(IROOTS(ICI)-1)/NWDVAR+1,
     *               (ICI-1)*NRECJ+6,1)
C        write(6,*) 'readingg',me,iroots(ici),ici,':'
C        write(6,*) me,(istsym(i,ici),i=1,iroots(ici))
Cnb      call fixinf(istsym(1,ici),iroots(ici),engyst(1,ici),ici)
C     linear molecule/atom degenerate irrep fix
C     not only that, but a thetan can specify low symmetry even for
C     groups with Cnz, n>2
  100 CONTINUE
Cnb
C     for some reason the state symmetries are not saved correctly
C     on slave nodes. While this may or may not be a good idea,
C     broadcast for now
      IF(GOPARR) CALL DDI_BCAST(2309,'I',ISTSYM,MXRT*NUMCI,MASTER)
      RETURN
      END
C*MODULE SOLIB   *DECK HSORES
      SUBROUTINE HSORES(NHSO,HSO,EIG,RWORK,WORK,IROOTS,MULST,ENGYST,INDX
     *                 ,MAXNCO,COPCON,COPCON1,PRTPRM,NZSPIN,MHSO,GLOLAB)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(ZERO=0.0D+00)
      LOGICAL PRTPRM,GOPARR,DSKWRK,MASWRK
      COMPLEX*16 HSO,WORK
      CHARACTER*6 GLOLAB
      COMMON /IOFILE/ IRS,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DIMENSION HSO(NHSO,NHSO),EIG(*),RWORK(*),WORK(*),IROOTS(*),
     *          MULST(*),INDX(*),COPCON(MHSO,MXRT,MXRT,NUMCI,NUMCI),
     *          COPCON1(MHSO,MXRT,MXRT,NUMCI,NUMCI),ENGYST(MXRT,*)
C
C     PUNCH OUT HSO MATRIX, print out the weights and other results
C
      IF(MASWRK) WRITE(IP,9310) GLOLAB
      CALL ZPUSQL(HSO,NHSO,NHSO,NHSO)
      IF(MASWRK) WRITE(IW,9313) GLOLAB
      IF(MASWRK) WRITE(IW,9315) (EIG(I),I=1,NHSO)
      IF(MASWRK) WRITE(IP,9312) GLOLAB
      IF(MASWRK) WRITE(IP,9314) (EIG(I),I=1,NHSO)
C
C     DIAGONALIZE HSO MATRIX:
C
      CALL ZHEEV('V','L',NHSO,HSO,NHSO,EIG,WORK,NHSO*2,RWORK,INFO)
      IF(INFO.NE.0) THEN
         IF (MASWRK) WRITE(IW,9240) INFO
         ENDIF
C
C     Calculate the weight of states for spin multiplicity:
      IF (MASWRK) WRITE(IW,9320) GLOLAB
      DO 320 IST=1,NHSO
         WTOTAL = ZERO
         J  = 0
         IWGHT = 0
         DO 322 ICI=1,NUMCI
            DO 324 IRT=1,IROOTS(ICI)
               W1 = ZERO
               DO 326 IM=1,MULST(ICI)
                  J = J + 1
#if defined(SPEC_CPU)
                  W1 = W1 + DBLE(HSO(J,IST)*DCONJG(HSO(J,IST)))
#else
                  W1 = W1 + DREAL(HSO(J,IST)*DCONJG(HSO(J,IST)))
#endif
  326             CONTINUE
               IWGHT = IWGHT + 1
               RWORK(IWGHT) = W1
               WTOTAL = WTOTAL + W1
C              weight of adiabatic states...
  324          CONTINUE
  322       CONTINUE
C        write(iw,9322) iwght
C
C        compare the weights and find which state has largest.
         WMAX = RWORK(1)
         IMAX = 1
         DO 330 IX=2,IWGHT
            IF(RWORK(IX).GT.WMAX) THEN
               WMAX = RWORK(IX)
               IMAX = IX
               ENDIF
  330       CONTINUE
C        find which is the imax-th state.
         ICIW = 0
         DO 332 ICI=1,NUMCI
            ICIW = ICIW + IROOTS(ICI)
            IF(IMAX.LE.ICIW) THEN
               JST = IMAX - (ICIW - IROOTS(ICI))
               INDX(IST) = ICI*1000 + JST
               WORK(IST) = DCMPLX(EIG(IST),WMAX)
               GO TO 334
               ENDIF
  332       CONTINUE
  334    CONTINUE
         IF (MASWRK) WRITE(IW,9330) IST,EIG(IST),WMAX,INDX(IST)
         IF(MASWRK) WRITE(IW,9332) WTOTAL
         LOOP = (IWGHT-1)/10 +1
         DO 336 LP=1,LOOP
            J2 = LP*10
            J1 = J2 -9
            IF(J2.GT.IWGHT) J2 = IWGHT
            IF (MASWRK) WRITE(IW,9331) (J, J=J1,J2)
            IF (MASWRK) WRITE(IW,9332) (RWORK(J),J=J1,J2)
  336       CONTINUE
  320    CONTINUE
C
C     PRINT OUT RESULTS:
      LOOP = (NHSO-1)/5 +1
      IF(PRTPRM) THEN
         IF (MASWRK) WRITE(IW,9400) GLOLAB
         CALL ZPRSQL(NUMCI,NHSO,HSO,EIG,INDX,MULST,IROOTS,IW,0)
      ENDIF
      EZERO=ZERO
      IF(NZSPIN.EQ.0) EZERO=EIG(1)
      IF (MASWRK) WRITE(IW,9417) GLOLAB
      IF(NZSPIN.EQ.0) WRITE(IW,9410)
      IF (MASWRK) WRITE(IW,9406) (EIG(I)-EZERO,I=1,NHSO)
C     the following IDs are very useful for a many state calculation
      IF (MASWRK) WRITE(IW,9417) GLOLAB
      DO 400 I=1,LOOP
        J2 = I*5
        J1 = J2 -4
        IF(J2.GT.NHSO) J2=NHSO
        IF (MASWRK) WRITE(IW,9404) (J,INDX(J),J=J1,J2)
        IF (MASWRK) WRITE(IW,9406) (EIG(J),J=J1,J2)
  400 CONTINUE
      IF(MASWRK) THEN
         WRITE(IP,9416) GLOLAB
         WRITE(IP,9314) (EIG(I),I=1,NHSO)
         WRITE(IP,9420)
      END IF
      IF(MASWRK) WRITE(IW,9800) GLOLAB
      TOL=1.0D-02
      DO 300 ICI1=1,NUMCI
        DO 300 ICI2=1,NUMCI
          MUL1=MULST(ICI1)
          MUL2=MULST(ICI2)
          IROOT1=IROOTS(ICI1)
          IROOT2=IROOTS(ICI2)
          IT1=1
  100     CONTINUE
            ND1=NETHER(ENGYST(IT1,ICI1),IROOT1-IT1+1)
            MINIT2=1
            IF(ICI1.EQ.ICI2) MINIT2=IT1
            IT2=MINIT2
  200       CONTINUE
              ND2=NETHER(ENGYST(IT2,ICI2),IROOT2-IT2+1)
              C=ZERO
              C1=ZERO
              DO 250 IR=IT1,IT1+ND1-1
                DO 250 JR=IT2,IT2+ND2-1
                  C=C+COPCON(1,IR,JR,ICI1,ICI2)
                  C1=C1+COPCON1(1,IR,JR,ICI1,ICI2)
  250         CONTINUE
              C=SQRT(C)
              C1=SQRT(C1)
              IF(ABS(C).GT.TOL) THEN
                IE1=IT1+ND1-1
                IE2=IT2+ND2-1
                IF(MASWRK) THEN
                IF(MAXNCO.EQ.1) THEN
                  IF(ND1.EQ.1.AND.ND2.EQ.1)
     *              WRITE(IW,9810) MUL1,ICI1,MUL2,ICI2,IT1,IT2,C
                  IF(ND1.NE.1.AND.ND2.EQ.1)
     *              WRITE(IW,9812) MUL1,ICI1,MUL2,ICI2,IT1,IE1,IT2,C
                  IF(ND1.EQ.1.AND.ND2.NE.1)
     *              WRITE(IW,9814) MUL1,ICI1,MUL2,ICI2,IT1,IT2,IE2,C
                  IF(ND1.NE.1.AND.ND2.NE.1)
     *              WRITE(IW,9816) MUL1,ICI1,MUL2,ICI2,IT1,IE1,IT2,IE2,C
                ELSE
                  IF(ND1.EQ.1.AND.ND2.EQ.1)
     *              WRITE(IW,9810) MUL1,ICI1,MUL2,ICI2,IT1,IT2,C,C1
                  IF(ND1.NE.1.AND.ND2.EQ.1)
     *              WRITE(IW,9812) MUL1,ICI1,MUL2,ICI2,IT1,IE1,IT2,C,C1
                  IF(ND1.EQ.1.AND.ND2.NE.1)
     *              WRITE(IW,9814) MUL1,ICI1,MUL2,ICI2,IT1,IT2,IE2,C,C1
                  IF(ND1.NE.1.AND.ND2.NE.1)
     *              WRITE(IW,9816) MUL1,ICI1,MUL2,ICI2,IT1,IE1,IT2,IE2,
     *                             C,C1
                ENDIF
                END IF
              ENDIF
              IT2=IT2+ND2
            IF(IT2.LE.IROOT2) GOTO 200
            IT1=IT1+ND1
          IF(IT1.LE.IROOT1) GOTO 100
  300 CONTINUE
      IF(MASWRK) WRITE(IW,9840)
      RETURN
 9310 FORMAT(' $SPNORB --- ',A6,' SPIN-ORBIT MATRIX ELEMENTS.')
 9312 FORMAT(A6,' ADIABATIC STATES---')
 9313 FORMAT(/3X,A6,' ADIABATIC STATES (CM-1 UNIT) ---')
 9314 FORMAT(5F15.3)
 9315 FORMAT(3X,5F15.3)
 9240 FORMAT(/1X,'ERROR IN SPNHSO:  INFO=',I6)
 9320 FORMAT(/1X,'WEIGHTS OF SO ',A6,' STATES ---')
 9330 FORMAT(/1X,'STATE',I4,': RELATIVE E=',F15.3,
     *           ' CM-1; WEIGHT =',F6.3,I6)
 9331 FORMAT(4X,10I7)
 9332 FORMAT(8X,10F7.4)
 9400 FORMAT(//1X,59("-"),/1X,'EIGENVALUES AND EIGENVECTORS OF THE SO ',
     *       A6,' HAMILTONIAN',/1X,59(1H-))
 9404 FORMAT(1X,5(I6,'(',I7,')'))
 9406 FORMAT(1X,5F15.3)
 9410 FORMAT(1X,'(RELATIVE TO THE LOWEST SPIN-MIXED STATE)')
 9416 FORMAT(A6,' SPIN-MIXED STATES---')
 9417 FORMAT(/3X,'SPIN-MIXED ',A6,' STATES (CM-1 UNIT) ---')
 9420 FORMAT(' $END')
 9800 FORMAT(/1X,'NON-ZERO ',A6,' SPIN-ORBIT COUPLING CONSTANTS (CM-1)',
     *       /1X,69(1H-)/1X,'MUL1(CI)|MUL2(CI)| CI ROOT 1 | CI ROOT 2 ',
     *        '|    FULL     | 1E ONLY'/1X,69(1H-))
 9810 FORMAT(1X,2(I2,' (',I2,') |'),2(I6,5X,'|'),F13.2,'|',F13.2)
 9812 FORMAT(1X,2(I2,' (',I2,') |'),I5,'-',I2,3X,'|',I6,5X,2('|',F13.2))
 9814 FORMAT(1X,2(I2,' (',I2,') |'),I6,5X,'|',I5,'-',I2,3X,2('|',F13.2))
 9816 FORMAT(1X,2(I2,' (',I2,') |'),2(I5,'-',I2,3X,'|'),F13.2,'|',F13.2)
 9840 FORMAT(1X,69("-")/)
      END
C*MODULE SOLIB   *DECK IRRCSF
      INTEGER FUNCTION IRRCSF(N1,NFZC,IECONF)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IECONF(N1)
      PARAMETER (MXAO=2047)
      LOGICAL ABEL
      COMMON /SYMMOL/ GROUP,COMPLX,IGROUP,NAXIS,ILABMO,ABEL
      COMMON /SOSYM/  EULANG(4,48),GAM(48,48),IRMON(MXAO)
      COMMON /SYMMUL/ NIJREP(14,14),IJREP(2,14,14,14)
C
      IF(.NOT.ABEL) THEN
         IRRCSF=0
         RETURN
      ENDIF
C     find direct product of irreducible representations for all
C     orbitals in a csf. The same orbital occupancy is used in
C     all determinants in a given CSF.
      IRRCSF=1
      DO 210 I=1,N1
         IF(IECONF(I).EQ.1) IRRCSF=IJREP(2,1,IRRCSF,IRMON(I+NFZC))
  210 CONTINUE
      RETURN
      END
C*MODULE SOLIB   *DECK IRRUNP
      SUBROUTINE IRRUNP(IGP,IGAR,NGAM)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION IGAR(*)
      NGAM=0
      IGPAK=IGP
  110 CONTINUE
         NGAM=NGAM+1
         IGAR(NGAM)=IAND(IGPAK,15)
C        write(iw,*) 'unpacked',iGar(ngam)
         IGPAK=ISHFT(IGPAK,-4)
      IF(IGPAK.NE.0) GOTO 110
      RETURN
      END
C*MODULE SOLIB   *DECK INTSYM
      SUBROUTINE INTSYM(MULST,IROOTS,ISTSYM,IRRL,GSYLYES)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      LOGICAL GOPARR,DSKWRK,MASWRK,SYLYES(3),GSYLYES(3)
      CHARACTER*1 XYZ(3)
      DIMENSION MULST(*),IROOTS(*),IMS(3),IRRL(3),ISTSYM(MXRT,NUMCI)
      DATA IMS/-1,1,0/
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA XYZ/'X','Y','Z'/,KOW/4HYEA /,KINE/4HNAY /
#else
      CHARACTER*4 :: KOW_STR
      EQUIVALENCE (KOW, KOW_STR)
      CHARACTER*4 :: KINE_STR
      EQUIVALENCE (KINE, KINE_STR)
      DATA XYZ/'X','Y','Z'/,KOW_STR/"YEA "/,KINE_STR/"NAY "/
#endif
C
C     check if the integrals are needed for the SOC (by symmetry)
C     note that the integrals may be skipped even if the matrix elements
C     will be calculated because of a transition moment option.
C     (the number of integrals will be zero in this case)
C
      GSYLYES(1)=.FALSE.
      GSYLYES(2)=.FALSE.
      GSYLYES(3)=.FALSE.
      IZEROT=0
      DO 220 ICI1=1,NUMCI
        DO 210 ICI2=ICI1,NUMCI
          IF(MULST(ICI1).EQ.1.AND.MULST(ICI2).EQ.1) GOTO 210
C           no interaction between singlets
          IF(ICI1.EQ.ICI2.AND.IROOTS(ICI1).LE.1) GOTO 210
C           no interaction between same states
          IF(ABS(MULST(ICI1)-MULST(ICI2)).GT.2) GOTO 210
C           no interaction if |S1-S2|>1
C
          DO 140 KART=1,3
             CALL GCISOL(SYLYES(KART),ISTSYM,IMS(KART),ICI1,ICI2,
     *                   IROOTS,IZEROT,1,IRRL,0)
             GSYLYES(KART)=GSYLYES(KART).OR.SYLYES(KART)
  140     CONTINUE
C
  210   CONTINUE
  220 CONTINUE
      DO KART=1,3
         LAME=KINE
         IF(GSYLYES(KART)) LAME=KOW
         IF(MASWRK) WRITE(IW,9000) XYZ(KART),LAME
C        if(nosym.gt.0) gsylyes(1)=.true.
      ENDDO
      RETURN
 9000 FORMAT(1X,'SOC INTEGRALS NEED BE CALCULATED FOR L',A1,': ',A4)
      END
C
C*MODULE SOLIB   *DECK MIME2
      SUBROUTINE MIME2(COND1,COND2,A1,A2,IPRHSO,MP)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      LOGICAL COND1,COND2
      COMPLEX*16 A1,A2
      DATA TOL/1.0D-04/
C
C     the same tol as in mimeout
C
      IF(MOD(IPRHSO/2,2).EQ.1.AND.ABS(A1+A2).LT.TOL) RETURN
      IF(COND1) THEN
         IF(MP.EQ.1) WRITE(IW,9999) A1,A2
         IF(MP.EQ.2) WRITE(IW,9990) A1
      ELSE IF(COND2) THEN
              IF(MP.EQ.1) WRITE(IW,9999) -DCONJG(A1),-DCONJG(A2)
              IF(MP.EQ.2) WRITE(IW,9990) -DCONJG(A1)
           ELSE
              IF(MP.EQ.1) WRITE(IW,9999) DCONJG(A1),DCONJG(A2)
              IF(MP.EQ.2) WRITE(IW,9990) DCONJG(A1)
           ENDIF
      RETURN
 9990 FORMAT( 1X,'1-EL. MP2 CONTRIB.(CM-1)    ',2F11.4,'I')
 9999 FORMAT( 1X,'1-EL. CONTRIBUTION(CM-1)    ',2F11.4,'I',
     *       /1X,'2-EL. CONTRIBUTION(CM-1)    ',2F11.4,'I')
      END
C*MODULE SOLIB   *DECK MIMEIT
      SUBROUTINE MIMEIT(NAME,CHAR,IN,N)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*1 CHAR,NUM(9)
      CHARACTER*4 CNAME
      DATA NUM/'1','2','3','4','5','6','7','8','9'/
C
      CHAR=' '
      IF(N.LE.1) RETURN
C
      WRITE(UNIT=CNAME, FMT='(A4)') NAME
      IP=4+2
      DO 100 I=4,1,-1
         IP=IP-1
         IF(CNAME(I:I).NE.' ') GOTO 200
  100 CONTINUE
C
  200 CONTINUE
      IF(IP.LE.0.OR.IP.GT.4) CALL ABRT
      CNAME(IP:IP)=':'
      IF(IP.LT.4) THEN
         CNAME(IP+1:IP+1)=NUM(IN)
      ELSE
         CHAR=NUM(IN)
      ENDIF
      RETURN
      END
C*MODULE SOLIB   *DECK MIMEOUT
      SUBROUTINE MIMEOUT(IROOT,J1,MS1,ICI1,JROOT,J2,MS2,ICI2,OP1,OP2,OP3
     *                  ,CYG1,CYG2,CYG3,ISTSYM,IRCIOR,IPRHSO,IPRINT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (TWO=2.0D+00)
      DIMENSION IRCIOR(MXRT,*),ISTSYM(MXRT,*)
      COMMON /SYMBLK/ NIRRED,NSALC,NSALC2,NSALC3
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      COMMON /SYMREP/ IRPNAM(14),IPA(14),LAMBDA(14),LAMBD0(14),
     *                IADDR1(14),IADDR2(14),IADDR3(14)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      CHARACTER*1 CHARI,CHARJ,CYG1,CYG2,CYG3
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA IQUEST/4H??  /,TOL/1.0D-04/
#else
      CHARACTER*4 :: IQUEST_STR
      EQUIVALENCE (IQUEST, IQUEST_STR)
      DATA IQUEST_STR/"??  "/,TOL/1.0D-04/
#endif
C
C     iprint==0 Hso         iprhso = -1 no print-out
C             1 R                    0/1 symmetry/sequential labelling
C                                    0/2 all/non-zero matrix el. output
C
C     ircior contains number n from n-(2S+1)G:i
C     istsym contains G & i
C
C     -log(tol) is the number of digits after the decimal point
C     printed by this routine. It is used for IPRHSO=2 option
C
      IF(IPRHSO.LT.0.OR.(MOD(IPRHSO/2,2).EQ.1.AND.ABS(OP1).LT.TOL.AND.
     *   ABS(OP2).LT.TOL.AND.(IPRINT.EQ.0.OR.ABS(OP3).LT.TOL))) RETURN
      IRCIORI=IRCIOR(IROOT,ICI1)
      ISYMI=ISTSYM(IROOT,ICI1)
      IF(ISYMI.GT.NIRRED.OR.ISYMI.LE.0) THEN
         LAMBDAI=1
         ISTI=IRCIORI
         IDEGI=1
         IGNAMI=IQUEST
      ELSE
         LAMBDAI=LAMBD0(ISYMI)
         ISTI=(IRCIORI-1)/LAMBDAI+1
         IDEGI=MOD(IRCIORI-1,LAMBDAI)+1
         IGNAMI=IRPNAM(ISYMI)
      ENDIF
      IRCIORJ=IRCIOR(JROOT,ICI2)
      ISYMJ=ISTSYM(JROOT,ICI2)
      IF(ISYMJ.GT.NIRRED.OR.ISYMJ.LE.0) THEN
         LAMBDAJ=1
         ISTJ=IRCIORJ
         IDEGJ=1
         IGNAMJ=IQUEST
      ELSE
         LAMBDAJ=LAMBD0(ISYMJ)
         ISTJ=(IRCIORJ-1)/LAMBDAJ+1
         IDEGJ=MOD(IRCIORJ-1,LAMBDAJ)+1
         IGNAMJ=IRPNAM(ISYMJ)
      ENDIF
      CALL MIMEIT(IGNAMI,CHARI,IDEGI,LAMBDAI)
      CALL MIMEIT(IGNAMJ,CHARJ,IDEGJ,LAMBDAJ)
      ISTYLE=MOD(IPRHSO,2)
      IF(IPRINT.EQ.0) THEN
         IF(ISTYLE.EQ.0) THEN
            WRITE(IW,9000) ISTI,J1+1,IGNAMI,CHARI,MS1/TWO,ISTJ,J2+1,
     *                     IGNAMJ,CHARJ,MS2/TWO,OP1,OP2,CYG1
         ELSE
            WRITE(IW,9100) IROOT,J1/TWO,MS1/TWO,JROOT,J2/TWO,
     *                     MS2/TWO,OP1,OP2,CYG1
         ENDIF
      ELSE
         IF(ISTYLE.EQ.0) THEN
            WRITE(IW,9200) ISTI,J1+1,IGNAMI,CHARI,ISTJ,J2+1,IGNAMJ,CHARJ
     *                    ,OP1,OP2,OP3,CYG1,CYG2,CYG3
         ELSE
            WRITE(IW,9300) IROOT,J1/TWO,JROOT,J2/TWO,OP1,OP2,OP3,
     *                     CYG1,CYG2,CYG3
         ENDIF
      ENDIF
      RETURN
 9000 FORMAT(1X,'<',I2,'-',I1,A4,A1,'(MS=',F4.1,')|HSO|',I2,'-',I1,
     *       A4,A1,'(MS=',F4.1,')> =',2F11.4,'I',4X,'[',A1,']')
 9100 FORMAT(1X,'<NST=',I2,',S=',F3.1,',MS=',F4.1,'|HSO|NST=',I2,
     *       ',S=',F3.1,',MS=',F4.1,'> =',2F11.4,'I',4X,'[',A1,']')
 9200 FORMAT(3X,'<',I2,'-',I1,A4,A1,'|R|',I2,'-',I1,A4,A1,'> =',
     *       3F13.4,1X,'[',3A1,']')
 9300 FORMAT(3X,'<NST=',I2,',S=',F3.1,'|R|NST=',I2,',S=',F3.1,'>=',
     *       3F13.4,1X,'[',3A1,']')
      END
C*MODULE SOLIB   *DECK NETHER
      INTEGER FUNCTION NETHER(ENERGY,IROOT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION ENERGY(*)
C
C     determine the degeneracy of energy
C
      TOL=1.0D-06
      NETHER=1
      E=ENERGY(1)
      DO 100 IT=2,IROOT
         IF(ABS(E-ENERGY(IT)).GT.TOL) RETURN
         NETHER=NETHER+1
  100 CONTINUE
      RETURN
      END
C*MODULE SOLIB   *DECK PROPAGATE
      SUBROUTINE PROPAGATE(ICI1,ICI2,I,J,J1,M1,J2,M2,ELEM,ELEM1,NHSO,HSO
     *                    ,MULST,IROOTS,ISTSYM,IRCIOR,NPROP,NSYMFOR,
     *                     ADD2E,ALLOWED,NOSYM,IPRHSO,COPCON,COPCON1,
     *                     HSOTOL,SAMEMUL)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(ZERO=0.0D+00,ONE=1.0D+00)
      COMPLEX*16 HSO,ELEM,ELEM1,SL,SL1,SL2
      LOGICAL ADD2E,ALLOWED,GOPARR,DSKWRK,MASWRK,SAMEMUL
      CHARACTER*1 CYGNET
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /SPINFO/ NCORE,MS,NAO,NAOD,NAO2,NAO4,MSKNAO
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      DIMENSION HSO(NHSO,NHSO),MULST(*),IROOTS(*),ISTSYM(MXRT,*),
     *          IRCIOR(MXRT,*)
C
C     use all symmetry propreties to propagate the calculated matrix
C     element inside the matrix
C
C     remember, m1=m1-2 etc means m1=m1-1 because m1 is double of m1!!
C
      ISHIFT=MULST(ICI1)*(I-1)+1
      JSHIFT=MULST(ICI2)*(J-1)+1
      DO 200 ICI=1,ICI1-1
         ISHIFT=ISHIFT+MULST(ICI)*IROOTS(ICI)
  200 CONTINUE
      DO 220 ICI=1,ICI2-1
         JSHIFT=JSHIFT+MULST(ICI)*IROOTS(ICI)
  220 CONTINUE
C     if(ici1.eq.ici2) then
      IF(SAMEMUL) THEN
         IF(MS.EQ.1)  ISHIFT=ISHIFT+1
C        if(ms.eq.-1) jshift=jshift+1
      ELSE
         JSHIFT=JSHIFT+1-MS
      ENDIF
      C0=CLEBSCH(2,J2,-MS*2,M2,J1,M1)
      IF(ABS(C0).LT.1.0D-10) THEN
         IF(NOSYM.GT.1) THEN
            C0=ONE
C           imitate operation, full functioning will not be possible
C           but these are zero by symmetry anyway
         ELSE
            IF(MASWRK) WRITE(IW,9000) 2,J2,-MS*2,M2,J1,M1
            CALL ABRT
         ENDIF
      ENDIF
      SL=ELEM/C0
      SL1=ELEM1/C0
      SL2=SL-SL1
      CYGNET='C'
      IF(.NOT.ALLOWED) CYGNET='Z'
      MSPROP=1
      IF(MS.NE.0) MSPROP=2
      K=0
  100 CONTINUE
         IF(K.NE.0) NPROP=NPROP+MSPROP
         C=CLEBSCH(2,J2,-MS*2,M2-K*2,J1,M1-K*2)
         ELEM=SL*C
         IF(.NOT.ADD2E) ELEM=SL1*C
         IF(ABS(C).LE.1.0D-10) CYGNET='W'
C        get at ms=-1
         DO 150 MSS=MS,-MS,-2
           IF(MSS.NE.-1) THEN
             MM1=M1-K*2
             MM2=M2-K*2
             IND1=ISHIFT+K
             IND2=JSHIFT+K
C          else if(ici1.eq.ici2) then
           ELSE IF(SAMEMUL) THEN
             ELEM=-DCONJG(ELEM)
             MM2=M1-K*2
             MM1=M2-K*2
             IND1=ISHIFT-1+K
             IND2=JSHIFT+1+K
           ELSE
             ELEM= DCONJG(ELEM)
             MM1=-(M1-K*2)
             MM2=-(M2-K*2)
             IND1=ISHIFT-1+MULST(ICI1)-K
             IND2=JSHIFT-1+MULST(ICI2)-K
           ENDIF
           IF(MASWRK) THEN
              WRITE(IW,*) ' '
              CALL MIMEOUT(I,J1,MM1,ICI1,J,J2,MM2,ICI2,
#if defined(SPEC_CPU)
     *                     DBLE(ELEM),DIMAG(ELEM),ZERO,CYGNET,' ',' ',
#else
     *                     DREAL(ELEM),DIMAG(ELEM),ZERO,CYGNET,' ',' ',
#endif
     *                     ISTSYM,IRCIOR,IPRHSO,0)
           END IF
           IF(ABS(ELEM).GT.HSOTOL.AND..NOT.ALLOWED) THEN
             IF(MASWRK) WRITE(IW,9100) ELEM
             NSYMFOR=NSYMFOR+1
           ENDIF
           IF(MASWRK) CALL MIME2(MSS.NE.-1,SAMEMUL,SL1*C,SL2*C,IPRHSO,1)
           HSO(IND1,IND2)=ELEM
           HSO(IND2,IND1)=DCONJG(ELEM)
#if defined(SPEC_CPU)
           COPCON =COPCON +DBLE(ELEM*DCONJG(ELEM))
#else
           COPCON =COPCON +DREAL(ELEM*DCONJG(ELEM))
#endif
#if defined(SPEC_CPU)
           COPCON1=COPCON1+DBLE(SL1*DCONJG(SL1))*C*C
#else
           COPCON1=COPCON1+DREAL(SL1*DCONJG(SL1))*C*C
#endif
           CYGNET='P'
  150    CONTINUE
         K=K+1
      IF(M1-K*2.GE.-J1.AND.M2-K*2.GE.-J2) GOTO 100
      RETURN
 9000 FORMAT(/1X,'YOU HAVE ENCOUNTERED A POLE IN THE ALGORITHM.',/,
     *   1X,'IT CANNOT OCCUR FOR MULTIPLICITIES LESS THAN AT LEAST 16:',
     *       6I3,/)
 9100 FORMAT(1X,'THE ABOVE MATRIX ELEMENT OF ZERO BY SYMMETRY IS ',
     *       2F14.8,'I'/)
      END
C*MODULE SOLIB   *DECK PROPZEFF
      SUBROUTINE PROPZEFF(ICI1,ICI2,IROOT,JROOT,MS,MAXNCO,ELEM,ELEM1,
     *                    SL1,RR,VALZ,MULST,IROOTS,ISTSYM,IRCIOR,
     *                    ADD2E,NPROP,NSYMFOR,NZERO,NCALC,ALLOWED,
     *                    RALLOW,SKIPDM,NOSYM,IPRHSO,NHSO,NTDM,
     *                    MHSO,HSO,TDM,ALZ,COPCON,COPCON1,HSOTOL,
     *                    JZOPT,SAMEMUL)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      PARAMETER(ZERO=0.0D+00,ONE=1.0D+00)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,ADD2E,ALLOWED,RALLOW(3),SKIPDM,
     *        SAMEMUL
C
      COMPLEX*16 HSO(NHSO,NHSO,MHSO),ELEM,ELEM1(MHSO),SL,SL1(MHSO),SL2
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      DIMENSION MULST(*),IROOTS(*),ISTSYM(MXRT,NUMCI),IRCIOR(MXRT,NUMCI)
     *       ,TDM(3,NTDM,NTDM),ALZ(NTDM,NTDM),RR(3),COPCON(2),COPCON1(2)
      CHARACTER*1 CYGNET,COLT(3)
C     data colt/3*'c'/
C
C     use all symmetry propreties to propagate the calculated matrix
C     element inside the matrix
C     j1,j2 are also doubled (j**2*2), same for m1,m2
C     this is a modified version of propagate for PB SOC (q.v.)
C     it propagates Hso matrix elements even if they are zero
C     note that rx,ry,rz depend only on radial part, no propagation
C
      CYGNET='C'
      IF(.NOT.ALLOWED) CYGNET='Z'
      DO 90 MM=1,3
         IF(RALLOW(MM)) THEN
            COLT(MM)='C'
         ELSE
            COLT(MM)='Z'
         ENDIF
   90 CONTINUE
      J1=MULST(ICI1)-1
      J2=MULST(ICI2)-1
C
C     since transition dipole moments are wanted,
C     hso is also calc for singlet/singlet
C
      MSI=J1
      MSJ=J2
C     ms is *not* doubled (for compatibility with propagate)
C     if(ici1.eq.ici2.and.ms.eq.1) msi=msi-2
      IF(SAMEMUL.AND.MS.EQ.1) MSI=MSI-2
C     if(ici1.ne.ici2.and.ms.eq.0) msj=msj-2
      IF(.NOT.SAMEMUL.AND.MS.EQ.0) MSJ=MSJ-2
C     if(ici1.ne.ici2) then
      IF(.NOT.SAMEMUL) THEN
         COLT(1)='S'
         COLT(2)='S'
         COLT(3)='S'
      ENDIF
      ISHIFT=MULST(ICI1)*(IROOT-1)+1
      JSHIFT=MULST(ICI2)*(JROOT-1)+1
      IRSHIFT=IROOT
      JRSHIFT=JROOT
      DO 200 ICI=1,ICI1-1
         ISHIFT=ISHIFT+MULST(ICI)*IROOTS(ICI)
         IRSHIFT=IRSHIFT+IROOTS(ICI)
  200 CONTINUE
      DO 220 ICI=1,ICI2-1
         JSHIFT=JSHIFT+MULST(ICI)*IROOTS(ICI)
         JRSHIFT=JRSHIFT+IROOTS(ICI)
  220 CONTINUE
C     if(ici1.eq.ici2) then
      IF(SAMEMUL) THEN
         IF(MS.EQ.1)  ISHIFT=ISHIFT+1
C        if(ms.eq.-1) jshift=jshift+1
      ELSE
         JSHIFT=JSHIFT+1-MS
      ENDIF
C     a bit crooked second quantisation normalisation
      IF(MS.EQ.0) THEN
         C0=CLEBSCH(2,J2,-MS*2,MSJ,J1,MSI)
      ELSE
        IF(SAMEMUL) THEN
           C0=CLEBSCH(2,J2,0,MSJ,J1,MSJ)
         ELSE
           C0=CLEBSCH(2,J2,0,MSI,J1,MSI)
         ENDIF
      ENDIF
      IF(ABS(C0).LT.1.0D-10.AND.(J1.NE.0.OR.J2.NE.0)) THEN
         IF(NOSYM.NE.0) THEN
            C0=ONE
C           to avoid division by 0 and fake operation
         ELSE
            IF (MASWRK) WRITE(IW,9000) 2,J2,-MS*2,MSJ,J1,MSI
            CALL ABRT
         ENDIF
      ENDIF
      IF(ABS(ELEM).LT.HSOTOL) THEN
         IF(ALLOWED.OR.NOSYM.GT.0) THEN
C           write(iw,*) 'below element is zero factually',elem,allowed,
C    *                  nosym,msi,msj
            NZERO=NZERO+1
         ENDIF
      ENDIF
      IF(ALLOWED.OR.NOSYM.GT.0) NCALC=NCALC+1
C
C     remember, m1=m1-2 etc means m1=m1-1 because m1 is double of m1!!
C
      DO IH=1,MHSO
      IF(J1.NE.0.OR.J2.NE.0) THEN
         SL=ELEM/C0
         SL1(IH)=ELEM1(IH)/C0
      ELSE
         SL=ELEM
         SL1(IH)=ELEM1(IH)
      ENDIF
      ENDDO
      SL2=SL-SL1(1)
      IF(MHSO.GT.1) SL2=SL2-SL1(2)
      IF(JZOPT.NE.0) THEN
        IF(MS.EQ.0) ALZ(IRSHIFT,JRSHIFT)=VALZ
        IF(MS.EQ.0.AND.IRSHIFT.NE.JRSHIFT) ALZ(JRSHIFT,IRSHIFT)=-VALZ
      ENDIF
      IF(MS.EQ.0.AND..NOT.SKIPDM) THEN
         TDM(1,IRSHIFT,JRSHIFT)=RR(1)
         TDM(2,IRSHIFT,JRSHIFT)=RR(2)
         TDM(3,IRSHIFT,JRSHIFT)=RR(3)
         IF(MASWRK) THEN
            IF(.NOT.(RALLOW(1).OR.RALLOW(2).OR.RALLOW(3)).AND.
     *         SQRT(DDOT(3,RR,1,RR,1)).GT.HSOTOL)
     *         WRITE(IW,9140) (RR(II),II=1,3)
            CALL MIMEOUT(IROOT,J1,MSI,ICI1,JROOT,J2,MSJ,ICI2,RR(1),RR(2)
     *            ,RR(3),COLT(1),COLT(2),COLT(3),ISTSYM,IRCIOR,IPRHSO,1)
         ENDIF
      ENDIF
      MSPROP=1
      IF(MS.NE.0) MSPROP=2
      K=0
  100 CONTINUE
         IF(K.NE.0.AND.(ICI1.NE.ICI2.OR.IROOT.NE.JROOT.OR.MS.LE.0))
     *      NPROP=NPROP+MSPROP
         C=CLEBSCH(2,J2,-MS*2,MSJ-K*2,J1,MSI-K*2)
         ELEM=SL*C
         IF(MHSO.GT.1) ELEM1(2)=SL1(2)*C
         IF(.NOT.ADD2E) ELEM=SL1(1)*C
         IF(.NOT.ADD2E.AND.MHSO.GT.1) ELEM=ELEM+SL1(2)*C
         IF(ABS(C).LE.1.0D-10) CYGNET='W'
C        get at ms=-1
         DO 150 MSS=MS,-MS,-2
           IF(MSS.NE.-1) THEN
             MM1=MSI-K*2
             MM2=MSJ-K*2
             IND1=ISHIFT+K
             IND2=JSHIFT+K
           ELSE IF(SAMEMUL) THEN
             ELEM=-DCONJG(ELEM)
             IF(MHSO.GT.1) ELEM1(2)=-DCONJG(ELEM1(2))
             MM2=MSI-K*2
             MM1=MSJ-K*2
             IND1=ISHIFT-1+K
             IND2=JSHIFT+1+K
           ELSE
             ELEM= DCONJG(ELEM)
             IF(MHSO.GT.1) ELEM1(2)= DCONJG(ELEM1(2))
             MM1=-(MSI-K*2)
             MM2=-(MSJ-K*2)
             IND1=ISHIFT-1+MULST(ICI1)-K
             IND2=JSHIFT-1+MULST(ICI2)-K
           ENDIF
           IF(MASWRK) CALL MIMEOUT(IROOT,J1,MM1,ICI1,JROOT,J2,MM2,ICI2,
#if defined(SPEC_CPU)
     *                     DBLE(ELEM),DIMAG(ELEM),ZERO,CYGNET,' ',' ',
#else
     *                     DREAL(ELEM),DIMAG(ELEM),ZERO,CYGNET,' ',' ',
#endif
     *                     ISTSYM,IRCIOR,IPRHSO,0)
           IF(ABS(ELEM).GT.HSOTOL.AND..NOT.ALLOWED) THEN
C            if(maswrk) write(iw,9130) elem
C9130 FORMAT(/1X,'<HSO> BELOW OF ZERO BY SYMMETRY IS ',2F14.8,'I')
C
C            after changing the algorithm to be CSF-based rather than
C            CI state based this symmetry-tracking became "frills".
C            secondly, the introduction of contraction pushed some zeros
C            beyond the zero threshold.
C
             NSYMFOR=NSYMFOR+1
           ENDIF
           DO IH=1,MHSO
           IF((MAXNCO.GT.1.OR.IH.EQ.2).AND.MASWRK)
     *       CALL MIME2(MSS.NE.-1,SAMEMUL,SL1(IH)*C,SL2*C,IPRHSO,IH)
           ENDDO
           IF(MAXNCO.GT.1) THEN
             COPCON1(1)=COPCON1(1)+(ABS(SL1(1))*C)**2
           IF(MHSO.GT.1) COPCON1(2)=COPCON1(2)+(ABS(SL1(1)+SL1(2))*C)**2
           ENDIF
C          add total contribution (CAS+MCQDPT)
#if defined(SPEC_CPU)
           COPCON(MHSO)=COPCON(MHSO)+DBLE(ELEM*DCONJG(ELEM))
#else
           COPCON(MHSO)=COPCON(MHSO)+DREAL(ELEM*DCONJG(ELEM))
#endif
           HSO(IND1,IND2,MHSO)=HSO(IND1,IND2,MHSO)+ELEM
           IF(IND1.NE.IND2)
     *        HSO(IND2,IND1,MHSO)=HSO(IND2,IND1,MHSO)+DCONJG(ELEM)
           IF(MHSO.GT.1) THEN
C
C       substract the MCQDPT correction from CAS+MCQDPT to get just CAS
C
             COPCON(1)=COPCON(1)+ABS(ELEM-ELEM1(2))**2
             HSO(IND1,IND2,1)=HSO(IND1,IND2,1)+ELEM-ELEM1(2)
             IF(IND1.NE.IND2)
     *          HSO(IND2,IND1,1)=HSO(IND2,IND1,1)+DCONJG(ELEM-ELEM1(2))
           ENDIF
           CYGNET='P'
  150      CONTINUE
         K=K+1
      IF(MSI-K*2.GE.-J1.AND.MSJ-K*2.GE.-J2) GOTO 100
      RETURN
 9000 FORMAT(/1X,'YOU HAVE ENCOUNTERED A POLE IN THE ALGORITHM.',/,
     *        1X,'CLEBSCH',6I3,'IS ZERO. CONTRAFUSED.'/)
 9140 FORMAT(/1X,'<R> BELOW OF ZERO BY SYMMETRY IS ',3F14.8)
      END
C*MODULE SOLIB   *DECK RETSOSYM
      SUBROUTINE RETSOSYM(NEEDSYM)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      CALL RETFM(NEEDSYM)
      RETURN
      END
C*MODULE SOLIB   *DECK SETSOSYM
      SUBROUTINE SETSOSYM(LSTSYM,LIRCIOR,NEEDSYM,MULST,IROOTS,IRRL,IRRR,
     *                    GSYLYES)
Cdg   subroutine setsosym(memij,LNIJDR,Lijdrp,lstsym,lircior,needsym,
Cdg  *                    mulst,iroots,irrl,irrr)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,DEBUG,GSYLYES(3)
C
      PARAMETER (MXATM=500, MXSH=1000, MXAO=2047, MXGRP=48, MAXCP=4096)
C     PARAMETER (maxdirr=12,mxsdirr=MXGRP)
C
Cdg   COMMON /SYMREP/ IRPNAM(14),IPA(14),LAMBDA(14),LAMBD0(14),
Cdg  *                IADDR1(14),IADDR2(14),IADDR3(14)
      COMMON /SYMTRY/ MAPSHL(MXSH,48),MAPCTR(MXATM,48),
     *                T(432),INVT(48),NT
      COMMON /SYMBLK/ NIRRED,NSALC,NSALC2,NSALC3
      COMMON /SOSYM/ EULANG(4,MXGRP),GAM(MXGRP,48),IRMON(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      COMMON /FMCOM / X(1)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /SOGUG/  CP(MAXCP),NUNIQ,LIOBP,ISODAF,NSODA,LSTREC,JSODAF,
     *                JSODA,NRECJ
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /RUNOPT/ RUNTYP,EXETYP,NEVALS,NGLEVL,NHLEVL
      DIMENSION MULST(*),IROOTS(*),TF(3,3),IRRL(3),IRRR(3)
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DEBUGG/8HDEBUG   /,DBGME/8HSALC    /
#else
      CHARACTER*8 :: DEBUGG_STR
      EQUIVALENCE (DEBUGG, DEBUGG_STR)
      CHARACTER*8 :: DBGME_STR
      EQUIVALENCE (DBGME, DBGME_STR)
      DATA DEBUGG_STR/"DEBUG   "/,DBGME_STR/"SALC    "/
#endif
C
C     lambda is the same as lambd0 except of complex representations
C     (then lambd0 is 2 and lambda is 1).
C     since GAMESS classifies everything in pseudo-irreducible reps
C     lambd0, this is what is used in the SOC code. Should this be
C     changed, all lambd0 are to be replaced by lambda.
C
C     this routine has been deprived of double-group related lines.
C     these are commented with "cdg"
C
      DEBUG=(EXETYP.EQ.DEBUGG.OR.EXETYP.EQ.DBGME).AND.MASWRK
      IF(DEBUG) WRITE(IW,9010) NT
      DO 100 I=1,NT
         IF(DEBUG) WRITE(IW,*) 'ROTATION MATRIX ', I
C    trpose takes care of t which are stored row-wise instead of column
         CALL TRPOSE(T(1+(I-1)*9),TF,3,3,0)
         IF(DEBUG) CALL PRSQ(TF,3,3,3)
         CALL IEULER(IRET,TF,EULANG(1,I),EULANG(2,I),EULANG(3,I),
     *               EULANG(4,I))
         INV=IRET
C        euler angles will be standardised for double groups in kine
        IF(DEBUG) WRITE(IW,9020) EULANG(1,I),EULANG(2,I),EULANG(3,I),INV
  100 CONTINUE
      MAXSPN=MULST(NUMCI)-1
Cdg   MinSPN=MULST(1)-1
C     write(iw,*) 'minimum/maximum doubled spin is ',minspn,maxspn
Cdg   if((minspn/2)*2.ne.minspn) then
Cdg      iflagd=1
Cdg      nired1=maxdirr
C        for memory allocation, the actual number set later
C        beware to change the order of the indexes in Lijdrp,
C        as the nired1 passed to dmult will differ from defined here
Cdg   else
Cdg      iflagd=0
Cdg      nired1=nirred
Cdg   endif
C     memd=0
Cdg   memij=0
Cdg   do 110 j=minspn,maxspn,2
C        memd=memd+(j+1)*(j+1)
Cdg      memij=memij+j+1
Cd110 continue
C     LDW  = LOADFM + 1
C     LNIJDR= LDW + memd*nt
      CALL VALFM(LOADFM)
Cdg   LNIJDR= LOADFM + 1
Cdg   Lijdrp= LNIJDR + (NWDVAR-1+nirred*memij)/NWDVAR
Cdg   Lstsym= Lijdrp + (NWDVAR-1+nirred*memij*nired1)/NWDVAR
Cdg   lncior= lstsym + (mxrt/NWDVAR+1)*numci
Cdg   lircior=lncior + ((nirred+1)*numci-1)/NWDVAR+1
Cdg   last   =lircior+ (mxrt*numci-1)/NWDVAR+1
      LSTSYM =LOADFM + 1
      LNCIOR =LSTSYM + (MXRT/NWDVAR+1)*NUMCI
      LIRCIOR=LNCIOR + ((NIRRED+1)*NUMCI-1)/NWDVAR+1
      LAST   =LIRCIOR+ (MXRT*NUMCI-1)/NWDVAR+1
Cdg   if(iflagd.ne.0) then
Cdg      ldlambda = last
Cdg      ldgam    = ldlambda+ (maxdirr-1)/NWDVAR+1
Cdg      ldirpnam = ldgam   + nt*mxsdirr*2
Cdg      ldchar   = ldirpnam+ (maxdirr-1)/NWDVAR+1
Cdg      last     = ldchar  + nt*maxdirr*2
Cdg   else
Cdg      ldgam    = last
C        this is only formally used
Cdg   endif
      NEEDSYM = LAST - LOADFM-1
      CALL GETFM(NEEDSYM)
      IF(DEBUG) WRITE(IW,9030) NEEDSYM
      IF(MASWRK) WRITE(IW,9035)
      CALL INIFAC(MAXSPN*4)
C     call inifac(2*maxspn)
C     must use symmetries of d!!!
C     do 130 j=minspn,maxspn,2
C        n=(j+1)*(j+1)
C        do 130 i=1,nt
C           do 120 m=j,-j,-2
C              do 120 m1=j,-j,-2
C                 x(LDW+(j-m)/2+(j-m1)/2*(j+1)+n*(i-1)+
C    *            (j-minspn)/2*nt*n)=wigner(j,m,m1,eulang(4,i))
C 120       continue
C           if(debug) then
C              write(iw,*) 'wigner function for el ',i
C              call prsq(x(ldw+n*(i-1)+(j-minspn)/2*nt*n),j+1,j+1,j+1)
C           endif
C 130 continue
Cdg   if(iflagd.ne.0) then
Cdg      if(maswrk) write(iw,9040)
Cdg      call DSYMOR2(nired1,x(ldlambda),x(ldgam),x(ldchar),x(ldirpnam))
Cdg      if(nired1.eq.0) then
Cdg         if(maswrk) write(iw,9000)
Cdg      else
Cdg         if(maswrk) write(iw,9050) nired1,(intel(x(ldlambda),i),i=1,
Cdg  *                                nired1)
Cdg      endif
Cdg   endif
Cdg   if(maswrk) write(iw,9120)
Cdg   index=1
Cdg   do 150 j=minspn,maxspn,2
C        n=(j+1)*(j+1)
C        gam -> double rep for iflagd
Cdg      if(iflagd.eq.0) then
Cdg         call dmult(j,x(LNIJDR),index,memij,x(Lijdrp),nired1,x(ldgam)
Cdg  *                ,lambd0,irpnam,iflagd,debug)
C    *                ,lambd0,x(ldw+(j-minspn)/2*nt*n),irpnam,iflagd)
Cdg      else
Cdg         call dmult(j,x(LNIJDR),index,memij,x(Lijdrp),nired1,x(ldgam)
Cdg  *        ,x(ldlambda),x(ldirpnam),iflagd,debug)
C    *        ,x(ldlambda),x(ldw+(j-minspn)/2*nt*n),x(ldirpnam),iflagd)
Cdg      endif
Cdg      index=index+(j+1)
Cd150 continue
      CALL GETSTSYM(X(LSTSYM),X(JSODA),IROOTS)
      CALL ASSORCI(X(LSTSYM),X(LNCIOR),X(LIRCIOR),IROOTS)
      CALL LRIRREP(IRRL,IRRR)
      CALL INTSYM(MULST,IROOTS,X(LSTSYM),IRRL,GSYLYES)
      RETURN
C
C9000 format(/1x,'Sorry, not available yet.',/,
C    *        1x,'No space symmetry will be used.',/)
 9010 FORMAT(/1X,'PREPARING SYMMETRY RELATED INFORMATION FOR SOC',
     *         ' GROUP SIZE ',I2)
 9020 FORMAT(/1X,'IS PARAMETRISED BY 3 EULER ANGLES: ALPHA=',F8.4,
     *         ' BETA=',F8.4,' GAMMA=',F8.4,' INV',I2,/)
 9030 FORMAT(/1X,70("+")//,I7,' WORDS ALLOCATED FOR SOC SYMMETRY',/)
 9035 FORMAT(/1X,70("+")//)
C9040 format(/1x,'generating double-valued irreducible representations')
C9050 format(/1x,,I2,' irreps generated! Sizes are:',14I2/)
C9120 format(/1x,'Spin-orbit states are mixed to form irreducible ',/,
C    *        1x,'representations bases of the double group: ',/,
C    *        1x,'single-valued for integer spin and double-valued for',
C    *           ' half-integer spin.',/,
C    *        1x,'n@G stands for n-th row of irrep G.',//,
C    *        1x,'Spin-orbital states',8X,'irreps of the double group.')
      END
C*MODULE TRNSTN   *DECK SETZEFF
      SUBROUTINE SETZEFF(ZEFTYP,ZEFF)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXATM=500)
      DIMENSION ZEFF(MXATM),ZEF321(54),ZEFSBK(86)
C
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /ECP2  / CLP(400),ZLP(400),NLP(400),KFIRST(MXATM,6),
     *                KLAST(MXATM,6),LMAX(MXATM),LPSKIP(MXATM),
     *                IZCORE(MXATM)
      COMMON /INTOPT/ ISCHWZ,IECP,NECP,IEFLD
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA TYP321/8H3-21G   /,TYPSBK/8HSBKJC   /
#else
      CHARACTER*8 :: TYP321_STR
      EQUIVALENCE (TYP321, TYP321_STR)
      CHARACTER*8 :: TYPSBK_STR
      EQUIVALENCE (TYPSBK, TYPSBK_STR)
      DATA TYP321_STR/"3-21G   "/,TYPSBK_STR/"SBKJC   "/
#endif
C
C     Zeff for 3-21G(d) basis set:
C
      DATA (ZEF321(I),I=1,54)/ 1.0D+00, 2.0D+00,
     2     1.5D+00,   2.2D+00,   3.00D+00,  3.9D+00,
     *     4.9D+00,   6.0D+00,   7.2D+00,  10.0D+00,
     3    10.67D+00, 11.52D+00, 12.35D+00, 13.16D+00,
     *    13.95D+00, 14.72D+00, 15.47D+00, 18.0D+00,
     4    22.42D+00, 23.00D+00,
     T    21.00D+00, 22.00D+00, 23.00D+00, 24.00D+00, 25.00D+00,
     T    26.00D+00, 27.00D+00, 28.00D+00, 29.00D+00, 30.00D+00,
     *    34.72D+00, 34.88D+00, 34.98D+00, 35.02D+00, 35.00D+00,
     *    36.00D+00,
     5    45.88D+00, 47.12D+00,
     T    39.00D+00, 40.00D+00, 41.00D+00, 42.00D+00, 43.00D+00,
     T    44.00D+00, 45.00D+00, 46.00D+00, 47.00D+00, 48.00D+00,
     *    60.72D+00, 62.00D+00, 63.24D+00, 64.48D+00, 65.72D+00,
     *    54.00D+00/
C
C     Zeff for SBKJC(d,p) basis set
C
      DATA (ZEFSBK(I),I=1,54)/ 1.0D+00, 2.0D+00,
     2     1.5D+00,   2.2D+00,   3.00D+00,  3.9D+00,
     *     4.9D+00,   6.0D+00,   7.2D+00,  10.0D+00,
     3    132.0D+00, 144.0D+00, 156.0D+00, 168.0D+00,
     *    180.0D+00, 192.0D+00, 204.0D+00, 18.0D+00,
     4    779.0D+00, 820.0D+00,
     T    8.6D+00, 9.6D+00, 10.6D+00, 11.6D+00, 12.8D+00,
     T    13.9D+00, 15.1D+00, 16.4D+00, 17.7D+00, 330.0D+00,
     *    341.0D+00, 1312.D+00, 1353.D+00, 1394.D+00, 1435.D+00,
     *    36.00D+00,
     5    4070.D+00, 4180.D+00,
     T    184.9D+00, 192.0D+00, 199.3D+00, 206.6D+00, 214.1D+00,
     T    221.8D+00, 229.5D+00, 237.4D+00, 245.3D+00, 1584.0D+00,
     *    1617.D+00, 5500.D+00, 5610.D+00, 5720.D+00, 5830.D+00,
     *    54.00D+00/
C
      DATA (ZEFSBK(I),I=55,86)/
     6    12210.00D+00, 12432.00D+00,
     T    803.7D+00,
     L    58.00D+00, 59.00D+00, 60.00D+00, 61.00D+00, 62.00D+00,
     L    63.00D+00, 64.00D+00, 65.00D+00, 66.00D+00, 67.00D+00,
     L    68.00D+00, 69.00D+00, 70.00D+00, 71.00D+00,
     T                1025.3D+00, 1049.7D+00, 1074.5D+00, 1099.5D+00,
     T    1124.8D+00, 1150.4D+00, 1176.2D+00, 1202.4D+00, 9040.0D+00,
     *    9153.0D+00, 18204.0D+00, 18426.0D+00, 18648.0D+00,
     *    18870.0D+00, 86.0D+00/
C
C     pick effective charges from the 3 papers of Koseki, et al.
C     anything read in by the user is to be accepted graciously.
C     Note that a user can supply zero effective charges, for example,
C     for the purpose of getting Lz eigenvalues for CI roots.
C
      DO 120 I=1,NAT
         IF(ZEFF(I).GE.0.0D+00) GO TO 120
         NUCLX = INT(ZAN(I)+IZCORE(I)+0.5D+00)
         IF(ZEFTYP.EQ.TYP321  .AND.  NUCLX.LE.54) THEN
            ZEFF(I) = ZEF321(NUCLX)
         ELSE IF(ZEFTYP.EQ.TYPSBK  .AND.  NUCLX.LE.86) THEN
            ZEFF(I) = ZEFSBK(NUCLX)
         ELSE
            ZEFF(I) = ZAN(I)
C           use tru nuclear charges for MCPs
            IF(IECP.EQ.5.OR.IECP.EQ.6) ZEFF(I)=ZEFF(I)+IZCORE(I)
         ENDIF
  120 CONTINUE
      RETURN
      END
C*MODULE SOLIB   *DECK SMINUS
      SUBROUTINE SMINUS(IW,NELEC,IPRIM,CGC,NPRIM,IPRMSM,CGCSM,NPRMSM,
     *                  NTRAP,MXSPIN,MXPRM,OUT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL OUT,GOPARR,DSKWRK,MASWRK
      DIMENSION IPRIM(MXSPIN,MXPRM),CGC(MXPRM),IPRMSM(MXSPIN,MXPRM),
     *          CGCSM(MXPRM),NTRAP(MXSPIN)
C
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      PARAMETER (SMALL=1.0D-10, ONE=1.0D+00)
C
C     ----- OPERATE WITH S- ON SPIN-EIGENFUNCTIONS -----
C
      IF(OUT) THEN
         WRITE(IW,9010) NPRIM,NELEC
         DO 10 IP=1,NPRIM
          WRITE(IW,9011) CGC(IP),(IPRIM(M,IP),M=1,NELEC)
   10     CONTINUE
         END IF
C
      NEWP=0
      DO 50 IP=1,NPRIM
       DO 51 NE=1,NELEC
        IF(IPRIM(NE,IP).NE.1) GO TO 51
        CALL ICOPY(NELEC,IPRIM(1,IP),1,NTRAP,1)
        NTRAP(NE)=2
C
C ... SEARCH IPRMSM.
        DO 52 NP=1,NEWP
         DO 53 NEX=1,NELEC
          IF(NTRAP(NEX).NE.IPRMSM(NEX,NP)) GO TO 52
   53     CONTINUE
C
C ... FIND THE SAME IPRMSM.
         CGCSM(NP)=CGCSM(NP)+CGC(IP)
         GO TO 51
C
C ... FIND A NEW IPRMSM.
   52    CONTINUE
        NEWP=NEWP+1
        CALL ICOPY(NELEC,NTRAP,1,IPRMSM(1,NEWP),1)
        CGCSM(NEWP)=CGC(IP)
   51   CONTINUE
   50  CONTINUE
C
      IF(NEWP.GT.MXPRM) THEN
        IF (MASWRK) WRITE(IW,9054) NEWP,MXPRM
        CALL ABRT
      END IF
      XNORM=DDOT(NEWP,CGCSM,1,CGCSM,1)
      IF(ABS(XNORM).LT.SMALL) THEN
         IF (MASWRK) WRITE(IW,156) XNORM,NPRIM,NELEC
         IF (MASWRK) WRITE(IW,*) NEWP,(CGCSM(NP),NP=1,NEWP)
         CALL ABRT
      END IF
      XNORM = ONE/SQRT(XNORM)
C
C ... DELETE PRIMITIVE FUNCTIONS WHICH HAVE ZERO COEFFICIENT.
      NPRMSM=0
      DO 62 IP=1,NEWP
        IF(ABS(CGCSM(IP)).LT.SMALL) GO TO 62
        NPRMSM=NPRMSM+1
        CALL ICOPY(NELEC,IPRMSM(1,IP),1,IPRMSM(1,NPRMSM),1)
        CGCSM(NPRMSM)=CGCSM(IP)*XNORM
        IF(OUT) WRITE(IW,660) NPRMSM,CGCSM(NPRMSM),
     +                      (IPRMSM(M,NPRMSM),M=1,NELEC)
   62 CONTINUE
      RETURN
  156 FORMAT(/1X,'ERROR IN SMINUS: XNORM=',F20.10,
     *       ' NPRIM,NELEC=',2I4)
  660 FORMAT(1X,I2,F15.8,' : SPIN= ',50I1)
 9010 FORMAT(1X,'NO. OF PRIMITIVE FUNCTIONS =',2I5)
 9011 FORMAT(10X,F20.10,20I2)
 9054 FORMAT(//1X,'ERROR: NEED MORE MEMORY.',
     *                 '  MEWP BECOMES GREATER THAN MXPRM.',
     *       //1X,'       NEWP, MXPRM=',2I5)
      END
C*MODULE SOLIB   *DECK SOBOOK
      SUBROUTINE SOBOOK(IW,ICI1,ICI2,IROOTS,SOL1MUL,SOL2MUL,STMMUL,
     *                  ALZMUL,ISTSYM,IRRR,IRRL,SYLYES,SYRYES,MAXNCO,
     *                  MULST,IRCIOR,ADD2E,NPROP,NSYMFOR,NSPAC,NZERO,
     *                  NCALC,SKIPDM,NOSYM,IPRHSO,NHSO,NTDM,MHSO,
     *                  HSO,TDM,ALZ,COPCON,COPCON1,HSOTOL,HSOMAX,
     *                  JZOPT,SAMEMUL)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C          note that mhso, the dimension of hso1+sl1, is either 1 or 2
      COMPLEX*16 HSO(NHSO,NHSO),HSOT,HSO1(2),SL1(2)
C
      LOGICAL GOPARR,DSKWRK,MASWRK,ADD2E,SYLYES(0:3),SYRYES(3),SKIPDM,
     *        SAMEMUL
C
      PARAMETER (ZERO=0.0D+00,TWO=2.0D+00)
C
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
C
      DIMENSION SOL1MUL(3,MXRT,MXRT,MHSO),SOL2MUL(3,MXRT,MXRT),
     *          STMMUL(3,MXRT,MXRT),IROOTS(*),ALZMUL(MXRT,MXRT),
     *          ISTSYM(MXRT,NUMCI),IRRL(3),IRRR(3),MULST(*),
     *          IRCIOR(MXRT,NUMCI),TDM(3,NTDM,NTDM),ALZ(NTDM,NTDM),
     *          COPCON(MHSO,MXRT,MXRT),COPCON1(MHSO,MXRT,MXRT)
      DIMENSION SOL1(3,2),SOL2(3),STM(3)
C
C     ----- OUTPUT OF FINAL RESULTS -----
C
      SQRT2=SQRT(TWO)
      IROOTI=IROOTS(ICI1)
      IROOTJ=IROOTS(ICI2)
C
C     write(6,*) 'x book'
C     write(6,1) ((SOL1MUL(1,i,j,1),i=1,irooti),j=1,irootj)
C     write(6,*) 'y book'
C     write(6,1) ((SOL1MUL(2,i,j,1),i=1,irooti),j=1,irootj)
C     write(6,*) 'z book'
C     write(6,1) ((SOL1MUL(3,i,j,1),i=1,irooti),j=1,irootj)
C   1 format(7E11.5)
C
        DO 2200 IT1=1,IROOTI
          MINIT2=1
          IF(ICI1.EQ.ICI2) MINIT2=IT1
          DO 2200 IT2=MINIT2,IROOTJ
            ITT1=IT1
            ITT2=IT2
            DO IH=1,MHSO
              CALL DCOPY(3,SOL1MUL(1,ITT1,ITT2,IH),1,SOL1(1,IH),1)
            ENDDO
            CALL DCOPY(3,SOL2MUL(1,ITT1,ITT2),1,SOL2,1)
            CALL DCOPY(3, STMMUL(1,ITT1,ITT2),1,STM ,1)
            IF(JZOPT.NE.0) THEN
               SLZ=ALZMUL(ITT1,ITT2)
            ELSE
               SLZ=ZERO
            ENDIF
            CALL CISOL(SYRYES(2),ISTSYM(ITT1,ICI1), 1,
     *                           ISTSYM(ITT2,ICI2),IRRR,0)
            CALL CISOL(SYRYES(3),ISTSYM(ITT1,ICI1), 0,
     *                           ISTSYM(ITT2,ICI2),IRRR,0)
            CALL CISOL(SYRYES(1),ISTSYM(ITT1,ICI1),-1,
     *                           ISTSYM(ITT2,ICI2),IRRR,0)
            MAXMS=1
            IF(MULST(ICI1).EQ.1.AND.MULST(ICI2).EQ.1) MAXMS=0
            DO 2100 MS=0,MAXMS
              CALL CISOL(SYLYES(MS),ISTSYM(ITT1,ICI1),MS,
     *                              ISTSYM(ITT2,ICI2),IRRL,1)
Cdg           m1=j1
Cdg           m2=j2
Cdg           if(ms.ne.0.and.ici1.eq.ici2) m1=m1-2
Cdg           if(ms.eq.0.and.ici1.ne.ici2) m2=m2-2
Cdg           if(j1.eq.0.and.j2.eq.0.and.ms.ne.0) then
Cdg                symyes(ms)=.false.
Cdg           else
Cdg                call cisom(symyes(ms),istsym(itt1,ici1),j1,m1,
Cdg  *                istsym(itt2,ici2),j2,m2,mulst,ndim,nijdr,ijdrep)
Cdg           endif
Cdg           sylyes(ms)=sylyes(ms).and.symyes(ms)
              IF(.NOT.SYLYES(MS).AND.NOSYM.EQ.0.AND.MULST(ICI1).NE.0.OR.
     *           MULST(ICI2).NE.0.OR.MS.EQ.0) NSPAC=NSPAC+1
C
              DO IH=1,MHSO
              IF(MS.EQ.0) THEN
C               <MSI/L0S0/MSJ> = <LZ> = -I<Z>
                HSO1(IH) =  DCMPLX(ZERO,-SOL1(3,IH))
                HSOT =  DCMPLX(ZERO,-SOL2(3))
              ELSE
C               <MSI/L+S-/MSJ> = <LX> +I<LY> = -I<X> +<Y>;
C               L=RXP =-I(R X D/DR)
C               was divided by two because LS= -1/2(L+S- + L-S+) + L0S0
C               now the new convention
C
                HSO1(IH) =  DCMPLX(-SOL1(2,IH),SOL1(1,IH))/SQRT2
                HSOT =  DCMPLX(-SOL2(2),SOL2(1))/SQRT2
              ENDIF
              ENDDO
              HSOT=HSOT+HSO1(1)
              IF(MHSO.GT.1) HSOT=HSOT+HSO1(2)
C
C             <MSI/L-S+/MSJ> = <LX> -I<LY> = -I<X> -<Y>;
C                L=RXP =-I(R X D/DR)
C              HSOCR = -SOLY(ms)
C              HSOCI = -SOLX(ms)
C
               CALL PROPZEFF(ICI1,ICI2,ITT1,ITT2,MS,MAXNCO,HSOT,HSO1,
     *                       SL1,STM,SLZ,MULST,IROOTS,ISTSYM,IRCIOR,
     *                       ADD2E,NPROP,NSYMFOR,NZERO,NCALC,
     *                       SYLYES(MS),SYRYES,SKIPDM,NOSYM,IPRHSO,
     *                       NHSO,NTDM,MHSO,HSO,TDM,ALZ,
     *                       COPCON(1,ITT1,ITT2),COPCON1(1,ITT1,ITT2),
     *                       HSOTOL,JZOPT,SAMEMUL)
               HSOMAX=MAX(HSOMAX,ABS(HSOT))
 2100   CONTINUE
        IF(MASWRK.AND.IPRHSO.GE.0.AND.MOD(IPRHSO/2,2).NE.1)
     *     WRITE(IW,*) ' '
 2200 CONTINUE
      RETURN
      END
C*MODULE SOLIB   *DECK SOCINFO
      SUBROUTINE SOCINFO(NOSYM,IPRHSO)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA DIPMOM/8HDM      /,ZEFF/8HHSOZEFF /
#else
      CHARACTER*8 :: DIPMOM_STR
      EQUIVALENCE (DIPMOM, DIPMOM_STR)
      CHARACTER*8 :: ZEFF_STR
      EQUIVALENCE (ZEFF, ZEFF_STR)
      DATA DIPMOM_STR/"DM      "/,ZEFF_STR/"HSOZEFF "/
#endif
C
C     notation print-out
C
      WRITE(IW,9013)
      WRITE(IW,9015)
      IF(OPERR.NE.DIPMOM) WRITE(IW,9017)
      WRITE(IW,9019)
      IF(OPERR.NE.DIPMOM) WRITE(IW,9021)
      IF(OPERR.EQ.ZEFF) WRITE(IW,9023)
      IF(NOSYM.GT.0) WRITE(IW,9020)
      IF(NOSYM.LE.0) WRITE(IW,9030)
      IF(IPRHSO.EQ.0.AND.OPERR.NE.DIPMOM) WRITE(IW,9040)
      IF(OPERR.NE.DIPMOM) WRITE(IW,9050)
      RETURN
 9013 FORMAT(/1X,'THE FOLLOWING LEGEND IS USED BELOW:')
 9015 FORMAT(1X,'[C] - THE MATRIX ELEMENT ACTUALLY CALCULATED')
 9017 FORMAT(1X,
     *  '[P] - OBTAINED FROM [C] BY THE WIGNER-ECKART THEOREM (<HSO>)')
 9019 FORMAT(1X,'[Z] - ZERO BY POINT GROUP SYMMETRY')
 9021 FORMAT(1X,'[W] - ZERO BY THE WIGNER-ECKART THEOREM (<HSO>)')
 9023 FORMAT(1X,'[S] - ZERO BY SPIN ORTHOGONALITY (<R>)')
 9020 FORMAT(/1X,'[Z]''S ARE ACTUALLY CALCULATED.',/)
 9030 FORMAT(/1X,'[Z]''S ARE NOT CALCULATED.',/)
 9040 FORMAT(/1X,'CI STATE FORMAT:   N-(2S+1)G:B',
     *       /1X,'G- IRREP LABEL, B NUMBERS DEGENERATE COMPONENTS OF G',
     *       /1X,'N NUMBERS EQUIVALENT TERMS (2S+1)G:B')
 9050 FORMAT(/1X,'STATE ID FORMAT: NCI*1000+IROOT,',
     *       /1X,'WHERE NCI IS THE NTH $DRT GROUP',
     *       /1X,'AND IROOT IS THE CI ROOT''S ORDINAL NUMBER.',/)
      END
C*MODULE SOLIB   *DECK SOINTS
      SUBROUTINE SOINTS(MAXL,L1,L2,ZEFF,IRECK1,IRECK2,N2INT,NSO2BUF,
     *                  MAXNCO,NRES,NFT2SO,MAXIA,INTREST,ENUCR,GSYLYES,
     *                  DISTINT,MCP2E,IONECNT,DBGINT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL DBG,GOPARR,DSKWRK,MASWRK,GSYLYES(3),DISTINT,DBGINT,UNCON
      PARAMETER (TWO=2.0D+00,MXATM=500,MXMPSH=2*MXATM,MXMPGT=5*MXMPSH)
      COMMON /FMCOM / X(1)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /PCKLAB/ LABSIZ
      COMMON /RELWFN/ RMETHOD,QRQMT,CLIG,CLIG2,QRTOL,IQRORD,MODQR,NESOC,
     *                NRATOM,NUMU,NQMTR,NQRDAF,MORDA,NDARELB
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      DIMENSION KARTEN(0:4),N2INT(3),ZEFF(*),WORDS(3),NFT2SO(3),ASYM(3),
     *        ITAG(3)
      COMPLEX*16 HSO,HSO1
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA KARTEN/1,4,6,10,15/,WORDS/8HONE     ,8HTWO     ,8HTWO     /,
     *     RESC/8HRESC    /,ANONE/8HNONE    /,DK/8HDK      /,
     *     ITAG/4HRELX,4HRELY,4HRELZ/
#else
      CHARACTER*8 :: WORDS_STR(3)
      EQUIVALENCE (WORDS, WORDS_STR)
      CHARACTER*8 :: RESC_STR
      EQUIVALENCE (RESC, RESC_STR)
      CHARACTER*8 :: ANONE_STR
      EQUIVALENCE (ANONE, ANONE_STR)
      CHARACTER*8 :: DK_STR
      EQUIVALENCE (DK, DK_STR)
      CHARACTER*4 :: ITAG_STR(3)
      EQUIVALENCE (ITAG, ITAG_STR)
      DATA KARTEN/1,4,6,10,15/,WORDS_STR/"ONE     ","TWO     ",
     *     "TWO     "/,
     *     RESC_STR/"RESC    "/,ANONE_STR/"NONE    "/,
     *     DK_STR/"DK      "/,
     *     ITAG_STR/"RELX","RELY","RELZ"/
#endif
C
C     SOC integrals calculation and storage
C
      DBG=.FALSE.
C
      CALL SETPNRM
C
      UNCON=RMETHOD.NE.ANONE.AND.MOD(MODQR,2).EQ.1
      LU1=NUMU
      IF(UNCON.AND.NESOC.LE.0) LU1=L1
      LU2=(LU1*LU1+LU1)/2
C     for uncon .false. lu1=l1
C
      MAXFUN=KARTEN(MAXL)
      MAXP=MAXL+1
      MAXP1=MAXP+1
      ND51=MAXP*MAXP
      ND52=MAXP1*MAXP1
      N2AO=MAXFUN
C
C     the choice of the buffer size nso2buf is tricky.
C     it can be any number
C
      CALL VALFM(LOADFM)
      CALL GOTFM(NGOT)
      IF(NGOT.GE.NRES) NGOT=NGOT-NRES
C
      MIN1=2**16-2**5
C           this code tries to engross entire value
C     NSO2BUF=NGOT/3
C     NSO2BUF=MAX(NSO2BUF,MIN1)
C     MAX1=L2*(L2+1)*(LABSIZ/NWDVAR+1)
C     if(nt.gt.1) max1=max1/(nt-1)
C     NSO2BUF=MIN(NSO2BUF,MAX1)
C           this tries to use only a fixed, nonhuge amount
      NSO2BUF = MIN1
C
      NSAVE=MAXNCO
      IF(MAXNCO.EQ.3) NSAVE=2
      IF(MCP2E.EQ.1) NSAVE=1
      IF(INTREST.NE.0) THEN
         NSAVE=1
         NSO2BUF=MAX(N2INT(1),N2INT(2),N2INT(3))
      ENDIF
      CALL VICLR(N2INT,1,3)
      LSO1AO= LOADFM +1
      LIA   = LSO1AO +LU2*3
      LXINT = LIA    +(MAXIA-1)/NWDVAR+1
      LYINT = LXINT  +ND52*ND51
      LZINT = LYINT  +ND52*ND51
      LXINTI= LZINT  +ND52*ND51
      LYINTI= LXINTI +ND52*ND51
      LZINTI= LYINTI +ND52*ND51
      LXINTJ= LZINTI +ND52*ND51
      LYINTJ= LXINTJ +ND52*ND51
      LZINTJ= LYINTJ +ND52*ND51
      LAST  = LZINTJ +ND52*ND51
      IF(NSAVE.GT.1) THEN
         LSO2AO = LAST
         LSO2BUF= LSO2AO+3*N2AO*N2AO*N2AO*N2AO
         LAST   = LSO2BUF+NSO2BUF*3
      ELSE
         LSO2BUF= LAST
         LSO2AO = LAST
      ENDIF
      IF((RMETHOD.EQ.RESC.OR.RMETHOD.EQ.DK).AND.NESOC.GT.0) THEN
         LRESCAO=LAST
         LLEFTRA=LRESCAO+LU1*LU1
         LRIGHTRA=LLEFTRA+LU1*LU1
         LAST=LRIGHTRA+LU1*LU1
      ENDIF
      IF(MCP2E.GT.0) THEN
         LCORINT=LAST
         LCORIX=LCORINT+L1*L1*3
         LATOM=LCORIX+L1*L1*3
         LCG=LATOM+(MXMPSH-1)/NWDVAR+1
         LAST=LCG+MXMPGT
      ENDIF
      NEED  = LAST-LOADFM-1
      CALL GETFM(NEED)
C
C     save info for restart check
C
      IF(NSAVE.EQ.2) THEN
         DO I=1,3
            WRITE(NFT2SO(I)) NWDVAR,LABSIZ,L2,NSO2BUF,IONECNT,ENUCR
         ENDDO
      ENDIF
      IF(MASWRK) THEN
C        WRITE(IW,9000) WORDS(MAXNCO)
         WRITE(IW,9000) WORDS(NSAVE)
         IF(MCP2E.EQ.1) WRITE(IW,9001)
         IF(MCP2E.EQ.2) WRITE(IW,9002)
         IF(IONECNT.EQ.1) WRITE(IW,9003)
         IF(IONECNT.EQ.2) WRITE(IW,9004)
         WRITE(IW,9005) NEED
         IF(GOPARR.AND.DISTINT) WRITE(IW,9006) 'DISTRIBUTED'
         IF(GOPARR.AND..NOT.DISTINT) WRITE(IW,9006) 'DUPLICATED'
      ENDIF
C
C     compute MCP 2e SOC integrals
C
      IF(MCP2E.GT.0) CALL MCP2EINT(L1,X(LCORINT),X(LCORIX),X(LATOM),
     *                 X(LCG),GSYLYES,MAXP,MAXP1,ND51,ND52,X(LXINT),
     *                 X(LYINT),X(LZINT),X(LXINTI),X(LYINTI),X(LZINTI),
     *                 X(LXINTJ),X(LYINTJ),X(LZINTJ),IONECNT,DBGINT)
C
      CALL PREINIT(MAXIA,X,X,X(LIA),0)
C
C     for internally uncontracted basis set, if RESC corrections are
C     requested (nesoc>0), two electron SOC integrals are computed in
C     the contracted basis set as no 2e RESC corrections are implemented
C     in this case (it would require a *major* computational effort).
C     One electron integrals are computed in the uncontracted basis set,
C     in order to compute RESC corrections.
C
      IF(.NOT.UNCON.OR.NSAVE.GT.1.OR.NESOC.EQ.0)
     *CALL BRTHSO(HSO,HSO1,1,1,1,1,KARTEN,X,X,X,X,X,X,X,X,X,X,X,X,X,X,
     *            X,X,X,X,X,X,1,MAXFUN,MAXP,MAXP1,ND51,ND52,X(LXINT),
     *            X(LYINT),X(LZINT),X(LXINTI),X(LYINTI),X(LZINTI),
     *            X(LXINTJ),X(LYINTJ),X(LZINTJ),X(LIA),ZEFF,NSAVE,L2,
     *            X(LSO1AO),N2AO,X(LSO2AO),NSO2BUF,X(LSO2BUF),N2INT,
     *            NFT2SO,GSYLYES,DISTINT,IONECNT,DBG)
      IF(UNCON.AND.NESOC.GT.0) THEN
C       switch to the uncontracted basis set
        CALL FLIPBASIS(17)
        CALL BRTHSO(HSO,HSO1,1,1,1,1,KARTEN,X,X,X,X,X,X,X,X,X,X,X,X,X,X,
     *            X,X,X,X,X,X,1,MAXFUN,MAXP,MAXP1,ND51,ND52,X(LXINT),
     *            X(LYINT),X(LZINT),X(LXINTI),X(LYINTI),X(LZINTI),
     *            X(LXINTJ),X(LYINTJ),X(LZINTJ),X(LIA),ZEFF,  1  ,LU2,
     *            X(LSO1AO),N2AO,X(LSO2AO),NSO2BUF,X(LSO2BUF),N2INT,
     *            NFT2SO,GSYLYES,DISTINT,IONECNT,DBG)
        CALL FLIPBASIS(15)
C       switch back to the contracted basis set
      ENDIF
      IF(MASWRK.AND.NSAVE.GT.1) WRITE(IW,9010) (N2INT(I),I=1,3)
C
C     lastly, add relativistic corrections to SOC integrals if needed
C     and save the 1e SOC integrals to disk
C
      DO 150 I=1,3
         LSO1AOI=LSO1AO+LU2*(I-1)
         IF((RMETHOD.EQ.RESC.OR.RMETHOD.EQ.DK).AND.NESOC.GT.0) THEN
C
C       RESC transformation of 1e integrals. Note that it can be
C       combined with the MO 2-index transf but only for NUMVEC=1
C       the transformation below is valid in general case
C       read left and right transformation matrices A and B
C       RESC transforms AO ints -> A * AO ints * Bt
C       t stands for transposed (RESC uses math not phys convention)
C
           CALL DAREAD(IDAF,IODA,X(LLEFTRA),LU1*LU1,NDARELB,0)
           CALL DSCAL(LU1*LU1,TWO*CLIG,X(LLEFTRA),1)
           CALL TRPOSQ(X(LLEFTRA),LU1)
           CALL DAREAD(IDAF,IODA,X(LRIGHTRA),LU1*LU1,NDARELB+1,0)
           CALL DSCAL(LU1*LU1,TWO*CLIG,X(LRIGHTRA),1)
           CALL TRPOSQ(X(LRIGHTRA),LU1)
           CALL TMOINT(X(LRESCAO),X(LLEFTRA),X(LRIGHTRA),X(LSO1AOI),
     *                 ITAG(I),2,LU1,LU1,LU2,DBGINT)
           IF(DBGINT) THEN
              WRITE(IW,*) 'BEFORE SYMMETRISATION ',ITAG(I)
              CALL PRSQ(X(LRESCAO),LU1,LU1,LU1)
           ENDIF
           CALL ASYMTRZE(X(LRESCAO),LU1,LU1,ASYM(I))
           IF(DBGINT) THEN
              WRITE(IW,*) 'AFTER SYMMETRISATION ',ITAG(I)
              CALL PRSQ(X(LRESCAO),LU1,LU1,LU1)
           ENDIF
C          copy should not average
           CALL CPYSQT(X(LRESCAO),X(LSO1AOI),LU1,1)
           IF(UNCON) CALL CONTRAM(X(LRESCAO),X(LSO1AOI),LU2,0,-1)
           IF(DBGINT.AND.UNCON) THEN
              WRITE(IW,*) 'AFTER RECONTRACTION ',ITAG(I)
              CALL PRTRIL(X(LRESCAO),L1)
           ENDIF
         ENDIF
         CALL DAWRIT(IDAF,IODA,X(LSO1AOI),L2,IRECK1+I,0)
         IF(MCP2E.GT.0)
     *   CALL DAWRIT(IDAF,IODA,X(LCORINT+(I-1)*L1*L1),L1*L1,IRECK2+I,0)
  150 CONTINUE
      IF(RMETHOD.EQ.RESC.AND.NESOC.GT.0.AND.MASWRK)
     *   WRITE(IW,8888) ASYM(1),ASYM(2),ASYM(3)
      IF(MASWRK.AND.RMETHOD.EQ.RESC.AND.NESOC.GT.1
     *   .AND.MAXNCO.EQ.2.AND.NUMVEC.EQ.2) WRITE(IW,9020)
C
      CALL RETFM(NEED)
      RETURN
 8888 FORMAT(1X,'AVERAGE NON-ANTISYMMETRY INTRODUCED BY RESC TRANSFORM',
     *   'ATION IS:'/
     *     1X,1P,3E12.4,' CM-1 PER X,Y,Z ONE-ELECTRON INTEGRAL.'/)
 9000 FORMAT(/9X,37("-")/10X,A3,'-ELECTRON SPIN-ORBIT AO INTEGRALS')
 9001 FORMAT(20X,'MCP CORE-ACTIVE 2E')
 9002 FORMAT(11X,'MCP CORE-ACTIVE 2E AND VALENCE 2E')
 9003 FORMAT(12X,'ONE-CENTER 1E AND ONE-CENTER 2E')
 9004 FORMAT(15X,'ALL 1E AND ONE-CENTER 2E')
 9005 FORMAT(9X,37("-")/9X,'USING ',I10,' WORDS OF MEMORY')
 9006 FORMAT(9X,'2E SOC INTEGRALS WILL BE ',A11)
 9010 FORMAT(/9X,'SAVING 2E INTEGRALS:',I10,'(LX),',I10,'(LY),',I10,
     *       '(LZ)')
 9020 FORMAT(/7X,'WARNING: ACTIVE-ACTIVE 2E AVERAGED SYMMETRISATION',
     *       ' WILL NOT BE PERFORMED.',/)
      END
C*MODULE SOLIB   *DECK SOSTATS
      SUBROUTINE SOSTATS(NCALC,NZERO,NPROP,NHERM,NSPRO,NSPAC,NSYMFOR,
     *                   NOSYM,NHSO,HSOTOL,HSOMAX)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO=0.0D+00)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      COMMON /SYMBLK/ NIRRED,NSALC,NSALC2,NSALC3
C
C     fallen out of favour symmetry statistics
C
      NCALC1=NCALC
      IF(NCALC.EQ.0) NCALC1=1
      WRITE(IW,9700) NCALC,NCALC*100./(NHSO*NHSO),NZERO,NZERO*100./
     *               NCALC1,NHSO*(NHSO-1)/2,NHERM,NPROP,NSPRO,NSPAC,
     *               NHSO*NHSO-NCALC-NHSO*(NHSO-1)/2-NPROP-NHERM-
     *               NSPRO-NSPAC,NHSO*NHSO
      IF(NOSYM.GT.0) WRITE(IW,9710) NSYMFOR
      IF(NIRRED.LE.1) HSOMAX=ZERO
      WRITE(IW,9720) SYMTOL*HSOMAX,HSOTOL
      RETURN
 9700 FORMAT(/1X,'SYMMETRY STATISTICS:',/,
     *   1X,I5,' ELEMENTS ACTUALLY CALCULATED (',F3.0,'%)'/
     *   1X,I5,' OF THEM ARE ZERO (',F4.0,'%) EXTRA WORK)'/
     *   1X,I5,' ELEMENTS OBTAINED BY HERMITICITY OF HSO'/
     *   1X,I5,' ELEMENTS OBTAINED WITH THE WIGNER-ECKART THEOREM',/
     *   1X,I5,' ELEMENTS ARE ZERO BY HERMITICITY OF HSO'/
     *   1X,I5,' ELEMENTS ARE ZERO BY THE WIGNER-ECKART THEOREM (*)'/
     *   1X,I5,' ELEMENTS ARE ZERO BY SPACE SYMMETRY (*)'/
     *   1X,I5,' ELEMENTS ARE ZERO BY (*) NOT INCLUDED IN THE ABOVE'/
     *   1X,'------------------------------------------------------'/
     *   1X,I5,' TOTAL NUMBER OF ELEMENTS'/)
 9710 FORMAT(1X,I5,' ELEMENTS FORBIDDEN BY SYMMETRY ARE NONZERO',/)
 9720 FORMAT(/5X,'THE MAX HSO MATRIX ELEMENT ABSOLUTE ERROR IS ',
     *            1P,E8.2,' CM-1.',
     *       /19X,'THE EXPECTED ERROR (SET IN HSOTOL) IS ',1P,E8.2,/)
      END
C*MODULE SOLIB   *DECK SPNJZ
      SUBROUTINE SPNJZ(NHSO,NTDM,HSO,ALZ,AJZMAT,EIG,RWORK,WORK,MULST,
     *                IROOTS,L2VAL,INDX,OMEGA,GLOLAB,PRTPRM,PRTLZ,DEBUG)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (ZERO=0.0D+00,TWO=2.0D+00)
      DIMENSION ALZ(NTDM,NTDM),EIG(*),RWORK(*),MULST(*),IROOTS(*),
     *          OMEGA(*),INDX(*),L2VAL(*)
      LOGICAL GOPARR,DSKWRK,MASWRK,PRTPRM,DEBUG,PRTLZ
      CHARACTER*6 GLOLAB
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      COMPLEX*16 HSO(NHSO,NHSO),AJZMAT(NHSO,NHSO),WORK(*)
C
C     calculate expectation value of Jz=Lz+Sz for the spin-mixed states
C
      IF(PRTPRM.AND.MASWRK) THEN
         WRITE(IW,8800)
         CALL PRSQ(ALZ,NTDM,NTDM,NTDM)
      ENDIF
C     diagonalise Lz for diatomic molecules to guide in state assignment
C     First multiply by -i to get Lz from "[r x d/dr]".
C     if(linear.and.nat.gt.1) then
      IF(PRTLZ) THEN
        DO I=1,NTDM
          DO J=1,NTDM
            AJZMAT(I,J)=DCMPLX(ZERO,-ALZ(I,J))
          ENDDO
        ENDDO
        CALL ZHEEV('V','L',NTDM,AJZMAT,NHSO,EIG,WORK,NHSO*2,RWORK,INFO)
        IF(INFO.NE.0) THEN
          IF (MASWRK) WRITE(IW,9240) INFO
        ENDIF
C       write(6,*) 'Lz values are:',ntdm
C       call prsq(eig,ntdm,1,1)
        TOLZ=SYMTOL*1.0D+01
        INDEX=0
        IF(MASWRK) WRITE(6,8810) GLOLAB
        DO ICI1=1,NUMCI
          LINP=L2VAL(ICI1)
          DO IROOT=1,IROOTS(ICI1)
            INDEX=INDEX+1
            NLZ=0
            CALL VCLR(RWORK,1,NTDM)
            INDEX1=0
            DO ICI2=1,NUMCI
              DO JROOT=1,IROOTS(ICI2)
                INDEX1=INDEX1+1
                CC=ABS(AJZMAT(INDEX,INDEX1))
                IF(CC*CC.GT.TOLZ) THEN
C                 avoid adding the same Lz value twice
                IF(NLZ.NE.0.AND.ABS(EIG(INDEX1)-EIG(INDX(NLZ))).LT.TOLZ)
     *          THEN
                ELSE
                  NLZ=NLZ+1
                  INDX(NLZ)=INDEX1
                ENDIF
                ENDIF
                RWORK(NLZ)=RWORK(NLZ)+CC*CC*1.0D+02
              ENDDO
            ENDDO
            IF(MASWRK) THEN
               IF(LINP.GE.0) WRITE(IW,8825) ICI1*1000+IROOT,LINP,
     *                            (EIG(INDX(I)),RWORK(I),I=1,NLZ)
               IF(LINP.LT.0) WRITE(IW,8820) ICI1*1000+IROOT,
     *                            (EIG(INDX(I)),RWORK(I),I=1,NLZ)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C     first compute Jz in the LS state basis.
C     IF (MASWRK) WRITE(IW,8900) GLOLAB
      CALL VCLR(AJZMAT,1,NHSO*NHSO*2)
      IND=0
      INDEX=0
      DO 300 ICI1=1,NUMCI
        MUL1=MULST(ICI1)
        SS=(MUL1+1)/TWO
C       nota bene: mul1 is 2S+1 and similarly ms1 is not Ms
        DO 300 IROOT=1,IROOTS(ICI1)
          IND=IND+1
          DO 200 MS1=MUL1,1,-1
            INDEX=INDEX+1
            JND=0
            JNDEX=MUL1-MS1+1
            DO 110 ICI2=1,NUMCI
              MUL2=MULST(ICI2)
              IF(MUL2.NE.MUL1) THEN
                JND=JND+IROOTS(ICI2)
                JNDEX=JNDEX+MUL2*IROOTS(ICI2)
                GOTO 110
              ENDIF
              DO 100 JROOT=1,IROOTS(ICI2)
                JND=JND+1
C
C               fill the value for ms2=ms1
C               Jz matrix is diagonal-blocked (which means it is made of
C               blocks and each block is a diagonal matrix; blocks are
C               all over the matrix).
C               lz=-ALZ(IND,JND)
C
                AJZMAT(INDEX,JNDEX)=DCMPLX(ZERO,-ALZ(IND,JND))
C
C               -i*Lz apears as we calculate not Lz, but [r x d/dr]z
                JNDEX=JNDEX+MUL2
  100         CONTINUE
  110       CONTINUE
            AJZMAT(INDEX,INDEX)=AJZMAT(INDEX,INDEX)+DCMPLX(MS1-SS,ZERO)
C
C           add Sz: ms1-ss reempts Ms (as ms1 runs from 1 to 2S+1)
C           thus real part of Jz is Sz, imaginary part is -[r x d/dr]z.
C
  200     CONTINUE
  300 CONTINUE
C
C     now do the 2-index transformation (similarity transformation)
C     convert Jz to the SOC state basis.
C     This transformation seems to be cheaper compared to a 4-nested
C     loop that uses sparseness of Jz.
C     Jz'=C-dagger * Jz * C
C
      CALL ZTFSQU(AJZMAT,HSO,WORK,NHSO)
C     IF(I.EQ.J.AND.MASWRK) WRITE(IW,9000) I,LZ,DREAL(SZ),DREAL(LZ+SZ)
      IF(DEBUG) CALL ZPUSQL(AJZMAT,NHSO,NHSO,NHSO)
      CALL ZHEEV('V','L',NHSO,AJZMAT,NHSO,EIG,WORK,NHSO*2,RWORK,INFO)
      IF(INFO.NE.0) THEN
         IF (MASWRK) WRITE(IW,9240) INFO
      ENDIF
      IF (MASWRK) WRITE(IW,9500) GLOLAB
      ZMAX=SYMTOL*1.0D+01
C     here the array rwork is treacherously reused
C     Initialise omega with nonsense (Omega=|Jz|)
C     call dacopy(nhso,-two,omega,1)
      DO 800 I=1,NHSO
         NJZ=0
         CALL VCLR(RWORK,1,NHSO)
         DO 700 J=1,NHSO
            Z=ABS(AJZMAT(I,J))
            IF(Z*Z.GE.ZMAX) THEN
              IF(NJZ.NE.0.AND.ABS(EIG(J)-EIG(INDX(NJZ))).LT.ZMAX) THEN
              ELSE
                 NJZ=NJZ+1
                 INDX(NJZ)=J
              ENDIF
            ENDIF
            RWORK(NJZ)=RWORK(NJZ)+Z*Z*1.0D+02
C             RWORK(NJZ)=EIG(J)
  700    CONTINUE
         IF(MASWRK) WRITE(IW,9510) I,(EIG(INDX(J)),RWORK(J),J=1,NJZ)
C
C        simplified algorithm to assign omega. Will find Jz with the
C        max weight, not Omega. In case of bad contamination these
C        are not the same.
C
         INDW=IDAMAX(NJZ,RWORK,1)
         OOMEGA=ABS(EIG(INDX(INDW)))
         OOW=RWORK(INDW)
         ZMAX1=ZMAX*1.0D+01
C        Find Jz and -Jz equal to omega
         DO J=1,NJZ
           IF(J.NE.INDW.AND.ABS(ABS(EIG(INDX(J)))-OOMEGA).LT.ZMAX1)
     *       OOW=OOW+RWORK(J)
         ENDDO
         IF(ABS(1.0D+02-OOW).GT.ZMAX1*1.0D+02) OOMEGA=-OOMEGA
C        write(6,*) 'www',oomega,oow,njz
C        negative sign indicates questionable assignment
         OMEGA(I)=OOMEGA
  800 CONTINUE
C
C     PRINT OUT Jz diagonalisation RESULTS:
      IF(PRTPRM) THEN
         IF (MASWRK) WRITE(IW,9400)
         CALL ZPRSQL(NUMCI,NHSO,AJZMAT,EIG,INDX,MULST,IROOTS,IW,1)
C        IF (MASWRK) WRITE(iw,9417)
C        IF (MASWRK) WRITE(iw,9406) (EIG(I),I=1,NHSO)
      ELSE
         LOOP = (NHSO-1)/5 +1
         IF (MASWRK) WRITE(IW,9417)
         DO 600 I=1,LOOP
           J2 = I*5
           J1 = J2 -4
           IF(J2.GT.NHSO) J2=NHSO
C          IF (MASWRK) WRITE(Iw,9404) (j,indx(j),j=j1,j2)
           IF (MASWRK) WRITE(IW,9406) (EIG(J),J=J1,J2)
  600      CONTINUE
      ENDIF
      RETURN
 8800 FORMAT(/1X,'-I*LZ MATRIX IN THE CI STATE BASIS',/)
 8810 FORMAT(/1X,'ASSIGNMENT OF LZ VALUES TO THE ',A6,' LS STATES.',
     *       /1X,'WARNING: THIS ASSIGNMENT WILL NOT WORK IF NOT ALL ',
     *           'DEGENERATE COMPONENTS',/1X,'ARE PROVIDED '
     *          ,'(E.G., IF ONLY PI-X, BUT NOT PI-Y IS INCLUDED.)')
 8820 FORMAT(1X,'STATE',I6,': <LZ>=',99(F6.1,'(',F5.1,'%)'))
 8825 FORMAT(1X,'STATE',I6,': <LZ> INPUT:',I2,', ACTUAL ',99(F6.1,
     *          '(',F5.1,'%)'))
C8900 FORMAT(/1X,'DIAGONAL ELEMENTS OF THE JZ=LZ+SZ MATRIX IN THE',
C    *       /1X,'BASIS OF SO ',A6,' HAMILTONIAN EIGENVECTORS.'/)
C9000 FORMAT(1X,'STATE ',I4,': <LZ>=',2F8.4,'I, <SZ>=',F8.4,', <JZ>=',
C    *       F8.4)
 9240 FORMAT(/1X,'ERROR IN SPNJZ:  INFO=',I6)
 9400 FORMAT(//1X,'---------------------------------------------',
     *        /1X,'EIGENVALUES AND EIGENVECTORS OF THE JZ MATRIX',
     *        /1X,' IN THE BASIS OF SO HAMILTONIAN EIGENVECTORS ',
     *        /1X,'---------------------------------------------')
C9404 FORMAT(1X,5(i6,'(',i7,')'))
 9406 FORMAT(1X,5F15.4)
 9417 FORMAT(/3X,'EIGENVALUES OF THE JZ MATRIX ---')
 9500 FORMAT(/1X,'ASSIGNMENT OF JZ VALUES TO THE EIGENVECTORS OF SO ',
     *       A6,' HAMILTONIAN:',/)
 9510 FORMAT(1X,'STATE',I4,': <JZ>=',99(F6.1,'(',F5.1,'%)'))
      END
C
C*MODULE SOLIB   *DECK SPNTMX
      SUBROUTINE SPNTMX(N1,NHSO,NTDM,HSO,EIG,TDM,INDX,MULST,IROOTS,
     *                  GLOLAB)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMPLEX*16 HSO(NHSO,NHSO),COEF,CTOT(3),ZDOTC,CZERO
      CHARACTER*6 GLOLAB
      PARAMETER (MXAO=2047)
      PARAMETER (SMALL=1.0D-06, ZERO=0.0D+00)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      DIMENSION EIG(NHSO),TDM(3,NTDM,NTDM),INDX(NHSO),MULST(*),IROOTS(*)
C
C     transition moment between two spin-orbit couples states
      CZERO=DCMPLX(ZERO,ZERO)
      IF (MASWRK) WRITE(IW,9010) GLOLAB,N1,NHSO,NUMCI
      IF(N1.GT.MXAO) CALL ABRT
      WRITE(IP,9292)
      ICTM=0
      DO 100 I=1,NHSO
        DO 200 J=I,NHSO
C   i and j run over spin-orbit states (column indices in Hso matrix)
          CTOT(1) = CZERO
          CTOT(2) = CZERO
          CTOT(3) = CZERO
          IND1=0
          INDR1=0
          DO 300 ICI1=1,NUMCI
            DO 400 IROOT=1,IROOTS(ICI1)
              INDR1=INDR1+1
              IND2=0
              INDR2=0
              DO 600 ICI2=1,NUMCI
                IF(MULST(ICI1).NE.MULST(ICI2)) THEN
                  INDR2=INDR2+IROOTS(ICI2)
                  IND2=IND2+IROOTS(ICI2)*MULST(ICI2)
                  GOTO 600
                ENDIF
                DO 700 JROOT=1,IROOTS(ICI2)
                  INDR2=INDR2+1
                  COEF=CZERO
                  DO 800 MS=1,MULST(ICI2)
                    COEF=COEF+DCONJG(HSO(IND1+MS,I))*HSO(IND2+MS,J)
C                   write(6,*) 'www',i,j,IND1+ms,IND2+ms,HSO(IND1+ms,I),
C                              HSO(IND2+ms,J)
  800             CONTINUE
                  IK1=MIN(INDR1,INDR2)
                  IK2=MAX(INDR1,INDR2)
                  DO 900 K=1,3
                    CTOT(K)=CTOT(K)+COEF*TDM(K,IK1,IK2)
C               write(6,*) 'wwwc',k,coef,TDM(K,IK1,IK2),'res ',CTOT(K)
  900             CONTINUE
                  IND2=IND2+MULST(ICI2)
  700           CONTINUE
  600         CONTINUE
              IND1=IND1+MULST(ICI1)
  400       CONTINUE
  300       CONTINUE
C
C       tdm is independent of ms, so the initial sum:
C       tmij(,i,j) = sum(k,l) dconjg(hso(k,i))*hso(l,j)*tdm(,k,l)
C       is rewritten by splitting k and l into ici,iroots and ms
C       tmij(,i,j) = sum(c) sum(d) tdm(,c,d) *
C                    sum(ms) dconjg(hso(cms,i)) * hso(dms,j)
C       where c and d include ici+iroot each
C       note that tdm is filled as upper triangular matrix (symmetric)
C       the sum can be rearranged to use the symmetricity
C       also ignored tdm(,c,d)=0 if c and d are of diff. multiplicities
C
#if defined(SPEC_CPU)
            TMIJ = SQRT(DBLE(ZDOTC(3,CTOT,1,CTOT,1)))
#else
            TMIJ = SQRT(DREAL(ZDOTC(3,CTOT,1,CTOT,1)))
#endif
            IF(TMIJ.GT.SMALL) THEN
               ICTM = ICTM +1
               IF(ICTM.EQ.1.AND.MASWRK) WRITE(IW,9290)
               EDIFF = EIG(J) - EIG(I)
               IF (MASWRK) WRITE(IW,9302) I,INDX(I),J,INDX(J),EDIFF,
     *                        CTOT(1),CTOT(2),CTOT(3),TMIJ
               WRITE(IP,9304) I,INDX(I),J,INDX(J),EDIFF,
     *                        CTOT(1),CTOT(2),CTOT(3),TMIJ
               ENDIF
C
C           Calculate oscillator strength:
C           in preparation.
C
  200   CONTINUE
  100 CONTINUE
      IF(ICTM.EQ.0.AND.MASWRK) WRITE(IW,9333)
      WRITE(IP,9330)
      RETURN
 9010 FORMAT(/1X,'***** START CALCULATING TRANSITION MOMENT ',
     *           'AMONG SPIN-MIXED ',A6,' STATES *****',
     *      //1X,'NUMBER OF ORBITALS            =',I5,
     *       /1X,'DIMENSION OF HSO MATRIX       =',I5,
     *       /1X,'NUMBER OF SPIN-MULTIPLICITIES =',I5,
     *      //1X,'NOTE THAT T(X),T(Y),T(Z) ARE COMPLEX NUMBERS.')
 9290 FORMAT(//8X,'STATES',9X,'ENERGY DIFF.',9X,'T(X)',19X,'T(Y)',19X,
     *       'T(Z)',14X,'TM')
 9292 FORMAT(' $SPNTRN MOMENT')
 9302 FORMAT(1X,2(I4,'(',I5,')'),F12.2,3(2F11.5,'I'),F11.5)
 9304 FORMAT(2(I4,I5),F12.2,7F13.6)
 9330 FORMAT(' $END')
 9333 FORMAT(/1X,'NO EFFECTIVE TRANSITIONS AMONG THESE STATES !')
      END
C*MODULE SOLIB    *DECK TMOINT
      SUBROUTINE TMOINT(MOINT,VST1,VST2,AOINT,LABEL,IFORM,N1,L1,L2,DBG)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION MOINT
C
      PARAMETER(MXAO=2047)
C
      DIMENSION MOINT(N1,N1),AOINT(L2),VST1(L1,N1),VST2(L1,N1)
C
      LOGICAL DBG
C
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
C
      PARAMETER (ZERO=0.0D+00)
C
C     ----- TRANSFORM INTEGRALS TO MO TRANSITION FORMAT -----
C               MOINT = VST1-DAGGER * AOINT * VST2
C     AOINT IS IN TRIANGULAR STORAGE, THE ARGUMENT IFORM TELLS
C     WHETHER THIS MATRIX IS SYMMETRIC OR ANTISYMMETRIC.
C     THE OTHER ARRAYS ARE RECTANGULAR.
C
      IF(DBG) THEN
         WRITE(IW,9000) LABEL
         CALL PRTRIL(AOINT,L1)
      END IF
C
      DO 160 I=1,N1
         DO 150 J=1,N1
            SUM=ZERO
            DO 120 K=1,L1
               DO 110 L=1,L1
                  IF(K.GT.L) THEN
                     KL = IA(K) + L
                     XINTAO=AOINT(KL)
                  ELSE
                     KL = IA(L) + K
                     XINTAO=AOINT(KL)
                     IF(IFORM.EQ.2) XINTAO=-XINTAO
                  END IF
                  SUM=SUM+VST1(K,I)*XINTAO*VST2(L,J)
  110          CONTINUE
  120       CONTINUE
            MOINT(I,J)=SUM
  150    CONTINUE
  160 CONTINUE
C
      IF(DBG) THEN
         WRITE(IW,9020) LABEL
         CALL PRSQ(MOINT,N1,N1,N1)
      END IF
C     RETURN
C
 9000 FORMAT(1X,A4,' AO INTEGRALS')
 9020 FORMAT(1X,A4,' TRANSFORMED INTEGRALS')
      END
C*MODULE SOLIB   *DECK TMOINT2
      SUBROUTINE TMOINT2(MOINT,MOINT4,VST1,VST2,VST1C,VST2C,PART,WORK,
     *                   N2INT,NBUF,BUF,IBUF,NEEDDC,DC,COR2INT,COREPART,
     *                   CORE2,KART,N1,N1C,N2,N2A,L1,M1,LDIMC,NEC,MAXNCO
     *                  ,NFT2SO,IQ,IQ1,DISTINT,DOSOL,MCP2E,MCPREC,DBG)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION MOINT,MOINT4
      LOGICAL GOPARR,DSKWRK,MASWRK,DBG,KL,DISTINT,DOSOL
      CHARACTER*1 XYZ(3)
      PARAMETER(TWO=2.0D+00,THREE=3.0D+00)
      DIMENSION MOINT(N2,*),MOINT4(N1,N1,N1,N1),VST1(L1,N1),VST2(L1,N1),
     *          VST1C(L1,N1C),VST2C(L1,N1C),PART(L1,L1,L1,M1),WORK(N1),
     *          BUF(*),IBUF(*),DC(L1,L1,2),COR2INT(L1,L1,2),COREPART(L1,
     *          N1C),CORE2(N1C,N1C,LDIMC,2),IQ(N1,N1),IQ1(N1,N1)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /MACHIN/ NWDVAR,MAXFM,MAXSM,LIMFM,LIMSM
      COMMON /PCKLAB/ LABSIZ
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      COMMON /TRNMOM/ OPERR,SYMTOL,NOCC,NUMCI,NFZC,NUMVEC,ICI,MXRT,NSTAT
      DATA XYZ/'X','Y','Z'/
C
C     four index transformation for active and core orbitals
C     built upon tmoint. vst1-t * vst1-t * AO ints * vst2 * vst2
C     (t stands for trasposed and * stands for supermatrix product)
C
C     moint is used for numvec=1 and moint4 for numvec=2 due to
C     permutation symmetry presence (numvec=1) and absence(numvec=2).
C     core2 will contain active-core MO integrals
C
C     integral convention - Pople(?) [11|22]
C     moint is stored as doubly upper triangular matrix
C
C     if the transformation is not needed, zero out arrays and quit
C     (the reason TMOINT2 is called is the compulsory calc of dipmom)
C
C     nec has the number of different core densities (usually 1 except
C     for MCQDPT).
C
      IF(.NOT.DOSOL) THEN
        IF(MAXNCO.EQ.2) THEN
          IF(NUMVEC.EQ.1) CALL VCLR(MOINT,1,N2*N2A)
          IF(NUMVEC.NE.1) CALL VCLR(MOINT4,1,N1*N1*N1*N1)
          IF(N2A.NE.N2) CALL VCLR(MOINT(1,N2A+1),1,N2)
        ENDIF
        IF(NEEDDC.NE.0) CALL VCLR(COR2INT,1,L1*L1*NEC)
        DO IK=1,NEC
          CALL VCLR(CORE2(1,1,1,IK),1,N1C*N1C)
        ENDDO
        RETURN
      ENDIF
C
C     otherwise start the toil
C
      LOC=64/4/NWDVAR*LABSIZ
      MASK=2**LOC-1
      IF(NEEDDC.NE.0) CALL VCLR(COR2INT,1,L1*L1*NEC)
C     nlast=n2int
      IF(MAXNCO.EQ.3.AND.NEEDDC.EQ.0) GOTO 1010
      IF(MCP2E.GT.0) THEN
C
C       reading only into NFZC core, since all MCP cores are
C       automatically a part of NFZC (not correlated in MCQDPT).
C
        CALL DAREAD(IDAF,IODA,COR2INT,L1*L1,MCPREC,0)
C        if(me.eq.1) write(6,*) me,' reporting, read :',dskwrk
      ENDIF
      IF(MCP2E.EQ.1) GOTO 1010
C
      DO 1005 IACT=1,N1,M1
        MO1=M1
        IF(IACT+M1.GT.N1) MO1=N1-IACT+1
        IF(MAXNCO.EQ.2.AND.MASWRK) WRITE(IW,9400) IACT,IACT+MO1-1,N1,
     *                                            XYZ(KART)
        IF(MAXNCO.EQ.2) CALL VCLR(PART,1,L1*L1*L1*MO1)
C
        NDONE=0
        CALL SEQREW(NFT2SO)
        READ(NFT2SO) NWDVAR1,LABSIZ1,L21,MAXL1,IONECNT1,ENUCR1
C         five dummies read, dummy summed
        NWDVAR1=NWDVAR1+LABSIZ1+L21+MAXL1+IONECNT1+INT(ENUCR1)
        NSTEP=1+(LABSIZ-1)/NWDVAR+1
C
C       AO integral file loop
C
 1234   CONTINUE
          READ(NFT2SO,END=1002,ERR=1003) NLAST
C         write(iw,*) 'redd',nlast,kart
          IF(NLAST.GT.NBUF) THEN
            WRITE(IW,*) 'INSUFFICIENT BUFFER IN TMOINT2',NLAST,NBUF
            CALL ABRT
          ENDIF
          IF (NLAST .GT. 0)
     *       READ(NFT2SO,END=1002,ERR=1003) (BUF(MMM),MMM=1,NLAST)
C     if(dbg) write(iw,*) 'read 2e AO chunk',(buf(m),m=1,nlast,labsiz+1)
          DO 1000 II=1,NLAST,NSTEP
            VAL=BUF(II)
C
C       currently i,j,k,l are not always in the "canonical" order, i.e.
C       sometimes i>j or k>l. While this is no problem now if later
C       instead of square a triangular MO matrix be constructed, iand j
C       (or k and l) should be transposed - antisymmetric (symmetric)
C
            IF (LABSIZ.EQ.1) THEN
              LAB=IBUF((II+1)*NWDVAR)
              I=IAND(LAB,MASK)
              J=IAND(ISHFT(LAB,-LOC),MASK)
              K=IAND(ISHFT(LAB,-LOC-LOC),MASK)
              L=IAND(ISHFT(LAB,-LOC*3),MASK)
            ELSE IF (LABSIZ.EQ.2) THEN
              LAB1=IBUF((II+1)*NWDVAR-1)
              LAB2=IBUF((II+1)*NWDVAR)
              I=IAND(LAB1,MASK)
              J=IAND(ISHFT(LAB1,-LOC),MASK)
              K=IAND(LAB2,MASK)
              L=IAND(ISHFT(LAB2,-LOC),MASK)
            ELSE
              CALL ABRT
            ENDIF
C
C       ij=i.ne.j
C       for i=j val = 0 by antisymmetry of [ij||kl] = - [ji||kl]
C
            IF(I.EQ.J.AND.ABS(VAL).GT.1.0D-08) THEN
              WRITE(IW,*) 'COULOMB 12',VAL,KART,I,J,K,L
              CALL ABRT
            ENDIF
            KL=K.NE.L
C       ijkl=ij.and.kl
C
            IF(MAXNCO.EQ.2) THEN
C       contract the fourth index (active)
C       [ij|kl], [ji|kl], [ij|lk], [ji|lk]
C
             CALL DAXPY(MO1, VAL,VST2(L,IACT),L1,PART(I,J,K,1),L1*L1*L1)
             CALL DAXPY(MO1,-VAL,VST2(L,IACT),L1,PART(J,I,K,1),L1*L1*L1)
             IF(KL) THEN
             CALL DAXPY(MO1, VAL,VST2(K,IACT),L1,PART(I,J,L,1),L1*L1*L1)
             CALL DAXPY(MO1,-VAL,VST2(K,IACT),L1,PART(J,I,L,1),L1*L1*L1)
             ENDIF
            ENDIF
C
C       contract two core indices
C
            IF(NEEDDC.NE.0.AND.IACT.EQ.1) THEN
            DO IK=1,NEC
C          Coulomb 12
C          if(.not.ij) then
C             use the symmetric properties of DC for Coulombic terms
C             val1=dc(i,j,ik)*val*FOUR
C             if(kl) val1=val1+val1
C             cor2int(k,l)=cor2int(k,l)+val1
C             cor2int(l,k)=cor2int(l,k)+val1
C
C             all contributions are zero because val(i,i,k,l)=0
C          endif
C          Coulomb 21
              VAL2=DC(K,L,IK)*VAL*TWO
              IF(KL) VAL2=VAL2+VAL2
              COR2INT(I,J,IK)=COR2INT(I,J,IK)+VAL2
              COR2INT(J,I,IK)=COR2INT(J,I,IK)-VAL2
C          Exchange 12
              VAL3= -VAL*THREE
              COR2INT(K,J,IK)=COR2INT(K,J,IK)+DC(I,L,IK)*VAL3
              COR2INT(K,I,IK)=COR2INT(K,I,IK)-DC(J,L,IK)*VAL3
              IF(KL) THEN
                COR2INT(L,J,IK)=COR2INT(L,J,IK)+DC(I,K,IK)*VAL3
                COR2INT(L,I,IK)=COR2INT(L,I,IK)-DC(J,K,IK)*VAL3
              ENDIF
              VAL4=VAL3
C          Exchange 21
              COR2INT(I,L,IK)=COR2INT(I,L,IK)+DC(K,J,IK)*VAL4
              COR2INT(J,L,IK)=COR2INT(J,L,IK)-DC(K,I,IK)*VAL4
              IF(KL) THEN
                COR2INT(I,K,IK)=COR2INT(I,K,IK)+DC(L,J,IK)*VAL4
                COR2INT(J,K,IK)=COR2INT(J,K,IK)-DC(L,I,IK)*VAL4
              ENDIF
              ENDDO
            ENDIF
 1000     CONTINUE
          NDONE=NDONE+NLAST
        IF(NDONE.LT.N2INT.OR.N2INT.EQ.0) GOTO 1234
C    i.e. if reading integrals from a separate run when n2int is unknown
 1002   CONTINUE
        IF(MASWRK.AND.N2INT.EQ.0) THEN
          IF(NDONE.NE.0) WRITE(IW,9100) NDONE,XYZ(KART)
          IF(NDONE.EQ.0) WRITE(IW,9200) XYZ(KART)
        ENDIF
        IF(MAXNCO.EQ.2) THEN
C
C     contract the third index (active)
C
        DO 2000 L7=1,MO1
          DO 2000 J=1,L1
            DO 2000 I=1,L1
              CALL MRARBR(PART(I,J,1,L7),L1*L1,1,L1,VST1,L1,N1,WORK,1)
              CALL DCOPY(N1,WORK,1,PART(I,J,1,L7),L1*L1)
 2000 CONTINUE
C
C     contract the second index (active)
C
        DO 3000 L7=1,MO1
          DO 3000 K1=1,N1
            DO 3000 I=1,L1
              CALL MRARBR(PART(I,1,K1,L7),L1,1,L1,VST2,L1,N1,WORK,1)
              CALL DCOPY(N1,WORK,1,PART(I,1,K1,L7),L1)
 3000 CONTINUE
C
C     contract the first index  (active)
C
        DO 4100 L7=1,MO1
          LOL=IACT+L7-1
          IF(NUMVEC.EQ.1) LASTK=LOL
          IF(NUMVEC.NE.1) LASTK=N1
          DO 4100 K1=1,LASTK
            IF(NUMVEC.EQ.1.AND.IQ1(K1,LOL).GT.N2A) GOTO 4100
C           if the run is dragged, skip unwanted integrals
            DO 4000 J1=1,N1
              IF(NUMVEC.EQ.1) LTI=J1
              IF(NUMVEC.NE.1) LTI=N1
              CALL MRARBR(PART(1,J1,K1,L7),1,1,L1,VST1,L1,LTI,WORK,1)
              IF(NUMVEC.EQ.1) THEN
                CALL DCOPY(LTI,WORK,1,MOINT(IQ(1,J1),IQ1(K1,LOL)),1)
C               unfortunately the convention is not followed:
C               upper triangular matrix is stored
              ELSE
                CALL DCOPY(LTI,WORK,1,MOINT4(1,J1,K1,LOL),1)
              ENDIF
 4000 CONTINUE
 4100 CONTINUE
      ENDIF
 1005 CONTINUE
 1010 CONTINUE
C
C     contract the second and first active indices for active/core
C
      DO IK=1,NEC
      CALL MRARBR(COR2INT(1,1,IK),L1,L1,L1,VST2C,L1,N1C,COREPART,L1)
      CALL MRTRBR(VST1C,L1,L1,N1C,COREPART,L1,N1C,CORE2(1,1,1,IK),N1C)
      ENDDO
C
C     sum contributions from all nodes, only 2e are distributed
C
      IF(DISTINT) THEN
         DO IK=1,NEC
           CALL DDI_GSUMF(2305,CORE2(1,1,1,IK),N1C*N1C)
         ENDDO
         IF(MAXNCO.EQ.2) THEN
            IF(NUMVEC.EQ.1) CALL DDI_GSUMF(2306,MOINT,N2*N2A)
            IF(NUMVEC.NE.1) CALL DDI_GSUMF(2307,MOINT4,N1*N1*N1*N1)
         ENDIF
      ENDIF
C     zero out integrals to replace the dragged uncalculated integrals
      IF(N2A.NE.N2) CALL VCLR(MOINT(1,N2A+1),1,N2)
C
      IF(DBG) THEN
         IF(MAXNCO.EQ.2) THEN
            WRITE(IW,9020) XYZ(KART),'(ACTIVE)'
            IF(NUMVEC.EQ.1) CALL PRSQ(MOINT,N2,N2,N2)
            IF(NUMVEC.EQ.2) CALL PRSQ(MOINT4,N1*N1,N1*N1,N1*N1)
         ENDIF
         DO IK=1,NEC
            WRITE(IW,9010) XYZ(KART),'(C-A)'
            CALL PRSQ(COR2INT(1,1,IK),L1,L1,L1)
            WRITE(IW,9020) XYZ(KART),'(CORE)'
            CALL PRSQ(CORE2(1,1,1,IK),N1C,N1C,N1C)
         ENDDO
      END IF
      RETURN
 1003 WRITE(IW,9300) XYZ(KART)
      CALL ABRT
C
 9010 FORMAT(1X,'L(1,2)',A1,' AO INTEGRALS ',A8)
 9020 FORMAT(1X,'L(1,2)',A1,' TRANSFORMED INTEGRALS ',A8)
 9100 FORMAT(1X,'READ ',I10,' L',A1,' INTEGRALS',/)
 9200 FORMAT(/1X,'WARNING: NO L',A1,' INTEGRALS,',
     *           ' INTEGRAL FILE ERRORS?',/)
 9300 FORMAT(/1X,'ERROR READING INTEGRAL FILE L',A1/)
 9400 FORMAT(1X,'PROCESSING',I4,'-',I4,' OUT OF',I4,' MOS, <L',A1,'>')
      END
C*MODULE SOLIB   *DECK ZPUSQL
      SUBROUTINE ZPUSQL(V,M,N,NDIM)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
C
      COMPLEX*16 V(NDIM,M)
C
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C     ----- PUNCH A RECTANGULAR MATRIX WITH ORDERING LABELS -----
C     -V- IS -N- ROWS BY -M- COLUMNS, WITH TRUE LEAD DIMENSION -NDIM-
C
      IF (MASWRK) THEN
      DO 120 J = 1,M
      IC = 0
      MAX = 0
  100 MIN = MAX+1
      MAX = MAX+5
      IC = IC+1
      IF (MAX .GT. N) MAX = N
      MODJ=MOD(J,100)
C     WRITE (IP,9008) MODJ,IC,(V(J,I),I = MIN,MAX)
      WRITE (IP,9008) MODJ,   (V(J,I),I = MIN,MAX)
      IF (MAX .LT. N) GO TO 100
  120 CONTINUE
      ENDIF
      RETURN
C
 9008 FORMAT(I5,1P,5E15.8,/,5X,1P,5E15.8)
C9008 FORMAT(I2,I3,1P,5E15.8,/,5X,1P,5E15.8)
      END
C*MODULE SOLIB   *DECK ZPRSQL
      SUBROUTINE ZPRSQL(NUMCI,NHSO,HSO,EIG,INDX,MULST,IROOTS,IW,LAMB)
C     SUBROUTINE  PRSQL
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      LOGICAL GOPARR,DSKWRK,MASWRK
      COMPLEX*16 HSO(NHSO,NHSO)
      DIMENSION EIG(NHSO),INDX(NHSO),MULST(*),IROOTS(*)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
      PARAMETER  (ONE=1.0D+00, TWO=2.0D+00)
C
C     ----- PRINT OUT A SQUARE MATRIX WITH BASIS SET LABELS -----
C     -V- IS -N- ROWS BY -M- COLUMNS, WITH LEADING DIMENSION -NDIM-
C
C     lamb=0 <Hso>,   lamb=1 <Jz>
C
      IF(.NOT.MASWRK) RETURN
      MAX = 5
      IMAX = 0
  100 IMIN = IMAX+1
      IMAX = IMAX+MAX
      IF (IMAX .GT. NHSO) IMAX = NHSO
      WRITE (IW,9008)
      WRITE (IW,9028) (     I ,I = IMIN,IMAX)
      IF(LAMB.EQ.0) WRITE (IW,9030) ( EIG(I),I = IMIN,IMAX)
      IF(LAMB.EQ.1) WRITE (IW,9032) ( EIG(I),I = IMIN,IMAX)
      IF(LAMB.EQ.0) WRITE (IW,9038) (INDX(I),I = IMIN,IMAX)
      WRITE (IW,9008)
      J = 0
      DO 120 ICI=1,NUMCI
        DO 120 IRT=1,IROOTS(ICI)
          HIGHM = (MULST(ICI)-1)/TWO + ONE
          DO 120 IM=1,MULST(ICI)
            J = J +1
            SPIN = HIGHM - IM
            IF(LAMB.EQ.0) WRITE (IW,9048) J,IRT,MULST(ICI),SPIN,
#if defined(SPEC_CPU)
     *                     (DBLE(HSO(J,I)),I = IMIN,IMAX)
#else
     *                     (DREAL(HSO(J,I)),I = IMIN,IMAX)
#endif
            IF(LAMB.EQ.1) WRITE (IW,9049) J,INDX(J),
#if defined(SPEC_CPU)
     *                     (DBLE(HSO(J,I)),I = IMIN,IMAX)
#else
     *                     (DREAL(HSO(J,I)),I = IMIN,IMAX)
#endif
            WRITE(IW,9058) (DIMAG(HSO(J,I)),I = IMIN,IMAX)
  120 CONTINUE
      IF (IMAX .LT. NHSO) GO TO 100
      RETURN
 9008 FORMAT(1X)
 9028 FORMAT(24X,5(I3,8X))
 9030 FORMAT(1X,'  STATE ENERGY --- ',5F11.2)
 9032 FORMAT(1X,'  STATE   JZ   --- ',5F11.4)
 9038 FORMAT(1X,'      STATE ID --- ',5(5X,I5,1X))
 9048 FORMAT(1X,I4,'<',I2,'>',I3,'(',F5.1,') R',5F11.6)
 9049 FORMAT(1X,I4,' <',I5,'>',6X,' R',5F11.6)
 9058 FORMAT(19X,'I',5F11.6)
      END
C*MODULE SOLIB   *DECK SOMP2OUT
      SUBROUTINE SOMP2OUT(MHSO,NHSO,EIG,OMEGA,EZERO,WOUT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /IOFILE/ IR,IW,IP,IJKO,IJKT,IDAF,NAV,IODA(400)
      LOGICAL WOUT
      CHARACTER LET(2)
      DIMENSION EIG(NHSO,2),OMEGA(NHSO,2),EZERO(2),IOUT(2)
      PARAMETER (XKIZER=219478.36857D+00,ZERO=0.0D+00)
C
C     output CAS and SO-MCQDPT energy levels side by side
C
      IF(MHSO.EQ.2) THEN
         WRITE(IW,9000)
         IF(WOUT) WRITE(IW,9020)
         WRITE(IP,9000)
      ENDIF
      IF(MHSO.EQ.1) THEN
         WRITE(IW,9005)
         IF(WOUT) WRITE(IW,9025)
         WRITE(IP,9005)
      ENDIF
      WRITE(IP,*) '$SOLEV'
      IOUT(1)=IW
      IOUT(2)=IP
      DO I=1,NHSO
         DO J=1,2
         JOUT=IOUT(J)
         LET(1)=' '
         LET(2)=' '
         IF(MHSO.EQ.2) THEN
          IF(WOUT) THEN
          IF(OMEGA(I,1).LT.ZERO) LET(1)='?'
          IF(OMEGA(I,2).LT.ZERO) LET(2)='?'
          WRITE(JOUT,9010) I,ABS(OMEGA(I,1)),LET(1),EIG(I,1)-EIG(1,1),
     *                       ABS(OMEGA(I,2)),LET(2),EIG(I,2)-EIG(1,2),
     *                 EIG(I,1)/XKIZER+EZERO(1),EIG(I,2)/XKIZER+EZERO(2)
          ELSE
          WRITE(JOUT,9011) I,EIG(I,1)-EIG(1,1),EIG(I,2)-EIG(1,2),
     *                 EIG(I,1)/XKIZER+EZERO(1),EIG(I,2)/XKIZER+EZERO(2)
          ENDIF
         ELSE
          IF(WOUT) THEN
          IF(OMEGA(I,1).LT.ZERO) LET(1)='?'
          WRITE(JOUT,9015) I,ABS(OMEGA(I,1)),LET(1),EIG(I,1)-EIG(1,1),
     *                      EIG(I,1)/XKIZER+EZERO(1)
          ELSE
          WRITE(JOUT,9016) I,EIG(I,1)-EIG(1,1),EIG(I,1)/XKIZER+EZERO(1)
          ENDIF
         ENDIF
         ENDDO
      ENDDO
      WRITE(IP,*) '$END'
      RETURN
 9000 FORMAT(/11X,'CAS     AND  MCQDPT LEVELS IN CM-1 ; ',
     *       'CAS AND MCQDPT LEVELS IN A.U.',/)
 9005 FORMAT(/1X,'CI ENERGY LEVELS IN CM-1 AND ',
     *       'CI ABSOLUTE LEVELS IN A.U.',/)
 9010 FORMAT(1X,I4,2(F5.1,A1,F12.4),2F18.10)
 9011 FORMAT(1X,I4,2(5X,  1X,F12.4),2F18.10)
 9015 FORMAT(1X,I4,F5.1,A1,F12.4,F18.10)
 9016 FORMAT(1X,I4,5X,  1X,F12.4,F18.10)
 9020 FORMAT(4X,'N',1X,'OMEGA',2X,'&',2X,'E(CAS)',1X,'OMEGA',' & ',
     *          'E(MCQDPT)',7X,'E(CAS)',11X,'E(MCQDPT)')
 9025 FORMAT(4X,'N',1X,'OMEGA',6X,'E(CI)',9X,'E(CI)')
      END
C*MODULE SOLIB   *DECK ZTFSQU
      SUBROUTINE ZTFSQU(H,T,WRK,N)
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
C
      DIMENSION H(N,N),T(N,N),WRK(N)
      PARAMETER (ONE=(1.0D+00,0.0D+00),ZERO=(0.0D+00,0.0D+00))
C
C     ----- TRANSFORM THE SQUARE MATRIX H in place USING VECTORS T -----
C                      H = T-DAGGER * H * T
C     THE ORDER OF THE SQUARE MATRICES H, F, AND T IS N.
C     all matrices are complex (dagger is a real dagger!).
C
C     T-dagger * H
C
      DO I=1,N
        CALL ZGEMV('C',N,N,ONE,T,N,H(1,I),1,ZERO,WRK,1)
        CALL ZCOPY(N,WRK,1,H(1,I),1)
      ENDDO
C
C     (T-dagger * H) * T
C     (Hi * T is done as (T-trans * Hi-trans)-trans,
C     where Hi is the i-th row of H
C
      DO I=1,N
        CALL ZGEMV('T',N,N,ONE,T,N,H(I,1),N,ZERO,WRK,1)
        CALL ZCOPY(N,WRK,1,H(I,1),N)
      ENDDO
C
      RETURN
      END
