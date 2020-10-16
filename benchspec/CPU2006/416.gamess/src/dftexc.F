C 24 JAN 02 - MWS - FIX CHAR VARIABLE USAGE, CHANGE CORR.ONLY KEYWORDS
C                   TO VWN/LYP/OP, FIX LYP FUNCTIONAL FOR NB=0 CASE
C 25 JUN 01 - MWS - ALTER COMMON BLOCK WFNOPT
C 13 JUN 01 - TT,SY,MK,DGF IMPLEMENT GRID-BASED DFT
C
C*MODULE DFTEXC  *DECK B88XF
      SUBROUTINE B88XF(RHFGVB,UROHF,ROA,ROB,GRDAA,GRDBB,
     >                   GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
     >                   FTOTWT,SIGKA,SIGKB,DKDXA,DKDXB,
     >                   XALF,B88X,DUMA,DUMXA,DUMYA,DUMZA,
     >                   DUMB,DUMXB,DUMYB,DUMZB)
C***********************************************************************
C
C     BECKE 88 EXCHANGE FUNCTIONAL
C
C     A. D. BECKE
C     PHYS. REV. A 38,3098 (1988)
C
C     -----
C     INPUT
C     -----
C
C     ROA,B   .... SPINDENSITIES
C     GRAD**  .... DENSITY GRADIENTS
C     TOTWT   .... VOLUME OF THIS GRID
C
C     ------
C     OUTPUT
C     ------
C
C     XALF    ....  EXCHANGE ENERGY OF LDA PART
C     B88X    ....  EXCHANGE ENERGY OF GGA PART
C     DUM***  ....  FIRST DERIVATIVES OF THE EXCHANGE FUNCTINALS
C     SIGKA,B ....  EXC=-1/2*INT(SIGK*RHO^(4/3))
C     DKDXA,B ....  DSIGK/DX
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL UROHF, RHFGVB
      PARAMETER (ZERO=0.0D+00,ONE=1.0D+00, TWO=2.0D+00, THREE=3.0D+00)
      COMMON /DFTEXC/ PI,CSLT,CB88,CLYP,CVWN,QOP,NEXFG,NCORFG,NPFFG
C***********************************************************************
       X1 = (3.0D+00/PI)**(ONE/3.0D+00)
       BECKEB=0.0042D+00
       FACE  =-1.5D+00*X1*(ONE/TWO )**(TWO/THREE)
       FACP  = 4.0D+00/3.0D+00
C***********************************************************************
C      CLOSED SHELL CALCULATION
C***********************************************************************
       IF (RHFGVB) THEN
         RHO=ROA
         GRD=GRDAA
         RHO13=RHO**(ONE/3.0D+00)
         RHOE =RHO13**4
         X    =SQRT(GRD)/RHOE
         Q    =SQRT(ONE+X*X)
         S    =LOG(X+Q)
         T    =ONE+6.0D+00*BECKEB*X*S
         G    =-BECKEB*X*X/T
         H    =(-TWO*BECKEB*X+6.0D+00*(BECKEB*X)**2*(-S+X/Q))/T**2
         R    =FACP*RHO13
         E    =FACE
         XCL  =E * RHOE
         XCNL =G * RHOE
         XALF =XALF +CB88*TWO*XCL *FTOTWT
         B88X =B88X +CB88*TWO*XCNL*FTOTWT
         U    =CB88*R*(E+G-X*H)
         DUMA =DUMA+U
         W    =CB88*0.5D+00*H/SQRT(GRD)
         DUMXA=DUMXA+TWO*W*GRADXA
         DUMYA=DUMYA+TWO*W*GRADYA
         DUMZA=DUMZA+TWO*W*GRADZA
C     ----- FOR OP CORRELATION -----
         SIGKA= -(FACE+G)*TWO
         DKDXA= -H*TWO
C     -----------------------------
C***********************************************************************
C      OPEN SHELL CALCULATION
C***********************************************************************
       ELSE IF (UROHF) THEN
C     ----- ALPHA CONTRIBTUTION ----
         RHO13A=ROA**(ONE/3.0D+00)
         RHOEA =RHO13A**4
         XA    =SQRT(GRDAA)/RHOEA
         QA    =SQRT(ONE+XA**2)
         SA    =LOG(XA+QA)
         TA    =ONE+6.0D+00*BECKEB*XA*SA
         GA    =-BECKEB*XA*XA/TA
         HA    =(-TWO*BECKEB*XA
     >            +6.0D+00*(BECKEB*XA)**2*(-SA+XA/QA))/TA**2
         RA    =FACP*RHO13A
         EA    =FACE
         UA    =CB88*RA*(EA+GA-XA*HA)
         DUMA  =DUMA+UA
         WA    =CB88*0.5D+00*HA/SQRT(GRDAA)
         DUMXA =DUMXA+TWO*WA*GRADXA
         DUMYA =DUMYA+TWO*WA*GRADYA
         DUMZA =DUMZA+TWO*WA*GRADZA
C     ----- BETA CONTRIBTUTION ----
         RHO13B=ROB**(ONE/3.0D+00)
         RHOEB =RHO13B**4
         EB    =FACE
         IF(ROB.LT.1.0D-15) THEN
           GB  = ZERO
           HB  = ZERO
           GOTO 9999
         ENDIF
         XB    =SQRT(GRDBB)/RHOEB
         QB    =SQRT(ONE+XB**2)
         SB    =LOG(XB+QB)
         TB    =ONE+6.0D+00*BECKEB*XB*SB
         GB    =-BECKEB*XB*XB/TB
         HB    =(   -TWO* BECKEB*XB
     >              +6.0D+00*(BECKEB*XB  )**2*(-SB+XB/QB))/TB**2
         RB    =FACP*RHO13B
         UB    =CB88*RB*(EB+GB-XB*HB)
         DUMB  =DUMB+UB
         WB    =CB88*0.5D+00*HB/SQRT(GRDBB)
         DUMXB =DUMXB+TWO*WB*GRADXB
         DUMYB =DUMYB+TWO*WB*GRADYB
         DUMZB =DUMZB+TWO*WB*GRADZB
 9999    CONTINUE
         XCL   =EA*RHOEA+EB*RHOEB
         XCNL  =GA*RHOEA+GB*RHOEB
         XALF  =XALF  +CB88*XCL *FTOTWT
         B88X  =B88X  +CB88*XCNL*FTOTWT
C      ----- FOR OP CORRELATION -----
         SIGKA= -(FACE+GA)*TWO
         SIGKB= -(FACE+GB)*TWO
         DKDXA= -HA*TWO
         DKDXB= -HB*TWO
C     -------------------------------
       ENDIF
      RETURN
      END
C*MODULE DFTEXC  *DECK CALCEXC
      SUBROUTINE CALCEXC(ROA,ROB,FTOTWT,GRDAA,GRDBB,GRDAB,
     >                  GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
     >                  XALPHA,XGRD,VXCA1,DUMAX,DUMAY,DUMAZ,
     >                  VXCB1,DUMBX,DUMBY,DUMBZ,ECF)
C
C***********************************************************************
C       DRIVER FOR THE  DFT EXCHANGE
C      CORRELATION ENERGY CALCULATION
C***********************************************************************
C
C   * NEXFG CONTROLS EXCHANGE FUNCTIONAL
C     -1  ---  BECKE 3 PARAMETER
C      0  ---  HARTREE-FOCK
C      1  ---  SLATER
C      2  ---  BECKE 88
C      3  ---  PERDEW-WANG 91
C      4  ---  PERDEW-BURKE-ERNZERHOF (PBE)
C      5  ---  GILL 96
C      6  ---  PARAMETER-FREE
C      7 ... PLEASE ADD NEW FUNCTIONAL
C
C   * NCORFG CONTROLS CORRELATION FUNCTIONAL
C     -1  ---  B3LYP
C      0  ---  NONE (EXCHANGE ONLY)
C      1  ---  VWN FORMULA 5
C      2  ---  LEE-YANG-PARR
C      3  ---  PERDEW-WANG 91
C      4  ---  ONE-PARAMETER-PROGRESSIVE (OP)
C      5... PLEASE ADD NEW FUNCTIONAL
C
C   * OTHER ARIABLES....
C
C     ROA,ROB                    <---     DENSITY
C     GRADXA,GRADXB,ETC          <---     GRAD(RHO)
C     GRDAA,GRDBB,GRDAB          <---     GRAD(RHO) * GRAD(RHO)
C     VXCA1,VXCB1                <---     DEXC/DRHO
C     DUMAX,DUMAY,DUMAZ,ETC      <---     DEXC/DG
C     XALPHA                     <---     EX FROM LDA TERM
C     XGRD                       <---     EX FROM GRADIENT TERM
C     ECF                        <---     CORRELATION ENERGY
C
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MXATM=500)
C
      COMMON /DFTEXC/ PI,CSLT,CB88,CLYP,CVWN,QOP,NEXFG,NCORFG,NPFFG
      COMMON /INFOA / NAT,ICH,MUL,NUM,NQMT,NE,NA,NB,
     *                ZAN(MXATM),C(3,MXATM)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /WFNOPT/ SCFTYP,CITYP,DFTYPE,CCTYP,MPLEVL,MPCTYP
C
      LOGICAL UROHF, RHFGVB
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA RHF,UHF,ROHF,GVB/8HRHF     ,8HUHF     ,8HROHF     ,
     *     8HGVB     /
#else
      CHARACTER*8 :: RHF_STR, UHF_STR, ROHF_STR, GVB_STR
      EQUIVALENCE (RHF, RHF_STR), (UHF, UHF_STR), (GVB, GVB_STR),
     * (ROHF, ROHF_STR)
      DATA RHF_STR,UHF_STR,ROHF_STR,GVB_STR/"RHF     ","UHF     ",
     * "ROHF    " ,"GVB     "/
#endif
C***********************************************************************
      UROHF = SCFTYP.EQ.UHF  .OR.  SCFTYP.EQ.ROHF
      RHFGVB = SCFTYP.EQ.RHF  .OR.  SCFTYP.EQ.GVB
C
C***********************************************************************
C           EXCHANGE FUNCTIONAL CALCULATION
C***********************************************************************
      DKDXA=0.0D+00
      DKDXB=0.0D+00
      SIGKA=0.0D+00
      SIGKB=0.0D+00
      DUMMY=0.0D+00
C
C     -----HARTREE-FOCK EXCHANGE-----
C
      IF(NEXFG.EQ.0) THEN
         IF (NCORFG.EQ.4) THEN
C     ----- HOP METHOD -----
            CALL B88XF(RHFGVB,UROHF,ROA,ROB,GRDAA,GRDBB,
     >           GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
     >           FTOTWT,SIGKA,SIGKB,DKDXA,DKDXB,
     >           DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,
     >           DUMMY,DUMMY,DUMMY,DUMMY)
C
         ENDIF
C
C     ----- SLATER EXCHANGE -----
C
      ELSE IF(NEXFG.EQ.1) THEN
         CALL LSDSLT(RHFGVB,UROHF,ROA,ROB,FTOTWT,VXCA1,VXCB1,XALPHA,
     *               SIGKA,SIGKB)
C
C     ----- BECKE 88 EXCHANGE -----
C
      ELSE IF(NEXFG.EQ.2) THEN
C
         CALL B88XF(RHFGVB,UROHF,ROA,ROB,GRDAA,GRDBB,
     >        GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
     >        FTOTWT,SIGKA,SIGKB,DKDXA,DKDXB,
     >        XALPHA,XGRD,VXCA1,DUMAX,DUMAY,DUMAZ,
     >        VXCB1,DUMBX,DUMBY,DUMBZ)
C
C     ----- PW91 EXCHANGE -----
C
C     ELSE IF(NEXFG.EQ.3) THEN
C     CALL PW91XF(ROA,ROB,GRDAA,GRDBB,GRDAB,
C     >           GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
C     >           TOTWT,NFCT,NPTDUM,FACT,NCNTR,IPT,
C     >           XALPHA,XGRD,VXCA1,DUMAX,DUMAY,DUMAZ,
C     >           VXCB1,DUMBX,DUMBY,DUMBZ)
C
C     ----- PBE EXCHANGE -----
C
C     CAUTION: THIS FUNCTIONAL MAY NOT BE CORRECTLY PROGRAMMED.
C     MIKE SCHMIDT FOUND IN JUNE 2001 THAT ETHANOL 6-31G(D) HAS
C     AN ENERGY OF -153.9850674001 WITH GAMESS,
C              BUT -153.9851024042 WITH NWCHEM.  SINCE THE OTHER
C     FUNCTIONALS AGREE TO 1 OR 2 IN THE 7TH DIGIT, THIS SEEMS
C     TO BE A FAIRLY LARGE DISCREPANCY.  NWCHEM RUN WITH "XPBE".
C
C         PBE EXCHANGE/CORRELATION FUNCTIONAL:
C           J.P.PERDEW, K.BURKE, M.ERNZERHOF
C              PHYS.REV.LETT.  77, 3865-8(1996); ERR. 78,1396(1997)
C           M.ERNZERHOF, G.E.SCUSERIA
C              J.CHEM.PHYS. 110, 5029-5036(1999)
C         NOTE THAT ONLY THE EXCHANGE FUNCTIONAL IS IN GAMESS.
C
      ELSE IF(NEXFG.EQ.4) THEN
         CALL PBEXF(RHFGVB,UROHF,ROA,ROB,GRDAA,GRDBB,
     >        GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
     >        FTOTWT,SIGKA,SIGKB,DKDXA,DKDXB,
     >        XALPHA,XGRD,VXCA1,DUMAX,DUMAY,DUMAZ,
     >        VXCB1,DUMBX,DUMBY,DUMBZ)
C
C     ----- GILL 96 EXCHANGE -----
C
      ELSE IF(NEXFG.EQ.5) THEN
         CALL G96XF(RHFGVB,UROHF,ROA,ROB,GRDAA,GRDBB,
     >        GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
     >        FTOTWT,SIGKA,SIGKB,DKDXA,DKDXB,
     >        XALPHA,XGRD,VXCA1,DUMAX,DUMAY,DUMAZ,
     >        VXCB1,DUMBX,DUMBY,DUMBZ)
C
C     ----- PARAMETER FREE EXCHANGE -----
C
      ELSE IF(NEXFG.EQ.6) THEN
         CALL PFREE(RHFGVB,UROHF,ROA,ROB,GRDAA,GRDBB,
     >        GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
     >        FTOTWT,SIGKA,SIGKB,DKDXA,DKDXB,
     >        XALPHA,XGRD,VXCA1,DUMAX,DUMAY,DUMAZ,
     >        VXCB1,DUMBX,DUMBY,DUMBZ)
C
C     ----- B3 EXCHANGE -----
C
      ELSE IF(NEXFG.EQ.-1) THEN
         CALL LSDSLT(RHFGVB,UROHF,ROA,ROB,FTOTWT,VXCA1,VXCB1,XALPHA,
     *               SIGKA,SIGKB)
         CALL B88XF(RHFGVB,UROHF,ROA,ROB,GRDAA,GRDBB,
     >        GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
     >        FTOTWT,SIGKA,SIGKB,DKDXA,DKDXB,
     >        XALPHA,XGRD,VXCA1,DUMAX,DUMAY,DUMAZ,
     >        VXCB1,DUMBX,DUMBY,DUMBZ)
      ELSE
         WRITE(IW,*) 'CANNOT USE THIS EXCHANGE FUNCTIONAL'
         CALL ABRT
         STOP
      ENDIF
C***********************************************************************
C           CORRELATION FUNCTIONAL CALCULATION
C***********************************************************************
C
C     ----- NO CORRELATION FUNCTIONAL-----
C
      IF (NCORFG.EQ.0) THEN
C
C     ----- VWN CORRELATION FUNCTIONAL -----
C
      ELSE IF(NCORFG.EQ.1) THEN
         CALL VWN5CF(RHFGVB,UROHF,ROA,ROB,FTOTWT,VXCA1,VXCB1,ECF)
C
C     ----- LEE-YANG-PARR CORRELATION FUNCTIONAL -----
C
      ELSE IF(NCORFG.EQ.2) THEN
         CALL LYPCF(RHFGVB,UROHF,NB,ROA,ROB,GRDAA,GRDBB,GRDAB,
     >        GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
     >        FTOTWT,ECF,
     >        VXCA1,DUMAX,DUMAY,DUMAZ,VXCB1,DUMBX,DUMBY,DUMBZ)
C
C     ----- PERDEW-WANG 91 CORRELATION FUNCTIONAL-----
C
C      ELSE IF(NCORFG.EQ.3) THEN
C         CALL PW91CF(ROA,ROB,GRDAA,GRDBB,GRDAB,
C     >        GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
C     >        TOTWT,NFCT,NPTDUM,FACT,ICNTR,IPT,ECF,
C     >        VXCA1,DUMAX,DUMAY,DUMAZ,VXCB1,DUMBX,DUMBY,DUMBZ)
C
C     ----- B3LYP CORRELATION PART -----
C
      ELSE IF(NCORFG.EQ.-1) THEN
         CALL VWN5CF(RHFGVB,UROHF,ROA,ROB,FTOTWT,VXCA1,VXCB1,ECF)
         CALL LYPCF(RHFGVB,UROHF,NB,ROA,ROB,GRDAA,GRDBB,GRDAB,
     >        GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
     >        FTOTWT,ECF,
     >        VXCA1,DUMAX,DUMAY,DUMAZ,VXCB1,DUMBX,DUMBY,DUMBZ)
C
C     ----- ONE-PARAMETER PROGRESSIVE CORRELATION FUNCTIONAL -----
C
      ELSE IF(NCORFG.EQ.4) THEN
         CALL OPCOR(RHFGVB,UROHF,ROA,ROB,GRDAA,GRDBB,
     >        GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
     >        FTOTWT,ECF,
     >        SIGKA,SIGKB,DKDXA,DKDXB,
     >        VXCA1,DUMAX,DUMAY,DUMAZ,VXCB1,DUMBX,DUMBY,DUMBZ)
      ELSE
         WRITE(IW,*) 'CANNOT USE THIS CORRELATIONAL FUNCTIONAL'
         CALL ABRT
         STOP
      ENDIF
      RETURN
      END
C*MODULE DFTEXC  *DECK G96XF
      SUBROUTINE G96XF(RHFGVB,UROHF,ROA,ROB,GRDAA,GRDBB,
     >                 GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
     >                 FTOTWT,SIGKA,SIGKB,DKDXA,DKDXB,
     >                 XALF,B88X,DUMA,DUMXA,DUMYA,DUMZA,
     >                 DUMB,DUMXB,DUMYB,DUMZB)
C***********************************************************************
C
C     GILL 96 EXCHANGE FUNCTIONAL
C
C     P. M. W. GILL
C     MOL. PHYS. 89, 433 (1996)
C
C     -----
C     INPUT
C     -----
C
C     ROA,B   .... SPIN DENSITIES
C     GRAD**  .... DENSITY GRADIENTS
C     TOTWT   .... VOLUME OF THIS GRID
C
C     ------
C     OUTPUT
C     ------
C
C     XALF    ....  EXCHANGE ENERGY OF LDA PART
C     B88X    ....  EXCHANGE ENERGY OF GGA PART
C     DUM***  ....  FIRST DERIVATIVES OF THE EXCHANGE FUNCTINALS
C     SIGKA,B ....  EXC=-1/2*INT(SIGK*RHO^(4/3))
C     DKDXA,B ....  DSIGK/DX
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL UROHF, RHFGVB
      PARAMETER (ZERO=0.0D+00,ONE=1.0D+00, TWO=2.0D+00, THREE=3.0D+00)
      COMMON /DFTEXC/ PI,CSLT,CB88,CLYP,CVWN,QOP,NEXFG,NCORFG,NPFFG
C***********************************************************************
       X1 = (3.0D+00/PI)**(ONE/3.0D+00)
       TWO13 = TWO**(ONE/3.0D+00)
       FACE  =-0.75D+00*X1*TWO13
       FACP  = 4.0D+00/3.0D+00
C***********************************************************************
C      CLOSED SHELL CALCULATION
C***********************************************************************
       IF (RHFGVB) THEN
         RHO=ROA
         GRD=GRDAA
         RHO13=RHO**(ONE/3.0D+00)
         RHOE =RHO13*RHO
         X    =SQRT(GRD)/RHOE
         G    =-X**(THREE/TWO)/137.0D+00
         H    =-(1.5D+00/137.0D+00)*SQRT(X)
         R    =FACP*RHO13
         E    =FACE
         XCL  =E*RHOE
         XCNL =G*RHOE
         XALF =XALF +CSLT*XCL *TWO*FTOTWT
         B88X =B88X +CB88*XCNL*TWO*FTOTWT
         U    =CSLT*R*(E+G-X*H)
         DUMA =DUMA+U
         W    =CB88*0.5D+00*H/SQRT(GRD)
         DUMXA=DUMXA+TWO*W*GRADXA
         DUMYA=DUMYA+TWO*W*GRADYA
         DUMZA=DUMZA+TWO*W*GRADZA
C     ----- FOR OP CORRELATION -----
         SIGKA= -(FACE+G)*TWO
         DKDXA= -H*TWO
C***********************************************************************
C      OPEN SHELL CALCULATION
C***********************************************************************
       ELSE IF (UROHF) THEN
         RHO13A=ROA**(ONE/3.0D+00)
         RHOEA =RHO13A*ROA
         EA    =FACE
         IF(ROA.LT.1.0D-10) THEN
           GA  = ZERO
           GOTO 9998
         ENDIF
         XA    =SQRT(GRDAA)/RHOEA
         GA    =-XA**(THREE/TWO)/137.0D+00
         HA    =-(1.5D+00/137.0D+00)*SQRT(XA)
         RA    =FACP*RHO13A
         UA    =CSLT*RA*(EA+GA-XA*HA)
         DUMA  =DUMA+UA
         WA    =CB88*0.5D+00*HA/SQRT(GRDAA)
         DUMXA =DUMXA+TWO*WA*GRADXA
         DUMYA =DUMYA+TWO*WA*GRADYA
         DUMZA =DUMZA+TWO*WA*GRADZA
 9998    CONTINUE
         RHO13B=ROB**(ONE/3.0D+00)
         RHOEB =RHO13B*ROB
         EB    =FACE
         IF(ROB.LT.1.0D-10) THEN
           GB  = ZERO
           GOTO 9999
         ENDIF
         XB    =SQRT(GRDBB)/RHOEB
         GB    =-XB**(THREE/TWO)/137.0D+00
         HB    =-(1.5D+00/137.0D+00)*SQRT(XB)
         RB    =FACP*RHO13B
         UB    =CSLT*RB*(EB+GB-XB*HB)
         DUMB  =DUMB+UB
         WB    =CB88*0.5D+00*HB/SQRT(GRDBB)
         DUMXB =DUMXB+TWO*WB*GRADXB
         DUMYB =DUMYB+TWO*WB*GRADYB
         DUMZB =DUMZB+TWO*WB*GRADZB
 9999    CONTINUE
         XCL   =EA*RHOEA+EB*RHOEB
         XCNL  =GA*RHOEA+GB*RHOEB
         XALF  =XALF  +CSLT*XCL *FTOTWT
         B88X  =B88X  +CB88*XCNL*FTOTWT
C      ----- FOR OP CORRELATION -----
         SIGKA= -(FACE+GA)*TWO
         SIGKB= -(FACE+GB)*TWO
         DKDXA= -HA*TWO
         DKDXB= -HB*TWO
       ENDIF
C***********************************************************************
      RETURN
      END
C*MODULE DFTEXC     *DECK INPGDFT
      SUBROUTINE INPGDFT(DFTYPE,PFTYP)
C***********************************************************************
C
C     NEXFG CONTROLS EXCHANGE FUNCTIONAL
C    -1  ---  BECKE 3 PARAMETER
C     0  ---  HARTREE-FOCK
C     1  ---  SLATER
C     2  ---  BECKE 88
C     3  ---  PERDEW-WANG 91
C     4  ---  PERDEW-BURKE-ERNZERHOF (PBE)
C     5  ---  GILL 96
C     6  ---  PARAMETER-FREE
C     7... PLEASE ADD NEW FUNCTIONAL
C
C     NCORFG CONTROLS CORRELATION FUNCTIONAL
C    -1  ---  B3LYP
C     0  ---  NONE (EXCHANGE ONLY)
C     1  ---  VWN FORMULA 5
C     2  ---  LEE-YANG-PARR
C     3  ---  PERDEW-WANG 91
C     4  ---  ONE-PARAMETER-PROGRESSIVE (OP)
C     5... PLEASE ADD NEW FUNCTIONAL
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
C      /DFTHF/,/DFTCF/ ARE INFORMATION FOR GRID CODE
C      /DFTPAR/ IS INFORMATION FOR GRID-FREE CODE
C
      SAVE EXCHR,CORCHR
C
      COMMON /DFGRID/ DFTTHR,SW0,NDFTFG,NRAD,NTHE,NPHI,NRAD0,NTHE0,NPHI0
      COMMON /DFTEXC/ PI,CSLT,CB88,CLYP,CVWN,QOP,NEXFG,NCORFG,NPFFG
      COMMON /DFTPAR/ DFTTYP(20),EXENA,EXENB,EXENC,IDFT34,NAUXFUN,
     *                                                    NAUXSHL
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
C
C     NUMBER OF EXCHANGE AND CORRELATION FUNCTIONALS
C
      PARAMETER (NEX=6,NCOR=4)
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00)
      CHARACTER*8  EX(NEX),COR(NCOR)
      CHARACTER*10 EXNM(NEX),CORNM(NCOR),EXCHR,CORCHR,XCHNG,CRRN
      LOGICAL GOPARR,DSKWRK,MASWRK
      DOUBLE PRECISION LOCAL,LSDA
      DIMENSION NUMEX(NEX),NUMCOR(NCOR),DQOP(NEX)
C
      DATA EX/'S','B','PW91','PBE','G','P'/
      DATA NUMEX/1,2,3,4,5,6/
      DATA EXNM/'SLATER','BECKE88','PW91','PBE','GILL96','PFREE'/
      DATA DQOP/2.5654D+00,2.3670D+00,ZERO,2.3789D+00,2.4167D+00,
     *          2.3866D+00/
C
      DATA COR/'VWN','LYP','PW91','OP'/
      DATA NUMCOR/1,2,3,4/
      DATA CORNM/'VWN5','LYP88','PW91','OP'/
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA LOCAL  /8HLOCAL   /,SLATER /8HSLATER  /,BECKE  /8HBECKE   /
#else
      CHARACTER*8 :: LOCAL_STR
      EQUIVALENCE (LOCAL, LOCAL_STR)
      CHARACTER*8 :: SLATER_STR
      EQUIVALENCE (SLATER, SLATER_STR)
      CHARACTER*8 :: BECKE_STR
      EQUIVALENCE (BECKE, BECKE_STR)
      DATA LOCAL_STR/"LOCAL   "/,SLATER_STR/"SLATER  "/,
     * BECKE_STR/"BECKE   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA GILL   /8HGILL    /,G96    /8HG96     /,PBE    /8HPBE     /
#else
      CHARACTER*8 :: GILL_STR
      EQUIVALENCE (GILL, GILL_STR)
      CHARACTER*8 :: G96_STR
      EQUIVALENCE (G96, G96_STR)
      CHARACTER*8 :: PBE_STR
      EQUIVALENCE (PBE, PBE_STR)
      DATA GILL_STR/"GILL    "/,G96_STR/"G96     "/,PBE_STR/"PBE     "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA PW91   /8HPW91    /
#else
      CHARACTER*8 :: PW91_STR
      EQUIVALENCE (PW91, PW91_STR)
      DATA PW91_STR/"PW91    "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA HOP    /8HOP      /,HLYP   /8HLYP     /,HVWN   /8HVWN     /
#else
      CHARACTER*8 :: HOP_STR
      EQUIVALENCE (HOP, HOP_STR)
      CHARACTER*8 :: HLYP_STR
      EQUIVALENCE (HLYP, HLYP_STR)
      CHARACTER*8 :: HVWN_STR
      EQUIVALENCE (HVWN, HVWN_STR)
      DATA HOP_STR/"OP      "/,HLYP_STR/"LYP     "/,HVWN_STR/"VWN     "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA LSDA   /8HLSDA    /,PFREE  /8HPFREE   /
#else
      CHARACTER*8 :: LSDA_STR
      EQUIVALENCE (LSDA, LSDA_STR)
      CHARACTER*8 :: PFREE_STR
      EQUIVALENCE (PFREE, PFREE_STR)
      DATA LSDA_STR/"LSDA    "/,PFREE_STR/"PFREE   "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA B3LYP  /8HB3LYP   /,BHHLYP /8HBHHLYP  /,HALF   /8HHALF    /
#else
      CHARACTER*8 :: B3LYP_STR
      EQUIVALENCE (B3LYP, B3LYP_STR)
      CHARACTER*8 :: BHHLYP_STR
      EQUIVALENCE (BHHLYP, BHHLYP_STR)
      CHARACTER*8 :: HALF_STR
      EQUIVALENCE (HALF, HALF_STR)
      DATA B3LYP_STR/"B3LYP   "/,BHHLYP_STR/"BHHLYP  "/,
     * HALF_STR/"HALF    "/
#endif
C
      NDFTFG=1
      NEXFG =0
      NCORFG=0
      CSLT     = ONE
      CB88     = ONE
      CLYP     = ONE
      CVWN     = ONE
      DFTTYP(3) = 0.0D+00
C
C     ----- EXCHANGE ONLY METHOD -----
C
      IF(DFTYPE.EQ.LOCAL .OR. DFTYPE.EQ.LSDA .OR. DFTYPE.EQ.SLATER) THEN
         NEXFG     = 1
         EXCHR     = EXNM(1)
         CORCHR    = 'NONE'
      ELSEIF(DFTYPE.EQ.BECKE)  THEN
         NEXFG     = 2
         EXCHR     = EXNM(2)
         CORCHR    = 'NONE'
      ELSEIF(DFTYPE.EQ.PW91) THEN
C             THIS SHOULD BE REVIEWED SOMEDAY, CORRELATION FUNCTIONAL?
         NEXFG     = 3
         EXCHR     = EXNM(3)
         CORCHR    = 'NONE'
      ELSEIF(DFTYPE.EQ.PBE)  THEN
C             THIS IS PRESENTLY ONLY THE EXCHANGE PART OF PBE
         NEXFG     = 4
         EXCHR     = EXNM(4)
         CORCHR    = 'NONE'
      ELSEIF(DFTYPE.EQ.G96.OR.DFTYPE.EQ.GILL)  THEN
         NEXFG     = 5
         EXCHR     = EXNM(5)
         CORCHR    = 'NONE'
      ELSEIF(DFTYPE.EQ.PFREE)  THEN
         NEXFG     = 6
         EXCHR     = EXNM(6)
         CALL INPPFREE(PFTYP)
         CORCHR    = 'NONE'
C
C    ----- CORRELATION ONLY METHOD -----
C
      ELSEIF(DFTYPE.EQ.HVWN)   THEN
         NCORFG    = 1
         EXCHR     = 'HFX'
         CORCHR    = CORNM(1)
         DFTTYP(3) = 1.0D+00
      ELSEIF(DFTYPE.EQ.HLYP)   THEN
         NCORFG    = 2
         EXCHR     = 'HFX'
         CORCHR    = CORNM(2)
         DFTTYP(3) = 1.0D+00
      ELSEIF(DFTYPE.EQ.HOP)     THEN
         NCORFG    = 4
         EXCHR     = 'HFX'
         CORCHR    = CORNM(4)
         DFTTYP(3) = 1.0D+00
         QOP       = 2.3670D+00
C
C     ----- HYBRID METHOD -----
C
      ELSEIF(DFTYPE.EQ.B3LYP)  THEN
         NEXFG     =-1
         NCORFG    =-1
         EXCHR     = 'B88&HFX'
         CORCHR    = 'LYP88&VWN5'
         CSLT      = 0.08D+00
         CB88      = 0.72D+00
         CLYP      = 0.81D+00
         CVWN      = 0.19D+00
         DFTTYP(3) = 0.20D+00
      ELSEIF(DFTYPE.EQ.HALF) THEN
         NEXFG     = 2
         EXCHR     = 'B88&HFX'
         CORCHR    = 'NONE'
         CB88      = 0.5D+00
         DFTTYP(3) = 0.5D+00
      ELSEIF(DFTYPE.EQ.BHHLYP) THEN
         NEXFG     = 2
         NCORFG    = 2
         EXCHR     = 'B88&HFX'
         CORCHR    = 'LYP88'
         CB88      = 0.5D+00
         DFTTYP(3) = 0.5D+00
      ELSE
C
C     ---- OTHER FUNCTIONAL  -----
C
         CALL NAMEXC(NEX,NCOR,EX,COR,NUMEX,NUMCOR,DFTYPE,
     *               EXNM,CORNM,EXCHR,CORCHR,DQOP,PFTYP)
      END IF
      IF(MASWRK) WRITE (IW,9030) DFTYPE,QOP,PFTYP,NRAD,NTHE,NPHI,
     *                                           NRAD0,NTHE0,NPHI0
      RETURN
C
 9030 FORMAT(/5X,'GRID-BASED DFT OPTIONS'/5X,15("-")/
     *     5X,7HDFTTYP=,A8  ,5X,7HQOP   =,F8.3,5X,7HPFTYP =,A8/,
     *     5X,7HNRAD  =,I8  ,5X,7HNTHE  =,I8,  5X,7HNPHI  =,I8/,
     *     5X,7HNRAD0 =,I8  ,5X,7HNTHE0 =,I8,  5X,7HNPHI0 =,I8/)
C
      ENTRY RNAMEXC(XCHNG,CRRN)
      XCHNG = EXCHR
      CRRN  = CORCHR
      RETURN
      END
C*MODULE DFTEXC     *DECK INPPFREE
      SUBROUTINE INPPFREE(PFTYP)
C**********************************************************************
C
C     KINFG;
C     1  ...  THOMAS-FERMI-WEIZSACKER (TFW)
C     2  ...  THOMAS-FERMI-LAMDA-WEIZSACKER (TFLW) [LAMBDA = 0.8945]
C     3  ...  LEE-LEE-PARR (LLP)
C     4  ...  THAKKER (THAKKER)
C     5  ...  ??
C     6  ...  PADE (PADE)
C     7  ...  NEW-LLP (NLLP)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION LLP,NLLP
      SAVE PFKIN
C
      COMMON /DFTEXC/ PI,CSLT,CB88,CLYP,CVWN,QOP,NEXFG,NCORFG,NPFFG
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
C
      CHARACTER*10 PFKIN,PFCHR
C
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA TFW    /8HTFW     /,TFLW   /8HTFLW    /,LLP    /8HLLP     /
#else
      CHARACTER*8 :: TFW_STR
      EQUIVALENCE (TFW, TFW_STR)
      CHARACTER*8 :: TFLW_STR
      EQUIVALENCE (TFLW, TFLW_STR)
      CHARACTER*8 :: LLP_STR
      EQUIVALENCE (LLP, LLP_STR)
      DATA TFW_STR/"TFW     "/,TFLW_STR/"TFLW    "/,LLP_STR/"LLP     "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA THAK   /8HTHAK    /,FIT    /8HFIT     /,PADE   /8HPADE    /
#else
      CHARACTER*8 :: THAK_STR
      EQUIVALENCE (THAK, THAK_STR)
      CHARACTER*8 :: FIT_STR
      EQUIVALENCE (FIT, FIT_STR)
      CHARACTER*8 :: PADE_STR
      EQUIVALENCE (PADE, PADE_STR)
      DATA THAK_STR/"THAK    "/,FIT_STR/"FIT     "/,PADE_STR/"PADE    "/
#endif
#if !defined(SPEC_CPU_NO_HOLLERITH)
      DATA NLLP   /8HNLLP    /
#else
      CHARACTER*8 :: NLLP_STR
      EQUIVALENCE (NLLP, NLLP_STR)
      DATA NLLP_STR/"NLLP    "/
#endif
C
      NPFFG=0
C
      IF(PFTYP.EQ.TFW) THEN
         NPFFG  = 1
         PFKIN   = 'TF-W'
         QOP     = 2.3866D+00
      ELSEIF(PFTYP.EQ.TFLW)  THEN
         NPFFG  = 2
         PFKIN   = 'TF-LAMBDAW'
         QOP     = 2.3687D+00
      ELSEIF(PFTYP.EQ.LLP)  THEN
         NPFFG  = 3
         PFKIN   = 'LLP'
         QOP     = 2.3924D+00
      ELSEIF(PFTYP.EQ.THAK)  THEN
         NPFFG  = 4
         PFKIN   = 'THAKKER'
         QOP     = 2.3895D+00
      ELSEIF(PFTYP.EQ.FIT)  THEN
         NPFFG  = 5
         PFKIN   = 'FIT'
         QOP     = 2.3665D+00
      ELSEIF(PFTYP.EQ.PADE)  THEN
         NPFFG  = 6
         PFKIN   = 'PADE'
         QOP     = 2.3833D+00
      ELSEIF(PFTYP.EQ.NLLP)  THEN
         NPFFG  = 7
         PFKIN   = 'NLLP'
         QOP     =2.3653D+00
      ELSE
         WRITE (IW,*) 'PFTYP IS EITHER NOT GIVEN OR INCORRECT'
         CALL ABRT
      ENDIF
      RETURN
C
      ENTRY REFPFREE(PFCHR)
      PFCHR = PFKIN
      RETURN
      END
C*MODULE DFTEXC     *DECK KINETIC
      SUBROUTINE KINETIC(X,X2,TAUS,TAU,DTADX)
C
C**********************************************************************
C     DETERMINE THE VALUE OF THE TAU FOR EACH KINETIC ENERGY DENSITY
C
C     KINFG;
C     1  ...  THOMAS-FERMI-WEIZSACKER (TFW)
C     2  ...  THOMAS-FERMI-LAMDA-WEIZSACKER (TFLW) [LAMBDA = 0.8945]
C     3  ...  LEE-LEE-PARR (LLP)
C     4  ...  THAKKER (THAKKER)
C     5  ...  ??
C     6  ...  PADE (PADE)
C     7  ...  NEW-LLP (NLLP)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER ( ONE=1.0D+00, TWO=2.0D+00, THREE=3.0D+00)
C
      COMMON /DFTEXC/ PI,CSLT,CB88,CLYP,CVWN,QOP,NEXFG,NCORFG,NPFFG
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
C
C     ---- TFW ----
C
      IF (NPFFG.EQ.1) THEN
         TAU  =TAUS+X2/36.0D+00
         DTADX= X/18.0D+00
C
C     ---- TFLW ----
C
      ELSEIF (NPFFG.EQ.2) THEN
         TAU  =TAUS+0.8945D+00*X2/36.0D+00
         DTADX=0.8945D+00*X/18.0D+00
C
C     ----- LLP ----
C
      ELSEIF (NPFFG.EQ.3) THEN
         BB   =0.0044188D+00
         CC   =0.0253D+00
         Q    =SQRT(ONE+X2)
         S    =LOG(X+Q)
         T    =ONE+CC*X*S
         G    =BB*X2/T
         TAU  =TAUS*(ONE+G)
         DTADX=TAUS*(TWO*BB*X+BB*CC*X2*(S-X/Q))/T/T
C
C     ---- THAKKER ----
C
      ELSE IF (NPFFG.EQ.4) THEN
         BB   =0.0055D+00
         CC   =0.0253D+00
         DD   =0.072D+00
         Q    =SQRT(ONE+X2)
         S    =LOG(X+Q)
         T    =ONE+CC*X*S
         G    =BB*X2/T
         H    =ONE+TWO**(5.D+00/THREE)*X
         TAU  =TAUS*(ONE+G-DD*X/H)
         DTADX=TAUS*(TWO*BB*X+BB*CC*X2*(S-X/Q))/T/T
     >        -TAUS*DD/H/H
C
C     ---- FIT ----
C
      ELSE IF (NPFFG.EQ.4) THEN
         DENOM=ONE+0.0024D+00*X2
         TAU  =TAUS+X2/36.0D+00/DENOM
         DTADX=X/18.0D+00/DENOM-X2*X*0.0048D+00/36.0D+00/DENOM/DENOM
C
C     ---- PADE ----
C
      ELSE IF (NPFFG.EQ.5) THEN
         YYA  = X2/36.0D+00/TAUS
         YYA2 = YYA*YYA
         YYA3 = YYA*YYA2
         YYA4 = YYA2*YYA2
         YYA5 = YYA2*YYA3
         YYA6 = YYA3*YYA3
         A1   = 0.95D+00
         A2   = 14.28111D+00
         A3   =-19.57962D+00
         B1   =-0.05D+00
         B2   = 9.99802D+00
         B3   = 2.96085D+00
         DENOM= B3*YYA3+B2*YYA2+B1*YYA+ONE
         TAU  = TAUS*(9.0D+00*B3*YYA4+A3*YYA3+A2*YYA2+A1*YYA+ONE)/DENOM
         PNOMER= 9.0D+00*B3*B3*YYA6+18.0D+00*B3*B2*YYA5
     >         +(27.0D+00*B3*B1+A3*B2-B3*A2)*YYA4
     >         +(36.0D+00*B3+TWO*(A3*B1-B3*A1))*YYA3
     >         +(THREE*(A3-B3)+A2*B1-B2*A1)*YYA2
     >         +TWO*(A2-B2)*YYA+A1-B1
         DTADX= PNOMER/DENOM/DENOM*X/18.0D+00
      ELSE
         WRITE(IW,*) 'INVALID NUMBER OF NPFFG =',NPFFG
         CALL ABRT
         STOP
      END IF
C
      RETURN
      END
C
C*MODULE DFTEXC     *DECK LENXC
      SUBROUTINE LENXC(CHAR,NUM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*8 CHAR
      CHARACTER*26 ALPHA
      PARAMETER(ALPHA='ABCDEFGHIJKLMNOPQRSTUVWXYZ')
C
      DO I=1,8
         NUM=I
         IF (INDEX(ALPHA,CHAR(NUM:NUM)).EQ.0) GO TO 15
      ENDDO
C
      NUM = 8
C
 15   CONTINUE
      NUM=NUM-1
      RETURN
      END
C*MODULE DFTEXC  *DECK LSDSLT
      SUBROUTINE LSDSLT(RHFGVB,UROHF,ROA,ROB,FTOTWT,VXCA,VXCB,XALPHA,
     *                  SIGKA,SIGKB)
C***********************************************************************
C
C     SLATER(DIRAC) EXCHANGE FUNCTIONAL
C
C     XALF    ....  EXCHANGE ENERGY OF LDA PART
C     B88X    ....  EXCHANGE ENERGY OF GGA PART
C     DUM     ....  EXCHANGE GRADIENT  TO RHO
C     DUMX    ....  EXCHANGE GRADIENT TO X
C     DUMY    ....  EXCHANGE GRADIENT TO Y
C     DUMZ    ....  EXCHANGE GRADIENT TO Z
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL UROHF, RHFGVB
      PARAMETER (ONE=1.0D+00, TWO=2.0D+00, THREE=3.0D+00)
      COMMON /DFTEXC/ PI,CSLT,CB88,CLYP,CVWN,QOP,NEXFG,NCORFG,NPFFG
C***********************************************************************
       IF (RHFGVB) THEN
         RHO13= ROA**(ONE/THREE)
         FACP = -(6.0D+00/PI)**(ONE/THREE)
         FACE = -(THREE/4.0D+00)*(6.0D+00/PI)**(ONE/THREE)
         VXCA = VXCA+CSLT*FACP*RHO13
         VXCB = VXCB
         RHOE = RHO13**4
         XALPHA=XALPHA+CSLT*FTOTWT*RHOE*FACE*TWO
C     ----- FOR OP CORRELATION -----
         SIGKA= -FACE*TWO
C     ------------------------------
       ELSE IF (UROHF) THEN
         RHO13A=ROA**(ONE/THREE)
         RHO13B=ROB**(ONE/THREE)
         FACP=-(6.0D+00/PI)**(ONE/THREE)
         FACE=-(THREE/4.0D+00)*(6.0D+00/PI)**(ONE/THREE)
         VXCA = VXCA+CSLT*FACP*RHO13A
         VXCB = VXCB+CSLT*FACP*RHO13B
         RHOEA= RHO13A**4
         RHOEB= RHO13B**4
         RHOE = RHOEA + RHOEB
         XALPHA=XALPHA+CSLT*FTOTWT*RHOE*FACE
C     ----- FOR OP CORRELATION -----
         SIGKA= -(FACE+FACE)
         SIGKB= -(FACE+FACE)
C     ------------------------------
       ENDIF
C***********************************************************************
      RETURN
      END
C*MODULE DFTEXC  *DECK LYPCF
      SUBROUTINE LYPCF(RHFGVB,UROHF,NB,ROA,ROB,GRDAA,GRDBB,GRDAB,
     >                 GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
     >                 FTOTWT,ELYP,
     >                 DUMA,DUMXA,DUMYA,DUMZA,DUMB,DUMXB,DUMYB,DUMZB)
C***********************************************************************
C
C     LYP CORRELATION FUNCTIONAL
C
C     C. LEE, W. YANG AND R. G. PARR
C     PHYSICAL REVIEW B 37, 785-789 (1988)
C
C     -----
C     INPUT
C     -----
C
C     ROA,B   .... SPIN DENSITIES
C     GRAD**  .... DENSITY GRADIENTS
C     FTOTWT  .... VOLUME OF THIS GRID
C
C     ------
C     OUTPUT
C     ------
C
C     ELYP    ....  CORRELATION ENERGY OF LYP
C     DUM***  ....  FIRST DERIVATIVES OF THE EXCHANGE FUNCTINALS
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL UROHF, RHFGVB
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, TWO=2.0D+00, THREE=3.0D+00)
      COMMON /DFTEXC/ PI,CSLT,CB88,CLYP,CVWN,QOP,NEXFG,NCORFG,NPFFG
C
       FACE = (TWO**(11.0D+00/THREE))*(THREE/10.0D+00)*
     >        (THREE*PI**2)**(TWO/THREE)
       FACP = ONE*CLYP
       ALYPA=0.04918D+00
       ALYPB=0.13200D+00
       ALYPC=0.25330D+00
       ALYPD=0.34900D+00
C***********************************************************************
       IF (RHFGVB) THEN
C***********************************************************************
            RHOA  =ROA
            RHOB  =ROB
            GAA   =GRDAA
            GBB   =GRDBB
            GAB   =GRDAB
            RHOT  =RHOA+RHOB
            RHO13 =RHOT**(ONE/THREE)
            RHO13A=RHOA**(ONE/THREE)
            RHO13B=RHOB**(ONE/THREE)
            RHO43 =RHO13**4
            RHO83A=RHO13A**8
            RHO83B=RHO13B**8
            XC    =ALYPC/RHO13
            XD    =ALYPD/RHO13
            RX    =XD/(ONE+XD)
            DELTA =XC+RX
            OMEGA =(ALYPA*ALYPB/ALYPD)*RX*EXP(-XC)/RHO43
            DELTAP=(-DELTA+RX*RX)/THREE/RHOT
            OMEGAP=( DELTA-5.0D+00 )/THREE/RHOT
            RA    =RHOA/RHOT
            RB    =RHOB/RHOT
            RAA   =RA*RA
            RBB   =RB*RB
            RAB   =RA*RB
C***********************************************************************
            FAA   =(RAB/9)*(ONE-THREE*DELTA-(DELTA-11)*RA)
            FBB   =(RAB/9)*(ONE-THREE*DELTA-(DELTA-11)*RB)
            FAB   =(RAB/9)*(47-7*DELTA)
            DDAA  =-OMEGA*(FAA-RBB           )
            DDBB  =-OMEGA*(FBB-RAA           )
            DDAB  =-OMEGA*(FAB-    4.0D+00/THREE)
            T1    =(-4.0D+00*ALYPA/ALYPD)*RX*RAB*RHO43
            T2A   =(-FACE*OMEGA*RAB)*RHO83A
            T2B   =(-FACE*OMEGA*RAB)*RHO83B
            T2    =T2A+T2B
            ELYPI =T1+T2+DDAA*GAA+DDBB*GBB+DDAB*GAB
C***********************************************************************
            DFAADA=-(RAB/9)*((THREE+RA)*DELTAP+
     1                             (DELTA-11)*RB/RHOT)
            DFBBDA=-(RAB/9)*((THREE+RB)*DELTAP-
     1                             (DELTA-11)*RB/RHOT)
            DFABDA=-(RAB/9)*7*DELTAP
            D2DAAA= OMEGAP*DDAA-OMEGA*(DFAADA+RBB*(TWO/RHOT))
            D2DABB= OMEGAP*DDBB-OMEGA*(DFBBDA-RAB*(TWO/RHOT))
            D2DAAB= OMEGAP*DDAB-OMEGA* DFABDA
            DT1DA =(T1/RHOT)*(RX/THREE+ONE)
            DT2DA =OMEGAP*(T2A+T2B)+(8.0D+00/THREE)*T2A/RHOA
            DDA   =DT1DA+DT2DA+D2DAAA*GAA
     1                        +D2DABB*GBB
     2                        +D2DAAB*GAB
C***********************************************************************
            ELYP  =ELYP  +FACP*ELYPI*FTOTWT
            U     = DDA
            U     =U*FACP
            DUMA  =DUMA+U
            W     =(DDAA+0.5D+00*DDAB)
            W     =W*FACP
            DUMXA =DUMXA+TWO*W*GRADXA
            DUMYA =DUMYA+TWO*W*GRADYA
            DUMZA =DUMZA+TWO*W*GRADZA
C***********************************************************************
       ELSE IF (UROHF) THEN
C***********************************************************************
            GAA   =GRDAA
            GBB   =GRDBB
            GAB   =GRDAB
            RHOA  =ROA
            RHOB  =ROB
            RHOT  =RHOA+RHOB
            RHO13 =RHOT**(ONE/THREE)
            RHO13A=RHOA**(ONE/THREE)
            RHO13B=RHOB**(ONE/THREE)
            RHO43 =RHO13**4
            RHO83A=RHO13A**8
            RHO83B=RHO13B**8
            XC    =ALYPC/RHO13
            XD    =ALYPD/RHO13
            RX    =XD/(ONE+XD)
            DELTA =XC+RX
            OMEGA =(ALYPA*ALYPB/ALYPD)*RX*EXP(-XC)/RHO43
            DELTAP=(-DELTA+RX*RX)/THREE/RHOT
            OMEGAP=( DELTA-5.0D+00 )/THREE/RHOT
            RA    =RHOA/RHOT
            RB    =RHOB/RHOT
            RAA   =RA*RA
            RBB   =RB*RB
            RAB   =RA*RB
            ZETA  =RA-RB
C***********************************************************************
            FAA   =(RAB/9)*(ONE-THREE*DELTA-(DELTA-11)*RA)
            FBB   =(RAB/9)*(ONE-THREE*DELTA-(DELTA-11)*RB)
            FAB   =(RAB/9)*(47-7*DELTA)
            DDAA  =-OMEGA*(FAA-RBB           )
            DDBB  =-OMEGA*(FBB-RAA           )
            DDAB  =-OMEGA*(FAB-    4.0D+00/THREE)
            T1    =(-4.0D+00*ALYPA/ALYPD)*RX*RAB*RHO43
            T2A   =(-FACE*OMEGA*RAB)*RHO83A
            T2B   =(-FACE*OMEGA*RAB)*RHO83B
            T2    =T2A+T2B
            ELYPI =T1+T2+DDAA*GAA+DDBB*GBB+DDAB*GAB
C***********************************************************************
            DFAADA=-ZETA*FAA/RHOA
     1             -(RAB/9)*((THREE+RA)*DELTAP+
     2                             (DELTA-11)*RB/RHOT)
            DFBBDA=-ZETA*FBB/RHOA
     1             -(RAB/9)*((THREE+RB)*DELTAP-
     2                             (DELTA-11)*RB/RHOT)
            DFABDA=-ZETA*FAB/RHOA
     1             -(RAB/9)*7*DELTAP
            IF(NB.GT.0) THEN
            DFAADB= ZETA*FAA/RHOB
     1             -(RAB/9)*((THREE+RA)*DELTAP-
     2                             (DELTA-11)*RA/RHOT)
            DFBBDB= ZETA*FBB/RHOB
     1             -(RAB/9)*((THREE+RB)*DELTAP+
     2                             (DELTA-11)*RA/RHOT)
            DFABDB= ZETA*FAB/RHOB
     1             -(RAB/9)*7*DELTAP
            ELSE
               DFAADB = ZERO
               DFBBDB = ZERO
               DFABDB = ZERO
            END IF
C***********************************************************************
            D2DAAA= OMEGAP*DDAA-OMEGA*(DFAADA+RBB*(TWO/RHOT))
            D2DBAA= OMEGAP*DDAA-OMEGA*(DFAADB-RAB*(TWO/RHOT))
            D2DABB= OMEGAP*DDBB-OMEGA*(DFBBDA-RAB*(TWO/RHOT))
            D2DBBB= OMEGAP*DDBB-OMEGA*(DFBBDB+RAA*(TWO/RHOT))
            D2DAAB= OMEGAP*DDAB-OMEGA* DFABDA
            D2DBAB= OMEGAP*DDAB-OMEGA* DFABDB
C***********************************************************************
            DT1DA =(T1/RHOT)*(RX/THREE+RB/RA)
            DT2DA =(OMEGAP-ZETA/RHOA)*T2B+
     1             (OMEGAP+(8.0D+00/THREE-ZETA)/RHOA)*T2A
            IF(NB.GT.0) THEN
               DT1DB =(T1/RHOT)*(RX/THREE+RA/RB)
               DT2DB =(OMEGAP+ZETA/RHOB)*T2A+
     1                (OMEGAP+(8.0D+00/THREE+ZETA)/RHOB)*T2B
            ELSE
               DT1DB = ZERO
               DT2DB = ZERO
            END IF
            DDA   =DT1DA+DT2DA+D2DAAA*GAA
     1                        +D2DABB*GBB
     2                        +D2DAAB*GAB
            DDB   =DT1DB+DT2DB+D2DBAA*GAA
     1                        +D2DBBB*GBB
     2                        +D2DBAB*GAB
C***********************************************************************
            ELYP   =ELYP  +FACP*ELYPI*FTOTWT
            UA     = DDA
            UA     =UA*FACP
            DUMA   =DUMA+UA
            UB     = DDB
            UB     =UB*FACP
            DUMB   =DUMB+UB
            WAA    =TWO*DDAA
            WAA    =WAA*FACP
            WBB    =TWO*DDBB
            WBB    =WBB*FACP
            WAB    =    DDAB
            WAB    =WAB*FACP
            DUMXA  =DUMXA+WAA*GRADXA+WAB*GRADXB
            DUMYA  =DUMYA+WAA*GRADYA+WAB*GRADYB
            DUMZA  =DUMZA+WAA*GRADZA+WAB*GRADZB
            DUMXB  =DUMXB+WBB*GRADXB+WAB*GRADXA
            DUMYB  =DUMYB+WBB*GRADYB+WAB*GRADYA
            DUMZB  =DUMZB+WBB*GRADZB+WAB*GRADZA
C***********************************************************************
            ENDIF
        RETURN
      END
C*MODULE DFTEXC   *DECK NAMEXC
      SUBROUTINE NAMEXC(NEX,NCOR,EX,COR,NUMEX,NUMCOR,DFTYPE,
     *                  EXNM,CORNM,EXCHR,CORCHR,DQOP,PFTYP)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      COMMON /DFTEXC/ PI,CSLT,CB88,CLYP,CVWN,QOP,NEXFG,NCORFG,NPFFG
C
      CHARACTER*8  EX(NEX),COR(NCOR)
      CHARACTER*10 EXNM(NEX),CORNM(NEX),EXCHR,CORCHR
      CHARACTER*8  TRYFC,INPXC
C
      DIMENSION NUMEX(NEX),NUMCOR(NCOR),DQOP(NEX)
C
      WRITE(UNIT=INPXC,FMT='(A8)') DFTYPE
C
      DO 10 IEX=1,NEX
         DO 20 ICOR=1,NCOR
            CALL LENXC(EX(IEX),LEX)
            CALL LENXC(COR(ICOR),LCOR)
            TRYFC=EX(IEX)(1:LEX)//COR(ICOR)(1:LCOR)
            IF(TRYFC.EQ.INPXC) GOTO 30
 20      CONTINUE
 10   CONTINUE
C
      WRITE (6,9040) INPXC
 9040 FORMAT(1X,'GRID CODE FINDS UNSUPPORTED DFTTYP=',A8)
      CALL ABRT
      STOP
C
 30   CONTINUE
      NEXFG  = NUMEX(IEX)
      NCORFG = NUMCOR(ICOR)
      EXCHR  = EXNM(IEX)
      CORCHR = CORNM(ICOR)
C
      IF (ICOR.EQ.4.AND.QOP.LT.1.0D-15) QOP=DQOP(IEX)
      IF (IEX.EQ.6) CALL INPPFREE(PFTYP)
      RETURN
      END
C*MODULE DFTEXC  *DECK OPCOR
      SUBROUTINE OPCOR(RHFGVB,UROHF,ROA,ROB,GRDAA,GRDBB,
     >                 GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
     >                 FTOTWT,EOP,
     >                 SIGKA,SIGKB,DKDXA,DKDXB,
     >                 DUMA,DUMXA,DUMYA,DUMZA,DUMB,DUMXB,DUMYB,DUMZB)
C***********************************************************************
C
C     ONE-PARAMETER PROGRESSIVE CORRELATION FUNCTIONAL
C
C     T. TSUNEDA AND K. HIRAO
C     J. CHEM. PHYS. 110, 10664-10678 (1999)
C
C     -----
C     INPUT
C     -----
C
C     ROA,B   .... SPINDENSITIES
C     GRAD**  .... DENSITY GRADIENTS
C     FTOTWT  .... VOLUME OF THIS GRID
C
C     ------
C     OUTPUT
C     ------
C
C     EOP    ....  CORRELATION ENERGY OF OP
C     DUM***  ....  FIRST DERIVATIVES OF THE CORRELATION FUNCTINALS
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL UROHF, RHFGVB
      PARAMETER ( ONE=1.0D+00, TWO=2.0D+00, THREE=3.0D+00)
      COMMON /DFTEXC/ PI,CSLT,CB88,CLYP,CVWN,QOP,NEXFG,NCORFG,NPFFG
      FACP   = ONE
C***********************************************************************
       IF (RHFGVB) THEN
C***********************************************************************
            RHOI=ROA
            RHOA=ROA
            RHOB=ROA
            RHOP=RHOI**(ONE/THREE)
            RHOE=RHOI*RHOP
            DRHO=SQRT(GRDAA)
            X   =DRHO/RHOE
C***********************************************************************
C      THE EXCHANGE PART DERIVED FROM THE EXCHANGE FUNCTIONAL
C***********************************************************************
            DXDR  =-(4.0D+00/THREE)*X/RHOI
            DKDR  =DKDXA*DXDR
            DKDAA =DKDXA/(TWO*DRHO*RHOE)
C***********************************************************************
C      THE CALCULATION OF THE OP CORRELATION ENERGY
C***********************************************************************
            BETA  =(QOP/TWO)*RHOP*SIGKA
            BETA4 =BETA**4
            BETA3 =BETA**3
            BETA2 =BETA**2
            DENOM =BETA4+(1.1284D+00)*BETA3+(0.3183D+00)*BETA2
            ENEWP =((1.5214D+00)*BETA+(0.5764D+00))/DENOM
            ENEWI =-ENEWP*RHOA*RHOB
            EOP   =EOP  +ENEWI*FTOTWT
C***********************************************************************
C      THE CALCULATION OF THE FIRST DERIVATIVES OF OP
C***********************************************************************
            DEPDB =((-4.5642D+00)*BETA4+(-5.7391D+00)*BETA3
     >             +(-2.4355D+00)*BETA2+(-3.6694D-01)*BETA)/DENOM**2
            DRPKDR=ONE/THREE*RHOI**(-TWO/THREE)*SIGKA+RHOP*DKDR
            DBDR  =QOP/4.0D+00*DRPKDR
            DEDR  =-RHOI*ENEWP-RHOI**2*DBDR*DEPDB
            U     =DEDR
            U     =U*FACP
            DUMA  =DUMA+U
C***********************************************************************
            DBDAA =QOP/4.0D+00*RHOP*DKDAA
            DEDAA =-RHOI**2*DBDAA*DEPDB
            W     =DEDAA
            W     =W*FACP
            DUMXA =DUMXA+TWO*W*GRADXA
            DUMYA =DUMYA+TWO*W*GRADYA
            DUMZA =DUMZA+TWO*W*GRADZA
C***********************************************************************
       ELSE IF (UROHF) THEN
C***********************************************************************
            RHOI  = ROA + ROB
            DRHOA =SQRT(GRDAA)
            DRHOB =SQRT(GRDBB)
            RHOA =ROA
            RHOB =ROB
            IF (RHOA.GT.RHOB*1.0D+04) GOTO 1
            RHO13A=RHOA**(  ONE/THREE)
            RHO13B=RHOB**(  ONE/THREE)
            RHO43A=RHO13A*RHOA
            RHO43B=RHO13B*RHOB
            XA    =DRHOA/RHO43A
            XB    =DRHOB/RHO43B
C***********************************************************************
C      THE EXCHANGE PART DERIVED FROM THE EXCHANGE FUNCTIONAL
C***********************************************************************
            DXADRA  =-(4.0D+00/THREE)*XA/RHOA
            DKADRA  =DKDXA*DXADRA
C
            DXBDRB  =-(4.0D+00/THREE)*XB/RHOB
            DKBDRB  =DKDXB*DXBDRB
C
            DKADGA  =DKDXA/RHO43A
            DKADGAGA=DKADGA/TWO/DRHOA
C
            DKBDGB  =DKDXB/RHO43B
            DKBDGBGB=DKBDGB/TWO/DRHOB
C***********************************************************************
C      THE CALCULATION OF THE OP CORRELATION ENERGY
C***********************************************************************
            BDENOM=RHO13A*SIGKA+RHO13B*SIGKB
            BETA  =QOP*RHO13A*RHO13B*SIGKA*SIGKB/BDENOM
            BETA2 =BETA**2
            BETA3 =BETA**3
            BETA4 =BETA**4
            DENOM =BETA4+(1.1284D+00)*BETA3+(0.3183D+00)*BETA2
            ENEWP =((1.5214D+00)*BETA+0.5764D+00)/DENOM
            ENEWI =-RHOA*RHOB*ENEWP
            EOP   =EOP   +ENEWI *FTOTWT
C***********************************************************************
C      THE CALCULATION OF THE FIRST DERIVATIVES OF OP
C***********************************************************************
            DEPDB  =((-4.5642D+00)*BETA4+(-5.7391D+00)*BETA3
     1              +(-2.4355D+00)*BETA2+(-3.6694D-01)*BETA)
     2              /DENOM**2
            DRPKDRA =ONE/THREE*(RHOA**(-TWO/THREE))*SIGKA
     1               +RHO13A*DKADRA
            DBDRA   =QOP*DRPKDRA*(RHO13B*SIGKB/BDENOM)**2
            DEDRA   =-RHOB*ENEWP-RHOA*RHOB*DBDRA*DEPDB
C***********************************************************************
            DRPKDRB =ONE/THREE*(RHOB**(-TWO/THREE))*SIGKB
     1               +RHO13B*DKBDRB
            DBDRB   =QOP*DRPKDRB*(RHO13A*SIGKA/BDENOM)**2
            DEDRB   =-RHOA*ENEWP-RHOB*RHOA*DBDRB*DEPDB
C***********************************************************************
            DBDGAGA =QOP*RHO13A*(RHO13B**2)*(SIGKB**2)
     1               /(BDENOM**2)*DKADGAGA
            DEDGAGA =-RHOA*RHOB*DBDGAGA*DEPDB
C***********************************************************************
            DBDGBGB =QOP*RHO13B*(RHO13A**2)*(SIGKA**2)
     1           /(BDENOM**2)*DKBDGBGB
            DEDGBGB =-RHOB*RHOA*DBDGBGB*DEPDB
C***********************************************************************
            UA     =DEDRA
            UA     =UA*FACP
            DUMA   =DUMA+UA
            UB     =DEDRB
            UB     =UB*FACP
            DUMB   =DUMB+UB
            WAA    =DEDGAGA
            WAA    =WAA*FACP
            WBB    =DEDGBGB
            WBB    =WBB*FACP
            DUMXA  =DUMXA+TWO*WAA*GRADXA
            DUMYA  =DUMYA+TWO*WAA*GRADYA
            DUMZA  =DUMZA+TWO*WAA*GRADZA
            DUMXB  =DUMXB+TWO*WBB*GRADXB
            DUMYB  =DUMYB+TWO*WBB*GRADYB
            DUMZB  =DUMZB+TWO*WBB*GRADZB
 1          CONTINUE
       ENDIF
C***********************************************************************
      RETURN
      END
C*MODULE DFTEXC  *DECK PBEXF
      SUBROUTINE PBEXF(RHFGVB,UROHF,ROA,ROB,GRDAA,GRDBB,
     >                 GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
     >                 FTOTWT,SIGKA,SIGKB,DKDXA,DKDXB,
     >                 XALF,B88X,DUMA,DUMXA,DUMYA,DUMZA,
     >                 DUMB,DUMXB,DUMYB,DUMZB)
C***********************************************************************
C
C     PERDEW-BURKE-ERNZERHOF (PBE) EXCHANGE FUNCTIONAL
C
C     J. P. PERDEW , K. BURKE AND M. ERNZERHOF
C     PHYS. REV. LET. 77,3865 (1996)
C
C     -----
C     INPUT
C     -----
C
C     ROA,B   .... SPINDENSITIES
C     GRAD**  .... DENSITY GRADIENTS
C     FTOTWT  .... VOLUME OF THIS GRID
C
C     ------
C     OUTPUT
C     ------
C
C     XALF    ....  EXCHANGE ENERGY OF LDA PART
C     B88X    ....  EXCHANGE ENERGY OF GGA PART
C     DUM***  ....  FIRST DERIVATIVES OF THE EXCHANGE FUNCTINALS
C     SIGKA,B ....  EXC=-1/2*INT(SIGK*RHO^(4/3))
C     DKDXA,B ....  DSIGK/DX
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL UROHF, RHFGVB
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, TWO=2.0D+00)
      COMMON /DFTEXC/ PI,CSLT,CB88,CLYP,CVWN,QOP,NEXFG,NCORFG,NPFFG
C***********************************************************************
       X1 = (3.0D+00/PI)**(ONE/3.0D+00)
       X2 = (3.0D+00*PI*PI)**(ONE/3.0D+00)
       TWO13 = TWO**(ONE/3.0D+00)
       FACE  =-0.75D+00*X1*TWO13
       FACP  = 4.0D+00/3.0D+00
       IF (RHFGVB) THEN
         RHO=ROA
         GRD=GRDAA
         RHO13=RHO**(ONE/3.0D+00)
         RHOE =RHO13**4
         DRHO =SQRT(GRD)
         X    =DRHO/RHOE
         S    =X/TWO/TWO13/X2
         Q    =ONE+0.21951D+00*S*S/0.804D+00
         G    =FACE*0.804D+00*(ONE-ONE/Q)
         H    =FACE*TWO*0.21951D+00*S/Q**2
         R    =FACP*RHO13
         E    =FACE
         XCL  =E*RHOE
         XCNL =G*RHOE
         XALF =XALF +CB88*XCL *TWO*FTOTWT
         B88X =B88X +CB88*XCNL*TWO*FTOTWT
         U    =CB88*R*(E+G-S*H)
         DUMA =DUMA+U
         W    =CB88*0.5D+00*H/TWO/TWO13/X2/SQRT(GRD)
         DUMXA=DUMXA+TWO*W*GRADXA
         DUMYA=DUMYA+TWO*W*GRADYA
         DUMZA=DUMZA+TWO*W*GRADZA
C     ----- FOR OP CORRELATION -----
         SIGKA= -(FACE+G)*TWO
         DKDXA= -H/TWO13/X2
C     -----------------------------
C***********************************************************************
C      OPEN SHELL CALCULATION
C***********************************************************************
       ELSE IF (UROHF) THEN
         RHO13A=ROA**(ONE/3.0D+00)
         RHOEA =RHO13A**4
         SA    =SQRT(GRDAA)/RHOEA/TWO/TWO13/X2
         QA    =ONE+0.21951D+00*SA*SA/0.804D+00
         GA    =FACE*0.804D+00*(ONE-ONE/QA)
         HA    =FACE*TWO*0.21951D+00*SA/QA**2
         RA    =FACP*RHO13A
         EA    =FACE
         UA    =CSLT*RA*(EA+GA-SA*HA)
         DUMA  =DUMA+UA
         WA    =CB88*0.5D+00*HA/TWO/TWO13/X2/SQRT(GRDAA)
         DUMXA =DUMXA+TWO*WA*GRADXA
         DUMYA =DUMYA+TWO*WA*GRADYA
         DUMZA =DUMZA+TWO*WA*GRADZA
         RHO13B=ROB**(ONE/3.0D+00)
         RHOEB =RHO13B**4
         EB    =FACE
         IF(ROB.LT.1.0D-15) THEN
           GB  = ZERO
           GOTO 9999
         ENDIF
         SB    =SQRT(GRDBB)/RHOEB/TWO/TWO13/X2
         QB    =ONE+0.21951D+00*SB*SB/0.804D+00
         GB    =FACE*0.804D+00*(ONE-ONE/QB)
         HB    =FACE*TWO*0.21951D+00*SB/QB**2
         RB    =FACP*RHO13B
         UB    =CSLT*RB*(EB+GB-SB*HB)
         DUMB  =DUMB+UB
         WB    =CB88*0.5D+00*HB/TWO/TWO13/X2/SQRT(GRDBB)
         DUMXB =DUMXB+TWO*WB*GRADXB
         DUMYB =DUMYB+TWO*WB*GRADYB
         DUMZB =DUMZB+TWO*WB*GRADZB
 9999    CONTINUE
         XCL   =EA*RHOEA+EB*RHOEB
         XCNL  =GA*RHOEA+GB*RHOEB
         XALF  =XALF  +CSLT*XCL *FTOTWT
         B88X  =B88X  +CB88*XCNL*FTOTWT
C      ----- FOR OP CORRELATION -----
         SIGKA= -(FACE+GA)*TWO
         SIGKB= -(FACE+GB)*TWO
         DKDXA= -HA/TWO13/X2
         DKDXB= -HB/TWO13/X2
C     -------------------------------
       ENDIF
C***********************************************************************
      RETURN
      END
C*MODULE DFTEXC  *DECK PFREE
      SUBROUTINE PFREE(RHFGVB,UROHF,ROA,ROB,GRDAA,GRDBB,
     >                 GRADXA,GRADYA,GRADZA,GRADXB,GRADYB,GRADZB,
     >                 FTOTWT,SIGKA,SIGKB,DKDXA,DKDXB,
     >                 XALF,PFX,DUMA,DUMXA,DUMYA,DUMZA,
     >                 DUMB,DUMXB,DUMYB,DUMZB)
C
C***********************************************************************
C
C     PARAMETER-FREE EXCHANGE FUNCTIONAL
C
C     T. TSUNEDA AND K. HIRAO
C     PHYS. REV. B IN PRESS (2000)
C
C     -----
C     INPUT
C     -----
C
C     ROA,B   .... SPIN DENSITIES
C     GRAD**  .... DENSITY GRADIENTS
C     TOTWT   .... VOLUME OF THIS GRID
C
C     ------
C     OUTPUT
C     ------
C
C     PFX    ....  EXCHANGE ENERGY OF GGA PART
C     DUM***  ....  FIRST DERIVATIVES OF THE EXCHANGE FUNCTINALS
C     SIGKA,B ....  EXC=-1/2*INT(SIGK*RHO^(4/3))
C     DKDXA,B ....  DSIGK/DX
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (ZERO=0.0D+00,HALF=0.5D+00,
     >           ONE=1.0D+00, TWO=2.0D+00, THREE=3.0D+00)
      COMMON /DFTEXC/ PI,CSLT,CB88,CLYP,CVWN,QOP,NEXFG,NCORFG,NPFFG
      LOGICAL UROHF, RHFGVB
C
C     ---- CALCULATE PHYSICAL CONSTANTS ----
C
      FACP  = 4.0D+00/3.0D+00
      X1    = (3.0D+00/PI)**(ONE/3.0D+00)
      FACE  = 1.5D+00*X1*(ONE/TWO )**(TWO/THREE)
      TAUS  =(THREE/5.0D+00)*(6.0D+00*PI*PI)**(TWO/THREE)
      COEF1 = 27.0D+00*PI/5.0D+00
      COEF2 = 7.0D+00/108.0D+00
C
C***********************************************************************
C      CLOSED SHELL CALCULATION
C***********************************************************************
      IF (RHFGVB) THEN
         RHO = ROA
         GRD = GRDAA
         RHO13 = RHO**(ONE/THREE)
         RHOE = RHO*RHO13
         X = SQRT(GRD)/RHOE
         X2 = X*X
C
C     ---- CALCULATE KINETIC DENSITY -----
C
         CALL KINETIC(X,X2,TAUS,TAU,DTADX)
C
C     ---- CALCULATE K-SIGMA AND IT'S DERIVATIVE -----
C
         SIGKA =-HALF*COEF1/TAU*(ONE+COEF2*X2/TAU)
         DKDXA = HALF*COEF1/TAU*(DTADX+TWO*COEF2*X*(X*DTADX/TAU-ONE))
     >        /TAU
C
         R    =FACP*RHO13
         XCNL =SIGKA*RHOE
         PFX  =PFX +CB88*TWO*XCNL*FTOTWT
         U    =CB88*R*(SIGKA-X*DKDXA)
         DUMA =DUMA+U
         W    =CB88*HALF*DKDXA/SQRT(GRD)
         DUMXA=DUMXA+TWO*W*GRADXA
         DUMYA=DUMYA+TWO*W*GRADYA
         DUMZA=DUMZA+TWO*W*GRADZA
         XALF =ZERO
C     ---- FOR OP CORRELATION ----
         IF (-SIGKA.LE.FACE) THEN
            SIGKA = -FACE
            DKDXA = ZERO
         ENDIF
         SIGKA=-SIGKA-SIGKA
         DKDXA=-DKDXA-DKDXA
C
C***********************************************************************
C     OPEN SHELL CALCULATION
C***********************************************************************
      ELSE IF (UROHF) THEN
         RHO13A = ROA**(ONE/THREE)
         RHOEA  = ROA*RHO13A
         XA     = SQRT(GRDAA)/RHOEA
         X2A    = XA*XA
C
C     ---- CALCULATE KINETIC DENSITY -----
C
         CALL KINETIC(XA,X2A,TAUS,TAUA,DTADXA)
C
C     ---- CALCULATE K-SIGMA AND IT'S DERIVATIVE -----
C
         SIGKA =-HALF*COEF1/TAUA*(ONE+COEF2*X2A/TAUA)
         DKDXA = HALF*COEF1/TAUA*(DTADXA+TWO*COEF2*XA
     >        *(XA*DTADXA/TAUA-ONE))/TAUA
C
         RA    = FACP*RHO13A
         UA    = CB88*RA*(SIGKA-XA*DKDXA)
         DUMA  = DUMA+UA
         WA    = CB88*HALF*DKDXA/SQRT(GRDAA)
         DUMXA = DUMXA+TWO*WA*GRADXA
         DUMYA = DUMYA+TWO*WA*GRADYA
         DUMZA = DUMZA+TWO*WA*GRADZA
C
C     ---- BETA CONTRIBUTION
C
         IF(ROB.LT.1.0D-15) THEN
            SIGKB=ZERO
           GOTO 9999
         ENDIF
C
         RHO13B = ROB**(ONE/THREE)
         RHOEB  = ROB*RHO13B
         XB     = SQRT(GRDBB)/RHOEB
         X2B    = XB*XB
C
C     ---- CALCULATE KINETIC DENSITY -----
C
         CALL KINETIC(XB,X2B,TAUS,TAUB,DTADXB)
C
C     ---- CALCULATE K-SIGMA AND IT'S DERIVATIVE -----
C
         SIGKB =-HALF*COEF1/TAUB*(ONE+COEF2*X2B/TAUB)
         DKDXB = HALF*COEF1/TAUB*(DTADXB+TWO*COEF2*XB
     >        *(XB*DTADXB/TAUB-ONE))/TAUB
C
         RB    = FACP*RHO13B
         UB    = CB88*RB*(SIGKB-XB*DKDXB)
         DUMB  = DUMB+UB
         WB    = CB88*HALF*DKDXB/SQRT(GRDBB)
         DUMXB = DUMXB+TWO*WB*GRADXB
         DUMYB = DUMYB+TWO*WB*GRADYB
         DUMZB = DUMZB+TWO*WB*GRADZB
 9999    CONTINUE
         XCNL  = SIGKA*RHOEA+SIGKB*RHOEB
         PFX   = PFX +CB88*XCNL*FTOTWT
         XALF =ZERO
C     ---- FOR OP CORRELATION ----
         IF (-SIGKA.LE.FACE) THEN
            SIGKA = -FACE
            DKDXA = ZERO
         ENDIF
         IF (-SIGKB.LE.FACE) THEN
            SIGKB = -FACE
            DKDXB = ZERO
         ENDIF
         SIGKA=-SIGKA-SIGKA
         DKDXA=-DKDXA-DKDXA
         SIGKB=-SIGKB-SIGKB
         DKDXB=-DKDXB-DKDXB
C
      END IF
      RETURN
      END
C*MODULE DFTEXC  *DECK VWN5CF
      SUBROUTINE VWN5CF(RHFGVB,UROHF,ROA,ROB,FTOTWT,VXCA,VXCB,EVWN)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL UROHF, RHFGVB
C
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, TWO=2.0D+00, THREE=3.0D+00,
     *           FOUR=4.0D+00, SIX=6.0D+00, EIGHT=8.0D+00,
     *           ANINE=9.0D+00)
      PARAMETER (THIRD=ONE/THREE, FRTHRD=FOUR/THREE, SIXTH=ONE/SIX)
C                FACR = (THREE/(FOUR*PI))**SIXTH
C                FACH = FOUR/((TWO**THIRD-ONE)*ANINE)
      PARAMETER (FACR = 0.78762331789974325053D+00)
      PARAMETER (FACH = 1.70992093416136561756D+00)
      PARAMETER (FACG = ANINE/EIGHT)
      PARAMETER (FACDG= THREE/TWO)
C
C        THE A,B,C,X0 PARAMETERS OF THE FIT ARE AS FOLLOWS.
C        P,F,A MEAN PARAMAGNETIC, FERROMAGNETIC, AND STIFFNESS TERMS.
C        NOTE UNITS OF THE -A- CONSTANTS ARE RYDBERGS, SO DIVIDE BY 2.
C
      PARAMETER (VWNAP= 0.0621814D+00/TWO, VWNBP= 3.7274400D+00,
     *           VWNCP=12.9352000D+00, VWNX0P=-0.1049800D+00)
      PARAMETER (VWNAF= VWNAP/TWO, VWNBF= 7.0604200D+00,
     *           VWNCF=18.0578000D+00, VWNX0F=-0.3250000D+00)
      PARAMETER (VWNAA=-0.03377372D+00/TWO, VWNBA= 1.1310700D+00,
     *           VWNCA=13.0045000D+00, VWNX0A=-0.0047584D+00)
C
      COMMON /DFTEXC/ PI,CSLT,CB88,CLYP,CVWN,QOP,NEXFG,NCORFG,NPFFG
C
C     VWN CORRELATION FUNCTIONAL - FORMULA V
C     S.H.VOSKO, L.WILK, M.NUSAIR  CAN.J.PHYS. 58, 1200-11(1980)
C     THE NECESSARY FORMULAE ARE CLEARLY SPELLED OUT IN THE APPENDIX OF
C     B.G.JOHNSON, P.M.W.GILL, J.A.POPLE  J.CHEM.PHYS. 98, 5612-26(1993)
C     THE PARAMETERS APPEARING ABOVE CAN BE FOUND IN THE MOLPRO MANUAL,
C     ALTHOUGH A FEW ARE VISIBLE IN THE VWN PAPER ITSELF.
C
C     ON ENTRY,
C     ROA,B   .... SPIN DENSITIES
C     FTOTWT  .... VOLUME OF THIS GRID POINT
C
C     ON EXIT,
C     EVWN    .... CORRELATION ENERGY OF VWN5
C     VXCA,B  .... FIRST DERIVATIVES OF THE CORRELATION FUNCTIONAL
C
      IF (RHFGVB) THEN
        FACVWN = CVWN
        RHOA =ROA
        RHOB =ROB
        RHOT =RHOA+RHOB
        RHO16=RHOT**SIXTH
C
        Q    = SQRT(FOUR*VWNCP-VWNBP*VWNBP)
        XX0  = VWNX0P*VWNX0P+VWNX0P*VWNBP+VWNCP
        COEF1= VWNAP
        COEF2=-VWNAP*VWNBP*VWNX0P/XX0
        COEF3= VWNAP*(TWO*VWNBP/Q)*((VWNCP-VWNX0P*VWNX0P)/XX0)
C
        X    =FACR/RHO16
        XX   =X*X+X*VWNBP+VWNCP
        T1   =LOG(X*X/XX)
        T2   =LOG((X-VWNX0P)*(X-VWNX0P)/XX)
        T3   =ATAN(Q/(TWO*X+VWNBP))
        EPSC =COEF1*T1+COEF2*T2+COEF3*T3
        EVWN =EVWN+FACVWN*EPSC*RHOT*FTOTWT
C
        DEPSC= VWNAP*(ONE/X-(X/XX)*(ONE+VWNBP/(X-VWNX0P)))
        DUMA = (EPSC-X*DEPSC/THREE)
        DUMB = ZERO
        VXCA = VXCA + FACVWN*DUMA
        VXCB = VXCB + DUMB
      END IF
C
      IF (UROHF) THEN
        FACVWN=CVWN
        RHOA  =ROA
        RHOB  =ROB
        RHOT  =RHOA+RHOB
        RHO16 =RHOT**SIXTH
C
        QP    = SQRT(FOUR*VWNCP-VWNBP*VWNBP)
        QF    = SQRT(FOUR*VWNCF-VWNBF*VWNBF)
        QA    = SQRT(FOUR*VWNCA-VWNBA*VWNBA)
        XX0P  = VWNX0P*VWNX0P+VWNX0P*VWNBP+VWNCP
        XX0F  = VWNX0F*VWNX0F+VWNX0F*VWNBF+VWNCF
        XX0A  = VWNX0A*VWNX0A+VWNX0A*VWNBA+VWNCA
C
C          THE THREE POTENTIALS
C
        COEF1P= VWNAP
        COEF1F= VWNAF
        COEF1A= VWNAA
        COEF2P=-VWNAP*VWNBP*VWNX0P/XX0P
        COEF2F=-VWNAF*VWNBF*VWNX0F/XX0F
        COEF2A=-VWNAA*VWNBA*VWNX0A/XX0A
        COEF3P= VWNAP*(TWO*VWNBP/QP)*((VWNCP-VWNX0P*VWNX0P)/XX0P)
        COEF3F= VWNAF*(TWO*VWNBF/QF)*((VWNCF-VWNX0F*VWNX0F)/XX0F)
        COEF3A= VWNAA*(TWO*VWNBA/QA)*((VWNCA-VWNX0A*VWNX0A)/XX0A)
C
        X     =FACR/RHO16
        XXP   =X*X+X*VWNBP+VWNCP
        XXF   =X*X+X*VWNBF+VWNCF
        XXA   =X*X+X*VWNBA+VWNCA
        T1P   =LOG(X*X/XXP)
        T1F   =LOG(X*X/XXF)
        T1A   =LOG(X*X/XXA)
        T2P   =LOG((X-VWNX0P)*(X-VWNX0P)/XXP)
        T2F   =LOG((X-VWNX0F)*(X-VWNX0F)/XXF)
        T2A   =LOG((X-VWNX0A)*(X-VWNX0A)/XXA)
        T3P   =ATAN(QP/(TWO*X+VWNBP))
        T3F   =ATAN(QF/(TWO*X+VWNBF))
        T3A   =ATAN(QA/(TWO*X+VWNBA))
C
        EPSCP =COEF1P*T1P+COEF2P*T2P+COEF3P*T3P
        EPSCF =COEF1F*T1F+COEF2F*T2F+COEF3F*T3F
        EPSCA =COEF1A*T1A+COEF2A*T2A+COEF3A*T3A
C
C          DERIVATIVE OF THE THREE POTENTIALS
C
        XBP    = TWO*X+VWNBP
        XBF    = TWO*X+VWNBF
        XBA    = TWO*X+VWNBA
        XBQ2P  = XBP*XBP + QP*QP
        XBQ2F  = XBF*XBF + QF*QF
        XBQ2A  = XBA*XBA + QA*QA
        BRACKP = TWO/(X-VWNX0P) -XBP/XXP -FOUR*(TWO*VWNX0P+VWNBP)/XBQ2P
        BRACKF = TWO/(X-VWNX0F) -XBF/XXF -FOUR*(TWO*VWNX0F+VWNBF)/XBQ2F
        BRACKA = TWO/(X-VWNX0A) -XBA/XXA -FOUR*(TWO*VWNX0A+VWNBA)/XBQ2A
        CURLYP = TWO/X - XBP/XXP - FOUR*VWNBP/XBQ2P
        CURLYF = TWO/X - XBF/XXF - FOUR*VWNBF/XBQ2F
        CURLYA = TWO/X - XBA/XXA - FOUR*VWNBA/XBQ2A
        DEPSCP = VWNAP*(CURLYP - VWNBP*VWNX0P*BRACKP/XX0P)
        DEPSCF = VWNAF*(CURLYF - VWNBF*VWNX0F*BRACKF/XX0F)
        DEPSCA = VWNAA*(CURLYA - VWNBA*VWNX0A*BRACKA/XX0A)
C
C          SOME AUXILIARY COMBINATIONS
C
        HX  = FACH*(EPSCF-EPSCP)/EPSCA - ONE
        DHX = FACH*(DEPSCF-DEPSCP-(EPSCF-EPSCP)*DEPSCA/EPSCA)/EPSCA
C
        ZETA  = (RHOA-RHOB)/RHOT
        ZETA2 = ZETA *ZETA
        ZETA3 = ZETA2*ZETA
        ZETA4 = ZETA2*ZETA2
        GZETA = ((ONE+ZETA)**FRTHRD+(ONE-ZETA)**FRTHRD-TWO)*FACG
        DGZETA= ((ONE+ZETA)**THIRD -(ONE-ZETA)**THIRD     )*FACDG
C
C          TOTAL POTENTIAL, AND CONTRIBUTION TO ENERGY
C
        POT   = EPSCP + EPSCA*GZETA*(ONE+HX*ZETA4)
        EVWN  = EVWN + FACVWN*POT*RHOT*FTOTWT
C
C          CONTRIBUTION TO FUNCTIONAL DERIVATIVE
C
        DPOT1 = -(X/SIX)*(DEPSCP + DEPSCA*GZETA*(ONE+HX*ZETA4)
     *                           +  EPSCA*GZETA*DHX*ZETA4)
        DPOT2 = EPSCA*(DGZETA*(ONE+HX*ZETA4) + FOUR*GZETA*HX*ZETA3)
        DUMA  = POT + (DPOT1 + DPOT2*(ONE-ZETA))
        DUMB  = POT + (DPOT1 - DPOT2*(ONE+ZETA))
C
        VXCA  = VXCA + FACVWN*DUMA
        VXCB  = VXCB + FACVWN*DUMB
      ENDIF
C
      RETURN
      END
