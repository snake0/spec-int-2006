C  4 NOV 03 - GNM - MPCDAT: ADDED PARAMETERS: LI (PM3); NA (AM1/PM3);
C                   MG (AM1); K (AM1/PM3); AND CA (AM1/PM3).
C 29 AUG 99 - CHC - MPCDAT: RENAME COMMON BLOCK /FIELD/ TO /FIELDG/
C  3 FEB 97 - MWS - MPCDAT: INITIALIZE UNDEFINED ARRAYS TO ZERO
C 13 JUN 96 - MWS - REMOVE SOME FTNCHEK WARNINGS
C  2 MAR 95 - MWS - INITIALIZE SOME HYDROGEN PARAMETERS TO ZERO
C 10 NOV 94 - MWS - REMOVE SOME FTNCHEK WARNINGS
C 28 APR 93 - MWS - SPLIT AN INITIALIZATION TO LESS THAN 19 LINES
C  9 MAR 92 - JHJ - NEW MODULE CONTAINING MOPAC PARAMETERIZATIONS
C
C         THE FOLLOWING STATEMENT APPEARED IN MOPAC 6.0 AND
C         IS REPRODUCED HERE TO COMPLY WITH SAID STATEMENT.
C
C         NOTICE OF PUBLIC DOMAIN NATURE OF THIS PROGRAM
C
C      'THIS COMPUTER PROGRAM IS A WORK OF THE UNITED STATES
C       GOVERNMENT AND AS SUCH IS NOT SUBJECT TO PROTECTION BY
C       COPYRIGHT (17 U.S.C. # 105.)  ANY PERSON WHO FRAUDULENTLY
C       PLACES A COPYRIGHT NOTICE OR DOES ANY OTHER ACT CONTRARY
C       TO THE PROVISIONS OF 17 U.S. CODE 506(C) SHALL BE SUBJECT
C       TO THE PENALTIES PROVIDED THEREIN.  THIS NOTICE SHALL NOT
C       BE ALTERED OR REMOVED FROM THIS SOFTWARE AND IS TO BE ON
C       ALL REPRODUCTIONS.'
C
C*MODULE MPCDAT  *DECK MPCDAT
      SUBROUTINE MPCDAT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /NATORB/ NATOR2(107)
C**********************************************************************
C
C     COMMON BLOCKS FOR AM1
C
C**********************************************************************
      COMMON /ELEMTS/ ELEMN2(107)
     1       /CORE  / CORE2(107)
     2       /MULTIP/ DD2(107),QQ2(107),AM2(107),AD2(107),AQ2(107)
     4       /ONELEC/ USS2(107),UPP2(107),UDD2(107)
     5       /BETAS / BETAS2(107),BETAP2(107),BETAD2(107)
     6       /TWOELE/ GSS2(107),GSP2(107),GPP2(107),GP22(107),
     7                HSP2(107),GSD2(107),GPD2(107),GDD2(107)
     8       /ATOMIC/ EISOL2(107),EHEAT2(107)
     9       /VSIPS / VS2(107),VP2(107),VD2(107)
     1       /ISTOPE/ AMS2(107)
     2       /IDEAA / GESA12(107,10),GESA22(107,10),GESA32(107,10)
     3       /IDEAP / GESP12(107,10),GESP22(107,10),GESP32(107,10)
     4       /GAUSS / FN12(107),FN22(107)
C**********************************************************************
C
C     COMMON BLOCKS FOR MNDO
C
C**********************************************************************
      COMMON /MNDO/  USSM2(107), UPPM2(107), UDDM2(107), ZSM2(107),
     1ZPM2(107), ZDM2(107), BETSM2(107), BETPM2(107), BETDM2(107),
     2ALPM2(107),ESOLM2(107),DDM2(107),QQM2(107),AMM2(107),ADM2(107),
     3AQM2(107),GSSM2(107),GSPM2(107),GPPM2(107),GP2M2(107),HSPM2(107),
     4PLVOM2(107)
C**********************************************************************
C
C     COMMON BLOCKS FOR PM3
C
C**********************************************************************
      COMMON /PM3 /  USSP32(107), UPPP32(107), UDDP32(107),ZSPM32(107),
     1ZPPM32(107), ZDPM32(107), BETSP2(107), BETPP2(107), BETDP2(107),
     2ALPP32(107), ESOLP2(107), DDPM32(107), QQPM32(107), AMPM32(107),
     3ADPM32(107), AQPM32(107) ,GSSP32(107), GSPP32(107), GPPP32(107),
     4GP2P32(107), HSPP32(107),PLVOP2(107)
C**********************************************************************
C
C     COMMON BLOCKS FOR AM1
C
C**********************************************************************
      COMMON /AM1BLO/USSA12(107), UPPA12(107), UDDA12(107),ZSAM12(107),
     1ZPAM12(107), ZDAM12(107), BETSA2(107), BETPA2(107), BETDA2(107),
     2ALPA12(107), ESOLA2(107), DDAM12(107), QQAM12(107), AMAM12(107),
     3ADAM12(107), AQAM12(107) ,GSSA12(107), GSPA12(107), GPPA12(107),
     4GP2A12(107), HSPA12(107),PLVOA2(107)
     5       /REFS/ REFMN2(107), REFM32(107), REFAM2(107), REFP32(107)
C**********************************************************************
C
C  COMMON BLOCKS FOR MINDO/3
C
C**********************************************************************
      COMMON /ONELE3 /  USS32(18),UPP32(18)
     1       /TWOEL3 /  F032(107)
     2       /ATOMI3 /  ESOL32(18),EHET32(18)
     3       /BETA3  /  BETA32(153)
     4       /ALPHA3 /  ALP32(153)
     5       /EXPON3 /  ZS32(18),ZP32(18)
C
C  END OF MINDO/3 COMMON BLOCKS
C
C
C ELECTRIC FIELD OPTIONS FOR POLARIZABILITY
C
      COMMON /FIELDG/ EFIELD(3)
      CHARACTER ELEMNT*2, REFMN*80, REFM3*80, REFAM*80, REFPM3*80
      CHARACTER ELEMN2*2, REFMN2*80, REFM32*80, REFAM2*80, REFP32*80
C
      DIMENSION  NATORB(107)
C**********************************************************************
C
C     DIMENSION BLOCKS FOR AM1
C
C**********************************************************************
      DIMENSION  ELEMNT(107),
     1        CORE(107),
     2        DD(107),QQ(107),AM(107),AD(107),
     4        USS(107),UPP(107),UDD(107),
     5        BETAS(107),BETAP(107),BETAD(107),
     6        GSS(107),GSP(107),GPP(107),GP2(107),
     8        EISOL(107),EHEAT(107),
     9        VS(107),VP(107),VD(107),
     1        AMS(107),
     2        GUESA1(107,10),GUESA2(107,10),GUESA3(107,10),
     3        GUESP1(107,10),GUESP2(107,10),GUESP3(107,10),
     4        FN1(107),FN2(107)
C**********************************************************************
C
C     DIMENSION BLOCKS FOR MNDO
C
C**********************************************************************
      DIMENSION   USSM(107), UPPM(107), UDDM(107), ZSM(107),
     1ZPM(107), ZDM(107), BETASM(107), BETAPM(107), BETADM(107),
     2ALPM(107), EISOLM(107), DDM(107), QQM(107), AMM(107), ADM(107),
     3AQM(107) ,GSSM(107), GSPM(107), GPPM(107), GP2M(107), HSPM(107),
     4POLVOM(107)
C**********************************************************************
C
C     DIMENSION BLOCKS FOR PM3
C
C**********************************************************************
      DIMENSION   USSPM3(107), UPPPM3(107), UDDPM3(107), ZSPM3(107),
     1ZPPM3(107), ZDPM3(107), BETASP(107), BETAPP(107), BETADP(107),
     2ALPPM3(107), EISOLP(107), DDPM3(107), QQPM3(107), AMPM3(107),
     3ADPM3(107), AQPM3(107) ,GSSPM3(107), GSPPM3(107), GPPPM3(107),
     4GP2PM3(107), HSPPM3(107),POLVOP(107)
C**********************************************************************
C
C     DIMENSION BLOCKS FOR AM1
C
C**********************************************************************
      DIMENSION USSAM1(107), UPPAM1(107), UDDAM1(107), ZSAM1(107),
     1ZPAM1(107), ZDAM1(107), BETASA(107), BETAPA(107), BETADA(107),
     2ALPAM1(107), EISOLA(107), DDAM1(107), QQAM1(107), AMAM1(107),
     3ADAM1(107), AQAM1(107) ,GSSAM1(107), GSPAM1(107), GPPAM1(107),
     4GP2AM1(107), HSPAM1(107),POLVOA(107),
     5        REFMN(107), REFM3(107), REFAM(107), REFPM3(107)
C**********************************************************************
C
C  DIMENSION BLOCKS FOR MINDO/3
C
C**********************************************************************
      DIMENSION   USS3(18),UPP3(18),
     1         F03(107),
     2         EISOL3(18),EHEAT3(18),
     3         BETA3(153),
     4         ALP3(153),
     5         ZS3(18),ZP3(18)
C
C  END OF MINDO/3 DIMENSION BLOCKS
C
C-MWS- THE FOLLOWING -AM1- VARIABLES SEEM TO BE UNUSED,
C-MWS- SO SET TO ZERO SO THEY ARE AT LEAST INITIALIZED.
C
      DATA DD/107*0.0D+00/
      DATA QQ/107*0.0D+00/
      DATA AM/107*0.0D+00/
      DATA AD/107*0.0D+00/
      DATA USS/107*0.0D+00/
      DATA UPP/107*0.0D+00/
      DATA UDD/107*0.0D+00/
      DATA BETAS/107*0.0D+00/
      DATA BETAP/107*0.0D+00/
      DATA BETAD/107*0.0D+00/
      DATA GSS/107*0.0D+00/
      DATA GSP/107*0.0D+00/
      DATA GPP/107*0.0D+00/
      DATA GP2/107*0.0D+00/
      DATA EISOL/107*0.0D+00/
      DATA VD/107*0.0D+00/
      DATA FN1/107*0.0D+00/
      DATA FN2/107*0.0D+00/
C
C-MWS- THE FOLLOWING -PM3- VARIABLES SEEM TO BE UNUSED,
C-MWS- SO SET TO ZERO SO THEY ARE AT LEAST INITIALIZED.
C
      DATA UDDPM3/107*0.0D+00/
      DATA BETADP/107*0.0D+00/
      DATA POLVOP/107*0.0D+00/
C
C-MWS- THE FOLLOWING -AM1- VARIABLES SEEM TO BE UNUSED,
C-MWS- SO SET TO ZERO SO THEY ARE AT LEAST INITIALIZED.
C
      DATA UDDAM1/107*0.0D+00/
      DATA BETADA/107*0.0D+00/
      DATA POLVOA/107*0.0D+00/
C
      DATA ELEMNT/' H','HE',
     1 'LI','BE',' B',' C',' N',' O',' F','NE',
     2 'NA','MG','AL','SI',' P',' S','CL','AR',
     3 ' K','CA','SC','TI',' V','CR','MN','FE','CO','NI','CU',
     4 'ZN','GA','GE','AS','SE','BR','KR',
     5 'RB','SR',' Y','ZR','NB','MO','TC','RU','RH','PD','AG',
     6 'CD','IN','SN','SB','TE',' I','XE',
     7 'CS','BA','LA','CE','PR','ND','PM','SM','EU','GD','TB','DY',
     8 'HO','ER','TM','YB','LU','HF','TA',' W','RE','OS','IR','PT',
     9 'AU','HG','TL','PB','BI','PO','AT','RN',
     1 'FR','RA','AC','TH','PA',' U','NP','PU','AM','CM','BK','CF','XX',
     2 'FM','MD','CB','++',' +','--',' -','TV'/
C
C   NATORB IS THE NUMBER OF ATOMIC ORBITALS PER ATOM.
C
      DATA NATORB/2*1, 8*4, 8*4, 2*4, 9*9, 7*4,
     12*4, 9*9, 7*4, 2*2, 14*8, 9*9, 7*4, 15*0,1,5*0/
C**********************************************************************
C                      VALENCE SHELLS ARE DEFINED AS                  *
C  PQN   VALENCE SHELLS                                               *
C                 P-GROUP              F-GROUP    TRANSITION METALS   *
C   1       1S                                                        *
C   2       2S 2P                                                     *
C   3       3S 3P  OR  3S 3P 3D                                       *
C   4       4S 4P                                    4S 4P 3D         *
C   5       5S 5P                                    5S 5P 4D         *
C   6       6S 6P                       6S 4F        6S 6P 5D         *
C   7  NOT ASSIGNED YET  ****DO  NOT  USE****                         *
C**********************************************************************
      DATA      POLVOM(1) /0.2287D+00/
      DATA      POLVOM(6) /0.2647D+00/
      DATA      POLVOM(7) /0.3584D+00/
      DATA      POLVOM(8) /0.2324D+00/
      DATA      POLVOM(9) /0.1982D+00/
      DATA      POLVOM(17)/1.3236D+00/
      DATA      POLVOM(35)/2.2583D+00/
      DATA      POLVOM(53)/4.0930D+00/
C
C                STANDARD ATOMIC MASSES
C
      DATA  (AMS(NUC),NUC=1,54) /
     *   1.00790D+00,  4.00260D+00,  6.94000D+00, 9.01218D+00,
     *  10.81000D+00, 12.01100D+00, 14.00670D+00, 15.99940D+00,
     *  18.99840D+00, 20.17900D+00, 22.98977D+00, 24.30500D+00,
     *  26.98154D+00, 28.08550D+00, 30.97376D+00, 32.06000D+00,
     *  35.45300D+00, 39.94800D+00, 39.09830D+00, 40.08000D+00,
     *  44.95590D+00, 47.90000D+00, 50.94150D+00, 51.99600D+00,
     *  54.93800D+00, 55.84700D+00, 58.93320D+00, 58.71000D+00,
     *  63.54600D+00, 65.38000D+00, 69.73500D+00, 72.59000D+00,
     *  74.92160D+00, 78.96000D+00, 79.90400D+00, 83.80000D+00,
     *  85.46780D+00, 87.62000D+00, 88.90590D+00, 91.22000D+00,
     *  92.90640D+00, 95.94000D+00, 98.90620D+00, 101.0700D+00,
     * 102.9055D+00, 106.4000D+00, 107.8680D+00, 112.4100D+00,
     * 114.8200D+00, 118.6900D+00, 121.7500D+00, 127.6000D+00,
     * 126.9045D+00, 131.3000D+00/
      DATA  (AMS(NUC),NUC=55,107) /
     2 132.9054D+00, 137.3300D+00, 15*0.000D+00, 178.4900D+00,
     * 180.9479D+00,
     3 183.8500D+00, 186.2070D+00, 190.2000D+00, 192.2200D+00,
     * 195.0900D+00,
     4 196.9665D+00, 200.5900D+00, 204.3700D+00, 207.2000D+00,
     * 208.9804D+00,
     5 18*0.000D+00,   1.0079D+00,  5*0.000D+00/
C
C   CORE IS THE CHARGE ON THE ATOM AS SEEN BY THE ELECTRONS
C
      DATA CORE/1.0D+00,0.0D+00,
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
C     ENTHALPIES OF FORMATION OF GASEOUS ATOMS ARE TAKEN FROM \ANNUAL
C     REPORTS,1974,71B,P 117\  THERE ARE SOME SIGNIFICANT DIFFERENCES
C     BETWEEN THE VALUES REPORTED THERE AND THE VALUES PREVIOUSLY IN
C     THE BLOCK DATA OF THIS PROGRAM.  ONLY THE THIRD  ROW ELEMENTS
C     HAVE BEEN UPDATED.
C
C ALL THE OTHER ELEMENTS ARE TAKEN FROM CRC HANDBOOK 1981-1982
      DATA EHEAT(1)  / 52.102D+00/
      DATA EHEAT(2)  /  0.000D+00/
C
      DATA EHEAT(3)  / 38.410D+00/
      DATA EHEAT(4)  / 76.960D+00/
      DATA EHEAT(5)  /135.700D+00/
      DATA EHEAT(6)  /170.890D+00/
      DATA EHEAT(7)  /113.000D+00/
      DATA EHEAT(8)  / 59.559D+00/
      DATA EHEAT(9)  / 18.890D+00/
      DATA EHEAT(10) /  0.000D+00/
C
      DATA EHEAT(11) / 25.850D+00/
      DATA EHEAT(12) / 35.000D+00/
      DATA EHEAT(13) / 79.490D+00/
      DATA EHEAT(14) /108.390D+00/
      DATA EHEAT(15) / 75.570D+00/
      DATA EHEAT(16) / 66.400D+00/
      DATA EHEAT(17) / 28.990D+00/
      DATA EHEAT(18) /  0.000D+00/
C
C GNM, NEW VALUE FOR K'S HEAT OF FORMATION. OLD VALUE = 21.42 KCAL/MOL.
      DATA EHEAT(19) / 21.270D+00/
      DATA EHEAT(20) / 42.600D+00/
      DATA EHEAT(21) / 90.300D+00/
      DATA EHEAT(22) /112.300D+00/
      DATA EHEAT(23) /122.900D+00/
      DATA EHEAT(24) / 95.000D+00/
      DATA EHEAT(25) / 67.700D+00/
      DATA EHEAT(26) / 99.300D+00/
      DATA EHEAT(27) /102.400D+00/
      DATA EHEAT(28) /102.800D+00/
      DATA EHEAT(29) / 80.700D+00/
      DATA EHEAT(30) / 31.170D+00/
      DATA EHEAT(31) / 65.400D+00/
      DATA EHEAT(32) / 89.500D+00/
      DATA EHEAT(33) / 72.300D+00/
      DATA EHEAT(34) / 54.300D+00/
      DATA EHEAT(35) / 26.740D+00/
      DATA EHEAT(36) /  0.000D+00/
C
      DATA EHEAT(37) / 19.600D+00/
      DATA EHEAT(38) / 39.100D+00/
      DATA EHEAT(39) /101.500D+00/
      DATA EHEAT(40) /145.500D+00/
      DATA EHEAT(41) /172.400D+00/
      DATA EHEAT(42) /157.300D+00/
      DATA EHEAT(44) /155.500D+00/
      DATA EHEAT(45) /133.000D+00/
      DATA EHEAT(46) / 90.000D+00/
      DATA EHEAT(47) / 68.100D+00/
      DATA EHEAT(48) / 26.720D+00/
      DATA EHEAT(49) / 58.000D+00/
      DATA EHEAT(50) / 72.200D+00/
      DATA EHEAT(51) / 63.200D+00/
      DATA EHEAT(52) / 47.000D+00/
      DATA EHEAT(53) / 25.517D+00/
      DATA EHEAT(54) /  0.000D+00/
C
      DATA EHEAT(55) / 18.700D+00/
      DATA EHEAT(56) / 42.500D+00/
      DATA EHEAT(58) /101.300D+00/
      DATA EHEAT(62) / 49.400D+00/
      DATA EHEAT(68) / 75.800D+00/
      DATA EHEAT(70) / 36.350D+00/
      DATA EHEAT(72) /148.000D+00/
      DATA EHEAT(73) /186.900D+00/
      DATA EHEAT(74) /203.100D+00/
      DATA EHEAT(75) /185.000D+00/
      DATA EHEAT(76) /188.000D+00/
      DATA EHEAT(77) /160.000D+00/
      DATA EHEAT(78) /135.200D+00/
      DATA EHEAT(79) / 88.000D+00/
      DATA EHEAT(80) / 14.690D+00/
      DATA EHEAT(81) / 43.550D+00/
      DATA EHEAT(82) / 46.620D+00/
      DATA EHEAT(83) / 50.100D+00/
      DATA EHEAT(86) /  0.000D+00/
      DATA EHEAT(102)  / 207.0D+00/
C
      DATA VS(1) /  -13.605  /
      DATA VS(5)/-15.16D+00/
      DATA VS(6)/-21.34D+00/
      DATA VS(7)/-27.51D+00/
      DATA VS(8)/-35.30D+00/
      DATA VS(9)/-43.70D+00/
      DATA VS(14)/-17.82D+00/
      DATA VS(15)/-21.10D+00/
      DATA VS(16)/-23.84D+00/
      DATA VS(17)/-25.26D+00/
      DATA VP(1)  /  0.0D+00  /
      DATA VP(5)/-8.52D+00/
      DATA VP(6)/-11.54D+00/
      DATA VP(7)/-14.34D+00/
      DATA VP(8)/-17.91D+00/
      DATA VP(9)/-20.89D+00/
      DATA VP(14)/-8.51D+00/
      DATA VP(15)/-10.29D+00/
      DATA VP(16)/-12.41D+00/
      DATA VP(17)/-15.09D+00/
C      DATA NPQ/1,1, 2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3, 4,4,4,4,4,4,4,4,
C     +4,4,4,4,4,4,4,4,4,4, 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5/
C
C *** ONE CENTER REPULSION INTEGRALS
C     GSS ::= (SS,SS)
C     GPP ::= (PP,PP)
C     GSP ::= (SS,PP)
C     GP2 ::= (PP,P*P*)
C     HSP ::= (SP,SP)
C***********************************************************************
      DATA GSSM(1) / 12.848D+00 /
      DATA GSSM(4)/9.00D+00/
      DATA GSSM(5)/10.59D+00/
      DATA GSSM(6) / 12.23D+00 /
      DATA GSSM(7)/13.59D+00/
      DATA GSSM(8)/15.42D+00/
      DATA GSSM(9)/16.92D+00/
      DATA GSSM(13)/8.09D+00/
      DATA GSSM(14)/9.82D+00/
      DATA GSSM(15)/11.56D+00/
      DATA GSSM(16)/12.88D+00/
      DATA GSSM(17)/15.03D+00/
      DATA GSSM(35)/15.03643948D+00/
      DATA GSSM(53)/15.04044855D+00/
      DATA GPPM(4)/6.97D+00/
      DATA GPPM(5)/8.86D+00/
      DATA GPPM(6) / 11.08D+00 /
      DATA GPPM(7)/12.98D+00/
      DATA GPPM(8)/14.52D+00/
      DATA GPPM(9)/16.71D+00/
      DATA GPPM(13)/5.98D+00/
      DATA GPPM(14)/7.31D+00/
      DATA GPPM(15)/8.64D+00/
      DATA GPPM(16)/9.90D+00/
      DATA GPPM(17)/11.30D+00/
      DATA GPPM(35)/11.27632539D+00/
      DATA GPPM(53)/11.14778369D+00/
      DATA GSPM(4)/7.43D+00/
      DATA GSPM(5)/9.56D+00/
      DATA GSPM(6) / 11.47D+00 /
      DATA GSPM(7)/12.66D+00/
      DATA GSPM(8)/14.48D+00/
      DATA GSPM(9)/17.25D+00/
      DATA GSPM(13)/6.63D+00/
      DATA GSPM(14)/8.36D+00/
      DATA GSPM(15)/10.08D+00/
      DATA GSPM(16)/11.26D+00/
      DATA GSPM(17)/13.16D+00/
      DATA GSPM(35)/13.03468242D+00/
      DATA GSPM(53)/13.05655798D+00/
      DATA GP2M(4)/6.22D+00/
      DATA GP2M(5)/7.86D+00/
      DATA GP2M(6) / 9.84D+00 /
      DATA GP2M(7)/11.59D+00/
      DATA GP2M(8)/12.98D+00/
      DATA GP2M(9)/14.91D+00/
      DATA GP2M(13)/5.40D+00/
      DATA GP2M(14)/6.54D+00/
      DATA GP2M(15)/7.68D+00/
      DATA GP2M(16)/8.83D+00/
      DATA GP2M(17)/9.97D+00/
      DATA GP2M(35)/9.85442552D+00/
      DATA GP2M(53)/9.91409071D+00/
      DATA HSPM(4)/1.28D+00/
      DATA HSPM(5)/1.81D+00/
      DATA HSPM(6) / 2.43D+00 /
      DATA HSPM(7)/3.14D+00/
      DATA HSPM(8)/3.94D+00/
      DATA HSPM(9)/4.83D+00/
      DATA HSPM(13)/0.70D+00/
      DATA HSPM(14)/1.32D+00/
      DATA HSPM(15)/1.92D+00/
      DATA HSPM(16)/2.26D+00/
      DATA HSPM(17)/2.42D+00/
      DATA HSPM(35)/2.45586832D+00/
      DATA HSPM(53)/2.45638202D+00/
C
C     THE MONOCENTRIC INTEGRALS HSP AND GSP FOR ALUMINIUM ARE ONLY
C     ESTIMATES. A VALUE OF G1 FOR AL IS NEEDED TO RESOLVE OLEARIS
C     INTEGRALS.
C
C     OPTIMIZED MNDO PARAMETERS FOR H, BE, B, C, N, O, F
C                                                     CL
C     ESTIMATED MNDO PARAMETERS FOR       AL,SI, P, S
C
C     ELEMENTS H, C, N, O WERE PARAMETERIZED BY WALTER THIEL
C     ELEMENTS B,SI,P,S   WERE      ..          MICHAEL MCKEE
C     ELEMENTS BE,F,AL,CL WERE      ..          HENRY RZEPA
C
C**********************************************************************
C
C    START OF MINDO/3 PARAMETERS
C
C**********************************************************************
C *** F03 IS THE ONE CENTER AVERAGED REPULSION INTEGRAL FOR USE IN THE
C        TWO CENTER ELECTRONIC REPULSION INTEGRAL EVALUATION.
      DATA REFM3  ( 1)/'  H: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  ( 5)/'  B: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  ( 6)/'  C: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  ( 7)/'  N: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  ( 8)/'  O: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  ( 9)/'  F: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  (14)/' SI: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  (15)/'  P: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  (16)/'  S: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA REFM3  (17)/' CL: (MINDO/3): R.C.BINGHAM ET.AL., J.AM.CHEM.SO
     1C. 97,1285,1294,1302,1307 (1975)'/
      DATA F03              /  12.848D+00, 10.0D+00, 10.0D+00, 0.0D+00,
     1  8.958D+00, 10.833D+00, 12.377D+00, 13.985D+00, 16.250D+00,
     2         10.000D+00, 10.000D+00, 0.000D+00, 0.000D+00,7.57D+00 ,
     *  9.00D+00 ,
     3   10.20D+00 , 11.73,10.0D+00,35*0.0D+00,10.0D+00,53*10.0D+00/
C *** USS AND UPP ARE THE ONE-CENTER CORE ELECTRON ATTRACTION AND KINETI
C     ENERGY INTEGRALS FOR S AND P ELECTRONS RESPECTIVELY IN E.V.
      DATA USS3   / -12.505D+00, 0.000D+00, 0.000D+00, 0.000D+00,
     1              -33.61D+00, -51.79D+00, -66.06D+00, -91.73D+00 ,
     2             -129.86D+00,
     3                0.0000D+00 , 0.000 D+00 ,0.000D+00 , 0.000D+00 ,
     4       -39.82D+00 , -56.23D+00 , -73.39D+00 , -98.99D+00 ,.0D+00/
      DATA UPP3   /   0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00,
     1 -25.11D+00 , -39.18D+00 , -56.40D+00 , -78.80D+00 , -105.93D+00 ,
     2                   0.000D+00 , 0.000D+00 , 0.000D+00 , 0.000D+00 ,
     3       -29.15D+00 , -42.31D+00 , -57.25D+00 , -76.43D+00 ,.0D+00/
C *** EISOL3 AND EHEAT3 ARE THE GS ELECTRONIC ENERGY OF THE NEUTRAL ATOM
C     (IN E.V.) AND THE HEAT OF FORMATION IF THE FREE ATOM (IN KCAL/MOL)
      DATA EISOL3  /-12.505D+00 , 0.0D+00 , 0.0D+00 ,0.0D+00 ,
     1 -61.70D+00,-119.47D+00,-187.51D+00, -307.07D+00 , -475.00D+00 ,
     2                         0.0D+00 , 0.0D+00 , 0.0D+00 , 0.0D+00 ,
     3 -90.98D+00 , -150.81D+00 , -229.15D+00 , -345.93D+00 , 0.0D+00/
      DATA EHEAT3  / 52.102D+00 , 0.0D+00 , 0.0D+00 , 0.0D+00 ,
     1 135.7 D+00,170.89D+00, 113.0D+00 , 59.559D+00 ,  18.86D+00 ,
     2                       0.0D+00 , 0.0D+00 , 0.0D+00 , 0.0D+00 ,
     3     106.0D+00 ,   79.8D+00 ,  65.65D+00 ,  28.95D+00 , 0.0D+00 /
C *** BETA3 AND ALP3 ARE THE BOND PARAMETERS USED IN THE
C     RESONANCE INTEGRAL AND THE CORE CORE REPULSION INTEGRAL RESPECTIVE
C     THAT IS ACCORDING TO THE FOLLOWING CONVENTION
C
C     HERE IS THE
C     BOND TYPE DESIGNATION
C
C
C         H   B   C   N   O   F  SI   P   S  CL
C       -----------------------------------------
C      H  1  11  16  22  29  37  92 106 121 137
C      B     15  20  26  33  41
C      C         21  27  34  42  97 111 126 142
C      N             28  35  43         127 143
C      O                 36  44     113 128
C      F                     45     114
C     SI                        105
C      P                            120     151
C      S                                136 152
C     CL                                    153
      DATA BETA3(1),ALP3(1)   /  0.244770D+00 ,  1.489450D+00 /
      DATA BETA3(11),ALP3(11)   /  0.185347D+00 ,  2.090352D+00 /
      DATA BETA3(15),ALP3(15)   /  0.151324D+00 ,  2.280544D+00 /
      DATA BETA3(16),ALP3(16)   /  0.315011D+00 ,  1.475836D+00 /
      DATA BETA3(20),ALP3(20)   /  0.250031D+00 ,  2.138291D+00 /
      DATA BETA3(21),ALP3(21)   /  0.419907D+00 ,  1.371208D+00 /
      DATA BETA3(22),ALP3(22)   /  0.360776D+00 ,  0.589380D+00 /
      DATA BETA3(26),ALP3(26)   /  0.310959D+00 ,  1.909763D+00 /
      DATA BETA3(27),ALP3(27)   /  0.410886D+00 ,  1.635259D+00 /
      DATA BETA3(28),ALP3(28) /  0.377342D+00 ,  2.029618D+00 /
      DATA BETA3(29),ALP3(29) /  0.417759D+00 ,  0.478901D+00 /
      DATA BETA3(33),ALP3(33) /  0.349745D+00 ,  2.484827D+00 /
      DATA BETA3(34),ALP3(34) /  0.464514D+00 ,  1.820975D+00 /
      DATA BETA3(35),ALP3(35) /  0.458110D+00 ,  1.873859D+00 /
      DATA BETA3(36),ALP3(36) /  0.659407D+00 ,  1.537190D+00 /
      DATA BETA3(37),ALP3(37) /  0.195242D+00 ,  3.771362D+00 /
      DATA BETA3(41),ALP3(41) /  0.219591D+00 ,  2.862183D+00 /
      DATA BETA3(42),ALP3(42) /  0.247494D+00 ,  2.725913D+00 /
      DATA BETA3(43),ALP3(43) /  0.205347D+00 ,  2.861667D+00 /
      DATA BETA3(44),ALP3(44) /  0.334044D+00 ,  2.266949D+00 /
      DATA BETA3(45),ALP3(45) /  0.197464D+00 ,  3.864997D+00 /
      DATA BETA3(92),ALP3(92) /  0.289647D+00 ,  0.940789D+00 /
      DATA BETA3(97),ALP3(97) /  0.411377D+00 ,  1.101382D+00 /
      DATA BETA3(105),ALP3(105) /  0.291703D+00 ,  0.918432D+00 /
      DATA BETA3(106),ALP3(106) /  0.320118D+00 ,  0.923170D+00 /
      DATA BETA3(111),ALP3(111) /  0.457816D+00 ,  1.029693D+00 /
      DATA BETA3(113),ALP3(113) /  0.470000D+00 ,  1.662500D+00 /
      DATA BETA3(114),ALP3(114) /  0.300000D+00 ,  1.750000D+00 /
      DATA BETA3(120),ALP3(120) /  0.311790D+00 ,  1.186652D+00 /
      DATA BETA3(121),ALP3(121) /  0.220654D+00 ,  1.700698D+00 /
      DATA BETA3(126),ALP3(126) /  0.284620D+00 ,  1.761370D+00 /
      DATA BETA3(127),ALP3(127) /  0.313170D+00 ,  1.878176D+00/
      DATA BETA3(128),ALP3(128) /  0.422890D+00 ,  2.077240D+00 /
      DATA BETA3(136),ALP3(136) /  0.202489D+00 ,  1.751617D+00 /
      DATA BETA3(137),ALP3(137) /  0.231653D+00 ,  2.089404D+00 /
      DATA BETA3(142),ALP3(142) /  0.315480D+00 ,  1.676222D+00 /
      DATA BETA3(143),ALP3(143) /  0.302298D+00 ,  1.817064D+00 /
      DATA BETA3(151),ALP3(151) /  0.277322D+00 ,  1.543720D+00 /
      DATA BETA3(152),ALP3(152) /  0.221764D+00 ,  1.950318D+00 /
      DATA BETA3(153),ALP3(153) /  0.258969D+00 ,  1.792125D+00 /
C *** HERE COMES THE OPTIMIZED SLATER_S EXPONENTS FOR THE EVALUATION
C     OF THE OVERLAP INTEGRALS AND MOLECULAR DIPOLE MOMENTS.
      DATA ZS3(1),ZP3(1)      /  1.3D+00       ,  0.0D+00      /
      DATA ZS3(5),ZP3(5)      /  1.211156D+00 ,  0.972826D+00 /
      DATA ZS3(6),ZP3(6)      /  1.739391D+00 ,  1.709645D+00 /
      DATA ZS3(7),ZP3(7)      /  2.704546D+00 ,  1.870839D+00 /
      DATA ZS3(8),ZP3(8)      /  3.640575D+00 ,  2.168448D+00 /
      DATA ZS3(9),ZP3(9)      /  3.111270D+00 ,  1.41986D+00 /
      DATA ZS3(14),ZP3(14)    /  1.629173D+00 ,  1.381721D+00 /
      DATA ZS3(15),ZP3(15)    /  1.926108D+00 ,  1.590665D+00 /
      DATA ZS3(16),ZP3(16)    /  1.719480D+00 ,  1.403205D+00 /
      DATA ZS3(17),ZP3(17)    /  3.430887D+00 ,  1.627017D+00 /
C************************************************************
C                                                           *
C               DATA FOR THE SPARKLES                       *
C                                                           *
C************************************************************
C                               DATA FOR THE " ++ " SPARKLE
      DATA EHEAT(103)    / 0.0D+00/
      DATA VS(103)       /10.0D+00/
C
C  START OF MNDO
C
      DATA ALPM(103)     / 1.5D+00/
      DATA EISOLM(103)   / 0.0D+00/
      DATA AMM(103)      / 0.5D+00/
C
C  START OF AM1
C
      DATA ALPAM1(103)   / 1.5D+00/
      DATA EISOLA(103)   / 0.0D+00/
      DATA AMAM1(103)    / 0.5D+00/
C
C  START OF PM3
C
      DATA ALPPM3(103)   / 1.5D+00/
      DATA EISOLP(103)   / 0.0D+00/
      DATA AMPM3(103)    / 0.5D+00/
C                               DATA FOR THE " + " SPARKLE
      DATA EHEAT(104)    / 0.0D+00/
      DATA VS(104)       /10.0D+00/
      DATA ALPAM1(104)   / 1.5D+00/
      DATA EISOLA(104)   / 0.0D+00/
      DATA AMAM1(104)    / 0.5D+00/
      DATA ALPM(104)     / 1.5D+00/
      DATA EISOLM(104)   / 0.0D+00/
      DATA AMM(104)      / 0.5D+00/
      DATA ALPPM3(104)   / 1.5D+00/
      DATA EISOLP(104)   / 0.0D+00/
      DATA AMPM3(104)    / 0.5D+00/
C                               DATA FOR THE " -- " SPARKLE
      DATA EHEAT(105)    / 0.0D+00/
      DATA VS(105)       /10.0D+00/
      DATA ALPAM1(105)   / 1.5D+00/
      DATA EISOLA(105)   / 0.0D+00/
      DATA AMAM1(105)    / 0.5D+00/
      DATA ALPM(105)     / 1.5D+00/
      DATA EISOLM(105)   / 0.0D+00/
      DATA AMM(105)      / 0.5D+00/
      DATA ALPPM3(105)   / 1.5D+00/
      DATA EISOLP(105)   / 0.0D+00/
      DATA AMPM3(105)    / 0.5D+00/
C                               DATA FOR THE " - " SPARKLE
      DATA EHEAT(106)    / 0.0D+00/
      DATA VS(106)       /10.0D+00/
      DATA ALPAM1(106)   / 1.5D+00/
      DATA EISOLA(106)   / 0.0D+00/
      DATA AMAM1(106)    / 0.5D+00/
      DATA ALPM(106)     / 1.5D+00/
      DATA EISOLM(106)   / 0.0D+00/
      DATA AMM(106)      / 0.5D+00/
      DATA ALPPM3(106)   / 1.5D+00/
      DATA EISOLP(106)   / 0.0D+00/
      DATA AMPM3(106)    / 0.5D+00/
C**********************************************************************
C
C    START OF MNDO PARAMETERS
C
C**********************************************************************
C                    DATA FOR ELEMENT  1        HYDROGEN
      DATA REFMN  ( 1)/'  H: (MNDO):  M.J.S. DEWAR, W. THIEL, J. AM. CHE
     1M. SOC., 99, 4899, (1977)       '/
      DATA USSM   ( 1)/     -11.9062760D+00/
      DATA UPPM   ( 1)/0.0D+00/
      DATA BETASM ( 1)/      -6.9890640D+00/
      DATA BETAPM ( 1)/0.0D+00/
      DATA ZSM    ( 1)/       1.3319670D+00/
      DATA ZPM    ( 1)/0.0D+00/
      DATA ALPM   ( 1)/       2.5441341D+00/
      DATA EISOLM ( 1)/     -11.9062760D+00/
      DATA AMM    ( 1)/       0.4721793D+00/
      DATA ADM    ( 1)/       0.4721793D+00/
      DATA AQM    ( 1)/       0.4721793D+00/
C                    DATA FOR ELEMENT  3        LITHIUM
      DATA REFMN  ( 3)/' LI: (MNDO):  TAKEN FROM MNDOC BY W.THIEL,
     1QCPE NO.438, V. 2, P.63, (1982).'/
      DATA USSM   (  3)/      -5.1280000D+00/
      DATA UPPM   (  3)/      -2.7212000D+00/
      DATA BETASM (  3)/      -1.3500400D+00/
      DATA BETAPM (  3)/      -1.3500400D+00/
      DATA ZSM    (  3)/       0.7023800D+00/
      DATA ZPM    (  3)/       0.7023800D+00/
      DATA ALPM   (  3)/       1.2501400D+00/
      DATA EISOLM (  3)/      -5.1280000D+00/
      DATA GSSM   (  3)/       7.3000000D+00/
      DATA GSPM   (  3)/       5.4200000D+00/
      DATA GPPM   (  3)/       5.0000000D+00/
      DATA GP2M   (  3)/       4.5200000D+00/
      DATA HSPM   (  3)/       0.8300000D+00/
      DATA DDM    (  3)/       2.0549783D+00/
      DATA QQM    (  3)/       1.7437069D+00/
      DATA AMM    (  3)/       0.2682837D+00/
      DATA ADM    (  3)/       0.2269793D+00/
      DATA AQM    (  3)/       0.2614581D+00/
C                    DATA FOR ELEMENT  4        BERYLLIUM
      DATA REFMN  ( 4)/' BE: (MNDO):  M.J.S. DEWAR, H.S. RZEPA, J. AM. C
     1HEM. SOC., 100, 777, (1978)     '/
      DATA USSM   ( 4)/     -16.6023780D+00/
      DATA UPPM   ( 4)/     -10.7037710D+00/
      DATA BETASM ( 4)/      -4.0170960D+00/
      DATA BETAPM ( 4)/      -4.0170960D+00/
      DATA ZSM    ( 4)/       1.0042100D+00/
      DATA ZPM    ( 4)/       1.0042100D+00/
      DATA ALPM   ( 4)/       1.6694340D+00/
      DATA EISOLM ( 4)/     -24.2047560D+00/
      DATA DDM    ( 4)/       1.4373245D+00/
      DATA QQM    ( 4)/       1.2196103D+00/
      DATA AMM    ( 4)/       0.3307607D+00/
      DATA ADM    ( 4)/       0.3356142D+00/
      DATA AQM    ( 4)/       0.3846373D+00/
C                    DATA FOR ELEMENT  5        BORON
      DATA REFMN  ( 5)/'  B: (MNDO):  M.J.S. DEWAR, M.L. MCKEE, J. AM. C
     1HEM. SOC., 99, 5231, (1977)     '/
      DATA USSM   ( 5)/     -34.5471300D+00/
      DATA UPPM   ( 5)/     -23.1216900D+00/
      DATA BETASM ( 5)/      -8.2520540D+00/
      DATA BETAPM ( 5)/      -8.2520540D+00/
      DATA ZSM    ( 5)/       1.5068010D+00/
      DATA ZPM    ( 5)/       1.5068010D+00/
      DATA ALPM   ( 5)/       2.1349930D+00/
      DATA EISOLM ( 5)/     -64.3159500D+00/
      DATA DDM    ( 5)/       0.9579073D+00/
      DATA QQM    ( 5)/       0.8128113D+00/
      DATA AMM    ( 5)/       0.3891951D+00/
      DATA ADM    ( 5)/       0.4904730D+00/
      DATA AQM    ( 5)/       0.5556979D+00/
C                    DATA FOR ELEMENT  6        CARBON
      DATA REFMN  ( 6)/'  C: (MNDO):  M.J.S. DEWAR, W. THIEL, J. AM. CHE
     1M. SOC., 99, 4899, (1977)       '/
      DATA USSM   ( 6)/     -52.2797450D+00/
      DATA UPPM   ( 6)/     -39.2055580D+00/
      DATA BETASM ( 6)/     -18.9850440D+00/
      DATA BETAPM ( 6)/      -7.9341220D+00/
      DATA ZSM    ( 6)/       1.7875370D+00/
      DATA ZPM    ( 6)/       1.7875370D+00/
      DATA ALPM   ( 6)/       2.5463800D+00/
      DATA EISOLM ( 6)/    -120.5006060D+00/
      DATA DDM    ( 6)/       0.8074662D+00/
      DATA QQM    ( 6)/       0.6851578D+00/
      DATA AMM    ( 6)/       0.4494671D+00/
      DATA ADM    ( 6)/       0.6149474D+00/
      DATA AQM    ( 6)/       0.6685897D+00/
C                    DATA FOR ELEMENT  7        NITROGEN
      DATA REFMN  ( 7)/'  N: (MNDO):  M.J.S. DEWAR, W. THIEL, J. AM. CHE
     1M. SOC., 99, 4899, (1977)       '/
      DATA USSM   ( 7)/     -71.9321220D+00/
      DATA UPPM   ( 7)/     -57.1723190D+00/
      DATA BETASM ( 7)/     -20.4957580D+00/
      DATA BETAPM ( 7)/     -20.4957580D+00/
      DATA ZSM    ( 7)/       2.2556140D+00/
      DATA ZPM    ( 7)/       2.2556140D+00/
      DATA ALPM   ( 7)/       2.8613420D+00/
      DATA EISOLM ( 7)/    -202.5662010D+00/
      DATA DDM    ( 7)/       0.6399037D+00/
      DATA QQM    ( 7)/       0.5429763D+00/
      DATA AMM    ( 7)/       0.4994487D+00/
      DATA ADM    ( 7)/       0.7843643D+00/
      DATA AQM    ( 7)/       0.81264450D+00/
C                    DATA FOR ELEMENT  8        OXYGEN
      DATA REFMN  ( 8)/'  O: (MNDO):  M.J.S. DEWAR, W. THIEL, J. AM. CHE
     1M. SOC., 99, 4899, (1977)       '/
      DATA USSM   ( 8)/     -99.6443090D+00/
      DATA UPPM   ( 8)/     -77.7974720D+00/
      DATA BETASM ( 8)/     -32.6880820D+00/
      DATA BETAPM ( 8)/     -32.6880820D+00/
      DATA ZSM    ( 8)/       2.6999050D+00/
      DATA ZPM    ( 8)/       2.6999050D+00/
      DATA ALPM   ( 8)/       3.1606040D+00/
      DATA EISOLM ( 8)/    -317.8685060D+00/
      DATA DDM    ( 8)/       0.5346024D+00/
      DATA QQM    ( 8)/       0.4536252D+00/
      DATA AMM    ( 8)/       0.5667034D+00/
      DATA ADM    ( 8)/       0.9592562D+00/
      DATA AQM    ( 8)/       0.9495934D+00/
C                    DATA FOR ELEMENT  9        FLUORINE
      DATA REFMN  ( 9)/'  F: (MNDO):  M.J.S. DEWAR, H.S. RZEPA, J. AM. C
     1HEM. SOC., 100, 777, (1978)     '/
      DATA USSM   ( 9)/    -131.0715480D+00/
      DATA UPPM   ( 9)/    -105.7821370D+00/
      DATA BETASM ( 9)/     -48.2904660D+00/
      DATA BETAPM ( 9)/     -36.5085400D+00/
      DATA ZSM    ( 9)/       2.8484870D+00/
      DATA ZPM    ( 9)/       2.8484870D+00/
      DATA ALPM   ( 9)/       3.4196606D+00/
      DATA EISOLM ( 9)/    -476.6837810D+00/
      DATA DDM    ( 9)/       0.5067166D+00/
      DATA QQM    ( 9)/       0.4299633D+00/
      DATA AMM    ( 9)/       0.6218302D+00/
      DATA ADM    ( 9)/       1.0850301D+00/
      DATA AQM    ( 9)/       1.0343643D+00/
C
C AM1 AND PM3 VARIABLES COMMENTED OUT.
C                               DATA FOR THE SODIUM-LIKE SPARKLE
      DATA REFMN  (11)/' NA: (MNDO):  SODIUM-LIKE SPARKLE.   USE WITH CA
     1RE.                             '/
C     DATA REFAM  (11)/' NA: (AM1):   SODIUM-LIKE SPARKLE.   USE WITH CA
C     1RE.                             '/
C     DATA REFPM3 (11)/' NA: (PM3):   SODIUM-LIKE SPARKLE.   USE WITH CA
C     1RE.                             '/
      DATA VS(11)       /10.0D+00/
C      DATA ALPAM1(11)      / 1.668D+00/
      DATA ALPM(11)        / 1.660D+00/
C      DATA ALPPM3(11)      / 1.681D+00/
C
C      DATA EISOLA(11)      / 0.0D+00/
      DATA EISOLM(11)      / 0.0D+00/
C      DATA EISOLP(11)      / 0.0D+00/
C
C      DATA AMAM1(11)       / 0.5D+00/
      DATA AMM(11)         / 0.5D+00/
C      DATA AMPM3(11)       / 0.5D+00/
C                    DATA FOR ELEMENT 13        ALUMINUM
      DATA REFMN  (13)/' AL: (MNDO):  L.P. DAVIS, ET.AL.  J. COMP. CHEM.
     1, 2, 433, (1981) SEE MANUAL.    '/
      DATA USSM   (13)/     -23.8070970D+00/
      DATA UPPM   (13)/     -17.5198780D+00/
      DATA BETASM (13)/      -2.6702840D+00/
      DATA BETAPM (13)/      -2.6702840D+00/
      DATA ZSM    (13)/       1.4441610D+00/
      DATA ZPM    (13)/       1.4441610D+00/
      DATA ZDM    (13)/       1.0000000D+00/
      DATA ALPM   (13)/       1.8688394D+00/
      DATA EISOLM (13)/     -44.4840720D+00/
      DATA DDM    (13)/       1.3992387D+00/
      DATA QQM    (13)/       1.1586797D+00/
      DATA AMM    (13)/       0.2973172D+00/
      DATA ADM    (13)/       0.2635574D+00/
      DATA AQM    (13)/       0.3673560D+00/
C                    DATA FOR ELEMENT 14          SILICON
      DATA REFMN  (14)/' SI: (MNDO): M.J.S.DEWAR, ET. AL. ORGANOMETALLIC
     1S  5, 375 (1986)                '/
      DATA USSM   (14)/     -37.0375330D+00/
      DATA UPPM   (14)/     -27.7696780D+00/
      DATA BETASM (14)/      -9.0868040D+00/
      DATA BETAPM (14)/      -1.0758270D+00/
      DATA ZSM    (14)/       1.3159860D+00/
      DATA ZPM    (14)/       1.7099430D+00/
      DATA ZDM    (14)/       1.0000000D+00/
      DATA ALPM   (14)/       2.2053160D+00/
      DATA EISOLM (14)/     -82.8394220D+00/
      DATA DDM    (14)/       1.2580349D+00/
      DATA QQM    (14)/       0.9785824D+00/
      DATA AMM    (14)/       0.3608967D+00/
      DATA ADM    (14)/       0.3664244D+00/
      DATA AQM    (14)/       0.4506740D+00/
C                    DATA FOR ELEMENT 15        PHOSPHORUS
      DATA REFMN  (15)/'  P: (MNDO): M.J.S.DEWAR, M.L.MCKEE, H.S.RZEPA,
     1J. AM. CHEM. SOC., 100 3607 1978'/
      DATA USSM   (15)/     -56.1433600D+00/
      DATA UPPM   (15)/     -42.8510800D+00/
      DATA BETASM (15)/      -6.7916000D+00/
      DATA BETAPM (15)/      -6.7916000D+00/
      DATA ZSM    (15)/       2.1087200D+00/
      DATA ZPM    (15)/       1.7858100D+00/
      DATA ZDM    (15)/       1.0000000D+00/
      DATA ALPM   (15)/       2.4152800D+00/
      DATA EISOLM (15)/    -152.9599600D+00/
      DATA DDM    (15)/       1.0129699D+00/
      DATA QQM    (15)/       0.9370090D+00/
      DATA AMM    (15)/       0.4248438D+00/
      DATA ADM    (15)/       0.4882420D+00/
      DATA AQM    (15)/       0.4979406D+00/
C                    DATA FOR ELEMENT 16        SULFUR
C
      DATA REFMN  (16)/'  S: (MNDO): M.J.S.DEWAR, C.H. REYNOLDS, J. COM
     1P. CHEM. 7, 140-143 (1986)      '/
      DATA USSM   (16)/     -72.2422810D+00/
      DATA UPPM   (16)/     -56.9732070D+00/
      DATA BETASM (16)/     -10.7616700D+00/
      DATA BETAPM (16)/     -10.1084330D+00/
      DATA ZSM    (16)/       2.3129620D+00/
      DATA ZPM    (16)/       2.0091460D+00/
      DATA ZDM    (16)/       1.0000000D+00/
      DATA ALPM   (16)/       2.4780260D+00/
      DATA EISOLM (16)/    -226.0123900D+00/
      DATA DDM    (16)/       0.9189935D+00/
      DATA QQM    (16)/       0.8328514D+00/
      DATA AMM    (16)/       0.4733554D+00/
      DATA ADM    (16)/       0.5544502D+00/
      DATA AQM    (16)/       0.5585244D+00/
C                    DATA FOR ELEMENT 17        CHLORINE
      DATA REFMN  (17)/' CL: (MNDO): M.J.S.DEWAR, H.S.RZEPA, J. COMP. CH
     1EM., 4, 158, (1983)             '/
      DATA USSM   (17)/    -100.2271660D+00/
      DATA UPPM   (17)/     -77.3786670D+00/
      DATA BETASM (17)/     -14.2623200D+00/
      DATA BETAPM (17)/     -14.2623200D+00/
      DATA ZSM    (17)/       3.7846450D+00/
      DATA ZPM    (17)/       2.0362630D+00/
      DATA ZDM    (17)/       1.0000000D+00/
      DATA ALPM   (17)/       2.5422010D+00/
      DATA EISOLM (17)/    -353.1176670D+00/
      DATA DDM    (17)/       0.4986870D+00/
      DATA QQM    (17)/       0.8217603D+00/
      DATA AMM    (17)/       0.5523705D+00/
      DATA ADM    (17)/       0.8061220D+00/
      DATA AQM    (17)/       0.6053435D+00/
C AM1 AND PM3 VARIABLES COMMENTED OUT.
C                               DATA FOR THE POTASSIUM-LIKE SPARKLE
C     DATA REFAM   (19)/' K:  (AM1):  POTASSIUM-LIKE SPARKLE.   USE WITH
C     1 CARE.                          '/
      DATA REFMN   (19)/' K:  (MNDO): POTASSIUM-LIKE SPARKLE.   USE WITH
     1 CARE.                          '/
C     DATA REFPM3  (19)/' K:  (PM3):  POTASSIUM-LIKE SPARKLE.   USE WITH
C     1 CARE.                          '/
      DATA VS(19)       /10.0D+00/
C      DATA ALPAM1(19)      / 1.405D+00/
      DATA ALPM(19)        / 1.396D+00/
C      DATA ALPPM3(19)      / 1.400D+00/
C
C      DATA EISOLA(19)      / 0.0D+00/
      DATA EISOLM(19)      / 0.0D+00/
C      DATA EISOLP(19)      / 0.0D+00/
C
C      DATA AMAM1(19)       / 0.5D+00/
      DATA AMM(19)         / 0.5D+00/
C      DATA AMPM3(19)       / 0.5D+00/
C                    DATA FOR ELEMENT 24  CHROMIUM
      DATA REFMN  (24)/' CR: (MNDO):  M.J.S. DEWAR, E.F. HEALY, J.J.P.
     1STEWART (IN PREPARATION)        '/
      DATA USSM   (24)/     -17.5170270D+00/
      DATA UPPM  (24)/     -12.5337290D+00/
      DATA UDDM  (24)/     -44.1249280D+00/
      DATA BETASM (24)/      -0.1000000D+00/
      DATA BETAPM (24)/      -0.1000000D+00/
      DATA BETADM (24)/      -8.7766360D+00/
      DATA ZSM    (24)/       1.5000000D+00/
      DATA ZPM    (24)/       1.5000000D+00/
      DATA ZDM    (24)/       2.8845490D+00/
      DATA ALPM   (24)/       3.0683070D+00/
      DATA EISOLM (24)/    -134.8187920D+00/
      DATA GSSM   (24)/       6.0000000D+00/
      DATA GSPM   (24)/       4.1500000D+00/
      DATA GPPM   (24)/       5.0000000D+00/
      DATA GP2M   (24)/       3.5000000D+00/
      DATA HSPM   (24)/       1.0000000D+00/
      DATA DDM    (24)/       1.7320508D+00/
      DATA QQM    (24)/       1.4142136D+00/
      DATA AMM    (24)/       0.2205072D+00/
      DATA ADM    (24)/       0.2711332D+00/
      DATA AQM    (24)/       0.4464656D+00/
C                    DATA FOR ELEMENT 30        ZINC
      DATA REFMN  (30)/' ZN: (MNDO):  M.J.S. DEWAR, K.M. MERZ, ORGANOMET
     1ALLICS, 5, 1494-1496 (1986)     '/
C                    DATA FOR ELEMENT 30
      DATA USSM  ( 30)/     -20.8397160D+00/
      DATA UPPM  ( 30)/     -19.6252240D+00/
      DATA BETASM( 30)/      -1.0000000D+00/
      DATA BETAPM( 30)/      -2.0000000D+00/
      DATA ZSM   ( 30)/       2.0473590D+00/
      DATA ZPM   ( 30)/       1.4609460D+00/
      DATA ZDM   ( 30)/       1.0000000D+00/
      DATA ALPM  ( 30)/       1.5064570D+00/
      DATA EISOLM( 30)/     -29.8794320D+00/
      DATA GSSM  ( 30)/      11.8000000D+00/
      DATA GSPM  ( 30)/      11.1820180D+00/
      DATA GPPM  ( 30)/      13.3000000D+00/
      DATA GP2M  ( 30)/      12.9305200D+00/
      DATA HSPM  ( 30)/       0.4846060D+00/
      DATA DDM   ( 30)/       1.3037826D+00/
      DATA QQM   ( 30)/       1.4520183D+00/
      DATA AMM   ( 30)/       0.4336641D+00/
      DATA ADM   ( 30)/       0.2375912D+00/
      DATA AQM   ( 30)/       0.2738858D+00/
C                    DATA FOR ELEMENT 32        GERMANIUM
      DATA REFMN  (32)/' GE: (MNDO): M.J.S.DEWAR, G.L.GRADY, E.F.HEALY,O
     1RGANOMETALLICS 6 186-189, (1987)'/
      DATA USSM  ( 32)/     -33.9493670D+00/
      DATA UPPM  ( 32)/     -27.4251050D+00/
      DATA BETASM( 32)/      -4.5164790D+00/
      DATA BETAPM( 32)/      -1.7555170D+00/
      DATA ZSM   ( 32)/       1.2931800D+00/
      DATA ZPM   ( 32)/       2.0205640D+00/
      DATA ALPM  ( 32)/       1.9784980D+00/
      DATA EISOLM( 32)/     -76.2489440D+00/
      DATA GSSM  ( 32)/       9.8000000D+00/
      DATA GSPM  ( 32)/       8.3000000D+00/
      DATA GPPM  ( 32)/       7.3000000D+00/
      DATA GP2M  ( 32)/       6.5000000D+00/
      DATA HSPM  ( 32)/       1.3000000D+00/
      DATA DDM   ( 32)/       1.2556091D+00/
      DATA QQM   ( 32)/       1.0498655D+00/
      DATA AMM   ( 32)/       0.3601617D+00/
      DATA ADM   ( 32)/       0.3643722D+00/
      DATA AQM   ( 32)/       0.4347337D+00/
C                    DATA FOR ELEMENT 35        BROMINE
      DATA REFMN  (35)/' BR: (MNDO): M.J.S.DEWAR, E.F. HEALY, J. COMP. C
     1HEM., 4, 542, (1983)            '/
      DATA USSM   (35)/     -99.9864405D+00/
      DATA UPPM   (35)/     -75.6713075D+00/
      DATA BETASM (35)/      -8.9171070D+00/
      DATA BETAPM (35)/      -9.9437400D+00/
      DATA ZSM    (35)/       3.8543019D+00/
      DATA ZPM    (35)/       2.1992091D+00/
      DATA ZDM    (35)/       1.0000000D+00/
      DATA ALPM   (35)/       2.4457051D+00/
      DATA EISOLM (35)/    -346.6812500D+00/
      DATA DDM    (35)/       0.6051074D+00/
      DATA QQM    (35)/       0.9645873D+00/
      DATA AMM    (35)/       0.5526068D+00/
      DATA ADM    (35)/       0.7258330D+00/
      DATA AQM    (35)/       0.5574589D+00/
C                    DATA FOR ELEMENT 50        TIN
      DATA REFMN  (50)/' SN: (MNDO): M.J.S.DEWAR,G.L.GRADY,J.J.P.STEWART
     1, J.AM.CHEM.SOC.,106 6771 (1984)'/
      DATA USSM  (50)/     -40.8518020D+00/
      DATA UPPM   (50)/     -28.5602490D+00/
      DATA BETASM (50)/      -3.2351470D+00/
      DATA BETAPM (50)/      -4.2904160D+00/
      DATA ZSM    (50)/       2.0803800D+00/
      DATA ZPM   (50)/       1.9371060D+00/
      DATA ALPM   (50)/       1.8008140D+00/
      DATA EISOLM (50)/     -92.3241020D+00/
      DATA GSSM   (50)/       9.8000000D+00/
      DATA GSPM   (50)/       8.3000000D+00/
      DATA GPPM   (50)/       7.3000000D+00/
      DATA GP2M   (50)/       6.5000000D+00/
      DATA HSPM   (50)/       1.3000000D+00/
      DATA DDM    (50)/       1.5697766D+00/
      DATA QQM    (50)/       1.3262292D+00/
      DATA AMM    (50)/       0.3601617D+00/
      DATA ADM    (50)/       0.3219998D+00/
      DATA AQM    (50)/       0.3713827D+00/
C                    DATA FOR ELEMENT 53        IODINE
      DATA REFMN  (53)/'  I: (MNDO): M.J.S.DEWAR, E.F. HEALY, J.J.P. STE
     1WART, J.COMP.CHEM., 5,358,(1984)'/
      DATA USSM   (53)/    -100.0030538D+00/
      DATA UPPM   (53)/     -74.6114692D+00/
      DATA BETASM (53)/      -7.4144510D+00/
      DATA BETAPM (53)/      -6.1967810D+00/
      DATA ZSM    (53)/       2.2729610D+00/
      DATA ZPM    (53)/       2.1694980D+00/
      DATA ZDM    (53)/       1.0000000D+00/
      DATA ALPM   (53)/       2.2073200D+00/
      DATA EISOLM (53)/    -340.5983600D+00/
      DATA DDM    (53)/       1.4253233D+00/
      DATA QQM    (53)/       1.1841707D+00/
      DATA AMM    (53)/       0.5527541D+00/
      DATA ADM    (53)/       0.4593451D+00/
      DATA AQM    (53)/       0.4585376D+00/
C                    DATA FOR ELEMENT 80        MERCURY
      DATA REFMN  (80)/' HG: (MNDO): M.J.S.DEWAR,  ET. AL. ORGANOMETALLI
     1CS 4, 1964, (1985) SEE MANUAL   '/
      DATA USSM   ( 80)/     -19.8095740D+00/
      DATA UPPM   ( 80)/     -13.1025300D+00/
      DATA BETASM ( 80)/      -0.4045250D+00/
      DATA BETAPM ( 80)/      -6.2066830D+00/
      DATA ZSM    ( 80)/       2.2181840D+00/
      DATA ZPM    ( 80)/       2.0650380D+00/
      DATA ALPM   ( 80)/       1.3356410D+00/
      DATA EISOLM ( 80)/     -28.8191480D+00/
      DATA GSSM   ( 80)/      10.8000000D+00/
      DATA GSPM   ( 80)/       9.3000000D+00/
      DATA GPPM   ( 80)/      14.3000000D+00/
      DATA GP2M   ( 80)/      13.5000000D+00/
      DATA HSPM   ( 80)/       1.3000000D+00/
      DATA DDM    ( 80)/       1.7378048D+00/
      DATA QQM    ( 80)/       1.4608064D+00/
      DATA AMM    ( 80)/       0.3969129D+00/
      DATA ADM    ( 80)/       0.3047694D+00/
      DATA AQM    ( 80)/       0.3483102D+00/
C                    DATA FOR ELEMENT 82        LEAD
      DATA REFMN  (82)/' PB: (MNDO): M.J.S.DEWAR, ET.AL ORGANOMETALLICS
     14 1973-1980 (1985)              '/
      DATA USSM   ( 82)/     -47.3196920D+00/
      DATA UPPM   ( 82)/     -28.8475600D+00/
      DATA BETASM ( 82)/      -8.0423870D+00/
      DATA BETAPM ( 82)/      -3.0000000D+00/
      DATA ZSM    ( 82)/       2.4982860D+00/
      DATA ZPM    ( 82)/       2.0820710D+00/
      DATA ALPM   ( 82)/       1.7283330D+00/
      DATA EISOLM ( 82)/    -105.8345040D+00/
      DATA GSSM   ( 82)/       9.8000000D+00/
      DATA GSPM   ( 82)/       8.3000000D+00/
      DATA GPPM   ( 82)/       7.3000000D+00/
      DATA GP2M   ( 82)/       6.5000000D+00/
      DATA HSPM   ( 82)/       1.3000000D+00/
      DATA DDM    ( 82)/       1.5526624D+00/
      DATA QQM    ( 82)/       1.4488558D+00/
      DATA AMM    ( 82)/       0.3601617D+00/
      DATA ADM    ( 82)/       0.3239309D+00/
      DATA AQM    ( 82)/       0.3502057D+00/
C
C     START OF "OLD" ELEMENTS: THESE ARE OLD PARAMETERS WHICH
C     CAN BE USED, IF DESIRED, BY SPECIFYING "<CHEMICAL SYMBOL>YEAR"
C     AS IN SI1978 OR  S1983.
C
C                    DATA FOR ELEMENT 90        SILICON
      DATA REFMN  (90)/' SI: (MNDO): M.J.S.DEWAR, M.L.MCKEE, H.S.RZEPA,
     1J. AM. CHEM. SOC., 100 3607 1978'/
      DATA USSM   (90)/     -40.5682920D+00/
      DATA UPPM   (90)/     -28.0891870D+00/
      DATA BETASM (90)/      -4.2562180D+00/
      DATA BETAPM (90)/      -4.2562180D+00/
      DATA ZSM    (90)/       1.4353060D+00/
      DATA ZPM    (90)/       1.4353060D+00/
      DATA ZDM    (90)/       1.0000000D+00/
      DATA ALPM   (90)/       2.1961078D+00/
      DATA EISOLM (90)/     -90.5399580D+00/
      DATA DDM    (90)/       1.4078712D+00/
      DATA QQM    (90)/       1.1658281D+00/
      DATA AMM    (90)/       0.3608967D+00/
      DATA ADM    (90)/       0.3441817D+00/
      DATA AQM    (90)/       0.3999442D+00/
      DATA HSPM(90)/1.32D+00/
      DATA GP2M(90)/6.54D+00/
      DATA GPPM(90)/7.31D+00/
      DATA GSPM(90)/8.36D+00/
      DATA GSSM(90)/9.82D+00/
      DATA REFMN  (91)/'  S: (MNDO): M.J.S.DEWAR, H.S. RZEPA, M.L.MCKEE,
     1 J.AM.CHEM.SOC.100, 3607 (1978).'/
      DATA USSM   (91)/     -75.2391520D+00/
      DATA UPPM   (91)/     -57.8320130D+00/
      DATA BETASM (91)/     -11.1422310D+00/
      DATA BETAPM (91)/     -11.1422310D+00/
      DATA ZSM    (91)/       2.6135910D+00/
      DATA ZPM    (91)/       2.0343930D+00/
      DATA ZDM    (91)/       1.0000000D+00/
      DATA ALPM   (91)/       2.4916445D+00/
      DATA EISOLM (91)/    -235.4413560D+00/
      DATA DDM    (91)/       0.8231596D+00/
      DATA QQM    (91)/       0.8225156D+00/
      DATA AMM    (91)/       0.4733554D+00/
      DATA ADM    (91)/       0.5889395D+00/
      DATA AQM    (91)/       0.5632724D+00/
      DATA REFMN (102)/' CB: (MNDO):  CAPPED BOND  (HYDROGEN-LIKE, TAKES
     1 ON A  ZERO CHARGE.)            '/
      DATA USSM  (102)/     -11.9062760D+00/
      DATA BETASM(102)/-9999999.0000000D+00/
      DATA ZSM   (102)/       4.0000000D+00/
      DATA ZPM   (102)/       0.3000000D+00/
      DATA ZDM   (102)/       0.3000000D+00/
      DATA ALPM  (102)/       2.5441341D+00/
      DATA EISOLM(102)/       4.0000000D+00/
      DATA GSSM  (102)/      12.8480000D+00/
      DATA HSPM  (102)/       0.1000000D+00/
      DATA DDM   (102)/       0.0684105D+00/
      DATA QQM   (102)/       1.0540926D+00/
      DATA AMM   (102)/       0.4721793D+00/
      DATA ADM   (102)/       0.9262742D+00/
      DATA AQM   (102)/       0.2909059D+00/
C**********************************************************************
C
C    START OF AM1 PARAMETERS
C
C**********************************************************************
C                    DATA FOR ELEMENT  1       AM1:   HYDROGEN
      DATA REFAM  ( 1)/'  H: (AM1): M.J.S. DEWAR ET AL, J. AM. CHEM. SOC
     1. 107 3902-3909 (1985)          '/
      DATA USSAM1( 1)/     -11.3964270D+00/
      DATA UPPAM1( 1)/0.0D+00/
      DATA BETASA( 1)/      -6.1737870D+00/
      DATA BETAPA( 1)/0.0D+00/
      DATA ZSAM1 ( 1)/       1.1880780D+00/
      DATA ZPAM1 ( 1)/0.0D+00/
      DATA ALPAM1( 1)/       2.8823240D+00/
      DATA EISOLA( 1)/     -11.3964270D+00/
      DATA GSSAM1( 1)/      12.8480000D+00/
      DATA GSPAM1( 1)/0.0D+00/
      DATA GPPAM1( 1)/0.0D+00/
      DATA GP2AM1( 1)/0.0D+00/
      DATA HSPAM1( 1)/0.0D+00/
      DATA AMAM1 ( 1)/       0.4721793D+00/
      DATA ADAM1 ( 1)/       0.4721793D+00/
      DATA AQAM1 ( 1)/       0.4721793D+00/
      DATA GUESA1( 1,1)/       0.1227960D+00/
      DATA GUESA2( 1,1)/       5.0000000D+00/
      DATA GUESA3( 1,1)/       1.2000000D+00/
      DATA GUESA1( 1,2)/       0.0050900D+00/
      DATA GUESA2( 1,2)/       5.0000000D+00/
      DATA GUESA3( 1,2)/       1.8000000D+00/
      DATA GUESA1( 1,3)/      -0.0183360D+00/
      DATA GUESA2( 1,3)/       2.0000000D+00/
      DATA GUESA3( 1,3)/       2.1000000D+00/
C                    DATA FOR ELEMENT  3       AM1:   LITHIUM    *
      DATA REFAM  ( 3)/' LI: (MNDO):  TAKEN FROM MNDOC BY W.THIEL,
     1QCPE NO.438, V. 2, P.63, (1982).'/
      DATA USSAM1(  3)/      -5.1280000D+00/
      DATA UPPAM1(  3)/      -2.7212000D+00/
      DATA BETASA(  3)/      -1.3500400D+00/
      DATA BETAPA(  3)/      -1.3500400D+00/
      DATA ZSAM1 (  3)/       0.7023800D+00/
      DATA ZPAM1 (  3)/       0.7023800D+00/
      DATA ALPAM1(  3)/       1.2501400D+00/
      DATA EISOLA(  3)/      -5.1280000D+00/
      DATA GSSAM1(  3)/       7.3000000D+00/
      DATA GSPAM1(  3)/       5.4200000D+00/
      DATA GPPAM1(  3)/       5.0000000D+00/
      DATA GP2AM1(  3)/       4.5200000D+00/
      DATA HSPAM1(  3)/       0.8300000D+00/
      DATA DDAM1 (  3)/       2.0549783D+00/
      DATA QQAM1 (  3)/       1.7437069D+00/
      DATA AMAM1 (  3)/       0.2682837D+00/
      DATA ADAM1 (  3)/       0.2269793D+00/
      DATA AQAM1 (  3)/       0.2614581D+00/
C                    DATA FOR ELEMENT  4       AM1:   BERYLLIUM  *
      DATA REFAM  ( 4)/' BE: (MNDO):  M.J.S. DEWAR, H.S. RZEPA, J. AM. C
     1HEM. SOC., 100, 777, (1978)     '/
      DATA USSAM1( 4)/     -16.6023780D+00/
      DATA UPPAM1( 4)/     -10.7037710D+00/
      DATA BETASA( 4)/      -4.0170960D+00/
      DATA BETAPA( 4)/      -4.0170960D+00/
      DATA ZSAM1 ( 4)/       1.0042100D+00/
      DATA ZPAM1 ( 4)/       1.0042100D+00/
      DATA ALPAM1( 4)/       1.6694340D+00/
      DATA EISOLA( 4)/     -24.2047560D+00/
      DATA GSSAM1( 4)/       9.0000000D+00/
      DATA GSPAM1( 4)/       7.4300000D+00/
      DATA GPPAM1( 4)/       6.9700000D+00/
      DATA GP2AM1( 4)/       6.2200000D+00/
      DATA HSPAM1( 4)/       1.2800000D+00/
      DATA DDAM1 ( 4)/       1.4373245D+00/
      DATA QQAM1 ( 4)/       1.2196103D+00/
      DATA AMAM1 ( 4)/       0.3307607D+00/
      DATA ADAM1 ( 4)/       0.3356142D+00/
      DATA AQAM1 ( 4)/       0.3846373D+00/
C                    DATA FOR ELEMENT  5       AM1:   BORON  *
      DATA REFAM  ( 5)/'  B: (AM1):  M.J.S. DEWAR, C. JIE, E. G. ZOEBISC
     1H ORGANOMETALLICS 7, 513 (1988) '/
C                    DATA FOR ELEMENT  5
      DATA USSAM1(  5)/     -34.4928700D+00/
      DATA UPPAM1(  5)/     -22.6315250D+00/
      DATA BETASA(  5)/      -9.5991140D+00/
      DATA BETAPA(  5)/      -6.2737570D+00/
      DATA ZSAM1 (  5)/       1.6117090D+00/
      DATA ZPAM1 (  5)/       1.5553850D+00/
      DATA ALPAM1(  5)/       2.4469090D+00/
      DATA EISOLA(  5)/     -63.7172650D+00/
      DATA GSSAM1(  5)/      10.5900000D+00/
      DATA GSPAM1(  5)/       9.5600000D+00/
      DATA GPPAM1(  5)/       8.8600000D+00/
      DATA GP2AM1(  5)/       7.8600000D+00/
      DATA HSPAM1(  5)/       1.8100000D+00/
      DATA DDAM1 (  5)/       0.9107622D+00/
      DATA QQAM1 (  5)/       0.7874223D+00/
      DATA AMAM1 (  5)/       0.3891951D+00/
      DATA ADAM1 (  5)/       0.5045152D+00/
      DATA AQAM1 (  5)/       0.5678856D+00/
C                    DATA FOR ELEMENT  6       AM1:   CARBON
      DATA REFAM  ( 6)/'  C: (AM1): M.J.S. DEWAR ET AL, J. AM. CHEM. SOC
     1. 107 3902-3909 (1985)          '/
      DATA USSAM1( 6)/     -52.0286580D+00/
      DATA UPPAM1( 6)/     -39.6142390D+00/
      DATA BETASA( 6)/     -15.7157830D+00/
      DATA BETAPA( 6)/      -7.7192830D+00/
      DATA ZSAM1 ( 6)/       1.8086650D+00/
      DATA ZPAM1 ( 6)/       1.6851160D+00/
      DATA ALPAM1( 6)/       2.6482740D+00/
      DATA EISOLA( 6)/    -120.8157940D+00/
      DATA GSSAM1( 6)/      12.2300000D+00/
      DATA GSPAM1( 6)/      11.4700000D+00/
      DATA GPPAM1( 6)/      11.0800000D+00/
      DATA GP2AM1( 6)/       9.8400000D+00/
      DATA HSPAM1( 6)/       2.4300000D+00/
      DATA DDAM1 ( 6)/       0.8236736D+00/
      DATA QQAM1 ( 6)/       0.7268015D+00/
      DATA AMAM1 ( 6)/       0.4494671D+00/
      DATA ADAM1 ( 6)/       0.6082946D+00/
      DATA AQAM1 ( 6)/       0.6423492D+00/
      DATA GUESA1( 6,1)/       0.0113550D+00/
      DATA GUESA2( 6,1)/       5.0000000D+00/
      DATA GUESA3( 6,1)/       1.6000000D+00/
      DATA GUESA1( 6,2)/       0.0459240D+00/
      DATA GUESA2( 6,2)/       5.0000000D+00/
      DATA GUESA3( 6,2)/       1.8500000D+00/
      DATA GUESA1( 6,3)/      -0.0200610D+00/
      DATA GUESA2( 6,3)/       5.0000000D+00/
      DATA GUESA3( 6,3)/       2.0500000D+00/
      DATA GUESA1( 6,4)/      -0.0012600D+00/
      DATA GUESA2( 6,4)/       5.0000000D+00/
      DATA GUESA3( 6,4)/       2.6500000D+00/
C                    DATA FOR ELEMENT  7       AM1:   NITROGEN
      DATA REFAM  ( 7)/'  N: (AM1): M.J.S. DEWAR ET AL, J. AM. CHEM. SOC
     1. 107 3902-3909 (1985)          '/
      DATA USSAM1( 7)/     -71.8600000D+00/
      DATA UPPAM1( 7)/     -57.1675810D+00/
      DATA BETASA( 7)/     -20.2991100D+00/
      DATA BETAPA( 7)/     -18.2386660D+00/
      DATA ZSAM1 ( 7)/       2.3154100D+00/
      DATA ZPAM1 ( 7)/       2.1579400D+00/
      DATA ALPAM1( 7)/       2.9472860D+00/
      DATA EISOLA( 7)/    -202.4077430D+00/
      DATA GSSAM1( 7)/      13.5900000D+00/
      DATA GSPAM1( 7)/      12.6600000D+00/
      DATA GPPAM1( 7)/      12.9800000D+00/
      DATA GP2AM1( 7)/      11.5900000D+00/
      DATA HSPAM1( 7)/       3.1400000D+00/
      DATA DDAM1 ( 7)/       0.6433247D+00/
      DATA QQAM1 ( 7)/       0.5675528D+00/
      DATA AMAM1 ( 7)/       0.4994487D+00/
      DATA ADAM1 ( 7)/       0.7820840D+00/
      DATA AQAM1 ( 7)/       0.7883498D+00/
      DATA GUESA1( 7,1)/       0.0252510D+00/
      DATA GUESA2( 7,1)/       5.0000000D+00/
      DATA GUESA3( 7,1)/       1.5000000D+00/
      DATA GUESA1( 7,2)/       0.0289530D+00/
      DATA GUESA2( 7,2)/       5.0000000D+00/
      DATA GUESA3( 7,2)/       2.1000000D+00/
      DATA GUESA1( 7,3)/      -0.0058060D+00/
      DATA GUESA2( 7,3)/       2.0000000D+00/
      DATA GUESA3( 7,3)/       2.4000000D+00/
C                    DATA FOR ELEMENT  8       AM1:   OXYGEN
      DATA REFAM  ( 8)/'  O: (AM1): M.J.S. DEWAR ET AL, J. AM. CHEM. SOC
     1. 107 3902-3909 (1985)          '/
      DATA USSAM1( 8)/     -97.8300000D+00/
      DATA UPPAM1( 8)/     -78.2623800D+00/
      DATA BETASA( 8)/     -29.2727730D+00/
      DATA BETAPA( 8)/     -29.2727730D+00/
      DATA ZSAM1 ( 8)/       3.1080320D+00/
      DATA ZPAM1 ( 8)/       2.5240390D+00/
      DATA ALPAM1( 8)/       4.4553710D+00/
      DATA EISOLA( 8)/    -316.0995200D+00/
      DATA GSSAM1( 8)/      15.4200000D+00/
      DATA GSPAM1( 8)/      14.4800000D+00/
      DATA GPPAM1( 8)/      14.5200000D+00/
      DATA GP2AM1( 8)/      12.9800000D+00/
      DATA HSPAM1( 8)/       3.9400000D+00/
      DATA DDAM1 ( 8)/       0.4988896D+00/
      DATA QQAM1 ( 8)/       0.4852322D+00/
      DATA AMAM1 ( 8)/       0.5667034D+00/
      DATA ADAM1 ( 8)/       0.9961066D+00/
      DATA AQAM1 ( 8)/       0.9065223D+00/
      DATA GUESA1( 8,1)/       0.2809620D+00/
      DATA GUESA2( 8,1)/       5.0000000D+00/
      DATA GUESA3( 8,1)/       0.8479180D+00/
      DATA GUESA1( 8,2)/       0.0814300D+00/
      DATA GUESA2( 8,2)/       7.0000000D+00/
      DATA GUESA3( 8,2)/       1.4450710D+00/
C                    DATA FOR ELEMENT  9       AM1:   FLUORINE  *
      DATA REFAM  ( 9)/'  F: (AM1): M.J.S. DEWAR AND E. G. ZOEBISCH, THE
     1OCHEM, 180, 1 (1988).           '/
      DATA USSAM1( 9)/    -136.1055790D+00/
      DATA UPPAM1( 9)/    -104.8898850D+00/
      DATA BETASA( 9)/     -69.5902770D+00/
      DATA BETAPA( 9)/     -27.9223600D+00/
      DATA ZSAM1 ( 9)/       3.7700820D+00/
      DATA ZPAM1 ( 9)/       2.4946700D+00/
      DATA ALPAM1( 9)/       5.5178000D+00/
      DATA EISOLA( 9)/    -482.2905830D+00/
      DATA GSSAM1( 9)/      16.9200000D+00/
      DATA GSPAM1( 9)/      17.2500000D+00/
      DATA GPPAM1( 9)/      16.7100000D+00/
      DATA GP2AM1( 9)/      14.9100000D+00/
      DATA HSPAM1( 9)/       4.8300000D+00/
      DATA DDAM1 ( 9)/       0.4145203D+00/
      DATA QQAM1 ( 9)/       0.4909446D+00/
      DATA AMAM1 ( 9)/       0.6218302D+00/
      DATA ADAM1 ( 9)/       1.2088792D+00/
      DATA AQAM1 ( 9)/       0.9449355D+00/
      DATA GUESA1( 9,1)/       0.2420790D+00/
      DATA GUESA2( 9,1)/       4.8000000D+00/
      DATA GUESA3( 9,1)/       0.9300000D+00/
      DATA GUESA1( 9,2)/       0.0036070D+00/
      DATA GUESA2( 9,2)/       4.6000000D+00/
      DATA GUESA3( 9,2)/       1.6600000D+00/
C
C                    DATA FOR ELEMENT 11       AM1:   SODIUM  *
      DATA REFAM  (11)/' NA: (AM1): E. N. BROTHERS, K. M. MERZ, JR.   J
     *.PHYS.CHEM. B 106, 2779 (2002)  '/
      DATA USSAM1(11)/      -5.2555362D+00/
      DATA UPPAM1(11)/      -2.0812781D+00/
      DATA BETASA(11)/      -1.4536944D+00/
      DATA BETAPA(11)/      -0.2298064D+00/
      DATA ZSAM1 (11)/       0.6797792D+00/
      DATA ZPAM1 (11)/       1.2170468D+00/
      DATA ALPAM1(11)/       2.2487164D+00/
      DATA EISOLA(11)/      -5.2555362D+00/
      DATA GSSAM1(11)/       7.3459178D+00/
      DATA GSPAM1(11)/       8.4042550D+00/
      DATA GPPAM1(11)/       4.1130516D+00/
      DATA GP2AM1(11)/       4.0370957D+00/
      DATA HSPAM1(11)/       0.2487699D+00/
      DATA DDAM1 (11)/       1.5899755D+00/
      DATA QQAM1 (11)/       1.3749019D+00/
      DATA AMAM1 (11)/       0.2699713D+00/
      DATA ADAM1 (11)/       0.1618047D+00/
      DATA AQAM1 (11)/       0.1865738D+00/
      DATA GUESA1(11,1)/       0.5322668D+00/
      DATA GUESA2(11,1)/       0.4800304D+00/
      DATA GUESA3(11,1)/       1.1681055D+00/
      DATA GUESA1(11,2)/       0.9223598D+00/
      DATA GUESA2(11,2)/       1.9076776D+00/
      DATA GUESA3(11,2)/       1.1537670D+00/
C
C                    DATA FOR ELEMENT 12       AM1:   MAGNESIUM  *
      DATA REFAM  (12)/' MG: (AM1): M.C.HUTTER, J.R.REIMERS, N.S.HUSH,
     *J.PHYS.CHEM.B 102, 8080 (1998)  '/
      DATA USSAM1(12)/     -14.96959313D+00/
      DATA UPPAM1(12)/     -11.56229248D+00/
      DATA BETASA(12)/      -1.25974355D+00/
      DATA BETAPA(12)/      -0.77836604D+00/
      DATA ZSAM1 (12)/       1.22339270D+00/
      DATA ZPAM1 (12)/       1.02030798D+00/
      DATA ALPAM1(12)/       1.67049799D+00/
      DATA EISOLA(12)/     -22.43786349D+00/
      DATA GSSAM1(12)/       7.50132277D+00/
      DATA GSPAM1(12)/       6.34591536D+00/
      DATA GPPAM1(12)/       4.77534467D+00/
      DATA GP2AM1(12)/       4.34017279D+00/
      DATA HSPAM1(12)/       0.48930466D+00/
      DATA DDAM1 (12)/       1.75012115D+00/
      DATA QQAM1 (12)/       1.64001467D+00/
      DATA AMAM1 (12)/       0.27568258D+00/
      DATA ADAM1 (12)/       0.19957236D+00/
      DATA AQAM1 (12)/       0.26440152D+00/
      DATA GUESA1(12,1)/       2.55017735D+00/
      DATA GUESA2(12,1)/       4.29397225D+00/
      DATA GUESA3(12,1)/       0.79989601D+00/
      DATA GUESA1(12,2)/      -0.00565806D+00/
      DATA GUESA2(12,2)/       2.96053910D+00/
      DATA GUESA3(12,2)/       1.47499983D+00/
      DATA GUESA1(12,3)/      -0.00610286D+00/
      DATA GUESA2(12,3)/       2.61416919D+00/
      DATA GUESA3(12,3)/       2.42604040D+00/
C                    DATA FOR ELEMENT 13       AM1:   ALUMINUM  *
      DATA REFAM  (13)/' AL: (AM1):  M. J. S. DEWAR, A. J. HOLDER, ORGAN
     1OMETALLICS, 9, 508-511 (1990).  '/
      DATA USSAM1( 13)/     -24.3535850D+00/
      DATA UPPAM1( 13)/     -18.3636450D+00/
      DATA BETASA( 13)/      -3.8668220D+00/
      DATA BETAPA( 13)/      -2.3171460D+00/
      DATA ZSAM1 ( 13)/       1.5165930D+00/
      DATA ZPAM1 ( 13)/       1.3063470D+00/
      DATA ZDAM1 ( 13)/       1.0000000D+00/
      DATA ALPAM1( 13)/       1.9765860D+00/
      DATA EISOLA( 13)/     -46.4208150D+00/
      DATA GSSAM1( 13)/       8.0900000D+00/
      DATA GSPAM1( 13)/       6.6300000D+00/
      DATA GPPAM1( 13)/       5.9800000D+00/
      DATA GP2AM1( 13)/       5.4000000D+00/
      DATA HSPAM1( 13)/       0.7000000D+00/
      DATA DDAM1 ( 13)/       1.4040443D+00/
      DATA QQAM1 ( 13)/       1.2809154D+00/
      DATA AMAM1 ( 13)/       0.2973172D+00/
      DATA ADAM1 ( 13)/       0.2630229D+00/
      DATA AQAM1 ( 13)/       0.3427832D+00/
      DATA GUESA1( 13,1)/       0.0900000D+00/
      DATA GUESA2( 13,1)/      12.3924430D+00/
      DATA GUESA3( 13,1)/       2.0503940D+00/
C                    DATA FOR ELEMENT 14       AM1:   SILICON  *
      DATA REFAM  (14)/' SI: (AM1): M.J.S.DEWAR, C. JIE, ORGANOMETALLICS
     1, 6, 1486-1490 (1987).          '/
      DATA USSAM1(14)/     -33.9536220D+00/
      DATA UPPAM1(14)/     -28.9347490D+00/
      DATA BETASA(14)/      -3.784852D+00/
      DATA BETAPA(14)/      -1.968123D+00/
      DATA ZSAM1 (14)/       1.830697D+00/
      DATA ZPAM1 (14)/       1.2849530D+00/
      DATA ZDAM1 (14)/       1.0000000D+00/
      DATA ALPAM1(14)/       2.257816D+00/
      DATA EISOLA(14)/     -79.0017420D+00/
      DATA GSSAM1(14)/       9.8200000D+00/
      DATA GSPAM1(14)/       8.3600000D+00/
      DATA GPPAM1(14)/       7.3100000D+00/
      DATA GP2AM1(14)/       6.5400000D+00/
      DATA HSPAM1(14)/       1.3200000D+00/
      DATA DDAM1 (14)/       1.1631107D+00/
      DATA QQAM1 (14)/       1.3022422D+00/
      DATA AMAM1 (14)/       0.3608967D+00/
      DATA ADAM1 (14)/       0.3829813D+00/
      DATA AQAM1 (14)/       0.3712106D+00/
      DATA GUESA1(14,1)/       0.25D+00/
      DATA GUESA2(14,1)/       9.000D+00/
      DATA GUESA3(14,1)/       0.911453D+00/
      DATA GUESA1(14,2)/       0.061513D+00/
      DATA GUESA2(14,2)/       5.00D+00/
      DATA GUESA3(14,2)/       1.995569D+00/
      DATA GUESA1(14,3)/       0.0207890D+00/
      DATA GUESA2(14,3)/       5.00D+00/
      DATA GUESA3(14,3)/       2.990610D+00/
C                    DATA FOR ELEMENT 15        PHOSPHORUS
      DATA REFAM  (15)/'  P: (AM1): M.J.S.DEWAR, JIE, C, THEOCHEM, 187,
     11 (1989)                        '/
      DATA USSAM1( 15)/     -42.0298630D+00/
      DATA UPPAM1( 15)/     -34.0307090D+00/
      DATA BETASA( 15)/      -6.3537640D+00/
      DATA BETAPA( 15)/      -6.5907090D+00/
      DATA ZSAM1 ( 15)/       1.9812800D+00/
      DATA ZPAM1 ( 15)/       1.8751500D+00/
      DATA ZDAM1 ( 15)/       1.0000000D+00/
      DATA ALPAM1( 15)/       2.4553220D+00/
      DATA EISOLA( 15)/    -124.4368355D+00/
      DATA GSSAM1( 15)/      11.5600050D+00/
      DATA GSPAM1( 15)/       5.2374490D+00/
      DATA GPPAM1( 15)/       7.8775890D+00/
      DATA GP2AM1( 15)/       7.3076480D+00/
      DATA HSPAM1( 15)/       0.7792380D+00/
      DATA DDAM1 ( 15)/       1.0452022D+00/
      DATA QQAM1 ( 15)/       0.8923660D+00/
      DATA AMAM1 ( 15)/       0.4248440D+00/
      DATA ADAM1 ( 15)/       0.3275319D+00/
      DATA AQAM1 ( 15)/       0.4386854D+00/
      DATA GUESA1( 15,1)/      -0.0318270D+00/
      DATA GUESA2( 15,1)/       6.0000000D+00/
      DATA GUESA3( 15,1)/       1.4743230D+00/
      DATA GUESA1( 15,2)/       0.0184700D+00/
      DATA GUESA2( 15,2)/       7.0000000D+00/
      DATA GUESA3( 15,2)/       1.7793540D+00/
      DATA GUESA1( 15,3)/       0.0332900D+00/
      DATA GUESA2( 15,3)/       9.0000000D+00/
      DATA GUESA3( 15,3)/       3.0065760D+00/
C                    DATA FOR ELEMENT 16       AM1:   SULFUR  *
C
      DATA REFAM  (16)/'  S: (AM1): M.J.S.DEWAR, Y-C YUAN, THEOCHEM, IN
     1 PRESS                          '/
      DATA USSAM1(16)/     -56.6940560D+00/
      DATA UPPAM1(16)/     -48.7170490D+00/
      DATA BETASA(16)/      -3.9205660D+00/
      DATA BETAPA(16)/      -7.9052780D+00/
      DATA ZSAM1 (16)/       2.3665150D+00/
      DATA ZPAM1 (16)/       1.6672630D+00/
      DATA ZDAM1 (16)/       1.0000000D+00/
      DATA ALPAM1(16)/       2.4616480D+00/
      DATA EISOLA(16)/    -191.7321930D+00/
      DATA GSSAM1(16)/      11.7863290D+00/
      DATA GSPAM1(16)/       8.6631270D+00/
      DATA GPPAM1(16)/      10.0393080D+00/
      DATA GP2AM1(16)/       7.7816880D+00/
      DATA HSPAM1(16)/       2.5321370D+00/
      DATA DDAM1 (16)/       0.9004265D+00/
      DATA QQAM1 (16)/       1.0036329D+00/
      DATA AMAM1 (16)/       0.4331617D+00/
      DATA ADAM1 (16)/       0.5907115D+00/
      DATA AQAM1 (16)/       0.6454943D+00/
      DATA GUESA1(16,1)/      -0.5091950D+00/
      DATA GUESA2(16,1)/       4.5936910D+00/
      DATA GUESA3(16,1)/       0.7706650D+00/
      DATA GUESA1(16,2)/      -0.0118630D+00/
      DATA GUESA2(16,2)/       5.8657310D+00/
      DATA GUESA3(16,2)/       1.5033130D+00/
      DATA GUESA1(16,3)/       0.0123340D+00/
      DATA GUESA2(16,3)/      13.5573360D+00/
      DATA GUESA3(16,3)/       2.0091730D+00/
C                    DATA FOR ELEMENT 17       AM1:   CHLORINE  *
      DATA REFAM  (17)/' CL: (AM1): M.J.S. DEWAR AND E. G. ZOEBISCH, THE
     1OCHEM, 180, 1 (1988).           '/
      DATA USSAM1(17)/    -111.6139480D+00/
      DATA UPPAM1(17)/     -76.6401070D+00/
      DATA BETASA(17)/     -24.5946700D+00/
      DATA BETAPA(17)/     -14.6372160D+00/
      DATA ZSAM1 (17)/       3.6313760D+00/
      DATA ZPAM1 (17)/       2.0767990D+00/
      DATA ZDAM1 (17)/       1.0000000D+00/
      DATA ALPAM1(17)/       2.9193680D+00/
      DATA EISOLA(17)/    -372.1984310D+00/
      DATA GSSAM1(17)/      15.0300000D+00/
      DATA GSPAM1(17)/      13.1600000D+00/
      DATA GPPAM1(17)/      11.3000000D+00/
      DATA GP2AM1(17)/       9.9700000D+00/
      DATA HSPAM1(17)/       2.4200000D+00/
      DATA DDAM1 (17)/       0.5406286D+00/
      DATA QQAM1 (17)/       0.8057208D+00/
      DATA AMAM1 (17)/       0.5523705D+00/
      DATA ADAM1 (17)/       0.7693200D+00/
      DATA AQAM1 (17)/       0.6133369D+00/
      DATA GUESA1(17,1)/       0.0942430D+00/
      DATA GUESA2(17,1)/       4.0000000D+00/
      DATA GUESA3(17,1)/       1.3000000D+00/
      DATA GUESA1(17,2)/       0.0271680D+00/
      DATA GUESA2(17,2)/       4.0000000D+00/
      DATA GUESA3(17,2)/       2.1000000D+00/
C GNM, ADDED EXPT'L K AM1 PARAMETERS.
C                    DATA FOR ELEMENT 19       AM1:   POTASSIUM *
      DATA REFAM  (19)/'  K: (AM1): BROTHERS ET AL.
     *                                '/
      DATA USSAM1( 19)/      -3.9255578D+00/
      DATA UPPAM1( 19)/      -3.6636147D+00/
      DATA BETASA( 19)/      -0.6317993D+00/
      DATA BETAPA( 19)/      -0.9161666D+00/
      DATA ZSAM1 ( 19)/       0.6488138D+00/
      DATA ZPAM1 ( 19)/       1.4000748D+00/
      DATA ALPAM1( 19)/       2.9372756D+00/
      DATA EISOLA( 19)/      -3.9255578D+00/
      DATA GSSAM1( 19)/       3.8069197D+00/
      DATA GSPAM1( 19)/       3.2954026D+00/
      DATA GPPAM1( 19)/       4.1818731D+00/
      DATA GP2AM1( 19)/       3.8783801D+00/
      DATA HSPAM1( 19)/       0.3622387D+00/
      DATA DDAM1 ( 19)/       1.3243088D+00/
      DATA QQAM1 ( 19)/       1.5151479D+00/
      DATA AMAM1 ( 19)/       0.1399088D+00/
      DATA ADAM1 ( 19)/       0.2092712D+00/
      DATA AQAM1 ( 19)/       0.2508810D+00/
      DATA GUESA1( 19,1)/       0.6346355D+00/
      DATA GUESA2( 19,1)/       0.6000376D+00/
      DATA GUESA3( 19,1)/       1.6916535D+00/
      DATA GUESA1( 19,2)/       0.9037603D+00/
      DATA GUESA2( 19,2)/       4.1170154D+00/
      DATA GUESA3( 19,2)/       1.6886041D+00/
C GNM, ADDED EXPT'L CA AM1 PARAMETERS.
C                    DATA FOR ELEMENT 20       AM1:   CALCIUM *
      DATA REFAM  (20)/' CA: (AM1): BROTHERS ET AL.
     *                                '/
      DATA USSAM1(20)/     -10.7028455D+00/
      DATA UPPAM1(20)/      -8.0820513D+00/
      DATA BETASA(20)/      -0.2442123D+00/
      DATA BETAPA(20)/      -0.1570320D+00/
      DATA ZSAM1 (20)/       1.5325986D+00/
      DATA ZPAM1 (20)/       1.3035969D+00/
      DATA ALPAM1(20)/       4.4716160D+00/
      DATA EISOLA(20)/     -17.0382445D+00/
      DATA GSSAM1(20)/       4.3674464D+00/
      DATA GSPAM1(20)/       3.0888635D+00/
      DATA GPPAM1(20)/       1.4706614D+00/
      DATA GP2AM1(20)/       1.1658114D+00/
      DATA HSPAM1(20)/       0.2137340D+00/
      DATA DDAM1 (20)/       1.7789472D+00/
      DATA QQAM1 (20)/       1.6272825D+00/
      DATA AMAM1 (20)/       0.1605089D+00/
      DATA ADAM1 (20)/       0.1423588D+00/
      DATA AQAM1 (20)/       0.2388124D+00/
      DATA GUESA1(20,1)/       0.3717684D+00/
      DATA GUESA2(20,1)/       0.6177864D+00/
      DATA GUESA3(20,1)/       1.1915704D+00/
      DATA GUESA1(20,2)/       0.8386009D+00/
      DATA GUESA2(20,2)/       1.6856748D+00/
      DATA GUESA3(20,2)/       1.1027440D+00/
C                    DATA FOR ELEMENT 30        ZINC
      DATA REFAM  (30)/' ZN: (AM1):  M.J.S. DEWAR, K.M. MERZ, ORGANOMET
     1ALLICS, 7, 522-524 (1988)       '/
      DATA USSAM1( 30)/     -21.0400080D+00/
      DATA UPPAM1( 30)/     -17.6555740D+00/
      DATA BETASA( 30)/      -1.9974290D+00/
      DATA BETAPA( 30)/      -4.7581190D+00/
      DATA ZSAM1 ( 30)/       1.9542990D+00/
      DATA ZPAM1 ( 30)/       1.3723650D+00/
      DATA ZDAM1 ( 30)/       1.0000000D+00/
      DATA ALPAM1( 30)/       1.4845630D+00/
      DATA EISOLA( 30)/     -30.2800160D+00/
      DATA GSSAM1( 30)/      11.8000000D+00/
      DATA GSPAM1( 30)/      11.1820180D+00/
      DATA GPPAM1( 30)/      13.3000000D+00/
      DATA GP2AM1( 30)/      12.9305200D+00/
      DATA HSPAM1( 30)/       0.4846060D+00/
      DATA DDAM1 ( 30)/       1.3581113D+00/
      DATA QQAM1 ( 30)/       1.5457406D+00/
      DATA AMAM1 ( 30)/       0.4336641D+00/
      DATA ADAM1 ( 30)/       0.2317423D+00/
      DATA AQAM1 ( 30)/       0.2621165D+00/
C                    DATA FOR ELEMENT 32        GERMANIUM
      DATA REFAM  (32)/' GE: (AM1): M.J.S.DEWAR AND C.JIE, ORGANOMETALLI
     1CS, 8, 1544, (1989)             '/
      DATA USSAM1( 32)/     -34.1838890D+00/
      DATA UPPAM1( 32)/     -28.6408110D+00/
      DATA BETASA( 32)/      -4.3566070D+00/
      DATA BETAPA( 32)/      -0.9910910D+00/
      DATA ZSAM1 ( 32)/       1.2196310D+00/
      DATA ZPAM1 ( 32)/       1.9827940D+00/
      DATA ALPAM1( 32)/       2.1364050D+00/
      DATA EISOLA( 32)/     -78.7084810D+00/
      DATA GSSAM1( 32)/      10.1686050D+00/
      DATA GSPAM1( 32)/       8.1444730D+00/
      DATA GPPAM1( 32)/       6.6719020D+00/
      DATA GP2AM1( 32)/       6.2697060D+00/
      DATA HSPAM1( 32)/       0.9370930D+00/
      DATA DDAM1 ( 32)/       1.2472095D+00/
      DATA QQAM1 ( 32)/       1.0698642D+00/
      DATA AMAM1 ( 32)/       0.3737084D+00/
      DATA ADAM1 ( 32)/       0.3180309D+00/
      DATA AQAM1 ( 32)/       0.3485612D+00/
C                    DATA FOR ELEMENT 35       AM1:   BROMINE  *
      DATA REFAM  (35)/' BR: (AM1): M.J.S. DEWAR AND E. G. ZOEBISCH, THE
     1OCHEM, 180, 1 (1988).           '/
      DATA USSAM1(35)/    -104.6560630D+00/
      DATA UPPAM1(35)/     -74.9300520D+00/
      DATA BETASA(35)/     -19.3998800D+00/
      DATA BETAPA(35)/      -8.9571950D+00/
      DATA ZSAM1 (35)/       3.0641330D+00/
      DATA ZPAM1 (35)/       2.0383330D+00/
      DATA ZDAM1 (35)/       1.0000000D+00/
      DATA ALPAM1(35)/       2.5765460D+00/
      DATA EISOLA(35)/    -352.3142087D+00/
      DATA GSSAM1(35)/      15.0364395D+00/
      DATA GSPAM1(35)/      13.0346824D+00/
      DATA GPPAM1(35)/      11.2763254D+00/
      DATA GP2AM1(35)/       9.8544255D+00/
      DATA HSPAM1(35)/       2.4558683D+00/
      DATA DDAM1 (35)/       0.8458104D+00/
      DATA QQAM1 (35)/       1.0407133D+00/
      DATA AMAM1 (35)/       0.5526071D+00/
      DATA ADAM1 (35)/       0.6024598D+00/
      DATA AQAM1 (35)/       0.5307555D+00/
      DATA GUESA1(35,1)/       0.0666850D+00/
      DATA GUESA2(35,1)/       4.0000000D+00/
      DATA GUESA3(35,1)/       1.5000000D+00/
      DATA GUESA1(35,2)/       0.0255680D+00/
      DATA GUESA2(35,2)/       4.0000000D+00/
      DATA GUESA3(35,2)/       2.3000000D+00/
C                    DATA FOR ELEMENT 53       AM1:   IODINE  *
      DATA REFAM  (53)/'  I: (AM1): M.J.S. DEWAR AND E. G. ZOEBISCH, THE
     1OCHEM, 180, 1 (1988).           '/
      DATA USSAM1(53)/    -103.5896630D+00/
      DATA UPPAM1(53)/     -74.4299970D+00/
      DATA BETASA(53)/      -8.4433270D+00/
      DATA BETAPA(53)/      -6.3234050D+00/
      DATA ZSAM1 (53)/       2.1028580D+00/
      DATA ZPAM1 (53)/       2.1611530D+00/
      DATA ZDAM1 (53)/       1.0000000D+00/
      DATA ALPAM1(53)/       2.2994240D+00/
      DATA EISOLA(53)/    -346.8642857D+00/
      DATA GSSAM1(53)/      15.0404486D+00/
      DATA GSPAM1(53)/      13.0565580D+00/
      DATA GPPAM1(53)/      11.1477837D+00/
      DATA GP2AM1(53)/       9.9140907D+00/
      DATA HSPAM1(53)/       2.4563820D+00/
      DATA DDAM1 (53)/       1.4878778D+00/
      DATA QQAM1 (53)/       1.1887388D+00/
      DATA AMAM1 (53)/       0.5527544D+00/
      DATA ADAM1 (53)/       0.4497523D+00/
      DATA AQAM1 (53)/       0.4631775D+00/
      DATA GUESA1(53,1)/       0.0043610D+00/
      DATA GUESA2(53,1)/       2.3000000D+00/
      DATA GUESA3(53,1)/       1.8000000D+00/
      DATA GUESA1(53,2)/       0.0157060D+00/
      DATA GUESA2(53,2)/       3.0000000D+00/
      DATA GUESA3(53,2)/       2.2400000D+00/
C                    DATA FOR ELEMENT 80        MERCURY
      DATA REFAM  (80)/' HG: (AM1): M.J.S.DEWAR AND C.JIE, ORGANOMETALLI
     1CS 8, 1547, (1989)              '/
      DATA USSAM1( 80)/     -19.9415780D+00/
      DATA UPPAM1( 80)/     -11.1108700D+00/
      DATA BETASA( 80)/      -0.9086570D+00/
      DATA BETAPA( 80)/      -4.9093840D+00/
      DATA ZSAM1 ( 80)/       2.0364130D+00/
      DATA ZPAM1 ( 80)/       1.9557660D+00/
      DATA ALPAM1( 80)/       1.4847340D+00/
      DATA EISOLA( 80)/     -29.0831560D+00/
      DATA GSSAM1( 80)/      10.8000000D+00/
      DATA GSPAM1( 80)/       9.3000000D+00/
      DATA GPPAM1( 80)/      14.3000000D+00/
      DATA GP2AM1( 80)/      13.5000000D+00/
      DATA HSPAM1( 80)/       1.3000000D+00/
      DATA DDAM1 ( 80)/       1.8750829D+00/
      DATA QQAM1 ( 80)/       1.5424241D+00/
      DATA AMAM1 ( 80)/       0.3969129D+00/
      DATA ADAM1 ( 80)/       0.2926605D+00/
      DATA AQAM1 ( 80)/       0.3360599D+00/
C
C     START OF "OLD" ELEMENTS: THESE ARE OLD PARAMETERS WHICH
C     CAN BE USED, IF DESIRED, BY SPECIFYING "<CHEMICAL SYMBOL>YEAR"
C     AS IN SI1978 OR  S1983.
C
C                    DATA FOR ELEMENT 90        SILICON
      DATA REFAM  (90)/' SI: (MNDO): M.J.S.DEWAR, M.L.MCKEE, H.S.RZEPA,
     1J. AM. CHEM. SOC., 100 3607 1978'/
      DATA USSAM1 (90)/     -40.5682920D+00/
      DATA UPPAM1 (90)/     -28.0891870D+00/
      DATA BETASA (90)/      -4.2562180D+00/
      DATA BETAPA (90)/      -4.2562180D+00/
      DATA ZSAM1  (90)/       1.4353060D+00/
      DATA ZPAM1  (90)/       1.4353060D+00/
      DATA ZDAM1  (90)/       1.0000000D+00/
      DATA ALPAM1  (90)/       2.1961078D+00/
      DATA EISOLA (90)/     -90.5399580D+00/
      DATA DDAM1  (90)/       1.4078712D+00/
      DATA QQAM1  (90)/       1.1658281D+00/
      DATA AMAM1  (90)/       0.3608967D+00/
      DATA ADAM1  (90)/       0.3441817D+00/
      DATA AQAM1  (90)/       0.3999442D+00/
      DATA HSPAM1 (90)/1.32D+00/
      DATA GP2AM1 (90)/6.54D+00/
      DATA GPPAM1 (90)/7.31D+00/
      DATA GSPAM1 (90)/8.36D+00/
      DATA GSSAM1 (90)/9.82D+00/
      DATA REFAM  (91)/'  S: (MNDO): M.J.S.DEWAR, H.S. RZEPA, M.L.MCKEE,
     1 J.AM.CHEM.SOC.100, 3607 (1978).'/
      DATA USSAM1 (91)/     -75.2391520D+00/
      DATA UPPAM1 (91)/     -57.8320130D+00/
      DATA BETASA (91)/     -11.1422310D+00/
      DATA BETAPA (91)/     -11.1422310D+00/
      DATA ZSAM1  (91)/       2.6135910D+00/
      DATA ZPAM1  (91)/       2.0343930D+00/
      DATA ZDAM1  (91)/       1.0000000D+00/
      DATA ALPAM1 (91)/       2.4916445D+00/
      DATA EISOLA (91)/    -235.4413560D+00/
      DATA GSSAM1 (91)/      12.8800000D+00/
      DATA GSPAM1 (91)/      11.2600000D+00/
      DATA GPPAM1 (91)/       9.9000000D+00/
      DATA GP2AM1 (91)/       8.8300000D+00/
      DATA HSPAM1 (91)/       2.2600000D+00/
      DATA DDAM1  (91)/       0.8231596D+00/
      DATA QQAM1  (91)/       0.8225156D+00/
      DATA AMAM1  (91)/       0.4733554D+00/
      DATA ADAM1  (91)/       0.5889395D+00/
      DATA AQAM1  (91)/       0.5632724D+00/
      DATA REFAM (102)/' CB: (AM1):  CAPPED BOND  (HYDROGEN-LIKE, TAKES
     1 ON ZERO CHARGE.)               '/
C                    DATA FOR ELEMENT102
      DATA USSAM1 (102)/     -11.9062760D+00/
      DATA BETASA (102)/-9999999.0000000D+00/
      DATA ZSAM1  (102)/       4.0000000D+00/
      DATA ZPAM1  (102)/       0.3000000D+00/
      DATA ZDAM1  (102)/       0.3000000D+00/
      DATA ALPAM1 (102)/       2.5441341D+00/
      DATA EISOLA (102)/       4.0000000D+00/
      DATA GSSAM1 (102)/      12.8480000D+00/
      DATA HSPAM1 (102)/       0.1000000D+00/
      DATA DDAM1  (102)/       0.0684105D+00/
      DATA QQAM1  (102)/       1.0540926D+00/
      DATA AMAM1  (102)/       0.4721793D+00/
      DATA ADAM1  (102)/       0.9262742D+00/
      DATA AQAM1  (102)/       0.2909059D+00/
C
C     START OF MNDO-PM3 PARAMETER SET
C
C                    DATA FOR ELEMENT  1        HYDROGEN
      DATA REFPM3 ( 1)/'  H: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3(  1)/     -13.0733210D+00/
      DATA UPPPM3(  1)/0.0D+00/
      DATA BETASP(  1)/      -5.6265120D+00/
      DATA BETAPP(  1)/0.0D+00/
      DATA ZSPM3 (  1)/       0.9678070D+00/
      DATA ZPPM3 (  1)/0.0D+00/
      DATA ALPPM3(  1)/       3.3563860D+00/
      DATA EISOLP(  1)/     -13.0733210D+00/
      DATA GSSPM3(  1)/      14.7942080D+00/
      DATA GSPPM3(  1)/0.0D+00/
      DATA GPPPM3(  1)/0.0D+00/
      DATA GP2PM3(  1)/0.0D+00/
      DATA HSPPM3(  1)/0.0D+00/
      DATA AMPM3 (  1)/       0.5437048D+00/
      DATA ADPM3 (  1)/       0.5437048D+00/
      DATA AQPM3 (  1)/       0.5437048D+00/
      DATA GUESP1(  1,1)/       1.1287500D+00/
      DATA GUESP2(  1,1)/       5.0962820D+00/
      DATA GUESP3(  1,1)/       1.5374650D+00/
      DATA GUESP1(  1,2)/      -1.0603290D+00/
      DATA GUESP2(  1,2)/       6.0037880D+00/
      DATA GUESP3(  1,2)/       1.5701890D+00/
C ADD LI PM3 PARAMETERS.
C                    DATA FOR ELEMENT  3        LITHIUM
      DATA REFPM3( 3)/ ' LI: (PM3): E. ANDERS, R. KOCH, P. FREUNSCHT, J.
     *COMPUT.CHEM. 14, 1301 (1993)    '/
      DATA USSPM3(  3)/      -5.3000000D+00/
      DATA UPPPM3(  3)/      -3.4000000D+00/
      DATA BETASP(  3)/      -0.5500000D+00/
      DATA BETAPP(  3)/      -1.5000000D+00/
      DATA ZSPM3 (  3)/       0.6500000D+00/
      DATA ZPPM3 (  3)/       0.7500000D+00/
      DATA ALPPM3(  3)/       1.2550000D+00/
      DATA EISOLP(  3)/      -5.3000000D+00/
      DATA GSSPM3(  3)/       4.5000000D+00/
      DATA GSPPM3(  3)/       3.0000000D+00/
      DATA GPPPM3(  3)/       5.2500000D+00/
      DATA GP2PM3(  3)/       4.5000000D+00/
      DATA HSPPM3(  3)/       0.1500000D+00/
      DATA DDPM3 (  3)/       2.0357652D+00/
      DATA QQPM3 (  3)/       1.6329932D+00/
      DATA AMPM3 (  3)/       0.1653804D+00/
      DATA ADPM3 (  3)/       0.1156738D+00/
      DATA AQPM3 (  3)/       0.3166080D+00/
      DATA GUESP1(  3,1)/      -0.4500000D+00/
      DATA GUESP2(  3,1)/       5.0000000D+00/
      DATA GUESP3(  3,1)/       1.0000000D+00/
      DATA GUESP1(  3,2)/       0.8000000D+00/
      DATA GUESP2(  3,2)/       6.5000000D+00/
      DATA GUESP3(  3,2)/       1.0000000D+00/
C                    DATA FOR ELEMENT  4        BERYLLIUM
      DATA REFPM3( 4)/ ' BE: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3(  4)/     -17.2647520D+00/
      DATA UPPPM3(  4)/     -11.3042430D+00/
      DATA BETASP(  4)/      -3.9620530D+00/
      DATA BETAPP(  4)/      -2.7806840D+00/
      DATA ZSPM3 (  4)/       0.8774390D+00/
      DATA ZPPM3 (  4)/       1.5087550D+00/
      DATA ALPPM3(  4)/       1.5935360D+00/
      DATA EISOLP(  4)/     -25.5166530D+00/
      DATA GSSPM3(  4)/       9.0128510D+00/
      DATA GSPPM3(  4)/       6.5761990D+00/
      DATA GPPPM3(  4)/       6.0571820D+00/
      DATA GP2PM3(  4)/       9.0052190D+00/
      DATA HSPPM3(  4)/       0.5446790D+00/
      DATA DDPM3 (  4)/       1.0090531D+00/
      DATA QQPM3 (  4)/       0.8117586D+00/
      DATA AMPM3 (  4)/       0.3312330D+00/
      DATA ADPM3 (  4)/       0.2908996D+00/
      DATA AQPM3 (  4)/       0.3530008D+00/
      DATA GUESP1(  4,1)/       1.6315720D+00/
      DATA GUESP2(  4,1)/       2.6729620D+00/
      DATA GUESP3(  4,1)/       1.7916860D+00/
      DATA GUESP1(  4,2)/      -2.1109590D+00/
      DATA GUESP2(  4,2)/       1.9685940D+00/
      DATA GUESP3(  4,2)/       1.7558710D+00/
C                    DATA FOR ELEMENT  6        CARBON
      DATA REFPM3 ( 6)/'  C: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3(  6)/     -47.2703200D+00/
      DATA UPPPM3(  6)/     -36.2669180D+00/
      DATA BETASP(  6)/     -11.9100150D+00/
      DATA BETAPP(  6)/      -9.8027550D+00/
      DATA ZSPM3 (  6)/       1.5650850D+00/
      DATA ZPPM3 (  6)/       1.8423450D+00/
      DATA ALPPM3(  6)/       2.7078070D+00/
      DATA EISOLP(  6)/    -111.2299170D+00/
      DATA GSSPM3(  6)/      11.2007080D+00/
      DATA GSPPM3(  6)/      10.2650270D+00/
      DATA GPPPM3(  6)/      10.7962920D+00/
      DATA GP2PM3(  6)/       9.0425660D+00/
      DATA HSPPM3(  6)/       2.2909800D+00/
      DATA DDPM3 (  6)/       0.8332396D+00/
      DATA QQPM3 (  6)/       0.6647750D+00/
      DATA AMPM3 (  6)/       0.4116394D+00/
      DATA ADPM3 (  6)/       0.5885862D+00/
      DATA AQPM3 (  6)/       0.7647667D+00/
      DATA GUESP1(  6,1)/       0.0501070D+00/
      DATA GUESP2(  6,1)/       6.0031650D+00/
      DATA GUESP3(  6,1)/       1.6422140D+00/
      DATA GUESP1(  6,2)/       0.0507330D+00/
      DATA GUESP2(  6,2)/       6.0029790D+00/
      DATA GUESP3(  6,2)/       0.8924880D+00/
C                    DATA FOR ELEMENT  7        NITROGEN
      DATA REFPM3 ( 7)/'  N: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3(  7)/     -49.3356720D+00/
      DATA UPPPM3(  7)/     -47.5097360D+00/
      DATA BETASP(  7)/     -14.0625210D+00/
      DATA BETAPP(  7)/     -20.0438480D+00/
      DATA ZSPM3 (  7)/       2.0280940D+00/
      DATA ZPPM3 (  7)/       2.3137280D+00/
      DATA ALPPM3(  7)/       2.8305450D+00/
      DATA EISOLP(  7)/    -157.6137755D+00/
      DATA GSSPM3(  7)/      11.9047870D+00/
      DATA GSPPM3(  7)/       7.3485650D+00/
      DATA GPPPM3(  7)/      11.7546720D+00/
      DATA GP2PM3(  7)/      10.8072770D+00/
      DATA HSPPM3(  7)/       1.1367130D+00/
      DATA DDPM3 (  7)/       0.6577006D+00/
      DATA QQPM3 (  7)/       0.5293383D+00/
      DATA AMPM3 (  7)/       0.4375151D+00/
      DATA ADPM3 (  7)/       0.5030995D+00/
      DATA AQPM3 (  7)/       0.7364933D+00/
      DATA GUESP1(  7,1)/       1.5016740D+00/
      DATA GUESP2(  7,1)/       5.9011480D+00/
      DATA GUESP3(  7,1)/       1.7107400D+00/
      DATA GUESP1(  7,2)/      -1.5057720D+00/
      DATA GUESP2(  7,2)/       6.0046580D+00/
      DATA GUESP3(  7,2)/       1.7161490D+00/
C                    DATA FOR ELEMENT  8        OXYGEN
      DATA REFPM3 ( 8)/'  O: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3(  8)/     -86.9930020D+00/
      DATA UPPPM3(  8)/     -71.8795800D+00/
      DATA BETASP(  8)/     -45.2026510D+00/
      DATA BETAPP(  8)/     -24.7525150D+00/
      DATA ZSPM3 (  8)/       3.7965440D+00/
      DATA ZPPM3 (  8)/       2.3894020D+00/
      DATA ALPPM3(  8)/       3.2171020D+00/
      DATA EISOLP(  8)/    -289.3422065D+00/
      DATA GSSPM3(  8)/      15.7557600D+00/
      DATA GSPPM3(  8)/      10.6211600D+00/
      DATA GPPPM3(  8)/      13.6540160D+00/
      DATA GP2PM3(  8)/      12.4060950D+00/
      DATA HSPPM3(  8)/       0.5938830D+00/
      DATA DDPM3 (  8)/       0.4086173D+00/
      DATA QQPM3 (  8)/       0.5125738D+00/
      DATA AMPM3 (  8)/       0.5790430D+00/
      DATA ADPM3 (  8)/       0.5299630D+00/
      DATA AQPM3 (  8)/       0.8179630D+00/
      DATA GUESP1(  8,1)/      -1.1311280D+00/
      DATA GUESP2(  8,1)/       6.0024770D+00/
      DATA GUESP3(  8,1)/       1.6073110D+00/
      DATA GUESP1(  8,2)/       1.1378910D+00/
      DATA GUESP2(  8,2)/       5.9505120D+00/
      DATA GUESP3(  8,2)/       1.5983950D+00/
C                    DATA FOR ELEMENT  9        FLUORINE
      DATA REFPM3 ( 9)/'  F: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3(  9)/    -110.4353030D+00/
      DATA UPPPM3(  9)/    -105.6850470D+00/
      DATA BETASP(  9)/     -48.4059390D+00/
      DATA BETAPP(  9)/     -27.7446600D+00/
      DATA ZSPM3 (  9)/       4.7085550D+00/
      DATA ZPPM3 (  9)/       2.4911780D+00/
      DATA ALPPM3(  9)/       3.3589210D+00/
      DATA EISOLP(  9)/    -437.5171690D+00/
      DATA GSSPM3(  9)/      10.4966670D+00/
      DATA GSPPM3(  9)/      16.0736890D+00/
      DATA GPPPM3(  9)/      14.8172560D+00/
      DATA GP2PM3(  9)/      14.4183930D+00/
      DATA HSPPM3(  9)/       0.7277630D+00/
      DATA DDPM3 (  9)/       0.3125302D+00/
      DATA QQPM3 (  9)/       0.4916328D+00/
      DATA AMPM3 (  9)/       0.3857650D+00/
      DATA ADPM3 (  9)/       0.6768503D+00/
      DATA AQPM3 (  9)/       0.6120047D+00/
      DATA GUESP1(  9,1)/      -0.0121660D+00/
      DATA GUESP2(  9,1)/       6.0235740D+00/
      DATA GUESP3(  9,1)/       1.8568590D+00/
      DATA GUESP1(  9,2)/      -0.0028520D+00/
      DATA GUESP2(  9,2)/       6.0037170D+00/
      DATA GUESP3(  9,2)/       2.6361580D+00/
C
C                    DATA FOR ELEMENT 11        SODIUM
      DATA REFPM3(11)/ ' NA: (PM3): E. N. BROTHERS, K. M. MERZ JR., J.PH
     *YS.CHEM. B 106, 2779 (2002)     '/
      DATA USSPM3( 11)/      -5.2945929D+00/
      DATA UPPPM3( 11)/      -2.4596564D+00/
      DATA BETASP( 11)/      -0.1510870D+00/
      DATA BETAPP( 11)/      -0.2184096D+00/
      DATA ZSPM3 ( 11)/       1.1375011D+00/
      DATA ZPPM3 ( 11)/       1.1877433D+00/
      DATA ALPPM3( 11)/       2.3677169D+00/
      DATA EISOLP( 11)/      -5.2945929D+00/
      DATA GSSPM3( 11)/       3.9558692D+00/
      DATA GSPPM3( 11)/       7.1929109D+00/
      DATA GPPPM3( 11)/       5.3363963D+00/
      DATA GP2PM3( 11)/       5.0588074D+00/
      DATA HSPPM3( 11)/       0.5687889D+00/
      DATA DDPM3 ( 11)/       1.7352377D+00/
      DATA QQPM3 ( 11)/       1.4088230D+00/
      DATA AMPM3 ( 11)/       0.1453829D+00/
      DATA ADPM3 ( 11)/       0.2121063D+00/
      DATA AQPM3 ( 11)/       0.2575829D+00/
      DATA GUESP1( 11,1)/       0.6433655D+00/
      DATA GUESP2( 11,1)/       1.5465054D+00/
      DATA GUESP3( 11,1)/       0.9976699D+00/
      DATA GUESP1( 11,2)/       1.0871788D+00/
      DATA GUESP2( 11,2)/       1.4529000D+00/
      DATA GUESP3( 11,2)/       1.4506099D+00/
C                    DATA FOR ELEMENT 12        MAGNESIUM
      DATA REFPM3(12)/ ' MG: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 12)/     -14.6236880D+00/
      DATA UPPPM3( 12)/     -14.1734600D+00/
      DATA BETASP( 12)/      -2.0716910D+00/
      DATA BETAPP( 12)/      -0.5695810D+00/
      DATA ZSPM3 ( 12)/       0.6985520D+00/
      DATA ZPPM3 ( 12)/       1.4834530D+00/
      DATA ALPPM3( 12)/       1.3291470D+00/
      DATA EISOLP( 12)/     -22.5530760D+00/
      DATA GSSPM3( 12)/       6.6943000D+00/
      DATA GSPPM3( 12)/       6.7939950D+00/
      DATA GPPPM3( 12)/       6.9104460D+00/
      DATA GP2PM3( 12)/       7.0908230D+00/
      DATA HSPPM3( 12)/       0.5433000D+00/
      DATA DDPM3 ( 12)/       1.1403950D+00/
      DATA QQPM3 ( 12)/       1.1279899D+00/
      DATA AMPM3 ( 12)/       0.2460235D+00/
      DATA ADPM3 ( 12)/       0.2695751D+00/
      DATA AQPM3 ( 12)/       0.2767522D+00/
      DATA GUESP1( 12,1)/       2.1170500D+00/
      DATA GUESP2( 12,1)/       6.0094770D+00/
      DATA GUESP3( 12,1)/       2.0844060D+00/
      DATA GUESP1( 12,2)/      -2.5477670D+00/
      DATA GUESP2( 12,2)/       4.3953700D+00/
      DATA GUESP3( 12,2)/       2.0636740D+00/
C                    DATA FOR ELEMENT 13        ALUMINUM
      DATA REFPM3 (13)/' AL: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3( 13)/     -24.8454040D+00/
      DATA UPPPM3( 13)/     -22.2641590D+00/
      DATA BETASP( 13)/      -0.5943010D+00/
      DATA BETAPP( 13)/      -0.9565500D+00/
      DATA ZSPM3 ( 13)/       1.7028880D+00/
      DATA ZPPM3 ( 13)/       1.0736290D+00/
      DATA ZDPM3 ( 13)/       1.0000000D+00/
      DATA ALPPM3( 13)/       1.5217030D+00/
      DATA EISOLP( 13)/     -46.8647630D+00/
      DATA GSSPM3( 13)/       5.7767370D+00/
      DATA GSPPM3( 13)/      11.6598560D+00/
      DATA GPPPM3( 13)/       6.3477900D+00/
      DATA GP2PM3( 13)/       6.1210770D+00/
      DATA HSPPM3( 13)/       4.0062450D+00/
      DATA DDPM3 ( 13)/       1.2102799D+00/
      DATA QQPM3 ( 13)/       1.5585645D+00/
      DATA AMPM3 ( 13)/       0.2123020D+00/
      DATA ADPM3 ( 13)/       0.6418584D+00/
      DATA AQPM3 ( 13)/       0.2262838D+00/
      DATA GUESP1( 13,1)/      -0.4730900D+00/
      DATA GUESP2( 13,1)/       1.9158250D+00/
      DATA GUESP3( 13,1)/       1.4517280D+00/
      DATA GUESP1( 13,2)/      -0.1540510D+00/
      DATA GUESP2( 13,2)/       6.0050860D+00/
      DATA GUESP3( 13,2)/       2.5199970D+00/
C                    DATA FOR ELEMENT 14        SILICON
      DATA REFPM3 (14)/' SI: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3( 14)/     -26.7634830D+00/
      DATA UPPPM3( 14)/     -22.8136350D+00/
      DATA BETASP( 14)/      -2.8621450D+00/
      DATA BETAPP( 14)/      -3.9331480D+00/
      DATA ZSPM3 ( 14)/       1.6350750D+00/
      DATA ZPPM3 ( 14)/       1.3130880D+00/
      DATA ZDPM3 ( 14)/       1.0000000D+00/
      DATA ALPPM3( 14)/       2.1358090D+00/
      DATA EISOLP( 14)/     -67.7882140D+00/
      DATA GSSPM3( 14)/       5.0471960D+00/
      DATA GSPPM3( 14)/       5.9490570D+00/
      DATA GPPPM3( 14)/       6.7593670D+00/
      DATA GP2PM3( 14)/       5.1612970D+00/
      DATA HSPPM3( 14)/       0.9198320D+00/
      DATA DDPM3 ( 14)/       1.3144550D+00/
      DATA QQPM3 ( 14)/       1.2743396D+00/
      DATA AMPM3 ( 14)/       0.1854905D+00/
      DATA ADPM3 ( 14)/       0.3060715D+00/
      DATA AQPM3 ( 14)/       0.4877432D+00/
      DATA GUESP1( 14,1)/      -0.3906000D+00/
      DATA GUESP2( 14,1)/       6.0000540D+00/
      DATA GUESP3( 14,1)/       0.6322620D+00/
      DATA GUESP1( 14,2)/       0.0572590D+00/
      DATA GUESP2( 14,2)/       6.0071830D+00/
      DATA GUESP3( 14,2)/       2.0199870D+00/
C                    DATA FOR ELEMENT 15        PHOSPHORUS
      DATA REFPM3 (15)/'  P: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3( 15)/     -40.4130960D+00/
      DATA UPPPM3( 15)/     -29.5930520D+00/
      DATA BETASP( 15)/     -12.6158790D+00/
      DATA BETAPP( 15)/      -4.1600400D+00/
      DATA ZSPM3 ( 15)/       2.0175630D+00/
      DATA ZPPM3 ( 15)/       1.5047320D+00/
      DATA ZDPM3 ( 15)/       1.0000000D+00/
      DATA ALPPM3( 15)/       1.9405340D+00/
      DATA EISOLP( 15)/    -117.9591740D+00/
      DATA GSSPM3( 15)/       7.8016150D+00/
      DATA GSPPM3( 15)/       5.1869490D+00/
      DATA GPPPM3( 15)/       6.6184780D+00/
      DATA GP2PM3( 15)/       6.0620020D+00/
      DATA HSPPM3( 15)/       1.5428090D+00/
      DATA DDPM3 ( 15)/       1.0644947D+00/
      DATA QQPM3 ( 15)/       1.1120386D+00/
      DATA AMPM3 ( 15)/       0.2867187D+00/
      DATA ADPM3 ( 15)/       0.4309446D+00/
      DATA AQPM3 ( 15)/       0.3732517D+00/
      DATA GUESP1( 15,1)/      -0.6114210D+00/
      DATA GUESP2( 15,1)/       1.9972720D+00/
      DATA GUESP3( 15,1)/       0.7946240D+00/
      DATA GUESP1( 15,2)/      -0.0939350D+00/
      DATA GUESP2( 15,2)/       1.9983600D+00/
      DATA GUESP3( 15,2)/       1.9106770D+00/
C                    DATA FOR ELEMENT 16        SULFUR
      DATA REFPM3 (16)/'  S: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3( 16)/     -49.8953710D+00/
      DATA UPPPM3( 16)/     -44.3925830D+00/
      DATA BETASP( 16)/      -8.8274650D+00/
      DATA BETAPP( 16)/      -8.0914150D+00/
      DATA ZSPM3 ( 16)/       1.8911850D+00/
      DATA ZPPM3 ( 16)/       1.6589720D+00/
      DATA ZDPM3 ( 16)/       1.0000000D+00/
      DATA ALPPM3( 16)/       2.2697060D+00/
      DATA EISOLP( 16)/    -183.4537395D+00/
      DATA GSSPM3( 16)/       8.9646670D+00/
      DATA GSPPM3( 16)/       6.7859360D+00/
      DATA GPPPM3( 16)/       9.9681640D+00/
      DATA GP2PM3( 16)/       7.9702470D+00/
      DATA HSPPM3( 16)/       4.0418360D+00/
      DATA DDPM3 ( 16)/       1.1214313D+00/
      DATA QQPM3 ( 16)/       1.0086488D+00/
      DATA AMPM3 ( 16)/       0.3294622D+00/
      DATA ADPM3 ( 16)/       0.6679118D+00/
      DATA AQPM3 ( 16)/       0.6137472D+00/
      DATA GUESP1( 16,1)/      -0.3991910D+00/
      DATA GUESP2( 16,1)/       6.0006690D+00/
      DATA GUESP3( 16,1)/       0.9621230D+00/
      DATA GUESP1( 16,2)/      -0.0548990D+00/
      DATA GUESP2( 16,2)/       6.0018450D+00/
      DATA GUESP3( 16,2)/       1.5799440D+00/
C                    DATA FOR ELEMENT 17        CHLORINE
      DATA REFPM3 (17)/' CL: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3( 17)/    -100.6267470D+00/
      DATA UPPPM3( 17)/     -53.6143960D+00/
      DATA BETASP( 17)/     -27.5285600D+00/
      DATA BETAPP( 17)/     -11.5939220D+00/
      DATA ZSPM3 ( 17)/       2.2462100D+00/
      DATA ZPPM3 ( 17)/       2.1510100D+00/
      DATA ZDPM3 ( 17)/       1.0000000D+00/
      DATA ALPPM3( 17)/       2.5172960D+00/
      DATA EISOLP( 17)/    -315.1949480D+00/
      DATA GSSPM3( 17)/      16.0136010D+00/
      DATA GSPPM3( 17)/       8.0481150D+00/
      DATA GPPPM3( 17)/       7.5222150D+00/
      DATA GP2PM3( 17)/       7.5041540D+00/
      DATA HSPPM3( 17)/       3.4811530D+00/
      DATA DDPM3 ( 17)/       0.9175856D+00/
      DATA QQPM3 ( 17)/       0.7779230D+00/
      DATA AMPM3 ( 17)/       0.5885190D+00/
      DATA ADPM3 ( 17)/       0.6814522D+00/
      DATA AQPM3 ( 17)/       0.3643694D+00/
      DATA GUESP1( 17,1)/      -0.1715910D+00/
      DATA GUESP2( 17,1)/       6.0008020D+00/
      DATA GUESP3( 17,1)/       1.0875020D+00/
      DATA GUESP1( 17,2)/      -0.0134580D+00/
      DATA GUESP2( 17,2)/       1.9666180D+00/
      DATA GUESP3( 17,2)/       2.2928910D+00/
C GNM, ADD EXPT'L K PM3 PARAMETERS.
C                    DATA FOR ELEMENT 19        POTASSIUM
      DATA REFPM3(19)/ '  K: (PM3): BROTHERS ET AL.
     *                                '/
      DATA USSPM3( 19)/      -3.7822395D+00/
      DATA UPPPM3( 19)/      -3.0421288D+00/
      DATA BETASP( 19)/      -0.3487847D+00/
      DATA BETAPP( 19)/      -0.9835604D+00/
      DATA ZSPM3 ( 19)/       0.8069458D+00/
      DATA ZPPM3 ( 19)/       1.3979828D+00/
      DATA ALPPM3( 19)/       2.9774501D+00/
      DATA EISOLP( 19)/      -3.7822395D+00/
      DATA GSSPM3( 19)/       3.0745283D+00/
      DATA GSPPM3( 19)/       3.6069514D+00/
      DATA GPPPM3( 19)/       4.8400002D+00/
      DATA GP2PM3( 19)/       3.8591930D+00/
      DATA HSPPM3( 19)/       0.3409232D+00/
      DATA DDPM3 ( 19)/       1.6848623D+00/
      DATA QQPM3 ( 19)/       1.5174152D+00/
      DATA AMPM3 ( 19)/       0.1129926D+00/
      DATA ADPM3 ( 19)/       0.1759227D+00/
      DATA AQPM3 ( 19)/       0.3647615D+00/
      DATA GUESP1( 19,1)/       0.6526527D+00/
      DATA GUESP2( 19,1)/       0.5107622D+00/
      DATA GUESP3( 19,1)/       1.6872750D+00/
      DATA GUESP1( 19,2)/       0.9482869D+00/
      DATA GUESP2( 19,2)/       4.0873287D+00/
      DATA GUESP3( 19,2)/       1.7513510D+00/
C GNM, ADD EXPT'L CA PM3 PARAMETERS.
C                    DATA FOR ELEMENT 20        CALCIUM
      DATA REFPM3(20)/ ' CA: (PM3): BROTHERS ET AL.
     *                                '/
      DATA USSPM3( 20)/     -10.2071218D+00/
      DATA UPPPM3( 20)/      -7.7810750D+00/
      DATA BETASP( 20)/      -0.1623934D+00/
      DATA BETAPP( 20)/      -0.1973349D+00/
      DATA ZSPM3 ( 20)/       1.3269934D+00/
      DATA ZPPM3 ( 20)/       1.1934961D+00/
      DATA ALPPM3( 20)/       4.7429384D+00/
      DATA EISOLP( 20)/     -16.8026842D+00/
      DATA GSSPM3( 20)/       3.6115593D+00/
      DATA GSPPM3( 20)/       2.8617753D+00/
      DATA GPPPM3( 20)/       2.2438292D+00/
      DATA GP2PM3( 20)/       0.9614178D+00/
      DATA HSPPM3( 20)/       0.3280651D+00/
      DATA DDPM3 ( 20)/       2.0356677D+00/
      DATA QQPM3 ( 20)/       1.7774004D+00/
      DATA AMPM3 ( 20)/       0.1327291D+00/
      DATA ADPM3 ( 20)/       0.1541388D+00/
      DATA AQPM3 ( 20)/       0.3653424D+00/
      DATA GUESP1( 20,1)/       0.4319048D+00/
      DATA GUESP2( 20,1)/       0.6140009D+00/
      DATA GUESP3( 20,1)/       1.3019130D+00/
      DATA GUESP1( 20,2)/       1.3092636D+00/
      DATA GUESP2( 20,2)/       2.0757594D+00/
      DATA GUESP3( 20,2)/       0.9859522D+00/
C                    DATA FOR ELEMENT 30        ZINC
      DATA REFPM3(30)/ ' ZN: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 30)/     -18.5321980D+00/
      DATA UPPPM3( 30)/     -11.0474090D+00/
      DATA BETASP( 30)/      -0.7155780D+00/
      DATA BETAPP( 30)/      -6.3518640D+00/
      DATA ZSPM3 ( 30)/       1.8199890D+00/
      DATA ZPPM3 ( 30)/       1.5069220D+00/
      DATA ZDPM3 ( 30)/       1.0000000D+00/
      DATA ALPPM3( 30)/       1.3501260D+00/
      DATA EISOLP( 30)/     -27.3872000D+00/
      DATA GSSPM3( 30)/       9.6771960D+00/
      DATA GSPPM3( 30)/       7.7362040D+00/
      DATA GPPPM3( 30)/       4.9801740D+00/
      DATA GP2PM3( 30)/       4.6696560D+00/
      DATA HSPPM3( 30)/       0.6004130D+00/
      DATA DDPM3 ( 30)/       1.5005758D+00/
      DATA QQPM3 ( 30)/       1.4077174D+00/
      DATA AMPM3 ( 30)/       0.3556485D+00/
      DATA ADPM3 ( 30)/       0.2375689D+00/
      DATA AQPM3 ( 30)/       0.2661069D+00/
      DATA GUESP1( 30,1)/      -0.1112340D+00/
      DATA GUESP2( 30,1)/       6.0014780D+00/
      DATA GUESP3( 30,1)/       1.5160320D+00/
      DATA GUESP1( 30,2)/      -0.1323700D+00/
      DATA GUESP2( 30,2)/       1.9958390D+00/
      DATA GUESP3( 30,2)/       2.5196420D+00/
C                    DATA FOR ELEMENT 31        GALLIUM
      DATA REFPM3(31)/ ' GA: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 31)/     -29.8555930D+00/
      DATA UPPPM3( 31)/     -21.8753710D+00/
      DATA BETASP( 31)/      -4.9456180D+00/
      DATA BETAPP( 31)/      -0.4070530D+00/
      DATA ZSPM3 ( 31)/       1.8470400D+00/
      DATA ZPPM3 ( 31)/       0.8394110D+00/
      DATA ALPPM3( 31)/       1.6051150D+00/
      DATA EISOLP( 31)/     -57.3280250D+00/
      DATA GSSPM3( 31)/       8.4585540D+00/
      DATA GSPPM3( 31)/       8.9256190D+00/
      DATA GPPPM3( 31)/       5.0868550D+00/
      DATA GP2PM3( 31)/       4.9830450D+00/
      DATA HSPPM3( 31)/       2.0512600D+00/
      DATA DDPM3 ( 31)/       0.9776692D+00/
      DATA QQPM3 ( 31)/       2.5271534D+00/
      DATA AMPM3 ( 31)/       0.3108620D+00/
      DATA ADPM3 ( 31)/       0.5129360D+00/
      DATA AQPM3 ( 31)/       0.1546208D+00/
      DATA GUESP1( 31,1)/      -0.5601790D+00/
      DATA GUESP2( 31,1)/       5.6232730D+00/
      DATA GUESP3( 31,1)/       1.5317800D+00/
      DATA GUESP1( 31,2)/      -0.2727310D+00/
      DATA GUESP2( 31,2)/       1.9918430D+00/
      DATA GUESP3( 31,2)/       2.1838640D+00/
C                    DATA FOR ELEMENT 32        GERMANIUM
      DATA REFPM3(32)/ ' GE: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 32)/     -35.4671955D+00/
      DATA UPPPM3( 32)/     -31.5863583D+00/
      DATA BETASP( 32)/      -5.3250024D+00/
      DATA BETAPP( 32)/      -2.2501567D+00/
      DATA ZSPM3 ( 32)/       2.2373526D+00/
      DATA ZPPM3 ( 32)/       1.5924319D+00/
      DATA ALPPM3( 32)/       1.9723370D+00/
      DATA EISOLP( 32)/     -84.0156006D+00/
      DATA GSSPM3( 32)/       5.3769635D+00/
      DATA GSPPM3( 32)/      10.2095293D+00/
      DATA GPPPM3( 32)/       7.6718647D+00/
      DATA GP2PM3( 32)/       6.9242663D+00/
      DATA HSPPM3( 32)/       1.3370204D+00/
      DATA DDPM3 ( 32)/       1.1920304D+00/
      DATA QQPM3 ( 32)/       1.3321263D+00/
      DATA AMPM3 ( 32)/       0.1976098D+00/
      DATA ADPM3 ( 32)/       0.3798182D+00/
      DATA AQPM3 ( 32)/       0.3620669D+00/
      DATA GUESP1( 32,1)/       0.9631726D+00/
      DATA GUESP2( 32,1)/       6.0120134D+00/
      DATA GUESP3( 32,1)/       2.1633655D+00/
      DATA GUESP1( 32,2)/      -0.9593891D+00/
      DATA GUESP2( 32,2)/       5.7491802D+00/
      DATA GUESP3( 32,2)/       2.1693724D+00/
C                    DATA FOR ELEMENT 33        ARSENIC
      DATA REFPM3(33)/ ' AS: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 33)/     -38.5074240D+00/
      DATA UPPPM3( 33)/     -35.1524150D+00/
      DATA BETASP( 33)/      -8.2321650D+00/
      DATA BETAPP( 33)/      -5.0173860D+00/
      DATA ZSPM3 ( 33)/       2.6361770D+00/
      DATA ZPPM3 ( 33)/       1.7038890D+00/
      DATA ALPPM3( 33)/       1.7944770D+00/
      DATA EISOLP( 33)/    -122.6326140D+00/
      DATA GSSPM3( 33)/       8.7890010D+00/
      DATA GSPPM3( 33)/       5.3979830D+00/
      DATA GPPPM3( 33)/       8.2872500D+00/
      DATA GP2PM3( 33)/       8.2103460D+00/
      DATA HSPPM3( 33)/       1.9510340D+00/
      DATA DDPM3 ( 33)/       0.9679655D+00/
      DATA QQPM3 ( 33)/       1.2449874D+00/
      DATA AMPM3 ( 33)/       0.3230063D+00/
      DATA ADPM3 ( 33)/       0.5042239D+00/
      DATA AQPM3 ( 33)/       0.2574219D+00/
      DATA GUESP1( 33,1)/      -0.4600950D+00/
      DATA GUESP2( 33,1)/       1.9831150D+00/
      DATA GUESP3( 33,1)/       1.0867930D+00/
      DATA GUESP1( 33,2)/      -0.0889960D+00/
      DATA GUESP2( 33,2)/       1.9929440D+00/
      DATA GUESP3( 33,2)/       2.1400580D+00/
C                    DATA FOR ELEMENT 34        SELENIUM
      DATA REFPM3(34)/ ' SE: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 34)/     -55.3781350D+00/
      DATA UPPPM3( 34)/     -49.8230760D+00/
      DATA BETASP( 34)/      -6.1578220D+00/
      DATA BETAPP( 34)/      -5.4930390D+00/
      DATA ZSPM3 ( 34)/       2.8280510D+00/
      DATA ZPPM3 ( 34)/       1.7325360D+00/
      DATA ALPPM3( 34)/       3.0439570D+00/
      DATA EISOLP( 34)/    -192.7748115D+00/
      DATA GSSPM3( 34)/       7.4325910D+00/
      DATA GSPPM3( 34)/      10.0604610D+00/
      DATA GPPPM3( 34)/       9.5683260D+00/
      DATA GP2PM3( 34)/       7.7242890D+00/
      DATA HSPPM3( 34)/       4.0165580D+00/
      DATA DDPM3 ( 34)/       0.8719813D+00/
      DATA QQPM3 ( 34)/       1.2244019D+00/
      DATA AMPM3 ( 34)/       0.2731566D+00/
      DATA ADPM3 ( 34)/       0.7509697D+00/
      DATA AQPM3 ( 34)/       0.5283737D+00/
      DATA GUESP1( 34,1)/       0.0478730D+00/
      DATA GUESP2( 34,1)/       6.0074000D+00/
      DATA GUESP3( 34,1)/       2.0817170D+00/
      DATA GUESP1( 34,2)/       0.1147200D+00/
      DATA GUESP2( 34,2)/       6.0086720D+00/
      DATA GUESP3( 34,2)/       1.5164230D+00/
C                    DATA FOR ELEMENT 35        BROMINE
      DATA REFPM3 (35)/' BR: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3( 35)/    -116.6193110D+00/
      DATA UPPPM3( 35)/     -74.2271290D+00/
      DATA BETASP( 35)/     -31.1713420D+00/
      DATA BETAPP( 35)/      -6.8140130D+00/
      DATA ZSPM3 ( 35)/       5.3484570D+00/
      DATA ZPPM3 ( 35)/       2.1275900D+00/
      DATA ZDPM3 ( 35)/       1.0000000D+00/
      DATA ALPPM3( 35)/       2.5118420D+00/
      DATA EISOLP( 35)/    -352.5398970D+00/
      DATA GSSPM3( 35)/      15.9434250D+00/
      DATA GSPPM3( 35)/      16.0616800D+00/
      DATA GPPPM3( 35)/       8.2827630D+00/
      DATA GP2PM3( 35)/       7.8168490D+00/
      DATA HSPPM3( 35)/       0.5788690D+00/
      DATA DDPM3 ( 35)/       0.2759025D+00/
      DATA QQPM3 ( 35)/       0.9970532D+00/
      DATA AMPM3 ( 35)/       0.5859399D+00/
      DATA ADPM3 ( 35)/       0.6755383D+00/
      DATA AQPM3 ( 35)/       0.3823719D+00/
      DATA GUESP1( 35,1)/       0.9604580D+00/
      DATA GUESP2( 35,1)/       5.9765080D+00/
      DATA GUESP3( 35,1)/       2.3216540D+00/
      DATA GUESP1( 35,2)/      -0.9549160D+00/
      DATA GUESP2( 35,2)/       5.9447030D+00/
      DATA GUESP3( 35,2)/       2.3281420D+00/
C                    DATA FOR ELEMENT 48        CADMIUM
      DATA REFPM3(48)/ ' CD: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 48)/     -15.8285840D+00/
      DATA UPPPM3( 48)/       8.7497950D+00/
      DATA BETASP( 48)/      -8.5819440D+00/
      DATA BETAPP( 48)/      -0.6010340D+00/
      DATA ZSPM3 ( 48)/       1.6793510D+00/
      DATA ZPPM3 ( 48)/       2.0664120D+00/
      DATA ALPPM3( 48)/       1.5253820D+00/
      DATA EISOLP( 48)/     -22.4502080D+00/
      DATA GSSPM3( 48)/       9.2069600D+00/
      DATA GSPPM3( 48)/       8.2315390D+00/
      DATA GPPPM3( 48)/       4.9481040D+00/
      DATA GP2PM3( 48)/       4.6696560D+00/
      DATA HSPPM3( 48)/       1.6562340D+00/
      DATA DDPM3 ( 48)/       1.5982681D+00/
      DATA QQPM3 ( 48)/       1.2432402D+00/
      DATA AMPM3 ( 48)/       0.3383668D+00/
      DATA ADPM3 ( 48)/       0.3570290D+00/
      DATA AQPM3 ( 48)/       0.2820582D+00/
C                    DATA FOR ELEMENT 49        INDIUM
      DATA REFPM3(49)/ ' IN: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 49)/     -26.1762050D+00/
      DATA UPPPM3( 49)/     -20.0058220D+00/
      DATA BETASP( 49)/      -2.9933190D+00/
      DATA BETAPP( 49)/      -1.8289080D+00/
      DATA ZSPM3 ( 49)/       2.0161160D+00/
      DATA ZPPM3 ( 49)/       1.4453500D+00/
      DATA ALPPM3( 49)/       1.4183850D+00/
      DATA EISOLP( 49)/     -51.9750470D+00/
      DATA GSSPM3( 49)/       6.5549000D+00/
      DATA GSPPM3( 49)/       8.2298730D+00/
      DATA GPPPM3( 49)/       6.2992690D+00/
      DATA GP2PM3( 49)/       4.9842110D+00/
      DATA HSPPM3( 49)/       2.6314610D+00/
      DATA DDPM3 ( 49)/       1.5766241D+00/
      DATA QQPM3 ( 49)/       1.7774563D+00/
      DATA AMPM3 ( 49)/       0.2409004D+00/
      DATA ADPM3 ( 49)/       0.4532655D+00/
      DATA AQPM3 ( 49)/       0.3689812D+00/
      DATA GUESP1( 49,1)/      -0.3431380D+00/
      DATA GUESP2( 49,1)/       1.9940340D+00/
      DATA GUESP3( 49,1)/       1.6255160D+00/
      DATA GUESP1( 49,2)/      -0.1095320D+00/
      DATA GUESP2( 49,2)/       5.6832170D+00/
      DATA GUESP3( 49,2)/       2.8670090D+00/
C                    DATA FOR ELEMENT 50        TIN
      DATA REFPM3(50)/ ' SN: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 50)/     -34.5501920D+00/
      DATA UPPPM3( 50)/     -25.8944190D+00/
      DATA BETASP( 50)/      -2.7858020D+00/
      DATA BETAPP( 50)/      -2.0059990D+00/
      DATA ZSPM3 ( 50)/       2.3733280D+00/
      DATA ZPPM3 ( 50)/       1.6382330D+00/
      DATA ALPPM3( 50)/       1.6996500D+00/
      DATA EISOLP( 50)/     -78.8877790D+00/
      DATA GSSPM3( 50)/      10.1900330D+00/
      DATA GSPPM3( 50)/       7.2353270D+00/
      DATA GPPPM3( 50)/       5.6738100D+00/
      DATA GP2PM3( 50)/       5.1822140D+00/
      DATA HSPPM3( 50)/       1.0331570D+00/
      DATA DDPM3 ( 50)/       1.3120038D+00/
      DATA QQPM3 ( 50)/       1.5681814D+00/
      DATA AMPM3 ( 50)/       0.3744959D+00/
      DATA ADPM3 ( 50)/       0.3218163D+00/
      DATA AQPM3 ( 50)/       0.2832529D+00/
      DATA GUESP1( 50,1)/      -0.1503530D+00/
      DATA GUESP2( 50,1)/       6.0056940D+00/
      DATA GUESP3( 50,1)/       1.7046420D+00/
      DATA GUESP1( 50,2)/      -0.0444170D+00/
      DATA GUESP2( 50,2)/       2.2573810D+00/
      DATA GUESP3( 50,2)/       2.4698690D+00/
C                    DATA FOR ELEMENT 51        ANTIMONY
      DATA REFPM3(51)/ ' SB: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 51)/     -56.4321960D+00/
      DATA UPPPM3( 51)/     -29.4349540D+00/
      DATA BETASP( 51)/     -14.7942170D+00/
      DATA BETAPP( 51)/      -2.8179480D+00/
      DATA ZSPM3 ( 51)/       2.3430390D+00/
      DATA ZPPM3 ( 51)/       1.8999920D+00/
      DATA ALPPM3( 51)/       2.0343010D+00/
      DATA EISOLP( 51)/    -148.9382890D+00/
      DATA GSSPM3( 51)/       9.2382770D+00/
      DATA GSPPM3( 51)/       5.2776800D+00/
      DATA GPPPM3( 51)/       6.3500000D+00/
      DATA GP2PM3( 51)/       6.2500000D+00/
      DATA HSPPM3( 51)/       2.4244640D+00/
      DATA DDPM3 ( 51)/       1.4091903D+00/
      DATA QQPM3 ( 51)/       1.3521354D+00/
      DATA AMPM3 ( 51)/       0.3395177D+00/
      DATA ADPM3 ( 51)/       0.4589010D+00/
      DATA AQPM3 ( 51)/       0.2423472D+00/
      DATA GUESP1( 51,1)/       3.0020280D+00/
      DATA GUESP2( 51,1)/       6.0053420D+00/
      DATA GUESP3( 51,1)/       0.8530600D+00/
      DATA GUESP1( 51,2)/      -0.0188920D+00/
      DATA GUESP2( 51,2)/       6.0114780D+00/
      DATA GUESP3( 51,2)/       2.7933110D+00/
C                    DATA FOR ELEMENT 52        TELLURIUM
      DATA REFPM3(52)/ ' TE: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 52)/     -44.9380360D+00/
      DATA UPPPM3( 52)/     -46.3140990D+00/
      DATA BETASP( 52)/      -2.6651460D+00/
      DATA BETAPP( 52)/      -3.8954300D+00/
      DATA ZSPM3 ( 52)/       4.1654920D+00/
      DATA ZPPM3 ( 52)/       1.6475550D+00/
      DATA ALPPM3( 52)/       2.4850190D+00/
      DATA EISOLP( 52)/    -168.0945925D+00/
      DATA GSSPM3( 52)/      10.2550730D+00/
      DATA GSPPM3( 52)/       8.1691450D+00/
      DATA GPPPM3( 52)/       7.7775920D+00/
      DATA GP2PM3( 52)/       7.7551210D+00/
      DATA HSPPM3( 52)/       3.7724620D+00/
      DATA DDPM3 ( 52)/       0.3484177D+00/
      DATA QQPM3 ( 52)/       1.5593085D+00/
      DATA AMPM3 ( 52)/       0.3768862D+00/
      DATA ADPM3 ( 52)/       1.1960743D+00/
      DATA AQPM3 ( 52)/       0.2184786D+00/
      DATA GUESP1( 52,1)/       0.0333910D+00/
      DATA GUESP2( 52,1)/       5.9563790D+00/
      DATA GUESP3( 52,1)/       2.2775750D+00/
      DATA GUESP1( 52,2)/      -1.9218670D+00/
      DATA GUESP2( 52,2)/       4.9732190D+00/
      DATA GUESP3( 52,2)/       0.5242430D+00/
C                    DATA FOR ELEMENT 53        IODINE
      DATA REFPM3 (53)/'  I: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1 10, 209 (1989).                '/
      DATA USSPM3( 53)/     -96.4540370D+00/
      DATA UPPPM3( 53)/     -61.0915820D+00/
      DATA BETASP( 53)/     -14.4942340D+00/
      DATA BETAPP( 53)/      -5.8947030D+00/
      DATA ZSPM3 ( 53)/       7.0010130D+00/
      DATA ZPPM3 ( 53)/       2.4543540D+00/
      DATA ZDPM3 ( 53)/       1.0000000D+00/
      DATA ALPPM3( 53)/       1.9901850D+00/
      DATA EISOLP( 53)/    -288.3160860D+00/
      DATA GSSPM3( 53)/      13.6319430D+00/
      DATA GSPPM3( 53)/      14.9904060D+00/
      DATA GPPPM3( 53)/       7.2883300D+00/
      DATA GP2PM3( 53)/       5.9664070D+00/
      DATA HSPPM3( 53)/       2.6300350D+00/
      DATA DDPM3 ( 53)/       0.1581469D+00/
      DATA QQPM3 ( 53)/       1.0467302D+00/
      DATA AMPM3 ( 53)/       0.5009902D+00/
      DATA ADPM3 ( 53)/       1.6699104D+00/
      DATA AQPM3 ( 53)/       0.5153082D+00/
      DATA GUESP1( 53,1)/      -0.1314810D+00/
      DATA GUESP2( 53,1)/       5.2064170D+00/
      DATA GUESP3( 53,1)/       1.7488240D+00/
      DATA GUESP1( 53,2)/      -0.0368970D+00/
      DATA GUESP2( 53,2)/       6.0101170D+00/
      DATA GUESP3( 53,2)/       2.7103730D+00/
C                    DATA FOR ELEMENT 80        MERCURY
      DATA REFPM3(80)/ ' HG: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 80)/     -17.7622290D+00/
      DATA UPPPM3( 80)/     -18.3307510D+00/
      DATA BETASP( 80)/      -3.1013650D+00/
      DATA BETAPP( 80)/      -3.4640310D+00/
      DATA ZSPM3 ( 80)/       1.4768850D+00/
      DATA ZPPM3 ( 80)/       2.4799510D+00/
      DATA ALPPM3( 80)/       1.5293770D+00/
      DATA EISOLP( 80)/     -28.8997380D+00/
      DATA GSSPM3( 80)/       6.6247200D+00/
      DATA GSPPM3( 80)/      10.6392970D+00/
      DATA GPPPM3( 80)/      14.7092830D+00/
      DATA GP2PM3( 80)/      16.0007400D+00/
      DATA HSPPM3( 80)/       2.0363110D+00/
      DATA DDPM3 ( 80)/       1.2317811D+00/
      DATA QQPM3 ( 80)/       1.2164033D+00/
      DATA AMPM3 ( 80)/       0.2434664D+00/
      DATA ADPM3 ( 80)/       0.4515472D+00/
      DATA AQPM3 ( 80)/       0.2618394D+00/
      DATA GUESP1( 80,1)/       1.0827200D+00/
      DATA GUESP2( 80,1)/       6.4965980D+00/
      DATA GUESP3( 80,1)/       1.1951460D+00/
      DATA GUESP1( 80,2)/      -0.0965530D+00/
      DATA GUESP2( 80,2)/       3.9262810D+00/
      DATA GUESP3( 80,2)/       2.6271600D+00/
C                    DATA FOR ELEMENT 81        THALLIUM
      DATA REFPM3(81)/ ' TL: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 81)/     -30.0531700D+00/
      DATA UPPPM3( 81)/     -26.9206370D+00/
      DATA BETASP( 81)/      -1.0844950D+00/
      DATA BETAPP( 81)/      -7.9467990D+00/
      DATA ZSPM3 ( 81)/       6.8679210D+00/
      DATA ZPPM3 ( 81)/       1.9694450D+00/
      DATA ALPPM3( 81)/       1.3409510D+00/
      DATA EISOLP( 81)/     -56.6492050D+00/
      DATA GSSPM3( 81)/      10.4604120D+00/
      DATA GSPPM3( 81)/      11.2238830D+00/
      DATA GPPPM3( 81)/       4.9927850D+00/
      DATA GP2PM3( 81)/       8.9627270D+00/
      DATA HSPPM3( 81)/       2.5304060D+00/
      DATA DDPM3 ( 81)/       0.0781362D+00/
      DATA QQPM3 ( 81)/       1.5317110D+00/
      DATA AMPM3 ( 81)/       0.3844326D+00/
      DATA ADPM3 ( 81)/       2.5741815D+00/
      DATA AQPM3 ( 81)/       0.2213264D+00/
      DATA GUESP1( 81,1)/      -1.3613990D+00/
      DATA GUESP2( 81,1)/       3.5572260D+00/
      DATA GUESP3( 81,1)/       1.0928020D+00/
      DATA GUESP1( 81,2)/      -0.0454010D+00/
      DATA GUESP2( 81,2)/       2.3069950D+00/
      DATA GUESP3( 81,2)/       2.9650290D+00/
C                    DATA FOR ELEMENT 82        LEAD
      DATA REFPM3(82)/ ' PB: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 82)/     -30.3227560D+00/
      DATA UPPPM3( 82)/     -24.4258340D+00/
      DATA BETASP( 82)/      -6.1260240D+00/
      DATA BETAPP( 82)/      -1.3954300D+00/
      DATA ZSPM3 ( 82)/       3.1412890D+00/
      DATA ZPPM3 ( 82)/       1.8924180D+00/
      DATA ALPPM3( 82)/       1.6200450D+00/
      DATA EISOLP( 82)/     -73.4660775D+00/
      DATA GSSPM3( 82)/       7.0119920D+00/
      DATA GSPPM3( 82)/       6.7937820D+00/
      DATA GPPPM3( 82)/       5.1837800D+00/
      DATA GP2PM3( 82)/       5.0456510D+00/
      DATA HSPPM3( 82)/       1.5663020D+00/
      DATA DDPM3 ( 82)/       0.9866290D+00/
      DATA QQPM3 ( 82)/       1.5940562D+00/
      DATA AMPM3 ( 82)/       0.2576991D+00/
      DATA ADPM3 ( 82)/       0.4527678D+00/
      DATA AQPM3 ( 82)/       0.2150175D+00/
      DATA GUESP1( 82,1)/      -0.1225760D+00/
      DATA GUESP2( 82,1)/       6.0030620D+00/
      DATA GUESP3( 82,1)/       1.9015970D+00/
      DATA GUESP1( 82,2)/      -0.0566480D+00/
      DATA GUESP2( 82,2)/       4.7437050D+00/
      DATA GUESP3( 82,2)/       2.8618790D+00/
C                    DATA FOR ELEMENT 83        BISMUTH
      DATA REFPM3(83)/ ' BI: (PM3): J. J. P. STEWART, J. COMP. CHEM.
     1(ACCEPTED)                      '/
      DATA USSPM3( 83)/     -33.4959380D+00/
      DATA UPPPM3( 83)/     -35.5210260D+00/
      DATA BETASP( 83)/      -5.6072830D+00/
      DATA BETAPP( 83)/      -5.8001520D+00/
      DATA ZSPM3 ( 83)/       4.9164510D+00/
      DATA ZPPM3 ( 83)/       1.9349350D+00/
      DATA ALPPM3( 83)/       1.8574310D+00/
      DATA EISOLP( 83)/    -109.2774910D+00/
      DATA GSSPM3( 83)/       4.9894800D+00/
      DATA GSPPM3( 83)/       6.1033080D+00/
      DATA GPPPM3( 83)/       8.6960070D+00/
      DATA GP2PM3( 83)/       8.3354470D+00/
      DATA HSPPM3( 83)/       0.5991220D+00/
      DATA DDPM3 ( 83)/       0.2798609D+00/
      DATA QQPM3 ( 83)/       1.5590294D+00/
      DATA AMPM3 ( 83)/       0.1833693D+00/
      DATA ADPM3 ( 83)/       0.6776013D+00/
      DATA AQPM3 ( 83)/       0.2586520D+00/
      DATA GUESP1( 83,1)/       2.5816930D+00/
      DATA GUESP2( 83,1)/       5.0940220D+00/
      DATA GUESP3( 83,1)/       0.4997870D+00/
      DATA GUESP1( 83,2)/       0.0603200D+00/
      DATA GUESP2( 83,2)/       6.0015380D+00/
      DATA GUESP3( 83,2)/       2.4279700D+00/
C                    DATA FOR ELEMENT  103      CAPPED BOND
      DATA REFPM3(102)/' CB: (PM3):  CAPPED BOND  (HYDROGEN-LIKE, TAKES
     1ON A  ZERO CHARGE.)             '/
      DATA USSPM3(102)/     -11.9062760D+00/
      DATA BETASP(102)/-9999999.0000000D+00/
      DATA ZSPM3 (102)/       4.0000000D+00/
      DATA ZPPM3 (102)/       0.3000000D+00/
      DATA ZDPM3 (102)/       0.3000000D+00/
      DATA ALPPM3(102)/       2.5441341D+00/
      DATA EISOLP(102)/       4.0000000D+00/
      DATA GSSPM3(102)/      12.8480000D+00/
      DATA HSPPM3(102)/       0.1000000D+00/
      DATA DDPM3 (102)/       0.0684105D+00/
      DATA QQPM3 (102)/       1.0540926D+00/
      DATA AMPM3 (102)/       0.4721793D+00/
      DATA ADPM3 (102)/       0.9262742D+00/
      DATA AQPM3 (102)/       0.2909059D+00/
C
C     CHECK FOR USER PROVIDED PARAMETERS.
C
C-----CALL PRMTRS
C
      EFIELD(1) = 0.0D+00
      EFIELD(2) = 0.0D+00
      EFIELD(3) = 0.0D+00
C
      DO 100 I=1,107
         NATOR2(I) = NATORB(I)
C
C   COMMON BLOCKS FOR AM1
C
         ELEMN2(I) = ELEMNT(I)
         CORE2(I)  = CORE(I)
         DD2(I)    = DD(I)
         QQ2(I)    = QQ(I)
         AM2(I)    = AM(I)
         AD2(I)    = AD(I)
CJAN         ZS2(I)    = ZS(I)
CJAN         ZP2(I)    = ZP(I)
CJAN         ZD2(I)    = ZD(I)
         USS2(I)   = USS(I)
         UPP2(I)   = UPP(I)
         UDD2(I)   = UDD(I)
         BETAS2(I) = BETAS(I)
         BETAP2(I) = BETAP(I)
         BETAD2(I) = BETAD(I)
         GSS2(I)   = GSS(I)
         GSP2(I)   = GSP(I)
         GPP2(I)   = GPP(I)
         GP22(I)   = GP2(I)
         EISOL2(I) = EISOL(I)
         EHEAT2(I) = EHEAT(I)
         VS2(I)    = VS(I)
         VP2(I)    = VP(I)
         VD2(I)    = VD(I)
         AMS2(I)   = AMS(I)
         DO 50 J=1,10
            GESA12(I,J) = GUESA1(I,J)
            GESA22(I,J) = GUESA2(I,J)
            GESA32(I,J) = GUESA3(I,J)
            GESP12(I,J) = GUESP1(I,J)
            GESP22(I,J) = GUESP2(I,J)
            GESP32(I,J) = GUESP3(I,J)
   50    CONTINUE
         FN12(I)   = FN1(I)
         FN22(I)   = FN2(I)
C
C   COMMON BLOCKS FOR MNDO
C
         USSM2(I)  = USSM(I)
         UPPM2(I)  = UPPM(I)
         UDDM2(I)  = UDDM(I)
         ZSM2(I)   = ZSM(I)
         ZPM2(I)   = ZPM(I)
         ZDM2(I)   = ZDM(I)
         BETSM2(I) = BETASM(I)
         BETPM2(I) = BETAPM(I)
         BETDM2(I) = BETADM(I)
         ALPM2(I)  = ALPM(I)
         ESOLM2(I) = EISOLM(I)
         DDM2(I)   = DDM(I)
         QQM2(I)   = QQM(I)
         AMM2(I)   = AMM(I)
         ADM2(I)   = ADM(I)
         AQM2(I)   = AQM(I)
         GSSM2(I)  = GSSM(I)
         GSPM2(I)  = GSPM(I)
         GPPM2(I)  = GPPM(I)
         GP2M2(I)  = GP2M(I)
         HSPM2(I)  = HSPM(I)
         PLVOM2(I) = POLVOM(I)
C
C  COMMON BLOCKS FOR PM3
C
         USSP32(I) = USSPM3(I)
         UPPP32(I) = UPPPM3(I)
         UDDP32(I) = UDDPM3(I)
         ZSPM32(I) = ZSPM3(I)
         ZPPM32(I) = ZPPM3(I)
         ZDPM32(I) = ZDPM3(I)
         BETSP2(I) = BETASP(I)
         BETPP2(I) = BETAPP(I)
         BETDP2(I) = BETADP(I)
         ALPP32(I) = ALPPM3(I)
         ESOLP2(I) = EISOLP(I)
         DDPM32(I) = DDPM3(I)
         QQPM32(I) = QQPM3(I)
         AMPM32(I) = AMPM3(I)
         ADPM32(I) = ADPM3(I)
         AQPM32(I) = AQPM3(I)
         GSSP32(I) = GSSPM3(I)
         GSPP32(I) = GSPPM3(I)
         GPPP32(I) = GPPPM3(I)
         GP2P32(I) = GP2PM3(I)
         HSPP32(I) = HSPPM3(I)
         PLVOP2(I) = POLVOP(I)
C
C   COMMON BLOCKS FOR AM1
C
         USSA12(I) = USSAM1(I)
         UPPA12(I) = UPPAM1(I)
         UDDA12(I) = UDDAM1(I)
         ZSAM12(I) = ZSAM1(I)
         ZPAM12(I) = ZPAM1(I)
         ZDAM12(I) = ZDAM1(I)
         BETSA2(I) = BETASA(I)
         BETPA2(I) = BETAPA(I)
         BETDA2(I) = BETADA(I)
         ALPA12(I) = ALPAM1(I)
         ESOLA2(I) = EISOLA(I)
         DDAM12(I) = DDAM1(I)
         QQAM12(I) = QQAM1(I)
         AMAM12(I) = AMAM1(I)
         ADAM12(I) = ADAM1(I)
         AQAM12(I) = AQAM1(I)
         GSSA12(I) = GSSAM1(I)
         GSPA12(I) = GSPAM1(I)
         GPPA12(I) = GPPAM1(I)
         GP2A12(I) = GP2AM1(I)
         HSPA12(I) = HSPAM1(I)
         PLVOA2(I) = POLVOA(I)
         REFMN2(I) = REFMN(I)
         REFM32(I) = REFM3(I)
         REFAM2(I) = REFAM(I)
         REFP32(I) = REFPM3(I)
C
C    COMMON  BLOCKS FOR MINDO/3
C
         F032(I)   = F03(I)
  100 CONTINUE
C
      DO 200 I = 1,18
         USS32(I)  = USS3(I)
         UPP32(I)  = UPP3(I)
         ESOL32(I) = EISOL3(I)
         EHET32(I) = EHEAT3(I)
         ZS32(I)   = ZS3(I)
         ZP32(I)   = ZP3(I)
  200 CONTINUE
C
      DO 300 I = 1,153
         BETA32(I) = BETA3(I)
         ALP32(I)  = ALP3(I)
  300 CONTINUE
      RETURN
      END
