C     Last change:  ERB   9 Jul 2001    3:56 pm
      PROGRAM RESAN2K
C       RESIDUALS ANALYSIS PROGRAM BY R. L. COOLEY, USGS, DENVER, COLO.
C       MODIFIED FOR MODFLOWP BY MARY C. HILL, USGS, DENVER, COLO.
C          NOT ADAPTED FOR DRY OBSERVATIONS
C ***      modified to compute Cook's D and DFBETAS
C ***      by Richard M. Yager, Ithaca, New York
c ***      changes begin and end with c ***
C***       f77 resanp.f -o resanp
C       Modified to work with MODFLOW-2000 by E.R. Banta, 8/12/1999
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*40 VERSION
      PARAMETER (VERSION='1.1.2 04/03/2001')
C     MAKE NP AND ND EQUAL TO OR LARGER THAN THE NUMBER OF PARAMETERS
C        AND DEPENDENT-VARIABLE OBSERVATIONS.
      PARAMETER (NP=500,ND=10000,NQ=500,MPD=100,IPD=100,
     &           NALL=ND+NQ+MPD+IPD)
      CHARACTER*4 BLANK
      DIMENSION X(NP,NALL), X0(NP,NQ), COV(NP,NP), W(ND),
     &          R(NALL,NALL), D(NALL), G(NALL),
     &          F(NALL), BUFF2(NALL), WQ(NQ,NQ), WP(NP),
     &          NIPR(IPD), WTPS(IPD,IPD), PRM(NP,MPD)
C *** VARIABLES FOR COOK'S D AND DFBETAS
      DIMENSION E(NALL), CD(NALL), C(NP,NALL), S(NALL), DFBETA(NALL,NP)
      INTEGER ISYM(NALL), ISYM1(NALL)
      CHARACTER*10 PARNAM(NP)
      CHARACTER*12 DID(NALL), DID1(NALL), TMPDID
      CHARACTER*80 STRIP2
      CHARACTER*80 OUTNAM
      CHARACTER*84 FN
C *** variables
C     X(NVAR,NOBS):    sensitivity matrix AND THE SENSITIVITY MATRIX
C                      TIMES THE SQUARE ROOT OF THE WEIGHT MATRIX. FOR
C                      THE LATTER THE SECOND DIMENSION IS NOBS+MPR+IPR.
C     W(NOBS):         weight matrix
C     COV(NVAR,NVAR):  covariance matrix
C     E(NOBS):         weighted residuals
C     F(NOBS):         cumulative probability
C     BUFF2(NOBS):     standard normal statistic
C     VAR:             s^2
C     R(I) = (I-R)*var
C     C = COV*XT*wt2
C     CD:              Cook's D
C     DFBETA:          DFBETAS
      COMMON/ITP/IIN,IOUT
      COMMON/FLT/X,R
C!      EQUIVALENCE (X(1,1),D(1)),(W(1),F(1),WP(1)),(COV(1,1),G(1))
 1010 FORMAT (7I5,I10,F25.0)
 1020 FORMAT (8F15.0)
 1030 FORMAT (//,
     &        ' NUMBER OF ESTIMATED PARAMETERS.............. ',I6,/,
     &        ' NUMBER OF OBSERVATIONS ..................... ',I6,/,
     &        ' NUMBER OF HEADS ............................ ',I6,/,
     &        ' NUMBER OF OBSERVATIONS OTHER THAN HEADS .... ',I6,/,
     &        ' NUMBER OF PRIOR EQUATIONS................... ',I6,/,
     &        ' NUMBER OF PRIOR WITH FULL WEIGHT MATRIX..... ',I6,/,
     &        ' NUMBER OF SETS OF RANDOM DEVIATES .......... ',I6,/,
     &        ' NUMBER FOR RANDOM NUMBER GENERATOR ......... ',I11,/,
     &        ' CALCULATED ERROR VARIANCE .................. ',G13.6)
 1040 FORMAT (/,14X,'RELIABILITY WEIGHTS FOR SAMPLE INFORMATION',/,
     &2X,2(6X,'OBSERVATION',15X),/,
     &2X,2('OBS#',5X,'NAME',7X,'W**.5',7X))
 1060 FORMAT (/,' COVARIANCE MATRIX FOR ESTIMATED PARAMETERS')
 1070 FORMAT (/,1X,'SENSITIVITIES FOR OPTIMUM PARAMETERS')
 1080 FORMAT (/,9X,'ORDERED, INDEPENDENT NORMAL DEVIATES',/,
     &10X,'OBSERVATION',/
     &4X,'NO.',6X,'NAME',12X,'D',13X,'F')
 1090 FORMAT (/,9X,'ORDERED, CORRELATED NORMAL DEVIATES'/,
     &10X,'OBSERVATION',/
     &4X,'NO.',6X,'NAME',12X,'G',13X,'F')
 1100 FORMAT (/,1X,'COVARIANCE MATRIX FOR RESIDUALS')
 1110 FORMAT (/,1X,'DATA GENERATED FROM RANDOM NUMBER SET NO. ',I3)
 1120 FORMAT (/,1X,'CORRELATION MATRIX FOR RESIDUALS')
 1130 FORMAT (/,20X,'CORRELATED NORMAL DEVIATES',/,
     &2X,2(6X,'OBSERVATION',15X),/
     &2X,2(1X,'NO.',5X,'NAME',10X,'G',8X))
 1140 FORMAT (/,20X,'INDEPENDENT NORMAL DEVIATES',/,
     &2X,2(6X,'OBSERVATION',15X),/
     &2X,2(1X,'NO.',5X,'NAME',10X,'G',8X))
 1150 FORMAT(' ERROR OPENING FILE "',A,'" -- STOP EXECUTION')
 1160 FORMAT (35X,'RESAN-2000',//,
     &22X,'MODFLOW-2000 POST-PROCESSING PROGRAM',/,
     &24X,'TO PERFORM ANALYSIS OF RESIDUALS',/,
     &29X,'Version ',A/)
 1170 FORMAT(
     &' ERROR FINDING INPUT FILES.  OUTNAM MUST BE SPECIFIED AS A',/,
     &' STRING OTHER THAN "NONE" IN THE OBS FILE.  THE FILE',/,
     &' RESAN.ERR MAY CONTAIN ADDITIONAL DIAGNOSTIC INFORMATION')
 1180 FORMAT(16F25.0)
 1185 FORMAT(16F15.0)
 1190 FORMAT(1X,'"ORDERED INDEPENDENT DEVIATE"',2X,
     &'"PROBABILITY PLOTTING POSITION"',2X,
     &'"OBSERVATION"',2X,'"PLOT-SYMBOL"',2X,
     &'RANDOM NUMBER SET NO. ',I3)
 1200 FORMAT(5X,G15.3,17X,G15.3,5X,A12,2X,I5)
 1210 FORMAT(5X,G15.3,15X,G15.3,15X,A,3X,I6)
 1220 FORMAT(A4)
 1230 FORMAT(1X,'WEIGHTED RESIDUALS WILL BE READ FROM FILE ',A)
 1240 FORMAT(1X,'"ORDERED CORRELATED DEVIATE"',2X,
     &'"PROBABILITY PLOTTING POSITION"',2X,
     &'"OBSERVATION"',2X,'"PLOT-SYMBOL"',2X,
     &'RANDOM NUMBER SET NO. ',I3)
 1250 FORMAT (G15.7,1X,I5,2X,A)
 1260 FORMAT (/,' COOK''S D STATISTIC'//
     &  ' OBSERVATION',2X,'PLOT-SYMBOL',2X,
     &  'STUDENT_RESIDUAL    VAR(E)     VAR(Y)',6X,
     &  'COOK''S D')
 1270 FORMAT (1X,A,4X,I4,10X,G10.4,2X,3(2X,G10.4))
 1280 FORMAT (G15.8,2X,A,2X,I9)
 1290 FORMAT (//,' ANALYSIS OF COOKS D',/,
     &  ' FOR PLOTTING, COOKS D STATISTICS ARE LISTED IN THE _RC OUTPUT'
     &  ,' FILE',//,
     &  ' INFLUENTIAL OBSERVATIONS WITH COOKS D > CRITICAL VALUE ',
     &  '(4/(NOBS+MPR+IPR)) = ',F5.3//
     &  ' OBS# OBSERVATION   PLOT-SYMBOL   COOK''S D')
 1300 FORMAT (3X,'"COOK''S D"',4X,'"OBSERVATION"   "PLOT-SYMBOL"')
 1310 FORMAT (I5,1X,A,4X,1I4,4X,E18.8)
 1320 FORMAT (' INFLUENTIAL OBSERVATIONS IDENTIFIED:',I5,/)
 1330 FORMAT (/,' ANALYSIS USING DFBETAS',/,
     &  ' FOR PLOTTING, DFBETA STATISTICS ARE LISTED IN THE _RB OUTPUT '
     &  ,'FILE',//,
     &  ' INFLUENTIAL OBSERVATIONS WITH DFBETA > CRITICAL VALUE ',
     &  '(2/(NOBS+MPR+IPR)**0.5) = ',F5.3//
     &  ' PARAMETERS INFLUENCED IDENTIFIED BY #'//,
     &  34X,'PARAMETER NUMBER'/
     &  ' OBS#   ID          PLOT-SYMBOL',2X,20I2)
 1340 FORMAT (I5,1X,A,4X,1I4,7X,A)
 1350 FORMAT(6(A10,1X))
 1360 FORMAT('OBSERVATION  SYMBOL  ',500(1X,A10,:,5X))
 1370 FORMAT(A12,1X,I5,500(1X,G15.8))
 1380 FORMAT(/,' The following files have been prepared: ',
     &5(/,3X,A),/)
 1390 FORMAT (1X,A,4X,I4,10X,'RR OUT OF RANGE: ',G25.16)
 1400 FORMAT (//,' ***SETS OF RANDOM NUMBERS***',//,
     & ' FOR PLOTTING, SETS OF INDEPENDENT RANDOM NUMBERS ARE LISTED ',
     & 'IN THE _RD OUTPUT FILE,',/,
     & ' SETS OF CORRELATED RANDOM NUMBERS ARE LISTED IN THE _RG ',
     & 'OUTPUT FILE')
 1410 FORMAT(/,1X,'*** ERROR: ',A,/,1X,'EXCEEDS ARRAY LIMIT OF ',
     &       I8,/,1X,'INCREASE ',A,' AND RECOMPILE PROGRAM')
 1420 FORMAT(16I5)
 1430 FORMAT(1X,'DFBETAS FOR THE FOLLOWING OBSERVATIONS CANNOT BE',
     &' CALCULATED, PROBABLY',/,' DUE TO A COMBINATION OF MODEL',
     &' NONLINEARITY AND LARGE LEVERAGE:')
 1440 FORMAT(5X,A)
 1450 FORMAT(/,' *** DUE TO PRECISION LIMITATION, CORRELATION',
     &         ' MATRIX CANNOT BE CALCULATED')
C
C     WRITE PROGRAM NAME AND VERSION TO SCREEN
      WRITE (*,*)
      WRITE (*,1160) VERSION
C ***
C**DEFINE AND OPEN INPUT FILES AND OUTPUT FILE, AND DIMENSION ARRAYS
C   FOR SUBROUTINE PRTOT
      IIN=1
      IOUT=2
      IPLOTD=3
      IPLOTG=4
      INWR=8
      ICOOK=9
      IDFB=10
      idebug=15
      open(idebug,file='resan2k.dbg')
      OPEN(IOUT,FILE='RESAN.ERR',STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL',FORM='FORMATTED')
C     FIND INPUT FILE BY READING NAME AND OBS FILES, AND READING OUTNAM
      CALL FINDINP(IOUT,OUTNAM,INWR)
      IF (OUTNAM.EQ.'NONE') THEN
        WRITE(*,1170)
        CLOSE(IOUT)
        STOP
      ELSE
        CLOSE(IOUT,STATUS='DELETE')
      ENDIF
      LENGNAM = NONB_LEN(OUTNAM,80)
      FN = OUTNAM(1:LENGNAM)//'.#rs'
      OPEN (IOUT,FILE=FN,STATUS='UNKNOWN')
      WRITE (IOUT,1160) VERSION
C  OPEN FILE FOR NORMAL PROBABILITY GRAPH OF RANDOM NUMBERS
      FN = OUTNAM(1:LENGNAM)//'._rd'
      OPEN(IPLOTD,FILE=FN,STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL',FORM='FORMATTED')
C  OPEN FILE FOR NORMAL PROBABILITY GRAPH OF CORRELATED RANDOM NUMBERS
      FN = OUTNAM(1:LENGNAM)//'._rg'
      OPEN(IPLOTG,FILE=FN,STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL',FORM='FORMATTED')
C  OPEN FILE FOR COOK'S D STATISTIC
      FN = OUTNAM(1:LENGNAM)//'._rc'
      OPEN(ICOOK,FILE=FN,STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL',FORM='FORMATTED')
C  OPEN FILE FOR DF BETAS
      FN = OUTNAM(1:LENGNAM)//'._rb'
      OPEN(IDFB,FILE=FN,STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL',FORM='FORMATTED')
      BLANK='    '
C**READ AND PRINT INPUT DATA FROM THE _RS FILE AND CONVERT IT AS NEEDED
C  FOR CALCULATIONS
C**NOTE: NRAN MUST BE ODD AND MUST LIE BETWEEN 1 AND 1048575
      FN = OUTNAM(1:LENGNAM)//'._w'
      OPEN (INWR,FILE=FN,STATUS='OLD',ACCESS='SEQUENTIAL',
     &      FORM='FORMATTED',ERR=10,IOSTAT=IOS)
   10 CONTINUE
      IF (IOS.NE.0) THEN
        LENGNAM = NONB_LEN(FN,84)
        WRITE (*,1150) FN(1:LENGNAM)
        STOP
      ENDIF
      WRITE(IOUT,1230) FN(1:LENGNAM+3)
C
      FN = OUTNAM(1:LENGNAM)//'._rs'
      OPEN (IIN,FILE=FN,STATUS='OLD',ACCESS='SEQUENTIAL',
     &      FORM='FORMATTED',ERR=20,IOSTAT=IOS)
   20 CONTINUE
      IF (IOS.NE.0) THEN
        LENGNAM = NONB_LEN(FN,84)
        WRITE (*,1150) FN(1:LENGNAM)
        STOP
      ENDIF
C     READ ITEM 1 OF _rs FILE
      READ(IIN,1010) NVAR,NOBS,NH,NQT,MPR,IPR,NSETS,NRAN,VAR
      WRITE(IOUT,1030) NVAR,NOBS,NH,NQT,MPR,IPR,NSETS,NRAN,VAR
      NTOT = NOBS+MPR+IPR
C
C     CHECK ARRAY DIMENSIONS
      IERR = 0
      IF (NVAR.GT.NP) THEN
        WRITE(IOUT,1410) 'NUMBER OF PARAMETERS',NP,'NP'
        IERR = 1
      ENDIF
      IF (NOBS.GT.ND) THEN
        WRITE(IOUT,1410) 'NUMBER OF OBSERVATIONS',ND,'ND'
        IERR = 1
      ENDIF
      IF (NQT.GT.NQ) THEN
        WRITE(IOUT,1410) 'NUMBER OF OBSERVATIONS OTHER THAN HEADS',NQ,
     &                    'NQ'
        IERR = 1
      ENDIF
      IF (MPR.GT.MPD) THEN
        WRITE(IOUT,1410) 'NUMBER OF PRIOR-INFORMATION EQUATIONS',MPD,
     &                   'MPD'
        IERR = 1
      ENDIF
      IF (IPR.GT.IPD) THEN
        WRITE(IOUT,1410)
     &'NUMBER OF PARAMETERS WITH CORRELATED PRIOR INFORMATION',IPD,'IPD'
        IERR = 1
      ENDIF
      IF (IERR.GT.0) STOP
C
C *** READ WEIGHTED RESIDUALS FROM THE _W FILE TO DEFINE THE DATA PLOT
C     SYMBOLS AND OBS-NAME, AND AS NEEDED TO CALCULATE Cook's D
      DO 30 I=1,NTOT
        READ(INWR,1250) E(I),ISYM(I),DID(I)
        DID1(I) = DID(I)
        ISYM1(I) = ISYM(I)
   30 CONTINUE
      READ(IIN,1350) (PARNAM(I),I=1,NVAR)
C     READ AND WRITE VARIANCE-COVARIANCE MATRIX ON THE PARAMETERS
      DO 50 J=1,NVAR
        READ(IIN,1180) (COV(I,J),I=J,NVAR)
        DO 40 I=J,NVAR
   40   COV(J,I)=COV(I,J)
   50 CONTINUE
      WRITE(IOUT,1060)
      CALL UPARPM(COV,NVAR,8,IOUT,PARNAM,NP)
C     OBSERVATIONS WITH A DIAGONAL WEIGHT MATRIX
C         THE SQUARE-ROOT OF THE WEIGHT IS READ
      IF (NH.GT.0) READ(IIN,1185) (W(I),I=1,NH)
      WRITE(IOUT,1040)
      CALL PRTOTA(DID,W,NH)
C    OBSERVATIONS WITH A FULL WEIGHT MATRIX
C         THE SQUARE-ROOT OF THE WEIGHT MATRIX IS READ
      IF (NQT.GT.0) THEN
        DO 60 I = 1, NQT
          READ (IIN,1185) (WQ(I,J),J=1,NQT)
   60   CONTINUE
        WRITE (IOUT,*) ' '
        WRITE (IOUT,*) ' OBSERVATIONS WITH A FULL WEIGHT MATRIX:'
        CALL PRTOT(WQ,NQT,NQT,NQ)
      ENDIF
C     READ AND WRITE SENSITIVITIES
      DO 70 J=1,NOBS
        READ(IIN,1185) (X(I,J),I=1,NVAR)
   70 CONTINUE
      WRITE(IOUT,1070)
      CALL PRTOTX(X,NVAR,NOBS,NP,DID,PARNAM)
C     INITIALIZE ARRAY ELEMENTS THAT WILL HOLD SENSITIVITIES FOR PRIOR
C     INFORMATION
      IF(MPR+IPR.GT.0) THEN
        DO 90 I=1,MPR+IPR
          DO 80 J=1,NVAR
            X(J,NOBS+I)=0.D0
   80     CONTINUE
   90   CONTINUE
      ENDIF
C     PRIOR INFORMATION FROM EQUATIONS
      IF (MPR.GT.0) THEN
        DO 100 IPM=1,MPR
          READ(IIN,1020) (PRM(J,IPM),J=1,NVAR),WP(IPM)
  100   CONTINUE
      ENDIF
C     PRIOR INFORMATION WITH FULL WEIGHT MATRIX
      IF (IPR.GT.0) THEN
        READ(IIN,1420) (NIPR(I),I=1,IPR)
        DO 110 I=1,IPR
          READ(IIN,1020) (WTPS(I,J),J=1,IPR)
  110   CONTINUE
      ENDIF
C
C     CALCULATE W**.5 X, THE SQUARE-ROOT OF THE WEIGHT MATRIX TIMES X
C
C        OBSERVATIONS WITH A DIAGONAL WEIGHT MATRIX
      DO 130 J=1,NH
        WT=W(J)
        DO 120 I=1,NVAR
          X(I,J)=X(I,J)*WT
  120   CONTINUE
  130 CONTINUE
C        OBSERVATIONS WITH FULL WEIGHT MATRIX
      IF (NQT.GT.0) THEN
        DO 160 I=1,NVAR
           DO 150 K=1,NQT
              XT = 0.D0
              DO 140 L=1,NQT
                 XT = XT + WQ(K,L) * X(I,NH+L)
  140         CONTINUE
              X0(I,K) = XT
  150      CONTINUE
  160   CONTINUE
        DO 180 I=1,NVAR
          DO 170 K=1,NQT
            X(I,NH+K) = X0(I,K)
  170     CONTINUE
  180   CONTINUE
      ENDIF
C        PRIOR INFORMATION FROM EQUATIONS
      IF(MPR.GT.0) THEN
        DO 200 I=1,NVAR
          DO 190 L=1,MPR
            X(I,L+NOBS) = WP(L) * PRM(I,L)
  190     CONTINUE
  200   CONTINUE
      ENDIF
C        PRIOR WITH A FULL WEIGHT MATRIX
      IF (IPR.GT.0) THEN
        DO 220 I=1,IPR
          DO 210 K=1,IPR
            X(NIPR(I),I+MPR+NOBS) = WTPS(K,I)
  210     CONTINUE
  220   CONTINUE
      ENDIF
C
C**COMPUTE (I-R)*VAR MATRIX
C
      DO 250 K=1,NTOT
        DO 240 J=1,NVAR
          SUM=0.D0
          DO 230 I=1,NVAR
            SUM=SUM+X(I,K)*COV(I,J)
  230     CONTINUE
          R(J,K)=SUM
  240   CONTINUE
  250 CONTINUE
C
      DO 280 K=1,NTOT
        DO 270 J=K,NTOT
          SUM=0.D0
          DO 260 I=1,NVAR
            SUM=SUM+X(I,K)*R(I,J)
  260     CONTINUE
          R(J,K)=-SUM
  270   CONTINUE
  280 CONTINUE
C
      DO 300 J=1,NTOT
        DO 290 I=J,NTOT
          R(J,I)=R(I,J)
  290   CONTINUE
        R(J,J)=VAR+R(J,J)
  300 CONTINUE
C *** COMPUTE COOK'S D USING EQN 3.12.2 FROM DRAPER & SMITH, 1981
C *** NOTE: R MATRIX AT THIS POINT IN PROGRAM IS (I-R)*VAR
      WRITE(IOUT,1260)
      STD = SQRT(VAR)
      DO 310 I=1,NTOT
        RR = 1.D0-(R(I,I)/VAR)
        IF (RR.GE.0.0D0 .AND. RR.LT.1.0D0) THEN
          CCD   = E(I)/( SQRT(STD*(1.D0-RR)) )
          CD(I) = CCD*CCD*RR / ( (1.D0-RR)*NVAR )
          WRITE(IOUT,1270)DID(I),ISYM(I),
     &                   CCD,STD*(SQRT(1.D0-RR)),STD*(SQRT(RR)),CD(I)
        ELSE
          CD(I) = 0.0D0  ! To ensure that all CD are defined
          WRITE(IOUT,1390)DID(I),ISYM(I),RR
        ENDIF
  310 CONTINUE
C***
C*** Find Cook's D > critical value 4./(NOBS+MPR+IPR)
C***
      CUTOFF = 4.D0 / REAL(NTOT)
      NUM = 0
      WRITE(IOUT,1290) CUTOFF
      DO 320 I=1,NTOT
        IF (CD(I).GT.CUTOFF) THEN
          WRITE(IOUT,1310) I,DID(I),ISYM(I),CD(I)
          NUM = NUM + 1
        ENDIF
  320 CONTINUE
      WRITE(IOUT,1320) NUM
C
C     WRITE COOK'S d TO _RC FILE FOR PLOTTING
C
      WRITE(ICOOK,1300)
      DO 330 I=1,NTOT
        WRITE(ICOOK,1280) CD(I),DID(I),ISYM(I)
  330 CONTINUE
C
C *** Compute C matrix = COV * transpose(X) * WT**.5
C *** NOTE: X MATRIX AT THIS POINT IN PROGRAM IS transpose(X) * WT**.5
      DO 360 J=1,NVAR
        DO 350 I=1,NTOT
          C(J,I) = 0.D0
          DO 340 K=1,NVAR
            C(J,I) = C(J,I) + COV(J,K)*X(K,I)
  340     CONTINUE
  350   CONTINUE
  360 CONTINUE
C *** Compute DFBETAS using eqn 2.7 from Belsley & others, 1980, p. 13
      IBAD = 0
      DO 390 I=1,NTOT
        RR   = 1.D0-(R(I,I)/VAR)
        S(I)=(NTOT-NVAR)*VAR - E(I)**2.D0/(1.D0-RR)
        S(I)=( 1.D0/FLOAT(NTOT-NVAR-1) )*S(I)
C
C  TEST FOR NEGATIVE S(I), WHICH CAN'T BE HANDLED -- ERB 3/30/01.  THIS
C  SITUATION CAN ARISE DUE TO A COMBINATION OF MODEL NONLINEARITY AND
C  LARGE LEVERAGE OF THE OBSERVATION.  SEE COOK AND WEISBERG, 1982,
C  "RESIDUALS AND INFLUENCE IN REGRESSION," MONOGR. STAT. APPL.
C  PROBABLILITY, VOL. 18, CHAPMAN AND HALL, NEW YORK, P. 14.
        IF (S(I).GT.0.0D0) THEN
          S(I) = SQRT(S(I))
          DFB1 = E(I) / ( S(I) * (1.D0-RR) )
        ELSE
          DFB1 = 0.0D0
          IF (IBAD.EQ.0) WRITE(IOUT,1430)
          WRITE(IOUT,1440) DID(I)
          IBAD = 1
        ENDIF
C
        DO 380 J=1,NVAR
          CC = 0.D0
          DO 370 K = 1,NTOT
            CC = CC + C(J,K)**2.D0
  370     CONTINUE
          DFB2 = C(J,I) / SQRT(CC)
          DFBETA(I,J) = DFB1 * DFB2
  380   CONTINUE
  390 CONTINUE
C***
C*** Find DFBETAS > critical value 2/(NTOT)**0.5
C***
      NUM = 0
      CUTOFF = NTOT
      CUTOFF = 2.D0/SQRT(CUTOFF)
      WRITE(IOUT,1330) CUTOFF,(J,J=1,NVAR)
      DO 420 I=1,NTOT
        DO 410 J=1,NVAR
          IF (ABS(DFBETA(I,J)).lt.CUTOFF) GO TO 410
          STRIP2 = ' '
          DO 400 K=1,NVAR
            IF (ABS(DFBETA(I,K)).gt.CUTOFF) THEN
              STRIP2(2*K:2*K) = '#'
            ELSE
              STRIP2(2*K:2*K) = '-'
            ENDIF
  400     CONTINUE
          WRITE(IOUT,1340) I,DID(I),ISYM(I),STRIP2
          NUM = NUM + 1
          GO TO 420
  410   CONTINUE
  420 CONTINUE
      WRITE(IOUT,1320) NUM
C
C     WRITE DFBETA'S TO _RB FILE FOR PLOTTING
C
      WRITE(IDFB,1360) (PARNAM(I),I=1,NVAR)
      DO 430 I=1,NTOT
        WRITE(IDFB,1370) DID(I),ISYM(I),(DFBETA(I,J),J=1,NVAR)
  430 CONTINUE
C ***
C**COMPUTE THEORETICAL FREQUENCIES FOR DATA SETS
      TMP=NTOT+1
      DO 440 I=1,NTOT
        TEMP=I
        F(I)=(TEMP-.5D0)/TMP
        CALL UNORM(BUFF2(I),F(I),-1)
  440 CONTINUE
      SIGMA=SQRT(VAR)
C
C**LOOP FOR SETS
C
      WRITE (IOUT,1400)
      DO 530 K=1,NSETS
        WRITE(IOUT,1110) K
C**COMPUTE RANDOM NORMAL DEVIATES D AND CORRELATED NORMAL DEVIATES G
        DO 460 I=1,NTOT
          SUM=-6.D0
          DO 450 J=1,12
            SUM=SUM+RANUM(NRAN)
  450     CONTINUE
          D(I)=SIGMA*SUM
  460   CONTINUE
        DO 480 J=1,NTOT
          SUM=0.D0
          DO 470 I=1,NTOT
            SUM=SUM+R(I,J)*D(I)
  470     CONTINUE
          G(J)=SUM/VAR
  480   CONTINUE
C
C**MEASURE OF NORMALITY AND INDEPENDENCE AND RUNS TEST
C      CALL SRUN(D,NTOT,IOUT,'D   ')
C      CALL SNORM(D,BUFF,NTOT,IOUT,'D   ',NOBS+MPR+IPR)
C      CALL SRUN(G,NTOT,IOUT,'G   ')
C      CALL SNORM(G,BUFF,NTOT,IOUT,'G   ',NOBS+MPR+IPR)
C
C**ORDER AND PRINT RANDOM NORMAL DEVIATES AND CORRELATED NORMAL DEVIATES
        WRITE(IOUT,1140)
        CALL PRTOTA(DID1,D,NTOT)
        DO 500 I=1,NTOT
          DO 490 J=I,NTOT
            IF(D(J).GE.D(I)) GO TO 490
            TMP=D(I)
            D(I)=D(J)
            D(J)=TMP
  490     CONTINUE
  500   CONTINUE
        WRITE(IOUT,1080)
        CALL PRTOTD2(DID1,D,F,NTOT)
        WRITE(IPLOTD,1190) K
        WRITE(IPLOTD,1210)
     &    (D(I),BUFF2(I),DID1(I),ISYM1(I),I=1,NTOT)
        WRITE(IPLOTD,1220) BLANK
        WRITE(IOUT,1130)
        CALL PRTOTA(DID,G,NTOT)
        DO 520 I=1,NTOT
          DO 510 J=I,NTOT
            IF(G(J).GE.G(I)) GO TO 510
            TMP=G(I)
            G(I)=G(J)
            G(J)=TMP
            TMPDID=DID1(I)
            DID1(I)=DID1(J)
            DID1(J)=TMPDID
            ISYMTMP=ISYM1(I)
            ISYM1(I)=ISYM1(J)
            ISYM1(J)=ISYMTMP
  510     CONTINUE
  520   CONTINUE
        WRITE(IOUT,1090)
        CALL PRTOTD2(DID1,G,F,NTOT)
        WRITE(IPLOTG,1240) K
        WRITE(IPLOTG,1210)
     &       (G(I),BUFF2(I),DID1(I),ISYM1(I),I=1,NTOT)
        WRITE(IPLOTG,1220) BLANK
  530 CONTINUE
C
C   END SET LOOP
C
C**PRINT COVARIANCE MATRIX (I-R)*VAR
      WRITE(IOUT,1100)
      CALL URESPM(R,NTOT,8,IOUT,DID,NALL)
C**COMPUTE AND PRINT CORRELATION MATRIX
      DO 540 I=1,NTOT
        IF (R(I,I).LT.0.0D0) THEN
          WRITE(IOUT,1450)
          GOTO 570
        ENDIF
        D(I)=SQRT(R(I,I))
  540 CONTINUE
      DO 560 J=1,NTOT
        TMP=D(J)
        DO 550 I=J,NTOT
          R(I,J)=R(I,J)/(TMP*D(I))
          R(J,I)=R(I,J)
  550   CONTINUE
  560 CONTINUE
      WRITE(IOUT,1120)
C
      CALL URESPM(R,NTOT,8,IOUT,DID,NALL)
C
  570 CONTINUE
C     WRITE NAMES OF OUTPUT FILES TO SCREEN
      WRITE(*,1380) OUTNAM(1:LENGNAM)//'.#rs',OUTNAM(1:LENGNAM)//'._rd',
     &             OUTNAM(1:LENGNAM)//'._rg',OUTNAM(1:LENGNAM)//'._rc',
     &             OUTNAM(1:LENGNAM)//'._rb'
      STOP
      END
C=======================================================================
      SUBROUTINE PRTOT(C,NR,NC,NRD)
C**PRINT MATRICES AND VECTORS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION C(1)
      COMMON/ITP/IIN,IOUT
      IF(NC.EQ.1) GO TO 25
      DO 20 L=1,NC,10
      J10=L+9
      IF(J10.GT.NC) J10=NC
      WRITE(IOUT,35) (J,J=L,J10)
      WRITE(IOUT,50)
      KBC=(L-1)*NRD
      KEC=(J10-1)*NRD
      DO 10 I=1,NR
      KB=KBC+I
      KE=KEC+I
   10 WRITE(IOUT,40) I,(C(K),K=KB,KE,NRD)
   20 CONTINUE
      RETURN
   25 N=NR/3
      IF((3*N).NE.NR) N=N+1
      DO 30 K=1,N
   30 WRITE(IOUT,80) (L,C(L),L=K,NR,N)
      RETURN
   35 FORMAT ('0',10(8X,I4))
   40 FORMAT (1X,I4,10(1X,G11.5))
   50 FORMAT (' ')
   80 FORMAT (3X,3(I4,6X,G11.5,3X))
      END
C=======================================================================
      SUBROUTINE PRTOTA(DID,VALB,NO)
C**PRINT VALUES IN TWO GROUPS OF THREE COLUMNS
      DOUBLE PRECISION VALB
      CHARACTER*12 DID(NO)
      DIMENSION VALB(NO)
      COMMON/ITP/IIN,IOUT
      NR=NO/2
      IF(2*NR.NE.NO) NR=NR+1
      DO 10 K=1,NR
      WRITE(IOUT,20) (L,DID(L),VALB(L),L=K,NO,NR)
   10 CONTINUE
      RETURN
   20 FORMAT (2X,2(I4,2X,A,1X,G11.5,2X))
      END
C=======================================================================
      SUBROUTINE PRTOTC(IVAL,NO)
C**PRINT INTEGERS IN THREE GROUPS OF TWO COLUMNS
      DIMENSION IVAL(NO)
      COMMON/ITP/IIN,IOUT
      NR=NO/3
      IF(3*NR.NE.NO) NR=NR+1
      DO 10 K=1,NR
      WRITE(IOUT,20) (L,IVAL(L),L=K,NO,NR)
   10 CONTINUE
      RETURN
   20 FORMAT (3X,3(I4,7X,I4,9X))
      END
C=======================================================================
      SUBROUTINE PRTOTN(VALA,VALB,NO)
C**PRINT VALUES IN TWO GROUPS OF THREE COLUMNS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION VALA(NO),VALB(NO)
      COMMON/ITP/IIN,IOUT
      NR=NO/2
      IF(2*NR.NE.NO) NR=NR+1
      DO 10 K=1,NR
      WRITE(IOUT,20) (L,VALA(L),VALB(L),L=K,NO,NR)
   10 CONTINUE
      RETURN
   20 FORMAT (3X,2(I4,3X,G11.5,4X,G11.5,4X))
      END
C=======================================================================
      SUBROUTINE PRTOTD2(DID,VALA,VALB,NO)
C**PRINT VALUES IN TWO GROUPS OF THREE COLUMNS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*12 DID(NO)
      DIMENSION VALA(NO),VALB(NO)
      COMMON/ITP/IIN,IOUT
      DO 10 L=1,NO
        WRITE(IOUT,20) L,DID(L),VALA(L),VALB(L)
   10 CONTINUE
      RETURN
   20 FORMAT (3X,I4,3X,A,3X,G11.5,3X,G11.5)
      END
C=======================================================================
      SUBROUTINE PRTOTX(X,NR,NC,NRD,DID,PARNAM)
C**PRINT MATRICES DIVIDED VERTICALLY INTO FIVE-COLUMN BLOCKS
      DOUBLE PRECISION X
      INTEGER I, IIN1, IOUT, J, J10, K, NC, NR, NRD
      DIMENSION X(NRD,NC)
      CHARACTER*10 PARNAM(NR)
      CHARACTER*12 DID(NC)
      COMMON /ITP   / IIN1, IOUT
      DO 20 K = 1, NC, 5
        J10 = K + 4
        IF (J10.GT.NC) J10 = NC
        WRITE (IOUT,500) (DID(J),J=K,J10)
        WRITE (IOUT,510) ('------------',J=K,J10)
        DO 10 I = 1, NR
          WRITE (IOUT,505) PARNAM(I), (X(I,J),J=K,J10)
   10   CONTINUE
   20 CONTINUE
  500 FORMAT (/,1X,'PARAMETER',3X,5(A12,1X))
  505 FORMAT (1X,A10,2X,5(G12.5,1X))
  510 FORMAT (1X,10('-'),2X,5(A12,1X))
      RETURN
      END
C=======================================================================
      SUBROUTINE SRUN (A,NTOT,IOUT,ID)
C-----------------------------------------------------------------------
C     RUNS TEST
C-----------------------------------------------------------------------
      CHARACTER*4 ID
      DOUBLE PRECISION A
      DIMENSION A(NTOT)
C-----------------------------------------------------------------------
  680 FORMAT(/,' STATISTICS FOR THE INDEPENDENT NORMAL DEVIATES, D :',/,
     1       ' SUM OF SQUARED DEVIATES :',E10.3,/,
     2       ' MAXIMUM DEVIATE  :',E10.3,' DEV#',I7,/,
     3       ' MINIMUM DEVIATE  :',E10.3,' DEV#',I7,/,
     4       ' AVERAGE DEVIATE  :',E10.3,/,
     X       ' # DEVIATES >= 0. :',I7,/,' # DEVIATES < 0.  :',I7,/,
     5       ' NUMBER OF RUNS  :',I5,'  IN',I5,' VALUES')
  685 FORMAT(/,' STATISTICS FOR THE CORRELATED NORMAL DEVIATES, G :',/,
     1       ' SUM OF SQUARED DEVIATES :',E10.3,/,
     2       ' MAXIMUM DEVIATE  :',E10.3,' DEV#',I7,/,
     3       ' MINIMUM DEVIATE  :',E10.3,' DEV#',I7,/,
     4       ' AVERAGE DEVIATE  :',E10.3,/,
     X       ' # DEVIATES >= 0. :',I7,/,' # DEVIATES < 0.  :',I7,/,
     5       ' NUMBER OF RUNS  :',I5,'  IN',I5,' VALUES')
  690 FORMAT(' RUNS STATISTIC (TOO FEW RUNS):',G13.3,/,
     1       '  (IF #NEG>10 AND #POS>10, P(STAT < -1.28) = 0.10,',/,
     2       '                           P(STAT < -1.645) = 0.05,',/,
     3       '                           P(STAT < -1.96) = 0.025)',/,
     4       ' RUNS STATISTIC (TOO MANY RUNS):',G13.3,/,
     5       '  (IF #NEG>10 AND #POS>10, P(STAT > 1.28) = 0.10,',/,
     6       '                           P(STAT > 1.645) = 0.05,',/,
     7       '                           P(STAT > 1.96) = 0.025)')
       RSQ=0.
       NNEG=0
       NPOS=0
       AVE=0.
       NRUNS=1
       VMAX=-1.D20
       VMIN=1.D20
       DO 160 N=1,NTOT
         WTR=A(N)
         RSQ=RSQ+(WTR**2)
         IF(WTR.GT.VMAX) THEN
           VMAX=WTR
           NMAX=N
         ENDIF
         IF(WTR.LT.VMIN) THEN
           VMIN=WTR
           NMIN=N
         ENDIF
         IF(WTR.GE.0.) NPOS=NPOS+1
         IF(WTR.LT.0.) NNEG=NNEG+1
         IF(N.GT.1.AND.(WTRL*WTR).LT.0.) NRUNS=NRUNS+1
         WTRL=WTR
         AVE=AVE+WTR
  160 CONTINUE
C-------FINAL PRINTOUT
      AVE=AVE/REAL(NTOT)
      RP=REAL(NPOS)
      RN=REAL(NNEG)
      RNP=2.*RP*RN
      RNS=RP+RN
      RNR=REAL(NRUNS)
      ERUNS=(RNP/RNS)+1.0
      SDRUNS=((RNP*(RNP-RNS))/((RNS**2.)*(RNS-1.)))**.5
      STRUNS=(RNR-ERUNS+.5)/SDRUNS
      ST2RNS=(RNR-ERUNS-.5)/SDRUNS
      IF(ID.EQ.'D   ')WRITE(IOUT,680) RSQ,VMAX,NMAX,VMIN,NMIN,AVE,NPOS,
     1                               NNEG,NRUNS,N-1
      IF(ID.EQ.'G   ')WRITE(IOUT,685) RSQ,VMAX,NMAX,VMIN,NMIN,AVE,NPOS,
     1                               NNEG,NRUNS,N-1
      WRITE(IOUT,690) STRUNS,ST2RNS
      RETURN
      END
C=======================================================================
      SUBROUTINE SNORM(A,H,NTOT,IOUT,ID,NOBS)
C-----------------------------------------------------------------------
C     MEASURE OF NORMALITY AND INDEPENDENCE
C-----------------------------------------------------------------------
      CHARACTER*4 ID
      DOUBLE PRECISION A,H,U,RNORM
      DIMENSION A(NTOT),H(NTOT)
C-----------------------------------------------------------------------
      AVE=0.
      NND=0
      DO 160 N=1,NTOT
        NND=NND+1
        H(NND)=A(N)
        AVE=AVE+A(N)
  160 CONTINUE
C------TEST FOR NORMALITY AND INDEPENDENCE
      AVE=AVE/REAL(NND)
C-------ORDER THE RESIDUALS
      DO 240 NN=1,NND-1
        NMIN=NN
        RMIN=H(NN)
        DO 200 N=NN+1,NND
          IF(H(N).LE.RMIN) THEN
            RMIN=H(N)
            NMIN=N
          ENDIF
  200   CONTINUE
        IF(NMIN.NE.NN) THEN
          H(NMIN)=H(NN)
          H(NN)=RMIN
        ENDIF
  240 CONTINUE
      IF(ID.EQ.'D   ') WRITE(IOUT,510) NND
      IF(ID.EQ.'G   ') WRITE(IOUT,515) NND
  510 FORMAT(/,' ORDERED INDEPENDENT DEVIATES',
     1/,' NUMBER OF DEVIATES :',I10)
  515 FORMAT(/,' ORDERED CORRELATED DEVIATES',
     1/,' NUMBER OF DEVIATES :',I10)
      WRITE(IOUT,530) (H(N),N=1,NND)
  530 FORMAT(8G10.3)
C------CALCULATE THE STATISTIC
      RNUM=0.
      DEN1=0.
      DEN2=0.
      DO 420 N=1,NND
        RNORM=(REAL(N)-.5)/REAL(NND)
        CALL UNORM(U,RNORM,-1)
        DIF=H(N)-AVE
        RNUM=RNUM+DIF*U
        DEN1=DEN1+DIF**2
        DEN2=DEN2+U**2
  420 CONTINUE
      STAT=RNUM**2/(DEN1*DEN2)
      IF(ID.EQ.'D   ') WRITE(IOUT,520) STAT
      IF(ID.EQ.'G   ') WRITE(IOUT,525) STAT
  520 FORMAT(' CORRELATION BETWEEN ORDERED INDEPENDENT DEVIATES AND',/,
     1' INDEPENDENT NORMAL ORDER STATISTICS (EQ.25 OF M&G) ='
     1,G13.3)
  525 FORMAT(' CORRELATION BETWEEN ORDERED CORRELATED DEVIATES AND',/,
     1' INDEPENDENT NORMAL ORDER STATISTICS (EQ.25 OF M&G) ='
     1,G13.3)
      RETURN
      END
C=======================================================================
      SUBROUTINE UNORM(U,RNORM,IP)
C-----VERSION 19981117 ERB
C     ******************************************************************
C     FIND THE PROBABILITY RELATED TO A U (IP=1), OR A U RELATED TO A
C     PROBABILITY (IP=-1) FOR A STANDARD GAUSSIAN DISTRIBUTION
C     ******************************************************************
C        SPECIFICATIONS:
      REAL AU
      DOUBLE PRECISION PNORM, ARNORM, FACTOR, RNORM, U
      INTEGER I, IP
      DIMENSION PNORM(2,54)
C     ------------------------------------------------------------------
      DATA (PNORM(1,I),I=1,54)/0.0, .15, .20, .25, .30, .35, .40, .45, 
     &      .50, .55, .60, .65, .70, .75, .80, .85, .90, .95, 1.00, 
     &      1.05, 1.10, 1.15, 1.20, 1.25, 1.30, 1.35, 1.40, 1.45, 1.50, 
     &      1.55, 1.60, 1.65, 1.70, 1.75, 1.80, 1.85, 1.90, 1.95, 2.0,
     &      2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3.0, 3.5, 4.0,
     &      4.5, 5.0, 5.5/
      DATA (PNORM(2,I),I=1,54)/.5, .5596, .5793, .5987, .6179, .6368, 
     &      .6554, .6736, .6915, .7088, .7257, .7422, .7580, .7734, 
     &      .7881, .8023, .8159, .8289, .8413, .8531, .8643, .8749, 
     &      .8849, .8944, .90320, .91149, .91924, .92647, .93319, 
     &      .93943, .94520, .95053, .95543, .95994, .96407, .96784, 
     &      .97128, .97441, .9772, .9821, .98610, .9893, .9918, .9938,
     &      .9953, .9965, .9974, .9981, .9987, .99976737,  .99996833,
     &      .99999660, .99999971, 1.0/
C     ------------------------------------------------------------------
C
C-----GIVEN U, GET THE CUMULATIVE PROBABILITY
      IF (IP.EQ.1) THEN
C-------FIND THE VALUES ABOVE AND BELOW U
        AU = ABS(U)
        IF (AU.GE.5.5) THEN
          RNORM = 1.0
          IF (U.LT.0.0) RNORM = 0.0
        ELSE
          DO 10 I = 1, 53
            IF (AU.GE.PNORM(1,I) .AND. AU.LT.PNORM(1,I+1)) THEN
C-------INTERPOLATE
              FACTOR = (AU-PNORM(1,I))/(PNORM(1,I+1)-PNORM(1,I))
              RNORM = PNORM(2,I) + FACTOR*(PNORM(2,I+1)-PNORM(2,I))
              IF (U.LT.0) RNORM = 1.0 - RNORM
              GOTO 30
            ENDIF
   10     CONTINUE
          STOP 'ERROR IN UNORM -- U NOT FOUND'
        ENDIF
C-----GIVEN THE CUMULATIVE PROBABILITY, GET U
      ELSEIF (IP.EQ.-1) THEN
C-------FIND THE VALUES ABOVE AND BELOW RNORM
        ARNORM = RNORM
        IF (RNORM.LT..50) ARNORM = 1. - RNORM
        IF (ARNORM.EQ.1.0) THEN
          U = 5.5
          IF (RNORM.LT..5) U = -5.5
        ELSE
          DO 20 I = 1, 53
            IF (ARNORM.GE.PNORM(2,I) .AND. ARNORM.LT.PNORM(2,I+1)) THEN
C-------INTERPOLATE
              FACTOR = (ARNORM-PNORM(2,I))/(PNORM(2,I+1)-PNORM(2,I))
              U = PNORM(1,I) + FACTOR*(PNORM(1,I+1)-PNORM(1,I))
              IF (RNORM.LT..50) U = -U
              GOTO 30
            ENDIF
   20     CONTINUE
          STOP 'ERROR IN UNORM -- RNORM NOT FOUND'
        ENDIF
      ENDIF
   30 RETURN
      END
C=======================================================================
      FUNCTION RANUM(IRAN)
C  NOTE: DICK COOLEY SAYS HE HAS A BETTER RANDOM-NUMBER GENERATOR THAT
C  DOES NOT REPEAT - ERB 3/15/2001
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA MODU,MULT,NADD/1048576,1027,221589/
      IRAN=MULT*IRAN+NADD
      IRAN=IRAN-(IRAN/MODU)*MODU
      RANUM=FLOAT(IRAN)/FLOAT(MODU)
      RETURN
      END
C=======================================================================
      INTEGER FUNCTION NONB_LEN(CHARVAR,LENGTH)
C
C FUNCTION TO RETURN NON-BLANK LENGTH OF CONTENTS OF A CHARACTER VARIABLE
C
C Variable list:
C   CHARVAR  = CHARACTER VARIABLE OF INTEREST
C   LENGTH   = DIMENSIONED LENGTH OF CHARVAR
C
      CHARACTER*(*) CHARVAR,C*1
      INTEGER LENGTH
C
      C = ' '
      K = LENGTH+1
C
      DO 20 WHILE (C .EQ. ' ' .AND. K .GE. 1)
        K = K-1
        C = CHARVAR(K:K)
 20   CONTINUE
C
      NONB_LEN = K
      RETURN
      END
C=======================================================================
      SUBROUTINE FINDINP(IOUT,OUTNAM,INWR)
C
      CHARACTER*80 NAMFIL, OBFNAM, OUTNAM, OUTNAMU
      CHARACTER*200 LINE
      LOGICAL EXISTS
      DOUBLE PRECISION R
C
C     NAMFIL is the name of the name file
C     OBFNAM is the name of the OBS file
C     OUTNAM is the name of the output filename base listed in
C            the OBS file
C     OUTNAMU is an upper-case version of OUTNAM, for comparison
C            with 'NONE'
C
  500 FORMAT(' Enter the name of the NAME file:',/)
  510 FORMAT(' The NAME file is: ',A)
  520 FORMAT(A)
  530 FORMAT(/,' Error reading NAME file.  Enter blank line to quit.',/)
  540 FORMAT(/,' Error opening NAME file.  Enter blank line to quit.',/)
  550 FORMAT(/,' Error opening file "',A,'"')
C
    1 CONTINUE
      NAMFIL = ' '
      ISTAT = 0
C *** The following statement should be uncommented in order to use
C *** GETARG to retrieve a command line argument.
      CALL GETARG(1,NAMFIL)
      IF(NAMFIL.NE.' ') THEN
        INQUIRE (FILE=NAMFIL,EXIST=EXISTS)
        IF(.NOT.EXISTS) THEN
          NC=INDEX(NAMFIL,' ')
          NAMFIL(NC:NC+3)='.nam'
          INQUIRE (FILE=NAMFIL,EXIST=EXISTS)
          IF(.NOT.EXISTS) THEN
            WRITE(*,*) ' Name file does not exist'
            STOP
          ENDIF
        ENDIF
      ELSE
        WRITE(*,500)
        READ(*,520,ERR=5,IOSTAT=ISTAT) NAMFIL
      ENDIF
      IF (NAMFIL.EQ.' ') STOP
    5 CONTINUE
      IF (ISTAT.NE.0) THEN
        WRITE(*,530)
        GOTO 1
      ENDIF
      OPEN(INWR,FILE=NAMFIL,STATUS='OLD',ERR=7,IOSTAT=ISTAT)
    7 CONTINUE
      IF (ISTAT.NE.0) THEN
        WRITE(*,540)
        GOTO 1
      ENDIF
      WRITE(*,510) NAMFIL
C
      OUTNAM=' '
   10 CONTINUE
C       Read down in NAME file to find name of OBS file
        CALL URDCOM(INWR,IOUT,LINE,ISTAT)
        IF (ISTAT.EQ.1) THEN
C         END-OF-FILE reached
          OUTNAM='NONE'
          GOTO 40
        ENDIF
        LLOC = 1
C       Look for OBS file type in the name file
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INWR)
        IF (LINE(ISTART:ISTOP).EQ.'OBS') THEN
C         Assign OBFNAM as the name of the OBS file, read from the NAME
C         file
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INWR)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INWR)
          OBFNAM = LINE(ISTART:ISTOP)
          CLOSE(INWR)
          OPEN(INWR,FILE=OBFNAM,STATUS='OLD',ERR=20,IOSTAT=ISTAT)
   20     CONTINUE
          IF (ISTAT.NE.0) THEN
            NLEN = NONB_LEN(OBFNAM,80)
            WRITE(*,550) OBFNAM(1:NLEN)
            STOP
          ENDIF
C         Assign OUTNAM as filename base read from OBS file
          CALL URDCOM(INWR,IOUT,LINE,ISTAT)
          LLOC = 1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INWR)
          OUTNAM = LINE(ISTART:ISTOP)
          CLOSE(INWR)
          CALL UCASE(OUTNAM,OUTNAMU,1)
          IF (OUTNAMU.EQ.'NONE') OUTNAM = 'NONE'
          GOTO 40
        ENDIF
      GOTO 10
C
   40 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE URDCOM(IN,IOUT,LINE,ISTAT)
C
C-----VERSION 02FEB1999 URDCOM
C     ******************************************************************
C     READ COMMENTS FROM A FILE AND PRINT THEM.  RETURN THE FIRST LINE
C     THAT IS NOT A COMMENT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*(*) LINE
C     ------------------------------------------------------------------
      ISTAT=0
   10 READ(IN,'(A)',END=40,ERR=50) LINE
      IF(LINE(1:1).NE.'#') RETURN
      L=LEN(LINE)
      IF(L.GT.79) L=79
      DO 20 I=L,1,-1
      IF(LINE(I:I).NE.' ') GO TO 30
   20 CONTINUE
   30 WRITE(IOUT,'(1X,A)') LINE(1:I)
      GO TO 10
C
C     FLAG END OF FILE
   40 CONTINUE
      ISTAT=1
      RETURN
C
C     FLAG READ ERROR
   50 CONTINUE
      ISTAT=2
      RETURN
      END
C=======================================================================
      SUBROUTINE URWORD(LINE,ICOL,ISTART,ISTOP,NCODE,N,R,IOUT,IN)
C
C
C-----VERSION 1003 05AUG1992 URWORD
C     ******************************************************************
C     ROUTINE TO EXTRACT A WORD FROM A LINE OF TEXT, AND OPTIONALLY
C     CONVERT THE WORD TO A NUMBER.
C        ISTART AND ISTOP WILL BE RETURNED WITH THE STARTING AND
C          ENDING CHARACTER POSITIONS OF THE WORD.
C        THE LAST CHARACTER IN THE LINE IS SET TO BLANK SO THAT IF ANY
C          PROBLEMS OCCUR WITH FINDING A WORD, ISTART AND ISTOP WILL
C          POINT TO THIS BLANK CHARACTER.  THUS, A WORD WILL ALWAYS BE
C          RETURNED UNLESS THERE IS A NUMERIC CONVERSION ERROR.  BE SURE
C          THAT THE LAST CHARACTER IN LINE IS NOT AN IMPORTANT CHARACTER
C          BECAUSE IT WILL ALWAYS BE SET TO BLANK.
C        A WORD STARTS WITH THE FIRST CHARACTER THAT IS NOT A SPACE OR
C          COMMA, AND ENDS WHEN A SUBSEQUENT CHARACTER THAT IS A SPACE
C          OR COMMA.  NOTE THAT THESE PARSING RULES DO NOT TREAT TWO
C          COMMAS SEPARATED BY ONE OR MORE SPACES AS A NULL WORD.
C        FOR A WORD THAT BEGINS WITH "'", THE WORD STARTS WITH THE
C          CHARACTER AFTER THE QUOTE AND ENDS WITH THE CHARACTER
C          PRECEDING A SUBSEQUENT QUOTE.  THUS, A QUOTED WORD CAN
C          INCLUDE SPACES AND COMMAS.  THE QUOTED WORD CANNOT CONTAIN
C          A QUOTE CHARACTER.
C        IF NCODE IS 1, THE WORD IS CONVERTED TO UPPER CASE.
C        IF NCODE IS 2, THE WORD IS CONVERTED TO AN INTEGER.
C        IF NCODE IS 3, THE WORD IS CONVERTED TO A REAL NUMBER.
C        NUMBER CONVERSION ERROR IS WRITTEN TO UNIT IOUT IF IOUT IS
C          POSITIVE; ERROR IS WRITTEN TO DEFAULT OUTPUT IF IOUT IS 0;
C          NO ERROR MESSAGE IS WRITTEN IF IOUT IS NEGATIVE.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*(*) LINE
      CHARACTER*20 RW,STRING
      DOUBLE PRECISION R
C     ------------------------------------------------------------------
C
C1------Set last char in LINE to blank and set ISTART and ISTOP to point
C1------to this blank as a default situation when no word is found.  If
C1------starting location in LINE is out of bounds, do not look for a
C1------word.
      LINLEN=LEN(LINE)
      LINE(LINLEN:LINLEN)=' '
      ISTART=LINLEN
      ISTOP=LINLEN
      LINLEN=LINLEN-1
      IF(ICOL.LT.1 .OR. ICOL.GT.LINLEN) GO TO 100
C
C2------Find start of word, which is indicated by first character that
C2------is not a blank and not a comma.
      DO 10 I=ICOL,LINLEN
      IF(LINE(I:I).NE.' ' .AND. LINE(I:I).NE.',') GO TO 20
10    CONTINUE
      ICOL=LINLEN+1
      GO TO 100
C
C3------Found start of word.  Look for end.
C3A-----When word is quoted, only a quote can terminate it.
20    IF(LINE(I:I).EQ.'''') THEN
         I=I+1
         IF(I.LE.LINLEN) THEN
            DO 25 J=I,LINLEN
            IF(LINE(J:J).EQ.'''') GO TO 40
25          CONTINUE
         END IF
C
C3B-----When word is not quoted, space or comma will terminate.
      ELSE
         DO 30 J=I,LINLEN
         IF(LINE(J:J).EQ.' ' .OR. LINE(J:J).EQ.',') GO TO 40
30       CONTINUE
      END IF
C
C3C-----End of line without finding end of word; set end of word to
C3C-----end of line.
      J=LINLEN+1
C
C4------Found end of word; set J to point to last character in WORD and
C-------set ICOL to point to location for scanning for another word.
40    ICOL=J+1
      J=J-1
      IF(J.LT.I) GO TO 100
      ISTART=I
      ISTOP=J
C
C5------Convert word to upper case and RETURN if NCODE is 1.
      IF(NCODE.EQ.1) THEN
         IDIFF=ICHAR('a')-ICHAR('A')
         DO 50 K=ISTART,ISTOP
            IF(LINE(K:K).GE.'a' .AND. LINE(K:K).LE.'z')
     1             LINE(K:K)=CHAR(ICHAR(LINE(K:K))-IDIFF)
50       CONTINUE
         RETURN
      END IF
C
C6------Convert word to a number if requested.
100   IF(NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
         RW=' '
         L=20-ISTOP+ISTART
         IF(L.LT.1) GO TO 200
         RW(L:20)=LINE(ISTART:ISTOP)
         IF(NCODE.EQ.2) READ(RW,'(I20)',ERR=200) N
         IF(NCODE.EQ.3) READ(RW,'(F20.0)',ERR=200) R
      END IF
      RETURN
C
C7------Number conversion error.
200   IF(NCODE.EQ.3) THEN
         STRING= 'A REAL NUMBER'
         L=13
      ELSE
         STRING= 'AN INTEGER'
         L=10
      END IF
C
C7A-----If output unit is negative, set last character of string to 'E'.
      IF(IOUT.LT.0) THEN
         N=0
         R=0.
         LINE(LINLEN+1:LINLEN+1)='E'
         RETURN
C
C7B-----If output unit is positive; write a message to output unit.
      ELSE IF(IOUT.GT.0) THEN
         IF(IN.GT.0) THEN
            WRITE(IOUT,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
         ELSE
            WRITE(IOUT,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
         END IF
201      FORMAT(1X,/1X,'FILE UNIT',I4,' : ERROR CONVERTING "',A,
     1       '" TO ',A,' IN LINE:',/1X,A)
202      FORMAT(1X,/1X,'KEYBOARD INPUT : ERROR CONVERTING "',A,
     1       '" TO ',A,' IN LINE:',/1X,A)
C
C7C-----If output unit is 0; write a message to default output.
      ELSE
         IF(IN.GT.0) THEN
            WRITE(*,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
         ELSE
            WRITE(*,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
         END IF
      END IF
C
C7D-----STOP after writing message.
      STOP
      END
C=======================================================================
      SUBROUTINE UCASE(WORDIN,WORDOUT,ICASE)
C     VERSION 19980316 ERB
C     ******************************************************************
C     CONVERT A CHARACTER STRING TO ALL UPPER (ICASE > 0) OR ALL
C     LOWER (ICASE < 0) CASE.  IF ICASE = 0, NO CASE CONVERSION IS DONE.
C     ******************************************************************
C       SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER WORDIN*(*), WORDOUT*(*)
      INTEGER ICASE, LENGNB, LENGIN, LENGOUT
C
C     DETERMINE LENGTH OF STRING VARIABLES
      LENGIN = LEN(WORDIN)
      LENGOUT = LEN(WORDOUT)
C
C     DETERMINE IF WORDOUT IS LONG ENOUGH TO CONTAIN NON-BLANK LENGTH
C     OF WORDIN
      LENGNB = NONB_LEN(WORDIN,LENGIN)
      IF (LENGNB.GT.LENGOUT) STOP 'STRING-LENGTH ERROR IN UCASE'
C
      WORDOUT = WORDIN
      IDIFF=ICHAR('a')-ICHAR('A')
      IF (ICASE.GT.0) THEN
C       CONVERT STRING TO UPPER CASE
        DO 10 K=1,LENGNB
          IF(WORDIN(K:K).GE.'a' .AND. WORDIN(K:K).LE.'z')
     1      WORDOUT(K:K)=CHAR(ICHAR(WORDIN(K:K))-IDIFF)
10      CONTINUE
C
      ELSEIF (ICASE.LT.0) THEN
C       CONVERT STRING TO LOWER CASE
        DO 20 K=1,LENGNB
          IF(WORDIN(K:K).GE.'A' .AND. WORDIN(K:K).LE.'Z')
     1      WORDOUT(K:K)=CHAR(ICHAR(WORDIN(K:K))+IDIFF)
20      CONTINUE
C
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE UPARPM(BUF,NPE,IPRC,IOUT,PARNAM,NPD)
C
C-----VERSION 19980825 ERB
C     ******************************************************************
C     PRINT ONE NPE*NPE CORRELATION OR VARIANCE-COVARIANCE MATRIX
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*10 PARNAM(NPE)
      DOUBLE PRECISION BUF
      DIMENSION BUF(NPD,NPD)
C     ------------------------------------------------------------------
C
C2------MAKE SURE THE FORMAT CODE (IPRC) IS VALID
      IP=IPRC
      IF(IP.LT.1 .OR. IP.GT.10) IP=1
C
C3------LABEL COLUMNS WITH PARAMETER NAMES.
      IF(IP.EQ.1) CALL UCOLLBL(1,NPE,0,11,11,IOUT,PARNAM,NPE)
      IF(IP.EQ.2) CALL UCOLLBL(1,NPE,0,10,12,IOUT,PARNAM,NPE)
      IF(IP.EQ.3) CALL UCOLLBL(1,NPE,0,9,13,IOUT,PARNAM,NPE)
      IF(IP.EQ.4) CALL UCOLLBL(1,NPE,0,8,14,IOUT,PARNAM,NPE)
      IF(IP.EQ.5) CALL UCOLLBL(1,NPE,0,8,15,IOUT,PARNAM,NPE)
      IF(IP.EQ.6) CALL UCOLLBL(1,NPE,0,6,11,IOUT,PARNAM,NPE)
      IF(IP.EQ.7) CALL UCOLLBL(1,NPE,0,5,12,IOUT,PARNAM,NPE)
      IF(IP.EQ.8) CALL UCOLLBL(1,NPE,0,5,13,IOUT,PARNAM,NPE)
      IF(IP.EQ.9) CALL UCOLLBL(1,NPE,0,4,14,IOUT,PARNAM,NPE)
      IF(IP.EQ.10 ) CALL UCOLLBL(1,NPE,0,4,15,IOUT,PARNAM,NPE)
C
C4------LOOP THROUGH THE ROWS PRINTING EACH ONE IN ITS ENTIRETY.
      DO 1000 I=1,NPE
C
C------------ FORMAT 11G10.3
        IF (IP.EQ.1) THEN 
          WRITE(IOUT,11) PARNAM(I),(BUF(J,I),J=1,NPE)
   11     FORMAT(1X,A,1X,1PG10.3,10(1X,G10.3):/(11X,11(1X,G10.3)))
C
C------------ FORMAT 10G11.4
        ELSEIF (IP.EQ.2) THEN
          WRITE(IOUT,21) PARNAM(I),(BUF(J,I),J=1,NPE)
   21     FORMAT(1X,A,1X,1PG11.4,9(1X,G11.4):/(11X,10(1X,G11.4)))
C
C------------ FORMAT 9G12.5
        ELSEIF (IP.EQ.3) THEN
          WRITE(IOUT,31) PARNAM(I),(BUF(J,I),J=1,NPE)
   31     FORMAT(1X,A,1X,1PG12.5,8(1X,G12.5):/(11X,9(1X,G12.5)))
C
C------------ FORMAT 8G13.6
        ELSEIF (IP.EQ.4) THEN
          WRITE(IOUT,41) PARNAM(I),(BUF(J,I),J=1,NPE)
   41     FORMAT(1X,A,1X,1PG13.6,7(1X,G13.6):/(11X,8(1X,G13.6)))
C
C------------ FORMAT 8G14.7
        ELSEIF (IP.EQ.5) THEN
          WRITE(IOUT,51) PARNAM(I),(BUF(J,I),J=1,NPE)
   51     FORMAT(1X,A,1X,1PG14.7,7(1X,G14.7):/(11X,8(1X,G14.7)))
C
C------------ FORMAT 6G10.3
        ELSEIF (IP.EQ.6) THEN 
          WRITE(IOUT,61) PARNAM(I),(BUF(J,I),J=1,NPE)
   61     FORMAT(1X,A,1X,1PG10.3,5(1X,G10.3):/(11X,6(1X,G10.3)))
C
C------------ FORMAT 5G11.4
        ELSEIF (IP.EQ.7) THEN
          WRITE(IOUT,71) PARNAM(I),(BUF(J,I),J=1,NPE)
   71     FORMAT(1X,A,1X,1PG11.4,4(1X,G11.4):/(11X,5(1X,G11.4)))
C
C------------ FORMAT 5G12.5
        ELSEIF (IP.EQ.8) THEN
          WRITE(IOUT,81) PARNAM(I),(BUF(J,I),J=1,NPE)
   81     FORMAT(1X,A,1X,1PG12.5,4(1X,G12.5):/(11X,5(1X,G12.5)))
C
C------------ FORMAT 4G13.6
        ELSEIF (IP.EQ.9) THEN
          WRITE(IOUT,91) PARNAM(I),(BUF(J,I),J=1,NPE)
   91     FORMAT(1X,A,1X,1PG13.6,3(1X,G13.6):/(11X,4(1X,G13.6)))
C
C------------ FORMAT 4G14.7
        ELSEIF (IP.EQ.10) THEN
          WRITE(IOUT,101) PARNAM(I),(BUF(J,I),J=1,NPE)
  101     FORMAT(1X,A,1X,1PG14.7,3(1X,G14.7):/(11X,4(1X,G14.7)))
C
        ENDIF
 1000 CONTINUE
C
C5------RETURN
      RETURN
      END
C **********************************************************************
      SUBROUTINE UCOLLBL(NLBL1,NLBL2,NSPACE,NCPL,NDIG,IOUT,PARNAM,NPE)
C
C-----VERSION 19980825 ERB
C     ******************************************************************
C     LABEL THE COLUMNS OF MATRIX PRINTOUT WITH PARAMETER NAMES
C        NLBL1 IS THE START COLUMN (NUMBER)
C        NLBL2 IS THE STOP COLUMN (NUMBER)
C        NSPACE IS NUMBER OF BLANK SPACES TO LEAVE AT START OF LINE
C        NCPL IS NUMBER OF COLUMNS PER LINE
C        NDIG IS NUMBER OF CHARACTERS IN EACH COLUMN FIELD
C        IOUT IS OUTPUT UNIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*1 DOT, SPACE, BF*200
      CHARACTER*10 PARNAM(NPE)
C
      DATA DOT,SPACE/'.',' '/
C     ------------------------------------------------------------------
C
C1------CALCULATE # OF COLUMNS TO BE PRINTED (NLBL), WIDTH
C1------OF A LINE (NTOT), NUMBER OF LINES (NWRAP).
      WRITE(IOUT,1)
    1 FORMAT(1X)
      NLBL = NLBL2-NLBL1+1
      N = NLBL
      IF(NLBL.GT.NCPL) N = NCPL
      LENPN = LEN(PARNAM(1))
      MINCOLSP = LENPN+1
      IF (NDIG.LT.MINCOLSP) THEN
        WRITE(IOUT,200) NDIG,MINCOLSP
 200    FORMAT(' SPECIFIED FIELD WIDTH TOO SMALL FOR PARNAM',
     &         '--STOP EXECUTION (UCOLLBL)',/
     &         ' NDIG = ',I2,'   MINCOLSP = ',I2)
        STOP
      ENDIF
      NTOT = NSPACE+LENPN+N*NDIG
      IND = (NDIG-LENPN-1)/2
      NWRAP = (NLBL-1)/NCPL+1
      J1 = NLBL1-NCPL
      J2 = NLBL1-1
C
C2------BUILD AND PRINT EACH LINE
      DO 40 N=1,NWRAP
C
C3------CLEAR THE BUFFER (BF).
        DO 20 I=1,130
          BF(I:I)=SPACE
   20   CONTINUE
        NBF = MINCOLSP+1+IND-NDIG
C
C4------DETERMINE FIRST (J1) AND LAST (J2) COLUMN # FOR THIS LINE.
        J1=J1+NCPL
        J2=J2+NCPL
        IF(J2.GT.NLBL2) J2=NLBL2
C5------LOAD THE COLUMN LABELS INTO THE BUFFER.
        DO 30 J=J1,J2
          NBF=NBF+NDIG
          NBF2 = NBF+LENPN-1
          BF(NBF:NBF2) = PARNAM(J)
   30   CONTINUE
C
C6------PRINT THE CONTENTS OF THE BUFFER (I.E. PRINT THE LINE).
        WRITE(IOUT,31) BF(1:NBF2)
   31   FORMAT(1X,A)
C
   40 CONTINUE
C
C7------PRINT A LINE OF DOTS (FOR ESTHETIC PURPOSES ONLY).
   50 CONTINUE
      WRITE(IOUT,51) (DOT,I=1,NTOT)
   51 FORMAT(1X,200A1)
C
C8------RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE URESPM(BUF,NTOT,IPRC,IOUT,DID,NDQP)
C
C-----VERSION 19980825 ERB
C     ******************************************************************
C     PRINT ONE NTOT*NTOT CORRELATION OR VARIANCE-COVARIANCE MATRIX FOR
C     RESIDUALS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*12 DID(NTOT)
      DOUBLE PRECISION BUF
      DIMENSION BUF(NDQP,NDQP)
C     ------------------------------------------------------------------
C
C2------MAKE SURE THE FORMAT CODE (IPRC) IS VALID
      IP=IPRC
      IF(IP.LT.1 .OR. IP.GT.10) IP=1
C
C3------LABEL COLUMNS WITH PARAMETER NAMES.
      IF(IP.EQ.1) CALL UCOLLBLR(1,NTOT,0,11,11,IOUT,DID,NTOT)
      IF(IP.EQ.2) CALL UCOLLBLR(1,NTOT,0,10,12,IOUT,DID,NTOT)
      IF(IP.EQ.3) CALL UCOLLBLR(1,NTOT,0,9,13,IOUT,DID,NTOT)
      IF(IP.EQ.4) CALL UCOLLBLR(1,NTOT,0,8,14,IOUT,DID,NTOT)
      IF(IP.EQ.5) CALL UCOLLBLR(1,NTOT,0,8,15,IOUT,DID,NTOT)
      IF(IP.EQ.6) CALL UCOLLBLR(1,NTOT,0,6,11,IOUT,DID,NTOT)
      IF(IP.EQ.7) CALL UCOLLBLR(1,NTOT,0,5,12,IOUT,DID,NTOT)
      IF(IP.EQ.8) CALL UCOLLBLR(1,NTOT,0,5,13,IOUT,DID,NTOT)
      IF(IP.EQ.9) CALL UCOLLBLR(1,NTOT,0,4,14,IOUT,DID,NTOT)
      IF(IP.EQ.10 ) CALL UCOLLBLR(1,NTOT,0,4,15,IOUT,DID,NTOT)
C
C4------LOOP THROUGH THE ROWS PRINTING EACH ONE IN ITS ENTIRETY.
      DO 1000 I=1,NTOT
C
C------------ FORMAT 11G10.3
        IF (IP.EQ.1) THEN 
          WRITE(IOUT,11) DID(I),(BUF(J,I),J=1,NTOT)
   11     FORMAT(1X,A,1X,1PG10.3,10(1X,G10.3):/(13X,11(1X,G10.3)))
C
C------------ FORMAT 10G11.4
        ELSEIF (IP.EQ.2) THEN
          WRITE(IOUT,21) DID(I),(BUF(J,I),J=1,NTOT)
   21     FORMAT(1X,A,1X,1PG11.4,9(1X,G11.4):/(13X,10(1X,G11.4)))
C
C------------ FORMAT 9G12.5
        ELSEIF (IP.EQ.3) THEN
          WRITE(IOUT,31) DID(I),(BUF(J,I),J=1,NTOT)
   31     FORMAT(1X,A,1X,1PG12.5,8(1X,G12.5):/(13X,9(1X,G12.5)))
C
C------------ FORMAT 8G13.6
        ELSEIF (IP.EQ.4) THEN
          WRITE(IOUT,41) DID(I),(BUF(J,I),J=1,NTOT)
   41     FORMAT(1X,A,1X,1PG13.6,7(1X,G13.6):/(13X,8(1X,G13.6)))
C
C------------ FORMAT 8G14.7
        ELSEIF (IP.EQ.5) THEN
          WRITE(IOUT,51) DID(I),(BUF(J,I),J=1,NTOT)
   51     FORMAT(1X,A,1X,1PG14.7,7(1X,G14.7):/(13X,8(1X,G14.7)))
C
C------------ FORMAT 6G10.3
        ELSEIF (IP.EQ.6) THEN 
          WRITE(IOUT,61) DID(I),(BUF(J,I),J=1,NTOT)
   61     FORMAT(1X,A,1X,1PG10.3,5(1X,G10.3):/(13X,6(1X,G10.3)))
C
C------------ FORMAT 5G11.4
        ELSEIF (IP.EQ.7) THEN
          WRITE(IOUT,71) DID(I),(BUF(J,I),J=1,NTOT)
   71     FORMAT(1X,A,1X,1PG11.4,4(1X,G11.4):/(13X,5(1X,G11.4)))
C
C------------ FORMAT 5G12.5
        ELSEIF (IP.EQ.8) THEN
          WRITE(IOUT,81) DID(I),(BUF(J,I),J=1,NTOT)
   81     FORMAT(1X,A,1X,1PG12.5,4(1X,G12.5):/(13X,5(1X,G12.5)))
C
C------------ FORMAT 4G13.6
        ELSEIF (IP.EQ.9) THEN
          WRITE(IOUT,91) DID(I),(BUF(J,I),J=1,NTOT)
   91     FORMAT(1X,A,1X,1PG13.6,3(1X,G13.6):/(13X,4(1X,G13.6)))
C
C------------ FORMAT 4G14.7
        ELSEIF (IP.EQ.10) THEN
          WRITE(IOUT,101) DID(I),(BUF(J,I),J=1,NTOT)
  101     FORMAT(1X,A,1X,1PG14.7,3(1X,G14.7):/(13X,4(1X,G14.7)))
C
        ENDIF
 1000 CONTINUE
C
C5------RETURN
      RETURN
      END
C **********************************************************************
      SUBROUTINE UCOLLBLR(NLBL1,NLBL2,NSPACE,NCPL,NDIG,IOUT,DID,NDTOT)
C
C-----VERSION 19980825 ERB
C     ******************************************************************
C     LABEL THE COLUMNS OF MATRIX PRINTOUT WITH OBSERVATION NAMES
C        NLBL1 IS THE START COLUMN (NUMBER)
C        NLBL2 IS THE STOP COLUMN (NUMBER)
C        NSPACE IS NUMBER OF BLANK SPACES TO LEAVE AT START OF LINE
C        NCPL IS NUMBER OF COLUMNS PER LINE
C        NDIG IS NUMBER OF CHARACTERS IN EACH COLUMN FIELD
C        IOUT IS OUTPUT UNIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*1 DOT, SPACE, BF*200
      CHARACTER*12 DID(NDTOT)
C
      DATA DOT,SPACE/'.',' '/
C     ------------------------------------------------------------------
C
C1------CALCULATE # OF COLUMNS TO BE PRINTED (NLBL), WIDTH
C1------OF A LINE (NTOT), NUMBER OF LINES (NWRAP).
      WRITE(IOUT,1)
    1 FORMAT(1X)
      NLBL = NLBL2-NLBL1+1
      N = NLBL
      IF(NLBL.GT.NCPL) N = NCPL
      LENPN = LEN(DID(1))
      MINCOLSP = LENPN+1
      IF (NDIG.LT.MINCOLSP) THEN
        WRITE(IOUT,200) NDIG,MINCOLSP
 200    FORMAT(' SPECIFIED FIELD WIDTH TOO SMALL FOR DID',
     &         '--STOP EXECUTION (UCOLLBL)',/
     &         ' NDIG = ',I2,'   MINCOLSP = ',I2)
        STOP
      ENDIF
      NTOT = NSPACE+LENPN+N*NDIG
      IND = (NDIG-LENPN-1)/2
      NWRAP = (NLBL-1)/NCPL+1
      J1 = NLBL1-NCPL
      J2 = NLBL1-1
C
C2------BUILD AND PRINT EACH LINE
      DO 40 N=1,NWRAP
C
C3------CLEAR THE BUFFER (BF).
        DO 20 I=1,130
          BF(I:I)=SPACE
   20   CONTINUE
        NBF = MINCOLSP+1+IND-NDIG
C
C4------DETERMINE FIRST (J1) AND LAST (J2) COLUMN # FOR THIS LINE.
        J1=J1+NCPL
        J2=J2+NCPL
        IF(J2.GT.NLBL2) J2=NLBL2
C5------LOAD THE COLUMN LABELS INTO THE BUFFER.
        DO 30 J=J1,J2
          NBF=NBF+NDIG
          NBF2 = NBF+LENPN-1
          BF(NBF:NBF2) = DID(J)
   30   CONTINUE
C
C6------PRINT THE CONTENTS OF THE BUFFER (I.E. PRINT THE LINE).
        WRITE(IOUT,31) BF(1:NBF2)
   31   FORMAT(1X,A)
C
   40 CONTINUE
C
C7------PRINT A LINE OF DOTS (FOR ESTHETIC PURPOSES ONLY).
   50 CONTINUE
      WRITE(IOUT,51) (DOT,I=1,NTOT)
   51 FORMAT(1X,200A1)
C
C8------RETURN
      RETURN
      END

