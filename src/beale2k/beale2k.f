C     Last change:  ERB  25 Feb 2002    2:20 pm
      PROGRAM BEALE2K
C
C       CALCULATES BEALE'S MEASURE OF NONLINEARITY
C
C       MARY C. HILL   15DEC1992
C         MODIFIED FROM COOLEY AND NAFF (1990, P. 187-189)
C
C       $Date: 1997/07/24 15:14:14 $
C       $Revision: 3.1 $
C
C       MODIFIED BEALE'S MEASURE PROGRAM BY R. L. COOLEY, USGS, DENVER,
C       COLO.   SEE COOLEY AND NAFF (1990, P. 187-198)
C       MODIFIED FOR MODFLOWP BY M. C. HILL 01JUN1992
C       MODIFIED FOR MODFLOW-2000 BY E.R. BANTA 8/12/1999
C         VARIABLES IN THE PARAMETER STATEMENT:
C          NDD = OR > SUM OF ALL OBSERVATIONS USED IN THE
C                     REGRESSION
C          NHD = OR > SUM OF THE HEAD OBSERVATIONS (NH IN LINE 5)
C          NDMHD = OR > SUM OF ALL OTHER OBSERVATIONS
C          NPD = OR > NUMBER OF PARAMETERS ESTIMATED IN THE REGRESSION
C          MPD = OR > MPR OF DATA LINE 5 OF MODFLOWP
C-------ASSIGN VERSION NUMBER AND DATE
      CHARACTER*40 VERSION
      PARAMETER (VERSION='1.2.01 10/03/2003')
      REAL B, BN, BOPT, FC, FL, FOBS, FOPT, FSTAT, PRM, SSE, SSTAT,
     &     SUM, SUMA, SUMAA, SUMB, SUMC, SUMCC, SUMD, SUMDD, SUMY,
cc     &     SUMYY, TEMP, TEMP1, TMP, VAR, W, WP, WQ, X
     &     SUMYY, TEMP, TEMP1, TMP, W, WP, WQ, X
Cerb change VAR from single to double precision
      DOUBLE PRECISION VAR
      INTEGER I, II, IIN1, IIN2, IMP, IOUT, IP, J, JJ, LN, M, MPD, MPR,
     &        N1, N2, NDD, NDMH, NDMHD, NH, NHD, NOBS, NPD, NPTS,
     &        NRES, NVAR
      CHARACTER*10 PARNAM
      CHARACTER*12 OBSNAM
      ALLOCATABLE :: PARNAM(:), OBSNAM(:)
      ALLOCATABLE :: BOPT(:), FOPT(:), B(:), FC(:), FL(:), X(:,:), W(:),
     &               WQ(:,:), WP(:), FOBS(:), PRM(:,:), SUMY(:), LN(:),
     &               WTP(:,:), NIPR(:), BPRI(:)
      COMMON /ITP   / IIN1, IOUT
cc      COMMON /FLT   / X
C
      CHARACTER*80 VERSN, OUTNAM
      CHARACTER*84 FN
      VERSN =
     &  '$Id: bealep.f,v 3.1 1997/07/24 15:14:14 rsregan Exp rsregan $'
      VERSN =
     &'@(#)BEALEP - MODFLOWP UTILITY TO CALCULATE BEALE''S MEASURE OF NO
     &NLINEARLITY'
      VERSN =
     &'@(#)BEALEP - MODIFIED FROM COOLEY AND NAFF (1990), BY M.C. HILL 0
     &1JUN1992'
      VERSN = '@(#)BEALEP - CONTACT: H2OSOFT@USGS.GOV'
      VERSN = '@(#)BEALEP - VERSION: 3.1x 1997/07/24'
C
C**FORMAT LIST
  400 FORMAT(' ERROR OPENING FILE "',A,'" -- STOP EXECUTION')
  480 FORMAT (35X,'BEALE-2000',//,
     &16X,'MODFLOW-2000 POST-PROCESSING PROGRAM TO CALCULATE',/,
     &25X,'BEALE''S MEASURE OF NONLINEARITY',/,
     &29X,'Version ',A/)
  490 FORMAT(
     &' ERROR FINDING INPUT FILES.  OUTNAM MUST BE SPECIFIED AS A',/,
     &' STRING OTHER THAN "NONE" IN THE OBS FILE.  THE FILE',/,
     &' BEALE.ERR MAY CONTAIN ADDITIONAL DIAGNOSTIC INFORMATION')
  500 FORMAT (5I10,2F14.0)
  505 FORMAT (8F15.0)
  510 FORMAT (
     &' NUMBER OF ESTIMATED PARAMETERS...................:',I5,/,       (NVAR)
     &' NUMBER OF HEAD OBSERVATIONS......................:',I5,/,       (NH)
     &' NUMBER OF ALL OTHER OBSERVATIONS.................:',I5,/,       (NDMH)
     &' TOTAL NUMBER OF OBSERVATIONS.....................:',I5,//,      (NOBS)
     &' NUMBER OF PRIOR-INFORMATION EQUATIONS............:',I5,/,       (MPR)
     &' NUMBER OF PRIOR INFORMATION W/ FULL WEIGHT MATRIX:',I5,/,       (IPR)
     &' NUMBER OF DATA SETS USED FOR BEALES MEASURE......:',I5,//,      (NPTS)
     &' CALCULATED ERROR VARIANCE........................:',G12.5)      (VAR)
  515 FORMAT (/,23X,'OPTIMUM PARAMETERS',/,
     &4X,2('NO. NAME',10X,'BOPT',8X))
  520 FORMAT (/,6X,
     &        'DEPENDENT VARIABLES COMPUTED WITH OPTIMUM PARAMETERS',/,
     &        4X,2('NO. OBSERVATION',5X,'FOPT',8X))
  525 FORMAT (/,21X,'PARAMETERS FOR SAMPLE NO.',I4,/,4X,
     &        2('NO. NAME',12X,'B',9X))
  530 FORMAT (/,9X,'DEPENDENT VARIABLES COMPUTED FOR SAMPLE NO.',I4,/,
     &        4X,2('NO. OBSERVATION',6X,'FC',9X))
  532 FORMAT (/,
     &' F STATISTIC FOR BEALE''S MEASURE SET TO...........:',G12.5)
  535 FORMAT (/,' SENSITIVITIES FOR OPTIMUM PARAMETERS')
  540 FORMAT (/,2X,
     &        'LINEARIZED DEPENDENT VARIABLES COMPUTED FOR SAMPLE NO.',
     &        I4,/,4X,2('NO.',1X,'OBSERVATION',6X,'FL',9X))
  545 FORMAT (/,' USING FSTAT =',G12.5,', BEALES MEASURE =',G12.5,/,
     &        ' IF BEALES MEASURE IS GREATER THAN',G11.2,
     &        ', THE MODEL IS NONLINEAR.',/,
     &        ' IF BEALES MEASURE IS LESS THAN ',G11.2,
     &        ', THE MODEL IS EFFECTIVELY LINEAR,',/,
     &        ' AND LINEAR CONFIDENCE INTERVALS ',
     &        'ARE FAIRLY ACCURATE IF THE RESIDUALS ARE',/,
     &        ' NORMALLY DISTRIBUTED.',/)
  550 FORMAT (/,' SS((FC-FOPT)*W**.5) = ',G11.5,/,
     &        ' SS((FL-FOPT)*W**.5) =',G12.5)
  555 FORMAT (/,11X,'RELIABILITY WEIGHTS FOR SAMPLE INFORMATION',/,4X,
     &        2('NO. OBSERVATION',6X,'W',10X))
  560 FORMAT (16I5)
  565 FORMAT (/,12X,'STARTING PARAMETERS FROM DATA SET 8',/,4X,
     &        3('NO.',8X,'IPR',10X))
  570 FORMAT (/,14X,'STANDARD DEVIATIONS OF PRIOR INFORMATION'/,4X,
     &        3('NO.',10X,'WP',9X))
  575 FORMAT (/,' EV =',G12.5)
  580 FORMAT (6F13.0)
  585 FORMAT (/,' DIMENSIONS EXCEED DIMENSIONS IN PARAMETER STATEMENT',
     &        ' -- STOP EXECUTION')
  590 FORMAT (/,11X,'OBSERVED VALUES OF THE DEPENDENT VARIABLES',/,4X,
     &        2('NO. OBSERVATION',5X,'FOBS',8X))
  595 FORMAT (8F13.0)
  600 FORMAT (/,' MULTIPLE PRIOR NUMBER',I3,' ESTIMATE(LOG IF LN>0)=',
     &        G10.3,', WEIGHT=',G10.3)
  605 FORMAT (' THE FOLLOWING TABLE SHOWS VALUES OF THE',
     &        ' STATISTIC DESCRIBED BY COOLEY AND',/,
     &        ' NAFF (1990,P.174,TOP OF RIGHT COLUMN).',
     &        ' THE STATISTIC EQUALS THE NONLINEAR SUM',/,
     &        ' OF SQUARED ERRORS EVALUATED FOR EACH DATA',
     &        ' SET (NSSE) MINUS THE SUM OF SQUARED',/,
     &        ' ERRORS FOR THE OPTIMUM PARAMETER VALUES (',G10.3,').',/,
     &      ' IF THE MODEL IS LINEAR, THE STATISTIC SHOULD BE CLOSE TO '
     &      ,G10.3,'.',/,
     &      ' IF THE CORRELATIONS BETWEEN PARAMETERS IS SMALL, THE',
     &      ' TABLE SHOWS WHICH ',/,' INDIVIDUAL',
     &     ' PARAMETERS ARE MOST NONLINEAR. THE FIRST PAIR OF PARAMETER'
     &     ,' SETS ARE ',/,' RELATED TO THE',
     &     ' FIRST PARAMETER, THE SECOND PAIR ARE RELATED TO THE SECOND'
     &     ,/,' PARAMETER, AND SO ON.',//,' PARAMETER',17X,
     &     '      STATISTIC       PERCENT',/,
     &     '   SET       NSSE    STATISTIC   -',G10.3,'   DIFFERENCE',/)
  610 FORMAT (I6,5X,2G10.3,G12.3,F12.2)
  615 FORMAT (8I10)
  620 FORMAT (/,9X,'PARAMETER',/,3X,'NO.',5X,'NAME',7X,'LN')
  625 FORMAT (1X,I5,3X,A,3X,I2)
  630 FORMAT (6(A12,1X))
  635 FORMAT (' END OF BEALE2.DAT',/,'  MODFLOW-2000 SOLUTION MUST NOT',
     &        ' HAVE CONVERGED FOR NEXT SET OF PARAMETER VALUES.',/,
     &        ' CHECK PARAMETER VALUES; NEGATIVE VALUES CAN BE MADE TO',
     &        ' STAY POSITIVE BY ESTIMATING THE LOG TRANSFORM')
  640 FORMAT (20A4)
  645 FORMAT (6(A10,1X))
  650 FORMAT (3(A10,1X,G12.3))
  660 FORMAT(/,' OBSERVATIONS WITH A FULL WEIGHT MATRIX:')
  670 FORMAT(/,' PRIOR INFORMATION WITH FULL WEIGHT MATRIX')
  680 FORMAT(/,'   FULL WEIGHT MATRIX:')
  690 FORMAT(3X,'PARAMETERS INVOLVED AND PRIOR VALUES:')
  700 FORMAT(5X,A,2X,1PG13.6)
 1410 FORMAT(/,1X,'*** ERROR: ',A,/,1X,'EXCEEDS ARRAY LIMIT OF ',
     &       I8,/,1X,'INCREASE ',A,' AND RECOMPILE PROGRAM')
C
C     WRITE PROGRAM NAME AND VERSION TO SCREEN
      WRITE (*,*)
      WRITE (*,480) VERSION
C**DEFINE INPUT AND OUTPUT UNIT NUMBERS
      IIN1 = 11
      IIN2 = 2
      IOUT = 3
      OPEN (IOUT,FILE='BEALE.ERR',STATUS='UNKNOWN')
C     FIND INPUT FILE NAMES BY READING NAME AND OBS FILES,
C     AND READING OUTNAM
      CALL FINDINP(IOUT,OUTNAM,IIN1)
      IF (OUTNAM.EQ.'NONE') THEN
        WRITE(*,490)
        CLOSE(IOUT)
        STOP
      ELSE
        CLOSE(IOUT,STATUS='DELETE')
      ENDIF
      LENGNAM = NONB_LEN(OUTNAM,80)
C**OPEN FILES
      FN = OUTNAM(1:LENGNAM)//'.#be'
      OPEN (IOUT,FILE=FN,STATUS='UNKNOWN')
      WRITE (IOUT,480) VERSION
      FN = OUTNAM(1:LENGNAM)//'._b1'
      OPEN (IIN1,FILE=FN,STATUS='OLD',ACCESS='SEQUENTIAL',
     &      FORM='FORMATTED',ERR=2,IOSTAT=IOS)
    2 CONTINUE
      IF (IOS.NE.0) THEN
        LENGNAM = NONB_LEN(FN,84)
        WRITE (*,400) FN(1:LENGNAM)
        STOP
      ENDIF
      FN = OUTNAM(1:LENGNAM)//'._b2'
      OPEN (IIN2,FILE=FN,STATUS='OLD',ACCESS='SEQUENTIAL',
     &      FORM='FORMATTED',ERR=4,IOSTAT=IOS)
    4 CONTINUE
      IF (IOS.NE.0) THEN
        LENGNAM = NONB_LEN(FN,84)
        WRITE (*,400) FN(1:LENGNAM)
        STOP
      ENDIF
C**READ BASE DATA
      IPR = 0
C     READ ITEM 1 FROM _b1 FILE
      READ (IIN1,500) NVAR, NOBS, NDMH, MPR, IPR, VAR
      NH = NOBS - NDMH
      NRES = NVAR
      NPTS = 2*NVAR
      WRITE (IOUT,510) NVAR, NH, NDMH, NOBS, MPR, IPR, NPTS, VAR
      NDD = NOBS
      NHD = NH
      NDMHD = NDMH
      NPD = NVAR
      MPD = MPR
      IPD = IPR
C
C     DYNAMICALLY ALLOCATE ARRAYS
      ALLOCATE (PARNAM(NPD))
      ALLOCATE (OBSNAM(NDD))
      ALLOCATE (BOPT(NPD), FOPT(NDD), B(NPD), FC(NDD), FL(NDD),
     &          X(NPD,NDD), W(NHD), WQ(NDMHD,NDMHD), WP(NPD+MPD),
     &          FOBS(NDD), PRM(NPD+1,MPD), SUMY(2*NPD),
     &          LN(NPD), WTP(IPD,IPD), NIPR(IPD), BPRI(IPD))
C
C    DETERMINE THE VALUE OF THE F STATISTIC
      IDOF = NOBS + IPR + MPR - NVAR
      CALL FSTT(NVAR,IDOF,FSTAT)
      WRITE (IOUT,532) FSTAT
C     CHECK DIMENSIONS
      IF (NVAR.GT.NPD .OR. NOBS.GT.NDD .OR. MPR.GT.MPD .OR.
     &    NH.GT.NHD .OR. NDMH.GT.NDMHD) THEN
        WRITE (IOUT,585)
        STOP
      ENDIF
C     READ AND WRITE PARAMETER NAMES
C     READ ITEMS 2 AND 3 FROM _b1 FILE
      READ (IIN1,645) (PARNAM(I),I=1,NVAR)
      READ (IIN1,580) (BOPT(J),J=1,NVAR)
      WRITE (IOUT,515)
      CALL PRTOTB10(BOPT,PARNAM,NVAR)
C     READ AND WRITE OBSERVATION NAMES AND SIMULATED VALUES
C     READ ITEMS 4 AND 5 FROM _b1 FILE
      READ (IIN1,630) (OBSNAM(I),I=1,NOBS)
      READ (IIN1,580) (FOPT(I),I=1,NOBS)
      WRITE (IOUT,520)
      CALL PRTOTB12(FOPT,OBSNAM,NOBS)
C     READ AND WRITE OBSERVED VALUES
C     READ ITEM 6 FROM _b1 FILE
      READ (IIN1,580) (FOBS(I),I=1,NOBS)
      WRITE (IOUT,590)
      CALL PRTOTB12(FOBS,OBSNAM,NOBS)
C     READ AND WRITE WEIGHTS FOR HEADS
      WRITE (IOUT,555)
      IF (NH.GT.0) THEN
C       READ ITEM 7 FROM _b1 FILE
        READ (IIN1,505) (W(I),I=1,NH)
        WRITE (IOUT,*) ' HEADS:'
        CALL PRTOTB12(W,OBSNAM,NH)
      ENDIF
C    READ AND WRITE FULL WEIGHT MATRIX ON OBSERVATIONS
      IF (NDMH.GT.0) THEN
        DO 10 I = 1, NDMH
C         READ ITEM 8 FROM _b1 FILE
          READ (IIN1,505) (WQ(I,J),J=1,NDMH)
   10   CONTINUE
        WRITE (IOUT,660)
        CALL PRTOTD(WQ,NDMH,NDMH,NDMHD,OBSNAM,NH)
      ENDIF
C     READ AND WRITE SENSITIVITIES FOR OBSERVATIONS
      DO 20 J = 1, NOBS
C       READ ITEM 9 FROM _b1 FILE
        READ (IIN1,580) (X(I,J),I=1,NVAR)
   20 CONTINUE
      WRITE (IOUT,535)
      CALL PRTOTX(X,NVAR,NOBS,NPD,OBSNAM,PARNAM)
C     READ AND WRITE PRIOR INFORMATION FROM EQUATIONS
      IF (MPR.GT.0) THEN
        DO 30 IMP = 1, MPR
C         READ ITEM 10 FROM _b1 FILE
          READ (IIN1,505) (PRM(IP,IMP),IP=1,NVAR+1), WP(NVAR+IMP)
          WRITE (IOUT,600) IMP, PRM(NVAR+1,IMP), WP(NVAR+IMP)
          CALL PRTOTB10(PRM(1,IMP),PARNAM,NVAR)
   30   CONTINUE
      ENDIF
C     READ AND WRITE PRIOR INFORMATION WITH FULL WEIGHT MATRIX
      IF (IPR.GT.0) THEN
        WRITE (IOUT,670)
C       READ ITEM 11 AND FIRST ITEM 12 FROM _b1 FILE
        READ (IIN1,615) (NIPR(I),I=1,IPR)
        READ (IIN1,580) (BPRI(I),I=1,IPR)
        WRITE (IOUT,690)
        WRITE (IOUT,700) (PARNAM(NIPR(I)),BPRI(I),I=1,IPR)
        DO 35 I = 1, IPR
C         READ SECOND ITEM 12 FROM _b1 FILE
          READ (IIN1,580) (WTP(IP,I),IP=1,IPR)
   35   CONTINUE
        WRITE (IOUT,680)
        CALL PRTOTP(WTP,IPR,IPR,IPD,PARNAM,NIPR,IPD,NPD)
      ENDIF
C     READ AND WRITE FLAGS INDICATING LOG TRANSFORMED PARAMETERS
C     READ ITEM 13 FROM _b1 FILE
      READ (IIN1,615) (LN(I),I=1,NVAR)
      WRITE (IOUT,620)
      WRITE (IOUT,625) (I,PARNAM(I),LN(I),I=1,NVAR)
      DO 40 IP = 1, NVAR
        IF (LN(IP).NE.0) BOPT(IP) = ALOG(BOPT(IP))
   40 CONTINUE
C**READ DATA FOR EACH SAMPLE AND COMPUTE MODIFIED BEALE'S MEASURE, BN,
C**AND THE STATISTIC FROM COOLEY AND NAFF(1990,P.174,TOP OF RIGHT COLUMN)
      SUMA = 0.0
      SUMB = 0.0
      DO 180 M = 1, NPTS
C       READ A PARAMETER SET
        READ (IIN2,595,END=190) (B(J),J=1,NVAR)
        WRITE (IOUT,525) M
        CALL PRTOTB10(B,PARNAM,NVAR)
        DO 50 I = 1, NVAR
          IF (LN(I).NE.0) B(I) = ALOG(B(I))
   50   CONTINUE
C       READ VALUES SIMULATED WITH THIS PARAMETER SET
        READ (IIN2,580) (FC(I),I=1,NOBS)
        WRITE (IOUT,530) M
        CALL PRTOTB12(FC,OBSNAM,NOBS)
C       DO CALCULATIONS FOR THIS SET
        SUMC = 0.0
        SUMD = 0.0
        SUMY(M) = 0.0
C       OBSERVATIONS WITH DIAGONAL WEIGHT MATRIX
        IF (NH.GT.0) THEN
          DO 70 J = 1, NH
            SUM = FOPT(J)
            DO 60 I = 1, NVAR
              SUM = SUM + X(I,J)*(B(I)-BOPT(I))
   60       CONTINUE
            FL(J) = SUM
            TMP = FC(J) - SUM
            SUMA = SUMA + TMP*W(J)*TMP
            TMP = FC(J) - FOPT(J)
            SUMC = SUMC + TMP*W(J)*TMP
            TMP = SUM - FOPT(J)
            SUMD = SUMD + TMP*W(J)*TMP
            TMP = FOBS(J) - FC(J)
            SUMY(M) = SUMY(M) + TMP*W(J)*TMP
   70     CONTINUE
        ENDIF
C           OBSERVATIONS WITH FULL WEIGHT MATRIX
        IF (NDMH.GT.0) THEN
          N1 = NH + 1
          N2 = NH + NDMH
          DO 90 J = N1, N2
            SUM = FOPT(J)
            DO 80 I = 1, NVAR
              SUM = SUM + X(I,J)*(B(I)-BOPT(I))
   80       CONTINUE
            FL(J) = SUM
   90     CONTINUE
          DO 110 I = N1, N2
            II = I - NH
            SUMAA = 0.0
            SUMCC = 0.0
            SUMDD = 0.0
            SUMYY = 0.0
            DO 100 J = N1, N2
              JJ = J - NH
              SUMAA = SUMAA + WQ(II,JJ)*(FC(J)-FL(J))
              SUMCC = SUMCC + WQ(II,JJ)*(FC(J)-FOPT(J))
              SUMDD = SUMDD + WQ(II,JJ)*(FL(J)-FOPT(J))
              SUMYY = SUMYY + WQ(II,JJ)*(FOBS(J)-FC(J))
  100       CONTINUE
            SUMA = SUMA + SUMAA*(FC(I)-FL(I))
            SUMC = SUMC + SUMCC*(FC(I)-FOPT(I))
            SUMD = SUMD + SUMDD*(FL(I)-FOPT(I))
            SUMY(M) = SUMY(M) + SUMYY*(FOBS(I)-FC(I))
  110     CONTINUE
        ENDIF
C           PRIOR INFORMATION FROM EQUATIONS
        IF (MPR.GT.0) THEN
          DO 140 J = 1, MPR
            TEMP = 0.0
            TEMP1 = 0.0
            DO 130 I = 1, NVAR
              TEMP1 = TEMP1 + PRM(I,J)*BOPT(I)
              TEMP = TEMP + PRM(I,J)*B(I)
  130       CONTINUE
            TMP = TEMP - TEMP1
            TMP = TMP*WP(NVAR+J)*TMP
            SUMC = SUMC + TMP
            SUMD = SUMD + TMP
            TMP = PRM(NVAR+1,J) - TEMP
            SUMY(M) = SUMY(M) + TMP*WP(NVAR+J)*TMP
  140     CONTINUE
        ENDIF
C           PRIOR WITH FULL WEIGHT MATRIX
        IF (IPR.GT.0) THEN
          DO 170 I = 1, IPR
            SUMCC = 0.0
            SUMDD = 0.0
            SUMYY = 0.0
            DO 160 J = 1, IPR
              SUMCC = SUMCC + WTP(I,J)*( B(NIPR(J))-BOPT(NIPR(J)) )
              SUMDD = SUMDD + WTP(I,J)*( B(NIPR(J))-BOPT(NIPR(J)) )
              SUMYY = SUMYY + WTP(I,J)*( BPRI(J)-B(NIPR(J)) )
  160       CONTINUE
            SUMC = SUMC + SUMCC*( B(NIPR(I))-BOPT(NIPR(I)) )
            SUMD = SUMD + SUMDD*( B(NIPR(I))-BOPT(NIPR(I)) )
            SUMY(M) = SUMY(M) + SUMYY*( BPRI(I)-B(NIPR(I)) ) ! Check with Steen
  170     CONTINUE
        ENDIF
C      WRITE RESULTS FOR THIS DATA SET
        WRITE (IOUT,540) M
        CALL PRTOTB12(FL,OBSNAM,NOBS)
        WRITE (IOUT,550) SUMC, SUMD
        SUMB = SUMB + SUMD*SUMD
  180 CONTINUE
C     END LOOP FOR DATA SETS; CALCULATE AND WRITE RESULTS
      TMP = NRES
      BN = TMP*VAR*SUMA/SUMB
      WRITE (IOUT,545) FSTAT, BN, 1./FSTAT, .09/FSTAT
      SSTAT = VAR*NRES*FSTAT
      SSE = VAR*(NOBS+MPR+IPR-NVAR)
      WRITE (IOUT,605) SSE, SSTAT, SSTAT
      WRITE (IOUT,610) (I,SUMY(I),SUMY(I)-SSE,SUMY(I)-SSE-SSTAT,
     &                 100.*(SUMY(I)-SSE-SSTAT)/SSTAT,I=1,NPTS)
      GOTO 200
  190 WRITE (IOUT,635)
  200 STOP
      END
C=======================================================================
      SUBROUTINE PRTOTB4(VAL,VID,NO)
C**PRINT VALUES IN THREE GROUPS OF THREE COLUMNS
      INTEGER IIN1, IOUT, K, L, NO, NR
      REAL VAL
      CHARACTER*4 VID(NO)
      DIMENSION VAL(NO)
      COMMON /ITP   / IIN1, IOUT
      NR = NO/3
      IF (3*NR.NE.NO) NR = NR + 1
      DO 10 K = 1, NR
        WRITE (IOUT,500) (L,VID(L),VAL(L),L=K,NO,NR)
   10 CONTINUE
      RETURN
  500 FORMAT (3X,3(I3,2X,A4,1X,G11.5,3X))
      END
C=======================================================================
      SUBROUTINE PRTOTB10(VAL,VID,NO)
C**PRINT VALUES IN TWO GROUPS OF THREE COLUMNS
      INTEGER IIN1, IOUT, K, L, NO, NR
      REAL VAL
      CHARACTER*10 VID(NO)
      DIMENSION VAL(NO)
      COMMON /ITP   / IIN1, IOUT
      NR = NO/2
      IF (2*NR.NE.NO) NR = NR + 1
      DO 10 K = 1, NR
        WRITE (IOUT,500) (L,VID(L),VAL(L),L=K,NO,NR)
   10 CONTINUE
      RETURN
  500 FORMAT (3X,2(I3,2X,A,1X,G11.5,3X))
      END
C=======================================================================
      SUBROUTINE PRTOTB12(VAL,VID,NO)
C**PRINT VALUES IN TWO GROUPS OF THREE COLUMNS
      INTEGER IIN1, IOUT, K, L, NO, NR
      REAL VAL
      CHARACTER*12 VID(NO)
      DIMENSION VAL(NO)
      COMMON /ITP   / IIN1, IOUT
      NR = NO/2
      IF (2*NR.NE.NO) NR = NR + 1
      DO 10 K = 1, NR
        WRITE (IOUT,500) (L,VID(L),VAL(L),L=K,NO,NR)
   10 CONTINUE
      RETURN
  500 FORMAT (3X,2(I3,2X,A,1X,G11.5,3X))
      END
C=======================================================================
      SUBROUTINE PRTOTD(W,NR,NC,NRD,OBSNAM,NH)
C     PRINT A FULL WEIGHT MATRIX FOR OBSERVATIONS DIVIDED VERTICALLY
C     INTO FIVE-COLUMN BLOCKS
      INTEGER I, IIN1, IOUT, J, J10, K, NC, NH, NR, NRD
      REAL W(NRD,NC)
      CHARACTER*12 OBSNAM(NC+NH)
      COMMON /ITP   / IIN1, IOUT
C
  500 FORMAT (/,1X,'OBSERVATION',2X,5(1X,A12))
  505 FORMAT (1X,A12,1X,5(1X,G12.5))
  510 FORMAT (1X,12('-'),1X,5(1X,A12))
C
      DO 20 K = 1, NC, 5
        J10 = K + 4
        IF (J10.GT.NC) J10 = NC
        WRITE (IOUT,500) (OBSNAM(J),J=K+NH,J10+NH)
        WRITE (IOUT,510) ('------------',J=K,J10)
        DO 10 I = 1, NR
          WRITE (IOUT,505) OBSNAM(I+NH), (W(I,J),J=K,J10)
   10   CONTINUE
   20 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE PRTOTP(W,NR,NC,NRD,PARNAM,NIPR,IPD,NPD)
C     PRINT A FULL WEIGHT MATRIX FOR PRIOR INFORMATION DIVIDED
C     VERTICALLY INTO FIVE-COLUMN BLOCKS
      INTEGER I, IIN1, IOUT, J, J10, K, NC, NR, NRD
      INTEGER NIPR(IPD)
      REAL W(NRD,NC)
      CHARACTER*10 PARNAM(NPD)
      COMMON /ITP   / IIN1, IOUT
C
  500 FORMAT (/,1X,'PARAMETER',2X,5(2X,A10,1X))
  505 FORMAT (1X,A10,1X,5(1X,G12.5))
  510 FORMAT (1X,10('-'),1X,5(1X,A12))
C
      DO 20 K = 1, NC, 5
        J10 = K + 4
        IF (J10.GT.NC) J10 = NC
        WRITE (IOUT,500) (PARNAM(NIPR(J)),J=K,J10)
        WRITE (IOUT,510) ('------------',J=K,J10)
        DO 10 I = 1, NR
          WRITE (IOUT,505) PARNAM(NIPR(I)), (W(I,J),J=K,J10)
   10   CONTINUE
   20 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE PRTOTX(X,NR,NC,NRD,OBSNAM,PARNAM)
C**PRINT MATRICES DIVIDED VERTICALLY INTO FIVE-COLUMN BLOCKS
      INTEGER I, IIN1, IOUT, J, J10, K, NC, NR, NRD
      DIMENSION X(NRD,NC)
      CHARACTER*10 PARNAM(NR)
      CHARACTER*12 OBSNAM(NC)
      COMMON /ITP   / IIN1, IOUT
      DO 20 K = 1, NC, 5
        J10 = K + 4
        IF (J10.GT.NC) J10 = NC
        WRITE (IOUT,500) (OBSNAM(J),J=K,J10)
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
      SUBROUTINE FSTT(NP,IDOF,TST)
C     ******************************************************************
C     DETERMINE THE VALUE OF THE F STATISTIC NEEDED TO CALCULATE
C     BEALE'S MEASURE OF LINEARITY AND SCHEFFE CONFIDENCE INTERVALS
C     -- MODIFIED FROM UCODE VERSION -- ERB 9/23/99
C     ******************************************************************
C        SPECIFICATIONS:
      REAL TST, T
      INTEGER IDOF, I, J, ITABLE1, ITABLE2, NP
C     ------------------------------------------------------------------
      DIMENSION ITABLE1(19), ITABLE2(34), T(19,34)
C     NP is indicator for table1
      DATA (ITABLE1(I),I=1,19)/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12,
     &      15, 20, 24, 30, 40, 60, 120, 32000/
C     IDOF is indicator for table2
      DATA (ITABLE2(I),I=1,34)/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
     &      13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
     &      28, 29, 30, 40, 60, 120, 32000/
C     TABLE IS SET UP AS (NP,IDOF)
C
      DATA (T(1,I),I=1,34)
     &     /161.4, 18.51, 10.13, 7.71, 6.61, 5.99, 5.59,
     &      5.32, 5.12, 4.96, 4.84, 4.75, 4.67, 4.60, 4.54, 4.49,
     &      4.45, 4.41, 4.38, 4.35, 4.32, 4.30, 4.28, 4.26, 4.24,
     &      4.23, 4.21, 4.20, 4.18, 4.17, 4.08, 4.00, 3.92, 3.84/
C
      DATA (T(2,I),I=1,34)
     &     /199.5,19.00,9.55,6.94,5.79,5.14,4.74,
     &      4.46, 4.26, 4.10, 3.98, 3.89, 3.81, 3.74, 3.68, 3.63,
     &      3.59, 3.55, 3.52, 3.49, 3.47, 3.44, 3.42, 3.40, 3.39,
     &      3.37, 3.35, 3.34, 3.33, 3.32, 3.23, 3.15, 3.07, 3.00/
C
      DATA (T(3,I),I=1,34)/215.7,19.16,9.28,6.59,5.41,4.76,4.35,
     &      4.07, 3.86, 3.71, 3.59, 3.49, 3.41, 3.34, 3.29, 3.24,
     &      3.20, 3.16, 3.13, 3.10, 3.07, 3.05, 3.03, 3.01, 2.99,
     &      2.98, 2.96, 2.95, 2.93, 2.92, 2.84, 2.76, 2.68, 2.60/
C
      DATA (T(4,I),I=1,34)/224.6,19.25,9.12,6.39,5.19,4.53,4.12,
     &      3.84, 3.63, 3.48, 3.36, 3.26, 3.18, 3.11, 3.06, 3.01,
     &      2.96, 2.93, 2.90, 2.87, 2.84, 2.82, 2.80, 2.78, 2.76,
     &      2.74, 2.73, 2.71, 2.70, 2.69, 2.61, 2.53, 2.45, 2.37/
C
      DATA (T(5,I),I=1,34)/230.2,19.30,9.01,6.26,5.05,4.39,3.97,
     &      3.69, 3.48, 3.33, 3.20, 3.11, 3.03, 2.96, 2.90, 2.85,
     &      2.81, 2.77, 2.74, 2.71, 2.68, 2.66, 2.64, 2.62, 2.60,
     &      2.59, 2.57, 2.56, 2.55, 2.53, 2.45, 2.37, 2.29, 2.21/
C
      DATA (T(6,I),I=1,34)/234.0,19.33,8.94,6.16,4.95,4.28,3.87,
     &      3.58, 3.37, 3.22, 3.09, 3.00, 2.92, 2.85, 2.79, 2.74,
     &      2.70, 2.66, 2.63, 2.60, 2.57, 2.55, 2.53, 2.51, 2.49,
     &      2.47, 2.46, 2.45, 2.43, 2.42, 2.34, 2.25, 2.17, 2.10/
C
      DATA (T(7,I),I=1,34)/236.8,19.35,8.89,6.09,4.88,4.21,3.79,
     &      3.50, 3.29, 3.14, 3.01, 2.91, 2.83, 2.76, 2.71, 2.66,
     &      2.61, 2.58, 2.54, 2.51, 2.49, 2.46, 2.44, 2.42, 2.40,
     &      2.39, 2.37, 2.36, 2.35, 2.33, 2.25, 2.17, 2.09, 2.01/
C
      DATA (T(8,I),I=1,34)/238.9,19.37,8.85,6.04,4.82,4.15,3.73,
     &      3.44, 3.23, 3.07, 2.95, 2.85, 2.77, 2.70, 2.64, 2.59,
     &      2.55, 2.51, 2.48, 2.45, 2.42, 2.40, 2.37, 2.36, 2.34,
     &      2.32, 2.31, 2.29, 2.28, 2.27, 2.18, 2.10, 2.02, 1.94/
C
      DATA (T(9,I),I=1,34)/240.5,19.38,8.81,6.00,4.77,4.10,3.68,
     &      3.39, 3.18, 3.02, 2.90, 2.80, 2.71, 2.65, 2.59, 2.54,
     &      2.49, 2.46, 2.42, 2.39, 2.37, 2.34, 2.32, 2.30, 2.28,
     &      2.27, 2.25, 2.24, 2.22, 2.21, 2.12, 2.04, 1.96, 1.88/
C
      DATA (T(10,I),I=1,34)/241.9,19.40,8.79,5.96,4.74,4.06,3.64,
     &      3.35, 3.14, 2.98, 2.85, 2.75, 2.67, 2.60, 2.54, 2.49,
     &      2.45, 2.41, 2.38, 2.35, 2.32, 2.30, 2.27, 2.25, 2.24,
     &      2.22, 2.20, 2.19, 2.18, 2.16, 2.08, 1.99, 1.91, 1.83/
C
      DATA (T(11,I),I=1,34)/243.9,19.41,8.74,5.91,4.68,4.00,3.57,
     &      3.28, 3.07, 2.91, 2.79, 2.69, 2.60, 2.53, 2.48, 2.42,
     &      2.38, 2.34, 2.31, 2.28, 2.25, 2.23, 2.20, 2.18, 2.16,
     &      2.15, 2.13, 2.12, 2.10, 2.09, 2.00, 1.92, 1.83, 1.75/
C
      DATA (T(12,I),I=1,34)/245.9,19.43,8.70,5.86,4.62,3.94,3.51,
     &      3.22, 3.01, 2.85, 2.72, 2.62, 2.53, 2.46, 2.40, 2.35,
     &      2.31, 2.27, 2.23, 2.20, 2.18, 2.15, 2.13, 2.11, 2.09,
     &      2.07, 2.06, 2.04, 2.03, 2.01, 1.92, 1.84, 1.75, 1.67/
C
      DATA (T(13,I),I=1,34)/248.0,19.45,8.66,5.80,4.56,3.87,3.44,
     &      3.15, 2.94, 2.77, 2.65, 2.54, 2.46, 2.39, 2.33, 2.28,
     &      2.23, 2.19, 2.16, 2.12, 2.10, 2.07, 2.05, 2.03, 2.01,
     &      1.99, 1.97, 1.96, 1.94, 1.93, 1.84, 1.75, 1.66, 1.57/
C
      DATA (T(14,I),I=1,34)/249.1,19.45,8.64,5.77,4.53,3.84,3.41,
     &      3.12, 2.90, 2.74, 2.61, 2.51, 2.42, 2.35, 2.29, 2.24,
     &      2.19, 2.15, 2.11, 2.08, 2.05, 2.03, 2.01, 1.98, 1.96,
     &      1.95, 1.93, 1.91, 1.90, 1.89, 1.79, 1.70, 1.61, 1.52/
C
      DATA (T(15,I),I=1,34)/250.1,19.46,8.62,5.75,4.50,3.81,3.38,
     &      3.08, 2.86, 2.70, 2.57, 2.47, 2.38, 2.31, 2.25, 2.19,
     &      2.15, 2.11, 2.07, 2.04, 2.01, 1.98, 1.96, 1.94, 1.92,
     &      1.90, 1.88, 1.87, 1.85, 1.84, 1.74, 1.65, 1.55, 1.46/
C
      DATA (T(16,I),I=1,34)/251.1,19.47,8.59,5.72,4.46,3.77,3.34,
     &      3.04, 2.83, 2.66, 2.53, 2.43, 2.34, 2.27, 2.20, 2.15,
     &      2.10, 2.06, 2.03, 1.99, 1.96, 1.94, 1.91, 1.89, 1.87,
     &      1.85, 1.84, 1.82, 1.81, 1.79, 1.69, 1.59, 1.50, 1.39 /
C
      DATA (T(17,I),I=1,34)/252.2,19.48,8.57,5.69,4.43,3.74,3.30,
     &      3.01, 2.79, 2.62, 2.49, 2.38, 2.30, 2.22, 2.16, 2.11,
     &      2.06, 2.02, 1.98, 1.95, 1.92, 1.89, 1.86, 1.84, 1.82,
     &      1.80, 1.79, 1.77, 1.75, 1.74, 1.64, 1.53, 1.43, 1.32/
C
      DATA (T(18,I),I=1,34)/253.3,19.49,8.55,5.66,4.40,3.70,3.27,
     &      2.97, 2.75, 2.58, 2.45, 2.34, 2.25, 2.18, 2.11, 2.06,
     &      2.01, 1.97, 1.93, 1.90, 1.87, 1.84, 1.81, 1.79, 1.77,
     &      1.75, 1.73, 1.71, 1.70, 1.68, 1.58, 1.47, 1.35, 1.22/
C
      DATA (T(19,I),I=1,34)/254.3,19.50,8.53,5.63,4.36,3.67,3.23,
     &      2.93, 2.71, 2.54, 2.40, 2.30, 2.21, 2.13, 2.07, 2.01,
     &      1.96, 1.92, 1.88, 1.84, 1.81, 1.78, 1.76, 1.73, 1.71,
     &      1.69, 1.67, 1.65, 1.64, 1.62, 1.51, 1.39, 1.25, 1.00/
C
C     ------------------------------------------------------------------
C
      IF (NP.LE.10.AND.IDOF.LE.30) THEN
C       ENTRIES ARE EXACT FOR FIRST AND SECOND SUBSCRIPTS
        TST=T(NP,IDOF)
        RETURN
      ENDIF
C
      IF (NP.GT.10.AND.IDOF.LE.30) THEN
C       INTERPOLATE BETWEEN ENTRIES FOR FIRST SUBSCRIPT;
C       ENTRIES FOR SECOND SUBSCRIPT ARE EXACT
        DO 10 I=11,19
          IF(NP.LE.ITABLE1(I)) THEN
            TST = ((T(I,IDOF)-T(I-1,IDOF))*REAL(NP-ITABLE1(I-1))
     &            /REAL(ITABLE1(I)-ITABLE1(I-1)))+T(I-1,IDOF)
            RETURN
          ENDIF
   10   CONTINUE
        TST = T(19,IDOF)
        RETURN
      ENDIF
C
      IF (NP.LE.10.AND.IDOF.GT.30) THEN
C       INTERPOLATE BETWEEN ENTRIES FOR SECOND SUBSCRIPT;
C       ENTRIES FOR FIRST SUBSCRIPT ARE EXACT
        DO 20 J=31,34
          IF(IDOF.LE.ITABLE2(J)) THEN
            TST = ((T(NP,J)-T(NP,J-1))*REAL(IDOF-ITABLE2(J-1))
     &            /REAL(ITABLE2(J)-ITABLE2(J-1)))+T(NP,J-1)
            RETURN
          ENDIF
   20   CONTINUE
        TST = T(NP,34)
        RETURN
      ENDIF
C
      IF (NP.LE.ITABLE1(19)) THEN
        IF (IDOF.LE.ITABLE2(34)) THEN
C         INTERPOLATE BETWEEN ENTRIES FOR FIRST AND SECOND SUBSCRIPTS
          DO 40 I=11,19
            IF(NP.LE.ITABLE1(I)) THEN
              DO 30 J=31,34
                IF(IDOF.LE.ITABLE2(J)) THEN
                  TST1 = ((T(I,J)-T(I-1,J))*REAL(NP-ITABLE1(I-1))
     &                   /REAL(ITABLE1(I)-ITABLE1(I-1)))+T(I-1,J)
                  TST2 = ((T(I,J-1)-T(I-1,J-1))*REAL(NP-ITABLE1(I-1))
     &                   /REAL(ITABLE1(I)-ITABLE1(I-1)))+T(I-1,J-1)
                  TST = ((TST1-TST2)*REAL(IDOF-ITABLE2(J-1))
     &                   /REAL(ITABLE2(J)-ITABLE2(J-1)))+TST2
                  RETURN
                ENDIF
   30         CONTINUE
            ENDIF
   40     CONTINUE
        ELSE
C         NP IS WITHIN RANGE OF VALUES IN ITABLE1, BUT IDOF EXCEEDS
C         LARGEST VALUE IN ITABLE2
          DO 50 I=11,19
            IF (NP.LE.ITABLE1(I)) THEN
              TST = ((T(I,34)-T(I-1,34))*REAL(NP-ITABLE1(I-1))
     &              /REAL(ITABLE1(I)-ITABLE1(I-1)))+T(I-1,34)
              RETURN
            ENDIF
   50     CONTINUE
        ENDIF
      ENDIF
C
      TST=T(19,34)
C
C     RETURN
      END

