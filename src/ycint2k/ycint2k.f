! Time of File Save by ERB: 1/5/2005 10:23AM
C     Last change:  ERB  25 Feb 2002    2:21 pm
      PROGRAM YCINT2K
C
C     CALCULATE LINEAR CONFIDENCE INTERVALS ON SIMULATED HYDRAULIC HEADS
C     AND FLOWS ALONG HEAD-DEPENDENT BOUNDARIES.
C
C     MARY C. HILL     15DEC1992
C         MODIFIED FROM AN UNPUBLISHED PROGRAM CALLED RELIAB BY
C         RICHARD L. COOLEY (WRITTEN COMMUN., 1992)
C     Modified to work with MODFLOW-2000 by E.R. Banta, 8/24/1999
C
C       $Date: 1997/07/24 15:14:34 $
C       $Revision: 3.1 $
C
C     NDD MUST EQUAL OR EXCEED THE NUMBER OF HEADS AND FLOWS FOR WHICH
C          INTERVALS ARE CALCULATED
C     NDQ MUST EQUAL OR EXCEED THE NUMBER OF FLOWS FOR WHICH INTERVALS
C          ARE CALCULATED
C     NPD MUST EQUAL OR EXCEED THE NUMBER OF PARAMETER VALUES
      CHARACTER*40 VERSION
      PARAMETER (VERSION='1.2 02/25/2002')
      REAL C, FSTAT, H, H1, HD, HL, HL1, HU, HU1, S2, S21, STATS,
     &     V, V1, WQ, X, X1
      INTEGER I, IDIF, IIN0, IIN1, IIN2, IOUT, IPR, IPRED, ISYM, ISYM1,
     &        NVAR, J, MPR, N, NDD, NDMH, NDMHD, NH, NOBS, NPD
      DIMENSION STATS(4)
      CHARACTER*12 DID, DID1
      CHARACTER*10 PARNAM
      ALLOCATABLE :: DID(:), DID1(:), PARNAM(:)
      ALLOCATABLE :: X(:,:), C(:,:), H(:), H1(:), X1(:,:), V(:), V1(:),
     &               WQ(:), ISYM(:), ISYM1(:)
      COMMON /ITP   / IIN1, IOUT
      CHARACTER*18 MESSAGE
      CHARACTER*80 VERSN, OUTNAM
      CHARACTER*84 FN
      CHARACTER*45 LABEL(6)
C
      DATA LABEL/'"INDIVIDUAL CONFIDENCE INTERVALS"',
     &           '"SIMULTANEOUS CONFIDENCE INTERVALS, LIMITED"',
     &           '"SIMULTANEOUS CONFIDENCE INTERVALS, INFINITE"',
     &           '"INDIVIDUAL PREDICTION INTERVALS"',
     &           '"SIMULTANEOUS PREDICTION INTERVALS, LIMITED"',
     &           '"SIMULTANEOUS PREDICTION INTERVALS, INFINITE"'/
      VERSN =
     &    '$Id: ycint.f,v 3.1 1997/07/24 15:14:34 rsregan Exp rsregan $'
      VERSN =
     &'@(#)YCINT - MODFLOWP UTILITY TO CALCULATE LINEAR CONFIDENCE INTER
     &VALS ON'
      VERSN =
     &'@(#)YCINT - SIMULATED HYDRAULIC HEADS AND FLOWS ALONG HEAD-DEPEND
     &ENT BOUNDARIES'
      VERSN =
     &'@(#)YCINT - MODIFIED BY M.C. HILL FROM UNPUBLISHED PROGRAM RELIAB
     & BY R.L COOLEY'
      VERSN = '@(#)YCINT - CONTACT: H2OSOFT@USGS.GOV'
      VERSN = '@(#)YCINT - VERSION: 3.1x 1997/07/24'
C
C-----FORMAT LIST
  400 FORMAT(' ERROR OPENING FILE "',A,'" -- STOP EXECUTION')
  480 FORMAT (35X,'YCINT-2000',//,
     &15X,'MODFLOW-2000 POST-PROCESSING PROGRAM TO CALCULATE',/,
     &18X,'LINEAR CONFIDENCE AND PREDICTION INTERVALS',/,
     &29X,'Version ',A/)
  490 FORMAT(
     &' ERROR FINDING INPUT FILES.  OUTNAM MUST BE SPECIFIED AS A',/,
     &' STRING OTHER THAN "NONE" IN THE OBS FILE.  THE FILE',/,
     &' YCINT.ERR MAY CONTAIN ADDITIONAL DIAGNOSTIC INFORMATION')
  500 FORMAT (4I10)
  505 FORMAT (8F15.0)
  510 FORMAT (' NUMBER OF ESTIMATED PARAMETERS........... =',I5,/,
     &        ' NUMBER OF INTERVALS...................... =',I5,/,
     &        ' NUMBER OF PRIOR INFORMATION.............. =',I5,/,
     &        ' DEGREES OF FREEDOM....................... =',I5,/,
     &        ' READ CRITICAL VALUES (IF > 0) (IFSTAT)... =',I5)
  515 FORMAT (/,' INTERVALS ARE CALCULATED ON DIFFERENCES')
  520 FORMAT (/,3X,'VALUES COMPUTED WITH OPTIMUM PARAME',
     &'TERS FOR PREDICTIVE CONDITIONS',/,
     &4X,2(4X,'OBSERVATION',17X),/,
     &4X,2('NO.    NAME       VALUE',9X))
  525 FORMAT (/,3X,'VALUES COMPUTED WITH OPTIMUM PARAME',
     &'TERS FOR BASE CONDITIONS',/,
     &4X,2(4X,'OBSERVATION',17X),/,
     &4X,2('NO.    NAME       VALUE',9X))
  530 FORMAT (/,' INTERVALS ARE NOT CALCULATED ON DIFFERENCES')
  535 FORMAT (/,
     &' SENSITIVITIES FOR OPTIMUM PARAMETERS FOR PREDICTIVE CONDITIONS')
  540 FORMAT (/,
     &      ' SENSITIVITIES FOR OPTIMUM PARAMETERS FOR BASE CONDITIONS')
  545 FORMAT (16F13.0)
  550 FORMAT (6F13.0)
  555 FORMAT (/,' NDD, NDMHD AND NPD, DEFINED IN THE PARAMETER,',
     &        ' STATEMENT ARE NOT LARGE ENOUGH.',/,'    SET NDD >=',I5,
     &        ', NDQ >=',I5,' AND NPD >=',I5,' -- STOP EXECUTION')
  560 FORMAT (/,' VARIANCE-COVARIANCE MATRIX FOR ESTIMATED PARAMETERS')
  565 FORMAT (/,4X,'CRITICAL VALUE FOR THE INTERVALS',A,'= ',G11.5,///,
     &9X,'OBSERVATION  SIMULATED',/,
     &        '  NO.       ',
     &'NAME',6X,'DIFFERENCE     STD. DEV.      CONFIDENCE INTERVAL')
  570 FORMAT (/,4X,'CRITICAL VALUE FOR THE INTERVALS',A,'= ',G11.5,///,
     &9X,'OBSERVATION  SIMULATED',/,
     &'  NO.',7X,'NAME',8X,'VALUE',7X,'STD. DEV.',7X,
     &'CONFIDENCE INTERVAL')
  575 FORMAT (1X,I5,3X,A,G13.6,2(1X,G13.6),';',G13.6,A,2(G13.6,1X),
     &        G13.6,';',G13.6)
  580 FORMAT (20A4)
  585 FORMAT (/,' PREDICTION INTERVALS ARE CALCULATED')
  590 FORMAT (/,' CONFIDENCE INTERVALS ARE CALCULATED')
  595 FORMAT (/,4X,'CRITICAL VALUE FOR THE INTERVALS',A,'= ',G11.5,///,
     &9X,'OBSERVATION  SIMULATED',/,
     &'  NO.       ',
     &'NAME',6X,'DIFFERENCE    STD. DEV.       PREDICTION INTERVAL')
  600 FORMAT (/,4X,'CRITICAL VALUE FOR THE INTERVALS',A,'= ',G11.5,///,
     &9X,'OBSERVATION  SIMULATED'/,
     &'  NO.       NAME',8X,'VALUE',7X,'STD. DEV.',7X,
     &'PREDICTION INTERVAL')
  610 FORMAT (6(A12,1X))
  615 FORMAT (16I5)
  620 FORMAT (I10)
  630 FORMAT (/,1X,79('*'),/,1X,79('*'))
  640 FORMAT (/,1X,'USER-SPECIFIED CRITICAL VALUES:',/,
     &3X,'FOR INDIVIDUAL INTERVALS (STATIND)...............',
     &'............. = ',G12.5,/,
     &3X,'FOR FINITE NUMBER OF SIMULTANEOUS INTERVALS (STAT',
     &'SF).......... = ',G12.5,/,
     &3X,'FOR UNDEFINED NUMBER OF SIMULTANEOUS INTERVALS (F',
     &'STATSI)...... = ',G12.5,/,
     &3X,'FOR SIMULTANEOUS PREDICTION INTERVALS WHEN K > NP',
     &' (FSTATKGTNP) = ',G12.5)
  650 FORMAT (/,3X,'FSTATKGTNP IS USED TO GENERATE ONE SET OF',
     &' SIMULTANEOUS PREDICTION INTERVALS')
  660 FORMAT (A)
  670 FORMAT (4(G12.5,1X),A12,I5)
  721 FORMAT(/,' INDIVIDUAL 95% CONFIDENCE INTERVALS'
     & ,//,4X,'UNCERTAINTY ON EACH PREDICTION IS CONSIDERED SEPARATELY'
     & ,/,4X,'IF SIMULTANEOUS UNCERTAINTY IS DESIRED, GO TO NEXT TABLE')
  722 FORMAT(/,I5,' SIMULTANEOUS 95% CONFIDENCE INTERVALS'
     & ,//,'    UNCERTAINTY ON EACH PREDICTION IS CONSIDERED JOINTLY'
     &,/,'    IF UNCERTAINTY OVER AN AREA IS DESIRED, GO TO NEXT TABLE')
  723 FORMAT(/,' UNDEFINED NUMBER OF SIMULTANEOUS 95% CONFIDENCE INTERV'
     &,'ALS',//,4X,'UNCERTAINTY IS CONSIDERED OVER AN AREA',
     &' (I.E. AN INFINITE NUMBER OF POINTS)')
  724 FORMAT(/,' INDIVIDUAL 95% PREDICTION INTERVALS'
     & ,//,4X,'UNCERTAINTY ON EACH PREDICTION IS CONSIDERED SEPARATELY'
     & ,/,4X,'IF SIMULTANEOUS UNCERTAINTY IS DESIRED, GO TO NEXT TABLE')
  725 FORMAT(/,I5,' SIMULTANEOUS 95% PREDICTION INTERVALS '
     & ,//,'    UNCERTAINTY ON EACH PREDICTION IS CONSIDERED JOINTLY'
     &,/,'    IF UNCERTAINTY OVER AN AREA IS DESIRED, GO TO NEXT TABLE')
  726 FORMAT(/,' UNDEFINED NUMBER OF SIMULTANEOUS 95% PREDICTION INTERV'
     &,'ALS',//,4X,'UNCERTAINTY IS CONSIDERED OVER AN AREA',
     &' (I.E. AN INFINITE NUMBER OF POINTS)')
  727 FORMAT(/,' INDIVIDUAL 95% CONFIDENCE INTERVALS',
     &' ON DIFFERENCES',/,'      BETWEEN PREDICTIVE AND BASE CASES'
     & ,//,4X,'UNCERTAINTY ON EACH DIFFERENCE IS CONSIDERED SEPARATELY'
     & ,/,4X,'IF SIMULTANEOUS UNCERTAINTY IS DESIRED, GO TO NEXT TABLE')
  728 FORMAT(/,I5,' SIMULTANEOUS 95% CONFIDENCE INTERVALS',
     &  ' ON DIFFERENCES',/,'      BETWEEN PREDICTIVE AND BASE CASES'
     & ,//,'    UNCERTAINTY ON EACH DIFFERENCE IS CONSIDERED JOINTLY'
     &,/,'    IF UNCERTAINTY OVER AN AREA IS DESIRED, GO TO NEXT TABLE')
  729 FORMAT(/,' UNDEFINED NUMBER OF SIMULTANEOUS 95% CONFIDENCE INTERV'
     &,'ALS',' ON DIFFERENCES',/,'      BETWEEN PREDICTIVE AND BASE CAS'
     &,'ES',//,4X,'UNCERTAINTY IS CONSIDERED OVER AN AREA',
     &' (I.E. AN INFINITE NUMBER OF POINTS)')
  730 FORMAT(/,' INDIVIDUAL 95% PREDICTION INTERVALS',
     &  ' ON DIFFERENCES',/,'      BETWEEN PREDICTIVE AND BASE CASES'
     & ,//,4X,'UNCERTAINTY ON EACH DIFFERENCE IS CONSIDERED SEPARATELY'
     & ,/,4X,'IF SIMULTANEOUS UNCERTAINTY IS DESIRED, GO TO NEXT TABLE')
  731 FORMAT(/,I5,' SIMULTANEOUS 95% PREDICTION INTERVALS',
     &  ' ON DIFFERENCES',/,'      BETWEEN PREDICTIVE AND BASE CASES'
     & ,//,'    UNCERTAINTY ON EACH DIFFERENCE IS CONSIDERED JOINTLY'
     &,/,'    IF UNCERTAINTY OVER AN AREA IS DESIRED, GO TO NEXT TABLE')
  732 FORMAT(/,' UNDEFINED NUMBER OF SIMULTANEOUS 95% PREDICTION',
     &' INTERVALS ON DIFFERENCES'/7X,'BETWEEN PREDICTIVE AND BASE CASES'
     &,//,4X,'UNCERTAINTY IS CONSIDERED OVER AN AREA',
     &' (I.E. AN INFINITE NUMBER OF POINTS)')
  733 FORMAT(/,'    PREDICTION INTERVALS INCLUDE MEASUREMENT ERROR,'
     &      ,/,'        I.E. GIVEN THE VARIANCE LISTED IN THE'
     &      ,/,'        OBSERVATION INPUT FILES USED TO DEFINE THE'
     &      ,/,'        PREDICTIONS, THERE IS A 95% PROBABILITY'
     &      ,/,'        THAT THE MEASUREMENT WILL FALL WITHIN THE'
     &      ,/,'        INDICATED RANGE')
  735 FORMAT(/,'    95% CONFIDENCE INTERVALS INDICATE THAT THERE IS'
     &       ,/,'        95% PROBABILITY THAT THE ACTUAL VALUE WILL BE '
     &       ,/,'        WITHIN THE INDICATED RANGE')
  740 FORMAT(/,' SIMULTANEOUS 95% PREDICTION INTERVALS '
     & ,//,'    UNCERTAINTY ON EACH PREDICTION IS CONSIDERED JOINTLY')
  745 FORMAT(/,' SIMULTANEOUS 95% PREDICTION INTERVALS',
     &  ' ON DIFFERENCES',/,'      BETWEEN PREDICTIVE AND BASE CASES'
     & ,//,'    UNCERTAINTY ON EACH DIFFERENCE IS CONSIDERED JOINTLY')
  823 FORMAT(/,4X,'BONFERRONI CONFIDENCE INTERVALS ARE USED')
  824 FORMAT(/,4X,'SCHEFFE CONFIDENCE INTERVALS ARE USED')
  825 FORMAT(/,4X,'BONFERRONI PREDICTION INTERVALS ARE USED')
  826 FORMAT(/,4X,'SCHEFFE PREDICTION INTERVALS ARE USED')
  830 FORMAT(/,4X,'CONFIDENCE INTERVALS ARE GENERATED WITH',
     &' USER-SPECIFIED CRITICAL VALUE')
  835 FORMAT(/,4X,'PREDICTION INTERVALS ARE GENERATED WITH',
     &' USER-SPECIFIED CRITICAL VALUE')
C
C     WRITE PROGRAM NAME AND VERSION TO SCREEN
      WRITE (*,*)
      WRITE (*,480) VERSION
C
C-----DEFINE INPUT FILE, OUTPUT FILE, AND ARRAY DIMENSION
      IIN1 = 1
      IIN2 = 2
      IIN0 = 3
      IOUT = 4
      IYOUT = 8
      OPEN (IOUT,FILE='YCINT.ERR',STATUS='UNKNOWN')
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
C     OPEN MAIN OUTPUT FILE
      FN = OUTNAM(1:LENGNAM)//'.#yc'
      OPEN (IOUT,FILE=FN,STATUS='UNKNOWN')
      WRITE (IOUT,480) VERSION
C     OPEN INPUT FILES
      FN = OUTNAM(1:LENGNAM)//'._y1'
      OPEN (IIN1,FILE=FN,STATUS='OLD',ERR=2,IOSTAT=IOS)
    2 CONTINUE
      IF (IOS.NE.0) THEN
        LENGNAM = NONB_LEN(FN,84)
        WRITE (*,400) FN(1:LENGNAM)
        STOP
      ENDIF
C     UNIT IIN2 OPENED BELOW, IF NEEDED
      FN = OUTNAM(1:LENGNAM)//'._y0'
      OPEN (IIN0,FILE=FN,STATUS='OLD',ERR=4,IOSTAT=IOS)
    4 CONTINUE
      IF (IOS.NE.0) THEN
        LENGNAM = NONB_LEN(FN,84)
        WRITE (*,400) FN(1:LENGNAM)
        STOP
      ENDIF
C
C     READ ITEMS 1-4 OF FILE ._y0
      READ (IIN0,*) NDCALIB
      READ (IIN0,*) MPR
      READ (IIN0,*) IPR
      READ (IIN0,*) IDIF
      IF (IDIF.NE.0) IDIF = 1
C
C     OPEN EITHER ._yp FILE or ._yd FILE, DEPENDING ON IDIF
      IF (IDIF.EQ.0) THEN
        FN = OUTNAM(1:LENGNAM)//'._yp'
      ELSE
        FN = OUTNAM(1:LENGNAM)//'._yd'
      ENDIF
      OPEN (IYOUT,FILE=FN,STATUS='UNKNOWN')
C     UNIT IVAR OPENED BELOW, IF NEEDED
C-----READ FIRST LINE OF FILE ._y1
      READ (IIN1,500) NVAR, NOBS, NH, IFSTAT
      IDOF = NDCALIB + IPR + MPR - NVAR
      WRITE (IOUT,510) NVAR, NOBS, MPR+IPR, IDOF, IFSTAT
      NDMH = NOBS - NH
      NDD = NOBS
      NPD = NVAR
      NDMHD = NDMH
C
C     DYNAMICALLY ALLOCATE ARRAYS
      ALLOCATE (DID(NDD), DID1(NDD), PARNAM(NPD))
      ALLOCATE (X(NPD,NDD), C(NPD,NPD), H(NDD), H1(NDD), X1(NPD,NDD),
     &          V(NDD), V1(NDD), WQ(NDMHD), ISYM(NDD), ISYM1(NDD))
C
      IF (IFSTAT.EQ.0) THEN
        MESSAGE = ' '
        LENMESS = 1
      ELSE
        MESSAGE = ' (USER-SPECIFIED) '
        LENMESS = 18
      ENDIF
      READ (IIN0,*) (PARNAM(I),I=1,NVAR)
C
C-----INITIALIZE V AND V1
      DO 10 N = 1, NOBS
        V(N) = 0.0
        V1(N) = 0.0
   10 CONTINUE
C
C-----READ OR CALCULATE STATISTICS
      IF (IFSTAT.GT.0) THEN
C-------READ USER-DEFINED STATISTICS
        READ (IIN1,*) STATIND, STATSF, FSTATSI, FSTATKGTNP
        WRITE (IOUT,640) STATIND, STATSF, FSTATSI, FSTATKGTNP
        IF (FSTATKGTNP.NE.0.0) WRITE (IOUT,650)
      ELSE
C-------CALCULATE STATISTICS
C       STATISTIC FOR INDIVIDUAL CONFIDENCE INTERVALS
        CALL TSTAT(IDOF,STATIND)
C       TEST FOR SMALLEST CRITICAL VALUE FOR SIMULTANEOUS INTERVALS
        CALL BSTAT(NOBS,IDOF,CRIT1,IOUT)
        ID = NVAR
        IF (NOBS.LT.NVAR) ID=NOBS
        CALL FSTT(ID,IDOF,CRIT2)
        CRIT2 = SQRT (ID*CRIT2)
        STATSF = CRIT1
        IF (CRIT2.LT.CRIT1) STATSF = CRIT2
C       STATISTIC FOR UNDEFINED NUMBER OF INTERVALS
        CALL FSTT(NVAR,IDOF,STAT)
        FSTATSI = SQRT (NVAR * STAT)
        FSTATKGTNP = 0.0
      ENDIF
      IF (IDIF.EQ.1) WRITE (IOUT,515)
      IF (IDIF.NE.1) WRITE (IOUT,530)
      STATS(1) = STATIND
      STATS(2) = STATSF
      STATS(3) = FSTATSI
      STATS(4) = FSTATKGTNP
      IF (NVAR.GT.NPD .OR. NOBS.GT.NDD .OR. NDMH.GT.NDMHD) THEN
        WRITE (IOUT,555) NVAR, NOBS, NDMH
        STOP
      ENDIF
C
C-----READ FILE ._y0 - VARIANCE-COVARIANCE MATRIX ON THE PARAMETERS FROM
C     THE CALIBRATION.  MUST BE PRODUCED USING THE STRESSES AND
C     OBSERVATIONS FROM THE CALIBRATION.
      DO 30 I = 1, NVAR
        READ (IIN0,545) (C(I,J),J=I,NVAR)
        IF (I.LT.NVAR) THEN
          DO 20 J = I + 1, NVAR
            C(J,I) = C(I,J)
   20     CONTINUE
        ENDIF
   30 CONTINUE
      WRITE (IOUT,560)
      CALL UPARPM(C,NVAR,8,IOUT,PARNAM,NPD)
C
C-----READ FILE ._y2 - BASE CONDITIONS FOR INTERVALS ON DIFFERENCES.
C     PRODUCED WITH IYCFLG=2.
C
      IF (IDIF.EQ.1) THEN
        FN = OUTNAM(1:LENGNAM)//'._y2'
        OPEN (IIN2,FILE=FN,STATUS='OLD',ERR=40,IOSTAT=IOS)
   40   CONTINUE
        IF (IOS.NE.0) THEN
          LENGNAM = NONB_LEN(FN,84)
          WRITE (*,400) FN(1:LENGNAM)
          STOP
        ENDIF
        READ (IIN2,610) (DID1(N),N=1,NOBS)
        READ (IIN2,615) (ISYM1(N),N=1,NOBS)
        READ (IIN2,550) (H1(N),N=1,NOBS)
        WRITE (IOUT,525)
        CALL PRTOTA(DID1,H1,NOBS)
C
        IF (NH.GT.0) READ (IIN2,505) (V1(I),I=1,NH)
        IF (NDMH.GT.0) THEN
C---------READ DIAGONAL TERMS OF FULL WEIGHT MATRIX FOR DATA OTHER THAN
C         HEADS
          READ (IIN2,505) (WQ(J),J=1,NDMH)
          DO 50 I = 1, NDMH
            IF (WQ(I).GT.0.0) V1(NH+I) = 1.0/WQ(I)
   50     CONTINUE
        ENDIF
        DO 70 N = 1, NOBS
          READ (IIN2,550) (X1(I,N),I=1,NVAR)
   70   CONTINUE
        WRITE (IOUT,540)
        CALL PRTOTX(X1,NVAR,NOBS,NPD,DID1,PARNAM)
      ENDIF
C
C-----READ FILE ._y1 - PREDICTIVE CONDITIONS FOR INTERVALS ON
C     DIFFERENCES.  PRODUCED WITH IYCFLG=1.
      READ (IIN1,610) (DID(N),N=1,NOBS)
      READ (IIN1,615) (ISYM(N),N=1,NOBS)
      READ (IIN1,550) (H(N),N=1,NOBS)
      WRITE (IOUT,520)
      CALL PRTOTA(DID,H,NOBS)
      IF (NH.GT.0) READ (IIN1,505) (V(N),N=1,NH)
      IF (NDMH.GT.0) THEN
C-------READ DIAGONAL TERMS OF FULL WEIGHT MATRIX FOR FLOWS
        READ (IIN1,505) (WQ(J),J=1,NDMH)
        DO 80 I = 1, NDMH
          IF (WQ(I).GT.0.0) V(NH+I) = 1.0/WQ(I)
   80   CONTINUE
      ENDIF
      DO 100 N = 1, NOBS
        READ (IIN1,550) (X(I,N),I=1,NVAR)
  100 CONTINUE
      WRITE (IOUT,535)
      CALL PRTOTX(X,NVAR,NOBS,NPD,DID,PARNAM)
C
C GENERATE CONFIDENCE (IPRED=0) AND PREDICTION (IPRED=1) INTERVAL TABLES
      IFLG = 0
      DO 300 IPRED = 0,1
        DO 280 ITAB = 1,3
          IF (IFLG.EQ.1) GOTO 280
          INT = ITAB + 3*IPRED + 6*IDIF
          ILAB = ITAB + 3*IPRED
          FSTAT = STATS(ITAB)
          IF ((INT.EQ.5 .OR. INT.EQ.11) .AND. STATS(4).NE.0.0) THEN
            IFLG = 1
            FSTAT = STATS(4)
          ENDIF
          WRITE (IOUT,630)
C---------CALCULATE AND PRINT THE INTERVALS
C         WRITE TABLE TYPE AND DESCRIPTION OF INTERVALS
          IF (INT.EQ.1) WRITE(IOUT,721)
          IF (INT.EQ.2) WRITE(IOUT,722) NOBS
          IF (INT.EQ.3) WRITE(IOUT,723)
          IF (INT.EQ.4) WRITE(IOUT,724)
          IF (INT.EQ.5 .AND. IFLG.EQ.0) WRITE(IOUT,725) NOBS
          IF (INT.EQ.5 .AND. IFLG.EQ.1) WRITE(IOUT,740)
          IF (INT.EQ.6) WRITE(IOUT,726)
          IF (INT.EQ.7) WRITE(IOUT,727)
          IF (INT.EQ.8) WRITE(IOUT,728) NOBS
          IF (INT.EQ.9) WRITE(IOUT,729)
          IF (INT.EQ.10) WRITE(IOUT,730)
          IF (INT.EQ.11 .AND. IFLG.EQ.0) WRITE(IOUT,731) NOBS
          IF (INT.EQ.11 .AND. IFLG.EQ.1) WRITE(IOUT,745)
          IF (INT.EQ.12) WRITE(IOUT,732)
          IF (INT.GE.4.AND.INT.LE.6) THEN
            WRITE (IOUT,733)
          ELSEIF (INT.GE.10.AND.INT.LE.12) THEN
            WRITE (IOUT,733)
          ELSE
            WRITE (IOUT,735)
          ENDIF
          IF (INT.EQ.2.OR.INT.EQ.8) THEN
            IF (IFSTAT.EQ.0) THEN
              IF (CRIT1.GE.CRIT2) WRITE(IOUT,823)
              IF (CRIT1.LT.CRIT2) WRITE(IOUT,824)
            ENDIF
          ELSEIF (INT.EQ.5.OR.INT.EQ.11) THEN
            IF (IFSTAT.EQ.0) THEN
              IF (CRIT1.GE.CRIT2) WRITE(IOUT,825)
              IF (CRIT1.LT.CRIT2) WRITE(IOUT,826)
            ENDIF
          ENDIF
C
          IF (IPRED.EQ.0) THEN
            IF (IDIF.EQ.1) WRITE (IOUT,565) MESSAGE(1:LENMESS), FSTAT
            IF (IDIF.NE.1) WRITE (IOUT,570) MESSAGE(1:LENMESS), FSTAT
          ELSE
            IF (IDIF.EQ.1) WRITE (IOUT,595) MESSAGE(1:LENMESS), FSTAT
            IF (IDIF.NE.1) WRITE (IOUT,600) MESSAGE(1:LENMESS), FSTAT
          ENDIF
C
          WRITE (IYOUT,660) LABEL(ILAB)
          DO 130 N = 1, NOBS
            S2 = 0.0
            S21 = 0.0
            DO 120 I = 1, NVAR
              DO 110 J = 1, NVAR
                IF (IDIF.EQ.1) S21 = S21 + (X(I,N)-X1(I,N))*C(I,J)
     &                               *(X(J,N)-X1(J,N))
                S2 = S2 + X(I,N)*C(I,J)*X(J,N)
  110         CONTINUE
  120       CONTINUE
            IF (IPRED.EQ.1) S2 = S2 + V(N)
            S2 = S2**.5
            IF (IDIF.EQ.1) THEN
C              S21 = S21 + V(N) + V1(N) ! Change suggested by E. Poeter 10/4/04
              IF (IPRED.EQ.1) S21 = S21 + V(N) + V1(N)
              S21 = S21**.5
            ENDIF
            HL = H(N) - FSTAT*S2
            HU = H(N) + FSTAT*S2
            IF (IDIF.EQ.1) THEN
              HD = H(N) - H1(N)
              HL1 = HD - FSTAT*S21
              HU1 = HD + FSTAT*S21
              WRITE (IOUT,575) N, DID1(N), HD, S21, HL1, HU1
              WRITE (IYOUT,670) HL1, HU1, HD, S21, DID1(N), ISYM1(N)
            ELSE
              WRITE (IOUT,575) N, DID(N), H(N), S2, HL, HU
              WRITE (IYOUT,670) HL, HU, H(N), S2, DID(N), ISYM(N)
            ENDIF
  130     CONTINUE
  280   CONTINUE
  300 CONTINUE
C
      STOP
      END
C=======================================================================
      SUBROUTINE TSTAT(IDOF,TST)
C-----VERSION 1000 01DEC1997
C     ******************************************************************
C     DETERMINE THE VALUE OF THE T STATISTIC NEEDED TO CALCULATE LINEAR
C     INDIVIDUAL CONFIDENCE INTERVALS FOR A TWO-SIDED SIGNIFICANCE LEVEL
C     OF 0.05
C     ******************************************************************
C        SPECIFICATIONS:
      REAL TST, TABLE
      INTEGER IDOF, I, ITABLE
C     ------------------------------------------------------------------
      DIMENSION ITABLE(35), TABLE(35)
      DATA (ITABLE(I),I=1,35)/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
     &      13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
     &      28, 29, 30, 40, 60, 120, 240, 500/
      DATA (TABLE(I),I=1,35)/12.706, 4.303, 3.182, 2.776, 2.571, 2.447,
     &      2.365, 2.306, 2.262, 2.228, 2.201, 2.179, 2.160, 2.145,
     &      2.131, 2.120, 2.110, 2.101, 2.093, 2.086, 2.080, 2.074,
     &      2.069, 2.064, 2.060, 2.056, 2.052, 2.048, 2.045, 2.042,
     &      2.021, 2.000, 1.980, 1.970, 1.960/
C     ------------------------------------------------------------------
C
      IF (IDOF.LE.30) THEN
        TST=TABLE(IDOF)
        RETURN
      ENDIF
      DO 10 I=31,35
        IF(IDOF.LE.ITABLE(I)) THEN
          TST = TABLE(I-1)+(TABLE(I)-TABLE(I-1))*
     &         REAL(IDOF-ITABLE(I-1))/REAL(ITABLE(I)-ITABLE(I-1))
          RETURN
        ENDIF
   10 CONTINUE
      TST=TABLE(35)
      RETURN
      END
C=======================================================================
      SUBROUTINE BSTAT(K,IDOF,TST,IOUT)
C-----VERSION 1000 01DEC1997
C     ******************************************************************
C     DETERMINE THE VALUE OF THE F STATISTIC NEEDED TO CALCULATE
C     BEALE'S MEASURE OF LINEARITY
C      -- SUPERFICIALLY MODIFIED FROM UCODE VERSION - ERB, 8/24/1999
C     ******************************************************************
C        SPECIFICATIONS:
      REAL TST, T
      INTEGER IDOF, I, J, ITABLE1, ITABLE2, K
C     ------------------------------------------------------------------
      DIMENSION ITABLE1(12), ITABLE2(17), T(17,12)
C     IDOF IS INDICATOR FOR TABLE1
      DATA (ITABLE1(I),I=1,12)/5, 7, 10, 12,
     &      15, 20, 24, 30, 40, 60, 120, 32000/
C     K IS INDICATOR FOR TABLE2
      DATA (ITABLE2(I),I=1,17)/ 2, 3, 4, 5, 6, 7, 8, 9, 10,
     &      15, 20, 25, 30, 35, 40, 45, 50/
C     TABLE IS SET UP AS (K,IDOF)
C
      DATA (T(1,I),I=1,12)
     &                    /3.17, 2.84, 2.64, 2.56, 2.49, 2.42,
     &                     2.39, 2.36, 2.33, 2.30, 2.27, 2.24/
C
      DATA (T(2,I),I=1,12)
     &                    /3.54, 3.13, 2.87, 2.78, 2.69, 2.61,
     &                     2.58, 2.54, 2.50, 2.47, 2.43, 2.39/
C
      DATA (T(3,I),I=1,12)
     &                    /3.81, 3.34, 3.04, 2.94, 2.84, 2.75,
     &                     2.70, 2.66, 2.62, 2.58, 2.54, 2.50/
C
      DATA (T(4,I),I=1,12)
     &                    /4.04, 3.50, 3.17, 3.06, 2.95, 2.85,
     &                     2.80, 2.75, 2.71, 2.66, 2.62, 2.58/
C
      DATA (T(5,I),I=1,12)
     &                    /4.22, 3.64, 3.28, 3.15, 3.04, 2.93,
     &                     2.88, 2.83, 2.78, 2.73, 2.68, 2.64/
C
      DATA (T(6,I),I=1,12)
     &                    /4.38, 3.76, 3.37, 3.24, 3.11, 3.00,
     &                     2.94, 2.89, 2.84, 2.79, 2.74, 2.69/
C
      DATA (T(7,I),I=1,12)
     &                    /4.53, 3.86, 3.45, 3.31, 3.18, 3.06,
     &                     3.00, 2.94, 2.89, 2.84, 2.79, 2.74/
C
      DATA (T(8,I),I=1,12)
     &                    /4.66, 3.95, 3.52, 3.37, 3.24, 3.11,
     &                     3.05, 2.99, 2.93, 2.88, 2.83, 2.77/
C
      DATA (T(9,I),I=1,12)
     &                    /4.78, 4.03, 3.58, 3.43, 3.29, 3.16,
     &                     3.09, 3.03, 2.97, 2.92, 2.86, 2.81/
C
      DATA (T(10,I),I=1,12)
     &                    /5.25, 4.36, 3.83, 3.65, 3.48, 3.33,
     &                     3.26, 3.19, 3.12, 3.06, 2.99, 2.94/
C
      DATA (T(11,I),I=1,12)
     &                    /5.60, 4.59, 4.01, 3.80, 3.62, 3.46,
     &                     3.38, 3.30, 3.23, 3.16, 3.09, 3.02/
C
      DATA (T(12,I),I=1,12)
     &                    /5.89, 4.78, 4.15, 3.93, 3.74, 3.55,
     &                     3.47, 3.39, 3.31, 3.24, 3.16, 3.09/
C
      DATA (T(13,I),I=1,12)
     &                    /6.15, 4.95, 4.27, 4.04, 3.82, 3.63,
     &                     3.54, 3.46, 3.38, 3.30, 3.22, 3.15/
C
      DATA (T(14,I),I=1,12)
     &                    /6.36, 5.09, 4.37, 4.13, 3.90, 3.70,
     &                     3.61, 3.52, 3.43, 3.34, 3.27, 3.19/
C
      DATA (T(15,I),I=1,12)
     &                    /6.56, 5.21, 4.45, 4.20, 3.97, 3.76,
     &                     3.66, 3.57, 3.48, 3.39, 3.31, 3.23/
C
      DATA (T(16,I),I=1,12)
     &                    /6.70, 5.31, 4.53, 4.26, 4.02, 3.80,
     &                     3.70, 3.61, 3.51, 3.42, 3.34, 3.26/
C
      DATA (T(17,I),I=1,12)
     &                    /6.86, 5.40, 4.59, 4.32, 4.07, 3.85,
     &                     3.74, 3.65, 3.55, 3.46, 3.37, 3.29/
C
C     ------------------------------------------------------------------
    3 FORMAT(/,'* The number of intervals and degrees of freedom',/,
     &       '* are below the range of this table, & 2,5 is used',/)
    4   FORMAT(/,'** The number of degress of freedom is below the',/,
     &         '** range considered in the table of this code (5) **',/)
C
      IF (K.LE.2.AND.IDOF.LE.5) THEN
        TST=T(1,1)
        IF (K.LT.2.OR.IDOF.LT.5) THEN
          WRITE (IOUT,3)
        ENDIF
        RETURN
      ENDIF
C
      IF (K.LE.2.AND.IDOF.GT.5) THEN
        DO 10 I=2,12
          IF(IDOF.LE.ITABLE1(I)) THEN
            TST = ((T(1,I)-T(1,I-1))*REAL(IDOF-ITABLE1(I-1))
     &            /REAL(ITABLE1(I)-ITABLE1(I-1)))+T(1,I-1)
            RETURN
          ENDIF
   10   CONTINUE
      ENDIF
C
      IF (IDOF.LT.5) THEN
        WRITE (IOUT,4) IDOF
        RETURN
      ENDIF
C
      IF (K.LE.50) THEN
        DO 20 I=2,17
          IF(K.LE.ITABLE2(I)) THEN
            DO 30 J=2,12
              IF(IDOF.LE.ITABLE1(J)) THEN
                TST1 = ((T(I,J)-T(I-1,J))*REAL(K-ITABLE2(I-1))
     &                 /REAL(ITABLE2(I)-ITABLE2(I-1)))+T(I-1,J)
                TST2 = ((T(I,J-1)-T(I-1,J-1))*REAL(K-ITABLE2(I-1))
     &                 /REAL(ITABLE2(I)-ITABLE2(I-1)))+T(I-1,J-1)
                TST = ((TST1-TST2)*REAL(IDOF-ITABLE1(J-1))
     &                /REAL(ITABLE1(J)-ITABLE1(J-1)))+TST2
                RETURN
              ENDIF
   30       CONTINUE
          ENDIF
   20   CONTINUE
      ENDIF
C
C
      WRITE(IOUT,999)
  999 FORMAT (/,'***** K is larger than statistics table maximum ***',
     &        /,'      consequently the largest statistic is used',
     &        /,'***** check the validity of this situation      ***',/)
      TST=T(17,1)
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
C=======================================================================
      SUBROUTINE PRTOT(C,NR,NC,NRD)
C**PRINT MATRICES DIVIDED VERTICALLY INTO TEN-COLUMN BLOCKS
      REAL C
      INTEGER I, IIN1, IOUT, J, J10, K, NC, NR, NRD
      DIMENSION C(NRD,NC)
      COMMON /ITP   / IIN1, IOUT
      DO 20 K = 1, NC, 10
        J10 = K + 9
        IF (J10.GT.NC) J10 = NC
        WRITE (IOUT,500) (J,J=K,J10)
        WRITE (IOUT,510)
        DO 10 I = 1, NR
          WRITE (IOUT,505) I, (C(I,J),J=K,J10)
   10   CONTINUE
   20 CONTINUE
  500 FORMAT ('0',10(9X,I3))
  505 FORMAT (I4,1X,10(1X,G11.5))
  510 FORMAT (1X)
      RETURN
      END
C=======================================================================
      SUBROUTINE PRTOTA(DID,VALB,NO)
C**PRINT VALUES IN TWO GROUPS OF THREE COLUMNS
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
      SUBROUTINE PRTOTB(VAL,NO,DID)
C**PRINT VALUES IN THREE GROUPS OF THREE COLUMNS
      INTEGER IIN1, IOUT, K, L, NO, NR
      REAL VAL
      CHARACTER*12 DID(NO)
      DIMENSION VAL(NO)
      COMMON /ITP   / IIN1, IOUT
      NR = NO/3
      IF (3*NR.NE.NO) NR = NR + 1
      DO 10 K = 1, NR
        WRITE (IOUT,500) (L,DID(L),VAL(L),L=K,NO,NR)
   10 CONTINUE
      RETURN
  500 FORMAT (3X,3(I3,2X,A,1X,G11.5,3X))
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

