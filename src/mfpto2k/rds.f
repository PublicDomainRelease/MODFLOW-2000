C     Last change:  ERB   7 May 2001   11:56 am
C ======================================================================
      SUBROUTINE RNAMFIL(IUNAM,IUBAS,IUBCF,IUPAR,IOUT,VERBOSE,IERR)
C     ******************************************************************
C     READ THE NAME FILE AND OPEN ANY FILES THAT MIGHT BE NEEDED
C     ******************************************************************
      CHARACTER*4 FTYPE
      CHARACTER*7 CSTAT
      CHARACTER*11 FMTARG
      CHARACTER*200 LINE
      LOGICAL VERBOSE
C
  500 FORMAT(' ERROR: BAS FILE HAS NOT BEEN OPENED')
  510 FORMAT(' ERROR: PAR FILE HAS NOT BEEN OPENED')
  520 FORMAT(' ERROR: FILE "',A,'" CANNOT BE OPENED')
  530 FORMAT(//,
     &' ********************** WARNING **************************',/,
     &' A file of type "',A,'" is listed in the NAME file with',/,
     &' a unit number of 5.  Some Fortran compilers use unit 5',/,
     &' to designate console input.   Another unit number may',/,
     &' need to be used to avoid having the program bomb.',/,
     &' *********************************************************',//)

C
C-----READ A LINE; IGNORE BLANK LINES AND PRINT COMMENT LINES.
      IUBAS = 0
      IUPAR = 0
   10 CONTINUE
      READ(IUNAM,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GO TO 10
      IF(LINE(1:1).EQ.'#') THEN
        WRITE(IOUT,'(A)') LINE
        GO TO 10
      END IF
C
C-----DECODE THE FILE TYPE AND UNIT NUMBER.
      FTYPE = '    '
      LLOC=1
      CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,IOUT,INUNIT)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INUNIT)
      IF (IU.EQ.5) THEN
        WRITE(*,530) LINE(ITYP1:ITYP2)
        WRITE(IOUT,530) LINE(ITYP1:ITYP2)
      ENDIF
C
C-----CHECK FOR "BAS" FILE TYPE.
      IF (LINE(ITYP1:ITYP2).EQ.'BAS') THEN
        IUBAS = IU
        FTYPE = 'BAS'
        FMTARG = 'FORMATTED'
        CSTAT = 'OLD'
C-----CHECK FOR "BCF" FILE TYPE.
      ELSE IF (LINE(ITYP1:ITYP2).EQ.'BCF') THEN
        IUBCF = IU
        FTYPE = 'BCF'
        FMTARG = 'FORMATTED'
        CSTAT = 'OLD'
C-----CHECK FOR "UNFORMATTED" FILE TYPE.
      ELSE IF (LINE(ITYP1:ITYP2).EQ.'DATA(BINARY)') THEN
        FTYPE = 'DATA'
        FMTARG = 'UNFORMATTED'
        CSTAT = 'UNKNOWN'
C-----CHECK FOR "FORMATTED" FILE TYPE.
      ELSE IF(LINE(ITYP1:ITYP2).EQ.'DATA') THEN
        FTYPE = 'DATA'
        FMTARG = 'FORMATTED'
        CSTAT = 'UNKNOWN'
C-----CHECK FOR "PAR" FILE TYPE.
      ELSE IF (LINE(ITYP1:ITYP2).EQ.'PAR') THEN
        IUPAR = IU
        FTYPE = 'PAR'
        FMTARG = 'FORMATTED'
        CSTAT = 'OLD'
      ENDIF
C
C     IF THE FILE IS ONE CVDS67 NEEDS,
C     DETERMINE FILE NAME AND THE ACCESS METHOD (DIRECT OR
C     SEQUENTIAL).
      IF (FTYPE.EQ.'DATA' .OR. FTYPE.EQ.'BAS' .OR. FTYPE.EQ.'PAR' .OR.
     &    FTYPE.EQ.'BCF') THEN
        CALL URWORD(LINE,LLOC,INAM1,INAM2,0,N,R,IOUTG,INUNIT)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INUNIT)
        IF(LINE(ISTART:ISTOP).EQ.'DIRECT') THEN
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRECL,R,IOUT,INUNIT)
          WRITE(IOUT,40) LINE(INAM1:INAM2),
     &                   LINE(ITYP1:ITYP2),IU,IRECL
   40     FORMAT(1X,/1X,'OPENING ',A,/
     &           1X,'FILE TYPE:',A,'   UNIT',I4,'   DIRECT ACCESS',I10)
          CLOSE (UNIT=IU)
          OPEN(UNIT=IU,FILE=LINE(INAM1:INAM2),FORM=FMTARG,
     &         ACCESS='DIRECT',STATUS=CSTAT,RECL=IRECL,IOSTAT=ISTAT,
     &         ERR=45)
   45     CONTINUE
          IF (ISTAT.NE.0) THEN
             WRITE(*,520) LINE(INAM1:INAM2)
             STOP
          ENDIF
        ELSE
          WRITE(IOUT,50) LINE(INAM1:INAM2),LINE(ITYP1:ITYP2),IU
   50     FORMAT(1X,/1X,'OPENING ',A,/1X,'FILE TYPE:',A,'   UNIT',I4)
          CLOSE (UNIT=IU)
          OPEN(UNIT=IU,FILE=LINE(INAM1:INAM2),FORM=FMTARG,
     &         ACCESS='SEQUENTIAL',STATUS=CSTAT,IOSTAT=ISTAT,ERR=55)
   55     CONTINUE
          IF (ISTAT.NE.0) THEN
             WRITE(*,520) LINE(INAM1:INAM2)
             STOP
          ENDIF
        ENDIF
      ENDIF
C
      GOTO 10
 1000 CONTINUE
      IF (IUBAS.EQ.0) THEN
        WRITE(IOUT,500)
        IERR = 1
      ENDIF
      IF (IUPAR.EQ.0) THEN
        WRITE(IOUT,510)
        IERR = 1
      ENDIF
C
      RETURN
      END
C ======================================================================
      SUBROUTINE RBAS(IUBAS,IOUT,HEADNG,NLAY,NROW,NCOL,NPER,VERBOSE,
     &                IERR,NSTP,PERLENG,IBOUND,IFREFM,HOLD,TS,MAXSTEP,
     &                JJ,MAXPER,ISS,MAXCEL)
C     ******************************************************************
C     READ BAS FILE
C     ******************************************************************
      INTEGER NSTP(MAXPER), IBOUND(MAXCEL,MAXCEL)
      REAL PERLENG(MAXPER), HOLD(MAXCEL,MAXCEL), TS(MAXSTEP)
      CHARACTER*80 HEADNG(2)
      CHARACTER*200 LINE
      LOGICAL VERBOSE
C
  500 FORMAT(A)
  510 FORMAT(4I10)
  520 FORMAT(/,' IN RBAS, NLAY NROW NCOL NPER = ',/,9X,4I5)
  530 FORMAT(F10.0)
  540 FORMAT (F10.0,I10,F10.0)
  550 FORMAT(/,' TIME-DISCRETIZATION DATA READ FROM BAS FILE',/)
  555 FORMAT(' (SIMULATION IS STEADY-STATE, SO MODFLOWP IGNORES TIME-',
     &       'DISCRETIZATION DATA)',/)
  560 FORMAT('   STRESS PERIOD ',I5,'  HAS ',I4,' TIME STEP(S)')
  570 FORMAT('     TIME STEP ',I5,'  LENGTH IS ',G12.5)
 1000 FORMAT(' ERROR IN READING ITEM 3 OF THE BAS PACKAGE INPUT FILE')
C
      READ(IUBAS,500) (HEADNG(I),I=1,2)
      WRITE(IOUT,500) (HEADNG(I),I=1,2)
      READ(IUBAS,510,ERR=200) NLAY,NROW,NCOL,NPER
      WRITE(IOUT,520) NLAY,NROW,NCOL,NPER
C
C-----READ OPTIONS
      IXSEC = 0
      ICHFLG = 0
      IFREFM = 0
      READ(IUBAS,'(A)') LINE
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IUBAS)
      IF(LINE(ISTART:ISTOP).EQ.'XSECTION') THEN
         IXSEC=1
      ELSE IF(LINE(ISTART:ISTOP).EQ.'CHTOCH') THEN
         ICHFLG=1
      ELSE IF(LINE(ISTART:ISTOP).EQ.'FREE') THEN
         IFREFM=1
         WRITE(IOUT,20)
   20    FORMAT (1X,'THE FREE FORMAT OPTION HAS BEEN SELECTED')
      END IF
C
C     READ ITEM 4
      IF(IFREFM.EQ.0) THEN
         READ(IUBAS,'(2I10)') IAPART,ISTRT
      ELSE
         READ(IUBAS,*) IAPART,ISTRT
      END IF
C
C     READ ITEMS 6 (IBOUND ARRAYS)
      IF(IXSEC.EQ.0) THEN
         DO 40 K=1,NLAY
         KK=K
         CALL U2DINT(IBOUND(1,1),'IBOUND                  ',NROW,
     &               NCOL,KK,IUBAS,IOUT)
   40    CONTINUE
      ELSE
         CALL U2DINT(IBOUND(1,1),'IBOUND                  ',NLAY,
     &               NCOL,-1,IUBAS,IOUT)
      END IF
C
C     READ ITEM 7 (HNOFLO)
      IF(IFREFM.EQ.0) THEN
         READ(IUBAS,'(F10.0)') TMP
      ELSE
         READ(IUBAS,*) TMP
      END IF
C
C     READ ITEM 8 (SHEAD)
      IF(IXSEC.EQ.0) THEN
         DO 60 K=1,NLAY
         KK=K
         CALL U2DREL(HOLD(1,1),'STARTING HEADS          ',NROW,
     &               NCOL,KK,IUBAS,IOUT)
   60    CONTINUE
      ELSE
         CALL U2DREL(HOLD(1,1),'STARTING HEADS          ',NLAY,
     &               NCOL,-1,IUBAS,IOUT)
      END IF
C
C     READ ITEM 9 (STRESS-PERIOD DATA)
C
C     NSTP IS THE NUMBER OF TIME STEPS IN EACH STRESS PERIOD
C     PERLENG IS THE LENGTH OF EACH STRESS PERIOD
C     TS IS THE LENGTH OF EACH TIME STEP
C     K IS THE STRESS-PERIOD NUMBER
C     JJ IS THE TIME-STEP NUMBER
C
C     TIME STEP 0 PRECEDES ALL STRESS PERIODS AND TIME STEPS DEFINED
C     IN THE BAS FILE.  TIME STEP 0 CAN BE ASSUMED TO HAVE PERLEN = 0.0
C
      JJ = 0
      DO 120 K = 1, NPER
        READ (IUBAS,540,END=130) PERLEN, NSTP1, TSMULT
        IF (VERBOSE) WRITE(*,580) K,PERLEN,NSTP1,TSMULT
  580 FORMAT(' STRESS PERIOD ',I4,':',/,1P,
     &'   PERLEN = ',G12.5,',   NSTP = ',I4,',   TSMULT = ',G12.5)
        JJ = JJ + 1
        PERLENG(K) = PERLEN
        NSTP(K) = NSTP1
        IF (TSMULT.NE.1.)
     &      TS(JJ) = PERLEN*(1.-TSMULT)/(1.-(TSMULT**NSTP1))
        IF (TSMULT.EQ.1.) TS(JJ) = PERLEN/NSTP1
        IF (NSTP1.GT.1) THEN
          DO 110 I = 2, NSTP1
            JJ = JJ + 1
            TS(JJ) = TSMULT*TS(JJ-1)
  110     CONTINUE
        ENDIF
cc        IF (JJ.GT.NPER) GOTO 130
Cerb Change logical operator 5/7/2001
        IF (JJ.GE.NPER) GOTO 130
  120 CONTINUE
C
  130 CONTINUE
      NPERBAS = K-1
C
      JTS = 0
C     WRITE TIME-DISCRETIZATION DATA TO IOUT
      WRITE(IOUT,550)
      IF (ISS.NE.0) WRITE(IOUT,555)
      DO 160 I = 1,NPERBAS
        WRITE(IOUT,560) I,NSTP(I)
        DO 150 J = 1,NSTP(I)
          JTS = JTS + 1
          WRITE(IOUT,570) J,TS(JTS)
  150   CONTINUE
  160 CONTINUE
C
      IF (VERBOSE) WRITE(*,*) ' BAS INPUT FILE READ SUCCESSFULLY'
      RETURN
  200 CONTINUE
      WRITE(*,1000)
      IERR=1
      RETURN
      END
C
C ======================================================================
      SUBROUTINE RBCF(IUBCF,IOUT,ISS,VERBOSE,IERR)
C     ******************************************************************
C     READ LINE 1 OF BCF FILE
C     ******************************************************************
      LOGICAL VERBOSE
C
  500 FORMAT(I10)
  510 FORMAT(/,' SIMULATION IS TRANSIENT',/)
  520 FORMAT(/,' SIMULATION IS STEADY-STATE',/)
C
      READ(IUBCF,500) ISS
      IF (ISS.EQ.0) THEN
        WRITE(IOUT,510)
      ELSE
        WRITE(IOUT,520)
      ENDIF
C
      RETURN
      END

C ======================================================================
      SUBROUTINE RLINES(IUPAR,TITLE,NP,NSM,NSN,N2,LZ1,NMM,NMP,NZM,NH,
     &                  MOBS,MAXM,NQ,NQC,NQT,NRWD,NZER,NPNG,NLOG,
     &                  VERBOSE,IERR,IOWTQ)
C     ******************************************************************
C     READ LINES 1-8 OF PAR FILE
C     ******************************************************************
      CHARACTER*80 TITLE(2)
      LOGICAL VERBOSE
C
  505 FORMAT(A)
  510 FORMAT(16I5)
C
      READ(IUPAR,505) TITLE(1)
      READ(IUPAR,505) TITLE(2)
      READ(IUPAR,510) NP,NSM,NSN,N2,LZ1,NMM,NMP,NZM
      READ(IUPAR,510) IPRNP,ICHECK,NUMR
      READ(IUPAR,510) NH,MOBS,MAXM,NQ,NQC,NQT,MPR,IPR
      READ(IUPAR,510) IPAR,ISN,ISCALS,NOPT,ISENS
      READ(IUPAR,510) IUHEAD,IOUB,IUBD,IUNHEAD,IOUE,IOUYR,IUNORM,IOUPRI,
     &                IOUHDS,IOUFLW,IOSTAR,IOWTQ,IOUADV
      READ(IUPAR,510) NRWD,NZER,NPNG,NLOG
      IF (VERBOSE) WRITE(*,*) 'LINES 1-8 OF PAR FILE READ'
C
      RETURN
      END
C
C ======================================================================
      SUBROUTINE RDS1(IUPAR,NRWD,NZER,NPNG,NLOG,VERBOSE,IERR)
C     ******************************************************************
C     READ DATA SETS 1A, 1B, 1C, 1D
C     ******************************************************************
      LOGICAL VERBOSE
C
  510 FORMAT(16I5)
C
      IF (NRWD.GT.0) READ(IUPAR,510) (IDUM,I=1,NRWD)
      IF (NZER.GT.0) READ(IUPAR,510) (IDUM,I=1,NZER)
      IF (NPNG.GT.0) READ(IUPAR,510) (IDUM,I=1,NPNG)
      IF (NLOG.GT.0) READ(IUPAR,510) (IDUM,I=1,NLOG)
      IF ((NRWD.GT.0 .OR. NZER.GT.0 .OR. NPNG.GT.0 .OR. NLOG.GT.0) .AND.
     &     VERBOSE) WRITE (*,*)' DATA SET(S) 1 READ'
      RETURN
      END
C
C ======================================================================
      SUBROUTINE RDS2(IUPAR,NLL,LZ,N2,LZ1,NP,VERBOSE,IERR)
C     ******************************************************************
C     READ DATA SETS 2
C     ******************************************************************
      INTEGER NLL(N2), LZ(LZ1)
      CHARACTER*4 PID
      LOGICAL VERBOSE
C
  100 FORMAT(' READING PARAMETER ',I2,'   PID = ',A)
  510 FORMAT(16I5)
  520 FORMAT(A4,I1,11I5)
  530 FORMAT(F10.0)
  540 FORMAT(' ERROR: PID "',A,'" IS NOT SUPPORTED')
C
      DO 50 NDS = 1,NP
        IF (N2.LE.11) THEN
          READ(IUPAR,520) PID,LN,(NLL(I),I=1,N2)
        ELSE
          READ(IUPAR,520) PID,LN,(NLL(I),I=1,11)
          READ(IUPAR,510) (NLL(I),I=12,N2)
        ENDIF
        IF (VERBOSE) WRITE(*,100) NDS,PID
C       READ DATA SETS 2A OR 2B, DEPENDING ON PID
        IF (PID.EQ.'ANI ' .OR. PID.EQ.'ANIV') THEN
          CONTINUE
        ELSEIF (PID.EQ.'Q   ' .OR. PID.EQ.'CH  ' .OR.
     &          PID.EQ.'KRB ' .OR. PID.EQ.'KDR ' .OR.
     &          PID.EQ.'GHB ' .OR. PID.EQ.'KST ') THEN
          READ(IUPAR,530) RDUM
          NCEL = ABS(NLL(1))
          DO 30 I=1,NCEL
            READ(IUPAR,530) RDUM
   30     CONTINUE
        ELSEIF (PID.EQ.'RCH ' .OR. PID.EQ.'ETM ') THEN
          IFLAG = NLL(1)
          IF (IFLAG.GT.0) THEN
            IF (LZ1.LE.14) THEN
              READ(IUPAR,530) RDUM
            ELSE
              READ(IUPAR,530) RDUM,IDUM,IDUM,(LZ(I),I=1,14)
              READ(IUPAR,510) (LZ(I),I=15,LZ1)
            ENDIF
          ENDIF
        ELSEIF (PID.EQ.'T   ' .OR. PID.EQ.'KV  ' .OR.
     &          PID.EQ.'S1  ' .OR. PID.EQ.'S2  ') THEN
          DO 40 I = 1,N2
            IF (NLL(I).GT.0) THEN
              READ(IUPAR,530) RDUM
            ENDIF
   40     CONTINUE
        ELSE
          WRITE(*,540) PID
          IERR = 1
          RETURN
        ENDIF
   50 CONTINUE
C
      IF (VERBOSE) WRITE(*,*) ' DATA SET(S) 2 READ'
      RETURN
      END
C
C ======================================================================
      SUBROUTINE RDS3(IUPAR,IOUT,NMM,NMP,NCOL,NROW,MULT,VERBOSE,IERR)
C     ******************************************************************
C     READ DATA SETS 3
C     ******************************************************************
      REAL MULT(NCOL,NROW)
      LOGICAL VERBOSE
C
  500 FORMAT(' READING MULTIPLIER ARRAY NUMBER ',I2)
  510 FORMAT(I5)
  520 FORMAT(/,' EXECUTING SUBROUTINE RDS3...',/)
C
      WRITE(IOUT,520)
C
      IF (NMM.LE.0) RETURN
C
      DO 10 M = 1,NMM
        WRITE(IOUT,500) M
        IF (VERBOSE) WRITE(*,500) M
        CALL U2DREL(MULT,'A MULTIPLIER ARRAY      ',NROW,NCOL,1,
     &              IUPAR,IOUT)
   10 CONTINUE
C
      IF (NMP.GT.0) THEN
        READ(IUPAR,510) IDUM
      ENDIF
C
      IF (VERBOSE) WRITE(*,*) ' MULTIPLIER ARRAY(S) READ'
      RETURN
      END
C
C ======================================================================
      SUBROUTINE RDS4(IUPAR,IOUT,NZM,NCOL,NROW,IZON,VERBOSE,IERR)
C     ******************************************************************
C     READ DATA SETS 4
C     ******************************************************************
      INTEGER IZON(NCOL,NROW)
      LOGICAL VERBOSE
C
  500 FORMAT(' READING ZONE ARRAY NUMBER ',I2)
  510 FORMAT(I5)
  520 FORMAT(/,' EXECUTING SUBROUTINE RDS4...',/)
  530 FORMAT(' IN RDS4, NCOL NROW NZM = ',/
     &9X,2I5,I4,/)
C
      WRITE(IOUT,520)
      IF (VERBOSE) WRITE(IOUT,530) NCOL,NROW,NZM
C
      IF (NZM.LE.0) RETURN
C
      DO 10 M = 1,NZM
        WRITE(IOUT,500) M
        IF (VERBOSE) WRITE(*,500) M
        CALL U2DINT(IZON,'A ZONE ARRAY            ',NROW,NCOL,1,
     &              IUPAR,IOUT)
   10 CONTINUE
C
      IF (VERBOSE) WRITE(*,*) ' ZONE ARRAY(S) READ'
      RETURN
      END
C
C ======================================================================
      SUBROUTINE RDS5(IUPAR,IUH,IUF,IOUT,VERBOSE,IERR,EVH,EVF)
C     ******************************************************************
C     READ DATA SET 5
C     ******************************************************************
      LOGICAL VERBOSE
C
  500 FORMAT(2F10.0,2I5,F10.0)
  510 FORMAT(' IN RDS5, IUH IUF = ',2I5)
  520 FORMAT(/,' EXECUTING SUBROUTINE RDS5...',/)
C
      WRITE(IOUT,520)
      READ(IUPAR,500) EVH,EVF,IUH,IUF,EV
C
C  Added next 2 statements 5/2/2001 to account for EV, which previously
C  had been ignored - Re: conv. with Dick Yager
      EVH = EVH / EV
      EVF = EVF / EV
C
      IF (IUH.EQ.0) THEN
        IUH = IUPAR
      ELSE
        IUH = ABS(IUH)
      ENDIF
C
      IF (IUF.EQ.0) THEN
        IUF = IUPAR
      ELSE
        IUF = ABS(IUF)
      ENDIF
C
      IF (VERBOSE) THEN
        WRITE(*,510) IUH,IUF
        WRITE(*,*) ' DATA SET 5 READ'
      ENDIF
      RETURN
      END
C
C ======================================================================
      SUBROUTINE RDS6(BASE,IUH,IOUT,VERBOSE,IERR,NH,MOBS,MAXM,MLAY,PR,
     &                NDER,NLAY,NCOL,NROW,DID,EVH,PROGNAM,MODTCONV,
     &                NOPRSP,TOMULT,NPER,NSTP,MAXSTEP,TS,MAXPER)
C     ******************************************************************
C     READ DATA SET 6 AND WRITE HOB FILE
C     ******************************************************************
      INTEGER MLAY(MAXM,0:MOBS), NDER(5,0:NH), NSTP(MAXPER)
      REAL PR(MAXM,0:MOBS), TS(MAXSTEP)
      CHARACTER*4 DID(NH)
      CHARACTER*30 PROGNAM
      CHARACTER*200 FNAME, BASE
      LOGICAL VERBOSE
C
  500 FORMAT (A4,1X,4I5,3F8.0,F10.0,F8.0,I5)
  505 FORMAT (8(I5,F5.0))
  510 FORMAT (A4,1X,I5,F8.0,3F10.0,I5)
  515 FORMAT (2X,'TRANSIENT DATA AT THIS LOCATION, ITT =',I4)
  520 FORMAT (1X,I5,2X,A4,21X,I5,19X,F8.2,4X,2G12.5)
  530 FORMAT (//,' OBSERVED HEAD DATA',//,3X,'OBS#  ID   LAYER ROW',
     &      ' COLUMN   TIME STEP  ROW/COLUMN/TIME OFFSETS  OBSERVATION '
     &      ,' STATISTIC')
  535 FORMAT (1X,I5,2X,A4,I5,1X,2I5,5X,I5,3X,3F8.3,4X,2G12.5)
  540 FORMAT (5X,'MULTIPLE LAYERS AND PROPORTIONS :',5(I5,',',F5.2,3X))
  550 FORMAT (' FOR OBS',I5,' ROW OR COLUMN NUMBER INVALID -- ',
     &        'STOP EXECUTION (HED1RP)',/)
  555 FORMAT (' FOR OBS ',I5,
     &        ' LAYER INVALID -- STOP EXECUTION',' (RDS6)',/)
  570 FORMAT (' ')
  575 FORMAT (' FOR OBS',I5,
     &        ' ITT MUST = 1 OR 2 -- STOP EXECUTION (RDS6)',/)
  580 FORMAT(/,' Head-observation data have been written',/,
     &' to file: ',A)
  600 FORMAT('# File "',A,'"',/,
     &'# Contents are data to be read by the Head-Observation Package',/
     &'# of the Observation Process of MODFLOW-2000.',/,
     &'# File written by program ',A)
  601 FORMAT(3(1X,I9),T96,'Item 1: NH MOBS MAXM')
  602 FORMAT(1P,2(1X,G14.7),T96,'Item 2: TOMULTH EVH')
  603 FORMAT(A,4(1X,I4),1X,G14.7,2(1X,F6.3),2(1X,G14.7),2(1X,I4),T96,
     &'Item 3')
  604 FORMAT(5(1X,I4,1X,F9.7)) ! Item 4
  605 FORMAT(I5,T96,'Item 5: ITT')
  606 FORMAT(A,1X,I5,1P,4(1X,G14.6),2(1X,I4),T96,'Item 6')
C
C     OPEN HOB FILE
      IUHOB = IGETUNIT(7,1000)
      LENBASE = NONB_LEN(BASE,200)
      FNAME = BASE(1:LENBASE)//'.hob'
      LENF = LENBASE+4
      OPEN(IUHOB,FILE=FNAME)
      CLOSE(IUHOB,STATUS='DELETE')
      OPEN(IUHOB,FILE=FNAME)
C
C-------WRITE INTRODUCTORY LINES TO IOUT
      WRITE (IOUT,530)
C
C     WRITE COMMENTS AND ITEMS 1 AND 2 TO HOB FILE
      WRITE (IUHOB,600) FNAME(1:LENF), PROGNAM
      WRITE (IUHOB,601) NH, MOBS, MAXM
      WRITE (IUHOB,602) TOMULT, EVH
C-------INITIALIZE VARIABLES
      IHPRINT = 1
      IF (IUH.LT.0) IHPRINT = 0
      IUH = ABS(IUH)
      JT = 0
      ML = 0
      NT = 0
      NTC = 0
      ITT = 0
      IF (MOBS.GT.0) THEN
        DO 20 MM = 1, MOBS
          DO 10 M = 1, MAXM
            MLAY(M,MM) = 0
            PR(M,MM) = 0.0
   10     CONTINUE
   20   CONTINUE
      ENDIF
      DO 30 N = 1, NH
        NDER(5,N) = 0
   30 CONTINUE
C-------LOOP THROUGH HEAD OBSERVATIONS
      DO 90 N = 1, NH
C----------READ FIRST TRANSIENT OBSERVATIONS AT ONE LOCATION (DATA SET
C          6C)
        IF (N.GT.1 .AND. (NDER(4,N-1).LT.0.OR.NTC.LT.NT)) THEN
          IF (NDER(4,N-1).LT.0) THEN
            N1 = N - 1
            NT = -NDER(4,N1)
            NTC = 1
            READ (IUH,510) DID(N1), NDER(4,N1), TOFF, HOBS,
     &                     WT, DUM, IWT
            IF (IHPRINT.EQ.1) WRITE (IOUT,520) N1, DID(N1), NDER(4,N1), 
     &                               TOFF, HOBS, WT
            IF (HOBS.EQ.0 .AND. IWT.EQ.2) THEN
              WRITE (IOUT,580) N1
              IERR = 1
            ENDIF
            CALL TCONV(TOFF,MODTCONV,NOPRSP,TOMULT,NPER,NSTP,
     &                 MAXSTEP,TS,TOFFSET,IOUT,NDER(4,N1),IREFSP,MAXPER)
            WRITE(IUHOB,606) DID(N1),IREFSP,TOFFSET,HOBS,WT,DUM,IWT,1
          ENDIF
C-------SUBSEQUENT OBSERVATIONS AT ONE LOCATION
          NTC = NTC + 1
C----------ASSIGN INFORMATION WHICH STAYS THE SAME
          DO 40 I = 1, 3
            NDER(I,N) = NDER(I,N1)
   40     CONTINUE
cc          ROFF(N) = ROFF(N1)
cc          COFF(N) = COFF(N1)
cc          IOFF(N) = IOFF(N1)
cc          JOFF(N) = JOFF(N1)
          IF (NDER(1,N1).LT.0) THEN
            ML1 = ML
            ML = ML + 1
            DO 60 M = 1, MAXM
              PR(M,ML) = PR(M,ML1)
              MLAY(M,ML) = MLAY(M,ML1)
   60       CONTINUE
          ENDIF
C----------READ INFORMATION UNIQUE TO THIS OBSERVATION (DATA SET 6C)
          READ (IUH,510) DID(N), NDER(4,N), TOFF, HOBS, WT1, WT2,
     &                   IWT
          IF (ITT.EQ.2) THEN
            NDER(5,N) = N1
          ENDIF
          IF (IHPRINT.EQ.1) WRITE (IOUT,520) N, DID(N), NDER(4,N), 
     &                                       TOFF, HOBS, WT
          IF (NTC.EQ.NT .AND. IHPRINT.EQ.1) WRITE (IOUT,570)
          CALL TCONV(TOFF,MODTCONV,NOPRSP,TOMULT,NPER,NSTP,
     &               MAXSTEP,TS,TOFFSET,IOUT,NDER(4,N),IREFSP,MAXPER)
          WRITE(IUHOB,606) DID(N),IREFSP,TOFFSET,HOBS,WT1,WT2,IWT,1
          GOTO 90
        ENDIF
C----------READ A DATA SET 6
        READ (IUH,500) DID(N), (NDER(I,N),I=1,4), ROFF, COFF,
     &                 TOFF, HOBS, WT, IWT
        IF (IHPRINT.EQ.1) WRITE (IOUT,535) N, DID(N), (NDER(I,N),I=1,4),
     &                                     ROFF, COFF, TOFF,
     &                                     HOBS, WT
        CALL TCONV(TOFF,MODTCONV,NOPRSP,TOMULT,NPER,NSTP,
     &             MAXSTEP,TS,TOFFSET,IOUT,NDER(4,N),IREFSP,MAXPER)
        WRITE(IUHOB,603) DID(N),(NDER(I,N),I=1,3),IREFSP,TOFFSET,ROFF,
     &                   COFF,HOBS,WT,IWT,1
C-------ERROR CHECKING
        K = NDER(1,N)
        I = NDER(2,N)
        J = NDER(3,N)
        IF (J.LE.0 .OR. J.GT.NCOL .OR. I.LE.0 .OR. I.GT.NROW) THEN
          WRITE (IOUT,550) N
          IERR = 1
        ENDIF
C-------INITIALIZE SOME VARIABLES
        MM = 1
        TPR = 1.
C-------READ INFORMATION FOR MULTILAYER OBSERVATIONS (DATA SET 6A)
        IF (K.LT.0) THEN
          ML = ML + 1
          READ (IUH,505) (MLAY(M,ML),PR(M,ML),M=1,-K)
          IF (IHPRINT.EQ.1) WRITE(IOUT,540) (MLAY(M,ML),PR(M,ML),M=1,-K)
          WRITE (IUHOB,604) (MLAY(M,ML),PR(M,ML),M=1,-K)
          MM = -K
          TPR = 0.0
        ENDIF
C-------READ FLAG FOR USING TEMPORAL CHANGES IN HEAD (DATA SET 6B)
        IF (NDER(4,N).LT.0) THEN
          READ (IUH,505) ITT
          IF (IHPRINT.EQ.1) WRITE (IOUT,515) ITT
          WRITE (IUHOB,605) ITT
          IF (ITT.NE.1 .AND. ITT.NE.2) THEN
            WRITE (IOUT,575) N
            STOP
          ENDIF
        ENDIF
C-------ERROR CHECKING
        DO 70 M = 1, MM
          KK = K
C----------ASSIGN LAYER NUMBERS AND ADD PROPORTIONS FOR MULTILAYER
C----------OBSERVATION WELLS
          IF (K.LT.0) THEN
            KK = MLAY(M,ML)
            IF (KK.EQ.0) GOTO 70
          ENDIF
C----------CHECK LAYER NUMBER AND WHETHER THE CELL IS ACTIVE
          IF (KK.LE.0 .OR. KK.GT.NLAY) THEN
            WRITE (IOUT,555) N
            IERR = 1
          ENDIF
   70   CONTINUE
   90 CONTINUE
C
      WRITE(*,580) FNAME(1:LENF)
      RETURN
      END
C ======================================================================
      SUBROUTINE RDS7(BASE,IUF,IOUT,VERBOSE,IERR,DID,EVF,PROGNAM,
     &                MODTCONV,NOPRSP,TOMULT,NPER,NSTP,MAXSTEP,TS,
     &                MAXPER,NQ,NQC,NQT,IOWTQ)
C     ******************************************************************
C     READ DATA SET 7 AND WRITE INPUT FILE(S) FOR FLOW-OBSERVATION
C     PACKAGES
C     ******************************************************************
      INTEGER NSTP(MAXPER), IUFOUT(4), KNQT(4), KNQC(4), LENF(4),
     &        LENPACK(4)
      REAL TS(MAXSTEP)
      CHARACTER*2 SUF(4)
      CHARACTER*4 DID(NQT), EXT(4)
      CHARACTER*21 PACKAGE(4)
      CHARACTER*30 PROGNAM
      CHARACTER*200 FNAME(4), BASE
      LOGICAL VERBOSE, ISOPEN(4), NEEDEDIT
C
      DATA (SUF(I),I=1,4)/'RV','GB','ST','DR'/
      DATA (EXT(I),I=1,4)/'.orv','.ogb','.ost','.odr'/
      DATA (PACKAGE(I),I=1,4)/'River','General Head Boundary',
     &                        'Streamflow Routing','Drain'/
C
  500 FORMAT(3I5)
  520 FORMAT(A4,1X,I5,F8.0,2F10.0,I5)
  530 FORMAT(15X,2F5.0,F10.0)
  540 FORMAT(4F10.0)
  550 FORMAT(' ERROR IN READING DATA SET 7')
  560 FORMAT(/,
     &' Head-dependent flow-observation data for',/,
     &' the ',A,' Package have been',/,
     &' written to file: ',A)
  570 FORMAT(/,
     &' NOTE: NQT',A2,' in file ',A,' will need to',/,
     &' be changed to ',I5,' to avoid input error',/,
     &' when MODFLOW-2000 is run.')
  580 FORMAT(/,
     &' NOTE: NQC',A2,' in file ',A,' can be changed to ',I5)
  590 FORMAT(//,
     &' ********************** WARNING ************************',/,
     &' Your MODFLOWP data set specifies IOWTQ as non-zero,',/,
     &' indicating that it provides a WTQ array containing a',/,
     &' full weight matrix on head-dependent flow observations. ',/,
     &' This program does not insert WTQ array values into',/,
     &' MODFLOW-2000 flow-observation input files.  To include ',/,
     &' the WTQ array values in MODFLOW-2000, you must edit the ',/,
     &' input file(s) by hand.',/,
     &' *******************************************************',//)
  595 FORMAT(//,
     &'  ******************** IMPORTANT *************************',/,
     &'  *  The file(s) generated by this program may need      *',/,
     &'  *  minor changes before being used with MODFLOW-2000.  *',/,
     &'  *  Please read NOTEs above for changes that may need   *',/,
     &'  *  to be made.                                         *',/,
     &'  ********************************************************',//)
  600 FORMAT('# File "',A,'"',/,
     &'# Contents are data to be read by the ',A,' Package',/
     &'# of the Observation Process of MODFLOW-2000.',/,
     &'# File written by program ',A)
  601 FORMAT(3(1X,I9),T70,'Item 1: NQ',A2,' NQC',A2,' NQT',A2)
  602 FORMAT(1P,2(1X,G14.7),1X,I9,T70,'Item 2: TOMULT',A2,' EVF',A2,
     &' IOWTQ',A2)
  603 FORMAT(2(1X,I4),T70,'Item 3: NQOB',A2,' NQCL',A2)
  604 FORMAT(A,1X,I5,3(1X,G14.6),2(1X,I4),T70,'Item 4')
  605 FORMAT(3(1X,I9),1X,1P,G14.6,T70,'Item 5: LAYER ROW COLUMN FACTOR')
  606 FORMAT(2(1X,I9),1X,1P,G14.6,T70,'Item 5: SEGMENT REACH FACTOR')
C
      NEEDEDIT = .FALSE.
C
      DO 10 I = 1,4
        ISOPEN(I) = .FALSE.
        KNQT(I) = 0
        KNQC(I) = 0
        LENPACK(I) = NONB_LEN(PACKAGE(I),21)
   10 CONTINUE
C
C     READ DATA FOR EACH CELL GROUP
      DO 100 I = 1,NQ
C       READ DATA SET 7
        READ(IUF,500,ERR=150) IBT,NQOB,NQCL
        NQCLA = ABS(NQCL)
        KNQT(IBT) = KNQT(IBT) + NQOB
        KNQC(IBT) = KNQC(IBT) + NQCLA
C       IF THIS IS THE FIRST CELL GROUP OF A BOUNDARY TYPE, OPEN A FILE
C       AND WRITE A HEADING
        IF (.NOT. ISOPEN(IBT)) THEN
          IUFOUT(IBT) = IGETUNIT(7,1000)
          LENBASE = NONB_LEN(BASE,200)
          FNAME(IBT) = BASE(1:LENBASE)//EXT(IBT)
          LENF(IBT) = LENBASE+4
          OPEN(IUFOUT(IBT),FILE=FNAME(IBT))
          CLOSE(IUFOUT(IBT),STATUS='DELETE')
          OPEN(IUFOUT(IBT),FILE=FNAME(IBT))
          ISOPEN(IBT) = .TRUE.
C         WRITE COMMENTS AND ITEMS 1 - 2 TO FLOW OBSERVATION PACKAGE
C         INPUT FILE
          WRITE (IUFOUT(IBT),600) FNAME(IBT)(1:LENF(IBT)),
     &           PACKAGE(IBT)(1:LENPACK(IBT)), PROGNAM
          WRITE (IUFOUT(IBT),601) NQ, NQC, NQT, (SUF(IBT),J=1,3)
          WRITE (IUFOUT(IBT),602) TOMULT, EVF, 0, (SUF(IBT),J=1,3)
        ENDIF
C       WRITE ITEM 3 FOR THIS CELL GROUP
        WRITE (IUFOUT(IBT),603) NQOB, NQCL, (SUF(IBT),J=1,2)
C       READ DATA SET(S) 7A FOR THIS CELL GROUP AND WRITE ITEM 4
        DO 70 J = 1,NQOB
          READ(IUF,520,ERR=150) DID(J),ITS,TOFF,HOBS,STAT,IST
          CALL TCONV(TOFF,MODTCONV,NOPRSP,TOMULT,NPER,NSTP,
     &               MAXSTEP,TS,TOFFSET,IOUT,ITS,IREFSP,MAXPER)
          WRITE(IUFOUT(IBT),604) DID(J),IREFSP,TOFFSET,HOBS,STAT,IST,
     &                           IBT+1
   70   CONTINUE
C       READ DATA SET(S) 7B FOR THIS CELL GROUP AND WRITE ITEM 5
        NQCLA = ABS(NQCL)
        DO 80 J = 1,NQCLA
          IF (IBT.EQ.3) THEN
            READ(IUF,530,ERR=150) SEG,RCH,FACTOR
            ISEG = INT(SEG)
            IRCH = INT(RCH)
            WRITE(IUFOUT(IBT),606) ISEG,IRCH,FACTOR
          ELSE
            READ(IUF,540,ERR=150) RLAY,ROW,COL,FACTOR
            LAY = INT(RLAY)
            IROW = INT(ROW)
            ICOL = INT(COL)
            WRITE(IUFOUT(IBT),605) LAY,IROW,ICOL,FACTOR
          ENDIF
   80   CONTINUE
  100 CONTINUE
C
C     WRITE MESSAGE(S) FOR EACH FILE GENERATED
      DO 120 I = 1,4
        IF (ISOPEN(I)) THEN
          WRITE(*,560) PACKAGE(I)(1:LENPACK(I)),FNAME(I)(1:LENF(I))
          IF (NQT.NE.KNQT(I)) THEN
            NEEDEDIT = .TRUE.
            WRITE(*,570) SUF(I),FNAME(I)(1:LENF(I)),KNQT(I)
          ENDIF
          IF (NQC.NE.KNQC(I)) THEN
            NEEDEDIT = .TRUE.
            WRITE(*,580) SUF(I),FNAME(I)(1:LENF(I)),KNQC(I)
          ENDIF
        ENDIF
  120 CONTINUE
C
      IF (IOWTQ.NE.0) WRITE(*,590)
      IF (NEEDEDIT) WRITE(*,595)
      RETURN
C
  150 CONTINUE
      WRITE(*,550)
      STOP
      END

