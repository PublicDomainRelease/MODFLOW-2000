C     Last change:  ERB   7 May 2001   11:30 am
      PROGRAM MFPTO2K
C     ******************************************************************
C     CONVERTS MODFLOWP DATA SETS 6 AND 7 TO INPUT FILES
C     FOR PACKAGES OF THE MODFLOW-2000 OBSERVATION PROCESS
C     ******************************************************************
C     To use this program, the name of the NAME file must provided.
C     The "BAS" file, "BCF" file, "PAR" file, and any "DATA" files that
C     contain MODFLOWP input data, which are listed in the NAME file,
C     must be accessible to the program by using the pathnames listed in
C     the NAME file.
C     ******************************************************************
      PARAMETER (MAXNLL=500, MAXIZ=200, MAXCEL=500,
     &           MAXMLAY=100, MAXOBS=5000, MAXPER=500, MAXSTEP=5000)
C     ------------------------------------------------------------------
      INTEGER NLL(MAXNLL), LZ(MAXIZ), IZON(MAXCEL,MAXCEL),
     &        MLAY(MAXMLAY,0:MAXOBS), NDER(5,0:MAXOBS), NSTP(MAXPER)
      REAL MULT(MAXCEL,MAXCEL), PR(MAXMLAY,0:MAXOBS), PERLENG(MAXPER),
     &     TS(MAXSTEP)
      CHARACTER*1 V
      CHARACTER*4 PID, DID(MAXOBS)
      CHARACTER*12 OUTFIL
      CHARACTER*30 PROGNAM
      CHARACTER*80 HEADNG(2), TITLE(2)
      CHARACTER*200 FNAME, NAMFIL, BASE
      LOGICAL VERBOSE
C     ------------------------------------------------------------------
  490 FORMAT(' Enter "V" to enable verbose output, which may help',/,
     &' identify problems:'/)
  500 FORMAT(' Error in opening file: ',A,/,' Enter blank line to quit')
  505 FORMAT(A)
  510 FORMAT(' Enter the name of the NAME file from the MODFLOWP run:',
     &       /)
  520 FORMAT(' Enter the file name base to be used for MODFLOW-2000',
     &       ' input files:',/)
  530 FORMAT(/,' File ',a,' contains information that',/,
     &' may be useful in identifying problems.')
  550 FORMAT(' OUTPUT FROM PROGRAM ',A,/)
  560 FORMAT(10X,'Program ',A,/)
  570 FORMAT(/,' A file named ',A,' will be created and will',/,
     &' contain information that may be useful in identifying',/,
     &' problems.',/)
C     ------------------------------------------------------------------
      IERR = 0
      IUNAM = 99
      IOUT = 100
      PROGNAM = 'MFPTO2K version 0.3 05/07/2001'
      OUTFIL = 'MFPTO2K.OUT'
      VERBOSE = .FALSE.
C
C     IDENTIFY PROGRAM AND OPEN AN OUTPUT FILE FOR STUFF USER NORMALLY
C     WOULDN'T NEED TO SEE
      WRITE(*,560) PROGNAM
      WRITE(*,490)
      READ(*,505) V
      IF (V.EQ.'V' .OR. V.EQ.'v') THEN
        VERBOSE = .TRUE.
        WRITE(*,570) OUTFIL
      ENDIF
      OPEN(IOUT,FILE=OUTFIL)
      CLOSE(IOUT,STATUS='DELETE')
      OPEN(IOUT,FILE=OUTFIL)
      WRITE(IOUT,550) PROGNAM
C
C     GET THE NAME OF THE NAME FILE AND OPEN IT
   10 CONTINUE
      NAMFIL = ' '
      WRITE(*,510)
      READ(*,505) NAMFIL
      IF (NAMFIL.EQ.' ') GOTO 400
      OPEN(IUNAM,FILE=NAMFIL,STATUS='OLD',IOSTAT=ISTAT,ERR=20)
   20 CONTINUE
      IF (ISTAT.NE.0) THEN
        WRITE(*,500) NAMFIL
        GOTO 10
      ENDIF
C
C     GET THE BASE NAME TO BE USED FOR MODFLOW-2000 INPUT FILES
   30 CONTINUE
      BASE = ' '
      WRITE(*,520)
      READ(*,505) BASE
      IF (BASE.EQ.' ') GOTO 400
      LENBASE = NONB_LEN(BASE,200)
C
C     READ THE NAME FILE AND OPEN REQUIRED FILES LISTED IN IT
      CALL RNAMFIL(IUNAM,IUBAS,IUBCF,IUPAR,IOUT,VERBOSE,IERR)
C
C     READ LINE 1 OF THE BCF FILE
      CALL RBCF(IUBCF,IOUT,ISS,VERBOSE,IERR)
      IF (IERR.NE.0) GOTO 400
C
C     READ THE BAS FILE
      CALL RBAS(IUBAS,IOUT,HEADNG,NLAY,NROW,NCOL,NPER,VERBOSE,IERR,NSTP,
     &          PERLENG,IZON,IFREFM,MULT,TS,MAXSTEP,JJ,MAXPER,ISS,
     &          MAXCEL)
      IF (IERR.NE.0) GOTO 400
C     READ LINES 1-8 OF THE PAR FILE
      CALL RLINES(IUPAR,TITLE,NP,NSM,NSN,N2,LZ1,NMM,NMP,NZM,NH,MOBS,
     &            MAXM,NQ,NQC,NQT,NRWD,NZER,NPNG,NLOG,VERBOSE,IERR,
     &            IOWTQ)
      IF (IERR.NE.0) GOTO 400
C     READ DATA SETS 1A, 1B, 1C, 1D
      CALL RDS1(IUPAR,NRWD,NZER,NPNG,NLOG,VERBOSE,IERR)
      IF (IERR.NE.0) GOTO 400
C     READ DATA SETS 2 (PARAMETER DEFINITIONS)
      CALL RDS2(IUPAR,NLL,LZ,N2,LZ1,NP,VERBOSE,IERR)
      IF (IERR.NE.0) GOTO 400
C     READ DATA SET 3 (MULTIPLIER ARRAYS)
      CALL RDS3(IUPAR,IOUT,NMM,NMP,NCOL,NROW,MULT,VERBOSE,IERR)
      IF (IERR.NE.0) GOTO 400
C     READ DATA SET 4 (ZONE ARRAYS)
      CALL RDS4(IUPAR,IOUT,NZM,NCOL,NROW,IZON,VERBOSE,IERR)
      IF (IERR.NE.0) GOTO 400
C     READ DATA SET 5
      CALL RDS5(IUPAR,IUH,IUF,IOUT,VERBOSE,IERR,EVH,EVF)
      IF (IERR.NE.0) GOTO 400
C
C     DETERMINE TIME-OFFSET MULTIPLIER AND METHOD FOR GENERATING
C     REFERENCE STRESS PERIOD
      CALL RSPTO(ISS,MODTCONV,NOPRSP,TOMULT)
C
C     READ DATA SET 6 AND PRINT HEAD-OBSERVATION DATA
      IF (NH.GT.0) THEN
        CALL RDS6(BASE,IUH,IOUT,VERBOSE,IERR,NH,MOBS,MAXM,MLAY,PR,NDER,
     &            NLAY,NCOL,NROW,DID,EVH,PROGNAM,MODTCONV,NOPRSP,
     &            TOMULT,NPER,NSTP,MAXSTEP,TS,MAXPER)
      ENDIF
C
C     READ DATA SET 7 AND PRINT FLOW-OBSERVATION DATA
      IF (NQ.GT.0) THEN
        CALL RDS7(BASE,IUF,IOUT,VERBOSE,IERR,DID,EVF,PROGNAM,MODTCONV,
     &            NOPRSP,TOMULT,NPER,NSTP,MAXSTEP,TS,MAXPER,
     &            NQ,NQC,NQT,IOWTQ)
      ENDIF
C
  400 CONTINUE
      IF (VERBOSE) THEN
        WRITE(*,530) OUTFIL
      ELSE
        CLOSE(IOUT,STATUS='DELETE')
      ENDIF
C
      STOP
      END
C

