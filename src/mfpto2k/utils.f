C=======================================================================
      SUBROUTINE RSPTO(ISS,MODTCONV,NOPRSP,TOMULT)
C     ******************************************************************
C     DETERMINE TIME-OFFSET MULTIPLIER, MODEL-TIME CONVERSION FACTOR,
C     AND METHOD FOR GENERATING REFERENCE STRESS PERIOD FOR OBSERVATIONS
C     ******************************************************************
      REAL TU(6)
C
      DATA (TU(I),I=1,6)/1.0,60.0,3600.0,86400.0,31536000.0,31557600.0/
C
  500 FORMAT(////,' Enter number for model time unit used in MODFLOWP ',
     &       'simulation:')
  510 FORMAT('   1 - Seconds',/,
     &       '   2 - Minutes',/,
     &       '   3 - Hours',/,
     &       '   4 - Days',/,
     &       '   5 - Years (365 days)',/,
     &       '   6 - Years (365.25 days)',/)
  520 FORMAT(////,' Enter number for model time unit to be used in',
     &       ' MODFLOW-2000 simulation:')
  540 FORMAT(////,' Enter number for time unit to be used for all',/,
     &       ' observations in MODFLOW-2000 simulation:')
  550 FORMAT(/,' All values entered must be between 1 and 6',/)
  560 FORMAT(////,
     &' Enter 1, 2, or 3 to select method by which the',/,
     &' Reference Stress Period (IREFSP) is determined:',/,
     &'   1 - IREFSP will be 1 for all observations',/,
     &'   2 - IREFSP will be 2 for all observations that fall in',/,
     &'         stress periods after stress period 1',/,
     &'   3 - IREFSP for each observation will be the stress period',/,
     &'         in which the observation falls',//,
     &' For MODFLOW-2000 simulations in which the first stress',/,
     &' period is steady and others are transient, "2" is recommended.',
     &//,
     &' For MODFLOW-2000 simulations in which all stress periods',/,
     &' are transient, "1" is recommended.',//,
     &' Method 3 assumes that the MODFLOW-2000 simulation will have',/,
     &' a steady-state stress period 1 to correspond to the initial',/,
     &' MODFLOWP steady-state stress period.',//,
     &' Enter method 1, 2, or 3:',/)
  570 FORMAT(/,' Method must be between 1 and 3.',/)
  580 FORMAT(////)
C
C     CALCULATE TIME-OFFSET MULTIPLIER (TOMULT) FOR ALL OBSERVATIONS
      IF (ISS.EQ.0) THEN
C       SIMULATION IS TRANSIENT
   10   CONTINUE
        WRITE(*,500)
        WRITE(*,510)
        READ(*,*) ITUMFP
        WRITE(*,520)
        WRITE(*,510)
        READ(*,*) ITUMF2K
        WRITE(*,540)
        WRITE(*,510)
        READ(*,*) ITUOBS
C
C       CHECK VALUES
        IF (ITUMFP.LT.1 .OR. ITUMFP.GT.6 .OR.
     &      ITUMF2K.LT.1 .OR. ITUMF2K.GT.6 .OR.
     &      ITUOBS.LT.1 .OR. ITUOBS.GT.6) THEN
          WRITE(*,550)
          GOTO 10
        ENDIF
C
        MODTCONV = TU(ITUMFP)/TU(ITUMF2K)
        TOMULT = TU(ITUOBS)/TU(ITUMF2K)
   20   CONTINUE
        WRITE(*,560)
        READ(*,*) NOPRSP
        IF (NOPRSP.LT.1 .OR. NOPRSP.GT.3) THEN
          WRITE(*,570)
          GOTO 20
        ENDIF
        WRITE(*,580)
      ELSE
C       SIMULATION IS STEADY-STATE
        MODTCONV = 1.0
        TOMULT = 1.0
        NOPRSP = 0
      ENDIF
C
      RETURN
      END

C=======================================================================
      SUBROUTINE URDCOM(IN,IOUT,LINE)
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
   10 READ(IN,'(A)') LINE
      IF(LINE(1:1).NE.'#') RETURN
      L=LEN(LINE)
      IF(L.GT.79) L=79
      DO 20 I=L,1,-1
      IF(LINE(I:I).NE.' ') GO TO 30
   20 CONTINUE
   30 IF (IOUT.GT.0) WRITE(IOUT,'(1X,A)') LINE(1:I)
      GO TO 10
C
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
      INTEGER FUNCTION NONB_LEN(CHARVAR,LENGTH)
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
C
      END
C ======================================================================
      SUBROUTINE U2DREL(A,ANAME,II,JJ,K,IN,IOUT)
C
C
C-----VERSION 1539 22JUNE1993 U2DREL
C     ******************************************************************
C     ROUTINE TO INPUT 2-D REAL DATA MATRICES
C       A IS ARRAY TO INPUT
C       ANAME IS 24 CHARACTER DESCRIPTION OF A
C       II IS NO. OF ROWS
C       JJ IS NO. OF COLS
C       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT --)
C              IF K=0, NO LAYER IS PRINTED
C              IF K<0, CROSS SECTION IS PRINTED)
C       IN IS INPUT UNIT
C       IOUT IS OUTPUT UNIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*24 ANAME
      DIMENSION A(JJ,II)
      CHARACTER*20 FMTIN
      CHARACTER*80 CNTRL
      CHARACTER*16 TEXT
      CHARACTER*80 FNAME
      DATA NUNOPN/99/
C     ------------------------------------------------------------------
C
C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
C
C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT',I4,':',/1X,A)
         ICLOSE=1
      ELSE
C
C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
    1    FORMAT(I10,F10.0,A20,I10)
cc         write(iout,600)locat,cnstnt,fmtin,iprn
cc  600 format(' in u2drel, locat cnstnt fmtin iprn = ',/,i5,g12.5,a20,i5)
      END IF
C
C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,IOUT,IN)
         IF(LOCAT.NE.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            IF(ICLOSE.NE.0) THEN
               IF(FMTIN.EQ.'(BINARY)') THEN
                  OPEN(UNIT=LOCAT,FILE=FNAME,FORM='UNFORMATTED')
               ELSE
                  OPEN(UNIT=LOCAT,FILE=FNAME)
               END IF
            END IF
            IF(LOCAT.GT.0 .AND. FMTIN.EQ.'(BINARY)') LOCAT=-LOCAT
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
C
C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT) 200,50,90
C
C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
   50 DO 80 I=1,II
      DO 80 J=1,JJ
   80 A(J,I)=CNSTNT
      IF(K.GT.0) WRITE(IOUT,2) ANAME,CNSTNT,K
    2 FORMAT(1X,/1X,A,' =',G15.7,' FOR LAYER',I4)
      IF(K.LE.0) WRITE(IOUT,3) ANAME,CNSTNT
    3 FORMAT(1X,/1X,A,' =',G15.7)
      RETURN
C
C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 IF(K.GT.0) THEN
         WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
   94    FORMAT(1X,///11X,A,' FOR LAYER',I4,/
     1       1X,'READING ON UNIT',I4,' WITH FORMAT: ',A)
      ELSE IF(K.EQ.0) THEN
         WRITE(IOUT,95) ANAME,LOCAT,FMTIN
   95    FORMAT(1X,///11X,A,/
     1       1X,'READING ON UNIT',I4,' WITH FORMAT: ',A)
      ELSE
         WRITE(IOUT,96) ANAME,LOCAT,FMTIN
   96    FORMAT(1X,///11X,A,' FOR CROSS SECTION',/
     1       1X,'READING ON UNIT',I4,' WITH FORMAT: ',A)
      END IF
      DO 100 I=1,II
      IF(FMTIN.EQ.'(FREE)') THEN
         READ(LOCAT,*) (A(J,I),J=1,JJ)
      ELSE
         READ(LOCAT,FMTIN) (A(J,I),J=1,JJ)
      END IF
  100 CONTINUE
      GO TO 300
C
C4C-----LOCAT<0; READ UNFORMATTED ARRAY VALUES.
  200 LOCAT=-LOCAT
      IF(K.GT.0) THEN
         WRITE(IOUT,201) ANAME,K,LOCAT
  201    FORMAT(1X,///11X,A,' FOR LAYER',I4,/
     1    1X,'READING BINARY ON UNIT',I4)
      ELSE IF(K.EQ.0) THEN
         WRITE(IOUT,202) ANAME,LOCAT
  202    FORMAT(1X,///1X,A,/
     1    1X,'READING BINARY ON UNIT',I4)
      ELSE
         WRITE(IOUT,203) ANAME,LOCAT
  203    FORMAT(1X,///1X,A,' FOR CROSS SECTION',/
     1    1X,'READING BINARY ON UNIT',I4)
      END IF
      READ(LOCAT) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY
      READ(LOCAT) A
C
C5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
  300 IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
      ZERO=0.
      IF(CNSTNT.EQ.ZERO) GO TO 320
      DO 310 I=1,II
      DO 310 J=1,JJ
      A(J,I)=A(J,I)*CNSTNT
  310 CONTINUE
C
C6------IF PRINT CODE (IPRN) >0 OR =0 THEN PRINT ARRAY VALUES.
  320 IF(IPRN.GE.0) CALL ULAPRW(A,ANAME,0,0,JJ,II,0,IPRN,IOUT)
C
C7------RETURN
      RETURN
C
C8------CONTROL RECORD ERROR.
  500 IF(K.GT.0) THEN
         WRITE(IOUT,501) ANAME,K
  501    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,
     1     ' FOR LAYER',I4,':')
      ELSE
         WRITE(IOUT,502) ANAME
  502    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      END IF
      WRITE(IOUT,'(1X,A)') CNTRL
      STOP
      END
C ======================================================================
      SUBROUTINE U2DINT(IA,ANAME,II,JJ,K,IN,IOUT)
C
C
C-----VERSION 0801 01NOV1995 U2DINT
C     ******************************************************************
C     ROUTINE TO INPUT 2-D INTEGER DATA MATRICES
C       IA IS ARRAY TO INPUT
C       ANAME IS 24 CHARACTER DESCRIPTION OF IA
C       II IS NO. OF ROWS
C       JJ IS NO. OF COLS
C       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT --
C              IF K=0, NO LAYER IS PRINTED
C              IF K<0, CROSS SECTION IS PRINTED)
C       IN IS INPUT UNIT
C       IOUT IS OUTPUT UNIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*24 ANAME
      DIMENSION IA(JJ,II)
      CHARACTER*20 FMTIN
      CHARACTER*80 CNTRL
      CHARACTER*80 FNAME
      DATA NUNOPN/99/
C     ------------------------------------------------------------------
C
C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
C
C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT',I4,':',/1X,A)
         ICLOSE=1
      ELSE
C
C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=600) LOCAT,ICONST,FMTIN,IPRN
    1    FORMAT(I10,I10,A20,I10)
      END IF
C
C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,ICONST,R,IOUT,IN)
         IF(LOCAT.NE.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            IF(ICLOSE.NE.0) THEN
               IF(FMTIN.EQ.'(BINARY)') THEN
                  OPEN(UNIT=LOCAT,FILE=FNAME,FORM='UNFORMATTED')
               ELSE
                  OPEN(UNIT=LOCAT,FILE=FNAME)
               END IF
            END IF
            IF(LOCAT.GT.0 .AND. FMTIN.EQ.'BINARY') LOCAT=-LOCAT
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
C
C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT) 200,50,90
C
C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO ICONST. RETURN.
   50 DO 80 I=1,II
      DO 80 J=1,JJ
   80 IA(J,I)=ICONST
      IF(K.GT.0) WRITE(IOUT,82) ANAME,ICONST,K
   82 FORMAT(1X,/1X,A,' =',I15,' FOR LAYER',I4)
      IF(K.LE.0) WRITE(IOUT,83) ANAME,ICONST
   83 FORMAT(1X,/1X,A,' =',I15)
      RETURN
C
C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 IF(K.GT.0) THEN
         WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
   94    FORMAT(1X,///11X,A,' FOR LAYER',I4,/
     1       1X,'READING ON UNIT',I4,' WITH FORMAT: ',A)
      ELSE IF(K.EQ.0) THEN
         WRITE(IOUT,95) ANAME,LOCAT,FMTIN
   95    FORMAT(1X,///11X,A,/
     1       1X,'READING ON UNIT',I4,' WITH FORMAT: ',A)
      ELSE
         WRITE(IOUT,96) ANAME,LOCAT,FMTIN
   96    FORMAT(1X,///11X,A,' FOR CROSS SECTION',/
     1       1X,'READING ON UNIT',I4,' WITH FORMAT: ',A)
      END IF
      DO 100 I=1,II
      IF(FMTIN.EQ.'(FREE)') THEN
         READ(LOCAT,*) (IA(J,I),J=1,JJ)
      ELSE
         READ(LOCAT,FMTIN) (IA(J,I),J=1,JJ)
      END IF
  100 CONTINUE
      GO TO 300
C
C4C-----LOCAT<0; READ UNFORMATTED RECORD CONTAINING ARRAY VALUES.
  200 LOCAT=-LOCAT
      IF(K.GT.0) THEN
         WRITE(IOUT,201) ANAME,K,LOCAT
  201    FORMAT(1X,///11X,A,' FOR LAYER',I4,/
     1    1X,'READING BINARY ON UNIT',I4)
      ELSE IF(K.EQ.0) THEN
         WRITE(IOUT,202) ANAME,LOCAT
  202    FORMAT(1X,///11X,A,/
     1    1X,'READING BINARY ON UNIT',I4)
      ELSE
         WRITE(IOUT,203) ANAME,LOCAT
  203    FORMAT(1X,///11X,A,' FOR CROSS SECTION',/
     1    1X,'READING BINARY ON UNIT',I4)
      END IF
      READ(LOCAT)
      READ(LOCAT) IA
C
C5------IF ICONST NOT ZERO THEN MULTIPLY ARRAY VALUES BY ICONST.
  300 IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
      IF(ICONST.EQ.0) GO TO 320
      DO 310 I=1,II
      DO 310 J=1,JJ
      IA(J,I)=IA(J,I)*ICONST
  310 CONTINUE
C
C6------IF PRINT CODE (IPRN) <0 THEN RETURN.
  320 IF(IPRN.LT.0) RETURN
C
C7------PRINT COLUMN NUMBERS AT TOP OF PAGE.
      IF(IPRN.GT.9 .OR. IPRN.EQ.0) IPRN=6
      GO TO(401,402,403,404,405,406,407,408,409), IPRN
401   CALL UCOLNO(1,JJ,4,60,2,IOUT)
      GO TO 500
402   CALL UCOLNO(1,JJ,4,40,3,IOUT)
      GO TO 500
403   CALL UCOLNO(1,JJ,4,30,4,IOUT)
      GO TO 500
404   CALL UCOLNO(1,JJ,4,25,5,IOUT)
      GO TO 500
405   CALL UCOLNO(1,JJ,4,20,6,IOUT)
      GO TO 500
406   CALL UCOLNO(1,JJ,4,10,12,IOUT)
      GO TO 500
407   CALL UCOLNO(1,JJ,4,25,3,IOUT)
      GO TO 500
408   CALL UCOLNO(1,JJ,4,15,5,IOUT)
      GO TO 500
409   CALL UCOLNO(1,JJ,4,10,7,IOUT)
C
C8------PRINT EACH ROW IN THE ARRAY.
500   DO 510 I=1,II
      GO TO(501,502,503,504,505,506,507,508,509), IPRN
C
C----------------FORMAT 60I1
  501 WRITE(IOUT,551) I,(IA(J,I),J=1,JJ)
  551 FORMAT(1X,I3,1X,60(1X,I1):/(5X,60(1X,I1)))
      GO TO 510
C
C----------------FORMAT 40I2
  502 WRITE(IOUT,552) I,(IA(J,I),J=1,JJ)
  552 FORMAT(1X,I3,1X,40(1X,I2):/(5X,40(1X,I2)))
      GO TO 510
C
C----------------FORMAT 30I3
  503 WRITE(IOUT,553) I,(IA(J,I),J=1,JJ)
  553 FORMAT(1X,I3,1X,30(1X,I3):/(5X,30(1X,I3)))
      GO TO 510
C
C----------------FORMAT 25I4
  504 WRITE(IOUT,554) I,(IA(J,I),J=1,JJ)
  554 FORMAT(1X,I3,1X,25(1X,I4):/(5X,25(1X,I4)))
      GO TO 510
C
C----------------FORMAT 20I5
  505 WRITE(IOUT,555) I,(IA(J,I),J=1,JJ)
  555 FORMAT(1X,I3,1X,20(1X,I5):/(5X,20(1X,I5)))
      GO TO 510
C
C----------------FORMAT 10I11
  506 WRITE(IOUT,556) I,(IA(J,I),J=1,JJ)
  556 FORMAT(1X,I3,1X,10(1X,I11):/(5X,10(1X,I11)))
      GO TO 510
C
C----------------FORMAT 25I2
  507 WRITE(IOUT,557) I,(IA(J,I),J=1,JJ)
  557 FORMAT(1X,I3,1X,25(1X,I2):/(5X,25(1X,I2)))
      GO TO 510
C
C----------------FORMAT 15I4
  508 WRITE(IOUT,558) I,(IA(J,I),J=1,JJ)
  558 FORMAT(1X,I3,1X,15(1X,I4):/(5X,10(1X,I4)))
      GO TO 510
C
C----------------FORMAT 10I6
  509 WRITE(IOUT,559) I,(IA(J,I),J=1,JJ)
  559 FORMAT(1X,I3,1X,10(1X,I6):/(5X,10(1X,I6)))
C
  510 CONTINUE
C
C9------RETURN
      RETURN
C
C10-----CONTROL RECORD ERROR.
  600 IF(K.GT.0) THEN
         WRITE(IOUT,601) ANAME,K
  601    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,
     1     ' FOR LAYER',I4,':')
      ELSE
         WRITE(IOUT,602) ANAME
  602    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      END IF
      WRITE(IOUT,'(1X,A)') CNTRL
      STOP
      END
C ======================================================================
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
C ======================================================================
      SUBROUTINE UCOLNO(NLBL1,NLBL2,NSPACE,NCPL,NDIG,IOUT)
C
C
C-----VERSION 0934 22JUNE1992 UCOLNO
C     ******************************************************************
C     OUTPUT COLUMN NUMBERS ABOVE A MATRIX PRINTOUT
C        NLBL1 IS THE START COLUMN LABEL (NUMBER)
C        NLBL2 IS THE STOP COLUMN LABEL (NUMBER)
C        NSPACE IS NUMBER OF BLANK SPACES TO LEAVE AT START OF LINE
C        NCPL IS NUMBER OF COLUMN NUMBERS PER LINE
C        NDIG IS NUMBER OF CHARACTERS IN EACH COLUMN FIELD
C        IOUT IS OUTPUT CHANNEL
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*1 DOT,SPACE,DG,BF
      DIMENSION BF(130),DG(10)
C
      DATA DG(1),DG(2),DG(3),DG(4),DG(5),DG(6),DG(7),DG(8),DG(9),DG(10)/
     1         '0','1','2','3','4','5','6','7','8','9'/
      DATA DOT,SPACE/'.',' '/
C     ------------------------------------------------------------------
C
C1------CALCULATE # OF COLUMNS TO BE PRINTED (NLBL), WIDTH
C1------OF A LINE (NTOT), NUMBER OF LINES (NWRAP).
      WRITE(IOUT,1)
    1 FORMAT(1X)
      NLBL=NLBL2-NLBL1+1
      N=NLBL
      IF(NLBL.GT.NCPL) N=NCPL
      NTOT=NSPACE+N*NDIG
      IF(NTOT.GT.130) GO TO 50
      NWRAP=(NLBL-1)/NCPL + 1
      J1=NLBL1-NCPL
      J2=NLBL1-1
C
C2------BUILD AND PRINT EACH LINE
      DO 40 N=1,NWRAP
C
C3------CLEAR THE BUFFER (BF).
      DO 20 I=1,130
      BF(I)=SPACE
   20 CONTINUE
      NBF=NSPACE
C
C4------DETERMINE FIRST (J1) AND LAST (J2) COLUMN # FOR THIS LINE.
      J1=J1+NCPL
      J2=J2+NCPL
      IF(J2.GT.NLBL2) J2=NLBL2
C5------LOAD THE COLUMN #'S INTO THE BUFFER.
      DO 30 J=J1,J2
      NBF=NBF+NDIG
      I2=J/10
      I1=J-I2*10+1
      BF(NBF)=DG(I1)
      IF(I2.EQ.0) GO TO 30
      I3=I2/10
      I2=I2-I3*10+1
      BF(NBF-1)=DG(I2)
      IF(I3.EQ.0) GO TO 30
      BF(NBF-2)=DG(I3+1)
   30 CONTINUE
C
C6------PRINT THE CONTENTS OF THE BUFFER (I.E. PRINT THE LINE).
      WRITE(IOUT,31) (BF(I),I=1,NBF)
   31 FORMAT(1X,130A1)
C
   40 CONTINUE
C
C7------PRINT A LINE OF DOTS (FOR ESTHETIC PURPOSES ONLY).
   50 NTOT=NTOT
      IF(NTOT.GT.130) NTOT=130
      WRITE(IOUT,51) (DOT,I=1,NTOT)
   51 FORMAT(1X,130A1)
C
C8------RETURN
      RETURN
      END
C ======================================================================
      SUBROUTINE ULAPRW(BUF,TEXT,KSTP,KPER,NCOL,NROW,ILAY,IPRN,IOUT)
C
C
C-----VERSION 0758 01NOV1995 ULAPRW
C     ******************************************************************
C     PRINT 1 LAYER ARRAY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*16 TEXT
      DIMENSION BUF(NCOL,NROW)
C     ------------------------------------------------------------------
C
C1------PRINT A HEADER DEPENDING ON ILAY
      IF(ILAY.GT.0) THEN
         WRITE(IOUT,1) TEXT,ILAY,KSTP,KPER
    1    FORMAT('1',/2X,A,' IN LAYER',I3,' AT END OF TIME STEP',I3,
     1     ' IN STRESS PERIOD',I3/2X,71('-'))
      ELSE IF(ILAY.LT.0) THEN
         WRITE(IOUT,2) TEXT,KSTP,KPER
    2    FORMAT('1',/2X,A,' FOR CROSS SECTION AT END OF TIME STEP',I3,
     1     ' IN STRESS PERIOD',I3/2X,77('-'))
      END IF
C
C2------MAKE SURE THE FORMAT CODE (IP OR IPRN) IS
C2------BETWEEN 1 AND 13.
    5 IP=IPRN
      IF(IP.LT.1 .OR. IP.GT.18) IP=12
C
C3------CALL THE UTILITY MODULE UCOLNO TO PRINT COLUMN NUMBERS.
      IF(IP.EQ.1) CALL UCOLNO(1,NCOL,0,11,11,IOUT)
      IF(IP.EQ.2) CALL UCOLNO(1,NCOL,0,9,14,IOUT)
      IF(IP.GT.2 .AND. IP.LT.7) CALL UCOLNO(1,NCOL,3,15,8,IOUT)
      IF(IP.GT.6 .AND. IP.LT.12) CALL UCOLNO(1,NCOL,3,20,6,IOUT)
      IF(IP.EQ.12) CALL UCOLNO(1,NCOL,0,10,12,IOUT)
      IF(IP.GE.13 .AND. IP.LE.18) CALL UCOLNO(1,NCOL,3,10,7,IOUT)
C
C4------LOOP THROUGH THE ROWS PRINTING EACH ONE IN ITS ENTIRETY.
      DO 1000 I=1,NROW
      GO TO(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,
     1      180), IP
C
C------------ FORMAT 11G10.3
   10 WRITE(IOUT,11) I,(BUF(J,I),J=1,NCOL)
   11 FORMAT(1X,I3,2X,1PG10.3,10(1X,G10.3):/(5X,11(1X,G10.3)))
      GO TO 1000
C
C------------ FORMAT 9G13.6
   20 WRITE(IOUT,21) I,(BUF(J,I),J=1,NCOL)
   21 FORMAT(1X,I3,2X,1PG13.6,8(1X,G13.6):/(5X,9(1X,G13.6)))
      GO TO 1000
C
C------------ FORMAT 15F7.1
   30 WRITE(IOUT,31) I,(BUF(J,I),J=1,NCOL)
   31 FORMAT(1X,I3,1X,15(1X,F7.1):/(5X,15(1X,F7.1)))
      GO TO 1000
C
C------------ FORMAT 15F7.2
   40 WRITE(IOUT,41) I,(BUF(J,I),J=1,NCOL)
   41 FORMAT(1X,I3,1X,15(1X,F7.2):/(5X,15(1X,F7.2)))
      GO TO 1000
C
C------------ FORMAT 15F7.3
   50 WRITE(IOUT,51) I,(BUF(J,I),J=1,NCOL)
   51 FORMAT(1X,I3,1X,15(1X,F7.3):/(5X,15(1X,F7.3)))
      GO TO 1000
C
C------------ FORMAT 15F7.4
   60 WRITE(IOUT,61) I,(BUF(J,I),J=1,NCOL)
   61 FORMAT(1X,I3,1X,15(1X,F7.4):/(5X,15(1X,F7.4)))
      GO TO 1000
C
C------------ FORMAT 20F5.0
   70 WRITE(IOUT,71) I,(BUF(J,I),J=1,NCOL)
   71 FORMAT(1X,I3,1X,20(1X,F5.0):/(5X,20(1X,F5.0)))
      GO TO 1000
C
C------------ FORMAT 20F5.1
   80 WRITE(IOUT,81) I,(BUF(J,I),J=1,NCOL)
   81 FORMAT(1X,I3,1X,20(1X,F5.1):/(5X,20(1X,F5.1)))
      GO TO 1000
C
C------------ FORMAT 20F5.2
   90 WRITE(IOUT,91) I,(BUF(J,I),J=1,NCOL)
   91 FORMAT(1X,I3,1X,20(1X,F5.2):/(5X,20(1X,F5.2)))
      GO TO 1000
C
C------------ FORMAT 20F5.3
  100 WRITE(IOUT,101) I,(BUF(J,I),J=1,NCOL)
  101 FORMAT(1X,I3,1X,20(1X,F5.3):/(5X,20(1X,F5.3)))
      GO TO 1000
C
C------------ FORMAT 20F5.4
  110 WRITE(IOUT,111) I,(BUF(J,I),J=1,NCOL)
  111 FORMAT(1X,I3,1X,20(1X,F5.4):/(5X,20(1X,F5.4)))
      GO TO 1000
C
C------------ FORMAT 10G11.4
  120 WRITE(IOUT,121) I,(BUF(J,I),J=1,NCOL)
  121 FORMAT(1X,I3,2X,1PG11.4,9(1X,G11.4):/(5X,10(1X,G11.4)))
      GO TO 1000
C
C------------ FORMAT 10F6.0
  130 WRITE(IOUT,131) I,(BUF(J,I),J=1,NCOL)
  131 FORMAT(1X,I3,1X,10(1X,F6.0):/(5X,10(1X,F6.0)))
      GO TO 1000
C
C------------ FORMAT 10F6.1
  140 WRITE(IOUT,141) I,(BUF(J,I),J=1,NCOL)
  141 FORMAT(1X,I3,1X,10(1X,F6.1):/(5X,10(1X,F6.1)))
      GO TO 1000
C
C------------ FORMAT 10F6.2
  150 WRITE(IOUT,151) I,(BUF(J,I),J=1,NCOL)
  151 FORMAT(1X,I3,1X,10(1X,F6.2):/(5X,10(1X,F6.2)))
      GO TO 1000
C
C------------ FORMAT 10F6.3
  160 WRITE(IOUT,161) I,(BUF(J,I),J=1,NCOL)
  161 FORMAT(1X,I3,1X,10(1X,F6.3):/(5X,10(1X,F6.3)))
      GO TO 1000
C
C------------ FORMAT 10F6.4
  170 WRITE(IOUT,171) I,(BUF(J,I),J=1,NCOL)
  171 FORMAT(1X,I3,1X,10(1X,F6.4):/(5X,10(1X,F6.4)))
      GO TO 1000
C
C------------ FORMAT 10F6.5
  180 WRITE(IOUT,181) I,(BUF(J,I),J=1,NCOL)
  181 FORMAT(1X,I3,1X,10(1X,F6.5):/(5X,10(1X,F6.5)))
C
 1000 CONTINUE
C
C5------RETURN
      RETURN
      END
C=======================================================================
      INTEGER FUNCTION IGETUNIT(IFIRST,MAXUNIT)
C     VERSION 19981030 ERB
C     ******************************************************************
C     FIND FIRST UNUSED FILE UNIT NUMBER BETWEEN IFIRST AND MAXUNIT
C     ******************************************************************
C        SPECIFICATIONS:
C     -----------------------------------------------------------------
      INTEGER I, IFIRST, IOST, MAXUNIT
      LOGICAL LEX, LOP
C     -----------------------------------------------------------------
C
      LEX = .FALSE.
      LOP = .TRUE.
C
C     LOOP THROUGH RANGE PROVIDED TO FIND FIRST UNUSED UNIT NUMBER
      DO 10 I=IFIRST,MAXUNIT
        INQUIRE(UNIT=I,IOSTAT=IOST,EXIST=LEX,OPENED=LOP,ERR=5)
        IF (IOST.EQ.0 .AND. LEX) THEN
          IF (.NOT.LOP) THEN
            IGETUNIT = I
            RETURN
          ENDIF
        ENDIF
 5      CONTINUE  
10    CONTINUE
C
C     IF THERE ARE NO UNUSED UNIT NUMBERS IN RANGE PROVIDED, RETURN
C     A VALUE INDICATING AN ERROR
      IGETUNIT = -1
C
      RETURN
      END
C=======================================================================
      SUBROUTINE TCONV(TOFF,MODTCONV,NOPRSP,TOMULT,NPER,NSTP,
     &                 MAXSTEP,TS,TOFFSET,IOUT,MFPTS,IREFSP,MAXPER)
C     ******************************************************************
C     MAKE TIME-UNIT CONVERSIONS FOR ONE OBSERVATION TIME -- ASSIGN
C     IREFSP AND TOFFSET
C     ******************************************************************
      INTEGER NSTP(MAXPER)
      REAL TS(MAXSTEP)
C     ------------------------------------------------------------------
  500 FORMAT(' ERROR: OBSERVATION TIME STEP NOT WITHIN SIMULATION',/,
     &' TIME DEFINED IN BAS FILE')
C
C     IF MFPTS < 0, IT'S THE FIRST OBSERVATION IN A SET OF TRANSIENT
C     OBSERVATIONS
      IF (MFPTS.LT.0) THEN
        IREFSP = MFPTS
        TOFFSET = 0.0
        RETURN
      ENDIF
C
C     IF EITHER THE SIMULATION IS STEADY STATE OR THE OBSERVATION IS FOR
C     THE END OF TIME STEP 0, SET IREFSP AND TOFFSET AND RETURN
      IF (NOPRSP.EQ.0 .OR. (MFPTS.EQ.0 .AND. TOFF.EQ.0.0)) THEN
        IREFSP = 1
        TOFFSET = 0.0
        RETURN
      ENDIF
C
C     THE SIMULATION IS TRANSIENT AND THE OBSERVATION TIME IS AFTER
C     THE END OF TIME STEP 0
C
C     DETERMINE IREFSP
      IF (NOPRSP.LE.2) THEN
        IREFSP = NOPRSP
      ELSE
C       FIND STRESS PERIOD IN WHICH OBSERVATION OCCURS, BASED ON
C       MFPTS, TOFF, AND NSTP.  ALSO FIND TIME STEP NUMBER IN STRESS
C       PERIOD.  THIS ALGORITHM ASSUMES THAT IN THE MODFLOW-2000
C       SIMULATION, STRESS PERIOD 1 WILL BE A STEADY-STATE STRESS
C       PERIOD CORRESPONDING TO THE INTIAL STEADY-STATE STRESS PERIOD
C       CODED INTO MODFLOWP
        KTS = 0
        DO 20 ISP = 1, NPER
          KTS = KTS + NSTP(ISP)
          IF (MFPTS.LT.KTS) THEN
            IREFSP = ISP + 1
            GOTO 30
          ELSEIF (MFPTS.EQ.KTS) THEN
            IF (TOFF.EQ.0.0) THEN
              IREFSP = ISP + 1
              GOTO 30
            ENDIF
          ENDIF
   20   CONTINUE
        WRITE(IOUT,500)
        STOP
   30   CONTINUE
      ENDIF
C
C     FIND MODFLOWP TIME STEP CORRESPONDING TO FIRST TIME STEP OF
C     MODFLOW-2000 STRESS PERIOD IREFSP
      IF (IREFSP.LE.2) THEN
        MFPTS1 = IREFSP - 1
      ELSE
        KTS = 0
C       ISP is a MODFLOW-2000 stress period number
        DO 50 ISP = 2, IREFSP-1
C         NSTP IS TIME STEP COUNT FROM MODFLOWP SIMULATION
          KTS = KTS + NSTP(ISP-1)
   50   CONTINUE
        MFPTS1 = KTS + 1
      ENDIF
C
C     CALCULATE TOFFSET
C
C     ACCUMULATE TIME FROM BEGINNING OF IREFSP TO END OF MFPTS + TOFF
C     ISP is a MODFLOW-2000 stress period number
      TOFFSET = 0.0
      DO 60 ITS = MFPTS1,MFPTS
        IF (ITS.GT.0) TOFFSET = TOFFSET + TS(ITS)
   60 CONTINUE
      IF (TOFF.GT.0.0) THEN
        TOFFSET = TOFFSET + TOFF*TS(MFPTS+1)
      ENDIF
      TOFFSET = MODTCONV*TOFFSET/TOMULT
C
      RETURN
      END

