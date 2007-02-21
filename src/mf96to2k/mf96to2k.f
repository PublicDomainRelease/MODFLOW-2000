C
C  *********************************************************************
C     PROGRAM to convert MODFLOW-88 and MODFLOW-96 data files to MODFLOW-2K
C  *********************************************************************
      INCLUDE 'mf96to2k.inc'
C
      CHARACTER*80 OLDNAMFIL
      CHARACTER*1 CANS
      LOGICAL EX
C     ------------------------------------------------------------------
C
      !OPEN(UNIT=*,CARRIAGECONTROL='LIST')
C
      ISUM=1
      INBAS=0
      INBCF=0
C
C  Define file units for use for log file and general use
      IOUT=98
      IUMISC=99
C
C  Get the name file
      WRITE(*,*) ' MODFLOW-88/96 to MODFLOW-2000 Data Conversion'
      WRITE(*,*)
     1   ' Enter the name of an existing MODFLOW-88/96 name file:'
      READ(*,'(A)') OLDNAMFIL
C
C  Check for an existing dataset
      INQUIRE(FILE=OLDNAMFIL,EXIST=EX)
      IF(.NOT. EX) THEN
         WRITE(*,*) 'Name file does not exist:',OLDNAMFIL
         STOP
      END IF
      OPEN(UNIT=IUMISC,FILE=OLDNAMFIL,STATUS='OLD')
C
C  Get the name of a base name for the new dataset:
      NC=0
      DO 5 I=80,1,-1
      IF(OLDNAMFIL(I:I).EQ.'.') THEN
         NC=I-1
         GO TO 8
      END IF
5     CONTINUE
      IF(NC.EQ.0) THEN
         OLDNAMFIL='MF96TO2K'
         NC=8
      END IF
8     WRITE(*,9) OLDNAMFIL(1:NC)
9     FORMAT(1X,
     1 'Enter a base name for creating MODFLOW-2000 files (CR=',A,'):')
      READ(*,'(A)') DSNAME
      IF(DSNAME.EQ.' ') DSNAME=OLDNAMFIL(1:NC)
      NDSNAM=LEN(DSNAME)
      IF(IUCASE.NE.0) CALL USTRUC(DSNAME,NDSNAM)
      DO 20 I=1,NDSNAM
      IF(DSNAME(I:I).EQ.' ') GO TO 11
20    CONTINUE
      I=NDSNAM+1
11    NDSNAM=I-1
      WRITE(*,*) ' The files will be named:'
      WRITE(*,*) DSNAME(1:NDSNAM)//'.nam, ',DSNAME(1:NDSNAM)//'.ba6, ',
     1           DSNAME(1:NDSNAM)//'.bc6, and ',DSNAME(1:NDSNAM)//'.dis'
C
      INQUIRE(FILE=DSNAME(1:NDSNAM)//'.nam',EXIST=EX)
      IF(EX) THEN
         WRITE(*,*) 'New name file already exists: ',
     1     DSNAME(1:NDSNAM)//'.nam'
         WRITE(*,*) 'Overwrite [Y/N]?'
         READ(*,*) CANS
         IF(CANS.NE.'Y' .AND. CANS.NE.'y') STOP
      END IF
      INQUIRE(FILE=DSNAME(1:NDSNAM)//'.ba6',EXIST=EX)
      IF(EX) THEN
         WRITE(*,*) 'New BAS file already exists: ',
     1     DSNAME(1:NDSNAM)//'.ba6'
         WRITE(*,*) 'Overwrite [Y/N]?'
         READ(*,*) CANS
         IF(CANS.NE.'Y' .AND. CANS.NE.'y') STOP
      END IF
      INQUIRE(FILE=DSNAME(1:NDSNAM)//'.bc6',EXIST=EX)
      IF(EX) THEN
         WRITE(*,*) 'New bcf file already exists: ',
     1     DSNAME(1:NDSNAM)//'.bc6'
         WRITE(*,*) 'Overwrite [Y/N]?'
         READ(*,*) CANS
         IF(CANS.NE.'Y' .AND. CANS.NE.'y') STOP
      END IF
      INQUIRE(FILE=DSNAME(1:NDSNAM)//'.dis',EXIST=EX)
      IF(EX) THEN
         WRITE(*,*) 'New DIS file already exists: ',
     1     DSNAME(1:NDSNAM)//'.dis'
         WRITE(*,*) 'Overwrite [Y/N]?'
         READ(*,*) CANS
         IF(CANS.NE.'Y' .AND. CANS.NE.'y') STOP
      END IF
C
C  Open log file and print some information about array sizes
      OPEN(UNIT=IOUT,FILE='mf96to2k.log')
      WRITE(IOUT,*) ' MODFLOW-88/96 to MODFLOW-2K Data Conversion'
      WRITE(IOUT,*)
      WRITE(IOUT,*) ' Length of X array:',LENX
      WRITE(IOUT,*) ' Maximum number of file names:',MXFNAM
      WRITE(IOUT,*) ' Maximum number of stress periods:',MXPER
      WRITE(IOUT,*)
C
C  Read the BAS and BCF files
      CALL MFREAD(IUMISC)
C
C  Write modified NAM, BAS, and BCF files; and create new DIS file
      CALL BASSV2K
      CALL DISSV2K
      CALL BCFSV2K
      CALL NAMESV
C
      STOP
      END
      SUBROUTINE NAMESV
C     ******************************************************************
C     SAVE NAME FILE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'mf96to2k.inc'
C     ------------------------------------------------------------------
      OPEN(FILE=DSNAME(1:NDSNAM)//'.nam',UNIT=IUMISC)
C
C  Write all files except those for which IUFILE is zero.
      DO 10 I=1,NFIL
      IF(IUFILE(I).NE.0) THEN
         IF(IUFILE(I).EQ.INBAS) THEN
            WRITE(IUMISC,4) 'BAS6',INBAS,DSNAME(1:NDSNAM)//'.ba6'
4           FORMAT(A,I10,1X,A)
         ELSE IF(IUFILE(I).EQ.INBCF) THEN
            WRITE(IUMISC,4) 'BCF6',INBCF,DSNAME(1:NDSNAM)//'.bc6'
         ELSE
            J=LENSTR(CNF(I))
            IF(J.NE.0) WRITE(IUMISC,'(A)') CNF(I)(1:J)
         END IF
      END IF
10    CONTINUE
C
C  Find an unused unit for DIS file
      DO 100 I=10,1000
      INDIS=I
      DO 50 J=1,MXFNAM
      IF(IUFILE(J).EQ.INDIS) GO TO 100
50    CONTINUE
      WRITE(IUMISC,4) 'DIS ',INDIS,DSNAME(1:NDSNAM)//'.dis'
      RETURN
C
100   CONTINUE
C
      RETURN
      END
      SUBROUTINE MFREAD(INUNIT)
C     ******************************************************************
C     Read a MODFLOW-88/96 data set.
      INCLUDE 'mf96to2k.inc'
C     ------------------------------------------------------------------
C
C3------DEFINE PROBLEM__ROWS,COLUMNS,LAYERS,STRESS PERIODS,PACKAGES
      CALL SBASO(INUNIT)
      IF(INBAS.EQ.0) THEN
         WRITE(*,*) ' No BAS Package file was defined'
         STOP
      END IF
      IF(INBCF.EQ.0) THEN
         WRITE(*,*) ' No BCF Package file was defined'
         STOP
      END IF
      CALL BASDF
C
C4------ALLOCATE SPACE IN "X" ARRAY.
      CALL BASAL()
      CALL BCFAL()
      IF((ISUM-1).GT.LENX) STOP
      IF(NPER.GT.MXPER) THEN
         WRITE(*,*) ' TOO MANY STRESS PERIODS'
         WRITE(*,*) ' mf96to2k allows a maximum of ',MXPER,
     1               ' stress periods'
         STOP
      END IF
C
C6------READ AND PREPARE INFORMATION FOR ENTIRE SIMULATION.
C  Set all TOP and BOT arrays as undefined.
      DO 10 I=1,NLAY
      METOP(I)=-1
      MEBOT(I)=-1
10    CONTINUE
      CALL BASRP(X(LCHOLD),IX(LCIBOU))
      CALL BCFRP(X(LCDELR),X(LCDELC),X(LCHY),X(LCTOP),X(LCBOT),
     1           X(LCCC),X(LCCV),X(LCSC1),X(LCSC2),X(LCTRPY),X(LCWETD))
C
C7------SIMULATE EACH STRESS PERIOD.
      DO 300 KPER=1,NPER
      KKPER=KPER
C
C7A-----READ STRESS PERIOD TIMING INFORMATION.
      CALL BASST(KKPER)
C
C7B-----READ AND PREPARE INFORMATION FOR STRESS PERIOD.
  300 CONTINUE
C
      RETURN
      END
      SUBROUTINE BASDF
C
C     ******************************************************************
C     DEFINE KEY MODEL PARAMETERS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'mf96to2k.inc'
      CHARACTER*80 LINE1,LINE2
C     ------------------------------------------------------------------
      INRD=IREMAP(INBAS)
C
C1------PRINT THE NAME OF THE PROGRAM.
      WRITE(IOUT,1)
    1 FORMAT('1',33X,'MODFLOW',/6X,'U.S. GEOLOGICAL SURVEY MODULAR',
     1      ' FINITE-DIFFERENCE GROUND-WATER FLOW MODEL')
C
C2------READ AND PRINT A HEADING.
      READ(INRD,'(A)') HEADNG(1)
      READ(INRD,'(A)') HEADNG(2)
      WRITE(IOUT,'(1X,/1X,A)') HEADNG(1)
      WRITE(IOUT,'(1X,A)') HEADNG(2)
C
C3------READ LINE SPECIFYING NUMBER OF LAYERS,ROWS,COLUMNS,STRESS
C3------PERIODS AND UNITS OF TIME CODE, BUT DON'T DECODE UNTIL IT IS
C3------DETERMINED THAT FREE OR FIXED FORMAT IS BEING USED.
      READ(INRD,'(A)') LINE1
C
C4------READ OPTIONS RECORD AND LOOK FOR OPTIONS
      READ(INRD,'(A)') LINE2
      IXSEC=0
      ICHFLG=0
      IFREFM=0
      LLOC=1
    5 CALL URWORD(LINE2,LLOC,ISTART,ISTOP,1,N,R,0,INBAS)
      IF(LINE2(ISTART:ISTOP).EQ.'XSECTION') THEN
         IXSEC=1
      ELSE IF(LINE2(ISTART:ISTOP).EQ.'CHTOCH') THEN
         ICHFLG=1
      ELSE IF(LINE2(ISTART:ISTOP).EQ.'FREE') THEN
         IFREFM=1
         WRITE(IOUT,6)
    6    FORMAT (1X,'THE FREE FORMAT OPTION HAS BEEN SELECTED')
      END IF
      IF(LLOC.LT.80) GO TO 5
C
C5------READ NUMBER OF LAYERS, ROWS, COLUMNS, STRESS PERIODS, AND
C5------ITMUNI USING FREE OR FIXED FORMAT.
      IF(IFREFM.EQ.0) THEN
         READ(LINE1,'(5I10)') NLAY,NROW,NCOL,NPER,ITMUNI
      ELSE
         LLOC=1
         CALL URWORD(LINE1,LLOC,ISTART,ISTOP,2,NLAY,R,0,INBAS)
         CALL URWORD(LINE1,LLOC,ISTART,ISTOP,2,NROW,R,0,INBAS)
         CALL URWORD(LINE1,LLOC,ISTART,ISTOP,2,NCOL,R,0,INBAS)
         CALL URWORD(LINE1,LLOC,ISTART,ISTOP,2,NPER,R,0,INBAS)
         CALL URWORD(LINE1,LLOC,ISTART,ISTOP,2,ITMUNI,R,0,INBAS)
      END IF
C
C6------PRINT # OF LAYERS, ROWS, COLUMNS AND STRESS PERIODS.
      WRITE(IOUT,7) NLAY,NROW,NCOL
    7 FORMAT(1X,I4,' LAYERS',I10,' ROWS',I10,' COLUMNS')
      WRITE(IOUT,8) NPER
    8 FORMAT(1X,I3,' STRESS PERIOD(S) IN SIMULATION')
C
C7------SELECT AND PRINT A MESSAGE SHOWING TIME UNITS AND OTHER OPTIONS.
      IF(ITMUNI.LT.0 .OR. ITMUNI.GT.5) ITMUNI=0
      IF(ITMUNI.EQ.0) THEN
         WRITE(IOUT,9)
    9    FORMAT(1X,'MODEL TIME UNITS ARE UNDEFINED')
      ELSE IF(ITMUNI.EQ.1) THEN
         WRITE(IOUT,11)
   11    FORMAT(1X,'MODEL TIME UNIT IS SECONDS')
      ELSE IF(ITMUNI.EQ.2) THEN
         WRITE(IOUT,21)
   21    FORMAT(1X,'MODEL TIME UNIT IS MINUTES')
      ELSE IF(ITMUNI.EQ.3) THEN
         WRITE(IOUT,31)
   31    FORMAT(1X,'MODEL TIME UNIT IS HOURS')
      ELSE IF(ITMUNI.EQ.4) THEN
         WRITE(IOUT,41)
   41    FORMAT(1X,'MODEL TIME UNIT IS DAYS')
      ELSE
         WRITE(IOUT,51)
   51    FORMAT(1X,'MODEL TIME UNIT IS YEARS')
      END IF
      IF(IXSEC.NE.0) WRITE(IOUT,61)
   61 FORMAT(1X,'CROSS SECTION OPTION IS SPECIFIED')
      IF(ICHFLG.NE.0) WRITE(IOUT,62)
   62 FORMAT(1X,'CALCULATE FLOW BETWEEN ADJACENT CONSTANT-HEAD CELLS')
C
C8------INITIALIZE TOTAL ELAPSED TIME COUNTER STORAGE ARRAY COUNTER
C8------AND CALCULATE NUMBER OF CELLS.
      TOTIM=0.
      ISUM=1
C
C9------RETURN
      RETURN
      END
      SUBROUTINE BASAL
C     ******************************************************************
C     ALLOCATE SPACE FOR BASIC MODEL ARRAYS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'mf96to2k.inc'
C     ------------------------------------------------------------------
      INRD=IREMAP(INBAS)
C
C1------PRINT A MESSAGE IDENTIFYING THE PACKAGE.
      WRITE(IOUT,1)INBAS
    1 FORMAT(1X,/1X,'BAS5 -- BASIC MODEL PACKAGE, VERSION 5, 1/1/95',
     2' INPUT READ FROM UNIT',I3)
C
C2------READ & PRINT FLAG IAPART (RHS & BUFFER SHARE SPACE?) AND
C2------FLAG ISTRT (SHOULD STARTING HEADS BE KEPT FOR DRAWDOWN?).
      IF(IFREFM.EQ.0) THEN
         READ(INRD,'(2I10)') IAPART,ISTRT
      ELSE
         READ(INRD,*) IAPART,ISTRT
      END IF
      IF(IAPART.NE.0) WRITE(IOUT,2)
    2 FORMAT(1X,
     1    'ARRAYS RHS AND BUFF WILL HAVE SEPARATE MEMORY ALLOCATIONS')
      IF(IAPART.EQ.0) WRITE(IOUT,3)
    3 FORMAT(1X,'ARRAYS RHS AND BUFF WILL SHARE MEMORY')
      IF(ISTRT.NE.0) WRITE(IOUT,4)
    4 FORMAT(1X,'INITIAL HEAD WILL BE KEPT THROUGHOUT THE SIMULATION')
      IF(ISTRT.EQ.0) WRITE(IOUT,5)
    5 FORMAT(1X,'INITIAL HEAD WILL NOT BE KEPT THROUGHOUT THE',
     1 ' SIMULATION, WHICH MEANS',/1X,'DRAWDOWN CANNOT BE CALCULATED')
C
C3------STORE LOCATION OF FIRST UNALLOCATED SPACE IN X.
      ISOLD=ISUM
      NRCL=NROW*NCOL*NLAY
C
C4------ALLOCATE SPACE FOR ARRAYS.
      LCHOLD=ISUM
      ISUM=ISUM+NRCL
      LCIBOU=ISUM
      ISUM=ISUM+NRCL
      LCCC=ISUM
      ISUM=ISUM+NRCL
      LCCV=ISUM
      ISUM=ISUM+NROW*NCOL*(NLAY-1)
      LCDELR=ISUM
      ISUM=ISUM+NCOL
      LCDELC=ISUM
      ISUM=ISUM+NROW
      ISP=ISUM-ISOLD
C
C7------PRINT AMOUNT OF SPACE USED.
      WRITE(IOUT,6) ISP
    6 FORMAT(1X,I10,' ELEMENTS IN X ARRAY ARE USED BY BAS')
      ISUM1=ISUM-1
      WRITE(IOUT,7) ISUM1,LENX
    7 FORMAT(1X,I10,' ELEMENTS OF X ARRAY USED OUT OF ',I10)
      IF(ISUM1.GT.LENX) WRITE(IOUT,8)
    8 FORMAT(1X,'   ***X ARRAY MUST BE DIMENSIONED LARGER***')
C
C
C8------RETURN
      RETURN
C
      END
      SUBROUTINE BASRP(HOLD,IBOUND)
C     ******************************************************************
C     READ AND INITIALIZE BASIC MODEL ARRAYS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'mf96to2k.inc'
C
      CHARACTER*24 ANAME(2)
      DIMENSION IBOUND(NCOL,NROW,NLAY),HOLD(NCOL,NROW,NLAY)
C
      DATA ANAME(1) /'          BOUNDARY ARRAY'/
      DATA ANAME(2) /'            INITIAL HEAD'/
C     ------------------------------------------------------------------
      INRD=IREMAP(INBAS)
C
C1------PRINT SIMULATION TITLE, CALCULATE # OF CELLS IN A LAYER.
      WRITE(IOUT,'(''1'',/1X,A)') HEADNG(1)
      WRITE(IOUT,'(1X,A)') HEADNG(2)
C
C2------READ BOUNDARY ARRAY(IBOUND) ONE LAYER AT A TIME.
      IF(IXSEC.EQ.0) THEN
         DO 100 K=1,NLAY
         KK=K
         CALL U2DINT(IBOUND(1,1,KK),ANAME(1),NROW,NCOL,KK,INBAS,IOUT,
     1     CNIBOU(K),LOIBOU(K),FMIBOU(K),IPIBOU(K),
     2     MEIBOU(K),IBIBOU(K),FNIBOU(K))
  100    CONTINUE
      ELSE
         CALL U2DINT(IBOUND(1,1,1),ANAME(1),NLAY,NCOL,-1,INBAS,IOUT,
     1     CNIBOU(1),LOIBOU(1),FMIBOU(1),IPIBOU(1),
     2     MEIBOU(1),IBIBOU(1),FNIBOU(1))
      END IF
C
C3------READ AND PRINT HEAD VALUE TO BE PRINTED FOR NO-FLOW CELLS.
      IF(IFREFM.EQ.0) THEN
         READ(INRD,'(F10.0)') HNOFLO
      ELSE
         READ(INRD,*) HNOFLO
      END IF
      WRITE(IOUT,3) HNOFLO
    3 FORMAT(1X,/1X,'AQUIFER HEAD WILL BE SET TO ',1PG11.5,
     1       ' AT ALL NO-FLOW NODES (IBOUND=0).')
C
C4------READ INITIAL HEADS.
      IF(IXSEC.EQ.0) THEN
         DO 300 K=1,NLAY
         KK=K
         CALL U2DREL(HOLD(1,1,KK),ANAME(2),NROW,NCOL,KK,INBAS,IOUT,
     1     CNHOLD(K),LOHOLD(K),FMHOLD(K),IPHOLD(K),
     2     MEHOLD(K),IBHOLD(K),FNHOLD(K))
  300    CONTINUE
      ELSE
         CALL U2DREL(HOLD(1,1,1),ANAME(2),NLAY,NCOL,-1,INBAS,IOUT,
     1     CNHOLD(1),LOHOLD(1),FMHOLD(1),IPHOLD(1),
     2     MEHOLD(1),IBHOLD(1),FNHOLD(1))
      END IF
C
C
C9------RETURN
 1000 RETURN
      END
      SUBROUTINE BASST(K)
C
C     ******************************************************************
C     SETUP TIME PARAMETERS FOR NEW TIME PERIOD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'mf96to2k.inc'
C     ------------------------------------------------------------------
      INRD=IREMAP(INBAS)
C
C1------READ AND WRITE LENGTH OF STRESS PERIOD, NUMBER OF TIME STEPS AND
C1------TIME STEP MULTIPLIER.
      IF(IFREFM.EQ.0) THEN
         READ(INRD,'(F10.0,I10,F10.0)') PERLEN(K),NSTP(K),TSMULT(K)
      ELSE
         READ(INRD,*) PERLEN(K),NSTP(K),TSMULT(K)
      END IF
      WRITE (IOUT,1) K,PERLEN(K),NSTP(K),TSMULT(K)
    1 FORMAT('1',/28X,'STRESS PERIOD NO.',I4,', LENGTH =',G15.7,/
     1            28X,46('-'),//
     2            30X,'NUMBER OF TIME STEPS =',I6,//
     3            31X,'MULTIPLIER FOR DELT =',F10.3)
C
C
C5------RETURN
      RETURN
      END
      SUBROUTINE SBASO(INUNIT)
C
C     ******************************************************************
C     OPEN FILES.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'mf96to2k.inc'
      INCLUDE 'openspec.inc'
      CHARACTER*80 LINE
      CHARACTER*20 FMTARG
      CHARACTER*20 ACSARG
C     ---------------------------------------------------------------
      NFIL=0
C
C2------READ A LINE; IGNORE BLANK LINES.
10    READ(INUNIT,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GO TO 10
      NFIL=NFIL+1
      CNF(NFIL)=LINE
      IUFILE(NFIL)=0
      IUOPEN(NFIL)=0
      IF(LINE(1:1).EQ.'#') THEN
         WRITE(IOUT,'(A)') LINE
         GO TO 10
      END IF
C
C3------DECODE THE FILE TYPE AND UNIT NUMBER.
      LLOC=1
      CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,0,INUNIT)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,0,INUNIT)
      IUFILE(NFIL)=IU
C
C4------CHECK FOR A VALID FILE TYPE.
      FMTARG='FORMATTED'
      ACSARG='SEQUENTIAL'
C
C4A-----CHECK FOR "BAS" FILE TYPE.
      IF(LINE(ITYP1:ITYP2).EQ.'BAS') THEN
         INBAS=IU
         IUOPEN(NFIL)=IU
C
C4B-----CHECK FOR "BCF" FILE TYPE.
      ELSE IF(LINE(ITYP1:ITYP2).EQ.'BCF') THEN
         INBCF=IU
         IUOPEN(NFIL)=IU
C
C4C-----CHECK FOR "UNFORMATTED" FILE TYPE.
      ELSE IF(LINE(ITYP1:ITYP2).EQ.'DATA(BINARY)') THEN
         FMTARG=FORM
         ACSARG=ACCESS
         IUOPEN(NFIL)=IU
C
C4D-----CHECK FOR "FORMATTED FILE TYPE.
      ELSE IF(LINE(ITYP1:ITYP2).EQ.'DATA') THEN
         IUOPEN(NFIL)=IU
      END IF
C
C5------Get the file name
      IF(IUCASE.EQ.0) THEN
         CALL URWORD(LINE,LLOC,INAM1,INAM2,0,N,R,0,INUNIT)
      ELSE
         CALL URWORD(LINE,LLOC,INAM1,INAM2,1,N,R,0,INUNIT)
      END IF
      FN(NFIL)=LINE(INAM1:INAM2)
      FRM(NFIL)=FMTARG
      ACS(NFIL)=ACSARG
C
C  Abort if the dataset is already MODFLOW-2K
      IF(LINE(ITYP1:ITYP2).EQ.'DIS' .OR.
     1   LINE(ITYP1:ITYP2).EQ.'BCF6' .OR.
     2   LINE(ITYP1:ITYP2).EQ.'BAS6') THEN
         WRITE(*,*) 'File ',LINE(INAM1:INAM2),' is type: ',
     1         LINE(ITYP1:ITYP2)
         WRITE(*,*)
     1    'This indicates that the dataset is already converted',
     2    ' to MODFLOW-2K'
         STOP
      END IF
C
      WRITE(IOUT,36) LINE(INAM1:INAM2),
     1  LINE(ITYP1:ITYP2),IU
36    FORMAT(1X,/1X,'OPENING ',A,/
     1  1X,'FILE TYPE:',A,'   UNIT',I4)
      GO TO 10
C
C7------END OF NAME FILE.
1000  CLOSE(UNIT=INUNIT)
C
C  Remap units IUMISC,IOUT,ISTDIN,ISTDOT
      IUMAP(1,1)=5
      IUMAP(2,1)=6
      IUMAP(3,1)=IUMISC
      IUMAP(4,1)=IOUT
      IUMAP(1,2)=0
      IUMAP(2,2)=0
      IUMAP(3,2)=0
      IUMAP(4,2)=0
      K1=10
      DO 1100 I=1,4
      DO 1050 J=1,NFIL
      IF(IUMAP(I,1).EQ.IUOPEN(J)) THEN
         DO 1040 K=K1,90
         DO 1030 L=1,NFIL
         IF(IUOPEN(L).EQ.K) GO TO 1040
1030     CONTINUE
         IUMAP(I,2)=K
         K1=K+1
         GO TO 1100
1040     CONTINUE
         WRITE(*,*) ' Unable to remap unit ',IUMAP(I,1)
         STOP
      END IF
1050  CONTINUE
1100  CONTINUE
C
C  Open the files
      DO 1200 I=1,NFIL
      IF(IUOPEN(I).NE.0) THEN
         IU=IREMAP(IUOPEN(I))
         OPEN(UNIT=IU,FILE=FN(I),FORM=FRM(I),ACCESS=ACS(I))
      END IF
1200  CONTINUE
C
      RETURN
      END
      SUBROUTINE BASSV2K()
C     ******************************************************************
C     SAVE DATA FOR BAS PACKAGE in MODFLOW2K format
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'mf96to2k.inc'
C
      CHARACTER*80 LINE
      CHARACTER*80 FILNAM
C     ------------------------------------------------------------------
C
      IFIL=IUMISC
      FILNAM=DSNAME(1:NDSNAM)//'.ba6'
      OPEN(FILE=FILNAM,UNIT=IFIL)
      WRITE(IFIL,'(A,A)') '#',HEADNG(1)
      WRITE(IFIL,'(A,A)') '#',HEADNG(2)
C  Write options record
      LL=0
      IF(IFREFM.NE.0) THEN
         LL=4
         LINE(1:LL)='FREE'
      END IF
      IF(ICHFLG.NE.0) THEN
         LINE(LL+1:LL+7)= ' CHTOCH'
         LL=LL+7
      END IF
      IF(IXSEC.NE.0) THEN
         LINE(LL+1:LL+9)= ' XSECTION'
         LL=LL+9
      END IF
      IF(LL.EQ.0) WRITE(IFIL,'(A)') 'NO OPTIONS'
      IF(LL.GT.0) WRITE(IFIL,'(A)') LINE(1:LL)
C
      IF(IXSEC.EQ.0) THEN
         DO 10 K=1,NLAY
         KK=K
         LOC=LCIBOU+(K-1)*NCOL*NROW
         CALL U2DISV(IX(LOC),NCOL,NROW,KK,IFIL,'IBOUND',
     1       CNIBOU(K),LOIBOU(K),FMIBOU(K),IPIBOU(K),MEIBOU(K),
     2       IBIBOU(K),FNIBOU(K))
10       CONTINUE
      ELSE
         CALL U2DISV(IX(LCIBOU),NCOL,NLAY,0,IFIL,'IBOUND',
     1       CNIBOU(1),LOIBOU(1),FMIBOU(1),IPIBOU(1),MEIBOU(1),
     2       IBIBOU(1),FNIBOU(1))
      END IF
C
      IF(ABS(HNOFLO).LE.999999. .AND. ABS(HNOFLO).GE.100.) THEN
         WRITE(IFIL,'(F10.2,A)') HNOFLO,'  HNOFLO'
      ELSE
         WRITE(IFIL,'(1P,E10.3,A)') HNOFLO,'  HNOFLO'
      END IF
C
      IF(IXSEC.EQ.0) THEN
         DO 20 K=1,NLAY
         KK=K
         LOC=LCHOLD+(K-1)*NCOL*NROW
         CALL U2DRSV(X(LOC),NCOL,NROW,KK,IFIL,'Initial Head',
     1       CNHOLD(K),LOHOLD(K),FMHOLD(K),IPHOLD(K),MEHOLD(K),
     2       IBHOLD(K),FNHOLD(K))
20       CONTINUE
      ELSE
         CALL U2DRSV(X(LCHOLD),NCOL,NROW,0,IFIL,'Initial Head',
     1       CNHOLD(1),LOHOLD(1),FMHOLD(1),IPHOLD(1),MEHOLD(1),
     2       IBHOLD(1),FNHOLD(1))
      END IF
C
      CLOSE(UNIT=IFIL)
      RETURN
      END
      SUBROUTINE DISSV2K()
C     ******************************************************************
C     Write DIS file
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'mf96to2k.inc'
C
      CHARACTER*1 CANS
      CHARACTER*2 CSSTR
      CHARACTER*80 FILNAM
      CHARACTER*4 CLAY
C     ------------------------------------------------------------------
C
      IFIL=IUMISC
      FILNAM=DSNAME(1:NDSNAM)//'.dis'
      OPEN(FILE=FILNAM,UNIT=IFIL)
C
      LENUNI=0
      WRITE(IFIL,'(6I5,A)') NLAY,NROW,NCOL,NPER,
     1   ITMUNI,0,'  NLAY,NROW,NCOL,NPER,ITMUNI,LENUNI'
C
C  Construct LAYCBD
      NBOTM=0
      IF(NLAY.GT.1) THEN
         DO 8 K=1,NLAY-1
         WRITE(*,7) K
7      FORMAT(' Is there a quasi 3-D confining bed below layer',I4,'?')
         READ(*,'(A)') CANS
            IF(CANS.EQ.'Y' .OR. CANS.EQ.'y') THEN
              LAYCBD(K)=1
              NBOTM=NBOTM+1
            ELSE
              LAYCBD(K)=0
              NBOTM=NBOTM+2
            END IF
8        CONTINUE
      END IF
      LAYCBD(NLAY)=0
      WRITE(IFIL,'(20I2)') (LAYCBD(K),K=1,NLAY)
C
      IF(MEDELR.EQ.2) MEDELR=1
      CALL U1DRSV(X(LCDELR),NCOL,IFIL,'DELR',
     1    CNDELR,LODELR,FMDELR,IPDELR,MEDELR,FNDELR)
      IF(MEDELC.EQ.2) MEDELC=1
      CALL U1DRSV(X(LCDELC),NROW,IFIL,'DELC',
     1    CNDELC,LODELC,FMDELC,IPDELC,MEDELC,FNDELC)
C
C  Write BOTM array
C  Define system top
      IF(METOP(1).LT.0) THEN
C  If TOP is undefined, show range of BOT to help user
         IF(MEBOT(1).GE.0) THEN
            CALL ARRRNG(X(LCBOT),NCOL,NROW,
     1       'Bottom elevation of layer   1',CNBOT(1))

C  BOT does not exist -- check to see if TOP for next layer down
C  can be used
         ELSE IF(LAYCBD(1).EQ.0 .AND. 1.NE.NLAY) THEN
C  Yes -- layer below exists and there is no confining bed, so TOP
C  for layer below could be a valid BOT for this layer.  See if it exists
            IF(METOP(2).GE.0) THEN
C     Yes, TOP(2) below exists -- show its range
               LOC=LCTOP+NCOL*NROW
               CALL ARRRNG(X(LOC),NCOL,NROW,'BOT of layer   1',
     1                       CNTOP(2))
            END IF
         END IF
         CALL DEFEL(X(LCTOP),NCOL,NROW,'Top elevation',
     1         METOP(1),CNTOP(1),LOTOP(1),1)
      ELSE
C  TOP is already defined
         CALL ARRRNG(X(LCTOP),NCOL,NROW,
     1       'Top elevation of layer 1',CNTOP(1))
      END IF
      IF(METOP(1).EQ.2) METOP(1)=1
      CALL U2DRSV(X(LCTOP),NCOL,NROW,0,IFIL,'TOP of system',
     1      CNTOP(1),LOTOP(1),FMTOP(1),IPTOP(1),METOP(1),
     2      IBTOP(1),FNTOP(1))
C
C  Define bottom of each unit
      DO 10 K=1,NLAY
      KK=K
      WRITE(CLAY,'(I4)') K
      IF(MEBOT(K).GE.0) THEN
C  BOT array exists, so use it.
         IF(MEBOT(K).EQ.2) MEBOT(K)=1
         LOC=LCBOT+(K-1)*NCOL*NROW
         CALL ARRRNG(X(LOC),NCOL,NROW,
     1        'Bottom elevation of layer'//CLAY,CNBOT(K))
         CALL U2DRSV(X(LOC),NCOL,NROW,KK,IFIL,'Layer BOTM',
     1      CNBOT(K),LOBOT(K),FMBOT(K),IPBOT(K),MEBOT(K),
     2      IBBOT(K),FNBOT(K))
      ELSE
C  BOT does not exist -- check to see if TOP for next layer down
C  can be used
         IF(LAYCBD(K).EQ.0 .AND. K.NE.NLAY) THEN
C  Yes -- layer below exists and there is no confining bed, so TOP
C  for layer below could be a valid BOT for this layer.  See if it exists
            IF(METOP(K+1).GE.0) THEN
C     Yes, TOP below exists -- use it
               IF(METOP(K+1).EQ.2) METOP(K+1)=1
               LOC=LCTOP+K*NCOL*NROW
               CALL ARRRNG(X(LOC),NCOL,NROW,'BOT of layer'//CLAY,
     1                       CNTOP(K+1))
               CALL U2DRSV(X(LOC),NCOL,NROW,KK,IFIL,'Layer BOTM',
     1             CNTOP(K+1),LOTOP(K+1),FMTOP(K+1),IPTOP(K+1),
     2             METOP(K+1),IBTOP(K+1),FNTOP(K+1))
            ELSE
C     No, TOP below is undefined -- have to define BOT
               LOC=LCBOT+(K-1)*NCOL*NROW
               CALL DEFEL(X(LOC),NCOL,NROW,'Bottom elevation of',
     1             MEBOT(K),CNBOT(K),LOBOT(K),KK)
               CALL U2DRSV(X(LOC),NCOL,NROW,KK,IFIL,'Layer BOTM',
     1            CNBOT(K),LOBOT(K),FMBOT(K),IPBOT(K),MEBOT(K),
     2            IBBOT(K),FNBOT(K))
            END IF
         ELSE
C  No, TOP for layer below cannot substitute for BOT -- have to
C  define BOT
            LOC=LCBOT+(K-1)*NCOL*NROW
            CALL DEFEL(X(LOC),NCOL,NROW,'Bottom elevation of',
     1          MEBOT(K),CNBOT(K),LOBOT(K),KK)
            CALL U2DRSV(X(LOC),NCOL,NROW,KK,IFIL,'Layer BOTM',
     1         CNBOT(K),LOBOT(K),FMBOT(K),IPBOT(K),MEBOT(K),
     2         IBBOT(K),FNBOT(K))
         END IF
      END IF
C
C  Write BOT for confining bed if there is a confining bed.
      IF(LAYCBD(K).NE.0 .AND. K.NE.NLAY) THEN
         LOC=LCTOP+K*NCOL*NROW
         IF(METOP(K+1).LT.0) THEN
C  TOP below is undefined -- have to define it
            CALL DEFEL(X(LOC),NCOL,NROW,
     1         'Confining bed bottom elevation of',
     2          METOP(K+1),CNTOP(K+1),LOTOP(K+1),KK)
         ELSE
            CALL ARRRNG(X(LOC),NCOL,NROW,
     1       'Confining bed bottom elevation of layer'//CLAY,CNTOP(K+1))
         END IF
         IF(METOP(K+1).EQ.2) METOP(K+1)=1
         CALL U2DRSV(X(LOC),NCOL,NROW,KK,IFIL,'Confining bed BOTM',
     1      CNTOP(K+1),LOTOP(K+1),FMTOP(K+1),IPTOP(K+1),METOP(K+1),
     2      IBTOP(K+1),FNTOP(K+1))
      END IF
C
10    CONTINUE
C
      CSSTR='TR'
      IF(ISS.NE.0) CSSTR='SS'
      DO 30 N=1,NPER
      WRITE(IFIL,'(1P,E10.3,I10,E10.3,2X,A,A)')
     1     PERLEN(N),NSTP(N),TSMULT(N),CSSTR,
     2      '  PERLEN,NSTP,TSMULT,Ss/tr'
30    CONTINUE
      CLOSE(UNIT=IFIL)
C
      RETURN
      END
      SUBROUTINE BCFAL()
C
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR BLOCK-CENTERED FLOW PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'mf96to2k.inc'
C
      CHARACTER*80 LINE
      CHARACTER*12 AVGNAM(4)
      DATA AVGNAM/'HARMONIC    ','ARITHMETIC  ',
     1            'LOGARITHMIC ','*UNCONFINED*'/
C     ------------------------------------------------------------------
      IN=IREMAP(INBCF)
C
C1------IDENTIFY PACKAGE
      WRITE(IOUT,1) INBCF
    1 FORMAT(1X,/1X,'BCF5 -- BLOCK-CENTERED FLOW PACKAGE, VERSION 5',
     1', 9/1/93',' INPUT READ FROM UNIT',I3)
C
C2------READ AND PRINT ISS (STEADY-STATE FLAG), IBCFCB (FLAG FOR
C2------PRINTING OR UNIT# FOR RECORDING CELL-BY-CELL FLOW TERMS), HDRY
C2------(HEAD AT CELLS THAT CONVERT TO DRY), AND WETTING PARAMETERS.
      IF(IFREFM.EQ.0) THEN
         READ(IN,'(2I10,F10.0,I10,F10.0,2I10)')
     1              ISS,IBCFCB,HDRY,IWDFLG,WETFCT,IWETIT,IHDWET
      ELSE
         READ(IN,*) ISS,IBCFCB,HDRY,IWDFLG,WETFCT,IWETIT,IHDWET
      END IF
      IF(ISS.EQ.0) WRITE(IOUT,3)
    3 FORMAT(1X,'TRANSIENT SIMULATION')
      IF(ISS.NE.0) WRITE(IOUT,4)
    4 FORMAT(1X,'STEADY-STATE SIMULATION')
      IF(IBCFCB.LT.0) WRITE(IOUT,8)
    8 FORMAT(1X,'CONSTANT-HEAD CELL-BY-CELL FLOWS WILL BE PRINTED',
     1     ' WHEN ICBCFL IS NOT 0')
      IF(IBCFCB.GT.0) WRITE(IOUT,9) IBCFCB
    9 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT',I3)
      WRITE(IOUT,11) HDRY
   11 FORMAT(1X,'HEAD AT CELLS THAT CONVERT TO DRY=',G13.5)
      IF(IWDFLG.NE.0) GO TO 35
      WRITE(IOUT,12)
   12 FORMAT(1X,'WETTING CAPABILITY IS NOT ACTIVE')
      GO TO 39
C
   35 WRITE(IOUT,36)
   36 FORMAT(1X,'WETTING CAPABILITY IS ACTIVE')
      IF(IWETIT.LE.0) IWETIT=1
      WRITE(IOUT,37)WETFCT,IWETIT
   37 FORMAT(1X,'WETTING FACTOR=',F10.5,
     1     '     WETTING ITERATION INTERVAL=',I4)
      WRITE(IOUT,38)IHDWET
   38 FORMAT(1X,'FLAG THAT SPECIFIES THE EQUATION TO USE FOR HEAD',
     1    ' AT WETTED CELLS=',I4)
C
C3------STOP THE SIMULATION IF THERE ARE MORE THAN 200 LAYERS.
   39 IF(NLAY.LE.200) GO TO 50
      WRITE(IOUT,41)
   41 FORMAT(1X,/1X,'YOU HAVE SPECIFIED MORE THAN 200 MODEL LAYERS'/1X,
     1 'SPACE IS RESERVED FOR A MAXIMUM OF 200 LAYERS IN ARRAYS LAYCON',
     2 ' AND LAYAVG')
      STOP
C
C4------READ LAYCON & PRINT TITLE FOR LAYCON TABLE.
   50 IF(IFREFM.EQ.0) THEN
         READ(IN,'(40I2)') (LAYCON(I),I=1,NLAY)
      ELSE
         READ(IN,*) (LAYCON(I),I=1,NLAY)
      END IF
      WRITE(IOUT,52)
   52 FORMAT(1X,5X,'LAYER  LAYER-TYPE CODE     INTERBLOCK T',
     1      /1X,5X,44('-'))
C
C5------PRINT LAYCON
      DO 100 I=1,NLAY
      IF(LAYCON(I).EQ.30 .OR. LAYCON(I).EQ.32) LAYCON(I)=LAYCON(I)-10
      INAM=LAYCON(I)/10
      LAYAVG(I)=INAM*10
      LAYCON(I)=LAYCON(I)-LAYAVG(I)
      L=LAYCON(I)
      INAM=INAM+1
      WRITE(IOUT,55) I,L,LAYAVG(I),AVGNAM(INAM)
   55 FORMAT(1X,I9,I13,I11,' -- ',A)
  100 CONTINUE
C
C6------COMPUTE THE NUMBER OF CELLS IN THE ENTIRE GRID AND IN ONE LAYER.
      NRC=NROW*NCOL
      ISIZ=NRC*NLAY
C
C7------ALLOCATE SPACE FOR ARRAYS.
      ISOLD=ISUM
      LCSC1=ISUM
      IF(ISS.EQ.0) ISUM=ISUM+ISIZ
      LCSC2=ISUM
      IF(ISS.EQ.0) ISUM=ISUM+ISIZ
      LCTRPY=ISUM
      ISUM=ISUM+NLAY
      LCBOT=ISUM
      ISUM=ISUM+ISIZ
      LCHY=ISUM
      ISUM=ISUM+ISIZ
      LCTOP=ISUM
      ISUM=ISUM+ISIZ
      LCWETD=ISUM
      IF(IWDFLG.NE.0)ISUM=ISUM+ISIZ
C
C8------PRINT THE AMOUNT OF SPACE USED BY THE BCF PACKAGE.
      ISP=ISUM-ISOLD
      WRITE(IOUT,101) ISP
  101 FORMAT(1X,I10,' ELEMENTS IN X ARRAY ARE USED BY BCF')
      ISUM1=ISUM-1
      WRITE(IOUT,102) ISUM1,LENX
  102 FORMAT(1X,I10,' ELEMENTS OF X ARRAY USED OUT OF ',I10)
      IF(ISUM1.GT.LENX) WRITE(IOUT,103)
  103 FORMAT(1X,'   ***X ARRAY MUST BE DIMENSIONED LARGER***')
C
C9------RETURN.
      RETURN
      END
      SUBROUTINE BCFRP(DELR,DELC,HY,TOP,BOT,CC,CV,SC1,SC2,TRPY,WETDRY)
C
C     ******************************************************************
C     READ AND INITIALIZE DATA FOR BLOCK-CENTERED FLOW PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'mf96to2k.inc'
      DIMENSION TRPY(NLAY),DELR(NCOL),DELC(NROW),
     1  HY(NCOL,NROW,NLAY),TOP(NCOL,NROW,NLAY),BOT(NCOL,NROW,NLAY),
     2  CC(NCOL,NROW,NLAY),CV(NCOL,NROW,NLAY),SC1(NCOL,NROW,NLAY),
     3  SC2(NCOL,NROW,NLAY),WETDRY(NCOL,NROW,NLAY)
      CHARACTER*24 ANAME(11)
C
      DATA ANAME(1) /'    PRIMARY STORAGE COEF'/
      DATA ANAME(2) /'    TRANSMIS. ALONG ROWS'/
      DATA ANAME(3) /'   HYD. COND. ALONG ROWS'/
      DATA ANAME(4) /'VERT HYD COND /THICKNESS'/
      DATA ANAME(5) /'                  BOTTOM'/
      DATA ANAME(6) /'                     TOP'/
      DATA ANAME(7) /'  SECONDARY STORAGE COEF'/
      DATA ANAME(8) /'COLUMN TO ROW ANISOTROPY'/
      DATA ANAME(9) /'                    DELR'/
      DATA ANAME(10)/'                    DELC'/
      DATA ANAME(11)/'        WETDRY PARAMETER'/
C     ------------------------------------------------------------------
C
C1------READ TRPY,DELR,DELC.
      CALL U1DREL(TRPY,ANAME(8),NLAY,INBCF,IOUT,CNTRPY,LOTRPY,FMTRPY,
     1         IPTRPY,METRPY,FNTRPY)
      CALL BCFMRK(METRPY,LOTRPY)
      CALL U1DREL(DELR,ANAME(9),NCOL,INBCF,IOUT,CNDELR,LODELR,FMDELR,
     1         IPDELR,MEDELR,FNDELR)
      CALL BCFMRK(MEDELR,LODELR)
      CALL U1DREL(DELC,ANAME(10),NROW,INBCF,IOUT,CNDELC,LODELC,FMDELC,
     1         IPDELC,MEDELC,FNDELC)
      CALL BCFMRK(MEDELC,LODELC)
C
C2------READ ALL PARAMETERS FOR EACH LAYER.
      DO 200 K=1,NLAY
      KK=K
C
C2A-----FIND ADDRESS OF EACH LAYER IN THREE DIMENSION ARRAYS.
C
C2B-----READ PRIMARY STORAGE COEFFICIENT INTO ARRAY SC1 IF TRANSIENT.
      IF(ISS.EQ.0) THEN
         CALL U2DREL(SC1(1,1,K),ANAME(1),NROW,NCOL,KK,INBCF,IOUT,
     1     CNSC1(K),LOSC1(K),FMSC1(K),IPSC1(K),
     2     MESC1(K),IBSC1(K),FNSC1(K))
         CALL BCFMRK(MESC1(K),LOSC1(K))
      END IF
C
C2C-----READ TRANSMISSIVITY INTO ARRAY CC IF LAYER TYPE IS 0 OR 2.
      IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.1) GO TO 100
      CALL U2DREL(CC(1,1,K),ANAME(2),NROW,NCOL,KK,INBCF,IOUT,
     1     CNCC(K),LOCC(K),FMCC(K),IPCC(K),
     2     MECC(K),IBCC(K),FNCC(K))
      CALL BCFMRK(MECC(K),LOCC(K))
      GO TO 110
C
C2D-----READ HYDRAULIC CONDUCTIVITY(HY) AND BOTTOM ELEVATION(BOT)
C2D-----IF LAYER TYPE IS 1 OR 3.
  100 CALL U2DREL(HY(1,1,K),ANAME(3),NROW,NCOL,KK,INBCF,IOUT,
     1     CNHY(K),LOHY(K),FMHY(K),IPHY(K),
     2     MEHY(K),IBHY(K),FNHY(K))
      CALL BCFMRK(MEHY(K),LOHY(K))
      CALL U2DREL(BOT(1,1,K),ANAME(5),NROW,NCOL,KK,INBCF,IOUT,
     1     CNBOT(K),LOBOT(K),FMBOT(K),IPBOT(K),
     2     MEBOT(K),IBBOT(K),FNBOT(K))
      CALL BCFMRK(MEBOT(K),LOBOT(K))
C
C2E-----READ VERTICAL HYCOND/THICK INTO ARRAY CV IF NOT BOTTOM LAYER;
C2E-----MULTIPLIED BY CELL AREA TO CONVERT TO CONDUCTANCE LATER.
  110 IF(K.EQ.NLAY) GO TO 120
      CALL U2DREL(CV(1,1,K),ANAME(4),NROW,NCOL,KK,INBCF,IOUT,
     1     CNCV(K),LOCV(K),FMCV(K),IPCV(K),
     2     MECV(K),IBCV(K),FNCV(K))
      CALL BCFMRK(MECV(K),LOCV(K))
C
C2F-----READ SECONDARY STORAGE COEFFICIENT INTO ARRAY SC2 IF TRANSIENT
C2F-----AND LAYER TYPE IS 2 OR 3.
  120 IF(LAYCON(K).NE.3 .AND. LAYCON(K).NE.2) GO TO 130
      IF(ISS.EQ.0) THEN
         CALL U2DREL(SC2(1,1,K),ANAME(7),NROW,NCOL,KK,INBCF,IOUT,
     1     CNSC2(K),LOSC2(K),FMSC2(K),IPSC2(K),
     2     MESC2(K),IBSC2(K),FNSC2(K))
         CALL BCFMRK(MESC2(K),LOSC2(K))
      END IF
C
C2G-----READ TOP ELEVATION(TOP) IF LAYER TYPE IS 2 OR 3.
      CALL U2DREL(TOP(1,1,K),ANAME(6),NROW,NCOL,KK,INBCF,IOUT,
     1     CNTOP(K),LOTOP(K),FMTOP(K),IPTOP(K),
     2     METOP(K),IBTOP(K),FNTOP(K))
      CALL BCFMRK(METOP(K),LOTOP(K))
C
C2H-----READ WETDRY CODES IF LAYER TYPE IS 1 OR 3 AND WETTING
C2H-----CAPABILITY HAS BEEN INVOKED (IWDFLG NOT 0).
  130 IF(LAYCON(K).NE.3.AND.LAYCON(K).NE.1)GO TO 200
      IF(IWDFLG.EQ.0)GO TO 200
      CALL U2DREL(WETDRY(1,1,K),ANAME(11),NROW,NCOL,KK,INBCF,IOUT,
     1     CNWETD(K),LOWETD(K),FMWETD(K),IPWETD(K),
     2     MEWETD(K),IBWETD(K),FNWETD(K))
      CALL BCFMRK(MEWETD(K),LOWETD(K))
  200 CONTINUE
C
C4------RETURN
      RETURN
      END
      SUBROUTINE BCFMRK(IMETH,LOCAT)
C  SET IUFILE=0 IF IMETH=2
      INCLUDE 'mf96to2k.inc'
C
      IF(IMETH.NE.2) RETURN
      DO 10 I=1,NFIL
      IF(LOCAT.EQ.IUFILE(I)) IUFILE(I)=0
10    CONTINUE
      RETURN
      END
      SUBROUTINE BCFSV2K()
C
C     ******************************************************************
C     SAVE DATA FOR BLOCK-CENTERED FLOW PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'mf96to2k.inc'
C
      CHARACTER*80 LINE
      CHARACTER*80 FILNAM
C     ------------------------------------------------------------------
C *************generate a new cfn with file type=BCF6 -- use the same IU
C
      IFIL=IUMISC
      FILNAM=DSNAME(1:NDSNAM)//'.bc6'
      OPEN(FILE=FILNAM,UNIT=IFIL)
      IF(ABS(HDRY).LE.99999. .AND. ABS(HDRY).GE.100.) THEN
         WRITE(IFIL,'(I10,F10.2,I10,1P,E10.2,2I10,A)')
     1      IBCFCB,HDRY,IWDFLG,WETFCT,IWETIT,IHDWET,
     2   '  IBCFCB,HDRY,IWDFLG,WETFCT,IWETIT,IHDWET'
      ELSE
         WRITE(IFIL,'(I10,1P,E10.2,I10,E10.2,2I10,A)')
     1      IBCFCB,HDRY,IWDFLG,WETFCT,IWETIT,IHDWET,
     2   '  IBCFCB,HDRY,IWDFLG,WETFCT,IWETIT,IHDWET'
      END IF
C  LAYCON
      IF(IFREFM.EQ.0) THEN
         WRITE(IFIL,'(40I2)') (LAYCON(I)+LAYAVG(I),I=1,NLAY)
C  Original model -- use I2 format
      ELSE
C  New model -- Write with I2 format if LAYAVG for all layers is 0.  This
C  will allow original model (I2 format) and new model (list directed format)
C  to both read the file.  Write with I3 format if is not 0; this cannot
C  be read by original model.
         DO 20 K=1,NLAY
         IF(LAYAVG(K).NE.0) GO TO 25
20       CONTINUE
C  All LAYAVG values are 0 -- use I2 format.
         WRITE(IFIL,'(40I2)') (LAYCON(I),I=1,NLAY)
         GO TO 26
C  A LAYAVG value is not 0, use I3 format.
25       WRITE(IFIL,'(20I3)') (LAYCON(I)+LAYAVG(I),I=1,NLAY)
      END IF
C
26    IF(METRPY.EQ.2) METRPY=1
      CALL U1DRSV(X(LCTRPY),NLAY,IFIL,'TRPY',
     1    CNTRPY,LOTRPY,FMTRPY,IPTRPY,METRPY,FNTRPY)
C
C2------WRITE ALL PARAMETERS FOR EACH LAYER
      DO 200 K=1,NLAY
      KK=K
C
C2B-----PRIMARY STORAGE COEFFICIENT INTO ARRAY SC1 IF TRANSIENT
      IF(ISS.EQ.0) THEN
         LOC=LCSC1+(K-1)*NCOL*NROW
         IF(MESC1(K).EQ.2) MESC1(K)=1
         CALL U2DRSV(X(LOC),NCOL,NROW,KK,IFIL,'SF1',
     1       CNSC1(K),LOSC1(K),FMSC1(K),IPSC1(K),MESC1(K),
     2       IBSC1(K),FNSC1(K))
      END IF
C
C2C-----TRANSMISSIVITY INTO ARRAY CC IF LAYER TYPE IS 0 OR 2
      IF(LAYCON(K).EQ.0 .OR. LAYCON(K).EQ.2) THEN
         LOC=LCCC+(K-1)*NCOL*NROW
         IF(MECC(K).EQ.2) MECC(K)=1
         CALL U2DRSV(X(LOC),NCOL,NROW,KK,IFIL,'TRAN',
     1       CNCC(K),LOCC(K),FMCC(K),IPCC(K),MECC(K),
     2       IBCC(K),FNCC(K))
      ELSE
C
C2D-----HYDRAULIC CONDUCTIVITY(HY)
C2D-----IF LAYER TYPE IS 1 OR 3
         LOC=LCHY+(K-1)*NCOL*NROW
         IF(MEHY(K).EQ.2) MEHY(K)=1
         CALL U2DRSV(X(LOC),NCOL,NROW,KK,IFIL,'HY',
     1       CNHY(K),LOHY(K),FMHY(K),IPHY(K),MEHY(K),
     2       IBHY(K),FNHY(K))
      END IF
C
C2E-----VERTICAL HYCOND/THICK
      IF(K.NE.NLAY) THEN
         IF(MECV(K).EQ.2) MECV(K)=1
         LOC=LCCV+(K-1)*NCOL*NROW
         CALL U2DRSV(X(LOC),NCOL,NROW,KK,IFIL,'VCONT',
     1       CNCV(K),LOCV(K),FMCV(K),IPCV(K),MECV(K),
     2       IBCV(K),FNCV(K))
      END IF
C
C2F-----SECONDARY STORAGE COEFFICIENT (ARRAY SF2) IF TRANSIENT
C2F-----AND LAYER TYPE IS 2 OR 3
      IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.2) THEN
         IF(ISS.EQ.0) THEN
            IF(MESC2(K).EQ.2) MESC2(K)=1
            LOC=LCSC2+(K-1)*NCOL*NROW
            CALL U2DRSV(X(LOC),NCOL,NROW,KK,IFIL,'SF2',
     1       CNSC2(K),LOSC2(K),FMSC2(K),IPSC2(K),MESC2(K),
     2       IBSC2(K),FNSC2(K))
         END IF
      END IF
C
C2H-----WETDRY CODES IF LAYER TYPE IS 1 OR 3 AND WETTING
C2H-----CAPABILITY HAS BEEN INVOKED (IWDFLG NOT 0)
      IF( (LAYCON(K).EQ.3.OR.LAYCON(K).EQ.1) .AND. IWDFLG.NE.0 ) THEN
         IF(MEWETD(K).EQ.2) MEWETD(K)=1
         LOC=LCWETD+(K-1)*NCOL*NROW
         CALL U2DRSV(X(LOC),NCOL,NROW,KK,IFIL,'WETDRY',
     1       CNWETD(K),LOWETD(K),FMWETD(K),IPWETD(K),MEWETD(K),
     2       IBWETD(K),FNWETD(K))
      END IF
  200 CONTINUE
C
      CLOSE(UNIT=IFIL)
C
      RETURN
      END
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
      SUBROUTINE ULAPRW(BUF,TEXT,KSTP,KPER,NCOL,NROW,ILAY,IPRN,IOUT,CON)
C
C
C-----VERSION 1520 18SEPT1992 ULAPRW
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
    1    FORMAT(1H1,1X,A,' IN LAYER',I3,' AT END OF TIME STEP',I3,
     1     ' IN STRESS PERIOD',I3/2X,71('-'))
      ELSE IF(ILAY.LT.0) THEN
         WRITE(IOUT,2) TEXT,KSTP,KPER
    2    FORMAT(1H1,1X,A,' FOR CROSS SECTION AT END OF TIME STEP',I3,
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
   10 WRITE(IOUT,11) I,(BUF(J,I)*CON,J=1,NCOL)
   11 FORMAT(1H0,I3,2X,1PG10.3,10(1X,G10.3):/(5X,11(1X,G10.3)))
      GO TO 1000
C
C------------ FORMAT 9G13.6
   20 WRITE(IOUT,21) I,(BUF(J,I)*CON,J=1,NCOL)
   21 FORMAT(1H0,I3,2X,1PG13.6,8(1X,G13.6):/(5X,9(1X,G13.6)))
      GO TO 1000
C
C------------ FORMAT 15F7.1
   30 WRITE(IOUT,31) I,(BUF(J,I)*CON,J=1,NCOL)
   31 FORMAT(1H0,I3,1X,15(1X,F7.1):/(5X,15(1X,F7.1)))
      GO TO 1000
C
C------------ FORMAT 15F7.2
   40 WRITE(IOUT,41) I,(BUF(J,I)*CON,J=1,NCOL)
   41 FORMAT(1H0,I3,1X,15(1X,F7.2):/(5X,15(1X,F7.2)))
      GO TO 1000
C
C------------ FORMAT 15F7.3
   50 WRITE(IOUT,51) I,(BUF(J,I)*CON,J=1,NCOL)
   51 FORMAT(1H0,I3,1X,15(1X,F7.3):/(5X,15(1X,F7.3)))
      GO TO 1000
C
C------------ FORMAT 15F7.4
   60 WRITE(IOUT,61) I,(BUF(J,I)*CON,J=1,NCOL)
   61 FORMAT(1H0,I3,1X,15(1X,F7.4):/(5X,15(1X,F7.4)))
      GO TO 1000
C
C------------ FORMAT 20F5.0
   70 WRITE(IOUT,71) I,(BUF(J,I)*CON,J=1,NCOL)
   71 FORMAT(1H0,I3,1X,20(1X,F5.0):/(5X,20(1X,F5.0)))
      GO TO 1000
C
C------------ FORMAT 20F5.1
   80 WRITE(IOUT,81) I,(BUF(J,I)*CON,J=1,NCOL)
   81 FORMAT(1H0,I3,1X,20(1X,F5.1):/(5X,20(1X,F5.1)))
      GO TO 1000
C
C------------ FORMAT 20F5.2
   90 WRITE(IOUT,91) I,(BUF(J,I)*CON,J=1,NCOL)
   91 FORMAT(1H0,I3,1X,20(1X,F5.2):/(5X,20(1X,F5.2)))
      GO TO 1000
C
C------------ FORMAT 20F5.3
  100 WRITE(IOUT,101) I,(BUF(J,I)*CON,J=1,NCOL)
  101 FORMAT(1H0,I3,1X,20(1X,F5.3):/(5X,20(1X,F5.3)))
      GO TO 1000
C
C------------ FORMAT 20F5.4
  110 WRITE(IOUT,111) I,(BUF(J,I)*CON,J=1,NCOL)
  111 FORMAT(1H0,I3,1X,20(1X,F5.4):/(5X,20(1X,F5.4)))
      GO TO 1000
C
C------------ FORMAT 10G11.4
  120 WRITE(IOUT,121) I,(BUF(J,I)*CON,J=1,NCOL)
  121 FORMAT(1H0,I3,2X,1PG11.4,9(1X,G11.4):/(5X,10(1X,G11.4)))
      GO TO 1000
C
C------------ FORMAT 10F6.0
  130 WRITE(IOUT,131) I,(BUF(J,I)*CON,J=1,NCOL)
  131 FORMAT(1H0,I3,1X,10(1X,F6.0):/(5X,10(1X,F6.0)))
      GO TO 1000
C
C------------ FORMAT 10F6.1
  140 WRITE(IOUT,141) I,(BUF(J,I)*CON,J=1,NCOL)
  141 FORMAT(1H0,I3,1X,10(1X,F6.1):/(5X,10(1X,F6.1)))
      GO TO 1000
C
C------------ FORMAT 10F6.2
  150 WRITE(IOUT,151) I,(BUF(J,I)*CON,J=1,NCOL)
  151 FORMAT(1H0,I3,1X,10(1X,F6.2):/(5X,10(1X,F6.2)))
      GO TO 1000
C
C------------ FORMAT 10F6.3
  160 WRITE(IOUT,161) I,(BUF(J,I)*CON,J=1,NCOL)
  161 FORMAT(1H0,I3,1X,10(1X,F6.3):/(5X,10(1X,F6.3)))
      GO TO 1000
C
C------------ FORMAT 10F6.4
  170 WRITE(IOUT,171) I,(BUF(J,I)*CON,J=1,NCOL)
  171 FORMAT(1H0,I3,1X,10(1X,F6.4):/(5X,10(1X,F6.4)))
      GO TO 1000
C
C------------ FORMAT 10F6.5
  180 WRITE(IOUT,181) I,(BUF(J,I)*CON,J=1,NCOL)
  181 FORMAT(1H0,I3,1X,10(1X,F6.5):/(5X,10(1X,F6.5)))
C
 1000 CONTINUE
C
C5------RETURN
      RETURN
      END
      SUBROUTINE U1DREL(A,ANAME,JJ,IN,IOUT,
     1      CNSTNT,LOCAT,FMTIN,IPRN,IMETH,FNAME)
C
C
C-----VERSION 1740 18APRIL1993 U1DREL
C     ******************************************************************
C     ROUTINE TO INPUT 1-D REAL DATA MATRICES
C       A IS ARRAY TO INPUT
C       ANAME IS 24 CHARACTER DESCRIPTION OF A
C       JJ IS NO. OF ELEMENTS
C       IN IS INPUT UNIT
C       IOUT IS OUTPUT UNIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*24 ANAME
      DIMENSION A(JJ)
      CHARACTER*20 FMTIN
      CHARACTER*80 CNTRL
      CHARACTER*(*) FNAME
      DATA NUNOPN/99/
C     ------------------------------------------------------------------
      INRD=IREMAP(IN)
C
C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(INRD,'(A)') CNTRL
C
C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,0,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
         IMETH=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
         IMETH=1
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,0,IN)
         IMETH=2
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,0,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         IMETH=3
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT',I4,':',/1X,A)
         OPEN(UNIT=LOCAT,FILE=FNAME)
         ICLOSE=1
      ELSE
C
C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
    1    FORMAT(I10,F10.0,A20,I10)
         IMETH=2
         IF(LOCAT.EQ.0) IMETH=0
         IF(LOCAT.EQ.IN) IMETH=1
      END IF
C
C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,0,IN)
         IF(LOCAT.GT.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,0,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,0,IN)
         END IF
      END IF
C
C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.GT.0) GO TO 90
C
C4A-----LOCAT <0 OR =0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
      DO 80 J=1,JJ
   80 A(J)=CNSTNT
      WRITE(IOUT,3) ANAME,CNSTNT
    3 FORMAT(1X,/1X,A,' =',G15.7)
      CNSTNT=1.0
      RETURN
C
C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 INRD=LOCAT
      IF(IMETH.NE.3) INRD=IREMAP(LOCAT)
      WRITE(IOUT,5) ANAME,LOCAT,FMTIN
    5 FORMAT(1X,///11X,A,/
     1       1X,'READING ON UNIT',I4,' WITH FORMAT: ',A20)
      IF(FMTIN.EQ.'(FREE)') THEN
         READ(INRD,*) (A(J),J=1,JJ)
      ELSE
         READ(INRD,FMTIN) (A(J),J=1,JJ)
      END IF
      IF(ICLOSE.NE.0) CLOSE(UNIT=INRD)
C
C6------IF PRINT CODE (IPRN) =0 OR >0 THEN PRINT ARRAY VALUES.
120   IF(IPRN.EQ.0) THEN
         WRITE(IOUT,1001) (A(J),J=1,JJ)
1001     FORMAT((1X,1PG12.5,9(1X,G12.5)))
      ELSE IF(IPRN.GT.0) THEN
         WRITE(IOUT,1002) (A(J),J=1,JJ)
1002     FORMAT((1X,1PG12.5,4(1X,G12.5)))
      END IF
C
C7------RETURN
      RETURN
C
C8------CONTROL RECORD ERROR.
500   WRITE(IOUT,502) ANAME
502   FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      WRITE(IOUT,'(1X,A)') CNTRL
      STOP
      END
      SUBROUTINE U2DINT(IA,ANAME,II,JJ,K,IN,IOUT,
     1      CNSTNT,LOCAT,FMTIN,IPRN,IMETH,IBIN,FNAME)
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
      INCLUDE 'openspec.inc'
      CHARACTER*24 ANAME
      DIMENSION IA(JJ,II)
      CHARACTER*20 FMTIN
      CHARACTER*80 CNTRL
      CHARACTER*(*) FNAME
      DATA NUNOPN/99/
C     ------------------------------------------------------------------
      INRD=IREMAP(IN)
      IBIN=0
C
C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(INRD,'(A)') CNTRL
C
C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,0,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
         IMETH=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
         IMETH=1
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,0,IN)
         IMETH=2
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,0,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         IMETH=3
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
         IMETH=2
         IF(LOCAT.EQ.0) IMETH=0
         IF(LOCAT.EQ.IN) IMETH=1
      END IF
C
C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,ICONST,R,0,IN)
         IF(LOCAT.NE.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,0,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            IF(ICLOSE.NE.0) THEN
               IF(FMTIN.EQ.'(BINARY)') THEN
                  OPEN(UNIT=LOCAT,FILE=FNAME,FORM=FORM,ACCESS=ACCESS)
               ELSE
                  OPEN(UNIT=LOCAT,FILE=FNAME)
               END IF
            END IF
            IF(LOCAT.GT.0 .AND. FMTIN.EQ.'BINARY') LOCAT=-LOCAT
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,0,IN)
         END IF
      END IF
C
C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      CNSTNT=ICONST
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
      CNSTNT=1.0
      RETURN
C
C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 IURD=LOCAT
      IF(IMETH.NE.3) IURD=IREMAP(LOCAT)
      IF(K.GT.0) THEN
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
         READ(IURD,*) (IA(J,I),J=1,JJ)
      ELSE
         READ(IURD,FMTIN) (IA(J,I),J=1,JJ)
      END IF
  100 CONTINUE
      GO TO 300
C
C4C-----LOCAT<0; READ UNFORMATTED RECORD CONTAINING ARRAY VALUES.
  200 LOCAT=-LOCAT
      IURD=LOCAT
      IF(IMETH.NE.3) IURD=IREMAP(LOCAT)
      IBIN=1
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
      READ(IURD)
      READ(IURD) IA
C
C6------IF PRINT CODE (IPRN) <0 THEN RETURN.
  300 IF(ICLOSE.NE.0) CLOSE(UNIT=IURD)
      IF(IPRN.LT.0) RETURN
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
      SUBROUTINE U2DREL(A,ANAME,II,JJ,K,IN,IOUT,
     1      CNSTNT,LOCAT,FMTIN,IPRN,IMETH,IBIN,FNAME)
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
      INCLUDE 'openspec.inc'
      CHARACTER*24 ANAME
      DIMENSION A(JJ,II)
      CHARACTER*20 FMTIN
      CHARACTER*80 CNTRL
      CHARACTER*16 TEXT
      CHARACTER*(*) FNAME
      DATA NUNOPN/99/
C     ------------------------------------------------------------------
      IURD=IREMAP(IN)
      IBIN=0
C
C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IURD,'(A)') CNTRL
C
C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,0,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
         IMETH=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
         IMETH=1
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,0,IN)
         IMETH=2
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,0,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         IMETH=3
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
         IMETH=2
         IF(LOCAT.EQ.0) IMETH=0
         IF(LOCAT.EQ.IN) IMETH=1
      END IF
C
C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,0,IN)
         IF(LOCAT.NE.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,0,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            IF(ICLOSE.NE.0) THEN
               IF(FMTIN.EQ.'(BINARY)') THEN
                  OPEN(UNIT=LOCAT,FILE=FNAME,FORM=FORM,ACCESS=ACCESS)
               ELSE
                  OPEN(UNIT=LOCAT,FILE=FNAME)
               END IF
            END IF
            IF(LOCAT.GT.0 .AND. FMTIN.EQ.'(BINARY)') LOCAT=-LOCAT
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,0,IN)
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
      CNSTNT=1.0
      RETURN
C
C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 IURD=LOCAT
      IF(IMETH.NE.3) IURD=IREMAP(LOCAT)
      IF(K.GT.0) THEN
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
         READ(IURD,*) (A(J,I),J=1,JJ)
      ELSE
         READ(IURD,FMTIN) (A(J,I),J=1,JJ)
      END IF
  100 CONTINUE
      GO TO 300
C
C4C-----LOCAT<0; READ UNFORMATTED ARRAY VALUES.
  200 LOCAT=-LOCAT
      IURD=LOCAT
      IF(IMETH.NE.3) IURD=IREMAP(LOCAT)
      IBIN=1
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
      READ(IURD) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY
      READ(IURD) A
C
C6------IF PRINT CODE (IPRN) >0 OR =0 THEN PRINT ARRAY VALUES.
  300 IF(ICLOSE.NE.0) CLOSE(UNIT=IURD)
      IF(IPRN.GE.0) CALL ULAPRW(A,ANAME,0,0,JJ,II,0,IPRN,IOUT,CNSTNT)
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
      SUBROUTINE U1DRSV(ARRAY,NC,IFIL,LABEL,
     1      CNSTNT,LOCAT,FMTIN,IPRN,IMETH,FNAME)
C     ******************************************************************
C     Write a control record for a 1-D real array and if the array is
C     an INTERNAL array, write the array.
C     ******************************************************************
      DIMENSION ARRAY(NC)
      CHARACTER*20 FMTIN
      CHARACTER*(*) LABEL,FNAME
C
C  New control record
      IF(IMETH.EQ.0) THEN
C  CONSTANT
         IF(IFIL.GT.0) WRITE(IFIL,11) ARRAY(1),LABEL
11       FORMAT('CONSTANT ',1PE14.6,2X,A)
         RETURN
C
      ELSE IF(IMETH.EQ.1) THEN
C  INTERNAL
         IF(IFIL.GT.0) WRITE(IFIL,
     1        '(''INTERNAL '',1PE14.6,1X,A,I5,2X,A)')
     2         CNSTNT,'(10F13.0)',IPRN,LABEL
         IF(IFIL.GT.0) CALL U1DRWR(ARRAY,NC,IFIL)
C
      ELSE IF(IMETH.EQ.3) THEN
C  OPEN/CLOSE
         IF(IFIL.GT.0) THEN
            LLOC=1
            CALL URWORD(FNAME,LLOC,ISTART,ISTOP,0,N,R,0,0)
            WRITE(IFIL,20) FNAME(ISTART:ISTOP),CNSTNT,
     1                 FMTIN,IPRN,LABEL
20          FORMAT('OPEN/CLOSE ',A,1PE14.6,' ''',A,'''',I5,2X,A)
         END IF
      ELSE
C  EXTERNAL
         IF(IFIL.GT.0) THEN
            WRITE(IFIL,21) LOCAT,CNSTNT,FMTIN,IPRN,LABEL
21          FORMAT('EXTERNAL ',I5,1PE14.6,' ''',A,'''',I5,2X,A)
         END IF
      END IF
C
      RETURN
      END
      SUBROUTINE U2DRSV(ARRAY,NC,NR,K,IFIL,LABEL,
     1      CNSTNT,LOCAT,FMTIN,IPRN,IMETH,IBIN,FNAME)
C     ******************************************************************
C     Write a control record for a 2-D real array and if the array is
C     an INTERNAL array, write the array.
C     ******************************************************************
      DIMENSION ARRAY(NC,NR)
      CHARACTER*(*) LABEL,FNAME,FMTIN
      CHARACTER*10 LAYER
C
      IF(IFIL.LE.0) RETURN
      LAYER=' '
      IF(K.GT.0) WRITE(LAYER,5) K
5     FORMAT(' layer',I4)
C
      IF(IMETH.EQ.0) THEN
C  Constant
         WRITE(IFIL,11) ARRAY(1,1),LABEL,LAYER
11       FORMAT('CONSTANT ',1PE14.6,2X,2A)
C
      ELSE IF(IMETH.EQ.1) THEN
C  INTERNAL
         WRITE(IFIL,21) CNSTNT,'(10F13.0)',IPRN,LABEL,LAYER
21       FORMAT('INTERNAL ',1PE14.6,1X,A,I5,2X,2A)
         CALL U2DRWR(ARRAY,NC,NR,IFIL)
C
      ELSE IF(IMETH.EQ.3) THEN
C  OPEN/CLOSE
         WRITE(IFIL,25) FNAME,CNSTNT,FMTIN,IPRN,LABEL,LAYER
25       FORMAT('OPEN/CLOSE ',A,1PE14.6,' ''',A,'''',I5,2X,2A)
C
      ELSE
C  EXTERNAL
         IF(IBIN.NE.0) FMTIN='(BINARY)'
         WRITE(IFIL,31) LOCAT,CNSTNT,FMTIN,IPRN,LABEL,LAYER
31       FORMAT('EXTERNAL ',I5,1PE14.6,' ''',A,'''',I5,2X,2A)
      END IF
C
      RETURN
      END
      SUBROUTINE U2DISV(IARRAY,NC,NR,K,IFIL,LABEL,
     1      CNSTNT,LOCAT,FMTIN,IPRN,IMETH,IBIN,FNAME)
C     ******************************************************************
C     Write a control record for a 2-D real array and if the array is
C     an INTERNAL array, write the array.
C     ******************************************************************
      DIMENSION IARRAY(NC,NR)
      CHARACTER*(*) LABEL,FNAME,FMTIN
      CHARACTER*10 LAYER
C
      IF(IFIL.LE.0) RETURN
      ICON=CNSTNT
      LAYER=' '
      IF(K.GT.0) WRITE(LAYER,5) K
5     FORMAT(' layer',I4)
C
      IF(IMETH.EQ.0) THEN
C  Constant
         WRITE(IFIL,11) IARRAY(1,1),LABEL,LAYER
11       FORMAT('CONSTANT ',I10,2X,2A)
C
      ELSE IF(IMETH.EQ.1) THEN
C  INTERNAL
         WRITE(IFIL,21) ICON,'(20I4)',IPRN,LABEL,LAYER
21       FORMAT('INTERNAL ',I10,1X,A,I5,2X,2A)
         CALL U2DIWR(IARRAY,NC,NR,IFIL)
C
      ELSE IF(IMETH.EQ.3) THEN
C  OPEN/CLOSE
         WRITE(IFIL,25) FNAME,ICON,FMTIN,IPRN,LABEL,LAYER
25       FORMAT('OPEN/CLOSE ',A,I10,' ''',A,'''',I5,2X,2A)
C
      ELSE
C  EXTERNAL
         IF(IBIN.NE.0) FMTIN='(BINARY)'
         WRITE(IFIL,31) LOCAT,ICON,FMTIN,IPRN,LABEL,LAYER
31       FORMAT('EXTERNAL ',I5,I10,' ''',A,'''',I5,2X,2A)
      END IF
C
      RETURN
      END
      SUBROUTINE U1DRWR(A,JJ,IFIL)
C     ******************************************************************
C     Write a formatted 1-D real array
C     ******************************************************************
      DIMENSION A(JJ)
C
      WRITE(IFIL,'(1P,10E13.6)') (A(J),J=1,JJ)
      RETURN
      END
      SUBROUTINE U2DRWR(A,JJ,II,IFIL)
C     ******************************************************************
C     Write a formatted 2-D real array
C     ******************************************************************
      DIMENSION A(JJ,II)
C
      DO 10 I=1,II
      WRITE(IFIL,'(1P,10E13.6)') (A(J,I),J=1,JJ)
10    CONTINUE
      RETURN
      END
      SUBROUTINE U2DIWR(IA,JJ,II,IFIL)
C     ******************************************************************
C     Write a formatted 2-D integer array
C     ******************************************************************
      DIMENSION IA(JJ,II)
C
      DO 10 I=1,II
      DO 10 J=1,JJ
      IF(IA(J,I).GT.9999) IA(J,I)=9999
      IF(IA(J,I).LT.-999) IA(J,I)=-999
10    CONTINUE
C
      DO 20 I=1,II
      WRITE(IFIL,'(20I4)') (IA(J,I),J=1,JJ)
20    CONTINUE
      RETURN
      END
      SUBROUTINE USTRUC(C,NC)
C     ******************************************************************
C     Convert a string to upper case.
C     ******************************************************************
      CHARACTER*(*) C
C
      IDIFF=ICHAR('a')-ICHAR('A')
      DO 10 I=1,NC
      IF(C(I:I).GE.'a' .AND. C(I:I).LE.'z')
     1        C(I:I)=CHAR(ICHAR(C(I:I))-IDIFF)
10    CONTINUE
      RETURN
      END
      SUBROUTINE DEFEL(ARRAY,NC,NR,LABEL,IMETH,CNSTNT,LOCAT,K)
C     ******************************************************************
C     Create an array with a constant value specified by user.
C     ******************************************************************
      DIMENSION ARRAY(NC,NR)
      CHARACTER*(*) LABEL
C
4     WRITE(*,5) LABEL,K
5     FORMAT(' Enter a constant value for ',A,' Layer',I4)
      READ(*,*,ERR=4) CNSTNT
      LOCAT=0
      IMETH=0
      DO 20 I=1,NR
      DO 20 J=1,NC
      ARRAY(J,I)=CNSTNT
20    CONTINUE
      CNSTNT=1.0
C
      RETURN
      END
      FUNCTION LENSTR(A)
      CHARACTER*(*) A
      NC=LEN(A)
      DO 10 I=NC,1,-1
      IF(A(I:I).NE.' ') GO TO 20
10    CONTINUE
      I=0
20    LENSTR=I
      RETURN
      END
      FUNCTION IREMAP(LOCAT)
      INCLUDE 'mf96to2k.inc'
C
      I=LOCAT
      IF(LOCAT.EQ.IUMAP(1,1)) I=IUMAP(1,2)
      IF(LOCAT.EQ.IUMAP(2,1)) I=IUMAP(2,2)
      IF(LOCAT.EQ.IUMAP(3,1)) I=IUMAP(3,2)
      IF(LOCAT.EQ.IUMAP(4,1)) I=IUMAP(4,2)
      IREMAP=I
      RETURN
      END
      SUBROUTINE ARRRNG(ARRAY,NC,NR,LABEL,CNSTNT)
C  Find and print the range of values in an array
      DIMENSION ARRAY(NC,NR)
      CHARACTER*(*) LABEL
C
      RSMALL=ARRAY(1,1)
      RBIG=ARRAY(1,1)
      DO 10 I=1,NR
      DO 10 J=1,NC
      IF(ARRAY(J,I).GT.RBIG) RBIG=ARRAY(J,I)
      IF(ARRAY(J,I).LT.RSMALL) RSMALL=ARRAY(J,I)
10    CONTINUE
C
      WRITE(*,*) LABEL,' range:',RSMALL*CNSTNT,RBIG*CNSTNT
      RETURN
      END
